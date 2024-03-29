
/* file earthloc.c
 *      ==========
 *
 * version 9, 22-May-2006
 *
 * computes distance and azimuth of two locations on earth
 * transferred from FORTRAN program MBAZ of P. Hellweg
 * K. Stammler, 21-May-92
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1992,  Klaus Stammler, Federal Institute for Geosciences
 *                                       and Natural Resources (BGR), Germany
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */


#include <stdio.h>
#include <string.h>
#include <math.h>
#include "basecnst.h"
#include BC_SYSBASE
#include "shconst.h"
#include "earthloc.h"



/* macros */
#define DEG2RAD(x) ((x)*BC_PI/180.0)
#define RAD2DEG(x) ((x)/BC_PI*180.0)
#define IDXRECLTH 110



/* global variables */
static char mbv_indexfile[BC_FILELTH+1]    /* name of FER index file */
	={"fereg.dat"};
static char mbv_namefile[BC_FILELTH+1]     /* name of FER name file */
	={"fername.dat"};
static char mbv_corrfile[BC_FILELTH+1]     /* station corrections */
	={"inputs\\grfcorr.dat"};



/* prototypes of local routines */
static void mb_geocr( double lon, double lat, double *a, double *b, double *c );
static double mb_azm( double x, double y );
void mbh_fer_lookup( float alat, char drclat, float alng, char drclng,
	int *igreg, int *isreg );
void mbh_fer_region(int igreg, char grname[], int isreg, char srname[] );



/*---------------------------------------------------------------------*/


/* test main module */


#ifdef XXX
int main( int argc, char *argv[] )
{
	/* local variables */
	double   slat, slon;             /* station coordinates */
	double   elat, elon;             /* epicentre coordinates */
	double   dist, azim, bazim;      /* results */
	int      feridx;                 /* FER index */
	STATUS   status;                 /* return status */
	char     fername[BC_LINELTH+1];  /* FER name */

	/* executable code */

	status = BC_NOERROR;

	if  (argc != 5)  {
		printf( "*** Usage: mbdis <slat> <slon> <elat> <elon> ***\n" );
		return 0;
	} /*endif*/

	sscanf( argv[1], "%lf", &slat );
	sscanf( argv[2], "%lf", &slon );
	sscanf( argv[3], "%lf", &elat );
	sscanf( argv[4], "%lf", &elon );

	mb_locdiff( slat, slon, elat, elon, &dist, &azim, &bazim );

	printf( "   distance:     %6.2f deg\n", dist );
	printf( "   azimuth:      %6.2f deg\n", azim );
	printf( "   back-azimuth: %6.2f deg\n", bazim );

	mb_ferindex( elat, elon, &feridx, &status );
	printf( "   FER index: %d\n", feridx );
	mb_fername( feridx, BC_LINELTH, fername, &status );
	printf( "   FER name: %s\n", fername );
	return 0;

} /* end of main */
#endif /* XXX */

#ifdef YYY
/* second test main module */
int main( int argc, char *argv[] )
{
	/* local variables */
	float    slo_in, az_in;
	float    slo_out, az_out;
	float    slo_dist, az_dist;
	STATUS   status;

	/* executable code */

	status = BC_NOERROR;
	if  (argc != 3)  {
		printf( "*** Usage: earthloc <slo> <az> ***\n" );
		return 0;
	} /*endif*/

	sscanf( argv[1], "%f", &slo_in );
	sscanf( argv[2], "%f", &az_in );
	mb_statcorr( slo_in, az_in, &slo_out, &az_out, &slo_dist,
		&az_dist, &status );

	if  (Severe(&status))  {
		printf( "*** status code %d ***\n", status );
	} else {
		printf( "   corr: slo %5.2f,  az %6.3f\n", slo_out, az_out );
		printf( "   dist: slo %5.2f,  az %6.3f\n", slo_dist, az_dist );
	} /*endif*/

	return 0;

} /* end of main */
#endif /* YYY */



/*---------------------------------------------------------------------*/



void mb_locdiff( double slat, double slon, double elat, double elon,
	double *distance, double *azim, double *bazim )

/* returns distance and azimuth in degrees of two locations on
 * earth
 *
 * parameters of routine
 * double     slat, slon;    input; latitude and longitude of station
 * double     elat, elon;    input; latitude and longitude of epicentre
 * double     *distance;     output; distance in degrees
 * double     *azim;         output; azimuth in degrees
 * double     *bazim;        output; back-azimuth in degrees
 */
{
	/* local variables */
	double   as, bs, cs, ds;
	double   ae, be, ce, de;
	double   bls, cbls, sbls, ble;
	double   codel, bgdel;
	double   xi, xj, xk;
	double   sindt, cosz, sinz;

	/* executable code */

	mb_geocr( slon, slat, &as, &bs, &cs );
	ds = sqrt( 1.0 - cs*cs );
	mb_geocr( elon, elat, &ae, &be, &ce );
	de = sqrt( 1.0 - ce*ce );

	bls = DEG2RAD( slon );
	cbls = cos( bls );
	sbls = sin( bls );
	codel = ae*as + be*bs + ce*cs;

	sindt = sqrt( 1.0-codel*codel );
	if  (codel == 0.0)  {
		bgdel = BC_PI/2.0;
	} else {
		bgdel = atan( fabs(sindt/codel) );
		if  (codel <= 0.0)
			bgdel = BC_PI - bgdel;
	} /*endif*/

	*distance = RAD2DEG( bgdel );

	/* azimuths */
	xi = bs*ce - be*cs;
	xj = as*ce - ae*cs;
	xk = as*be - ae*bs;
	cosz = (xi*sbls + xj*cbls)/sindt;
	sinz = xk/(ds*sindt);
	*bazim = mb_azm( cosz, sinz );
	ble = DEG2RAD( elon );
	cosz = -(xi*sin(ble) + xj*cos(ble))/sindt;
	sinz = -xk/(de*sindt);
	*azim = mb_azm( cosz, sinz );

} /* end of mb_locdiff */



/*---------------------------------------------------------------------*/



void mb_deg_to_km( int listlth, double lat[], double lon[],
	float x[], float y[] )

/* computes x-y coordinates of points on surface of earth given in lat&lon.
 * The reference point is assumed to be given as last element of the list.
 *
 * parameters of routine
 * int        listlth;       length of following arrays
 * double     lat[], lon[];  input; given locations in deg
 * float      x[], y[];      output; returned locations in km
 */
{
	/* local variables */
	int      i;                /* counter */
	double   reflat, reflon;   /* reference point */
	double   dist, az, baz;    /* distance azimuth back-azimuth */

	/* executable code */

	reflat = lat[listlth-1];
	reflon = lon[listlth-1];
	x[listlth-1] = 0.0;
	y[listlth-1] = 0.0;

	for  (i=0; i<(listlth-1); i++)  {
		mb_locdiff( lat[i], lon[i], reflat, reflon, &dist, &az, &baz );
		dist *= SHC_DEG_TO_KM;
		if  (dist < SHC_EPSILON)  {
			x[i] = y[i] = 0.0;
		} else {
			az /= SHC_RAD_TO_DEG;
			x[i] = dist * sin(az);
			y[i] = dist * cos(az);
		} /*endif*/
	} /*endfor*/

} /* end of mb_deg_to_km */



/*---------------------------------------------------------------------*/



static void mb_geocr( double lon, double lat, double *a, double *b, double *c )

/* no comment available
 *
 */
{
	/* local variables */
	double   blbda, bphi, ep, ug, vg;

	/* executable code */

	blbda = DEG2RAD( lon );
	bphi = DEG2RAD( lat );
	ep = 1.0 - 1.0/297.0;
	ug = ep*ep*tan(bphi);
	vg = 1.0/sqrt(1.0+ug*ug);
	*a = vg*cos(blbda);
	*b = vg*sin(blbda);
	*c = ug*vg;

} /* end of mb_geocr */



/*---------------------------------------------------------------------*/



static double mb_azm( double x, double y )

/* no comment available
 *
 */
{
	/* local variables */
	double   th;

	/* executable code */

	if  (x == 0.0)  {
		if  (y > 0.0)  return 90.0;
		if  (y < 0.0)  return 270.0;
		return 0.0;
	} /*endif*/

	th = RAD2DEG( atan(fabs(y/x)) );
	if  (x > 0.0)  {
		if  (y < 0.0)  return (360.0-th);
		return th;
	} else {
		if  (y >= 0.0)  return (180.0-th);
		return (180.0+th);
	} /*endif*/

} /* end of mb_azm */



/*---------------------------------------------------------------------*/

#ifdef XXX

/* this is old version with old names */

void mb_ferindex( float lat, float lon, int *feridx, STATUS *status )

/* returns FER index from given location (lat,lon) in degrees
 *
 * parameters of routine
 * float      lat, lon;     input; location in degrees
 */
{
	/* local variables */
	int      in, ie, i, j;
	float    zu, eloni, re;
	FILE     *reg;
	int      ferrec[IDXRECLTH];
	char     line[BC_LINELTH];

	/* executable code */

	if  (lat < -90.0 || lat > 90.0)  {
		*feridx = -1;
		*status = MBE_ILLLAT;
		return;
	} else if  (lon < -180.0 || lon > 180.0)  {
		*feridx = -1;
		*status = MBE_ILLLON;
		return;
	} /*endif*/

	ie = in = 0;
	if  (lat < 0.0)  in = 180;
	if  (lon < 0.0)  ie = 90;
	lat = fabs( lat );
	lon = fabs( lon );
	zu = (float)ie + (float)in + lat + 0.0001;
	i = (int)zu + 1;

	eloni = lon + 0.0001;

	reg = sy_fopen( mbv_indexfile, "r" );
	if  (reg == NULL)  {
		*status = MBE_OPNIDX;
		return;
	} /*endif*/
	j = 1;
	while  (j < i)  {
		if  (fgets( line, BC_LINELTH, reg ) == NULL)  {
			*status = MBE_EOF;
			fclose( reg );
			return;
		} /*endif*/
		if  (*line == '\n')  j++;
	} /*endwhile*/
	fscanf( reg, "%d\n", ferrec );
	for  (j=1; j<=ferrec[0]; j++)
		fscanf( reg, "%d\n", ferrec+j );
	/* fseek( reg, (i-1)*IDXRECLTH*sizeof(int), 0 ); */
	/* fread( ferrec, sizeof(int), IDXRECLTH, reg ); */
	fclose( reg );

	j = ferrec[0];
	for  (i=2; i<j; i += 3)  {
		re = (float)(ferrec[i]) + (float)(ferrec[i+1])*0.001;
		if  (eloni < re)  {
			*feridx = ferrec[i-1];
			break;
		} /*endif*/
	} /*endfor*/

} /* end of mb_ferindex */

#endif

/*---------------------------------------------------------------------*/



void mb_ferindex( float lat, float lon, int *feridx, STATUS *status )

/* returns FER index from given location (lat,lon) in degrees
 *
 * parameters of routine
 * float      lat, lon;     input; location in degrees
 */
{
	/* local variables */
	int      isreg;        /* seismic region name, not used here */

	/* executable code */

	mbh_fer_lookup( lat, ' ', lon, ' ', feridx, &isreg );

} /* end of mb_ferindex */



/*---------------------------------------------------------------------*/

#ifdef XXX

/* this is old version with old names */

void mb_fername( int ferindex, int maxlth, char fername[], STATUS *status )

/* returns name of FER
 *
 * parameters of routine
 * int        ferindex;       input; FER index
 * char       fername[];      output; FER name
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	FILE     *fn;      /* file pointer */
	int      slth;     /* string length */

	/* executable code */

	if  (maxlth < 79)  {
		*status = MBE_SHORTSTR;
		return;
	} /*endif*/

	fn = sy_fopen( mbv_namefile, "r" );
	if  (fn == NULL)  {
		*status = MBE_OPNNAM;
		return;
	} /*endif*/

	while  (ferindex-- > 0)
		fgets( fername, maxlth, fn );

	sy_fclose( fn );

	slth = (int)strlen( fername ) - 1;
	if  (fername[slth] == '\n')
		fername[slth] = '\0';

} /* end of mb_fername */

#endif

/*---------------------------------------------------------------------*/



void mb_fername( int ferindex, int maxlth, char fername[], STATUS *status )

/* returns name of FER
 *
 * parameters of routine
 * int        ferindex;       input; FER index
 * char       fername[];      output; FER name
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	char     srname[cBcLineLth+1];   /* seismic region name, not used here */

	/* executable code */

	if  (maxlth < 79)  {
		*status = MBE_SHORTSTR;
		return;
	} /*endif*/

	mbh_fer_region( ferindex, fername, 1, srname );

} /* end of mb_fername */



/*---------------------------------------------------------------------*/



void mb_setindexfile( char name[], STATUS *status )

/* tells where to find the FER index file
 *
 * parameters of routine
 * char       name[];     input; name (and path) of FER index file
 * STATUS     *status;    output; return status
 */
{
	/* executable code */

	if  (strlen(name) > BC_FILELTH)  {
		*status = MBE_SHORTSTR;
		return;
	} /*endif*/
	strcpy( mbv_indexfile, name );


} /* end of mb_setindexfile */



/*---------------------------------------------------------------------*/



void mb_setfernamefile( char name[], STATUS *status )

/* tells where to find the FER name file
 *
 * parameters of routine
 * char       name[];     input; name (and path) of FER name file
 * STATUS     *status;    output; return status
 */
{
	/* executable code */

	if  (strlen(name) > BC_FILELTH)  {
		*status = MBE_SHORTSTR;
		return;
	} /*endif*/
	strcpy( mbv_namefile, name );


} /* end of mb_setfernamefile */



/*---------------------------------------------------------------------*/



void mb_getlocname( float lat, float lon, int maxlth,
	char name[], STATUS *status )

/* returns FER name of location (lat,lon)
 *
 * parameters of routine
 * float      lat, lon;       input; location in degrees
 * int        maxlth;         input; maximum length of output string
 * char       name[];         output; FER name
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	int      feridx;       /* FER index */

	/* executable code */

	mb_ferindex( lat, lon, &feridx, status );
	if  (Severe(status))  return;
	mb_fername( feridx, maxlth, name, status );

} /* end of mb_getlocname */



/*---------------------------------------------------------------------*/



void mb_spherediff( float slat, float slon, float elat, float elon,
	float *dist, float *bazim )

/* computes difference and azimuth of two locations (slat,slon)
 * and (elat,elon)
 *
 * parameters of routine
 * float      slat, slon;    input; (station) location in degrees
 * float      elat, elon;    input; (epicentre) location in degrees
 * float      *dist;         output; distance in degrees
 * float      *bazim;        output; (back-)azimuth in degrees
 */
{
	/* local variables */
	float    gamma;    /* top angle in triangle */
	BOOLEAN  invert;   /* invert angle */
	double   arg;      /* argument of acos */

	/* executable code */

	/* transformation to mathematical system */
	slat = 90.0 - slat;
	elat = 90.0 - elat;
	if  (slon < 0.0)  slon = 360 + slon;
	if  (elon < 0.0)  elon = 360 + elon;

	/* compute top angle */
	gamma = elon - slon;
	if  (gamma < 0.0)  gamma += 360.0;
	invert = (gamma > 180.0);
	if  (invert)  gamma = 360.0 - gamma;

	/* degrees to radian */
	slat = DEG2RAD( slat );
	elat = DEG2RAD( elat );
	gamma = DEG2RAD( gamma );

	arg = cos(elat)*cos(slat) + sin(elat)*sin(slat)*cos(gamma);
	if  (arg > 1.0)  {fprintf(stderr,"-->acos(%e) [1]\n",arg); arg = 1.0;}
	if  (arg < -1.0)  {fprintf(stderr,"-->acos(%e) [1]\n",arg); arg = -1.0;}
	*dist = acos( arg );
	arg = (cos(elat)-cos(slat)*cos(*dist))/(sin(slat)*sin(*dist));
	if  (arg > 1.0)  {fprintf(stderr,"-->acos(%e) [2]\n",arg); arg = 1.0;}
	if  (arg < -1.0)  {fprintf(stderr,"-->acos(%e) [2]\n",arg); arg = -1.0;}
	*bazim = acos( arg );

	/* back to degrees */
	*dist = RAD2DEG( *dist );
	*bazim = RAD2DEG( *bazim );

	if  (invert)  *bazim = 360.0 - *bazim;

} /* end of mb_spherediff */



/*---------------------------------------------------------------------*/



void mb_sphereadd( float slat, float slon, float dist, float bazim,
	float *elat, float *elon )

/* computes new location from input location (slat,slon) and distance
 * "dist" and azimuth "bazim"
 *
 * parameters of routine
 * float      slat, slon;    input; (station) input location in degrees
 * float      dist;          input; distance in degrees
 * float      bazim;         input; (back-)azimuth in degrees
 * float      *elat, *elon;  output; (epicentre) location in degrees
 */
{
	/* local variables */
	float    gamma;    /* top angle in triangle */
	BOOLEAN  invert;   /* invert angle */
	double   arg;      /* argument of acos */

	/* executable code */

	/* transformation to mathematical system */
	slat = 90.0 - slat;
	if  (slon < 0.0)  slon = 360 + slon;

	invert = (bazim > 180.0);
	if  (invert)  bazim = 360 - bazim;

	/* degrees to radian */
	slat = DEG2RAD( slat );
	dist = DEG2RAD( dist );
	bazim = DEG2RAD( bazim );

	arg = cos(dist)*cos(slat) + sin(dist)*sin(slat)*cos(bazim);
	if  (arg > 1.0)  {fprintf(stderr,"-->acos(%e) [3]\n",arg); arg = 1.0;}
	if  (arg < -1.0)  {fprintf(stderr,"-->acos(%e) [3]\n",arg); arg = -1.0;}
	*elat = acos( arg );
	arg = (cos(dist)-cos(*elat)*cos(slat))/(sin(*elat)*sin(slat));
	if  (arg > 1.0)  {fprintf(stderr,"-->acos(%e) [4]\n",arg); arg = 1.0;}
	if  (arg < -1.0)  {fprintf(stderr,"-->acos(%e) [4]\n",arg); arg = -1.0;}
	gamma = acos( arg );

	/* back to degrees */
	*elat = RAD2DEG( *elat );
	gamma = RAD2DEG( gamma );

	if  (invert)  gamma = 360.0 - gamma;
	*elon = slon + gamma;
	if  (*elon > 360.0)  *elon -= 360.0;

	/* transformation back to geo-system */
	*elat = 90.0 - *elat;
	if  (*elon > 180.0)  *elon -= 360.0;

} /* end of mb_sphereadd */



/*---------------------------------------------------------------------*/



void mb_statcorr( float slo_in, float az_in, float *slo_out,
	float *az_out, float *slo_dist, float *az_dist, STATUS *status )

/* applies station correction for slowness and azimuth.  The
 * corrections are listed in a correction file mbv_corrfile.
 * The difference vector of the next correction point is
 * added to the input data
 *
 * parameters of routine
 * float      slo_in;    input; input slowness in sec/deg
 * float      az_in;     input; input azimuth in deg
 * float      *slo_out;  output; corrected slowness in sec/deg
 * float      *az_out;   output; corrected azimuth in deg
 * float      *slo_dist; output; slowness distance to the next correction
 * float      *az_dist;  output; azimuth distance to the next correction
 * STATUS     *status;   output; return status
 */
#define STARTPOS 26
{
	/* local variables */
	FILE     *cf;                  /* pointer to correction file */
	char     line[BC_LINELTH+1];   /* current line */
	float    a0, ac;               /* uncorrected and corrected azimuth */
	float    s0, sc;               /* uncorrected and corrected slowness */
	float    a0m, s0m;             /* az & slo of next position */
	float    acm, scm;             /* corr. az & slo of next position */
	float    a0r;                  /* a0 in radians */
	float    ui, vi;               /* input position */
	float    u0, v0;               /* corrector position */
	float    mindist;              /* minimum distance */
	float    dist;                 /* current distance */

	/* executable code */

	cf = sy_fopen( mbv_corrfile, "r" );
	if  (cf == NULL)  {
		*status = MBE_OPNCORR;
		return;
	} /*endif*/

	az_in = DEG2RAD( az_in );
	ui = slo_in * cos( az_in );
	vi = slo_in * sin( az_in );
	mindist = -1.0;  /* init value */

	/* find closest existing correction */
	while  (fgets(line,BC_LINELTH,cf) != NULL)  {
		if  (*line != '!')  {
			if  (sscanf(line+STARTPOS,"%f %f %f %f",&s0,&a0,&sc,&ac) != 4)  {
				*status = MBE_RDCORR;
				sy_fclose( cf );
				return;
			} /*endif*/
			a0r = DEG2RAD( a0 );
			u0 = s0 * cos( a0r );
			v0 = s0 * sin( a0r );
			dist = (u0-ui)*(u0-ui) + (v0-vi)*(v0-vi);
			if  (mindist < 0.0  ||  dist < mindist)  {
				mindist = dist;
				a0m = a0;  s0m = s0;
				acm = ac;  scm = sc;
			} /*endif*/
		} /*endif*/
	} /*endwhile*/

	sy_fclose( cf );

	if  (mindist < 0.0)  {
		*status = MBE_EMPTYLIST;
		return;
	} /*endif*/

	/* apply corrections */
	az_in = RAD2DEG( az_in );
	*slo_out = slo_in + scm - s0m;
	*az_out = az_in + acm - a0m;
	if  (*az_out < 0.0)  *az_out += 360.0;
	if  (*az_out > 360.0)  *az_out -= 360.0;
	*slo_dist = s0m - slo_in;
	*az_dist = a0m - az_in;

} /* end of mb_statcorr */
#undef STARTPOS



/*---------------------------------------------------------------------*/



void mb_setcorrfilename( char name[], STATUS *status )

/* tells where to find the slowness and azimuth correction file
 *
 * parameters of routine
 * char       name[];     input; name (and path) of correction file
 * STATUS     *status;    output; return status
 */
{
	/* executable code */

	if  (strlen(name) > BC_FILELTH)  {
		*status = MBE_SHORTSTR;
		return;
	} /*endif*/
	strcpy( mbv_corrfile, name );


} /* end of mb_setcorrfilename */



/*---------------------------------------------------------------------*/

/* subroutines copied from ISC program CD */


void mbh_fer_lookup( float alat, char drclat, float alng, char drclng,
	int *igreg, int *isreg )

/*Caveat : This  subroutine is part of a suite of programs which is         */
/*         software  quality  registered.   Software is supplied on         */
/*         the understanding the user is responsible for validation         */
/*         and accepts that the authors have  no  liability for its         */
/*         use or misuse.                                                   */
/*                                                                          */
/*                                                                          */
/*                  REGION NUMBER LOOKUP PROGRAM                            */
/*                  -------======---------------                            */
/*                                                                          */
/*                                                                          */
/*     Given a latitude and a longitude this subroutine looks up the        */
/*     geographical region number as defined by Edward A. Flinn and         */
/*     Eric R. Engdahl in Reviews of Geophysics Vol.3 No.1 pp.123-149       */
/*     Feb.1965 and in the Bulletin of the Seismological Society of         */
/*     America Vol.14 No.3-Part II pp.771-992 Jun.1974.                     */
/*     Also included are four additional geographical regions as agreed     */
/*     at IUGG in August 1987, six additional regions approved at IASPEI    */
/*     in August 1989, a further eighteen regions presented at IUGG in      */
/*     August 1991, and all finally approved at IUGG in July 1995.          */
/*                                                                          */
/*                                                                          */
/*     From the call  lookup(alat,drclat,alng,drclng,igreg,isreg)           */
/*                                                                          */
/*     and given the latitude (ALAT), the direction (DRCLAT) 'N' or 'S',    */
/*          and the longitude (ALNG), the direction (DRCLNG) 'E' or 'W',    */
/*     the program looks up the geographical region number (IGREG), and     */
/*                          also the seismic region number (ISREG).         */
/*                                                                          */
/*     Options:  If the directions are set to blank the program uses the    */
/*     sign of the latitude and longitude, positive 'N' and 'E' and         */
/*     negative 'S' and 'W'.  The direction code is not case sensitive      */
/*     but the program will only recognise 'N', 'S', 'E', 'W', or blank.    */
/*     Note:  No test is made for a latitude and longitude near a           */
/*     boundary; the nearest integer value is used.                         */
/*                                                                          */
/*                                                                          */
/*     The world is segmentised by Flinn and Engdahl as follows -           */
/*                                                                          */
/*                                     Y.N                                  */
/*                                      .                                   */
/*                                      .                                   */
/*                           NW  ***    .    ***  NE                        */
/*                               *2*    .    *1*                            */
/*                           +-  ***    .    ***  ++                        */
/*                                      .                                   */
/*                                      .                                   */
/*                         Quad 2       .       Quad 1                      */
/*                                      .                                   */
/*                  W                   .                    E              */
/*                  ..........................................              */
/*                  X                   .                    X              */
/*                                      .                                   */
/*                         Quad 3       .       Quad 4                      */
/*                                      .                                   */
/*                                      .                                   */
/*                           SW  ***    .    ***  SE                        */
/*                               *3*    .    *4*                            */
/*                           --  ***    .    ***  -+                        */
/*                                      .                                   */
/*                                      .                                   */
/*                                     Y.S                                  */
/*                                                                          */
/*                                                                          */
/*     See also the region name lookup program (REGION).                    */
/*                                                                          */
/*                                                                          */
{
int lat, lng, i, j, n, quad;
static int date = 1995;
static int nn = 757;
static int ind[5] = { 0,    0, 1919, 3535, 4747};
static int num[5] = { 0, 1914, 1579, 1161, 1304};
static short int nsreg[759] = { 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4,
    4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9,10,10,10,10,
   10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,
   12,12,12,12,13,13,13,14,14,14,14,14,14,14,15,15,15,15,15,15,16,16,16,16,16,
   16,16,16,16,16,16,16,16,17,17,18,18,18,18,18,18,19,19,19,19,19,19,19,19,19,
   19,19,19,19,19,20,20,20,20,20,20,20,20,20,20,20,21,21,21,21,21,21,22,22,22,
   22,22,22,22,22,22,22,22,22,22,23,23,23,23,23,23,23,23,23,23,23,23,24,24,24,
   24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,
   25,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,27,27,27,27,27,
   28,28,28,28,28,28,28,28,28,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,
   29,29,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,
   31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,
   31,32,32,32,32,32,32,32,32,32,32,32,32,32,33,33,33,33,33,33,33,33,33,33,33,
   33,33,33,33,33,33,33,33,33,33,33,33,34,34,34,34,34,34,34,34,34,34,34,34,34,
   34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,
   34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,
   34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,
   34,34,35,35,35,35,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,37,
   37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,
   37,37,37,37,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,38,38,38,38,38,38,
   38,38,38,38,38,38,38,38,38,38,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,
   39,39,39,39,39,39,39,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,
   40,40,40,40,40,41,41,41,41,41,41,41,41,41,41,41,42,42,42,42,42,42,42,42,42,
   42,42,42,42,42,42,42,43,43,43,43,43,43,43,43,43,43,44,44,44,44,44,44,44,45,
   45,45,46,46,46,46,46,46,47,47,47,47,48,48,48,48,48,48,48,48,49,49,49,49,49,
   49,50,50,50, 5, 7,10,25,25,25,25,25,32,32,33,33,33,37,37,37,37,37,37,37,37,
   37,37,37,37,37,43,44, 0,
 };
static short int lln[6060][3] = {
  0,  0,561, 0,  9,565, 0, 14,566, 0, 18,567, 0, 30,568, 0, 31,569, 0, 35,570,
  0, 41,571, 0, 44,420, 0, 65,421, 0, 70,422, 0, 78,420, 0, 92,705, 0, 97,706,
  0,104,707, 0,105,301, 0,108,261, 0,119,265, 0,125,266, 0,127,267, 0,130,196,
  0,141,198, 0,145,614, 0,165,618, 1,  0,561, 1,  9,563, 1, 11,565, 1, 13,566,
  1, 18,567, 1, 30,568, 1, 35,570, 1, 41,571, 1, 46,420, 1, 64,421, 1, 69,420,
  1, 70,422, 1, 78,420, 1, 92,705, 1, 97,706, 1,103,707, 1,105,301, 1,108,261,
  1,119,262, 1,120,265, 1,125,266, 1,127,267, 1,130,196, 1,141,198, 1,145,614,
  1,165,618, 2,  0,561, 2,  9,562, 2, 16,566, 2, 18,567, 2, 31,568, 2, 35,570,
  2, 41,571, 2, 47,420, 2, 62,421, 2, 68,420, 2, 70,422, 2, 75,423, 2, 80,420,
  2, 90,705, 2, 96,706, 2,101,707, 2,105,301, 2,111,261, 2,118,262, 2,125,263,
  2,126,266, 2,128,267, 2,130,196, 2,141,198, 2,145,614, 2,165,618, 3,  0,561,
  3,  9,562, 3, 15,564, 3, 17,566, 3, 19,567, 3, 31,568, 3, 34,570, 3, 42,571,
  3, 47,419, 3, 48,420, 3, 60,421, 3, 68,420, 3, 70,422, 3, 75,423, 3, 80,420,
  3, 90,705, 3, 96,706, 3,101,707, 3,104,301, 3,113,261, 3,118,262, 3,125,263,
  3,128,264, 3,131,209, 3,142,614, 3,165,615, 3,171,618, 4,  0,752, 4,  1,753,
  4,  3,755, 4, 10,562, 4, 15,564, 4, 19,567, 4, 20,564, 4, 23,567, 4, 28,557,
  4, 35,570, 4, 36,558, 4, 43,571, 4, 47,419, 4, 49,420, 4, 58,421, 4, 68,420,
  4, 70,422, 4, 75,423, 4, 80,420, 4, 90,705, 4, 95,706, 4,100,707, 4,104,301,
  4,114,261, 4,119,258, 4,121,262, 4,125,263, 4,128,264, 4,131,209, 4,142,614,
  4,165,615, 4,171,618, 5,  0,752, 5,  1,753, 5,  3,755, 5, 10,562, 5, 15,564,
  5, 27,557, 5, 36,558, 5, 45,571, 5, 47,419, 5, 49,420, 5, 56,421, 5, 66,420,
  5, 70,422, 5, 75,423, 5, 79,424, 5, 83,420, 5, 90,705, 5, 94,706, 5,100,707,
  5,104,301, 5,115,261, 5,119,258, 5,121,262, 5,123,259, 5,127,248, 5,128,260,
  5,131,209, 5,142,614, 5,165,615, 6,  0,752, 6,  1,753, 6,  3,755, 6, 11,562,
  6, 15,564, 6, 26,557, 6, 35,558, 6, 47,419, 6, 50,420, 6, 55,421, 6, 64,420,
  6, 70,422, 6, 75,423, 6, 79,424, 6, 83,319, 6, 90,704, 6, 98,706, 6, 99,707,
  6,103,708, 6,104,301, 6,116,261, 6,119,253, 6,120,258, 6,121,259, 6,127,248,
  6,128,260, 6,131,209, 6,142,614, 6,165,615, 7,  0,752, 7,  1,753, 7,  3,755,
  7, 12,562, 7, 15,564, 7, 25,557, 7, 34,558, 7, 47,419, 7, 50,420, 7, 55,421,
  7, 63,420, 7, 70,422, 7, 75,423, 7, 79,424, 7, 83,319, 7, 90,704, 7, 98,707,
  7,101,708, 7,104,301, 7,116,261, 7,118,253, 7,121,259, 7,127,248, 7,128,260,
  7,131,209, 7,142,614, 7,162,615, 8,  0,753, 8,  3,755, 8, 12,556, 8, 19,564,
  8, 24,557, 8, 34,558, 8, 46,560, 8, 47,419, 8, 51,420, 8, 54,421, 8, 61,417,
  8, 71,418, 8, 76,314, 8, 79,424, 8, 82,319, 8, 90,704, 8, 98,707, 8,101,708,
  8,104,736, 8,107,301, 8,117,252, 8,118,253, 8,121,259, 8,127,248, 8,128,260,
  8,131,209, 8,142,614, 8,160,615, 9,  0,753, 9,  3,755, 9, 12,556, 9, 21,564,
  9, 23,557, 9, 34,558, 9, 44,560, 9, 47,419, 9, 51,420, 9, 54,421, 9, 61,417,
  9, 71,418, 9, 76,314, 9, 80,424, 9, 82,319, 9, 90,704, 9, 98,707, 9,100,708,
  9,104,736, 9,108,301, 9,117,252, 9,119,253, 9,122,257, 9,124,259, 9,127,248,
  9,128,260, 9,131,209, 9,142,614, 9,159,615,10,  0,753,10,  4,755,10, 12,556,
 10, 22,564,10, 23,557,10, 35,558,10, 43,560,10, 47,419,10, 52,420,10, 54,421,
 10, 59,417,10, 71,418,10, 75,314,10, 80,319,10, 90,703,10, 98,707,10,100,708,
 10,103,735,10,105,736,10,110,301,10,117,248,10,118,252,10,120,253,10,121,254,
 10,123,255,10,124,256,10,126,248,10,128,260,10,131,209,10,142,210,10,146,614,
 10,159,615,10,161,616,10,164,617,10,167,615,11,  0,751,11,  2,753,11,  4,755,
 11, 12,556,11, 23,557,11, 35,558,11, 44,559,11, 47,415,11, 50,419,11, 52,416,
 11, 57,740,11, 59,417,11, 71,418,11, 75,314,11, 80,319,11, 90,703,11, 98,707,
 11,100,708,11,102,733,11,103,735,11,106,736,11,110,301,11,117,248,11,119,252,
 11,121,254,11,123,255,11,124,256,11,125,251,11,126,248,11,128,260,11,131,209,
 11,142,210,11,148,614,11,159,615,11,161,616,11,164,617,11,167,615,12,  0,751,
 12,  2,754,12,  4,755,12, 12,556,12, 22,557,12, 36,558,12, 43,555,12, 45,559,
 12, 47,415,12, 52,416,12, 57,740,12, 60,417,12, 71,418,12, 74,314,12, 81,319,
 12, 90,703,12, 98,707,12,100,733,12,103,735,12,107,736,12,110,301,12,117,248,
 12,120,250,12,122,249,12,124,251,12,126,248,12,128,260,12,131,209,12,141,210,
 12,148,215,12,150,611,12,159,615,12,161,616,12,164,617,12,167,615,13,  0,751,
 13,  1,754,13,  4,755,13,  7,754,13, 12,556,13, 22,557,13, 36,558,13, 43,555,
 13, 48,415,13, 52,416,13, 57,740,13, 60,417,13, 73,314,13, 81,319,13, 90,703,
 13, 97,298,13, 99,733,13,103,735,13,108,736,13,110,301,13,117,248,13,120,250,
 13,122,249,13,125,248,13,128,260,13,131,241,13,137,214,13,141,210,13,144,216,
 13,146,210,13,148,215,13,150,611,13,159,615,14,  0,754,14, 12,556,14, 22,557,
 14, 36,558,14, 42,555,14, 48,351,14, 51,415,14, 53,740,14, 61,417,14, 73,314,
 14, 81,319,14, 90,703,14, 97,298,14, 99,733,14,105,734,14,107,736,14,110,301,
 14,117,248,14,119,249,14,125,248,14,128,260,14,131,241,14,137,214,14,141,215,
 14,144,216,14,147,215,14,150,611,14,159,615,15,  0,745,15,  3,754,15, 12,556,
 15, 23,557,15, 37,558,15, 41,554,15, 42,555,15, 48,351,15, 53,740,15, 61,417,
 15, 73,314,15, 81,319,15, 94,298,15, 99,733,15,106,734,15,107,736,15,110,301,
 15,117,248,15,119,249,15,122,248,15,126,260,15,131,241,15,137,214,15,141,215,
 15,145,216,15,147,215,15,150,611,15,159,615,16,  0,745,16,  4,754,16, 12,556,
 16, 24,557,16, 37,558,16, 40,554,16, 42,555,16, 48,351,16, 56,740,16, 61,417,
 16, 73,314,16, 83,319,16, 94,298,16, 99,733,16,105,734,16,107,736,16,110,301,
 16,117,248,16,119,249,16,123,248,16,126,260,16,131,241,16,137,214,16,141,215,
 16,145,216,16,147,215,16,150,611,16,159,615,17,  0,745,17,  4,754,17, 12,556,
 17, 24,557,17, 38,558,17, 39,554,17, 41,555,17, 48,351,17, 57,740,17, 62,417,
 17, 72,314,17, 84,319,17, 94,296,17, 98,733,17,105,734,17,106,736,17,107,737,
 17,110,301,17,117,248,17,120,249,17,123,248,17,126,260,17,131,241,17,137,214,
 17,141,215,17,145,216,17,147,215,17,150,611,18,  0,745,18,  4,754,18, 12,556,
 18, 24,557,18, 39,554,18, 41,555,18, 48,351,18, 58,740,18, 62,417,18, 72,314,
 18, 85,319,18, 93,296,18, 98,733,18,101,734,18,105,736,18,107,737,18,108,300,
 18,112,301,18,117,248,18,120,249,18,123,248,18,126,260,18,131,241,18,137,214,
 18,141,215,18,144,216,18,147,215,18,150,611,19,  0,745,19,  3,551,19,  5,754,
 19, 12,556,19, 24,557,19, 38,554,19, 40,555,19, 48,351,19, 58,740,19, 63,417,
 19, 72,314,19, 87,319,19, 93,296,19, 98,733,19,101,734,19,104,736,19,106,737,
 19,108,300,19,112,301,19,117,248,19,126,260,19,131,241,19,137,214,19,141,215,
 19,144,216,19,146,215,19,150,611,20,  0,745,20,  1,551,20,  8,754,20, 12,556,
 20, 25,557,20, 38,554,20, 39,555,20, 45,351,20, 59,740,20, 63,417,20, 70,314,
 20, 87,319,20, 92,296,20,100,734,20,105,736,20,107,737,20,108,664,20,112,301,
 20,117,248,20,126,260,20,131,241,20,137,214,20,141,215,20,144,216,20,146,215,
 20,150,611,21,  0,551,21,  9,754,21, 12,556,21, 25,557,21, 38,554,21, 39,555,
 21, 45,351,21, 60,740,21, 64,417,21, 69,314,21, 89,316,21, 91,319,21, 92,295,
 21, 93,296,21, 99,297,21,102,734,21,103,736,21,108,664,21,112,242,21,117,243,
 21,123,247,21,126,241,21,137,214,21,141,215,21,150,611,22,  0,551,22, 10,754,
 22, 12,556,22, 25,553,22, 35,557,22, 37,554,22, 38,555,22, 45,351,22, 60,740,
 22, 65,356,22, 68,314,22, 88,315,22, 89,316,22, 92,315,22, 93,294,22, 94,296,
 22, 98,297,22,102,318,22,105,736,22,107,664,22,112,242,22,117,243,22,120,244,
 22,121,243,22,123,247,22,126,241,22,137,213,22,145,611,23,  0,551,23, 12,552,
 23, 25,553,23, 36,554,23, 38,555,23, 45,351,23, 60,355,23, 61,356,23, 67,710,
 23, 68,712,23, 69,314,23, 88,315,23, 89,316,23, 91,315,23, 93,294,23, 95,296,
 23, 97,297,23,100,318,23,106,664,23,112,242,23,118,243,23,120,244,23,122,243,
 23,123,246,23,126,239,23,132,241,23,137,213,23,145,611,24,  0,551,24, 10,552,
 24, 25,553,24, 36,554,24, 37,555,24, 45,351,24, 57,355,24, 61,356,24, 66,710,
 24, 68,712,24, 71,308,24, 88,315,24, 89,316,24, 91,315,24, 93,294,24, 95,296,
 24, 97,297,24, 99,318,24,105,664,24,114,242,24,119,243,24,120,244,24,122,243,
 24,123,246,24,126,238,24,127,239,24,132,241,24,137,213,24,145,611,24,177,612,
 25,  0,551,25, 10,552,25, 25,553,25, 35,554,25, 36,555,25, 44,351,25, 52,352,
 25, 55,351,25, 57,353,25, 62,354,25, 65,710,25, 70,712,25, 71,308,25, 88,315,
 25, 93,317,25, 94,294,25, 96,296,25, 97,297,25, 99,318,25,104,664,25,116,242,
 25,120,243,25,121,244,25,122,243,25,123,245,25,125,246,25,126,238,25,129,239,
 25,132,240,25,137,213,25,145,611,25,177,612,26,  0,551,26, 10,552,26, 25,553,
 26, 34,554,26, 36,555,26, 44,351,26, 52,352,26, 53,353,26, 62,354,26, 65,710,
 26, 69,712,26, 71,308,26, 84,309,26, 88,315,26, 90,317,26, 95,294,26, 96,296,
 26, 98,297,26, 99,318,26,102,307,26,103,318,26,104,664,26,117,242,26,121,243,
 26,123,245,26,126,238,26,130,239,26,132,240,26,137,212,26,145,611,26,177,612,
 27,  0,551,27, 10,552,27, 25,553,27, 34,554,27, 35,555,27, 44,351,27, 50,352,
 27, 52,353,27, 63,354,27, 65,710,27, 69,712,27, 72,308,27, 81,309,27, 85,310,
 27, 88,311,27, 89,312,27, 92,313,27, 93,317,27, 95,294,27, 98,297,27, 99,318,
 27,101,307,27,103,318,27,105,664,27,118,242,27,122,243,27,123,245,27,126,234,
 27,127,238,27,131,239,27,132,240,27,137,212,27,145,611,27,177,612,28,  0,396,
 28, 10,552,28, 25,553,28, 35,555,28, 44,351,28, 49,352,28, 51,353,28, 62,354,
 28, 65,710,28, 71,712,28, 73,308,28, 80,309,28, 82,310,28, 86,306,28, 92,313,
 28, 98,297,28, 99,318,28,100,307,28,106,664,28,108,307,28,109,664,28,122,666,
 28,126,234,28,128,238,28,131,239,28,132,240,28,137,212,28,145,611,28,177,612,
 29,  0,396,29, 10,552,29, 25,553,29, 35,555,29, 44,375,29, 47,351,29, 49,352,
 29, 50,353,29, 61,354,29, 65,710,29, 72,712,29, 74,308,29, 80,309,29, 81,310,
 29, 84,306,29, 94,313,29, 97,306,29, 99,307,29,109,664,29,123,666,29,126,234,
 29,129,238,29,131,239,29,132,237,29,137,211,29,144,611,29,177,612,30,  0,396,
 30,  9,397,30, 10,401,30, 25,553,30, 34,373,30, 37,374,30, 43,375,30, 47,346,
 30, 49,347,30, 50,348,30, 61,350,30, 65,709,30, 66,710,30, 73,712,30, 75,308,
 30, 79,305,30, 81,306,30, 99,307,30,109,664,30,123,666,30,126,234,30,129,235,
 30,132,237,30,137,211,30,144,611,30,177,612,31,  0,396,31,  9,397,31, 10,401,
 31, 18,400,31, 19,401,31, 25,553,31, 34,373,31, 37,374,31, 41,375,31, 47,346,
 31, 48,347,31, 50,348,31, 61,350,31, 65,709,31, 68,710,31, 74,712,31, 75,308,
 31, 78,305,31, 80,306,31, 99,307,31,110,664,31,122,666,31,126,234,31,129,235,
 31,132,237,31,137,211,31,144,611,31,177,612,32,  0,396,32,  8,397,32, 11,401,
 32, 16,400,32, 19,401,32, 25,371,32, 34,373,32, 37,374,32, 39,375,32, 46,346,
 32, 48,347,32, 50,348,32, 61,350,32, 65,709,32, 69,710,32, 74,711,32, 75,303,
 32, 78,304,32, 80,306,32, 96,325,32, 97,306,32, 98,307,32,108,664,32,122,666,
 32,126,234,32,128,235,32,132,236,32,135,237,32,137,211,32,144,611,33,  0,396,
 33,  8,397,33, 12,400,33, 25,371,33, 35,374,33, 39,375,33, 45,346,33, 47,347,
 33, 50,348,33, 61,349,33, 65,709,33, 70,710,33, 74,711,33, 75,302,33, 76,303,
 33, 77,302,33, 78,304,33, 80,306,33, 90,325,33,102,307,33,103,322,33,106,664,
 33,121,665,33,125,231,33,129,235,33,132,236,33,135,233,33,137,230,33,138,211,
 33,141,229,33,148,611,34,  0,396,34,  8,397,34, 12,400,34, 23,370,34, 27,371,
 34, 31,372,34, 35,374,34, 41,375,34, 45,346,34, 46,347,34, 50,348,34, 61,349,
 34, 65,709,34, 71,710,34, 74,711,34, 75,302,34, 78,304,34, 80,306,34, 90,325,
 34,102,322,34,107,664,34,121,665,34,125,231,34,130,232,34,133,233,34,136,232,
 34,137,230,34,140,228,34,141,229,34,148,611,35,  0,396,35,  8,397,35, 12,400,
 35, 23,370,35, 27,369,35, 28,371,35, 31,372,35, 35,374,35, 41,375,35, 45,346,
 35, 47,347,35, 50,348,35, 61,342,35, 64,349,35, 65,718,35, 71,710,35, 73,720,
 35, 75,302,35, 79,304,35, 81,321,35, 82,306,35, 90,325,35,103,322,35,109,664,
 35,121,665,35,125,231,35,130,660,35,132,232,35,137,227,35,139,230,35,140,228,
 35,142,229,35,148,611,36,  0,396,36,  8,397,36, 12,400,36, 14,398,36, 16,400,
 36, 21,368,36, 25,369,36, 29,366,36, 36,374,36, 41,375,36, 44,346,36, 46,345,
 36, 49,347,36, 50,348,36, 59,341,36, 62,340,36, 63,342,36, 65,718,36, 71,717,
 36, 73,720,36, 75,324,36, 79,321,36, 91,325,36,103,322,36,105,323,36,107,322,
 36,109,664,36,123,665,36,125,231,36,130,660,36,136,226,36,137,227,36,140,228,
 36,142,229,36,148,611,37,  0,387,37,  9,397,37, 11,398,37, 16,399,37, 21,368,
 37, 25,369,37, 27,366,37, 44,343,37, 45,345,37, 49,338,37, 53,341,37, 56,348,
 37, 57,341,37, 60,340,37, 64,342,37, 65,717,37, 72,715,37, 74,719,37, 76,321,
 37, 91,325,37,102,322,37,104,323,37,111,658,37,124,231,37,130,660,37,136,226,
 37,139,227,37,141,228,37,142,229,37,148,611,38,  0,377,38,  1,386,38,  5,387,
 38, 11,398,38, 16,390,38, 18,399,38, 20,364,38, 24,365,38, 27,366,38, 44,343,
 38, 45,344,38, 47,345,38, 48,344,38, 49,338,38, 53,340,38, 55,341,38, 58,340,
 38, 65,714,38, 68,715,38, 70,717,38, 72,715,38, 73,719,38, 75,321,38, 91,325,
 38,100,322,38,104,323,38,111,658,38,124,659,38,129,660,38,136,223,38,138,226,
 38,140,227,38,141,228,38,143,229,38,148,611,39,  0,377,39,  1,386,39,  5,387,
 39,  8,388,39, 10,389,39, 15,390,39, 19,392,39, 21,364,39, 23,365,39, 26,366,
 39, 44,344,39, 49,338,39, 53,340,39, 63,339,39, 65,714,39, 68,715,39, 72,716,
 39, 73,719,39, 74,321,39, 94,322,39,101,323,39,111,658,39,124,659,39,129,660,
 39,136,223,39,139,226,39,140,227,39,142,228,39,143,229,39,148,611,40,  0,377,
 40,  1,386,40,  5,387,40,  8,388,40, 10,389,40, 14,390,40, 18,382,40, 19,391,
 40, 20,392,40, 21,364,40, 24,365,40, 26,366,40, 43,367,40, 45,337,40, 50,338,
 40, 52,340,40, 62,339,40, 65,714,40, 69,715,40, 72,716,40, 74,320,40, 78,321,
 40, 94,322,40, 98,323,40,111,658,40,125,659,40,130,660,40,136,223,40,139,226,
 40,140,227,40,141,228,40,143,229,40,148,611,41,  0,377,41,  4,387,41,  8,380,
 41, 10,389,41, 12,390,41, 17,382,41, 19,391,41, 21,383,41, 23,363,41, 27,366,
 41, 41,367,41, 44,362,41, 45,337,41, 49,338,41, 52,340,41, 55,336,41, 56,339,
 41, 57,340,41, 61,339,41, 65,713,41, 69,716,41, 76,320,41, 80,321,41, 95,322,
 41, 97,323,41,111,658,41,126,659,41,131,660,41,136,223,41,139,224,41,146,225,
 41,150,611,42,  0,378,42,  4,387,42,  8,380,42, 10,381,42, 15,382,42, 18,383,
 42, 22,359,42, 28,360,42, 41,362,42, 45,337,42, 48,338,42, 52,336,42, 56,339,
 42, 58,340,42, 59,339,42, 65,713,42, 71,716,42, 75,330,42, 80,320,42, 81,332,
 42, 96,322,42, 97,323,42,100,334,42,111,658,42,129,659,42,130,657,42,132,661,
 42,135,660,42,136,223,42,139,224,42,146,225,42,150,611,43,  0,538,43,  3,379,
 43,  8,380,43, 10,381,43, 14,382,43, 16,383,43, 23,359,43, 28,360,43, 39,362,
 43, 45,337,43, 48,338,43, 51,336,43, 56,339,43, 62,336,43, 64,339,43, 65,713,
 43, 75,330,43, 80,331,43, 81,332,43, 96,334,43,112,658,43,130,657,43,132,661,
 43,136,223,43,140,224,43,146,221,43,148,222,43,153,611,44,  0,538,44,  7,545,
 44, 13,382,44, 15,383,44, 22,358,44, 29,360,44, 33,361,44, 37,362,44, 44,357,
 44, 48,338,44, 50,336,44, 56,339,44, 61,336,44, 65,713,44, 75,329,44, 80,331,
 44, 81,332,44, 95,334,44,112,658,44,130,657,44,134,661,44,137,223,44,141,224,
 44,146,221,44,150,222,44,155,611,45,  0,538,45,  7,545,45, 14,383,45, 21,358,
 45, 28,357,45, 32,361,45, 37,357,45, 48,338,45, 51,336,45, 58,339,45, 59,336,
 45, 65,713,45, 75,329,45, 80,331,45, 83,332,45, 91,334,45,115,658,45,130,657,
 45,134,661,45,138,223,45,141,224,45,146,221,45,152,222,45,164, 16,46,  0,538,
 46,  6,544,46, 10,545,46, 13,546,46, 14,383,46, 17,549,46, 21,358,46, 28,357,
 46, 49,336,46, 50,338,46, 53,336,46, 65,713,46, 75,329,46, 82,331,46, 85,332,
 46, 91,334,46,117,658,46,133,657,46,135,661,46,141,662,46,144,663,46,146,220,
 46,149,221,46,154,222,46,164, 16,47,  0,538,47,  7,544,47,  9,543,47, 10,546,
 47, 17,549,47, 22,358,47, 28,357,47, 48,336,47, 65,713,47, 75,329,47, 82,331,
 47, 86,332,47, 91,334,47,119,658,47,130,657,47,135,661,47,141,662,47,144,663,
 47,146,220,47,150,221,47,155,222,47,164, 16,48,  0,538,48,  8,543,48, 13,546,
 48, 17,547,48, 21,549,48, 22,357,48, 47,336,48, 65,713,48, 75,329,48, 85,331,
 48, 87,332,48, 88,334,48,116,658,48,129,657,48,135,661,48,141,662,48,144,663,
 48,148,220,48,151,221,48,156,222,48,164, 16,49,  0,538,49,  6,543,49, 13,547,
 49, 19,548,49, 23,357,49, 47,336,49, 65,713,49, 75,329,49, 86,331,49, 88,333,
 49, 91,334,49, 94,333,49, 99,334,49,107,333,49,117,657,49,120,658,49,126,657,
 49,131,656,49,137,661,49,141,662,49,145,663,49,148,220,49,153,221,49,157,222,
 49,164, 16,50,  0,533,50,  1,538,50,  3,541,50,  6,543,50, 13,547,50, 16,548,
 50, 24,724,50, 49,336,50, 65,713,50, 75,329,50, 84,326,50, 89,333,50, 99,334,
 50,102,333,50,109,327,50,113,333,50,117,328,50,118,657,50,120,658,50,126,657,
 50,128,656,50,137,661,50,141,662,50,145,663,50,150,220,50,153,221,50,158,222,
 50,164, 16,50,175,  6,51,  0,533,51,  2,534,51,  3,540,51,  6,543,51, 15,548,
 51, 24,724,51, 50,336,51, 53,724,51, 55,335,51, 61,336,51, 65,713,51, 75,329,
 51, 80,326,51, 97,333,51,103,327,51,113,328,51,119,657,51,121,658,51,126,657,
 51,128,656,51,137,661,51,141,662,51,144,663,51,152,220,51,156,217,51,157,218,
 51,159,219,51,164, 16,51,170,  5,51,175,  6,52,  0,533,52,  2,534,52,  4,540,
 52,  7,543,52, 14,548,52, 24,724,52, 55,335,52, 61,336,52, 65,713,52, 75,329,
 52, 79,326,52, 97,333,52,101,326,52,102,327,52,113,328,52,119,657,52,121,658,
 52,125,657,52,127,656,52,137,661,52,141,662,52,144,663,52,152,220,52,156,217,
 52,158,218,52,159,219,52,164, 16,52,170,  5,52,175,  6,53,  0,533,53,  1,534,
 53,  5,540,53,  7,543,53, 14,548,53, 24,724,53, 55,335,53, 61,336,53, 65,713,
 53, 75,329,53, 78,326,53,102,327,53,113,328,53,119,657,53,127,656,53,137,661,
 53,141,662,53,144,663,53,155,217,53,158,218,53,161,219,53,164,  4,53,170,  5,
 53,175,  6,54,  0,534,54,  8,543,54, 14,537,54, 16,548,54, 20,724,54, 55,335,
 54, 63,326,54, 65,713,54, 71,326,54,102,327,54,113,328,54,120,656,54,137,661,
 54,141,662,54,144,663,54,155,217,54,159,218,54,163,219,54,164,  4,54,170,  5,
 54,175,  3,55,  0,534,55,  8,542,55, 13,536,55, 15,537,55, 21,724,55, 55,335,
 55, 63,326,55,102,327,55,113,328,55,120,656,55,139,663,55,155,217,55,161,218,
 55,163,219,55,164,  4,55,170,  3,56,  0,534,56,  8,542,56, 12,536,56, 17,537,
 56, 21,724,56, 55,335,56, 63,326,56,102,327,56,113,328,56,120,656,56,139,663,
 56,155,217,56,161,218,56,164,  4,56,170,  3,57,  0,534,57,  8,542,57, 11,536,
 57, 17,537,57, 21,724,57, 55,335,57, 63,326,57,102,327,57,113,328,57,120,656,
 57,141,663,57,156,217,57,162,218,57,164,  3,58,  0,534,58,  5,535,58, 10,536,
 58, 18,537,58, 21,724,58, 55,335,58, 63,326,58,110,328,58,120,656,58,142,671,
 58,155,663,58,157,217,58,165,  3,59,  0,534,59,  4,535,59, 12,536,59, 19,537,
 59, 21,724,59, 55,335,59, 63,326,59,110,328,59,120,656,59,142,671,59,156,663,
 59,160,217,59,165,671,59,171,  3,60,  0,534,60,  4,535,60, 12,536,60, 19,721,
 60, 27,723,60, 29,724,60, 55,335,60, 63,725,60, 85,726,60,130,671,60,173,  3,
 61,  0,642,61,  4,535,61, 12,536,61, 19,721,61, 28,723,61, 31,724,61, 55,335,
 61, 63,725,61, 85,726,61,130,671,61,176,  3,62,  0,642,62,  5,535,62, 12,536,
 62, 20,721,62, 30,723,62, 32,724,62, 55,335,62, 63,725,62, 85,726,62,130,671,
 63,  0,642,63,  7,535,63, 12,536,63, 21,721,63, 30,723,63, 32,724,63, 55,335,
 63, 65,725,63, 85,726,63,130,671,64,  0,642,64, 10,646,64, 14,536,64, 23,721,
 64, 29,723,64, 31,724,64, 55,335,64, 65,725,64, 85,726,64,130,671,65,  0,642,
 65, 10,646,65, 14,536,65, 24,721,65, 29,723,65, 31,724,65, 55,335,65, 68,725,
 65, 85,726,65,130,671,66,  0,642,66, 11,646,66, 16,536,66, 24,721,66, 29,723,
 66, 30,724,66, 55,335,66, 68,725,66, 85,726,66,130,671,67,  0,642,67, 12,646,
 67, 17,536,67, 24,721,67, 29,723,67, 30,724,67, 61,335,67, 68,725,67, 85,726,
 67,130,671,67,178,670,68,  0,642,68, 12,646,68, 20,536,68, 22,721,68, 28,723,
 68, 30,724,68, 40,647,68, 43,724,68, 64,335,68, 68,725,68, 85,726,68,130,671,
 68,158,670,69,  0,642,69, 15,646,69, 26,721,69, 28,723,69, 29,722,69, 32,724,
 69, 37,647,69, 48,724,69, 66,725,69, 85,726,69,130,671,69,150,670,70,  0,642,
 70, 18,646,70, 31,647,70, 50,648,70, 58,724,70, 61,649,70, 66,725,70, 85,726,
 70,130,670,70,161,669,70,168,670,70,172,669,70,178,670,71,  0,642,71, 20,647,
 71, 23,646,71, 29,647,71, 50,648,71, 59,649,71, 66,725,71, 85,726,71,130,670,
 71,159,669,71,178,670,72,  0,642,72, 20,647,72, 50,648,72, 59,649,72, 68,650,
 72, 85,726,72,130,655,72,139,670,72,151,669,73,  0,640,73, 10,642,73, 20,647,
 73, 52,648,73, 59,649,73, 69,650,73, 85,726,73,110,653,73,130,655,73,135,668,
 73,145,669,74,  0,640,74, 10,642,74, 20,647,74, 52,648,74, 62,649,74, 79,650,
 74, 85,653,74, 88,726,74,110,653,74,130,655,74,135,668,74,155,669,75,  0,640,
 75, 10,643,75, 25,647,75, 54,648,75, 67,649,75, 81,650,75, 85,653,75,116,655,
 75,135,668,75,155,669,76,  0,640,76,  5,643,76, 25,647,76, 58,648,76, 70,649,
 76, 88,653,76,116,655,76,135,668,76,161,669,77,  0,640,77,  5,643,77, 35,647,
 77, 65,648,77, 70,649,77, 88,653,77, 97,652,77,101,653,77,116,655,77,135,667,
 77,154,668,77,161,634,78,  0,640,78,  5,643,78, 35,647,78, 70,649,78, 93,652,
 78,110,654,78,135,667,78,161,634,79,  0,640,79,  5,643,79, 35,645,79, 70,649,
 79, 85,652,79,110,654,79,135,667,79,161,634,80,  0,641,80,  5,643,80, 35,645,
 80, 70,649,80, 85,652,80,110,654,80,135,633,80,155,634,81,  0,641,81,  5,643,
 81, 35,645,81, 70,649,81, 85,652,81,110,654,81,135,633,81,155,634,82,  0,641,
 82, 35,645,82, 70,651,82,135,633,82,155,634,83,  0,641,83, 35,644,83, 70,651,
 83,135,633,83,155,634,84,  0,641,84, 35,644,84, 70,651,84,130,633,84,160,634,
 85,  0,641,85, 35,644,85, 70,651,85,120,633,85,170,634,86,  0,641,86, 35,644,
 86, 70,651,86,120,633,86,170,634,87,  0,641,87, 35,644,87, 70,651,87,100,633,
 88,  0,633,89,  0,633,90,180,633, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,561, 0,  9,407, 0, 18,406, 0, 32,402, 0, 49,528, 0, 62,113,
  0, 70,103, 0, 75,106, 0, 79,105, 0, 81,104, 0, 86,696, 0, 89,697, 0, 92,696,
  0, 96,695, 0,100,757, 0,106,693, 0,120,611, 0,153,620, 0,165,611, 1,  0,561,
  1,  9,407, 1, 18,406, 1, 33,402, 1, 49,528, 1, 58,529, 1, 60,528, 1, 62,113,
  1, 64,101, 1, 67,113, 1, 70,103, 1, 78,106, 1, 79,105, 1, 80,104, 1, 86,696,
  1, 96,695, 1,100,757, 1,106,693, 1,120,611, 1,153,620, 1,156,622, 1,161,620,
  1,165,611, 2,  0,561, 2, 18,402, 2, 20,406, 2, 34,402, 2, 49,528, 2, 53,531,
  2, 54,530, 2, 55,528, 2, 56,530, 2, 57,529, 2, 60,528, 2, 62,113, 2, 64,101,
  2, 67,103, 2, 77,102, 2, 79, 83, 2, 83, 76, 2, 86,696, 2, 96,695, 2,100,757,
  2,106,693, 2,120,611, 2,153,620, 2,156,622, 2,161,620, 2,165,611, 3,  0,561,
  3, 18,402, 3, 26,406, 3, 35,402, 3, 50,528, 3, 52,531, 3, 54,530, 3, 58,529,
  3, 60,528, 3, 62,113, 3, 64,101, 3, 67,103, 3, 77,102, 3, 78, 83, 3, 83, 76,
  3, 89,693, 3,100,757, 3,106,693, 3,120,611, 3,153,620, 3,156,622, 3,161,620,
  3,165,611, 4,  0,752, 4,  3,750, 4,  7,749, 4, 13,561, 4, 18,402, 4, 28,406,
  4, 36,402, 4, 50,531, 4, 54,530, 4, 58,529, 4, 60,528, 4, 61,101, 4, 68,103,
  4, 77,102, 4, 78, 83, 4, 83, 76, 4, 89,693, 4,100,757, 4,106,693, 4,120,611,
  4,153,620, 4,156,622, 4,161,620, 4,165,611, 5,  0,752, 5,  3,750, 5,  7,749,
  5, 13,561, 5, 18,402, 5, 30,406, 5, 38,402, 5, 50,531, 5, 54,530, 5, 57,529,
  5, 61,101, 5, 68,103, 5, 77,102, 5, 78, 83, 5, 83, 76, 5, 90,693, 5,100,757,
  5,106,693, 5,120,611, 5,153,620, 5,161,621, 5,164,620, 5,165,611, 6,  0,752,
  6,  3,750, 6,  8,749, 6, 12,748, 6, 13,561, 6, 18,402, 6, 30,406, 6, 40,402,
  6, 52,531, 6, 54,530, 6, 57,529, 6, 61,101, 6, 70, 99, 6, 77,102, 6, 78, 83,
  6, 83, 76, 6, 90,693, 6,100,757, 6,106,693, 6,120,611, 6,153,620, 6,161,621,
  6,164,620, 6,165,611, 7,  0,752, 7,  3,750, 7,  8,749, 7, 11,748, 7, 15,561,
  7, 18,402, 7, 32,406, 7, 40,402, 7, 57,529, 7, 61,101, 7, 72, 99, 7, 77, 82,
  7, 78, 81, 7, 79, 83, 7, 80, 81, 7, 82, 83, 7, 83, 77, 7, 86, 76, 7, 91,693,
  7, 97, 63, 7,100,730, 7,106,693, 7,120,611, 7,153,620, 7,165,611, 8,  0,752,
  8,  3,750, 8,  8,747, 8, 10,748, 8, 15,561, 8, 18,402, 8, 32,406, 8, 43,402,
  8, 58,529, 8, 60, 97, 8, 61,101, 8, 73, 99, 8, 76, 96, 8, 77, 82, 8, 78, 81,
  8, 82, 80, 8, 83, 78, 8, 84, 77, 8, 87, 76, 8, 92,693, 8, 96, 63, 8,100,730,
  8,106,693, 8,120,611, 8,153,620, 8,165,611, 9,  0,752, 9,  3,750, 9,  8,747,
  9, 11,748, 9, 13,747, 9, 15,561, 9, 18,402, 9, 35,406, 9, 44,402, 9, 60, 97,
  9, 63,101, 9, 71,100, 9, 72,101, 9, 73, 99, 9, 75, 96, 9, 78, 81, 9, 82, 80,
  9, 83, 78, 9, 85, 77, 9, 88, 76, 9, 92,693, 9, 95, 63, 9,100,730, 9,106, 63,
  9,110,693, 9,120,611, 9,153,620, 9,165,611,10,  0,752,10,  3,751,10,  6,750,
 10,  7,745,10,  8,747,10, 18,402,10, 40,403,10, 45,402,10, 60, 98,10, 62, 97,
 10, 69,101,10, 71,100,10, 72,101,10, 73, 99,10, 74, 96,10, 76, 79,10, 83, 78,
 10, 86, 77,10, 88, 76,10, 93, 63,10,100,730,10,106, 63,10,110,693,10,120,611,
 11,  0,751,11,  5,745,11,  9,747,11, 18,402,11, 42,403,11, 46,402,11, 60, 95,
 11, 63, 94,11, 68, 97,11, 72, 96,11, 76, 79,11, 83, 75,11, 86, 74,11, 88, 76,
 11, 93, 63,11,100,730,11,106, 63,11,110,693,11,120,611,12,  0,751,12,  4,745,
 12, 11,746,12, 13,747,12, 16,746,12, 18,402,12, 43,403,12, 47,402,12, 59, 95,
 12, 63, 94,12, 69, 97,12, 71, 96,12, 73, 94,12, 83, 75,12, 87, 74,12, 88, 76,
 12, 93, 68,12, 96, 63,12,100,730,12,106, 63,12,113,693,12,120,611,13,  0,751,
 13,  3,745,13, 12,746,13, 18,402,13, 43,403,13, 48,402,13, 59, 95,13, 63, 94,
 13, 83, 75,13, 87, 72,13, 88, 73,13, 90, 71,13, 92, 68,13, 96, 67,13, 98, 65,
 13,104,730,13,106, 63,13,113,693,13,120,611,14,  0,751,14,  2,745,14, 12,746,
 14, 18,402,14, 43,403,14, 48,402,14, 59, 95,14, 63, 94,14, 83, 75,14, 85, 72,
 14, 89, 70,14, 92, 69,14, 94, 68,14, 95, 67,14, 98, 65,14,104,730,14,106, 63,
 14,115,693,14,120,611,15,  0,745,15, 10,744,15, 11,745,15, 12,744,15, 13,746,
 15, 18,402,15, 43,403,15, 48,402,15, 59, 92,15, 64, 94,15, 83, 72,15, 89, 70,
 15, 91, 62,15, 93, 69,15, 94, 66,15, 98, 65,15,104, 64,15,106, 63,15,115,693,
 15,120,611,15,150,612,15,165,619,15,172,611,16,  0,745,16,  5,744,16, 14,746,
 16, 18,402,16, 43,403,16, 48,402,16, 59, 92,16, 64, 94,16, 81,731,16, 88, 93,
 16, 89, 70,16, 90, 62,16, 92, 61,16, 94, 60,16, 98, 58,16,102, 65,16,103, 64,
 16,106, 63,16,115,693,16,120,611,16,150,612,16,165,619,16,172,611,17,  0,745,
 17,  6,744,17, 18,402,17, 43,403,17, 48,402,17, 59, 92,17, 64, 91,17, 65, 90,
 17, 67, 89,17, 69, 88,17, 72, 87,17, 75, 86,17, 79,731,17, 88, 93,17, 89, 62,
 17, 92, 61,17, 95, 60,17, 98, 59,17,101, 58,17,102, 56,17,104, 64,17,105, 54,
 17,108, 53,17,115,693,17,120,611,17,150,612,17,165,619,17,172,611,18,  0,745,
 18,  6,744,18, 18,402,18, 43,403,18, 48,402,18, 60, 92,18, 64, 91,18, 65, 90,
 18, 67, 89,18, 69, 88,18, 72, 87,18, 75, 86,18, 79,731,18, 87, 84,18, 91,527,
 18, 95,525,18, 97,523,18, 99, 59,18,102, 57,18,103, 56,18,104, 55,18,105, 54,
 18,108, 53,18,115,693,18,120,611,18,150,612,18,154,613,18,157,612,18,172,611,
 19,  0,745,19,  6,744,19, 18,402,19, 43,403,19, 48,402,19, 60, 92,19, 64, 91,
 19, 65, 90,19, 67, 89,19, 68,402,19, 69, 88,19, 72, 87,19, 74, 85,19, 81,731,
 19, 87, 84,19, 91,527,19, 95,525,19, 98,523,19,100, 57,19,103,524,19,104, 55,
 19,106, 54,19,108, 53,19,115,693,19,120,611,19,150,612,19,154,613,19,157,612,
 19,172,611,20,  0,745,20,  6,744,20, 18,402,20, 43,403,20, 48,402,20, 69, 88,
 20, 72, 87,20, 74, 85,20, 86, 84,20, 91,527,20, 96,525,20, 99,523,20,102,524,
 20,105, 55,20,106, 54,20,108, 53,20,115,693,20,120,611,20,150,612,20,154,613,
 20,159,612,20,175,611,21,  0,745,21,  6,744,21, 13,743,21, 18,402,21, 43,403,
 21, 48,402,21, 69,515,21, 74, 85,21, 86, 84,21, 91,526,21, 97,525,21, 99,523,
 21,101,524,21,104,523,21,105, 52,21,106, 51,21,108, 53,21,115,693,21,120,611,
 21,150,612,21,155,613,21,161,612,21,175,611,22,  0,551,22,  1,745,22,  6,744,
 22, 13,743,22, 18,402,22, 43,403,22, 48,402,22, 69,515,22, 76, 85,22, 86,526,
 22, 97,525,22, 99,523,22,105, 52,22,107, 51,22,109, 47,22,117,693,22,120,611,
 22,150,612,22,157,613,22,161,612,22,178,611,23,  0,551,23,  3,745,23,  6,744,
 23, 12,743,23, 18,402,23, 43,403,23, 48,402,23, 69,515,23, 78, 85,23, 86,526,
 23, 97,523,23,106, 52,23,107, 49,23,109, 48,23,111, 47,23,118,693,23,120,611,
 23,150,612,23,178,611,24,  0,551,24,  4,745,24,  7,744,24, 12,743,24, 18,402,
 24, 43,403,24, 48,402,24, 69,513,24, 72,515,24, 80,514,24, 82,526,24, 97,523,
 24,108, 49,24,110, 48,24,113, 47,24,119,693,24,120,611,24,150,612,25,  0,551,
 25,  6,744,25, 12,743,25, 16,394,25, 22,402,25, 42,403,25, 48,402,25, 69,513,
 25, 73,515,25, 80,514,25, 82,526,25, 97,518,25, 98,522,25,108, 49,25,111, 48,
 25,113, 47,25,120,611,25,150,612,26,  0,551,26,  7,744,26,  9,743,26, 15,394,
 26, 22,402,26, 42,403,26, 48,402,26, 69,513,26, 74,515,26, 80,514,26, 83,526,
 26, 97,520,26, 98,518,26,100,522,26,109, 49,26,112, 48,26,114, 47,26,120,611,
 26,150,612,27,  0,551,27,  9,743,27, 14,394,27, 22,402,27, 41,403,27, 47,402,
 27, 69,513,27, 75,515,27, 80,514,27, 83,526,27, 97,520,27, 98,519,27, 99,518,
 27,100,522,27,106,521,27,108, 50,27,110, 49,27,112, 48,27,115, 47,27,121,611,
 27,160,612,28,  0,396,28,  7,395,28, 13,394,28, 22,402,28, 39,403,28, 47,402,
 28, 69,513,28, 80,514,28, 83,526,28, 95,520,28, 98,519,28,100,518,28,101,522,
 28,104,521,28,109, 50,28,111, 49,28,113, 48,28,115, 47,28,121,611,28,160,612,
 29,  0,396,29,  5,395,29, 11,394,29, 22,402,29, 38,403,29, 47,402,29, 69,513,
 29, 80,514,29, 83,508,29, 86,526,29, 89,504,29, 94,503,29, 95,500,29,100,518,
 29,105,521,29,109, 50,29,112, 49,29,114, 48,29,116, 47,29,122,611,29,165,612,
 30,  0,396,30,  3,395,30, 10,394,30, 22,402,30, 36,403,30, 46,402,30, 69,513,
 30, 81,510,30, 83,508,30, 88,505,30, 90,504,30, 94,503,30, 95,500,30,100,498,
 30,104,518,30,106,521,30,109, 50,30,113, 49,30,115, 48,30,117, 47,30,122,611,
 30,165,612,31,  0,396,31,  3,395,31, 10,393,31, 22,402,31, 34,403,31, 44,402,
 31, 69,513,31, 81,509,31, 85,507,31, 88,505,31, 92,504,31, 94,503,31, 95,500,
 31,100,498,31,105,518,31,107,517,31,109,516,31,112, 46,31,113, 49,31,115, 48,
 31,117, 47,31,122,611,31,170,612,32,  0,396,32,  1,395,32, 10,393,32, 22,402,
 32, 32,403,32, 43,402,32, 69,513,32, 79,511,32, 81,509,32, 85,507,32, 88,505,
 32, 91,504,32, 94,503,32, 95,500,32,100,498,32,103,496,32,109,495,32,112, 46,
 32,115, 45,32,118, 38,32,125,611,33,  0,396,33,  2,395,33,  9,384,33, 10,393,
 33, 22,402,33, 30,403,33, 42,402,33, 69,513,33, 78,511,33, 82,509,33, 85,507,
 33, 88,505,33, 91,502,33, 94,501,33, 95,500,33,100,498,33,103,496,33,109,495,
 33,112, 42,33,114, 44,33,115, 43,33,120, 38,33,127,611,34,  0,396,34,  2,395,
 34,  8,384,34, 10,393,34, 20,404,34, 35,403,34, 41,402,34, 69,513,34, 76,512,
 34, 79,511,34, 83,509,34, 85,507,34, 88,505,34, 91,502,34, 94,501,34, 95,499,
 34,100,497,34,103,496,34,109,495,34,112, 42,34,114, 44,34,115, 43,34,121, 38,
 34,127,611,35,  0,396,35,  2,385,35,  8,384,35, 10,739,35, 20,404,35, 35,403,
 35, 39,402,35, 69,513,35, 75,512,35, 84,506,35, 90,502,35, 94,501,35, 95,499,
 35,100,497,35,103,496,35,109,495,35,112, 42,35,114, 40,35,116, 39,35,122, 38,
 35,130,611,36,  0,387,36,  2,385,36,  8,384,36, 10,739,36, 20,404,36, 25,405,
 36, 27,404,36, 35,403,36, 37,402,36, 69,513,36, 75,512,36, 82,506,36, 89,486,
 36, 91,484,36, 94,501,36, 95,499,36,100,497,36,103,496,36,109,495,36,112, 42,
 36,114, 41,36,115, 40,36,118, 39,36,123, 38,36,132,611,37,  0,377,37,  7,376,
 37, 10,739,37, 20,404,37, 25,405,37, 31,404,37, 35,403,37, 36,402,37, 69,513,
 37, 75,493,37, 77,492,37, 80,491,37, 83,490,37, 88,488,37, 89,487,37, 90,485,
 37, 91,483,37, 94,482,37, 95,480,37,102,479,37,109,478,37,114, 41,37,117, 40,
 37,119, 39,37,123, 38,37,132,611,38,  0,377,38,  7,376,38, 10,739,38, 20,404,
 38, 25,405,38, 31,404,38, 35,402,38, 69,513,38, 75,493,38, 77,492,38, 80,491,
 38, 83,490,38, 86,489,38, 88,488,38, 90,485,38, 91,483,38, 94,482,38, 95,480,
 38,102,479,38,109,478,38,114, 37,38,118, 40,38,120, 36,38,123, 35,38,124, 34,
 38,132,611,39,  0,377,39,  7,376,39, 10,402,39, 20,404,39, 27,405,39, 33,404,
 39, 35,402,39, 69,513,39, 74,494,39, 75,493,39, 77,492,39, 79,491,39, 81,471,
 39, 85,469,39, 87,467,39, 91,483,39, 94,482,39, 95,480,39,102,479,39,109,478,
 39,114, 37,39,120, 36,39,123, 35,39,125, 34,39,132,611,40,  0,377,40,  7,376,
 40, 10,402,40, 20,404,40, 35,402,40, 69,513,40, 72,472,40, 74,494,40, 75,473,
 40, 81,471,40, 85,469,40, 87,467,40, 91,481,40, 95,465,40, 96,463,40,102,479,
 40,109,478,40,114, 37,40,120, 36,40,124, 35,40,125, 34,40,132,611,41,  0,377,
 41,  6,376,41, 10,402,41, 20,404,41, 35,402,41, 69,476,41, 73,472,41, 75,473,
 41, 80,471,41, 85,469,41, 87,467,41, 91,465,41, 96,463,41,104,460,41,111,478,
 41,114, 37,41,120, 36,41,124, 35,41,125, 34,41,132,611,42,  0,378,42,  2,377,
 42, 10,402,42, 20,404,42, 35,402,42, 66,477,42, 70,476,42, 73,472,42, 80,470,
 42, 83,468,42, 87,467,42, 88,466,42, 91,465,42, 97,463,42,104,460,42,111,457,
 42,114, 33,42,117, 32,42,124, 31,42,125, 30,42,132,611,43,  0,378,43,  2,377,
 43, 10,402,43, 20,403,43, 35,402,43, 65,452,43, 67,477,43, 70,475,43, 71,474,
 43, 73,472,43, 79,470,43, 82,468,43, 87,466,43, 91,464,43, 97,462,43,104,460,
 43,111,457,43,114, 33,43,117, 32,43,124, 31,43,125, 30,43,132,611,44,  0,538,
 44,  2,539,44,  5,402,44, 20,403,44, 35,402,44, 62,452,44, 67,475,44, 71,474,
 44, 73,472,44, 76,470,44, 82,468,44, 87,466,44, 92,464,44, 97,462,44,104,460,
 44,109,459,44,111,458,44,112,457,44,114, 33,44,117, 32,44,124, 31,44,125, 30,
 44,132,611,45,  0,538,45,  2,539,45,  5,402,45, 22,403,45, 35,402,45, 59,452,
 45, 65,451,45, 67,475,45, 71,447,45, 74,470,45, 83,468,45, 88,466,45, 93,464,
 45, 97,462,45,104,456,45,114, 33,45,117, 32,45,119, 28,45,124, 31,45,125, 30,
 45,132, 21,45,142, 17,45,165, 16,46,  0,538,46,  2,539,46,  5,402,46, 23,403,
 46, 35,402,46, 52,455,46, 56,402,46, 59,452,46, 62,453,46, 64,451,46, 68,475,
 46, 70,447,46, 79,441,46, 84,468,46, 90,466,46, 92,464,46, 97,461,46,104,456,
 46,114, 33,46,117, 29,46,123, 28,46,124, 27,46,125, 26,46,132, 21,46,142, 17,
 46,165, 16,47,  0,538,47,  5,402,47, 24,403,47, 34,402,47, 52,455,47, 59,454,
 47, 64,451,47, 68,448,47, 70,447,47, 79,441,47, 86,468,47, 90,464,47, 97,461,
 47,104,456,47,116, 33,47,117, 29,47,124, 27,47,125, 26,47,132, 21,47,142, 17,
 47,165, 16,48,  0,538,48,  5,402,48, 24,403,48, 34,402,48, 52,446,48, 53,455,
 48, 59,454,48, 64,448,48, 70,447,48, 79,441,48, 94,464,48, 97,461,48,104,456,
 48,116, 33,48,117, 29,48,123, 25,48,132, 21,48,142, 17,48,165, 16,49,  0,538,
 49,  3,402,49, 24,403,49, 34,402,49, 52,446,49, 53,455,49, 59,454,49, 61,450,
 49, 65,448,49, 67,447,49, 79,441,49, 95,439,49,102,438,49,110, 24,49,114, 23,
 49,123, 25,49,132, 21,49,142, 17,49,165, 16,50,  0,533,50,  6,402,50, 24,403,
 50, 33,402,50, 52,446,50, 55,455,50, 58,449,50, 67,447,50, 79,441,50, 95,439,
 50,102,438,50,110, 24,50,115, 23,50,125, 25,50,132, 21,50,142, 17,50,165, 16,
 50,172,  7,51,  0,533,51,  6,532,51, 11,402,51, 24,403,51, 33,402,51, 52,446,
 51, 55,455,51, 57,449,51, 67,443,51, 80,441,51, 95,439,51,102,438,51,110, 24,
 51,117, 23,51,127, 25,51,129, 22,51,135, 21,51,142, 17,51,165, 16,51,167,  9,
 51,172,  7,52,  0,533,52,  6,532,52, 11,402,52, 25,403,52, 32,738,52, 37,402,
 52, 52,446,52, 55,445,52, 66,443,52, 80,441,52, 95,439,52,102,438,52,110, 24,
 52,118, 23,52,129, 22,52,135, 21,52,142, 17,52,165, 16,52,166,  9,52,172,  7,
 53,  0,533,53,  6,532,53, 11,402,53, 25,403,53, 32,738,53, 37,402,53, 52,446,
 53, 55,445,53, 68,443,53, 81,441,53, 95,439,53,102,438,53,110, 24,53,120, 23,
 53,130, 22,53,135, 21,53,142, 17,53,163, 10,53,165,  9,53,172,  7,54,  0,533,
 54,  8,532,54, 11,402,54, 32,738,54, 37,402,54, 52,446,54, 57,445,54, 68,443,
 54, 81,441,54, 93,439,54,102,438,54,110, 24,54,120, 23,54,130, 19,54,131, 22,
 54,135, 21,54,142, 17,54,160, 12,54,163, 10,54,165,  9,54,172,  3,55,  0,534,
 55,  1,533,55,  9,402,55, 32,738,55, 37,402,55, 52,446,55, 59,445,55, 64,443,
 55, 78,440,55, 86,441,55, 91,439,55,102,438,55,110, 24,55,120, 23,55,130, 19,
 55,135, 20,55,142, 15,55,151, 17,55,157, 12,55,163, 10,55,165,  9,55,169,  3,
 56,  0,534,56,  1,533,56,  8,402,56, 31,738,56, 37,402,56, 52,446,56, 61,445,
 56, 64,443,56, 77,440,56, 88,441,56, 90,439,56,102,438,56,110, 24,56,120, 23,
 56,130, 19,56,136, 20,56,142, 15,56,151, 13,56,155, 12,56,162, 11,56,163,  3,
 56,167,  8,56,172,  3,57,  0,534,57,  1,533,57,  8,402,57, 30,738,57, 37,402,
 57, 52,446,57, 61,445,57, 64,443,57, 78,440,57, 90,439,57,102,438,57,110, 24,
 57,120, 23,57,132, 19,57,137, 20,57,142, 15,57,151, 13,57,155, 12,57,159, 11,
 57,163,  3,57,167,  8,57,172,  3,58,  0,534,58,  2,533,58,  8,402,58, 29,738,
 58, 35,402,58, 52,446,58, 62,445,58, 64,443,58, 78,440,58, 93,439,58,102,438,
 58,110, 24,58,120, 23,58,133, 19,58,139, 20,58,142, 15,58,151, 13,58,154, 12,
 58,158, 11,58,163,  3,59,  0,533,59,  4,402,59, 28,738,59, 33,402,59, 52,446,
 59, 63,445,59, 65,443,59, 66,442,59, 69,443,59, 78,440,59, 94,439,59,102,438,
 59,110, 24,59,120, 23,59,134, 19,59,142, 15,59,148, 14,59,152,  2,59,168,  3,
 60,  0,533,60,  4,402,60, 27,738,60, 31,402,60, 42,680,60, 49,444,60, 64,445,
 60, 65,442,60, 69,443,60, 78,440,60, 94,679,60,124, 18,60,139, 19,60,142,  2,
 60,148, 14,60,152,  2,60,168,  3,61,  0,642,61, 10,637,61, 30,402,61, 42,680,
 61, 50,444,61, 64,442,61, 71,443,61, 79,440,61, 93,679,61,127, 18,61,141,  2,
 61,167,  3,62,  0,642,62, 10,637,62, 30,402,62, 40,680,62, 51,444,62, 64,682,
 62, 70,442,62, 72,443,62, 78,442,62, 80,440,62, 91,679,62,129, 18,62,141,  1,
 62,161,676,62,166,  3,62,169,674,62,170,  3,63,  0,642,63, 10,637,63, 18,638,
 63, 21,637,63, 30,402,63, 40,680,63, 52,444,63, 64,682,63, 71,442,63, 80,679,
 63,130, 18,63,141,  1,63,161,676,63,165,673,63,168,674,63,172,  3,64,  0,642,
 64, 10,637,64, 14,638,64, 23,637,64, 30,402,64, 40,680,64, 53,444,64, 64,682,
 64, 79,679,64,131, 18,64,141,  1,64,161,676,64,167,673,64,171,671,65,  0,642,
 65, 10,637,65, 14,638,65, 23,637,65, 30,402,65, 35,636,65, 40,680,65, 54,444,
 65, 62,682,65, 79,679,65,132,677,65,141,676,65,168,673,65,169,671,66,  0,642,
 66, 10,637,66, 30,402,66, 33,636,66, 40,680,66, 54,444,66, 61,682,66, 75,679,
 66,134,677,66,141,676,66,167,673,66,169,670,67,  0,642,67, 10,637,67, 30,636,
 67, 40,680,67, 54,444,67, 63,682,67, 74,679,67,136,677,67,141,676,67,166,673,
 67,170,672,67,172,670,68,  0,642,68, 10,637,68, 25,636,68, 40,680,68, 54,444,
 68, 65,682,68, 75,679,68,136,677,68,141,676,68,167,672,68,176,670,69,  0,639,
 69, 20,636,69, 40,680,69, 55,681,69, 67,682,69, 78,679,69,137,677,69,141,676,
 69,164,672,69,178,670,70,  0,639,70, 20,636,70, 40,680,70, 55,681,70, 67,682,
 70, 90,679,70,130,675,70,141,676,70,163,672,70,177,670,71,  0,639,71, 20,636,
 71, 40,680,71, 56,681,71, 71,682,71, 90,679,71,127,675,71,154,676,71,158,675,
 71,166,672,71,177,670,72,  0,639,72, 20,636,72, 40,680,72, 57,681,72, 74,682,
 72, 90,679,72,127,675,72,166,669,73,  0,640,73, 20,636,73, 40,680,73, 57,681,
 73, 76,682,73, 90,679,73,127,675,73,166,669,74,  0,640,74, 17,636,74, 40,680,
 74, 58,681,74, 79,678,74,116,679,74,125,675,74,166,669,75,  0,640,75, 17,636,
 75, 40,680,75, 60,681,75, 79,678,75,125,675,75,166,669,76,  0,640,76, 17,636,
 76, 40,680,76, 71,681,76, 78,678,76,125,634,76,166,669,77,  0,640,77, 17,636,
 77, 40,680,77, 73,678,77,125,634,78,  0,640,78, 17,636,78, 40,680,78, 73,678,
 78,120,634,79,  0,640,79, 17,636,79, 40,680,79, 70,678,79,110,634,80,  0,641,
 80, 15,635,80, 30,636,80, 40,680,80, 55,635,80, 67,678,80,100,634,81,  0,641,
 81, 10,635,81, 65,678,81, 95,634,82,  0,641,82, 10,635,82, 60,678,82, 90,634,
 83,  0,641,83, 10,635,83, 60,678,83, 80,634,84,  0,641,84, 10,635,84, 60,634,
 85,  0,641,85, 10,633,85, 60,634,86,  0,641,86, 10,633,86, 75,634,87,  0,641,
 87, 10,633,87, 75,634,88,  0,633,89,  0,633,90,180,633, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,409, 0,  9,407, 0, 18,406, 0, 31,409, 0, 45,528, 0, 62,113, 0, 70,103,
  0, 75,110, 0, 76,107, 0, 80,105, 0, 81,104, 0, 86,696, 0, 89,697, 0, 92,696,
  0, 96,695, 0,100,694, 0,107,693, 0,120,632, 0,153,620, 0,165,632, 0,168,624,
  0,176,632, 1,  0,409, 1,  9,407, 1, 18,406, 1, 27,409, 1, 43,528, 1, 62,113,
  1, 69,103, 1, 73,111, 1, 75,110, 1, 76,107, 1, 80,105, 1, 81,104, 1, 86,696,
  1, 89,697, 1, 92,696, 1, 96,695, 1,100,694, 1,107,693, 1,120,632, 1,153,620,
  1,165,632, 1,168,624, 1,176,632, 2,  0,409, 2,  9,407, 2, 18,406, 2, 20,409,
  2, 38,528, 2, 62,113, 2, 70,103, 2, 71,111, 2, 75,110, 2, 78,107, 2, 79,105,
  2, 82,104, 2, 86,696, 2, 96,698, 2,100,694, 2,109,693, 2,120,632, 2,153,620,
  2,165,632, 2,168,624, 2,176,632, 3,  0,409, 3,  9,407, 3, 18,409, 3, 37,528,
  3, 62,113, 3, 70,111, 3, 77,110, 3, 79,105, 3, 80,110, 3, 81,109, 3, 82,108,
  3, 86,696, 3, 96,698, 3,100,694, 3,109,693, 3,120,632, 3,153,620, 3,165,632,
  3,168,624, 3,176,632, 4,  0,409, 4,  9,407, 4, 18,409, 4, 35,528, 4, 62,113,
  4, 70,112, 4, 74,111, 4, 78,110, 4, 81,109, 4, 82,108, 4, 86,699, 4, 92,698,
  4,100,694, 4,111,693, 4,120,632, 4,153,620, 4,165,632, 4,168,624, 4,176,632,
  5,  0,409, 5,  9,408, 5, 18,409, 5, 34,528, 5, 62,113, 5, 72,112, 5, 74,111,
  5, 80,109, 5, 82,108, 5, 86,699, 5, 92,698, 5,100,694, 5,111,693, 5,120,632,
  5,134,630, 5,144,632, 5,153,620, 5,165,632, 5,168,624, 5,176,632, 6,  0,409,
  6,  9,408, 6, 18,409, 6, 34,528, 6, 62,113, 6, 72,112, 6, 75,111, 6, 79,109,
  6, 82,108, 6, 86,699, 6, 92,698, 6,100,694, 6,113,693, 6,120,632, 6,134,630,
  6,144,632, 6,153,620, 6,165,632, 6,168,624, 6,176,632, 7,  0,409, 7,  9,408,
  7, 18,409, 7, 34,528, 7, 62,113, 7, 73,112, 7, 75,111, 7, 79,109, 7, 80,108,
  7, 84,699, 7, 92,698, 7,100,694, 7,113,693, 7,120,632, 7,134,630, 7,144,632,
  7,148,620, 7,157,626, 7,165,632, 7,168,625, 7,175,632, 7,178,623, 8,  0,409,
  8,  9,408, 8, 18,409, 8, 34,528, 8, 62,113, 8, 72,112, 8, 75,116, 8, 78,109,
  8, 80,108, 8, 84,699, 8, 92,698, 8,100,694, 8,113,693, 8,120,632, 8,134,630,
  8,144,632, 8,148,620, 8,157,626, 8,165,632, 8,168,625, 8,175,632, 8,178,623,
  9,  0,409, 9,  9,408, 9, 18,409, 9, 34,528, 9, 62,113, 9, 70,112, 9, 74,116,
  9, 78,109, 9, 79,108, 9, 83,699, 9, 92,698, 9,104,694, 9,115,693, 9,120,632,
  9,134,630, 9,144,632, 9,148,620, 9,157,626, 9,165,632, 9,168,625, 9,175,632,
  9,178,623,10,  0,409,10,  9,408,10, 18,409,10, 35,528,10, 62,113,10, 65,119,
 10, 68,113,10, 70,112,10, 73,116,10, 77,115,10, 79,114,10, 83,699,10, 92,698,
 10,104,694,10,115,693,10,120,632,10,134,630,10,144,632,10,148,620,10,157,626,
 10,165,169,10,176,180,11,  0,409,11,  9,408,11, 18,409,11, 36,528,11, 62,113,
 11, 65,119,11, 68,118,11, 70,112,11, 71,116,11, 77,115,11, 78,114,11, 82,699,
 11, 92,698,11,104,694,11,117,693,11,120,632,11,134,630,11,144,632,11,148,620,
 11,157,626,11,165,169,11,176,180,12,  0,409,12, 10,410,12, 18,409,12, 37,528,
 12, 62,113,12, 64,119,12, 68,118,12, 69,116,12, 76,115,12, 78,114,12, 82,699,
 12, 92,698,12,107,694,12,117,693,12,120,632,12,134,630,12,144,632,12,148,620,
 12,157,626,12,165,169,12,176,180,13,  0,409,13, 10,410,13, 18,409,13, 38,528,
 13, 61,119,13, 68,118,13, 70,116,13, 76,115,13, 77,114,13, 81,699,13, 92,698,
 13,107,694,13,119,693,13,120,632,13,133,631,13,149,628,13,157,626,13,165,169,
 13,168,170,13,173,169,13,176,181,14,  0,409,14, 10,410,14, 18,409,14, 38,528,
 14, 60,120,14, 68,118,14, 70,116,14, 75,115,14, 77,114,14, 81,699,14, 92,698,
 14,107,694,14,119,693,14,120,632,14,133,631,14,149,628,14,157,626,14,165,169,
 14,168,170,14,173,169,14,176,181,15,  0,409,15, 10,410,15, 18,409,15, 38,528,
 15, 60,120,15, 69,118,15, 70,117,15, 74,115,15, 76,114,15, 80,683,15,107,684,
 15,120,632,15,133,631,15,149,628,15,157,627,15,165,169,15,173,173,15,176,181,
 16,  0,409,16, 10,410,16, 18,409,16, 38,528,16, 58,120,16, 68,118,16, 70,117,
 16, 72,115,16, 75,114,16, 79,683,16,107,684,16,120,632,16,133,631,16,149,628,
 16,157,627,16,165,169,16,173,173,16,176,181,17,  0,409,17, 10,410,17, 18,409,
 17, 38,528,17, 58,120,17, 69,118,17, 70,115,17, 73,114,17, 77,683,17,107,684,
 17,124,632,17,133,631,17,147,628,17,157,627,17,166,174,17,173,173,17,176,181,
 18,  0,409,18, 10,410,18, 18,409,18, 39,528,18, 58,120,18, 68,124,18, 69,123,
 18, 70,122,18, 71,121,18, 77,683,18,107,684,18,124,632,18,133,631,18,147,628,
 18,156,627,18,166,174,18,173,173,18,176,181,19,  0,409,19, 10,410,19, 18,409,
 19, 39,528,19, 58,120,19, 59,126,19, 60,125,19, 68,124,19, 69,123,19, 70,122,
 19, 71,121,19, 77,683,19,107,684,19,124,632,19,133,631,19,147,628,19,156,627,
 19,166,174,19,173,173,19,176,181,20,  0,409,20, 10,410,20, 18,409,20, 39,528,
 20, 58,126,20, 62,125,20, 68,124,20, 69,123,20, 70,122,20, 71,121,20, 77,683,
 20,107,684,20,124,632,20,133,631,20,145,629,20,156,627,20,166,174,20,173,173,
 20,176,181,21,  0,409,21, 10,410,21, 18,409,21, 40,528,21, 58,126,21, 62,125,
 21, 67,124,21, 69,123,21, 70,122,21, 71,121,21, 77,683,21,107,684,21,124,632,
 21,133,631,21,145,629,21,156,627,21,166,174,21,173,173,21,176,181,22,  0,409,
 22, 10,410,22, 18,409,22, 40,528,22, 56,126,22, 62,129,22, 65,128,22, 67,124,
 22, 68,123,22, 70,122,22, 71,121,22, 77,683,22,102,685,22,115,684,22,124,632,
 22,133,631,22,145,629,22,156,627,22,166,174,22,176,171,23,  0,409,23, 10,410,
 23, 18,409,23, 41,528,23, 55,126,23, 62,129,23, 64,128,23, 67,127,23, 68,123,
 23, 70,122,23, 71,121,23, 77,683,23,102,685,23,115,684,23,124,632,23,133,631,
 23,145,629,23,156,627,23,166,174,23,176,171,24,  0,409,24, 10,410,24, 18,409,
 24, 44,528,24, 54,126,24, 59,133,24, 62,129,24, 67,127,24, 69,123,24, 70,122,
 24, 71,121,24, 77,683,24,102,685,24,115,684,24,124,632,24,133,631,24,145,629,
 24,156,632,24,170,175,24,176,171,25,  0,409,25,  9,410,25, 18,409,25, 47,528,
 25, 54,126,25, 58,133,25, 62,132,25, 63,129,25, 67,130,25, 68,127,25, 69,123,
 25, 70,122,25, 71,121,25, 77,683,25,102,685,25,115,684,25,124,632,25,133,631,
 25,145,629,25,156,632,25,170,175,25,176,171,26,  0,409,26,  9,410,26, 18,409,
 26, 48,528,26, 54,126,26, 58,133,26, 62,132,26, 64,131,26, 66,130,26, 68,127,
 26, 69,123,26, 70,122,26, 71,121,26, 77,683,26,102,685,26,115,684,26,124,632,
 26,170,175,26,176,171,27,  0,409,27,  8,410,27, 18,409,27, 48,528,27, 55,126,
 27, 57,133,27, 62,132,27, 65,131,27, 66,130,27, 68,127,27, 69,123,27, 70,122,
 27, 72,121,27, 77,683,27,102,685,27,115,684,27,124,632,27,170,177,28,  0,409,
 28,  8,410,28, 18,409,28, 48,528,28, 56,133,28, 62,132,28, 66,130,28, 67,138,
 28, 69,127,28, 70,136,28, 71,135,28, 72,134,28, 77,683,28,102,685,28,115,684,
 28,124,632,28,170,177,29,  0,409,29,  8,410,29, 18,409,29, 49,528,29, 57,133,
 29, 62,132,29, 66,138,29, 68,137,29, 69,127,29, 70,136,29, 71,135,29, 72,134,
 29, 77,683,29,102,685,29,115,684,29,120,632,29,170,177,29,177,178,29,179,177,
 30,  0,409,30,  9,410,30, 18,409,30, 49,528,30, 56,142,30, 58,133,30, 62,141,
 30, 66,138,30, 67,137,30, 69,127,30, 71,135,30, 72,134,30, 77,683,30,100,686,
 30,102,685,30,115,684,30,120,632,30,170,177,30,177,178,30,179,177,31,  0,409,
 31,  9,410,31, 18,409,31, 50,528,31, 55,142,31, 58,133,31, 62,141,31, 66,138,
 31, 67,137,31, 70,127,31, 71,135,31, 72,134,31, 77,687,31, 85,683,31, 97,686,
 31,102,685,31,115,684,31,119,632,31,170,177,32,  0,409,32, 10,410,32, 27,409,
 32, 51,528,32, 53,142,32, 58,133,32, 62,141,32, 65,140,32, 67,139,32, 70,127,
 32, 71,135,32, 72,134,32, 77,687,32, 85,683,32, 97,686,32,105,684,32,119,632,
 32,170,179,33,  0,409,33, 10,410,33, 26,409,33, 52,528,33, 53,142,33, 58,133,
 33, 62,141,33, 65,140,33, 67,139,33, 69,127,33, 71,135,33, 72,134,33, 77,687,
 33, 85,683,33, 94,686,33,105,684,33,118,632,33,170,179,34,  0,409,34,  6,411,
 34, 16,410,34, 25,409,34, 53,142,34, 58,133,34, 62,146,34, 63,141,34, 65,140,
 34, 67,139,34, 69,127,34, 71,135,34, 73,134,34, 77,687,34, 85,683,34, 94,686,
 34,105,684,34,118,632,34,170,179,35,  0,409,35,  6,411,35, 16,410,35, 23,409,
 35, 56,133,35, 62,146,35, 65,140,35, 66,139,35, 70,127,35, 71,136,35, 72,135,
 35, 73,134,35, 80,683,35, 91,686,35, 98,756,35,105,684,35,117,632,35,174,688,
 36,  0,409,36,  6,411,36, 16,410,36, 22,409,36, 56,133,36, 62,146,36, 68,139,
 36, 70,127,36, 71,136,36, 72,135,36, 74,134,36, 80,683,36, 91,686,36, 98,756,
 36,105,684,36,117,632,36,174,688,37,  0,409,37,  6,411,37, 16,410,37, 21,409,
 37, 56,133,37, 62,146,37, 71,145,37, 72,136,37, 73,135,37, 74,134,37, 80,683,
 37, 88,686,37, 95,756,37,100,692,37,105,684,37,117,632,37,174,688,38,  0,409,
 38,  6,411,38, 16,410,38, 21,409,38, 56,133,38, 62,146,38, 71,145,38, 72,136,
 38, 73,135,38, 74,134,38, 80,683,38, 88,686,38, 95,756,38,100,692,38,105,684,
 38,117,632,38,174,688,39,  0,409,39,  6,411,39, 16,410,39, 21,409,39, 60,133,
 39, 62,146,39, 71,145,39, 72,136,39, 73,135,39, 74,134,39, 80,683,39, 85,686,
 39, 92,756,39, 95,692,39,105,684,39,117,632,39,174,688,40,  0,409,40,  6,411,
 40, 16,410,40, 21,409,40, 62,146,40, 71,145,40, 72,136,40, 73,135,40, 74,143,
 40, 81,683,40, 85,686,40, 92,756,40, 95,692,40,105,684,40,117,632,40,174,688,
 41,  0,409,41,  6,411,41, 16,410,41, 21,409,41, 62,146,41, 71,145,41, 72,144,
 41, 74,143,41, 81,686,41, 89,756,41, 94,692,41,105,684,41,117,632,41,174,688,
 42,  0,409,42,  6,411,42, 16,410,42, 21,409,42, 63,146,42, 71,145,42, 72,144,
 42, 75,143,42, 81,686,42, 89,756,42, 94,692,42,105,684,42,117,632,42,170,689,
 43,  0,409,43,  8,410,43, 22,409,43, 64,146,43, 71,145,43, 72,144,43, 75,143,
 43, 81,686,43, 86,692,43,105,684,43,117,632,43,170,689,44,  0,409,44,  8,410,
 44, 22,409,44, 64,146,44, 71,145,44, 72,144,44, 75,143,44, 81,686,44, 86,692,
 44,105,684,44,117,632,44,170,689,45,  0,409,45,  8,410,45, 22,409,45, 66,146,
 45, 71,145,45, 72,144,45, 75,143,45, 83,692,45,105,684,45,117,632,45,170,689,
 46,  0,409,46,  6,410,46, 22,152,46, 24,409,46, 66,146,46, 71,145,46, 72,144,
 46, 76,143,46, 83,692,46,105,684,46,117,632,46,170,690,47,  0,409,47,  4,410,
 47, 22,152,47, 26,409,47, 66,146,47, 71,145,47, 72,144,47, 76,143,47, 83,692,
 47,105,684,47,117,632,47,170,690,48,  0,409,48,  2,410,48, 22,152,48, 28,409,
 48, 66,146,48, 71,145,48, 73,144,48, 76,143,48, 83,692,48,105,684,48,117,632,
 48,170,690,49,  0,412,49,  2,410,49, 10,156,49, 23,152,49, 30,409,49, 55,148,
 49, 65,409,49, 67,146,49, 71,145,49, 73,144,49, 76,143,49, 83,692,49,105,684,
 49,117,632,49,170,690,50,  0,412,50,  2,410,50, 10,156,50, 24,152,50, 32,409,
 50, 55,148,50, 65,409,50, 67,146,50, 71,145,50, 73,144,50, 76,143,50, 83,692,
 50,105,684,50,125,632,50,170,690,51,  0,412,51,  2,410,51, 10,156,51, 26,152,
 51, 34,409,51, 55,148,51, 65,409,51, 68,146,51, 71,145,51, 72,144,51, 76,143,
 51, 83,692,51,105,684,51,125,632,51,170,690,52,  0,412,52,  2,410,52, 10,156,
 52, 28,152,52, 31,151,52, 40,409,52, 55,148,52, 65,147,52, 71,144,52, 76,143,
 52, 83,692,52,105,684,52,127,691,52,147,632,52,170,690,53,  0,412,53,  2,410,
 53, 10,156,53, 29,152,53, 31,151,53, 40,409,53, 55,148,53, 65,147,53, 71,144,
 53, 75,143,53, 83,692,53,105,684,53,127,691,53,147,632,53,170,690,54,  0,412,
 54,  2,410,54,  9,156,54, 22,153,54, 31,151,54, 40,409,54, 55,148,54, 65,147,
 54, 71,144,54, 74,143,54, 83,692,54,105,684,54,127,691,54,154,632,54,170,690,
 55,  0,412,55,  2,410,55,  8,156,55, 22,153,55, 31,151,55, 40,150,55, 59,149,
 55, 65,147,55, 72,143,55, 83,692,55,113,684,55,127,691,55,154,632,55,170,690,
 56,  0,412,56,  2,410,56,  7,732,56, 18,156,56, 22,153,56, 31,151,56, 40,150,
 56, 59,149,56, 73,692,56,113,684,56,127,691,56,157,632,56,170,690,57,  0,412,
 57,  2,410,57,  7,732,57, 18,156,57, 22,153,57, 31,150,57, 59,149,57, 73,692,
 57,117,684,57,127,691,57,157,632,57,170,690,58,  0,412,58,  2,410,58,  7,732,
 58, 22,153,58, 31,150,58, 59,149,58, 73,692,58,117,684,58,127,691,58,160,632,
 58,170,690,59,  0,412,59,  2,410,59,  7,732,59, 22,153,59, 31,150,59, 59,149,
 59, 73,692,59,117,684,59,127,691,59,160,632,59,170,690,60,  0,414,60,  5,156,
 60, 15,732,60, 22,153,60, 31,150,60, 52,154,60, 59,149,60, 73,692,60,148,691,
 60,170,690,61,  0,414,61,  5,156,61, 15,732,61, 22,153,61, 31,150,61, 52,154,
 61, 62,149,61, 73,692,61,148,691,62,  0,414,62,  5,156,62, 56,154,62, 65,692,
 62,149,691,63,  0,414,63,  5,156,63, 54,155,63, 61,154,63, 65,692,63,149,691,
 64,  0,414,64,  5,156,64, 56,155,64, 65,692,64,150,691,65,  0,414,65,  5,156,
 65, 59,155,65, 67,692,65,159,691,66,  0,414,66,  5,156,66, 59,155,66, 70,692,
 66,159,691,67,  0,414,67,  5,156,67, 60,155,67, 70,692,67,162,691,68,  0,414,
 68,  5,156,68, 60,155,68, 76,692,68,162,691,69,  0,729,69, 11,156,69, 60,155,
 69, 76,692,69,162,691,70,  0,729,70, 11,156,70, 60,155,70, 76,692,70,141,728,
 71,  0,729,71, 14,157,71, 60,155,71, 76,692,71,141,728,72,  0,729,72, 19,157,
 72, 60,155,72, 76,692,72, 95,729,72,103,692,72,141,728,73,  0,729,73, 21,157,
 73, 60,155,73, 71,729,73,103,692,73,120,729,73,129,692,73,141,728,74,  0,729,
 74, 24,157,74, 60,155,74, 66,729,74,141,728,75,  0,729,75, 27,157,75, 56,729,
 75,147,728,76,  0,729,76, 32,157,76, 50,729,76,152,728,77,  0,729,77,162,728,
 78,  0,729,79,  0,729,80,  0,729,81,  0,729,82,  0,729,83,  0,729,84,  0,729,
 85,  0,729,86,  0,729,87,  0,729,88,  0,729,89,  0,729,90,180,729, 0,  0,  0,
  0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,409, 0,  9,565, 0, 14,566, 0, 18,567, 0, 30,568, 0, 31,569,
  0, 35,570, 0, 41,571, 0, 43,425, 0, 65,421, 0, 70,422, 0, 78,425, 0, 92,273,
  0, 98,274, 0,105,275, 0,109,261, 0,118,265, 0,124,269, 0,127,267, 0,130,196,
  0,141,198, 0,145,199, 0,149,190, 0,154,191, 0,164,632, 0,171,618, 1,  0,409,
  1,  9,565, 1, 14,566, 1, 17,567, 1, 29,572, 1, 31,569, 1, 35,570, 1, 42,574,
  1, 43,425, 1, 67,421, 1, 70,422, 1, 78,425, 1, 92,273, 1, 98,274, 1,107,275,
  1,109,261, 1,118,268, 1,124,269, 1,127,267, 1,130,196, 1,137,197, 1,140,196,
  1,141,198, 1,145,199, 1,149,190, 1,154,191, 1,164,632, 1,171,618, 2,  0,409,
  2,  9,565, 2, 12,566, 2, 16,567, 2, 28,572, 2, 31,569, 2, 35,573, 2, 37,570,
  2, 41,574, 2, 43,425, 2, 67,421, 2, 70,422, 2, 78,425, 2, 92,273, 2, 99,274,
  2,107,275, 2,109,261, 2,118,268, 2,124,270, 2,129,272, 2,131,196, 2,137,201,
  2,139,197, 2,141,200, 2,143,198, 2,145,199, 2,149,190, 2,154,191, 2,164,632,
  2,171,618, 3,  0,409, 3, 10,565, 3, 12,566, 3, 16,567, 3, 28,572, 3, 31,569,
  3, 35,573, 3, 38,570, 3, 41,574, 3, 45,425, 3, 67,421, 3, 68,426, 3, 78,425,
  3, 92,273, 3,100,274, 3,107,275, 3,111,261, 3,118,268, 3,124,270, 3,126,271,
  3,127,272, 3,131,196, 3,136,201, 3,141,202, 3,142,200, 3,146,203, 3,150,190,
  3,154,191, 3,164,632, 3,171,618, 4,  0,409, 4, 11,566, 4, 15,567, 4, 28,572,
  4, 31,573, 4, 39,570, 4, 40,574, 4, 45,425, 4, 67,421, 4, 68,426, 4, 78,425,
  4, 94,273, 4,101,274, 4,107,275, 4,118,268, 4,123,280, 4,132,196, 4,137,201,
  4,141,202, 4,144,200, 4,146,207, 4,147,203, 4,150,192, 4,153,190, 4,154,193,
  4,161,191, 4,164,632, 4,175,623, 5,  0,409, 5, 12,575, 5, 13,567, 5, 28,572,
  5, 31,573, 5, 40,574, 5, 47,425, 5, 67,421, 5, 68,426, 5, 78,425, 5, 94,273,
  5,102,274, 5,105,276, 5,106,277, 5,108,275, 5,118,268, 5,123,280, 5,132,204,
  5,135,196, 5,137,205, 5,139,201, 5,141,202, 5,145,207, 5,148,192, 5,153,190,
  5,154,193, 5,161,191, 5,164,632, 5,175,623, 6,  0,409, 6, 12,575, 6, 17,567,
  6, 28,572, 6, 32,573, 6, 40,574, 6, 47,425, 6, 67,421, 6, 68,426, 6, 78,425,
  6, 94,273, 6,104,276, 6,106,277, 6,114,278, 6,118,279, 6,123,280, 6,131,281,
  6,133,204, 6,136,196, 6,138,205, 6,139,201, 6,141,202, 6,146,207, 6,148,192,
  6,154,193, 6,162,191, 6,164,632, 6,175,623, 7,  0,409, 7, 12,575, 7, 17,567,
  7, 19,575, 7, 22,567, 7, 29,572, 7, 32,573, 7, 40,574, 7, 49,425, 7, 66,429,
  7, 68,426, 7, 78,425, 7, 96,273, 7,105,277, 7,114,278, 7,118,279, 7,123,280,
  7,130,281, 7,133,204, 7,136,208, 7,137,205, 7,140,201, 7,141,202, 7,143,206,
  7,146,207, 7,149,192, 7,154,193, 7,164,183, 7,173,632, 7,176,623, 8,  0,409,
  8, 12,575, 8, 22,567, 8, 29,572, 8, 32,573, 8, 40,574, 8, 49,425, 8, 66,429,
  8, 68,426, 8, 78,425, 8, 96,273, 8,105,282, 8,107,277, 8,114,283, 8,116,285,
  8,119,286, 8,124,289, 8,128,290, 8,130,281, 8,133,208, 8,137,205, 8,141,206,
  8,146,207, 8,151,194, 8,155,193, 8,164,183, 8,173,632, 8,176,623, 9,  0,409,
  9, 12,575, 9, 22,567, 9, 29,572, 9, 32,576, 9, 33,573, 9, 40,574, 9, 51,425,
  9, 65,429, 9, 68,426, 9, 78,425, 9, 96,273, 9,105,282, 9,114,284, 9,116,285,
  9,119,287, 9,121,288, 9,123,289, 9,127,290, 9,132,208, 9,140,205, 9,141,206,
  9,144,595, 9,146,207, 9,151,194, 9,156,193, 9,164,183, 9,165,184, 9,168,183,
  9,173,632, 9,176,623,10,  0,409,10, 13,575,10, 22,567,10, 29,576,10, 33,577,
 10, 35,573,10, 41,574,10, 51,425,10, 65,429,10, 72,425,10, 98,273,10,105,282,
 10,114,284,10,116,291,10,119,287,10,121,288,10,123,289,10,125,290,10,132,208,
 10,141,594,10,144,595,10,147,207,10,151,194,10,156,195,10,158,193,10,164,183,
 10,165,184,10,168,183,10,173,180,11,  0,409,11, 13,575,11, 24,576,11, 25,567,
 11, 29,576,11, 33,577,11, 35,573,11, 37,581,11, 41,574,11, 48,583,11, 51,425,
 11, 65,429,11, 71,425,11, 98,273,11,105,282,11,114,284,11,116,291,11,119,292,
 11,121,293,11,125,290,11,130,591,11,138,208,11,141,594,11,144,595,11,151,194,
 11,156,195,11,159,193,11,164,183,11,165,184,11,168,183,11,173,180,12,  0,409,
 12, 12,575,12, 24,576,12, 28,567,12, 30,576,12, 33,577,12, 35,581,12, 41,574,
 12, 48,583,12, 51,425,12, 64,429,12, 71,425,12,105,588,12,116,291,12,119,292,
 12,121,293,12,129,591,12,138,593,12,141,594,12,145,595,12,151,194,12,156,195,
 12,159,193,12,164,183,12,165,184,12,168,183,12,173,180,13,  0,409,13, 12,575,
 13, 22,576,13, 33,577,13, 35,581,13, 41,574,13, 47,583,13, 51,425,13, 64,429,
 13, 70,425,13,105,588,13,125,590,13,129,591,13,137,593,13,141,594,13,146,595,
 13,151,194,13,156,195,13,162,185,13,165,186,13,169,185,13,173,181,14,  0,409,
 14, 12,575,14, 22,576,14, 32,581,14, 34,577,14, 36,581,14, 41,582,14, 46,583,
 14, 51,425,14, 64,429,14, 70,425,14,105,588,14,124,590,14,129,591,14,137,593,
 14,141,594,14,147,595,14,156,195,14,162,185,14,165,186,14,170,185,14,173,181,
 15,  0,409,15, 11,575,15, 22,576,15, 30,581,15, 34,577,15, 36,581,15, 41,582,
 15, 45,583,15, 51,425,15, 64,429,15, 70,425,15,105,588,15,123,590,15,129,591,
 15,138,593,15,141,594,15,147,595,15,156,596,15,162,185,15,165,186,15,170,185,
 15,173,181,15,177,182,16,  0,409,16, 11,575,16, 22,576,16, 29,580,16, 33,581,
 16, 35,577,16, 36,581,16, 41,582,16, 44,583,16, 51,425,16, 64,429,16, 70,425,
 16,105,588,16,121,590,16,129,591,16,138,594,16,147,595,16,156,596,16,162,185,
 16,165,186,16,170,185,16,173,181,16,177,182,17,  0,409,17, 11,578,17, 18,575,
 17, 23,576,17, 27,580,17, 33,581,17, 40,582,17, 43,583,17, 51,425,17, 53,427,
 17, 67,429,17, 70,425,17,105,588,17,121,590,17,129,591,17,138,594,17,147,595,
 17,159,597,17,162,187,17,164,185,17,166,186,17,171,185,17,173,181,17,177,182,
 18,  0,409,18, 11,578,18, 21,579,18, 26,580,18, 33,581,18, 38,582,18, 43,583,
 18, 50,425,18, 53,427,18, 67,429,18, 70,425,18,105,588,18,120,590,18,129,591,
 18,138,594,18,149,605,18,159,597,18,162,187,18,165,185,18,167,186,18,171,185,
 18,173,181,18,177,182,19,  0,409,19, 12,578,19, 21,579,19, 26,580,19, 33,581,
 19, 36,582,19, 43,583,19, 50,425,19, 53,427,19, 67,429,19, 70,425,19,105,588,
 19,117,590,19,129,591,19,138,594,19,150,605,19,159,597,19,162,187,19,165,185,
 19,168,186,19,171,185,19,175,171,20,  0,409,20, 13,578,20, 21,579,20, 28,580,
 20, 33,581,20, 36,582,20, 42,583,20, 50,425,20, 53,427,20, 67,429,20, 72,425,
 20,105,588,20,114,590,20,129,591,20,138,594,20,150,605,20,159,597,20,162,187,
 20,166,188,20,169,186,20,171,185,20,175,171,21,  0,409,21, 13,578,21, 21,579,
 21, 28,580,21, 32,581,21, 36,582,21, 42,583,21, 49,425,21, 53,427,21, 67,429,
 21, 72,425,21,105,588,21,113,590,21,129,591,21,138,594,21,152,605,21,159,597,
 21,163,187,21,167,188,21,169,189,21,173,185,21,175,171,22,  0,409,22, 14,578,
 22, 20,579,22, 29,584,22, 31,581,22, 36,582,22, 42,583,22, 49,425,22, 53,427,
 22, 67,429,22, 73,425,22,105,589,22,112,590,22,129,591,22,138,594,22,152,605,
 22,160,597,22,164,187,22,169,189,22,175,171,23,  0,409,23, 14,578,23, 20,579,
 23, 27,584,23, 32,581,23, 36,582,23, 42,583,23, 49,425,23, 53,427,23, 67,429,
 23, 74,425,23,105,589,23,112,590,23,129,591,23,138,594,23,153,605,23,161,597,
 23,165,187,23,169,189,23,175,171,24,  0,409,24, 14,578,24, 20,579,24, 26,584,
 24, 32,581,24, 36,582,24, 42,583,24, 48,425,24, 53,427,24, 67,429,24, 76,425,
 24,105,589,24,112,590,24,129,591,24,138,594,24,154,605,24,162,597,24,169,189,
 24,175,171,25,  0,409,25, 14,578,25, 20,584,25, 21,579,25, 23,584,25, 24,579,
 25, 26,584,25, 32,581,25, 35,582,25, 42,583,25, 48,425,25, 63,428,25, 67,741,
 25, 70,429,25, 76,425,25,105,589,25,112,590,25,129,591,25,138,594,25,151,604,
 25,155,605,25,162,597,25,169,189,25,175,171,26,  0,409,26, 14,578,26, 20,584,
 26, 21,579,26, 22,584,26, 31,586,26, 32,581,26, 33,587,26, 35,425,26, 62,428,
 26, 66,741,26, 71,429,26, 77,425,26,105,589,26,112,590,26,129,592,26,141,594,
 26,152,604,26,155,605,26,162,597,26,169,606,26,173,189,26,175,171,27,  0,409,
 27, 15,578,27, 20,584,27, 33,587,27, 35,425,27, 61,428,27, 65,741,27, 72,429,
 27, 77,425,27,105,589,27,112,590,27,129,592,27,141,594,27,152,604,27,155,605,
 27,165,606,27,173,176,27,178,177,28,  0,409,28, 15,578,28, 20,584,28, 33,587,
 28, 35,425,28, 60,428,28, 64,741,28, 68,425,28, 73,429,28, 78,425,28,105,589,
 28,113,590,28,129,592,28,141,594,28,152,604,28,155,605,28,165,606,28,173,176,
 28,178,177,29,  0,409,29, 15,587,29, 16,584,29, 27,585,29, 29,584,29, 32,587,
 29, 35,425,29, 59,428,29, 63,425,29, 73,429,29, 78,425,29,105,589,29,113,590,
 29,129,592,29,141,601,29,152,604,29,155,605,29,165,606,29,173,176,29,178,177,
 30,  0,409,30, 15,587,30, 17,584,30, 32,587,30, 35,425,30, 58,428,30, 62,425,
 30, 74,429,30, 79,434,30,105,589,30,114,590,30,129,592,30,141,601,30,152,604,
 30,155,605,30,165,607,30,173,176,30,178,177,31,  0,409,31, 15,587,31, 17,584,
 31, 31,587,31, 35,425,31, 56,428,31, 61,425,31, 74,429,31, 79,434,31,105,589,
 31,114,590,31,129,600,31,135,592,31,141,601,31,152,604,31,155,605,31,165,607,
 31,173,176,31,178,177,32,  0,409,32, 15,587,32, 17,584,32, 30,587,32, 35,425,
 32, 54,428,32, 60,425,32, 74,429,32, 79,434,32,105,589,32,114,590,32,129,600,
 32,140,592,32,141,601,32,151,603,32,154,605,32,165,607,32,173,176,32,178,179,
 33,  0,409,33, 15,587,33, 17,584,33, 29,587,33, 35,425,33, 52,428,33, 58,425,
 33, 74,429,33, 79,434,33, 87,425,33,105,589,33,114,590,33,126,599,33,133,600,
 33,140,592,33,141,601,33,150,603,33,153,605,33,165,607,33,173,176,33,178,179,
 34,  0,409,34, 15,587,34, 17,584,34, 27,587,34, 35,425,34, 49,428,34, 56,425,
 34, 74,429,34, 79,434,34, 84,435,34, 87,425,34,105,589,34,114,590,34,125,599,
 34,134,600,34,140,592,34,141,601,34,150,603,34,153,605,34,165,607,34,173,160,
 34,176,176,34,178,179,35,  0,409,35, 15,587,35, 35,425,35, 47,428,35, 54,425,
 35, 74,429,35, 84,435,35, 87,425,35,105,598,35,114,590,35,120,599,35,135,600,
 35,141,602,35,144,601,35,149,603,35,152,605,35,165,607,35,172,158,35,173,159,
 35,175,160,36,  0,409,36, 15,587,36, 35,425,36, 45,428,36, 53,425,36, 74,429,
 36, 84,435,36, 87,425,36,105,598,36,114,599,36,136,600,36,141,602,36,148,601,
 36,149,603,36,152,605,36,165,607,36,172,158,36,174,159,36,175,160,37,  0,409,
 37, 15,587,37, 35,425,37, 45,428,37, 51,425,37, 73,429,37, 84,435,37, 90,425,
 37,105,598,37,114,599,37,139,600,37,141,603,37,151,605,37,165,607,37,172,158,
 37,174,159,37,177,160,38,  0,409,38, 15,587,38, 35,425,38, 44,428,38, 51,425,
 38, 73,429,38, 84,435,38, 92,425,38,105,598,38,114,599,38,139,600,38,141,603,
 38,151,605,38,165,607,38,172,158,38,174,159,38,178,160,39,  0,409,39, 15,587,
 39, 35,425,39, 44,428,39, 50,425,39, 73,429,39, 84,435,39, 97,425,39,105,598,
 39,114,599,39,142,608,39,149,610,39,165,607,39,172,158,39,174,159,39,177,160,
 40,  0,409,40, 15,587,40, 35,425,40, 40,428,40, 50,425,40, 72,429,40, 84,435,
 40,105,598,40,114,437,40,142,608,40,149,610,40,165,161,40,173,163,40,175,159,
 40,177,160,41,  0,409,41,  9,413,41, 20,430,41, 32,431,41, 44,432,41, 57,425,
 41, 72,429,41, 84,435,41,105,598,41,114,437,41,142,609,41,150,610,41,160,161,
 41,171,162,41,174,163,41,175,159,41,176,160,42,  0,409,42,  9,413,42, 20,430,
 42, 32,431,42, 44,432,42, 57,425,42, 71,429,42, 84,435,42,114,437,42,142,609,
 42,150,610,42,160,161,42,171,162,42,174,164,43,  0,409,43,  9,413,43, 20,430,
 43, 32,431,43, 44,432,43, 57,425,43, 71,429,43, 84,435,43,114,437,43,142,609,
 43,150,610,43,160,161,43,169,162,43,173,164,44,  0,409,44,  9,413,44, 20,430,
 44, 32,431,44, 44,432,44, 57,425,44, 63,433,44, 76,429,44, 84,435,44,114,437,
 44,142,609,44,150,610,44,160,161,44,167,162,44,172,164,45,  0,409,45,  9,413,
 45, 20,430,45, 32,431,45, 44,432,45, 57,425,45, 63,433,45, 76,429,45, 84,435,
 45,114,437,45,142,609,45,150,610,45,160,161,45,167,162,45,171,164,45,177,168,
 46,  0,409,46,  9,413,46, 20,430,46, 32,431,46, 44,432,46, 57,425,46, 63,433,
 46, 76,429,46, 84,435,46,114,437,46,142,700,46,150,165,46,162,161,46,168,162,
 46,170,164,46,177,168,47,  0,409,47,  9,413,47, 20,430,47, 32,431,47, 44,432,
 47, 57,425,47, 63,433,47, 76,429,47, 84,435,47,114,742,47,142,700,47,150,165,
 47,162,161,47,169,164,47,177,168,48,  0,409,48,  9,413,48, 20,430,48, 32,431,
 48, 44,432,48, 57,425,48, 63,433,48, 76,429,48, 84,435,48,114,742,48,142,700,
 48,150,165,48,162,161,48,169,164,48,173,168,49,  0,412,49,  9,413,49, 20,430,
 49, 32,431,49, 44,432,49, 57,425,49, 63,433,49, 76,429,49, 84,435,49,114,742,
 49,142,700,49,150,165,49,162,166,49,170,168,50,  0,412,50,  9,413,50, 20,430,
 50, 32,431,50, 44,432,50, 57,425,50, 63,433,50, 76,436,50, 80,425,50, 96,435,
 50,114,742,50,142,700,50,150,165,50,162,166,50,170,168,51,  0,412,51,  9,413,
 51, 20,430,51, 32,431,51, 44,432,51, 57,425,51, 63,433,51, 76,436,51, 80,425,
 51, 96,435,51,114,742,51,142,700,51,150,165,51,162,166,51,170,168,52,  0,412,
 52,  9,413,52, 20,430,52, 32,425,52, 63,433,52, 76,436,52, 80,425,52, 96,435,
 52,114,742,52,120,437,52,135,701,52,155,167,52,164,168,53,  0,412,53,  9,413,
 53, 20,430,53, 32,425,53, 63,433,53, 76,436,53, 80,425,53, 96,435,53,114,742,
 53,120,437,53,135,701,53,155,167,53,164,168,54,  0,412,54,  9,413,54, 20,430,
 54, 32,425,54, 63,433,54, 76,436,54, 83,425,54,114,437,54,135,701,54,155,167,
 54,164,168,55,  0,412,55,  9,413,55, 20,430,55, 32,425,55, 71,436,55, 83,425,
 55,114,437,55,135,701,55,155,167,55,164,168,56,  0,412,56,  9,413,56, 20,430,
 56, 32,425,56, 71,436,56, 83,425,56,114,437,56,135,701,56,155,167,56,164,168,
 57,  0,412,57,  9,413,57, 20,430,57, 32,425,57, 71,436,57, 86,425,57,114,437,
 57,135,701,57,155,167,57,164,168,58,  0,412,58,  9,413,58, 20,430,58, 32,425,
 58, 71,436,58, 86,425,58,114,437,58,135,701,58,155,167,58,164,168,59,  0,412,
 59,  9,413,59, 20,430,59, 32,425,59, 71,436,59, 86,425,59,114,437,59,135,701,
 59,155,167,59,164,168,60,  0,414,60, 20,425,60, 71,436,60, 86,425,60,114,437,
 60,135,701,60,155,167,60,164,168,61,  0,414,61, 20,425,61, 71,436,61, 86,425,
 61,114,437,61,148,702,62,  0,414,62, 20,425,62, 71,436,62, 86,425,62,114,437,
 62,148,702,63,  0,414,63, 20,425,63, 71,436,63, 86,425,63,114,437,63,148,702,
 64,  0,414,64, 20,425,64, 71,436,64, 86,425,64,114,437,64,148,702,65,  0,414,
 65, 20,425,65, 51,729,65, 56,425,65, 71,436,65, 86,425,65, 95,729,65,105,425,
 65,111,729,65,114,437,65,148,702,66,  0,414,66, 20,425,66, 48,729,66, 59,425,
 66, 71,436,66, 81,729,66,145,437,66,148,702,67,  0,414,67, 20,425,67, 44,729,
 67, 71,436,67, 79,729,67,148,702,68,  0,414,68, 20,425,68, 33,729,68, 36,425,
 68, 39,729,68,156,702,69,  0,729,69,162,702,70,  0,729,70,162,727,70,168,702,
 70,171,728,71,  0,729,71,162,727,71,171,728,72,  0,729,72,159,727,72,171,728,
 73,  0,729,73,159,727,73,170,728,74,  0,729,74,155,727,74,167,728,75,  0,729,
 75,155,727,75,164,728,76,  0,729,76,155,727,76,164,728,77,  0,729,77,155,727,
 77,170,729,77,178,728,78,  0,729,78,155,727,78,170,729,79,  0,729,80,  0,729,
 81,  0,729,82,  0,729,83,  0,729,84,  0,729,85,  0,729,86,  0,729,87,  0,729,
 88,  0,729,89,  0,729,90,180,729, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
  0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0, 0,  0,  0,
 };

if (drclat == ' ' && drclng == ' ')
{
if (alat >= 0. && alng >= 0.) quad = 1;
if (alat >= 0. && alng <  0.) quad = 2;
if (alat <  0. && alng <  0.) quad = 3;
if (alat <  0. && alng >= 0.) quad = 4; }
else if ((drclat == 'N' || drclat == 'n')
      && (drclng == 'E' || drclng == 'e')) quad = 1;
else if ((drclat == 'N' || drclat == 'n')
      && (drclng == 'W' || drclng == 'w')) quad = 2;
else if ((drclat == 'S' || drclat == 's')
      && (drclng == 'W' || drclng == 'w')) quad = 3;
else if ((drclat == 'S' || drclat == 's')
      && (drclng == 'E' || drclng == 'e')) quad = 4;
else quad = 0;

if (quad > 0)
{
if (alat >= 0.)lat =  (int)alat;
if (alat <  0.)lat = -(int)alat;
if (alng >= 0.)lng =  (int)alng;
if (alng <  0.)lng = -(int)alng;

n = ind[quad];
for (i = 0; lat > lln[i+n][0]; ++i)if (i == num[quad]) break;
     if (i < num[quad]) {
     for (j = i; lat == lln[j+n][0] && lng >= lln[j+n][1]; ++j)
     if (j == num[quad]) break;
     *igreg = lln[--j+n][2]; }
else *igreg = 0;
}

if (*igreg <= 0) *igreg = ++nn;
*isreg = nsreg[*igreg];

return;
}

void mbh_fer_region(int igreg, char grname[], int isreg, char srname[] )

/*Caveat : This  subroutine is part of a suite of programs which is         */
/*         software  quality  registered.   Software is supplied on         */
/*         the understanding the user is responsible for validation         */
/*         and accepts that the authors have  no  liability for its         */
/*         use or misuse.                                                   */
/*                                                                          */
/*                                                                          */
/*                  REGION NAME LOOKUP PROGRAM                              */
/*                  -------====---------------                              */
/*                                                                          */
/*                                                                          */
/*     Given a geographical region number this subroutine looks up          */
/*     the name of the region as defined by Edward A. Flinn and             */
/*     Eric R. Engdahl in Reviews of Geophysics Vol.3 No.1 pp.123-149       */
/*     Feb.1965 and in the Bulletin of the Seismological Society of         */
/*     America Vol.14 No.3-Part II pp.771-992 Jun.1974.                     */
/*     Also included are four additional geographical regions as agreed     */
/*     at IUGG in August 1987, six additional regions approved at IASPEI    */
/*     in August 1989, a further eighteen regions presented at IUGG in      */
/*     August 1991, and all finally approved at IUGG in July 1995.          */
/*                                                                          */
/*                                                                          */
/*     From the call  region(igreg,grname,isreg,srname)                     */
/*                                                                          */
/*     The region names of the geographical region number IGREG and the     */
/*     seismic region number ISREG are stored in the character arrays       */
/*     GRNAME and SRNAME respectively.  The length of GRNAME and SRNAME     */
/*     is up to 52 characters.                                              */
/*                                                                          */
/*                                                                          */
/*     See also the region number  lookup program.                          */
/*                                                                          */
/*                                                                          */
{
static int date = 1995;
static int nn = 757;
static short int nsreg[759] = { 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4,
    4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9,10,10,10,10,
   10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,
   12,12,12,12,13,13,13,14,14,14,14,14,14,14,15,15,15,15,15,15,16,16,16,16,16,
   16,16,16,16,16,16,16,16,17,17,18,18,18,18,18,18,19,19,19,19,19,19,19,19,19,
   19,19,19,19,19,20,20,20,20,20,20,20,20,20,20,20,21,21,21,21,21,21,22,22,22,
   22,22,22,22,22,22,22,22,22,22,23,23,23,23,23,23,23,23,23,23,23,23,24,24,24,
   24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,
   25,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,27,27,27,27,27,
   28,28,28,28,28,28,28,28,28,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,
   29,29,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,
   31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,
   31,32,32,32,32,32,32,32,32,32,32,32,32,32,33,33,33,33,33,33,33,33,33,33,33,
   33,33,33,33,33,33,33,33,33,33,33,33,34,34,34,34,34,34,34,34,34,34,34,34,34,
   34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,
   34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,
   34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,
   34,34,35,35,35,35,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,37,
   37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,
   37,37,37,37,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,38,38,38,38,38,38,
   38,38,38,38,38,38,38,38,38,38,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,
   39,39,39,39,39,39,39,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,
   40,40,40,40,40,41,41,41,41,41,41,41,41,41,41,41,42,42,42,42,42,42,42,42,42,
   42,42,42,42,42,42,42,43,43,43,43,43,43,43,43,43,43,44,44,44,44,44,44,44,45,
   45,45,46,46,46,46,46,46,47,47,47,47,48,48,48,48,48,48,48,48,49,49,49,49,49,
   49,50,50,50, 5, 7,10,25,25,25,25,25,32,32,33,33,33,37,37,37,37,37,37,37,37,
   37,37,37,37,37,43,44, 0,
 };
static char *sreg[] = {"                                                    ",
 "ALASKA - ALEUTIAN ISLANDS ARC","EASTERN ALASKA TO VANCOUVER ISLAND",
 "CALIFORNIA - NEVADA AREA","BAJA CALIFORNIA AND GULF OF CALIFORNIA",
 "MEXICO - GUATEMALA AREA","CENTRAL AMERICA","CARIBBEAN LOOP",
 "ANDEAN SOUTH AMERICA","EXTREME SOUTH AMERICA","SOUTHERN ANTILLES",
 "NEW ZEALAND AREA","KERMADEC - TONGA - SAMOA BASIN AREA","FIJI ISLANDS AREA",
 "VANUATU ISLANDS","BISMARCK AND SOLOMON ISLANDS","NEW GUINEA AREA",
 "CAROLINE ISLANDS TO GUAM","GUAM TO JAPAN",
 "JAPAN - KURIL ISLANDS - KAMCHATKA PENINSULA",
 "SOUTHWESTERN JAPAN AND RYUKYU ISLANDS","TAIWAN","THE PHILIPPINES",
 "BORNEO - SULAWESI","SUNDA ARC","MYANMAR AND SOUTHEAST ASIA",
 "INDIA - XIZANG - SICHUAN - YUNNAN","SOUTHERN XINJIANG TO GANSU",
 "LAKE ISSYK-KUL TO LAKE BAYKAL","WESTERN ASIA",
 "MIDDLE EAST - CRIMEA - EASTERN BALKANS","WESTERN MEDITERRANEAN SEA AREA",
 "ATLANTIC OCEAN","INDIAN OCEAN","EASTERN NORTH AMERICA",
 "EASTERN SOUTH AMERICA","NORTHWESTERN EUROPE","AFRICA","AUSTRALIA",
 "PACIFIC BASIN","ARCTIC ZONE","EASTERN ASIA",
 "NORTHEASTERN ASIA, NORTHERN ALASKA TO GREENLAND",
 "SOUTHEASTERN AND ANTARCTIC PACIFIC OCEAN","GALAPAGOS AREA","MACQUARIE LOOP",
 "ANDAMAN ISLANDS TO SUMATERA","BALUCHISTAN","HINDU KUSH AND PAMIR",
 "NORTHERN EURASIA","ANTARCTICA",
 };
static char *greg[] = {"                                                    ",
 "Central Alaska, United States","Southern Alaska, United States",
 "Bering Sea","Komandorsky Islands, Russia, region",
 "Near Islands, Aleutian Islands, United States",
 "Rat Islands, Aleutian Islands, United States",
 "Andreanof Islands, Aleutian Islands, United States",
 "Pribilof Islands, Alaska, United States",
 "Fox Islands, Aleutian Islands, United States",
 "Unimak Island, Alaska, United States, region",
 "Bristol Bay, Alaska, United States","Alaska Peninsula, United States",
 "Kodiak Island, Alaska, United States, region",
 "Kenai Peninsula, Alaska, United States","Gulf of Alaska",
 "South of Aleutian Islands","South of Alaska",
 "Southern Yukon Territory, Canada","Southeastern Alaska, United States",
 "Off coast of southeastern Alaska, United States","West of Vancouver Island",
 "Queen Charlotte Islands, Canada, region","British Columbia, Canada",
 "Alberta, Canada","Vancouver Island, Canada, region",
 "Off coast of Washington, United States",
 "Near coast of Washington, United States","Washington-Oregon border region",
 "Washington, United States","Off coast of Oregon, United States",
 "Near coast of Oregon, United States","Oregon, United States",
 "Western Idaho, United States",
 "Off coast of northern California, United States",
 "Near coast of northern California, United States",
 "Northern California, United States","Nevada, United States",
 "Off coast of California, United States","Central California, United States",
 "California-Nevada border region","Southern Nevada, United States",
 "Western Arizona, United States","Southern California, United States",
 "California-Arizona border region",
 "California-Baja California border region",
 "Western Arizona-Sonora border region",
 "Off west coast of Baja California, Mexico","Baja California, Mexico",
 "Gulf of California, Mexico","Sonora, Mexico","Off coast of central Mexico",
 "Near coast of central Mexico","Revilla Gigedo Islands, Mexico, region",
 "Off coast of Jalisco, Mexico","Near coast of Jalisco, Mexico",
 "Near coast of Michoacan, Mexico","Michoacan, Mexico",
 "Near coast of Guerrero, Mexico","Guerrero, Mexico","Oaxaca, Mexico",
 "Chiapas, Mexico","Mexico-Guatemala border region","Off coast of Mexico",
 "Off coast of Michoacan, Mexico","Off coast of Guerrero, Mexico",
 "Near coast of Oaxaca, Mexico","Off coast of Oaxaca, Mexico",
 "Off coast of Chiapas, Mexico","Near coast of Chiapas, Mexico","Guatemala",
 "Near coast of Guatemala","Honduras","El Salvador","Near coast of Nicaragua",
 "Nicaragua","Off coast of central America","Off coast of Costa Rica",
 "Costa Rica","North of Panama","Panama-Costa Rica border region","Panama",
 "Panama-Colombia border region","South of Panama",
 "Yucatan Peninsula, Mexico","Cuba region","Jamaica region","Haiti region",
 "Dominican Republic region","Mona Passage","Puerto Rico region",
 "Virgin Islands","Leeward Islands","Belize","Caribbean Sea",
 "Windward Islands","Near north coast of Colombia","Near coast of Venezuela",
 "Trinidad","Northern Colombia","Lake Maracaibo, Venezuela","Venezuela",
 "Near west coast of Colombia","Colombia","Off coast of Ecuador",
 "Near coast of Ecuador","Colombia-Ecuador border region","Ecuador",
 "Off coast of northern Peru","Near coast of northern Peru",
 "Peru-Ecuador border region","Northern Peru","Peru-Brazil border region",
 "Western Brazil","Off coast of Peru","Near coast of Peru","Central Peru",
 "Southern Peru","Peru-Bolivia border region","Northern Bolivia",
 "Central Bolivia","Off coast of northern Chile",
 "Near coast of northern Chile","Northern Chile",
 "Chile-Bolivia border region","Southern Bolivia","Paraguay",
 "Chile-Argentina border region","Jujuy Province, Argentina",
 "Salta Province, Argentina","Catamarca Province, Argentina",
 "Tucuman Province, Argentina","Santiago del Estero Province, Argentina",
 "Northeastern Argentina","Off coast of central Chile",
 "Near coast of central Chile","Central Chile","San Juan Province, Argentina",
 "La Rioja Province, Argentina","Mendoza Province, Argentina",
 "San Luis Province, Argentina","Cordoba Province, Argentina","Uruguay",
 "Off coast of southern Chile","Southern Chile",
 "Southern Chile-Argentina border region","Southern Argentina",
 "Tierra del Fuego","Falkland Islands region","Drake Passage","Scotia Sea",
 "South Georgia Island region","South Georgia Rise",
 "South Sandwich Islands region","South Shetland Islands",
 "Antarctic Peninsula","Southwestern Atlantic Ocean","Weddell Sea",
 "Off west coast of North Island, New Zealand","North Island, New Zealand",
 "Off east coast of North Island, New Zealand",
 "Off west coast of South Island, New Zealand","South Island, New Zealand",
 "Cook Strait, New Zealand","Off east coast of South Island, New Zealand",
 "North of Macquarie Island","Auckland Islands, New Zealand, region",
 "Macquarie Island, Australia, region","South of New Zealand",
 "Samoa Islands region","Samoa Islands","South of Fiji Islands",
 "West of Tonga Islands (REGION NOT IN USE)","Tonga Islands",
 "Tonga Islands region","South of Tonga Islands","North of New Zealand",
 "Kermadec Islands region","Kermadec Islands, New Zealand",
 "South of Kermadec Islands","North of Fiji Islands","Fiji Islands region",
 "Fiji Islands","Santa Cruz Islands region","Santa Cruz Islands",
 "Vanuatu Islands region","Vanuatu Islands","New Caledonia","Loyalty Islands",
 "Southeast of Loyalty Islands","New Ireland, Papua New Guinea, region",
 "North of Solomon Islands","New Britain, Papua New Guinea, region",
 "Bougainville - Solomon Islands region",
 "D'Entrecasteaux Islands, Papua New Guinea, region",
 "South of Solomon Islands","Irian Jaya, Indonesia, region",
 "Near north coast of Irian Jaya, Indonesia",
 "Ninigo Islands, Papua New Guinea, region",
 "Admiralty Islands, Papua New Guinea, region",
 "Near north coast of New Guinea, Papua New Guinea","Irian Jaya, Indonesia",
 "New Guinea, Papua New Guinea","Bismarck Sea",
 "Aru Islands, Indonesia, region","Near south coast of Irian Jaya, Indonesia",
 "Near south coast of New Guinea, Papua New Guinea",
 "Eastern New Guinea, Papua New Guinea, region","Arafura Sea",
 "Western Caroline Islands, Micronesia","South of Mariana Islands",
 "Southeast of Honshu, Japan","Bonin Islands, Japan, region",
 "Volcano Islands, Japan, region","West of Mariana Islands",
 "Mariana Islands region","Mariana Islands","Kamchatka Peninsula, Russia",
 "Near east coast of Kamchatka Peninsula, Russia",
 "Off east coast of Kamchatka Peninsula, Russia",
 "Northwest of Kuril Islands, Russia","Kuril Islands, Russia",
 "East of Kuril Islands, Russia","Eastern Sea of Japan",
 "Hokkaido, Japan, region","Off southeast coast of Hokkaido, Japan",
 "Near west coast of eastern Honshu, Japan","Eastern Honshu, Japan",
 "Near east coast of eastern Honshu, Japan","Off east coast of Honshu, Japan",
 "Near south coast of eastern Honshu, Japan","South Korea",
 "Western Honshu, Japan","Near south coast of western Honshu, Japan",
 "Northwest of Ryukyu Islands, Japan","Kyushu, Japan","Shikoku, Japan",
 "Southeast of Shikoku, Japan","Ryukyu Islands, Japan",
 "Southeast of Ryukyu Islands, Japan","West of Bonin Islands, Japan",
 "Philippine Sea","Near coast of southeastern China","Taiwan region","Taiwan",
 "Northeast of Taiwan","Southwestern Ryukyu Islands, Japan",
 "Southeast of Taiwan","Philippine Islands region",
 "Luzon, Philippine Islands","Mindoro, Philippine Islands",
 "Samar, Philippine Islands","Palawan, Philippine Islands","Sulu Sea",
 "Panay, Philippine Islands","Cebu, Philippine Islands",
 "Leyte, Philippine Islands","Negros, Philippine Islands",
 "Sulu Archipelago, Philippine Islands","Mindanao, Philippine Islands",
 "East of Philippine Islands","Borneo","Celebes Sea",
 "Talaud Islands, Indonesia","North of Halmahera, Indonesia",
 "Minahassa Peninsula, Sulawesi, Indonesia","Northern Molucca Sea",
 "Halmahera, Indonesia","Sulawesi, Indonesia","Southern Molucca Sea",
 "Ceram Sea","Buru, Indonesia","Seram, Indonesia",
 "Southwest of Sumatera, Indonesia","Southern Sumatera, Indonesia","Java Sea",
 "Sunda Strait, Indonesia","Jawa, Indonesia","Bali Sea","Flores Sea",
 "Banda Sea","Tanimbar Islands, Indonesia, region","South of Jawa, Indonesia",
 "Bali, Indonesia, region","South of Bali, Indonesia",
 "Sumbawa, Indonesia, region","Flores, Indonesia, region",
 "Sumba, Indonesia, region","Savu Sea","Timor, Indonesia, region","Timor Sea",
 "South of Sumbawa, Indonesia","South of Sumba, Indonesia",
 "South of Timor, Indonesia","Myanmar-India border region",
 "Myanmar-Bangladesh border region","Myanmar","Myanmar-China border region",
 "Near south coast of Myanmar","Southeast Asia (REGION NOT IN USE)",
 "Hainan Island, China","South China Sea","Eastern Kashmir",
 "Kashmir-India border region","Kashmir-Xizang border region",
 "Western Xizang-India border region","Xizang","Sichuan, China",
 "Northern India","Nepal-India border region","Nepal","Sikkim, India",
 "Bhutan","Eastern Xizang-India border region","Southern India",
 "India-Bangladesh border region","Bangladesh","Northeastern India",
 "Yunnan, China","Bay of Bengal","Kyrgyzstan-Xinjiang border region",
 "Southern Xinjiang, China","Gansu, China","Western Nei Mongol, China",
 "Kashmir-Xinjiang border region","Qinghai, China",
 "Southwestern Siberia, Russia","Lake Baykal, Russia, region",
 "East of Lake Baykal, Russia","Eastern Kazakhstan",
 "Lake Issyk-Kul, Kyrgyzstan, region","Kazakhstan-Xinjiang border region",
 "Northern Xinjiang, China","Tuva-Buryatia-Mongolia border region","Mongolia",
 "Ural Mountains, Russia, region","Western Kazakhstan","Eastern Caucasus",
 "Caspian Sea","Northwestern Uzbekistan","Turkmenistan",
 "Iran-Turkmenistan border region","Turkmenistan-Afghanistan border region",
 "Turkey-Iran border region","Iran-Armenia-Azerbaijan border region",
 "Northwestern Iran","Iran-Iraq border region","Western Iran",
 "Northern and central Iran","Northwestern Afghanistan",
 "Southwestern Afghanistan","Eastern Arabian Peninsula","Persian Gulf",
 "Southern Iran","Southwestern Pakistan","Gulf of Oman",
 "Off coast of Pakistan","Ukraine - Moldova - Southwestern Russia region",
 "Romania","Bulgaria","Black Sea","Crimea, Ukraine, region",
 "Western Caucasus","Greece-Bulgaria border region","Greece","Aegean Sea",
 "Turkey","Turkey-Georgia-Armenia border region","Southern Greece",
 "Dodecanese Islands, Greece","Crete, Greece","Eastern Mediterranean Sea",
 "Cyprus region","Dead Sea region","Jordan - Syria region","Iraq","Portugal",
 "Spain","Pyrenees","Near south coast of France","Corsica, France",
 "Central Italy","Adriatic Sea","Northwestern Balkan Peninsula",
 "West of Gibraltar","Strait of Gibraltar","Balearic Islands, Spain",
 "Western Mediterranean Sea","Sardinia, Italy","Tyrrhenian Sea",
 "Southern Italy","Albania","Greece-Albania border region",
 "Madeira Islands, Portugal, region","Canary Islands, Spain, region",
 "Morocco","Northern Algeria","Tunisia","Sicily, Italy","Ionian Sea",
 "Central Mediterranean Sea","Near coast of Libya","North Atlantic Ocean",
 "Northern Mid-Atlantic Ridge","Azores Islands region",
 "Azores Islands, Portugal","Central Mid-Atlantic Ridge",
 "North of Ascension Island","Ascension Island region","South Atlantic Ocean",
 "Southern Mid-Atlantic Ridge","Tristan da Cunha region",
 "Bouvet Island region","Southwest of Africa","Southeastern Atlantic Ocean",
 "Eastern Gulf of Aden","Socotra region","Arabian Sea",
 "Lakshadweep, India, region","Northeastern Somalia","North Indian Ocean",
 "Carlsberg Ridge","Maldive Islands region","Laccadive Sea","Sri Lanka",
 "South Indian Ocean","Chagos Archipelago region",
 "Mauritius - Reunion region","Southwest Indian Ridge","Mid-Indian Ridge",
 "South of Africa","Prince Edward Islands, South Africa, region",
 "Crozet Islands region","Kerguelen Islands region","Broken Ridge",
 "Southeast Indian Ridge","Southern Kerguelen Plateau","South of Australia",
 "Saskatchewan, Canada","Manitoba, Canada","Hudson Bay","Ontario, Canada",
 "Hudson Strait, Canada, region","Northern Quebec, Canada","Davis Strait",
 "Labrador, Canada","Labrador Sea","Southern Quebec, Canada",
 "Gaspe Peninsula, Canada","Eastern Quebec, Canada",
 "Anticosti Island, Canada","New Brunswick, Canada","Nova Scotia, Canada",
 "Prince Edward Island, Canada","Gulf of St. Lawrence, Canada",
 "Newfoundland, Canada","Montana, United States",
 "Eastern Idaho, United States","Hebgen Lake, Montana, United States, region",
 "Yellowstone, United States, region","Wyoming, United States",
 "North Dakota, United States","South Dakota, United States",
 "Nebraska, United States","Minnesota, United States","Iowa, United States",
 "Wisconsin, United States","Illinois, United States",
 "Michigan, United States","Indiana, United States",
 "Southern Ontario, Canada","Ohio, United States","New York, United States",
 "Pennsylvania, United States",
 "Vermont - New Hampshire, United States, region","Maine, United States",
 "Southern New England, United States","Gulf of Maine, United States",
 "Utah, United States","Colorado, United States","Kansas, United States",
 "Iowa-Missouri border region","Missouri-Kansas border region",
 "Missouri, United States","Missouri-Arkansas border region",
 "Missouri-Illinois border region",
 "New Madrid, Missouri, United States, region",
 "Cape Girardeau, Missouri, United States, region",
 "Southern Illinois, United States","Southern Indiana, United States",
 "Kentucky, United States","West Virginia, United States",
 "Virginia, United States","Chesapeake Bay, United States, region",
 "New Jersey, United States","Eastern Arizona, United States",
 "New Mexico, United States","Northwestern Texas-Oklahoma border region",
 "Western Texas, United States","Oklahoma, United States",
 "Central Texas, United States","Arkansas-Oklahoma border region",
 "Arkansas, United States","Louisiana-Texas border region",
 "Louisiana, United States","Mississippi, United States",
 "Tennessee, United States","Alabama, United States",
 "Western Florida, United States","Georgia, United States",
 "Florida-Georgia border region","South Carolina, United States",
 "North Carolina, United States","Off east coast of United States",
 "Florida Peninsula, United States","Bahama Islands",
 "Eastern Arizona-Sonora border region","New Mexico-Chihuahua border region",
 "Texas-Mexico border region","Southern Texas, United States",
 "Near coast of Texas, United States","Chihuahua, Mexico","Northern Mexico",
 "Central Mexico","Jalisco, Mexico","Veracruz, Mexico","Gulf of Mexico",
 "Bay of Campeche","Brazil","Guyana","Suriname","French Guiana","Eire",
 "United Kingdom","North Sea","Southern Norway","Sweden","Baltic Sea",
 "France","Bay of Biscay","The Netherlands","Belgium","Denmark","Germany",
 "Switzerland","Northern Italy","Austria","Czech and Slovak Republics",
 "Poland","Hungary","Northwest Africa (REGION NOT IN USE)","Southern Algeria",
 "Libya","Egypt","Red Sea","Western Arabian Peninsula","Chad region","Sudan",
 "Ethiopia","Western Gulf of Aden","Northwestern Somalia",
 "Off south coast of northwest Africa","Cameroon","Equatorial Guinea",
 "Central African Republic","Gabon","Congo","Zaire","Uganda",
 "Lake Victoria region","Kenya","Southern Somalia","Lake Tanganyika region",
 "Tanzania","Northwest of Madagascar","Angola","Zambia","Malawi","Namibia",
 "Botswana","Zimbabwe","Mozambique","Mozambique Channel","Madagascar",
 "South Africa","Lesotho","Swaziland","Off coast of South Africa",
 "Northwest of Australia","West of Australia","Western Australia",
 "Northern Territory, Australia","South Australia",
 "Gulf of Carpentaria, Australia","Queensland, Australia","Coral Sea",
 "Northwest of New Caledonia","New Caledonia region","Southwest of Australia",
 "Off south coast of Australia","Near coast of South Australia",
 "New South Wales, Australia","Victoria, Australia",
 "Near southeast coast of Australia","Near east coast of Australia",
 "East of Australia","Norfolk Island, Australia, region",
 "Northwest of New Zealand","Bass Strait, Australia",
 "Tasmania, Australia, region","Southeast of Australia","North Pacific Ocean",
 "Hawaiian Islands region","Hawaiian Islands, United States",
 "Eastern Caroline Islands, Micronesia, region","Marshall Islands region",
 "Enewetak Atoll, Marshall Islands, region",
 "Bikini Atoll, Marshall Islands, region","Gilbert Islands, Kiribati, region",
 "Johnston Island region","Line Islands, Kiribati, region",
 "Palmyra Island, Kiribati, region","Kiritimati, Kiribati, region",
 "Tuvalu region","Phoenix Islands, Kiribati, region","Tokelau Islands region",
 "Northern Cook Islands","Cook Islands region","Society Islands region",
 "Tubuai Islands region","Marquesas Islands region",
 "Tuamotu Archipelago region","South Pacific Ocean","Lomonosov Ridge",
 "Arctic Ocean","Near north coast of Kalaallit Nunaat",
 "Eastern Kalaallit Nunaat","Iceland region","Iceland",
 "Jan Mayen Island region","Greenland Sea","North of Svalbard",
 "Norwegian Sea","Svalbard, Norway, region","North of Franz Josef Land",
 "Franz Josef Land, Russia","Northern Norway","Barents Sea",
 "Novaya Zemlya, Russia","Kara Sea",
 "Near coast of northwestern Siberia, Russia","North of Severnaya Zemlya",
 "Severnaya Zemlya, Russia","Near coast of northern Siberia, Russia",
 "East of Severnaya Zemlya","Laptev Sea","Southeastern Siberia, Russia",
 "Priamurye-Northeastern China border region","Northeastern China",
 "North Korea","Sea of Japan","Primorye, Russia","Sakhalin Island, Russia",
 "Sea of Okhotsk","Southeastern China","Yellow Sea",
 "Off east coast of southeastern China","North of New Siberian Islands",
 "New Siberian Islands, Russia","Eastern Siberian Sea",
 "Near north coast of eastern Siberia, Russia","Eastern Siberia, Russia",
 "Chukchi Sea","Bering Strait","St. Lawrence Island, United States, region",
 "Beaufort Sea","Northern Alaska, United States",
 "Northern Yukon Territory, Canada","Queen Elizabeth Islands, Canada",
 "Northwest Territories, Canada","Western Kalaallit Nunaat","Baffin Bay",
 "Baffin Island, Canada, region","Southeastcentral Pacific Ocean",
 "Southern East Pacific Rise","Easter Island region","West Chile Rise",
 "Juan Fernandez Islands, Chile, region","East of North Island, New Zealand",
 "Chatham Islands, New Zealand, region","South of Chatham Islands",
 "Pacific-Antarctic Ridge","Southern Pacific Ocean",
 "Eastcentral Pacific Ocean","Central East Pacific Rise",
 "West of Galapagos Islands","Galapagos Islands region",
 "Galapagos Islands, Ecuador","Southwest of Galapagos Islands",
 "Southeast of Galapagos Islands","South of Tasmania",
 "West of Macquarie Island","Balleny Islands region",
 "Andaman Islands, India, region","Nicobar Islands, India, region",
 "Off west coast of northern Sumatera, Indonesia",
 "Northern Sumatera, Indonesia","Malay Peninsula","Gulf of Thailand",
 "Southeastern Afghanistan","Pakistan","Southwestern Kashmir",
 "India-Pakistan border region","Central Kazakhstan",
 "Southeastern Uzbekistan","Tajikistan","Kyrgyzstan",
 "Afghanistan-Tajikistan border region","Hindu Kush, Afghanistan, region",
 "Tajikistan-Xinjiang border region","Northwestern Kashmir","Finland",
 "Norway-Murmansk border region","Finland-Karelia border region",
 "Baltic States - Belarus - Northwestern Russia region",
 "Northwestern Siberia, Russia","Northern and central Siberia, Russia",
 "Victoria Land, Antarctica","Ross Sea","Antarctica",
 "Northern East Pacific Rise","North of Honduras",
 "East of South Sandwich Islands","Thailand","Laos","Kampuchea","Vietnam",
 "Gulf of Tongking","Reykjanes Ridge","Azores-Cape St. Vincent Ridge",
 "Owen Fracture Zone region","Indian Ocean Triple Junction",
 "Western Indian-Antarctic Ridge","Western Sahara","Mauritania","Mali",
 "Senegal - Gambia region","Guinea region","Sierra Leone","Liberia region",
 "Cote d'Ivoire","Burkina Faso","Ghana","Benin - Togo region","Niger",
 "Nigeria","Southeast of Easter Island","Galapagos Triple Junction region",
 };

if (isreg <= 0 && igreg > 0 && igreg <= nn) isreg = nsreg[igreg];

strcpy( grname, greg[0] );
if (igreg > 0 && igreg <= nn) strcpy( grname, greg[igreg] );

strcpy( srname, sreg[0] );
if (isreg > 0 && isreg <= 50) strcpy( srname, sreg[isreg] );

return;
}



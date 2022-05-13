
/* file fit_travel.c
 *      ============
 *
 * version 9, 21-Nov-2005
 *
 * Fits epicenter location to relative travel times at given depth
 * Needs input file like:
 *
 BFO    0.00
 BRG  -24.19
BRNL  -28.93
 BUG  -11.28
 CLL  -23.90
 CLZ  -19.53
 FUR   -5.76
GRFO  -11.82
 HAM  -25.45
 LID  -38.10
 MOX  -17.43
 TNS   -8.30
 WET  -13.72
! depth  471.00
! latitude   43.40
! longitude  134.76
 *
 * This tells relative travel times of stations in sec and hypocenter location
 * K. Stammler, 8-Aug-96
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1996,  Klaus Stammler, Federal Institute for Geosciences
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "cpar.h"
#include "ptusrdef.h"
#include "earthloc.h"
#include "glusrdef.h"
#include "erusrdef.h"
#include "tcusrdef.h"


/* constants */
#define cTfStatLth 5
#define cTfMaxStations 100


/* types */
typedef struct {
	char     station[cTfStatLth+1];     /* station code */
	double   slat;                      /* station latitude in deg */
	double   slon;                      /* station longitude in deg */
	float    elev;                      /* station elevation */
	float    reltime;                   /* relative onset time */
	float    resid;                     /* computed relidual for best fit */
	char     phase[cBcLineLth+1];       /* phase name */
} TFtReltime;

typedef struct {
	TFtReltime     *rel;                /* pointer to relative times */
	int            obsnum;              /* number of observations */
	double         elat;                /* epicenter latitude */
	double         elon;                /* epicenter longitude */
	float          depth;               /* depth of event */
	char           reftime[cBcLineLth+1]; /* reference time */
} TFtObservation;


/* prototypes of local routines */
static void TfReadObservation( char fname[], TFtObservation *obs );



int main( int argc, char *argv[] )
{
	/* local variables */
	char     relfile[cBcFileLth+1];       /* name of relative onset file */
	TFtObservation obs;                   /* measured values */
	char     *inputs;                     /* pointer to environment */
	char     str[cBcLineLth+1];           /* scratch string */
	int      i;                           /* counter */
	TSyStatus status;                     /* return status */
	double   lat1, lat2, dlat;            /* start, end, delta lat */
	double   lon1, lon2, dlon;            /* start, end, delta lon */
	double   wlat, wlon;                  /* window sizes in lat & lon */
	double   clat, clon;                  /* center location */
	double   lat, lon;                    /* running values */
	float    dep1, dep2, ddep;            /* first, last and step of depth */
	float    dep, mindep;                 /* running value and optimum depth */
	double   dist, azim, bazim;           /* distance vector */
	double   minlat, minlon;              /* best location */
	float    travtime;                    /* absolute travel time in sec */
	float    travoffset;                  /* offset to first value */
	float    reltrav;                     /* relative travel time */
	float    squerror;                    /* squared error */
	float    minsquerror;                 /* minimum rms */
	int      scnt;                        /* station counter */
	float    tmp;                         /* scratch */
	int      itmp;                        /* scratch */
	TSyBoolean first_station;             /* first station used */
	TSyBoolean xyz_output;                /* output for psxyz */
	TSyBoolean loncorrect;                /* correct longitude dimensions */
	TSyBoolean headerline;                /* print header line */
	TSyBoolean nicenum;                   /* nice numbers (1/4 degrees) */
	char     *cptr;                       /* pointer to char */
	char     tmpstr[cBcLineLth+1];        /* scratch string */
	char     pha[cBcLineLth+1];           /* phase a */
	char     phb[cBcLineLth+1];           /* phase b */
	float    tta, ttb;                    /* travel times */
	float    weight;                      /* weight factor for diff phases */
	float    oreltime[cTfMaxStations];    /* observed relative times */
	float    treltime[cTfMaxStations];    /* theo relative times */
	int      relobsidx[cTfMaxStations];   /* index numbers of rel.observations */
	float    reldiff[cTfMaxStations];     /* difference in relative travel t. */
	int      diffobsidx[cTfMaxStations];  /* index numbers of difference obs */
	int      rtc;                         /* counter relative time observations*/
	int      dtc;                         /* counter for difference time obs */
	float    osum, tsum;                  /* scratch */
	char     origtime[cBcTimeLth+1];      /* origin time */

	/* executable code */

	pa_init( argc, argv );
	if  (pa_pnumber() != 1 && pa_pnumber() != 2)  {
		fprintf( stderr, "Usage: %s <reltime-file> [<weight>]\n",
			pa_progname() );
		fprintf( stderr, "   -wlat=<wlat>   width of latitude window\n" );
		fprintf( stderr, "   -wlon=<wlon>   width of longitude window\n" );
		fprintf( stderr, "   -dlat=<dlat>   step size in latitude\n" );
		fprintf( stderr, "   -dlon=<dlon>   step size in longitude\n" );
		fprintf( stderr, "   -clat=<clat>   center of latitude window\n" );
		fprintf( stderr, "   -clon=<clon>   center of longitude window\n" );
		fprintf( stderr, "   -loncorrect    correct longitude dimensions\n" );
		fprintf( stderr, "   -infoline      print info lines with '!'\n" );
		fprintf( stderr, "   -nicenum       use nice numbers (1/4 deg)\n" );
		fprintf( stderr, "   -xyz           write xyz output\n" );
		return 1;
	} /*endif*/

	/* defaults */
	wlat = 5.0;
	wlon = 5.0;
	dlat = 1.0;
	dlon = 1.0;
	clat = 0.0;
	clon = 0.0;
	dep1 = 33.0;
	ddep = 50.0;
	weight = 1.0;
	*origtime = '\0';

	/* get parameters (overwrite defaults) */
	strcpy( relfile, pa_pvalue(1) );
	if  (pa_pnumber() == 2)  sscanf( pa_pvalue(2), "%f", &weight );
	if  (pa_qspecified("-wlat"))
		sscanf( pa_qvalue("-wlat"), "%lf", &wlat );
	if  (pa_qspecified("-wlon"))
		sscanf( pa_qvalue("-wlon"), "%lf", &wlon );
	if  (pa_qspecified("-dlat"))
		sscanf( pa_qvalue("-dlat"), "%lf", &dlat );
	if  (pa_qspecified("-dlon"))
		sscanf( pa_qvalue("-dlon"), "%lf", &dlon );
	if  (pa_qspecified("-clat"))
		sscanf( pa_qvalue("-clat"), "%lf", &clat );
	if  (pa_qspecified("-clon"))
		sscanf( pa_qvalue("-clon"), "%lf", &clon );
	if  (pa_qspecified("-dep1"))
		sscanf( pa_qvalue("-dep1"), "%f", &dep1 );
	dep2 = dep1;
	if  (pa_qspecified("-dep2"))
		sscanf( pa_qvalue("-dep2"), "%f", &dep2 );
	if  (pa_qspecified("-ddep"))
		sscanf( pa_qvalue("-ddep"), "%f", &ddep );
	loncorrect = pa_qspecified( "-loncorrect" );
	headerline = pa_qspecified( "-infoline" );
	nicenum = pa_qspecified( "-nicenum" );

	xyz_output = pa_qspecified( "-xyz" );

	/* initialization of libraries */
	inputs = (char *)getenv( "SH_INPUTS" );
	if  (inputs == NULL)  {
		fprintf( stderr, "%s: environment SH_INPUTS not set\n", pa_progname() );
		return 1;
	} /*endif*/
	/* station location file */
	strcpy( str, inputs );
	strcat( str, "/" );
	strcat( str, "STATINF.DAT" );
	gl_locfile_name( str );
	/* travel time tables */
	strcpy( str, inputs );
	strcat( str, "/" );
	status = cBcNoError;
	pt_settabledir( str, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );

	TfReadObservation( relfile, &obs );

#ifdef XXX
	/* test output */
	printf( "epicenter: %7.2lf %7.2lf %7.2f\n", obs.elat, obs.elon, obs.depth );
	printf( "no of reltimes: %d\n", obs.obsnum );
	for  (i=0; i<obs.obsnum; i++)
		printf( "%4s (%7.2lf,%7.2lf) %7.2f\n", obs.rel[i].station,
			obs.rel[i].slat, obs.rel[i].slon, obs.rel[i].reltime );
#endif

	/* set bounds */
	if  (clat == 0.0 && clon == 0.0)  {
		clat = obs.elat;
		clon = obs.elon;
	} /*endif*/
	if  (loncorrect)  {
		wlon /= cos( clat / 180.0 * M_PI );
		dlon /= cos( clat / 180.0 * M_PI );
	} /*endif*/

	if  (nicenum)  {
		/* round to next quarter degrees */
		clat = floor(4.0*clat+0.5) / 4.0;
		clon = floor(4.0*clon+0.5) / 4.0;
		dlat = floor(4.0*dlat+0.5) / 4.0;
		dlon = floor(4.0*dlon+0.5) / 4.0;
		itmp = Nint( wlat/dlat );
		wlat = (double)itmp * dlat;
		itmp = Nint( wlon/dlon );
		wlon = (double)itmp * dlon;
	} /*endif*/

	lat1 = clat - wlat/2.0;
	lat2 = clat + wlat/2.0;
	lon1 = clon - wlon/2.0;
	lon2 = clon + wlon/2.0;

#ifdef XXX
	if  (headerline)
		printf( "! %8.3lf %8.3lf %8.3lf %8.3lf %8.3lf %8.3lf\n",
			lat1, lat2, dlat, lon1, lon2, dlon );
#endif

	/* loop on longitude */
	minlat = lat1;
	minlon = lon1;
	mindep = dep1;
	minsquerror = -1.0;
	for  (dep=dep1; dep<=dep2; dep += ddep)  {
		for  (lon=lon1; lon<=(lon2+dlon/10.0); lon += dlon)  {

			/* loop on latitude */
			for  (lat=lat1; lat<=(lat2+dlat/10.0); lat += dlat)  {

				/* loop on all observations */
				first_station = TRUE;
				squerror = 0.0;
				scnt = 0;
				rtc = 0;
				dtc = 0;
				for  (i=0; i<obs.obsnum; i++)  {

					/* get distance from station to current grid point */
					mb_locdiff( obs.rel[i].slat, obs.rel[i].slon, lat, lon,
						&dist, &azim, &bazim );
					/* travel time on this distance */
					strcpy( tmpstr, obs.rel[i].phase );
					cptr = strchr( tmpstr, '-' );
					if  (cptr != NULL)  {
						/* this is a difference travel time between two phases */
						*cptr = ' ';
						if  (sscanf(tmpstr,"%s %s",pha,phb) != 2)  {
							fprintf( stderr, "%s: error parsing phase.  Abort.\n",
								argv[0] );
							exit( 1 );
						} /*endif*/
						if  (dtc == cTfMaxStations)  {
							fprintf( stderr, "%s: too many diff times\n", argv[0] );
							exit( 1 );
						} /*endif*/
						status = cBcNoError;
						tta = pt_travel( pha, (float)dist, dep, &status );
						if  (status == cBcNoError)
							ttb = pt_travel( phb, (float)dist, dep, &status );
						if  (status == cBcNoError)  {
							reltrav = tta - ttb;
							reldiff[dtc] = (reltrav - obs.rel[i].reltime) * weight;
							squerror += (reldiff[dtc]*(reldiff[dtc]));
							diffobsidx[dtc] = i;
							dtc++;
							scnt++;
						} /*endif*/
						status = cBcNoError;
					} else {
						/* this is the plane wave part */
						status = cBcNoError;
						if  (rtc == cTfMaxStations)  {
							fprintf( stderr, "%s: too many stations\n", argv[0] );
							exit( 1 );
						} /*endif*/
						treltime[rtc] = pt_travel( obs.rel[i].phase, (float)dist,
							dep, &status );
						oreltime[rtc] = obs.rel[i].reltime;
						relobsidx[rtc] = i;
						rtc++;
						if  (SySevere(&status))  {
							treltime[rtc] = 1.0e5;
							status = cBcNoError;
						} /*endif*/
					} /*endif*/
				} /*endfor observations*/

				if  (rtc < 3)  {
					fprintf( stderr, "%s: less than 3 relative times.\n", argv[0] );
					exit( 1 );
				} /*endif*/

				/* from obs.obsnum observations found rtc main phases */
				/* now demean these rtc times and compute rms */
				/* loop over all stations or main phases */
				osum = tsum = 0.0;
				for  (i=0; i<rtc; i++)  osum += oreltime[i];
				for  (i=0; i<rtc; i++)  tsum += treltime[i];
				osum /= (float)rtc;
				tsum /= (float)rtc;
				for  (i=0; i<rtc; i++)  oreltime[i] -= osum;
				for  (i=0; i<rtc; i++)  treltime[i] -= tsum;
				for  (i=0; i<rtc; i++)  {
					tmp = oreltime[i] - treltime[i];
					squerror += tmp*tmp;
					scnt++;
				} /*endfor*/

				if  (scnt > 0)  squerror = sqrt( squerror / (float)scnt );

	 			/* store best values */
				if  (minsquerror < 0.0 || squerror < minsquerror)  {
					minsquerror = squerror;
					minlat = lat;
					minlon = lon;
					mindep = dep;
					for (i=0; i<rtc; i++)
						obs.rel[relobsidx[i]].resid = oreltime[i] - treltime[i];
					for  (i=0; i<dtc; i++)
						obs.rel[diffobsidx[i]].resid = reldiff[i] / weight;
					if  (obs.reftime[0] != '\0')  {
						/* compute origin time from first (0th) main phase */
						status = cBcNoError;
						/* take abolute time of phase reading ... */
						tc_tadd( obs.reftime, oreltime[0]+osum, origtime, &status );
						if  (SySevere(&status))  err_writemsg( status, "", TRUE );
						/* ... and subtract residual time ... */
						tc_tadd( origtime, -obs.rel[relobsidx[0]].resid,
							origtime, &status );
						if  (SySevere(&status))  err_writemsg( status, "", TRUE );
						/* ... then subtract theoretical travel time */
						tc_tadd( origtime, -(treltime[0]+tsum), origtime, &status );
						if  (SySevere(&status))  err_writemsg( status, "", TRUE );
					} /*endif*/
				} /*endif*/

				if  (xyz_output)  {
					printf( "  %8.3lf %8.3lf %7.3f %5.1f depth",
						lat, lon, squerror, dep );
#ifdef XXX
					/* mark reference location */
					tmp = (clat-lat)*(clat-lat) + (clon-lon)*(clon-lon);
					if  (tmp < 1.0e-8)  printf( "  ref" );
#endif
					printf( "\n" );
				} else {
					printf( "  %7.2f", squerror );
				} /*endif*/

			} /*endfor latitude*/

			if  (!xyz_output)
				printf( "\n" );

		} /*endfor longitude*/

	} /*endfor depth*/

	if  (headerline)  {
		for  (i=0; i<obs.obsnum; i++)
			printf( "! %5s %5s %7.2f\n", obs.rel[i].station,
				obs.rel[i].phase, obs.rel[i].resid );
		printf(
			"! minsq: %7.3f  lat: %8.2lf  lon: %8.2lf  dep: %5.1f  orig: %s\n",
			minsquerror, minlat, minlon, mindep, origtime );
	} /*endif*/

	return 0;

} /* end of main */



/*----------------------------------------------------------------------------*/



static void TfReadObservation( char fname[], TFtObservation *obs )

/* Reads relative times from file
 *
 * parameters of routine
 * char       fname[];          input; name of relative time file
 * TFtObservation *obs;         output; measured values read from file
 */
{
	/* local variables */
	FILE     *rf;                         /* pointer to relative time file */
	char     line[cBcLineLth+1];          /* current line of file */
	int      i;                           /* counter */
	GLT_STATINF statinf;                  /* station info */
	TSyStatus status;                     /* return status */

	/* executable code */

	/* open file */
	rf = fopen( fname, "r" );
	if  (rf == NULL)  {
		fprintf( stderr, "%s: cannot open input file %s\n",
			pa_progname(), fname );
		exit( 1 );
	} /*endif*/

	/* default values */
	obs->elat = 0.0;
	obs->elon = 0.0;
	obs->depth = 33.0;
	obs->reftime[0] = '\0';

	/* first count number of relative times and find epicenter start location */
	obs->obsnum = 0;
	while  (fgets(line,cBcLineLth,rf) != NULL)  {
		if  (*line != '\n' && *line != '!')  (obs->obsnum)++;
		if  (strncmp(line,"! latitude ",11) == 0)
			sscanf( line+11, "%lf", &(obs->elat) );
		if  (strncmp(line,"! longitude ",12) == 0)
			sscanf( line+12, "%lf", &(obs->elon) );
		if  (strncmp(line,"! depth ",8) == 0)
			sscanf( line+8, "%f", &(obs->depth) );
		if  (strncmp(line,"! reftime ",10) == 0)
			sscanf( line+10, "%s", obs->reftime );
	} /*endwhile*/

	/* allocate memory for relative times */
	obs->rel = (TFtReltime *)malloc( (obs->obsnum)*sizeof(TFtReltime) );
	if  (obs->rel == NULL)  {
		fprintf( stderr, "%s: memory allocation error\n", pa_progname() );
		exit( 1 );
	} /*endif*/

	/* reread file and get relative times */
	i = 0;
	rewind( rf );
	while  (fgets(line,cBcLineLth,rf) != NULL)  {
		if  (*line == '\n' || *line == '!')  continue;
		if  (sscanf(line,"%s %f %s",&(obs->rel[i].station),&(obs->rel[i].reltime),
			obs->rel[i].phase) != 3)  {
			sscanf( line, "%s %f", &(obs->rel[i].station),
				&(obs->rel[i].reltime) );
			strcpy( obs->rel[i].phase, "P" );
		} /*endif*/
		if  (obs->rel[i].phase[0] >= '0' && obs->rel[i].phase[0] <= '9')
			strcpy( obs->rel[i].phase, "P" );
		obs->rel[i].resid = 0.0;
		i++;
	} /*endwhile*/
	if  (i != obs->obsnum)  {
		fprintf( stderr, "%s: program bug (1)\n", pa_progname() );
		exit( 1 );
	} /*endif*/

	fclose( rf );

	/* get station information */
	status = cBcNoError;
	for  (i=0; i<(obs->obsnum); i++)  {
		gl_statinf( obs->rel[i].station, &statinf, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
		obs->rel[i].slat = statinf.lat;
		obs->rel[i].slon = statinf.lon;
		obs->rel[i].elev = statinf.elevation;
	} /*endfor*/

} /* end of TfReadObservation */



/*----------------------------------------------------------------------------*/



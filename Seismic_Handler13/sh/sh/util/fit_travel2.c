
/* file fit_travel2.c
 *      =============
 *
 * version 10, 21-Nov-2005
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
#include "numres.h"
#include "glusrdef.h"
#include "erusrdef.h"
#include "tcusrdef.h"
#include "globalparams.h"


/* constants */
#define cTfStatLth 5
#define cTfMaxStations 500
#define cTfEmptyDepth -9999.0

/* vector indices */
#define cTfIdxLat 1
#define cTfIdxLon 2
#define cTfIdxDep 3


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
	char           origtime[cBcLineLth+1]; /* origin time (if computed) */
} TFtObservation;


/* global variables */
static TFtObservation tfv_obs;            /* observed picks */
static float          tfv_weight;         /* rel weight of diff phases */
static float          tfv_fixdepth;       /* fixed depth or cTfEmptyDepth */
static TSyBoolean     tfv_trace=FALSE;    /* trace all computed locations */

/* prototypes of local routines */
static void TfReadObservation( char fname[], TFtObservation *obs );
float TfResidualRms( TFtObservation *obs, float lat, float lon, float dep,
	float weight );
float TfEvalFunc( float x[] );



int main( int argc, char *argv[] )
{
	/* local variables */
	char     relfile[cBcFileLth+1];       /* name of relative onset file */
	char     *inputs;                     /* pointer to environment */
	char     str[cBcLineLth+1];           /* scratch string */
	float    lat1, lon1, dep1;            /* start hypocenter */
	float    startstep;                   /* start step size */
	TSyStatus status;                     /* return status */
	float    ftol;                        /* tolerance value */
	int      nfunc;                       /* number of function calls */
	char     tmpstr[cBcLineLth+1];        /* scratch string */
	int      i;                           /* counter */
	/* NR vector definitions */
	float    vec1[3], vec2[3], vec3[3], vec4[3];  /* start points */
	float    *simplex[4], **nrsimplex;    /* pointer to above vectors */
	float    res[4], *nrres;              /* results at start points */
	int      ndim;                        /* number of dimensions (2 or 3) */

	/* executable code */

	pa_init( argc, argv );
	if  (pa_pnumber() != 4 && pa_pnumber() != 5)  {
		fprintf( stderr,
			"Usage: %s [-ftol=<ftol>] [-fixdepth] [-startstep=<step>] <reltime-file> <lat> <lon> <depth> [<weight>]\n",
			pa_progname() );
		return 1;
	} /*endif*/

	tfv_weight = 1.0;
	tfv_fixdepth = cTfEmptyDepth;
	ftol = 0.02;
	startstep = 1.0;

	GpReadParfile();

	/* get parameters (overwrite defaults) */
	strcpy( relfile, pa_pvalue(1) );
	strcpy( tmpstr, pa_pvalue(2) );
	if  (*tmpstr == 'm' || *tmpstr == 's')  *tmpstr = '-';
	sscanf( tmpstr, "%f", &lat1 );
	strcpy( tmpstr, pa_pvalue(3) );
	if  (*tmpstr == 'm' || *tmpstr == 'w')  *tmpstr = '-';
	sscanf( tmpstr, "%f", &lon1 );
	sscanf( pa_pvalue(4), "%f", &dep1 );
	if  (pa_pnumber() == 5)  sscanf( pa_pvalue(5), "%f", &tfv_weight );
	if  (pa_qspecified("-ftol"))  sscanf( pa_qvalue("-ftol"), "%f", &ftol );
	if  (pa_qspecified("-startstep"))
		sscanf( pa_qvalue("-startstep"), "%f", &startstep );
	if  (pa_qspecified("-trace"))  tfv_trace = TRUE;
	if  (pa_qspecified("-fixdepth"))  tfv_fixdepth = dep1;

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

	/* read in relative onset file to global variable */
	TfReadObservation( relfile, &tfv_obs );

	/* setup vector constructions for NR */
	simplex[0] = vec1 - 1;
	simplex[1] = vec2 - 1;
	simplex[2] = vec3 - 1;
	simplex[3] = vec4 - 1;
	nrsimplex = simplex - 1;
	nrres = res - 1;
	ndim = (tfv_fixdepth == cTfEmptyDepth) ? 3 : 2;

	/* setup start points */
	if  (ndim == 2)  {
		/* need 3 start points */
		vec1[0] = lat1;
		vec1[1] = lon1;
		res[0] = TfEvalFunc( vec1-1 );
		vec2[0] = lat1 - startstep;
		vec2[1] = lon1;
		res[1] = TfEvalFunc( vec2-1 );
		vec3[0] = lat1;
		vec3[1] = lon1 - startstep;
		res[2] = TfEvalFunc( vec3-1 );
	} else {
		/* need 4 start points */
		vec1[0] = lat1;
		vec1[1] = lon1;
		vec1[2] = dep1;
		res[0] = TfEvalFunc( vec1-1 );
		vec2[0] = lat1 - startstep;
		vec2[1] = lon1;
		vec2[2] = dep1;
		res[1] = TfEvalFunc( vec2-1 );
		vec3[0] = lat1;
		vec3[1] = lon1 - startstep;
		vec3[2] = dep1;
		res[2] = TfEvalFunc( vec3-1 );
		vec4[0] = lat1;
		vec4[1] = lon1;
		vec4[2] = dep1 + 50.0;
		res[3] = TfEvalFunc( vec4-1 );
	} /*endif*/

	nr_amoeba( nrsimplex, nrres, ndim, ftol, TfEvalFunc, &nfunc );

	/* print residual times of best solution */
	for  (i=0; i<tfv_obs.obsnum; i++)
		printf( "! %5s %12s %7.2f\n", tfv_obs.rel[i].station,
			tfv_obs.rel[i].phase, tfv_obs.rel[i].resid );

	/* print resulting location */
	printf ( "! nfunc %d  ftol %6.4f\n", nfunc, ftol );
	if  (ndim == 2)  {
		printf(
			"! minsq: %7.3f  lat: %8.2lf  lon: %8.2lf  dep: %5.1f  orig: %s\n",
			nrres[1], nrsimplex[1][1], nrsimplex[1][2], tfv_fixdepth,
			tfv_obs.origtime );
	} else {
		printf(
			"! minsq: %7.3f  lat: %8.2lf  lon: %8.2lf  dep: %5.1f  orig: %s\n",
			nrres[1], nrsimplex[1][1], nrsimplex[1][2], fabs(nrsimplex[1][3]),
			tfv_obs.origtime );
	} /*endif*/


} /* end of main */


/*----------------------------------------------------------------------------*/


#define PENALTY 50.0


float TfResidualRms( TFtObservation *obs, float lat, float lon, float dep,
	float weight )

/* Returns rms of residuals of the observed realtive travel times 'obs'
 * for a given hypocenter
 *
 * parameters of routine
 * TFtObservation  *obs;    input; observed realtive travel times
 * float         lat, lon;  input; epicenter location
 * float         dep;       input; source depth in km
 * float         weight;    input; weight of diff phases
 */
{
	/* local variables */
	float    rms;            /* rms value */
	float    minrms;                 /* minimum rms */
	int      scnt;                        /* station counter */
	int      rtc;                         /* counter relative time observations*/
	int      dtc;                         /* counter for difference time obs */
	int      i;                           /* counter */
	double   dist, azim, bazim;           /* distance vector */
	char     tmpstr[cBcLineLth+1];        /* scratch string */
	char     *cptr;                       /* pointer to char */
	char     pha[cBcLineLth+1];           /* phase a */
	char     phb[cBcLineLth+1];           /* phase b */
	TSyStatus status;                     /* return status */
	float    tta, ttb;                    /* travel times */
	float    reltrav;                     /* relative travel time */
	float    reldiff[cTfMaxStations];     /* difference in relative travel t. */
	float    oreltime[cTfMaxStations];    /* observed relative times */
	float    treltime[cTfMaxStations];    /* theo relative times */
	float    osum, tsum, tmp;             /* scratch */
	int      relobsidx[cTfMaxStations];   /* index numbers of rel.observations */
	int      diffobsidx[cTfMaxStations];  /* index numbers of difference obs */

	/* executable code */

	if  (dep >= 700.0  || dep <= -700.0)  return 1.0e5;

	/* loop on all observations */
	obs->origtime[0] = '\0';
	rms = 0.0;
	scnt = 0;
	rtc = 0;
	dtc = 0;
	for  (i=0; i<obs->obsnum; i++)  {

		/* get distance from station to given point */
		mb_locdiff( obs->rel[i].slat, obs->rel[i].slon, lat, lon,
			&dist, &azim, &bazim );
		/* travel time on this distance */
		strcpy( tmpstr, obs->rel[i].phase );
		cptr = strchr( tmpstr, '-' );
		if  (cptr != NULL)  {
			/* this is a difference travel time between two phases */
			*cptr = ' ';
			if  (sscanf(tmpstr,"%s %s",pha,phb) != 2)  {
				fprintf( stderr, "%s: error parsing phase.  Abort.\n",
					pa_progname() );
				return 1.0e5;
			} /*endif*/
			if  (dtc == cTfMaxStations)  {
				fprintf( stderr, "%s: too many diff times\n", pa_progname() );
				return 1.0e5;
			} /*endif*/
			status = cBcNoError;
			tta = pt_travel( pha, (float)dist, dep, &status );
			if  (status == cBcNoError)
				ttb = pt_travel( phb, (float)dist, dep, &status );
			if  (status == cBcNoError)  {
				reltrav = tta - ttb;
				reldiff[dtc] = (reltrav - obs->rel[i].reltime) * weight;
			} else {
				reldiff[dtc] = PENALTY;
			} /*endif*/
			rms += (reldiff[dtc]*(reldiff[dtc]));
			diffobsidx[dtc] = i;
			dtc++;
			scnt++;
			status = cBcNoError;
		} else {
			/* this is the plane wave part */
			status = cBcNoError;
			if  (rtc == cTfMaxStations)  {
				fprintf( stderr, "%s: too many stations\n", pa_progname() );
				return 1.0e5;
			} /*endif*/
			treltime[rtc] = pt_travel( obs->rel[i].phase, (float)dist,
				dep, &status );
			oreltime[rtc] = obs->rel[i].reltime;
			relobsidx[rtc] = i;
			rtc++;
			if  (SySevere(&status))  {
				treltime[rtc] = PENALTY;
				status = cBcNoError;
			} /*endif*/
		} /*endif*/
	} /*endfor observations*/

	if  (rtc < 3)  {
		fprintf( stderr, "%s: less than 3 relative times.\n", pa_progname() );
		return 1.0e5;
	} /*endif*/

	/* from obs->obsnum observations found rtc main phases */
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
		rms += tmp*tmp;
		scnt++;
	} /*endfor*/

	/* store residuals in obs */
	for (i=0; i<rtc; i++)
		obs->rel[relobsidx[i]].resid = oreltime[i] - treltime[i];
	for  (i=0; i<dtc; i++)
		obs->rel[diffobsidx[i]].resid = reldiff[i] / weight;

	/* compute origin time if absolute time available */
	if  (obs->reftime[0] != '\0')  {
		/* compute origin time from first (0th) main phase */
		status = cBcNoError;
		/* take abolute time of phase reading ... */
		tc_tadd( obs->reftime, oreltime[0]+osum, obs->origtime, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
		/* ... and subtract residual time ... */
		tc_tadd( obs->origtime, -obs->rel[relobsidx[0]].resid, obs->origtime,
			&status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
		/* ... then subtract theoretical travel time */
		tc_tadd( obs->origtime, -(treltime[0]+tsum), obs->origtime, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
	} /*endif*/

	if  (scnt > 0)  rms = sqrt( rms / (float)scnt );

	if  (tfv_trace)
		printf( "  %8.3f %8.3f %7.3f %5.1f depth\n", lat, lon, rms, dep );

	return rms;

} /* end of TfResidualRms */



/*----------------------------------------------------------------------------*/



float TfEvalFunc( float x[] )

/* Evaluation function for NR amoeba routine
 *
 * parameters of routine
 * float      x[];    input; input vector (2 or 3 dimensions dep. on fixed depth
 */
{
	/* local variables */
	float    lat, lon, depth;  /* hypocenter to evaluate */

	/* executable code */

	lat = x[cTfIdxLat];
	lon = x[cTfIdxLon];
	if  (tfv_fixdepth == cTfEmptyDepth)
		depth = x[cTfIdxDep];
	else
		depth = tfv_fixdepth;

	/* check values */
	if  (depth < 0.0)  depth = (-depth);
	if  (lat > 90.0)  {
		lat = 180.0 - lat;
		lon += 180.0;
		if  (lon > 180.0)  lon -= 360.0;
	} /*endif*/
	if  (lat < -90.0)  {
		lat = -180.0 - lat;
		lon += 180.0;
		if  (lon > 180.0)  lon -= 360.0;
	} /*endif*/
	if  (lon > 180.0)  lon -= 360.0;
	if  (lon < -180.0)  lon += 360.0;

	return TfResidualRms( &tfv_obs, lat, lon, depth, tfv_weight );

} /* end of TfEvalFunc */



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
	int      i, j;                        /* counters */
	GLT_STATINF statinf;                  /* station info */
	TSyStatus status;                     /* return status */
	int      beamidx;                     /* beam index */
	float    beamlat, beamlon;            /* beam reference location */
	TSyBoolean twice;                     /* station more than once used */
	int      statcnt;                     /* station counter */

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
	beamidx = -1;
	beamlat = beamlon = 0.0;
	statcnt = 0;
	status = cBcNoError;
	for  (i=0; i<(obs->obsnum); i++)  {
		if  (strcmp(obs->rel[i].station,"BEAM") == 0)  {
			if  (beamidx >= 0)  {
				fprintf( stderr, "%s: two BEAM entries.  Abort.\n", pa_progname() );
				exit( 1 );
			} /*endif*/
			beamidx = i;
		} else {
			gl_statinf( obs->rel[i].station, &statinf, &status );
			if  (SySevere(&status))  err_writemsg( status, "", TRUE );
			obs->rel[i].slat = statinf.lat;
			obs->rel[i].slon = statinf.lon;
			obs->rel[i].elev = statinf.elevation;
			/* look whether station already used */
			twice = FALSE;
			for  (j=0; j<i; j++)
				if  (strcmp(obs->rel[j].station,obs->rel[i].station) == 0)
					twice = TRUE;
			if  (!twice)  {
				beamlat += statinf.lat;
				beamlon += statinf.lon;
				statcnt++;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	if  (beamidx >= 0)  {
		if  (statcnt == 0)  statcnt = 1;
		beamlat /= (float)statcnt;
		beamlon /= (float)statcnt;
		obs->rel[beamidx].slat = beamlat;
		obs->rel[beamidx].slon = beamlon;
		obs->rel[beamidx].elev = 0.0;
	} /*endif*/

} /* end of TfReadObservation */



/*----------------------------------------------------------------------------*/



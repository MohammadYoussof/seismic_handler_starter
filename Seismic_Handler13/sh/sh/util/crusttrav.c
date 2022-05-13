
/* file crusttrav.c
 *      ===========
 *
 * version 3, 21-Nov-2005
 *
 * travel times in simple 2-layer crust model
 * K. Stammler, 4-Jul-2002
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
#include "shconst.h"
#include "cpar.h"
#include "tcusrdef.h"
#include "glusrdef.h"
#include "eventdsc.h"
#include "numres.h"


#define CtcStartMinDist 3.0
#define CtcStartAmpDist 3.0
#define CtcStartMinTime 2.0
#define CtcStartAmpTime 3.0
#define CtcStartMinDepth 1.0
#define CtcStartAmpDepth 2.0


/* 'oll' array indices (3-dim) */
#define CtcIdxOllOrigin 1
#define CtcIdxOllXpos   2
#define CtcIdxOllYpos   3
/* 'olld' array indices (4-dim) */
#define CtcIdxOlldOrigin 1
#define CtcIdxOlldXpos   2
#define CtcIdxOlldYpos   3
#define CtcIdxOlldDepth  4


typedef struct {
	char    station[EvSTATIONLTH+1];   /* station name */
	float   lat, lon;                  /* location of station */
	float   elev;                      /* elevation of station */
	float   xpos, ypos;                /* relative location in km */
	char    phase[EvPHASELTH+1];       /* phasename */
	char    onset[cBcTimeLth+1];       /* onset time */
	float   tons;                      /* relative onset time */
} CtPhaseT;

typedef struct {
	float   laywd;                     /* layer width in km */
	float   vpu;                       /* upper P velocity in km/s */
	float   vpd;                       /* lower P velocity in km/s */
	float   vsu;                       /* upper S velocity in km/s */
	float   vsd;                       /* lower S velocity in km/s */
} CtVelmodT;

typedef enum {
	CtcDepthFixed,                     /* depth given */
	CtcDepthFree                       /* depth fitted by picks */
} CtDepthT;

typedef enum {
	CtcOptOLL,                         /* optimize Origin, Lat, Lon */
	CtcOptOLLD                         /* optimize Origin, Lat, Lon, Depth */
} CtOptParT;

typedef struct {
	float   lat, lon;                  /* event location in deg */
	float   xpos, ypos;                /* relative location of epicenter in km */
	float   depth;                     /* source depth in km */
	CtDepthT dptype;                   /* type of depth (fixed or free) */
	float   reltime;                   /* relative onset time */
	char    srctime[cBcTimeLth+1];     /* absolute onset time */
} CtSourceT;


/* prototypes of local routines */
void CtReadPhases( char evtfile[], CtPhaseT *phentry[], int *num );
void CtCompletePickInfo( CtPhaseT ph[], int phnum, float *reflat, float *reflon,
	char reftime[] );
void CtRelToAbs( float reflat, float reflon, char reftime[], CtSourceT *src );
void CtDumpPhases( CtPhaseT phentry[], int num );
void CtWriteResultEvt( CtSourceT *src, char theofile[], float reflat,
	float reflon, CtVelmodT *vm, char evtname[] );
void CtCheckPar( CtVelmodT *vm, CtSourceT *src );
void CtLocate( float ftol, CtPhaseT ph[], int phnum, CtVelmodT *vm,
	CtSourceT *src );
float CtEvalFunc( float x[] );



/* global variables for all routines */
static int    ctv_debug;      /* debug level */



int main( int argc, char *argv[] )
{
	/* local variables */
	char     evtfile[cBcFileLth+1];   /* evt file */
	CtPhaseT *phentry;                /* phase entry */
	int      phnum;                   /* number of phases */
	float    reflat, reflon;          /* reference location */
	char     reftime[cBcTimeLth+1];   /* reference time */
	CtVelmodT vm;                     /* velocity model */
	CtSourceT src;                    /* source parameters */
	float    ftol;                    /* fitting tolerance */
	char     outfile[cBcFileLth+1];   /* output evt file */
	char     theofile[cBcFileLth+1];  /* file with stations for theo phases */

	/* executable code */

	pa_init( argc, argv );

	if  (pa_pnumber() != 5)  {
		fprintf( stderr, "Usage: %s <evtfile> <srcdep> <laywd> <vpu> <vpd>\n",
			argv[0] );
		fprintf( stderr, "     -debug=<debuglevel>\n" );
		fprintf( stderr, "     -ftol=<tolerance>\n" );
		fprintf( stderr, "     -out=<out-evtfile>\n" );
		fprintf( stderr, "     -theo=<theofile>\n" );
		fprintf( stderr, "     -freedepth\n" );
		return 1;
	} /*endif*/

	/* get parameters */
	strcpy( evtfile, pa_pvalue(1) );
	sscanf( pa_pvalue(2), "%f", &src.depth );
	sscanf( pa_pvalue(3), "%f", &vm.laywd );
	sscanf( pa_pvalue(4), "%f", &vm.vpu );
	sscanf( pa_pvalue(5), "%f", &vm.vpd );
	vm.vsu = vm.vpu / 1.732;
	vm.vsd = vm.vpd / 1.732;

	ctv_debug = 0;
	if  (pa_qspecified("-debug"))
		sscanf( pa_qvalue("-debug"), "%d", &ctv_debug );
	ftol = 0.001;
	if  (pa_qspecified("-ftol"))
		sscanf( pa_qvalue("-ftol"), "%f", &ftol );
	*outfile = '\0';
	if  (pa_qspecified("-out"))
		strcpy( outfile, pa_qvalue("-out") );
	*theofile = '\0';
	if  (pa_qspecified("-theo"))
		strcpy( theofile, pa_qvalue("-theo") );
	src.dptype = pa_qspecified("-freedepth") ? CtcDepthFree : CtcDepthFixed;

	/* read phases */
	CtReadPhases( evtfile, &phentry, &phnum );
	CtCompletePickInfo( phentry, phnum, &reflat, &reflon, reftime );
	if  (ctv_debug > 2)  CtDumpPhases( phentry, phnum );

	CtCheckPar( &vm, &src );
	CtLocate( ftol, phentry, phnum, &vm, &src );
	CtRelToAbs( reflat, reflon, reftime, &src );

	if  (*outfile != '\0')
		CtWriteResultEvt( &src, theofile, reflat, reflon, &vm, outfile );

	printf( "lat: %6.3f  lon: %7.3f  depth: %4.1f (%d) orig: %s\n",
		src.lat, src.lon, src.depth, src.dptype, src.srctime );

	return 0;

} /* end of main */


/*----------------------------------------------------------------------------*/


#define MAXPHASE 30


void CtReadPhases( char evtfile[], CtPhaseT *phentry[], int *num )

/* reads in phases from evtfile.
 *
 * parameters of routine
 * char       evtfile[];      input; event file
 * CtPhaseT   phentry[];      output; list of phase entries
 * int        *num;           output; number of entries read
 */
{
	/* local variables */
	FILE     *evt;         /* pointer to evtfile */
	EvEventT ph;           /* phase entry */
	TSyBoolean eof_found;  /* end of file found */
	TSyStatus locstat;     /* local status */

	/* executable code */

	evt = fopen( evtfile, "r" );
	if  (evt == NULL)  {
		fprintf( stderr, "%s: cannot open input file %s\n", pa_progname(),
			evtfile );
		exit( 1 );
	} /*endif*/

	*phentry = (CtPhaseT *)malloc( (int)sizeof(CtPhaseT) * MAXPHASE );
	if  (phentry == NULL)  {
		fprintf( stderr, "%s: error allocating phase entries\n", pa_progname() );
		exit( 1 );
	} /*endif*/

	*num = 0;
	while  (!feof(evt))  {
		EvInitializeEvent( &ph );
		locstat = cBcNoError;
		EvGetEvent( evt, &ph, &eof_found, &locstat );
		if  (eof_found)  break;
		if  (SySevere(&locstat))  {
			fprintf( stderr, "%s: error reading evt file %s\n", pa_progname(),
				evtfile );
			fclose( evt );
			free( *phentry );
			exit( 1 );
		} /*endif*/
		if  (*num == MAXPHASE)  {
			fprintf( stderr, "%s: too many phases in file %s\n", pa_progname(),
				evtfile );
			fclose( evt );
			free( *phentry );
			exit( 1 );
		} /*endif*/
		strcpy( (*phentry)[*num].station, ph.station );
		strcpy( (*phentry)[*num].phase, ph.phase );
		strcpy( (*phentry)[*num].onset, ph.onset_time );
		(*num)++;
	} /*endwhile*/

	fclose( evt );

} /* end of CtReadPhases */



/*----------------------------------------------------------------------------*/



void CtCompletePickInfo( CtPhaseT ph[], int phnum, float *reflat, float *reflon,
	char reftime[] )

/* completes phase info (reads station location, computes relative times
 * and positions).
 *
 * parameters of routine
 * CtPhaseT   ph[];              modify; list of phases
 * int        phnum;             input; number of phases
 * float      *reflat, *reflon;  output; reference location
 * char       reftime[];         output; reference time
 */
{
	/* local variables */
	int      i;             /* counter */
	GLT_STATINF  *statinf;  /* pointer to station info */
	TSyStatus locstat;      /* local status */
	char     *env;          /* pointer to environment */
	char     locname[cBcFileLth+1];   /* pointer to location file */
	float    tdiff;         /* time difference */

	/* executable code */

	env = (char *)getenv( "SH_INPUTS" );
	if  (strlen(env)+12 > cBcFileLth)  {
		fprintf( stderr, "%s: locname name too long\n", pa_progname() );
		exit( 1 );
	} /*endif*/
	strcpy( locname, env );
	strcat( locname, "/" );
	strcat( locname, "STATINF.DAT" );
	gl_locfile_name( locname );

	/* get station locations */
	*reflat = *reflon = 0.0;
	*reftime = '\0';
	for  (i=0; i<phnum; i++)  {
		locstat = cBcNoError;
		statinf = gl_store_station( ph[i].station, TRUE, &locstat );
		if  (SySevere(&locstat))  {
			fprintf( stderr, "%s: unknown station %s\n",
				pa_progname(), ph[i].station );
			exit( 1 );
		} /*endif*/
		ph[i].lat = (float)(statinf->lat);
		ph[i].lon = (float)(statinf->lon);
		ph[i].elev = (statinf->elevation == GLC_INVALID_NUMBER)
			? 0.0 : statinf->elevation/1000.0;
		*reflat += ph[i].lat;
		*reflon += ph[i].lon;
		if  (*reftime == '\0')  {
			strcpy( reftime, ph[i].onset );
		} else {
			tdiff = tc_tdiff( ph[i].onset, reftime, &locstat );
			if  (SySevere(&locstat))  {
				fprintf( stderr, "%s: illegal time string %s\n",
					pa_progname(), ph[i].onset );
				exit( 1 );
			} /*endif*/
			if  (tdiff < 0.0)  strcpy( reftime, ph[i].onset );
		} /*endif*/
	} /*endfor*/

	*reflat /= (float)phnum;
	*reflon /= (float)phnum;

	/* relative locations & times */
	for  (i=0; i<phnum; i++)  {
		ph[i].xpos = (ph[i].lon - *reflon) * SHC_DEG_TO_KM
			* cos(ph[i].lat/SHC_RAD_TO_DEG);
		ph[i].ypos = (ph[i].lat - *reflat) * SHC_DEG_TO_KM;
		locstat = cBcNoError;
		ph[i].tons = tc_tdiff( ph[i].onset, reftime, &locstat );
	} /*endfor*/

} /* end of CtCompletePickInfo */



/*----------------------------------------------------------------------------*/



void CtRelToAbs( float reflat, float reflon, char reftime[], CtSourceT *src )

/* Computes absolute times and positions from realtive values
 *
 * parameters of routine
 * float      reflat, reflon;        input; reference location
 * char       reftime[];             input; reference time
 * CtSourceT  *src;                  input/output; source info to be completed
 */
{
	/* local variables */
	TSyStatus status;      /* return status */

	/* exeutable code */

	status = cBcNoError;
	tc_tadd( reftime, src->reltime, src->srctime, &status );
	if  (SySevere(&status))  {
		fprintf( stderr, "%s: error adding time\n", pa_progname() );
		exit( 1 );
	} /*endif*/

	src->lat = src->ypos / SHC_DEG_TO_KM + reflat;
	src->lon = src->xpos / (SHC_DEG_TO_KM * cos(src->lat/SHC_RAD_TO_DEG))
		+ reflon;

} /* end of CtRelToAbs */



/*----------------------------------------------------------------------------*/



void CtDumpPhases( CtPhaseT phentry[], int num )

/* Dumps out phases read in
 *
 * parameters of routine
 * CtPhaseT   phentry[];      input; list of phases
 * int        num;            input; number of phases
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	for  (i=0; i<num; i++)  {
		printf( "%5s <%6.2f,%5.2f><%7.2f,%7.2f><%5.3f> %3s %s %7.2f\n",
			phentry[i].station, phentry[i].lat, phentry[i].lon,
			phentry[i].xpos, phentry[i].ypos, phentry[i].elev,
			phentry[i].phase, phentry[i].onset, phentry[i].tons );
	} /*endif*/

} /* end of CtDumpPhases */



/*----------------------------------------------------------------------------*/



void CtCheckPar( CtVelmodT *vm, CtSourceT *src )

/* Checks parameters given by user
 *
 * parameters of routine
 * CtVelmodT  *vm;       input; velocity model
 * CtSourceT  *src;      input; source parameters
 */
{
	/* local variables */

	/* executable code */

	if  (src->dptype == CtcDepthFixed && src->depth >= vm->laywd)  {
		fprintf( stderr, "%s: src depth must be greater than layer width\n",
			pa_progname() );
		exit( 1 );
	} /*endif*/
	if  (vm->vpu >= vm->vpd)  {
		fprintf( stderr, "%s: lower layer must be faster than upper layer\n",
			pa_progname() );
		exit( 1 );
	} /*endif*/

} /* end of CtCheckPar */



/*----------------------------------------------------------------------------*/



void CtWriteResultEvt( CtSourceT *src, char theofile[], float reflat,
	float reflon, CtVelmodT *vm, char evtname[] )

/* Writes output evt file
 *
 * parameters of routine
 * CtSourceT  *src;        input; location result
 * char       theofile[];  input; list of stations or NULL
 * float      reflat;      input; reference latitude
 * float      reflon;      input; reference longitude
 * CtVelmodT  *vm;         input; velocity model or NULL
 * char       evtname[];   input; name of output file
 */
{
	/* local variables */
	EvEventT evt;                     /* event structure */
	TSyStatus status;                 /* return status */
	int      i;                       /* counter */
	FILE     *theo;                   /* pointer to theofile */
	char     station[cBcLineLth+1];   /* station name */
	int      slen;                    /* string length */
	float    xsta, ysta;              /* relative station location */
	float    tmpa, tmpb;              /* scratch */
	float    dist;                    /* distance source - station */
	float    travt;                   /* travel time */
	GLT_STATINF  *statinf;            /* pointer to station info */

	/* executable code */

	EvInitializeEvent( &evt );
	evt.latitude = src->lat;
	evt.longitude = src->lon;
	evt.depth = src->depth;
	if  (src->dptype == CtcDepthFixed)  {
		evt.depth_type = EvcDepthPreset;
	} else {
		evt.depth_type = EvcDepthFree;
	} /*endif*/
	strcpy( evt.origin_time, src->srctime );

	status = cBcNoError;
	EvCreateEventfile( evtname, &evt, &status );
	if  (SySevere(&status))  {
		fprintf( stderr, "%s: error writing event file\n", pa_progname() );
		exit( 1 );
	} /*endif*/

	/* if no theoretical phases requested return */
	if  (theofile == NULL || vm == NULL)  return;

	/* loop all stations list file */
	/* open file */
	theo = fopen( theofile, "r" );
	if  (theo == NULL)  {
		fprintf( stderr, "%s: input theofile %s not found.\n", pa_progname(),
			theofile );
		exit( 1 );
	} /*endif*/

	/* loop all lines */
	while  (fgets(station,cBcLineLth,theo) != NULL)  {

		/* read next station name */
		if  (*station == '!' || *station == '\n')  continue;
		slen = strlen( station );
		for  (i=0; i<=slen; i++)
			if  (station[i] == ' ' || station[i] == '\n')
				station[i] = '\0';

		/* compute relative station location */
		statinf = gl_store_station( station, TRUE, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: unknown station %s\n", pa_progname(), station );
			exit( 1 );
		} /*endif*/
		xsta = (statinf->lon - reflon) * SHC_DEG_TO_KM
			* cos(statinf->lat/SHC_RAD_TO_DEG);
		ysta = (statinf->lat - reflat) * SHC_DEG_TO_KM;

		/* compute distance to epicenter */
		tmpa = src->xpos - xsta;
		tmpb = src->ypos - ysta;
		dist = sqrt( tmpa*tmpa + tmpb*tmpb );

		/* phase Pg */
		EvInitializeEvent( &evt );
		strcpy( evt.station, station );
		strcpy( evt.phase, "Pg" );
		tmpa = src->depth + statinf->elevation/1000.0;
		travt = sqrt( tmpa*tmpa + dist*dist ) / vm->vpu;
		tc_tadd( src->srctime, travt, evt.onset_time, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error computing theo arrival\n", pa_progname() );
			exit( 1 );
		} /*endif*/
		evt.pick_type = EvcPickTypeTheo;
		evt.component = 'Z';
		EvAppendEventfile( evtname, &evt, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error writing event file\n", pa_progname() );
			exit( 1 );
		} /*endif*/
		if  (ctv_debug > 1)
			printf( "   %s %s dist %7.2f trav %7.2f\n",
				station, "Pg", dist, travt );

		/* phase Pn */
		EvInitializeEvent( &evt );
		strcpy( evt.station, station );
		strcpy( evt.phase, "Pn" );
		travt = dist/vm->vpd + (2.0*(vm->laywd)
				+ (statinf->elevation/1000.0)-(src->depth))
				* sqrt(1.0/((vm->vpu)*(vm->vpu)) - 1.0/((vm->vpd)*(vm->vpd)));
		tc_tadd( src->srctime, travt, evt.onset_time, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error computing theo arrival\n", pa_progname() );
			exit( 1 );
		} /*endif*/
		evt.pick_type = EvcPickTypeTheo;
		evt.component = 'Z';
		EvAppendEventfile( evtname, &evt, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error writing event file\n", pa_progname() );
			exit( 1 );
		} /*endif*/
		if  (ctv_debug > 1)
			printf( "   %s %s dist %7.2f trav %7.2f\n",
				station, "Pn", dist, travt );

		/* phase Sg */
		EvInitializeEvent( &evt );
		strcpy( evt.station, station );
		strcpy( evt.phase, "Sg" );
		tmpa = src->depth + statinf->elevation/1000.0;
		travt = sqrt( tmpa*tmpa + dist*dist ) / vm->vsu;
		tc_tadd( src->srctime, travt, evt.onset_time, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error computing theo arrival\n", pa_progname() );
			exit( 1 );
		} /*endif*/
		evt.pick_type = EvcPickTypeTheo;
		evt.component = 'N';
		EvAppendEventfile( evtname, &evt, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error writing event file\n", pa_progname() );
			exit( 1 );
		} /*endif*/
		if  (ctv_debug > 1)
			printf( "   %s %s dist %7.2f trav %7.2f\n",
				station, "Sg", dist, travt );

		/* phase Sn */
		EvInitializeEvent( &evt );
		strcpy( evt.station, station );
		strcpy( evt.phase, "Sn" );
		travt = dist/vm->vsd + (2.0*(vm->laywd)
				+ (statinf->elevation/1000.0)-(src->depth))
				* sqrt(1.0/((vm->vsu)*(vm->vsu)) - 1.0/((vm->vsd)*(vm->vsd)));
		tc_tadd( src->srctime, travt, evt.onset_time, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error computing theo arrival\n", pa_progname() );
			exit( 1 );
		} /*endif*/
		evt.pick_type = EvcPickTypeTheo;
		evt.component = 'N';
		EvAppendEventfile( evtname, &evt, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error writing event file\n", pa_progname() );
			exit( 1 );
		} /*endif*/
		if  (ctv_debug > 1)
			printf( "   %s %s dist %7.2f trav %7.2f\n",
				station, "Sn", dist, travt );

	} /*endwhile*/

	/* close file */
	fclose( theo );

} /* end of CtWriteResultEvt */



/*----------------------------------------------------------------------------*/

/* global variables for the following routines */
static CtPhaseT   *ctv_ph;        /* phase picks */
static int        ctv_phnum;      /* number of picks */
static CtVelmodT  ctv_vm;         /* velocity model */
static CtSourceT  ctv_src;        /* source parameters */
static CtOptParT  ctv_optpar;     /* which parameters to optimize */

/*----------------------------------------------------------------------------*/



void CtLocate( float ftol, CtPhaseT ph[], int phnum, CtVelmodT *vm,
	CtSourceT *src )

/* Locates event by minimizing residuals
 *
 * parameters of routine
 * float      ftol;      input; fitting tolerance
 * CtPhaseT   ph[];      input; phase picks
 * int        phnum;     input; number of phases
 * CtVelmodT  *vm;       input; velocity model
 * CtSourceT  *src;      input/output; source parameters
 */
{
	/* local variables */
	TSyStatus status;                   /* return status */
	int      i, j;                      /* counter */
	int      closest;                   /* index of first phase found */
	int      nfunc;                     /* number of function calls */
	char     str[cBcLineLth+1];         /* scratch */
	FILE     *fp;                       /* file pointer */
	static int nrseed=24;               /* for 'random' numbers */
	/* NR vector definitions */
	int      spacedim;                  /* dimension of parameter space */
	float    **vec;                     /* parameter vectors */
	float    **simplex, **nrsimplex;    /* pointer to above vectors */
	float    *res, *nrres;              /* results at start points */

	/* executable code */

	/* get random seed from time */
	system( "date +%M%S >ctseed.000" );
	fp = fopen( "ctseed.000", "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: cannot open scratch file for random seed\n",
			pa_progname() );
	} else {
		fgets( str, cBcLineLth, fp );
		sscanf( str, "%d", &nrseed );
		fclose( fp );
	} /*endif*/

	/* find closest station (with relative time =0.0) */
	closest = -1;
	for  (i=0; i<phnum; i++)  {
		if  (ph[i].tons == 0.0)  {
			closest = i;
			break;
		} /*endif*/
	} /*endfor*/
	if  (closest == -1)  {
		fprintf( stderr, "%s: no closest phase found.\n", pa_progname() );
		exit( 1 );
	} /*endif*/

	/* set global variables for CtEval_... routines */
	ctv_ph = ph;
	ctv_phnum = phnum;
	ctv_vm = *vm;
	ctv_src = *src;
	ctv_optpar = (ctv_src.dptype == CtcDepthFixed) ? CtcOptOLL : CtcOptOLLD;

	/* set dimension of inversion according to set of optimizing parameters */
	switch  (ctv_optpar)  {
	case CtcOptOLL:   spacedim = 3;  break;
	case CtcOptOLLD:  spacedim = 4;  break;
	default:
		fprintf( stderr, "%s: illegal OptPar Type\n", pa_progname() );
		exit( 1 );
	} /*endswitch*/

	/* allocate vectors */
	status = cBcNoError;
	vec = (float **)sy_allocmem( spacedim+1, (int)sizeof(float *), &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );
	simplex = (float **)sy_allocmem( spacedim+1, (int)sizeof(float *), &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );
	res = (float *)sy_allocmem( spacedim+1, (int)sizeof(float), &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );
	for  (i=0; i<=spacedim; i++)  {
		vec[i] = (float *)sy_allocmem( spacedim, (int)sizeof(float), &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
	} /*endfor*/

	/* setup vector constructions for NR */
	for  (i=0; i<=spacedim; i++)
		simplex[i] = vec[i] - 1;
	nrsimplex = simplex - 1;
	nrres = res - 1;

	/* set starting points */
	sy_random(0.0);
	switch  (ctv_optpar)  {
	case CtcOptOLL:
		simplex[0][CtcIdxOllOrigin] = 0.0;
		simplex[0][CtcIdxOllXpos] = ph[closest].xpos;
		simplex[0][CtcIdxOllYpos] = ph[closest].ypos;
		simplex[1][CtcIdxOllOrigin] = CtcStartMinTime
			+ nr_ran3(&nrseed)*CtcStartAmpTime;
		simplex[1][CtcIdxOllXpos] = ph[closest].xpos;
		simplex[1][CtcIdxOllYpos] = ph[closest].ypos;
		simplex[2][CtcIdxOllOrigin] = 0.0;
		simplex[2][CtcIdxOllXpos] = ph[closest].xpos
			- (CtcStartMinDist + nr_ran3(&nrseed)*CtcStartAmpDist);
		simplex[2][CtcIdxOllYpos] = ph[closest].ypos;
		simplex[3][CtcIdxOllOrigin] = 0.0;
		simplex[3][CtcIdxOllXpos] = ph[closest].xpos;
		simplex[3][CtcIdxOllYpos] = ph[closest].ypos
			- (CtcStartMinDist + nr_ran3(&nrseed)*CtcStartAmpDist);
		break;
	case CtcOptOLLD:
		simplex[0][CtcIdxOlldOrigin] = 0.0;
		simplex[0][CtcIdxOlldXpos] = ph[closest].xpos;
		simplex[0][CtcIdxOlldYpos] = ph[closest].ypos;
		simplex[0][CtcIdxOlldDepth] = ctv_src.depth;
		simplex[1][CtcIdxOlldOrigin] = CtcStartMinTime
			+ nr_ran3(&nrseed)*CtcStartAmpTime;
		simplex[1][CtcIdxOlldXpos] = ph[closest].xpos;
		simplex[1][CtcIdxOlldYpos] = ph[closest].ypos;
		simplex[1][CtcIdxOlldDepth] = ctv_src.depth;
		simplex[2][CtcIdxOlldOrigin] = 0.0;
		simplex[2][CtcIdxOlldXpos] = ph[closest].xpos
			- (CtcStartMinDist + nr_ran3(&nrseed)*CtcStartAmpDist);
		simplex[2][CtcIdxOlldYpos] = ph[closest].ypos;
		simplex[2][CtcIdxOlldDepth] = ctv_src.depth;
		simplex[3][CtcIdxOlldOrigin] = 0.0;
		simplex[3][CtcIdxOlldXpos] = ph[closest].xpos;
		simplex[3][CtcIdxOlldYpos] = ph[closest].ypos
			- (CtcStartMinDist + nr_ran3(&nrseed)*CtcStartAmpDist);
		simplex[3][CtcIdxOlldDepth] = ctv_src.depth;
		simplex[4][CtcIdxOlldOrigin] = 0.0;
		simplex[4][CtcIdxOlldXpos] = ph[closest].xpos;
		simplex[4][CtcIdxOlldYpos] = ph[closest].ypos;
		simplex[4][CtcIdxOlldDepth] = ctv_src.depth
			- (CtcStartMinDepth + nr_ran3(&nrseed)*CtcStartAmpDepth);
		if  (simplex[4][CtcIdxOlldDepth] <= 0.0)
			simplex[4][CtcIdxOlldDepth] *= -1.0;
		break;
	default:
		fprintf( stderr, "this cannot happen\n" );
		exit( 1 );
	} /*endswitch*/

	for  (i=0; i<=spacedim; i++)
		res[i] = CtEvalFunc( simplex[i] );

	nr_amoeba( nrsimplex, nrres, spacedim, ftol, CtEvalFunc, &nfunc );

	if  (ctv_debug > 1)  printf( "   %d evaluation calls\n", nfunc );

	/* find index with smallest misfit */
	j = 0;
	for  (i=0; i<spacedim; i++)
		if  (res[i+1] < res[j])  j = i+1;

	switch  (ctv_optpar)  {
	case CtcOptOLL:
		src->reltime = nrsimplex[j+1][CtcIdxOllOrigin];
		src->xpos = nrsimplex[j+1][CtcIdxOllXpos];
		src->ypos = nrsimplex[j+1][CtcIdxOllYpos];
		break;
	case CtcOptOLLD:
		src->reltime = nrsimplex[j+1][CtcIdxOlldOrigin];
		src->xpos = nrsimplex[j+1][CtcIdxOlldXpos];
		src->ypos = nrsimplex[j+1][CtcIdxOlldYpos];
		src->depth = fabs(nrsimplex[j+1][CtcIdxOlldDepth]);
		break;
	default:
		fprintf( stderr, "this cannot happen 2\n" );
		exit( 1 );
	} /*endswitch*/

	if  (ctv_debug > 2)  {
		printf( "   selected: reltime %f xpos %f ypos %f depth %f\n",
			src->reltime, src->xpos, src->ypos, src->depth );
	} /*endif*/

} /* end of CtLocate */



/*----------------------------------------------------------------------------*/



float CtEvalFunc( float x[] )

/* Evaluation function for fitting origin-time,latitude,longitude 'oll'
 *
 * parameters of routine
 * float      x[];         input; input vector in 3-dim space
 */
{
	/* local variables */
	int      i;           /* counter */
	float    tmpa, tmpb;  /* scratch */
	float    dist;        /* distance epicenter station in km */
	float    arriv;       /* arrival time */
	float    misfit;      /* misfit number */
	float    depth;       /* depth parameter */

	/* executable code */

	switch (ctv_optpar)  {
	case CtcOptOLL:
		depth = ctv_src.depth;
		break;
	case CtcOptOLLD:
		depth = fabs( x[CtcIdxOlldDepth] );
		if  (depth > ctv_vm.laywd)  depth = fabs(2.0*ctv_vm.laywd - depth);
		break;
	default:
		fprintf( stderr, "this cannot happen 3\n" );
		exit( 1 );
	} /*endswitch*/

	misfit = 0.0;

	for  (i=0; i<ctv_phnum; i++)  {
		tmpa = x[CtcIdxOllXpos] - ctv_ph[i].xpos;
		tmpb = x[CtcIdxOllYpos] - ctv_ph[i].ypos;
		dist = sqrt( tmpa*tmpa + tmpb*tmpb );
		if  (strcmp(ctv_ph[i].phase,"Pg") == 0)  {
			tmpa = depth + ctv_ph[i].elev;
			arriv = x[CtcIdxOllOrigin]
				+ sqrt( tmpa*tmpa + dist*dist ) / ctv_vm.vpu;
		} else if  (strcmp(ctv_ph[i].phase,"Pn") == 0)  {
			arriv = x[CtcIdxOllOrigin] + dist/ctv_vm.vpd
				+ (2.0*ctv_vm.laywd+ctv_ph[i].elev-depth)
				* sqrt(1.0/(ctv_vm.vpu*ctv_vm.vpu) - 1.0/(ctv_vm.vpd*ctv_vm.vpd));
		} else if  (strcmp(ctv_ph[i].phase,"Sg") == 0)  {
			tmpa = depth + ctv_ph[i].elev;
			arriv = x[CtcIdxOllOrigin]
				+ sqrt( tmpa*tmpa + dist*dist ) / ctv_vm.vsu;
		} else if  (strcmp(ctv_ph[i].phase,"Sn") == 0)  {
			arriv = x[CtcIdxOllOrigin] + dist/ctv_vm.vsd
				+ (2.0*ctv_vm.laywd+ctv_ph[i].elev-depth)
				* sqrt(1.0/(ctv_vm.vsu*ctv_vm.vsu) - 1.0/(ctv_vm.vsd*ctv_vm.vsd));
		} else {
			fprintf( stderr, "%s: illegal phase %s\n",
				pa_progname(), ctv_ph[i].phase );
			exit( 1 );
		} /*endif*/

		if  (ctv_debug > 3)
			printf( "   %s, dist %7.3f arriv %7.3f\n",
				ctv_ph[i].phase, dist, arriv );

		tmpa = arriv - ctv_ph[i].tons;
		misfit += tmpa*tmpa;

	} /*endfor*/

	tmpa = sqrt( misfit );

	if  (ctv_debug > 0)  {
		printf( "   %7.2f %7.2f %7.2f  :  %7.3f\n",
			x[CtcIdxOllOrigin], x[CtcIdxOllXpos], x[CtcIdxOllYpos], tmpa );
	} /*endif*/

	return tmpa;

} /* end of CtEvalFunc */



/*----------------------------------------------------------------------------*/

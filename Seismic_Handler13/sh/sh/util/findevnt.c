
/* file findevnt.c
 *      ==========
 *
 * version 3, 13-Feb-97
 *
 * finds events matching a given arrival time of a specified phase
 * recorded at a given station
 *
 * Syntax: findevnt <station> <phase> <onset>
 *
 * K. Stammler, 29-Jun-92
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
#include BASECNST

#ifdef BC_SUN
#define PTUSRDEF "../source/ptusrdef.h"
#endif

#ifdef BC_VAX
#define PTUSRDEF "shc_utilsrc:ptusrdef.h"
#endif

#ifdef BC_STDLIB_EX
#include <stdlib.h>
#endif

#include BC_SYSBASE
#include BC_CPAR
#include BC_EARTHLOC
#include BC_TCUSRDEF
#include PTUSRDEF
#include BC_GLUSRDEF
#include BC_UTUSRDEF
#include BC_INPFILES


#define PDEDIRECTORY IF_PDEDIR
#define STATIONLOCS IF_STATLOCFILE
#define TRAVELDIR IF_TRAVELDIR


#define MAXEVLTH 20

typedef struct {
	char     id[BC_SHORTSTRLTH+1]; /* ID */
	TIME     orig;       /* origin time */
	float    lat, lon;   /* event location */
	float    depth;      /* event depth */
	float    magn;       /* magnitude of event */
	float    dist;       /* distance in deg from recording station */
	float    baz;        /* back-azimuth */
	float    residual;   /* travel time residual */
} EVENTINFO;



/* prototypes of local routines */
void searchfile( char pdefile[], STATLOC statlat, STATLOC statlon,
	char phase[], char onsetstr[], float searchwdw, float toltime,
	int maxevlth, int *evlth, EVENTINFO ev[], BOOLEAN *ok );
void printlist( int evlth, EVENTINFO ev[], FILE *out );


/* global veriable */
char shd_errors[BC_FILELTH+1];



int main( int argc, char *argv[] )
{
	/* local variables */
	char     station[BC_SHORTSTRLTH+1];    /* station name */
	char     phase[BC_SHORTSTRLTH+1];      /* phase name */
	char     onsetstr[BC_LINELTH+1];       /* onset time string */
	float    searchwdw;                    /* search window in sec */
	float    toltime;                      /* tolerance time in sec */
	STATLOC  statlat, statlon;             /* location of station */
	STATUS   status;                       /* return status */
	NTIME    ntime;                        /* numeric time */
	char     pdefile[BC_FILELTH+1];        /* name of PDE file */
	char     outfile[BC_FILELTH+1];        /* output file */
	unsigned year;                         /* year of event */
	int      evlth;                        /* number of events found */
	FILE     *out;                         /* pointer to output file */
	BOOLEAN  ok;                           /* flag */
	EVENTINFO ev[MAXEVLTH];                /* event list */
	char    path[BC_FILELTH+1];            /* path to input files */
	char    str[BC_LINELTH+1];             /* scratch string */
#	ifdef BC_SUN
	char    *eptr;                         /* environment pointer */
#	endif

	/* executable code */

	status = BC_NOERROR;

#	ifdef BC_SUN
	eptr = getenv( "SH_ERRORS" );
	if  (eptr != NULL)  strcpy( shd_errors, eptr );
	eptr = getenv( "SH_INPUTS" );
	if  (eptr != NULL)  {
		strcpy( path, eptr );
	} else {
		*path = '\0';
	} /*endif*/
	eptr = getenv( "SH_PDE" );
	if  (eptr != NULL)  {
		strcpy( pdefile, eptr );
	} else {
		*pdefile = '\0';
	} /*endif*/
#	else
	strcpy( path, IF_PATH );
	strcpy( pdefile, IF_PDEDIR );
#	endif

	/* get parameters */
	pa_init( argc, argv );
	if  (pa_pnumber() != 3)  {
		printf( "*** Usage: findevnt <station> <phase> <onset> ***\n" );
		printf( "    qualifers: -o=<output> -s=<searchwdw> -t=<tolerance>\n" );
		return 0;
	} /*endif*/
	if  (strlen(pa_pvalue(1)) > BC_SHORTSTRLTH)  {
		printf( "*** station name too long ***\n" );
		return 0;
	} /*endif*/
	strcpy( station, pa_pvalue(1) );
	if  (strlen(pa_pvalue(2)) > BC_SHORTSTRLTH)  {
		printf( "*** phase name too long ***\n" );
		return 0;
	} /*endif*/
	strcpy( phase, pa_pvalue(2) );
	if  (strlen(pa_pvalue(3)) > BC_LINELTH)  {
		printf( "*** onset string too long ***\n" );
		return 0;
	} /*endif*/
	strcpy( onsetstr, pa_pvalue(3) );
	ut_cap( station );
	/* get output file */
	if  (pa_qspecified("-o"))  {
		strcpy( outfile, pa_qvalue("-o") );
		out = sy_fopen( outfile, "w" );
		if  (out == NULL)  {
			printf( "*** couldn't open output file %s ***\n", outfile );
			return 0;
		} /*endif*/
	} else {
		out = stdout;
	} /*endif*/
	/* get search window */
	if  (pa_qspecified("-s"))  {
		sscanf( pa_qvalue("-s"), "%f", &searchwdw );
	} else {
		searchwdw = 3600.0;
	} /*endif*/
	/* get tolerance time */
	if  (pa_qspecified("-t"))  {
		sscanf( pa_qvalue("-t"), "%f", &toltime );
	} else {
		toltime = 5.0;
	} /*endif*/

	/* get location of station */
	pt_settabledir( path, &status );
	if  (Severe(&status))  {
		printf( "*** couldn't set travel time directory ***\n" );
		return 0;
	} /*endif*/
	strcpy( str, path );
	strcat( str, IF_STATLOCFILE );
	gl_locfile_name( str );
	gl_statloc( station, &statlat, &statlon, &status );
	if  (Severe(&status))  {
		printf( "*** couldn't get station location ***\n" );
		return 0;
	} /*endif*/

	/* get year of onset time */
	tc_t2n( onsetstr, &ntime, &status );
	if  (Severe(&status))  {
		printf( "*** couldn't convert time %s ***\n", onsetstr );
		return 0;
	} /*endif*/
	year = ntime.year;
	/* search PDE files, search two if searchwdw intersects two PDE's */
	tc_nadd( &ntime, -searchwdw, &ntime, &status );
	evlth = 0;
	if  (ntime.year < year)  {  /* search previous PDE as well */
		strcpy( pdefile, PDEDIRECTORY );
		strcat( pdefile, "PDE_" );
		sprintf( pdefile+strlen(pdefile), "%4d", year-1 );
		strcat( pdefile, ".DAT" );
		searchfile( pdefile, statlat, statlon, phase, onsetstr,
			searchwdw, toltime, MAXEVLTH, &evlth, ev, &ok );
		if  (!ok)  return 0;
	} /*endif*/
	strcat( pdefile, "PDE_" );
	sprintf( pdefile+strlen(pdefile), "%4d", year );
	strcat( pdefile, ".DAT" );
	searchfile( pdefile, statlat, statlon, phase, onsetstr,
		searchwdw, toltime, MAXEVLTH, &evlth, ev, &ok );
	if  (!ok)  return 0;

	/* output result */
	printlist( evlth, ev, out );
	if  (out != stdout)
		sy_fclose( out );

	return 0;

} /* end of main */



/*---------------------------------------------------------------------*/



void searchfile( char pdefile[], STATLOC statlat, STATLOC statlon,
	char phase[], char onsetstr[], float searchwdw, float toltime,
	int maxevlth, int *evlth, EVENTINFO ev[], BOOLEAN *ok )

/* searches PDE file "pdefile" for matching event which has theoretical
 * onset time of phase within a given time tolerance "toltime".
 *
 * parameters of routine
 * char       pdefile[];         input; PDE file
 * STATLOC    statlat, statlon;  input; station location
 * char       phase[];           input; phase name
 * char       onsetstr[];        input; onset time of phase
 * float      searchwdw;         input; events are checked within searchwdw
 * float      toltime;           input; tolerance time
 * int        maxevlth;          input; maximum number of events
 * int        *evlth             modify; length of event list
 * EVENTINFO  ev[];              modify; list of events
 * BOOLEAN    *ok;               output; return status
 */
{
	/* local variables */
	FILE     *pde;                /* pointer to PDE file */
	char     line[BC_LINELTH+1];  /* current line */
	char     str[BC_LINELTH+1];   /* scratch string */
	NTIME    ntime;               /* numerical time */
	TIME     onset;               /* absolute onset time */
	float    travtime;            /* theor. travel time */
	EVENTINFO lev;                /* current event */
	double   d1, d2, d3;          /* double scratch */
	STATUS   status;              /* return status */

	/* executable code */

	*ok = TRUE;

	/* open input file */
	pde = sy_fopen( pdefile, "r" );
	if  (pde == NULL)  {
		fprintf( stderr, "*** couldn't open PDE file %s ***\n", pdefile );
		*ok = FALSE;
		return;
	} /*endif*/

	/* convert onset time */
	status = BC_NOERROR;
	tc_t2a( onsetstr, &onset, &status );
	if  (Severe(&status))  {
		fprintf( stderr, "*** couldn't convert time %s, status %d ***\n",
			onsetstr, status );
		*ok = FALSE;
		sy_fclose( pde );
		return;
	} /*endif*/

	/* loop over events */
	FOREVER  {
		/* get next line and read it */
		if  (fgets(line,BC_LINELTH,pde) == NULL)  {
			break;
		} /*endif*/
#		ifdef XXX
		sscanf( line, "%s %d %2d%2d%2d%2d%2d.%2d%2s%7f%8f%5f%7s%3f",
			lev.id, &(ntime.year), &(ntime.month), &(ntime.day),
			&(ntime.hour), &(ntime.min), &(ntime.sec), &(ntime.ms),
			scr1, &(lev.lat), &(lev.lon), &(lev.depth), scr2, &(lev.magn) );
#		endif  /* doesn't work */
/*		sscanf( line, "%s %d", lev.id, &(ntime.year) ); */
		sscanf( line+6, "%d", &(ntime.year) );
		strncpy( str, line+11, 2 );  str[2] = '\0';
		sscanf( str, "%2d", &(ntime.month) );
		strncpy( str, line+13, 2 );  str[2] = '\0';
		sscanf( str, "%2d", &(ntime.day) );
		strncpy( str, line+15, 2 );  str[2] = '\0';
		sscanf( str, "%2d", &(ntime.hour) );
		strncpy( str, line+17, 2 );  str[2] = '\0';
		sscanf( str, "%2d", &(ntime.min) );
		strncpy( str, line+19, 2 );  str[2] = '\0';
		sscanf( str, "%2d", &(ntime.sec) );
		strncpy( str, line+22, 2 );  str[2] = '\0';
		sscanf( str, "%2d", &(ntime.ms) );
		strncpy( str, line+26, 7 );  str[7] = '\0';
		sscanf( str, "%7f", &(lev.lat) );
		strncpy( str, line+33, 8 );  str[8] = '\0';
		sscanf( str, "%8f", &(lev.lon) );
		strncpy( str, line+41, 5 );  str[5] = '\0';
		sscanf( str, "%5f", &(lev.depth) );
		strncpy( str, line+53, 3 );  str[3] = '\0';
		sscanf( str, "%3f", &(lev.magn) );
		ntime.ms *= 10;
		/* convert origin time */
		tc_n2a( &ntime, &(lev.orig), &status );
		if  (Severe(&status))  {
			fprintf( stderr, "*** error converting time, status %d ***\n",
				status );
			sy_fclose( pde );
			*ok = FALSE;
			return;
		} /*endif*/
		/* check difference time and process event if in search window */
		lev.residual = tc_adiff( &onset, &(lev.orig) );
		if  (lev.residual < 0.)  break;  /* no more events matching */
		if  (lev.residual < searchwdw)  {
			/* compute distance and back-azimuth */
			mb_locdiff( statlat, statlon, lev.lat, lev.lon, &d1,&d2,&d3 );
			lev.dist = d1;
			lev.baz = d3;
			/* compute theoretical travel time */
			travtime = pt_travel( phase, lev.dist, lev.depth, &status );
			if  (Severe(&status))  {
				fprintf( stderr,
	"*** couldn't compute travel time.  phase: %s  dist: %f,  depth %f ***\n",
					phase, lev.dist, lev.depth );
				status = BC_NOERROR;
				travtime = lev.residual+1.e10;
			} /*endif*/
			lev.residual -= travtime;
			/* if residual within tolerance time, take it */
			if  (Abs(lev.residual) <= toltime)  {
				if  (*evlth >= maxevlth)  {
					fprintf( stderr, "*** event list too long, maximum %d ***\n",
						maxevlth );
					sy_fclose( pde );
					*ok = FALSE;
					return;
				} /*endif*/
				ev[(*evlth)++] = lev;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	sy_fclose( pde );

} /* end of searchfile */



/*---------------------------------------------------------------------*/



void printlist( int evlth, EVENTINFO ev[], FILE *out )

/* prints result to output stream
 *
 * parameters of routine
 * int        evlth;      input; number of events found
 * EVENTINFO  ev[];       input; event list
 * FILE       *out;       input; output file
 */
{
	/* local variables */
	int      i, k, m;             /* counter */
	float    maxmag;              /* maximum magnitude */
	char     tstr[BC_LINELTH+1];  /* time string */
	STATUS   status;              /* return status */

	/* executable code */

	status = BC_NOERROR;

	if  (evlth == 0)  {
		fprintf( stderr, "*** no events matched within tolerance time ***\n" );
		return;
	} /*endif*/

	/* find maximum magnitude */
	maxmag = ev[0].magn;
	m = 0;
	for  (i=1; i<evlth; i++)
		if  (ev[i].magn > maxmag)
			m = i;

	for  (i=0; i<evlth; i++)  {
		k = (i == 0) ? m : i;
		if  (i == m)  k = 0;
		tc_a2t( &(ev[k].orig), tstr, &status );
		if  (Severe(&status))  {
			printf( "*** error converting origin time ***\n" );
			return;
		} /*endif*/
		fprintf( out, "%2d)  %s %6.2f %7.2f %5.1f ",
			i+1, tstr, ev[k].lat, ev[k].lon, ev[k].depth );
		fprintf( out, "%4.1f %5.1f %6.1f %7.2f\n",
			ev[k].magn, ev[k].dist, ev[k].baz, ev[k].residual );
	} /*endfor*/
	fprintf( out, "     origin                    lat    lon    depth  magn dist  baz     residual\n" );

} /* end of printlist */



/*---------------------------------------------------------------------*/

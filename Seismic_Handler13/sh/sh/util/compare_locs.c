
/* file compare_locs.c
 *      ==============
 *
 * version 1, 19-Jan-2005
 *
 * Compares source locations retrieved from EMSC page
 * K. Stammler, 19-Jan-2005
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
#include "basecnst.h"
#include "sysbase.h"
#include "erusrdef.h"
#include "cpar.h"
#include "tcusrdef.h"
#include "earthloc.h"

#define MAXDIST 15.0

typedef struct {
	NTIME     ntime;      /* origin time */
	float     lat;        /* epi latitute */
	float     lon;        /* epi longitude */
	float     magn;       /* magnitude */
	char      agency[10]; /* reporting agency */
} EvInfoT;


/* prototypes of local routines */
static void ReadLine( FILE *fp, EvInfoT *ev );


int main( int argc, char *argv[] )
{
	/* local variables */
	char     f_msg[cBcFileLth+1];     /* file with messages to test */
	char     f_refmsg[cBcFileLth+1];  /* file with reference messages */
	FILE     *fm, *fr;                /* poiner to message and reference file */
	EvInfoT  ev;                      /* event info */
	EvInfoT  evs;                     /* current reference event */
	EvInfoT  evr;                     /* best reference event found */
	float    tdiff;                   /* time difference */
	float    opt_tdiff;               /* smallest time difference */
	double   ddist, azim, bazim;      /* localtion difference */
	float    dist;                    /* single precision difference */
	float    opt_dist;                /* optimum distance found */
	TSyStatus status;                 /* return status */

	/* executable code */

	if  (argc < 3)  {
		fprintf( stderr, "Usage: %s <msg> <refmsg>\n", argv[0] );
		return 1;
	} /*endif*/

	/* get parameters */
	strcpy( f_msg, argv[1] );
	strcpy( f_refmsg, argv[2] );

	/* open files */
	fm = fopen( f_msg, "r" );
	if  (fm == NULL)  {
		fprintf( stderr, "%s: file %s not found.  Abort.\n", argv[0], f_msg );
		return 1;
	} /*endif*/

	while (!feof(fm))  {

		/* read next line from event file */
		ReadLine( fm, &ev );
		printf( "%4d-%02d-%02d %02d:%02d:%02d  %5.1f %6.1f %3.1f   ",
			ev.ntime.year, ev.ntime.month, ev.ntime.day, ev.ntime.hour,
			ev.ntime.min, ev.ntime.sec, ev.lat, ev.lon, ev.magn );

		/* scan reference file */
		fr = fopen( f_refmsg, "r" );
		if  (fr == NULL)  {
			fprintf( stderr, "%s: file %s not found.  Abort.\n", argv[0], f_refmsg );
			fclose( fm );
			return 1;
		} /*endif*/
		opt_tdiff = 5000.0;
		opt_dist = 200.0;
		while  (!feof(fr))  {
			ReadLine( fr, &evs );
			status = cBcNoError;
			tdiff = tc_ndiff( &ev.ntime, &evs.ntime, &status );
			if  (SySevere(&status))  {
				/*fprintf( stderr, "    -- %d %d %d %d %d %d\n", evs.ntime.year,
					evs.ntime.month, evs.ntime.day, evs.ntime.hour, evs.ntime.min,
					evs.ntime.sec );
				err_writemsg( status, "", TRUE );*/
				continue;
			} /*endif*/
			mb_locdiff( ev.lat, ev.lon, evs.lat, evs.lon, &ddist, &azim, &bazim );
			dist = ddist;
			if  (fabs(tdiff) < opt_tdiff && dist < MAXDIST)  {
				opt_tdiff = fabs(tdiff);
				opt_dist = dist;
				evr = evs;
			} /*endif*/
		} /*endwhile*/
		fclose( fr );

		if  (opt_tdiff < 90.0 && opt_dist < MAXDIST)  {
			printf( "%3.0f   %5.1f  ", opt_tdiff, opt_dist );
			if  (ev.magn > 0.0)  {
				printf( "%3.1f/%4.1f", ev.magn, ev.magn-evr.magn );
			} else {
				printf( "NoMag   " );
			} /*endif*/
			printf( " %s\n", evr.agency );
		} else {
			printf( "XXX" );
			if  (ev.magn > 0.0)  {
				printf( "          %3.1f", ev.magn );
			} else {
				printf( "          NoMag  " );
			} /*endif*/
			printf( "\n" );
		} /*endif*/

	} /*endwhile*/

	fclose( fm );

	return 0;

} /* end of main */



/*----------------------------------------------------------------------------*/



static void ReadLine( FILE *fp, EvInfoT *ev )

/* Reads event from file
 *
 * parameters of routine
 * FILE       *fp;    input; pointer to file
 * EvInfoT    *ev;    output; event info read
 */
{
	/* local variables */
	char     line[cBcLineLth+1];    /* current line */

	/* executable code */

	ev->ntime.year = 0;
	ev->ntime.month = 0;
	ev->ntime.day = 0;
	ev->ntime.hour = 0;
	ev->ntime.min = 0;
	ev->ntime.sec = 0;
	ev->ntime.ms = 0;
	ev->lat = 0.0;
	ev->lon = 0.0;
	ev->magn = 0.0;

	fgets( line, cBcLineLth, fp );
	sscanf( line+4, "%4d/%2d/%2d %2d:%2d:%2d", &(ev->ntime.year),
		&(ev->ntime.month), &(ev->ntime.day), &(ev->ntime.hour),
		&(ev->ntime.min), &(ev->ntime.sec) );
	sscanf( line+26, "%f", &(ev->lat) );
	if  (line[31] == 'S')  ev->lat *= -1;
	sscanf( line+33, "%f", &(ev->lon) );
	if  (line[38] == 'W')  ev->lon *= -1;
	if  (line[48] != ' ')
		sscanf( line+48, "%f", &(ev->magn) );
	strncpy( ev->agency, line+56, 4 );
	ev->agency[4] = '\0';

	/*
	printf( "%4d-%02d-%02d %02d:%02d:%02d    %f %f %f\n", ev->ntime.year,
		ev->ntime.month, ev->ntime.day, ev->ntime.hour, ev->ntime.min,
		ev->ntime.sec, ev->lat, ev->lon, ev->magn );
	*/

} /* end of ReadLine*/



/*----------------------------------------------------------------------------*/

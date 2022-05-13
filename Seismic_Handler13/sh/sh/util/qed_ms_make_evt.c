
/* file qed_ms_make_evt.c
 *      =================
 *
 * version 2, 21-Jul-95
 *
 * reads output file of correlation profram between QED's and LP-detections
 * and creates time list with separately created evt-files for input in SHM
 *
 * K. Stammler, 18-Jul-95
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
#include "eventdsc.h"
#include "tcusrdef.h"



#define POS_ONSET 0
#define POS_ORIGIN 26
#define POS_LAT 50
#define POS_LON 58
#define POS_DEPTH 67




int main( int argc, char *argv[] )
{
	/* local variables */
	char     listfile[cBcFileLth+1];    /* name of input file */
	char     outdir[cBcFileLth+1];      /* output directory */
	FILE     *fp;                       /* pointer to input file */
	char     line[cBcLongStrLth+1];     /* line of input file */
	char     evtfile[cBcFileLth+1];     /* name of output file */
	EvEventT event;                     /* event info */
	EvStatusT status;                   /* return status */
	int      evtcnt;                    /* evt file counter */
	char     onset[cBcLineLth+1];       /* onset time */
	float    time_offset;               /* time offset */

	/* executable code */

	status = cBcNoError;

	/* get parameters */
	if  (argc != 4)  {
		fprintf( stderr, "Usage: %s <listfile> <evt-outdir> <time-offset>\n",
			argv[0] );
		return 1;
	} /*endif*/
	strcpy( listfile, argv[1] );
	strcpy( outdir, argv[2] );
	strcat( outdir, "/" );
	sscanf( argv[3], "%f", &time_offset );

	/* open input file */
	fp = fopen( listfile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: error opening input file %s\n", argv[0], listfile );
		return 1;
	} /*endif*/

	/* read all event lines */
	evtcnt = 1;
	while  (fgets(line,cBcLongStrLth,fp) != NULL)  {
		if  (*line == '!')  continue;
		EvInitializeEvent( &event );
		sscanf( line+POS_ONSET, "%s", onset );
		sscanf( line+POS_ORIGIN, "%s", event.origin_time );
		sscanf( line+POS_LAT, "%f", &event.latitude );
		sscanf( line+POS_LON, "%f", &event.longitude );
		sscanf( line+POS_DEPTH, "%f", &event.depth );
		strcpy( evtfile, outdir );
		sprintf( evtfile+strlen(evtfile), "autoevt_%03d.evt", evtcnt++ );
		EvCreateEventfile( evtfile, &event, &status );
		if  (Severe(&status))  {
			fprintf( stderr, "%s: error writing evt-file %s\n", argv[0], evtfile );
			return 1;
		} /*endif*/
		tc_tadd( onset, -time_offset, onset, &status );
		if  (Severe(&status))  {
			fprintf( stderr, "%s: error computing time %s\n", argv[0], onset );
			return 1;
		} /*endif*/
		printf( "%s %s\n", onset, evtfile );
	} /*endwhile*/

	fclose( fp );
	return 0;

} /* end of main */

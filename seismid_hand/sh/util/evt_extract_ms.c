
/* file evt_extract_ms.c
 *      ================
 *
 * version 2, 16-Jan-2007
 *
 * extracts MS values from evt file
 * K. Stammler, 6-Jul-95
 *
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
#include "tcusrdef.h"
#include "cpar.h"
#include "eventdsc.h"
#include "erusrdef.h"



/* prototypes of local routines */
static void WriteInfoLineAndClear( EvEventT *linfo );



int main( int argc, char *argv[] )
{
	/* local variables */
	char     inpfile[cBcFileLth+1];  /* name of input file */
	FILE     *fp;                    /* pointer to input file */
	EvEventT event;                  /* event (phase) info */
	BOOLEAN  eof;                    /* eof found */
	EvStatusT status;                /* return status */
	long     curr_evid;              /* current event ID */
	EvEventT linfo;                  /* L info */

	/* executable code */

	status = cBcNoError;
	pa_init( argc, argv );

	if  (pa_pnumber() != 1)  {
		fprintf( stderr, "Usage: %s <inpfile>\n", pa_progname() );
		return 1;
	} /*endif*/
	strcpy( inpfile, pa_pvalue(1) );

	/* open input file */
	fp = sy_fopen( inpfile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: input file %s not found\n",
			pa_progname(), inpfile );
		return 1;
	} /*endif*/

	/* loop all phases on input */
	linfo.evid = 0;
	FOREVER  {
		EvGetEvent( fp, &event, &eof, &status );
		if  (eof)  break;
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		if  (event.evid != linfo.evid)  {
			/* new event, write out info */
			WriteInfoLineAndClear( &linfo );
			linfo.evid = event.evid;
		} /*endif*/
		if  (event.mag[EvcMagMs] != EvEMPTY_MAGNITUDE)  {
			linfo.mag[EvcMagMs] = event.mag[EvcMagMs];
			strcpy( linfo.station, event.station );
			if  (strcmp(event.phase,"L") != 0
				&& strcmp(event.phase,"(L)") != 0)  {
				fprintf( stderr, "%s: MS not on L-phase (phase %s)\n",
					pa_progname(), event.phase );
			} else {
				strcpy( linfo.onset_time, event.onset_time );
			} /*endif*/
		} /*endif*/
		if  (event.latitude != EvEMPTY_LATITUDE)  {
			linfo.latitude = event.latitude;
			linfo.longitude = event.longitude;
			linfo.depth = event.depth;
			strcpy( linfo.origin_time, event.origin_time );
		} /*endif*/
	} /*endfor*/
	WriteInfoLineAndClear( &linfo );

	sy_fclose( fp );

} /* end of main */



/*----------------------------------------------------------------------------*/



static void WriteInfoLineAndClear( EvEventT *linfo )

/* write out information if available and clears variables
 *
 * parameters of routine
 * EvEventT   *linfo;      modify; L info
 */
{
	/* local variables */

	/* executable code */

	if  (linfo->mag[EvcMagMs] != 0.0)  {
		printf( "%d %24s %5s %3.1f %24s %6.2f %7.2f %5.1f\n", linfo->evid,
			linfo->onset_time, linfo->station,
			linfo->mag[EvcMagMs], linfo->origin_time, linfo->latitude, linfo->longitude,
			linfo->depth );
	} /*endif*/

	linfo->mag[EvcMagMs] = 0.0;
	linfo->latitude = 0.0;
	linfo->longitude = 0.0;
	linfo->depth = 0.0;
	linfo->onset_time[0] = '\0';
	linfo->origin_time[0] = '\0';
	linfo->station[0] = '\0';

} /* end of WriteInfoLineAndClear */



/*----------------------------------------------------------------------------*/

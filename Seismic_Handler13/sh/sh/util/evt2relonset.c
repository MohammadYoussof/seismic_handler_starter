
/* file evt2relonset.c
 *      ==============
 *
 * version 2, 3-Mar-2000
 *
 * Returns relative travel times of evt-phases (using first evt-phase found).
 * Ignores all phases in evt-file which do not match the passed phase name.
 * Similar routine as in 'evt2reltimes.c', but with phase matching.
 * K. Stammler, 3-Apr-97
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


/* prototypes of local routines */
void EfPutRelOnsets( FILE *out, EvEventT *event, EvStatusT *status );


/* global variables */
static char erv_phasename[cBcLineLth+1];      /* phase name to be used */



int main( int argc, char *argv[] )
{
	/* local variables */
	char      infile[cBcFileLth+1];    /* input file */
	char      outfile[cBcFileLth+1];   /* output file */
	EvStatusT status;                  /* return status */
	int       i;                       /* counter */

	/* executable code */

	if  (argc != 4)  {
		fprintf( stderr, "Usage: %s <phase> <input> <output>\n", argv[0] );
		return 1;
	} /*endif*/

	strcpy( erv_phasename, argv[1] );
	strcpy( infile, argv[2] );
	strcpy( outfile, argv[3] );

	status = EveNOERROR;
	EvReformatEventfile( infile, outfile, EvGetEvent,
		EfPutRelOnsets, &status );

	return 0;

} /* end of main */



/*---------------------------------------------------------------------------*/



void EfPutRelOnsets( FILE *out, EvEventT *event, EvStatusT *status )

/* Prints error messages of event to file 'out'.
 *
 * parameters of routine
 * FILE       *out;          input; output file
 * EvEventT   *event;        input; event information
 * EvStatusT  *status;       output; return status
 */
{
	/* local variables */
	static TSyBoolean first=TRUE;  /* first phase */
	static char   phasename[cBcLineLth+1];  /* name of phase */
	static char   arrtime[cBcTimeLth+1];    /* arrival time of first phase */
	static float  depth=EvEMPTY_DEPTH;      /* depth found */
	static float  lat=EvEMPTY_LATITUDE;     /* epicenter latitude */
	static float  lon=EvEMPTY_LONGITUDE;    /* epicenter longitude */
	static char   phase[cBcLineLth+1]="";   /* phase name */
	float    tdiff;                         /* relative time in sec */

	/* executable code */

	if  (event == NULL)  {
		if  (depth != EvEMPTY_DEPTH)  fprintf( out, "! depth %7.2f\n", depth );
		if  (lat != EvEMPTY_LATITUDE)  fprintf( out, "! latitude %7.2f\n", lat );
		if  (lon != EvEMPTY_LONGITUDE)  fprintf( out, "! longitude %7.2f\n", lon);
		if  (*phase != '\0')  fprintf( out, "! phase %s\n", phase );
		return;
	} /*endif*/

	/* ignore phases not matching */
	if  (strcmp(event->phase,erv_phasename) != 0)  return;

	if  (first)  {
		strcpy( phasename, event->phase );
		strcpy( arrtime, event->onset_time );
		first = FALSE;
	} /*endif*/

	if  (event->depth != EvEMPTY_DEPTH)  depth = event->depth;
	if  (event->latitude != EvEMPTY_LATITUDE)  lat = event->latitude;
	if  (event->longitude != EvEMPTY_LONGITUDE)  lon = event->longitude;
	if  (*phase == '\0' && event->phase[0] != '\0')
		strcpy( phase, event->phase );

	tdiff = tc_tdiff( event->onset_time, arrtime, status );
	if  (SySevere(status))  return;

	fprintf( out, "%4s %7.2f -1.0\n", event->station, tdiff );

} /* end of EfPutRelOnsets */



/*---------------------------------------------------------------------------*/


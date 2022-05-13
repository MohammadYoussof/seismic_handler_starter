
/* file matching_event.csh
 *      ==================
 *
 * version 2, 22-May-2006
 *
 * Returns matching event for given phase, slowness and azimuth
 * K. Stammler, 9-Apr-2006
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "erusrdef.h"
#include "tcusrdef.h"
#include "ptusrdef.h"
#include "earthloc.h"
#include "glusrdef.h"
#include "seismics.h"


int main( int argc, char *argv[] )
{
	/* local variables */
	char     agency[cBcLineLth+1];    /* name of agency to be searched */
	char     phase[cBcLineLth+1];     /* phase to be saerched */
	char     onset[cBcLineLth+1];     /* onset time of phase */
	char     station[cBcLineLth+1];   /* station name of phasepick */
	float    slowness;                /* slowness of phase */
	float    azimuth;                 /* azimuth of phase */
	TSyStatus status;                 /* return status */
	float    lat, lon;                /* epicentre */
	float    depth;                   /* source depth */
	char     origin[cBcLineLth+1];    /* origin time */
	char     repagency[cBcLineLth+1]; /* reporting agency */
	/* scratch */
	char     *env;                    /* pointer to environment */
	char     str[cBcLongStrLth+1];    /* string */

	/* executable code */

	if  (argc != 7)  {
		fprintf( stderr, "Usage: %s <agency> <phase> <onset> <station> <slowness> <azimuth>\n",
			argv[0] );
		return 1;
	} /*endif*/

	/* get parameters */
	strcpy( agency, argv[1] );
	strcpy( phase, argv[2] );
	strcpy( onset, argv[3] );
	strcpy( station, argv[4] );
	sscanf( argv[5], "%f", &slowness );
	sscanf( argv[6], "%f", &azimuth );

	/*
	printf( "ag:%s phase:%s onset:%s station:%s slow:%4.1f az:%5.1f\n",
		agency, phase, onset, station, slowness, azimuth );
	*/

	status = cBcNoError;

	/* initialisation */
	env = (char *)getenv( "SH_INPUTS" );
	if  (env == NULL) {
		fprintf( stderr, "%s: environment variable SH_INPUTS not set.  Abort.\n" );
		return 1;
	} /*endif*/
	pt_settabledir( env, &status );
	if  (SySevere(&status))  {
		fprintf( stderr, "%s: error setting tt table dir\n", argv[0] );
		return 1;
	} /*endif*/
	strcpy( str, env );
	strcat( str, "STATINF.DAT" );
	gl_locfile_name( str );

	si_match_location( agency, phase, onset, station, slowness, azimuth,
		TRUE, &lat, &lon, &depth, origin, repagency, &status );

	if  (status == cBcNoError)  {
		printf( "lat:%5.2f lon:%6.2f depth:%5.1f origin:%s rep:%s\n",
			lat, lon, depth, origin, repagency );
	} else if  (status == SIE_NO_EV_FOUND)  {
		printf( "no matching event found\n" );
	} else {
		printf( "status %d\n", status );
	} /*endif*/

	return 0;

} /* end of main */

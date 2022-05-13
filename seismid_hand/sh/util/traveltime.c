
/* file traveltime.c
 *      ============
 *
 * version 4, 21-Nov-2005
 *
 * Computes travel time in seconds
 * K. Stammler, 7-Aug-96
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
#include "ptusrdef.h"
#include "erusrdef.h"
#include "cpar.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char     *env;                       /* pointer to environment */
	char     tablepath[cBcFileLth+1];    /* name of table directory */
	TSyStatus status;                    /* return status */
	char     phase[cBcLineLth+1];        /* phase name */
	float    distance;                   /* distance in deg */
	float    depth;                      /* depth in km */
	float    ttime;                      /* travel time in sec */
	TSyBoolean compute_slowness;         /* compute slowness instead of time */
	TSyBoolean quiet;                    /* don't complain on wrong distance */

	/* executable code */

	pa_init( argc, argv );
	if  (pa_pnumber() != 3)  {
		fprintf( stderr, "Usage: %s <phase> <distance> <depth>\n",
			pa_progname() );
		fprintf( stderr, "  Options:   -s   compute slowness instead of time\n" );
		fprintf( stderr, "             -q   be quiet on errors\n" );
		return 1;
	} /*endif*/

	status = cBcNoError;

	env = (char *)getenv( "SH_INPUTS" );
	if  (env == NULL)  {
		fprintf( stderr, "%s: environment SH_INPUTS not set\n", pa_progname() );
		return 1;
	} /*endif*/
	strcpy( tablepath, env );
	strcat( tablepath, "/" );
	pt_settabledir( tablepath, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );

	/* get command line parameters */
	strcpy( phase, pa_pvalue(1) );
	sscanf( pa_pvalue(2), "%f", &distance );
	sscanf( pa_pvalue(3), "%f", &depth );
	compute_slowness = pa_qspecified( "-s" );
	quiet = pa_qspecified( "-q" );

	if  (compute_slowness)  {
		ttime = pt_slowness( phase, distance, depth, &status );
	} else {
		ttime = pt_travel( phase, distance, depth, &status );
	} /*endif*/
	if  (SySevere(&status))  {
		ttime = 0.0;
		if  (!quiet)
			err_writemsg( status, phase, FALSE );
	} /*endif*/
	printf( "%7.2f\n", ttime );

	return 0;

} /* end of main */

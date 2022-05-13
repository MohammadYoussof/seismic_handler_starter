
/* file slo2distance.c
 *      ==============
 *
 * version 2, 21-Nov-2005
 *
 * Fits distance to given slowness and phase.
 * K. Stammler, 1-Dec-96
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
	float    slowness;                   /* slowness in sec/deg */
	float    distance;                   /* distance in deg */
	float    depth;                      /* depth in km */

	/* executable code */

	pa_init( argc, argv );
	if  (pa_pnumber() != 3)  {
		fprintf( stderr, "Usage: %s <phase> <slowness> <depth>\n",
			pa_progname() );
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
	sscanf( pa_pvalue(2), "%f", &slowness );
	sscanf( pa_pvalue(3), "%f", &depth );

	distance = pt_distance( phase, slowness, depth, &status );
	if  (SySevere(&status))  {
		distance = 0.0;
		err_writemsg( status, phase, FALSE );
	} /*endif*/
	printf( "%7.2f\n", distance );

	return 0;

} /* end of main */

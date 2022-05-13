
/* file getresid.c
 *      ==========
 *
 * version 2, 27-Feb-2001
 *
 * Returns interpolated residual times from residual tables.
 * K. Stammler, 5-Feb-2001
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
#include "residual.h"


/* #define DEBUG */



int main( int argc, char *argv[] )
{
	/* local variables */
	char     fname[cBcFileLth+1];      /* name of residual file */
	char     station[cBcLineLth+1];    /* station name */
	char     phase[cBcLineLth+1];      /* phase name */
	float    slowness;                 /* slowness in sec/deg */
	float    azimuth;                  /* back-azimuth in deg */
	TSyStatus status;                  /* return status */

	/* executable code */

	/* get parameters */
	if  (argc != 5 && argc != 6)  {
		fprintf( stderr, "Usage: %s <station> <phase> <slowness> <azimuth> [<file>]\n",
			argv[0] );
		return 1;
	} /*endif*/
	strcpy( station, argv[1] );
	strcpy( phase, argv[2] );
	sscanf( argv[3], "%f", &slowness );
	sscanf( argv[4], "%f", &azimuth );
	if  (argc == 6)  {
		strcpy( fname, argv[5] );
	} else {
		*fname = '\0';
	} /*endif*/

	status = cBcNoError;
	RsReadTables( fname, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );

#	ifdef DEBUG

	RsDumpTables();
	printf( "station %s  slo %4.2f  azim %5.1f  resid: %5.2f\n", station,
		slowness, azimuth, RsResidual(station,phase,slowness,azimuth,&status));
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );
	return 0;

#	else

	printf( "%5.2f\n", RsResidual(station,phase,slowness,azimuth,&status) );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );
	return 0;

#	endif

} /* end of main */



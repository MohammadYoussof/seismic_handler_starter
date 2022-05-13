
/* file gsetest.c
 *      =========
 *
 * version 1, 8-Dec-94
 *
 * test module for gsedata
 * K. Stammler, 8-Dec-94
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
#include BC_SYSBASE
#include "gsedata.h"


int main( void )
{
	/* local variables */
	FILE     *fp;           /* pointer to file */
	SGseTrace gsetrc;
	TSyStatus status;

	/* executable code */

	status = cBcNoError;

	fp = fopen( "/tmp01/klaus/2066.data", "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "error opening file\n" );
		return 1;
	} /*endif*/

	status = cBcNoError;
	GseReadTrace( fp, &gsetrc, &status );

	fclose( fp );
	return 0;

} /* end of main */

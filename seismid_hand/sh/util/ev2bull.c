
/* file ev2bull.c
 *      =========
 *
 * version 2, 11-Aug-93
 *
 * reformats event to bulletin input format
 * K. Stammler, 28-Jul-93
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
#include "eventdsc.h"
#include "evbullet.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char      infile[BC_FILELTH+1];    /* input file */
	char      outfile[BC_FILELTH+1];   /* output file */
	char      header[BC_LINELTH+1];    /* header line */
	EvStatusT status;                  /* return status */

	/* executable code */

	if  (argc < 2 || argv[1][0] == EvEOS)  {
		printf( "   input filename: " );
		EvReadLineInteractive( stdin, BC_FILELTH, infile );
	} else {
		strcpy( infile, argv[1] );
	} /*endif*/

	if  (argc < 3 || argv[2][0] == EvEOS)  {
		printf( "   output filename: " );
		EvReadLineInteractive( stdin, BC_FILELTH, outfile );
	} else {
		strcpy( outfile, argv[2] );
	} /*endif*/

	if  (argc < 4 || argv[3][0] == EvEOS)  {
		printf( "   bulletin header line: " );
		EvReadLineInteractive( stdin, BC_LINELTH, header );
	} else {
		strcpy( infile, argv[3] );
	} /*endif*/

	EfSetBulletinHeader( header );
	status = EveNOERROR;
	EvReformatEventfile( infile, outfile, EvGetEvent,
		EfPutBulletin, &status );

#	ifndef BC_VAX
	return 0;
#	endif

} /* end of main */


/* file logseries.c
 *      ===========
 *
 * version 1, 4-Nov-98
 *
 * Returns series of numbers between a and b multiplied by c
 *
 * K. Stammler, 4-Nov-98
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


int main( int argc, char *argv[] )
{
	/* local variables */
	float    a, b, c, x;

	/* executable code */

	/* get parameters */
	if  (argc != 4)  {
		fprintf( stderr, "Usage: %s <from> <to> <mult>\n", argv[0] );
		return 1;
	} /*endif*/

	sscanf( argv[1], "%f", &a );
	sscanf( argv[2], "%f", &b );
	sscanf( argv[3], "%f", &c );

	x = a;
	while  (x <= b)  {
		printf( "%e\n", x );
		x *= c;
	} /*endwhile*/

	return 0;

} /* end of main */

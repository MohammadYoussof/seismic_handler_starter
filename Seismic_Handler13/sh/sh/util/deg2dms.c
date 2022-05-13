
/* file deg2dms.c
 *      =========
 *
 * version 1, 14-Dec-2005
 *
 * Converts degrees to deg,min,sec
 * K. Stammler, 14-Dec-2005
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
	float    inp;         /* input number (deg) */
	float    tmp;         /* scratch */
	int      d, m;        /* degrees and minutes */

	/* executable code */

	if  (argc != 2)  {
		fprintf( stderr, "Usage: $0 <inpdeg>\n", argv[0] );
		return 1;
	} /*endif*/

	sscanf( argv[1], "%f", &inp );

	d = (int)inp;
	tmp = (inp - (float)d) * 60.0;
	m = (int)tmp;
	tmp = (tmp - (float)m) * 60.0;

	printf( "%d %d %f (%f)\n", d, m, tmp, inp );

} /* end of main */

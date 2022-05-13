
/* file fit_to_line.c
 *      =============
 *
 * version 3, 21-Nov-2005
 *
 * Fits input data points to line.  Reads x,y pairs from stdin.
 * K. Stammler, 31-Mar-2003
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
#include <math.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "numres.h"

#define MAXNUM 500

/* global variables */
float flv_x[MAXNUM];      /* x coordinates */
float flv_y[MAXNUM];      /* y coordinates */
float flv_sig[MAXNUM];    /* std deviations (not used) */


int main( int argc, char *argv[] )
{
	/* local variables */
	int      cnt;               /* point counter */
	int      i;                 /* running index */
	int      mincnt;            /* minimum number of phases */
	float    offset, slope;     /* parameters of the fitted line */
	float    sigoff, sigslope;  /* std deviations of result */
	float    chi2, q;           /* not used */

	/* executable code */

	mincnt = 2;
	if  (argc > 1)  {
		sscanf( argv[1], "%d", &mincnt );
	} /*endif*/

	cnt = 0;
	while  (!feof(stdin))  {
		if  (cnt == MAXNUM)  {
			fprintf( stderr, "fit_to_line:  too many points.  Abort.\n" );
			exit( 1 );
		} /*endif*/
		if  (fscanf( stdin, "%f %f\n", flv_x+cnt, flv_y+cnt ) != 2)  {
			fprintf( stderr, "fit_to_line: error reading numbers from stdin.\n" );
			exit( 1 );
		} /*endif*/
		flv_sig[cnt] = 1.0;
		cnt++;
	} /*endwhile*/

	if  (cnt < mincnt)  return;

	nr_fit( flv_x-1, flv_y-1, cnt, flv_sig-1, 0, &offset, &slope, &sigoff,
		&sigslope, &chi2, &q );

#ifdef XXX
	for  (i=0; i<cnt; i++)
		printf( "%f %f\n", flv_x[i], flv_y[i] );
#endif

	printf( "%g %g %g %g\n", offset, slope, sigoff, sigslope );

	return 0;

} /* end of main */

/* file simrec.c
 *      ========
 *
 * version 2, 24-Jul-92
 *
 * recursive simulation filter of the seismometer-galvanometer
 * system (h1,w1,h2,w2) recorded at a seismometer (h0,w0)
 * K. Stammler, 2-Jun-92
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
#include BASECNST
#include BC_SYSBASE



int main( int argc, char *argv[] )
{
	/* local variables */
	float    h0, t0, w0;           /* recording seismometer */
	float    h1, t1, w1;           /* simulated seismometer */
	float    h2, t2, w2;           /* simulated galvanometer */
	float    dt;                   /* sample distance in sec */
	char     fname[BC_FILELTH+1];  /* output file */
	float    a[6];                 /* numerator coefficients */
	float    b[6];                 /* denominator coefficients */
	int      na, nb;               /* number of coefficients */
	int      i;                    /* counter */
	FILE     *fp;                  /* file pointer */
	float    w02, w12, w22;        /* squares */
	float    h0w0, h1w1, h2w2;     /* products */

	/* executable code */

	if  (argc != 9)  {
		printf( "\n" );
		printf( "Creates recursive simulation filter.  The filter removes\n" );
		printf( "a seismometer <h0>, <w0>=2*pi/<T0> and applies a\n" );
		printf( "transfer function of a seismometer-galvanometer system\n" );
		printf( "<h1>,<T1>,<h2>,<T2>.  The output filter is written\n" );
		printf( "to file <file> and is valid for traces with the\n" );
		printf( "sample distance <dt> sec.\n\n" );
		printf( "*** Usage: simrec <h0> <T0> <h1> <T1> <h2> <T2> <dt> <file> ***\n" );
		return 0;
	} /*endif*/

	if  (sscanf( argv[1], "%f", &h0 ) != 1)  {
		printf( "*** couldn't read h0 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( argv[2], "%f", &t0) != 1)  {
		printf( "*** couldn't read T0 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( argv[3], "%f", &h1 ) != 1)  {
		printf( "*** couldn't read h1 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( argv[4], "%f", &t1) != 1)  {
		printf( "*** couldn't read T1 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( argv[5], "%f", &h2 ) != 1)  {
		printf( "*** couldn't read h2 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( argv[6], "%f", &t2) != 1)  {
		printf( "*** couldn't read T2 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( argv[7], "%f", &dt) != 1)  {
		printf( "*** couldn't read dt ***\n" );
		return 0;
	} /*endif*/
	strcpy( fname, argv[8] );
	strcat( fname, ".FLR" );
	w0 = 2.0*BC_PI/t0;
	w1 = 2.0*BC_PI/t1;
	w2 = 2.0*BC_PI/t2;

	/* prewarp frequencies */
	w0 = tan( dt*w0/2.0 );
	w1 = tan( dt*w1/2.0 );
	w2 = tan( dt*w2/2.0 );

	/* some often used values */
	w02 = w0*w0;
	w12 = w1*w1;
	w22 = w2*w2;
	h0w0 = h0*w0;
	h1w1 = h1*w1;
	h2w2 = h2*w2;

	fp = sy_fopen( fname, "w" );
	if  (fp == NULL)  {
		printf( "*** couldn't open output file %s ***\n", fname );
		return 0;
	} /*endif*/

	fprintf( fp, "! recursive filter file of a simulation filter\n" );
	fprintf( fp, "! removes seismometer h0,T0 and applies transfer\n" );
	fprintf( fp, "! function (h1,T1,h2,T2)\n" );
	fprintf( fp, "! h0: %f\n", h0 );
	fprintf( fp, "! T0: %f\n", t0 );
	fprintf( fp, "! h1: %f\n", h1 );
	fprintf( fp, "! T1: %f\n", t1 );
	fprintf( fp, "! h2: %f\n", h2 );
	fprintf( fp, "! T2: %f\n", t2 );
	fprintf( fp, "!\n" );
	fprintf( fp, "1357913578\n" );  /* filter ID */
	fprintf( fp, "3\n" );           /* rec ID */
	fprintf( fp, "%e\n", dt );      /* sample distance */
	fprintf( fp, "%e\n", 1.0 );     /* normalization */

#ifdef XXX
   /* computed by Riemann2 */
	na = 4;
	a[0] = 1.0 + 2.0*h0w0 + w02;
	a[1] = -1.0 + 2.0*h0w0 + 3.0*w02;
	a[2] = -1.0 - 2.0*h0w0 + 3.0*w02;
	a[3] = 1.0 - 2.0*h0w0 + w02;

	nb = 4;
	b[0] = 1.0 + 2.0*h1w1 + w12;
	b[1] = -1.0 + 2.0*h1w1 + 3.0*w12;
	b[2] = -1.0 - 2.0*h1w1 + 3.0*w12;
	b[3] = 1.0 - 2.0*h1w1 + w12;
#endif

	na = 3;
	a[0] = w02 + 2.0*h0w0 + 1.0;
	a[1] = 2.0*(w02 - 1);
	a[2] = w02 - 2.0*h0w0 + 1.0;

	nb = 3;
	b[0] = w12 + 2.0*h1w1 + 1.0;
	b[1] = 2.0*(w12 - 1.0);
	b[2] = w12 - 2.0*h1w1 + 1.0;

	fprintf( fp, "%d\n", na );
	for  (i=0; i<na; fprintf(fp,"%e\n",a[i++])) {}
	fprintf( fp, "%d\n", nb );
	for  (i=0; i<nb; fprintf(fp,"%e\n",b[i++])) {}
	fprintf( fp, "@\n" );
	fprintf( fp, "3\n" );
	fprintf( fp, "%e\n", dt );      /* sample distance */
	fprintf( fp, "%e\n", w22 );     /* normalization */ /*!!!*/

#ifdef XXX
   /* computed by Riemann2 */
	na = 4;
	a[0] = w22;
	a[1] = 3.0*w22;
	a[2] = a[1];
	a[3] = a[0];

	nb = 4;
	b[0] = 1.0 + 2.0*h2w2 + w22;
	b[1] = -1.0 + 2.0*h2w2 + 3.0*w22;
	b[2] = -1.0 - 2.0*h2w2 + 3.0*w22;
	b[3] = 1.0 - 2.0*h2w2 + w22;
#endif

	na = 3;
	a[0] = 1.0;
	a[1] = 2.0;
	a[2] = 1.0;

	nb = 3;
	b[0] = w22 + 2.0*h2w2 + 1.0;
	b[1] = 2.0*(w22 - 1.0);
	b[2] = w22 - 2.0*h2w2 + 1.0;

	fprintf( fp, "%d\n", na );
	for  (i=0; i<na; fprintf(fp,"%e\n",a[i++])) {}
	fprintf( fp, "%d\n", nb );
	for  (i=0; i<nb; fprintf(fp,"%e\n",b[i++])) {}

	sy_fclose( fp );
	return 0;

} /* end of main */

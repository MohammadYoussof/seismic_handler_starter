
/* file restrec.c
 *      =========
 *
 * version 3, 25-Aug-94
 *
 * computes recursive filter coefficients for a restitution filter
 * The filter removes the seismometer h0, w0 and applies a
 * Butterworth highpass of order 3 with the corner freqency wc
 * K. Stammler, 31-May-92
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
	float    h0;                   /* seismomemter damping constant */
	float    t0, w0;               /* seismometer period, frq */
	float    tc, wc;               /* corner frequency of highpass */
	float    wc2, wc3;             /* powers of wc */
	float    dt;                   /* sample distance in sec */
	char     fname[BC_FILELTH+1];  /* output file */
	float    a[5];                 /* numerator coefficients */
	float    b[5];                 /* denominator coefficients */
	int      na, nb;               /* number of coefficients */
	int      i;                    /* counter */
	FILE     *fp;                  /* file pointer */

	/* executable code */

	if  (argc != 6)  {
		printf( "\n" );
		printf( "Creates recursive restitution filter.  The filter removes\n" );
		printf( "a seismometer <h0>, <w0>=2*pi/<T0> and applies a\n" );
		printf( "Butterworth highpass of order 3 with the corner\n" );
		printf( "frequency <fc>=1/<Tc>.  The output filter is written\n" );
		printf( "to file <file> and is valid for traces with the\n" );
		printf( "sample distance <dt> sec.\n\n" );
		printf( "*** Usage: restrec <h0> <T0> <Tc> <dt> <file> ***\n" );
		return 0;
	} /*endif*/

	if  (sscanf( argv[1], "%f", &h0 ) != 1)  {
		printf( "*** couldn't read h0 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( argv[2], "%f", &t0) != 1)  {
		printf( "*** couldn't read t0 ***\n" );
		return 0;
	} /*endif*/
	w0 = 2.0*BC_PI/t0;
	if  (sscanf( argv[3], "%f", &tc) != 1)  {
		printf( "*** couldn't read tc ***\n" );
		return 0;
	} /*endif*/
	wc = 2.0*BC_PI/tc;
	if  (sscanf( argv[4], "%f", &dt) != 1)  {
		printf( "*** couldn't read dt ***\n" );
		return 0;
	} /*endif*/
	strcpy( fname, argv[5] );
	strcat( fname, ".FLR" );

	/* prewarp frequencies */
	w0 = tan( dt*w0/2.0 );
	wc = tan( dt*wc/2.0 );

	wc2 = wc*wc;
	wc3 = wc2*wc;

#ifdef XXX
	na = 5;
	a[0] = 1.0 + 2.0*h0*w0 + w0*w0;
	a[1] = 4.0*w0*(h0+w0);
	a[2] = 6.0*w0*w0 - 2.0;
	a[3] = 4.0*w0*(w0-h0);
	a[4] = 1.0 - 2.0*h0*w0 + w0*w0;

	nb = 5;
	b[0] = 1.0 + 2.0*wc + 2.0*wc2 + wc3;
	b[1] = -2.0 + 4.0*wc2 + 4.0*wc3;
	b[2] = -4.0*wc + 6.0*wc3;
	b[3] = 2.0 - 4.0*wc2 + 4.0*wc3;
	b[4] = -1.0 + 2.0*wc - 2.0*wc2 + wc3;
#endif

	na = 4;
	a[0] =  1.0 + 2.0*h0*w0 + w0*w0;
	a[1] = -1.0 + 2.0*h0*w0 + 3.0*w0*w0;
	a[2] = -1.0 - 2.0*h0*w0 + 3.0*w0*w0;
	a[3] =  1.0 - 2.0*h0*w0 + w0*w0;

	nb = 4;
	b[0] =  1.0 + 2.0*wc + 2.0*wc2 + wc3;
	b[1] = -3.0 - 2.0*wc + 2.0*wc2 + 3.0*wc3;
	b[2] =  3.0 - 2.0*wc - 2.0*wc2 + 3.0*wc3;
	b[3] = -1.0 + 2.0*wc - 2.0*wc2 + wc3;

	fp = sy_fopen( fname, "w" );
	if  (fp == NULL)  {
		printf( "*** couldn't open output file %s ***\n", fname );
		return 0;
	} /*endif*/

	fprintf( fp, "! recursive filter file of a restitution filter\n" );
	fprintf( fp, "! removes seismometer h0, T0 and applies Butterworth\n" );
	fprintf( fp, "! highpass of order three at corner frequency 1/tc\n" );
	fprintf( fp, "! h0: %f\n", h0 );
	fprintf( fp, "! T0: %f\n", t0 );
	fprintf( fp, "! tc: %f\n", tc );
	fprintf( fp, "!\n" );
	fprintf( fp, "1357913578\n" );  /* filter ID */
	fprintf( fp, "3\n" );           /* rec ID */
	fprintf( fp, "%e\n", dt );      /* sample distance */
	fprintf( fp, "%e\n", dt/2.0 );  /* normalization */
	fprintf( fp, "%d\n", na );
	for  (i=0; i<na; fprintf(fp,"%e\n",a[i++])) {}
	fprintf( fp, "%d\n", nb );
	for  (i=0; i<nb; fprintf(fp,"%e\n",b[i++])) {}

	sy_fclose( fp );
	return 0;

} /* end of main */

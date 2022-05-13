/* file simfreq.c
 *      =========
 *
 * version 1, 24-Jul-92
 *
 * FFT simulation filter of the seismometer-galvanometer
 * system (h1,w1,h2,w2) recorded at a seismometer (h0,w0)
 * K. Stammler, 24-Jul-92
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
#include BC_CPAR



/* global type */
typedef struct {
	float    re;    /* real part */
	float    im;    /* imaginary part */
} COMPLEX;

#define MAXDEGREE 30



/* prototypes of local routines */
void poly_z( float h, float w, COMPLEX *s1, COMPLEX *s2 );



int main( int argc, char *argv[] )
{
	/* local variables */
	float    h0, t0, w0;           /* recording seismometer */
	float    h1, t1, w1;           /* simulated seismometer */
	float    h2, t2, w2;           /* simulated galvanometer */
	float    norm;                 /* normalization */
	char     fname[BC_FILELTH+1];  /* output file */
	int      i;                    /* counter */
	FILE     *fp;                  /* file pointer */
	int      no_of_zeroes;         /* number of zeroes */
	int      no_of_poles;          /* number of poles */
	COMPLEX  zero[MAXDEGREE];      /* zeroes */
	COMPLEX  pole[MAXDEGREE];      /* poles */

	/* executable code */

	pa_init( argc, argv );
	if  (pa_pnumber() != 7)  {
		printf( "\n" );
		printf( "Creates FFT simulation filter.  The filter removes\n" );
		printf( "a seismometer <h0>, <w0>=2*pi/<T0> and applies a\n" );
		printf( "transfer function of a seismometer-galvanometer system\n" );
		printf( "<h1>,<T1>,<h2>,<T2>.  The output filter is written\n" );
		printf( "to file <file> (specify without extension)\n\n" );
		printf( "*** Usage: simfreq <h0> <T0> <h1> <T1> <h2> <T2> <file> ***\n" );
		return 0;
	} /*endif*/

	/* get parameters */
	if  (sscanf( pa_pvalue(1), "%f", &h0 ) != 1)  {
		printf( "*** couldn't read h0 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( pa_pvalue(2), "%f", &t0 ) != 1)  {
		printf( "*** couldn't read t0 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( pa_pvalue(3), "%f", &h1 ) != 1)  {
		printf( "*** couldn't read h0 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( pa_pvalue(4), "%f", &t1 ) != 1)  {
		printf( "*** couldn't read h0 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( pa_pvalue(5), "%f", &h2 ) != 1)  {
		printf( "*** couldn't read h0 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( pa_pvalue(6), "%f", &t2 ) != 1)  {
		printf( "*** couldn't read h0 ***\n" );
		return 0;
	} /*endif*/
	strncpy( fname, pa_pvalue(7), BC_FILELTH-4 );
	strcat( fname, ".FLF" );

	w0 = 2.0*BC_PI/t0;
	w1 = 2.0*BC_PI/t1;
	w2 = 2.0*BC_PI/t2;
	norm = w2*w2;

	no_of_zeroes = 2;
	poly_z( h0, w0, zero, zero+1 );
	no_of_poles = 4;
	poly_z( h1, w1, pole, pole+1 );
	poly_z( h2, w2, pole+2, pole+3 );

	/* write filter file */
	fp = fopen( fname, "w" );
	fprintf( fp, "! simulation filter for\n" );
	fprintf( fp, "! seismometer  h1 (%f), T1 (%f)\n", h1, t1 );
	fprintf( fp, "! galvanometer h2 (%f), T2 (%f)\n", h2, t2 );
	fprintf( fp, "! recorded at instrument\n" );
	fprintf( fp, "! seismometer  h0 (%f), T0 (%f)\n", h0, t0 );
	fprintf( fp, "%ld\n", 1357913578L );  /* magic longword */
	fprintf( fp, "%d\n", 1 );             /* storage ID */
	fprintf( fp, "%e\n", norm );          /* normalisation */
	fprintf( fp, "%d\n", no_of_zeroes );
	for  (i=0; i<no_of_zeroes; i++)
		fprintf( fp, "(%e,%e)\n", zero[i].re, zero[i].im );
	fprintf( fp, "%d\n", no_of_poles );
	for  (i=0; i<no_of_poles; i++)
		fprintf( fp, "(%e,%e)\n", pole[i].re, pole[i].im );
	fclose( fp );
	return 0;

} /* end of main */



/*-----------------------------------------------------------------------------*/



void poly_z( float h, float w, COMPLEX *s1, COMPLEX *s2 )

/* Berechnet die Nullstellen des Polynoms  s**2 + 2*h*w*s + w**2
 * nach der Formel
 * NS = -w * (h +- sqrt(h**2-1))
 *
 * parameters of routine
 * real          h       ! Input; (Daempfung)
 * real          w       ! Input; (Eigenfrequenz)
 * FCOMPLEX      *s1     ! Output; 1. Nullstelle
 * FCOMPLEX      *s2     ! Output; 2. Nullstelle
 */
{
	/* local variables */
	float    discrim;       /* Diskriminante */

	/* executable code */

	discrim = h*h - 1.0;

	if  (discrim < 0.0)  {  /* komplexe Nullstellen */
		s1->re = 0.0;
		s1->im = sqrt(-discrim);
		s2->re = 0.0;
		s2->im = -s1->im;
	} else {               /* reelle NS */
		s1->re = sqrt(discrim);
		s1->im = 0.0;
		s2->re = -s1->re;
		s2->im = 0.0;
	} /*endif*/

	s1->re += h;
	s2->re += h;
	s1->re *= -w;
	s1->im *= -w;
	s2->re *= -w;
	s2->im *= -w;

} /* end of poly_z */



/*-----------------------------------------------------------------------------*/

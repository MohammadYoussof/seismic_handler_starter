
/* file restfreq.c
 *      ==========
 *
 * version 2, 25-Aug-94
 *
 * creates restitution filter for seismometer h0,T0
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
#ifdef BC_STDLIB_EX
#include <stdlib.h>
#endif /* BC_STDLIB_EX */
#include BC_CPAR



/* global type */
typedef struct {
	float    re;    /* real part */
	float    im;    /* imaginary part */
} COMPLEX;

#define MAXDEGREE 30



/* prototypes of local routines */
void get_hp_lp_poles( float wc, int n, int max_poles, COMPLEX pole[],
	int *no_of_poles );
void ep( int k, int n, COMPLEX *res );
void poly_z( float h, float w, COMPLEX *s1, COMPLEX *s2 );



int main( int argc, char *argv[] )
{
	/* local variables */
	float    h0, t0, w0;           /* seismometer */
	float    tc, wc;               /* corner period & and ang. freq. */
	int      order;                /* order of highpass */
	char     fname[BC_FILELTH+1];  /* name of output file */
	FILE     *fp;                  /* pointer to filter file */
	int      no_of_zeroes;         /* number of zeroes */
	int      no_of_poles;          /* number of poles */
	COMPLEX  zero[MAXDEGREE];      /* zeroes */
	COMPLEX  pole[MAXDEGREE];      /* poles */
	int      i;                    /* counter */

	/* executable code */

	pa_init( argc, argv );
	if  (pa_pnumber() != 5)  {
		printf( "\n" );
		printf( "Creates FFT restitution filter.  The filter removes\n" );
		printf( "a seismometer <h0>, <w0>=2*pi/<T0> and applies a\n" );
		printf( "Butterworth highpass of order <order> with the corner\n" );
		printf( "frequency <fc>=1/<Tc>.  The output filter is written\n" );
		printf( "to file <file> (specify without extension)\n" );
		printf( "*** Usage: restfreq <h0> <T0> <Tc> <order> <file> ***\n" );
		return 0;
	} /*endif*/

	/* read parameters */
	if  (sscanf( pa_pvalue(1), "%f", &h0 ) != 1)  {
		printf( "*** couldn't read h0 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( pa_pvalue(2), "%f", &t0 ) != 1)  {
		printf( "*** couldn't read t0 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( pa_pvalue(3), "%f", &tc ) != 1)  {
		printf( "*** couldn't read tc ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( pa_pvalue(4), "%d", &order ) != 1)  {
		printf( "*** couldn't read order ***\n" );
		return 0;
	} /*endif*/
	strncpy( fname, pa_pvalue(5), BC_FILELTH-4 );
	strcat( fname, ".FLF" );

	wc = 2.0*BC_PI/tc;
	w0 = 2.0*BC_PI/t0;

	if  (order < 3)  {
		printf( "*** order too small (must be greater or equal to 3) ***\n" );
		return 0;
	} /*endif*/

	/* get zeroes */
	no_of_zeroes = order-1;
	poly_z( h0, w0, zero, zero+1 );
	for  (i=2; i<no_of_zeroes; i++)  {
		zero[i].re = 0.;
		zero[i].im = 0.;
	} /*endfor*/
	/* get poles */
	get_hp_lp_poles( wc, order, MAXDEGREE, pole, &no_of_poles );

	/* write filter file */
	fp = fopen( fname, "w" );
	fprintf( fp, "! restitution filter for seismometer h0 (%f) and\n", h0 );
	fprintf( fp, "! T0 (%f sec).  Applied highpass has corner period\n", t0 );
	fprintf( fp, "! %f sec and order %d\n", tc, order );
	fprintf( fp, "%ld\n", 1357913578L );  /* magic longword */
	fprintf( fp, "%d\n", 1 );             /* storage ID */
	fprintf( fp, "%e\n", 1.0 );           /* normalisation */
	fprintf( fp, "%d\n", no_of_zeroes );
	for  (i=0; i<no_of_zeroes; i++)
		fprintf( fp, "(%e,%e)\n", zero[i].re, zero[i].im );
	fprintf( fp, "%d\n", no_of_poles );
	for  (i=0; i<no_of_poles; i++)
		fprintf( fp, "(%e,%e)\n", pole[i].re, pole[i].im );
	fclose( fp );
	return 0;

} /* end of main */



/*--------------------------------------------------------------------------*/



void get_hp_lp_poles( float wc, int n, int max_poles, COMPLEX pole[],
	int *no_of_poles )

/* computes poles of butterworth low/highpass of order n
 *
 * parameters of routine
 * float      wc;          input; corner frequency
 * int        n;           input; order of filter
 * int        max_poles;   input; maximum length of pole array
 * COMPLEX    pole[]       output; poles
 * int        no_of_poles; output; number of poles
 */
{
	/* local variables */
	int      k;            /* counter */

	/* executable code */

	if  (n > max_poles)  {
		printf( "*** too many poles ***\n" );
		exit( 0 );
	} /*endif*/
	*no_of_poles = n;

	for  (k=0; k<n; k++)  {
		ep( k+1, n, pole+k ); /* pole(k) = wc * ep(k,n) */
		pole[k].re *= wc;
		pole[k].im *= wc;
	} /*endfor*/

} /* end of get_hp_lp_poles */



/*--------------------------------------------------------------------------*/



void ep( int k, int n, COMPLEX *res )

/* returns E(k,N) from above formula in "res"
 *
 * parameters of routine
 * int        k       input; 1 <= k <= n
 * int        n       input; order
 */
{
	/* local variables */
	float    arc;     /* angle */

	/* executable code */

	/* degrees = (90./float(n)) * float(2*k-1) */
	arc = BC_PI*0.5/(float)n * (float)(2*k-1);
	res->re = -sin( arc );
	res->im = cos( arc );

} /* end of ep */



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



/*--------------------------------------------------------------------------*/


/* file BUTFREQ.C
 *      =========
 *
 * version 1, 24-Jul-92
 *
 * creates Butterworth filter file
 * same as but_freq, but different user interface
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

/*
 *     The transfer function of all Butterworth filter of order N
 *     consists of factors of the form:
 *        F(s,k,N)  =  1  /  ( T(s) - E(k,N) ) ;   k = 1,..,N
 *
 *        H(s)  =  F(s,1,N) * F(s,2,N) * .. * F(s,N,N)
 *
 *     where
 *
 *        E(k,N)  =  i * exp( i*pi*(2k-1) / (2*N) ) ;  k = 1,..,N
 *
 *     and
 *
 *        T(s)  =  s / wc                       lowpass
 *        T(s)  =  wc / s                       highpass
 *        T(s)  =  (s*s + wl*wu) / (s*(wu-wl))  bandpass
 *        T(s)  =  (s*(wu-wl)) / (s*s + wl*wu)  bandstop
 *
 *     The program computes poles p and zeroes n of the transfer function
 *     using the equations
 *
 *        T(p)  =  E(k,N)
 *        denominator( T(n) ) = 0
 *
 *     From these result the final equations
 *
 *     N poles of LP and HP
 *        p  =  wc * E(k,N)
 *
 *     2N poles of BP and BS
 *        p  =  1/2 * ( E(k,N)*(wu-wl) +- sqrt( radicant ) )
 *           where  radicant  =  E(k,N)**2 * (wu-wl)**2 - 4*wu*wl
 *
 *     N zeroes of HP and BP
 *        n  =  0
 *
 *     2N zeroes of BS
 *        n  =  +-i * sqrt( wu*wl )
 *
 *     LP has no zeroes
 *
 *     The computed poles and zeroes are stored in a filter file.
 *
 *     The normalisation constant G results in
 *
 *        LP:  G = wc**N
 *        HP:  G = 1
 *        BP:  G = (wu - wl)**N
 *        BS:  G = 1
 */


#include <stdio.h>
#include <string.h>
#include <math.h>
#include BASECNST
#ifdef BC_STDLIB_EX
#include <stdlib.h>
#endif /* BC_STDLIB_EX */
#include BC_SYSBASE
#include BC_CPAR


/* global type */
typedef struct {
	float    re;    /* real part */
	float    im;    /* imaginary part */
} COMPLEX;


/* prototypes of local routines */
void read_freq( char str[], char inp_kind, float *freq );
void get_hp_lp_poles( float wc, int n, int max_poles, COMPLEX pole[],
	int *no_of_poles );
void get_bp_bs_poles( float wl, float wu, int n, int max_poles,
	COMPLEX pole[], int *no_of_poles );
void ep( int k, int n, COMPLEX *res );
void ep2( int k, int n, COMPLEX *res );
float intpow( float base, int exp );


int main( int argc, char *argv[] )
{

	/* constant */
#	define MAX_POLES 50
		/* maximum number of poles */

	/* local variables */
	char     f_type[BC_LINELTH+1]; /* type of filter: LP,HP,BP,BS */
	char     inp_kind;             /* kind of input: w, f, t */
	COMPLEX  pole[MAX_POLES];      /* poles of transfer function */
	int      no_of_poles;          /* number of poles */
	float    wu, wl;               /* upper and lower corner freq. */
	int      order;                /* order of filter */
	COMPLEX  zero[MAX_POLES];      /* zeroes of transfer function */
	int      no_of_zeroes;         /* number of zeroes */
	int      i;                    /* counter */
	float    sq;                   /* scratch */
	float    norm_const;           /* normalisation */
	char     filename[BC_FILELTH+1];/* Name of filter file */
	char     info1[BC_LINELTH+1];  /* first info line */
	char     info2[BC_LINELTH+1];  /* second info line */
	FILE     *fp;                  /* filter file pointer */


	/* executable code */

	pa_init( argc, argv );
	if  (pa_pnumber() < 3)  {
		printf( "*** Usage: butfreq <ftype> <itype> <c1> [<c2>] ***\n" );
		printf( "    Qualifiers: -o=<order> -f=<filterfile>\n" );
		return 0;
	} /*endif*/

	strncpy( f_type, pa_pvalue(2), 1 );
	inp_kind = Cap( *f_type );
	strncpy( f_type, pa_pvalue(1), 2 );
	f_type[0] = Cap( f_type[0] );
	f_type[1] = Cap( f_type[1] );
	f_type[2] = '\0';

   if  (strcmp(f_type,"HP") == 0 || strcmp(f_type,"LP") == 0)  {
		read_freq( pa_pvalue(3), inp_kind, &wl );
		sprintf( info2, "! corner frequency %e Hz", wl/(2.*BC_PI) );
	} else if  (strcmp(f_type,"BP") == 0 || strcmp(f_type,"BS") == 0)  {
		read_freq( pa_pvalue(3), inp_kind, &wl );
		if  (pa_pnumber() != 4)  {
			printf( "*** no upper frequency bound specified ***\n" );
			return 0;
		} /*endif*/
		read_freq( pa_pvalue(4), inp_kind, &wu );
		if  (wu < wl)  {  /* swap wu,wl */
			sq = wu;
			wu = wl;
			wl = sq;
		} /*endif*/
		sprintf( info2, "! lower bound %e Hz,  upper bound %e Hz",
			wl/(2.0*BC_PI), wu/(2.0*BC_PI) );
	} /*endif*/

	/* get order & output filter file */
	if  (pa_qspecified("-o"))  {
		sscanf( pa_qvalue("-o"), "%d", &order );
	} else {
		order = 3;
	} /*endif*/
	if  (pa_qspecified("-f"))  {
		strncpy( filename, pa_qvalue("-f"), BC_LINELTH );
	} else {
		strcpy( filename, "filter" );
	} /*endif*/

	/* computation of poles */
	if  (strcmp(f_type,"HP") == 0)  {
		get_hp_lp_poles( wl, order, MAX_POLES, pole, &no_of_poles );
		sprintf( info1, "! Butterworth Highpass of order %d", order );
	} else if  (strcmp(f_type,"LP") == 0)  {
		get_hp_lp_poles( wl, order, MAX_POLES, pole, &no_of_poles );
		sprintf( info1, "! Butterworth Lowpass of order %d", order );
	} else if  (strcmp(f_type,"BP") == 0)  {
		get_bp_bs_poles( wl, wu, order, MAX_POLES, pole, &no_of_poles );
		sprintf( info1, "! Butterworth Bandpass of order %d", order );
	} else if  (strcmp(f_type,"BS") == 0)  {
		get_bp_bs_poles( wl, wu, order, MAX_POLES, pole, &no_of_poles );
		sprintf( info1, "! Butterworth Bandstop of order %d", order );
	} else {
		printf( "*** wrong filter type ***\n" );
		return 0;
	} /*endif*/

	/* computation of zeroes and normalisation */
	for  (i=0; i<MAX_POLES; i++)  {
		zero[i].re = 0.0;
		zero[i].im = 0.0;
	} /*endfor*/
	if  (strcmp(f_type,"HP") == 0)  {
		no_of_zeroes = order;  /* n zeroes at s=0 */
		norm_const = 1.;
	} else if  (strcmp(f_type,"LP") == 0)  {
		no_of_zeroes = 0;      /* no zeroes */
		norm_const = intpow( wl, order );
	} else if  (strcmp(f_type,"BP") == 0)  {
		no_of_zeroes = order;  /* n zeroes at s=0 */
		norm_const = intpow( wu-wl, order );
	} else if  (strcmp(f_type,"BS") == 0)  {
		no_of_zeroes = 2*order;
		sq = sqrt( wl*wu );
		for  (i=0; i<order; i++)  {
			zero[2*i].re = 0.;
			zero[2*i].im = sq;
			zero[2*i+1].re = 0.;
			zero[2*i+1].im = -sq;
		} /*endfor*/
		norm_const = 1.;
	} /*endif*/

	/* write filter file */
	strcat( filename, ".FLF" );
	fp = fopen( filename, "w" );
	fprintf( fp, "%s\n", info1 );
	fprintf( fp, "%s\n", info2 );
	fprintf( fp, "%ld\n", 1357913578L );  /* magic longword */
	fprintf( fp, "%d\n", 1 );             /* storage ID */
	fprintf( fp, "%e\n", norm_const );    /* normalisation */
	fprintf( fp, "%d\n", no_of_zeroes );
	for  (i=0; i<no_of_zeroes; i++)
		fprintf( fp, "(%e,%e)\n", zero[i].re, zero[i].im );
	fprintf( fp, "%d\n", no_of_poles );
	for  (i=0; i<no_of_poles; i++)
		fprintf( fp, "(%e,%e)\n", pole[i].re, pole[i].im );
	fclose( fp );

	return 0;

} /* end of main */



/*---------------------------------------------------------------------------*/



void read_freq( char str[], char inp_kind, float *freq )

/* reads in frequency from string "str".  Output is circular frequency
 *
 * parameters of routine
 * char       inp_kind;       input; determines kind of input
 * float      freq;           output; circular frequency
 */
{
	/* executable code */

	sscanf( str, "%f", freq );
	if  (inp_kind == 'T')  {
		*freq = 2.*BC_PI / *freq;
	} else if  (inp_kind == 'F')  {
		*freq = 2.*BC_PI*(*freq);
	} else if  (inp_kind != 'W')  {
		printf( "*** wrong input type ***\n" );
		exit( 0 );
	} /*endif*/

} /* end of read_freq */



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



void get_bp_bs_poles( float wl, float wu, int n, int max_poles,
	COMPLEX pole[], int *no_of_poles )

/* computes poles of bandpass/stop filter of order n
 *
 * parameters of routine
 * float      wl;          input; lower corner frequency
 * float      wu;          input; upper corner frequency
 * int        n;           input; order
 * int        max_poles;   input; max length of pole array
 * COMPLEX    pole[];      output; poles
 * int        no_of_poles; output; number of poles
 */
{
	/* local variables */
	int      k;       /* counter */
	float    w_dist;  /* difference of corner frequencies */
	float    w_dist2; /* square of w_dist */
	float    wlwu4;   /* 4 * wl * wu */
	COMPLEX  root;    /* scratch */
	float    r, phi;  /* scratch */

	/* executable code */

	*no_of_poles = 2*n;
	if  (*no_of_poles > max_poles)  {
		printf( "*** too many poles ***\n" );
		exit( 0 );
	} /*endif*/

	w_dist = wu - wl;
	w_dist2 = w_dist * w_dist;
	wlwu4 = 4. * wu * wl;

	for  (k=0; k<n; k++)  {
		/* root = cmplx(w_dist2,0.) * ep2(k,n) - cmplx(wlwu4,0.) */
		ep2( k+1, n, &root );
		root.re *= w_dist2;
		root.im *= w_dist2;
		root.re -= wlwu4;
		/* root = csqrt( root ) */
		r = sqrt( root.re*root.re + root.im*root.im );
		phi = atan2( root.im, root.re );
		r = sqrt( r );
		phi /= 2.0;
		root.re = r * cos( phi );
		root.im = r * sin( phi );
		/* pole(2*k-1) = 0.5 * (ep(k,n)*cmplx(w_dist,0.) + root) */
		ep( k+1, n, pole+2*k );
		pole[2*k].re *= w_dist;
		pole[2*k].im *= w_dist;
		pole[2*k].re += root.re;
		pole[2*k].im += root.im;
		pole[2*k].re *= 0.5;
		pole[2*k].im *= 0.5;
		/* pole(2*k) = 0.5 * (ep(k,n)*cmplx(w_dist,0.) - root) */
		ep( k+1, n, pole+2*k+1 );
		pole[2*k+1].re *= w_dist;
		pole[2*k+1].im *= w_dist;
		pole[2*k+1].re -= root.re;
		pole[2*k+1].im -= root.im;
		pole[2*k+1].re *= 0.5;
		pole[2*k+1].im *= 0.5;
	} /*endfor*/

} /* end of get_bp_bs_poles */



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



/*--------------------------------------------------------------------------*/



void ep2( int k, int n, COMPLEX *res )

/* returns E(k,N)**2 from above formula in "res"
 *
 * parameters of routine
 * int        k       input; 1 <= k <= n
 * int        n       input; order
 */
{
	/* local variables */
	float    arc;      /* angle */

	/* executable code */

	arc = BC_PI/(float)n * (float)(2*k-1);
	res->re = -cos( arc );
	res->im = -sin( arc );

} /* end of ep2 */



/*--------------------------------------------------------------------------*/



float intpow( float base, int exp )

/* computes integer power of "base"
 *
 * parameters of routine
 * float      base       input; base
 * int        exp        input; exponent
 */
{
	/* local variables */
	int      i;         /* counter */
	float    ret;       /* return value */

	/* executable code */

	ret = 1.;
	for  (i=1; i<=exp; i++)
		ret *= base;

	return ret;

} /* end of intpow */



/*--------------------------------------------------------------------------*/

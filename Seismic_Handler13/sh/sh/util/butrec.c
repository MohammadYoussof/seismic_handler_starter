
/* file BUTREC.C
 *      =========
 *
 * version 1, 24-Jul-91
 *
 * creates recursive filter file for Butterworth HP, LP, BP and BS
 * filters
 * same as but_rec, only different user interface
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
 *
 *     Die z-Transformierten aller Butterworthfilter der Ordnung N setzen
 *     sich zusammen aus N Faktoren F(s',k,N) der Form
 *
 *        F(s',k,N)  =  1 / ( T(s') - E(k,N) ) ;  k = 1,..,N
 *
 *        H(z)  =  F(s',1,N) * F(s',2,N) * .. * F(s',N,N)
 *
 *     wobei
 *
 *        E(k,N)  =  i * exp( i*pi*(2*k-1) / (2*N) ) ;  k = 1,..,N
 *
 *     und
 *
 *        T(s')  =  s' / wc'                            Tiefpass
 *        T(s')  =  wc' / s'                            Hochpass
 *        T(s')  =  (s'*s' + wl'*wu') / (s'*(wu'-wl'))  Bandpass
 *        T(s')  =  (s'*(wu'-wl')) / (s'*s' + wu'*wl')  Bandsperre
 *
 *     mit
 *
 *                z - 1
 *        s'  =  -------
 *                z + 1
 *
 *        wc'  =  tan( (dt/2) wc )   verzerrte Eckfrequenz (Tief-/Hochpass)
 *        wl'  =  tan( (dt/2) wl )   verz. obere Eckfrequenz (Bandpass/-sperre)
 *        wu'  =  tan( (dt/2) wu )   verz. untere Eckfrequenz (Bandpass/-sperre)
 *
 *     Wegen E(N-k+1,N) = conjug( E(k,N) ) koennen in der Produktformel fuer
 *     H(z) immer zwei Faktoren zusammengefasst werden, so dass sich dafuer
 *     ein reeller Ausdruck ergibt. Daraus folgt:
 *
 *     N gerade:
 *
 *        H(z)  =  Q(z,1,N) * Q(z,2,N) * .. * Q(z,N/2,N)
 *
 *     N ungerade:
 *
 *        H(z)  =  L(z)  *  Q(z,1,N) * Q(z,2,N) * .. * Q(z,(N-1)/2,N)
 *
 *     mit
 *
 *        Q(z,k,N)  =  1  /  ( T(s')*T(s') - 2*Re(E(k,N))*T(s') + 1 )
 *
 *        L(z)  =  1  /  ( T(s') + 1 )
 *
 *     Durch Ausmultiplizieren der s'(z) entstehen fuer Q(z,k,N) und L(z)
 *     bei jeder Filterart (TP,HP,BP,BS) jeweils eine rationale Funktion
 *     der Form
 *
 *                      a0 + a1*z**(-1) + a2*z**(-2) + a3*z**(-3) + a4*z**(-4)
 *        Q(z,k,N)  =  --------------------------------------------------------
 *                      b0 + b1*z**(-1) + b2*z**(-2) + b3*z**(-3) + b4*z**(-4)
 *
 *        ai = ai(k,N); bi = bi(k,N)
 *
 *                  a0' + a1'*z**(-1) + a2'*z**(-2)
 *        L(z)  =  ---------------------------------
 *                  b0' + b1'*z**(-1) + b2'*z**(-2)
 *
 *     so dass jeder Filter der Ordnung N durch eine Kaskade von rekursiven
 *     L- und Q-Filtern dargestellt werden kann.
 *
 *     Dieses Programm berechnet fuer alle Filterarten (TP,HP,BP,BS) die
 *     Koeffizienten der Kaskade ai(k,N), bi(k,N), ai', bi' (k = 1,..,N)
 *     und speichert sie in einem Filter-File ab. Das Filter-File enthaelt
 *     also N/2 (fuer N gerade) oder (N+1)/2 (fuer N ungerade) einzelne Filter,
 *     die bei einem Filtervorgang alle nacheinander abgearbeitet werden
 *     muessen.
 *
 *     Beispiele:
 *
 *     N=1:  H(z)  =  L(z)
 *                    ^ einziges Filter im File
 *
 *     N=2:  H(z)  =  Q(z,1,2)
 *                    ^ einziges Filter im File
 *
 *     N=3:  H(z)  =  L(z) * Q(z,1,3)
 *                           ^ zweites Filter
 *                    ^ erstes Filter
 *
 *     N=4:  H(z)  =  Q(z,1,4) * Q(z,2,4)
 *                               ^ zweites Filter
 *                    ^ erstes Filter
 *
 *     N=5:  H(z)  =  L(z) * Q(z,1,5) * Q(z,2,5)
 *                                      ^ drittes Filter
 *                           ^ zweites Filter
 *                    ^ erstes Filter
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


/* prototypes of local routines */
void lp_l( float wc, float dt, float a[], float b[], int *lth, float *norm );
void lp_q( int k, int n, float wc, float dt, float a[], float b[],
	int *lth, float *norm );
void hp_l( float wc, float dt, float a[], float b[], int *lth, float *norm );
void hp_q( int k, int n, float wc, float dt, float a[], float b[],
	int *lth, float *norm );
void bp_l( float wl, float wu, float dt, float a[], float b[],
	int *lth, float *norm );
void bp_q( int k, int n, float wl, float wu, float dt,
	float a[], float b[], int *lth, float *norm );
void bs_l( float wl, float wu, float dt, float a[], float b[],
	int *lth, float *norm );
void bs_q( int k, int n, float wl, float wu, float dt,
	float a[], float b[], int *lth, float *norm );
void read_freq( char str[], char inp_kind, float *freq );
float real_ep( int k, int n );


int main( int argc, char *argv[] )
{
	/* local variables */
	char     f_type[BC_LINELTH+1];   /* type of filter (LP,HP,BP,BS) */
	char     inp_kind;               /* input type of frequencies */
	char     info1[BC_LINELTH+1];    /* comment line 1 */
	char     info2[BC_LINELTH+1];    /* comment line 2 */
   float    wl, wu;                 /* corner (angular) frequencies */
	float    sq;                     /* scratch */
	int      order;                  /* order of filter */
	float    delta_t;                /* sample distance in sec */
	char     filename[BC_FILELTH+1]; /* name of filter file */
	int      i, k;                   /* counter */
	int      max_k;                  /* maximum k */
	FILE     *fp;                    /* filter file pointer */

	float    a[5];                   /* numerator coefficients */
	float    b[5];                   /* denominator coefficients */
	int      coeff_no;               /* number of coefficients */
	float    norm;                   /* normalisation */

	/* executable code */

	pa_init( argc, argv );

	if  (pa_pnumber() < 4)  {
		printf( "*** Usage: butrec <dt> <ftype> <itype> <c1> [<c2>] ***\n" );
		printf( "    Qualifiers: -o=<order> -f=<filterfile>\n" );
		return 0;
	} /*endif*/

	sscanf( pa_pvalue(1), "%f", &delta_t );
	strncpy( f_type, pa_pvalue(3), 1 );
	inp_kind = Cap( *f_type );
	strncpy( f_type, pa_pvalue(2), 2 );
	f_type[0] = Cap( f_type[0] );
	f_type[1] = Cap( f_type[1] );
	f_type[2] = '\0';

	if  (strcmp(f_type,"HP") == 0 || strcmp(f_type,"LP") == 0)  {
		read_freq( pa_pvalue(4), inp_kind, &wl );
		sprintf( info2, "! corner frequency %e Hz", wl/(2.*BC_PI) );
	} else if  (strcmp(f_type,"BP") == 0 || strcmp(f_type,"BS") == 0)  {
		read_freq( pa_pvalue(4), inp_kind, &wl );
		if  (pa_pnumber() < 5)  {
			printf( "*** no upper frequency bound specified ***\n" );
			return 0;
		} /*endif*/
		read_freq( pa_pvalue(5), inp_kind, &wu );
		if  (wu < wl)  {  /* swap wu,wl */
			sq = wu;
			wu = wl;
			wl = sq;
		} /*endif*/
		sprintf( info2, "! lower bound %e Hz,  upper bound %e Hz",
			wl/(2.*BC_PI), wu/(2.*BC_PI) );
	} /*endif*/

	if  (pa_qspecified("-o"))  {
		sscanf( pa_qvalue("-o"), "%d", &order );
	} else {
		order = 3;
	} /*endif*/
	if  (pa_qspecified("-f"))  {
		strncpy( filename, pa_qvalue("-f"), BC_FILELTH );
	} else {
		strcpy( filename, "filter" );
	} /*endif*/

	/* create first info line */
	if  (strcmp(f_type,"LP") == 0)  {
		sprintf( info1, "! butterworth low-pass of order %d", order );
	} else if  (strcmp(f_type,"HP") == 0)  {
		sprintf( info1, "! butterworth high-pass of order %d", order );
	} else if  (strcmp(f_type,"BP") == 0)  {
		sprintf( info1, "! butterworth band-pass of order %d", order );
	} else if  (strcmp(f_type,"BS") == 0)  {
		sprintf( info1, "! butterworth band-stop of order %d", order );
	} else {
		printf( "*** wrong filter type ***\n" );
		return 0;
	} /*endif*/

	max_k = order / 2;  /* for even order */

	strcat( filename, ".FLR" );
	fp = fopen( filename, "w" );
	if  (fp == NULL)  {
		printf( "*** error opening output file %s ***\n", filename );
		return 0;
	} /*endif*/

	fprintf( fp, "%s\n", info1 );
	fprintf( fp, "%s\n", info2 );
	fprintf( fp, "%ld\n", 1357913578L );   /* magic longword */

	if  ((order % 2) != 0)  {  /* odd order -> generate L(z) */
		max_k = (order-1) / 2;  /* overwrite old value */
		fprintf( fp, "%d\n", 3 ); /* storage ID */
		fprintf( fp, "%e\n", delta_t ); /* sample distance */
		if  (strcmp(f_type,"LP") == 0)  {
			lp_l( wl, delta_t, a, b, &coeff_no, &norm );
		} else if  (strcmp(f_type,"HP") == 0)  {
			hp_l( wl, delta_t, a, b, &coeff_no, &norm );
		} else if  (strcmp(f_type,"BP") == 0)  {
			bp_l( wl, wu, delta_t, a, b, &coeff_no, &norm );
		} else if  (strcmp(f_type,"BS") == 0)  {
			bs_l( wl, wu, delta_t, a, b, &coeff_no, &norm );
		} /*endif*/
		fprintf( fp, "%e\n", norm );       /* write normalisation */
		fprintf( fp, "%d\n", coeff_no+1 ); /* number of numerator coeff. */
		for  (i=0; i<=coeff_no; i++)       /* write numerator coeff. */
			fprintf( fp, "%e\n", a[i] );
		fprintf( fp, "%d\n", coeff_no+1 ); /* number of denominator coeff. */
		for  (i=0; i<=coeff_no; i++)       /* write denominator coeff. */
			fprintf( fp, "%e\n", b[i] );
		if  (max_k > 0)  fprintf( fp, "%c\n", '@' ); /* separator */
	} /*endif*/


	/* generate Q(z,k,N) for k = 1,..,max_k */
	for  (k=1; k<=max_k; k++)  {
		fprintf( fp, "%d\n", 3 );  /* storage ID */
		fprintf( fp, "%e\n", delta_t ); /* sample distance */
		if  (strcmp(f_type,"LP") == 0)  {
			lp_q( k, order, wl, delta_t, a, b, &coeff_no, &norm );
		} else if  (strcmp(f_type,"HP") == 0)  {
			hp_q( k, order, wl, delta_t, a, b, &coeff_no, &norm );
		} else if  (strcmp(f_type,"BP") == 0)  {
			bp_q( k, order, wl, wu, delta_t, a, b, &coeff_no, &norm );
		} else if  (strcmp(f_type,"BS") == 0)  {
			bs_q( k, order, wl, wu, delta_t, a, b, &coeff_no, &norm );
		} /*endif*/
		fprintf( fp, "%e\n", norm );       /* write normalisation */
		fprintf( fp, "%d\n", coeff_no+1 ); /* number of numerator coeff */
		for  (i=0; i<=coeff_no; i++)       /* write numerator coeff */
			fprintf( fp, "%e\n", a[i] );
		fprintf( fp, "%d\n", coeff_no+1 ); /* number of denominator coeff */
		for  (i=0; i<=coeff_no; i++)       /* write denominator coeff */
			fprintf( fp, "%e\n", b[i] );
		if  (k < max_k)  fprintf( fp, "%c\n", '@' ); /* separator */
	} /*endfor*/

	fclose( fp );

	return 0;

} /* end of main */



/*--------------------------------------------------------------------------*/



void lp_l( float wc, float dt, float a[], float b[], int *lth, float *norm )

/* computes coefficients of L(z) for lowpass filters
 *
 * parameters of routine
 * float      wc;      input; corner frequency
 * float      dt;      input; sampling distance
 * float      a[];     output; numerator coefficients
 * float      b[];     output; denominator coefficients
 * int        *lth;    output; length of output arrays a abd b [0..*lth]
 * float      *norm;   output; normalisation
 */
{
	/* local variables */
	float    w_warp;      /* warped frequency */

	/* executable code */

	w_warp = tan( (dt/2.) * wc );
	a[0] = w_warp;
	a[1] = w_warp;
	b[0] = w_warp + 1.;
	b[1] = w_warp - 1.;

	*norm = 1.;
	*lth = 1;

} /* end of lp_l */



/*--------------------------------------------------------------------------*/



void lp_q( int k, int n, float wc, float dt, float a[], float b[],
	int *lth, float *norm )

/* computes coefficients of Q(z,k,N) for lowpass filters
 *
 * parameters of routine
 * int        k;          input; counter index
 * int        n;          input; order of filter
 * float      wc;         input; corner angular frequency
 * float      dt;         input; sampling distance
 * float      a[];        output; numerator coefficients
 * float      b[];        output; denominator coefficients
 * int        *lth;       output; length of output arrays a,b [0..*lth]
 * float      *norm;      output; normalisation
 */
{
	/* local variables */
	float    w_warp;     /* warped frequency */
	float    w_warp_2;   /* square of w_warp */

	/* executable code */

	w_warp = tan( (dt/2.) * wc );
	w_warp_2 = w_warp * w_warp;

	a[0] = w_warp_2;
	a[1] = 2. * w_warp_2;
	a[2] = w_warp_2;
	b[0] = 1. - 2.*real_ep(k,n)*w_warp + w_warp_2;
	b[1] = 2. * (w_warp_2 - 1.);
	b[2] = 1. + 2.*real_ep(k,n)*w_warp + w_warp_2;

	*norm = 1.;
	*lth = 2;

} /* end of lp_q */



/*--------------------------------------------------------------------------*/



void hp_l( float wc, float dt, float a[], float b[], int *lth, float *norm )

/* computes coefficients of L(z) for highpass filters
 *
 * parameters of routine
 * float      wc;      input; corner frequency
 * float      dt;      input; sampling distance
 * float      a[];     output; numerator coefficients
 * float      b[];     output; denominator coefficients
 * int        *lth;    output; length of output arrays a abd b [0..*lth]
 * float      *norm;   output; normalisation
 */
{
	/* local variables */
	float    w_warp;    /* warped frequency */

	/* executable code */

	w_warp = tan( (dt/2.) * wc );

	a[0] = 1.;
	a[1] = -1.;
	b[0] = w_warp + 1.;
	b[1] = w_warp - 1.;

	*norm = 1.;
	*lth = 1;

} /* end of hp_l */



/*--------------------------------------------------------------------------*/



void hp_q( int k, int n, float wc, float dt, float a[], float b[],
	int *lth, float *norm )

/* computes coefficients of Q(z,k,N) for highpass filters
 *
 * parameters of routine
 * int        k;          input; counter index
 * int        n;          input; order of filter
 * float      wc;         input; corner angular frequency
 * float      dt;         input; sampling distance
 * float      a[];        output; numerator coefficients
 * float      b[];        output; denominator coefficients
 * int        *lth;       output; length of output arrays a,b [0..*lth]
 * float      *norm;      output; normalisation
 */
{
	/* local variables */
	float    w_warp;     /* warped frequency */
	float    w_warp_2;   /* square of w_warp */

	/* executable code */

	w_warp = tan( (dt/2.) * wc );
	w_warp_2 = w_warp * w_warp;

	a[0] = 1.;
	a[1] = -2.;
	a[2] = 1.;
	b[0] = 1. - 2.*real_ep(k,n)*w_warp + w_warp_2;
	b[1] = 2. * (w_warp_2 - 1.);
	b[2] = 1. + 2.*real_ep(k,n)*w_warp + w_warp_2;

	*norm = 1.;
	*lth = 2;

} /* end of hp_q */



/*--------------------------------------------------------------------------*/



void bp_l( float wl, float wu, float dt, float a[], float b[],
	int *lth, float *norm )

/* computes coefficients of L(z) for bandpass filters
 *
 * parameters of routine
 * float      wl;      input; lower corner frequency
 * float      wu;      input; upper corner frequency
 * float      dt;      input; sampling distance
 * float      a[];     output; numerator coefficients
 * float      b[];     output; denominator coefficients
 * int        *lth;    output; length of output arrays a abd b [0..*lth]
 * float      *norm;   output; normalisation
 */
{
	/* local variables */
	float    w_warp;   /* warped frequency */
	float    alpha;    /* scratch */

	/* executable code */

	w_warp = tan( dt * (wu-wl) / 2. );
	alpha = cos( dt * (wu+wl) / 2. ) / cos( dt * (wu-wl) / 2. );

	a[0] = w_warp;
	a[1] = 0.;
	a[2] = -w_warp;
	b[0] = 1. + w_warp;
	b[1] = -2. * alpha;
	b[2] = 1. - w_warp;

	*norm = 1.;
	*lth = 2;

} /* end of bp_l */



/*--------------------------------------------------------------------------*/



void bp_q( int k, int n, float wl, float wu, float dt,
	float a[], float b[], int *lth, float *norm )

/* computes coefficients of Q(z,k,N) for bandpass filters
 *
 * parameters of routine
 * int        k;          input; counter index
 * int        n;          input; order of filter
 * float      wl;         input; lower corner angular frequency
 * float      wu;         input; upper corner angular frequency
 * float      dt;         input; sampling distance
 * float      a[];        output; numerator coefficients
 * float      b[];        output; denominator coefficients
 * int        *lth;       output; length of output arrays a,b [0..*lth]
 * float      *norm;      output; normalisation
 */
{
	/* local variables */
	float    w_warp;    /* warped frequency */
	float    w_warp_2;  /* square of w_warp */
	float    alpha;     /* scratch */
	float    alpha_2;   /* square of alpha */

	/* executable code */

	w_warp = tan( dt * (wu - wl) / 2. );
	w_warp_2 = w_warp * w_warp;
	alpha = cos( dt * (wu + wl) / 2. ) / cos( dt * (wu - wl) / 2. );
	alpha_2 = alpha * alpha;

	a[0] = w_warp_2;
	a[1] = 0.;
	a[2] = -2. * w_warp_2;
	a[3] = 0.;
	a[4] = w_warp_2;
	b[0] = 1. - 2.*real_ep(k,n)*w_warp + w_warp_2;
	b[1] = 4. * alpha * ( real_ep(k,n)*w_warp - 1. );
	b[2] = 2. * ( 1. + 2.*alpha_2 - w_warp_2 );
	b[3] = -4. * alpha * ( real_ep(k,n)*w_warp + 1. );
	b[4] = 1. + 2.*real_ep(k,n)*w_warp + w_warp_2;

	*norm = 1.;
	*lth = 4;

} /* end of bp_q */



/*--------------------------------------------------------------------------*/



void bs_l( float wl, float wu, float dt, float a[], float b[],
	int *lth, float *norm )

/* computes coefficients of L(z) for bandstop filters
 *
 * parameters of routine
 * float      wl;      input; lower corner frequency
 * float      wu;      input; upper corner frequency
 * float      dt;      input; sampling distance
 * float      a[];     output; numerator coefficients
 * float      b[];     output; denominator coefficients
 * int        *lth;    output; length of output arrays a abd b [0..*lth]
 * float      *norm;   output; normalisation
 */
{
	/* local variables */
	float    w_warp;   /* warped frequency */
	float    alpha;    /* scratch */

	/* executable code */

	w_warp = tan( dt * (wu - wl) / 2. );
	alpha = cos( dt * (wu + wl) / 2. ) / cos( dt * (wu - wl) / 2. );

	a[0] = 1.;
	a[1] = -2. * alpha;
	a[2] = 1.;
	b[0] = 1. + w_warp;
	b[1] = -2. * alpha;
	b[2] = 1. - w_warp;

	*norm = 1.;
	*lth = 2;

} /* end of bs_l */



/*--------------------------------------------------------------------------*/



void bs_q( int k, int n, float wl, float wu, float dt,
	float a[], float b[], int *lth, float *norm )

/* computes coefficients of Q(z,k,N) for bandstop filters
 *
 * parameters of routine
 * int        k;          input; counter index
 * int        n;          input; order of filter
 * float      wl;         input; lower corner angular frequency
 * float      wu;         input; upper corner angular frequency
 * float      dt;         input; sampling distance
 * float      a[];        output; numerator coefficients
 * float      b[];        output; denominator coefficients
 * int        *lth;       output; length of output arrays a,b [0..*lth]
 * float      *norm;      output; normalisation
 */
{
	/* local variables */
	float    w_warp;     /* warped frequency */
	float    w_warp_2;   /* square of w_warp */
	float    alpha;      /* scratch */
	float    alpha_2;    /* square of alpha */

	/* executable code */

	w_warp = tan( dt * (wu - wl) / 2. );
	w_warp_2 = w_warp * w_warp;
	alpha = cos( dt * (wu + wl) / 2. ) / cos( dt * (wu - wl) / 2. );
	alpha_2 = alpha * alpha;

	a[0] = 1.;
	a[1] = -4. * alpha;
	a[2] = 2. * ( 1. + 2.*alpha_2 );
	a[3] = -4. * alpha;
	a[4] = 1.;
	b[0] = 1. - 2.*real_ep(k,n)*w_warp + w_warp_2;
	b[1] = 4. * alpha * ( real_ep(k,n)*w_warp - 1. );
	b[2] = 2. * ( 1. + 2.*alpha_2 - w_warp_2 );
	b[3] = -4. * alpha * ( real_ep(k,n)*w_warp + 1. );
	b[4] = 1. + 2.*real_ep(k,n)*w_warp + w_warp_2;

	*norm = 1.;
	*lth = 4;

} /* end of bs_q */



/*--------------------------------------------------------------------------*/



void read_freq( char str[], char inp_kind, float *freq )

/* reads frequency from user. returns *freq
 *
 * parameters of routine
 * char       str[];       input; source string
 * char       inp_kind;    input; input mode: t, f or w
 * float      *freq;       output; returned angular frequency
 */
{
	/* executable code */

	sscanf( str, "%f\n", freq );
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



float real_ep( int k, int n )

/* computes Re(E(k,N)) from above formula
 *
 * parameters of routine
 * int        k;      input; counter index 1,..,n
 * int        n;      input; order of filter
 */
{
	/* executable code */

	return (-sin( (BC_PI/2.0/(float)n) * (float)(2*k-1) ));

} /* end of real_ep */



/*--------------------------------------------------------------------------*/

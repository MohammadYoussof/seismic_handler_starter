
/* file SHCORR.C
 *      ========
 *
 * version 7, 29-Jan-2007
 *
 * trace correlation routines
 * K. Stammler, 7-JUL-1990
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1992,  Klaus Stammler, Federal Institute for Geosciences
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
#include <math.h>
#include "basecnst.h"
#include BC_SYSBASE
#include "shconst.h"
#include "mxusrdef.h"
#include "scusrdef.h"
#include "scerrors.h"


/* global variables */
static REAL    corrl_lo_scv={-30.0};    /* correlation length start */
static REAL    corrl_hi_scv={100.0};    /* correlation length end */
static int     corrmode_scv={1};        /* mode of corr computation */

/*  mode = 1:
 *                   sum_{i} (x[i]-x_mean) * (y[i]-y_mean)
 *  cc  =   ----------------------------------------------------------
 *          sqrt(sum_{i}(x[i]-x_mean)^2) * sqrt(sum{j}(y[i]-y_mean)^2)
 *
 *  mode = 2:
 *          sum_{i} (x[i]-x_mean) * (y[i]-y_mean)
 *  cc  =   -------------------------------------
 *               sum_{i}(x[i]-x_mean)^2)
 *
 *  mode = 3:
 *          sum_{i} x[i] * y[i]
 *  cc  =   -------------------
 *           sum_{i} (x[i])^2
 *
 *  mode = 4:
 *          
 *  cc  =   sum_{i} x[i] * y[i]
 */


#define Sign(r) (((r) > 0.0) ? 1.0 : -1.0)

/* prototypes of local routines */
SAMPLE sc_cc_coeff( SAMPLE wav[], long lth, SAMPLE trc[] );
void sc_trccorr( SAMPLE z[], SAMPLE r[], long lth,
	REAL *zz, REAL *rr, REAL *zr );

/*---------------------------------------------------------------------------*/



void sc_set_corrm( int mode, STATUS *status )

/* set correlation mode
 *
 * parameters of routine
 * int       mode;     input; correlation mode (see above)
 * STATUS    *status;  output; return status
 */
{
	/* executable code */

	if  ((mode < 1) || (mode > 4))  {
		*status = SCE_UKCCMODE;
		return;
	} /*endif*/
	corrmode_scv = mode;

} /* end of sc_set_corrm */



/*---------------------------------------------------------------------------*/



void sc_set_corrl( REAL from_t, REAL to_t, STATUS *status )

/* sets correlation length for all following correlations
 *
 * parameters of routine
 * REAL       from_t;     input; rel. start time
 * REAL       to_t;       input; rel. end time
 * STATUS     *status;    output; return status
 */
{
	/* executable code */

	if  (to_t <= from_t)  {
		*status = SCE_ZEROWDW;
		return;
	} /*endif*/

	corrl_lo_scv = from_t;
	corrl_hi_scv = to_t;

} /* end of sc_set_corrl */



/*---------------------------------------------------------------------------*/



long sc_restrclth( REAL dt )

/* returns length of result trace of correlation
 *
 * parameters of routine
 * REAL       dt;            input; sample distance
 */
{
	/* executable code */

	return (Nlong((corrl_hi_scv-corrl_lo_scv) / dt) + 1);

} /* end of sc_restrclth */



/*---------------------------------------------------------------------------*/



void sc_do_corr( SAMPLE *wav, long wav_s, long wav_e, SAMPLE *trc,
	long maxtrc, REAL dt, SAMPLE *res, STATUS *status )

/* computes crosscorrelation of two traces "wav" and "trc".  The
 * wavelet wav[wav_s..wav_e] is correlated with the trace windows
 * trc[wav_s-cls..wav_e-cls], trc[wav_s-cls+1..wav_e-cls+1], ...
 * up to trc[wav_s+cle..wav_e+cle].  Not existing samples of the
 * "trc"-array are replaced by zeroes.
 *
 * parameters of routine
 * SAMPLE     *wav;          input; (whole) wavelet trace
 * long       wav_s, wav_e;  input; wavelet window (samples)
 * SAMPLE     *trc;          input; 2. correlation trace
 * long       maxtrc;        input; length of 2. correlation trace (samples)
 * REAL       dt;            input; sample distance in sec
 * SAMPLE     *res;          output; result trace
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	long     wavlth;     /* length of wavelet in samples */
	long     reslth;     /* length of result trace in samples */
	long     shift;      /* shift of wavelet */
	long     i;          /* counter */

	/* executable code */

	wavlth = wav_e - wav_s + 1;
	wav += wav_s;
	reslth = sc_restrclth( dt );
	shift = wav_s + Nlong( corrl_lo_scv / dt );

	if  (maxtrc < wavlth)  {
		*status = SCE_SHORTTRC;
		return;
	} /*endif*/

	for  (i=0;i<reslth;i++)  {
		if  (shift < 0)  {
			*res++ = sc_cc_coeff( wav-shift, wavlth+shift, trc );
		} else if  ((shift+wavlth) > maxtrc)  {
			*res++ = sc_cc_coeff( wav, maxtrc-shift, trc+shift );
		} else {
			*res++ = sc_cc_coeff( wav, wavlth, trc+shift );
		} /*endif*/
		shift++;
		sy_sharecpu();
	} /*endfor*/

} /* end of sc_do_corr */



/*---------------------------------------------------------------------------*/



SAMPLE sc_cc_coeff( SAMPLE wav[], long lth, SAMPLE trc[] )

/* computes crosscorrelation coefficients of two wavelets "wav" and
 * "trc" with length "wavlth".  Result is stored in "cc"
 *
 * parameters of routine
 * SAMPLE     wav[];    input; first wavelet
 * long       lth;      input; length of both wavelets
 * SAMPLE     trc[];    input; second wavelet
 * SAMPLE     *cc;      output; computed crosscorrelation coefficient
 */
{
	/* local variables */
	SAMPLE   w_mean, t_mean;    /* mean values */
	SAMPLE   wq, tq;            /* square sums */
	SAMPLE   wn, tn;            /* samples removed from mean */
	SAMPLE   *w, *t;            /* moving pointer */
	SAMPLE   cc;                /* correlation coefficient */
	long     i;                 /* counter */

	/* executable code */

	cc = 0.0;
	if  (lth <= 0)  return 0.0;

	switch  (corrmode_scv)  {
		case 1:

			/* compute mean values */
			w_mean = t_mean = 0.0;
			w = wav; t = trc;
			for  (i=0;i<lth;i++)  {
				w_mean += *w++;
				t_mean += *t++;
				if  (!(i&0x7f))  sy_sharecpu();
			} /*endfor*/
			w_mean /= (float)lth;
			t_mean /= (float)lth;
			sy_sharecpu();

			/* compute square sums and correlation */
			wq = tq = 0.0;
			w = wav;  t = trc;
			for  (i=0;i<lth;i++)  {
				wn = *w++ - w_mean;
				tn = *t++ - t_mean;
				wq += wn * wn;
				tq += tn * tn;
				cc += wn * tn;
				if  (!(i&0x7f))  sy_sharecpu();
			} /*endfor*/
			sy_sharecpu();

			/* normalise correlation */
			wq = sqrt(wq) * sqrt(tq);
			if  (wq > 1.0e-25)  {
				cc /= wq;
			} else {
				cc = 0.0;
			} /*endif*/
			break;

		case 2:

			/* compute mean values */
			w_mean = t_mean = 0.0;
			w = wav; t = trc;
			for  (i=0;i<lth;i++)  {
				w_mean += *w++;
				t_mean += *t++;
				if  (!(i&0x7f))  sy_sharecpu();
			} /*endfor*/
			w_mean /= (float)lth;
			t_mean /= (float)lth;
			sy_sharecpu();

			/* compute square sum and correlation */
			wq = 0.0;
			w = wav;  t = trc;
			for  (i=0;i<lth;i++)  {
				wn = *w++ - w_mean;
				tn = *t++ - t_mean;
				wq += wn * wn;
				cc += wn * tn;
				if  (!(i&0x7f))  sy_sharecpu();
			} /*endfor*/
			sy_sharecpu();

			/* normalise correlation */
			if  (wq > 1.0e-25)  {
				cc /= wq;
			} else {
				cc = 0.0;
			} /*endif*/
			break;

		case 3:

			/* compute square sum and correlation */
			wq = 0.0;
			w = wav;  t = trc;
			for  (i=0;i<lth;i++)  {
				wn = *w++;
				wq += wn * wn;
				cc += wn * (*t++);
				if  (!(i&0x7f))  sy_sharecpu();
			} /*endfor*/
			sy_sharecpu();

			/* normalise correlation */
			if  (wq > 1.0e-25)  {
				cc /= wq;
			} else {
				cc = 0.0;
			} /*endif*/
			break;

		case 4:

			/* compute correlation */
			w = wav;  t = trc;
			for  (i=0;i<lth;i++)  {
				cc += (*w++) * (*t++);
				if  (!(i&0x7f))  sy_sharecpu();
			} /*endfor*/
			break;

		default:
			sy_alert( "*** program error in SHCORR.C ***" );
			return 0.0;

	} /*endswitch*/

	return cc;

} /* end of sc_cc_coeff */



/*---------------------------------------------------------------------------*/



void sc_polar2( SAMPLE zarr[], SAMPLE rarr[], long lth, REAL *angle,
	REAL eigval[] )

/* computes main direction of polarisation in 2-dimensional case.
 * Result is the angle "angle" to the z-direction (in degrees)
 *
 * the coherence matrix
 *
 *          -               -        -        -
 *         |  <Z*Z>   <Z*R>  |      |  z    c  |
 *   C  =  |                 |  =:  |          |
 *         |  <Z*R>   <R*R>  |      |  c    r  |
 *          -               -        -        -
 *
 * is diagonalised.  The direction of the eigenvector with the
 * largest eigenvalue is the desired direction
 *
 * Let
 *   k := (z - r) / c
 * and
 *   alpha[1] := 1/2 * (k + sqrt(k*k+4))
 *   alpha[2] := 1/2 * (k - sqrt(k*k+4))
 *
 * then the eigenvalues lambda[i] and eigenvectors v[i] (i = 1,2)
 * are given by
 *
 *   lambda[i] = r + c*alpha[i]
 *   v[i] = ( alpha[i], 1 )
 *
 *
 * parameters of routine
 * SAMPLE     zarr[];  input; samples of z-trace
 * SAMPLE     rarr[];  input; samples of r-trace
 * long       lth;     input; length of traces in samples
 * REAL       *angle;  output; computed angle
 * REAL       eigval[];output; 2 eigenvalues returned
 */
{
	/* local variables */
	REAL     z, c, r, k; /* variables from above formula */
	REAL     alpha[2];   /* see above */
	REAL     lambda[2];  /* eigenvalues of coherence matrix */

	/* executable code */

	sc_trccorr( zarr, rarr, lth, &z, &r, &c );

	if  (Abs(c) < SHC_EPSILON)  {
		*angle = 0.0;
		if  (eigval != NULL)  {
			eigval[0] = 0.0;
			eigval[1] = 0.0;
		} /*endif*/
	} else {
		k = (z - r) / c;
		alpha[0] = 0.5 * (k + sqrt(k*k+4));
		alpha[1] = 0.5 * (k - sqrt(k*k+4));
		lambda[0] = r + c*alpha[0];
		lambda[1] = r + c*alpha[1];
		if  (Abs(lambda[0]) > Abs(lambda[1]))  {
			*angle = atan( 1.0 / alpha[0] );
		} else {
			*angle = atan( 1.0 / alpha[1] );
		} /*endif*/
		if  (eigval != NULL)  {
			eigval[0] = lambda[0];
			eigval[1] = lambda[1];
		} /*endif*/
	} /*endif*/

	*angle *= SHC_RAD_TO_DEG;

} /* end of sc_polar2 */



/*---------------------------------------------------------------------------*/



void sc_polar3( SAMPLE zarr[], SAMPLE narr[], SAMPLE earr[],
	long lth, REAL *azim, REAL *inci, REAL eigval[], STATUS *status )

/* computes main direction of polarisation in 3-dimensional case.
 * Result is azimuth and angle of incidence (in degrees).
 *
 * the coherence matrix
 *
 *          -                       -         -              -
 *         |  <Z*Z>   <N*Z>   <E*Z>  |      |  zz    zn   ze  |
 *         |                         |      |                 |
 *   C  =  |  <Z*N>   <N*N>   <E*N>  |  =:  |  zn    nn   ne  |
 *         |                         |      |                 |
 *         |  <Z*E>   <N*E>   <E*E>  |      |  ze    ne   ee  |
 *          -                       -        -               -
 *
 * is diagonalised.  The direction of the eigenvector with the
 * largest eigenvalue is the desired direction
 *
 * parameters of routine
 * SAMPLE     zarr[];  input; samples of z-trace
 * SAMPLE     narr[];  input; samples of n-trace
 * SAMPLE     earr[];  input; samples of e-trace
 * long       lth;     input; length of traces in samples
 * REAL       *azim;   output; computed azimuth
 * REAL       *inci;   output; computed angle of incidence
 * REAL       eigval[];output; 3 eigenvalues returned
 * STATUS     *status; output; return status
 */
{
	/* local constants */
#   define Z 0
#   define N 1
#   define E 2

	/* local variables */
	REAL     cm[MXC_DIM][MXC_DIM];     /* coherence matrix */
	REAL     trafo[MXC_DIM][MXC_DIM];  /* eigenvectors of cm */
	REAL     eval[MXC_DIM];            /* eigenvalues of cm */
	int      max_ev;                   /* index of maximum eigenvalue */

	/* executable code */

	if  (MXC_DIM != 3)  {
		*status = SCE_MXDIM;
		return;
	} /*endif*/

	/* compute coherence matrix */
	sc_trccorr( zarr, narr, lth, &(cm[Z][Z]), &(cm[N][N]), &(cm[Z][N]) );
	sc_trccorr( zarr, earr, lth, &(cm[Z][Z]), &(cm[E][E]), &(cm[Z][E]) );
	sc_trccorr( narr, earr, lth, &(cm[N][N]), &(cm[E][E]), &(cm[N][E]) );
	cm[N][Z] = cm[Z][N];
	cm[E][Z] = cm[Z][E];
	cm[E][N] = cm[N][E];

	/* diagonalise coherence matrix */
	mx_diag_matrix( cm, eval, trafo, status );
	if  (*status != SCE_NOERROR)  return;

	/* search maximum eigenvalue */
	max_ev = Z;
	if  (Abs(eval[N]) > Abs(eval[max_ev]))  max_ev = N;
	if  (Abs(eval[E]) > Abs(eval[max_ev]))  max_ev = E;

	if  ((trafo[E][max_ev] == 0.0) && (trafo[N][max_ev] == 0.0))  {
		*azim = 0.0;
	} else {
		*azim = atan2( trafo[E][max_ev], trafo[N][max_ev] ) * SHC_RAD_TO_DEG;
	} /*endif*/
	*inci = acos( trafo[Z][max_ev] ) * SHC_RAD_TO_DEG;

	*azim += 180.0;
	if  (*azim > 360.0)  *azim -= 360.0;

	if  (eigval != NULL)  {
		eigval[0] = eval[0];
		eigval[1] = eval[1];
		eigval[2] = eval[2];
	} /*endif*/

#   undef Z
#   undef N
#   undef E

} /* end of sc_polar3 */



/*---------------------------------------------------------------------------*/



void sc_trccorr( SAMPLE z[], SAMPLE r[], long lth,
	REAL *zz, REAL *rr, REAL *zr )

/* computes sums of squares and cross products
 *
 * parameters of routine
 * SAMPLE     z[];    input; z-array
 * SAMPLE     r[];    input; r-array
 * long       lth;    input; length of traces
 * REAL       *zz;    output; sum of z squares
 * REAL       *rr;    output; sum of r squares
 * REAL       *zr;    output; sum of r*z products
 */
{
	/* local variables */
	long     i;        /* counter */

	/* executable code */

	*zz = *rr = *zr = 0.0;
	for  (i=0;i<lth;i++)  {
		*zz += *z * *z;
		*rr += *r * *r;
		*zr += (*z++) * (*r++);
	} /*endfor*/

} /* end of sc_trccorr */



/*---------------------------------------------------------------------------*/

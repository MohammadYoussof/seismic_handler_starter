
/* file SCUSRDEF.H
 *      ==========
 *
 * version 4, 29-Jan-2007
 *
 * prototypes of module SHCORR.C
 * K. Stammler, 22-MAY-91
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


#ifndef __SCUSRDEF
#define __SCUSRDEF

#ifndef __SHCONST
#include "shconst.h"
#endif

/*---------------------------------------------------------------------------*/


void sc_set_corrm( int mode, STATUS *status );

/* set correlation mode
 *
 * parameters of routine
 * int       mode;     input; correlation mode (see above)
 * STATUS    *status;  output; return status
 */


/*---------------------------------------------------------------------------*/


void sc_set_corrl( REAL from_t, REAL to_t, STATUS *status );

/* sets correlation length for all following correlations
 *
 * parameters of routine
 * REAL       from_t;     input; rel. start time
 * REAL       to_t;       input; rel. end time
 * STATUS     *status;    output; return status
 */


/*---------------------------------------------------------------------------*/


long sc_restrclth( REAL dt );

/* returns length of result trace of correlation
 *
 * parameters of routine
 * REAL       dt;            input; sample distance
 */


/*---------------------------------------------------------------------------*/


void sc_do_corr( SAMPLE *wav, long wav_s, long wav_e, SAMPLE *trc,
	long maxtrc, REAL dt, SAMPLE *res, STATUS *status );

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


/*---------------------------------------------------------------------------*/


void sc_polar2( SAMPLE zarr[], SAMPLE rarr[], long lth, REAL *angle,
	REAL eigval[] );

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


/*---------------------------------------------------------------------------*/


void sc_polar3( SAMPLE zarr[], SAMPLE narr[], SAMPLE earr[],
	long lth, REAL *azim, REAL *inci, REAL eigval[], STATUS *status );

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


/*---------------------------------------------------------------------------*/

#endif /* __SCUSRDEF */

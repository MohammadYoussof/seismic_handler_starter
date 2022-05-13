
/* file FFUSRDEF.H
 *      ==========
 *
 * version 9, 22-May-2006
 *
 * prototypes of module FFTFILTR.C
 * K. Stammler, 11-JUL-1990
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


#ifndef __FFUSRDEF
#define __FFUSRDEF

#ifndef __SHCONST
#include "shconst.h"
#endif



/* constants */

#define FFC_MAXDEGREE 25
	/* maximum number of poles or zeroes */

/* modes of hilbphase computation */
#define FFC_GET_PHASE 0
#define FFC_GET_PHASE_I 1
#define FFC_GET_PHASE_R 2



/* types */

typedef struct {
	REAL     norm;                 /* normalisation */
	int      no_of_zeroes;         /* number of zeroes */
	COMPLEX  zero[FFC_MAXDEGREE];  /* zeroes of transfer function */
	int      no_of_poles;          /* number of poles */
	COMPLEX  pole[FFC_MAXDEGREE];  /* poles of transfer function */
} FFT_RATFCT;

typedef struct {
	long     lth;                  /* number of vertices */
	REAL     *frq;                 /* pointer to frequencies */
	REAL     *mod;                 /* modulus of filter function */
	REAL     *phase;               /* phase of filter function */
} FFT_FILFUNC;


/* macros */

#define ff_free_filfunc(f) \
	sy_deallocmem((f)->frq);(f)->lth=0;(f)->frq=(f)->mod=(f)->phase=NULL



/*-------------------------------------------------------------------------*/


void ff_filter_input( int listlth, char *flt_list[], int poslist[],
	STATUS *status );

/* reads a list of filters into memory.  If "poslist[i]" is negative,
 * the i-th filter is inverted, that means poles become zeroes and
 * zeroes become poles.
 *
 * parameters of routine
 * int        listlth;      input; length of filter list
 * char       *flt_list[];  input; filter list
 * int        poslist[];    input; position list
 * STATUS     *status;      output; return status
 */


/*-------------------------------------------------------------------------*/


void ff_filfunc_input( char file[], FFT_FILFUNC *filf, STATUS *status );

/* reads digital filter function to specified filter structure "filf" or
 * into internal variable if "filf" is NULL
 *
 * parameters of routine
 * char       file[];         input; name of filter file
 * FFT_FILFUNC *filf;         output; output structure or NULL
 * STATUS     *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void ff_filter( BOOLEAN filf, SAMPLE src[], long lth, REAL dt,
	float taper, SAMPLE dst[], STATUS *status );

/* filters array "src" with all filters read in.  The result is
 * stored in "dst".
 *
 * parameters of routine
 * BOOLEAN    filf;        input; digital filter (TRUE) or poles-zeroes
 * SAMPLE     src[];       input; input trace
 * long       lth;         input; length of traces
 * REAL       dt;          input; sample distance in sec
 * float      taper;       input; start of taper window [0..1]
 * SAMPLE     dst[];       output; filtered trace
 * STATUS     *status;     output; return status
 */


/*-------------------------------------------------------------------------*/


void ff_attenuate( SAMPLE src[], long lth, REAL dt, REAL att,
	SAMPLE dst[], STATUS *status );

/* attenuates array "src" with t* of "att".  The result is
 * stored in "dst".
 *
 * parameters of routine
 * SAMPLE     src[];       input; input trace
 * long       lth;         input; length of traces
 * REAL       dt;          input; sample distance in sec
 * REAL       att;         input; attenuation (t* in sec)
 * SAMPLE     dst[];       output; filtered trace
 * STATUS     *status;     output; return status
 */


/*-------------------------------------------------------------------------*/


void ff_hilbert( SAMPLE src[], long lth, SAMPLE dst[], STATUS *status );

/* computes Hilbert transformation of "src"
 *
 * parameters of routine
 * SAMPLE     src[];       input; input trace
 * long       lth;         input; length of traces
 * SAMPLE     dst[];       output; filtered trace
 * STATUS     *status;     output; return status
 */


/*-------------------------------------------------------------------------*/


void ff_hilbphase( int mode, SAMPLE src[], long lth, SAMPLE dst[],
	STATUS *status );

/* computes Hilbert transformation of "src", uses this as imaginary
 * part of the input function and returns the phase in deg of this
 * complex value.
 *
 * parameters of routine
 * int        mode;        input; mode of computation
 * SAMPLE     src[];       input; input trace
 * long       lth;         input; length of traces
 * SAMPLE     dst[];       output; filtered trace
 * STATUS     *status;     output; return status
 */


/*-------------------------------------------------------------------------*/


void ff_mindelay( SAMPLE src[], long lth, long offset, BOOLEAN ac,
	SAMPLE dst[], STATUS *status );

/* transforms "src" into a minimum delay wavelet
 *
 * parameters of routine
 * SAMPLE     src[];       input; input trace
 * long       lth;         input; length of traces
 * long       offset;      input; number of offset samples
 * BOOLEAN    ac;          input; input trace is autocorrelation
 * SAMPLE     dst[];       output; minimum delay wavelet trace
 * STATUS     *status;     output; return status
 */


/*-------------------------------------------------------------------------*/


void ff_specdiv( SAMPLE f1[], SAMPLE f2[], long lth, REAL dt,
	REAL wlevel, REAL alpha, REAL offset, SAMPLE res[], STATUS *status );

/* divides spectrum of f1 by spectrum of f2 and stores the result
 * in res.  The formula is:
 *    res[i] = f1[i]*f2^[i]/phi[i] * G[i],  where
 *    phi[i] = max{f2[i]*f2^[i], wlevel*max[f2[i]*f2^[i]]}
 *    G[i] = exp( -w[i]*w[i]/(4*alpha*alpha) )
 *    w[i]:   circular frequency
 *    ^:      complex conjugate
 *
 * parameters of routine
 * SAMPLE     f1[], f2[];     input; input wavelets
 * long       lth;            input; length of traces
 * REAL       wlevel;         input; water level
 * REAL       alpha;          input; width of gauss peak
 * REAL       offset;         input; time offset of output trace
 * SAMPLE     res[];          output; results trace
 * STATUS     *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void ff_compress( STATUS *status );

/* combines a cascade of filters read in to a single filter function
 *
 * parameters of routine
 * STATUS     *status;    output; return status
 */


/*-------------------------------------------------------------------------*/


REAL ff_filter_amplitude( char filter[], REAL frq, STATUS *status );

/* returns amplitude of filter "filter" at frequency "frq"
 *
 * parameters of routine
 * char       filter[];      input; name of filter
 * REAL       frq;           input; frequency in Hz (no angular frequency)
 * STATUS     *status;       output; return status
 *                           returns amplitude of filter
 */



/*-------------------------------------------------------------------------*/


void ff_shorten_zeroes( FFT_RATFCT fct[], int no );

/* shortens zeroes in all filter functions fct[0..no-1]
 *
 * parameters of routine
 * FFT_RATFCT fct[];      modify; filter functions to be shortened
 * int        no;         input; number of filter functions
 */


/*-------------------------------------------------------------------------*/

#endif /* __FFUSRDEF */

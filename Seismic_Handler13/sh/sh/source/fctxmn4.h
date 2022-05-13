
/* file FCTXMN4.H
 *      =========
 *
 * version 7, 22-May-2006
 *
 * prototypes of module SHMENU4.C
 * K. Stammler, 5-APR-1991
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


#ifndef __FCTXMN4
#define __FCTXMN4


/*--------------------------------------------------------------------*/


void mn4_append( PARAM *par, STATUS *status );

/* appends trace to another
 * par 1:  trace to append
 * par 2:  destination trace
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn4_int( PARAM *par, STATUS *status );

/* integrates traces
 * par 1:  trace list
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn4_derive( PARAM *par, STATUS *status );

/* computes derivatives of traces
 * par 1:  trace list
 * par 2:  algorithm ID
 *           1: y[i] = 1/(2*h) (-y[i-1] + y[i+1])
 *           2: y[i] = 1/(12*h) (y[i-2] - 8*y[i-1] + 8y[i+1] - y[i+2])
 *           3: y[i] = 1/h (-y[i] + y[i+1])
 *           4: y[i] = 1/(2*h) (-3*y[i] + 4*y[i+1] - y[i+2])
 *           5: y[i] = 1/(12*h) (-3*y[i-1] - 10*y[i] + 18*y[i+1] -
 *                         6*y[i+2] + y[i+3])
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn4_demean( PARAM *par, STATUS *status );

/* removes mean value from list of traces
 * par 1:  trace list
 * par 2:  lo time
 * par 3:  hi time
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn4_polfil( PARAM *par, STATUS *status );

/* polarisation filter
 * par 1:  trace list
 * par 2:  coherence length
 * par 3:  lo time
 * par 4:  hi time
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn4_spectrum( PARAM *par, STATUS *status );

/* computes power spectrum
 * par 1:  trace list
 * par 2:  order of approximation (number of poles)
 * par 3:  lo time
 * par 4:  hi time
 * par 5:  lo frq bound
 * par 6:  hi frq bound
 * par 7:  frq step
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn4_fft( PARAM *par, STATUS *status );

/* computes power spectrum with FFT
 * par 1:  trace list
 * par 2:  lowdw
 * par 3:  hiwdw
 * par 4:  k
 * par 5:  m
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn4_connect( PARAM *par, STATUS *status );

/* connects two traces
 * par 1:  subfunction (MUL, DIV)
 * par 2:  first input trace
 * par 3:  second input trace
 * par 4:  lowdw
 * par 5:  hiwdw
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn4_trend( PARAM *par, STATUS *status );

/* removes trend in traces
 * par 1:  trace list
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn4_fit( PARAM *par, STATUS *status );

/* fits a trace to a given wavelet
 * par 1:  trace to be fitted
 * par 2:  lo-bound
 * par 3:  hi-bound
 * par 4:  fit function
 * par 5..: outputs
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn4_stalta( PARAM *par, STATUS *status );

/* applies STA/LTA algorith to trace
 * par 1:  detection trace
 * par 2:  sta length in samples
 * par 3:  lta length in samples
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn4_stability( PARAM *par, STATUS *status );

/* applies stability algorithm to trace
 * par 1:  detection trace
 * par 2:  tolerance
 * par 3:  add number
 * par 4:  factor
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn4_leveldetec( PARAM *par, STATUS *status );

/* level detector, writes out times of values above certain level
 * par 1:  trace
 * par 2:  amplitude level
 * par 3:  output file
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/

#endif /* __FCTXMN4 */

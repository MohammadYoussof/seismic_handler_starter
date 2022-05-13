
/* file FCTXMN2.H
 *      =========
 *
 * version 2, 22-MAY-2006
 *
 * prototypes of module SHMENU2.C
 * K. Stammler, 2-JUL-1990
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

#ifndef __FCTXMN2
#define __FCTXMN2

#ifndef __CPUSRDEF
#include "cpusrdef.h"
#endif


/*-------------------------------------------------------------------------*/


void mn2_fili( PARAM *par, int *status );

/* reads one or more filter files into memory
 * 1. param:  F or R  (FFT filters or recursive filters)
 * 2.-N. param:  filter names
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void mn2_filter( PARAM *par, int *status );

/* filters one or more traces with filters read with FILI
 * 1. param:  F or R  (FFT filters or recursive filters)
 * 2. param:  trace list
 * 3. param:  lower bound of time window
 * 4. param:  upper bound of time window
 * 5. param:  if attenuation, t* in sec
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void mn2_cmd( PARAM *par, int *status );

/* creates command file containing all previously entered command
 * (since start of session or last CMD command).
 * 1. param:  name of command file (no extension)
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void mn2_time( PARAM *par, int *status );

/* returns time value of selected position
 * 1. param:  output var - absolute time position
 * 2. param:  output var - relative time position
 * 3. param:  output var - y value
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void mn2_mark( PARAM *par, int *status );

/* marks time positions on a trace
 * 1. param:  trace position number
 * 2. param:  time position (absolute or relative)
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void mn2_corr( PARAM *par, int *status );

/* computes crosscorrelation of two traces
 * 1. param:  trace number of correlation wavelet
 * 2. param:  wavelet window start
 * 3. param:  wavelet window end
 * 4. param:  2. correlation trace number
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void mn2_corrl( PARAM *par, int *status );

/* sets correlation length
 * 1. param:  start of correlation window
 * 2. param:  end of correlation window
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void mn2_mdir( PARAM *par, int *status );

/* computation of direction of polarisation
 * 1. param:  trace list (2 or 3 traces)
 * 2. param:  start of correlation window
 * 3. param:  end of correlation window
 * 4. param:  output var - 2-dim angle or azimuth (3-dim)
 * 5. param:  output var - angle of incidence (3-dim)
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void mn2_trcfct( PARAM *par, int *status );

/* trace functions
 * 1. param:  function name
 * 2. param:  ...
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/



void mn2_hide( PARAM *par, int *status );

/* hides traces (remove from display, keep in memeory)
 * 1. param:  trace list
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void mn2_display( PARAM *par, int *status );

/* displays traces at position
 * 1. param:  trace list
 * 2. param:  start position
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void mn2_extract( PARAM *par, int *status );

/* extracts info from q-file
 * 1. param:  q-file name
 * 2. param:  record number
 * 3. param:  info entry description string
 * 4. param:  output variable
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/

#endif /* __FCTXMN2 */

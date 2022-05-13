
/* file FCTXMN5.H
 *      =========
 *
 * version 16, 22-May-2006
 *
 * prototypes of module SHMENU5.C
 * K. Stammler, 20-Feb-1992
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

#ifndef __FCTXMN5
#define __FCTXMN5

#ifndef __CPUSRDEF
#include "cpusrdef.h"
#endif



/*--------------------------------------------------------------------*/


void mn5_smooth( PARAM *par, STATUS *status );

/* smooths traces; modifies existing traces
 * par 1:  trace list
 * par 2:  order of smoothing
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn5_specdiv( PARAM *par, STATUS *status );

/* divides spectra of two traces
 * par 1:  trace 1
 * par 2:  trace 2
 * par 3:  lo window
 * par 4:  hi window
 * par 5:  water level
 * par 6:  width of gauss peak
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn5_prompt( PARAM *par, int maxlth, char prompt[], STATUS *status );

/* changes prompt of SH
 * par 1:  new prompt text
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * int        maxlth;    input; maximum length of output string
 * char       prompt[];  output; new prompt
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn5_log( PARAM *par, STATUS *status );

/* logarithmic plot routine
 * par 1:  subfunction
 * par 2-n:  depending on subfunction
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void mn5_readg( PARAM *par, STATUS *status );

/* reads traces from GRN files
 * par 1:  device
 * par 2:  start time
 * par 3:  length in seconds
 * par 4:  station list
 * par 5:  component list
 * par 6:  sample rate
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * int      *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void mn5_decimate( PARAM *par, STATUS *status );

/* decimates traces using mean values
 * 1. param:  trace list
 * 2. param:  integer decimation factor
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*--------------------------------------------------------------------*/


void mn5_sample( PARAM *par, STATUS *status );

/* returns sum (mean) of samples within a selected time window
 * par 1:  trace
 * par 2:  lo time
 * par 3:  hi time
 * par 4:  output value
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn5_mean( PARAM *par, STATUS *status );

/* returns moving average
 * par 1:  trace list
 * par 2:  no of samples
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn5_maximum( PARAM *par, STATUS *status );

/* computes a maximum trace and a value trace from a set of input
 * traces.  For each sample it takes the maximum amplitude of all
 * input traces as the new output sample on the maximum trace and
 * the value of a specified info entry on the value trace.
 *
 * par 1:       list of input traces
 * par 2:       info entry
 * par 3 & 4:   time window (optional)
 *
 * parameters of routine
 * PARAM      *par;          input; SH command parameters
 * STATUS     *status;       output; return status
 */


/*--------------------------------------------------------------------*/

#ifdef XXX
void mn5_set_shmsetup_pointer( SHT_GLOBAL_SET *ptr );
#endif

/* special routine to set pointer to global variable of motif modules
 *
 * parameters of routine
 * SHT_GLOBAL_SET *ptr;      input; specifies address of global paramters
 */


/*--------------------------------------------------------------------*/


void mn5_shmsetup( PARAM *par, STATUS *status );

/* setup routine for global parameters of motif modules
 * par 1:       item
 * par 2:       value
 *
 * parameters of routine
 * PARAM      *par;          input; SH command parameters
 * STATUS     *status;       output; return status
 */


/*--------------------------------------------------------------------*/


void mn5_replace( PARAM *par, STATUS *status );

/* replaces part of trace by another trace
 * par 1:       trace to be modified
 * par 2:       trace to be inserted
 *
 * parameters of routine
 * PARAM      *par;          input; SH command parameters
 * STATUS     *status;       output; return status
 */


/*--------------------------------------------------------------------*/


void mn5_findgap( PARAM *par, STATUS *status );

/* replaces part of trace by another trace
 * par 1:       trace list
 * par 2:       name of output file or TT
 *
 * parameters of routine
 * PARAM      *par;          input; SH command parameters
 * STATUS     *status;       output; return status
 */


/*--------------------------------------------------------------------*/


void mn5_fixgap( PARAM *par, STATUS *status );

/* removes  zeros from traces introducing a straight line 
	connecting non-zero values
 * par 1:    trace list
 * par 2:    value to be replaced (default 0)
 * par 3:    lo time bound
 * par 4:    hi time bound
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn5_set_external_routine( void (*ext_rout)(char cmd[],STATUS *status) );

/* sets external routine
 *
 * parameters of routine
 * void       (*ext_rout)(char cmd[],STATUS *status)   input; external routine
 */


/*--------------------------------------------------------------------*/


void mn5_external_routine( PARAM *par, STATUS *status );

/* calls external routine mn5v_external_routine
 *
 * parameters of routine
 * PARAM      *par;          input; SH command parameters
 * STATUS     *status;       output; return status
 */


/*--------------------------------------------------------------------*/



#endif  /* __FCTXMN5 */

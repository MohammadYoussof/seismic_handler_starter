
/* file FCTXMN3.H
 *      =========
 *
 * version 2, 22-MAY-2006
 *
 * prototypes of module SHMENU3.C
 * K. Stammler, 10-NOV-1990
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


#ifndef __FCTXMN3
#define __FCTXMN3

/*--------------------------------------------------------------------*/


void mn3_overlay( PARAM *par, STATUS *status );

/* overlay traces
 * 1. param:  trace list to be overlayed
 *
 * parameters of routine
 * PARAM      *par;     input; menu parameters
 * STATUS     *status   output; return status
 */


/*--------------------------------------------------------------------*/


void mn3_call( PARAM *par, STATUS *status );

/* special functions
 * 1. param:  name of function
 *
 * parameters of routine
 * PARAM      *par;     input; menu parameters
 * STATUS     *status   output; return status
 */


/*--------------------------------------------------------------------*/


void mn3_unit( PARAM *par, STATUS *status );

/* normalise traces to 1 (maximum amplitude of all traces in given window
 * is set to 1).
 * 1. param:  trace list
 * 2. param:  time window start
 * 3. param:  time window end
 *
 * parameters of routine
 * PARAM      *par;     input; menu parameters
 * STATUS     *status   output; return status
 */


/*----------------------------------------------------------------------------*/


void mn3_fold( PARAM *par, STATUS *status );

/* folds traces
 * par 1:    trace list to be folded
 * par 2, 3: time window
 * par 4:    filter trace
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void mn3_cut( PARAM *par, STATUS *status );

/* cuts traces
 * par 1:    trace list to be cut
 * par 2, 3: time window
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn3_mirror( PARAM *par, STATUS *status );

/* mirrors traces
 * par 1:    trace list to be mirrored
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn3_curve( PARAM *par, STATUS *status );

/* draws curves into display
 * par 1:    name of curve file
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn3_despike( PARAM *par, STATUS *status );

/* despikes traces
 * par 1:    trace list
 * par 2:    detection factor
 * par 3:    lo time bound
 * par 4:    hi time bound
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn3_mend( PARAM *par, STATUS *status );

/* mends trace
 * par 1:    trace
 * par 2:    order
 * par 3:    lo time bound
 * par 4:    hi time bound
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn3_spikefil( PARAM *par, STATUS *status );

/* computes spiking filter
 * par 1:    trace
 * par 2:    lo time bound
 * par 3:    hi time bound
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


void mn3_arp( PARAM *par, STATUS *status );

/* autoregressive process
 * par 1:               trace list
 * par 2:               order of process N
 * par 3 .. 3+N-1:      process coefficients
 * par 3+N .. 3+2*N-1:  start values (default 0.)
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------*/


#endif /* __FCTXMN3 */


/* file FCT_MNX.H
 *      =========
 *
 * version 2, 22-MAY-2006
 *
 * prototypes of module SHMENUX.C
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

#ifndef __FCTXMNX
#define __FCTXMNX

#ifndef __CPUSRDEF
#include "cpusrdef.h"
#endif


/*----------------------------------------------------------------------------*/


void mnx_locate( PARAM *par, int *status );

/* locates events using arrival times at array stations
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  trace list
 * 2. param:  azimuth variable
 * 3. param:  slowness variable
 * 4. param:  azimuth-error variable
 * 5. param:  slowness-error variable
 */


/*----------------------------------------------------------------------------*/


void mnx_beam( PARAM *par, int *status );

/* shift traces according to their station location and given azimuth
 * and slowness
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  trace list
 * 2. param:  azimuth
 * 3. param:  slowness
 */


/*----------------------------------------------------------------------------*/


void mnx_shift( PARAM *par, int *status );

/* shift traces
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  trace list
 * 2. param:  time (explicit shift)
 */


/*-------------------------------------------------------------------------*/


void mnx_al( PARAM *par, int *status );

/* align traces
 * 1. param:  time position (default 0)
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void mnx_syw( PARAM *par, int *status );

/* sets y-window in display
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*-------------------------------------------------------------------------*/


void mnx_styw( PARAM *par, int *status );

/* sets time & y-window in display
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void mnx_yinfo( PARAM *par, STATUS *status );

/* sets y-info entry in display
 * 1. param:   info entry name
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * STATUS   *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void mnx_am( PARAM *par, int *status );

/* determine amplitudes
 * 1. param:  trace
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void mnx_resample( PARAM *par, int *status );

/* resamples traces
 * 1. param:  trace list
 * 2. param:  new sample distance (in sec)
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void mnx_title( PARAM *par, int *status );

/* sets new title
 * 1. param:  title line
 * 2. param:  text of title
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void mnx_hc( PARAM *par, int *status );

/* makes hardcopy of window
 * 1. param:  window number (channel map)
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void mnx_pmch( PARAM *par, int *status );

/* selects window for particle motion diagrams
 * 1. param:  window number (channel map)
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void mnx_pm( PARAM *par, int *status );

/* draws particle motion diagrams
 * 1. param: trace list
 * 2. param: lo bound of time window [FIXED]
 * 3. param: hi bound of time window [FIXED]
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */


/*----------------------------------------------------------------------------*/

#endif /* __FCTXMNX */

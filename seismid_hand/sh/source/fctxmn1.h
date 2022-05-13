
/* file FCTXMN1.H
 *      =========
 *
 * version 2, 22-MAY-2006
 *
 * prototypes of module SHMENU1.C
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


#ifndef __FCTXMN1
#define __FCTXMN1

#ifndef __CPUSRDEF
#include "cpusrdef.h"
#endif

/*------------------------------------------------------------------------*/


void mn1_zoom( PARAM *par, int *status );

/* zoom amplitudes */

/* parameters of routine */
/* PARAM    *par;           input; name & parameters */
/* int      *status;        output; return status */


/*-------------------------------------------------------------------------*/


void mn1_set( PARAM *par, int *status );

/* changes info values of traces */

/* parameters of routine */
/* PARAM    *par;           input; name & parameters */
/* int      *status;        output; return status */


/*-------------------------------------------------------------------------*/



void mn1_stw( PARAM *par, int *status );

/* sets time window in display */

/* parameters of routine */
/* PARAM    *par;           input; name & parameters */
/* int      *status;        output; return status */


/*-------------------------------------------------------------------------*/


void mn1_create( PARAM *par, int *status );

/* creates a synthetic trace */

/* parameters of routine */
/* PARAM    *par;           input; name & parameters */
/* int      *status;        output; return status */

/* 1. par:  type of trace */
/* 2. par:  sample distance */
/* 3. par:  length of trace in seconds */
/* 4. par:  max. amplitude */
/* 5. par:  ... */


/*-------------------------------------------------------------------------*/


void mn1_del( PARAM *par, int *status );

/* deletes traces */

/* parameters of routine */
/* PARAM    *par;           input; name & parameters */
/* int      *status;        output; return status */


/*-------------------------------------------------------------------------*/


void mn1_trctxtp( PARAM *par, int *status );

/* sets position of trace info text */

/* parameters of routine */
/* PARAM    *par;           input; name & parameters */
/* int      *status;        output; return status */

/* 1. param:  x-position factor, units of display width */
/* 2. param:  y-position factor, units of display height */


/*-------------------------------------------------------------------------*/


void mn1_copy( PARAM *par, int *status );

/* copies one or more traces
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  trace list
 */


/*-------------------------------------------------------------------------*/


void mn1_rot( PARAM *par, int *status );

/* rotates 2 or 3 traces
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  trace list
 * if 2-dim
 *    2. param:  angle
 *    3. param:  lo-wdw
 *    4. param:  hi-wdw
 * if 3-dim
 *    2. param:  azimuth
 *    3. param:  angle of incidence
 *    4. param:  lo-wdw
 *    5. param:  hi-wdw
 */


/*-------------------------------------------------------------------------*/


void mn1_norm( PARAM *par, int *redraw, int *status );

/* selects type of normalisation
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *redraw;        output; redraw display
 * int      *status;        output; return status
 *
 * 1. param:  normalisation type
 */


/*-------------------------------------------------------------------------*/


void mn1_rd( PARAM *par, int *rdlevel, int *status );

/* selects redraw level
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *rdlevel;       output; redraw level
 * int      *status;        output; return status
 *
 * 1. param:  force redraw
 */


/*-------------------------------------------------------------------------*/


void mn1_sum( PARAM *par, int *status );

/* sums traces
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  trace list
 * 2. param:  lo-wdw
 * 3. param:  hi-wdw
 */


/*-------------------------------------------------------------------------*/


void mn1_entry( PARAM *par, int *status );

/* entry manipulations
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  keyword
 * 2. param:  ...
 * 3. param:  ...
 */


/*-------------------------------------------------------------------------*/

#endif /* __FCTXMN1 */

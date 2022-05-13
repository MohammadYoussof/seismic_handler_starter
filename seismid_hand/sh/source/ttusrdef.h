
/* file TTUSRDEF.H
 *      ==========
 *
 * version 5, 22-May-2006
 *
 * user interface of module TRAVTIME
 * K. Stammler, 15-SEP-1990
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


#ifndef __TTUSRDEF
#define __TTUSRDEF

/* seismic phases */
#define TTC_PHASE_P 1
#define TTC_PHASE_S 2
#define TTC_PHASE_pP 3
#define TTC_PHASE_sP 4
#define TTC_PHASE_pS 5
#define TTC_PHASE_sS 6

#define TTC_P_S_DIFF   101
#define TTC_P_Ppp_DIFF 102
#define TTC_P_Pps_DIFF 103
#define TTC_P_S_xDIFF  104


/*---------------------------------------------------------------------*/


void tt_read_mantle_vel( char velfile[], STATUS *status );

/* reads velocity distribution
 *
 * parameters of routine
 * char       velfile[];      input; name of velocity file
 * STATUS     *status;        output; return status
 */


/*---------------------------------------------------------------------*/


void tt_change_mantle_vel( BOOLEAN change_p, float maxdepth,
	float fac, STATUS *status );

/* multiplies either all P-velocities (change_p=TRUE) or all
 * S-velocities (change_p=FALSE) with a constant factor "fac"
 *
 * parameters of routine
 * BOOLEAN    change_p;    input; change P-velocities (TRUE) or S (FALSE)
 * float      maxdepth;    input; maximum depth to be changed
 * float      fac;         input; multiplication factor
 * STATUS     *status;     output; return status
 */


/*---------------------------------------------------------------------*/


float tt_cart_psdiff( float convdepth, float slowness, STATUS *status );

/* computes travel time difference of P and S through velocity
 * layers from "convepth" km to surface.
 *
 * parameters of routine
 * float      convdepth;     input; conversion depth (in km)
 * float      slowness;      input; slowness of wave (in sec/degree)
 * STATUS     *status;       output; return status
 *                           returns travel time difference (in sec)
 */


/*---------------------------------------------------------------------*/


float tt_mantle_diff( int diff_phase, float convdepth, float slowness,
	STATUS *status );

/* computes travel time difference of a phase "diff_phase" through velocity
 * layers from "convdepth" km to surface. (new version)
 *
 * parameters of routine
 * int        diff_phase;    input; phase difference ID
 * float      convdepth;     input; conversion depth (in km)
 * float      slowness;      input; slowness of wave (in sec/degree)
 * STATUS     *status;       output; return status
 *                           returns travel time difference (in sec)
 */


/*-------------------------------------------------------------------*/


float tt_travel( int phase, float distance, float depth,
	STATUS *status );

/* returns travel time in seconds for different phases
 *
 * parameters of routine
 * int        phase;       input; phase ID
 * float      distance;    input; distance of source in degrees
 * float      depth;       input; depth of source in km
 * STATUS     *status;     output; return status
 *                         returns travel time of phase in sec
 */


/*-------------------------------------------------------------------*/


float tt_slowness( int phase, float distance, float depth,
	STATUS *status );

/* returns slowness of "phase"
 *
 * parameters of routine
 * int        phase;     input; phase ID
 * float      distance;  input; distance of source in degrees
 * float      depth;     input; depth of source in km
 * STATUS     *status;   output; return status
 *                       returns slowness of phase in sec/degree
 */


/*-------------------------------------------------------------------*/


void tt_settable( char tablename[], STATUS *status );

/* sets new table name
 *
 * parameters of routine
 * char       tablename[];   input; new file name of tt table
 * STATUS     *status;       output; return status
 */


/*---------------------------------------------------------------------*/

#endif /* __TTUSRDEF */

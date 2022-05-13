
/* file PTUSRDEF.H
 *      ==========
 *
 * version 6, 22-May-2006
 *
 * prototypes of module PTRAVTIM.C
 * K. Stammler, 18-Dec-91
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


#ifndef __PTUSRDEF
#define __PTUSRDEF


/*-------------------------------------------------------------------*/


void pt_settabledir( char tabdir[], STATUS *status );

/* sets directory of table files
 *
 * parameters of routine
 * char       tabdir[];    input; new directory
 * STATUS     *status;     output; return status
 */


/*-------------------------------------------------------------------*/


float pt_travel( char phase[], float distance, float depth,
	STATUS *status );

/* returns travel time of phase "phase"
 *
 * parameters of routine
 * char       phase[];      input; phase to compute travel time
 * float      distance;     input; distance of event in degrees
 * float      depth;        input; depth of event in km
 * STATUS     *status;      output; return status
 */


/*-------------------------------------------------------------------*/


float pt_slowness( char phase[], float distance, float depth,
	STATUS *status );

/* returns slowness of "phase"
 *
 * parameters of routine
 * char       phase[];   input; name of phase
 * float      distance;  input; distance of source in degrees
 * float      depth;     input; depth of source in km
 * STATUS     *status;   output; return status
 *                       returns slowness of phase in sec/degree
 */


/*-------------------------------------------------------------------*/


float pt_distance( char phase[], float slowness, float depth,
	STATUS *status );

/* determines distance in degrees of a phase from slowness in sec/deg
 *
 * parameters of routine
 * char       phase[];      input; phase name
 * float      slowness;     input; slowness in sec/deg
 * float      depth;        input; depth in km
 * STATUS     *status;      output; return status
 *                          returns distance in degrees or zero
 */


/*-------------------------------------------------------------------*/


float pt_depth( char phase1[], char phase2[], float tdiff,
	float distance, STATUS *status );

/* fits depth to given time difference "tdiff" of two phases
 * "T(phase2)-T(phase1)" at a fixed distance
 *
 * parameters of routine
 * char       phase1[];       input; name of main phase
 * char       phase2[];       input; name of corresponding depth phase
 * float      tdiff;          input; time difference between phases (sec)
 * float      distance;       input; distance in deg
 * STATUS     *status;        output; return status
 *                            returns fitted depth in km
 */


/*-------------------------------------------------------------------*/


float pt_distance_pd( char phase1[], char phase2[], float tdiff,
	float depth, STATUS *status );

/* fits distance to given time difference "tdiff" of two phases
 * "T(phase2)-T(phase1)" at a fixed depth
 *
 * parameters of routine
 * char       phase1[];       input; name of first phase
 * char       phase2[];       input; name of second phase
 * float      tdiff;          input; time difference between phases (sec)
 * float      depth;          input; depth in km
 * STATUS     *status;        output; return status
 *                            returns fitted distance in deg
 */


/*-------------------------------------------------------------------*/


#endif /* __PTUSRDEF */

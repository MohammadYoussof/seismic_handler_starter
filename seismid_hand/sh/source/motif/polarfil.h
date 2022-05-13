
/* file polarfil.h
 *      ==========
 *
 * version 3, 22-May-2006
 *
 * Header file for module polarfil.c
 * K. Stammler, 29-Oct-2005
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



/* constants */
#define POL_ID_COHLTH 1
#define POL_ID_POW_LINFIL 2



/*----------------------------------------------------------------------------*/


void pol_initialize( Widget parent, Widget pmwdw, TSyStatus *status );

/* initializes polarisation filter window
 *
 * parameters of routine
 * Widget     parent;      input; widget ID of root window
 * Widget     pmwdw;       input; Widget ID of polarisation filter window
 * TSyStatus  *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void pol_close_window( void );

/* Closes polarisation filter window
 *
 * parameters of routine
 * none
 */


/*----------------------------------------------------------------------------*/


void pol_draw_poltraces( float defzoom, TSyStatus *status );

/* Draws polarisation filtered traces into separate window
 *
 * parameters of routine
 * float      defzoom;        input; default zoom value for display
 * TSyStatus  *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void pol_incpar( int parid, int inc );

/* increments parameter
 *
 * parameter of routine
 * int       parid;      input; parameter ID
 * int       inc;        input; increment value
 */


/*----------------------------------------------------------------------------*/

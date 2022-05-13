
/* file partmotion.h
 *      ============
 *
 * version 2, 22-Nov-2006
 *
 * Header file for module partmotion.c
 * K. Stammler, 16-Nov-98
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
#define cPmmModeUndefined    0
#define cPmmModeLineColored  1
#define cPmmModeLineMono     2
#define cPmmModePlot1        3
#define cPmmModePlot3        4
#define cPmmModeZoomOn       5
#define cPmmModeZoomOff      6




/*----------------------------------------------------------------------------*/


void pmm_initialize( Widget parent, Widget pmwdw, TSyStatus *status );

/* initializes particle motion window
 *
 * parameters of routine
 * Widget     parent;      input; widget ID of root window
 * Widget     pmwdw;       input; Widget ID of particle motion window
 * TSyStatus  *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void pmm_close_window( void );

/* Closes particle motion window
 *
 * parameters of routine
 * none
 */


/*----------------------------------------------------------------------------*/


void pmm_draw_pmotion( float defzoom );

/* Draws particle motion diagram(s) into separate window
 *
 * parameters of routine
 * float      defzoom;        input; default zoom value for display
 */


/*----------------------------------------------------------------------------*/


void pmm_set_mode( int mode );

/* Configuring some parameters
 *
 * parameters of routine
 * int        mode;         input; mode to set
 */


/*----------------------------------------------------------------------------*/


/* file spectrum.h
 *      ==========
 *
 * version 9, 18-Oct-2006
 *
 * Header file for spectrum module
 * K. Stammler, 1-Sep-98
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

/* error codes */
#define SPE_OFFSET 5300
#define SPE_STROVFL    (SPE_OFFSET+1)   /* string overflow */
#define SPE_OPENREAD   (SPE_OFFSET+2)   /* error opening input file */
#define SPE_SPGSHORT   (SPE_OFFSET+3)   /* spectrogram input trace too short */
#define SPE_OPENWRITE  (SPE_OFFSET+4)   /* error opening input file */


/* constants */
#define cSpcUnchanged 0
#define cSpcAutoscale 1
#define cSpcFrqLoUp 2
#define cSpcFrqLoDown 3
#define cSpcFrqHiUp 4
#define cSpcFrqHiDown 5
#define cSpcAmpLoUp 6
#define cSpcAmpLoDown 7
#define cSpcAmpHiUp 8
#define cSpcAmpHiDown 9
#define cSpcResized 10

#define cSpcModePowSpc 1
#define cSpcModeFft 2
#define cSpcModeFft2 3



/*----------------------------------------------------------------------------*/


void spc_initialize( Widget parent, Widget spcwdw, STATUS *status );

/* initializes spectrum window
 *
 * parameters of routine
 * Widget     parent;      input; widget ID of root window
 * Widget     spcwdw;      input; Widget ID of spectrum window
 * STATUS     *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void spc_close_window( void );

/* Closes spectrum window
 *
 * parameters of routine
 * none
 */


/*----------------------------------------------------------------------------*/


void spc_insert_spectrum( Widget spcwdw, TSyStatus *status );

/* Plots new spectrum to window
 *
 * parameters of routine
 * Widget     spcwdw;        input; widget ID of spectrum drawing area
 * TSyStatus; *status;       output; return status
 */


/*----------------------------------------------------------------------------*/


void spc_change_display( int mode, TSyStatus *status );

/* Changes display
 *
 * parameters of routine
 * int        mode;        input; which change
 * TSyStatus  *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void spc_recompute_spectra( int mode, TSyStatus *status );

/* Recomputes spectra of all traces with new mode.
 *
 * parameters of routine
 * int        mode;        input; computation method
 * TSyStatus  *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void spc_get_dialog_values( Widget w[], TSyStatus *status );

/* Gets parameters from dialog box
 *
 * parameters of routine
 * Widget        w[];       input; widget array
 * TSyStatus     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void spc_set_dialog_values( Widget w[] );

/* Gets parameters from dialog box
 *
 * parameters of routine
 * Widget        w[];       input; widget array
 */


/*----------------------------------------------------------------------------*/


void spc_handle_xevent( Widget w, XEvent *ev, TSyStatus *status );

/* Handles X events in spectrum window
 *
 * parameters of routine
 * Widget     w;       input; widget
 * XEvent     *ev;     input; event reported
 * TSyStatus  *status; output; return status
 */


/*----------------------------------------------------------------------------*/


void spc_start_fitline( void );

/* Next two mouse clicks are left and right window border for line fitting
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void spc_spectrogram( TSyStatus *status );

/* Writes spectrogram data to file and calls display program
 *
 * parameters of routine
 * Widget     spcwdw;        input; widget ID of spectrum drawing area
 */


/*----------------------------------------------------------------------------*/

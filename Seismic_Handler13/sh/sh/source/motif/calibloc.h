
/* file calibloc.h
 *      ==========
 *
 * version 5, 22-May-2006
 *
 * header file of module calibloc.c
 * K. Stammler, 23-Sep-93
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



/* error cods */
#define CALE_OFFSET           4900
#define CALE_STROVFL          (CALE_OFFSET+1)    /* string overflow */
#define CALE_OPENREAD         (CALE_OFFSET+2)    /* error opening input file */
#define CALE_NOCALIBFILE      (CALE_OFFSET+3)    /* no calibration file found */
#define CALE_READLOOKUP       (CALE_OFFSET+4)    /* error reading lookup file */
#define CALE_READCALIB        (CALE_OFFSET+5)    /* error reading calib file */
#define CALE_EMPTYCAL         (CALE_OFFSET+6)    /* empty calibration file */


/* prototypes of module */

/*----------------------------------------------------------------------------*/


void cal_initialize( Widget parent, Widget calwdw, STATUS *status );

/* initializes calibration window
 *
 * parameters of routine
 * Widget     parent;      input; widget ID of root window
 * Widget     calwdw;      input; Widget ID of calibration window
 * STATUS     *status;     output; return status
 */


/*----------------------------------------------------------------------------*/



void cal_display_calib( Widget w, CUT_PARAMS *par, STATUS *status );

/* displays calibration vectors for within a
 * window centered on given slowness and azimuth.  The analysis parameters
 * are copied to a local data structure.
 *
 * parameters of routine
 * Widget     w;           input; widget ID of calibration window
 * CUT_PARAMS *par;        input; analysis parameters
 * STATUS     *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void cal_handle_xevent( Widget w, XEvent *ev );

/* handles events of calibration window
 *
 * parameters of routine
 * Widget     w;         input; widget ID of calibration window
 * XEvent     *ev;       input; event happened
 */


/*----------------------------------------------------------------------------*/



void cal_accept_values( Widget w, CUT_PARAMS *par );

/* puts corrected values of slowness and azimuth as well as location
 * parameters to passed structure and
 * terminates calibration window.
 *
 * parameters of routine
 * Widget     w;          input; widget ID of calibration window
 * CUT_PARAMS *par;       output; analysis parameters
 */


/*----------------------------------------------------------------------------*/


/* file pixmaps.h
 *      =========
 *
 * version 7, 22-May-2006
 *
 * header file of module pixmaps.c
 * K. Stammler, 21-May-93
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


#include <X11/X.h>


/* error numbers */
#define PIXE_OFFSET 4100
#define PIXE_LINKEDIN      (PIXE_OFFSET+1)    /* already linked in */
#define PIXE_ILLWDW        (PIXE_OFFSET+2)    /* illegal window */


/* constants */
#define PIXC_ILLEGAL_WDW 0
	/* illegal window ID */

#define PIXC_COL_FOREGROUND 1
#define PIXC_COL_BACKGROUND 2



/*--------------------------------------------------------------------------*/


void pix_create_window_buffer( Display *dsp, Window wdw, BOOLEAN on,
	STATUS *status );

/* creates window backing store buffer
 *
 * parameters of routine
 * Display    *dsp;      input; pointer to display
 * Window     wdw        input; window to back up
 * BOOLEAN    on;        input; switch backing store on
 */


/*--------------------------------------------------------------------------*/


void pix_resize_window_buffer( Display *dsp, Window wdw, STATUS *status );

/* Resizes existing pixmap for backing store
 *
 * parameters of routine
 * Display    *dsp;      input; display used
 * Window     *wdw;      input; window with changed size
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------------*/


void pix_manage_exposure( XExposeEvent *ev );

/* restores window on screen
 *
 * parameters of routine
 * XExposeEvent  *ev;        input; event descriptor
 */


/*--------------------------------------------------------------------------*/


void pix_ClearWindow( Display *dsp, Window wdw );

/* clears window
 *
 * parameters of routine
 * Display    *dsp;       input; pointer to display
 * Window     wdw;        input; window ID
 */


/*--------------------------------------------------------------------------*/


void pix_DrawLine( Display *dsp, Window wdw, GC gc, int x1, int y1,
	int x2, int y2 );

/* draws a line on screen and off-screen resources if initialized
 *
 * parameters of routine
 * Display    *dsp;        input; pointer to display
 * Window     wdw;         input; window on screen
 * GC         gc;          input; graphics context
 * int        x1,y1,x2,y2; input; line start and end
 */


/*--------------------------------------------------------------------------*/


void pix_DrawLines( Display *dsp, Window wdw, GC gc, XPoint pxy[],
	int no_of_points, int mode );

/* draws multiple lines on screen and off-screen resources if initialized
 *
 * parameters of routine
 * Display    *dsp;        input; pointer to display
 * Window     wdw;         input; window on screen
 * GC         gc;          input; graphics context
 * XPoint     pxy[];       input; array of coordinates
 * int        no_of_points;input; number of points
 * int        mode;        input; coordinate mode
 */


/*--------------------------------------------------------------------------*/


void pix_DrawString( Display *dsp, Window wdw, GC gc, int x, int y,
	char str[], int lth );

/* draws string on screen and off-screen resources if initialized
 *
 * parameters of routine
 * Display    *dsp;        input; pointer to display
 * Window     wdw;         input; window on screen
 * GC         gc;          input; graphics context
 * int        x, y;        input; string position
 * char       str[];       input; string to draw
 * int        lth;         input; string length
 */


/*--------------------------------------------------------------------------*/


void pix_FillRectangle( Display *dsp, Window wdw, GC gc, int x, int y,
	unsigned width, unsigned height );

/* fills rectangle on screen and off-screen resources if initialized
 *
 * parameters of routine
 * Display    *dsp;          input; pointer to display
 * Window     wdw;           input; window on screen
 * GC         gc;            input; graphics context
 * int        x, y;          input; position
 * unsigned   width, height; input; width and height of rectangle
 */


/*--------------------------------------------------------------------------*/


void pix_set_max_drawlth( int max_drawlth );

/* Sets maximum number of samples to be plotted in one XDrawLines call.
 *
 * parameters of routine
 * int        max_drawlth;        input; maximum number of samples
 */


/*--------------------------------------------------------------------------*/


void pix_DrawArc( Display *dsp, Window wdw, GC gc, int x, int y,
	unsigned width, unsigned height, int angle1, int angle2 );

/* draws elliptical arc
 *
 * parameters of routine
 * Display    *dsp;          input; pointer to display
 * Window     wdw;           input; window on screen
 * GC         gc;            input; graphics context
 * int        x, y;          input; arc position
 * unsigned   width, height; input; width and height of arc
 * int        angle1, angle2;input; start and end angle in deg * 64
 */

/*--------------------------------------------------------------------------*/


void pix_SetClipRectangles( Display *dsp, GC gc, int xo, int yo,
	XRectangle rect[], int n, int ordering );

/* Sets clipping rectangle
 *
 * parameters of routine
 * Display       *dsp;       input; display
 * GC            gc;         input; graphics context to modify
 * int           xo, yo;     input; origin of clip map
 * XRectangle    rect[];     input; list of rectangles
 * int           n;          input; number of rectangles
 * int           ordering;   input; ordering of rectangles
 */


/*----------------------------------------------------------------------------*/


unsigned long pix_colour( int type );

/* returns colour value for foreground and background
 *
 * parameters of routine
 * int        type; input; type of colour
 */


/*--------------------------------------------------------------------------*/


/* file pixmaps.c
 *      =========
 *
 * version 14, 22-May-2006
 *
 * off-screen resources (for window restore and hardcopy options)
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


#include <stdio.h>
#include "basecnst.h"
#include "globalparams.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include <Xm/Xm.h>
#undef BC_DEFINE_TRUE_FALSE
#include BC_SYSBASE
#include "pixmaps.h"

#define ALL_GC_VALUES (GCFunction|GCPlaneMask|GCForeground|GCBackground\
	|GCLineWidth|GCLineStyle|GCCapStyle|GCJoinStyle|GCFillStyle|GCFillRule\
	|GCTile|GCStipple|GCTileStipXOrigin|GCTileStipYOrigin|GCFont\
	|GCSubwindowMode|GCGraphicsExposures|GCClipXOrigin|GCClipYOrigin\
	|GCClipMask|GCDashOffset|GCDashList|GCArcMode)


/* types */

typedef struct pixt_windowchan {
	Window    wdw;        /* window ID */
	Pixmap    restore;    /* associated backing store pixmap */
	Pixmap    hc;         /* hardcopy pixmap */
	BOOLEAN   restore_on; /* do backing store */
	BOOLEAN   hc_on;      /* do hardcopy output */
	struct pixt_windowchan *next; /* next structure */
} PIXT_WINDOWCHAN;



/* global variables */
PIXT_WINDOWCHAN *pixv_root=NULL;     /* root pointer */
GC              pixv_copy_gc;        /* graphics context for copying */
GC              pixv_clear_gc;       /* graphics context for clearing areas */
BOOLEAN         pixv_copy_gc_init=FALSE;   /* GC initialized */
int             pixv_max_drawlth=4096;  /* maximum length of DrawLines array */
int             pixv_coloured;       /* display coloured? */
static Colormap pixv_cmap;           /* colour map */
static unsigned long pixv_col_fg;    /* foreground pixels */
static unsigned long pixv_col_bg;    /* background pixels */



/* prototypes of local routines */
static PIXT_WINDOWCHAN *pix_getchan( Window w );
static void pix_init_gc( Display *dsp, Window wdw );



/*--------------------------------------------------------------------------*/



void pix_create_window_buffer( Display *dsp, Window wdw, BOOLEAN on,
	STATUS *status )

/* creates window backing store buffer
 *
 * parameters of routine
 * Display    *dsp;      input; pointer to display
 * Window     wdw        input; window to back up
 * BOOLEAN    on;        input; switch backing store on
 */
{
	/* local variables */
	Window   root_wdw;          /* root window */
	int      x, y;              /* window position */
	unsigned w, h;              /* window coo's */
	unsigned border;            /* border width of window */
	unsigned depth;             /* depth of window */
	PIXT_WINDOWCHAN   *chan;    /* channel pointer */

	/* executable code */

	chan = pix_getchan( wdw );
	if  (chan != NULL)  {
		if  (chan->restore != PIXC_ILLEGAL_WDW)  {
			*status = PIXE_LINKEDIN;
			return;
		} /*endif*/
	} else {
		/* link in */
		if  (pixv_root == NULL)  {
			pixv_root = (PIXT_WINDOWCHAN *)sy_allocmem(
				1, (int)sizeof(PIXT_WINDOWCHAN), status );
			if  (Severe(status))  return;
			chan = pixv_root;
		} else {
			chan = pixv_root;
			while  (chan->next != NULL)
				chan = chan->next;
			chan->next = (PIXT_WINDOWCHAN *)sy_allocmem(
				1, (int)sizeof(PIXT_WINDOWCHAN), status );
			if  (Severe(status))  return;
			chan = chan->next;
		} /*endif*/
		chan->wdw = wdw;
		chan->restore = PIXC_ILLEGAL_WDW;
		chan->restore_on = FALSE;
		chan->hc = PIXC_ILLEGAL_WDW;
		chan->hc_on = FALSE;
		chan->next = NULL;
	} /*endif*/

	/* create pixmap */
	XGetGeometry( dsp, wdw, &root_wdw, &x,&y,&w,&h, &border, &depth );
	chan->restore = XCreatePixmap( dsp, wdw, w, h, depth );
	chan->restore_on = TRUE;

	if  (!pixv_copy_gc_init)  pix_init_gc( dsp, wdw );
	XFillRectangle( dsp, chan->restore, pixv_clear_gc, 0, 0, w, h );

} /* end of pix_create_window_buffer */



/*--------------------------------------------------------------------------*/



void pix_resize_window_buffer( Display *dsp, Window wdw, STATUS *status )

/* Resizes existing pixmap for backing store
 *
 * parameters of routine
 * Display    *dsp;      input; display used
 * Window     *wdw;      input; window with changed size
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	PIXT_WINDOWCHAN   *chan;    /* channel pointer */
	Window   root_wdw;          /* root window */
	int      x, y;              /* window position */
	unsigned w, h;              /* window coo's */
	unsigned border;            /* border width of window */
	unsigned depth;             /* depth of window */

	/* executable code */

	chan = pix_getchan( wdw );
	if  (chan == NULL)  {
		*status = PIXE_ILLWDW;
		return;
	} /*endif*/

	/* delete old pixmap */
	XFreePixmap( dsp, chan->restore );

	/* create new one */
	XGetGeometry( dsp, wdw, &root_wdw, &x,&y,&w,&h, &border, &depth );
	chan->restore = XCreatePixmap( dsp, wdw, w, h, depth );

} /* end of pix_resize_window_buffer */



/*--------------------------------------------------------------------------*/



static PIXT_WINDOWCHAN *pix_getchan( Window w )

/* returns window channels (or NULL)
 *
 * parameter of routine
 * Window    w;       input; window to look for
 */
{
	/* local variables */
	PIXT_WINDOWCHAN *ptr;   /* moving pointer */

	/* executable code */

	ptr = pixv_root;
	while  (ptr != NULL)  {
		if  (ptr->wdw == w)  return ptr;
		ptr = ptr->next;
	} /*endwhile*/
	return NULL;

} /* pix_getchan */



/*--------------------------------------------------------------------------*/



static void pix_init_gc( Display *dsp, Window wdw )

/* initializes copy GC pixv_copy_gc (and flag pixv_copy_gc_init)
 *
 * no parameters
 */
{
	/* local variables */
	XColor   col;            /* X colour */
	int      xstatus;        /* X return status */
	Screen   *s;              /* X screen */

	/* executable code */

	s = XDefaultScreenOfDisplay( dsp );
	pixv_coloured = ((XDefaultVisualOfScreen(s))->class == TrueColor
		|| (XDefaultVisualOfScreen(s))->class == PseudoColor
		|| (XDefaultVisualOfScreen(s))->class == DirectColor
		|| (XDefaultVisualOfScreen(s))->class == StaticColor);
	if  (pixv_coloured)  {
		pixv_cmap = XDefaultColormapOfScreen( s );
		col.red = Nint( GpGetFloat(cGpF_colour_bg_red) * 65535.0 );
		col.green = Nint( GpGetFloat(cGpF_colour_bg_green) * 65535.0 );
		col.blue = Nint( GpGetFloat(cGpF_colour_bg_blue) * 65535.0 );
		col.flags = DoRed | DoGreen | DoBlue;
		xstatus = XAllocColor( dsp, pixv_cmap, &col );
		if  (xstatus == 0)  {
			pixv_col_bg = XWhitePixelOfScreen(s);
		} else {
			pixv_col_bg = col.pixel;
		} /*endif*/
		col.red = Nint( GpGetFloat(cGpF_colour_fg_red) * 65535.0 );
		col.green = Nint( GpGetFloat(cGpF_colour_fg_green) * 65535.0 );
		col.blue = Nint( GpGetFloat(cGpF_colour_fg_blue) * 65535.0 );
		col.flags = DoRed | DoGreen | DoBlue;
		xstatus = XAllocColor( dsp, pixv_cmap, &col );
		if  (xstatus == 0)  {
			pixv_col_fg = XBlackPixelOfScreen(s);
		} else {
			pixv_col_fg = col.pixel;
		} /*endif*/
	} else {
		pixv_col_bg = XWhitePixelOfScreen(s);
		pixv_col_fg = XBlackPixelOfScreen(s);
	} /*endif*/

	pixv_copy_gc = XCreateGC( dsp, wdw, 0, NULL );
	XCopyGC( dsp, XDefaultGCOfScreen(XDefaultScreenOfDisplay(dsp)),
		ALL_GC_VALUES, pixv_copy_gc );
	XSetForeground( dsp, pixv_copy_gc, pixv_col_fg );
	XSetBackground( dsp, pixv_copy_gc, pixv_col_bg );
	XSetFunction( dsp, pixv_copy_gc, GXcopy );

	pixv_clear_gc = XCreateGC( dsp, wdw, 0, NULL );
	XCopyGC( dsp, pixv_copy_gc, ALL_GC_VALUES, pixv_clear_gc );
	XSetForeground( dsp, pixv_clear_gc, pixv_col_bg );
	XSetBackground( dsp, pixv_clear_gc, pixv_col_fg );

	pixv_copy_gc_init = TRUE;

} /* end of pix_init_gc */



/*--------------------------------------------------------------------------*/



void pix_manage_exposure( XExposeEvent *ev )

/* restores window on screen
 *
 * parameters of routine
 * XExposeEvent  *ev;        input; event descriptor
 */
{
	/* local variables */
	PIXT_WINDOWCHAN  *chan;   /* window channels */

	/* executable code */

	chan = pix_getchan( ev->window );
	if  (chan == NULL)  {
		printf( "*SHM: pix_manage_exposure: no window channels ***\n" );
		return;
	} else if  (chan->restore == PIXC_ILLEGAL_WDW)  {
		printf( "*SHM: pix_manage_exposure: no restore channel ***\n" );
		return;
	} /*endif*/

	if  (!pixv_copy_gc_init)  pix_init_gc( ev->display, ev->window );

	XCopyArea( ev->display, chan->restore, ev->window, pixv_copy_gc,
		ev->x, ev->y, ev->width, ev->height, ev->x, ev->y );

} /* end of pix_manage_exposure */



/*--------------------------------------------------------------------------*/



void pix_ClearWindow( Display *dsp, Window wdw )

/* clears window
 *
 * parameters of routine
 * Display    *dsp;       input; pointer to display
 * Window     wdw;        input; window ID
 */
{
	/* local variables */
	Window   root_wdw;          /* root window */
	int      x, y;              /* window position */
	unsigned w, h;              /* window coo's */
	unsigned border;            /* window border width */
	unsigned depth;             /* depth of window */
	PIXT_WINDOWCHAN   *chan;    /* window channels */

	/* executable code */

	/* clear window on screen */
	XClearWindow( dsp, wdw );

	/* check for off-screen resources */
	chan = pix_getchan( wdw );
	if  (chan == NULL)  return;

	if  (!pixv_copy_gc_init)  pix_init_gc( dsp, wdw );
	if  (chan->restore_on)  {
		XGetGeometry( dsp, wdw, &root_wdw, &x,&y,&w,&h,
			&border, &depth );
		XFillRectangle( dsp, chan->restore, pixv_clear_gc, 0, 0, w, h );
	} /*endif*/
	if  (chan->hc_on)  {
		XGetGeometry( dsp, chan->hc, &root_wdw, &x,&y,&w,&h,
			&border, &depth );
		XFillRectangle( dsp, chan->hc, pixv_clear_gc, 0, 0, w, h );
	} /*endif*/

} /* end of pix_ClearWindow */



/*--------------------------------------------------------------------------*/



void pix_DrawLine( Display *dsp, Window wdw, GC gc, int x1, int y1,
	int x2, int y2 )

/* draws a line on screen and off-screen resources if initialized
 *
 * parameters of routine
 * Display    *dsp;        input; pointer to display
 * Window     wdw;         input; window on screen
 * GC         gc;          input; graphics context
 * int        x1,y1,x2,y2; input; line start and end
 */
{
	/* local variables */
	PIXT_WINDOWCHAN   *chan;    /* window channels */

	/* executable code */

	/* draw on screen */
	XDrawLine( dsp, wdw, gc, x1, y1, x2, y2 );

	/* check for off-screen resources */
	chan = pix_getchan( wdw );
	if  (chan == NULL)  return;
	if  (chan->restore_on)
		XDrawLine( dsp, chan->restore, gc, x1, y1, x2, y2 );
	if  (chan->hc_on)
		XDrawLine( dsp, chan->hc, gc, x1, y1, x2, y2 );

} /* end of pix_DrawLines */



/*--------------------------------------------------------------------------*/

#ifdef XXX  /* old version without limitation of samples */

void pix_DrawLines( Display *dsp, Window wdw, GC gc, XPoint pxy[],
	int no_of_points, int mode )

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
{
	/* local variables */
	PIXT_WINDOWCHAN   *chan;    /* window channels */

	/* executable code */

	/* draw on screen */
	XDrawLines( dsp, wdw, gc, pxy, no_of_points, mode );

	/* check for off-screen resources */
	chan = pix_getchan( wdw );
	if  (chan == NULL)  return;
	if  (chan->restore_on)
		XDrawLines( dsp, chan->restore, gc, pxy, no_of_points, mode );
	if  (chan->hc_on)
		XDrawLines( dsp, chan->hc, gc, pxy, no_of_points, mode );

} /* end of pix_DrawLines */

#endif

/*--------------------------------------------------------------------------*/



void pix_DrawLines( Display *dsp, Window wdw, GC gc, XPoint pxy[],
	int no_of_points, int mode )

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
{
	/* local variables */
	PIXT_WINDOWCHAN   *chan;        /* window channels */
	int               to_be_drawn;  /* remaining samples */
	int               are_drawn;    /* already drawn points */
	int               curr_draw;    /* current draw length */

	/* executable code */

	/* draw on screen */
	to_be_drawn = no_of_points;
	are_drawn = 0;
	while  (to_be_drawn > 0)  {
		curr_draw = (to_be_drawn > pixv_max_drawlth)
			? pixv_max_drawlth : to_be_drawn;
		/* do all the drawing */
		XDrawLines( dsp, wdw, gc, pxy+are_drawn, curr_draw, mode );
		chan = pix_getchan( wdw );
		if  (chan != NULL)  {
			if  (chan->restore_on)
				XDrawLines( dsp, chan->restore, gc, pxy+are_drawn, curr_draw, mode);
			if  (chan->hc_on)
				XDrawLines( dsp, chan->hc, gc, pxy+are_drawn, curr_draw, mode );
		} /*endif*/
		to_be_drawn -= curr_draw;
		are_drawn += curr_draw;
		if  (to_be_drawn > 0)  {
			/* go back one sample */
			are_drawn--;
			to_be_drawn++;
		} /*endif*/
	} /*endwhile*/

} /* end of pix_DrawLines */



/*--------------------------------------------------------------------------*/



void pix_DrawString( Display *dsp, Window wdw, GC gc, int x, int y,
	char str[], int lth )

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
{
	/* local variables */
	PIXT_WINDOWCHAN   *chan;    /* window channels */

	/* executable code */

	/* draw on screen */
	XDrawString( dsp, wdw, gc, x, y, str, lth );

	/* check for off-screen resources */
	chan = pix_getchan( wdw );
	if  (chan == NULL)  return;
	if  (chan->restore_on)
		XDrawString( dsp, chan->restore, gc, x, y, str, lth );
	if  (chan->hc_on)
		XDrawString( dsp, chan->hc, gc, x, y, str, lth );

} /* end of pix_DrawString */



/*--------------------------------------------------------------------------*/



void pix_FillRectangle( Display *dsp, Window wdw, GC gc, int x, int y,
	unsigned width, unsigned height )

/* fills rectangle on screen and off-screen resources if initialized
 *
 * parameters of routine
 * Display    *dsp;          input; pointer to display
 * Window     wdw;           input; window on screen
 * GC         gc;            input; graphics context
 * int        x, y;          input; position
 * unsigned   width, height; input; width and height of rectangle
 */
{
	/* local variables */
	PIXT_WINDOWCHAN   *chan;    /* window channels */

	/* executable code */

	/* draw on screen */
	XFillRectangle( dsp, wdw, gc, x, y, width, height );

	/* check for off-screen resources */
	chan = pix_getchan( wdw );
	if  (chan == NULL)  return;
	if  (chan->restore_on)
		XFillRectangle( dsp, chan->restore, gc, x, y, width, height );
	if  (chan->hc_on)
		XFillRectangle( dsp, chan->hc, gc, x, y, width, height );

} /* end of pix_FillRectangle */



/*--------------------------------------------------------------------------*/



void pix_DrawArc( Display *dsp, Window wdw, GC gc, int x, int y,
	unsigned width, unsigned height, int angle1, int angle2 )

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
{
	/* local variables */
	PIXT_WINDOWCHAN   *chan;    /* window channels */

	/* executable code */

	/* draw on screen */
	XDrawArc( dsp, wdw, gc, x, y, width, height, angle1, angle2 );

	/* check for off-screen resources */
	chan = pix_getchan( wdw );
	if  (chan == NULL)  return;
	if  (chan->restore_on)
		XDrawArc( dsp, chan->restore, gc, x, y, width, height, angle1, angle2 );
	if  (chan->hc_on)
		XDrawArc( dsp, chan->hc, gc, x, y, width, height, angle1, angle2 );

} /* end of pix_DrawArc */



/*--------------------------------------------------------------------------*/



void pix_SetClipRectangles( Display *dsp, GC gc, int xo, int yo,
	XRectangle rect[], int n, int ordering )

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
{
	/* executable code */

	XSetClipRectangles( dsp, gc, xo, yo, rect, n, ordering );

} /* end of pix_SetClipRectangles */



/*--------------------------------------------------------------------------*/



void pix_set_max_drawlth( int max_drawlth )

/* Sets maximum number of samples to be plotted in one XDrawLines call.
 *
 * parameters of routine
 * int        max_drawlth;        input; maximum number of samples
 */
{
	/* executable code */

	pixv_max_drawlth = max_drawlth;

} /* end of pix_set_max_drawlth */



/*----------------------------------------------------------------------------*/



unsigned long pix_colour( int type )

/* returns colour value for foreground and background
 *
 * parameters of routine
 * int        type; input; type of colour
 */
{
	/* local variables */

	/* executable code */

	switch (type)  {
	case PIXC_COL_BACKGROUND:
		return pixv_col_bg;
	case PIXC_COL_FOREGROUND:
		return pixv_col_fg;
	default:
		return pixv_col_fg;
	} /*endswitch*/

} /* end of pix_colour */



/*--------------------------------------------------------------------------*/

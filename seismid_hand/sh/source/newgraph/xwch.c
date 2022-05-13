
/* file XWCH.C
 *      ======
 *
 * version 15, 14-Jan-2005
 *
 * XWindow channel for graphics module
 * K. Stammler, 21-AUG-1990
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include BASECNST
#ifdef BC_VAX
#include <decw$include:xlib.h>
#include <decw$include:xutil.h>
#include <decw$include:xatom.h>
#include <decw$include:cursorfont.h>
#endif
#ifdef BC_SUN
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
/* typedef char *caddr_t; */  /* I really don't know why this is necessary on HP */
#include <X11/Xutil.h>
#endif
#include BC_SYSBASE
#include "graphbas.h"
#include "xwusrdef.h"
#include "xwerrors.h"
#include "../globalparams.h"


/* global constants */
#define X_PIX_PER_CM 30.0
#define Y_PIX_PER_CM 30.0
#define MAXSTYLE 10
	/* number of style blocks (GC's) */
#define DEFSTYLE 0
	/* default style number */
#define MAXDEFSTYLE 6
	/* number of different line styles */
#define XWRITEOFF 3
#define YWRITEOFF 3
	/* offsets of write position */
#define DEFAULTWIDTH 100.0
	/* default width of user coo */
#define DEFAULTHEIGHT 100.0
	/* default height of user coo */
#define MAXSTRLTH 200
#define MAXSTORESTR 512

/* #define EVENTS (ExposureMask|KeyPressMask|FocusChangeMask) */
#define EVENTS (ExposureMask|KeyPressMask)
	/* events to wait for */

#define XWC_FONTLIST "xfonts.lis"
	/* name of font list file */

#define NUMMASK 0x0f

/* ASCII codes */
#define DEL (char)0x7f
#define BS (char)8
#define CR (char)13
#define LF (char)10
#define ESC (char)27


/* macros */
#define xwh_illegalwdw(w) ((w) >= XWC_MAXWDW)
	/* illegal window number */
#define xwh_wdwopen(w) (((w) < XWC_MAXWDW) && ((1 << (w)) & xwv_ison))
	/* window w is open */
#define ASWAP(x,y) if (xwv_arrayswap) {XWCOO tmp;tmp=(x);(x)=(y);(y)=tmp;}
	/* swap coo's in xw_arrayplot */


/* global variables */
static BOOLEAN       xwv_dspinit={FALSE};   /* Display initialised */
static Display       *xwv_dsp;              /* Display */
static Screen        *xwv_screen;           /* Screen */
static Window        xwv_wdw[XWC_MAXWDW];   /* window specifiers */
static Colormap      xwv_cmap;              /* colour map */
static BOOLEAN       xwv_colored;           /* coloured screen */
static int           xwv_ison=0;            /* stores open status */
static XWCOO         xwv_gxpos[XWC_MAXWDW]; /* graphic x-position */
static XWCOO         xwv_gypos[XWC_MAXWDW]; /* graphic y-position */
static int           xwv_wxpos[XWC_MAXWDW]; /* text x-position */
static int           xwv_wypos[XWC_MAXWDW]; /* text y-position */
static XWCOO         xwv_txoff[XWC_MAXWDW]; /* x-offset in user coordinates */
static XWCOO         xwv_tyoff[XWC_MAXWDW]; /* y-offset in user coordinates */
static float         xwv_txfac[XWC_MAXWDW]; /* x-zoom factor in user coo's */
static float         xwv_tyfac[XWC_MAXWDW]; /* y-zoom factor in user coo's */
static GC            xwv_gc[MAXSTYLE];      /* style blocks */
static GC            xwv_default_gc;        /* default style */
static XFontStruct   *xwv_font[MAXSTYLE];   /* font structure */
static XFontStruct   *xwv_default_font;     /* default font */
static unsigned long xwv_default_fore;      /* default foreground pixel */
static unsigned long xwv_default_back;      /* default background pixel */
static int           xwv_laststyle;         /* last style used (text) */
static XWCOO         xwv_userh;             /* user height */
static BOOLEAN       xwv_tomem=TRUE;        /* store hardcopy in memory */
static char          xwv_wstr[XWC_MAXWDW][MAXSTORESTR+1];
static BOOLEAN       xwv_arrayswap=FALSE;   /* swap arrayplot */
														  /* "xw_write"-strings for redraw */
static int           xwv_lstyle[MAXSTYLE]   /* line styles */
		={LineSolid,LineSolid,LineSolid,LineSolid,LineSolid,LineSolid,
		  LineSolid,LineSolid,LineSolid,LineSolid};
static int           xwv_lwidth[MAXSTYLE]   /* line widths */
		={1,1,1,1,1,1,1,1,1,1};
static int           xwv_defstyle[MAXDEFSTYLE] /* default styles */
		={LineSolid,LineDoubleDash,LineOnOffDash,LineDoubleDash,LineOnOffDash,
		  LineDoubleDash};
static char          xwv_inputdir[BC_FILELTH+1];  /* input directory */


/* prototypes of local routines */
static void xwh_transform( WDW wdw, XWCOO x, XWCOO y, int *u, int *v );
static void xwh_backtrans( WDW wdw, int x, int y, XWCOO *u, XWCOO *v );
static void xwh_getwdwsize( WDW wdw, int *ww, int *wh );
static int  xwh_wdwdepth( WDW wdw );
static int  xwh_getfontheight( int style );
static int  xwh_getfontwidth( int style );
static void xwh_newline( WDW wdw );
static void xwh_scroll( WDW wdw );
static void xwh_read_character( char *ch );
static void xwh_drawcross( WDW wdw, GC gc, int x, int y );
static void xwh_setcolor( int style, char value[], STATUS *status );
static WDW xwh_findwdw( Window xwdw );
static int xwh_getfontnum( WDW wdw, float size );


/*----------------------------------------------------------------------------*/



void xw_init( WDW wdw, int attribs, float xlo, float ylo,
	float width, float height, STATUS *status )

/* initialises window
 *
 * parameters of routine
 * WDW        wdw;            input; channels to be initialised
 * int        attribs;        input; window attributes
 * float      xlo, ylo;       input; position of window (in cm)
 * float      width, height;  input; size of window (in cm)
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	int                  wx, wy;   /* position of window */
	int                  ww, wh;   /* size of window */
	int                  i;        /* counter */
	XSetWindowAttributes xswa;     /* set attribute block */
	XSizeHints           xsh;      /* size hints */ 
	XWMHints             xwmh;     /* window manager hints */
	char                 str[MAXSTRLTH+1]; /* scratch string */

	/* executable code */

	if  (!xwv_dspinit)  {

		/* initialise everything ... */
		xwv_dsp = XOpenDisplay( NULL );
		if  (xwv_dsp == NULL)  {
			*status = XWE_OPNDSP;
			return;
		} /*endif*/
		xwv_screen = XDefaultScreenOfDisplay( xwv_dsp );
		/* XSynchronize( xwv_dsp, 1 ); only for test !!! */

		/* get colourmap if coloured screen */
		xwv_colored = ((XDefaultVisualOfScreen(xwv_screen))->class == TrueColor
			|| (XDefaultVisualOfScreen(xwv_screen))->class == PseudoColor
			|| (XDefaultVisualOfScreen(xwv_screen))->class == DirectColor
			|| (XDefaultVisualOfScreen(xwv_screen))->class == StaticColor);
		if  (xwv_colored)
			xwv_cmap = XDefaultColormapOfScreen( xwv_screen );

		/* create GC's */
		xwv_default_gc = XCreateGC( xwv_dsp, XRootWindowOfScreen(xwv_screen),
			0, NULL );
		xwv_default_fore = XBlackPixelOfScreen(xwv_screen);
		xwv_default_back = XWhitePixelOfScreen(xwv_screen);
		XSetForeground( xwv_dsp, xwv_default_gc, xwv_default_fore );
		XSetBackground( xwv_dsp, xwv_default_gc, xwv_default_back );
		for  (i=0; i<MAXSTYLE; i++)  {
			xwv_gc[i] = XCreateGC( xwv_dsp, XRootWindowOfScreen(xwv_screen),
				0, NULL );
			XSetForeground( xwv_dsp, xwv_gc[i], xwv_default_fore );
			XSetBackground( xwv_dsp, xwv_gc[i], xwv_default_back );
		} /*endfor*/
		xwv_default_font = XQueryFont( xwv_dsp, XGContextFromGC(xwv_gc[0]) );
		for  (i=0; i<MAXSTYLE; i++)
			xwv_font[i] = xwv_default_font;

		xwv_dspinit = TRUE;
	} /*endif*/

	wdw &= NUMMASK;
	if  (xwh_illegalwdw(wdw))  {
		*status = XWE_ILWDW;
		return;
	} /*endif*/

	if  ((1 << wdw) & xwv_ison)  {
		*status = XWE_WOPNTWICE;
		return;
	} /*endif*/
	ww = (int)(width*X_PIX_PER_CM);
	wh = (int)(height*Y_PIX_PER_CM);
	wx = (int)(xlo*X_PIX_PER_CM);
	wy = XHeightOfScreen(xwv_screen) - (int)(ylo*Y_PIX_PER_CM) - wh;
	xswa.event_mask = EVENTS;
	xswa.background_pixel = xwv_default_back;
	xswa.border_pixel = xwv_default_fore;
	xswa.override_redirect = True;
	xswa.save_under = True;
	xswa.backing_store = WhenMapped;
	xswa.border_pixmap = CopyFromParent;
	xwv_wdw[wdw] = XCreateWindow( xwv_dsp, XRootWindowOfScreen(xwv_screen),
		wx, wy, ww, wh, 4, XDefaultDepthOfScreen(xwv_screen), InputOutput,
		XDefaultVisualOfScreen(xwv_screen),
		CWEventMask|CWBackPixel|CWBorderPixel| /*CWOverrideRedirect|*/
		CWBackingStore|CWSaveUnder|CWBorderPixmap,
		&xswa );
	xsh.x = wx; xsh.y = wy; xsh.width = ww; xsh.height = wh;
	xsh.flags = PPosition | PSize;
	XSetNormalHints( xwv_dsp, xwv_wdw[wdw], &xsh );
	xwmh.flags = InputHint;
	xwmh.input = (attribs & XWF_WINPUT);
	XSetWMHints( xwv_dsp, xwv_wdw[wdw], &xwmh );
	XMapWindow( xwv_dsp, xwv_wdw[wdw] );
	xwv_wxpos[wdw] = XWRITEOFF;
	xwv_wypos[wdw] = xwh_getfontheight(DEFSTYLE) + YWRITEOFF;
	xwv_ison |= (1 << wdw);
	sprintf( str, "Wdw %d", wdw );
	XChangeProperty( xwv_dsp, xwv_wdw[wdw], XA_WM_NAME, XA_STRING, 8,
		PropModeReplace, str, strlen(str) );

	xw_setcoo( wdw, 0., 0., DEFAULTWIDTH, DEFAULTHEIGHT, status );

	xw_updatewdw();

} /* end of xw_init */



/*----------------------------------------------------------------------------*/



void xw_resizewdw( WDW wdw, float x, float y, float w, float h,
	STATUS *status )

/* resizes and repositions window
 *
 * parameters of routine
 * WDW        wdw;       input; window number
 * float      x, y;      input; new position of window (in cm)
 * float      w, h;      input; new size of window (in cm)
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	int          wx, wy;     /* new position (window coordinates) */
	int          ww, wh;     /* new size (window coordinates) */
	XSizeHints   xsh;        /* size hints */

	/* executable code */

	wdw &= NUMMASK;
	if  (!xwh_wdwopen(wdw))  return;

	ww = (int)(w*X_PIX_PER_CM);
	wh = (int)(h*Y_PIX_PER_CM);
	wx = (int)(x*X_PIX_PER_CM);
	wy = XHeightOfScreen(xwv_screen) - (int)(y*Y_PIX_PER_CM) - wh;
	xsh.x = wx; xsh.y = wy; xsh.width = ww; xsh.height = wh;
	xsh.flags = PPosition | PSize;
	XSetNormalHints( xwv_dsp, xwv_wdw[wdw], &xsh );
	/* XMoveResizeWindow( xwv_dsp, xwv_wdw[wdw], wx, wy, ww, wh ); */
	/* XSynchronize( xwv_dsp, 1 ); */

} /* end of xw_resizewdw */



/*----------------------------------------------------------------------------*/



void xw_exit( WDW wdw )

/* closes window
 *
 * parameters of routine
 * WDW        wdw;     input; window number
 */
{
	/* local variables */
	int      dmy=0;

	/* executable code */

	wdw &= NUMMASK;
	if  ((1<<wdw) & xwv_ison)  {
		XUnmapWindow( xwv_dsp, xwv_wdw[wdw] );
		XDestroyWindow( xwv_dsp, xwv_wdw[wdw] );
		xwv_ison &= ~(1<<wdw);
	} /*endif*/

} /* end of xw_exit */



/*----------------------------------------------------------------------------*/



void xw_finish( void )

/* closes all open windows
 *
 * no parameters
 */
{
	/* executable code */

	if  (xwv_dspinit)  {
		XCloseDisplay( xwv_dsp );
		xwv_dspinit = FALSE;
	} /*endif*/

} /* end of xw_finish */



/*----------------------------------------------------------------------------*/



void xw_erase( WDW wdw )

/* clears window
 *
 * parameters of routine
 * WDW        wdw;     input; window number
 */
{
	/* local variables */
	int      dmy;

	/* executable code */

	wdw &= NUMMASK;
	if  (xwh_wdwopen(wdw))  {
		XClearWindow( xwv_dsp, xwv_wdw[wdw] );
		XFlush( xwv_dsp );
		xwv_wxpos[wdw] = XWRITEOFF;
		xwv_wypos[wdw] = xwh_getfontheight(DEFSTYLE) + YWRITEOFF;
		xwv_wstr[wdw][0] = '\0';
	} /*endif*/

} /* end of xw_erase */



/*----------------------------------------------------------------------------*/



void xw_popwdw( WDW wdw )

/* pops window on top of the others
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 */
{
	/* local variables */
	/* XImage   *img;  */          /* image */
	/* int      ww, wh, depth;*/   /* window size & depth */

	/* executable code */

	wdw &= NUMMASK;
	if  (xwh_wdwopen(wdw))  {
#     ifdef KSTEST
		xwh_getwdwsize( wdw, &ww, &wh );
		depth = xwh_wdwdepth( wdw );
		img = XGetImage( xwv_dsp, xwv_wdw[wdw], 0, 0, ww, wh, -1, ZPixmap );
#     endif
		XSynchronize( xwv_dsp, TRUE );
		XRaiseWindow( xwv_dsp, xwv_wdw[wdw] );
		XFlush( xwv_dsp );
		XSynchronize( xwv_dsp, FALSE );
#     ifdef KSTEST
		XPutImage( xwv_dsp, xwv_wdw[wdw], xwv_gc[wdw], img, 0, 0, 0, 0, ww, wh );
		XDestroyImage( img );
#     endif
	} /*endif*/

} /* end of xw_popwdw */



/*----------------------------------------------------------------------------*/



void xw_pushwdw( WDW wdw )

/* pushes window below the others
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 */
{
	/* executable code */

	wdw &= NUMMASK;
	if  (xwh_wdwopen(wdw))  {
		XSynchronize( xwv_dsp, TRUE );
		XLowerWindow( xwv_dsp, xwv_wdw[wdw] );
		XSynchronize( xwv_dsp, FALSE );
	} /*endif*/

} /* end of xw_pushwdw */



/*----------------------------------------------------------------------------*/



void xw_setwdwname( WDW wdw, char name[] )

/* set new name of window
 *
 * parameters of routine
 * WDW       wdw;       input; window number
 * char      name[];    input; new name
 */
{
	/* executable code */

	wdw &= NUMMASK;
	if  xwh_wdwopen(wdw)
		XChangeProperty( xwv_dsp, xwv_wdw[wdw], XA_WM_NAME, XA_STRING, 8,
			PropModeReplace, name, strlen(name) );

} /* end of xw_setwdwname */



/*----------------------------------------------------------------------------*/



void xw_setcoo( WDW wdw, XWCOO x, XWCOO y, XWCOO w, XWCOO h,
	STATUS *status )

/* sets new user coordinate system
 *
 * parameters of routine
 * WDW        wdw;      input; channel map
 * XWCOO      x, y;     input; origin of user coordinates
 * XWCOO      w, h;     input; size of user coordinates
 * STATUS     *status;  output; return status
 */
{
	/* local variables */
	int      ww, wh;         /* window size */

	/* executable code */

	wdw &= NUMMASK;
	if  (!xwh_wdwopen(wdw))  return;

	xwh_getwdwsize( wdw, &ww, &wh );
	xwv_txfac[wdw] = (float)ww / (float)w;
	xwv_tyfac[wdw] = -(float)wh / (float)h;
	xwv_txoff[wdw] = x * xwv_txfac[wdw];
	xwv_tyoff[wdw] = xwv_tyfac[wdw]*y - wh;

	xwv_userh = h;

} /* end of xw_setcoo */



/*----------------------------------------------------------------------------*/



void xw_moveto( WDW wdw, XWCOO x, XWCOO y )

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * XWCOO      x, y;     input; position to be moved to
 */
{
	/* local variables */
	int      mfstatus;     /* mf return status */

	/* executable code */

	wdw &= NUMMASK;
	if  (xwh_wdwopen(wdw))  {
		xwv_gxpos[wdw] = x;
		xwv_gypos[wdw] = y;
	} /*endif*/

} /* end of xw_moveto */



/*----------------------------------------------------------------------------*/



void xw_drawto( WDW wdw, int style, XWCOO x, XWCOO y )

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * int        style;    input; style block number
 * XWCOO      x, y;     input; position to be moved to
 */
{
	/* local variables */
	int      u0, v0;     /* start position (wdw coordinates) */
	int      u1, v1;     /* end position (wdw coordinates) */
	int      mfstatus;   /* mf return status */

	/* executable code */

	wdw &= NUMMASK;
	if  (style < 0 || style >= MAXSTYLE)
		style = DEFSTYLE;

	if  (xwh_wdwopen(wdw))  {
		xwh_transform( wdw, xwv_gxpos[wdw], xwv_gypos[wdw], &u0, &v0 );
		xwh_transform( wdw, x, y, &u1, &v1 );
		XDrawLine( xwv_dsp, xwv_wdw[wdw], xwv_gc[style], u0, v0, u1, v1 );
		/* XSynchronize( xwv_dsp, 1 ); */
		/* XFlush( xwv_dsp ); */
		xwv_gxpos[wdw] = x;
		xwv_gypos[wdw] = y;
	} /*endif*/

} /* end of xw_drawto */



/*----------------------------------------------------------------------------*/



void xw_arrayplot( WDW wdw, int style, long cnt, int red, XWCOO xoff,
	XWCOO xinc, XWCOO yoff, XWCOO yarr[], XWCOO yzoom, STATUS *status )

/* plots an array of data
 *
 * parameters of routine
 * WDW        wdw;       input; window number
 * int        style;     input; style block number
 * long       cnt;       input; length of data array
 * int        red;       input; reduction factor
 * XWCOO      xoff;      input; x-position of first sample
 * XWCOO      xinc;      input; x increment
 * XWCOO      yoff;      input; y-position of first sample
 * XWCOO      yarr[];    input; data array
 * float      yzoom;     input; zoom factor in y-direction
 * STATUS     *status;   output; return status
 */
/* #define MAXPLOT 16000 */
#define MAXPLOT 4000
{
	/* local variables */
	XWCOO    *yptr;     /* moving pointer */
	XWCOO    lx, ly;    /* scratch values */
	XPoint   *pxy;      /* multiple lines */
	XPoint   *p;        /* moving pointer */
	long     drawlth;   /* number of points to draw */
	long     currdraw;  /* current draw length */
	long     drawn;     /* points already drawn */
	long     i;         /* counter */
	int      ix, iy;    /* window coordinates */

	/* executable code */

	wdw &= NUMMASK;
	if  (cnt == 0L)  return;
	if  (red <= 0)  return;
	if  (style < 0 || style >= MAXSTYLE)
		style = DEFSTYLE;

	drawlth = cnt / red;
	if  ((red > 1) && ((cnt % red) != 0))
		drawlth++;

	if  (xwh_wdwopen(wdw))  {
		pxy = (XPoint *)sy_allocmem( drawlth, sizeof(XPoint), status );
		if  Severe(status)  return;
		yptr = yarr;
		p = pxy;
		for  (i=0; i<drawlth; i++)  {
			lx = xoff + xinc*(XWCOO)(red*i);
			ly = yoff + yzoom * (*yptr);
			ASWAP(lx,ly)
			xwh_transform( wdw, lx, ly, &ix, &iy );
			p->x = ix;
			p->y = iy;
			p++;
			yptr += red;
		} /*endwhile*/
		if  (drawlth > MAXPLOT)  {
			drawn = 0;
			do  {
				currdraw = drawlth - drawn;
				if  (currdraw > MAXPLOT)  currdraw = MAXPLOT-1;
				if  (drawn > 0)  {
					XDrawLines( xwv_dsp, xwv_wdw[wdw], xwv_gc[style],
						pxy+drawn-1, currdraw+1, CoordModeOrigin );
				} else {
					XDrawLines( xwv_dsp, xwv_wdw[wdw], xwv_gc[style],
						pxy, currdraw, CoordModeOrigin );
				} /*endif*/
				drawn += currdraw;
			}  while (drawn < drawlth);
		} else {
			XDrawLines( xwv_dsp, xwv_wdw[wdw], xwv_gc[style], pxy, drawlth,
				CoordModeOrigin );
		} /*endif*/
		sy_deallocmem( pxy );
	} /*endif*/

} /* end of xw_arrayplot */



/*----------------------------------------------------------------------------*/



void xw_text( WDW wdw, int style, XWCOO x, XWCOO y, char text[] )

/* writes text at position (x,y)
 *
 * parameters of routine
 * WDW        wdw;       input; window number
 * int        style;     input; style block number
 * XWCOO      x, y;      input; position of text
 * char       text[];    input; text to be written
 */
{
	/* local variables */
	int      wx, wy;    /* window coordinates */
	int      mfstatus;  /* mf return status */

	/* executable code */

	wdw &= NUMMASK;
	if  (style < 0 || style >= MAXSTYLE)
		style = DEFSTYLE;

	if  (xwh_wdwopen(wdw))  {
		xwh_transform( wdw, x, y, &wx, &wy );
		XDrawString( xwv_dsp, xwv_wdw[wdw], xwv_gc[style],
			wx, wy, text, (int)strlen(text) );
	} /*endif*/

	xwv_laststyle = style;

} /* end of xw_text */



/*----------------------------------------------------------------------------*/



void xw_wrtch( WDW wdw, char ch )

/* writes single character to current write position
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * char       ch;       input; character to be written
 */
{
	/* local variables */
	char     cs[2];     /* "string" */

	/* executable code */

	wdw &= NUMMASK;
	if  (xwh_wdwopen(wdw))  {
		*cs = ch;
		cs[1] = '\0';
		if  (ch == DEL)  {
			xw_wrtch( wdw, BS );
			xw_wrtch( wdw, ' ' );
			xw_wrtch( wdw, BS );
		} else if  (ch == BS)  {
			xwv_wxpos[wdw] -= xwh_getfontwidth( DEFSTYLE );
		} else if  (ch == '\n')  {
			xwh_newline( wdw );
		} else {
			XDrawImageString( xwv_dsp, xwv_wdw[wdw], xwv_gc[DEFSTYLE],
				xwv_wxpos[wdw], xwv_wypos[wdw], cs, 1 );
			xwv_wxpos[wdw] += xwh_getfontwidth( DEFSTYLE );
		} /*endif*/
	} /*endif*/

} /* xw_wrtch */



/*----------------------------------------------------------------------------*/



void xw_write( WDW wdw, char text[] )

/* writes text at the current write position
 *
 * parameters of routine
 * WDW        wdw;    input; window number
 * char       text[]; input; text to be written
 */
{
	/* local variables */
	BOOLEAN  newline;           /* newline at end */
	int      txtlth;            /* length of text */

	/* executable code */

	wdw &= NUMMASK;
	if  (xwh_wdwopen(wdw))  {

		if  (*text == '\n')  {
			xwh_newline( wdw );
			text++;
		} /*endif*/

		/* check <CR> at end */
		txtlth = (int)strlen( text );
		if  (txtlth == 0)  return;
		strncpy( xwv_wstr[wdw], text, MAXSTORESTR );
		newline = (text[txtlth-1] == '\n');
		if  (newline)
			text[--txtlth] = '\0';

		XDrawImageString( xwv_dsp, xwv_wdw[wdw], xwv_gc[DEFSTYLE],
			xwv_wxpos[wdw], xwv_wypos[wdw], text, txtlth );

		if  (newline)  {
			text[txtlth] = '\n';
			xwh_newline( wdw );
		} else {
			xwv_wxpos[wdw] += XTextWidth( xwv_font[DEFSTYLE], text, txtlth );
		} /*endif*/

	} /*endif*/

} /* end of xw_write */



/*----------------------------------------------------------------------------*/



void xw_read( WDW wdw, int maxlth, char text[] )

/* reads text string from window
 *
 * parameters of routine
 * WDW        wdw;      input; channel map
 * int        maxlth;   input; maximum length of string
 * char       text[];   output; string read
 */
{
	/* local variables */
	int      i;         /* counter */

	/* executable code */

	wdw &= NUMMASK;
	if  (xwh_wdwopen(wdw))  {
		XGrabKeyboard( xwv_dsp, xwv_wdw[wdw], True,
			GrabModeAsync, GrabModeAsync, CurrentTime );
		i = 0;
		while  (i < maxlth)  {
			xwh_read_character( text+i );
			if  ((text[i] == CR) || (text[i] == LF))  {
				text[i] = '\0';
				break;
			} else if  ((text[i] == BS) || (text[i] == DEL))  {
				if  (i != 0)  {
					xw_wrtch( wdw, DEL );
					text[i--] = '\0';
				} /*endif*/
			} else if  (text[i] == '\t')  {
				text[i] = ' ';
				xw_wrtch( wdw, text[i++] );
			} else if  (text[i] == ESC)  {
				for  (;i>0;i--)  {
					xw_wrtch( wdw, DEL );
					text[i] = '\0';
				} /*endfor*/
				*text = '\0';
			} else if  (text[i] >= ' ')  {
				xw_wrtch( wdw, text[i++] );
			} /*endif*/
		} /*endwhile*/
		xwh_newline( wdw );
		XUngrabKeyboard( xwv_dsp, CurrentTime );
	} /*endif*/

} /* end of xwc_read */



/*----------------------------------------------------------------------------*/



void xw_getloc( WDW wdw, XWCOO *x, XWCOO *y, char *ch )

/* returns user selected mouse position and key
 *
 * parameters of routine
 * WDW        wdw;          input; window number
 * XWCOO      *x, *y;       output; selected position
 * char       *ch;          output; selected key (mouse button = '@')
 */
{
	/* local variables */
	XEvent   event;       /* event */
	XEvent   evcop;       /* other event */
	BOOLEAN  quit;        /* quit loop */
	int      dmy;         /* scratch */
	int      xpos, ypos;  /* cursor position */
	GC       cross_gc;    /* crosshair cursors's GC */
	Cursor   crsr;        /* mouse form */
	WDW      rdw;         /* redraw window */
	int      rdmap;       /* window redraw map */
	XColor   screen_color, exact_color;   /* color values */
	Status   xstatus;                     /* X return status */

/* #define REDCROSS */

	/* executable code */

	wdw &= NUMMASK;
	if  (xwh_wdwopen(wdw))  {

		/* XSynchronize( xwv_dsp, 1 ); */
		XFlush( xwv_dsp );
		cross_gc = XCreateGC( xwv_dsp, xwv_wdw[wdw], 0, NULL );
		if  (GpGetBoolean(cGpB_reverse_xors))  {
			XSetForeground( xwv_dsp, cross_gc, xwv_default_back );
			XSetBackground( xwv_dsp, cross_gc, xwv_default_fore );
		} else {
			XSetForeground( xwv_dsp, cross_gc, xwv_default_fore );
			XSetBackground( xwv_dsp, cross_gc, xwv_default_back );
		} /*endif*/
		XSetFunction( xwv_dsp, cross_gc, GXxor );
		XGrabKeyboard( xwv_dsp, xwv_wdw[wdw], True,
			GrabModeAsync, GrabModeAsync, CurrentTime );
		XSelectInput( xwv_dsp, xwv_wdw[wdw],
			EVENTS|ButtonPressMask|PointerMotionMask );
		quit = FALSE;
		rdmap = 0;

		/* clear keyboard & button press buffer */
		while  (XCheckWindowEvent(xwv_dsp,xwv_wdw[wdw],KeyPressMask,&evcop));
		while  (XCheckWindowEvent(xwv_dsp,xwv_wdw[wdw],ButtonPressMask,&evcop));

		/* change mouse form to cross */
		crsr = XCreateFontCursor( xwv_dsp, XC_crosshair );
		XDefineCursor( xwv_dsp, xwv_wdw[wdw], crsr );

		/* draw centered crosshair cursor */
		xwh_getwdwsize( wdw, &xpos, &ypos );
		xpos /= 2.0; ypos /= 2.0;
		xwh_drawcross( wdw, cross_gc, xpos, ypos );

		do  {

			XNextEvent( xwv_dsp, &event );

			switch  (event.type)  {
			case Expose:
				/* delete all other Expose events of the same window */
				while  (XCheckWindowEvent(xwv_dsp,event.xexpose.window,
					ExposureMask,&evcop))
					event = evcop;
				dmy = XWE_NOERROR;
				rdw = xwh_findwdw( event.xexpose.window );
				if  ((rdw >= 0) && !((1<<rdw) & rdmap))  {
					xwh_drawcross( wdw, cross_gc, xpos, ypos );
					xw_redraw( rdw, &dmy );
					xwh_drawcross( wdw, cross_gc, xpos, ypos );
					rdmap |= (1<<rdw);
				} /*endif*/
				break;
			case ButtonPress:
				if  (event.xbutton.window == xwv_wdw[wdw])  {
					*ch = '@';
					xwh_backtrans( wdw, event.xbutton.x, event.xbutton.y, x, y );
					quit = TRUE;
				} /*endif*/
				break;
			case KeyPress:
				dmy = XLookupString( &(event.xkey), ch, 1, NULL, NULL );
				xwh_backtrans( wdw, event.xkey.x, event.xkey.y, x, y );
				quit = TRUE;
				break;
			case MotionNotify:
				while  (XCheckWindowEvent(xwv_dsp,xwv_wdw[wdw],
					PointerMotionMask,&evcop))  {
					event = evcop;
				} /*endwhile*/
				xwh_drawcross( wdw, cross_gc, xpos, ypos );
				xpos = event.xmotion.x;
				ypos = event.xmotion.y;
				xwh_drawcross( wdw, cross_gc, xpos, ypos );
				break;
			case FocusIn:
				while  (XCheckWindowEvent(xwv_dsp,xwv_wdw[wdw],
					FocusChangeMask,&evcop))
					event = evcop;
				dmy = XWE_NOERROR;
				rdw = xwh_findwdw( event.xfocus.window );
				if  ((rdw >= 0) && !((1<<rdw) & rdmap))  {
					xwh_drawcross( wdw, cross_gc, xpos, ypos );
					xw_redraw( rdw, &dmy );
					if  (dmy != XWE_NOERROR)  quit = TRUE;
					xwh_drawcross( wdw, cross_gc, xpos, ypos );
					rdmap |= (1 << rdw);
				} /*endif*/
				break;
			case FocusOut:
				break;
			default:
				break;
			} /*endswitch*/

		}  while  (!quit);

		xwh_drawcross( wdw, cross_gc, xpos, ypos );
		XFlush( xwv_dsp );
		XUndefineCursor( xwv_dsp, xwv_wdw[wdw] );
		XFreeCursor( xwv_dsp, crsr );
		XSelectInput( xwv_dsp, xwv_wdw[wdw], EVENTS );
		XUngrabKeyboard( xwv_dsp, CurrentTime );
		XFreeGC( xwv_dsp, cross_gc );
		/* XSynchronize( xwv_dsp, 1 ); */
		XFlush( xwv_dsp );

	} /*endif*/

} /* end of xw_getloc */



/*----------------------------------------------------------------------------*/



void xw_setstyle( WDW wdw, int style, char item[], char value[],
	STATUS *status )

/* sets attribute in style block number "style"
 *
 * parameters of routine
 * WDW        wdw;       input; window number
 * int        style;     input; style block number
 * char       item[];    input; description of attribute
 * char       value[];   input; new value of attribute (as string expression)
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	int         num, cnt;
	float       r;                 /* scratch */
	char        **fontlist;        /* list of font names */
	BOOLEAN     local_item;        /* local item (not available on CC) */

	/* executable code */

	if  (!xwv_dspinit)  return;

	if  ((style < 0) || (style > MAXSTYLE))  {
		*status = XWE_ILSTYLE;
		return;
	} /*endif*/

	wdw &= NUMMASK;
	local_item = FALSE;
	if  (strcmp(item,"FONT") == 0)  {
		if  (xwv_font[style] != xwv_default_font)     /* unload font */
			XFreeFont( xwv_dsp, xwv_font[style] );
		if  (strcmp(value,"DEFAULT") == 0)  {
			xwv_font[style] = xwv_default_font;
			XCopyGC( xwv_dsp, xwv_default_gc, GCFont, xwv_gc[style] );
		} else {
			xwv_font[style] = XLoadQueryFont( xwv_dsp, value );
			XSetFont( xwv_dsp, xwv_gc[style], xwv_font[style]->fid );
		} /*endif*/
	} else if  (strcmp(item,"CHARSIZE") == 0  ||
		strcmp(item,"CHARHEIGHT") == 0)  {
		if  (sscanf(value,"%f",&r) != 1)  {
			*status = XWE_ILVALUE;
			return;
		} /*endif*/
		num = xwh_getfontnum( wdw, r );
		if  (num <= 0)  return;
		fontlist = XListFonts( xwv_dsp, "*", num+1, &cnt );
		xw_setstyle( wdw, style, "FONT", fontlist[cnt-1], status );
		XFreeFontNames( fontlist );
	} else if  (strcmp(item,"FONTNUM") == 0)  {
		local_item = TRUE;
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = XWE_ILVALUE;
			return;
		} /*endif*/
		if  (num < 0)  return;
		fontlist = XListFonts( xwv_dsp, "*", num+1, &cnt );
		if  (num >= cnt)  num = cnt-1;
		xw_setstyle( wdw, style, "FONT", fontlist[num], status );
		XFreeFontNames( fontlist );
	} else if  (strcmp(item,"COLOR") == 0)  {
		xwh_setcolor( style, value, status );
	} else if  (strcmp(item,"LINEWIDTH") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = XWE_ILVALUE;
			return;
		} /*endif*/
		xwv_lwidth[style] = num;
		XSetLineAttributes( xwv_dsp, xwv_gc[style], xwv_lwidth[style],
			xwv_lstyle[style], CapRound, JoinRound );
	} else if  (strcmp(item,"LINESTYLE") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = XWE_ILVALUE;
			return;
		} else if  (num < 0 || num >= MAXDEFSTYLE)  {
			*status = XWE_ILVALUE;
			return;
		} /*endif*/
		xwv_lstyle[style] = xwv_defstyle[num];
		XSetLineAttributes( xwv_dsp, xwv_gc[style], xwv_lwidth[style],
			xwv_lstyle[style], CapRound, JoinRound );
	} else if  (strcmp(item,"WRMODE") == 0)  {
		if  (strcmp(value,"REPLACE") == 0)  {
			XSetForeground( xwv_dsp, xwv_gc[style], xwv_default_fore );
			XSetBackground( xwv_dsp, xwv_gc[style], xwv_default_back );
			XSetFunction( xwv_dsp, xwv_gc[style], GXcopy );
		} else if  (strcmp(value,"XOR") == 0)  {
			if  (GpGetBoolean(cGpB_reverse_xors))  {
				XSetForeground( xwv_dsp, xwv_gc[style], xwv_default_back );
				XSetBackground( xwv_dsp, xwv_gc[style], xwv_default_fore );
			} else  {
				XSetForeground( xwv_dsp, xwv_gc[style], xwv_default_fore );
				XSetBackground( xwv_dsp, xwv_gc[style], xwv_default_back );
			} /*endif*/
			XSetFunction( xwv_dsp, xwv_gc[style], GXxor );
		} else {
			*status = XWE_ILVALUE;
			return;
		} /*endif*/
	} else {
		*status = XWE_ILSTYATT;
		return;
	} /*endif*/

} /* end of xw_setstyle */



/*----------------------------------------------------------------------------*/



void xw_charsize( WDW wdw, int style, float size, STATUS *status )

/* sets character size in units of display height
 *
 * parameters of routine
 * WDW        wdw;       input; window ID
 * int        style;     input; style number
 * float      size;      input; size of character in units of display height
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	int      num;         /* font number */
	int      cnt;         /* length of font list */
	char     **fontlist;  /* font list */

	/* executable code */

	wdw &= NUMMASK;

	if  ((style < 0) || (style >= MAXSTYLE))  {
		*status = XWE_ILSTYLE;
		return;
	} /*endif*/

	num = xwh_getfontnum( wdw, size );
	if  (num <= 0)  return;
	fontlist = XListFonts( xwv_dsp, "*", num+1, &cnt );
	xw_setstyle( wdw, style, "FONT", fontlist[cnt-1], status );
	XFreeFontNames( fontlist );

} /* end of xw_charsize */



/*----------------------------------------------------------------------------*/



void xw_linestyle( WDW wdw, int style, int linestyle, STATUS *status )

/* sets line linestyle in style block number "style"
 *
 * parameters of routine
 * WDW        wdw;        input; window ID
 * int        style;      input; style block number
 * int        linestyle;  input; number of line style
 * STATUS     *status;    output; return status
 */
{
	/* executable code */

	wdw &= NUMMASK;

	if  ((style < 0) || (style >= MAXSTYLE))  {
		*status = XWE_ILSTYLE;
		return;
	} else if  (linestyle < 0 || linestyle >= MAXDEFSTYLE)  {
		*status = XWE_ILVALUE;
		return;
	} /*endif*/

	xwv_lstyle[style] = xwv_defstyle[linestyle];
	XSetLineAttributes( xwv_dsp, xwv_gc[style], xwv_lwidth[style],
		xwv_lstyle[style], CapRound, JoinRound );

} /* end of xw_linestyle */



/*----------------------------------------------------------------------------*/



void xw_linewidth( WDW wdw, int style, int width, STATUS *status )

/* sets line width in pixels in style block "style"
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * int        style;    input; style block number
 * int        width;    input; width of line in pixels
 * STATUS     *status;  output; return status
 */
{
	/* executable code */

	wdw &= NUMMASK;

	if  ((style < 0) || (style >= MAXSTYLE))  {
		*status = XWE_ILSTYLE;
		return;
	} /*endif*/

	xwv_lwidth[style] = width;
	XSetLineAttributes( xwv_dsp, xwv_gc[style], xwv_lwidth[style],
		xwv_lstyle[style], CapRound, JoinRound );

} /* end of xw_linewidth */



/*----------------------------------------------------------------------------*/



void xw_color( WDW wdw, int style, float red, float green, float blue,
	STATUS *status )

/* sets color of style block "style"
 *
 * parameters of routine
 * WDW        wdw;       input; window ID
 * int        style;            input; style block number
 * float      red, green, blue; input; red, green and blue intensities (0..1)
 * STATUS     *status;          output; return status
 */
{
	/* local variables */
	char     valstr[80];      /* value string */

	/* executable code */

	if  ((style < 0) || (style >= MAXSTYLE))  {
		*status = XWE_ILSTYLE;
		return;
	} /*endif*/

	wdw &= NUMMASK;

	if  (red < 0.0)  *status = XWE_ILVALUE;
	if  (red > 1.0)  *status = XWE_ILVALUE;
	if  (green < 0.0)  *status = XWE_ILVALUE;
	if  (green > 1.0)  *status = XWE_ILVALUE;
	if  (blue < 0.0)  *status = XWE_ILVALUE;
	if  (blue > 1.0)  *status = XWE_ILVALUE;
	if  (Severe(status))  return;

	sprintf( valstr, "%5.3f,%5.3f,%5.3f", red, green, blue );
	xwh_setcolor( style, valstr, status );
	if  (Severe(status))  return;

} /* end of xw_color */



/*----------------------------------------------------------------------------*/



XWCOO xw_chheight( WDW wdw )

/* returns char height in user coordinates of last used style
 *
 * parameters of routine
 * WDW        wdw;     input; window number
 */
{
	/* local variables */
	int      iheight;   /* height in pixels */
	int      wh;        /* window height */

	/* executable code */

	wdw &= NUMMASK;
	if  (!xwh_wdwopen(wdw))  return;
	iheight = xwv_font[xwv_laststyle]->max_bounds.ascent +
		xwv_font[xwv_laststyle]->max_bounds.descent;
	xwh_getwdwsize( wdw, NULL, &wh );
	return ((float)iheight/(float)wh * xwv_userh);

} /* end of xw_cheight */



/*----------------------------------------------------------------------------*/



void xw_redraw( WDW wdw, STATUS *status )

/* redraws window
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * STATUS     *status;  output; return status
 */
{
	/* local variables */
	PAINTBOX pb;        /* paintbox routines */

	/* executable code */

	pb.wpb.pbtype = GBC_WPAINT;
	pb.wpb.prepare = xw_prepare;
	pb.wpb.moveto = xw_moveto;
	pb.wpb.drawto = xw_drawto;
	pb.wpb.arrayplot = xw_arrayplot;
	pb.wpb.text = xw_text;
	pb.wpb.setstyle = xw_setstyle;
	pb.wpb.setcoo = xw_setcoo;
	pb.wpb.cleanup = xw_cleanup;
	(*gbv_playback)( wdw, wdw, &pb, NULL, status );

} /* end of xw_redraw */



/*----------------------------------------------------------------------------*/



void xw_prepare( WDW wdw, STATUS *status )

/* prepares window for redraw
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * STATUS     *status;  output; return status
 */
{
	/* executable code */

	xw_erase( wdw );

} /* end of xw_prepare */



/*----------------------------------------------------------------------------*/



void xw_cleanup( WDW wdw, char outf[], STATUS *status )

/* cleanup after redraw.  not used here
 *
 * parameters of routine
 * WDW        wdw;     input; window number
 * char       outf[];  output; name of output file
 * STATUS     *status; output; return status
 */
{

} /* end of xw_cleanup */



/*----------------------------------------------------------------------------*/



float xw_aspectratio( WDW wdw )

/* returns ratio of width to height
 *
 * parameter of routine
 * WDW       wdw;       input; window number
 */
{
	/* local variables */
	int      w, h;     /* size of window */

	/* executable code */

	wdw &= NUMMASK;
	xwh_getwdwsize( wdw, &w, &h );
	return  (float)w/(float)h;

} /* end of xw_aspectratio */



/*----------------------------------------------------------------------------*/



void xw_updatewdw( void )

/* updates windows if necessary
 * no parameters
 */
{
	/* local variables */
	XEvent   event;     /* event */
	int      rdw;       /* redraw window */
	int      rdmap;     /* redraw window map */
	int      dmy;

	/* executable code */

	if  (xwv_dsp == NULL)  return;

	rdmap = 0;
	while  (XCheckMaskEvent( xwv_dsp, ExposureMask, &event ))  {
		if  (event.type != Expose)
			printf( "*** program error in xw_updatewdw ***\n" );
		dmy = XWE_NOERROR;
		rdw = xwh_findwdw( event.xexpose.window );
		if  ((rdw >= 0) && !((1<<rdw) & rdmap))  {
			xw_redraw( rdw, &dmy );
			rdmap |= (1<<rdw);
		} /*endif*/
	} /*endwhile*/

} /* end of xw_updatewdw */



/*----------------------------------------------------------------------------*/



void xw_flushbuffers( void )

/* flushes all output buffers of graphic windows
 *
 * no parameters
 */
{
	/* executable code */

	XFlush( xwv_dsp );

} /* end of xw_flushbuffers */



/*----------------------------------------------------------------------------*/



void xw_arrayswap( BOOLEAN on_off )

/* switches arrayswap (swapping of x and y in xw_arrayplot) on or off
 *
 * parameters of routine
 * BOOLEAN    on_off;     input; TRUE=on, FALSE=off
 */
{
	/* executable code */

	xwv_arrayswap = on_off;

} /* end of xw_arrayswap */



/*----------------------------------------------------------------------------*/
/*                          local routines                                    */
/*----------------------------------------------------------------------------*/



static void xwh_transform( WDW wdw, XWCOO u, XWCOO v, int *x, int *y )

/* transforms user coordinates into window coordinates
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * XWCOO      u, v;     input; user coordinates
 * int        *x, *y;   output; window coordinates
 */
{
	/* executable code */

	*x = (int)(u*xwv_txfac[wdw] - xwv_txoff[wdw]);
	*y = (int)(v*xwv_tyfac[wdw] - xwv_tyoff[wdw]);

} /* end of xwh_transform */



/*----------------------------------------------------------------------------*/



static void xwh_backtrans( WDW wdw, int x, int y, XWCOO *u, XWCOO *v )

/* transforms window coordinates into user coordinates
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * int        x, y;     input; window coordinates
 * XWCOO      *u, *v;   output; user coordinates
 */
{
	/* executable code */

	*u = ((XWCOO)x + xwv_txoff[wdw]) / xwv_txfac[wdw];
	*v = ((XWCOO)y + xwv_tyoff[wdw]) / xwv_tyfac[wdw];

} /* end of xwh_backtrans */



/*----------------------------------------------------------------------------*/



static void xwh_getwdwsize( WDW wdw, int *ww, int *wh )

/* returns window size (in window coordinates)
 *
 * parameters of routine
 * int        wdw;       input; window number
 * int        *ww, *wh;  output; window size (if != NULL)
 */
{
	/* local variables */
	Window   root;        /* root window */
	int      x, y;        /* window pos */
	unsigned w, h;        /* window size */
	unsigned depth;       /* depth of window */
	unsigned border;      /* border width */

	/* executable code */

	XGetGeometry( xwv_dsp, xwv_wdw[wdw], &root, &x, &y, &w, &h,
		&border, &depth );

	if  (ww != NULL)  *ww = (int)w;
	if  (wh != NULL)  *wh = (int)h;

} /* end of xwh_getwdwsize */



/*----------------------------------------------------------------------------*/



static int xwh_wdwdepth( WDW wdw )

/* returns window depth
 *
 * parameters of routine
 * WDW        wdw;       input; window number
 *                       returns window depth
 */
{
	/* local variables */
	Window   root;        /* root window */
	int      x, y;        /* window pos */
	unsigned w, h;        /* window size */
	unsigned depth;       /* depth of window */
	unsigned border;      /* border width */

	/* executable code */

	XGetGeometry( xwv_dsp, xwv_wdw[wdw], &root, &x, &y, &w, &h,
		&border, &depth );

	return depth;

} /* end of xwh_wdwdepth */



/*----------------------------------------------------------------------------*/



static void xwh_newline( WDW wdw )

/* starts new output line in window
 *
 * parameters of routine
 * WDW        wdw;       input; window number
 */
{
	/* local variables */
	int      wh;    /* window height */
	int      fh;    /* font height */

	/* executable code */

	xwh_getwdwsize( wdw, NULL, &wh );
	xwv_wxpos[wdw] = XWRITEOFF;
	fh = xwh_getfontheight( DEFSTYLE );
	if  ((xwv_wypos[wdw]+fh+YWRITEOFF) > wh)  {
		xwh_scroll( wdw );
	} else {
		xwv_wypos[wdw] += fh;
	} /*endif*/

} /* end of xwh_newline */



/*----------------------------------------------------------------------------*/



static void xwh_scroll( WDW wdw )

/* scrolls window up
 *
 * parameters of routine
 * WDW        wdw;       input; window number
 */
{
	/* local variables */
	int      ww, wh;    /* window size */
	int      fh;        /* font height */

	/* executable code */

	xwh_getwdwsize( wdw, &ww, &wh );
	fh = xwh_getfontheight( DEFSTYLE );
	XCopyArea( xwv_dsp, xwv_wdw[wdw], xwv_wdw[wdw], xwv_gc[wdw],
		0, fh, ww, wh-fh, 0, 0 );
	XClearArea( xwv_dsp, xwv_wdw[wdw], 0, wh-fh, ww, fh, False );

} /* end of xwh_scroll */



/*----------------------------------------------------------------------------*/



static int xwh_getfontheight( int style )

/* returns font height of "style"
 *
 * parameters of routine
 * int        style;     input; attribute block number
 */
{
	/* executable code */

	return (int)(xwv_font[style]->max_bounds.ascent +
		xwv_font[style]->max_bounds.descent );

} /* end of xwh_getfontheight */



/*----------------------------------------------------------------------------*/



static int xwh_getfontwidth( int style )

/* returns font width of "style"
 *
 * parameters of routine
 * int        style;     input; attribute block number
 */
{
	/* executable code */

	return (int)(xwv_font[style]->max_bounds.rbearing -
		xwv_font[style]->max_bounds.lbearing );

} /* end of xwh_getfontwidth */



/*----------------------------------------------------------------------------*/



static void xwh_read_character( char *ch )

/* reads a single character from terminal
 *
 * parameters of routine
 * char       *ch;       output; character read
 */
{
	/* local constants */
#  define KEYBUFLTH 20

	/* local variables */
	XEvent      event;                /* event */
	XEvent      evcop;                /* dummy event */
	static char keybuf[KEYBUFLTH+1];  /* keyboard buffer */
	static int  keylth;               /* length of keyboard buffer */
	static char *cptr;                /* buffer pointer */
	BOOLEAN     quit;                 /* quit loop */
	WDW         rdw;                  /* redraw window */
	int         rdmap;                /* redraw window map */
	int         dmy=XWE_NOERROR;

	/* executable code */

	if  (keylth > 0)  {
		*ch = *cptr++;
		keylth--;
		return;
	} /*endif*/

	rdmap = 0;
	quit = FALSE;
	do  {

		XNextEvent( xwv_dsp, &event );

		switch  (event.type)  {
		case Expose:
			/* delete all other Expose events of the same window */
			while  (XCheckWindowEvent(xwv_dsp,event.xexpose.window,
				ExposureMask,&evcop))
			rdw = xwh_findwdw( event.xexpose.window );
/*         if  ((rdw >= 0) && !((1<<rdw) & rdmap))  {  */
			if  (rdw >= 0)  {
				xw_redraw( rdw, &dmy );
				rdmap |= (1<<rdw);
			} /*endif*/
			break;
		case ButtonPress:
			break;
		case KeyPress:
			keylth = XLookupString( &(event.xkey), keybuf, KEYBUFLTH, NULL, NULL );
			if  (keylth > 0)  {
				cptr = keybuf;
				*ch = *cptr++;
				keylth--;
				quit = TRUE;
			} /*endif*/
			break;
		case MotionNotify:
			break;
		} /*endswitch*/

	}  while  (!quit);

} /* end of xwh_read_character */



/*----------------------------------------------------------------------------*/



static void xwh_drawcross( WDW wdw, GC gc, int x, int y )

/* draws crosshair cursor in window
 *
 * parameters of routine
 * WDW        wdw;       input; window number
 * GC         gc;        input; graphics context
 * int        x, y;      input; cross coordinates
 */
{
	/* local variables */
	int      ww, wh;    /* size of window */

	/* executable code */

	xwh_getwdwsize( wdw, &ww, &wh );
	XDrawLine( xwv_dsp, xwv_wdw[wdw], gc, 0, y, ww, y );
	XDrawLine( xwv_dsp, xwv_wdw[wdw], gc, x, 0, x, wh );

} /* end of xwh_drawcross */



/*----------------------------------------------------------------------------*/



static void xwh_setcolor( int style, char value[], STATUS *status )

/* defines a new foreground color for style number "style"
 *
 * parameters of routine
 * int        style;     input; style number
 * char       value[];   input; new color value (text or red,green,blue)
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	XColor   screen_color, exact_color;   /* color values */
	Status   xstatus;                     /* X return status */
	float    r, g, b;                     /* red, green, blue */

	/* executable code */

	if  (!xwv_colored)  return;

	if  (isdigit(*value) || *value == '.')  {
		if  (sscanf( value, "%f,%f,%f", &r, &g, &b ) != 3)  {
			*status = XWE_ILCOLOR;
			return;
		} else if  ((r < 0.0) || (r > 1.0) || (g < 0.0) || (g > 1.0) || (b < 0.0)
			|| (b > 1.0))  {
			*status = XWE_ILCOLOR;
			return;
		} /*endif*/
		screen_color.red = (unsigned short)(r * 65535.0);
		screen_color.green = (unsigned short)(g * 65535.0);
		screen_color.blue = (unsigned short)(b * 65535.0);
		xstatus = XAllocColor( xwv_dsp, xwv_cmap, &screen_color );
	} else {
		xstatus = XAllocNamedColor( xwv_dsp, xwv_cmap, value, &screen_color,
			&exact_color );
	} /*endif*/
#ifdef XXX
	if  (xstatus == 0)  {
		*status = XWE_COLALLOC;
		return;
	} /*endif*/
#endif
	XSetForeground( xwv_dsp, xwv_gc[style], screen_color.pixel );

} /* end of xwh_setcolor */



/*----------------------------------------------------------------------------*/



static WDW xwh_findwdw( Window xwdw )

/* returns XW window index from XWindow ID, if not found returns -1
 *
 * parameters of routine
 * Window     xwdw;    input; XWindow ID
 *                     returns window index
 */
{
	/* local variables */
	WDW      w;      /* window counter */

	/* executable code */

	for  (w=0; w<XWC_MAXWDW; w++)
		if  (xwv_wdw[w] == xwdw)
			return w;

	return -1;

} /* end of xwh_findwdw */



/*----------------------------------------------------------------------------*/



static int xwh_getfontnum( WDW wdw, float size )

/* returns font number
 * parameters of routine
 * WDW        wdw;       input; window number
 * float      size;      input; character size
 *                       returns font number
 */
{
	/* local constants */
#  define   MINSIZE 9
#  define   MAXSIZE 40

	/* local variables */
	int        wh;             /* window height */
	int        fh;             /* font height */
	static int fn[MAXSIZE+1]=  /* font numbers */
		{0,0,0,0,0,0,0,0,0,37,31,32,407,159,33,256,262,257,275,258,35,35,35,
	 /* 0 1 2 3 4 5 6 7 8  9 10 11 12  13  14 15  16  17  18  19  20 21 22 */
		 30,30,36,265,265,397,397,393,393,260,266,266,266,430,430,198,198,394};
	 /* 23 24 25 26  27  28  29  30  31  32  33  34  35  36  37  38  39  40 */
	FILE       *fp;                 /* file pointer */
	char       **fontlist;          /* list of fonts */
	int        cnt;                 /* number of fonts */
	int        i;                   /* counter */
	char       line[BC_LINELTH+1];  /* current line */
	char       fname[BC_LINELTH+1]; /* name of font */
	int        psize;               /* size in pixel */
	char       listfile[BC_FILELTH+1];  /* name of font list file */

	/* executable code */

	size *= 2;   /* don't know why */

	xwh_getwdwsize( wdw, NULL, &wh );
	/* fh = Nint( size * (float)wh ); */
	fh = (int)( size * (float)wh );
	if  (fh < MINSIZE)  fh = MINSIZE;
	if  (fh > MAXSIZE)  fh = MAXSIZE;

	if  (strlen(xwv_inputdir)+strlen(XWC_FONTLIST) > BC_FILELTH)  {
		printf( "*** %s + %s : name overflow ***\n", xwv_inputdir, XWC_FONTLIST );
		return fn[fh];
	} /*endif*/
	strcpy( listfile, xwv_inputdir );
	strcat( listfile, XWC_FONTLIST );
	fp = sy_fopen( listfile, "r" );
	if  (fp == NULL)  {
		printf( "*** no xfonts.lis ***\n" );
		return fn[fh];
	} else {
		do  {
			if  (fgets(line,BC_LINELTH,fp) == NULL)  {
				printf( "*** size %d didn't match ***\n", fh );
				sy_fclose( fp );
				return fn[fh];
			} /*endif*/
			sscanf(line, "%d %s", &psize, fname );
		}  while (psize != fh);
		sy_fclose( fp );
		fontlist = XListFonts( xwv_dsp, "*", 1000, &cnt );
		for  (i=0; i<cnt; i++)
			if  (strcmp(fname,fontlist[i]) == 0)  {
				XFreeFontNames( fontlist );
				/*printf("*** font found ***\n" );*/
				return i;
			} /*endif*/
		XFreeFontNames( fontlist );
		return fn[fh];
	} /*endif*/

} /* end of xwh_getfontnum */



/*----------------------------------------------------------------------------*/



void xw_set_inputdir( char dir[], STATUS *status )

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name
 * STATUS    *status;   output; return status
 */
{
	/* executable code */

	if  (strlen(dir) > BC_FILELTH)  {
		*status = XWE_STROVFL;
		return;
	} /*endif*/
	strcpy( xwv_inputdir, dir );

} /* end of xw_set_inputdir */



/*----------------------------------------------------------------------------*/


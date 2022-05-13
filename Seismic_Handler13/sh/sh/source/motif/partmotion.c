
/* file partmotion.c
 *      ============
 *
 * version 5, 29-Jan-2007
 *
 * Particle motion window
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


#include <stdio.h>
#include <string.h>
#include <math.h>
#include "basecnst.h"
#include "sysbase.h"
#include <Xm/Xm.h>
#include "infoidx.h"
#include "fctxdm.h"
#include "scusrdef.h"
#include "motifgraph.h"
#include "partmotion.h"
#include "shm_widgets.h"
#include "pixmaps.h"
#include "globalparams.h"




/* constants */
#ifdef XXX
#define LEFT_MARGIN 100
#define RIGHT_MARGIN 40
#define TOP_MARGIN 40
#define BOTTOM_MARGIN 100
#endif
#define LEFT_MARGIN 10
#define RIGHT_MARGIN 10
#define TOP_MARGIN 10
#define BOTTOM_MARGIN 10

#define BOXSEP 0.03
#define MAXPM 3
#define MAXSTRLTH 10

#define MAXCOL 10



/* types */

typedef struct {
	int       wx;         /* window x-coordinate */
	int       wy;         /* window y-coordinate */
	int       ww;         /* window width */
	int       wh;         /* window height */
} TPmmWindow;

typedef struct {
	float    xo;          /* x offset */
	float    yo;          /* y offset */
	float    xf;          /* x scaling */
	float    yf;          /* y scaling */
} TPmmScale;



/* global variables */
static Widget     pmmv_root_wdw; /* root window of drawing area */
static Widget     pmmv_draw_wdw; /* drawing area */
static Colormap   pmmv_cmap;     /* color map */
static TSyBoolean pmmv_is_init=FALSE;  /* module is initialized */
static GC         pmmv_gc;       /* drawing GC */
static GC         pmmv_drgc;     /* drawing GC */
static GC         pmmv_colgc[MAXCOL];    /* colored lines GC's */
static TSyBoolean pmmv_use_zoom=FALSE;   /* use zoom scale */
static TSyBoolean pmmv_use_colors=TRUE;  /* use colored lines */
static TSyBoolean pmmv_col_init=FALSE;   /* color initialized */
static TPmmWindow pmmv_twdw;     /* total window size */
static TPmmWindow pmmv_ewdw;     /* effective window size */
static TPmmScale  pmmv_scale;    /* scaling */
static int        pmmv_no_of_pm=3; /* number of hodograms per window */



/* prototypes of local routines */
static void pmm_transform( float x, float y, int *px, int *py );
static void pmm_make_scaling( float displaysize );
static void pmm_drawcurve( float x[], float y[], int lth, float xorig,
	float yorig, float norm, char cmt[] );
static pmm_init_colors( void );
static void pmm_draw_colored_lines( XPoint pxy[], int lth );



/*----------------------------------------------------------------------------*/



void pmm_initialize( Widget parent, Widget pmwdw, STATUS *status )

/* initializes particle motion window
 *
 * parameters of routine
 * Widget     parent;      input; widget ID of root window
 * Widget     pmwdw;       input; Widget ID of particle motion window
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	XColor   screen_color;         /* color */
	int      xstatus;              /* return status for color allocation */

	/* executable code */

	pmmv_root_wdw = parent;
	pmmv_draw_wdw = pmwdw;

	if  (!XtIsManaged(parent))
		XtManageChild( parent );

	if  (pmmv_is_init)  return;

	/* get color map */
	pmmv_cmap = XDefaultColormapOfScreen( XtScreen(pmwdw) );

	/* GC for axes and labels */
	pmmv_gc = XCreateGC( XtDisplay(pmwdw), XtWindow(pmwdw), 0 , NULL );
	XSetForeground( XtDisplay(pmwdw), pmmv_gc, pix_colour(PIXC_COL_FOREGROUND) );
	XSetBackground( XtDisplay(pmwdw), pmmv_gc, pix_colour(PIXC_COL_BACKGROUND) );

	/* GC for particle motion lines */
	pmmv_drgc = XCreateGC( XtDisplay(pmwdw), XtWindow(pmwdw), 0, NULL );
	XSetBackground( XtDisplay(pmwdw), pmmv_drgc,
			XWhitePixelOfScreen(XtScreen(pmwdw)) );
	screen_color.red = 65535;
	screen_color.green = screen_color.blue = 0;
	screen_color.flags = DoRed | DoGreen | DoBlue;
	xstatus = XAllocColor( XtDisplay(pmmv_draw_wdw), pmmv_cmap, &screen_color );
	if  (xstatus == 0)  {
		XSetForeground( XtDisplay(pmmv_draw_wdw), pmmv_drgc,
			XBlackPixelOfScreen(XtScreen(pmmv_draw_wdw)) );
	} else {
		XSetForeground( XtDisplay(pmmv_draw_wdw), pmmv_drgc,
			screen_color.pixel );
	} /*endif*/

	pix_create_window_buffer( XtDisplay(pmwdw), XtWindow(pmwdw),
		TRUE, status );

	pmmv_is_init = TRUE;

} /* end of pmm_initialize */



/*----------------------------------------------------------------------------*/



void pmm_close_window( void )

/* Closes particle motion window
 *
 * parameters of routine
 * none
 */
{
	/* executable code */

	if  (XtIsManaged(pmmv_root_wdw))  XtUnmanageChild(pmmv_root_wdw);

} /* end of pmm_close_window */



/*----------------------------------------------------------------------------*/


#define Z 0
#define N 1
#define E 2
#define TMP 3
#define MX 3
#define LT 4
#define PMSEP 0.25



void pmm_draw_pmotion( float defzoom )

/* Draws particle motion diagram(s) into separate window
 *
 * parameters of routine
 * float      defzoom;        input; default zoom value for display
 */
{
	/* local variables */
	int      trcno[LT];      /* trace numbers for z,n,e */
	void     *trcptr[LT];    /* pointer to traces */
	char     comp[LT];       /* components */
	float    *smp[LT];       /* pointer to samples */
	float    *x[MAXPM];      /* x-coordinates */
	float    *y[MAXPM];      /* y-coordinates */
	float    xorig[MAXPM];   /* x-origins */
	float    yorig[MAXPM];   /* y-origins */
	char     cmt[MAXPM][MAXSTRLTH+1];  /* label text */
	char     str[cBcLineLth+1];        /* scratch string */
	int      no_of_pm;       /* number of diagrams */
	TSyBoolean dragset;      /* drag window set */
	float    stime;          /* drag window start time in sec */
	float    etime;          /* drag window end time in sec */
	int      i, t, l, c;     /* counters */
	long     spos;           /* sample position */
	TSyStatus locstat;       /* local status */
	float    totmax;         /* absolute maximum */
	float    trcmin, trcmax; /* min, max of trace */
	float    norm;           /* normalization */
	int      lth;            /* length of pm plot in samples */
	float    dt;             /* sample distance in sec */
	int      line_pm;        /* number of pm's per line */
	float    displaysize;    /* size of display in user units */
	float    swdw, ewdw;     /* current setting of time window */
	float    zoom;           /* zoom value */

	/* executable code */

	mg_get_drag_trace( trcno+Z, &dragset );
	if  (!dragset)  return;
	mg_find_3_traces( trcno[Z], trcno+N, trcno+E );
	if  (trcno[N] == -1 || trcno[E] == -1)  return;

	mg_get_drag_window( &stime, &etime, &dragset );

	/* get trace pointers */
	for  (i=0; i<MX; i++)  trcptr[i] = mg_trcptr( trcno[i] );
	/* sort traces z,n,e */
	locstat = cBcNoError;
	for  (i=0; i<MX; i++)  comp[i] = db_getc( trcptr[i], EC_COMP, &locstat );
	comp[TMP] = '\0';
	if  (comp[Z] != 'Z')  {
		if  (comp[N] == 'Z')  {
			i = N;
		} else if  (comp[E] == 'Z')  {
			i = E;
		} else {
			printf( "*SHM: pmm_draw_pmotion: no Z component\n" );
			return;
		} /*endif*/
		/* flip i and Z */
		comp[TMP] = comp[i];
		comp[i] = comp[Z];
		comp[Z] = comp[TMP];
		trcptr[TMP] = trcptr[i];
		trcptr[i] = trcptr[Z];
		trcptr[Z] = trcptr[TMP];
		trcno[TMP] = trcno[i];
		trcno[i] = trcno[Z];
		trcno[Z] = trcno[TMP];
	} /*endif*/
	if  (comp[N] != 'N')  {
		/* flip N and E */
		comp[TMP] = comp[N];
		comp[N] = comp[E];
		comp[E] = comp[TMP];
		trcptr[TMP] = trcptr[N];
		trcptr[N] = trcptr[E];
		trcptr[E] = trcptr[TMP];
		trcno[TMP] = trcno[N];
		trcno[N] = trcno[E];
		trcno[E] = trcno[TMP];
	} /*endif*/

	/* find maximum value & normalisation */
	dm_get_timewdw( &swdw, &ewdw );
	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: swdw:%f, ewdw:%f\n", swdw, ewdw );
	totmax = 0.0;
	c = 0;
	if  (pmmv_no_of_pm == 1)  c = 1;
	for  (i=c; i<MX; i++)  {
		spos = dm_getsample( trcptr[i], swdw, TRUE );
		lth = dm_getsample( trcptr[i], ewdw, TRUE ) - spos;
		smp[i] = db_getp( trcptr[i], EP_DATA, &locstat );
		if  (SySevere(&locstat))  return;
		sl_findmax( smp[i]+spos, lth, &trcmin, &trcmax );
		trcmin = Abs(trcmin);
		trcmax = Abs(trcmax);
		if  (trcmin > trcmax)
			trcmax = trcmin;
		if  (trcmax > totmax)
			totmax = trcmax;
	} /*endfor*/
	if  (totmax <= 0.0)  return;
	norm = 0.5 / totmax;

	/* get length of traces */
	dt = db_getr( trcptr[Z], ER_DELTA, &locstat );
	if  (SySevere(&locstat))  return;
	lth = Nint( (etime-stime)/dt );
	zoom = (pmmv_use_zoom) ? defzoom : 1.0;

	/* get samples */
	for  (i=0; i<MX; i++)  {
		smp[i] = db_getp( trcptr[i], EP_DATA, &locstat );
		if  (SySevere(&locstat))  return;
		spos = dm_sgetsample( trcptr[i], stime, &locstat );
		if  (SySevere(&locstat))  return;
		smp[i] += spos;
		spos = dm_sgetsample( trcptr[i], etime, &locstat );
		if  (SySevere(&locstat))  {
			printf( "*SHM: partmotion: no sample for end times\n" );
			return;
		} /*endif*/
	} /*endfor*/

	/* setup x- and y-coordinates */
	no_of_pm = pmmv_no_of_pm;
	x[0] = smp[E];  y[0] = smp[N];
	x[1] = smp[N];  y[1] = smp[Z];
	x[2] = smp[E];  y[2] = smp[Z];

	/* get label text */
	db_gets( trcptr[Z], ES_STATION, cBcLineLth, str, &locstat );
	if  (SySevere(&locstat))  return;
	if  (strlen(str) > 5)  str[5] = '\0';
	sprintf( cmt[0], "%s %c-%c", str, comp[E], comp[N] );
	sprintf( cmt[1], "%s %c-%c", str, comp[N], comp[Z] );
	sprintf( cmt[2], "%s %c-%c", str, comp[E], comp[Z] );

	/* compute number of pm's per line & size of display*/
	line_pm = 1;
	while  ((line_pm*line_pm) < no_of_pm)
		line_pm++;
	displaysize = line_pm + (line_pm+1)*PMSEP;

	/* compute origins */
	for  (t=0; t<no_of_pm; t++)  {
		l = t / line_pm + 1;
		c = t % line_pm + 1;
		xorig[t] = (float)c * PMSEP + ((float)c - 0.5);
		yorig[t] = displaysize - (float)l * PMSEP - ((float)l - 0.5);
	} /*endfor*/

	/* draw it */
	pmm_make_scaling( displaysize );
	pix_ClearWindow( XtDisplay(pmmv_draw_wdw), XtWindow(pmmv_draw_wdw) );
	for  (t=0; t<no_of_pm; t++)  {
#ifdef XXX
		if  (begcirc > 0.)
			pmh_drawcircle( ch, xorig[t]+x[t][0]*norm*zoom[t],
			yorig[t]+y[t][0]*norm*zoom[t], begcirc );
#endif
		pmm_drawcurve( x[t], y[t], lth, xorig[t], yorig[t], zoom*norm, cmt[t] );
	} /*endfor*/

} /* end of pmm_draw_pmotion */


#undef Z
#undef N
#undef E
#undef MX
#undef LT



/*----------------------------------------------------------------------------*/



static void pmm_make_scaling( float displaysize )

/* Computes window sizes and scaling variables, sets clipping rectangle on
 * all GC's.
 *
 * parameters of routine
 * none (changes global variables)
 */
{
	/* local variables */
	/*XRectangle   rect;*/      /* clipping rectangle */

	/* executable code */

	/* determine window sizes */
	mg_get_windowsize( pmmv_draw_wdw, &pmmv_twdw.ww, &pmmv_twdw.wh );
	pmmv_twdw.wx = pmmv_twdw.wy = 0;
	pmmv_ewdw.wx = LEFT_MARGIN;
	pmmv_ewdw.wy = BOTTOM_MARGIN;
	pmmv_ewdw.ww = pmmv_twdw.ww - LEFT_MARGIN - RIGHT_MARGIN;
	pmmv_ewdw.wh = pmmv_twdw.wh - TOP_MARGIN - BOTTOM_MARGIN;

	/* determine scaling */
	pmmv_scale.xo = 0.0;
	pmmv_scale.yo = 0.0;
	pmmv_scale.xf = (float)pmmv_ewdw.ww / (displaysize - pmmv_scale.xo);
	pmmv_scale.yf = (float)pmmv_ewdw.wh / (displaysize - pmmv_scale.yo);

	/* save aspect ratio */
	if  (pmmv_scale.xf < pmmv_scale.yf)  {
		pmmv_scale.yf = pmmv_scale.xf;
	} else {
		pmmv_scale.xf = pmmv_scale.yf;
	} /*endif*/

#ifdef XXX
	/* loop all traces and set clipping rectangle */
	rect.x = spcv_ewdw.wx;
	rect.y = spcv_twdw.wh - spcv_ewdw.wy - spcv_ewdw.wh;
	rect.width = spcv_ewdw.ww;
	rect.height = spcv_ewdw.wh;
	trc = spcv_roottrc;
	while  (trc != NULL)  {
		pix_SetClipRectangles( XtDisplay(spcv_draw_wdw), trc->gc, 0, 0, &rect, 1,
			Unsorted );
		trc = trc->next;
	} /*endwhile*/
#endif

} /* end of pmm_make_scaling */



/*----------------------------------------------------------------------------*/



static void pmm_transform( float x, float y, int *px, int *py )

/* Transforms input user coordinates into window pixel coordinates
 *
 * parameters of routine
 * float      x, y;      input; user coordinates
 * int        *px, *py;  output; window coordinates
 */
{
	/* local variables */

	/* executable code */

	x = (x - pmmv_scale.xo) * pmmv_scale.xf;
	y = (y - pmmv_scale.yo) * pmmv_scale.yf;
	*px = Nint(x) + pmmv_ewdw.wx;
	*py = pmmv_twdw.wh - (Nint(y) + pmmv_ewdw.wy);

} /* end of pmm_transform */



/*----------------------------------------------------------------------------*/



static void pmm_drawcurve( float x[], float y[], int lth, float xorig,
	float yorig, float norm, char cmt[] )

/* Draws particle motion diagram
 *
 * parameters of routine
 * float      x[], y[];     input; x- and y-positions to draw
 * int        lth;          input; length of arrays
 * float      xorig, yorig; input; origin for drawing
 * float      norm;         input; normalization to be applied
 * char       cmt[];        input; label
 */
{
	/* local variables */
	static XPoint *pxy=NULL;         /* pointer to x,y positions */
	static int    palloc=0;          /* number of points allocated */
	TSyStatus     locstat;           /* local status */
	int           i;                 /* counter */
	int           px, py;            /* integer positions */
	float         sep;               /* separation distance */
	float         azim;              /* azimuth found */
	char          str[cBcLineLth+1]; /* label text */

	/* executable code */

	locstat = cBcNoError;

	if  (palloc < lth)  {
		if  (pxy != NULL)  sy_deallocmem( pxy );
		palloc = 0;
		pxy = (XPoint *)sy_allocmem( lth, (int)sizeof(XPoint), &locstat );
		if  (SySevere(&locstat))  return;
		palloc = lth;
	} /*endif*/

	for  (i=0; i<lth; i++)  {
		pmm_transform( xorig+norm*x[i], yorig+norm*y[i], &px, &py );
		pxy[i].x = px;
		pxy[i].y = py;
	} /*endfor*/

	if  (pmmv_use_colors)  {
		pmm_draw_colored_lines( pxy, lth );
	} else {
		pix_DrawLines( XtDisplay(pmmv_draw_wdw), XtWindow(pmmv_draw_wdw),
			pmmv_drgc, pxy, lth, CoordModeOrigin );
	} /*endif*/

	/* draw frame */
	sep = 0.5 + BOXSEP;
	if  (palloc < 5)  {
		printf( "*SHM: pmm_drawcurve: palloc < 5, should not happen\n" );
		return;
	} /*endif*/
	pmm_transform( xorig-sep, yorig-sep, &px, &py );
	pxy[0].x = px; pxy[0].y = py;
	pmm_transform( xorig+sep, yorig-sep, &px, &py );
	pxy[1].x = px; pxy[1].y = py;
	pmm_transform( xorig+sep, yorig+sep, &px, &py );
	pxy[2].x = px; pxy[2].y = py;
	pmm_transform( xorig-sep, yorig+sep, &px, &py );
	pxy[3].x = px; pxy[3].y = py;
	pmm_transform( xorig-sep, yorig-sep, &px, &py );
	pxy[4].x = px; pxy[4].y = py;
	pix_DrawLines( XtDisplay(pmmv_draw_wdw), XtWindow(pmmv_draw_wdw),
		pmmv_gc, pxy, 5, CoordModeOrigin );

	/* compute azimuth & write it to label text */
	sc_polar2( y, x, lth, &azim, NULL );
	sprintf( str, "%s  angle %5.1f (+180)", cmt, azim );

	/* write label */
	if  (cmt != NULL)  {
		pmm_transform( xorig-0.45-BOXSEP, yorig-0.5-BOXSEP/2.0, &px, &py );
		pix_DrawString( XtDisplay(pmmv_draw_wdw), XtWindow(pmmv_draw_wdw),
			pmmv_gc, px, py, str, (int)strlen(str) );
	} /*endif*/

} /* end of pmm_drawcurve */



/*----------------------------------------------------------------------------*/



static pmm_init_colors( void )

/* initializes colors for colored line drawing
 *
 * no parameters
 */
{
	/* local variables */
	int      i;                    /* counter */
	XColor   screen_color;         /* color */
	int      xstatus;              /* return status for color allocation */

	/* executable code */

	for  (i=0; i<MAXCOL; i++)  {
		/* create GC and set color */
		pmmv_colgc[i] = XCreateGC( XtDisplay(pmmv_draw_wdw),
			XtWindow(pmmv_draw_wdw), 0, NULL );
		XSetBackground( XtDisplay(pmmv_draw_wdw), pmmv_drgc,
			XWhitePixelOfScreen(XtScreen(pmmv_draw_wdw)) );
		screen_color.red = Nint( (float)i/(float)(MAXCOL-1) * 65535.0 );
		screen_color.blue = 0.0; /* Nint( ((float)i-(float)(MAXCOL-1)/2.0
			+ (float)(MAXCOL-1)/2.0) * 65535.0/5.0 ); */
		screen_color.green = Nint( (float)(MAXCOL-1-i)/(float)(MAXCOL-1) * 65535 );
		screen_color.flags = DoRed | DoGreen | DoBlue;
		xstatus = XAllocColor( XtDisplay(pmmv_draw_wdw), pmmv_cmap, &screen_color );
		if  (xstatus == 0)  {
			XSetForeground( XtDisplay(pmmv_draw_wdw), pmmv_colgc[i],
				XBlackPixelOfScreen(XtScreen(pmmv_draw_wdw)) );
		} else {
			XSetForeground( XtDisplay(pmmv_draw_wdw), pmmv_colgc[i],
				screen_color.pixel );
		} /*endif*/
	} /*endfor*/

	pmmv_col_init = TRUE;

} /* end of pmm_init_colors */



/*----------------------------------------------------------------------------*/


static void pmm_draw_colored_lines( XPoint pxy[], int lth )

/* Draws colored lines
 *
 * parameters of routine
 * XPoint     pxy[];          input; x-,y-array
 * int        lth;            input; length of array
 */
{
	/* local variables */
	int      n, c;             /* counters */
	int      wl;               /* window length in samples */

	/* executable code */

	if  (!pmmv_col_init)  pmm_init_colors();

	wl = lth / MAXCOL;
	if  (wl < 2)  wl = 2;
	n = 0;

	for  (c=0; c<MAXCOL-1; c++)  {
		if  (n+wl > lth)  break;
		pix_DrawLines( XtDisplay(pmmv_draw_wdw), XtWindow(pmmv_draw_wdw),
			pmmv_colgc[c], pxy+n, wl, CoordModeOrigin );
		n += wl-1;
	} /*endfor*/
	if  (n < lth-1)  {
		pix_DrawLines( XtDisplay(pmmv_draw_wdw), XtWindow(pmmv_draw_wdw),
			pmmv_colgc[MAXCOL-1], pxy+n, lth-n, CoordModeOrigin );
	} /*endif*/

} /* end of pmm_draw_colored_lines */


/*----------------------------------------------------------------------------*/



void pmm_set_mode( int mode )

/* Configuring some parameters
 *
 * parameters of routine
 * int        mode;         input; mode to set
 */
{
	/* executable code */

	switch  (mode)  {
	case cPmmModeLineColored:
		pmmv_use_colors = TRUE;
		break;
	case cPmmModeLineMono:
		pmmv_use_colors = FALSE;
		break;
	case cPmmModeZoomOn:
		pmmv_use_zoom = TRUE;
		break;
	case cPmmModeZoomOff:
		pmmv_use_zoom = FALSE;
		break;
	case cPmmModePlot1:
		pmmv_no_of_pm = 1;
		break;
	case cPmmModePlot3:
		pmmv_no_of_pm = 3;
		break;
	default:
		printf( "*SHM: pmm_set_mode: unknown mode %d\n", mode );
		break;
	} /*endswitch*/

} /* end of pmm_set_mode */



/*----------------------------------------------------------------------------*/

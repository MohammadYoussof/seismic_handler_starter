
/* file polarfil.c
 *      ==========
 *
 * version 4, 22-May-2006
 *
 * Polarisation filters
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
#include "polarfil.h"
#include "shm_widgets.h"
#include "../pfusrdef.h"




/* constants */
#define LEFT_MARGIN 10
#define RIGHT_MARGIN 10
#define TOP_MARGIN 10
#define BOTTOM_MARGIN 10

#define BOXSEP 0.03
#define MAXPM 3
#define MAXSTRLTH 10

#define MAXCOL 10

#define MINLTH 1000



/* types */

typedef struct {
	int       wx;         /* window x-coordinate */
	int       wy;         /* window y-coordinate */
	int       ww;         /* window width */
	int       wh;         /* window height */
} TPolWindow;

typedef struct {
	float    xo;          /* x offset */
	float    yo;          /* y offset */
	float    xf;          /* x scaling */
	float    yf;          /* y scaling */
} TPolScale;



/* global variables */
static Widget     polv_root_wdw; /* root window of drawing area */
static Widget     polv_draw_wdw; /* drawing area */
static Colormap   polv_cmap;     /* color map */
static TSyBoolean polv_is_init=FALSE;  /* module is initialized */
static GC         polv_gc;       /* drawing GC */
static GC         polv_drgc;     /* drawing GC */
static TPolWindow polv_twdw;     /* total window size */
static TPolWindow polv_ewdw;     /* effective window size */
static TPolScale  polv_scale;    /* scaling */
static float      *polv_dat[MAXPM];  /* temp. data for 3 components */
static int        polv_datlth=0; /* length of above arrays */
/* filter parameters */
static int        polv_cohlth=10; /* coherence length in samples */
static float      polv_pow_linfil=1.0; /* power of linearity filter */



/* prototypes of local routines */
static void pol_transform( float x, float y, int *px, int *py );
static void pol_make_scaling( float displaysize_x, float displaysize_y );
static void pol_drawcurve( float dt, float y[], int lth, float xorig,
	float yorig, float norm, char cmt[] );



/*----------------------------------------------------------------------------*/



void pol_initialize( Widget parent, Widget polwdw, STATUS *status )

/* initializes polarisation filter window
 *
 * parameters of routine
 * Widget     parent;      input; widget ID of root window
 * Widget     polwdw;      input; Widget ID of polarisation filter window
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	XColor   screen_color;         /* color */
	int      xstatus;              /* return status for color allocation */
	int      i;                    /* counter */

	/* executable code */

	polv_root_wdw = parent;
	polv_draw_wdw = polwdw;

	if  (!XtIsManaged(parent))
		XtManageChild( parent );

	if  (polv_is_init)  return;

	/* get color map */
	polv_cmap = XDefaultColormapOfScreen( XtScreen(polwdw) );

	/* GC for axes and labels */
	polv_gc = XCreateGC( XtDisplay(polwdw), XtWindow(polwdw), 0 , NULL );
	XSetForeground( XtDisplay(polwdw), polv_gc,
			XBlackPixelOfScreen(XtScreen(polwdw)) );
	XSetBackground( XtDisplay(polwdw), polv_gc,
			XWhitePixelOfScreen(XtScreen(polwdw)) );

	/* GC for particle motion lines */
	polv_drgc = XCreateGC( XtDisplay(polwdw), XtWindow(polwdw), 0, NULL );
	XSetBackground( XtDisplay(polwdw), polv_drgc,
			XWhitePixelOfScreen(XtScreen(polwdw)) );
	screen_color.red = 65535;
	screen_color.green = screen_color.blue = 0;
	screen_color.flags = DoRed | DoGreen | DoBlue;
	xstatus = XAllocColor( XtDisplay(polv_draw_wdw), polv_cmap, &screen_color );
	if  (xstatus == 0)  {
		XSetForeground( XtDisplay(polv_draw_wdw), polv_drgc,
			XBlackPixelOfScreen(XtScreen(polv_draw_wdw)) );
	} else {
		XSetForeground( XtDisplay(polv_draw_wdw), polv_drgc,
			screen_color.pixel );
	} /*endif*/

	pix_create_window_buffer( XtDisplay(polwdw), XtWindow(polwdw),
		TRUE, status );

	/* allocate memory for data traces */
	for  (i=0; i<MAXPM; i++)  {
		polv_dat[i] = (float *)sy_allocmem( MINLTH, (int)sizeof(float), status );
		if  (SySevere(status))  return;
	} /*endif*/
	polv_datlth = MINLTH;

	polv_is_init = TRUE;

} /* end of pol_initialize */



/*----------------------------------------------------------------------------*/



void pol_close_window( void )

/* Closes polarisation filter window
 *
 * parameters of routine
 * none
 */
{
	/* executable code */

	if  (XtIsManaged(polv_root_wdw))  XtUnmanageChild(polv_root_wdw);

} /* end of pol_close_window */



/*----------------------------------------------------------------------------*/


#define Z 0
#define N 1
#define E 2
#define TMP 3
#define MX 3
#define LT 4
#define PMSEP 0.25



void pol_draw_poltraces( float defzoom, TSyStatus *status )

/* Draws polarisation filtered traces into separate window
 *
 * parameters of routine
 * float      defzoom;        input; default zoom value for display
 * TSyStatus  *status;        output; return status
 */
{
	/* local variables */
	int      trcno[LT];      /* trace numbers for z,n,e */
	void     *trcptr[LT];    /* pointer to traces */
	char     comp[LT];       /* components */
	float    *smp[LT];       /* pointer to samples */
	float    xorig[MAXPM];   /* x-origins */
	float    yorig[MAXPM];   /* y-origins */
	char     cmt[MAXPM][MAXSTRLTH+1];  /* label text */
	char     str[cBcLineLth+1];        /* scratch string */
	int      no_of_pm=3;     /* number of diagrams */
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
	float    displaysize_x;  /* size of display in user units */
	float    displaysize_y;  /* size of display in user units */
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
			printf( "*SHM: pmm_poltraces: no Z component\n" );
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

#ifdef XXX
	/* find maximum value & normalisation */
	dm_get_timewdw( &swdw, &ewdw );
	totmax = 0.0;
	c = 0;
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
#endif

	/* get length of traces */
	dt = db_getr( trcptr[Z], ER_DELTA, &locstat );
	if  (SySevere(&locstat))  return;
	lth = Nint( (etime-stime)/dt );
	zoom = defzoom;

	/* get samples */
	for  (i=0; i<MX; i++)  {
		smp[i] = db_getp( trcptr[i], EP_DATA, &locstat );
		if  (SySevere(&locstat))  return;
		spos = dm_sgetsample( trcptr[i], stime, &locstat );
		if  (SySevere(&locstat))  return;
		smp[i] += spos;
		spos = dm_sgetsample( trcptr[i], etime, &locstat );
		if  (SySevere(&locstat))  {
			printf( "*SHM: polarfil: no sample for end times\n" );
			return;
		} /*endif*/
	} /*endfor*/

	/* get label text */
	db_gets( trcptr[Z], ES_STATION, cBcLineLth, str, &locstat );
	if  (SySevere(&locstat))  return;
	if  (strlen(str) > 5)  str[5] = '\0';
	sprintf( cmt[0], "%s-Z", str );
	sprintf( cmt[1], "%s-N", str );
	sprintf( cmt[2], "%s-E", str );

	/* compute number of pm's per line & size of display*/
	displaysize_x = (float)lth*dt;
	line_pm = 3;
	displaysize_y = (float)line_pm + (float)(line_pm+1)*PMSEP;

	/* compute origins */
	for  (t=0; t<no_of_pm; t++)  {
		xorig[t] = 0.0;
		yorig[t] = displaysize_y - (float)(t+1)*PMSEP - (float)t - 0.5;
	} /*endfor*/

	/* get memory for filtered output traces */
	if  (lth > polv_datlth)  {
		polv_datlth = lth / MINLTH + 1;
		polv_datlth *= MINLTH;
		for  (t=0; t<no_of_pm; t++)  {
			sy_deallocmem( polv_dat[t] );
			polv_dat[t] = (float *)sy_allocmem( polv_datlth, (int)sizeof(float),
				status );
			if  (SySevere(status))  return;
		} /*endfor*/
	} /*endif*/

	pf_filter( smp[Z], smp[N], smp[E], polv_dat[Z], polv_dat[N], polv_dat[E],
		lth, polv_cohlth, polv_pow_linfil, 500 );

	/* find normalisation */
	totmax = 0.0;
	for  (i=0; i<MX; i++)  {
		trcmin = trcmax = polv_dat[i][0];
		for  (c=1; c<lth; c++)  {
			if  (polv_dat[i][c] > trcmax)  trcmax = polv_dat[i][c];
			if  (polv_dat[i][c] < trcmin)  trcmin = polv_dat[i][c];
		} /*endif*/
		trcmin = Abs(trcmin);
		trcmax = Abs(trcmax);
		if  (trcmin > trcmax)
			trcmax = trcmin;
		if  (trcmax > totmax)
			totmax = trcmax;
	} /*endfor*/
	if  (totmax <= 0.0)  return;
	norm = 0.5 / totmax;

	/* draw it */
	pol_make_scaling( displaysize_x, displaysize_y );
	pix_ClearWindow( XtDisplay(polv_draw_wdw), XtWindow(polv_draw_wdw) );
	for  (t=0; t<no_of_pm; t++)  {
		pol_drawcurve( dt, polv_dat[t], lth, xorig[t], yorig[t], zoom*norm, cmt[t] );
	} /*endfor*/

	sprintf( str, "cohlth:%d linpow:%4.1f", polv_cohlth, polv_pow_linfil );
	pix_DrawString( XtDisplay(polv_draw_wdw), XtWindow(polv_draw_wdw),
		polv_gc, 10, 50, str, (int)strlen(str) );

} /* end of pol_poltraces */


#undef Z
#undef N
#undef E
#undef MX
#undef LT



/*----------------------------------------------------------------------------*/



static void pol_make_scaling( float displaysize_x, float displaysize_y )

/* Computes window sizes and scaling variables, sets clipping rectangle on
 * all GC's.
 *
 * parameters of routine
 * float      displaysize_x, displaysize_y; input display size in user units
 */
{
	/* local variables */
	/*XRectangle   rect;*/      /* clipping rectangle */

	/* executable code */

	/* determine window sizes */
	mg_get_windowsize( polv_draw_wdw, &polv_twdw.ww, &polv_twdw.wh );
	polv_twdw.wx = polv_twdw.wy = 0;
	polv_ewdw.wx = LEFT_MARGIN;
	polv_ewdw.wy = BOTTOM_MARGIN;
	polv_ewdw.ww = polv_twdw.ww - LEFT_MARGIN - RIGHT_MARGIN;
	polv_ewdw.wh = polv_twdw.wh - TOP_MARGIN - BOTTOM_MARGIN;

	/* determine scaling */
	polv_scale.xo = 0.0;
	polv_scale.yo = 0.0;
	polv_scale.xf = (float)polv_ewdw.ww / (displaysize_x - polv_scale.xo);
	polv_scale.yf = (float)polv_ewdw.wh / (displaysize_y - polv_scale.yo);

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



static void pol_transform( float x, float y, int *px, int *py )

/* Transforms input user coordinates into window pixel coordinates
 *
 * parameters of routine
 * float      x, y;      input; user coordinates
 * int        *px, *py;  output; window coordinates
 */
{
	/* local variables */

	/* executable code */

	x = (x - polv_scale.xo) * polv_scale.xf;
	y = (y - polv_scale.yo) * polv_scale.yf;
	*px = Nint(x) + polv_ewdw.wx;
	*py = polv_twdw.wh - (Nint(y) + polv_ewdw.wy);

} /* end of pol_transform */



/*----------------------------------------------------------------------------*/



static void pol_drawcurve( float dt, float y[], int lth, float xorig,
	float yorig, float norm, char cmt[] )

/* Draws particle motion diagram
 *
 * parameters of routine
 * float      dt;           input; sample rate
 * float      y[];          input; x- and y-positions to draw
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
		pol_transform( xorig+((float)i*dt), yorig+norm*y[i], &px, &py );
		pxy[i].x = px;
		pxy[i].y = py;
	} /*endfor*/

	pix_DrawLines( XtDisplay(polv_draw_wdw), XtWindow(polv_draw_wdw),
		polv_drgc, pxy, lth, CoordModeOrigin );

#ifdef XXX
	/* draw frame */
	sep = 0.5 + BOXSEP;
	if  (palloc < 5)  {
		printf( "*SHM: pmm_drawcurve: palloc < 5, should not happen\n" );
		return;
	} /*endif*/
	pol_transform( xorig-sep, yorig-sep, &px, &py );
	pxy[0].x = px; pxy[0].y = py;
	pol_transform( xorig+sep, yorig-sep, &px, &py );
	pxy[1].x = px; pxy[1].y = py;
	pol_transform( xorig+sep, yorig+sep, &px, &py );
	pxy[2].x = px; pxy[2].y = py;
	pol_transform( xorig-sep, yorig+sep, &px, &py );
	pxy[3].x = px; pxy[3].y = py;
	pol_transform( xorig-sep, yorig-sep, &px, &py );
	pxy[4].x = px; pxy[4].y = py;
	pix_DrawLines( XtDisplay(polv_draw_wdw), XtWindow(polv_draw_wdw),
		polv_gc, pxy, 5, CoordModeOrigin );
#endif

	sprintf( str, "%s", cmt );

	/* write label */
	if  (cmt != NULL)  {
		pol_transform( xorig-0.45-BOXSEP, yorig-0.5-BOXSEP/2.0, &px, &py );
		pix_DrawString( XtDisplay(polv_draw_wdw), XtWindow(polv_draw_wdw),
			polv_gc, px, py, str, (int)strlen(str) );
	} /*endif*/

} /* end of pol_drawcurve */



/*----------------------------------------------------------------------------*/



void pol_incpar( int parid, int inc )

/* increments parameter
 *
 * parameter of routine
 * int       parid;      input; parameter ID
 * int       inc;        input; increment value
 */
{
	/* local variables */
	TSyStatus  locstat;    /* local status */

	/* executable code */

	switch  (parid)  {
	case POL_ID_COHLTH:  polv_cohlth += inc;  break;
	case POL_ID_POW_LINFIL:  polv_pow_linfil += (float)inc;  break;
	default:
		/* no nothing */;
	} /*endif*/

	locstat = cBcNoError;
	pol_draw_poltraces( 1.0, &locstat );

} /* end of pol_incpar */



/*----------------------------------------------------------------------------*/

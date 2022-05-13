
/* file motifgraph.c
 *      ============
 *
 * version 63, 11-Jan-2007
 *
 * trace display manager
 * K. Stammler, 18-Feb-93
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
#undef BC_DEFINE_TRUE_FALSE
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include <Xm/Xm.h>
#include <Mrm/MrmPublic.h>
#include <X11/cursorfont.h>
#include BC_SYSBASE
#include "infoidx.h"
#include "fctxdm.h"
#include "trusrdef.h"
#include "tcusrdef.h"
#include "ffusrdef.h"
#include "phaseinf.h"
#include "phasemgr.h"
#include "pixmaps.h"
#include "motifgraph.h"
#include "trcselect.h"
#include "globalparams.h"


#define MGC_CHMAP 0
	/* gc_... channel map (for module shdataba.c) */
#define WAVE_DRAW 1
#define WAVE_CLEAR 2
#define WAVE_SIGN 3
#define WAVE_HILB 4


/* text positions of status printouts, y-position realtive to margin_t */
#define TEXT_X_TIME 20
#define TEXT_Y_TIME 0
#define TEXT_X_FILTER 235
#define TEXT_Y_FILTER 0
#define TEXT_X_SORT 350
#define TEXT_Y_SORT 0
#define TEXT_X_STATUS 400
#define TEXT_Y_STATUS 0
#define TEXT_X_LASTCMD 620
#define TEXT_Y_LASTCMD 0
#define TEXT_X_BUSY 800
#define TEXT_Y_BUSY 0
#define TEXT_Y_SPAN_SINGLE 20

/* flag value for plot-single routine not to use 3-trace display */
#define NO3TRACES -30000

/* maximum number of windows where cursor form may be changed */
#define MAXCRSRDSP 5


/* trace X Environment (display parameters) */
typedef struct {
	GC          gc;     /* graphics context */
	XFontStruct *font;  /* font pointer */
	XPoint      orig;   /* origin */
} MGT_TRCXENV;

typedef struct {
	MGT_TRCXENV    *xenv;    /* drawing parameters */
	float          zoom;     /* zoom factor */
	float          maxampl;  /* maximum amplitude on trace */
	float          *smp;     /* pointer to sample data */
	long           length;   /* number of samples to plot */
	float          dt;       /* sample distance */
	void           *trc;     /* trace pointer */
} MGT_TRCINFO;

typedef struct {
	int      trcno;       /* number of traces */
	float    maxampl;     /* maximum amplitude */
	float    totslth;     /* total length in seconds */
	long     totnlth;     /* total length in samples */
	float    unitheight;  /* height per trace in pixels */
	float    pixpersec;   /* horizontal scaling (pixels/second) */
	int      wdweff_w;    /* effective window width */
	int      wdweff_h;    /* effective window height */
	int      wdweff_x;    /* x-offset of effective window */
	int      wdweff_y;    /* y-offset of effective window */
	int      wdw_w;       /* total window width */
	int      wdw_h;       /* total window height */
	BOOLEAN  tw_set;      /* time window set */
	float    tw_start;    /* start of time window */
	float    tw_end;      /* end of time window */
	int      type;        /* window type (main or single trace) */
} MGT_DSPINFO;

typedef struct {
	int      margin_r;    /* right margin */
	int      margin_l;    /* left margin */
	int      margin_t;    /* top margin */
	int      margin_b;    /* bottom margin */
} MGT_WDWSETUP;

/* axis descriptor */
typedef struct {
	BOOLEAN  init;          /* structure initialized ? */
	BOOLEAN  plot;          /* plot axis ? */
	float    vpos;          /* vertical position */
	float    line_s;        /* start position of axis line */
	float    line_e;        /* end position of line */
	float    tick_s;        /* position of first tick */
	float    tick_intv;     /* interval between ticks */
	float    lab1val;       /* value of first label */
	float    lab_intv;      /* label interval */
	int      lab1cnt;       /* number of first labelled tick */
	int      labcnt;        /* every "labcnt"-th tick is labelled */
	int      tick_lth;      /* length of an unlabelled tick in pixel */
	int      ltick_lth;     /* length of a labelled tick in pixel */
	float    labshift;      /* tick-relative (x-) position of label */
	float    labdist;       /* distance of label from axis */
	GC       style;         /* axis style */
	char     labfmt[16];    /* format string of label */
	BOOLEAN  use_abstime;   /* use absolute time */
	TIME     abstime;       /* absolute time */
} MGT_AXIS;

typedef struct {
	int      x1, y1;    /* lower left corner */
	int      x2, y2;    /* upper right corner */
	BOOLEAN  on;        /* box drawn */
} MGT_DRAGBOX;



/* prototypes of local routines */
static MGT_TRCXENV *mg_create_trcxenv( Widget w, STATUS *status );
static MGT_TRCINFO *mg_trcsetup( Widget w, MGT_DSPINFO *di, STATUS *status );
static void mgh_plot_traces( Widget w, MGT_DSPCTRL *ctrl, MGT_DSPINFO *di,
	MGT_TRCINFO *ti, STATUS *status );
static void mg_haxis( Widget w, MGT_AXIS *ax, MGT_DSPINFO *di );
static void mg_autoaxis( float start, float end, int labelmul, float *ticks,
	float *ticki, float *labs, float *labi, int *labelcnt );
static void mg_print_trcinfo( Widget w, MGT_DSPINFO *di, MGT_TRCINFO ti[],
	STATUS *status );
static void mg_print_s_trcinfo( Widget w, MGT_DSPINFO *di, MGT_TRCINFO ti[],
	int t, int pixoff, STATUS *status );
static void mgh_wave_cursor( Widget w, int mode, MGT_DSPINFO *di,
	MGT_DSPCTRL *ctrl, int px, int py );
static void mgh_get_pixelpos_single( int trcno, float time, int *x, int *y,
	BOOLEAN *ok );
static void mgh_find_phase( int trcno, float reftime, float resol,
	TPiPhase *phase, BOOLEAN *found );
static void mgh_get_colormap( Screen *s );
static void mgh_init_graphic_contexts( Widget w, BOOLEAN *ok );
static void mgh_plot_bold( Widget w, MGT_DSPCTRL *ctrl, MGT_DSPINFO *di,
	MGT_TRCINFO *ti, long startidx, long wdwlth, MGT_GRAPHIC_INTERP *ip,
	STATUS *status );
static void mgh_mark_selected_trace( Widget w, unsigned number );
static void mgh_set_fg_color( Display *dsp, GC gc, unsigned long fg );
static void mgh_set_bg_color( Display *dsp, GC gc, unsigned long bg );
static void mgh_label_rounding( float accuracy, TIME *atime );



/* global variables */
static MGT_WDWSETUP    mgv_wdwsetup;                    /* window parameters */
static MGT_AXIS        mgv_t_axis;                         /* main time axis */
static MGT_AXIS        mgv_ts_axis;                /* single trace time axis */
static MGT_TRCINFO     *mgv_ti=NULL;   /* current trace info's (mg_trcsetup) */
static MGT_DSPINFO     mgv_di;           /* display info, set by mg_trcsetup */
static MGT_DSPINFO     mgv_sdi;           /* info about single trace display */
static BOOLEAN         mgv_gc_init=FALSE;                /* GC`s initialized */
static GC              mgv_mark_gc;           /* line attributes for marking */
static GC              mgv_mark0_gc;     /* attributes for marking quality 0 */
static GC              mgv_theo_gc;      /* line attributes for theo marking */
static GC              mgv_auto_gc;      /* line attributes for auto marking */
static GC              mgv_xor_gc;                       /* for XOR drawings */
static GC              mgv_single_gc;           /* for single trace drawings */
static GC              mgv_single2_gc;          /* for single trace drawings */
static GC              mgv_single3_gc;          /* for single trace drawings */
static GC              mgv_phase_gc;                        /* phase markers */
static GC              mgv_over_gc;                          /* overwrite GC */
static GC              mgv_clear_gc;                             /* clear GC */
static GC              mgv_trcalert_gc;                    /* alert trace GC */
static GC              mgv_addfil_gc;          /* additional filter alert GC */
static MGT_DRAGBOX     mgv_drag;                                 /* drag box */
static BOOLEAN         mgv_phase_sel;   /* phase cursor active (mg_do_phase) */
static BOOLEAN         mgv_colored;                     /* screen is colored */
static Colormap        mgv_cmap;                                /* color map */
static char            mgv_filter[BC_LINELTH+1];              /* filter name */
static BOOLEAN         mgv_reverse_xors=FALSE; /* rev. back/foregr. on XOR's */
static Font            mgv_fixed_font=(Font)0;      /* fixed font to be used */
static BOOLEAN         mgv_disable_rd=FALSE;   /* disables redraw of display */
static BOOLEAN         mgv_show_3_traces=FALSE;  /* 3 trcs in single trc wdw */
static Cursor          mgv_crsr[MGC_XCRSR_MAX+1];           /* mouse cursors */
static Widget          mgv_crsrdsp[MAXCRSRDSP];      /* drawing area widgets */
static int             mgv_crsrdspnum=0;            /* length of above array */
                                          /* min/max in single trace display */



/*----------------------------------------------------------------------------*/



void mg_init_tracedisplay( Widget w, int r, int l, int t, int b )

/* initializes display parameters.  Input r,l,t,b valid only if >=0.
 *
 * parameters of routine
 * Widget     w;           input; Widget of trace display (DrawingArea)
 * int        r, l, t, b;  input; right, left, top, bottom margin in pixel
 */
{
	/* executable code */

	mgv_wdwsetup.margin_r = (r < 0) ? 50 : r;
	mgv_wdwsetup.margin_l = (l < 0) ? 80 : l;
	mgv_wdwsetup.margin_t = (t < 0) ? 20 : t; /*50*/;
	mgv_wdwsetup.margin_b = (b < 0) ? 50 : b;

} /* end of mg_init_tracedisplay */



/*----------------------------------------------------------------------------*/



void mg_set_reverse_xors( BOOLEAN val )

/* on some screens XOR GC's should use reversed background/foreground
 *
 * parameters of routine
 * BOOLEAN    val;       input; reverse XOR's or not
 */
{
	/* executable code */

	mgv_reverse_xors = val;

} /* end of mg_set_reverse_xors */



/*----------------------------------------------------------------------------*/



void mg_show_3_traces( BOOLEAN val )

/* Switch on/off 3 trace display in single trace window
 *
 * parameters of routine
 * BOOLEAN    val;       input; value of switch
 */
{
	/* executable code */

	mgv_show_3_traces = val;

} /* end of mg_show_3_traces */



/*----------------------------------------------------------------------------*/



void mg_tracedisplay( Widget w, MGT_DSPCTRL *ctrl, STATUS *status )

/* draws traces on DrawingArea "w"
 *
 * parameters of routine
 * Widget        w;         input; DrawingArea Widget
 * MGT_DSPCTRL   *ctrl;     input; display control parameters
 * STATUS        *status;   output; return status
 *
 * changes global variables mgv_di, mgv_ti.
 */
{
	/* local variables */
	TIME     abstime;             /* absolute time for time axis */
	STATUS   locstat=BC_NOERROR;  /* local status variable */

	/* executable code */

	if  (mgv_ti != NULL)  sy_deallocmem( mgv_ti );
	mgv_ti = mg_trcsetup( w, &mgv_di, status );
	mgv_di.type = MGC_WDW_MAIN;
	if  (GpGetInt(cGpI_debug_level) > 4)
		printf( "SHM-dgb5: mgv_ti set to %x\n", mgv_ti );
	if  (Severe(status))  return;

	pix_set_max_drawlth( GpGetInt(cGpI_x_max_drawlth) );
	pix_ClearWindow( XtDisplay(w), XtWindow(w) );
	if  (mgv_di.trcno == 0)  {
		if  (mgv_ti != NULL)  sy_deallocmem( mgv_ti );
		mgv_ti = NULL;
		if  (GpGetInt(cGpI_debug_level) > 4)
			printf( "SHM-dgb5: mgv_ti set to NULL\n" );
		mg_draw_cursor( w, MGC_WDW_MAIN, ctrl, MGC_CRSR_REFRESH, 0, 0, NULL );
		return;
	} /*endif*/

	db_gett( mgv_ti[0].trc, ET_START, &abstime, &locstat );
	mgv_t_axis.abstime = abstime;
	tc_aadd( &mgv_t_axis.abstime, -db_getr(mgv_ti[0].trc,ER_TORIG,&locstat),
		&mgv_t_axis.abstime );
	mgv_t_axis.use_abstime = (locstat == BC_NOERROR);
	mg_haxis( w, &mgv_t_axis, &mgv_di );
	mg_print_trcinfo( w, &mgv_di, mgv_ti, status );
	if  (Severe(status))  {
		sy_deallocmem( mgv_ti );
		mgv_ti = NULL;
		mg_draw_cursor( w, MGC_WDW_MAIN, ctrl, MGC_CRSR_REFRESH, 0, 0, NULL );
		if  (GpGetInt(cGpI_debug_level) > 4)
			printf( "SHM-dgb5: mgv_ti set to NULL (2)\n" );
		return;
	} /*endif*/
	if  (!mgv_disable_rd)
		mgh_plot_traces( w, ctrl, &mgv_di, mgv_ti, status );
	if  (Severe(status))  {
		sy_deallocmem( mgv_ti );
		mgv_ti = NULL;
		mg_draw_cursor( w, MGC_WDW_MAIN, ctrl, MGC_CRSR_REFRESH, 0, 0, NULL );
		if  (GpGetInt(cGpI_debug_level) > 4)
			printf( "SHM-dgb5: mgv_ti set to NULL (3)\n" );
		return;
	} /*endif*/
	if  (!mgv_disable_rd)
		mg_plot_phases( w, ctrl->show_phase_acc, status );

	mg_do_drag( w, MGC_DRAG_REFRESH, 0, 0 );
	mg_draw_cursor( w, MGC_WDW_MAIN, ctrl, MGC_CRSR_REFRESH, 0, 0, NULL );
	mg_do_phase( w, MGC_WDW_MAIN, MGC_PHASE_REFRESH, 0, 0, NULL, NULL, NULL );
	mg_print_filter( w, mgv_filter );

	XFlush( XtDisplay(w) );

} /* end of mg_tracedisplay */



/*----------------------------------------------------------------------------*/



static MGT_TRCXENV *mg_create_trcxenv( Widget w, STATUS *status )

/* creates display parameter block for a new trace
 *
 * parameters of routine
 * Widget     w;          input; Widget ID
 * STATUS     *status;    output; return status
 *                        returns pointer to parameter block
 */
{
	/* local variables */
	MGT_TRCXENV   *p;     /* pointer to structure */

	/* executable code */

	p = (MGT_TRCXENV *)sy_allocmem( 1, (int)sizeof(MGT_TRCXENV), status );
	if  (Severe(status))  return NULL;

	p->gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	mgh_set_fg_color( XtDisplay(w), p->gc, pix_colour(PIXC_COL_FOREGROUND) );
	mgh_set_bg_color( XtDisplay(w), p->gc, pix_colour(PIXC_COL_BACKGROUND) );
	if  (mgv_fixed_font != (Font)0)
		XSetFont( XtDisplay(w), p->gc, mgv_fixed_font );
	p->font = XQueryFont( XtDisplay(w), XGContextFromGC(p->gc) );

	return p;

} /* end of mg_create_trcxenv */



/*----------------------------------------------------------------------------*/



static MGT_TRCINFO *mg_trcsetup( Widget w, MGT_DSPINFO *di, STATUS *status )

/* returns pointer to array of TRCINFO blocks.  Informations are set
 * so the traces are ready to be displayed.  The pointer returned should
 * be freed after displaying the data.
 *
 * parameters of routine
 * Widget        w;          input; widget ID
 * MGT_DSPINFO   *di;        output; display info
 * STATUS        *status;    output; return status
 */
{
	/* local variables */
	MGT_TRCINFO  *p;      /* pointer to structure */
	void     *trc;        /* current trace pointer */
	int      t;           /* trace counter */
	float    y_orig;      /* y origin */
	float    dt;          /* sample distance im sec */
	long     firstsmp;    /* first sample to be displayed */
	float    x_correct;   /* correction of x-position */
	float    tmp;         /* scratch */

	/* executable code */

	mg_get_windowsize( w, &(di->wdw_w), &(di->wdw_h) );
	di->wdweff_w = di->wdw_w - mgv_wdwsetup.margin_r -
		mgv_wdwsetup.margin_l;
	di->wdweff_h = di->wdw_h - mgv_wdwsetup.margin_b -
		mgv_wdwsetup.margin_t;
	if  (di->wdweff_w < 2 || di->wdweff_h < 2)  {
		printf( "*SHM: zero window ***\n" );
		di->trcno = 0;
		return NULL;
	} /*endif*/
	di->wdweff_x = mgv_wdwsetup.margin_l;
	di->wdweff_y = mgv_wdwsetup.margin_t/*b*/;

	dm_get_timewdw( &(di->tw_start), &(di->tw_end) );
	di->tw_set = (di->tw_start != 0.0 || di->tw_end != 0.0);

	di->trcno = db_dsplth( MGC_CHMAP );
	if  (di->trcno == 0)  return NULL;

	di->unitheight = (float)(di->wdweff_h) / (float)(di->trcno);

	p = (MGT_TRCINFO *)sy_allocmem( di->trcno, (int)sizeof(MGT_TRCINFO),
		status );
	if  (Severe(status))  return NULL;

	if  (GpGetBoolean(cGpB_top_down_order))  {
		y_orig = mgv_wdwsetup.margin_t/*b*/ + di->unitheight / 2.0;
	} else {
		y_orig = mgv_wdwsetup.margin_t/*b*/ + di->unitheight / 2.0 +
			di->unitheight * (float)(di->trcno-1);
	} /*endif*/
	di->maxampl = 0.0;
	di->totslth = 0.0;
	di->totnlth = 0;
	for  (t=0; t<(di->trcno); t++)  {
		if  (t == 0)  {
			trc = db_dspfirst( MGC_CHMAP, NULL );
		} else {
			trc = db_getp( trc, EP_DSPN, NULL );
		} /*endif*/
		p[t].trc = trc;
		p[t].xenv = (MGT_TRCXENV *)db_getp( trc, EP_USR1, NULL );
		if  (p[t].xenv == NULL)  {
			p[t].xenv = mg_create_trcxenv( w, status );
			if  (Severe(status))  {
				sy_deallocmem( p );
				return NULL;
			} /*endif*/
			if  (db_getf(trc,EF_NOCALIB,NULL) || db_getf(trc,EF_QUAL,NULL))
				p[t].xenv->gc = mgv_trcalert_gc;
			db_setp( trc, EP_USR1, p[t].xenv, status );
			if  (Severe(status))  {
				sy_deallocmem( p );
				return NULL;
			} /*endif*/
		} /*endif*/
		p[t].zoom = db_getr( trc, ER_ZOOM, NULL );
		tmp = db_getr( trc, ER_MINVAL, NULL );
		p[t].maxampl = db_getr( trc, ER_MAXVAL, NULL );
		tmp = Abs( tmp );
		p[t].maxampl = Abs( p[t].maxampl );
		if  (tmp > p[t].maxampl)  p[t].maxampl = tmp;
		/*
		p[t].smp = (float *)db_getp( trc, EP_DATA, NULL );
		p[t].length = db_getl( trc, EL_LENGTH, NULL );
		*/
		p[t].smp = (float *)db_getp( trc, EP_DATA, NULL ) + 
			db_getl( trc, EL_DSPFST, NULL );
		p[t].length = db_getl( trc, EL_DSPCNT, NULL );
		p[t].dt = db_getr( trc, ER_DELTA, NULL );
		p[t].xenv->orig.y = Nint( y_orig );
		/* x-orig is set below in separate loop */
		if  (GpGetBoolean(cGpB_top_down_order))  {
			y_orig += di->unitheight;
		} else {
			y_orig -= di->unitheight;
		} /*endif*/
		tmp = Abs( p[t].maxampl );
		if  (tmp > di->maxampl)  di->maxampl = tmp;
#ifdef XXX
		tmp = (float)(p[t].length) * (float)(p[t].dt);
		if  (!di->tw_set)
			tmp += db_getr( trc, ER_TORIG, NULL );
		if  (tmp > di->totslth)  di->totslth = tmp;
#endif
		if  (p[t].length > di->totnlth)  di->totnlth = p[t].length;
	} /*endfor*/

	dm_dspcoo( NULL, NULL, &(di->totslth), NULL );
	di->pixpersec = (float)(di->wdweff_w)/di->totslth;

	/* set x-origins */
	for  (t=0; t<(di->trcno); t++)  {
		firstsmp = db_getl( p[t].trc, EL_DSPFST, status );
		if  (Severe(status))  {sy_deallocmem(p); return NULL;}
		if  (firstsmp > 0)  {
			dt = db_getr( p[t].trc, ER_DELTA, status );
			if  (Severe(status))  {sy_deallocmem(p); return NULL;}
			x_correct = (float)firstsmp*dt +
				db_getr(p[t].trc,ER_TORIG,status) - di->tw_start;
			if  (Severe(status))  {sy_deallocmem(p); return NULL;}
			if  (GpGetInt(cGpI_debug_level) > 0)
				if  (Abs(x_correct) > dt)
					printf( "SHM-dbg1: x_correct %f, dt %f, trc %d\n", x_correct, dt, t+1 );
			p[t].xenv->orig.x = mgv_wdwsetup.margin_l +
				Nint(x_correct*(di->pixpersec));
		} else {
			if  (di->tw_set)  {
				tmp = di->tw_start;
			} else {
				dm_dspcoo( &tmp, NULL,NULL,NULL );
			} /*endif*/
			p[t].xenv->orig.x = mgv_wdwsetup.margin_l +
				Nint((db_getr(p[t].trc,ER_TORIG,NULL) - tmp) *
				di->pixpersec);
		} /*endif*/
	} /*endfor*/

	return p;

} /* end of mg_trcsetup */



/*----------------------------------------------------------------------------*/



void mg_get_windowsize( Widget w, int *width, int *height )

/* returns size of window in pixels
 *
 * parameters of routine
 * Widget     w;         input; widget ID
 * int        *width;    output; window width
 * int        *height;   output; window height
 */
{
	/* local variables */
	Window   root_ret;   /* root window */
	int      x, y;       /* position within root window */
	unsigned bw, dp;     /* border width & depth */
	unsigned xw, xh;

	/* executable code */

	XGetGeometry( XtDisplay(w), XtWindow(w), &root_ret, &x, &y, &xw, &xh, &bw, &dp );
	*width = xw;
	*height = xh;

} /* end of mg_get_windowsize */



/*----------------------------------------------------------------------------*/



static void mgh_plot_traces( Widget w, MGT_DSPCTRL *ctrl, MGT_DSPINFO *di,
	MGT_TRCINFO *ti, STATUS *status )

/* plots traces in drawing area
 *
 * parameters of routine
 * Widget        w;        input; widget ID
 * MGT_DSPCTRL   *ctrl;    input; display control parameters
 * MGT_DSPINFO   *di;      input; display info
 * MGT_TRCINFO   *ti;      input; trace infos
 * STATUS        *status;  output; return status
 */
{
	/* local variables */
	Display  *dsp;        /* Display */
	Window   *wdw;        /* window */
	XPoint   *pxy;        /* plot array */
	XPoint   *p;          /* movin pointer */
	float    *dat;        /* sample pointer */
	int      t;           /* trace counter */
	long     s;           /* sample counter */
	float    vnorm;       /* vertical normalization */
	float    xinc;        /* x increment */
	int      xoff, yoff;  /* trace origin */
	long     efflth;      /* effective length */
	int      oldx, oldy;  /* last coordinates */
	int      newx, newy;  /* new coordinates */
	unsigned filent;      /* additional filter entry */
	char     filname[cBcFileLth+1]; /* additional filter name */
	TSyStatus locstat;    /* local status */
	GC       locgc;       /* local GC */

	/* executable code */

	dsp = XtDisplay( w );

	if  (di->totslth == 0.0 || di->totnlth == 0)  {
		if  (GpGetInt(cGpI_debug_level) > 0)
			printf( "SHM-dbg1: zero display window\n" );
		return;
	} /*endif*/

	pxy = (XPoint *)sy_allocmem( di->totnlth, (int)sizeof(XPoint), status );
	if  (Severe(status))  return;

	/* get index number of filter entry */
	db_ident( "FILTER", &filent, status );
	if  (SySevere(status))  return;

	for  (t=0; t<(di->trcno); t++)  {
		if  (ti[t].length > 1)  {
			p = pxy;
			dat = ti[t].smp;
			xoff = ti[t].xenv->orig.x;
			yoff = ti[t].xenv->orig.y;
			xinc = (float)(di->wdweff_w)/di->totslth * ti[t].dt;
			vnorm = db_getr( ti[t].trc, ER_NORM, NULL );
			vnorm *= db_getr( ti[t].trc, ER_ZOOM, NULL );
			vnorm *= di->unitheight * ctrl->zoom;
			efflth = 1;
			oldx = p->x = xoff;
			oldy = p->y = yoff - Nint( *dat * vnorm );
			p++; dat++;
			for  (s=1; s<(ti[t].length); s++)  {
				newx = xoff + Nint( s*xinc );
				newy = yoff - Nint( *dat * vnorm );
				if  (newx != oldx || newy != oldy)  {
					p->x = newx;
					p->y = newy;
					p++;
					efflth++;
					oldx = newx;
					oldy = newy;
				} /*endif*/
				dat++;
			} /*endfor*/
			/* check for additional file, mark trace with addfilgc, */
			locstat = cBcNoError;
			db_gets( ti[t].trc, filent, cBcFileLth, filname, &locstat );
			if  (locstat == cBcNoError && *filname != '\0')  {
				locgc = mgv_addfil_gc;
			} else {
				locgc = ti[t].xenv->gc;
			} /*endif*/
			pix_DrawLines( dsp, XtWindow(w), locgc, pxy, efflth,
				CoordModeOrigin );
		} /*endif*/
	} /*endfor*/

	sy_deallocmem( pxy );

} /* end of mgh_plot_traces */



/*----------------------------------------------------------------------------*/



void mg_plot_bold( Widget wm, Widget ws, MGT_DSPCTRL *ctrl, int wdwno,
	int trcno, long startidx, long wdwlth, MGT_GRAPHIC_INTERP *ip,
	STATUS *status )

/* plots part of a trace bold in drawing area
 *
 * parameters of routine
 * Widget        wm;       input; widget ID of main drawing area
 * Widget        ws;       input; widget ID of single trace drawing area
 * MGT_DSPCTRL   *ctrl;    input; display control
 * int           wdwno;    input window number
 * int           trcno;    input; trace number
 * long          startidx; input; start index of trace
 * long          wdwlth;   input; window length in samples
 * MGT_GRAPHIC_INTERP *ip; input; graphic interpolation
 * STATUS        *status;  output; return status
 */
{
	/* executable code */

	trcno--;
	if  (trcno >= mgv_di.trcno || trcno < 0)  {
		printf("*SHM: mg_plot_bold: illegal trace (bug in program)***\n");
		return;
	} /*endif*/

	if  (wdwno == MGC_WDW_MAIN)  {
		mgh_plot_bold( wm, ctrl, &mgv_di, mgv_ti+trcno, startidx,
			wdwlth, ip, status );
	} else if  (wdwno == MGC_WDW_SINGLE)  {
		mgh_plot_bold( ws, ctrl, &mgv_sdi, mgv_ti+trcno, startidx,
			wdwlth, ip, status );
	} else {
		printf( "*SHM: mg_plot_bold: this cannot happen ***\n" );
		return;
	} /*endif*/

} /* end of mg_plot_bold */



/*----------------------------------------------------------------------------*/



static void mgh_plot_bold( Widget w, MGT_DSPCTRL *ctrl, MGT_DSPINFO *di,
	MGT_TRCINFO *ti, long startidx, long wdwlth, MGT_GRAPHIC_INTERP *ip,
	STATUS *status )

/* plots part of a trace bold in drawing area
 *
 * parameters of routine
 * Widget        w;        input; widget ID
 * MGT_DSPCTRL   *ctrl;    input; display control parameters
 * MGT_DSPINFO   *di;      input; display info
 * MGT_TRCINFO   *ti;      input; trace info
 * long          startidx; input; start index
 * long          wdwlth;   input; length of trace window
 * MGT_GRAPHIC_INTERP *ip; input; graphic interpolation
 * STATUS        *status;  output; return status
 */
{
	/* local variables */
	Display  *dsp;        /* Display */
	Window   *wdw;        /* window */
	XPoint   *pxy;        /* plot array */
	XPoint   *p;          /* moving pointer */
	float    *dat;        /* sample pointer */
	long     s;           /* sample counter */
	float    vnorm;       /* vertical normalization */
	float    xinc;        /* x increment */
	int      xoff, yoff;  /* trace origin */
	long     efflth;      /* effective length */
	int      oldx, oldy;  /* last coordinates */
	int      newx, newy;  /* new coordinates */
	long     trclth;      /* length of trace in single trace window */
	float    maxval;      /* maximum value in single trace window */
	float    meanval;     /* mean value in single trace window */
	GC       bold_gc;     /* bold GC */
	XGCValues gc_val;     /* GC values */
	int      i;           /* counter */
	int      xstatus;     /* X return value */
	XColor   screen_color, exact_color;  /* colors */

	/* executable code */

	dsp = XtDisplay( w );

	if  (di->totslth == 0.0)  {
		if  (GpGetInt(cGpI_debug_level) > 0)
			printf( "SHM-dbg1: zero display window\n" );
		return;
	} /*endif*/

	pxy = (XPoint *)sy_allocmem( wdwlth, (int)sizeof(XPoint), status );
	if  (Severe(status))  return;

	bold_gc = XCreateGC( dsp, XtWindow(w), 0, NULL );
	XCopyGC( dsp, ti->xenv->gc, GCFunction|GCForeground|
		GCBackground|GCLineWidth|GCLineStyle|GCCapStyle|GCJoinStyle|
		GCFont, bold_gc );
	gc_val.line_width = 3;
	gc_val.cap_style = CapRound;
	gc_val.join_style = JoinRound;
	XChangeGC( dsp, bold_gc, GCLineWidth|GCCapStyle|GCJoinStyle, &gc_val );
	if  (mgv_colored)  {
		xstatus = XAllocNamedColor( dsp, mgv_cmap, "magenta",
			&screen_color, &exact_color );
		if  (xstatus != 0)
			mgh_set_fg_color( XtDisplay(w), bold_gc,
				screen_color.pixel );
	} /*endif*/

	/* get pointer to data at beginning of window */
	dat = (float *)db_getp( ti->trc, EP_DATA, NULL );
	if  (di->tw_set)
		dat += dm_getsample( ti->trc, di->tw_start, TRUE );

	xinc = (float)(di->wdweff_w)/di->totslth * ti->dt;
	if  (di->type == MGC_WDW_MAIN)  {
		xoff = ti->xenv->orig.x + Nlong( startidx * xinc );
		yoff = ti->xenv->orig.y;
		meanval = 0.;
		vnorm = db_getr( ti->trc, ER_NORM, NULL );
		vnorm *= db_getr( ti->trc, ER_ZOOM, NULL );
		vnorm *= di->unitheight * ctrl->zoom;
	} else {
		startidx += db_getl( ti->trc, EL_DSPFST, NULL );
		startidx -= dm_getsample( ti->trc, di->tw_start, TRUE );
		xoff = mgv_sdi.wdweff_x + Nlong( startidx * xinc );
		yoff = mgv_wdwsetup.margin_t/*b*/ + mgv_sdi.unitheight / 2.0;
		trclth = dm_getsample( ti->trc, di->tw_end, TRUE );
		trclth -= dm_getsample( ti->trc, di->tw_start, TRUE ) - 1;
		if  (trclth < 1)  {
			printf( "*SHM: plot_bold: short trace (bug) ***\n" );
			XFreeGC( dsp, bold_gc );
			sy_deallocmem( pxy );
			return;
		} /*endif*/
		maxval = Abs( *dat );
		meanval = 0.;
		for  (i=1; i<trclth; i++)  {
			if  (Abs(dat[i]) > maxval)  maxval = Abs(dat[i]);
			meanval += dat[i];
		} /*endfor*/
		meanval /= (float)trclth;
		maxval -= meanval;
		vnorm = di->unitheight / maxval * 0.5;
	} /*endif*/

	/* set data pointer to beginning of selection */
	dat += startidx;

	if  (ti->length > 1)  {
		p = pxy;
		/* dat = ti->smp + startidx; */
		efflth = 1;
		if  (ip->do_interpolation)  {
			oldx = p->x = xoff + ip->frac_1*xinc;
			oldy = p->y = yoff - Nint( (ip->ampl_1-meanval)*vnorm );
		} else {
			oldx = p->x = xoff;
			oldy = p->y = yoff - Nint( (*dat-meanval) * vnorm );
		} /*endif*/
		p++; dat++;
		for  (s=1; s<wdwlth; s++)  {
			newx = xoff + Nint( s*xinc );
			newy = yoff - Nint( (*dat-meanval) * vnorm );
			if  (newx != oldx || newy != oldy)  {
				p->x = newx;
				p->y = newy;
				p++;
				efflth++;
				oldx = newx;
				oldy = newy;
			} /*endif*/
			dat++;
		} /*endfor*/
		if  (ip->do_interpolation)  {
			pxy[efflth-1].x = xoff + Nint( ((float)(wdwlth-1) +
				ip->frac_e) * xinc );
			pxy[efflth-1].y = yoff -
				Nint( (ip->ampl_e-meanval) * vnorm );
		} /*endif*/
		pix_DrawLines( dsp, XtWindow(w), bold_gc, pxy, efflth,
			CoordModeOrigin );
	} /*endif*/

	XFreeGC( dsp, bold_gc );
	sy_deallocmem( pxy );

} /* end of mgh_plot_bold */



/*------------------------------------------------------------------------*/



static void mg_haxis( Widget w, MGT_AXIS *ax, MGT_DSPINFO *di )

/* draws a horizontal axis
 *
 * parameters of routine
 * Widget        w;         input; widget of DrawingArea
 * MGT_AXIS      ax;        input; axis parameter
 * MGT_DSPINFO   *di;       input; display info
 */
{
	/* local variables */
	static XFontStruct *font;      /* pointer to font structure */
	char     label[30];     /* labelling of axis */
	float    currpos;       /* current position */
	int      currpos_i;     /* current position in pixels */
	int      ticklth;       /* current tick length */
	float    currval;       /* current label value */
	int      labelcnt;      /* label counter */
	float    lshift;        /* label shift */
	float    ldist;         /* label distance */
	float    lines, linee;  /* axis start & end (in coo's) */
	float    lines_pos;     /* position of lines */
	float    linee_pos;     /* position of linee */
	float    ticks, ticki;  /* tick start & interval */
	float    labs, labi;    /* label start & interval */
	int      vertpos;       /* vertical position */
	float    hnorm;         /* horizontal normalization */
	int      textw;         /* text width in pixel */
	int      texth;         /* text height in pixel */
	TIME     abscurr;       /* current absolute time */
	STATUS   locstat;       /* local status */
	int      label_x;       /* x-position of label */
	int      label_x_end;   /* end of last label */

	/* executable code */

	if  (!ax->init)  {
		ax->style = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
		mgh_set_fg_color( XtDisplay(w), ax->style,
			pix_colour(PIXC_COL_FOREGROUND) );
		mgh_set_bg_color( XtDisplay(w), ax->style,
			pix_colour(PIXC_COL_BACKGROUND) );
		if  (mgv_fixed_font != (Font)0)
			XSetFont( XtDisplay(w), ax->style, mgv_fixed_font );
		ax->vpos = di->wdw_h - mgv_wdwsetup.margin_b;  /* di->wdweff_y; */
		ax->ltick_lth = 10;
		ax->tick_lth = 5;
		ax->labcnt = 5;
		ax->plot = TRUE;
		strcpy( ax->labfmt, "%3.1f" );
		ax->init = TRUE;
		font = XQueryFont( XtDisplay(w),
			XGContextFromGC(ax->style) );
		ax->use_abstime = TRUE;
	} /*endif*/

	texth = font->ascent + font->descent;

	if  (!(ax->plot))  return;

	ax->vpos = di->wdw_h - mgv_wdwsetup.margin_b;  /* di->wdweff_y; */ /* ins */
	vertpos = Nint( ax->vpos );

	if  ((ax->line_s == 0.0) && (ax->line_e == 0.0))  {
		if  (di->tw_set)  {
			lines = di->tw_start;
			linee = di->tw_end;
		} else {
			/*
			lines = 0.0;
			linee = di->totslth;
			*/
			dm_dspcoo( &lines, NULL, &linee, NULL );
			linee += lines;
		} /*endif*/
	} else {
		lines = ax->line_s;
		linee = ax->line_e;
	} /*endif*/
	if  ((ax->tick_intv == 0.0) && (ax->lab_intv == 0.0))  {
		mg_autoaxis( lines, linee, ax->labcnt, &ticks, &ticki,
			&labs, &labi, &labelcnt );
	} else {
		if  (ax->tick_intv == 0.0)  {
			ticks = lines;
			ticki = (linee-lines) / 10.;
		} else {
			ticks = ax->tick_s;
			ticki = ax->tick_intv;
		} /*endif*/
		if  (ax->lab_intv == 0.0)  {
			labs = ticks;
			labi = ticki;
		} else {
			if  (GpGetInt(cGpI_debug_level) > 0)
				printf( "SHM-dbg1: undefined lab1val %f5.2 used\n", ax->lab1val );
			labs = ax->lab1val;
			labi = ax->lab_intv;
		} /*endif*/
		labelcnt = ax->lab1cnt;
	} /*endif*/

	hnorm = di->pixpersec;
	if  (di->tw_set)  {
		lines_pos = lines - di->tw_start;
		linee_pos = linee - di->tw_start;
	} else {
		lines_pos = lines;
		linee_pos = linee;
	} /*endif*/

	pix_DrawLine( XtDisplay(w), XtWindow(w), ax->style,
		di->wdweff_x+Nint(hnorm*lines_pos), vertpos,
		di->wdweff_x+Nint(hnorm*linee_pos), vertpos );

	lshift = ax->labshift;
	ldist = ax->labdist;
	currval = labs;
	label_x_end = SYC_MININT;

	for  (currpos=ticks;currpos<=linee; currpos += ticki)  {
		currpos_i = di->wdweff_x + Nint( hnorm * (currpos-di->tw_start) );
		if  (--labelcnt == 0)  {  /* labelled tick */
			ticklth = ax->ltick_lth;
			labelcnt = ax->labcnt;
			if  (ax->use_abstime)  {
				locstat = BC_NOERROR;
				tc_aadd( &(ax->abstime), currval, &abscurr );
				mgh_label_rounding( ticki*GpGetFloat(cGpF_axis_label_rounding),
					&abscurr );
				tc_a2t( &abscurr, label, &locstat );
				if  (locstat == BC_NOERROR)  {
					strncpy( label, label+12, 12 );
					label[12] = '\0';
					if  (strcmp(label+8,".000") == 0)
						label[8] = '\0';
				} else {
					sprintf( label, ax->labfmt, currval );
				} /*endif*/
			} else {
				sprintf( label, ax->labfmt, currval );
			} /*endif*/
			textw = XTextWidth( font, label, (int)strlen(label) );
			label_x = currpos_i-textw/2+Nint(lshift);
			if  (label_x > label_x_end)  {
				pix_DrawString( XtDisplay(w), XtWindow(w),
					ax->style, label_x,
					vertpos+ticklth+texth+Nint(ldist),
					label, (int)strlen(label) );
				label_x_end = label_x + textw;
			} /*endif*/
		} else {
			ticklth = ax->tick_lth;
		} /*endif*/
		pix_DrawLine( XtDisplay(w), XtWindow(w), ax->style,
			currpos_i, vertpos, currpos_i, vertpos+ticklth );
		currval += labi;
	} /*endfor*/

} /* end of mg_haxis */



/*------------------------------------------------------------------------*/



static void mg_autoaxis( float start, float end, int labelmul, float *ticks,
	float *ticki, float *labs, float *labi, int *labelcnt )

/* computes axis parameters
 *
 * parameters of routine
 * float      start;      input; start coo
 * float      end;        input; end coo
 * int        labelmul;   input; every "labelmul"-th tick is labelled
 * float      *ticks;     output; position of first tick
 * float      *ticki;     output; interval of ticks
 * float      *labs;      output; value of first label
 * float      *labi;      output; label interval
 * int        *labelcnt;  output; number of first labelled tick
 */
{
	/* local variables */
	float    width;    /* width of axis */
	int      labnum, ticknum;

	/* executable code */

	/* compute labelled intervals */
	width = end - start;
	*labi = pow( 10., floor(log10(width)) );
	while  ((width / *labi) < 3.0)
		*labi /= 2.0;
	*ticki = *labi / (float)labelmul;

	if  (start == 0.0)  {
		*labs = 0.0;
		*labelcnt = 1;
	} else if  ((start < 0.0) && (end > 0.0))  {
		labnum = Nint(floor((-start+0.01) / *labi));
		ticknum = Nint(floor((-start+0.01) / *ticki));
		*labelcnt = ticknum - labelmul*labnum + 1;
		*labs = -(float)ticknum * (*ticki);
	} else if  (start > 0.0)  {
		labnum = 1;
		while  (((float)labnum * *labi) < start)
			labnum++;
		ticknum = labnum * labelmul;
		while  (((float)ticknum * *ticki) >= start)
			ticknum--;
		*labelcnt = labelmul*labnum - (++ticknum) + 1;
		*labs = (float)ticknum * (*ticki);
	} else {
		labnum = -1;
		while  (((float)labnum * *labi) >= start)
			labnum--;
		ticknum = ++labnum * labelmul;
		while  (((float)ticknum * *ticki) >= start)
			ticknum--;
		*labelcnt = labelmul*labnum - (++ticknum) + 1;
		*labs = (float)ticknum * (*ticki);
	} /*endif*/

	*labi /= (float)labelmul;
	*ticki = *labi;
	*ticks = *labs;

} /* end of mg_autoaxis */



/*----------------------------------------------------------------------------*/



static void mg_print_trcinfo( Widget w, MGT_DSPINFO *di, MGT_TRCINFO ti[],
	STATUS *status )

/* prints trace informations on DrawingArea
 *
 * parameters of routine
 * Widget        w;        input; DrawingArea widget
 * MGT_DSPINFO   *di;      input; display info
 * MGT_TRCINFO   ti[];     input; trace infos
 * STATUS        *status;  output; return status
 */
{
	/* local variables */
	int      t;                  /* trace counter */
	int      charw;              /* char width in pixels */
	int      texth;              /* text height in pixels */
	char     str[BC_LINELTH+1];  /* scratch string */
	int      x, y;               /* text position */

	/* executable code */

	for  (t=0; t<(di->trcno); t++)  {
		texth = ti[t].xenv->font->ascent + ti[t].xenv->font->descent;
		charw = ti[t].xenv->font->max_bounds.rbearing -
			ti[t].xenv->font->min_bounds.lbearing;
		x = di->wdweff_x - 10*charw;
		y = ti[t].xenv->orig.y + texth/2;
		tr_setruntrc( ti[t].trc, t+1 );
		*str = '\0';  /* first call to dm_infstr */
		while  (dm_infstr(BC_LINELTH,str,status))  {
			if  (Severe(status))  return;
			pix_DrawString( XtDisplay(w), XtWindow(w),
				ti[t].xenv->gc, x, y, str, (int)strlen(str) );
			y += texth;
		} /*endwhile*/
		tr_setruntrc( NULL, 0 );
		/* plot zero line */
		pix_DrawLine( XtDisplay(w), XtWindow(w), mgv_xor_gc,
			ti[t].xenv->orig.x+di->wdweff_w+10, ti[t].xenv->orig.y,
			ti[t].xenv->orig.x+di->wdweff_w+18, ti[t].xenv->orig.y );
	} /*endfor*/

} /* end of mg_print_trcinfo */



/*----------------------------------------------------------------------------*/



static void mg_print_s_trcinfo( Widget w, MGT_DSPINFO *di, MGT_TRCINFO ti[],
	int t, int pixoff, STATUS *status )

/* prints trace informations in single trace window
 *
 * parameters of routine
 * Widget        w;        input; Single Trace Box widget
 * MGT_DSPINFO   *di;      input; display info
 * MGT_TRCINFO   ti[];     input; trace infos
 * int           t;        input; trace number
 * STATUS        *status;  output; return status
 */
{
	/* local variables */
	int      charw;              /* char width in pixels */
	int      texth;              /* text height in pixels */
	char     str[BC_LINELTH+1];  /* scratch string */
	int      x, y;               /* text position */
	GC       locgc;              /* local GC */

	/* executable code */

	locgc = mgv_single_gc;
	if  (pixoff == NO3TRACES)  pixoff = 0;
	if  (pixoff > 0)  locgc = mgv_single2_gc;
	if  (pixoff < 0)  locgc = mgv_single3_gc;

	texth = ti[t].xenv->font->ascent + ti[t].xenv->font->descent;
	charw = ti[t].xenv->font->max_bounds.rbearing -
		ti[t].xenv->font->min_bounds.lbearing;
	x = di->wdweff_x - 10*charw;
	y = di->wdweff_y + di->wdweff_h/2 + texth/2 + pixoff;
	tr_setruntrc( ti[t].trc, t+1 );
	*str = '\0';  /* first call to dm_infstr */
	while  (dm_infstr(BC_LINELTH,str,status))  {
		if  (Severe(status))  return;
		pix_DrawString( XtDisplay(w), XtWindow(w),
			locgc, x, y, str, (int)strlen(str) );
		y += texth;
	} /*endwhile*/
	tr_setruntrc( NULL, 0 );

} /* end of mg_print_s_trcinfo */



/*----------------------------------------------------------------------------*/



void mg_get_time_and_trace( int x, int y, float *time, int *trcno, BOOLEAN *ok )

/* returns time and trace selected by pixels (x,y)
 *
 * parameters of routine
 * int        x, y;        input; pixel coordinates
 * float      *time;       output; time in sec
 * int        *trcno;      output; trace number
 * BOOLEAN    *ok;         output; validitation
 */
{
	/* local variables */
	int      t;       /* trace counter */

	/* executable code */

	if  (trcno != NULL)  *trcno = 0;
	*ok = (mgv_di.pixpersec != 0.0 && mgv_di.trcno > 0 && mgv_ti != NULL);
	if  (!(*ok))  return;

	*time = mgv_di.tw_start + (float)(x-mgv_di.wdweff_x)/mgv_di.pixpersec;
	if  (trcno == NULL)  return;
	for  (t=0; t<mgv_di.trcno; t++)  {
		if  (Abs(y-mgv_ti[t].xenv->orig.y) < mgv_di.unitheight/2)  {
			*trcno = t + 1;
			return;
		} /*endif*/
	} /*endfor*/
	*ok = FALSE;

} /* end of mg_get_time_and_trace */



/*----------------------------------------------------------------------------*/



void mg_get_time_and_trace_single( int x, int y, float *time,
	int *trcno, BOOLEAN *ok )

/* returns time and trace selected by pixels (x,y) in single trace window
 *
 * parameters of routine
 * int        x, y;        input; pixel coordinates
 * float      *time;       output; time in sec
 * int        *trcno;      output; trace number
 * BOOLEAN    *ok;         output; validitation
 */
{
	/* local variables */
	float    dummy;

	/* executable code */

	if  (trcno != NULL)  *trcno = 0;
	*ok = (mgv_sdi.pixpersec != 0.0 && mgv_sdi.trcno > 0 && mgv_ti != NULL);
	if  (!(*ok))  return;

	*time = mgv_sdi.tw_start + (float)(x-mgv_sdi.wdweff_x)/mgv_sdi.pixpersec;
	if  (trcno == NULL)  return;
	mg_get_time_and_trace( mgv_drag.x1, (mgv_drag.y1+mgv_drag.y2)/2,
		&dummy, trcno, ok );

} /* end of mg_get_time_and_trace_single */



/*----------------------------------------------------------------------------*/



void mg_get_pixelpos( int trcno, float time, int *x, int *y, BOOLEAN *ok )

/* returns pixel position of trace "trcno" at time "time" (y-coordinate is
 * given at zero line
 *
 * parameters of routine
 * int        trcno;     input; trace number on display
 * float      time;      input; time position
 * int        *x, *y;    output; pixel position
 * BOOLEAN    *ok;       output; validitation
 */
{
	/* executable code */

	*ok = FALSE;
	trcno--;
	if  (trcno < 0 || trcno >= mgv_di.trcno)  return;
	if  (time < mgv_di.tw_start || time > mgv_di.tw_end)  return;
	*ok = TRUE;
	if  (x != NULL)
		*x = Nint( (time-mgv_di.tw_start)*mgv_di.pixpersec +
			mgv_di.wdweff_x );
	if  (y != NULL)
		*y = mgv_ti[trcno].xenv->orig.y;

} /* end of mg_get_pixelpos */



/*----------------------------------------------------------------------------*/



static void mgh_get_pixelpos_single( int trcno, float time, int *x, int *y,
	BOOLEAN *ok )

/* returns pixel position of trace "trcno" at time "time" (y-coordinate is
 * given at zero line in single trace window
 *
 * parameters of routine
 * int        trcno;     input; trace number on display
 * float      time;      input; time position
 * int        *x, *y;    output; pixel position
 * BOOLEAN    *ok;       output; validitation
 */
{
	/* local variables */
	int      loctrcno;       /* local trace number */
	float    dummy;          /* dummy */

	/* executable code */

	if  (!mgv_drag.on)  {
		*ok = FALSE;
		return;
	} /*endif*/

	mg_get_time_and_trace( mgv_drag.x1, (mgv_drag.y1+mgv_drag.y2)/2,
		&dummy, &loctrcno, ok );
	if  (!ok)  return;
	if  (loctrcno != trcno)  {*ok = FALSE; return;}

	if  (time < mgv_sdi.tw_start || time > mgv_sdi.tw_end)  {
		*ok = FALSE;
		return;
	} /*endif*/
	*ok = TRUE;
	if  (x != NULL)
		*x = Nint( (time-mgv_sdi.tw_start)*mgv_sdi.pixpersec +
			mgv_sdi.wdweff_x );
	if  (y != NULL)
		*y = mgv_wdwsetup.margin_t/*b*/ + mgv_sdi.unitheight / 2.0;

} /* end of mgh_get_pixelpos_single */



/*----------------------------------------------------------------------------*/



void mg_get_drag_window( float *start, float *end, BOOLEAN *set )

/* returns current drag window in sec
 *
 * parameters of routine
 * float      *start;      output; start of time window
 * float      *end;        output; end of time window
 * BOOLEAN    *set;        output; time window set
 */
{
	/* executable code */

	*set = mgv_drag.on;
	if  (!(*set))  return;

	mg_get_time_and_trace( mgv_drag.x1, mgv_drag.y1, start, NULL, set );
	if  (!(*set))  return;
	mg_get_time_and_trace( mgv_drag.x2, mgv_drag.y1, end, NULL, set );
	if  (!(*set))  return;

} /* end of mg_get_drag_window */



/*----------------------------------------------------------------------------*/



void mg_get_drag_trace( int *trcno, BOOLEAN *set )

/* returns trace number which is selected by drag window
 *
 * parameters of routine
 * int        *trcno;  output; trace number on display (valid only if *set=TRUE)
 * BOOLEAN    *set;    output; drag window is set
 */
{
	/* local variables */
	float    dummy;

	/* executable code */

	mg_get_time_and_trace( mgv_drag.x1, (mgv_drag.y1+mgv_drag.y2)/2,
		&dummy, trcno, set );

} /* end of mg_get_drag_trace */



/*----------------------------------------------------------------------------*/



void mg_mark_trace( Widget wm, Widget ws, int trcno, float time, char label[],
	int kind, float left_acc, float right_acc )

/* marks trace "trcno" at time "time"
 *
 * parameters of routine
 * Widget     ws;         input; widget ID of main window
 * Widget     wm;         input; widget ID of single trace window
 * int        trcno;      input; trace number on display
 * float      time;       input; time position
 * char       label;      input; label text
 * char       kind;       input; phase kind
 * float      left_acc;   input; left accuracy in sec
 * float      right_acc;  input; right accuracy in sec
 */
{
	/* local variables */
	BOOLEAN  ok;        /* validitation */
	int      x, y;      /* pixel coordinates */
	int      xal, xar;  /* x coordinates for accuracies */
	int      halflth;   /* half length of mark */
	GC       lgc;       /* local GC */

	/* executable code */

	if  (!mgv_gc_init)  mgh_init_graphic_contexts( wm, &mgv_gc_init );
	switch (kind)  {
	case MGC_PHASE_KIND_THEO:         lgc = mgv_theo_gc;   break;
	case MGC_PHASE_KIND_AUTO:         lgc = mgv_auto_gc;   break;
	case MGC_PHASE_KIND_MANUALLY_0:   lgc = mgv_mark0_gc;  break;
	default:                          lgc = mgv_mark_gc;   break;
	} /*endswitch*/

	if  (wm != (Widget)0)  {
		mg_get_pixelpos( trcno, time, &x, &y, &ok );
		if  (!ok)  return;

		halflth = Nint( mgv_di.unitheight/2.0 );
		pix_DrawLine( XtDisplay(wm), XtWindow(wm), lgc,
			x, y-halflth, x, y+halflth );
		pix_DrawString( XtDisplay(wm), XtWindow(wm), lgc, x+2,
			y-halflth+3, label, (int)strlen(label) );
		if  (left_acc > 0.0 || right_acc > 0.0)  {
			xal = mgv_di.wdweff_x;
			xar = mgv_di.wdweff_x+mgv_di.wdweff_w;
			mg_get_pixelpos( trcno, time-left_acc, &xal, &y, &ok );
			mg_get_pixelpos( trcno, time+right_acc, &xar, &y, &ok );
			pix_DrawLine( XtDisplay(wm), XtWindow(wm), lgc, xal, y, xar, y );
		} /*endif*/
		if  (ws == (Widget)0)  XFlush( XtDisplay(wm) );
	} /*endif*/

	if  (ws == (Widget)0)  return;
	if  (!mgv_drag.on)  return;

	mgh_get_pixelpos_single( trcno, time, &x, &y, &ok );
	if  (!ok)  return;

	halflth = Nint( mgv_sdi.unitheight/2.0 );
	pix_DrawLine( XtDisplay(ws), XtWindow(ws), lgc,
		x, y-halflth, x, y+halflth );
	pix_DrawString( XtDisplay(ws), XtWindow(ws), lgc, x+2, y-halflth+3,
		label, (int)strlen(label) );
	if  (left_acc > 0.0 || right_acc > 0.0)  {
		xal = mgv_sdi.wdweff_x;
		xar = mgv_sdi.wdweff_x+mgv_di.wdweff_w;
		mgh_get_pixelpos_single( trcno, time-left_acc, &xal, &y, &ok );
		mgh_get_pixelpos_single( trcno, time+right_acc, &xar, &y, &ok );
		pix_DrawLine( XtDisplay(ws), XtWindow(ws), lgc, xal, y, xar, y );
	} /*endif*/

	XFlush( XtDisplay(ws) );

} /* end of mg_mark_trace */



/*----------------------------------------------------------------------------*/



void mg_do_drag( Widget w, int mode, int x, int y )

/* draws dragging lines and boxes
 *
 * parameters of routine
 * Widget     w;         input; widget ID
 * int        mode;      input; which item
 * int        x, y;      input; new position
 */
{
	/* local variables */
	static int last_mode;   /* last drag mode */
	XPoint   box[5];        /* box coo's */
	BOOLEAN  visible;       /* box is visible */
	int      tmp;           /* scratch */
	int      bw, bh;        /* width & height of box */
	float    timpos;        /* time position */
	int      trcno;         /* trace number */
	BOOLEAN  ok;            /* status flag */

	/* executable code */

	if  (!mgv_gc_init)  mgh_init_graphic_contexts( w, &mgv_gc_init );

	/* erase old box if visible or refresh it */
	visible = mgv_drag.on;
	if  (mgv_drag.on)  {  /* erase old box */
		box[0].x = mgv_drag.x1;   box[0].y = mgv_drag.y1;
		box[1].x = mgv_drag.x1;   box[1].y = mgv_drag.y2;
		box[2].x = mgv_drag.x2;   box[2].y = mgv_drag.y2;
		box[3].x = mgv_drag.x2;   box[3].y = mgv_drag.y1;
		box[4].x = mgv_drag.x1;   box[4].y = mgv_drag.y1;
		pix_DrawLines( XtDisplay(w), XtWindow(w), mgv_xor_gc,
			box, 5, CoordModeOrigin );
		mgv_drag.on = FALSE;
	} /*endif*/

	if  (mode == MGC_DRAG_REFRESH)  {
		mgv_drag.on = visible;
		return;
	} /*endif*/
	if  (mode == MGC_DRAG_CLEAR)
		return;
	if  (mode == MGC_DRAG_CONTINUE)
		mode = last_mode;

	/* modify input coordinates */
	mg_get_time_and_trace( x, y, &timpos, &trcno, &ok );
	if  (!ok)  return;

	/* do not accept x coordinate outside of trace area */
	if  (x < mgv_di.wdweff_x)  x = mgv_di.wdweff_x;
	if  (x > mgv_di.wdweff_x+mgv_di.wdweff_w)
		x = mgv_di.wdweff_x+mgv_di.wdweff_w;

	switch  (mode)  {
	case MGC_DRAG_START:
		y = mgv_ti[trcno-1].xenv->orig.y-mgv_di.unitheight/2+1;
		break;
	case MGC_DRAG_RUBBER_BOX:
		y = mgv_ti[trcno-1].xenv->orig.y+mgv_di.unitheight/2+1;
		mgv_drag.y1 = y - mgv_di.unitheight;
		break;
	case MGC_DRAG_MOVE_BOX:
		y = mgv_ti[trcno-1].xenv->orig.y;
		break;
	} /*endswitch*/

	switch  (mode)  {
	case MGC_DRAG_START:
		if  (visible)  {
			if  (mgv_drag.x1 > mgv_drag.x2)  {
				tmp = mgv_drag.x1;
				mgv_drag.x1 = mgv_drag.x2;
				mgv_drag.x2 = tmp;
			} /*endif*/
			if  (mgv_drag.y1 > mgv_drag.y2)  {
				tmp = mgv_drag.y1;
				mgv_drag.y1 = mgv_drag.y2;
				mgv_drag.y2 = tmp;
			} /*endif*/
			if  (y >= mgv_drag.y1 && y <= mgv_drag.y2 &&
				x > mgv_drag.x2-GpGetInt(cGpI_drag_box_rubber_val)
				&& x < mgv_drag.x2+GpGetInt(cGpI_drag_box_rubber_val))  {
				mode = MGC_DRAG_RUBBER_BOX;
			} else {
				mode = MGC_DRAG_MOVE_BOX;
			} /*endif*/
		} else {
			mgv_drag.x1 = mgv_drag.x2 = x;
			mgv_drag.y1 = mgv_drag.y2 = y;
			mode = MGC_DRAG_RUBBER_BOX;
		} /*endif*/
		mgv_drag.on = TRUE;
		break;
	case MGC_DRAG_RUBBER_BOX:
		mgv_drag.x2 = x;
		mgv_drag.y2 = y;
		mgv_drag.on = TRUE;
		if  (mgv_drag.x2-mgv_drag.x1 < GpGetInt(cGpI_min_drag_box_width))
			mgv_drag.x2 = mgv_drag.x1 + GpGetInt(cGpI_min_drag_box_width);
		break;
	case MGC_DRAG_MOVE_BOX:
		bw = mgv_drag.x2 - mgv_drag.x1;
		bh = mgv_drag.y2 - mgv_drag.y1;
		bw /= 2;  bh /= 2;
		mgv_drag.x1 = x - bw;
		mgv_drag.x2 = x + bw;
		mgv_drag.y1 = y - bh;
		mgv_drag.y2 = y + bh;
		mgv_drag.on = TRUE;
		if  (mgv_drag.x1 < mgv_di.wdweff_x)  {
			mgv_drag.x1 = mgv_di.wdweff_x;
			mgv_drag.x2 = mgv_drag.x1 + 2*bw;
		} else if  (mgv_drag.x2 > (mgv_di.wdweff_x+mgv_di.wdweff_w))  {
			mgv_drag.x2 = mgv_di.wdweff_x + mgv_di.wdweff_w;
			mgv_drag.x1 = mgv_drag.x2 - 2*bw;
		} /*endif*/
		break;
	case MGC_DRAG_END:
		mgv_drag.on = TRUE;
		break;
	default:
		printf( "*SHM: mg_do_drag: illegal drag mode ***\n" );
	} /*endswitch*/

	if  (mgv_drag.on)  {  /* draw new box */
		box[0].x = mgv_drag.x1;   box[0].y = mgv_drag.y1;
		box[1].x = mgv_drag.x1;   box[1].y = mgv_drag.y2;
		box[2].x = mgv_drag.x2;   box[2].y = mgv_drag.y2;
		box[3].x = mgv_drag.x2;   box[3].y = mgv_drag.y1;
		box[4].x = mgv_drag.x1;   box[4].y = mgv_drag.y1;
		pix_DrawLines( XtDisplay(w), XtWindow(w), mgv_xor_gc,
			box, 5, CoordModeOrigin );
	} /*endif*/

	XFlush( XtDisplay(w) );

	last_mode = mode;

} /* end of mg_do_drag */



/*----------------------------------------------------------------------------*/



void mg_plot_single( Widget w, int trcno, float t_start, float t_end,
	BOOLEAN acc, int pixoff, STATUS *status )

/* plots a single trace into the single trace window
 *
 * parameters of routine
 * Widget     w;         input; widget ID of single trace window
 * int        trcno;     input; trace number
 * float      t_start;   input; window start (sec)
 * float      t_end;     input; window end (sec)
 * BOOLEAN    acc;       input; show accuracies
 * int        pixoff;    input; pixel offset; if 0, then main trace
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	static float    vnorm;  /* vertical normalization */
	float    *smp;          /* pointer to samples */
	long     firstsmp;      /* first sample to be plotted */
	long     trclth;        /* length of trace in samples */
	int      x_orig;        /* x-position of trace */
	int      y_orig;        /* y-position of trace */
	float    x_correct;     /* correction of origin */
	float    maxval;        /* absolute maximum amplitude of trace */
	float    minsmp, maxsmp;/* minimum and maximum values of samples */
	long     i, s;          /* sample counter */
	XPoint   *pxy;          /* pixel array */
	XPoint   *p;            /* moving pointer */
	float    xinc;          /* x-increment */
	int      efflth;        /* effective plot length */
	int      oldx, oldy;    /* pixel coo's of previous sample */
	int      newx, newy;    /* pixel coo's of current sample */
	STATUS   locstat;       /* local status */
	float    meanval;       /* mean value */
	float    dt;            /* sample distance in sec */
	TPiPhaseList *phaselist;/* phase list */
	TPiPhase phase;         /* phase info */
	GC       locgc;         /* local GC */
	char     spanstr[cBcLineLth+1]; /* string for time span info */
	char     fmtstr[cBcLineLth+1]; /* format string */
	char     *minmaxfmt;    /* pointer to minmax format string */

	/* executable code */

	if  (!mgv_gc_init)  mgh_init_graphic_contexts( w, &mgv_gc_init );

	locgc = mgv_single_gc;
	if  (pixoff != NO3TRACES)  {
		if  (pixoff > 0)  locgc = mgv_single2_gc;
		if  (pixoff < 0)  locgc = mgv_single3_gc;
	} /*endif*/

	trcno--;
	if  (trcno >= mgv_di.trcno)  return;

	if  (pixoff == 0 || pixoff == NO3TRACES)
		pix_ClearWindow( XtDisplay(w), XtWindow(w) );
	mgv_sdi.type = MGC_WDW_SINGLE;
	mg_get_windowsize( w, &(mgv_sdi.wdw_w), &(mgv_sdi.wdw_h) );
	mgv_sdi.wdweff_w = mgv_sdi.wdw_w - mgv_wdwsetup.margin_r -
		mgv_wdwsetup.margin_l;
	mgv_sdi.wdweff_h = mgv_sdi.wdw_h - mgv_wdwsetup.margin_b -
		mgv_wdwsetup.margin_t;
	if  (mgv_sdi.wdweff_w < 2 || mgv_sdi.wdweff_h < 2)  {
		printf( "*SHM: zero window ***\n" );
		mgv_sdi.trcno = 0;
		return;
	} /*endif*/
	mgv_sdi.wdweff_x = mgv_wdwsetup.margin_l;
	mgv_sdi.wdweff_y = mgv_wdwsetup.margin_t/*b*/;
	mgv_sdi.tw_start = t_start;
	mgv_sdi.tw_end = t_end;
	mgv_sdi.tw_set = TRUE;
	mgv_sdi.trcno = 1;
	mgv_sdi.unitheight = (float)(mgv_sdi.wdweff_h) / (float)(mgv_sdi.trcno);

	dt = db_getr( mgv_ti[trcno].trc, ER_DELTA, status );
	firstsmp = dm_getsample( mgv_ti[trcno].trc, mgv_sdi.tw_start, TRUE );
	smp = (float *)db_getp( mgv_ti[trcno].trc, EP_DATA, status ) + firstsmp;
	if  (Severe(status))  return;
	trclth = dm_getsample( mgv_ti[trcno].trc, mgv_sdi.tw_end, TRUE );
	trclth -= firstsmp - 1;
	if  (trclth <= 1)  return;

	if  (Severe(status))  return;
	mgv_sdi.totslth = (float)trclth * dt;
	mgv_sdi.pixpersec = (float)(mgv_sdi.wdweff_w) / mgv_sdi.totslth;

	/* find maximum amplitude and mean value */
	maxval = minsmp = maxsmp = Abs(*smp);
	meanval = 0.;
	for  (i=1; i<trclth; i++)  {
		if  (Abs(smp[i]) > maxval)  maxval = Abs(smp[i]);
		if  (smp[i] > maxsmp)  maxsmp = smp[i];
		if  (smp[i] < minsmp)  minsmp = smp[i];
		meanval += smp[i];
	} /*endfor*/
	meanval /= (float)trclth;
	maxval -= meanval;

	x_correct = (float)firstsmp*dt + db_getr(mgv_ti[trcno].trc,ER_TORIG,status)
		- mgv_sdi.tw_start;
	if  (Severe(status))  return;
	if  (GpGetInt(cGpI_debug_level) > 0)
		if  (Abs(x_correct) > dt)
			printf("SHM-dbg1: x_correct %f, dt %f\n",x_correct,dt);
	x_orig = mgv_sdi.wdweff_x + Nint(x_correct*mgv_sdi.pixpersec);
	y_orig = mgv_wdwsetup.margin_t/*b*/ + mgv_sdi.unitheight / 2.0;
	if  (pixoff != NO3TRACES)  y_orig += pixoff;

	pxy = (XPoint *)sy_allocmem( trclth, (int)sizeof(XPoint), status );
	if  (Severe(status))  return;

	p = pxy;
	xinc = mgv_sdi.pixpersec * dt;
	if  (pixoff == 0 || pixoff == NO3TRACES)  {
		vnorm = mgv_sdi.unitheight / maxval * 0.5;
		if  (pixoff != NO3TRACES)  vnorm /= 2.0;
	} /*endif*/
	efflth = 1;
	oldx = p->x = x_orig;
	oldy = p->y = y_orig - Nint( (*smp-meanval) * vnorm );
	p++; smp++;
	for  (s=1; s<trclth; s++)  {
		newx = x_orig + Nint( s*xinc );
		newy = y_orig - Nint( (*smp-meanval) * vnorm );
		if  (newx != oldx || newy != oldy)  {
			p->x = newx;
			p->y = newy;
			p++;
			efflth++;
			oldx = newx;
			oldy = newy;
		} /*endif*/
		smp++;
	} /*endfor*/
	pix_DrawLines( XtDisplay(w), XtWindow(w), locgc,
		pxy, efflth, CoordModeOrigin );

	/* mark zero line */
	pxy[0].x = x_orig + mgv_sdi.wdweff_w + 10;
	pxy[0].y = y_orig + Nint( meanval * vnorm );
	pxy[1].x = pxy[0].x + 10;
	pxy[1].y = pxy[0].y;
	pix_DrawLines( XtDisplay(w), XtWindow(w), locgc,
		pxy, 2, CoordModeOrigin );

	sy_deallocmem( pxy );

	locstat = BC_NOERROR;
	db_gett( mgv_ti[trcno].trc, ET_START, &mgv_ts_axis.abstime, &locstat );
	tc_aadd( &mgv_ts_axis.abstime, -db_getr(mgv_ti[trcno].trc,ER_TORIG,&locstat),
		&mgv_ts_axis.abstime );
	mgv_ts_axis.use_abstime = (locstat == BC_NOERROR);
	mg_haxis( w, &mgv_ts_axis, &mgv_sdi );

	if  (pixoff == 0 || pixoff == NO3TRACES)  {

		/* mark phases */
		phaselist = PmGetPhaseList( mgv_ti[trcno].trc );
		if  (phaselist != NULL)  {
			for  (i=0; i<PiPhaseListLength(phaselist); i++)  {
				phase = *PiGetPhase( phaselist, i, NULL );
				/*phase.phasetrc = mg_trcptr( trcno+1 );*/
				mg_mark_one_phase( (Widget)0, w, &phase, mgv_ti[trcno].trc, acc );
			} /*endfor*/
		} /*endif*/

		/* print width of window as text */
		minmaxfmt = GpGetString( cGpS_minmax_format );
		if  (*minmaxfmt != '\0')  {
			sprintf( fmtstr, "width %%4.2f sec  min:%s max:%s",
				minmaxfmt, minmaxfmt );
			sprintf( spanstr, fmtstr, mgv_sdi.totslth, minsmp, maxsmp );
		} else {
			sprintf( spanstr, "width %4.2f sec", mgv_sdi.totslth );
		} /*endif*/
		pix_DrawString( XtDisplay(w), XtWindow(w), mgv_single_gc,
			x_orig, TEXT_Y_SPAN_SINGLE, spanstr, strlen(spanstr) );

	} /*endif*/

	/* print trace info */
	mg_print_s_trcinfo( w, &mgv_sdi, mgv_ti, trcno, pixoff, &locstat );

	XFlush( XtDisplay(w) );

} /* end of mg_plot_single */



/*----------------------------------------------------------------------------*/



void mg_plot_drag_window( Widget w, BOOLEAN acc, STATUS *status )

/* plots time window selected by drag box into single trace window
 *
 * parameters of routine
 * Widget     w;        input; widget ID of single trace window
 * BOOLEAN    acc;      input; show accuracies on phases
 * STATUS     *status;  output; return status */
{
	/* local variables */
	int      trcno;       /* main trace number */
	int      trca, trcb;  /* other traces */
	float    t_lo, t_hi;  /* time window */
	BOOLEAN  ok;          /* window ok */
	int      y;           /* y-position of trace */

	/* executable code */

	if  (!mgv_drag.on)  {
		pix_ClearWindow( XtDisplay(w), XtWindow(w) );
		XFlush( XtDisplay(w) );
		return;
	} /*endif*/

	y = (mgv_drag.y1 + mgv_drag.y2) / 2;
	mg_get_time_and_trace( mgv_drag.x1, y, &t_lo, &trcno, &ok );
	if  (!ok)  return;
	mg_get_time_and_trace( mgv_drag.x2, y, &t_hi, NULL, &ok );
	if  (!ok)  return;

	if  (mgv_show_3_traces)  {
		mg_plot_single( w, trcno, t_lo, t_hi, acc, 0, status );
		y = mgv_sdi.wdweff_h / 4;
		mg_find_3_traces( trcno, &trca, &trcb );
		if  (trca > 0)
			mg_plot_single( w, trca, t_lo, t_hi, acc, y, status );
		if  (trcb > 0)
			mg_plot_single( w, trcb, t_lo, t_hi, acc, -y, status );
	} else {
		mg_plot_single( w, trcno, t_lo, t_hi, acc, NO3TRACES, status );
	} /*endif*/


} /* end of mg_plot_drag_window */



/*----------------------------------------------------------------------------*/



void mg_draw_cursor( Widget w, int wdwno, MGT_DSPCTRL *ctrl, int mode,
	int x, int y, BOOLEAN *ok )

/* plots graphic cursor
 *
 * parameters of routine
 * Widget     w;       input; widget ID of window
 * int        wdwno;   input; window number
 * MGT_DSPCTRL *ctrl;  input; display control
 * int        mode;    input; cursor mode
 * int        x, y;    input; cursor position
 * BOOLEAN    *ok;     output; if != NULL, cursor display succeeded ?
 */
{
	/* local variables */
	static int      cpos_x;        /* old cursor position */
	static int      cpos_y;        /* -- " -- */
	static BOOLEAN  cpos_on;       /* cursor visible */
	static int      last_mode=MGC_CRSR_NOCURSOR;     /* last mode */
	int      curr_mode;            /* current mode */
	int      wdw_w;                /* window width */
	int      wdw_h;                /* window height */
	BOOLEAN  visible;              /* cursor is visible */
	MGT_DSPINFO *ldi;              /* pointer to display info */
	TSyBoolean set_xcursor;        /* set x-cursor ? */

	/* executable code */

	if  (ok != NULL)  *ok = TRUE;
	set_xcursor = TRUE;

	if  (!mgv_gc_init)  mgh_init_graphic_contexts( w, &mgv_gc_init );

	ldi = (wdwno == MGC_WDW_MAIN) ? &mgv_di : &mgv_sdi;

	if  ((mode == MGC_CRSR_REFRESH || mode == MGC_CRSR_CLEAR) &&
		!cpos_on)  /* nothing to do */
		return;

#ifdef XXX
	if  (mode != MGC_CRSR_CROSSHAIR && mode != MGC_CRSR_WAVEFORM &&
		mode != MGC_CRSR_NOCURSOR)  {
		curr_mode = last_mode;
		if  (curr_mode != MGC_CRSR_CROSSHAIR &&
			curr_mode != MGC_CRSR_WAVEFORM &&
			curr_mode != MGC_CRSR_NOCURSOR)
			return;
	} else {
		curr_mode = mode;
	} /*endif*/
#endif
	if  (mode > MGC_CRSR_LAST)  {
		set_xcursor = FALSE;
		curr_mode = last_mode;
		if  (curr_mode > MGC_CRSR_LAST)
			return;
	} else {
		curr_mode = mode;
	} /*endif*/
	visible = cpos_on;

	mg_get_windowsize( w, &wdw_w, &wdw_h );

	switch  (curr_mode)  {
	case MGC_CRSR_NOCURSOR:
		if  (set_xcursor)  {
			mg_set_cursor( MGC_XCRSR_NORMAL );
		} /*endif*/
		break;
	case MGC_CRSR_CROSSHAIR:
		if  (set_xcursor)
			mg_set_cursor( MGC_XCRSR_CROSS );
		if  (cpos_on)  {
			pix_DrawLine(  XtDisplay(w), XtWindow(w), mgv_xor_gc,
				0, cpos_y, wdw_w-1, cpos_y );
			pix_DrawLine(  XtDisplay(w), XtWindow(w), mgv_xor_gc,
				cpos_x, 0, cpos_x, wdw_h-1 );
			cpos_on = FALSE;
		} /*endif*/
		if  (mode == MGC_CRSR_REFRESH)  {
			if  (visible)  cpos_on = TRUE;
			last_mode = curr_mode;
			return;
		} /*endif*/
		if  (mode == MGC_CRSR_CLEAR)  {last_mode = curr_mode; return;}
		cpos_x = x;
		cpos_y = y;
		pix_DrawLine(  XtDisplay(w), XtWindow(w), mgv_xor_gc,
			0, cpos_y, wdw_w-1, cpos_y );
		pix_DrawLine(  XtDisplay(w), XtWindow(w), mgv_xor_gc,
			cpos_x, 0, cpos_x, wdw_h-1 );
		cpos_on = TRUE;
		break;
	case MGC_CRSR_WAVEFORM:
	case MGC_CRSR_WAVEFORM_NEG:
	case MGC_CRSR_WAVEFORM_HILB:
	case MGC_CRSR_WAVEFORM_NEGHILB:
		if  (set_xcursor)
			mg_set_cursor( MGC_XCRSR_WAVE );
		if  (!mgv_drag.on)  {
			if  (ok != NULL)  *ok = FALSE;
			last_mode = curr_mode;
			return;
		} /*endif*/
		if  (cpos_on)  {
			mgh_wave_cursor( w, WAVE_DRAW, ldi, ctrl, cpos_x, cpos_y);
			cpos_on = FALSE;
		} /*endif*/
		if  (mode == MGC_CRSR_REFRESH)  {
			if  (visible)  cpos_on = TRUE;
			last_mode = curr_mode;
			return;
		} /*endif*/
		if  (mode == MGC_CRSR_OFF)  {last_mode = curr_mode; return;}
		if  (mode == MGC_CRSR_CLEAR)  {
			mgh_wave_cursor( w, WAVE_CLEAR, ldi, ctrl, 0, 0 );
			last_mode = curr_mode;
			return;
		} /*endif*/
		/* switch to correct waveform */
		if  (mode == MGC_CRSR_WAVEFORM)  {
			/* positive sign, no hilbert trafo */
			mgh_wave_cursor( w, WAVE_SIGN, ldi, ctrl, 1, 0 );
			mgh_wave_cursor( w, WAVE_HILB, ldi, ctrl, 0, 0 );
		} else if  (mode == MGC_CRSR_WAVEFORM_NEG)  {
			/* negative sign, no hilbert trafo */
			mgh_wave_cursor( w, WAVE_SIGN, ldi, ctrl, -1, 0 );
			mgh_wave_cursor( w, WAVE_HILB, ldi, ctrl, 0, 0 );
		} else if  (mode == MGC_CRSR_WAVEFORM_HILB)  {
			/* positive sign, hilbert trafo on */
			mgh_wave_cursor( w, WAVE_SIGN, ldi, ctrl, 1, 0 );
			mgh_wave_cursor( w, WAVE_HILB, ldi, ctrl, 1, 0 );
		} else if  (mode == MGC_CRSR_WAVEFORM_NEGHILB)  {
			/* negative sign, hilbert trafo on */
			mgh_wave_cursor( w, WAVE_SIGN, ldi, ctrl, -1, 0 );
			mgh_wave_cursor( w, WAVE_HILB, ldi, ctrl, 1, 0 );
		} /*endif*/
		cpos_x = x;
		cpos_y = y;
		mgh_wave_cursor( w, WAVE_DRAW, ldi, ctrl, cpos_x, cpos_y );
		cpos_on = TRUE;
		break;
	default:
		mg_set_cursor( MGC_XCRSR_NORMAL );
		printf( "*SHM: illegal cursor mode %d\n", curr_mode );
		last_mode = curr_mode;
		return;
	} /*endswitch*/

	last_mode = curr_mode;

} /* end of mg_draw_cursor */



/*----------------------------------------------------------------------------*/



static void mgh_wave_cursor( Widget w, int mode, MGT_DSPINFO *di,
	MGT_DSPCTRL *ctrl, int px, int py )

/* Draws waveform cursor if mode==WAVE_DRAW.  If mode==WAVE_SIGN,
 * px contains the waveform sign.  If mode==WAVE_HILB, px contains
 * flag for do_hilbert.
 *
 * parameters of routine
 * Widget     w;          input; widget ID
 * int        mode;       input; draw or clear
 * MGT_DSPINFO *di;       input; display parameters
 * MGT_DSPCTRL *ctrl;     input; display control
 * int        px, py;     input; plot position
 */
{
	/* local variables */
	static float    *wavptr=NULL;     /* waveform pointer */
	static long     trclth;           /* length of waveform */
	static XPoint   *pxy;             /* plot array */
	static float    dt;               /* sample distance */
	static float    maxval;           /* maximum amplitude */
	static float    g_vnorm;          /* vertical normalization */
	static int      w_sign=1;         /* sign of wave */
	static BOOLEAN  do_hilbert=FALSE; /* perform Hilbert trafo */
	static BOOLEAN  hilbert_done=FALSE;/* Hilbert trafo done */
	float    vnorm;                   /* actual used normalization */
	int      trcno;                   /* trace number */
	STATUS   locstat=BC_NOERROR;      /* return status */
	int      y;                       /* y-position */
	BOOLEAN  ok;                      /* trcno ok */
	float    t_start, t_end;          /* time window */
	long     s_first;                 /* first sample */
	float    *datptr;                 /* data pointer */
	long     i;                       /* counter */
	XPoint   *p;                      /* moving pointer */
	float    xinc;                    /* x increment */
	long     efflth;                  /* effective length */
	int      oldx, oldy, newx, newy;  /* plot coordinates */
	int      diffx, diffy;            /* difference of coo's */
	float    *smp;                    /* moving pointer */
	long     s;                       /* sample counter */
	float    *hilbptr;                /* temporary storage for Hilbert pointer */
	GC       local_gc;                /* local GC */

	/* executable code */

	if  (mode == WAVE_CLEAR)  {
		if  (wavptr != NULL)  {
			sy_deallocmem( wavptr );
			wavptr = NULL;
			sy_deallocmem( pxy );
			pxy = NULL;
			g_vnorm = 0.0;
		} /*endif*/
		return;
	} else if  (mode == WAVE_SIGN)  {
		w_sign = px;
		return;
	} else if  (mode == WAVE_HILB)  {
		do_hilbert = (px == 1);
		return;
	} /*endif*/

	/* get waveform if not yet allocated */
	if  (wavptr == NULL)  {
		if  (!mgv_drag.on)  {
			if  (GpGetInt(cGpI_debug_level) > 0)
				printf( "SHM-dbg1: no waveform available\n" );
			return;
		} /*endif*/
		if  (mgv_di.trcno == 0)  {
			if  (GpGetInt(cGpI_debug_level) > 0)
				printf( "SHM-dbg1: no trace available\n" );
			return;
		} /*endif*/
		y = (mgv_drag.y1 + mgv_drag.y2) / 2;
		mg_get_time_and_trace( mgv_drag.x1, y, &t_start, &trcno, &ok );
		if  (!ok)  {
			if  (GpGetInt(cGpI_debug_level) > 0)
				printf("SHM-dbg1: ts ts ts...\n" );
			return;
		} /*endif*/
		mg_get_time_and_trace( mgv_drag.x2, y, &t_end, NULL, &ok );
		if  (!ok)  {
			if  (GpGetInt(cGpI_debug_level) > 0)
				printf("SHM-dbg1: ts ts ts...\n" );
			return;
		} /*endif*/
		trcno--;
		datptr = (float *)db_getp( mgv_ti[trcno].trc, EP_DATA, NULL );
		s_first = dm_getsample( mgv_ti[trcno].trc, t_start, TRUE );
		trclth = dm_getsample( mgv_ti[trcno].trc, t_end, TRUE );
		trclth -= s_first + 1;
		if  (trclth <= 1)  {
			printf( "*SHM: waveform too short\n" );
			return;
		} /*endif*/
		datptr += s_first;
		wavptr = (float *)sy_allocmem( trclth, (int)sizeof(float),
			&locstat );
		if  (Severe(&locstat))  {
			wavptr = NULL;
			printf( "*SHM: waveform allocation error (1)\n" );
			return;
		} /*endif*/
		pxy = (XPoint *)sy_allocmem( trclth, (int)sizeof(XPoint),
			&locstat );
		if  (Severe(&locstat))  {
			sy_deallocmem( wavptr );
			wavptr = NULL;
			pxy = NULL;
			printf( "*SHM: waveform allocation error (2)\n" );
			return;
		} /*endif*/
		maxval = *datptr;
		for (i=0; i<trclth; i++)  {
			wavptr[i] = datptr[i];
			if  (Abs(datptr[i]) > maxval)  maxval = Abs(datptr[i]);
		} /*endfor*/
		dt = db_getr( mgv_ti[trcno].trc, ER_DELTA, NULL );
		g_vnorm = db_getr( mgv_ti[trcno].trc, ER_NORM, NULL );
		g_vnorm *= db_getr( mgv_ti[trcno].trc, ER_ZOOM, NULL );
		g_vnorm *= di->unitheight * ctrl->zoom;
		hilbert_done = FALSE;
	} /*endif*/

	if  (do_hilbert && !hilbert_done)  {
		hilbptr = (float *)sy_allocmem( trclth, (int)sizeof(float),
			&locstat );
		if  (Severe(&locstat))  {
			sy_deallocmem( wavptr );
			wavptr = NULL;
			pxy = NULL;
			printf( "*SHM: waveform allocation error (3)\n" );
			return;
		} /*endif*/
		ff_hilbert( wavptr, trclth, hilbptr, &locstat );
		if  (Severe(&locstat))  {
			printf( "*SHM: hilbert trafo error ***\n" );
			sy_deallocmem( hilbptr );
			locstat = BC_NOERROR;
		} else {
			sy_deallocmem( wavptr );
			wavptr = hilbptr;
		} /*endif*/
		hilbert_done = TRUE;
	} /*endif*/

	local_gc = mgv_xor_gc;
	if  (do_hilbert)  local_gc = mgv_theo_gc;
	if  (w_sign == -1)  local_gc = mgv_auto_gc;

	/* plot waveform */
	smp = wavptr;
	p = pxy;
	xinc = di->pixpersec * dt;
	vnorm = (di->type == MGC_WDW_SINGLE) ? di->unitheight/maxval * 0.5 : g_vnorm;
	efflth = 1;
	oldx = p->x = px;
	oldy = p->y = py - w_sign * Nint( *smp * vnorm );
	p++; smp++;
	for  (s=1; s<trclth; s++)  {
		newx = px + Nint( s*xinc );
		newy = py - w_sign * Nint( *smp * vnorm );
		diffx = newx - oldx;
		diffy = newy - oldy;
		diffx = Abs( diffx );
		diffy = Abs( diffy );
		if  (diffx > 1 || diffy > 1)  {
			p->x = newx;
			p->y = newy;
			p++;
			efflth++;
			oldx = newx;
			oldy = newy;
		} /*endif*/
		smp++;
	} /*endfor*/
	pix_DrawLines( XtDisplay(w), XtWindow(w), local_gc,
		pxy, efflth, CoordModeOrigin );

} /* end of mgh_wave_cursor */



/*----------------------------------------------------------------------------*/



void mg_do_phase( Widget w, int wdwno, int mode, int x, int y, char onset[],
	int *no, TPiPhase *close )

/* manages selecting, moving and deleting of phases
 *
 * parameters of routine
 * Widget     w;      input; widget of trace window
 * int        wdwno;  input; window number
 * int        mode;   input; selection mode
 * int        x, y;   input; position of pointer
 * char       onset[];output; if != NULL returns onset time string
 * int        *no;    output; if != NULL returns trace number
 * TPiPhase  *close; output; if != NULL returns close phase
 */
{
	/* local variables */
	static int      phase_x;       /* position of mark (x) */
	static int      phase_y;       /* position of mark (y) */
	static Widget   lastwid;       /* last widget */
	static int      lastwdw=0;     /* last window number */
	MGT_DSPINFO     *ldi;          /* pointer to display info */
	float           phase_pos;     /* time position */
	int             phase_trc;     /* phase trace */
	int             halflth;       /* half length of mark */
	TIME            tstart;        /* start time */
	STATUS          locstat=BC_NOERROR; /* local status */
	float           close_pos;     /* variable for close phase */
	int             close_trc;     /* -- " -- */
	BOOLEAN         close_ok;      /* -- " -- */
	TPiPhase        close_phase;   /* -- " -- */

	/* executable code */

	/* check for appropriate x coo */
	if  (x <= mgv_di.wdweff_x)  x = mgv_di.wdweff_x + 1;
	if  (x >= (mgv_di.wdweff_x+mgv_di.wdweff_w))
		x = mgv_di.wdweff_x + mgv_di.wdweff_w - 1;

	if  (wdwno == MGC_WDW_LAST)  {
		if  (lastwdw == 0)  return;
		wdwno = lastwdw;
		w = lastwid;
	} else if  (wdwno != lastwdw && lastwdw != 0)  {
		mg_do_phase( lastwid, lastwdw, MGC_PHASE_CLEAR, 0, 0, NULL,NULL,NULL);
	} /*endif*/
	lastwdw = wdwno;
	lastwid = w;

	if  ((mode == MGC_PHASE_CLEAR || mode == MGC_PHASE_REFRESH) &&
		!mgv_phase_sel)  {
		if  (close != NULL)  close->name[0] = '\0';
		if  (onset != NULL)  onset[0] = '\0';
		return;
	} /*endif*/
	if  ((mgv_di.trcno == 0) || (wdwno == MGC_WDW_SINGLE && !mgv_drag.on))  {
		if  (close != NULL)  close->name[0] = '\0';
		if  (onset != NULL)  onset[0] = '\0';
		if  (no != NULL)  *no = 0;
		return;
	} /*endif*/

	if  (!mgv_gc_init)  mgh_init_graphic_contexts( w, &mgv_gc_init );

	ldi = (wdwno == MGC_WDW_SINGLE) ? &mgv_sdi : &mgv_di;
	halflth = Nint( (ldi->unitheight) / 2.0 );

	/* if a close phase exists already, delete it */
	if  (mode == MGC_PHASE_START && !mgv_phase_sel)  {
		if  (wdwno == MGC_WDW_SINGLE)  {
			mg_get_time_and_trace_single( x, y, &close_pos,
				&close_trc, &close_ok );
		} else {
			mg_get_time_and_trace( x, y, &close_pos,
				&close_trc, &close_ok );
		} /*endif*/
		if  (close_ok)  {
			mgh_find_phase( close_trc, close_pos,
				GpGetFloat(cGpF_close_phase_resol)/(ldi->pixpersec),
				&close_phase, &close_ok );
			if  (close_ok && close != NULL)  {
				PmRemovePhase( mgv_ti[close_trc-1].trc, &close_phase, &locstat );
				locstat = BC_NOERROR;
				*close = close_phase;
				/*close->phasetrc = mg_trcptr( close_trc );*/
			} else if  (close != NULL)  {
				/*close->phasetrc = NULL;*/
				close->name[0] = '\0';
			} /*endif*/
		} /*endif*/
	} else {
		if  (close != NULL)
			close->name[0] = '\0';
	} /*endif*/

	switch  (mode)  {
	case MGC_PHASE_START:
	case MGC_PHASE_REFRESH:
	case MGC_PHASE_CLEAR:
		if  (mgv_phase_sel)  {  /* delete old mark */
			pix_DrawLine( XtDisplay(w), XtWindow(w), mgv_phase_gc,
				phase_x, phase_y-halflth, phase_x, phase_y+halflth );
			mgv_phase_sel = FALSE;
		} /*endif*/
		if  (mode == MGC_PHASE_REFRESH)  {
			mgv_phase_sel = TRUE;
			return;
		} /*endif*/
		if  (mode == MGC_PHASE_CLEAR)  return;
		if  (wdwno == MGC_WDW_SINGLE)  {
			mg_get_time_and_trace_single( x, y, &phase_pos,
				&phase_trc, &mgv_phase_sel );
		} else {
			mg_get_time_and_trace( x, y, &phase_pos,
				&phase_trc, &mgv_phase_sel );
		} /*endif*/
		if  (!mgv_phase_sel)  return;
		phase_trc--;
		phase_x = x;
		if  (wdwno == MGC_WDW_SINGLE)  {
			phase_y = mgv_wdwsetup.margin_t/*b*/ + mgv_sdi.unitheight / 2.0;
		} else {
			phase_y = mgv_ti[phase_trc].xenv->orig.y;
		} /*endif*/
		pix_DrawLine( XtDisplay(w), XtWindow(w), mgv_phase_gc,
			phase_x, phase_y-halflth, phase_x, phase_y+halflth );
		mgv_phase_sel = TRUE;
		break;
	default:
		printf( "*SHM: mg_do_phase:  illegal phase mode\n" );
		return;
	} /*endswitch*/

	if  (onset == NULL)  return;

	*onset = '\0';
	db_gett( mgv_ti[phase_trc].trc, ET_START, &tstart, &locstat );
	if  (Severe(&locstat))  return;
	tc_a2t( &tstart, onset, &locstat );
	if  (Severe(&locstat))  return;
	phase_pos -= db_getr( mgv_ti[phase_trc].trc, ER_TORIG, &locstat );
	tc_tadd( onset, phase_pos, onset, &locstat );
	if  (no != NULL)  *no = phase_trc + 1;

} /* end of mg_do_phase */



/*----------------------------------------------------------------------------*/



void mg_mark_one_phase( Widget wm, Widget ws, TPiPhase *phase, TPmTrace *phtrc,
	TSyBoolean acc )

/* marks a phase on a specified trace
 *
 * parameters of routine
 * Widget     wm;        input; widget of main window
 * Widget     ws;        input; widget of single trace window
 * int        trcno;     input; trace number
 * TPiPhase   *phase;    input; pointer to phase info
 * TPmTrace   *phtrc;    input; pointer to phase trace
 * TSyBoolean acc;       input; draw accuracies
 */
{
	/* local variables */
	float    reltime;                 /* relative time */
	char     label[BC_LINELTH+1];     /* label string */
	TIME     tstart;                  /* start time */
	TIME     onset;                   /* onset time */
	STATUS   locstat=BC_NOERROR;      /* local status */
	/* int      trcno;         */          /* trace number */
	int      phase_kind;              /* kind of phase */
	float    acc_l, acc_r;            /* accuracies */
	char     *cptr;                   /* moving pointer */

	/* executable code */

	if  (phtrc == NULL)  {
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: mg_mark_one_phase: NULL phtrc\n" );
		return;
	} /*endif*/

	db_gett( phtrc, ET_START, &tstart, &locstat );
	if  (Severe(&locstat))  return;
	tc_t2a( phase->onset, &onset, &locstat );
	if  (Severe(&locstat))  return;
	reltime = tc_adiff( &onset, &tstart ) +
		db_getr( phtrc, ER_TORIG, &locstat );

	if  (acc)  {
		acc_l = phase->onset_acc_l;
		acc_r = phase->onset_acc_r;
	} else {
		acc_l = acc_r = 0.0;
	} /*endif*/

	if  (GpGetBoolean(cGpB_full_phase_names))  {
		cptr = label;
		if  (! phase->reliable)  *cptr++ = '(';
		*cptr++ = phase->spec;
		strcpy( cptr, phase->name );
		cptr += strlen( phase->name );
		if  (phase->sign == 1)  *cptr++ = 'c';
		else if  (phase->sign == -1)  *cptr++ = 'd';
		if  (! phase->reliable)  *cptr++ = ')';
		*cptr = '\0';
	} else {
		if  (phase->reliable)  {
			strcpy( label, phase->name );
		} else {
			sprintf( label, "(%s)", phase->name );
		} /*endif*/
	} /*endif*/
	if  (phase->source == cPiSourceTheo)  {
		if  (!mgv_colored)  strcat( label, "(t)" );
		phase_kind = MGC_PHASE_KIND_THEO;
	} else if  (phase->source == cPiSourceAuto)  {
		if  (!mgv_colored)  strcat( label, "(a)" );
		phase_kind = MGC_PHASE_KIND_AUTO;
	} else {
		if  (phase->quality < 2)  {
			phase_kind = MGC_PHASE_KIND_MANUALLY_0;
		} else {
			phase_kind = MGC_PHASE_KIND_MANUALLY;
		} /*endif*/
	} /*endif*/
	mg_mark_trace( wm, ws, mg_trcnum(phtrc), reltime, label,
		phase_kind, acc_l, acc_r );

} /* end of mg_mark_one_phase */



/*----------------------------------------------------------------------------*/



void mg_plot_phases( Widget wm, BOOLEAN acc, STATUS *status )

/* marks all phases
 *
 * parameters of routine
 * Widget     wm;      input; widget ID of main window
 * BOOLEAN    acc;     input; show accuracies
 * STATUS     *status; output; return status
 */
{
	/* local variables */
	int      t;                   /* trace counter */
	int      p;                   /* phase counter */
	TPiPhase  phase;             /* phase info */
	TPiPhaseList *phaselist;     /* phase list */
	STATUS   locstat;             /* local status */

	/* executable code */

	for  (t=0; t<mgv_di.trcno; t++)  {
		locstat = BC_NOERROR;
		phaselist = PmGetPhaseList( mgv_ti[t].trc );
		if  (phaselist != NULL)  {
			for  (p=0; p<PiPhaseListLength(phaselist); p++)  {
				phase = *PiGetPhase( phaselist, p, NULL );
				/*phase.phasetrc = mgv_ti[t].trc;*/
				mg_mark_one_phase( wm, (Widget)0, &phase, mgv_ti[t].trc, acc );
			} /*endfor*/
		} /*endif*/
	} /*endfor*/

} /* end of mg_plot_phases */



/*----------------------------------------------------------------------------*/



static void mgh_find_phase( int trcno, float reftime, float resol,
				TPiPhase *phase, BOOLEAN *found )

/* finds phase close to specified time
 *
 * parameters of routine
 * int        trcno;         input; trace number
 * float      reftime;       input; reference time
 * float      resol;         input; resolution (allowed deviation)
 * TPiPhase  *phase;        output; phase info about phase found
 * BOOLEAN    *found;        output; a phase was found
 */
{
	/* local variables */
	TPiPhaseList *phaselist;       /* phase list */
	TPiPhase *cph;                 /* current phase */
	STATUS   locstat;              /* local status */
	int      p;                    /* phase counter */
	TIME     tstart, tcurr;        /* start time and current time */
	float    difftime;             /* difference time */
	float    torig;                /* origin time of trace */

	/* executable code */

	*found = FALSE;
	trcno--;
	locstat = cBcNoError;
	phaselist = PmGetPhaseList( mgv_ti[trcno].trc );
	if  (phaselist == NULL)  return;

	locstat = cBcNoError;
	db_gett( mgv_ti[trcno].trc, ET_START, &tstart, &locstat );
	if  (Severe(&locstat))  return;
	torig = db_getr( mgv_ti[trcno].trc, ER_TORIG, &locstat );

	for  (p=0; p<PiPhaseListLength(phaselist); p++)  {
		cph = PiGetPhase( phaselist, p, &locstat );
		tc_t2a( cph->onset, &tcurr, &locstat );
		difftime = tc_adiff( &tcurr, &tstart ) - reftime + torig;
		difftime = Abs( difftime );
		if  (difftime <= resol)  {
			*phase = *cph;
			*found = TRUE;
			return;
		} /*endif*/
	} /*endfor*/

} /* end of mgh_find_phase */



/*----------------------------------------------------------------------------*/



BOOLEAN mg_toggle_trace_selection( Widget w, int x, int y )

/* Toggles trace selection if mouse position on button event is in left margin of main
 * window
 *
 * parameters of routine
 * Widget     w;         input; drawing area widget
 * int        x, y;      input; position of mouse
 *                       returns TRUE if a trace was selected
 */
{
	/* local variables */
	float    dummy_time;         /* dummy */
	int      trcno;              /* trace number */
	BOOLEAN  ok;                 /* selection ok */

	/* executable code */

	if (x > mgv_di.wdweff_x)  return FALSE;

	mg_get_time_and_trace( x, y, &dummy_time, &trcno, &ok );
	if  (!ok)  {
		printf( "*SHM: trace selection not ok\n" );
		return FALSE;
	} /*endif*/

	ts_toggle_selection( trcno );
	mgh_mark_selected_trace( w, trcno );

	return TRUE;

} /* end of mg_toggle_trace_selection */



/*----------------------------------------------------------------------------*/



BOOLEAN mg_select_trace( Widget w, int x, int y )

/* selects trace if mouse position on button event is in left margin of main
 * window
 *
 * parameters of routine
 * Widget     w;         input; drawing area widget
 * int        x, y;      input; position of mouse
 *                       returns TRUE if a trace was selected
 */
{
	/* local variables */
	float    dummy_time;         /* dummy */
	int      trcno;              /* trace number */
	BOOLEAN  ok;                 /* selection ok */

	/* executable code */

	if (x > mgv_di.wdweff_x)  return FALSE;

	mg_get_time_and_trace( x, y, &dummy_time, &trcno, &ok );
	if  (!ok)  {
		printf( "*SHM: trace selection not ok\n" );
		return FALSE;
	} /*endif*/

	if  (!ts_is_selected(trcno))  {
		ts_select( trcno );
		mgh_mark_selected_trace( w, trcno );
	} /*endif*/

	return TRUE;

} /* end of mg_select_trace */



/*----------------------------------------------------------------------------*/



BOOLEAN mg_deselect_trace( Widget w, int x, int y )

/* deselects trace if mouse position on button event is in left margin of main
 * window
 *
 * parameters of routine
 * Widget     w;         input; drawing area widget
 * int        x, y;      input; position of mouse
 *                       returns TRUE if a trace was selected
 */
{
	/* local variables */
	float    dummy_time;         /* dummy */
	int      trcno;              /* trace number */
	BOOLEAN  ok;                 /* selection ok */

	/* executable code */

	if (x > mgv_di.wdweff_x)  return FALSE;

	mg_get_time_and_trace( x, y, &dummy_time, &trcno, &ok );
	if  (!ok)  {
		printf( "*SHM: trace selection not ok\n" );
		return FALSE;
	} /*endif*/

	if  (ts_is_selected(trcno))  {
		ts_deselect( trcno );
		mgh_mark_selected_trace( w, trcno );
	} /*endif*/

	return TRUE;

} /* end of mg_deselect_trace */



/*----------------------------------------------------------------------------*/



void mg_clear_selections( Widget w )

/* Clears all selections on display
 *
 * parameters of routine
 * Widget     w;        input; display widget
 */
{
	/* local variables */
	void     *trc;        /* pointer to trace */
	int      trcno;       /* number of trace on display */

	/* executable code */

	trcno = 1;
	trc = NULL;
	FOREVER {
		trc = db_getp( trc, EP_DSPN, NULL );
		if  (trc == NULL)  break;
		if  (ts_is_selected(trcno))  mgh_mark_selected_trace( w, trcno );
		trcno++;
	} /*endfor*/
	ts_clear_selections();

} /* end of mg_clear_selections */



/*----------------------------------------------------------------------------*/



static void mgh_mark_selected_trace( Widget w, unsigned number )

/* marks trace number "number" on screen
 *
 * parameters of routine
 * Widget     w;              input; drawing area widget
 * unsigned   number          input; trace number
 */
{
	/* local variables */

	/* executable code */

	if  (number == 0 || number > mg_dsptrcs())  return;

	pix_FillRectangle( XtDisplay(w), XtWindow(w), mgv_xor_gc,
		mgv_di.wdweff_x-65, mgv_ti[number-1].xenv->orig.y-3, 15, 12 );

} /* end of mgh_mark_selected_trace */



/*----------------------------------------------------------------------------*/



int mg_dsptrcs( void )

/* returns number of traces on display
 *
 * no parameters
 */
{
	/* executable code */

	return mgv_di.trcno;

} /* end of mg_dsptrcs */



/*----------------------------------------------------------------------------*/



void *mg_trcptr( int trcno )

/* returns TRACE pointer of trace number "trcno"
 *
 * parameters of routine
 * int        trcno;       input; trace number on display
 *                         returns pointer to trace info
 */
{
	/* executable code */

	trcno--;
	if  (trcno < 0 || trcno >= mgv_di.trcno)  return NULL;
	return mgv_ti[trcno].trc;

} /* end of mg_trcptr */



/*----------------------------------------------------------------------------*/



int mg_trcnum( void *trcptr )

/* returns trace number on display from pointer to trace.  Returns 0 if trace
 * is not on display
 *
 * parameters of routine
 * void       *trcptr;        input; pointer to trace
 */
{
	/* local variables */
	int      i;          /* counter */

	/* executable code */

	for  (i=1; i<=mg_dsptrcs(); i++)
		if  (trcptr == mg_trcptr(i))  return i;
	return 0;

} /* end of mg_trcnum */



/*----------------------------------------------------------------------------*/



static void mgh_get_colormap( Screen *s )

/* gets color map
 *
 */
{
	/* local variables */

	/* executable code */

	mgv_colored = ((XDefaultVisualOfScreen(s))->class == TrueColor
		|| (XDefaultVisualOfScreen(s))->class == PseudoColor
		|| (XDefaultVisualOfScreen(s))->class == DirectColor
		|| (XDefaultVisualOfScreen(s))->class == StaticColor);
	if  (mgv_colored)
		mgv_cmap = XDefaultColormapOfScreen( s );

} /* end of mgh_get_colormap */



/*----------------------------------------------------------------------------*/



static void mgh_init_graphic_contexts( Widget w, BOOLEAN *ok )

/* initializes global GC's and cursors
 *
 * parameters of routine
 * Widget      w;       input; widget ID
 * BOOLEAN    *ok;      output; return flag
 */
{
	/* local variables */
	int      xstatus;        /* X return value */
	XColor   screen_color;
	float    ftmp;           /* scratch */

	/* executable code */

	mgv_fixed_font = XLoadFont( XtDisplay(w), "fixed" );

	if  (w == (Widget)0)  {*ok = FALSE; return;}

	mgh_get_colormap( XtScreen(w) );  /* sets mgv_cmap & mgv_colored */

	mgv_single_gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	mgh_set_fg_color( XtDisplay(w), mgv_single_gc,
		pix_colour(PIXC_COL_FOREGROUND) );
	mgh_set_bg_color( XtDisplay(w), mgv_single_gc,
		pix_colour(PIXC_COL_BACKGROUND) );
	XSetFont( XtDisplay(w), mgv_single_gc, mgv_fixed_font );

	mgv_single2_gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	mgh_set_fg_color( XtDisplay(w), mgv_single2_gc,
		pix_colour(PIXC_COL_FOREGROUND) );
	mgh_set_bg_color( XtDisplay(w), mgv_single2_gc,
		pix_colour(PIXC_COL_BACKGROUND) );
	XSetFont( XtDisplay(w), mgv_single2_gc, mgv_fixed_font );
	if  (mgv_colored)  {
		screen_color.red = Nint( 0.0 * 65535.0 );
		screen_color.green = Nint( 0.7 * 65535.0 );
		screen_color.blue = Nint( 0.0 * 65535.0 );
		screen_color.flags = DoRed | DoGreen | DoBlue;
		xstatus = XAllocColor( XtDisplay(w), mgv_cmap, &screen_color );
		if  (xstatus == 0)  {
			mgh_set_fg_color( XtDisplay(w), mgv_single2_gc,
				XBlackPixelOfScreen(XtScreen(w)) );
		} else {
			mgh_set_fg_color( XtDisplay(w), mgv_single2_gc,
				screen_color.pixel );
		} /*endif*/
	} else {
		mgh_set_fg_color( XtDisplay(w), mgv_single2_gc,
			XBlackPixelOfScreen(XtScreen(w)) );
		XSetLineAttributes( XtDisplay(w), mgv_single2_gc, 2, LineSolid,
			CapNotLast, JoinMiter );
	} /*endif*/

	mgv_single3_gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	mgh_set_fg_color( XtDisplay(w), mgv_single3_gc,
		pix_colour(PIXC_COL_FOREGROUND) );
	mgh_set_bg_color( XtDisplay(w), mgv_single3_gc,
		pix_colour(PIXC_COL_BACKGROUND) );
	XSetFont( XtDisplay(w), mgv_single3_gc, mgv_fixed_font );
	if  (mgv_colored)  {
		screen_color.red = Nint( 0.0 * 65535.0 );
		screen_color.green = Nint( 0.4 * 65535.0 );
		screen_color.blue = Nint( 0.2 * 65535.0 );
		screen_color.flags = DoRed | DoGreen | DoBlue;
		xstatus = XAllocColor( XtDisplay(w), mgv_cmap, &screen_color );
		if  (xstatus == 0)  {
			mgh_set_fg_color( XtDisplay(w), mgv_single3_gc,
				XBlackPixelOfScreen(XtScreen(w)) );
		} else {
			mgh_set_fg_color( XtDisplay(w), mgv_single3_gc,
				screen_color.pixel );
		} /*endif*/
	} else {
		mgh_set_fg_color( XtDisplay(w), mgv_single3_gc,
			XBlackPixelOfScreen(XtScreen(w)) );
		XSetLineAttributes( XtDisplay(w), mgv_single3_gc, 2, LineSolid,
			CapNotLast, JoinMiter );
	} /*endif*/

	mgv_phase_gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetFunction( XtDisplay(w), mgv_phase_gc, GXxor );
	if  (mgv_colored)  {
		screen_color.red = Nint( GpGetFloat(cGpF_colour_crsr_red) * 65535.0 );
		screen_color.green = Nint( GpGetFloat(cGpF_colour_crsr_green) * 65535.0 );
		screen_color.blue = Nint( GpGetFloat(cGpF_colour_crsr_blue) * 65535.0 );
		screen_color.flags = DoRed | DoGreen | DoBlue;
		xstatus = XAllocColor( XtDisplay(w), mgv_cmap, &screen_color );
		if  (xstatus == 0)  {
			mgh_set_fg_color( XtDisplay(w), mgv_phase_gc,
				XBlackPixelOfScreen(XtScreen(w)) );
		} else {
			mgh_set_fg_color( XtDisplay(w), mgv_phase_gc,
				screen_color.pixel );
		} /*endif*/
	} else {
		mgh_set_fg_color( XtDisplay(w), mgv_phase_gc,
			XBlackPixelOfScreen(XtScreen(w)) );
	} /*endif*/
	mgh_set_bg_color( XtDisplay(w), mgv_phase_gc,
		pix_colour(PIXC_COL_BACKGROUND) );
	XSetFont( XtDisplay(w), mgv_phase_gc, mgv_fixed_font );

	mgv_over_gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetFunction( XtDisplay(w), mgv_over_gc, GXcopy/*GXset*/ );
	mgh_set_fg_color( XtDisplay(w), mgv_over_gc,
		pix_colour(PIXC_COL_FOREGROUND) );
	mgh_set_bg_color( XtDisplay(w), mgv_over_gc,
		pix_colour(PIXC_COL_BACKGROUND) );
	XSetFont( XtDisplay(w), mgv_over_gc, mgv_fixed_font );

	mgv_clear_gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetFunction( XtDisplay(w), mgv_clear_gc, GXcopy/*GXclear*/ );
	mgh_set_fg_color( XtDisplay(w), mgv_clear_gc,
		pix_colour(PIXC_COL_BACKGROUND) );
	mgh_set_bg_color( XtDisplay(w), mgv_clear_gc,
		pix_colour(PIXC_COL_FOREGROUND) );
	XSetFont( XtDisplay(w), mgv_clear_gc, mgv_fixed_font );

	mgv_trcalert_gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetFunction( XtDisplay(w), mgv_trcalert_gc, GXcopy/*GXset*/ );
	if  (mgv_colored)  {
		screen_color.red = Nint( GpGetFloat(cGpF_colour_alert_red) * 65535.0 );
		screen_color.green = Nint( GpGetFloat(cGpF_colour_alert_green) * 65535.0 );
		screen_color.blue = Nint( GpGetFloat(cGpF_colour_alert_blue) * 65535.0 );
		screen_color.flags = DoRed | DoGreen | DoBlue;
		xstatus = XAllocColor( XtDisplay(w), mgv_cmap, &screen_color );
		if  (xstatus == 0)  {
			mgh_set_fg_color( XtDisplay(w), mgv_trcalert_gc,
				XBlackPixelOfScreen(XtScreen(w)) );
		} else {
			mgh_set_fg_color( XtDisplay(w), mgv_trcalert_gc,
				screen_color.pixel );
		} /*endif*/
	} else {
		mgh_set_fg_color( XtDisplay(w), mgv_trcalert_gc,
			XBlackPixelOfScreen(XtScreen(w)) );
	} /*endif*/
	mgh_set_bg_color( XtDisplay(w), mgv_trcalert_gc,
		pix_colour(PIXC_COL_BACKGROUND) );
	XSetFont( XtDisplay(w), mgv_trcalert_gc, mgv_fixed_font );

	mgv_addfil_gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetFunction( XtDisplay(w), mgv_addfil_gc, GXcopy/*GXset*/ );
	if  (mgv_colored)  {
		screen_color.red = Nint( GpGetFloat(cGpF_colour_addfil_red) * 65535.0 );
		screen_color.green = Nint( GpGetFloat(cGpF_colour_addfil_green) * 65535.0 );
		screen_color.blue = Nint( GpGetFloat(cGpF_colour_addfil_blue) * 65535.0 );
		screen_color.flags = DoRed | DoGreen | DoBlue;
		xstatus = XAllocColor( XtDisplay(w), mgv_cmap, &screen_color );
		if  (xstatus == 0)  {
			mgh_set_fg_color( XtDisplay(w), mgv_addfil_gc,
				XBlackPixelOfScreen(XtScreen(w)) );
		} else {
			mgh_set_fg_color( XtDisplay(w), mgv_addfil_gc,
				screen_color.pixel );
		} /*endif*/
	} else {
		mgh_set_fg_color( XtDisplay(w), mgv_addfil_gc,
			XBlackPixelOfScreen(XtScreen(w)) );
	} /*endif*/
	mgh_set_bg_color( XtDisplay(w), mgv_addfil_gc,
		pix_colour(PIXC_COL_BACKGROUND) );
	XSetFont( XtDisplay(w), mgv_addfil_gc, mgv_fixed_font );

	mgv_xor_gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetFunction( XtDisplay(w), mgv_xor_gc, GXxor );
	if  (mgv_colored)  {
		screen_color.red = Nint( GpGetFloat(cGpF_colour_crsr_red) * 65535.0 );
		screen_color.green = Nint( GpGetFloat(cGpF_colour_crsr_green) * 65535.0 );
		screen_color.blue = Nint( GpGetFloat(cGpF_colour_crsr_blue) * 65535.0 );
		screen_color.flags = DoRed | DoGreen | DoBlue;
		xstatus = XAllocColor( XtDisplay(w), mgv_cmap, &screen_color );
		if  (xstatus == 0)  {
			mgh_set_fg_color( XtDisplay(w), mgv_xor_gc,
				XBlackPixelOfScreen(XtScreen(w)) );
		} else {
			mgh_set_fg_color( XtDisplay(w), mgv_xor_gc,
				screen_color.pixel );
		} /*endif*/
	} else {
		mgh_set_fg_color( XtDisplay(w), mgv_xor_gc,
			XBlackPixelOfScreen(XtScreen(w)) );
	} /*endif*/
	mgh_set_bg_color( XtDisplay(w), mgv_xor_gc,
		pix_colour(PIXC_COL_BACKGROUND) );
	/* XSetLineAttributes( XtDisplay(w), mgv_xor_gc, 1,
		LineOnOffDash, CapButt, JoinMiter ); */
	XSetFont( XtDisplay(w), mgv_xor_gc, mgv_fixed_font );

	mgv_auto_gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetFunction( XtDisplay(w), mgv_auto_gc, GXxor );
	if  (mgv_colored)  {
		screen_color.red = Nint( GpGetFloat(cGpF_colour_auto_red) * 65535.0 );
		screen_color.green = Nint( GpGetFloat(cGpF_colour_auto_green) * 65535.0 );
		screen_color.blue = Nint( GpGetFloat(cGpF_colour_auto_blue) * 65535.0 );
		screen_color.flags = DoRed | DoGreen | DoBlue;
		xstatus = XAllocColor( XtDisplay(w), mgv_cmap, &screen_color );
		if  (xstatus == 0)  {
			mgh_set_fg_color( XtDisplay(w), mgv_auto_gc,
				XBlackPixelOfScreen(XtScreen(w)) );
		} else {
			mgh_set_fg_color( XtDisplay(w), mgv_auto_gc,
				screen_color.pixel );
		} /*endif*/
	} else {
		mgh_set_fg_color( XtDisplay(w), mgv_auto_gc,
			XBlackPixelOfScreen(XtScreen(w)) );
	} /*endif*/
	mgh_set_bg_color( XtDisplay(w), mgv_auto_gc,
		pix_colour(PIXC_COL_BACKGROUND) );
	XSetFont( XtDisplay(w), mgv_auto_gc, mgv_fixed_font );

	mgv_theo_gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetFunction( XtDisplay(w), mgv_theo_gc, GXxor );
	if  (mgv_colored)  {
		screen_color.red = Nint( GpGetFloat(cGpF_colour_theo_red) * 65535.0 );
		screen_color.green = Nint( GpGetFloat(cGpF_colour_theo_green) * 65535.0 );
		screen_color.blue = Nint( GpGetFloat(cGpF_colour_theo_blue) * 65535.0 );
		screen_color.flags = DoRed | DoGreen | DoBlue;
		xstatus = XAllocColor( XtDisplay(w), mgv_cmap, &screen_color );
		if  (xstatus == 0)  {
			mgh_set_fg_color( XtDisplay(w), mgv_theo_gc,
				XBlackPixelOfScreen(XtScreen(w)) );
		} else {
			mgh_set_fg_color( XtDisplay(w), mgv_theo_gc,
				screen_color.pixel );
		} /*endif*/
	} else {
		mgh_set_fg_color( XtDisplay(w), mgv_theo_gc,
			XBlackPixelOfScreen(XtScreen(w)) );
	} /*endif*/
	mgh_set_bg_color( XtDisplay(w), mgv_theo_gc,
		pix_colour(PIXC_COL_BACKGROUND) );
	XSetFont( XtDisplay(w), mgv_theo_gc, mgv_fixed_font );

	mgv_mark_gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetFunction( XtDisplay(w), mgv_mark_gc, GXxor );
	if  (mgv_colored)  {
		screen_color.red = Nint( GpGetFloat(cGpF_colour_mark_red) * 65535.0 );
		screen_color.green = Nint( GpGetFloat(cGpF_colour_mark_green) * 65535.0 );
		screen_color.blue = Nint( GpGetFloat(cGpF_colour_mark_blue) * 65535.0 );
		screen_color.flags = DoRed | DoGreen | DoBlue;
		xstatus = XAllocColor( XtDisplay(w), mgv_cmap, &screen_color );
		if  (xstatus == 0)  {
			mgh_set_fg_color( XtDisplay(w), mgv_mark_gc,
				XBlackPixelOfScreen(XtScreen(w)) );
		} else {
			mgh_set_fg_color( XtDisplay(w), mgv_mark_gc,
				screen_color.pixel );
		} /*endif*/
	} else {
		mgh_set_fg_color( XtDisplay(w), mgv_mark_gc,
			XBlackPixelOfScreen(XtScreen(w)) );
		XSetLineAttributes( XtDisplay(w), mgv_mark_gc, 2, LineSolid,
			CapNotLast, JoinMiter );
	} /*endif*/
	mgh_set_bg_color( XtDisplay(w), mgv_mark_gc,
		pix_colour(PIXC_COL_BACKGROUND) );
	XSetFont( XtDisplay(w), mgv_mark_gc, mgv_fixed_font );

	mgv_mark0_gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetFunction( XtDisplay(w), mgv_mark0_gc, GXxor );
	if  (mgv_colored)  {
		screen_color.red = Nint( GpGetFloat(cGpF_colour_mark0_red) * 65535.0 );
		screen_color.green = Nint( GpGetFloat(cGpF_colour_mark0_green) * 65535.0 );
		screen_color.blue = Nint( GpGetFloat(cGpF_colour_mark0_blue) * 65535.0 );
		screen_color.flags = DoRed | DoGreen | DoBlue;
		xstatus = XAllocColor( XtDisplay(w), mgv_cmap, &screen_color );
		if  (xstatus == 0)  {
			mgh_set_fg_color( XtDisplay(w), mgv_mark0_gc,
				XBlackPixelOfScreen(XtScreen(w)) );
		} else {
			mgh_set_fg_color( XtDisplay(w), mgv_mark0_gc,
				screen_color.pixel );
		} /*endif*/
	} else {
		mgh_set_fg_color( XtDisplay(w), mgv_mark0_gc,
			XBlackPixelOfScreen(XtScreen(w)) );
		XSetLineAttributes( XtDisplay(w), mgv_mark0_gc, 2, LineSolid,
			CapNotLast, JoinMiter );
	} /*endif*/
	mgh_set_bg_color( XtDisplay(w), mgv_mark0_gc,
		pix_colour(PIXC_COL_BACKGROUND) );
	XSetFont( XtDisplay(w), mgv_mark0_gc, mgv_fixed_font );

	mgv_crsr[MGC_XCRSR_NORMAL] =
		XCreateFontCursor( XtDisplay(w), XC_sb_left_arrow );
	mgv_crsr[MGC_XCRSR_CROSS] = XCreateFontCursor( XtDisplay(w), XC_crosshair );
	mgv_crsr[MGC_XCRSR_BUSY] = XCreateFontCursor( XtDisplay(w), XC_watch );
	mgv_crsr[MGC_XCRSR_WAVE] = XCreateFontCursor( XtDisplay(w), XC_draft_small );
	mgv_crsr[MGC_XCRSR_LEFT] = XCreateFontCursor( XtDisplay(w), XC_left_tee );
	mgv_crsr[MGC_XCRSR_RIGHT] = XCreateFontCursor( XtDisplay(w), XC_right_tee );

	*ok = TRUE;

} /* end of mgh_init_graphic_contexts */



/*----------------------------------------------------------------------------*/


#define NO_TIME " --:--:--.---"


void mg_print_time( Widget w, int wdwno, int x, int y )

/* prints absolute time in window
 *
 * parameters of routine
 * Widget     w;         input; Widget ID of window where to write time string
 * int        wdwno;     input; number of window which caused MotionNotify
 * int        x, y;      input; cursor position
 */
{
	/* local variables */
	static char oldstr[BC_LINELTH+1]; /* old text */
	float    reltime;                 /* relative time */
	int      trcno;                   /* trace number */
	BOOLEAN  ok;                      /* return flag */
	char     timestr[BC_LINELTH+1];   /* time string */
	TIME     trctime;                 /* start time of trace */
	float    torig;                   /* time offset */
	STATUS   locstat;                 /* local status */
	int      slen;                    /* string length */

	/* executable code */

	if  (mgv_di.trcno == 0)  return;

	if  (wdwno == MGC_WDW_SINGLE)  {
		mg_get_time_and_trace_single( x, y, &reltime, &trcno, &ok );
	} else {
		mg_get_time_and_trace( x, y, &reltime, &trcno, &ok );
	} /*endif*/
	if  (ok)  {
		locstat = BC_NOERROR;
		db_gett( mgv_ti[trcno-1].trc, ET_START, &trctime, &locstat );
		if  (locstat == BC_NOERROR)  {
			tc_aadd( &trctime, reltime, &trctime );
			torig = 0.0;
			torig = db_getr( mgv_ti[trcno-1].trc, ER_TORIG, &locstat );
			tc_aadd( &trctime, -torig, &trctime );
			tc_a2t( &trctime, timestr, &locstat );
			if  (locstat != BC_NOERROR)  printf("*SHM: mg_print_time\n");
		} else {
			strncpy( timestr, oldstr, 11 );
			timestr[11] = '\0';
			strcat( timestr, NO_TIME );
		} /*endif*/
	} else {
		strncpy( timestr, oldstr, 11 );
		timestr[11] = '\0';
		strcat( timestr, NO_TIME );
	} /*endif*/

	/* append time span info in min */
	slen = strlen( timestr );
	sprintf( timestr+slen, "  >%4.2f<", mgv_di.totslth/60.0 );

	pix_DrawString( XtDisplay(w), XtWindow(w), mgv_clear_gc,
		TEXT_X_TIME, mgv_wdwsetup.margin_t+TEXT_Y_TIME,
		oldstr, (int)strlen(oldstr) );
	pix_DrawString( XtDisplay(w), XtWindow(w), mgv_over_gc,
		TEXT_X_TIME, mgv_wdwsetup.margin_t+TEXT_Y_TIME,
		timestr, (int)strlen(timestr) );
	strcpy( oldstr, timestr );

} /* end of mg_print_time */



/*----------------------------------------------------------------------------*/



void mg_current_time_window( char stime[], float *width )

/* returns current time window of main display (using first trace of display)
 *
 * parameters of routine
 * char       stime[];     output; absolute start time of window
 * float      *width;      output; width of display in s
 */
{
	/* local variables */
	int      trcno;                   /* trace number */
	BOOLEAN  ok;                      /* return flag */
	char     timestr[BC_LINELTH+1];   /* time string */
	TIME     trctime;                 /* start time of trace */
	float    torig;                   /* time offset */
	STATUS   locstat;                 /* local status */
	int      slen;                    /* string length */

	/* executable code */

	if  (mgv_di.trcno == 0)  {
		strcpy( stime, "1-Jan-1970_00:00" );
		*width = 0.0;
		return;
	} /*endif*/

	locstat = BC_NOERROR;
	db_gett( mgv_ti[0].trc, ET_START, &trctime, &locstat );
	if  (locstat == BC_NOERROR)  {
		torig = db_getr( mgv_ti[0].trc, ER_TORIG, &locstat );
		tc_aadd( &trctime, -torig, &trctime );
		tc_a2t( &trctime, stime, &locstat );
		if  (locstat != BC_NOERROR)  printf("*SHM: mg_current_time_window\n");
	} else {
		strcpy( stime, "1-Jan-1970_00:00" );
		*width = 0.0;
		return;
	} /*endif*/

	*width = (float)(mgv_di.wdweff_w)/mgv_di.pixpersec;

} /* end of mg_current_time_window */



/*----------------------------------------------------------------------------*/


#define BUSY_STRING "Computing..."


void mg_print_busy( Widget w, BOOLEAN busy )

/* prints busy message on window
 *
 * parameters of routine
 * Widget     w;         input; widget of DrawingArea
 * BOOLEAN    busy;      input; busy flag on/off
 */
{
	/* executable code */

	if  (busy)  {
		pix_DrawString( XtDisplay(w), XtWindow(w), mgv_over_gc,
			TEXT_X_BUSY, mgv_wdwsetup.margin_t+TEXT_Y_BUSY,
			BUSY_STRING, (int)strlen(BUSY_STRING) );
	} else {
		pix_DrawString( XtDisplay(w), XtWindow(w), mgv_clear_gc,
			TEXT_X_BUSY, mgv_wdwsetup.margin_t+TEXT_Y_BUSY,
			BUSY_STRING, (int)strlen(BUSY_STRING) );
	} /*endif*/
	XFlush( XtDisplay(w) );

} /* end of mg_print_busy */



/*----------------------------------------------------------------------------*/



void mg_print_status( Widget w, char text[], BOOLEAN on_off )

/* prints/clears status message on window
 *
 * parameters of routine
 * Widget     w;         input; widget of DrawingArea
 * char       text[];    input; text to print
 * BOOLEAN    on_off;    input; print/clear
 */
{
	/* executable code */

	if  (on_off)  {
		pix_DrawString( XtDisplay(w), XtWindow(w), mgv_over_gc,
			TEXT_X_STATUS, mgv_wdwsetup.margin_t+TEXT_Y_STATUS,
			text, (int)strlen(text) );
	} else {
		pix_DrawString( XtDisplay(w), XtWindow(w), mgv_clear_gc,
			TEXT_X_STATUS, mgv_wdwsetup.margin_t+TEXT_Y_STATUS,
			text, (int)strlen(text) );
	} /*endif*/
	XFlush( XtDisplay(w) );

} /* end of mg_print_status */



/*----------------------------------------------------------------------------*/




void mg_print_filter( Widget w, char filter[] )

/* prints busy message on window
 *
 * parameters of routine
 * Widget     w;         input; widget of DrawingArea
 * char       filter[];  input; name of filter
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];      /* scratch string */
	static char oldstr[BC_LINELTH+1];  /* last name printed */

	/* executable code */

	if  (strncmp(filter,"S+",2) == 0)  filter += 2;

	if  (strlen(filter) > BC_LINELTH-10)  {
		strcpy( str, "Filter: name too long" );
	} else if  (*filter == '\0')  {
		strcpy( str, "Filter: None" );
		*mgv_filter = '\0';
	} else {
		sprintf( str, "Filter: %s", filter );
		strcpy( mgv_filter, filter );
	} /*endif*/

	pix_DrawString( XtDisplay(w), XtWindow(w), mgv_clear_gc,
		TEXT_X_FILTER, mgv_wdwsetup.margin_t+TEXT_Y_FILTER,
		oldstr, (int)strlen(oldstr) );
	pix_DrawString( XtDisplay(w), XtWindow(w), mgv_over_gc,
		TEXT_X_FILTER, mgv_wdwsetup.margin_t+TEXT_Y_FILTER,
		str, (int)strlen(str) );
	strcpy( oldstr, str );
	XFlush( XtDisplay(w) );

} /* end of mg_print_filter */



/*----------------------------------------------------------------------------*/




void mg_print_lastcmd( Widget w, char lastcmd[] )

/* prints last command in window
 *
 * parameters of routine
 * Widget     w;          input; widget of DrawingArea
 * char       lastcmd[];  input; name of filter
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];      /* scratch string */
	static char oldstr[BC_LINELTH+1];  /* last name printed */

	/* executable code */

	if  (strlen(lastcmd) > BC_LINELTH-10)  {
		strcpy( str, "LastCmd: name too long" );
	} else if  (*lastcmd == '\0' || *lastcmd == ' ')  {
		strcpy( str, "LastCmd: Empty" );
		*mgv_filter = '\0';
	} else {
		sprintf( str, "LastCmd: %s", lastcmd );
	} /*endif*/

	pix_DrawString( XtDisplay(w), XtWindow(w), mgv_clear_gc,
		TEXT_X_LASTCMD, mgv_wdwsetup.margin_t+TEXT_Y_LASTCMD,
		oldstr, (int)strlen(oldstr) );
	pix_DrawString( XtDisplay(w), XtWindow(w), mgv_over_gc,
		TEXT_X_LASTCMD, mgv_wdwsetup.margin_t+TEXT_Y_LASTCMD,
		str, (int)strlen(str) );
	strcpy( oldstr, str );
	XFlush( XtDisplay(w) );

} /* end of mg_print_lastcmd */



/*----------------------------------------------------------------------------*/




void mg_print_sortinfo( Widget w, char sortinfo[] )

/* prints sort info in window
 *
 * parameters of routine
 * Widget     w;          input; widget of DrawingArea
 * char       sortinfo[]; input; sort info
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];      /* scratch string */
	static char oldstr[BC_LINELTH+1];  /* last name printed */

	/* executable code */

	if  (strlen(sortinfo) > BC_LINELTH-10)  {
		strcpy( str, "sort:?" );
	} else {
		strcpy( str, sortinfo );
	} /*endif*/

	if  (*oldstr != '\0')
		pix_DrawString( XtDisplay(w), XtWindow(w), mgv_clear_gc,
			TEXT_X_SORT, mgv_wdwsetup.margin_t+TEXT_Y_SORT,
			oldstr, (int)strlen(oldstr) );
	if  (*str != '\0')
		pix_DrawString( XtDisplay(w), XtWindow(w), mgv_over_gc,
			TEXT_X_SORT, mgv_wdwsetup.margin_t+TEXT_Y_SORT,
			str, (int)strlen(str) );
	strcpy( oldstr, str );
	XFlush( XtDisplay(w) );

} /* end of mg_print_sortinfo */



/*----------------------------------------------------------------------------*/



void mg_selected_wave( float **wavptr, int *lth, char stime[], float *dt,
	char station[], char chan[], char *comp )

/* Returns pointer to selected waveform or NULL if nothing selected.
 * Copies the waveform, so the data at '*wavptr' must not be freed after use.
 *
 * parameters of routine
 * float      **wavptr;    output; pointer to waveform or NULL
 * int        *lth;        output; length of trace in samples
 * char       stime[];     output; start time of waveform
 * float      *dt;         output; sample distance of trace
 * char       station[];   output; station name (min length cBcShortStrLth)
 * char       chan[];      output; channel name (min length 3)
 * char       *comp;       output; component (min length 1)
 */
{
	/* local variables */
	int      y;               /* y-position of trace */
	int      trcno;           /* trace number */
	float    t_start, t_end;  /* start and end time of trace (in sec) */
	TSyBoolean ok;            /* return status */
	float    *datptr;         /* pointer to sample data */
	int      s_first;         /* first sample wanted */
	TSyStatus locstat;        /* local status variable */
	int      i;               /* counter */
	TIME     abstime;         /* start time */

	/* executable code */

	/* initialize */
	*wavptr = NULL;
	*lth = 0;
	*stime = '\0';
	ok = TRUE;
	locstat = cBcNoError;

	if  (mgv_di.trcno == 0)  {
		printf( "*SHM: mg_selected_wave: no trace available\n" );
		return;
	} /*endif*/
	if  (!mgv_drag.on)  {
		printf( "*SHM: mg_selected_wave: no waveform available\n" );
		return;
	} /*endif*/
	y = (mgv_drag.y1 + mgv_drag.y2) / 2;
	mg_get_time_and_trace( mgv_drag.x1, y, &t_start, &trcno, &ok );
	if  (!ok)  {printf("*SHM: mg_selected_wave: error 1\n" ); return;}
	mg_get_time_and_trace( mgv_drag.x2, y, &t_end, NULL, &ok );
	if  (!ok)  {printf("*SHM: mg_selected_wave: error 2\n" ); return;}
	trcno--;
	datptr = (float *)db_getp( mgv_ti[trcno].trc, EP_DATA, NULL );
	s_first = dm_getsample( mgv_ti[trcno].trc, t_start, TRUE );
	*lth = dm_getsample( mgv_ti[trcno].trc, t_end, TRUE );
	*lth -= s_first + 1;
	if  (*lth <= 1)  {
		printf( "*SHM: mg_selected_wave: waveform too short\n" );
		return;
	} /*endif*/
	datptr += s_first;

	/* *wavptr = datptr; */

	/* this makes a copy of the data */
	*wavptr = (float *)sy_allocmem( *lth, (int)sizeof(float), &locstat );
	if  (Severe(&locstat))  {
		*wavptr = NULL;
		printf( "*SHM: mg_selected_wave: waveform allocation error\n" );
		return;
	} /*endif*/
	for (i=0; i<(*lth); i++)
		(*wavptr)[i] = datptr[i];

	*dt = db_getr( mgv_ti[trcno].trc, ER_DELTA, NULL );
	*comp = db_getc( mgv_ti[trcno].trc, EC_COMP, NULL );
	chan[0] = db_getc( mgv_ti[trcno].trc, EC_CHAN1, NULL );
	chan[1] = db_getc( mgv_ti[trcno].trc, EC_CHAN2, NULL );
	chan[2] = '\0';
	db_gets( mgv_ti[trcno].trc, ES_STATION, cBcShortStrLth, station, &locstat );

	db_gett( mgv_ti[trcno].trc, ET_START, &abstime, &locstat );
	if  (SySevere(&locstat))  return;
	tc_aadd( &abstime, t_start, &abstime );
	tc_a2t( &abstime, stime, &locstat );

} /* end of mg_selected_wave */



/*----------------------------------------------------------------------------*/



void mg_set_cursor_widgets( Widget w[], int num )

/* Sets widgets of drawing areas where cursor form may be changed
 *
 * parameters of routine
 * Widget        w[];       input; drawing area widgets
 * int           num;       input; length of above array (<= MAXCRSRDSP)
 */
{
	/* local variables */
	int      i;               /* counter */

	/* executable code */

	if  (num > MAXCRSRDSP)  {
		printf( "*SHM: number of cursor windows exceeded.  Truncated.\n" );
		num = MAXCRSRDSP;
	} /*endif*/

	mgv_crsrdspnum = num;
	for  (i=0; i<num; i++)
		mgv_crsrdsp[i] = w[i];

} /* end of mg_set_cursor_widgets */



/*----------------------------------------------------------------------------*/



void mg_add_cursor_widget( Widget w )

/* Adds widget to list of drawing areas where cursor form may be changed
 *
 * parameters of routine
 * Widget        w;       input; drawing area widgets
 * int           num;       input; length of above array (<= MAXCRSRDSP)
 */
{
	/* local variables */

	/* executable code */

	if  (mgv_crsrdspnum >= MAXCRSRDSP)  {
		printf( "*SHM: number of cursor windows exceeded.  Ignored.\n" );
		return;
	} /*endif*/

	mgv_crsrdsp[mgv_crsrdspnum++] = w;

} /* end of mg_add_cursor_widget */



/*----------------------------------------------------------------------------*/



void mg_set_cursor( int cursor )

/* Sets cursor form in all windows passed in mg_set_cursor_widgets.
 *
 * parameters of routine
 * Display    *dsp;        input; pointer to display
 * Window     w;           input; window of cursor
 * int        cursor;      input; corsor ID
 */
{
	/* local variables */
	static int crsr_defined=FALSE;  /* is a cursor defined ? */
	int      i;          /* counter */

	/* executable code */

	if  (cursor < 0 || cursor > MGC_XCRSR_MAX)  {
		printf( "*SHM: mg_set_cursor: bug in program\n" );
		return;
	} /*endif*/

	if  (cursor == MGC_XCRSR_NORMAL)  {
		/*XDefineCursor( dsp, w, mgv_crsr[cursor] );*/
		if  (crsr_defined)  {
			for  (i=0; i<mgv_crsrdspnum; i++)
				if  (XtIsManaged(mgv_crsrdsp[i]))
					XUndefineCursor( XtDisplay(mgv_crsrdsp[i]),
						XtWindow(mgv_crsrdsp[i]) );
			crsr_defined = FALSE;
		} else {
			printf( "*SHM: cursor already undefined\n" );
		} /*endif*/
	} else {
		if  (crsr_defined)  {
			printf( "*SHM: cursor already defined, undefine first\n" );
			for  (i=0; i<mgv_crsrdspnum; i++)
				if  (XtIsManaged(mgv_crsrdsp[i]))
					XUndefineCursor( XtDisplay(mgv_crsrdsp[i]),
						XtWindow(mgv_crsrdsp[i]) );
		} /*endif*/
		for  (i=0; i<mgv_crsrdspnum; i++)
			if  (XtIsManaged(mgv_crsrdsp[i]))
				XDefineCursor( XtDisplay(mgv_crsrdsp[i]), XtWindow(mgv_crsrdsp[i]),
					mgv_crsr[cursor] );
		crsr_defined = TRUE;
	} /*endif*/

} /* end of mg_set_cursor */



/*----------------------------------------------------------------------------*/



void mg_rubber_line( Widget w, int wdwno, int mode, int x, int y )

/* Draws rubber line
 *
 * parameters of routine
 * Widget     w;             input; widget ID of drawing area
 * int        wdwno;         input; window number
 * int        mode;          input; mode (START,CONT,END)
 * int        x, y;          input; mouse position
 */
{
	/* local variables */
	static int  init_wdwno;       /* window number of first call */
	static BOOLEAN drawn;         /* line is drawn */
	static int  init_x, init_y;   /* first position */
	static int  old_x, old_y;     /* last position */
	int         new_x, new_y;     /* current position */
	float       time;             /* selected time (not used) */
	int         trcno;            /* number of selected trace */
	BOOLEAN     ok;               /* selection ok ? */

	/* executable code */

	switch  (mode)  {
	case MGC_RUBBER_START:
		if  (wdwno == MGC_WDW_SINGLE)  {
			mg_get_time_and_trace_single( x, y, &time, &trcno, &ok );
			init_y = old_y = mgv_wdwsetup.margin_t/*b*/ + mgv_sdi.unitheight / 2.0;
		} else {
			mg_get_time_and_trace( x, y, &time, &trcno, &ok );
			init_y = old_y = mgv_ti[trcno-1].xenv->orig.y;
		} /*endif*/
		if  (!ok)  {
			printf( "*SHM: mg_rubber_line: couldn't get trace\n" );
			return;
		} /*endif*/
		init_x = old_x = x;
		init_wdwno = wdwno;
		drawn = FALSE;
		break;
	case MGC_RUBBER_CONT_L:
		if  (wdwno != init_wdwno)  return;
		if  (drawn)  {
			pix_DrawLine( XtDisplay(w), XtWindow(w), mgv_mark_gc,
				init_x, init_y, old_x, old_y );
		} /*endif*/
		new_x = x;
		new_y = old_y;
		if  (new_x < init_x)  {
			pix_DrawLine( XtDisplay(w), XtWindow(w), mgv_mark_gc,
				init_x, init_y, new_x, new_y );
			drawn = TRUE;
			old_x = new_x;
			old_y = new_y;
		} else {
			drawn = FALSE;
		} /*endif*/
		break;
	case MGC_RUBBER_CONT_R:
		if  (wdwno != init_wdwno)  return;
		if  (drawn)  {
			pix_DrawLine( XtDisplay(w), XtWindow(w), mgv_mark_gc,
				init_x, init_y, old_x, old_y );
		} /*endif*/
		new_x = x;
		new_y = old_y;
		if  (new_x > init_x)  {
			pix_DrawLine( XtDisplay(w), XtWindow(w), mgv_mark_gc,
				init_x, init_y, new_x, new_y );
			drawn = TRUE;
			old_x = new_x;
			old_y = new_y;
		} else {
			drawn = FALSE;
		} /*endif*/
		break;
	case MGC_RUBBER_END:
		if  (wdwno != init_wdwno)  return;
		if  (drawn)  {
			pix_DrawLine( XtDisplay(w), XtWindow(w), mgv_mark_gc,
				init_x, init_y, old_x, old_y );
		} /*endif*/
		drawn = FALSE;
		break;
	default:
		printf( "*SHM: illegal rubber_line mode %d\n", mode );
		return;
	} /*endswitch*/

	XFlush( XtDisplay(w) );

} /* end of mg_rubber_line */



/*----------------------------------------------------------------------------*/



static void mgh_set_bg_color( Display *dsp, GC gc, unsigned long bg )

/* sets background color
 *
 * parameters of routine
 * same as in XSetBackground;
 */
{
	/* local variables */
	XGCValues  val;      /* values of GC */

	/* executable code */

	XSetBackground( dsp, gc, bg );

#ifdef XXX
	if  (mgv_reverse_xors)  {
		XGetGCValues( dsp, gc, GCFunction, &val );
		if  (val.function == GXxor)  {
			XSetForeground( dsp, gc, bg );
		} else {
			XSetBackground( dsp, gc, bg );
		} /*endif*/
	} else {
		XSetBackground( dsp, gc, bg );
	} /*endif*/
#endif

} /* end of mgh_set_bg_color */



/*----------------------------------------------------------------------------*/



static void mgh_set_fg_color( Display *dsp, GC gc, unsigned long fg )

/* sets foreground color
 *
 * parameters of routine
 * same as in XSetForeground;
 */
{
	/* local variables */
	XGCValues  val;      /* values of GC */

	/* executable code */

	if  (mgv_reverse_xors)  {
		XGetGCValues( dsp, gc, GCFunction, &val );
		if  (val.function == GXxor)  {
			XSetForeground( dsp, gc, ~fg );
		} else {
			XSetForeground( dsp, gc, fg );
		} /*endif*/
	} else {
		XSetForeground( dsp, gc, fg );
	} /*endif*/

#ifdef XXX
	if  (mgv_reverse_xors)  {
		XGetGCValues( dsp, gc, GCFunction, &val );
		if  (val.function == GXxor)  {
			XSetBackground( dsp, gc, fg );
		} else {
			XSetForeground( dsp, gc, fg );
		} /*endif*/
	} else {
		XSetForeground( dsp, gc, fg );
	} /*endif*/
#endif

} /* end of mgh_set_fg_color */



/*----------------------------------------------------------------------------*/



void mg_disable_redraw( BOOLEAN on_off )

/* Disables/Enables redraw with mg_tracedisplay
 *
 * parameters of routine
 * BOOLEAN    on_off;     input; switch value
 */
{
	/* executable code */

	mgv_disable_rd = on_off;

} /* end of mg_disable_redraw */



/*----------------------------------------------------------------------------*/



void mg_find_3_traces( int tmain, int *ta, int *tb )

/* Finds associated traces to tmain (traces of same station and different
 * components.  Returns -1 if no traces found
 *
 * parameters of routine
 * int        tmain;        input; number of main trace
 * int        *ta, *tb;     output; associated traces
 */
{
	/* local variables */
	void     *pmain, *pa, *pb;            /* trace pointers */
	TSyStatus locstat;                    /* local status */
	char     mstation[cBcShortStrLth+1];  /* station name of main trace */
	char     astation[cBcShortStrLth+1];  /* station name of trace a */
	int      idx[4];                      /* 4 traces are searched */
	int      i;                           /* counter */

	/* executable code */

	*ta = *tb = -1;

	pmain = mg_trcptr( tmain );
	if  (pmain == NULL)  {
		printf( "*SHM: no trcptr for main trace (%d) found\n", tmain );
		return;
	} /*endif*/
	locstat = cBcNoError;
	db_gets( pmain, ES_STATION, cBcShortStrLth, mstation, &locstat );
	if  (*mstation == '\0')  return;

	/* set search list */
	for  (i=0; i<4; i++)  idx[i] = 0;
	i = 0;
	if  (tmain > 2)  idx[i++] = tmain - 2;
	if  (tmain > 1)  idx[i++] = tmain - 1;
	if  (tmain < mgv_di.trcno)  idx[i++] = tmain + 1;
	if  (tmain < mgv_di.trcno-1)  idx[i++] = tmain + 2;

	/* look all traces in search list */
	for  (i=0; i<4; i++)  {
		if  (idx[i] == 0)  continue;
		pa = mg_trcptr( idx[i] );
		if  (pa == NULL)  {
			printf( "*SHM: no trcptr for trace %d found.\n", idx[i] );
			continue;
		} /*endif*/
		db_gets( pa, ES_STATION, cBcShortStrLth, astation, &locstat );
		if  (strcmp(astation,mstation) == 0)  {
			if  (*ta == -1)  {
				*ta = idx[i];
			} else {
				*tb = idx[i];
				return;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

} /* end of mg_find_3_traces */



/*----------------------------------------------------------------------------*/



void mg_make_alert_trace( Widget w, void *trcptr, TSyStatus *status )

/* makes alert trace
 *
 * parameters of routine
 * Widget     w;        input; widget of drawing area
 * void       *trcptr;  input; pointer to trace
 * TSyStatus  *status;  output; return status
 */
{
	/* local variables */
	MGT_TRCXENV *xenv;                     /* pointer to trace environment */

	/* executable code */

	xenv = (MGT_TRCXENV *)db_getp( trcptr, EP_USR1, NULL );
	if  (xenv == NULL)  xenv = mg_create_trcxenv( w, status );
	if  (xenv != NULL)  {
		db_setp( trcptr, EP_USR1, xenv, status );
		xenv->gc = mgv_trcalert_gc;
	} /*endif*/

} /* end of mg_make_alert_trace */



/*----------------------------------------------------------------------------*/



void mg_get_last_drag_box( TSyBoolean *valid,
	int *x1, int *y1, int *x2, int *y2 )

/* returns position and size of last dragbox
 *
 * parameters of routine
 * TSyBoolean    *valid;    output; drag box visible? if FALSE x,y's are void
 * int           *x1, *y1;  output; lower left corner
 * int           *x2, *y2;  output; upper right corner
 */
{
	/* executable code */

	*valid = mgv_drag.on;
	*x1 = mgv_drag.x1;
	*y1 = mgv_drag.y1;
	*x2 = mgv_drag.x2;
	*y2 = mgv_drag.y2;

} /* end of mg_get_last_drag_box */



/*----------------------------------------------------------------------------*/



static void mgh_label_rounding( float accuracy, TIME *atime )

/* makes 'nice' numbers on time axis
 *
 * parameters of routine
 * float      accuracy; input; maximum time to modify atime to get nice number
 * TIME       *atime;   modify; time to be rounded
 */
{
	/* local variables */
	TSyStatus locstat;             /* local status */
	NTIME    ntime;                /* numeric time */
	int      iacc;                 /* accuracy in ms */
	char     stime[cBcTimeLth+1];  /* time string */
	int      msecs;                /* milliseconds of time to be rounded */
	float    corrtime;             /* time correction to get nice number */

	/* executable code */

	locstat = cBcNoError;

	if  (GpGetInt(cGpI_debug_level) > 8)  {
		tc_a2t( atime, stime, &locstat );
		printf( "SHM-dbg9: round %s acc %f -> ", stime, accuracy );
	} /*endif*/

	tc_a2n( atime, &ntime, &locstat );
	if  (SySevere(&locstat))  return;
	iacc = Nint( accuracy*1000.0 );
	msecs = ntime.ms;
	if  (msecs > 500)  msecs = ntime.ms - 1000;

	if  (Abs(msecs) < iacc)  {
		corrtime = (float)msecs/1000.0;
	} else if  (iacc >= 100)  {
		corrtime = (float)(msecs % 100)/1000.0;
	} else if  (iacc >= 10)  {
		corrtime = (float)(msecs % 10)/1000.0;
	} else {
		corrtime = 0.0;
	} /*endif*/

	tc_aadd( atime, -corrtime, atime );

	if  (GpGetInt(cGpI_debug_level) > 8)  {
		tc_a2t( atime, stime, &locstat );
		printf( "%s\n", stime );
	} /*endif*/

} /* end of mgh_label_rounding */



/*----------------------------------------------------------------------------*/

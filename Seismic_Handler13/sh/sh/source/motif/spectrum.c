
/* file spectrum.c
 *      ==========
 *
 * version 14, 18-Oct-2006
 *
 * Spectrum window
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



#include <stdio.h>
#include <string.h>
#include <math.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "tcusrdef.h"
#include <Xm/Xm.h>
#include <X11/cursorfont.h>
#include "../numres.h"
#include "pixmaps.h"
#include "motifgraph.h"
#include "cbutil.h"
#include "spectrum.h"
#include "shm_widgets.h"
#include "globalparams.h"


/* constants */
#define LEFT_MARGIN 100
#define RIGHT_MARGIN 40
#define TOP_MARGIN 40
#define BOTTOM_MARGIN 100
#define TICKLTH 10


/* types */

typedef struct {
	float     frqlo;      /* frequency, lower bound */
	float     frqhi;      /* frequency, upper bound */
	float     amplo;      /* amplitude, lower bound */
	float     amphi;      /* amplitude, upper bound */
} TSpcBounds;

typedef struct {
	int       wx;         /* window x-coordinate */
	int       wy;         /* window y-coordinate */
	int       ww;         /* window width */
	int       wh;         /* window height */
} TSpcWindow;

typedef struct {
	float    xo;          /* x offset */
	float    yo;          /* y offset */
	float    xf;          /* x scaling */
	float    yf;          /* y scaling */
} TSpcScale;

typedef struct _spc_trace {
	char     station[cBcShortStrLth+1];  /* station name */
	char     chan[3];                    /* channel name (BH, LH, ...) */
	char     comp;                       /* component */
	char     stime[cBcTimeLth+1];        /* time of first sample */
	float    dt;                         /* sample distance in sec */
	float    *datptr;                    /* pointer to data */
	int      datlth;                     /* length of data array */
	float    df;                         /* sample distance in Hz */
	float    *fftptr;                    /* pointer to FFT data */
	int      fftlth;                     /* length of FFT data array */
	GC       gc;                         /* plot style for trace */
	int      text_height;                /* text height in pixel */
	struct _spc_trace *next;             /* pointer to next structure */
} TSpcTrace;

typedef struct {
	TSyBoolean draw_grid;                /* show grid */
	int      comp_method;                /* computation method */
	TSyBoolean trunc_trc;                /* truncate trace instead of pad zeros*/
	/* parameters for power spectrum */
	int      ps_numwdw;                  /* number of windows */
	int      ps_overlap;                 /* overlap windows */
} TSpcParams;

typedef enum {
	cSpcFL_idle,       /* nothing to do */
	cSpcFL_wait_left,  /* wait for left border click */
	cSpcFL_wait_right  /* wait for right border click */
} TSpcFitLineState;

typedef enum {
	cSpcCH_init,       /* enable crosshair cursor */
	cSpcCH_newpos,     /* show new position */
	cSpcCH_finish      /* disable crosshair */
} TSpcCrosshairMode;


/* global variables */
static Widget     spcv_root_wdw; /* root window of drawing area */
static Widget     spcv_draw_wdw; /* drawing area */
static TSpcBounds spcv_bounds={1.0e-3,1.0e2,1.0e-5,1.0e5}; /* drawing bounds */
static TSpcWindow spcv_twdw;     /* total window size */
static TSpcWindow spcv_ewdw;     /* effective window size */
static TSpcScale  spcv_scale;    /* scaling */
static TSpcTrace  *spcv_roottrc=NULL;  /* pointer to root trace */
static TSpcParams spcv_par;      /* computation parameters */
static GC         spcv_gc;       /* drawing GC */
static GC         spcv_xor_gc;   /* XOR GC */
static GC         spcv_line_gc;  /* GC for drawing fitted lines */
static GC         spcv_grid_gc;  /* GC for grid */
static Colormap   spcv_cmap;     /* color map */
static TSyBoolean spcv_is_init=FALSE;  /* module is initialized */
static Cursor     spcv_crsr_left;  /* left cursor */
static Cursor     spcv_crsr_right; /* right cursor */
static BOOLEAN    spcv_crosshair=FALSE;    /* show crosshair cursor */
static TSpcFitLineState spcv_fitline_state=cSpcFL_idle;
											/* status of fit line clicks */



/* prototypes of local routines */
static void spc_draw_axes( void );
static void spc_transform( float x, float y, int *px, int *py );
static void spc_plot_trace( GC gc, float *dat, int lth, float d,
	TSyStatus *status );
static void spc_make_scaling( void );
static void spc_redraw( TSyStatus *status );
static void spc_write_label( GC gc, int texth, char label[] );
static void spc_compute_spectrum( TSpcTrace *trc, TSyStatus *status );
static void spc_auto_scaling( void );
static void spc_print_position( Widget w, int x, int y );
static void spc_compute_line( int f1, int f2, float *b, float *m,
	TSyStatus *status );
static void spc_display_linefit( float b, float m );
static void spc_crosshair_cursor( TSpcCrosshairMode mode, int x, int y );
static void spc_print_info( Widget w, char info[] );
void spc_compute_spectrogram( TSpcTrace *trc, char fname[], TSyStatus *status );
static float spc_taper( int idx, int maxidx );





/*----------------------------------------------------------------------------*/



void spc_initialize( Widget parent, Widget spcwdw, STATUS *status )

/* initializes spectrum window
 *
 * parameters of routine
 * Widget     parent;      input; widget ID of root window
 * Widget     spcwdw;      input; Widget ID of spectrum window
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	XColor   screen_color;         /* color */
	int      xstatus;              /* return status for color allocation */

	/* executable code */

	spcv_root_wdw = parent;
	spcv_draw_wdw = spcwdw;

	spc_print_position( spcwdw, -1, -1 );

	if  (!XtIsManaged(parent))
		XtManageChild( parent );

	if  (spcv_is_init)  return;

	/* get color map */
	spcv_cmap = XDefaultColormapOfScreen( XtScreen(spcwdw) );

	/* GC for axes and labels */
	spcv_gc = XCreateGC( XtDisplay(spcwdw), XtWindow(spcwdw), 0 , NULL );
	XSetForeground( XtDisplay(spcwdw), spcv_gc, pix_colour(PIXC_COL_FOREGROUND));
	XSetBackground( XtDisplay(spcwdw), spcv_gc, pix_colour(PIXC_COL_BACKGROUND));

	/* XOR GC for displaying position */
	spcv_xor_gc = XCreateGC( XtDisplay(spcwdw), XtWindow(spcwdw), 0 , NULL );
	XSetFunction( XtDisplay(spcwdw), spcv_xor_gc, GXxor );
	if  (GpGetBoolean(cGpB_reverse_xors))  {
		XSetForeground( XtDisplay(spcwdw), spcv_xor_gc,
			XWhitePixelOfScreen(XtScreen(spcwdw)) );
	} else {
		XSetForeground( XtDisplay(spcwdw), spcv_xor_gc,
			XBlackPixelOfScreen(XtScreen(spcwdw)) );
	} /*endif*/
	XSetBackground( XtDisplay(spcwdw), spcv_xor_gc,
		pix_colour(PIXC_COL_BACKGROUND) );

	/* GC for drawing fitted lines */
	spcv_line_gc = XCreateGC( XtDisplay(spcwdw), XtWindow(spcwdw), 0 , NULL );
	if  (GpGetBoolean(cGpB_reverse_xors))  {
		XSetForeground( XtDisplay(spcwdw), spcv_line_gc,
			XWhitePixelOfScreen(XtScreen(spcwdw)) );
	} else {
		XSetForeground( XtDisplay(spcwdw), spcv_line_gc,
			XBlackPixelOfScreen(XtScreen(spcwdw)) );
	} /*endif*/
	XSetBackground( XtDisplay(spcwdw), spcv_line_gc,
		XWhitePixelOfScreen(XtScreen(spcwdw)) );
	XSetFunction( XtDisplay(spcwdw), spcv_line_gc, GXxor );

	/* GC for grid lines */
	spcv_grid_gc = XCreateGC( XtDisplay(spcwdw), XtWindow(spcwdw), 0 , NULL );
	XSetBackground( XtDisplay(spcwdw), spcv_gc,
			XWhitePixelOfScreen(XtScreen(spcwdw)) );
	if  (((GpGetFloat(cGpF_colour_bg_red)+GpGetFloat(cGpF_colour_bg_green)
		+GpGetFloat(cGpF_colour_bg_blue)) / 3.0) > 0.5)  {
		/* background is bright, take light grey */
		screen_color.red = screen_color.green = screen_color.blue =
			Nint( 0.9 * 65535.0 );
	} else {
		/* background is dark, take dark grey */
		screen_color.red = screen_color.green = screen_color.blue =
			Nint( 0.2 * 65535.0 );
	} /*endif*/
	screen_color.flags = DoRed | DoGreen | DoBlue;
	xstatus = XAllocColor( XtDisplay(spcv_draw_wdw), spcv_cmap, &screen_color );
	if  (xstatus == 0)  {
		XSetForeground( XtDisplay(spcv_draw_wdw), spcv_grid_gc,
			XBlackPixelOfScreen(XtScreen(spcv_draw_wdw)) );
	} else {
		XSetForeground( XtDisplay(spcv_draw_wdw), spcv_grid_gc,
			screen_color.pixel );
	} /*endif*/

	spcv_crsr_left = XCreateFontCursor( XtDisplay(spcwdw), XC_left_side );
	spcv_crsr_right = XCreateFontCursor( XtDisplay(spcwdw), XC_right_side );

	spcv_par.draw_grid = TRUE;
	spcv_par.comp_method = cSpcModeFft;
	spcv_par.ps_numwdw = 1;
	spcv_par.ps_overlap = FALSE;
	spcv_par.trunc_trc = FALSE;

	spcv_crosshair = FALSE;

	spc_make_scaling();

	pix_create_window_buffer( XtDisplay(spcwdw), XtWindow(spcwdw),
		TRUE, status );

	spc_write_label( (GC)0, 0, "--RESET--" );

	spc_draw_axes();

	spcv_is_init = TRUE;

} /* end of spc_initialize */



/*----------------------------------------------------------------------------*/



void spc_insert_spectrum( Widget spcwdw, TSyStatus *status )

/* Plots new spectrum to window
 *
 * parameters of routine
 * Widget     spcwdw;        input; widget ID of spectrum drawing area
 */
{
	/* local variables */
	TSpcTrace *trc;                /* new trace pointer */
	TSpcTrace *prev;               /* previous trace */
	char     stream[cBcLineLth+1]; /* stream string */
	XColor   screen_color;         /* color */
	int      xstatus;              /* return status for color allocation */
	XRectangle rect;               /* clipping rectangle */
	int      trcnum;               /* number of traces */
	static XFontStruct *font;      /* pointer to font structure */
	static float xred=0.9, xgreen=0.1, xblue=0.0;  /* start colors */

	/* executable code */

	/* make new trace pointer */
	trc = sy_allocmem( 1, (int)sizeof(TSpcTrace), status );
	if  (SySevere(status))  return;
	trc->next = NULL;
	trc->datptr = trc->fftptr = NULL;

	/* get trace data */
	mg_selected_wave( &(trc->datptr), &(trc->datlth), trc->stime, &(trc->dt),
		trc->station, trc->chan, &(trc->comp) );
	if  (trc->datptr == NULL)  {
		sy_deallocmem( trc );
		printf( "*SHM: spc_insert_spectrum: no trace data.  Abort.\n" );
		return;
	} /*endif*/

	/* get plot style */
	trc->gc = XCreateGC( XtDisplay(spcv_draw_wdw), XtWindow(spcv_draw_wdw),
		0, NULL );
	screen_color.red = Nint( xred * 65535.0 );
	screen_color.green = Nint( xgreen * 65535.0 );
	screen_color.blue = Nint( xblue * 65535.0 );
	screen_color.flags = DoRed | DoGreen | DoBlue;
	xstatus = XAllocColor( XtDisplay(spcv_draw_wdw), spcv_cmap, &screen_color );
	if  (xstatus == 0)  {
		XSetForeground( XtDisplay(spcv_draw_wdw), trc->gc,
			XBlackPixelOfScreen(XtScreen(spcv_draw_wdw)) );
	} else {
		XSetForeground( XtDisplay(spcv_draw_wdw), trc->gc, screen_color.pixel );
	} /*endif*/
	xred += 0.11;  if  (xred > 1.0)  xred -= 1.0;
	xgreen += 0.28;  if  (xgreen > 1.0)  xgreen -= 1.0;
	xblue += 0.63;  if  (xblue > 1.0)  xblue -= 1.0;
	font = XQueryFont( XtDisplay(spcv_draw_wdw), XGContextFromGC(trc->gc) );
	trc->text_height = font->ascent + font->descent;
	rect.x = spcv_ewdw.wx;
	rect.y = spcv_twdw.wh - spcv_ewdw.wy - spcv_ewdw.wh;
	rect.width = spcv_ewdw.ww;
	rect.height = spcv_ewdw.wh;
	pix_SetClipRectangles( XtDisplay(spcv_draw_wdw), trc->gc, 0, 0, &rect, 1,
		Unsorted );

	spc_print_position( spcwdw, -1, -1 );

	spc_compute_spectrum( trc, status );
	if  (SySevere(status))  return;

	/* append trace to list */
	trcnum = 0;
	if  (spcv_roottrc == NULL)  {
		prev = NULL;
		spcv_roottrc = trc;
	} else {
		trcnum = 1;
		prev = spcv_roottrc;
		while  (prev->next != NULL)  {
			prev = prev->next;
			trcnum++;
		} /*endwhile*/
		prev->next = trc;
	} /*endif*/
	trcnum++;

	/* make autoscaling if only one trace */
	if  (trcnum == 1)
		spc_auto_scaling();

	spc_plot_trace( trc->gc, trc->fftptr, trc->fftlth, trc->df, status );
	if  (SySevere(status))  return;
	sprintf( stream, "%s-%s-%c", trc->station, trc->chan, trc->comp );
	spc_write_label( trc->gc, trc->text_height, stream );

#ifdef XXX
	{
	static int i=0;
	Cursor c;
	i++;
	c = XCreateFontCursor( XtDisplay(spcwdw), i );
	XDefineCursor( XtDisplay(spcwdw), XtWindow(spcwdw), c );
	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: cursor number %d\n", i );
	}
#endif

} /* end of spc_insert_spectrum */



/*----------------------------------------------------------------------------*/



void spc_close_window( void )

/* Closes spectrum window
 *
 * parameters of routine
 * none
 */
{
	/* executable code */
	TSpcTrace     *c, *p;

	spc_draw_axes();
	if  (XtIsManaged(spcv_root_wdw))  XtUnmanageChild(spcv_root_wdw);

	/* free all memory */
	if  (spcv_roottrc == NULL)  return;

	c = spcv_roottrc;
	do  {
		if  (c->datptr != NULL)  sy_deallocmem( c->datptr );
		if  (c->fftptr != NULL)  sy_deallocmem( c->fftptr );
		p = c->next;
		sy_deallocmem( c );
		c = p;
	}  while (c != NULL);
	spcv_roottrc = NULL;

} /* end of spc_close_window */



/*----------------------------------------------------------------------------*/



void spc_change_display( int mode, TSyStatus *status )

/* Changes display
 *
 * parameters of routine
 * int        mode;        input; which change
 */
{
	/* local variables */

	/* executable code */

	if  (!spcv_is_init)  return;

	switch  (mode)  {
	case cSpcUnchanged:
		break;
	case cSpcAutoscale:
		spc_auto_scaling();
		break;
	case cSpcFrqLoUp:
		spcv_bounds.frqlo *= 10.0;
		break;
	case cSpcFrqLoDown:
		spcv_bounds.frqlo /= 10.0;
		break;
	case cSpcFrqHiUp:
		spcv_bounds.frqhi *= 10.0;
		break;
	case cSpcFrqHiDown:
		spcv_bounds.frqhi /= 10.0;
		break;
	case cSpcAmpLoUp:
		spcv_bounds.amplo *= 10.0;
		break;
	case cSpcAmpLoDown:
		spcv_bounds.amplo /= 10.0;
		break;
	case cSpcAmpHiUp:
		spcv_bounds.amphi *= 10.0;
		break;
	case cSpcAmpHiDown:
		spcv_bounds.amphi /= 10.0;
		break;
	case cSpcResized:
		if  (spcv_is_init)
			pix_resize_window_buffer( XtDisplay(spcv_draw_wdw),
				XtWindow(spcv_draw_wdw), status );
		break;
	default:
		printf( "*SHM: program bug (spc_change_display)\n" );
		return;
	} /*end switch*/

	spc_make_scaling();
	spc_draw_axes();
	spc_redraw( status );

} /* end of spc_change_display */



/*----------------------------------------------------------------------------*/



void spc_recompute_spectra( int mode, TSyStatus *status )

/* Recomputes spectra of all traces with new mode.
 *
 * parameters of routine
 * int        mode;        input; computation method
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	TSpcTrace   *trc;       /* trace pointer */

	/* executable code */

	spcv_par.comp_method = mode;
	if  (spcv_roottrc == NULL)  return;

	trc = spcv_roottrc;
	do  {
		spc_compute_spectrum( trc, status );
		if  (SySevere(status))  return;
		trc = trc->next;
	}  while (trc != NULL);

	spc_redraw( status );

} /* end of spc_recompute_spectra */



/*----------------------------------------------------------------------------*/



void spc_get_dialog_values( Widget w[], TSyStatus *status )

/* Gets parameters from dialog box
 *
 * parameters of routine
 * Widget        w[];       input; widget array
 * TSyStatus     *status;   output; return status
 */
{
	/* local variables */
	char        *c;                 /* pointer to char */
	TSpcParams  opar;               /* old parametere values */
	TSyBoolean  redraw, recompute;  /* flags */

	/* executable code */

	opar = spcv_par;

	spcv_par.draw_grid =
		XmToggleButtonGetState( w[k_widget_spcsetup_grid_on] );
	spcv_par.ps_overlap =
		XmToggleButtonGetState( w[k_widget_spcsetup_overlap_on] );
	spcv_par.trunc_trc =
		XmToggleButtonGetState( w[k_widget_spcsetup_trunc_on] );
	if  (XmToggleButtonGetState(w[k_widget_spcsetup_mode_powspc]))  {
		spcv_par.comp_method = cSpcModePowSpc;
	} else if  (XmToggleButtonGetState(w[k_widget_spcsetup_mode_fft]))  {
		spcv_par.comp_method = cSpcModeFft;
	} else {
		spcv_par.comp_method = cSpcModeFft2;
	} /*endif*/
	c = cu_get_string( w[k_widget_spcsetup_numwdw_text] );
	sscanf( c, "%d", &spcv_par.ps_numwdw );

	redraw = recompute = FALSE;
	if  (opar.draw_grid != spcv_par.draw_grid)  redraw = TRUE;
	if  (opar.comp_method != spcv_par.comp_method)  recompute=TRUE;
	if  (opar.trunc_trc != spcv_par.trunc_trc)  recompute=TRUE;
	if  (opar.ps_overlap != spcv_par.ps_overlap)  recompute=TRUE;
	if  (opar.ps_numwdw != spcv_par.ps_numwdw)  recompute=TRUE;

	if  (recompute)  {
		spc_recompute_spectra( spcv_par.comp_method, status );
		return;
	} /*endif*/
	if  (redraw)  spc_redraw( status );

} /* end of spc_get_dialog_values */



/*----------------------------------------------------------------------------*/



void spc_set_dialog_values( Widget w[] )

/* Sets parameters of dialog box
 *
 * parameters of routine
 * Widget        w[];       input; widget array
 */
{
	/* local variables */
	char     str[cBcLineLth+1];           /* scratch string */

	/* executable code */

	XmToggleButtonSetState( w[k_widget_spcsetup_grid_on],
		spcv_par.draw_grid, FALSE );
	XmToggleButtonSetState( w[k_widget_spcsetup_grid_off],
		!spcv_par.draw_grid, FALSE );
	XmToggleButtonSetState( w[k_widget_spcsetup_overlap_on],
		spcv_par.ps_overlap, FALSE );
	XmToggleButtonSetState( w[k_widget_spcsetup_overlap_off],
		!spcv_par.ps_overlap, FALSE );
	XmToggleButtonSetState( w[k_widget_spcsetup_trunc_on],
		spcv_par.trunc_trc, FALSE );
	XmToggleButtonSetState( w[k_widget_spcsetup_trunc_off],
		!spcv_par.trunc_trc, FALSE );
	sprintf( str, "%d", spcv_par.ps_numwdw );
	cu_set_string( w[k_widget_spcsetup_numwdw_text], str );

	if  (spcv_par.comp_method == cSpcModePowSpc)  {
		XmToggleButtonSetState( w[k_widget_spcsetup_mode_powspc], TRUE, FALSE );
		XmToggleButtonSetState( w[k_widget_spcsetup_mode_fft], FALSE, FALSE );
		XmToggleButtonSetState( w[k_widget_spcsetup_mode_fft2], FALSE, FALSE );
	} else if  (spcv_par.comp_method == cSpcModeFft)  {
		XmToggleButtonSetState( w[k_widget_spcsetup_mode_powspc], FALSE, FALSE );
		XmToggleButtonSetState( w[k_widget_spcsetup_mode_fft], TRUE, FALSE );
		XmToggleButtonSetState( w[k_widget_spcsetup_mode_fft2], FALSE, FALSE );
	} else {
		XmToggleButtonSetState( w[k_widget_spcsetup_mode_powspc], FALSE, FALSE );
		XmToggleButtonSetState( w[k_widget_spcsetup_mode_fft], FALSE, FALSE );
		XmToggleButtonSetState( w[k_widget_spcsetup_mode_fft2], TRUE, FALSE );
	} /*endif*/

} /* end of spc_set_dialog_values */



/*----------------------------------------------------------------------------*/



static void spc_draw_axes( void )

/* Draws axes into spectrum window
 *
 * parameters of routine
 * none
 */
{
	/* local variables */
	Widget   w;                   /* drawing area */
	static XFontStruct *font;     /* pointer to font structure */
	int      lx1, lx2, ly1, ly2;  /* line coordinates */
	float    curr, cc;            /* current pos */
	char     str[cBcLineLth+1];   /* text */
	int      textw, texth;        /* width and height of text */
	int      i;                   /* counter */

	/* executable code */

	w = spcv_draw_wdw;

	font = XQueryFont( XtDisplay(w), XGContextFromGC(spcv_gc) );
	texth = font->ascent + font->descent;

	pix_ClearWindow( XtDisplay(w), XtWindow(w) );
	spc_write_label( (GC)0, 0, "--RESET--" );

	/* draw vertical ticks and grid and x-labelling */
	for  (curr=spcv_bounds.frqlo; curr <= spcv_bounds.frqhi; curr *= 10.0)  {
		sprintf( str, "%3.1f", log10(curr) );
		spc_transform( curr, spcv_bounds.amplo, &lx1, &ly1 );
		pix_DrawLine( XtDisplay(w), XtWindow(w), spcv_gc,
			lx1, ly1, lx1, ly1+TICKLTH );
		/* grid */
		if  (spcv_par.draw_grid)
			pix_DrawLine( XtDisplay(w), XtWindow(w), spcv_grid_gc,
				lx1, ly1, lx1, ly1-spcv_ewdw.wh );
		textw = XTextWidth( font, str, (int)strlen(str) );
		pix_DrawString( XtDisplay(w), XtWindow(w), spcv_gc,
			lx1-textw/2, ly1+texth+TICKLTH, str, (int)strlen(str) );
		/* small ticks */
		for  (i=1; i<=9; i++)  {
			cc = curr + (float)i*curr;
			if  (cc > spcv_bounds.frqhi)  break;
			spc_transform( cc, spcv_bounds.amplo, &lx1, &ly1 );
			pix_DrawLine( XtDisplay(w), XtWindow(w), spcv_gc,
				lx1, ly1, lx1, ly1+TICKLTH/2 );
			/* grid */
			if  (spcv_par.draw_grid)
				pix_DrawLine(  XtDisplay(w), XtWindow(w), spcv_grid_gc,
					lx1, ly1, lx1, ly1-spcv_ewdw.wh );
		} /*endfor*/
	} /*endfor*/

	/* draw vertical ticks and grid and y-labelling */
	for  (curr=spcv_bounds.amplo; curr <= spcv_bounds.amphi; curr *= 10.0)  {
		sprintf( str, "%3.1f", log10(curr) );
		spc_transform( spcv_bounds.frqlo, curr, &lx1, &ly1 );
		pix_DrawLine( XtDisplay(w), XtWindow(w), spcv_gc,
			lx1, ly1, lx1-TICKLTH, ly1 );
		/* grid */
		if  (spcv_par.draw_grid)
			pix_DrawLine( XtDisplay(w), XtWindow(w), spcv_grid_gc,
				lx1, ly1, lx1+spcv_ewdw.ww, ly1 );
		textw = XTextWidth( font, str, (int)strlen(str) );
		pix_DrawString( XtDisplay(w), XtWindow(w), spcv_gc,
			lx1-textw-TICKLTH-3, ly1+texth/2, str, (int)strlen(str) );
		/* small ticks */
		for  (i=1; i<=9; i++)  {
			cc = curr + (float)i*curr;
			if  (cc > spcv_bounds.amphi)  break;
			spc_transform( spcv_bounds.frqlo, cc, &lx1, &ly1 );
			pix_DrawLine(  XtDisplay(w), XtWindow(w), spcv_gc,
				lx1, ly1, lx1-TICKLTH/2, ly1 );
			/* grid */
			if  (spcv_par.draw_grid)
				pix_DrawLine( XtDisplay(w), XtWindow(w), spcv_grid_gc,
					lx1, ly1, lx1+spcv_ewdw.ww, ly1 );
		} /*endfor*/
	} /*endfor*/

	/* draw the two axes */
	spc_transform( spcv_bounds.frqlo, spcv_bounds.amplo, &lx1, &ly1 );
	spc_transform( spcv_bounds.frqhi, spcv_bounds.amplo, &lx2, &ly2 );
	pix_DrawLine( XtDisplay(w), XtWindow(w), spcv_gc, lx1, ly1, lx2, ly2 );
	spc_transform( spcv_bounds.frqlo, spcv_bounds.amplo, &lx1, &ly1 );
	spc_transform( spcv_bounds.frqlo, spcv_bounds.amphi, &lx2, &ly2 );
	pix_DrawLine( XtDisplay(w), XtWindow(w), spcv_gc, lx1, ly1, lx2, ly2 );

	/* label */
	strcpy( str, "log. Frequency log(f/[Hz])" );
	textw = XTextWidth( font, str, (int)strlen(str) );
	pix_DrawString( XtDisplay(w), XtWindow(w), spcv_gc, spcv_ewdw.wx
		+spcv_ewdw.ww/2-textw/2, spcv_twdw.wh-spcv_ewdw.wy+3*texth, str,
		(int)strlen(str) );

} /* end of spc_draw_axes */



/*----------------------------------------------------------------------------*/



static void spc_transform( float x, float y, int *px, int *py )

/* Transforms input user coordinates into window pixel coordinates
 *
 * parameters of routine
 * float      x, y;      input; user coordinates
 * int        *px, *py;  output; window coordinates
 */
{
	/* local variables */

	/* executable code */

	/* take logarithm */
	if  (x <= 0.0)  x = 0.0;
	if  (y <= 0.0)  y = 0.0;
	x = (log10(x) - spcv_scale.xo) * spcv_scale.xf;
	y = (log10(y) - spcv_scale.yo) * spcv_scale.yf;
	*px = Nint(x) + spcv_ewdw.wx;
	*py = spcv_twdw.wh - (Nint(y) + spcv_ewdw.wy);

} /* end of spc_transform */



/*----------------------------------------------------------------------------*/



static void spc_plot_trace( GC gc, float *dat, int lth, float d,
	TSyStatus *status )

/* Plots trace data into window
 *
 * parameters of routine
 * float       dat[];       input; data array
 * int         lth;         input; length of array
 * float       d;           input; sample distance
 */
{
	/* local variables */
	XPoint   *pxy;        /* plot array */
	int      i;           /* counter */
	int      px, py;      /* temp */

	/* executable code */

	pxy = (XPoint *)sy_allocmem( lth, (int)sizeof(XPoint), status );
	if  (Severe(status))  return;

	for  (i=1; i<lth; i++)  {
		spc_transform( (float)i*d, dat[i], &px, &py );
		pxy[i-1].x = px;
		pxy[i-1].y = py;
	} /*endfor*/

	pix_DrawLines( XtDisplay(spcv_draw_wdw), XtWindow(spcv_draw_wdw),
		gc, pxy, lth-1, CoordModeOrigin );

	sy_deallocmem( pxy );

} /* end of spc_plot_trace */



/*----------------------------------------------------------------------------*/



static void spc_make_scaling( void )

/* Computes window sizes and scaling variables, sets clipping rectangle on
 * all GC's.
 *
 * parameters of routine
 * none (changes global variables)
 */
{
	/* local variables */
	TSpcTrace    *trc;      /* pointer to trace */
	XRectangle   rect;      /* clipping rectangle */

	/* executable code */

	/* determine window sizes */
	mg_get_windowsize( spcv_draw_wdw, &spcv_twdw.ww, &spcv_twdw.wh );
	spcv_twdw.wx = spcv_twdw.wy = 0;
	spcv_ewdw.wx = LEFT_MARGIN;
	spcv_ewdw.wy = BOTTOM_MARGIN;
	spcv_ewdw.ww = spcv_twdw.ww - LEFT_MARGIN - RIGHT_MARGIN;
	spcv_ewdw.wh = spcv_twdw.wh - TOP_MARGIN - BOTTOM_MARGIN;

	/* determine scaling */
	spcv_scale.xo = log10( spcv_bounds.frqlo );
	spcv_scale.yo = log10( spcv_bounds.amplo );
	spcv_scale.xf = log10( spcv_bounds.frqhi );
	spcv_scale.xf = (float)spcv_ewdw.ww / (spcv_scale.xf - spcv_scale.xo);
	spcv_scale.yf = log10( spcv_bounds.amphi );
	spcv_scale.yf = (float)spcv_ewdw.wh / (spcv_scale.yf - spcv_scale.yo);

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

} /* end of spc_make_scaling */



/*----------------------------------------------------------------------------*/



static void spc_redraw( TSyStatus *status )

/* Redraws all traces on display
 *
 * parameters of routine
 * TSyStatus  *status;      output; return status
 */
{
	/* local variables */
	TSpcTrace   *trc;                 /* pointer to trace */
	char        stream[cBcLineLth+1]; /* stream string */

	/* executable code */

	spc_print_position( spcv_draw_wdw, -1, -1 );
	spc_draw_axes();

	if  (spcv_roottrc == NULL)  return;

	trc = spcv_roottrc;
	do  {
		spc_plot_trace( trc->gc, trc->fftptr, trc->fftlth, trc->df, status );
		if  (SySevere(status))  return;
		sprintf( stream, "%s-%s-%c", trc->station, trc->chan, trc->comp );
		spc_write_label( trc->gc, trc->text_height, stream );
		trc = trc->next;
	}  while (trc != NULL);

	spcv_crosshair = FALSE;  /* to prevent permanent lines on display */

} /* end of spc_redraw */



/*----------------------------------------------------------------------------*/



static void spc_write_label( GC gc, int texth, char label[] )

/* Writes label to window
 *
 * parameters of routine
 * GC         gc;       input; graphics context
 * int        texth;    input; text height
 * char       label[];  input; text to write
 */
{
	/* local variables */
	static int   xpos;     /* x-position relative to text origin */
	static int   ypos;     /* y-position relative to text origin */

	/* executable code */

	if  (strcmp(label,"--RESET--") == 0)  {
		xpos = spcv_twdw.ww - 150;
		ypos = 60;
		return;
	} /*endif*/

	pix_DrawString( XtDisplay(spcv_draw_wdw), XtWindow(spcv_draw_wdw), gc,
			xpos, ypos, label, (int)strlen(label) );

	ypos += texth + 2;

} /* end of spc_write_label */



/*----------------------------------------------------------------------------*/



static void spc_compute_spectrum( TSpcTrace *trc, TSyStatus *status )

/* Computes spectrum trace.  Takes data at 'trc->datptr' as input, uses
 * method 'trc->mode' and creates output data at 'trc->fftptr'.  Previously
 * existing data at 'trc->fftptr' (if != NULL) are freed.
 *
 * parameters at routine
 * TSpcTrace  *trc;        input; pointer to trace info
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	float    *ftmp;               /* pointer to temporary array */
	int      tmplth;              /* length of temporary array */
	int      i;                   /* counter */

	/* executable code */

	/* determine lengths of temporary array and of output trace */
	if  (spcv_par.comp_method == cSpcModePowSpc)  {
		if  (spcv_par.trunc_trc)  {
			i = tmplth = 1;
			while  (tmplth <= trc->datlth)  {
				i <<= 1;
				tmplth = (spcv_par.ps_overlap)
					? (2*spcv_par.ps_numwdw+1)*i : 4*spcv_par.ps_numwdw*i;
			} /*endwhile*/
			i /= 2;
			trc->fftlth = i;
			tmplth = (spcv_par.ps_overlap)
				? (2*spcv_par.ps_numwdw+1)*i : 4*spcv_par.ps_numwdw*i;
		} else {
			i = (spcv_par.ps_overlap) ? (trc->datlth)/(2*spcv_par.ps_numwdw+1)
				: (trc->datlth)/(4*spcv_par.ps_numwdw);
			trc->fftlth = i;
			i = 1;
			while  (i < (trc->fftlth))  i <<= 1;
			trc->fftlth = i;
			tmplth = (spcv_par.ps_overlap) ? (2*spcv_par.ps_numwdw+1)*(trc->fftlth)
				: 4*spcv_par.ps_numwdw*(trc->fftlth);
		} /*endif*/
	} else if  (spcv_par.comp_method == cSpcModeFft
		|| spcv_par.comp_method == cSpcModeFft2)  {
		i = 1;
		while  (i < (trc->datlth))  i <<= 1;
		tmplth = i;
		if  (spcv_par.trunc_trc)  tmplth /= 2;
		trc->fftlth = tmplth/2;
	} else {
		*status = 1;
		printf( "*SHM: spc_compute_spectrum: bug in program\n" );
		return;
	} /*endif*/
	if  (tmplth > trc->datlth)  {
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: have %d samples (wdw %d), need %d --> %d samples zeroed\n",
				trc->datlth, trc->fftlth, tmplth, tmplth - trc->datlth );
	} else {
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf(
				"SHM-dbg2: have %d samples (wdw %d), need %d --> %d samples truncated\n",
				trc->datlth, trc->fftlth, tmplth, trc->datlth - tmplth );
	} /*endif*/

	/* allocate memory (wouldn't be necessary for tmplth < trc->datlth, but...)*/
	ftmp = (float *)sy_allocmem( tmplth, (int)sizeof(float), status );
	if  (SySevere(status))  {
		printf( "*SHM: mg_selected_wave: waveform allocation error\n" );
		return;
	} /*endif*/
	if  (trc->fftptr != NULL)  sy_deallocmem( trc->fftptr );
	trc->fftptr = (float *)sy_allocmem( trc->fftlth, (int)sizeof(float),
		status );
	if  (Severe(status))  {
		trc->fftptr = NULL;
		sy_deallocmem( ftmp );
		printf( "*SHM: mg_selected_wave: waveform allocation error\n" );
		return;
	} /*endif*/

	/* this makes a copy of the data */
	if  (tmplth <= trc->datlth)  {
		for  (i=0; i<tmplth; i++)
			ftmp[i] = trc->datptr[i];
	} else {
		for  (i=0; i<(trc->datlth); i++)
			ftmp[i] = trc->datptr[i];
		for  (i=(trc->datlth); i<tmplth; i++)
			ftmp[i] = 0.0;
	}/*endif*/

	/* compute spectrum */
	if  (spcv_par.comp_method == cSpcModePowSpc)  {
		nr_spctrm( ftmp-1, (trc->fftptr)-1, trc->fftlth,
			spcv_par.ps_numwdw, spcv_par.ps_overlap );
		trc->df = 1./(float)(2*(trc->fftlth)) / trc->dt;
		/* multiply power spectrum by 2pi for some reason */
		for  (i=0; i<(trc->fftlth); i++)
			trc->fftptr[i] *= 2.0*cBcPi;
	} else if  (spcv_par.comp_method == cSpcModeFft)  {
		nr_realft( ftmp-1, tmplth/2, 1 );
		trc->fftptr[0] = ftmp[0];  /* is this right ??? */
		trc->df = 1./(float)(2*(trc->fftlth)) / trc->dt;
		for  (i=1; i<(trc->fftlth); i++)
			trc->fftptr[i] = trc->dt 
				* sqrt( ftmp[2*i]*ftmp[2*i] + ftmp[2*i+1]*ftmp[2*i+1] );
	} else if  (spcv_par.comp_method == cSpcModeFft2)  {
		nr_realft( ftmp-1, tmplth/2, 1 );
		trc->fftptr[0] = ftmp[0];  /* is this right ??? */
		trc->df = 1./(float)(2*(trc->fftlth)) / trc->dt;
		for  (i=1; i<(trc->fftlth); i++)
			trc->fftptr[i] = trc->dt 
				* ( ftmp[2*i]*ftmp[2*i] + ftmp[2*i+1]*ftmp[2*i+1] );
	} else {
		*status = 1;
		printf( "*SHM: spc_compute_spectrum: bug in program, mode=%d\n",
			spcv_par.comp_method );
		return;
	} /*endif*/

	sy_deallocmem( ftmp );

} /* end of spc_compute_spectrum */



/*----------------------------------------------------------------------------*/



static void spc_auto_scaling( void )

/* Computes display bound appropriate for first trace in list
 *
 * parameters of routine
 * none
 */
{
	/* local variables */
	TSpcTrace    *trc;            /* pointer to trace */
	float        frqmin, frqmax;  /* frequency bounds */
	float        ampmin, ampmax;  /* amplitude bounds */
	float        min, max;        /* extreme values */
	int          i;               /* counter */

	/* executable code */

	if  (spcv_roottrc == NULL)  return;

	frqmin = spcv_roottrc->df;
	frqmax = spcv_roottrc->df * spcv_roottrc->fftlth;
	ampmin = ampmax = spcv_roottrc->fftptr[1];
	/* search all traces */
	trc = spcv_roottrc;
	while  (trc != NULL)  {
		min = trc->df;
		max = trc->df * trc->fftlth;
		if  (min < frqmin)  frqmin = min;
		if  (max > frqmax)  frqmax = max;
		for  (i=1; i<(trc->fftlth); i++)  {
			if  (trc->fftptr[i] < ampmin)  ampmin = trc->fftptr[i];
			if  (trc->fftptr[i] > ampmax)  ampmax = trc->fftptr[i];
		} /*endfor*/
		trc = trc->next;
	} /*endwhile*/

	/* x-direction */
	min = log10( frqmin );
	max = log10( frqmax );
	min = (float)((int)min - 1);
	max = (float)((int)max + 1);
	min = pow( 10.0, min );
	max = pow( 10.0, max );
	spcv_bounds.frqlo = min;
	spcv_bounds.frqhi = max;

	/* y-direction */
	min = log10( ampmin );
	max = log10( ampmax );
	min = (float)((int)min - 1);
	max = (float)((int)max + 1);
	min = pow( 10.0, min );
	max = pow( 10.0, max );
	spcv_bounds.amplo = min;
	spcv_bounds.amphi = max;

	spc_make_scaling();
	spc_draw_axes();

} /* end of spc_auto_scaling */



/*----------------------------------------------------------------------------*/



void spc_handle_xevent( Widget w, XEvent *ev, TSyStatus *status )

/* Handles X events in spectrum window
 *
 * parameters of routine
 * Widget     w;       input; widget
 * XEvent     *ev;     input; event reported
 * TSyStatus  *status; output; return status
 */
{
	/* local variables */
	static int     frq_a, frq_b;       /* frq window selected */
	float          b, m;               /* parameters of line */

	/* executable code */

	if  (ev->type == MotionNotify)  {
		spc_print_position( w, ev->xmotion.x, ev->xmotion.y );
		if  (spcv_crosshair)
			spc_crosshair_cursor( cSpcCH_newpos, ev->xmotion.x, ev->xmotion.y );
	} else if  (ev->type == ButtonPress) {
		if  (ev->xbutton.button == Button1)  {
			if  (spcv_fitline_state != cSpcFL_idle)  {
				if  (spcv_fitline_state == cSpcFL_wait_left)  {
					spcv_fitline_state = cSpcFL_wait_right;
					frq_a = ev->xmotion.x;
					pix_FillRectangle( XtDisplay(spcv_draw_wdw),
						XtWindow(spcv_draw_wdw), spcv_gc,
						ev->xmotion.x-2, ev->xmotion.y-2, 5, 5 );
					XDefineCursor( XtDisplay(spcv_draw_wdw), XtWindow(spcv_draw_wdw),
						spcv_crsr_left );
				} else {
					frq_b = ev->xmotion.x;
					XUndefineCursor( XtDisplay(spcv_draw_wdw), XtWindow(spcv_draw_wdw));
					pix_FillRectangle( XtDisplay(spcv_draw_wdw),
						XtWindow(spcv_draw_wdw), spcv_gc,
						ev->xmotion.x-2, ev->xmotion.y-2, 5, 5 );
					spc_compute_line( frq_a, frq_b, &b, &m, status );
					spc_display_linefit( b, m );
					spcv_fitline_state = cSpcFL_idle;
				} /*endif*/
			} /*endif*/
		} else if  (ev->xbutton.button = Button2)  {
			if (spcv_crosshair)  {
				spcv_crosshair = FALSE;
				spc_crosshair_cursor( cSpcCH_finish, ev->xmotion.x, ev->xmotion.y );
			} else {
				spcv_crosshair = TRUE;
				spc_crosshair_cursor( cSpcCH_init, ev->xmotion.x, ev->xmotion.y );
			} /*endif*/
		} /*endif*/
	} else if  (ev->type == ButtonRelease) {
	} /*endif*/

} /* end of spc_handle_xevent */



/*----------------------------------------------------------------------------*/



static void spc_print_position( Widget w, int x, int y )

/* Prints mouse position in window
 *
 * parameters of routine
 * Widget     w;     input; widget ID
 * int        x, y;  input; cursor position
 */
#define TEXT_X 10
#define TEXT_Y 15
{
	/* local variables */
	static char oldstr[BC_LINELTH+1]="";  /* previous string */
	char     str[BC_LINELTH+1];           /* scratch string */
	float    frq, amp;                    /* current position */
	float    lfrq, lamp;                  /* logarithm of frq and amp */

	/* executable code */

	if  (x == -1 && y == -1)  {
		/* reset string */
		if  (*oldstr != '\0')
			pix_DrawString( XtDisplay(w), XtWindow(w), spcv_xor_gc,
				TEXT_X, TEXT_Y, oldstr, (int)strlen(oldstr) );
		*oldstr = '\0';
		return;
	} /*endif*/

	frq = (float)((x-spcv_ewdw.wx)/spcv_scale.xf + spcv_scale.xo);
	amp = (float)((spcv_twdw.wh-y-spcv_ewdw.wy)/spcv_scale.yf + spcv_scale.yo);
	lfrq = pow( 10.0, frq );
	lamp = pow( 10.0, amp );

	if  (frq > 0.0)  {
		sprintf( str, "frq: 10^(%5.1f)Hz=%8gHz   amp: 10^(%5.1f)=%8g",
			frq, lfrq, amp, lamp );
	} else {
		sprintf( str, "frq: 10^(%5.1f)Hz=%8gs    amp: 10^(%5.1f)=%8g",
			frq, 1.0/lfrq, amp, lamp );
	} /*endif*/

	if  (*oldstr != '\0')
		pix_DrawString( XtDisplay(w), XtWindow(w), spcv_xor_gc,
			TEXT_X, TEXT_Y, oldstr, (int)strlen(oldstr) );
	pix_DrawString( XtDisplay(w), XtWindow(w), spcv_xor_gc,
		TEXT_X, TEXT_Y, str, (int)strlen(str) );
	XFlush( XtDisplay(w) );

	strcpy( oldstr, str );

} /* end of spc_print_position */



/*----------------------------------------------------------------------------*/



static void spc_print_info( Widget w, char info[] )

/* Prints mouse position in window
 *
 * parameters of routine
 * Widget     w;     input; widget ID
 */
#define INFO_X 10
#define INFO_Y 15
{
	/* local variables */
	static char oldstr[BC_LINELTH+1]="";  /* previous string */

	/* executable code */

	if  (*info == '\0')  {
		/* reset string */
		if  (*oldstr != '\0')
			pix_DrawString( XtDisplay(w), XtWindow(w), spcv_xor_gc,
				INFO_X, spcv_twdw.wh-INFO_Y, oldstr, (int)strlen(oldstr) );
		*oldstr = '\0';
		return;
	} /*endif*/

	if  (*oldstr != '\0')
		pix_DrawString( XtDisplay(w), XtWindow(w), spcv_xor_gc,
			INFO_X, spcv_twdw.wh-INFO_Y, oldstr, (int)strlen(oldstr) );
	pix_DrawString( XtDisplay(w), XtWindow(w), spcv_xor_gc,
		INFO_X, spcv_twdw.wh-INFO_Y, info, (int)strlen(info) );
	XFlush( XtDisplay(w) );

	strcpy( oldstr, info );

} /* end of spc_print_info */



/*----------------------------------------------------------------------------*/



void spc_start_fitline( void )

/* Next two mouse clicks are left and right window border for line fitting
 *
 * no parameters
 */
{
	/* local variables */

	/* executable code */

	spcv_fitline_state = cSpcFL_wait_left;
	XDefineCursor( XtDisplay(spcv_draw_wdw), XtWindow(spcv_draw_wdw),
		spcv_crsr_right );

} /* end if spc_start_fit_line */



/*----------------------------------------------------------------------------*/



static void spc_compute_line( int f1, int f2, float *b, float *m,
	TSyStatus *status )

/* fits line to points given in frq window
 *
 * parameters of routine
 * int        f1, f2;      input; frq window
 * float      *b, *m;      output; line parameters in logarithmic frq and ampl
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	float    frq1, frq2;       /* exp frequency bounds */
	float    lfrq1, lfrq2;     /* linear frequency bounds */
	float    f;                /* current frequency */
	int      i;                /* counter */
	TSpcTrace *trc;            /* trace pointer */
	int      trcno;            /* number of traces */
	int      maxsmpno;         /* max numer of samples */
	int      scnt;             /* sample counter */
	float    *f_ampl;          /* amplitudes to fit */
	float    *f_frq;           /* frequencies */
	float    siga, sigb, chi2, q;  /* line fit qualities */

	/* executable code */

	frq1 = (float)((f1-spcv_ewdw.wx)/spcv_scale.xf + spcv_scale.xo);
	frq2 = (float)((f2-spcv_ewdw.wx)/spcv_scale.xf + spcv_scale.xo);
	lfrq1 = pow( 10.0, frq1 );
	lfrq2 = pow( 10.0, frq2 );

	trcno = 0;
	trc = spcv_roottrc;
	maxsmpno = 0;
	if  (trc == NULL)  return;
	do  {
		trcno++;
		maxsmpno += Nint((lfrq2-lfrq1)/(trc->df)) + 1;
		trc = trc->next;
	}  while (trc != NULL);

	/* allocate memory */
	f_ampl = (float *)sy_allocmem( maxsmpno, (int)sizeof(float), status );
	if  (SySevere(status)) return;
	f_frq = (float *)sy_allocmem( maxsmpno, (int)sizeof(float), status );
	if  (SySevere(status)) return;

	trc = spcv_roottrc;
	scnt = 0;
	do  {
		for  (i=1; i<trc->fftlth; i++)  {
			f = (float)i * trc->df;
			if  (lfrq1 < f && f < lfrq2)  {
				if  (scnt == maxsmpno)  {
					*status = 1;
					printf( "*SHM: spc_compute_line: bug in program\n" );
				} /*endif*/
				f_frq[scnt] = log10( f );
				f_ampl[scnt] = log10( trc->fftptr[i] );
				scnt++;
			} /*endif*/
		} /*endfor*/
		trc = trc->next;
	}  while (trc != NULL);

	nr_fit( f_frq-1, f_ampl-1, scnt, NULL, FALSE,
		b, m, &siga, &sigb, &chi2, &q );
	/*printf( "b: %f  m: %f\n", *b, *m );*/

	sy_deallocmem( f_ampl );
	sy_deallocmem( f_frq );

} /* end of spc_compute_line */



/*----------------------------------------------------------------------------*/



static void spc_display_linefit( float b, float m )

/* displays line fit
 *
 * parameters of routine
 * float      b, m;  input; line parameters in logarithmic frq and ampl
 */
{
	/* local variables */
	float    frq, ampl;           /* point in window */
	int      lx1, ly1, lx2, ly2;  /* end points of line to draw */
	XRectangle rect;              /* clipping rectangle */
	char     str[cBcLineLth+1];   /* info string */

	/* executable code */

	/* logarithmic points */
	frq = log10( spcv_bounds.frqlo );
	ampl = m*frq + b;
	/* take exp */
	frq = spcv_bounds.frqlo;
	ampl = pow( 10.0, ampl );
	/* get x,y coordinates */
	spc_transform( frq, ampl, &lx1, &ly1 );
	/* logarithmic points */
	frq = log10( spcv_bounds.frqhi );
	ampl = m*frq + b;
	/* take exp */
	frq = spcv_bounds.frqhi;
	ampl = pow( 10.0, ampl );
	/* get x,y coordinates */
	spc_transform( frq, ampl, &lx2, &ly2 );

	rect.x = spcv_ewdw.wx;
	rect.y = spcv_twdw.wh - spcv_ewdw.wy - spcv_ewdw.wh;
	rect.width = spcv_ewdw.ww;
	rect.height = spcv_ewdw.wh;
	pix_SetClipRectangles( XtDisplay(spcv_draw_wdw), spcv_line_gc,
		0, 0, &rect, 1, Unsorted );
	pix_DrawLine( XtDisplay(spcv_draw_wdw), XtWindow(spcv_draw_wdw),
		spcv_line_gc, lx1, ly1, lx2, ly2 );

	sprintf( str,
		"line parameters log(a) = m*log(f) + b, m = %5.2f  b = %5.2f", m, b );
	spc_print_info( spcv_draw_wdw, str );

} /* end of spc_display_linefit */



/*----------------------------------------------------------------------------*/



static void spc_crosshair_cursor( TSpcCrosshairMode mode, int x, int y )

/* Show crosshair cursor
 *
 * parameters of routine
 * TSpcCrosshairMode  mode;   input; display mode
 * int                x, y;   input; position of cursor
 */
{
	/* local variables */
	XRectangle rect;              /* clipping rectangle */
	static int xo, yo;            /* old positions */

	/* executable code */

	rect.x = spcv_ewdw.wx;
	rect.y = spcv_twdw.wh - spcv_ewdw.wy - spcv_ewdw.wh;
	rect.width = spcv_ewdw.ww;
	rect.height = spcv_ewdw.wh;
	pix_SetClipRectangles( XtDisplay(spcv_draw_wdw), spcv_line_gc,
		0, 0, &rect, 1, Unsorted );

	switch  (mode)  {
		case cSpcCH_newpos:
		case cSpcCH_finish:
			/* remove old cross */
			pix_DrawLine( XtDisplay(spcv_draw_wdw), XtWindow(spcv_draw_wdw),
				spcv_line_gc, 0, yo, spcv_twdw.ww, yo );
			pix_DrawLine( XtDisplay(spcv_draw_wdw), XtWindow(spcv_draw_wdw),
				spcv_line_gc, xo, 0, xo, spcv_twdw.wh );
			if  (mode == cSpcCH_finish)  break;
		case cSpcCH_init:
			pix_DrawLine( XtDisplay(spcv_draw_wdw), XtWindow(spcv_draw_wdw),
				spcv_line_gc, 0, y, spcv_twdw.ww, y );
			pix_DrawLine( XtDisplay(spcv_draw_wdw), XtWindow(spcv_draw_wdw),
				spcv_line_gc, x, 0, x, spcv_twdw.wh );
			xo = x;
			yo = y;
		break;
	} /*endswitch*/

} /* end of spc_crosshair_cursor */



/*----------------------------------------------------------------------------*/



void spc_spectrogram( TSyStatus *status )

/* Writes spectrogram data to file and calls display program
 *
 * parameters of routine
 * Widget     spcwdw;        input; widget ID of spectrum drawing area
 */
{
	/* local variables */
	static int spctcnt=0;          /* spectrogram counter */
	TSpcTrace *trc;                /* new trace pointer */
	char     *env;                 /* pointer to environment */
	char     fname[cBcFileLth+1];  /* name of output file */
	char     stream[cBcLineLth+1]; /* stream string */
	char     cmd[cBcLongStrLth+1]; /* command line */

	/* executable code */

	/* make new trace pointer */
	trc = sy_allocmem( 1, (int)sizeof(TSpcTrace), status );
	if  (SySevere(status))  return;
	trc->next = NULL;
	trc->datptr = trc->fftptr = NULL;

	/* get trace data */
	mg_selected_wave( &(trc->datptr), &(trc->datlth), trc->stime, &(trc->dt),
		trc->station, trc->chan, &(trc->comp) );
	/* trc->stime not set, check mg_selected_wave !!! */
	if  (trc->datptr == NULL)  {
		sy_deallocmem( trc );
		printf( "*SHM: spc_spectrogram: no trace data.  Abort.\n" );
		return;
	} /*endif*/

	env = (char *)getenv( "SH_SCRATCH" );
	if  (strlen(env)+19 >= cBcFileLth)  {
		*status = SPE_STROVFL;
		return;
	} /*endif*/
	spctcnt++;
	sprintf( fname, "%sspectrogram_%03d.dat", env, spctcnt );

	spc_compute_spectrogram( trc, fname, status );
	if  (SySevere(status))  return;

	/* should call plot program here */
	env = (char *)getenv( "SH_SOURCE" );
	if  (2*strlen(env)+strlen(fname)+60 >= cBcLongStrLth)  {
		*status = SPE_STROVFL;
		return;
	} /*endif*/
	sprintf( cmd, "UIDPATH=%s/img/mapspec.uid; export UIDPATH; %s/img/mapspec %s 30 &", env, env, fname );
	system( cmd );

} /* end of spc_spectrogram */



/*----------------------------------------------------------------------------*/



void spc_compute_spectrogram( TSpcTrace *trc, char fname[], TSyStatus *status )

/* Computes spectrogram and writes result to file
 *
 * parameters of routine
 * TSpcTrace     *trc;        input; trace info
 * char          fname[];     input; name of output file
 * TSyStatus     *status;     output; return status
 */
{
	/* local variables */
	int      spg_length;             /* length of each fft trace */
	int      spg_step;               /* step size in samples */
	FILE     *fp;                    /* pointer to output file */
	char     spgtime[cBcTimeLth+1];  /* start time of spectrogram */
	int      spglines;               /* number of spectrogram lines */
	int      i, j;                   /* counters */
	float    *ftmp;                  /* pointer to temporary array */
	float    *dsrc;                  /* pointer to FFT input data */

	/* executable code */

	spg_length = GpGetInt( cGpI_spectrogram_width );
	spg_step = GpGetInt( cGpI_spectrogram_step );

	tc_tadd( trc->stime, (float)(spg_length/2)*(trc->dt), spgtime, status );
	if  (SySevere(status))  return;
	spglines = (((trc->datlth)-spg_length)/spg_step)+1;

	if  (trc->datlth <= spg_length)  {
		*status = SPE_SPGSHORT;
		return;
	} /*endif*/

	/* allocate memory for FFT */
	ftmp = (float *)sy_allocmem( spg_length, (int)sizeof(float), status );
	if  (SySevere(status))  return;

	trc->fftlth = spg_length / 2;
	trc->df = 1./(float)(spg_length) / trc->dt;

	fp = fopen( fname, "w" );
	if  (fp == NULL)  {
		*status = SPE_OPENWRITE;
		return;
	} /*endif*/

	fprintf( fp, "! spectrogram file created by SHM\n" );
	fprintf( fp, "!* %s-%s-%c\n", trc->station, trc->chan, trc->comp );
	fprintf( fp, "!  DELTA:   %g\n", (trc->dt)*spg_step );
	fprintf( fp, "!  START:   %s\n", spgtime );
	fprintf( fp, "!  LENGTH:  %d\n", trc->fftlth );
	fprintf( fp, "!  LINES:   %d\n", spglines );
	fprintf( fp, "!  DF:      %f\n", trc->df );
	fprintf( fp, "\n" );

	dsrc = trc->datptr;
	for  (i=0; i<spglines; i++)  {
		if  (dsrc+spg_length > (trc->datptr)+(trc->datlth))
			printf( "--> bug in spc_compute_spectrogram (%d)\n", i );
		for  (j=0; j<spg_length; j++)
			ftmp[j] = dsrc[j] * spc_taper( j, spg_length );
		nr_realft( ftmp-1, spg_length/2, 1 );
		fprintf( fp, "%e\n", ftmp[0] );
		for  (j=1; j<(trc->fftlth); j++)
			fprintf( fp, "%e\n",
				sqrt( ftmp[2*j]*ftmp[2*j] + ftmp[2*j+1]*ftmp[2*j+1] ) );
		fprintf( fp, "\n" );
		dsrc += spg_step;
	} /*endfor*/

	sy_deallocmem( ftmp );
	fclose( fp );

} /* end of spc_compute_spectrogram */



/*----------------------------------------------------------------------------*/



static float spc_taper( int idx, int maxidx )

/* returns taper value
 *
 * parameters of routine
 * int        idx;       input; current sample index (0<=idx<maxidx)
 * int        maxidx;    input; number of indices
 *                       returns taper value
 */
{
	/* local variables */

	/* executable code */

	return (cos( ((2.0*(float)idx/(float)maxidx) - 1.0)*BC_PI ) + 1.0)/2.0;

} /* end of spc_taper */



/*----------------------------------------------------------------------------*/

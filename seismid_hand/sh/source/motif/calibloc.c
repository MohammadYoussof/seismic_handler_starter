
/* file calibloc.c
 *      ==========
 *
 * version 15, 17-Oct-2006
 *
 * manager for calibration window
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



#include <stdio.h>
#include <string.h>
#include <math.h>
#include "basecnst.h"
#undef BC_DEFINE_TRUE_FALSE
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include <Xm/Xm.h>
#include BC_SYSBASE
#include "shvars.h"
#include "erusrdef.h"
#include "infoidx.h"
#include "ptusrdef.h"
#include "earthloc.h"
#include "pixmaps.h"
#include "motifgraph.h"
#include "cbutil.h"
#include "calibloc.h"
#include "globalparams.h"



/* local constants */
#define CALC_LOOKUP_FILE "calib_lookup.txt"
	/* name of lookup file in inputs directory */
#define CALC_STARTPOS 26
	/* start position of lines in calibration files */

#define CALC_LOCSTR_PRINT 0
#define CALC_LOCSTR_RESET 1
#define CALC_LOCSTR_CLEAR 2

/* macros */
#define DEG2RAD(x) ((x)*BC_PI/180.0)
#define RAD2DEG(x) ((x)/BC_PI*180.0)
#define MATH2GEO(x) (90.0-(x))
/* #define MATH2GEO(x) (x) */
#define calh_xtrafo(x) ( ((x)-calv_dsp_xorig)*calv_dsp_xfac )
#define calh_ytrafo(y) ( ((y)-calv_dsp_yorig)*calv_dsp_yfac )
#define calh_xbtrafo(x) ( (x)/calv_dsp_xfac + calv_dsp_xorig )
#define calh_ybtrafo(y) ( (y)/calv_dsp_yfac + calv_dsp_yorig )


/* global variables */
static float calv_dsp_xorig;     /* x-origin */
static float calv_dsp_yorig;     /* y-origin */
static float calv_dsp_width;     /* width of display */
static float calv_dsp_height;    /* height of display */
static float calv_dsp_xfac;      /* x transformation factor */
static float calv_dsp_yfac;      /* y transformation factor */
static int calv_wdw_height;      /* window height in pixels */
static float calv_vec_x0;        /* current vector */
static float calv_vec_y0;        /* current vector */
static float calv_vec_x1;        /* current vector */
static float calv_vec_y1;        /* current vector */
static BOOLEAN calv_gc_init;     /* GC's initialized */
static GC calv_gc_line;          /* line GC */
static GC calv_gc_xor;           /* XOR GC */
static GC calv_gc_grid;          /* grid GC */
static Widget calv_root_wdw;     /* root window of drawing area */
static CUT_PARAMS calv_params;   /* anaylsis parameters */



/* prototypes of local routines */
static void calh_find_calibfile( char calfile[], STATUS *status );
static void calh_initialize_display( Widget w, float xcenter, float ycenter );
static void calh_show_vector( Widget w, GC gc, float sx1, float sy1,
	float sx2, float sy2 );
static void calh_print_position( Widget w, int x, int y );
static void calh_change_correction( Widget w, int x, int y, BOOLEAN mark );
static void calh_terminate( Widget w, float *slo, float *az );
static void calh_plot_grid( Widget w );
static void calh_print_location( Widget w, int mode );
static void calh_mark_position( Widget w, GC gc, float sx, float sy );



/*----------------------------------------------------------------------------*/



void cal_initialize( Widget parent, Widget calwdw, STATUS *status )

/* initializes calibration window
 *
 * parameters of routine
 * Widget     parent;      input; widget ID of root window
 * Widget     calwdw;      input; Widget ID of calibration window
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	static BOOLEAN   is_init=FALSE;    /* already initialized */

	/* executable code */

	if  (!XtIsManaged(parent))  {
		XtManageChild( parent );
		calv_root_wdw = parent;
		calh_print_position( calwdw, -1, -1 );  /* reset string */
		calh_print_location( calwdw, CALC_LOCSTR_RESET );
	} /*endif*/

	if  (is_init)  return;

	pix_create_window_buffer( XtDisplay(calwdw), XtWindow(calwdw),
		TRUE, status );

	is_init = TRUE;

} /* end of cal_initialize */



/*----------------------------------------------------------------------------*/



void cal_display_calib( Widget w, CUT_PARAMS *par, STATUS *status )

/* displays calibration vectors for within a
 * window centered on given slowness and azimuth.  The analysis parameters
 * are copied to a local data structure.
 *
 * parameters of routine
 * Widget     w;           input; widget ID of calibration window
 * CUT_PARAMS *par;        input; analysis parameters
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	char     fstr[BC_FILELTH+1];          /* scratch */
	char     calfile[BC_FILELTH+1];       /* calibration file */
	FILE     *calf;                       /* pointer to calibration file */
	char     line[BC_LONGSTRLTH+1];       /* current line in calib file */
	float    s0, a0;                      /* beam slowness & azimuth */
	float    sc, ac;                      /* corrected slowness & azimuth */
	float    a0m, s0m;                    /* az & slo of next position */
	float    acm, scm;                    /* corr. az & slo of next position */
	float    a0r, acr;                    /* a0, ac in radians */
	float    ui, vi;                      /* input position */
	float    u0, v0;                      /* calibration position */
	float    u1, v1;                      /* -- " -- */
	float    dist;                        /* sqrd distance in slowness space */
	float    mindist;                     /* minimum distance */
	float    slo_out, az_out;             /* corrected slowness & azimuth */
	float    slowness, azimuth;           /* uncorrected slowness & azimuth */

	/* executable code */

	/* store analysis parameters */
	calv_params = *par;

	slowness = par->b_slowness;
	azimuth = par->b_azimuth;

	/* find name of calibration file */
	calh_find_calibfile( fstr, status );
	if  (Severe(status))  {
		calh_terminate( w, &slowness, &azimuth );
		return;
	} /*endif*/
	if  (strlen(shd_inputs)+strlen(fstr) > BC_FILELTH)  {
		*status = CALE_STROVFL;
		err_setcontext( " ## file: " ); err_setcontext( fstr );
		return;
	} /*endif*/
	strcpy( calfile, shd_inputs );
	strcat( calfile, fstr );

	/* open calibration file */
	calf = sy_fopen( calfile, "r" );
	if  (calf == NULL)  {
		*status = CALE_OPENREAD;
		err_setcontext( " ## file: " ); err_setcontext( calfile );
		return;
	} /*endif*/

	/* initialize variables for loop */
	mindist = -1.0;   /* empty value */
	azimuth = MATH2GEO( azimuth );
	dist = DEG2RAD( azimuth );  /* misuse of variable dist */
	ui = slowness * cos( dist );
	vi = slowness * sin( dist );
	calh_initialize_display( w, ui, vi );

	/* read calibration file */
	while  (fgets(line,BC_LONGSTRLTH,calf) != NULL)  {
		/* ignore comment lines */
		if  (*line == '!')  continue;
		/* read lines with calibration info */
		if  (sscanf(line+CALC_STARTPOS,"%f %f %f %f",&s0,&a0,&sc,&ac) != 4)  {
			*status = CALE_READCALIB;
			return;
		} /*endif*/
		/* transform to sx and sy and compute distance to input */
		ac = MATH2GEO( ac );
		a0 = MATH2GEO( a0 );
		a0r = DEG2RAD( a0 );
		u0 = s0 * cos( a0r );
		v0 = s0 * sin( a0r );
		acr = DEG2RAD( ac );
		u1 = sc * cos( acr );
		v1 = sc * sin( acr );
		calh_show_vector( w, calv_gc_line, u0, v0, u1, v1 );
		dist = (u0-ui)*(u0-ui) + (v0-vi)*(v0-vi);
		/* if distance is minimum store values */
		if  (dist < mindist || mindist < 0.0)  {
			mindist = dist;
			a0m = a0; s0m = s0;
			acm = ac; scm = sc;
		} /*endif*/
	} /*endwhile*/

	/* close calibration file */
	sy_fclose( calf );

	if  (mindist < 0.0)  {   /* then there was no info in file */
		*status = CALE_EMPTYCAL;
		err_setcontext( " ## file: " ); err_setcontext( calfile );
		return;
	} /*endif*/

	/* suggest correction */
	slo_out = slowness + scm - s0m;
	az_out = azimuth + acm - a0m;
	if  (az_out < 0.0)  az_out += 360.0;
	if  (az_out > 360.0)  az_out -= 360.0;

	calh_plot_grid( w );

	/* plot suggestion */
	a0r = DEG2RAD( azimuth );
	calv_vec_x0 = slowness * cos( a0r );
	calv_vec_y0 = slowness * sin( a0r );
	acr = DEG2RAD( az_out );
	calv_vec_x1 = slo_out * cos( acr );
	calv_vec_y1 = slo_out * sin( acr );
	calh_show_vector( w, calv_gc_xor, calv_vec_x0, calv_vec_y0,
		calv_vec_x1, calv_vec_y1 );

	calh_print_position( w, Nint(calh_xtrafo(calv_vec_x1)),
		calv_wdw_height-Nint(calh_ytrafo(calv_vec_y1)) );
	calh_print_location( w, CALC_LOCSTR_PRINT );

} /* end of cal_display_calib */



/*----------------------------------------------------------------------------*/



void cal_handle_xevent( Widget w, XEvent *ev )

/* handles events of calibration window
 *
 * parameters of routine
 * Widget     w;         input; widget ID of calibration window
 * XEvent     *ev;       input; event happened
 */
{
	/* local variables */
	static BOOLEAN btn_pressed=FALSE;  /* button is pressed */

	/* executable code */

	if  (ev->type == MotionNotify)  {
		if  (btn_pressed)  {
			calh_print_position( w, ev->xmotion.x, ev->xmotion.y );
			calh_change_correction( w, ev->xbutton.x, ev->xbutton.y, FALSE );
		} /*endif*/
	} else if  (ev->type == ButtonPress) {
		btn_pressed = TRUE;
		calh_print_position( w, ev->xmotion.x, ev->xmotion.y );
		calh_change_correction( w, ev->xbutton.x, ev->xbutton.y, TRUE );
	} else if  (ev->type == ButtonRelease) {
		btn_pressed = FALSE;
		calh_print_position( w, ev->xmotion.x, ev->xmotion.y );
		calh_change_correction( w, ev->xbutton.x, ev->xbutton.y, FALSE );
		calh_print_location( w, CALC_LOCSTR_CLEAR );
		calh_print_location( w, CALC_LOCSTR_PRINT );
		/* calh_terminate( w ); */
	} /*endif*/

} /* end of cal_handle_event */



/*----------------------------------------------------------------------------*/



void cal_accept_values( Widget w, CUT_PARAMS *par )

/* puts corrected values of slowness and azimuth as well as location
 * parameters to passed structure and
 * terminates calibration window.
 *
 * parameters of routine
 * Widget     w;          input; widget ID of calibration window
 * CUT_PARAMS *par;       output; analysis parameters
 */
{
	/* local variables */
	float    slowness, azimuth;    /* corrected values */

	/* executable code */

	calh_terminate( w, &slowness, &azimuth );
	if  (par == NULL)  return;

	par->l_slowness = slowness;
	par->l_azimuth = azimuth;
	par->source_lat = calv_params.source_lat;
	par->source_lon = calv_params.source_lon;
	par->distance = calv_params.distance;
	/* par->loc_meth = calv_params.loc_meth; */
	strcpy( par->regname, calv_params.regname );
	strcpy( par->phase, calv_params.phase );

} /* end of cal_accept_values */



/*----------------------------------------------------------------------------*/



static void calh_print_position( Widget w, int x, int y )

/* prints position of cursor in window
 *
 * parameters of routine
 * Widget     w;       input; widget ID of calibration window
 * float      x, y;    input; cursor position
 */
#define TEXT_X 10
#define TEXT_Y 15
{
	/* local variables */
	float    sx, sy;                      /* slowness space coo */
	float    slo, az;                     /* slwoness & azimuth */
	static char oldstr[BC_LINELTH+1]="";  /* previous string */
	char     str[BC_LINELTH+1];           /* scratch string */

	/* executable code */

	if  (x == -1 && y == -1)  {
		/* reset string */
		*oldstr = '\0';
		return;
	} /*endif*/

	sx = calh_xbtrafo( (float)(x) );
	sy = calh_ybtrafo( (float)(calv_wdw_height-y) );
	slo = sqrt( sx*sx + sy*sy );
	az = atan2( sy, sx );
	az = RAD2DEG( az );
	az = MATH2GEO( az );
	if  (az < 0.0)  az += 360.0;
	if  (az > 360.0)  az -= 360.0;

	calv_params.l_slowness = slo;
	calv_params.l_azimuth = az;
	sprintf( str, "slowness: %5.2f   azimuth: %5.1f", slo, az );
	if  (*oldstr != '\0')
		pix_DrawString( XtDisplay(w), XtWindow(w), calv_gc_xor,
			TEXT_X, TEXT_Y, oldstr, (int)strlen(oldstr) );
	pix_DrawString( XtDisplay(w), XtWindow(w), calv_gc_xor,
		TEXT_X, TEXT_Y, str, (int)strlen(str) );
	strcpy( oldstr, str );

} /* end of calh_print_position */



/*----------------------------------------------------------------------------*/



static void calh_print_location( Widget w, int mode )

/* prints out current location
 *
 * parameters of routine
 * Widget     w;            input; Widget ID of calibration window
 * int        mode;         input; mode: CALC_LOCSTR_PRINT, .._RESET, .._CLEAR
 */
#define LOCTEXT_X 10
#define LOCTEXT_Y 30
{
	/* local variables */
	static char oldstr[BC_LINELTH+1]="";  /* previous string */
	char     str[BC_LINELTH+1];           /* scratch string */
	GLT_STATINF *statinf;                 /* station info */
	STATUS   locstat;                     /* local status */

	/* executable code */

	if  (mode == CALC_LOCSTR_RESET)  {
		/* reset string */
		*oldstr = '\0';
		return;
	} else if  (mode == CALC_LOCSTR_CLEAR)  {
		if  (*oldstr != '\0')
			pix_DrawString( XtDisplay(w), XtWindow(w), calv_gc_xor,
				LOCTEXT_X, LOCTEXT_Y, oldstr, (int)strlen(oldstr) );
		*oldstr = '\0';
		XFlush( XtDisplay(w) );
		return;
	} /*endif*/

	locstat = BC_NOERROR;
	if  (strcmp(GpGetString(cGpS_refstation),"CENTRE") == 0
		|| strcmp(GpGetString(cGpS_refstation),"CENTER") == 0)  {
		cu_localization( "P", CUF_DOLOC_DISTANCE, &calv_params, &locstat );
		if  (Severe(&locstat))  {
			strcpy( calv_params.regname, "*** illegal ***" );
			calv_params.source_lat = 0.0;
			calv_params.source_lon = 0.0;
		} /*endif*/
	} else {
		calv_params.regname[0] = '\0';
		statinf = gl_store_station( GpGetString(cGpS_refstation), TRUE, &locstat );
		if  (Severe(&locstat))
			strcpy( calv_params.regname, "*** illegal station ***" );
		if  (locstat == BC_NOERROR)
			calv_params.distance =
				pt_distance( "P", calv_params.l_slowness, calv_params.depth,
					&locstat );
		if  (Severe(&locstat) && calv_params.regname[0] == '\0')
			strcpy( calv_params.regname, "*** illegal distance ***" );
		if  (locstat == BC_NOERROR)
			mb_sphereadd( statinf->lat, statinf->lon, calv_params.distance,
				calv_params.l_azimuth, &calv_params.source_lat,
				&calv_params.source_lon );
		if  (locstat == BC_NOERROR)
			mb_getlocname( calv_params.source_lat, calv_params.source_lon,
				BC_LINELTH, calv_params.regname, &locstat );
		if  (Severe(&locstat) && calv_params.regname[0] == '\0')
			strcpy( calv_params.regname, "*** illegal FER ***" );
	} /*endif*/

	sprintf( str, "lat:%5.1f lon:%6.1f fer: %s", calv_params.source_lat,
		calv_params.source_lon, calv_params.regname );
	if  (*oldstr != '\0')
		pix_DrawString( XtDisplay(w), XtWindow(w), calv_gc_xor,
			LOCTEXT_X, LOCTEXT_Y, oldstr, (int)strlen(oldstr) );
	pix_DrawString( XtDisplay(w), XtWindow(w), calv_gc_xor,
		LOCTEXT_X, LOCTEXT_Y, str, (int)strlen(str) );
	XFlush( XtDisplay(w) );
	strcpy( oldstr, str );

} /* end of calh_print_location */



/*----------------------------------------------------------------------------*/



static void calh_change_correction( Widget w, int x, int y, BOOLEAN mark )

/* changes correction of current slowness vector
 *
 * parameters of routine
 * Widget     w;       input; widget ID of calibration window
 * float      x, y;    input; new position
 * BOOLEAN    mark;    input; mark old position
 */
{
	/* local variables */
	float    sx, sy;      /* slowness space coo */

	/* executable code */

	sx = calh_xbtrafo( (float)(x) );
	sy = calh_ybtrafo( (float)(calv_wdw_height-y) );

	/* delete old vector */
	calh_show_vector( w, calv_gc_xor, calv_vec_x0, calv_vec_y0,
		calv_vec_x1, calv_vec_y1 );
	if  (mark)
		calh_mark_position( w, calv_gc_xor, calv_vec_x1, calv_vec_y1 );
	/* print new */
	calv_vec_x1 = sx;
	calv_vec_y1 = sy;
	calh_show_vector( w, calv_gc_xor, calv_vec_x0, calv_vec_y0,
		calv_vec_x1, calv_vec_y1 );

} /* end of calh_change_correction */



/*----------------------------------------------------------------------------*/



static void calh_initialize_display( Widget w, float xcenter, float ycenter )

/* initializes calibration display
 *
 * parameters of routine
 * Widget     w;                input; widget ID of calibration window
 * float      xcenter, ycenter; input; center of slowness space
 */
{
	/* local variables */
	Window   root_wdw;                  /* root window */
	int      wg_x, wg_y;                /* window geometry */
	unsigned wg_w, wg_h, wg_bd, wg_dp;  /* window geometry */
	BOOLEAN  colored;                   /* colored screen */
	Screen   *s;                        /* screen pointer */
	Colormap cmap;                      /* color map */
	XColor   color;                     /* color */
	int      xstatus;                   /* return status */

	/* executable code */

	pix_ClearWindow( XtDisplay(w), XtWindow(w) );
	XGetGeometry( XtDisplay(w), XtWindow(w), &root_wdw, &wg_x, &wg_y,
		&wg_w, &wg_h, &wg_bd, &wg_dp );

	/* setup global variables */
	calv_wdw_height = wg_h;
	calv_dsp_width = GpGetFloat(cGpF_calib_wdw_width);
	calv_dsp_height = GpGetFloat(cGpF_calib_wdw_height);
	calv_dsp_xorig = xcenter - calv_dsp_width/2.0;
	calv_dsp_yorig = ycenter - calv_dsp_height/2.0;
	calv_dsp_xfac = (float)wg_w / calv_dsp_width;
	calv_dsp_yfac = (float)wg_h / calv_dsp_height;

	if  (!calv_gc_init)  {
		s = XtScreen(w);
		colored = ((XDefaultVisualOfScreen(s))->class == TrueColor
			|| (XDefaultVisualOfScreen(s))->class == PseudoColor
			|| (XDefaultVisualOfScreen(s))->class == DirectColor
			|| (XDefaultVisualOfScreen(s))->class == StaticColor);
		if  (colored)  {
			cmap = XDefaultColormapOfScreen( s );
			color.red = 65535; color.green = color.blue = 0;
			color.flags = DoRed | DoGreen | DoBlue;
			xstatus = XAllocColor( XtDisplay(w), cmap, &color );
		} /*endif*/
		if  ((GpGetFloat(cGpF_colour_bg_red)+GpGetFloat(cGpF_colour_bg_green)
			+GpGetFloat(cGpF_colour_bg_blue))/3.0 > 0.5)  {
			calv_gc_line = XCreateGC( XtDisplay(w), XtWindow(w), 0 , NULL );
			XSetForeground( XtDisplay(w), calv_gc_line,
				XBlackPixelOfScreen(XtScreen(w)) );
			XSetBackground( XtDisplay(w), calv_gc_line,
				XWhitePixelOfScreen(XtScreen(w)) );
			calv_gc_grid = XCreateGC( XtDisplay(w), XtWindow(w), 0 , NULL );
			XSetForeground( XtDisplay(w), calv_gc_grid,
				XBlackPixelOfScreen(XtScreen(w)) );
			XSetBackground( XtDisplay(w), calv_gc_grid,
				XWhitePixelOfScreen(XtScreen(w)) );
		} else {
			calv_gc_line = XCreateGC( XtDisplay(w), XtWindow(w), 0 , NULL );
			XSetForeground( XtDisplay(w), calv_gc_line,
				XWhitePixelOfScreen(XtScreen(w)) );
			XSetBackground( XtDisplay(w), calv_gc_line,
				XBlackPixelOfScreen(XtScreen(w)) );
			calv_gc_grid = XCreateGC( XtDisplay(w), XtWindow(w), 0 , NULL );
			XSetForeground( XtDisplay(w), calv_gc_grid,
				XWhitePixelOfScreen(XtScreen(w)) );
			XSetBackground( XtDisplay(w), calv_gc_grid,
				XBlackPixelOfScreen(XtScreen(w)) );
		} /*endif*/
		calv_gc_xor = XCreateGC( XtDisplay(w), XtWindow(w), 0 , NULL );
		XSetForeground( XtDisplay(w), calv_gc_xor,
			XBlackPixelOfScreen(XtScreen(w)) );
		XSetBackground( XtDisplay(w), calv_gc_xor,
			XWhitePixelOfScreen(XtScreen(w)) );
		XSetFunction( XtDisplay(w), calv_gc_xor, GXxor );
		s = XtScreen(w);
		colored = ((XDefaultVisualOfScreen(s))->class == TrueColor
			|| (XDefaultVisualOfScreen(s))->class == PseudoColor
			|| (XDefaultVisualOfScreen(s))->class == DirectColor
			|| (XDefaultVisualOfScreen(s))->class == StaticColor);
		if  (colored)  {
			cmap = XDefaultColormapOfScreen( s );
			color.red = 65535; color.green = color.blue = 0;
			color.flags = DoRed | DoGreen | DoBlue;
			xstatus = XAllocColor( XtDisplay(w), cmap, &color );
			if  (xstatus == 0)  {
				printf( "*SHM: XAllocColor failed\n" );
			} else {
				XSetForeground( XtDisplay(w), calv_gc_xor, color.pixel );
			} /*endif*/
			color.red = 65535/2; color.green = 65535/2; color.blue = 65535;
			color.flags = DoRed | DoGreen | DoBlue;
			xstatus = XAllocColor( XtDisplay(w), cmap, &color );
			if  (xstatus == 0)  {
				printf( "*SHM: XAllocColor failed\n" );
			} else {
				XSetForeground( XtDisplay(w), calv_gc_grid, color.pixel );
			} /*endif*/
		} /*endif*/
		calv_gc_init = TRUE;
	} /*endif*/

} /* end of calh_initialize_display */



/*----------------------------------------------------------------------------*/



static void calh_show_vector( Widget w, GC gc, float sx1, float sy1,
	float sx2, float sy2 )

/* displays slowness vector (sx,sy)
 *
 * parameters of routine
 * Widget     w;        input; widget ID of calibration window
 * GC         gc;       input; graphic context
 * float      sx1, sy1; input; slowness vector start
 * float      sx2, sy2; input; slowness vector end
 */
{
	/* local variables */
	float    x, y;                     /* start point */
	float    x0, y0, x1, y1, x2, y2;   /* triangle of arrow */
	float    dx, dy;                   /* differences */
	float    angle, angle_diff;
	float    r;

	/* executable code */

	if  (sx1 < calv_dsp_xorig)  return;
	if  (sx1 > calv_dsp_xorig+calv_dsp_width)  return;
	if  (sy1 < calv_dsp_yorig)  return;
	if  (sy1 > calv_dsp_yorig+calv_dsp_height)  return;

	x = calh_xtrafo(sx1);
	y = calh_ytrafo(sy1);
	x0 = calh_xtrafo(sx2);
	y0 = calh_ytrafo(sy2);

	pix_DrawLine( XtDisplay(w), XtWindow(w), gc,
		Nint(x), calv_wdw_height-Nint(y), Nint(x0), calv_wdw_height-Nint(y0) );

	/* compute arrow */
	r = (float)calv_wdw_height / 20.0;
	angle_diff = DEG2RAD( 10.0 );
	angle = atan2( y0-y, x0-x ) + BC_PI;
	angle -= angle_diff;
	dx = r * cos( angle );
	dy = r * sin( angle );
	x1 = x0 + dx;
	y1 = y0 + dy;
	angle += 2.0*angle_diff;
	dx = r * cos( angle );
	dy = r * sin( angle );
	x2 = x0 + dx;
	y2 = y0 + dy;
	pix_DrawLine( XtDisplay(w), XtWindow(w), gc,
		Nint(x1), calv_wdw_height-Nint(y1), Nint(x0), calv_wdw_height-Nint(y0) );
	pix_DrawLine( XtDisplay(w), XtWindow(w), gc,
		Nint(x2), calv_wdw_height-Nint(y2), Nint(x0), calv_wdw_height-Nint(y0) );

} /* end of calh_show_vector */



/*----------------------------------------------------------------------------*/



#define CSIZE 10



static void calh_mark_position( Widget w, GC gc, float sx, float sy )

/* mark position at slowness (sx,sy)
 *
 * parameters of routine
 * Widget     w;        input; widget ID of calibration window
 * GC         gc;       input; graphic context
 * float      sx, sy;   input; point in slowness space
 */
{
	/* local variables */
	float    x, y;                     /* start point */

	/* executable code */

	if  (sx < calv_dsp_xorig)  return;
	if  (sx > calv_dsp_xorig+calv_dsp_width)  return;
	if  (sy < calv_dsp_yorig)  return;
	if  (sy > calv_dsp_yorig+calv_dsp_height)  return;

	x = calh_xtrafo(sx);
	y = calh_ytrafo(sy);

	pix_DrawArc( XtDisplay(w), XtWindow(w), gc, 
		Nint(x)-CSIZE/2, calv_wdw_height-Nint(y)-CSIZE/2,
		CSIZE, CSIZE, 0, 360*64 );

} /* end of calh_mark_position */



#undef CSIZE



/*----------------------------------------------------------------------------*/



static void calh_find_calibfile( char calfile[], STATUS *status )

/* finds calibration file using traces on screen
 *
 * parameters of routine
 * char       calfile[];       output; calibration file found
 * STATUS     *status;         output; return status
 */
{
	/* local variables */
	char     lookupfile[BC_FILELTH+1];   /* calibration lookup file */
	char     line[BC_LONGSTRLTH+1];      /* current line of lookup file */
	FILE     *fp;                        /* pointer to lookup file */
	char     statlist[BC_LINELTH+1];     /* station list */
	char     cstat[BC_LINELTH+1];        /* current station */
	STATUS   locstat;                    /* local status */
	BOOLEAN  list_ok;                    /* station list ok */
	void     *trc;                       /* trace pointer */

	/* executable code */

	if  (strlen(shd_inputs)+strlen(CALC_LOOKUP_FILE) > BC_FILELTH)  {
		*status = CALE_STROVFL;
		err_setcontext( " ## lookup file dir & file: " );
		err_setcontext( shd_inputs ); err_setcontext( ", " );
		err_setcontext( CALC_LOOKUP_FILE );
		return;
	} /*endif*/

	strcpy( lookupfile, shd_inputs );
	strcat( lookupfile, CALC_LOOKUP_FILE );

	/* open lookup file */
	fp = sy_fopen( lookupfile, "r" );
	if  (fp == NULL)  {
		*status = CALE_OPENREAD;
		err_setcontext( " ## file: " ); err_setcontext( lookupfile );
		return;
	} /*endif*/

	while  (fgets(line,BC_LONGSTRLTH,fp) != NULL)  {
		if  (*line == '!')  continue;
		*statlist = ',';
		if  (sscanf(line,"%s %s",statlist+1,calfile) != 2)  {
			*status = CALE_READLOOKUP;
			sy_fclose( fp );
			return;
		} /*endif*/
		if  (strlen(statlist) >= BC_LINELTH || strlen(calfile) > BC_FILELTH)  {
			printf( "*SHM:!! string length exceeded: terminate program !!***\n" );
			return;
		} /*endif*/
		strcat( statlist, "," );
		/* now scan all traces on display */
		list_ok = TRUE;
		*cstat = ',';
		trc = db_getp( NULL, EP_DSPN, NULL );
		while  (trc != NULL)  {
			locstat = BC_NOERROR;
			db_gets( trc, ES_STATION, BC_LINELTH-1, cstat+1, &locstat );
			if  (Severe(&locstat))  {list_ok = FALSE; break;}
			if  (strcmp(cstat+1,"BEAM") != 0 && strcmp(cstat+1,"ALIGN") != 0)  {
				strcat( cstat, "," );
				if  (strstr(statlist,cstat) == NULL)  {list_ok = FALSE; break;}
			} /*endif*/
			trc = db_getp( trc, EP_DSPN, NULL );
		} /*endwhile*/
		if  (list_ok)  {
			sy_fclose( fp );
			return;
		} /*endif*/
	} /*endwhile*/

	sy_fclose( fp );
	*status = CALE_NOCALIBFILE;

} /* end of calh_find_calibfile */



/*----------------------------------------------------------------------------*/



static void calh_plot_grid( Widget w )

/* plots grid
 *
 * parameters of routine
 * Widget     w;         input; widget ID of calibration window
 */
#define AZSTEP 2.0
#define PIXOFF 5
{
	/* local variables */
	static XFontStruct *font=NULL;   /* font info */
	float    az1, az2, slo1, slo2;   /* slowness & azimuth */
	float    sx1, sy1, sx2, sy2;     /* transformed values */
	float    tmp;                    /* scratch */
	char     str[BC_SHORTSTRLTH+1];  /* scratch string */
	int      lx, ly;                 /* label offset */
	int      textheight;             /* height of text */

	/* executable code */

	/* radians */
	slo1 = 3.0;
	slo2 = 10.0;
	for  (az1=0.0; az1<360.0; az1 += GpGetFloat(cGpF_calib_azimuth_grid))  {
		tmp = MATH2GEO( az1 );
		tmp = DEG2RAD( tmp );
		sx1 = slo1 * cos( tmp );
		sy1 = slo1 * sin( tmp );
		tmp = MATH2GEO( az1 );
		tmp = DEG2RAD( tmp );
		sx2 = slo2 * cos( tmp );
		sy2 = slo2 * sin( tmp );
		pix_DrawLine( XtDisplay(w), XtWindow(w), calv_gc_grid,
			Nint(calh_xtrafo(sx1)), calv_wdw_height-Nint(calh_ytrafo(sy1)),
			Nint(calh_xtrafo(sx2)), calv_wdw_height-Nint(calh_ytrafo(sy2)) );
	} /*endfor*/

	/* circles */
	for  (slo1=3.0; slo1<12.0; slo1 += GpGetFloat(cGpF_calib_slowness_grid))  {
		az1 = 0.0; az2 = az1 + AZSTEP;
      while  (az1 < 360.0)  {
			tmp = MATH2GEO( az1 );
			tmp = DEG2RAD( tmp );
			sx1 = slo1 * cos( tmp );
			sy1 = slo1 * sin( tmp );
			tmp = MATH2GEO( az2 );
			tmp = DEG2RAD( tmp );
			sx2 = slo1 * cos( tmp );
			sy2 = slo1 * sin( tmp );
			pix_DrawLine( XtDisplay(w), XtWindow(w), calv_gc_grid,
				Nint(calh_xtrafo(sx1)), calv_wdw_height-Nint(calh_ytrafo(sy1)),
				Nint(calh_xtrafo(sx2)), calv_wdw_height-Nint(calh_ytrafo(sy2)) );
			az1 = az2;  az2 = az1 + AZSTEP;
		} /*endwhile*/
	} /*endfor*/

	/* labels */
	if  (font == NULL)
		font = XQueryFont( XtDisplay(w), XGContextFromGC(calv_gc_grid) );
	textheight = font->max_bounds.ascent + font->max_bounds.descent;
	for  (az1=0.0; az1<360.0; az1 += GpGetFloat(cGpF_calib_azimuth_grid))
		for  (slo1=3.0; slo1<12.0; slo1 += GpGetFloat(cGpF_calib_slowness_grid)) {
			tmp = MATH2GEO( az1 );
			tmp = DEG2RAD( tmp );
			sx1 = slo1 * cos( tmp );
			sy1 = slo1 * sin( tmp );
			sprintf( str, "%3.1f,%2.0f", slo1, az1 );
			if  (0.0 <= az1 && az1 < 90.0)  {
				lx = PIXOFF;
				ly = -PIXOFF;
			} else if  (90.0 <= az1 && az1 < 180.0)  {
				lx = PIXOFF;
				ly = PIXOFF + textheight;
			} else if  (180.0 <= az1 && az1 < 270.0)  {
				lx = -PIXOFF - XTextWidth(font,str,strlen(str));
				ly = PIXOFF + textheight;
			} else {
				lx = -PIXOFF - XTextWidth(font,str,strlen(str));
				ly = -PIXOFF;
			} /*endif*/
			pix_DrawString( XtDisplay(w), XtWindow(w), calv_gc_grid,
				Nint(calh_xtrafo(sx1))+lx, 
				calv_wdw_height-Nint(calh_ytrafo(sy1))+ly,
				str, (int)strlen(str) );
		} /*endfor*/

} /* end of calh_plot_grid */



/*----------------------------------------------------------------------------*/



static void calh_terminate( Widget w, float *slo, float *az )

/* removes window from screen
 *
 * parameters of routine
 * Widget     w;        input; widget ID of calibration window
 * float      *slo;     output; final corrected slowness
 * float      *az;      output; final corrected azimuth
 */
{
	/* local variables */
	float    slowness;     /* corrected slowness */
	float    azimuth;      /* corrected azimuth */

	/* executable code */

	if  (calv_vec_x1 != 0.0 && calv_vec_y1 != 0.0)  {
		slowness = sqrt( calv_vec_x1*calv_vec_x1 + calv_vec_y1*calv_vec_y1 );
		azimuth = atan2( calv_vec_y1, calv_vec_x1 );
		azimuth = RAD2DEG( azimuth );
		azimuth = MATH2GEO( azimuth );
		if  (azimuth < 0.0)  azimuth += 360.0;
		if  (azimuth > 360.0)  azimuth -= 360.0;
	} else {
		slowness = azimuth = 0.0;
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: corr slowness: %5.2f   corr azimuth: %5.1f\n",
			slowness, azimuth );

	*slo = slowness;
	*az = azimuth;

	if  (XtIsManaged(calv_root_wdw))  XtUnmanageChild(calv_root_wdw);

} /* end of calh_terminate */



/*----------------------------------------------------------------------------*/

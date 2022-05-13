
/* file animatrix.c
 *      ===========
 *
 * version 7, 22-Mar-2006
 *
 * animate matrices
 * K. Stammler, 31-Oct-97
 */



#include <stdio.h>
#include <math.h>
#include "../basecnst.h"
#include <Xm/Xm.h>
#include <Mrm/MrmPublic.h>
#undef BC_DEFINE_TRUE_FALSE
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_CPAR
#include "../motif/pixmaps.h"
#include "../tcusrdef.h"

#define MY_PI 3.14159265358979323846
#define MY_E 2.7182818284590452354



#define w_toplevel 0
#define w_main 1
#define w_draw 2
#define w_menu_main 5
#define w_entry_quit 51
#define w_entry_plot 52
#define w_entry_plot_log 53
#define w_entry_plot_p2 54
#define w_entry_plot_p4 55
#define w_entry_radial_grid 56
#define w_entry_colors_green_red 57
#define w_entry_colors_blue_red 58
#define w_entry_colors_black_white 59
#define w_entry_colors_blue_yellow 60
#define w_entry_readnext 61
#define w_entry_readprev 62
#define w_entry_readloop 63
#define MmcMAX_WIDGET 100

#define MmcMAX_COLOR 101
#define MmcMAXCMT 16

#define MmcCHARHEIGHT 30

#define MmcCOLORS_GREEN_RED 1
#define MmcCOLORS_BLUE_RED 2
#define MmcCOLORS_BLACK_WHITE 3
#define MmcCOLORS_BLUE_YELLOW 4




/* local types */

typedef struct {
	float     **val;        /* pointer to matrix */
	int       width;        /* width of matrix */
	int       height;       /* height of matrix */
	float     minval;       /* minimum value */
	float     maxval;       /* maximum value */
} MmtMATRIX;

typedef struct {
	int       colnum;       /* number of colors */
	unsigned  margin_l;     /* left margin */
	unsigned  margin_r;     /* right margin */
	unsigned  margin_t;     /* top margin */
	unsigned  margin_b;     /* bottom margin */
} MmtDSP_SETUP;



/* global variables */
static char		*mmv_vec[]={"animatrix.uid"};   /* MRM database file list */
static Display *mmv_display;                   /* display */
static Widget  mmv_w[MmcMAX_WIDGET];           /* widget array */
static char    mmv_pname[BC_FILELTH+1];        /* program name */
static MmtMATRIX         mmv_matrix;           /* matrix */
static MmtDSP_SETUP      mmv_setup;            /* display setup */
static GC                mmv_gc[MmcMAX_COLOR]; /* color gc's */
static GC                mmv_cleargc;          /* for erasing */

static char    mmv_cmt[MmcMAXCMT][BC_LINELTH+1];  /* comment lines */
static int     mmv_cmtcnt;                        /* comment counter */
static float   mmv_scale;                         /* scale factor */
static float   mmv_yscale;                        /* y-scale factor (cart.) */
static BOOLEAN mmv_pixinit=FALSE;                 /* pixel buffer initialized */
static BOOLEAN mmv_grid_on=TRUE;                  /* draw grid */
static BOOLEAN mmv_cartesian;                     /* cartesian coo. system */
static char    mmv_abstime[cBcTimeLth+1];         /* absolute time */
static float   mmv_ymin;                          /* y start value (cart) */
static BOOLEAN mmv_square_it;                     /* square input data */
static char    mmv_inpfile[BC_FILELTH+1];         /* name of input file */
static int     mmv_fcnt;                          /* file counter */
static int     mmv_stepnum=100;                   /* number of steps */
static int     mmv_sleep=0;                       /* sleep time in us */
static float   mmv_relpow=1.0;                    /* relative power of FK */
static float   mmv_peakqual=0.0;                  /* FK peak quality number */
static float   mmv_colscale=1.0;                  /* color scaling based on relpow */
static BOOLEAN mmv_powscale=FALSE;                /* scale with rel. power */
static float   mmv_minrelpow=1.1;                 /* min. relpow for detection */
static float   mmv_minpeakqual=1.0e20;            /* min. peakqual for detect. */



/* prototypes */
static void cb_call_create_widget( Widget widget, int *tag,
	XmAnyCallbackStruct *data );
static void cb_call_activate( Widget widget, int *tag,
	XmToggleButtonCallbackStruct *data );
void cb_action_motion_rad( Widget w, XEvent *ev, String *params,
	Cardinal *parno );
void cb_action_motion_cart( Widget w, XEvent *ev, String *params,
	Cardinal *parno );
static void cb_call_expose( Widget widget, char *tag,
	XmDrawingAreaCallbackStruct *data );

static void mm_prepare_gc( Widget w, MmtDSP_SETUP *setup, GC gc[] );
static void mm_read_matrix( char filename[], BOOLEAN sqr, MmtMATRIX *mat );
static void mm_display_matrix( Widget w, GC gc[], MmtDSP_SETUP *setup,
	MmtMATRIX *mat, int power );
static void mm_draw_radial_grid( Widget w );
static void mm_change_gc( Widget w, int mode, MmtDSP_SETUP *setup, GC gc[] );


/* routines to register */
static MrmCount         mmv_regnum = 3 ;
static MrmRegisterArg   mmv_regvec[] = {
	{ "call_create_widget", (caddr_t)cb_call_create_widget },
	{ "call_activate", (caddr_t)cb_call_activate },
	{ "call_expose", (caddr_t)cb_call_expose }
};



/* action routines */
static   XtActionsRec mmv_new_actions_rad[] = {
	{"cb_action_motion_rad", cb_action_motion_rad}
};
static   XtActionsRec mmv_new_actions_cart[] = {
	{"cb_action_motion_cart", cb_action_motion_cart}
};



int main( int argc, char *argv[] )
{
	/* local variables */
	MrmHierarchy      hierarchy;        /* hierarchy */
	MrmCode           class;            /* class */
	XtAppContext      app_context;      /* application contex */
	Arg arglist[2];
	int n;
	int               pix_w, pix_h;     /* size of window in pixel */
	char              fname[cBcFileLth+1]; /* filename to use */
	int               i;                /* counter */
	TSyBoolean        interactive=TRUE; /* interactive mode */

	/* executable code */

	pa_init( argc, argv );
	pix_w = pix_h = 0;

	if  (pa_pnumber() < 4)  {
		fprintf( stderr, "*** Usage: %s <file-prefix> <colnum> <maxslow> <firstnum> [<loopcnt>]***\n",
			argv[0] );
		fprintf( stderr, "      parameters:\n" );
		fprintf( stderr, "      <file-prefix>     file prefix of fk files (without number)\n" );
		fprintf( stderr, "      <colnum>          number of colours used\n" );
		fprintf( stderr, "      <maxslow>         maximum slowness of FK spectra\n" );
		fprintf( stderr, "      <firstnum>        starting number of FK file\n" );
		fprintf( stderr, "      <loopcnt>         number of files to loop on loop menu entry\n" );
		fprintf( stderr, "      qualifiers:\n" );
		fprintf( stderr, "      -w=<pixwidth>     width of display window in pixel\n" );
		fprintf( stderr, "      -h=<pixheight>    height of display window in pixel\n" );
		fprintf( stderr, "      -r=<pixmargin>    right margin pixels\n" );
		fprintf( stderr, "      -noninter         noninteractive mode, immediately start loop\n" );
		fprintf( stderr, "      -sleep=<microsec> sleep time between pictures\n" );
		fprintf( stderr, "      -nogrid           no slowness/azimuth grid lines\n" );
		fprintf( stderr, "      -powscale         scale colours by rel. power (>0.5=full)\n" );
		fprintf( stderr, "      -square           square spectra before plotting\n" );
		fprintf( stderr, "      -minrelpow=<rp>   minimum rel power for detection\n" );
		fprintf( stderr, "      -minpeakqual=<q>  minimum peak quality for detection\n" );
		return 1;
	} /*endif*/

	/* set margins */
	mmv_setup.margin_l = 20;
	mmv_setup.margin_r = 320;
	mmv_setup.margin_t = 20;
	mmv_setup.margin_b = 20;

	/* strcpy( mmv_pname, argv[0] ); */
	strcpy( mmv_pname, "animatrix" );
	strcpy( mmv_inpfile, pa_pvalue(1) );
	sscanf( pa_pvalue(2), "%d", &mmv_setup.colnum );
	sscanf( pa_pvalue(3), "%f", &mmv_scale );
	sscanf( pa_pvalue(4), "%d", &mmv_fcnt );
	if  (pa_pnumber() >= 5)
		sscanf( pa_pvalue(5), "%d", &mmv_stepnum );
	if  (mmv_setup.colnum < 2)  mmv_setup.colnum = 2;
	if  (mmv_setup.colnum > 500)  mmv_setup.colnum = 500;
	if  (pa_qspecified("-w"))  {
		sscanf( pa_qvalue("-w"), "%d", &pix_w );
		if  (pix_w < 400)  pix_w = 400;
	} /*endif*/
	if  (pa_qspecified("-h"))  {
		sscanf( pa_qvalue("-h"), "%d", &pix_h );
		if  (pix_h < 400)  pix_h = 400;
	} /*endif*/
	if  (pa_qspecified("-r"))
		sscanf( pa_qvalue("-r"), "%d", &mmv_setup.margin_r );
	if  (pa_qspecified("-nogrid"))  mmv_grid_on = FALSE;
	if  (pa_qspecified("-noninter"))  interactive = FALSE;
	if  (pa_qspecified("-powscale"))  mmv_powscale = TRUE;
	mmv_square_it = FALSE;
	if  (pa_qspecified("-square"))  mmv_square_it = TRUE;
	mmv_cartesian = FALSE;
	if  (pa_qspecified("-ymax"))  {
		mmv_cartesian = TRUE;
		sscanf( pa_qvalue("-ymax"), "%f", &mmv_yscale );
	} /*endif*/
	mmv_ymin = 0.0;
	if  (pa_qspecified("-ymin"))
		sscanf( pa_qvalue("-ymin"), "%f", &mmv_ymin );
	if  (pa_qspecified("-minrelpow"))
		sscanf( pa_qvalue("-minrelpow"), "%f", &mmv_minrelpow );
	if  (pa_qspecified("-minpeakqual"))
		sscanf( pa_qvalue("-minpeakqual"), "%f", &mmv_minpeakqual );
	*mmv_abstime = '\0';
	if  (pa_qspecified("-xtime"))
		strcpy( mmv_abstime, pa_qvalue("-xtime") );
	if  (pa_qspecified("-sleep"))  {
		sscanf( pa_qvalue("-sleep"), "%d", &mmv_sleep );
		if  (mmv_sleep > 1000000)  mmv_sleep = 1000000;
	} /*endif*/

	if  (mmv_ymin != 0.0)
		mmv_yscale -= mmv_ymin;

	MrmInitialize ();
	XtToolkitInitialize();
	app_context = XtCreateApplicationContext();
	mmv_display = XtOpenDisplay(app_context, NULL, mmv_pname, "mapmatrix",
		NULL, 0, &argc, argv);
	if (mmv_display == NULL) {
		fprintf(stderr, "%s:  Can't open display\n", mmv_pname );
		exit(1);
	} /*endif*/

	if  (mmv_cartesian)
		XtAppAddActions( app_context, mmv_new_actions_cart, 1 );
	else
		XtAppAddActions( app_context, mmv_new_actions_rad, 1 );

	n = 0;
	/* XtSetArg(arglist[n], XmNallowShellResize, True);  n++; */
	mmv_w[w_toplevel] = XtAppCreateShell( mmv_pname, NULL,
		applicationShellWidgetClass, mmv_display, arglist, n);

	if  (MrmOpenHierarchy(1,mmv_vec,NULL,&hierarchy) != MrmSUCCESS) {
		fprintf ( stderr, "can't open hierarchy\n" );
		exit( 1 );
	} /*endif*/

	if  (MrmRegisterNames(mmv_regvec,mmv_regnum) != MrmSUCCESS)  {
		fprintf( stderr, "can't register names\n" );
		exit( 1 );
	} /*endif*/

	if (MrmFetchWidget(hierarchy,"main_window",mmv_w[w_toplevel],
		mmv_w+w_main,&class) != MrmSUCCESS)  {
		fprintf( stderr, "can't fetch widget\n" );
		exit( 1 );
	} /*endif*/

	n = 0;
	if  (pix_w > 0)  {XtSetArg( arglist[n], XmNwidth, pix_w ); n++;}
	if  (pix_h > 0)  {XtSetArg( arglist[n], XmNheight, pix_h ); n++;}
	if  (n > 0)  XtSetValues( mmv_w[w_draw], arglist, n );
	/*if  (n > 0)  XtSetValues( mmv_w[w_main], arglist, n );*/

	XtManageChild( mmv_w[w_main] );
	XtRealizeWidget( mmv_w[w_toplevel] );

	mmv_cleargc = XCreateGC( XtDisplay(mmv_w[w_draw]),
		XtWindow(mmv_w[w_draw]), 0, NULL );
	XSetForeground( XtDisplay(mmv_w[w_draw]), mmv_cleargc,
		WhitePixel(XtDisplay(mmv_w[w_draw]),0) );
	XSetBackground( XtDisplay(mmv_w[w_draw]), mmv_cleargc,
		WhitePixel(XtDisplay(mmv_w[w_draw]),0) );

	/* read in data file */
	mmv_matrix.val = NULL;
	sprintf( fname, "%s%05d.out", mmv_inpfile, mmv_fcnt );
	mm_read_matrix( fname, mmv_square_it, &mmv_matrix );
	mm_prepare_gc( mmv_w[w_draw], &mmv_setup, mmv_gc );
	mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, 1 ); 
	if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );

	if  (interactive)  {
		XtAppMainLoop(app_context);
	} else {
		for  (i=0; i<mmv_stepnum; i++)  {
			/* read in data file */
			sprintf( fname, "%s%05d.out", mmv_inpfile, ++mmv_fcnt );
		mm_read_matrix( fname, mmv_square_it, &mmv_matrix );
			mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, 1); 
			if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );
			if  (mmv_sleep > 0)  usleep( mmv_sleep );
		} /*endfor*/
	} /*endif*/

	return 0;

} /* end of main */



/*--------------------------------------------------------------------------*/



static void cb_call_create_widget( Widget widget, int *tag,
	XmAnyCallbackStruct *data )

/* Callback routine on createing widgets
 *
 * parameters of routine
 * Widget     widget;          input; widget number of drawing area
 * int        *tag;            input; widget number
 * XmAnyCallbackStruct *data;  input; not used here
 */
{
	/* local variables */
	int      wno = *tag;                 /* widget number */
	char     acttable[BC_LONGSTRLTH+1];  /* action table */
	XtTranslations new_table;            /* translated table */
	Arg      al[1];

	/* executable code */

	if  (wno >= MmcMAX_WIDGET || wno < 0)  {
		fprintf( stderr, "--> illegal widget number %d\n", wno );
		return;
	} /*endif*/

	/* printf( "[%d]", wno ); */
	mmv_w[wno] = widget;

	if  (wno == w_draw)  {
		if  (mmv_cartesian)  {
			strcpy( acttable, "<Motion>: cb_action_motion_cart()\n" );
			strcat( acttable, "<Btn1Down>: cb_action_motion_cart()\n" );
			strcat( acttable, "<Btn1Up>: cb_action_motion_cart()\n" );
			strcat( acttable, "<Btn2Down>: cb_action_motion_cart()\n" );
			strcat( acttable, "<Btn2Up>: cb_action_motion_cart()\n" );
			strcat( acttable, "<Btn3Down>: cb_action_motion_cart()\n" );
			strcat( acttable, "<Btn3Up>: cb_action_motion_cart()\n" );
			strcat( acttable, "<KeyPress>: cb_action_motion_cart()\n" );
		} else {
			strcpy( acttable, "<Motion>: cb_action_motion_rad()\n" );
			strcat( acttable, "<Btn1Down>: cb_action_motion_rad()\n" );
			strcat( acttable, "<Btn1Up>: cb_action_motion_rad()\n" );
			strcat( acttable, "<Btn2Down>: cb_action_motion_rad()\n" );
			strcat( acttable, "<Btn2Up>: cb_action_motion_rad()\n" );
			strcat( acttable, "<Btn3Down>: cb_action_motion_rad()\n" );
			strcat( acttable, "<Btn3Up>: cb_action_motion_rad()\n" );
			strcat( acttable, "<KeyPress>: cb_action_motion_rad()\n" );
		} /*endif*/
		new_table = XtParseTranslationTable( acttable );
		XtSetArg( al[0], XmNtranslations, new_table );
		XtSetValues( mmv_w[w_draw], al, 1 );
	} /*endif*/

} /* end of cb_call_create_widget */



/*--------------------------------------------------------------------------*/



static void cb_call_activate( Widget widget, int *tag,
	XmToggleButtonCallbackStruct *data )

/* Callback routine on button selection
 *
 * parameters of routine
 * Widget     widget;          input; widget number of drawing area
 * int        *tag;            input; tag entry
 * XmPushButtonCallbackStruct *data;  input; 
 */
{
	/* local variables */
	static int power=1;           /* current power */
	STATUS   locstat;             /* local status */
	char     fname[cBcFileLth+1]; /* filename to read data */
	int      i;                   /* counter */

	/* executable code */

	if  (!mmv_pixinit)  {
		locstat = BC_NOERROR;
		pix_create_window_buffer( XtDisplay(mmv_w[w_draw]),
			XtWindow(mmv_w[w_draw]), TRUE, &locstat );
		if  (Severe(&locstat))  {
			fprintf( stderr, "mapmatrix: error creating window buffer\n" );
			exit( 1 );
		} /*endif*/
		mmv_pixinit = TRUE;
	} /*endif*/

	switch  (*tag)  {
	case w_entry_quit:
		exit( 0 );
	case w_entry_readnext:
		/* read in data file */
		sprintf( fname, "%s%05d.out", mmv_inpfile, ++mmv_fcnt );
		mm_read_matrix( fname, mmv_square_it, &mmv_matrix );
		/*mm_prepare_gc( mmv_w[w_draw], &mmv_setup, mmv_gc );*/
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, 1 ); 
		if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );
		break;
	case w_entry_readprev:
		/* read in data file */
		sprintf( fname, "%s%05d.out", mmv_inpfile, --mmv_fcnt );
		mm_read_matrix( fname, mmv_square_it, &mmv_matrix );
		/*mm_prepare_gc( mmv_w[w_draw], &mmv_setup, mmv_gc );*/
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, 1 ); 
		if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );
		break;
	case w_entry_readloop:
		for  (i=0; i<mmv_stepnum; i++)  {
			/* read in data file */
			sprintf( fname, "%s%05d.out", mmv_inpfile, ++mmv_fcnt );
			mm_read_matrix( fname, mmv_square_it, &mmv_matrix );
			/*mm_prepare_gc( mmv_w[w_draw], &mmv_setup, mmv_gc );*/
			mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, 1); 
			if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );
			if  (mmv_sleep > 0)  usleep( mmv_sleep );
		} /*endfor*/
		break;
	case w_entry_plot:
		power=1;
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, 1 ); 
		if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );
		break;
	case w_entry_plot_log:
		power = 0;
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, 0 );
		if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );
		break;
	case w_entry_plot_p2:
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix,
			++power );
		if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );
		break;
	case w_entry_plot_p4:
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix,
			--power );
		if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );
		break;
	case w_entry_radial_grid:
		mmv_grid_on = !mmv_grid_on;
		if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );
		else mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix,
			power );
		break;
	case w_entry_colors_green_red:
		mm_change_gc( mmv_w[w_draw], MmcCOLORS_GREEN_RED, &mmv_setup, mmv_gc );
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, power);
		if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );
		break;
	case w_entry_colors_blue_red:
		mm_change_gc( mmv_w[w_draw], MmcCOLORS_BLUE_RED, &mmv_setup, mmv_gc );
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, power);
		if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );
		break;
	case w_entry_colors_black_white:
		mm_change_gc( mmv_w[w_draw], MmcCOLORS_BLACK_WHITE, &mmv_setup, mmv_gc );
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, power);
		if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );
		break;
	case w_entry_colors_blue_yellow:
		mm_change_gc( mmv_w[w_draw], MmcCOLORS_BLUE_YELLOW, &mmv_setup, mmv_gc );
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, power);
		if  (mmv_grid_on)  mm_draw_radial_grid( mmv_w[w_draw] );
		break;
	default:
		fprintf( stderr, "illegal widget activated (%d)\n", *tag );
		return;
	} /*endswitch*/

} /* end of cb_call_activate */



/*--------------------------------------------------------------------------*/



static void cb_call_expose( Widget widget, char *tag,
	XmDrawingAreaCallbackStruct *data )

/* Callback routine on creation of drawing area
 *
 * parameters of routine
 * Widget     widget;          input; widget number of drawing area
 * char       *tag;            input; not used here
 * XmDrawingAreaCallbackStruct *data;  input; 
 */
{
	/* local variables */

	/* executable code */

	if  (mmv_pixinit)
		pix_manage_exposure( &(data->event->xexpose) );

} /* end of cb_call_expose */



/*--------------------------------------------------------------------------*/


#define POSTEXT_X 10
#define POSTEXT_Y 10



void cb_action_motion_rad( Widget w, XEvent *ev, String *params,
	Cardinal *parno )

/* action routine for pointer motion
 *
 * parameters of routine
 * Widget     w;      input; widget ID
 * XEvent     *ev;    input; current event
 * ...   (don't know much about it)
 */
{
	/* local variables */
	static BOOLEAN is_init=FALSE;       /* already initialized */
	static int     pos_x, pos_y;        /* draw position */
	static GC      xgc;                 /* clear GC */
	static char    oldstr[BC_LINELTH+1];/* old string */
	static char    oldstr2[BC_LINELTH+1];/* old string */
	static int     eff_w, eff_h;        /* effective size of window */
	char     postext[BC_LINELTH+1];     /* position text */
	char     postext2[BC_LINELTH+1];    /* position text */
	Window   root;                      /* root window */
	int      w_x, w_y;                  /* window position */
	unsigned w_w, w_h;                  /* size of window */
   unsigned border, depth;             /* window pars (not used) */
	float    slow_x, slow_y;            /* slowness position */
	float    slowness, azimuth;         /* slowness and azimuth */
	float    veloc;                     /* current velocity */

	/* executable code */

	if  (w != mmv_w[w_draw])  {printf("--> motion: ill wdw\n"); return;}

	if  (!is_init)  {
		XGetGeometry( XtDisplay(w), XtWindow(w), &root, &w_x, &w_y, &w_w, &w_h,
			&border, &depth );
		pos_x = w_w - mmv_setup.margin_r + POSTEXT_X;
		pos_y = w_h - mmv_setup.margin_b - POSTEXT_Y;
		eff_w = w_w - mmv_setup.margin_l - mmv_setup.margin_r;
		eff_h = w_h - mmv_setup.margin_t - mmv_setup.margin_b;
		xgc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
		/* XSetForeground( XtDisplay(w), xgc, BlackPixel(XtDisplay(w),0) ); */
		/* XSetBackground( XtDisplay(w), xgc, WhitePixel(XtDisplay(w),0) ); */
		XCopyGC( XtDisplay(w), mmv_gc[mmv_setup.colnum], GCFont, xgc );
		XSetForeground( XtDisplay(w), xgc, WhitePixel(XtDisplay(w),0) );
		XSetBackground( XtDisplay(w), xgc, BlackPixel(XtDisplay(w),0) );
		/* XSetFunction( XtDisplay(w), xgc, GXxor ); */
		is_init = TRUE;
	} /*endif*/

	if  (ev->type == MotionNotify)  {
		if  (ev->xmotion.x < mmv_setup.margin_l
			|| ev->xmotion.y < mmv_setup.margin_t
			|| ev->xmotion.x > mmv_setup.margin_l+eff_w
			|| ev->xmotion.y > mmv_setup.margin_t+eff_h)  {
			*postext = *postext2 = '\0';
		} else {
			slow_x = (float)(ev->xmotion.x - mmv_setup.margin_l);
			slow_y = (float)(ev->xmotion.y - mmv_setup.margin_t);
			slow_x = slow_x / (float)eff_w * mmv_scale * 2.0 - mmv_scale;
			slow_y = -(slow_y / (float)eff_h * mmv_scale * 2.0 - mmv_scale);
			slowness = sqrt( slow_x*slow_x + slow_y*slow_y );
			if  (slow_x == 0.0 && slow_y == 0.0)  azimuth = 0.0;
			else azimuth = atan2( slow_x, slow_y ) / (2.0*MY_PI) * 360.0;
			if  (azimuth < 0.0)  azimuth += 360.0;
			if  (slowness > 0.0)  veloc = 1.0 / slowness;
			sprintf( postext, " curr. slowness  : %6.2f,%6.2f", slow_x, slow_y );
			sprintf( postext2, " vel, slo, az: %6.2f %6.2f  %6.2f",
				veloc, slowness, azimuth );
		} /*endif*/
		if  (*oldstr != '\0')  {
			pix_DrawString( XtDisplay(w), XtWindow(w), xgc,
				pos_x, pos_y, oldstr, strlen(oldstr) );
			pix_DrawString( XtDisplay(w), XtWindow(w), xgc,
				pos_x, pos_y+20, oldstr2, strlen(oldstr2) );
		} /*endif*/
		if  (*postext != '\0')  {
			pix_DrawString( XtDisplay(w), XtWindow(w), mmv_gc[mmv_setup.colnum],
				pos_x, pos_y, postext, strlen(postext) );
			pix_DrawString( XtDisplay(w), XtWindow(w), mmv_gc[mmv_setup.colnum],
				pos_x, pos_y+20, postext2, strlen(postext2) );
		} /*endif*/
		strcpy( oldstr, postext );
		strcpy( oldstr2, postext2 );
		XFlush( XtDisplay(w) );
	} /*endif*/

	if  (ev->type == KeyPress)  {
		/* this is a workaround for accelerators not working on recent Suse  *
       * Linux implementations                                             */
		int tag;
		Modifiers mret;
		KeySym keysym;
		XtTranslateKeycode( ev->xkey.display, ev->xkey.keycode, ev->xkey.state,
			&mret, &keysym );
		switch  ((char)keysym)  {
		case 'q':   tag = w_entry_quit;                 break;
		case 'b':   tag = w_entry_readprev;             break;
		case 'n':   tag = w_entry_readnext;             break;
		case 'f':   tag = w_entry_readloop;             break;
		case 'p':   tag = w_entry_plot;                 break;
		case 'l':   tag = w_entry_plot_log;             break;
		case 'u':   tag = w_entry_plot_p2;              break;
		case 'd':   tag = w_entry_plot_p4;              break;
		case 'r':   tag = w_entry_radial_grid;          break;
		case '1':   tag = w_entry_colors_green_red;     break;
		case '2':   tag = w_entry_colors_blue_red;      break;
		case '3':   tag = w_entry_colors_black_white;   break;
		case '4':   tag = w_entry_colors_blue_yellow;   break;
		default:    tag = 0;
			printf( "--> pressed key %d, keysym %x\n", ev->xkey.keycode, keysym );
		} /*endswitch*/
		if  (tag > 0)  cb_call_activate( w, &tag, NULL );
	} /*endif*/

} /* end of cb_action_motion_rad */



/*--------------------------------------------------------------------------*/




void cb_action_motion_cart( Widget w, XEvent *ev, String *params,
	Cardinal *parno )

/* action routine for pointer motion
 *
 * parameters of routine
 * Widget     w;      input; widget ID
 * XEvent     *ev;    input; current event
 * ...   (don't know much about it)
 */
{
	/* local variables */
	static BOOLEAN is_init=FALSE;       /* already initialized */
	static int     pos_x, pos_y;        /* draw position */
	static GC      xgc;                 /* clear GC */
	static char    oldstr[BC_LINELTH+1];/* old string */
	static char    oldstr2[BC_LINELTH+1];/* old string */
	static int     eff_w, eff_h;        /* effective size of window */
	char     postext[BC_LINELTH+1];     /* position text */
	char     postext2[BC_LINELTH+1];    /* position text */
	Window   root;                      /* root window */
	int      w_x, w_y;                  /* window position */
	unsigned w_w, w_h;                  /* size of window */
   unsigned border, depth;             /* window pars (not used) */
	float    slow_x, slow_y;            /* slowness position */
	char     ctime[cBcTimeLth+1];       /* current absolute time */
	STATUS   locstat;                   /* local status */

	/* executable code */

	if  (w != mmv_w[w_draw])  {printf("--> motion: ill wdw\n"); return;}

	if  (!is_init)  {
		XGetGeometry( XtDisplay(w), XtWindow(w), &root, &w_x, &w_y, &w_w, &w_h,
			&border, &depth );
		pos_x = w_w - mmv_setup.margin_r + POSTEXT_X;
		pos_y = w_h - mmv_setup.margin_b - POSTEXT_Y;
		eff_w = w_w - mmv_setup.margin_l - mmv_setup.margin_r;
		eff_h = w_h - mmv_setup.margin_t - mmv_setup.margin_b;
		xgc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
		/* XSetForeground( XtDisplay(w), xgc, BlackPixel(XtDisplay(w),0) ); */
		/* XSetBackground( XtDisplay(w), xgc, WhitePixel(XtDisplay(w),0) ); */
		XCopyGC( XtDisplay(w), mmv_gc[mmv_setup.colnum], GCFont, xgc );
		XSetForeground( XtDisplay(w), xgc, WhitePixel(XtDisplay(w),0) );
		XSetBackground( XtDisplay(w), xgc, BlackPixel(XtDisplay(w),0) );
		/* XSetFunction( XtDisplay(w), xgc, GXxor ); */
		is_init = TRUE;
	} /*endif*/

	if  (ev->type == MotionNotify)  {
		if  (ev->xmotion.x < mmv_setup.margin_l
			|| ev->xmotion.y < mmv_setup.margin_t
			|| ev->xmotion.x > mmv_setup.margin_l+eff_w
			|| ev->xmotion.y > mmv_setup.margin_t+eff_h)  {
			*postext = *postext2 = '\0';
		} else {
			slow_x = (float)(ev->xmotion.x - mmv_setup.margin_l);
			slow_y = (float)(ev->xmotion.y - mmv_setup.margin_t);
			slow_x = slow_x / (float)eff_w * mmv_scale;
			slow_y = mmv_ymin + mmv_yscale - (slow_y / (float)eff_h * mmv_yscale);
			locstat = cBcNoError;
			if  (*mmv_abstime == '\0')  {
				sprintf( postext,  " curr. time: %6.2f", slow_x );
			} else {
				tc_tadd( mmv_abstime, slow_x, ctime, &locstat );
				/*sprintf( postext, " curr. time: %s", ctime );*/
				strcpy( postext, ctime );
			} /*endif*/
			sprintf( postext2, " slowness  : %6.2f", slow_y );
		} /*endif*/
		if  (*oldstr != '\0')  {
			pix_DrawString( XtDisplay(w), XtWindow(w), xgc,
				pos_x, pos_y, oldstr, strlen(oldstr) );
			pix_DrawString( XtDisplay(w), XtWindow(w), xgc,
				pos_x, pos_y+20, oldstr2, strlen(oldstr2) );
		} /*endif*/
		if  (*postext != '\0')  {
			pix_DrawString( XtDisplay(w), XtWindow(w), mmv_gc[mmv_setup.colnum],
				pos_x, pos_y, postext, strlen(postext) );
			pix_DrawString( XtDisplay(w), XtWindow(w), mmv_gc[mmv_setup.colnum],
				pos_x, pos_y+20, postext2, strlen(postext2) );
		} /*endif*/
		strcpy( oldstr, postext );
		strcpy( oldstr2, postext2 );
		XFlush( XtDisplay(w) );
	} /*endif*/

	if  (ev->type == KeyPress)  {
		/* this is a workaround for accelerators not working on recent Suse  *
       * Linux implementations                                             */
		int tag;
		Modifiers mret;
		KeySym keysym;
		XtTranslateKeycode( ev->xkey.display, ev->xkey.keycode, ev->xkey.state,
			&mret, &keysym );
		switch  ((char)keysym)  {
		case 'n':   tag = w_entry_readnext;        break;
		default:    tag = 0;
			printf( "--> pressed key %d, keysym %ld\n", ev->xkey.keycode, keysym );
		} /*endswitch*/
		if  (tag > 0)  cb_call_activate( w, &tag, NULL );
	} /*endif*/

} /* end of cb_action_motion_cart */



/*--------------------------------------------------------------------------*/



#define TMPNAME "animatrix.000"
#define TMPNAMEGZ "animatrix.000.gz"



static void mm_read_matrix( char filename[], BOOLEAN sqr, MmtMATRIX *mat )

/* Allocates memory for matrix and reads it from file
 *
 * parameters of routine
 * char       filename[];        input; name of input file
 * BOOLEAN    sqr;               input; square input
 * MmtMATRIX  *mat;              output; matrix
 */
{
	/* local variables */
	FILE     *fp;                     /* pointer to input file */
	char     line[BC_LINELTH+1];      /* current line in input file */
	int      i, j;                    /* counters */
	char     fname[cBcFileLth+1];     /* name of input file */
	char     uname[cBcFileLth+1];     /* name of used file */
	BOOLEAN  compressed;              /* file is compressed */
	char     syscmd[cBcLongStrLth+1]; /* shell command */

	/* executable code */

	strcpy( fname, filename );

	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		strcat( fname, ".gz" );
		fp = fopen( fname, "r" );
		if  (fp == NULL)  {
			fprintf( stderr, "%s: couldn't open input file %s\n",
				mmv_pname, fname );
			return;
		} /*endif*/
		fclose( fp );
		compressed = TRUE;
		sprintf( syscmd, "cp %s %s", fname, TMPNAMEGZ );
		system( syscmd );
		sprintf( syscmd, "gzip -d %s", TMPNAME );
		system( syscmd );
/*#		ifdef SH_SETUP_LINUX*/
		strcpy( uname, TMPNAME );
/*
   #		else
		strcpy( uname, filename );
   #		endif
*/
		fp = fopen( uname, "r" );
		if  (fp == NULL)  {
			fprintf( stderr, "%s: cannot open decompressed file of %s (%s)\n",
				mmv_pname, fname, uname );
			return;
		} /*endif*/
	} else {
		compressed = FALSE;
	} /*endif*/

	/* read off comments and get relpow and peakqual if specified */
	mmv_cmtcnt = 0;
	mmv_relpow = 0.5;
	mmv_peakqual = 0.0;
	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (*line != '!')  break;
		if  (strncmp(line,"!* rel. power      :",20) == 0)  {
			sscanf( line+20, "%f", &mmv_relpow );
			if  (mmv_relpow < 0.0)  {
				mmv_colscale = 0.0;
			} else if  (mmv_relpow > 0.5)  {
				mmv_colscale = 1.0;
			} else {
				mmv_colscale = 2.0 * mmv_relpow;
			} /*endif*/
		} else if  (strncmp(line,"!* peakqual        :",20) == 0)  {
			sscanf( line+20, "%f", &mmv_peakqual );
		} /*endif*/
		if  (line[1] == '*' && mmv_cmtcnt < MmcMAXCMT)  {
			strcpy( mmv_cmt[mmv_cmtcnt], line+2 );
			i = strlen( mmv_cmt[mmv_cmtcnt] ) - 1;
			if  (mmv_cmt[mmv_cmtcnt][i] == '\n')
				mmv_cmt[mmv_cmtcnt][i] = '\0';
			if  (mmv_cmtcnt == 0 && i < (cBcLineLth-10))
				sprintf( mmv_cmt[0]+i, " (#%d)", mmv_fcnt );
			mmv_cmtcnt++;
		} /*endif*/
	} /*endwhile*/

	if  (sscanf(line,"%d %d",&(mat->width),&(mat->height)) != 2)  {
		fprintf( stderr, "%s: read error on input file %s\n",
			mmv_pname, fname );
		exit( 1 );
	} /*endif*/

	/* allocate pointers */
	if  (mat->val != NULL)  free( mat->val );
	mat->val = (float **)malloc( sizeof(float *) * mat->height );
	if  (mat->val == NULL)  {
		fprintf( stderr, "%s: allocation error (pointers)\n", mmv_pname );
		exit( 1 );
	} /*endif*/
	/* allocate rows */
	for  (i=0; i<(mat->height); i++)  {
		mat->val[i] = (float *)malloc( sizeof(float) * mat->width );
		if  (mat->val[i] == NULL)  {
			fprintf( stderr, "%s: allocation error (row %d)\n", mmv_pname, i );
			exit( 1 );
		} /*endif*/
	} /*endfor*/

	/* read in data */
	for  (i=0; i<(mat->height); i++)
		for  (j=0; j<(mat->width); j++)  {
			fscanf( fp, "%f\n", (mat->val[i])+j );
			if  (sqr)  mat->val[i][j] = (mat->val[i][j])*(mat->val[i][j]);
		} /*endfor*/

	fclose( fp );
	if  (compressed)  {
		sprintf( syscmd, "\\rm %s", uname );
		system( syscmd ); 
	} /*endif*/

	/* find min & max, test output */
	mat->minval = mat->maxval = mat->val[0][0];
	for  (i=0; i<(mat->height); i++)  {
		for  (j=0; j<(mat->width); j++)  {
			/* printf( "%f ", mat->val[i][j] ); */
			if  (mat->val[i][j] > mat->maxval)  mat->maxval = mat->val[i][j];
			if  (mat->val[i][j] < mat->minval)  mat->minval = mat->val[i][j];
		} /*endfor*/
		/* printf( "\n" ); */
	} /*endfor*/

	/*printf( "--> min, max: %f %f\n", mat->minval, mat->maxval );*/

} /* end of mm_read_matrix */



/*--------------------------------------------------------------------------*/




static void mm_prepare_gc( Widget w, MmtDSP_SETUP *setup, GC gc[] )

/* creates GC's for display
 *
 * parameters of routine
 * Widget     w;          input; widget of drawing area
 * MmtDSP_SETUP *setup;   input; display setup
 * GC         gc[];       output; array of GC's
 */
{
	/* local variables */
	static char *visual_class[] = {
		"StaticGray", "GrayScale", "StaticColor", "PseudoColor",
		"TrueColor", "DirectColor"
	};
	int      default_depth;       /* default depth */
	Visual   *default_visual;     /* default visual */
	XColor   color;               /* color */
	Colormap default_cmap;        /* default colormap */
	XVisualInfo visual_info;      /* visual info */
	unsigned i;                   /* counter */
	float    frac;                /* color fraction */
	float    frac_x, c;           /* scratch */
	int      screen_num;          /* screen number */
	Font     out_font;            /* font for text */

	/* executable code */

	screen_num = 0;

	default_depth = DefaultDepth( XtDisplay(w), screen_num );
	default_visual = DefaultVisual( XtDisplay(w), screen_num );
	default_cmap = DefaultColormap( XtDisplay(w), screen_num );
	if  (default_depth == 1)  {
		fprintf( stderr, "%s: StaticGray visual not supported\n",
			mmv_pname );
		exit( 1 );
	} /*endif*/

	i = 5;
	while  (!XMatchVisualInfo(XtDisplay(w),screen_num,default_depth,
		i--,&visual_info))
		;

#ifdef XXX
	printf( "%s: found a %s class visual at default depth %d\n",
		mmv_pname, visual_class[++i] );
#endif
	if  (i < StaticColor)  {
		fprintf( stderr, "%s: visual class %s is not supported\n",
			mmv_pname, visual_class[i] );
		exit( 1 );
	} /*endif*/

#ifdef XXX
	if  (visual_info.visual != default_visual)
		printf( "%s: %s class visual at default depth is not default visual\n",
			mmv_pname, visual_class[i] );
#endif

	if  (setup->colnum > MmcMAX_COLOR-1)  {
		fprintf( stderr, "%s: too many colors.  Only %d permitted\n",
			mmv_pname, MmcMAX_COLOR-1 );
		exit( 1 );
	} /*endif*/

	for  (i=0; i<(setup->colnum); i++)  {
		frac = (float)i / (float)((setup->colnum)-1);
			/* red increases from 0 to c and then stays 1 */
		c = 0.5; /*0.4;*/
		frac_x = (frac < c) ? frac/c : 1.0;
		color.red = Nint( frac_x * 65535.0 );
		/* green increases from c to 1 and is zero before c */
		c = 0.5;
		frac_x = (frac > c) ? (frac-c)/(1.0-c) : 0.0;
		color.green = Nint( frac_x * 65535.0 );
		/* blue decreases from 1 to 0 between 0 and c and then stays 0 */
		c = 0.5; /*0.8;*/
		frac_x = (frac < c) ? (1.0-frac/c) : 0.0;
		color.blue = Nint( frac_x * 65535.0 );
		color.flags = DoRed | DoGreen | DoBlue;
		if  (!XAllocColor(XtDisplay(w),default_cmap,&color))  {
			fprintf( stderr, "%s: error allocating color\n", mmv_pname );
			exit( 1 );
		} /*endif*/
		gc[i] = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
		XSetForeground( XtDisplay(w), gc[i], color.pixel );
		XSetBackground( XtDisplay(w), gc[i],
			WhitePixel(XtDisplay(w),screen_num) );
	} /*endfor*/

	out_font = XLoadFont( XtDisplay(w),
		"-b&h-lucidatypewriter-bold-r-normal-sans-*-100-75-*-*-*-*-1" );

	i = setup->colnum;
	gc[i] = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetForeground( XtDisplay(w), gc[i], BlackPixel(XtDisplay(w),screen_num) );
	XSetBackground( XtDisplay(w), gc[i], WhitePixel(XtDisplay(w),screen_num) );
	XSetFont( XtDisplay(w), gc[i], out_font );

} /* end of mm_prepare_gc */



/*--------------------------------------------------------------------------*/



static void mm_display_matrix( Widget w, GC gc[], MmtDSP_SETUP *setup,
	MmtMATRIX *mat, int power )

/* displays matrix on screen
 *
 * parameters of routine
 * Widget     w;           input; widget of drawing area
 * GC         gc[];        input; GC's
 * MmtDSP_SETUP *setup;    input; display setup
 * MmtMATRIX  *mat;        input; matrix
 * int        power;       input; power on amplitudes
 */
{
	/* local variables */
	Window   root;        /* root window */
	int      w_x, w_y;    /* position of window */
	unsigned w_w, w_h;    /* size of window */
	unsigned border;      /* border width */
	unsigned depth;       /* depth of window */
	float    rect_width;  /* width of rectangle */
	float    rect_height; /* height of rectangle */
	unsigned i, j;        /* counters */
	float    rect_x, rect_y; /* position of rectangle */
	float    frac;        /* scratch */
	unsigned colstep;     /* color number */
	int      cmt_x, cmt_y;/* comment position */

	/* executable code */

	XGetGeometry( XtDisplay(w), XtWindow(w), &root, &w_x, &w_y, &w_w, &w_h,
		&border, &depth );

	/* get width and height of rectangle */
	rect_width = (float)(w_w-(setup->margin_l)-(setup->margin_r))
		/ (float)(mat->width);
#ifdef XXX
	if  (rect_width <= 1.0)  {
		fprintf( stderr, "%s: illegal window size\n", mmv_pname );
		exit( 1 );
	} /*endif*/
#endif
	rect_height = (float)(w_h-(setup->margin_t)-(setup->margin_b))
		/ (float)(mat->height);
#ifdef XXX
	if  (rect_height <= 1.0)  {
		fprintf( stderr, "%s: illegal window size\n", mmv_pname );
		exit( 1 );
	} /*endif*/
#endif

	cmt_x = w_w - setup->margin_r + 10;
	cmt_y = MmcCHARHEIGHT;
	if  (mmv_relpow > mmv_minrelpow && mmv_peakqual > mmv_minpeakqual)  {
		pix_FillRectangle( XtDisplay(w), XtWindow(w), gc[mmv_setup.colnum-1],
			cmt_x, cmt_y-MmcCHARHEIGHT, 300, 550 );
	} else {
		pix_FillRectangle( XtDisplay(w), XtWindow(w), mmv_cleargc,
			cmt_x, cmt_y-MmcCHARHEIGHT, 300, 550 );
	} /*endif*/
	for  (i=0; i<mmv_cmtcnt; i++)  {
		pix_DrawString( XtDisplay(w), XtWindow(w), gc[setup->colnum], cmt_x,
			cmt_y, mmv_cmt[i], strlen(mmv_cmt[i]) );
		cmt_y += MmcCHARHEIGHT;
	} /*endfor*/

	for  (i=0; i<(mat->height); i++)  {
		for  (j=0; j<(mat->width); j++)  {
#			ifdef XXX  /* this is original */
			/* rect_x = (float)(mat->width-1-i) * rect_width; this is mirrored */
			rect_x = (float)i * rect_width;
			rect_y = (float)j * rect_height;
#			endif
			rect_x = (float)j * rect_width;
			rect_y = (float)(mat->height-1-i) * rect_height;
			frac = (mat->val[i][j]-mat->minval) / (mat->maxval-mat->minval);
			if  (mmv_powscale)  frac *= mmv_colscale;
			if  (power == 0)  {
				frac = log( 1.0 + (MY_E-1.0)*frac );
			} else if  (power > 1)  {
				frac = pow( frac, (float)power );
			} else if  (power < 0)  {
				frac = pow( frac, 1.0/(float)(-power+1) );
			} /*endif*/
			colstep = Nint( frac * (float)(setup->colnum-1) );
			pix_FillRectangle( XtDisplay(w), XtWindow(w), gc[colstep],
				Nint(rect_x)+setup->margin_l, Nint(rect_y)+setup->margin_t,
				Nint(rect_width)+1, Nint(rect_height)+1 );
		} /*endfor*/
	} /*endfor*/

	XFlush( XtDisplay(w) );

} /* end of mm_display_matrix */



/*--------------------------------------------------------------------------*/



static void mm_draw_radial_grid( Widget w )

/* Draws radial grid into the map.
 *
 * parameters of routine
 * Widget     w;               input; widget ID
 */
{
	/* local variables */
	Window   root;        /* root window */
	int      w_x, w_y;    /* position of window */
	unsigned w_w, w_h;    /* size of window */
	unsigned border;      /* border width */
	unsigned depth;       /* depth of window */
	unsigned eff_w, eff_h;/* effective width and height */
	int      center_x;    /* center of coordinate system in pixel */
	int      center_y;    /* center of coordinate system in pixel */
	int      nice_num_i;  /* integer nice number */
	float    nice_num;    /* nice number of grid */
	float    radius;      /* radius */
	float    azimuth;     /* azimuth */
	int      pix_radius_x;/* length of x-radius in pixel */
	int      pix_radius_y;/* length of y-radius in pixel */
	float    fx, fy;      /* coordinate position */
	int      cx, cy;      /* pixel position */
	char     str[BC_LINELTH+1]; /* scratch string */

	/* executable code */

	XGetGeometry( XtDisplay(w), XtWindow(w), &root, &w_x, &w_y, &w_w, &w_h,
		&border, &depth );

	/* eff_w and eff_h (both are usually the same) tell how many pixels
	 * are used for two times mmv_scale.
	 */
	eff_w = w_w - mmv_setup.margin_l - mmv_setup.margin_r;
	eff_h = w_h - mmv_setup.margin_t - mmv_setup.margin_b;
	center_x = eff_w/2 + mmv_setup.margin_l;
	center_y = eff_h/2 + mmv_setup.margin_t;

	nice_num_i = 1;
	while  (mmv_scale/(float)nice_num_i > 8.0)  {
		sprintf( str, " %d", nice_num_i );
		switch  (str[1])  {
		case '1':   str[1] = '2';  break;
		case '2':   str[1] = '5';  break;
		case '5':   str[0] = '1'; str[1] = '0';  break;
		default:  strcpy( str, "1000000" );  break;
		} /*endswitch*/
		sscanf( str, "%d", &nice_num_i );
	} /*endwhile*/
	nice_num = (float)nice_num_i;

	for  (radius=nice_num; radius<=mmv_scale; radius += nice_num)  {
		pix_radius_x = Nint( radius / mmv_scale * (float)(eff_w/2) );
		pix_radius_y = Nint( radius / mmv_scale * (float)(eff_h/2) );
		pix_DrawArc( XtDisplay(w), XtWindow(w), mmv_gc[mmv_setup.colnum],
			center_x-pix_radius_x, center_y-pix_radius_y,
			2*pix_radius_x, 2*pix_radius_y, 0, 360*64 );
	} /*endfor*/

	radius -= nice_num;
	for  (azimuth=0.0; azimuth<360.0; azimuth += 30.0)  {
		fx = radius * cos( azimuth/180.0*MY_PI );
		fy = radius * sin( azimuth/180.0*MY_PI );
		cx = center_x + Nint( fx / mmv_scale * (float)(eff_w/2) );
		cy = center_y + Nint( fy / mmv_scale * (float)(eff_h/2) );
		pix_DrawLine( XtDisplay(w), XtWindow(w), mmv_gc[mmv_setup.colnum],
			center_x, center_y, cx, cy );
	} /*endfor*/

	sprintf( str, " radial grid step %g", nice_num );
	cx = w_w - mmv_setup.margin_r + 10;
	cy = (mmv_cmtcnt+1) * MmcCHARHEIGHT;
	pix_DrawString( XtDisplay(w), XtWindow(w), mmv_gc[mmv_setup.colnum], cx, cy,
		str, strlen(str) );

	XFlush( XtDisplay(w) );

} /* end of mm_draw_radial_grid */



/*--------------------------------------------------------------------------*/



static void mm_change_gc( Widget w, int mode, MmtDSP_SETUP *setup, GC gc[] )

/* changes gc's to other colors
 *
 * parameters of routine
 * Widget     w;         input; widget ID
 * int        mode;      input; which colors
 * MmtDSP_SETUP *setup;  input; setup
 * GC         gc[];      modify; GC's to change
 */
{
	/* local variables */
	int      i;           /* counter */
	float    frac;        /* color fraction */
	float    frac_x;      /* scratch */
	float    c, c1;       /* scratch */
	XColor   color;       /* color info */
	Colormap default_cmap;/* default colormap */
	int      screen_num;  /* screen number */

	/* executable code */

	screen_num = 0;

	default_cmap = DefaultColormap( XtDisplay(w), screen_num );

	for  (i=0; i<(setup->colnum); i++)  {
		frac = (float)i / (float)((setup->colnum)-1);
		switch  (mode)  {
		case MmcCOLORS_GREEN_RED:
			color.red = Nint(frac*65535.0);
			color.green = Nint((1-frac)*65535.0);
			color.blue = 0;
			break;
		case MmcCOLORS_BLUE_RED:
			color.red = Nint(frac*65535.0);
			color.green = 0;
			color.blue = Nint((1-frac)*65535.0);
			break;
		case MmcCOLORS_BLUE_YELLOW:
#ifdef XXX
			frac_x = (frac < 0.5) ? 2.0*frac : 1.0;
			color.red = Nint( frac_x * 65535.0 );
			frac_x = (frac > 0.5) ? 2.0*(frac-0.5) : 0.0;
			color.green = Nint( frac_x * 65535.0 );
			frac_x = (frac < 0.5) ? (1.0-2.0*frac) : 0.0;
			color.blue = Nint( frac_x * 65535.0 );
#endif
			/* red increases from 0 to c and then stays 1 */
			c = 0.5; /*0.4;*/
			frac_x = (frac < c) ? frac/c : 1.0;
			color.red = Nint( frac_x * 65535.0 );
			/* green increases from c to 1 and is zero before c */
			c = 0.5;
			frac_x = (frac > c) ? (frac-c)/(1.0-c) : 0.0;
			color.green = Nint( frac_x * 65535.0 );
			/* blue decreases from 1 to 0 between 0 and c and then stays 0 */
			c = 0.5; /*0.8;*/
			frac_x = (frac < c) ? (1.0-frac/c) : 0.0;
			color.blue = Nint( frac_x * 65535.0 );
			break;
		default:
			color.red = Nint((1-frac)*65535.0);
			color.green = color.blue = color.red;
			break;
		} /*endswitch*/
		color.flags = DoRed | DoGreen | DoBlue;
		if  (!XAllocColor(XtDisplay(w),default_cmap,&color))  {
			fprintf( stderr, "%s: error allocating color\n", mmv_pname );
			exit( 1 );
		} /*endif*/
		XSetForeground( XtDisplay(w), gc[i], color.pixel );
		XSetBackground( XtDisplay(w), gc[i],
			WhitePixel(XtDisplay(w),screen_num) );
	} /*endfor*/

} /* end of mm_change_gc */



/*--------------------------------------------------------------------------*/

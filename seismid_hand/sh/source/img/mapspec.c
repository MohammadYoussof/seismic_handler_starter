
/* file mapmatrix.c
 *      ===========
 *
 * version 2, 11-Oct-2006
 *
 * maps spectrograms
 * K. Stammler, 7-Oct-2006
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
#define MmcMAX_WIDGET 100

#define MmcMAX_COLOR 101
#define MmcMAXCMT 10

#define MmcCHARHEIGHT 12

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
static char		*mmv_vec[]={"mapspec.uid"};     /* MRM database file list */
static Display *mmv_display;                   /* display */
static Widget  mmv_w[MmcMAX_WIDGET];           /* widget array */
static char    mmv_pname[BC_FILELTH+1];        /* program name */
static MmtMATRIX         mmv_matrix;           /* matrix */
static MmtDSP_SETUP      mmv_setup;            /* display setup */
static GC                mmv_gc[MmcMAX_COLOR]; /* color gc's */

static char    mmv_cmt[MmcMAXCMT][BC_LINELTH+1];  /* comment lines */
static int     mmv_cmtcnt;                        /* comment counter */
static float   mmv_scale;                         /* scale factor */
static float   mmv_yscale;                        /* y-scale factor (cart.) */
static float   mmv_df;                            /* frequency sampling */
static BOOLEAN mmv_pixinit=FALSE;                 /* pixel buffer initialized */
static float   mmv_ymin;                          /* y start value (cart) */
static BOOLEAN mmv_logdsp=TRUE;                   /* logarithmic display */
static char    mmv_starttime[cBcLineLth+1]="";    /* start time */
static char    mmv_inpfile[BC_FILELTH+1];         /* name of input file */




/* prototypes */
static void cb_call_create_widget( Widget widget, int *tag,
	XmAnyCallbackStruct *data );
static void cb_call_activate( Widget widget, int *tag,
	XmToggleButtonCallbackStruct *data );
void cb_action_motion_cart( Widget w, XEvent *ev, String *params,
	Cardinal *parno );
static void cb_call_expose( Widget widget, char *tag,
	XmDrawingAreaCallbackStruct *data );
static void mm_draw_labels( Widget w );

static void mm_prepare_gc( Widget w, MmtDSP_SETUP *setup, GC gc[] );
static void mm_read_matrix( char filename[], MmtMATRIX *mat );
static void mm_display_matrix( Widget w, GC gc[], MmtDSP_SETUP *setup,
	MmtMATRIX *mat, int power );
static void mm_change_gc( Widget w, int mode, MmtDSP_SETUP *setup, GC gc[] );


/* routines to register */
static MrmCount         mmv_regnum = 3 ;
static MrmRegisterArg   mmv_regvec[] = {
	{ "call_create_widget", (caddr_t)cb_call_create_widget },
	{ "call_activate", (caddr_t)cb_call_activate },
	{ "call_expose", (caddr_t)cb_call_expose }
};

/* action routines */
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
	TSyStatus         locstat;          /* local status */

	/* executable code */

	pa_init( argc, argv );
	pix_w = pix_h = 0;

	if  (pa_pnumber() != 2)  {
		fprintf( stderr, "*** Usage: %s <file> <colnum> ***\n", argv[0] );
		fprintf( stderr, "      qualifiers:\n" );
		fprintf( stderr, "      -w=<pixwidth>    width of window in pixel\n" );
		fprintf( stderr, "      -h=<pixheight>   height of window in pixel\n" );
		return 1;
	} /*endif*/

	/* set margins */
	mmv_setup.margin_l = 40;
	mmv_setup.margin_r = 20;
	mmv_setup.margin_t = 30;
	mmv_setup.margin_b = 40;

	mmv_matrix.val = NULL;

	/* strcpy( mmv_pname, argv[0] ); */
	strcpy( mmv_pname, "mapspec" );
	strcpy( mmv_inpfile, pa_pvalue(1) );
	sscanf( pa_pvalue(2), "%d", &mmv_setup.colnum );
	mmv_scale = 1.0;
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
	if  (pa_qspecified("-ymax"))  {
		sscanf( pa_qvalue("-ymax"), "%f", &mmv_yscale );
	} /*endif*/
	mmv_ymin = 0.0;
	if  (pa_qspecified("-ymin"))
		sscanf( pa_qvalue("-ymin"), "%f", &mmv_ymin );

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

	XtAppAddActions( app_context, mmv_new_actions_cart, 1 );

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

	/* read in data file */
	mm_read_matrix( mmv_inpfile, &mmv_matrix );
	mm_prepare_gc( mmv_w[w_draw], &mmv_setup, mmv_gc );

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
	mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, 1 ); 

	XtAppMainLoop(app_context);

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
		strcpy( acttable, "<Motion>: cb_action_motion_cart()\n" );
		strcat( acttable, "<Btn1Down>: cb_action_motion_cart()\n" );
		strcat( acttable, "<Btn1Up>: cb_action_motion_cart()\n" );
		strcat( acttable, "<Btn2Down>: cb_action_motion_cart()\n" );
		strcat( acttable, "<Btn2Up>: cb_action_motion_cart()\n" );
		strcat( acttable, "<Btn3Down>: cb_action_motion_cart()\n" );
		strcat( acttable, "<Btn3Up>: cb_action_motion_cart()\n" );
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
	case w_entry_plot:
		power=1;
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, 1 ); 
		break;
	case w_entry_plot_log:
		/*power = 0;*/
		mmv_logdsp = !mmv_logdsp;
		mm_read_matrix( mmv_inpfile, &mmv_matrix );
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, 0 );
		break;
	case w_entry_plot_p2:
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix,
			++power );
		break;
	case w_entry_plot_p4:
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix,
			--power );
		break;
	case w_entry_colors_green_red:
		mm_change_gc( mmv_w[w_draw], MmcCOLORS_GREEN_RED, &mmv_setup, mmv_gc );
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, power);
		break;
	case w_entry_colors_blue_red:
		mm_change_gc( mmv_w[w_draw], MmcCOLORS_BLUE_RED, &mmv_setup, mmv_gc );
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, power);
		break;
	case w_entry_colors_black_white:
		mm_change_gc( mmv_w[w_draw], MmcCOLORS_BLACK_WHITE, &mmv_setup, mmv_gc );
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, power);
		break;
	case w_entry_colors_blue_yellow:
		mm_change_gc( mmv_w[w_draw], MmcCOLORS_BLUE_YELLOW, &mmv_setup, mmv_gc );
		mm_display_matrix( mmv_w[w_draw], mmv_gc, &mmv_setup, &mmv_matrix, power);
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


#define POSTEXT_X 0
#define POSTEXT_Y 8



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
	float    slow_x, frequ;            /* slowness position */
	char     ctime[cBcTimeLth+1];       /* current absolute time */
	STATUS   locstat;                   /* local status */

	/* executable code */

	if  (w != mmv_w[w_draw])  {printf("--> motion: ill wdw\n"); return;}

	XGetGeometry( XtDisplay(w), XtWindow(w), &root, &w_x, &w_y, &w_w, &w_h,
		&border, &depth );
	pos_x = mmv_setup.margin_l + POSTEXT_X;
	pos_y = w_h - POSTEXT_Y;
	eff_w = w_w - mmv_setup.margin_l - mmv_setup.margin_r;
	eff_h = w_h - mmv_setup.margin_t - mmv_setup.margin_b;
	if  (!is_init)  {
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
			frequ = (float)(ev->xmotion.y - mmv_setup.margin_t);
			slow_x = slow_x / (float)eff_w * mmv_scale;
			frequ = mmv_ymin + mmv_yscale - (frequ / (float)eff_h * mmv_yscale);
			locstat = cBcNoError;
			tc_tadd( mmv_starttime, slow_x, ctime, &locstat );
			sprintf( postext, "time: %s,  %6.2f s", ctime, slow_x );
			if  (mmv_logdsp)  {
				frequ /= mmv_yscale;
				frequ = log(mmv_df) + frequ*(log(mmv_yscale)-log(mmv_df));
				frequ = exp( frequ );
			}  /*endif*/
			sprintf( postext2, "freq: %6.2f Hz", frequ );
		} /*endif*/
		if  (*oldstr != '\0')  {
			pix_DrawString( XtDisplay(w), XtWindow(w), xgc,
				pos_x, pos_y, oldstr, strlen(oldstr) );
			pix_DrawString( XtDisplay(w), XtWindow(w), xgc,
				pos_x+260, pos_y, oldstr2, strlen(oldstr2) );
		} /*endif*/
		if  (*postext != '\0')  {
			pix_DrawString( XtDisplay(w), XtWindow(w), mmv_gc[mmv_setup.colnum],
				pos_x, pos_y, postext, strlen(postext) );
			pix_DrawString( XtDisplay(w), XtWindow(w), mmv_gc[mmv_setup.colnum],
				pos_x+260, pos_y, postext2, strlen(postext2) );
		} /*endif*/
		strcpy( oldstr, postext );
		strcpy( oldstr2, postext2 );
		XFlush( XtDisplay(w) );
	} /*endif*/

} /* end of cb_action_motion_cart */



/*--------------------------------------------------------------------------*/


#define LNCONV 2.30258509


static void mm_draw_labels( Widget w )

/* Draws labels to spectrogram
 *
 * parameters of routine
 * Widget     w;          input; drawing widget
 */
{
	/* local variables */
	static BOOLEAN l_is_init=FALSE;       /* already initialized */
	int            pos_x, pos_y;          /* draw position */
	static int     eff_w, eff_h;          /* effective size of window */
	static GC      wgc;                   /* write GC */
	static float   nicenum[] = {          /* nice numbers */
		1.0,2.0,5.0,10.0,20.0,50.0,100.0,200.0,500.0,
		1000.0,2000.0,5000.0,10000.0,0.0
	};
	Window         root;                    /* root window */
	int            w_x, w_y;                /* window position */
	unsigned       w_w, w_h;                /* size of window */
   unsigned       border, depth;           /* window pars (not used) */
	float          t_inc;                   /* time increment for labels */
	int            i, j;                    /* counters */
	float          tmp;                     /* scratch */
	char           label[cBcShortStrLth+1]; /* label string */
	int            itmp;                    /* int scratch */
	float          ylogscale;               /* log scale */

	/* executable code */

	XGetGeometry( XtDisplay(w), XtWindow(w), &root, &w_x, &w_y, &w_w, &w_h,
		&border, &depth );
	eff_w = w_w - mmv_setup.margin_l - mmv_setup.margin_r;
	eff_h = w_h - mmv_setup.margin_t - mmv_setup.margin_b;
	if  (!l_is_init)  {
		wgc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
		XCopyGC( XtDisplay(w), mmv_gc[mmv_setup.colnum], GCFont, wgc );
		XSetForeground( XtDisplay(w), wgc, BlackPixel(XtDisplay(w),0) );
		XSetBackground( XtDisplay(w), wgc, WhitePixel(XtDisplay(w),0) );
		l_is_init = TRUE;
	} /*endif*/

	/* get label distance on time */
	t_inc = mmv_scale / 4.0;
	tmp = t_inc;
	i = j = 0;
	while  (nicenum[i] > 0.0)  {
		if  (fabs(t_inc-nicenum[i]) < tmp)  {
			tmp = fabs(t_inc-nicenum[i]);
			j = i;
		} /*endif*/
		i++;
	} /*endif*/
	t_inc = nicenum[j];

	tmp = 0.0;
	while  (tmp < mmv_scale)  {
		sprintf( label, "%g", tmp );
		pos_x = mmv_setup.margin_l + (int)(tmp/mmv_scale*(float)eff_w);
		pos_y = w_h - mmv_setup.margin_b;
		pix_DrawLine( XtDisplay(w), XtWindow(w), wgc, pos_x, pos_y, pos_x, pos_y+5 );
		pix_DrawString( XtDisplay(w), XtWindow(w), wgc,
			pos_x-5, pos_y+MmcCHARHEIGHT+5, label, strlen(label) );
		tmp += t_inc;
	} /*endwhile*/

	if  (mmv_logdsp)  {
		pos_x = mmv_setup.margin_l;
		itmp = (int)(log(mmv_df)/LNCONV);
		if  (itmp > 0)  itmp++;
		ylogscale = (log(mmv_yscale)-log(mmv_df))/LNCONV;
		while  (pow((float)itmp,10.0) < mmv_yscale*1.01)  {
			pos_y = w_h - mmv_setup.margin_b
				- ((float)itmp-(log(mmv_df)/LNCONV))/ylogscale*(float)eff_h;
			sprintf( label, "%d", itmp );
			pix_DrawLine( XtDisplay(w), XtWindow(w), wgc,
				pos_x, pos_y, pos_x-5, pos_y );
			pix_DrawString( XtDisplay(w), XtWindow(w), wgc,
				pos_x-20, pos_y-(MmcCHARHEIGHT/2)+6, label, strlen(label) );
			pix_DrawString( XtDisplay(w), XtWindow(w), wgc,
				pos_x-33, pos_y+5, "10", 2 );
			itmp++;
		} /*endwhile*/
	} else {
		pos_x = mmv_setup.margin_l;
		itmp = 0;
		while  ((float)itmp < mmv_yscale*1.01)  {
			pos_y = w_h - mmv_setup.margin_b
				- ((float)itmp/mmv_yscale)*(float)eff_h;
			pix_DrawLine( XtDisplay(w), XtWindow(w), wgc,
				pos_x, pos_y, pos_x-5, pos_y );
			sprintf( label, "%d", itmp );
			pix_DrawString( XtDisplay(w), XtWindow(w), wgc,
				pos_x-20, pos_y+4, label, strlen(label) );
			itmp += 5;
		} /*endwhile*/
	} /*endif*/

	/* legend */
	pos_x = 5;
	pos_y = 13;
	strcpy( label, "frq in Hz" );
	pix_DrawString( XtDisplay(w), XtWindow(w), wgc,
		pos_x, pos_y, label, strlen(label) );
	pos_x = w_w-80;
	pos_y = w_h-10;
	strcpy( label, "time in s" );
	pix_DrawString( XtDisplay(w), XtWindow(w), wgc,
		pos_x, pos_y, label, strlen(label) );

} /* end of mm_draw_labels */



/*--------------------------------------------------------------------------*/



static void mm_read_matrix( char filename[], MmtMATRIX *mat )

/* Allocates memory for matrix and reads it from file
 *
 * parameters of routine
 * char       filename[];        input; name of input file
 * MmtMATRIX  *mat;              output; matrix
 */
{
	/* local variables */
	FILE     *fp;         /* pointer to input file */
	char     line[BC_LINELTH+1];   /* current line in input file */
	int      i, j;        /* counters */
	float    *lfrq;       /* scratch array */
	float    logmin;      /* log frq minimum */
	float    logmax;      /* log frq maximum */
	float    logdf;       /* logarithmic difference */
	float    logf;        /* current logarithmic frq */
	float    tmp;         /* scratch */
	int      idx;         /* index number */

	/* executable code */

	fp = fopen( filename, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: couldn't open input file %s\n",
			mmv_pname, filename );
		exit( 1 );
	} /*endif*/

	/* read off comments */
	mmv_cmtcnt = 0;
	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (*line != '!')  break;
		if  (line[1] == '*' && mmv_cmtcnt < MmcMAXCMT)  {
			strcpy( mmv_cmt[mmv_cmtcnt], line+2 );
			i = strlen( mmv_cmt[mmv_cmtcnt] ) - 1;
			if  (mmv_cmt[mmv_cmtcnt][i] == '\n')
				mmv_cmt[mmv_cmtcnt][i] = '\0';
			mmv_cmtcnt++;
		} /*endif*/
		if  (strncmp(line,"!  LENGTH:",10) == 0)  {
			if  (sscanf( line+10, "%d", &(mat->height) ) != 1)  {
				fprintf( stderr, "%s: error reading LENGTH\n", mmv_pname );
				exit( 1 );
			} /*endif*/
		} else if  (strncmp(line,"!  LINES:",9) == 0)  {
			if  (sscanf( line+9, "%d", &(mat->width) ) != 1)  {
				fprintf( stderr, "%s: error reading LINES\n", mmv_pname );
				exit( 1 );
			} /*endif*/
		} else if  (strncmp(line,"!  DELTA:",9) == 0)  {
			if  (sscanf( line+9, "%f", &mmv_scale ) != 1)  {
				fprintf( stderr, "%s: error reading DELTA\n", mmv_pname );
				exit( 1 );
			} /*endif*/
		} else if  (strncmp(line,"!  DF:",6) == 0)  {
			if  (sscanf( line+6, "%f", &mmv_df ) != 1)  {
				fprintf( stderr, "%s: error reading DF\n", mmv_pname );
				exit( 1 );
			} /*endif*/
		} else if  (strncmp(line,"!  START:",9) == 0)  {
			if  (sscanf( line+9, "%s", &mmv_starttime ) != 1)  {
				fprintf( stderr, "%s: error reading START\n", mmv_pname );
				exit( 1 );
			} /*endif*/
		} /*endif*/
	} /*endwhile*/

	if  (mmv_starttime[0] != '\0' && mmv_cmtcnt > 0)  {
		strcat( mmv_cmt[0], " " );
		strcat( mmv_cmt[0], mmv_starttime );
	} /*endif*/

	mmv_scale *= (float)(mat->width);
	mmv_yscale = mmv_df * (float)(mat->height);

	/* allocate pointers if not yet done */
	if  (mat->val == NULL)  {
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
	} /*endif*/

	/* read in data */
	for  (j=0; j<(mat->width); j++)  {
		for  (i=0; i<(mat->height); i++)
			fscanf( fp, "%f\n", (mat->val[i])+j );
		} /*endfor*/

	fclose( fp );

	/* make logarithmic frequency */
	if  (mmv_logdsp)  {

		lfrq = (float *)malloc( sizeof(float) * mat->height );
		if  (lfrq == NULL)  {
			fprintf( stderr, "%s: allocation error (lfrq)\n", mmv_pname );
			exit( 1 );
		} /*endif*/

		logmax = log( mmv_df * (float)(mat->height) );
		logmin = log( mmv_df );
		logdf = (logmax-logmin) / (float)((mat->height)-1);

		for  (j=0; j<(mat->width); j++)  {
			for  (i=0; i<(mat->height); i++)  {
				logf = exp( logmin + (float)i*logdf );
				tmp = logf / mmv_df;
				idx = (int)tmp;
				tmp -= (float)idx;
				if  (idx >= (mat->height)-1)  {
					lfrq[i] = mat->val[(mat->height)-1][j];
				} else {
					lfrq[i] = mat->val[idx][j]
						+ tmp * (mat->val[idx+1][j]-mat->val[idx][j]);
				} /*endif*/
				/*printf( "--> %d: %d,%d,%f\n", j, i, idx, tmp );*/
			} /*endfor*/
			for  (i=0; i<(mat->height); i++)  {
				mat->val[i][j] = lfrq[i];
			} /*endfor*/
		} /*endfor*/

		free( lfrq );

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

	printf( "%s: found a %s class visual at default depth %d\n",
		mmv_pname, visual_class[++i] );
	if  (i < StaticColor)  {
		fprintf( stderr, "%s: visual class %s is not supported\n",
			mmv_pname, visual_class[i] );
		exit( 1 );
	} /*endif*/

	if  (visual_info.visual != default_visual)
		printf( "%s: %s class visual at default depth is not default visual\n",
			mmv_pname, visual_class[i] );

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
#		ifdef XXX
		color.red = Nint(frac*65535.0);
		color.green = Nint((1-frac)*65535.0);
		color.blue = 0;
#		endif
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
		"-b&h-lucidatypewriter-bold-r-normal-sans-*-100-*-*-*-*-*-1" );

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
	static GC      cgc;   /* clear GC */
	static BOOLEAN d_is_init=FALSE;  /* GC is initialised */
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

	if  (!d_is_init)  {
		cgc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
		XCopyGC( XtDisplay(w), mmv_gc[mmv_setup.colnum], GCFont, cgc );
		XSetForeground( XtDisplay(w), cgc, WhitePixel(XtDisplay(w),0) );
		XSetBackground( XtDisplay(w), cgc, BlackPixel(XtDisplay(w),0) );
		d_is_init = TRUE;
	} /*endif*/
	pix_FillRectangle( XtDisplay(w), XtWindow(w), cgc, 0, 0, w_w, w_h );	

	/* get width and height of rectangle */
	rect_width = (float)(w_w-(setup->margin_l)-(setup->margin_r))
		/ (float)(mat->width);
	rect_height = (float)(w_h-(setup->margin_t)-(setup->margin_b))
		/ (float)(mat->height);

	cmt_x = setup->margin_l + 50;
	cmt_y = MmcCHARHEIGHT + 3;
	for  (i=0; i<mmv_cmtcnt; i++)  {
		pix_DrawString( XtDisplay(w), XtWindow(w), gc[setup->colnum], cmt_x,
			cmt_y, mmv_cmt[i], strlen(mmv_cmt[i]) );
		cmt_y += MmcCHARHEIGHT;
	} /*endfor*/

	for  (i=0; i<(mat->height); i++)  {
		for  (j=0; j<(mat->width); j++)  {
			rect_x = (float)j * rect_width;
			rect_y = (float)(mat->height-1-i) * rect_height;
			frac = (mat->val[i][j]-mat->minval) / (mat->maxval-mat->minval);
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

	mm_draw_labels( w );

	XFlush( XtDisplay(w) );

} /* end of mm_display_matrix */



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

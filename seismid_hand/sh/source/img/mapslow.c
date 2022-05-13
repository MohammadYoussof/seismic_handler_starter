
/* file mapslow.c
 *      =========
 *
 * version 10, 15-Oct-2005
 *
 * maps slowness values
 * K. Stammler, 9-Sep-2002
 */



#include <stdio.h>
#include <math.h>
#include "basecnst.h"
#include <Xm/Xm.h>
#include <Mrm/MrmPublic.h>
#include <X11/cursorfont.h>
#undef BC_DEFINE_TRUE_FALSE
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "cpar.h"
#include "../motif/pixmaps.h"
#include "ptusrdef.h"
#include "tcusrdef.h"


#define COL_BLUE


#define w_toplevel 0
#define w_main 1
#define w_draw 2
#define w_menu_main 5
#define w_entry_quit 51
#define w_entry_plot 52
#define w_entry_show_1 53
#define w_entry_show_2 54
#define w_entry_show_3 55
#define w_entry_show_4 56
#define w_entry_show_5 57
#define w_entry_show_6 58
#define w_entry_show_7 59
#define w_entry_check_1 60
#define w_entry_check_2 61
#define w_entry_check_3 62
#define w_entry_check_4 63
#define w_entry_check_5 64
#define w_entry_check_6 65
#define w_entry_check_7 66
#define w_entry_fk_1    67
#define w_entry_fk_2    68
#define w_entry_fk_3    69
#define w_entry_fk_4    70
#define w_entry_fk_5    71
#define w_entry_fk_6    72
#define w_entry_fk_7    73

#define MscMAX_WIDGET 100

#define MscMAX_COLOR 101

#define MscCHARHEIGHT 30

#define MscBG 0
#define MscBLACK 1
#define MscYELLOW 2
#define MscSELECT 3
#define MscFIXED_COLORS 4

#define MscMAX_DETFILES 10
#define MscDEFAULT_SETUPFILE "mapslow.par"

#define MsfGMT      0x01
#define MsfXWDW     0x02
#define MsfEXIT     0x04
#define MsfDUMPEVAL 0x08
#define MsfDEBUG    0x10


/* local types */

typedef struct {
	float     timewidth;        /* width of matrix (in s) */
	float     slowheight;       /* height of matrix (in s/deg) */
	float     mintime;          /* start time */
	float     minslow;          /* start slowness */
} MstMapArea;

typedef struct {
	int       colnum;       /* number of colors */
	unsigned  margin_l;     /* left margin */
	unsigned  margin_r;     /* right margin */
	unsigned  margin_t;     /* top margin */
	unsigned  margin_b;     /* bottom margin */
} MstDspSetup;

typedef struct {
	float     aztol;        /* azimuth tolerance */
	float     minqual;      /* minimum quality number */
	float     maxqual;      /* maximum quality number (cutoff value) */
	float     minrelpow;    /* minimum relative power */
	float     minabspow;    /* minimum absolute power */
	float     mineval;      /* minimum evaluation number */
	int       eval_avlth;   /* average length in samples for evaluation function */
	float     m_q;          /* quality multiplier */
	float     m_r;          /* relative power multiplier */
	float     m_a;          /* absolute power multiplier */
	float     m_qr;         /* quality/relative power multiplier */
	float     m_qa;         /* quality/absolute power multiplier */
	float     m_ra;         /* relative/absolute power multiplier */
	float     m_qra;        /* quality/relative power/absolute power multiplier */
	float     sloint;       /* slowness interval (half-height) */
	float     col_r;        /* red color fraction */
	float     col_g;        /* green color fraction */
	float     col_b;        /* blue color fraction */
	GC        gc;           /* graphics context */
	char      text[cBcLineLth+1]; /* descriptive text */
} MstDetPar;



/* global variables */
static char    msv_detfile[MscMAX_DETFILES][cBcFileLth+1]; /* detfiles */
static MstDetPar msv_detpar[MscMAX_DETFILES];  /* detection display parameters */
static char		*msv_vec[]={"mapslow.uid"};     /* MRM database file list */
static Display *msv_display;                   /* display */
static Widget  msv_w[MscMAX_WIDGET];           /* widget array */
static char    msv_pname[cBcFileLth+1];        /* program name */
static MstMapArea        msv_area;             /* matrix */
static MstDspSetup       msv_dsp;              /* display setup */
static GC                msv_gc[MscMAX_COLOR]; /* color gc's */
static GC                msv_gl[MscFIXED_COLORS]; /* fixed colors */
static Cursor            msv_crsr_busy;        /* busy cursor */

static int     msv_wdw_height;                    /* window height in pixels */
static float   msv_tscale;                        /* time scale factor */
static float   msv_sscale;                        /* y-scale factor (cart.) */
static BOOLEAN msv_pixinit=FALSE;                 /* pixel buffer initialized */
static char    msv_abstime[cBcTimeLth+1];         /* absolute time */
static int     msv_flags;                         /* flags */
static float   msv_baz=0.0;                       /* back azimuth */
static char    msv_dsptime[cBcTimeLth+1];         /* display time */
static float   msv_dspslow;                       /* display slowness */

static float   msv_scale=1.0;
static float   msv_yscale=1.0;

static float   msv_dist;                          /* distance of event */
static float   msv_depth;                         /* depth of event */

static char    *msv_phases[] = {
	"P", "PP", "PPP", "PcP", "Pdiff", "PKPab", "PKPbc", "PKPdf", "PKKPdf",
	"PKKPbc", "PKKPab", "S", "SP", "SS", "SSS", "ScS", "PcS", "Sdiff", "SKSac",
	"SKSdf", "SKKPab", "SKKPbc", "SKKPdf", "SKKSac", "SKKSdf", "PxPxdf",
	"PxPxab", "PxPxbc", "SKPab", "SKPdf", "PKiKP", ""
};




/* prototypes */
static void cb_call_create_widget( Widget widget, int *tag,
	XmAnyCallbackStruct *data );
static void cb_call_activate( Widget widget, int *tag,
	XmToggleButtonCallbackStruct *data );
void cb_action_motion_cart( Widget w, XEvent *ev, String *params,
	Cardinal *parno );
static void cb_call_expose( Widget widget, char *tag,
	XmDrawingAreaCallbackStruct *data );

static void ms_read_setup( char setup[], MstDetPar par[] );
static void ms_prepare_gc( Widget w, MstDetPar par[], GC gl[] );
static void ms_draw_background( Widget w, MstDspSetup *dsp, float *tscale,
	float *sscale );
static void ms_trafo( float t, float s, int *x, int *y );
static void ms_draw_axes( Widget w, MstDspSetup *dsp, MstMapArea *area, GC gc,
	char text[] );
static ms_put_phases( Widget w, GC gc, float dist, float depth,
	TSyBoolean lab );
static void ms_put_detfile( Widget w, char detfile[], MstDetPar *detpar,
	float baz, GC gc[] );


/* routines to register */
static MrmCount         msv_regnum = 3 ;
static MrmRegisterArg   msv_regvec[] = {
	{ "call_create_widget", (caddr_t)cb_call_create_widget },
	{ "call_activate", (caddr_t)cb_call_activate },
	{ "call_expose", (caddr_t)cb_call_expose }
};



/* action routines */
static   XtActionsRec msv_new_actions_cart[] = {
	{"cb_action_motion_cart", cb_action_motion_cart}
};



int main( int argc, char *argv[] )
{
	/* local variables */
	MrmHierarchy      hierarchy;        /* hierarchy */
	MrmCode           class;            /* class */
	XtAppContext      app_context;      /* application context */
	Arg arglist[2];
	int n;
	int               pix_w, pix_h;     /* size of window in pixel */
	char              *env;             /* pointer to environment */
	TSyStatus         status;           /* SH return status */
	char              setup[cBcFileLth+1];   /* display setup input file */
	char              str[cBcLineLth+1];/* scratch string */
	char              title[cBcLongStrLth+1]; /* title of figure */
	int               i;                /* counter */
	char              xwdfile[cBcFileLth+1]; /* xwd file */
	char              cmdline[cBcLineLth+1]; /* command line */

	/* executable code */

	pa_init( argc, argv );
	pix_w = pix_h = 0;

	msv_flags = 0;

	if  (pa_pnumber() != 6)  {
		fprintf( stderr, "*** Usage: %s <starttime> <timewdw> <maxslow> <distance> <depth> <baz> ***\n", argv[0] );
		fprintf( stderr, "      qualifiers:\n" );
		fprintf( stderr, "      -d<n>=<detfile-n> det file number n\n" );
		fprintf( stderr, "      -ms=<minslow>    minimum slowness (default 0)\n" );
		fprintf( stderr, "      -w=<pixwidth>    width of window in pixel\n" );
		fprintf( stderr, "      -h=<pixheight>   height of window in pixel\n" );
		fprintf( stderr, "      -s=<setup>       default $SH_INPUTS/mapslow.par\n" );
		fprintf( stderr, "      -t=<title>       title of figure\n" );
		fprintf( stderr, "      -x=<xwdfile>     create xwd output file\n" );
		fprintf( stderr, "      -q               quit program immediately\n" );
		fprintf( stderr, "      -gmt             print data usable for gmt\n" );
		return 1;
	} /*endif*/

	/* travel time table settings */
	env = (char *)getenv( "SH_INPUTS" );
	if  (env == NULL)  {
		fprintf( stderr, "environment SH_INPUTS not set.  Abort.\n" );
		exit( 1 );
	} /*endif*/
	status = cBcNoError;
	pt_settabledir( env, &status );
	if  (SySevere(&status)) exit(1);

	/* set margins */
	msv_dsp.margin_l = 40;
	msv_dsp.margin_r = 100;
	msv_dsp.margin_t = 30;
	msv_dsp.margin_b = 60;
	msv_dsp.colnum = 3;

	strcpy( msv_pname, "mapslow" );
	strcpy( msv_abstime, pa_pvalue(1) );
	sscanf( pa_pvalue(2), "%f", &msv_area.timewidth );
	sscanf( pa_pvalue(3), "%f", &msv_area.slowheight );
	sscanf( pa_pvalue(4), "%f", &msv_dist );
	sscanf( pa_pvalue(5), "%f", &msv_depth );
	sscanf( pa_pvalue(6), "%f", &msv_baz );
	if  (msv_dsp.colnum < 2)  msv_dsp.colnum = 2;
	if  (msv_dsp.colnum > 500)  msv_dsp.colnum = 500;
	if  (pa_qspecified("-ms"))  {
		sscanf( pa_qvalue("-ms"), "%f", &msv_area.minslow );
	} else {
		msv_area.minslow = 0.0;
	} /*endif*/
	if  (pa_qspecified("-w"))  {
		sscanf( pa_qvalue("-w"), "%d", &pix_w );
		if  (pix_w < 400)  pix_w = 400;
	} /*endif*/
	if  (pa_qspecified("-h"))  {
		sscanf( pa_qvalue("-h"), "%d", &pix_h );
		if  (pix_h < 400)  pix_h = 400;
	} /*endif*/
	if  (pa_qspecified("-s"))  {
		strncpy( setup, pa_qvalue("-s"), cBcFileLth );
	} else {
		strcpy( setup, env );
		strcat( setup, "/" );
		strcat( setup, MscDEFAULT_SETUPFILE );
	} /*endif*/
	if  (pa_qspecified("-x"))  {
		strcpy( xwdfile, pa_qvalue("-x") );
	} else {
		*xwdfile = '\0';
	} /*endif*/
	if  (pa_qspecified("-t"))  {
		strcpy( title, pa_qvalue("-t") );
	} else {
		strcpy( title, msv_abstime );
	} /*endif*/
	if  (pa_qspecified("-q"))  {
		msv_flags |= MsfEXIT;
	} /*endif*/
	if  (pa_qspecified("-gmt"))  {
		msv_flags |= MsfGMT;
	} else {
		msv_flags |= MsfXWDW;
	} /*endif*/
	if  (pa_qspecified("-dumpeval"))  {
		msv_flags |= MsfDUMPEVAL;
	} /*endif*/
	for  (i=0; i<MscMAX_DETFILES; i++)  {
		sprintf( str, "-d%d", i+1 );
		if  (pa_qspecified(str))  {
			strcpy( msv_detfile[i], pa_qvalue(str) );
		} else {
			msv_detfile[i][0] = '\0';
		} /*endif*/
	} /*endfor*/

	msv_scale = msv_area.timewidth;
	msv_yscale = msv_area.slowheight;

	if  (MsfXWDW & msv_flags)  {

		MrmInitialize ();
		XtToolkitInitialize();
		app_context = XtCreateApplicationContext();
		msv_display = XtOpenDisplay(app_context, NULL, msv_pname, "mapslow",
			NULL, 0, &argc, argv);
		if (msv_display == NULL) {
			fprintf(stderr, "%s:  Can't open display\n", msv_pname );
			exit(1);
		} /*endif*/

		XtAppAddActions( app_context, msv_new_actions_cart, 1 );

		n = 0;
		/* XtSetArg(arglist[n], XmNallowShellResize, True);  n++; */
		msv_w[w_toplevel] = XtAppCreateShell( msv_pname, NULL,
			applicationShellWidgetClass, msv_display, arglist, n);

		if  (MrmOpenHierarchy(1,msv_vec,NULL,&hierarchy) != MrmSUCCESS) {
			fprintf ( stderr, "can't open hierarchy\n" );
			exit( 1 );
		} /*endif*/

		if  (MrmRegisterNames(msv_regvec,msv_regnum) != MrmSUCCESS)  {
			fprintf( stderr, "can't register names\n" );
			exit( 1 );
		} /*endif*/

		if (MrmFetchWidget(hierarchy,"main_window",msv_w[w_toplevel],
			msv_w+w_main,&class) != MrmSUCCESS)  {
			fprintf( stderr, "can't fetch widget\n" );
			exit( 1 );
		} /*endif*/

		n = 0;
		if  (pix_w > 0)  {XtSetArg( arglist[n], XmNwidth, pix_w ); n++;}
		if  (pix_h > 0)  {XtSetArg( arglist[n], XmNheight, pix_h ); n++;}
		if  (n > 0)  XtSetValues( msv_w[w_draw], arglist, n );

		XtManageChild( msv_w[w_main] );
		XtRealizeWidget( msv_w[w_toplevel] );

		msv_crsr_busy = XCreateFontCursor( XtDisplay(msv_w[w_draw]), XC_watch );

		if  (!msv_pixinit)  {
			status = cBcNoError;
			pix_create_window_buffer( XtDisplay(msv_w[w_draw]),
				XtWindow(msv_w[w_draw]), TRUE, &status );
			if  (Severe(&status))  {
				fprintf( stderr, "mapmatrix: error creating window buffer\n" );
				exit( 1 );
			} /*endif*/
			msv_pixinit = TRUE;
		} /*endif*/

	} /*endif*/

	ms_read_setup( setup, msv_detpar );

	if  (MsfXWDW & msv_flags)
		ms_prepare_gc( msv_w[w_draw], msv_detpar, msv_gl );

	/* draw background */
	ms_draw_background( msv_w[w_draw], &msv_dsp, &msv_tscale, &msv_sscale );
	ms_draw_axes( msv_w[w_draw], &msv_dsp, &msv_area, msv_gl[MscBLACK], title );

	for  (i=MscMAX_DETFILES-1; i>=0; i--)
		if  (msv_detfile[i][0] != '\0')
			ms_put_detfile( msv_w[w_draw], msv_detfile[i], &(msv_detpar[i]),
				msv_baz, msv_gc );

	if  (msv_dist > 0)
		ms_put_phases( msv_w[w_draw], msv_gl[MscYELLOW], msv_dist,
			msv_depth, TRUE );

	if  (MsfXWDW & msv_flags)  {
		XFlush( XtDisplay(msv_w[w_draw]) );
		if  (*xwdfile != '\0')  {
			system( "sleep 1" );
			sprintf( cmdline, "xwd -name mapslow >%s", xwdfile );
			system( cmdline );
		} /*endif*/
		if  (!(MsfEXIT & msv_flags))
			XtAppMainLoop(app_context);
	} /*endif*/
	if  (MsfGMT & msv_flags)  {
		printf( "echo showpage\n" );
	} /*endif*/

	return 0;

} /* end of main */



/*--------------------------------------------------------------------------*/



static void cb_call_create_widget( Widget widget, int *tag,
	XmAnyCallbackStruct *data )

/* Callback routine on creating widgets
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

	if  (wno >= MscMAX_WIDGET || wno < 0)  {
		fprintf( stderr, "--> illegal widget number %d\n", wno );
		return;
	} /*endif*/

	/* printf( "[%d]", wno ); */
	msv_w[wno] = widget;

	if  (wno == w_draw)  {
		strcpy( acttable, "<Motion>: cb_action_motion_cart()\n" );
		strcat( acttable, "<Btn1Down>: cb_action_motion_cart()\n" );
		strcat( acttable, "<Btn1Up>: cb_action_motion_cart()\n" );
		strcat( acttable, "<Btn2Down>: cb_action_motion_cart()\n" );
		strcat( acttable, "<Btn2Up>: cb_action_motion_cart()\n" );
		strcat( acttable, "<Btn3Down>: cb_action_motion_cart()\n" );
		strcat( acttable, "<Btn3Up>: cb_action_motion_cart()\n" );
		/*strcat( acttable, "<KeyPress>: cb_action_motion_cart()\n" );*/
		new_table = XtParseTranslationTable( acttable );
		XtSetArg( al[0], XmNtranslations, new_table );
		XtSetValues( msv_w[w_draw], al, 1 );
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
	static int power=1;             /* current power */
	STATUS   locstat;               /* local status */
	char     cmd[cBcLongStrLth+1];  /* shell command */
	int      i;                     /* index */

	/* executable code */

	if  (!msv_pixinit)  {
		locstat = BC_NOERROR;
		pix_create_window_buffer( XtDisplay(msv_w[w_draw]),
			XtWindow(msv_w[w_draw]), TRUE, &locstat );
		if  (Severe(&locstat))  {
			fprintf( stderr, "mapmatrix: error creating window buffer\n" );
			exit( 1 );
		} /*endif*/
		msv_pixinit = TRUE;
	} /*endif*/

	switch  (*tag)  {
	case w_entry_quit:
		exit( 0 );
	case w_entry_show_1:
	case w_entry_show_2:
	case w_entry_show_3:
	case w_entry_show_4:
	case w_entry_show_5:
	case w_entry_show_6:
	case w_entry_show_7:
		sprintf( cmd, "$SH_UTIL/mapslow_wavedsp.csh %d %s %f %f %s %5.1f %5.1f",
			(*tag)-w_entry_show_1+1, msv_dsptime, msv_dspslow, msv_baz,
			msv_abstime, msv_dist, msv_depth );
		XDefineCursor( XtDisplay(msv_w[w_draw]), XtWindow(msv_w[w_draw]), msv_crsr_busy );
		XFlush( XtDisplay(msv_w[w_draw]) );
		system( cmd );
		XUndefineCursor( XtDisplay(msv_w[w_draw]), XtWindow(msv_w[w_draw]) );
		break;
	case w_entry_check_1:
	case w_entry_check_2:
	case w_entry_check_3:
	case w_entry_check_4:
	case w_entry_check_5:
	case w_entry_check_6:
	case w_entry_check_7:
		i = (*tag) - w_entry_check_1;
		sprintf( cmd, "$SH_UTIL/mapslow_detfct.csh %s %f %f %f %f,%f,%f,%f,%f,%f,%f,%f,%d",
			msv_detfile[i], msv_area.timewidth, msv_baz, msv_detpar[i].mineval,
			msv_detpar[i].m_q, msv_detpar[i].m_r, msv_detpar[i].m_a,
			msv_detpar[i].m_qr, msv_detpar[i].m_qa, msv_detpar[i].m_ra,
			msv_detpar[i].m_qra, msv_detpar[i].maxqual, msv_detpar[i].eval_avlth );
		XDefineCursor( XtDisplay(msv_w[w_draw]), XtWindow(msv_w[w_draw]), msv_crsr_busy );
		XFlush( XtDisplay(msv_w[w_draw]) );
		system( cmd );
		XUndefineCursor( XtDisplay(msv_w[w_draw]), XtWindow(msv_w[w_draw]) );
		break;
	case w_entry_fk_1:
	case w_entry_fk_2:
	case w_entry_fk_3:
	case w_entry_fk_4:
	case w_entry_fk_5:
	case w_entry_fk_6:
	case w_entry_fk_7:
		sprintf( cmd, "$SH_UTIL/mapslow_fk.csh %d %s %f",
			(*tag)-w_entry_fk_1+1, msv_dsptime, msv_baz );
		XDefineCursor( XtDisplay(msv_w[w_draw]), XtWindow(msv_w[w_draw]), msv_crsr_busy );
		XFlush( XtDisplay(msv_w[w_draw]) );
		system( cmd );
		XUndefineCursor( XtDisplay(msv_w[w_draw]), XtWindow(msv_w[w_draw]) );
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

	if  (msv_pixinit)
		pix_manage_exposure( &(data->event->xexpose) );

} /* end of cb_call_expose */



/*--------------------------------------------------------------------------*/


#define POSTEXT_X -650
#define POSTEXT_Y 637
#define SELTEXT_X -230
#define TEXTSPACE 200
#define MARKWIDTH 11

#define MODIF_SHIFT 0x11
#define MODIF_LOCK  0x12
#define MODIF_CTRL  0x14
#define MODIF_ALT   0x18

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
	static int     sel_x, sel_y;        /* selected position */
	static GC      xgc;                 /* clear GC */
	static char    oldstr[BC_LINELTH+1]="";/* old string */
	static char    oldstr2[BC_LINELTH+1]="";/* old string */
	static char    oldsel[BC_LINELTH+1]="";/* old selection text */
	static int     eff_w, eff_h;        /* effective size of window */
	char     postext[BC_LINELTH+1];     /* position text */
	char     postext2[BC_LINELTH+1];    /* position text */
	char     seltext[BC_LINELTH+1];     /* selected time & slowness */
	Window   root;                      /* root window */
	int      w_x, w_y;                  /* window position */
	unsigned w_w, w_h;                  /* size of window */
   unsigned border, depth;             /* window pars (not used) */
	float    slow_x, slow_y;            /* slowness position */
	char     ctime[cBcTimeLth+1];       /* current absolute time */
	STATUS   locstat;                   /* local status */
	int      i;                         /* counter */
	Modifiers mret;                     /* return key */
	KeySym   keysym;                    /* key symbol */
	int      tag;                       /* key tag */

	/* executable code */

	if  (w != msv_w[w_draw])  {printf("--> motion: ill wdw\n"); return;}

	if  (!is_init)  {
		XGetGeometry( XtDisplay(w), XtWindow(w), &root, &w_x, &w_y, &w_w, &w_h,
			&border, &depth );
		pos_x = w_w - msv_dsp.margin_r + POSTEXT_X;
		pos_y = w_h - msv_dsp.margin_b - POSTEXT_Y;
		eff_w = w_w - msv_dsp.margin_l - msv_dsp.margin_r;
		eff_h = w_h - msv_dsp.margin_t - msv_dsp.margin_b;
		sel_x = w_w - msv_dsp.margin_r + SELTEXT_X;
		sel_y = pos_y;
		xgc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
		XCopyGC( XtDisplay(w), msv_gl[MscBLACK], GCFont, xgc );
		XSetForeground( XtDisplay(w), xgc, WhitePixel(XtDisplay(w),0) );
		XSetBackground( XtDisplay(w), xgc, BlackPixel(XtDisplay(w),0) );
		/* XSetFunction( XtDisplay(w), xgc, GXxor ); */
		is_init = TRUE;
	} /*endif*/

	if  (ev->type == MotionNotify)  {
		if  (ev->xmotion.x < msv_dsp.margin_l
			|| ev->xmotion.y < msv_dsp.margin_t
			|| ev->xmotion.x > msv_dsp.margin_l+eff_w
			|| ev->xmotion.y > msv_dsp.margin_t+eff_h)  {
			*postext = *postext2 = '\0';
		} else {
			slow_x = (float)(ev->xmotion.x - msv_dsp.margin_l);
			slow_y = (float)(ev->xmotion.y - msv_dsp.margin_t);
			slow_x = slow_x / (float)eff_w * msv_scale;
			slow_y = msv_area.minslow + msv_yscale
				- (slow_y / (float)eff_h * msv_yscale);
			locstat = cBcNoError;
			if  (*msv_abstime == '\0')  {
				sprintf( postext,  " curr. time: %6.2f", slow_x );
			} else {
				tc_tadd( msv_abstime, slow_x, ctime, &locstat );
				/* through out milliseconds */
				i = 0;
				while  (ctime[i] != '\0')  {
					if  (ctime[i] == '.')  {
						ctime[i] = '\0';
						break;
					} /*endif*/
					i++;
				} /*endwhile*/
				/* strcpy( postext, ctime ); */
				sprintf( postext, "%s  r:%6.1f", ctime, slow_x );
			} /*endif*/
			sprintf( postext2, " slowness  : %6.2f", slow_y );
		} /*endif*/
		if  (*oldstr != '\0')  {
			pix_DrawString( XtDisplay(w), XtWindow(w), xgc,
				pos_x, pos_y, oldstr, strlen(oldstr) );
			pix_DrawString( XtDisplay(w), XtWindow(w), xgc,
				pos_x+TEXTSPACE, pos_y, oldstr2, strlen(oldstr2) );
		} /*endif*/
		if  (*postext != '\0')  {
			pix_DrawString( XtDisplay(w), XtWindow(w), msv_gl[MscBLACK],
				pos_x, pos_y, postext, strlen(postext) );
			pix_DrawString( XtDisplay(w), XtWindow(w), msv_gl[MscBLACK],
				pos_x+TEXTSPACE, pos_y, postext2, strlen(postext2) );
		} /*endif*/
		strcpy( oldstr, postext );
		strcpy( oldstr2, postext2 );
		XFlush( XtDisplay(w) );
	} else if  (ev->type == ButtonPress) {
		if  (!(ev->xmotion.x < msv_dsp.margin_l
			|| ev->xmotion.y < msv_dsp.margin_t
			|| ev->xmotion.x > msv_dsp.margin_l+eff_w
			|| ev->xmotion.y > msv_dsp.margin_t+eff_h))  {
			slow_x = (float)(ev->xmotion.x - msv_dsp.margin_l);
			msv_dspslow = (float)(ev->xmotion.y - msv_dsp.margin_t);
			slow_x = slow_x / (float)eff_w * msv_scale;
			msv_dspslow = msv_area.minslow + msv_yscale
				- (msv_dspslow / (float)eff_h * msv_yscale);
			locstat = cBcNoError;
			tc_tadd( msv_abstime, slow_x, msv_dsptime, &locstat );
			/* through out milliseconds */
			i = 0;
			while  (msv_dsptime[i] != '\0')  {
				if  (msv_dsptime[i] == '.')  {
					msv_dsptime[i] = '\0';
					break;
				} /*endif*/
				i++;
			} /*endwhile*/
			/* manage display of selected time & slowness */
			sprintf( seltext, "selected: %5.2f @ %20s",
				msv_dspslow, msv_dsptime );
			if  (*oldsel != '\0')  {
				pix_DrawString( XtDisplay(w), XtWindow(w), xgc,
					sel_x, sel_y, oldsel, strlen(oldsel) );
			} /*endif*/
			pix_DrawString( XtDisplay(w), XtWindow(w), msv_gl[MscBLACK],
				sel_x, sel_y, seltext, strlen(seltext) );
			strcpy( oldsel, seltext );
			/* flash selected point */
			pix_FillRectangle( XtDisplay(w), XtWindow(w), msv_gl[MscSELECT],
				ev->xmotion.x-MARKWIDTH/2, ev->xmotion.y-MARKWIDTH/2,
				MARKWIDTH, MARKWIDTH );
			XFlush( XtDisplay(w) );
			usleep( 500000 );
			pix_FillRectangle( XtDisplay(w), XtWindow(w), msv_gl[MscSELECT],
				ev->xmotion.x-MARKWIDTH/2, ev->xmotion.y-MARKWIDTH/2,
				MARKWIDTH, MARKWIDTH );
			/* write to stdout */
			printf( "time %s  slowness %4.1f  azimuth %5.1f\n",
				msv_dsptime, msv_dspslow, msv_baz );
		} /*endif*/
	} else if  (ev->type == KeyPress) {
		XtTranslateKeycode( ev->xkey.display, ev->xkey.keycode, ev->xkey.state,
			&mret, &keysym );
		if  (ev->xkey.state == MODIF_CTRL)  {
			switch  ((char)keysym)  {
			case '1':   tag = w_entry_check_1;         break;
			case '2':   tag = w_entry_check_2;         break;
			case '3':   tag = w_entry_check_3;         break;
			case '4':   tag = w_entry_check_4;         break;
			case '5':   tag = w_entry_check_5;         break;
			case '6':   tag = w_entry_check_6;         break;
			case '7':   tag = w_entry_check_7;         break;
			default:    tag = 0;
				printf( "--> pressed key %d, keysym %ld\n", ev->xkey.keycode, keysym );
			} /*endswitch*/
		} else {
			switch  ((char)keysym)  {
			case '1':   tag = w_entry_show_1;         break;
			case '2':   tag = w_entry_show_2;         break;
			case '3':   tag = w_entry_show_3;         break;
			case '4':   tag = w_entry_show_4;         break;
			case '5':   tag = w_entry_show_5;         break;
			case '6':   tag = w_entry_show_6;         break;
			case '7':   tag = w_entry_show_7;         break;
			case 'q':   tag = w_entry_quit;           break;
			default:    tag = 0;
				printf( "--> pressed key %d, keysym %ld\n", ev->xkey.keycode, keysym );
			} /*endswitch*/
		} /*endif*/
	} /*endif*/

} /* end of cb_action_motion_cart */



/*--------------------------------------------------------------------------*/



static void ms_read_setup( char setup[], MstDetPar par[] )

/* Reads in display parameters for detection lists
 *
 * parameters of routine
 * char       setup[];        input; name of input file
 * MstDetPar  par[];          output; list of parameters
 */
{
	/* local variables */
	FILE     *fp;          /* pointer to input file */
	int      i;            /* counter */
	char     line[cBcLineLth+1];  /* current line of file */
	char     val[cBcLineLth+1];   /* current value */
	char     key[cBcLineLth+1];   /* current keyword */

	/* executable code */

	/* initialize setup pars */
	for  (i=0; i<MscMAX_DETFILES; i++)  {
		par[i].aztol = 30.0;
		par[i].minqual = 1.0e2;
		par[i].maxqual = 1.0e5;
		par[i].sloint = 0.25;
		par[i].minrelpow = 0.0;
		par[i].minabspow = 0.0;
		par[i].mineval = 0.0;
		par[i].eval_avlth = 10;
		par[i].m_q = 0.0;
		par[i].m_r = 0.0;
		par[i].m_a = 0.0;
		par[i].m_qa = 0.0;
		par[i].m_qr = 0.0;
		par[i].m_ra = 0.0;
		par[i].m_qra = 0.0;
		par[i].col_r = 1.0;
		par[i].col_g = 0.0;
		par[i].col_b = 0.0;
		par[i].text[0] = '\0';
	} /*endfor*/

	fp = fopen( setup, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "mapslow: cannot find input file %s\n", setup );
		exit( 1 );
	} /*endif*/

	/* read through file */
	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		if  (sscanf(line,"%d %s %s",&i,key,val) != 3)  {
			fprintf( stderr, "mapslow: syntax error in file %s:\n", setup );
			fprintf( stderr, "%s", line );
			exit( 1 );
		} /*endif*/
		i--;
		if  (i < 0 || i >= MscMAX_DETFILES)  {
			fprintf( stderr, "mapslow: illegal detlist number %d\n", i );
			continue;
		} /*endif*/
		if  (strcmp(key,"aztol") == 0)  {
			sscanf( val, "%f", &(par[i].aztol) );
		} else if  (strcmp(key,"minqual") == 0)  {
			sscanf( val, "%f", &(par[i].minqual) );
		} else if  (strcmp(key,"maxqual") == 0)  {
			sscanf( val, "%f", &(par[i].maxqual) );
		} else if  (strcmp(key,"sloint") == 0)  {
			sscanf( val, "%f", &(par[i].sloint) );
		} else if  (strcmp(key,"minrelpow") == 0)  {
			sscanf( val, "%f", &(par[i].minrelpow) );
		} else if  (strcmp(key,"minabspow") == 0)  {
			sscanf( val, "%f", &(par[i].minabspow) );
		} else if  (strcmp(key,"mineval") == 0)  {
			sscanf( val, "%f", &(par[i].mineval) );
		} else if  (strcmp(key,"eval_avlth") == 0)  {
			sscanf( val, "%d", &(par[i].eval_avlth) );
		} else if  (strcmp(key,"mul_q") == 0)  {
			sscanf( val, "%f", &(par[i].m_q) );
		} else if  (strcmp(key,"mul_r") == 0)  {
			sscanf( val, "%f", &(par[i].m_r) );
		} else if  (strcmp(key,"mul_a") == 0)  {
			sscanf( val, "%f", &(par[i].m_a) );
		} else if  (strcmp(key,"mul_qr") == 0)  {
			sscanf( val, "%f", &(par[i].m_qr) );
		} else if  (strcmp(key,"mul_qa") == 0)  {
			sscanf( val, "%f", &(par[i].m_qa) );
		} else if  (strcmp(key,"mul_ra") == 0)  {
			sscanf( val, "%f", &(par[i].m_ra) );
		} else if  (strcmp(key,"mul_qra") == 0)  {
			sscanf( val, "%f", &(par[i].m_qra) );
		} else if  (strcmp(key,"color") == 0)  {
			sscanf( val, "%f,%f,%f", &(par[i].col_r), &(par[i].col_g),
				&(par[i].col_b) );
		} else if  (strcmp(key,"text") == 0)  {
			strcpy( par[i].text, val );
		} else {
			fprintf( stderr, "mapslow: illegal key entry in setup: %s\n", key );
		} /*endif*/
	} /*endwhile*/

	fclose( fp );

} /* end of ms_read_setup */



/*--------------------------------------------------------------------------*/




static void ms_prepare_gc( Widget w, MstDetPar par[], GC gl[] )

/* creates GC's for display
 *
 * parameters of routine
 * Widget     w;          input; widget of drawing area
 * MstDetpar  *par;       input/output(gc); display setup
 * GC         *bg;        output; background color
 * GC         *lab;       output; used for labelling
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
			msv_pname );
		exit( 1 );
	} /*endif*/

	i = 5;
	while  (!XMatchVisualInfo(XtDisplay(w),screen_num,default_depth,
		i--,&visual_info))
		;

	/*printf( "%s: found a %s class visual at default depth %d\n",
		msv_pname, visual_class[++i] ); */
	if  (i < StaticColor)  {
		fprintf( stderr, "%s: visual class %s is not supported\n",
			msv_pname, visual_class[i] );
		exit( 1 );
	} /*endif*/

#ifdef XXX
	if  (visual_info.visual != default_visual)
		printf( "%s: %s class visual at default depth is not default visual\n",
			msv_pname, visual_class[i] );
#endif

	for  (i=0; i<MscMAX_DETFILES; i++)  {
		color.red = Nint( par[i].col_r * 65535.0 );
		color.green = Nint( par[i].col_g * 65535.0 );
		color.blue = Nint( par[i].col_b * 65535.0 );
		color.flags = DoRed | DoGreen | DoBlue;
		if  (!XAllocColor(XtDisplay(w),default_cmap,&color))  {
			fprintf( stderr, "%s: error allocating color\n", msv_pname );
			exit( 1 );
		} /*endif*/
		par[i].gc = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
		XSetForeground( XtDisplay(w), par[i].gc, color.pixel );
		XSetBackground( XtDisplay(w), par[i].gc,
			WhitePixel(XtDisplay(w),screen_num) );
	} /*endfor*/

	out_font = XLoadFont( XtDisplay(w),
		"-b&h-lucidatypewriter-bold-r-normal-sans-*-100-*-*-*-*-*-1" );

#ifdef COL_BLUE
	color.red = 0;
	color.green = 0;
	color.blue = 65535;
#else
	color.red = 65535;
	color.green = 65535;
	color.blue = 65535;
#endif
	color.flags = DoRed | DoGreen | DoBlue;
	if  (! XAllocColor(XtDisplay(w), default_cmap, &color ))  {
		fprintf( stderr, "%s error allocating color\n", msv_pname );
		exit( 1 );
	} /*endif*/
	gl[MscBG] = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetForeground( XtDisplay(w), gl[MscBG], color.pixel );
	XSetBackground( XtDisplay(w), gl[MscBG],
		WhitePixel(XtDisplay(w),screen_num) );
	XSetFont( XtDisplay(w), gl[MscBG], out_font );

#ifdef COL_BLUE
	color.red = 65535;
	color.green = 65535;
	color.blue = 0;
#else
	color.red = 0;
	color.green = 0;
	color.blue = 0;
#endif
	color.flags = DoRed | DoGreen | DoBlue;
	if  (! XAllocColor(XtDisplay(w), default_cmap, &color ))  {
		fprintf( stderr, "%s error allocating color\n", msv_pname );
		exit( 1 );
	} /*endif*/
	gl[MscYELLOW] = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetForeground( XtDisplay(w), gl[MscYELLOW], color.pixel );
	XSetBackground( XtDisplay(w), gl[MscYELLOW],
		WhitePixel(XtDisplay(w),screen_num) );
	XSetFont( XtDisplay(w), gl[MscYELLOW], out_font );

	color.red = 0;
	color.green = 0;
	color.blue = 0;
	color.flags = DoRed | DoGreen | DoBlue;
	if  (! XAllocColor(XtDisplay(w), default_cmap, &color ))  {
		fprintf( stderr, "%s error allocating color\n", msv_pname );
		exit( 1 );
	} /*endif*/
	gl[MscSELECT] = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetFunction( XtDisplay(w), gl[MscSELECT], GXxor );
	XSetForeground( XtDisplay(w), gl[MscSELECT], color.pixel );
	XSetBackground( XtDisplay(w), gl[MscSELECT],
		WhitePixel(XtDisplay(w),screen_num) );
	XSetFont( XtDisplay(w), gl[MscSELECT], out_font );

	gl[MscBLACK] = XCreateGC( XtDisplay(w), XtWindow(w), 0, NULL );
	XSetForeground( XtDisplay(w), gl[MscBLACK],
		BlackPixel(XtDisplay(w),screen_num) );
	XSetBackground( XtDisplay(w), gl[MscBLACK],
		 WhitePixel(XtDisplay(w),screen_num) );
	XSetFont( XtDisplay(w), gl[MscBLACK], out_font );

} /* end of ms_prepare_gc */



/*--------------------------------------------------------------------------*/



static void ms_draw_background( Widget w, MstDspSetup *dsp, float *tscale,
	float *sscale )

/* Draws background
 *
 * parameters of routine
 * Widget       w;         input; drawing widget
 * MstDspSetup  *dsp;      input; display settings
 * float        *tscale;   output; scaling pixel to time units
 * float        *sscale;   output; scaling pixel to slowness units
 */
{
	/* local variables */
	Window   root;        /* root window */
	int      w_x, w_y;    /* position of window */
	unsigned w_w, w_h;    /* size of window */
	unsigned border;      /* border width */
	unsigned depth;       /* depth of window */
	int      rect_width;  /* width of rectangle */
	int      rect_height; /* height of rectangle */

	/* executable code */

	if  (MsfXWDW & msv_flags)  {

		XGetGeometry( XtDisplay(w), XtWindow(w), &root, &w_x, &w_y, &w_w, &w_h,
			&border, &depth );

		/* get width and height of rectangle */
		rect_width = w_w - (dsp->margin_l) - (dsp->margin_r);
		rect_height = w_h - (dsp->margin_t) - (dsp->margin_b);

		pix_FillRectangle( XtDisplay(w), XtWindow(w),
			msv_gl[MscBG], dsp->margin_l, dsp->margin_t,
			rect_width, rect_height );

		*tscale = msv_area.timewidth / (float)rect_width;
		*sscale = msv_area.slowheight / (float)rect_height;
		msv_wdw_height = w_h;

	} /*endif*/

} /* end of ms_draw_background */



/*--------------------------------------------------------------------------*/



static void ms_trafo( float t, float s, int *x, int *y )

/* Converts time and slowness to pixel coordinates
 *
 * parameters of routine
 * float      t, s;    input; time and slowness
 * int        *x, *y;  output; pixel coordinates (if not NULL)
 */
{
	/* executable code */

	if  (x != NULL)
		*x = msv_dsp.margin_l + Nint(t/msv_tscale);

	if  (y != NULL)
		*y = msv_wdw_height
			- (msv_dsp.margin_b + Nint((s-msv_area.minslow)/msv_sscale));

} /* end of ms_trafo */



/*--------------------------------------------------------------------------*/



static void ms_draw_axes( Widget w, MstDspSetup *dsp, MstMapArea *area, GC gc,
	char text[] )

/* Draws axes and labelling
 *
 * parameters of routine
 * Widget       w;           input; drawing widget
 * MstDspSetup  *dsp;        input; display settings
 * GC           gc;          input; drawing attributes
 */
{
	/* local variables */
	int      ixa, iya, ixb, iyb;     /* pixel coordinates */
	float    step;                   /* label steps */
	float    x;                      /* current value */
	char     str[cBcLineLth+1];      /* output string */

	/* executable code */

	if  (MsfXWDW & msv_flags)  {

		ms_trafo( 0.0, area->minslow, &ixa, &iya );
		ms_trafo( area->timewidth, area->minslow, &ixb, NULL );
		ixb--;
		pix_DrawLine( XtDisplay(w), XtWindow(w), gc, ixa, iya, ixb, iya );

		step = 10.0;
		if  (area->timewidth > 100.0)  step = 50.0;
		if  (area->timewidth > 500.0)  step = 200.0;
		if  (area->timewidth > 1000.0)  step = 500.0; 
		if  (area->timewidth > 5000.0)  step = 2000.0; 
		if  (area->timewidth > 10000.0)  step = 5000.0; 
		if  (area->timewidth > 50000.0)  step = 20000.0; 

		x = 0.0;
		while  (x <= area->timewidth)  {
			ms_trafo( x, area->minslow, &ixa, &iya );
			iyb = iya + 5;
			pix_DrawLine( XtDisplay(w), XtWindow(w), gc, ixa, iya, ixa, iyb );
			sprintf( str, "%d", Nint(x) );
			pix_DrawString( XtDisplay(w), XtWindow(w), gc,
				ixa-10, iyb+10, str, strlen(str) );
			x += step;
		} /*endwhile*/

		ms_trafo( 0.0, area->minslow-2.0, &ixa, &iya );
		pix_DrawString( XtDisplay(w), XtWindow(w), gc, ixa, iya, text,
			strlen(text));

		ms_trafo( 0.0, msv_area.minslow, &ixa, &iya );
		ms_trafo( 0.0, area->slowheight+area->minslow, NULL, &iyb );
		ixb--;
		pix_DrawLine( XtDisplay(w), XtWindow(w), gc, ixa, iya, ixa, iyb );
		step = 1.0;
		if  (area->slowheight > 5.0)  step = 2.0;
		if  (area->slowheight > 10.0)  step = 5.0;
		if  (area->slowheight > 100.0)  step = 20.0;

		x = (float)((int)(msv_area.minslow / 5.0)) * 5.0;
		while  (x <= (area->slowheight + area->minslow))  {
			ms_trafo( 0.0, x, &ixa, &iya );
			ixb = ixa - 5;
			pix_DrawLine( XtDisplay(w), XtWindow(w), gc, ixa, iya, ixb, iya );
			sprintf( str, "%d", Nint(x) );
			pix_DrawString( XtDisplay(w), XtWindow(w), gc,
				ixb-20, iya+4, str, strlen(str) );
			x += step;
		} /*endwhile*/

		ms_trafo( -area->timewidth * 0.03,
			area->minslow + area->slowheight * 1.02, &ixa, &iya );
		strcpy( str, "Slowness (s/deg)" );
		pix_DrawString( XtDisplay(w), XtWindow(w), gc,
			ixa, iya, str, strlen(str) );
		ms_trafo( area->timewidth * 1.03,
			area->minslow - area->slowheight * 0.03, &ixa, &iya );
		strcpy( str, "Time (s)" );
		pix_DrawString( XtDisplay(w), XtWindow(w), gc,
			ixa, iya, str, strlen(str) );

	} /*endif*/

	if  (MsfGMT & msv_flags)  {
		printf( "psbasemap -R0/5000/0/30 -JX6/4 -Bf1000/f5 -K\n" );
	} /*endif*/

} /* end of ms_draw_axes */



/*--------------------------------------------------------------------------*/



static ms_put_phases( Widget w, GC gc, float dist, float depth,
	TSyBoolean lab )

/* Puts theoretical phases on the display
 *
 * parameters of the routine
 * Widget     w;      input; drawing widget
 * GC         gc;     input; attributes
 * MstDetPar  par[];  input; display parameters (color)
 * float      dist;   input; distance of event
 * float      depth;  input; depth of event
 */
{
	/* local variables */
	int      i;        /* phase counter */
	TSyStatus status;  /* return status */
	float    travt;    /* travel time of phase */
	float    slo;      /* slowness of phase */
	int      ix, iy;   /* output coordinates */
	char     str[cBcLineLth+1]; /* output string */
	int      xoff, yoff; /* x, yoffset */

	/* executable code */

	i = 0;
	while  (msv_phases[i][0] != '\0')  {
		/*printf( "--> %s ", msv_phases[i] );*/
		/* show PKiKP only, if it is closer than 113 deg */
		if  (strcmp(msv_phases[i],"PKiKP") == 0 && dist > 113.0)  {
			i++;
			continue;
		} /*endif*/
		status = cBcNoError;
		travt = pt_travel( msv_phases[i], dist, depth, &status );
		if  (status == cBcNoError)
			slo = pt_slowness( msv_phases[i], dist, depth, &status );
			if  (msv_area.minslow == 0.0)
				slo = fabs( slo );
		if  (status == cBcNoError)  {
			/*printf( "%f %f", travt, slo );*/
			if  (travt < msv_area.timewidth && slo < msv_area.slowheight)  {
				if  (MsfGMT & msv_flags)  {
					printf( "pstext -K -O -R -G#PHASECOL# -JX6/4 <<END\n" );
					printf( "%f %f 10 0.0 4 9 %s\n", travt, slo, msv_phases[i] );
					printf( "END\n" );
					printf( "psxy -K -O -Sc0.1 -R -G#PHASECOL# -JX6/4 <<END\n" );
					printf( "%f %f\n", travt, slo );
					printf( "END\n" );
				} /*endif*/
				if  (MsfXWDW & msv_flags)  {
					ms_trafo( travt, slo, &ix, &iy );
					pix_FillRectangle( XtDisplay(w), XtWindow(w), gc, ix-1,
						iy+1, 3, 3 );
					if  (lab)  {
						strcpy( str, msv_phases[i] );
						xoff = -2;
						yoff = -2;
						if  (strncmp(str,"PxPx",4) == 0)  {
							str[1] = str[3] = '\'';
							xoff = 4;
							yoff = 8;
						} else if  ((strncmp(str,"SKP",3) == 0) ||
							(strncmp(str,"SKKP",4) == 0) ||
							(strncmp(str,"Sdif",4) == 0))  {
							yoff = 14;
						} /*endif*/
						pix_DrawString( XtDisplay(w), XtWindow(w), gc,
							ix+xoff, iy+yoff, str, strlen(str) );
					} /*endif*/
				} /*endif*/
			} /*endif*/
		} /*endif*/
		/*printf( "\n" );*/
		i++;
	} /*endwhile*/

} /* end of ms_put_phases */



/*--------------------------------------------------------------------------*/



#define MAXAVLTH 50
#define LEGENDWIDTH 90


static void ms_put_detfile( Widget w, char detfile[], MstDetPar *detpar,
	float thbaz, GC gc[] )

/* Displays detections
 *
 * parameters of routine
 * Widget     w;            input; display widget
 * char       detfile[];    input; detection file
 * MstDetPar  *detpar;      input; detection display parameters
 * float      thbaz;        input; theoretical back azimuth
 * GC         gc[];         input; color table
 */
{
	/* local variables */
	static int ypos=200;             /* text y position */
	static int lcnt=1;               /* list counter */
	FILE     *fp;                    /* pointer to file */
	char     line[cBcLineLth+1];     /* current line in file */
	char     timestr[cBcLineLth+1];  /* time string */
	float    qual;                   /* quality number */
	float    slo;                    /* slowness */
	float    baz;                    /* back azimuth */
	float    relpow;                 /* relative power */
	float    abspow;                 /* absolute power */
	float    reltime;                /* relative time in s */
	float    dt;                     /* time distance between detections */
	float    bazdiff;                /* deviation in back azimuth */
	float    eval;                   /* evaluation number */
	float    movav;                  /* moving average */
	float    smpbuf[MAXAVLTH];       /* sample buffer */
	int      smpcnt;                 /* sample counter */
	int      smpidx;                 /* sample index for sample buffer */
	int      i;                      /* counter */
	TSyStatus status;                /* return status */
	TSyBoolean do_output;            /* plot detection */
	int      ixa, ixb, iya, iyb;     /* output coordinates */
	int      wd, hg;                 /* width and height of marker */
	TSyBoolean negslow;              /* negative slowness = azimuth + 180 */
	TSyBoolean debug;                /* debug flag */
	/* for XGet Geometry */
	Window   root;                   /* root window */
	int      w_x, w_y;               /* window position */
	unsigned w_w, w_h;               /* size of window */
   unsigned border, depth;          /* window pars (not used) */

	/* executable code */

	debug = (MsfDEBUG & msv_flags);

	fp = fopen( detfile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: cannot open detection file %s\n",
			msv_pname, detfile );
		return;
	} /*endif*/

	status = cBcNoError;

	/* find time distance between detections */
	fgets( line, cBcLineLth, fp );
	fgets( line, cBcLineLth, fp );
	strcpy( timestr, line+4 );
	i = 2;
	while  (timestr[i] != ' ' && timestr[i] != '\0')  i++;
	timestr[i] = '\0';
	fgets( line, cBcLineLth, fp );
	i = 6;
	while  (line[i] != ' ' && line[i] != '\0')  i++;
	line[i] = '\0';
	dt = tc_tdiff( line+4, timestr, &status );
	if  (SySevere(&status))  {
		fprintf( stderr, "%s: error converting time in %s\n",
			msv_pname, detfile );
		exit( 1 );
	} /*endif*/
	rewind( fp );

	if  (MsfGMT & msv_flags)  {
		printf( "psxy -K -O -JX6/4 -G#COL%02d# -R -Ss <<END\n", lcnt );
	} /*endif*/

	if  (detpar->eval_avlth > MAXAVLTH)  {
		fprintf( stderr, "mapslow: average length too large.  Abort.\n" );
		exit( 1 );
	} /*endif*/

	/* read through all lines of file */
	movav = 0.0;
	smpcnt = 0;
	smpidx = 0;
	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		if  (sscanf(line+4,"%s %f %f %f %f %f",
			timestr,&qual,&slo,&baz,&relpow,&abspow) != 6)  {
			fprintf( stderr, "%s: error reading file %s\n", msv_pname, detfile );
			exit( 1 );
		} /*endif*/
		if  (debug)  printf( "%s %d  ", timestr, 8-lcnt );
		reltime = tc_tdiff( timestr, msv_abstime, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error converting time in %s\n",
				msv_pname, detfile );
			exit( 1 );
		} /*endif*/
		negslow = FALSE;
		bazdiff = fabs( baz - thbaz );
		if  (bazdiff > 180.0)  bazdiff = fabs(bazdiff - 360.0);
		if  (bazdiff > 90.0)  {
			bazdiff = fabs(bazdiff - 180.0);
			negslow = TRUE;
		} /*endif*/
		do_output = (reltime >= 0.0);
		if  (debug)  printf( "%d", do_output );
		do_output = do_output && (reltime < msv_area.timewidth);
		if  (debug)  printf( "%d", do_output );
		do_output = do_output && (qual > detpar->minqual);
		if  (debug)  printf( "%d", do_output );
		if  (slo > 1.5)  {
			do_output = do_output && (bazdiff < detpar->aztol );
		} else if  (slo > 1.0)  {
			do_output = do_output && (bazdiff < 2.0*(detpar->aztol) );
		} /*endif*/
		if  (debug)  printf( "%d", do_output );
		if  (msv_area.minslow != 0.0 && negslow)  slo = -slo;
		do_output = do_output && (slo < (msv_area.minslow+msv_area.slowheight));
		if  (debug)  printf( "%d", do_output );
		if  (msv_area.minslow != 0.0)
			do_output = do_output && (slo > msv_area.minslow);
		if  (debug)  printf( "%d", do_output );
		do_output = do_output && (relpow > detpar->minrelpow);
		if  (debug)  printf( "%d", do_output );
		do_output = do_output && (abspow > detpar->minabspow);
		if  (debug)  printf( "%d", do_output );
		if  (qual > detpar->maxqual)  qual = detpar->maxqual;
		eval = detpar->m_q * qual + detpar->m_r * relpow + detpar->m_a * abspow
			+ detpar->m_qr * qual * relpow + detpar->m_qa * qual * abspow
			+ detpar->m_ra * relpow * abspow + detpar->m_qra * qual * relpow * abspow;
		/* remove average of the last n values */
		smpcnt++;
		if (smpcnt <= detpar->eval_avlth)  {
			if  (smpcnt == 1)  {
				movav = eval;
			} else {
				movav = (movav*(float)(smpcnt-1) + eval) / (float)smpcnt;
			} /*endif*/
			smpbuf[smpidx++] = eval;
		} else {
			i = smpidx - detpar->eval_avlth;
			if  (i < 0)  i += detpar->eval_avlth;
			movav += eval/(float)(detpar->eval_avlth)
				- smpbuf[i]/(float)(detpar->eval_avlth);
			if  (smpidx >= detpar->eval_avlth)  smpidx = 0;
			smpbuf[smpidx++] = eval;
		} /*endif*/
		eval -= movav;
		if  (MsfDUMPEVAL & msv_flags)
			printf( "%s %d %g\n", timestr, 8-lcnt, eval );
		do_output = do_output && (eval >= detpar->mineval);
		if  (do_output)  {
			/*printf( "%f %e %f %f\n", reltime, qual, slo, baz );*/
			if  (MsfXWDW & msv_flags)  {
				ms_trafo( reltime, slo+detpar->sloint, &ixa, &iya );
				ms_trafo( reltime+dt, slo-detpar->sloint, &ixb, &iyb );
				wd = ixb - ixa;
				if  (wd < 1)  wd = 1;
				hg = iyb - iya;
				if  (hg < 1)  hg = 1;
				pix_FillRectangle( XtDisplay(w), XtWindow(w), detpar->gc,
					ixa, iya, wd, hg );
			} /*endif*/
			if  (MsfGMT & msv_flags)  {
				float wd;  /* width */
				if  (lcnt == 7)  wd = 0.05;
				else if  (lcnt > 3 && lcnt < 7)  wd = 0.1;
				else  wd = 0.15;
				printf( "%f %f %f\n", reltime, slo, wd );
			} /*endif*/
		} /*endif*/
		if  (debug)  printf( "\n" );
	} /*endwhile*/

	if  (MsfGMT & msv_flags)  {
		printf( "END\n" );
	} /*endif*/

	fclose( fp );

	/* legend */
	if  (MsfXWDW & msv_flags)  {
		XGetGeometry( XtDisplay(w), XtWindow(w), &root, &w_x, &w_y, &w_w, &w_h,
			&border, &depth );
		ixa = w_w - LEGENDWIDTH;
		pix_FillRectangle( XtDisplay(w), XtWindow(w), detpar->gc,
			ixa, ypos, 5, 5 );
		pix_DrawString( XtDisplay(w), XtWindow(w), msv_gl[MscBLACK],
			ixa+8, ypos+5, detpar->text, strlen(detpar->text) );
		ypos -= 20;
	} /*endif*/

	lcnt++;

} /* end of ms_put_detfile */



/*--------------------------------------------------------------------------*/

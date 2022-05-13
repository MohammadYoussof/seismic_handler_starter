
/* File GEMCH.C
 *      =======
 *
 * version 18, 2-Dec-92
 *
 * GEM channel of graphic output
 * K. Stammler, 9-AUG-1991
 */


#include <string.h>
#include <aes.h>
#include <vdi.h>
#include <tos.h>
#include <stdio.h>
#include BASECNST
#include BC_ASCII
#include BC_SYSBASE
/* #include "vdivars.h" */
#include "gmusrdef.h"
#include "gmerrors.h"
#include "graphbas.h"
/* #include "playback.h" */

#ifdef BC_KS_PRIVATE
#include "e:\pc\util\pxusrdef.h"
#endif

#define USE_EVENT_MANAGER   /* use event manager */
#define USE_MENU_BAR        /* display menu bar */

#ifdef USE_EVENT_MANAGER
#include "evusrdef.h"
#define gm_read_character(c) ev_read_character(c,FALSE)
#else
#define gm_read_character gmh_rdch
#endif


/* global constants */
#define CLIPON      1
#define CLIPOFF     0
#define WORDSIZE    16          /* wordsize in bits */
#define XWRITEOFF   1           /* x offset for text output */
#define YWRITEOFF   2           /* y offset for text output */
#define WDWNAMLTH   30          /* max. length of window names */

#define LSTYLES 10              /* maximum number of line style blocks */
#define CSTYLES 10              /* maximum number of char style blocks */
#define DSTYLE  0               /* default style block */


#define gmh_inbuf()  Bconstat(2) /* character in buffer */
#define gmh_wdwopen(a) (((a) >= 0) && (gmv_wdwid[a] != -1))
#define gmh_illwdw(w) (((w) <= 0) || ((w) >= GBC_MAXWDW))

typedef struct lstyblock {
   int      wrmode;     /* writing mode */
   int      lntype;     /* line style */
   int      lnpatt;     /* line pattern of user defined style */
   int      lnwidth;    /* line width */
   int      lncolor;    /* line color */
} LSTYLE;

typedef struct cstyblock {
   int      wrmode;     /* writing mode */
   int      cheight;    /* character height */
   int      angle;      /* character rotation */
   int      cfont;      /* char font */
   int      ceffects;   /* special effects */
   int      ccolor;     /* char color */
} CSTYLE;

/* global variables */
/* static int    gmv_apid; */              /* application identifier */
static int       gmv_wrkinit={FALSE};      /* workstation initialized */
static int       gmv_vdid;                 /* virtual display identifier */
static int       gmv_wdwid[GBC_MAXWDW]={-1,-1,-1,-1,-1,-1,-1};/* wdw identifiers */
static GBC_COO   gmv_txoff[GBC_MAXWDW];    /* transformation parameters */
static GBC_COO   gmv_txfac[GBC_MAXWDW];
static GBC_COO   gmv_tyoff[GBC_MAXWDW];    /* transformation parameters */
static GBC_COO   gmv_tyfac[GBC_MAXWDW];
static GBC_COO   gmv_xpos[GBC_MAXWDW];     /* current graphic position */
static GBC_COO   gmv_ypos[GBC_MAXWDW];
static int       gmv_xwrite[GBC_MAXWDW];   /* current write position in window */
static int       gmv_ywrite[GBC_MAXWDW];
static int       gmv_boxh;                 /* height of character box */
static int       gmv_boxw;                 /* width of character box */
static int       gmv_tcrsr={FALSE};        /* text cursor shown */
static char      gmv_crsrch={5};           /* text cursor character */
static LSTYLE    gmv_lstyle[LSTYLES];      /* line style blocks */
static CSTYLE    gmv_cstyle[CSTYLES];      /* char style blocks */
static int       gmv_currlstyle={0};       /* currently used line style block */
static int       gmv_currcstyle={0};       /* currently used char style block */
static char      gmv_scratchdir[BC_LINELTH+1]
                 ={"e:\\pc\\sh\\scratch\\sh$"}; /* scratch directory */
static char      gmv_lastline[GBC_MAXWDW][BC_LINELTH+1];   /* last line written */
static int       gmv_init;                 /* initialised windows */
static int       gmv_screenw;              /* screen width in pixel */
static int       gmv_screenh;              /* screen height in pixel */
static float     gmv_wpixelpercm;          /* horizontal pixels/cm */
static float     gmv_hpixelpercm;          /* vertical pixels/cm */

#ifdef USE_MENU_BAR
static BOOLEAN   gmv_rsrcloaded;           /* resource file loaded */
static OBJECT    *gmv_menutree;            /* menu tree */
#endif /* USE_MENU_BAR */

static char   wdwname_gcv[GBC_MAXWDW][WDWNAMLTH+1] /* window names */
	= {" Wdw 0 "," Wdw 1 "," Wdw 2 "," Wdw 3 "," Wdw 4 "," Wdw 5 "," Wdw 6 "};


/* prototypes of local routines */
static void gmh_transform( int wdw, GBC_COO ux, GBC_COO uy, int *dx, int *dy );
static void gmh_backtrans( int wdw, int dx, int dy, GBC_COO *ux, GBC_COO *uy );
static void gmh_newline( int wdw );
static void gmh_carriage_return( int wdw );
static void gmh_scroll( int wdw );
static void gmh_erase( int wdw );
static void gmh_tstwdw( int wdw );
static void gmh_txtcrsr( int wdw, int state );
static void gmh_line( int wdw, int x0, int y0, int x1, int y1 );
static void gmh_lstyle( unsigned int style );
static void gmh_cstyle( unsigned int style );
static void gmh_rdch( char *ch );
static void gmh_crsr( void );


/*------------------------------------------------------------------------*/



void gm_init( int wdw, int attribs, GBC_COO xlo, GBC_COO ylo,
	GBC_COO width, GBC_COO height, STATUS *status )

/* initialises GEM
 *
 * parameters of routine
 * int        wdw;       input; window to be initialised
 * int        attribs;   input; window attributes
 * GBC_COO    xlo;       input; x-coo of lower left corner
 * GBC_COO    ylo;       input; y-coo of lower left corner
 * GBC_COO    width;     input; widths of window
 * GBC_COO    height;    input; height of window
 * STATUS     *status;   output; return status
 */
{
   /* local variables */
   int      i;            /* counter */
   int      work_in[11];  /* workstation setup */
   int      work_out[57]; /* output */
   int      ix, iy;       /* dummy */
   int      w[4];         /* window workspace */
	int      fontnumber;   /* number of font */
	char     ch;           /* scratch */
	char     rsrcname[BC_FILELTH+1];  /* name of resource file */
	char     rsrcf[BC_FILELTH+1];     /* translated name */
	int      msg[16];      /* dummy message buffer */

   /* executable code */

	if  (gmh_illwdw(wdw))  return;   /* should not occur */

   if  (((1<<wdw) & gmv_init) != 0)  {
      *status = GME_INITWICE;
      return;
   } else if  (wdw >= 0)  {
      if  (gmv_wdwid[wdw] != -1)  {
         *status = GME_INITWICE;
         return;
      } /*endif*/
   } /*endif*/

   if  (!gmv_wrkinit)  {
      if  (syv_apid == 0)  syv_apid = appl_init();
      for  ( i=0; i<10; work_in[i++]=1 );
      work_in[10] = 2;
      v_opnvwk( work_in, &gmv_vdid, work_out );
      gmv_wrkinit = TRUE;
      v_hide_c( gmv_vdid );
      graf_mouse( HOURGLASS, 0L );
		fontnumber = 1;
#		ifdef  BC_KS_PRIVATE
		if  (vq_gdos())  {
			vst_load_fonts( gmv_vdid, 0 );
			fontnumber = vqt_name( gmv_vdid, 2, rsrcname );
			vst_font( gmv_vdid, fontnumber );
		} /*endif*/
#		endif  /* BC_KS_PRIVATE */
      /* v_clrwk( gmv_vdid ); */
      vsf_color( gmv_vdid, 0 );
      graf_handle( &gmv_boxw, &gmv_boxh, &ix, &iy );
		gmv_screenw = work_out[0]+1;
		gmv_screenh = work_out[1]+1;
		gmv_wpixelpercm = 1.0 / (float)(work_out[3]) * 1.e4;
		gmv_hpixelpercm = 1.0 / (float)(work_out[4]) * 1.e4;
      for  (i=0;i<LSTYLES;i++)  {
         gmv_lstyle[i].wrmode = GBC_REPLACE;
         gmv_lstyle[i].lntype = 1;
         gmv_lstyle[i].lnpatt = 0xaaaa;
         gmv_lstyle[i].lnwidth = 1;
         gmv_lstyle[i].lncolor = 1;
      } /*endfor*/
      for  (i=0;i<CSTYLES;i++)  {
         gmv_cstyle[i].wrmode = GBC_REPLACE;
         gmv_cstyle[i].cheight = 13;
         gmv_cstyle[i].angle = 0;
         gmv_cstyle[i].cfont = fontnumber;
         gmv_cstyle[i].ceffects = 0;
         gmv_cstyle[i].ccolor = 1;
      } /*endfor*/
      gmh_cstyle( DSTYLE );
      gmh_lstyle( DSTYLE );
#		ifdef USE_MENU_BAR
		strcpy( rsrcname, gmv_scratchdir );
		strcat( rsrcname, "RSRC.RSC" );
		fo_translate( rsrcname, TRUE, BC_LINELTH, rsrcf, &i );
		gmv_rsrcloaded = rsrc_load( rsrcf );
		if  (!gmv_rsrcloaded)  {
			while  (i == FOC_ANOTHER)  {
				fo_translate( "", FALSE, BC_LINELTH, rsrcf, &i );
				gmv_rsrcloaded = rsrc_load( rsrcf );
				if  (gmv_rsrcloaded)  break;
			} /*endwhile*/
		} /*endif*/
		if  (!gmv_rsrcloaded)  {
			*status = GME_NORSRC;
		} else {
			rsrc_gaddr( 0, 0, &gmv_menutree );
			menu_bar( gmv_menutree, TRUE );
		} /*endif*/
#		endif /* USE_MENU_BAR */
   } else {
      v_hide_c( gmv_vdid );
   } /*endif*/

   if  (wdw >= 0)  {
      w[2] = (int)(width*gmv_wpixelpercm);
      w[3] = (int)(height*gmv_hpixelpercm);
      w[0] = (int)(xlo*gmv_wpixelpercm);
      w[1] = gmv_screenh - (int)(ylo*gmv_hpixelpercm) - w[3];

		/* work area size given */
		wind_calc( WC_BORDER, attribs, w[0], w[1], w[2], w[3],
			w, w+1, w+2, w+3 );

      if  (w[2] <=0)  w[2] = gmv_screenw;
      if  (w[3] <=0)  w[3] = gmv_screenh;
      if  ((w[0] < 0) || (w[0] >= gmv_screenw))  w[0] = 0;
      if  ((w[0]+w[2]) > gmv_screenw)  w[2] = gmv_screenw - w[0];
      if  ((w[1] < 0) || (w[1] >= gmv_screenh))  w[1] = 0;
      if  ((w[1]+w[3]) > gmv_screenh)  w[3] = gmv_screenh - w[1];

      gmv_wdwid[wdw] = wind_create( attribs, 0, 19, gmv_screenw-1,
			gmv_screenh-20 );
      if  (gmv_wdwid[wdw] < 0)  {
         gmv_wdwid[wdw] = -1;
         *status = GME_NOWDW;
         v_show_c( gmv_vdid, 1 );
         return;
      } /*endif*/
		wind_set( gmv_wdwid[wdw], WF_NAME, wdwname_gcv[wdw] );
      wind_open( gmv_wdwid[wdw], w[0], w[1], w[2], w[3] );
      gmh_erase( wdw );
		evnt_mesag( msg );  /* skip initial redraw message */
      wind_get( gmv_wdwid[wdw], WF_WORKXYWH, w, w+1, w+2, w+3 );
      gmv_xwrite[wdw] = w[0] + XWRITEOFF;
      gmv_ywrite[wdw] = w[1] + gmv_boxh + YWRITEOFF;
      gmv_init |= (1 << wdw);
   } /*endif*/

#	ifdef USE_EVENT_MANAGER
	ev_read_character(&ch,TRUE);
#	endif /* USE_EVENT_MANAGER */

   v_show_c( gmv_vdid, 1 );

} /* end of gm_init */



/*------------------------------------------------------------------------*/



void gm_exit( int wdw )

/* closes window
 *
 * parameters of routine
 * int        wdw;          input; window to be closed
 */
{
   /* executable code */

	if  (gmh_illwdw(wdw))  return;

   if  (gmh_wdwopen(wdw))  {
      v_hide_c( gmv_vdid );
      wind_close( gmv_wdwid[wdw] );
      wind_delete( gmv_wdwid[wdw] );
      gmv_wdwid[wdw] = -1;
      gmv_init &= ~(1 << wdw);
      v_show_c( gmv_vdid, 1 );
   } /*endif*/

} /* end of gm_exit */



/*------------------------------------------------------------------------*/



void gm_finish( void )

/* closes all graphic channels and terminates graphics
 *
 * no parameters
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	/* close all windows */
	for  (i=0;i<GBC_MAXWDW;i++)
		if  ((1<<i) & gmv_init)
			gm_exit( i );

	gmv_init = 0;

#	ifdef BC_KS_PRIVATE
	if  (vq_gdos())  vst_unload_fonts( gmv_vdid, 0 );
#	endif  /* BC_KS_PRIVATE */
   v_clsvwk( gmv_vdid );
#	ifdef USE_MENU_BAR
	if  (gmv_rsrcloaded)  {
		menu_bar( gmv_menutree, FALSE );
		rsrc_free();
	} /*endif*/
#	endif /* USE_MENU_BAR */
   appl_exit();
   gmv_wrkinit = FALSE;

} /* end of gm_finish */



/*------------------------------------------------------------------------*/



void gm_resizewdw( int wdw, GBC_COO xpos, GBC_COO ypos,
	GBC_COO width, GBC_COO height, STATUS *status )

/* resizes window
 *
 * parameters of routine
 * int        wdw;            input; wdw number
 * GBC_COO    xpos, ypos;     input; position of window
 * GBC_COO    width, height;  input; size of window
 * STATUS     *status;        output; return status
 */
{
   /* local variables */
   int      w[4];     /* dev coo's */

   /* executable code */

	if  (gmh_illwdw(wdw))  return;
   if  (!gmh_wdwopen(wdw))  return;

   w[2] = (int)(width*gmv_wpixelpercm);
   w[3] = (int)(height*gmv_hpixelpercm);
   w[0] = (int)(xpos*gmv_wpixelpercm);
   w[1] = gmv_screenh - (int)(ypos*gmv_hpixelpercm) - w[3];
   if  ((w[0] < 0) || ((w[0]+w[2]) > gmv_screenw) || (w[1] < 0) ||
      ((w[1]+w[3]) > gmv_screenh) || (w[2] <= 0) || (w[3] <= 0))  {
      *status = GME_ILLWDW;
      return;
   } /*endif*/
	v_hide_c( gmv_vdid );
   wind_set( gmv_wdwid[wdw], WF_CURRXYWH, w[0], w[1], w[2], w[3] );
	v_show_c( gmv_vdid, 1 );
	wind_get( gmv_wdwid[wdw], WF_WORKXYWH, w, w+1, w+2, w+3 );
	gmv_xwrite[wdw] = w[0] + XWRITEOFF;
	gmv_ywrite[wdw] = w[1] + gmv_boxh + YWRITEOFF;

} /* end of gm_resizewdw */



/*------------------------------------------------------------------------*/



void gm_popwdw( int wdw )

/* pops window on top
 *
 * parameters of routine
 * int        wdw;            input; window number
 */
{
	/* local variables */
	int      w;        /* window counter */

   /* executable code */

	if  (wdw == GBC_CYCLEWDW)  {

		wind_get( 0, WF_TOP, &wdw, 0, 0, 0 );
		wdw = gm_getwdw( wdw );
		if  (wdw == 0)  return;
		w = wdw + 1;
		FOREVER  {
			if  (w >= GBC_MAXWDW)  w = 0;
			if  (w == wdw)  return;
			if  (gmh_wdwopen(w))  {
				v_hide_c( gmv_vdid );
	   		wind_set( gmv_wdwid[w], WF_TOP, 0, 0, 0, 0 );
				v_show_c( gmv_vdid, 1 );
				return;
			} /*endif*/
			w++;
		} /*endfor*/

	} else {

		if  (wdw == 0)  return;
		if  (!gmh_wdwopen(wdw))  return;
		v_hide_c( gmv_vdid );
	   wind_set( gmv_wdwid[wdw], WF_TOP, 0, 0, 0, 0 );
		v_show_c( gmv_vdid, 1 );

	} /*endif*/

} /* end of gm_popwdw */



/*------------------------------------------------------------------------*/



void gm_setwdwname( int wdw, char name[] )

/* sets new window name
 *
 * parameters of routine
 * int       wdw;       input; window number
 * char      name[];    input; new name
 */
{
	/* executable code */

	if  (gmh_illwdw(wdw))  return;
	if  (!gmh_wdwopen(wdw))  return;
	strncpy( wdwname_gcv[wdw], name, WDWNAMLTH );
	wind_set( gmv_wdwid[wdw], WF_NAME, wdwname_gcv[wdw] );

} /* end of gm_setwdwname */



/*------------------------------------------------------------------------*/



void gm_setstyle( int wdw, int style, char item[], char value[],
	STATUS *status )

/* sets style attribute
 *
 * parameters of routine
 * int        wdw;       input; wdw number (dummy in the ATARI version)
 * int        style;     input; style block number
 * char       item[];    input; name of attribute
 * char       value[];   input; new value of attribute (text)
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	int      num;      /* integer parameter */
	float    r;        /* real parameter */
	int      w[4];     /* for wind_get call */

	/* executable code */

	if  (!gmh_wdwopen(wdw))  {
		;
	} /*endif*/

   if (style >= CSTYLES || style >= LSTYLES  ||  style < 0)  {
		*status = GME_ILSTYLE;
		return;
	} /*endif*/
	if  (strcmp(item,"CHARSIZE") == 0)  {
		if  (sscanf(value,"%f",&r) != 1)  {
			*status = GME_ILVALUE;
			return;
		} /*endif*/
		wind_get( gmv_wdwid[wdw], WF_WORKXYWH, w, w+1, w+2, w+3 );
		gmv_cstyle[style].cheight = Nint( r*(float)w[3] );
	} else if  (strcmp(item,"WRMODE") == 0)  {
		if  (strcmp(value,"REPLACE") == 0)  {
			gmv_cstyle[style].wrmode = MD_REPLACE;
			gmv_lstyle[style].wrmode = MD_REPLACE;
		} else if  (strcmp((char *)value,"TRANSPARENT") == 0)  {
			gmv_cstyle[style].wrmode = MD_TRANS;
			gmv_lstyle[style].wrmode = MD_TRANS;
		} else if  (strcmp(value,"XOR") == 0)  {
			gmv_cstyle[style].wrmode = MD_XOR;
			gmv_lstyle[style].wrmode = MD_XOR;
		} else if  (strcmp(value,"ERASE") == 0)  {
			gmv_cstyle[style].wrmode = MD_ERASE;
			gmv_lstyle[style].wrmode = MD_ERASE;
		} else {
			*status = GME_ILVALUE;
			return;
		} /*endif*/
	} else if  (strcmp(item,"LINESTYLE") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = GME_ILVALUE;
			return;
		} /*endif*/
	   gmv_lstyle[style].lntype = num;
	} else if  (strcmp(item,"COLOR") == 0)  {
		/* not implemented */
	} else if  (strcmp(item,"CHARROT") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = GME_ILVALUE;
			return;
		} /*endif*/
		gmv_cstyle[style].angle = num*10;
	} else if  (strcmp(item,"FONT") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = GME_ILVALUE;
			return;
		} /*endif*/
		gmv_cstyle[style].cfont = num;
	} else if  (strcmp(item,"LINEWIDTH") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = GME_ILVALUE;
			return;
		} /*endif*/
		gmv_lstyle[style].lnwidth = num;
	} else if  (strcmp(item,"LINEPATTERN") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = GME_ILVALUE;
			return;
		} /*endif*/
	   gmv_lstyle[style].lnpatt = num;
	} else if  (strcmp(item,"CHARSIZE_ATARI") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = GME_ILVALUE;
			return;
		} /*endif*/
		gmv_cstyle[style].cheight = num;
	} else if  (strcmp(item,"TEXTEFFECTS") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = GME_ILVALUE;
			return;
		} /*endif*/
		gmv_cstyle[style].ceffects = num;
	} else {
		*status = GME_UKITEM;
		return;
	} /*endif*/

   if  (style == gmv_currcstyle)  gmh_cstyle( style );
   if  (style == gmv_currlstyle)  gmh_lstyle( style );

} /* end of gm_setstyle */



/*------------------------------------------------------------------------*/



void gm_setcoo( int wdw, GBC_COO x, GBC_COO y, GBC_COO w, GBC_COO h,
	STATUS *status )

/* sets user coordinates in window.  If x,y,w,h are all equal to zero,
 * the last call is repeated.
 *
 * parameters of routine
 * int        wdw;        input; window
 * GBC_COO    x, y, w, h; input; user coordinates
 * STATUS     *status;    output; return status
 */
{
   /* local variables */
	static GBC_COO coosave[GBC_MAXWDW][4];   /* params of last call */
   int      wx, wy, ww, wh;  /* workspace coordinates of window */

   /* executable code */

	if  (gmh_illwdw(wdw))  return;

	if  ((wdw >= 0) && (x == 0.0) && (y == 0.0) && (w == 0.0) && (h == 0.0))  {
		x = coosave[wdw][0];
		y = coosave[wdw][1];
		w = coosave[wdw][2];
		h = coosave[wdw][3];
	} /*endif*/

   if  ((w <= 0.) || (h <= 0.))  {
      *status = GME_ILLCOO;
      return;
   } /*endif*/

   if  (gmh_wdwopen(wdw))  {
      wind_get( gmv_wdwid[wdw], WF_WORKXYWH, &wx, &wy, &ww, &wh );
      gmv_txfac[wdw] = (float)ww / w;
      gmv_tyfac[wdw] = (float)wh / h;
      gmv_txoff[wdw] = gmv_txfac[wdw]*x - (float)wx;
      gmv_tyoff[wdw] = gmv_tyfac[wdw]*(-y-h) - (float)wy;
		coosave[wdw][0] = x;
		coosave[wdw][1] = y;
		coosave[wdw][2] = w;
		coosave[wdw][3] = h;
   } /*endif*/

} /* end of gm_setcoo */



/*------------------------------------------------------------------------*/



float gm_aspectratio( int wdw )

/* returns ratio of width to height of window "wdw"
 *
 * parameters of routine
 * int        wdw;      input; window number
 *                      returns aspect ratio
 */
{
	/* local variables */
	int      wx, wy, ww, wh;   /* window coo's */

	/* executable code */

   if  (gmh_wdwopen(wdw))  {
      wind_get( gmv_wdwid[wdw], WF_WORKXYWH, &wx, &wy, &ww, &wh );
		return  (float)ww / (float)wh;
	} /*endif*/
	return 1.0;

} /* end of gm_aspectratio */



/*------------------------------------------------------------------------*/



void gm_moveto( int wdw, GBC_COO x, GBC_COO y )

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        wdw;      input; window
 * GBC_COO    x, y;     input; location to move to
 */
{
   /* executable code */

	if  (gmh_illwdw(wdw))  return;
   if  (gmh_wdwopen(wdw))  {
      gmv_xpos[wdw] = x;
      gmv_ypos[wdw] = y;
   } /*endif*/

} /* end of gm_moveto */



/*------------------------------------------------------------------------*/



void gm_drawto( int wdw, int style, GBC_COO x, GBC_COO y )

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        wdw;       input; window
 * int        style;     input; attribute block number
 * GBC_COO    x, y;      input; location to draw to
 */
{
   /* local variables */
   int      c[4];            /* clipping rectangle */
   int      l[4];            /* line coordinates */
   int      wg;              /* window get field */

   /* executable code */

   if  (style != gmv_currlstyle)  gmh_lstyle( style );

	if  (gmh_illwdw(wdw))  return;
   if  (gmh_wdwopen(wdw))  {
      gmh_transform( wdw, gmv_xpos[wdw], gmv_ypos[wdw], l, l+1 );
      gmh_transform( wdw, x, y, l+2, l+3 );
      wg = WF_FIRSTXYWH;
		v_hide_c( gmv_vdid );
      FOREVER  {
         wind_get( gmv_wdwid[wdw], wg, c, c+1, c+2, c+3 );
         if  (c[2] == 0)  break;
         c[2] += c[0]-1;
         c[3] += c[1]-1;
         vs_clip( gmv_vdid, CLIPON, c );
         v_pline( gmv_vdid, 2, l );
         wg = WF_NEXTXYWH;
      } /*endfor*/
		v_show_c( gmv_vdid, 1 );
      gmv_xpos[wdw] = x;
      gmv_ypos[wdw] = y;
   } /*endif*/

} /* end of gm_drawto */



/*------------------------------------------------------------------------*/



void gm_setpixel( int wdw, int style, GBC_COO x, GBC_COO y )

/* sets pixel at position (x,y) in user coordinates
 *
 * parameters of routine
 * int        wdw;       input; window
 * int        style;     input; style block number
 * GBC_COO    x, y;      input; location of pixel
 */
{
   /* local variables */
   int      c[4];            /* clipping rectangle */
   int      p[2];            /* pixel coordinates */
   int      wg;              /* window get field */

   /* executable code */

   if  (style != gmv_currlstyle)  gmh_lstyle( style );

	if  (gmh_illwdw(wdw))  return;
   if  (gmh_wdwopen(wdw))  {
      gmh_transform( wdw, x, y, p, p+1 );
      wg = WF_FIRSTXYWH;
		v_hide_c( gmv_vdid );
      FOREVER  {
         wind_get( gmv_wdwid[wdw], wg, c, c+1, c+2, c+3 );
         if  (c[2] == 0)  break;
         c[2] += c[0]-1;
         c[3] += c[1]-1;
         vs_clip( gmv_vdid, CLIPON, c );
         v_pmarker( gmv_vdid, 1, p );
         wg = WF_NEXTXYWH;
      } /*endfor*/
		v_show_c( gmv_vdid, 1 );
   } /*endif*/

} /* end of gm_setpixel */



/*------------------------------------------------------------------------*/



void gm_arrayplot( int wdw, int style, long cnt, int red, GBC_COO xoff,
	GBC_COO xinc, GBC_COO yoff, GBC_COO yarr[], GBC_COO yzoom, STATUS *status )

/* plots an array of sample data
 *
 * parameters of routine
 * int        wdw;       input; window
 * int        style;     input; line style ID
 * long       cnt;       input; length of data array
 * int        red;       input; reduction factor for plotting
 * GBC_COO    xoff;      input; x-position of first sample
 * GBC_COO    xinc;      input; x increment
 * GBC_COO    yoff;      input; y-position of first sample
 * GBC_COO    yarr[];    input; sample data
 * GBC_COO    yzoom;     input; zoom factor of sample data
 * STATUS     *status;   output; return status
 */
{
   /* local constants */
#	ifdef __68881__
#	define MAXPLOT 1024    /* must be less than SYC_MAXINT; */
#	else
#	define MAXPLOT 128    /* must be less than SYC_MAXINT; */
#	endif

   /* local variables */
   int      *pxy;     /* pointer to xy-array */
   long     i, j;     /* counters */
   int      c[4];     /* clipping rectangle */
   int      wg;       /* wind_get field */

   /* executable code */

   if  (style != gmv_currlstyle)  gmh_lstyle( style );

   if  (cnt == 0L)  return;
	if  (gmh_illwdw(wdw))  return;
   if  (!gmh_wdwopen(wdw))  return;

   /* allocate memory */
   pxy = (int *)sy_allocmem( (long)MAXPLOT, (int)sizeof(int)*2, status );
   if  (*status != GME_NOERROR)  return;
   v_hide_c( gmv_vdid );

   if  (red == 1)  {
      i = 0;
      while  (i < cnt)  {
         for  (j=0;j<MAXPLOT;j++)  {
            gmh_transform( wdw, xoff+xinc*(GBC_COO)i,
               yoff + yzoom * (*yarr++), pxy+2*j, pxy+2*j+1 );
            if  (++i == cnt)  {
               j++;
               break;
            } /*endif*/
         } /*endfor*/
         if  (i != cnt)  {i--; yarr--;}  /* connect lines */
         wg = WF_FIRSTXYWH;
         FOREVER  {
            wind_get( gmv_wdwid[wdw], wg, c, c+1, c+2, c+3 );
            if  (c[2] == 0)  break;
            c[2] += c[0]-1;
            c[3] += c[1]-1;
            vs_clip( gmv_vdid, CLIPON, c );
            v_pline( gmv_vdid, (int)j, pxy );
            wg = WF_NEXTXYWH;
         } /*endfor*/
      } /*endwhile*/
   } else {
      i = 0;
      while  (i < cnt)  {
         for  (j=0;j<MAXPLOT;j++)  {
            gmh_transform( wdw, xoff+xinc*(GBC_COO)i,
               yoff + yzoom * (*yarr), pxy+2*j, pxy+2*j+1 );
            yarr += red;
            i += red;
            if  (i >= cnt)  {
               j++;
               break;
            } /*endif*/
         } /*endfor*/
         if  ((i+(long)red) < cnt)  {
            i -= red; yarr -= red;  /* connect lines */
         } /*endif*/
         wg = WF_FIRSTXYWH;
         FOREVER  {
            wind_get( gmv_wdwid[wdw], wg, c, c+1, c+2, c+3 );
            if  (c[2] == 0)  break;
            c[2] += c[0]-1;
            c[3] += c[1]-1;
            vs_clip( gmv_vdid, CLIPON, c );
            v_pline( gmv_vdid, (int)j, pxy );
            wg = WF_NEXTXYWH;
         } /*endfor*/
      } /*endwhile*/
   } /*endif*/

   v_show_c( gmv_vdid, 1 );
   sy_deallocmem( pxy );

} /* end of gm_arrayplot */



/*------------------------------------------------------------------------*/



void gm_erase( int wdw )

/* clears window
 *
 * parameters of routine
 * int        wdw;       input; window
 */
{
   /* local variables */
   int      c[4];     /* clipping rectangle */
   int      wg;       /* window get field */

   /* executable code */

	if  (gmh_illwdw(wdw))  return;
   if  (gmh_wdwopen(wdw))  {
      v_hide_c( gmv_vdid );
      vs_clip( gmv_vdid, CLIPOFF, c );
      wg = WF_FIRSTXYWH;
      FOREVER  {
         wind_get( gmv_wdwid[wdw], wg, c, c+1, c+2, c+3 );
         if  (c[2] == 0)  break;
         c[2] += c[0]-1;
         c[3] += c[1]-1;
         v_bar( gmv_vdid, c );
         wg = WF_NEXTXYWH;
      } /*endfor*/
      v_show_c( gmv_vdid, 1 );
   } /*endif*/

} /* end of gm_erase */



/*------------------------------------------------------------------------*/



void gm_text( int wdw, int style, GBC_COO x, GBC_COO y, char text[] )

/* writes text to window
 *
 * parameters of routine
 * int        wdw;        input; window
 * int        style;      input; character style block number
 * GBC_COO    x, y;       input; text position
 * char       text[];     input; output text
 */
{
   /* local variables */
   int      c[4];     /* clipping rectangle */
   int      wg;       /* window get field */
   int      tx, ty;   /* device text position */

   /* executable code */

	if  (*text == '\0')  return;
	/* if  (GCF_NOECHO & wdw)  return; */
	if  (style != gmv_currcstyle)  gmh_cstyle( style );

	if  (gmh_illwdw(wdw))  return;
   if  (gmh_wdwopen(wdw))  {
      v_hide_c( gmv_vdid );
      gmh_transform( wdw, x, y, &tx, &ty );
      wg = WF_FIRSTXYWH;
      FOREVER  {
         wind_get( gmv_wdwid[wdw], wg, c, c+1, c+2, c+3 );
         if  (c[2] == 0)  break;
         c[2] += c[0]-1;
         c[3] += c[1]-1;
         vs_clip( gmv_vdid, CLIPON, c );
         v_gtext( gmv_vdid, tx, ty, text );
         wg = WF_NEXTXYWH;
      } /*endfor*/
      v_show_c( gmv_vdid, 1 );
   } /*endif*/

} /* end of gm_text */



/*------------------------------------------------------------------------*/



void gm_write( int wdw, char text[] )

/* writes text to window at current write position
 *
 * parameters of routine
 * int        wdw;          input; window
 * char       text[];       input; output text
 */
{
   /* local variables */
   int      c[4];       /* clipping rectangle */
   int      wg;         /* window get field */
   char     ltxt[200];  /* local text */
   int      txtlth;     /* text length */

   /* executable code */

	if  (*text == '\0')  return;
	/* if  (GCF_NOECHO & wdw)  return; */
   if  (gmv_currcstyle != DSTYLE)  gmh_cstyle( DSTYLE );

	if  (gmh_illwdw(wdw))  return;
   if  (gmh_wdwopen(wdw))  {

      v_hide_c( gmv_vdid );
      /* check <CR><LF> at beginning */
      if  (text[0] == '\n')  {
         gmh_newline( wdw );
         text++;
      } /*endif*/
      /* check <CR>(<LF>) at end */
      txtlth = (int)strlen( text );
      if  (txtlth == 0)  return;
      if  (text[txtlth-1] == '\n')  {
         strncpy( ltxt, text, --txtlth );
         ltxt[txtlth] = '\0';
         txtlth = -1;  /* scroll at end */
      } else if  (text[txtlth-1] == '\r')  {
         strncpy( ltxt, text, --txtlth );
         ltxt[txtlth] = '\0';
         txtlth = -2;  /* <CR> at end */
      } else {
         strcpy( ltxt, text );
      } /*endif*/

      wg = WF_FIRSTXYWH;
      FOREVER  {
         wind_get( gmv_wdwid[wdw], wg, c, c+1, c+2, c+3 );
         if  (c[2] == 0)  break;
         c[2] += c[0]-1;
         c[3] += c[1]-1;
         vs_clip( gmv_vdid, CLIPON, c );
         v_gtext( gmv_vdid, gmv_xwrite[wdw], gmv_ywrite[wdw], ltxt );
         wg = WF_NEXTXYWH;
      } /*endfor*/

      if  (txtlth == -1)  {
         gmh_newline( wdw );
		} else if  (txtlth == -2)  {
			gmh_carriage_return( wdw );
      } else {
         gmv_xwrite[wdw] += txtlth*gmv_boxw;
			/* store line: */
			if  ((txtlth+strlen(gmv_lastline[wdw])) <= BC_LINELTH)
				strcat( gmv_lastline[wdw], ltxt );
      } /*endif*/
      v_show_c( gmv_vdid, 1 );

   } /*endif*/

} /* end of gm_write */



/*------------------------------------------------------------------------*/



void gm_wrtch( int wdw, char ch )

/* writes a single character to window
 *
 * parameters of routine
 * int        wdw;         input; window
 * char       ch;          input; char to be written
 */
{
   /* local variables */
   int      wg;       /* window get field */
   int      c[4];     /* clipping rectangle */
   char     ct[2];    /* output text */

   /* executable code */

	/* if  (GCF_NOECHO & wdw)  return; */
   if  (gmv_currcstyle != DSTYLE)  gmh_cstyle( DSTYLE );

	if  (gmh_illwdw(wdw))  return;
   if  (gmh_wdwopen(wdw))  {
      v_hide_c( gmv_vdid );
      ct[0] = ch;
      ct[1] = '\0';
      if  (ch == DEL)  {
         gm_wrtch( wdw, BS );
         gm_wrtch( wdw, ' ' );
         gm_wrtch( wdw, BS );
      } else if  (ch == BS)  {
         gmv_xwrite[wdw] -= gmv_boxw;
			if  (strlen(gmv_lastline[wdw]) < BC_LINELTH)
				strcat( gmv_lastline[wdw], ct );
      } else if  (ch == '\n')  {
         gmh_newline( wdw );
      } else if  (ch == CR)  {
			gmh_carriage_return( wdw );
      } else {
         wg = WF_FIRSTXYWH;
         FOREVER  {
            wind_get( gmv_wdwid[wdw], wg, c, c+1, c+2, c+3 );
            if  (c[2] == 0)  break;
            c[2] += c[0]-1;
            c[3] += c[1]-1;
            vs_clip( gmv_vdid, CLIPON, c );
            v_gtext( gmv_vdid, gmv_xwrite[wdw], gmv_ywrite[wdw], ct );
            wg = WF_NEXTXYWH;
         } /*endfor*/
         gmv_xwrite[wdw] += gmv_boxw;
			if  (strlen(gmv_lastline[wdw]) < BC_LINELTH)
				strcat( gmv_lastline[wdw], ct );
      } /*endif*/
      v_show_c( gmv_vdid, 1 );
   } /*endif*/

} /* end of gm_wrtch */



/*------------------------------------------------------------------------*/



int gm_txtwidth( int wdw )

/* returns width of window in characters
 *
 * parameters of routine
 * int		  wdw;		 input; window number
 */
{
	/* local variables */
	int		x, y, w, h;     /* window coordinates */

   /* executable code */
	
	if  (gmh_illwdw(wdw))  return 0;
   if  (gmh_wdwopen(wdw))  {
		wind_get( gmv_wdwid[wdw], WF_WORKXYWH, &x, &y, &w, &h );
		return (w/gmv_boxw);
	} /*endif*/

	return 0;

} /* end of gm_txtwidth */



/*------------------------------------------------------------------------*/



int gm_txtheight( int wdw )

/* returns height of window in characters
 *
 * parameters of routine
 * int		  wdw;		 input; window number
 */
{
	/* local variables */
	int		x, y, w, h;     /* window coordinates */

   /* executable code */
	
	if  (gmh_illwdw(wdw))  return 1;
   if  (gmh_wdwopen(wdw))  {
		wind_get( gmv_wdwid[wdw], WF_WORKXYWH, &x, &y, &w, &h );
		return (h/gmv_boxh);
	} /*endif*/

	return 1;

} /* end of gm_txtheight */



/*------------------------------------------------------------------------*/



void gm_read( int wdw, int maxlth, char text[] )

/* reads text from terminal
 *
 * parameters of routine
 * int      wdw;          input; window number
 * int      maxlth;       input; maximum length of text
 * char     text[];       output; text read
 */
{
   /* local variables */
   int      i;        /* counter */
	BOOLEAN  echo;     /* echo characters */

   /* executable code */

   if  (gmv_currcstyle != DSTYLE)  gmh_cstyle( DSTYLE );

	echo = TRUE; /* echo = !(GCF_NOECHO & wdw); */
	if  (gmh_illwdw(wdw))  return;
   if  (!gmh_wdwopen(wdw))  return;

   v_hide_c( gmv_vdid );
   i = 0;
   while  (i < maxlth-1)  {
      if  (!gmh_inbuf())  gmh_txtcrsr( wdw, TRUE );
      gm_read_character( text+i );
      gmh_txtcrsr( wdw, FALSE );
      if  ((text[i] == CR) || (text[i] == LF))  {
         text[i] = '\0';
         break;
      } else if  ((text[i] == BS) || (text[i] == DEL))  {
         if  (i != 0)  {
            if  (echo)  gm_wrtch( wdw, DEL );
            text[i--] = '\0';
         } /*endif*/
      } else if  (text[i] == '\t')  {
         text[i] = ' ';
         if  (echo)  gm_wrtch( wdw, text[i++] );
      } else if  (text[i] == ESC)  {
         for  (;i>0;i--)  {
            if  (echo)  gm_wrtch( wdw, DEL );
            text[i] = '\0';
         } /*endfor*/
         text[0] = '\0';
      } else if  (text[i] == 'œ')  {
         gmh_crsr();
      } else if  (text[i] >= ' ')  {
         if  (echo)  gm_wrtch( wdw, text[i++] );
      } /*endif*/
   } /*endwhile*/

   if  (echo)  gmh_newline( wdw );

   v_show_c( gmv_vdid, 1 );

} /* end of gm_read */



/*------------------------------------------------------------------------*/



float gm_chheight( int wdw )

/* returns current character height in window "wdw"
 *
 * parameter of routine
 * int       wdw;      input; window number
 */
{
	/* executable code */

	if  (gmh_illwdw(wdw))  return 0.0;
   if  (!gmh_wdwopen(wdw))  return 0.0;
	return ((float)gmv_boxh/gmv_tyfac[wdw]);

} /* end of gm_chheight */



/*------------------------------------------------------------------------*/



void gm_getloc( int wdw, GBC_COO *x, GBC_COO *y, char *ch )

/* requests mouse position in window
 *
 * parameters of routine
 * int        wdw;        input; window
 * GBC_COO    *x, *y;     output; location selected
 * char       *ch;        output; key pressed
 */
{
   /* local variables */
   int      mx, my;     /* device coordinates */
	int		oldx, oldy; /* previous coo's */
   int      w[4];       /* window workspace */
	int		cshown;     /* cross displayed */
   /* int      state; K.S. 21.1.92 */
	int      events;
	int      dmy;
	int      key;

   /* executable code */

	if  (gmh_illwdw(wdw))  return;
   if  (gmh_wdwopen(wdw))  {
      wind_get( gmv_wdwid[wdw], WF_WORKXYWH, w, w+1, w+2, w+3 );
      w[2] += w[0]-1;
      w[3] += w[1]-1;
      graf_mouse( THIN_CROSS, 0L );
	   vswr_mode( gmv_vdid, GBC_XOR );
      do  {
			cshown = FALSE; oldx = oldy = 10000;
         /* while  (gmh_inbuf() == 0)  { K.S. 12.1.92 */
			FOREVER  {
				events = evnt_multi( MU_KEYBD|MU_TIMER,
					1,1,1, 0,0,0,0,0, 0,0,0,0,0, NULL, 0,0, &mx, &my,
					&dmy, &dmy, &key, &dmy );
					*ch = (char)key;
				if  (MU_KEYBD & events)  break;
	         /* vq_mouse( gmv_vdid, &state, &mx, &my ); K.S. 12.1.92 */
			   if  ((mx>w[0]) && (mx<w[2]) && (my>w[1]) && (my<w[3]))  {
					if  ((mx != oldx) || (my != oldy))  {
						v_hide_c( gmv_vdid );
						if  (cshown)  {
							gmh_line( wdw, w[0], oldy, w[2], oldy );
							gmh_line( wdw, oldx, w[1], oldx, w[3] );
						} /*endif*/
						gmh_line( wdw, w[0], my, w[2], my );
						gmh_line( wdw, mx, w[1], mx, w[3] );
						v_show_c( gmv_vdid, 1 );
						cshown = TRUE;
						oldx = mx; oldy = my;
					} /*endif*/
				} /*endif*/
			} /*endfor*/
         /* gm_read_character( ch ); K.S. 12.1.92 */
         /* vq_mouse( gmv_vdid, &state, &mx, &my ); K.S. 12.1.92 */
			if  (cshown)  {
				v_hide_c( gmv_vdid );
				gmh_line( wdw, w[0], oldy, w[2], oldy );
				gmh_line( wdw, oldx, w[1], oldx, w[3] );
				v_show_c( gmv_vdid, 1 );
				cshown = FALSE;
			} /*endif*/
      } while ((mx < w[0]) || (mx > w[2]) || (my < w[1]) || (my > w[3]));
	   vswr_mode( gmv_vdid, gmv_lstyle[gmv_currlstyle].wrmode );
      graf_mouse( HOURGLASS, 0L );
      gmh_backtrans( wdw, mx, my, x, y );
   } /*endif*/

} /* end of gm_getloc */



/*------------------------------------------------------------------------*/

#ifdef XXX

void gm_watch( int wdw, GBC_COO *x, GBC_COO *y, char *ch )

/* returns mouse position and key if pressed
 *
 * parameters of routine
 * int      wdw;           input; channel map
 * GBC_COO  *x, *y;        output; location selected
 * char     *ch;           output; key pressed
 */
{
   /* local variables */
   int      mx, my;     /* device coordinates */
   int      w[4];       /* window workspace */
   int      state;

   /* executable code */

	if  (gmh_illwdw(wdw))  return;
   if  (gmh_wdwopen(wdw))  {
      wind_get( gmv_wdwid[wdw], WF_WORKXYWH, w, w+1, w+2, w+3 );
      w[2] += w[0]-1;
      w[3] += w[1]-1;
      /* graf_mouse( THIN_CROSS, 0L ); */
	   /* vswr_mode( gmv_vdid, GBC_XOR ); */
      do  {
			vq_mouse( gmv_vdid, &state, &mx, &my );
         ev_read_character( ch, TRUE );
		} while ((mx < w[0]) || (mx > w[2]) || (my < w[1]) || (my > w[3]));
	   /* vswr_mode( gmv_vdid, gmv_lstyle[gmv_currlstyle].wrmode ); */
      /* graf_mouse( HOURGLASS, 0L ); */
      gmh_backtrans( wdw, mx, my, x, y );
   } /*endif*/

} /* end of gm_watch */

#endif  /* XXX */

/*------------------------------------------------------------------------*/



void gm_set_outputdir( char dir[], STATUS *status )

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name of scratch path
 * STATUS    *status;   output; return status
 */
{
	/* executable code */

	if  (strlen(dir) > BC_FILELTH)  {
		*status = GME_STROVFL;
		return;
	} /*endif*/
	strcpy( gmv_scratchdir, dir );

} /* end of gm_set_outputdir */



/*------------------------------------------------------------------------*/



void gm_prepare( int wdw, STATUS *status )

/* prepares window for redrawing
 *
 * int      wdw;      input; window
 * STATUS   *status;  output; return status
 */
{
	/* local variables */
   int      ipar[4];             /* int parameter */
	char     tpar[BC_LINELTH+1];  /* string */
	char     *c;                  /* moving pointer */

	/* executable code */

	if  (gmh_illwdw(wdw))  return;
	if  (!gmh_wdwopen(wdw))  return;

	gmh_erase( wdw );

	/* set write position */
   if  (gmv_currcstyle != DSTYLE)  gmh_cstyle( DSTYLE );
	wind_get( gmv_wdwid[wdw], WF_WORKXYWH, ipar, ipar+1, ipar+2, ipar+3 );
	gmv_xwrite[wdw] = ipar[0] + XWRITEOFF;
	gmv_ywrite[wdw] = ipar[1] + gmv_boxh + YWRITEOFF;

	/* write last line written in text mode */
	if  (gmv_lastline[wdw][0] != '\0')  {
		strcpy( tpar, gmv_lastline[wdw] );
		gmv_lastline[wdw][0] = '\0';
		c = tpar;
		while  (*c != '\0')
			gm_wrtch( wdw, *c++ );
		gmh_txtcrsr( wdw, TRUE );
	} /*endif*/

	if  (Severe(status))  return;  /* only to use status */

} /* end of gm_prepare */



/*------------------------------------------------------------------------*/



void gm_cleanup( int wdw, char outf[], STATUS *status )

/* cleanup routine after redraw
 *
 * parameters of routine
 * int        wdw;       input; window
 * char       outf[];    output; not used for this channel
 * STATUS     *status;   output; return status
 */
{
	/* executable code */

	if  (wdw)  {
		;          /* just to access wdw once */
	} /*endif*/

	if  (outf != NULL)
		*outf = '\0';

	if  (Severe(status))  return;  /* just to access status once */

} /* end of gm_cleanup */



/*------------------------------------------------------------------------*/
/*                   routines for event manager                           */
/*------------------------------------------------------------------------*/



int gm_getwdw( int gem_wdw )

/* returns GC window number from GEM window number.
 * This routine is used only by the event manager
 *
 * parameter of routine
 * int       gem_wdw;    input; GEM window handle (from message buffer)
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	for  (i=0;i<GBC_MAXWDW;i++)
		if  (gmv_wdwid[i] == gem_wdw)
			return i;

	return 0;

} /* end of gm_getwdw */



/*------------------------------------------------------------------------*/



void gm_pixinc_write( int wdw, int x_inc, int y_inc )

/* changes write position of window "wdw"
 *
 * parameters of routine
 * int        wdw;          input; window number
 * int        x_inc, y_inc; input; increment of coordinates
 */
{
	/* executable code */

	if  (gmh_illwdw(wdw))  return;
	gmv_xwrite[wdw] += x_inc;
	gmv_ywrite[wdw] += y_inc;

} /* end of gm_pixinc_write */



/*------------------------------------------------------------------------*/



void gm_redraw( int wdw, STATUS *status )

/* redraws window
 *
 * parameters of routine
 * int        wdw;     input; window
 * STATUS     *status; output; return status
 */
{
	/* local variables */
	PAINTBOX pb;       /* paintbox routines */

	/* executable code */

	pb.wpb.pbtype = GBC_WPAINT;
	pb.wpb.prepare = gm_prepare;
	pb.wpb.moveto = gm_moveto;
	pb.wpb.drawto = gm_drawto;
	pb.wpb.arrayplot = gm_arrayplot;
	pb.wpb.text = gm_text;
	pb.wpb.setstyle = gm_setstyle;
	pb.wpb.setcoo = gm_setcoo;
	pb.wpb.cleanup = gm_cleanup;
	(*gbv_playback)( wdw, wdw, &pb, NULL, status );
	/* gb_playback( wdw, wdw, &pb, NULL, status ); */

} /* end of gm_redraw */



/*------------------------------------------------------------------------*/



int gm_vdid( void )  { return gmv_vdid; }

BOOLEAN gm_rsrcloaded( void )  { return gmv_rsrcloaded; }



/*------------------------------------------------------------------------*/
/*                           supporting routines                          */
/*------------------------------------------------------------------------*/



static void gmh_transform( int wdw, GBC_COO ux, GBC_COO uy, int *dx, int *dy )

/* transforms user coordinates (ux,uy) to device coordinates (dx,dy)
 *
 * parameters of routine
 * int      wdw;          input; window number
 * GBC_COO  ux, uy;       input; user coordinates
 * int      *dx, *dy;     output; device coordinates
 */
{
   /* executable code */

   *dx = (int)(ux*gmv_txfac[wdw] - gmv_txoff[wdw]);
   *dy = (int)(-uy*gmv_tyfac[wdw] - gmv_tyoff[wdw]);

} /* end of gmh_transform */



/*------------------------------------------------------------------------*/



static void gmh_backtrans( int wdw, int dx, int dy, GBC_COO *ux, GBC_COO *uy )

/* transforms device coordinates (dx,dy) to user coordinates (ux,uy)
 *
 * parameters of routine
 * int      wdw;           input; window number
 * int      dx, dy;        input; device coordinates
 * GBC_COO  *ux, *uy;      output; user coordinates
 */
{
   /* executable code */

   *ux = ((float)dx + gmv_txoff[wdw]) / gmv_txfac[wdw];
   *uy = -((float)dy + gmv_tyoff[wdw]) / gmv_tyfac[wdw];

} /* end of gmh_backtrans */



/*------------------------------------------------------------------------*/



static void gmh_newline( int wdw )

/* starts a new output line in window
 *
 * parameters of routine
 * int      wdw;          input; window number
 */
{
   /* local variables */
   int      w[4];     /* window workspace */

   /* executable code */

   wind_get( gmv_wdwid[wdw], WF_WORKXYWH, w, w+1, w+2, w+3 );
   gmv_xwrite[wdw] = w[0] + XWRITEOFF;
   if  ((gmv_ywrite[wdw]+gmv_boxh+YWRITEOFF) > (w[1]+w[3]-1))  {
      gmh_scroll( wdw );
   } else {
      gmv_ywrite[wdw] += gmv_boxh;
   } /*endif*/
	gmv_lastline[wdw][0] = '\0';

} /* end of gmh_newline */



/*------------------------------------------------------------------------*/



static void gmh_carriage_return( int wdw )

/* restarts a output line in wdw
 *
 * parameters of routine
 * int      wdw;            input; window number
 */
{
   /* local variables */
   int      w[4];     /* window workspace */

   /* executable code */

   wind_get( gmv_wdwid[wdw], WF_WORKXYWH, w, w+1, w+2, w+3 );
   gmv_xwrite[wdw] = w[0] + XWRITEOFF;
	gmv_lastline[wdw][0] = '\0';

} /* end of gmh_carriage_return */



/*------------------------------------------------------------------------*/



static void gmh_scroll( int wdw )

/* scrolls down window one line
 *
 * parameters of routine
 * int        wdw;       input; window number
 */
{
   /* local variables */
   static MFDB  src, dst; /* memory form definition blocks */
	static BOOLEAN mfdb_init=FALSE;  /* mfdb's already initialised */
   int          c[4];     /* clipping rectangle */
   int          w[8];     /* window coo */
   int          b[4];     /* clear rectangle */
   int          wg;       /* window get field */

   /* executable code */

	if  (!mfdb_init)  {
		src.fd_addr = NULL;
		src.fd_w = gmv_screenw;
		src.fd_h = gmv_screenh;
		src.fd_wdwidth = gmv_screenw/WORDSIZE;
		src.fd_stand = 0;
		src.fd_nplanes = 1;
		dst.fd_addr = NULL;
		dst.fd_w = gmv_screenw;
		dst.fd_h = gmv_screenh;
		dst.fd_wdwidth = gmv_screenw/WORDSIZE;
		dst.fd_stand = 0;
		dst.fd_nplanes = 1;
		mfdb_init = TRUE;
	} /*endif*/

   if  (gmh_illwdw(wdw))  return;

   wind_get( gmv_wdwid[wdw], WF_WORKXYWH, w, w+1, w+2, w+3 );
   w[1] -= 1;       /* inserted */
   w[2] += w[0]-1;
   w[3] += w[1];    /* w[1]-1; */
   w[4] = w[0];
   w[5] = w[1];
   w[6] = w[2];
   w[7] = w[3]-gmv_boxh;
   w[1] += gmv_boxh;
   b[0] = w[0];
   b[1] = w[3]-gmv_boxh+1;
   b[2] = w[2];
   b[3] = w[3];
   wg = WF_FIRSTXYWH;
   FOREVER  {
      wind_get( gmv_wdwid[wdw], wg, c, c+1, c+2, c+3 );
      if  (c[2] == 0)  break;
      c[2] += c[0]-1;  /* c[0]-1; */
      c[3] += c[1]-1;  /* c[1]-1; */
      vs_clip( gmv_vdid, CLIPON, c );
      vro_cpyfm( gmv_vdid, 3, w, &src, &dst );
      v_bar( gmv_vdid, b );
      wg = WF_NEXTXYWH;
   } /*endfor*/

} /* end of gmh_scroll */



/*------------------------------------------------------------------------*/



static void gmh_line( int wdw, int x0, int y0, int x1, int y1 )

/* plots line in window "wdw"
 *
 * parameters of routine
 * int   wdw;          input; window number
 * int   x0, y0;       input; start position
 * int   x1, y1;       input; end position
 */
{
	/* local variables */
   int      c[4];     /* clipping rectangle */
   int      l[4];     /* line coo's */
   int      wg;       /* window get field */

	/* executable code */

   wg = WF_FIRSTXYWH;
   FOREVER  {
      wind_get( gmv_wdwid[wdw], wg, c, c+1, c+2, c+3 );
      if  (c[2] == 0)  break;
      c[2] += c[0]-1;
      c[3] += c[1]-1;
      vs_clip( gmv_vdid, CLIPON, c );
		l[0] = x0; l[1] = y0; l[2] = x1; l[3] = y1;
		v_pline( gmv_vdid, 2, l );
      wg = WF_NEXTXYWH;
   } /*endfor*/

} /* end of gmh_line */



/*------------------------------------------------------------------------*/



static void gmh_erase( int wdw )

/* clears window
 *
 * parameters of routine
 * int      wdw;           input; window number
 */
{
   /* local variables */
   int      c[4];     /* clipping rectangle */
   int      wg;       /* window get field */

   /* executable code */

   v_hide_c( gmv_vdid );
   vs_clip( gmv_vdid, CLIPOFF, c );
   wg = WF_FIRSTXYWH;
   FOREVER  {
      wind_get( gmv_wdwid[wdw], wg, c, c+1, c+2, c+3 );
      if  (c[2] == 0)  break;
      c[2] += c[0]-1;
      c[3] += c[1]-1;
      v_bar( gmv_vdid, c );
      wg = WF_NEXTXYWH;
   } /*endfor*/
   v_show_c( gmv_vdid, 1 );

} /* end of gmh_erase */



/*------------------------------------------------------------------------*/

#ifdef XXX

static void gmh_tstwdw( wdw )

/* shows rectangle list of window */

/* parameters of routine */
int      wdw;            /* input; window number */

{
   /* local variables */
   int          c[4];     /* clipping rectangle */
   int          d[12];
   int          wg;       /* window get field */

   /* executable code */

   if  (wdw < 0)  return;

   vs_clip( gmv_vdid, CLIPOFF, c );
   vsl_type( gmv_vdid, 3 );
   wg = WF_FIRSTXYWH;
   FOREVER  {
      wind_get( gmv_wdwid[wdw], wg, c, c+1, c+2, c+3 );
      if  (c[2] == 0)  break;
      c[2] += c[0]-1;
      c[3] += c[1]-1;
      d[0] = c[0]; d[1] = c[1];
      d[2] = c[2]; d[3] = c[1];
      d[4] = c[2]; d[5] = c[3];
      d[6] = c[0]; d[7] = c[3];
      d[8] = c[0]; d[9] = c[1];
      d[10] = c[2]; d[11] = c[3];
      v_pline( gmv_vdid, 5, d );
      wg = WF_NEXTXYWH;
   } /*endfor*/
   vsl_type( gmv_vdid, 1 );

} /* end of gmh_tstwdw */

#endif  /* XXX */

/*------------------------------------------------------------------------*/



static void gmh_txtcrsr( int wdw, int state )

/* displays or hides a text cursor at the current position
 *
 * parameters of routine
 * int      wdw;           window number
 * int      state;         input; TRUE=show cursor, FALSE=hide cursor
 */
{
   /* local variables */
   int      wg;       /* window get field */
   int      c[4];     /* clipping rectangle */
   char     ct[2];    /* output text */

   /* executable code */

   if  ((!gmv_tcrsr) && !state)  return;
   ct[0] = (state) ? gmv_crsrch : ' ';
   ct[1] = '\0';

   if  (gmv_currcstyle != DSTYLE)  gmh_cstyle( DSTYLE );

   if  (gmh_wdwopen(wdw))  {
      wg = WF_FIRSTXYWH;
      FOREVER  {
         wind_get( gmv_wdwid[wdw], wg, c, c+1, c+2, c+3 );
         if  (c[2] == 0)  break;
         c[2] += c[0]-1;
         c[3] += c[1]-1;
         vs_clip( gmv_vdid, CLIPON, c );
         v_gtext( gmv_vdid, gmv_xwrite[wdw], gmv_ywrite[wdw], ct );
         wg = WF_NEXTXYWH;
      } /*endfor*/
   } /*endif*/

   gmv_tcrsr = state;

} /* end of gmh_txtcrsr */



/*------------------------------------------------------------------------*/



static void gmh_lstyle( unsigned style )

/* sets the currently used line style block
 *
 * parameters of routine
 * unsigned int  style;      input; line style block number
 */
{
   /* executable code */

   if  (style >= LSTYLES)  return;
   vswr_mode( gmv_vdid, gmv_lstyle[style].wrmode );
   vsl_type( gmv_vdid, gmv_lstyle[style].lntype );
   vsl_udsty( gmv_vdid, gmv_lstyle[style].lnpatt );
   vsl_width( gmv_vdid, gmv_lstyle[style].lnwidth );
   vsl_color( gmv_vdid, gmv_lstyle[style].lncolor );
   gmv_currlstyle = style;

} /* end of gmh_lstyle */



/*------------------------------------------------------------------------*/



static void gmh_cstyle( unsigned style )

/* sets the currently used char style block
 *
 * parameters of routine
 * unsigned   style;     input; line style block number
 */
{
   /* local variables */
   int      d1, d2;

   /* executable code */

   if  (style >= CSTYLES)  return;
   vswr_mode( gmv_vdid, gmv_cstyle[style].wrmode );
   vst_height( gmv_vdid, gmv_cstyle[style].cheight,
      &d1, &d2, &gmv_boxw, &gmv_boxh );
   vst_rotation( gmv_vdid, gmv_cstyle[style].angle );
   vst_font( gmv_vdid, gmv_cstyle[style].cfont );
   vst_effects( gmv_vdid, gmv_cstyle[style].ceffects );
   vst_color( gmv_vdid, gmv_cstyle[style].ccolor );
   gmv_currcstyle = style;

} /* end of gmh_cstyle */



/*------------------------------------------------------------------------*/



static void gmh_rdch( char *ch )

/* reads single char without echo
 *
 * parameters of routine
 * char       *ch;         output; char read
 */
{
   /* local variables */
   /* long     l; */

   /* executable code */

   /*l = Cnecin();*/  /* 1. version */
   /*l = Bconin(2);*/ /* 2. version */
   /* *ch = (char)l; */

	*ch = (char)evnt_keybd();

} /* end of gmh_rdch */



/*------------------------------------------------------------------------*/



static void gmh_crsr( void )

/* selects new cursor (hazard version) */

{
   /* local variables */
   char     str[3];
   int      ascnum;

   /* executable code */
   str[0] = (char)Bconin(2);
   str[1] = (char)Bconin(2);
   str[2] = '\0';
   if  (sscanf(str,"%x",&ascnum) == 1)  gmv_crsrch = (char)ascnum;

} /* end of gmh_crsr */



/*------------------------------------------------------------------------*/


static int gm_vdihandle( void )  {return gmv_vdid;}


/*------------------------------------------------------------------------*/

#ifdef BC_KS_PRIVATE

void gm_pixhardcopy( int wdw, int mag, char outfile[] )

/* makes pixel hardcopy of window (creates DeskJet file "outfile")
 *
 * parameters of routine
 * int        wdw;     input; window number
 * int        mag;     input; magnification (0=normal, 1=double size)
 * char       outfile  output; output file (DeskJet)
 */
{
	/* executable code */

	px_window_hardcopy( gmv_vdid, gmv_wdwid[wdw], mag, outfile );

} /* end of gm_pixhardcopy */

#endif /* BC_KS_PRIVARE */

/*------------------------------------------------------------------------*/

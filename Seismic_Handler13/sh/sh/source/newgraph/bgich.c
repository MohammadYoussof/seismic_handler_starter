
/* File BGICH.C
 *      =======
 *
 * version 1, 30-DEC-91
 *
 * BGI channel of graphic output
 * K. Stammler, 30-Dec-1991
 */


#include <string.h>
#include <stdio.h>
#include <graphics.h>
#include BASECNST
#include BC_ASCII
#include BC_SYSBASE
#include "graphbas.h"
#include "bgusrdef.h"
#include "bgerrors.h"

#ifdef __TOS__
#include <ext.h>
#else
#include <dos.h>
#include <conio.h>
#endif

#ifdef BC_STDLIB_EX
#include <stdlib.h>
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


#define bgh_wdwopen(a) (((a) >= 0) && bgv_vp[a].open)
#define bgh_illwdw(w) (((w) <= 0) || ((w) >= GBC_MAXWDW))
#define bgh_checkvp(a) if  (bgv_currvp!=(a))  {\
        setviewport(bgv_vp[a].x,bgv_vp[a].y,\
        bgv_vp[a].x+bgv_vp[a].w-1,bgv_vp[a].y+bgv_vp[a].h-1,TRUE);\
        bgv_currvp = (a);}
#define bgh_theight (textheight("H")+3)

#define bg_read_character(c) (*(c))=getch()
#define bgh_inbuf() FALSE

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

typedef struct {
	int       x, y, w, h;      /* viewport pos and size */
	BOOLEAN   open;            /* window opened */
} VIEWPORT;

/* global variables */
static BOOLEAN   bgv_grinit;               /* is already initialized */
static int       bgv_graphdriver;          /* device driver */
static int       bgv_graphmode;            /* graphics mode */
static int       bgv_errorcode;            /* error code */
static int       bgv_currvp;               /* current viewport */
static VIEWPORT  bgv_vp[GBC_MAXWDW];       /* viewports */
static GBC_COO   bgv_txoff[GBC_MAXWDW];    /* transformation parameters */
static GBC_COO   bgv_txfac[GBC_MAXWDW];
static GBC_COO   bgv_tyoff[GBC_MAXWDW];    /* transformation parameters */
static GBC_COO   bgv_tyfac[GBC_MAXWDW];
static GBC_COO   bgv_xpos[GBC_MAXWDW];     /* current graphic position */
static GBC_COO   bgv_ypos[GBC_MAXWDW];
static int       bgv_xwrite[GBC_MAXWDW];   /* current write position in window */
static int       bgv_ywrite[GBC_MAXWDW];
static int       bgv_tcrsr={FALSE};        /* text cursor shown */
static char      bgv_crsrch={32};          /* text cursor character */
static char      bgv_scratchdir[BC_LINELTH+1]
                 ={"e:\\pure_c\\sh\\scratch\\sh$"}; /* scratch directory */
static char      bgv_lastline[GBC_MAXWDW][BC_LINELTH+1];   /* last line written */
static int       bgv_init;                 /* initialised windows */
static int       bgv_screenw;              /* screen width in pixel */
static int       bgv_screenh;              /* screen height in pixel */
static float     bgv_wpixelpercm;          /* horizontal pixels/cm */
static float     bgv_hpixelpercm;          /* vertical pixels/cm */
static LSTYLE    bgv_lstyle[LSTYLES];      /* line style blocks */
static CSTYLE    bgv_cstyle[CSTYLES];      /* char style blocks */
static int       bgv_currlstyle={0};       /* currently used line style block */
static int       bgv_currcstyle={0};       /* currently used char style block */

#ifdef XXX
static char   bgv_wdwname[GBC_MAXWDW][WDWNAMLTH+1] /* window names */
	= {" Wdw 0 "," Wdw 1 "," Wdw 2 "," Wdw 3 "," Wdw 4 "," Wdw 5 "," Wdw 6 "};
#endif


/* prototypes of local routines */
static void bgh_transform( int wdw, GBC_COO ux, GBC_COO uy, int *dx, int *dy );
static void bgh_backtrans( int wdw, int dx, int dy, GBC_COO *ux, GBC_COO *uy );
static void bgh_newline( int wdw );
static void bgh_carriage_return( int wdw );
static void bgh_scroll( int wdw );
static void bgh_erase( int wdw );
static void bgh_txtcrsr( int wdw, int state );
static void bgh_line( int wdw, int x0, int y0, int x1, int y1 );
static void bgh_lstyle( unsigned int style );
static void bgh_cstyle( unsigned int style );
static void bgh_drawborder( void );
static void bgh_getbordersize( int wx, int wy, int ww, int wh, int off,
	int *x, int *y, int *w, int *h );


/*------------------------------------------------------------------------*/



void bg_init( int wdw, int attribs, GBC_COO xlo, GBC_COO ylo,
	GBC_COO width, GBC_COO height, STATUS *status )

/* initialises BGI window
 *
 * parameters of routine
 * int        wdw;       input; window to be initialized
 * int        attribs;   input; window attributes
 * GBC_COO    xlo;       input; x-coo of lower left corner
 * GBC_COO    ylo;       input; y-coo of lower left corner
 * GBC_COO    width;     input; widths of window
 * GBC_COO    height;    input; height of window
 * STATUS     *status;   output; return status
 */
{
   /* local variables */
	int      wx, wy, ww, wh;  /* window coo */
	int      i;               /* counter */

   /* executable code */

	if  (bgh_illwdw(wdw))  return;   /* should not occur */

   if  (((1<<wdw) & bgv_init) != 0)  {
      *status = BGE_INITWICE;
      return;
   } else if  (wdw >= 0)  {
      if  (bgv_vp[wdw].open)  {
         *status = BGE_INITWICE;
         return;
      } /*endif*/
   } /*endif*/

   if  (!bgv_grinit)  {
		bgv_grinit = TRUE;
      /* v_hide_c( gmv_vdid ); */
      /* graf_mouse( HOURGLASS, 0L ); */
		bgv_graphdriver = DETECT;
		initgraph( &bgv_graphdriver, &bgv_graphmode, "..\\FONTS" );
		bgv_errorcode = graphresult();   /* get result of initialization */
		if  (bgv_errorcode != grOk)  {
			printf( "*** Graphics System Error: %s\n", grapherrormsg(bgv_errorcode) );
			exit( 1 );
		} /*endif*/
		bgv_screenw = getmaxx()+1;
		bgv_screenh = getmaxy()+1;
		bgv_wpixelpercm = 37.8; /*!!!*/
		bgv_hpixelpercm = 37.8; /*!!!*/
		cleardevice();
		setgraphmode( getgraphmode() );

      for  (i=0;i<LSTYLES;i++)  {
         bgv_lstyle[i].wrmode = GBC_REPLACE;
         bgv_lstyle[i].lntype = 0;
         bgv_lstyle[i].lnpatt = 0xaaaa;
         bgv_lstyle[i].lnwidth = 1;
         bgv_lstyle[i].lncolor = 1;
      } /*endfor*/
      for  (i=0;i<CSTYLES;i++)  {
         bgv_cstyle[i].wrmode = GBC_REPLACE;
         bgv_cstyle[i].cheight = 1;
         bgv_cstyle[i].angle = 0;
         bgv_cstyle[i].cfont = 0;
         bgv_cstyle[i].ceffects = 0;
         bgv_cstyle[i].ccolor = 1;
      } /*endfor*/
      bgh_cstyle( DSTYLE );
      bgh_lstyle( DSTYLE );

   } else {
      /* v_hide_c( gmv_vdid ); */
   } /*endif*/

   if  (wdw >= 0)  {
      bgv_vp[wdw].w = (int)(width*bgv_wpixelpercm);
      bgv_vp[wdw].h = (int)(height*bgv_hpixelpercm);
      bgv_vp[wdw].x = (int)(xlo*bgv_wpixelpercm);
      bgv_vp[wdw].y = bgv_screenh - (int)(ylo*bgv_hpixelpercm) - bgv_vp[wdw].h;
		bgh_getbordersize( bgv_vp[wdw].x, bgv_vp[wdw].y, bgv_vp[wdw].w,
			bgv_vp[wdw].h, 5, &wx, &wy, &ww, &wh );

		/* check window size */
      if  (ww <=0)  ww = bgv_screenw;
      if  (wh <=0)  wh = bgv_screenh;
      if  ((wx < 0) || (wx >= bgv_screenw))  wx = 0;
      if  ((wx+ww) > bgv_screenw)  ww = bgv_screenw - wx;
      if  ((wy < 0) || (wy >= bgv_screenh))  wy = 0;
      if  ((wy+wh) > bgv_screenh)  wh = bgv_screenh - wy;

		bgh_getbordersize( wx, wy, ww, wh, -5, &bgv_vp[wdw].x,
			&bgv_vp[wdw].y, &bgv_vp[wdw].w, &bgv_vp[wdw].h );

		setviewport( wx, wy, wx+ww-1, wy+wh-1, TRUE );
		bgh_drawborder();
		bgh_getbordersize( wx, wy, ww, wh, -4, &wx, &wy, &ww, &wh );
		setviewport( wx, wy, wx+ww-1, wy+wh-1, TRUE );
		bgh_drawborder();
		setviewport( bgv_vp[wdw].x, bgv_vp[wdw].y,
			bgv_vp[wdw].x+bgv_vp[wdw].w-1,
			bgv_vp[wdw].y+bgv_vp[wdw].h-1, TRUE );

		clearviewport();
		settextjustify( LEFT_TEXT, BOTTOM_TEXT );
		bgv_vp[wdw].open = TRUE;
      bgv_xwrite[wdw] = XWRITEOFF;
      bgv_ywrite[wdw] = bgh_theight + YWRITEOFF;
      bgv_init |= (1 << wdw);
		bgv_currvp = wdw;
   } /*endif*/

   /* v_show_c( gmv_vdid, 1 ); */

	if  (attribs)  {}  /* just to access attribs */

} /* end of bg_init */



/*------------------------------------------------------------------------*/



void bg_exit( int wdw )

/* closes window
 *
 * parameters of routine
 * int        wdw;          input; window to be closed
 */
{
   /* executable code */

	if  (bgh_illwdw(wdw))  return;

   if  (bgh_wdwopen(wdw))  {
      /* v_hide_c( gmv_vdid ); */
      bgv_vp[wdw].open = FALSE;
      bgv_init &= ~(1 << wdw);
		bgv_currvp = -1;
      /* v_show_c( gmv_vdid, 1 ); */
   } /*endif*/

} /* end of bg_exit */



/*------------------------------------------------------------------------*/



void bg_finish( void )

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
		if  ((1<<i) & bgv_init)
			bg_exit( i );

	bgv_init = 0;
	closegraph();

	bgv_grinit = FALSE;

} /* end of bg_finish */



/*------------------------------------------------------------------------*/



void bg_resizewdw( int wdw, GBC_COO xpos, GBC_COO ypos,
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
   int      wx,wy,ww,wh;     /* dev coo's */

   /* executable code */

	if  (bgh_illwdw(wdw))  return;
   if  (!bgh_wdwopen(wdw))  return;
	if  (Severe(status))  {}  /* just to access status */

	/* remove old border and window content */
	bgh_getbordersize( bgv_vp[wdw].x, bgv_vp[wdw].y, bgv_vp[wdw].w,
		bgv_vp[wdw].h, 5, &wx, &wy, &ww, &wh );
	setviewport( wx, wy, wx+ww-1, wy+wh-1, TRUE );
	clearviewport();

	/* new size */
   bgv_vp[wdw].w = (int)(width*bgv_wpixelpercm);
   bgv_vp[wdw].h = (int)(height*bgv_hpixelpercm);
   bgv_vp[wdw].x = (int)(xpos*bgv_wpixelpercm);
   bgv_vp[wdw].y = bgv_screenh - (int)(ypos*bgv_hpixelpercm) - bgv_vp[wdw].h;
	bgh_getbordersize( bgv_vp[wdw].x, bgv_vp[wdw].y, bgv_vp[wdw].w,
		bgv_vp[wdw].h, 5, &wx, &wy, &ww, &wh );

	/* check window size */
   if  (ww <=0)  ww = bgv_screenw;
   if  (wh <=0)  wh = bgv_screenh;
   if  ((wx < 0) || (wx >= bgv_screenw))  wx = 0;
   if  ((wx+ww) > bgv_screenw)  ww = bgv_screenw - wx;
   if  ((wy < 0) || (wy >= bgv_screenh))  wy = 0;
   if  ((wy+wh) > bgv_screenh)  wh = bgv_screenh - wy;

	setwritemode( COPY_PUT );

	bgh_getbordersize( wx, wy, ww, wh, -5, &bgv_vp[wdw].x,
		&bgv_vp[wdw].y, &bgv_vp[wdw].w, &bgv_vp[wdw].h );

	setviewport( wx, wy, wx+ww-1, wy+wh-1, TRUE );
	bgh_drawborder();
	bgh_getbordersize( wx, wy, ww, wh, -4, &wx, &wy, &ww, &wh );
	setviewport( wx, wy, wx+ww-1, wy+wh-1, TRUE );
	bgh_drawborder();
	setviewport( bgv_vp[wdw].x, bgv_vp[wdw].y,
		bgv_vp[wdw].x+bgv_vp[wdw].w-1,
		bgv_vp[wdw].y+bgv_vp[wdw].h-1, TRUE );

	/* v_show_c( gmv_vdid, 1 ); */
   bgv_xwrite[wdw] = XWRITEOFF;
   bgv_ywrite[wdw] = bgh_theight + YWRITEOFF;
	bgv_currvp = wdw;

} /* end of bg_resizewdw */



/*------------------------------------------------------------------------*/



void bg_popwdw( int wdw )

/* pops window on top
 *
 * parameters of routine
 * int        wdw;            input; window number
 */
{
#ifdef XXX
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
#endif
} /* end of bg_popwdw */



/*------------------------------------------------------------------------*/



void bg_setwdwname( int wdw, char name[] )

/* sets new window name
 *
 * parameters of routine
 * int       wdw;       input; window number
 * char      name[];    input; new name
 */
{
	/* executable code */

#ifdef XXX
	if  (gmh_illwdw(wdw))  return;
	if  (!gmh_wdwopen(wdw))  return;
	strncpy( wdwname_gcv[wdw], name, WDWNAMLTH );
	wind_set( gmv_wdwid[wdw], WF_NAME, wdwname_gcv[wdw] );
#endif

} /* end of bg_setwdwname */



/*------------------------------------------------------------------------*/



void bg_setstyle( int wdw, int style, char item[], char value[],
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
	/* float    r;*/        /* real parameter */

	/* executable code */

	if  (!bgh_wdwopen(wdw))  {
		return;
	} /*endif*/

   if (style >= CSTYLES || style >= LSTYLES  ||  style < 0)  {
		*status = BGE_ILSTYLE;
		return;
	} /*endif*/
	if  (strcmp(item,"CHARSIZE") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = BGE_ILVALUE;
			return;
		} /*endif*/
		bgv_cstyle[style].cheight = num;
#ifdef XXX
		if  (sscanf(value,"%f",&r) != 1)  {
			*status = BGE_ILVALUE;
			return;
		} /*endif*/
		bgv_cstyle[style].cheight = Nint( r*(float)bgv_vp[wdw].h );
#endif
	} else if  (strcmp(item,"WRMODE") == 0)  {
	} else if  (strcmp(item,"LINESTYLE") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = BGE_ILVALUE;
			return;
		} /*endif*/
	   bgv_lstyle[style].lntype = num;
	} else if  (strcmp(item,"COLOR") == 0)  {
		/* not implemented */
	} else if  (strcmp(item,"CHARROT") == 0)  {
	} else if  (strcmp(item,"FONT") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = BGE_ILVALUE;
			return;
		} /*endif*/
		bgv_cstyle[style].cfont = num;
	} else if  (strcmp(item,"LINEWIDTH") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = BGE_ILVALUE;
			return;
		} /*endif*/
		bgv_lstyle[style].lnwidth = num;
	} else if  (strcmp(item,"LINEPATTERN") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = BGE_ILVALUE;
			return;
		} /*endif*/
	   bgv_lstyle[style].lnpatt = num;
	} else if  (strcmp(item,"CHARSIZE_ATARI") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = BGE_ILVALUE;
			return;
		} /*endif*/
		bgv_cstyle[style].cheight = num;
	} else if  (strcmp(item,"TEXTEFFECTS") == 0)  {
		if  (sscanf(value,"%d",&num) != 1)  {
			*status = BGE_ILVALUE;
			return;
		} /*endif*/
		bgv_cstyle[style].ceffects = num;
	} else {
		*status = BGE_UKITEM;
		return;
	} /*endif*/

   if  (style == bgv_currcstyle)  bgh_cstyle( style );
   if  (style == bgv_currlstyle)  bgh_lstyle( style );

} /* end of bg_setstyle */



/*------------------------------------------------------------------------*/



void bg_setcoo( int wdw, GBC_COO x, GBC_COO y, GBC_COO w, GBC_COO h,
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

   /* executable code */

	if  (bgh_illwdw(wdw))  return;

	if  ((wdw >= 0) && (x == 0.0) && (y == 0.0) && (w == 0.0) && (h == 0.0))  {
		x = coosave[wdw][0];
		y = coosave[wdw][1];
		w = coosave[wdw][2];
		h = coosave[wdw][3];
	} /*endif*/

   if  ((w <= 0.) || (h <= 0.))  {
      *status = BGE_ILLCOO;
      return;
   } /*endif*/

   if  (bgh_wdwopen(wdw))  {
      bgv_txfac[wdw] = (float)bgv_vp[wdw].w / w;
      bgv_tyfac[wdw] = (float)bgv_vp[wdw].h / h;
      bgv_txoff[wdw] = bgv_txfac[wdw]*x/* - (float)bgv_vp[wdw].x*/;
      bgv_tyoff[wdw] = bgv_tyfac[wdw]*(-y-h)/* - (float)bgv_vp[wdw].y*/;
		coosave[wdw][0] = x;
		coosave[wdw][1] = y;
		coosave[wdw][2] = w;
		coosave[wdw][3] = h;
   } /*endif*/

} /* end of bg_setcoo */



/*------------------------------------------------------------------------*/



float bg_aspectratio( int wdw )

/* returns ratio of width to height of window "wdw"
 *
 * parameters of routine
 * int        wdw;      input; window number
 *                      returns aspect ratio
 */
{
	/* local variables */

	/* executable code */

   if  (bgh_wdwopen(wdw))  {
		return  (float)bgv_vp[wdw].w / (float)bgv_vp[wdw].h;
	} /*endif*/
	return 1.0;

} /* end of bg_aspectratio */



/*------------------------------------------------------------------------*/



void bg_moveto( int wdw, GBC_COO x, GBC_COO y )

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        wdw;      input; window
 * GBC_COO    x, y;     input; location to move to
 */
{
   /* executable code */

	if  (bgh_illwdw(wdw))  return;
   if  (bgh_wdwopen(wdw))  {
      bgv_xpos[wdw] = x;
      bgv_ypos[wdw] = y;
   } /*endif*/

} /* end of bg_moveto */



/*------------------------------------------------------------------------*/



void bg_drawto( int wdw, int style, GBC_COO x, GBC_COO y )

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        wdw;       input; window
 * int        style;     input; attribute block number
 * GBC_COO    x, y;      input; location to draw to
 */
{
   /* local variables */
   int      l[4];            /* line coordinates */

   /* executable code */

   if  (style != bgv_currlstyle)  bgh_lstyle( style );

	if  (bgh_illwdw(wdw))  return;
   if  (bgh_wdwopen(wdw))  {
		bgh_checkvp(wdw);
      bgh_transform( wdw, bgv_xpos[wdw], bgv_ypos[wdw], l, l+1 );
      bgh_transform( wdw, x, y, l+2, l+3 );
		/* v_hide_c( gmv_vdid ); */
		line( l[0], l[1], l[2], l[3] );
		/* v_show_c( gmv_vdid, 1 ); */
      bgv_xpos[wdw] = x;
      bgv_ypos[wdw] = y;
   } /*endif*/

} /* end of bg_drawto */



/*------------------------------------------------------------------------*/



void bg_arrayplot( int wdw, int style, long cnt, int red, GBC_COO xoff,
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
/* #  define MAXPLOT 128 */   /* must be less than SYC_MAXINT; ATARI: <= 128; */
#  define MAXPLOT 1024    /* must be less than SYC_MAXINT; ATARI: <= 128; */

   /* local variables */
   int      *pxy;     /* pointer to xy-array */
   long     i, j;     /* counters */

   /* executable code */

   if  (style != bgv_currlstyle)  bgh_lstyle( style );

   if  (cnt == 0L)  return;
	if  (bgh_illwdw(wdw))  return;
   if  (!bgh_wdwopen(wdw))  return;
	bgh_checkvp(wdw);

   /* allocate memory */
   pxy = (int *)sy_allocmem( (long)MAXPLOT, (int)sizeof(int)*2, status );
   if  (*status != BGE_NOERROR)  return;
   /* v_hide_c( gmv_vdid ); */

   if  (red == 1)  {
      i = 0;
      while  (i < cnt)  {
         for  (j=0;j<MAXPLOT;j++)  {
            bgh_transform( wdw, xoff+xinc*(GBC_COO)i,
               yoff + yzoom * (*yarr++), pxy+2*j, pxy+2*j+1 );
            if  (++i == cnt)  {
               j++;
               break;
            } /*endif*/
         } /*endfor*/
         if  (i != cnt)  {i--; yarr--;}  /* connect lines */
			drawpoly( (int)j, pxy );
      } /*endwhile*/
   } else {
      i = 0;
      while  (i < cnt)  {
         for  (j=0;j<MAXPLOT;j++)  {
            bgh_transform( wdw, xoff+xinc*(GBC_COO)i,
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
			drawpoly( (int)j, pxy );
      } /*endwhile*/
   } /*endif*/

   /* v_show_c( gmv_vdid, 1 ); */
   sy_deallocmem( pxy );

} /* end of bg_arrayplot */



/*------------------------------------------------------------------------*/



void bg_erase( int wdw )

/* clears window
 *
 * parameters of routine
 * int        wdw;       input; window
 */
{
   /* local variables */

   /* executable code */

	if  (bgh_illwdw(wdw))  return;
   if  (bgh_wdwopen(wdw))  {
		bgh_checkvp(wdw);
      /* v_hide_c( gmv_vdid ); */
		clearviewport();
      /* v_show_c( gmv_vdid, 1 ); */
   } /*endif*/

} /* end of bg_erase */



/*------------------------------------------------------------------------*/



void bg_text( int wdw, int style, GBC_COO x, GBC_COO y, char text[] )

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
   int      tx, ty;   /* device text position */

   /* executable code */

	if  (*text == '\0')  return;
	/* if  (GCF_NOECHO & wdw)  return; */
	if  (style != bgv_currcstyle)  bgh_cstyle( style );

	if  (bgh_illwdw(wdw))  return;
   if  (bgh_wdwopen(wdw))  {
		bgh_checkvp(wdw);
      /* v_hide_c( gmv_vdid ); */
      bgh_transform( wdw, x, y, &tx, &ty );
		outtextxy( tx, ty, text );
      /* v_show_c( gmv_vdid, 1 ); */
   } /*endif*/

} /* end of bg_text */



/*------------------------------------------------------------------------*/



void bg_write( int wdw, char text[] )

/* writes text to window at current write position
 *
 * parameters of routine
 * int        wdw;          input; window
 * char       text[];       input; output text
 */
{
   /* local variables */
   char     ltxt[200];  /* local text */
   int      txtlth;     /* text length */

   /* executable code */

	if  (*text == '\0')  return;
	/* if  (GCF_NOECHO & wdw)  return; */
   if  (bgv_currcstyle != DSTYLE)  bgh_cstyle( DSTYLE );

	if  (bgh_illwdw(wdw))  return;
   if  (bgh_wdwopen(wdw))  {
		bgh_checkvp( wdw );

      /* v_hide_c( gmv_vdid ); */
      /* check <CR><LF> at beginning */
      if  (text[0] == '\n')  {
         bgh_newline( wdw );
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

		outtextxy( bgv_xwrite[wdw], bgv_ywrite[wdw], ltxt );

      if  (txtlth == -1)  {
         bgh_newline( wdw );
		} else if  (txtlth == -2)  {
			bgh_carriage_return( wdw );
      } else {
         bgv_xwrite[wdw] += txtlth*textwidth("a");
			/* store line: */
			if  ((txtlth+strlen(bgv_lastline[wdw])) <= BC_LINELTH)
				strcat( bgv_lastline[wdw], ltxt );
      } /*endif*/
      /* v_show_c( gmv_vdid, 1 ); */

   } /*endif*/

} /* end of bg_write */



/*------------------------------------------------------------------------*/



void bg_wrtch( int wdw, char ch )

/* writes a single character to window
 *
 * parameters of routine
 * int        wdw;         input; window
 * char       ch;          input; char to be written
 */
{
   /* local variables */
   char     ct[2];    /* output text */
   int      h, w;

   /* executable code */

	/* if  (GCF_NOECHO & wdw)  return; */
   if  (bgv_currcstyle != DSTYLE)  bgh_cstyle( DSTYLE );

	if  (bgh_illwdw(wdw))  return;
   if  (bgh_wdwopen(wdw))  {
		bgh_checkvp( wdw );
      /* v_hide_c( gmv_vdid ); */
      ct[0] = ch;
      ct[1] = '\0';
      if  (ch == DEL)  {
         bg_wrtch( wdw, BS );
         bg_wrtch( wdw, ' ' );
         bg_wrtch( wdw, BS );
      } else if  (ch == BS)  {
         bgv_xwrite[wdw] -= textwidth("a");
			if  (strlen(bgv_lastline[wdw]) < BC_LINELTH)
				strcat( bgv_lastline[wdw], ct );
      /*} else if  (ch == ' ')  {
	h = textheight( "H" );
	w = textwidth( "H" ); */
      } else if  (ch == '\n')  {
         bgh_newline( wdw );
      } else if  (ch == CR)  {
			bgh_carriage_return( wdw );
      } else {
			outtextxy( bgv_xwrite[wdw], bgv_ywrite[wdw], ct );
         bgv_xwrite[wdw] += textwidth("a");
			if  (strlen(bgv_lastline[wdw]) < BC_LINELTH)
				strcat( bgv_lastline[wdw], ct );
      } /*endif*/
      /* v_show_c( gmv_vdid, 1 ); */
   } /*endif*/

} /* end of bg_wrtch */



/*------------------------------------------------------------------------*/



int bg_txtwidth( int wdw )

/* returns width of window in characters
 *
 * parameters of routine
 * int		  wdw;		 input; window number
 */
{
	/* local variables */

   /* executable code */
	
	if  (bgh_illwdw(wdw))  return 0;
   if  (bgh_wdwopen(wdw))  {
		return (bgv_vp[wdw].w/textwidth("a"));
	} /*endif*/

	return 0;

} /* end of bg_txtwidth */



/*------------------------------------------------------------------------*/



int bg_txtheight( int wdw )

/* returns height of window in characters
 *
 * parameters of routine
 * int		  wdw;		 input; window number
 */
{
	/* local variables */

   /* executable code */

	if  (bgh_illwdw(wdw))  return 1;
   if  (bgh_wdwopen(wdw))  {
		return (bgv_vp[wdw].h/bgh_theight);
	} /*endif*/

	return 1;

} /* end of bg_txtheight */



/*------------------------------------------------------------------------*/



void bg_read( int wdw, int maxlth, char text[] )

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

   if  (bgv_currcstyle != DSTYLE)  bgh_cstyle( DSTYLE );

	echo = TRUE; /* echo = !(GCF_NOECHO & wdw); */
	if  (bgh_illwdw(wdw))  return;
   if  (!bgh_wdwopen(wdw))  return;
	bgh_checkvp( wdw );

   /* v_hide_c( gmv_vdid ); */
   i = 0;
   while  (i < maxlth-1)  {
      if  (!bgh_inbuf())  bgh_txtcrsr( wdw, TRUE );
      bg_read_character( text+i );
      bgh_txtcrsr( wdw, FALSE );
      if  ((text[i] == CR) || (text[i] == LF))  {
         text[i] = '\0';
         break;
      } else if  ((text[i] == BS) || (text[i] == DEL))  {
         if  (i != 0)  {
            if  (echo)  bg_wrtch( wdw, DEL );
            text[i--] = '\0';
         } /*endif*/
      } else if  (text[i] == '\t')  {
         text[i] = ' ';
         if  (echo)  bg_wrtch( wdw, text[i++] );
      } else if  (text[i] == ESC)  {
         for  (;i>0;i--)  {
            if  (echo)  bg_wrtch( wdw, DEL );
            text[i] = '\0';
         } /*endfor*/
         text[0] = '\0';
      } else if  (text[i] >= ' ')  {
         if  (echo)  bg_wrtch( wdw, text[i++] );
      } /*endif*/
   } /*endwhile*/

   if  (echo)  bgh_newline( wdw );

   /* v_show_c( gmv_vdid, 1 ); */

} /* end of bg_read */



/*------------------------------------------------------------------------*/



float bg_chheight( int wdw )

/* returns current character height in window "wdw"
 *
 * parameter of routine
 * int       wdw;      input; window number
 */
{
	/* executable code */

	if  (bgh_illwdw(wdw))  return 0.0;
   if  (!bgh_wdwopen(wdw))  return 0.0;
	return ((float)bgh_theight/bgv_tyfac[wdw]);

} /* end of bg_chheight */



/*------------------------------------------------------------------------*/



void bg_getloc( int wdw, GBC_COO *x, GBC_COO *y, char *ch )

/* requests mouse position in window
 *
 * parameters of routine
 * int        wdw;        input; window
 * GBC_COO    *x, *y;     output; location selected
 * char       *ch;        output; key pressed
 */
{
   /* local variables */
   static int   mx=-1, my=-1;     /* device coordinates */
	int      maxx, maxy; /* maximum coo */
	BOOLEAN  quit;       /* quit loop */

   /* executable code */

	if  (bgh_illwdw(wdw))  return;
   if  (bgh_wdwopen(wdw))  {
		bgh_checkvp( wdw );
		maxx = bgv_vp[wdw].x + bgv_vp[wdw].w - 1;
		maxy = bgv_vp[wdw].y + bgv_vp[wdw].h - 1;
      /* graf_mouse( THIN_CROSS, 0L ); */
	   setwritemode( XOR_PUT );
		if  (mx < 0 || mx > maxx || my < 0 || my > maxy)  {
			mx = bgv_vp[wdw].w/2;
			my = bgv_vp[wdw].h/2;
		} /*endif*/
		quit = FALSE;
      do  {
			line( 0, my, maxx, my );
			line( mx, 0, mx, maxy );
			bg_read_character( ch );
			line( 0, my, maxx, my );
			line( mx, 0, mx, maxy );
			if  (*ch == 'E' || *ch == 'e' || *ch == 'X'|| *ch == 'x')  break;
			switch  (*ch)  {
				case 'i': case '8': my -= 10;  break;
				case 'm': case '2': my += 10;  break;
				case 'j': case '4': mx -= 10;  break;
				case 'k': case '6': mx += 10;  break;
				case 'I':  my -= 1;  break;
				case 'M':  my += 1;  break;
				case 'J':  mx -= 1;  break;
				case 'K':  mx += 1;  break;
				default:  quit = TRUE; break;
			} /*endswitch*/
		}  while (!quit);
	   setwritemode( COPY_PUT );
      /* graf_mouse( HOURGLASS, 0L ); */
      bgh_backtrans( wdw, mx, my, x, y );
		*ch = Cap( *ch );
   } /*endif*/

} /* end of bg_getloc */



/*------------------------------------------------------------------------*/



void bg_set_outputdir( char dir[], STATUS *status )

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name of scratch path
 * STATUS    *status;   output; return status
 */
{
	/* executable code */

	if  (strlen(dir) > BC_FILELTH)  {
		*status = BGE_STROVFL;
		return;
	} /*endif*/
	strcpy( bgv_scratchdir, dir );

} /* end of bg_set_outputdir */



/*------------------------------------------------------------------------*/



void bg_prepare( int wdw, STATUS *status )

/* prepares window for redrawing
 *
 * int      wdw;      input; window
 * STATUS   *status;  output; return status
 */
{
	/* local variables */
	char     tpar[BC_LINELTH+1];  /* string */
	char     *c;                  /* moving pointer */

	/* executable code */

	if  (bgh_illwdw(wdw))  return;
	if  (!bgh_wdwopen(wdw))  return;

	bgh_erase( wdw );

	/* set write position */
   if  (bgv_currcstyle != DSTYLE)  bgh_cstyle( DSTYLE );
	bgv_xwrite[wdw] = XWRITEOFF;
	bgv_ywrite[wdw] = bgh_theight + YWRITEOFF;

	/* write last line written in text mode */
	if  (bgv_lastline[wdw][0] != '\0')  {
		strcpy( tpar, bgv_lastline[wdw] );
		bgv_lastline[wdw][0] = '\0';
		c = tpar;
		while  (*c != '\0')
			bg_wrtch( wdw, *c++ );
		bgh_txtcrsr( wdw, TRUE );
	} /*endif*/

	if  (Severe(status))  return;  /* only to use status */

} /* end of bg_prepare */



/*------------------------------------------------------------------------*/



void bg_cleanup( int wdw, char outf[], STATUS *status )

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

} /* end of bg_cleanup */



/*------------------------------------------------------------------------*/
/*                           supporting routines                          */
/*------------------------------------------------------------------------*/



static void bgh_transform( int wdw, GBC_COO ux, GBC_COO uy, int *dx, int *dy )

/* transforms user coordinates (ux,uy) to device coordinates (dx,dy)
 *
 * parameters of routine
 * int      wdw;          input; window number
 * GBC_COO  ux, uy;       input; user coordinates
 * int      *dx, *dy;     output; device coordinates
 */
{
   /* executable code */

   *dx = (int)(ux*bgv_txfac[wdw] - bgv_txoff[wdw]);
   *dy = (int)(-uy*bgv_tyfac[wdw] - bgv_tyoff[wdw]);

} /* end of bgh_transform */



/*------------------------------------------------------------------------*/



static void bgh_backtrans( int wdw, int dx, int dy, GBC_COO *ux, GBC_COO *uy )

/* transforms device coordinates (dx,dy) to user coordinates (ux,uy)
 *
 * parameters of routine
 * int      wdw;           input; window number
 * int      dx, dy;        input; device coordinates
 * GBC_COO  *ux, *uy;      output; user coordinates
 */
{
   /* executable code */

   *ux = ((float)dx + bgv_txoff[wdw]) / bgv_txfac[wdw];
   *uy = -((float)dy + bgv_tyoff[wdw]) / bgv_tyfac[wdw];

} /* end of bgh_backtrans */



/*------------------------------------------------------------------------*/



static void bgh_newline( int wdw )

/* starts a new output line in window
 *
 * parameters of routine
 * int      wdw;          input; window number
 */
{
   /* local variables */

   /* executable code */

   bgv_xwrite[wdw] = XWRITEOFF;
   if  ((bgv_ywrite[wdw]+bgh_theight+YWRITEOFF) > bgv_vp[wdw].h-1)  {
      bgh_scroll( wdw );
   } else {
      bgv_ywrite[wdw] += bgh_theight;
   } /*endif*/
	bgv_lastline[wdw][0] = '\0';

} /* end of bgh_newline */



/*------------------------------------------------------------------------*/



static void bgh_carriage_return( int wdw )

/* restarts a output line in wdw
 *
 * parameters of routine
 * int      wdw;            input; window number
 */
{
   /* local variables */

   /* executable code */

   bgv_xwrite[wdw] = XWRITEOFF;
	bgv_lastline[wdw][0] = '\0';

} /* end of bgh_carriage_return */



/*------------------------------------------------------------------------*/



static void bgh_scroll( int wdw )

/* scrolls down window one line
 *
 * parameters of routine
 * int        wdw;       input; window number
 */
{
	clearviewport();
	bgv_xwrite[wdw] = XWRITEOFF;
	bgv_ywrite[wdw] = bgh_theight + YWRITEOFF;

#ifdef XXX
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
   w[7] = w[3]-bgh_theight;
   w[1] += bgh_theight;
   b[0] = w[0];
   b[1] = w[3]-bgh_theight+1;
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
#endif
} /* end of bgh_scroll */



/*------------------------------------------------------------------------*/

#ifdef XXX

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

#endif

/*------------------------------------------------------------------------*/



static void bgh_erase( int wdw )

/* clears window
 *
 * parameters of routine
 * int      wdw;           input; window number
 */
{
   /* local variables */

   /* executable code */

   /* v_hide_c( gmv_vdid ); */
	bgh_checkvp( wdw );
	clearviewport();
   /* v_show_c( gmv_vdid, 1 ); */

} /* end of gmh_erase */



/*------------------------------------------------------------------------*/



static void bgh_txtcrsr( int wdw, int state )

/* displays or hides a text cursor at the current position
 *
 * parameters of routine
 * int      wdw;           window number
 * int      state;         input; TRUE=show cursor, FALSE=hide cursor
 */
{
   /* local variables */
   char     ct[2];    /* output text */

   /* executable code */

   if  ((!bgv_tcrsr) && !state)  return;
   ct[0] = (state) ? bgv_crsrch : ' ';
   ct[1] = '\0';

   if  (bgv_currcstyle != DSTYLE)  bgh_cstyle( DSTYLE );

   if  (bgh_wdwopen(wdw))  {
		outtextxy( bgv_xwrite[wdw], bgv_ywrite[wdw], ct );
   } /*endif*/

   bgv_tcrsr = state;

} /* end of bgh_txtcrsr */



/*------------------------------------------------------------------------*/



static void bgh_lstyle( unsigned style )

/* sets the currently used line style block
 *
 * parameters of routine
 * unsigned int  style;      input; line style block number
 */
{
   /* executable code */

   if  (style >= LSTYLES)  return;
   /* vswr_mode( gmv_vdid, gmv_lstyle[style].wrmode ); */
	setlinestyle( bgv_lstyle[style].lntype, bgv_lstyle[style].lnpatt,
		bgv_lstyle[style].lnwidth );
   bgv_currlstyle = style;

} /* end of bgh_lstyle */



/*------------------------------------------------------------------------*/



static void bgh_cstyle( unsigned style )

/* sets the currently used char style block
 *
 * parameters of routine
 * unsigned   style;     input; line style block number
 */
{
   /* local variables */

   /* executable code */

   if  (style >= CSTYLES)  return;
   /* vswr_mode( gmv_vdid, gmv_cstyle[style].wrmode ); */
	settextstyle( bgv_cstyle[style].cfont, HORIZ_DIR,
		bgv_cstyle[style].cheight );
   /* vst_effects( gmv_vdid, gmv_cstyle[style].ceffects ); */
   /* vst_color( gmv_vdid, gmv_cstyle[style].ccolor ); */
   bgv_currcstyle = style;

} /* end of bgh_cstyle */



/*------------------------------------------------------------------------*/



static void bgh_drawborder( void )

/* draws viewport border
 */
{
	/* local variables */
	struct viewporttype vp;   /* viewport type */

	/* executable code */

	/* setcolor( MaxColors - 1 ); */           /* Set current color to white   */

	setlinestyle( SOLID_LINE, 0, NORM_WIDTH );
	getviewsettings( &vp );
	rectangle( 0, 0, vp.right-vp.left, vp.bottom-vp.top );

} /* end of bgh_drawborder */



/*------------------------------------------------------------------------*/



static void bgh_getbordersize( int wx, int wy, int ww, int wh, int off,
	int *x, int *y, int *w, int *h )

/* computes size of window including border
 *
 * parameters of routine
 * int        wx,wy,ww,wh;    input; workinf area
 * int        off;            input; size of border
 * int        *x,*y,*w,*h;    output; total area
 */
{
	/* executable code */

	*x = wx - off;
	*y = wy - off;
	*w = ww + 2*off;
	*h = wh + 2*off;

} /* end of getbordersize */



/*------------------------------------------------------------------------*/

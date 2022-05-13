
/* file DJCH.C
 *      ======
 *
 * version 4, 5-Jan-92
 *
 * HP DeskJet channel for graphics output module
 * K. Stammler, 20-NOV-1991
 */


#include <stdio.h>
#include <string.h>
#include <vdi.h>
#include BASECNST
#include BC_SYSBASE
#include "graphbas.h"
#include "djusrdef.h"
#include "djerrors.h"


/* global constants */
#define DEFAULTWIDTH 100.0
   /* default width of user coo */
#define DEFAULTHEIGHT 100.0
   /* default height of user coo */
#define DEFAULTPLOTW 25.9
#define DEFAULTPLOTH 19.5
   /* default plot size */
#define DOTSPERCM 118.11 /*28.35*/
   /* resolution of printer */

#define MAXWIDTH 2352
#define MAXHEIGHT 3300

#define DJFILENAME "HC.vd"
   /* DeskJet file */
#define DJHEADER "e:\\pure_c\\sh\\local\\header.vd"
   /* DeskJet header file */

/* fct ID's */
#define ID_PLINE 1
#define ID_GTEXT 2
#define ID_WRMODE 3
#define ID_THEIGHT 4
#define ID_TROTATION 5
#define ID_TFONT 6
#define ID_TEFFECTS 7
#define ID_TCOLOR 8
#define ID_LTYPE 9
#define ID_LUDSTY 10
#define ID_LWIDTH 11
#define ID_LCOLOR 12

/* styles */
#define LSTYLES 10              /* maximum number of line style blocks */
#define CSTYLES 10              /* maximum number of char style blocks */
#define DSTYLE  0               /* default style block */

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


/* global variable */
static char djv_outputdir[BC_FILELTH+1];  /* output directory */


/* global variables */
static BOOLEAN djv_isinit;           /* driver is initialised */
static DJOUT   djv_width;            /* width of display */
static DJOUT   djv_height;           /* heigth of display */
static DJOUT   djv_xoffset;          /* x-offset */
static DJOUT   djv_yoffset;          /* y-offset */
static float   djv_trafo_xf;         /* trafo x-factor */
static float   djv_trafo_yf;         /* trafo y-factor */
static DJOUT   djv_trafo_xo;         /* x-offset */
static DJOUT   djv_trafo_yo;         /* y-offset */
static BOOLEAN djv_arrayswap=FALSE;  /* swap arrayplot coo's */
static LSTYLE  djv_lstyle[LSTYLES];  /* line style blocks */
static CSTYLE  djv_cstyle[CSTYLES];  /* char style blocks */
static int     djv_currlstyle;       /* currently used line style */
static int     djv_currcstyle;       /* currently used char style */
static FILE    *djv_pf;              /* pointer to VDI file */
static BOOLEAN djv_move;             /* moveto buffer is not empty */
static float   djv_move_x;           /* moveto x-position */
static float   djv_move_y;           /* moveto y-position */
static char    djv_currdjf[BC_FILELTH+1];  /* name of active VDI file */

/* prototypes of local routines */
static void djh_trafo( GBC_COO ux, GBC_COO uy, DJOUT *dx, DJOUT *dy );
static void djh_create_hcname( char name[] );
static void djh_insertheader( FILE *dj );
static void djh_closedjf( FILE *dj );
static void djh_cstyle( unsigned style );
static void djh_lstyle( unsigned style );



/*----------------------------------------------------------------------------*/



void dj_init( int attribs, float xlo, float ylo, float width,
	float height, STATUS *status )

/* initialises GDOS channel
 *
 * parameters of routine
 * int        attribs;        input; attributes
 * float      xlo, ylo;       input; offset of output
 * float      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */
{
   /* local variables */
   int      i;      /* counter */

   /* executable code */

	if  (attribs == 0)  {
		;                  /* just to access attrib once */
	} /*endif*/

   xlo *= DOTSPERCM; ylo *= DOTSPERCM;
	width *= DOTSPERCM; height *= DOTSPERCM;

   if  (*djv_currdjf == '\0')  {
      strcpy( djv_currdjf, djv_outputdir );
      strcat( djv_currdjf, DJFILENAME );
   } /*endif*/

   if  ((width <= 0.0) || (height <= 0.0))  {
      *status = DJE_ILPAR;
      return;
   } /*endif*/

   if  (djv_isinit)  dj_exit();

   djv_xoffset = Nint( xlo );
   djv_yoffset = Nint( ylo );
   djv_width = Nint( width );
   djv_height = Nint( height );
	if  (djv_width > MAXWIDTH)  djv_width = MAXWIDTH;
	if  (djv_height > MAXHEIGHT)  djv_height = MAXHEIGHT;
	if  (djv_xoffset+djv_width > MAXWIDTH)
		djv_xoffset = MAXWIDTH-djv_width;
	if  (djv_yoffset+djv_height > MAXWIDTH)
		djv_yoffset = MAXHEIGHT-djv_height;

	/* DJ styles ... */ /*???*/
   for  (i=0;i<LSTYLES;i++)  {
      djv_lstyle[i].wrmode = GBC_REPLACE;
      djv_lstyle[i].lntype = 1;
      djv_lstyle[i].lnpatt = 0xaaaa;
      djv_lstyle[i].lnwidth = 1;
      djv_lstyle[i].lncolor = 1;
   } /*endfor*/
   for  (i=0;i<CSTYLES;i++)  {
      djv_cstyle[i].wrmode = GBC_REPLACE;
      djv_cstyle[i].cheight = 13;
      djv_cstyle[i].angle = 0;
      djv_cstyle[i].cfont = 1;
      djv_cstyle[i].ceffects = 0;
      djv_cstyle[i].ccolor = 1;
   } /*endfor*/
   djh_cstyle( DSTYLE );
   djh_lstyle( DSTYLE );
	djv_currlstyle = DSTYLE;
	djv_currcstyle = DSTYLE;

   /* open DeskJet file */
   djv_pf = sy_fopen( djv_currdjf, "w" ); /*!!! wb !!!*/
   if  (djv_pf == NULL)  {
      *status = DJE_FOPNWR;
      return;
   } /*endif*/
   djh_insertheader( djv_pf );

   djv_isinit = TRUE;
   djv_move = FALSE;

   dj_setcoo( 0., 0., DEFAULTWIDTH, DEFAULTHEIGHT, status );

} /* end of dj_init */



/*----------------------------------------------------------------------------*/



void dj_exit( void )

/* exits DeskJet channel
 *
 * no parameters
 */
{
   /* executable code */

   if  (!djv_isinit)  return;
   fclose( djv_pf );
   sy_fdelete( djv_currdjf );
   djv_isinit = FALSE;

} /* end of dj_exit */



/*----------------------------------------------------------------------------*/



void dj_finish( void )

/* finishes graphics
 *
 */
{
	/* executable code */

	djv_isinit = FALSE;

} /* end of dj_finish */



/*----------------------------------------------------------------------------*/



void dj_resize( float xlo, float ylo, float width, float height,
   STATUS *status )

/* resizes DeskJet output
 *
 * parameters of routine
 * DJOUT      xlo, ylo;       input; offset of output
 * DJOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */
{
   /* executable code */

   xlo *= DOTSPERCM; ylo *= DOTSPERCM;
	width *= DOTSPERCM; height *= DOTSPERCM;

   if  ((width <= 0.0) || (height <= 0.0))  {
      *status = DJE_ILPAR;
      return;
   } /*endif*/

   if  (!djv_isinit)  dj_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

   djv_xoffset = Nint( xlo );
   djv_yoffset = Nint( ylo );
   djv_width = Nint( width );
   djv_height = Nint( height );
	if  (djv_width > MAXWIDTH)  djv_width = MAXWIDTH;
	if  (djv_height > MAXHEIGHT)  djv_height = MAXHEIGHT;
	if  (djv_xoffset+djv_width > MAXWIDTH)
		djv_xoffset = MAXWIDTH-djv_width;
	if  (djv_yoffset+djv_height > MAXWIDTH)
		djv_yoffset = MAXHEIGHT-djv_height;

} /* end of dj_resize */



/*----------------------------------------------------------------------------*/



void dj_erase( void )

/* clears DeskJet output channel
 *
 * no parameters
 */
{
   /* local variables */
   int      dmy=0;

   /* executable code */

   if  (djv_isinit)  {
      fseek( djv_pf, 0L, 0 );
      djh_insertheader( djv_pf );
		djv_move = FALSE;
   } else {
      dj_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );
   } /*endif*/

} /* end of dj_erase */



/*----------------------------------------------------------------------------*/



void dj_setcoo( GBC_COO x, GBC_COO y, GBC_COO w, GBC_COO h, STATUS *status )

/* sets user coordinates
 *
 * parameters of routine
 * GBC_COO    x, y;     input; offset
 * GBC_COO    w, h;     input; size
 * STATUS     *status;  output; return status
 */
{
   /* executable code */

   if  ((w <= 0.0) || (h <= 0.0))  {
      *status = DJE_ILPAR;
      return;
   } /*endif*/

   if  (!djv_isinit)  dj_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

   djv_trafo_xf = djv_width / w;
   djv_trafo_yf = djv_height / h;
   djv_trafo_xo = x*djv_trafo_xf - djv_xoffset;
   djv_trafo_yo = -djv_trafo_yf*(y+h) - djv_yoffset;

} /* end of dj_setcoo */



/*----------------------------------------------------------------------------*/



void dj_moveto( GBC_COO x, GBC_COO y )

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * GBC_COO    x, y;    input; position
 */
{
   /* local variables */
   STATUS   dmy=0;    /* scratch */

   /* executable code */

   if  (!djv_isinit)  dj_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );

   djv_move_x = x;
   djv_move_y = y;
   djv_move = TRUE;

} /* end of dj_moveto */



/*----------------------------------------------------------------------------*/



void dj_drawto( int style, GBC_COO x, GBC_COO y )

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        style;   input; style parameter
 * GBC_COO    x, y;    input; position
 */
{
   /* local variables */
   STATUS   dmy=0;    /* scratch */
	static float    last_x, last_y;
	int      w[4];     /* line coo's */

   /* executable code */

   if  (!djv_isinit)  dj_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );

   if  (style != djv_currlstyle)  djh_lstyle( style );

   if  (djv_move)  {
      djh_trafo( djv_move_x, djv_move_y, w, w+1 );
      djv_move = FALSE;
	} else {
      djh_trafo( last_x, last_y, w, w+1 );
   } /*endif*/
	last_x = x;  last_y = y;

   djh_trafo( x, y, w+2, w+3 );
	fprintf( djv_pf, "%d 2 %d %d %d %d\n", ID_PLINE, w[0],w[1],w[2],w[3] );
	/* v_pline( djv_p, 2, w ); */

} /* end of dj_drawto */


/*----------------------------------------------------------------------------*/



void dj_arrayplot( int style, long cnt, int red, GBC_COO xoff,
   GBC_COO xinc, GBC_COO yoff, GBC_COO yarr[], float yzoom, STATUS *status )

/* plots array of data points
 *
 * parameters of routine
 * int        style;       input; style parameter
 * long       cnt;         input; number of data samples in yarr
 * int        red;         input; reduction factor
 * GBC_COO    xoff;        input; x offset
 * GBC_COO    xinc;        input; x increment
 * GBC_COO    yoff;        input; y offset
 * GBC_COO    yarr[];      input; data array
 * float      yzoom;       input; amplitude zoom factor
 * STATUS     *status;     output; return status
 */
{
	/* constants */
#	define MAXPLOT 128

   /* local variables */
   long     i, j, k;    /* sample counters */
	int      *pxy;       /* xy-array */

   /* executable code */

   if  (!djv_isinit)  dj_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

   if  (style != djv_currlstyle)  djh_lstyle( style );

#ifdef XXX
   if  (djv_arrayswap)  {
      cx = yoff + (*yarr)*yzoom;
      cy = xoff;
      djh_trafo( cx, cy, &dx, &dy );
      fprintf( djv_pf, "%d %d M\n", Nint(dx), Nint(dy) );
      for  (i=red; i<cnt; i += red)  {
         cx = yoff + yarr[i]*yzoom;
         cy = xoff + xinc*(float)i;
         djh_trafo( cx, cy, &dx, &dy );
         fprintf( djv_pf, "%d %d L\n", Nint(dx), Nint(dy) );
      } /*endfor*/
   } else {
      cx = xoff;
      cy = yoff + (*yarr)*yzoom;
      djh_trafo( cx, cy, &dx, &dy );
      fprintf( djv_pf, "%d %d M\n", Nint(dx), Nint(dy) );
      for  (i=red; i<cnt; i += red)  {
         cx = xoff + xinc*(float)i;
         cy = yoff + yarr[i]*yzoom;
         djh_trafo( cx, cy, &dx, &dy );
         fprintf( djv_pf, "%d %d L\n", Nint(dx), Nint(dy) );
      } /*endfor*/
   } /*endif*/
#endif

   if  (cnt == 0L)  return;
   /* allocate memory */
   pxy = (int *)sy_allocmem( (long)MAXPLOT, (int)sizeof(int)*2, status );
   if  (*status != DJE_NOERROR)  return;

   if  (red == 1)  {
      i = 0;
      while  (i < cnt)  {
         for  (j=0;j<MAXPLOT;j++)  {
            djh_trafo( xoff+xinc*(GBC_COO)i,
               yoff + yzoom * (*yarr++), pxy+2*j, pxy+2*j+1 );
            if  (++i == cnt)  {
               j++;
               break;
            } /*endif*/
         } /*endfor*/
         if  (i != cnt)  {i--; yarr--;}  /* connect lines */
			fprintf( djv_pf, "%d %d\n", ID_PLINE, (int)j );
			for  (k=0; k<j; k++)
				fprintf( djv_pf, "%d %d\n", pxy[2*k], pxy[2*k+1] );
			/* v_pline( djv_p, (int)j, pxy ); */
      } /*endwhile*/
   } else {
      i = 0;
      while  (i < cnt)  {
         for  (j=0;j<MAXPLOT;j++)  {
            djh_trafo( xoff+xinc*(GBC_COO)i,
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
			fprintf( djv_pf, "%d %d\n", ID_PLINE, (int)j );
			for  (k=0; k<j; k++)
				fprintf( djv_pf, "%d %d\n", pxy[2*k], pxy[2*k+1] );
         /* v_pline( djv_p, (int)j, pxy ); */
      } /*endwhile*/
   } /*endif*/

   sy_deallocmem( pxy );

   djv_move = FALSE;

} /* end of dj_arrayplot */



/*----------------------------------------------------------------------------*/



void dj_text( int style, GBC_COO x, GBC_COO y, char text[] )

/* prints text at position (x,y)
 *
 * parameters of routine
 * int        style;     input; style parameter (size)
 * GBC_COO    x, y;      input; text position (user coordinates)
 * char       text[];    input; text to be printed
 */
{
   /* local variables */
   DJOUT    dx, dy;   /* device coordinates */
   /* int      slen; */    /* length of string */
   /* float    size; */    /* size of output */
   STATUS   dmy;      /* scratch */

   /* executable code */

   if  (!djv_isinit)  dj_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );

	if  (style != djv_currcstyle)  djh_cstyle( style );
   djh_trafo( x, y, &dx, &dy );
	fprintf( djv_pf, "%d %d %d\n%s\n", ID_GTEXT, dx, dy, text );
	/* v_gtext( djv_p, dx, dy, text ); */

} /* end of dj_text */



/*----------------------------------------------------------------------------*/



void dj_setstyle( int style, char item[], char value[], STATUS *status )

/* sets style parameter number "style"
 *
 * parameters of routine
 * int        style;     input; number of style
 * char       item[];    input; name of style attribute
 * char       value[];   input; new value of style attribute (as string expr.)
 * STATUS     *status;   output; return status
 */
{
   /* local variables */
   float    num;      /* scratch */
	int      i;        /* scratch */
   /* float    r, g, b;*/  /* colours */

   /* executable code */

   if (style >= CSTYLES || style >= LSTYLES  ||  style < 0)  {
		*status = DJE_ILSTYLE;
		return;
	} /*endif*/

   /* if  (!djv_isinit)  dj_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status); */

   if  (strcmp(item,"CHARSIZE") == 0  ||  strcmp(item,"CHARHEIGHT") == 0)  {
      if  (sscanf(value,"%f",&num) != 1)  {
         *status = DJE_ILVALUE;
         return;
      } /*endif*/
      djv_cstyle[style].cheight = Nint( num*(float)djv_height );
	} else if  (strcmp(item,"WRMODE") == 0)  {
   } else if  (strcmp(item,"LINESTYLE") == 0)  {
		if  (sscanf(value,"%d",&i) != 1)  {
			*status = DJE_ILVALUE;
			return;
		} /*endif*/
	   djv_lstyle[style].lntype = i;
   } else if  (strcmp(item,"COLOR") == 0)  {
      /* not implemented */
   } else if  (strcmp(item,"CHARROT") == 0)  {
   } else if  (strcmp(item,"FONT") == 0)  {
		if  (sscanf(value,"%d",&i) != 1)  {
			*status = DJE_ILVALUE;
			return;
		} /*endif*/
   } else if  (strcmp(item,"LINEWIDTH") == 0)  {
      if  (sscanf(value,"%d",&i) != 1)  {
         *status = DJE_ILVALUE;
         return;
      } /*endif*/
		djv_lstyle[style].lnwidth = i;
	} else if  (strcmp(item,"LINEPATTERN") == 0)  {
	} else if  (strcmp(item,"CHARSIZE_ATARI") == 0)  {
		if  (sscanf(value,"%d",&i) != 1)  {
			*status = DJE_ILVALUE;
			return;
		} /*endif*/
		if  (i <= 4)  {
			djv_cstyle[style].cheight = 13;
		} else if  (i <= 6)  {
			djv_cstyle[style].cheight = 26;
		} else if  (i <= 13)  {
			djv_cstyle[style].cheight = 52;
		} else {
			djv_cstyle[style].cheight = 104;
		} /*endif*/
	} else if  (strcmp(item,"TEXTEFFECTS") == 0)  {
		if  (sscanf(value,"%d",&i) != 1)  {
			*status = DJE_ILVALUE;
			return;
		} /*endif*/
		djv_cstyle[style].ceffects = i;
   } else {
      *status = DJE_UKITEM;
      return;
   } /*endif*/

} /* end of dj_setstyle */



/*----------------------------------------------------------------------------*/



void dj_set_outputdir( char dir[], STATUS *status )

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name of scratch path
 * STATUS    *status;   output; return status
 */
{
	/* executable code */

	if  (strlen(dir) > BC_FILELTH)  {
		*status = DJE_STROVFL;
		return;
	} /*endif*/
	strcpy( djv_outputdir, dir );

} /* end of dj_set_outputdir */



/*----------------------------------------------------------------------------*/



void dj_prepare( STATUS *status )

/* prepares hardcopy
 *
 * parameters of routine
 * STATUS     *status;    output; return status
 */
{
   /* local variables */

   /* executable code */

	dj_erase();
	if  (Severe(status))  return;  /* just to use status */

} /* end of dj_prepare */



/*----------------------------------------------------------------------------*/



void dj_cleanup( char outf[], STATUS *status )

/* finishes hardcopy
 *
 * parameters of routine
 * char       outf[];     output; output filename
 * STATUS     *status;    output; return status
 */
{
   /* local variables */
   char     hcname[BC_LINELTH+1];    /* name of hardcopy file */
   char     str[BC_LINELTH+1];       /* scratch string */

   /* executable code */

   /* close DeskJet file */
   djh_closedjf( djv_pf );

   /* create hardcopy name */
   djh_create_hcname( hcname );

   strcpy( str, djv_outputdir );
   strcat( str, hcname );
	sy_fdelete( str );
   sy_frename( djv_currdjf, str );
	if  (outf != NULL)
		strcpy( outf, str );

   /* open DeskJet file */
   djv_pf = sy_fopen( djv_currdjf, "w" );
   if  (djv_pf == NULL)  {
      *status = DJE_FOPNWR;
      return;
   } /*endif*/
   djh_insertheader( djv_pf );

} /* end of dj_cleanup */



/*----------------------------------------------------------------------------*/



void dj_setpar( char item[], char value[], STATUS *status )

/* sets hardcopy parameters
 *
 * parameters of routine
 * char       item[];     input; item to be set
 * char       value[];    input; new value of item
 * STATUS     *status;    output; return status
 */
{
   /* executable code */

   if  (strlen(value) > BC_LINELTH)  {
      *status = DJE_STROVFL;
      return;
   } /*endif*/

   if  (strcmp(item,"PRINT_CMD") == 0)  {
      /* strcpy( djv_printfmt, value ); */
   } else {
      *status = DJE_UKHCITEM;
      return;
   } /*endif*/

} /* end of dj_setpar */



/*----------------------------------------------------------------------------*/



void dj_arrayswap( BOOLEAN on_off )

/* switches array-swap on ot off
 *
 * parameter of routine
 * BOOLEAN   on_off;     TRUE=on, FALSE=off
 */
{
   /* executable code */

   djv_arrayswap = on_off;

} /* end of dj_arrayswap */



/*----------------------------------------------------------------------------*/
/*                            local routines                                  */
/*----------------------------------------------------------------------------*/



static void djh_trafo( GBC_COO ux, GBC_COO uy, DJOUT *dx, DJOUT *dy )

/* transforms user coordinates (ux,uy) to device coordinates (dx,dy)
 *
 * parameters of routine
 * GBC_COO    ux, uy;     input; user coordinates
 * DJOUT      dx, dy;     output; device coordinates
 */
{
   /* executable code */

   *dx = (int)(ux*djv_trafo_xf - djv_trafo_xo);
   *dy = (int)(-uy*djv_trafo_yf - djv_trafo_yo);

} /* end of djh_trafo */



/*----------------------------------------------------------------------------*/



static void djh_create_hcname( char name[] )

/* creates hardcopy filename
 *
 * parameters of routine
 * char       name[];      output; filename created
 */
{
   /* local variables */
   static int   hc_count=0;   /* hardcopy counter */

   /* executable code */

   strcpy( name, "HC" );
   sprintf( name+2, "%03d", ++hc_count );
   strcat( name, ".vd" );

} /* end of djh_create_filename */



/*----------------------------------------------------------------------------*/



static void djh_insertheader( FILE *dj )

/* inserts header to DeskJet file
 *
 * parameter of routine
 * FILE      *dj;     input; DeskJet file to insert header
 */
{
	/* local variables */
  	FILE     *hf;                     /* header file */
	char     line[BC_LONGSTRLTH+1];   /* current line */
	char     timestr[BC_LINELTH+1];   /* time string */
#	ifdef BSD
	char     *tstr;
	struct timeval date;
	struct tm *p;
#	endif

	/* executable code */

#	ifdef BSD
	if  (time(&date) == -1)  {
		strcpy( timestr, "--- no system time available ---" );
	} else {
		gettimeofday( &date, NULL );
		p = localtime( &date.tv_sec );
		tstr = asctime( p );
		sprintf( timestr, "%.6s %.4s %.8s", tstr+4, tstr+20, tstr+11 );
	} /*endif*/
#	else
	strcpy( timestr, "xxx" );
#	endif

	/* fprintf( dj, "! HC-file\n" ); */

	hf = sy_fopen( DJHEADER, "r" );
	if  (hf == NULL)  {
		/* fprintf( dj, "!! no header file available\n" ); */
		/* printf( "-->  error opening header file\n" ); */
		return;
	} /*endif*/

	while  (fgets(line,BC_LONGSTRLTH,hf) != NULL)
		fprintf( dj, "%s", line );

	fclose( hf );

} /* end of djh_insertheader */



/*----------------------------------------------------------------------------*/



static void djh_closedjf( FILE *dj )

/* closes hardcopy file
 *
 * parameter of routine
 * FILE      *dj;      input; pointer to hardcopy file
 */
{

	/* executable code */

	/* fprintf( dj, "%% EndDocument\n" ); */
	fclose( dj );

} /* end of djh_closedjf */



/*------------------------------------------------------------------------*/



static void djh_lstyle( unsigned style )

/* sets the currently used line style block
 *
 * parameters of routine
 * unsigned int  style;      input; line style block number
 */
{
   /* executable code */

   if  (style >= LSTYLES)  return;
	if  (djv_pf == NULL)  return;
	fprintf( djv_pf, "%d %d\n", ID_WRMODE, djv_lstyle[style].wrmode );
	fprintf( djv_pf, "%d %d\n", ID_LTYPE, djv_lstyle[style].lntype );
	fprintf( djv_pf, "%d %d\n", ID_LUDSTY, djv_lstyle[style].lnpatt );
	fprintf( djv_pf, "%d %d\n", ID_LWIDTH, djv_lstyle[style].lnwidth );
	fprintf( djv_pf, "%d %d\n", ID_LCOLOR, djv_lstyle[style].lncolor );
	djv_currlstyle = style;

} /* end of djh_lstyle */



/*------------------------------------------------------------------------*/



static void djh_cstyle( unsigned style )

/* sets the currently used char style block
 *
 * parameters of routine
 * unsigned   style;     input; line style block number
 */
{
   /* executable code */

   if  (style >= CSTYLES)  return;
	if  (djv_pf == NULL)  return;
	fprintf( djv_pf, "%d %d\n", ID_WRMODE, djv_cstyle[style].wrmode );
	fprintf( djv_pf, "%d %d\n", ID_THEIGHT, djv_cstyle[style].cheight );
	fprintf( djv_pf, "%d %d\n", ID_TROTATION, djv_cstyle[style].angle );
	fprintf( djv_pf, "%d %d\n", ID_TFONT, djv_cstyle[style].cfont );
	fprintf( djv_pf, "%d %d\n", ID_TEFFECTS, djv_cstyle[style].ceffects );
	fprintf( djv_pf, "%d %d\n", ID_TCOLOR, djv_cstyle[style].ccolor );
	djv_currcstyle = style;

} /* end of djh_cstyle */



/*----------------------------------------------------------------------------*/

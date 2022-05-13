
/* file PSCH.C
 *      ======
 *
 * version 14, 5-Apr-2006
 *
 * PostScript channel for graphics output module
 * K. Stammler, 20-JUN-1991
 */

/* #define BSD */


#include <stdio.h>
#include <string.h>
#include BASECNST
#include BC_SYSBASE
#include "graphbas.h"
#include "psusrdef.h"
#include "pserrors.h"

#ifdef BSD
#include <sys/time.h>
#endif


/* global constants */
#define PSC_MAXSTYLE 10
	/* maximum number of different line styles */
#define DEFAULTWIDTH 100.0
	/* default width of user coo */
#define DEFAULTHEIGHT 100.0
	/* default height of user coo */
#define DEFAULTPLOTW 25.9
#define DEFAULTPLOTH 19.5
	/* default plot size */
#define MAXDEFSTYLE 6
	/* number of default styles */
#define DOTSPERCM 118.11 /*28.35*/
	/* resolution of printer */

#define PSFILENAME "HC_TMP"
	/* PostScript file */
#define PSHEADER "header.ps"
	/* default PostScript header file */


/* global variable */
static char psv_outputdir[BC_FILELTH+1];  /* output directory */
static char psv_inputdir[BC_FILELTH+1];   /* input directory */


/* global variables */
static BOOLEAN psv_isinit;    /* calcomp is initialised */
static PSOUT   psv_width;     /* width of display */
static PSOUT   psv_height;    /* heigth of display */
static PSOUT   psv_xoffset;   /* x-offset */
static PSOUT   psv_yoffset;   /* y-offset */
static float   psv_trafo_xf;  /* trafo x-factor */
static float   psv_trafo_yf;  /* trafo y-factor */
static PSOUT   psv_trafo_xo;  /* x-offset */
static PSOUT   psv_trafo_yo;  /* y-offset */
static BOOLEAN psv_arrayswap=FALSE;      /* swap arrayplot coo's */
static int     psv_currstyle; /* currently used style */
static float   psv_csize[PSC_MAXSTYLE];  /* character sizes (units of height) */
static float   psv_lwidth[PSC_MAXSTYLE]; /* line widths */
static FILE    *psv_pf;       /* pointer to PostScript file */
static BOOLEAN psv_move=FALSE;/* moveto buffer is not empty */
static GBC_COO psv_move_x;    /* moveto x-coo */
static GBC_COO psv_move_y;    /* moveto y-coo */

static char    psv_currpsf[BC_FILELTH+1];   /* name of active PS file */
static int     psv_strokenum=5000; /* max number of lineto's between stroke's */
static char    psv_headerfile[BC_FILELTH+1]=PSHEADER; /* PS header file */
#ifdef XXX
static char    psv_printfmt[BC_LINELTH+1]=  /* print command */
					{"lpr -P lj %s"};
#endif

/* prototypes of local routines */
void psh_trafo( GBC_COO ux, GBC_COO uy, PSOUT *dx, PSOUT *dy );
void psh_create_hcname( char name[] );
void psh_insertheader( FILE *ps );
void psh_closepsf( FILE *ps );
void psh_changestyle( int style );

/*----------------------------------------------------------------------------*/



void ps_init( int attribs, PSOUT xlo, PSOUT ylo, PSOUT width, PSOUT height,
	STATUS *status )

/* initialises PostScript channel
 *
 * parameters of routine
 * int        attribs;        input; attributes
 * PSOUT      xlo, ylo;       input; offset of output
 * PSOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	int      i;      /* counter */
	FILE     *fp;    /* file pointer */

	/* executable code */

	if  (attribs == 0)  {
		;                  /* just to access attrib once */
	} /*endif*/

	xlo *= DOTSPERCM; ylo *= DOTSPERCM; width *= DOTSPERCM; height *= DOTSPERCM;

	if  (*psv_currpsf == '\0')  {
		if  (strlen(psv_currpsf)+strlen(PSFILENAME)+8 > BC_FILELTH)  {
			*status = PSE_STROVFL;
			return;
		} /*endif*/
		/* search for existing files */
		i = 0;
		FOREVER  {
			sprintf( psv_currpsf, "%s%s%05d.ps", psv_outputdir, PSFILENAME, i );
			fp = fopen( psv_currpsf, "r" );
			if  (fp == NULL)  break;
			fclose( fp );
			i++;
		} /*endfor*/
	} /*endif*/

	if  ((width <= 0.0) || (height <= 0.0))  {
		*status = PSE_ILPAR;
		return;
	} /*endif*/

	if  (psv_isinit)  ps_exit();

	psv_xoffset = xlo;
	psv_yoffset = ylo;
	psv_width = width;
	psv_height = height;

	psv_currstyle = 0;
	for  (i=0; i<PSC_MAXSTYLE; i++)
		if  (psv_csize[i] == 0.0)
			psv_csize[i] = 40.0;
	for  (i=0; i<PSC_MAXSTYLE; i++)
		if  (psv_lwidth[i] == 0.0)
			psv_lwidth[i] = 0.4;

	/* open PostScript file */
	psv_pf = sy_fopen( psv_currpsf, "w" );
	if  (psv_pf == NULL)  {
		*status = PSE_FOPNWR;
		return;
	} /*endif*/
	psv_isinit = TRUE;
	psv_move = FALSE;

	psh_insertheader( psv_pf );
	ps_setcoo( 0., 0., DEFAULTWIDTH, DEFAULTHEIGHT, status );

} /* end of ps_init */



/*----------------------------------------------------------------------------*/



void ps_exit( void )

/* exits PostScript channel
 *
 * no parameters
 */
{
	/* executable code */

	if  (!psv_isinit)  return;
	fclose( psv_pf );
	sy_fdelete( psv_currpsf );
	psv_isinit = FALSE;

} /* end of ps_exit */



/*----------------------------------------------------------------------------*/



void ps_resize( PSOUT xlo, PSOUT ylo, PSOUT width, PSOUT height,
	STATUS *status )

/* resizes output of calcomp
 *
 * parameters of routine
 * PSOUT      xlo, ylo;       input; offset of output
 * PSOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */
{
	/* executable code */

	xlo *= DOTSPERCM; ylo *= DOTSPERCM; width *= DOTSPERCM; height *= DOTSPERCM;

	if  ((width <= 0.0) || (height <= 0.0))  {
		*status = PSE_ILPAR;
		return;
	} /*endif*/

	if  (!psv_isinit)  ps_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	psv_xoffset = xlo;
	psv_yoffset = ylo;
	psv_width = width;
	psv_height = height;

} /* end of ps_resize */



/*----------------------------------------------------------------------------*/



void ps_erase( void )

/* clears calcomp output channel
 *
 * no parameters
 */
{
	/* local variables */
	int      dmy=0;

	/* executable code */

	if  (psv_isinit)  {
		fseek( psv_pf, 0L, 0 );
		psh_insertheader( psv_pf );
		psv_currstyle = 0;
	} else {
		ps_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );
	} /*endif*/
	psv_move = FALSE;

} /* end of ps_erase */



/*----------------------------------------------------------------------------*/



void ps_setcoo( GBC_COO x, GBC_COO y, GBC_COO w, GBC_COO h, STATUS *status )

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
		*status = PSE_ILPAR;
		return;
	} /*endif*/

	if  (!psv_isinit)  ps_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	psv_trafo_xf = psv_width / w;
	psv_trafo_yf = psv_height / h;
	psv_trafo_xo = psv_xoffset - x*psv_trafo_xf;
	psv_trafo_yo = psv_yoffset - y*psv_trafo_yf;

} /* end of ps_setcoo */



/*----------------------------------------------------------------------------*/



void ps_moveto( GBC_COO x, GBC_COO y )

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * GBC_COO    x, y;    input; position
 */
{
	/* local variables */
	STATUS   dmy=0;    /* scratch */

	/* executable code */

	if  (!psv_isinit)  ps_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );

	psv_move_x = x;
	psv_move_y = y;
	psv_move = TRUE;

} /* end of ps_moveto */



/*----------------------------------------------------------------------------*/



void ps_drawto( int style, GBC_COO x, GBC_COO y )

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        style;   input; style parameter
 * GBC_COO    x, y;    input; position
 */
{
	/* local variables */
	static int linecnt=0; /* counter of lineto commands */
	PSOUT    dx, dy;      /* device coordinates */
	STATUS   dmy=0;       /* scratch */

	/* executable code */

	if  (!psv_isinit)  ps_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );

	if  ((style < 0) || (style >= PSC_MAXSTYLE))  style = 0;
	if  (style != psv_currstyle)
		psh_changestyle( style );

	if  (psv_move)  {
		psh_trafo( psv_move_x, psv_move_y, &dx, &dy );
		fprintf( psv_pf, "%d %d M\n", Nint(dx), Nint(dy) );
		psv_move = FALSE;
		linecnt = 0;
	} /*endif*/

	psh_trafo( x, y, &dx, &dy );
	fprintf( psv_pf, "%d %d L\n", Nint(dx), Nint(dy) );

	if  (++linecnt > psv_strokenum)  {
		fprintf( psv_pf, "stroke\n" );
		fprintf( psv_pf, "%d %d M\n", Nint(dx), Nint(dy) );
		linecnt = 0;
	} /*endif*/

} /* end of ps_drawto */


/*----------------------------------------------------------------------------*/



void ps_arrayplot( int style, long cnt, int red, GBC_COO xoff,
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
	/* local variables */
	long     i;          /* sample counter */
	GBC_COO  cx, cy;     /* current user coordinates */
	PSOUT    dx, dy;     /* current PS coordinates */
	long     lcnt;       /* lineto counter */

	/* executable code */

	if  (!psv_isinit)  ps_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	if  (style != psv_currstyle)
		psh_changestyle( style );

	fprintf( psv_pf, "newpath\n" );

	lcnt = 0;
	if  (psv_arrayswap)  {
		cx = yoff + (*yarr)*yzoom;
		cy = xoff;
		psh_trafo( cx, cy, &dx, &dy );
		fprintf( psv_pf, "%d %d M\n", Nint(dx), Nint(dy) );
		for  (i=red; i<cnt; i += red)  {
			cx = yoff + yarr[i]*yzoom;
			cy = xoff + xinc*(float)i;
			psh_trafo( cx, cy, &dx, &dy );
			fprintf( psv_pf, "%d %d L\n", Nint(dx), Nint(dy) );
			if  (++lcnt > psv_strokenum)  {
				fprintf( psv_pf, "stroke\n" );
				fprintf( psv_pf, "%d %d M\n", Nint(dx), Nint(dy) );
				lcnt = 0;
			} /*endif*/
		} /*endfor*/
	} else {
		cx = xoff;
		cy = yoff + (*yarr)*yzoom;
		psh_trafo( cx, cy, &dx, &dy );
		fprintf( psv_pf, "%d %d M\n", Nint(dx), Nint(dy) );
		for  (i=red; i<cnt; i += red)  {
			cx = xoff + xinc*(float)i;
			cy = yoff + yarr[i]*yzoom;
			psh_trafo( cx, cy, &dx, &dy );
			fprintf( psv_pf, "%d %d L\n", Nint(dx), Nint(dy) );
			if  (++lcnt > psv_strokenum)  {
				fprintf( psv_pf, "stroke\n" );
				fprintf( psv_pf, "%d %d M\n", Nint(dx), Nint(dy) );
				lcnt = 0;
			} /*endif*/
		} /*endfor*/
	} /*endif*/

	fprintf( psv_pf, "stroke\n" );

	psv_move = FALSE;

} /* end of ps_arrayplot */



/*----------------------------------------------------------------------------*/



void ps_text( int style, GBC_COO x, GBC_COO y, char text[] )

/* prints text at position (x,y)
 *
 * parameters of routine
 * int        style;     input; style parameter (size)
 * GBC_COO    x, y;      input; text position (user coordinates)
 * char       text[];    input; text to be printed
 */
{
	/* local variables */
	PSOUT    dx, dy;   /* device coordinates */
	/* int      slen; */    /* length of string */
	/* float    size; */    /* size of output */
	STATUS   dmy;      /* scratch */

	/* executable code */

	if  (!psv_isinit)  ps_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );

	if  (style < 0)  style = 0;
	if  (style >= PSC_MAXSTYLE)  style = PSC_MAXSTYLE-1;
	psh_trafo( x, y, &dx, &dy );
	/* slen = (int)strlen( text ); */
	/* size = psv_csize[style] * psv_height; */
	if  (style != psv_currstyle)
		psh_changestyle( style );
	fprintf( psv_pf, "%d %d M\n(%s)\ndrawstr\n", Nint(dx), Nint(dy), text );

} /* end of ps_text */



/*----------------------------------------------------------------------------*/



void ps_setstyle( int style, char item[], char value[], STATUS *status )

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
	float    r, g, b;  /* colours */

	/* executable code */

	if  ((style < 0) || (style >= PSC_MAXSTYLE))  {
		*status = PSE_ILSTYLE;
		return;
	} /*endif*/

	/* if  (!psv_isinit)  ps_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status); */

	if  (strcmp(item,"CHARSIZE") == 0  ||  strcmp(item,"CHARHEIGHT") == 0)  {
		if  (sscanf(value,"%f",&num) != 1)  {
			*status = PSE_ILVALUE;
			return;
		} /*endif*/
		psv_csize[style] = num*3000.0;
	} else if  (strcmp(item,"WRMODE") == 0)  {
	} else if  (strcmp(item,"LINESTYLE") == 0)  {
		/* not yet implemented */
	} else if  (strcmp(item,"COLOR") == 0)  {
		if  (sscanf(value,"%f,%f,%f",&r,&g,&b) != 3)  {
			*status = PSE_ILVALUE;
			return;
		} /*endif*/
		/* not yet implemented */
	} else if  (strcmp(item,"CHARROT") == 0)  {
	} else if  (strcmp(item,"FONT") == 0)  {
	} else if  (strcmp(item,"LINEWIDTH") == 0)  {
		if  (sscanf(value,"%f",&num) != 1)  {
			*status = PSE_ILVALUE;
			return;
		} /*endif*/
		psv_lwidth[style] = num*1.8;
	} else if  (strcmp(item,"LINEPATTERN") == 0)  {
	} else if  (strcmp(item,"CHARSIZE_ATARI") == 0)  {
	} else if  (strcmp(item,"TEXTEFFECTS") == 0)  {
	} else {
		*status = PSE_UKITEM;
		return;
	} /*endif*/

} /* end of ps_setstyle */



/*----------------------------------------------------------------------------*/



void ps_set_outputdir( char dir[], STATUS *status )

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name of scratch path
 * STATUS    *status;   output; return status
 */
{
	/* executable code */

	if  (strlen(dir) > BC_FILELTH)  {
		*status = PSE_STROVFL;
		return;
	} /*endif*/
	strcpy( psv_outputdir, dir );

} /* end of ps_set_outputdir */



/*----------------------------------------------------------------------------*/



void ps_set_inputdir( char dir[], STATUS *status )

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name
 * STATUS    *status;   output; return status
 */
{
	/* executable code */

	if  (strlen(dir) > BC_FILELTH)  {
		*status = PSE_STROVFL;
		return;
	} /*endif*/
	strcpy( psv_inputdir, dir );

} /* end of ps_set_inputdir */



/*----------------------------------------------------------------------------*/



void ps_prepare( STATUS *status )

/* prepares hardcopy
 *
 * parameters of routine
 * STATUS     *status;    output; return status
 */
{
	/* local variables */

	/* executable code */

	ps_erase();
	if  (Severe(status))  return;  /* just to use status */

} /* end of ps_prepare */



/*----------------------------------------------------------------------------*/



void ps_cleanup( char outf[], STATUS *status )

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
	/* char     cmd[BC_LINELTH+1]; */      /* command string */

	/* executable code */

	if  (!psv_isinit)  ps_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	/* close PostScript file */
	psh_closepsf( psv_pf );

	/* create hardcopy name */
	psh_create_hcname( hcname );

	strcpy( str, psv_outputdir );
	strcat( str, hcname );
	sy_fdelete( str );
	sy_frename( psv_currpsf, str );
	if  (outf != NULL)  strcpy( outf, str );

#   ifdef XXX
	/* send it to the printer */
	if  (*psv_printfmt != '\0')  {
		sprintf( cmd, psv_printfmt, str );
		sy_system( cmd, status );
	} /*endif*/
#   endif

	/* open PostScript file */
	psv_pf = sy_fopen( psv_currpsf, "w" );
	if  (psv_pf == NULL)  {
		*status = PSE_FOPNWR;
		return;
	} /*endif*/
	psh_insertheader( psv_pf );
	psv_currstyle = 0;    /* reset style block number */

} /* end of ps_cleanup */



/*----------------------------------------------------------------------------*/



void ps_setpar( char item[], char value[], STATUS *status )

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
		*status = PSE_STROVFL;
		return;
	} /*endif*/

	if  (strcmp(item,"PRINT_CMD") == 0)  {
		/* strcpy( psv_printfmt, value ) */;
	} else if  (strcmp(item,"STROKE") == 0)  {
		sscanf( value, "%d", &psv_strokenum );
	} else if  (strcmp(item,"PSHEADER") == 0)  {
		if  (strlen(value) < BC_FILELTH)
			strcpy( psv_headerfile, value );
	} else {
		*status = PSE_UKHCITEM;
		return;
	} /*endif*/

} /* end of ps_setpar */



/*----------------------------------------------------------------------------*/



void ps_arrayswap( BOOLEAN on_off )

/* switches array-swap on ot off
 *
 * parameter of routine
 * BOOLEAN   on_off;     TRUE=on, FALSE=off
 */
{
	/* executable code */

	psv_arrayswap = on_off;

} /* end of ps_arrayswap */



/*----------------------------------------------------------------------------*/
/*                            local routines                                  */
/*----------------------------------------------------------------------------*/



void psh_trafo( GBC_COO ux, GBC_COO uy, PSOUT *dx, PSOUT *dy )

/* transforms user coordinates (ux,uy) to device coordinates (dx,dy)
 *
 * parameters of routine
 * GBC_COO    ux, uy;     input; user coordinates
 * PSOUT      dx, dy;     output; device coordinates
 */
{
	/* executable code */

	*dx = ux * psv_trafo_xf + psv_trafo_xo;
	*dy = uy * psv_trafo_yf + psv_trafo_yo;

} /* end of psh_trafo */



/*----------------------------------------------------------------------------*/



void psh_create_hcname( char name[] )

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
	strcat( name, ".ps" );

} /* end of psh_create_filename */



/*----------------------------------------------------------------------------*/


void psh_insertheader( FILE *ps )

/* inserts header to PostScript file
 *
 * parameter of routine
 * FILE      *ps;     input; PostScript file to insert header
 */
{
	/* local variables */
	FILE     *hf;                     /* header file */
	char     line[BC_LONGSTRLTH+1];   /* current line */
	char     timestr[BC_LINELTH+1];   /* time string */
#   ifdef BSD
	char     *tstr;
	struct timeval date;
	struct tm *p;
#   endif

	/* executable code */

#   ifdef BSD
	if  (time(&date) == -1)  {
		strcpy( timestr, "--- no system time available ---" );
	} else {
		gettimeofday( &date, NULL );
		p = localtime( &date.tv_sec );
		tstr = asctime( p );
		sprintf( timestr, "%.6s %.4s %.8s", tstr+4, tstr+20, tstr+11 );
	} /*endif*/
#   else
		strcpy( timestr, "                                              " );
#   endif

	fprintf( ps, "%%! PS-Adobe-2.0 EPSF -1.2\n" );
	fprintf( ps, "%%%% Creator: SH (SeismicHandler)\n" );
	fprintf( ps, "%%%% For: Hardcopy of graphic window\n" );
	fprintf( ps, "%%%% Title: SH Hardcopy\n" );
	fprintf( ps, "%%%% Creation Date: %s\n", timestr );
	fprintf( ps, "%%%% Bounding Box: \n" );
	fprintf( ps, "%%%% EndComments\n" );
	fprintf( ps, "%%%% \n" );
	fprintf( ps, "%%%% BeginDictSet\n" );

	strcpy( line, psv_inputdir );
	strcat( line, psv_headerfile );
	hf = sy_fopen( line, "r" );
	if  (hf == NULL)  {
		fprintf( ps, "%%%%\n%%%% no header file available\n%%%%\n" );
		printf( "-->  error opening header file\n" );
		return;
	} /*endif*/

	while  (fgets(line,BC_LONGSTRLTH,hf) != NULL)
		fputs( line, ps );

	fclose( hf );
	fprintf( ps, "%%%% EndDictSet\n" );
	fprintf( ps, "\n%%%% BeginDocument\n" );
	fprintf( ps, "newpath\n" );

} /* end of psh_insertheader */



/*----------------------------------------------------------------------------*/


void psh_closepsf( FILE *ps )

/* closes hardcopy file
 *
 * parameter of routine
 * FILE      *ps;      input; pointer to hardcopy file
 */
{

	/* executable code */

	fprintf( ps, "stroke\n" );
	fprintf( ps, "endpage\n" );
	fprintf( ps, "%% EndDocument\n" );
	fclose( ps );

} /* end of psh_closepsf */



/*----------------------------------------------------------------------------*/


void psh_changestyle( int style )

/* changes the current style
 *
 * parameters of routine
 * int        style;      input; new style number
 */
{
	/* executable code */

	if  (style < 0)  style = 0;
	if  (style >= PSC_MAXSTYLE)  style = 0;
	psv_currstyle = style;
	fprintf( psv_pf, "stroke\n" );
	fprintf( psv_pf, "newpath\n" );
	fprintf( psv_pf, "%f setlinewidth\n", psv_lwidth[style] );
	fprintf( psv_pf, "/Courier findfont %f scalefont setfont\n",
		psv_csize[style] );

} /* end of psh_changestyle */



/*----------------------------------------------------------------------------*/

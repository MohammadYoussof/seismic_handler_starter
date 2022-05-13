
/* file HPCH.C
 *      ======
 *
 * version 7, 21-Dec-92
 *
 * HPGL channel for graphics output module
 * K. Stammler, 25-Sep-1992
 */

#include <stdio.h>
#include <string.h>
#include BASECNST
#include BC_SYSBASE
#include BC_ASCII
#include "graphbas.h"
#include "hpusrdef.h"
#include "hperrors.h"


/* global constants */
#define HPC_MAXSTYLE 10
	/* maximum number of different line styles */
#define DEFAULTWIDTH 100.0
	/* default width of user coo */
#define DEFAULTHEIGHT 100.0
	/* default height of user coo */
#define DEFAULTPLOTW 25.9
#define DEFAULTPLOTH 19.5
	/* default plot size */
#define DOTSPERCM 398.83
	/* pixel units of printer (no scaling, 1021 dpi) */

#define HPFILENAME "HC.HP"
	/* HPGL file */
#define HPHEADER "HEADER.HP"
	/* HPGL header file */


/* global variable */
static char hpv_outputdir[BC_FILELTH+1];  /* output directory */
static char hpv_inputdir[BC_FILELTH+1];   /* input directory */


/* global variables */
static BOOLEAN hpv_isinit;      /* HP module is initialised */
static HPOUT   hpv_width;       /* width of display */
static HPOUT   hpv_height;      /* heigth of display */
static HPOUT   hpv_xoffset;     /* x-offset */
static HPOUT   hpv_yoffset;     /* y-offset */
static float   hpv_trafo_xf;    /* trafo x-factor */
static float   hpv_trafo_yf;    /* trafo y-factor */
static HPOUT   hpv_trafo_xo;    /* x-offset */
static HPOUT   hpv_trafo_yo;    /* y-offset */
static BOOLEAN hpv_arrayswap=FALSE;      /* swap arrayplot coo's */
static int     hpv_currstyle;   /* currently used style */
static float   hpv_csize[HPC_MAXSTYLE];  /* character sizes (units of height) */
static int     hpv_lwidth[HPC_MAXSTYLE]; /* line widths */
static FILE    *hpv_pf;         /* pointer to PostScript file */
static int     hpv_drawcnt=0;   /* points drawn */
static int     hpv_maxdrawcnt=10;  /* maximum number of points per line */
static char    hpv_labterm=ETX; /* label termination character */

static char    hpv_currhpf[BC_FILELTH+1];   /* name of active HPGL file */


/* prototypes of local routines */
void hph_trafo( GBC_COO ux, GBC_COO uy, HPOUT *dx, HPOUT *dy );
void hph_create_hcname( char name[] );
void hph_insertheader( FILE *hp, STATUS *status );
void hph_closehpf( FILE *hp );
void hph_changestyle( int style );

/*----------------------------------------------------------------------------*/



void hp_init( int attribs, float xlo, float ylo, float width, float height,
	STATUS *status )

/* initialises HPGL channel
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

	xlo *= DOTSPERCM; ylo *= DOTSPERCM; width *= DOTSPERCM; height *= DOTSPERCM;

	if  (*hpv_currhpf == '\0')  {
		strcpy( hpv_currhpf, hpv_outputdir );
		strcat( hpv_currhpf, HPFILENAME );
	} /*endif*/

	if  ((width <= 0.0) || (height <= 0.0))  {
		*status = HPE_ILPAR;
		return;
	} /*endif*/

	if  (hpv_isinit)  hp_exit();

	hpv_xoffset = xlo;
	hpv_yoffset = ylo;
	hpv_width = width;
	hpv_height = height;

	hpv_currstyle = 0;
	for  (i=0; i<HPC_MAXSTYLE; i++)
		if  (hpv_csize[i] == 0.0)
			hpv_csize[i] = 0.5;
	for  (i=0; i<HPC_MAXSTYLE; i++)
		if  (hpv_lwidth[i] == 0)
			hpv_lwidth[i] = 1;

	/* open HPGL file */
	hpv_pf = sy_fopen( hpv_currhpf, "w" );
	if  (hpv_pf == NULL)  {
		*status = HPE_FOPNWR;
		return;
	} /*endif*/
	hpv_isinit = TRUE;
	hpv_drawcnt = 0;

	hph_insertheader( hpv_pf, status );
	if  (Severe(status))  return;
	hp_setcoo( 0., 0., DEFAULTWIDTH, DEFAULTHEIGHT, status );

} /* end of hp_init */



/*----------------------------------------------------------------------------*/



void hp_exit( void )

/* exits HPGL channel
 *
 * no parameters
 */
{
	/* executable code */

	if  (!hpv_isinit)  return;
	fclose( hpv_pf );
	sy_fdelete( hpv_currhpf );
	hpv_isinit = FALSE;

} /* end of hp_exit */



/*----------------------------------------------------------------------------*/



void hp_resize( float xlo, float ylo, float width, float height,
	STATUS *status )

/* resizes output of calcomp
 *
 * parameters of routine
 * HPOUT      xlo, ylo;       input; offset of output
 * HPOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */
{
	/* executable code */

	xlo *= DOTSPERCM; ylo *= DOTSPERCM; width *= DOTSPERCM; height *= DOTSPERCM;

	if  ((width <= 0.0) || (height <= 0.0))  {
		*status = HPE_ILPAR;
		return;
	} /*endif*/

	if  (!hpv_isinit)  hp_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	hpv_xoffset = xlo;
	hpv_yoffset = ylo;
	hpv_width = width;
	hpv_height = height;

} /* end of hp_resize */



/*----------------------------------------------------------------------------*/



void hp_erase( void )

/* clears calcomp output channel
 *
 * no parameters
 */
{
	/* local variables */
	STATUS   locstat = BC_NOERROR;

	/* executable code */

	if  (hpv_isinit)  {
		fseek( hpv_pf, 0L, 0 );
		hph_insertheader( hpv_pf, &locstat );
	} else {
		hp_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &locstat );
	} /*endif*/
	hpv_drawcnt = 0;

} /* end of hp_erase */



/*----------------------------------------------------------------------------*/



void hp_setcoo( GBC_COO x, GBC_COO y, GBC_COO w, GBC_COO h, STATUS *status )

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
		*status = HPE_ILPAR;
		return;
	} /*endif*/

	if  (!hpv_isinit)  hp_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	hpv_trafo_xf = hpv_width / w;
	hpv_trafo_yf = hpv_height / h;
	hpv_trafo_xo = hpv_xoffset - x*hpv_trafo_xf;
	hpv_trafo_yo = hpv_yoffset - y*hpv_trafo_yf;

} /* end of hp_setcoo */



/*----------------------------------------------------------------------------*/



void hp_moveto( GBC_COO x, GBC_COO y )

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * GBC_COO    x, y;    input; position
 */
{
	/* local variables */
	HPOUT    dx, dy;   /* device coordinates */
	STATUS   dmy=0;    /* scratch */

	/* executable code */

	if  (!hpv_isinit)  hp_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );

	if  (hpv_drawcnt > 0)  {
		fprintf( hpv_pf, ";\n" );
		hpv_drawcnt = 0;
	} /*endif*/
	hph_trafo( x, y, &dx, &dy );
	fprintf( hpv_pf, "PU %d,%d;\n", Nint(dx), Nint(dy) );
	hpv_drawcnt = 0;

} /* end of hp_moveto */



/*----------------------------------------------------------------------------*/



void hp_drawto( int style, GBC_COO x, GBC_COO y )

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        style;   input; style parameter
 * GBC_COO    x, y;    input; position
 */
{
	/* local variables */
	HPOUT    dx, dy;   /* device coordinates */
	STATUS   dmy=0;    /* scratch */

	/* executable code */

	if  (!hpv_isinit)  hp_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );

	if  ((style < 0) || (style >= HPC_MAXSTYLE))  style = 0;
	if  (style != hpv_currstyle)
		hph_changestyle( style );

	hph_trafo( x, y, &dx, &dy );
	if  (hpv_drawcnt > hpv_maxdrawcnt)  {
		fprintf( hpv_pf, ";\n" );
		hpv_drawcnt = 0;
	} /*endif*/
	if  (hpv_drawcnt == 0)  {
		fprintf( hpv_pf, "PD %d,%d", Nint(dx), Nint(dy) );
	} else {
		fprintf( hpv_pf, ",%d,%d", Nint(dx), Nint(dy) );
	} /*endif*/
	hpv_drawcnt++;

} /* end of hp_drawto */


/*----------------------------------------------------------------------------*/



void hp_arrayplot( int style, long cnt, int red, GBC_COO xoff,
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
	long     i, j;       /* sample counter */
	GBC_COO  cx, cy;     /* current user coordinates */
	HPOUT    dx, dy;     /* current HPGL coordinates */

	/* executable code */

	if  (!hpv_isinit)  hp_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	if  (hpv_drawcnt > 0)  {
		fprintf( hpv_pf, ";\n" );
		hpv_drawcnt = 0;
	} /*endif*/

	if  (style != hpv_currstyle)
		hph_changestyle( style );

	if  (hpv_arrayswap)  {
		cx = yoff + (*yarr)*yzoom;
		cy = xoff;
		hph_trafo( cx, cy, &dx, &dy );
		fprintf( hpv_pf, "PU %d,%d;\n", Nint(dx), Nint(dy) );
		for  (i=red; i<cnt; i += red)  {
			cx = yoff + yarr[i]*yzoom;
			cy = xoff + xinc*(float)i;
			hph_trafo( cx, cy, &dx, &dy );
			fprintf( hpv_pf, "PD %d,%d;\n", Nint(dx), Nint(dy) );
		} /*endfor*/
	} else {
		cx = xoff;
		cy = yoff + (*yarr)*yzoom;
		hph_trafo( cx, cy, &dx, &dy );
		fprintf( hpv_pf, "PU %d,%d;\n", Nint(dx), Nint(dy) );
		j = 0;
		for  (i=red; i<cnt; i += red)  {
			cx = xoff + xinc*(float)i;
			cy = yoff + yarr[i]*yzoom;
			hph_trafo( cx, cy, &dx, &dy );
			if  (i == red)  {
				fprintf( hpv_pf, "PD %d,%d", Nint(dx), Nint(dy) );
			} else if  (j == hpv_maxdrawcnt)  {
				fprintf( hpv_pf, ";\nPD %d,%d", Nint(dx), Nint(dy) );
				j = 0;
			} else {
				fprintf( hpv_pf, ",%d,%d", Nint(dx), Nint(dy) );
			} /*endif*/
			j++;
		} /*endfor*/
		fprintf( hpv_pf, ";\n" );
	} /*endif*/

	hpv_drawcnt = 0;

} /* end of hp_arrayplot */



/*----------------------------------------------------------------------------*/



void hp_text( int style, GBC_COO x, GBC_COO y, char text[] )

/* prints text at position (x,y)
 *
 * parameters of routine
 * int        style;     input; style parameter (size)
 * GBC_COO    x, y;      input; text position (user coordinates)
 * char       text[];    input; text to be printed
 */
{
	/* local variables */
	HPOUT    dx, dy;   /* device coordinates */
	STATUS   dmy;      /* scratch */

	/* executable code */

	if  (!hpv_isinit)  hp_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );
	if  (hpv_drawcnt > 0)  {
		fprintf( hpv_pf, ";\n" );
		hpv_drawcnt = 0;
	} /*endif*/

	if  (style < 0)  style = 0;
	if  (style >= HPC_MAXSTYLE)  style = HPC_MAXSTYLE-1;
	hph_trafo( x, y, &dx, &dy );
	if  (style != hpv_currstyle)
		hph_changestyle( style );
	fprintf( hpv_pf, "PU %d,%d;\n", Nint(dx), Nint(dy) );
	fprintf( hpv_pf, "LB%s%c\n", text, hpv_labterm );

} /* end of hp_text */



/*----------------------------------------------------------------------------*/



void hp_setstyle( int style, char item[], char value[], STATUS *status )

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

	if  ((style < 0) || (style >= HPC_MAXSTYLE))  {
		*status = HPE_ILSTYLE;
		return;
	} /*endif*/

	/* if  (!hpv_isinit)  hp_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status); */

	if  (strcmp(item,"CHARSIZE") == 0  ||  strcmp(item,"CHARHEIGHT") == 0)  {
		if  (sscanf(value,"%f",&num) != 1)  {
			*status = HPE_ILVALUE;
			return;
		} /*endif*/
		hpv_csize[style] = (float)hpv_height*num/DOTSPERCM;
	} else if  (strcmp(item,"WRMODE") == 0)  {
	} else if  (strcmp(item,"LINESTYLE") == 0)  {
		/* not yet implemented */
	} else if  (strcmp(item,"COLOR") == 0)  {
		if  (sscanf(value,"%f,%f,%f",&r,&g,&b) != 3)  {
			*status = HPE_ILVALUE;
			return;
		} /*endif*/
		/* not yet implemented */
	} else if  (strcmp(item,"CHARROT") == 0)  {
	} else if  (strcmp(item,"FONT") == 0)  {
	} else if  (strcmp(item,"LINEWIDTH") == 0)  {
		if  (sscanf(value,"%f",&num) != 1)  {
			*status = HPE_ILVALUE;
			return;
		} /*endif*/
		hpv_lwidth[style] = Nint(num);
	} else if  (strcmp(item,"LINEPATTERN") == 0)  {
	} else if  (strcmp(item,"CHARSIZE_ATARI") == 0)  {
	} else if  (strcmp(item,"TEXTEFFECTS") == 0)  {
	} else {
		*status = HPE_UKITEM;
		return;
	} /*endif*/

} /* end of hp_setstyle */



/*----------------------------------------------------------------------------*/



void hp_set_outputdir( char dir[], STATUS *status )

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name of scratch path
 * STATUS    *status;   output; return status
 */
{
	/* executable code */

	if  (strlen(dir) > BC_FILELTH)  {
		*status = HPE_STROVFL;
		return;
	} /*endif*/
	strcpy( hpv_outputdir, dir );

} /* end of hp_set_outputdir */



/*----------------------------------------------------------------------------*/



void hp_set_inputdir( char dir[], STATUS *status )

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name
 * STATUS    *status;   output; return status
 */
{
	/* executable code */

	if  (strlen(dir) > BC_FILELTH)  {
		*status = HPE_STROVFL;
		return;
	} /*endif*/
	strcpy( hpv_inputdir, dir );

} /* end of hp_set_inputdir */



/*----------------------------------------------------------------------------*/



void hp_prepare( STATUS *status )

/* prepares hardcopy
 *
 * parameters of routine
 * STATUS     *status;    output; return status
 */
{
	/* local variables */

	/* executable code */

	hp_erase();
	if  (Severe(status))  return;  /* just to use status */

} /* end of hp_prepare */



/*----------------------------------------------------------------------------*/



void hp_cleanup( char outf[], STATUS *status )

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

	if  (!hpv_isinit)  {
		*status = HPE_NOTINIT;
		return;
	} /*endif*/
	if  (hpv_drawcnt > 0)  {
		fprintf( hpv_pf, ";\n" );
		hpv_drawcnt = 0;
	} /*endif*/

	/* close HPGL file */
	hph_closehpf( hpv_pf );

	/* create hardcopy name */
	hph_create_hcname( hcname );

	strcpy( str, hpv_outputdir );
	strcat( str, hcname );
	sy_fdelete( str );
	sy_frename( hpv_currhpf, str );
	if  (outf != NULL)  strcpy( outf, str );

	/* open new HPGL file */
	hpv_pf = sy_fopen( hpv_currhpf, "w" );
	if  (hpv_pf == NULL)  {
		*status = HPE_FOPNWR;
		return;
	} /*endif*/
	hph_insertheader( hpv_pf, status );

} /* end of hp_cleanup */



/*----------------------------------------------------------------------------*/



void hp_setpar( char item[], char value[], STATUS *status )

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
		*status = HPE_STROVFL;
		return;
	} /*endif*/

	if  (strcmp(item,"MAXDRAW") == 0)  {
		sscanf( value, "%d", &hpv_maxdrawcnt );
	} else {
		*status = HPE_UKHCITEM;
		return;
	} /*endif*/

} /* end of hp_setpar */



/*----------------------------------------------------------------------------*/



void hp_arrayswap( BOOLEAN on_off )

/* switches array-swap on ot off
 *
 * parameter of routine
 * BOOLEAN   on_off;     TRUE=on, FALSE=off
 */
{
	/* executable code */

	hpv_arrayswap = on_off;

} /* end of hp_arrayswap */



/*----------------------------------------------------------------------------*/
/*                            local routines                                  */
/*----------------------------------------------------------------------------*/



void hph_trafo( GBC_COO ux, GBC_COO uy, HPOUT *dx, HPOUT *dy )

/* transforms user coordinates (ux,uy) to device coordinates (dx,dy)
 *
 * parameters of routine
 * GBC_COO    ux, uy;     input; user coordinates
 * HPOUT      dx, dy;     output; device coordinates
 */
{
	/* executable code */

	*dx = ux * hpv_trafo_xf + hpv_trafo_xo;
	*dy = uy * hpv_trafo_yf + hpv_trafo_yo;

} /* end of hph_trafo */



/*----------------------------------------------------------------------------*/



void hph_create_hcname( char name[] )

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
	strcat( name, ".HP" );

} /* end of hph_create_filename */



/*----------------------------------------------------------------------------*/


void hph_insertheader( FILE *hp, STATUS *status )

/* inserts header to PostScript file
 *
 * parameter of routine
 * FILE      *hp;     input; PostScript file to insert header
 */
{
	/* local variables */
	FILE     *hf;                     /* header file */
	char     line[BC_LONGSTRLTH+1];   /* current line */
	long     bytes;                   /* number of bytes read */
	char     str[BC_LINELTH+1];       /* scratch */

	/* executable code */

	*str = ESC;
	sprintf( str+1, ".(" );
	fprintf( hp, str );  /* plotter on */

	hf = sy_fopen( HPHEADER, "r" );
	if  (hf == NULL)  {
		strcpy( line, hpv_inputdir );
		strcat( line, HPHEADER );
		hf = sy_fopen( line, "r" );
		if  (hf == NULL)  {
			fprintf( hp, "*** no header file available ***" );
			*status = HPE_NOHEADER;
			return;
		} /*endif*/
	} /*endif*/

	do  {
		bytes = fread( line, sizeof(char), BC_LONGSTRLTH, hf );
		if  (bytes > 0)
			fwrite( line, sizeof(char), bytes, hp );
	}  while  (bytes == BC_LONGSTRLTH);

	fclose( hf );

} /* end of hph_insertheader */



/*----------------------------------------------------------------------------*/


void hph_closehpf( FILE *hp )

/* closes hardcopy file
 *
 * parameter of routine
 * FILE      *hp;      input; pointer to hardcopy file
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];    /* scratch string */

	/* executable code */

	fprintf( hp, "IW;SP;PG;" );
	*str = ESC;
	sprintf( str+1, ".)" );
	fprintf( hp, str );  /* plotter off */
	fclose( hp );

} /* end of hph_closehpf */



/*----------------------------------------------------------------------------*/


void hph_changestyle( int style )

/* changes the current style
 *
 * parameters of routine
 * int        style;      input; new style number
 */
{
	/* executable code */

	if  (hpv_drawcnt > 0)  {
		fprintf( hpv_pf, ";\n" );
		hpv_drawcnt = 0;
	} /*endif*/
	if  (style < 0)  style = 0;
	if  (style >= HPC_MAXSTYLE)  style = 0;
	hpv_currstyle = style;
	fprintf( hpv_pf, "SI %4.2f %4.2f\n", hpv_csize[style]*0.7,
		hpv_csize[style] );
	fprintf( hpv_pf, "SP %d;\n", hpv_lwidth[style] );

} /* end of hph_changestyle */



/*----------------------------------------------------------------------------*/


/* file CCCH.C
 *      ======
 *
 * version 5, 1-Oct-92
 *
 * Calcomp channel for graphic output module
 * K. Stammler, 21-AUG-1990
 */



#include <stdio.h>
#include <string.h>
#include BASECNST
#ifdef BC_VAX
#include <descrip.h>
#endif
#include BC_SYSBASE
#include "graphbas.h"
#include "ccusrdef.h"
#include "ccerrors.h"


/* global constants */
#define CCC_PEN_UP 3
	/* plot subfunction code (pen up) */
#define CCC_PEN_DOWN 2
	/* plot subfunction code (pen down) */
#define CCC_PLOTEND 999
	/* plot subfunction code (end of plot) */
#define CCC_MAXSTYLE 10
	/* maximum number of different line styles */
#define NO_OF_PENS 3
	/* number of plotter pens */
#define DEFAULTWIDTH 100.0
	/* default width of user coo */
#define DEFAULTHEIGHT 100.0
	/* default height of user coo */
#define DEFAULTPLOTW 25.9
#define DEFAULTPLOTH 19.5
	/* default plot size */
#define MAXDEFSTYLE 6
	/* number of default style */
#define STRLTH 80
#ifdef BC_VAX
#define CC_CLOSE_LOG "SYS$LOGIN:CC$CLOSE_LOG.LOG"
#endif
#define TMP_CALCOMP "HC_CC.PFL" 
#define TOSCREEN 0
#define TOFILE 1


/* external variable */
int         ccv_channel=TOSCREEN;      /* calcomp switch screen/file */


/* global variables */
static char    ccv_outputdir[BC_FILELTH+1];   /* output directory */
static BOOLEAN ccv_isinit;    /* calcomp is initialised */
static CCOUT   ccv_width;     /* width of display */
static CCOUT   ccv_height;    /* heigth of display */
static CCOUT   ccv_xoffset;   /* x-offset */
static CCOUT   ccv_yoffset;   /* y-offset */
static float   ccv_trafo_xf;  /* trafo x-factor */
static float   ccv_trafo_yf;  /* trafo y-factor */
static CCOUT   ccv_trafo_xo;  /* x-offset */
static CCOUT   ccv_trafo_yo;  /* y-offset */
static BOOLEAN ccv_arrayswap=FALSE;     /* swap arrayplot coo's */
static float   ccv_dashl[CCC_MAXSTYLE]; /* linestyles */
static float   ccv_csize[CCC_MAXSTYLE]; /* character sizes (units of height) */
static float   ccv_cangle[CCC_MAXSTYLE] /* text angles */
	={0.,0.,0.,0.,0.,0.,0.,0.,0.,0.};
static float   ccv_defstyle[MAXDEFSTYLE] /* default line styles */
	={0.0,.1,.2,.3,.4,.5};
static int     ccv_pen[CCC_MAXSTYLE]     /* pen number */
	={1,1,1,1,1,1,1,1,1,1};
static int     ccv_currpen=1;            /* current pen */


/* hardcopy setup */
static char    ccv_ccpar[STRLTH+1];      /* CCFILE parameters */
static char    ccv_hcrot[STRLTH+1]={"L"};/* output value: CCFILE/OUT=ccv_hcrot*/


/* prototypes of local routines */
void cch_init( void );
void cch_close( BOOLEAN delete );
void cch_trafo( CCCOO ux, CCCOO uy, CCOUT *dx, CCOUT *dy );
void cch_create_hcname( char name[] );

void plot( float *x, float *y, int *ip );

/*----------------------------------------------------------------------------*/



void cc_init( int attribs, CCOUT xlo, CCOUT ylo, CCOUT width, CCOUT height,
	STATUS *status )

/* initialises calcomp channel
 *
 * parameters of routine
 * int        attribs;        input; attributes
 * CCOUT      xlo, ylo;       input; offset of output
 * CCOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	char     str[BC_FILELTH+1];     /* scratch string */
#	ifdef BC_VAX
	$DESCRIPTOR( dirname, " " );
	$DESCRIPTOR( calcomp, "CALCOMP_PFL" );
#	endif
	int      i;      /* counter */

	/* executable code */

	if  ((width <= 0.0) || (height <= 0.0))  {
		*status = CCE_ILPAR;
		return;
	} /*endif*/

	if  (ccv_isinit)  cc_exit();

	ccv_xoffset = xlo;
	ccv_yoffset = ylo;
	ccv_width = width;
	ccv_height = height;

	for  (i=0; i<CCC_MAXSTYLE; i++)
		if  (ccv_csize[i] == 0.0)
			ccv_csize[i] = 0.05;

	/* open calcomp file SYS$LOGIN:HC$CC.PFL */
#	ifdef BC_VAX
	strcpy( str, ccv_outputdir );
	strcat( str, TMP_CALCOMP );
	dirname.dsc$w_length = strlen( str );
	dirname.dsc$a_pointer = str;
	lib$set_logical( &calcomp, &dirname );
#	endif
	ccv_channel = TOFILE;
	cch_init();
	ccv_isinit = TRUE;

	cc_setcoo( 0., 0., DEFAULTWIDTH, DEFAULTHEIGHT, status );

} /* end of cc_init */



/*----------------------------------------------------------------------------*/



void cc_exit( void )

/* exits calcomp channel
 *
 * no parameters
 */
{
	/* local variables */
#	ifdef BC_VAX
	$DESCRIPTOR( calcomp, "CALCOMP_PFL" );
#	endif

	/* executable code */

	if  (!ccv_isinit)  return;
	ccv_channel = TOFILE;
	cch_close( TRUE );
	ccv_isinit = FALSE;
#	ifdef BC_VAX
	lib$delete_logical( &calcomp );
#	endif

} /* end of cc_exit */



/*----------------------------------------------------------------------------*/



void cc_resize( CCOUT xlo, CCOUT ylo, CCOUT width, CCOUT height,
	STATUS *status )

/* resizes output of calcomp
 *
 * parameters of routine
 * CCOUT      xlo, ylo;       input; offset of output
 * CCOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */
{
	/* executable code */

	if  ((width <= 0.0) || (height <= 0.0))  {
		*status = CCE_ILPAR;
		return;
	} /*endif*/

	if  (!ccv_isinit)  cc_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	ccv_xoffset = xlo;
	ccv_yoffset = ylo;
	ccv_width = width;
	ccv_height = height;

} /* end of cc_resize */



/*----------------------------------------------------------------------------*/



void cc_erase( void )

/* clears calcomp output channel
 *
 * no parameters
 */
{
	/* local variables */
	int      dmy=0;

	/* executable code */

	ccv_channel = TOFILE;
	if  (ccv_isinit)  {
		cch_close( TRUE );
		cch_init();
	} else {
		cc_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );
	} /*endif*/

} /* end of cc_erase */



/*----------------------------------------------------------------------------*/



void cc_setcoo( CCCOO x, CCCOO y, CCCOO w, CCCOO h, STATUS *status )

/* sets user coordinates
 *
 * parameters of routine
 * CCCOO      x, y;     input; offset
 * CCCOO      w, h;     input; size
 * STATUS     *status;  output; return status
 */
{
	/* executable code */

	if  ((w <= 0.0) || (h <= 0.0))  {
		*status = CCE_ILPAR;
		return;
	} /*endif*/

	if  (!ccv_isinit)  cc_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	ccv_trafo_xf = ccv_width / w;
	ccv_trafo_yf = ccv_height / h;
	ccv_trafo_xo = ccv_xoffset - x*ccv_trafo_xf;
	ccv_trafo_yo = ccv_yoffset - y*ccv_trafo_yf;

} /* end of cc_setcoo */



/*----------------------------------------------------------------------------*/



void cc_moveto( CCCOO x, CCCOO y )

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * CCCOO      x, y;    input; position
 */
{
	/* local variables */
	int      code;     /* plot subfunction */
	CCOUT    dx, dy;   /* device coordinates */
	STATUS   dmy=0;    /* scratch */

	/* executable code */

	if  (!ccv_isinit)  cc_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );

	ccv_channel = TOFILE;
	cch_trafo( x, y, &dx, &dy );
	code = CCC_PEN_UP;
	plot( &dx, &dy, &code );

} /* end of cc_moveto */



/*----------------------------------------------------------------------------*/



void cc_drawto( int style, CCCOO x, CCCOO y )

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        style;   input; style parameter
 * CCCOO      x, y;    input; position
 */
{
	/* local variables */
	int      code;     /* plot subfunction */
	CCOUT    dx, dy;   /* device coordinates */
	STATUS   dmy=0;    /* scratch */

	/* executable code */

	if  (!ccv_isinit)  cc_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );

	if  ((style < 0) || (style >= CCC_MAXSTYLE))  style = 0;
	cch_trafo( x, y, &dx, &dy );
	ccv_channel = TOFILE;
	if  (ccv_currpen != ccv_pen[style])  {
		newpen( ccv_pen+style );
		ccv_currpen = ccv_pen[style];
	} /*endif*/
	if  (ccv_dashl[style] == 0.0)  {
		code = CCC_PEN_DOWN;
		plot( &dx, &dy, &code );
	} else {
		dashp( &dx, &dy, ccv_dashl+style );
	} /*endif*/

} /* end of cc_drawto */


/*----------------------------------------------------------------------------*/



void cc_arrayplot( int style, long cnt, int red, CCCOO xoff,
	CCCOO xinc, CCCOO yoff, CCCOO yarr[], float yzoom, STATUS *status )

/* plots array of data points
 *
 * parameters of routine
 * int        style;       input; style parameter
 * long       cnt;         input; number of data samples in yarr
 * int        red;         input; reduction factor
 * CCCOO      xoff;        input; x offset
 * CCCOO      xinc;        input; x increment
 * CCCOO      yoff;        input; y offset
 * CCCOO      yarr[];      input; data array
 * float      yzoom;       input; amplitude zoom factor
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	long     i;          /* sample counter */

	/* executable code */

	if  (!ccv_isinit)  cc_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	if  (ccv_arrayswap)  {
		cc_moveto( yoff + (*yarr) * yzoom, xoff );
		for  (i=red; i<cnt; i += red )
			cc_drawto( style, yoff + yarr[i] * yzoom, xoff + xinc*(float)i );
	} else {
		cc_moveto( xoff, yoff + (*yarr) * yzoom );
		for  (i=red; i<cnt; i += red )
			cc_drawto( style, xoff + xinc*(float)i, yoff + yarr[i] * yzoom );
	} /*endif*/

} /* end of cc_arrayplot */



/*----------------------------------------------------------------------------*/



void cc_text( int style, CCCOO x, CCCOO y, char text[] )

/* prints text at position (x,y)
 *
 * parameters of routine
 * int        style;     input; style parameter (size)
 * CCCOO      x, y;      input; text position (user coordinates)
 * char       text[];    input; text to be printed
 */
{
	/* local variables */
	CCOUT    dx, dy;   /* device coordinates */
	int      slen;     /* length of string */
	float    size;     /* size of output */
	STATUS   dmy;      /* scratch */

	/* executable code */

	if  (!ccv_isinit)  cc_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, &dmy );

	if  (style < 0)  style = 0;
	if  (style >= CCC_MAXSTYLE)  style = CCC_MAXSTYLE-1;
	cch_trafo( x, y, &dx, &dy );
	slen = strlen( text );
	size = ccv_csize[style] * ccv_height;
	ccv_channel = TOFILE;
	if  (ccv_currpen != ccv_pen[style])  {
		newpen( ccv_pen+style );
		ccv_currpen = ccv_pen[style];
	} /*endif*/
	symbol( &dx, &dy, &size, text, ccv_cangle+style, &slen );

} /* end of cc_text */



/*----------------------------------------------------------------------------*/



void cc_setstyle( int style, char item[], char value[], STATUS *status )

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
	float    r, g, b;  /* colours */

	/* executable code */

	if  ((style < 0) || (style >= CCC_MAXSTYLE))  {
		*status = CCE_ILSTYLE;
		return;
	} /*endif*/

	if  (!ccv_isinit)  cc_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	if  (strcmp(item,"CHARSIZE") == 0  ||  strcmp(item,"CHARHEIGHT") == 0)  {
		if  (sscanf(value,"%f",&num) != 1)  {
			*status = CCE_ILVALUE;
			return;
		} /*endif*/
		ccv_csize[style] = num;
	} else if  (strcmp(item,"LINESTYLE") == 0)  {
		if  (sscanf(value,"%i",&i) != 1)  {
			*status = CCE_ILVALUE;
			return;
		} else if  ((i < 0) || (i >= MAXDEFSTYLE))  {
			*status = CCE_ILVALUE;
			return;
		} /*endif*/
		ccv_dashl[style] = ccv_defstyle[i];
	} else if  (strcmp(item,"LINESTYLE_C") == 0)  {
		if  (sscanf(value,"%f",&num) != 1)  {
			*status = CCE_ILVALUE;
			return;
		} /*endif*/
		ccv_dashl[style] = num;
	} else if  (strcmp(item,"COLOR") == 0)  {
		if  (sscanf(value,"%f,%f,%f",&r,&g,&b) != 3)  {
			*status = CCE_ILVALUE;
			return;
		} /*endif*/
		i = (int)((r+b+g)/3.0) + 1;
		i %= NO_OF_PENS;
		if  (i > 0)  ccv_pen[style] = i;
	} else if  (strcmp(item,"CHARROT") == 0)  {
		if  (sscanf(value,"%f",&num) != 1)  {
			*status = CCE_ILVALUE;
			return;
		} /*endif*/
		ccv_cangle[style] = num;
	} else if  (strcmp(item,"FONT") == 0)  {
	} else if  (strcmp(item,"LINEWIDTH") == 0)  {
		if  (sscanf(value,"%f",&num) != 1)  {
			*status = CCE_ILVALUE;
			return;
		} /*endif*/
		ccv_pen[style] = Nint( num );
	} else {
		*status = CCE_UKITEM;
		return;
	} /*endif*/

} /* end of cc_setstyle */



/*----------------------------------------------------------------------------*/



void cc_set_outputdir( char dir[], STATUS *status )

/* sets output directory
 *
 * parameters of routine
 * char       dir[];        input; new directory path
 * STATUS     *status;      output; return status
 */
{
	/* executable code */

	if  (strlen(dir) > BC_FILELTH)  {
		*status = CCE_STROVFL;
		return;
	} /*endif*/
	strcpy( ccv_outputdir, dir );

} /* end of cc_set_outputdir */



/*----------------------------------------------------------------------------*/



void cc_prepare( STATUS *status )

/* prepares hardcopy
 *
 * parameters of routine
 * STATUS     *status;     output; return status
 */
{
	/* executable code */

	cc_erase();

} /* end of cc_prepare */



/*----------------------------------------------------------------------------*/



void cc_cleanup( char outf[], STATUS *status )

/* finishes hardcopy
 *
 * parameters of routine
 * char       outf[];      output; output filename
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	char     hcname[BC_FILELTH+1];    /* name of hardcopy file */
	char     tmpname[BC_FILELTH+1];   /* temporary calcomp file */

	/* executable code */

	if  (!ccv_isinit)  cc_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	/* create hardcopy name */
	cch_create_hcname( hcname );

	/* close calcomp file */
	ccv_channel = TOFILE;
	cch_close( FALSE );
	strcpy( outf, ccv_outputdir );
	strcat( outf, hcname );
	sy_fdelete( outf );
	strcpy( tmpname, ccv_outputdir );
	strcat( tmpname, TMP_CALCOMP );
	sy_frename( tmpname, outf );

	cch_init();

} /* end of cc_cleanup */



/*----------------------------------------------------------------------------*/



void cc_setpar( char item[], char value[], STATUS *status )

/* sets hardcopy parameters
 *
 * parameters of routine
 * char       item[];     input; item to be set
 * char       value[];    input; new value of item
 * STATUS     *status;    output; return status
 */
{
	/* executable code */

	if  (strlen(value) > STRLTH)  {
		*status = CCE_STROVFL;
		return;
	} /*endif*/

	if  (strcmp(item,"CCPAR") == 0)  {
		strcpy( ccv_ccpar, value );
	} else if  (strcmp(item,"CCROT") == 0)  {
		strcpy( ccv_hcrot, value );
	} else {
		*status = CCE_UKHCITEM;
		return;
	} /*endif*/

} /* end of cc_setpar */



/*----------------------------------------------------------------------------*/



void cc_charsize( int style, float size, STATUS *status )

/* sets character size in units of display height
 *
 * parameters of routine
 * int        style;     input; style number
 * float      size;      input; size of character in units of display height
 * STATUS     *status;   output; return status
 */
{
	/* executable code */

	if  ((style < 0) || (style >= CCC_MAXSTYLE))  {
		*status = CCE_ILSTYLE;
		return;
	} /*endif*/

	if  (!ccv_isinit)  cc_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	ccv_csize[style] = size;

} /* end of cc_charsize */



/*----------------------------------------------------------------------------*/



void cc_linestyle( int style, int linestyle, STATUS *status )

/* sets line linestyle in style block number "style"
 *
 * parameters of routine
 * int        style;      input; style block number
 * int        linestyle;  input; number of line style
 * STATUS     *status;    output; return status
 */
{
	/* executable code */

	if  ((style < 0) || (style >= CCC_MAXSTYLE))  {
		*status = CCE_ILSTYLE;
		return;
	} else if  ((linestyle < 0) || (linestyle >= MAXDEFSTYLE))  {
		*status = CCE_ILVALUE;
		return;
	} /*endif*/

	if  (!ccv_isinit)  cc_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	ccv_dashl[style] = ccv_defstyle[linestyle];

} /* end of cc_linestyle */



/*----------------------------------------------------------------------------*/



void cc_color( int style, float red, float green, float blue, STATUS *status )

/* sets color of style block "style"
 *
 * parameters of routine
 * int        style;            input; style block number
 * float      red, green, blue; input; red, green and blue intensities (0..1)
 * STATUS     *status;          output; return status
 */
{
	/* local variables */
	int      pen;      /* pen number */

	/* executable code */

	if  ((style < 0) || (style >= CCC_MAXSTYLE))  {
		*status = CCE_ILSTYLE;
		return;
	} /*endif*/

	if  (!ccv_isinit)  cc_init( 0, 0.0, 0.0, DEFAULTPLOTW, DEFAULTPLOTH, status);

	pen = (int)((red+blue+green)/3.0) + 1;
	pen %= NO_OF_PENS;
	ccv_pen[style] = pen;

} /* end of cc_color */



/*----------------------------------------------------------------------------*/



float cc_aspectratio( void )

/* returns ratio of width to height of output
 * no parameters
 */
{
	return ccv_width/ccv_height;

} /* end of cc_aspectratio */



/*----------------------------------------------------------------------------*/



void cc_arrayswap( BOOLEAN on_off )

/* switches array-swap on ot off
 *
 * parameter of routine
 * BOOLEAN   on_off;     TRUE=on, FALSE=off
 */
{
	/* executable code */

	ccv_arrayswap = on_off;

} /* end of cc_arrayswap */



/*----------------------------------------------------------------------------*/
/*                            local routines                                  */
/*----------------------------------------------------------------------------*/



void cch_init( void )

/* initialises calcomp channel
 *
 * no parameters
 */
{
	/* local variables */
	int      unit;     /* file unit */
	float    x, y;     /* position */

	/* executable code */

	/* FORTRAN routine */
	unit = 53;
	x = y = 0.0;
	plots( &unit, &x, &y );

} /* end of cch_init */



/*----------------------------------------------------------------------------*/



void cch_close( BOOLEAN delete )

/* closes calcomp channel
 *
 * parameters of routine
 * BOOLEAN    delete;      input; delete or not calcomp file
 */
{
	/* local variables */
	float    x, y;           /* position */
	int      code;           /* plot subfunction code */
	char     str[BC_FILELTH+1];  /* scratch */
#	ifdef BC_VAX
	$DESCRIPTOR( outstream, "SYS$OUTPUT" );
	$DESCRIPTOR( logfile, CC_CLOSE_LOG );
#	endif

	/* executable code */

#	ifdef BC_VAX
	lib$set_logical( &outstream, &logfile );
#	endif
	x = y = 0.0;
	code = CCC_PLOTEND;
	plot( &x, &y, &code );
#	ifdef BC_VAX
	lib$delete_logical( &outstream );
	/* delete log file */
	sy_fdelete( CC_CLOSE_LOG );
#	endif

	/* delete calcomp file */
	if  (delete)  {
		strcpy( str, ccv_outputdir );
		strcat( str, TMP_CALCOMP );
		sy_fdelete( str );
	} /*endif*/

} /* end of cch_close */



/*----------------------------------------------------------------------------*/



void cch_trafo( CCCOO ux, CCCOO uy, CCOUT *dx, CCOUT *dy )

/* transforms user coordinates (ux,uy) to device coordinates (dx,dy)
 *
 * parameters of routine
 * CCCOO      ux, uy;     input; user coordinates
 * CCOUT      dx, dy;     output; device coordinates
 */
{
	/* executable code */

	*dx = ux * ccv_trafo_xf + ccv_trafo_xo;
	*dy = uy * ccv_trafo_yf + ccv_trafo_yo;

} /* end of cch_trafo */



/*----------------------------------------------------------------------------*/



void cch_create_hcname( char name[] )

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
	strcat( name, ".PFL" );

} /* end of cch_create_filename */



/*----------------------------------------------------------------------------*/



void plot( float *x, float *y, int *ip )

/* THE plot routine
 *
 * parameters of routine
 * float      *x, *y;     input; plot coordinates
 * int        *ip;        input; plot mode
 */
{

	if  (ccv_channel == TOSCREEN)  {
		pltk( x, y, ip );
	} else {
		plcf( x, y, ip );
	} /*endif*/

} /* end of plot */



/*----------------------------------------------------------------------------*/

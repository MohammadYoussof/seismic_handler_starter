
/* file TKCH.C
 *      ======
 *
 * version 4, 6-Oct-92
 *
 * Calcomp channel for graphic output module
 * K. Stammler, 21-AUG-1990
 */


#include <stdio.h>
#include <string.h>
#include <descrip.h>
#include BASECNST
#include BC_SYSBASE
#include "tkusrdef.h"
#include "ccerrors.h"


/* global constants */
#define TKC_PEN_UP 3
	/* plot subfunction code (pen up) */
#define TKC_PEN_DOWN 2
	/* plot subfunction code (pen down) */
#define TKC_PLOTEND 999
	/* plot subfunction code (end of plot) */
#define TKC_MAXSTYLE 10
	/* maximum number of different line styles */
#define DEFAULTWIDTH 100.0
	/* default width of user coo */
#define DEFAULTHEIGHT 100.0
	/* default height of user coo */
#define MAXDEFSTYLE 6
	/* number of default style */
#define STRLTH 80
#define TOSCREEN 0


/* external variable */
extern int  ccv_channel;      /* calcomp switch screen/file */


/* global variables */
static TKOUT   tkv_width;     /* width of display */
static TKOUT   tkv_height;    /* heigth of display */
static TKOUT   tkv_xoffset;   /* x-offset */
static TKOUT   tkv_yoffset;   /* y-offset */
static float   tkv_trafo_xf;  /* trafo x-factor */
static float   tkv_trafo_yf;  /* trafo y-factor */
static TKOUT   tkv_trafo_xo;  /* x-offset */
static TKOUT   tkv_trafo_yo;  /* y-offset */
static TKCOO   tkv_userx;     /* user coo, x lo */
static TKCOO   tkv_usery;     /* user coo, y lo */
static TKCOO   tkv_userw;     /* user coo, width */
static TKCOO   tkv_userh;     /* user coo, height */
static BOOLEAN tkv_send=TRUE; /* send output immediately */
static BOOLEAN tkv_arrayswap=FALSE;     /* swap arrayplot coo's */
static BOOLEAN tkv_isinit;    /* module is initialised */
static float   tkv_dashl[TKC_MAXSTYLE]; /* linestyles */
static float   tkv_csize[TKC_MAXSTYLE]; /* character sizes (units of height) */
static float   tkv_cangle[TKC_MAXSTYLE] /* text angles */
	={0.,0.,0.,0.,0.,0.,0.,0.,0.,0.};
static float   tkv_defstyle[MAXDEFSTYLE] /* default line styles */
	={0.0,.1,.2,.3,.4,.5};


/* prototypes of local routines */
void tkh_init( void );
void tkh_close( void );
void tkh_trafo( TKCOO ux, TKCOO uy, TKOUT *dx, TKOUT *dy );

/*----------------------------------------------------------------------------*/



void tk_init( int attribs, TKOUT xlo, TKOUT ylo, TKOUT width, TKOUT height,
	STATUS *status )

/* initialises calcomp channel
 *
 * parameters of routine
 * int        attribs;        input; attributes
 * TKOUT      xlo, ylo;       input; offset of output
 * TKOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	int      i;      /* counter */

	/* executable code */

	if  ((width <= 0.0) || (height <= 0.0))  {
		*status = CCE_ILPAR;
		return;
	} /*endif*/

	/* tkv_xoffset = xlo;
	 * tkv_yoffset = ylo;
	 * tkv_width = width;
	 * tkv_height = height;
	 */
	tkv_xoffset = 0.0;
	tkv_yoffset = 0.0;
	tkv_width = 34.57;
	tkv_height = 26.32;

	for  (i=0; i<TKC_MAXSTYLE; i++)
		tkv_csize[i] = 0.05 * tkv_height;

	tk_setcoo( 0., 0., DEFAULTWIDTH, DEFAULTHEIGHT, status );

	ccv_channel = TOSCREEN;
	tkh_init();

} /* end of tk_init */



/*----------------------------------------------------------------------------*/



void tk_exit( void )

/* exits calcomp channel
 *
 * no parameters
 */
{
	/* executable code */

	if  (!tkv_isinit)  return;
	ccv_channel = TOSCREEN;
	tkh_close();

} /* end of tk_exit */



/*----------------------------------------------------------------------------*/



void tk_resize( TKOUT xlo, TKOUT ylo, TKOUT width, TKOUT height,
	STATUS *status )

/* resizes output of calcomp
 *
 * parameters of routine
 * TKOUT      xlo, ylo;       input; offset of output
 * TKOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */
{
	/* executable code */

	if  ((width <= 0.0) || (height <= 0.0))  {
		*status = CCE_ILPAR;
		return;
	} /*endif*/

/*  tkv_xoffset = xlo;
 * tkv_yoffset = ylo;
 * tkv_width = width;
 * tkv_height = height;
 */

} /* end of tk_resize */



/*----------------------------------------------------------------------------*/



void tk_erase( void )

/* clears calcomp output channel
 *
 * no parameters
 */
{
	/* executable code */

/* ccv_channel = TOSCREEN;
 * tkh_close();
 * tkh_init();
 */
	newpag();
	if  (tkv_send)  tsend();

} /* end of tk_erase */



/*----------------------------------------------------------------------------*/



void tk_setcoo( TKCOO x, TKCOO y, TKCOO w, TKCOO h, STATUS *status )

/* sets user coordinates
 *
 * parameters of routine
 * TKCOO      x, y;     input; offset
 * TKCOO      w, h;     input; size
 * STATUS     *status;  output; return status
 */
{
	/* executable code */

	if  ((w <= 0.0) || (h <= 0.0))  {
		*status = CCE_ILPAR;
		return;
	} /*endif*/

	tkv_userx = x;
	tkv_usery = y;
	tkv_userw = w;
	tkv_userh = h;
	tkv_trafo_xf = tkv_width / w;
	tkv_trafo_yf = tkv_height / h;
	tkv_trafo_xo = tkv_xoffset - x*tkv_trafo_xf;
	tkv_trafo_yo = tkv_yoffset - y*tkv_trafo_yf;

} /* end of tk_setcoo */



/*----------------------------------------------------------------------------*/



void tk_moveto( TKCOO x, TKCOO y )

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * TKCOO      x, y;    input; position
 */
{
	/* local variables */
	int      code;     /* plot subfunction */
	TKOUT    dx, dy;   /* device coordinates */

	/* executable code */

	ccv_channel = TOSCREEN;
	tkh_trafo( x, y, &dx, &dy );
	code = TKC_PEN_UP;
	plot( &dx, &dy, &code );

} /* end of tk_moveto */



/*----------------------------------------------------------------------------*/



void tk_drawto( int style, TKCOO x, TKCOO y )

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        style;   input; style parameter
 * TKCOO      x, y;    input; position
 */
{
	/* local variables */
	int      code;     /* plot subfunction */
	TKOUT    dx, dy;   /* device coordinates */

	/* executable code */

	if  ((style < 0) || (style >= TKC_MAXSTYLE))  style = 0;
	tkh_trafo( x, y, &dx, &dy );
	ccv_channel = TOSCREEN;
	if  (tkv_dashl[style] == 0.0)  {
		code = TKC_PEN_DOWN;
		plot( &dx, &dy, &code );
	} else {
		dashp( &dx, &dy, tkv_dashl+style );
	} /*endif*/
	if  (tkv_send)  tsend();

} /* end of tk_drawto */


/*----------------------------------------------------------------------------*/



void tk_arrayplot( int style, long cnt, int red, TKCOO xoff,
	TKCOO xinc, TKCOO yoff, TKCOO yarr[], float yzoom, STATUS *status )

/* plots array of data points
 *
 * parameters of routine
 * int        style;       input; style parameter
 * long       cnt;         input; number of data samples in yarr
 * int        red;         input; reduction factor
 * TKCOO      xoff;        input; x offset
 * TKCOO      xinc;        input; x increment
 * TKCOO      yoff;        input; y offset
 * TKCOO      yarr[];      input; data array
 * float      yzoom;       input; amplitude zoom factor
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	long     i;          /* sample counter */

	/* executable code */

	tkv_send = FALSE;
	if  (tkv_arrayswap)  {
		tk_moveto( yoff + (*yarr) * yzoom, xoff );
		for  (i=red; i<cnt; i += red )
			tk_drawto( style, yoff + yarr[i] * yzoom, xoff + xinc*(float)i );
	} else {
		tk_moveto( xoff, yoff + (*yarr) * yzoom );
		for  (i=red; i<cnt; i += red )
			tk_drawto( style, xoff + xinc*(float)i, yoff + yarr[i] * yzoom );
	} /*endif*/
	tkv_send = TRUE;
	tsend();

} /* end of tk_arrayplot */



/*----------------------------------------------------------------------------*/



void tk_text( int style, TKCOO x, TKCOO y, char text[] )

/* prints text at position (x,y)
 *
 * parameters of routine
 * int        style;     input; style parameter (size)
 * TKCOO      x, y;      input; text position (user coordinates)
 * char       text[];    input; text to be printed
 */
{
	/* local variables */
	TKOUT    dx, dy;   /* device coordinates */
	int      slen;     /* length of string */

	/* executable code */

	if  (style < 0)  style = 0;
	if  (style >= TKC_MAXSTYLE)  style = TKC_MAXSTYLE-1;
	tkh_trafo( x, y, &dx, &dy );
	slen = strlen( text );
	ccv_channel = TOSCREEN;
	symbol( &dx, &dy, tkv_csize+style, text, tkv_cangle+style, &slen );
	if  (tkv_send)  tsend();

} /* end of tk_text */



/*----------------------------------------------------------------------------*/



void tk_write( char text[] )

/* write text to current write position
 *
 * parameters of routine
 * char       text[];     input; text to be printed
 */
{
	/* executable code */

	anmode();
	tsend();
	printf( "%s", text );

} /* end of tk_write */



/*----------------------------------------------------------------------------*/



void tk_setstyle( int style, char item[], char value[], STATUS *status )

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

	/* executable code */

	if  ((style < 0) || (style >= TKC_MAXSTYLE))  {
		*status = CCE_ILSTYLE;
		return;
	} /*endif*/

	if  (strcmp(item,"CHARSIZE") == 0)  {
		if  (sscanf(value,"%f",&num) != 1)  {
			*status = CCE_ILVALUE;
			return;
		} /*endif*/
		tkv_csize[style] = num * tkv_height;
	} else if  (strcmp(item,"LINESTYLE") == 0)  {
		if  (sscanf(value,"%i",&i) != 1)  {
			*status = CCE_ILVALUE;
			return;
		} else if  ((i < 0) || (i >= MAXDEFSTYLE))  {
			*status = CCE_ILVALUE;
			return;
		} /*endif*/
		tkv_dashl[style] = tkv_defstyle[i];
	} else if  (strcmp(item,"LINESTYLE_C") == 0)  {
		if  (sscanf(value,"%f",&num) != 1)  {
			*status = CCE_ILVALUE;
			return;
		} /*endif*/
		tkv_dashl[style] = num;
	} else if  (strcmp(item,"COLOR") == 0)  {
		if  (sscanf(value,"%i",&i) != 1)  {
			*status = CCE_ILVALUE;
			return;
		} /*endif*/
		/* i = i % NO_OF_PENS; */ /* !!! */
		if  (tkv_isinit)  {
			ccv_channel = TOSCREEN;
			if  (i > 0)  newpen( &i );
		} /*endif*/
	} else if  (strcmp(item,"FONT") == 0)  {
	} else if  (strcmp(item,"LINEWIDTH") == 0)  {
	} else {
		*status = CCE_UKITEM;
		return;
	} /*endif*/

} /* end of tk_setstyle */



/*----------------------------------------------------------------------------*/



void tk_charsize( int style, float size, STATUS *status )

/* sets character size in units of display height
 *
 * parameters of routine
 * int        style;     input; style number
 * float      size;      input; size of character in units of display height
 * STATUS     *status;   output; return status
 */
{
	/* executable code */

	if  ((style < 0) || (style >= TKC_MAXSTYLE))  {
		*status = CCE_ILSTYLE;
		return;
	} /*endif*/

	tkv_csize[style] = size * tkv_height;

} /* end of tk_charsize */



/*----------------------------------------------------------------------------*/



void tk_linestyle( int style, int linestyle, STATUS *status )

/* sets line linestyle in style block number "style"
 *
 * parameters of routine
 * int        style;      input; style block number
 * int        linestyle;  input; number of line style
 * STATUS     *status;    output; return status
 */
{
	/* executable code */

	if  ((style < 0) || (style >= TKC_MAXSTYLE))  {
		*status = CCE_ILSTYLE;
		return;
	} else if  ((linestyle < 0) || (linestyle >= MAXDEFSTYLE))  {
		*status = CCE_ILVALUE;
		return;
	} /*endif*/

	tkv_dashl[style] = tkv_defstyle[linestyle];

} /* end of tk_linestyle */



/*----------------------------------------------------------------------------*/



void tk_getloc( TKCOO *x, TKCOO *y, char *ch )

/* returns user selected position
 *
 * parameters of routine
 * TKCOO      *x, *y;      output; selected position (in user coo)
 * char       *ch;         output; key pressed
 */
{
	/* local variables */
	int      ascii;       /* ascii code of key */
	float    xmax, ymax;  /* maximum values of x & y */

	/* executable code */

	xmax = tkv_userx + tkv_userw;
	ymax = tkv_usery + tkv_userh;
	dwindo( &tkv_userx, &xmax, &tkv_usery, &ymax );

	vcursr( &ascii, x, y );
	*ch = (char)ascii;

} /* end of tk_getloc */



/*----------------------------------------------------------------------------*/



float tk_aspectratio()

/* returns ratio of width to height
 * no parameters
 */
{
	return tkv_width/tkv_height;

} /* end of tk_aspectratio */



/*----------------------------------------------------------------------------*/



void tk_arrayswap( BOOLEAN on_off )

/* switches array-swap on ot off
 *
 * parameter of routine
 * BOOLEAN   on_off;     TRUE=on, FALSE=off
 */
{
	/* executable code */

	tkv_arrayswap = on_off;

} /* end of tk_arrayswap */



/*----------------------------------------------------------------------------*/



void tk_prepare( STATUS *status )

/* prepare redraw
 *
 * parameters of routine
 * STATUS     *status;     output;  return status
 */
{
	/* executable code */

	tk_erase();

} /* end of tk_prepare */



/*----------------------------------------------------------------------------*/



void tk_cleanup( char outf[], STATUS *status )

/* after redraw (not used in this channel)
 *
 * parameters of routine
 * char       outf[];     output; always empty
 * STATUS     *status;    output; return status
 */
{
	/* executable code */

	*outf = '\0';

} /* end of tk_cleanup */



/*----------------------------------------------------------------------------*/
/*                            local routines                                  */
/*----------------------------------------------------------------------------*/



void tkh_init( void )

/* initialises calcomp channel
 *
 * no parameters
 */
{
	/* local variables */
	int      unit;     /* file unit */
	float    x, y;     /* position */
	$DESCRIPTOR( instream, "SYS$INPUT" );            /* string SYS$INPUT */
	$DESCRIPTOR( infile, "SYS$LOGIN:TK$INIT.000" );  /* input file */
	FILE     *rf;      /* input file */

	/* executable code */

	/* FORTRAN routine */
	unit = 53;
	x = y = 0.0;

	rf = fopen( infile.dsc$a_pointer, "w" );
	if  (rf == NULL)  {
		plots( &unit, &x, &y );
	} else {
		fprintf( rf, "0\n" );
		fclose( rf );
		lib$set_logical( &instream, &infile );
		plots( &unit, &x, &y );
		lib$delete_logical( &instream );
	} /*endif*/
	tkv_isinit = TRUE;

} /* end of tkh_init */



/*----------------------------------------------------------------------------*/



void tkh_close( void )

/* closes calcomp channel
 *
 * no parameters
 */
{
	/* local variables */
	float    x, y;           /* position */
	int      code;           /* plot subfunction code */

	/* executable code */

	x = y = 0.0;
	code = TKC_PLOTEND;
	plot( &x, &y, &code );

} /* end of tkh_close */



/*----------------------------------------------------------------------------*/



void tkh_trafo( TKCOO ux, TKCOO uy, TKOUT *dx, TKOUT *dy )

/* transforms user coordinates (ux,uy) to device coordinates (dx,dy)
 *
 * parameters of routine
 * TKCOO      ux, uy;     input; user coordinates
 * TKOUT      dx, dy;     output; device coordinates
 */
{
	/* executable code */

	*dx = ux * tkv_trafo_xf + tkv_trafo_xo;
	*dy = uy * tkv_trafo_yf + tkv_trafo_yo;

} /* end of tkh_trafo */



/*----------------------------------------------------------------------------*/

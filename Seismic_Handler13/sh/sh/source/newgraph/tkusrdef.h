
/* file TKUSRDEF.H
 *      ==========
 *
 * version 3, 6-Oct-92
 *
 * interface of Calcomp channel to graphics module
 * K. Stammler, 1-Oct-92
 */

#ifndef __TKUSRDEF
#define __TKUSRDEF


typedef float TKCOO;
typedef float TKOUT;


/* macros */
#define tk_alphamode anmode
   /* switch to alphanumeric mode */

/* not implemented routines */
#define tk_set_outputdir(d,s)
#define tk_set_inputdir(d,s)
#define tk_wrtch(c)
#define tk_setpar(i,v,s)


#ifdef BC_G_TEK


/*----------------------------------------------------------------------------*/


void tk_init( int attribs, TKOUT xlo, TKOUT ylo, TKOUT width, TKOUT height,
   STATUS *status );

/* initialises calcomp channel
 *
 * parameters of routine
 * int        attribs;        input; attributes
 * TKOUT      xlo, ylo;       input; offset of output
 * TKOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void tk_exit( void );

/* exits calcomp channel
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/



#define tk_finish tk_exit



/*----------------------------------------------------------------------------*/


void tk_resize( TKOUT xlo, TKOUT ylo, TKOUT width, TKOUT height,
   STATUS *status );

/* resizes output
 *
 * parameters of routine
 * TKOUT      xlo, ylo;       input; offset of output
 * TKOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void tk_erase( void );

/* clears calcomp output channel
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void tk_setcoo( TKCOO x, TKCOO y, TKCOO w, TKCOO h, STATUS *status );

/* sets user coordinates
 *
 * parameters of routine
 * TKCOO      x, y;     input; offset
 * TKCOO      w, h;     input; size
 * STATUS     *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void tk_moveto( TKCOO x, TKCOO y );

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * TKCOO      x, y;    input; position
 */


/*----------------------------------------------------------------------------*/


void tk_drawto( int style, TKCOO x, TKCOO y );

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        style;   input; style parameter
 * TKCOO      x, y;    input; position
 */


/*----------------------------------------------------------------------------*/


void tk_arrayplot( int style, long cnt, int red, TKCOO xoff,
   TKCOO xinc, TKCOO yoff, TKCOO yarr[], float yzoom, STATUS *status );

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


/*----------------------------------------------------------------------------*/


void tk_text( int style, TKCOO x, TKCOO y, char text[] );

/* prints text at position (x,y)
 *
 * parameters of routine
 * int        style;     input; style parameter (size)
 * TKCOO      x, y;      input; text position (user coordinates)
 * char       text[];    input; text to be printed
 */


/*----------------------------------------------------------------------------*/


void tk_write( char text[] );

/* write text to current write position
 *
 * parameters of routine
 * char       text[];     input; text to be printed
 */


/*----------------------------------------------------------------------------*/


void tk_setstyle( int style, char item[], char value[], STATUS *status );

/* sets style parameter number "style"
 *
 * parameters of routine
 * int        style;     input; number of style
 * char       item[];    input; name of style attribute
 * char       value[];   input; new value of style attribute (as string expr.)
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void tk_charsize( int style, float size, STATUS *status );

/* sets character size in units of display height
 *
 * parameters of routine
 * int        style;     input; style number
 * float      size;      input; size of character in units of display height
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void tk_linestyle( int style, int linestyle, STATUS *status );

/* sets line linestyle in style block number "style"
 *
 * parameters of routine
 * int        style;      input; style block number
 * int        linestyle;  input; number of line style
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


float tk_aspectratio();

/* returns ratio of width to height
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void tk_getloc( TKCOO *x, TKCOO *y, char *ch );

/* returns user selected position
 *
 * parameters of routine
 * TKCOO      *x, *y;      output; selected position (in user coo)
 * char       *ch;         output; key pressed
 */


/*----------------------------------------------------------------------------*/


void tk_arrayswap( BOOLEAN on_off );

/* switches array-swap on ot off
 *
 * parameter of routine
 * BOOLEAN   on_off;     TRUE=on, FALSE=off
 */


/*----------------------------------------------------------------------------*/


void tk_prepare( STATUS *status );

/* prepare redraw
 *
 * parameters of routine
 * STATUS     *status;     output;  return status
 */


/*----------------------------------------------------------------------------*/


void tk_cleanup( char outf[], STATUS *status );

/* after redraw (not used in this channel)
 *
 * parameters of routine
 * char       outf[];     output; always empty
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


#else /* BC_G_TEK */

#define tk_init(a,x,y,w,h,s)
#define tk_exit()
#define tk_finish()
#define tk_resize(x,y,w,h,s)
#define tk_erase()
#define tk_setcoo(x,y,w,h,s)
#define tk_moveto(x,y)
#define tk_drawto(s,x,y)
#define tk_arrayplot(a,c,r,x,i,o,y,z,s)
#define tk_text(s,x,y,t)
#define tk_setstyle(a,i,v,s)
#define tk_prepare(s)
#define tk_cleanup(o,s)
#define tk_charsize(a,z,s)
#define tk_linestyle(a,l,s)
#define tk_color(a,r,g,b,s)
#define tk_write(t)
#define tk_getloc(x,y,c)
#define tk_aspectratio() 1.0
#define tk_arrayswap(b)

#endif /* BC_G_TEK */


#endif /* __TKUSRDEF */


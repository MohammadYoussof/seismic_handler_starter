
/* file CCUSRDEF.H
 *      ==========
 *
 * version 4, 30-Sep-92
 *
 * interface of Calcomp channel to graphics module
 * K. Stammler, 23-Sep-92
 */

#ifndef __CCUSRDEF
#define __CCUSRDEF


typedef float CCCOO;
typedef float CCOUT;


#ifdef BC_G_CALCOMP


/*----------------------------------------------------------------------------*/


void cc_init( int attribs, CCOUT xlo, CCOUT ylo, CCOUT width, CCOUT height,
   STATUS *status );

/* initialises calcomp channel
 *
 * parameters of routine
 * int        attribs;        input; attributes
 * CCOUT      xlo, ylo;       input; offset of output
 * CCOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void cc_exit( void );

/* exits calcomp channel
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/



#define cc_finish cc_exit



/*----------------------------------------------------------------------------*/


void cc_resize( CCOUT xlo, CCOUT ylo, CCOUT width, CCOUT height,
   STATUS *status );

/* resizes output of calcomp
 *
 * parameters of routine
 * CCOUT      xlo, ylo;       input; offset of output
 * CCOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void cc_erase( void );

/* clears calcomp output channel
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void cc_setcoo( CCCOO x, CCCOO y, CCCOO w, CCCOO h, STATUS *status );

/* sets user coordinates
 *
 * parameters of routine
 * CCCOO      x, y;     input; offset
 * CCCOO      w, h;     input; size
 * STATUS     *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void cc_moveto( CCCOO x, CCCOO y );

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * CCCOO      x, y;    input; position
 */


/*----------------------------------------------------------------------------*/


void cc_drawto( int style, CCCOO x, CCCOO y );

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        style;   input; style parameter
 * CCCOO      x, y;    input; position
 */


/*----------------------------------------------------------------------------*/


void cc_arrayplot( int style, long cnt, int red, CCCOO xoff,
   CCCOO xinc, CCCOO yoff, CCCOO yarr[], float yzoom, STATUS *status );

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


/*----------------------------------------------------------------------------*/


void cc_text( int style, CCCOO x, CCCOO y, char text[] );

/* prints text at position (x,y)
 *
 * parameters of routine
 * int        style;     input; style parameter (size)
 * CCCOO      x, y;      input; text position (user coordinates)
 * char       text[];    input; text to be printed
 */


/*----------------------------------------------------------------------------*/


void cc_setstyle( int style, char item[], char value[], STATUS *status );

/* sets style parameter number "style"
 *
 * parameters of routine
 * int        style;     input; number of style
 * char       item[];    input; name of style attribute
 * char       value[];   input; new value of style attribute (as string expr.)
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void cc_set_outputdir( char dir[], STATUS *status );

/* sets output directory
 *
 * parameters of routine
 * char       dir[];        input; new directory path
 * STATUS     *status;      output; return status
 */


/*----------------------------------------------------------------------------*/



#define cc_set_inputdir(d,s)



/*----------------------------------------------------------------------------*/


void cc_prepare( STATUS *status );

/* prepares hardcopy
 *
 * parameters of routine
 * STATUS     *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void cc_cleanup( char outf[], STATUS *status );

/* finishes hardcopy
 *
 * parameters of routine
 * char       outf[];      output; output filename
 * STATUS     *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void cc_setpar( char item[], char value[], STATUS *status );

/* sets hardcopy parameters
 *
 * parameters of routine
 * char       item[];     input; item to be set
 * char       value[];    input; new value of item
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void cc_charsize( int style, float size, STATUS *status );

/* sets character size in units of display height
 *
 * parameters of routine
 * int        style;     input; style number
 * float      size;      input; size of character in units of display height
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void cc_linestyle( int style, int linestyle, STATUS *status );

/* sets line linestyle in style block number "style"
 *
 * parameters of routine
 * int        style;      input; style block number
 * int        linestyle;  input; number of line style
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void cc_color( int style, float red, float green, float blue, STATUS *status );

/* sets color of style block "style"
 *
 * parameters of routine
 * int        style;            input; style block number
 * float      red, green, blue; input; red, green and blue intensities (0..1)
 * STATUS     *status;          output; return status
 */


/*----------------------------------------------------------------------------*/


float cc_aspectratio( void );

/* returns ratio of width to height of output
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void cc_arrayswap( BOOLEAN on_off );

/* switches array-swap on ot off
 *
 * parameter of routine
 * BOOLEAN   on_off;     TRUE=on, FALSE=off
 */


/*----------------------------------------------------------------------------*/


#else  /* BC_G_CALCOMP */


#define cc_init(a,x,y,w,h,s)
#define cc_exit()
#define cc_finish()
#define cc_resize(x,y,w,h,s)
#define cc_erase()
#define cc_setcoo(x,y,w,h,s)
#define cc_moveto(x,y)
#define cc_drawto(s,x,y)
#define cc_arrayplot(a,c,r,x,i,o,y,z,s)
#define cc_text(s,x,y,t)
#define cc_setstyle(a,i,v,s)
#define cc_set_outputdir(d,s)
#define cc_set_inputdir(d,s)
#define cc_prepare(s)
#define cc_cleanup(o,s)
#define cc_setpar(i,v,s)
#define cc_charsize(a,z,s)
#define cc_linestyle(a,l,s)
#define cc_color(a,r,g,b,s)
#define cc_aspectratio() 1.0
#define cc_arrayswap(b)


#endif  /* BC_G_CALCOMP */

#endif /* __CCUSRDEF */


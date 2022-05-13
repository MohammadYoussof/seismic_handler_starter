
/* file HPUSRDEF.H
 *      ==========
 *
 * version 1, 24-Sep-92
 *
 * interface of HPGL channel to graphics module
 * K. Stammler, 24-Sep-92
 */

#ifndef __HPUSRDEF
#define __HPUSRDEF


typedef float HPCOO;
typedef int HPOUT;


#ifdef BC_G_HPGL


/*----------------------------------------------------------------------------*/


void hp_init( int attribs, float xlo, float ylo, float width, float height,
   STATUS *status );

/* initialises calcomp channel
 *
 * parameters of routine
 * int        attribs;        input; attributes
 * float      xlo, ylo;       input; offset of output
 * float      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void hp_exit( void );

/* exits calcomp channel
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/



#define hp_finish hp_exit



/*----------------------------------------------------------------------------*/


void hp_resize( float xlo, float ylo, float width, float height,
   STATUS *status );

/* resizes output of calcomp
 *
 * parameters of routine
 * float      xlo, ylo;       input; offset of output
 * float      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void hp_erase( void );

/* clears calcomp output channel
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void hp_setcoo( HPCOO x, HPCOO y, HPCOO w, HPCOO h, STATUS *status );

/* sets user coordinates
 *
 * parameters of routine
 * HPCOO      x, y;     input; offset
 * HPCOO      w, h;     input; size
 * STATUS     *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void hp_moveto( HPCOO x, HPCOO y );

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * HPCOO      x, y;    input; position
 */


/*----------------------------------------------------------------------------*/


void hp_drawto( int style, HPCOO x, HPCOO y );

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        style;   input; style parameter
 * HPCOO      x, y;    input; position
 */


/*----------------------------------------------------------------------------*/


void hp_arrayplot( int style, long cnt, int red, HPCOO xoff,
   HPCOO xinc, HPCOO yoff, HPCOO yarr[], float yzoom, STATUS *status );

/* plots array of data points
 *
 * parameters of routine
 * int        style;       input; style parameter
 * long       cnt;         input; number of data samples in yarr
 * int        red;         input; reduction factor
 * HPCOO      xoff;        input; x offset
 * HPCOO      xinc;        input; x increment
 * HPCOO      yoff;        input; y offset
 * HPCOO      yarr[];      input; data array
 * float      yzoom;       input; amplitude zoom factor
 * STATUS     *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void hp_text( int style, HPCOO x, HPCOO y, char text[] );

/* prints text at position (x,y)
 *
 * parameters of routine
 * int        style;     input; style parameter (size)
 * HPCOO      x, y;      input; text position (user coordinates)
 * char       text[];    input; text to be printed
 */


/*----------------------------------------------------------------------------*/


void hp_setstyle( int style, char item[], char value[], STATUS *status );

/* sets style parameter number "style"
 *
 * parameters of routine
 * int        style;     input; number of style
 * char       item[];    input; name of style attribute
 * char       value[];   input; new value of style attribute (as string expr.)
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void hp_set_outputdir( char dir[], STATUS *status );

/* sets output directory
 *
 * parameters of routine
 * char       dir[];        input; new directory path
 * STATUS     *status;      output; return status
 */


/*----------------------------------------------------------------------------*/


void hp_set_inputdir( char dir[], STATUS *status );

/* sets input directory
 *
 * parameters of routine
 * char       dir[];        input; new directory path
 * STATUS     *status;      output; return status
 */


/*----------------------------------------------------------------------------*/


void hp_prepare( STATUS *status );

/* prepares hardcopy
 *
 * parameters of routine
 * STATUS     *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void hp_cleanup( char outf[], STATUS *status );

/* finishes hardcopy
 *
 * parameters of routine
 * char       outf[];      output; output filename
 * STATUS     *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void hp_setpar( char item[], char value[], STATUS *status );

/* sets hardcopy parameters
 *
 * parameters of routine
 * char       item[];     input; item to be set
 * char       value[];    input; new value of item
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void hp_charsize( int style, float size, STATUS *status );

/* sets character size in units of display height
 *
 * parameters of routine
 * int        style;     input; style number
 * float      size;      input; size of character in units of display height
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void hp_linestyle( int style, int linestyle, STATUS *status );

/* sets line linestyle in style block number "style"
 *
 * parameters of routine
 * int        style;      input; style block number
 * int        linestyle;  input; number of line style
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void hp_color( int style, float red, float green, float blue, STATUS *status );

/* sets color of style block "style"
 *
 * parameters of routine
 * int        style;            input; style block number
 * float      red, green, blue; input; red, green and blue intensities (0..1)
 * STATUS     *status;          output; return status
 */


/*----------------------------------------------------------------------------*/


#define hp_aspectratio() 1.0

#ifdef XXX
float hp_aspectratio( void );

/* returns ratio of width to height of output
 * no parameters
 */
#endif


/*----------------------------------------------------------------------------*/


void hp_arrayswap( BOOLEAN on_off );

/* switches array-swap on ot off
 *
 * parameter of routine
 * BOOLEAN   on_off;     TRUE=on, FALSE=off
 */


/*----------------------------------------------------------------------------*/


#else  /* BC_G_CALCOMP */


#define hp_init(a,x,y,w,h,s)
#define hp_exit()
#define hp_finish()
#define hp_resize(x,y,w,h,s)
#define hp_erase()
#define hp_setcoo(x,y,w,h,s)
#define hp_moveto(x,y)
#define hp_drawto(s,x,y)
#define hp_arrayplot(a,c,r,x,i,o,y,z,s)
#define hp_text(s,x,y,t)
#define hp_setstyle(a,i,v,s)
#define hp_set_outputdir(d,s)
#define hp_set_inputdir(d,s)
#define hp_prepare(s)
#define hp_cleanup(o,s)
#define hp_setpar(i,v,s)
#define hp_charsize(a,z,s)
#define hp_linestyle(a,l,s)
#define hp_color(a,r,g,b,s)
#define hp_aspectratio() 1.0
#define hp_arrayswap(b)


#endif  /* BC_G_HPGL */

#endif /* __HPUSRDEF */



/* file PSUSRDEF.H
 *      ==========
 *
 * version 4, 15-Sep-92
 *
 * interface of PostScript channel to graphics module
 * K. Stammler, 12-AUG-91
 */

#ifndef __PSUSRDEF
#define __PSUSRDEF


typedef float PSOUT;


#ifdef BC_G_POSTSCRIPT


/*----------------------------------------------------------------------------*/


void ps_init( int attribs, PSOUT xlo, PSOUT ylo, PSOUT width, PSOUT height,
   STATUS *status );

/* initialises PostScript channel
 *
 * parameters of routine
 * int        attribs;        input; attributes
 * PSOUT      xlo, ylo;       input; offset of output
 * PSOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void ps_exit( void );

/* exits PostScript channel
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


#define ps_finish ps_exit


/*----------------------------------------------------------------------------*/


void ps_resize( PSOUT xlo, PSOUT ylo, PSOUT width, PSOUT height,
   STATUS *status );

/* resizes output of PostScript
 *
 * parameters of routine
 * PSOUT      xlo, ylo;       input; offset of output
 * PSOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void ps_erase( void );

/* clears PostScript output channel
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void ps_prepare( STATUS *status );

/* prepares hardcopy
 *
 * parameters of routine
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void ps_cleanup( char outf[], STATUS *status );

/* finishes hardcopy
 *
 * parameters of routine
 * char       outf[];     output; output filename
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void ps_setcoo( GBC_COO x, GBC_COO y, GBC_COO w, GBC_COO h, STATUS *status );

/* sets user coordinates
 *
 * parameters of routine
 * GBC_COO    x, y;     input; offset
 * GBC_COO    w, h;     input; size
 * STATUS     *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void ps_moveto( GBC_COO x, GBC_COO y );

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * GBC_COO    x, y;    input; position
 */


/*----------------------------------------------------------------------------*/


void ps_drawto( int style, GBC_COO x, GBC_COO y );

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        style;   input; style parameter
 * GBC_COO    x, y;    input; position
 */


/*----------------------------------------------------------------------------*/


void ps_arrayplot( int style, long cnt, int red, GBC_COO xoff,
   GBC_COO xinc, GBC_COO yoff, GBC_COO yarr[], float yzoom, STATUS *status );

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


/*----------------------------------------------------------------------------*/


void ps_text( int style, GBC_COO x, GBC_COO y, char text[] );

/* prints text at position (x,y)
 *
 * parameters of routine
 * int        style;     input; style parameter (size)
 * GBC_COO    x, y;      input; text position (user coordinates)
 * char       text[];    input; text to be printed
 */


/*----------------------------------------------------------------------------*/


void ps_setstyle( int style, char item[], char value[], STATUS *status );

/* sets style parameter number "style"
 *
 * parameters of routine
 * int        style;     input; number of style
 * char       item[];    input; name of style attribute
 * char       value[];   input; new value of style attribute (as string expr.)
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void ps_set_outputdir( char dir[], STATUS *status );

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name of scratch path
 * STATUS    *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void ps_set_inputdir( char dir[], STATUS *status );

/* sets input directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name
 * STATUS    *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


#define ps_charsize(s,x,t)
#ifdef XXX
void ps_charsize( int style, float size, STATUS *status );

/* sets character size in units of display height
 *
 * parameters of routine
 * int        style;     input; style number
 * float      size;      input; size of character in units of display height
 * STATUS     *status;   output; return status
 */
#endif

/*----------------------------------------------------------------------------*/


#define ps_linestyle(s,l,t)
#ifdef XXX
void ps_linestyle( int style, int linestyle, STATUS *status );

/* sets line linestyle in style block number "style"
 *
 * parameters of routine
 * int        style;      input; style block number
 * int        linestyle;  input; number of line style
 * STATUS     *status;    output; return status
 */
#endif

#define ps_linewidth(s,l,t)

/*----------------------------------------------------------------------------*/


#define ps_color(s,r,g,b,t)
#ifdef XXX
void ps_color( int style, float red, float green, float blue, STATUS *status );

/* sets color of style block "style"
 *
 * parameters of routine
 * int        style;            input; style block number
 * float      red, green, blue; input; red, green and blue intensities (0..1)
 * STATUS     *status;          output; return status
 */
#endif

/*----------------------------------------------------------------------------*/


void ps_setpar( char item[], char value[], STATUS *status );

/* sets hardcopy parameters
 *
 * parameters of routine
 * char       item[];     input; item to be set
 * char       value[];    input; new value of item
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


#define ps_aspectratio() 1.0
#ifdef XXX
float ps_aspectratio();

/* returns ratio of width to height of output
 * no parameters
 */
#endif

/*----------------------------------------------------------------------------*/


void ps_arrayswap( BOOLEAN on_off );

/* switches array-swap on or off
 *
 * parameter of routine
 * BOOLEAN   on_off;     TRUE=on, FALSE=off
 */


/*----------------------------------------------------------------------------*/



#else  /* BC_G_POSTSCRIPT */



#define ps_init(a,x,y,w,h,s)
#define ps_exit()
#define ps_finish()
#define ps_resize(x,y,w,h,s)
#define ps_erase()
#define ps_prepare(s)
#define ps_cleanup(f,s)
#define ps_setcoo(x,y,w,h,s)
#define ps_moveto(x,y)
#define ps_drawto(s,x,y)
#define ps_arrayplot(s,c,r,o,i,f,y,z,x)
#define ps_text(s,x,y,t)
#define ps_setstyle(s,i,v,x)
#define ps_set_outputdir(d,s)
#define ps_set_inputdir(d,s)

#define ps_charsize(s,x,t)
#define ps_linestyle(s,l,t)
#define ps_linewidth(s,l,t)
#define ps_color(s,r,g,b,t)
#define ps_setpar(i,v,s)
#define ps_aspectratio() 1.0
#define ps_arrayswap(o)



#endif /* BC_G_POSTSCRIPT */

#endif /* __PSUSRDEF */


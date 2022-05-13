
/* file XWUSRDEF.H
 *      ==========
 *
 * version 3, 3-Sep-93
 *
 * user interface of graphics module
 * K. Stammler, 6-AUG-1990
 */

#ifndef __XWUSRDEF
#define __XWUSRDEF


#ifdef BC_G_XWINDOW


/* graphics */

#define XWC_MAXWDW 7
   /* maximum number of windows available */
#define XWC_CYCLEWDW -1
   /* cycle windows (parameter in xw_popwdw) */

/* window attribute flags */
#define XWF_WINPUT 0x40
   /* window is used as input window frequently */


/* types */
typedef float XWCOO;      /* user coordinates */
typedef unsigned int WDW;   /* window number */

/* not implemented */
#define xw_set_outputdir(d,s)


/*------------*/
/* prototypes */
/*----------------------------------------------------------------------------*/


void xw_init( WDW wdw, int attribs, float xlo, float ylo,
   float width, float height, STATUS *status );

/* initialises graphic channels
 *
 * parameters of routine
 * WDW        wdw;            input; channels to be initialised
 * int        attribs;        input; window attributes
 * float      xlo, ylo;       input; position of window (in cm)
 * float      width, height;  input; size of window (in cm)
 * STATUS     *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void xw_exit( WDW wdw );

/* exits from graphic channel(s)
 *
 * parameters of routine
 * WDW         wdw;     input; window number
 */


/*----------------------------------------------------------------------------*/


void xw_finish( void );

/* closes all open graphic channels
 *
 * no parameters
 */

/*----------------------------------------------------------------------------*/


void xw_resizewdw( WDW wdw, float x, float y, float w, float h,
   STATUS *status );

/* resizes and repositions window
 *
 * parameters of routine
 * WDW        wdw;       input; window number
 * float      x, y;      input; new position of window (in cm)
 * float      w, h;      input; new size of window (in cm)
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void xw_popwdw( WDW wdw );

/* pops window on top of the others
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 */


/*----------------------------------------------------------------------------*/


void xw_pushwdw( WDW wdw );

/* pushes window below the others
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 */


/*----------------------------------------------------------------------------*/


void xw_erase( WDW wdw );

/* clears channel(s)
 *
 * parameters of routine
 * WDW        wdw;     input; window number
 */


/*----------------------------------------------------------------------------*/


void xw_setwdwname( WDW wdw, char name[] );

/* set new name of window
 *
 * parameters of routine
 * WDW       wdw;       input; window number
 * char      name[];    input; new name
 */


/*----------------------------------------------------------------------------*/


void xw_setcoo( WDW wdw, XWCOO x, XWCOO y, XWCOO w, XWCOO h,
   STATUS *status );

/* sets new user coordinate system
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * XWCOO      x, y;     input; origin of user coordinates
 * XWCOO      w, h;     input; size of user coordinates
 * STATUS     *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void xw_moveto( WDW wdw, XWCOO x, XWCOO y );

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * XWCOO      x, y;     input; position to be moved to
 */


/*----------------------------------------------------------------------------*/


void xw_drawto( WDW wdw, int style, XWCOO x, XWCOO y );

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * int        style;    input; style block number
 * XWCOO      x, y;     input; position to be moved to
 */


/*----------------------------------------------------------------------------*/


void xw_arrayplot( WDW wdw, int style, long cnt, int red, XWCOO xoff,
   XWCOO xinc, XWCOO yoff, XWCOO yarr[], XWCOO yzoom, STATUS *status );

/* plots an array of data
 *
 * parameters of routine
 * WDW        wdw;       input; window number
 * int        style;     input; style block number
 * long       cnt;       input; length of data array
 * int        red;       input; reduction factor
 * XWCOO      xoff;      input; x-position of first sample
 * XWCOO      xinc;      input; x increment
 * XWCOO      yoff;      input; y-position of first sample
 * XWCOO      yarr[];    input; data array
 * float      yzoom;     input; zoom factor in y-direction
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void xw_text( WDW wdw, int style, XWCOO x, XWCOO y, char text[] );

/* writes text at position (x,y)
 *
 * parameters of routine
 * WDW        wdw;       input; window number
 * int        style;     input; style block number
 * XWCOO      x, y;      input; position of text
 * char       text[];    input; text to be written
 */


/*----------------------------------------------------------------------------*/


void xw_wrtch( WDW wdw, char ch );

/* writes single character to current write position
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * char       ch;       input; character to be written
 */


/*----------------------------------------------------------------------------*/


void xw_write( WDW wdw, char text[] );

/* writes text at the current write position
 *
 * parameters of routine
 * WDW        wdw;    input; window number
 * char       text[]; input; text to be written
 */


/*----------------------------------------------------------------------------*/


void xw_read( WDW wdw, int maxlth, char text[] );

/* reads text string from window
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * int        maxlth;   input; maximum length of string
 * char       text[];   output; string read
 */


/*----------------------------------------------------------------------------*/


void xw_getloc( WDW wdw, XWCOO *x, XWCOO *y, char *ch );

/* returns user selected mouse position and key
 *
 * parameters of routine
 * WDW        wdw;          input; window number
 * XWCOO      *x, *y;       output; selected position
 * char       *ch;          output; selected key (mouse button = '@')
 */


/*----------------------------------------------------------------------------*/


void xw_setstyle( WDW wdw, int style, char item[], char value[],
   STATUS *status );

/* sets attribute in style block number "style"
 *
 * parameters of routine
 * WDW        wdw;       input; window number
 * int        style;     input; style block number
 * char       item[];    input; description of attribute
 * char       value[];   input; new value of attribute (as string expression)
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void xw_charsize( WDW wdw, int style, float size, STATUS *status );

/* sets character size in units of display height
 *
 * parameters of routine
 * WDW        wdw;       input; window ID
 * int        style;     input; style number
 * float      size;      input; size of character in units of display height
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void xw_linestyle( WDW wdw, int style, int linestyle, STATUS *status );

/* sets line linestyle in style block number "style"
 *
 * parameters of routine
 * WDW        wdw;        input; window ID
 * int        style;      input; style block number
 * int        linestyle;  input; number of line style
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void xw_linewidth( WDW wdw, int style, int width, STATUS *status );

/* sets line width in pixels in style block "style"
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * int        style;    input; style block number
 * int        width;    input; width of line in pixels
 * STATUS     *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void xw_color( WDW wdw, int style, float red, float green, float blue,
   STATUS *status );

/* sets color of style block "style"
 *
 * parameters of routine
 * WDW        wdw;       input; window ID
 * int        style;            input; style block number
 * float      red, green, blue; input; red, green and blue intensities (0..1)
 * STATUS     *status;          output; return status
 */


/*----------------------------------------------------------------------------*/


XWCOO xw_chheight( WDW wdw );

/* returns char height in user coordinates of last used style
 *
 * parameters of routine
 * WDW        wdw;     input; window number
 */


/*----------------------------------------------------------------------------*/


void xw_prepare( WDW wdw, STATUS *status );

/* prepares window for redraw
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * STATUS     *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void xw_cleanup( WDW wdw, char outf[], STATUS *status );

/* cleanup after redraw.  not used here
 *
 * parameters of routine
 * WDW        wdw;     input; window number
 * char       outf[];  output; name of output file
 * STATUS     *status; output; return status
 */


/*----------------------------------------------------------------------------*/


void xw_redraw( WDW wdw, STATUS *status );

/* redraws window
 *
 * parameters of routine
 * WDW        wdw;      input; window number
 * STATUS     *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


float xw_aspectratio( WDW wdw );

/* returns ratio of width to height
 *
 * parameter of routine
 * WDW       wdw;       input; window number
 */


/*----------------------------------------------------------------------------*/


void xw_updatewdw( void );

/* updates windows if necessary
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void xw_flushbuffers( void );

/* flushes all output buffers of graphic windows
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void xw_arrayswap( BOOLEAN on_off );

/* switches arrayswap (swapping of x and y in xw_arrayplot) on or off
 *
 * parameters of routine
 * BOOLEAN    on_off;     input; TRUE=on, FALSE=off
 */


/*----------------------------------------------------------------------------*/


void xw_set_inputdir( char dir[], STATUS *status );

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name
 * STATUS    *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


#else  /* BC_G_XWINDOW */


#define xw_init(n,a,x,y,w,h,s)
#define xw_exit(w)
#define xw_finish()
#define xw_resizewdw(n,x,y,w,h,s)
#define xw_popwdw(w)
#define xw_pushwdw(w)
#define xw_erase(w)
#define xw_setwdwname(w,n)
#define xw_setcoo(n,x,y,w,h,s)
#define xw_moveto(w,x,y)
#define xw_drawto(w,s,x,y)
#define xw_arrayplot(w,s,c,r,o,i,y,a,z,v)
#define xw_text(w,s,x,y,t)
#define xw_wrtch(w,c)
#define xw_write(w,t)
#define xw_read(w,m,t)
#define xw_getloc(w,x,y,c)
#define xw_setstyle(w,s,i,v,t)
#define xw_charsize(w,s,z,t)
#define xw_linestyle(w,s,l,t)
#define xw_linewidth(w,s,d,t)
#define xw_color(w,s,r,g,b,t)
#define xw_chheight(w) 1
#define xw_prepare(w,s)
#define xw_cleanup(w,o,s)
#define xw_redraw(w,s)
#define xw_aspectratio(w) 1.0
#define xw_updatewdw()
#define xw_flushbuffers()
#define xw_arrayswap(f)
#define xw_set_outputdir(d,s)
#define xw_set_inputdir(d,s)


#endif /* BC_G_XWINDOW */

#endif /* __XWUSRDEF */

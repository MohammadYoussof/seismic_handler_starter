
/* file VWUSRDEF.H
 *      ==========
 *
 * version 3, 30-Sep-92
 *
 * user interface to VWCH.C
 * K. Stammler, 10-SEP-1990
 */

#ifndef __VWUSRDEF
#define __VWUSRDEF

#define VWC_MAXWDW 7
   /* maximum number of windows */
#define VWC_MAXSTYLE 10
   /* maximum number of styles */

typedef int VWDW;
   /* window type */
typedef float VWCOO;
   /* window coordinates */


/* not implemented routines */
#define vw_wrtch(w,c) vw_not_implemented()
#define vw_write(w,t) vw_not_implemented()
#define vw_read(w,m,t) vw_not_implemented()
#define vw_set_inputdir(d,s)



#ifdef BC_G_VWS



/*----------------------------------------------------------------------------*/


void vw_init( VWDW wdw, int attrib, float xlo, float ylo, float width,
   float height, STATUS *status );

/* initialises VWS graphics if necessary and opens window
 *
 * parameters of routine
 * VWDW       wdw;           input; window number
 * int        attribs;       input; window attributes
 * float      xlo, ylo;      input; position of window (in cm)
 * float      width, height; input; size of window (in cm)
 * STATUS     *status;       output; return status
 */


/*----------------------------------------------------------------------------*/


void vw_exit( VWDW wdw );

/* closes window
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 */


/*----------------------------------------------------------------------------*/


void vw_finish( void );

/* closes all windows
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void vw_popwdw( VWDW wdw );

/* pops window on top
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 */


/*----------------------------------------------------------------------------*/


void vw_pushwdw( VWDW wdw );

/* pushes window down
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 */


/*----------------------------------------------------------------------------*/


void vw_resizewdw( VWDW wdw, float x, float y, float w, float h,
   STATUS *status );

/* resizes window
 *
 * parameters of routine
 * float      x, y;      input; new position of window
 * float      w, h;      input; width and height
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void vw_erase( VWDW wdw );

/* clears window
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 */


/*----------------------------------------------------------------------------*/


void vw_setwdwname( VWDW wdw, char name[] );

/* sets new window name
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 * char       name[];   input; new window name
 */

/* !!! not supported in VWS !!! */


/*----------------------------------------------------------------------------*/


void vw_setcoo( VWDW wdw, VWCOO x, VWCOO y, VWCOO w, VWCOO h,
   STATUS *status );

/* sets user coordinates
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 * VWCOO      x, y;      input; origin of user coordinates
 * VWCOO      w, h;      input; size of user coordinates
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void vw_moveto( VWDW wdw, VWCOO x, VWCOO y );

/* moves the graphic position to (x,y)
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 * VWCOO      x, y;      input; new position (user coordinates)
 */


/*----------------------------------------------------------------------------*/


void vw_drawto( VWDW wdw, int style, VWCOO x, VWCOO y );

/* draws to the graphic position (x,y)
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 * int        style;     input; style block number
 * VWCOO      x, y;      input; new position (user coordinates)
 */


/*----------------------------------------------------------------------------*/


void vw_arrayplot( VWDW wdw, int style, long cnt, int red, VWCOO xoff,
   VWCOO xinc, VWCOO yoff, VWCOO yarr[], float yzoom, STATUS *status );

/* plots an array of data
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 * int        style;     input; style block number
 * long       cnt;       input; length of data array
 * int        red;       input; reduction factor
 * VWCOO      xoff;      input; x-position of first sample
 * VWCOO      xinc;      input; x increment
 * VWCOO      yoff;      input; y-position of first sample
 * VWCOO      yarr[];    input; data array
 * float      yzoom;     input; zoom factor in y-direction
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void vw_text( VWDW wdw, int style, VWCOO x, VWCOO y, char text[] );

/* writes text at position (x,y) in window wdw
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 * int        style;     input; style block number
 * VWCOO      x, y;      input; position of text
 * char       text[];    input; text to be printed
 */


/*----------------------------------------------------------------------------*/


void vw_setstyle( VWDW wdw, int style, char item[], char value[],
   STATUS *status );

/* sets attributes in attribute block
 *
 * parameters of routine
 * VWDW       wdw;       input; window number
 * int        style;     input; style block number
 * char       item[];    input; attribute name
 * char       value[];   input; new value (as string)
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void vw_charsize( VWDW wdw, int style, float size, STATUS *status );

/* sets character size in units of display height
 *
 * parameters of routine
 * VWDW       wdw;       input; window ID
 * int        style;     input; style number
 * float      size;      input; size of character in units of display height
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void vw_linestyle( VWDW wdw, int style, int linestyle, STATUS *status );

/* sets line linestyle in style block number "style"
 *
 * parameters of routine
 * VWDW       wdw;        input; window ID
 * int        style;      input; style block number
 * int        linestyle;  input; number of line style
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void vw_linewidth( VWDW wdw, int style, int width, STATUS *status );

/* sets line width in pixels in style block "style"
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 * int        style;    input; style block number
 * int        width;    input; width of line in pixels
 * STATUS     *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void vw_color( VWDW wdw, int style, float red, float green, float blue,
   STATUS *status );

/* sets color of style block "style"
 *
 * parameters of routine
 * VWDW       wdw;       input; window ID
 * int        style;            input; style block number
 * float      red, green, blue; input; red, green and blue intensities (0..1)
 * STATUS     *status;          output; return status
 */


/*----------------------------------------------------------------------------*/


void vw_set_outputdir( char dir[], STATUS *status );

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name of scratch path
 * STATUS    *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void vw_getloc( VWDW wdw, VWCOO *x, VWCOO *y, char *ch );

/* returns location selected by user
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 * VWCOO      *x, *y;   output; location selected
 * char       *ch;      output; key pressed
 */


/*----------------------------------------------------------------------------*/


void vw_cleanup( VWDW wdw, char outf[], STATUS *status );

/* makes hardcopy of window wdw
 *
 * parameters of routine
 * VWDW       wdw;        input; window number
 * char       outf[];     output; name of output file
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void vw_prepare( STATUS *status );

/* prepares hardcopy
 *
 * parameters of routine
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void vw_setpar( char item[], char value[], STATUS *status );

/* sets parameters
 *
 * parameters of routine
 * char       item[];     input; item to be set
 * char       value[];    input; new value of item
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


float vw_aspectratio( VWDW wdw );

/* returns ratio of width to height
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 */


/*----------------------------------------------------------------------------*/


VWCOO vw_chheight( VWDW wdw );

/* returns character height in user coordinates of last used style
 *
 * parameters of routine
 * VWDW       wdw;      input; window number
 */


/*----------------------------------------------------------------------------*/


void vw_arrayswap( BOOLEAN on_off );

/* switches arrayplot-swap on or off
 *
 * parameters of routine
 * BOOLEAN    on_off;    TRUE=on, FALSE=off
 */


/*----------------------------------------------------------------------------*/


void vw_not_implemented( void );

/* dummy routine
 */


/*----------------------------------------------------------------------------*/


#else  /* BC_G_VWS */

#define vw_init(v,a,x,y,w,h,s)
#define vw_exit(w)
#define vw_finish()
#define vw_popwdw(w)
#define vw_pushwdw(w)
#define vw_resizewdw(v,x,y,w,h,s)
#define vw_erase(w)
#define vw_setwdwname(w,n)
#define vw_setcoo(v,x,y,w,h,s)
#define vw_moveto(w,x,y)
#define vw_drawto(w,a,x,y)
#define vw_arrayplot(w,a,n,r,o,i,f,y,z,s)
#define vw_text(w,a,x,y,t)
#define vw_setstyle(w,a,i,v,s)
#define vw_charsize(w,a,z,s)
#define vw_linestyle(w,a,l,s)
#define vw_linewidth(v,a,w,s)
#define vw_color(w,a,r,g,b,s)
#define vw_getloc(w,x,y,c)
#define vw_prepare(s)
#define vw_cleanup(w,f,s)
#define vw_setpar(i,v,s)
#define vw_aspectratio(w) 1.0
#define vw_chheight(w) 1.0
#define vw_set_outputdir(d,s)
#define vw_arrayswap(o)
#define vw_not_implemented()


#endif /* BC_G_VWS */
#endif /* __VWUSRDEF */

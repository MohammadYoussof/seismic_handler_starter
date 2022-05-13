
/* file BGUSRDEF.H
 *      ==========
 *
 * version 4, 15-Sep-92
 *
 * prototypes of module BGICH.C
 * K. Stammler, 31-Dec-91
 */

#ifndef __BGUSRDEF
#define __BGUSRDEF


typedef float BGICOO;


#ifdef BC_G_BGI


/* not implemented */
#define bg_setpar(i,v,s)
#define bg_arrayswap(f)


/*------------------------------------------------------------------------*/


void bg_init( int wdw, int attribs, BGICOO xlo, BGICOO ylo,
	BGICOO width, BGICOO height, STATUS *status );

/* initialises GEM
 *
 * parameters of routine
 * int        wdw;       input; window to be initialised
 * int        attribs;   input; window attributes
 * BGICOO     xlo;       input; x-coo of lower left corner
 * BGICOO     ylo;       input; y-coo of lower left corner
 * BGICOO     width;     input; widths of window
 * BGICOO     height;    input; height of window
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void bg_exit( int wdw );

/* closes window
 *
 * parameters of routine
 * int        wdw;          input; window to be closed
 */


/*------------------------------------------------------------------------*/


void bg_finish( void );

/* closes all graphic channels and terminates graphics
 *
 * no parameters
 */


/*------------------------------------------------------------------------*/


void bg_resizewdw( int wdw, BGICOO xpos, BGICOO ypos,
	BGICOO width, BGICOO height, STATUS *status );

/* resizes window
 *
 * parameters of routine
 * int        wdw;            input; wdw number
 * BGICOO     xpos, ypos;     input; position of window
 * BGICOO     width, height;  input; size of window
 * STATUS     *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void bg_popwdw( int wdw );

/* pops window on top
 *
 * parameters of routine
 * int        wdw;            input; window number
 */


/*------------------------------------------------------------------------*/


void bg_setwdwname( int wdw, char name[] );

/* sets new window name
 *
 * parameters of routine
 * int       wdw;       input; window number
 * char      name[];    input; new name
 */


/*------------------------------------------------------------------------*/


void bg_setstyle( int wdw, int style, char item[], char value[],
	STATUS *status );

/* sets style attribute
 *
 * parameters of routine
 * int        wdw;       input; wdw number (dummy in the ATARI version)
 * int        style;     input; style block number
 * char       item[];    input; name of attribute
 * char       value[];   input; new value of attribute (text)
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void bg_setcoo( int wdw, BGICOO x, BGICOO y, BGICOO w, BGICOO h,
	STATUS *status );

/* sets user coordinates in window.  If x,y,w,h are all equal to zero,
 * the last call is repeated.
 *
 * parameters of routine
 * int        wdw;        input; window
 * BGICOO     x, y, w, h; input; user coordinates
 * STATUS     *status;    output; return status
 */


/*------------------------------------------------------------------------*/


float bg_aspectratio( int wdw );

/* returns ratio of width to height of window "wdw"
 *
 * parameters of routine
 * int        wdw;      input; window number
 *                      returns aspect ratio
 */


/*------------------------------------------------------------------------*/


void bg_moveto( int wdw, BGICOO x, BGICOO y );

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        wdw;      input; window
 * BGICOO     x, y;     input; location to move to
 */


/*------------------------------------------------------------------------*/


void bg_drawto( int wdw, int style, BGICOO x, BGICOO y );

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        wdw;       input; window
 * int        style;     input; attribute block number
 * BGICOO     x, y;      input; location to draw to
 */


/*------------------------------------------------------------------------*/


void bg_arrayplot( int wdw, int style, long cnt, int red, BGICOO xoff,
	BGICOO xinc, BGICOO yoff, BGICOO yarr[], BGICOO yzoom, STATUS *status );

/* plots an array of sample data
 *
 * parameters of routine
 * int        wdw;       input; output channels
 * int        style;     input; line style ID
 * long       cnt;       input; length of data array
 * int        red;       input; reduction factor for plotting
 * BGICOO     xoff;      input; x-position of first sample
 * BGICOO     xinc;      input; x increment
 * BGICOO     yoff;      input; y-position of first sample
 * BGICOO     yarr[];    input; sample data
 * BGICOO     yzoom;     input; zoom factor of sample data
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void bg_erase( int wdw );

/* clears window
 *
 * parameters of routine
 * int        wdw;       input; window
 */


/*------------------------------------------------------------------------*/


void bg_text( int wdw, int style, BGICOO x, BGICOO y, char text[] );

/* writes text to window
 *
 * parameters of routine
 * int        wdw;        input; window
 * int        style;      input; character style block number
 * BGICOO     x, y;       input; text position
 * char       text[];     input; output text
 */


/*------------------------------------------------------------------------*/


void bg_write( int wdw, char text[] );

/* writes text to window at current write position
 *
 * parameters of routine
 * int        wdw;          input; channel map
 * char       text[];       input; output text
 */


/*------------------------------------------------------------------------*/


void bg_wrtch( int wdw, char ch );

/* writes a single character to window
 *
 * parameters of routine
 * int        wdw;         input; window
 * char       ch;          input; char to be written
 */


/*------------------------------------------------------------------------*/


int bg_txtwidth( int wdw );

/* returns width of window in characters
 *
 * parameters of routine
 * int		  wdw;		 input; window number
 */


/*------------------------------------------------------------------------*/


int bg_txtheight( int wdw );

/* returns height of window in characters
 *
 * parameters of routine
 * int		  wdw;		 input; window number
 */


/*------------------------------------------------------------------------*/


void bg_read( int wdw, int maxlth, char text[] );

/* reads text from terminal
 *
 * parameters of routine
 * int      wdw;          input; window number
 * int      maxlth;       input; maximum length of text
 * char     text[];       output; text read
 */


/*------------------------------------------------------------------------*/


float bg_chheight( int wdw );

/* returns current character height in window "wdw"
 *
 * parameter of routine
 * int       wdw;      input; window number
 */


/*------------------------------------------------------------------------*/


void bg_getloc( int wdw, BGICOO *x, BGICOO *y, char *ch );

/* requests mouse position in window
 *
 * parameters of routine
 * int        wdw;        input; window
 * BGICOO     *x, *y;     output; location selected
 * char       *ch;        output; key pressed
 */


/*------------------------------------------------------------------------*/


void bg_set_outputdir( char dir[], STATUS *status );

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name of scratch path
 * STATUS    *status;   output; return status
 */


/*------------------------------------------------------------------------*/

#define bg_set_inputdir(d,s)

/*------------------------------------------------------------------------*/


void bg_prepare( int wdw, STATUS *status );

/* prepares window for redrawing
 *
 * int      wdw;      input; window
 * STATUS   *status;  output; return status
 */


/*------------------------------------------------------------------------*/


void bg_cleanup( int wdw, char outf[], STATUS *status );

/* cleanup routine after redraw
 *
 * parameters of routine
 * int        wdw;       input; window
 * char       outf[];    output; not used for this channel
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


#else /* BC_G_BGI */

/* empty definitions */
#define bg_init(c,a,x,y,w,h,s)
#define bg_exit(w)
#define bg_finish()
#define bg_resizewdw(c,x,y,w,h,s)
#define bg_popwdw(w)
#define bg_setwdwname(w,n)
#define bg_setstyle(w,s,i,v,x)
#define bg_setcoo(n,x,y,w,h,s)
#define bg_aspectratio(w) 1.0
#define bg_moveto(w,x,y)
#define bg_drawto(w,s,x,y)
#define bg_arrayplot(w,s,c,r,o,i,f,y,z,x)
#define bg_erase(w)
#define bg_text(w,s,x,y,t)
#define bg_write(w,t)
#define bg_wrtch(w,c)
#define bg_txtwidth(w) 80
#define bg_txtheight(w) 24
#define bg_read(w,m,t)
#define bg_chheight(w) 0.0
#define bg_getloc(w,x,y,c)
#define bg_set_outputdir(d,s)
#define bg_set_inputdir(d,s)
#define bg_prepare(w,s)
#define bg_cleanup(w,f,s)
#define bg_setpar(i,v,s)

#endif  /* BC_G_BGI */

#endif /* __BGUSRDEF */

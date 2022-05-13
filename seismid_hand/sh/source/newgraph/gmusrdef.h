
/* file GMUSRDEF.H
 *      ==========
 *
 * version 9, 15-Sep-92
 *
 * prototypes of module GEMCH.C
 * K. Stammler, 9-AUG-91
 */

#ifndef __GMUSRDEF
#define __GMUSRDEF


typedef float GEMCOO;


#ifdef BC_G_GEM


/* not implemented */
#define gm_setpar(i,v,s)
#define gm_arrayswap(f)


/*------------------------------------------------------------------------*/


void gm_init( int wdw, int attribs, GEMCOO xlo, GEMCOO ylo,
	GEMCOO width, GEMCOO height, STATUS *status );

/* initialises GEM
 *
 * parameters of routine
 * int        wdw;       input; window to be initialised
 * int        attribs;   input; window attributes
 * GEMCOO     xlo;       input; x-coo of lower left corner
 * GEMCOO     ylo;       input; y-coo of lower left corner
 * GEMCOO     width;     input; widths of window
 * GEMCOO     height;    input; height of window
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void gm_exit( int wdw );

/* closes window
 *
 * parameters of routine
 * int        wdw;          input; window to be closed
 */


/*------------------------------------------------------------------------*/


void gm_finish( void );

/* closes all graphic channels and terminates graphics
 *
 * no parameters
 */


/*------------------------------------------------------------------------*/


void gm_resizewdw( int wdw, GEMCOO xpos, GEMCOO ypos,
	GEMCOO width, GEMCOO height, STATUS *status );

/* resizes window
 *
 * parameters of routine
 * int        wdw;            input; wdw number
 * GEMCOO     xpos, ypos;     input; position of window
 * GEMCOO     width, height;  input; size of window
 * STATUS     *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void gm_popwdw( int wdw );

/* pops window on top
 *
 * parameters of routine
 * int        wdw;            input; window number
 */


/*------------------------------------------------------------------------*/


void gm_setwdwname( int wdw, char name[] );

/* sets new window name
 *
 * parameters of routine
 * int       wdw;       input; window number
 * char      name[];    input; new name
 */


/*------------------------------------------------------------------------*/


void gm_setstyle( int wdw, int style, char item[], char value[],
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


void gm_setcoo( int wdw, GEMCOO x, GEMCOO y, GEMCOO w, GEMCOO h,
	STATUS *status );

/* sets user coordinates in window.  If x,y,w,h are all equal to zero,
 * the last call is repeated.
 *
 * parameters of routine
 * int        wdw;        input; window
 * GEMCOO     x, y, w, h; input; user coordinates
 * STATUS     *status;    output; return status
 */


/*------------------------------------------------------------------------*/


float gm_aspectratio( int wdw );

/* returns ratio of width to height of window "wdw"
 *
 * parameters of routine
 * int        wdw;      input; window number
 *                      returns aspect ratio
 */


/*------------------------------------------------------------------------*/


void gm_moveto( int wdw, GEMCOO x, GEMCOO y );

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        wdw;      input; window
 * GEMCOO     x, y;     input; location to move to
 */


/*------------------------------------------------------------------------*/


void gm_drawto( int wdw, int style, GEMCOO x, GEMCOO y );

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        wdw;       input; window
 * int        style;     input; attribute block number
 * GEMCOO     x, y;      input; location to draw to
 */


/*------------------------------------------------------------------------*/


void gm_setpixel( int wdw, int style, GEMCOO x, GEMCOO y );

/* sets pixel at position (x,y) in user coordinates
 *
 * parameters of routine
 * int        wdw;       input; window
 * int        style;     input; style block number
 * GBC_COO    x, y;      input; location of pixel
 */


/*------------------------------------------------------------------------*/


void gm_arrayplot( int wdw, int style, long cnt, int red, GEMCOO xoff,
	GEMCOO xinc, GEMCOO yoff, GEMCOO yarr[], GEMCOO yzoom, STATUS *status );

/* plots an array of sample data
 *
 * parameters of routine
 * int        wdw;       input; output channels
 * int        style;     input; line style ID
 * long       cnt;       input; length of data array
 * int        red;       input; reduction factor for plotting
 * GEMCOO     xoff;      input; x-position of first sample
 * GEMCOO     xinc;      input; x increment
 * GEMCOO     yoff;      input; y-position of first sample
 * GEMCOO     yarr[];    input; sample data
 * GEMCOO     yzoom;     input; zoom factor of sample data
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void gm_erase( int wdw );

/* clears window
 *
 * parameters of routine
 * int        wdw;       input; window
 */


/*------------------------------------------------------------------------*/


void gm_text( int wdw, int style, GEMCOO x, GEMCOO y, char text[] );

/* writes text to window
 *
 * parameters of routine
 * int        wdw;        input; window
 * int        style;      input; character style block number
 * GEMCOO     x, y;       input; text position
 * char       text[];     input; output text
 */


/*------------------------------------------------------------------------*/


void gm_write( int wdw, char text[] );

/* writes text to window at current write position
 *
 * parameters of routine
 * int        wdw;          input; channel map
 * char       text[];       input; output text
 */


/*------------------------------------------------------------------------*/


void gm_wrtch( int wdw, char ch );

/* writes a single character to window
 *
 * parameters of routine
 * int        wdw;         input; window
 * char       ch;          input; char to be written
 */


/*------------------------------------------------------------------------*/


int gm_txtwidth( int wdw );

/* returns width of window in characters
 *
 * parameters of routine
 * int		  wdw;		 input; window number
 */


/*------------------------------------------------------------------------*/


int gm_txtheight( int wdw );

/* returns height of window in characters
 *
 * parameters of routine
 * int		  wdw;		 input; window number
 */


/*------------------------------------------------------------------------*/


void gm_read( int wdw, int maxlth, char text[] );

/* reads text from terminal
 *
 * parameters of routine
 * int      wdw;          input; window number
 * int      maxlth;       input; maximum length of text
 * char     text[];       output; text read
 */


/*------------------------------------------------------------------------*/


float gm_chheight( int wdw );

/* returns current character height in window "wdw"
 *
 * parameter of routine
 * int       wdw;      input; window number
 */


/*------------------------------------------------------------------------*/


void gm_getloc( int wdw, GEMCOO *x, GEMCOO *y, char *ch );

/* requests mouse position in window
 *
 * parameters of routine
 * int        wdw;        input; window
 * GEMCOO     *x, *y;     output; location selected
 * char       *ch;        output; key pressed
 */


/*------------------------------------------------------------------------*/


void gm_set_outputdir( char dir[], STATUS *status );

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name of scratch path
 * STATUS    *status;   output; return status
 */


/*------------------------------------------------------------------------*/

#define gm_set_inputdir(d,s)

/*------------------------------------------------------------------------*/


void gm_prepare( int wdw, STATUS *status );

/* prepares window for redrawing
 *
 * int      wdw;      input; window
 * STATUS   *status;  output; return status
 */


/*------------------------------------------------------------------------*/


void gm_cleanup( int wdw, char outf[], STATUS *status );

/* cleanup routine after redraw
 *
 * parameters of routine
 * int        wdw;       input; window
 * char       outf[];    output; not used for this channel
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/
/*                   routines for event manager                           */
/*------------------------------------------------------------------------*/


int gm_getwdw( int gem_wdw );

/* returns GC window number from GEM window number.
 * This routine is used only by the event manager
 *
 * parameter of routine
 * int       gem_wdw;    input; GEM window handle (from message buffer)
 */


/*------------------------------------------------------------------------*/


void gm_pixinc_write( int wdw, int x_inc, int y_inc );

/* changes write position of window "wdw"
 *
 * parameters of routine
 * int        wdw;          input; window number
 * int        x_inc, y_inc; input; increment of coordinates
 */


/*------------------------------------------------------------------------*/


void gm_redraw( int wdw, STATUS *status );

/* redraws window
 *
 * parameters of routine
 * int        wdw;     input; window
 * STATUS     *status; output; return status
 */


/*------------------------------------------------------------------------*/


int gm_vdid( void );
BOOLEAN gm_rsrcloaded( void );


/*------------------------------------------------------------------------*/


#ifdef BC_KS_PRIVATE

void gm_pixhardcopy( int wdw, int mag, char outfile[] );

/* makes pixel hardcopy of window (creates DeskJet file "outfile")
 *
 * parameters of routine
 * int        wdw;     input; window number
 * int        mag;     input; magnification (0=normal, 1=double size)
 * char       outfile  output; output file (DeskJet)
 */

#endif /* BC_KS_PRIVATE */


/*------------------------------------------------------------------------*/


#else /* BC_G_GEM */

/* empty definitions */
#define gm_init(c,a,x,y,w,h,s)
#define gm_exit(w)
#define gm_finish()
#define gm_resizewdw(c,x,y,w,h,s)
#define gm_popwdw(w)
#define gm_setwdwname(w,n)
#define gm_setstyle(w,s,i,v,x)
#define gm_setcoo(n,x,y,w,h,s)
#define gm_aspectratio(w) 1.0
#define gm_moveto(w,x,y)
#define gm_drawto(w,s,x,y)
#define gm_arrayplot(w,s,c,r,o,i,f,y,z,x)
#define gm_erase(w)
#define gm_text(w,s,x,y,t)
#define gm_write(w,t)
#define gm_wrtch(w,c)
#define gm_txtwidth(w) 80
#define gm_txtheight(w) 24
#define gm_read(w,m,t)
#define gm_chheight(w) 0.0
#define gm_getloc(w,x,y,c)
#define gm_set_outputdir(d,s)
#define gm_set_inputdir(d,s)
#define gm_prepare(w,s)
#define gm_cleanup(w,f,s)
#define gm_setpar(i,v,s)
#define gm_setpixel(w,s,x,y)

#endif  /* BC_G_GEM */

#endif /* __GMUSRDEF */

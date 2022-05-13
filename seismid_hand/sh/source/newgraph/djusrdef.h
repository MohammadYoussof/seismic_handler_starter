/* file DJUSRDEF.H
 *      ==========
 *
 * version 3, 25-Sep-92
 *
 * prototypes of module DJCH.C
 * K. Stammler, 20-NOV-91
 */


#ifndef __DJUSRDEF
#define __DJUSRDEF


typedef int DJOUT;


#ifdef BC_G_DESKJET


/*----------------------------------------------------------------------------*/


void dj_init( int attribs, float xlo, float ylo, float width,
	float height, STATUS *status );

/* initialises PostScript channel
 *
 * parameters of routine
 * int        attribs;        input; attributes
 * DJOUT      xlo, ylo;       input; offset of output
 * DJOUT      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void dj_exit( void );

/* exits DeskJet channel
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/



void dj_finish( void );



/*----------------------------------------------------------------------------*/


void dj_resize( float xlo, float ylo, float width, float height,
   STATUS *status );

/* resizes DeskJet output
 *
 * parameters of routine
 * float      xlo, ylo;       input; offset of output
 * float      width, height;  input; size of output
 * STATUS     *status;        output; return status
 */


/*----------------------------------------------------------------------------*/


void dj_erase( void );

/* clears DeskJet output channel
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void dj_setcoo( GBC_COO x, GBC_COO y, GBC_COO w, GBC_COO h, STATUS *status );

/* sets user coordinates
 *
 * parameters of routine
 * GBC_COO    x, y;     input; offset
 * GBC_COO    w, h;     input; size
 * STATUS     *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void dj_moveto( GBC_COO x, GBC_COO y );

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * GBC_COO    x, y;    input; position
 */


/*----------------------------------------------------------------------------*/


void dj_drawto( int style, GBC_COO x, GBC_COO y );

/* draws to position (x,y) in user coordinates
 *
 * parameters of routine
 * int        style;   input; style parameter
 * GBC_COO    x, y;    input; position
 */


/*----------------------------------------------------------------------------*/


void dj_arrayplot( int style, long cnt, int red, GBC_COO xoff,
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


void dj_text( int style, GBC_COO x, GBC_COO y, char text[] );

/* prints text at position (x,y)
 *
 * parameters of routine
 * int        style;     input; style parameter (size)
 * GBC_COO    x, y;      input; text position (user coordinates)
 * char       text[];    input; text to be printed
 */


/*----------------------------------------------------------------------------*/


void dj_setstyle( int style, char item[], char value[], STATUS *status );

/* sets style parameter number "style"
 *
 * parameters of routine
 * int        style;     input; number of style
 * char       item[];    input; name of style attribute
 * char       value[];   input; new value of style attribute (as string expr.)
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void dj_set_outputdir( char dir[], STATUS *status );

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name of scratch path
 * STATUS    *status;   output; return status
 */


/*----------------------------------------------------------------------------*/

#define dj_set_inputdir(d,s)

/*----------------------------------------------------------------------------*/


void dj_prepare( STATUS *status );

/* prepares hardcopy
 *
 * parameters of routine
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void dj_cleanup( char outf[], STATUS *status );

/* finishes hardcopy
 *
 * parameters of routine
 * char       outf[];     output; output filename
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void dj_setpar( char item[], char value[], STATUS *status );

/* sets hardcopy parameters
 *
 * parameters of routine
 * char       item[];     input; item to be set
 * char       value[];    input; new value of item
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/



#define dj_aspectratio() 1.0



/*----------------------------------------------------------------------------*/


void dj_arrayswap( BOOLEAN on_off );

/* switches array-swap on ot off
 *
 * parameter of routine
 * BOOLEAN   on_off;     TRUE=on, FALSE=off
 */


/*----------------------------------------------------------------------------*/



#else  /* BC_G_DESKJET */



#define dj_init(a,x,y,w,h,s)
#define dj_exit()
#define dj_finish()
#define dj_resize(x,y,w,h,s)
#define dj_erase()
#define dj_prepare(s)
#define dj_cleanup(f,s)
#define dj_setcoo(x,y,w,h,s)
#define dj_moveto(x,y)
#define dj_drawto(s,x,y)
#define dj_arrayplot(s,c,r,o,i,f,y,z,x)
#define dj_text(s,x,y,t)
#define dj_setstyle(s,i,v,x)
#define dj_set_outputdir(d,s)
#define dj_set_inputdir(d,s)

#define dj_charsize(s,x,t)
#define dj_linestyle(s,l,t)
#define dj_linewidth(s,l,t)
#define dj_color(s,r,g,b,t)
#define dj_setpar(i,v,s)
#define dj_aspectratio() 1.0
#define dj_arrayswap(o)



#endif /* BC_G_DESKJET */
#endif /* __DJUSRDEF */


/* file MMUSRDEF.H
 *
 * version 5, 20-May-92
 *
 * prototypes of module MEMCH.C
 * K. Stammler, 9-AUG-91
 */

#ifndef __MMUSRDEF
#define __MMUSRDEF


/*------------------------------------------------------------------------*/


void mm_init( int wdw, STATUS *status );

/* opens memory channel
 *
 * parameters of routine
 * int        wdw;       input; channel number
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void mm_exit( int wdw );

/* closes memory channel
 *
 * parameters of routine
 * int        wdw;       input; channel number
 */


/*------------------------------------------------------------------------*/


void mm_finish( void );

/* closes all channels
 *
 * no parameters
 */


/*------------------------------------------------------------------------*/


void mm_setcoo( int wdw, GBC_COO x, GBC_COO y, GBC_COO w, GBC_COO h,
	STATUS *status );

/* sets user coordinate system
 *
 * parameters of routine
 * int        wdw;         input; window number
 * GBC_COO    x, y, w, h;  input; coordinates
 * STATUS     *status;     output; return status
 */


/*------------------------------------------------------------------------*/


void mm_moveto( int wdw, GBC_COO x, GBC_COO y );

/* moves to (x,y)
 *
 * parameters of routine
 * int        wdw;      input; channel number
 * GBC_COO    x, y;     input; coordinates
 */


/*------------------------------------------------------------------------*/


void mm_drawto( int wdw, int style, GBC_COO x, GBC_COO y );

/* draws to (x,y)
 *
 * parameters of routine
 * int        wdw;      input; channel number
 * int        style;    input; style block number
 * GBC_COO    x, y;     input; coordinates
 */


/*------------------------------------------------------------------------*/


void mm_setpixel( int wdw, int style, GBC_COO x, GBC_COO y );

/* sets pixel at user coordinates (x,y)
 *
 * parameters of routine
 * int        wdw;      input; channel number
 * int        style;    input; style block number
 * GBC_COO    x, y;     input; coordinates
 */


/*------------------------------------------------------------------------*/


void mm_arrayplot( int wdw, int style, long cnt, int red,
	GBC_COO xoff, GBC_COO xinc, GBC_COO yoff, GBC_COO yarr[],
	float yzoom, STATUS *status );

/* plots an array of sample data
 *
 * parameters of routine
 * int        wdw;            input; output channels
 * int        style;          input; line style ID
 * long       cnt;            input; length of data array
 * int        red;            input; reduction factor for plotting
 * GBC_COO    xoff;           input; x-position of first sample
 * GBC_COO    xinc;           input; x increment
 * GBC_COO    yoff;           input; y-position of first sample
 * GBC_COO    yarr[];         input; sample data
 * GBC_COO    yzoom;          input; zoom factor of sample data
 * STATUS     *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void mm_erase( int wdw );

/* erases channel
 *
 * parameter of routine
 * int       wdw;     input; channel number
 */


/*------------------------------------------------------------------------*/


void mm_text( int wdw, int style, GBC_COO x, GBC_COO y, char text[] );

/* writes text at position (x,y)
 *
 * parameters of routine
 * int        wdw;     input; channel number
 * int        style;   input; style block number
 * GBC_COO    x, y;    input; coordinates
 * char       text[];  input; text to be printed
 */


/*------------------------------------------------------------------------*/


void mm_setstyle( int wdw, int style, char item[], char value[], STATUS *status );

/* writes text at position (x,y)
 *
 * parameters of routine
 * int        wdw;     input; channel number
 * int        style;   input; style block number
 * char       item[];  input; item name
 * char       value[]; input; value
 * STATUS     *status; output; return status
 */


/*------------------------------------------------------------------------*/


void mm_playback( int src, int dst, PAINTBOX *pb, char outf[], STATUS *status );

/* redraws vectors sored in memory
 *
 * parameters of routine
 * int        src;      input; source channel
 * int        dst;      input; destination channel
 * PAINTBOX   *pb;      input; paintbox routines
 * char       outf[];   output; name of output file (if any)
 * STATUS     *status;  output; return status
 */


/*------------------------------------------------------------------------*/

#endif /* __MMUSRDEF */


/* file GRAPHCH.C
 *      =========
 *
 * version 17, 24-May-93
 *
 * channel dispatcher of graphic output
 * K. Stammler, 9-AUG-91
 */


#define __THIS_IS_GRAPHCH

#include <stdio.h>
#include BASECNST
#include BC_SYSBASE
#include "graphbas.h"
#include "gmusrdef.h"
#include "mmusrdef.h"
#include "psusrdef.h"
#include "ccusrdef.h"
#include "tkusrdef.h"
#include "hpusrdef.h"
#include "djusrdef.h"
#include "bgusrdef.h"
#include "xwusrdef.h"
#include "vwusrdef.h"
#include BC_GCUSRDEF



#define WDWMASK 7



/* redraw routine available for all graphic modules
 * imported via graphbas.h
 */
void (*gbv_playback)( CHMAP src, CHMAP dst, PAINTBOX *pb,
	char outf[], STATUS *status );


/* global variables */
static BOOLEAN   gcv_init;           /* module initialized */
static unsigned int gcv_initmap;     /* initialized modules */
static BOOLEAN   gcv_swap;           /* swap display */
static void      (*gcv_writeext)(char text[]);
                                     /* write routine to external channel */


/* macros */
#define CSWAP(x,y) if (gcv_swap) {COOTYPE tmp;tmp=(x);(x)=(y);(y)=tmp;}


/*------------------------------------------------------------------------*/



void gc_init( CHMAP map, int attribs, COOTYPE xlo, COOTYPE ylo,
	COOTYPE width, COOTYPE height, STATUS *status )

/* initialises channels
 *
 * parameters of routine
 * CHMAP      map;       input; channel map
 * int        attribs;   input; window attributes
 * COOTYPE    xlo;       input; x-coo of lower left corner
 * COOTYPE    ylo;       input; y-coo of lower left corner
 * COOTYPE    width;     input; widths of window
 * COOTYPE    height;    input; height of window
 * STATUS     *status;   output; return status
 */
{
	/* executable code */

	if  (!gcv_init)  {
		gbv_playback = mm_playback;
		gcv_init = TRUE;
	} /*endif*/

	if  (GCF_GEM & map)  {
		gm_init( map & WDWMASK, attribs, xlo, ylo, width, height, status );
		if  (Severe(status))  return;
		gcv_initmap |= GCF_GEM;
	} /*endif*/

	if  (GCF_XWDW & map)  {
		xw_init( map & WDWMASK, attribs, xlo, ylo, width, height, status );
		if  (Severe(status))  return;
		gcv_initmap |= GCF_XWDW;
	} /*endif*/

	if  (GCF_BGI & map)  {
		bg_init( map & WDWMASK, attribs, xlo, ylo, width, height, status );
		if  (Severe(status))  return;
		gcv_initmap |= GCF_BGI;
	} /*endif*/

	if  (GCF_MEM & map)  {
		mm_init( map & WDWMASK, status );
		if  (Severe(status))  return;
		gcv_initmap |= GCF_MEM;
	} /*endif*/

	if  (GCF_PSF & map)  {
		ps_init( attribs, xlo, ylo, width, height, status );
		if  (Severe(status))  return;
		gcv_initmap |= GCF_PSF;
	} /*endif*/

	if  (GCF_CALCOMP & map)  {
		cc_init( attribs, xlo, ylo, width, height, status );
		if  (Severe(status))  return;
		gcv_initmap |= GCF_CALCOMP;
	} /*endif*/

	if  (GCF_TEK & map)  {
		tk_init( attribs, xlo, ylo, width, height, status );
		if  (Severe(status))  return;
		gcv_initmap |= GCF_TEK;
	} /*endif*/

	if  (GCF_HPGL & map)  {
		hp_init( attribs, xlo, ylo, width, height, status );
		if  (Severe(status))  return;
		gcv_initmap |= GCF_HPGL;
	} /*endif*/

	if  (GCF_DJ & map)  {
		dj_init( attribs, xlo, ylo, width, height, status );
		if  (Severe(status))  return;
		gcv_initmap |= GCF_DJ;
	} /*endif*/

	if  (GCF_VWS & map)  {
		vw_init( map & WDWMASK, attribs, xlo, ylo, width, height, status );
		if  (Severe(status))  return;
		gcv_initmap |= GCF_VWS;
	} /*endif*/

} /* end of gc_init */



/*------------------------------------------------------------------------*/



void gc_exit( CHMAP map )

/* closes channel
 *
 * parameters of routine
 * CHMAP      map;          input; window to be closed
 */
{
	/* executable code */

	if  (GCF_GEM & map)
		gm_exit( map & WDWMASK );

	if  (GCF_XWDW & map)
		xw_exit( map & WDWMASK );

	if  (GCF_BGI & map)
		bg_exit( map & WDWMASK );

	if  (GCF_MEM & map)
		mm_exit( map & WDWMASK );

	if  (GCF_PSF & map)
		ps_exit();

	if  (GCF_CALCOMP & map)
		cc_exit();

	if  (GCF_TEK & map)
		tk_exit();

	if  (GCF_HPGL & map)
		hp_exit();

	if  (GCF_DJ & map)
		dj_exit();

	if  (GCF_VWS & map)
		vw_exit( map & WDWMASK );

} /* end of gc_exit */



/*------------------------------------------------------------------------*/



void gc_finish( void )

/* closes all channels and terminates graphics
 *
 * no parameters
 */
{
	/* executable code */

	gm_finish();
	xw_finish();
	bg_finish();
	mm_finish();
	ps_finish();
	cc_finish();
	tk_finish();
	hp_finish();
	dj_finish();
	vw_finish();
	gcv_initmap = 0;

} /* end of gc_finish */



/*------------------------------------------------------------------------*/



void gc_resizewdw( CHMAP map, COOTYPE xpos, COOTYPE ypos,
	COOTYPE width, COOTYPE height, STATUS *status )

/* resizes channel
 *
 * parameters of routine
 * CHMAP      map;            input; channel map
 * COOTYPE    xpos, ypos;     input; position of window
 * COOTYPE    width, height;  input; size of window
 * STATUS     *status;        output; return status
 */
{
	/* executable code */

	if  (GCF_GEM & map)  {
		gm_resizewdw( map & WDWMASK, xpos, ypos, width, height, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_XWDW & map)  {
		xw_resizewdw( map & WDWMASK, xpos, ypos, width, height, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_BGI & map)  {
		bg_resizewdw( map & WDWMASK, xpos, ypos, width, height, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_PSF & map)  {
		ps_resize( xpos, ypos, width, height, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_CALCOMP & map)  {
		cc_resize( xpos, ypos, width, height, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_TEK & map)  {
		tk_resize( xpos, ypos, width, height, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_HPGL & map)  {
		hp_resize( xpos, ypos, width, height, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_DJ & map)  {
		dj_resize( xpos, ypos, width, height, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_VWS & map)  {
		vw_resizewdw( map & WDWMASK, xpos, ypos, width, height, status );
		if  (Severe(status))  return;
	} /*endif*/

} /* end of gc_resizewdw */



/*------------------------------------------------------------------------*/



void gc_popwdw( CHMAP map )

/* pops window on top
 *
 * parameters of routine
 * CHMAP      map;            input; channel map
 */
{
	/* executable code */

	if  (GCF_GEM & map)
		gm_popwdw( map & WDWMASK );

	if  (GCF_XWDW & map)
		xw_popwdw( map & WDWMASK );

	if  (GCF_VWS & map)
		vw_popwdw( map & WDWMASK );

} /* end of gc_popwdw */



/*------------------------------------------------------------------------*/



void gc_setwdwname( CHMAP map, char name[] )

/* sets new window name
 *
 * parameters of routine
 * CHMAP     map;       input; channel map
 * char      name[];    input; new name
 */
{
	/* executable code */

	if  (GCF_GEM & map)
		gm_setwdwname( map & WDWMASK, name );

	if  (GCF_XWDW & map)
		xw_setwdwname( map & WDWMASK, name );

	if  (GCF_BGI & map)
		bg_setwdwname( map & WDWMASK, name );

	if  (GCF_VWS & map)
		vw_setwdwname( map & WDWMASK, name );

} /* end of gc_setwdwname */



/*------------------------------------------------------------------------*/



void gc_setstyle( CHMAP map, unsigned style, char item[],
	char value[], STATUS *status )

/* sets style attribute
 *
 * parameters of routine
 * CHMAP      map;       input; channel map
 * unsigned   style;     input; style block number
 * char       item[];    input; name of attribute
 * char       value[];   input; new value of attribute (text)
 * STATUS     *status;   output; return status
 */
{
	/* executable code */

	if  (GCF_GEM & map)  {
		gm_setstyle( map & WDWMASK, style, item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_XWDW & map)  {
		xw_setstyle( map & WDWMASK, style, item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_BGI & map)  {
		bg_setstyle( map & WDWMASK, style, item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_MEM & map)  {
		mm_setstyle( map & WDWMASK, style, item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_PSF & map)  {
		ps_setstyle( style, item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_CALCOMP & map)  {
		cc_setstyle( style, item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_TEK & map)  {
		tk_setstyle( style, item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_HPGL & map)  {
		hp_setstyle( style, item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_DJ & map)  {
		dj_setstyle( style, item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_VWS & map)  {
		vw_setstyle( map & WDWMASK, style, item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

} /* end of gc_setstyle */



/*------------------------------------------------------------------------*/



void gc_setcoo( CHMAP map, COOTYPE x, COOTYPE y, COOTYPE w, COOTYPE h,
	STATUS *status )

/* sets user coordinates in window.  If x,y,w,h are all equal to zero,
 * the last call is repeated.
 *
 * parameters of routine
 * CHMAP      map;        input; channel map
 * COOTYPE    x, y, w, h; input; user coordinates
 * STATUS     *status;    output; return status
 */
{
	/* executable code */

	CSWAP(x,y)
	CSWAP(w,h)

	if  (GCF_GEM & map)  {
		gm_setcoo( map & WDWMASK, x, y, w, h, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_XWDW & map)  {
		xw_setcoo( map & WDWMASK, x, y, w, h, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_BGI & map)  {
		bg_setcoo( map & WDWMASK, x, y, w, h, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_MEM & map)  {
		mm_setcoo( map & WDWMASK, x, y, w, h, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_PSF & map)  {
		ps_setcoo( x, y, w, h, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_CALCOMP & map)  {
		cc_setcoo( x, y, w, h, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_TEK & map)  {
		tk_setcoo( x, y, w, h, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_HPGL & map)  {
		hp_setcoo( x, y, w, h, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_DJ & map)  {
		dj_setcoo( x, y, w, h, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_VWS & map)  {
		vw_setcoo( map & WDWMASK, x, y, w, h, status );
		if  (Severe(status))  return;
	} /*endif*/

} /* end of gc_setcoo */



/*------------------------------------------------------------------------*/



float gc_aspectratio( CHMAP map )

/* returns ratio of width to height
 *
 * parameters of routine
 * CHMAP      map;      input; channel map
 *                      returns aspect ratio
 */
{
	/* executable code */

	if  (GCF_GEM & map)
		return gm_aspectratio( map & WDWMASK );

	if  (GCF_XWDW & map)
		return xw_aspectratio( map & WDWMASK );

	if  (GCF_BGI & map)
		return bg_aspectratio( map & WDWMASK );

	if  (GCF_PSF & map)
		return ps_aspectratio();

	if  (GCF_CALCOMP & map)
		return cc_aspectratio();

	if  (GCF_TEK & map)
		return tk_aspectratio();

	if  (GCF_HPGL & map)
		return hp_aspectratio();

	if  (GCF_DJ & map)
		return dj_aspectratio();

	if  (GCF_VWS & map)
		return vw_aspectratio( map & WDWMASK );

	return 1.0;

} /* end of gc_aspectratio */



/*------------------------------------------------------------------------*/



void gc_moveto( CHMAP map, COOTYPE x, COOTYPE y )

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * CHMAP      map;      input; channel map
 * COOTYPE    x, y;     input; location to move to
 */
{
	/* executable code */

	CSWAP(x,y)

	if  (GCF_GEM & map)
		gm_moveto( map & WDWMASK, x, y );

	if  (GCF_XWDW & map)
		xw_moveto( map & WDWMASK, x, y );

	if  (GCF_BGI & map)
		bg_moveto( map & WDWMASK, x, y );

	if  (GCF_MEM & map)
		mm_moveto( map & WDWMASK, x, y );

	if  (GCF_PSF & map)
		ps_moveto( x, y );

	if  (GCF_CALCOMP & map)
		cc_moveto( x, y );

	if  (GCF_TEK & map)
		tk_moveto( x, y );

	if  (GCF_HPGL & map)
		hp_moveto( x, y );

	if  (GCF_DJ & map)
		dj_moveto( x, y );

	if  (GCF_VWS & map)
		vw_moveto( map & WDWMASK, x, y );

} /* end of gc_moveto */



/*------------------------------------------------------------------------*/



void gc_drawto( CHMAP map, int style, COOTYPE x, COOTYPE y )

/* moves to position (x,y) in user coordinates
 *
 * parameters of routine
 * CHMAP      map;      input; channel map
 * int        style;    input; style block number
 * COOTYPE    x, y;     input; location to move to
 */
{
	/* executable code */

	CSWAP(x,y)

	if  (GCF_GEM & map)
		gm_drawto( map & WDWMASK, style, x, y );

	if  (GCF_XWDW & map)
		xw_drawto( map & WDWMASK, style, x, y );

	if  (GCF_BGI & map)
		bg_drawto( map & WDWMASK, style, x, y );

	if  (GCF_MEM & map)
		mm_drawto( map & WDWMASK, style, x, y );

	if  (GCF_PSF & map)
		ps_drawto( style, x, y );

	if  (GCF_CALCOMP & map)
		cc_drawto( style, x, y );

	if  (GCF_TEK & map)
		tk_drawto( style, x, y );

	if  (GCF_HPGL & map)
		hp_drawto( style, x, y );

	if  (GCF_DJ & map)
		dj_drawto( style, x, y );

	if  (GCF_VWS & map)
		vw_drawto( map & WDWMASK, style, x, y );

} /* end of gc_drawto */



/*------------------------------------------------------------------------*/



void gc_setpixel( CHMAP map, int style, COOTYPE x, COOTYPE y )

/* sets pixel at position (x,y) in user coordinates
 *
 * parameters of routine
 * CHMAP      map;      input; channel map
 * int        style;    input; style block number
 * COOTYPE    x, y;     input; location to move to
 */
{
	/* executable code */

	CSWAP(x,y)

	if  (GCF_GEM & map)
		gm_setpixel( map & WDWMASK, style, x, y );

	if  (GCF_MEM & map)
		mm_setpixel( map & WDWMASK, style, x, y );

} /* end of gc_setpixel */



/*------------------------------------------------------------------------*/



void gc_arrayplot( CHMAP map, int style, long cnt, int red, COOTYPE xoff,
	COOTYPE xinc, COOTYPE yoff, COOTYPE yarr[], COOTYPE yzoom, STATUS *status )

/* plots an array of sample data
 *
 * parameters of routine
 * CHMAP      map;       input; channel map
 * int        style;     input; line style ID
 * long       cnt;       input; length of data array
 * int        red;       input; reduction factor for plotting
 * COOTYPE    xoff;      input; x-position of first sample
 * COOTYPE    xinc;      input; x increment
 * COOTYPE    yoff;      input; y-position of first sample
 * COOTYPE    yarr[];    input; sample data
 * COOTYPE    yzoom;     input; zoom factor of sample data
 * STATUS     *status;   output; return status
 */
{
	/* executable code */

	if  (cnt < 2)  return;

	if  (GCF_GEM & map)  {
		gm_arrayplot( map & WDWMASK, style, cnt, red, xoff, xinc,
			yoff, yarr, yzoom, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_XWDW & map)  {
		xw_arrayplot( map & WDWMASK, style, cnt, red, xoff, xinc,
			yoff, yarr, yzoom, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_BGI & map)  {
		bg_arrayplot( map & WDWMASK, style, cnt, red, xoff, xinc,
			yoff, yarr, yzoom, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_MEM & map)  {
		mm_arrayplot( map & WDWMASK, style, cnt, red, xoff, xinc,
			yoff, yarr, yzoom, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_PSF & map)  {
		ps_arrayplot( style, cnt, red, xoff, xinc, yoff, yarr,
			yzoom, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_CALCOMP & map)  {
		cc_arrayplot( style, cnt, red, xoff, xinc, yoff, yarr,
			yzoom, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_TEK & map)  {
		tk_arrayplot( style, cnt, red, xoff, xinc, yoff, yarr,
			yzoom, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_HPGL & map)  {
		hp_arrayplot( style, cnt, red, xoff, xinc, yoff, yarr,
			yzoom, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_DJ & map)  {
		dj_arrayplot( style, cnt, red, xoff, xinc, yoff, yarr,
			yzoom, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_VWS & map)  {
		vw_arrayplot( map & WDWMASK, style, cnt, red, xoff, xinc,
			yoff, yarr, yzoom, status );
		if  (Severe(status))  return;
	} /*endif*/

} /* end of gc_arrayplot */



/*------------------------------------------------------------------------*/



void gc_erase( CHMAP map )

/* clears channel
 *
 * parameters of routine
 * CHMAP      map;       input; channel map
 */
{
	/* executable code */

	if  (GCF_GEM & map)
		gm_erase( map & WDWMASK );

	if  (GCF_XWDW & map)
		xw_erase( map & WDWMASK );

	if  (GCF_BGI & map)
		bg_erase( map & WDWMASK );

	if  (GCF_MEM & map)
		mm_erase( map & WDWMASK );

	if  (GCF_PSF & map)
		ps_erase();

	if  (GCF_CALCOMP & map)
		cc_erase();

	if  (GCF_TEK & map)
		tk_erase();

	if  (GCF_HPGL & map)
		hp_erase();

	if  (GCF_DJ & map)
		dj_erase();

	if  (GCF_VWS & map)
		vw_erase( map & WDWMASK );

} /* end of gc_erase */



/*------------------------------------------------------------------------*/



void gc_text( CHMAP map, int style, COOTYPE x, COOTYPE y, char text[] )

/* writes text to window
 *
 * parameters of routine
 * CHMAP      map;        input; channel map
 * int        style;      input; character style block number
 * COOTYPE    x, y;       input; text position
 * char       text[];     input; output text
 */
{
	/* executable code */

	CSWAP(x,y)

	if  (GCF_GEM & map)
		gm_text( map & WDWMASK, style, x, y, text );

	if  (GCF_XWDW & map)
		xw_text( map & WDWMASK, style, x, y, text );

	if  (GCF_BGI & map)
		bg_text( map & WDWMASK, style, x, y, text );

	if  (GCF_MEM & map)
		mm_text( map & WDWMASK, style, x, y, text );

	if  (GCF_PSF & map)
		ps_text( style, x, y, text );

	if  (GCF_CALCOMP & map)
		cc_text( style, x, y, text );

	if  (GCF_TEK & map)
		tk_text( style, x, y, text );

	if  (GCF_HPGL & map)
		hp_text( style, x, y, text );

	if  (GCF_DJ & map)
		dj_text( style, x, y, text );

	if  (GCF_VWS & map)
		vw_text( map & WDWMASK, style, x, y, text );

} /* end of gc_text */



/*------------------------------------------------------------------------*/



void gc_write( CHMAP map, char text[] )

/* writes text to window at current write position
 *
 * parameters of routine
 * CHMAP      map;          input; channel map
 * char       text[];       input; output text
 */
{
	/* executable code */

	if  (GCF_GEM & map)
		gm_write( map & WDWMASK, text );

	if  (GCF_XWDW & map)
		xw_write( map & WDWMASK, text );

	if  (GCF_BGI & map)
		bg_write( map & WDWMASK, text );

	if  (GCF_STDCH & map)
		printf( "%s", text );

	if  (GCF_VWS & map)
		vw_write( map & WDWMASK, text );

	if  (GCF_TEK & map)
		tk_write( text );

	if  (GCF_EXTERN & map && gcv_writeext != NULL)
		(*gcv_writeext)( text );

} /* end of gc_write */



/*------------------------------------------------------------------------*/



void gc_wrtch( CHMAP map, char ch )

/* writes a single character to channel
 *
 * parameters of routine
 * CHMAP      map;         input; channel map
 * char       ch;          input; char to be written
 */
{
	/* executable code */

	if  (GCF_GEM & map)
		gm_wrtch( map & WDWMASK, ch );

	if  (GCF_XWDW & map)
		xw_wrtch( map & WDWMASK, ch );

	if  (GCF_BGI & map)
		bg_wrtch( map & WDWMASK, ch );

	if  (GCF_STDCH & map)
		printf( "%c", ch );

	if  (GCF_VWS & map)
		vw_wrtch( map & WDWMASK, ch );

	if  (GCF_TEK & map)
		tk_wrtch( ch );

} /* end of gc_wrtch */



/*------------------------------------------------------------------------*/



int gc_txtwidth( CHMAP map )

/* returns width of channel in characters
 *
 * parameters of routine
 * CHMAP      map;      input; channel map
 */
{
	/* executable code */

	if  (GCF_GEM & map)
		return gm_txtwidth( map & WDWMASK );

	if  (GCF_BGI & map)
		return bg_txtwidth( map & WDWMASK );

	if  (GCF_STDCH & map)
		return 80;

	return 80;

} /* end of gc_txtwidth */



/*------------------------------------------------------------------------*/



int gc_txtheight( CHMAP map )

/* returns height of window in characters
 *
 * parameters of routine
 * CHMAP      wdw;     input; channel map
 */
{
	/* executable code */

	if  (GCF_GEM & map)
		return gm_txtheight( map & WDWMASK );

	if  (GCF_BGI & map)
		return bg_txtheight( map & WDWMASK );

	if  (GCF_STDCH & map)
		return 24;

	return 24;

} /* end of gc_txtheight */



/*------------------------------------------------------------------------*/



float gc_chheight( CHMAP map )

/* returns current character height in window "wdw"
 *
 * parameter of routine
 * CHMAP     map;      input; channel map
 */
{
	/* executable code */

	if  (GCF_GEM & map)
		return gm_chheight( map & WDWMASK );

	if  (GCF_XWDW & map)
		return xw_chheight( map & WDWMASK );

	if  (GCF_BGI & map)
		return bg_chheight( map & WDWMASK );

	return 0.0;

} /* end of gc_chheight */



/*------------------------------------------------------------------------*/



void gc_read( CHMAP map, int maxlth, char text[] )

/* reads text from terminal
 *
 * parameters of routine
 * CHMAP    map;          input; channel map
 * int      maxlth;       input; maximum length of text
 * char     text[];       output; text read
 */
{
	/* executable code */

	if  (GCF_GEM & map)  {
		gm_read( map & WDWMASK, maxlth, text );
		return;
	} /*endif*/

	if  (GCF_XWDW & map)  {
		xw_read( map & WDWMASK, maxlth, text );
		return;
	} /*endif*/

	if  (GCF_BGI & map)  {
		bg_read( map & WDWMASK, maxlth, text );
		return;
	} /*endif*/

	if  (GCF_VWS & map)  {
		vw_read( map & WDWMASK, maxlth, text );
		return;
	} /*endif*/

	fgets( text, maxlth, stdin );

} /* end of gc_read */



/*------------------------------------------------------------------------*/



void gc_getloc( CHMAP map, COOTYPE *x, COOTYPE *y, char *ch )

/* requests mouse position in window
 *
 * parameters of routine
 * CHMAP      map;        input; channel map
 * COOTYPE    *x, *y;     output; location selected
 * char       *ch;        output; key pressed
 */
{
	/* executable code */

	if  (GCF_GEM & map)  {
		gm_getloc( map & WDWMASK, x, y, ch );
		CSWAP(*x,*y)
		return;
	} /*endif*/

	if  (GCF_XWDW & map)  {
		xw_getloc( map & WDWMASK, x, y, ch );
		CSWAP(*x,*y)
		return;
	} /*endif*/

	if  (GCF_BGI & map)  {
		bg_getloc( map & WDWMASK, x, y, ch );
		CSWAP(*x,*y)
		return;
	} /*endif*/

	if  (GCF_VWS & map)  {
		vw_getloc( map & WDWMASK, x, y, ch );
		CSWAP(*x,*y)
		return;
	} /*endif*/

	if  (GCF_TEK & map)  {
		tk_getloc( x, y, ch );
		CSWAP(*x,*y)
		return;
	} /*endif*/

} /* end of gc_getloc */



/*------------------------------------------------------------------------*/



void gc_set_outputdir( char dir[], STATUS *status )

/* sets scratch directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name of scratch path
 * STATUS    *status;   output; return status
 */
{
	/* executable code */

	gm_set_outputdir( dir, status );
	if  (Severe(status))   return;
	bg_set_outputdir( dir, status );
	if  (Severe(status))   return;
	ps_set_outputdir( dir, status );
	if  (Severe(status))   return;
	cc_set_outputdir( dir, status );
	if  (Severe(status))   return;
	tk_set_outputdir( dir, status );
	if  (Severe(status))   return;
	hp_set_outputdir( dir, status );
	if  (Severe(status))   return;
	dj_set_outputdir( dir, status );
	if  (Severe(status))   return;
	xw_set_outputdir( dir, status );
	if  (Severe(status))  return;
	vw_set_outputdir( dir, status );
	if  (Severe(status))  return;

} /* end of gc_set_outputdir */



/*------------------------------------------------------------------------*/



void gc_set_inputdir( char dir[], STATUS *status )

/* sets input directory
 *
 * parameters of routine
 * char      dir[];     input; new directory name
 * STATUS    *status;   output; return status
 */
{
	/* executable code */

	gm_set_inputdir( dir, status );
	if  (Severe(status))   return;
	bg_set_inputdir( dir, status );
	if  (Severe(status))   return;
	ps_set_inputdir( dir, status );
	if  (Severe(status))   return;
	cc_set_inputdir( dir, status );
	if  (Severe(status))   return;
	tk_set_inputdir( dir, status );
	if  (Severe(status))   return;
	hp_set_inputdir( dir, status );
	if  (Severe(status))   return;
	dj_set_inputdir( dir, status );
	if  (Severe(status))   return;
	xw_set_inputdir( dir, status );
	if  (Severe(status))  return;
	vw_set_inputdir( dir, status );
	if  (Severe(status))  return;

} /* end of gc_set_inputdir */



/*------------------------------------------------------------------------*/



void gc_setpar( CHMAP map, char item[], char value[], STATUS *status )

/* sets channel specific parameters
 *
 * parameters of routine
 * CHMAP      map;       input; channel map
 * char       item[];    input; name of item to be changed
 * char       value[];   input; new value of item
 * STATUS     *status;   output; return status
 */
{
	/* executable code */

	if  (GCF_GEM & map)  {
		gm_setpar( item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_BGI & map)  {
		bg_setpar( item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_PSF & map)  {
		ps_setpar( item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_CALCOMP & map)  {
		cc_setpar( item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_TEK & map)  {
		tk_setpar( item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_HPGL & map)  {
		hp_setpar( item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_DJ & map)  {
		dj_setpar( item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_VWS & map)  {
		vw_setpar( item, value, status );
		if  (Severe(status))  return;
	} /*endif*/

} /* end of gc_setpar */



/*----------------------------------------------------------------------------*/



void gc_update( void )

/* updates window content
 * no parameters
 */
{
   /* executable code */

   if  (GCF_XWDW & gcv_initmap)
      xw_updatewdw();

} /* end of gc_update */



/*------------------------------------------------------------------------*/



void gc_flushbuffers( void )

/* flushes all output buffers
 *
 * no parameters
 */
{
   /* executable code */

	if  (GCF_XWDW & gcv_initmap)
		xw_flushbuffers();

} /* end of gc_flushbuffers */



/*------------------------------------------------------------------------*/



void gc_playback( CHMAP src, CHMAP dst, char outf[], STATUS *status )

/* copies content of channel src into channel dst
 *
 * parameters of routine
 * CHMAP      src;     input; source channel
 * CHMAP      dst;     input; destination channel
 * char       outf[];  output; output filename
 * STATUS     *status; output; return status
 */
{
	/* local variables */
	PAINTBOX pb;       /* paintbox routines */

	/* executable code */

#	ifdef BC_G_GEM
	if  (GCF_GEM & dst)  {
		pb.wpb.pbtype = GBC_WPAINT;
		pb.wpb.prepare = gm_prepare;
		pb.wpb.moveto = gm_moveto;
		pb.wpb.drawto = gm_drawto;
		pb.wpb.arrayplot = gm_arrayplot;
		pb.wpb.text = gm_text;
		pb.wpb.setstyle = gm_setstyle;
		pb.wpb.setcoo = gm_setcoo;
		pb.wpb.cleanup = gm_cleanup;
		mm_playback( src & WDWMASK, dst & WDWMASK, &pb, outf, status );
		if  (Severe(status))  return;
	} /*endif*/
#	endif  /* BC_G_GEM */

#	ifdef BC_G_XWDW
	if  (GCF_XWDW & dst)  {
		pb.wpb.pbtype = GBC_WPAINT;
		pb.wpb.prepare = xw_prepare;
		pb.wpb.moveto = xw_moveto;
		pb.wpb.drawto = xw_drawto;
		pb.wpb.arrayplot = xw_arrayplot;
		pb.wpb.text = xw_text;
		pb.wpb.setstyle = xw_setstyle;
		pb.wpb.setcoo = xw_setcoo;
		pb.wpb.cleanup = xw_cleanup;
		mm_playback( src & WDWMASK, dst & WDWMASK, &pb, outf, status );
		if  (Severe(status))  return;
	} /*endif*/
#	endif  /* BC_G_XWDW */

#	ifdef BC_G_BGI
	if  (GCF_BGI & dst)  {
		pb.wpb.pbtype = GBC_WPAINT;
		pb.wpb.prepare = bg_prepare;
		pb.wpb.moveto = bg_moveto;
		pb.wpb.drawto = bg_drawto;
		pb.wpb.arrayplot = bg_arrayplot;
		pb.wpb.text = bg_text;
		pb.wpb.setstyle = bg_setstyle;
		pb.wpb.setcoo = bg_setcoo;
		pb.wpb.cleanup = bg_cleanup;
		mm_playback( src & WDWMASK, dst & WDWMASK, &pb, outf, status );
		if  (Severe(status))  return;
	} /*endif*/
#	endif  /* BC_G_BGI */

#	ifdef BC_G_POSTSCRIPT
	if  (GCF_PSF & dst)  {
		pb.ppb.pbtype = GBC_PPAINT;
		pb.ppb.prepare = ps_prepare;
		pb.ppb.moveto = ps_moveto;
		pb.ppb.drawto = ps_drawto;
		pb.ppb.arrayplot = ps_arrayplot;
		pb.ppb.text = ps_text;
		pb.ppb.setstyle = ps_setstyle;
		pb.ppb.setcoo = ps_setcoo;
		pb.ppb.cleanup = ps_cleanup;
		mm_playback( src & WDWMASK, dst & WDWMASK, &pb, outf, status );
		if  (Severe(status))  return;
	} /*endif*/
#	endif  /* BC_G_POSTSCRIPT */

#	ifdef BC_G_CALCOMP
	if  (GCF_CALCOMP & dst)  {
		pb.ppb.pbtype = GBC_PPAINT;
		pb.ppb.prepare = cc_prepare;
		pb.ppb.moveto = cc_moveto;
		pb.ppb.drawto = cc_drawto;
		pb.ppb.arrayplot = cc_arrayplot;
		pb.ppb.text = cc_text;
		pb.ppb.setstyle = cc_setstyle;
		pb.ppb.setcoo = cc_setcoo;
		pb.ppb.cleanup = cc_cleanup;
		mm_playback( src & WDWMASK, dst & WDWMASK, &pb, outf, status );
		if  (Severe(status))  return;
	} /*endif*/
#	endif  /* BC_G_CALCOMP */

#	ifdef BC_G_TEK
	if  (GCF_TEK & dst)  {
		pb.ppb.pbtype = GBC_PPAINT;
		pb.ppb.prepare = tk_prepare;
		pb.ppb.moveto = tk_moveto;
		pb.ppb.drawto = tk_drawto;
		pb.ppb.arrayplot = tk_arrayplot;
		pb.ppb.text = tk_text;
		pb.ppb.setstyle = tk_setstyle;
		pb.ppb.setcoo = tk_setcoo;
		pb.ppb.cleanup = tk_cleanup;
		mm_playback( src & WDWMASK, dst & WDWMASK, &pb, outf, status );
		if  (Severe(status))  return;
	} /*endif*/
#	endif  /* BC_G_TEK */

#	ifdef BC_G_HPGL
	if  (GCF_HPGL & dst)  {
		pb.ppb.pbtype = GBC_PPAINT;
		pb.ppb.prepare = hp_prepare;
		pb.ppb.moveto = hp_moveto;
		pb.ppb.drawto = hp_drawto;
		pb.ppb.arrayplot = hp_arrayplot;
		pb.ppb.text = hp_text;
		pb.ppb.setstyle = hp_setstyle;
		pb.ppb.setcoo = hp_setcoo;
		pb.ppb.cleanup = hp_cleanup;
		mm_playback( src & WDWMASK, dst & WDWMASK, &pb, outf, status );
		if  (Severe(status))  return;
	} /*endif*/
#	endif  /* BC_G_HPGL */

#	ifdef BC_G_DESKJET
	if  (GCF_DJ & dst)  {
		pb.ppb.pbtype = GBC_PPAINT;
		pb.ppb.prepare = dj_prepare;
		pb.ppb.moveto = dj_moveto;
		pb.ppb.drawto = dj_drawto;
		pb.ppb.arrayplot = dj_arrayplot;
		pb.ppb.text = dj_text;
		pb.ppb.setstyle = dj_setstyle;
		pb.ppb.setcoo = dj_setcoo;
		pb.ppb.cleanup = dj_cleanup;
		mm_playback( src & WDWMASK, dst & WDWMASK, &pb, outf, status );
		if  (Severe(status))  return;
	} /*endif*/
#	endif  /* BC_G_DESKJET */

#	ifdef BC_G_VWS
	if  (GCF_VWS & dst)  {
		vw_cleanup( src & WDWMASK, outf, status );
	} /*endif*/
#	endif /* BC_G_VWS */

} /* end of gc_playback */



/*------------------------------------------------------------------------*/



void gc_set_write_extern( void (*wr_rout)(char text[]) )

/* sets the write routine for the external channel
 *
 * parameters of routine
 * void       (*wr_rout)(char text[]);    input; output routine
 */
{
	/* executable code */

	gcv_writeext = wr_rout;

} /* end of gc_set_write_extern */



/*------------------------------------------------------------------------*/



void gc_closeplot( CHMAP map, char outf[], STATUS *status )

/* closes plot file on hardcopy channels
 *
 * parameters of routine
 * CHMAP      map;         input; channel map
 * char       outf[];      output; name of plotfile
 * STATUS     *status;     output; return status
 */
{
	/* executable code */

	if  (GCF_PSF & map)  {
		ps_cleanup( outf, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_CALCOMP & map)  {
		cc_cleanup( outf, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_HPGL & map)  {
		hp_cleanup( outf, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_DJ & map)  {
		dj_cleanup( outf, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GCF_VWS & map)  {
		vw_cleanup( map & WDWMASK, outf, status );
		if  (Severe(status))  return;
	} /*endif*/

} /* end of gc_closeplot */


 
/*------------------------------------------------------------------------*/



void gc_swap( BOOLEAN on_off )

/* switches x-y swapping on or off
 *
 * parameters of routine
 * BOOLEAN    on_off;    input; TRUE=on, FALSE=off
 */
{
   /* executable code */

   gcv_swap = on_off;
#  ifdef BC_G_XWINDOW
   xw_arrayswap( on_off );
#  endif
#  ifdef BC_G_POSTSCRIPT
   ps_arrayswap( on_off );
#  endif
#  ifdef BC_G_CALCOMP
   cc_arrayswap( on_off );
#  endif
#  ifdef BC_G_TEK
   tk_arrayswap( on_off );
#  endif
#  ifdef BC_G_HPGL
   hp_arrayswap( on_off );
#  endif
#  ifdef BC_G_BGI
   bg_arrayswap( on_off );
#  endif
#  ifdef BC_G_GEM
   gm_arrayswap( on_off );
#  endif
#  ifdef BC_G_DESKJET
   dj_arrayswap( on_off );
#  endif
#  ifdef BC_G_VWS
   vw_arrayswap( on_off );
#  endif

} /* end of gc_swap */



/*------------------------------------------------------------------------*/

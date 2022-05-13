
/* file MEMCH.C
 *      =======
 *
 * version 9, 20-May-92
 *
 * memory channel of graphics output
 * K. Stammler, 9-AUG-91
 */

#include <stdio.h>
#include <string.h>
#include BASECNST
#include BC_SYSBASE
#include "mfusrdef.h"
#include "graphbas.h"
#include "mmusrdef.h"
#include "gcerrors.h"


#define mmh_illchan(c) ((c)<0 || (c)>=GBC_MAXWDW)

#define ID_MOVETO   1
#define ID_DRAWTO   2
#define ID_ARRAY    3
#define ID_TEXT     4
#define ID_SETSTYLE 5
#define ID_SETCOO   6
#define ID_SETPIXEL 7

/* global variables */
static int     mmv_init=0;                 /* initialised channels */
/* setcoo values, to be restored on playback calls */
/* currcoo contains the current active coo's (set by setcoo) */
static GBC_COO mmv_currcoo_x[GBC_MAXWDW+1];
static GBC_COO mmv_currcoo_y[GBC_MAXWDW+1];
static GBC_COO mmv_currcoo_w[GBC_MAXWDW+1]={10.0,10.0,10.0,10.0,10.0,10.0,10.0};
static GBC_COO mmv_currcoo_h[GBC_MAXWDW+1]={10.0,10.0,10.0,10.0,10.0,10.0,10.0};
/* startcoo contains the coo's valid at the most recent erase command */
static GBC_COO mmv_startcoo_x[GBC_MAXWDW+1];
static GBC_COO mmv_startcoo_y[GBC_MAXWDW+1];
static GBC_COO mmv_startcoo_w[GBC_MAXWDW+1]={10.0,10.0,10.0,10.0,10.0,10.0,10.0};
static GBC_COO mmv_startcoo_h[GBC_MAXWDW+1]={10.0,10.0,10.0,10.0,10.0,10.0,10.0};


/*------------------------------------------------------------------------*/



void mm_init( int wdw, STATUS *status )

/* opens memory channel
 *
 * parameters of routine
 * int        wdw;       input; channel number
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	int      i;         /* counter */

	/* executable code */

	if  (mmh_illchan(wdw))  return;
	mf_open( wdw, TRUE, "w", status );
	if  (Severe(status))  return;
	mmv_init |= (1 << wdw);

	for  (i=0; i<=GBC_MAXWDW; i++)  {
		mmv_startcoo_x[i] = 0.;
		mmv_startcoo_y[i] = 0.;
		mmv_startcoo_w[i] = 10.;
		mmv_startcoo_h[i] = 10.;
		mmv_currcoo_x[i] = 0.;
		mmv_currcoo_y[i] = 0.;
		mmv_currcoo_w[i] = 10.;
		mmv_currcoo_h[i] = 10.;
	} /*endif*/

} /* end of mm_init */



/*------------------------------------------------------------------------*/



void mm_exit( int wdw )

/* closes memory channel
 *
 * parameters of routine
 * int        wdw;       input; channel number
 */
{
	/* local variables */
	STATUS   locstat=BC_NOERROR;  /* local status */

	/* executable code */

	if  (mmh_illchan(wdw))  return;
	mf_close( wdw, &locstat );
	if  (Severe(&locstat))  return;
	mmv_init &= ~(1 << wdw);

} /* end of mm_exit */



/*------------------------------------------------------------------------*/



void mm_finish( void )

/* closes all channels
 *
 * no parameters
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	/* close all channels */
	for  (i=0;i<GBC_MAXWDW;i++)
		if  ((1<<i) & mmv_init)
			mm_exit( i );

	mmv_init = 0;

} /* end of mm_finish */



/*------------------------------------------------------------------------*/



void mm_setcoo( int wdw, GBC_COO x, GBC_COO y, GBC_COO w, GBC_COO h,
	STATUS *status )

/* sets user coordinate system
 *
 * parameters of routine
 * int        wdw;         input; window number
 * GBC_COO    x, y, w, h;  input; coordinates
 * STATUS     *status;     output; return status
 */
{
	/* executable code */

	if  (mmh_illchan(wdw))  return;
	mf_writeint( wdw, ID_SETCOO, status );
	if  (Severe(status))  return;
	mf_writereal( wdw, x, status );
	if  (Severe(status))  return;
	mf_writereal( wdw, y, status );
	if  (Severe(status))  return;
	mf_writereal( wdw, w, status );
	if  (Severe(status))  return;
	mf_writereal( wdw, h, status );
	if  (Severe(status))  return;

	mmv_currcoo_x[wdw] = x;
	mmv_currcoo_y[wdw] = y;
	mmv_currcoo_w[wdw] = w;
	mmv_currcoo_h[wdw] = h;

} /* end of mm_setcoo */



/*------------------------------------------------------------------------*/



void mm_moveto( int wdw, GBC_COO x, GBC_COO y )

/* moves to (x,y)
 *
 * parameters of routine
 * int        wdw;      input; channel number
 * GBC_COO    x, y;     input; coordinates
 */
{
	/* local variables */
	STATUS   locstat=BC_NOERROR;   /* local status */

	/* executable code */

	if  (mmh_illchan(wdw))  return;
	mf_writeint( wdw, ID_MOVETO, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );
	mf_writereal( wdw, x, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );
	mf_writereal( wdw, y, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );

} /* end of mm_moveto */



/*------------------------------------------------------------------------*/



void mm_drawto( int wdw, int style, GBC_COO x, GBC_COO y )

/* draws to (x,y)
 *
 * parameters of routine
 * int        wdw;      input; channel number
 * int        style;    input; style block number
 * GBC_COO    x, y;     input; coordinates
 */
{
	/* local variables */
	STATUS   locstat=BC_NOERROR;   /* local status */

	/* executable code */

	if  (mmh_illchan(wdw))  return;
	mf_writeint( wdw, ID_DRAWTO, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );
	mf_writeint( wdw, style, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );
	mf_writereal( wdw, x, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );
	mf_writereal( wdw, y, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );

} /* end of mm_drawto */



/*------------------------------------------------------------------------*/



void mm_setpixel( int wdw, int style, GBC_COO x, GBC_COO y )

/* sets pixel at user coordinates (x,y)
 *
 * parameters of routine
 * int        wdw;      input; channel number
 * int        style;    input; style block number
 * GBC_COO    x, y;     input; coordinates
 */
{
	/* local variables */
	STATUS   locstat=BC_NOERROR;   /* local status */

	/* executable code */

	if  (mmh_illchan(wdw))  return;
	mf_writeint( wdw, ID_SETPIXEL, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );
	mf_writeint( wdw, style, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );
	mf_writereal( wdw, x, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );
	mf_writereal( wdw, y, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );

} /* end of mm_setpixel */



/*------------------------------------------------------------------------*/



void mm_arrayplot( int wdw, int style, long cnt, int red,
	GBC_COO xoff, GBC_COO xinc, GBC_COO yoff, GBC_COO yarr[],
	float yzoom, STATUS *status )

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
{
	/* local variables */
	long     i;       /* counter */

	/* executable code */

	mf_writeint( wdw, ID_ARRAY, status );
	if  (Severe(status))  return;
	mf_writeint( wdw, style, status );
	if  (Severe(status))  return;
	mf_writelong( wdw, cnt, status );
	if  (Severe(status))  return;
	mf_writeint( wdw, red, status );
	if  (Severe(status))  return;
	mf_writereal( wdw, xoff, status );
	if  (Severe(status))  return;
	mf_writereal( wdw, xinc, status );
	if  (Severe(status))  return;
	mf_writereal( wdw, yoff, status );
	for  (i=0;i<cnt;i++)  mf_writereal( wdw, *yarr++, status );
	if  (Severe(status))  return;
	mf_writereal( wdw, yzoom, status );
	if  (Severe(status))  return;

} /* end of mm_arrayplot */



/*------------------------------------------------------------------------*/



void mm_erase( int wdw )

/* erases channel
 *
 * parameter of routine
 * int       wdw;     input; channel number
 */
{
	/* local variables */
	STATUS   locstat=BC_NOERROR;    /* local status */

	/* executable code */

	mf_rewind( wdw, &locstat );
	if  (Severe(&locstat))  printf( "*** MM rewind error ***\n" );
	mmv_startcoo_x[wdw] = mmv_currcoo_x[wdw];
	mmv_startcoo_y[wdw] = mmv_currcoo_y[wdw];
	mmv_startcoo_w[wdw] = mmv_currcoo_w[wdw];
	mmv_startcoo_h[wdw] = mmv_currcoo_h[wdw];

} /* end of mm_erase */



/*------------------------------------------------------------------------*/



void mm_text( int wdw, int style, GBC_COO x, GBC_COO y, char text[] )

/* writes text at position (x,y)
 *
 * parameters of routine
 * int        wdw;     input; channel number
 * int        style;   input; style block number
 * GBC_COO    x, y;    input; coordinates
 * char       text[];  input; text to be printed
 */
{
	/* local variables */
	STATUS   locstat=BC_NOERROR;    /* local status */

	/* executable code */

	mf_writeint( wdw, ID_TEXT, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );
	mf_writeint( wdw, style, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );
	mf_writereal( wdw, x, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );
	mf_writereal( wdw, y, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );
	mf_writestr( wdw, text, &locstat );
	if  (Severe(&locstat))  printf( "*** MM store error ***\n" );

} /* end of mm_text */



/*------------------------------------------------------------------------*/



void mm_setstyle( int wdw, int style, char item[], char value[], STATUS *status )

/* writes text at position (x,y)
 *
 * parameters of routine
 * int        wdw;     input; channel number
 * int        style;   input; style block number
 * char       item[];  input; item name
 * char       value[]; input; value
 * STATUS     *status; output; return status
 */
{
	/* local variables */

	/* executable code */

	mf_writeint( wdw, ID_SETSTYLE, status );
	if  (Severe(status))  printf( "*** MM store error ***\n" );
	mf_writeint( wdw, style, status );
	if  (Severe(status))  printf( "*** MM store error ***\n" );
	mf_writestr( wdw, item, status );
	if  (Severe(status))  printf( "*** MM store error ***\n" );
	mf_writestr( wdw, value, status );
	if  (Severe(status))  printf( "*** MM store error ***\n" );

} /* end of mm_setstyle */



/*------------------------------------------------------------------------*/



void mm_playback( int src, int dst, PAINTBOX *pb, char outf[], STATUS *status )

/* redraws vectors sored in memory
 *
 * parameters of routine
 * int        src;      input; source channel
 * int        dst;      input; destination channel
 * PAINTBOX   *pb;      input; paintbox routines
 * char       outf[];   output; name of output file (if any)
 * STATUS     *status;  output; return status
 */
{
	/* local variables */
	STATUS   locstat;            /* local status */
	int      cmd_id;             /* command ID */
	float    rpar[4];            /* float parameters */
	int      ipar[2];            /* integer parameters */
	long     lpar;               /* long parameter */
	char     spar[BC_LINELTH+1]; /* string parameter */
	char     spar2[BC_LINELTH+1];/* 2. string parameter */
	GBC_COO  *yptr, *ycop;       /* pointer to sample array */
	long     i;                  /* counter */

	/* executable code */

	if  (pb->wpb.pbtype == GBC_WPAINT)  {
		(pb->wpb.prepare)( dst, status );
		if  (Severe(status))  return;
      (pb->wpb.setcoo)( dst, mmv_startcoo_x[src], mmv_startcoo_y[src],
			mmv_startcoo_w[src], mmv_startcoo_h[src], status );
	} else {
		(pb->ppb.prepare)( status );
		if  (Severe(status))  return;
      (pb->ppb.setcoo)( mmv_startcoo_x[src], mmv_startcoo_y[src],
			mmv_startcoo_w[src], mmv_startcoo_h[src], status );
	} /*endif*/
	if  (Severe(status))  return;

	/* if no hardcopy stream return */
	if  (!mf_isopen(src))  return;

	mf_close( src, status );
	if  (Severe(status))  return;
	mf_open( src, TRUE, "r", status );
   if  (Severe(status))  return;

	locstat = BC_NOERROR;
	FOREVER  {
		cmd_id = mf_readint( src, &locstat );
		if  (locstat != BC_NOERROR)  break;
      switch  (cmd_id)  {
      case ID_MOVETO:
			rpar[0] = mf_readreal( src, status );
			rpar[1] = mf_readreal( src, status );
			if  (Severe(status))  break;
			if  (pb->wpb.pbtype == GBC_WPAINT)  {
	         (pb->wpb.moveto)( dst, rpar[0], rpar[1] );
			} else {
	         (pb->ppb.moveto)( rpar[0], rpar[1] );
			} /*endif*/
         break;
      case ID_DRAWTO:
			ipar[0] = mf_readint( src, status );
			rpar[0] = mf_readreal( src, status );
			rpar[1] = mf_readreal( src, status );
			if  (Severe(status))  break;
			if  (pb->wpb.pbtype == GBC_WPAINT)  {
	         (pb->wpb.drawto)( dst, ipar[0], rpar[0], rpar[1] );
			} else {
	         (pb->ppb.drawto)( ipar[0], rpar[0], rpar[1] );
			} /*endif*/
         break;
      case ID_ARRAY:
			ipar[0] = mf_readint( src, status );
			lpar = mf_readlong( src, status );
			ipar[1] = mf_readint( src, status );
			rpar[0] = mf_readreal( src, status );
			rpar[1] = mf_readreal( src, status );
			rpar[2] = mf_readreal( src, status );
			if  (Severe(status))  break;
         yptr = (GBC_COO *)sy_allocmem( lpar, (int)sizeof(GBC_COO), status );
         if  (Severe(status))  {
            mf_close( src, status );
            return;
         } /*endif*/
         ycop = yptr;
         for  (i=0;i<lpar;i++)  *yptr++ = mf_readreal( src, status );
			rpar[3] = mf_readreal( src, status );
			if  (Severe(status))  break;
			if  (pb->wpb.pbtype == GBC_WPAINT)  {
	         (pb->wpb.arrayplot)( dst, ipar[0], lpar, ipar[1], rpar[0],
					rpar[1],	rpar[2], ycop, rpar[3], status );
			} else {
	         (pb->ppb.arrayplot)( ipar[0], lpar, ipar[1], rpar[0],
					rpar[1],	rpar[2], ycop, rpar[3], status );
			} /*endif*/
         sy_deallocmem( ycop );
         if  (Severe(status))  {
            mf_close( src, status ); /* fclose( fp ); */
            return;
         } /*endif*/
         break;
      case ID_TEXT:
			ipar[0] = mf_readint( src, status );
			rpar[0] = mf_readreal( src, status );
			rpar[1] = mf_readreal( src, status );
			mf_readstr( src, spar, status );
			if  (Severe(status))  break;
			if  (pb->wpb.pbtype == GBC_WPAINT)  {
	         (pb->wpb.text)( dst, ipar[0], rpar[0], rpar[1], spar );
			} else {
	         (pb->ppb.text)( ipar[0], rpar[0], rpar[1], spar );
			} /*endif*/
         break;
      case ID_SETSTYLE:
			ipar[0] = mf_readint( src, status );
			mf_readstr( src, spar, status );
			mf_readstr( src, spar2, status );
			if  (Severe(status))  break;
			if  (pb->wpb.pbtype == GBC_WPAINT)  {
	         (pb->wpb.setstyle)( dst, ipar[0], spar, spar2, status );
			} else {
	         (pb->ppb.setstyle)( ipar[0], spar, spar2, status );
			} /*endif*/
         break;
      case ID_SETCOO:
			rpar[0] = mf_readreal( src, status );
			rpar[1] = mf_readreal( src, status );
			rpar[2] = mf_readreal( src, status );
			rpar[3] = mf_readreal( src, status );
			if  (Severe(status))  break;
			if  (pb->wpb.pbtype == GBC_WPAINT)  {
	         (pb->wpb.setcoo)( dst, rpar[0],rpar[1],rpar[2],rpar[3], status );
			} else {
	         (pb->ppb.setcoo)( rpar[0],rpar[1],rpar[2],rpar[3], status );
			} /*endif*/
         break;
      default:
         *status = GCE_UDID;
			mf_close( src, status );
         return;
      } /*endswitch*/
		if  (Severe(status))  break;
   } /*endwhile*/

	if  (pb->wpb.pbtype == GBC_WPAINT)  {
        (pb->wpb.cleanup)( dst, outf, status );
	} else {
        (pb->ppb.cleanup)( outf, status );
	} /*endif*/

	mf_close( src, status );
	mf_open( src, TRUE, "a", status );

} /* end of mm_playback */



/*------------------------------------------------------------------------*/

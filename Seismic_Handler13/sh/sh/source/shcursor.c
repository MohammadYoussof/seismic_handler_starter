
/* file SHCURSOR.C
 *      ==========
 *
 * version 6, 22-May-2006
 *
 * graphic cursor management
 *
 * K. Stammler, 23-MAY-1990
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1992,  Klaus Stammler, Federal Institute for Geosciences
 *                                       and Natural Resources (BGR), Germany
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */


#include <stdio.h>
#include "basecnst.h"
#include BC_SYSBASE
#include "shconst.h"
#include "shvars.h"
#include BC_GCUSRDEF
#include "uiusrdef.h"
#include "tcusrdef.h"
#include "infoidx.h"
#include "fctxcr.h"
#include "fctxdm.h"
#include "sherrors.h"



/*---------------------------------------------------------------------------*/



void cr_window( int mode, float *lo, float *hi, int *status )

/* selects time window on display
 *
 * parameters of routine
 * int        mode;      input; selection mode (mark)
 * float      *lo;       output; lower window bound
 * float      *hi;       output; upper window bound
 * int        *status;   output; return status
 */
{
	/* local variables */
	float    tmp;      /* scratch */

	/* executable code */

	cr_getloc( mode, lo, NULL, NULL, status );
	if  (*status != SHE_NOERROR)  return;
	cr_getloc( mode, hi, NULL, NULL, status );
	if  (*status != SHE_NOERROR)  return;
	if  (*lo > *hi)  {
		tmp = *lo;
		*lo = *hi;
		*hi = tmp;
	} /*endif*/

} /* end of cr_window */



/*---------------------------------------------------------------------------*/



void cr_ywindow( int mode, float *lo, float *hi, int *status )

/* selects y-window on display
 *
 * parameters of routine
 * int        mode;      input; selection mode (mark)
 * float      *lo;       output; lower window bound
 * float      *hi;       output; upper window bound
 * int        *status;   output; return status
 */
{
	/* local variables */
	float    tmp;      /* scratch */

	/* executable code */

	cr_getloc( mode, NULL, lo, NULL, status );
	if  (*status != SHE_NOERROR)  return;
	cr_getloc( mode, NULL, hi, NULL, status );
	if  (*status != SHE_NOERROR)  return;
	if  (*lo > *hi)  {
		tmp = *lo;
		*lo = *hi;
		*hi = tmp;
	} /*endif*/

} /* end of cr_ywindow */



/*---------------------------------------------------------------------------*/



void cr_tywindow( int mode, float *tlo, float *thi, float *ylo,
	float *yhi, int *status )

/* selects time & y-window on display
 *
 * parameters of routine
 * int        mode;       input; selection mode (mark)
 * float      *tlo, *thi; output; time window
 * float      *ylo, *yhi; output; y-window
 * int        *status;    output; return status
 */
{
	/* local variables */
	float    tmp;      /* scratch */

	/* executable code */

	cr_getloc( mode, tlo, ylo, NULL, status );
	if  (*status != SHE_NOERROR)  return;
	cr_getloc( mode, thi, yhi, NULL, status );
	if  (*status != SHE_NOERROR)  return;
	if  (*tlo > *thi)  {
		tmp = *tlo;
		*tlo = *thi;
		*thi = tmp;
	} /*endif*/
	if  (*ylo > *yhi)  {
		tmp = *ylo;
		*ylo = *yhi;
		*yhi = tmp;
	} /*endif*/

} /* end of cr_tywindow */



/*---------------------------------------------------------------------------*/



void cr_gettrctime( int mode, TRACE **trc, REAL *time, int *status )

/* let user select a trace by graphic cursor and returns trace pointer
 * and time selected
 *
 * parameters of routine
 * int        mode;      input; select mode
 * TRACE      **trc;     output; trace pointer
 * REAL       *time;     output; select time
 * int        *status;   output; return status
 */
{
	/* local variables */
	REAL     y;     /* selected location */

	/* executable code */

	cr_getloc( mode, time, &y, NULL, status );
	if  (Severe(status))  return;
	*trc = dm_trace( gc, y, status );
	if  (Severe(status))  *trc = NULL;

} /* end of cr_gettrctime */



/*---------------------------------------------------------------------------*/



TRACE *cr_gettrc( int *status )

/* let user select a trace by graphic cursor and returns trace pointer
 *
 * parameters of routine
 * int        *status;   output; return status
 *                       returns trace pointer
 */
{
	/* local variables */
	REAL     y;     /* selected location */

	/* executable code */

	cr_getloc( MM_NOMARK, NULL, &y, NULL, status );
	if  (*status != SHE_NOERROR)  return NULL;
	return dm_trace( gc, y, status );

} /* end of cr_gettrc */



/*---------------------------------------------------------------------------*/



void cr_getloc( int mode, float *x, float *y, char *ch, int *status )

/* returns location selected by user
 *
 * parameters of routine
 * int        mode;     input; selection mode
 * float      *x, *y;   output; selected position
 * char       *ch;      output; key pressed for selection
 * int        *status;  output; return status
 */
{
	/* local variables */
	float    lx, ly;    /* location */
	char     lch;       /* key */
	TRACE    *trc;      /* trace pointer */

	/* executable code */

	ui_getloc( gc, &lx, &ly, &lch, status );
	lch = Cap( lch );
	if  (ch != NULL)  *ch = lch;
	if  (*status != SHE_NOERROR)  return;
	if  (lch == SHC_ABORTCH)  { *status = SHE_ABORT; return; }
	if  (lch == SHC_EXITCH)   { *status = SHE_EXIT; return; }
	if  (x != NULL)  *x = lx;
	if  (y != NULL)  *y = ly;
	if  (mode == MM_PTMARK)  {
		cr_ptmark( lx, ly );
	} else if  (mode == MM_TRCMARK)  {
		trc = dm_trace( gc, ly, status );
		if  (*status != SHE_NOERROR)  return;
		cr_trcmark( trc, lx, 1.0 );
	} /*endif*/

} /* end of cr_getloc */



/*---------------------------------------------------------------------------*/



void cr_ptmark( float x, float y )

/* marks point on display
 *
 * parameters of routine
 * float      x, y;    input; position of mark
 */
{
	/* local variables */
	float    mwidth, mheight;   /* (half) size of mark */

	/* executable code */

	dm_dspcoo( NULL, NULL, &mwidth, &mheight );
	mwidth /= 20.0;
	mheight /= 20.0;

	/* horizontal line */
	gc_moveto( gc, x-mwidth, y );
	gc_drawto( gc, SHC_MARKSTYLE, x+mwidth, y );
	/* vertical line */
	gc_moveto( gc, x, y-mheight );
	gc_drawto( gc, SHC_MARKSTYLE, x, y+mheight );
	gc_flushbuffers();

} /* end of cr_ptmark */



/*---------------------------------------------------------------------------*/



void cr_trcmark( TRACE *trc, REAL time, REAL size )

/* marks trace at time "time"
 *
 * parameters of routine
 * TRACE      *trc;      input; trace pointer
 * REAL       time;      input; time position
 * REAL       size;      input; size of marker (from 0.0 to 1.0)
 */
{
	/* local variables */
	REAL     y;        /* y position of mark */

	/* executable code */

	size /= 2.0;
	y = db_getr( trc, ER_SORIG, NULL );
	gc_moveto( gc, time, y-size );
	gc_drawto( gc, SHC_MARKSTYLE, time, y+size );
	gc_flushbuffers();

} /* end of cr_trcmark */



/*---------------------------------------------------------------------------*/



void cr_trcmark_label( TRACE *trc, REAL time, char label[],
	REAL pos, REAL size )

/* marks trace at time "time"
 *
 * parameters of routine
 * TRACE      *trc;      input; trace pointer
 * REAL       time;      input; time position
 * char       label[];   input; label name
 * REAL       pos;       input; position of label (from 0.0 to 1.0)
 * REAL       size;      input; size of label (from 0.0 to 1.0)
 */
{
	/* local variables */
	REAL     y;        /* y position of mark */

	/* executable code */

	pos *= size;
	size /= 2.0;

	y = db_getr( trc, ER_SORIG, NULL );
	gc_moveto( gc, time, y-size );
	gc_drawto( gc, SHC_MARKSTYLE, time, y+size );
	gc_text( gc, SHC_TRCINFOSTYLE, time, y-size+pos, label );
	gc_flushbuffers();

} /* end of cr_trcmark_label */



/*---------------------------------------------------------------------------*/

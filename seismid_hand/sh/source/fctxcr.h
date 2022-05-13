
/* file FCTXCR.H
 *      ========
 *
 * version 4, 22-May-2006
 *
 * prototypes of module SHCURSOR.C
 * K. Stammler, 22-MAY-91
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1996,  Klaus Stammler, Federal Institute for Geosciences
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

#ifndef __FCTXCR
#define __FCTXCR

#ifndef __SHCONST
#include "shconst.h"
#endif

#define MM_NOMARK 1
#define MM_PTMARK 2
#define MM_TRCMARK 3


/*---------------------------------------------------------------------------*/


void cr_window( int mode, float *lo, float *hi, int *status );

/* selects time window on display
 *
 * parameters of routine
 * int        mode;      input; selection mode (mark)
 * float      *lo;       output; lower window bound
 * float      *hi;       output; upper window bound
 * int        *status;   output; return status
 */


/*---------------------------------------------------------------------------*/


void cr_ywindow( int mode, float *lo, float *hi, int *status );

/* selects y-window on display
 *
 * parameters of routine
 * int        mode;      input; selection mode (mark)
 * float      *lo;       output; lower window bound
 * float      *hi;       output; upper window bound
 * int        *status;   output; return status
 */


/*---------------------------------------------------------------------------*/


void cr_tywindow( int mode, float *tlo, float *thi, float *ylo,
	float *yhi, int *status );

/* selects time & y-window on display
 *
 * parameters of routine
 * int        mode;       input; selection mode (mark)
 * float      *tlo, *thi; output; time window
 * float      *ylo, *yhi; output; y-window
 * int        *status;    output; return status
 */


/*---------------------------------------------------------------------------*/


void cr_gettrctime( int mode, TRACE **trc, REAL *time, int *status );

/* let user select a trace by graphic cursor and returns trace pointer
 * and time selected
 *
 * parameters of routine
 * int        mode;      input; select mode
 * TRACE      **trc;     output; trace pointer
 * REAL       *time;     output; select time
 * int        *status;   output; return status
 */


/*---------------------------------------------------------------------------*/


TRACE *cr_gettrc( int *status );

/* let user select a trace by graphic cursor and returns trace pointer
 *
 * parameters of routine
 * int        *status;   output; return status
 *                       returns trace pointer
 */


/*---------------------------------------------------------------------------*/


void cr_getloc( int mode, float *x, float *y, char *ch, int *status );

/* returns location selected by user
 *
 * parameters of routine
 * int        mode;     input; selection mode
 * float      *x, *y;   output; selected position
 * char       *ch;      output; key pressed for selection
 * int        *status;  output; return status
 */


/*---------------------------------------------------------------------------*/


void cr_ptmark( float x, float y );

/* marks point on display
 *
 * parameters of routine
 * float      x, y;    input; position of mark
 */


/*---------------------------------------------------------------------------*/


void cr_trcmark( TRACE *trc, REAL time, REAL size );

/* marks trace at time "time"
 *
 * parameters of routine
 * TRACE      *trc;      input; trace pointer
 * REAL       time;      input; time position
 * REAL       size;      input; size of marker (from 0.0 to 1.0)
 */


/*---------------------------------------------------------------------------*/


void cr_trcmark_label( TRACE *trc, REAL time, char label[],
	REAL pos, REAL size );

/* marks trace at time "time"
 *
 * parameters of routine
 * TRACE      *trc;      input; trace pointer
 * REAL       time;      input; time position
 * char       label[];   input; label name
 * REAL       pos;       input; position of label (from 0.0 to 1.0)
 * REAL       size;      input; size of mark (from 0.0 to 1.0)
 */


/*---------------------------------------------------------------------------*/

#endif /* __FCTXCR */

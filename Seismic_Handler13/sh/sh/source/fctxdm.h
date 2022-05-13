
/* file FCTXDM.H
 *      ========
 *
 * version 4, 22-May-2006
 *
 * prototypes of module SHDSPMGR
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

#ifndef __FCTXDM
#define __FCTXDM

#ifndef __SHCONST
#include "shconst.h"
#endif

/*------------------------------------------------------------------------*/


void dm_setup( int *status );

/* sets up display parameters. Called before each redraw */

/* parameters of routine */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void dm_redraw( int *status );

/* redraws display */

/* parameters of routine */
/* int      *status;      output; return status */


/*------------------------------------------------------------------------*/


long dm_getsample( void *ptr, float time, int check );

/* returns the sample point located at time. If check then array bounds are
   corrected */

/* parameters of routine */
/* void     *ptr;           input; info block pointer */
/* float    time;           input; time position */
/* int      check;          input; array bound check flag */
/* 								 returns sample point index */


/*------------------------------------------------------------------------*/


long dm_sgetsample( void *ptr, REAL time, int *status );

/* returns the sample point located at time.
 *
 * parameters of routine
 * char     *ptr;           input; info block pointer
 * float    time;           input; time position
 * int      *status;        input; array bound check flag
 *                          returns sample point index
 */


/*------------------------------------------------------------------------*/


void *dm_trace( int dln, REAL ypos, int *status );

/* returns trace pointer; input is y-position
 *
 * paramters of routine
 * int       dln;        input; display list number
 * REAL      ypos;       input; y-position selected
 * int       *status;    output; return status
 *                       returns trace pointer
 */


/*------------------------------------------------------------------------*/


void dm_norm( void );

/* sets normalisation constants of each trace */


/*------------------------------------------------------------------------*/


void dm_setnorm( int norm );

/* sets normalisation type
 *
 * parameter of routine
 * int       norm;        input; normalisation type
 */


/*------------------------------------------------------------------------*/


void dm_timewdw( float lo_t, float hi_t, int *status );

/* sets time window in display */

/* parameters of routine */
/* float		lo_t;			input; lower bound of time window */
/* float		hi_t;			input; upper bound of time window */
/* int		*status;		output; return status */


/*------------------------------------------------------------------------*/


void dm_get_timewdw( float *lo_t, float *hi_t );

/* returns time window in display
 *
 * parameters of routine
 * float      *lo_t;         input; lower bound of time window
 * float      *hi_t;         input; upper bound of time window
 */


/*------------------------------------------------------------------------*/


void dm_ywdw( float lo_y, float hi_y, int *status );

/* sets y-window in display
 *
 * parameters of routine
 * float		lo_y;			input; lower bound of y-window
 * float		hi_y;			input; upper bound of y-window
 * int		*status;		output; return status
 */


/*------------------------------------------------------------------------*/


void dm_setyentry( unsigned ientry, STATUS *status );

/* sets y-entry for y-display arrangement
 *
 * parameters of routine
 * unsigned   ientry;      input; y-entry or E_NONE
 * STATUS     *status;     output; return status
 */


/*------------------------------------------------------------------------*/


void dm_inftext( char *str, int *status );

/* sets the info text of all traces */

/* parameter of routine */
/* char      *str;      input; new info text */
/* int       *status;   output; return status */

/*------------------------------------------------------------------------*/


void dm_infpos( float xfac, float yfac );

/* sets the position factors of trace info text */

/* parameters of routine */
/* float      xfac;      input; x-position factor, units of dspw_dmv */
/* float      yfac;      input; y-position factor, units of dsph_dmv */


/*------------------------------------------------------------------------*/


void dm_setaxis( char *name, char *item, char *value, int *status );

/* sets item in axis descriptor "name" */

/* parameters of routine */
/* char		*name;		input; name of descriptor */
/* char		*item;		input; name of item */
/* char		*value;		input; new value */
/* int		*status;		output; return status */


/*------------------------------------------------------------------------*/


void dm_dspcoo( float *x, float *y, float *w, float *h );

/* returns current display coordinates */

/* parameters of routine */
/* float *x, *y, *w, *h;    output; x-pos, y-pos, width & height */


/*------------------------------------------------------------------------*/


void dm_setmargin( int dln, char id, float margin, STATUS *status );

/* sets new value of margin
 *
 * parameters of routine
 * int        dln;     input; display list number
 * char       id;      input; L(eft), R(ight), T(op), B(ottom)
 * float      margin;  input; margin value
 * STATUS     *status; output; return status
 */


/*------------------------------------------------------------------------*/


void dm_settitle( int dln, unsigned lineno, char text[], STATUS *status );

/* sets new title text of line "lineno"
 *
 * parameters of routine
 * int        dln;          input; display list number
 * unsigned   lineno;       input; line number
 * char       text[];       input; new text of title line
 * STATUS     *status;      output; return status
 */


/*------------------------------------------------------------------------*/


void dm_settitlepos( int dln, unsigned lineno, float xpos, float ypos,
	STATUS *status );

/* sets new position of title line "lineno" in units of width and
 * height of display
 *
 * parameters of routine
 * int        dln;          input; display list number
 * unsigned   lineno;       input; line number
 * float      xpos, ypos;   input; new position of title
 * STATUS     *status;      output; return status
 */


/*------------------------------------------------------------------------*/


void dm_amplicut( float cut );

/* sets cut off value for amplitudes
 *
 * parameters of routine
 * float      cut;    input; cut off value
 */


/*------------------------------------------------------------------------*/


void dm_setoverlay( TRACE *list[], int lth, STATUS *status );

/* defines new overlay list
 *
 * parameters of routine
 * TRACE      *list[];    input; list to be added
 * int        lth;        input; length of list
 * STATUS     *status;    output; return status
 */


/*------------------------------------------------------------------------*/


void dm_deloverlays( void );

/* deletes all overlays
 * no parameters
 */


/*------------------------------------------------------------------------*/


BOOLEAN dm_infstr( int maxlth, char *str, STATUS *status );

/* returns info string to display
 *
 * parameters of routine
 * int       maxlth;    input; maximum length of string
 * char      *str;      output; info string
 * STATUS    *status;   output; return status
 *                      returns whether a string is available
 */


/*------------------------------------------------------------------------*/

#endif /* __FCTXDM */

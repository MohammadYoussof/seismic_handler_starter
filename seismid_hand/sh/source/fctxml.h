
/* file FCTML.H
 *      =======
 *
 * version 5, 22-Nov-2006
 *
 * prototypes of module SHMIDLEV.C
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


#ifndef __FCTXML
#define __FCTXML

#ifndef __SHCONST
#include "shconst.h"
#endif
#ifndef __CPUSRDEF
#include "cpusrdef.h"
#endif



/*--------------------------------------------------------------------------*/


void ml_newstarttime( TRACE *src, TRACE *dst, REAL wdwstart,
	STATUS *status );

/* sets starttime of "dst" trace using starttime of "src" trace and
 * time window start "wdwstart"
 *
 * parameters of routine
 * TRACE      *src;     input; source trace
 * TRACE      *dst;     modify; trace which starttime is to be set
 * REAL       wdwstart; input; start time of window
 * STATUS     *status;  output; return status
 */


/*------------------------------------------------------------------------*/


void ml_inittrc( TRACE *trc, SAMPLE *datptr, long lth, float dt );

/* sets the required info entries of "trc"
 *
 * parameters of routine
 * TRACE    *trc;       input; trace pointer
 * SAMPLE   *datptr;    input; data array
 * long     lth;        input; length of data array
 * float    dt;         input; sample distance
 */


/*------------------------------------------------------------------------*/


void ml_defaults( SHENTRY ientry, void *infptr, STATUS *status );

/* sets new default values
 *
 * parameters of routine
 * SHENTRY    ientry;     input; infoe entry number
 * void       *infptr;    input; pointer to new info value
 * STATUS     *status;    output; return status
 */


/*------------------------------------------------------------------------*/


void ml_trclist( char *liststr, TRACE *trclist[],
	int *listlth, STATUS *status );

/* creates list of trace pointers from character string. Maximum
 * length of trace list is SHC_ILISTLTH
 *
 * parameters of routine
 * char     *liststr;      input; list string (position numbers)
 * TRACE    *trclist;      output; list of trace pointers
 * int      *listlth;      output; length of trace list
 * STATUS   *status;       output; return status
 */


/*------------------------------------------------------------------------*/


void ml_gettrcpar( PARAM *par, int pos, int wdw, char *prompt,
	TRACE *trclist[], int *listlth, STATUS *status );

/* extracts trace list from parameter number "pos".  Calls cursor
 * selection routine if no string is available or if the string
 * cannot be converted to a trace list.
 *
 * parameters of routine
 * PARAM    *par;          input; menu parameter
 * int      pos;           input; parameter number
 * int      wdw;           input; window number for prompting
 * char     *prompt;       input; prompt string
 * TRACE    *trclist[];    output; trace list
 * int      *listlth;      output; length of list
 * STATUS   *status;       output; return status
 */


/*------------------------------------------------------------------------*/


TRACE *ml_trc( int dln, int pos, STATUS *status );

/* returns trace pointer of "pos"-th trace in display
 *
 * parameters of routine
 * int      dln;           input; display list number
 * int      pos;           input; position of trace
 * STATUS   *status;			output; return status
 */


/*------------------------------------------------------------------------*/


TRACE *ml_get1trcpar( PARAM *par, int pos, int wdw, char *prompt,
	STATUS *status );

/* returns trace pointer obtained from "pos"-th parameter of "par"
 * If the parameter is not available the cursor selection routine
 * is called.
 *
 * parameters of routine
 * PARAM      *par;      input; command line parameters
 * int        pos;       input; number of parameter
 * int        wdw;       input; window for prompting
 * char       *prompt;   input; prompt string
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void ml_windowpars( PARAM *par, int pos, int wdw, char loprompt[],
	char hiprompt[], REAL *lo, REAL *hi, STATUS *status );

/* returns window selected by user in command line "par" at positions
 * "pos" and "pos+1" or by graphic cursor
 *
 * parameters of routine
 * PARAM      *par;       input; command line
 * int        pos;        input; number of lo-wdw parameter (hi is pos+1)
 * CHMAP      wdw;        input; prompt window
 * char       loprompt[]; input; prompt string for lower bound
 * char       hiprompt[]; input; prompt string for upper bound
 * REAL       *lo, *hi;   output; selected window
 * STATUS     *status;    output; return status
 */


/*------------------------------------------------------------------------*/


void ml_crsrlst_time( int mode, TRACE *trc[], REAL time[],
	int *listlth, STATUS *status );

/* let user select a list of traces by graphic cursor.  The selected
 * time positions of each trace is returned in "time"-array
 *
 * parameters of routine
 * int        mode;      input; selection mode (mark time points)
 * TRACE      *trc[];    output; trace list
 * REAL       time[];    output; selected time positions
 * int        *listlth;  output; number of traces selected
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void ml_crsrlst( TRACE *trc[], int *listlth, STATUS *status );

/* let user select a list of traces by graphic cursor.
 *
 * parameters of routine
 * TRACE      *trc[];    output; trace list
 * int        *listlth;  output; number of traces selected
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void ml_cpyinfo( TRACE *src, TRACE *dst, STATUS *status );

/* copies info from source to destination trace
 *
 * parameters of routine
 * TRACE      *src;         input; source trace pointer
 * TRACE      *dst;         input; destination trace pointer
 * STATUS     *status;      output; return status
 */


/*--------------------------------------------------------------------------*/


int ml_listwdw( char liststr[], STATUS *status );

/* returns window of list string
 *
 * parameters of routine
 * char       liststr[];    input; list string
 * STATUS     *status;      output; return status
 *                          returns window number
 */


/*--------------------------------------------------------------------------*/


void ml_resample( TRACE *trc, REAL new_dt, STATUS *status );

/* peforms resampling of a trace
 *
 * parameters of routine
 * TRACE      *trc;      input; trace pointer
 * REAL       new_dt;    input; new delta t
 * STATUS     *status;   output; return status
 */


/*--------------------------------------------------------------------------*/


void ml_decimate( TRACE *trc, int decimation, BOOLEAN nomean, STATUS *status );

/* peforms decimation of a trace using mean values
 *
 * parameters of routine
 * TRACE      *trc;        input; trace pointer
 * int        decimation;  input; decimation factor
 * BOOLEAN    nomean;      input; take no mean value, plain resampling
 * STATUS     *status;     output; return status
 */


/*--------------------------------------------------------------------------*/


void ml_cut( TRACE *trc, REAL lowdw, REAL hiwdw, SHENTRY entry,
	REAL vel, STATUS *status );

/* cuts a trace at (relative) times "lowdw" and "hiwdw" if entry=0.
 * if entry is of time-type, the time window bounds refer to the
 * time info entry of the trace, if entry is of real type it is
 * assumed to describe a distance and "vel" is used to compute
 * a reduced velocity time window.
 *
 * parameters of routine
 * TRACE      *trc;          input; trace pointer
 * REAL       lowdw, hiwdw;  input; time window
 * SHENTRY    entry;         input; info entry or 0
 * REAL       vel;           input; velocity for reduced velocity cut
 * STATUS     *status;       output; return status
 */


/*--------------------------------------------------------------------------*/


void ml_reduced_time( int listlth, TRACE *trc[], REAL vel, STATUS *status );

/* makes reduced time shifts
 *
 * parameters of routine
 * int        listlth;      input; length of trace list
 * TRACE      *trc[];       input; trace list
 * REAL       vel;          input; reduction velocity
 * STATUS     *status;      output; return status
 */


/*--------------------------------------------------------------------------*/


void ml_time_align( int listlth, TRACE *trc[], char *altime, STATUS *status );

/* shifts traces to fit absolute times
 *
 * parameters of routine
 * int        listlth;      input; length of trace list
 * TRACE      *trc[];       input; trace list
 * char       *altime;      input; shift reference time string
 * STATUS     *status;      output; return status
 */


/*--------------------------------------------------------------------------*/


void ml_sddelay( int listlth, TRACE *trc[], REAL slowness,
	char diststr[], STATUS *status );

/* adds slowness-distance delay to traces listed in trc[0..listlth-1]
 *
 * parameters of routine
 * int        listlth;     input; length of trace list
 * TRACE      *trc[];      input; trace list
 * REAL       slowness;    input; slowness
 * char       diststr[];   input; distance info entry descr. string
 * STATUS     *status;     output; return status
 */


/*--------------------------------------------------------------------------*/

#endif /* __FCTXML */

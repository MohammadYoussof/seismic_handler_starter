
/* file TCUSRDEF.H
 *      ==========
 *
 * version 4, 22-May-2006
 *
 * v 3: 29-Nov-94, K. Stammler, new naming conventions
 *
 * types and prototypes of module TIMECONV.C
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


#ifndef __TCUSRDEF
#define __TCUSRDEF

#define TCC_TIMSTRLTH 30
#define cTcTimStrLth TCC_TIMSTRLTH

typedef struct timblc {
   unsigned long     hi;
   unsigned int      lo;
} TIME;
#define STcTime TIME

typedef struct ntimblc {
   unsigned int      year, month, day;  /* date */
   unsigned int      hour, min, sec;    /* time */
   unsigned int      ms;                /* milliseconds */
} NTIME;
#define STcNtime NTIME


/*----------------------------------------------------------------------*/


#define TcT2n tc_t2n

void tc_t2n( char text[], NTIME *ntime, STATUS *status );

/* converts from text format to numeric format
 *
 * parameters of routine
 * char     text[];        input; text time format
 * NTIME    *ntime;        output; numeric time
 * STATUS   *status;       output; return status
 */


/*----------------------------------------------------------------------*/


#define TcStd2n tc_std2n

void tc_std2n( char text[], NTIME *ntime, STATUS *status );

/* converts from standard text format (e.g. 12-JAN-1990_5:30:20.001)
 * to numeric format
 *
 * parameters of routine
 * char     text[];         input; text time format
 * NTIME    *ntime;         output; numeric time
 * STATUS   *status;        output; return status
 */


/*----------------------------------------------------------------------*/


#define TcN2t tc_n2t

void tc_n2t( NTIME *ntime, char text[], STATUS *status );

/* converts numeric format to text format
 *
 * parameters of routine
 * NTIME    *ntime;         input; numeric time
 * char     text[];         output; text format
 * STATUS   *status;        output; return status
 */


/*----------------------------------------------------------------------*/


#define TcN2a tc_n2a

void tc_n2a( NTIME *ntime, TIME *time, STATUS *status );

/* converts numeric time to absolute time
 *
 * parameters of routine
 * NTIME    *ntime;         input; numeric time
 * TIME     *time;          output; absolute time
 * STATUS   *status;        output; return status
 */


/*----------------------------------------------------------------------*/


#define TcA2n tc_a2n

void tc_a2n( TIME *time, NTIME *ntime, STATUS *status );

/* converts absolute time to numeric time
 *
 * parameters of routine
 * TIME     *time;          input; absolute time
 * NTIME    *ntime;         output; numeric time
 * STATUS   *status;        output; return status
 */


/*----------------------------------------------------------------------*/


#define TcT2a tc_t2a

void tc_t2a( char text[], TIME *time, STATUS *status );

/* converts text format to absolute time
 *
 * parameters of routine
 * char     text[];         input; text time
 * TIME     *time;          output; absolute time
 * STATUS   *status;        output; return status
 */


/*----------------------------------------------------------------------*/


#define TcA2t tc_a2t

void tc_a2t( TIME *time, char text[], STATUS *status );

/* converts absolute time to text format
 *
 * parameters of routine
 * TIME     *time;          input; absolute time
 * char     text[];         output; text time
 * STATUS   *status;        output; return status
 */


/*----------------------------------------------------------------------*/


#define TcAdiff tc_adiff

float tc_adiff( TIME *atmin, TIME *atsub );

/* returns difference "atmin-atsub" (absolute times) in seconds
 *
 * parameters of routine
 * TIME     *atmin;        input; minuend
 * TIME     *atsub;        input; subtrahend
 *                         returns: difference in seconds
 */


/*----------------------------------------------------------------------*/


#define TcNdiff tc_ndiff

float tc_ndiff( NTIME *ntmin, NTIME *ntsub, STATUS *status );

/* returns difference "ntmin-ntsub" (numeric times) in seconds
 *
 * parameters of routine
 * NTIME    *ntmin;         input; minuend
 * NTIME    *ntsub;         input; subtrahend
 * STATUS   *status;        output; return status
 *                          returns: difference in seconds
 */


/*----------------------------------------------------------------------*/


#define TcTdiff tc_tdiff

float tc_tdiff( char ttmin[], char ttsub[], STATUS *status );

/* returns difference "ttmin-ttsub" (text format) in seconds
 *
 * parameters of routine
 * char     ttmin[];        input; minuend
 * char     ttsub[];        input; subtrahend
 * STATUS   *status;        output; return status
 *                          returns: difference in seconds
 */


/*----------------------------------------------------------------------*/


#define TcAadd tc_aad

void tc_aadd( TIME *time, float addtim, TIME *ares );

/* adds "addtim" seconds to "time" (absolute time)
 *
 * parameters of routine
 * TIME     *time;          input; input time
 * float    addtim;         input; seconds to be added
 * TIME     *ares;          output; result time
 */


/*----------------------------------------------------------------------*/


#define TcNadd tc_nadd

void tc_nadd( NTIME *ntime, float addtim, NTIME *nres, STATUS *status );

/* adds "addtim" seconds to "ntime" (numeric time)
 *
 * parameters of routine
 * NTIME    *ntime;         input; input time
 * float    addtim;         input; seconds to be added
 * NTIME    *nres;          output; result time
 * STATUS   *status;        output; return status
 */


/*----------------------------------------------------------------------*/


#define TcTadd tc_tadd

void tc_tadd( char text[], float addtim, char tres[], STATUS *status );

/* adds "addtim" seconds to "text" (text format)
 *
 * parameters of routine
 * char     text[];         input; input time
 * float    addtim;         input; seconds to be added
 * char     tres[];         output; result time
 * STATUS   *status;        output; return status
 */


/*----------------------------------------------------------------------*/


#define TcSetEmpty tc_setemtpy

void tc_setempty( TIME *time );

/* sets time to an empty value
 *
 * parameters of routine
 * TIME       *time;   output; time set to empty value
 */

/*----------------------------------------------------------------------*/

#define tc_isempty(t) ((t)->hi == -1)
#define TcIsEmpty(t) ((t)->hi == -1)

/*----------------------------------------------------------------------*/


#define TcGetMonth tc_getmonth

int tc_getmonth( char name[], STATUS *status );

/* returns number of month
 *
 * parameters of routine
 * char     name[];         input; name of month
 * STATUS   *status;        output; return status
 */


/*----------------------------------------------------------------------*/


#define TcJulian tc_julian

int tc_julian( unsigned year, unsigned month, unsigned day,
	STATUS *status );

/* computes julian date (number of day in year)
 *
 * parameters of routine
 * unsigned int  year, month, day;     input; date
 * STATUS        *status;              output; return status
 *                                     returns number of day
 */


/*----------------------------------------------------------------------*/


#define TcDayOfMn tc_dayofmn

void tc_dayofmn( unsigned year, unsigned julday, unsigned *month,
	unsigned *day, STATUS *status );

/* computes day of month & month from day of year
 *
 * parameters of routine
 * unsigned int  year;      input; year
 * unsigned int  julday;    input; julian day
 * unsigned int  *month;    output; month
 * unsigned int  *day;      output; day of month
 * STATUS        *status;   output; return status
 */


/*----------------------------------------------------------------------*/


#endif /* __TCUSRDEF */


/* File TIMECONV.C
 *      ==========
 *
 * version 7, 22-May-2006
 *
 * time conversion routines
 * K. Stammler, 15-MAR-1990
 *
 * possible formats:
 *
 * A:  absolute time (seconds since 1970)
 * T:  text format (dd-mmm-yyy_hh:mm:ss.ff, e.g.: 21-JUL-1980_5:20:21.31)
 * N:  numeric (7 integers containing year, month, day, hour, min, sec &
 *                                                                    millisec)
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
#include <ctype.h>
#include <string.h>
#include "basecnst.h"
#include BC_SYSBASE
#include "tcusrdef.h"
#include "utusrdef.h"
#include "tcerrors.h"

#define MAXMONTH 12
#define MINYEAR  1970
								/* smallest year value */
#define SUBYEAR  1969
								/* first leap year minus three */
#define YEARSEC  31536000L
#define DAYSEC   86400L
#define HOURSEC  3600
#define MINSEC   60
#define YEARDAYS 365

							  /* year is leap year, valid for 1900 < y < 2100 */
#define isleap(y) (((y)%4) == 0)
							  /* total days since MINYEAR */
#define daytotal(y) (((y)-MINYEAR)*YEARDAYS + ((y)-SUBYEAR)/4)

static char      mnname_tcv[MAXMONTH+1][4] = { "???",
						  "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
						  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
					  };                              /* month names */

static int      mnlth_tcv[MAXMONTH] = {31,28,31,30,31,30,31,31,30,31,30,31};
																 /* length of months */

static NTIME    dftime_tcv={1970,7,21,0,0,0,0};  /* default time */


/* prototypes of local routines */
static char tc_1sep( char str[] );


/*------------------------------------------------------------------------*/



void tc_t2n( char text[], NTIME *ntime, STATUS *status )

/* converts from text format to numeric format
 *
 * parameters of routine
 * char     text[];        input; text time format
 * NTIME    *ntime;        output; numeric time
 * STATUS   *status;       output; return status
 */
{
	/* local variables */
	char     sep1;     /* first separator */
	int      num[7];   /* integer list */
	int      ll;       /* list length */
	int      flip;     /* exchange year <-> day */
	int      setdef;   /* set default time */
	int      dmy;      /* scratch */

	/* executable code */

	setdef = (*text == '*');
	if  (setdef)  text++;

	*ntime = dftime_tcv;
	sep1 = tc_1sep( text );
	if  ((sep1 == '-') || (sep1 == ':'))  {
		tc_std2n( text, ntime, status );
		if  (*status != TCE_NOERROR)  return;
		if  (setdef)  {
			dftime_tcv.year = ntime->year;
			dftime_tcv.month = ntime->month;
			dftime_tcv.day = ntime->day;
		} /*endif*/
		return;
	} else if  (sep1 == '/')  {
		ut_nlist( text, "/,:. _", 7, num, &ll, status );
		if  (*status != TCE_NOERROR)  return;
		flip = FALSE;
	} else if  (sep1 == '.')  {
		ut_nlist( text, ".,_:", 7, num, &ll, status );
		if  (ll == 2)  {
			tc_std2n( text, ntime, status );
			return;
		} /*endif*/
		if  (*status != TCE_NOERROR)  return;
		flip = TRUE;
	} else if  (sep1 == ',')  {
		ut_nlist( text, ",", 7, num, &ll, status );
		if  (*status != TCE_NOERROR)  return;
		flip = TRUE;
	} else if  (sep1 == '\0')  {
		if  (*text != '\0')  *status = TCE_NOSEP;
		return;
	} else {
		*status = TCE_ILTERM;
		return;
	} /*endif*/

	if  ((ll < 1) || (ll > 7))  {
		*status = TCE_INT7;
		return;
	} /*endif*/
	if  (flip)  {
		dmy = num[0];
		num[0] = num[2];
		num[2] = dmy;
	} /*endif*/
	if  (ll >= 1) ntime->year = num[0];
	if  (ll >= 2) ntime->month = num[1];
	if  (ll >= 3) ntime->day = num[2];
	if  (ll >= 4) ntime->hour = num[3];
	if  (ll >= 5) ntime->min = num[4];
	if  (ll >= 6) ntime->sec = num[5];
	if  (ll >= 7) ntime->ms = num[6];

	if  (ntime->year < 1900)  ntime->year += 1900;
	if  (ntime->year < MINYEAR)  ntime->year += 100;

	if  (setdef)  {
		dftime_tcv.year = ntime->year;
		dftime_tcv.month = ntime->month;
		dftime_tcv.day = ntime->day;
	} /*endif*/

} /* end of tc_t2n */



/*------------------------------------------------------------------------*/



void tc_std2n( char text[], NTIME *ntime, STATUS *status )

/* converts from standard text format (e.g. 12-JAN-1990_5:30:20.001)
 * to numeric format
 *
 * parameters of routine
 * char     text[];         input; text time format
 * NTIME    *ntime;         output; numeric time
 * STATUS   *status;        output; return status
 */
{
	/* local constants */
#  define NUMLTH 10
#  define TPINIT  0
#  define TPDAY   1
#  define TPMONTH 2
#  define TPYEAR  3
#  define TPHOUR  4
#  define TPMIN   5
#  define TPSEC   6
#  define TPMS    7

	/* local variables */
	char     numstr[NUMLTH+1];   /* digit string */
	char     *cc;                /* current char copied */
	int      state;              /* current state */
	int      num;                /* scratch */
	int      numl;               /* length of number */
	BOOLEAN  quit;               /* loop exit */

	/* executable code */

	*numstr = '\0';
	cc = numstr;
	state = TPINIT;
	quit = FALSE;

	/* skip blanks */
	while  ((*text == ' ') && (*text != '\0'))  text++;

	while  (!quit)  {
		if  (isalnum(*text))  {
			*cc++ = *text++;
		} else {
			quit = (*text == '\0');
			*cc = '\0';
			if  (state == TPINIT)  {
				if  (*text == '-')  {
					state = TPDAY;
				} else if  (*text == ':')  {
					state = TPHOUR;
				} else if  (*text == '.')  {
					state = TPSEC;
				} else {
					*status = TCE_ILTERM;
					return;
				} /*endif*/
			} /*endif*/
			if  (state == TPMONTH)  {
				ut_cap( numstr );
				num = tc_getmonth( numstr, status );
				if  (*status != TCE_NOERROR)  return;
			} else {
				if  (sscanf( numstr, "%d", &num ) != 1)  {
					*status = TCE_NCONV;
					return;
				} /*endif*/
			} /*endif*/
			switch  (state)  {
				case TPDAY:
					ntime->day = num;
					break;
				case TPMONTH:
					ntime->month = num;
					break;
				case TPYEAR:
					if  (num < 1900)  num += 1900;
					if  (num < MINYEAR)  num += 100;
					ntime->year = num;
					break;
				case TPHOUR:
					ntime->hour = num;
					break;
				case TPMIN:
					ntime->min = num;
					break;
				case TPSEC:
					ntime->sec = num;
					break;
				case TPMS:
					numl = (int)strlen( numstr );
					if  (numl == 1)  num *= 100;
					if  (numl == 2)  num *= 10;
					if  (numl > 3)  {
						*status = TCE_TOOACC;
						return;
					} /*endif*/
					ntime->ms = num;
					break;
				default:
					*status = TCE_ADDCH;
					return;
			} /*endswitch*/
			cc = numstr;
			text++;
			if  (*text == ' ')  text++;
			state++;
		} /*endif*/
	} /*endwhile*/

} /* end of tc_std2n */



/*------------------------------------------------------------------------*/



void tc_n2t( NTIME *ntime, char text[], STATUS *status )

/* converts numeric format to text format
 *
 * parameters of routine
 * NTIME    *ntime;         input; numeric time
 * char     text[];         output; text format
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      yy;       /* year */

	/* executable code */

	if  (ntime->year > 9999)  {
		*status = TCE_TYOOR;
		return;
	} else if  ((ntime->month > 12) || (ntime->month < 1))  {
		*status = TCE_TMOOR;
		return;
	} else if  ((ntime->day > 31) || (ntime->day < 1))  {
		*status = TCE_TDOOR;
		return;
	} else if  (ntime->hour > 23)  {
		*status = TCE_THOOR;
		return;
	} else if  (ntime->min > 59)  {
		*status = TCE_TIOOR;
		return;
	} else if  (ntime->sec > 59)  {
		*status = TCE_TSOOR;
		return;
	} else if  (ntime->ms > 999)  {
		*status = TCE_TCOOR;
		return;
	} /*endif*/

	sprintf( text, "%2d", ntime->day );
	text[2] = '-'; text[3] = '\0';
	strcat( text, mnname_tcv[ntime->month] );
	text[6] = '-';
	yy = ntime->year;
	if  (yy < 1900)  yy += 1900;
	sprintf( text+7, "%04d", yy );
	text[11] = '_';
	sprintf( text+12, "%02d", ntime->hour );
	text[14] = ':';
	sprintf( text+15, "%02d", ntime->min );
	text[17] = ':';
	sprintf( text+18, "%02d", ntime->sec );
	text[20] = '.';
	sprintf( text+21, "%03d", ntime->ms );

} /* end of tc_n2t */



/*------------------------------------------------------------------------*/



void tc_n2a( NTIME *ntime, TIME *time, STATUS *status )

/* converts numeric time to absolute time
 *
 * parameters of routine
 * NTIME    *ntime;         input; numeric time
 * TIME     *time;          output; absolute time
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      julday;   /* julian day */

	/* executable code */

	julday = tc_julian( ntime->year, ntime->month, ntime->day, status );
	if  (*status != TCE_NOERROR)  return;

	time->hi = (long)(ntime->year - MINYEAR) * YEARSEC +
		(((long)(ntime->year - SUBYEAR) / 4) + (long)(julday-1)) * DAYSEC +
		(long)(ntime->hour) * HOURSEC + (long)(ntime->min) * MINSEC +
		(long)(ntime->sec);
	time->lo = ntime->ms;

} /* end of tc_n2a */



/*------------------------------------------------------------------------*/



void tc_a2n( TIME *time, NTIME *ntime, STATUS *status )

/* converts absolute time to numeric time
 *
 * parameters of routine
 * TIME     *time;          input; absolute time
 * NTIME    *ntime;         output; numeric time
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	long     totsec;    /* total number of seconds */

	/* executable code */

	totsec = time->hi;
	ntime->ms = time->lo;

	ntime->sec = (int)(totsec % MINSEC);
	totsec /= MINSEC;
	ntime->min = (int)(totsec % 60);
	totsec /= 60;
	ntime->hour = (int)(totsec % 24);
	totsec /= 24;

	ntime->year = (int)(totsec / YEARDAYS) + MINYEAR;
	ntime->day = daytotal( ntime->year );
	if  (ntime->day > (int)totsec)  {
		(ntime->year)--;
		ntime->day = daytotal( ntime->year );
	} /*endif*/

	tc_dayofmn( ntime->year, (int)totsec-(ntime->day)+1, &(ntime->month),
		&(ntime->day), status );

} /* end of tc_a2n */



/*------------------------------------------------------------------------*/



void tc_t2a( char text[], TIME *time, STATUS *status )

/* converts text format to absolute time
 *
 * parameters of routine
 * char     text[];         input; text time
 * TIME     *time;          output; absolute time
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	NTIME    ntime;    /* numeric time */

	/* executable code */

	tc_t2n( text, &ntime, status );
	if  (*status != TCE_NOERROR)  return;
	tc_n2a( &ntime, time, status );

} /* end of tc_t2a */



/*------------------------------------------------------------------------*/



void tc_a2t( TIME *time, char text[], STATUS *status )

/* converts absolute time to text format
 *
 * parameters of routine
 * TIME     *time;          input; absolute time
 * char     text[];         output; text time
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	NTIME    ntime;    /* numeric time */

	/* executable code */

	tc_a2n( time, &ntime, status );
	if  (*status != TCE_NOERROR)  return;
	tc_n2t( &ntime, text, status );

} /* end of tc_a2t */



/*------------------------------------------------------------------------*/



float tc_adiff( TIME *atmin, TIME *atsub )

/* returns difference "atmin-atsub" (absolute times) in seconds
 *
 * parameters of routine
 * TIME     *atmin;        input; minuend
 * TIME     *atsub;        input; subtrahend
 *                         returns: difference in seconds
 */
{
	/* local variables */
	int      n;        /* scratch (compiler error) */
	long     l;        /* same */

	/* executable code */

	l = atmin->hi - atsub->hi;
	n = atmin->lo - atsub->lo;
	return  ((float)l + (float)n/1000.);

/* return  ((float)(atmin->hi - atsub->hi)
			  + (float)(atmin->lo - atsub->lo)/1000.);
*/
} /* end of tc_adiff */



/*------------------------------------------------------------------------*/



float tc_ndiff( NTIME *ntmin, NTIME *ntsub, STATUS *status )

/* returns difference "ntmin-ntsub" (numeric times) in seconds
 *
 * parameters of routine
 * NTIME    *ntmin;         input; minuend
 * NTIME    *ntsub;         input; subtrahend
 * STATUS   *status;        output; return status
 *                          returns: difference in seconds
 */
{
	/* local variables */
	TIME     atmin, atsub;  /* absolute conversions of parameters */

	/* executable code */

	tc_n2a( ntmin, &atmin, status );
	if  (*status != TCE_NOERROR)  return 0.;
	tc_n2a( ntsub, &atsub, status );
	if  (*status != TCE_NOERROR)  return 0.;
	return tc_adiff( &atmin, &atsub );

} /* end of tc_ndiff */



/*------------------------------------------------------------------------*/



float tc_tdiff( char ttmin[], char ttsub[], STATUS *status )

/* returns difference "ttmin-ttsub" (text format) in seconds
 *
 * parameters of routine
 * char     ttmin[];        input; minuend
 * char     ttsub[];        input; subtrahend
 * STATUS   *status;        output; return status
 *                          returns: difference in seconds
 */
{
	/* local variables */
	TIME     atmin, atsub;  /* absolute conversions of parameters */

	/* executable code */

	tc_t2a( ttmin, &atmin, status );
	if  (*status != TCE_NOERROR)  return 0.;
	tc_t2a( ttsub, &atsub, status );
	if  (*status != TCE_NOERROR)  return 0.;
	return tc_adiff( &atmin, &atsub );

} /* end of tc_tdiff */



/*------------------------------------------------------------------------*/



void tc_aadd( TIME *time, float addtim, TIME *ares )

/* adds "addtim" seconds to "time" (absolute time)
 *
 * parameters of routine
 * TIME     *time;          input; input time
 * float    addtim;         input; seconds to be added
 * TIME     *ares;          output; result time
 */
{
	/* local variables */
	long     l;         /* scratch */

	/* executable code */

	if  (Abs(addtim) > 1.0e5)  {  /* ignore fractions of seconds */
		ares->hi = time->hi + Nlong(addtim);
		ares->lo = time->lo;
		return;
	} /*endif*/

	l = (long)time->lo + Nlong(addtim*1000.);
	if  (l >= 0)  {
		ares->hi = time->hi + l/1000;
		ares->lo = (int)(l % 1000);
	} else {
		ares->hi = time->hi + l/1000 - 1;
		ares->lo = 1000 - (int)((-l) % 1000);
	} /*endif*/
	if  (ares->lo >= 1000)  {
		ares->lo -= 1000;
		(ares->hi)++;
	} /*endif*/

} /* end of tc_aadd */



/*------------------------------------------------------------------------*/



void tc_nadd( NTIME *ntime, float addtim, NTIME *nres, STATUS *status )

/* adds "addtim" seconds to "ntime" (numeric time)
 *
 * parameters of routine
 * NTIME    *ntime;         input; input time
 * float    addtim;         input; seconds to be added
 * NTIME    *nres;          output; result time
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	TIME     atime;  /* scratch */

	/* executable code */

	tc_n2a( ntime, &atime, status );
	if  (*status != TCE_NOERROR)  return;
	tc_aadd( &atime, addtim, &atime );
	tc_a2n( &atime, nres, status );

} /* end of tc_nadd */



/*------------------------------------------------------------------------*/



void tc_tadd( char text[], float addtim, char tres[], STATUS *status )

/* adds "addtim" seconds to "text" (text format)
 *
 * parameters of routine
 * char     text[];         input; input time
 * float    addtim;         input; seconds to be added
 * char     tres[];         output; result time
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	TIME     atime;  /* scratch */

	/* executable code */

	tc_t2a( text, &atime, status );
	if  (*status != TCE_NOERROR)  return;
	tc_aadd( &atime, addtim, &atime );
	tc_a2t( &atime, tres, status );

} /* end of tc_tadd */



/*------------------------------------------------------------------------*/



void tc_setempty( TIME *time )

/* sets time to an empty value
 *
 * parameters of routine
 * TIME       *time;   output; time set to empty value
 */
{

	time->hi = -1L;

} /* end of tc_setempty */



/*------------------------------------------------------------------------*/



int tc_getmonth( char name[], STATUS *status )

/* returns number of month
 *
 * parameters of routine
 * char     name[];         input; name of month
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	for  (i=0;i<=MAXMONTH;i++)
	  if  (strcmp(mnname_tcv[i],name) == 0)  return i;

	*status = TCE_ILNAME;
	return 0;

} /* end of tc_getmonth */



/*------------------------------------------------------------------------*/



int tc_julian( unsigned year, unsigned month, unsigned day,
	STATUS *status )

/* computes julian date (number of day in year)
 *
 * parameters of routine
 * unsigned int  year, month, day;     input; date
 * STATUS        *status;              output; return status
 *                                     returns number of day
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	month--;
	if  (month >= MAXMONTH)  {
		*status = TCE_TMOOR;
		return 0;
	} /*endif*/
	mnlth_tcv[1] = isleap(year) ? 29 : 28;
	for  (i=0;i<month;i++)
		day += mnlth_tcv[i];

	return (day);

} /* end of tc_julian */



/*------------------------------------------------------------------------*/



void tc_dayofmn( unsigned year, unsigned julday, unsigned *month,
	unsigned *day, STATUS *status )

/* computes day of month & month from day of year
 *
 * parameters of routine
 * unsigned int  year;      input; year
 * unsigned int  julday;    input; julian day
 * unsigned int  *month;    output; month
 * unsigned int  *day;      output; day of month
 * STATUS        *status;   output; return status
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	mnlth_tcv[1] = isleap(year) ? 29 : 28;
	for  (i=0;julday>mnlth_tcv[i];i++)  {
		julday -= mnlth_tcv[i];
		if  (i == (MAXMONTH-1))  {
			*status = TCE_TMOOR;
			return;
		} /*endif*/
	} /*endfor*/

	*month = i+1;
	*day = julday;

} /* end of tc_dayofmn */



/*------------------------------------------------------------------------*/



static char tc_1sep( char str[] )

/* returns first separator in str
 *
 * parameters of routine
 * char     str[];       input; search string
 *                       returns: first separator in str
 */
{
	/* executable code */

	while  (*str != '\0')  {
		if  ((*str > ' ') && !isalnum(*str))  return *str;
		str++;
	} /*endwhile*/

	return '\0';

} /* end of tc_1sep */



/*------------------------------------------------------------------------*/

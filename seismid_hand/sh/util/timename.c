
/* file timename.c
 *      ==========
 *
 * version 19, 23-Dec-2006
 *
 * string operations with times and filenames
 * K. Stammler, 4-Mar-95
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



#include <stdio.h>
#include <string.h>
#include BASECNST
#include BC_SYSBASE
#include BC_TCUSRDEF
#include BC_CPAR


#define err_writemsg( s, t, b ) {fprintf(stderr,"err:%d\n",s);return 1;}


/* global variables */
static char   vtn_progname[BC_FILELTH+1];     /* program name */



/* prototypes of local routines */
static void DigitToTimeString( char digit[], char timestr[],
	STATUS *status );
static void NameToStationTimeString( char name[], char str[],
	STATUS *status );



int main( int argc, char *argv[] )
{
	/* local variables */
	char     subfct[BC_LINELTH+1];       /* subfunction */
	char     time1[BC_LINELTH+1];        /* time string 1 */
	char     time2[BC_LINELTH+1];        /* time string 2 */
	NTIME    ntime;                      /* numeric time */
	NTIME    ntime2;                     /* second time */
	TIME     atime;                      /* absolute time */
	float    secs;                       /* number of sec */
	STATUS   status;                     /* return status */
	int      julday;                     /* julian day */
	int      sfdb_date;                  /* sfdb date integer */
	double   sfdb_time;                  /* sfdb time: hhmmss.sss */

	/* executable code */

	status = BC_NOERROR;

	/* get parameters */
	/* strcpy( vtn_progname, argv[0] ); */
	strcpy( vtn_progname, "timename" );

	pa_init( argc, argv );
	if  (pa_pnumber() < 2)  {
		fprintf( stderr, "Usage: %s <subfct> <p1> .. <pN>\n\n", vtn_progname );
		fprintf( stderr, "   time_addsec <abstime> <sec>\n" );
		fprintf( stderr, "      add seconds to absolute time\n" );
		fprintf( stderr, "   time_subsec <abstime> <sec>\n" );
		fprintf( stderr, "      subtract seconds from absolute time\n");
		fprintf( stderr, "   time_subday <abstime> <daynum>\n" );
		fprintf( stderr, "      subtract an integer number of days from a time\n" );
		fprintf( stderr, "      result is a date (no hours:mins:secs)\n" );
		fprintf( stderr, "   time_intdiff <abstime1> <abstime2>\n" );
		fprintf( stderr, "      compute difference between two absolute times\n" );
		fprintf( stderr, "      result is an integer number of seconds\n" );
		fprintf( stderr, "   time_floatdiff <abstime1> <abstime2>\n" );
		fprintf( stderr, "      compute difference between two absolute times\n" );
		fprintf( stderr, "      result is a real valued number of seconds\n" );
		fprintf( stderr, "   time_daydiff <abstime1> <abstime2>\n" );
		fprintf( stderr, "      compute difference between two absolute times\n" );
		fprintf( stderr, "      result is an integer number of days\n" );
		fprintf( stderr, "   time_dayfdiff <abstime1> <abstime2>\n" );
		fprintf( stderr, "      compute difference between two absolute times\n" );
		fprintf( stderr, "      result is a real valued number of days\n" );
		fprintf( stderr, "   time_truncate <abstime>\n" );
		fprintf( stderr, "      truncate absolute time to previous midnight\n" );
		fprintf( stderr, "   time_digit6 <abstime>\n" );
		fprintf( stderr, "      convert absolute time in digit string yymmdd\n" );
		fprintf( stderr, "   time_digit10 <abstime>\n" );
		fprintf( stderr, "      convert absolute time in digit string yymmdd_hhmm\n" );
		fprintf( stderr, "   time_digit12 <abstime>\n" );
		fprintf( stderr, "      convert absolute time in digit string yymmdd_hhmmss\n" );
		fprintf( stderr, "   digit_time <digitstring>\n" );
		fprintf( stderr, "      convert digit string of length 6, 10 or 12 to absolute time\n" );
		fprintf( stderr, "   name_time <filename>\n" );
		fprintf( stderr, "      convert typical filename of type station_yymmdd_hhmm.xxx\n" );
		fprintf( stderr, "      to <station> and absolute time separated by a blank\n" );
		fprintf( stderr, "   julian <year> <month> <day>\n" );
		fprintf( stderr, "      compute julian day number from year, month, day\n" );
		fprintf( stderr, "   day_of_month <year> <julianday>\n" );
		fprintf( stderr, "      compute month and day from year and julian day number\n" );
		fprintf( stderr, "   this_month <abstime>\n" );
		fprintf( stderr, "      returns date of first day in month\n" );
		fprintf( stderr, "   inc_month <abstime>\n" );
		fprintf( stderr, "      increment given date by one month\n" );
		fprintf( stderr, "   month_diff <abstime1> <abstime2>\n" );
		fprintf( stderr, "      compute difference in months between to absolute times\n" );
		fprintf( stderr, "      ignores days, hours, minutes and seconds\n" );
		fprintf( stderr, "   time_to_int <abstime>\n" );
		fprintf( stderr, "      convert absolute time to a list of 7 integers:\n" );
		fprintf( stderr, "      year, month, day, hour, minute, second, millisecond\n" );
		fprintf( stderr, "   time_to_epoch <abstime>\n" );
		fprintf( stderr, "      convert absolute time to number of seconds since 1970\n" );
		fprintf( stderr, "      (no leap seconds)\n" );
		fprintf( stderr, "   epoch_to_time <epoch>\n" );
		fprintf( stderr, "      inverse of above transformation\n" );
		fprintf( stderr, "   time_to_sfdb <abstime>\n" );
		fprintf( stderr, "      convert absolute time sfdb time: <yyyymmdd> <hhmmss.sss>\n" );
#ifdef XXX
		fprintf( stderr, "   time_to_rrdepoch <abstime>\n" );
		fprintf( stderr, "      convert absolute to epoch time (just 7200 secs less\n" );
		fprintf( stderr, "      than time_to_epoch).  This is used in rrd program.\n" );
		fprintf( stderr, "   rrdepoch_to_time <rrdepoch>\n" );
		fprintf( stderr, "      inverse of above transformation\n" );
		fprintf( stderr, "   gsedate <abstime>\n" );
		fprintf( stderr, "      reformat date to date given in GSE files\n" );
		fprintf( stderr, "   time_drm <abstime>\n" );
		fprintf( stderr, "      dont know\n" );
#endif
		return 1;
	} /*endif*/

	strcpy( subfct, pa_pvalue(1) );

	if  (strcmp(subfct,"time_addsec") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		sscanf( pa_pvalue(3), "%f", &secs );
		tc_tadd( time1, secs, time2, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%s\n", time2 );
	} else if  (strcmp(subfct,"time_subsec") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		sscanf( pa_pvalue(3), "%f", &secs );
		if  (strcmp(time1,"TT") == 0)  {
			while  (!feof(stdin))  {
				scanf( "%s\n", time1 );
				tc_tadd( time1, -secs, time2, &status );
				if  (Severe(&status))  err_writemsg( status, "", TRUE );
				printf( "%s\n", time2 );
			} /*endwhile*/
		} else {
			tc_tadd( time1, -secs, time2, &status );
			if  (Severe(&status))  err_writemsg( status, "", TRUE );
			printf( "%s\n", time2 );
		} /*endif*/
	} else if  (strcmp(subfct,"time_subday") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		sscanf( pa_pvalue(3), "%f", &secs );
		tc_tadd( time1, -secs*86400.0, time2, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		if  (time2[11] == '_')  time2[11] = '\0';
		if  (time2[10] == '_')  time2[10] = '\0';
		printf( "%s\n", time2 );
	} else if  (strcmp(subfct,"time_intdiff") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		strcpy( time2, pa_pvalue(3) );
		if  (strcmp(time1,"TT") == 0)  {
			while  (!feof(stdin))  {
				scanf( "%s\n", time1 );
				secs = tc_tdiff( time1, time2, &status );
				if  (Severe(&status))  err_writemsg( status, "", TRUE );
				printf( "%d\n", Nint(secs) );
			} /*endwhile*/
		} else {
			secs = tc_tdiff( time1, time2, &status );
			if  (Severe(&status))  err_writemsg( status, "", TRUE );
			printf( "%d\n", Nint(secs) );
		} /*endif*/
	} else if  (strcmp(subfct,"time_floatdiff") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		strcpy( time2, pa_pvalue(3) );
		if  (strcmp(time1,"TT") == 0)  {
			while  (!feof(stdin))  {
				scanf( "%s\n", time1 );
				secs = tc_tdiff( time1, time2, &status );
				if  (Severe(&status))  err_writemsg( status, "", TRUE );
				printf( "%e\n", secs );
			} /*endwhile*/
		} else {
			secs = tc_tdiff( time1, time2, &status );
			if  (Severe(&status))  err_writemsg( status, "", TRUE );
			printf( "%e\n", secs );
		} /*endif*/
	} else if  (strcmp(subfct,"time_daydiff") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		strcpy( time2, pa_pvalue(3) );
		secs = tc_tdiff( time1, time2, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%d\n", Nint(secs/86400.0) );
	} else if  (strcmp(subfct,"time_dayfdiff") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		strcpy( time2, pa_pvalue(3) );
		secs = tc_tdiff( time1, time2, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%f\n", secs/86400.0 );
	} else if  (strcmp(subfct,"time_truncate") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		tc_t2n( time1, &ntime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		ntime.hour = ntime.min = ntime.sec = ntime.ms = 0;
		tc_n2t( &ntime, time1, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%s\n", time1 );
	} else if  (strcmp(subfct,"time_digit6") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		tc_t2n( time1, &ntime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		if  (ntime.year > 1900)  ntime.year -= 1900;
		if  (ntime.year >= 100)  ntime.year -= 100;
		printf( "%02d%02d%02d\n", ntime.year, ntime.month, ntime.day );
	} else if  (strcmp(subfct,"time_digit10") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		tc_t2n( time1, &ntime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		if  (ntime.year > 1900)  ntime.year -= 1900;
		if  (ntime.year >= 100)  ntime.year -= 100;
		printf( "%02d%02d%02d_%02d%02d\n", ntime.year, ntime.month,
			ntime.day, ntime.hour, ntime.min );
	} else if  (strcmp(subfct,"time_digit12") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		tc_t2n( time1, &ntime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		if  (ntime.year > 1900)  ntime.year -= 1900;
		if  (ntime.year >= 100)  ntime.year -= 100;
		printf( "%02d%02d%02d_%02d%02d%02d\n", ntime.year, ntime.month,
			ntime.day, ntime.hour, ntime.min, ntime.sec );
	} else if  (strcmp(subfct,"digit_time") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		DigitToTimeString( time1, time2, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%s\n", time2 );
	} else if  (strcmp(subfct,"name_time") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		NameToStationTimeString( time1, time2, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%s\n", time2 );
	} else if  (strcmp(subfct,"julian") == 0)  {
		if  (pa_pnumber() != 4)  {
			fprintf( stderr, "Usage: timename julian <year> <month> <day>\n" );
			return 1;
		} /*endif*/
		sscanf( pa_pvalue(2), "%d", &ntime.year );
		sscanf( pa_pvalue(3), "%d", &ntime.month );
		sscanf( pa_pvalue(4), "%d", &ntime.day );
		julday = tc_julian( ntime.year, ntime.month, ntime.day, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%03d\n", julday );
	} else if  (strcmp(subfct,"day_of_month") == 0)  {
		sscanf( pa_pvalue(2), "%d", &ntime.year );
		sscanf( pa_pvalue(3), "%d", &julday );
		tc_dayofmn( ntime.year, julday, &ntime.month, &ntime.day, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%02d %02d\n", ntime.month, ntime.day );
	} else if  (strcmp(subfct,"this_month") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		tc_t2n( time1, &ntime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		ntime.day = 1;
		ntime.hour = ntime.min = ntime.sec = ntime.ms = 0;
		tc_n2t( &ntime, time2, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%s\n", time2 );
	} else if  (strcmp(subfct,"inc_month") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		tc_t2n( time1, &ntime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		if  (++(ntime.month) > 12)  {
			ntime.month = 1;
			ntime.year++;
		} /*endif*/
		tc_n2t( &ntime, time2, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%s\n", time2 );
	} else if  (strcmp(subfct,"month_diff") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		strcpy( time2, pa_pvalue(3) );
		tc_t2n( time1, &ntime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		tc_t2n( time2, &ntime2, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%d\n", (ntime.year-ntime2.year)*12 + ntime.month-ntime2.month );
	} else if  (strcmp(subfct,"time_to_int") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		tc_t2n( time1, &ntime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%04d %02d %02d %02d %02d %02d %03d\n",
			ntime.year, ntime.month, ntime.day,
			ntime.hour, ntime.min, ntime.sec, ntime.ms );
	} else if  (strcmp(subfct,"time_drm") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		tc_t2n( time1, &ntime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		if  (ntime.year > 1950)  ntime.year -= 1900;
		if  (ntime.year >= 100)  ntime.year -= 100;
		printf( "%02d/%02d/%02d-%02d:%02d:%02d\n",
			ntime.month, ntime.day, ntime.year,
			ntime.hour, ntime.min, ntime.sec );
	} else if  (strcmp(subfct,"gsedate") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		tc_t2n( time1, &ntime, &status );
		printf( "%4d/%02d/%02d\n", ntime.year, ntime.month, ntime.day );
	} else if  (strcmp(subfct,"time_to_epoch") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		tc_t2a( time1, &atime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%d\n", atime.hi );
	} else if  (strcmp(subfct,"epoch_to_time") == 0)  {
		sscanf( pa_pvalue(2), "%d", &atime.hi );
		atime.lo = 0;
		tc_a2t( &atime, time1, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%s\n", time1 );
	} else if  (strcmp(subfct,"time_to_rrdepoch") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		tc_t2a( time1, &atime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%d\n", atime.hi-7200 );
	} else if  (strcmp(subfct,"rrdepoch_to_time") == 0)  {
		sscanf( pa_pvalue(2), "%d", &atime.hi );
		atime.hi += 7200;
		atime.lo = 0;
		tc_a2t( &atime, time1, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%s\n", time1 );
	} else if  (strcmp(subfct,"time_to_sfdb") == 0)  {
		strcpy( time1, pa_pvalue(2) );
		tc_t2n( time1, &ntime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		sfdb_date = ntime.year*10000 + ntime.month*100 + ntime.day;
		sfdb_time = (double)ntime.hour*10000.0 + (double)ntime.min*100.0
			+ (double)ntime.sec + (double)ntime.ms/1000.0;
		printf( "%8d %10.3lf\n", sfdb_date, sfdb_time );
	} else if  (strcmp(subfct,"sfdb_to_time") == 0)  {
		sscanf( pa_pvalue(2), "%d", &sfdb_date );
		sscanf( pa_pvalue(3), "%lf", &sfdb_time );
		ntime.year = sfdb_date / 10000;
		ntime.month = (sfdb_date % 10000) / 100;
		ntime.day = sfdb_date % 100;
		ntime.hour = Nint( sfdb_time / 10000.0 - 0.499 );
		sfdb_time -= (double)ntime.hour*10000.0;
		ntime.min = Nint( sfdb_time / 100.0 - 0.499 );
		sfdb_time -= (double)ntime.min*100.0;
		ntime.sec = Nint( sfdb_time - 0.499 );
		sfdb_time -= (double)ntime.sec;
		ntime.ms = Nint( sfdb_time*1000.0 );
		tc_n2t( &ntime, time1, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		printf( "%s\n", time1 );
	} else {
		fprintf( stderr, "%s: unknown subfunction %s\n",
			vtn_progname, subfct );
		return 1;
	} /*endif*/

	return 0;

} /* end of main */



/*------------------------------------------------------------------*/



static void DigitToTimeString( char digit[], char timestr[],
	STATUS *status )

/* converts digit string to time string
 *
 * parameters of routine
 * char       digit[];       input; digit string
 * char       timestr[];     output; time string
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];    /* scratch string */
	NTIME    ntime;                /* numeric time */
	char     *cptr;                /* moving pointer */

	/* executable code */

	*timestr = '\0';
	str[2] = '\0';
	ntime.hour = ntime.min = ntime.sec = ntime.ms = 0;
	cptr = digit;
	while  (*cptr == '_' || *cptr == ' ')  cptr++;
	if  (*cptr == '\0')  return;
	str[0] = *cptr++;  str[1] = *cptr++;
	if  (sscanf(str,"%d",&ntime.year) != 1)  {
		fprintf( stderr, "%s: error converting %s\n", vtn_progname, digit);
		return;
	} /*endif*/
	if  (ntime.year < 70)  {
		ntime.year += 2000;
	} else  {
		ntime.year += 1900;
	} /*endif*/
	while  (*cptr == '_' || *cptr == ' ')  cptr++;
	str[0] = *cptr++;  str[1] = *cptr++;
	if  (sscanf(str,"%d",&ntime.month) != 1)  {
		fprintf( stderr, "%s: error converting %s\n", vtn_progname, digit);
		return;
	} /*endif*/
	while  (*cptr == '_' || *cptr == ' ')  cptr++;
	str[0] = *cptr++;  str[1] = *cptr++;
	if  (sscanf(str,"%d",&ntime.day) != 1)  {
		fprintf( stderr, "%s: error converting %s\n", vtn_progname, digit);
		return;
	} /*endif*/
	if  (*cptr != '\0' && *cptr != '.')  {
		while  (*cptr == '_' || *cptr == ' ')  cptr++;
		str[0] = *cptr++;  str[1] = *cptr++;
		if  (sscanf(str,"%d",&ntime.hour) != 1)  {
			fprintf( stderr, "%s: error converting %s\n", vtn_progname, digit);
			return;
		} /*endif*/
	} /*endif*/
	if  (*cptr != '\0' && *cptr != '.')  {
		while  (*cptr == '_' || *cptr == ' ')  cptr++;
		str[0] = *cptr++;  str[1] = *cptr++;
		if  (sscanf(str,"%d",&ntime.min) != 1)  {
			fprintf( stderr, "%s: error converting %s\n", vtn_progname, digit);
			return;
		} /*endif*/
	} /*endif*/
	if  (*cptr != '\0' && *cptr != '.')  {
		while  (*cptr == '_' || *cptr == ' ')  cptr++;
		str[0] = *cptr++;  str[1] = *cptr++;
		if  (sscanf(str,"%d",&ntime.sec) != 1)  {
			fprintf( stderr, "%s: error converting %s\n", vtn_progname, digit);
			return;
		} /*endif*/
	} /*endif*/

	tc_n2t( &ntime, timestr, status );
	return;

} /* end of DigitToTimeString */



/*------------------------------------------------------------------*/



static void NameToStationTimeString( char name[], char str[],
	STATUS *status )

/* converts filename to station and time
 *
 * parameters of routine
 * char       name[];        input; filename
 * char       str[];         output; station and time (sep. by blank)
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	char     *cptr;       /* char pointer */
	int      lth;         /* string length */

	/* executable code */

	cptr = strchr( name, '_' );
	if  (cptr == NULL)  {
		fprintf( stderr, "%s: no separation '_' in %s\n",
			vtn_progname, name );
		return;
	} /*endif*/

	lth = (int)(cptr - name);
	strncpy( str, name, lth );
	str[lth] = ' ';
	str[lth+1] = '\0';
	DigitToTimeString( ++cptr, str+lth+1, status );

} /* end of NameToStationTimeString */



/*------------------------------------------------------------------*/


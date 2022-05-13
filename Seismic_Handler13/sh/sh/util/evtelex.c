
/* file evtelex.c
 *      =========
 *
 * version 17, 16-Jan-2007
 *
 * TELEX format of seismo messages
 * K. Stammler, 28-Jul-93
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
#include "basecnst.h"
#include "sysbase.h"
#include "tcusrdef.h"
#include "utusrdef.h"
#include "eventdsc.h"
#include "evtelex.h"



#define LINE_INDENT 6
	/* size of indentation in characters */
#define LINE_LENGTH 60
	/* maximum length of telex lines */
#define SLOW_LIMIT 100.0
	/* upper limit for slowness */



/* global variables */
static char efv_seismo_header[BC_LINELTH+1]="";
	/* header of seismo report */
static char efv_seismo_station[BC_LINELTH+1]="";
	/* station name in seismo report */
/* comment values */
static float efv_cmt_mb=EvEMPTY_MAGNITUDE;       /* mb */
static float efv_cmt_ms=EvEMPTY_MAGNITUDE;       /* MS */
static float efv_cmt_ml=EvEMPTY_MAGNITUDE;       /* ml */
static float efv_cmt_distdeg=EvEMPTY_DISTANCE;   /* distance in deg */
static float efv_cmt_distkm=EvEMPTY_DISTANCE;    /* distance in km */
static BOOLEAN efv_cmt=FALSE;                    /* any comment ? */
static BOOLEAN efv_ignore_tflag=FALSE;       /* Ignore setting of telex flag */



/* prototypes of local routines */
static void EfhPutTelexString( FILE *telex, char str[],
	BOOLEAN newdate );
static void EfhPutComment( FILE *telex );



/*-------------------------------------------------------------------*/



void EfPutTelex( FILE *telex, EvEventT *event, EvStatusT *status )

/* Prints event "event" in telex format to file "telex".  This
 * routine may be used as a "put_routine" in "EvReformatEvenfile".
 *
 * parameters of routine
 * FILE       *telex;        input; output file
 * EvEventT   *event;        input; event information
 * EvStatusT  *status;       output; return status
 */
{
	/* local variables */
	static BOOLEAN first_call=TRUE;         /* first call to routine */
	static char curr_date[BC_TIMELTH+1]=""; /* current date string */
	static long event_id=0;                 /* current event ID */
	static BOOLEAN lp_done=FALSE;           /* LP info done for this event */
	static char ref_station[BC_LINELTH+1];  /* reference (first) station */
	char     date[BC_TIMELTH+1];       /* event date */
	char     time[BC_TIMELTH+1];       /* event time */
	char     phase[BC_SHORTSTRLTH+1];  /* phase name */
	NTIME    ntime;                    /* numeric time */
	char     str[BC_LINELTH+1];        /* scratch string */
	char     fmt[BC_SHORTSTRLTH+1];    /* format string */
	float    local_slowness;           /* local slowness */
	float    local_azimuth;            /* local azimuth */
	BOOLEAN  add_info;                 /* print out additional info */
	BOOLEAN  secondary_phase;          /* secondary phase */
	BOOLEAN  is_longperiod;            /* is longperiod information */
	BOOLEAN  print_sloaz;              /* print slowness & azimuth */

	/* executable code */

	/* print trailer if event pointer is NULL */
	if  (event == NULL)  {
		/* fprintf( telex, "\nstop+++\n" ); */
		if  (efv_cmt)  EfhPutComment( telex );
		fprintf( telex, "\n" );
		first_call = TRUE;
		return;
	} /*endif*/

	/* check for Telex flag */
	if  (strchr(event->phase_flags,'T') == NULL && !efv_ignore_tflag)  return;

	/* print header if first call */
	if  (first_call)  {
		/* should include header file here */
		if  (*efv_seismo_header != '\0')
			fprintf( telex, "%s\n\n", efv_seismo_header );
		if  (strcmp(efv_seismo_station,"gra1") == 0)  {
			fprintf( telex, "seismo grf\n" );
		} else {
			fprintf( telex, "\n%s\n", efv_seismo_station );
		} /*endif*/
		first_call = FALSE;
	} /*endif*/

	/* check event for valid entries */
	if  (event->onset_time[0] == EvEOS)  {
		if   (event->comment.length == 0)
			printf( "--> event with no onset time ignored\n" );
		return;
	} /*endif*/

	/* check if secondary_phase and check for LP entries */
	secondary_phase = (event->evid == event_id);
	if  (!secondary_phase && efv_cmt)  EfhPutComment( telex );
	event_id = event->evid;
	is_longperiod = (strcmp(event->phase,"L") == 0
		|| strcmp(event->phase,"(L)") == 0);
	if  (secondary_phase && is_longperiod && lp_done)  {
		printf( "--> additional LP information ignored (station %s)\n",
			event->station );
		return;
	} /*endif*/
	if  (!secondary_phase)  lp_done = FALSE;
	if  (!lp_done)  lp_done = is_longperiod;

	if  (event->station[0] != EvEOS)  {
		if  (secondary_phase)  {
			if  (strcmp(event->station,ref_station) != 0 && !is_longperiod)  {
				printf( "\n" );
				printf( "============================\n" );
				printf( "||  station %s ignored  ||\n", event->station );
				printf( "============================\n" );
				printf( "\n" );
				return;
			} /*endif*/
		} else {
			strcpy( ref_station, event->station );
		} /*endif*/
	} /*endif*/

	/* create date and time strings */
	if  (event->onset_time[1] == '-')  {  /* 1-digit day */
		strncpy( date, (event->onset_time)+2, 3 );
		date[3] = '0';
		date[4] = event->onset_time[0];
	} else if  (event->onset_time[2] == '-')  {  /* 2-digit day */
		strncpy( date, (event->onset_time)+3, 3 );
		date[3] = event->onset_time[0];
		date[4] = event->onset_time[1];
	} else {  /* illegal day */
		printf( "*** illegal onset time %s found ***\n", event->onset_time );
		return;
	} /*endif*/
	date[5] = EvEOS;
	tc_t2n( event->onset_time, &ntime, status );
	if  (Severe(status))  return;
	if  (ntime.ms >= 950)  {
		ntime.ms = 0;
		tc_nadd( &ntime, 1.0, &ntime, status );
		if  (Severe(status))  return;
	} /*endif*/
	if  (secondary_phase)  {
		sprintf( time, "%02d", ntime.min );
		sprintf( time+2, "%02d", ntime.sec );
		if  (event->onset_acc == EvcOnsetAccSecond)  {
			time[4] = EvEOS;
		} else {
			time[4] = '0' + Nint( (float)(ntime.ms)/100.0 );
			time[5] = EvEOS;
		} /*endif*/
	} else {
		sprintf( time, "%02d", ntime.hour );
		sprintf( time+2, "%02d", ntime.min );
		sprintf( time+4, "%02d", ntime.sec );
		if  (event->onset_acc == EvcOnsetAccSecond)  {
			time[6] = EvEOS;
		} else {
			time[6] = '0' + Nint( (float)(ntime.ms)/100.0 );
			time[7] = EvEOS;
		} /*endif*/
	} /*endif*/

	/* check date */
	if  (strcmp(date,curr_date) != 0 && !secondary_phase)  {
		/* new day, new line */
		strcpy( curr_date, date );
		ut_uncap( date );
		EfhPutTelexString( telex, date, TRUE );
	} /*endif*/

	/* phase identifier */
	if  (event->phase[0] == EvEOS || event->phase[0] == 'X')  {
		*phase = EvEOS;
	} else if  (strcmp(event->phase,"PKP2") == 0)  {
		*phase = EvEOS;
	} else if  (strncmp(event->phase,"PKP",3) == 0)  {
		strcpy( phase, "PKP" );
	} else if  (strncmp(event->phase,"SKS",3) == 0)  {
		strcpy( phase, "SKS" );
	} else if  (strncmp(event->phase,"SKKS",4) == 0)  {
		strcpy( phase, "SKKS" );
	} else {
		strcpy( phase, event->phase );
	} /*endif*/
	if  (strcmp(phase,"Pdiff") == 0)  strcpy( phase, "Pdif" );
	EfTelexPhasename( phase );
	if  (strlen(phase) < 5 && event->onset_type != EvcOnsetUndefined)  {
		switch  (event->onset_type) {
		case EvcOnsetEmergent:  *str = 'e';  break;
		case EvcOnsetImpulsive:  *str = 'i'; break;
		default:
			printf( "*** illegal onset type found ***\n" );
			EvPutEvent( stdout, event, status );
			*str = EvEOS;
			break;
		} /*endswitch*/
		str[1] = EvEOS;
		strcat( str, phase );
	} else {
		strcpy( str, phase );
	} /*endif*/
	if  (strlen(str) < 5 && event->sign != EvcSignUndefined)  {
		switch  (event->sign)  {
		case EvcSignPositive: strcat( str, "c" ); break;
		case EvcSignNegative: strcat( str, "d" ); break;
		default:
			printf( "*** illegal sign found ***\n" );
			EvPutEvent( stdout, event, status );
			break;
		} /*endswitch*/
	} /*endif*/
	strcat( str, time );
	if  (is_longperiod)  {
		/* throw away everything from before and set new */
		*str = 'l';
		str[1] = event->component;
		str[2] = EvEOS;
	} /*endif*/
	ut_uncap( str );
	EfhPutTelexString( telex, str, FALSE );

	/* period */
	if  (event->period != EvEMPTY_PERIOD)  {
		sprintf( fmt, "t%s", EfFORMAT_PERIOD );
		sprintf( str, fmt, event->period );
		EfhPutTelexString( telex, str, FALSE );
	} /*endif*/

	/* amplitude */
	if  (event->amplitude != EvEMPTY_AMPLITUDE)  {
		if  (is_longperiod)  {
			sprintf( fmt, "a%s", EfFORMAT_AMPLITUDE_LP );
			sprintf( str, fmt, event->amplitude/1000.0 );
		} else {
			sprintf( fmt, "a%s", EfFORMAT_AMPLITUDE_SP );
			sprintf( str, fmt, event->amplitude );
		} /*endif*/
		EfhPutTelexString( telex, str, FALSE );
	} /*endif*/

	/* surface waves / long period (this is old version) */
	if  (event->lp_component != EvEMPTY_COMPONENT)  {
		str[0] = 'l';
		str[1] = event->lp_component;
		str[2] = EvEOS;
		ut_uncap( str );
		EfhPutTelexString( telex, str, FALSE );
		if  (event->lp_period != EvEMPTY_PERIOD)  {
			sprintf( fmt, "t%s", EfFORMAT_PERIOD );
			sprintf( str, fmt, event->lp_period );
			ut_uncap( str );
			EfhPutTelexString( telex, str, FALSE );
		} /*endif*/
		if  (event->lp_amplitude != EvEMPTY_AMPLITUDE)  {
			sprintf( fmt, "a%s", EfFORMAT_AMPLITUDE_LP );
			sprintf( str, fmt, event->lp_amplitude );
			ut_uncap( str );
			EfhPutTelexString( telex, str, FALSE );
		} /*endif*/
	} /*endif*/

	/* get azimuth & slowness */
	if  (event->l_slowness != EvEMPTY_SLOWNESS)  {
		local_slowness = event->l_slowness;
	} else if  (event->b_slowness != EvEMPTY_SLOWNESS)  {
		local_slowness = 0.0; /*event->b_slowness;*/
	} else {
		local_slowness = 0.0;
	} /*endif*/
	if  (event->l_azimuth != EvEMPTY_AZIMUTH)  {
		local_azimuth = event->l_azimuth;
	} else if  (event->b_azimuth != EvEMPTY_AZIMUTH)  {
		local_azimuth = 0.0; /*event->b_azimuth;*/
	} else {
		local_azimuth = 0.0;
	} /*endif*/

	/* check if azimuth & slowness should be printed */
#ifdef XXX
	print_sloaz = (strstr(event->phase,"PKP") == NULL
		&& local_slowness > 0.0
		&& local_slowness < SLOW_LIMIT);
#endif
	print_sloaz = (local_slowness > 0.0 && local_slowness < SLOW_LIMIT);

	if  (print_sloaz)  {
		sprintf( fmt, "slo%s", EfFORMAT_SLOWNESS );
		sprintf( str, fmt, local_slowness );
		EfhPutTelexString( telex, str, FALSE );
		sprintf( fmt, "az%s", EfFORMAT_AZIMUTH );
		sprintf( str, fmt, local_azimuth );
		EfhPutTelexString( telex, str, FALSE );
	} /*endif*/

	/* magnitude mb */
	if  (event->mag[EvcMagMb] != EvEMPTY_MAGNITUDE)  {
		efv_cmt_mb = event->mag[EvcMagMb];
		efv_cmt = TRUE;
	} /*endif*/

	/* magnitude ms */
	if  (event->mag[EvcMagMs] != EvEMPTY_MAGNITUDE)  {
		efv_cmt_ms = event->mag[EvcMagMs];
		efv_cmt = TRUE;
	} /*endif*/

	/* magnitude ml */
	if  (event->mag[EvcMagMl] != EvEMPTY_MAGNITUDE)  {
		efv_cmt_ml = event->mag[EvcMagMl];
		efv_cmt = TRUE;
	} /*endif*/

	/* distance (deg) */
	if  (event->distance_deg != EvEMPTY_DISTANCE)  {
		efv_cmt_distdeg = event->distance_deg;
		efv_cmt = TRUE;
	} /*endif*/

	/* distance (km) */
	if  (event->distance_km != EvEMPTY_DISTANCE)  {
		efv_cmt_distkm = event->distance_km;
		efv_cmt = TRUE;
	} /*endif*/

} /* end of EfPutTelex */



/*-------------------------------------------------------------------*/



void EfSetTelexHeader( char header[] )

/* Sets header text of seismo message
 *
 * parameters of routine
 * char       header[];      input; header text
 */
{
	/* executable code */

	if  (strlen(header) > BC_LINELTH)  {
		printf( "*** telex header text too long, truncated ***\n" );
		strncpy( efv_seismo_header, header, BC_LINELTH );
	} else {
		strcpy( efv_seismo_header, header );
	} /*endif*/
	ut_uncap( efv_seismo_header );

} /* end of EfSetTelexHeader */



/*-------------------------------------------------------------------*/



void EfSetTelexStation( char station[] )

/* Sets station name in seismo message
 *
 * parameters of routine
 * char       station[];      input; header text
 */
{
	/* executable code */

	if  (strlen(station) > BC_LINELTH)  {
		printf( "*** station name too long, truncated ***\n" );
		strncpy( efv_seismo_station, station, BC_LINELTH );
	} else {
		strcpy( efv_seismo_station, station );
	} /*endif*/
	ut_uncap( efv_seismo_station );

} /* end of EfSetTelexStation */



/*-------------------------------------------------------------------*/



void EfTelexPhasename( char name[] )

/* replaces lowercase "s" and "p" (in depth phases) to "X" and "A",
 * respectively
 *
 * parameters of routine
 * char       name[];      modify; name to be translated
 */
{
	/* executable code */

	while  (*name != EvEOS)  {
		if  (*name == 'p')  *name = 'A';
		if  (*name == 's')  *name = 'X';
		name++;
	} /*endwhile*/

} /* end of EfTelexPhasename */



/*-------------------------------------------------------------------*/



void EfSetIgnoreTelexFlag( BOOLEAN flag )

/* Sets value of ignore telex flag
 *
 * parameters of routine
 * BOOLEAN    flag;        input; new value of flag
 */
{
	/* executable code */

	efv_ignore_tflag = flag;

} /* end of EfSetIgnoreTelexFlag */



/*-------------------------------------------------------------------*/



static void EfhPutTelexString( FILE *telex, char str[],
	BOOLEAN newdate )

/* Prints string on file
 *
 * parameters of routine
 * FILE       *telex;      input; output file
 * char       *str[];      input; output string
 * BOOLEAN    newdate;     input; new date string
 */
{
	/* local variables */
	static unsigned currpos=0;   /* current line position */
	static BOOLEAN  putblank;    /* insert blank */
	unsigned slen;               /* string length */
	unsigned i;                  /* counter */

	/* executable code */

	slen = (unsigned)strlen( str );
	if  (newdate)  {
		if  (currpos > 0)  fprintf( telex, "\n" );
		fprintf( telex, "%s ", str );
		currpos = slen+1;
		while  (currpos < LINE_INDENT)  {
			currpos++;
			fprintf( telex, " " );
		} /*endwhile*/
		putblank = FALSE;
		return;
	} /*endif*/

	if  (currpos+slen+1 >= LINE_LENGTH)  {
		fprintf( telex, "\n" );
		for  (i=0; i<LINE_INDENT; i++)
			fprintf( telex, " " );
		currpos = LINE_INDENT;
		fprintf( telex, "%s", str );
		currpos += slen;
	} else if  (putblank)  {
		fprintf( telex, " %s", str );
		currpos += slen+1;
	} else {
		fprintf( telex, "%s", str );
		currpos += slen;
	} /*endif*/
	putblank = TRUE;

} /* end of EfhPutTelexString */



/*-------------------------------------------------------------------*/



static void EfhPutComment( FILE *telex )

/* writes comment at the end of event.  Reads and clears the global
 * comment variables efv_cmt_...
 *
 * parameters of routine
 * FILE       *telex;      input; pointer to output file
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];        /* scratch string */
	char     fmt[BC_SHORTSTRLTH+1];    /* format string */
	BOOLEAN  km_done;                  /* km distance available */

	/* executable code */

	km_done = FALSE;

	EfhPutTelexString( telex, "((", FALSE );

	/* magnitude mb */
	if  (efv_cmt_mb != EvEMPTY_MAGNITUDE)  {
		sprintf( fmt, "mb%s", EfFORMAT_MAGNITUDE );
		sprintf( str, fmt, efv_cmt_mb );
		EfhPutTelexString( telex, str, FALSE );
	} /*endif*/

	/* magnitude ms */
	if  (efv_cmt_ms != EvEMPTY_MAGNITUDE)  {
		sprintf( fmt, "ms%s", EfFORMAT_MAGNITUDE );
		sprintf( str, fmt, efv_cmt_ms );
		EfhPutTelexString( telex, str, FALSE );
	} /*endif*/

	/* magnitude ml */
	if  (efv_cmt_ml != EvEMPTY_MAGNITUDE)  {
		sprintf( fmt, "ml%s", EfFORMAT_MAGNITUDE );
		sprintf( str, fmt, efv_cmt_ml );
		EfhPutTelexString( telex, str, FALSE );
	} /*endif*/

	/* distance (km) */
	if  (efv_cmt_distkm != EvEMPTY_DISTANCE)  {
		sprintf( fmt, "d%skm", EfFORMAT_DISTANCE );
		sprintf( str, fmt, efv_cmt_distkm );
		EfhPutTelexString( telex, str, FALSE );
		km_done = TRUE;
	} /*endif*/

	/* distance (deg) */
	if  (efv_cmt_distdeg != EvEMPTY_DISTANCE && !km_done)  {
		sprintf( fmt, "d%s", EfFORMAT_DISTANCE );
		sprintf( str, fmt, efv_cmt_distdeg );
		EfhPutTelexString( telex, str, FALSE );
	} /*endif*/

	EfhPutTelexString( telex, "))", FALSE );

	/* clear info */
	efv_cmt_mb = EvEMPTY_MAGNITUDE;
	efv_cmt_ms = EvEMPTY_MAGNITUDE;
	efv_cmt_ml = EvEMPTY_MAGNITUDE;
	efv_cmt_distdeg = EvEMPTY_DISTANCE;
	efv_cmt_distkm = EvEMPTY_DISTANCE;
	efv_cmt = FALSE;

} /* end of EfhPutComment */



/*-------------------------------------------------------------------*/

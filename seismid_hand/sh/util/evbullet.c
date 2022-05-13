
/* file evbullet.c
 *      ==========
 *
 * version 4, 16-Jan-2007
 *
 * Conversion of event-file to bulletin input file
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
#include BASECNST
#include BC_SYSBASE
#include BC_TCUSRDEF
#include BC_UTUSRDEF
#include "eventdsc.h"
#include "evtelex.h"
#include "evbullet.h"



/* global variables */
static char efv_bull_header[BC_LINELTH+1]=""; /* bulletin header line */



/*-------------------------------------------------------------------*/



void EfPutBulletin( FILE *bull, EvEventT *event, EvStatusT *status )

/* Puts event to bulletin file
 *
 * parameters of routine
 * FILE       *bull;         input; pointer to output file
 * EvEventT   *event;        input; event information
 * EvStatusT  *status;       output; return status
 */
{
	/* local variables */
	static BOOLEAN first_call=TRUE;    /* first call to routine */
	STATUS   locstat;                  /* local status */
	NTIME    ntime;                    /* numeric time */
	char     str[BC_LINELTH+1];        /* scratch string */
	char     phase[BC_SHORTSTRLTH+1];  /* phase name */
	unsigned slen;                     /* string length */
	unsigned i;                        /* counter */

	/* executable code */

	if  (first_call)  {
		first_call = FALSE;
		fprintf( bull, "%s\n", efv_bull_header );
	} /*endif*/

	/* if event == NULL, then reset first_call */
	if  (event == NULL)  {
		first_call = TRUE;
		return;
	} /*endif*/

	/* put here station name (5 characters) */
	slen = strlen( event->station );
	if  (slen < 2)  {
		fprintf( bull, "GRFA1" );
	} else {
		fprintf( bull, "GRF" );
		fprintf( bull, "%s", (event->station)+slen-2 );
	} /*endif*/

	/* following onsets */
	if  (event->onset_count != 0 &&
		event->onset_count != EvEMPTY_ONSET_COUNT)  {
		fprintf( bull, "%c", '0'+(char)(event->onset_count) );
	} else {
		fprintf( bull, " " );
	} /*endif*/

	if  (event->onset_time[0] == EvEOS)  {
		printf( "*** event without onset time found ***\n" );
		fprintf( bull, "\n" );
		return;
	} /*endif*/

	locstat = BC_NOERROR;
	tc_t2n( event->onset_time, &ntime, &locstat );
	if  (Severe(&locstat))  {
		printf( "*** could not convert time %s ***\n", event->onset_time );
		fprintf( bull, "\n" );
		return;
	} /*endif*/

	/* print date */
	sprintf( str, "%02d", ntime.day );
	sprintf( str+2, "%02d", ntime.month );
	i = ntime.year;
	if  (i >= 100)  i -= 1900;
	if  (i >= 100)  i -= 100;
	sprintf( str+4, "%02d", i );
	fprintf( bull, "%s", str );

	/* print phase name */
	strcpy( phase, event->phase );
	EfTelexPhasename( phase );
	ut_cap( phase );
	switch  (event->onset_type)  {
	case EvcOnsetEmergent:  strcpy( str, "E" );  break;
	case EvcOnsetImpulsive:  strcpy( str, "I" );  break;
	case EvcOnsetUndefined: *str = EvEOS;  break;
	default:
		*str = EvEOS;
		printf( "*** illegal onset type found ***\n" );
		break;
	} /*endswitch*/
	strcat( str, phase );
	slen = (unsigned)strlen( str );
	fprintf( bull, "%s", str );
	for  (i=slen; i<7; i++)
		fprintf( bull, " " );

	/* print sign */
	switch  (event->sign)  {
	case EvcSignPositive:  fprintf( bull, "C" );  break;
	case EvcSignNegative:  fprintf( bull, "D" );  break;
	case EvcSignUndefined: fprintf( bull, " " );  break;
	default:
		printf( "*** illegal onset sign found ***\n" );
		fprintf( bull, " " );
		break;
	} /*endswitch*/

	/* print component */
	if  (event->component == EvEMPTY_COMPONENT)  {
		fprintf( bull, " " );
	} else {
		fprintf( bull, "%c", event->component );
	} /*endif*/

	/* print time */
	sprintf( str, "%02d", ntime.hour );
	sprintf( str+2, "%02d", ntime.min );
	sprintf( str+4, "%02d", ntime.sec );
	if  (event->onset_acc == EvcOnsetAccSecond)  {
		str[6] = ' ';
	} else {
		str[6] = '0' + Nint( (float)(ntime.ms)/100.0 );
	} /*endif*/
	str[7] = EvEOS;
	fprintf( bull, "%s", str );

	/* print period */
	if  (event->period == EvEMPTY_PERIOD)  {
		fprintf( bull, "   " );
	} else {
		sprintf( str, "%3d", Nint(event->period*10.0) );
		if  (strlen(str) == 3)  {
			fprintf( bull, "%s", str );
		} else {
			printf( "*** period %s truncated ***\n", str );
			fprintf( bull, "***" );
		} /*endif*/
	} /*endif*/

	/* print amplitude */
	if  (event->amplitude == EvEMPTY_AMPLITUDE)  {
		fprintf( bull, "    " );
	} else {
		sprintf( str, "%4d", Nint(event->amplitude) );
		if  (strlen(str) == 4)  {
			fprintf( bull, "%s", str );
		} else {
			printf( "*** amplitude %s truncated ***\n", str );
			fprintf( bull, "****" );
		} /*endif*/
	} /*endif*/

	/* print LP period */
	if  (event->lp_period == EvEMPTY_PERIOD)  {
		fprintf( bull, "   " );
	} else {
		sprintf( str, "%3d", Nint(event->lp_period*10.0) );
		if  (strlen(str) == 3)  {
			fprintf( bull, "%s", str );
		} else {
			printf( "*** LP period %s truncated ***\n", str );
			fprintf( bull, "***" );
		} /*endif*/
	} /*endif*/

	/* print LP amplitude */
	if  (event->lp_amplitude == EvEMPTY_AMPLITUDE)  {
		fprintf( bull, "   " );
	} else {
		if  (event->lp_amplitude >= 1.0)  {
			sprintf( str, "%3d", Nint(event->lp_amplitude) );
			if  (strlen(str) == 3)  {
				fprintf( bull, "%s", str );
			} else {
				printf( "*** amplitude %s truncated ***\n", str );
				fprintf( bull, "***" );
			} /*endif*/
		} else {
			fprintf( bull, "%3d", -Nint(event->lp_amplitude*100.0) );
		} /*endif*/
	} /*endif*/

	/* print slowness */
	if  (event->l_slowness == EvEMPTY_SLOWNESS)  {
		fprintf( bull, "   " );
	} else {
		sprintf( str, "%3d", Nint(event->l_slowness*10.0) );
		if  (strlen(str) == 3)  {
			fprintf( bull, "%s", str );
		} else {
			printf( "*** slowness %s truncated ***\n", str );
			fprintf( bull, "***" );
		} /*endif*/
	} /*endif*/

	/* print azimuth */
	if  (event->l_azimuth == EvEMPTY_AZIMUTH)  {
		fprintf( bull, "   " );
	} else {
		sprintf( str, "%3d", Nint(event->l_azimuth) );
		if  (strlen(str) == 3)  {
			fprintf( bull, "%s", str );
		} else {
			printf( "*** azimuth %s truncated ***\n", str );
			fprintf( bull, "***" );
		} /*endif*/
	} /*endif*/

	/* print quality */
	if  (event->quality == EvEMPTY_QUALITY)  {
		fprintf( bull, " " );
	} else {
		sprintf( str, "%1d", event->quality );
		if  (strlen(str) == 1)  {
			fprintf( bull, "%s", str );
		} else {
			printf( "*** quality %s truncated ***\n", str );
			fprintf( bull, "*" );
		} /*endif*/
	} /*endif*/

	/* print magnitude mb */
	if  (event->mag[EvcMagMb] == EvEMPTY_MAGNITUDE)  {
		fprintf( bull, "  " );
	} else {
		sprintf( str, "%2d", Nint(event->mag[EvcMagMb]*10.0) );
		if  (strlen(str) == 2)  {
			fprintf( bull, "%s", str );
		} else {
			printf( "*** magnitude mb %s truncated ***\n", str );
			fprintf( bull, "**" );
		} /*endif*/
	} /*endif*/

	/* print magnitude ms */
	if  (event->mag[EvcMagMs] == EvEMPTY_MAGNITUDE)  {
		fprintf( bull, "  " );
	} else {
		sprintf( str, "%2d", Nint(event->mag[EvcMagMs]*10.0) );
		if  (strlen(str) == 2)  {
			fprintf( bull, "%s", str );
		} else {
			printf( "*** magnitude ms %s truncated ***\n", str );
			fprintf( bull, "**" );
		} /*endif*/
	} /*endif*/

	/* print magnitude ml */
	if  (event->mag[EvcMagMl] == EvEMPTY_MAGNITUDE)  {
		fprintf( bull, "  " );
	} else {
		sprintf( str, "%2d", Nint(event->mag[EvcMagMl]*10.0) );
		if  (strlen(str) == 2)  {
			fprintf( bull, "%s", str );
		} else {
			printf( "*** magnitude ml %s truncated ***\n", str );
			fprintf( bull, "**" );
		} /*endif*/
	} /*endif*/

	/* print distance (deg) / distance (km) */
	if  (event->distance_deg != EvEMPTY_DISTANCE)  {
		sprintf( str, "%3d", Nint(event->distance_deg) );
		if  (strlen(str) == 3)  {
			fprintf( bull, "%s", str );
		} else {
			printf( "*** distance (deg) %s truncated ***\n", str );
			fprintf( bull, "***" );
		} /*endif*/
	} else if  (event->distance_km != EvEMPTY_DISTANCE)  {
		sprintf( str, "%6d", -Nint(event->distance_km) );
		if  (strlen(str) == 6)  {
			fprintf( bull, "%s", str );
		} else {
			printf( "*** distance (km) %s truncated ***\n", str );
			fprintf( bull, "******" );
		} /*endif*/
	} /*endif*/

	fprintf( bull, "\n" );

	if  (event->region[0] != EvEOS)  {
		fprintf( bull, "C                                        " );
		strcpy( str, event->region );
		ut_cap( str );
		fprintf( bull, "%s\n", str );
	} /*endif*/

	if  (Severe(status))  return;  /* just to touch status */

} /* end of EfPutBulletin */



/*-------------------------------------------------------------------*/



void EfSetBulletinHeader( char header[] )

/* Sets header text of bulletin
 *
 * parameters of routine
 * char       header[];      input; header text
 */
{
	/* executable code */

	if  (strlen(header) > BC_LINELTH)  {
		printf( "*** bulletin header text too long, truncated ***\n" );
		strncpy( efv_bull_header, header, BC_LINELTH );
	} else {
		strcpy( efv_bull_header, header );
	} /*endif*/
	ut_cap( efv_bull_header );

} /* end of EfSetBulletinHeader */



/*-------------------------------------------------------------------*/


/* file eventdsc.c
 *      ==========
 *
 * version 36, 19-Sep-2007
 *
 * event descriptor routines
 * K. Stammler, 25-Jul-93
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_TCUSRDEF
#include "eventdsc.h"



#define EvCOMMENT_END '~'




/* global variables */
static BOOLEAN evv_show_empty=FALSE;
	/* print empty values */



/* prototypes of local routines */
static void EvhFixedLengthString( char str[], int length, FILE *fp );
static char *EvhFindValue( char line[], char item[] );
static void EvhPromptReal( EvFloatT *value, FILE *inp, char prompt[],
	EvFloatT empty_val, EvFloatT limit_lo, EvFloatT limit_hi,
	char format[] );
static void EvhPromptInt( EvQualityT *value, FILE *inp, char prompt[],
	EvQualityT empty_val, EvQualityT limit_lo, EvQualityT limit_hi,
	char format[] );
static void EvhPromptComponent( char *comp, FILE *inp, char prompt[] );
static void EvhPromptString( char text[], unsigned maxlth,
	FILE *inp, char prompt[] );
static void EvhPromptOnsetTime( char onset[], EvOnsetAccT *acc, FILE *inp );
static void EvhPromptOnsetType( EvOnsetT *type, FILE *inp );
static void EvhPromptSign( EvSignT *sign, FILE *inp );
static void EvhPromptComment( EvCommentT *comment, FILE *inp,
	EvStatusT *status );
static BOOLEAN EvhKnownPhase( char phase[] );
static void EvhCheckTimeInput( char time[], BOOLEAN *ok, EvOnsetAccT *acc );



/*-------------------------------------------------------------------*/



void EvReadEventInteractive( FILE *inp, EvEventT *event,
	BOOLEAN *eof, EvStatusT *status )

/* Reads event from terminal
 *
 * parameters of routine
 * FILE       *inp;          input; input channel (usually stdin)
 * EvEventT   *event;        output; event read
 * BOOLEAN    *eof;          output; no more events to be entered
 * EvStatusT  *status;       output; return status
 */
{
	/* local variables */
	static BOOLEAN first=TRUE;             /* first loop */
	static int     remain_count=0;         /* counter of remaining onsets */
	char           str[BC_LINELTH+1];      /* scratch string */
	BOOLEAN        ok;                     /* repeat flag */

	/* executable code */

	/* ask whether or not to finish */
	*eof = FALSE;
	if  (!first && remain_count == 0)  {
		do  {
			fprintf( stdout, "   another event to enter ? [y/n]: " );
			EvReadLineInteractive( inp, BC_LINELTH, str );
			*str = Cap( *str );
			ok = (*str == 'Y' || *str == 'N' || *str == 'J');
		}  while (!ok);
		*eof = (*str == 'N');
		if  (*eof)  return;
	} else if  (remain_count > 0)  {
		printf( "   now enter secondary onset (remaining %d)\n\n",
			remain_count );
	} /*endif*/
	first = FALSE;

	EvInitializeEvent( event );
	strcpy( event->station, "GRA1" );

	do  {

		/* set fixed station code */
		strcpy( event->array, "GRF" );

		/* prompt station name */
		EvhPromptString( event->station, EvSTATIONLTH, inp, EvITEM_STATION );

		/* get date & time */
		EvhPromptOnsetTime( event->onset_time, &(event->onset_acc), inp );

		/* get onset type */
		EvhPromptOnsetType( &(event->onset_type), inp );

		/* get phase name */
		do  {
			EvhPromptString( event->phase, EvPHASELTH, inp, EvITEM_PHASE );
			ok = (event->phase[0] == EvEOS) || EvhKnownPhase(event->phase);
			if  (!ok)
				printf( "*** unknown phase %s, repeat input ***\n", event->phase );
		} while  (!ok);

		/* get sign */
		EvhPromptSign( &(event->sign), inp );

		/* get component */
		EvhPromptComponent( &(event->component), inp, EvITEM_COMPONENT );

		/* get onset count */
		if  (remain_count == 0)  {
			EvhPromptInt( &(event->onset_count), inp, EvITEM_ONSET_COUNT,
				EvEMPTY_ONSET_COUNT, EvLIMIT_LO_ONSET_COUNT,
				EvLIMIT_HI_ONSET_COUNT, EvFORMAT_ONSET_COUNT );
		} else {
			event->onset_count = EvEMPTY_ONSET_COUNT;
		} /*endif*/

		/* get period */
		EvhPromptReal( &(event->period), inp, EvITEM_PERIOD, EvEMPTY_PERIOD,
			EvLIMIT_LO_PERIOD, EvLIMIT_HI_PERIOD, EvFORMAT_PERIOD );

		/* get amplitude */
		EvhPromptReal( &(event->amplitude), inp, EvITEM_AMPLITUDE,
			EvEMPTY_AMPLITUDE, EvLIMIT_LO_AMPLITUDE,
			EvLIMIT_HI_AMPLITUDE, EvFORMAT_AMPLITUDE );

		/* get LP component */
		EvhPromptComponent( &(event->lp_component), inp, EvITEM_LP_COMPONENT );

		/* get LP period */
		EvhPromptReal( &(event->lp_period), inp, EvITEM_LP_PERIOD,
			EvEMPTY_PERIOD, EvLIMIT_LO_PERIOD,
			EvLIMIT_HI_PERIOD, EvFORMAT_PERIOD );

		/* get LP amplitude */
		EvhPromptReal( &(event->lp_amplitude), inp, EvITEM_LP_AMPLITUDE,
			EvEMPTY_AMPLITUDE, EvLIMIT_LO_AMPLITUDE,
			EvLIMIT_HI_AMPLITUDE, EvFORMAT_AMPLITUDE );

		/* get beam slowness */
		EvhPromptReal( &(event->b_slowness), inp, EvITEM_B_SLOWNESS,
			EvEMPTY_SLOWNESS, EvLIMIT_LO_SLOWNESS,
			EvLIMIT_HI_SLOWNESS, EvFORMAT_SLOWNESS );

		/* get beam azimuth */
		EvhPromptReal( &(event->b_azimuth), inp, EvITEM_B_AZIMUTH,
			EvEMPTY_AZIMUTH, EvLIMIT_LO_AZIMUTH,
			EvLIMIT_HI_AZIMUTH, EvFORMAT_AZIMUTH );

		/* get epicenter slowness */
		EvhPromptReal( &(event->l_slowness), inp, EvITEM_L_SLOWNESS,
			EvEMPTY_SLOWNESS, EvLIMIT_LO_SLOWNESS,
			EvLIMIT_HI_SLOWNESS, EvFORMAT_SLOWNESS );

		/* get epicenter azimuth */
		EvhPromptReal( &(event->l_azimuth), inp, EvITEM_L_AZIMUTH,
			EvEMPTY_AZIMUTH, EvLIMIT_LO_AZIMUTH,
			EvLIMIT_HI_AZIMUTH, EvFORMAT_AZIMUTH );

		/* get distance (deg) */
		EvhPromptReal( &(event->distance_deg), inp, EvITEM_DISTANCE_DEG,
			EvEMPTY_DISTANCE, EvLIMIT_LO_DISTANCE_DEG,
			EvLIMIT_HI_DISTANCE_DEG, EvFORMAT_DISTANCE );

		/* get distance (km) */
		EvhPromptReal( &(event->distance_km), inp, EvITEM_DISTANCE_KM,
			EvEMPTY_DISTANCE, EvLIMIT_LO_DISTANCE_KM,
			EvLIMIT_HI_DISTANCE_KM, EvFORMAT_DISTANCE );

		/* get quality */
		EvhPromptInt( &(event->quality), inp, EvITEM_QUALITY,
			EvEMPTY_QUALITY, EvLIMIT_LO_QUALITY, EvLIMIT_HI_QUALITY,
			EvFORMAT_QUALITY );

		/* get magnitude ms */
		EvhPromptReal( &(event->mag[EvcMagMs]), inp, EvITEM_MS,
			EvEMPTY_MAGNITUDE, EvLIMIT_LO_MAGNITUDE,
			EvLIMIT_HI_MAGNITUDE, EvFORMAT_MAGNITUDE );

		/* get magnitude mb */
		EvhPromptReal( &(event->mag[EvcMagMb]), inp, EvITEM_MB,
			EvEMPTY_MAGNITUDE, EvLIMIT_LO_MAGNITUDE,
			EvLIMIT_HI_MAGNITUDE, EvFORMAT_MAGNITUDE );

		/* get magnitude ml */
		EvhPromptReal( &(event->mag[EvcMagMl]), inp, EvITEM_ML,
			EvEMPTY_MAGNITUDE, EvLIMIT_LO_MAGNITUDE,
			EvLIMIT_HI_MAGNITUDE, EvFORMAT_MAGNITUDE );

		/* get region name */
		EvhPromptString( event->region, EvREGIONLTH, inp, EvITEM_REGION );

		/* get comment */
		EvhPromptComment( &(event->comment), inp, status );
		if  (Severe(status))  return;

		do  {
			fprintf( stdout, "\n\n\n" );
			EvPutEvent( stdout, event, status );
			if  (Severe(status))  return;
			fprintf( stdout, "   event ok ? [y/n]: " );
			EvReadLineInteractive( inp, BC_LINELTH, str );
			*str = Cap( *str );
			ok = (*str == 'Y' || *str == 'N' || *str == 'J');
		}  while (!ok);
		ok = (*str != 'N');

	}  while (!ok);

	if  (event->onset_count != EvEMPTY_ONSET_COUNT)  {
		remain_count = event->onset_count;
	} else {
		if  (remain_count > 0)
			remain_count--;
	} /*endif*/

} /* end of EvReadEventInteractive */



/*-------------------------------------------------------------------*/



void EvInitializeEvent( EvEventT *event )

/* Initializes event structure
 *
 * parameters of routine
 * EvEventT   *event;       output; event structure to be initialized
 */
{
	/* local variables */
	int      i;     /* counter */

	/* executable code */

	event->array[0] = EvEOS;
	event->station[0] = EvEOS;
	event->onset_time[0] = EvEOS;
	event->onset_type = EvcOnsetUndefined;
	event->onset_acc = EvcOnsetAccUndefined;
	event->onset_wdw_l = EvEMPTY_ONSET_WDW;
	event->onset_wdw_r = EvEMPTY_ONSET_WDW;
	event->onset_count = EvEMPTY_ONSET_COUNT;
	event->phase[0] = EvEOS;
	event->sign = EvcSignUndefined;
	event->component = EvEMPTY_COMPONENT;
	event->period = EvEMPTY_PERIOD;
	event->amplitude = EvEMPTY_AMPLITUDE;
	event->amplitude_time = EvEMPTY_AMPLITUDE_TIME;
	event->amplitude_vel = EvEMPTY_AMPLITUDE;
	event->lp_component = EvEMPTY_COMPONENT;
	event->lp_period = EvEMPTY_PERIOD;
	event->lp_amplitude = EvEMPTY_AMPLITUDE;
	event->bb_period = EvEMPTY_PERIOD;
	event->bb_amplitude = EvEMPTY_AMPLITUDE;
	event->b_slowness = EvEMPTY_SLOWNESS;
	event->b_azimuth = EvEMPTY_AZIMUTH;
	event->l_slowness = EvEMPTY_SLOWNESS;
	event->l_azimuth = EvEMPTY_AZIMUTH;
	event->theo_azim = EvEMPTY_AZIMUTH;
	event->theo_back_azim = EvEMPTY_AZIMUTH;
	event->distance_deg = EvEMPTY_DISTANCE;
	event->distance_km = EvEMPTY_DISTANCE;
	event->signoise = EvEMPTY_SIGNOISE;
	event->residual = EvEMPTY_RESIDUAL;
	event->resid_corr = EvEMPTY_RESID_CORR;
	event->quality = EvEMPTY_QUALITY;
	event->pick_type = EvcPickTypeUndefined;
	for  (i=0; i<EvcMagLast; i++)
		event->mag[i] = event->meanmag[i] = EvEMPTY_MAGNITUDE;
	event->mu_descr[0] = EvEOS;
	event->region_table = EvcRegionTableUndefined;
	event->region_id = EvEMPTY_REGION_ID;
	event->region[0] = EvEOS;
	event->comment.length = 0;
	event->comment.text = NULL;
	event->latitude = EvEMPTY_LATITUDE;
	event->longitude = EvEMPTY_LONGITUDE;
	event->depth = EvEMPTY_DEPTH;
	event->depth_type = EvcDepthUndefined;
	event->origin_time[0] = EvEOS;
	event->loc_method = EvcLocMethUndefined;
	event->loc_quality = EvcLocQualUndefined;
	event->velmod[0] = EvEOS;
	event->loc_addpar[0] = EvEOS;
	event->weight = EvEMPTY_WEIGHT;
	event->filter[0] = EvEOS;
	event->evid = EvEMPTY_EVID;
	event->ref_latitude = EvEMPTY_LATITUDE;
	event->ref_longitude = EvEMPTY_LONGITUDE;
	event->ref_name[0] = EvEOS;
	event->analyst[0] = EvEOS;
	event->stations_used = EvEMPTY_STATIONS_USED;
	event->event_type = EvcEventTypeUndefined;
	event->source[0] = EvEOS;
	event->ap_source = EvcApSourceUndefined;
	event->err.lat_km = EvEMPTY_ERR_FLOAT;
	event->err.lon_km = EvEMPTY_ERR_FLOAT;
	event->err.dep = EvEMPTY_ERR_FLOAT;
	event->err.orig = EvEMPTY_ERR_FLOAT;
	event->err.smajor = EvEMPTY_ERR_FLOAT;
	event->err.sminor = EvEMPTY_ERR_FLOAT;
	event->err.majstrike = EvEMPTY_ERR_FLOAT;
	event->err.azim_max_gap = EvEMPTY_ERR_FLOAT;
	event->err.resid_rms = EvEMPTY_ERR_FLOAT;
	event->phase_flags[0] = EvEOS;
	event->momten[0] = EvEOS;
	event->momten_descr[0] = EvEOS;
	event->m0 = EvEMPTY_MAGNITUDE;
	event->fps_angles[0] = EvEOS;
	event->fps_descr[0] = EvEOS;
	event->cornerfreq = EvEMPTY_FREQ;
	event->lowfreqlevel = EvEMPTY_FREQ;

} /* end of EvInitializeEvent */



/*-------------------------------------------------------------------*/



void EvPutEvent( FILE *file, EvEventT *event, EvStatusT *status )

/* Writes event descriptor to file
 *
 * parameters of routine
 * FILE       *file;         input; pointer to output file
 * EvEventT   *event;        input; event to write to file
 * EvStatusT  *status;       output; return status
 */
{
	/* local variables */
	char     fmt[BC_SHORTSTRLTH+1];    /* format string */

	/* executable code */

	/* if event pointer is NULL then return (no trailer required) */
	if  (event == NULL)  return;

	/* event ID */
	if  (event->evid == EvEMPTY_EVID)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_EVID, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_EVID, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_EVID );
		fprintf( file, fmt, event->evid );
	} /*endif*/

	/* array name */
	if  (event->array[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ARRAY, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ARRAY, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->array );
	} /*endif*/

	/* official station code */
	if  (event->station[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_STATION, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_STATION, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->station );
	} /*endif*/

	/* onset time */
	if  (event->onset_time[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ONSET_TIME, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ONSET_TIME, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->onset_time );
	} /*endif*/

	/* onset type */
	switch  (event->onset_type)  {
	case EvcOnsetUndefined:
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ONSET_TYPE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
		break;
	case EvcOnsetEmergent:
		EvhFixedLengthString( EvITEM_ONSET_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_ONSET_EMERGENT );
		break;
	case EvcOnsetImpulsive:
		EvhFixedLengthString( EvITEM_ONSET_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_ONSET_IMPULSIVE );
		break;
	default:
		*status = EveILL_VALUE;
		return;
	} /*endswitch*/

	/* onset accuracy */
	switch  (event->onset_acc)  {
	case EvcOnsetAccUndefined:
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ONSET_ACC, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
		break;
	case EvcOnsetAccSecond:
		EvhFixedLengthString( EvITEM_ONSET_ACC, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_ONSET_ACC_SEC );
		break;
	case EvcOnsetAccMillisecond:
		EvhFixedLengthString( EvITEM_ONSET_ACC, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_ONSET_ACC_MSEC );
		break;
	default:
		*status = EveILL_VALUE;
		return;
	} /*endswitch*/

	/* left onset window */
	if  (event->onset_wdw_l == EvEMPTY_ONSET_WDW)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ONSET_WDW_L, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ONSET_WDW_L, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_ONSET_WDW );
		fprintf( file, fmt, event->onset_wdw_l );
	} /*endif*/

	/* right onset window */
	if  (event->onset_wdw_r == EvEMPTY_ONSET_WDW)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ONSET_WDW_R, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ONSET_WDW_R, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_ONSET_WDW );
		fprintf( file, fmt, event->onset_wdw_r );
	} /*endif*/

	/* onset count */
	if  (event->onset_count == EvEMPTY_ONSET_COUNT)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ONSET_COUNT, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ONSET_COUNT, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_ONSET_COUNT );
		fprintf( file, fmt, event->onset_count );
	} /*endif*/

	/* phase name */
	if  (event->phase[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_PHASE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_PHASE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->phase );
	} /*endif*/

	/* onset sign */
	switch  (event->sign)  {
	case EvcSignUndefined:
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_SIGN, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
		break;
	case EvcSignPositive:
		EvhFixedLengthString( EvITEM_SIGN, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_SIGN_POSITIVE );
		break;
	case EvcSignNegative:
		EvhFixedLengthString( EvITEM_SIGN, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_SIGN_NEGATIVE );
		break;
	default:
		*status = EveILL_VALUE;
		return;
	} /*endswitch*/

	/* event type */
	switch  (event->event_type)  {
	case EvcEventTypeUndefined:
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_EVENT_TYPE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
		break;
	case EvcEventTypeTeleQuake:
		EvhFixedLengthString( EvITEM_EVENT_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_EVTYPE_TELE_QUAKE );
		break;
	case EvcEventTypeNuclear:
		EvhFixedLengthString( EvITEM_EVENT_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_EVTYPE_NUCLEAR );
		break;
	case EvcEventTypeRegioQuake:
		EvhFixedLengthString( EvITEM_EVENT_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_EVTYPE_REGIO_QUAKE );
		break;
	case EvcEventTypeLocalQuake:
		EvhFixedLengthString( EvITEM_EVENT_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_EVTYPE_LOCAL_QUAKE );
		break;
	case EvcEventTypeBlast:
		EvhFixedLengthString( EvITEM_EVENT_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_EVTYPE_BLAST );
		break;
	case EvcEventTypeMining:
		EvhFixedLengthString( EvITEM_EVENT_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_EVTYPE_MINING );
		break;
	default:
		*status = EveILL_VALUE;
		return;
	} /*endswitch*/

	/* applied filter */
	if  (event->filter[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_FILTER, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_FILTER, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->filter );
	} /*endif*/

	/* component */
	if  (event->component == EvEMPTY_COMPONENT)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_COMPONENT, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_COMPONENT, EvDESCR_LENGTH, file );
		fprintf( file, "%c\n", event->component );
	} /*endif*/

	/* period */
	if  (event->period == EvEMPTY_PERIOD)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_PERIOD, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_PERIOD, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_PERIOD );
		fprintf( file, fmt, event->period );
	} /*endif*/

	/* amplitude */
	if  (event->amplitude == EvEMPTY_AMPLITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_AMPLITUDE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_AMPLITUDE, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_AMPLITUDE );
		fprintf( file, fmt, event->amplitude );
	} /*endif*/

	/* velocity amplitude */
	if  (event->amplitude_vel == EvEMPTY_AMPLITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_AMPLITUDE_VEL, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_AMPLITUDE_VEL, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_AMPLITUDE );
		fprintf( file, fmt, event->amplitude_vel );
	} /*endif*/

	/* amplitude time */
	if  (event->amplitude_time == EvEMPTY_AMPLITUDE_TIME)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_AMPLITUDE_TIME, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_AMPLITUDE_TIME, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_AMPLITUDE_TIME );
		fprintf( file, fmt, event->amplitude_time );
	} /*endif*/

	/* LP component */
	if  (event->lp_component == EvEMPTY_COMPONENT)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_LP_COMPONENT, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_LP_COMPONENT, EvDESCR_LENGTH, file );
		fprintf( file, "%c\n", event->lp_component );
	} /*endif*/

	/* LP period */
	if  (event->lp_period == EvEMPTY_PERIOD)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_LP_PERIOD, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_LP_PERIOD, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_PERIOD );
		fprintf( file, fmt, event->lp_period );
	} /*endif*/

	/* LP amplitude */
	if  (event->lp_amplitude == EvEMPTY_AMPLITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_LP_AMPLITUDE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_LP_AMPLITUDE, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_AMPLITUDE );
		fprintf( file, fmt, event->lp_amplitude );
	} /*endif*/

	/* BB period */
	if  (event->bb_period == EvEMPTY_PERIOD)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_BB_PERIOD, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_BB_PERIOD, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_PERIOD );
		fprintf( file, fmt, event->bb_period );
	} /*endif*/

	/* BB amplitude */
	if  (event->bb_amplitude == EvEMPTY_AMPLITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_BB_AMPLITUDE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_BB_AMPLITUDE, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_AMPLITUDE );
		fprintf( file, fmt, event->bb_amplitude );
	} /*endif*/

	/* amplitude & period source */
	switch  (event->ap_source)  {
	case EvcApSourceUndefined:
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_AP_SOURCE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
		break;
	case EvcApSourceDirect:
		EvhFixedLengthString( EvITEM_AP_SOURCE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_APSRC_DIRECT );
		break;
	case EvcApSourceBeam:
		EvhFixedLengthString( EvITEM_AP_SOURCE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_APSRC_BEAM );
		break;
	case EvcApSourceAlign:
		EvhFixedLengthString( EvITEM_AP_SOURCE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_APSRC_ALIGN );
		break;
	default:
		*status = EveILL_VALUE;
		return;
	} /*endswitch*/

	/* signal/noise ratio */
	if  (event->signoise == EvEMPTY_SIGNOISE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_SIGNOISE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_SIGNOISE, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_SIGNOISE );
		fprintf( file, fmt, event->signoise );
	} /*endif*/

	/* residual time */
	if  (event->residual == EvEMPTY_RESIDUAL)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_RESIDUAL, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_RESIDUAL, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_RESIDUAL );
		fprintf( file, fmt, event->residual );
	} /*endif*/

	/* residual correction */
	if  (event->resid_corr == EvEMPTY_RESID_CORR)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_RESID_CORR, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_RESID_CORR, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_RESIDUAL );
		fprintf( file, fmt, event->resid_corr );
	} /*endif*/

	/* quality */
	if  (event->quality == EvEMPTY_QUALITY)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_QUALITY, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_QUALITY, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_QUALITY );
		fprintf( file, fmt, event->quality );
	} /*endif*/

	/* pick type */
	switch  (event->pick_type)  {
	case EvcPickTypeUndefined:
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_PICK_TYPE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
		break;
	case EvcPickTypeManual:
		EvhFixedLengthString( EvITEM_PICK_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_PICKTYPE_MANUAL );
		break;
	case EvcPickTypeAuto:
		EvhFixedLengthString( EvITEM_PICK_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_PICKTYPE_AUTO );
		break;
	case EvcPickTypeTheo:
		EvhFixedLengthString( EvITEM_PICK_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_PICKTYPE_THEO );
		break;
	default:
		*status = EveILL_VALUE;
		return;
	} /*endswitch*/

	/* phase weight */
	if  (event->weight == EvEMPTY_WEIGHT)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_WEIGHT, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_WEIGHT, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_WEIGHT );
		fprintf( file, fmt, event->weight );
	} /*endif*/

	/* beam slowness */
	if  (event->b_slowness == EvEMPTY_SLOWNESS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_B_SLOWNESS, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_B_SLOWNESS, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_SLOWNESS );
		fprintf( file, fmt, event->b_slowness );
	} /*endif*/

	/* beam azimuth */
	if  (event->b_azimuth == EvEMPTY_AZIMUTH)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_B_AZIMUTH, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_B_AZIMUTH, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_AZIMUTH );
		fprintf( file, fmt, event->b_azimuth );
	} /*endif*/

	/* epicenter slowness */
	if  (event->l_slowness == EvEMPTY_SLOWNESS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_L_SLOWNESS, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_L_SLOWNESS, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_SLOWNESS );
		fprintf( file, fmt, event->l_slowness );
	} /*endif*/

	/* epicenter (back) azimuth */
	if  (event->l_azimuth == EvEMPTY_AZIMUTH)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_L_AZIMUTH, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_L_AZIMUTH, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_AZIMUTH );
		fprintf( file, fmt, event->l_azimuth );
	} /*endif*/

	/* theoretical azimuth */
	if  (event->theo_azim == EvEMPTY_AZIMUTH)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_THEO_AZIM, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_THEO_AZIM, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_AZIMUTH );
		fprintf( file, fmt, event->theo_azim );
	} /*endif*/

	/* theoretical back azimuth */
	if  (event->theo_back_azim == EvEMPTY_AZIMUTH)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_THEO_BACK_AZIM, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_THEO_BACK_AZIM, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_AZIMUTH );
		fprintf( file, fmt, event->theo_back_azim );
	} /*endif*/

	/* distance (deg) */
	if  (event->distance_deg == EvEMPTY_DISTANCE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_DISTANCE_DEG, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_DISTANCE_DEG, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_DISTANCE );
		fprintf( file, fmt, event->distance_deg );
	} /*endif*/

	/* distance (km) */
	if  (event->distance_km == EvEMPTY_DISTANCE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_DISTANCE_KM, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_DISTANCE_KM, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_DISTANCE );
		fprintf( file, fmt, event->distance_km );
	} /*endif*/

	/* magnitude ms */
	if  (event->mag[EvcMagMs] == EvEMPTY_MAGNITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MS, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MS, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_MAGNITUDE );
		fprintf( file, fmt, event->mag[EvcMagMs] );
	} /*endif*/

	/* magnitude mb */
	if  (event->mag[EvcMagMb] == EvEMPTY_MAGNITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MB, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MB, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_MAGNITUDE );
		fprintf( file, fmt, event->mag[EvcMagMb] );
	} /*endif*/

	/* magnitude ml */
	if  (event->mag[EvcMagMl] == EvEMPTY_MAGNITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ML, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ML, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_MAGNITUDE );
		fprintf( file, fmt, event->mag[EvcMagMl] );
	} /*endif*/

	/* magnitude mw */
	if  (event->mag[EvcMagMw] == EvEMPTY_MAGNITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MW, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MW, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_MAGNITUDE );
		fprintf( file, fmt, event->mag[EvcMagMw] );
	} /*endif*/

	/* magnitude mbb */
	if  (event->mag[EvcMagMbb] == EvEMPTY_MAGNITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MBB, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MBB, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_MAGNITUDE );
		fprintf( file, fmt, event->mag[EvcMagMbb] );
	} /*endif*/

	/* magnitude mu (user magnitude) */
	if  (event->mag[EvcMagMu] == EvEMPTY_MAGNITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MU, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MU, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_MAGNITUDE );
		fprintf( file, fmt, event->mag[EvcMagMu] );
	} /*endif*/

	/* mean magnitude ms */
	if  (event->meanmag[EvcMagMs] == EvEMPTY_MAGNITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MEAN_MS, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MEAN_MS, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_MAGNITUDE );
		fprintf( file, fmt, event->meanmag[EvcMagMs] );
	} /*endif*/

	/* magnitude mb */
	if  (event->meanmag[EvcMagMb] == EvEMPTY_MAGNITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MEAN_MB, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MEAN_MB, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_MAGNITUDE );
		fprintf( file, fmt, event->meanmag[EvcMagMb] );
	} /*endif*/

	/* magnitude ml */
	if  (event->meanmag[EvcMagMl] == EvEMPTY_MAGNITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MEAN_ML, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MEAN_ML, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_MAGNITUDE );
		fprintf( file, fmt, event->meanmag[EvcMagMl] );
	} /*endif*/

	/* magnitude mw */
	if  (event->meanmag[EvcMagMw] == EvEMPTY_MAGNITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MEAN_MW, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MEAN_MW, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_MAGNITUDE );
		fprintf( file, fmt, event->meanmag[EvcMagMw] );
	} /*endif*/

	/* magnitude mbb */
	if  (event->meanmag[EvcMagMbb] == EvEMPTY_MAGNITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MEAN_MBB, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MEAN_MBB, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_MAGNITUDE );
		fprintf( file, fmt, event->meanmag[EvcMagMbb] );
	} /*endif*/

	/* magnitude mu (user magnitude) */
	if  (event->meanmag[EvcMagMu] == EvEMPTY_MAGNITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MEAN_MU, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MEAN_MU, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_MAGNITUDE );
		fprintf( file, fmt, event->meanmag[EvcMagMu] );
	} /*endif*/

	/* description of mu */
	if  (event->mu_descr[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MU_DESCR, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MU_DESCR, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->mu_descr );
	} /*endif*/

	/* corner frequency of spectrum */
	if  (event->cornerfreq == EvEMPTY_FREQ)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_CORNERFREQ, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_CORNERFREQ, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_FREQ );
		fprintf( file, fmt, event->cornerfreq );
	} /*endif*/

	/* low frequency amplitude level of spectrum */
	if  (event->lowfreqlevel == EvEMPTY_FREQ)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_LOWFREQLEVEL, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_LOWFREQLEVEL, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_FREQ );
		fprintf( file, fmt, event->lowfreqlevel );
	} /*endif*/

	/* comment */
	if  (event->comment.length == 0)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_COMMENT, EvDESCR_LENGTH, file );
			fprintf( file, "%s%c\n", EvEMPTY_TEXT, EvCOMMENT_END );
		} /*endif*/
	} else {
		if  (event->comment.text == NULL)  {
			*status = EveCOMMENT_PTR;
			return;
		} /*endif*/
		EvhFixedLengthString( EvITEM_COMMENT, EvDESCR_LENGTH, file );
		fprintf( file, "%s%c\n", event->comment.text, EvCOMMENT_END );
	} /*endif*/

	/* latitude */
	if  (event->latitude == EvEMPTY_LATITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_LATITUDE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_LATITUDE, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_LATITUDE );
		fprintf( file, fmt, event->latitude );
	} /*endif*/

	/* longitude */
	if  (event->longitude == EvEMPTY_LONGITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_LONGITUDE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_LONGITUDE, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_LONGITUDE );
		fprintf( file, fmt, event->longitude );
	} /*endif*/

	/* depth */
	if  (event->depth == EvEMPTY_DEPTH)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_DEPTH, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_DEPTH, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_DEPTH );
		fprintf( file, fmt, event->depth );
	} /*endif*/

	/* depth type */
	switch  (event->depth_type)  {
	case EvcDepthUndefined:
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_DEPTH_TYPE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
		break;
	case EvcDepthPreset:
		EvhFixedLengthString( EvITEM_DEPTH_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_DEPTH_PRESET );
		break;
	case EvcDepthEstimated:
		EvhFixedLengthString( EvITEM_DEPTH_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_DEPTH_ESTIMATED );
		break;
	case EvcDepthFree:
		EvhFixedLengthString( EvITEM_DEPTH_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_DEPTH_FREE );
		break;
	case EvcDepthPoor:
		EvhFixedLengthString( EvITEM_DEPTH_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_DEPTH_POOR );
		break;
	case EvcDepthLessWell:
		EvhFixedLengthString( EvITEM_DEPTH_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_DEPTH_LESSWELL );
		break;
	case EvcDepthReliable:
		EvhFixedLengthString( EvITEM_DEPTH_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_DEPTH_RELIABLE );
		break;
	case EvcDepthExternal:
		EvhFixedLengthString( EvITEM_DEPTH_TYPE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_DEPTH_EXTERNAL );
		break;
	default:
		*status = EveILL_VALUE;
		return;
	} /*endswitch*/

	/* origin time */
	if  (event->origin_time[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ORIGIN_TIME, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ORIGIN_TIME, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->origin_time );
	} /*endif*/

	/* region table */
	switch  (event->region_table)  {
	case EvcRegionTableUndefined:
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_REGION_TABLE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
		break;
	case EvcRegionTableFlinnEngdahl:
		EvhFixedLengthString( EvITEM_REGION_TABLE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_REGTABLE_FLINN_ENGDAHL );
		break;
	case EvcRegionTableGermanGeo:
		EvhFixedLengthString( EvITEM_REGION_TABLE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_REGTABLE_GERMAN_GEO );
		break;
	case EvcRegionTableSeismoTec:
		EvhFixedLengthString( EvITEM_REGION_TABLE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_REGTABLE_SEISMOTEC );
		break;
	default:
		*status = EveILL_VALUE;
		return;
	} /*endswitch*/

	/* region ID */
	if  (event->region_id == EvEMPTY_REGION_ID)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_REGION_ID, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_REGION_ID, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_REGION_ID );
		fprintf( file, fmt, event->region_id );
	} /*endif*/

	/* source region */
	if  (event->region[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_REGION, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_REGION, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->region );
	} /*endif*/

	/* location method */
	switch  (event->loc_method)  {
	case EvcLocMethUndefined:
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_LOC_METHOD, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
		break;
	case EvcLocMethCorrBeam:
		EvhFixedLengthString( EvITEM_LOC_METHOD, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LMETHOD_CORRBEAM );
		break;
	case EvcLocMethUncorrBeam:
		EvhFixedLengthString( EvITEM_LOC_METHOD, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LMETHOD_UNCORRBEAM );
		break;
	case EvcLocMethResidCorr:
		EvhFixedLengthString( EvITEM_LOC_METHOD, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LMETHOD_RESIDCORR );
		break;
	case EvcLocMethHypo:
		EvhFixedLengthString( EvITEM_LOC_METHOD, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LMETHOD_HYPO );
		break;
	case EvcLocMethLocsat:
		EvhFixedLengthString( EvITEM_LOC_METHOD, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LMETHOD_LOCSAT );
		break;
	case EvcLocMethHypocenter:
		EvhFixedLengthString( EvITEM_LOC_METHOD, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LMETHOD_HYPOCENTER );
		break;
	case EvcLocMethHypo71:
		EvhFixedLengthString( EvITEM_LOC_METHOD, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LMETHOD_HYPO71 );
		break;
	case EvcLocMethHyposat:
		EvhFixedLengthString( EvITEM_LOC_METHOD, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LMETHOD_HYPOSAT );
		break;
	case EvcLocMethRelTrav:
		EvhFixedLengthString( EvITEM_LOC_METHOD, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LMETHOD_RELTRAV );
		break;
	case EvcLocMethExternal:
		EvhFixedLengthString( EvITEM_LOC_METHOD, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LMETHOD_EXTERNAL );
		break;
	default:
		*status = EveILL_VALUE;
		return;
	} /*endswitch*/

	/* location quality */
	switch  (event->loc_quality)  {
	case EvcLocQualUndefined:
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_LOC_QUALITY, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
		break;
	case EvcLocQualTooWeak:
		EvhFixedLengthString( EvITEM_LOC_QUALITY, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LOCQ_TOOWEAK );
		break;
	case EvcLocQualIncoherent:
		EvhFixedLengthString( EvITEM_LOC_QUALITY, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LOCQ_INCOHERENT );
		break;
	case EvcLocQualNoBearing:
		EvhFixedLengthString( EvITEM_LOC_QUALITY, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LOCQ_NOBEARING );
		break;
	case EvcLocQualRegion:
		EvhFixedLengthString( EvITEM_LOC_QUALITY, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LOCQ_REGION );
		break;
	case EvcLocQualReliable:
		EvhFixedLengthString( EvITEM_LOC_QUALITY, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", EvTEXT_LOCQ_RELIABLE );
		break;
	default:
		*status = EveILL_VALUE;
		return;
	} /*endswitch*/

	/* velocity model used for location */
	if  (event->velmod[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_LOC_VELMOD, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_LOC_VELMOD, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->velmod );
	} /*endif*/

	/* additional parameters used for location */
	if  (event->loc_addpar[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_LOC_ADDPAR, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_LOC_ADDPAR, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->loc_addpar );
	} /*endif*/

	/* number of stations used */
	if  (event->stations_used == EvEMPTY_STATIONS_USED)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_STATIONS_USED, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_STATIONS_USED, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_STATIONS_USED );
		fprintf( file, fmt, event->stations_used );
	} /*endif*/

	/* reference latitude */
	if  (event->ref_latitude == EvEMPTY_LATITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_REF_LATITUDE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_REF_LATITUDE, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_LATITUDE );
		fprintf( file, fmt, event->ref_latitude );
	} /*endif*/

	/* reference longitude */
	if  (event->ref_longitude == EvEMPTY_LONGITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_REF_LONGITUDE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_REF_LONGITUDE, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_LONGITUDE );
		fprintf( file, fmt, event->ref_longitude );
	} /*endif*/

	/* reference location name */
	if  (event->ref_name[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_REF_NAME, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_REF_NAME, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->ref_name );
	} /*endif*/

	/* source of information */
	if  (event->source[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_SOURCE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_SOURCE, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->source );
	} /*endif*/

	/* latitude error */
	if  (event->err.lat_km == EvEMPTY_ERR_FLOAT)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ERR_LAT, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ERR_LAT, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_ERR_FLOAT );
		fprintf( file, fmt, event->err.lat_km );
	} /*endif*/

	/* longitude error */
	if  (event->err.lon_km == EvEMPTY_ERR_FLOAT)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ERR_LON, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ERR_LON, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_ERR_FLOAT );
		fprintf( file, fmt, event->err.lon_km );
	} /*endif*/

	/* depth error */
	if  (event->err.dep == EvEMPTY_ERR_FLOAT)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ERR_DEPTH, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ERR_DEPTH, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_ERR_FLOAT );
		fprintf( file, fmt, event->err.dep );
	} /*endif*/

	/* origin time error */
	if  (event->err.orig == EvEMPTY_ERR_FLOAT)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ERR_ORIGIN, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ERR_ORIGIN, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_ERR_FLOAT );
		fprintf( file, fmt, event->err.orig );
	} /*endif*/

	/* error ellipse semi-major */
	if  (event->err.smajor == EvEMPTY_ERR_FLOAT)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ERR_SMAJOR, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ERR_SMAJOR, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_ERR_FLOAT );
		fprintf( file, fmt, event->err.smajor );
	} /*endif*/

	/* error ellipse semi-minor */
	if  (event->err.sminor == EvEMPTY_ERR_FLOAT)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ERR_SMINOR, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ERR_SMINOR, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_ERR_FLOAT );
		fprintf( file, fmt, event->err.sminor );
	} /*endif*/

	/* error ellipse strike */
	if  (event->err.majstrike == EvEMPTY_ERR_FLOAT)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ERR_MAJSTRIKE, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ERR_MAJSTRIKE, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_ERR_FLOAT );
		fprintf( file, fmt, event->err.majstrike );
	} /*endif*/

	/* maximum azimuthal gap */
	if  (event->err.azim_max_gap == EvEMPTY_ERR_FLOAT)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_AZIM_MAX_GAP, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_AZIM_MAX_GAP, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_ERR_FLOAT );
		fprintf( file, fmt, event->err.azim_max_gap );
	} /*endif*/

	/* error ellipse strike */
	if  (event->err.resid_rms == EvEMPTY_ERR_FLOAT)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_RESID_RMS, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_RESID_RMS, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_ERR_FLOAT );
		fprintf( file, fmt, event->err.resid_rms );
	} /*endif*/

	/* phase flags */
	if  (event->phase_flags[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_PHASE_FLAGS, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_PHASE_FLAGS, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->phase_flags );
	} /*endif*/

	/* analyst */
	if  (event->analyst[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_ANALYST, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_ANALYST, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->analyst );
	} /*endif*/

	/* moment tensor elements */
	if  (event->momten[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MOMTEN, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MOMTEN, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->momten );
	} /*endif*/

	/* description of moment tensor */
	if  (event->momten_descr[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_MOMTEN_DESCR, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_MOMTEN_DESCR, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->momten_descr );
	} /*endif*/

	/* scalar moment */
	if  (event->m0 == EvEMPTY_MAGNITUDE)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_M0, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_M0, EvDESCR_LENGTH, file );
		sprintf( fmt, "%s\n", EvFORMAT_MOMENT );
		fprintf( file, fmt, event->m0 );
	} /*endif*/

	/* fault plane solution */
	if  (event->fps_angles[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_FPS_ANGLES, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_FPS_ANGLES, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->fps_angles );
	} /*endif*/

	/* description of fault plane values */
	if  (event->fps_descr[0] == EvEOS)  {
		if  (evv_show_empty)  {
			EvhFixedLengthString( EvITEM_FPS_DESCR, EvDESCR_LENGTH, file );
			fprintf( file, "%s\n", EvEMPTY_TEXT );
		} /*endif*/
	} else {
		EvhFixedLengthString( EvITEM_FPS_DESCR, EvDESCR_LENGTH, file );
		fprintf( file, "%s\n", event->fps_descr );
	} /*endif*/

	/* put terminator */
	fprintf( file, "%s\n\n\n", EvITEM_END_OF_EVENT );

} /* end of EvPutEvent */



/*-------------------------------------------------------------------*/



void EvCreateEventfile( char filename[], EvEventT *event,
	EvStatusT *status )

/* Creates new event file named "filename".  An already existing
 * file of the same name is overwritten
 *
 * parameters of routine
 * char       filename[];      input; name of event file to be created
 * EvEventT   *event;          input; event information
 * EvStatusT  *status;         output; return status
 */
{
	/* local variables */
	FILE     *fp;        /* file pointer */

	/* executable code */

	fp = fopen( filename, "w" );
	if  (fp == NULL)  {
		*status = EveOPEN_WRITE;
		return;
	} /*endif*/

	EvPutEvent( fp, event, status );

	fclose( fp );

} /* end of EvCreateEventfile */



/*-------------------------------------------------------------------*/



void EvAppendEventfile( char filename[], EvEventT *event,
	EvStatusT *status )

/* Appends event to existing file named "filename".  If the file does
 * not yet exist it will be created.
 *
 * parameters of routine
 * char       filename[];      input; name of event file to be appended
 * EvEventT   *event;          input; event information
 * EvStatusT  *status;         output; return status
 */
{
	/* local variables */
	FILE     *fp;        /* file pointer */

	/* executable code */

	fp = fopen( filename, "a" );
	if  (fp == NULL)  {
		*status = EveOPEN_WRITE;
		return;
	} /*endif*/

	EvPutEvent( fp, event, status );

	fclose( fp );

} /* end of EvAppendEventfile */



/*-------------------------------------------------------------------*/



void EvGetEvent( FILE *file, EvEventT *event, BOOLEAN *eof,
	EvStatusT *status )

/* Reads one event block from file "file".  If "eof" is TRUE then
 * the end of file is found and "event" contains no valid information.
 * In this case "status" is set to an error code only if the file is
 * incorrectly terminated.
 *
 * parameters of routine
 * FILE       *file;       input; pointer to input file
 * EvEventT   *event;      output; event read from file
 * BOOLEAN    *eof;        output; end of file found
 * EvStatusT  *status;     output; return status
 */
{
	/* local variables */
	char     line[BC_LINELTH+1];     /* current line read */
	char     *value;                 /* pointer to value */
	unsigned slen;                   /* string length */
	BOOLEAN  event_started;          /* some information found */

	/* executable code */

	EvInitializeEvent( event );

	*eof = FALSE;
	event_started = FALSE;
	FOREVER {
		/* get next line from file */
		if  (fgets(line,BC_LINELTH,file) == NULL)  {
			*eof = TRUE;
			if  (event_started)
				*status = EveMISSING_TERM;
			return;
		} /*endif*/
		/* check if there is something to read */
		if  (*line == '!' || *line == '\n' || *line == EvEOS)  continue;
		event_started = TRUE;
		/* remove line terminator */
		slen = (unsigned)strlen( line );
		if  (line[slen-1] == '\n')  line[slen-1] = EvEOS;
		/* try to identify information */
		if  (NULL != (value = EvhFindValue(line,EvITEM_ARRAY)))  {
			if  (event->array[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvSTATIONLTH)  {*status = EveSTROVFL; return;}
				strcpy( event->array, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_STATION)))  {
			if  (event->station[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvSTATIONLTH)  {*status = EveSTROVFL; return;}
				strcpy( event->station, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ONSET_TIME)))  {
			if  (event->onset_time[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvTIMELTH)  {*status = EveSTROVFL; return;}
				strcpy( event->onset_time, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ONSET_COUNT)))  {
			if  (event->onset_count != EvEMPTY_ONSET_COUNT)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%d",&(event->onset_count)) != 1)  {
					*status = EveREAD_INT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ONSET_TYPE)))  {
			if  (event->onset_type != EvcOnsetUndefined)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvTEXT_ONSET_EMERGENT) == 0)  {
				event->onset_type = EvcOnsetEmergent;
			} else if  (strcmp(value,EvTEXT_ONSET_IMPULSIVE) == 0)  {
				event->onset_type = EvcOnsetImpulsive;
			} else if  (strcmp(value,EvEMPTY_TEXT) == 0)  {
				event->onset_type = EvcOnsetUndefined;
			} else {
				fprintf( stderr, "*** unkown: %s ***\n", line );
				*status = EveILL_VALUE;
				return;
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ONSET_ACC)))  {
			if  (event->onset_acc != EvcOnsetAccUndefined)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvTEXT_ONSET_ACC_SEC) == 0)  {
				event->onset_acc = EvcOnsetAccSecond;
			} else if  (strcmp(value,EvTEXT_ONSET_ACC_MSEC) == 0)  {
				event->onset_acc = EvcOnsetAccMillisecond;
			} else if  (strcmp(value,EvEMPTY_TEXT) == 0)  {
				event->onset_acc = EvcOnsetAccUndefined;
			} else {
				fprintf( stderr, "*** unkown: %s ***\n", line );
				*status = EveILL_VALUE;
				return;
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ONSET_WDW_L)))  {
			if  (event->onset_wdw_l != EvEMPTY_ONSET_WDW)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->onset_wdw_l)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ONSET_WDW_R)))  {
			if  (event->onset_wdw_r != EvEMPTY_ONSET_WDW)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->onset_wdw_r)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_PHASE)))  {
			if  (event->phase[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvPHASELTH)  {*status = EveSTROVFL; return;}
				strcpy( event->phase, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_SIGN)))  {
			if  (event->sign != EvcSignUndefined)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvTEXT_SIGN_POSITIVE) == 0)  {
				event->sign = EvcSignPositive;
			} else if  (strcmp(value,EvTEXT_SIGN_NEGATIVE) == 0)  {
				event->sign = EvcSignNegative;
			} else if  (strcmp(value,EvEMPTY_TEXT) == 0)  {
				event->sign = EvcSignUndefined;
			} else {
				fprintf( stderr, "*** unkown: %s ***\n", line );
				*status = EveILL_VALUE;
				return;
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_COMPONENT)))  {
			if  (event->component != EvEMPTY_COMPONENT)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > 1)  {*status = EveSTROVFL; return;}
				event->component = Cap( *value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_PERIOD)))  {
			if  (event->period != EvEMPTY_PERIOD)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->period)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_AMPLITUDE)))  {
			if  (event->amplitude != EvEMPTY_AMPLITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->amplitude)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_AMPLITUDE_TIME)))  {
			if  (event->amplitude_time != EvEMPTY_AMPLITUDE_TIME)
				{*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->amplitude_time)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_AMPLITUDE_VEL)))  {
			if  (event->amplitude_vel != EvEMPTY_AMPLITUDE)
				{*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->amplitude_vel)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_LP_COMPONENT)))  {
			if  (event->lp_component != EvEMPTY_COMPONENT)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > 1)  {*status = EveSTROVFL; return;}
				event->lp_component = Cap( *value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_LP_PERIOD)))  {
			if  (event->lp_period != EvEMPTY_PERIOD)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->lp_period)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_LP_AMPLITUDE)))  {
			if  (event->lp_amplitude != EvEMPTY_AMPLITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->lp_amplitude)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_BB_PERIOD)))  {
			if  (event->bb_period != EvEMPTY_PERIOD)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->bb_period)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_BB_AMPLITUDE)))  {
			if  (event->bb_amplitude != EvEMPTY_AMPLITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->bb_amplitude)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_B_SLOWNESS)))  {
			if  (event->b_slowness != EvEMPTY_SLOWNESS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->b_slowness)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_B_AZIMUTH)))  {
			if  (event->b_azimuth != EvEMPTY_AZIMUTH)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->b_azimuth)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_L_SLOWNESS)))  {
			if  (event->l_slowness != EvEMPTY_SLOWNESS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->l_slowness)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_L_AZIMUTH)))  {
			if  (event->l_azimuth != EvEMPTY_AZIMUTH)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->l_azimuth)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_THEO_AZIM)))  {
			if  (event->theo_azim != EvEMPTY_AZIMUTH)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->theo_azim)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_THEO_BACK_AZIM)))  {
			if  (event->theo_back_azim != EvEMPTY_AZIMUTH)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->theo_back_azim)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_DISTANCE_DEG)))  {
			if  (event->distance_deg != EvEMPTY_DISTANCE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->distance_deg)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_DISTANCE_KM)))  {
			if  (event->distance_km != EvEMPTY_DISTANCE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->distance_km)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_SIGNOISE)))  {
			if  (event->signoise != EvEMPTY_SIGNOISE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->signoise)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_RESIDUAL)))  {
			if  (event->residual != EvEMPTY_RESIDUAL)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->residual)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_RESID_CORR)))  {
			if  (event->resid_corr != EvEMPTY_RESID_CORR)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->resid_corr)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_QUALITY)))  {
			if  (event->quality != EvEMPTY_QUALITY)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%d",&(event->quality)) != 1)  {
					*status = EveREAD_INT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_PICK_TYPE)))  {
			if  (event->pick_type != EvcPickTypeUndefined)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvTEXT_PICKTYPE_MANUAL) == 0)  {
				event->pick_type = EvcPickTypeManual;
			} else if  (strcmp(value,EvTEXT_PICKTYPE_AUTO) == 0)  {
				event->pick_type = EvcPickTypeAuto;
			} else if  (strcmp(value,EvTEXT_PICKTYPE_THEO) == 0)  {
				event->pick_type = EvcPickTypeTheo;
			} else if  (strcmp(value,EvEMPTY_TEXT) == 0)  {
				event->pick_type = EvcOnsetUndefined;
			} else {
				fprintf( stderr, "*** unkown: %s ***\n", line );
				*status = EveILL_VALUE;
				return;
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MS)))  {
			if  (event->mag[EvcMagMs] != EvEMPTY_MAGNITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->mag[EvcMagMs])) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MB)))  {
			if  (event->mag[EvcMagMb] != EvEMPTY_MAGNITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->mag[EvcMagMb])) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ML)))  {
			if  (event->mag[EvcMagMl] != EvEMPTY_MAGNITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->mag[EvcMagMl])) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MW)))  {
			if  (event->mag[EvcMagMw] != EvEMPTY_MAGNITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->mag[EvcMagMw])) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MBB)))  {
			if  (event->mag[EvcMagMbb] != EvEMPTY_MAGNITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->mag[EvcMagMbb])) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MU)))  {
			if  (event->mag[EvcMagMu] != EvEMPTY_MAGNITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->mag[EvcMagMu])) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MEAN_MS)))  {
			if  (event->meanmag[EvcMagMs] != EvEMPTY_MAGNITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->meanmag[EvcMagMs])) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MEAN_MB)))  {
			if  (event->meanmag[EvcMagMb] != EvEMPTY_MAGNITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->meanmag[EvcMagMb])) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MEAN_ML)))  {
			if  (event->meanmag[EvcMagMl] != EvEMPTY_MAGNITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->meanmag[EvcMagMl])) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MEAN_MW)))  {
			if  (event->meanmag[EvcMagMw] != EvEMPTY_MAGNITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->meanmag[EvcMagMw])) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MEAN_MBB)))  {
			if  (event->meanmag[EvcMagMbb] != EvEMPTY_MAGNITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->meanmag[EvcMagMbb])) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MEAN_MU)))  {
			if  (event->meanmag[EvcMagMu] != EvEMPTY_MAGNITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->meanmag[EvcMagMu])) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MU_DESCR)))  {
			if  (event->mu_descr[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvUSERMAGNLTH)  {*status = EveSTROVFL; return;}
				strcpy( event->mu_descr, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_CORNERFREQ)))  {
			if  (event->cornerfreq != EvEMPTY_FREQ)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->cornerfreq)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_LOWFREQLEVEL)))  {
			if  (event->lowfreqlevel != EvEMPTY_FREQ)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->lowfreqlevel)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_REGION_TABLE)))  {
			if  (event->region_table != EvcRegionTableUndefined)
				{*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvTEXT_REGTABLE_FLINN_ENGDAHL) == 0)  {
				event->region_table = EvcRegionTableFlinnEngdahl;
			} else if  (strcmp(value,EvTEXT_REGTABLE_GERMAN_GEO) == 0)  {
				event->region_table = EvcRegionTableGermanGeo;
			} else if  (strcmp(value,EvTEXT_REGTABLE_SEISMOTEC) == 0)  {
				event->region_table = EvcRegionTableSeismoTec;
			} else if  (strcmp(value,EvEMPTY_TEXT) == 0)  {
				event->region_table = EvcRegionTableUndefined;
			} else {
				fprintf( stderr, "*** unkown: %s ***\n", line );
				*status = EveILL_VALUE;
				return;
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_REGION_ID)))  {
			if  (event->region_id != EvEMPTY_REGION_ID)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%d",&(event->region_id)) != 1)  {
					*status = EveREAD_INT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_REGION)))  {
			if  (event->region[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvREGIONLTH)  {*status = EveSTROVFL; return;}
				strcpy( event->region, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_COMMENT)))  {
			if  (event->comment.length != 0)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				EvAddComment( &(event->comment), value, status );
				if  (Severe(status))  return;
			} /*endif*/
			value = strchr( line, EvCOMMENT_END );
			while  (value == NULL)  {
				if  (fgets(line,BC_LINELTH,file) == NULL)  break;
				value = strchr( line, EvCOMMENT_END );
				if  (value != NULL)  *value = EvEOS;
				EvAddComment( &(event->comment), line, status );
				if  (Severe(status))  return;
			} /*endwhile*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_LATITUDE)))  {
			if  (event->latitude != EvEMPTY_LATITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->latitude)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_LONGITUDE)))  {
			if  (event->longitude != EvEMPTY_LONGITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->longitude)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_DEPTH)))  {
			if  (event->depth != EvEMPTY_DEPTH)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->depth)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_DEPTH_TYPE)))  {
			if  (event->depth_type != EvcDepthUndefined)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvTEXT_DEPTH_PRESET) == 0)  {
				event->depth_type = EvcDepthPreset;
			} else if  (strcmp(value,EvTEXT_DEPTH_ESTIMATED) == 0)  {
				event->depth_type = EvcDepthEstimated;
			} else if  (strcmp(value,EvTEXT_DEPTH_FREE) == 0)  {
				event->depth_type = EvcDepthFree;
			} else if  (strcmp(value,EvTEXT_DEPTH_POOR) == 0)  {
				event->depth_type = EvcDepthPoor;
			} else if  (strcmp(value,EvTEXT_DEPTH_LESSWELL) == 0)  {
				event->depth_type = EvcDepthLessWell;
			} else if  (strcmp(value,EvTEXT_DEPTH_RELIABLE) == 0)  {
				event->depth_type = EvcDepthReliable;
			} else if  (strcmp(value,EvTEXT_DEPTH_EXTERNAL) == 0)  {
				event->depth_type = EvcDepthExternal;
			} else if  (strcmp(value,EvEMPTY_TEXT) == 0)  {
				event->depth_type = EvcDepthUndefined;
			} else {
				fprintf( stderr, "*** unkown: %s ***\n", line );
				*status = EveILL_VALUE;
				return;
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ORIGIN_TIME)))  {
			if  (event->origin_time[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvTIMELTH)  {*status = EveSTROVFL; return;}
				strcpy( event->origin_time, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_LOC_METHOD)))  {
			if  (event->loc_method != EvcLocMethUndefined)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvTEXT_LMETHOD_CORRBEAM) == 0)  {
				event->loc_method = EvcLocMethCorrBeam;
			} else if  (strcmp(value,EvTEXT_LMETHOD_UNCORRBEAM) == 0)  {
				event->loc_method = EvcLocMethUncorrBeam;
			} else if  (strcmp(value,EvTEXT_LMETHOD_RESIDCORR) == 0)  {
				event->loc_method = EvcLocMethResidCorr;
			} else if  (strcmp(value,EvTEXT_LMETHOD_HYPO) == 0)  {
				event->loc_method = EvcLocMethHypo;
			} else if  (strcmp(value,EvTEXT_LMETHOD_LOCSAT) == 0)  {
				event->loc_method = EvcLocMethLocsat;
			} else if  (strcmp(value,EvTEXT_LMETHOD_HYPOCENTER) == 0)  {
				event->loc_method = EvcLocMethHypocenter;
			} else if  (strcmp(value,EvTEXT_LMETHOD_HYPO71) == 0)  {
				event->loc_method = EvcLocMethHypo71;
			} else if  (strcmp(value,EvTEXT_LMETHOD_HYPOSAT) == 0)  {
				event->loc_method = EvcLocMethHyposat;
			} else if  (strcmp(value,EvTEXT_LMETHOD_RELTRAV) == 0)  {
				event->loc_method = EvcLocMethRelTrav;
			} else if  (strcmp(value,EvTEXT_LMETHOD_EXTERNAL) == 0)  {
				event->loc_method = EvcLocMethExternal;
			} else if  (strcmp(value,EvEMPTY_TEXT) == 0)  {
				event->loc_method = EvcLocMethUndefined;
			} else {
				fprintf( stderr, "*** unkown: %s ***\n", line );
				*status = EveILL_VALUE;
				return;
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_LOC_QUALITY)))  {
			if  (event->loc_quality != EvcLocQualUndefined)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvTEXT_LOCQ_TOOWEAK) == 0)  {
				event->loc_quality = EvcLocQualTooWeak;
			} else if  (strcmp(value,EvTEXT_LOCQ_INCOHERENT) == 0)  {
				event->loc_quality = EvcLocQualIncoherent;
			} else if  (strcmp(value,EvTEXT_LOCQ_NOBEARING) == 0)  {
				event->loc_quality = EvcLocQualNoBearing;
			} else if  (strcmp(value,EvTEXT_LOCQ_REGION) == 0)  {
				event->loc_quality = EvcLocQualRegion;
			} else if  (strcmp(value,EvTEXT_LOCQ_RELIABLE) == 0)  {
				event->loc_quality = EvcLocQualReliable;
			} else if  (strcmp(value,EvEMPTY_TEXT) == 0)  {
				event->loc_quality = EvcLocQualUndefined;
			} else {
				fprintf( stderr, "*** unkown: %s ***\n", line );
				*status = EveILL_VALUE;
				return;
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_LOC_VELMOD)))  {
			if  (event->velmod[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvVELMODLTH)  {*status = EveSTROVFL; return;}
				strcpy( event->velmod, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_LOC_ADDPAR)))  {
			if  (event->loc_addpar[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvLOCADDPARLTH)  {*status = EveSTROVFL; return;}
				strcpy( event->loc_addpar, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_WEIGHT)))  {
			if  (event->weight != EvEMPTY_WEIGHT)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%d",&(event->weight)) != 1)  {
					*status = EveREAD_INT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_FILTER)))  {
			if  (event->filter[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvFILTERLTH)  {*status = EveSTROVFL; return;}
				strcpy( event->filter, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_EVID)))  {
			if  (event->evid != EvEMPTY_EVID)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%ld",&(event->evid)) != 1)  {
					*status = EveREAD_INT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_REF_LATITUDE)))  {
			if  (event->ref_latitude != EvEMPTY_LATITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->ref_latitude)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_REF_LONGITUDE)))  {
			if  (event->ref_longitude != EvEMPTY_LONGITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->ref_longitude)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_REF_NAME)))  {
			if  (event->ref_name[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvSTATIONLTH)  {*status = EveSTROVFL; return;}
				strcpy( event->ref_name, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ANALYST)))  {
			if  (event->analyst[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvANALYSTLTH)  {*status = EveSTROVFL; return;}
				strcpy( event->analyst, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_SOURCE)))  {
			if  (event->source[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvSOURCELTH)  {*status = EveSTROVFL; return;}
				strcpy( event->source, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_STATIONS_USED)))  {
			if  (event->stations_used != EvEMPTY_STATIONS_USED)
				{*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%d",&(event->stations_used)) != 1)  {
					*status = EveREAD_INT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_EVENT_TYPE)))  {
			if  (event->event_type != EvcEventTypeUndefined)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvTEXT_EVTYPE_TELE_QUAKE) == 0)  {
				event->event_type = EvcEventTypeTeleQuake;
			} else if  (strcmp(value,EvTEXT_EVTYPE_NUCLEAR) == 0)  {
				event->event_type = EvcEventTypeNuclear;
			} else if  (strcmp(value,EvTEXT_EVTYPE_REGIO_QUAKE) == 0)  {
				event->event_type = EvcEventTypeRegioQuake;
			} else if  (strcmp(value,EvTEXT_EVTYPE_LOCAL_QUAKE) == 0)  {
				event->event_type = EvcEventTypeLocalQuake;
			} else if  (strcmp(value,EvTEXT_EVTYPE_BLAST) == 0)  {
				event->event_type = EvcEventTypeBlast;
			} else if  (strcmp(value,EvTEXT_EVTYPE_MINING) == 0)  {
				event->event_type = EvcEventTypeMining;
			} else if  (strcmp(value,EvEMPTY_TEXT) == 0)  {
				event->event_type = EvcEventTypeUndefined;
			} else {
				fprintf( stderr, "*** unkown: %s ***\n", line );
				*status = EveILL_VALUE;
				return;
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_AP_SOURCE)))  {
			if  (event->ap_source != EvcApSourceUndefined)
				{*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvTEXT_APSRC_DIRECT) == 0)  {
				event->ap_source = EvcApSourceDirect;
			} else if  (strcmp(value,EvTEXT_APSRC_BEAM) == 0)  {
				event->ap_source = EvcApSourceBeam;
			} else if  (strcmp(value,EvTEXT_APSRC_ALIGN) == 0)  {
				event->ap_source = EvcApSourceAlign;
			} else if  (strcmp(value,EvEMPTY_TEXT) == 0)  {
				event->ap_source = EvcApSourceUndefined;
			} else {
				fprintf( stderr, "*** unkown: %s ***\n", line );
				*status = EveILL_VALUE;
				return;
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ERR_LAT)))  {
			if  (event->err.lat_km != EvEMPTY_ERR_FLOAT)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->err.lat_km)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ERR_LON)))  {
			if  (event->err.lon_km != EvEMPTY_ERR_FLOAT)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->err.lon_km)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ERR_DEPTH)))  {
			if  (event->err.dep != EvEMPTY_ERR_FLOAT)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->err.dep)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ERR_ORIGIN)))  {
			if  (event->err.orig != EvEMPTY_ERR_FLOAT)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->err.orig)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ERR_SMAJOR)))  {
			if  (event->err.smajor != EvEMPTY_ERR_FLOAT)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->err.smajor)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ERR_SMINOR)))  {
			if  (event->err.sminor != EvEMPTY_ERR_FLOAT)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->err.sminor)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_ERR_MAJSTRIKE)))  {
			if  (event->err.majstrike != EvEMPTY_ERR_FLOAT)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->err.majstrike)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_AZIM_MAX_GAP)))  {
			if  (event->err.azim_max_gap != EvEMPTY_ERR_FLOAT)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->err.azim_max_gap)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_RESID_RMS)))  {
			if  (event->err.resid_rms != EvEMPTY_ERR_FLOAT)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->err.resid_rms)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_PHASE_FLAGS)))  {
			if  (event->phase_flags[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvFLAGLTH)  {*status = EveSTROVFL; return;}
				strcpy( event->phase_flags, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MOMTEN)))  {
			if  (event->momten[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvMOMTENLTH)  {*status = EveSTROVFL; return;}
				strcpy( event->momten, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_MOMTEN_DESCR)))  {
			if  (event->momten_descr[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvMOMTENDESCRLTH)  {*status = EveSTROVFL; return;}
				strcpy( event->momten_descr, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_M0)))  {
			if  (event->m0 != EvEMPTY_MAGNITUDE)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)
				if  (sscanf(value,"%f",&(event->m0)) != 1)  {
					*status = EveREAD_FLOAT;
					fprintf( stderr, "*** line: %s ***\n", line );
					return;
				} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_FPS_ANGLES)))  {
			if  (event->fps_angles[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvFPS_ANGLES)  {*status = EveSTROVFL; return;}
				strcpy( event->fps_angles, value );
			} /*endif*/
		} else if  (NULL != (value = EvhFindValue(line,EvITEM_FPS_DESCR)))  {
			if  (event->fps_descr[0] != EvEOS)  {*status = EveINFO_TWICE; return;}
			if  (*value < ' ')  {*status = EveILL_VALUE; return;}
			if  (strcmp(value,EvEMPTY_TEXT) != 0)  {
				if  (strlen(value) > EvFPS_DESCRLTH)  {*status = EveSTROVFL; return;}
				strcpy( event->fps_descr, value );
			} /*endif*/
		} else if  (strcmp(line,EvITEM_END_OF_EVENT) == 0)  {
			return;
		} else {
			fprintf( stderr, "*** unkown: %s ***\n", line );
			*status = EveUNKNOWN_LINE;
			return;
		} /*endif*/
	} /*endfor*/

} /* end of EvGetEvent */



/*-------------------------------------------------------------------*/



void EvReformatEventfile( char infile[], char outfile[],
	EvGetRoutineT *get_routine, EvPutRoutineT *put_routine,
	EvStatusT *status )

/* Reformats event file using specified event input and output
 * routines.
 *
 * parameters of routine
 * char           infile[];       input; name of input file
 * char           outfile[];      input; name of output file
 * EvGetRoutineT  *get_routine;   input; input routine
 * EvPutRoutineT  *put_routine;   input; output routine
 * EvStatusT      *status;        output; return status
 */
{
	/* local variables */
	FILE     *inp, *out;       /* pointers to input and output files */
	EvEventT event;            /* event info */
	BOOLEAN  eof;              /* end of file found */

	/* executable code */

	inp = (strcmp(infile,"TT") == 0) ? stdin : NULL;
	if  (inp != stdin)  {
		inp = fopen( infile, "r" );
		if  (inp == NULL)  {
			*status = EveOPEN_READ;
			fprintf( stderr, "*** couldn't open input file %s ***\n", infile );
			return;
		} /*endif*/
	} /*endif*/
	out = (strcmp(outfile,"TT") == 0) ? stdout : NULL;
	if  (out != stdout)  {
		out = fopen( outfile, "w" );
		if  (out == NULL)  {
			*status = EveOPEN_WRITE;
			fclose( inp );
			fprintf( stderr, "*** couldn't open output file %s ***\n", outfile );
			return;
		} /*endif*/
	} /*endif*/

	FOREVER  {
		(*get_routine)( inp, &event, &eof, status );
		if  (eof)  break;
		if  (Severe(status))  return;
		(*put_routine)( out, &event, status );
		if  (Severe(status))  return;
	} /*endfor*/
	/* tell that this is the end */
	(*put_routine)( out, NULL, status );

	if  (inp != stdin)
		fclose( inp );
	if  (out != stdout)
		fclose( out );

	return;

} /* end of EvReformatEventfile */



/*-------------------------------------------------------------------*/



void EvAddComment( EvCommentT *comment, char *new_text,
	EvStatusT *status )

/* Appends new text to comment of event
 *
 * parameters of routine
 * EvCommentT *comment;       modify; comment to be modified
 * char       new_text[];     input; text to be added
 * EvStatusT  *status,        output; return status
 */
{
	/* local variables */
	unsigned newlen;     /* new string length */
	unsigned oldlen;     /* old string length */
	char     *ptr;       /* temporary pointer */

	/* executable code */

	newlen = (unsigned)strlen( new_text );

	if  (comment->length == 0)  {
		comment->text = (char *)malloc( newlen+1 );
		if  (comment->text == NULL)  {
			*status = EveMEM_ALLOC;
			return;
		} /*endif*/
		comment->length = newlen+1;
		strcpy( comment->text, new_text );
		return;
	} /*endif*/

	if  (comment->text == NULL)  {
		*status = EveCOMMENT_PTR;
		return;
	} /*endif*/

	oldlen = (unsigned)strlen( comment->text );
	if  (oldlen+newlen+1 <= comment->length)  {
		strcat( comment->text, new_text );
		return;
	} /*endif*/

	ptr = (char *)malloc( newlen+oldlen+1 );
	if  (ptr == NULL)  {
		*status = EveMEM_ALLOC;
		return;
	} /*endif*/
	strcpy( ptr, comment->text );
	strcat( ptr, new_text );
	free( comment->text );
	comment->text = ptr;
	comment->length = newlen+oldlen+1;

} /* end of EvAddComment */



/*-------------------------------------------------------------------*/



void EvDeleteComment( EvCommentT *comment, EvStatusT *status )

/* Deletes comment
 *
 * parameters of routine
 * EvCommentT *comment;       modify; comment to be deleted
 * EvStatusT  *status;        output; return status
 */
{
	/* executable code */

	if  (comment->length == 0)  return;
	if  (comment->text == NULL)  {
		*status = EveCOMMENT_PTR;
		return;
	} /*endif*/
	free( comment->text );
	comment->length = 0;
	comment->text = NULL;

} /* end of EvDeleteComment */



/*-------------------------------------------------------------------*/



void EvShowEmptyValues( BOOLEAN show )

/* Sets display flag for empty values to TRUE or FALSE
 *
 * parameters of routine
 * BOOLEAN    show;         input; display flag
 */
{
	/* executable code */

	evv_show_empty = show;

} /* end of EvShowEmptyValues */



/*-------------------------------------------------------------------*/



void EvReadLineInteractive( FILE *inp, int length, char text[] )

/* read text from standard input
 *
 * parameters of routine
 * FILE       *inp;         input; input channel (usually stdin)
 * int        length;       input; maximum length of output string
 * char       text[];       output; text read in
 */
{
	/* local variables */

	/* executable code */

	/*
	 * should be:   fgets( text, length, inp );
	 * but this does not work on ATARI
	 */

	while  (length-- > 0)  {
		*text = getc( inp );
		if  (*text++ == '\n')  break;
	} /*endwhile*/
	*text = EvEOS;
	if  (*(text-1) == '\n')  *(text-1) = EvEOS;

} /* end of EvReadLineInteractive */



/*-------------------------------------------------------------------*/



static void EvhFixedLengthString( char str[], int length, FILE *fp )

/* Prints fixed length string to output channel
 *
 * parameters of routine
 * char       str[];       input; output string
 * int        length;      input; minimum length, padded with blanks and ':'
 * FILE       *fp;         input; output file pointer
 */
{
	/* local variables */
	unsigned i;       /* counter */

	/* executable code */

	fprintf( fp, "%s", str );
	for  (i=(unsigned)strlen(str); i<(length-2); i++)
		fprintf( fp, " " );
	fprintf( fp, ": " );

} /* end of EvhFixedLengthString */



/*-------------------------------------------------------------------*/



static char *EvhFindValue( char line[], char item[] )

/* Trues to find value of specified item.  If found it returns pointer
 * to start of value string, otherwise it returns NULL
 *
 * parameters of routine
 * char       line[];       input; current line
 * char       item[];       input; name of item
 *                          returns pointer to info or NULL
 */
{
	/* local variables */
	unsigned slen;       /* string length */

	/* executable code */

	/* get length of item name */
	slen = (unsigned)strlen( item );

	/* skip beginning blanks of line */
	while  (*line == ' ')  line++;

	/* compare item name */
	if  (strncmp(line,item,slen) != 0)  return NULL;

	/* goto to end of item name and skip blanks and colons */
	line += slen;
	if  (*line != ' ' && *line != ':')  return NULL;
	while  (*line == ' ' || *line == ':')  line++;
	return line;

} /* end of EvhFindValue */



/*-------------------------------------------------------------------*/



static BOOLEAN EvhKnownPhase( char phase[] )

/* Checks whether "phase" is a legal phase name
 *
 * parameters of routine
 * char       phase[];      input; phase name to be checked
 */
{
	/* local variables */
	char     lp[EvPHASELTH+1];    /* local phase name */
	int      slen;                /* string length */

	/* executable code */

	if  (strlen(phase) > EvPHASELTH)  {
		printf( "*** phase name overflow %s ***\n", phase );
		return FALSE;
	} /*endif*/

	if  (*phase != '(')  {
		strcpy( lp, phase );
	} else {
		strcpy( lp, phase+1 );
		slen = (int)strlen( lp ) - 1;
		if  (slen >= 0 && lp[slen] == ')')  lp[slen] = EvEOS;
	} /*endif*/

	if  (strcmp(lp,"P") == 0)  return TRUE;
	if  (strcmp(lp,"pP") == 0)  return TRUE;
	if  (strcmp(lp,"sP") == 0)  return TRUE;
	if  (strcmp(lp,"PP") == 0)  return TRUE;
	if  (strcmp(lp,"pPP") == 0)  return TRUE;
	if  (strcmp(lp,"sPP") == 0)  return TRUE;
	if  (strcmp(lp,"PPP") == 0)  return TRUE;
	if  (strcmp(lp,"pPKP") == 0)  return TRUE;
	if  (strcmp(lp,"PKP") == 0)  return TRUE;
	if  (strcmp(lp,"PKKP") == 0)  return TRUE;
	if  (strcmp(lp,"PKP2") == 0)  return TRUE;
	if  (strcmp(lp,"Pn") == 0)  return TRUE;
	if  (strcmp(lp,"Pg") == 0)  return TRUE;
	if  (strcmp(lp,"Pdif") == 0)  return TRUE;
	if  (strcmp(lp,"Pdiff") == 0)  return TRUE;
	if  (strcmp(lp,"S") == 0)  return TRUE;
	if  (strcmp(lp,"pS") == 0)  return TRUE;
	if  (strcmp(lp,"sS") == 0)  return TRUE;
	if  (strcmp(lp,"SKS") == 0)  return TRUE;
	if  (strcmp(lp,"SKKS") == 0)  return TRUE;
	if  (strcmp(lp,"Sn") == 0)  return TRUE;
	if  (strcmp(lp,"Sg") == 0)  return TRUE;
	if  (strcmp(lp,"SS") == 0)  return TRUE;
	if  (strcmp(lp,"pSS") == 0)  return TRUE;
	if  (strcmp(lp,"sSS") == 0)  return TRUE;
	if  (strcmp(lp,"SSS") == 0)  return TRUE;
	if  (strcmp(lp,"PKS") == 0)  return TRUE;
	if  (strcmp(lp,"SKP") == 0)  return TRUE;
	if  (strcmp(lp,"Sdif") == 0)  return TRUE;
	if  (strcmp(lp,"Sdiff") == 0)  return TRUE;
	return FALSE;

} /* end of EvhKnownPhase */



/*-------------------------------------------------------------------*/



static void EvhPromptReal( EvFloatT *value, FILE *inp, char prompt[],
	EvFloatT empty_val, EvFloatT limit_lo, EvFloatT limit_hi,
	char format[] )

/* Reads real valued parameter interactively
 *
 * parameters of routine
 * EvFloatT   *value;       modify; value to be read in
 * FILE       *inp;         input; input channel (usually stdin)
 * char       prompt[];     input; prompt text
 * EvFloatT   empty_val;    input; empty value of parameter
 * EvFloatT   limit_lo;     input; lower limit of value
 * EvFloatT   limit_hi;     input; upper limit of value
 * char       format[];     input; format string for default value
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];       /* scratch string */
	char     def[BC_LINELTH+1];       /* default value */
	unsigned slen;                    /* string length */
	BOOLEAN  ok;                      /* loop flag */

	/* executable code */

	do  {
		if  (*value == empty_val)  {
			strcpy( def, EvEMPTY_TEXT );
		} else {
			sprintf( def, format, *value );
		} /*endif*/
		fprintf( stdout, "   %s [<CR>=%s]: ", prompt, def );
		*str = EvEOS;
		EvReadLineInteractive( inp, BC_LINELTH, str );
		slen = (unsigned)strlen( str );
		if  (slen > 0 && str[slen-1] == '\n')  str[slen-1] = EvEOS;
		ok = TRUE;
		if  (*str == EvEOS)  {
			/* leave it as it is */
		} else if  (strcmp(str,EvEMPTY_INPUT) == 0)  {
			*value = empty_val;
		} else if  (sscanf(str,"%f",value) != 1)  {
			printf( "*** illegal input, repeat ***\n" );
			*value = empty_val;
			ok = FALSE;
		} else {
			if  (*value < limit_lo || *value > limit_hi)  {
				printf( "*** illegal range, repeat input ***\n" );
				ok = FALSE;
			} /*endif*/
		} /*endif*/
	}  while  (!ok);

} /* end of EvhPromptReal */



/*-------------------------------------------------------------------*/



static void EvhPromptInt( EvQualityT *value, FILE *inp, char prompt[],
	EvQualityT empty_val, EvQualityT limit_lo, EvQualityT limit_hi,
	char format[] )

/* Reads integer valued parameter interactively
 *
 * parameters of routine
 * EvQualityT *value;       modify; value to be read in
 * FILE       *inp;         input; input channel (usually stdin)
 * char       prompt[];     input; prompt text
 * EvQualityT empty_val;    input; empty value of parameter
 * EvQualityT limit_lo;     input; lower limit of value
 * EvQualityT limit_hi;     input; upper limit of value
 * char       format[];     input; format string for default value
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];       /* scratch string */
	char     def[BC_LINELTH+1];       /* default value */
	unsigned slen;                    /* string length */
	BOOLEAN  ok;                      /* loop flag */

	/* executable code */

	do  {
		if  (*value == empty_val)  {
			strcpy( def, EvEMPTY_TEXT );
		} else {
			sprintf( def, format, *value );
		} /*endif*/
		fprintf( stdout, "   %s [<CR>=%s]: ", prompt, def );
		*str = EvEOS;
		EvReadLineInteractive( inp, BC_LINELTH, str );
		slen = (unsigned)strlen( str );
		if  (slen > 0 && str[slen-1] == '\n')  str[slen-1] = EvEOS;
		ok = TRUE;
		if  (*str == EvEOS)  {
			/* leave it as it is */
		} else if  (strcmp(str,EvEMPTY_INPUT) == 0)  {
			*value = empty_val;
		} else if  (sscanf(str,"%d",value) != 1)  {
			printf( "*** illegal input, repeat ***\n" );
			*value = empty_val;
			ok = FALSE;
		} else {
			if  (*value < limit_lo || *value > limit_hi)  {
				printf( "*** illegal range, repeat input ***\n" );
				ok = FALSE;
			} /*endif*/
		} /*endif*/
	}  while  (!ok);

} /* end of EvhPromptInt */



/*-------------------------------------------------------------------*/



static void EvhPromptComponent( char *comp, FILE *inp, char prompt[] )

/* Prompts for input of component
 *
 * parameters of routine
 * char       *comp;       modify;    component to be read
 * FILE       *inp;        input;     input channel (usually stdin)
 * char       prompt[];    input;     prompt text
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];     /* scratch string */
	char     def[BC_LINELTH+1];     /* default value */
	unsigned slen;                  /* string length */
	BOOLEAN  ok;                    /* input ok */

	/* executable code */

	do  {
		if  (*comp == EvEMPTY_COMPONENT)  {
			strcpy( def, EvEMPTY_TEXT );
		} else {
			sprintf( def, "%c", *comp );
		} /*endif*/
		fprintf( stdout, "   %s [Z,N,E,R,T,<CR>=%s]: ", prompt, def );
		*str = EvEOS;
		EvReadLineInteractive( inp, BC_LINELTH, str );
		slen = (unsigned)strlen( str );
		if  (slen > 0 && str[slen-1] == '\n')  str[--slen] = EvEOS;
		*str = Cap( *str );
		ok = TRUE;
		if  (*str == EvEOS)  {
			/* leave it as it is */
		} else if  (strcmp(str,EvEMPTY_INPUT) == 0)  {
			*comp = EvEMPTY_COMPONENT;
		} else if  (slen > 1)  {
			printf( "*** only one character, repeat input ***\n" );
			ok = FALSE;
		} else if  (*str != 'Z' && *str != 'N' && *str != 'E'
			&& *str != 'R' && *str != 'T')  {
			printf( "*** illegal input, repeat ***\n" );
			ok = FALSE;
		} else {
			*comp = *str;
		} /*endif*/
	}  while  (!ok);

} /* end of EvhPromptComponent */



/*-------------------------------------------------------------------*/



static void EvhPromptString( char text[], unsigned maxlth,
	FILE *inp, char prompt[] )

/* Prompts for string parameter
 *
 * parameters of routine
 * char       text[];     modify; string to be read in
 * unsigned   maxlth;     inputl maximum length of output string
 * FILE       *inp;       input; input channel (usually stdin)
 * char       prompt[];   input; prompt text
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];      /* scratch string */
	char     def[BC_LINELTH+1];      /* default value */
	unsigned slen;                   /* string length */
	BOOLEAN  ok;                     /* input ok */

	/* executable code */

	do  {
		if  (*text == EvEOS)  {
			strcpy( def, EvEMPTY_TEXT );
		} else {
			strcpy( def, text );
		} /*endif*/
		fprintf( stdout, "   %s [<CR>=%s]: ", prompt, def );
		*str = EvEOS;
		EvReadLineInteractive( inp, BC_LINELTH, str );
		slen = (unsigned)strlen( str );
		if  (slen > 0 && str[slen-1] == '\n')  str[slen-1] = EvEOS;
		ok = TRUE;
		if  (*str == EvEOS)  {
			/* leave it as it is */
		} else if  (strcmp(str,EvEMPTY_INPUT) == 0)  {
			text[0] = EvEOS;
		} else if  (slen > maxlth)  {
			fprintf( stdout, "*** name too long, repeat input ***\n" );
			ok = FALSE;
		} else {
			strcpy( text, str );
		} /*endif*/
	}  while  (!ok);

} /* end of EvhPromptString */



/*-------------------------------------------------------------------*/



static void EvhPromptOnsetTime( char onset[], EvOnsetAccT *acc, FILE *inp )

/* prompts for onset time
 *
 * parameters of routine
 * char       onset[];      modify; onset time
 * EvOnsetAccT *acc;        output; accuracy of onset
 * FILE       *inp;         input; input channel (usually stdin)
 */
{
	/* local variables */
	static char date[EvTIMELTH+1]="";    /* date string */
	static char time[EvTIMELTH+1]="";    /* time string */
	char     str[BC_LINELTH+1];          /* scratch string */
	int      slen;                       /* string length */
	STATUS   locstat;                    /* local status */
	BOOLEAN  ok;                         /* input ok */

	/* executable code */

	do  {

		/* get date */
		do  {
			if  (*date == EvEOS)  {
				fprintf( stdout, "   date: " );
			} else {
				fprintf( stdout, "   date [default %s]: ", date );
			} /*endif*/
			*str = EvEOS;
			EvReadLineInteractive( inp, BC_LINELTH, str );
			slen = (int)strlen( str ) - 1;
			if  (slen >= 0 && str[slen] == '\n')  str[slen] = EvEOS;
			if  (slen > EvTIMELTH/2)
				printf( "*** date string too long, repeat input ***\n" );
			if  (*str != EvEOS)  strcpy( date, str );
		}  while  (slen > EvTIMELTH/2);

		/* get time */
		do  {
			if  (*time == EvEOS)  {
				fprintf( stdout, "   time: " );
			} else {
				fprintf( stdout, "   time [default %s]: ", time );
			} /*endif*/
			*str = EvEOS;
			EvReadLineInteractive( inp, BC_LINELTH, str );
			slen = (int)strlen( str ) - 1;
			if  (slen >= 0 && str[slen] == '\n')  str[slen] = EvEOS;
			if  (slen > EvTIMELTH/2)
				printf( "*** time string too long, repeat input ***\n" );
			ok = TRUE;
			if  (*str != EvEOS)  {
				EvhCheckTimeInput( str, &ok, acc );
				if  (ok)  {
					strcpy( time, str );
				} else {
					printf( "*** illegal time: %s, repeat input ***\n", str );
				} /*endif*/
			} /*endif*/
		}  while  (slen > EvTIMELTH/2 || !ok);

		/* append date & time */
		strcpy( onset, date );
		if  (strchr(date,'-'))  {
			strcat( onset, "_" );
		} else {
			strcat( onset, "," );
		} /*endif*/
		strcat( onset, time );

		/* check it */
		locstat = BC_NOERROR;
		tc_tadd( onset, 0.0, onset, &locstat );
		if  (locstat != BC_NOERROR)
			fprintf( stdout, "*** input error, repeat date & time ***\n" );

	}  while  (locstat != BC_NOERROR);

} /* end of EvhPromptOnsetTime */



/*-------------------------------------------------------------------*/



static void EvhPromptOnsetType( EvOnsetT *type, FILE *inp )

/* Prompts for onset type
 *
 * parameters of routine
 * EvOnsetT   *type;       modify; onset type
 * FILE       *inp;        input; input channel (usually stdin)
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];     /* scratch string */
	char     def[BC_LINELTH+1];     /* default value */
	unsigned slen;                  /* string length */
	BOOLEAN  ok;                    /* input ok */

	/* executable code */

	do  {
		switch  (*type)  {
		case EvcOnsetEmergent:
			strcpy( def, EvTEXT_ONSET_EMERGENT );
			break;
		case EvcOnsetImpulsive:
			strcpy( def, EvTEXT_ONSET_IMPULSIVE );
			break;
		default:
			strcpy( def, EvEMPTY_TEXT );
			break;
		} /*endswitch*/
		fprintf( stdout, "   %s [e,i,<CR>=%s]: ", EvITEM_ONSET_TYPE, def );
		*str = EvEOS;
		EvReadLineInteractive( inp, BC_LINELTH, str );
		slen = (unsigned)strlen( str );
		if  (slen > 0 && str[slen-1] == '\n')  str[--slen] = EvEOS;
		*str = Cap( *str );
		ok = TRUE;
		if  (strcmp(str,EvEMPTY_INPUT) == 0)  {
			*type = EvcOnsetUndefined;
		} else if  (slen > 1)  {
			printf( "*** only one character, repeat input ***\n" );
			ok = FALSE;
		} else if  (*str == 'E')  {
			*type = EvcOnsetEmergent;
		} else if  (*str == 'I')  {
			*type = EvcOnsetImpulsive;
		} else if  (*str == EvEOS)  {
			/* leave it as it is */
		} else {
			fprintf( stdout, "*** illegal input, repeat ***\n" );
			ok = FALSE;
		} /*endif*/
	}  while  (!ok);

} /* end of EvhPromptOnsetType */



/*-------------------------------------------------------------------*/


static void EvhPromptSign( EvSignT *sign, FILE *inp )

/* Prompts for sign of onset
 *
 * parameters of routine
 * EvSignT    *sign;        modify; sign of onset
 * FILE       *inp;         input; input channel (usually stdin)
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];     /* scratch string */
	char     def[BC_LINELTH+1];     /* default value */
	unsigned slen;                  /* string length */
	BOOLEAN  ok;                    /* input ok */

	/* executable code */
	do  {
		switch  (*sign)  {
		case EvcSignPositive:
			strcpy( def, EvTEXT_SIGN_POSITIVE );
			break;
		case EvcSignNegative:
			strcpy( def, EvTEXT_SIGN_NEGATIVE );
			break;
		default:
			strcpy( def, EvEMPTY_TEXT );
			break;
		} /*endswitch*/
		fprintf( stdout, "   %s [+,-,<CR>=%s]: ", EvITEM_SIGN, def );
		*str = EvEOS;
		EvReadLineInteractive( inp, BC_LINELTH, str );
		slen = (unsigned)strlen( str );
		if  (slen > 0 && str[slen-1] == '\n')  str[--slen] = EvEOS;
		*str = Cap( *str );
		ok = TRUE;
		if  (strcmp(str,EvEMPTY_INPUT) == 0)  {
			*sign = EvcSignUndefined;
		} else if  (slen > 1)  {
			printf( "*** only one character, repeat input ***\n" );
			ok = FALSE;
		} else if  (*str == '+' || *str == 'C')  {
			*sign = EvcSignPositive;
		} else if  (*str == '-' || *str == 'D')  {
			*sign = EvcSignNegative;
		} else if  (*str == EvEOS)  {
			/* leave it as it is */
		} else {
			fprintf( stdout, "*** illegal input, repeat ***\n" );
			ok = FALSE;
		} /*endif*/
	}  while  (!ok);

} /* end of EvhPromptSign */



/*-------------------------------------------------------------------*/



static void EvhPromptComment( EvCommentT *comment, FILE *inp,
	EvStatusT *status )

/* Prompts for comment
 *
 * parameters of routine
 * EvCommentT *comment;       modify; comment to be read in
 * FILE       *inp;           input; input channel (usually stdin)
 * EvStatusT  *status;        output; return status
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];     /* scratch string */
	unsigned slen;                  /* string length */
	BOOLEAN  ok;                    /* input ok */

	/* executable code */

	do  {
		fprintf( stdout, "   %s [<CR>=not specified]: ", EvITEM_COMMENT );
		*str = EvEOS;
		EvReadLineInteractive( inp, BC_LINELTH, str );
		slen = (unsigned)strlen( str );
		if  (slen > 0 && str[slen-1] == '\n')  str[slen-1] = EvEOS;
		ok = TRUE;
		if  (*str == EvEOS)  {
		} else if  (slen > BC_LINELTH)  {
			fprintf( stdout, "*** text too long, repeat input ***\n" );
			ok = FALSE;
		} else {
			EvAddComment( comment, str, status );
			if  (Severe(status))  return;
		} /*endif*/
	}  while  (!ok);

} /* end of EvhPromptComment */



/*-------------------------------------------------------------------*/



static void EvhCheckTimeInput( char time[], BOOLEAN *ok, EvOnsetAccT *acc )

/* Checks time input and reformats it if possible
 *
 * parameters of routine
 * char       time[];     modify; time string to be checked and reformatted
 * BOOLEAN    *ok;        output; input is valid
 * EvOnsetAccT *acc;      output; accuracy of time
 */
{
	/* local variables */
	unsigned colons;        /* colon counter */
	unsigned dots;          /* dot counter */
	unsigned blanks;        /* blank counter */
	unsigned slen;          /* string length */
	unsigned hour, min, sec, ms;   /* time values */
	char     str[BC_SHORTSTRLTH+1];/* scratch string */

	/* executable code */

	*ok = FALSE;
	*acc = EvcOnsetAccUndefined;

	/* count terminators */
	colons = dots = blanks = slen = 0;
	while  (time[slen] != EvEOS)  {
		if  (time[slen] == ':')  colons++;
		if  (time[slen] == '.')  dots++;
		if  (time[slen] == ' ')  blanks++;
		if  (time[slen] == ',')  {time[slen] = ' '; blanks++;}
		slen++;
	} /*endwhile*/

	if  (dots > 1)  return;
	if  (colons > 0 && blanks > 0)  return;
	if  (dots > 0 && blanks > 0)  return;
	if  (dots > 0 && colons == 0)  return;
	if  (colons == 0 && blanks == 0 && slen != 6 && slen != 7) return;

	if  (colons == 2 && dots == 1)  {
		if  (sscanf(time,"%d:%d:%d.%d",&hour,&min,&sec,&ms) != 4)  return;
		ms *= 100;
		*acc = EvcOnsetAccMillisecond;
	} else if  (colons == 2 && dots == 0)  {
		if  (sscanf(time,"%d:%d:%d",&hour,&min,&sec) != 3)  return;
		ms = 0;
		*acc = EvcOnsetAccSecond;
	} else if  (colons == 1 && dots == 0)  {
		if  (sscanf(time,"%d:%d",&hour,&min) != 2)  return;
		sec = ms = 0;
		*acc = EvcOnsetAccSecond;
	} else if  (blanks == 3)  {
		if  (sscanf(time,"%d %d %d %d",&hour,&min,&sec,&ms) != 4)  return;
		ms *= 100;
		*acc = EvcOnsetAccMillisecond;
	} else if  (blanks == 2)  {
		if  (sscanf(time,"%d %d %d",&hour,&min,&sec) != 3)  return;
		ms = 0;
		*acc = EvcOnsetAccSecond;
	} else if  (blanks == 1)  {
		if  (sscanf(time,"%d %d",&hour,&min) != 2)  return;
		sec = ms = 0;
		*acc = EvcOnsetAccSecond;
	} else if  (blanks == 0 && colons == 0 && slen == 6)  {
		str[0] = time[0]; str[1] = time[1]; str[2] = EvEOS;
		if  (sscanf(str,"%d",&hour) != 1)  return;
		str[0] = time[2]; str[1] = time[3]; str[2] = EvEOS;
		if  (sscanf(str,"%d",&min) != 1)  return;
		str[0] = time[4]; str[1] = time[5]; str[2] = EvEOS;
		if  (sscanf(str,"%d",&sec) != 1)  return;
		ms = 0;
		*acc = EvcOnsetAccSecond;
	} else if  (blanks == 0 && colons == 0 && slen == 7)  {
		str[0] = time[0]; str[1] = time[1]; str[2] = EvEOS;
		if  (sscanf(str,"%d",&hour) != 1)  return;
		str[0] = time[2]; str[1] = time[3]; str[2] = EvEOS;
		if  (sscanf(str,"%d",&min) != 1)  return;
		str[0] = time[4]; str[1] = time[5]; str[2] = EvEOS;
		if  (sscanf(str,"%d",&sec) != 1)  return;
		str[0] = time[6]; str[1] = EvEOS;
		if  (sscanf(str,"%d",&ms) != 1)  return;
		ms *= 100;
		*acc = EvcOnsetAccMillisecond;
	} else {
		return;
	} /*endif*/

	/* check range */
	if  (hour > 23)  return;
	if  (min > 59)  return;
	if  (sec > 59)  return;
	if  (ms > 999)  return;

	/* reformat */
	sprintf( time, "%02u:%02u:%02u.%03u", hour, min, sec, ms );
	*ok = TRUE;

} /* end of EvhCheckTimeInput */



/*-------------------------------------------------------------------*/

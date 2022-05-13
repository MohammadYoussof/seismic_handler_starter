
/* file ev2view.c
 *      =========
 *
 * version 7, 12-Nov-2004
 *
 * summarizes .evt files to a faster readable output
 * K. Stammler, 20-Oct-94
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
#include "eventdsc.h"



/* prototypes of local routines */
void EfPutView( FILE *view, EvEventT *event, EvStatusT *status );



int main( int argc, char *argv[] )
{
	/* local variables */
	char      infile[BC_FILELTH+1];    /* input file */
	char      outfile[BC_FILELTH+1];   /* output file */
	char      header[BC_LINELTH+1];    /* header line */
	EvStatusT status;                  /* return status */

	/* executable code */

	if  (argc < 2 || argv[1][0] == EvEOS)  {
		printf( "   input filename: " );
		EvReadLineInteractive( stdin, BC_FILELTH, infile );
	} else {
		strcpy( infile, argv[1] );
	} /*endif*/

	if  (argc < 3 || argv[2][0] == EvEOS)  {
		printf( "   output filename: " );
		EvReadLineInteractive( stdin, BC_FILELTH, outfile );
	} else {
		strcpy( outfile, argv[2] );
	} /*endif*/

	status = EveNOERROR;
	EvReformatEventfile( infile, outfile, EvGetEvent,
		EfPutView, &status );

#	ifndef BC_VAX
	return 0;
#	endif

} /* end of main */



/*----------------------------------------------------------------------------*/



void EfPutView( FILE *view, EvEventT *event, EvStatusT *status )

/* prints event info in short form
 *
 * parameters of routine
 * FILE       *view;      input; output file
 * EvEventT   *event;     input; event info
 * EvStatusT  *status;    output; return status
 */
{
	/* local variables */
	static BOOLEAN print_date=TRUE;     /* print out date */
	char     phasename[BC_LINELTH+1];   /* phase name */
	BOOLEAN  loc_done;                  /* location done */
	int      i;                         /* counter */

	/* executable code */

	if  (event == NULL)  {print_date=TRUE; return;}

	if  (event->onset_time[0] == EvEOS)  {
		if  (event->comment.length > 0)  {
			fprintf( view, "%s\n", event->comment.text );
		} else {
			fprintf( view, "*** phase with no onset time found ***\n" );
		} /*endif*/
		return;
	} /*endif*/

	if  (print_date)  {
		strncpy( phasename, event->onset_time, 11 );
		phasename[11] = '\0';
		i = 0;
		while  (phasename[i] != '_' && phasename[i] != '\0')  i++;
		phasename[i] = '\0';
		fprintf( view, "\nDate %s  ", phasename );
		if  (event->evid == EvEMPTY_EVID)  {
			fprintf( view, "(no evid)" );
		} else {
			fprintf( view, "(%ld)", event->evid );
		} /*endif*/
		fprintf( view, "   Type: " );
		switch  (event->event_type)  {
		case EvcEventTypeTeleQuake: fprintf( view, "tele     " );  break;
		case EvcEventTypeNuclear:   fprintf( view, "nuclear  " );  break;
		case EvcEventTypeRegioQuake:fprintf( view, "regional " );  break;
		case EvcEventTypeLocalQuake:fprintf( view, "local    " );  break;
		case EvcEventTypeBlast:     fprintf( view, "blast    " );  break;
		case EvcEventTypeMining:    fprintf( view, "mining   " );  break;
		default:                    fprintf( view, "undefined" );  break; 
		} /*endswitch*/
		fprintf( view, "    -%s-\n----------------\n\n", event->analyst );
		print_date = FALSE;
	} /*endif*/

	loc_done = FALSE;

	if  (event->onset_type == EvcOnsetImpulsive)  *phasename = 'i';
	else   *phasename = 'e';
	phasename[1] = '\0';
	strcat( phasename, event->phase );
	if  (event->sign == EvcSignPositive)  strcat( phasename, "c" );
	else if  (event->sign == EvcSignNegative)  strcat( phasename, "d" );
	i = 0;
	while  (event->onset_time[i] != '_' && event->onset_time[i] != '\0')
		i++;
	if  (event->phase_flags[0] != '\0')  {
		fprintf( view, "%5s-%c %s %7s                 flg:%s", event->station,
			event->component, event->onset_time+i+1, phasename,
			event->phase_flags );
	} else {
		fprintf( view, "%5s-%c %s %7s", event->station,
			event->component, event->onset_time+i+1, phasename );
	} /*endif*/
	if  (event->signoise != EvEMPTY_SIGNOISE)
		fprintf( view, "   SNR: %4.1f", event->signoise );
	if  (event->b_slowness != EvEMPTY_SLOWNESS ||
		event->b_azimuth != EvEMPTY_AZIMUTH ||
		event->l_slowness != EvEMPTY_SLOWNESS ||
		event->l_azimuth != EvEMPTY_AZIMUTH)  {
		fprintf( view, "\n   -- " );
		if  (event->b_slowness == EvEMPTY_SLOWNESS)  {
			fprintf( view, "        " );
		} else {
			fprintf( view, " sb:%4.1f", event->b_slowness );
		} /*endif*/
		if  (event->b_azimuth == EvEMPTY_AZIMUTH)  {
			fprintf( view, "             " );
		} else {
			fprintf( view, "    ab: %5.1f", event->b_azimuth );
		} /*endif*/
		if  (event->l_slowness == EvEMPTY_SLOWNESS)  {
			fprintf( view, "            " );
		} else {
			fprintf( view, "     sc:%4.1f", event->l_slowness );
		} /*endif*/
		if  (event->l_azimuth == EvEMPTY_AZIMUTH)  {
			fprintf( view, "             " );
		} else {
			fprintf( view, "    ac: %5.1f", event->l_azimuth );
		} /*endif*/
	} /*endif*/
	if  (event->distance_deg != EvEMPTY_DISTANCE ||
		event->distance_km != EvEMPTY_DISTANCE || event->depth != EvEMPTY_DEPTH) {
		fprintf( view, "\n   -- " );
		if  (event->distance_deg == EvEMPTY_DISTANCE)  {
			if  (event->distance_km == EvEMPTY_DISTANCE)  {
				fprintf( view, "           " );
			} else {
				fprintf( view, "  d:%5.1fkm", event->distance_km );
			} /*endif*/
		} else {
			fprintf( view, "  d:%5.1f  ", event->distance_deg );
		} /*endif*/
		if  (event->depth == EvEMPTY_DEPTH)  {
			fprintf( view, "           " );
		} else {
			fprintf( view, "  z: %5.1f", event->depth );
			switch  (event->depth_type)  {
			case EvcDepthUndefined:  fprintf( view, "%c", 'u' );  break;
			case EvcDepthPreset:     fprintf( view, "%c", 'n' );  break;
			case EvcDepthEstimated:  fprintf( view, "%c", 'g' );  break;
			case EvcDepthFree:       fprintf( view, "%c", ' ' );  break;
			case EvcDepthPoor:       fprintf( view, "%c", '?' );  break;
			case EvcDepthLessWell:   fprintf( view, "%c", '*' );  break;
			case EvcDepthReliable:   fprintf( view, "%c", 'd' );  break;
			case EvcDepthExternal:   fprintf( view, "%c", 'e' );  break;
			default:                 fprintf( view, "%c", '-' );  break;
			} /*endswitch*/
		} /*endif*/
	} /*endif*/
	if  (event->period != EvEMPTY_PERIOD || event->amplitude != EvEMPTY_AMPLITUDE
		|| event->mag[EvcMagMl] != EvEMPTY_MAGNITUDE || event->mag[EvcMagMb] != EvEMPTY_MAGNITUDE
		|| event->mag[EvcMagMs] != EvEMPTY_MAGNITUDE)  {
		fprintf( view, "\n   -- " );
		if  (event->period == EvEMPTY_PERIOD)  {
			fprintf( view, "        " );
		} else {
			fprintf( view, "  p:%4.1f", event->period );
		} /*endif*/
		if  (event->amplitude == EvEMPTY_AMPLITUDE)  {
			fprintf( view, "            " );
		} else {
			fprintf( view, "     a:%6.1f", event->amplitude );
		} /*endif*/
		if  (event->mag[EvcMagMl] != EvEMPTY_MAGNITUDE)  {
			fprintf( view, "  ml: %3.1f", event->mag[EvcMagMl] );
		} /*endif*/
		if  (event->mag[EvcMagMb] != EvEMPTY_MAGNITUDE)  {
			fprintf( view, "  mb: %3.1f", event->mag[EvcMagMb] );
		} /*endif*/
		if  (event->mag[EvcMagMs] != EvEMPTY_MAGNITUDE)  {
			fprintf( view, "  MS: %3.1f", event->mag[EvcMagMs] );
		} /*endif*/
	} /*endif*/
	if  (event->latitude != EvEMPTY_LATITUDE || event->region[0] != EvEOS
		|| event->origin_time[0] != EvEOS)  {
		fprintf( view, "\n   -- " );
		if  (event->latitude != EvEMPTY_LATITUDE)  {
			fprintf( view, "lat:%6.1f", event->latitude );
			fprintf( view, " lon:%6.1f  ", event->longitude );
		} /*endif*/
		if  (event->region[0] != EvEOS)  {
			fprintf( view, "%s  ", event->region );
			loc_done = TRUE;
		} /*endif*/
		if  (event->origin_time[0] != EvEOS)  {
			i = 0;
			while  (event->origin_time[i] != '_' && event->origin_time[i] != '\0')
				i++;
			fprintf( view, "%s", event->origin_time+i+1 );
		} /*endif*/
	} /*endif*/
	if  (loc_done)  {
		fprintf( view, "\n   -- " );
		fprintf( view, "method:" );
		switch  (event->loc_method)  {
		case EvcLocMethCorrBeam:   fprintf( view, "c-beam" );  break;
		case EvcLocMethUncorrBeam: fprintf( view, "u-beam" );  break;
		case EvcLocMethHypo:       fprintf( view, "hypoe" );  break;
		case EvcLocMethLocsat:     fprintf( view, "locsat" );  break;
		case EvcLocMethExternal:   fprintf( view, "external" );  break;
		case EvcLocMethRelTrav:    fprintf( view, "reltrav" );  break;
		default:                   fprintf( view, "undefined" ); break;
		} /*endswitch*/
		fprintf( view, ",qual:" );
		switch  (event->loc_quality)  {
		case EvcLocQualTooWeak:    fprintf( view, "weak" ); break;
		case EvcLocQualIncoherent: fprintf( view, "incoherent" ); break;
		case EvcLocQualNoBearing:  fprintf( view, "no_bearing" ); break;
		case EvcLocQualRegion:     fprintf( view, "region" ); break;
		case EvcLocQualReliable:   fprintf( view, "reliable" ); break;
		default:                   fprintf( view, "undefined" ); break;
		} /*endswitch*/
		fprintf( view, ",used:%d,src:%s", event->stations_used, event->source );
#ifdef XXX
		if  (event->phase_flags[0] != '\0')
			fprintf( view, ",flags:%s", event->phase_flags );
#endif
	} /*endif*/
 	fprintf( view, "\n\n" );
	

} /* end of EfPutView */



/*----------------------------------------------------------------------------*/

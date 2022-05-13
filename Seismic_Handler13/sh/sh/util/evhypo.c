
/* file evhypo.c
 *      ========
 *
 * version 7, 31-Jan-2007
 *
 * prints out onset information in format readable by HYPOELLIPSE program
 * K. Stammler, 8-Sep-93
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
#include "eventdsc.h"
#include "evhypo.h"


/* global constants */
#define EFC_INSTRLTH 100



/* global variables */
static char efv_instruction[EFC_INSTRLTH+1]="";  /* instruction line for hypo */
static BOOLEAN efv_use_s;                        /* use S waves */



/* prototypes of local routines */
static void EfhPrintPhaseLine( FILE *hypo, EvEventT *p, EvEventT *s,
	EvStatusT *status );



/*-------------------------------------------------------------------*/



void EfPutHypo( FILE *hypo, EvEventT *event, EvStatusT *status )

/* Prints event "event" in HYPOELLIPSE format to file "hypo".  This
 * routine may be used as a "put_routine" in "EvReformatEvenfile".
 * It is assumed that phases picked at a single trace (station) are
 * passed in consecutive calls to the routine.  Only the first P and the
 * first S arrival on each station is passed to Hypoellipse.
 *
 * parameters of routine
 * FILE       *hypo;          input; output file
 * EvEventT   *event;         input; event information
 * EvStatusT  *status;        output; return status
 */
{
	/* local variables */
	static EvEventT  p_ev;  /* P phase */
	static EvEventT  s_ev;  /* S phase */

	/* executable code */

	if  (event == NULL)  {
		EfhPrintPhaseLine( hypo, &p_ev, &s_ev, status );
		p_ev.onset_time[0] = '\0';
		s_ev.onset_time[0] = '\0';
		fprintf( hypo, "%s\n", efv_instruction );
		return;
	} else if  ((p_ev.station[0] != '\0' &&
		strcmp(event->station,p_ev.station) != 0) || (s_ev.station[0] != '\0' &&
		strcmp(event->station,s_ev.station) != 0))  {
		EfhPrintPhaseLine( hypo, &p_ev, &s_ev, status );
		p_ev.onset_time[0] = '\0';
		s_ev.onset_time[0] = '\0';
		p_ev.station[0] = '\0';
		s_ev.station[0] = '\0';
	} /*endif*/

	if  (event->phase[0] == 'P')  {
		if  (p_ev.onset_time[0] == '\0')
			p_ev = *event;
		else if  (tc_tdiff(p_ev.onset_time,event->onset_time,status) > 0.0)
			p_ev = *event;
	} else if  (event->phase[0] == 'S')  {
		if  (s_ev.onset_time[0] == '\0')
			s_ev = *event;
		else if  (tc_tdiff(s_ev.onset_time,event->onset_time,status) > 0.0)
			s_ev = *event;
	} else {
		printf( "--> EfPutHypo: phase %s ignored\n", event->phase );
	} /*endif*/

} /* end of EfPutHypo */



/*-------------------------------------------------------------------*/



void EfSetHypoDepth( BOOLEAN fixed, float depth )

/* Sets fixed or free depth for instruction record.  "depth" is valid
 * only if "fixed" is TRUE.
 *
 * parameters of routine
 * BOOLEAN    fixed;        input; fixed depth if TRUE
 * float      depth;        input value of fixed depth in km
 */
{
	/* local variables */
	int      i;           /* counter */
	char     *ch;         /* pointer to character */

	/* executable code */

	/* initialize instruction line to blanks if not yet done */
	if  (*efv_instruction == '\0')  {
		for  (i=0; i<EFC_INSTRLTH; i++)
			efv_instruction[i] = ' ';
		efv_instruction[EFC_INSTRLTH] = '\0';
	} /*endif*/

	sprintf( efv_instruction+19, "%5.2f", depth );
	efv_instruction[24] = ' ';
	ch = efv_instruction + 18;
	if  (fixed)  {
		if  (*ch == '0' || *ch == ' ')  *ch = '1';
	} else {
		if  (*ch == '1')  *ch = '0';
	} /*endif*/

} /* end of EfSetHypoDepth */



/*-------------------------------------------------------------------*/



void EfSetHypoUseS( BOOLEAN use )

/* Enables/disables usage of S waves for location
 *
 * parameters of routine
 * BOOLEAN    use;        input; if TRUE, S waves are use
 */
{
	/* executable code */

	efv_use_s = use;

} /* end of EfSetHypoUseS */



/*-------------------------------------------------------------------*/



static void EfhPrintPhaseLine( FILE *hypo, EvEventT *p, EvEventT *s,
	EvStatusT *status )

/* Writes event info to file.  Station name is assumed to be the same for
 * both phases.  Phase info is assumed to be valid if onset time is not
 * empty.
 *
 * parameters of routine
 * FILE       *hypo;      input; pointer to output file
 * EvEventT   *p, *s;     input; phase info, valid if name != '\0'
 * EvStatusT  *status;    output; return status
 */
{
	/* local variables */
	NTIME    ntime;                    /* numeric time */
	NTIME    reftime;                  /* reference time */
	float    diff;                     /* difference of S to reference time */

	/* executable code */

	if  (p->onset_time[0] == '\0' && s->onset_time[0] == '\0')  {
		/* printf( "--> EfhPrintPhaseLine: strange two empty phases\n" ); */
		return;
	} else if  (p->onset_time[0] == '\0')  {
		printf( "--> EfhPrintPhaseLine: empty P phase, S ignored\n" );
		return;
	} /*endif*/

	/* print station name */
	fprintf( hypo, "%4s", p->station );

	/* p-information */

	/* type of arrival */
	if  (p->onset_type == EvcOnsetImpulsive)  {
		fprintf( hypo, "I" );
	} else {
		fprintf( hypo, "E" );
	} /*endif*/

	/* phase descriptor */
	if  (p->component == 'E')
		fprintf( hypo, "E" );
	else if  (p->component == 'N')
		fprintf( hypo, "N" );
	else 
		fprintf( hypo, "P" );

	/* first motion direction */
	if  (p->sign == EvcSignPositive)  {
		fprintf( hypo, "C" );
	} else if  (p->sign == EvcSignNegative)  {
		fprintf( hypo, "D" );
	} else {
		fprintf( hypo, "." );
	} /*endif*/

	/* weight code */
	if  (p->weight != EvEMPTY_WEIGHT && p->weight < 10 && p->weight >= 0)  {
		fprintf( hypo, "%d", p->weight );
	} else {
		fprintf( hypo, " " );
	} /*endif*/

	/* reflection code */
	fprintf( hypo, " " );

	/* date & time of arrival */
	tc_t2n( p->onset_time, &ntime, status );
	if  (Severe(status))  return;
	ntime.year -= 1900;
	if  (ntime.year >= 100)  ntime.year -= 100;
	fprintf( hypo, "%02d%02d%02d%02d%02d%02d.%02d", ntime.year, ntime.month,
		ntime.day, ntime.hour, ntime.min, ntime.sec, ntime.ms/10 );

	if  (s->onset_time[0] == '\0' || !efv_use_s)  {
		/* terminate line and return */
		fprintf( hypo, "\n" );
		return;
	} /*endif*/

	/* date & time of arrival */
	reftime = ntime;
	reftime.sec = 0;
	reftime.ms = 0;
	if  (reftime.year < 30)  reftime.year += 2000;
	if  (reftime.year < 100)  reftime.year += 1900;
	tc_t2n( s->onset_time, &ntime, status );
	if  (Severe(status))  return;
	diff = tc_ndiff( &ntime, &reftime, status );
	if  (diff < 0.0)  {
		printf( "*** negative S time, ignore S ***\n" );
		fprintf( hypo, "\n" );
		return;
	} /*endif*/
	if  (diff < 100.0)  {
		fprintf( hypo, "       %5.2f", diff );
	} else {
		/* don't know whether this is correct */
		fprintf( hypo, "       %5.1f", diff );
	} /*endif*/
	if  (Severe(status))  return;

	/* type of arrival */
	if  (s->onset_type == EvcOnsetImpulsive)  {
		fprintf( hypo, "I" );
	} else {
		fprintf( hypo, "E" );
	} /*endif*/

	/* phase descriptor */
	fprintf( hypo, "S" );

	/* first motion direction */
	if  (s->sign == EvcSignPositive)  {
		fprintf( hypo, "C" );
	} else if  (s->sign == EvcSignNegative)  {
		fprintf( hypo, "D" );
	} else {
		fprintf( hypo, " " );
	} /*endif*/

	/* weight code */
	if  (s->weight != EvEMPTY_WEIGHT && s->weight < 10 && s->weight >= 0)  {
		fprintf( hypo, "%d", s->weight );
	} else {
		fprintf( hypo, " " );
	} /*endif*/

	/* terminate line */
	fprintf( hypo, "\n" );

} /* end of EfhPrintPhaseLine */



/*-------------------------------------------------------------------*/


/* file evt2reltimes.c
 *      ==============
 *
 * version 9, 21-nov-2005
 *
 * Returns relative travel times of evt-phases (using first evt-phase found)
 * K. Stammler, 21-Jul-94
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
#include "eventdsc.h"
#include "tcusrdef.h"


#define MAXPHASENUM 50


/* prototypes of local routines */
void EfPutRelTimes( FILE *out, EvEventT *event, EvStatusT *status );

/* global variables */
static char efv_mainphase[cBcLineLth+1];            /* main phase name */
static char efv_cur_station[EvSTATIONLTH+1]="";     /* current station name */
static char efv_cur_mainonset[cBcTimeLth+1]="";     /* current main phase */
static char efv_pnames[EvPHASELTH+1][MAXPHASENUM];  /* phase names of diff ph.*/
static char efv_ponsets[cBcTimeLth+1][MAXPHASENUM]; /* phase onsets of diff p.*/
static int efv_pcnt=0;                         /* number of phases found */

static char *efv_accphases[] = {
	"P", "PKKPdf", "PKPab", "PKPbc", "PKPdf", "PP", "PS", "PcP", "PcS",
	"Pdiff", "Pg", "Pn", "S", "SKKPbc", "SKKPdf", "SKKSac", "SKKSdf",
	"SKSac", "SKSdf", "SP", "SS", "SSS", "ScS", "Sdiff", "Sg", "Sn", "pP",
	"pPKPab", "pPKPbc", "pPKPdf", "pPdiff", "pS", "pSKSac", "pSKSdf",
	"sP", "sPKPab", "sPKPbc", "sPKPdf", "sPdiff", "sS", "sSKSac",
	"sSKSdf", "sSdiff", "SKPdf", "SKPab", ""
};


int main( int argc, char *argv[] )
{
	/* local variables */
	char      infile[BC_FILELTH+1];    /* input file */
	char      outfile[BC_FILELTH+1];   /* output file */
	EvStatusT status;                  /* return status */
	int       i;                       /* counter */

	/* executable code */

	if  (argc != 3 && argc != 4)  {
		fprintf( stderr, "Usage: %s <input> <output> [<mainphase>]\n", argv[0] );
		return 1;
	} /*endif*/

	strcpy( infile, argv[1] );
	strcpy( outfile, argv[2] );
	if  (argc == 4)  {
		strcpy( efv_mainphase, argv[3] );
	} else {
		strcpy( efv_mainphase, "" );
	} /*endif*/

	status = EveNOERROR;
	EvReformatEventfile( infile, outfile, EvGetEvent,
		EfPutRelTimes, &status );

	return 0;

} /* end of main */



/*---------------------------------------------------------------------------*/



void EfPutRelTimes( FILE *out, EvEventT *event, EvStatusT *status )

/* Prints error messages of event to file 'out'.
 *
 * parameters of routine
 * FILE       *out;          input; output file
 * EvEventT   *event;        input; event information
 * EvStatusT  *status;       output; return status
 */
{
	/* local variables */
	static TSyBoolean first=TRUE;  /* first phase */
	static char   arrtime[cBcTimeLth+1];    /* arrival time of first phase */
	static float  depth=EvEMPTY_DEPTH;      /* depth found */
	static float  lat=EvEMPTY_LATITUDE;     /* epicenter latitude */
	static float  lon=EvEMPTY_LONGITUDE;    /* epicenter longitude */
	float    tdiff;                         /* relative time in sec */
	int      i;                             /* counter */

	/* executable code */

	if  (event == NULL)  {
		if  (depth != EvEMPTY_DEPTH)  fprintf( out, "! depth %7.2f\n", depth );
		if  (lat != EvEMPTY_LATITUDE)  fprintf( out, "! latitude %7.2f\n", lat );
		if  (lon != EvEMPTY_LONGITUDE)  fprintf( out, "! longitude %7.2f\n", lon);
		fprintf( out, "! reftime %s\n", arrtime );
		/* if  (*phase != '\0')  fprintf( out, "! phase %s\n", phase ); */
		return;
	} /*endif*/

	/* check for allowed phase */
	for  (i=0;;i++)  {
		if  (efv_accphases[i][0] == '\0')  return;
		if  (strcmp(efv_accphases[i],event->phase) == 0)  break;
	} /*endfor*/

	/* take first phase as main phase if not specified */
	if  (*efv_mainphase == '\0')  strcpy( efv_mainphase, event->phase );

	if  (first && strcmp(event->phase,efv_mainphase) == 0)  {
		strcpy( arrtime, event->onset_time );
		first = FALSE;
	} /*endif*/

	if  (event->depth != EvEMPTY_DEPTH)  depth = event->depth;
	if  (event->latitude != EvEMPTY_LATITUDE)  lat = event->latitude;
	if  (event->longitude != EvEMPTY_LONGITUDE)  lon = event->longitude;

	if  (strcmp(event->phase,efv_mainphase) == 0)  {

		/* write out relative time to first mainphase time found */
		tdiff = tc_tdiff( event->onset_time, arrtime, status );
		if  (event->resid_corr != EvEMPTY_RESID_CORR)  tdiff -= event->resid_corr;
		if  (SySevere(status))  return;
		fprintf( out, "%4s %7.2f %s\n", event->station, tdiff, event->phase );

		/* write out differences to already processed diff phases */
		/* ignore residual corrrections on phase differences */
		if  (strcmp(efv_cur_station,event->station) == 0)  {
			strcpy( efv_cur_mainonset, event->onset_time );
			for  (i=0; i<efv_pcnt; i++)  {
				tdiff = tc_tdiff( efv_ponsets[i], efv_cur_mainonset, status );
				if  (SySevere(status))  return;
				fprintf( out, "%4s %9.2f %s-%s\n", efv_cur_station, tdiff,
					efv_pnames[i], efv_mainphase );
			} /*endfor*/
		} else {
			/* new station found, possibly pending diff phases ignored */
			strcpy( efv_cur_mainonset, event->onset_time );
			strcpy( efv_cur_station, event->station );
		} /*endif*/
		efv_pcnt = 0;

	} else {

		/* if new station found reset all pending phases */
		if  (strcmp(efv_cur_station,event->station) != 0)  {
			*efv_cur_mainonset = '\0';
			strcpy( efv_cur_station, event->station );
			efv_pcnt = 0;
		} /*endif*/

		/* write out difference to mainphase if mainphase already processed */
		if  (*efv_cur_mainonset != '\0')  {
			/* corresponding main phase found, write out difference */
			tdiff = tc_tdiff( event->onset_time, efv_cur_mainonset, status );
			if  (SySevere(status))  return;
			fprintf( out, "%4s %9.2f %s-%s\n", efv_cur_station, tdiff,
				event->phase, efv_mainphase );
		} else {
			/* no main phase found, store this phase */
			if  (efv_pcnt == MAXPHASENUM)  {
				fprintf( stderr, "evt2reltimes: too many phases found.  Abort.\n" );
				exit( 1 );
			} /*endif*/
			strcpy( efv_pnames[efv_pcnt], event->phase );
			strcpy( efv_ponsets[efv_pcnt], event->onset_time );
			efv_pcnt++;
		} /*endif*/

	} /*endif*/

} /* end of EfPutRelTimes */



/*---------------------------------------------------------------------------*/


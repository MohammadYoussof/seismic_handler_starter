
/* file evt2phasetimes.c
 *      ================
 *
 * version 3, 3-Jan-2006
 *
 * Writes out realtive arrival times (to origin time) and epicentral distances
 * K. Stammler, 8-Aug-96
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
#include <math.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "cpar.h"
#include "earthloc.h"
#include "glusrdef.h"
#include "erusrdef.h"
#include "tcusrdef.h"
#include "eventdsc.h"


/* constants */
#define cPtStatLth 5
#define cPtMaxPhases 100


/* types */
typedef struct {
	char     station[cPtStatLth+1];     /* station code */
	double   slat;                      /* station latitude in deg */
	double   slon;                      /* station longitude in deg */
	float    elev;                      /* station elevation */
	char     abstime[cBcTimeLth+1];     /* absolute onset time */
	float    reltime;                   /* relative onset time */
	float    epidist;                   /* epicentral distance */
	float    resid;                     /* computed relidual for best fit */
	char     phase[EvPHASELTH+1];       /* phase name */
} TPtReltime;


/* global variables */
static TPtReltime tpv_inf[cPtMaxPhases];  /* phase info */



int main( int argc, char *argv[] )
{
	/* local variables */
	char     *inputs;                     /* pointer to environment variables */
	char     str[cBcLineLth+1];           /* scratch string */
	char     evtfile[cBcFileLth+1];       /* evt filename */
	FILE     *evt;                        /* pointer to evt file */
	EvEventT phinf;                       /* phase info */
	TSyBoolean eof;                       /* EOF found */
	TSyStatus status;                     /* return status */
	int      phcnt;                       /* phase counter */
	int      i;                           /* counter */
	char     origin[cBcTimeLth+1];        /* origin time */
	GLT_STATINF statinf;                  /* station info */
	float    epilat, epilon;              /* epicenter */
	double   d_dist, d_azim, d_bazim;     /* epicentral distance, azimuths */
	TSyBoolean longdesc;                  /* for longdesc entry */
	TSyBoolean origok;                    /* origin time found */
	TSyBoolean epicok;                    /* epicentre found */

	/* executable code */

	status = cBcNoError;

	pa_init( argc, argv );
	if  (pa_pnumber() != 1)  {
		fprintf( stderr, "Usage: %s <evtfile>\n",
			pa_progname() );
		return 1;
	} /*endif*/

	/* get parameters (overwrite defaults) */
	strcpy( evtfile, pa_pvalue(1) );

	longdesc = pa_qspecified( "-longdesc" );

	/* initialization of libraries */
	inputs = (char *)getenv( "SH_INPUTS" );
	if  (inputs == NULL)  {
		fprintf( stderr, "%s: environment SH_INPUTS not set\n", pa_progname() );
		return 1;
	} /*endif*/
	/* station location file */
	strcpy( str, inputs );
	strcat( str, "/" );
	strcat( str, "STATINF.DAT" );
	gl_locfile_name( str );

#ifdef XXX
	/* travel time tables */
	strcpy( str, inputs );
	strcat( str, "/" );
	status = cBcNoError;
	pt_settabledir( str, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );
#endif

	evt = fopen( evtfile, "r" );
	if  (evt == NULL)  {
		fprintf( stderr, "%s: input file %s not found.  Abort.\n",
			pa_progname(), evtfile );
		return 1;
	} /*endif*/

	/* read through all phases */
	eof = FALSE;
	phcnt = 0;
	*origin = '\0';
	epilat = epilon = 0.0;
	FOREVER  {

		EvInitializeEvent( &phinf );
		EvGetEvent( evt, &phinf, &eof, &status );
		if  (eof)  break;
		if  (phcnt == cPtMaxPhases)  {
			fprintf( stderr, "%s: too many phases.  Abort.\n", pa_progname() );
			exit( 1 );
		} /*endif*/

		if  (strcmp(phinf.phase,"L") == 0)  continue;
		if  (strcmp(phinf.phase,"X") == 0)  continue;
		if  (strcmp(phinf.station,"BEAM") == 0)  continue;
		if  (strcmp(phinf.station,"ALIGN") == 0)  continue;

		strcpy( tpv_inf[phcnt].station, phinf.station );
		strcpy( tpv_inf[phcnt].phase, phinf.phase );
		strcpy( tpv_inf[phcnt].abstime, phinf.onset_time );

		if  (phinf.origin_time[0] != '\0')  strcpy( origin, phinf.origin_time );
		if  (phinf.latitude != EvEMPTY_LATITUDE)  epilat = phinf.latitude;
		if  (phinf.longitude != EvEMPTY_LONGITUDE)  epilon = phinf.longitude;

		phcnt++;

	} /*endwhile*/

	fclose( evt );

	origok = epicok = TRUE;
	if  (*origin == '\0')  {
		if  (longdesc)  {
			origok = FALSE;
		} else {
			fprintf( stderr, "%s: no origin time found in %s.  Abort.\n",
				pa_progname(), evtfile );
			exit( 1 );
		} /*endif*/
	} /*endif*/
	if  (epilat == 0.0 && epilon == 0.0)  {
		if  (longdesc)  {
			epicok = FALSE;
		} else {
			fprintf( stderr, "%s: no epicenter found in %s.  Abort.\n",
				pa_progname(), evtfile );
			exit( 1 );
		} /*endif*/
	} /*endif*/

	for  (i=0; i<phcnt; i++)  {
		if  (!origok || !epicok)  {
			tpv_inf[i].reltime = 0.0;
			tpv_inf[i].epidist = 0.0;
			continue;
		} /*endif*/
		tpv_inf[i].reltime = tc_tdiff( tpv_inf[i].abstime, origin, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
		gl_statinf( tpv_inf[i].station, &statinf, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
		mb_locdiff( statinf.lat, statinf.lon, (double)epilat, (double)epilon,
			&d_dist, &d_azim, &d_bazim );
		tpv_inf[i].epidist = (float)d_dist * 111.19;
	} /*endif*/

	if  (longdesc)  {
		for  (i=0; i<phcnt; i++)  {
			printf( "%-5s time: %s phase: %-6s", tpv_inf[i].station,
				tpv_inf[i].abstime, tpv_inf[i].phase );
			if  (origok && epicok)
				printf( "ttime: %7.2f dist: %8.1f",
					tpv_inf[i].reltime, tpv_inf[i].epidist );
			printf( "\n" );
		} /*endif*/
	} else {
		for  (i=0; i<phcnt; i++)  {
			printf( "%-4s %-5s %7.2f %8.1f    %s\n", tpv_inf[i].phase,
				tpv_inf[i].station,  tpv_inf[i].reltime, tpv_inf[i].epidist,
				tpv_inf[i].abstime );
		} /*endif*/
	} /*endif*/

	return 0;

} /* end of main */




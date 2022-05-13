
/* file archevent_shc.c
 *      ===============
 *
 * version 1, 2-mar-95
 *
 * creates SHC command file for archiving analysed events
 * K. Stammler, 2-mar-95
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
#include "eventdsc.h"


#define MAXSTAT 30



/* prototypes of local routines */
static void PutArchShc( FILE *out, EvEventT *event, EvStatusT *status );



int main( int argc, char *argv[] )
{
	/* local variables */
	char      infile[BC_FILELTH+1];    /* input file */
	char      outfile[BC_FILELTH+1];   /* output file */
	EvStatusT status;                  /* return status */

	/* executable code */

	if  (argc != 3)  {
		fprintf( stderr, "Usage: %s <evt-file> <shc-file>\n", argv[0] );
		return 1;
	} /*endif*/

	strcpy( infile, argv[1] );
	strcpy( outfile, argv[2] );

	status = EveNOERROR;
	EvReformatEventfile( infile, outfile, EvGetEvent,
		PutArchShc, &status );

	return 0;

} /* end of main */



/*---------------------------------------------------------------------------*/



static void PutArchShc( FILE *out, EvEventT *event, EvStatusT *status )

/* Creates SHC file for archiving event.
 *
 * parameters of routine
 * FILE       *out;          input; output file
 * EvEventT   *event;        input; event information
 * EvStatusT  *status;       output; return status
 */
{
	/* local variables */
	static char    first_onset[BC_TIMELTH+1]="";   /* first onset time */
	static char    last_onset[BC_TIMELTH+1]="";    /* last onset time */
	static int     num_onset=0;                    /* number of onsets */
	float          diff;                           /* time difference in secs */
	static BOOLEAN use_grf=FALSE;                  /* grf stations used */
	static char    statlist[MAXSTAT][BC_SHORTSTRLTH+1]; /* station list */
	static int     num_stat=0;                     /* number of stations */
	static BOOLEAN l_wave=FALSE;                   /* surface waves observed */
	static int     evid;                           /* event ID */
	int            i;                              /* counter */
	BOOLEAN        stat_found;                     /* station found */
	char           readtime[BC_TIMELTH+1];         /* read time */
	char           lreadtime[BC_TIMELTH+1];        /* long period read time */
	float          s_offset, e_offset;             /* offs times at beg and end*/
	float          readlth;                        /* read length in sec */

	/* executable code */

	if  (event == NULL)  {
		if  (num_onset == 0)  {
			fprintf( stderr, "archevent_shc: no onset in evt-file\n" );
			return;
		} /*endif*/
		diff = tc_tdiff( last_onset, first_onset, status );
		if  (Severe(status))  return;
		s_offset = 90.0;
		e_offset = 180.0;
		if  (diff > 3600.0)  {
			s_offset = 300.0;
			e_offset = 300.0;
		} else if  (diff > 900.0)  {
			s_offset = 120.0;
			e_offset = 240.0;
		} /*endif*/
		tc_tadd( first_onset, -s_offset, readtime, status );
		if  (Severe(status))  return;
		readtime[17] = '\0';  /* truncate seconds */
		tc_tadd( readtime, -600.0, lreadtime, status );
		if  (Severe(status))  return;
		readlth = diff + s_offset + e_offset;
		if  (readlth < 360.0)  readlth = 360.0;
		if  (use_grf)  {
			fprintf( out, "del all\n" );
			fprintf( out, "reads arch: %s %d grf zne bh\n",
				readtime, Nint(readlth) );
			fprintf( out, "writegse grf_%09d.gse\n", evid );
			if  (l_wave)  {
				fprintf( out, "del all\n" );
				fprintf( out, "reads arch: %s 7200 gra1 zne bh\n", lreadtime );
				fprintf( out, "demean all\n" );
				fprintf( out, "fili f lp_5sec_4\n" );
				fprintf( out, "filter f all\n" );
				fprintf( out, "del 1-3\n" );
				fprintf( out, "resample all 1.0\n" );
				fprintf( out, "writegse grf_lp_%09d.gse\n", evid );
			} /*endif*/
		} /*endif*/
#ifdef XXX
		if  (num_stat > 0)  {
			fprintf( out, "del all\n" );
			for  (i=0; i<num_stat; i++)
				fprintf( out, "reads data: %s %d %s zne bh\n",
					readtime, Nint(readlth), statlist[i] );
			fprintf( out, "writegse grsn_%09d.gse\n", evid );
		} /*endif*/
#endif
		fprintf( out, "return\n\n" );
		return;
	} /*endif*/

	if  (event->onset_time[0] == EvEOS)  return;

	evid = event->evid;

	if  (strcmp(event->phase,"L") == 0 || strcmp(event->phase,"(L)") == 0)  {
		l_wave = TRUE;
	} else {
		if  (num_onset == 0)  {
			strcpy( first_onset, event->onset_time );
			strcpy( last_onset, event->onset_time );
			num_onset = 1;
		} else {
			if  (tc_tdiff(event->onset_time,first_onset,status) < 0.0)
				strcpy( first_onset, event->onset_time );
			if  (tc_tdiff(last_onset,event->onset_time,status) < 0.0)
				strcpy( last_onset, event->onset_time );
		} /*endif*/
	} /*endif*/

	if  (strncmp(event->station,"GR",2) == 0
		&& (event->station[2] >= 'A' && event->station[2] <= 'C')
		&& (event->station[3] >= '0' && event->station[3] <= '9'))  {
		use_grf = TRUE;
	} else {
		/* find station */
		stat_found = FALSE;
		for  (i=0; i<num_stat; i++)  {
			stat_found = (strcmp(event->station,statlist[i]) == 0);
			if  (stat_found)  break;
		} /*endfor*/
		/* append station if not yet in list */
		if  (!stat_found)  {
			if  (num_stat == MAXSTAT)  {
				fprintf( stderr, "archevent_shc: number of stations exceeded\n" );
				return;
			} /*endif*/
			if  (strlen(event->station) > BC_SHORTSTRLTH)  {
				fprintf( stderr, "archevent_shc: station name %s too long\n",
					event->station );
				return;
			} /*endif*/
			strcpy( statlist[num_stat++], event->station );
		} /*endif*/
	} /*endif*/

	num_onset++;

} /* end of PutArchShc */



/*---------------------------------------------------------------------------*/


/* file autoloc2evt.c
 *      =============
 *
 * version 2, 21-Nov-2005
 *
 * Converts Autoloc event output file to SHM evt file
 * K. Stammler, 11-Nov-2004
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "tcusrdef.h"
#include "eventdsc.h"


int main( int argc, char *argv[] )
{
	/* local variables */
	char     fname[cBcFileLth+1];     /* name of input file */
	FILE     *fp;                     /* pointer to input file */
	char     outname[cBcFileLth+1];   /* name of output file */
	FILE     *out;                    /* pointer to output file */
	char     line[cBcLineLth+1];      /* current line of file */
	TSyBoolean dataline;              /* data lines found */
	TSyStatus status;                 /* return status */
	char     firstpick[cBcLineLth+1]; /* time of first pick */
	EvEventT evt;                     /* phase info */
	char     station[cBcLineLth+1];   /* station name */
	char     netcode[cBcLineLth+1];   /* network code */
	char     datestr[cBcLineLth+1];   /* date string */
	char     timestr[cBcLineLth+1];   /* time string */
	char     phase[EvPHASELTH];       /* default phase name */
	char     comp;                    /* component */
	NTIME    ntime;                   /* numeric time */
	char     *env;                    /* pointer to environment variable */
	char     *c;                      /* pointer for parsing line */
	/* for location and origin time */
	float    lat, lon;
	char     slat, slon;

	/* executable code */

	status = cBcNoError;
	*firstpick = '\0';

	/* fixed parameters */
	comp = 'Z';

	if  (argc < 3)  {
		fprintf( stderr, "Usage: %s <autoloc-event> <output> [<phase>]\n", argv[0] );
		return 1;
	} /*endif*/

	/* get parameters */
	if  (strlen(argv[1]) > cBcFileLth)  {
		fprintf( stderr, "%s: filename too long.  Abort.\n", argv[0] );
		return 2;
	} /*endif*/
	strcpy( fname, argv[1] );
	if  (strlen(argv[2]) > cBcFileLth)  {
		fprintf( stderr, "%s: output filename too long.  Abort.\n", argv[0] );
		return 2;
	} /*endif*/
	strcpy( outname, argv[2] );
	if  (argc > 3)  {
		if  (strlen(argv[3]) > EvPHASELTH)  {
			fprintf( stderr, "%s: phase name %s too long.  Abort.\n",
				argv[0], argv[3] );
			return 5;
		} /*endif*/
		strcpy( phase, argv[3] );
	} else {
		strcpy( phase, "X" );
	} /*endif*/

	/* open input file */
	if  (strcmp(fname,"-") == 0)  {
		fp = stdin;
	} else {
		fp = fopen( fname, "r" );
		if  (fp == NULL)  {
			fprintf( stderr, "%s: error opening input file %s\n", argv[0], fname );
			return 3;
		} /*endif*/
	} /*endif*/

	/* read through file */
	dataline = FALSE;
	out = NULL;
	while  (fgets(line,cBcLineLth,fp) != NULL)  {

		if  (strncmp(line,"  Stat  Net   Date",18) == 0)  {
			dataline = TRUE;
			continue;
		} /*endif*/

		if  (dataline)  {

			/* parse data line */
			if  (strchr(line,'/') == NULL)  break;
			/*printf( "%s", line );*/
			EvInitializeEvent( &evt );
			if  (sscanf(line,"%s %s %s %s",station,netcode,datestr,timestr) != 4) {
				fprintf( stderr, "%s: error reading file %s at line:\n%s",
					argv[0], fname, line );
				fclose( fp );
				return 4;
			} /*endif*/
			strncpy( evt.station, station, EvSTATIONLTH );
			strcpy( evt.phase, phase );
			evt.component = comp;
			evt.pick_type = EvcPickTypeAuto;
			if  (strlen(datestr)+strlen(timestr)+1 > EvTIMELTH)  {
				fprintf( stderr, "%s: date/time too long.  Abort\n", argv[0] );
				return 4;
			} /*endif*/
			strcat( datestr, "_" );
			strcat( datestr, timestr );
			tc_tadd( datestr, 0.0, evt.onset_time, &status );
			if  (SySevere(&status))  {
				fprintf( stderr, "%s: error converting time.  Abort.\n", argv[0] );
				exit( 1 );
			} /*endif*/

			/* prepare output file */
			if  (out == NULL)  {
				if  (strcmp(outname,"-") == 0)  {
					out = stdout;
				} else if  (strcmp(outname,"auto") == 0)  {
					tc_t2n( evt.onset_time, &ntime, &status );
					if  (SySevere(&status))  {
						fprintf( stderr, "%s: error converting time %s.  Abort.\n",
							argv[0], evt.onset_time );
						exit( 1 );
					} /*endif*/
					env = (char *)getenv( "SH_SCRATCH" );
					if  (env == NULL)  {
						fprintf( stderr, "%s: SH_SCRATCH not set.  Abort.\n", argv[0] );
						exit( 7 );
					} /*endif*/
					sprintf( outname, "%sauto_%02d%02d%02d_%02d%02d%02d.evt",
						env, ntime.year, ntime.month, ntime.day, ntime.hour,
						ntime.min, ntime.sec );
					out = fopen( outname, "w" );
					if  (out == NULL)  {
						fprintf( stderr, "%s: error opening output file %s\n",
							argv[0], outname );
						exit( 8 );
					} /*endif*/
					strcpy( firstpick, evt.onset_time );
				} else {
					out = fopen( outname, "w" );
					if  (out == NULL)  {
						fprintf( stderr, "%s: error opening output file %s\n",
							argv[0], outname );
						exit( 6 );
					} /*endif*/
				} /*endif*/
			} /*endif*/

			EvPutEvent( out, &evt, &status );
			if  (SySevere(&status))  {
				fprintf( stderr, "%s: error writing evtfile.  Abort.\n", argv[0] );
				exit( 1 );
			} /*endif*/

		} /*endif*/

	} /*endwhile*/

	/* search for LocSAT location */
	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		if  (strncmp(line," LocSAT solution",16) == 0)  {
			fgets( line, cBcLineLth, fp );
			fgets( line, cBcLineLth, fp );
			c = strstr( line, "20" );   /* look for year */
			if  (c != NULL)  {
				if  (c[4] == '/')  {  /* is it really a date ? */
					if  (sscanf(c,"%4d/%2d/%2d  %2d:%2d:%2d  %f %c %f %c",
						&ntime.year,&ntime.month,&ntime.day,&ntime.hour,&ntime.min,
						&ntime.sec,&lat,&slat,&lon,&slon) == 10)  {
						ntime.ms = 0;
						EvInitializeEvent( &evt );
						tc_n2t( &ntime, evt.origin_time, &status );
						if  (SySevere(&status))  {
							fprintf( stderr, "%s: err conv time.\n", argv[0] );
							exit( 1 );
						} /*endif*/
						if  (slat == 'S')  lat = -lat;
						if  (slon == 'W')  lon = -lon;
						evt.latitude = lat;
						evt.longitude = lon;
						EvPutEvent( out, &evt, &status );
					} /*endif*/
				} /*endif*/
			} /*endif*/
			break;
		} /*endif*/
	} /*endwhile*/

	if  (fp != stdin)  fclose( fp );
	if  (out != stdout)  fclose( out );

	if  (*firstpick != '\0')
		printf( "%s %s\n", firstpick, outname );

	return 0;

} /* end of main */

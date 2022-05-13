
/* file weather2ascii.c
 *      ===============
 *
 * version 2, 21-Nov-2005
 *
 * Reads in data files of weather station and writes ASCII files for input
 * to write_steim1
 *
 * K. Stammler, 22-Feb-2004
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
#include "erusrdef.h"
#include "tcusrdef.h"


/* time difference between samples in s */
#define DT 60.0
/* number of data values per time */
#define VALNO 9
/* station name in data files */
#define STATNAME "GRFO"


/* global variables */
static FILE *wav_fp[VALNO];   /* file pointers */
static char *wav_prefix[VALNO] = { "wt+wv_", "wt+wd_", "wt+tv_", "wt+ta_", "wt+ti_", "wt+hm_", "wt+pa_", "wt+p0_", "wt+pr_" };
static float wav_mult[VALNO] =   { 10.0,     1.0,      10.0,     10.0,     10.0,     10.0,     10.0,     10.0,     10.0     };
static int wav_idx[VALNO] =      { 23,       46,       68,       90,       112,      133,      154,      178,      198      };
static char *wav_chan[VALNO] =   { "wv0",    "wd0",    "tv0",    "ta0",    "ti0",    "hm0",    "pa0",    "p00",    "pr0"    };


/* prototypes of local routines */
void wa_openfiles( NTIME *ntime );
void wa_closefiles( void );
void wa_write_headers( NTIME *ntime );



int main( int argc, char *argv[] )
{
	/* local variables */
	char     inpfile[cBcFileLth+1];    /* name of input data file */
	FILE     *fp;                      /* pointer to input file */
	char     line[cBcLongStrLth+1];    /* current line of data file */
	NTIME    ntime, otime;             /* numeric times */
	TSyStatus status;                  /* return status */
	float    tdiff;                    /* sample difference */
	char     timestr[cBcTimeLth+1];    /* time string */
	float    val[VALNO];               /* data values */
	int      i;                        /* counter */
	int      linecnt;                  /* line counter */

	/* executable code */

	if  (argc != 2)  {
		fprintf( stderr, "Usage: %s <inputfile>\n", argv[0] );
		return 1;
	} /*endif*/

	/* get parameters */
	strcpy( inpfile, argv[1] );

	/* open input file */
	fp = fopen( inpfile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: cannot open input file %s.  Abort.\n",
			argv[0], inpfile );
		return 1;
	} /*endif*/

	status = cBcNoError;
	otime.year = -1;
	linecnt = 1;
	/* read through data file */
	while  (fgets(line,cBcLongStrLth,fp) != NULL)  {
		if  (line[3] != '.' || line[6] != '.')  continue;
		if  (sscanf( line, " %2d.%2d.%2d %2d:%2d", &ntime.day, &ntime.month,
			&ntime.year, &ntime.hour, &ntime.min ) != 5)  {
			fprintf( stderr, "%s: error reading data line %s", argv[0], line );
			continue;
		} /*endif*/
		ntime.sec = ntime.ms = 0;
		if  (ntime.year < 1900 && ntime.year < 60)  ntime.year += 2000;
		else if  (ntime.year < 1900)  ntime.year += 1900;
		if  (otime.year == -1)  {
			wa_openfiles( &ntime );
			wa_write_headers( &ntime );
		} else {
			tdiff = tc_ndiff( &ntime, &otime, &status );
			if  (SySevere(&status))  {
				err_writemsg( status, "", FALSE );
				status = cBcNoError;
				continue;
			} /*endif*/
			if  (fabs(tdiff-DT) > 0.01)  {
				wa_closefiles();
				tc_n2t( &otime, timestr, &status );
				if  (SySevere(&status))  err_writemsg( status, "", TRUE );
				fprintf( stderr, "%s: data gap found between %s and ",
					argv[0], timestr );
				tc_n2t( &ntime, timestr, &status );
				if  (SySevere(&status))  err_writemsg( status, "", TRUE );
				fprintf( stderr, "%s\n", timestr );
				wa_openfiles( &ntime );
				wa_write_headers( &ntime );
			} /*endif*/
		} /*endif*/
		/* read data values of this line */
		for  (i=0; i<VALNO; i++)  {
			if  (sscanf(line+wav_idx[i],"%f",val+i) != 1)  {
				fprintf( stderr, "%s: error reading data in line %s",
					argv[0], line );
				wa_closefiles();
				fclose( fp );
				return 1;
			} /*endif*/
			fprintf( wav_fp[i], " %d", Nint(val[i]*wav_mult[i]) );
			if  (linecnt % 8 == 0)  fprintf( wav_fp[i], "\n" );
		} /*endfor*/
		otime = ntime;
		linecnt++;
		/*printf( "%s", line );*/
	} /*endwhile*/

	wa_closefiles();
	fclose( fp );

	return 0;

} /* end of main */



/*----------------------------------------------------------------------------*/



void wa_openfiles( NTIME *ntime )

/* Opens output data files including start time into names
 *
 * parameters of routine
 * NTIME      *ntime;        input; start time of file
 */
{
	/* local variables */
	char     fname[cBcFileLth+1];     /* name of output file */
	int      i;                       /* file counter */

	/* executable code */

	for  (i=0; i<VALNO; i++)  {

		sprintf( fname, "%s%02d%02d%02d_%02d%02d.asc", wav_prefix[i],
			ntime->year-2000, ntime->month, ntime->day, ntime->hour, ntime->min );
		wav_fp[i] = fopen( fname, "w" );
		if  (wav_fp[i] == NULL)  {
			fprintf( stderr, "weather2ascii: cannot open output file %s.  Abort.\n",
				fname );
			exit( 1 );
		} /*endif*/

	} /*endfor*/

} /* end of wa_openfiles */



/*----------------------------------------------------------------------------*/



void wa_closefiles( void )

/* closes all open output files
 *
 * no parameters
 */
{
	/* local variables */
	int      i;            /* counter */

	/* executable code */

	for  (i=0; i<VALNO; i++)
		fclose( wav_fp[i] );

} /* end of wa_closefiles */



/*----------------------------------------------------------------------------*/



void wa_write_headers( NTIME *ntime )

/* Writes all header files for use with write_steim1
 *
 * parameters of routine
 * NTIME      *ntime;       input; start time of file
 */
{
	/* local variables */
	char     fname[cBcFileLth+1];     /* name of output file */
	int      i;        /* counter */
	FILE     *fp;      /* pointer to output file */
	char     timestr[cBcTimeLth+1];  /* time string */
	TSyStatus status;  /* return status */

	/* executable code */

	status = cBcNoError;
	tc_n2t( ntime, timestr, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );

	for  (i=0; i<VALNO; i++)  {

		sprintf( fname, "%s%02d%02d%02d_%02d%02d.hdr", wav_prefix[i],
			ntime->year-2000, ntime->month, ntime->day, ntime->hour, ntime->min );
		fp = fopen( fname, "w" );
		if  (fp == NULL)  {
			fprintf( stderr, "weather2ascii: cannot open header file %s.  Abort.\n",
				fname );
			exit( 1 );
		} /*endif*/

		fprintf( fp, "sequence number:    1\n" );
		fprintf( fp, "station:            %s\n", STATNAME );
		fprintf( fp, "channel:            %s\n", wav_chan[i] );
		fprintf( fp, "start time:         %s\n", timestr );
		fprintf( fp, "sample rate factor: 1\n", timestr );
		fprintf( fp, "sample rate multiplier: %d\n", -Nint(DT) );

		fclose( fp );

	} /*endfor*/

} /* end of wa_write_headers */



/*----------------------------------------------------------------------------*/

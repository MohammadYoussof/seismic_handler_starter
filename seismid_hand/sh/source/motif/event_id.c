
/* file event_id.c
 *      ==========
 *
 * version 6, 22-May-2006
 *
 * Generates unique event and parameter ID's
 * K. Stammler, 12-Sep-93
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
#include BC_SYSBASE
#include BC_TCUSRDEF
#include "event_id.h"


/* constants */
#define EiTABLE_LTH 366
	/* maximum number of entries (days/year) */
#define EiCMT_LINES 5
	/* maximum number of comment lines */


/* types */

typedef struct {
	char     comment[EiCMT_LINES][BC_LINELTH+1];      /* comment */
	int      cmtlines;                /* number of comment lines */
	long     date_id[EiTABLE_LTH];                       /* date */
	int      day_count[EiTABLE_LTH];              /* daily count */
	int      table_lth;                     /* number of entries */
} EiTableT;



/* global variables */
static char       eiv_path[BC_FILELTH+1];    /* path to data files */



/* prototypes of local routines */
static long EihGetUniqueID( char onset[], char type[], STATUS *status );
static void EihGenerateFilename( unsigned year, char type[], char file[] );
static void EihReadDataFile( char file[], EiTableT *table,
	STATUS *status );
static void EihWriteDataFile( char file[], EiTableT *table,
	STATUS *status );
static void EihNewEvent( EiTableT *table, long did, long *evid,
	STATUS *status );



/*------------------------------------------------------------------*/

#ifdef XXX

#define do_test(k)	strcpy( onset, k ); \
							evid = EiGetEventID( onset, &status ); \
							if  (Severe(&status))  return 1; \
							printf( "onset %s,  event ID: %ld\n", onset, evid )



int main( void )

/* test main routine
 */
{
	/* local variables */
	char     onset[BC_TIMELTH+1];     /* onset time */
	long     evid;                    /* event ID */
	STATUS   status;                  /* return status */

	/* executable code */

	status = BC_NOERROR;
	EiSetPath( "/home/b3sn06/klaus/evid/", &status );

	do_test( "01-AUG-93_03:55:46" );
	do_test( "01-AUG-93_04:55:46" );
	do_test( "01-AUG-93_05:55:46" );
	do_test( "01-AUG-93_06:55:46" );
	do_test( "01-AUG-93_07:55:46" );
	do_test( "02-AUG-93_01:55:46" );
	do_test( "02-AUG-93_02:55:46" );
	do_test( "02-AUG-93_03:55:46" );

	return 0;

} /* end of main */

#endif /* XXX */

/*------------------------------------------------------------------*/



void EiSetPath( char path[], STATUS *status )

/* Sets path for event ID files
 *
 * parameters of routine
 * char       path[];       input; path name
 * STATUS     *status;      output; return status
 */
{
	/* executable code */

	if  (strlen(path) > BC_FILELTH-EiEV_FILE_LTH)  {
		*status = EieSTROVFL;
		return;
	} /*endif*/

	strcpy( eiv_path, path );

} /* end of EiSetPath */



/*------------------------------------------------------------------*/



long EiGetEventID( char onset[], STATUS *status )

/* Returns unique event ID for event
 *
 * parameters of routine
 * char       onset[];       input; onset time of event
 * STATUS     *status;       output; return status
 *                           returns event ID
 */
{
	/* executable code */

	return EihGetUniqueID( onset, EiEV_FILE_PREFIX, status );

} /* end of EiGetEventID */



/*------------------------------------------------------------------*/



long EiGetParameterID( char onset[], STATUS *status )

/* Returns unique parameter ID.
 *
 * parameters of routine
 * char       onset[];       input; onset time of event
 * STATUS     *status;       output; return status
 *                           returns event ID
 */
{
	/* executable code */

	return EihGetUniqueID( onset, EiPAR_FILE_PREFIX, status );

} /* end of EiGetEventID */



/*------------------------------------------------------------------*/



static long EihGetUniqueID( char onset[], char type[], STATUS *status )

/* Returns unique ID.
 *
 * parameters of routine
 * char       onset[];       input; onset time of event
 * STATUS     *status;       output; return status
 *                           returns event ID
 */
{
	/* local variables */
	long     did;                  /* date ID */
	NTIME    ntime;                /* numeric time */
	char     fname[BC_FILELTH+1];  /* data file name */
	EiTableT table;                /* ID table */
	long     evid;                 /* event ID */

	/* executable code */

	tc_t2n( onset, &ntime, status );
	if  (Severe(status))  return 0;
	/* if  (ntime.year >= 2000)  ntime.year -= 2000; */
	/* want to have 100 after year 99 */
	if  (ntime.year >= 1900)  ntime.year -= 1900;
	did = (long)ntime.year*10000L + (long)ntime.month*100L + ntime.day;

	EihGenerateFilename( ntime.year, type, fname );
	EihReadDataFile( fname, &table, status );
	if  (Severe(status))  return 0;
	EihNewEvent( &table, did, &evid, status );
	if  (Severe(status))  return 0;
	EihWriteDataFile( fname, &table, status );
	if  (Severe(status))  return 0;
	return evid;

} /* end of EiGetEventID */



/*------------------------------------------------------------------*/



static void EihGenerateFilename( unsigned year, char type[], char file[] )

/* Generates filename of table.  The file variable must have a
 * minimum length of BC_FILELTH.
 *
 * parameters of routine
 * unsigned   year;      input; year of event
 * char       type[];    input; file type ("evid" or "par")
 * char       file[];    output; generated filename
 */
{
	/* local variables */
	char     str[BC_SHORTSTRLTH+1];   /* scratch */

	/* executable code */

	if  (year < 50)  year += 2000;
	if  (year < 1900)  year += 1900;
	sprintf( str, "%d", year );

	strcpy( file, eiv_path );
	strcat( file, type );
	strcat( file, str );
	strcat( file, EiFILE_EXT );

} /* end of EihGenerateFilename */



/*------------------------------------------------------------------*/



static void EihReadDataFile( char file[], EiTableT *table,
	STATUS *status )

/* Reads data table from file
 *
 * parameters of routine
 * char       file[];        input; data file name
 * EiTableT   *table;        output; table read
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	FILE     *fp;                  /* file pointer */
	char     line[BC_LINELTH+1];   /* current line */
	BOOLEAN  start;                /* start of file */
	long     did;                  /* date ID */
	int      dcnt;                 /* daily count */

	/* executable code */

	table->cmtlines = 0;
	table->table_lth = 0;

	fp = sy_fopen( file, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "EihReadDataFile: file %s not found\n", file );
		return;
	} /*endif*/

	start = TRUE;
	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (*line == EiCOMMENT_CHAR)  {
			if  (start && table->cmtlines < EiCMT_LINES)  {
				strcpy( table->comment[(table->cmtlines)++], line );
			} else {
				fprintf( stderr, "EihReadDataFile: comment line ignored\n" );
			} /*endif*/
		} else {
			if  (table->table_lth == EiTABLE_LTH)  {
				*status = EieTABLE_OVFL;
				sy_fclose( fp );
				return;
			} else if  (sscanf(line,"%ld %d",&did,&dcnt) != 2)  {
				fprintf( stderr, "EihReadDataFile: illegal line %s\n", line );
				*status = EieILL_LINE;
				sy_fclose( fp );
				return;
			} /*endif*/
			table->date_id[table->table_lth] = did;
			table->day_count[table->table_lth] = dcnt;
			(table->table_lth)++;
		} /*endif*/
	} /*endwhile*/

	sy_fclose( fp );

} /* end of EihReadDataFile */



/*------------------------------------------------------------------*/



static void EihWriteDataFile( char file[], EiTableT *table,
	STATUS *status )

/* Writes table to file
 *
 * parameters of routine
 * char       file[];        input; name of output file
 * EiTableT   *table;        input; table to write
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	FILE     *fp;           /* pointer to output file */
	int      i;             /* counter */

	/* executable code */

	fp = sy_fopen( file, "w" );
	if  (fp == NULL)  {
		fprintf( stderr, "EihWriteDataFile: output file %s could not be opened\n",
			file );
		*status = EieOPEN_OUTPUT;
		return;
	} /*endif*/

	for  (i=0; i<table->cmtlines; i++)
		fprintf( fp, "%s", table->comment[i] );
	for  (i=0; i<table->table_lth; i++)
		fprintf( fp, "%06ld %03d\n", table->date_id[i], table->day_count[i] );

	sy_fclose( fp );

} /* end of EihWriteDataFile */



/*------------------------------------------------------------------*/



static void EihNewEvent( EiTableT *table, long did, long *evid,
	STATUS *status )

/* returns new event ID and updates table
 *
 * parameters of routine
 * EiTableT   *table;      modify; table to read & update event ID
 * long       did;         input; date ID
 * long       *evid;       output; event id
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	int      i;         /* counter */
	BOOLEAN  found;     /* found day */
	int      dcnt;      /* daily count */

	/* executable code */

	found = FALSE;
	for  (i=0; i<table->table_lth; i++)  {
		if  (table->date_id[i] == did)  {
			found = TRUE;
			break;
		} /*endif*/
	} /*endfor*/

	if  (found)  {
		dcnt = ++(table->day_count[i]);
		if  (dcnt >= 1000)  {
			*status = EieEVID_OVFL;
			return;
		} /*endif*/
	} else {
		if  (table->table_lth >= EiTABLE_LTH)  {
			*status = EieTABLE_OVFL;
			return;
		} /*endif*/
		table->date_id[table->table_lth] = did;
		table->day_count[table->table_lth] = 1;
		(table->table_lth)++;
		dcnt = 1;
	} /*endif*/
	*evid = did*1000L + dcnt;

} /* end of EihNewEvent */



/*------------------------------------------------------------------*/


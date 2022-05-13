
/* file evhypout.c
 *      ==========
 *
 * version 3, 2-Nov-93
 *
 * reads output file of HYPOELLIPSE to EvEventT structure
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
#include BASECNST
#include BC_SYSBASE
#include BC_TCUSRDEF
#include "eventdsc.h"
#include "evhypout.h"



/* constants */
#define MAX_STATION 30



/* global variables */
static char efv_stat[MAX_STATION][BC_LINELTH+1];  /* station info */
static int  efv_stat_num=0;                       /* number of stations found */



/* prototypes of local routines */
static void EfhReadResultLine( char line[], EvEventT *event,
	EvStatusT *status );



/*------------------------------------------------------------------*/



void EfGetHypoOut( FILE *file, EvEventT *event, BOOLEAN *eof,
	EvStatusT *status )

/* Reads event structure from output file of HYPOELLIPSE.  Currently
 * it is assumed that there is only one event written in the
 * HYPOELLIPSE file.  This means that the second call to this routine
 * on the same file always returns *eof == TRUE.
 *
 * parameters of routine
 * FILE       *file;       input; pointer to input file
 * EvEventT   *event;      output; event read from file
 * BOOLEAN    *eof;        output; end of file found
 * EvStatusT  *status;     output; return status
 */
{
	/* local variables */
	char     line[BC_LONGSTRLTH+1];     /* current line of file */
	int      slen;                      /* length of string */

	/* executable code */

	EvInitializeEvent( event );

	if  (feof(file))  {
		*eof = TRUE;
		return;
	} /*endif*/

	/* loop lines of file */
	FOREVER  {

		/* check end of file */
		if  (fgets(line,BC_LONGSTRLTH,file) == NULL)  {
			*eof = TRUE;
			return;
		} /*endif*/

		/* check for header line of results */
		if  (strstr(line,"date") != NULL && strstr(line,"origin") != NULL
			&& strstr(line,"lat") != NULL && strstr(line,"depth") != NULL)  {
			/* header line found, now read everything needed and then */
			/* return                                                 */
			/* ... */
			/* read next line */
			if  (fgets(line,BC_LONGSTRLTH,file) == NULL)  {
				*eof = TRUE;
				return;
			} /*endif*/
			EfhReadResultLine( line, event, status );
			if  (Severe(status))  return;
		} else if  (strstr(line,"-- travel times and delays --") != NULL)  {
			while  (fgets(line,BC_LONGSTRLTH,file) != NULL)  {
				slen = strlen( line );
				if  (line[slen-1] == '\n')
					line[--slen] = '\0';
				if  (slen < 5 || line[4] <= ' ')
					break;
				if  (efv_stat_num == MAX_STATION-1)  {
					printf( "*** evhypout: too many stations ***\n" );
					break;
				} /*endif*/
				if  (slen > BC_LINELTH)  {
					printf( "*** evhypout: line truncated ***\n" );
					line[BC_LINELTH] = '\0';
				} /*endif*/
				strcpy( efv_stat[efv_stat_num++], line );
			} /*endwhile*/
			/* exit loop and finish */
			break;
		} /*endif*/

	} /*endfor*/

	*eof = FALSE;
	/* put file pointer to the end, so the next call will return eof */
	fseek( file, 0, 2 );

} /* end of EfGetHypoOut */



/*------------------------------------------------------------------*/



void EfAppendStationInfo( char fname[] )

/* Appends station info of most recent Hypoellipse file read to given
 * file.
 *
 * parameters of routine
 * char       fname[];      input; filename to append info
 */
{
	/* local variables */
	FILE     *fp;                /* file pointer */
	int      i;                  /* counter */
	char     str[BC_LINELTH+1];  /* scratch */

	/* executable code */

	fp = fopen( fname, "a" );
	if  (fp == NULL)  {
		printf( "*** EfAppendStationInfo: couldn't open file %s ***\n", fname );
		return;
	} /*endif*/

	fprintf( fp, "\n\n\n" );
	for  (i=0; i<efv_stat_num; i++)  {
		sscanf( efv_stat[i], "%s", str );
		if  (strcmp(str,"stn") == 0)  continue;
		if  (efv_stat[i][8] == 's')  continue;
		fprintf( fp, "!SI: %s ", str );
		strncpy( str, efv_stat[i]+30, 6 );
		str[6] = '\0';
		fprintf( fp, "%s ", str );
		strncpy( str, efv_stat[i]+45, 6 );
		str[6] = '\0';
		fprintf( fp, "%s ", str );
		strncpy( str, efv_stat[i]+53, 3 );
		str[3] = '\0';
		fprintf( fp, "%s\n", str );
	} /*endfor*/

	fclose( fp );

} /* end of EfAppendStationInfo */



/*------------------------------------------------------------------*/



static void EfhReadResultLine( char line[], EvEventT *event,
	EvStatusT *status )

/* Reads result line
 *
 * parameters of routine
 * char       line[];       input; line read from HYPO file
 * EvEventT   *event;       output; event information
 * EvStatusT  *status;      output; return status
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];         /* scratch string */
	NTIME    origin;                    /* origin time */
	float    tmp;                       /* scratch */
	int      sign;                      /* sign of lat & lon */
	int      number;                    /* scratch */

	/* executable code */

	/* get origin time */
	strncpy( str, line+1, 2 );
	str[2] = EvEOS;
	sscanf( str, "%d", &origin.year );
	strncpy( str, line+3, 2 );
	str[2] = EvEOS;
	sscanf( str, "%d", &origin.month );
	strncpy( str, line+5, 2 );
	str[2] = EvEOS;
	sscanf( str, "%d", &origin.day );
	strncpy( str, line+8, 2 );
	str[2] = EvEOS;
	sscanf( str, "%d", &origin.hour );
	strncpy( str, line+10, 2 );
	str[2] = EvEOS;
	sscanf( str, "%d", &origin.min );
	strncpy( str, line+13, 5 );
	str[5] = EvEOS;
	sscanf( str, "%f", &tmp );
	origin.sec = (unsigned)tmp;
	origin.ms = (unsigned)Nint((tmp-(float)origin.sec)*1000.0);
	tc_n2t( &origin, event->origin_time, status );
	if  (Severe(status))  return;

	/* latitude */
	strncpy( str, line+18, 3 );
	str[3] = EvEOS;
	sscanf( str, "%d", &number );
	if  (Cap(line[21]) == 'N')  {
		sign = 1;
	} else if  (Cap(line[21]) == 'S')  {
		sign = -1;
	} else {
		printf( "-->EfGetHypoOut: illegal latitude sign\n" );
		*status = EveFORMAT_ERR;
		return;
	} /*endif*/
	strncpy( str, line+22, 5 );
	str[5] = EvEOS;
	sscanf( str, "%f", &tmp );
	if  (tmp >= 60.0)  {
		printf( "-->EfGetHypoOut: illegal latitude minutes\n" );
		*status = EveFORMAT_ERR;
		return;
	} /*endif*/
	event->latitude = ((EvFloatT)number + tmp/60.0) * (EvFloatT)sign;

	/* longitude */
	strncpy( str, line+27, 4 );
	str[4] = EvEOS;
	sscanf( str, "%d", &number );
	if  (Cap(line[31]) == 'E')  {
		sign = 1;
	} else if  (Cap(line[31]) == 'W')  {
		sign = -1;
	} else {
		printf( "-->EfGetHypoOut: illegal longitude sign\n" );
		*status = EveFORMAT_ERR;
		return;
	} /*endif*/
	strncpy( str, line+32, 5 );
	str[5] = EvEOS;
	sscanf( str, "%f", &tmp );
	if  (tmp >= 60.0)  {
		printf( "-->EfGetHypoOut: illegal longitude minutes\n" );
		*status = EveFORMAT_ERR;
		return;
	} /*endif*/
	event->longitude = ((EvFloatT)number + tmp/60.0) * (EvFloatT)sign;

	/* depth */
	sscanf( line+37, "%f", &(event->depth) );

	/* magnitude is currently ignored */

} /* end of EfhReadResultLine */



/*------------------------------------------------------------------*/

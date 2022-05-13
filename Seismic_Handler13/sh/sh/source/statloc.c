
/* file STATLOC.C
 * ==============
 *
 * version 15, 22-Oct-2006
 *
 * manager of station location file
 * K. Stammler, 29-JUL-1990
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1992,  Klaus Stammler, Federal Institute for Geosciences
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
#include BC_SYSBASE
#include "erusrdef.h"
#include "glusrdef.h"
#include "globalparams.h"


/* global constants */
#define MAXNAMELTH 10
	/* maximum length of station name */
#define MAXLINELTH 200
	/* maximum length of line */


/* global variables */
static GLT_STATLIST *glv_list_root=NULL;                /* root of station list */


/* prototypes of local routines */
static void glh_copy_station( char in[], char out[], STATUS *status );




/*-----------------------------------------------------------------------------*/



void gl_statloc( char station[], STATLOC *lat, STATLOC *lon, STATUS *status )

/* OBSOLETE FUNCTION; SHOULD NOT BE USED ANY MORE
 * reads location of station from location file
 *
 * parameters of routine
 * char       station[];   input; name of station
 * STATLOC    *lat, *lon;  output; latitude and longitude of station
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	FILE     *lf;                      /* locfile pointer */
	char     currstat[MAXNAMELTH+1];   /* current station */
	char     cstation[MAXNAMELTH+1];   /* copied station name */
	char     line[MAXLINELTH+1];       /* current line */

	/* executable code */

	if  (*station == '\0')  return;
	glh_copy_station( station, cstation, status );
	if  (Severe(status))  return;
	lf = sy_fopen( GpGetStringElem(cGpL_station_info_file,0), "r" );
	if  (lf == NULL)  {
		*status = GLE_OPNRD;
		err_setcontext( " ## file " );
		err_setcontext( GpGetStringElem(cGpL_station_info_file,0) );
		return;
	} /*endif*/
	do  {
		if  (fgets(line,MAXLINELTH,lf) == NULL)  {
			*status = GLE_NOTFOUND;
			break;
		} /*endif*/
		sscanf( line, "%s %lf %lf", currstat, lat, lon );
	}  while  (strcmp(cstation,currstat) != 0);
	fclose( lf );

} /* end of gl_statloc */



/*-----------------------------------------------------------------------------*/



void gl_full_statloc( char station[], STATLOC *lat, STATLOC *lon,
	int *arrcode, float *x, float *y, int maxcmt, char *comment,
	STATUS *status )

/* OBSOLETE FUNCTION; SHOULD NOT BE USED ANY MORE
 * reads location of station from location file
 *
 * parameters of routine
 * char       station[];   input; name of station
 * STATLOC    *lat, *lon;  output; latitude and longitude of station
 * int        *arrcode;    output; array code
 * float      *x, *y;      output; array coordinates
 * int        maxcmt;      input; maximum length of comment
 * char       *comment;    output; (maxcmt>0) comment on trace
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	FILE     *lf;                      /* locfile pointer */
	char     currstat[MAXNAMELTH+1];   /* current station */
	char     cstation[MAXNAMELTH+1];   /* copied station name */
	char     line[MAXLINELTH+1];       /* current line */
	char     *cmtptr;                  /* pointer to comment */
	int      slth;                     /* string length */

	/* executable code */

	if  (*station == '\0')  return;
	glh_copy_station( station, cstation, status );
	if  (Severe(status))  return;
	lf = sy_fopen( GpGetStringElem(cGpL_station_info_file,0), "r" );
	if  (lf == NULL)  {
		*status = GLE_OPNRD;
		err_setcontext( " ## file " );
		err_setcontext( GpGetStringElem(cGpL_station_info_file,0) );
		return;
	} /*endif*/
	do  {
		if  (fgets(line,MAXLINELTH,lf) == NULL)  {
			*status = GLE_NOTFOUND;
			break;
		} /*endif*/
		sscanf( line, "%s %lf %lf %d %f %f", currstat, lat, lon,
			arrcode, x, y );
		/* get comment */
		if  (comment != NULL  &&  maxcmt > 0)  {
			cmtptr = strchr( line , '!' );
			if  (cmtptr == NULL)  {
				*comment = '\0';
			} else if  (*(++cmtptr) == '\0')  {
				*comment = '\0';
			} else {
				strncpy( comment, cmtptr, maxcmt );
				slth = (int)strlen( comment ) - 1;
				if  (comment[slth] == '\n')
					comment[slth] = '\0';
			} /*endif*/
		} /*endif*/
	}  while  (strcmp(cstation,currstat) != 0);
	fclose( lf );

} /* end of gl_full_statloc */



/*-----------------------------------------------------------------------------*/



void gl_statinf( char station[], GLT_STATINF *inf, STATUS *status )

/* reads station information from data file
 *
 * parameters of routine
 * char         station[];   input; name of station
 * GLT_STATINF  *inf;        output; station info block
 * STATUS       *status;     output; return status
 */
{
	/* local variables */
	FILE     *lf;                          /* locfile pointer */
	char     currstat[GLC_MAXCODELTH+1];   /* current station */
	char     cstation[GLC_MAXCODELTH+1];   /* copied station name */
	char     line[GLC_MAXLINELTH+1];       /* current line */
	char     *cp;                          /* char pointer */
	int      filecnt;                      /* file counter */
	TSyBoolean found;                      /* station found */

	/* executable code */

	/* reset info block */
	inf->code[0] = '\0';
	inf->name[0] = '\0';
	inf->instr[0] = '\0';
	inf->lat = GLC_INVALID_NUMBER;
	inf->lon = GLC_INVALID_NUMBER;
	inf->arr_id = GLC_INVALID_ARRID;
	inf->xrel = GLC_INVALID_NUMBER;
	inf->yrel = GLC_INVALID_NUMBER;
	inf->elevation = GLC_INVALID_NUMBER;
	inf->velocity = GLC_INVALID_NUMBER;

	if  (*station == '\0')  return;
	glh_copy_station( station, cstation, status );
	if  (Severe(status))  return;

	found = FALSE;
	for  (filecnt=0;!found;filecnt++)  {
		found = FALSE;
		cp = GpGetStringElem( cGpL_station_info_file, filecnt );
		if  (cp == NULL)  break;
		if  (GpGetInt(cGpI_debug_level) > 4)
			printf( "SH-dbg5: open station info file %s\n", cp );
		lf = sy_fopen( cp, "r" );
		if  (lf == NULL)  {
			*status = GLE_OPNRD;
			err_setcontext( " ## file " );
			err_setcontext( GpGetStringElem(cGpL_station_info_file,filecnt) );
			return;
		} /*endif*/
		found = TRUE;
		do  {
			if  (fgets(line,GLC_MAXLINELTH,lf) == NULL)  {
				found = FALSE;
				break;
			} /*endif*/
			sscanf( line, "%s", currstat );
		}  while  (strcmp(cstation,currstat) != 0);
		fclose( lf );
	} /*endfor*/

	if  (!found)  {
		*status = GLE_NOTFOUND;
		err_setcontext( " ## station " );
		err_setcontext( cstation );
		return;
	} /*endif*/

	/* parse line */
	strcpy( inf->code, cstation );
	if  (!SySevere(status))  {
		cp = strstr( line, "name:" );
		if  (cp != NULL)
			sscanf( cp, "name:%s", inf->name );
		cp = strstr( line, "instrument:" );
		if  (cp != NULL)
			sscanf( cp, "instrument:%s", inf->instr );
		cp = strstr( line, "lat:" );
		if  (cp != NULL)
			sscanf( cp, "lat:%lf", &(inf->lat) );
		cp = strstr( line, "lon:" );
		if  (cp != NULL)
			sscanf( cp, "lon:%lf", &(inf->lon) );
		cp = strstr( line, "array:" );
		if  (cp != NULL)
			sscanf( cp, "array:%d", &(inf->arr_id) );
		cp = strstr( line, "xrel:" );
		if  (cp != NULL)
			sscanf( cp, "xrel:%lf", &(inf->xrel) );
		cp = strstr( line, "yrel:" );
		if  (cp != NULL)
			sscanf( cp, "yrel:%lf", &(inf->yrel) );
		cp = strstr( line, "elevation:" );
		if  (cp != NULL)
			sscanf( cp, "elevation:%f", &(inf->elevation) );
		cp = strstr( line, "velocity:" );
		if  (cp != NULL)
			sscanf( cp, "velocity:%f", &(inf->velocity) );
	} /*endif*/

} /* end of gl_statinf */



/*-----------------------------------------------------------------------------*/



GLT_STATINF *gl_store_station( char station[], BOOLEAN rm_err, STATUS *status )

/* retrieves station information and stores it permanently in a
 * station list.  If no station information is found then an error
 * status is returned, but the station info block is created and
 * enlisted anyway.  The caller of this function may insert
 * his own station information to the returned (empty) structure.
 * If the station is already in the list it remains unchanged and a
 * pointer to the existing info block is returned.
 * This function returns a NULL pointer only if no more memory
 * is available or the specified station code is too long or is empty
 *
 * parameters of routine
 * char       station[];     input; station code
 * BOOLEAN    rm_err;        input; remove info block on error
 * STATUS     *status;       output; return status
 *                           returns pointer to static info structure
 */
{
	/* local variables */
	GLT_STATLIST   *curr_s;      /* pointer to current station */
	GLT_STATLIST   *prev_s;      /* previous station */

	/* executable code */

	if  (*station == '\0')  {
		*status = GLE_EMPTYSTR;
		return NULL;
	} else if  (strlen(station) > GLC_MAXCODELTH)  {
		*status = GLE_STROVFL;
		return NULL;
	} /*endif*/

	/* check whether station is already there */
	prev_s = NULL;
	curr_s = glv_list_root;
	while  (curr_s != NULL)  {
		if  (strcmp(station,curr_s->inf.code) == 0)
			return &(curr_s->inf);
		prev_s = curr_s;
		curr_s = curr_s->next;
	} /*endwhile*/

	/* station not yet stored, do it now */
	curr_s = (GLT_STATLIST *)sy_allocmem( 1, (int)sizeof(GLT_STATLIST),
		status );
	if  (Severe(status))  return NULL;
	curr_s->next = NULL;
	gl_statinf( station, &(curr_s->inf), status );
	if  (Severe(status))  {
		if  (rm_err)  {
			sy_deallocmem( curr_s );
			return NULL;
		} /*endif*/
		strcpy( curr_s->inf.code, station );
		curr_s->inf.name[0] = '\0';
		curr_s->inf.instr[0] = '\0';
		curr_s->inf.lat = GLC_INVALID_NUMBER;
		curr_s->inf.lon = GLC_INVALID_NUMBER;
		curr_s->inf.arr_id = GLC_INVALID_ARRID;
		curr_s->inf.xrel = GLC_INVALID_NUMBER;
		curr_s->inf.yrel = GLC_INVALID_NUMBER;
		curr_s->inf.elevation = GLC_INVALID_NUMBER;
	} /*endif*/
	if  (glv_list_root == NULL)  {
		glv_list_root = curr_s;
	} else {
		prev_s->next = curr_s;
	} /*endif*/

	return &(curr_s->inf);

} /* end of gl_store_station */



/*-----------------------------------------------------------------------------*/



void gl_clear_station_list( void )

/* clears the station list in memory
 *
 * no parameters
 */
{
	/* local variables */
	GLT_STATLIST *curr_s;     /* pointer to station list element */
	GLT_STATLIST *next_s;     /* pointer to next station list element */

	/* executable code */

	curr_s = glv_list_root;
	glv_list_root = NULL;

	while  (curr_s != NULL)  {
		next_s = curr_s->next;
		sy_deallocmem( curr_s );
		curr_s = next_s;
	} /*endwhile*/

} /* end of gl_clear_station_list */



/*-----------------------------------------------------------------------------*/



void gl_insert( char station[], STATLOC lat, STATLOC lon,
	int arraycode, float x, float y, char comment[], STATUS *status )

/* inserts new station into location file
 *
 * parameters of routine
 * char       station[];    input; station name
 * STATLOC    lat, lon;     input; latitude & longitude of station
 * int        arraycode;    input; array number, if array station
 * float      x, y;         input; distance from array reference point (km)
 * char       comment[];    input; comment on station (full name, ...)
 * int        *status;      output; return status
 */
{
	/* local variables */
	int      locstat;    /* local status */
	char     cstation[MAXNAMELTH+1];   /* copied station name */
	STATLOC  l1, l2;     /* scratch */
	FILE     *lf;        /* location file pointer */

	/* executable code */

	locstat = GLE_NOERROR;
	glh_copy_station( station, cstation, status );
	if  (Severe(status))  return;
	gl_statloc( cstation, &l1, &l2, &locstat );
	if  (locstat != GLE_NOTFOUND)  {
		*status = GLE_STATOVWR;
		return;
	} /*endif*/

	lf = sy_fopen( GpGetStringElem(cGpL_station_info_file,0), "a" );
	if  (lf == NULL)  {
		*status = GLE_OPNWR;
		err_setcontext( " ## file " );
		err_setcontext( GpGetStringElem(cGpL_station_info_file,0) );
		return;
	} /*endif*/
	fprintf( lf, "%-10s %+13.8lf %+13.8lf %3d %+10.5f %+10.5f %s\n",
		cstation, lat, lon, arraycode, x, y, comment );
	fclose( lf );

} /* end of gl_insert */



/*-----------------------------------------------------------------------------*/



void gl_locfile_name( char fname[] )

/* sets name of location file
 *
 * parameters of routine
 * char       fname[];     input;
 */
{
	/* executable code */

	/*strncpy( glv_locfile, fname, BC_FILELTH );*/
	if  (GpGetBoolean(cGpB_overwrite_string_lists))
		GpParseStringList( cGpL_station_info_file, fname );

} /* end of gl_locfile_name */



/*-----------------------------------------------------------------------------*/



static void glh_copy_station( char in[], char out[], STATUS *status )

/* copies from in to out and translates GRF names if necessary
 *
 * parameters of routine
 * char       in[];     input; input name
 * char       out[];    output; output name
 * STATUS     *status;  output; return status
 */
{
	/* executable code */

	if  (strlen(in) > MAXNAMELTH)  {
		*status = GLE_STROVFL;
		return;
	} /*endif*/

	if  (strncmp(in,"GRF-",4) == 0)  {
		strcpy( out, "GR" );
		strcat( out, in+4 );
	} else if  (strncmp(in,"B@",2) == 0)  {
		strcpy( out, in+2 );
	} else {
		strcpy( out, in );
	} /*endif*/

} /* end of glh_copy_station */



/*-----------------------------------------------------------------------------*/


/* file GLUSRDEF.H
 *      ==========
 *
 * version 8, 22-May-2006
 *
 * prototypes & errors of module STATLOC.C
 * K. Stammler, 22-MAY-91
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

#ifndef __GLUSRDEF
#define __GLUSRDEF


/* errors */
#define GLE_NOERROR   0
#define GLE_OFFSET    2600
#define GLE_NOTFOUND  (GLE_OFFSET+1)   /* station not found */
#define GLE_OPNRD     (GLE_OFFSET+2)   /* error opening location file as input */
#define GLE_STATOVWR  (GLE_OFFSET+3)   /* station is already in location file */
#define GLE_OPNWR     (GLE_OFFSET+4)   /* error opening location file as output */
#define GLE_STROVFL   (GLE_OFFSET+5)   /* string overflow */
#define GLE_EMPTYSTR  (GLE_OFFSET+6)   /* empty name specified */


/* constants */
#define GLC_MAXCODELTH 20    /* maximum length of station code */
#define GLC_MAXNAMELTH 80    /* maximum length of full name */
#define GLC_MAXINSTRLTH 50   /* maximum length of instrument name */
#define GLC_MAXLINELTH 250   /* maximum length of info line */


/* validity check values & routines */
#define GLC_INVALID_ARRID 0
	/* for array ID only */
#define GLC_INVALID_NUMBER -1.0e10
#define gl_valid_number(s) ((s) > -1.0e9)
	/* for all real and double numbers */


/* types */
typedef double STATLOC;

typedef struct {
	char     code[GLC_MAXCODELTH+1];    /* station code */
	char     name[GLC_MAXNAMELTH+1];    /* full name */
	char     instr[GLC_MAXINSTRLTH+1];  /* instrument name */
	STATLOC  lat, lon;                  /* location */
	int      arr_id;                    /* array ID */
	STATLOC  xrel, yrel;                /* relative array pos in km */
	float    elevation;                 /* elevation in m */
	float    velocity;                  /* average velocity in km/sec */
} GLT_STATINF;

typedef struct glt_statlist {
	GLT_STATINF         inf;     /* station info block */
	struct glt_statlist *next;   /* pointer to next station */
} GLT_STATLIST;


/* prototypes */
/*-----------------------------------------------------------------------------*/


void gl_statloc( char station[], STATLOC *lat, STATLOC *lon, int *status );

/* reads location of station from location file
 *
 * parameters of routine
 * char       station[];   input; name of station
 * STATLOC    *lat, *lon;  output; latitude and longitude of station
 * int        *status;     output; return status
 */


/*-----------------------------------------------------------------------------*/


void gl_full_statloc( char station[], STATLOC *lat, STATLOC *lon,
	int *arrcode, float *x, float *y, int maxcmt, char *comment, int *status );

/* reads location of station from location file
 *
 * parameters of routine
 * char       station[];   input; name of station
 * STATLOC    *lat, *lon;  output; latitude and longitude of station
 * int        *arrcode;    output; array code
 * float      *x, *y;      output; array coordinates
 * int        maxcmt;      input; maximum length of comment
 * char       *comment;    output; (maxcmt>0) comment on trace
 * int        *status;     output; return status
 */


/*-----------------------------------------------------------------------------*/


void gl_statinf( char station[], GLT_STATINF *inf, STATUS *status );

/* reads station information from data file
 *
 * parameters of routine
 * char         station[];   input; name of station
 * GLT_STATINF  *inf;        output; station info block
 * STATUS       *status;     output; return status
 */


/*-----------------------------------------------------------------------------*/


GLT_STATINF *gl_store_station( char station[], BOOLEAN rm_err, STATUS *status );

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


/*-----------------------------------------------------------------------------*/


void gl_insert( char station[], STATLOC lat, STATLOC lon,
	int arraycode, float x, float y, char comment[], int *status );

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


/*-----------------------------------------------------------------------------*/


void gl_locfile_name( char fname[] );

/* sets name of location file
 *
 * parameters of routine
 * char       fname[];     input;
 */


/*-----------------------------------------------------------------------------*/

#endif /* __GLUSRDEF */

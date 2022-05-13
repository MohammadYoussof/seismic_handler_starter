
/* file residual.h
 *      ==========
 *
 * version 2, 22-May-2006
 *
 * header file of module residual.c
 * K. Stammler, 3-Feb-2001
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


/* error codes */
#define eRsOffset 6400
#define eRsFileNotFound       (eRsOffset+1)    /* file not found */
#define eRsTableNotClosed     (eRsOffset+2)    /* table not closed */
#define eRsIncompleteTable    (eRsOffset+3)    /* information incomplete */
#define eRsLongName           (eRsOffset+4)    /* station name too long */
#define eRsReadSlowness       (eRsOffset+5)    /* error reading slowness */
#define eRsSyntaxError        (eRsOffset+6)    /* syntax error */
#define eRsReadTablth         (eRsOffset+7)    /* error reading table length */
#define eRsNoTableFound       (eRsOffset+8)    /* no table found */
#define eRsAzimOutOfRange     (eRsOffset+9)    /* azimuth out of range */
#define eRsProgramBug         (eRsOffset+10)   /* program bug */
#define eRsLongPhaseList      (eRsOffset+11)   /* phase list too long */
#define eRsLongFilename       (eRsOffset+12)   /* filename too long */


#define cRsStationLth 10       /* length of station string */
#define cRsPhaseListLth 50     /* length of phase list */
#define cRsEmptySlowness -1.0  /* empty slowness value */
#define cRsDefaultName "residuals.dat"  /* default filename for data */

typedef struct _rstable {
	char      station[cRsStationLth+1];  /* name of station */
	char      phases[cRsPhaseListLth+1]; /* phase list to which this applies */
	float     minslow;                   /* start of slowness interval (s/deg) */
	float     maxslow;                   /* end of slowness interval (s/deg) */
	int       tablth;                    /* number of entries in table */
	float     *azim;                     /* pointer to azimuth values (deg) */
	float     *resid;                    /* pointer to residual values (s) */
	struct _rstable  *next;              /* pointer to next table */
} TRsTable;


/*----------------------------------------------------------------------------*/


void RsReadTables( char fname[], TSyStatus *status );

/* Read in residual tables from file
 *
 * parameters of routine
 * char       file[];       input; filename
 * STATUS     *status;      output; return status
 */


/*----------------------------------------------------------------------------*/


void RsInitTable( TRsTable *tab );

/* initializes table
 *
 * parameters of routine
 * TRsTable   *tab;      output; table to initialize
 */


/*----------------------------------------------------------------------------*/


float RsResidual( char station[], char phase[], float slowness, float azimuth,
	TSyStatus *status );

/* Returns residual time in s for given station, slowness and back-azimuth
 *
 * parameters of routine
 * char       station[];      input; station name
 * char       phase[];        input; phase name
 * float      slowness;       input; slowness in s/deg
 * float      azimuth;        input; back-azimuth n deg
 * TSyStatus  status;         output; return status
 */


/*----------------------------------------------------------------------------*/


void RsDumpTables( void );

/* Dumps all tables to stdout
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/

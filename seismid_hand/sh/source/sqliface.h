
/* File sqliface.h
 *      ==========
 *
 * version 3, 6-Feb-2007
 *
 * Interface to SQL databases.  For description of tables see sqliface.c
 * K. Stammler, 21-Dec-2006
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


#define SQLE_OFFSET       1400
#define SQLE_CREATETMP    (SQLE_OFFSET+1) /* error creating unique scratch file */
#define SQLE_STROVFL      (SQLE_OFFSET+2) /* string overflow */
#define SQLE_SFTAB_PARSE  (SQLE_OFFSET+3) /* error parsing sftab line */
#define SQLE_READTMP      (SQLE_OFFSET+4) /* error reading from scratch file */

/* station flags */
#define SQLF_STA_OPENACCESS   0x0001
#define SQLF_STA_ISGERMAN     0x0002
#define SQLF_STA_PRIMARCHIVE  0x0004
#define SQLF_STA_TMPNAME      0x0008
#define SQLF_STA_UNCHECKED    0x0010
#define SQLF_STA_MOBILE       0x0020
#define SQLF_STA_OUTDATED     0x0040
#define SQLF_STA_OFFLINE      0x0080

/*----------------------------------------------------------------------------*/


void SqlSelectDataFiles( char start[], float readlth, char listfile[],
	TSyStatus *status );

/* Selects a list of data files from database, table sftab, which contain the
 * requested time window
 *
 * parameters of routine
 * char       start[];          input; start time
 * float      readlth;          input; read length in seconds
 * char       listfile[];       output; file with database output
 * TSyStatus  *status;          output; return status
 */


/*----------------------------------------------------------------------------*/


void SqlParseSftabLine( char line[], char station[], SeedFileDescrT *descr,
	TSyStatus *status );

/* Parses output line of an SQL request to sftab table
 *
 * parameters of routine
 * char       line[];       input; current line of db file
 * char       station[];    input; search station, for speeding up the search
 * SeedFileDescrT *descr;   output; parsed line
 * TSyStatus  *status;      output; return status
 */

/*----------------------------------------------------------------------------*/


void SqlUniqueScratchFile( char fname[], TSyStatus *status );

/* Opens a scratch file with a unique name for exclusive use.  Should be closed
 * by DbCloseScratchFile on program exit.
 *
 * parameters of routine
 * char       fname[];        output; name of scratch file
 * TSyStatus  *status;        output: return status
 */


/*---------------------------------------------------------------------*/


void SqlDeleteScratchFile( void );

/* remove scratch file created with SqlUniqueScratchFile
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void SqlTranslateStationList( char name[], int maxlth, char list[],
TSyStatus *status );

/* translates a station list name into the station list
 *
 * parameters of routine
 * char       name[];     input; name of list
 * int        maxlth;     input, maximum length of output string
 * char       list[];     output; station list
 * TSyStatus  *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void SqlPathcacheFile( char fname[], TSyStatus *status );

/* Sets path cache file.  Used if a rootpath cache is created externally
 * and passed to the read routine to save calls to database (e.g. on
 * single calls to copy_recs)
 *
 * parameters of routine
 * char       fname[];     input; name of pathcache
 */


/*----------------------------------------------------------------------------*/

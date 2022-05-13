
/* File sqliface.c
 *      ==========
 *
 * version 5, 6-Feb-2007
 *
 * Interface to SQL databases
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

/* database tables used for data file access:
 *
 * table sftab; miniseed file entries
 * __________________________________
 *
 * station     VARCHAR(10),
 * chan        CHAR(2),
 * comp        CHAR,
 * pathid      INTEGER,
 * relpath     VARCHAR(255),
 * sdate       INTEGER,
 * stime       DOUBLE,
 * edate       INTEGER,
 * etime       DOUBLE,
 * recnum      INTEGER,
 * hswap       TINYINT,
 * recsize     INTEGER,
 * offset      INTEGER,
 * dataflags   INTEGER,
 * priority    TINYINT,
 * dataformat  TINYINT,
 * PRIMARY KEY(relpath),
 * index(sdate),
 * index(stime),
 * index(edate),
 * index(etime)
 *
 * station:    lowercase station name, e.g. 'grfo'
 * chan:       lowercase channel name, e.g. 'bh'
 * comp:       lowercase component, e.g. 'z'
 * pathid:     rootpath ID number;
 *             the path strings are taken from lookup table 'pathtab'
 *             suggested ranges of 'pathid' numbers:
 *             0      : empty rootpath, absolute path given in 'relpath'
 *             1-999  : paths to temporary files (not yet archived)
 *             1000   : reserved entry; separator between temporary and
 *                      archive paths
 *             >1000  : paths to archived data ('DVD-paths' or permanent
 *                      'RAID-paths')
 * relpath:    relative paths to data files
 * sdate:      start date of data file, integer YYYYMMDD
 * stime:      start time of data file, float HHMMSS.SSS
 * edate:      end date of data file, integer YYYYMMDD
 * etime:      end time of data file, float HHMMSS.SSS
 * recnum:     number of SEED records in data file, used for positioning
 *             within file
 * hswap:      swap header info in miniseed record headers (0=no, 1=yes)
 * recsize:    size of SEED records in bytes, typical values: 512 or 4096
 * offset:     number of leading SEED records in data file which are not data
 *             records, usually 0 or 1 in MiniSEED, larger values in full SEED
 *             volumes
 * dataflags:  data quality flags as defined in SEED record header.  A value set
 *             here affects all records of the file;
 *             0x01   Saturated
 *             0x02   Clipped
 *             0x04   Spikes
 *             0x08   Glitches
 *             0x10   Missing Data Padded
 *             0x20   Sync Error
 *             0x40   Filter Charging
 *             0x80   Time Tag
 *             a value of 0 means that data are ok
 * dataformat: data format ID; not yet implemented in SH/SHM.
 *             Suggested: 1=miniseed, 2=gcf
 * priority:   priority of entry; larger values = higher priority
 *             e.g. temporary data paths (id < 1000) should have lower priority
 *             than archived paths (id > 1000), since archived paths should be
 *             complete
 *
 *
 * table pathtab; lookup table for rootpath entries
 * _______________________________
 *
 * id          INTEGER,
 * rootpath    VARCHAR(255),
 * PRIMARY KEY(id)
 *
 * id:         path ID; description see 'pathid' in table sftab
 * rootpath:   rootpath to data
 */


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "shconst.h"
#include "tcusrdef.h"
#include "seed_io/seedcfg.h"
#include "sqliface.h"
#include "globalparams.h"


/* local types */

/* root path cache for minimising number of db calls */
typedef struct _pathcache {
	struct _pathcache *next;    /* pointer to next */
	int    path_id;             /* path id */
	char   *rpath;              /* pointer to root path */
} SqlPathCacheT;

/* station list cache for minimising calls to staadmin table */
typedef struct _stacache {
	struct _stacache *next;     /* pointer to next */
	char   *listname;           /* name of list */
	char   *listval;            /* pointer to station list */
} SqlStationCacheT;


/* global variables */
static char sqlv_sfdb_tmp[cBcFileLth+1]="";  /* scratch file for db output */
static SqlPathCacheT    *sqlv_pathcache;     /* pointer to list of paths */
static SqlStationCacheT *sqlv_stacache;      /* pointer to list of stations */
static char sqlv_cache_file[cBcFileLth+1]="";/* path cache file */

/*
 * prototypes of local routines
 * ----------------------------
 */
static char *SqlGetRootPath( int pathid );
static void SqlCacheRootPath( int pathid, char rootpath[], TSyStatus *status );
static char *SqlGetStationList( char name[] );
static void SqlCacheStationList( char name[], char stalist[], TSyStatus *status );


/* requested elements of table 'sftab' */
#define TABLE_ELEMENTS "station,chan,comp,pathid,relpath,sdate,stime,edate,etime,recnum,hswap,recsize,offset,dataflags,priority,dataformat"


/*----------------------------------------------------------------------------*/



void SqlSelectDataFiles( char start[], float readlth, char listfile[],
	TSyStatus *status )

/* Selects a list of data files from database, table sftab, which contain the
 * requested time window
 *
 * parameters of routine
 * char       start[];          input; start time
 * float      readlth;          input; read length in seconds
 * char       listfile[];       output; file with database output
 * TSyStatus  *status;          output; return status
 */
{
	/* local variables */
	static char last_start[cBcTimeLth+1]="";   /* last start time */
	static float last_readlth=0.0;             /* last read length */
	char     shellcmd[cBcVeryLongStrLth+1];    /* shell command */
	char     tmpfile[cBcFileLth+1];            /* scratch file, not used */
	NTIME    sntime, entime;                   /* numeric read times */
	int      datea, dateb;                     /* dates of seed entry */
	double   timea, timeb;                     /* times of entry */
	int      rd1date, rd2date;                 /* read date as integer */
	double   rd1time, rd2time;                 /* read time as double */

	/* executable code */

	/* create new scratch file if first call to this routine */
	if  (*last_start == '\0')  SqlUniqueScratchFile( tmpfile, status );
	if  (SySevere(status))  return;

	/* convert time to numeric format */
	tc_t2n( start, &sntime, status );
	if  (SySevere(status))  return;
	tc_nadd( &sntime, readlth, &entime, status );
	if  (SySevere(status))  return;

	/* call to db if new start time given */
	if  (strcmp(start,last_start) != 0 || readlth != last_readlth)  {
		rd1date = sntime.year*10000 + sntime.month*100 + sntime.day;
		rd1time = (double)sntime.hour*10000.0 + (double)sntime.min*100.0
			+ (double)sntime.sec + (double)sntime.ms/1000.0;
		rd2date = entime.year*10000 + entime.month*100 + entime.day;
		rd2time = (double)entime.hour*10000.0 + (double)entime.min*100.0
			+ (double)entime.sec + (double)entime.ms/1000.0;
		sprintf( shellcmd,
			"\\rm %s; %s %s \"select %s from sftab where (((edate > %d) OR (edate = %d AND etime >= %10.3lf)) AND ((sdate < %d) OR (sdate = %d AND stime <= %10.3lf))) ORDER BY sdate,stime\" >%s",
			sqlv_sfdb_tmp, GpGetString(cGpS_sfdb_command),
			GpGetString(cGpS_sfdb_exec_qual), TABLE_ELEMENTS, rd1date, rd1date,
			rd1time, rd2date, rd2date, rd2time, sqlv_sfdb_tmp );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SH-dbg3: executing %s\nSH-dbg3: command line length %d\n",
				shellcmd, strlen(shellcmd) );
		system( shellcmd );
		strcpy( last_start, start );
		last_readlth = readlth;
	} else {
		if  (GpGetInt(cGpI_debug_level) > 5)
			printf( "SH-dbg6: use result of last SQL call: %s\n", sqlv_sfdb_tmp );
	} /*endif*/

	strcpy( listfile, sqlv_sfdb_tmp );

} /* end of SqlSelectDataFiles */



/*----------------------------------------------------------------------------*/



void SqlUniqueScratchFile( char fname[], TSyStatus *status )

/* Opens a scratch file with a unique name for exclusive use.  Should be closed
 * by SqlDeleteScratchFile on program exit.
 *
 * parameters of routine
 * char       fname[];        output; name of scratch file
 * TSyStatus  *status;        output: return status
 */
{
	/* local variables */
	int      i;         /* counter */
	FILE     *fp;       /* pointer to file */

	/* executable code */

	i = 1;
	FOREVER  {
		sprintf( sqlv_sfdb_tmp, "/tmp/sql_sfdb_%d.000", i++ );
		fp = fopen( sqlv_sfdb_tmp, "r" );
		if  (fp == NULL)  break;
		fclose( fp );
	} /*endfor*/

	/* create empty file to reserve name */
	fp = fopen( sqlv_sfdb_tmp, "w" );
	if  (fp == NULL)  {
		*status = SQLE_CREATETMP;
	} else {
		fclose( fp );
	} /*endif*/

	strcpy( fname, sqlv_sfdb_tmp );

} /* end of SqlUniqueScratchFile */



/*---------------------------------------------------------------------*/



void SqlParseSftabLine( char line[], char station[], SeedFileDescrT *descr,
	TSyStatus *status )

/* Parses output line of an SQL request to sftab table.  The station parameter
 * is used to discontinue the processing of the current line if it does
 * not match.
 *
 * parameters of routine
 * char       line[];       input; current line of db file
 * char       station[];    input; search station, for speeding up the search
 * SeedFileDescrT *descr;   output; parsed line
 * TSyStatus  *status;      output; return status
 */
{
	/* local variables */
	char     tstation[cBcLineLth+1];      /* station code */
	char     tchan[cBcLineLth+1];         /* channel code */
	char     tcomp;                       /* component */
	int      pathid;                      /* path ID */
	char     relpath[cBcFileLth+1];       /* relative path */
	int      datea, dateb;                /* dates of seed entry */
	double   timea, timeb;                /* times of entry */
	NTIME    ntime;                       /* numeric time format */
	/* variables for path lookup */
	char     *rootpath;                   /* pointer to root path */
	char     tmpfileb[cBcFileLth+1];      /* second scratch file */
	char     xline[cBcLineLth+1];         /* current line of file */
	char     shellcmd[cBcVeryLongStrLth+1];   /* shell command */
	FILE     *fp;                         /* pointer to file */
	int      i;                           /* counter */

	/* executable code */

	descr->stream[0] = '\0';
	descr->t_start[0] = descr->t_end[0] = '\0';
	/* parse database line */
	if  (sscanf(line,"%s %s %c %d %s %d %lf %d %lf %d %d %d %d %d %d %d",
		tstation,tchan,&tcomp,&pathid,relpath,&datea,&timea,&dateb,&timeb,
		&(descr->recno),&(descr->swap_hdr),&(descr->reclth),&(descr->byteoff),
		&(descr->dataflags),&(descr->pri),&(descr->dataformat)) != 16)  {
		*status = SQLE_SFTAB_PARSE;
		return;
	} /*endif*/
	if  (strlen(tstation)+strlen(tchan)+1 > cBcShortStrLth)  {
		*status = SQLE_STROVFL;
		return;
	} /*endif*/
	sprintf( descr->stream, "%s-%s-%c", tstation, tchan, tcomp );
	if  (strcmp(station,tstation) != 0)  return;
	if  (GpGetInt(cGpI_debug_level) > 4) printf( "." );
	if  (GpGetInt(cGpI_debug_level) > 5)
		printf( "SH-dbg6: got line %sSH-dbg6: read times %d,%lf %d,%lf\n",
			line, datea, timea, dateb, timeb );
	ntime.year = datea / 10000;
	ntime.month = (datea % 10000) / 100;
	ntime.day = datea % 100;
	ntime.hour = Nint( timea / 10000.0 - 0.49999 );
	timea -= (double)ntime.hour*10000.0;
	ntime.min = Nint( timea / 100.0 - 0.49999 );
	timea -= (double)ntime.min*100.0;
	ntime.sec = Nint( timea - 0.49999 );
	timea -= (double)ntime.sec;
	ntime.ms = Nint( timea*1000.0 );
	if  (GpGetInt(cGpI_debug_level) > 5)
		printf( "SH-dbg6: start ntime: %d,%d,%d,%d,%d,%d,%d\n", ntime.year,
			ntime.month, ntime.day, ntime.hour, ntime.min, ntime.sec, ntime.ms);
	tc_n2t( &ntime, descr->t_start, status );
	if  (SySevere(status))  return;
	ntime.year = dateb / 10000;
	ntime.month = (dateb % 10000) / 100;
	ntime.day = dateb % 100;
	ntime.hour = Nint( timeb / 10000.0 - 0.49999 );
	timeb -= (double)ntime.hour*10000.0;
	ntime.min = Nint( timeb / 100.0 - 0.49999 );
	timeb -= (double)ntime.min*100.0;
	ntime.sec = Nint( timeb - 0.49999 );
	timeb -= (double)ntime.sec;
	ntime.ms = Nint( timeb*1000.0 );
	if  (GpGetInt(cGpI_debug_level) > 5)
		printf( "SH-dbg6: end ntime: %d,%d,%d,%d,%d,%d,%d\n", ntime.year,
			ntime.month, ntime.day, ntime.hour, ntime.min, ntime.sec, ntime.ms);
	tc_n2t( &ntime, descr->t_end, status );
	if  (SySevere(status))  return;

	/* resolve path, done here for all entries of the db file, */
	/* actually would need only one from the desired file */

	/* first look in cache */
	rootpath = SqlGetRootPath( pathid );
	if  (rootpath == NULL)  {
		/* second temporary file */
		strcpy( tmpfileb, sqlv_sfdb_tmp );
		strcat( tmpfileb, ".000" );
		/* create empty file to reserve name */
		fp = fopen( tmpfileb, "w" );
		fclose( fp );
		/* not in cache, get it from database */
		sprintf( shellcmd,
			"\\rm %s; %s %s \"select rootpath from pathtab where id = \'%d\'\" >%s",
			tmpfileb, GpGetString(cGpS_sfdb_command),
			GpGetString(cGpS_sfdb_exec_qual), pathid, tmpfileb );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SH-dbg3: executing %s\n", shellcmd );
		system( shellcmd );

		/* read output file */
		fp = fopen( tmpfileb, "r" );
		if  (fp == NULL)  {
			*status = SQLE_READTMP;
			err_setcontext( " ## error opening scratch file" );
			return;
		} /*endif*/
		/* read off header */
		fgets( xline, cBcLineLth, fp );
		fgets( descr->name, cBcFileLth, fp );
		i = strlen( descr->name );
		if  (i > 0 && descr->name[i-1] == '\n')  descr->name[i-1] = '\0';
		fclose( fp );
		/* put it to cache */
		SqlCacheRootPath( pathid, descr->name, status );
		sy_fdelete( tmpfileb );
	} else {
		if  (strlen(rootpath) > cBcFileLth)  {
			*status = SQLE_STROVFL;
			return;
		} /*endif*/
		strcpy( descr->name, rootpath );
	} /*endif*/

	if  (strlen(descr->name)+strlen(relpath)+1 > cBcFileLth)  {
		*status = SQLE_STROVFL;
		return;
	} /*endif*/
	strcat( descr->name, "/" );
	strcat( descr->name, relpath );

} /* end of SqlParseSftabLine */



/*---------------------------------------------------------------------*/



void SqlDeleteScratchFile( void )

/* remove scratch file created with SqlUniqueScratchFile
 *
 * no parameters
 */
{
	/* local variables */

	/* executable code */

	if  (*sqlv_sfdb_tmp != '\0')
		sy_fdelete( sqlv_sfdb_tmp );

} /* end of SqlDeleteScratchFile */



/*---------------------------------------------------------------------*/



static char *SqlGetRootPath( int pathid )

/* returns path in cache or NULL
 *
 * parameters of routine
 * int        pathid;     input; path ID
 */
{
	/* local variables */
	SqlPathCacheT *pc;            /* moving pointer */
	FILE     *fp;                 /* pointer to input file */
	char     line[cBcLineLth+1];  /* current line of file */
	int      id;                  /* current path id */
	static char rp[cBcLineLth+1]; /* current root path */

	/* executable code */

	pc = sqlv_pathcache;
	while  (pc != NULL)  {
		if  (pathid == pc->path_id)
			return (pc->rpath);
		pc = pc->next;
	} /*endwhile*/

	/* look also in cache file if defined */
	if  (*sqlv_cache_file != '\0')  {
		fp = fopen( sqlv_cache_file, "r" );
		if  (fp == NULL)  return NULL;
		while  (fgets(line,cBcLineLth,fp) != NULL)  {
			if  (sscanf(line,"%d %s",&id,rp) != 2)  continue;
			if  (id == pathid)  return rp;
		} /*endwhile*/
		fclose( fp );
	} /*endif*/

	return NULL;

} /* end of SqlGetRootPath */



/*---------------------------------------------------------------------*/



static void SqlCacheRootPath( int pathid, char rootpath[], TSyStatus *status )

/* Puts root path to cache
 *
 * parameters of routine
 * int        pathid;      input; ID number of path
 * char       rootpath[];  input; value of rootpath
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	SqlPathCacheT  *pc;     /* pointer to path cache element */
	SqlPathCacheT  *m;      /* moving pointer */

	/* executable code */

	pc = (SqlPathCacheT *)sy_allocmem( 1, (int)sizeof(SqlPathCacheT), status );
	if  (SySevere(status))  return;

	pc->next = NULL;
	pc->rpath = (char *)sy_allocmem( strlen(rootpath)+1, (int)sizeof(char), status );
	if  (SySevere(status))  return;
	strcpy( pc->rpath, rootpath );
	pc->path_id = pathid;

	if  (sqlv_pathcache == NULL)  {
		sqlv_pathcache = pc;
	} else {
		m = sqlv_pathcache;
		while  (m->next != NULL)
			m = m->next;
		m->next = pc;
	} /*endif*/

} /* end of SqlCacheRootPath */



/*----------------------------------------------------------------------------*/



void SqlPathcacheFile( char fname[], TSyStatus *status )

/* Sets path cache file.  Used if a rootpath cache is created externally
 * and passed to the read routine to save calls to database (e.g. on
 * single calls to copy_recs)
 *
 * parameters of routine
 * char       fname[];     input; name of pathcache
 */
{
	/* local variables */

	/* executable code */

	if  (strlen(fname) > cBcFileLth)  {
		*status = SQLE_STROVFL;
		return;
	} /*endif*/
	strcpy( sqlv_cache_file, fname );

} /* end of SqlPatchcacheFile */



/*----------------------------------------------------------------------------*/



void SqlTranslateStationList( char name[], int maxlth, char list[],
	TSyStatus *status )

/* translates a station list name into the station list
 *
 * parameters of routine
 * char       name[];     input; name of list
 * int        maxlth;     input, maximum length of output string
 * char       list[];     output; station list
 * TSyStatus  *status;    output; return status
 */
{
	/* local variables */
	char     *lptr;                      /* pointer to station list */
	int      i;                          /* counter */
	char     scrfil[cBcFileLth+1];       /* scratch file name */
	FILE     *fp;                        /* pointer to scratch file */
	char     sql[cBcVeryLongStrLth+1];   /* SQL command */
	char     line[cBcLineLth+1];         /* current line of file */
	int      lth;                        /* length of list string */
	int      slth;                       /* length of current string */
	char     *namptr;                    /* name pointer */
	int      flagval;                    /* flag value */

	/* executable code */

	namptr = name;
	/* try to get it from cache */
	lptr = SqlGetStationList( name );

	/* if not there get station list from database */
	if  (lptr == NULL)  {

		/* first get another unique scratch file name and create it */
		i = 1;
		FOREVER  {
			sprintf( scrfil, "/tmp/sql_slist_%d.000", i++ );
			fp = fopen( scrfil, "r" );
			if  (fp == NULL)  break;
			fclose( fp );
		} /*endfor*/
		fp = fopen( scrfil, "w" );
		if  (fp == NULL)  {
			*status = SQLE_CREATETMP;
		} else {
			fclose( fp );
		} /*endif*/

		/* skip db: in list name if there */
		if  (strncmp(namptr,"db:",3) == 0
			|| strncmp(namptr,"DB:",3) == 0)  namptr += 3;

		if  (strcmp(namptr,"all") == 0 || strcmp(namptr,"ALL") == 0)  {
			/* get really all stations from database */
			sprintf( sql, "\\rm %s; %s %s \"select station from staadm order by netpri,station\" >%s",
				scrfil, GpGetString(cGpS_sfdb_command),
				GpGetString(cGpS_sfdb_exec_qual), scrfil );
		} else if  (strncmp(namptr,"net:",4) == 0 || strncmp(namptr,"NET:",4) == 0)  {
			/* get stations of a net from database */
			namptr += 4;
			sprintf( sql, "\\rm %s; %s %s \"select station from staadm where netpri='%s' order by netpri,station\" >%s",
				scrfil, GpGetString(cGpS_sfdb_command),
				GpGetString(cGpS_sfdb_exec_qual), namptr, scrfil );
		} else {

			/* assume name is a attribute name, request value */
			sprintf( sql, "\\rm %s; %s %s \"select attrflag from staattr where attrname='%s'\" >%s",
				scrfil, GpGetString(cGpS_sfdb_command),
				GpGetString(cGpS_sfdb_exec_qual), namptr, scrfil );
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SH-dbg3: execute %s\n", sql );
			system( sql );
			/* read list from scratch file */
			fp = fopen( scrfil, "r" );
			if  (fp == NULL)  {
				*status = SQLE_READTMP;
				sy_fdelete( scrfil );
				return;
			} /*endif*/
			/* read off header */
			fgets( line, cBcLineLth, fp );
			/* read value */
			fgets( line, cBcLineLth, fp );
			fclose( fp );
			if  (sscanf(line,"%d",&flagval) != 1)  {
				*status = SQLE_READTMP;
				sy_fdelete( scrfil );
				return;
			} /*endif*/

			/* get flagged stations from database */
			sprintf( sql, "\\rm %s; %s %s \"select station from staadm where staflags&%d order by netpri,station\" >%s",
				scrfil, GpGetString(cGpS_sfdb_command),
				GpGetString(cGpS_sfdb_exec_qual), flagval, scrfil );

		} /*endif*/

		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SH-dbg3: execute %s\n", sql );
		system( sql );

		/* read list from scratch file */
		fp = fopen( scrfil, "r" );
		if  (fp == NULL)  {
			*status = SQLE_READTMP;
			sy_fdelete( scrfil );
			return;
		} /*endif*/
		lth = 0;
		*list = '\0';
		/* read off header */
		fgets( line, cBcLineLth, fp );

		/* if nothing found or left, terminate */
		if  (feof(fp))  {
			fclose( fp );
			/* cannot translate, this probably causes an error */
			if  (strlen(name) > maxlth)  {
				*status = SQLE_STROVFL;
			} else {
				strcpy( list, name );
			} /*endif*/
			sy_fdelete( scrfil );
			return;
		} /*endif*/

		while  (fgets(line,cBcLineLth,fp) != NULL)  {
			slth = strlen( line ) - 1;
			if  (line[slth] == '\n')  line[slth] = '\0'; else slth++;
			if  (lth+slth+1 > maxlth)  {
				fclose( fp );
				*status = SQLE_STROVFL;
				sy_fdelete( scrfil );
				return;
			} /*endif*/
			if  (lth > 0)  {
				strcat( list, "," );
				lth++;
			} /*endif*/
			strcat( list, line );
			lth += slth;
		} /*endwhile*/
		fclose( fp );

		sy_fdelete( scrfil );

		SqlCacheStationList( name, list, status );
		if  (SySevere(status))  return;

	} else {

		if  (strlen(lptr) > maxlth)  {
			*status = SQLE_STROVFL;
			return;
		} /*endif*/
		strcpy( list, lptr );

	} /*endif*/


} /* end of SqlTranslateStationList */



/*----------------------------------------------------------------------------*/



static char *SqlGetStationList( char name[] )

/* returns station list in cache or NULL
 *
 * parameters of routine
 * char       name[];     input; name of station list
 */
{
	/* local variables */
	SqlStationCacheT *pc;  /* moving pointer */

	/* executable code */

	pc = sqlv_stacache;
	while  (pc != NULL)  {
		if  (strcmp(name,pc->listname) == 0)
			return (pc->listval);
		pc = pc->next;
	} /*endwhile*/

	return NULL;

} /* end of SqlGetStationList */



/*---------------------------------------------------------------------*/



static void SqlCacheStationList( char name[], char stalist[], TSyStatus *status )

/* Puts root path to cache
 *
 * parameters of routine
 * char       name[];      input; name of station list
 * char       stalist[];   input; value of station list
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	SqlStationCacheT  *pc;     /* pointer to path cache element */
	SqlStationCacheT  *m;      /* moving pointer */

	/* executable code */

	pc = (SqlStationCacheT *)sy_allocmem( 1, (int)sizeof(SqlStationCacheT), status );
	if  (SySevere(status))  return;

	pc->next = NULL;
	pc->listval = (char *)sy_allocmem( strlen(stalist)+1, (int)sizeof(char), status );
	if  (SySevere(status))  return;
	strcpy( pc->listval, stalist );
	pc->listname = (char *)sy_allocmem( strlen(name)+1, (int)sizeof(char), status );
	if  (SySevere(status))  return;
	strcpy( pc->listname, name );

	if  (sqlv_stacache == NULL)  {
		sqlv_stacache = pc;
	} else {
		m = sqlv_stacache;
		while  (m->next != NULL)
			m = m->next;
		m->next = pc;
	} /*endif*/

} /* end of SqlCacheStationList */



/*----------------------------------------------------------------------------*/

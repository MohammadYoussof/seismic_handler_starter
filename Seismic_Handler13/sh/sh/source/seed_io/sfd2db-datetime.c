
/* file sfd2db.c
 *      ========
 *
 * version 3, 12-Nov-2006
 *
 * reads lines from sfdfile and creates commands to inster into sfdb database
 * K. Stammler, 4-Nov-2006
 */


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include "sysbase.h"
#include "cpar.h"
#include "tcusrdef.h"
#include "seedcfg.h"
#include "seed_lib.h"


int main( int argc, char *argv[] )
{
	/* local variables */
	int      priority;                  /* priority of entry */
	char     fname[cBcFileLth+1];       /* name of input file */
	FILE     *fp;                       /* pointer to input file */
	FILE     *out;                      /* pointer to output file */
	char     line[cBcLineLth+1];        /* current line of file */
	SeedFileDescrT descr;               /* seed file descriptor */
	TSyStatus status;                   /* return status */
	NTIME    stime, etime;              /* start time and end time */
	char     station[cBcShortStrLth+1]; /* station name */
	char     chan[cBcShortStrLth+1];    /* channel name */
	char     comp;                      /* component name */
	int      i;                         /* counter */
	char     rpath[cBcFileLth+1];       /* relative path name */
	int      pathid;                    /* path ID */
	char     abspath[cBcFileLth+1];     /* path to sfdfile */
	char     sfdbreq[cBcLineLth+1];     /* request command to sfdb database */
	char     *env;                      /* pointer to environment */
	char     shellcmd[cBcLongStrLth+1]; /* shell command */
	char     tmpfile[cBcFileLth+1];     /* temporary file */
	int      pathcnt;                   /* path counter */
	int      sfcnt;                     /* seed file counter */
	TSyBoolean pipe_io;                 /* input/output on stdin/stdout */
	TSyBoolean pathid_set;              /* path ID set on command line */
	TSyBoolean update;                  /* produce also update code */
	TSyBoolean invhdr;                  /* invert header info in sfd entry */

	/* executable code */

	status = cBcNoError;
	pa_init( argc, argv );

	if  (pa_pnumber() < 2 || pa_pnumber() > 3)  {
		fprintf( stderr, "Usage: %s <inpfile> <priority> [<pathid>]\n", argv[0] );
		return 1;
	} /*endif*/

	update = pa_qspecified( "-u" );
	invhdr = pa_qspecified( "-invhdr" );

	env = (char *)getenv( "SFDBREQ" );
	if  (env == NULL)  {
		strcpy( sfdbreq, "mysql sfdb -B -e" );
	} else {
		if  (strlen(env) > cBcLineLth)  {
			fprintf( stderr, "%s: SFDBREQ too long.  Abort.\n", argv[0] );
			return 1;
		} /*endif*/
		strcpy( sfdbreq, env );
	} /*endif*/

	/* temporary file */
	i = 1;
	FOREVER  {
		sprintf( tmpfile, "/tmp/sfd2db_%d.000", i++ );
		fp = fopen( tmpfile, "r" );
		if  (fp == NULL)  break;
		fclose( fp );
	} /*endfor*/
	/* create empty file to reserve name */
	fp = fopen( tmpfile, "w" );
	fclose( fp );

	/* get parameters */
	if  (strlen(pa_pvalue(1)) > cBcFileLth)  {
		fprintf( stderr, "%s: name too long.  Abort\n", argv[0] );
		return 1;
	} /*endif*/
	strcpy( fname, pa_pvalue(1) );
	if  (sscanf(pa_pvalue(2),"%d",&priority) != 1)  {
		fprintf( stderr, "%s: error reading priority.  Abort.\n", argv[0] );
		return 1;
	} /*endif*/
	pathid = -1;
	pathid_set = FALSE;
	if  (pa_pnumber() > 2)  {
		if  (sscanf(pa_pvalue(3),"%d",&pathid) != 1)  {
			fprintf( stderr, "%s: error reading pathid.  Abort.\n", argv[0] );
			return 1;
		} /*endif*/
		pathid_set = TRUE;
	} /*endif*/

	/* find last "/" */
	i = strlen( fname );
	while  (fname[i] != '/' && i > 0)
		i--;
	if  (fname[i] == '/')  {
		strncpy( abspath, fname, i );
		abspath[i] = '\0';
	} else {
		*abspath = '\0';
	} /*endif*/

	pathcnt = sfcnt = 0;

	if  (pathid == -1)  {
		/* get ID number for absolute path */
		/* (1) check whether path is already inserted */
		if  (strlen(sfdbreq)+strlen(abspath)+(2*strlen(tmpfile))+50 > cBcLongStrLth)  {
			fprintf( stderr, "%s: string overflow on shell command.  Abort.\n", argv[0] );
			return 1;
		} /*endif*/
		sprintf( shellcmd,
			"\\rm %s; %s \"select id from pathtab where rpath = \'%s\'\" >%s",
			tmpfile, sfdbreq, abspath, tmpfile );
		/*printf( "--> executing: %s\n", shellcmd );*/
		system( shellcmd );
		/* (2) read output file of sql command */
		fp = fopen( tmpfile, "r" );
		if  (fp == NULL)  {
			fprintf( stderr, "%s: open error on tmpfile %s.  This cannot happen.\n",
				argv[0], tmpfile );
			return 1;
		} /*endif*/
		/* read off header */
		fgets( line, cBcLineLth, fp );
		/* read result */
		if  (fgets( line, cBcLineLth, fp ) == NULL)  {
			pathid = -1;
		} else {
			if  (sscanf(line,"%d",&pathid) != 1)
				pathid = -1;
		} /*endif*/
		fclose( fp );
		/* (3) if not already inserted get highest id number and increment it */
		if  (pathid == -1)  {
			/* make new path entry */
			sprintf( shellcmd,
				"\\rm %s; %s \"select max(id) as id from pathtab;\" >%s",
				tmpfile, sfdbreq, tmpfile );
			/*printf( "--> executing %s\n", shellcmd );*/
			system( shellcmd );
			/* (4) read output file of sql command */
			fp = fopen( tmpfile, "r" );
			if  (fp == NULL)  {
				fprintf( stderr, "%s: open error on tmpfile %s.  This cannot happen.\n",
					argv[0], tmpfile );
				return 1;
			} /*endif*/
			/* read off header */
			fgets( line, cBcLineLth, fp );
			/* read result */
			if  (fgets( line, cBcLineLth, fp ) == NULL)  {
				pathid = 1;
			} else {
				if  (sscanf(line,"%d",&pathid) == 1)  {
					pathid++;
				} else {
					pathid = 1;
				} /*endif*/
			} /*endif*/
			fclose( fp );
			sprintf( shellcmd, "%s \"insert into pathtab values ( \'%d\', \'%s\' )\"",
				sfdbreq, pathid, abspath );
			/*printf( "--> executing %s\n", shellcmd );*/
			system( shellcmd );
			pathcnt++;
		} /*endif*/
	} /*endif*/
	/*printf( "--> pathid: %d\n", pathid );*/

	/* open input file */
	if  (strcmp(fname,"TT") == 0 || strcmp(fname,"tt") == 0)  {
		pipe_io = TRUE;
		fp = stdin;
	} else {
		pipe_io = FALSE;
		fp = fopen( fname, "r" );
		if  (fp == NULL)  {
			fprintf( stderr, "%s: input file %s not found.  Abort.\n",
				argv[0], fname );
			return 1;
		} /*endif*/
	} /*endif*/

	/* open output file */
	if  (pipe_io)  {
		out = stdout;
	} else {
		out = fopen( tmpfile, "w" );
		if  (out == NULL)  {
			fprintf( stderr, "%s: error opening scratch file %s for output. Cannot happen.\n",
				argv[0], tmpfile );
			return 1;
		} /*endif*/
	} /*endif*/

	/* read through input file */
	while  (fgets(line,cBcLineLth,fp) != NULL)  {

		/* read and parse next line */
		status = cBcNoError;
		SeedParseSfdLine( line, &descr, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error decoding line %s\n", argv[0], line );
			continue;
		} /*endif*/
		if  (invhdr)  descr.swap_hdr = !descr.swap_hdr;

		/* parse stream string */
		if  (strlen(descr.stream) > cBcShortStrLth)  {
			fprintf( stderr, "%s: stream too long: %s\n", argv[0], line );
			continue;
		} /*endif*/
		for  (i=0; descr.stream[i] != '\0'; i++)
			if  (descr.stream[i] == '-')  descr.stream[i] = ' ';
		i = sscanf( descr.stream, "%s %s %c", station, chan, &comp );
		if  (i < 3)  comp = ' ';
		if  (i < 2)  strcpy( chan , "  " );
		if  (i < 1)  strcpy( station , "   " );

		/* parse path */
		if  (strlen(descr.name) > cBcFileLth)  {
			fprintf( stderr, "%s: file path too long: %s\n", argv[0], line );
			continue;
		} /*endif*/
		if  (strncmp(descr.name,"$ROOT/",6) == 0)  {
			strcpy( rpath, descr.name+6 );
		} else {
			strcpy( rpath, descr.name );
			if  (!pathid_set)  pathid = 0;
		} /*endif*/

		/* convert times to integers */
		tc_t2n( descr.t_start, &stime, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error decoding start time %s\n", argv[0], line );
			continue;
		} /*endif*/
		tc_t2n( descr.t_end, &etime, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error decoding end time %s\n", argv[0], line );
			continue;
		} /*endif*/

		/* write command line */
		fprintf( out,
			"insert ignore into sftab values ( \"%s\", \"%s\", \"%c\", \"%d\", \"%s\", \"%4d-%02d-%02d %02d:%02d:%02d\", \"%d\", \"%4d-%02d-%02d %02d:%02d:%02d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\", \"%d\" );\n",
			station, chan, comp, pathid, rpath, stime.year, stime.month, stime.day,
			stime.hour, stime.min, stime.sec, stime.ms, etime.year, etime.month,
			etime.day, etime.hour, etime.min, etime.sec, etime.ms, descr.recno,
			descr.swap_hdr, descr.reclth, descr.byteoff, descr.dataflags, priority );
		if  (update)
			fprintf( out,
				"update ignore sftab set stime=\"%4d-%02d-%02d %02d:%02d:%02d\", sms=\"%d\", etime=\"%4d-%02d-%02d %02d:%02d:%02d\", ems=\"%d\", recnum=\"%d\", dataflags=\"%d\", priority=\"%d\", hswap=\"%d\" where relpath=\"%s\";\n",
				stime.year, stime.month, stime.day, stime.hour, stime.min,
				stime.sec, stime.ms, etime.year, etime.month, etime.day, etime.hour,
				etime.min, etime.sec, etime.ms, descr.recno, descr.dataflags,
				priority, descr.swap_hdr, rpath );
		sfcnt++;

	} /*endwhile*/

	if  (!pipe_io)  {
		fclose( fp );
		fclose( out );

		/*sprintf( shellcmd, "cat %s", tmpfile );*/
		/*system( shellcmd );*/
		sprintf( shellcmd, "mysql sfdb <%s", tmpfile );
		/*printf( "--> executing %s\n", shellcmd );*/
		system( shellcmd );
	} /*endif*/

	sy_fdelete( tmpfile );

	if  (!pipe_io)
		printf( "%d path(s) and %d file(s) inserted\n", pathcnt, sfcnt );

	return 0;

} /* end of main */

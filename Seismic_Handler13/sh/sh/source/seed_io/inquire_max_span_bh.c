
/* file inquire_max_span_bh.c
 *      =====================
 *
 * version 1, 27-Feb-96
 *
 * returns maximum time span in sfd-file of all bh-channels,
 * i.e. first begin time and
 * last end time of any stream in the file (independently of each other).
 * The main part of the code is taken from inquire_max_span.c
 *
 * K. Stammler, 27-Feb-96
 */




#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include "sysbase.h"
#include "seedcfg.h"
#include "seed_lib.h"
#include "cpar.h"
#include "tcusrdef.h"
#include "erusrdef.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char     sfdfile[cBcLineLth+1];      /* name of sfd file */
	FILE     *sfd;                       /* pointer to sfd file */
	char     line[Seed_C_SFDLINELTH+1];  /* one line of sfd file */
	TSyStatus status;                    /* return status */
	SeedFileDescrT fd;                   /* seed file descriptor */
	char     min_start[cBcTimeLth+1];    /* earliest start time */
	char     max_end[cBcTimeLth+1];      /* latest end time */
	char     l_stream[cBcShortStrLth+1]; /* copy of stream string */
	char     channel[cBcShortStrLth+1];  /* channel string */
	char     tmp[cBcShortStrLth+1];      /* dummy */

	/* executable code */

	status = cBcNoError;

	/* check parametrs and get it */
	pa_init( argc, argv );
	if  (pa_pnumber() != 1)  {
		fprintf( stderr, "Usage: %s <sfdfile>\n", pa_progname() );
		return 1;
	} /*endif*/
	strcpy( sfdfile, argv[1] );

	/* open sfd file */
	if  (strcmp(sfdfile,"stdin") == 0)  {
		sfd = stdin;
	} else {
		sfd = sy_fopen( sfdfile, "r" );
		if  (sfd == NULL)  {
			fprintf( stderr, "%s: error opening input file %s\n",
				pa_progname(), sfdfile );
			return 1;
		} /*endif*/
	} /*endif*/

	/* loop all lines of sfd file */
	*min_start = *max_end = '\0';
	while  (fgets(line,Seed_C_SFDLINELTH,sfd) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		SeedParseSfdLine( line, &fd, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		strcpy( l_stream, fd.stream );
		SeedPrepareStreamString( l_stream );
		sscanf( l_stream, "%s %s", tmp, channel );
		if  (strcmp(channel,"bh") != 0)  continue;
		if  (*min_start == '\0')  {
			strcpy( min_start, fd.t_start );
			strcpy( max_end, fd.t_end );
		} else {
			if  (tc_tdiff(fd.t_start,min_start,&status) < 0.0)
				strcpy( min_start,fd.t_start );
			if  (tc_tdiff(max_end,fd.t_end,&status) < 0.0)
				strcpy( max_end, fd.t_end );
		} /*endif*/
	} /*endwhile*/

	if  (sfd != stdin)  sy_fclose( sfd );

	printf( "%s %s\n", min_start, max_end );

	return 0;

} /* end of main */

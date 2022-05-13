
/* file inquire_min_span.c
 *      ==================
 *
 * version 1, 18-Sep-95
 *
 * returns minimum time span in sfd-file, i.e. last begin time and
 * first end time of any stream in the file (independently of each other).
 *
 * K. Stammler, 18-Sep-95
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
	char     max_start[cBcTimeLth+1];    /* latest start time */
	char     min_end[cBcTimeLth+1];      /* earliest end time */

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
	*max_start = *min_end = '\0';
	while  (fgets(line,Seed_C_SFDLINELTH,sfd) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		SeedParseSfdLine( line, &fd, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		if  (*max_start == '\0')  {
			strcpy( max_start, fd.t_start );
			strcpy( min_end, fd.t_end );
		} else {
			if  (tc_tdiff(fd.t_start,max_start,&status) > 0.0)
				strcpy( max_start,fd.t_start );
			if  (tc_tdiff(min_end,fd.t_end,&status) > 0.0)
				strcpy( min_end, fd.t_end );
		} /*endif*/
	} /*endwhile*/

	if  (sfd != stdin)  sy_fclose( sfd );

	printf( "%s %s\n", max_start, min_end );

	return 0;

} /* end of main */


/* file inquire_time_span.c
 *      ===================
 *
 * version 2, 20-Jan-97
 *
 * returns availability of specified time span
 * K. Stammler, 3-Mar-95
 */



#include <stdio.h>
#include <string.h>
#include BASECNST
#include BC_SYSBASE
#include "seedcfg.h"
#include "seed_lib.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char     sfdfile[BC_FILELTH+1];     /* sfd file */
	char     stream[BC_SHORTSTRLTH+1];  /* stream string */
	char     t_start[BC_TIMELTH+1];     /* start time */
	char     t_end[BC_TIMELTH+1];       /* end time */
	TSyBoolean avail;                   /* time span available */
	TSyStatus status;                   /* return status */

	/* executable code */

	if  (argc != 5)  {
		fprintf( stderr, "Usage: %s <sfdfile> <stream> <start> <end>\n", argv[0]);
		return 1;
	} /*endif*/

	strcpy( sfdfile, argv[1] );
	strcpy( stream, argv[2] );
	strcpy( t_start, argv[3] );
	strcpy( t_end, argv[4] );

	status = cBcNoError;
	avail = SeedInquireTime( sfdfile, stream, t_start, t_end, &status );
	if  (SySevere(&status))  {
		fprintf( stderr, "%s: error in SeedInquireTime (status %d)\n",
			argv[0], status );
		return 1;
	} /*endif*/

	if  (avail)  {
		printf( "yes\n" );
	} else {
		printf( "no\n" );
	} /*endif*/

} /* end of main */

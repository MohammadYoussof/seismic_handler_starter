
/* file inquire_avd.c
 *      =============
 *
 * version 1, 15-Mar-95
 *
 * returns available time span for specified stream
 * K. Stammler, 15-Mar-95
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

	if  (argc != 3)  {
		fprintf( stderr, "Usage: %s <sfdfile> <stream>\n", argv[0]);
		return 1;
	} /*endif*/

	strcpy( sfdfile, argv[1] );
	strcpy( stream, argv[2] );

	status = cBcNoError;
	SeedGetTimeSpan( sfdfile, stream, t_start, t_end, &status );
	if  (SySevere(&status))  {
		fprintf( stderr, "%s: error in SeedGetTimeSpan\n", argv[0] );
		return 1;
	} /*endif*/

	printf( "%s %s\n", t_start, t_end );

	return 0;

} /* end of main */

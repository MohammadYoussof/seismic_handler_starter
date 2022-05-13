
/* file lookup_cd.c
 *      ===========
 *
 * version 5, 14-Aug-97
 *
 * returns label of CD
 * K. Stammler, 8-May-95
 */




#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include "sysbase.h"
#include "erusrdef.h"
#include "cpar.h"
#include "seed_cd.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char     ctime[cBcLineLth+1];     /* time to find */
	char     stream[cBcLineLth+1];    /* stream name */
	char     jkpath[cBcLineLth+1];    /* jukebox root path */
	char     label[cBcLineLth+1];     /* label of CD */
	char     magic[cBcLineLth+1];     /* magic string to find in sfd-file */
	char     msg[cBcLineLth+1];       /* error message */
	TSyStatus status;                 /* return status */

	/* executable code */

	status = cBcNoError;
	pa_init( argc, argv );

	if  (pa_pnumber() != 2)  {
		fprintf( stderr, "Usage: %s <stream> <time>\n", pa_progname() );
		return 1;
	} /*endif*/

	strcpy( stream, pa_pvalue(1) );
	strcpy( ctime, pa_pvalue(2) );
	CdFindLabel( stream, ctime, jkpath, label, magic, &status );
	if  (status == sCdONLINE)  {
		printf( "online DATA-ARE-ONLINE\n" );
		return 0;
	} else if  (status == sCdNOT_FOUND)  {
		printf( "unavailable DATA-ARE-NOT-AVAILABLE\n" );
		return 0;
	} else if  (SySevere(&status))  {
		err_msg( status, msg );
		fprintf( stderr, "%s\n", msg );
		err_getcontext( msg );
		if  (*msg != '\0')  fprintf( stderr, "%s\n", msg );
		return 1;
	} /*endif*/

	printf( "%s %s %s\n", jkpath, label, magic );

	return 0;

} /* end of main */

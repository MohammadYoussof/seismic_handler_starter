
/* file gsetest.c
 *      =========
 *
 * version 1, 8-Dec-94
 *
 * test module for gsedata
 * K. Stammler, 8-Dec-94
 */


#include <stdio.h>
#include <string.h>
#include BASECNST
#include BC_SYSBASE
#include "gsedata.h"


int main( void )
{
	/* local variables */
	FILE     *fp;           /* pointer to file */
	SGseTrace gsetrc;
	TSyStatus status;

	/* executable code */

	status = cBcNoError;

	fp = fopen( "/tmp01/klaus/2066.data", "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "error opening file\n" );
		return 1;
	} /*endif*/

	status = cBcNoError;
	FOREVER  {
		GseReadTrace( fp, &gsetrc, &status );
		if  (SySevere(&status))  break;

	} /*endfor*/

	fclose( fp );
	return 0;

} /* end of main */

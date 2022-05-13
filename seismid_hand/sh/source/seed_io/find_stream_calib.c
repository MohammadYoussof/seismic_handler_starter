
/* file find_stream_calib.c
 *      ===================
 *
 * version 1, 12-Mar-96
 *
 * returns calibration of stream at a given time
 * K. Stammler, 12-Mar-96
 */



#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include "sysbase.h"
#include "cpar.h"
#include "erusrdef.h"
#include "seedcfg.h"
#include "seed_lib.h"


int main( int argc, char *argv[] )
{
	/* local variables */
	STATUS   status;         /* return status */
	float    calib;          /* calibration value */

	/* executable code */

	pa_init( argc, argv );
	if  (pa_pnumber() != 2)  {
		fprintf( stderr, "Usage: %s <stream> <time>\n", pa_progname() );
		return 1;
	} /*endif*/

	status = cBcNoError;
	calib = SeedFindStreamCalibration( argv[1], argv[2], &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );
	printf( "%e\n", calib );

	return 0;

} /* end of main */

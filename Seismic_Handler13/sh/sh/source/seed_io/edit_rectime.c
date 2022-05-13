
/* file split_seed.c
 *      ============
 *
 * version 1, 15-Jul-97
 *
 * Writes first record of input file to stdout with changed time
 * K. Stammler, 15-Jul-97
 */


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include "sysbase.h"
#include "cpar.h"
#include "erusrdef.h"
#include "seedcfg.h"
#include "seed_lib.h"



/* global variables */
static SeedSetupParT seedv_setup;       /* SEED setup parameters */



int main( int argc, char *argv[] )
{
	/* local variables */
	TSyStatus status;                   /* return status */
	SeedSbyteT *seedrec;                /* pointer to SEED record */
	SeedDataHeaderT *seedhdr;           /* pointer to seed data header */
	char     inpfile[cBcFileLth+1];     /* name of input file */
	char     newtime[cBcLineLth+1];     /* new time for record */
	FILE     *fp;                       /* file pointer */
	int      read_ret;                  /* return value of fread */
	NTIME    ntime;                     /* numeric time */

	/* executable code */

	status = cBcNoError;

	/* get parameters */
	pa_init( argc, argv );
	if  (pa_pnumber() != 2)  {
		fprintf( stderr, "*** Usage: %s <inpfile> <newtime> ***\n", argv[0] );
		return 1;
	} /*endif*/
	strcpy( inpfile, pa_pvalue(1) );
	strcpy( newtime, pa_pvalue(2) );

	/* convert time to ntime */
	tc_t2n( newtime, &ntime, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* get setup parameters */
	SeedSetup( &seedv_setup, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* allocate memory for SEED record */
	seedrec = (SeedSbyteT *)sy_allocmem( 1, seedv_setup.seed_rec_size,
		&status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	seedhdr = (SeedDataHeaderT *)seedrec;

	/* open input file */
	fp = fopen( inpfile, "rb" );
	if  (fp == NULL)  {
		fprintf( stderr, "*** %s: file %s not found\n", pa_progname(), inpfile );
		return 1;
	} /*endif*/

	/* read first record of file and close file */	
	read_ret = fread( (char *)seedrec, seedv_setup.seed_rec_size, 1, fp );
	if  (read_ret != 1)  {
		fprintf( stderr, "*** %s: read error on file %s\n",
			pa_progname(), inpfile );
		fclose( fp );
		return 1;
	} /*endif*/
	fclose( fp );

	/* change time */
	SeedNtimeToBtime( &ntime, &(seedhdr->starttime), &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* write out record */
	fwrite( (char *)seedrec, seedv_setup.seed_rec_size, 1, stdout );

	return 0;

} /* end of main */

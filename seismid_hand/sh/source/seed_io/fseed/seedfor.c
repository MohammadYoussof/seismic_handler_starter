
/* file seedfor.c
 *      =========
 *
 * version 2, 30-Dec-97
 *
 * FORTRAN interface to SEED reader
 * K. Stammler, 11-Nov-94
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "basecnst.h"
#include "sysbase.h"
#include "seedcfg.h"
#include "seed_lib.h"


/* prototypes of local routines */
static void do_abort( STATUS status );



/*---------------------------------------------------------------------------*/



void f_seed_init_( void )

/* initializes Seed library
 *
 * no parameters
 */
{
	/* local variables */
	SeedSetupParT setup;               /* SEED setup */
	STATUS   status;         /* return status */

	/* executable code */

	status = BC_NOERROR;
	SeedSetup( &setup, &status );
	if  (Severe(&status))  do_abort( status );
	SeedLibInitialize( &status );
	if  (Severe(&status))  do_abort( status );

} /* end of f_seed_init_ */



/*---------------------------------------------------------------------*/



void f_seed_read_( int *chan, char sfdfile[], char stream[], BOOLEAN *swap,
	char starttime[], float *seclength, float *fdat, int *maxlen,
	int *smplth, char actstart[], float *dt, float *calib, int *status )

/* reads data into 'fdat'
 *
 * parameters of routine
 */
{
	/* local variables */
	float    *ftmp;         /* data pointer */
	int      i;             /* counter */
	int      max;           /* max length */
	char     *cptr;         /* pointer to char */

	/* executable code */

	cptr = sfdfile;
	while  (*cptr != ' ' && *cptr != '\0')  cptr++;
	*cptr = '\0';
	cptr = stream;
	while  (*cptr != ' ' && *cptr != '\0')  cptr++;
	*cptr = '\0';
	cptr = starttime;
	while  (*cptr != ' ' && *cptr != '\0')  cptr++;
	*cptr = '\0';

	SeedReadStreamReal( *chan, sfdfile, stream, *swap, starttime, *seclength,
		&ftmp, smplth, actstart, dt, calib, status );
	if  (SySevere(status))  return;

	if  (*smplth <= *maxlen)  {
		max = *smplth;
	} else {
		fprintf( stderr, "trace truncated\n" );
		max = *maxlen;
		*smplth = max;
	} /*endif*/
	for  (i=0; i<max; i++)
		fdat[i] = ftmp[i];

	free( ftmp );

} /* end of f_seed_read_ */



/*---------------------------------------------------------------------*/



static void do_abort( STATUS status )

/* Aborts execution
 *
 * parameters of routine
 * STATUS     status;      input; error status
 */
{
	/* executable code */

	printf( "*** Abort.   Status: %d\n", status );
	exit( 1 );

} /* end of do_abort */



/*---------------------------------------------------------------------------*/

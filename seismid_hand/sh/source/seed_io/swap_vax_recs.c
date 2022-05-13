
/* file swap_vax_recs.c
 *      ===============
 *
 * version 2, 22-Dec-94
 *
 * swaps Mini-SEED Files from VAX
 * swaps header info of (U)WORD and LONG type
 * swaps complete data section assuming ULONG entries
 * K. Stammler, 18-Nov-94
 */


#include <stdio.h>
#include <string.h>
#include BASECNST
#include BC_SYSBASE
#include BC_CPAR
#include BC_TCUSRDEF
#include BC_ERUSRDEF
#include "seedcfg.h"
#include "seed_lib.h"



/* global variables */
static SeedSetupParT seedv_setup;       /* SEED setup parameters */



int main( int argc, char *argv[] )
{
	/* local variables */
	STATUS   status;                    /* return status */
	SeedSbyteT *seedrec;                /* pointer to SEED record */
	SeedDataHeaderT *seedhdr;           /* pointer to seed data header */
	char     inpfile[BC_FILELTH+1];     /* name of input file */
	char     outfile[BC_FILELTH+1];     /* name of output file */
	FILE     *inp, *out;                /* file pointers */
	int      fsize;                     /* size of file in bytes */
	int      numrecs;                   /* number of SEED records */
	int      i, j;                      /* counter */
	int      read_ret;                  /* return value of fread */
	int      write_ret;                 /* return value of fwrite */
	int      long_lth;                  /* length of record in longs */
	unsigned long *ulptr;               /* long pointer */
	unsigned long ultmp;                /* scratch */

	/* executable code */

	status = BC_NOERROR;

	pa_init( argc, argv );
	if  (pa_pnumber() != 2)  {
		fprintf( stderr, "Usage: %s <inpfile> <outfile> ***\n",
			argv[0] );
		return 1;
	} /*endif*/
	strcpy( inpfile, pa_pvalue(1) );
	strcpy( outfile, pa_pvalue(2) );

	/* get other parameters */
	SeedSetup( &seedv_setup, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* allocate memory for SEED record */
	seedrec = (SeedSbyteT *)sy_allocmem( 1, seedv_setup.seed_rec_size,
		&status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	seedhdr = (SeedDataHeaderT *)seedrec;
	ulptr = (unsigned long *)seedrec;
	long_lth = seedv_setup.seed_rec_size / sizeof(long);

	inp = fopen( inpfile, "rb" );
	if  (inp == NULL)  {
		fprintf( stderr, "*** %s: file %s not found\n", argv[0], inpfile );
		return 1;
	} /*endif*/
	out = fopen( outfile, "wb" );
	if  (out == NULL)  {
		fprintf( stderr, "*** %s: file %s not opened\n", argv[0], outfile );
		fclose( inp );
		return 1;
	} /*endif*/

	/* get length of file */
	fseek( inp, 0, 2 );
	fsize = ftell( inp );
	if  (fsize % seedv_setup.seed_rec_size != 0)  {
		fprintf( stderr, "*** %s: illegal size %ld bytes of SEED file %s\n",
			argv[0], fsize, inpfile );
		fclose( inp );
		fclose( out );
		return 1;
	} /*endif*/
	numrecs = (int)(fsize / (long)seedv_setup.seed_rec_size);
	fseek( inp, 0, 0 );

	/* copy records and swap */
	for  (i=0; i<numrecs; i++)  {
		read_ret = fread( (char *)seedrec, seedv_setup.seed_rec_size, 1, inp );
		if  (read_ret != 1)  {
			fprintf( stderr, "*** %s: read error on file %s\n",
				argv[0], inpfile );
			fclose( inp );
			fclose( out );
			return 1;
		} /*endif*/
		if  (seedrec[6] == 'D')  {  /* swap only data records */
			SeedSwapHeader( seedhdr );
			for  (j=16; j<long_lth; j++)  {
				ultmp = ulptr[j];
				ulptr[j] =  (ultmp & 0x000000ff) << 24;
				ulptr[j] += (ultmp & 0x0000ff00) << 8;
				ulptr[j] += (ultmp & 0x00ff0000) >> 8;
				ulptr[j] += (ultmp & 0xff000000) >> 24;
			} /*endfor*/
		} /*endif*/
		write_ret = fwrite( (char *)seedrec, seedv_setup.seed_rec_size, 1, out );
		if  (write_ret != 1)  {
			fprintf( stderr, "*** %s: write error on file %s\n",
				argv[0], outfile );
			fclose( inp );
			fclose( out );
			return 1;
		} /*endif*/
	} /*endif*/

	fclose( inp );
	fclose( out );

	return 0;

} /* end of main */

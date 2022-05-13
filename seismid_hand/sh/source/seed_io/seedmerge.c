
/* file seedmerge.c
 *      ===========
 *
 * version 4, 7-Mar-96
 *
 * merges two Mini-SEED files into one
 * K. Stammler, 7-Jun-94
 */


#include <stdio.h>
#include <string.h>
#include BASECNST
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#ifdef BC_INC_UNISTD
#include BC_INC_UNISTD
#endif
#include BC_SYSBASE
#include BC_CPAR
#include BC_ERUSRDEF
#include BC_TCUSRDEF
#include "seedcfg.h"
#include "seed_lib.h"



/* global variables */
static SeedSetupParT seedv_setup;       /* SEED setup parameters */



int main( int argc, char *argv[] )
{
	/* local variables */
	STATUS   status;                 /* return status */
	char     file1[BC_FILELTH+1];    /* input file 1 */
	char     file2[BC_FILELTH+1];    /* input file 2 */
	char     outfile[BC_FILELTH+1];  /* output file */
	char     str[BC_FILELTH+1];      /* scratch */
	FILE     *f1, *f2, *out;         /* file pointers */
	SeedSbyteT *seedrec;             /* pointer to SEED record */
	SeedSbyteT *seedrec_alt;         /* pointer to alternative SEED record */
	NTIME    f1_start, f1_end;       /* start & end time of file 1 */
	NTIME    f2_start, f2_end;       /* start & end time of file 2 */
	NTIME    c_start, c_end;         /* start & end of current record */
	NTIME    ntime;                  /* scratch */
	long     f1_size, f2_size;       /* file sizes in SEED records */
	long     i, i1, i2;              /* counters */
	float    tdiff;                  /* time difference */
	BOOLEAN  f2_inserted;            /* file 2 inserted */
	int      reccnt;                 /* record counter */
	SeedSbyteT ident;                /* record indentifier */

	/* executable code */

	status = BC_NOERROR;

	pa_init( argc, argv );
	if  (pa_pnumber() != 3)  {
		printf( "Usage: %s <file1> <file2> <output> ***\n", argv[0] );
		return 1;
	} /*endif*/

	/* get parameters */
	SeedSetup( &seedv_setup, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	strcpy( file1, pa_pvalue(1) );
	strcpy( file2, pa_pvalue(2) );
	strcpy( outfile, pa_pvalue(3) );

	/* allocate memory for SEED records */
	seedrec = (SeedSbyteT *)sy_allocmem( 1, seedv_setup.seed_rec_size, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	seedrec_alt = (SeedSbyteT *)sy_allocmem( 1, seedv_setup.seed_rec_size,
		&status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* open input files */
	f1 = sy_fopen( file1, "rb" );
	if  (f1 == NULL)  {
		fprintf( stderr, "*** %s: file %s not found ***\n", argv[0], file1 );
		sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
		return 1;
	} /*endif*/
	f2 = sy_fopen( file2, "rb" );
	if  (f2 == NULL)  {
		fprintf( stderr, "*** %s: file %s not found ***\n", argv[0], file2 );
		sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
		sy_fclose( f1 );
		return 1;
	} /*endif*/

	/* determine start and end times of file 1 */
	if  (fread(seedrec,seedv_setup.seed_rec_size,1,f1) != 1)  {
		fprintf( stderr, "*** %s: read error on file %s ***\n", argv[0], file1 );
		sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
		sy_fclose( f1 );
		sy_fclose( f2 );
		return 1;
	} /*endif*/
	SeedRecordTime( seedrec, &f1_start, &ntime, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	/* get size of file 1 */
	fseek( f1, 0, 2 );
	f1_size = ftell( f1 );
	if  (f1_size % seedv_setup.seed_rec_size != 0)  {
		fprintf( stderr, "*** %s: illegal size %ld bytes of SEED file %s\n",
			f1_size, file1 );
		sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
		sy_fclose( f1 );
		sy_fclose( f2 );
		return 1 ;
	} /*endif*/
	f1_size /= (long)seedv_setup.seed_rec_size;
	/* read last record */
	if  (f1_size > 1)  {
		fseek( f1, (f1_size-1)*seedv_setup.seed_rec_size, 0 );
		if  (fread(seedrec,seedv_setup.seed_rec_size,1,f1) != 1)  {
			fprintf( stderr, "*** %s: read error on file %s ***\n",
				argv[0], file1 );
			sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
			sy_fclose( f1 );
			sy_fclose( f2 );
			return 1;
		} /*endif*/
		SeedRecordTime( seedrec, &ntime, &f1_end, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
	} else {
		f1_end = ntime;
	} /*endif*/

	/* determine start and end times of file 2 */
	if  (fread(seedrec,seedv_setup.seed_rec_size,1,f2) != 1)  {
		fprintf( stderr, "*** %s: read error on file %s ***\n", argv[0], file2 );
		sy_fclose( f1 );
		sy_fclose( f2 );
		sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
		return 1;
	} /*endif*/
	SeedRecordTime( seedrec, &f2_start, &ntime, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	/* get size of file 2 */
	fseek( f2, 0, 2 );
	f2_size = ftell( f2 );
	if  (f2_size % seedv_setup.seed_rec_size != 0)  {
		fprintf( stderr, "*** %s: illegal size %ld bytes of SEED file %s\n",
			f2_size, file2 );
		sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
		sy_fclose( f1 );
		sy_fclose( f2 );
		return 1 ;
	} /*endif*/
	f2_size /= (long)seedv_setup.seed_rec_size;
	/* read last record */
	if  (f2_size > 1)  {
		fseek( f2, (f2_size-1)*seedv_setup.seed_rec_size, 0 );
		if  (fread(seedrec,seedv_setup.seed_rec_size,1,f2) != 1)  {
			fprintf( stderr, "*** %s: read error on file %s ***\n",
				argv[0], file2 );
			sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
			sy_fclose( f1 );
			sy_fclose( f2 );
			return 1;
		} /*endif*/
		SeedRecordTime( seedrec, &ntime, &f2_end, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
	} else {
		f2_end = ntime;
	} /*endif*/

	/* swap files if file 2 is earlier than file 1 */
	tdiff = tc_ndiff( &f1_start, &f2_start, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	if  (tdiff > 0.0)  {
		strcpy( str, file1 );    strcpy( file1, file2 );  strcpy( file2, str );
		out = f1;                f1 = f2;                 f2 = out;
		i = f1_size;             f1_size = f2_size;       f2_size = i;
		ntime = f1_start;        f1_start = f2_start;     f2_start = ntime;
		ntime = f1_end;          f1_end = f2_end;         f2_end = ntime;
	} /*endif*/

	/* open output file and rewind input files */
	out = sy_fopen( outfile, "wb" );
	if  (out == NULL)  {
		fprintf( stderr, "*** %s: couldn't open output file %s ***\n",
			argv[0], outfile );
		sy_fclose( f1 );  sy_fclose( f2 );
		sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
		return 1;
	} /*endif*/
	fseek( f1, 0, 0 );
	fseek( f2, 0, 0 );

	/* change counters .... */

	f2_inserted = FALSE;
	reccnt = 0;

	for  (i1=0; i1<f1_size; i1++)  {

		if  (fread(seedrec,seedv_setup.seed_rec_size,1,f1) != 1)  {
			fprintf( stderr, "*** %s: read error on file %s ***\n",
				argv[0], file1 );
			sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
			sy_fclose( f1 );
			sy_fclose( f2 );
			return 1;
		} /*endif*/
		SeedRecordTime( seedrec, &c_start, &c_end, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		tdiff = tc_ndiff( &f2_start, &c_start, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		if  (tdiff > 0.0 || f2_inserted)  {
			ident = seedrec[6];
			sprintf( (char *)seedrec, "%06d", ++reccnt );
			seedrec[6] = ident;
			if  (fwrite(seedrec,seedv_setup.seed_rec_size,1,out) != 1)  {
				fprintf( stderr, "*** %s: write error on file %s ***\n",
					pa_progname(), outfile );
				sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
				sy_fclose( f1 ); sy_fclose( f2 ); sy_fclose( out );
				return 1;
			} /*endif*/
			tdiff = tc_ndiff( &f2_start, &c_end, &status );
			if  (Severe(&status))  err_writemsg( status, "", TRUE );
			if  (tdiff < 0.0 && !f2_inserted)  {
				tc_n2t( &ntime, str, &status );
				fprintf( stderr,
					"*** %s: -1- time %s in %s and %s, del output ***\n",
					pa_progname(), str, file1, file2 );
				sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
				sy_fclose( f1 ); sy_fclose( f2 ); sy_fclose( out );
				sy_fdelete( outfile );
				return 1;
			} /*endif*/
		} else {
			ntime = c_start;
			/* copy file 2 to output file */
			for  (i2=0; i2<f2_size; i2++)  {
				if  (fread(seedrec_alt,seedv_setup.seed_rec_size,1,f2) != 1)  {
					fprintf( stderr, "*** %s: read error on file %s ***\n",
						pa_progname(), file2 );
					sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
					sy_fclose( f1 ); sy_fclose( f2 ); sy_fclose( out );
					sy_fdelete( outfile );
					return 1;
				} /*endif*/
				SeedRecordTime( seedrec_alt, &c_start, &c_end, &status );
				if  (Severe(&status))  err_writemsg( status, "", TRUE );
				ident = seedrec_alt[6];
				sprintf( (char *)seedrec_alt, "%06d", ++reccnt );
				seedrec_alt[6] = ident;
				if  (fwrite(seedrec_alt,seedv_setup.seed_rec_size,1,out) != 1)  {
					fprintf( stderr, "*** %s: write error on file %s ***\n",
						pa_progname(), outfile );
					sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
					sy_fclose( f1 ); sy_fclose( f2 ); sy_fclose( out );
					sy_fdelete( outfile );
					return 1;
				} /*endif*/
			} /*endfor*/
			tdiff = tc_ndiff( &ntime, &c_end, &status );
			if  (Severe(&status))  err_writemsg( status, "", TRUE );
			if  (tdiff < 0.0)  {
				tc_n2t( &ntime, str, &status );
				fprintf( stderr,
					"*** %s: -2- time %s in %s and %s, del output ***\n",
					pa_progname(), str, file1, file2 );
				sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
				sy_fclose( f1 ); sy_fclose( f2 ); sy_fclose( out );
				sy_fdelete( outfile );
				return 1;
			} /*endif*/
			f2_inserted = TRUE;
			/* now write waiting record from f1 */
			ident = seedrec[6];
			sprintf( (char *)seedrec, "%06d", ++reccnt );
			seedrec[6] = ident;
			if  (fwrite(seedrec,seedv_setup.seed_rec_size,1,out) != 1)  {
				fprintf( stderr, "*** %s: write error on file %s ***\n",
					pa_progname(), outfile );
				sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
				sy_fclose( f1 ); sy_fclose( f2 ); sy_fclose( out );
				return 1;
			} /*endif*/
		} /*endif*/

	} /*endfor*/

	/* if not yet inserted append file */
	if  (!f2_inserted)  {
		for  (i2=0; i2<f2_size; i2++)  {
			if  (fread(seedrec_alt,seedv_setup.seed_rec_size,1,f2) != 1)  {
				fprintf( stderr, "*** %s: read error on file %s ***\n",
					pa_progname(), file2 );
				sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
				sy_fclose( f1 ); sy_fclose( f2 ); sy_fclose( out );
				sy_fdelete( outfile );
				return 1;
			} /*endif*/
			SeedRecordTime( seedrec_alt, &c_start, &c_end, &status );
			if  (Severe(&status))  err_writemsg( status, "", TRUE );
			ident = seedrec_alt[6];
			sprintf( (char *)seedrec_alt, "%06d", ++reccnt );
			seedrec_alt[6] = ident;
			if  (fwrite(seedrec_alt,seedv_setup.seed_rec_size,1,out) != 1)  {
				fprintf( stderr, "*** %s: write error on file %s ***\n",
					pa_progname(), outfile );
				sy_deallocmem( seedrec ); sy_deallocmem( seedrec_alt );
				sy_fclose( f1 ); sy_fclose( f2 ); sy_fclose( out );
				sy_fdelete( outfile );
				return 1;
			} /*endif*/
		} /*endfor*/
	} /*endif*/

	/* close files */
	sy_fclose( f1 );
	sy_fclose( f2 );
	sy_fclose( out );

	/* if not yet inserted give append message */
	if  (f2_inserted)  {
		printf( "done (i)\n" );
	} else {
		/*printf( "append %s\n", file2 );*/
		printf( "done (a)\n" );
	} /*endif*/

} /*end of main */

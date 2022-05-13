
/* file split_seed.c
 *      ============
 *
 * version 3, 12-Apr-96
 *
 * splits SEED Files to Mini-SEED files, one file per stream
 * K. Stammler, 22-Dec-94
 */


#include <stdio.h>
#include <string.h>
#include BASECNST
#include BC_SYSBASE
#include BC_CPAR
#include BC_ERUSRDEF
#include "seedcfg.h"
#include "seed_lib.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	STATUS   status;                    /* return status */
	int      recsize;                   /* SEED record size in bytes */
	char     recsize_par[cBcLineLth];   /* record size parameter string */
	int      byteoff;                   /* byte offset at beginning */
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
	int      reccnt;                    /* record counter */
	int      filecnt;                   /* file counter */
	char     last_station[BC_LINELTH+1];/* last station name */
	char     last_chan[BC_LINELTH+1];   /* last channel */
	char     curr_station[BC_LINELTH+1];/* current station */
	char     curr_chan[BC_LINELTH+1];   /* current channel */
	char     ctmp;                      /* scratch */
	int      maxfile;                   /* maximum number of files */
	int      startnum;                  /* start number number of file */

	/* executable code */

	status = BC_NOERROR;

	pa_init( argc, argv );
	if  (pa_pnumber() != 1)  {
		fprintf( stderr, "*** Usage: %s <inpfile> ***\n", argv[0] );
		fprintf( stderr,
			"qualifiers:  -maxfile=<max>   maximum file number\n" );
		fprintf( stderr, "             -start=<start>   start number of file\n" );
		return 1;
	} /*endif*/
	strcpy( inpfile, pa_pvalue(1) );
	maxfile = 0;
	if  (pa_qspecified("-maxfile"))
		sscanf( pa_qvalue("-maxfile"), "%d", &maxfile );
	startnum = 1;
	if  (pa_qspecified("-start"))
		sscanf( pa_qvalue("-start"), "%d", &startnum );
	startnum--;

	/* check for record size qualifier */
	recsize = 0;  /* must be set or detected later, otherwise abort */
	byteoff = 0;
	if  (pa_qspecified("-seedrec"))  {
		strcpy( recsize_par, pa_qvalue("-seedrec") );
		if  (strcmp(recsize_par,"quickfind") == 0)  {
			/* just do nothing, leave recsize zero */
		} else {
			if  (sscanf(recsize_par,"%d",&recsize) != 1)  {
				fprintf( stderr, "%s: illegal seedrec qualifier.  Abort.\n",
					pa_progname() );
				return 1;
			} /*endif*/
		} /*endif*/
	} /*endif*/

	/* get other parameters */
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* allocate memory for SEED record */
	seedrec = (SeedSbyteT *)sy_allocmem( 1, Seed_C_MAX_RECLTH, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	seedhdr = (SeedDataHeaderT *)seedrec;

	/* open SEED file */
	inp = fopen( inpfile, "rb" );
	if  (inp == NULL)  {
		fprintf( stderr, "*** %s: file %s not found\n", argv[0], inpfile );
		return 1;
	} /*endif*/

	/* determine record size if not specified */
	if  (recsize == 0)  {
		SeedQuickFindReclth( inp, &recsize, &byteoff );
		if  (recsize <= 0)  {
			fclose( inp );
			if  (recsize == 0)  {
				fprintf( stderr, "%s: no valid SEED file %s\n",
					pa_progname(), inpfile );
			} else {
				fprintf( stderr, "%s: cannot find record size in %s\n",
					pa_progname(), inpfile );
			} /*endif*/
			return 1;
		} /*endif*/
	} /*endif*/

	/* get length of file */
	fseek( inp, 0, 2 );
	fsize = ftell( inp );
	if  (fsize % recsize != 0)  {
		fprintf( stderr, "*** %s: illegal size %ld bytes of SEED file %s\n",
			argv[0], fsize, inpfile );
		fclose( inp );
		return 1;
	} /*endif*/
	numrecs = (int)(fsize / (long)recsize);
	fseek( inp, 0, 0 );

	*last_station = *last_chan = '\0';
	filecnt = startnum;
	reccnt = 0;
	out = NULL;

	/* copy records and swap */
	for  (i=0; i<numrecs; i++)  {
		read_ret = fread( (char *)seedrec, recsize, 1, inp );
		if  (read_ret != 1)  {
			fprintf( stderr, "*** %s: read error on file %s\n",
				argv[0], inpfile );
			fclose( inp );
			if  (out != NULL)  fclose( out );
			return 1;
		} /*endif*/
		if  (seedrec[6] != 'D' && seedrec[6] != 'Q' && seedrec[6] != 'R')  continue;
		strncpy( curr_station, seedhdr->statcode, 5 );
		curr_station[5] = '\0';
		if  (curr_station[4] <= ' ')  curr_station[4] = '\0';
		if  (curr_station[3] <= ' ')  curr_station[3] = '\0';
		strncpy( curr_chan, seedhdr->channel, 3 );
		curr_chan[3] = '\0';
		if  (curr_chan[2] <= ' ')  curr_chan[2] = '\0';
		if  (curr_chan[1] <= ' ')  curr_chan[1] = '\0';
		if  (out == NULL || strcmp(curr_chan,last_chan) != 0
			|| strcmp(curr_station,last_station) != 0)  {
			if  (out != NULL)  fclose( out );
			sprintf( outfile, "miniseed_%03d", ++filecnt );
			if  (maxfile > 0 && filecnt > maxfile)  {
				fclose( inp );
				return 0;
			} /*endif*/
			out = fopen( outfile, "wb" );
			if  (out == NULL)  {
				fprintf( stderr, "*** %s: file %s not opened\n", argv[0], outfile );
				fclose( inp );
				return 1;
			} /*endif*/
			printf( "write file %s with %s %s\n",
				outfile, curr_station, curr_chan );
			reccnt = 0;
			strcpy( last_station, curr_station );
			strcpy( last_chan, curr_chan );
		} /*endif*/
		ctmp = seedrec[6];
		sprintf( (char *)seedrec, "%06d", ++reccnt );
		seedrec[6] = 'D';  /*ctmp;*/
		write_ret = fwrite( (char *)seedrec, recsize, 1, out );
		if  (write_ret != 1)  {
			fprintf( stderr, "*** %s: write error on file %s\n",
				argv[0], outfile );
			fclose( inp );
			fclose( out );
			return 1;
		} /*endif*/
	} /*endif*/

	fclose( inp );
	if  (out != NULL)  fclose( out );

	return 0;

} /* end of main */

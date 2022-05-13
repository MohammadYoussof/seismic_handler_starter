
/* file split_gcf.c
 *      ============
 *
 * version 2, 23-Jul-2004
 *
 * splits multiplexed GCF Files to smaller files, one file per stream
 * K. Stammler, 22-Oct-2003
 */


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include "sysbase.h"
#include "cpar.h"
#include "erusrdef.h"
#include "tcusrdef.h"
#include "gcflib.h"


int main( int argc, char *argv[] )
{
	/* local variables */
	STATUS   status;                    /* return status */
	int      blksize;                   /* GCF block size in bytes */
	char     blksize_par[cBcLineLth];   /* block size parameter string */
	unsigned char *gcfblk;              /* pointer to GCF block */
	GcfRawHeaderT rhdr;                 /* raw data header */
	GcfHeaderT hdr;                     /* decoded header */
	NTIME    ntime;                     /* numeric time */
	char     inpfile[BC_FILELTH+1];     /* name of input file */
	char     outfile[BC_FILELTH+1];     /* name of current output file */
	char     l_outfile[BC_FILELTH+1];   /* name of last output file */
	FILE     *inp, *out;                /* file pointers */
	int      fsize;                     /* size of file in bytes */
	int      numblks;                   /* number ofGCF blocks */
	int      i, j;                      /* counter */
	int      read_ret;                  /* return value of fread */
	int      write_ret;                 /* return value of fwrite */
	int      blkcnt;                    /* record counter */
	int      filecnt;                   /* file counter */
	GcfLongT last_stream;               /* last stream ID */
	GcfLongT last_system;               /* last system ID */
	GcfStringT stream_str;              /* decoded stream string */
	GcfStringT system_str;              /* decoded system string */
	char     ctmp;                      /* scratch */
	int      maxfile;                   /* maximum number of files */
	int      startnum;                  /* start number number of file */

	/* executable code */

	status = cBcNoError;

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
	blksize = Gcf_C_DEF_BLOCKSIZE;  /* must be set or detected later, otherwise abort */
	if  (pa_qspecified("-gcfblk"))  {
		strcpy( blksize_par, pa_qvalue("-gcfblk") );
		if  (sscanf(blksize_par,"%d",&blksize) != 1)  {
			fprintf( stderr, "%s: illegal gcfblk qualifier.  Abort.\n",
				pa_progname() );
			return 1;
		} /*endif*/
	} /*endif*/

	/* allocate memory for SEED record */
	gcfblk = (unsigned char *)sy_allocmem( 1, blksize, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* open GCF file */
	inp = fopen( inpfile, "rb" );
	if  (inp == NULL)  {
		fprintf( stderr, "*** %s: file %s not found\n", argv[0], inpfile );
		return 1;
	} /*endif*/

	/* get length of file */
	fseek( inp, 0, 2 );
	fsize = ftell( inp );
	if  (fsize % blksize != 0)  {
		fprintf( stderr, "*** %s: illegal size %ld bytes of GCF file %s\n",
			argv[0], fsize, inpfile );
		fclose( inp );
		return 1;
	} /*endif*/
	numblks = (int)(fsize / (long)blksize);
	fseek( inp, 0, 0 );

	last_stream = last_system = 0;
	filecnt = startnum;
	blkcnt = 0;
	out = NULL;
	l_outfile[0] = '\0';

	/* copy records and swap */
	for  (i=0; i<numblks; i++)  {

		/* read next block from file */
		read_ret = fread( (char *)gcfblk, blksize, 1, inp );
		if  (read_ret != 1)  {
			fprintf( stderr, "*** %s: read error on file %s\n",
				argv[0], inpfile );
			fclose( inp );
			if  (out != NULL)  fclose( out );
			return 1;
		} /*endif*/

		/* get header and decode it */
		rhdr.system_id = GcfOrder4Bytes( gcfblk );
		rhdr.stream_id = GcfOrder4Bytes( gcfblk+4 );
		rhdr.date_code = GcfOrder4Bytes( gcfblk+8 );
		rhdr.data_format = GcfOrder4Bytes( gcfblk+12 );
		GcfDecodeHeader( &rhdr, &hdr, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "*** %s: error decoding header in file %s\n",
				argv[0], inpfile );
			fclose( inp );
			if  (out != NULL)  fclose( out );
			return 1;
		} /*endif*/

		/* get output file name */
		tc_t2n( hdr.blktime, &ntime, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "*** %s: error decoding time in file %s\n",
				argv[0], inpfile );
			fclose( inp );
			if  (out != NULL)  fclose( out );
			return 1;
		} /*endif*/
		sprintf( outfile, "%s_%s_%02d%02d%02d.gcf", hdr.system_id, hdr.stream_id,
			ntime.year, ntime.month, ntime.day );

		if  (strcmp(outfile,l_outfile) != 0)  {

			if  (out != NULL)  fclose( out );

			filecnt++;
			if  (maxfile > 0 && filecnt > maxfile)  {
				fprintf( stderr, "%s: Aborted.  Too many files.\n" );
				fclose( inp );
				return 0;
			} /*endif*/
			out = fopen( outfile, "ab" );
			if  (out == NULL)  {
				fprintf( stderr, "*** %s: file %s not opened\n", argv[0], outfile );
				fclose( inp );
				return 1;
			} /*endif*/
			printf( "write file %s\n", outfile );
			blkcnt = 0;

			strcpy( l_outfile, outfile );

		} /*endif*/

		/* write out block */
		write_ret = fwrite( (char *)gcfblk, blksize, 1, out );
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

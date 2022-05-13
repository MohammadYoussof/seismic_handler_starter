
/* file gfdline.c
 *      =========
 *
 * version 2, 26-Oct-2004
 *
 * returns gfd line for specified GCF file.  It is assumed that the GCF
 * file contains a single channel (read from the first record) and has
 * GCF records of monotonically increasing times.
 * K. Stammler, 20-Oct-2004
 *
 */


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include "sysbase.h"
#include "cpar.h"
#include "tcusrdef.h"
#include "utusrdef.h"
#include "erusrdef.h"
#include "gcflib.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	int      blksize;                   /* SEED record size in bytes */
	char     blksize_par[cBcLineLth];   /* record size parameter string */
	TSyStatus   status;                 /* return status */
	char     gcffile[cBcFileLth+1];     /* name of GCF file */
	GcfRawHeaderT rhdr;                 /* raw header of GCF file */
	GcfHeaderT hdr;                     /* decoded header */
	int      read_ret;                  /* fread return values */
	NTIME    ntime;                     /* numeric time */
	FILE     *fp;                       /* pointer to GCF file */
	char     gfd_t_start[BC_TIMELTH+1]; /* start time */
	char     gfd_t_end[BC_TIMELTH+1];   /* end time */
	int      gfd_blkno;                 /* number of records in file */
	char     gfd_stream[cBcLineLth+1];  /* stream string */
	int      i;                         /* counter */
	long     fsize;                     /* file size */
	float    dt;                        /* sample distance in sec */
	char     *csrc, *cdst;              /* moving pointers */
	char     outfile[cBcFileLth+1];     /* output file */
	FILE     *out;                      /* pointer to output file */
	char     chanstr[cBcShortStrLth+1]; /* scratch string for channel */
	int      errcnt;                    /* error counter */
	TSyBoolean include_bad;             /* include bad data files */
	TSyBoolean use_timecorr;            /* use time correction */

	/* executable code */

	status = cBcNoError;
	blksize = 1024;  /* assumed, no documentation found */

	pa_init( argc, argv );
	if  (pa_pnumber() < 1 || pa_pnumber() > 2)  {
		fprintf( stderr, "Usage: %s <gcf-file> [<outfile>]***\n",
			pa_progname() );
		return 1;
	} /*endif*/
	strcpy( gcffile, pa_pvalue(1) );
	if  (pa_pnumber() == 2)  {
		strcpy( outfile, pa_pvalue(2) );
		out = fopen( outfile, "a" );
		if  (out == NULL)  {
			fprintf( stderr, "%s: output file %s couldn't be opened ***\n",
				pa_progname(), outfile );
			return 1;
		} /*endif*/
	} else {
		out = stdout;
	} /*endif*/

	/* check for block size qualifier */
	if  (pa_qspecified("-gcfblk"))  {
		strcpy( blksize_par, pa_qvalue("-gcfblk") );
		if  (sscanf(blksize_par,"%d",&blksize) != 1)  {
			fprintf( stderr, "%s: illegal gcfblk qualifier.  Abort.\n",
				pa_progname() );
			return 1;
		} /*endif*/
	} /*endif*/

	/* read tap codes from $HOME/.tapcodes */
	GcfReadUserTapcodes( NULL, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );

	include_bad = FALSE;
	if  (pa_qspecified("-include_bad"))
		include_bad = TRUE;

	/* open gcf file */
	fp = fopen( gcffile, "rb" );
	if  (fp == NULL)  {
		fprintf( stderr, "*** %s: file %s not found\n", pa_progname(), gcffile );
		fclose( out );
		return 1;
	} /*endif*/

	/* read first record */
	GcfReadRawHeader( fp, &rhdr, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );
	GcfDecodeHeader( &rhdr, &hdr, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );

	strcpy( gfd_t_start, hdr.blktime );

	sprintf( gfd_stream, "%s-%s-%c", hdr.station, hdr.chan, hdr.comp );
	ut_uncap( gfd_stream );

	/* get sample distance */
	dt = 1.0 / (float)hdr.smprate;

	/* get length of file */
	fseek( fp, 0, 2 );
	fsize = ftell( fp );
	if  (fsize % blksize != 0)  {
		fprintf( stderr, "*** %s: illegal size %ld bytes of GCF file %s\n",
			pa_progname(), fsize, gcffile );
		fclose( fp );
		fclose( out );
		return 1 ;
	} /*endif*/
	gfd_blkno = (int)(fsize / (long)blksize);

	/* get last record */
	fseek( fp, (long)(gfd_blkno-1)*(long)blksize, 0 );

	/* read last record */
	GcfReadRawHeader( fp, &rhdr, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );
	GcfDecodeHeader( &rhdr, &hdr, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );

	tc_tadd( hdr.blktime, (float)(hdr.numrec*hdr.cmpcode/hdr.smprate),
		gfd_t_end, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );

	fclose( fp );

	fprintf( out, "%c>%s %c>%s %c>%s %c>%s %c>%d %c>%d\n",
		Gcf_C_GfdStream, gfd_stream, Gcf_C_GfdName, gcffile,
		Gcf_C_GfdTStart, gfd_t_start,Gcf_C_GfdTEnd, gfd_t_end,
		Gcf_C_GfdBlkno, gfd_blkno, Gcf_C_GfdBlklth, blksize );

	if  (out != stdout)  fclose( out );

	return 0;

} /* end of main */



/*----------------------------------------------------------------------------*/


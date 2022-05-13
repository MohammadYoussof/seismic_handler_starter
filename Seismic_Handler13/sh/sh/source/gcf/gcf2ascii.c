
/* file gcf2ascii.c
 *      ===========
 *
 * version 1, 19-Oct-2003
 *
 * Converts GCF data to ASCII
 *
 * K. Stammler, 19-Oct-2003
 */

#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include "sysbase.h"
#include "tcusrdef.h"
#include "gcflib.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char     gcffile[cBcFileLth+1];  /* input GCF file */
	FILE     *gcf;                   /* pointer to GCF file */
	GcfRawHeaderT rhdr;              /* raw header */
	GcfHeaderT hdr;                  /* decoded header */
	int      i;                      /* counter */
	char     str[cBcLineLth+1];      /* text string */
	char     recend[cBcTimeLth+1];   /* end time of block */
	char     lasttime[cBcTimeLth+1]; /* last block end time */
	float    gaplth;                 /* length of data gap */
	GcfLongT last_stream_id;         /* last stream ID */
	TSyStatus status;                /* return status */
	int      trccnt;                 /* trace counter */
	int      trclth;                 /* lengthof output trace */
	GcfLongT *datablock;             /* memory for data block */
	int      *blocksmp;              /* memory for samples of a block */
	int      numsmp;                 /* number of samples in a block */
	int      crcnt;                  /* carriage return counter */

	/* executable code */

	if  (argc != 2)  {
		fprintf( stderr, "Usage: %s <inpfile>\n", argv[0] );
		return 1;
	} /*endif*/

	/* get parameters */
	strcpy ( gcffile, argv[1] );

	status = cBcNoError;

	GcfSetTapcode( 2, "HH", &status );
	if  (SySevere(&status))  err_writemsg( status, "setting tapcode", TRUE );

	gcf = fopen( gcffile, "r" );
	if  (gcf == NULL)  {
		fprintf( stderr, "%s: cannot open input file %s\n", argv[0], gcffile );
		return 1;
	} /*endif*/

	datablock = GcfAllocBlockMem( &status );
	if  (SySevere(&status))  err_writemsg( status, "error reading header", TRUE );
	blocksmp = GcfAllocBlockSamples( &status );
	if  (SySevere(&status))  err_writemsg( status, "error reading header", TRUE );

	*lasttime = '\0';
	gaplth = 1.0;
	last_stream_id = 0;
	trccnt = 0;
	trclth = 0;
	crcnt = 1;

	FOREVER  {

		GcfReadRawHeader( gcf, &rhdr, &status );
		if  (status == GcfERR_EOF_FOUND)  {
			status = cBcNoError;
			break;
		} /*endif*/
		if  (SySevere(&status))  err_writemsg( status, "error reading header", TRUE );

		GcfDecodeHeader( &rhdr, &hdr, &status );
		if  (SySevere(&status))
			err_writemsg( status, "error reading header", TRUE );

		/* check for data gap */
		tc_tadd( hdr.blktime, (float)(hdr.numrec*hdr.cmpcode/hdr.smprate),
			recend, &status );
		if  (SySevere(&status))
			err_writemsg( status, "error computing end time", TRUE );
		if  (*lasttime != '\0')
			gaplth = tc_tdiff( hdr.blktime, lasttime, &status );

		/* check for multiplexed data (i.e. other station,channel) */
		if  (last_stream_id == 0)  last_stream_id = rhdr.stream_id;

		if  (gaplth > 0.0 || (last_stream_id != rhdr.stream_id))  {
			if  (trccnt > 0)
				printf( "\n@@LTH%04d@@ %d\n\n", trccnt, trclth );
			trclth = 0;
			crcnt = 1;
			trccnt++;
			printf( "STATION: %s\n", hdr.station );
			printf( "COMP: %c\n", hdr.comp );
			printf( "START: %s\n", hdr.blktime );
			printf( "LENGTH: @@LTH%04d@@\n", trccnt );
			printf( "DELTA: %g\n", 1.0/(float)hdr.smprate );
			printf( "\n" );
		} /*endif*/

		GcfReadDataBlock( gcf, &rhdr, datablock, &status );
		if  (SySevere(&status))
			err_writemsg( status, "error reading data block", TRUE );

		GcfDecodeBlock( &rhdr, datablock, blocksmp, &numsmp, &status );
		if  (SySevere(&status))
			err_writemsg( status, "error decoding data block", TRUE );
		trclth += numsmp;

		/* write out samples */
		for  (i=0; i<numsmp; i++)  {
			printf( " %d", blocksmp[i] );
			if  (crcnt++ % 10 == 0)  printf( "\n" );
		} /*endif*/

		strcpy( lasttime, recend );
		last_stream_id = rhdr.stream_id;

	} /*endwhile*/

	if  (trccnt > 0)
		printf( "\n@@LTH%04d@@ %d\n\n", trccnt, trclth );

	fclose( gcf );
	GcfFreeMem( datablock );
	GcfFreeMem( blocksmp );

	return 0;

} /* end of main */

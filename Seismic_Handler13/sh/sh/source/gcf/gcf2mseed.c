
/* file gcf2mseed.c
 *      ===========
 *
 * version 2, 21-Nov-2005
 *
 * Converts GCF data to MiniSEED
 *
 * K. Stammler, 19-Oct-2003
 */

#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "tcusrdef.h"
#include "gcflib.h"
#include "../seed_io/seedcfg.h"
#include "../seed_io/seed_lib.h"


/* global variables */
static SeedSetupParT seedv_setup;       /* SEED setup parameters */

/* maxim size of data chunks */
#define CHUNKSIZE 4000


/* prototypes of local routines */
void InitSeedHeader( GcfHeaderT *gcfhdr, SeedDataHeaderT *seedhdr,
	TSyStatus *status );



int main( int argc, char *argv[] )
{
	/* local variables */
	char     gcffile[cBcFileLth+1];  /* input GCF file */
	char     seedfile[cBcFileLth+1]; /* output seed file */
	FILE     *gcf;                   /* pointer to GCF file */
	FILE     *seed;                  /* pointer to SEED output file */
	GcfRawHeaderT rhdr;              /* raw header */
	GcfHeaderT hdr;                  /* decoded header */
	int      i;                      /* counter */
	char     str[cBcLineLth+1];      /* text string */
	char     recend[cBcTimeLth+1];   /* end time of block */
	char     lasttime[cBcTimeLth+1]; /* last block end time */
	float    gaplth;                 /* length of data gap */
	int      blockcnt;               /* block counter */
	GcfLongT last_stream_id;         /* last stream ID */
	TSyStatus status;                /* return status */
	GcfLongT *datablock;             /* memory for data block */
	INT32    *smparr;                /* memory for samples of a block */
	int      numsmp;                 /* number of samples in a block */
	int      wrklth;                 /* length of work space in records */
	INT32    idx;                    /* sample index */
	SeedSbyteT wrk[CHUNKSIZE*4];     /* work space */
	SeedDataHeaderT seedhdr;         /* MiniSEED data header */
	INT32    prev_smp;               /* previous sample */
	NTIME    ctime;                  /* current time */

	/* executable code */

	if  (argc != 3)  {
		fprintf( stderr, "Usage: %s <inpfile> <outfile>\n", argv[0] );
		return 1;
	} /*endif*/

	/* get parameters */
	strcpy( gcffile, argv[1] );
	strcpy( seedfile, argv[2] );

	status = cBcNoError;

	SeedSetup( &seedv_setup, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	wrklth = CHUNKSIZE*7 / seedv_setup.seed_rec_size;

	GcfSetTapcode( 2, "HH", &status );
	if  (SySevere(&status))  err_writemsg( status, "setting tapcode", TRUE );

	gcf = fopen( gcffile, "r" );
	if  (gcf == NULL)  {
		fprintf( stderr, "%s: cannot open input file %s\n", argv[0], gcffile );
		return 1;
	} /*endif*/

	seed = fopen( seedfile, "w" );
	if  (gcf == NULL)  {
		fprintf( stderr, "%s: cannot open output file %s\n", argv[0], seedfile );
		fclose( gcf );
		return 1;
	} /*endif*/

	datablock = GcfAllocBlockMem( &status );
	if  (SySevere(&status))  err_writemsg( status, "error reading header", TRUE );
	smparr = (INT32 *)malloc( 8192 * sizeof(int) );
	if  (smparr == NULL)  {
		fprintf( stderr, "%s: error allocating memory\n", argv[0] );
		exit( 1 );
	} /* endif*/

	*lasttime = '\0';
	gaplth = 0.0;
	last_stream_id = 0;
	blockcnt = 0;
	idx = 0;
	prev_smp = 0;

	FOREVER  {

		blockcnt++;

		/* read and decode GCF header */
		GcfReadRawHeader( gcf, &rhdr, &status );
		if  (status == GcfERR_EOF_FOUND)  {
			status = cBcNoError;
			break;
		} /*endif*/
		if  (SySevere(&status))
			err_writemsg( status, "error reading header", TRUE );
		GcfDecodeHeader( &rhdr, &hdr, &status );
		if  (SySevere(&status))
			err_writemsg( status, "error reading header", TRUE );

		/* check for data gap (compare with last block) */
		tc_tadd( hdr.blktime, (float)(hdr.numrec*hdr.cmpcode/hdr.smprate),
			recend, &status );
		if  (SySevere(&status))
			err_writemsg( status, "error computing end time", TRUE );
		if  (*lasttime != '\0')
			gaplth = tc_tdiff( hdr.blktime, lasttime, &status );

		/* check for multiplexed data (i.e. other station,channel) */
		if  (last_stream_id == 0)  last_stream_id = rhdr.stream_id;

		/* if first run or multiplexed data then initialize SEED header */
		if  (blockcnt == 1 || last_stream_id != rhdr.stream_id)  {
			InitSeedHeader( &hdr, &seedhdr, &status );
			if  (SySevere(&status))
				err_writemsg( status, "error init SEED header", TRUE );
			SeedSetHeaderPrototype( &seedhdr );
			SeedBtimeToNtime( &(seedhdr.starttime), &ctime, &status );
			if  (SySevere(&status))
				err_writemsg( status, "error init SEED header", TRUE );
		} /*endif*/

		/* on data gap flush data and reset time */
		if  (gaplth > 0.0)  {
			printf( "Gap of %4.1fs found before %s\n", gaplth, hdr.blktime );
			/* flush remaining data */
			if  (idx > 0)
				SeedEncodeSteim1( fileno(seed), smparr, &prev_smp, &idx, &ctime,
					wrk, wrklth, TRUE, &status );
			if  (SySevere(&status))
				err_writemsg( status, "error flushing data", TRUE );
			tc_t2n( hdr.blktime, &ctime, &status );
			if  (SySevere(&status))
				err_writemsg( status, "error computing time", TRUE );
		} /*endif*/

		/* read and decode remaining GCF block (data) */
		GcfReadDataBlock( gcf, &rhdr, datablock, &status );
		if  (SySevere(&status))
			err_writemsg( status, "error reading data block", TRUE );
		GcfDecodeBlock( &rhdr, datablock, (int *)smparr+idx, &numsmp, &status );
		if  (SySevere(&status))
			err_writemsg( status, "error decoding data block", TRUE );

		/* write data out to SEED file */
		if  (idx > CHUNKSIZE)  {fprintf(stderr,"increase CHUNKSIZE\n"); exit(1);}
		idx += numsmp;
		SeedEncodeSteim1( fileno(seed), smparr, &prev_smp, &idx, &ctime,
			wrk, wrklth, FALSE, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );

		strcpy( lasttime, recend );
		last_stream_id = rhdr.stream_id;

	} /*endwhile*/

	/* flush remaining data */
	if  (idx > 0)
		SeedEncodeSteim1( fileno(seed), smparr, &prev_smp, &idx, &ctime,
			wrk, wrklth, TRUE, &status );

	fclose( gcf );
	fclose( seed );
	GcfFreeMem( datablock );
	GcfFreeMem( smparr );

	return 0;

} /* end of main */



/*----------------------------------------------------------------------------*/



void InitSeedHeader( GcfHeaderT *gcfhdr, SeedDataHeaderT *seedhdr,
	TSyStatus *status )

/* initialalizes SEED header
 *
 * parameters of routine
 * GcfHeaderT    *gcfhdr;      input; GCF header
 * SeedDataHeaderT *seedhdr;   output; seed header
 * TSyStatus     *status;      output; return status
 */
{
	/* local variables */
	NTIME    ntime;      /* numeric time */

	/*executable code */

	/* SEED header initialization */
	strcpy( seedhdr->seqno, "000001" );
	seedhdr->indicator = 'D';
	strcpy( seedhdr->statcode, gcfhdr->station );
	seedhdr->locid[0] = ' ';
	seedhdr->locid[1] = ' ';
	seedhdr->channel[0] = gcfhdr->chan[0];
	seedhdr->channel[1] = gcfhdr->chan[1];
	seedhdr->channel[2] = gcfhdr->comp;
	tc_t2n( gcfhdr->blktime, &ntime, status );
	if  (SySevere(status))  return;
	SeedNtimeToBtime( &ntime, &(seedhdr->starttime), status );
	if  (Severe(status))  return;
	seedhdr->smprate_fact = gcfhdr->smprate;
	seedhdr->smprate_mult = 1;
	seedhdr->activity = 0;
	seedhdr->ioflags = 0;
	seedhdr->quality = 0;
	seedhdr->timecorr = 0;

} /* end of InitSeedHeader */



/*----------------------------------------------------------------------------*/

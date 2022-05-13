
/* file seed512to4096.c
 *      ===============
 *
 * version 5, 2-Feb-2007
 *
 * Reformats 512 byte records to 4096 byte records by reading 8 512-records
 * and writing one 4096-record.  Some space in the output record remains
 * unused.
 * K. Stammler, 17-Sep-98
 *
 * v  5, K.S.  changed MAXSAMPLE from 5000 to 1000
 */


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "erusrdef.h"
#include "tcusrdef.h"
#include "cpar.h"
#include "seedcfg.h"
#include "seed_lib.h"


#define INPRECSIZE 512
	/* input record length */
#define OUTRECSIZE 4096
	/* output record length */
#define MERGEREC 8
	/* this number of 512 records is merged to one 4096 */
#define MAXSAMPLE 10000
	/* array size */
#define MAXSMPPERREC 3772
	/* maximum number of samples in one 4096 block */



/* global variables */
static SeedSbyteT    cv_seedrec[INPRECSIZE];  /* SEED input record */
static SeedSbyteT    cv_wrk[OUTRECSIZE*2];    /* SEED workspace */
static INT32         cv_smp[MAXSAMPLE];       /* decoded samples */


int main( int argc, char *argv[] )
{
	/* local variables */
	SeedDataHeaderT hdrproto;            /* header prototype */
	TSyStatus status;                    /* status value */
	char     fname[cBcFileLth+1];        /* name of input file */
	FILE     *fp;                        /* pointer to input file */
	int      read_ret;                   /* read return value */
	int      mergecnt;                   /* merge counter */
	float    dt;                         /* sample distance in sec */
	float    tdiff;                      /* time difference */
	NTIME    lasttime;                   /* end time of previous record */
	NTIME    bt, et;                     /* begin and end time of record */
	NTIME    wt;                         /* start time of sample array */
	TSyBoolean gap_found;                /* gap found */
	TSyBoolean swap;                     /* swap data */
	INT32    smpnum;                     /* current number of samples */
	INT32    new_smpnum;                 /* number of samples in current rec. */
	INT32    prev_smp;                   /* previos sample */
	int      av_timequal;                /* average time quality over 4096 rec */
	int      cur_timequal;               /* time qulaity of current 512 rec */
	int      tq_cnt;                     /* time quality counter */
	UBYTE    activity, ioflags, quality; /* data flags in header */
	TSyBoolean flushbuf;                 /* flush buffer */

	/* executable code */

	status = cBcNoError;

	/* initialize seed routines */
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	SeedSetOutputReclth( OUTRECSIZE );

	/* get parameters */
	pa_init( argc, argv );
	if  (pa_pnumber() != 1)  {
		fprintf( stderr, "Usage: %s <file512>\n", pa_progname() );
		fprintf( stderr, "   qualifiers:\n" );
		fprintf( stderr, "   -swap      swap data\n" );
		fprintf( stderr, "   -noswap    don't swap data\n" );
		return 1;
	} /*endif*/
	strcpy( fname, pa_pvalue(1) );
	swap = TRUE;
	if  (pa_qspecified("-swap"))  swap = TRUE;
	if  (pa_qspecified("-noswap"))  swap = FALSE;

	/* open input file */
	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: input file %s not found\n", pa_progname(), fname );
		return 1;
	} /*endif*/

	mergecnt = 0;
	lasttime.month = 0;
	gap_found = FALSE;
	smpnum = 0;
	prev_smp = 0;
	av_timequal = cur_timequal = tq_cnt = 0;
	activity = ioflags = quality = (UBYTE)0;

	FOREVER {

		/* read next record */
		read_ret = fread( cv_seedrec, 1, INPRECSIZE, fp );
		if  (read_ret == 0)  break;
		if  (read_ret < INPRECSIZE)  {
			fprintf( stderr, "%s: read error on %s, abort.\n", pa_progname(),
				fname );
			break;
		} /*endif*/
		/* use only data records */
		if  (cv_seedrec[6] != 'D')  continue;
		SeedStoreReclth( (SeedDataHeaderT *)cv_seedrec, INPRECSIZE );
		SeedRecordTime( cv_seedrec, &bt, &et, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: decode error on %s\n", pa_progname(), fname );
			status = cBcNoError;
			continue;
		} /*endif*/

		/* check for gaps */
		if  (lasttime.month == 0)  {
			dt = SeedGetSampleDist( (SeedDataHeaderT *)cv_seedrec );
			hdrproto = *(SeedDataHeaderT *)cv_seedrec ;
			SeedSetHeaderPrototype( &hdrproto );
			wt = bt;
		} else {
			tdiff = tc_ndiff( &bt, &lasttime, &status );
			gap_found = (Abs(tdiff) > dt/2.0);
		} /*endif*/
		lasttime = et;

		/* store data flags */
		activity |= ((SeedDataHeaderT *)cv_seedrec)->activity;
		ioflags |= ((SeedDataHeaderT *)cv_seedrec)->ioflags;
		quality |= ((SeedDataHeaderT *)cv_seedrec)->quality;

		/* increment record read counter */
		mergecnt++;
		if  (mergecnt == 1)  {
			wt = bt;
			if  (smpnum > 0)  tc_nadd( &wt, -smpnum*dt, &wt, &status );
			if  (SySevere(&status))  {
				fprintf( stderr, "%s: error computing times.  Abort.\n",
					pa_progname() );
				exit( 1 );
			} /*endif*/
		} /*endif*/

		if  (gap_found)  {
			/* set time quality if found */
			if  (tq_cnt > 0)  {
				av_timequal = Nint( (float)av_timequal/(float)tq_cnt );
				SeedSetNextTimequal( av_timequal );
				av_timequal = tq_cnt = 0;
			} /*endif*/
			hdrproto.activity = activity;
			hdrproto.ioflags = ioflags;
			hdrproto.quality = quality;
			SeedSetHeaderPrototype( &hdrproto );
			/* flush output, then decode new record */
			SeedEncodeSteim1( fileno(stdout), cv_smp, &prev_smp, &smpnum, &wt,
				cv_wrk, 2, TRUE, &status );
			if  (SySevere(&status))  {
				fprintf( stderr, "%s: error writing SEED output.  Abort.\n",
					pa_progname() );
				exit( 1 );
			} /*endif*/
			activity = ioflags = quality = (UBYTE)0;
			/* decode record */
			SeedDecodeSteim1( cv_seedrec, swap, MAXSAMPLE, cv_smp, &smpnum );
			mergecnt = 1;
			wt = bt;
			prev_smp = 0;
			/* get time quality */
			cur_timequal = SeedGetTimequal( (SeedDataHeaderT *)cv_seedrec );
			if  (cur_timequal >= 0)  {
				av_timequal += cur_timequal;
				tq_cnt++;
			} /*endif*/
		} else {
			/* decode new record, if 8 input records -> write output record */
			SeedDecodeSteim1( cv_seedrec, swap, MAXSAMPLE-smpnum,
				cv_smp+smpnum, &new_smpnum );
			smpnum += new_smpnum;
			/* get time quality */
			cur_timequal = SeedGetTimequal( (SeedDataHeaderT *)cv_seedrec );
			if  (cur_timequal >= 0)  {
				av_timequal += cur_timequal;
				tq_cnt++;
			} /*endif*/
			if  (mergecnt == MERGEREC || smpnum >= MAXSMPPERREC)  {
				/* set time quality if found */
				if  (tq_cnt > 0)  {
					av_timequal = Nint( (float)av_timequal/(float)tq_cnt );
					SeedSetNextTimequal( av_timequal );
					av_timequal = tq_cnt = 0;
				} /*endif*/
				hdrproto.activity = activity;
				hdrproto.ioflags = ioflags;
				hdrproto.quality = quality;
				hdrproto.timecorr = ((SeedDataHeaderT *)cv_seedrec)->timecorr;
				SeedSetHeaderPrototype( &hdrproto );
				flushbuf = (smpnum <= MAXSMPPERREC);
				/* write output record */
				SeedEncodeSteim1( fileno(stdout), cv_smp, &prev_smp, &smpnum, &wt,
					cv_wrk, 2, flushbuf, &status );
				if  (SySevere(&status))  {
					fprintf( stderr, "%s: error writing SEED output.  Abort.\n",
						pa_progname() );
					exit( 1 );
				} /*endif*/
				/* reset merge and sample counter */
				mergecnt = 0;
				if  (flushbuf)  smpnum = 0;
				activity = ioflags = quality = (UBYTE)0;
			} /*endif*/
		} /*endif*/

	} /*endfor*/

	/* flush output */
	if  (smpnum > 0)  {
		/* set time quality if found */
		if  (tq_cnt > 0)  {
			av_timequal = Nint( (float)av_timequal/(float)tq_cnt );
			SeedSetNextTimequal( av_timequal );
			av_timequal = tq_cnt = 0;
		} /*endif*/
		SeedEncodeSteim1( fileno(stdout), cv_smp, &prev_smp, &smpnum, &wt,
			cv_wrk, 2, TRUE, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error writing SEED output.  Abort.\n",
				pa_progname() );
			exit( 1 );
		} /*endif*/
	} /*endif*/

	fclose( fp );
	return 0;

} /* end of main */

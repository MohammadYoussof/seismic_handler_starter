
/* file copy_recs.c
 *      ===========
 *
 * version 20, 6-Feb-2007
 * v 10: 14-Aug-96, include jukebox reading
 * v 11: 14-Aug-97, include 2nd jukebox
 * v 12: 21-Aug-97  correct call of jk_bugfix_loop.csh
 * v 13: 13-Mar-98, check for locked data windows (CdDataLocked)
 * v 14: 13-Oct-98, convert string data to uppercase in record header
 * v 15: 11-Jan-00, variable record length
 * v 16: 14-Jan-00, fix output of CD name, change size output from recs to kB
 * v 17: 04-Feb-00, Y2K bug in standard_name
 * v 18: 09-Oct-02, write Netcode GR if qualifier -net=GR is given
 * v 19: 09-Mar-06, remove bugfix loop, no more CD jukeboxes used
 * v 20: 06-Feb-07, add GpReadParams and -defaultnet qualifier
 *
 * Counts kB in a stream within a given time window
 * if an output file is specified the records are copied to this file.
 * If the record length changes during data read a new output file is written.
 * This file gets a new standard name or the specified name extended by
 * a counter (*.1, *.2, ...).
 * Implementation: reads record by record and counts until end time is reached.
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
#include "seed_cd.h"



/* prototypes of local routines */
static void standard_name( char stream[], char start[], char name[] );



int main( int argc, char *argv[] )
{
	/* local variables */
	STATUS   status;                    /* return status */
	SeedSbyteT *seedrec;                /* pointer to SEED record */
	SeedDataHeaderT *seedhdr;           /* pointer to seed data header */
	int      reclth;                    /* SEED record length */
	int      last_reclth;               /* length of last record */
	char     sfdfile[cBcFileLth+1];     /* sfd file name */
	char     stream[cBcShortStrLth+1];  /* stream name */
	char     t_start[cBcTimeLth+1];     /* start time */
	char     t_end[cBcTimeLth+1];       /* end time */
	char     act_start[cBcTimeLth+1];   /* actual start time */
	char     outfile[cBcFileLth+1];     /* name of output file */
	int      outfile_lth;               /* length of outfile name */
	char     timestr[cBcTimeLth+1];     /* time string */
	NTIME    recstime, recetime;        /* start & end time of record */
	NTIME    p_recstime, p_recetime;    /* start & end time of previous record */
	NTIME    endntime;                  /* end time */
	int      rec_cnt;                   /* record counter */
	int      gap_cnt;                   /* gap counter */
	BOOLEAN  hdr_swapped;               /* header was swapped */
	FILE     *out;                      /* pointer to output file */
	SeedSbyteT ctmp;                    /* scratch */
	BOOLEAN  work_quiet;                /* print less error messages */
	BOOLEAN  exact;                     /* cut 1st and last recs to exact times*/
	BOOLEAN  swap;                      /* swap bytes (only imp. for -exact) */
	BOOLEAN  no_negative_gaps;          /* ignore negative gap records */
	BOOLEAN  negative_gap;              /* negative time gap found */
	float    tdiff;                     /* time difference */
	float    gapdiff;                   /* gap time difference */
	float    dt;                        /* sample distance in sec */
	BOOLEAN  read_jukebox;              /* read from jukebox */
	BOOLEAN  overwrite_netcode=FALSE;   /* overwrite netcode */
	char     cdlabel[cBcShortStrLth+1]; /* CD label */
	char     cdmagic[cBcLineLth+1];     /* magic string of CD */
	char     jkpath[cBcLineLth+1];      /* jukebox root path */
	int      i;                         /* counter */
	int      bytesize;                  /* total size in bytes */
	int      fcnt;                      /* file counter */
	char     netcode[cBcShortStrLth+1]; /* network code */
	char     pathcache[cBcFileLth+1];   /* path cache file */

	/* executable code */

	GpReadParfile();
	status = BC_NOERROR;
	work_quiet = FALSE;
	swap = TRUE;
	exact = FALSE;
	no_negative_gaps = FALSE;
	p_recstime.year = 0;
	p_recetime.year = 0;
	dt = 0.0;
	strcpy( cdlabel, "online" );
	reclth = last_reclth = 0;
	bytesize = 0;
	*pathcache = '\0';

	pa_init( argc, argv );
	if  (pa_pnumber() < 4 || pa_pnumber() > 5)  {
		fprintf( stderr,
			"Usage: %s <sfdfile> <stream> <start> <end> [<outfile>]\n",
			pa_progname() );
		return 1;
	} /*endif*/
	strcpy( sfdfile, pa_pvalue(1) );
	strcpy( stream, pa_pvalue(2) );
	strcpy( t_start, pa_pvalue(3) );
	strcpy( t_end, pa_pvalue(4) );
	*outfile = '\0';
	if  (pa_pnumber() >= 5)  strcpy( outfile, pa_pvalue(5) );
	if  (pa_qspecified("-quiet"))  work_quiet = TRUE;
	if  (pa_qspecified("-exact"))  exact = TRUE;
	if  (pa_qspecified("-noexact"))  exact = FALSE;
	if  (pa_qspecified("-swap"))  swap = TRUE;
	if  (pa_qspecified("-noswap"))  swap = FALSE;
	if  (pa_qspecified("-neggaps"))  no_negative_gaps = FALSE;
	if  (pa_qspecified("-noneggaps"))  no_negative_gaps = TRUE;
	if  (pa_qspecified("-invhdr"))  SeedSetInvertSwapHeader( TRUE );
	*netcode = '\0';
	if  (pa_qspecified("-net"))  {
		strcpy( netcode, pa_qvalue("-net") );
		overwrite_netcode = TRUE;
	} /*endif*/
	if  (pa_qspecified("-defaultnet"))  {
		strcpy( netcode, pa_qvalue("-defaultnet") );
		overwrite_netcode = FALSE;
	} /*endif*/
	if  (pa_qspecified("-pathcache"))  {
		strcpy( pathcache, pa_qvalue("-pathcache") );
		SqlPathcacheFile( pathcache );
	} /*endif*/
	read_jukebox = (strcmp(sfdfile,"JK:") == 0);

	/* open output file if requested */
	fcnt = 0;
	out = NULL;
	if  (*outfile != '\0')  {
		if  (strcmp(outfile,"standard") == 0)  {
			standard_name( stream, t_start, outfile );
		} else {
			fcnt = 1;  /* fcnt > 0 means keep given name as prefix */
		} /*endif*/
		outfile_lth = strlen( outfile );
		out = fopen( outfile, "wb" );
		if  (out == NULL)  {
			fprintf( stderr, "%s: output file %s couldn't be opened\n",
				pa_progname(), outfile );
			return 1;
		} /*endif*/
	} /*endif*/

	tc_t2n( t_end, &endntime, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* get other parameters */
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* allocate memory for SEED record */
	seedrec = (SeedSbyteT *)sy_allocmem( 1, Seed_C_MAX_RECLTH, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* if reading from jukebox get name of sfdfile */
	if  (read_jukebox)  {
		tdiff = tc_tdiff( t_end, t_start, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
		if  (CdDataLocked(stream,t_start,tdiff,&status))  {
			fprintf( stderr, "%s: %s is locked at %s.  Abort.\n",
				pa_progname(), stream, t_start );
			return 1;
		} /*endif*/
		CdFindLabel( stream, t_start, jkpath, cdlabel, cdmagic, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
		sprintf( sfdfile, "%s/%s/sfdfile.sfd", jkpath, cdlabel );
	} /*endif*/

	SeedSearchPosition( 0, sfdfile, stream, t_start, act_start, &status );
	if  (status == SeedERR_NEXTTIMEPOS)  {
		if  (!work_quiet)
			fprintf( stderr, "%s: stream %s, pos %s instead of %s\n",
				pa_progname(), stream, act_start, t_start );
		status = BC_NOERROR;
	} /*endif*/
	if  (Severe(&status))  {
		if  (!work_quiet)  {
			fprintf( stderr, "%s: time %s on stream %s not found\n",
				pa_progname(), t_start, stream );
			err_writemsg( status, "", TRUE );
		} /*endif*/
		printf( "0 kB found (CD %s)\n", cdlabel );
		return 0;
	} /*endif*/

	negative_gap = FALSE;
	rec_cnt = gap_cnt = 0;
	seedhdr = (SeedDataHeaderT *)seedrec;
	do  {
		status = cBcNoError;
		/* read next record */
		SeedReadNextRecord( 0, (SeedSbyteT *)seedrec, &hdr_swapped, &status );
		if  (status == SeedERR_NEXTTIMEPOS)  status = BC_NOERROR;
		if  (Severe(&status))  {
			if  (!work_quiet)  {
				fprintf( stderr, "%s: stream %s, error reading next record\n",
					pa_progname(), stream );
				err_writemsg( status, "", FALSE );
			} /*endif*/
			status = BC_NOERROR;
			break;
		} /*endif*/
		reclth = SeedGetReclth( seedhdr, FALSE );
		/* get start and end time */
		SeedRecordTime( seedrec, &recstime, &recetime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		/* if start time of record is out of requested window, return */
		if  (tc_ndiff(&endntime,&recstime,&status) < 0.0)  break;
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		/* change network code if requested */
		if  (*netcode != '\0')  {
			if  (overwrite_netcode || seedhdr->network[0] <= ' ')  {
				seedhdr->network[0] = netcode[0];
				seedhdr->network[1] = netcode[1];
			} /*endif*/
		} /*endif*/
		/* get dt on first time */
		if  (dt == 0.0)  dt = SeedGetSampleDist( seedhdr );
		if  (rec_cnt == 0 && exact)  {
			SeedCutStartRecord( seedrec, t_start, swap, &status );
			if  (Severe(&status))  err_writemsg( status, "", TRUE );
			if  (seedhdr->no_of_samples == 0)  continue;
		} /*endif*/
		tdiff = tc_ndiff( &endntime, &recetime, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		ctmp = seedrec[6];
		sprintf( (char *)seedrec, "%06d", ++rec_cnt );
		seedrec[6] = ctmp;
		bytesize += reclth;
		/* check for data gaps */
		if  (p_recstime.year != 0)  {
			gapdiff = tc_ndiff( &p_recetime, &recstime, &status );
			if  (Severe(&status))  err_writemsg( status, "", TRUE );
			if  (Abs(gapdiff) > dt/2.0)  gap_cnt++;
			negative_gap = (gapdiff < -dt/2.0);
			if  (negative_gap && no_negative_gaps)
				/* do not write record out and do not copy record times */
				/* to p_recsttime and p_recetime */
				continue;
		} /*endif*/
		if  (out != NULL)  {
			if  (tdiff <= 0.0 && exact)  {
				SeedCutEndRecord( seedrec, t_end, swap, &status );
				if  (Severe(&status))  err_writemsg( status, "", TRUE );
				if  (seedhdr->no_of_samples == 0)  {
					rec_cnt--;
					bytesize -= reclth;
					break;
				} /*endif*/
			} /*endif*/
			/* convert string data to uppercase */
			for  (i=0; i<5; i++)
				seedhdr->statcode[i] = Cap( seedhdr->statcode[i] );
			for  (i=0; i<3; i++)
				seedhdr->channel[i] = Cap( seedhdr->channel[i] );
			/* write out data */
			reclth = SeedGetReclth( seedhdr, TRUE );
			if  (last_reclth == 0)  last_reclth = reclth;
			if  (last_reclth != reclth)  {
				/* open new file */
				fclose( out );
				if  (fcnt > 0)  {
					/* use given name and append counter */
					sprintf( outfile+outfile_lth, ".%d", fcnt++ );
				} else {
					/* new standard name */
					tc_n2t( &recstime, timestr, &status );
					if  (Severe(&status))  err_writemsg( status, "", TRUE );
					standard_name( stream, timestr, outfile );
				} /*endif*/
				out = fopen( outfile, "wb" );
				if  (out == NULL)  {
					fprintf( stderr, "%s: output file %s couldn't be opened\n",
						pa_progname(), outfile );
					return 1;
				} /*endif*/
			} /*endif*/
			if  (fwrite((char *)seedrec,reclth,1,out) != 1)  {
				fprintf( stderr, "%s: write error on file %s\n",
					pa_progname(), outfile );
				fclose( out );
				return 1;
			} /*endif*/
			last_reclth = reclth;
		} /*endif*/
		p_recstime = recstime;
		p_recetime = recetime;
	}  while (tdiff > 0.0);
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	printf( "%d kB found (%d records, %d gaps) on stream %s (CD %s)\n",
		bytesize/1024, rec_cnt, gap_cnt, stream, cdlabel );

	if  (out != NULL)  fclose( out );

	return 0;

} /* end of main */




/*---------------------------------------------------------------------------*/



static void standard_name( char stream[], char start[], char name[] )

/* computes standard name for output file
 *
 * parameters of routine
 * char       stream[];       input; stream string
 * char       start[];        input; start time
 * char       name[];         output; name of file
 */
{
	/* local variables */
	char     lstream[BC_SHORTSTRLTH+1];  /* local stream copy */
	char     station[BC_SHORTSTRLTH+1];  /* name of station */
	char     channel[BC_SHORTSTRLTH+1];  /* name of channel */
	char     cmpstr[BC_SHORTSTRLTH+1];   /* component */
	char     *cptr;                      /* moving pointer */
	NTIME    ntime;                      /* numeric time */
	STATUS   locstat;                    /* local status */

	/* executable code */

	if  (strlen(stream) > BC_SHORTSTRLTH)  {
		strcpy( name, "stream-too-long" );
		return;
	} /*endif*/

	strcpy( lstream, stream );
	cptr = lstream;
	while  (*cptr != '\0')  {
		if  (*cptr == '-')  *cptr = ' ';
		cptr++;
	} /*endwhile*/
	sscanf( lstream, "%s %s %s", station, channel, cmpstr );

	locstat = BC_NOERROR;
	tc_t2n( start, &ntime, &locstat );
	if  (Severe(&locstat))  {
		strcpy( name, "illegal-start-time" );
		return;
	} /*endif*/
	if  (ntime.year > 1900)  ntime.year -= 1900;
	if  (ntime.year >= 100)  ntime.year -= 100;

	sprintf( name, "%s_%02d%02d%02d_%02d%02d.%s%c", station,
		ntime.year, ntime.month, ntime.day, ntime.hour, ntime.min,
		channel, *cmpstr );

} /* end of standard_name */



/*---------------------------------------------------------------------------*/

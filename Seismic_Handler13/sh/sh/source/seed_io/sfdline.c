
/* file sfdline.c
 *      =========
 *
 * version 15, 17-Jun-2005
 *
 * returns sfd line for specified SEED file.  It is assumed that the SEED
 * file contains a single channel (read from the first record) and has
 * SEED records of monotonically increasing times.
 * K. Stammler, 8-Apr-94
 *
 * v5, 23-Nov-94, K. Stammler, included swap option
 */


#include <stdio.h>
#include <string.h>
#include BASECNST
#include BC_SYSBASE
#include BC_CPAR
#include BC_TCUSRDEF
#include BC_UTUSRDEF
#include BC_ERUSRDEF
#include "seedcfg.h"
#include "seed_lib.h"


#define TIMECORRUNIT 1000.0



int main( int argc, char *argv[] )
{
	/* local variables */
	int      recsize;                   /* SEED record size in bytes */
	char     recsize_par[cBcLineLth];   /* record size parameter string */
	STATUS   status;                    /* return status */
	SeedSbyteT *seedrec;                /* pointer to SEED record */
	SeedDataHeaderT *seedhdr;           /* pointer to seed data header */
	char     seedfile[BC_FILELTH+1];    /* name of SEED file */
	int      read_ret;                  /* fread return values */
	NTIME    ntime;                     /* numeric time */
	FILE     *fp;                       /* pointer to SEED file */
	char     sfd_t_start[BC_TIMELTH+1]; /* start time */
	char     sfd_t_end[BC_TIMELTH+1];   /* end time */
	int      sfd_recno;                 /* number of records in file */
	char     sfd_stream[BC_LINELTH+1];  /* stream string */
	BOOLEAN  sfd_swap_hdr;              /* swap header necessary */
	int      i;                         /* counter */
	long     fsize;                     /* file size */
	float    dt;                        /* sample distance in sec */
	char     *csrc, *cdst;              /* moving pointers */
	char     outfile[BC_FILELTH+1];     /* output file */
	FILE     *out;                      /* pointer to output file */
	char     chanstr[BC_SHORTSTRLTH+1]; /* scratch string for channel */
	int      byteoff;                   /* byte offset at beginning */
	int      errcnt;                    /* error counter */
	TSyBoolean include_bad;             /* include bad data files */
	TSyBoolean use_timecorr;            /* use time correction */

	/* executable code */

	status = BC_NOERROR;
	recsize = 0;  /* must be set or detected later, otherwise abort */

	pa_init( argc, argv );
	if  (pa_pnumber() < 1 || pa_pnumber() > 2)  {
		fprintf( stderr, "Usage: %s <seed-file> [<outfile>]***\n",
			pa_progname() );
		return 1;
	} /*endif*/
	strcpy( seedfile, pa_pvalue(1) );
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

	/* check for record size qualifier */
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

	/* check for offset specified */
	byteoff = 0;
	if  (pa_qspecified("-byteoff"))
		sscanf( pa_qvalue("-byteoff"), "%d", &byteoff );

	include_bad = FALSE;
	if  (pa_qspecified("-include_bad"))
		include_bad = TRUE;

	use_timecorr = FALSE;
	if  (pa_qspecified("-timecorr"))
		use_timecorr = TRUE;

	/* open seed file */
	fp = fopen( seedfile, "rb" );
	if  (fp == NULL)  {
		fprintf( stderr, "*** %s: file %s not found\n", pa_progname(), seedfile );
		fclose( out );
		return 1;
	} /*endif*/

	/* determine record size if not specified */
	if  (recsize == 0)  {
		SeedQuickFindReclth( fp, &recsize, &byteoff );
		if  (recsize <= 0)  {
			fclose( fp );
			if  (recsize == 0)  {
				fprintf( stderr, "%s: no valid SEED file %s\n",
					pa_progname(), seedfile );
			} else {
				fprintf( stderr, "%s: cannot find record size in %s\n",
					pa_progname(), seedfile );
			} /*endif*/
			return 1;
		} /*endif*/
	} /*endif*/

	/* allocate memory for SEED record */
	seedrec = (SeedSbyteT *)sy_allocmem( 1, recsize, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* skip offset */
	if  (byteoff > 0)
		fseek( fp, byteoff, 0 );

	/* read first record */
	read_ret = (int)fread( (char *)seedrec, recsize, 1, fp );
	if  (read_ret != 1)  {
		fprintf( stderr, "*** %s: read error on file %s\n",
			pa_progname(), seedfile );
		fclose( fp );
		fclose( out );
		return 1;
	} /*endif*/
	seedhdr = (SeedDataHeaderT *)seedrec;
	sfd_swap_hdr = SeedSwapNecessary( seedhdr );
	if  (sfd_swap_hdr)  SeedSwapHeader( seedhdr );

	/* find start time */
	SeedBtimeToNtime( &(seedhdr->starttime), &ntime, &status );
	if  (Severe(&status))  {
		fprintf( stderr, "%s: couldn't read start time in file %s\n",
			pa_progname(), seedfile );
		fclose( fp );
		fclose( out );
		return 1;
	} /*endif*/
	/* check for time correction, K.S. 16-Mar-99 */
	if  (use_timecorr && (seedhdr->timecorr != 0)
		&& (((int)(seedhdr->activity) & Seed_F_ACT_TIMECORRAPP) == 0))  {
		tc_nadd( &ntime, (float)(seedhdr->timecorr)/TIMECORRUNIT,
			&ntime, &status );
		if  (Severe(&status))  {
			fprintf( stderr, "%s: couldn't apply time correction in file %s\n",
				pa_progname(), seedfile );
			fclose( fp );
			fclose( out );
			return 1;
		} /*endif*/
	} /*endif*/
	tc_n2t( &ntime, sfd_t_start, &status );
	if  (Severe(&status))  {
		fprintf( stderr, "%s: couldn't convert start time in file %s\n",
			pa_progname(), seedfile );
		fclose( fp );
		fclose( out );
		return 1;
	} /*endif*/

	/* channel name */
	csrc = seedhdr->statcode;
	cdst = sfd_stream;
	*cdst = '\0';
	i = 0;
	/* exit loop after 5 chars maximum, added 17-Jun-2005, K.S. */
	while  (*csrc > ' ' && *csrc <= 'z' && i < 5)  {
		*cdst++ = *csrc++;
		i++;
	} /*endwhile*/
	*cdst++ = '-';
	*cdst = '\0';
	chanstr[0] = Cap( seedhdr->channel[0] );
	chanstr[1] = Cap( seedhdr->channel[1] );
	chanstr[2] = '\0';
	if  (chanstr[0] <= ' ' || chanstr[0] > 'z')  chanstr[0] = '\0';
	if  (chanstr[1] <= ' ' || chanstr[1] > 'z')  chanstr[1] = '\0';
	ut_uncap( chanstr );
#ifdef XXX
	if  (strcmp(chanstr,"bh") == 0)  {         strcat( sfd_stream, "vbb-" );
	} else if  (strcmp(chanstr,"hh") == 0)  {  strcat( sfd_stream, "vsp-" );
	} else if  (strcmp(chanstr,"lh") == 0)  {  strcat( sfd_stream, "lp-" );
	} else {
		strcat( sfd_stream, chanstr );
		strcat( sfd_stream, "-" );
	} /*endif*/
#endif
	strcat( sfd_stream, chanstr );
	strcat( sfd_stream, "-" );
	i = (int)strlen( sfd_stream );
	sfd_stream[i] = seedhdr->channel[2];
	if  (sfd_stream[i] <= ' ' || sfd_stream[i] > 'z')  sfd_stream[i] = '\0';
	if  (sfd_stream[i] == '?' || sfd_stream[i] == '*')  sfd_stream[i] = 'x';
	sfd_stream[i+1] = '\0';
	ut_uncap( sfd_stream );

	/* get sample distance */
	if  (!include_bad)  {
		if  (seedhdr->smprate_fact == 0 || seedhdr->smprate_mult == 0)  {
			fprintf( stderr, "%s: ignore bad data file %s\n", argv[0], seedfile );
			fclose( fp );
			return 1;
		} /*endif*/
	} /*endif*/
	dt = SeedGetSampleDist( seedhdr );

	/* get length of file */
	fseek( fp, 0, 2 );
	fsize = ftell( fp ) - byteoff;
	if  (fsize % recsize != 0)  {
		fprintf( stderr, "*** %s: illegal size %ld bytes of SEED file %s\n",
			pa_progname(), fsize, seedfile );
		fclose( fp );
		fclose( out );
		return 1 ;
	} /*endif*/
	sfd_recno = (int)(fsize / (long)recsize);

	/* get last record */
	fseek( fp,
		(long)(sfd_recno-1)*(long)recsize + (long)byteoff, 0 );
	read_ret = (int)fread( (char *)seedrec, recsize, 1, fp );
	if  (read_ret != 1)  {
		fprintf( stderr, "*** %s: read error on file %s (end)\n",
			pa_progname(), seedfile );
		fclose( fp );
		fclose( out );
		return 1;
	} /*endif*/
	seedhdr = (SeedDataHeaderT *)seedrec;
	if  (SeedSwapNecessary(seedhdr))  SeedSwapHeader(seedhdr);
	errcnt = 0;
	SeedBtimeToNtime( &(seedhdr->starttime), &ntime, &status );
	if  (status == cBcNoError)
		tc_nadd( &ntime, (float)(seedhdr->no_of_samples)*dt, &ntime, &status );
	while  (Severe(&status))  {
		errcnt++;
		fprintf( stderr,
			"%s: unreadable last record, take %d before in file %s\n",
			pa_progname(), errcnt, seedfile );
		status = cBcNoError;
		/* get previous record */
		sfd_recno--;
		fseek( fp,
			(long)(sfd_recno-1)*(long)recsize + (long)byteoff,
			0 );
		read_ret = (int)fread( (char *)seedrec, recsize, 1, fp );
		if  (read_ret != 1)  {
			fprintf( stderr, "%s: read error on file %s (end)\n",
				pa_progname(), seedfile );
			fclose( fp );
			fclose( out );
			return 1;
		} /*endif*/
		seedhdr = (SeedDataHeaderT *)seedrec;
		if  (SeedSwapNecessary(seedhdr))  SeedSwapHeader(seedhdr);
		SeedBtimeToNtime( &(seedhdr->starttime), &ntime, &status );
		if  (status == cBcNoError)
			tc_nadd( &ntime, (float)(seedhdr->no_of_samples)*dt, &ntime, &status );
		if  (Severe(&status) && errcnt > 10)  {
			fprintf( stderr,
				"%s: couldn't read start time of last record in file %s\n",
				pa_progname(), seedfile );
			fclose( fp );
			fclose( out );
			return 1;
		} /*endif*/
	} /*endwhile*/
	tc_n2t( &ntime, sfd_t_end, &status );
	if  (Severe(&status))  {
		fprintf( stderr, "%s: couldn't convert end time in file %s\n",
			pa_progname(), seedfile );
		fclose( fp );
		fclose( out );
		return 1;
	} /*endif*/

	fclose( fp );

	fprintf( out, "%c>%s %c>%s %c>%s %c>%s %c>%d %c>%d %c>%d %c>%d\n",
		Seed_C_SfdStream, sfd_stream, Seed_C_SfdName, seedfile,
		Seed_C_SfdTStart, sfd_t_start, Seed_C_SfdTEnd, sfd_t_end,
		Seed_C_SfdRecno, sfd_recno, Seed_C_SfdSwapH, sfd_swap_hdr,
		Seed_C_SfdReclth, recsize, Seed_C_SfdOffset, byteoff );

	if  (out != stdout)  fclose( out );

	return 0;

} /* end of main */



/*----------------------------------------------------------------------------*/


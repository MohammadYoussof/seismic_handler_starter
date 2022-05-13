
/* file seed2sfd.c
 *      ========
 *
 * version 2, 8-Mar-2006
 *
 * Writes out sfd entries for reading from a seed volume.  Header entries
 * are ignored.
 * K. Stammler, 7-Mar-2006
 *
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
	char     seedfile[cBcFileLth+1];    /* name of SEED file */
	int      read_ret;                  /* fread return values */
	NTIME    ntime;                     /* numeric time */
	FILE     *fp;                       /* pointer to SEED file */
	char     cur_t_start[cBcTimeLth+1]; /* start time of current record */
	char     sfd_t_start[cBcTimeLth+1]; /* start time for sfd output */
	char     cur_t_end[cBcTimeLth+1];   /* end time of current record */
	char     sfd_t_end[cBcTimeLth+1];   /* end time for sfd output */
	int      cur_recno;                 /* current record number */
	int      sfd_recno;                 /* record number of sfd output */
	char     cur_stream[cBcLineLth+1];  /* current stream string */
	char     sfd_stream[cBcLineLth+1];  /* stream string for sfd output */
	char     cur_addinf[cBcLineLth+1];  /* netcode + locid in current record */
	char     sfd_addinf[cBcLineLth+1];  /* netcode + locid */
	BOOLEAN  sfd_swap_hdr;              /* swap header necessary */
	int      i;                         /* counter */
	long     fsize;                     /* file size */
	float    dt, prev_dt;               /* sample distance in sec */
	char     *csrc, *cdst;              /* moving pointers */
	char     outfile[cBcFileLth+1];     /* output file */
	FILE     *out;                      /* pointer to output file */
	char     chanstr[cBcShortStrLth+1]; /* scratch string for channel */
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
		out = fopen( outfile, "w" );
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

	/* loop all records */
	sfd_stream[0] = cur_stream[0] = sfd_t_start[0] = sfd_addinf[0] = '\0';
	cur_recno = 0;
	prev_dt = 0.0;
	while  ((int)fread( (char *)seedrec, recsize, 1, fp ) == 1)  {

		seedhdr = (SeedDataHeaderT *)seedrec;
		sfd_swap_hdr = SeedSwapNecessary( seedhdr );
		if  (sfd_swap_hdr)  SeedSwapHeader( seedhdr );
		cur_recno++;

		/* skip if not data record */
		if  (seedhdr->indicator != 'D' && seedhdr->indicator != 'R'
			&& seedhdr->indicator != 'Q')  continue;

		/* get channel name */
		csrc = seedhdr->statcode;
		cdst = cur_stream;
		*cdst = '\0';
		i = 0;
		/* exit loop after 5 chars maximum */
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
		strcat( cur_stream, chanstr );
		strcat( cur_stream, "-" );
		i = (int)strlen( cur_stream );
		cur_stream[i] = seedhdr->channel[2];
		if  (cur_stream[i] <= ' ' || cur_stream[i] > 'z')  cur_stream[i] = '\0';
		if  (cur_stream[i] == '?' || cur_stream[i] == '*')  cur_stream[i] = 'x';
		cur_stream[i+1] = '\0';
		ut_uncap( cur_stream );

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
		tc_n2t( &ntime, cur_t_start, &status );
		if  (Severe(&status))  {
			fprintf( stderr, "%s: couldn't convert start time in file %s\n",
				pa_progname(), seedfile );
			fclose( fp );
			fclose( out );
			return 1;
		} /*endif*/

		/* get sample distance */
		if  (seedhdr->smprate_fact == 0 || seedhdr->smprate_mult == 0)  {
			fprintf( stderr, "%s: bad dt in stream %s\n", argv[0], sfd_stream );
			dt = 0.0;
		} else {
			dt = SeedGetSampleDist( seedhdr );
		} /*endif*/

		/* find end time of record */
		tc_tadd( cur_t_start, dt*(float)(seedhdr->no_of_samples), cur_t_end,
			&status );
		if  (Severe(&status))  {
			fprintf( stderr, "%s: couldn't compute end time in file %s\n",
				pa_progname(), seedfile );
			fclose( fp );
			fclose( out );
			return 1;
		} /*endif*/

		/* get netcode and locid */
		cur_addinf[0] = seedhdr->network[0];
		cur_addinf[1] = seedhdr->network[1];
		cur_addinf[2] = seedhdr->locid[0];
		cur_addinf[3] = seedhdr->locid[1];
		cur_addinf[4] = '\0';
		for  (i=0; i<4; i++)  if  (cur_addinf[i] == ' ')  cur_addinf[i] = '.';

		/* if new stream, write out old stream and initialise new */
		if  ((strcmp(cur_stream,sfd_stream) != 0) || (dt != prev_dt)
			|| (strcmp(cur_addinf,sfd_addinf) != 0))  {

			if  (sfd_stream[0] != '\0')  {
				if  (prev_dt == 0.0)  {
					fprintf( stderr, "stream %s ignored due to bad dt\n", cur_stream );
				} else {
					fprintf( out, "%c>%s %c>%s %c>%s %c>%s %c>%d %c>%d %c>%d %c>%d",
						Seed_C_SfdStream, sfd_stream, Seed_C_SfdName, seedfile,
						Seed_C_SfdTStart, sfd_t_start, Seed_C_SfdTEnd, sfd_t_end,
						Seed_C_SfdRecno, cur_recno-sfd_recno, Seed_C_SfdSwapH, sfd_swap_hdr,
						Seed_C_SfdReclth, recsize, Seed_C_SfdOffset, (sfd_recno-1)*recsize );
					if  (strcmp(sfd_addinf,"....") != 0)
						fprintf( out, " %c>%s", Seed_C_SfdAddinf, sfd_addinf );
					fprintf( out, "\n" );
				} /*endif*/
			} /*endif*/

			/* initalise new stream info */
			strcpy( sfd_stream, cur_stream );
			strcpy( sfd_t_start, cur_t_start );
			strcpy( sfd_addinf, cur_addinf );
			sfd_recno = cur_recno;

		} /*endif*/

		strcpy( sfd_t_end, cur_t_end );
		prev_dt = dt;

	} /*endwhile*/

	if  (dt == 0.0)  {
		fprintf( stderr, "stream %s ignored due to bad dt\n", cur_stream );
	} else {
		if  (sfd_stream[0] != '\0')  {
			fprintf( out, "%c>%s %c>%s %c>%s %c>%s %c>%d %c>%d %c>%d %c>%d",
				Seed_C_SfdStream, sfd_stream, Seed_C_SfdName, seedfile,
				Seed_C_SfdTStart, sfd_t_start, Seed_C_SfdTEnd, sfd_t_end,
				Seed_C_SfdRecno, cur_recno-sfd_recno, Seed_C_SfdSwapH, sfd_swap_hdr,
				Seed_C_SfdReclth, recsize, Seed_C_SfdOffset, (sfd_recno-1)*recsize );
				if  (strcmp(sfd_addinf,"....") != 0)
					fprintf( out, " %c>%s", Seed_C_SfdAddinf, sfd_addinf );
				fprintf( out, "\n" );
		} /*endif*/
	} /*endif*/

	if  (out != stdout)  fclose( out );

	return 0;

} /* end of main */



/*----------------------------------------------------------------------------*/


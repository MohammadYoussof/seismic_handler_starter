
/* file seedquickdump.c
 *      ===============
 *
 * version 9, 9-Feb-2007
 *
 * Dumps out Headerinfo of SEED files.
 * K. Stammler, 10-Aug-98
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


typedef struct {
	WORD     block_id;        /* blockette ID = 1000 */
	WORD     next_block;      /* next blockette */
	UBYTE    algorithm;       /* compression algorithm */
	UBYTE    dunno1;          /* ... */
	UBYTE    reclthpow;       /* power of record length */
	UBYTE    dunno2;          /* ... */
} SeedDataOnlyBlocketteT;

typedef struct {
	WORD     block_id;        /* blockette ID = 1001 */
	WORD     next_block;      /* next blockette */
	UBYTE    timequal;        /* time quality */
	UBYTE    dunno1;          /* ...*/
} SeedDataExtBlocketteT;



int main( int argc, char *argv[] )
{
	/* local variables */
	STATUS   status;                    /* return status */
	int      recsize;                   /* SEED record size in bytes */
	char     recsize_par[cBcLineLth];   /* record size parameter string */
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
	int      reccnt;                    /* record counter */
	char     seq[cBcLineLth+1];         /* sequence number string */
	SeedDataOnlyBlocketteT *donly;      /* data only blockette */
	SeedDataExtBlocketteT *dext;        /* data extension blockette */
	UWORD    tmp;                       /* scratch */
	int      time_quality;              /* time quality factor */
	char     lasttime[cBcTimeLth+1];    /* time of last record */
	float    gaplth;                    /* length of gap in s */
	unsigned long int usign;            /* first sample values */
	BOOLEAN  print_locid;               /* print location ID */
	char     showinfo[cBcLineLth+1];    /* show info qualifier */
	BOOLEAN  b1000found;                /* blockette 1000 found */
	int      gap_count=0;               /* gap counter */
	float    gap_length=0.0;            /* total gap length in s */
	int      jitter_count=0;            /* counts very small 'gaps' (<dt/2) */
	BOOLEAN  write_newline;             /* write a new line after record */

	/* executable code */

	status = cBcNoError;
	print_locid = FALSE;

	pa_init( argc, argv );
	if  (pa_pnumber() < 1 || pa_pnumber() > 2)  {
		fprintf( stderr, "Usage: %s <seed-file> \n",
			pa_progname() );
		return 1;
	} /*endif*/
	strcpy( seedfile, pa_pvalue(1) );
	out = stdout;

	/* check for record size qualifier */
	recsize = 0;
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

	if  (pa_qspecified("-locid"))  print_locid = TRUE;

	*showinfo = '\0';
	if  (pa_qvalue("-show") != NULL)  {
		strcpy( showinfo, pa_qvalue("-show") );
	} /*endif*/

	/* allocate memory for SEED record */
	seedrec = (SeedSbyteT *)sy_allocmem( 1, Seed_C_MAX_RECLTH, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

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

	reccnt = 0;
	gaplth = 0.0;
	*lasttime = '\0';

	/* read records */
	FOREVER  {

		reccnt++;

		/* read next record */
		read_ret = (int)fread( (char *)seedrec, recsize, 1, fp );
		if  (read_ret != 1)  break;

		/* check for swapping */
		seedhdr = (SeedDataHeaderT *)seedrec;
		sfd_swap_hdr = SeedSwapNecessary( seedhdr );
		if  (sfd_swap_hdr)  SeedSwapHeader( seedhdr );

		strncpy( seq, seedhdr->seqno, 6 );
		seq[6] = '\0';
		if  (seedhdr->indicator != 'D' && seedhdr->indicator != 'R'
			&& seedhdr->indicator != 'Q')  {
			if  (*showinfo == '\0')
				printf( "%06d %6s %c\n", reccnt, seq, seedhdr->indicator );
			continue;
		} /*endif*/

		/* find start time */
		SeedBtimeToNtime( &(seedhdr->starttime), &ntime, &status );
		if  (Severe(&status))  {
			fprintf( stderr, "%s: couldn't read start time in file %s\n",
				pa_progname(), seedfile );
			fclose( fp );
			fclose( out );
			return 1;
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
		while  (*csrc > ' ' && *csrc <= 'z')  *cdst++ = *csrc++;
		*cdst++ = '-';
		*cdst = '\0';
		chanstr[0] = Cap( seedhdr->channel[0] );
		chanstr[1] = Cap( seedhdr->channel[1] );
		chanstr[2] = '\0';
		if  (chanstr[0] <= ' ' || chanstr[0] > 'z')  chanstr[0] = '\0';
		if  (chanstr[1] <= ' ' || chanstr[1] > 'z')  chanstr[1] = '\0';
		ut_uncap( chanstr );

		strcat( sfd_stream, chanstr );
		strcat( sfd_stream, "-" );
		i = (int)strlen( sfd_stream );
		sfd_stream[i] = seedhdr->channel[2];
		if  (chanstr[i] <= ' ' || chanstr[i] > 'z')  chanstr[i] = '\0';
		sfd_stream[i+1] = '\0';
		ut_uncap( sfd_stream );

		/* get sample distance */
		if  (seedhdr->smprate_fact == 0 || seedhdr->smprate_mult == 0)  {
			dt = 0.0;
		} else {
			dt = SeedGetSampleDist( seedhdr );
		} /*endif*/

		tc_tadd( sfd_t_start, (float)(seedhdr->no_of_samples)*dt, sfd_t_end,
			&status );
		if  (Severe(&status))  {
			fprintf( stderr, "%s: couldn't compute end time in file\n",
				pa_progname(), seedfile );
			fclose( fp );
			fclose( out );
			return 1;
		} /*endif*/
		if  (*lasttime != '\0')
			gaplth = tc_tdiff( sfd_t_start, lasttime, &status );

		b1000found = FALSE;
		time_quality = -1;
		if  (seedhdr->first != 0)  {
			donly = (SeedDataOnlyBlocketteT *)(seedrec+(seedhdr->first));
#ifdef XXX
			/* this is done in SeedSwapHeader now */
			if  (sfd_swap_hdr)  {
				tmp = (UWORD)(donly->block_id);
				donly->block_id = (tmp & 0xff) * 0x100;
				donly->block_id += (UWORD)(tmp & 0xff00) / (UWORD)0x100;
				tmp = (UWORD)(donly->next_block);
				donly->next_block = (tmp & 0xff) * 0x100;
				donly->next_block += (UWORD)(tmp & 0xff00) / (UWORD)0x100;
			} /*endif*/
#endif
			b1000found = (donly->block_id == 1000);
			if  (b1000found && donly->next_block > 0)  {
				dext = (SeedDataExtBlocketteT *)(seedrec+(donly->next_block));
#ifdef XXX
				/* this is done in SeedSwapHeader now */
				if  (sfd_swap_hdr)  {
					tmp = (UWORD)(dext->block_id);
					dext->block_id = (tmp & 0xff) * 0x100;
					dext->block_id += (UWORD)(tmp & 0xff00) / (UWORD)0x100;
				} /*endif*/
#endif
				if  (dext->block_id == 1001)  {
					time_quality = dext->timequal;
				} /*endif*/
				if  (!b1000found && dext->block_id == 1000)  b1000found = TRUE;
			} /*endif*/
		} /*endif*/

		usign = *((unsigned long int *)(seedrec+68));
		/*
		tmp = usign & 0x0000ffff;
		usign >>= 16;
		usign += (unsigned long)tmp * 65536;
		*/

		write_newline = TRUE;
		if  (*showinfo == '\0')  {
			printf( "%06d %6s %c %s %s %5d %7.5f %d ", reccnt, seq, seedhdr->indicator,
				sfd_stream, sfd_t_start, seedhdr->no_of_samples, dt, time_quality );

			if  (gaplth == 0.0)  {
				printf( "cont" );
			} else {
				printf( "%g", gaplth );
				if (Abs(gaplth) > (dt/2.0))  {
					gap_length += Abs(gaplth);
					gap_count++;
				} else {
					jitter_count++;
				} /*endif*/
			} /*endif*/

			if  (seedhdr->timecorr != 0)  printf( " (%d)", seedhdr->timecorr );

			if  (print_locid)  printf( " %c%c", seedhdr->locid[0], seedhdr->locid[1] );

		} else {

			if  (gaplth != 0.0)  {
				if (Abs(gaplth) > (dt/2.0))  {
					gap_length += Abs(gaplth);
					gap_count++;
				} else {
					jitter_count++;
				} /*endif*/
			} /*endif*/

			if  (strcmp(showinfo,"netcode") == 0)  {
				printf( "%c%c ", seedhdr->network[0], seedhdr->network[1] );
			} else if  (strcmp(showinfo,"timecorr") == 0)  {
				printf( "%d ", seedhdr->timecorr );
			} else if  (strcmp(showinfo,"activity") == 0)  {
				printf( "0x%x ", (int)(seedhdr->activity) );
			} else if  (strcmp(showinfo,"b1000") == 0)  {
				printf( "%d ", b1000found );
			} else {
				write_newline = FALSE;
			} /*endif*/

		} /*endif*/

		if  (write_newline)  printf( "\n" );

		strcpy( lasttime, sfd_t_end );

	} /*endfor*/

	if  (strcmp(showinfo,"gaps") == 0)  {
		printf( "gap_count %d gap_length %f jitter_count %d\n",
			gap_count, gap_length, jitter_count );
	} /*endif*/

	fclose( fp );
	return 0;

#ifdef XXX


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
	while  (Severe(&status))  {
		errcnt++;
		fprintf( stderr,
			"%s: unreadable last record, take %d before in file %s\n",
			pa_progname(), errcnt, seedfile );
		status = BC_NOERROR;
		/* get previous record */
		sfd_recno--;
		fseek( fp,
			(long)(sfd_recno-1)*(long)recsize + (long)byteoff,
			0 );
		read_ret = (int)fread( (char *)seedrec, recsize, 1, fp);
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
		if  (Severe(&status) && errcnt > 10)  {
			fprintf( stderr,
				"%s: couldn't read start time of last record in file %s\n",
				pa_progname(), seedfile );
			fclose( fp );
			fclose( out );
			return 1;
		} /*endif*/
	} /*endwhile*/
	tc_nadd( &ntime, (float)(seedhdr->no_of_samples)*dt, &ntime, &status );
	if  (Severe(&status))  {
		fprintf( stderr, "%s: couldn't compute end time in file\n",
			pa_progname(), seedfile );
		fclose( fp );
		fclose( out );
		return 1;
	} /*endif*/
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

#endif

} /* end of main */

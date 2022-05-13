
/* file sc_logmsg.c
 *      ===========
 *
 * version 3, 24-Sep-2007
 *
 * Write log messages of LOG.L stream
 * K. Stammler, 10-Dec-99
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
#include "seedcfg.h"
#include "seed_lib.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	int      recsize;                   /* record size */
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
	int      reccnt;                    /* record counter */
	char     text[cBcLongStrLth+1];     /* output text */
	int      print_html;                /* html output */

	/* executable code */

	status = cBcNoError;
	recsize = 512;
	byteoff = 512;

	pa_init( argc, argv );
	if  (pa_pnumber() < 1 || pa_pnumber() > 2)  {
		fprintf( stderr, "Usage: %s <seed-file> \n",
			pa_progname() );
		return 1;
	} /*endif*/
	strcpy( seedfile, pa_pvalue(1) );
	out = stdout;

	print_html = pa_qspecified( "-html" );

	/* allocate memory for SEED record */
	seedrec = (SeedSbyteT *)sy_allocmem( 1, recsize, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	fp = fopen( seedfile, "rb" );
	if  (fp == NULL)  {
		fprintf( stderr, "*** %s: file %s not found\n", pa_progname(), seedfile );
		fclose( out );
		return 1;
	} /*endif*/

	reccnt = 0;

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

		if  (seedhdr->indicator != 'D')  {
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

		i = (cBcLongStrLth > seedhdr->no_of_samples) ?
			seedhdr->no_of_samples : cBcLongStrLth;
		strncpy( text, (char *)seedhdr+(seedhdr->databegin), i );
		text[i] = '\0';
		if  (i > 0 && text[i-1] < ' ')  text[i-1] = '\0';
		if  (i > 1 && text[i-2] < ' ')  text[i-2] = '\0';

		/* printf( "%06d %s %s %s\n", reccnt, sfd_stream, sfd_t_start, text ); */
		if  (print_html)  printf( "<br>" );
		printf( "%s %s %s\n", sfd_t_start, sfd_stream, text );

	} /*endfor*/

	fclose( fp );
	return 0;

} /* end of main */

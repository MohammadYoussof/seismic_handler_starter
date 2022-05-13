
/* file makesfd.c
 *      =========
 *
 * version 1, 29-Dec-93
 *
 * Creates SEED file directory file from seed_tape LOG file.
 * K. Stammler, 29-Dec-93
 */




#include <stdio.h>
#include <string.h>
#include BASECNST
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_CPAR
#include "seedcfg.h"



/*
 * prototypes of local routines
 * ----------------------------
 */

static void do_abort( STATUS status );
static void ScanLogfile( FILE *log, FILE *sfd );



int main( int argc, char *argv[] )
{
	/* local variables */
	char     inpfile[BC_FILELTH+1];     /* name of input log file */
	char     outsfd[BC_FILELTH+1];      /* name of output sfd file */
	FILE     *log, *sfd;                /* file pointers */
	STATUS   status;                    /* return status */
	char     *def_dir;                  /* default directory */
	char     setupfile[BC_FILELTH+1];   /* SEED setup file */

	/* executable code */

	/* get parameters */
	pa_init( argc, argv );
	if  (pa_pnumber() != 2)  {
		printf( "*** Usage: %s <input-log> <output-sfd> ***\n", argv[0] );
		return 0;
	} /*endif*/
	strcpy( inpfile, pa_pvalue(1) );
	strcpy( outsfd, pa_pvalue(2) );

	/* get setup file */
	def_dir = getenv( "SEED_INPUTS" );
	if  (def_dir == NULL)  {
		strcpy( setupfile, "seed_setup.txt" );
	} else {
		strcpy( setupfile, def_dir );
		strcat( setupfile, "/" );
		strcat( setupfile, "seed_setup.txt" );
	} /*endif*/
	if  (pa_qspecified("-setup"))
		strcpy( setupfile, pa_qvalue("-setup") );

	/* do setup */
	status = BC_NOERROR;
	SeedReadSetupFile( setupfile, &status );
	if  (Severe(&status))  do_abort( status );
	SeedInitialize( &status );
	if  (Severe(&status))  do_abort( status );

	/* open files */
	log = fopen( inpfile, "r" );
	if  (log == NULL)  {
		printf( "*** couldn't open input file %s ***\n", inpfile );
		return 1;
	} /*endif*/
	sfd = fopen( outsfd, "w" );
	if  (sfd == NULL)  {
		printf( "*** couldn't open output file %s ***\n", outsfd );
		fclose( log );
		return 1;
	} /*endif*/

	ScanLogfile( log, sfd );

	/* close files */
	fclose( log );
	fclose( sfd );

	return 0;

} /* end of main */



/*---------------------------------------------------------------------*/



static void do_abort( STATUS status )

/* Aborts execution
 *
 * parameters of routine
 * STATUS     status;      input; error status
 */
{
	/* executable code */

	printf( "*** Abort.   Status: %d\n", status );
	exit( 1 );

} /* end of do_abort */



/*---------------------------------------------------------------------*/



static void ScanLogfile( FILE *log, FILE *sfd )

/* Scans input log file and create SEED file directory on 'sfd'
 *
 * parameters of routine
 * FILE       *log;          input; pointer to input log file
 * FILE       *sfd;          input; pointer to output sfd file
 */
{
	/* local variables */
	int      stream_idx;              /* stream index */
	char     line[BC_LINELTH+1];      /* current line */
	char     prev_line[BC_LINELTH+1]; /* previous line */
	int      curr_idx;                /* current stream index */
	char     stream_str[BC_LINELTH+1];/* stream string */
	char     *p_file;                 /* pointer to file name */
	char     seedfile[BC_LINELTH+1];  /* SEED filename */
	char     *ch;                     /* char pointer */
	char     tstart[BC_LINELTH+1];    /* start time */
	char     tend[BC_LINELTH+1];      /* end time */
	int      rec_num;                 /* number of records in file */
	int      slen;                    /* string length */

	/* executable code */

	/* loop all streams */
	SeedNextStream( Seed_C_NSM_RESET );
	while  (Seed_C_ILLEGAL
		!= (stream_idx = SeedNextStream(Seed_C_NSM_NEXT_STREAM)))  {

		SeedGetStreamString( stream_idx, stream_str );
		fprintf( sfd, "*%s\n", stream_str );

		fseek( log, 0, 0 );   /* rewind file */
		*prev_line = '\0';
		while  (fgets(line,BC_LINELTH,log) != NULL)  {

			/* if line isn't an open message, forget it */
			if  (*line != '$' || strncmp(line,"$ file ",7) != 0)  {
				strcpy( prev_line, line );
				continue;
			} /*endif*/

			/* get stream string from previous line */
			ch = strstr( prev_line, "stream " );
			if  (ch == NULL)  continue;
			ch += 7;
			sscanf( ch, "%s", stream_str );
			slen = (int)strlen( stream_str ) - 1;
			if  (stream_str[slen] == ',')  stream_str[slen] = '\0';

			/* identify stream */
			curr_idx = SeedIdentifyStreamString( stream_str );
			if  (curr_idx != stream_idx)  continue;

			/* find filename in line */
			p_file = line + 6;
			ch = p_file;
			while  (*(++ch) != ' ')
				if  (*ch == '/')
					p_file = ch;
			p_file++;
			sscanf( p_file, "%s", seedfile );

			/* find start time in line */
			ch = strstr( line, "start " );
			if  (ch == NULL)  continue;
			ch += 6;
			sscanf( ch, "%s", tstart );

			/* store current line */
			strcpy( prev_line, line );

			/* find next close line ($ SEED file closed) */
			while  (fgets(line,BC_LINELTH,log) != NULL)  {
				if  (strncmp(line,"$ SEED file closed",18) != 0)  {
					strcpy( prev_line, line );
					continue;
				} /*endif*/

				/* find end time */
				ch = strstr( line, "end " );
				if  (ch == NULL)  {
					printf( "*** illegal line:\n%s", line );
					return;
				} /*endif*/
				ch += 4;
				sscanf( ch, "%s", tend );
				slen = (int)strlen( tend ) - 1;
				if  (tend[slen] == ',')  tend[slen] = '\0';

				/* find number of records */
				ch = strchr( ch, ',' );
				if  (ch == NULL)  {
					printf( "*** illegal line:\n%s", line );
					return;
				} /*endif*/
				ch += 2;
				sscanf( ch, "%d", &rec_num );

				strcpy( prev_line, line );
				break;
			} /*endwhile*/

			fprintf( sfd, "%24s %24s %s %d\n", tstart, tend, seedfile, rec_num );

		} /*endwhile*/

	} /*endwhile*/

} /* end of ScanLogfile */



/*---------------------------------------------------------------------*/

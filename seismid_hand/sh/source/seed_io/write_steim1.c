
/* file write_steim1.c
 *      ==============
 *
 * version 8, 21-Nov-2005
 *
 * Writes data in Mini-SEED Format
 * K. Stammler, 1-Aug-94
 */



#include <stdio.h>
#include <string.h>
#include "../basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_TCUSRDEF
#include BC_ERUSRDEF
#include BC_CPAR
#include "seedcfg.h"
#include "seed_lib.h"



#define CHUNKSIZE 8000



/* global variables */
static SeedSetupParT seedv_setup;       /* SEED setup parameters */



/* prototypes of local routines */
static void ReadHeaderFile( char hdrfile[], SeedDataHeaderT *hdr,
	STATUS *status );
static void ReadDataChunk( FILE *fp, INT32 req_lth, INT32 dat[], INT32 *smp_read,
	BOOLEAN *eof );



int main( int argc, char *argv[] )
{
	/* local variables */
	STATUS   status;                 /* return status */
	char     hdrfile[BC_FILELTH+1];  /* header file */
	char     datfile[BC_FILELTH+1];  /* data file */
	char     outfile[BC_FILELTH+1];  /* name of output file */
	SeedDataHeaderT dathdr;          /* data header */
	FILE     *fp;                    /* pointer to input data file */
	FILE     *out;                   /* pointer to output data file */
	INT32    smp_read;               /* number of samples read from file */
	BOOLEAN  eof;                    /* end of file found */
	INT32    idx;                    /* sample index */
	INT32    dat[CHUNKSIZE*2];       /* sample buffer */
	SeedSbyteT wrk[CHUNKSIZE*4];     /* work space */
	int      wrklth;                 /* length of work space in records */
	INT32    prev_smp;               /* previous sample */
	NTIME    ctime;                  /* current time */
	int      skip;                   /* number of lines to skip */
	char     str[BC_LINELTH+1];      /* scratch */

	/* executable code */

	status = BC_NOERROR;

	pa_init( argc, argv );
	if  (pa_pnumber() != 3)  {
		printf( "Usage: %s <header> <data> <output> ***\n", argv[0] );
		return 1;
	} /*endif*/
	strcpy( hdrfile, pa_pvalue(1) );
	strcpy( datfile, pa_pvalue(2) );
	strcpy( outfile, pa_pvalue(3) );
	skip = 0;
	if  (pa_qspecified("-skip"))
		sscanf( pa_qvalue("-skip"), "%d", &skip );

	/* get parameters */
	SeedSetup( &seedv_setup, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	wrklth = CHUNKSIZE*7 / seedv_setup.seed_rec_size;

	/* read header file */
	ReadHeaderFile( hdrfile, &dathdr, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	/* get time from header */
	SeedBtimeToNtime( &(dathdr.starttime), &ctime, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	SeedSetHeaderPrototype( &dathdr );

	/* open input & output data files */
	fp = fopen( datfile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "write_steim1: data file %s not found\n", datfile );
		return 0;
	} /*endif*/
	out = fopen( outfile, "w" );
	if  (out == NULL)  {
		fprintf( stderr, "write_steim1: data file %s not opened\n", outfile );
		fclose( fp );
		return 0;
	} /*endif*/

	/* skip lines on data file */
	for  (idx=0; idx<skip; idx++)
		fgets( str, BC_LINELTH, fp );

	idx = 0;
	prev_smp = 0;
	do  {
		if  (idx > CHUNKSIZE)  {fprintf(stderr,"increase CHUNKSIZE\n"); exit(1);}
		ReadDataChunk( fp, CHUNKSIZE, dat+idx, &smp_read, &eof );
		idx += smp_read;
		SeedEncodeSteim1( fileno(out), dat, &prev_smp, &idx, &ctime, wrk, wrklth,
			FALSE, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
	}  while (!eof);

	/* flush remaining data */
	if  (idx > 0)
		SeedEncodeSteim1( fileno(out), dat, &prev_smp, &idx, &ctime, wrk, wrklth,
			TRUE, &status );

	fclose( fp );
	fclose( out );

	return 0;

} /* end of main */




/*----------------------------------------------------------------------------*/



#define KEYNUM_SEQNO 0
#define KEYNUM_INDICATOR 1
#define KEYNUM_STATION 2
#define KEYNUM_LOCATION 3
#define KEYNUM_CHANNEL 4
#define KEYNUM_STARTTIME 5
#define KEYNUM_SMPFACT 6
#define KEYNUM_SMPMULT 7
#define KEYNUM_ACTIVITY 8
#define KEYNUM_IOFLAGS 9
#define KEYNUM_QUALITY 10
#define KEYNUM_TIMECORR 11
#define KEYWORDNUM 12



static void ReadHeaderFile( char hdrfile[], SeedDataHeaderT *hdr,
	STATUS *status )

/* Reads in data header prototype
 *
 * parameters of routine
 * char       hdrfile[];           input; name of header file
 * SeedDataHeaderT *hdr;           output; header prototype
 * STATUS     *status;             output; return status
 */
{
	/* local variables */
	static char *keyword[] = {
		"sequence number:",
		"indicator:",
		"station:",
		"location ID:",
		"channel:",
		"start time:",
		"sample rate factor:",
		"sample rate multiplier:",
		"activity flags:",
		"IO flags:",
		"quality flags:",
		"time correction:"
	};
	FILE     *hf;                 /* pointer to header file */
	char     line[BC_LINELTH+1];  /* current line in header file */
	int      keynum;              /* keyword number */
	int      slen;                /* string length */
	BOOLEAN  key_found;           /* keyword found */
	int      tmp;                 /* scratch */
	char     str[BC_LINELTH+1];   /* scratch */
	NTIME    ntime;               /* numeric time */

	/* executable code */

	tc_t2n( "1-jun-1970_0:0", &ntime, status );
	if  (Severe(status))  return;

	strcpy( hdr->seqno, "000001" );
	hdr->indicator = 'D';
	hdr->Reserved_bytes_A = '\0';
	strcpy( hdr->statcode, "XXXX" );
	hdr->locid[0] = hdr->locid[1] = ' ';
	hdr->channel[0] = hdr->channel[1] = hdr->channel[2] = 'X';
	hdr->network[0] = hdr->network[1] = '\0';
	SeedNtimeToBtime( &ntime, &(hdr->starttime), status );
	if  (Severe(status))  return;
	hdr->no_of_samples = 0;
	hdr->smprate_fact = 0;
	hdr->smprate_mult = 0;
	hdr->activity = 0;
	hdr->ioflags = 0;
	hdr->quality = 0;
	hdr->no_of_blockettes = 0;
	hdr->timecorr = 0;
	hdr->databegin = sizeof( SeedDataHeaderT );
	hdr->first = 0;

	/* open header file */
	hf = sy_fopen( hdrfile, "r" );
	if  (hf == NULL)  {
		fprintf( stderr, "write_steim1: header file %s not found\n", hdrfile );
		*status = 1;
		return;
	} /*endif*/

	while  (fgets(line,BC_LINELTH,hf) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		key_found = FALSE;
		for  (keynum=0; keynum<KEYWORDNUM; keynum++)  {
			slen = strlen( keyword[keynum] );
			if  (strncmp(keyword[keynum],line,slen) == 0)  {
				key_found = TRUE;
				break;
			} /*endif*/
		} /*endfor*/
		if  (!key_found)  {
			fprintf( stderr, "write_steim1: illegal line in header file:\n%s",
				line );
			continue;
		} /*endif*/
		switch  (keynum)  {
		case KEYNUM_SEQNO:
			sscanf( line+slen, "%d", &tmp );
			if  (tmp > 999999)  tmp = 0;
			*str = hdr->indicator;
			sprintf( hdr->seqno, "%06d", tmp );
			hdr->indicator = *str;
			break;
		case KEYNUM_INDICATOR:
			sscanf( line+slen, "%c", &(hdr->indicator) );
			break;
		case KEYNUM_STATION:
			tmp = hdr->locid[0];
			sscanf( line+slen, "%s", str );
			if  (strlen(str) > 5)  {
				fprintf( stderr, "write_steim1: name %s truncated ", str );
				str[5] = '\0';
				fprintf( stderr, "to %s\n", str );
			} /*endif*/
			strcpy( hdr->statcode, str );
			hdr->locid[0] = tmp;
			break;
		case KEYNUM_LOCATION:
			sscanf( line+slen, "%s", str );
			if  (strlen(str) > 2)  {
				fprintf( stderr, "write_steim1: illegal loc length %s\n", str );
				*status = 1;
				return;
			} /*endif*/
			hdr->locid[0] = str[0]; hdr->locid[1] = str[1];
			break;
		case KEYNUM_CHANNEL:
			sscanf( line+slen, "%s", str );
			if  (strlen(str) > 3)  {
				fprintf( stderr, "write_steim1: illegal channel length %s\n", str );
				*status = 1;
				return;
			} /*endif*/
			hdr->channel[0] = str[0];
			hdr->channel[1] = str[1];
			hdr->channel[2] = str[2];
			break;
		case KEYNUM_STARTTIME:
			sscanf( line+slen, "%s", str );
			tc_t2n( str, &ntime, status );
			if  (Severe(status))  return;
			SeedNtimeToBtime( &ntime, &(hdr->starttime), status );
			if  (Severe(status))  return;
			break;
		case KEYNUM_SMPFACT:
			sscanf( line+slen, "%d", &tmp );
			hdr->smprate_fact = tmp;
			break;
		case KEYNUM_SMPMULT:
			sscanf( line+slen, "%d", &tmp );
			hdr->smprate_mult = tmp;
			break;
		case KEYNUM_ACTIVITY:
			sscanf( line+slen, "%d", &tmp );
			hdr->activity = (UBYTE)tmp;
			break;
		case KEYNUM_IOFLAGS:
			sscanf( line+slen, "%d", &tmp );
			hdr->ioflags = (UBYTE)tmp;
			break;
		case KEYNUM_QUALITY:
			sscanf( line+slen, "%d", &tmp );
			hdr->quality = (UBYTE)tmp;
			break;
		case KEYNUM_TIMECORR:
			sscanf( line+slen, "%d", &tmp );
			hdr->timecorr = tmp;
			break;
		default:
			fprintf( stderr, "write_steim1: program bug (1)\n" );
			return;
		} /*endswitch*/
	} /*endwhile*/

	sy_fclose( hf );

} /* end of ReadHeaderFile */



/*----------------------------------------------------------------------------*/



static void ReadDataChunk( FILE *fp, INT32 req_lth, INT32 dat[], INT32 *smp_read,
	BOOLEAN *eof )

/* reads chunk of data.
 *
 * parameters of routine
 * FILE       *fp;              input; pointer to input file
 * INT32      req_lth;          input; number of samples requested
 * INT32      dat[];            output; samples read
 * INT32      *smp_read;        output; number of samples read
 * BOOLEAN    *eof;             output; EOF found
 */
{
	/* local variables */
	int      errcnt;            /* error counter */

	/* executable code */

	errcnt = 0;
	*smp_read = 0;

	while  (*smp_read < req_lth)  {
		*eof = feof( fp );
		if  (*eof)  return;
		if  (fscanf( fp, "%ld", dat+(*smp_read) ) == 1)  {
			(*smp_read)++;
		} else {
			*eof = feof( fp );
			if  (*eof)  return;
			fprintf( stderr, "write_steim1: error reading ASCII data\n" );
			if  (++errcnt > 10)  return;
		} /*endif*/
	} /*endfor*/

} /* end of ReadDataChunk */



/*----------------------------------------------------------------------------*/

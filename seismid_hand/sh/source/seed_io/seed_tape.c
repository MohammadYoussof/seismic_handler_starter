
/* file seed_tape.c
 *      ===========
 *
 * version 12, 31-May-96
 *
 * Extracts SEED data from tape.
 * command is: 'seed_tape <tape-device> <selections>'
 *    <tape-device>        name of tape device, like /dev/rst0
 *    <selections>         name of selection file for streams and times
 * possible qualifiers are
 *    -setup=<setup-file>  name of setup file
 *    -physrec=<recsize>   size of physical record length in bytes
 *    -seedrec=<recsize>   size of SEED records in bytes (multiple of above)
 *    -newnames            use new names (with underscores "_")
 *    -oldnames            do not use new names
 *    -dump                dump unknown records to file
 *    -nodump              ignore unknown records
 *    -eom=<eof-cnt>       number of consecutive EOF's needed for EOM
 *    -err_ill=<max>       maximum number of illegal records tolerated
 *    -err_incomp=<max>    maximum number of incomplete records tolerated
 *    -err_read=<max>      maximum number of read errors tolerated
 *    -norew               no rewind at start
 *    -skip=<n>            skip n files at beginning
 *    -getlabel            read label only
 * K. Stammler, 26-Dec-93
 *
 * Replaces all station names 'IBB' by 'IBBN'
 */



#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/uio.h>
#include BASECNST
#include BC_SYSBASE
#include BC_CPAR
#include BC_TCUSRDEF
#include BC_UTUSRDEF
#include "erusrdef.h"
#include "seedcfg.h"
#include "tape_basics.h"
#include "unix_io.h"


#ifdef cBc_OS_AIX

void main( void )
{
}

#else


/*
 * constants
 * ---------
 */

/* tape read status codes */
#define Tape_C_OK 0
#define Tape_C_INCOMPLETE 1
#define Tape_C_EOF 2
#define Tape_C_EOM 3
#define Tape_C_READERR 4

/* record identification status codes */
#define Tape_C_ID_OK 0
#define Tape_C_ID_IGNORE 1
#define Tape_C_ID_FAILURE 2
#define Tape_C_ID_TRAILER 3

/* default time span for data files */
#define Tape_C_SEL_TIMESPAN 87000.0

/* execution flags */
#define Tape_F_REWIND     0x01
#define Tape_F_SKIPSTREAM 0x02
#define Tape_F_SKIPTIME   0x04
#define Tape_F_LABELONLY  0x08


/*
 * types
 * -----
 */

typedef struct {
	SeedStreamT  stream;               /* stream info */
	int          id_status;            /* identification status */
	char         start[BC_TIMELTH+1];  /* start time */
	char         end[BC_TIMELTH+1];    /* end time */
	char         msg[BC_LINELTH+1];    /* message string */
	char         fname[BC_FILELTH+1];  /* filename suggestion */
	float        dt;                   /* sample distance in sec */
	SeedDataHeaderT *dathdr;           /* data header */
} TapeRecDescrT;



/*
 * global variables
 * ----------------
 */

static SeedSetupParT seedv_setup;    /* setup parameters */



/*
 * prototypes of local routines
 * ----------------------------
 */

static void do_abort( STATUS status );
void TapeExtract( char tapedev[], int flags, int file_skip,
	char outdir[], STATUS *status );
void TapeReadLogicalRecord( int tape, char buf[], int *read_lth,
	int *tape_status );
void TapeIdentifyRecord( char *rec, TapeRecDescrT *descr );
int TapeRecordIsSelected( TapeRecDescrT *descr );
BOOLEAN TapeFileIsSelected( TapeRecDescrT *descr );
void TapeWriteToSeedFile( TapeRecDescrT *descr, char outdir[], STATUS *status );
void TapeCheckRecord( TapeRecDescrT *descr );
void TapeDumpRecord( TapeRecDescrT *descr, char outdir[] );
void TapeFindLabel( TapeRecDescrT *descr, int maxlth, char label[] );
void TapePrintTrailer( TapeRecDescrT *descr, char outdir[] );



int main( int argc, char *argv[] )
{
	/* local variables */
	STATUS   status;                   /* return status */
	char     tape[BC_FILELTH+1];       /* tape device */
	char     setupfile[BC_FILELTH+1];  /* setup file */
	char     selectfile[BC_FILELTH+1]; /* selection file */
	char     outdir[BC_FILELTH+1];     /* output directory */
	SeedSetupParT setup_change;        /* setup change values */
	long     setup_change_flags;       /* setup change flags */
	int      flags;                    /* execution flags */
	int      file_skip;                /* number of file to be skipped */
	char     *def_dir;                 /* pointer to default directory */

	/* executable code */

	/* get parameters */
	setup_change_flags = 0;
	pa_init( argc, argv );
	if  (pa_pnumber() != 2)  {
		fprintf( stderr, "\n\
      Usage: %s <tape-device> <selections>                                  \n\
      <tape-device>        name of tape device, like /dev/rst0              \n\
      <selections>         name of selection file for streams and times     \n\
      possible qualifiers are                                               \n\
      -setup=<setup-file>  name of setup file                               \n\
      -physrec=<recsize>   size of physical record length in bytes          \n\
      -seedrec=<recsize>   size of SEED records in bytes (multiple of above)\n\
      -newnames            use new names (with underscores '_')             \n\
      -oldnames            do not use new names                             \n\
      -auto_out            output directory found in GRSN_... variables     \n\
      -dump                dump unknown records to file                     \n\
      -nodump              ignore unknown records                           \n\
      -eom=<eof-cnt>       number of consecutive EOF's needed for EOM       \n\
      -err_ill=<max>       maximum number of illegal records tolerated      \n\
      -err_incomp=<max>    maximum number of incomplete records tolerated   \n\
      -err_read=<max>      maximum number of read errors tolerated          \n\
      -norew               no rewind of tape at start                       \n\
      -noskipstream        do not skip file if stream is not selected       \n\
      -skiptime            skip file if first time found is not selected    \n\
      -skip=<n>            skip <n> files at beginning                      \n\
      -getlabel            read label only                                  \n\
          \n", argv[0] );
		return 1;
	} /*endif*/
	flags = Tape_F_REWIND | Tape_F_SKIPSTREAM;
	strcpy( tape, pa_pvalue(1) );
	strcpy( selectfile, pa_pvalue(2) );
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
	if  (pa_qspecified("-physrec"))  {
		sscanf( pa_qvalue("-physrec"), "%d", &setup_change.phys_rec_size );
		setup_change_flags |= Seed_F_SET_PHYS_REC_SIZE;
	} /*endif*/
	if  (pa_qspecified("-seedrec"))  {
		sscanf( pa_qvalue("-seedrec"), "%d", &setup_change.seed_rec_size );
		setup_change_flags |= Seed_F_SET_SEED_REC_SIZE;
	} /*endif*/
	if  (pa_qspecified("-newnames"))  {
		setup_change.new_seed_names = TRUE;
		setup_change_flags |= Seed_F_SET_NEW_SEED_NAMES;
	} /*endif*/
	if  (pa_qspecified("-oldnames"))  {
		setup_change.new_seed_names = FALSE;
		setup_change_flags |= Seed_F_SET_NEW_SEED_NAMES;
	} /*endif*/
	if  (pa_qspecified("-dump"))  {
		setup_change.dump_ill_recs = TRUE;
		setup_change_flags |= Seed_F_SET_DUMP_ILL_RECS;
	} /*endif*/
	if  (pa_qspecified("-nodump"))  {
		setup_change.dump_ill_recs = FALSE;
		setup_change_flags |= Seed_F_SET_DUMP_ILL_RECS;
	} /*endif*/
	if  (pa_qspecified("-eom"))  {
		sscanf( pa_qvalue("-eom"), "%d", &setup_change.eom_eof_cnt );
		setup_change_flags |= Seed_F_SET_EOM_EOF_CNT;
	} /*endif*/
	if  (pa_qspecified("-err_ill"))  {
		sscanf( pa_qvalue("-err_ill"), "%d", &setup_change.max_errcnt_illegal );
		setup_change_flags |= Seed_F_SET_MAX_ERRCNT_ILLEGAL;
	} /*endif*/
	if  (pa_qspecified("-err_incomp"))  {
		sscanf( pa_qvalue("-err_incomp"), "%d",
		&setup_change.max_errcnt_incomplete );
		setup_change_flags |= Seed_F_SET_MAX_ERRCNT_INCOMPLETE;
	} /*endif*/
	if  (pa_qspecified("-err_read"))  {
		sscanf( pa_qvalue("-err_read"), "%d", &setup_change.max_errcnt_readerr );
		setup_change_flags |= Seed_F_SET_MAX_ERRCNT_READERR;
	} /*endif*/
	if  (pa_qspecified( "-norew" ))         flags &= ~Tape_F_REWIND;
	if  (pa_qspecified( "-noskipstream" ))  flags &= ~Tape_F_SKIPSTREAM;
	if  (pa_qspecified( "-skiptime" ))      flags |=  Tape_F_SKIPTIME;
	if  (pa_qspecified( "-getlabel" ))      flags |=  Tape_F_LABELONLY;
	file_skip = 0;
	if  (pa_qspecified("-skip"))
		sscanf( pa_qvalue("-skip"), "%d", &file_skip );
	strcpy( outdir, "./" );
	if  (pa_qspecified("-auto_out"))  strcpy( outdir, "automatic" );

	/* do setup */
	status = BC_NOERROR;
	SeedReadSetupFile( setupfile, &status );
	if  (Severe(&status))  err_writemsg( status, setupfile, TRUE );
	if  (setup_change_flags != 0)
		SeedChangeSetup( &setup_change, setup_change_flags, &status );
	if  (Severe(&status))  do_abort( status );
	SeedInitialize( &status );
	if  (Severe(&status))  do_abort( status );
	SeedGetSetup( &seedv_setup );
	SeedReadSelections( selectfile, &status );
	if  (Severe(&status))  err_writemsg( status, selectfile, TRUE );
	SeedPrintSelections();

	/* now access tape */
	TapeExtract( tape, flags, file_skip, outdir, &status );
	if  (Severe(&status))  do_abort( status );

	/* terminate */
	SeedFinish();
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

#ifdef XXX
	fprintf( stderr, "*** Abort.   Status: %d\n", status );
	exit( 1 );
#endif
	err_writemsg( status, "", TRUE );

} /* end of do_abort */



/*---------------------------------------------------------------------*/



void TapeExtract( char tapedev[], int flags, int file_skip,
	char outdir[], STATUS *status )

/* Reads data from tape and creates SEED files on directory "outdir".
 * If "outdir" is set to "automatic" then the output directory is found
 * from the $GRSN_... environment variables.
 *
 * parameters of routine
 * char       tapedev[];        input; tape device name
 * int        flags;            input; execution flags
 * int        file_skip;        input; number of files to be skipped
 * char       outdir[];         input/output; name of output directory
 * STATUS     *status;          output; return status
 */
{
	/* local variables */
	TapeRecDescrT descr;        /* record descriptor */
	int      tape;              /* tape file descriptor */
	char     *logrec;           /* logical record */
	int      tape_status;       /* status of last read operation */
	int      read_lth;          /* number of bytes read from tape */
	BOOLEAN  end_of_media;      /* end of tape found */
	int      errcnt_incomplete; /* error counter for Tape_C_INCOMPLETE */
	int      errcnt_readerr;    /* error counter for read errors */
	int      errcnt_illegal;    /* error counter for illegal records */
	BOOLEAN  new_tape_file;     /* new file on tape found */
	int      cnt_tape_file;     /* tape file counter */
	BOOLEAN  trailer_found;     /* trailer file found */
	char     tape_label[BC_LINELTH+1];  /* tape label */
	int      selret;            /* return value of selection request */

	/* executable code */

	errcnt_incomplete = 0;
	errcnt_readerr = 0;
	errcnt_illegal = 0;
	cnt_tape_file = 0;
	trailer_found = FALSE;

	/* allocate memory for logical record */
	logrec = (char *)sy_allocmem( seedv_setup.seed_rec_size,
		(int)sizeof(char), status );
	if  (Severe(status))  return;

	/* open and position tape */
	tape = open( tapedev, O_RDONLY );
	if  (tape < 0)  {
		fprintf( stderr, "*** error opening tape device %s ***\n", tapedev );
		*status = 1;
		sy_deallocmem( logrec );
		return;
	} /*endif*/
	printf( "\n\n" );
	if  (Tape_F_REWIND & flags)  {
		printf( "! perform rewind on tape %s\n", tapedev );
		TapeBasicsRewind( tape );
	} else {
		printf( "! no rewind on tape %s\n", tapedev );
	} /*endif*/
	if  (file_skip == 0)  {
		printf( "! no files skipped on tape %s\n", tapedev );
	} else {
		printf( "! skip %d files on tape %s\n", file_skip, tapedev );
		TapeBasicsSkipFile( tape, file_skip );
	} /*endif*/

	TapeCheckRecord( NULL );
	end_of_media = FALSE;
	new_tape_file = TRUE;
	do  {

		TapeReadLogicalRecord( tape, logrec, &read_lth, &tape_status );
		switch  (tape_status)  {
		case Tape_C_OK:
			TapeIdentifyRecord( logrec, &descr );
			if  (new_tape_file)  {
				if  (descr.id_status == Tape_C_ID_OK)  {
					printf( "! new tape file [%d]: stream %s-%s-%c,  start %s\n",
						++cnt_tape_file, descr.stream.station, descr.stream.channel,
						descr.stream.comp, descr.start );
					if  (!TapeFileIsSelected(&descr) && (Tape_F_SKIPTIME & flags))  {
						printf( "! time not selected, skip file\n" );
						TapeCheckRecord( NULL );
						TapeBasicsSkipFile( tape, 1 );
						new_tape_file = TRUE;
						break;
					} else {
						new_tape_file = FALSE;
					} /*endif*/
				} else if  (descr.id_status == Tape_C_ID_IGNORE)  {
					TapeFindLabel( &descr, BC_LINELTH, tape_label );
					printf( "! new tape file [%d]: %s, used for label\n",
						++cnt_tape_file, descr.msg );
					new_tape_file = FALSE;
					if  (Tape_F_LABELONLY & flags)  {
						close( tape ); sy_deallocmem( logrec );
						return;
					} /*endif*/
				} else {
					printf( "! new tape file [%d]: %s\n",
						++cnt_tape_file, descr.msg );
					new_tape_file = FALSE;
				} /*endif*/
			} /*endif*/
			if  (descr.id_status == Tape_C_ID_TRAILER || trailer_found)  {
				if  (!trailer_found)  {
					trailer_found = TRUE;
					printf( "! trailer found  --  End of data\n\n\n" );
				} /*endif*/
				TapePrintTrailer( &descr, outdir );
			} else if  (descr.id_status == Tape_C_ID_IGNORE)  {
				printf( "! %s; record ignored\n", descr.msg );
				break;
			} else if  (descr.id_status == Tape_C_ID_OK)  {
				TapeCheckRecord( &descr );
				selret = TapeRecordIsSelected( &descr );
				if  (selret == Seed_C_IsSelected)  {
					TapeWriteToSeedFile( &descr, outdir, status );
					if  (Severe(status))  {
						*status = 1; close( tape ); sy_deallocmem( logrec );
						return;
					} /*endif*/
				} else if  (selret == Seed_C_TimeNotSelected)  {
					/* close SEED file */
					TapeWriteToSeedFile( NULL, "", status );
				} else if  (selret == Seed_C_StreamNotSelected)  {
					if  (Tape_F_SKIPSTREAM & flags)  {
						printf( "! stream not selected, skip file\n" );
						TapeCheckRecord( NULL );
						TapeBasicsSkipFile( tape, 1 );
						new_tape_file = TRUE;
					} /*endif*/
				} else {
					fprintf( stderr, "%s: program bug (TapeRecordIsSelected)\n",
						pa_progname() );
					return;
				} /*endif*/
			} else {
				printf( "*** illegal record [%d]: %s\n",
					++errcnt_illegal, descr.msg );
				/* close SEED file */
				TapeWriteToSeedFile( NULL, "", status );
				if  (seedv_setup.dump_ill_recs)
					TapeDumpRecord( &descr, outdir );
				if  (errcnt_illegal > seedv_setup.max_errcnt_illegal
					|| Severe(status))  {
					if  (errcnt_illegal > seedv_setup.max_errcnt_illegal)
						printf( "*** too many illegal records --> abort\n" );
					*status = 1; close( tape ); sy_deallocmem( logrec );
					return;
				} /*endif*/
			} /*endif*/
			break;
		case Tape_C_INCOMPLETE:
			TapeIdentifyRecord( logrec, &descr );
			printf( "*** incomplete logical record found [%d] ***\n",
				++errcnt_incomplete );
			printf( "*** only %d bytes read\n", read_lth );
			printf( "*** record: %s\n", descr.msg );
			if  (errcnt_incomplete > seedv_setup.max_errcnt_incomplete)  {
				printf( "*** too many Tape_C_INCOMPLETE errors --> abort\n" );
				*status = 1; close( tape ); sy_deallocmem( logrec );
				return;
			} /*endif*/
			break;
		case Tape_C_EOF:
			/* print out end time if there is any */
			if  (descr.end[0] == '\0')  {
				printf( "! end of file\n" );
			} else {
				printf( "! end of file at %s\n", descr.end );
			} /*endif*/
			/* close SEED file */
			TapeWriteToSeedFile( NULL, "", status );
			TapeCheckRecord( NULL );
			new_tape_file = TRUE;
			break;
		case Tape_C_EOM:
			end_of_media = TRUE;
			printf( "\n\n! end of tape found\n" );
			break;
		case Tape_C_READERR:
			printf( "*** read error [%d] ***\n", ++errcnt_readerr );
			if  (errcnt_readerr > seedv_setup.max_errcnt_readerr)  {
				printf( "*** too many Tape_C_READERR errors --> abort\n" );
				*status = 1;
				close( tape );
				sy_deallocmem( logrec );
				return;
			} /*endif*/
			break;
		default:
			printf( "*** TapeExtract: this is a bug in the program (1)\n" );
			*status = 1;
			close( tape );
			sy_deallocmem( logrec );
			return;
		} /*endswitch*/

	}  while  (!end_of_media);

	close( tape );
	sy_deallocmem( logrec );

} /* end of TapeExtract */



/*---------------------------------------------------------------------*/



void TapeReadLogicalRecord( int tape, char buf[], int *read_lth,
	int *tape_status )

/* Reads a logical record from tape.  tape_status may have the following
 * values:
 *    Tape_C_OK:         read operation successful, data in buf
 *    Tape_C_INCOMPLETE: more than 0 but less then requested bytes read
 *                       "*read_lth" data stored in buf
 *    Tape_C_EOF:        end of file found on tape
 *    Tape_C_EOM:        end of media found on tape
 *
 * parameters of routine
 * int        tape;            input; tape file descriptor
 * char       buf[];           output; data read
 * int        *read_lth;       output; number of bytes read
 * int        *tape_status;    output; tape status (see above)
 */
{
	/* local variables */
	int      i;         /* counter */
	char     *mem;      /* memory pointer */
	int      read_ret;  /* return value of read */
	static int eof_count=0;   /* consecutive EOF counter */

	/* executable code */

	*read_lth = 0;
	for  (i=0, mem=buf; i<seedv_setup.seed_phys_recs;
		i++, mem += seedv_setup.phys_rec_size)  {
		read_ret = read( tape, mem, seedv_setup.phys_rec_size );
		if  (read_ret == 0)  {
			*tape_status = (++eof_count >= seedv_setup.eom_eof_cnt)
				? Tape_C_EOM : Tape_C_EOF;
			return;
		} else if  (read_ret == seedv_setup.phys_rec_size)  {
			*tape_status = Tape_C_OK;
			*read_lth += read_ret;
			eof_count = 0;
		} else if  (read_ret < 0)  {
			*tape_status = Tape_C_READERR;
			eof_count = 0;
		} else {
			*tape_status = Tape_C_INCOMPLETE;
			*read_lth += read_ret;
			eof_count = 0;
			return;
		} /*endif*/
	} /*endfor*/

} /* end of TapeReadLogicalRecord */



/*---------------------------------------------------------------------*/



void TapeIdentifyRecord( char *rec, TapeRecDescrT *descr )

/* Identifies logical record "rec".  Result is returned in "descr".
 * If station 'IBB' is found in the record it is replaced by 'IBBN'
 *
 * parameters of routine
 * char       *rec;         input/modify; logical record
 * TapeRecDescrT *descr;    output; record descriptor
 */
{
	/* local variables */
	SeedDataHeaderT *hdr;                  /* SEED header */
	int      i;                            /* counter */
	char     str[BC_LINELTH+1];            /* scratch string */
	NTIME    ntime;                        /* numeric time */
	STATUS   locstat;                      /* local status */
	char     tmp;                          /* scratch */

	/* executable code */

	locstat = BC_NOERROR;
	descr->stream.station[0] = '\0';
	descr->stream.channel[0] = '\0';
	descr->stream.comp = '\0';
	descr->fname[0] = '\0';
	descr->start[0] = '\0';
	descr->end[0] = '\0';
	descr->dt = 0.0;
	descr->dathdr = (SeedDataHeaderT *)rec;
	descr->id_status = Tape_C_ID_OK;

	if  (strncmp(rec,"DRM_TAPE",8) == 0)  {
		descr->id_status = Tape_C_ID_IGNORE;
		strcpy( descr->msg, "DRM data tape header" );
		return;
	} else if  (strncmp(rec,"TABLE OF CONTENTS",17) == 0)  {
		descr->id_status = Tape_C_ID_TRAILER;
		strcpy( descr->msg, "DRM trailer record" );
		return;
	} /*endif*/

	if  (*rec == '\0')  {
		descr->id_status = Tape_C_ID_FAILURE;
		strcpy( descr->msg, "unknown record, first byte is zero" );
		return;
	} /*endif*/

	for  (i=0; i<6; i++)
		if  (rec[i] < '0' || rec[i] > '9')  {
			descr->id_status = Tape_C_ID_FAILURE;
			strcpy( descr->msg,
				"unknown record, non-digit bytes in first 6 chars" );
			return;
		} /*endif*/

	hdr = (SeedDataHeaderT *)rec;
	if  (hdr->indicator != 'D')  {
		descr->id_status = Tape_C_ID_FAILURE;
		strncpy( str, rec, 7 );
		str[7] = '\0';
		sprintf( descr->msg, "no SEED data record, first 7 chars are >%s<", str );
		return;
	} /*endif*/

	ntime.year = hdr->starttime.year;
	ntime.hour = hdr->starttime.hours;
	ntime.min = hdr->starttime.minutes;
	ntime.sec = hdr->starttime.seconds;
	ntime.ms = hdr->starttime.frac_secs / 10;
	tc_dayofmn( ntime.year, hdr->starttime.day, &ntime.month,
		&ntime.day, &locstat );
	if  (locstat == BC_NOERROR)
		tc_n2t( &ntime, descr->start, &locstat );
	if  (Severe(&locstat))  {
		descr->id_status = Tape_C_ID_FAILURE;
		sprintf( descr->msg, "couldn't convert begin time %d,%d,%d,%d,%ld",
			(int)(hdr->starttime.year), (int)(hdr->starttime.hours),
			(int)(hdr->starttime.minutes), (int)(hdr->starttime.seconds),
			(long)(hdr->starttime.frac_secs) );
		return;
	} /*endif*/

	/* get sample rate */
	if  (hdr->smprate_fact > 0 && hdr->smprate_mult > 0)  {
		descr->dt = 1.0/((float)(hdr->smprate_fact) * (float)(hdr->smprate_mult));
	} else if  (hdr->smprate_fact > 0 && hdr->smprate_mult < 0)  {
		descr->dt = -1.0/((float)(hdr->smprate_fact)/(float)(hdr->smprate_mult));
	} else if  (hdr->smprate_fact < 0 && hdr->smprate_mult > 0)  {
		descr->dt = -1.0/((float)(hdr->smprate_mult)/(float)(hdr->smprate_fact));
	} else if  (hdr->smprate_fact < 0 && hdr->smprate_mult < 0)  {
		descr->dt = 1.0/((float)(hdr->smprate_fact)/(float)(hdr->smprate_mult));
	} else {
		printf( "*** illegal sample rate, set dt=1.0\n" );
		descr->dt = 1.0;
	} /*endif*/

	tc_tadd( descr->start, (descr->dt)*(float)(hdr->no_of_samples),
		descr->end, &locstat);
	if  (Severe(&locstat))  {
		descr->id_status = Tape_C_ID_FAILURE;
		sprintf( descr->msg, "couldn't convert end time. start %s, samples %ld",
			descr->start, (long)(hdr->no_of_samples) );
		return;
	} /*endif*/

	/* get station code */
	/* first check for IBB */
	if  (strncmp(hdr->statcode,"IBB",3) == 0)  {
		hdr->statcode[3] = 'N';
		hdr->statcode[4] = '\0';
	} /*endif*/
	strncpy( descr->stream.station, hdr->statcode, 5 );
	descr->stream.station[5] = '\0';
	i = 5;
	while  (descr->stream.station[--i] == ' ')
		if  (i >= 0)
			descr->stream.station[i] = '\0';
	ut_uncap( descr->stream.station );

	tmp = Cap( hdr->channel[0] );
	if  (tmp == 'B')  {
		strcpy( descr->stream.channel, "vbb" );
	} else if  (tmp == 'H')  {
		strcpy( descr->stream.channel, "vsp" );
	} else if  (tmp == 'L')  {
		strcpy( descr->stream.channel, "lp" );
	} else if  (tmp == 'V')  {
		strcpy( descr->stream.channel, "vlp" );
	} else {
		descr->id_status = Tape_C_ID_FAILURE;
		sprintf( descr->msg, "unknown SEED channel %c%c%c", hdr->channel[0],
			hdr->channel[1], hdr->channel[2] );
		return;
	} /*endif*/

	descr->stream.comp = Uncap( hdr->channel[2] );

	/* make filename */
	if  (ntime.year > 1900)  ntime.year -= 1900;
	if  (ntime.year > 100)  ntime.year -= 100;
	if  (seedv_setup.new_seed_names)  {
		sprintf( descr->fname, "%s_%02d%02d%02d_%02d%02d.", descr->stream.station,
			ntime.year, ntime.month, ntime.day, ntime.hour, ntime.min );
	} else {
		sprintf( descr->fname, "%s%02d%02d%02d%02d%02d%02d.",
			descr->stream.station, ntime.year, ntime.month, ntime.day,
			ntime.hour, ntime.min, ntime.sec );
	} /*endif*/
	strncat( descr->fname, hdr->channel, 3 );
	i = strlen( descr->fname );
	while  (descr->fname[--i] == ' ')
			if  (i >= 0)
				descr->fname[i] = '\0';
	ut_uncap( descr->fname );

	/* put ID message */
	sprintf( descr->msg, "stream %s-%s-%c,  start %s,  end %s",
		descr->stream.station, descr->stream.channel, descr->stream.comp,
		descr->start, descr->end );

} /* end of TapeIdentifyRecord */



/*---------------------------------------------------------------------*/



int TapeRecordIsSelected( TapeRecDescrT *descr )

/* Checks whether or not specified record is selected.  Returns
 * Seed_C_IsSelected, Seed_C_TimeNotSelected or Seed_C_StreamNotSelected
 *
 * parameters of routine
 * TapeRecDescrT      *descr;   input; logical record descriptor
 */
{
	/* local variables */
	STATUS   locstat;           /* local status */
	int      selret;            /* return value */

	/* executable code */

	locstat = BC_NOERROR;
	selret = SeedIsSelected( &(descr->stream), descr->start, &locstat );
	if  (selret != Seed_C_TimeNotSelected)  return selret;
	return SeedIsSelected(&(descr->stream),descr->end,&locstat);

} /* end of TapeRecordIsSelected */



/*---------------------------------------------------------------------*/



BOOLEAN TapeFileIsSelected( TapeRecDescrT *descr )

/* Checks whether or not specified file (first record given) is selected.
 *
 * parameters of routine
 * TapeRecDescrT      *descr;   input; logical record descriptor of first record
 */
{
	/* local variables */
	STATUS   locstat;                /* local status */
	char     endtime[cBcTimeLth+1];  /* proposed end time of file */

	/* executable code */

	locstat = BC_NOERROR;
	tc_tadd( descr->start, Tape_C_SEL_TIMESPAN, endtime, &locstat );
	if  (SySevere(&locstat))  return FALSE;

	return SeedWindowIsSelected( &(descr->stream), descr->start,
		endtime, &locstat );

} /* end of TapeFileIsSelected */



/*---------------------------------------------------------------------*/



void TapeWriteToSeedFile( TapeRecDescrT *descr, char outdir[], STATUS *status )

/* Writes SEED record to file.  If descr==NULL, file is closed.  If "outdir"
 * is set to "automatic" than the directory is found from the GRSN_...
 * environment variables.
 *
 * parameters of routine
 * TapeRecDescrT *descr;       input; record descriptor
 * char          outdir[];     input; output directory
 * STATUS        *status;      output; return status
 */
{
	/* local variables */
	static TapeRecDescrT ldescr;    /* local descriptor */
	static int   seed_fd={-1};      /* SEED file descriptor */
	static int   cnt_totalrecs=0;   /* total record counter */
	static int   cnt_filerecs=0;    /* records in current file */
	char   str[BC_SHORTSTRLTH+1];   /* scratch */
	int    write_ret;               /* write return value */
	char   seedfile[BC_FILELTH+1];  /* SEED filename */
	char   loutdir[BC_FILELTH+1];   /* local version of output directory */
	char   *env;                    /* environment pointer */

	/* executable code */

	/* close file if NULL descriptor */
	if  (descr == NULL)  {
		if  (seed_fd < 0)  return;  /* already closed */
		close( seed_fd );
		printf( "$ SEED file closed, end %s, %d records\n",
			ldescr.end, cnt_filerecs );
		seed_fd = -1;
		cnt_filerecs = 0;
		return;
	} /*endif*/

	/* store descriptor */
	ldescr = *descr;

	/* update record number */
	cnt_totalrecs++;
	sprintf( str, "%06d", ++cnt_filerecs );
	strncpy( (char *)descr->dathdr, str, 6 );

	/* open file if closed */
	if  (seed_fd < 0)  {
		if  (descr->fname[0] == '\0')  {
			printf( "*** couldn't generate output filename\n" );
			printf( "*** record: %s\n", descr->msg );
			return;
		} /*endif*/
		if  (strcmp(outdir,"automatic") == 0)  {
			strcpy( loutdir, "GRSN_" );
			strcat( loutdir, descr->stream.station );
			ut_cap( loutdir );
			env = getenv( loutdir );
			*loutdir = '\0';
			if  (env != NULL)  strcpy( loutdir, env );
			strcat( loutdir ,"/raw/" );
		} else {
			strcpy( loutdir, outdir );
		} /*endif*/
		strcpy( seedfile, loutdir );
		strcat( seedfile, descr->fname );
		seed_fd = open( seedfile, O_CREAT|O_WRONLY|O_TRUNC, 0x1ff );
		if  (seed_fd < 0)  {
			printf( "*** couldn't open output file %s\n", seedfile );
			printf( "*** record: %s\n", descr->msg );
			*status = 1;
			return;
		} /*endif*/
		printf( "$ file %s opened, start %s\n", seedfile, descr->start );
	} /*endif*/

	/* write to file */
	write_ret = write( seed_fd, (char *)descr->dathdr,
		seedv_setup.seed_rec_size );
	if  (write_ret != seedv_setup.seed_rec_size)  {
		printf( "*** couldn't write data, write_ret: %d\n", write_ret );
		printf( "*** record %s\n", descr->msg );
		*status = 1;
		return;
	} /*endif*/

} /* end of TapeWriteToSeedFile */



/*---------------------------------------------------------------------*/



void TapeCheckRecord( TapeRecDescrT *descr )

/* Checks record.  Messages are printed to stdout and are preceded by
 * '&' characters.  A NULL descr pointer resets internal variables.
 *
 * parameters of routine
 * TapeRecDescrT *descr;         input; record descriptor
 */
{
	/* local variables */
	static char last_endtime[BC_TIMELTH+1];   /* end time of last record */
	float    tdiff;                           /* difference time */
	STATUS   locstat;                         /* local status */

	/* executable code */

	if  (descr == NULL)  {
		*last_endtime = '\0';
		return;
	} /*endif*/

	/* check number of samples */
	if  (seedv_setup.seed_rec_size == 4096)
		if  (descr->dathdr->no_of_samples > 3772)
			printf( "& stream %s-%s-%c, rec-start %s, %d samples\n",
			descr->stream.station, descr->stream.channel, descr->stream.comp,
			descr->start, (int)(descr->dathdr->no_of_samples) );

	/* check for time gaps */
	if  (*last_endtime == '\0')  {
		strcpy( last_endtime, descr->end );
		return;
	} /*endif*/
	locstat = BC_NOERROR;
	tdiff = tc_tdiff( descr->start, last_endtime, &locstat );
	if  (Severe(&locstat))  {
		printf( "& TapeCheckRecord: tc_tdiff: %s %s\n",
			descr->start, last_endtime );
		strcpy( last_endtime, descr->end );
		return;
	} /*endif*/
	if  (Abs(tdiff) >= descr->dt)  {
		printf( "& stream %s-%s-%c, rec-start %s, gap: %6.3f sec %6.1f samples\n",
			descr->stream.station, descr->stream.channel, descr->stream.comp,
			descr->start, tdiff, tdiff/(descr->dt) );
	} /*endif*/
	strcpy( last_endtime, descr->end );

} /* end of TapeCheckRecord */



/*---------------------------------------------------------------------*/



void TapeDumpRecord( TapeRecDescrT *descr, char outdir[] )

/* Dumps record to separate file.  Filename is generated automatically
 *
 * parameters of routine
 * TapeRecDescrT *descr;     input; record descriptor
 * char          outdir[];   input; output directory
 */
{
	/* local variables */
	static int filecnt=0;           /* file counter */
	char     fname[BC_FILELTH+1];   /* output filename */
	char     str[BC_SHORTSTRLTH+1]; /* scratch */
	int      fd;                    /* file descriptor */

	/* executable code */

	sprintf( str, "%04d", ++filecnt );
	strcpy( fname, outdir );
	strcat( fname, "tape_dump_" );
	strcat( fname, str );
	strcat( fname, ".dat" );

	if  (filecnt > 1000)  {
		printf( "*** dump of file %s rejected ***\n", fname );
		return;
	} /*endif*/

	fd = open( fname, O_WRONLY|O_TRUNC|O_CREAT, 0x1ff );
	if  (fd < 0)  {
		printf( "*** record dump to file %s failed\n", fname );
		return;
	} /*endif*/

	write( fd, (char *)(descr->dathdr), seedv_setup.seed_rec_size );

	close( fd );

	printf( "! record dumped to file %s\n", fname );

} /* end of TapeDumpRecord */



/*---------------------------------------------------------------------*/



void TapeFindLabel( TapeRecDescrT *descr, int maxlth, char label[] )

/* Finds tape label in DRM_TAPE record.  If not found *label=='\0'.
 *
 * parameters of routine
 * TapeRecDescrT *descr;         input; record descriptor
 * int           maxlth;         input; maximum length of output string
 * char          label[];        output; tape label
 */
{
	/* local variables */
	char     *ch;             /* moving pointer */
	char     *max;            /* end pointer */
	int      slen;            /* string length */

	/* executable code */

	*label = '\0';
	ch = (char *)(descr->dathdr);
	max = ch + seedv_setup.seed_rec_size;

	while  (*ch != (char)(0x0e) && *ch != (char)(0x1f))
		if  (++ch >= max)  break;
	if  (ch >= max)  {
		for  (ch = (char *)(descr->dathdr); ch < max; ch++)  {
			if  (*ch == 'S')
				if  (ch[4] == '_' || ch[5] == '_')  break;
		} /*endfor*/
		if  (ch >= max)  return;
		ch--;
	} /*endif*/

	if  (*(++ch) == 'S')  ch++;
	slen = strlen( ch );
	if  (slen > maxlth)  {
		printf( "*** tape label longer than %d chars\n", maxlth );
		return;
	} /*endif*/
	strcpy( label, ch );
	while  (label[--slen] == ' ')
		if  (slen >= 0)  label[slen] = '\0';

	printf( "\n! tape label: %s\n\n", label );

} /* end of TapeFindLabel */



/*---------------------------------------------------------------------*/



void TapePrintTrailer( TapeRecDescrT *descr, char outdir[] )

/* Prints trailer records to standard output if printable, dumps it to
 * file if request and if not printable.
 *
 * parameters of routine
 * TapeRecDescrT *descr;       input; descriptor of trailer record
 * char          outdir[];     input; output directory for dumps
 */
{
	/* local variables */
	int      i;             /* counter */
	BOOLEAN  is_printable;  /* record is printable */
	BOOLEAN  is_zero;       /* zero record */
	char     *rec;          /* pointer to logical record */

	/* executable code */

	rec = (char *)(descr->dathdr);
	is_printable = TRUE;
	is_zero = TRUE;
	for  (i=0; i<seedv_setup.seed_rec_size; i++)  {
		if  (rec[i] == '\0')  {
		} else if  ((!isprint(rec[i]))
			&& rec[i] != (char)0x0a && rec[i] != (char)0x0d)  {
			is_printable = FALSE;
			break;
		} else {
			is_zero = FALSE;
		} /*endif*/
	} /*endif*/

	if  (is_zero)  {
		printf( "\n! zero record ignored\n" );
		return;
	} else if  (is_printable)  {
		printf( "%s", rec );
		return;
	} /*endif*/

	if  (seedv_setup.dump_ill_recs)
		TapeDumpRecord( descr, outdir );

} /* end of TapePrintTrailer */



/*---------------------------------------------------------------------*/

#endif

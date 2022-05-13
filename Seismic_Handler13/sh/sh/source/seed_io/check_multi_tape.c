
/* file check_multi_tape.c
 *      ==================
 *
 * version 3, 24-May-96
 * v2: 2-Jan-96: new version of data format, including special components 'g'
 *
 * checks multiplexed SEED tape
 * K. Stammler, 7-Sep-95
 */


#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/uio.h>
#include BASECNST
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_CPAR
#include BC_ERUSRDEF
#include BC_TCUSRDEF
#include "utusrdef.h"
#include "seedcfg.h"
#include "seed_lib.h"
#include "tape_basics.h"
#include "unix_io.h"


#ifdef cBc_OS_AIX

void main( void )
{
	printf( "this is dummy\n" );
}

#else



/* tape read status codes */
#define Tape_C_OK 0
#define Tape_C_INCOMPLETE 1
#define Tape_C_EOF 2
#define Tape_C_EOM 3
#define Tape_C_READERR 4


#define CHANNUM 9
#define CHANLTH 11

#define SPECIAL_COMP 'g'


/* global variables */
static SeedSetupParT cmtv_setup;       /* SEED setup parameters */
static char cmtv_chan[CHANNUM][CHANLTH] = {
	"vbb-z", "vbb-n", "vbb-e",
	"lp-z",  "lp-n",  "lp-e",
	"vlp-z", "vlp-n", "vlp-e"
};
static NTIME cmtv_ltime[CHANNUM];       /* last channel times */


/* prototypes of local routines */
void TapeReadLogicalRecord( int tape, char buf[], int *read_lth,
	int *tape_status );



int main( int argc, char *argv[] )
{
	/* local variables */
	char     device[cBcFileLth+1];       /* tape device */
	int      tape;                       /* logical unit */
	int      read_lth;                   /* number of bytes read from tape */
	int      tape_status;                /* tape status */
	NTIME    n_start, n_end;             /* start and end time of record */
	int      reccnt;                     /* record counter */
	int      seqno;                      /* sequence number */
	char     timestr[cBcTimeLth+1];      /* time string */
	char     station[cBcLineLth+1];      /* station code */
	char     channel[cBcLineLth+1];      /* channel code */
	char     str[cBcLineLth+1];          /* scratch string */
	int      chan;                       /* channel number */
	int      i;                          /* counter */
	char     tmp;                        /* scratch */
	char     tmp2;                       /* scratch */
	int      readerrcnt;                 /* read error counter */
	float    tdiff;                      /* time distance */
	float    dt;                         /* sample distance */
	TSyStatus status;                    /* return status */
	SeedDataHeaderT *dathdr;             /* data header */
	SeedSbyteT *seedrec;                 /* pointer to SEED record */

	/* executable code */

	status = BC_NOERROR;

	/* get parameters from command line */
	pa_init( argc, argv );
	if  (pa_pnumber() != 1)  {
		fprintf( stderr, "Usage: %s <device> ***\n", argv[0] );
		/* fprintf( stderr, "       qualifers:\n" ); */
		/* fprintf( stderr, "       -sfdfile=<sfdfile>   sfd file\n" ); */
		return 1;
	} /*endif*/
	strcpy( device, pa_pvalue(1) );

	/* get Seed parameters from setup file */
	SeedSetup( &cmtv_setup, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* allocate memory for one SEED record */
	seedrec = (SeedSbyteT *)sy_allocmem( 1, cmtv_setup.seed_rec_size,
		&status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	dathdr = (SeedDataHeaderT *)seedrec;

	/* open tape device */
	tape = open( device, O_RDONLY );
	if  (tape < 0)  {
		fprintf( stderr, "*** error opening tape device %s ***\n", device );
		sy_deallocmem( seedrec );
		return 1;
	} /*endif*/

	/* rewind tape */
	TapeBasicsRewind( tape );

	reccnt = 0;
	readerrcnt = 0;
	for  (i=0; i<CHANNUM; i++)  cmtv_ltime[i].month = 0;

	printf( "! multiplexed tape\n" );

	FOREVER  {
		TapeReadLogicalRecord( tape, (char *)seedrec, &read_lth, &tape_status );
		reccnt++;
		if  (tape_status == Tape_C_OK)  {
			/* check only data records */
			if  (dathdr->indicator != 'D')  continue;
			sscanf( dathdr->seqno, "%d", &seqno );
			/* get station information */
			strncpy( station, dathdr->statcode, 5 );
			station[5] = '\0';
			i = 5;
			while  (station[--i] == ' ')
				if  (i >= 0)
					station[i] = '\0';
			ut_uncap( station );
			/* get channel information */
			strncpy( str, dathdr->channel, 3 );
			str[3] = '\0';
			ut_uncap( str );
#			ifdef XXX
			if  (strcmp(str,"log") == 0 || strcmp(str,"ace") == 0
				|| strcmp(str,"uhe") == 0 || strcmp(str,"uhn") == 0
				|| strcmp(str,"uhz") == 0 || strcmp(str,"acu") == 0)  {
				/* printf( "! channel %s-%s ignored in record %d-(%d)\n",
					station, str, seqno, reccnt ); */
				continue;
			} /*endif*/
#			endif
			tmp = Cap( dathdr->channel[0] );
			tmp2 = Cap( dathdr->channel[1] );
			if  (tmp == 'B' && tmp2 == 'H')  {
				strcpy( channel, "vbb" );
			} else if  (tmp == 'H' && tmp2 == 'H')  {
				strcpy( channel, "vsp" );
			} else if  (tmp == 'L' && tmp2 == 'H')  {
				strcpy( channel, "lp" );
			} else if  (tmp == 'V' && tmp2 == 'H')  {
				strcpy( channel, "vlp" );
			} else {
				/* printf( "*** unknown SEED channel %s-%s in record %d-(%d)\n",
					station, str, seqno, reccnt ); */
				continue;
			} /*endif*/
			i = strlen( channel );
			channel[i++] = '-';
			channel[i++] = Uncap( dathdr->channel[2] );
			channel[i] = '\0';
			/* get time information */
			SeedRecordTime( seedrec, &n_start, &n_end, &status );
			if  (SySevere(&status))  {
				printf( "*** error converting time in record %d-(%d) ***\n",
					seqno, reccnt );
				status = cBcNoError;
			} /*endif*/
			dt = SeedGetSampleDist( dathdr );
			/* get channel number */
			for  (chan=0; chan<CHANNUM; chan++)
				if  (strcmp(cmtv_chan[chan],channel) == 0)  break;
			if  (chan == CHANNUM)  {
				printf( "*** illegal channel %s-%s found in record %d-(%d) ***\n",
					station, channel, seqno, reccnt );
				continue;
			} /*endif*/
			/* compare time information */
			if  (cmtv_ltime[chan].month == 0)  {
				tc_n2t( &n_start, timestr, &status );
				status = cBcNoError;
				printf( "! new tape stream [%d]: stream %s-%s,  start %s\n",
					chan, station, channel, timestr );
			} else {
				tdiff = tc_ndiff( &n_start, cmtv_ltime+chan, &status );
				if  (SySevere(&status))  {
					printf( "*** error in time diff at record %d-(%d) ***\n",
						seqno, reccnt );
					status = cBcNoError;
					tdiff = 0.0;
				} /*endif*/
				if  (Abs(tdiff) >= dt)  {
					tc_n2t( cmtv_ltime+chan, timestr, &status );
					status = cBcNoError;
					printf(
					"& stream %s-%s, rec-start %s, gap: %6.3f sec %6.1f samples\n",
						station, channel, timestr, tdiff, tdiff/dt );
				} /*endif*/
			} /*endif*/
			cmtv_ltime[chan] = n_end;
			tc_n2t( &n_start, timestr, &status );
		} else if  (tape_status == Tape_C_INCOMPLETE)  {
			printf( "*** incomplete record at record number %d-(%d) ***\n",
				seqno, reccnt );
		} else if  (tape_status == Tape_C_EOF || tape_status == Tape_C_EOM)  {
			break;
		} else if  (tape_status == Tape_C_READERR)  {
			printf( "*** read error at record number %d-(%d) ***\n",
				seqno, reccnt );
			if  (++readerrcnt > 30)  {
				printf( "*** too many read errors. Aborted\n" );
				return 1;
			} /*endif*/
			break;
		} else {
			fprintf( stderr, "%s: bug in program (illegal tape status)\n",
				pa_progname() );
			break;
		} /*endif*/
	} /*endfor*/

	sy_deallocmem( seedrec );

	for  (i=0; i<CHANNUM; i++)  {
		if  (cmtv_ltime[i].month != 0)  {
			tc_n2t( cmtv_ltime+i, timestr, &status );
			status = cBcNoError;
			printf( "! end of stream %s-%s at %s\n",
				station, cmtv_chan[i], timestr );
		} /*endif*/
	} /*endfor*/

	return 0;

} /* end of main */




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
	for  (i=0, mem=buf; i<cmtv_setup.seed_phys_recs;
		i++, mem += cmtv_setup.phys_rec_size)  {
		read_ret = read( tape, mem, cmtv_setup.phys_rec_size );
		if  (read_ret == 0)  {
			*tape_status = (++eof_count >= cmtv_setup.eom_eof_cnt)
				? Tape_C_EOM : Tape_C_EOF;
			return;
		} else if  (read_ret == cmtv_setup.phys_rec_size)  {
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


#endif

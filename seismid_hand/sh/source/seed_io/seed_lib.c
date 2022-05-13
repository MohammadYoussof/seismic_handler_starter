
/* file seed_lib.c
 *      ==========
 *
 * version 83, 28-Nov-2007
 *
 * SEED library
 * K. Stammler, 29-Dec-93
 * v 16, 22-Nov-94, K. Stammler, added headers for VAX-GCC
 * v 18, 23-Nov-94, K. Stammler, automatic detection of necessary byte swaps
 * v 43,  7-Dec-97, K. Stammler, fixed calib-bug when reading starts in a gap
 * v 44,  9-Dec-97, K. Stammler, fixed bug in ReadNextRecord (appeared in Linux)
 * v 46,  2-Mar-98, K. Stammler, fixed bug on records with too many samples
 * v 47, 24-Mar-98, K. Stammler, was still a bug in records with too many smples
 * v 48, 24-Apr-98, K. Stammler, replaced some printf's by fprintf's
 * v 49,  1-Jun-98, K. Stammler, still another bug at records with bad samples
 * v 50, 11-Aug-98, K. Stammler, implemented offset positions in SEED files
 * v 51, 16-Mar-99, K. Stammler, use time correction field in SEED records
 * v 52, 11-Jan-00, K. Stammler, variable SEED record length
 * v 53, 14-Jun-00, K. Stammler, SeedSwapNecessary until 2030
 * v 54, 11-Jul-00, K. Stammler, handle seed->databegin other than 64er numbers
 * v 56,  6-Dec-01, K. Stammler, implementation of Steim2 input
 * v 57, 11-Jan-02, K. Stammler, increased max samples/rec to 14000
 * v 58,  8-Jul-02, K. Stammler, implement VAX word order
 * v 59, 26-Aug-02, K. Stammler, only output printable chars on SeedLibWarning
 * v 60,  9-Sep-02, K. Stammler, check for illegal 'first' entry (bus error)
 * v 61, 17-Oct-02, K. Stammler, in SeedReadStreamReal, set fdat=NULL, first
 * v 62, 17-Nov-02, K. Stammler, SeedStoreRec: swap 'first' entry on swapped rec
 * v 63, 14-Apr-05, K. Stammler, insert blockette 1000 when writing miniSeed
 * v 64, 18-Nov-05, K. Stammler, compiler error on Suse10, empty default switch
 * v 65, 21-Nov-05, K. Stammler, change long to int32
 * v 66, 25-Nov-05, K. Stammler, possibility to insert time quality in data recs
 * v 67,  7-Mar-06, K. Stammler, netcode+locid used for reading
 * v 68, 28-Apr-06, K. Stammler, fixed bug in SeedGetReclth (was no test for swap)
 * v 70, 20-May-06, K. Stammler, new file format for calibration info
 * v 71, 22-May-06, K. Stammler, new flag-parameter in SeedReadStream for qc
 * v 72, 14-Jun-06, K. Stammler, missing fclose fixed on new sensitivities file
 * v 73, 22-Oct-06, K. Stammler, string lists on sensitivity file
 * v 74,  4-Nov-06, K. Stammler, add sfdb interface
 * v 75, 12-Nov-06, K. Stammler, add routine to remove temp file of sfdb request
 * v 76, 15-Dec-06, K. Stammler, date and time in INT and DOUBLE format
 * v 77, 18-Dec-06, K. Stammler, added some debug printout
 * v 78, 21-Dec-06, K. Stammler, restructered sfdb interface (sqliface.c)
 * v 79, 28-Dec-06, K. Stammler, 'default' sensitivity replaced by $SH_INPUTS/sensitivities.txt
 * v 80, 29-Dec-06, K. Stammler, implementation of data lock flag
 * v 81,  3-Jan-07, K. Stammler, set default of seedv_use_timecorr to TRUE, TIMECORRUNIT to 10000
 * v 82, 13-Feb-07, K. Stammler, swap blockette 1000 in SwapHeader
 * v 83, 28-Nov-07, K. Stammler, accept also Q and R as data record identifier
 */


#include <stdio.h>
#include <string.h>
#include <math.h>
#include BASECNST
#ifdef BC_INC_UNISTD
#include BC_INC_UNISTD
#endif
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#ifdef BC_SUN
#include <fcntl.h>
#include <sys/types.h>
/* #include <sys/uio.h> */
#endif
#ifdef BC_VAX
#include <file.h>
#endif
#include BC_SYSBASE
#include BC_TCUSRDEF
#include BC_ERUSRDEF
#include BC_UTUSRDEF
#include "globalparams.h"
#include "seedcfg.h"
#include "seed_lib.h"

#define ANSI_EXTENSIONS
#include "steim1.h"
#undef ANSI_EXTENSIONS

#ifdef BC_VAX
#define SEEK_SET 0
#endif



/*
 * constants
 * ---------
 */

#define PREV_XN_INVALID 0x7fffffffL
#define ROOTDIR "$ROOT"
#define ROOTDIRLTH 5
#define MAX_SMP_PER_REC 14000
#define TIMECORRUNIT 10000.0
#define DEFAULT_RECLTH 4096




/*
 * global variables
 * ----------------
 */

static SeedSetupParT   seedv_setup;                 /* setup parameters */
static SeedFileDescrT  seedv_fd[Seed_C_MAXCHAN];    /* file descriptors */
static SeedDataHeaderT seedv_rec[Seed_C_MAX_RECLTH/sizeof(SeedDataHeaderT)];
                                                    /* SEED record */
static int             seedv_out_reclth=DEFAULT_RECLTH;
                                            /* record length of output files */
static BOOLEAN         seedv_logrec;                /* log records */
static BOOLEAN         seedv_warnings=TRUE;         /* print warnings */
static BOOLEAN         seedv_invert_swap_hdr=FALSE; /* invert swap hdr flag */
static BOOLEAN         seedv_capfiles=FALSE;        /* capitalized filenames */
static BOOLEAN         seedv_decerr_abort=TRUE;     /* abort on decode errors */
static BOOLEAN         seedv_use_timecorr=TRUE;     /* use time corrections */
static char            seedv_last_fname[cBcFileLth+1]; /* last file read */
static int             seedv_timequal= -1;          /* time quality */

static SeedDataHeaderT seedv_hdr_proto = {        /* SEED header prototype */
	"",                     /* seqno, sequence number string */
	'D',                    /* indicator */
	'\0',                   /* Reserved_bytes_A */
	{'X','X','X',' ',' '},  /* statcode, station name */
	{' ',' '},              /* locid, location identifier */
	{'X','X','X'},          /* channel */
	{'\0','\0'},            /* Reserved_bytes_B */
	{0,0,0,0,0,0,0},        /* starttime */
	0,                      /* no_of_samples */
	1,                      /* smprate_fact */
	1,                      /* smprate_mult */
	0,                      /* activity */
	0,                      /* ioflags */
	0,                      /* quality */
	0,                      /* no_of_blockettes */
	0,                      /* timecorr */
	0,                      /* databegin */
	0                       /* first */
}; /* end of SEED header prototype */

/*
 * prototypes of local routines
 * ----------------------------
 */

static void SeedDecodeSteim1Frame( SeedSbyteT frame[], BOOLEAN swap,
	BOOLEAN wo, INT32 diff[], int *lth );
static void SeedDecodeSteim2Frame( SeedSbyteT frame[], BOOLEAN swap,
	BOOLEAN wo, INT32 diff[], int *lth );
static void SeedAdjustFilename( SeedFileDescrT *dsc, STATUS *status );
static void SeedLibWarning( char text[] );
static void SeedSwapLinuxRecord( SeedSbyteT *rec, int reclth );
static void SeedSplitStreamName( char inpname[], char outname[],
	char station[], char addinf[] );
static void SeedFindFileInDatabase( char stream_str[], char start[],
	char dbfile[], SeedFileDescrT *fdescr, STATUS *status );

/* #define SeedLibWarning(t) fprintf( stderr, "%s", t ); */



/*---------------------------------------------------------------------*/



void SeedLibInitialize( STATUS *status )

/* Initializes SEED library routines
 *
 * parameters of routine
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	int      i;            /* counter */

	/* executable code */

	SeedGetSetup( &seedv_setup );
	for  (i=0; i<Seed_C_MAXCHAN; seedv_fd[i++].ufd = Seed_C_ILLEGAL ) {}

} /* end of SeedLibInitialize */



/*---------------------------------------------------------------------*/



void SeedDecodeSteim1( SeedSbyteT rec[], BOOLEAN swap, INT32 maxsmplth,
	INT32 smp[], INT32 *smplth )

/* Decodes Steim1-compressed data record of length specified within record.
 * Data are returned in array 'smp', its length in 'smplth'.  No more than
 * maxsmplth samples are copied to the output array.  If more samples would
 * be available an error message is printed.
 * A NULL pointer for 'rec' resets internal variables to initial state.
 *
 * Can now also read Steim2 compression, 10-Dec-2001, K. Stammler
 *
 * parameters of routine
 * SeedSbyteT rec[];          input; SEED record
 * BOOLEAN    swap;           input; swap bytes
 * INT32      maxsmplth;      input; maximum length of output array
 * INT32      smp[];          output; decoded samples
 * INT32      *smplth;        output; number of output samples
 */
{
	/* local variables */
	static INT32 prev_xn=PREV_XN_INVALID;      /* previous xn */
	static int err_cnt=0;          /* error count for diff[0]==x0 */
	SeedSbyteT *frame;             /* byte pointer */
	int        frames_per_rec;     /* number of frames/record (incl. header) */
	int        framecnt;           /* frame counter */
	int        framecnt_start;     /* frame count start value */
	INT32      x0, xn;             /* integration constants */
	INT32      diffs[Seed_C_MAXFRAMESMP];  /* differences of one frame */
	int        declth;             /* number of differences in a frame */
	char       str[BC_LINELTH+1];  /* scratch string */
	INT32      cursmp;             /* current sample */
	INT32      fsampleno;          /* number of samples in frame */
	SeedDataHeaderT *rec_h;        /* data header structure */
	INT32      rsampleno;          /* number of samples in record */
	int        reclth;             /* record length */
	int        comprs;             /* compression ID */
	int        worder;             /* word order 0=VAX,8086 1=Sparc,68000 */

	/* executable code */

	if  (rec == NULL)  {
		prev_xn = PREV_XN_INVALID;
		return;
	} /*endif*/

	rec_h = (SeedDataHeaderT *)rec;
	rsampleno = (INT32)(rec_h->no_of_samples);
	if  (maxsmplth < rsampleno)  {
		sprintf( str,
			"SeedDecodeSteim1: passed array is too short by %ld samples\n",
			rsampleno-maxsmplth );
		SeedLibWarning( str );
		rsampleno = maxsmplth;
	} /*endif*/
	/* printf( "--> databegin: %d\n", (int)rec_h->databegin ); */

	reclth = SeedGetReclth( rec_h, FALSE );
	frames_per_rec = reclth / Seed_C_FRAMELTH;
	*smplth = 0;
	frame = rec;
	if  (rsampleno <= 0)  return;

	comprs = SeedGetCompression( rec_h );
	/* assume Steim1 compression if no blockette 1000 */
	if  (comprs == 0)  {
		comprs = Seed_C_DcmpSteim1;
		/*
		SeedLibWarning( "no blockette 1000 found, assume Steim1 compression\n" );
		*/
	} /*endif*/
	if  (comprs != Seed_C_DcmpSteim1 && comprs != Seed_C_DcmpSteim2)  {
		sprintf( str, "unimplemented Compression %d\n", comprs );
		SeedLibWarning( str );
		return;
	} /*endif*/
	worder = SeedGetWordOrder( rec_h );

	/* skip all frames wich are not data frames (minus one which is */
	/* incremented inside the following loop)                       */
	if  ((int)(rec_h->databegin) % Seed_C_FRAMELTH != 0)  {
		/* illegal value of databegin, print warning and assume it as 64 */
		sprintf( str, "SeedDecodeSteim1: illegal databegin %d\n",
			(int)(rec_h->databegin) );
		SeedLibWarning( str );
		/* increase databegin over the next 64er border, will be rounded off */
		rec_h->databegin += Seed_C_FRAMELTH;
	} /*endif*/
	framecnt_start = 1;
	/* a value of 0 is corrected to 64 */
	/* values of 128 and more cause a increment of data start frame */
	if  (rec_h->databegin > Seed_C_FRAMELTH)  {
		/*printf( "--> found databegin of %d\n", (int)(rec_h->databegin) );*/
		framecnt_start = (int)(rec_h->databegin) / Seed_C_FRAMELTH;
		frame += (framecnt_start-1)*Seed_C_FRAMELTH;
	} /*endif*/

	/* loop over all frames in record (first frame is header) */
	for  (framecnt=framecnt_start; framecnt<frames_per_rec; framecnt++)  {

		frame += Seed_C_FRAMELTH;

		/* get integration constants */
		if  (framecnt == framecnt_start)  {
			x0 = SeedGet4Bytes( frame+4, swap );
			xn = SeedGet4Bytes( frame+8, swap );
		} /*endif*/

		/* get all differences of this frame */
		if  (comprs == Seed_C_DcmpSteim1)  {
			SeedDecodeSteim1Frame( frame, swap, worder, diffs, &declth );
		} else {
			/* printf( "--> decompressing Steim2\n" ); */
			SeedDecodeSteim2Frame( frame, swap, worder, diffs, &declth );
		} /*endif*/
		if  (declth > Seed_C_MAXFRAMESMP)  {printf("*****1*****\n"); exit(111);}

		/* check difference to last record */
		if  (framecnt == framecnt_start)  {
			if  (prev_xn+diffs[0] != x0 && prev_xn != PREV_XN_INVALID)  {
				if  (diffs[0] == x0)  {
					/* this is probably a common error in compression */
					if  (err_cnt++ % 100 == 0)  {
						sprintf( str,
							"\n--- Steim1: diff[0] is always equal to x0\n" );
						SeedLibWarning( str );
					} /*endif*/
				} else {
					sprintf( str, "\n--- first sample in record: wrong diff\n" );
					SeedLibWarning( str );
					sprintf( str,
					"    should be (x[-1]+diff[0]) %d, x[0] is %d, diff[0] is %ld\n",
						prev_xn+diffs[0], x0, diffs[0] );
					SeedLibWarning( str );
					/* grn_printhdr( &hdr ); */
				} /*endif*/
			} /*endif*/
			cursmp = x0 - diffs[0];  /* is undone in following loop */
		} /*endif*/

		/* integrate samples and copy to output array */
		for  (fsampleno=0; fsampleno<declth; fsampleno++)  {
			cursmp += diffs[fsampleno];
			*smp++ = cursmp;
			if  (++(*smplth) == rsampleno)  break;
		} /*endfor*/

		if  (*smplth == rsampleno)  break;

	} /*endfor*/

	if  (cursmp != xn)  {
		sprintf( str, "*** checksum not ok in record\n" );
		SeedLibWarning( str );
	} /*endif*/

	prev_xn = xn;

} /* end of SeedDecodeSteim1 */



/*----------------------------------------------------------------------------*/



static void SeedDecodeSteim1Frame( SeedSbyteT frame[], BOOLEAN swap,
	BOOLEAN wo, INT32 diff[], int *lth )

/* Decodes differences of a single frame.
 *
 * parameters of routine
 * SeedSbyteT *frame;     input; byte stream
 * BOOLEAN    swap;       input; swap bytes
 * BOOLEAN    wo;         input; word order
 * INT32      diff[];     output; decoded differences
 * int        *lth;       output; number of differences
 */
{
	/* local variables */
	unsigned INT32  nibbles;    /* 2-bit nibbles */
	int             diffcnt;    /* difference counter */
	int             shift;      /* bit shift length */
	int             diff_id;    /* difference 2-bit ID (0,1,2,3) */
	short int       si;         /* scratch */
	unsigned INT32  ultmp;      /* scratch */
	SeedSbyteT      sbyte;      /* scratch */

	/* executable code */

	nibbles = SeedGet4Bytes( frame, swap );
	*lth = 0;
	for  (diffcnt=0; diffcnt<Seed_C_FRAMEDIFFS; diffcnt++)  {
		shift = 30 - diffcnt*2;
		diff_id = (int)((nibbles >> shift) & 3);
		switch  (diff_id)  {
		case 1:
			ultmp = SeedGet4Bytes( frame, swap );
			frame += 4;
			if  (wo == 0)  {
				sbyte = (SeedSbyteT)(ultmp & 0x000000ffL);
				diff[(*lth)++] = sbyte;
				sbyte = (SeedSbyteT)((ultmp & 0x0000ff00L) >>  8);
				diff[(*lth)++] = sbyte;
				sbyte = (SeedSbyteT)((ultmp & 0x00ff0000L) >> 16);
				diff[(*lth)++] = sbyte;
				sbyte = (SeedSbyteT)((ultmp & 0xff000000L) >> 24);
				diff[(*lth)++] = sbyte;
			} else {
				sbyte = (SeedSbyteT)((ultmp & 0xff000000L) >> 24);
				diff[(*lth)++] = sbyte;
				sbyte = (SeedSbyteT)((ultmp & 0x00ff0000L) >> 16);
				diff[(*lth)++] = sbyte;
				sbyte = (SeedSbyteT)((ultmp & 0x0000ff00L) >>  8);
				diff[(*lth)++] = sbyte;
				sbyte = (SeedSbyteT)(ultmp & 0x000000ffL);
				diff[(*lth)++] = sbyte;
			} /*endif*/
			break;
		case 2:
			ultmp = SeedGet4Bytes( frame, swap );
			frame += 4;
			if  (wo == 0)  {
				si = (short int)(ultmp & 0x0000ffffL);
				diff[(*lth)++] = si;
				si = (short int)((ultmp & 0xffff0000L) >> 16);
				diff[(*lth)++] = si;
			} else {
				si = (short int)((ultmp & 0xffff0000L) >> 16);
				diff[(*lth)++] = si;
				si = (short int)(ultmp & 0x0000ffffL);
				diff[(*lth)++] = si;
			} /*endif*/
			break;
		case 3:
			diff[(*lth)++] = (INT32)SeedGet4Bytes( frame, swap );
			frame += 4;
			break;
		default:
			frame += 4;  /* no data */
		} /*endswitch*/
	} /*endfor*/

} /* end of SeedDecodeSteim1Frame */



/*----------------------------------------------------------------------------*/



static void SeedDecodeSteim2Frame( SeedSbyteT frame[], BOOLEAN swap,
	int wo, INT32 diff[], int *lth )

/* Decodes differences of a single frame, Steim2.
 *
 * parameters of routine
 * SeedSbyteT *frame;     input; byte stream
 * BOOLEAN    swap;       input; swap bytes
 * int        wo;         input; word order
 * INT32      diff[];     output; decoded differences
 * int        *lth;       output; number of differences
 */
{
	/* local variables */
	unsigned INT32  nibbles;    /* 2-bit nibbles */
	int             diffcnt;    /* difference counter */
	int             shift;      /* bit shift length */
	int             diff_id;    /* difference 2-bit ID (0,1,2,3) */
	unsigned int    dnib;       /* decode nibble in decoded longword */
	unsigned int    udiff[8];   /* difference decoded */
	short int       si;         /* scratch */
	unsigned INT32  ultmp;      /* scratch */
	SeedSbyteT      sbyte;      /* scratch */
	int             i;          /* counter */

	/* executable code */

	if  (wo == 0)  {
		fprintf( stderr,
			"  Steim2: word order 0 (80x86) not implemented, check data\n" );
	} /*endif*/

	nibbles = SeedGet4Bytes( frame, swap );
	*lth = 0;
	for  (diffcnt=0; diffcnt<Seed_C_FRAMEDIFFS; diffcnt++)  {
		shift = 30 - diffcnt*2;
		diff_id = (int)((nibbles >> shift) & 3);
		switch  (diff_id)  {
		case 1:  /* 01 */
			/* this is four 8-bit differences */
			ultmp = SeedGet4Bytes( frame, swap );
			frame += 4;
			sbyte = (SeedSbyteT)((ultmp & 0xff000000L) >> 24);
			diff[(*lth)++] = sbyte;
			sbyte = (SeedSbyteT)((ultmp & 0x00ff0000L) >> 16);
			diff[(*lth)++] = sbyte;
			sbyte = (SeedSbyteT)((ultmp & 0x0000ff00L) >>  8);
			diff[(*lth)++] = sbyte;
			sbyte = (SeedSbyteT)(ultmp & 0x000000ffL);
			diff[(*lth)++] = sbyte;
			break;
		case 2:  /* 10 */
			/* ask high order two bits of decoded longword */
			ultmp = SeedGet4Bytes( frame, swap );
			frame += 4;
			dnib = (ultmp >> 30) & 3;
			switch  (dnib)  {
			case 1:  /* 01 */
				/* one 30-bit difference, set high oder bits acc. bit number 30 */
				if  (ultmp & 0x20000000L)
					diff[(*lth)++] = (int)(ultmp | 0xc0000000L);
				else
					diff[(*lth)++] = (int)(ultmp & 0x3fffffffL);
				break;
			case 2:  /* 10 */
				/* two 15 bit differences */
				udiff[0] = (ultmp >> 15) & 0x00007fffL;
				udiff[1] =  ultmp        & 0x00007fffL;
				for  (i=0; i<2; i++)
					if  (udiff[i] & 0x00004000L)
						diff[(*lth)++] = (int)(udiff[i] | 0xffff8000L);
					else
						diff[(*lth)++] = udiff[i];
				break;
			case 3:  /* 11 */
				udiff[0] = (ultmp >> 20) & 0x000003ffL;
				udiff[1] = (ultmp >> 10) & 0x000003ffL;
				udiff[2] =  ultmp        & 0x000003ffL;
				for  (i=0; i<3; i++)
					if  (udiff[i] & 0x00000200L)
						diff[(*lth)++] = (int)(udiff[i] | 0xfffffc00L);
					else
						diff[(*lth)++] = udiff[i];
				break;
			default:
				/* no data, ignore */;
			} /*endswitch*/
			break;
		case 3:  /* 11 */
			/* ask high order two bits of decoded longword */
			ultmp = SeedGet4Bytes( frame, swap );
			frame += 4;
			dnib = (ultmp >> 30) & 3;
			switch  (dnib)  {
			case 0:  /* 00 */
				udiff[0] = (ultmp >> 24) & 0x0000003fL;
				udiff[1] = (ultmp >> 18) & 0x0000003fL;
				udiff[2] = (ultmp >> 12) & 0x0000003fL;
				udiff[3] = (ultmp >>  6) & 0x0000003fL;
				udiff[4] =  ultmp        & 0x0000003fL;
				for  (i=0; i<5; i++)
					if  (udiff[i] & 0x00000020L)
						diff[(*lth)++] = (int)(udiff[i] | 0xffffffc0L);
					else
						diff[(*lth)++] = udiff[i];
				break;
			case 1:  /* 01 */
				udiff[0] = (ultmp >> 25) & 0x0000001fL;
				udiff[1] = (ultmp >> 20) & 0x0000001fL;
				udiff[2] = (ultmp >> 15) & 0x0000001fL;
				udiff[3] = (ultmp >> 10) & 0x0000001fL;
				udiff[4] = (ultmp >>  5) & 0x0000001fL;
				udiff[5] =  ultmp        & 0x0000001fL;
				for  (i=0; i<6; i++)
					if  (udiff[i] & 0x00000010L)
						diff[(*lth)++] = (int)(udiff[i] | 0xffffffe0L);
					else
						diff[(*lth)++] = udiff[i];
				break;
			case 2:  /* 10 */
				udiff[0] = (ultmp >> 24) & 0x0000000fL;
				udiff[1] = (ultmp >> 20) & 0x0000000fL;
				udiff[2] = (ultmp >> 16) & 0x0000000fL;
				udiff[3] = (ultmp >> 12) & 0x0000000fL;
				udiff[4] = (ultmp >>  8) & 0x0000000fL;
				udiff[5] = (ultmp >>  4) & 0x0000000fL;
				udiff[6] =  ultmp        & 0x0000000fL;
				for  (i=0; i<7; i++)
					if  (udiff[i] & 0x00000008L)
						diff[(*lth)++] = (int)(udiff[i] | 0xfffffff0L);
					else
						diff[(*lth)++] = udiff[i];
				break;
			default:
				/* no data, ignore */;
			} /*endswitch*/
			break;
		default: /* 00 */
			frame += 4;  /* no data */
		} /*endswitch*/
	} /*endfor*/

} /* end of SeedDecodeSteim2Frame */



/*----------------------------------------------------------------------------*/



unsigned short int SeedGet2Bytes( SeedSbyteT b[], BOOLEAN swap )

/* returns word from byte stream
 *
 * parameters of routine
 * SeedSbyteT b[];     input;   byte stream
 * BOOLEAN    swap;    input;   swap bytes
 */
{
	/* local variables */
	unsigned short int   word;

	/* executable code */

	if  (swap)  {
		word = ((unsigned short)(b[0]) & 0xff) * 0x100;
		word += (unsigned short)(b[1]) & 0xff;
	} else {
		word = ((unsigned short)(b[1]) & 0xff) * 0x100;
		word += (unsigned short)(b[0]) & 0xff;
	} /*endif*/
	return word;

} /* end of SeedGet2Bytes */



/*----------------------------------------------------------------------------*/



unsigned INT32 SeedGet4Bytes( SeedSbyteT b[], BOOLEAN swap )

/* returns word from byte stream
 *
 * parameters of routine
 * SeedSbyteT b[];     input;   byte stream
 * BOOLEAN    swap;    input;   swap bytes
 */
{
	/* local variables */
	unsigned INT32   lword;

	/* executable code */

	if  (swap)  {
		lword = ((unsigned INT32)(b[0]) & 0xffL) * 0x1000000L;
		lword += ((unsigned INT32)(b[1]) & 0xffL) * 0x10000L;
		lword += ((unsigned INT32)(b[2]) & 0xffL) * 0x100;
		lword += (unsigned INT32)b[3] & 0xffL;
	} else {
		lword = ((unsigned INT32)(b[3]) & 0xffL) * 0x1000000L;
		lword += ((unsigned INT32)(b[2]) & 0xffL) * 0x10000L;
		lword += ((unsigned INT32)(b[1]) & 0xffL) * 0x100;
		lword += (unsigned INT32)b[0] & 0xffL;
	} /*endif*/
	return lword;

} /* end of SeedGet4Bytes */



/*---------------------------------------------------------------------*/



void SeedFindFileInSfd( char stream_str[], char start[], char sfdfile[],
	SeedFileDescrT *fdescr, STATUS *status )

/* Finds SEED file in sfd-file of given stream and containing given
 * start time.  If no file is found the file with the next possible
 * time is returned
 *
 * parameters of routine
 * char       stream_str[];      input; stream string like "bfo-vbb-z"
 * char       start[];           input; start time
 * char       sfdfile[];         input; SEED file directory
 * SeedFileDescrT *fdescr;       output; SEED file descriptor
 * STATUS     *status;           output; return status
 */
{
	/* local variables */
	FILE     *sfd;                          /* pointer to sfd-file */
	char     line[Seed_C_SFDLINELTH+1];     /* current line */
	float    tdiff;                         /* time difference */
	float    min_tdiff;                     /* minimum tdiff */
	SeedFileDescrT next;                    /* file with next time */
	SeedFileDescrT curr;                    /* current line */
	char     lstream_str[BC_SHORTSTRLTH+1]; /* local lowercase stream string */
	char     station[cBcShortStrLth+1];     /* station name */
	char     xaddinf[Seed_C_AddinfLth+1];   /* netcode+locid searched */
	TSyBoolean from_db;                     /* DB file instead of sfdfile */
	TSyBoolean firstline;                   /* first line of sfdfile */

	/* executable code */

	if  (strlen(stream_str) > BC_SHORTSTRLTH)  {
		*status = SeedERR_STROVFL;
		err_setcontext( " ## stream " );
		err_setcontext( stream_str );
		return;
	} /*endif*/

	SeedSplitStreamName( stream_str, lstream_str, station, xaddinf );

	/* make stream string lowercase */
	ut_uncap( lstream_str );

	if  (GpGetInt(cGpI_debug_level) > 5)
		printf( "SEED-dbg6: bench: in SeedFindFileInSfd, conventional sfdfile\n" );

	/* initialize local descriptors */
	strcpy( next.stream, lstream_str );
	strcpy( curr.stream, lstream_str );
	next.t_start[0] = curr.t_start[0] = '\0';
	next.t_end[0] = curr.t_end[0] = '\0';
	next.name[0] = curr.name[0] = '\0';
	strcpy( next.sfdfile, sfdfile );
	strcpy( curr.sfdfile, sfdfile );
	next.recno = curr.recno = 0;
	next.ufd = curr.ufd = Seed_C_ILLEGAL;
	next.pos = curr.pos = 0;
	next.sample = curr.sample = 0;
	next.calib = curr.calib = 0.0;
	next.dataflags = curr.dataflags = 0;
	next.reclth = curr.reclth = DEFAULT_RECLTH;
	next.byteoff = curr.byteoff = 0;
	next.pri = curr.pri = 0;
	next.dataformat = curr.dataformat = Seed_C_FORMAT_MSEED;
	strcpy( next.addinf, Seed_C_EmptyAddinfStr );
	strcpy( curr.addinf, Seed_C_EmptyAddinfStr );

	sfd = sy_fopen( sfdfile, "r" );
	if  (sfd == NULL)  {
		*status = SeedERR_OPENINPUT;
		err_setcontext( " ## file " );
		err_setcontext( sfdfile );
		return;
	} /*endif*/

	/* find requested stream and time in sfd-file*/
	from_db = FALSE;  /* assume 'standard' sfdfile */
	firstline = TRUE;
	next.name[0] = '\0';
	min_tdiff = 1.0e10;
	fdescr->t_start[0] = '\0';
	while  (fgets(line,Seed_C_SFDLINELTH,sfd) != NULL)  {
		if  (*line == '!')  continue;
		if  (firstline)  {
			firstline = FALSE;
			if  (strncmp(line,"station",7) == 0)  {
				from_db = TRUE;
				continue; /* first line in db file contains no data */
			} /*endif*/
		} /*endif*/
		if  (from_db)  {
			SqlParseSftabLine( line, station, &curr, status );
			if  (seedv_invert_swap_hdr)  curr.swap_hdr = ! curr.swap_hdr;
		} else {
			SeedParseSfdLine( line, &curr, status );
		} /*endif*/
		if  (Severe(status))  {fclose(sfd); return;}
		if  (strcmp(curr.stream,lstream_str) != 0)  continue;
		if  (strcmp(xaddinf,Seed_C_EmptyAddinfStr) != 0 
			&& strcmp(xaddinf,curr.addinf) != 0)  continue;
		tdiff = tc_tdiff( start, curr.t_start, status );
		if  (Severe(status))  {fclose(sfd); return;}
		if  (!GpGetBoolean(cGpB_ignore_data_lock)
			&& (curr.dataflags & Seed_F_QUAL_DATA_LOCKED))  continue;
		if  (tdiff >= 0.0 && tc_tdiff(start,curr.t_end,status) < 0.0)  {
			/* found matching entry */
			if  (fdescr->t_start[0] != '\0')  {
				/* already found a previous matching entry */
				if  (fdescr->pri >= curr.pri)  continue; /* this has lower priority */
			} /*endif*/
			*fdescr = curr;
			if  (fdescr->calib == 0.0)  {
				fdescr->calib =
					SeedFindStreamCalibration( fdescr->stream, start, status );
				if  (*status == SeedERR_NOCALIB)  {
					*status = cBcNoError;
					fdescr->dataflags |= Seed_F_QUAL_NOCALIB;
				} /*endif*/
			} /*endif*/
			if  (strncmp(fdescr->name,ROOTDIR,ROOTDIRLTH) == 0)
				SeedAdjustFilename( fdescr, status );
			if  (GpGetInt(cGpI_debug_level) > 5)
				printf( "SEED-dbg6: bench: returning from SeedFindFileInSfd, conventional sfdfile (1)\n" );
			if  (!from_db)  {
				/* take first matching entry only in sfdfile */
				fclose( sfd );
				return;
			} /*endif*/
		} else if  (tdiff < 0.0)  {
			/* entries ordered by time, return if a matching entry previously found */
			if  (fdescr->t_start[0] != '\0')  {
				fclose( sfd );
				return;
			} /*endif*/
			/* store file with time next to requested */
			if  (Severe(status))  {fclose(sfd); return;}
			if  (next.name[0] == '\0' || tdiff > min_tdiff)  {
				min_tdiff = tdiff;
				next = curr;
			} /*endif*/
			if  (next.calib == 0.0)  {
				next.calib =
					SeedFindStreamCalibration( next.stream, start, status );
				if  (*status == SeedERR_NOCALIB)  {
					*status = cBcNoError;
					next.dataflags |= Seed_F_QUAL_NOCALIB;
				} /*endif*/
			} /*endif*/
		} /*endif*/
		if  (Severe(status))  {fclose(sfd); return;}
	} /*endwhile*/

	/* if a matching entry has been found, return here */
	if  (fdescr->t_start[0] != '\0')  {
		fclose( sfd );
		return;
	} /*endif*/

	*status = (next.name[0] == '\0') ? SeedERR_SFD_NOTFOUND : SeedERR_SFD_NEXT;
	if  (*status == SeedERR_SFD_NOTFOUND)  {
		err_setcontext( " ## stream " );
		err_setcontext( lstream_str );
		err_setcontext( ", addinf " );
		err_setcontext( xaddinf );
		err_setcontext( ", time " );
		err_setcontext( start );
	} /*endif*/
	fclose( sfd );
	*fdescr = next;
	if  (strncmp(fdescr->name,ROOTDIR,ROOTDIRLTH) == 0)
		SeedAdjustFilename( fdescr, status );
	if  (GpGetInt(cGpI_debug_level) > 5)
		printf( "SEED-dbg6: bench: returning from SeedFindFileInSfd, conventional sfdfile (2)\n" );
	return;

} /* end of SeedFindFileInSfd */



/*---------------------------------------------------------------------*/

#ifdef XXX

static void SeedFindFileInDatabase( char stream_str[], char start[],
	char dbfile[], SeedFileDescrT *fdescr, STATUS *status )

/* Finds SEED file in sfdb database. If no file is found the file with the
 * next possible time is returned
 *
 * parameters of routine
 * char       stream_str[];      input; stream string like "bfo-vbb-z"
 * char       start[];           input; start time
 * char       dbfile[];          input; DB output file
 * SeedFileDescrT *fdescr;       output; SEED file descriptor
 * STATUS     *status;           output; return status
 */
{
	/* local variables */
	static char last_start[cBcTimeLth+1]="";  /* last start time */
	char     shellcmd[cBcVeryLongStrLth+1];   /* shell command */
	char     tmpfileb[cBcFileLth+1];          /* scratch file 2 */
	FILE     *fp;                             /* pointer to file */
	char     tmpstr[cBcShortStrLth+1];        /* scratch string */
	char     station[cBcShortStrLth+1];       /* passed station name */
	char     chan[cBcShortStrLth+1];          /* passed channel name */
	char     comp;                            /* passed component name */
	char     tstation[cBcShortStrLth+1];      /* current station name */
	char     tchan[cBcShortStrLth+1];         /* current channel name */
	char     tcomp;                           /* current component name */
	int      i;                               /* counter */
	NTIME    ntime, ntimeb;                   /* numeric read time */
	char     line[cBcLineLth+1];              /* current line of file */
	int      pathid, pathidx;                 /* path ID */
	char     relpath[cBcLineLth+1];           /* relative pathname */
	SeedFileDescrT descr, descrx;             /* Seed file descr */
	float    tdiff;                           /* time difference */
	char     *rootpath;                       /* pointer to root path */
	int      pri, prix;                       /* priority of entry */
	int      dataformat;                      /* data format (not used yet) */
	int      datea, dateb;                    /* dates of seed entry */
	double   timea, timeb;                    /* times of entry */
	int      rddate;                          /* read date as integer */
	double   rdtime;                          /* read time as double */

	/* executable code */

	/* parse stream string */
	if  (strlen(stream_str) > cBcShortStrLth)  {
		*status = SeedERR_STROVFL;
		return;
	} /*endif*/
	strcpy( tmpstr, stream_str );
	ut_uncap( tmpstr );
	for  (i=0; tmpstr[i] != '\0'; i++)
		if  (tmpstr[i] == '-')  tmpstr[i] = ' ';
	i = sscanf( tmpstr, "%s %s %c", station, chan, &comp );
	if  (i < 3)  comp = ' ';
	if  (i < 2)  strcpy( chan , "  " );
	if  (i < 1)  strcpy( station , "   " );

	fp = fopen( dbfile, "r" );
	if  (fp == NULL)  {
		*status = SeedERR_BUG;
		err_setcontext( " ## error opening database file " );
		err_setcontext( dbfile );
		return;
	} /*endif*/
	/* read off header */
	fgets( line, cBcLineLth, fp );

	/* read through all entries (shouldn't be more than 2) */
	descr.ufd = Seed_C_ILLEGAL;
	descr.pos = 0;
	descr.sample = 0;
	strcpy( descr.addinf, Seed_C_EmptyAddinfStr );
	descr.sfdfile[0] = descrx.sfdfile[0] = '\0';
	descr.calib = 0.0;
	pri = prix = 0;
	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		/* parse line read */
		if  (sscanf(line,"%s %s %c %d %s %d %lf %d %lf %d %d %d %d %d %d %d",
			tstation,tchan,&tcomp,&pathid,relpath,&datea,&timea,&dateb,&timeb,
			&descr.recno,&descr.swap_hdr,&descr.reclth,&descr.byteoff,
			&descr.dataflags,&pri,&dataformat) != 16)  {
			*status = SeedERR_DBREAD;
			fclose( fp );
			return;
		} /*endif*/
		if  (strcmp(station,tstation) != 0)  continue;
		if  (strcmp(chan,tchan) != 0)  continue;
		if  (comp != tcomp )  continue;
		if  (GpGetInt(cGpI_debug_level) > 5)
			printf( "SHM-dbg6: got line %sSHM-dbg6: read times %d,%lf %d,%lf\n",
				line, datea, timea, dateb, timeb );
		ntime.year = datea / 10000;
		ntime.month = (datea % 10000) / 100;
		ntime.day = datea % 100;
		ntime.hour = Nint( timea / 10000.0 - 0.499 );
		timea -= (double)ntime.hour*10000.0;
		ntime.min = Nint( timea / 100.0 - 0.499 );
		timea -= (double)ntime.min*100.0;
		ntime.sec = Nint( timea - 0.499 );
		timea -= (double)ntime.sec;
		ntime.ms = Nint( timea*1000.0 );
		if  (GpGetInt(cGpI_debug_level) > 5)
			printf( "SHM-dbg6: start ntime: %d,%d,%d,%d,%d,%d,%d\n", ntime.year,
				ntime.month, ntime.day, ntime.hour, ntime.min, ntime.sec, ntime.ms);
		tc_n2t( &ntime, descr.t_start, status );
		if  (SySevere(status))  {fclose(fp); return;}
		ntime.year = dateb / 10000;
		ntime.month = (dateb % 10000) / 100;
		ntime.day = dateb % 100;
		ntime.hour = Nint( timeb / 10000.0 - 0.499 );
		timeb -= (double)ntime.hour*10000.0;
		ntime.min = Nint( timeb / 100.0 - 0.499 );
		timeb -= (double)ntime.min*100.0;
		ntime.sec = Nint( timeb - 0.499 );
		timeb -= (double)ntime.sec;
		ntime.ms = Nint( timeb*1000.0 );
		if  (GpGetInt(cGpI_debug_level) > 5)
			printf( "SHM-dbg6: start ntime: %d,%d,%d,%d,%d,%d,%d\n", ntime.year,
				ntime.month, ntime.day, ntime.hour, ntime.min, ntime.sec, ntime.ms);
		tc_n2t( &ntime, descr.t_end, status );
		if  (SySevere(status))  {fclose(fp); return;}
		if  (strlen(station)+strlen(chan)+3 > cBcShortStrLth)  {
			*status = SeedERR_STROVFL;
			return;		
		} /*endif*/
		/* database list may contain files which do not match the requested time */
		/* if priority is lower than the one we have, ignore entry */
		if  (pri < prix)  continue;
		/* if we already have a matching entry, compare with this here */
		if  (descrx.sfdfile[0] != '\0')  {
			/* take the one with earlier start time */
			tdiff = tc_tdiff( descr.t_start, descrx.t_start, status );
			if  (SySevere(status))  return;
			if  (tdiff > 0.0)  continue;
		} /*endif*/
		sprintf( descr.stream, "%s-%s-%c", station, chan, comp );
		strcpy( descr.name, relpath );
		strcpy( descr.sfdfile, "db:" );
		descrx = descr;
		prix = pri;
		pathidx = pathid;
	} /*endfor*/

	fclose( fp );

	if  (descrx.sfdfile[0] == '\0')  {
		*status = SeedERR_SFD_NOTFOUND;
		return;
	} /*endif*/
	descr = descrx;
	strcpy( relpath, descr.name );
	pathid = pathidx;

	/* now find absolute path, first look in cache */
	rootpath = SeedGetRootPath( pathid );
	if  (rootpath == NULL)  {
		/* second temporary file */
		i = 1;
		FOREVER  {
			sprintf( tmpfileb, "/tmp/sfdb_read_b_%d.000", i++ );
			fp = fopen( tmpfileb, "r" );
			if  (fp == NULL)  break;
			fclose( fp );
		} /*endfor*/
		/* create empty file to reserve name */
		fp = fopen( tmpfileb, "w" );
		fclose( fp );
		/* not in cache, get it from database */
		sprintf( shellcmd,
			"\\rm %s; %s %s \"select rootpath from pathtab where id = \'%d\'\" >%s",
			tmpfileb, GpGetString(cGpS_sfdb_command),
			GpGetString(cGpS_sfdb_exec_qual), pathid, tmpfileb );
		/*printf( "--> executing %s\n", shellcmd );*/
		system( shellcmd );

		/* read output file */
		fp = fopen( tmpfileb, "r" );
		if  (fp == NULL)  {
			*status = SeedERR_BUG;
			err_setcontext( " ## error opening scratch file" );
			return;
		} /*endif*/
		/* read off header */
		fgets( line, cBcLineLth, fp );
		fgets( descr.name, cBcFileLth, fp );
		i = strlen( descr.name );
		if  (i > 0 && descr.name[i-1] == '\n')  descr.name[i-1] = '\0';
		fclose( fp );
		/* put it to cache */
		SeedCacheRootPath( pathid, descr.name, status );
		sy_fdelete( tmpfileb );
	} else {
		strcpy( descr.name, rootpath );
	} /*endif*/

	strcat( descr.name, "/" );
	strcat( descr.name, relpath );

	if  (strlen(start) > cBcTimeLth)  {
		*status = SeedERR_BUG;
		err_setcontext( " ## time tring too long" );
		return;
	} /*endif*/

	descr.calib = SeedFindStreamCalibration( descr.stream, start, status );
	if  (*status == SeedERR_NOCALIB)  {
		*status = cBcNoError;
		descr.dataflags |= Seed_F_QUAL_NOCALIB;
	} /*endif*/
	if  (seedv_invert_swap_hdr)  descr.swap_hdr = ! descr.swap_hdr;
	*fdescr = descr;

} /* end of SeedFindFileInDatabase */

#endif

/*---------------------------------------------------------------------*/

#ifdef XXX

static void SeedFindFileInDatabaseOld( char stream_str[], char start[],
	SeedFileDescrT *fdescr, STATUS *status )

/* Finds SEED file in sfdb database. If no file is found the file with the
 * next possible time is returned
 *
 * parameters of routine
 * char       stream_str[];      input; stream string like "bfo-vbb-z"
 * char       start[];           input; start time
 * char       sfdfile[];         input; SEED file directory
 * SeedFileDescrT *fdescr;       output; SEED file descriptor
 * STATUS     *status;           output; return status
 */
{
	/* local variables */
	static char last_start[cBcTimeLth+1]="";  /* last start time */
	char     shellcmd[cBcVeryLongStrLth+1];   /* shell command */
	char     tmpfileb[cBcFileLth+1];          /* scratch file 2 */
	FILE     *fp;                             /* pointer to file */
	char     tmpstr[cBcShortStrLth+1];        /* scratch string */
	char     station[cBcShortStrLth+1];       /* passed station name */
	char     chan[cBcShortStrLth+1];          /* passed channel name */
	char     comp;                            /* passed component name */
	char     tstation[cBcShortStrLth+1];      /* current station name */
	char     tchan[cBcShortStrLth+1];         /* current channel name */
	char     tcomp;                           /* current component name */
	int      i;                               /* counter */
	NTIME    ntime, ntimeb;                   /* numeric read time */
	char     line[cBcLineLth+1];              /* current line of file */
	int      pathid, pathidx;                 /* path ID */
	char     relpath[cBcLineLth+1];           /* relative pathname */
	SeedFileDescrT descr, descrx;             /* Seed file descr */
	float    tdiff;                           /* time difference */
	char     *rootpath;                       /* pointer to root path */
	int      pri, prix;                       /* priority of entry */
	int      dataformat;                      /* data format (not used yet) */
#ifdef USE_DATETIME
	char     datea[cBcShortStrLth+1];         /* date 1 */
	char     timea[cBcShortStrLth+1];         /* time 1 */
	int      msa;                             /* millisecs 1 */
	char     dateb[cBcShortStrLth+1];         /* date 2 */
	char     timeb[cBcShortStrLth+1];         /* time 2 */
	int      msb;                             /* millisecs 2 */
#else
	int      datea, dateb;                    /* dates of seed entry */
	double   timea, timeb;                    /* times of entry */
	int      rddate;                          /* read date as integer */
	double   rdtime;                          /* read time as double */
#endif

	/* executable code */

	/* parse stream string */
	if  (strlen(stream_str) > cBcShortStrLth)  {
		*status = SeedERR_STROVFL;
		return;
	} /*endif*/
	strcpy( tmpstr, stream_str );
	ut_uncap( tmpstr );
	for  (i=0; tmpstr[i] != '\0'; i++)
		if  (tmpstr[i] == '-')  tmpstr[i] = ' ';
	i = sscanf( tmpstr, "%s %s %c", station, chan, &comp );
	if  (i < 3)  comp = ' ';
	if  (i < 2)  strcpy( chan , "  " );
	if  (i < 1)  strcpy( station , "   " );

	/* static temporary file, create if no call to db before */
	if  (last_start[0] == '\0')  {
		i = 1;
		FOREVER  {
			sprintf( seedv_sfdb_tmp, "/tmp/sfdb_read_a_%d.000", i++ );
			fp = fopen( seedv_sfdb_tmp, "r" );
			if  (fp == NULL)  break;
			fclose( fp );
		} /*endfor*/
		/* create empty file to reserve name */
		fp = fopen( seedv_sfdb_tmp, "w" );
		fclose( fp );
	} /*endif*/

	/* convert time to numeric format */
	tc_t2n( start, &ntime, status );
	if  (SySevere(status))  return;
	tc_nadd( &ntime, 1.0, &ntimeb, status );
	if  (SySevere(status))  return;

	if  (strcmp(start,last_start) != 0)  {
		/* call to db if new start time given */
#ifdef USE_DATETIME
		sprintf( shellcmd,
			"\\rm %s; %s %s \"select * from sftab where stime <= \'%4d-%02d-%02d %02d:%02d:%02d\' and etime >= \'%4d-%02d-%02d %02d:%02d:%02d\'\" >%s",
			seedv_sfdb_tmp, GpGetString(cGpS_sfdb_command), GpGetString(cGpS_sfdb_exec_qual),
			ntime.year, ntime.month, ntime.day,
			ntime.hour, ntime.min, ntime.sec, ntimeb.year, ntimeb.month, ntimeb.day,
			ntimeb.hour, ntimeb.min, ntimeb.sec, seedv_sfdb_tmp );
#else
		rddate = ntime.year*10000 + ntime.month*100 + ntime.day;
		rdtime = (double)ntime.hour*10000.0 + (double)ntime.min*100.0
			+ (double)ntime.sec + (double)ntime.ms/1000.0;
		sprintf( shellcmd,
			"\\rm %s; %s %s \"select * from sftab where ((sdate < %d) OR (sdate = %d AND stime <= %10.3f)) AND ((%d < edate) OR (%d = edate AND %10.3f <= etime))\" >%s",
			seedv_sfdb_tmp, GpGetString(cGpS_sfdb_command),
			GpGetString(cGpS_sfdb_exec_qual), rddate, rddate, rdtime, rddate,
			rddate, rdtime, seedv_sfdb_tmp );
#endif
		if  (GpGetInt(cGpI_debug_level) > 3)
			printf( "SH-dbg4: executing %s\n", shellcmd );
		system( shellcmd );
		strcpy( last_start, start );
	} /*endif*/

	fp = fopen( seedv_sfdb_tmp, "r" );
	if  (fp == NULL)  {
		*status = SeedERR_BUG;
		err_setcontext( " ## error opening scratch file " );
		err_setcontext( seedv_sfdb_tmp );
		return;
	} /*endif*/
	/* read off header */
	fgets( line, cBcLineLth, fp );

	/* read through all entries (shouldn't be more than 2) */
	descr.ufd = Seed_C_ILLEGAL;
	descr.pos = 0;
	descr.sample = 0;
	strcpy( descr.addinf, Seed_C_EmptyAddinfStr );
	descr.sfdfile[0] = descrx.sfdfile[0] = '\0';
	descr.calib = 0.0;
	pri = prix = 0;
	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		/* parse line read */
#ifdef USE_DATETIME
		if  (sscanf(line,"%s %s %c %d %s %s %s %d %s %s %d %d %d %d %d %d %d",
			tstation,tchan,&tcomp,&pathid,relpath,datea,timea,&msa,dateb,timeb,
			&msb,&descr.recno,&descr.swap_hdr,&descr.reclth,&descr.byteoff,
			&descr.dataflags,&pri) != 17)  {
			*status = SeedERR_DBREAD;
			fclose( fp );
			return;
		} /*endif*/
#else
		if  (sscanf(line,"%s %s %c %d %s %d %lf %d %lf %d %d %d %d %d %d %d",
			tstation,tchan,&tcomp,&pathid,relpath,&datea,&timea,&dateb,&timeb,
			&descr.recno,&descr.swap_hdr,&descr.reclth,&descr.byteoff,
			&descr.dataflags,&pri,&dataformat) != 16)  {
			*status = SeedERR_DBREAD;
			fclose( fp );
			return;
		} /*endif*/
#endif
		if  (strcmp(station,tstation) != 0)  continue;
		if  (strcmp(chan,tchan) != 0)  continue;
		if  (comp != tcomp )  continue;
		if  (GpGetInt(cGpI_debug_level) > 5)
			printf( "SHM-dbg6: got line %sSHM-dbg6: read times %d,%lf %d,%lf\n",
				line, datea, timea, dateb, timeb );
#ifdef USE_DATETIME
		/* change '-'s to '/'s in dates, so that tc_... routines can read it */
		for  (i=0; datea[i] != '\0'; i++)
			if  (datea[i] == '-')  datea[i] = '/';
		for  (i=0; dateb[i] != '\0'; i++)
			if  (dateb[i] == '-')  dateb[i] = '/';
		sprintf( descr.t_start, "%s_%s", datea, timea );
		sprintf( descr.t_end, "%s_%s", dateb, timeb );
		/* add the milliseconds, which were not used in DB search */
		tc_tadd( descr.t_start, (float)msa/1000.0, descr.t_start, status );
		if  (SySevere(status))  {fclose(fp); return;}
		tc_tadd( descr.t_end, (float)msb/1000.0, descr.t_end, status );
		if  (SySevere(status))  {fclose(fp); return;}
		tdiff = tc_tdiff( start, descr.t_start, status );
		if  (SySevere(status))  {fclose(fp); return;}
		if  (tdiff < 0.0)  continue;  /* then the milliseconds didn't match */
		tdiff = tc_tdiff( descr.t_end, start, status );
		if  (SySevere(status))  {fclose(fp); return;}
		if  (tdiff < 0.0)  continue;  /* then the milliseconds didn't match */
#else
		ntime.year = datea / 10000;
		ntime.month = (datea % 10000) / 100;
		ntime.day = datea % 100;
		ntime.hour = Nint( timea / 10000.0 - 0.499 );
		timea -= (double)ntime.hour*10000.0;
		ntime.min = Nint( timea / 100.0 - 0.499 );
		timea -= (double)ntime.min*100.0;
		ntime.sec = Nint( timea - 0.499 );
		timea -= (double)ntime.sec;
		ntime.ms = Nint( timea*1000.0 );
		if  (GpGetInt(cGpI_debug_level) > 5)
			printf( "SHM-dbg6: start ntime: %d,%d,%d,%d,%d,%d,%d\n", ntime.year,
				ntime.month, ntime.day, ntime.hour, ntime.min, ntime.sec, ntime.ms);
		tc_n2t( &ntime, descr.t_start, status );
		if  (SySevere(status))  {fclose(fp); return;}
		ntime.year = dateb / 10000;
		ntime.month = (dateb % 10000) / 100;
		ntime.day = dateb % 100;
		ntime.hour = Nint( timeb / 10000.0 - 0.499 );
		timeb -= (double)ntime.hour*10000.0;
		ntime.min = Nint( timeb / 100.0 - 0.499 );
		timeb -= (double)ntime.min*100.0;
		ntime.sec = Nint( timeb - 0.499 );
		timeb -= (double)ntime.sec;
		ntime.ms = Nint( timeb*1000.0 );
		if  (GpGetInt(cGpI_debug_level) > 5)
			printf( "SHM-dbg6: start ntime: %d,%d,%d,%d,%d,%d,%d\n", ntime.year,
				ntime.month, ntime.day, ntime.hour, ntime.min, ntime.sec, ntime.ms);
		tc_n2t( &ntime, descr.t_end, status );
		if  (SySevere(status))  {fclose(fp); return;}
#endif
		if  (strlen(station)+strlen(chan)+3 > cBcShortStrLth)  {
			*status = SeedERR_STROVFL;
			return;		
		} /*endif*/
		/* if priority is lower than the one we have, ignore entry */
		if  (pri < prix)  continue;
		/* if we already have a matching entry, compare with this here */
		if  (descrx.sfdfile[0] != '\0')  {
			/* take the one with earlier start time */
			tdiff = tc_tdiff( descr.t_start, descrx.t_start, status );
			if  (SySevere(status))  return;
			if  (tdiff > 0.0)  continue;
		} /*endif*/
		sprintf( descr.stream, "%s-%s-%c", station, chan, comp );
		strcpy( descr.name, relpath );
		strcpy( descr.sfdfile, "db:" );
		descrx = descr;
		prix = pri;
		pathidx = pathid;
	} /*endfor*/

	fclose( fp );

	if  (descrx.sfdfile[0] == '\0')  {
		*status = SeedERR_SFD_NOTFOUND;
		return;
	} /*endif*/
	descr = descrx;
	strcpy( relpath, descr.name );
	pathid = pathidx;

	/* now find absolute path, first look in cache */
	rootpath = SeedGetRootPath( pathid );
	if  (rootpath == NULL)  {
		/* second temporary file */
		i = 1;
		FOREVER  {
			sprintf( tmpfileb, "/tmp/sfdb_read_b_%d.000", i++ );
			fp = fopen( tmpfileb, "r" );
			if  (fp == NULL)  break;
			fclose( fp );
		} /*endfor*/
		/* create empty file to reserve name */
		fp = fopen( tmpfileb, "w" );
		fclose( fp );
		/* not in cache, get it from database */
		sprintf( shellcmd,
			"\\rm %s; %s %s \"select rootpath from pathtab where id = \'%d\'\" >%s",
			tmpfileb, GpGetString(cGpS_sfdb_command),
			GpGetString(cGpS_sfdb_exec_qual), pathid, tmpfileb );
		/*printf( "--> executing %s\n", shellcmd );*/
		system( shellcmd );

		/* read output file */
		fp = fopen( tmpfileb, "r" );
		if  (fp == NULL)  {
			*status = SeedERR_BUG;
			err_setcontext( " ## error opening scratch file" );
			return;
		} /*endif*/
		/* read off header */
		fgets( line, cBcLineLth, fp );
		fgets( descr.name, cBcFileLth, fp );
		i = strlen( descr.name );
		if  (i > 0 && descr.name[i-1] == '\n')  descr.name[i-1] = '\0';
		fclose( fp );
		/* put it to cache */
		SeedCacheRootPath( pathid, descr.name, status );
		sy_fdelete( tmpfileb );
	} else {
		strcpy( descr.name, rootpath );
	} /*endif*/

	strcat( descr.name, "/" );
	strcat( descr.name, relpath );

	if  (strlen(start) > cBcTimeLth)  {
		*status = SeedERR_BUG;
		err_setcontext( " ## time tring too long" );
		return;
	} /*endif*/

	descr.calib = SeedFindStreamCalibration( descr.stream, start, status );
	if  (*status == SeedERR_NOCALIB)  {
		*status = cBcNoError;
		descr.dataflags |= Seed_F_QUAL_NOCALIB;
	} /*endif*/
	if  (seedv_invert_swap_hdr)  descr.swap_hdr = ! descr.swap_hdr;
	*fdescr = descr;

} /* end of SeedFindFileInDatabaseOld */

#endif

/*---------------------------------------------------------------------*/



void SeedSearchPosition( int chan, char sfdfile[], char stream_str[],
	char req_start[], char act_start[], STATUS *status )

/* Searches for given time position in a stream file.  The corresponding
 * SEED file is opened and positioned, its file descriptor stored
 * in an internal variable.  'chan' must range from 0..Seed_C_MAXCHAN-1.
 * If the channel is already used the previously opened file is closed.
 *
 * parameters of routine
 * int        chan;             input; channel number
 * char       sfdfile[];        input; sfd-file
 * char       stream_str[];     input; stream string
 * char       req_start[];      input; requested start time
 * char       act_start[];      output; actual start time
 * STATUS     *status;          output; return status
 */
{
	/* local variables */
	BOOLEAN  next_time;      /* only next time possible */
	float    tdiff_tot;      /* total time window of file */
	float    tdiff_start;    /* difference to request time */
	SeedFileDescrT *fd;      /* pointer to file descriptor */
	INT32    seek_pos;       /* seek position */
	NTIME    req_ntime;      /* requested time in NTIME format */
	NTIME    cur_ntime;      /* start time of current SEED record */
	float    cur_span;       /* time span (sec) of current SEED record */
	float    dt;             /* sample distance in sec */
	int      jump_rec;       /* number of records to jump */
	int      new_pos;        /* new record position */
	int      jump_cnt;       /* jump counter */

	/* executable code */

	if  (chan < 0 || chan >= Seed_C_MAXCHAN)  {
		*status = SeedERR_CHAN_OOR;
		return;
	} /*endif*/
	fd = seedv_fd + chan;

	if  (fd->ufd != Seed_C_ILLEGAL)  {
		close( fd->ufd );
		fd->ufd = Seed_C_ILLEGAL;
	} /*endif*/

	SeedFindFileInSfd( stream_str, req_start, sfdfile, fd, status );
	next_time =  (*status == SeedERR_SFD_NEXT);
	if  (next_time)  *status = BC_NOERROR;
	if  (Severe(status))  return;

	if  (GpGetInt(cGpI_debug_level) > 4)
		printf( "Seed-dbg5: opening file %s\n", fd->name );
	fd->ufd = open( fd->name, O_RDONLY );
	if  (fd->ufd < 0)  {
		*status = SeedERR_OPENINPUT;
		err_setcontext( " ## file " );
		err_setcontext( fd->name );
		fd->ufd = Seed_C_ILLEGAL;
		return;
	} /*endif*/

	/* opened new file, jump to offset position if any */
	if  (fd->byteoff > 0)  {
		if  (lseek(fd->ufd,fd->byteoff,SEEK_SET) != fd->byteoff)  {
			*status = SeedERR_SEEDSEEK;
			err_setcontext( " ## file " );
			err_setcontext( fd->name );
			return;
		} /*endif*/
	} /*endif*/

	if  (next_time)  {
		strcpy( act_start, fd->t_start );
		*status = SeedERR_NEXTTIMEPOS;
		return;
	} /*endif*/
	tc_t2n( req_start, &req_ntime, status );
	if  (Severe(status))  return;

	/* compute approximate record position */
	tdiff_tot = tc_tdiff( fd->t_end, fd->t_start, status );
	if  (Severe(status))  return;
	tdiff_start = tc_tdiff( req_start, fd->t_start, status );
	if  (Severe(status))  return;
	new_pos = Nint( (float)((fd->recno)-1)/tdiff_tot * tdiff_start );

	for  (jump_cnt=0; jump_cnt<4; jump_cnt++)  {

		/* position SEED file */
		if  (new_pos != fd->pos)  {
			if  (new_pos >= fd->recno)  new_pos = fd->recno-1;
			if  (new_pos < 0)  new_pos = 0;
			fd->pos = new_pos;
			seek_pos = (INT32)(fd->pos) * (INT32)(fd->reclth) + (INT32)(fd->byteoff);
			if  (seedv_logrec)  printf( "-" );
			if  (lseek(fd->ufd,seek_pos,SEEK_SET) != seek_pos)  {
				*status = SeedERR_SEEDSEEK;
				err_setcontext( " ## file " );
				err_setcontext( fd->name );
				return;
			} /*endif*/
		} /*endif*/

		/* read current SEED record */
		if  (seedv_logrec)  printf( "[%d]", fd->pos );  /* log */
		if  (read(fd->ufd,(char *)seedv_rec,fd->reclth) != fd->reclth)  {
			*status = SeedERR_SEEDREAD;
			err_setcontext( " ## file " );
			err_setcontext( fd->name );
			return;
		} /*endif*/
		(fd->pos)++;
		SeedStoreReclth( seedv_rec, fd->reclth );
		if  (fd->swap_hdr)  SeedSwapHeader( seedv_rec );
		dt = SeedGetSampleDist( seedv_rec );

		/* check time found (return if time found) */
		SeedBtimeToNtime( &(seedv_rec->starttime), &cur_ntime, status );
		if  (Severe(status))  return;
		/* check for time correction, K.S. 16-Mar-99 */
		/* In old sfdfiles where the time correction is not applied */
		/* this may cause problems, since the specified time windows are */
		/* not exact.  This case is handled later. */
		if  (seedv_use_timecorr && (seedv_rec->timecorr != 0)
			&& (((int)(seedv_rec->activity) & Seed_F_ACT_TIMECORRAPP) == 0))  {
			tc_nadd( &cur_ntime, (float)(seedv_rec->timecorr)/TIMECORRUNIT,
				&cur_ntime, status );
			if  (Severe(status))  return;
		} /*endif*/
		tdiff_start = tc_ndiff( &req_ntime, &cur_ntime, status );
		if  (Severe(status))  return;
		cur_span = (float)(seedv_rec->no_of_samples) * dt;
		if  (tdiff_start >= 0.0)  {
			if  (cur_span > tdiff_start)  {
				/* found it ! */
				seek_pos = (INT32)(--(fd->pos)) * (INT32)(fd->reclth)
					+ (INT32)(fd->byteoff);
				lseek( fd->ufd, seek_pos, SEEK_SET );
				fd->sample = Nint( tdiff_start/dt );
				if  (fd->sample >/*=*/ (int)(seedv_rec->no_of_samples))  {
					printf( "! SeedSearchPosition: sample position adjusted (1)\n" );
					fd->sample = seedv_rec->no_of_samples /*- 1*/;
				} /*endif*/
				tc_nadd( &cur_ntime, (fd->sample)*dt, &req_ntime, status );
				if  (Severe(status))  return;
				tc_n2t( &req_ntime, act_start, status );
				return;
			} /*endif*/
		} /*endif*/

		/* estimate next jump */
		if  (tdiff_start > 0.0)  tdiff_start -= cur_span;
		jump_rec = Nint( floor(tdiff_start/cur_span) );
		if  (jump_rec >= 0)  jump_rec++;  else  jump_rec--;
		new_pos = fd->pos - 1 + jump_rec;

	} /*endfor*/

	/* if still not found then read record by record */
	if  (seedv_logrec)  printf( "|" );

	/* go back to time before */
	tdiff_start = tc_ndiff( &req_ntime, &cur_ntime, status );
	while  (tdiff_start < 0.0)  {
		fd->pos -= 2;
		if  (fd->pos < 0)  {
			if  ((tdiff_start > ((-1.5)*(float)(seedv_rec->timecorr)/TIMECORRUNIT))
				&& seedv_use_timecorr)  {
				/* This may happen if the sfdfile does not contain possible time  */
				/* corrections of the data.  Try to fix this by positioning to    */
				/* a time shifted by the correction.  The samples before this time*/
				/* are not read.  Call this routine recursively.                  */
				{
					char  newtime[cBcTimeLth+1];    /* new time position */
					tc_tadd( req_start, (float)(seedv_rec->timecorr)/TIMECORRUNIT,
						newtime, status );
					if  (SySevere(status))  {
						fprintf( stderr,
							"*** SeedSearchPosition: error in time calc\n");
						*status = SeedERR_BUG;
						return;
					} /*endif*/
					fprintf( stderr,
						"SeedSearchPosition: try to fix time correction bug\n" );
					SeedSearchPosition( chan, sfdfile, stream_str, newtime,
						act_start, status );
					return;
				}
			} else {
				fprintf( stderr, "*** SeedSearchPosition: check sfdfile\n" );
				*status = SeedERR_BUG;
				return;
			} /*endif*/
		} /*endif*/
		seek_pos = (INT32)(fd->pos) * (INT32)(fd->reclth) + (INT32)(fd->byteoff);
		if  (seedv_logrec)  printf( "-" );
		if  (lseek(fd->ufd,seek_pos,SEEK_SET) != seek_pos)  {
			*status = SeedERR_SEEDSEEK;
			err_setcontext( " ## file " );
			err_setcontext( fd->name );
			return;
		} /*endif*/
		if  (seedv_logrec)  printf( "[%d]", fd->pos );  /* log */
		if  (read(fd->ufd,(char *)seedv_rec,fd->reclth) != fd->reclth)  {
			*status = SeedERR_SEEDREAD;
			err_setcontext( " ## file " );
			err_setcontext( fd->name );
			return;
		} /*endif*/
		(fd->pos)++;
		SeedStoreReclth( seedv_rec, fd->reclth );
		if  (fd->swap_hdr)  SeedSwapHeader( seedv_rec );
		dt = SeedGetSampleDist( seedv_rec );
		SeedBtimeToNtime( &(seedv_rec->starttime), &cur_ntime, status );
		if  (Severe(status))  return;
		/* check for time correction, K.S. 16-Mar-99 */
		if  (seedv_use_timecorr && (seedv_rec->timecorr != 0)
			&& (((int)(seedv_rec->activity) & Seed_F_ACT_TIMECORRAPP) == 0))  {
			tc_nadd( &cur_ntime, (float)(seedv_rec->timecorr)/TIMECORRUNIT,
				&cur_ntime, status );
			if  (Severe(status))  return;
		} /*endif*/
		tdiff_start = tc_ndiff( &req_ntime, &cur_ntime, status );
		if  (Severe(status))  return;
		cur_span = (float)(seedv_rec->no_of_samples) * dt;
	} /*endwhile*/

	/* now find forward */
	while  (tdiff_start >= cur_span)  {
		if  (seedv_logrec)  printf( "[%d]", fd->pos );  /* log */
		if  (read(fd->ufd,(char *)seedv_rec,fd->reclth) != fd->reclth)  {
			*status = SeedERR_SEEDREAD;
			err_setcontext( " ## file " );
			err_setcontext( fd->name );
			return;
		} /*endif*/
		(fd->pos)++;
		SeedStoreReclth( seedv_rec, fd->reclth );
		if  (fd->swap_hdr)  SeedSwapHeader( seedv_rec );
		dt = SeedGetSampleDist( seedv_rec );
		SeedBtimeToNtime( &(seedv_rec->starttime), &cur_ntime, status );
		if  (Severe(status))  return;
		/* check for time correction, K.S. 16-Mar-99 */
		if  (seedv_use_timecorr && (seedv_rec->timecorr != 0)
			&& (((int)(seedv_rec->activity) & Seed_F_ACT_TIMECORRAPP) == 0))  {
			tc_nadd( &cur_ntime, (float)(seedv_rec->timecorr)/TIMECORRUNIT,
				&cur_ntime, status );
			if  (Severe(status))  return;
		} /*endif*/
		tdiff_start = tc_ndiff( &req_ntime, &cur_ntime, status );
		if  (Severe(status))  return;
		cur_span = (float)(seedv_rec->no_of_samples) * dt;
	} /*endwhile*/

	/* step one record back to get it again on next read command */
	seek_pos = (INT32)(--(fd->pos)) * (INT32)(fd->reclth) + (INT32)(fd->byteoff);
	lseek( fd->ufd, seek_pos, SEEK_SET );

	if  (tdiff_start >= 0.0)  {
		/* now found it */
		fd->sample = Nint( tdiff_start/dt );
		if  (fd->sample >/*=*/ (int)(seedv_rec->no_of_samples))  {
			printf( "! SeedSearchPosition: sample position adjusted (2)\n" );
			fd->sample = seedv_rec->no_of_samples /*- 1*/;
		} /*endif*/
		tc_nadd( &cur_ntime, (fd->sample)*dt, &req_ntime, status );
		if  (Severe(status))  return;
		tc_n2t( &req_ntime, act_start, status );
	} else {
		/* time gap */
		/* printf( "*** time gap: next possible time positioned ***\n" ); */
		tc_n2t( &cur_ntime, act_start, status );
		*status = SeedERR_NEXTTIMEPOS;
	} /*endif*/

} /* end of SeedSearchPosition */



/*----------------------------------------------------------------------------*/



void SeedBtimeToNtime( SeedBtimeT *btime, NTIME *ntime, STATUS *status )

/* converts SeedBtimeT to NTIME
 *
 * parameters of routine
 * SeedBtimeT *bt;      input; SEED binary time
 * NTIME      *nt;      output; KS numeric time
 * STATUS     *status;  output; return status
 */
{
	/* executable code */

	ntime->year = btime->year;
	tc_dayofmn( btime->year, btime->day, &(ntime->month),
		&(ntime->day), status );
	if  (Severe(status))  return;
	ntime->hour = btime->hours;
	ntime->min = btime->minutes;
	ntime->sec = btime->seconds;
	ntime->ms = btime->frac_secs/10;

} /* end of SeedBtimeToNtime */



/*----------------------------------------------------------------------------*/



void SeedNtimeToBtime( NTIME *ntime, SeedBtimeT *btime, STATUS *status )

/* converts NTIME to BTIME
 *
 * parameters of routine
 * NTIME      *nt;      input; KS numeric time
 * BTIME      *bt;      output; SEED binary time
 * STATUS     *status;  output; return status
 */
{
	/* executable code */

	btime->year = ntime->year;
	btime->day = tc_julian( ntime->year, ntime->month,
		ntime->day, status );
	if  (Severe(status))  return;
	btime->hours = ntime->hour;
	btime->minutes = ntime->min;
	btime->seconds = ntime->sec;
	btime->frac_secs = ntime->ms*10;

} /* end of SeedNtimeToBtime */



/*---------------------------------------------------------------------*/



float SeedGetSampleDist( SeedDataHeaderT *hdr )

/* Returns sample distance in sec.
 *
 * parameters of routine
 * SeedDataHeaderT  *hdr;        input; SEED data header
 *                               returns sample distance in sec
 */
{
	/* executable code */

	if  (hdr->smprate_fact > 0 && hdr->smprate_mult > 0)  {
		return (1.0/((float)(hdr->smprate_fact) * (float)(hdr->smprate_mult)));
	} else if  (hdr->smprate_fact > 0 && hdr->smprate_mult < 0)  {
		return (-1.0/((float)(hdr->smprate_fact)/(float)(hdr->smprate_mult)));
	} else if  (hdr->smprate_fact < 0 && hdr->smprate_mult > 0)  {
		return (-1.0/((float)(hdr->smprate_mult)/(float)(hdr->smprate_fact)));
	} else if  (hdr->smprate_fact < 0 && hdr->smprate_mult < 0)  {
		return (1.0/((float)(hdr->smprate_fact)/(float)(hdr->smprate_mult)));
	} else {
		fprintf( stderr, "*** illegal sample rate, set dt=1.0\n" );
		return 1.0;
	} /*endif*/

} /* end of SeedGetSampleDist */



/*---------------------------------------------------------------------*/



void SeedReadNextRecord( int chan, SeedSbyteT *rec, BOOLEAN *hdr_swapped,
	STATUS *status )

/* Reads next available record from SEED file.  If the header must be
 * swapped this is done and indicated in '*hdr_swapped'.
 *
 * parameters of routine
 * int        chan;               input; channel number
 * SeedSbyteT *rec;               output; SEED record
 * BOOLEAN    *hdr_swapped;       output; tells whether header was swapped
 * STATUS     *status;            output; return status
 */
{
	/* local variables */
	SeedFileDescrT *fd;                  /* SEED file descriptor */
	char     l_sfdfile[BC_FILELTH+1];    /* local sfdfile */
	char     l_stream[BC_SHORTSTRLTH+1]; /* local stream string */
	char     l_t_end[BC_TIMELTH+1];      /* end of file time */
	char     act_time[BC_TIMELTH+1];     /* positioned time */
	BOOLEAN  time_gap;                   /* time gap found */

	/* executable code */

	/* check channel number */
	if  (chan < 0 || chan >= Seed_C_MAXCHAN)  {
		*status = SeedERR_CHAN_OOR;
		return;
	} /*endif*/
	fd = seedv_fd + chan;

	if  (fd->ufd == Seed_C_ILLEGAL)  {
		*status = SeedERR_NOT_OPEN;
		return;
	} /*endif*/

	/* store filename for last used file request */
	strcpy( seedv_last_fname, fd->name );

	time_gap = FALSE;
	if  (fd->pos >= fd->recno)  {
		/* is at end of file */
		strcpy( l_sfdfile, fd->sfdfile );
		strcpy( l_stream, fd->stream );
		strcpy( l_t_end, fd->t_end );
		SeedSearchPosition( chan, l_sfdfile, l_stream, l_t_end,
			act_time, status );
		time_gap = (*status == SeedERR_NEXTTIMEPOS);
		if  (time_gap)  *status = BC_NOERROR;
		if  (Severe(status))  return;
	} /*endif*/

	if  (seedv_logrec)  printf( "[%d]", fd->pos );  /* log */
	if  (read(fd->ufd,(char *)rec,fd->reclth) != fd->reclth)  {
		*status = SeedERR_SEEDREAD;
		err_setcontext( " ## file " );
		err_setcontext( fd->name );
		return;
	} /*endif*/
	(fd->pos)++;
	SeedStoreReclth( (SeedDataHeaderT *)rec, fd->reclth );
	if  (fd->swap_hdr)  SeedSwapHeader( (SeedDataHeaderT *)rec );
	*hdr_swapped = fd->swap_hdr;

	if  (time_gap)  *status = SeedERR_NEXTTIMEPOS;

} /* end of SeedReadNextRecord */



/*---------------------------------------------------------------------*/



void SeedEncodeSteim1( int fd, INT32 smp[], INT32 *prev_smp, INT32 *idx,
	NTIME *ctime, SeedSbyteT *wrk, int wrklth, BOOLEAN complete, STATUS *status )

/* Encodes sample array smp to SEED output file fd (UNIX file descriptor).
 * If complete is set to TRUE the last (probably not completely used)
 * record is written to the file, otherwise the samples of the
 * last record remain in the sample array.
 *
 * parameters of routine
 * int        fd;            input; UNIX file descr of output file
 * INT32      smp[];         modify; sample array to encode
 * INT32      *prev_smp;     modify; last sample before smp[0]
 * INT32      *idx;          modify; length of sample array
 * NTIME      *ctime;        modify; input: start time, output: new start time
 * SeedSbyteT *wrk;          scratch; work space for encoding
 * int        wrklth;        input; length of work space in records
 * BOOLEAN    complete;      input; finish up all records (-> *idx == 0)
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	static int rec_cnt=1;            /* record counter */
	int      recs_used;              /* number of records used */
	SeedDataHeaderT *hdr;            /* SEED data header */
	int      i;                      /* counter */
	char     str[BC_SHORTSTRLTH+1];  /* scratch string */
	int      write_ret;              /* write return value */
	int      smp_written;            /* total number of samples written to file*/
	float    dt;                     /* sample distance */
	int      rlexp;                  /* record length exponent */
	SeedBlockette1000T *b1000;       /* blockette 1000 */
	SeedBlockette1001T *b1001;       /* blockette 1001 */

	/* executable code */

	/* put start time to header prototype */
	SeedNtimeToBtime( ctime, &seedv_hdr_proto.starttime, status );
	if  (Severe(status))  return;

	if  (*idx <= 0)  return;

	/* compute record length exponent, needed for blockette 1000 */
	i = 1;
	rlexp = 0;
	while  (i < seedv_out_reclth)  {
		i *= 2;
		rlexp++;
	} /*endif*/

	/* encode data */
	recs_used = (int)Steim_comp( smp, (DATA_HEADER *)&seedv_hdr_proto,
		(UINT32)(*idx), (WORD)(seedv_out_reclth), (INT32 *)wrk, *prev_smp);
	if  (recs_used > wrklth)  {
		fprintf( stderr,
			"***!!! SeedEncodeSteim1: record array exceeded -> abort !!!***\n" );
		exit( 1 );
	} /*endif*/

	/* if last record not to be written, keep it in smp-buffer */
	if  (complete)  {
		*prev_smp = (*idx > 0) ? smp[(*idx)-1] : 0;
		*idx = 0;
	} else {
		if  (recs_used == 1)  {
			/* printf( "*** SeedEncodeSteim1: only 1 record, no flush\n" ); */
			return;
		} /*endif*/
		hdr = (SeedDataHeaderT *)(wrk + (recs_used-1)*seedv_out_reclth);
		*prev_smp = smp[(*idx)-hdr->no_of_samples-1];
		memcpy( (char *)smp, (char *)(smp+(*idx)-(int)(hdr->no_of_samples)),
			(int)(hdr->no_of_samples)*sizeof(INT32) );
		*idx = hdr->no_of_samples;
		recs_used--;
	} /*endif*/

	/* write records to file */
	smp_written = 0;
	for  (i=0; i<recs_used; i++)  {
		hdr = (SeedDataHeaderT *)(wrk+i*seedv_out_reclth);
		sprintf( str, "%06d", rec_cnt++ );
		strncpy( (char *)hdr, str, 6 );
		hdr->Reserved_bytes_A = ' ';
		/* insert blockette 1000, 14-Apr-2005 */
		hdr->no_of_blockettes = 1;
		hdr->first = 48;
		b1000 = (SeedBlockette1000T *)((char *)hdr + (unsigned)(hdr->first));
		b1000->type = 1000;
		b1000->nextblock = 0;
		b1000->format = (UBYTE)Seed_C_DcmpSteim1;
		b1000->wordorder = (UBYTE)1;
		b1000->reclthexp = (UBYTE)rlexp;
		b1000->reserved = (UBYTE)0;
		/* end of blockette 1000 code */
		/* insert blockette 1001 if time quality >= 0 */
		if  (seedv_timequal >= 0)  {
			hdr->no_of_blockettes = 2;
			b1000->nextblock = 56;
			b1001 = (SeedBlockette1001T *)((char *)hdr + (unsigned)(b1000->nextblock));
			b1001->type = 1001;
			b1001->nextblock = 0;
			b1001->timequal = (UBYTE)seedv_timequal;
			b1001->reserved = (UBYTE)0;
			/* reset timequal to -1 */
			seedv_timequal = -1;
		} /*endif*/
		/* end of blockette 1001 code */
#		ifdef cBc_OS_LINUX
		SeedStoreReclth( hdr, seedv_out_reclth );
		SeedSwapHeader( hdr );
		SeedSwapLinuxRecord( (SeedSbyteT *)hdr, seedv_out_reclth );
#		endif
		write_ret = (int)write( fd, (char *)hdr, seedv_out_reclth );
#		ifdef cBc_OS_LINUX
		/* swap it back otherwise not readable any more */
		SeedSwapHeader( hdr );
		SeedSwapLinuxRecord( (SeedSbyteT *)hdr, seedv_out_reclth );
#		endif
		if  (write_ret != seedv_out_reclth)  {
			*status = SeedERR_SEEDWRITE;
			return;
		} /*endif*/
		smp_written += hdr->no_of_samples;
	} /*endfor*/

	/* update start time */
	dt = SeedGetSampleDist( hdr );
	tc_nadd( ctime, (float)smp_written*dt, ctime, status );

} /* end of SeedEncodeSteim1 */



/*---------------------------------------------------------------------*/



void SeedEncodeSteim1Chan( int fd, INT32 smp[], INT32 *prev_smp, INT32 *idx,
	NTIME *ctime, int *reccnt, SeedDataHeaderT *seedhdr, SeedSbyteT *wrk,
	int wrklth, BOOLEAN complete, STATUS *status )

/* Encodes sample array smp to SEED output file fd (UNIX file descriptor).
 * If complete is set to TRUE the last (probably not completely used)
 * record is written to the file, otherwise the samples of the
 * last record remain in the sample array.
 *
 * This is the same routine as SeedEncodeSteim1Chan except two additional
 * parameters reccnt and seedhdr.
 *
 * parameters of routine
 * int        fd;            input; UNIX file descr of output file
 * INT32      smp[];         modify; sample array to encode
 * INT32      *prev_smp;     modify; last sample before smp[0]
 * INT32      *idx;          modify; length of sample array
 * NTIME      *ctime;        modify; input: start time, output: new start time
 * int        *reccnt;       modify; record counter
 * SeedDataHeaderT *seedhdr; modify; SEED data header
 * SeedSbyteT *wrk;          scratch; work space for encoding
 * int        wrklth;        input; length of work space in records
 * BOOLEAN    complete;      input; finish up all records (-> *idx == 0)
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	int      recs_used;              /* number of records used */
	SeedDataHeaderT *hdr;            /* SEED data header */
	int      i;                      /* counter */
	char     str[BC_SHORTSTRLTH+1];  /* scratch string */
	int      write_ret;              /* write return value */
	int      smp_written;            /* total number of samples written to file*/
	float    dt;                     /* sample distance */

	/* executable code */

	/* put start time to header prototype */
	SeedNtimeToBtime( ctime, &(seedhdr->starttime), status );
	if  (Severe(status))  return;

	if  (*idx <= 0)  return;

	/* encode data */
	recs_used = (int)Steim_comp( smp, (DATA_HEADER *)seedhdr,
		(UINT32)(*idx), (WORD)(seedv_out_reclth), (INT32 *)wrk, *prev_smp );
	if  (recs_used > wrklth)  {
		fprintf( stderr,
			"***!!! SeedEncodeSteim1: record array exceeded -> abort !!!***\n" );
		exit( 1 );
	} /*endif*/

	/* if last record not to be written, keep it in smp-buffer */
	if  (complete)  {
		*prev_smp = (*idx > 0) ? smp[(*idx)-1] : 0;
		*idx = 0;
	} else {
		if  (recs_used == 1)  {
			/* printf( "*** SeedEncodeSteim1: only 1 record, no flush\n" ); */
			return;
		} /*endif*/
		hdr = (SeedDataHeaderT *)(wrk + (recs_used-1)*seedv_out_reclth);
		*prev_smp = smp[(*idx)-hdr->no_of_samples-1];
		memcpy( (char *)smp, (char *)(smp+(*idx)-(int)(hdr->no_of_samples)),
			(int)(hdr->no_of_samples)*sizeof(INT32) );
		*idx = hdr->no_of_samples;
		recs_used--;
	} /*endif*/

	/* write records to file */
	smp_written = 0;
	for  (i=0; i<recs_used; i++)  {
		hdr = (SeedDataHeaderT *)(wrk+i*seedv_out_reclth);
		sprintf( str, "%06d", *(reccnt)++ );
		strncpy( (char *)hdr, str, 6 );
		hdr->Reserved_bytes_A = ' ';
#		ifdef cBc_OS_LINUX
		SeedStoreReclth( hdr, seedv_out_reclth );
		SeedSwapHeader( hdr );
		SeedSwapLinuxRecord( (SeedSbyteT *)hdr, seedv_out_reclth );
#		endif
		write_ret = (int)write( fd, (char *)hdr, seedv_out_reclth );
#		ifdef cBc_OS_LINUX
		/* swap it back otherwise not readable any more */
		SeedSwapHeader( hdr );
		SeedSwapLinuxRecord( (SeedSbyteT *)hdr, seedv_out_reclth );
#		endif
		if  (write_ret != seedv_out_reclth)  {
			*status = SeedERR_SEEDWRITE;
			return;
		} /*endif*/
		smp_written += hdr->no_of_samples;
	} /*endfor*/

	/* update start time */
	dt = SeedGetSampleDist( hdr );
	tc_nadd( ctime, (float)smp_written*dt, ctime, status );

} /* end of SeedEncodeSteim1Chan */



/*---------------------------------------------------------------------*/



void SeedSetHeaderPrototype( SeedDataHeaderT *hdr )

/* copies data header to global variables
 *
 * parameters of routine
 * SeedDataHeaderT *hdr;        input; data header prototype
 */
{
	/* executable code */

	seedv_hdr_proto = *hdr;

} /* end of SeedSetHeaderPrototype */



/*---------------------------------------------------------------------*/



void SeedRecordTime( SeedSbyteT *rec, NTIME *start, NTIME *end,
	STATUS *status )

/* Returns start and end time of SEED record
 *
 * parameters of routine
 * SeedSbyteT *rec;           input; SEED record
 * NTIME      *start;         output; start time of record
 * NTIME      *end;           output; end time of record
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	SeedDataHeaderT *hdr;    /* pointer to data header */
	float    dt;            /* sample distance in sec */

	/* executable code */

	hdr = (SeedDataHeaderT *)rec;

	SeedBtimeToNtime( &(hdr->starttime), start, status );
	if  (Severe(status))  return;
	dt = SeedGetSampleDist( hdr );
	tc_nadd( start, dt*(float)(hdr->no_of_samples), end, status );
	if  (Severe(status))  return;

	if  (seedv_use_timecorr && (hdr->timecorr != 0)
		&& (((int)(hdr->activity) & Seed_F_ACT_TIMECORRAPP) == 0))  {
		tc_nadd( start, (float)(hdr->timecorr)/TIMECORRUNIT, start, status );
		tc_nadd( end, (float)(hdr->timecorr)/TIMECORRUNIT, end, status );
	} /*endif*/

} /* end of SeedRecordTime */



/*---------------------------------------------------------------------*/



/* #ifdef XXX */

#define BUFLTH 4096
/* should be enough for decoding one record */

#define FREE_AND_RETURN		sy_deallocmem( smp ); \
									sy_deallocmem( buf ); \
									sy_deallocmem( encode ); \
									close( out ); \
									return
/* on failure free all memory */




void SeedCopy( int chan, char sfdfile[], char fname[], char stream[],
	char req_begin[], char req_end[], char act_begin[], char act_end[],
	STATUS *status )

/* Copies SEED stream to SEED file
 *
 * parameters of routine
 * int        chan;           input; channel to be used
 * char       sfdfile[];      input; name of sfd-file for input data
 * char       fname[];        input; name of output file
 * char       stream[];       input; stream string
 * char       req_begin[];    input; requested start time
 * char       req_end[];      input; requested end time
 * char       act_begin[];    output; actual start time
 * char       act_end[];      output; actual end time
 * char       *status;        output; return status
 */
{
	/* local variables */
	NTIME    rec_start;        /* record start time */
	NTIME    rec_end;          /* record end time */
	NTIME    n_end;            /* request end time */
	INT32    *smp;             /* pointer to sample array */
	INT32    idx;              /* sample index */
	INT32    *buf;             /* one record buffer */
	INT32    buflth;           /* length of buffer */
	SeedSbyteT *encode;        /* encode buffer */
	int      encode_lth;       /* length of encode buffer in records */
	float    tdiff;            /* time difference */
	float    dt;               /* sample distance in sec */
	int      out;              /* UNIX file descriptor of output file */
	float    rec_span;         /* time span of record */
	int      i;                /* counter */
	BOOLEAN  quit;             /* quit loop */
	INT32    smp_written;      /* number of samples written */
	NTIME    ntime;            /* scratch time */
	BOOLEAN  hdr_swapped;      /* header was swapped */

	/* executable code */

	/* allocate memory */
	smp = (INT32 *)sy_allocmem( seedv_setup.copy_buf_lth, (int)sizeof(INT32),
		status );
	if  (Severe(status))  return;
	buf = (INT32 *)sy_allocmem( BUFLTH, (int)sizeof(INT32), status );
	if  (Severe(status))  {
		sy_deallocmem( smp );
		return;
	} /*endif*/
	/* encode buffer approximately of equal size as sample buffer 'smp'. */
	/* In the worst case this is not enough, but it is very unlikely.    */
	encode_lth = (int)(seedv_setup.copy_buf_lth*(int)sizeof(INT32)
		/ seedv_out_reclth + 1);
	encode = (SeedSbyteT *)sy_allocmem( encode_lth, seedv_out_reclth,
		status );
	if  (Severe(status))  {
		sy_deallocmem( smp );
		sy_deallocmem( buf );
		return;
	} /*endif*/

	/* open output file */
	out = open( fname, O_WRONLY|O_CREAT|O_TRUNC, 0x1ff );
	if  (out < 0)  {
		*status = SeedERR_OPENOUTPUT;
		err_setcontext( " ## file " );
		err_setcontext( fname );
		sy_deallocmem( smp );
		sy_deallocmem( buf );
		sy_deallocmem( encode );
	} /*endif*/

	/* position input file to requested time */
	SeedSearchPosition( chan, sfdfile, stream, req_begin, act_begin, status );
	if  (Severe(status))  {FREE_AND_RETURN;}
	/* check if there are any data in this time window */
	if  (tc_tdiff(act_begin,req_end,status) >= 0.0)  {
		*status = SeedERR_TIMEGAP;
		err_setcontext( " ## no data in requested time window" );
		FREE_AND_RETURN;
	} /*endif*/
	/* initialize values for loop */
	tc_t2n( act_begin, &rec_end, status );
	if  (Severe(status))  {FREE_AND_RETURN;}
	tc_t2n( req_end, &n_end, status );
	if  (Severe(status))  {FREE_AND_RETURN;}
	SeedNtimeToBtime( &rec_end, &seedv_hdr_proto.starttime, status );
	if  (Severe(status))  {FREE_AND_RETURN;}
	/* !!! set record number !!! */
	/* !!! apply time correction !!! */
	
	idx = 0;
	quit = FALSE;

	do  {

		/* decode next record */
		SeedReadNextRecord( chan, (SeedSbyteT *)seedv_rec, &hdr_swapped, status );
		if  (Severe(status))  {FREE_AND_RETURN;}
		SeedBtimeToNtime( &(seedv_rec->starttime), &rec_start, status );
		if  (Severe(status))  {FREE_AND_RETURN;}
		SeedDecodeSteim1( (SeedSbyteT *)seedv_rec, !hdr_swapped,
			BUFLTH, buf, &buflth );
		if  (buflth != (INT32)(seedv_rec->no_of_samples))  {
			fprintf( stderr, "*** inconsistency in sample length: should be %ld, is %ld\n",
				(INT32)(seedv_rec->no_of_samples), buflth );
		} /*endif*/
		dt = SeedGetSampleDist( seedv_rec );
		/* if decoded values do not fit into smp-array, write it out */
		if  (idx+buflth > seedv_setup.copy_buf_lth)  {
			SeedBtimeToNtime( &seedv_hdr_proto.starttime, &ntime, status );
			if  (Severe(status))  {FREE_AND_RETURN;}
			smp_written = idx;
			SeedEncodeSteim1( out, smp, 0, &idx, &ntime, encode, encode_lth,
				FALSE, status );
			if  (Severe(status))  {FREE_AND_RETURN;}
			smp_written -= idx;
			/* tc_nadd( &ntime, (float)smp_written*dt, &ntime, status ); */
			/* if  (Severe(status))  {FREE_AND_RETURN;} */
			/* SeedNtimeToBtime( &ntime, &seedv_hdr_proto.starttime, status ); */
			/* if  (Severe(status))  {FREE_AND_RETURN;} */
		} /*endif*/
		/* now check for time gap */
		rec_span = dt * (float)(seedv_rec->no_of_samples);
		tdiff = tc_ndiff( &rec_start, &rec_end, status );
		if  (Severe(status))  {FREE_AND_RETURN;}
		/* if there is a time gap, flush smp-buffer and start new record */
		if  (Abs(tdiff) > dt)  {
			printf( "! time gap\n" );
			SeedEncodeSteim1( out, smp, 0, &idx, &ntime, encode, encode_lth, TRUE,
				status );
			if  (Severe(status))  {FREE_AND_RETURN;}
			seedv_hdr_proto.starttime = seedv_rec->starttime;
		} /*endif*/
		/* compute end time for this record, used in next loop */
		tc_nadd( &rec_start, rec_span, &rec_end, status );
		if  (Severe(status))  {FREE_AND_RETURN;}
		/* check whether requested end time is reached */
		tdiff = tc_ndiff( &rec_end, &n_end, status );
		if  (tdiff >= 0.0)  {
			/* if end time reached decrease buflth ... */
			tdiff = tc_ndiff( &n_end, &rec_start, status );
			if  (Severe(status))  {FREE_AND_RETURN;}
			buflth = Nint( tdiff / dt );
			/* ... and compute actual end time */
			tc_nadd( &rec_start, dt*(float)buflth, &n_end, status );
			if  (Severe(status))  {FREE_AND_RETURN;}
			tc_n2t( &n_end, act_end, status );
			if  (Severe(status))  {FREE_AND_RETURN;}
			quit = TRUE;
		} /*endif*/
		/* now add decoded values to smp-array */
		for  (i=0; i<buflth; i++)
			smp[idx++] = buf[i];
		/* if end time found, flush buffer and exit loop */
		if  (quit)
			SeedEncodeSteim1( out, smp, 0, &idx, &ntime, encode, encode_lth, TRUE,
				status );

	}  while (!quit);

	FREE_AND_RETURN;

} /* end of SeedCopy */


#undef FREE_AND_RETURN

/* #endif */

/*---------------------------------------------------------------------*/



#define DO_ABORT \
	*status = SeedERR_SFD_READ; \
	err_setcontext( " ## string " ); err_setcontext( line-1 ); \
	return;




void SeedParseSfdLine( char line[], SeedFileDescrT *fd, STATUS *status )

/* parses line of SFD file.  Returns structure elements 'stream',
 * 't_start', 't_end', 'name', 'recno' and 'calib'.
 *
 * parameters of routine
 * char       line[];       input; SFD line
 * SeedFileDescrT *fd;      output; SEED file descriptor
 * STATUS     *status;      output; return status
 */
{
	/* local variables */

	/* executable code */

	fd->t_start[0] = '\0';
	fd->t_end[0] = '\0';
	fd->name[0] = '\0';
	fd->stream[0] = '\0';
	fd->recno = 0;
	fd->reclth = DEFAULT_RECLTH;
	fd->byteoff = 0;
	fd->calib = 0.0;
	fd->dataflags = 0;
	fd->swap_hdr = FALSE;
	fd->pri = 0;
	fd->dataformat = Seed_C_FORMAT_MSEED;
	strcpy( fd->addinf, Seed_C_EmptyAddinfStr );

	FOREVER  {

		if  (*line == '>')  line++;
		line = strchr( line, '>' );
		if  (line == NULL)  break;
		switch  (*(line-1))  {
		case Seed_C_SfdTStart:
			if  (sscanf(line+1,"%s",fd->t_start) != 1)  { DO_ABORT }
			break;
		case Seed_C_SfdTEnd:
			if  (sscanf(line+1,"%s",fd->t_end) != 1)  { DO_ABORT }
			break;
		case Seed_C_SfdName:
			if  (sscanf(line+1,"%s",fd->name) != 1)  { DO_ABORT }
			break;
		case Seed_C_SfdStream:
			if  (sscanf(line+1,"%s",fd->stream) != 1)  { DO_ABORT }
			break;
		case Seed_C_SfdRecno:
			if  (sscanf(line+1,"%d",&(fd->recno)) != 1)  { DO_ABORT }
			break;
		case Seed_C_SfdReclth:
			if  (sscanf(line+1,"%d",&(fd->reclth)) != 1)  { DO_ABORT }
			break;
		case Seed_C_SfdOffset:
			if  (sscanf(line+1,"%d",&(fd->byteoff)) != 1)  { DO_ABORT }
			break;
		case Seed_C_SfdCalib:
			if  (sscanf(line+1,"%f",&(fd->calib)) != 1)  { DO_ABORT }
			break;
		case Seed_C_SfdSwapH:
			if  (sscanf(line+1,"%d",&(fd->swap_hdr)) != 1)  { DO_ABORT }
			if  (seedv_invert_swap_hdr)  fd->swap_hdr = ! fd->swap_hdr;
			break;
		case Seed_C_SfdAddinf:
			strncpy( fd->addinf, line+1, Seed_C_AddinfLth);
			fd->addinf[Seed_C_AddinfLth] = '\0';
			ut_uncap( fd->addinf );
			break;
		} /*endswitch*/

	} /*endfor*/

} /* end of SeedParseSfdLine */



#undef DO_ABORT



/*---------------------------------------------------------------------*/



void SeedReadStream( int chan, char sfdfile[], char stream_str[],
	BOOLEAN swap, char req_start[], float seclth, INT32 **ldat,
	INT32 *smplth, char act_start[], float *dt, float *calib,
	int *flags, STATUS *status )

/* Reads sample data from SEED stream.  The memory for the sample
 * data is allocated by the routine.  The channel number is chosen
 * by the caller of the routine.  It is legal to use always channel
 * number 0 but it is faster to use a separate channel number for
 * each stream used if the streams are read repeatedly.
 *
 * parameters of routine
 * int        chan;         input; channel number (0..Seed_C_MAXCHAN-1)
 * char       sfdfile[];    input; name of sfd file to use
 * char       stream_str[]; input; stream string (like "bfo-vbb-z")
 * BOOLEAN    swap;         input; swap bytes on file
 * char       req_start[];  input; requested start time
 * float      seclth;       input; number of seconds to be read
 * INT32      **ldat;       output; sample array read
 * INT32      *smplth;      output; number of samples read
 * char       act_start[];  output; actual start time
 * float      *dt;          output; sample distance in sec
 * float      *calib;       output; calibration constant
 * int        *flags;       output; data flags found
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	SeedFileDescrT *fd;   /* pointer to SEED file descriptor */
	INT32    getlth;      /* number of samples to read */
	INT32    maxrecsmp;   /* maximum number of samples per record */
	INT32    actrecsmp;   /* actual number of samples in record */
	INT32    *recsmp;     /* pointer to samples of record */
	INT32    *lsrc, *ldst;/* moving pointers */
	NTIME    recstart;    /* record start time */
	NTIME    recend;      /* record end time */
	NTIME    expectime;   /* expected time */
	float    tdiff;       /* time gap in sec */
	INT32    padcnt;      /* counter for zero padding */
	char     str[BC_TIMELTH+1];  /* scratch string for time output */
	BOOLEAN  swap_data;   /* swap data */
	int      i;           /* counter */
	char     recmsg[cBcLineLth+1];  /* output string */

	/* executable code */

	/* check channel number */
	if  (chan < 0 || chan >= Seed_C_MAXCHAN)  {
		*status = SeedERR_CHAN_OOR;
		return;
	} /*endif*/
	fd = seedv_fd + chan;

	/* find requested position */
	tdiff = 0.0;
	SeedSearchPosition( chan, sfdfile, stream_str, req_start,
		act_start, status );
	if  (*status == SeedERR_NEXTTIMEPOS)  {
		*status = BC_NOERROR;
		tdiff = tc_tdiff( act_start, req_start, status );
		if  (Severe(status))  return;
		if  (tdiff >= seclth)  {*status = SeedERR_DATA_UNAVAIL; return;}
		fprintf( stderr,
			"SeedReadStream: time %s not found, start at %s\n",
			req_start, act_start );
	} /*endif*/
	if  (Severe(status))  return;

	maxrecsmp = MAX_SMP_PER_REC;
	*smplth = 0;
	*ldat = NULL;
	recsmp = NULL;
	*dt = 0.0;
	expectime.year = 0;
	/* reset decode routine */
	SeedDecodeSteim1( NULL, swap, 0, NULL, NULL );

	FOREVER  {

		/* read next record */
		SeedReadNextRecord( chan, (SeedSbyteT *)seedv_rec, &swap_data, status );
		if  (swap)  swap_data = !swap_data;
		if  (*status == SeedERR_NEXTTIMEPOS)  *status = BC_NOERROR;
		if  (Severe(status))  return;

		/* decode SEED record and select samples */
		/* ------------------------------------- */
		if  (*ldat == NULL)  {

			/* this is for first loop only */
			/* --------------------------- */
			*calib = fd->calib;
			*flags = fd->dataflags;
			if  (seedv_rec->quality != 0)  {
				if  (GpGetInt(cGpI_debug_level > 0))
					printf( "SHM-dbg1: SEED quality flags: 0x%x\n",
						(int)seedv_rec->quality );
				(*flags) |= seedv_rec->quality;
			} /*endif*/
			*dt = SeedGetSampleDist( seedv_rec );
			getlth = Nint32( seclth / *dt );
			if  (getlth <= 0)  {*status = SeedERR_ZEROLTH; return;}
			*ldat = (INT32 *)sy_allocmem( getlth, (int)sizeof(INT32), status );
			if  (Severe(status))  return;
			ldst = *ldat;
			recsmp = (INT32 *)sy_allocmem( maxrecsmp, (int)sizeof(INT32), status );
			if  (Severe(status))  {sy_deallocmem(*ldat); return;}
			/* if time gap at beginning, pad zeroes */
			if  (tdiff > 0.0)  {
				/* pad with zeroes and adjust start time */
				padcnt = Nint32( tdiff / (*dt) );
				fprintf( stderr, "SeedReadStream: record %6s, time %s\n",
					seedv_rec->seqno, act_start );
				fprintf( stderr, "   gap %5.3f sec, start with %ld zeroes\n",
					tdiff, padcnt );
				tc_tadd( act_start, -(*dt)*(float)padcnt, act_start, status );
				if  (Severe(status))  *status = BC_NOERROR; /* ok, not elegant */
				while  (padcnt-- > 0)  {
					ldst[(*smplth)++] = 0;
					if  (*smplth == getlth)  {
						sy_deallocmem( recsmp );
						fprintf( stderr,
							"   padding aborted, %ld zeroes remaining\n", padcnt );
						strcpy( act_start, req_start );
						return;
					} /*endif*/
				} /*endwhile*/
			} /*endif*/
			SeedDecodeSteim1( (SeedSbyteT *)seedv_rec, swap_data,
				maxrecsmp, recsmp, &actrecsmp );
			if  (actrecsmp != seedv_rec->no_of_samples)  {
				fprintf( stderr,
					"*** seed record samples: expected %d, actual: %d\n",
					seedv_rec->no_of_samples, actrecsmp );
				if  (seedv_decerr_abort)  {
					sy_deallocmem( *ldat );
					*ldat = NULL;
					sy_deallocmem( recsmp );
					*status = SeedERR_DECODE_ERR;
					return;
				} else {
					INT32    zeronum;       /* number of sample to zero */
					zeronum = seedv_rec->no_of_samples;
					if  (zeronum > maxrecsmp)  zeronum = maxrecsmp;
					fprintf( stderr, "   zero %d samples and continue\n", zeronum );
					for  (i=0; i<zeronum; i++)
						recsmp[i] = 0;
				} /*endif*/
#				ifdef XXX
				/* excluded this on 1-Jul-98, K.S. */
				fd->sample = 0;
				fprintf( stderr,
					"   read position within zeroed record set to zero\n" );
#				endif
				actrecsmp = seedv_rec->no_of_samples;
			} /*endif*/
			lsrc = recsmp + fd->sample;
			actrecsmp -= fd->sample;
			if  (actrecsmp < 0)  {
				sy_deallocmem( *ldat );
				*ldat = NULL;
				sy_deallocmem( recsmp );
				*status = SeedERR_BUG;
				fprintf( stderr, "*** SeedReadStream: bug (1) [sample: %d] ***\n",
					fd->sample );
				return;
			} /*endif*/
			SeedRecordTime( (SeedSbyteT *)seedv_rec, &recstart, &expectime,
				status );
			if  (Severe(status))  return;

		} else {

			/* this is for all other loops */
			/* --------------------------- */

			/* collect data quality flags */
			if  (seedv_rec->quality != 0)  {
				if  (GpGetInt(cGpI_debug_level > 0))
					printf( "SHM-dbg1: SEED quality flags: %x\n",
						(int)seedv_rec->quality );
				(*flags) |= seedv_rec->quality;
			} /*endif*/

			/* decode record */
			SeedDecodeSteim1( (SeedSbyteT *)seedv_rec, swap_data,
				maxrecsmp, recsmp, &actrecsmp );
			if  (actrecsmp != seedv_rec->no_of_samples)  {
				fprintf( stderr,
					"*** seed record samples: expected %d, actual: %d\n",
					seedv_rec->no_of_samples, actrecsmp );
				if  (seedv_decerr_abort)  {
					sy_deallocmem( *ldat );
					*ldat = NULL;
					sy_deallocmem( recsmp );
					*status = SeedERR_DECODE_ERR;
					return;
				} else {
					INT32    zeronum;       /* number of sample to zero */
					zeronum = seedv_rec->no_of_samples;
					if  (zeronum > maxrecsmp)  zeronum = maxrecsmp;
					fprintf( stderr, "   zero %d samples and continue\n", zeronum );
					for  (i=0; i<zeronum; i++)
						recsmp[i] = 0;
				} /*endif*/
				actrecsmp = seedv_rec->no_of_samples;
			} /*endif*/
			lsrc = recsmp;

			/* check for time gaps, pad with zeroes */
			SeedRecordTime( (SeedSbyteT *)seedv_rec, &recstart, &recend, status );
			if  (Severe(status))  return;
			tdiff = tc_ndiff( &recstart, &expectime, status );
			if  (Severe(status))  return;
			if  (tdiff <= -(*dt))  {
				/* double data */
				padcnt = Nint32( -tdiff / (*dt) );
				tc_n2t( &recstart, str, status );
				strncpy( recmsg, seedv_rec->seqno, 6 );
				recmsg[6] = '\0';
				fprintf( stderr, "SeedReadStream: record %6s, time %s\n",
					recmsg, str );
				fprintf( stderr, "   double data, %ld samples skipped\n",
					padcnt );
				if  (padcnt > (INT32)(seedv_rec->no_of_samples))  {
					*status = SeedERR_DOUBLEDATA;
					sy_deallocmem( recsmp );
					return;
				} /*endif*/
				/* skip samples */
				lsrc += padcnt;
				actrecsmp -= padcnt;
			} else if  (tdiff >= (*dt))  {
				/* time gap */
				padcnt = Nint32( tdiff / (*dt) );
				tc_n2t( &recstart, str, status );
				strncpy( recmsg, seedv_rec->seqno, 6 );
				recmsg[6] = '\0';
				fprintf( stderr, "SeedReadStream: record %6s, time %s\n",
					recmsg, str );
				fprintf( stderr,
					"   gap %5.3f sec, padding %ld zeroes at sample %ld\n",
					tdiff, padcnt, *smplth );
				/* pad zeroes */
				while  (padcnt-- > 0)  {
					ldst[(*smplth)++] = 0;
					if  (*smplth == getlth)  {
						sy_deallocmem( recsmp );
						fprintf( stderr, 
							"   padding aborted, %ld zeroes remaining\n", padcnt );
						return;
					} /*endif*/
				} /*endwhile*/
			} /*endif*/
			expectime = recend;

		} /*endif*/

		/* copy samples to output array */
		while  (actrecsmp-- > 0)  {
			ldst[(*smplth)++] = *lsrc++;
			if  (*smplth == getlth)  {  /* finished ! */
				sy_deallocmem( recsmp );
				return;
			} /*endif*/
		} /*endwhile*/

	} /*endfor*/

} /* end of SeedReadStream */



/*---------------------------------------------------------------------*/



void SeedReadStreamReal( int chan, char sfdfile[], char stream_str[],
	BOOLEAN swap, char req_start[], float seclth, float **fdat,
	INT32 *smplth, char act_start[], float *dt, float *calib,
	int *flags, STATUS *status )

/* Same as SeedReadStream except that a float array is returned
 *
 * parameters of routine
 * see SeedReadStream above
 */
{
	/* local variables */
	INT32    *lptr;         /* INT32 pointer to data array */
	INT32    *lp;           /* moving pointer */
	float    *fp;           /* moving pointer */
	INT32    i;             /* counter */

	/* executable code */

	*fdat = NULL;

	SeedReadStream( chan, sfdfile, stream_str, swap, req_start,
		seclth, &lptr, smplth, act_start, dt, calib, flags, status );
	if  (Severe(status))  return;

	if  (sizeof(INT32) != sizeof(float))  {
		printf( "--> sizeof(float) != sizeof(INT32)\n" );
		*status = 1;
		sy_deallocmem( lptr );
		return;
	} /*endif*/

	lp = lptr;
	fp = *fdat = (float *)lptr;
	for  (i=0; i<(*smplth); i++)
		*fp++ = (float)(*lp++) * (*calib);

} /* end of SeedReadStreamReal */



/*---------------------------------------------------------------------*/



float SeedFindStreamCalibration( char stream[], char stime[], STATUS *status )

/* returns calibration value for counts for given stream 'stream' at time
 * 'stime'.
 *
 * parameters of routine
 * char           stream[];     input; stream string
 * char           stime[];      input; time
 * STATUS         *status;      output; return status
 *                              returns calibration or 0.0
 */
{
	/* local variables */
	char     calfile[BC_FILELTH+1];    /* name of calibration file */
	char     *eptr;                    /* pointer to environment variable */
	FILE     *fp;                      /* pointer to input file */
	char     line[BC_LINELTH+1];       /* current line of input file */
	char     v_start[BC_LINELTH+1];    /* start of valid time */
	char     v_end[BC_LINELTH+1];      /* end of valid time */
	float    lcalib;                   /* current value of calibration */
	int      cmpidx;                   /* position of component */
	int      filecnt;                  /* file counter */
	char     *sf;                      /* pointer to sensitivity file name */

	/* executable code */

	if  (strcmp(GpGetStringElem(cGpL_sensitivity_file,0),"old-style") == 0)  {

		/* this is the old-style part, dealing with many, many
		 * seedcalib_... files in $SEED_INPUTS
		 */

		/* build filename */
		*calfile = '\0';
		eptr = getenv( "SEED_INPUTS" );
		if  (eptr != NULL)  {
			if  (strlen(eptr)+strlen(stream)+11 > BC_FILELTH)  {
				*status = SeedERR_STROVFL;
				return 0.0;
			} /*endif*/
			strcpy( calfile, eptr );
			strcat( calfile, "/" );
		} /*endif*/
		strcat( calfile, "seedcalib_" );
		strcat( calfile, stream );

		/* open file */
		fp = sy_fopen( calfile, "r" );
		if  (fp == NULL)  {
			*status = SeedERR_NOCALIB;
			fprintf( stderr, "*Seed: no calibration for %s, set to 1.0\n", stream );
			return 1.0;
		} /*endif*/

		while  (fgets(line,BC_LINELTH,fp) != NULL)  {
			if  (*line == '!' || *line == '\n')  continue;
			if  (sscanf(line,"%s %s %f",v_start,v_end,&lcalib) != 3)  {
				fprintf( stderr, "*Seed: format error in calibration file %s\n", stream );
				continue;
			} /*endif*/
			if  (tc_tdiff(stime,v_start,status) < 0.0)  continue;
			if  (Severe(status))  return 0.0;
			if  (tc_tdiff(stime,v_end,status) > 0.0)  continue;
			if  (Severe(status))  return 0.0;
			sy_fclose( fp );
			return lcalib;
		} /*endwhile*/

		sy_fclose( fp );
		*status = SeedERR_NOCALIB;
		fprintf( stderr, "*Seed: no valid calibration for %s %s, set to 1.0\n",
			stream, stime );
		return 1.0;

	} else {

		/* this is the new version introduced 20-May-2006, all information
		 * is in a single file, by default in $SH_INPUTS/sensitivities.txt
		 */

		/* loop all sensitivity files given */
		for  (filecnt=0;;filecnt++)  {

			/* get next sensitivity file name */
			sf = GpGetStringElem( cGpL_sensitivity_file, filecnt );
			if  (sf == NULL)  break;

			/* check for 'default' value, happens if GpReadParfile is not called */
			if  (strcmp(sf,"default") == 0)  {
				*calfile = '\0';
				eptr = getenv( "SH_INPUTS" );
				if  (eptr != NULL)  {
					if  (strlen(eptr)+18 > BC_FILELTH)  {
						*status = SeedERR_STROVFL;
						return 0.0;
					} /*endif*/
					strcpy( calfile, eptr );
					strcat( calfile, "/" );
				} /*endif*/
				strcat( calfile, "sensitivities.txt" );
				sf = calfile;
			} /*endif*/

			/* open it */
			if  (GpGetInt(cGpI_debug_level) > 5)
				printf( "SH-dbg6: open sensitivity file %s\n", sf );
			fp = sy_fopen( sf, "r" );
			if  (fp == NULL)  {
				fprintf( stderr, "*Seed: sensitivity file %s not found\n", sf );
				*status = SeedERR_NOCALIB;
				return 1.0;
			} /*endif*/

			/* wildcard takes stream with '?' as component */
			cmpidx = strlen( stream ) - 1;

			/* read through file */
			while  (fgets(line,cBcLineLth,fp) != NULL)  {
				if  (*line == '!' || *line == '#' || *line == '\n')  continue;
				if  (strncasecmp(line,stream,cmpidx) == 0)  {
					if  (line[cmpidx] == '?' || line[cmpidx] == stream[cmpidx])  {
						if  (sscanf(line+cmpidx+2,"%s %s %f",v_start,v_end,&lcalib) != 3)  {
							fprintf( stderr, "*Seed: format error in calibration file at %s\n",
								stream );
							continue;
						} /*endif*/
						if  (strcmp(v_start,"...") != 0)
							if  (tc_tdiff(stime,v_start,status) < 0.0)  continue;
						if  (Severe(status))  {
							sy_fclose( fp );
							return 0.0;
						} /*endif*/
						if  (strcmp(v_end,"...") != 0)
							if  (tc_tdiff(stime,v_end,status) > 0.0)  continue;
						if  (Severe(status))  {
							sy_fclose( fp );
							return 0.0;
						} /*endif*/
						sy_fclose( fp );
						return lcalib;
					} /*endif*/
				} /*endif*/
			} /*endif*/

			sy_fclose( fp );

		} /*endfor*/

		*status = SeedERR_NOCALIB;
		fprintf( stderr, "*Seed: no valid calibration for %s %s, set to 1.0\n",
			stream, stime );
		return 1.0;

	} /*endif*/

} /* end of SeedFindStreamCalibration */



/*---------------------------------------------------------------------*/



void SeedGetTimeSpan( char sfdfile[], char stream[], char start[],
	char end[], STATUS *status )

/* Returns first and last time of channel 'stream' in sfd-file
 *
 * parameters of routine
 * char       sfdfile[];        input; name of sfd-file
 * char       stream[];         input; stream string to be checked
 * char       start[];          output; first start time
 * char       end[];            output; last end time
 * STATUS     *status;          output; return status
 */
{
	/* local variables */
	FILE     *sfd;                      /* pointer to sfd-file */
	char     line[Seed_C_SFDLINELTH+1]; /* current sfd-line */
	SeedFileDescrT dsc;                 /* sfd descriptor */
	char     lstream[BC_SHORTSTRLTH+1]; /* lowercase stream name */

	/* executable code */

	sfd = sy_fopen( sfdfile, "r" );
	if  (sfd == NULL)  {
		*status = SeedERR_OPENINPUT;
		err_setcontext( " ## file " );
		err_setcontext( sfdfile );
		return;
	} /*endif*/

	strcpy( lstream, stream );
	ut_uncap( lstream );

	/* find requested stream */
	*start = *end = '\0';
	while  (fgets(line,Seed_C_SFDLINELTH,sfd) != NULL)  {
		if  (*line == '!')  continue;
		SeedParseSfdLine( line, &dsc, status );
		if  (strcmp(dsc.stream,lstream) != 0)  continue;
		if  (*start == '\0')  {
			strcpy( start, dsc.t_start );
		} else {
			if  (tc_tdiff(dsc.t_start,start,status) < 0.0)
				strcpy( start, dsc.t_start );
		} /*endif*/
		if  (Severe(status))  return;
		if  (*end == '\0')  {
			strcpy( end, dsc.t_end );
		} else {
			if  (tc_tdiff(dsc.t_end,end,status) > 0.0)
				strcpy( end, dsc.t_end );
		} /*endif*/
		if  (Severe(status))  return;
	} /*endwhile*/

	sy_fclose( sfd );

} /* end of SeedGetTimeSpan */



/*---------------------------------------------------------------------*/



BOOLEAN SeedInquireTime( char sfdfile[], char stream[], char start[],
	char end[], STATUS *status )

/* Checks whether the specified time span from 'start' to 'end' is
 * available for channel 'stream' in sfd-file 'sfdfile'
 *
 * parameters of routine
 * char       sfdfile[];        input; name of sfd-file
 * char       stream[];         input; stream string
 * char       start[];          input; start time
 * char       end[];            input; end time
 * STATUS     *status;          output; return status
 *                              returns TRUE if data are available
 */
{
	/* local variables */
	FILE     *sfd;                      /* pointer to sfd-file */
	char     line[Seed_C_SFDLINELTH+1]; /* current sfd-line */
	SeedFileDescrT dsc;                 /* sfd descriptor */
	char     lstream[BC_SHORTSTRLTH+1]; /* lowercase stream name */
	TIME     a_start, a_end;            /* absolute times of inquied span */
	TIME     a1, a2;                    /* absolute times of SEED file */

	/* executable code */

	sfd = sy_fopen( sfdfile, "r" );
	if  (sfd == NULL)  {
		*status = SeedERR_OPENINPUT;
		err_setcontext( " ## file " );
		err_setcontext( sfdfile );
		return FALSE;
	} /*endif*/

	strcpy( lstream, stream );
	ut_uncap( lstream );

	tc_t2a( start, &a_start, status );
	if  (Severe(status))  {sy_fclose(sfd); return FALSE;}
	tc_t2a( end, &a_end, status );
	if  (Severe(status))  {sy_fclose(sfd); return FALSE;}
	if  (tc_adiff(&a_end,&a_start) < 0.0)  {sy_fclose(sfd); return FALSE;}

	while  (fgets(line,Seed_C_SFDLINELTH,sfd) != NULL)  {
		if  (*line == '!')  continue;
		SeedParseSfdLine( line, &dsc, status );
		if  (strcmp(dsc.stream,lstream) != 0)  continue;
		tc_t2a( dsc.t_start, &a1, status );
		if  (Severe(status))  {sy_fclose(sfd); return FALSE;}
		tc_t2a( dsc.t_end, &a2, status );
		if  (Severe(status))  {sy_fclose(sfd); return FALSE;}
		if  (tc_adiff(&a_start,&a1) < 0.0)  continue;
		if  (tc_adiff(&a2,&a_end) < 0.0)  continue;
		sy_fclose( sfd );
		return TRUE;
	} /*endwhile*/

	sy_fclose( sfd );

	return FALSE;

} /* end of SeedInquireTime */



/*---------------------------------------------------------------------*/



void SeedInquireTimeList( char sfdfile[], char stations[], int itemlth,
	int listlth, char chan[], char comp, char start[], char end[],
	INT32 *avail, STATUS *status )

/* Checks whether the specified time span from 'start' to 'end' is
 * available for all stations (stream 'station-chan-comp') in sfd-file
 * 'sfdfile'.  'station' is a char string with 'listlth' equidistant
 * strings of length 'itemlth'.
 *
 * parameters of routine
 * char       sfdfile[];        input; name of sfd-file
 * char       stations[];       input; list of stations
 * int        itemlth;          input; chars used for a station name
 * int        listlth;          input; length of list (# of stations) (<= 32)
 * char       chan[];           input; 2-char channel string
 * char       comp;             input; component
 * char       start[];          input; start time
 * char       end[];            input; end time
 * INT32      *avail;           output; station bits
 * STATUS     *status;          output; return status
 */
{
	/* local variables */
	FILE     *sfd;                      /* pointer to sfd-file */
	char     line[Seed_C_SFDLINELTH+1]; /* current sfd-line */
	SeedFileDescrT dsc;                 /* sfd descriptor */
	char     lstream[BC_SHORTSTRLTH+1]; /* lowercase stream name */
	TIME     a_start, a_end;            /* absolute times of inquied span */
	TIME     a1, a2;                    /* absolute times of SEED file */
	char     *sptr;                     /* current station name */
	int      scnt;                      /* station counter */
	char     cmpstr[2];                 /* component */

	/* executable code */

	sfd = sy_fopen( sfdfile, "r" );
	if  (sfd == NULL)  {
		*status = SeedERR_OPENINPUT;
		err_setcontext( " ## file " );
		err_setcontext( sfdfile );
		return;
	} /*endif*/

	tc_t2a( start, &a_start, status );
	if  (Severe(status))  {sy_fclose(sfd); return;}
	tc_t2a( end, &a_end, status );
	if  (Severe(status))  {sy_fclose(sfd); return;}
	if  (tc_adiff(&a_end,&a_start) < 0.0)  {sy_fclose(sfd); return;}

	cmpstr[0] = comp;
	cmpstr[1] = '\0';

	*avail = 0;

	while  (fgets(line,Seed_C_SFDLINELTH,sfd) != NULL)  {

		/* get & parse next sfd line */
		if  (*line == '!')  continue;
		SeedParseSfdLine( line, &dsc, status );

		/* check time */
		tc_t2a( dsc.t_start, &a1, status );
		if  (Severe(status))  {sy_fclose(sfd); return;}
		tc_t2a( dsc.t_end, &a2, status );
		if  (Severe(status))  {sy_fclose(sfd); return;}
		if  (tc_adiff(&a_start,&a1) < 0.0)  continue;
		if  (tc_adiff(&a2,&a_end) < 0.0)  continue;

		/* now loop all stations */
		sptr = stations;
		for  (scnt=0; scnt<listlth; scnt++)  {
			if  (*sptr != '-')  {
				/* build stream name */
				strcpy( lstream, sptr );
					strcat( lstream, "-" );
				strcat( lstream, chan );
				strcat( lstream, "-" );
				strcat( lstream, cmpstr );
				ut_uncap( lstream );
				if  (strcmp(dsc.stream,lstream) == 0)
					*avail |= (1<<scnt);
			} /*endif*/
			/* increment station pointer */
			sptr += itemlth;
		} /*endfor*/

	} /*endwhile*/

	sy_fclose( sfd );

} /* end of SeedInquireTimeList */



/*---------------------------------------------------------------------*/



BOOLEAN SeedSwapNecessary( SeedDataHeaderT *hdr )

/* Checks whether a swap is necessary on data header
 *
 * parameters of routine
 * SeedDataHeaderT *hdr;     input; header to check
 *                           returns TRUE if swap must be performed
 */
{
	/* executable code */

	return  (hdr->starttime.year < 1950 || hdr->starttime.year > 2030);

} /* end of SeedSwapNecessary */



/*---------------------------------------------------------------------*/



void SeedSwapHeader( SeedDataHeaderT *hdr )

/* Swaps words and longwords in header
 *
 * parameters of routine
 * SeedDataHeaderT *hdr;          modify; data header to be swapped
 */
{
	/* local variables */
	WORD     v_word;      /* scratch */
	UWORD    v_uword;     /* scratch */
	INT32    v_long;      /* scratch */
	SeedBlockette1000T *b1000;  /* pointer to Blockette 1000 */
	UWORD    next_block;   /* next blockette pointer */

	/* executable code */

	v_uword = hdr->starttime.year;
	hdr->starttime.year = (v_uword & 0xff) * 0x100;
	hdr->starttime.year += (UWORD)(v_uword & 0xff00) / (UWORD)0x100;

	v_uword = hdr->starttime.day;
	hdr->starttime.day = (v_uword & 0xff) * 0x100;
	hdr->starttime.day += (UWORD)(v_uword & 0xff00) / (UWORD)0x100;

	v_uword = hdr->starttime.frac_secs;
	hdr->starttime.frac_secs = (v_uword & 0xff) * 0x100;
	hdr->starttime.frac_secs += (UWORD)(v_uword & 0xff00) / (UWORD)0x100;

	v_uword = hdr->no_of_samples;
	hdr->no_of_samples = (v_uword & 0xff) * 0x100;
	hdr->no_of_samples += (UWORD)(v_uword & 0xff00) / (UWORD)0x100;

	v_uword = (UWORD)(hdr->smprate_fact);
	hdr->smprate_fact = (int)( ((v_uword << 8) & 0xff00) |
		((v_uword >> 8) & 0x00ff) );

	v_uword = (UWORD)(hdr->smprate_mult);
	hdr->smprate_mult = (int)( ((v_uword << 8) & 0xff00) |
		((v_uword >> 8) & 0x00ff) );

	v_long = hdr->timecorr;
	hdr->timecorr =  (INT32)(((UINT32)v_long & 0x000000ffL) * (UINT32)0x1000000L);
	hdr->timecorr += (INT32)(((UINT32)v_long & 0x0000ff00L) * (UINT32)0x100L);
	hdr->timecorr += (INT32)(((UINT32)v_long & 0x00ff0000L) >> (UINT32)8L);
	hdr->timecorr += (INT32)(((UINT32)v_long & 0xff000000L) >> (UINT32)24L);

	v_uword = hdr->databegin;
	hdr->databegin = (v_uword & 0xff) * 0x100;
	hdr->databegin += (UWORD)(v_uword & 0xff00) / (UWORD)0x100;

	/* swap pointer to first blockette and store unswapped value */
	next_block = 0;
	v_uword = hdr->first;
	if  (v_uword >= 48 && v_uword < 64)  next_block = v_uword;
	hdr->first = (v_uword & 0xff) * 0x100;
	hdr->first += (UWORD)(v_uword & 0xff00) / (UWORD)0x100;
	if  (hdr->first >= 48 && hdr->first < 64)  next_block = hdr->first;

	/* if a pointer > 0 was found swap type and nextblock entry of this blockette */
	while  (next_block > 0)  {
		b1000 = (SeedBlockette1000T *)((char *)hdr + (unsigned)(next_block));
		next_block = 0;
		/* check for blockette 1000 and 1001 */
		if  (b1000->type == 1000)  b1000->type = 0xe803;
		else if  (b1000->type == 1001)  b1000->type = 0xe903;
		else if  (b1000->type == 0xe803)  b1000->type = 1000;
		else if  (b1000->type == 0xe903)  b1000->type = 1001;
		/* swap pointer to next blockette and store pointer value */
		v_uword = b1000->nextblock;
		if  (v_uword >= 48 && v_uword < 64)  next_block = v_uword;
		b1000->nextblock = (v_uword & 0xff) * 0x100;
		b1000->nextblock += (UWORD)(v_uword & 0xff00) / (UWORD)0x100;
		if  ((b1000->nextblock) >= 48 && (b1000->nextblock) < 64)
			next_block = b1000->nextblock;
	} /*endif*/

} /* end of SeedSwapHeader */



/*---------------------------------------------------------------------*/



void SeedCloseFiles( void )

/* closes all open SEED files
 *
 * no parameters
 */
{
	/* local variables */
	int      i;         /* counter */

	/* executable code */

	for  (i=0; i<Seed_C_MAXCHAN; i++)
		if  (seedv_fd[i].ufd != Seed_C_ILLEGAL)  {
			close( seedv_fd[i].ufd );
			seedv_fd[i].ufd = Seed_C_ILLEGAL;
		} /*endif*/

} /* end of SeedCloseFiles */



/*---------------------------------------------------------------------*/



static void SeedAdjustFilename( SeedFileDescrT *dsc, STATUS *status )

/* copies directory of sfd-file to data filename
 *
 * parameters of routine
 * SeedFileDescrT  *dsc;        modify; SEED file descriptor
 * STATUS          *status;     output; return status
 */
{
	/* local variables */
	int      i;                      /* counter */
	int      strlth;                 /* string length */
	char     path[BC_FILELTH+1];     /* path name */
	BOOLEAN  iscolon;                /* is there a colon ? */
	char     upname[cBcFileLth+1];   /* uppercase filename */

	/* executable code */

	/* find path in sfdfile */
	i = strlen( dsc->sfdfile ) - 1;
	while  (i >= 0 && dsc->sfdfile[i] != '/' && dsc->sfdfile[i] != ':'
		&& dsc->sfdfile[i] != '\\')  i--;
	if  (i > 0)  {
		strncpy( path, dsc->sfdfile, i );
		path[i] = '\0';
		iscolon = (dsc->sfdfile[i] == ':');
	} else {
		*path = '\0';
		iscolon = FALSE;
	} /*endif*/

	if  (strlen(dsc->name)-ROOTDIRLTH+strlen(path) > BC_FILELTH)  {
		*status = SeedERR_STROVFL;
		return;
	} /*endif*/

	if  (*path != '\0')  {
		strlth = strlen( path );
		for  (i=0; i<strlth; i++)
			if  (path[i] == '\\')  path[i] = '/';
		if  (iscolon)  {
			strcat( path, ":" );
			if  (seedv_capfiles)  {
				strcpy( upname, dsc->name+ROOTDIRLTH+1 );
				ut_cap( upname );
				strcat( path, upname );
			} else {
				strcat( path, dsc->name+ROOTDIRLTH+1 );
			} /*endif*/
		} else {
			if  (seedv_capfiles)  {
				strcpy( upname, dsc->name+ROOTDIRLTH );
				ut_cap( upname );
				strcat( path, upname );
			} else {
				strcat( path, dsc->name+ROOTDIRLTH );
			} /*endif*/
		} /*endif*/
	} else {
		strcpy( path, dsc->name+ROOTDIRLTH+1 );
	} /*endif*/
	strcpy( dsc->name, path );

} /* end of SeedAdjustFilename */



/*---------------------------------------------------------------------*/


static void SeedLibWarning( char text[] )

/* prints warning messages
 *
 * parameters of routine
 * char       text[];      input; output text
 */
{
	/* local variables */
	char     str[cBcLineLth+1];    /* output string */
	int      i;                    /* counter */

	/* executable code */

	if  (!seedv_warnings)  return;

#ifdef XXX
	i = 0;
	while  (i < cBcLineLth && text[i] != '\0')  {
		if  ((text[i] >= ' ' && text[i] <= 'z') || text[i] == '\n')  {
			str[i] = text[i];
		} else {
			str[i] = '.';
		} /*endif*/
		i++;
	} /*endwhile*/
	str[i] = '\0';

	fprintf( stderr, "%s", str );
#endif
	fprintf( stderr, "%s", text );

} /* end of SeedLibWarning */



/*---------------------------------------------------------------------*/



void SeedPrintWarnings( BOOLEAN on_off )

/* switches warning messages on or off
 *
 * parameters of routine
 * BOOLEAN    on_off;       input; switch
 */
{
	/* executable code */

	seedv_warnings = on_off;

} /* end of SeedPrintWarnings */



/*---------------------------------------------------------------------*/



void SeedAcceptCapfiles( BOOLEAN on_off )

/* switches on/off whether capitalized filenames on $ROOT-paths are accepted
 *
 * parameters of routine
 * BOOLEAN    on_off;       input; switch
 */
{
	/* executable code */

	seedv_capfiles = on_off;

} /* end of SeedPrintWarnings */



/*---------------------------------------------------------------------*/



BOOLEAN SeedSetInvertSwapHeader( BOOLEAN on_off )

/* Sets the global variable seedv_invert_swap_hdr to invert header swap
 * info in sfd-file.  Returns old value.
 *
 * parameters of routine
 * BOOLEAN    on_off;    input; switch value
 */
{
	/* local variables */
	BOOLEAN  old;

	/* executable code */

	old = seedv_invert_swap_hdr;
	seedv_invert_swap_hdr = on_off;
	return old;

} /* end of SeedSetInvertSwapHeader */



/*---------------------------------------------------------------------*/



BOOLEAN SeedSetDecodeErrorAbort( BOOLEAN on_off )

/* Sets the global value seedv_decerr_abort (abort on occurrence of
 * SEED decoding errors.  Returns old value.
 *
 * parameters of routine
 * BOOLEAN    on_off;        input; switch value
 */
{
	/* local variables */
	BOOLEAN  old;        /* old value */

	/* executable code */

	old = seedv_decerr_abort;
	seedv_decerr_abort = on_off;
	return old;

} /* end of SeedSetDecodeErrorAbort */



/*---------------------------------------------------------------------*/



BOOLEAN SeedUseTimeCorrection( BOOLEAN on_off )

/* Sets the global value seedv_use_timecorr (use timec orrection field
 * within SEED data records).  Returns old value.
 *
 * parameters of routine
 * BOOLEAN    on_off;        input; switch value
 */
{
	/* local variables */
	BOOLEAN  old;        /* old value */

	/* executable code */

	old = seedv_use_timecorr;
	seedv_use_timecorr = on_off;
	return old;

} /* end of SeedUseTimeCorrection */



/*---------------------------------------------------------------------*/



void SeedCutStartRecord( SeedSbyteT *seedrec, char extime[], BOOLEAN swap,
	STATUS *status )

/* Throws away the first samples in record until given time
 *
 * parameters of routine
 * char       *seedrec;         modify; SEED record to cut
 * char       extime[];         input; exact time
 * BOOLEAN    swap;             input; swap bytes ?
 * STATUS     *status;          output; return status
 */
{
	/* local variables */
	SeedDataHeaderT *hdrptr;   /* pointer to data header */
	SeedDataHeaderT *outrec;   /* output records */
	SeedDataHeaderT tmphdr;    /* temp store */
	float    dt;               /* sample rate */
	NTIME    rstart, rend;     /* record start and end times */
	NTIME    ntime;            /* numeric exact time */
	float    tdiff;            /* time difference in sec */
	int      cutlth;           /* number of samples to cut */
	INT32    smp[MAX_SMP_PER_REC]; /* space for decoded samples */
	INT32    smplth;           /* number of sample decoded */
	int      i;                /* counter */
	int      recs_used;        /* records used for encoding (should be 1) */
	int      reclth;           /* record length */

	/* executable code */

	/* retrieve header information needed here */
	hdrptr = (SeedDataHeaderT *)seedrec;
	dt = SeedGetSampleDist( hdrptr );
	reclth = SeedGetReclth( hdrptr, FALSE );
	SeedRecordTime( seedrec, &rstart, &rend, status );
	if  (SySevere(status))  return;
	tc_t2n( extime, &ntime, status );
	if  (SySevere(status))  return;
	tdiff = tc_ndiff( &ntime, &rstart, status );
	if  (SySevere(status))  return;
	cutlth = Nint( tdiff / dt );
	if  (cutlth > (int)(hdrptr->no_of_samples))  {
		*status = SeedERR_ILLCUT;
		return;
	} else if  (cutlth == (int)(hdrptr->no_of_samples))  {
		hdrptr->no_of_samples = 0;
		return;
	} else if  (cutlth <= 0)  {
		/* nothing to be done */
		return;
	} /*endif*/

	/* decode record */
	SeedDecodeSteim1( NULL, swap, 0, NULL, NULL );
	SeedDecodeSteim1( seedrec, swap, MAX_SMP_PER_REC, smp, &smplth );
	if  (smplth != (INT32)(hdrptr->no_of_samples))  {
		*status = SeedERR_DECODE_ERR;
		return;
	} /*endif*/

	/* throw away cutlth samples from the beginning */
	for  (i=cutlth; i<(int)smplth; i++)
		smp[i-cutlth] = smp[i];
	smplth -= (INT32)cutlth;
	tdiff = (float)cutlth * dt;
	tc_nadd( &rstart, tdiff, &rstart, status );
	if  (SySevere(status))  return;

	/* prepare changed record */
	outrec = (SeedDataHeaderT *)sy_allocmem( 2, reclth, status );
	/* get two records; need only one but who knows ... */
	hdrptr->no_of_samples = (UWORD)smplth;
	SeedNtimeToBtime( &rstart, &(hdrptr->starttime), status );
	if  (SySevere(status))  {
		sy_deallocmem( outrec );
		return;
	} /*endif*/
	recs_used = (int)Steim_comp( smp, (DATA_HEADER *)hdrptr, (UINT32)smplth,
		(WORD)(reclth), (INT32 *)outrec, 0 /*prev.smp*/ );
	if  (recs_used != 1)  {
		*status = SeedERR_BUG;
		sy_deallocmem( outrec );
		return;
	} /*endif*/
	/* copy result to output record and keep header */
	tmphdr = *hdrptr;
	memcpy( (char *)seedrec, (char *)outrec, reclth );
	SeedStoreReclth( (SeedDataHeaderT *)seedrec, reclth );
	*hdrptr = tmphdr;
	sy_deallocmem( outrec );

} /* end of SeedCutStartRecord */



/*---------------------------------------------------------------------*/



void SeedCutEndRecord( SeedSbyteT *seedrec, char extime[], BOOLEAN swap,
	STATUS *status )

/* Throws away the first samples in record until given time
 *
 * parameters of routine
 * char       *seedrec;         modify; SEED record to cut
 * char       extime[];         input; exact time
 * BOOLEAN    swap;             input; swap bytes ?
 * STATUS     *status;          output; return status
 */
{
	/* local variables */
	SeedDataHeaderT *hdrptr;   /* pointer to data header */
	SeedDataHeaderT *outrec;   /* output records */
	SeedDataHeaderT tmphdr;    /* temp store */
	float    dt;               /* sample rate */
	NTIME    rstart, rend;     /* record start and end times */
	NTIME    ntime;            /* numeric exact time */
	float    tdiff;            /* time difference in sec */
	int      cutlth;           /* number of samples to cut */
	INT32    smp[MAX_SMP_PER_REC]; /* space for decoded samples */
	INT32    smplth;           /* number of sample decoded */
	int      i;                /* counter */
	int      recs_used;        /* records used for encoding (should be 1) */
	int      reclth;           /* record length */

	/* executable code */

	/* retrieve header information needed here */
	hdrptr = (SeedDataHeaderT *)seedrec;
	dt = SeedGetSampleDist( hdrptr );
	reclth = SeedGetReclth( hdrptr, FALSE );
	SeedRecordTime( seedrec, &rstart, &rend, status );
	if  (SySevere(status))  return;
	tc_t2n( extime, &ntime, status );
	if  (SySevere(status))  return;
	tdiff = tc_ndiff( &rend, &ntime, status );
	if  (SySevere(status))  return;
	cutlth = Nint( tdiff / dt );
	if  (cutlth > (int)(hdrptr->no_of_samples))  {
		*status = SeedERR_ILLCUT;
		return;
	} else if  (cutlth == (int)(hdrptr->no_of_samples))  {
		hdrptr->no_of_samples = 0;
		return;
	} else if  (cutlth <= 0)  {
		/* nothing to be done */
		return;
	} /*endif*/

	/* decode record */
	SeedDecodeSteim1( NULL, swap, 0, NULL, NULL );
	SeedDecodeSteim1( seedrec, swap, MAX_SMP_PER_REC, smp, &smplth );
	if  (smplth != (INT32)(hdrptr->no_of_samples))  {
		*status = SeedERR_DECODE_ERR;
		return;
	} /*endif*/

	/* throw away cutlth samples at the end */
	smplth -= (INT32)cutlth;

	/* prepare changed record */
	outrec = (SeedDataHeaderT *)sy_allocmem( 2, reclth, status );
	/* get two records; need only one but who knows ... */
	hdrptr->no_of_samples = (UWORD)smplth;
	recs_used = (int)Steim_comp( smp, (DATA_HEADER *)hdrptr, (UINT32)smplth,
		(WORD)(reclth), (INT32 *)outrec, 0 /*prev.smp*/ );
	if  (recs_used != 1)  {
		*status = SeedERR_BUG;
		sy_deallocmem( outrec );
		return;
	} /*endif*/
	/* copy result to output record and keep header */
	tmphdr = *hdrptr;
	memcpy( (char *)seedrec, (char *)outrec, reclth );
	SeedStoreReclth( (SeedDataHeaderT *)seedrec, reclth );
	*hdrptr = tmphdr;
	sy_deallocmem( outrec );

} /* end of SeedCutEndRecord */



/*---------------------------------------------------------------------*/



char *SeedGetFilename( int chan )

/* returns name of currently opened file on channel 'chan'.  If no filename
 * is found, a pointer to an empty string is returned.
 *
 * parameters of routine
 * int        chan;        input; channel number
 *                         returns name of file
 */
{
	/* local variables */
	static char nullstr[2] = "";  /* null string */
	SeedFileDescrT *fd;   /* pointer to SEED file descriptor */

	/* executable code */

	/* check channel number */
	if  (chan < 0 || chan >= Seed_C_MAXCHAN)  return nullstr;
	fd = seedv_fd + chan;
	return ((fd->name[0] == '\0') ? seedv_last_fname : fd->name);

} /* end of SeedGetFilename */



/*---------------------------------------------------------------------*/



void SeedStoreReclth( SeedDataHeaderT *rec, int reclth )

/* Stores record length in SEED record. Uses Reserved_bytes_A for this purpose
 * if no blockette 1000 is available.
 *
 * parameters of routine
 * SeedDataHeaderT   *rec;   modify;  SEED record
 */
{
	/* local variables */
	int      num, i;        /* value and exp counter */
	TSyBoolean b1000;       /* blockette 1000 found */
	UWORD    *uw;           /* pointer to UWORD */
	char     *cp;           /* pointer to char */
	UWORD    recfirst;      /* offset to first blockette */
	UWORD    tmp;           /* scratch */

	/* executable code */

	/* determine which power of 2 */
	num = 1;
	i = 0;
	while  (num < reclth)  {
		i++;
		num *= 2;
	} /*endwhile*/

	/* if a blockette 1000 is there use it */
	b1000 = TRUE;
	if  (rec->no_of_blockettes == 0)  b1000 = FALSE;
	if  (b1000 && rec->first == 0)  b1000 = FALSE;

	if  (b1000)  {
		if  (rec->first > reclth)  {
			tmp = rec->first;
			recfirst = (tmp & 0xff) * 0x100;
			recfirst += (UWORD)(tmp & 0xff00) / (UWORD)0x100;
		} else {
			recfirst = rec->first;
		} /*endif*/
		if  (recfirst % 2 == 1)  {
			SeedLibWarning(
				"*** illegal 'first' entry in record (StoreReclth) ***\n" );
			b1000 = FALSE;
		} /*endif*/
	} /*endif*/

	if  (b1000)  {
		uw = (UWORD *)((char *)rec + (int)recfirst);
		if  (*uw != 1000 && *uw != 59395)  b1000 = FALSE;
	} /*endif*/

	if  (b1000)  {
		cp = (char *)rec + (int)recfirst + 6;
		*cp = (char)i;
	} else {
		rec->Reserved_bytes_A = (char)i;
	} /*endif*/

} /* end of SeedStoreReclth */



/*---------------------------------------------------------------------*/



int SeedGetReclth( SeedDataHeaderT *rec, TSyBoolean clean )

/* Returns SEED record length stored in record (by SeedStoreReclth)
 * If 'clean' is TRUE, the record length info is deleted from the record.
 *
 * parameters of routine
 * SeedDataHeaderT  *rec;      modify; SEED record
 * TSyBoolean       clean;     input; if TRUE, record length info is deleted
 */
{
	/* local variables */
	int      reclth;        /* record length */
	TSyBoolean b1000;       /* blockette 1000 found */
	UWORD    *uw;           /* pointer to UWORD */
	UWORD    recfirst;      /* offset to first blockette */
	char     *cp;           /* pointer to char */
	UWORD    tmp;           /* scratch */

	/* executable code */

	/* if a blockette 1000 is there use it */
	b1000 = TRUE;
	if  (rec->no_of_blockettes == 0)  b1000 = FALSE;
	if  (b1000 && rec->first == 0)  b1000 = FALSE;
	if  (b1000)  {
		if  (rec->first > (UWORD)sizeof(SeedDataHeaderT))  {
			tmp = rec->first;
			recfirst = (tmp & 0xff) * 0x100;
			recfirst += (UWORD)(tmp & 0xff00) / (UWORD)0x100;
		} else {
			recfirst = rec->first;
		} /*endif*/
		if  (recfirst % 2 == 1)  {
			SeedLibWarning(
				"*** illegal 'first' entry in record (StoreReclth) ***\n" );
			b1000 = FALSE;
		} /*endif*/
	} /*endif*/
	if  (b1000)  {
		uw = (UWORD *)((char *)rec + (int)(recfirst));
		if  (*uw != 1000 && *uw != 59395)  b1000 = FALSE;
	} /*endif*/

	if  (b1000)  {
		cp = (char *)rec + (int)(recfirst) + 6;
		reclth = (*cp);
		reclth = (1 << reclth);
	} else {
		if  (rec->Reserved_bytes_A == '\0' || rec->Reserved_bytes_A == ' ')  {
			fprintf( stderr,
				"SeedGetReclth: no record length found in record. Abort.\n" );
			exit( 1 );
		} /*endif*/
		reclth = (1 << (int)(rec->Reserved_bytes_A));
		if (clean)  rec->Reserved_bytes_A = ' ';
	} /*endif*/

	return reclth;

} /* end of SeedGetReclth */



/*---------------------------------------------------------------------*/



int SeedGetCompression( SeedDataHeaderT *rec )

/* Returns compression algorithm ID if blockette 1000 is found,
 * otherwise 0.
 *
 * parameters of routine
 * SeedDataHeaderT  *rec;      input; SEED record
 */
{
	/* local variables */
	TSyBoolean b1000;       /* blockette 1000 found */
	UWORD    *uw;           /* pointer to UWORD */
	char     *cp;           /* pointer to char */

	/* executable code */

	/* if a blockette 1000 is there use it */
	b1000 = TRUE;
	if  (rec->no_of_blockettes == 0)  b1000 = FALSE;
	if  (b1000 && rec->first == 0)  b1000 = FALSE;
	if  (rec->first % 2 == 1)  {
		SeedLibWarning(
			"*** illegal 'first' entry in record (GetCompression) ***\n" );
		return 0;
	} /*endif*/
	if  (b1000)  {
		uw = (UWORD *)((char *)rec + (int)(rec->first));
		if  (*uw != 1000 && *uw != 59395)  b1000 = FALSE;
	} /*endif*/
	if  (!b1000)  return 0;

	cp = (char *)rec + (int)(rec->first) + 4;
	return (int)(*cp);

} /* end of SeedGetCompression */



/*---------------------------------------------------------------------*/



int SeedGetTimequal( SeedDataHeaderT *rec )

/* Returns time quality or -1
 *
 * parameters of routine
 * SeedDataHeaderT   *rec;
 */
{
	/* local variables */
	SeedBlockette1001T *b1001;       /* blockette 1001 */

	/* executable code */

	if  (rec->no_of_blockettes <= 0)  return (-1);

	b1001 = (SeedBlockette1001T *)((char *)rec + (unsigned)(rec->first));
	if  (b1001->type == 1001)  return (int)(b1001->timequal);
	if  (rec->no_of_blockettes == 1)  return (-1);
	b1001 = (SeedBlockette1001T *)((char *)rec + (unsigned)(b1001->nextblock));
	if  (b1001->type == 1001)  return (int)(b1001->timequal);
	return (-1);
	/* not more than 2 blockettes checked, should implement loop */

} /* end of SeedGetTimequal */



/*---------------------------------------------------------------------*/



int SeedGetWordOrder( SeedDataHeaderT *rec )

/* Returns word order ID if blockette 1000 is found,
 * otherwise 1 (=Sparc).
 *
 * parameters of routine
 * SeedDataHeaderT  *rec;      input; SEED record
 */
{
	/* local variables */
	TSyBoolean b1000;       /* blockette 1000 found */
	UWORD    *uw;           /* pointer to UWORD */
	char     *cp;           /* pointer to char */

	/* executable code */

	/* if a blockette 1000 is there use it */
	b1000 = TRUE;
	if  (rec->no_of_blockettes == 0)  b1000 = FALSE;
	if  (b1000 && rec->first == 0)  b1000 = FALSE;
	if  (rec->first % 2 == 1)  {
		SeedLibWarning( "*** illegal 'first' entry in record ***\n" );
		return 1;
	} /*endif*/
	if  (b1000)  {
		uw = (UWORD *)((char *)rec + (int)(rec->first));
		if  (*uw != 1000 && *uw != 59395)  b1000 = FALSE;
	} /*endif*/
	if  (!b1000)  return 1;  /* Sparc word order is default */

	cp = (char *)rec + (int)(rec->first) + 5;
	return (int)(*cp);

} /* end of SeedGetWordOrder */



/*---------------------------------------------------------------------*/



void SeedSetOutputReclth( int reclth )

/* Sets size of SEED output records (default is 4096, if not called)
 *
 * parameters of routine
 * int        reclth;       input; size of output records in bytes
 */
{
	/* executable code */

	seedv_out_reclth = reclth;

} /* end of SeedSetOutputReclth */



/*----------------------------------------------------------------------------*/



void SeedSetNextTimequal( int tq )

/* Sets time quality for next Steim output record
 *
 * parameters of routine
 * int        tq;         input; time quality
 */
{
	/* executable code */

	seedv_timequal = tq;

} /* end of SeedSetNextTimequal */



/*----------------------------------------------------------------------------*/


#define IDOFFSET 6
#define DATCHARS "DQR"


void SeedQuickFindReclth( FILE *fp, int *recsize, int *byteoff )

/* Returns record size in bytes or 0 and the byte offset where data records
 * start.
 *
 * parameters of routine
 * FILE          *fp;          input; input file already opened
 * int           *recsize;     output; record size found
 * int           *byteoff;     output; byte offset found
 */
{
	/* local variables */
	char     id1, id2;  /* ID chars */
	TSyBoolean isdigit1, isdigit2; /* is number string */
	int      i;         /* counter */

	/* executable code */

	*recsize = 0;
	*byteoff = 0;
	isdigit1 = isdigit2 = TRUE;

	/* position file and read number string and ID */
	fseek( fp, 0, 0 );
	for  (i=0; i<IDOFFSET; i++)  {
		id1 = getc( fp );
		if  (id1 < '0' || id1 > '9')  isdigit1 = FALSE;
	} /*endfor*/
	id1 = getc( fp );
	fseek( fp, 512, 0 );
	for  (i=0; i<IDOFFSET; i++)  {
		id2 = getc( fp );
		if  (id2 < '0' || id2 > '9')  isdigit2 = FALSE;
	} /*endfor*/
	id2 = getc( fp );
	if  (id1 == EOF || isdigit1 == FALSE)  {
		/* no SEED file, leave recsize zero */
	} else if  (strchr(DATCHARS,id1) != NULL && strchr(DATCHARS,id2) == NULL)  {
		/* definitely not 512 */
		*recsize = 4096;
		*byteoff = 0;
	} else if  (id1 == 'V' && strchr(DATCHARS,id2) != NULL && isdigit2 == TRUE)  {
		/* very likely 512 */
		*recsize = 512;
		*byteoff = 512;
	} else if  (isdigit2 == FALSE)  {
		*recsize = 4096;
		*byteoff = 0;  /* this is a guess */
	} else if  (strchr(DATCHARS,id1) != NULL && strchr(DATCHARS,id2) != NULL)  {
		*byteoff = 0;
		fseek( fp, 1024+IDOFFSET, 0 );
		id1 = getc( fp );
		fseek( fp, 1024+512+IDOFFSET, 0 );
		id2 = getc( fp );
		if  (strchr(DATCHARS,id1) != NULL && strchr(DATCHARS,id2) != NULL)  {
			*recsize = 512;
		} else if  (strchr(DATCHARS,id1) != NULL && id2 == EOF)  {
			*recsize = 512;
		} else if  (id1 == EOF)  {
			*recsize = 512;
		} else {
			*recsize = 4096;
		} /*endfif*/
	} else {
		/* cannot determine */
		*recsize = -1;
	} /*endif*/

	/* rewind file */
	fseek( fp, 0, 0 );

} /* end of SeedQuickFindReclth */



/*---------------------------------------------------------------------*/



static void SeedSwapLinuxRecord( SeedSbyteT *rec, int reclth )


/* Modifies record for writing on Linux system
 *
 * parameters of routine
 * SeedSbyteT *rec;                modify; SEED record
 * int        reclth;              input; record length, if 0 found by SeedGetReclth
 */
{
	/* local variables */
	SeedSbyteT     *ptr;                /* moving pointer */
	SeedSbyteT     tmp;                 /* scratch */

	/* executable code */

	if  (reclth == 0)
		reclth = SeedGetReclth( (SeedDataHeaderT *)rec, FALSE );

	for  (ptr = rec+Seed_C_FRAMELTH; ptr < rec+reclth;
		ptr += 4)  {
		/* flip 0 and 3 */
		tmp = ptr[0];  ptr[0] = ptr[3];  ptr[3] = tmp;
		/* flip 1 and 2 */
		tmp = ptr[1];  ptr[1] = ptr[2];  ptr[2] = tmp;
	} /*endfor*/

} /* end of SeedSwapLinuxRecord */



/*---------------------------------------------------------------------*/



static void SeedSplitStreamName( char inpname[], char outname[],
	char station[], char addinf[] )

/* Splits extended stream name (e.g. anmo.iu00-bh-z) into standard stream
 * name (e.g. anmo-bh-z) and addinfo string (iu00).  Minimum output length
 * of addinf is Seed_C_AddinfLth+1.
 *
 * parameters of routine
 * char       inpname[];     input; extended stream name
 * char       outname[];     output; standard stream name
 * char       station[];     output; station name
 * char       addinf[];      output; addinfo string
 */
{
	/* local variables */
	int      i, j, k;         /* counter */
	int      slen;            /* length of stream string */
	TSyBoolean add;           /* addinfo string */
	TSyBoolean first;         /* first point */
	TSyBoolean isstat;        /* station part */

	/* executable code */

	/* default addinf */
	strcpy( addinf, Seed_C_EmptyAddinfStr );
	*station = '\0';

	first = TRUE;
	slen = strlen( inpname );
	j = k = 0;
	add = FALSE;
	isstat = TRUE;
	for  (i=0; i<slen; i++)  {
		if  (inpname[i] == '.' && first)  {
			add = TRUE;
			first = FALSE;
			if  (isstat)  {
				station[i] = '\0';
				isstat = FALSE;
			} /*endif*/
		} else if  (inpname[i] == '-')  {
			add = FALSE;
			outname[k++] = inpname[i];
			if  (isstat)  {
				station[i] = '\0';
				isstat = FALSE;
			} /*endif*/
		} else {
			if  (isstat)  station[i] = Uncap( inpname[i] );
			if  (add && j < Seed_C_AddinfLth)  {
				addinf[j++] = Uncap( inpname[i] );
			} else {
				outname[k++] = inpname[i];
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	outname[k] = '\0';

	/*printf( "--> %s -> %s,%s\n", inpname, outname, addinf );*/

} /* end of SeedSplitStreamName */


/*---------------------------------------------------------------------*/



/* FORTRAN interface routine */


void SeedReadStreamReal_( int chan, char sfdfile[], char stream_str[],
	BOOLEAN swap, char req_start[], float seclth, float **fdat,
	INT32 *smplth, char act_start[], float *dt, float *calib,
	int *flags, STATUS *status )
{

	SeedReadStreamReal( chan, sfdfile, stream_str, swap, req_start,
		seclth, fdat, smplth, act_start, dt, calib, flags, status );

} /* end of SeedReadStreamReal_ */


void SeedLibInitialize_( STATUS *status )
{

	SeedLibInitialize( status );

} /* end of SeedLibInitialize_ */


void SeedSetup_( STATUS *status )
{
	static SeedSetupParT setup;

	SeedSetup( &setup, status );

} /* end of SeedSetup_ */


/*---------------------------------------------------------------------*/


/* file seedcfg.h
 *      =========
 *
 * version 29, 29-Dec-2006
 *
 * SEED basic constants and types
 * K. Stammler, 25-Dec-93
 */


#ifndef __TCUSRDEF
#include BC_TCUSRDEF
#endif


/* temporary */
/* #define err_writemsg(a,b,c) printf("status %d\n",a) */




/*
 * error codes
 * -----------
 */

#define SeedERR_OFFSET       7000
#define SeedERR_OPENINPUT    (SeedERR_OFFSET+1)   /* error opening input file */
#define SeedERR_NOSETUP      (SeedERR_OFFSET+2)   /* no setup file read */
#define SeedERR_EOF_FOUND    (SeedERR_OFFSET+3)   /* EOF found */
#define SeedERR_ILLSTREAM    (SeedERR_OFFSET+4)   /* illegal stream name */
#define SeedERR_NOTIMELIST   (SeedERR_OFFSET+5)   /* no time list specified */
#define SeedERR_TWLOVFL      (SeedERR_OFFSET+6)   /* too many time lists */
#define SeedERR_TWLLTH       (SeedERR_OFFSET+7)   /* time list too long */
#define SeedERR_ILLTWL       (SeedERR_OFFSET+8)   /* illegal time window */
#define SeedERR_STREAMOVFL   (SeedERR_OFFSET+9)   /* too many streams */
#define SeedERR_ILLRECSIZE   (SeedERR_OFFSET+10)  /* illegal record size */
#define SeedERR_SFD_READ     (SeedERR_OFFSET+11)  /* error reading sfd-file */
#define SeedERR_SFD_NOTFOUND (SeedERR_OFFSET+12)  /* stream not found in sfd */
#define SeedERR_SFD_NEXT     (SeedERR_OFFSET+13)  /* next file returned */
#define SeedERR_CHAN_OOR     (SeedERR_OFFSET+14)  /* SEED channel out of range*/
#define SeedERR_STROVFL      (SeedERR_OFFSET+15)  /* string overflow */
#define SeedERR_SEEDREAD     (SeedERR_OFFSET+16)  /* read error on SEED file */
#define SeedERR_SEEDSEEK     (SeedERR_OFFSET+17)  /* seek error on SEED file */
#define SeedERR_BUG          (SeedERR_OFFSET+18)  /* program bug */
#define SeedERR_NOT_OPEN     (SeedERR_OFFSET+19)  /* channel not opened */
#define SeedERR_TIMEGAP      (SeedERR_OFFSET+20)  /* time gap */
#define SeedERR_OPENOUTPUT   (SeedERR_OFFSET+21)  /* error opening outp. file */
#define SeedERR_SEEDWRITE    (SeedERR_OFFSET+22)  /* error writing SEED file */
#define SeedERR_ZEROLTH      (SeedERR_OFFSET+23)  /* zero length trace quested*/
#define SeedERR_NEXTTIMEPOS  (SeedERR_OFFSET+24)  /* next possible time positioned */
#define SeedERR_DOUBLEDATA   (SeedERR_OFFSET+25)  /* double data in SEED file */
#define SeedERR_DATA_UNAVAIL (SeedERR_OFFSET+26)  /* data not available */
#define SeedERR_ILLCUT       (SeedERR_OFFSET+27)  /* illegal cut command */
#define SeedERR_DECODE_ERR   (SeedERR_OFFSET+28)  /* error decoding record */
#define SeedERR_READ_LOOKUP  (SeedERR_OFFSET+29)  /* error reading CD lookup */
#define SeedERR_ILL_COMPRS   (SeedERR_OFFSET+30)  /* illegal compression */
#define SeedERR_NOCALIB      (SeedERR_OFFSET+31)  /* no calibration gain found */



/* 
 * constants
 * ---------
 */

#define Seed_C_SHORTSTRLTH 7
#define Seed_C_SFDLINELTH 200
#define Seed_C_MAX_RECLTH 8192

/* set flags for setup parameters */
#define Seed_F_SET_MAX_NO_OF_STREAMS      0x00000001L
#define Seed_F_SET_MAX_NO_OF_TWL          0x00000002L
#define Seed_F_SET_MAX_LTH_OF_TWL         0x00000004L
#define Seed_F_SET_PHYS_REC_SIZE          0x00000008L
#define Seed_F_SET_SEED_REC_SIZE          0x00000010L
#define Seed_F_SET_NEW_SEED_NAMES         0x00000020L
#define Seed_F_SET_DUMP_ILL_RECS          0x00000040L
#define Seed_F_SET_EOM_EOF_CNT            0x00000080L
#define Seed_F_SET_MAX_ERRCNT_ILLEGAL     0x00000100L
#define Seed_F_SET_MAX_ERRCNT_INCOMPLETE  0x00000200L
#define Seed_F_SET_MAX_ERRCNT_READERR     0x00000400L
#define Seed_F_SET_COPY_BUF_LTH           0x00000800L

#define Seed_C_ILLEGAL -1

/* SEED data record activity flags */
#define Seed_F_ACT_CALIBRATION            0x01
#define Seed_F_ACT_TIMECORRAPP            0x02
#define Seed_F_ACT_TRIGSTART              0x04
#define Seed_F_ACT_TRIGEND                0x08
#define Seed_F_ACT_PLEAP                  0x10
#define Seed_F_ACT_NLEAP                  0x20
#define Seed_F_ACT_EVENT                  0x40

/* SEED data record I/O flags */
#define Seed_F_IO_PARITY                  0x01
#define Seed_F_IO_LONGREC                 0x02
#define Seed_F_IO_SHORTREC                0x04
#define Seed_F_IO_SERIESSTART             0x08
#define Seed_F_IO_SERIESEND               0x10
#define Seed_F_IO_CLOCKLOCKED             0x20

/* SEED data record quality flags */
#define Seed_F_QUAL_SATURATED             0x01
#define Seed_F_QUAL_CLIPPED               0x02
#define Seed_F_QUAL_SPIKES                0x04
#define Seed_F_QUAL_GLITCHES              0x08
#define Seed_F_QUAL_MISSING_PADDED        0x10
#define Seed_F_QUAL_SYNCERR               0x20
#define Seed_F_QUAL_FILTERCHARGING        0x40
#define Seed_F_QUAL_TIMETAG               0x80
/* private flag */
#define Seed_F_QUAL_NOCALIB               0x1000
#define Seed_F_QUAL_DATA_LOCKED           0x2000

/* SeedNextStream modes */
#define Seed_C_NSM_RESET 0
#define Seed_C_NSM_NEXT_STREAM 1
#define Seed_C_NSM_NEXT_SELECTED 2
#define Seed_C_NSM_TOTAL_NUMBER 3

#define Seed_C_MAXCHAN 10

/* Steim1 compression format */
#define Seed_C_FRAMELTH 64     /* frame length in bytes */
#define Seed_C_MAXFRAMESMP 120 /* maximum number of samples/frame */
#define Seed_C_FRAMEDIFFS 16   /* number of difference longwords in one frame */

/* SFD file item identification characters */
#define Seed_C_SfdTStart 'b'
#define Seed_C_SfdTEnd   'e'
#define Seed_C_SfdName   'n'
#define Seed_C_SfdRecno  'r'
#define Seed_C_SfdStream 's'
#define Seed_C_SfdCalib  'c'
#define Seed_C_SfdSwapH  'h'
#define Seed_C_SfdOffset 'o'
#define Seed_C_SfdReclth 'l'
#define Seed_C_SfdAddinf 'a'

/* return values to stream selection requests */
#define Seed_C_IsSelected 0
#define Seed_C_TimeNotSelected 1
#define Seed_C_StreamNotSelected 2

/* data compression types for blockette 1000 */
#define Seed_C_DcmpSteim1 10
#define Seed_C_DcmpSteim2 11

/* length of netcode and locid together */
#define Seed_C_AddinfLth 4
#define Seed_C_EmptyAddinfStr "...."

/* data formats (should not be here ...) */
#define Seed_C_FORMAT_MSEED 1



/*
 * types
 * -----
 */

#define SeedSbyteT signed char

typedef struct {
	char      setupfile[BC_FILELTH+1]; /* name of setup file */
	int       max_no_of_streams;    /* maximum number of streams */
	int       max_no_of_twl;        /* maximum number of time window lists */
	int       max_lth_of_twl;       /* maximum length of a time window list */
	int       phys_rec_size;        /* physical record size in bytes */
	int       seed_rec_size;        /* SEED record size */
	int       seed_phys_recs;       /* physical records per SEED records */
	BOOLEAN   new_seed_names;       /* use new names for SEED files (with "_") */
	BOOLEAN   dump_ill_recs;        /* dump unknown records to file */
	int       eom_eof_cnt;          /* number of consecutive EOF's for EOM */
	int       max_errcnt_illegal;   /* maximum number of illegal records */
	int       max_errcnt_incomplete;/* maximum number of incomplete records */
	int       max_errcnt_readerr;   /* maximum number of read errors */
	long      copy_buf_lth;         /* length of copy SEED buffer (in long's) */
} SeedSetupParT;

/* stream identifier */
typedef struct {
	char     array[Seed_C_SHORTSTRLTH+1];      /* array name or "none" */
	char     station[Seed_C_SHORTSTRLTH+1];    /* station name */
	char     channel[Seed_C_SHORTSTRLTH+1];    /* channel name, e.g. VBB */
	char     comp;                             /* component */
} SeedStreamT;

/* time type */
typedef TIME SeedTimeT;

/* time window lists */
typedef struct {
	SeedTimeT    *tw_begin;      /* start of time window */
	SeedTimeT    *tw_end;        /* end of time window */
	int          twl_length;     /* length of list */
} SeedTwlT;

/* SEED file descriptor */
typedef struct {
	char     stream[BC_SHORTSTRLTH+1];  /* stream string */
	char     t_start[BC_TIMELTH+1];     /* start time of file */
	char     t_end[BC_TIMELTH+1];       /* end time of file */
	char     name[BC_FILELTH+1];        /* file name */
	char     sfdfile[BC_FILELTH+1];     /* corresponding sfd-file */
	int      recno;                     /* number of SEED records in file */
	int      ufd;                       /* UNIX file descr. or Seed_C_ILLEGAL */
	int      pos;                       /* current read position in records */
	int      sample;                    /* sample position in record */
	float    calib;                     /* calibration of channel */
	int      dataflags;                 /* flags set on data Seed_F_QUAL_... */
	BOOLEAN  swap_hdr;                  /* swap header after read */
	int      reclth;                    /* SEED file record length */
	int      byteoff;                   /* byte offset in SEED file */
	int      pri;                       /* priority of entry */
	int      dataformat;                /* data format ID */
	char     addinf[Seed_C_AddinfLth+1];/* netcode+locid */
} SeedFileDescrT;

#define UBYTE unsigned char
#define WORD  short
#define UWORD unsigned short
#define INT32 int
#define UINT32 unsigned int

typedef struct {
    UWORD year ;	     /* e.g. 1991 */
    UWORD day ; 	     /* 1..366 */
    UBYTE hours ;	     /* 0..23 */
    UBYTE minutes ;	     /* 0..59 */
    UBYTE seconds ;	     /* 0..59, 60 for leap */
    UBYTE alignment_1 ;
    UWORD frac_secs ;		/* 0.0001 seconds, 0..9999 */
} SeedBtimeT ;

typedef struct {
	char   seqno[6];                      /* sequence number */
	char   indicator;                     /* type of record */
	char   Reserved_bytes_A;              /* not used */
	char   statcode[5];                   /* station name */
	char   locid[2];                      /* location identifier */
	char   channel[3];                    /* channel */
	char   network[2];                    /* network code */
	SeedBtimeT  starttime;                /* start time of record */
	UWORD  no_of_samples;                 /* number of samples in record */
	WORD   smprate_fact;                  /* sample rate factor */
	WORD   smprate_mult;                  /* sample rate multiplier */
	UBYTE  activity;                      /* activity flags */
	UBYTE  ioflags;                       /* IO flags */
	UBYTE  quality;                       /* data quality flags */
	UBYTE  no_of_blockettes;              /* number of blockettes to follow */
	INT32  timecorr;                      /* time correction in ms */
	UWORD  databegin;                     /* beginning of data */
	UWORD  first;                         /* first blockette */
} SeedDataHeaderT;


typedef struct {
	UWORD  type;                          /* blockette type (1000) */
	WORD   nextblock;                     /* byte number of next blockette */
	UBYTE  format;                        /* encoding format */
	UBYTE  wordorder;                     /* word order */
	UBYTE  reclthexp;                     /* record length (power of 2) */
	UBYTE  reserved;                      /* not used */
} SeedBlockette1000T;

typedef struct {
	UWORD  type;                          /* blockette type (1001) */
	WORD   nextblock;                     /* byte number of next blockette */
	UBYTE  timequal;                      /* time quality */
	UBYTE  reserved;                      /* ... */
} SeedBlockette1001T;


/* SeedDecodeSteim1 can also read Steim2 */
#define SeedDecodeSteim SeedDecodeSteim1


/*
 * prototypes
 * ----------
 */

/*--------------------------------------------------------------------*/


void SeedReadSetupFile( char fname[], STATUS *status );

/* Reads setup file (file containing basic constants) of SEED
 * processing.  Parameters are read to seedv_setup structure.
 *
 * parameters of routine
 * char       fname[];        input; name of setup file
 * STATUS     *status;        output; return status
 */


/*--------------------------------------------------------------------*/


void SeedChangeSetup( SeedSetupParT *par, long setflags, STATUS *status );

/* Changes setup values.  Only these values are copied whose setup-bit
 * is set in 'setflags'.
 *
 * parameters of routine
 * SeedSetupParT *par;          input; new values of setup parameters
 * long          setflags;      input; setup flags (Seed_F_SET_... - Bits)
 * STATUS        *status;       output; return status
 */


/*--------------------------------------------------------------------*/


void SeedSetup( SeedSetupParT *setup, STATUS *status );

/* Gets setup file.
 *
 * parameters of routine
 * SeedSetupParT *setup;     output; SEED setup
 * STATUS     *status;       output; return status
 */


/*--------------------------------------------------------------------*/


void SeedSetupF( STATUS *status );

/* Fortran interface to SeedSetup.
 *
 * parameters of routine
 * STATUS     *status;       output; return status
 */


/*--------------------------------------------------------------------*/


void SeedInitialize( STATUS *status );

/* Allocates memory for stream lists and time window lists.
 * Routine SeedReadSetupFile must be called before.
 *
 * parameters of routine
 * STATUS     *status;      output; return status
 */


/*--------------------------------------------------------------------*/


void SeedFinish( void );

/* Frees memory of time lists and stream lists
 *
 * no parameters
 */


/*--------------------------------------------------------------------*/


void SeedReadSelections( char fname[], STATUS *status );

/* Reads selections of streams and time windows from
 * given file.
 *
 * parameters of routine
 * char       fname[];        input; name of setup file
 */


/*--------------------------------------------------------------------*/


void SeedPrintSelections( void );

/* Prints selections read by SeedReadSelections to standard output
 *
 * no parameters
 */


/*--------------------------------------------------------------------*/


void SeedGetSetup( SeedSetupParT *setup );

/* Returns SEED setup parameters.  The values are copied to a passed
 * structure.  Changes to the parameters are not possible.
 *
 * parameters of routine
 * SeedSetupParT *setup;     output; setup parameters
 */


/*--------------------------------------------------------------------*/


BOOLEAN SeedIsSelected( SeedStreamT *str, char timestr[], STATUS *status );

/* Checks whether specified stream and time is in selection list
 *
 * parameters of routine
 * SeedStreamT   *str;        input; stream to check
 * SeedTimeT     *time;       input; time to check
 * STATUS        *status;     output; return status
 */


/*--------------------------------------------------------------------*/


BOOLEAN SeedWindowIsSelected( SeedStreamT *str, char t_start[],
	char t_end[], STATUS *status );

/* Checks whether specified time window is selected on any stream.
 *
 * parameters of routine
 * SeedStreamT   *str;        input; stream to check
 * char          t_start[];   input; start of time window
 * char          t_end[];       input; time to check
 * STATUS        *status;     output; return status
 */


/*--------------------------------------------------------------------*/


int SeedNextStream( int mode );

/* Returns index next stream or next selected stream depending on 'mode'
 * Possible values of 'mode' are:
 *    Seed_C_NSM_RESET:           start new list, returns illegal value
 *    Seed_C_NSM_NEXT_STREAM:     next stream, whether or not selected
 *    Seed_C_NSM_NEXT_SELECTED:   next selected stream
 *    Seed_C_NSM_TOTAL_NUMBER:    total number of streams
 * If no more stream is available, Seed_C_ILLEGAL is returned
 *
 * parameters of routine
 * int        mode;            input; controls returned value
 *                             returns requested stream index
 */


/*--------------------------------------------------------------------*/


int SeedIdentifyStreamString( char str[] );

/* Returns stream index.  'str' is stream string like "bfo-vbb-z".
 *
 * parameters of routine
 * char       str[];           input; stream string
 *                             returns stream index or Seed_C_ILLEGAL
 */


/*--------------------------------------------------------------------*/


void SeedGetStreamString( int stream_idx, char str[] );

/* Returns tream string from stream index.  'str' must be long
 * enough, i.e. 2*Seed_C_SHORTSTRLTH+3+1.
 *
 * parameters of routine
 * int        stream_idx;      input; stream index number
 * char       str[];           output; stream string
 */


/*--------------------------------------------------------------------*/


void SeedPrepareStreamString( char str[] );

/* replaces the characters "-","/","_" by blanks in given string
 *
 * parameters of routine
 * char       str[];        modify; string to be modified
 */


/*--------------------------------------------------------------------*/

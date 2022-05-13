
/* file gcflib.h
 *      ========
 *
 * version 5, 21-Nov-2005
 *
 * Header file of module gcflib.
 * Short description of GCF data format:
 * GCF data files structured in blocks of 1kB each.  Each block starts with
 * a raw header of four longwords, followed by a number of data records
 * (longwords).  The number of records following (max. 252 in 1kB block)
 * and their decoding in 4 byte-differences, 2 word-differences or a single
 * difference is described in the header entries.  The remaining longwords
 * of the data block are not used.
 *
 * K. Stammler, 18-Oct-2003
 */

/*
 * constants
 * ---------
 */

/* default size of GCF blocks in bytes */
#define Gcf_C_DEF_BLOCKSIZE 1024

/* GFD file entry identification characters */
#define Gcf_C_GfdTStart 'b'
#define Gcf_C_GfdTEnd   'e'
#define Gcf_C_GfdName   'n'
#define Gcf_C_GfdBlkno  'r'
#define Gcf_C_GfdStream 'g'
#define Gcf_C_GfdCalib  'c'
#define Gcf_C_GfdBlklth 'l'

/* maximum length of a gfd file */
#define Gcf_C_GFDLINELTH 200

/* maximum number of data channels */
#define Gcf_C_MAXCHAN 10

/* data flags */
#define Gcf_F_QUAL_NOCALIB 0x1000


/*
 * types
 * -----
 */

#define INT32 int

/* GCF string coded in Base 36 number */
#define GcfSTRING_LENGTH 7
typedef char GcfStringT[GcfSTRING_LENGTH];

typedef unsigned int GcfLongT;  /* GCF longword */

/* Gcf time code */
typedef unsigned int GcfTimeT;  /* GCF time type */

/* GCF raw header, first four longwords in GCF data block */
typedef struct {
	GcfLongT          system_id;         /* system ID, coded as base36 */
	GcfLongT          stream_id;         /* stream ID, coded as base36 */
	GcfTimeT          date_code;         /* start time of GCF block */
	GcfLongT          data_format;       /* data format information */
} GcfRawHeaderT;

/* Decoded header, expanded information from raw header */
/* length of station entry */
#define GcfSTATION_LENGTH 4
/* length of chan entry */
#define GcfCHAN_LENGTH 2
/* cmpcode values */
#define GcfCMP_32BIT 1
#define GcfCMP_16BIT 2
#define GcfCMP_8BIT  4
typedef struct {
	GcfStringT        system_id;                     /* system ID string */
	GcfStringT        stream_id;                     /* stream ID string */
	char              station[GcfSTATION_LENGTH+1];  /* station name */
	char              chan[GcfCHAN_LENGTH+1];        /* channel name */
	char              comp;                          /* component */
	char              blktime[cBcTimeLth+1];         /* start time of block */
	unsigned int      smprate;                       /* sample rate of data */
	unsigned int      cmpcode;                       /* compression code */
	unsigned int      numrec;                        /* no. of records in block*/
} GcfHeaderT;


/* GCF file descriptor */
typedef struct {
	char     stream[cBcShortStrLth+1];  /* stream string */
	char     t_start[cBcTimeLth+1];     /* start time of file */
	char     t_end[cBcTimeLth+1];       /* end time of file */
	char     name[cBcFileLth+1];        /* file name */
	char     gfdfile[cBcFileLth+1];     /* corresponding gfd-file */
	int      blkno;                     /* number of GCF blocks in file */
	FILE     *fp;                       /* pointer to data file or NULL */
	int      pos;                       /* current read position in records */
	int      sample;                    /* sample position in record */
	float    calib;                     /* calibration of channel */
	int      dataflags;                 /* data flags */
	BOOLEAN  swap_hdr;                  /* swap header after read */
	int      blklth;                    /* GCF file block length */
} GcfFileDescrT;

/*
 * error codes
 * -----------
 */

#define GcfERR_OFFSET        7700
#define GcfERR_READFILE      (GcfERR_OFFSET+1)  /* error reading input file */
#define GcfERR_ILLTAP        (GcfERR_OFFSET+2)  /* illegal tap code */
#define GcfERR_EOF_FOUND     (GcfERR_OFFSET+3)  /* EOF found while reading file */
#define GcfERR_ALLOCMEM      (GcfERR_OFFSET+4)  /* error allocating memory */
#define GcfERR_CHKSUM        (GcfERR_OFFSET+5)  /* error in block checksum */
#define GcfERR_STROVFL       (GcfERR_OFFSET+6)  /* string overflow */
#define GcfERR_OPENINPUT     (GcfERR_OFFSET+7)  /* error opening input file */
#define GcfERR_GFD_NOTFOUND  (GcfERR_OFFSET+8)  /* stream not found in gfd */
#define GcfERR_GFD_NEXT      (GcfERR_OFFSET+9)  /* next file returned */
#define GcfERR_GFD_READ      (GcfERR_OFFSET+10) /* error reading GFD file */
#define GcfERR_CHAN_OOR      (GcfERR_OFFSET+11) /* channel out of range */
#define GcfERR_NEXTTIMEPOS   (GcfERR_OFFSET+12) /* positioned to next time */
#define GcfERR_GCFSEEK       (GcfERR_OFFSET+13) /* seek error on gcf file */
#define GcfERR_BUG           (GcfERR_OFFSET+14) /* program bug */
#define GcfERR_NOT_OPEN      (GcfERR_OFFSET+15) /* file not open */
#define GcfERR_DATA_UNAVAIL  (GcfERR_OFFSET+16) /* data not available */
#define GcfERR_ZEROLTH       (GcfERR_OFFSET+17) /* zero length request */
#define GcfERR_DOUBLEDATA    (GcfERR_OFFSET+18) /* double data found */
#define GcfERR_ILLBYTEORDER  (GcfERR_OFFSET+19) /* illegal byte order */
#define GcfERR_TAPCODEFILE   (GcfERR_OFFSET+20) /* error in tapcode file */
#define GcfERR_ILLTAPCODE    (GcfERR_OFFSET+21) /* error in tapcode file */
#define GcfERR_NOCALIB       (GcfERR_OFFSET+22) /* no calibration gain found */


/*----------------------------------------------------------------------------*/


void GcfSetTapcode( int tapnum, char chan[], TSyStatus *status );

/* sets tap code, i.e. mapping from tap number to channel name
 *
 * parameters of routine
 * int        tapnum;     tap number to be set
 * char       chan[];     2 char channel code
 */


/*----------------------------------------------------------------------------*/


void GcfReadUserTapcodes( char fname[], TSyStatus *status );

/* Reads tap code names from file
 *
 * parameters of routine
 * char       fname[];    input; name of file or NULL
 *                              (then read from $HOME/.tapcodes)
 * TSyStatus  *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void GcfSetByteorder( char byteorder[], TSyStatus *status );

/* Sets byte order.  Bytes are counted from 1 to 4, Legal strings are
 * e.g. "1234" or "2143".
 *
 * parameters of routine
 * char       byteorder[]; input; byte order string of length 4 (+term char)
 * TSyStatus  *status; output; return status
 */


/*----------------------------------------------------------------------------*/


void GcfReadRawHeader( FILE *fp, GcfRawHeaderT *rhdr, TSyStatus *status );

/* Reads raw header from GCF file.
 *
 * parameters of routine
 * FILE          *fp;       input; pointer to input file (should be rewound)
 * GcfRawHeaderT *rhdr;     output; header longwords read from file
 * TSyStatus     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void GcfDecodeHeader( GcfRawHeaderT *rhdr, GcfHeaderT *hdr, TSyStatus *status );

/* Decodes raw header
 *
 * parameters fo routine
 * GcfRawHeaderT *rhdr;       input; raw header
 * GcfHeaderT    *hdr;        output; decoded header
 * TSyStatus     *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void GcfPrintHeader( FILE *out, GcfHeaderT *hdr );

/* Prints header infomration in one line to given output stream
 *
 * parameters of routine
 * FILE       *out;      input; output stream
 * GcfHeaderT *hdr;      input; header to be printed
 */


/*----------------------------------------------------------------------------*/


void GcfSkipDataBlock( FILE *gcf, GcfRawHeaderT *rhdr );

/* skips data block (jumps to next header or EOF) after a raw header has been
 * read
 * 
 * parameters of routine
 * FILE          *gcf; input; GCF data input file
 * GcfRawHeaderT *rhdr; input; raw header previously read
 */


/*----------------------------------------------------------------------------*/


GcfLongT *GcfAllocBlockMem( TSyStatus *status );

/* allocates memory for data block
 *
 * parameters of routine
 * TSyStatus  *status;      output; return status
 *                          returns pointer to block memory
 */


/*----------------------------------------------------------------------------*/


int *GcfAllocBlockSamples( TSyStatus *status );

/* allocates memory for samples of a block
 *
 * parameters of routine
 * TSyStatus  *status;      output; return status
 *                          returns pointer to memory for samples
 */


/*----------------------------------------------------------------------------*/


#define GcfFreeMem free


/*----------------------------------------------------------------------------*/


void GcfReadDataBlock( FILE *gcf, GcfRawHeaderT *rhdr, GcfLongT *mem,
	TSyStatus *status );

/* read in single data block from GCF file.  Corresponding header must
 * be already read in.
 *
 * parameters of routine
 * FILE       *gcf;      input; pointer to input GCF file
 * GcfRawHeaderT *rhdr;  input; GCF raw header previously read
 * GcfLongT   *mem;      output; pointer to memory
 * TSyStatus  *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void GcfDecodeBlock( GcfRawHeaderT *rhdr, GcfLongT *block, int *samples,
	int *smpnum, TSyStatus *status );

/* Decodes samples of a block.  Memory must be already allocated using
 * appropriate routines
 *
 * paramters of routine
 * GcfRawHeaderT *rhdr;      input; GCF raw header previously read
 * GcfLongT      *block;     input; data block previously read
 * int           *samples;   output; samples decoded
 * int           *smpnum;    output; number of samples
 * TSyStatus     *status;    output; return status
 */


/*---------------------------------------------------------------------*/


float GcfFindStreamCalibration( char stream[], char stime[], TSyStatus *status );

/* returns calibration value for counts for given stream 'stream' at time
 * 'stime'.
 *
 * parameters of routine
 * char           stream[];     input; stream string
 * char           stime[];      input; time
 * TSyStatus      *status;      output; return status
 *                              returns calibration or 0.0
 */


/*---------------------------------------------------------------------*/


void GcfParseGfdLine( char line[], GcfFileDescrT *fd, TSyStatus *status );

/* parses line of GFD file.  Returns structure elements 'stream',
 * 't_start', 't_end', 'name', 'recno' and 'calib'.
 *
 * parameters of routine
 * char       line[];       input; GFD line
 * GcfFileDescrT *fd;       output; GCF file descriptor
 * TSyStatus  *status;      output; return status
 */


/*---------------------------------------------------------------------*/


void GcfFindFileInGfd( char stream_str[], char start[], char gfdfile[],
	GcfFileDescrT *fdescr, TSyStatus *status );

/* Finds GCF file in gfd-file of given stream and containing given
 * start time.  If no file is found the file with the next possible
 * time is returned
 *
 * parameters of routine
 * char       stream_str[];      input; stream string like "bfo-vbb-z"
 * char       start[];           input; start time
 * char       gfdfile[];         input; GCF file directory
 * GcfFileDescrT *fdescr;        output; GCF file descriptor
 * TSyStatus  *status;           output; return status
 */


/*---------------------------------------------------------------------*/


void GcfSearchPosition( int chan, char gfdfile[], char stream_str[],
	char req_start[], char act_start[], TSyStatus *status );

/* Searches for given time position in a stream file.  The corresponding
 * GCF file is opened and positioned, its file descriptor stored
 * in an internal variable.  'chan' must range from 0..Gcf_C_MAXCHAN-1.
 * If the channel is already used the previously opened file is closed.
 *
 * parameters of routine
 * int        chan;             input; channel number
 * char       gfdfile[];        input; gfd-file
 * char       stream_str[];     input; stream string
 * char       req_start[];      input; requested start time
 * char       act_start[];      output; actual start time
 * TSyStatus  *status;          output; return status
 */


/*---------------------------------------------------------------------*/


void GcfReadNextBlock( int chan, GcfRawHeaderT *rhdr, GcfLongT *blk,
	TSyStatus *status );

/* Reads next available block (header and data) from GCF file.
 *
 * parameters of routine
 * int        chan;               input; channel number
 * GcfRawHeaderT *rhdr;           output; coded header of block
 * GcfLongT   *blk;               output; coded data of block
 * TSyStatus  *status;            output; return status
 */


/*---------------------------------------------------------------------*/


void GcfReadStream( int chan, char gfdfile[], char stream_str[],
	TSyBoolean unused, char req_start[], float seclth, INT32 **ldat,
	INT32 *smplth, char act_start[], float *dt, float *calib,
	int *flags, TSyStatus *status );

/* Reads sample data from GCF file.  The memory for the sample
 * data is allocated by the routine.  The channel number is chosen
 * by the caller of the routine.  It is legal to use always channel
 * number 0 but it is faster to use a separate channel number for
 * each stream used if the streams are read repeatedly.
 *
 * parameters of routine
 * int        chan;         input; channel number (0..Gcf_C_MAXCHAN-1)
 * char       gfdfile[];    input; name of gfd file to use
 * char       stream_str[]; input; stream string (like "bfo-vbb-z")
 * TSyBoolean unused;
 * char       req_start[];  input; requested start time
 * float      seclth;       input; number of seconds to be read
 * INT32      **ldat;       output; sample array read
 * INT32      *smplth;      output; number of samples read
 * char       act_start[];  output; actual start time
 * float      *dt;          output; sample distance in sec
 * float      *calib;       output; calibration constant
 * int        *flags;       output; data flags found
 * TSyStatus  *status;      output; return status
 */


/*---------------------------------------------------------------------*/


void GcfOrSeedReadStream( int chan, char gfdfile[], char stream_str[],
	TSyBoolean swap, char req_start[], float seclth, INT32 **ldat,
	INT32 *smplth, char act_start[], float *dt, float *calib,
	int *flags, TSyStatus *status );

/* Reads sample data from GCF or MiniSEED file.  Checks gfdfile for deciding
 * GCF or MiniSEED: if the the first character in gfdfile is 'g', GCF format
 * is read, otherwise MiniSEED format assumed. The memory for the sample
 * data is allocated by the routine.  The channel number is chosen
 * by the caller of the routine.  It is legal to use always channel
 * number 0 but it is faster to use a separate channel number for
 * each stream used if the streams are read repeatedly.
 *
 * parameters of routine
 * int        chan;         input; channel number (0..Gcf_C_MAXCHAN-1)
 * char       gfdfile[];    input; name of gfd file to use
 * char       stream_str[]; input; stream string (like "bfo-vbb-z")
 * TSyBoolean swap;         input; swap data in input
 * char       req_start[];  input; requested start time
 * float      seclth;       input; number of seconds to be read
 * INT32      **ldat;       output; sample array read
 * INT32      *smplth;      output; number of samples read
 * char       act_start[];  output; actual start time
 * float      *dt;          output; sample distance in sec
 * float      *calib;       output; calibration constant
 * int        *flags;       output; data flags found
 * TSyStatus  *status;      output; return status
 */


/*----------------------------------------------------------------------------*/


GcfLongT GcfGet4Bytes( FILE *fp, TSyStatus *status );

/* Reads 4 bytes from file
 *
 * parameters of routine
 * FILE       *fp;     input; pointer to file
 * TSyStatus  *status; output; return status
 *                     returns 4 bytes read
 */


/*----------------------------------------------------------------------------*/


GcfLongT GcfOrder4Bytes( unsigned char b[] );

/* Reorders 4 bytes read from stream to get longword
 *
 * parameters of routine
 * unsigned char b[];  input; 4 bytes read from file as char string
 *                     returns 4 bytes for longword
 */


/*----------------------------------------------------------------------------*/


void GcfBase36ToString( GcfLongT id, char str[] );

/* Converts base36 number to string.  Routine taken from Guralp manual.
 *
 * parameters of routine
 * GcfLongT id;       input; base36 number
 * char          str[];    output; decoded string
 */


/*----------------------------------------------------------------------------*/


void GcfDecodeTime( GcfTimeT gcftime, NTIME *ntime );

/* decodes GCF time
 *
 * parameters of routine
 * GcfTimeT   gcftime;      input; GCF encoded time
 * NTIME      *ntime;       output; numeric time
 */


/*---------------------------------------------------------------------*/


void GcfAcceptCapfiles( TSyBoolean on_off );

/* switches on/off whether capitalized filenames on $ROOT-paths are accepted
 *
 * parameters of routine
 * TSyBoolean    on_off;       input; switch
 */


/*----------------------------------------------------------------------------*/

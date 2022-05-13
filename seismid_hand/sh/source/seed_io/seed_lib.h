
/* file seed_lib.h
 *      ==========
 *
 * version 26, 21-Dec-2006
 *
 * header file of module seed_lib.c
 * K. Stammler, 29-Dec-93
 */




/*---------------------------------------------------------------------*/


void SeedLibInitialize( STATUS *status );

/* Initializes SEED library routines
 *
 * parameters of routine
 * STATUS     *status;      output; return status
 */


/*---------------------------------------------------------------------*/


void SeedDecodeSteim1( SeedSbyteT rec[], BOOLEAN swap, INT32 maxsmplth,
	INT32 smp[], INT32 *smplth );

/* Decodes Steim1-compressed data record of length 'seedv_setup.seed_rec_size'.
 * Data are returned in array 'smp', its length in 'smplth'.  No more than
 * maxsmplth samples are copied to the output array.  If more samples would
 * be available an error message is printed.
 * A NULL pointer for 'rec' resets internal variables to initial state.
 *
 * parameters of routine
 * SeedSbyteT rec[];          input; SEED record
 * BOOLEAN    swap;           input; swap bytes
 * INT32      maxsmplth;      input; maximum length of output array
 * INT32      smp[];          output; decoded samples
 * INT32      *smplth;        output; number of output samples
 */


/*---------------------------------------------------------------------*/


void SeedFindFileInSfd( char stream_str[], char start[], char sfdfile[],
	SeedFileDescrT *fdescr, STATUS *status );

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


/*---------------------------------------------------------------------*/


void SeedSearchPosition( int chan, char sfdfile[], char stream_str[],
	char req_start[], char act_start[], STATUS *status );

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


/*---------------------------------------------------------------------*/


void SeedReadNextRecord( int chan, SeedSbyteT *rec, BOOLEAN *hdr_swapped,
	STATUS *status );

/* Reads next available record from SEED file.  If the header must be
 * swapped this is done and indicated in '*hdr_swapped'.
 *
 * parameters of routine
 * int        chan;               input; channel number
 * SeedSbyteT *rec;               output; SEED record
 * BOOLEAN    *hdr_swapped;       output; tells whether header was swapped
 * STATUS     *status;            output; return status
 */


/*---------------------------------------------------------------------*/


float SeedGetSampleDist( SeedDataHeaderT *hdr );

/* Returns sample distance in sec.
 *
 * parameters of routine
 * SeedDataHeaderT  *hdr;        input; SEED data header
 *                               returns sample distance in sec
 */


/*---------------------------------------------------------------------*/


void SeedRecordTime( SeedSbyteT *rec, NTIME *start, NTIME *end,
	STATUS *status );

/* Returns start and end time of SEEd record
 *
 * parameters of routine
 * SeedSbyteT *rec;           input; SEED record
 * NTIME      *start;         output; start time of record
 * NTIME      *end;           output; end time of record
 * STATUS     *status;        output; return status
 */


/*---------------------------------------------------------------------*/


void SeedEncodeSteim1( int fd, INT32 smp[], INT32 *prev_smp, INT32 *idx,
	NTIME *ctime, SeedSbyteT *wrk, int wrklth, BOOLEAN complete, STATUS *status);

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
 * SeedSbyteT *wrk;          scratch; work space for encoding
 * NTIME      *ctime;        modify; input: start time, output: new start time
 * int        wrklth;        input; length of work space in records
 * BOOLEAN    complete;      input; finish up all records (-> *idx == 0)
 * STATUS     *status;       output; return status
 */


/*---------------------------------------------------------------------*/



void SeedEncodeSteim1Chan( int fd, INT32 smp[], INT32 *prev_smp, INT32 *idx,
	NTIME *ctime, int *reccnt, SeedDataHeaderT *seedhdr, SeedSbyteT *wrk,
	int wrklth, BOOLEAN complete, STATUS *status );

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


/*---------------------------------------------------------------------*/


void SeedSetHeaderPrototype( SeedDataHeaderT *hdr );

/* copies data header to global variables
 *
 * parameters of routine
 * SeedDataHeaderT *hdr;        input; data header prototype
 */


/*----------------------------------------------------------------------------*/


void SeedBtimeToNtime( SeedBtimeT *btime, NTIME *ntime, STATUS *status );

/* converts SeedBtimeT to NTIME
 *
 * parameters of routine
 * SeedBtimeT *bt;      input; SEED binary time
 * NTIME      *nt;      output; KS numeric time
 * STATUS     *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void SeedNtimeToBtime( NTIME *ntime, SeedBtimeT *btime, STATUS *status );

/* converts NTIME to BTIME
 *
 * parameters of routine
 * NTIME      *nt;      input; KS numeric time
 * BTIME      *bt;      output; SEED binary time
 * STATUS     *status;  output; return status
 */


/*---------------------------------------------------------------------*/


void SeedReadStream( int chan, char sfdfile[], char stream_str[],
	BOOLEAN swap, char req_start[], float seclth, INT32 **ldat,
	INT32 *smplth, char act_start[], float *dt, float *calib,
	int *flags, STATUS *status );

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


/*---------------------------------------------------------------------*/


void SeedReadStreamReal( int chan, char sfdfile[], char stream_str[],
	BOOLEAN swap, char req_start[], float seclth, float **fdat,
	INT32 *smplth, char act_start[], float *dt, float *calib,
	int *flags, STATUS *status );

/* Same as SeedReadStream except that a float array is returned
 *
 * parameters of routine
 * see SeedReadStream above
 */


/*---------------------------------------------------------------------*/


void SeedParseSfdLine( char line[], SeedFileDescrT *fd, STATUS *status );

/* parses line of SFD file.  Returns structure elements 'stream',
 * 't_start', 't_end', 'name', 'recno' and 'calib'.
 *
 * parameters of routine
 * char       line[];       input; SFD line
 * SeedFileDescrT *fd;      output; SEED file descriptor
 * STATUS     *status;      output; return status
 */


/*---------------------------------------------------------------------*/


void SeedGetTimeSpan( char sfdfile[], char stream[], char start[],
	char end[], STATUS *status );

/* Returns first and last time of channel 'stream' in sfd-file
 *
 * parameters of routine
 * char       sfdfile[];        input; name of sfd-file
 * char       stream[];         input; stream string to be checked
 * char       start[];          output; first start time
 * char       end[];            output; last end time
 * STATUS     *status;          output; return status
 */


/*---------------------------------------------------------------------*/


BOOLEAN SeedInquireTime( char sfdfile[], char stream[], char start[],
	char end[], STATUS *status );

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


/*---------------------------------------------------------------------*/


void SeedInquireTimeList( char sfdfile[], char stations[], int itemlth,
	int listlth, char chan[], char comp, char start[], char end[],
	INT32 *avail, STATUS *status );

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


/*---------------------------------------------------------------------*/


BOOLEAN SeedSwapNecessary( SeedDataHeaderT *hdr );

/* Checks whether a swap is necessary on data header
 *
 * parameters of routine
 * SeedDataHeaderT *hdr;     input; header to check
 *                           returns TRUE if swap must be performed
 */


/*---------------------------------------------------------------------*/


void SeedSwapHeader( SeedDataHeaderT *hdr );

/* Swaps words and longwords in header
 *
 * parameters of routine
 * SeedDataHeaderT *hdr;          modify; data heaer to be swapped
 */


/*---------------------------------------------------------------------*/


void SeedCloseFiles( void );

/* closes all open SEED files
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


unsigned short int SeedGet2Bytes( SeedSbyteT b[], BOOLEAN swap );

/* returns word from byte stream
 *
 * parameters of routine
 * SeedSbyteT b[];     input;   byte stream
 * BOOLEAN    swap;    input;   swap bytes
 */


/*----------------------------------------------------------------------------*/


unsigned INT32 SeedGet4Bytes( SeedSbyteT b[], BOOLEAN swap );

/* returns word from byte stream
 *
 * parameters of routine
 * SeedSbyteT b[];     input;   byte stream
 * BOOLEAN    swap;    input;   swap bytes
 */


/*---------------------------------------------------------------------*/


void SeedPrintWarnings( BOOLEAN on_off );

/* switches warning messages on or off
 *
 * parameters of routine
 * BOOLEAN    on_off;       input; switch
 */


/*---------------------------------------------------------------------*/


void SeedAcceptCapfiles( BOOLEAN on_off );

/* switches on/off whether capitalized filenames on $ROOT-paths are accepted
 *
 * parameters of routine
 * BOOLEAN    on_off;       input; switch
 */


/*---------------------------------------------------------------------*/


float SeedFindStreamCalibration( char stream[], char stime[], STATUS *status );

/* returns calibration value for counts for given stream 'stream' at time
 * 'stime'.
 *
 * parameters of routine
 * char           stream[];     input; stream string
 * char           stime[];      input; time
 * STATUS         *status;      output; return status
 *                              returns calibration or 0.0
 */


/*---------------------------------------------------------------------*/


BOOLEAN SeedSetInvertSwapHeader( BOOLEAN on_off );

/* Sets the global variable seedv_invert_swap_hdr to invert header swap
 * info in sfd-file.  Returns old value.
 *
 * parameters of routine
 * BOOLEAN    on_off;    input; switch value
 */


/*---------------------------------------------------------------------*/


void SeedCutStartRecord( SeedSbyteT *seedrec, char extime[], BOOLEAN swap,
	STATUS *status );

/* Throws away the first samples in record until given time
 *
 * parameters of routine
 * char       *seedrec;         modify; SEED record to cut
 * char       extime[];         input; exact time
 * BOOLEAN    swap;             input; swap bytes ?
 * STATUS     *status;          output; return status
 */


/*---------------------------------------------------------------------*/


BOOLEAN SeedSetDecodeErrorAbort( BOOLEAN on_off );

/* Sets the global value seedv_decerr_abort (abort on occurrence of
 * SEED decoding errors.  Returns old value.
 *
 * parameters of routine
 * BOOLEAN    on_off;        input; switch value
 */


/*---------------------------------------------------------------------*/


BOOLEAN SeedUseTimeCorrection( BOOLEAN on_off );

/* Sets the global value seedv_use_timecorr (use timec orrection field
 * within SEED data records).  Returns old value.
 *
 * parameters of routine
 * BOOLEAN    on_off;        input; switch value
 */


/*---------------------------------------------------------------------*/


void SeedCutEndRecord( SeedSbyteT *seedrec, char extime[], BOOLEAN swap,
	STATUS *status );

/* Throws away the last samples in record starting at given time
 *
 * parameters of routine
 * char       *seedrec;         modify; SEED record to cut
 * char       extime[];         input; exact time
 * BOOLEAN    swap;             input; swap bytes ?
 * STATUS     *status;          output; return status
 */


/*---------------------------------------------------------------------*/


char *SeedGetFilename( int chan );

/* returns name of currently opened file on channel 'chan'.  If no filename
 * is found, a pointer to an empty string is returned.
 *
 * parameters of routine
 * int        chan;        input; channel number
 *                         returns name of file
 */


/*---------------------------------------------------------------------*/


void SeedStoreReclth( SeedDataHeaderT *rec, int reclth );

/* Stores record length in SEED record.  This is no official solution.
 * Uses Reserved_bytes_A for this purpose.
 *
 * parameters of routine
 * SeedDataHeaderT   *rec;   modify;  SEED record
 */


/*---------------------------------------------------------------------*/


int SeedGetReclth( SeedDataHeaderT *rec, TSyBoolean clean );

/* Returns SEED record length stored in record (by SeedStoreReclth)
 * If 'clean' is TRUE, the record length info is deleted from the record.
 *
 * parameters of routine
 * SeedDataHeaderT  *rec;      modify; SEED record
 * TSyBoolean       clean;     input; if TRUE, record length info is deleted
 */


/*---------------------------------------------------------------------*/


void SeedSetOutputReclth( int reclth );

/* Sets size of SEED output records (default is 4096, if not called)
 *
 * parameters of routine
 * int        reclth;       input; size of output records in bytes
 */


/*---------------------------------------------------------------------*/


void SeedSetOutputReclth( int reclth );

/* Sets size of SEED output records (default is 4096, if not called)
 *
 * parameters of routine
 * int        reclth;       input; size of output records in bytes
 */


/*----------------------------------------------------------------------------*/


void SeedQuickFindReclth( FILE *fp, int *recsize, int *byteoff );

/* Returns record size in bytes or 0 and the byte offset where data records
 * start.
 *
 * parameters of routine
 * FILE          *fp;          input; input file already opened
 * int           *recsize;     output; record size found
 * int           *byteoff;     output; byte offset found
 */


/*---------------------------------------------------------------------*/


int SeedGetCompression( SeedDataHeaderT *rec );

/* Returns compression algorithm ID if blockette 1000 is found,
 * otherwise 0.
 *
 * parameters of routine
 * SeedDataHeaderT  *rec;      input; SEED record
 */


/*---------------------------------------------------------------------*/


int SeedGetTimequal( SeedDataHeaderT *rec );

/* Returns time quality or -1
 *
 * parameters of routine
 * SeedDataHeaderT   *rec;
 */


/*---------------------------------------------------------------------*/


int SeedGetWordOrder( SeedDataHeaderT *rec );

/* Returns word order ID if blockette 1000 is found,
 * otherwise 0.
 *
 * parameters of routine
 * SeedDataHeaderT  *rec;      input; SEED record
 */

/*---------------------------------------------------------------------*/

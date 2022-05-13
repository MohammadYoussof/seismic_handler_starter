/* file sh_len.h
 *      ========
 *
 * version 2, 14-Jun-93
 *
 * prototypes of ESSTF Lennartz binary reader for SH (foreign format input)
 * K. Stammler, 27-Aug-92
 * J.Zednik + M.Musil, 15-Jun-93
 */



/* status codes */
#define LEN_OFFSET 8400
#define LEN_OPENLEN     (LEN_OFFSET+1)   /* error opening ESSTF file */
#define LEN_READLEN     (LEN_OFFSET+2)   /* error reading ESSTF file */
#define LEN_TOOMANY     (LEN_OFFSET+3)   /* too many traces in ESSTF file */
#define LEN_ZEROSMP     (LEN_OFFSET+4)   /* zero sample rate found */
#define LEN_NAMEOVFL    (LEN_OFFSET+5)   /* station name too long */
#define LEN_NOCHECKSUM  (LEN_OFFSET+6)   /* no checksum found */
#define LEN_WRONGCHECK  (LEN_OFFSET+7)   /* checksum didn't match */
#define LEN_ILLTRC      (LEN_OFFSET+8)   /* illegal trace */
#define LEN_READTWICE   (LEN_OFFSET+9)   /* attempt to read trace twice */





/*----------------------------------------------------------------------------*/


void sh_len_trace( char file[], int rec, STATUS *status );

/* prepares ESSTF file for passing to SH
 *
 * parameters of routine
 * char       file[];    input; name of ESSTF file
 * int        rec;       input; number of record to read
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void sh_len_geti( char entryname[], long *info, STATUS *status );

/* returns integer value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * long       *info;           output; returned info
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------------*/


void sh_len_getr( char entryname[], float *info, STATUS *status );

/* returns float value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * float      *info;           output; returned info
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------------*/


void sh_len_gets( char entryname[], int maxlth, char info[], STATUS *status );

/* returns string value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * int        maxlth;          input; maximum length of output string
 * char       info[];          output; returned info
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------------*/


void sh_len_getc( char entryname[], char *info, STATUS *status );

/* returns character value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * char       *info;           output; returned info
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------*/


void sh_len_read( float smp[] );

/* returns sample data
 *
 * parameter of routine
 * float     smp[];     output; sample data
 */


/*----------------------------------------------------------------------*/



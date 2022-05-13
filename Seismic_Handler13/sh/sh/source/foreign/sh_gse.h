
/* file sh_gse.h
 *      ========
 *
 * version 3, 20-Jun-94
 *
 * prototypes of GSE reader for SH (foreign format input)
 * K. Stammler, 27-Aug-92
 */



/* status codes */
#define GSE_OFFSET 8100
#define GSE_OPENGSE     (GSE_OFFSET+1)   /* error opening GSE file */
#define GSE_READGSE     (GSE_OFFSET+2)   /* error reading GSE file */
#define GSE_TOOMANY     (GSE_OFFSET+3)   /* too many traces in GSE file */
#define GSE_ZEROSMP     (GSE_OFFSET+4)   /* zero sample rate found */
#define GSE_NAMEOVFL    (GSE_OFFSET+5)   /* station name too long */
#define GSE_NOCHECKSUM  (GSE_OFFSET+6)   /* no checksum found */
#define GSE_WRONGCHECK  (GSE_OFFSET+7)   /* checksum didn't match */
#define GSE_ILLTRC      (GSE_OFFSET+8)   /* illegal trace */
#define GSE_READTWICE   (GSE_OFFSET+9)   /* attempt to read trace twice */
#define GSE_NOSMPRATE   (GSE_OFFSET+10)  /* no sample rate specified */
#define GSE_NOCALIB     (GSE_OFFSET+11)  /* no calibration specified */
#define GSE_ZEROLTH     (GSE_OFFSET+12)  /* zero length trace */





/*----------------------------------------------------------------------------*/


void sh_gse_trace( char file[], int rec, STATUS *status );

/* prepares GSE file for passing to SH
 *
 * parameters of routine
 * char       file[];    input; name of GSE file
 * int        rec;       input; number of record to read
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void sh_gse_geti( char entryname[], long *info, STATUS *status );

/* returns integer value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * long       *info;           output; returned info
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------------*/


void sh_gse_getr( char entryname[], float *info, STATUS *status );

/* returns float value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * float      *info;           output; returned info
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------------*/


void sh_gse_gets( char entryname[], int maxlth, char info[], STATUS *status );

/* returns string value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * int        maxlth;          input; maximum length of output string
 * char       info[];          output; returned info
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------------*/


void sh_gse_getc( char entryname[], char *info, STATUS *status );

/* returns character value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * char       *info;           output; returned info
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------*/


void sh_gse_read( float smp[] );

/* returns sample data
 *
 * parameter of routine
 * float     smp[];     output; sample data
 */


/*----------------------------------------------------------------------------*/



/* file SHFRGN.H
 *      ========
 *
 * version 1, 2-aug-91
 *
 * prototypes of module SH_READFRGN.C
 * K. Stammler, 2-aug-91
 */


/*----------------------------------------------------------------------*/


void sh_frgn_trace( int format, char file[], int rec, STATUS *status );

/* switches to new trace
 *
 * parameters of routine
 * int        format;      input; format ID
 * char       file[];      input; name of input file
 * int        rec;         input; record number
 * STATUS     *status;     output; return status
 */


/*----------------------------------------------------------------------*/


void sh_frgn_geti( char entryname[], long *info, STATUS *status );

/* returns long integer info entry
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * long       *info;           output; info returned
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------*/


void sh_frgn_getr( char entryname[], REAL *info, STATUS *status );

/* returns float info entry
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * REAL       *info;           output; info returned
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------*/


void sh_frgn_gets( char entryname[], int maxlth, char info[], STATUS *status );

/* returns long integer info entry
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * int        maxlth;          input; maximum length of output string
 * char       info[];          output; info returned
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------*/


void sh_frgn_getc( char entryname[], char *info, STATUS *status );

/* returns character info entry
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * char       *info;           output; info returned
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------*/


void sh_frgn_read( SAMPLE smp[] );

/* returns sample data
 *
 * parameter of routine
 * SAMPLE    smp[];     output; sample data
 */


/*----------------------------------------------------------------------*/

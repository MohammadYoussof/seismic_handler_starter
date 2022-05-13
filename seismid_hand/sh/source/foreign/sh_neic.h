
/* file SH_NEIC.H
 *      =========
 *
 * version 1, 2-aug-91
 *
 * prototypes of module SH_NEIC.C
 * K. Stammler, 2-aug-91
 */


/*----------------------------------------------------------------------*/


void sh_neic_trace( char file[], int rec, STATUS *status );

/* switches to new trace
 *
 * parameters of routine
 * char       file[];      input; name of input file
 * int        rec;         input; record number
 * STATUS     *status;     output; return status
 */


/*----------------------------------------------------------------------*/


void sh_neic_geti( char entryname[], long *info, STATUS *status );

/* returns long integer info entry
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * long       *info;           output; info returned
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------*/


void sh_neic_getr( char entryname[], float *info, STATUS *status );

/* returns float info entry
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * float      *info;           output; info returned
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------*/


void sh_neic_gets( char entryname[], int maxlth, char info[], STATUS *status );

/* returns long integer info entry
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * int        maxlth;          input; maximum length of output string
 * char       info[];          output; info returned
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------*/


void sh_neic_getc( char entryname[], char *info, STATUS *status );

/* returns character info entry
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * char       *info;           output; info returned
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------*/


void sh_neic_read( float smp[] );

/* returns sample data
 *
 * parameter of routine
 * float     smp[];     output; sample data
 */


/*----------------------------------------------------------------------*/

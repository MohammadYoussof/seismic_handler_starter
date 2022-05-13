
/* file sh_ah.h
 *      =======
 *
 * version 4, 5-Dec-2001
 *
 * header file of module sh_ah.c
 * K. Stammler, 15-Dec-92
 */


#ifdef BC_FRGN_AH


#ifndef __SH_AH
#define __SH_AH


/* status codes */
#define AHE_OFFSET     6200
#define AHE_OPENRD     (AHE_OFFSET+1)    /* error opening input file */
#define AHE_NOMOREHD   (AHE_OFFSET+2)    /* no more header */
#define AHE_SKIPERR    (AHE_OFFSET+3)    /* skip error */
#define AHE_MEMOVFL    (AHE_OFFSET+4)    /* memory overflow */
#define AHE_READERR    (AHE_OFFSET+5)    /* read error of AH file */
#define AHE_ILLAHTYPE  (AHE_OFFSET+6)    /* illegal AH type */
#define AHE_UKAHTYPE   (AHE_OFFSET+7)    /* unknown AH type */



/*----------------------------------------------------------------------------*/


void sh_ah_trace( char file[], int rec, STATUS *status );

/* reads trace number "rec" from AH file "file" into memory
 *
 * parameters of routine
 * char       file[];      input; name of AH file
 * int        rec;         input; position of trace in file
 * STATUS     *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void sh_ah_geti( char entryname[], long *info, STATUS *status );

/* return integer info entry value
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * long       *info;           output; info returned
 * STATUS     *status;         ouput; return status
 */


/*----------------------------------------------------------------------------*/


void sh_ah_getr( char entryname[], float *info, STATUS *status );

/* return float info entry value
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * float      *info;           output; info returned
 * STATUS     *status;         ouput; return status
 */


/*----------------------------------------------------------------------------*/


void sh_ah_gets( char entryname[], int maxlth, char info[], STATUS *status );

/* return string info entry value
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * int        maxlth;          input; maximum length of output string
 * char       info[];          output; info returned
 * STATUS     *status;         ouput; return status
 */


/*----------------------------------------------------------------------------*/


void sh_ah_getc( char entryname[], char *info, STATUS *status );

/* return single char info entry value
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * char       *info;           output; info returned
 * STATUS     *status;         ouput; return status
 */


/*----------------------------------------------------------------------------*/


void sh_ah_read( float smp[] );

/* returns sample data
 *
 * parameters of routine
 * float      smp[];     output; sample data
 */


/*----------------------------------------------------------------------------*/


#endif /* __SH_AH */


#else

#define sh_ah_trace(f,r,s)
#define sh_ah_geti(n,i,s)
#define sh_ah_getr(n,i,s)
#define sh_ah_gets(n,m,i,s)
#define sh_ah_getc(n,i,s)
#define sh_ah_read(s)

#endif /* BC_FRGN_AH */

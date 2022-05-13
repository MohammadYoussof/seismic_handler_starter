
/* file MFUSRDEF.H
 *      ==========
 *
 * version 3, 24-May-92
 *
 * user interface of module MEMFILE
 * K. Stammler, 31-AUG-91
 */

#ifndef __MFUSRDEF
#define __MFUSRDEF

#define MFC_MAXMF 8
	/* maximum number of memory files */
#define MFC_REAL float


/*-------------------------------------------------------------------------*/


void mf_open( unsigned id, BOOLEAN mem, char *access, int *status );

/* open file number "id"
 *
 * parameters of routine
 * unsigned   id;        input; file number
 * BOOLEAN    mem;       input; memory file (TRUE) or disk file (FALSE)
 * char       *access;   input; read/write access;
 * int        *status;   output; return status
 */


/*-------------------------------------------------------------------------*/


void mf_close( unsigned id, int *status );

/* closes memory file
 *
 * parameters of routine
 * unsigned     id;      input; file ID
 * int          *status; output; return status
 */


/*-------------------------------------------------------------------------*/


void mf_rewind( unsigned id, int *status );

/* rewinds file
 *
 * parameters of routine
 * unsigned   id;         input; file ID
 * int        *status;    output; return status
 */


/*-------------------------------------------------------------------------*/


void mf_delete( unsigned id, int *status );

/* deletes file
 *
 * parameters of routine
 * unsigned   id;        input;  file ID
 * int        *status;   output; return status
 */


/*-------------------------------------------------------------------------*/


BOOLEAN mf_isopen( unsigned id );

/* returns whther or not file "id" is opened
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 *                     returns TRUE if file is opened
 */


/*-------------------------------------------------------------------------*/


void mf_writereal( unsigned id, MFC_REAL r, int *status );

/* write real number to file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * MFC_REAL  r;        input; number
 * int       *status;  output; return status
 */


/*-------------------------------------------------------------------------*/


void mf_writeint( unsigned id, int i, int *status );

/* write integer number to file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * int       i;        input; number
 * int       *status;  output; return status
 */


/*-------------------------------------------------------------------------*/


void mf_writelong( unsigned id, long l, int *status );

/* write long integer number to file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * long      l;        input; number
 * int       *status;  output; return status
 */


/*-------------------------------------------------------------------------*/


void mf_writechar( unsigned id, char c, int *status );

/* write integer number to file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * char      c;        input; character
 * int       *status;  output; return status
 */


/*-------------------------------------------------------------------------*/


void mf_writestr( unsigned id, char *str, int *status );

/* writes string to file
 *
 * parameters of routine
 * unsigned   id;        input; file ID
 * char       *str;      input; sring to be written
 * int        *status;   output; return status
 */


/*-------------------------------------------------------------------------*/


MFC_REAL mf_readreal( unsigned id, int *status );

/* read real number from file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * int       *status;  output; return status
 *                     returns read value
 */


/*-------------------------------------------------------------------------*/


int mf_readint( unsigned id, int *status );

/* read integer number from file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * int       *status;  output; return status
 *                     returns read value
 */


/*-------------------------------------------------------------------------*/


long mf_readlong( unsigned id, int *status );

/* read long integer number from file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * int       *status;  output; return status
 *                     returns read value
 */


/*-------------------------------------------------------------------------*/


char mf_readchar( unsigned id, int *status );

/* read character from file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * int       *status;  output; return status
 *                     returns read value
 */


/*-------------------------------------------------------------------------*/


void mf_readstr( unsigned id, char *str, int *status );

/* reads string from file
 *
 * parameters of routine
 * unsigned   id;        input; file ID
 * char       *str;      output; read string
 * int        *status;   output; return status
 */


/*-------------------------------------------------------------------------*/


/* errors */
#define MFE_NOERROR  0
#define MFE_OFFSET   3900
#define MFE_ILID     (MFE_OFFSET+1)   /* illegal file ID */
#define MFE_OVWR     (MFE_OFFSET+2)   /* file ID already used */
#define MFE_STROVFL  (MFE_OFFSET+3)   /* string overflow */
#define MFE_OPNWR    (MFE_OFFSET+4)   /* error opening output file */
#define MFE_DELOPN   (MFE_OFFSET+5)   /* attempt to delete open file */
#define MFE_BUFOVFL  (MFE_OFFSET+6)   /* buffer overflow */
#define MFE_NOTOPEN  (MFE_OFFSET+7)   /* file not opened */
#define MFE_ENDMEM   (MFE_OFFSET+8)   /* end of memfile found during read */

#endif

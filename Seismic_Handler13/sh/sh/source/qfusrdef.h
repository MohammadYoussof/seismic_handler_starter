
/* file QFUSRDEF.H
 *      ==========
 *
 * version 6, 22-May-2006
 *
 * prototypes of module QFILEIO
 * K. Stammler, 22-MAY-91
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1996,  Klaus Stammler, Federal Institute for Geosciences
 *                                       and Natural Resources (BGR), Germany
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */


#ifndef __QFUSRDEF
#define __QFUSRDEF

/* old values up to 15-Mar-2006 */
#ifdef XXX
#define QFC_IENTRY 50   /* number of integer entries */
#define QFC_LENTRY 20   /* number of long entries */
#define QFC_RENTRY 50   /* number of float entries */
#define QFC_SENTRY 30   /* number of string entries */
#define QFC_CENTRY 16   /* number of character entries */
#endif

#define QFC_IENTRY 500   /* number of integer entries */
#define QFC_LENTRY 200   /* number of long entries */
#define QFC_RENTRY 500   /* number of float entries */
#define QFC_SENTRY 300   /* number of string entries */
#define QFC_CENTRY 160   /* number of character entries */

#define QFC_LEMPTY 0x7fffffffL /* empty value for long entries */
#define QFC_IEMPTY 0x7fff      /* empty value for int entries */
/* #define QFC_REMPTY 1.23456e13  */ /* empty value for float entries */

#define QFC_LTYPE  0x1000      /* long info type */
#define QFC_ITYPE  0x2000      /* int info type */
#define QFC_RTYPE  0x3000      /* float info type */
#define QFC_STYPE  0x4000      /* string info type */
#define QFC_CTYPE  0x5000      /* int info type */

typedef float DATATYPE;     /* data type */

#define QFCL_POS (0|QFC_LTYPE)  /* position of trace data in binary file */
#define QFCL_LTH (1|QFC_LTYPE)  /* length of trace data in samples */


/* prototypes of routines in module QFILEIO.C */


/*------------------------------------------------------------------------*/


void qf_read( char *qfilnam, int trcno, BOOLEAN readbin,
				  DATATYPE *(*datptr), STATUS *status );

/* reads trace header number "trcno" in file "qfilnam" and sets global
info block ib_qfv.  If readbin=TRUE memory is allocated and the sample
data are read into this allocated memory. */

/* parameters of routine */
/* char     *qfilnam;       input; name of q-file to be read (no ext.) */
/* int      trcno;          input; number of trace to be read */
/* int      readbin;        input; read binary file or header only */
/* DATATYPE *(*datptr);     output; pointer to binary data read */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void qf_read_wdw( char *qfilnam, int trcno, long offset, long lth,
	DATATYPE **datptr, STATUS *status );

/* reads trace header number "trcno" in file "qfilnam" and sets global
 * info block ib_qfv.  Reads also window of sample data into new allocated
 * memory.
 *
 * parameters of routine
 * char     *qfilnam;     input; name of q-file to be read (no ext.)
 * int      trcno;        input; number of trace to be read
 * long     offset;       input; offset to trace start in samples
 * long     lth;          input; number of data samples to be read
 * DATATYPE **datptr;     output; pointer to binary data read
 * STATUS   *status;      output; return status
 */


/*------------------------------------------------------------------------*/


void qf_write( char *qfilnam, DATATYPE *datptr, int *record,
	STATUS *status );

/* writes header set by qf_set%-routines to header file and writes data
samples specified by pointer "datptr" to binary file */

/* parameters of routine */
/* char     *qfilnam;       input; name of q-file */
/* DATATYPE *datptr;        input; pointer to data array */
/* int      *record;        output; record number of written data */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void qf_rclose( STATUS *status );

/* closes input q-file */

/* parameters of routine */
/* int      *status;     output; return status */


/*------------------------------------------------------------------------*/


void qf_wclose( STATUS *status );

/* closes output q-file */

/* parameters of routine */
/* int      *status;     output; return status */


/*------------------------------------------------------------------------*/


void qf_change( char *qfilnam, int *reclist, int listlth, int idx,
					 void *value, int trcadd, STATUS *status );

/* changes the value of an info entry in the file "qfilnam" on records
"reclist" to the value "*value" */

/* parameters of routine */
/* char     *qfilnam;       input; name of q-file */
/* int      *reclist;       input; list of record numbers */
/* int      listlth;        input; length of list */
/* int      idx;            input; info index */
/* void     *value;         input; pointer to info */
/* int      trcadd;         input; increment of lines/trace */
/* STATUS   *status;        output; return status */


/*------------------------------------------------------------------------*/


void qf_comment( char **cmt, int linecnt );

/* sets the comment text used by all following qf_newhdr calls (when a
	new q-file is created) */

/* parameters of routine */
/* char     **cmt;          input; pointers to text lines */
/* int      linecnt;        input; number of lines */


/*------------------------------------------------------------------------*/


void qf_insertcmt( char qfile[], char cmtfile[], STATUS *status );

/* inserts file "cmtfile" as a comment to the header of q-file
 * "qfile"
 *
 * parameters of routine
 * char       qfile[];    input; name of q-file to insert comment
 * char       cmtfile[];  input; name of file to be included
 * STATUS     *status;    output; return status
 */


/*------------------------------------------------------------------------*/


void qf_inihdr( void );

/* initialises header for writing */

/* no parameters */


/*------------------------------------------------------------------------*/


void qf_insinfo( int idx, void *inf, STATUS *status );

/* appends an info value of arbitrary type to the output header */

/* parameters of routine */
/* int      idx;            input; info index */
/* void     *inf;           input; info to append */
/* STATUS   *status;        output; return status */


/*------------------------------------------------------------------------*/


void qf_setl( int idx, long inf, STATUS *status );

/* appends long integer info to global whdr_qfv */

/* parameters of routine */
/* int      idx;            input; info index */
/* long     inf;            input; info to be appended */
/* STATUS   *status;        output; return status */


/*------------------------------------------------------------------------*/


void qf_seti( int idx, int inf, STATUS *status );

/* appends integer info to global whdr_qfv */

/* parameters of routine */
/* int      idx;            input; info index */
/* int      inf;            input; info to be appended */
/* STATUS   *status;        output; return status */


/*------------------------------------------------------------------------*/


void qf_setr( int idx, float inf, STATUS *status );

/* appends float info to global whdr_qfv */

/* parameters of routine */
/* int      idx;            input; info index */
/* float    inf;            input; info to be appended */
/* STATUS   *status;        output; return status */


/*------------------------------------------------------------------------*/


void qf_sets( int idx, char *inf, STATUS *status );

/* appends string info to global whdr_qfv */

/* parameters of routine */
/* int      idx;            input; info index */
/* char     *inf;           input; info to be appended */
/* STATUS   *status;        output; return status */


/*------------------------------------------------------------------------*/


void qf_setc( int idx, char inf, STATUS *status );

/* appends char info to global whdr_qfv */

/* parameters of routine */
/* int      idx;            input; info index */
/* char     inf;            input; info to be appended */
/* STATUS   *status;        output; return status */


/*------------------------------------------------------------------------*/


long qf_getl( int idx, STATUS *status );

/* returns long values from last read header */

/* parameters of routine */
/* int      idx;            input; info index */
/* STATUS   *status;        output; return status */


/*------------------------------------------------------------------------*/


int qf_geti( int idx, STATUS *status );

/* returns int values from last read header */

/* parameters of routine */
/* int      idx;            input; info index */
/* STATUS   *status;        output; return status */


/*------------------------------------------------------------------------*/


float qf_getr( int idx, STATUS *status );

/* returns float values from last read header */

/* parameters of routine */
/* int      idx;            input; info index */
/* STATUS   *status;        output; return status */


/*------------------------------------------------------------------------*/


char *qf_gets( int idx, STATUS *status );

/* returns string pointer from last read header */

/* parameters of routine */
/* int      idx;            input; info index */
/* STATUS   *status;        output; return status */


/*------------------------------------------------------------------------*/


char qf_getc( int idx, STATUS *status );

/* returns char values from last read header */

/* parameters of routine */
/* int      idx;            input; info index */
/* STATUS   *status;        output; return status */


/*------------------------------------------------------------------------*/


int qf_risempty( float *f );

/* checks whether f is empty or not */

/* parameters of routine */
/* float		  *f;			 input; real number to be checked */
/*								 returns TRUE if f is empty */


/*------------------------------------------------------------------------*/


void qf_rsetempty( float *f );

/* sets f to empty value */

/* parameters of routine */
/* float			  *f;		output; number set to empty value */


/*------------------------------------------------------------------------*/


void qf_size( char *qfilnam, long *size, int *trcno, int *trclin,
	STATUS *status );

/* returns the size of a q-binary-file in bytes or -1 if not found
 *
 * parameters of routine
 * char     *qfilnam;        input; name of q-file
 * long     *size;           output; size determined
 * int      *trcno;          output; number of traces in q-file
 * int      *trclin;         output; number of lines per trace
 * STATUS   *status;         output; return status
 */


/*------------------------------------------------------------------------*/


void qf_defaultdir( char hdir[], char ddir[], STATUS *status );

/* sets default directories for header and binary files.  A NULL-parameter
 * disables default directory.  A " "-parameter leaves the default unchanged.
 *
 * parameters of routine
 * char       hdir[];     input; default directory for header files
 * char       ddir[];     input; default directory for binary files
 * STATUS     *status;    output; return status
 */


/*------------------------------------------------------------------------*/


void qf_byteswap( BOOLEAN swap );

/* set byte swap on or off
 *
 * parameter of routine
 * BOOLEAN      swap;      input; set byteswap TRUE or FALSE
 */


/*------------------------------------------------------------------------*/

#endif /* __QFUSRDEF */

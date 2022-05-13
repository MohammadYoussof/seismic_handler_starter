
/* File QFILEIO.C
 *      =========
 *
 * version 12, 22-May-2006
 *
 * Routines for Q-file input/output
 * K. Stammler, 21-FEB-1990
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1992,  Klaus Stammler, Federal Institute for Geosciences
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


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include BC_SYSBASE
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#ifdef BC_INC_UNISTD
#include BC_INC_UNISTD
#endif
#include "qferrors.h"
#include "qfusrdef.h"


/* global types */
#define QFC_SLTH 80     /* maximum length of each string entry in chars */
#define QFC_HLINELTH 79 /* maximum length of lines in header file */
typedef struct qfildscr {
	int    cmtlines;         /* no of comment lines at beginning of q-file */
	int    trclines;         /* no of header lines per trace */
	int    currtrc;          /* current trace header (recently read) */
	int    binopen;          /* binary file opened ? */
	FILE   *qhdr;            /* header q-file handle */
	BFILE  qbin;             /* data q-file handle */
	char   name[QFC_HLINELTH+1]; /* q-filename */
} QFILDSCR;
typedef struct infblc {
	long        li[QFC_LENTRY];
	int         ii[QFC_IENTRY];
	float       ri[QFC_RENTRY];
	char        si[QFC_SENTRY][QFC_SLTH];
	char        ci[QFC_CENTRY];
} INFBLC;


/* global constants */
#define QMAGIC 0xABCD      /* magic number of q-files */
#define HDRFILEXT ".QHD"   /* extension of header files */
#define BINFILEXT ".QBN"   /* extension of data files */
#define TMPFILEXT ".QXX"   /* extension of temporary files */
#define SKIPCH 3           /* number of chars skipped at each hdr line */
#define ENTDLTH 5          /* length of each entry descriptor in chars */
#define ENTNLTH 3          /* length of entry number in chars */
#define ENTDEND ':'        /* end character of entry descriptor */
#define ENTTERM '~'        /* entry termination character */
#define MAXHDRLTH 2000     /* maximum length of header in chars */
#define MINTRCLIN 4        /* minimum number of lines per trace in q-hdr */
#define IDXMASK 0x0fff     /* mask for index determination */
#define TYPMASK 0xf000     /* mask for type determination */
#define POSIDX (QFCL_POS&IDXMASK)  /* index number of position */
#define LTHIDX (QFCL_LTH&IDXMASK)  /* index number of length */
#ifdef XXX
/* this was not portable, K.S. 17-Mar-2006 */
#define R_EMPTY 0xffffffffL
#endif
#define R_EMPTY     5.37531e-38
#define R_EMPTY_MIN 5.375e-38
#define R_EMPTY_MAX 5.376e-38

/* global variables */
static QFILDSCR  fr_qfv={0,0,-1,FALSE};  /* read file descriptor */
static QFILDSCR  fw_qfv={0,0,-1,FALSE};  /* write file descriptor */
static char      whdr_qfv[MAXHDRLTH+1];  /* header to be written next */
static int       whlth_qfv;              /* current length of header */
static INFBLC    ib_qfv;                 /* info block filled by qf_read */
static char      *(*cmt_qfv);            /* pointer to comment */
static int       cmtlin_qfv = {0};       /* number of comment lines */
static long      wrtlth_qfv = QFC_LEMPTY;/* output lth for next qf_write */
static BOOLEAN   qfv_byteswap=FALSE;     /* swap bytes after reading */
static char      qfv_hdir[BC_FILELTH+1]; /* default header directory */
static char      qfv_ddir[BC_FILELTH+1]; /* default binary directory */


/* prototypes of local routines */
void qf_opnhdr( QFILDSCR *qf, char *qfilnam, int *status );
void qf_wophdr( QFILDSCR *qf, char *qfilnam, int *status );
void qf_wopbin( QFILDSCR *qf, char *qfilnam, int *status );
void qf_close( QFILDSCR *qf, int *status );
void qf_skiphdr( QFILDSCR *qf, int cnt, int *status );
void qf_rdhdr( QFILDSCR *qf, char *hdr, int *lth, int *status );
void qf_rdbin( QFILDSCR *qf, long pos, long lth, DATATYPE *(*ptr), int *status );
void qf_parsehdr( char *hdr, int lth, INFBLC *ib, int *status );
void qf_newhdr( QFILDSCR *qf, char *qfilnam, int cmtlin, char *cmt[], int trclin, int *status );
void qf_wrhdr( QFILDSCR *qf, int hdrcnt, int *status );
void qf_wrbin( QFILDSCR *qf, DATATYPE *datptr, long datlth, int *status );
void qf_iniqf( QFILDSCR *qf );
void qf_getnum( char *str, int num, int lth );
void qf_setb( INFBLC *ib, int idx, void *inf, int *status );
void qf_ibtostr( INFBLC *ib, int *status );
int qf_numinset( int num, int set[], int lth );
void qf_eql(long *dst, long *src);
void qf_eqi(int *dst, int *src);
void qf_eqr(float *dst, float *src);
void qf_dumpib( INFBLC *ib );
void qf_dumv( void );
void qf_dobyteswap( BYTE b[], long lth );
void qf_replchar( char str[], char find, char repl );


/*------------------------------------------------------------------------*/



void qf_read( char *qfilnam, int trcno, BOOLEAN readbin,
	DATATYPE **datptr, STATUS *status )

/* reads trace header number "trcno" in file "qfilnam" and sets global
 * info block ib_qfv.  If readbin=TRUE memory is allocated and the sample
 * data are read into this allocated memory.
 *
 * parameters of routine
 * char     *qfilnam;     input; name of q-file to be read (no ext.)
 * int      trcno;        input; number of trace to be read
 * BOOLEAN  readbin;      input; read binary file or header only
 * DATATYPE **datptr;     output; pointer to binary data read
 * STATUS   *status;      output; return status
 */
{
	/* local variables */
	char      rhdr[MAXHDRLTH+1];    /* header read by qf_rdhdr */
	int       hdrlth;               /* length of header */

	/* executable code */

	if  (trcno <= 0)  {
		*status = QFE_ILLTRCNO;
		return;
	} /*endif*/

	if  (fr_qfv.currtrc == -1)  {            /* header file not yet opened */
		qf_opnhdr( &fr_qfv, qfilnam, status );
		if  (*status != QFE_NOERROR)  return;
	} else if  ((strcmp(qfilnam,fr_qfv.name) != 0) ||
		(trcno < fr_qfv.currtrc))  {           /* other file opened or de-  */
		qf_close( &fr_qfv, status );           /* sired trace number smaller*/
		if  (*status != QFE_NOERROR)  return;  /* than current trace        */
		qf_opnhdr( &fr_qfv, qfilnam, status );
		if  (*status != QFE_NOERROR)  return;
	} /*endif*/

	/* now the correct filename is opened and */
	/* positioned at trace # fr_qfv.currtrc   */

	if  (fr_qfv.currtrc < trcno)  {                 /* if current trace    */
		qf_skiphdr( &fr_qfv, trcno-fr_qfv.currtrc-1, status );  /* number   */
		if  (*status != QFE_NOERROR)  return;        /* is too small, skip  */
		qf_rdhdr( &fr_qfv, rhdr, &hdrlth, status );  /* headers and read    */
		if  (*status != QFE_NOERROR)  return;        /* desired trace       */
		qf_parsehdr( rhdr, hdrlth, &ib_qfv, status );
		if  (*status != QFE_NOERROR)  return;
	} /*endif*/

	if  (!readbin)  return;     /* return if binary data shouldn't be read */

	if  ((ib_qfv.li[POSIDX] == QFC_LEMPTY) ||   /* if position or length */
		(ib_qfv.li[LTHIDX] == QFC_LEMPTY))  {    /* isn't specified it    */
		*status = QFE_MISSENT;                     /* can't be read         */
		return;
	} /*endif*/
	qf_rdbin( &fr_qfv, ib_qfv.li[POSIDX], ib_qfv.li[LTHIDX],
		datptr, status );

} /* end of qf_read */



/*------------------------------------------------------------------------*/



void qf_read_wdw( char *qfilnam, int trcno, long offset, long lth,
	DATATYPE **datptr, STATUS *status )

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
{
	/* local variables */
	char      rhdr[MAXHDRLTH+1];    /* header read by qf_rdhdr */
	int       hdrlth;               /* length of header */

	/* executable code */

	if  (trcno <= 0)  {
		*status = QFE_ILLTRCNO;
		return;
	} /*endif*/

	if  (fr_qfv.currtrc == -1)  {            /* header file not yet opened */
		qf_opnhdr( &fr_qfv, qfilnam, status );
		if  (*status != QFE_NOERROR)  return;
	} else if  ((strcmp(qfilnam,fr_qfv.name) != 0) ||
		(trcno < fr_qfv.currtrc))  {           /* other file opened or de-  */
		qf_close( &fr_qfv, status );           /* sired trace number smaller*/
		if  (*status != QFE_NOERROR)  return;  /* than current trace        */
		qf_opnhdr( &fr_qfv, qfilnam, status );
		if  (*status != QFE_NOERROR)  return;
	} /*endif*/

	/* now the correct filename is opened and */
	/* positioned at trace # fr_qfv.currtrc   */

	if  (fr_qfv.currtrc < trcno)  {                 /* if current trace    */
		qf_skiphdr( &fr_qfv, trcno-fr_qfv.currtrc-1, status );  /* number   */
		if  (*status != QFE_NOERROR)  return;        /* is too small, skip  */
		qf_rdhdr( &fr_qfv, rhdr, &hdrlth, status );  /* headers and read    */
		if  (*status != QFE_NOERROR)  return;        /* desired trace       */
		qf_parsehdr( rhdr, hdrlth, &ib_qfv, status );
		if  (*status != QFE_NOERROR)  return;
	} /*endif*/

	if  ((ib_qfv.li[POSIDX] == QFC_LEMPTY) ||   /* if position or length */
		(ib_qfv.li[LTHIDX] == QFC_LEMPTY))  {    /* isn't specified it    */
		*status = QFE_MISSENT;                     /* can't be read         */
		return;
	} /*endif*/
	if  (offset < 0  ||  lth < 0  ||  (offset+lth) > ib_qfv.li[LTHIDX])  {
		*status = QFE_ILSMPWDW;
		return;
	} /*endif*/
	qf_rdbin( &fr_qfv, ib_qfv.li[POSIDX]+offset, lth, datptr, status );

} /* end of qf_read_wdw */



/*------------------------------------------------------------------------*/



void qf_write( char *qfilnam, DATATYPE *datptr, int *record,
	STATUS *status )

/* writes header set by qf_set%-routines to header file and writes data
 * samples specified by pointer "datptr" to binary file
 *
 * parameters of routine
 * char     *qfilnam;     input; name of q-file
 * DATATYPE *datptr;      input; pointer to data array
 * int      *record;      output; record number of written data
 * STATUS   *status;      output; return status
 */
{
	/* local variables */
	static long   currpos;   /* current write position */
	static int    currrec;   /* last written record number */
	int           trclin;    /* number of lines per trace */

	/* executable code */

	/* maybe file is already opened for reading, so close input q-file */
	qf_rclose( status );
	if  (Severe(status))  return;

	if  ((fw_qfv.currtrc == -1) || (strcmp(qfilnam,fw_qfv.name) != 0))  {
		if  (fw_qfv.currtrc != -1)  {   /* close other q-file */
			qf_close( &fw_qfv, status );
			if  (*status != QFE_NOERROR)  return;
		} /*endif*/
		qf_size( qfilnam, &currpos, &currrec, &trclin, status );
		if  (*status == QFE_OPNHDR)  {  /* q-file doesn't exist */
			*status  = QFE_NOERROR;
			trclin = (whlth_qfv+ENTDLTH+1+10)/(QFC_HLINELTH-SKIPCH-2) + 2;
			if  (trclin < MINTRCLIN)  trclin = MINTRCLIN;
			qf_newhdr( &fw_qfv, qfilnam, cmtlin_qfv+1, cmt_qfv, trclin,status);
			if  (*status != QFE_NOERROR)  return;
			currpos = 0L;
			currrec = 0;
		} else if  (*status == QFE_NOERROR)  {  /* q-file exists already */
			qf_wophdr( &fw_qfv, qfilnam, status );
			if  (*status != QFE_NOERROR)  return;
		} else {
			return;
		} /*endif*/
		qf_wopbin( &fw_qfv, qfilnam, status );
		if  (*status != QFE_NOERROR)  return;
	} /*endif*/

	*record = ++currrec;
	qf_setl( QFCL_POS, currpos, status );
	if  (*status != QFE_NOERROR)  return;
	qf_wrhdr( &fw_qfv, currrec, status );
	if  (*status != QFE_NOERROR)  return;
	qf_wrbin( &fw_qfv, datptr, wrtlth_qfv, status );
	if  (*status != QFE_NOERROR)  return;

	currpos += wrtlth_qfv;

} /* end of qf_write */



/*------------------------------------------------------------------------*/



void qf_rclose( STATUS *status )

/* closes input q-file
 *
 * parameters of routine
 * STATUS   *status;     output; return status
 */
{
	/* executable code */
	qf_close( &fr_qfv, status );

} /* end of qf_rclose */



/*------------------------------------------------------------------------*/



void qf_wclose( STATUS *status )

/* closes output q-file
 *
 * parameters of routine
 * STATUS   *status;     output; return status
 */
{
	/* executable code */
	qf_close( &fw_qfv, status );

} /* end of qf_wclose */



/*------------------------------------------------------------------------*/


#define CLOSEANDRET   qf_close( &qfi, &locstat ); \
	qf_close( &qfo, &locstat ); return;



void qf_change( char *qfilnam, int *reclist, int listlth, int idx,
	void *value, int trcadd, STATUS *status )

/* changes the value of an info entry in the file "qfilnam" on records
 * "reclist" to the value "*value"
 *
 * parameters of routine
 * char     *qfilnam;     input; name of q-file
 * int      *reclist;     input; list of record numbers
 * int      listlth;      input; length of list
 * int      idx;          input; info index
 * void     *value;       input; pointer to info
 * int      trcadd;       input; increment of lines/trace
 * STATUS   *status;      output; return status
 */
{
	/* local variables */
	char     tmpname[QFC_HLINELTH+1];   /* name of temorary header file */
	char     str[QFC_HLINELTH+1];       /* scratch string */
	char     hdr[MAXHDRLTH];            /* header to be parsed */
	int      hdrlth;                    /* length of header */
	INFBLC   ib;                        /* info block */
	QFILDSCR qfi, qfo;                  /* q-file descriptors (in & out) */
	int      i;                         /* counter */
	int      trccnt;                    /* trace counter */
	int      locstat;                   /* local status */
	int      eof;                       /* end of file */
	int      infochange;                /* change/insert info in header ? */

	/* executable code */

	locstat = cBcNoError;
	qf_rclose( status );
	if  (Severe(status))  return;
	strcpy( tmpname, qfv_hdir );
	strcat( tmpname, qfilnam );
	strcpy( qfo.name, tmpname );
	strcat( tmpname, TMPFILEXT );

	qf_iniqf( &qfi );
	qf_iniqf( &qfo );

	qf_opnhdr( &qfi, qfilnam, status );
	if  (*status != QFE_NOERROR)  return;
	fseek( qfi.qhdr, 0L, 0 );  /* rewind file */

	qfo.qhdr = sy_fopen( tmpname, "w" );
	if  (qfo.qhdr == NULL)  {
		qf_close( &qfi, &locstat );
		*status = QFE_OPNHDR;
		return;
	} /*endif*/
	qfo.cmtlines = qfi.cmtlines;
	qfo.trclines = qfi.trclines + trcadd;
	qfo.currtrc = 0;
	qfo.binopen = FALSE;

	/* create first line */
	fprintf( qfo.qhdr, "%d %d %d\n", QMAGIC, qfo.cmtlines, qfo.trclines );

	/* copy comment lines */
	for (i=0;i<qfo.cmtlines;i++)  {
		if  (fgets(str,QFC_HLINELTH,qfi.qhdr) == NULL)  {
			*status = QFE_RDHDR;
			CLOSEANDRET
		} /*endif*/
		if  (i > 0)  fprintf( qfo.qhdr, "%s", str );
	} /*endfor*/

	/* copy/change info headers */
	trccnt = 0;
	locstat = QFE_NOERROR;
	FOREVER  {
		infochange = qf_numinset( ++trccnt, reclist, listlth );
		if  (infochange || (trcadd != 0))  {
			qf_rdhdr( &qfi, hdr, &hdrlth, &locstat );
			if  (locstat == QFE_EOFH)  {
				break;
			} else if  (locstat != QFE_NOERROR)  {
				*status = locstat;
				return;
			} /*endif*/
			qf_parsehdr( hdr, hdrlth, &ib, status );
			if  (*status != QFE_NOERROR)  {CLOSEANDRET}
			if  (infochange)  qf_setb( &ib, idx, value, status );
			if  (*status != QFE_NOERROR)  {CLOSEANDRET}
			qf_ibtostr( &ib, status );
			if  (*status != QFE_NOERROR)  {CLOSEANDRET}
			qf_wrhdr( &qfo, trccnt, status );
			if  (*status != QFE_NOERROR)  {CLOSEANDRET}
		} else {  /* plain copy */
			eof = FALSE;
			for  (i=0;i<qfo.trclines;i++)  {
				if  (fgets(str,QFC_HLINELTH,qfi.qhdr) == NULL)  {
					eof = (i==0);
					if  (eof)  break;
					*status = QFE_RDHDR;
					CLOSEANDRET
				} /*endif*/
				fprintf( qfo.qhdr, "%s", str );
			} /*endfor*/
			if  (eof)  break;
			qfi.currtrc++;
			qfo.currtrc++;
		} /*endif*/
	} /*endfor*/

	qf_close( &qfi, status );
	if  (*status != QFE_NOERROR)  {qf_close(&qfo,&locstat); return;}
	qf_close( &qfo, status );
	if  (*status != QFE_NOERROR)  return;

	strcpy( str, qfv_hdir );
	strcat( str, qfilnam );
	strcat( str, HDRFILEXT );
	sy_fdelete( str );
	sy_frename( tmpname, str );

} /* end of qf_change */



/*------------------------------------------------------------------------*/



void qf_comment( char **cmt, int linecnt )

/* sets the comment text used by all following qf_newhdr calls (when a
 * new q-file is created)
 *
 * parameters of routine
 * char     **cmt;        input; pointers to text lines
 * int      linecnt;       input; number of lines
 */
{
	/* executable code */

	cmt_qfv = cmt;
	cmtlin_qfv = linecnt;

} /* end of qf_comment */



/*------------------------------------------------------------------------*/



void qf_insertcmt( char qfile[], char cmtfile[], STATUS *status )

/* inserts file "cmtfile" as a comment to the header of q-file
 * "qfile"
 *
 * parameters of routine
 * char       qfile[];    input; name of q-file to insert comment
 * char       cmtfile[];  input; name of file to be included
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	int      linecnt;              /* line counter */
	FILE     *cf, *qif, *qof;      /* include-, input- & output file */
	char     line[QFC_HLINELTH+1]; /* current line */
	char     qname[BC_FILELTH+1];  /* input q-file name with extension */
	char     tmpname[BC_FILELTH+1];/* output q-file name with extension */
	unsigned magic;                /* magic number */
	int      qcmt;                 /* number of comment lines in q-file */
	int      trclines;             /* number of trace lines */

	/* executable code */


	/* close possibly opened header file */
	qf_wclose( status );
	if  (Severe(status))  return;
	qf_rclose( status );
	if  (Severe(status))  return;

	/* determine number of lines of include file */
	linecnt = 0;
	cf = sy_fopen( cmtfile, "r" );
	if  (cf == NULL)  {
		*status = QFE_OPNCMT;
		return;
	} /*endif*/
	while  (fgets(line,QFC_HLINELTH,cf) != NULL)
		linecnt++;
	if  (linecnt == 0)  {fclose( cf ); return;}
	fseek( cf, 0L, 0 );  /* rewind file */

	/* get q-file name with extension "qname" */
	if  (strlen(qfile) > BC_FILELTH-4)  {
		*status = QFE_NAMLTH;
		return;
	} /*endif*/
	strcpy( qname, qfv_hdir );
	strcat( qname, qfile );
	strcat( qname, HDRFILEXT );

	/* open input q-header and read first line */
	qif = sy_fopen( qname, "r" );
	if  (qif == NULL)  {
		fclose( cf );
		*status = QFE_OPNHDR;
		return;
	} /*endif*/
	if  (fgets(line,QFC_HLINELTH,qif) == NULL)  {
		fclose( cf );
		fclose( qif );
		*status = QFE_RDHDR;
		return;
	} /*endif*/
	if  (sscanf( line, "%d%d%d", &magic, &qcmt, &trclines ) != 3)  {
		fclose( cf );
		fclose( qif );
		*status = QFE_RDHDR;
		return;
	} /*endif*/
	if  (magic != QMAGIC)  {
		*status = QFE_NOQFILE;
		return;
	} /*endif*/

	/* output output header file */
	strcpy( tmpname, qfv_hdir );
	strcat( tmpname, qfile );
	strcat( tmpname, TMPFILEXT );
	qof = sy_fopen( tmpname, "w" );
	if  (qof == NULL)  {
		fclose( cf );
		fclose( qif );
		*status = QFE_OPNOUT;
		return;
	} /*endif*/

	/* create output file */
	qcmt += linecnt;
	/* write first line */
	fprintf( qof, "%d %d %d\n", QMAGIC, qcmt, trclines );
	/* write comment text */
	for  (; linecnt>0; linecnt--)  {
		fgets( line, QFC_HLINELTH, cf );
		fprintf( qof, "%s", line );
	} /*endfor*/
	/* copy input header */
	while  (fgets(line,QFC_HLINELTH,qif) != NULL)
		if  (line[strlen(line)-1] == '\n')  {
			fprintf( qof, "%s", line );
		} else {
			fprintf( qof, "%s\n", line );
		} /*endif*/

	/* close files */
	fclose( cf );
	fclose( qif );
	fclose( qof );

	/* delete old header, rename new one */
	sy_fdelete( qname );
	sy_frename( tmpname, qname );

} /* end of qf_insertcmt */



/*------------------------------------------------------------------------*/



void qf_byteswap( BOOLEAN swap )

/* set byte swap on or off
 *
 * parameter of routine
 * BOOLEAN      swap;      input; set byteswap TRUE or FALSE
 */
{
	qfv_byteswap = swap;

} /* end of qf_byteswap */



/*------------------------------------------------------------------------*/



void qf_defaultdir( char hdir[], char ddir[], STATUS *status )

/* sets default directories for header and binary files.  A NULL-parameter
 * disables default directory.  A " "-parameter leaves the default unchanged.
 *
 * parameters of routine
 * char       hdir[];     input; default directory for header files
 * char       ddir[];     input; default directory for binary files
 * STATUS     *status;    output; return status
 */
{
	/* executable code */

	if  (hdir == NULL)  {
		*qfv_hdir = '\0';
	} else {
		if  (strlen(hdir) > BC_FILELTH)  {
			*status = QFE_NAMLTH;
			return;
		} /*endif*/
		if  (*hdir != ' ')
			strcpy( qfv_hdir, hdir );
	} /*endif*/
	if  (ddir == NULL)  {
		*qfv_ddir = '\0';
	} else {
		if  (strlen(ddir) > BC_FILELTH)  {
			*status = QFE_NAMLTH;
			return;
		} /*endif*/
		if  (*ddir != ' ')
			strcpy( qfv_ddir, ddir );
	} /*endif*/

} /* end of qf_defaultdir */



/*------------------------------------------------------------------------*/



void qf_opnhdr( QFILDSCR *qf, char *qfilnam, STATUS *status )

/* opens q-file for reading and sets global q-file descriptor
 *
 * parameters of routine
 * QFILDSCR *qf;           modify; q-file descriptor
 * char     *qfilnam;      input; name of q-file
 * STATUS   *status;       output; return status
 */
{
	/* local variables */
	unsigned int  magic;          /* q magic number */
	char     str[QFC_HLINELTH+1]; /* scratch string */
	int      i;                   /* counter */
	char     *res;                /* fgets result */

	/* executable code */

	if  (qf->currtrc != -1)  {
		*status = QFE_OPNTWICE;
		return;
	} /*endif*/

	if  (strlen(qfilnam) > QFC_HLINELTH-4)  {
		*status = QFE_NAMLTH;
		return;
	} /*endif*/
	strcpy( qf->name, qfilnam );
	strcpy( str, qfv_hdir );
	strcat( str, qfilnam );
	strcat( str, HDRFILEXT );

	qf->qhdr = sy_fopen( str, "r" );
	if  (qf->qhdr == NULL)  {
		*status = QFE_OPNHDR;
		return;
	} /*endif*/

	/* read first line and get file infos */
	res = fgets( str, QFC_HLINELTH, qf->qhdr );
	if  ((res == NULL) || (res == (void *)EOF))  {
		*status = QFE_RDHDR;
		return;
	} /*endif*/
	if  (sscanf( str, "%d%d%d", &magic, &(qf->cmtlines),
		&(qf->trclines) ) != 3)  {
		*status = QFE_RDHDR;
		return;
	} /*endif*/
	if  (magic != QMAGIC)  {
		*status = QFE_NOQFILE;
		return;
	} /*endif*/

	/* read off comment lines */
	for  (i=1;i<qf->cmtlines;i++)  {
		res = fgets( str, QFC_HLINELTH, qf->qhdr );
		if  ((res == NULL) || (res == (void *)EOF))  {
			*status = QFE_RDHDR;
			return;
		} /*endif*/
	} /*endfor*/

	qf->currtrc = 0;

} /* end of qf_opnhdr */



/*------------------------------------------------------------------------*/



void qf_wophdr( QFILDSCR *qf, char *qfilnam, STATUS *status )

/* opens an existing header file for writing
 *
 * parameters of routine
 * QFILDSCR *qf;            modify; q-file descriptor
 * char     *qfilnam;       input; name of q-file
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	unsigned int  magic;          /* q magic number */
	char     fnam[QFC_HLINELTH+1];/* filename */
	char     str[QFC_HLINELTH+1]; /* scratch string */
	char     *res;                /* fgets result */

	/* executable code */

	if  (qf->currtrc != -1)  {
		*status = QFE_OPNTWICE;
		return;
	} /*endif*/

	if  (strlen(qfilnam) > QFC_HLINELTH-4)  {
		*status = QFE_NAMLTH;
		return;
	} /*endif*/
	strcpy( qf->name, qfilnam );
	strcpy( fnam, qfv_hdir );
	strcat( fnam, qfilnam );
	strcat( fnam, HDRFILEXT );

	qf->qhdr = sy_fopen( fnam, "r" );
	if  (qf->qhdr == NULL)  {
		*status = QFE_OPNHDR;
		return;
	} /*endif*/

	/* read first line and get file infos */
	res = fgets( str, QFC_HLINELTH, qf->qhdr );
	if  ((res == NULL) || (res == (void *)EOF))  {
		*status = QFE_RDHDR;
		return;
	} /*endif*/
	if  (sscanf( str, "%d%d%d", &magic, &(qf->cmtlines),
		&(qf->trclines) ) != 3)  {
		*status = QFE_RDHDR;
		return;
	} /*endif*/
	if  (magic != QMAGIC)  {
		*status = QFE_NOQFILE;
		return;
	} /*endif*/

	/* freopen( fnam, "a", qf->qhdr ); */
	sy_fclose( qf->qhdr );
	qf->qhdr = sy_fopen( fnam, "a" );

} /* end of qf_wophdr */



/*------------------------------------------------------------------------*/



void qf_wopbin( QFILDSCR *qf, char *qfilnam, STATUS *status )

/* opens q-binary-file for writing
 *
 * parameters of routine
 * QFILDSCR *qf;             modify; q-file descriptor
 * char     *qfilnam;        input; name of q-file
 * STATUS   *status;         output; return status
 */
{
	/* local variables */
	char     str[QFC_HLINELTH+1]; /* scratch string */

	/* executable code */

	if  (qf->binopen)  {
		*status = QFE_OPNTWICE;
		return;
	} /*endif*/

	if  (strlen(qfilnam) > QFC_HLINELTH-4)  {
		*status = QFE_NAMLTH;
		return;
	} /*endif*/
	strcpy( str, qfv_ddir );
	strcat( str, qfilnam );
	strcat( str, BINFILEXT );

	qf->qbin = sy_fbopen( str, "a" );
	if  (sy_fbfailed(qf->qbin))  {
		*status = QFE_OPNBIN;
		return;
	} /*endif*/

	qf->binopen = TRUE;

} /* end of qf_wopbin */



/*------------------------------------------------------------------------*/



void qf_close( QFILDSCR *qf, STATUS *status )

/* closes header & binary file
 *
 * parameters of routine
 * QFILDSCR *qf;             modify; q-file descriptor
 * STATUS   *status;         output; return status
 */
{
	/* executable code */

	if  (qf->currtrc != -1)  {
		if  (fclose(qf->qhdr) != 0)  *status = QFE_CLSFIL;
		qf->name[0] = '\0';
		qf->currtrc = -1;
	} /*endif*/

	if  (qf->binopen)  {
		if  (sy_fbclose(qf->qbin) != 0)  *status = QFE_CLSFIL;
		qf->binopen = FALSE;
	} /*endif*/

} /* end of qf_close */



/*------------------------------------------------------------------------*/



void qf_skiphdr( QFILDSCR *qf, int cnt, STATUS *status )

/* skips "cnt" headers in the q-file
 *
 * parameters of routine
 * QFILDSCR *qf;       modify; q-file descriptor
 * int      cnt;       input; number of headers skipped
 * STATUS   *status;   output; return status
 */
{
	/* local variables */
	int      i;                    /* counter */
	char     str[QFC_HLINELTH+1];  /* scratch */
	char     *res;                 /* fgets result */

	/* executable code */

	for  (i=0;i<cnt*(qf->trclines);i++)  {
		res = fgets( str, QFC_HLINELTH, qf->qhdr );
		if  ((res == NULL) || (res == (void *)EOF))  {
			*status = QFE_RDHDR;
			return;
		} /*endif*/
	} /*endfor*/

	qf->currtrc += cnt;

} /* end of qf_skiphdr */



/*------------------------------------------------------------------------*/



void qf_rdhdr( QFILDSCR *qf, char hdr[], int *lth, STATUS *status )

/* reads a header from the q-file
 *
 * parameters of routine
 * QFILDSCR *qf;        modify; q-file descriptor
 * char     hdr[];      output; header read from file
 * int      *lth;       output; length of header in chars
 * STATUS   *status;    output; return status
 */
{
	/* local variables */
	char     str[QFC_HLINELTH+1]; /* scratch string */
	char     *line;               /* pointer to beginning of info */
	int      currlen;             /* length of current header line */
	int      i;                   /* counter */
	char     *res;                /* fgets result */

	/* executable code */

	hdr[0] = '\0';
	*lth = 0;
	line = str + SKIPCH;

	/* read # of header lines and concatenate */
	for  (i=0;i<qf->trclines;i++)  {

		/* read line */
		res = fgets(str,QFC_HLINELTH,qf->qhdr);
		if  ((res == (void *)EOF) || (res == NULL))  {
			*status = ((i==0) ? QFE_EOFH : QFE_RDHDR);
			return;
		} else if  (strlen(str) < SKIPCH)  {
			*status = QFE_RDHDR;
			return;
		} /*endif*/

		/* skip line terminators */
		for  ( currlen=(int)strlen(line);
			(line[currlen-1]<' ') && (currlen>0) ;currlen-- )  {}

		/* concatenate */
		if  (currlen > 0)  {
			line[currlen] = '\0';
			strcat( hdr, line );
			*lth += currlen;
		} /*endif*/

	} /*endfor*/

	(qf->currtrc)++;

} /* end of qf_rdhdr */



/*------------------------------------------------------------------------*/



void qf_rdbin( QFILDSCR *qf, long pos, long lth, DATATYPE **ptr,
	STATUS *status )

/* reads data from binary file
 *
 * parameters of routine
 * QFILDSCR *qf;            modify; q-file descriptor
 * long     pos;            input; start position for reading
 * long     lth;            input; number of samples to read
 * DATATYPE **ptr;          output; pointer to data read from file
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	static long currpos;             /* current position in file */
	char        str[QFC_HLINELTH+1]; /* scratch string */
	long        lth_read;            /* samples read from file */

	/* executable code */

	if  (!qf->binopen)  {
		strcpy( str, qfv_ddir );
		strcat( str, qf->name );
		strcat( str, BINFILEXT );
		qf->qbin = sy_fbopen( str, "r" );
		if  (sy_fbfailed(qf->qbin))  {
			*status = QFE_OPNBIN;
			return;
		} /*endif*/
		qf->binopen = TRUE;
		currpos = 0;
	} /*endif*/

	if  (currpos != pos)  {
		sy_fbseek( qf->qbin, (long)sizeof(DATATYPE)*pos, 0 );
		currpos = pos;
	} /*endif*/

	*ptr = sy_allocmem( lth, (int)sizeof(DATATYPE), status );
	if  (*status != QFE_NOERROR)  return;
	lth_read = sy_fbread( *ptr, (int)sizeof(DATATYPE), lth, qf->qbin );
	if  (lth_read != lth)  {
		*status = (lth_read==0) ? QFE_EOFB : QFE_RDBIN;
		sy_deallocmem( *ptr );
		return;
	} /*endif*/

	if  (qfv_byteswap)
		qf_dobyteswap( (BYTE *)*ptr, lth*sizeof(DATATYPE) );

	currpos += lth;

} /* end of qf_rdbin */



/*------------------------------------------------------------------------*/



void qf_parsehdr( char hdr[], int lth, INFBLC *ib, STATUS *status )

/* parses a header and extracts info to the info block "ib"
 *
 * parameters of routine
 * char     hdr[];          input; header to be parsed
 * int      lth;            input; length of header in chars
 * INFBLC   *ib;            output; info read from header
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      i;              /* counter */
	char     *chptr;         /* header pointer */
	int      entcnt;         /* entry descriptor counter */
	char     type;           /* type of info entry */
	char     str[QFC_SLTH];  /* scratch string */
	int      entrynum;       /* entry number */

	/* executable code */

	/* reset info block */
	for  (i=0;i<QFC_LENTRY;i++)  {
		ib->li[i] = QFC_LEMPTY;
	} /*endfor*/
	for  (i=0;i<QFC_IENTRY;i++)  {
		ib->ii[i] = QFC_IEMPTY;
	} /*endfor*/
	for  (i=0;i<QFC_RENTRY;i++)  {
		/* ib->ri[i] = QFC_REMPTY; */
		qf_rsetempty( &(ib->ri[i]) );
	} /*endfor*/
	for  (i=0;i<QFC_SENTRY;i++)  {
		ib->si[i][0] = '\0';
	} /*endfor*/
	for  (i=0;i<QFC_CENTRY;i++)  {
		ib->ci[i] = '\0';
	} /*endfor*/

	chptr = hdr;
	entcnt = -1;
	for  (chptr=hdr;chptr<hdr+lth;chptr++)  {
		if  (++entcnt == 0)  {
			if  (*chptr != ' ')  {
				type = *chptr;
			}else{
				entcnt = -1;
			} /*endif*/
		}else if  (entcnt <= ENTNLTH)  {
			str[entcnt-1] = *chptr;
			if  (entcnt == ENTNLTH)  {
				str[entcnt] = '\0';
				if  (sscanf(str,"%d",&entrynum) != 1)  {
					*status = QFE_HDRPARSE;
					return;
				} /*endif*/
			} /*endif*/
		}else if  (entcnt <= ENTNLTH+1)  {
			if  (*chptr != ENTDEND)  {
				*status = QFE_HDRPARSE;
				return;
			} /*endif*/
		}else{
			if  (*chptr == ENTTERM)  {
				if  (entcnt-ENTDLTH >= QFC_SLTH)  {
					*status = QFE_LNGENT;
					return;
				} /*endif*/
				str[entcnt-ENTDLTH] = '\0';
				if  (type == 'L')  {
					if  ((entrynum < 0) || (entrynum >= QFC_LENTRY))  {
						*status = QFE_IBOVFL;
						return;
					} /*endif*/
					sscanf( str, "%ld", &(ib->li[entrynum]) );
				}else if  (type == 'I')  {
					if  ((entrynum < 0) || (entrynum >= QFC_IENTRY))  {
						*status = QFE_IBOVFL;
						return;
					} /*endif*/
					sscanf( str, "%d", &(ib->ii[entrynum]) );
				}else if  (type == 'R')  {
					if  ((entrynum < 0) || (entrynum >= QFC_RENTRY))  {
						*status = QFE_IBOVFL;
						return;
					} /*endif*/
					sscanf( str, "%f", &(ib->ri[entrynum]) );
				}else if  (type == 'S')  {
					if  ((entrynum < 0) || (entrynum >= QFC_SENTRY))  {
						*status = QFE_IBOVFL;
						return;
					} /*endif*/
					strcpy( &(ib->si[entrynum][0]), str );
				}else if  (type == 'C')  {
					if  ((entrynum < 0) || (entrynum >= QFC_CENTRY))  {
						*status = QFE_IBOVFL;
						return;
					} /*endif*/
					ib->ci[entrynum] = str[0];
				}else{
					*status = QFE_UDENT;
					return;
				} /*endif*/
				entcnt = -1;
			}else{
				str[entcnt-ENTDLTH] = *chptr;
			} /*endif*/
		} /*endif*/
	} /*endwhile*/

} /* end of qf_parsehdr */



/*------------------------------------------------------------------------*/



void qf_newhdr( QFILDSCR *qf, char qfilnam[], int cmtlin,
	char *cmt[], int trclin, STATUS *status )

/* creates beginning of new header file
 *
 * parameters of routine
 * QFILDSCR *qf;         modify; q-file descriptor
 * char     qfilnam[];   input; filename of new file
 * int      cmtlin;      input; number of comment lines
 * char     *cmt[];      input; comment lines (must include line term)
 * int      trclin;      input; lines per trace
 * STATUS   *status;     output; return status
 */
{
	/* local variables */
	char     str[QFC_HLINELTH+1];   /* scratch string */
	int      i;                     /* counter */
	int      slth;                  /* string length */

	/* executable code */

	if  (qf->currtrc != -1)  {
		*status = QFE_OPNTWICE;
		return;
	} /*endif*/

	if  (strlen(qfilnam) > QFC_HLINELTH-4)  {
		*status = QFE_NAMLTH;
		return;
	} /*endif*/

	strcpy( qf->name, qfilnam );
	strcpy( str, qfv_hdir );
	strcat( str, qfilnam );
	strcat( str, HDRFILEXT );

	qf->qhdr = sy_fopen( str, "w" );
	if  (qf->qhdr == NULL)  {
		*status = QFE_OPNHDR;
		return;
	} /*endif*/

	fprintf( qf->qhdr, "%d %d %d\n", QMAGIC, cmtlin, trclin );
	qf->cmtlines = cmtlin;
	qf->trclines = trclin;
	qf->currtrc = 0;

	for  (i=0;i<cmtlin-1;i++)  {
		fprintf( qf->qhdr, "%s", cmt[i] );
		slth = (int)strlen( cmt[i] );
		if  (cmt[i][slth-1] != '\n')  fprintf( qf->qhdr, "\n" );
	} /*endfor*/

} /* end of qf_newhdr */



/*------------------------------------------------------------------------*/



void qf_inihdr( void )

/* initialises header for writing
 *
 * no parameters
 */
{
	/* executable code */

	whdr_qfv[0] = '\0';
	whlth_qfv = 0;
	wrtlth_qfv = QFC_LEMPTY;

} /* end of qf_inihdr */



/*------------------------------------------------------------------------*/



void qf_insinfo( int idx, void *inf, STATUS *status )

/* appends an info value of arbitrary type to the output header
 *
 * parameters of routine
 * int      idx;            input; info index
 * void     *inf;           input; info to append
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	switch  (idx & TYPMASK)  {
		case QFC_LTYPE:
			qf_setl( idx, *(long*)inf, status );
			break;
		case QFC_ITYPE:
			qf_seti( idx, *(int*)inf, status );
			break;
		case QFC_RTYPE:
			qf_setr( idx, *(float*)inf, status );
			break;
		case QFC_STYPE:
			qf_sets( idx, (char *)inf, status );
 			break;
		case QFC_CTYPE:
			qf_setc( idx, *(char*)inf, status );
			break;
		default:
			*status = QFE_UDTYP;
			return;
	} /*endswitch*/

} /* end of qf_insinfo */



/*------------------------------------------------------------------------*/



void qf_setl( int idx, long inf, STATUS *status )

/* appends long integer info to global whdr_qfv
 *
 * parameters of routine
 * int      idx;            input; info index
 * long     inf;            input; info to be appended
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     str[QFC_SLTH+ENTDLTH+1];        /* scratch string */
	int      slth;                           /* length of string */

	/* executable code */

	if  ((idx & TYPMASK) != QFC_LTYPE)  {
		*status = QFE_TYPERR;
		return;
	} /*endif*/
	if  (idx == QFCL_LTH)  wrtlth_qfv = inf;

	idx &= IDXMASK;

	qf_getnum( str+1, idx, ENTNLTH );
	*str = 'L';
	str[ENTDLTH-1] = ENTDEND;
	sprintf( str+ENTDLTH, "%ld", inf );
	slth = (int)strlen( str );
	str[slth] = ENTTERM;
	str[slth+1] = ' ';
	str[slth+2] = '\0';
	slth += 2;
	if  (slth+whlth_qfv > MAXHDRLTH)  {
		*status = QFE_HDROVFL;
		return;
	} /*endif*/
	strcat( whdr_qfv, str );
	whlth_qfv += slth;

} /* end of qf_setl */



/*------------------------------------------------------------------------*/



void qf_seti( int idx, int inf, STATUS *status )

/* appends integer info to global whdr_qfv
 *
 * parameters of routine
 * int      idx;            input; info index
 * int      inf;            input; info to be appended
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     str[QFC_SLTH+ENTDLTH+1];        /* scratch string */
	int      slth;                           /* length of string */

	/* executable code */

	if  ((idx & TYPMASK) != QFC_ITYPE)  {
		*status = QFE_TYPERR;
		return;
	} /*endif*/
	idx &= IDXMASK;

	qf_getnum( str+1, idx, ENTNLTH );
	*str = 'I';
	str[ENTDLTH-1] = ENTDEND;
	sprintf( str+ENTDLTH, "%d", inf );
	slth = (int)strlen( str );
	str[slth] = ENTTERM;
	str[slth+1] = ' ';
	str[slth+2] = '\0';
	slth += 2;
	if  (slth+whlth_qfv > MAXHDRLTH)  {
		*status = QFE_HDROVFL;
		return;
	} /*endif*/
	strcat( whdr_qfv, str );
	whlth_qfv += slth;

} /* end of qf_seti */



/*------------------------------------------------------------------------*/



void qf_setr( int idx, float inf, STATUS *status )

/* appends float info to global whdr_qfv
 *
 * parameters of routine
 * int      idx;            input; info index
 * float    inf;            input; info to be appended
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     str[QFC_SLTH+ENTDLTH+1];        /* scratch string */
	int      slth;                           /* length of string */

	/* executable code */

	if  ((idx & TYPMASK) != QFC_RTYPE)  {
		*status = QFE_TYPERR;
		return;
	} /*endif*/
	idx &= IDXMASK;

	qf_getnum( str+1, idx, ENTNLTH );
	*str = 'R';
	str[ENTDLTH-1] = ENTDEND;
	sprintf( str+ENTDLTH, "%f", inf );
	slth = (int)strlen( str );
	str[slth] = ENTTERM;
	str[slth+1] = ' ';
	str[slth+2] = '\0';
	slth += 2;
	if  (slth+whlth_qfv > MAXHDRLTH)  {
		*status = QFE_HDROVFL;
		return;
	} /*endif*/
	strcat( whdr_qfv, str );
	whlth_qfv += slth;

} /* end of qf_setr */



/*------------------------------------------------------------------------*/



void qf_sets( int idx, char inf[], STATUS *status )

/* appends string info to global whdr_qfv
 *
 * parameters of routine
 * int      idx;            input; info index
 * char     inf[];          input; info to be appended
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     str[QFC_SLTH+ENTDLTH+1];        /* scratch string */
	int      slth;                           /* length of string */

	/* executable code */

	if  ((idx & TYPMASK) != QFC_STYPE)  {
		*status = QFE_TYPERR;
		return;
	} /*endif*/
	idx &= IDXMASK;

	qf_getnum( str+1, idx, ENTNLTH );
	*str = 'S';
	str[ENTDLTH-1] = ENTDEND;
	strcpy( str+ENTDLTH, inf );
	qf_replchar( str+ENTDLTH, ENTTERM, '_' );  /* replace term chars */
	slth = (int)strlen( str );
	str[slth] = ENTTERM;
	str[slth+1] = ' ';
	str[slth+2] = '\0';
	slth += 2;
	if  (slth+whlth_qfv > MAXHDRLTH)  {
		*status = QFE_HDROVFL;
		return;
	} /*endif*/
	strcat( whdr_qfv, str );
	whlth_qfv += slth;

} /* end of qf_sets */



/*------------------------------------------------------------------------*/



void qf_setc( int idx, char inf, STATUS *status )

/* appends char info to global whdr_qfv
 *
 * parameters of routine
 * int      idx;            input; info index
 * char     inf;            input; info to be appended
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     str[QFC_SLTH+ENTDLTH+1];        /* scratch string */
	int      slth;                           /* length of string */

	/* executable code */

	if  ((idx & TYPMASK) != QFC_CTYPE)  {
		*status = QFE_TYPERR;
		return;
	} /*endif*/
	idx &= IDXMASK;

	if  (inf == ENTTERM)  inf = '_';   /* replace term char */
	qf_getnum( str+1, idx, ENTNLTH );
	*str = 'C';
	str[ENTDLTH-1] = ENTDEND;
	str[ENTDLTH] = inf;
	str[ENTDLTH+1] = '\0';
	slth = (int)strlen( str );
	str[slth] = ENTTERM;
	str[slth+1] = ' ';
	str[slth+2] = '\0';
	slth += 2;
	if  (slth+whlth_qfv > MAXHDRLTH)  {
		*status = QFE_HDROVFL;
		return;
	} /*endif*/
	strcat( whdr_qfv, str );
	whlth_qfv += slth;

} /* end of qf_setc */



/*------------------------------------------------------------------------*/



long qf_getl( int idx, STATUS *status )

/* returns long values from last read header
 *
 * parameters of routine
 * int      idx;            input; info index
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((idx & TYPMASK) != QFC_LTYPE)  {
		*status = QFE_TYPERR;
		return QFC_LEMPTY;
	} /*endif*/
	idx &= IDXMASK;

	if  ((idx < 0) || (idx >= QFC_LENTRY))  {
		if  (status != NULL)  *status = QFE_ILLIDX;
		return (QFC_LEMPTY);
	} else if  (ib_qfv.li[idx] == QFC_LEMPTY)  {
		if  (status != NULL)  *status = QFE_EMPTY;
	} /*endif*/

	return (ib_qfv.li[idx]);

} /* end of qf_getl */



/*------------------------------------------------------------------------*/



int qf_geti( int idx, STATUS *status )

/* returns int values from last read header
 *
 * parameters of routine
 * int      idx;            input; info index
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((idx & TYPMASK) != QFC_ITYPE)  {
		*status = QFE_TYPERR;
		return QFC_IEMPTY;
	} /*endif*/
	idx &= IDXMASK;

	if  ((idx < 0) || (idx >= QFC_IENTRY))  {
		if  (status != NULL)  *status = QFE_ILLIDX;
		return (QFC_IEMPTY);
	} else if  (ib_qfv.ii[idx] == QFC_IEMPTY)  {
		if  (status != NULL)  *status = QFE_EMPTY;
	} /*endif*/

	return (ib_qfv.ii[idx]);

} /* end of qf_geti */



/*------------------------------------------------------------------------*/



float qf_getr( int idx, STATUS *status )

/* returns float values from last read header
 *
 * parameters of routine
 * int      idx;            input; info index
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((idx & TYPMASK) != QFC_RTYPE)  {
		*status = QFE_TYPERR;
		return 0.;
	} /*endif*/
	idx &= IDXMASK;

	if  ((idx < 0) || (idx >= QFC_RENTRY))  {
		if  (status != NULL)  *status = QFE_ILLIDX;
		return 0.;
	} else if  (qf_risempty(&(ib_qfv.ri[idx])))  {
		if  (status != NULL)  *status = QFE_EMPTY;
	} /*endif*/

	return (ib_qfv.ri[idx]);

} /* end of qf_getr */



/*------------------------------------------------------------------------*/



char *qf_gets( int idx, STATUS *status )

/* returns string pointer from last read header
 *
 * parameters of routine
 * int      idx;            input; info index
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((idx & TYPMASK) != QFC_STYPE)  {
		*status = QFE_TYPERR;
		return NULL;
	} /*endif*/
	idx &= IDXMASK;

	if  ((idx < 0) || (idx >= QFC_SENTRY))  {
		if  (status != NULL)  *status = QFE_ILLIDX;
		return (NULL);
	} else if  (ib_qfv.si[idx][0] == '\0')  {
		if  (status != NULL)  *status = QFE_EMPTY;
		return (NULL);
	} /*endif*/

	return (ib_qfv.si[idx]);

} /* end of qf_gets */



/*------------------------------------------------------------------------*/



char qf_getc( int idx, STATUS *status )

/* returns char values from last read header
 *
 * parameters of routine
 * int      idx;            input; info index
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((idx & TYPMASK) != QFC_CTYPE)  {
		*status = QFE_TYPERR;
		return '\0';
	} /*endif*/
	idx &= IDXMASK;

	if  ((idx < 0) || (idx >= QFC_CENTRY))  {
		if  (status != NULL)  *status = QFE_ILLIDX;
		return ('\0');
	} else if  (ib_qfv.ci[idx] == '\0')  {
		if  (status != NULL)  *status = QFE_EMPTY;
	} /*endif*/

	return (ib_qfv.ci[idx]);

} /* end of qf_getc */



/*------------------------------------------------------------------------*/



int qf_risempty( float *f )

/* checks whether f is empty or not
 *
 * parameters of routine
 * float        *f;      input; pointer to real number
 *                       returns TRUE if empty
 */
{
	/* executable code */
	/*return (*(long *)f == R_EMPTY);*/
	return ((*f) > R_EMPTY_MIN && (*f) < R_EMPTY_MAX);
}  /* end of qf_risempty */



/*------------------------------------------------------------------------*/



void qf_rsetempty( float *f )

/* sets f to empty value
 *
 * parameters of routine
 * float        *f;       output; real number set to empty
 */
{
	/* executable code */
	/* *(long *)f = R_EMPTY;*/
	*f = R_EMPTY;
}  /* end of qf_rsetempty */



/*------------------------------------------------------------------------*/



void qf_wrhdr( QFILDSCR *qf, int hdrcnt, STATUS *status )

/* writes the header set by qf_set*-routines to the current opened header
 * file
 *
 * parameters of routine
 * QFILDSCR *qf;            modify; q-file descriptor
 * int      hdrcnt;         input; number of header
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     line[QFC_HLINELTH+1]; /* line to write to file */
	int      i;                    /* counter */
	int      currpos;              /* current position in header */

	/* executable code */

	hdrcnt %= 100;

	currpos = 0;
	for  (i=0;i<qf->trclines;i++)  {
		qf_getnum( line, hdrcnt, SKIPCH-1 );
		line[SKIPCH-1] = '|';
 		if  (currpos < whlth_qfv)  {
			strncpy( line+SKIPCH, whdr_qfv+currpos, QFC_HLINELTH-SKIPCH-2 );
			currpos += QFC_HLINELTH-SKIPCH-2;
			if  (currpos <= whlth_qfv)  {
				line[QFC_HLINELTH-2] = '\n';
				line[QFC_HLINELTH-1] = '\0';
			}else{
				line[QFC_HLINELTH-2-(currpos-whlth_qfv)] = '\n';
				line[QFC_HLINELTH-1-(currpos-whlth_qfv)] = '\0';
			} /*endif*/
		}else{
			line[SKIPCH] = '\n';
			line[SKIPCH+1] = '\0';
		} /*endif*/
		fprintf( qf->qhdr, "%s", line );
	} /*endfor*/

	if  (currpos < whlth_qfv)  *status = QFE_TRCOVFL;
	(qf->currtrc)++;

} /* end of qf_wrhdr */



/*------------------------------------------------------------------------*/



void qf_wrbin( QFILDSCR *qf, DATATYPE *datptr, long datlth, STATUS *status )

/* writes data to the binary file
 *
 * parameters of routine
 * QFILDSCR *qf;            input; q-file descriptor
 * DATATYPE *datptr;        input; pointer to data array
 * long     datlth;         input; length of data
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	long          lth_written; /* data samples written */

	/* executable code */

	lth_written = sy_fbwrite( datptr, (int)sizeof(DATATYPE),
		datlth, qf->qbin );
	if  (lth_written != datlth)  *status = QFE_WRBIN;

} /* end of qf_wrbin */



/*------------------------------------------------------------------------*/



void qf_iniqf( QFILDSCR *qf )

/* initialises q-file descriptor
 *
 * parameters of routine
 * QFILDSCR *qf;    output; q-file descriptor
 */
{
	/* executable code */

	qf->cmtlines = 0;
	qf->trclines = 0;
	qf->currtrc = -1;
	qf->binopen = FALSE;

} /* end of qf_iniqf */



/*------------------------------------------------------------------------*/



void qf_getnum( char str[], int num, int lth )

/* writes "num" to string "str". Uses "lth" chars, if num too short,
 * zeroes are inserted at the beginning
 *
 * parameters of routine
 * char     str[];          output; number string
 * int      num;            input; number to convert
 * int      lth;            input; number of digits to be used
 */
{
	/* local variables */
	int      i;        /* counter */
	int      zlth;     /* number of zeroes to add */
	char     nstr[20]; /* int string */

	/* executable code */

	sprintf( nstr, "%d", num );
	zlth = lth - (int)strlen( nstr );
	if  (zlth < 0)  zlth = 0;
	for  (i=0;i<zlth;i++)  {
		str[i] = '0';
	} /*endfor*/
	str[zlth] = '\0';
	strcat( str, nstr );

} /* end of qf_getnum */



/*------------------------------------------------------------------------*/



void qf_size( char qfilnam[], long *size, int *trcno, int *trclin,
	STATUS *status )

/* returns the size of a q-binary-file in bytes or -1 if not found
 *
 * parameters of routine
 * char     qfilnam[];       input; name of q-file
 * long     *size;           output; size determined
 * int      *trcno;          output; number of traces in q-file
 * int      *trclin;         output; number of lines per trace
 * STATUS   *status;         output; return status
 */
{
	/* local variables */
	int      locstat;             /* local status */
	char     hdr[MAXHDRLTH+1];    /* trace header */
	int      hdrlth;              /* length of header */
	INFBLC   ib;                  /* info block */
	QFILDSCR qf;                  /* q-file descriptor */
	int      dmy;

	/* executable code */

	locstat = QFE_NOERROR;
	qf_iniqf( &qf );
	qf_opnhdr( &qf, qfilnam, status );
	if  (Severe(status))  return;

	*trclin = qf.trclines;

	*trcno = 0;
	locstat = QFE_NOERROR;
	FOREVER  {
		qf_rdhdr( &qf, hdr, &hdrlth, &locstat );
		if  (locstat != QFE_NOERROR)  break;
		qf_parsehdr( hdr, hdrlth, &ib, status );
		if  (*status != QFE_NOERROR)  break;
		(*trcno)++;
	} /*endfor*/
	qf_close( &qf, &dmy );

	if  (*status != QFE_NOERROR)  return;

	if  (locstat != QFE_EOFH)  {
		*status = locstat;
		return;
	} /*endif*/
	if ((ib.li[POSIDX] == QFC_LEMPTY) || (ib.li[LTHIDX] == QFC_LEMPTY)) {
		*status = QFE_MISSENT;
		return;
	} /*endif*/

	*size = ib.li[POSIDX] + ib.li[LTHIDX];

} /* end of qf_size */



/*------------------------------------------------------------------------*/



void qf_setb( INFBLC *ib, int idx, void *inf, STATUS *status )

/* sets the info "inf" in the info block "ib"
 *
 * parameters of routine
 * INFBLC   *ib;            modify; info block
 * int      idx;            input; info index
 * void     *inf;           input; info value
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      iidx;     /* info index (without type) */

	/* executable code */

	iidx = (idx & IDXMASK);
	switch  (idx & TYPMASK)  {
		case QFC_LTYPE:
			if  ((iidx < 0) || (iidx >= QFC_LENTRY))  {
				*status = QFE_ILLIDX;
				return;
			} /*endif*/
			qf_eql( &(ib->li[iidx]), inf );
			break;
		case QFC_ITYPE:
			if  ((iidx < 0) || (iidx >= QFC_IENTRY))  {
				*status = QFE_ILLIDX;
				return;
			} /*endif*/
			qf_eqi( &(ib->ii[iidx]), inf );
			break;
		case QFC_RTYPE:
			if  ((iidx < 0) || (iidx >= QFC_RENTRY))  {
				*status = QFE_ILLIDX;
				return;
			} /*endif*/
			qf_eqr( &(ib->ri[iidx]), inf );
			break;
		case QFC_STYPE:
			if  ((iidx < 0) || (iidx >= QFC_SENTRY))  {
				*status = QFE_ILLIDX;
				return;
			} /*endif*/
			strcpy( ib->si[iidx], inf );
			break;
		case QFC_CTYPE:
			if  ((iidx < 0) || (iidx >= QFC_CENTRY))  {
				*status = QFE_ILLIDX;
				return;
			} /*endif*/
			ib->ci[iidx] = *(char *)inf;
			break;
		default:
			*status = QFE_UDTYP;
			return;
	} /*endswitch*/

} /* end of qf_setb */



/*------------------------------------------------------------------------*/



void qf_ibtostr( INFBLC *ib, STATUS *status )

/* converts the info block to a string header
 *
 * parameters of routine
 * INFBLC   *ib;      input; info block
 * STATUS   *status;  output; return status
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	qf_inihdr();
	for  (i=0;i<QFC_LENTRY;i++)  {
		if  (ib->li[i] != QFC_LEMPTY)  qf_setl(QFC_LTYPE|i,ib->li[i],status);
		if  (*status != QFE_NOERROR)  return;
	} /*endfor*/
	for  (i=0;i<QFC_IENTRY;i++)  {
		if  (ib->ii[i] != QFC_IEMPTY)  qf_seti(QFC_ITYPE|i,ib->ii[i],status);
		if  (*status != QFE_NOERROR)  return;
	} /*endfor*/
	for  (i=0;i<QFC_RENTRY;i++)  {
		if  (!qf_risempty(&(ib->ri[i])))
			qf_setr(QFC_RTYPE|i,ib->ri[i],status);
		if  (*status != QFE_NOERROR)  return;
	} /*endfor*/
	for  (i=0;i<QFC_SENTRY;i++)  {
		if  (ib->si[i][0] != '\0')  qf_sets(QFC_STYPE|i,ib->si[i],status);
		if  (*status != QFE_NOERROR)  return;
	} /*endfor*/
	for  (i=0;i<QFC_CENTRY;i++)  {
		if  (ib->ci[i] != '\0')  qf_setc(QFC_CTYPE|i,ib->ci[i],status);
		if  (*status != QFE_NOERROR)  return;
	} /*endfor*/

} /* end of qf_ibtostr */



/*------------------------------------------------------------------------*/



int qf_numinset( int num, int set[], int lth )

/* checks whether "num" is in "set" or not
 *
 * parameters of routine
 * int      num;            input; number to be checked
 * int      set[];          input; check list
 * int      lth;            input; length of list
 */
{
	/* local variables */
	int      i;           /* counter */

	/* executable code */

	for  (i=0;i<lth;i++)  {if  (*(set++) == num)  return TRUE;}
	return FALSE;

} /* end of qf_numinset */



/*------------------------------------------------------------------------*/



void qf_eql(long *dst, long *src)   {*dst = *src; return;}
void qf_eqi(int *dst, int *src)     {*dst = *src; return;}
void qf_eqr(float *dst, float *src) {*dst = *src; return;}



/*------------------------------------------------------------------------*/
/*                      error processing                                  */
/*------------------------------------------------------------------------*/



void qf_dumpib( INFBLC *ib )

/* dumps ib to screen
 *
 * parameters of routine
 * INFBLC   *ib;    input; info block to dump
 */
{
	/* local variables */
	int      i;         /* counter */

	/* executable code */
	for  (i=0;i<QFC_LENTRY;i++)  {
		if  (ib->li[i] != QFC_LEMPTY)  printf( "\nL%d: %ld", i, ib->li[i] );
	} /*endfor*/
	for  (i=0;i<QFC_IENTRY;i++)  {
		if  (ib->ii[i] != QFC_IEMPTY)  printf( "\nI%d: %d", i, ib->ii[i] );
	} /*endfor*/
	for  (i=0;i<QFC_RENTRY;i++)  {
		if  (!qf_risempty(&(ib->ri[i])))  printf( "\nR%d: %f", i, ib->ri[i] );
	} /*endfor*/
	for  (i=0;i<QFC_SENTRY;i++)  {
		if  (ib->si[i][0] != '\0')  printf( "\nS%d: %s", i, ib->si[i] );
	} /*endfor*/
	for  (i=0;i<QFC_CENTRY;i++)  {
		if  (ib->ci[i] != '\0')  printf( "\nC%d: %c", i, ib->ci[i] );
	} /*endfor*/

} /* end of qf_dumpib */



/*------------------------------------------------------------------------*/



void qf_dobyteswap( BYTE b[], long lth )

/* swaps bytes in byte array "b"
 *
 * parameters of routine
 * BYTE       b[];       modify; data array to be byte swapped
 * long       lth;       input; length of data array in bytes
 */
{
	/* local variables */
	BYTE     *endb;    /* pointer to end of array */
	BYTE     tmp;      /* temporary storage */

	/* executable code */

   if  (lth & 1)  lth--;  /* decrease if lth odd */
	endb = b + lth;
	while  (b < endb)  {
		tmp = *b;
		*b++ = *(b+1);
		*b++ = tmp;
	} /*endwhile*/ 

} /* end of qf_dobyteswap */



/*------------------------------------------------------------------------*/



void qf_replchar( char str[], char find, char repl )

/* replaces all characters "find" in string "str" by characters "repl"
 *
 * parameters of routine
 * char       str[];      modify; string to be changed
 * char       find;       input; character to replace
 * char       repl;       input; replace character
 */
{
	/* executable code */

	str--;
	while  (*(++str) != '\0')
		if  (*str == find)
			*str = repl;

} /* end of qf_replchar */



/*------------------------------------------------------------------------*/

void qf_dumv() { qf_dumpib(&ib_qfv); }

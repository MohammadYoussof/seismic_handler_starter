/* file MEMFILE.C
 *      =========
 *
 * version 6, 29-Oct-97
 *   an old flag from v 4 (can't remember) ->  (HP !)
 *
 * memory files
 * K. Stammler, 31-JUL-1990
 */


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include "sysbase.h"
#include "shconst.h"
#include "erusrdef.h"
#include "mfusrdef.h"

#define MEMORY void
#define ALLOCLTH 10000L
#define MEMUNIT char

#define DELETED 0
#define CLOSED 1
#define OPENED 2

#define TONONE 0
#define TOMEM 1
#define TOFILE 2

typedef struct _segment {
	MEMORY    *mem;      /* start of memory block */
	MEMORY    *end;      /* end of memory block */
	struct _segment  *next;     /* next segment */
	struct _segment  *prev;     /* previous segment */
} SEGMENT;

typedef struct {
	MEMORY    *rp;       /* read pointer */
	MEMORY    *wp;       /* write pointer */
	SEGMENT   *rseg;     /* pointer to current read segment */
	SEGMENT   *wseg;     /* pointer to current write segment */
} MEMFILE;


/* global variables */
static BFILE    mfv_file[MFC_MAXMF]          /* file pointers */
	= {(BFILE)(-1),(BFILE)(-1),(BFILE)(-1),(BFILE)(-1),(BFILE)(-1),
	(BFILE)(-1),(BFILE)(-1),(BFILE)(-1)};
static MEMFILE  mfv[MFC_MAXMF];              /* memory files */
static char     mfv_directory[BC_FILELTH+1]  /* directory path */
						= {""};
static char     mfv_fname[MFC_MAXMF][BC_FILELTH+1]  /* file names */
	= { "MF00","MF01","MF02","MF03","MF04","MF05","MF06","MF07"};
static int      mfv_stat[MFC_MAXMF];         /* file status */
static int      mfv_switch[MFC_MAXMF];       /* stream switch */
static BOOLEAN  mfv_defswitch=TRUE;          /* default switch */


/* prototypes of local routines */
void mf_nextwriteseg( MEMFILE *mf, STATUS *status );
void mf_nextreadseg( MEMFILE *mf, STATUS *status );


/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/
/*
int main(void)

{
	MFC_REAL     r;
	int      i;
	char     c;
	int      id, status;


	status = 0;
	id = 1;

	r = 23.0;
	i = 13;
	c = 'A';
	mf_writereal( id, r, &status );
	mf_writeint( id, i, &status );
	mf_writechar( id, c, &status );

	r = mf_readreal( id, &status );
	i = mf_readint( id, &status );
	c = mf_readchar( id, &status );

	mf_writereal( id, r, &status );
	mf_writeint( id, i, &status );
	mf_writechar( id, c, &status );

	r = mf_readreal( id, &status );
	i = mf_readint( id, &status );
	c = mf_readchar( id, &status );

	mf_close( id, &status );
	mf_delete( id, &status );

	return 0;

}
*/
/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/


void mf_open( unsigned id, BOOLEAN mem, char *access, int *status )

/* open file number "id"
 *
 * parameters of routine
 * unsigned   id;        input; file number
 * BOOLEAN    mem;       input; memory file (TRUE) or disk file (FALSE)
 * char       *access;   input; read/write access;
 * int        *status;   output; return status
 */
{
	/* local variables */
	char     name[BC_FILELTH+1];   /* name of file */
	SEGMENT  *lseg;                /* segment pointer */

	/* executable code */

	if  (id >= MFC_MAXMF)  {
		*status = MFE_ILID;
		err_setcontext( " ## illegal ID was " ); err_setcontext_l( id );
		return;
	} else if  (mfv_stat[id] >= OPENED)  {
		*status = MFE_OVWR;
		err_setcontext( " ## ID: " ); err_setcontext_l( id );
		err_setcontext( ", status: " ); err_setcontext_l( mfv_stat[id] );
		return;
	} /*endif*/

	if  (mem)  {
		if  (mfv_stat[id] == DELETED)  {   /* nothing allocated */
			lseg = sy_allocmem( ALLOCLTH+sizeof(SEGMENT), 1, status);
			if  (*status != MFE_NOERROR)  return;
			lseg->mem = (MEMUNIT *)lseg + sizeof(SEGMENT);
			lseg->end = (MEMUNIT *)(lseg->mem) + ALLOCLTH;
			lseg->next = NULL;
			lseg->prev = NULL;
			mfv[id].rseg = lseg;
			mfv[id].wseg = lseg;
			mfv[id].wp = lseg->mem;
			mfv[id].rp = mfv[id].rseg->mem;
		} else {
			lseg = mfv[id].wseg;
			while  (lseg->prev != NULL)
				lseg = lseg->prev;
			mfv[id].rseg = lseg;
			mfv[id].rp = lseg->mem;
		} /*endif*/
		mfv_switch[id] = TOMEM;
	} else {
		if  ((strlen(mfv_directory)+strlen(mfv_fname[id])) > BC_FILELTH)  {
			*status = MFE_STROVFL;
			return;
		} /*endif*/
		strcpy( name, mfv_directory );
		strcat( name, mfv_fname[id] );
		mfv_file[id] = sy_fbopen( name, access );
		if  (sy_fbfailed(mfv_file[id]))  {
			*status = MFE_OPNWR;
			return;
		} /*endif*/
		mfv_switch[id] = TOFILE;
	} /*endif*/
	mfv_stat[id] = OPENED;

} /* end of mf_open */



/*-------------------------------------------------------------------------*/


void mf_close( unsigned id, int *status )

/* closes memory file
 *
 * parameters of routine
 * unsigned     id;      input; file ID
 * int          *status; output; return status
 */
{
	/* executable code */

	if  (id >= MFC_MAXMF)  {
		*status = MFE_ILID;
		return;
	} /*endif*/

	if  (mfv_switch[id] == TOFILE)  {
		if  (sy_fbfailed(mfv_file[id]))  return;
		sy_fbclose( mfv_file[id] );
		mfv_file[id] = (BFILE)(-1);
	} /*endif*/
	if  (mfv_stat[id] > CLOSED)  mfv_stat[id] = CLOSED;

} /* end of mf_close */



/*-------------------------------------------------------------------------*/



void mf_rewind( unsigned id, int *status )

/* rewinds file
 *
 * parameters of routine
 * unsigned   id;         input; file ID
 * int        *status;    output; return status
 */
{
	/* executable code */
	SEGMENT       *lseg;     /* segment pointer */

	if  (id >= MFC_MAXMF)  {
		*status = MFE_ILID;
		return;
	} /*endif*/

	if  (mfv_stat[id] != OPENED)  return;
	if  (mfv_switch[id] == TOMEM)  {
		lseg = mfv[id].wseg;
		while  (lseg->prev != NULL)
			lseg = lseg->prev;
		mfv[id].wseg = lseg;
		mfv[id].rseg = lseg;
		mfv[id].wp = lseg->mem;
		mfv[id].rp = lseg->mem;
	} else if  (mfv_switch[id] == TOFILE)  {
		sy_fbseek( mfv_file[id], 0L, 0 );
	} /*endif*/

} /* end of mf_rewind */



/*-------------------------------------------------------------------------*/



void mf_delete( unsigned id, int *status )

/* deletes file
 *
 * parameters of routine
 * unsigned   id;        input;  file ID
 * int        *status;   output; return status
 */
{
	/* local variables */
	char     name[BC_FILELTH+1];   /* file name */
	SEGMENT  *lseg;                /* segment pointer */

	/* executable code */

	if  (id >= MFC_MAXMF)  {
		*status = MFE_ILID;
		return;
	} else if  (mfv_stat[id] != CLOSED)  {
		*status = MFE_DELOPN;
		return;
	} /*endif*/

	if  (mfv_switch[id] == TOFILE)  {
		if  ((strlen(mfv_directory)+strlen(mfv_fname[id])) > BC_FILELTH)  {
			*status = MFE_STROVFL;
			return;
		} /*endif*/
		strcpy( name, mfv_directory );
		strcat( name, mfv_fname[id] );
		sy_fdelete( name );
	} else if  (mfv_switch[id] == TOMEM)  {
		lseg = mfv[id].wseg;
		while  (lseg->prev != NULL)
			lseg = lseg->prev;
		while  (lseg != NULL)  {
			mfv[id].wseg = lseg->next;
			sy_deallocmem( lseg );
			lseg = mfv[id].wseg;
		} /*endwhile*/
		mfv[id].rp = NULL;
		mfv[id].wp = NULL;
		mfv[id].rseg = NULL;
	} /*endif*/
	mfv_stat[id] = DELETED;

	mfv_switch[id] = TONONE;

} /* end of mf_delete */



/*-------------------------------------------------------------------------*/



BOOLEAN mf_isopen( unsigned id )

/* returns whther or not file "id" is opened
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 *                     returns TRUE if file is opened
 */
{
	/* executable code */

	if  (id >= MFC_MAXMF)  return FALSE;
	return (mfv_stat[id] == OPENED);

} /* end of mf_isopen */



/*-------------------------------------------------------------------------*/



void mf_writereal( unsigned id, MFC_REAL r, int *status )

/* write real number to file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * MFC_REAL  r;        input; number
 * int       *status;  output; return status
 */
{
	/* local variables */
	MFC_REAL  *pt;

	/* executable code */

	if  (id >= MFC_MAXMF)  {
		*status = MFE_ILID;
		return;
	} /*endif*/

	if  (mfv_switch[id] == TOMEM)  {
		if  (((MFC_REAL *)(mfv[id].wp)+1) > (MFC_REAL *)mfv[id].wseg->end)  {
			mf_nextwriteseg( mfv+id, status );
			if  (Severe(status))  return;
		} /*endif*/
		pt = mfv[id].wp;  *pt++ = r;  mfv[id].wp = pt;
		/* *((MFC_REAL *)(mfv[id].wp))++ = r; HP! */
	} else if  (mfv_switch[id] == TOFILE)  {
		sy_fbwrite( &r, (int)sizeof(MFC_REAL), 1L, mfv_file[id] );
	} else {
#ifdef XXX
		mf_open( id, mfv_defswitch, "a", status );
		if  (*status != MFE_NOERROR)  return;
		mf_writereal( id, r, status );
#endif
		/* fprintf( stderr, "mf_writereal: ignored output for chan %d\n", id ); */
	} /*endif*/

} /* end of mf_writereal */



/*-------------------------------------------------------------------------*/



void mf_writeint( unsigned id, int i, int *status )

/* write integer number to file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * int       i;        input; number
 * int       *status;  output; return status
 */
{
	/* local variables */
	int      *pt;

	/* executable code */

	if  (id >= MFC_MAXMF)  {
		*status = MFE_ILID;
		return;
	} /*endif*/

	if  (mfv_switch[id] == TOMEM)  {
		if  (((int *)(mfv[id].wp)+1) > (int *)mfv[id].wseg->end)  {
			mf_nextwriteseg( mfv+id, status );
			if  (Severe(status))  return;
		} /*endif*/
		pt = mfv[id].wp;  *pt++ = i;  mfv[id].wp = pt;
		/* *((int *)(mfv[id].wp))++ = i;  HP! */
	} else if  (mfv_switch[id] == TOFILE)  {
		sy_fbwrite( &i, (int)sizeof(int), 1L, mfv_file[id] );
	} else {
#ifdef XXX
		mf_open( id, mfv_defswitch, "a", status );
		if  (*status != MFE_NOERROR)  return;
		mf_writeint( id, i, status );
#endif
		/* fprintf( stderr, "mf_writeint: ignored output for chan %d\n", id ); */
	} /*endif*/

} /* end of mf_writeint */



/*-------------------------------------------------------------------------*/



void mf_writelong( unsigned id, long l, int *status )

/* write long integer number to file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * long      l;        input; number
 * int       *status;  output; return status
 */
{
	/* local variables */
	long     *pt;

	/* executable code */

	if  (id >= MFC_MAXMF)  {
		*status = MFE_ILID;
		return;
	} /*endif*/

	if  (mfv_switch[id] == TOMEM)  {
		if  (((long *)(mfv[id].wp)+1) > (long *)mfv[id].wseg->end)  {
			mf_nextwriteseg( mfv+id, status );
			if  (Severe(status))  return;
		} /*endif*/
		pt = mfv[id].wp;  *pt++ = l;  mfv[id].wp = pt;
		/* *((long *)(mfv[id].wp))++ = l;  HP! */
	} else if  (mfv_switch[id] == TOFILE)  {
		sy_fbwrite( &l, (int)sizeof(long), 1L, mfv_file[id] );
	} else {
#ifdef XXX
		mf_open( id, mfv_defswitch, "a", status );
		if  (*status != MFE_NOERROR)  return;
		mf_writelong( id, l, status );
#endif
		/* fprintf( stderr, "mf_writelong: ignored output for chan %d\n", id ); */
	} /*endif*/

} /* end of mf_writelong */



/*-------------------------------------------------------------------------*/



void mf_writechar( unsigned id, char c, int *status )

/* write character to file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * char      c;        input; character
 * int       *status;  output; return status
 */
{
	/* executable code */

	mf_writeint( id, (int)c, status );

} /* end of mf_writechar */



/*-------------------------------------------------------------------------*/



void mf_writestr( unsigned id, char *str, int *status )

/* writes string to file
 *
 * parameters of routine
 * unsigned   id;        input; file ID
 * char       *str;      input; sring to be written
 * int        *status;   output; return status
 */
{
	/* executable code */

	do  {
		mf_writeint( id, (int)(*str), status );
		if  (*status != MFE_NOERROR)  return;
	}  while  (*str++ != '\0');

} /* end of mf_writestr */



/*-------------------------------------------------------------------------*/



MFC_REAL mf_readreal( unsigned id, int *status )

/* read real number from file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * int       *status;  output; return status
 *                     returns read value
 */
{
	/* local variables */
	MFC_REAL     r;
	MFC_REAL     *pt;

	/* executable code */

	if  (id >= MFC_MAXMF)  {
		*status = MFE_ILID;
		return 0.0;
	} /*endif*/

	if  (mfv_switch[id] == TOMEM)  {
		if  (mfv[id].wseg == mfv[id].rseg)  {
			if  (((MFC_REAL *)(mfv[id].rp)+1) > (MFC_REAL *)mfv[id].wp)  {
				*status = MFE_ENDMEM;
				return 0.0;
			} /*endif*/
		} else {
			if  (((MFC_REAL *)(mfv[id].rp)+1) > (MFC_REAL *)mfv[id].rseg->end)  {
				mf_nextreadseg( mfv+id, status );
				if  (Severe(status))  return 0.0;
			} /*endif*/
		} /*endif*/
		pt = mfv[id].rp;  r = *pt++;  mfv[id].rp = pt;
		/* r = *((MFC_REAL *)(mfv[id].rp))++;  HP! */
	} else if  (mfv_switch[id] == TOFILE)  {
		sy_fbread( &r, (int)sizeof(MFC_REAL), 1L, mfv_file[id] );
	} else {
		*status = MFE_NOTOPEN;
	} /*endif*/

	return r;

} /* end of mf_readreal */



/*-------------------------------------------------------------------------*/



int mf_readint( unsigned id, int *status )

/* read integer number from file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * int       *status;  output; return status
 *                     returns read value
 */
{
	/* local variables */
	int     i;
	int     *pt;

	/* executable code */

	if  (id >= MFC_MAXMF)  {
		*status = MFE_ILID;
		return 0;
	} /*endif*/

	if  (mfv_switch[id] == TOMEM)  {
		if  (mfv[id].wseg == mfv[id].rseg)  {
			if  (((int *)(mfv[id].rp)+1) > (int *)mfv[id].wp)  {
				*status = MFE_ENDMEM;
				return 0;
			} /*endif*/
		} else {
			if  (((int *)(mfv[id].rp)+1) > (int *)mfv[id].rseg->end)  {
				mf_nextreadseg( mfv+id, status );
				if  (Severe(status))  return 0;
			} /*endif*/
		} /*endif*/
		pt = mfv[id].rp;  i = *pt++;  mfv[id].rp = pt;
		/* i = *((int *)(mfv[id].rp))++;  HP! */
	} else if  (mfv_switch[id] == TOFILE)  {
		sy_fbread( &i, (int)sizeof(int), 1L, mfv_file[id] );
	} else {
		*status = MFE_NOTOPEN;
	} /*endif*/

	return i;

} /* end of mf_readint */



/*-------------------------------------------------------------------------*/



long mf_readlong( unsigned id, int *status )

/* read long integer number from file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * int       *status;  output; return status
 *                     returns read value
 */
{
	/* local variables */
	long    l;
	long    *pt;

	/* executable code */

	if  (id >= MFC_MAXMF)  {
		*status = MFE_ILID;
		return 0;
	} /*endif*/

	if  (mfv_switch[id] == TOMEM)  {
		if  (mfv[id].wseg == mfv[id].rseg)  {
			if  (((long *)(mfv[id].rp)+1) > (long *)mfv[id].wp)  {
				*status = MFE_ENDMEM;
				return 0L;
			} /*endif*/
		} else {
			if  (((long *)(mfv[id].rp)+1) > (long *)mfv[id].rseg->end)  {
				mf_nextreadseg( mfv+id, status );
				if  (Severe(status))  return 0L;
			} /*endif*/
		} /*endif*/
		pt = mfv[id].rp;  l = *pt++;  mfv[id].rp = pt;
		/* l = *((long *)(mfv[id].rp))++;  HP! */
	} else if  (mfv_switch[id] == TOFILE)  {
		sy_fbread( &l, (int)sizeof(long), 1L, mfv_file[id] );
	} else {
		*status = MFE_NOTOPEN;
	} /*endif*/

	return l;

} /* end of mf_readlong */



/*-------------------------------------------------------------------------*/



char mf_readchar( unsigned id, int *status )

/* read character from file
 *
 * parameter of routine
 * unsigned  id;       input; file ID
 * int       *status;  output; return status
 *                     returns read value
 */
{
	/* executable code */

	return (char)mf_readint( id, status );

} /* end of mf_readchar */



/*-------------------------------------------------------------------------*/



void mf_readstr( unsigned id, char *str, int *status )

/* reads string from file
 *
 * parameters of routine
 * unsigned   id;        input; file ID
 * char       *str;      output; read string
 * int        *status;   output; return status
 */
{
	/* executable code */

	do  {
		*str = (char)mf_readint( id, status );
		if  (*status != MFE_NOERROR)  return;
	}  while  (*str++ != '\0');

} /* end of readstr */



/*-------------------------------------------------------------------------*/



void mf_nextwriteseg( MEMFILE *mf, STATUS *status )

/* creates new memory segment if necessary and sets up write pointer
 *
 * parameters of routine
 * MEMFILE    *mf;       input; memfile pointer
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	SEGMENT  *lseg;       /* pointer to segment */

	/* executable code */

	if  (mf->wseg->next == NULL)  {    /* create new segment */
		lseg = sy_allocmem( ALLOCLTH+sizeof(SEGMENT), 1, status );
		if  (Severe(status))  return;
		lseg->mem = (MEMUNIT *)lseg + sizeof(SEGMENT);
		lseg->end = (MEMUNIT *)(lseg->mem) + ALLOCLTH;
		lseg->next = NULL;
		lseg->prev = mf->wseg;
		mf->wseg->next = lseg;
	} /*endif*/
	mf->wseg = mf->wseg->next;
	mf->wp = mf->wseg->mem;

} /* end of mf_nextwriteseg */



/*-------------------------------------------------------------------------*/



void mf_nextreadseg( MEMFILE *mf, STATUS *status )

/* updates read pointer
 *
 * parameters of routine
 * MEMFILE    *mf;       input; memfile pointer
 * STATUS     *status;   output; return status
 */
{
	/* executable code */

	if  (mf->rseg->next == NULL)  {    /* read error */
		*status = MFE_BUFOVFL;
		return;
	} /*endif*/
	mf->rseg = mf->rseg->next;
	mf->rp = mf->rseg->mem;

} /* end of mf_nextreadseg */



/*-------------------------------------------------------------------------*/


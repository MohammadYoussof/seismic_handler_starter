
/* File SHDATABA.C
 *      ==========
 *
 * version 15, 22-May-2006
 *
 * data base of seismhandler program
 * K. Stammler, 6-MAR-1990
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include "shconst.h"
#include "shvars.h"
#include "qfusrdef.h"
#include "qiusrdef.h"
#include "tcusrdef.h"
#include BC_GCUSRDEF
#include "infoidx.h"
#include "sherrors.h"

#define MLEMPTY QFC_LEMPTY
#define MIEMPTY QFC_IEMPTY
#define MBEMPTY 0x7f
#define MCEMPTY '\0'
#define MTEMPTY 0L
#define MPEMPTY NULL

#define LISTCNT 0
#define DISPCNT 1

#define NODLN  -1
#define TEKDLN SHC_MAXWDW
#define CCDLN  (SHC_MAXWDW+1)
#define MEMDLN (SHC_MAXWDW+2)


/* global variables */
static MEMBLC       root_dbv[SHC_MAXDLN];        /* root pointer */
static MEMBLC       *crelist_dbv[SHC_ILISTLTH];  /* list of created */
static int          crelistlth_dbv;              /* length of list */



/*------------------------------------------------------------------------*/



void db_newlist( void )

/* resets list of created traces
 *
 * no parameters
 */
{  crelistlth_dbv = 0;

} /* end of db_newlist */



/*------------------------------------------------------------------------*/



MEMBLC *db_create( STATUS *status )

/* enlists new info block
 *
 * parameters of routine
 * STATUS   *status;        output; return status
 *                          returns info block pointer
 */
{
	/* local variables */
	MEMBLC   *cp;      /* pointer to current info block */
	MEMBLC   *p;       /* pointer to new info block */
	int      i;        /* counter */

	/* executable code */

	p = (MEMBLC *)sy_allocmem( 1L, (int)sizeof(MEMBLC), status );
	if  (Severe(status))  return NULL;
	/* initialise entries */
	for  (i=0;i<EMAX_LONG; p->lm[i++]=MLEMPTY ){}
	for  (i=0;i<EMAX_INT; p->im[i++]=MIEMPTY ){}
	for  (i=0;i<EMAX_BYTE; p->bm[i++]=MBEMPTY ){}
	for  (i=0;i<EMAX_REAL; qf_rsetempty( &(p->rm[i++]) )){}
	for  (i=0;i<EMAX_STR; p->sm[i++][0]=MCEMPTY ){}
	for  (i=0;i<EMAX_CHAR; p->cm[i++]=MCEMPTY ){}
	for  (i=0;i<EMAX_TIME; tc_setempty(&(p->tm[i++])) ){}
	for  (i=0;i<EMAX_PTR; p->pm[i++]=NULL ){}
	p->fm = 0;

	/* append to basic list */
	if  (root_dbv[0].pm[EPN_NEXT] == NULL)  {
		p->pm[EPN_NEXT] = NULL;
		p->pm[EPN_PREV] = NULL;
		root_dbv[0].pm[EPN_NEXT] = p;
	} else {
		cp = root_dbv[0].pm[EPN_NEXT];
		while  (cp->pm[EPN_NEXT] != NULL)  {
			cp = cp->pm[EPN_NEXT];
		} /*endwhile*/
		p->pm[EPN_NEXT] = NULL;
		p->pm[EPN_PREV] = cp;
		cp->pm[EPN_NEXT] = p;
	} /*endif*/

	root_dbv[0].im[LISTCNT]++;
	if  (crelistlth_dbv == SHC_ILISTLTH)  {
		*status = SHE_TLOVFL;
		return p;
	} /*endif*/
	crelist_dbv[crelistlth_dbv++] = p;
	return p;

} /* end of db_create */



/*------------------------------------------------------------------------*/



void db_enlist( MEMBLC *p, int pos )

/* enlists info block in current active display list
 *
 * parameters of routine
 * MEMBLC   *p;             input; info block to enlist
 * int      pos;            input; list position
 */
{
	/* local variables */
	MEMBLC   *cp;      /* current info block */
	int      dln;      /* display list number */

	/* executable code */

	dln = db_getdln( gc );
	if  (dln < 0)  return;

	/* insert in display list */
	if  ((root_dbv[dln].pm[EPN_DSPN] == NULL) ||  (pos == 1))  {
		p->pm[EPN_DSPN] = root_dbv[dln].pm[EPN_DSPN];
		p->pm[EPN_DSPP] = NULL;
		if  (p->pm[EPN_DSPN] != NULL)
			((MEMBLC *)p->pm[EPN_DSPN])->pm[EPN_DSPP] = p;
		root_dbv[dln].pm[EPN_DSPN] = p;
	} else {
		cp = root_dbv[dln].pm[EPN_DSPN];
		while  (--pos > 1)  {
			if  (cp->pm[EPN_DSPN] == NULL)  break;
			cp = cp->pm[EPN_DSPN];
		} /*endwhile*/
		p->pm[EPN_DSPN] = cp->pm[EPN_DSPN];
		p->pm[EPN_DSPP] = cp;
		cp->pm[EPN_DSPN] = p;
		if  (p->pm[EPN_DSPN] != NULL)
			((MEMBLC *)p->pm[EPN_DSPN])->pm[EPN_DSPP] = p;
	} /*endif*/

	root_dbv[dln].im[DISPCNT]++;

} /* end of db_enlist */



/*------------------------------------------------------------------------*/



void db_delete( MEMBLC *ptr )

/* removes info block from memory list
 *
 * parameters of routine
 * MEMBLC   *ptr;           pointer to block to be removed
 */
{
	/* executable code */

	if  (ptr->pm[EPN_PREV] == NULL)  {  /* first block */
		root_dbv[0].pm[EPN_NEXT] = ptr->pm[EPN_NEXT];
	} else {
		((MEMBLC *)(ptr->pm[EPN_PREV]))->pm[EPN_NEXT] = ptr->pm[EPN_NEXT];
	} /*endif*/
	if  (ptr->pm[EPN_NEXT] != NULL)
		((MEMBLC *)(ptr->pm[EPN_NEXT]))->pm[EPN_PREV] = ptr->pm[EPN_PREV];

	sy_deallocmem( ptr );
	root_dbv[0].im[LISTCNT]--;
	crelistlth_dbv = 0;

} /* end of db_delete */



/*------------------------------------------------------------------------*/



void db_delist( CHMAP map, MEMBLC *ptr )

/* removes info block "*ptr" from display list
 *
 * parameters of routine
 * CHMAP         map;       input; channel map
 * MEMBLC        *ptr;      input; info block to be removed
 */
{
	/* local variables */
	int      dln;      /* display list number */

	/* executable code */

	if  (!db_islisted(map,ptr))  return;  /* not in display list */
	dln = db_getdln( map );

	if  (ptr->pm[EPN_DSPP] == NULL)  {  /* first block */
		root_dbv[dln].pm[EPN_DSPN] = ptr->pm[EPN_DSPN];
	} else {
		((MEMBLC *)(ptr->pm[EPN_DSPP]))->pm[EPN_DSPN] = ptr->pm[EPN_DSPN];
	} /*endif*/
	if  (ptr->pm[EPN_DSPN] != NULL)
		((MEMBLC *)(ptr->pm[EPN_DSPN]))->pm[EPN_DSPP] = ptr->pm[EPN_DSPP];
	ptr->pm[EPN_DSPN] = NULL;
	ptr->pm[EPN_DSPP] = NULL;

	root_dbv[dln].im[DISPCNT]--;

} /* end of db_delist */



/*------------------------------------------------------------------------*/



void db_createdlist( void *trclist[], int *listlth )

/* returns list of created traces.  If trclist==NULL only listlth is
 * returned.
 *
 * parameters of routine
 * void       *trclist;    output; list of created traces if not NULL
 * int        *listlth;    output; length of list
 */
{
	/* local variables */
	int      i;       /* counter */

	/* executable code */

	*listlth = crelistlth_dbv;
	if  (trclist == NULL)  return;
	for  (i=0;i<crelistlth_dbv;i++)
		trclist[i] = crelist_dbv[i];

} /* end of db_createdlist */



/*------------------------------------------------------------------------*/



BOOLEAN db_islisted( CHMAP map, MEMBLC *ptr )

/* checks whether or not "ptr" is in the display list "dln"
 *
 * parameters of routine
 * CHMAP       map;       input; channel map
 * MEMBLC      *ptr;      input; trace pointer
 */
{
	/* local variables */
	MEMBLC   *m;       /* moving pointer */
	int      dln;      /* display list number */

	/* executable code */

	dln = db_getdln( map );
	if  (dln < 0)  return FALSE;
	m = (MEMBLC *)root_dbv[dln].pm[EPN_DSPN];
	while  (m != NULL)  {
		if  (m == ptr)  return TRUE;
		m = (MEMBLC *)m->pm[EPN_DSPN];
	} /*endwhile*/
	return FALSE;

} /* end of db_islisted */



/*------------------------------------------------------------------------*/



BOOLEAN db_hidden( MEMBLC *ptr )

/* checks whether or not "ptr" is in any display list
 *
 * parameter of routine
 * MEMBLC      *ptr;      input; trace pointer
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */
	for  (i=0;i<SHC_MAXDLN;i++)
		if  (root_dbv[i].pm[EPN_DSPN] == ptr)  return FALSE;

	return  (ptr->pm[EPN_DSPP] == NULL);

} /* end of db_hidden */



/*------------------------------------------------------------------------*/



int db_gettrcpos( MEMBLC *ptr )

/* returns position number of "ptr".
 *
 * parameters of routine
 * MEMBLC      *ptr;      input; trace pointer
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	i = 1;
	while  (ptr->pm[EPN_DSPP] != NULL)  {
		ptr = ptr->pm[EPN_DSPP];
		i++;
	} /*endwhile*/
	return i;

} /* end of db_gettrcpos */



/*------------------------------------------------------------------------*/



int db_dsplth( CHMAP map )

/* returns number of traces in display
 *
 * parameter of routine
 * CHMAP     map;      input; channel map
 */
{
	/* local variables */
	int      dln;      /* display list number */

	/* executable code */

	dln = db_getdln( map );
	if  (dln < 0)  return 0;
	return root_dbv[dln].im[DISPCNT];

} /* end of db_dsplth */



/*------------------------------------------------------------------------*/



int db_lstlth( void )

/* returns number of traces in memory
 *
 * no parameters
 */
{ return root_dbv[0].im[LISTCNT];
} /* end of db_lstlth */



/*------------------------------------------------------------------------*/



MEMBLC *db_dspfirst( CHMAP map, int *status )

/* returns first trace of display list "dln"
 *
 * parameters of routine
 * CHMAP      map;       input; channel map
 * int        *status;   output; return status (if status != NULL)
 */
{
	/* local variables */
	int      dln;      /* display list number */

	/* executable code */

	dln = db_getdln( map );
	if  (dln < 0)  {
		*status = SHE_EMPTYLST;
		return NULL;
	} /*endif*/
	if  ((status != NULL) && (root_dbv[dln].pm[EPN_DSPN] == NULL))
		*status = SHE_EMPTYLST;
	return (MEMBLC *)root_dbv[dln].pm[EPN_DSPN];

} /* end of db_dspfirst */



/*------------------------------------------------------------------------*/



int db_getdln( CHMAP map )

/* returns display list number from channel map
 * parameters of routine
 * CHMAP      map;      input; channel map
 *                      returns display list number (-1 = no display list)
 */
{
	/* executable code */

	if  (map == 0)  return MEMDLN;
	if  (GCF_TEK & map)  return TEKDLN;
	if  (GCF_CALCOMP & map)  return CCDLN;
	return (map & GCF_NUMMASK);

} /* end of db_getdln */



/*------------------------------------------------------------------------*/



long db_getl( MEMBLC *ptr, unsigned ientry, STATUS *status )

/* returns long info value
 *
 * parameters of routine
 * MEMBLC   *ptr;           input; info block pointer
 * unsigned ientry;         input; info entry
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char      qfile[BC_LINELTH+1];   /* source q-file name */

	/* executable code */

	if  ((ientry & E_TYPMASK) != EL_TYPE)  {
		if  (status != NULL)  *status = SHE_TYPMTCH;
		return MLEMPTY;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry >= EMAX_LONG)  {
		/* read from q-file */
		db_gets( ptr, ES_FILE, BC_LINELTH, qfile, status );
		if  (*status != SHE_NOERROR)  return MLEMPTY;
		qf_read( qfile, db_geti(ptr,EI_RECNO,NULL), FALSE, NULL, status );
		if  (*status != SHE_NOERROR)  return MLEMPTY;
		return qf_getl( qi_cnvlidx(EL_TYPE|ientry,status), status );
	} else {
		if  ((ptr->lm[ientry] == MLEMPTY) && (status != NULL))
			*status = SHE_NOINFO;
		return (ptr->lm[ientry]);
	} /*endif*/

} /* end of db_getl */



/*------------------------------------------------------------------------*/



int db_geti( MEMBLC *ptr, unsigned ientry, STATUS *status )

/* returns int info value
 *
 * parameters of routine
 * MEMBLC   *ptr;           input; info block pointer
 * unsigned ientry;         input; info entry
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char      qfile[BC_LINELTH+1];   /* source q-file name */

	/* executable code */

	if  ((ientry & E_TYPMASK) != EI_TYPE)  {
		if  (status != NULL)  *status = SHE_TYPMTCH;
		return MIEMPTY;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry >= EMAX_INT)  {
		/* read from q-file */
		db_gets( ptr, ES_FILE, BC_LINELTH, qfile, status );
		if  (*status != SHE_NOERROR)  return MIEMPTY;
		qf_read( qfile, db_geti(ptr,EI_RECNO,NULL), FALSE, NULL, status );
		if  (*status != SHE_NOERROR)  return MIEMPTY;
		return qf_geti( qi_cnviidx(EI_TYPE|ientry,status), status );
	} else {
		if  ((ptr->im[ientry] == MIEMPTY) && (status != NULL))
			*status = SHE_NOINFO;
		return (ptr->im[ientry]);
	} /*endif*/

} /* end of db_geti */



/*------------------------------------------------------------------------*/



BYTE db_getb( MEMBLC *ptr, unsigned ientry, STATUS *status )

/* returns byte info value
 *
 * parameters of routine
 * MEMBLC   *ptr;           input; info block pointer
 * unsigned ientry;         input; info entry
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char      qfile[BC_LINELTH+1];   /* source q-file name */

	/* executable code */

	if  ((ientry & E_TYPMASK) != EB_TYPE)  {
		if  (status != NULL)  *status = SHE_TYPMTCH;
		return MBEMPTY;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry >= EMAX_BYTE)  {
		/* read from q-file */
		db_gets( ptr, ES_FILE, BC_LINELTH, qfile, status );
		if  (*status != SHE_NOERROR)  return MBEMPTY;
		qf_read( qfile, db_geti(ptr,EI_RECNO,NULL), FALSE, NULL, status );
		if  (*status != SHE_NOERROR)  return MBEMPTY;
		return (BYTE)qf_geti( qi_cnvbidx(EB_TYPE|ientry,status), status );
	} else {
		if  ((ptr->bm[ientry] == MBEMPTY) && (status != NULL))
			*status = SHE_NOINFO;
		return (ptr->bm[ientry]);
	} /*endif*/

} /* end of db_getb */



/*------------------------------------------------------------------------*/



float db_getr( MEMBLC *ptr, unsigned ientry, STATUS *status )

/* returns float info value
 *
 * parameters of routine
 * MEMBLC   *ptr;           input; info block pointer
 * unsigned ientry;         input; info entry
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char      qfile[BC_LINELTH+1];   /* source q-file name */

	/* executable code */

	if  ((ientry & E_TYPMASK) != ER_TYPE)  {
		if  (status != NULL)  *status = SHE_TYPMTCH;
		return 0.;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry >= EMAX_REAL)  {
		/* read from q-file */
		db_gets( ptr, ES_FILE, BC_LINELTH, qfile, status );
		if  (*status != SHE_NOERROR)  return 0.;
		qf_read( qfile, db_geti(ptr,EI_RECNO,NULL), FALSE, NULL, status );
		if  (*status != SHE_NOERROR)  return 0.;
		return qf_getr( qi_cnvridx(ER_TYPE|ientry,status), status );
	} else {
		if  (qf_risempty(&(ptr->rm[ientry])))  {
			if  (status != NULL)  *status = SHE_NOINFO;
			return 0.;
		} /*endif*/
		return ptr->rm[ientry];
	} /*endif*/

} /* end of db_getr */



/*------------------------------------------------------------------------*/



void db_gets( MEMBLC *ptr, unsigned ientry, int maxlth, char str[],
	STATUS *status )

/* returns string info value
 *
 * parameters of routine
 * MEMBLC   *ptr;           input; info block pointer
 * unsigned ientry;         input; info entry
 * int      maxlth;         input; maximum length of output string
 * char     str[];          output; output string
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char      qfile[BC_LINELTH+1];   /* source q-file name */
	char      *sptr;                 /* string pointer */

	/* executable code */

	*str = '\0';
	if  ((ientry & E_TYPMASK) != ES_TYPE)  {
		if  (status != NULL)  *status = SHE_TYPMTCH;
		return;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry >= EMAX_STR)  {
		/* read from q-file */
		db_gets( ptr, ES_FILE, BC_LINELTH, qfile, status );
		if  (*status != SHE_NOERROR)  return;
		qf_read( qfile, db_geti(ptr,EI_RECNO,NULL), FALSE, NULL, status );
		if  (*status != SHE_NOERROR)  return;
		sptr = qf_gets( qi_cnvsidx(ES_TYPE|ientry,status), status );
		if  (Severe(status))  return;
		if  (strlen(sptr) > maxlth)  { *status = SHE_STRENT; return; }
		strcpy( str, sptr );
	} else {
		if  ((ptr->sm[ientry][0] == MCEMPTY) && (status != NULL))  {
			*status = SHE_NOINFO;
			return;
		} else if  (strlen(ptr->sm[ientry]) > maxlth)  {
			*status = SHE_STRENT;
			return;
		} /*endif*/
		strcpy( str, ptr->sm[ientry] );
	} /*endif*/

} /* end of db_gets */



/*------------------------------------------------------------------------*/



char db_getc( MEMBLC *ptr, unsigned ientry, STATUS *status )

/* returns char info value
 *
 * parameters of routine
 * MEMBLC   *ptr;           input; info block pointer
 * unsigned ientry;         input; info entry
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char      qfile[BC_LINELTH+1];   /* source q-file name */

	/* executable code */

	if  ((ientry & E_TYPMASK) != EC_TYPE)  {
		if  (status != NULL)  *status = SHE_TYPMTCH;
		return MCEMPTY;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry >= EMAX_CHAR)  {
		/* read from q-file */
		db_gets( ptr, ES_FILE, BC_LINELTH, qfile, status );
		if  (*status != SHE_NOERROR)  return MCEMPTY;
		qf_read( qfile, db_geti(ptr,EI_RECNO,NULL), FALSE, NULL, status );
		if  (*status != SHE_NOERROR)  return MCEMPTY;
		return qf_getc( qi_cnvcidx(EC_TYPE|ientry,status), status );
	} else {
		if  ((ptr->cm[ientry] == MCEMPTY) && (status != NULL))
			*status = SHE_NOINFO;
		return (ptr->cm[ientry]);
	} /*endif*/

} /* end of db_getc */



/*------------------------------------------------------------------------*/



void db_gett( MEMBLC *ptr, unsigned ientry, TIME *time, STATUS *status )

/* returns time info value
 *
 * parameters of routine
 * MEMBLC     *ptr;           input; info block pointer
 * unsigned   ientry;         input; info entry
 * TIME       *time;          output; output time
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	char      qfile[BC_LINELTH+1];   /* source q-file name */
	char      *sptr;                 /* pointer to q-info */

	/* executable code */

	if  ((ientry & E_TYPMASK) != ET_TYPE)  {
		if  (status != NULL)  *status = SHE_TYPMTCH;
		return;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry >= EMAX_TIME)  {
		/* read from q-file */
		db_gets( ptr, ES_FILE, BC_LINELTH, qfile, status );
		if  (Severe(status))  return;
		qf_read( qfile, db_geti(ptr,EI_RECNO,NULL), FALSE, NULL, status );
		if  (Severe(status))  return;
		sptr = qf_gets( qi_cnvtidx(ET_TYPE|ientry,status), status );
		if  (Severe(status))  return;
		tc_t2a( sptr, time, status );
	} else {
		if  (tc_isempty(&(ptr->tm[ientry])) && (status != NULL))  {
			*status = SHE_NOINFO;
			return;
		} /*endif*/
		*time = ptr->tm[ientry];
	} /*endif*/

} /* end of db_gett */



/*------------------------------------------------------------------------*/



void *db_getp( MEMBLC *ptr, unsigned ientry, STATUS *status )

/* returns pointer info value
 * 
 * parameters of routine
 * MEMBLC   *ptr;           input; info block pointer
 * unsigned ientry;         input; info entry
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      dln;      /* display list number */

	/* executable code */

	if  ((ientry & E_TYPMASK) != EP_TYPE)  {
		*status = SHE_TYPMTCH;
		return NULL;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry >= EMAX_PTR)  {
		return MPEMPTY;
		/* read from q-file */
	} else {
		if  (ptr == NULL)  {
			dln = (ientry == EPN_NEXT) ? 0 : db_getdln(gc);
			ptr = root_dbv + dln;
		} /*endif*/
		if  ((ptr->pm[ientry] == MPEMPTY) && (status != NULL))
			*status = SHE_NOINFO;
		return ptr->pm[ientry];
	} /*endif*/

} /* end of db_getp */



/*------------------------------------------------------------------------*/



BOOLEAN db_getf( MEMBLC *ptr, unsigned ientry, STATUS *status )

/* returns flag value
 *
 * parameters of routine
 * MEMBLC   *ptr;           input; info block pointer
 * unsigned ientry;         input; info entry
 * int      *status;        output; return status
 */
{
	/* executable code */

	if  ((ientry & E_TYPMASK) != EF_TYPE)  {
		if  (status != NULL)  *status = SHE_TYPMTCH;
		return FALSE;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry >= EMAX_FLAG)  {
		*status = SHE_NOINFO;
		return FALSE;
	} else {
		return (ptr->fm & (1 << ientry));
	} /*endif*/

} /* end of db_getf */



/*------------------------------------------------------------------------*/



void db_getx( MEMBLC *ptr, unsigned ientry, int maxlth, char str[],
	STATUS *status )

/* returns special info
 *
 * parameters of routine
 * MEMBLC   *ptr;           input; info block pointer
 * unsigned ientry;         input; info entry
 * int      maxlth;         input; maximum length of output string
 * char     str[];          output; output string
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char      scr[BC_LINELTH+1];     /* scratch string */
	TIME      atime;                 /* absolute time */

	/* executable code */

	*str = '\0';
	if  ((ientry & E_TYPMASK) != EX_TYPE)  {
		if  (status != NULL)  *status = SHE_TYPMTCH;
		return;
	} /*endif*/

	if  (ientry == EX_DATE)  {
		if  (maxlth < 11)  {
			*status = SHE_STROVFL;
			return;
		} /*endif*/
		db_gett( ptr, ET_START, &atime, status );
		if  (Severe(status))  return;
		tc_a2t( &atime, scr, status );
		if  (Severe(status))  return;
		strncpy( str, scr, 11 );
		str[11] = '\0';
	} else if  (ientry == EX_TIME)  {
		db_gett( ptr, ET_START, &atime, status );
		if  (Severe(status))  return;
		tc_a2t( &atime, scr, status );
		if  (Severe(status))  return;
		strncpy( str, scr+12, maxlth );
		str[maxlth] = '\0';
	} else {
		*status = SHE_UDNAME;
		return;
	} /*endif*/

} /* end of db_getx */



/*------------------------------------------------------------------------*/



void db_setl( MEMBLC *ptr, unsigned ientry, long value, STATUS *status )

/* sets long value in info block "*ptr"
 *
 * parameters of routine
 * MEMBLC   *ptr;           modify; info block to be changed
 * unsigned ientry;         input; info entry number
 * long     value;          input; new value
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((ientry&E_TYPMASK) != EL_TYPE)  {
		*status = SHE_TYPMTCH;
		return;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry < EMAX_LONG)  ptr->lm[ientry] = value;

	/* write to q-file */

} /* end of db_setl */



/*------------------------------------------------------------------------*/



void db_seti( MEMBLC *ptr, unsigned ientry, int value, STATUS *status )

/* sets int value in info block "*ptr"
 *
 * parameters of routine
 * MEMBLC   *ptr;           modify; info block to be changed
 * unsigned ientry;         input; info entry number
 * int      value;          input; new value
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((ientry&E_TYPMASK) != EI_TYPE)  {
		*status = SHE_TYPMTCH;
		return;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry < EMAX_INT)  ptr->im[ientry] = value;

	/* write to q-file */

} /* end of db_seti */



/*------------------------------------------------------------------------*/



void db_setb( MEMBLC *ptr, unsigned ientry, BYTE value, STATUS *status )

/* sets byte value in info block "*ptr"
 *
 * parameters of routine
 * MEMBLC   *ptr;           modify; info block to be changed
 * unsigned ientry;         input; info entry number
 * BYTE     value;          input; new value
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((ientry&E_TYPMASK) != EB_TYPE)  {
		*status = SHE_TYPMTCH;
		return;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry < EMAX_BYTE)  ptr->bm[ientry] = value;

	/* write to q-file */

} /* end of db_setb */



/*------------------------------------------------------------------------*/



void db_setr( MEMBLC *ptr, unsigned ientry, float value, STATUS *status )

/* sets real value in info block "*ptr"
 *
 * parameters of routine
 * MEMBLC   *ptr;           modify; info block to be changed
 * unsigned ientry;         input; info entry number
 * float    value;          input; new value
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((ientry&E_TYPMASK) != ER_TYPE)  {
		*status = SHE_TYPMTCH;
		return;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry < EMAX_REAL)  ptr->rm[ientry] = value;

	/* write to q-file */

} /* end of db_setr */



/*------------------------------------------------------------------------*/



void db_sets( MEMBLC *ptr, unsigned ientry, char value[], STATUS *status )

/* sets string value in info block "*ptr"
 *
 * parameters of routine
 * MEMBLC   *ptr;           modify; info block to be changed
 * unsigned ientry;         input; info entry number
 * char     value[];        input; new value
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((ientry&E_TYPMASK) != ES_TYPE)  {
		*status = SHE_TYPMTCH;
		return;
	} else if  (strlen(value) > MSTRLTH)  {
		*status = SHE_STRENT;
		return;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry < EMAX_STR)  strcpy( ptr->sm[ientry], value );

	/* write to q-file */

} /* end of db_sets */



/*------------------------------------------------------------------------*/



void db_setc( MEMBLC *ptr, unsigned ientry, char value, STATUS *status )

/* sets char value in info block "*ptr"
 *
 * parameters of routine
 * MEMBLC   *ptr;           modify; info block to be changed
 * unsigned ientry;         input; info entry number
 * char     value;          input; new value
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((ientry&E_TYPMASK) != EC_TYPE)  {
		*status = SHE_TYPMTCH;
		return;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry < EMAX_CHAR)  ptr->cm[ientry] = value;

	/* write to q-file */

} /* end of db_setc */



/*------------------------------------------------------------------------*/



void db_sett( MEMBLC *ptr, unsigned ientry, TIME *value, STATUS *status )

/* sets time in info block "*ptr"
 *
 * parameters of routine
 * MEMBLC   *ptr;           modify; info block to be changed
 * unsigned ientry;         input; info entry number
 * TIME     *value;         input; new value
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((ientry&E_TYPMASK) != ET_TYPE)  {
		*status = SHE_TYPMTCH;
		return;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry < EMAX_TIME)  ptr->tm[ientry] = *value;

	/* write to q-file */

} /* end of db_sett */



/*------------------------------------------------------------------------*/



void db_setp( MEMBLC *ptr, unsigned ientry, void *value, STATUS *status )

/* sets pointer value in info block "*ptr"
 *
 * parameters of routine
 * MEMBLC   *ptr;           modify; info block to be changed
 * unsigned ientry;         input; info entry number
 * void     *value;         input; new value
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((ientry&E_TYPMASK) != EP_TYPE)  {
		*status = SHE_TYPMTCH;
		return;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry < EMAX_PTR)  ptr->pm[ientry] = value;

	/* write to q-file */

} /* end of db_setp */



/*------------------------------------------------------------------------*/



void db_setf( MEMBLC *ptr, unsigned ientry, BOOLEAN value, STATUS *status )

/* sets flag value in info block "*ptr"
 *
 * parameters of routine
 * MEMBLC   *ptr;           modify; info block to be changed
 * unsigned ientry;         input; info entry number
 * BOOLEAN  value;          input; new value
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((ientry&E_TYPMASK) != EF_TYPE)  {
		*status = SHE_TYPMTCH;
		return;
	} /*endif*/
	ientry &= E_IDXMASK;

	if  (ientry < EMAX_FLAG)  {
		if  (value)  {
			ptr->fm |= (1 << ientry);
		} else {
			ptr->fm &= ~(1 << ientry);
		} /*endif*/
	} /*endif*/

	/* write to q-file */

} /* end of db_setf */



/*------------------------------------------------------------------------*/



#define MAXNAMLTH 10
		  /* maximum length of info entry names */

/* maximum number of names per type */
#define MAXLNAMES 20
#define MAXINAMES 20
#define MAXBNAMES 5
#define MAXRNAMES 30
#define MAXSNAMES 20
#define MAXCNAMES 5
#define MAXTNAMES 20
#define MAXPNAMES 7
#define MAXFNAMES 12
#define MAXXNAMES 2

/* name variables */
char inam_l_dbv[MAXLNAMES][MAXNAMLTH+1]=    /* long names */
	{"LENGTH","ALLOC","DSPFST","DSPCNT"};
char inam_i_dbv[MAXINAMES][MAXNAMLTH+1]=    /* int names */
	{"RECNO","ATTRIB","REDUCTION"};
char inam_b_dbv[MAXBNAMES][MAXNAMLTH+1]=    /* byte names */
	{""};
char inam_r_dbv[MAXRNAMES][MAXNAMLTH+1]=    /* real names */
	{"DELTA","MAXVAL","MINVAL","NORM","ZOOM","T-ORIGIN","S-ORIGIN","WEIGHT"};
char inam_s_dbv[MAXSNAMES][MAXNAMLTH+1]=    /* string names */
	{"COMMENT","STATION","OPINFO","FILE"};
char inam_c_dbv[MAXCNAMES][MAXNAMLTH+1]=    /* char names */
	{"COMP","CHAN1","CHAN2"};
char inam_t_dbv[MAXTNAMES][MAXNAMLTH+1]=    /* time names */
	{"START"};
char inam_p_dbv[MAXPNAMES][MAXNAMLTH+1]=    /* ptr names */
	{"NEXT","PREV","DSPN","DSPP","DATA","STAT"};
char inam_f_dbv[MAXFNAMES][MAXNAMLTH+1]=    /* flag names */
	{"QUAL","NOCALIB","MODIF","FROMQ","OVERLAY","AMPLICUT","USFLG1","USFLG2"};
char inam_x_dbv[MAXXNAMES][MAXNAMLTH+1]=    /* special entry names */
	{"DATE","TIME"};
/* entry variables */
unsigned ient_l_dbv[MAXLNAMES]=             /* long entry numbers */
	{EL_LENGTH,EL_ALLOC,EL_DSPFST,EL_DSPCNT};
unsigned ient_i_dbv[MAXINAMES]=             /* int entry numbers */
	{EI_RECNO,EI_ATTRIB,EI_REDUCT};
unsigned ient_b_dbv[MAXBNAMES]=             /* byte entry numbers */
	{0};
unsigned ient_r_dbv[MAXRNAMES]=             /* real entry numbers */
	{ER_DELTA,ER_MAXVAL,ER_MINVAL,ER_NORM,ER_ZOOM,ER_TORIG,ER_SORIG,
	ER_WEIGHT};
unsigned ient_s_dbv[MAXSNAMES]=             /* string entry numbers */
	{ES_COMMENT,ES_STATION,ES_OPINFO,ES_FILE};
unsigned ient_c_dbv[MAXCNAMES]=             /* char entry numbers */
	{EC_COMP,EC_CHAN1,EC_CHAN2};
unsigned ient_t_dbv[MAXTNAMES]=             /* time entry numbers */
	{ET_START};
unsigned ient_p_dbv[MAXPNAMES]=             /* ptr entry numbers */
	{EP_NEXT,EP_PREV,EP_DSPN,EP_DSPP,EP_DATA,EP_STAT};
unsigned ient_f_dbv[MAXFNAMES]=             /* flag entry numbers */
	{EF_QUAL,EF_NOCALIB,EF_MODIF,EF_FROMQ,EF_OVERLAY,EF_AMPLICUT,EF_USFLG1,EF_USFLG2};
unsigned ient_x_dbv[MAXXNAMES]=             /* special entry numbers */
	{EX_DATE,EX_TIME};
/* info counter */
int icnt_l_dbv={4};                  /* number of long entries */
int icnt_i_dbv={3};                  /* number of int entries */
int icnt_b_dbv={0};                  /* number of byte entries */
int icnt_r_dbv={8};                  /* number of real entries */
int icnt_s_dbv={4};                  /* number of string entries */
int icnt_c_dbv={3};                  /* number of char entries */
int icnt_t_dbv={1};                  /* number of time entries */
int icnt_p_dbv={6};                  /* number of ptr entries */
int icnt_f_dbv={8};                  /* number of flag entries */
int icnt_x_dbv={2};                  /* number of special entries */




void db_ident( char name[], unsigned *ientry, STATUS *status )

/* identifies name of info entry and returns info entry index
 *
 * parameters of routine
 * char     name[];     input; name of info entry
 * unsigned *ientry;    output; info index
 * STATUS   *status;    output; return status
 */
{
	/* local variables */
	int      i;            /* counter */

	/* executable code */

	/* check long entries */
	for  (i=0;i<icnt_l_dbv;i++)  {
		if  (strcmp(name,inam_l_dbv[i]) == 0)  {
			*ientry = EL_TYPE|ient_l_dbv[i];
			return;
		} /*endif*/
	} /*endfor*/

	/* check int entries */
	for  (i=0;i<icnt_i_dbv;i++)  {
		if  (strcmp(name,inam_i_dbv[i]) == 0)  {
			*ientry = EI_TYPE|ient_i_dbv[i];
			return;
		} /*endif*/
	} /*endfor*/

	/* check byte entries */
	for  (i=0;i<icnt_b_dbv;i++)  {
		if  (strcmp(name,inam_b_dbv[i]) == 0)  {
			*ientry = EB_TYPE|ient_b_dbv[i];
			return;
		} /*endif*/
	} /*endfor*/

	/* check real entries */
	for  (i=0;i<icnt_r_dbv;i++)  {
		if  (strcmp(name,inam_r_dbv[i]) == 0)  {
			*ientry = ER_TYPE|ient_r_dbv[i];
			return;
		} /*endif*/
	} /*endfor*/

	/* check string entries */
	for  (i=0;i<icnt_s_dbv;i++)  {
		if  (strcmp(name,inam_s_dbv[i]) == 0)  {
			*ientry = ES_TYPE|ient_s_dbv[i];
			return;
		} /*endif*/
	} /*endfor*/

	/* check char entries */
	for  (i=0;i<icnt_c_dbv;i++)  {
		if  (strcmp(name,inam_c_dbv[i]) == 0)  {
			*ientry = EC_TYPE|ient_c_dbv[i];
			return;
		} /*endif*/
	} /*endfor*/

	/* check time entries */
	for  (i=0;i<icnt_t_dbv;i++)  {
		if  (strcmp(name,inam_t_dbv[i]) == 0)  {
			*ientry = ET_TYPE|ient_t_dbv[i];
			return;
		} /*endif*/
	} /*endfor*/

	/* check ptr entries */
	for  (i=0;i<icnt_p_dbv;i++)  {
		if  (strcmp(name,inam_p_dbv[i]) == 0)  {
			*ientry = EP_TYPE|ient_p_dbv[i];
			return;
		} /*endif*/
	} /*endfor*/

	/* check flag entries */
	for  (i=0;i<icnt_f_dbv;i++)  {
		if  (strcmp(name,inam_f_dbv[i]) == 0)  {
			*ientry = EF_TYPE|ient_f_dbv[i];
			return;
		} /*endif*/
	} /*endfor*/

	/* check special entries */
	for  (i=0;i<icnt_x_dbv;i++)  {
		if  (strcmp(name,inam_x_dbv[i]) == 0)  {
			*ientry = EX_TYPE|ient_x_dbv[i];
			return;
		} /*endif*/
	} /*endfor*/

	if  (strncmp(name,"ENTRY-",6) == 0)  {
		if  (sscanf(name+7,"%d",ientry) == 1)  {
			if  (name[6] == 'L')  {
				*ientry |= EL_TYPE;
				return;
			} else if  (name[6] == 'I')  {
				*ientry |= EI_TYPE;
				return;
			} else if  (name[6] == 'B')  {
				*ientry |= EB_TYPE;
				return;
			} else if  (name[6] == 'R')  {
				*ientry |= ER_TYPE;
				return;
			} else if  (name[6] == 'S')  {
				*ientry |= ES_TYPE;
				return;
			} else if  (name[6] == 'C')  {
				*ientry |= EC_TYPE;
				return;
			} else if  (name[6] == 'T')  {
				*ientry |= ET_TYPE;
				return;
			} else if  (name[6] == 'P')  {
				*ientry |= EP_TYPE;
				return;
			} else if  (name[6] == 'F')  {
				*ientry |= EF_TYPE;
				return;
			} else if  (name[6] == 'X')  {
				*ientry |= EX_TYPE;
				return;
			} /*endif*/
		} /*endif*/
	} /*endif*/

	*status = SHE_UDNAME;

} /* end of db_ident */



/*------------------------------------------------------------------------*/



char *db_entryname( unsigned ientry )

/* returns pointer to name of entry (don't change string, use as input only).
 * If entry is not defined NULL isreturned
 *
 * parameters of routine
 * unsigned      ientry;     input; entry number
 *                           returns pointer to entry name
 */
{
	/* executable code */

	ientry &= E_IDXMASK;

	switch  (ientry & E_TYPMASK)  {
	case EL_TYPE:
		if  (ientry >= icnt_l_dbv)  return NULL;
		return inam_l_dbv[ientry];
	case EI_TYPE:
		if  (ientry >= icnt_i_dbv)  return NULL;
		return inam_i_dbv[ientry];
	case EB_TYPE:
		if  (ientry >= icnt_b_dbv)  return NULL;
		return inam_b_dbv[ientry];
	case ER_TYPE:
		if  (ientry >= icnt_r_dbv)  return NULL;
		return inam_r_dbv[ientry];
	case ES_TYPE:
		if  (ientry >= icnt_s_dbv)  return NULL;
		return inam_s_dbv[ientry];
	case EC_TYPE:
		if  (ientry >= icnt_c_dbv)  return NULL;
		return inam_c_dbv[ientry];
	case ET_TYPE:
		if  (ientry >= icnt_t_dbv)  return NULL;
		return inam_t_dbv[ientry];
	case EP_TYPE:
		if  (ientry >= icnt_p_dbv)  return NULL;
		return inam_p_dbv[ientry];
	case EF_TYPE:
		if  (ientry >= icnt_f_dbv)  return NULL;
		return inam_f_dbv[ientry];
	case EX_TYPE:
		if  (ientry >= icnt_x_dbv)  return NULL;
		return inam_x_dbv[ientry];
	default:
		return NULL;
	} /*endswitch*/

} /* end of db_entryname */



/*------------------------------------------------------------------------*/



void db_rename( char oldname[], char newname[], STATUS *status )

/* renames info entry
 *
 * parameters of routine
 * char      oldname[];     input; old info name
 * char      newname[];     input; new info name
 * STATUS    *status;       output; return status
 */
{
	/* local variables */
	int      i;            /* counter */

	/* executable code */

	if  (strlen(newname) > MAXNAMLTH)  {
		*status = SHE_INFNAM;
		return;
	} /*endif*/

	for  (i=0;i<icnt_l_dbv;i++)
		if  (strcmp(oldname,inam_l_dbv[i]) == 0)  {
			strcpy( inam_l_dbv[i], newname );
			return;
		} /*endif*/

	for  (i=0;i<icnt_i_dbv;i++)
		if  (strcmp(oldname,inam_i_dbv[i]) == 0)  {
			strcpy( inam_i_dbv[i], newname );
			return;
		} /*endif*/

	for  (i=0;i<icnt_b_dbv;i++)
		if  (strcmp(oldname,inam_b_dbv[i]) == 0)  {
			strcpy( inam_b_dbv[i], newname );
			return;
		} /*endif*/

	for  (i=0;i<icnt_r_dbv;i++)
		if  (strcmp(oldname,inam_r_dbv[i]) == 0)  {
			strcpy( inam_r_dbv[i], newname );
			return;
		} /*endif*/

	for  (i=0;i<icnt_s_dbv;i++)
		if  (strcmp(oldname,inam_s_dbv[i]) == 0)  {
			strcpy( inam_s_dbv[i], newname );
			return;
		} /*endif*/

	for  (i=0;i<icnt_c_dbv;i++)
		if  (strcmp(oldname,inam_c_dbv[i]) == 0)  {
			strcpy( inam_c_dbv[i], newname );
			return;
		} /*endif*/

	for  (i=0;i<icnt_t_dbv;i++)
		if  (strcmp(oldname,inam_t_dbv[i]) == 0)  {
			strcpy( inam_t_dbv[i], newname );
			return;
		} /*endif*/

	for  (i=0;i<icnt_p_dbv;i++)
		if  (strcmp(oldname,inam_p_dbv[i]) == 0)  {
			strcpy( inam_p_dbv[i], newname );
			return;
		} /*endif*/

	for  (i=0;i<icnt_f_dbv;i++)
		if  (strcmp(oldname,inam_f_dbv[i]) == 0)  {
			strcpy( inam_f_dbv[i], newname );
			return;
		} /*endif*/

	for  (i=0;i<icnt_x_dbv;i++)
		if  (strcmp(oldname,inam_x_dbv[i]) == 0)  {
			strcpy( inam_x_dbv[i], newname );
			return;
		} /*endif*/

	*status = SHE_UDNAME;

} /* end of db_rename */




/*------------------------------------------------------------------------*/



void db_define( char name[], unsigned sh_entry, STATUS *status )

/* defines new info entry "sh_entry"
 *
 * parameters of routine
 * char     name[];    input; name of new entry
 * unsigned sh_entry;  input; type & number of entry
 * STATUS   *status;   output; return status
 */
{
	/* local variables */
	unsigned loc_entry;  /* local entry */
	int      loc_stat;   /* local status */

	/* executable code */

	loc_stat = SHE_NOERROR;
	db_ident( name, &loc_entry, &loc_stat );
	if  (loc_stat != SHE_UDNAME)  {
		*status = SHE_ENTTWICE;
		return;
	} /*endif*/

	switch  (sh_entry & E_TYPMASK)  {
	case EL_TYPE:
		if  (icnt_l_dbv == MAXLNAMES)  {
			*status = SHE_ENTOVFL;
			return;
		} else if  (strlen(name) > MAXNAMLTH)  {
			*status = SHE_INFNAM;
			return;
		} /*endif*/
		strcpy( inam_l_dbv[icnt_l_dbv], name );
		ient_l_dbv[icnt_l_dbv++] = sh_entry;
		break;
	case EI_TYPE:
		if  (icnt_i_dbv == MAXINAMES)  {
			*status = SHE_ENTOVFL;
			return;
		} else if  (strlen(name) > MAXNAMLTH)  {
			*status = SHE_INFNAM;
			return;
		} /*endif*/
		strcpy( inam_i_dbv[icnt_i_dbv], name );
		ient_i_dbv[icnt_i_dbv++] = sh_entry;
		break;
	case EB_TYPE:
		if  (icnt_b_dbv == MAXBNAMES)  {
			*status = SHE_ENTOVFL;
			return;
		} else if  (strlen(name) > MAXNAMLTH)  {
			*status = SHE_INFNAM;
			return;
		} /*endif*/
		strcpy( inam_b_dbv[icnt_b_dbv], name );
		ient_b_dbv[icnt_b_dbv++] = sh_entry;
		break;
	case ER_TYPE:
		if  (icnt_r_dbv == MAXRNAMES)  {
			*status = SHE_ENTOVFL;
			return;
		} else if  (strlen(name) > MAXNAMLTH)  {
			*status = SHE_INFNAM;
			return;
		} /*endif*/
		strcpy( inam_r_dbv[icnt_r_dbv], name );
		ient_r_dbv[icnt_r_dbv++] = sh_entry;
		break;
	case ES_TYPE:
		if  (icnt_s_dbv == MAXSNAMES)  {
			*status = SHE_ENTOVFL;
			return;
		} else if  (strlen(name) > MAXNAMLTH)  {
			*status = SHE_INFNAM;
			return;
		} /*endif*/
		strcpy( inam_s_dbv[icnt_s_dbv], name );
		ient_s_dbv[icnt_s_dbv++] = sh_entry;
		break;
	case EC_TYPE:
		if  (icnt_c_dbv == MAXCNAMES)  {
			*status = SHE_ENTOVFL;
			return;
		} else if  (strlen(name) > MAXNAMLTH)  {
			*status = SHE_INFNAM;
			return;
		} /*endif*/
		strcpy( inam_c_dbv[icnt_c_dbv], name );
		ient_c_dbv[icnt_c_dbv++] = sh_entry;
		break;
	case ET_TYPE:
		if  (icnt_t_dbv == MAXTNAMES)  {
			*status = SHE_ENTOVFL;
			return;
		} else if  (strlen(name) > MAXNAMLTH)  {
			*status = SHE_INFNAM;
			return;
		} /*endif*/
		strcpy( inam_t_dbv[icnt_t_dbv], name );
		ient_t_dbv[icnt_t_dbv++] = sh_entry;
		break;
	case EP_TYPE:
		if  (icnt_p_dbv == MAXPNAMES)  {
			*status = SHE_ENTOVFL;
			return;
		} else if  (strlen(name) > MAXNAMLTH)  {
			*status = SHE_INFNAM;
			return;
		} /*endif*/
		strcpy( inam_p_dbv[icnt_p_dbv], name );
		ient_p_dbv[icnt_p_dbv++] = sh_entry;
		break;
	case EF_TYPE:
		if  (icnt_f_dbv == MAXFNAMES)  {
			*status = SHE_ENTOVFL;
			return;
		} else if  (strlen(name) > MAXNAMLTH)  {
			*status = SHE_INFNAM;
			return;
		} /*endif*/
		strcpy( inam_f_dbv[icnt_f_dbv], name );
		ient_f_dbv[icnt_f_dbv++] = sh_entry;
		break;
	default:
		*status = SHE_UKTYPE;
		return;
	} /*endswitch*/

} /* end of db_define */



/*------------------------------------------------------------------------*/



void db_listentries( FILE *f, STATUS *status )

/* lists all defined info entries in stream "f"
 *
 * parameter of routine
 * FILE      *f;      input; output file pointer
 * STATUS    *status; output; return status
 */
{
	/* local variables */
	int      i;      /* counter */
	int      shidx;  /* sh index */
	int      qfidx;  /* qf index */
	int      shflgs; /* sh flags */

	/* executable code */

	fprintf( f, "\n LONG   maxauto: %d,  maxmem: %d,  maxfile: %d\n",
		ENF_LONG, EMAX_LONG, QFC_LENTRY );
	fprintf( f, "        info    storage   sh    qf\n" );
	fprintf( f, "        name       type  idx   num\n" );
	fprintf( f, "     -----------------------------\n" );
	for  (i=0;i<icnt_l_dbv;i++)  {
		shidx = ient_l_dbv[i];
		shflgs = shidx;
		qfidx = qi_cnvlidx( shidx, status );
		if  (*status != SHE_NOERROR)  return;
		shidx &= E_IDXMASK;
		if  (shidx < ENF_LONG)  {
			fprintf( f, " %11s  auto-load  %3d   %3d", inam_l_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} else if  (shidx < EMAX_LONG)  {
			fprintf( f, " %11s   mem-only  %3d     -",
				inam_l_dbv[i], shidx );
		} else {
			fprintf( f, " %11s  file-only  %3d   %3d", inam_l_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} /*endif*/
		if  (shflgs & E_BNOCOPY)
			fprintf( f, "   nocopy" );
		if  (shflgs & E_RDONLY)
			fprintf( f, "   readonly" );
		fprintf( f, "\n" );
	} /*endfor*/

	fprintf( f, "\n\n INTEGER    maxauto: %d,  maxmem: %d,  maxfile: %d\n",
		ENF_INT, EMAX_INT, QFC_IENTRY );
	fprintf( f, "        info    storage   sh    qf\n" );
	fprintf( f, "        name       type  idx   num\n" );
	fprintf( f, "     -----------------------------\n" );
	for  (i=0;i<icnt_i_dbv;i++)  {
		shidx = ient_i_dbv[i];
		shflgs = shidx;
		qfidx = qi_cnviidx( shidx, status );
		if  (*status != SHE_NOERROR)  return;
		shidx &= E_IDXMASK;
		if  (shidx < ENF_INT)  {
			fprintf( f, " %11s  auto-load  %3d   %3d", inam_i_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} else if  (shidx < EMAX_INT)  {
			fprintf( f, " %11s   mem-only  %3d     -",
				inam_i_dbv[i], shidx );
		} else {
			fprintf( f, " %11s  file-only  %3d   %3d", inam_i_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} /*endif*/
		if  (shflgs & E_BNOCOPY)
			fprintf( f, "   nocopy" );
		if  (shflgs & E_RDONLY)
			fprintf( f, "   readonly" );
		fprintf( f, "\n" );
	} /*endfor*/

	fprintf( f, "\n\n BYTE (q-type INT)  maxauto: %d,  maxmem: %d,  maxfile: %d\n",
		ENF_BYTE, EMAX_BYTE, QFC_IENTRY );
	fprintf( f, "        info    storage   sh    qf\n" );
	fprintf( f, "        name       type  idx   num\n" );
	fprintf( f, "     -----------------------------\n" );
	for  (i=0;i<icnt_b_dbv;i++)  {
		shidx = ient_b_dbv[i];
		shflgs = shidx;
		qfidx = qi_cnvbidx( shidx, status );
		if  (*status != SHE_NOERROR)  return;
		shidx &= E_IDXMASK;
		if  (shidx < ENF_BYTE)  {
			fprintf( f, " %11s  auto-load  %3d   %3d", inam_b_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} else if  (shidx < EMAX_BYTE)  {
			fprintf( f, " %11s   mem-only  %3d     -",
				inam_b_dbv[i], shidx );
		} else {
			fprintf( f, " %11s  file-only  %3d   %3d", inam_b_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} /*endif*/
		if  (shflgs & E_BNOCOPY)
			fprintf( f, "   nocopy" );
		if  (shflgs & E_RDONLY)
			fprintf( f, "   readonly" );
		fprintf( f, "\n" );
	} /*endfor*/

	fprintf( f, "\n\n REAL   maxauto: %d,  maxmem: %d,  maxfile: %d\n",
		ENF_REAL, EMAX_REAL, QFC_RENTRY );
	fprintf( f, "        info    storage   sh    qf\n" );
	fprintf( f, "        name       type  idx   num\n" );
	fprintf( f, "     -----------------------------\n" );
	for  (i=0;i<icnt_r_dbv;i++)  {
		shidx = ient_r_dbv[i];
		shflgs = shidx;
		qfidx = qi_cnvridx( shidx, status );
		if  (*status != SHE_NOERROR)  return;
		shidx &= E_IDXMASK;
		if  (shidx < ENF_REAL)  {
			fprintf( f, " %11s  auto-load  %3d   %3d", inam_r_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} else if  (shidx < EMAX_REAL)  {
			fprintf( f, " %11s   mem-only  %3d     -",
				inam_r_dbv[i], shidx );
		} else {
			fprintf( f, " %11s  file-only  %3d   %3d", inam_r_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} /*endif*/
		if  (shflgs & E_BNOCOPY)
			fprintf( f, "   nocopy" );
		if  (shflgs & E_RDONLY)
			fprintf( f, "   readonly" );
		fprintf( f, "\n" );
	} /*endfor*/

	fprintf( f, "\n\n STRING    maxauto: %d,  maxmem: %d,  maxfile: %d\n",
		ENF_STR, EMAX_STR, QFC_SENTRY );
	fprintf( f, "        info    storage   sh    qf\n" );
	fprintf( f, "        name       type  idx   num\n" );
	fprintf( f, "     -----------------------------\n" );
	for  (i=0;i<icnt_s_dbv;i++)  {
		shidx = ient_s_dbv[i];
		shflgs = shidx;
		qfidx = qi_cnvsidx( shidx, status );
		if  (*status != SHE_NOERROR)  return;
		shidx &= E_IDXMASK;
		if  (shidx < ENF_STR)  {
			fprintf( f, " %11s  auto-load  %3d   %3d", inam_s_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} else if  (shidx < EMAX_STR)  {
			fprintf( f, " %11s   mem-only  %3d     -",
				inam_s_dbv[i], shidx );
		} else {
			fprintf( f, " %11s  file-only  %3d   %3d", inam_s_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} /*endif*/
		if  (shflgs & E_BNOCOPY)
			fprintf( f, "   nocopy" );
		if  (shflgs & E_RDONLY)
			fprintf( f, "   readonly" );
		fprintf( f, "\n" );
	} /*endfor*/

	fprintf( f, "\n\n CHAR   maxauto: %d,  maxmem: %d,  maxfile: %d\n",
		ENF_CHAR, EMAX_CHAR, QFC_CENTRY );
	fprintf( f, "        info    storage   sh    qf\n" );
	fprintf( f, "        name       type  idx   num\n" );
	fprintf( f, "     -----------------------------\n" );
	for  (i=0;i<icnt_c_dbv;i++)  {
		shidx = ient_c_dbv[i];
		shflgs = shidx;
		qfidx = qi_cnvcidx( shidx, status );
		if  (*status != SHE_NOERROR)  return;
		shidx &= E_IDXMASK;
		if  (shidx < ENF_CHAR)  {
			fprintf( f, " %11s  auto-load  %3d   %3d", inam_c_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} else if  (shidx < EMAX_CHAR)  {
			fprintf( f, " %11s   mem-only  %3d     -",
				inam_c_dbv[i], shidx );
		} else {
			fprintf( f, " %11s  file-only  %3d   %3d", inam_c_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} /*endif*/
		if  (shflgs & E_BNOCOPY)
			fprintf( f, "   nocopy" );
		if  (shflgs & E_RDONLY)
			fprintf( f, "   readonly" );
		fprintf( f, "\n" );
	} /*endfor*/

	fprintf( f, "\n\n TIME (q-type STR)  maxauto: %d,  maxmem: %d,  maxfile: %d\n",
		ENF_TIME, EMAX_TIME, QFC_SENTRY );
	fprintf( f, "        info    storage   sh    qf\n" );
	fprintf( f, "        name       type  idx   num\n" );
	fprintf( f, "     -----------------------------\n" );
	for  (i=0;i<icnt_t_dbv;i++)  {
		shidx = ient_t_dbv[i];
		shflgs = shidx;
		qfidx = qi_cnvtidx( shidx, status );
		if  (*status != SHE_NOERROR)  return;
		shidx &= E_IDXMASK;
		if  (shidx < ENF_TIME)  {
			fprintf( f, " %11s  auto-load  %3d   %3d", inam_t_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} else if  (shidx < EMAX_TIME)  {
			fprintf( f, " %11s   mem-only  %3d     -",
				inam_t_dbv[i], shidx );
		} else {
			fprintf( f, " %11s  file-only  %3d   %3d", inam_t_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} /*endif*/
		if  (shflgs & E_BNOCOPY)
			fprintf( f, "   nocopy" );
		if  (shflgs & E_RDONLY)
			fprintf( f, "   readonly" );
		fprintf( f, "\n" );
	} /*endfor*/

	fprintf( f, "\n\n FLAG (q-type CHAR)   maxauto: %d,  maxmem: %d,  maxfile: %d\n",
		ENF_FLAG, EMAX_FLAG, QFC_CENTRY );
	fprintf( f, "        info    storage   sh    qf\n" );
	fprintf( f, "        name       type  idx   num\n" );
	fprintf( f, "     -----------------------------\n" );
	for  (i=0;i<icnt_f_dbv;i++)  {
		shidx = ient_f_dbv[i];
		shflgs = shidx;
		qfidx = qi_cnvfidx( shidx, status );
		if  (*status != SHE_NOERROR)  return;
		shidx &= E_IDXMASK;
		if  (shidx < ENF_FLAG)  {
			fprintf( f, " %11s  auto-load  %3d   %3d", inam_f_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} else if  (shidx < EMAX_FLAG)  {
			fprintf( f, " %11s   mem-only  %3d     -",
				inam_f_dbv[i], shidx );
		} else {
			fprintf( f, " %11s  file-only  %3d   %3d", inam_f_dbv[i],
				shidx, qfidx & E_IDXMASK );
		} /*endif*/
		if  (shflgs & E_BNOCOPY)
			fprintf( f, "   nocopy" );
		if  (shflgs & E_RDONLY)
			fprintf( f, "   readonly" );
		fprintf( f, "\n" );
	} /*endfor*/

} /* end of db_listentries



/*------------------------------------------------------------------------*/



unsigned int db_nextentry( unsigned int type )

/* returns next entry of same type, if "type" equals E_NEXTENTRY.
 * If "type" is a type code (e.g. EL_TYPE) then the counter is initialised
 *
 * parameter of routine
 * unsigned int type;    input; type code or E_NEXTENTRY
 *                       returns next entry number of same type
 */
{

	/* local variables */
	static int          idxcnt;   /* entry loop counter */
	static unsigned int currtype; /* current type */

	/* executable code */

	if  (type != E_NEXTENTRY)  {
		idxcnt = 0;
		currtype = type;
		return 0;
	} /*endif*/

	switch  (currtype)  {
	case EL_TYPE:
		if  (idxcnt == icnt_l_dbv)  return E_EOLIST;
		return  ient_l_dbv[idxcnt++];
	case EI_TYPE:
		if  (idxcnt == icnt_i_dbv)  return E_EOLIST;
		return  ient_i_dbv[idxcnt++];
	case EB_TYPE:
		if  (idxcnt == icnt_b_dbv)  return E_EOLIST;
		return  ient_b_dbv[idxcnt++];
	case ER_TYPE:
		if  (idxcnt == icnt_r_dbv)  return E_EOLIST;
		return  ient_r_dbv[idxcnt++];
	case ES_TYPE:
		if  (idxcnt == icnt_s_dbv)  return E_EOLIST;
		return  ient_s_dbv[idxcnt++];
	case EC_TYPE:
		if  (idxcnt == icnt_c_dbv)  return E_EOLIST;
		return  ient_c_dbv[idxcnt++];
	case ET_TYPE:
		if  (idxcnt == icnt_t_dbv)  return E_EOLIST;
		return  ient_t_dbv[idxcnt++];
	case EP_TYPE:
		if  (idxcnt == icnt_p_dbv)  return E_EOLIST;
		return  ient_p_dbv[idxcnt++];
	case EF_TYPE:
		if  (idxcnt == icnt_f_dbv)  return E_EOLIST;
		return  ient_f_dbv[idxcnt++];
	default:
		return E_EOLIST;
	} /*endswitch*/

} /* end of db_nextentry */



/*------------------------------------------------------------------------*/

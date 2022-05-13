
/* file phaseinf.c
 *      ==========
 *
 * version 16, 22-May-2006
 *
 * Manages access to phase information.  Phase lists are stored in
 * TPiPhaseList structures which is a dynamically allocated
 * array of TPiPhase's.  A linked list is not used, because
 * it cannot be deallocated with a single call to sy_deallocmem
 * as it is performed by the SH delete command.  Execution speed
 * is not critical, because inserting and deleting is done only
 * after interaction with a dialog box by the user.
 *
 * v12, 8-Aug-97: added phase sorting routine
 *
 * K. Stammler, 14-Mar-93
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



#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "tcusrdef.h"
#include "phaseinf.h"
#include "globalparams.h"



/* prototypes of local routines */
static void PiTrcEnlist( TPiPhaseRoot *pr, STATUS *status );
static void PiTrcDelist( TPiPhaseRoot *pr );
static TPiPhaseRoot *PiTrcFindRoot( TPiTrcDescr *dscr );
static TPiPhaseList *PiInsertPhase( TPiPhaseList *list,
	TPiPhase *info, STATUS *status );



/* global variables */
static TPiPhaseRoot *piv_phaseroot=NULL;    /* root pointer to phase lists */



/*--------------------------------------------------------------------*/



static TPiPhaseList *PiInsertPhase( TPiPhaseList *list,
	TPiPhase *info, STATUS *status )

/* inserts a new phase to the phase list "list".  Returns pointer
 * to new phase list including old phases and inserted phase.
 * If a phase of the same name is already listed AND the "source"
 * entries are identical, then an error status is returned.
 *
 * parameters of routine
 * TPiPhaseList   *list;     input; old phase list
 * TPiPhase       *inf;      input; new phase to be added
 * STATUS         *status;   output; return status
 */
{
	/* local variables */
	TPiPhaseList  *newlist;    /* pointer to new list */
	int           alloc_no;    /* number of phases to allocate */
	int           i;           /* counter */

	/* executable code */

	if  (info->name[0] <= ' ')  {
		*status = ePiEmptyName;
		return list;
	} /*endif*/

	/* check whether phase is already listed */
	if  (list != NULL)  {
		for  (i=0; i<(list->n_used); i++)  {
			if  (strcmp(info->name,list->inf[i].name) == 0 &&
				info->source == list->inf[i].source)  {
				*status = ePiTwiceDef;
				return list;
			} /*endif*/
		} /*endfor*/
	} /*endif*/

	/* check space for new phase */
	if  (list == NULL  ||  list->n_alloc == list->n_used)  {
		/* need new allocation */
		alloc_no = cPiAllocLth;
		if  (list != NULL)
			alloc_no += list->n_alloc;
		newlist = sy_allocmem( 1, 2*(int)sizeof(int) /* header */ +
			alloc_no*(int)sizeof(TPiPhase) /* phases */, status );
		if  (Severe(status))  return list;  /* return old list */
		/* copy old info */
		newlist->n_alloc = alloc_no;
		if  (list != NULL)  {
			newlist->n_used = list->n_used;
			for  (i=0; i<(newlist->n_used); i++)
				newlist->inf[i] = list->inf[i];
		} else {
			newlist->n_used = 0;
		} /*endif*/
		/* free old list */
		sy_deallocmem( list );
	} else {
		/* can use old list */
		newlist = list;
	} /*endif*/

	/* append info */
	newlist->inf[(newlist->n_used)++] = *info;

	return newlist;

} /* end of PiInsertPhase */



/*--------------------------------------------------------------------*/



void PiDeletePhase( TPiPhaseList *list, TPiPhase *info,
	STATUS *status )

/* changes information of an existing phase (name and source entry
 * must match)
 *
 * parameters of routine
 * TPiPhaseList  *list;       modify; current phase list
 * TPiPhase      *info;       input; phase to be deleted
 * STATUS        *status;     output; return status
 */
{
	/* local variables */
	int      i, j;        /* counters */

	/* executable code */

	if  (list == NULL)  {
		*status = ePiNotFound;
		return;
	} else if  (list->n_used == 0)  {
		*status = ePiNotFound;
		return;
	} /*endif*/

	i = 0;
	while  (strcmp(info->name,list->inf[i].name) != 0 ||
		info->source != list->inf[i].source)  {
		if  (++i >= list->n_used)  {
			*status = ePiNotFound;
			return;
		} /*endif*/
	} /*endwhile*/

	for  (j=i+1; j<(list->n_used); j++)
		list->inf[j-1] = list->inf[j];
	(list->n_used)--;

} /* end of PiDeletePhase */



/*--------------------------------------------------------------------*/



int PiListLength( TPiPhaseList *list )

/* returns length of phase list "list"
 *
 * parameters of routine
 * TPiPhaseList  *list;    input; phase list
 *                         returns length of list
 */
{
	/* executable code */

	if  (list == NULL)  return 0;
	return (list->n_used);

} /* end of PiListLength */



/*--------------------------------------------------------------------*/



TPiPhase *PiGetPhase( TPiPhaseList *list, int number, TSyStatus *status )

/* returns phase number "number"
 *
 * parameters of routine
 * TPiPhaseList  *list;     input; phase list
 * int           number;    input; phase number
 * TSyStatus     *status;   output; return status or NULL
 *                          returns pointer to static phase info
 */
{
	/* executable code */

	if  (list == NULL)  {
		if  (status != NULL)  *status = ePiIllNumber;
		return NULL;
	} /*endif*/

	if  (number < 0 || number >= list->n_used)  {
		if  (status != NULL)  *status = ePiIllNumber;
		return NULL;
	} /*endif*/

	return &(list->inf[number]);

} /* end of PiGetPhase */



/*--------------------------------------------------------------------*/



TPiPhase *PiFindPhase( TPiPhaseList *list, char name[] )

/* finds a phase of a given name and not theoretically determined
 * and returns correspondend phase info block or NULL if not
 * found
 *
 * parameters of routine
 * TPiPhaseList  *list;       input; phase list
 * char          name[];      input; name of phase to be found
 */
{
	/* local variables */
	int      p;        /* phase counter */

	/* executable code */

	if  (list == NULL)  return NULL;
	for  (p=0; p<(list->n_used); p++)
		if  (strcmp(list->inf[p].name,name) == 0 &&
			list->inf[p].source != cPiSourceTheo)
			return &(list->inf[p]);
	return NULL;

} /* end of PiFindPhase */



/*--------------------------------------------------------------------*/



TPiPhase *PiNextPhase( TPiPhaseList *list, TPiPhase *last,
	STATUS *status )

/* Returns phases in chronological order.  If 'last==NULL', the
 * chronological first phase is returned.  Return NULL if no more
 * phase available.
 *
 * parameters of routine
 * TPiPhaseList  *list;       input; phase list to be processed
 * TPiPhase      *last;       input; last phase or NULL
 * STATUS        *status;     output; return status
 *                            returns next phase
 */
{
	/* local variables */
	TIME     mintime;     /* minimum time */
	TIME     ctime;       /* time of current phase */
	TIME     rettime;     /* time of return phase */
	TPiPhase *retphase;  /* phase to return */
	int      p;           /* phase counter */
	float    tdiff;       /* time difference */
	BOOLEAN  last_found;  /* last phase found */

	/* executable code */

	/* check for existing phases */
	if  (list == NULL)  return NULL;
	if  (list->n_used == 0)  return NULL;

	/* get minimum time */
	if  (last != NULL)  {
		tc_t2a( last->onset, &mintime, status );
		if  (Severe(status))  return NULL;
	} /*endif*/

	retphase = NULL;
	last_found = FALSE;
	for  (p=0; p<(list->n_used); p++)  {
		/* check for 'last' phase, this should not be returned */
		if  (list->inf+p == last)  {
			last_found = TRUE;
			continue;
		} /*endif*/
		/* get current time */
		if  (list->inf[p].onset[0] == '\0')  continue;
		tc_t2a( list->inf[p].onset, &ctime, status );
		if  (Severe(status))  return NULL;
		/* check if current time is later than minimum time */
		if  (last != NULL)  {
			tdiff = tc_adiff( &ctime, &mintime );
			if  (tdiff < 0.0)  continue;
			if  (tdiff == 0.0 && !last_found)  continue;
		} /*endif*/
		/* check if current time is earlier than previously found phase */
		if  (retphase != NULL)  {
			tdiff = tc_adiff( &rettime, &ctime );
			if  (tdiff <= 0.0)  continue;
		} /*endif*/
		retphase = list->inf+p;
		rettime = ctime;
	} /*endfor*/

	return retphase;

} /* end of PiNextPhase */



/*--------------------------------------------------------------------*/



TPiPhase *PiNotimePhase( TPiPhaseList *list )

/* returns first phase without onset time or NULL
 *
 * parameters of routine
 * TPiPhaseList  *list;     input; phase list to check
 *                          returns pointer to timeless phase or NULL
 */
{
	/* local variables */
	int      p;           /* phase counter */

	/* executable code */

	if  (list == NULL)  return NULL;
	for  (p=0; p<(list->n_used); p++)
		if  (list->inf[p].onset[0] == '\0')
			return (list->inf+p);
	return NULL;

} /* end of PiNotimePhase */



/*--------------------------------------------------------------------*/



static TPiPhaseRoot *PiTrcFindRoot( TPiTrcDescr *dscr )

/* Returns pointer to phase root of trace or NULL
 *
 * parameters of routine
 * TPiTrcDescr   *dscr;        input; trace description
 */
{
	/* local variables */
	TPiPhaseRoot  *curr;     /* pointer to current phase root */

	/* executable code */

	curr = piv_phaseroot;
	while  (curr != NULL)  {
		if  (strcmp(curr->trc.stream,dscr->stream) == 0)  return curr;
		curr = curr->next;
	} /*endwhile*/

	return NULL;

} /* end of PiTrcFindRoot */



/*--------------------------------------------------------------------*/



static void PiTrcEnlist( TPiPhaseRoot *pr, STATUS *status )

/* Enlists trace descriptor.  'pr' must point to statically allocated
 * memory.
 *
 * parameters of routine
 * PIT_TRCDSCR   *pr;         input; new phase root pointer
 * STATUS        *status;     output; return status
 */
{
	/* local variables */
	TPiPhaseRoot  *curr;    /* current pointer */
	BOOLEAN       insert;   /* make insertion */

	/* executable code */

	if  (piv_phaseroot == NULL)  {
		/* this is the first and only entry */
		piv_phaseroot = pr;
		pr->next = pr->prev = NULL;
	} else if  (strcmp(piv_phaseroot->trc.stream,pr->trc.stream) > 0)  {
		/* this is the new first entry */
		pr->prev = NULL;
		pr->next = piv_phaseroot;
		piv_phaseroot->prev = pr;
		piv_phaseroot = pr;
	} else {
		curr = piv_phaseroot;
		do  {
			insert = (curr->next == NULL);
			if  (!insert)
				insert = (strcmp(pr->trc.stream,curr->next->trc.stream) < 0);
			if  (!insert)
				curr = curr->next;
		}  while (!insert);
		/* insert after 'curr' */
		pr->next = curr->next;
		pr->prev = curr;
		curr->next = pr;
		if  (pr->next != NULL)  pr->next->prev = pr;
	} /*endif*/

} /* end of PiTrcEnlist */



/*----------------------------------------------------------------------------*/



static void PiTrcDelist( TPiPhaseRoot *pr )

/* Removes phaseroot pointer from phaseroot list and frees memory.
 *
 * parameters of routine
 * TPiPhaseRoot  *pr;        input; pointer to phaseroot (will be freed)
 */
{
	/* executable code */

	if  (pr->next != NULL)  pr->next->prev = pr->prev;
	if  (pr->prev == NULL)  {
		piv_phaseroot = pr->next;
	} else {
		pr->prev->next = pr->next;
	} /*endif*/
	sy_deallocmem( pr );

} /* end of PiTrcDelist */



/*----------------------------------------------------------------------------*/



void PiTrcInsertPhase( TPiTrcDescr *dscr, TPiPhase *phase, STATUS *status )

/* Inserts new phase into phase list of given trace.  If the trace is not
 * found in the phaseroot pointers a new entry is created.
 *
 * parameters of routine
 * TPiTrcDescr   *dscr;          input; trace descriptor
 * TPiPhase      *phase;         input; new phase to insert
 * STATUS        *status;        output; return status
 */
{
	/* local variables */
	TPiPhaseRoot  *phaseroot;    /* phase root found */

	/* executable code */

	phaseroot = PiTrcFindRoot( dscr );

	if  (phaseroot != NULL)  {
		/* insert phase and return */
		phaseroot->plist = PiInsertPhase( phaseroot->plist, phase, status );
#ifdef XXX
		if  (SySevere(status))  {
			if  (GpGetInt(cGpI_debug_level) > 0)
				printf( "SHM-dbg1: PiTrcInsertPhase failed\n" );
			*status = cBcNoError;
		} /*endif*/
#endif
		return;
	} /*endif*/

	/* create new phase root */
	phaseroot =
		(TPiPhaseRoot *)sy_allocmem( 1, (int)sizeof(TPiPhaseRoot), status );
	if  (SySevere(status))  return;
	phaseroot->trc = *dscr;
	phaseroot->plist = NULL;
	phaseroot->prev = NULL;
	phaseroot->next = NULL;

	/* insert phase info */
	phaseroot->plist = PiInsertPhase( phaseroot->plist, phase, status );
	if  (SySevere(status))  {sy_deallocmem(phaseroot);return;}

	/* enlist phase root */
	PiTrcEnlist( phaseroot, status );

} /* end of PiTrcInsertPhase */



/*----------------------------------------------------------------------------*/



void PiTrcDeletePhase( TPiTrcDescr *dscr, TPiPhase *phase, STATUS *status )

/* Deletes phase from phase list.  Removes phaseroot entry if empty.
 *
 * parameters of routine
 * TPiTrcDescr   *dscr;         input; trace descriptor
 * TPiPhase      *phase;        input; phase to be deleted
 * STATUS        *status;       output; return status
 */
{
	/* local variables */
	TPiPhaseRoot  *phaseroot;     /* phaseroot of trace */

	/* executable code */

	phaseroot = PiTrcFindRoot( dscr );

	if  (phaseroot == NULL)	 {
		*status = ePiNotFound;
		return;
	} /*endif*/

	PiDeletePhase( phaseroot->plist, phase, status );
	if  (SySevere(status))  return;

	/* don't know whether this can happen */
	if  (phaseroot->plist == NULL)  {
		PiTrcDelist( phaseroot );
		return;
	} /*endif*/

	/* check for empty phase list */
	if  (phaseroot->plist->n_used == 0)  {
		sy_deallocmem( phaseroot->plist );
		phaseroot->plist = NULL;
		PiTrcDelist( phaseroot );
	} /*endif*/

} /* end of PiTrcDeletePhase */



/*----------------------------------------------------------------------------*/



TPiPhase *PiTrcFindPhase( TPiTrcDescr *dscr, char name[] )

/* Tries to find phase 'name' on trace with descriptor 'dscr'.  Returns
 * phase pointer or NULL.
 *
 * parameters of routine
 * TPiTrcDescr   *dscr;       input; trace descriptor
 * char          name[];      input; phase name to find
 */
{
	/* local variables */
	TPiPhaseRoot  *phaseroot;     /* phaseroot of trace */

	/* executable code */

	phaseroot = PiTrcFindRoot( dscr );

	if  (phaseroot == NULL)	 return NULL;
	if  (phaseroot->plist == NULL)  return NULL;
	return PiFindPhase( phaseroot->plist, name );

} /* end of PiTrcFindPhase */



/*----------------------------------------------------------------------------*/



TPiPhaseList *PiTrcPhaseList( TPiTrcDescr *dscr )

/* Returns phase list for given trace
 *
 * parameters of routine
 * TPiTrcDescr   *dscr;           input; trace descriptor
 */
{
	/* local variables */
	TPiPhaseRoot  *phaseroot;     /* phaseroot of trace */

	/* executable code */
	phaseroot = PiTrcFindRoot( dscr );

	if  (phaseroot == NULL)	 return NULL;
	return  phaseroot->plist;

} /* end of PiTrcPhaseList */



/*----------------------------------------------------------------------------*/


#define MAXCNT 30


void PiPhaseDump( void )

/* Dumps out all phases in root lists
 *
 * parameters of routine
 * none
 */
{
	/* local variables */
	TPiPhaseRoot   *root;           /* current phase root */
	TPiPhaseList   *plist;          /* current phase list */
	int            i;               /* counter */
	int            maxcnt;          /* truncated number of phases */

	/* executable code */

	printf( "\nphase dump following:\n" );
	root = piv_phaseroot;
	while  (root != NULL)  {
		printf( "dump: %10s ", root->trc.stream );
		plist = root->plist;
		if  (plist == NULL)  {
			printf( "NULL\n" );
		} else {
			printf( "u%02d a%02d  ", plist->n_used, plist->n_alloc );
		} /*endif*/
		maxcnt = plist->n_used;
		if  (maxcnt > MAXCNT)  {
			printf( "   ridiculous large number of %d phases truncated to %d\n",
				maxcnt, MAXCNT );
			maxcnt = MAXCNT;
		} /*endif*/
		if  (maxcnt > 0)  {
			printf( "%s", plist->inf[0].name );
			for  (i=1; i<maxcnt; i++)
				printf( ",%s", plist->inf[i].name );
		} /*endif*/
		printf( "\n" );
		root = root->next;
	} /*endwhile*/

} /* end of PiPhaseDump */



/*----------------------------------------------------------------------------*/



void PiRenamePhase( char oldname[], char newname[], int *chgno )

/* Renames all phases in memory from 'oldname' to 'newname'.  A possibly
 * already existing 'newname' is deleted.
 *
 * parameters of routine
 * char       oldname[];       input; old name of phase
 * char       newname[];       input; new name of phase
 * int        *chgno;          output; total number of name changes performed
 */
{
	/* local variables */
	TPiPhaseRoot   *root;           /* current phase root */
	TPiPhaseList   *plist;          /* current phase list */
	int            i;               /* counter */
	TPiPhase       tmp;             /* scratch */
	TSyStatus      locstat;         /* local status */

	/* executable code */

	*chgno = 0;
	root = piv_phaseroot;
	while  (root != NULL)  {

		plist = root->plist;
		if  (plist == NULL)  {
			printf( "   *SHM: error in phase lists, NULL phase pointer\n" );
			return;
		} /*endif*/

		/* first check for already existing 'newname's */
		for  (i=0; i<(plist->n_used); i++)
			if  (strcmp(plist->inf[i].name,newname) == 0)  {
				tmp = plist->inf[i];
				locstat = cBcNoError;
				PiDeletePhase( plist, &tmp, &locstat );
			} /*endif*/

		/* now rename */
		for  (i=0; i<(plist->n_used); i++)
			if  (strcmp(plist->inf[i].name,oldname) == 0)  {
				strcpy( plist->inf[i].name, newname );
				(*chgno)++;
			} /*endif*/

		root = root->next;

	} /*endwhile*/

} /* end of PiRenamePhase */



/*----------------------------------------------------------------------------*/



TSyBoolean PiPhaseIsPicked( char name[] )

/* Returns TRUE if at least one phase of the given name exists
 *
 * parameters of routine
 * char       name[];       input; name of phase to look for
 */
{
	/* local variables */
	TPiPhaseRoot   *root;           /* current phase root */
	TPiPhaseList   *plist;          /* current phase list */
	int            i;               /* counter */

	/* executable code */

	root = piv_phaseroot;
	while  (root != NULL)  {

		plist = root->plist;
		if  (plist == NULL)  {
			printf( "   *SHM: error in phase lists, NULL phase pointer\n" );
			return FALSE;
		} /*endif*/

		for  (i=0; i<(plist->n_used); i++)
			if  (strcmp(plist->inf[i].name,name) == 0)
				return TRUE;

		root = root->next;

	} /*endwhile*/

	return FALSE;

} /* end of PiPhaseIsPicked */



/*----------------------------------------------------------------------------*/



TPiPhaseRoot *PiNextPhaseRoot( TPiPhaseRoot *last )

/* Returns next phase root pointer.  If 'last==NULL' it returns first phase
 * root pointer.
 *
 * parameters of routine
 * TPiPhaseRoot  *last;         input; pointer to last root or NULL
 */
{
	/* executable code */

	if  (last == NULL)  {
		return piv_phaseroot;
	} else {
		return last->next;
	} /*endif*/

} /* end of PiNextPhaseRoot */



/*----------------------------------------------------------------------------*/



void PiClearAllPhases( void )

/* Deletes all phases stored
 *
 * parameters of routine
 * none
 */
{
	/* local variables */
	TPiPhaseRoot *proot;             /* pointer to phase root */
	TPiPhaseRoot *freeptr;           /* scratch pointer */

	/* executable code */

	proot = piv_phaseroot;
	piv_phaseroot = NULL;

	/* free memory */
	while  (proot != NULL)  {
		if  (proot->plist != NULL)  sy_deallocmem( proot->plist );
		freeptr = proot;
		proot = proot->next;
		sy_deallocmem( freeptr );
	} /*endwhile*/

} /* end of PiClearAllPhases */



/*----------------------------------------------------------------------------*/



void PiSortPhaseList( TPiPhaseList *plist, TSyStatus *status )

/* Sorts all phases of list by time.
 * This routine was written and then found to be obsolete -> it's not tested !
 *
 * parameters of routine
 * TPiPhaseList   *list;      modify; phase list to be sorted
 * TSyStatus       *status;   output; return status
 */
{
	/* local variables */
	TSyBoolean needs_sorting;     /* phases need further sorting */
	int      i;                   /* counter */
	TPiPhase tmp_phase;           /* temporary storage */
	float    tdiff;               /* time difference */

	/* executable code */

	if  (plist == NULL)  return;
	if  (plist->n_used <= 1)  return;

	do  {
		needs_sorting = FALSE;
		for  (i=0; i<(plist->n_used-1); i++)  {
			tdiff = tc_tdiff( plist->inf[i+1].onset, plist->inf[i].onset, status );
			if  (SySevere(status))  return;
			if  (tdiff < 0.0)  {
				needs_sorting = TRUE;
				tmp_phase = plist->inf[i+1];
				plist->inf[i+1] = plist->inf[i];
				plist->inf[i] = tmp_phase;
			} /*endif*/
		} /*endfor*/
	}  while  (needs_sorting);

} /* end of PiSortPhaseList */



/*----------------------------------------------------------------------------*/



void PiSbGetSlowness( TPiSlowBox *sb, char name[], float *slowness,
	float *azimuth, float *c_slowness, float *c_azimuth, TSyBoolean *found )

/* Returns slowness and azimuth if stored for given phase
 *
 * parameters of routine
 * TPiSlowBox    *sb;       input; slowness box
 * char          name[];    input; name of phase
 * float         *slowness; output; slowness value found (if not NULL)
 * float         *azimuth;  output; back azimuth found (if not NULL)
 * float         *c_slowness; output; corrected slowness (if not NULL)
 * float         *c_azimuth; output; corrected back azimuth (if not NULL)
 * TSyBoolean    *found;    output; phase found inslowness box
 */
{
	/* local variables */
	int      i;           /* counter */

	/* executable code */

	*found = FALSE;
	for  (i=0; i<(sb->lth); i++)
		if  (strcmp(name,sb->phase[i]) == 0)  {
			if  (slowness != NULL)  *slowness = sb->slow[i];
			if  (azimuth != NULL)  *azimuth = sb->azim[i];
			if  (c_slowness != NULL)  *c_slowness = sb->cslow[i];
			if  (c_azimuth != NULL)  *c_azimuth = sb->cazim[i];
			*found = TRUE;
		} /*endif*/

} /* end of PiSbGetSlowness */



/*----------------------------------------------------------------------------*/



void PiSbSetSlowness( TPiSlowBox *sb, char name[], float slowness,
	float azimuth, float c_slowness, float c_azimuth, TSyStatus *status )

/* Sets slowness and azimuth for a given phase.  Overwrites values if
 * already set, adds new entry if not set yet.
 *
 * parameters of routine
 * TPiSlowBox    *sb;       modify; slowness values of phases
 * char          name[];    input; name of phase
 * float         slowness;  input; slowness of phase
 * float         azimuth;   input; back azimuth of phase
 * float         c_slowness; input; corrected slowness
 * float         c_azimuth; input; corrected back azimuth;
 * TSyStatus     *status;   output; return status
 */
{
	/* local variables */
	int      i;         /* counter */

	/* executable code */

	/* find if phase already listed */
	for  (i=0; i<(sb->lth); i++)
		if  (strcmp(name,sb->phase[i]) == 0)
			break;

	/* abort if list is full */
	if  (i >= cPiSlowBoxDim)  {
		*status = ePiSlowBoxOverflow;
		return;
	} /*endif*/

	/* increase list counter if not yet listed */
	if  (i == sb->lth)  (sb->lth)++;

	/* insert/overwrite values */
	strcpy( sb->phase[i], name );
	sb->slow[i] = slowness;
	sb->azim[i] = azimuth;
	sb->cslow[i] = c_slowness;
	sb->cazim[i] = c_azimuth;

} /* end of PiSbSetSlowness */



/*----------------------------------------------------------------------------*/



void PiSbClearAll( TPiSlowBox *sb )

/* Clears all entries in SlowBox
 *
 * parameters of routine
 * TPiSlowBox    output;     SlowBox to be cleared
 */
{
	/* executable code */

	sb->lth = 0;

} /* end of PiSbClearAll */



/*----------------------------------------------------------------------------*/

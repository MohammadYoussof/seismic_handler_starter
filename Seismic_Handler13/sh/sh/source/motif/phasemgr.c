
/* file phasemgr.c
 *      ==========
 *
 * version 3, 22-Feb-2006
 *
 * Phase management of SHM.  Uses routines of phaseinf.c and knows about
 * trace structure (access to trace info's).
 *
 * K. Stammler, 9-Feb-97
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
#include "sysbase.h"
#include "utusrdef.h"
#include "infoidx.h"
#include "phaseinf.h"
#include "phasemgr.h"




/*--------------------------------------------------------------------------*/



void PmGetTrcDescr( TPmTrace *trc, TPiTrcDescr *dscr, TSyStatus *status )

/* Returns trace descriptor for specified trace
 *
 * parameters of routine
 * TPmTrace      *trc;           input; pointer to trace info
 * TPiTrcDescr   *dscr;          output; return trace descriptor
 * TSyStatus     *status;        output; return status
 */
{
	/* local variables */
	char     str[cBcLineLth+1];       /* scratch string */

	/* executable code */

	/* initalize */
	dscr->code[0] = dscr->comp = dscr->stream[0] = '\0';

	db_gets( trc, ES_STATION, cBcLineLth, str, status );
	if  (SySevere(status))  return;
	if  (strlen(str) > cPiStatCodeLth)  {*status = ePmStrOvfl; return;}
	strcpy( dscr->code, str );
	ut_cap( dscr->code );

	dscr->comp = Cap( db_getc( trc, EC_COMP, status ) );
	if  (SySevere(status))  return;

	sprintf( dscr->stream, "%s-%c", dscr->code, dscr->comp );

} /* end of PmGetTrcDescr */



/*--------------------------------------------------------------------------*/



void PmAddPhase( TPmTrace *trc, TPiPhase *phase, TSyStatus *status )

/* Adds phase to trace
 *
 * parameters of routine
 * TPmTrace   *trc;          input; trace pointer
 * TPiPhase   *phase;        input; phase pointer
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	TPiTrcDescr   dscr;       /* trace descriptor */

	/* executable code */

	PmGetTrcDescr( trc, &dscr, status );
	if  (SySevere(status))  return;
	PiTrcInsertPhase( &dscr, phase, status );

} /* end of PmAddPhase */



/*--------------------------------------------------------------------------*/



void PmAddPhaseV2( char station[], char comp, TPiPhase *phase,
	TSyStatus *status )

/* Adds phase to trace.  Takes station & comp instead of trace pointer.
 *
 * parameters of routine
 * char       station[];     input; station name
 * char       comp;          input; component
 * TPiPhase   *phase;        input; phase pointer
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	TPiTrcDescr   dscr;       /* trace descriptor */

	/* executable code */

	/* prepare trace descriptor */
	dscr.code[0] = dscr.comp = dscr.stream[0] = '\0';
	if  (strlen(station) > cPiStatCodeLth)  {*status = ePmStrOvfl; return;}
	strcpy( dscr.code, station );
	ut_cap( dscr.code );
	dscr.comp = Cap( comp );
	sprintf( dscr.stream, "%s-%c", dscr.code, dscr.comp );

	PiTrcInsertPhase( &dscr, phase, status );

} /* end of PmAddPhaseV2 */



/*--------------------------------------------------------------------------*/



void PmRemovePhase( TPmTrace *trc, TPiPhase *phase, TSyStatus *status )

/* removes phase from trace
 *
 * parameters of routine
 * TPmTrace   *trc;          input; trace pointer
 * TPiPhase   *phase;        input; phase pointer
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	TPiTrcDescr   dscr;       /* trace descriptor */

	/* executable code */

	PmGetTrcDescr( trc, &dscr, status );
	if  (SySevere(status))  return;
	PiTrcDeletePhase( &dscr, phase, status );

} /* end of PmRemovePhase */



/*--------------------------------------------------------------------------*/



TPiPhase *PmFindPhase( TPmTrace *trc, char name[] )

/* tries to find phase of name 'name' on trace 'trc'.  Returns phase
 * pointer or NULL.
 *
 * parameters of routine
 * TPmTrace   *trc;             input; trace pointer
 * char       name[];           input; name of phase to find
 */
{
	/* local variables */
	TPiTrcDescr  dscr;       /* trace descriptor */
	TSyStatus    status;     /* return status */

	/* executable code */

	status = cBcNoError;
	PmGetTrcDescr( trc, &dscr, &status );
	if  (SySevere(&status))  return NULL;
	return PiTrcFindPhase( &dscr, name );

} /* end of PmFindPhase */



/*--------------------------------------------------------------------------*/



TPiPhase *PmFindPhaseV2( char station[], char comp, char name[] )

/* Tries to find phase of name 'name' on trace with stream name 'station-comp'.
 * Returns phase pointer or NULL.
 *
 * parameters of routine
 * char       station[];     input; station name
 * char       comp;          input; component
 * char       name[];        input; name of phase to find
 */
{
	/* local variables */
	TPiTrcDescr  dscr;       /* trace descriptor */

	/* executable code */

	/* prepare trace descriptor */
	dscr.code[0] = dscr.comp = dscr.stream[0] = '\0';
	if  (strlen(station) > cPiStatCodeLth)  return NULL;
	strcpy( dscr.code, station );
	ut_cap( dscr.code );
	dscr.comp = Cap( comp );
	sprintf( dscr.stream, "%s-%c", dscr.code, dscr.comp );

	return PiTrcFindPhase( &dscr, name );

} /* end of PmFindPhaseV2 */



/*--------------------------------------------------------------------------*/



TPiPhaseList *PmGetPhaseList( TPmTrace *trc )

/* Returns phaselist of trace 'trc' or NULL.
 *
 * parameters of routine
 * TPmTrace   *trc;             input; trace pointer
 */
{
	/* local variables */
	TPiTrcDescr  dscr;       /* trace descriptor */
	TSyStatus    status;     /* return status */

	/* executable code */

	status = cBcNoError;
	PmGetTrcDescr( trc, &dscr, &status );
	if  (SySevere(&status))  return NULL;
	return PiTrcPhaseList( &dscr );

} /* end of PmGetPhaseList */



/*----------------------------------------------------------------------------*/



TPmTrace *PmFindTrace( TPmTrace *prev, TPiTrcDescr *dscr )

/* Returns next trace pointer matching trace info '*dscr'.  If prev==NULL,
 * returns first matching trace pointer.  If no trace matches NULL is returned.
 *
 * parameters of routine
 * TPmTrace      *prev;       input; previous trace pointer or NULL
 * TPiTrcDescr   *dscr;       input; trace descriptor
 */
{
	/* local variables */
	TPmTrace    *trc;                       /* trace pointer */
	TSyStatus   locstat;                    /* local status */
	char        station[cBcShortStrLth+1];  /* current station */
	char        comp;                       /* current component */

	/* executable code */

	trc = prev;
	FOREVER  {
		locstat = cBcNoError;
		trc = db_getp( trc, EP_DSPN, &locstat );
		if  (trc == NULL)  break;
		db_gets( trc, ES_STATION, cBcShortStrLth, station, &locstat );
		if  (SySevere(&locstat))  continue;
		ut_cap( station );
		if  (strcmp(station,dscr->code) != 0)  continue;
		comp = db_getc( trc, EC_COMP, &locstat );
		comp = Cap( comp );
		if  (SySevere(&locstat))  continue;
		if  (comp != dscr->comp)  continue;
		return trc;
	} /*endfor*/

	return NULL;

} /* end of PmFindTrace */



/*----------------------------------------------------------------------------*/

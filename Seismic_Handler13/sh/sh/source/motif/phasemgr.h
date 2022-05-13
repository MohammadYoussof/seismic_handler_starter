
/* file phasemgr.h
 *      ==========
 *
 * version 3, 22-May-2006
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




/* errors */
#define ePmOffset         7800
#define ePmStrOvfl        (ePmOffset+1)


/* types */

/* pointer to trace info */
#define TPmTrace void



/*--------------------------------------------------------------------------*/


void PmGetTrcDescr( TPmTrace *trc, TPiTrcDescr *dscr, TSyStatus *status );

/* Returns trace descriptor for specified trace
 *
 * parameters of routine
 * TPmTrace      *trc;           input; pointer to trace info
 * TPiTrcDescr   *dscr;          output; return trace descriptor
 * TSyStatus     *status;        output; return status
 */


/*--------------------------------------------------------------------------*/


void PmAddPhase( TPmTrace *trc, TPiPhase *phase, TSyStatus *status );

/* Adds phase to trace
 *
 * parameters of routine
 * TPmTrace   *trc;          input; trace pointer
 * TPiPhase   *phase;        input; phase pointer
 * TSyStatus  *status;       output; return status
 */


/*--------------------------------------------------------------------------*/


void PmAddPhaseV2( char station[], char comp, TPiPhase *phase,
	TSyStatus *status );

/* Adds phase to trace.  Takes station & comp instead of trace pointer.
 *
 * parameters of routine
 * char       station[];     input; station name
 * char       comp;          input; component
 * TPiPhase   *phase;        input; phase pointer
 * TSyStatus  *status;       output; return status
 */


/*--------------------------------------------------------------------------*/


void PmRemovePhase( TPmTrace *trc, TPiPhase *phase, TSyStatus *status );

/* removes phase from trace
 *
 * parameters of routine
 * TPmTrace   *trc;          input; trace pointer
 * TPiPhase   *phase;        input; phase pointer
 * TSyStatus  *status;       output; return status
 */


/*--------------------------------------------------------------------------*/


TPiPhase *PmFindPhase( TPmTrace *trc, char name[] );

/* tries to find phase of name 'name' on trace 'trc'.  Returns phase
 * pointer or NULL.
 *
 * parameters of routine
 * TPmTrace   *trc;             input; trace pointer
 * char       name[];           input; name of phase to find
 */


/*--------------------------------------------------------------------------*/


TPiPhase *PmFindPhaseV2( char station[], char comp, char name[] );

/* Tries to find phase of name 'name' on trace with stream name 'station-comp'.
 * Returns phase pointer or NULL.
 *
 * parameters of routine
 * char       station[];     input; station name
 * char       comp;          input; component
 * char       name[];        input; name of phase to find
 */


/*--------------------------------------------------------------------------*/


TPiPhaseList *PmGetPhaseList( TPmTrace *trc );

/* Returns phaselist of trace 'trc' or NULL.
 *
 * parameters of routine
 * TPmTrace   *trc;             input; trace pointer
 */


/*----------------------------------------------------------------------------*/


TPmTrace *PmFindTrace( TPmTrace *prev, TPiTrcDescr *dscr );

/* Returns next trace pointer matching trace info '*dscr'.  If prev==NULL,
 * returns first matching trace pointer.  If no trace matches NULL is returned.
 *
 * parameters of routine
 * TPmTrace      *prev;       input; previous trace pointer or NULL
 * TPiTrcDescr   *dscr;       input; trace descriptor
 */


/*--------------------------------------------------------------------------*/

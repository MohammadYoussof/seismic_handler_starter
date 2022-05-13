
/* file phaseinf.h
 *      ==========
 *
 * version 27, 22-May-2006
 *
 * header file of module phaseinf.c
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

#ifndef __PHASEINF
#define __PHASEINF

#define cPiMaxPhaseLth 10
	/* maximum length of phase name */
#define cPiCommentLth 132
	/* maximum length of comment string */
#define cPiAllocLth 4
	/* step size in allocating phase structures */

/* selection kinds */
#define cPiSourceUndefined 0
#define cPiSourceManually  1
#define cPiSourceTheo      2
#define cPiSourceAuto      3

#define cPiSpecWeak       'w'
#define cPiSpecEmergent   'e'
#define cPiSpecImpulsive  'i'

#define cPiMagnUndefined   0
#define cPiMagnMb          1
#define cPiMagnMsPlain     2
#define cPiMagnMsCNa       3
#define cPiMagnMsCEu       4
#define cPiMagnMsCO        5
#define cPiMagnMsO         6
#define cPiMagnMl          7
#define cPiMagnMw          8
#define cPiMagnMbb         9
#define cPiMagnMu         10

#define cPiApsrcUndefined  0
#define cPiApsrcDirect     1
#define cPiApsrcBeam       2
#define cPiApsrcAlign      3

#define cPiResidUndefined  0
#define cPiResidPlaneWave  1
#define cPiResidLocsat     2

#define cPiAccEmpty       -1.0
#define cPiResidCorrEmpty -9999.0

#define cPiStatCodeLth 6
#define cPiStreamLth (cPiStatCodeLth+3)

#define cPiSlowBoxDim 8

/* phase flags */
#define fPiAttribMagn   0x0001       /* phase has reference magnitude */
#define fPiAttribLoc    0x0002       /* phase was used for location */
#define fPiAttribBeam   0x0004       /* phase was used for beam */
#define fPiAttribOutput 0x0008       /* location output at this phase */

/* error codes */
#define ePiOffset 4200
#define ePiTwiceDef        (ePiOffset+1)    /* phase already in list */
#define ePiNotFound        (ePiOffset+2)    /* phase not found */
#define ePiIllNumber       (ePiOffset+3)    /* illegal phase number */
#define ePiEmptyName       (ePiOffset+4)    /* empty phase name  */
#define ePiSlowBoxOverflow (ePiOffset+5)    /* too many entries in SlowBox */

/* types */

typedef struct {
	float     bbampl;                    /* BB amplitude */
	float     bbampl_time;               /* time of amplitude reading */
	float     bbperiod;                  /* BB signal period */
	float     mbb;                       /* broadband magnitude */
} TPiBBMagn;

typedef struct {
	char      name[cPiMaxPhaseLth+1];    /* name of phase */
	char      onset[BC_TIMELTH+1];       /* onset time */
	float     onset_acc_l;               /* left accuracy */
	float     onset_acc_r;               /* right accuray */
	char      spec;                      /* onset specification */
	/*void      *phasetrc;  */               /* pointer to selected trace */
	int       sign;                      /* sign, -1, 1, 0 */
	BOOLEAN   use;                       /* use it for location */
	BOOLEAN   reliable;                  /* reliable phase */
	int       source;                    /* kind of determination */
	int       weight;                    /* phase weight */
	int       quality;                   /* phase quality */
	float     ampl;                      /* amplitude */
	float     ampl_time;                 /* time relative to onset */
	float     ampl_veloc;                /* velocity restituted amplitude */
	float     ampl_displ;                /* displacement restituted ampl. */
	float     period;                    /* period */
	float     magnitude;                 /* magnitude, mb on P-phases, MS else */
	int       magn_source;               /* type of magnitude determination */
	int       ap_source;                 /* source of amplitude&period determ. */
	TPiBBMagn bb;                        /* broadband magnitude values */
	float     signoise;                  /* S/N ratio */
	float     resid;                     /* residual time of phase */
	int       resid_type;                /* type of residual */
	float     resid_corr;                /* correction for residuals */
	int       flags;                     /* phase flags */
	char      filter[BC_LINELTH+1];      /* filter of record */
	char      comment[cPiCommentLth+1]; /* comment */
} TPiPhase;

typedef struct {
	int          n_alloc;   /* number of info structures allocated */
	int          n_used;    /* number of info structures used */
	TPiPhase     inf[1];    /* array of phase info's */
	              /* ^-- array bound zero gives warning message */
} TPiPhaseList;

typedef struct {
	char      code[cPiStatCodeLth+1];      /* station code */
	char      comp;                        /* component */
	char      stream[cPiStreamLth+1];      /* stream=<statcode>-<chan>-<comp> */
} TPiTrcDescr;

typedef struct _phase_root {
	TPiTrcDescr         trc;               /* trace descriptor */
	TPiPhaseList        *plist;            /* pointer to phase list */
	struct _phase_root  *next;             /* pointer to next structure */
	struct _phase_root  *prev;             /* pointer to previous structure */
} TPiPhaseRoot;

typedef struct {
	int       lth;                         /* number of entries */
	char      phase[cPiSlowBoxDim][cPiMaxPhaseLth+1];    /* name of phase */
	float     slow[cPiSlowBoxDim];         /* slowness of phase */
	float     azim[cPiSlowBoxDim];         /* back azimuth of phase */
	float     cslow[cPiSlowBoxDim];        /* corrected slowness */
	float     cazim[cPiSlowBoxDim];        /* corrected back azimuth */
} TPiSlowBox;


/* macros */

#define PiPhaseListLength(a) ((a)->n_used)
#define PiPhaseListOfRoot(a) ((a)->plist)
#define PiStationOfRoot(a) ((a)->trc.code)
#define PiCompOfRoot(a) ((a)->trc.comp)
#define PiDescrOfRoot(a) (&((a)->trc))


/* routines */
/* -------- */



/*--------------------------------------------------------------------*/


void PiDeletePhase( TPiPhaseList *list, TPiPhase *info,
	STATUS *status );

/* changes information of an existing phase (name and source entry
 * must match)
 *
 * parameters of routine
 * TPiPhaseList  *list;       modify; current phase list
 * TPiPhase      *info;       input; phase to be deleted
 * STATUS        *status;     output; return status
 */


/*--------------------------------------------------------------------*/


int PiListLength( TPiPhaseList *list );

/* returns length of phase list "list"
 *
 * parameters of routine
 * TPiPhaseList  *list;    input; phase list
 *                         returns length of list
 */


/*--------------------------------------------------------------------*/


TPiPhase *PiGetPhase( TPiPhaseList *list, int number,
	STATUS *status );

/* returns phase number "number"
 *
 * parameters of routine
 * TPiPhaseList  *list;     input; phase list
 * int           number;    input; phase number
 *                          returns pointer to static phase info
 */


/*--------------------------------------------------------------------*/


TPiPhase *PiFindPhase( TPiPhaseList *list, char name[] );

/* finds a phase of a given name and not theoretically determined
 * and returns correspondend phase info block or NULL if not
 * found
 *
 * parameters of routine
 * TPiPhaseList  *list;       input; phase list
 * char          name[];      input; name of phase to be found
 */


/*--------------------------------------------------------------------*/


TPiPhase *PiNextPhase( TPiPhaseList *list, TPiPhase *last,
	STATUS *status );

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


/*--------------------------------------------------------------------*/


TPiPhase *PiNotimePhase( TPiPhaseList *list );

/* returns first phase without onset time or NULL
 *
 * parameters of routine
 * TPiPhaseList  *list;     input; phase list to check
 *                          returns pointer to timeless phase or NULL
 */


/*-------------------------------------------------------------------------*/


void PiTrcInsertPhase( TPiTrcDescr *dscr, TPiPhase *phase,
	STATUS *status );

/* Inserts new phase into phase list of given trace.  If the trace is not
 * found in the phaseroot pointers a new entry is created.
 *
 * parameters of routine
 * TPiTrcDescr   *dscr;          input; trace descriptor
 * TPiPhase      *phase;         input; new phase to insert
 * STATUS        *status;        output; return status
 */


/*--------------------------------------------------------------------------*/


TPiPhase *PiTrcFindPhase( TPiTrcDescr *dscr, char name[] );

/* Tries to find phase 'name' on trace with descriptor 'dscr'.  Returns
 * phase pointer or NULL.
 *
 * parameters of routine
 * TPiTrcDescr  *dscr;       input; trace descriptor
 * char          name[];      input; phase name to find
 */


/*----------------------------------------------------------------------------*/


TPiPhaseList *PiTrcPhaseList( TPiTrcDescr *dscr );

/* Returns phase list for given trace
 *
 * parameters of routine
 * TPiTrcDescr   *dscr;           input; trace descriptor
 */


/*----------------------------------------------------------------------------*/


TPiPhaseRoot *PiNextPhaseRoot( TPiPhaseRoot *last );

/* Returns next phase root pointer.  If 'last==NULL' it returns first phase
 * root pointer.
 *
 * parameters of routine
 * TPiPhaseRoot  *last;         input; pointer to last root or NULL
 */


/*--------------------------------------------------------------------------*/


void PiPhaseDump( void );

/* Dumps out all phases in root lists
 *
 * parameters of routine
 * none
 */


/*----------------------------------------------------------------------------*/


void PiRenamePhase( char oldname[], char newname[], int *chgno );

/* Renames all phases in memory from 'oldname' to 'newname'.  A possibly
 * already existing 'newname' is deleted.
 *
 * parameters of routine
 * char       oldname[];       input; old name of phase
 * char       newname[];       input; new name of phase
 * int        *chgno;          output; total number of name changes performed
 */


/*----------------------------------------------------------------------------*/


TSyBoolean PiPhaseIsPicked( char name[] );

/* Returns TRUE if at least one phase of the given name exists
 *
 * parameters of routine
 * char       name[];       input; name of phase to look for
 */


/*----------------------------------------------------------------------------*/


void PiClearAllPhases( void );

/* Deletes all phases stored
 *
 * parameters of routine
 * none
 */


/*----------------------------------------------------------------------------*/


void PiSortPhaseList( TPiPhaseList *plist, TSyStatus *status );

/* Sorts all phases of list by time.
 *
 * parameters of routine
 * TPiPhaseList   *list;      modify; phase list to be sorted
 * TSyStatus       *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void PiSbGetSlowness( TPiSlowBox *sb, char name[], float *slowness,
	float *azimuth, float *c_slowness, float *c_azimuith, TSyBoolean *found );

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


/*----------------------------------------------------------------------------*/


void PiSbSetSlowness( TPiSlowBox *sb, char name[], float slowness,
	float azimuth, float c_slowness, float c_azimuth, TSyStatus *status );

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


/*----------------------------------------------------------------------------*/


void PiSbClearAll( TPiSlowBox *sb );

/* Clears all entries in SlowBox
 *
 * parameters of routine
 * TPiSlowBox    output;     SlowBox to be cleared
 */


/*--------------------------------------------------------------------*/

#endif /* __PHASEINF */

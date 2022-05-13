
/* file cblib2.c
 *      =======
 *
 * version 42,9-Nov-2006
 *
 * library of utility routines for callbacks
 * K. Stammler, 28-Oct-93
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
#include <math.h>
#include "basecnst.h"
#undef BC_DEFINE_TRUE_FALSE
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/Scale.h>
#ifdef BC_INC_UNISTD
#include BC_INC_UNISTD
#endif
#include BC_SYSBASE
#include "shvars.h"
#include "infoidx.h"
#include "glusrdef.h"
#include "earthloc.h"
#include "tcusrdef.h"
#include "erusrdef.h"
#include "ptusrdef.h"
#include "erusrdef.h"
#include "motifgraph.h"
#include "cbutil.h"
#include "cblib2.h"
#include "seismics.h"
#include "phaseinf.h"
#include "phasemgr.h"
#include "shm_widgets.h"
#include "sysext.h"
#include "globalparams.h"



/* prototypes of local routines */
static void cl2h_amplitude_selection( Widget wm, Widget ws, Widget pt,
	MGT_DSPCTRL *ctrl, int wdwno, CUT_SELECTIONS *select, char filname[],
	BOOLEAN *delete, STATUS *status );
static void cl2h_onset_acc_selection( Widget wm, Widget ws,
	CUT_SELECTIONS *sel, BOOLEAN acc, STATUS *status );
static void cl2h_ampl_surface_selection( Widget wm, Widget ws,
	MGT_DSPCTRL *ctrl, int wdwno, CUT_SELECTIONS *sel, STATUS *status );
static void  get_20s_max ( float data[], int n, float dt, float *absbest,
	float *period, float *time );



/*---------------------------------------------------------------------------*/



void cl2_magnitude( int type, CUT_PARAMS *par, STATUS *status )

/* Determines magnitude mb and MS.  "type" specifies type of magnitude
 * to be determined.  Searches all P-phases selected on Z-component
 * and computes magnitude.  Result is put to P-phase info "magnitude".
 *
 * parameters of routine
 * int        type;            input; type of magnitude
 * CUT_PARAMS *par;            input; analysis parameters
 * TSyStatus  *status;         output; return status
 */
{
	/* local variables */
	TPiPhaseRoot *proot;               /* phase root pointer */
	TPiPhaseList *plist;               /* pointer to phase list */
	char        comp;                  /* component */
	TSyStatus   locstat;               /* local status */
	TPiPhase    *phase;                /* current phase */
	int         done_cnt;              /* counts phases found */
	char        station[BC_LINELTH+1]; /* station name */
	GLT_STATINF *statinf;              /* station info */
	double      distance, az, baz;     /* distance, azimuth, back-azimuth */
	char        str[BC_LINELTH+1];     /* scratch */
	STATLOC     stat_lat, stat_lon;    /* station location */
	TSyBoolean  loc_given;             /* epicenter location given */

	/* executable code */

	/* check location of epicenter */
	loc_given = TRUE;
	distance = 0.0;
	if  (par->source_lat == 0.0 && par->source_lon == 0.0)  {
		if  (par->distance == 0.0)  {
			*status = CB2E_NO_LOCATION;
			return;
		} /*endif*/
		distance = par->distance;
		if  (par->dist_unit == CUC_DIST_UNIT_KM)
			distance /= CUC_DEG_TO_KM;
		loc_given = FALSE;
	} /*endif*/

	/* loop over all phases in memory */
	done_cnt = 0;
	proot = NULL;
	FOREVER  {

		/* get next phase root pointer */
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;

		/* check component */
		comp = PiCompOfRoot( proot );
		if  (type == SIC_MAGN_MB)  {
			if  (comp != 'Z')  continue;
		} else if  (type >= SIC_MAGN_MS_PLAIN && type <= SIC_MAGN_MS_O)  {
			/*
			 * if  (comp != 'N' && comp != 'E' && comp != 'R' && comp != 'T')
			 * 	continue;
			 */
		} else {
			printf( "*SHM: cl2_magnitude: program bug (1) ***\n" );
			return;
		} /*endif*/

		/* retrieve station info */
		strcpy( station, PiStationOfRoot(proot) );
		if  (loc_given)  {
			if  (strcmp(station,"BEAM") == 0 || strcmp(station,"ALIGN") == 0)  {
				cu_reference_location( &stat_lat, &stat_lon,
					NULL,NULL,NULL, status );
				if  (Severe(status))  return;
			} else {
				statinf = gl_store_station( station, TRUE, status );
				if  (Severe(status))  return;
				if  (!gl_valid_number(statinf->lat))  continue;
				if  (!gl_valid_number(statinf->lon))  continue;
				stat_lat = statinf->lat;
				stat_lon = statinf->lon;
			} /*endif*/
		} /*endif*/

		/* get phase list and check info */
		plist = PiPhaseListOfRoot( proot );
		if  (type == SIC_MAGN_MB)  {
			phase = PiFindPhase( plist, "P" );
		} else if  (type >= SIC_MAGN_MS_PLAIN && type <= SIC_MAGN_MS_O)  {
			phase = PiFindPhase( plist, CUC_LP_PHASE_NAME );
		} else {
			printf( "*SHM: cl2_magnitude: program bug (2) ***\n" );
			return;
		} /*endif*/
		if  (phase == NULL)  continue;
		if  (phase->ampl_displ <= 0.0)  continue;
		if  (phase->period <= 0.0)  continue;

		/* compute epicentral distance, if location given */
		if  (loc_given)  {
			mb_locdiff( stat_lat, stat_lon, par->source_lat, par->source_lon,
				&distance, &az, &baz );
		} else {
			distance = par->distance;
			if  (par->dist_unit == CUC_DIST_UNIT_KM)
				distance /= CUC_DEG_TO_KM;
		} /*endif*/
		if  (type == SIC_MAGN_MB)  {
			phase->magnitude = si_magn_mb( phase->ampl_displ, phase->period,
				distance, status );
			phase->magn_source = cPiMagnMb;
			sprintf( str, "mb: %3.1f", phase->magnitude );
			if  (phase->bb.bbampl > 0.0)  {
				phase->bb.mbb = si_magn_mb( phase->bb.bbampl, 2.0*M_PI, distance,
					status );
			} /*endif*/
		} else if  (type == SIC_MAGN_MS_PLAIN)  {
			phase->magnitude = si_magn_ms_plain( phase->ampl_displ, phase->period,
				distance, status );
			phase->magn_source = cPiMagnMsPlain;
			sprintf( str, "MS: %3.1f", phase->magnitude );
		} else if  (type >= SIC_MAGN_MS_C_NA && type <= SIC_MAGN_MS_O)  {
			phase->magnitude = si_magn_ms_m_b( type, phase->ampl_displ,
				phase->period, distance, status );
			phase->magn_source = type;
			sprintf( str, "MS: %3.1f", phase->magnitude );
		} else {
			printf( "*SHM: cl2_magnitude: program bug (3) ***\n" );
			return;
		} /*endif*/
		cu_print_info( str );
		if  (Severe(status))  return;
		/*cu_update_phase( station, comp, phase, status );*/
		/*if  (Severe(status))  return;*/
		done_cnt++;
	} /*endfor*/

	sprintf( str, "%d magnitudes determined", done_cnt );
	cu_print_info( str );
	if  (done_cnt == 0)
		*status = CB2E_NO_MAGNITUDE;

} /* end of cl2_magnitude */



/*---------------------------------------------------------------------------*/



void cl2_magnitude_ml_selection( CUT_PARAMS *par, CUT_SELECTIONS *sel,
	STATUS *status )

/* Reads selected time window for ml determination, finds correspondend
 * E & N components, searches for maximum amplitudes and computes ml.
 *
 * parameters of routine
 * CUT_PARAMS      *par;           input; analysis parameters
 * CUT_SELECTIONS  *sel;           input; window selection
 * TSyStatus       *status;        output; return status
 */
{
	/* local variables */
	TPmTrace *trc[2];                    /* N and E component straces */
	char     search_station[BC_SHORTSTRLTH+1];  /* look for this station name */
	char     station[BC_SHORTSTRLTH+1];  /* current station name */
	char     search_comp;                /* search for this component */
	char     comp;                       /* current component */
	int      t;                          /* trace counter */
	TIME     wdwstart, trcstart;         /* window & trace start times */
	TSyStatus locstat;                   /* local status */
	char     str[BC_LINELTH+1];          /* scratch string */
	float    relstart;                   /* relative time of start of window */
	long     idxstart;                   /* start index of trace */
	float    wdwlth;                     /* length of selected window */
	long     trclth;                     /* length in samples */
	long     ltrclth;                    /* local trace length */
	float    dt;                         /* sample rate in sec */
	float    *dat;                       /* pointer to data */
	float    amin[2], amax[2];           /* minima and maxima of traces */
	float    ml_ampl;                    /* amplitude for ml */
	unsigned dist_entry;                 /* distance entry */
	float    distance_km;                /* distance in km */
	double   dist_deg, az, baz;          /* scratch */
	TPiPhase *phase;                     /* phase to insert magnitude info */
	GLT_STATINF *statinf;                /* station info */

	/* executable code */

	/* check if number of selections is ok */
	if  (sel->processed != 2)  {
		printf( "*SHM: cl2_magnitude_ml_selection: program bug (1) ***\n" );
		*status = CB2E_PROGRAM_BUG;
		return;
	} /*endif*/

	db_ident( "DISTANCE", &dist_entry, status );
	if  (Severe(status))  return;

	/* find second trace if window markers are on the same trace */
	trc[0] = sel->phtrc[0];
	if  (sel->phtrc[0] == sel->phtrc[1])  {
		/* get station name and component & check it */
		db_gets( trc[0], ES_STATION, BC_SHORTSTRLTH, search_station, status );
		if  (Severe(status))  return;
		search_comp = db_getc( trc[0], EC_COMP, status );
		if  (Severe(status))  return;
		if  (search_comp == 'N')  {
			search_comp = 'E';
		} else if  (search_comp == 'E')  {
			search_comp = 'N';
		} else {
			*status = CB2E_WRONG_COMP;
			return;
		} /*endif*/
		trc[1] = NULL;
		FOREVER  {
			locstat = cBcNoError;
			trc[1] = db_getp( trc[1], EP_DSPN, &locstat );
			if  (trc[1] == NULL)  break;
			locstat = BC_NOERROR;
			db_gets( trc[1], ES_STATION, BC_SHORTSTRLTH, station, &locstat);
			if  (Severe(&locstat))  continue;
			comp = db_getc( trc[1], EC_COMP, &locstat );
			if  (Severe(&locstat))  continue;
			if  (comp == search_comp && strcmp(station,search_station) == 0)
				break;
		} /*endfor*/
		if  (trc[1] == NULL)  {
			*status = CB2E_ML_NEED_2;
			return;
		} /*endif*/
	} else {
		trc[1] = sel->phtrc[1];
		locstat=BC_NOERROR;
		if  (db_getc(trc[0],EC_COMP,&locstat) == 'Z'
			|| db_getc(trc[1],EC_COMP,&locstat) == 'Z')  {
			*status = CB2E_WRONG_COMP;
			return;
		} /*endif*/
	} /*endif*/

	/* check sample rates */
	dt = db_getr( trc[0], ER_DELTA, status );
	if  (Severe(status))  return;
	if  (fabs(db_getr(trc[1],ER_DELTA,status)-dt) > dt*0.01)  {
		*status = CB2E_DIFF_SMP;
		return;
	} /*endif*/

	/* get length of selected window */
	wdwlth = tc_tdiff( sel->select[0].onset, sel->select[1].onset, status );
	if  (Severe(status))  return;
	wdwlth = fabs( wdwlth );
	trclth = Nlong( wdwlth / dt );
	tc_t2a( sel->select[0].onset, &wdwstart, status );
	if  (Severe(status))  return;

	/* find maximum amplitudes on both traces within window */
	for  (t=0; t<2; t++)  {
		db_gett( trc[t], ET_START, &trcstart, status );
		if  (Severe(status))  return;
		relstart = tc_adiff( &wdwstart, &trcstart );
		idxstart = Nlong( relstart / dt );
		if  (idxstart < 0)  idxstart = 0;
		ltrclth = trclth;
		if  (idxstart+ltrclth >= db_getl(trc[t],EL_LENGTH,status))  {
			ltrclth = db_getl(trc[t],EL_LENGTH,status) - idxstart;
			if  (ltrclth <= 1)  {
				*status = CB2E_ZERO_WDW;
				return;
			} /*endif*/
		} /*endif*/
		if  (Severe(status))  return;
		dat = (float *)db_getp( trc[t], EP_DATA, status ) + idxstart;
		if  (Severe(status))  return;
		amin[t] = amax[t] = *dat;
		while  (--ltrclth >= 0)  {
			dat++;
			if  (*dat < amin[t])  amin[t] = *dat;
			if  (*dat > amax[t])  amax[t] = *dat;
		} /*endwhile*/
		amin[t] = fabs( amin[t] );
		amax[t] = fabs( amax[t] );
		if  (amin[t] > amax[t])  amax[t] = amin[t];
	} /*endif*/
	ml_ampl = (amax[0] + amax[1]) / 2.0;

	/* get distance in km */
	distance_km = db_getr( trc[0], dist_entry, status );
	if  (Severe(status))  {
		if  (par->source_lat != 0.0 || par->source_lon != 0.0)  {
			*status = BC_NOERROR;
			statinf = gl_store_station( search_station, TRUE, status );
			if  (Severe(status))  return;
			mb_locdiff( statinf->lat, statinf->lon, par->source_lat,
				par->source_lon, &dist_deg, &az, &baz );
			distance_km = dist_deg * CUC_DEG_TO_KM;
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SHM-db3: took km-distance from source location (lat,lon)\n" );
		} else if  (par->dist_unit != CUC_DIST_UNIT_KM)  {
			printf( "*SHM: no km-distance found ***\n" );
			return;
		} else {
			*status = BC_NOERROR;
			distance_km = par->distance;
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SHM-dbg3: took km-distance from par-box\n" );
		} /*endif*/
	} /*endif*/

	sel->select[0].magnitude =
		si_magn_ml( ml_ampl, distance_km, status );
	sel->select[1].magnitude = sel->select[0].magnitude;

	/* magnitude should be inserted to first some phase */
	phase = PmFindPhaseV2( search_station, 'Z', "Pn" );
	if  (phase == NULL)  phase = PmFindPhaseV2( search_station, 'Z', "Pg" );
	if  (phase == NULL)  phase = PmFindPhaseV2( search_station, 'Z', "P" );
	if  (phase == NULL)  phase = PmFindPhaseV2( search_station, 'Z', "Pdiff" );
	if  (phase == NULL)  phase = PmFindPhaseV2( search_station, 'Z', "Sn" );
	if  (phase == NULL)  phase = PmFindPhaseV2( search_station, 'N', "Sn" );
	if  (phase == NULL)  phase = PmFindPhaseV2( search_station, 'E', "Sn" );
	if  (phase == NULL)  phase = PmFindPhaseV2( search_station, 'Z', "Sg" );
	if  (phase == NULL)  phase = PmFindPhaseV2( search_station, 'N', "Sg" );
	if  (phase == NULL)  phase = PmFindPhaseV2( search_station, 'E', "Sg" );
	if  (phase == NULL)  phase = PmFindPhaseV2( search_station, 'Z', "S" );
	if  (phase == NULL)  phase = PmFindPhaseV2( search_station, 'N', "S" );
	if  (phase == NULL)  phase = PmFindPhaseV2( search_station, 'E', "S" );
	if  (phase == NULL)  {
		printf( "*SHM: no P/S phases found on %s traces ***\n",
			search_station );
		*status = CB2E_NOMAGPHASE;
		return;
	} /*endif*/
	if  (Severe(status))  return;
	phase->magnitude = sel->select[0].magnitude;
	phase->magn_source = cPiMagnMl;

	/*cu_update_phase( search_station, ' ', phase, status );*/

} /* end of cl2_magnitude_ml_selection */



/*--------------------------------------------------------------------------*/



void cl2_process_selection( Widget w[], MGT_DSPCTRL *ctrl,
	int wdwno, CUT_PARAMS *par, CUT_SELECTIONS *sel, TPiPhase *curr,
	TPmTrace *phtrc, TSyStatus *status )

/* puts selection into list and calls routine which initiated the selections
 * if all selections are already made
 *
 * parameters of routine
 * Widget        w[];        input; widget array
 * MGT_DSPCTRL   *ctrl;      input; display control
 * int           wdwno;      input; window number
 * CUT_PARAMS    *par;       input; analysis parameters
 * CUT_SELECTIONS *sel;      modify; selections
 * TPiPhase      *curr;      input; current phase
 * TPmTrace      *phtrc;     input; phase trace
 * TSyStatus     *status;    output; return status
 */
{
	/* local variables */
	char        p[cPiMaxPhaseLth+1];    /* scratch name */
	int         i;                      /* counter */
	TSyBoolean  ok;                     /* deletion flag */
	TSyBoolean  delete;                 /* delete phases after use */
	TSyStatus   locstat=cBcNoError;     /* local status */
	char        *filname;               /* filter name */

	/* executable code */

	/* check whether last selection has been moved */
	if  (sel->processed > 0)  {
		if  (PmFindPhase(sel->phtrc[sel->processed-1],
			sel->select[sel->processed-1].name) == NULL)  {
			strcpy( p, sel->select[sel->processed-1].name );
			sel->select[sel->processed-1] = *curr;
			strcpy( sel->select[sel->processed-1].name, p );
			sel->phtrc[sel->processed-1] = phtrc;
			cu_accept_phase( w[k_widget_draw], w[k_widget_single_draw],
				&(sel->select[sel->processed-1]), sel->phtrc[sel->processed-1],
				ctrl->show_phase_acc, status );
			return;
		} /*endif*/
	} /*endif*/

	strcpy( p, sel->select[sel->processed].name );
	sel->select[sel->processed] = *curr;
	strcpy( sel->select[sel->processed].name, p );
	sel->phtrc[sel->processed] = phtrc;
	cu_accept_phase( w[k_widget_draw], w[k_widget_single_draw],
		&(sel->select[sel->processed]), sel->phtrc[sel->processed],
		ctrl->show_phase_acc, status );
	if  (SySevere(status))  return;

	if  (++(sel->processed) == sel->waiting)  {
		/* call initiator */
		mg_set_cursor( MGC_XCRSR_NORMAL );
		switch  (sel->initiator)  {
		case CUC_SELTYPE_AMPLPER_P:
		case CUC_SELTYPE_AMPLPER_Z:
		case CUC_SELTYPE_AMPL_MAN:
		case CUC_SELTYPE_PER_MAN:
			filname = cu_get_string( w[k_widget_filter_edit_text] );
			cl2h_amplitude_selection( w[k_widget_draw], w[k_widget_single_draw],
				w[k_widget_phase_name_text], ctrl, wdwno, sel, filname, &delete,
				status );
			break;
		case CUC_SELTYPE_AMPL_SURFACE:
			cl2h_ampl_surface_selection( w[k_widget_draw], w[k_widget_single_draw],
				ctrl, wdwno, sel, status );
			delete = TRUE;
			break;
		case CUC_SELTYPE_MAGN_ML:
			cl2_magnitude_ml_selection( par, sel, status );
			delete = TRUE;
			break;
		case CUC_SELTYPE_ONSET_ACC:
			cl2h_onset_acc_selection( w[k_widget_draw], w[k_widget_single_draw],
				sel, ctrl->show_phase_acc, status );
			delete = TRUE;
			break;
		default:
			*status = CUE_NOINITIATOR;
			delete = TRUE;
			break;
		} /*endswitch*/
		if  (delete)  {
			/* delete selected phases */
			for  (i=0; i<sel->waiting; i++)  {
				locstat = cBcNoError;
				PmRemovePhase( sel->phtrc[i], &(sel->select[i]), &locstat );
				if  (locstat == cBcNoError) 
					mg_mark_one_phase( w[k_widget_draw], w[k_widget_single_draw],
						&(sel->select[i]), sel->phtrc[i], ctrl->show_phase_acc );
			} /*endfor*/
		} /*endif*/
		/* remove status text */
		mg_print_status( w[k_widget_draw], sel->infotext, FALSE );
		/* reset selections */
		sel->waiting = 0;
		sel->processed = 0;
		sel->initiator = CUC_SELTYPE_NONE;
	} else {
		mg_set_cursor( MGC_XCRSR_LEFT );
	} /*endif*/

} /* end of cl2_process_selection */



/*--------------------------------------------------------------------------*/



static void cl2h_amplitude_selection( Widget wm, Widget ws, Widget pt,
	MGT_DSPCTRL *ctrl, int wdwno, CUT_SELECTIONS *sel, char filname[],
	TSyBoolean *delete, TSyStatus *status )

/* processing amplitude selection
 *
 * parameters of routine
 * Widget        wm;             input; widget ID of main drawing area
 * Widget        ws;             input; widget ID of single trace drawing area
 * Widget        pt;             input; widget ID of phase text
 * MGT_DSPCTRL   *ctrl;          input; display control
 * int           wdwno;          input; window number
 * CUT_SELECTIONS *select;       input; selections
 * char          filname[];      input; current filter applied
 * TSyBoolean    *delete;        output; delete phases after return
 * TSyStatus     *status;        output; return status
 */
{
	/* local variables */
	void     *trcptr1, *trcptr2;      /* trace pointers */
	float    *datptr;                 /* pointer to data */
	TIME     t_start;                 /* start time of trace */
	TIME     t_pick;                  /* picked time (window start) */
	STATUS   locstat;                 /* local status */
	float    dt;                      /* sample distance */
	float    tl_offset;               /* time offset in sec */
	float    tl_sellth;               /* selection length in sec */
	long     offset;                  /* offset in samples */
	long     sellth;                  /* selection length in samples */
	long     i;                       /* counter */
	float    datmin, datmax;          /* amplitude maxima */
	long     minpos, maxpos;          /* minimum and maximum position */
	long     minpos0, maxpos0;        /* minimum and maximum position (2)*/
	float    exact_minpos;            /* interpolated minimum position */
	float    exact_maxpos;            /* interpolated maximum position */
	long     absmaxpos;               /* position of absolute maximum */
	float    absmaxposrel;            /* above position relative to onset */
	TSyBoolean max_ok, min_ok;        /* zeroes found for maximum/minimum */
	long     startidx, wdwlth;        /* window between minimum & maximum */
	char     *findname;               /* phase name from phase dialog */
	TPiPhase *findphase;              /* pointer to P-phase structure */
	MGT_GRAPHIC_INTERP interp;        /* interpolation structure */
	float    tmp;                     /* scratch */
	TSyBoolean make_printout;         /* printout results at end */
	char     c_station[BC_SHORTSTRLTH+1]; /* current station */
	char     comp;                    /* component */
	void     *reftrc;                 /* reference trace */
	int      apsrc;                   /* amplitude & period source */

	/* executable code */

	*delete = TRUE;
	make_printout = TRUE;

	trcptr1 = sel->phtrc[0];
	trcptr2 = sel->phtrc[1];
	if  (trcptr1 == NULL || trcptr2 == NULL)  {
		printf( "*SHM: NULL trace ptrs in amplitude_selections ***\n" );
		return;
	} /*endif*/
	locstat = cBcNoError;

	datptr = db_getp( trcptr1, EP_DATA, status );
	if  (Severe(status))  return;
	dt = db_getr( trcptr1, ER_DELTA, status );
	if  (Severe(status))  return;
	db_gett( trcptr1, ET_START, &t_start, status );
	if  (Severe(status))  return;
	tc_t2a( sel->select[0].onset, &t_pick, status );
	if  (Severe(status))  return;
	tl_offset = tc_adiff( &t_pick, &t_start );
	tl_sellth = tc_tdiff( sel->select[1].onset, sel->select[0].onset,
		status );
	if  (Severe(status))  return;

	offset = Nlong( tl_offset / dt );
	sellth = Nlong( tl_sellth / dt );

	datmin = datmax = datptr[offset];
	minpos = maxpos = offset;
	for  (i=offset+1; i<(offset+sellth); i++)  {
		if  (datptr[i] > datmax)  {
			datmax = datptr[i];
			maxpos = i;
		} /*endif*/
		if  (datptr[i] < datmin)  {
			datmin = datptr[i];
			minpos = i;
		} /*endif*/
	} /*endfor*/

	findname = cu_get_string( pt );
	if  (*findname == 'b')  {
		*status = CUE_DUMMY_PHASE;
		err_setcontext( " ## phase " ); err_setcontext( findname );
		return;
	} /*endif*/
	findphase = PmFindPhase( trcptr1, findname );
	apsrc = cPiApsrcDirect;
	if  (findphase == NULL)  {
		/* try to find phase on reference station if BEAM or ALIGN trace */
		locstat = BC_NOERROR;
		comp = db_getc( trcptr1, EC_COMP, &locstat );
		reftrc = cl2_station_on_display( GpGetString(cGpS_refstation), comp );
		locstat = BC_NOERROR;
		db_gets( trcptr1, ES_STATION, BC_SHORTSTRLTH, c_station, &locstat );
		if  (locstat == BC_NOERROR)  {
			if  (strcmp(c_station,"BEAM") == 0)  {
				apsrc= cPiApsrcBeam;
			} else if  (strcmp(c_station,"ALIGN") == 0)  {
				apsrc = cPiApsrcAlign;
			} /*endif*/
		} /*endif*/
		if  (Severe(&locstat) || apsrc == cPiApsrcDirect)  {
			*status = CUE_NOPHASE;
			err_setcontext( " ## phase " ); err_setcontext( findname );
			return;
		} /*endif*/
		findphase = NULL;
		if  (reftrc != NULL)
			findphase = PmFindPhase( reftrc, findname );
		if  (findphase == NULL)  {
			*status = CUE_NOPHASE;
			err_setcontext( " ## phase " ); err_setcontext( findname );
			return;
		} /*endif*/
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: take phase %s on reference station %s\n",
				findname, GpGetString(cGpS_refstation) );
	} else {
		locstat = BC_NOERROR;
		db_gets( trcptr1, ES_STATION, BC_SHORTSTRLTH, c_station, &locstat );
	} /*endif*/

	/* check time difference between phase time and window start */
	tmp = tc_tdiff( sel->select[1].onset, findphase->onset, status );
	if  (Severe(status))  return;
	if  (tmp < 0.0 || tmp > GpGetFloat(cGpF_phase_ampl_diff))  {
		*status = CB2E_AMPLTIME_DIFF;
		err_setcontext( " ## phase " ); err_setcontext( findname );
		return;
	} /*endif*/

	absmaxpos = (Abs(datmax) > Abs(datmin)) ? maxpos : minpos;
	tc_aadd( &t_start, (float)absmaxpos*dt, &t_pick );
	tc_t2a( findphase->onset, &t_start, status );
	if  (Severe(status))  {
		err_setcontext( " ## time " ); err_setcontext(findphase->onset);
		return;
	} /*endif*/
	absmaxposrel = tc_adiff( &t_pick, &t_start );

	if  (sel->initiator == CUC_SELTYPE_AMPLPER_P)  {
		startidx = (maxpos > minpos) ? minpos : maxpos;
		startidx -= db_getl( trcptr1, EL_DSPFST, NULL );
		exact_minpos = (float)minpos;
		tmp = 2.0*datptr[minpos] - datptr[minpos-1] - datptr[minpos+1];
		if  (Abs(tmp) > 1.0e-10)
			exact_minpos += 0.5 * (datptr[minpos+1]-datptr[minpos-1]) / tmp;
		exact_maxpos = (float)maxpos;
		tmp = 2.0*datptr[maxpos] - datptr[maxpos-1] - datptr[maxpos+1];
		if  (Abs(tmp) > 1.0e-10)
			exact_maxpos += 0.5 * (datptr[maxpos+1]-datptr[maxpos-1]) / tmp;
		wdwlth = maxpos - minpos;
		wdwlth = Abs(wdwlth) + 1;
		if  (*filname != '\0')  {
			findphase->period = 2.0 * fabs(exact_maxpos-exact_minpos) * dt;
			findphase->ampl = (datmax - datmin) / 2.0;
			findphase->ampl_veloc = 0.0;
			findphase->ampl_displ = 0.0;
			findphase->ap_source = apsrc;
			locstat = BC_NOERROR;
			cu_ampl_restitution( trcptr1, findphase->ampl,
				findphase->period, "VEL", 		
				&(findphase->ampl_veloc), filname/*findphase->filter*/, &locstat );
			if  (Severe(&locstat))  findphase->ampl_veloc = 0.0;
			locstat = BC_NOERROR;
			cu_ampl_restitution( trcptr1, findphase->ampl,
				findphase->period, "DSP",
				&(findphase->ampl_displ), filname/*findphase->filter*/, &locstat );
			if  (Severe(&locstat))  findphase->ampl_displ = 0.0;
			findphase->ampl_time = absmaxposrel;
		} else {
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SHM-dbg3: BB amplitude determination\n" );
			findphase->bb.bbperiod = 2.0 * fabs(exact_maxpos-exact_minpos) * dt;
			findphase->bb.bbampl = (datmax - datmin) / 2.0;
			findphase->bb.bbampl_time = absmaxposrel;
			/* same to be done to Z-option !!!!! */
		} /*endif*/
		interp.do_interpolation = FALSE;   /* should be changed */
	} else if  (sel->initiator == CUC_SELTYPE_AMPLPER_Z)  {
		min_ok = max_ok = TRUE;
		if  (datmax <= 0.0)  {
			max_ok = FALSE;
		} else {
			minpos0 = maxpos0 = maxpos;
			while  (datptr[minpos0] > 0. && max_ok)
				if  (--minpos0 < offset)
					max_ok = FALSE;
			while  (datptr[maxpos0] > 0. && max_ok)
				if  (++maxpos0 >= (offset+sellth))
					max_ok = FALSE;
		} /*endif*/
		if  (datmin >= 0.0)  {
			min_ok = FALSE;
		} else {
			maxpos = minpos;
			while  (datptr[minpos] < 0. && min_ok)
				if  (--minpos < offset)
					min_ok = FALSE;
			while  (datptr[maxpos] < 0. && min_ok)
				if  (++maxpos >= (offset+sellth))
					min_ok = FALSE;
		} /*endif*/
		if  (!max_ok && !min_ok)  {
			*status = CUE_NOZEROLINE;
			return;
		} /*endif*/
		interp.do_interpolation = TRUE;
		interp.ampl_1 = 0.0;
		interp.ampl_e = 0.0;
		if  ((max_ok && min_ok && Abs(datmax) > Abs(datmin)) ||
			!min_ok)  {
			exact_minpos = (datptr[minpos0]*(float)(minpos0+1) -
				(float)minpos0*datptr[minpos0+1]) /
				(datptr[minpos0]-datptr[minpos0+1]);
			exact_maxpos = (datptr[maxpos0-1]*(float)maxpos0 -
				(float)(maxpos0-1)*datptr[maxpos0]) /
				(datptr[maxpos0-1]-datptr[maxpos0]);
			interp.frac_1 = exact_minpos - (float)minpos0;
			interp.frac_e = exact_maxpos - (float)maxpos0;
			startidx = minpos0;
			wdwlth = maxpos0 - minpos0 + 1;
		} else {
			exact_minpos = (datptr[minpos]*(float)(minpos+1) -
				(float)minpos*datptr[minpos+1]) /
				(datptr[minpos]-datptr[minpos+1]);
			exact_maxpos = (datptr[maxpos-1]*(float)maxpos -
				(float)(maxpos-1)*datptr[maxpos]) /
				(datptr[maxpos-1]-datptr[maxpos]);
			interp.frac_1 = exact_minpos - (float)minpos;
			interp.frac_e = exact_maxpos - (float)maxpos;
			startidx = minpos;
			wdwlth = maxpos - minpos + 1;
			datmax = -datmin;
		} /*endif*/
		startidx -= db_getl( trcptr1, EL_DSPFST, NULL );
		/* findphase->period = 2.0 * (float)(wdwlth-1) * dt; */
		findphase->period = 2.0 * (exact_maxpos - exact_minpos) * dt;
		findphase->ampl = datmax;
		findphase->ampl_veloc = 0.0;
		findphase->ampl_displ = 0.0;
		findphase->ap_source = apsrc;
		locstat = BC_NOERROR;
		cu_ampl_restitution( trcptr1, findphase->ampl,
			findphase->period, "VEL",
			&(findphase->ampl_veloc), filname/*findphase->filter*/, &locstat );
		if  (Severe(&locstat))  findphase->ampl_veloc = 0.0;
		locstat = BC_NOERROR;
		cu_ampl_restitution( trcptr1, findphase->ampl,
			findphase->period, "DSP",
			&(findphase->ampl_displ), filname/*findphase->filter*/, &locstat );
		if  (Severe(&locstat))  findphase->ampl_displ = 0.0;
		findphase->ampl_time = absmaxposrel;
	} else if  (sel->initiator == CUC_SELTYPE_PER_MAN)  {
		startidx = offset;
		startidx -= db_getl( trcptr1, EL_DSPFST, NULL );
		wdwlth = sellth;
		findphase->period = tl_sellth;
		findphase->ampl = (datmax - datmin) / 2.0;
		findphase->ampl_veloc = 0.0;
		findphase->ampl_displ = 0.0;
		findphase->ap_source = apsrc;
		locstat = BC_NOERROR;
		cu_ampl_restitution( trcptr1, findphase->ampl,
			findphase->period, "VEL",
			&(findphase->ampl_veloc), filname/*findphase->filter*/, &locstat );
		if  (Severe(&locstat))  findphase->ampl_veloc = 0.0;
		locstat = BC_NOERROR;
		cu_ampl_restitution( trcptr1, findphase->ampl,
			findphase->period, "DSP",
			&(findphase->ampl_displ), filname/*findphase->filter*/, &locstat );
		if  (Severe(&locstat))  findphase->ampl_displ = 0.0;
		findphase->ampl_time = absmaxposrel;
		cu_multiplication_dialog( trcptr1, findphase );
		make_printout = FALSE;
		interp.do_interpolation = FALSE;  /* this should be changed */
	} else {
		printf( "*SHM: initiator %d not implemented ***\n",
			sel->initiator );
		return;
	} /*endif*/

	/* update phase info on other traces of the same station */
	/*locstat = BC_NOERROR;*/
	/*cu_update_phase( c_station, ' ', findphase, &locstat );*/

	i = mg_trcnum( sel->phtrc[0] );
	if  (i == 0)  {
		*status = CB2E_PROGRAM_BUG;
		err_setcontext( " ## plot_bold: phase not on display\n" );
		return;
	} /*endif*/
	mg_plot_bold( wm, ws, ctrl, wdwno, i, startidx, wdwlth, &interp, status );

	if  (make_printout)
		cu_print_phase( findphase );

} /* end of cl2h_amplitude_selection */



/*---------------------------------------------------------------------------*/



static void cl2h_onset_acc_selection( Widget wm, Widget ws,
	CUT_SELECTIONS *sel, TSyBoolean acc, TSyStatus *status )

/* Get onset accuracy from selection phases
 *
 * parameters of routine
 * Widget         wm, ws;      input; widget ID's for main and single trace wdw
 * CUT_SELECTIONS *sel;        input; selection phases
 * TSyBoolean     acc;         input; show phase accuracies
 * TSyStatus      *status;     output; return status
 */
{
	/* local variables */
	TPiPhase  *p_phase;        /* parameter phase */
	float     tdiff;           /* time difference */

	/* executable code */

	p_phase = (TPiPhase *)(sel->addparam);
	if  (p_phase == NULL)  {
		if  (GpGetInt(cGpI_debug_level) > 0)
			printf( "SHM-dbg1: NULL phase pointer passed to onset acc selection\n" );
		return;
	} else if  (sel->phtrc[0] == NULL)  {
		if  (GpGetInt(cGpI_debug_level) > 0)
			printf( "SHM-dbg1: NULL phasetrc in passed phase to onset acc selection\n" );
		return;
	} /*endif*/

	tdiff = tc_tdiff( p_phase->onset, sel->select[0].onset, status );
	if  (Severe(status))  return;
	if  (tdiff < 0.0)  {
		if  (GpGetInt(cGpI_debug_level) > 3)
			printf("SHM-dbg4: tdiff-l corr 0.0\n");
		tdiff = 0.0;
	} /*endif*/
	p_phase->onset_acc_l = tdiff;
	tdiff = tc_tdiff( sel->select[1].onset, p_phase->onset, status );
	if  (Severe(status))  return;
	if  (tdiff < 0.0)  {
		if  (GpGetInt(cGpI_debug_level) > 3)
			printf( "SHM-dbg4: tdiff-r corr 0.0\n" );
		tdiff = 0.0;
	} /*endif*/
	p_phase->onset_acc_r = tdiff;

	/* if phase accuracies are to be displayed, delete phase from screen and */
	/* redraw it with accuracies */
	if  (acc)  {
		mg_mark_one_phase( wm, ws, p_phase, sel->phtrc[0], FALSE );
		mg_mark_one_phase( wm, ws, p_phase, sel->phtrc[0], TRUE );
	} /*endif*/

	/*mg_scan_traces( mg_trcnum(p_phase->phasetrc)-1, p_phase, TRUE );*/

} /* end of cl2h_onset_acc_selection */



/*---------------------------------------------------------------------------*/



#define ONSET_PREFIX "onset"
#define ONSET_EXEC "shm_exec_onset"



void cl2_extern_theo( Widget wm, Widget ws, CUT_PARAMS *par, TSyBoolean acc,
	TSyStatus *status )

/* Calls external program to compute theoretical travel times and puts the
 * phases to the traces on screen.
 *
 * parameters of routine
 * Widget     wm, ws;      input; widget ID's of main and single trace window
 * CUT_PARAMS *par;        input; analysis parameters
 * TSyBoolean acc;         input; display accuracy
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	TPmTrace *trc;                           /* trace pointer */
	char     station[BC_SHORTSTRLTH+1];      /* station name */
	char     last_station[BC_SHORTSTRLTH+1]; /* last station name */
	TSyStatus locstat;                       /* local status */
	GLT_STATINF *statinf;                    /* station info */
	FILE     *fp;                            /* pointer to file */
	char     fname[BC_FILELTH+1];            /* name of file */
	float    elevation;                      /* station elevation */
	char     cmd[BC_LONGSTRLTH+1];           /* shell command line */
	char     selfile[BC_FILELTH+1];          /* selected phase list */
	char     outfile[BC_FILELTH+1];          /* name of result file */
	char     line[BC_LINELTH+1];             /* current line */
	char     phase[BC_SHORTSTRLTH+1];        /* phase name */
	char     onset_t[BC_TIMELTH+1];          /* onset time of phase */
	TPiPhaseList *plist;                     /* phase list */
	int      plist_lth;                      /* phase list length */
	int      p;                              /* phase counter */
	TPiPhase *rem_phase;                     /* phase to be removed */
	TPiPhase tphase;                         /* theo phase */
	int      phase_type;                     /* type of phase */

	/* executable code */

	/* find phase type */
	if  (par->dist_unit == CUC_DIST_UNIT_DEG)  {
		if  (par->distance < 2.0)  {
			phase_type = CUC_TYPE_LOCAL_QUAKE;
		} else if  (par->distance < 10.0)  {
			phase_type = CUC_TYPE_REGIO_QUAKE;
		} else {
			phase_type = CUC_TYPE_TELE_QUAKE;
		} /*endif*/
	} else {
		if  (par->distance < 200.0)  {
			phase_type = CUC_TYPE_LOCAL_QUAKE;
		} else if  (par->distance < 1000.0)  {
			phase_type = CUC_TYPE_REGIO_QUAKE;
		} else {
			phase_type = CUC_TYPE_TELE_QUAKE;
		} /*endif*/
	} /*endif*/

	/* create output file with event info and list of stations */

	strcpy( fname, shd_scratch );
	strcat( fname, ONSET_PREFIX );
	strcat( fname, "_list.txt" );
	fp = sy_fopen( fname, "w" );
	if  (fp == NULL)  {
		*status = CB2E_OPEN_WRITE;
		return;
	} /*endif*/

	fprintf( fp, "%e %e %e %s\n", par->source_lat, par->source_lon,
		par->depth, par->origin );

	*last_station = '\0';

	trc = NULL;
	FOREVER  {
		locstat = cBcNoError;
		trc = db_getp( trc, EP_DSPN, &locstat );
		if  (trc == NULL)  break;
		locstat = cBcNoError;
		db_gets( trc, ES_STATION, BC_SHORTSTRLTH, station, &locstat );
		if  (Severe(&locstat))  continue;
		if  (strcmp(station,last_station) == 0)  continue;
		strcpy( last_station, station );
		if  (strcmp(station,"BEAM") == 0)  continue;
		if  (strcmp(station,"ALIGN") == 0)  continue;
		statinf = gl_store_station( station, TRUE, &locstat );
		if  (Severe(&locstat))  continue;
		if  (!gl_valid_number(statinf->lat))  continue;
		if  (!gl_valid_number(statinf->lon))  continue;
		elevation = 0.0;
		if  (gl_valid_number(statinf->elevation))
			elevation = statinf->elevation;
		fprintf( fp, "%s %lf %lf %f\n", station, statinf->lat,
			statinf->lon, elevation );
	} /*endfor*/

	sy_fclose( fp );

	/* get phase selection file and name of output file */
	strcpy( selfile, shd_inputs );
	strcat( selfile, "onset_phase_selection.txt" );
	strcpy( outfile, shd_scratch );
	strcat( outfile, ONSET_PREFIX );
	strcat( outfile, "_result.txt" );

	/* create and execute command line */
	sprintf( cmd, "%s/%s %s %s %s", GpGetString(cGpS_defpath_extprog),
		ONSET_EXEC, fname, selfile, outfile );
	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: cl2_extern_theo: executed command: %s\n", cmd );
	system( cmd );

	/* delete all theoretical phases from all traces */
	/* should use cu_delete_all_phases !!! */
	trc = NULL;
	FOREVER  {
		locstat = cBcNoError;
		trc = db_getp( trc, EP_DSPN, &locstat );
		if  (trc == NULL)  break;
		plist = PmGetPhaseList( trc );
		plist_lth = PiListLength( plist );
		for  (p=0; p<plist_lth; p++)  {
			locstat = BC_NOERROR;
			rem_phase = PiGetPhase( plist, p, &locstat );
			if  (locstat == BC_NOERROR &&
				rem_phase->source == cPiSourceTheo)  {
				mg_mark_one_phase( wm, ws, rem_phase, trc, acc );
				PiDeletePhase( plist, rem_phase, &locstat );
				p--;
				plist_lth--;
			} /*endif*/
		} /*endfor*/
	} /*endfor*/

	/* read output file */
	fp = sy_fopen( outfile, "r" );
	if  (fp == NULL)  {
		printf( "*SHM: couldn't open result file of onset_convert\n" );
		*status = CB2E_OPEN_READ;
		return;
	} /*endif*/

	/* setup phase structure */
	cu_reset_phase( &tphase );
	tphase.name[0] = '\0';  /* set later */
	tphase.spec = cPiSpecEmergent;
	/*tphase.phasetrc = NULL;*/  /* set later */
	tphase.use = FALSE;
	tphase.source = cPiSourceTheo;

	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (*line == '!')  continue;
		sscanf( line, "%s %s %s", last_station, phase, onset_t );
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: found %s %s %s\n", last_station, phase, onset_t );
		trc = NULL;
		FOREVER  {
			locstat = cBcNoError;
			trc = db_getp( trc, EP_DSPN, &locstat );
			if  (trc == NULL)  break;
			db_gets( trc, ES_STATION, BC_SHORTSTRLTH, station, &locstat );
			if  (Severe(&locstat))  continue;
			if  (strcmp(station,last_station) != 0)  continue;
			/*tphase.phasetrc = trc;*/
			strcpy( tphase.onset, onset_t );
			strcpy( tphase.name, phase );
			/* put phase on screen */
			cu_accept_phase( wm, ws, &tphase, trc, FALSE, status );
			if  (SySevere(status))  return;
		} /*endfor*/
	} /*endwhile*/

	sy_fclose( fp );

} /* end of cl2_extern_theo */


#undef ONSET_PREFIX
#undef ONSET_EXEC



/*---------------------------------------------------------------------------*/



void cl2_modify_comment( char **comment, TSyStatus *status )

/* Let the user change comment text '*comment'.  If *comment==NULL, current
 * comment text is empty.  The input comment memory is freed and new
 * memory allocated for new comment.
 *
 * parameters of routine
 * char       *comment;      modify; pointer to comment text
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	char     cmtfile[BC_FILELTH+1];     /* scratch file for comment */
	FILE     *cmt;                      /* pointer to comment file */
	char     cmd[BC_LINELTH+1];         /* shell command */
	long     cmtsize;                   /* size of comment in bytes */
	long     readsize;                  /* length of comment read */

	/* executable code */

	strcpy( cmtfile, shd_scratch );
	strcat( cmtfile, "gencomment.tmp" );

	/* write current comment to scratch file */
	cmt = sy_fopen( cmtfile, "w" );
	if  (cmt == NULL)  {
		*status = CB2E_OPEN_WRITE;
		err_setcontext( " ## file " ); err_setcontext( cmtfile );
		return;
	} /*endif*/
	if  (*comment != NULL)
		fputs( *comment, cmt );
	sy_fclose( cmt );

	/* free memory */
	if  (*comment != NULL)
		sy_deallocmem( *comment );

	/* open editor and wait for it */
	sprintf( cmd, "%s %s", GpGetString(cGpS_texteditor), cmtfile );
	system( cmd );

	/* open output file */
	cmt = sy_fopen( cmtfile, "r" );
	if  (cmt == NULL)  {
		*status = CB2E_OPEN_READ;
		err_setcontext( " ## file " ); err_setcontext( cmtfile );
		return;
	} /*endif*/

	/* get size and read file */
	fseek( cmt, 0, 2 );
	cmtsize = ftell( cmt );
	cmtsize--;   /* empirical */
	if  (cmtsize <= 0)  {
		*comment = NULL;
		return;
	} /*endif*/
	fseek( cmt, 0, 0 );
	*comment = (char *)sy_allocmem( cmtsize+1, (int)sizeof(char), status );
	if  (Severe(status))  {sy_fclose( cmt ); return;}
	readsize = fread( *comment, sizeof(char), cmtsize, cmt );
	(*comment)[readsize] = '\0';
	if  (readsize != cmtsize)  {
		*status = CB2E_READ_ERR;
		err_setcontext( " ## error reading comment file" );
	} /*endif*/

	/* close and delete scratch file */
	sy_fclose( cmt );
	sy_fdelete( cmtfile );

} /* end of cl2_modify_comment */



/*---------------------------------------------------------------------------*/



void *cl2_station_on_display( char station[], char comp )

/* returns pointer to reference station or NULL if reference station is not
 * found
 *
 * parameters of routine
 * char       station[];     input; name of reference station
 * char       comp;          input; component
 */
{
	/* local variables */
	void       *trc;                          /* current trace */
	char       c_station[BC_SHORTSTRLTH+1];   /* current station */
	char       c_comp;                        /* current component */
	TSyStatus  locstat;                       /* local status */

	/* executable code */

	trc = NULL;
	FOREVER  {
		locstat = cBcNoError;
		trc = db_getp( trc, EP_DSPN, &locstat );
		if  (trc == NULL)  break;
		db_gets( trc, ES_STATION, BC_SHORTSTRLTH, c_station, &locstat );
		if  (Severe(&locstat))  continue;
		if  (strcmp(station,c_station) != 0)  continue;
		c_comp = db_getc( trc, EC_COMP, &locstat );
		if  (Severe(&locstat))  continue;
		if  (comp != c_comp)  continue;
		return trc;
	} /*endfor*/

	return NULL;

} /* end of cl2_station_on_display */



/*---------------------------------------------------------------------------*/



void cl2_display_busy( Widget w[], BOOLEAN on )

/* If program is busy (on=TRUE) menus are disabled and "computing ..."-Text
 * is written in window
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * BOOLEAN    on;         input; switch busy on or off
 */
{
	/* executable code */

	XtSetSensitive( w[k_widget_menu_main], !on );
	XtSetSensitive( w[k_widget_menu_calib], !on );
	mg_print_busy( w[k_widget_draw], on );

} /* end of cl2_display_busy */



/*---------------------------------------------------------------------------*/



void cl2_param_box_defaults( Widget w[], CUT_PARAMS *par )

/* sets values of parameter dialog box
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * CUT_PARAMS *par;       input; parameter values
 */
{
	/* local variables */
	Arg      al[1];       /* argument list */
	int      w_locq;      /* location quality widget */

	/* executable code */

	/* should be set to xmv_dspctrl.zoom */
	XmScaleSetValue( w[k_widget_param_ctrl_zoom_scale], 0 );

	/* set quality option menu */
	switch  (par->loc_quality)  {
	case CUC_LOCQ_INCOHERENT: w_locq = k_widget_param_locq_incoherent; break;
	case CUC_LOCQ_NOBEARING:  w_locq = k_widget_param_locq_nobearing; break;
	case CUC_LOCQ_REGION:     w_locq = k_widget_param_locq_region; break;
	case CUC_LOCQ_RELIABLE:   w_locq = k_widget_param_locq_reliable; break;
	default:  w_locq = k_widget_param_locq_tooweak; break;
	} /*endswitch*/
	XtSetArg( al[0], XmNmenuHistory, w[w_locq] );
	XtSetValues( w[k_widget_param_locq_option], al, 1 );

} /* end of cl2_param_box_defaults */



/*---------------------------------------------------------------------------*/



static void cl2h_ampl_surface_selection( Widget wm, Widget ws,
	MGT_DSPCTRL *ctrl, int wdwno, CUT_SELECTIONS *sel, TSyStatus *status )

/* manages automatic amplitude & period measurement on surface waves.
 * Selects phase L automatically.
 *
 * parameters of routine
 * STATUS        *status;       output; return status
 */
{
	/* local variables */
	TPmTrace *trcptr1, *trcptr2;      /* trace pointers */
	float    *datptr;                 /* pointer to data */
	float    dt;                      /* sample distance */
	TIME     t_start;                 /* start time of trace */
	TIME     t_pick;                  /* picked time (window start) */
	float    tl_offset;               /* time offset in sec */
	float    tl_sellth;               /* selection length in sec */
	long     offset;                  /* offset in samples */
	long     sellth;                  /* selection length in samples */
	long     ampl_index;              /* start sample of LP-window */
	float    ampl_value;              /* plain amplitude */
	float    ampl_period;             /* period of amplitude */
	float    ampl_time;               /* amplitude time relative to wdw start */
	int      i;                       /* counter */
	long     startidx;                /* plot bold start index */
	long     wdwlth;                  /* plot bold window length */
	TPiPhase l_phase;                 /* L phase */
	TSyStatus locstat;                /* local status */
	MGT_GRAPHIC_INTERP interp;        /* interpolation structure */

	/* executable code */

	trcptr1 = sel->phtrc[0];
	trcptr2 = sel->phtrc[1];
	if  (trcptr1 == NULL || trcptr2 == NULL)  {
		printf( "*SHM: NULL trace ptrs in amplitude_selections ***\n" );
		return;
	} /*endif*/

	datptr = db_getp( trcptr1, EP_DATA, status );
	if  (Severe(status))  return;
	dt = db_getr( trcptr1, ER_DELTA, status );
	if  (Severe(status))  return;
	db_gett( trcptr1, ET_START, &t_start, status );
	if  (Severe(status))  return;
	tc_t2a( sel->select[0].onset, &t_pick, status );
	if  (Severe(status))  return;
	tl_offset = tc_adiff( &t_pick, &t_start );
	tl_sellth = tc_tdiff( sel->select[1].onset, sel->select[0].onset,
		status );
	if  (Severe(status))  return;

	offset = Nlong( tl_offset / dt );
	sellth = Nlong( tl_sellth / dt );

	if  (GpGetInt(cGpI_debug_level) > 2)
		printf( "SHM-dbg3: picked start %s, offset %d, length %d\n",
			sel->select[0].onset, offset, sellth );
	/* call frank's routine (datptr+offset,sellth,dt) */
	ampl_index = 10;
	ampl_value = 100.0;
	ampl_period = 20.0;
	get_20s_max( datptr+offset, sellth, dt, &ampl_value,
		&ampl_period, &ampl_time );
	if  (ampl_period == 0.0)  {
		*status = CB2E_L_NOTFOUND;
		return;
	} /*endif*/
	ampl_index = Nint( ampl_time / dt );
	if  (ampl_index > Nint(sellth/dt))  {
		*status = CB2E_L_ILLIDX;
		printf( "*SHM: illegal length returned from L selection routine\n" );
		return;
	} /*endif*/
	ampl_value /= 2.0;

	/* select L-phase automatically */
	cu_reset_phase( &l_phase );
	strcpy( l_phase.name, "L" );
	l_phase.source = cPiSourceAuto;
	/*l_phase.phasetrc = trcptr1;*/
	tc_tadd( sel->select[0].onset, ampl_index*dt,
		l_phase.onset, status );
	if  (Severe(status))  return;
	l_phase.period = ampl_period;
	l_phase.ampl = ampl_value;
	l_phase.ampl_veloc = 0.0;
	l_phase.ampl_displ = 0.0;
	l_phase.ap_source = cPiApsrcDirect;
	locstat = cBcNoError;
	cu_ampl_restitution( trcptr1, l_phase.ampl,
		l_phase.period, "VEL", 		
		&(l_phase.ampl_veloc), l_phase.filter, &locstat );
	if  (Severe(&locstat))  l_phase.ampl_veloc = 0.0;
	locstat = BC_NOERROR;
	cu_ampl_restitution( trcptr1, l_phase.ampl,
		l_phase.period, "DSP",
		&(l_phase.ampl_displ), l_phase.filter, &locstat );
	if  (Severe(&locstat))  l_phase.ampl_displ = 0.0;
	l_phase.ampl_time = 0.0;
	cu_accept_phase( wm, ws, &l_phase, trcptr1, FALSE, status );
	if  (Severe(status))  return;
	interp.do_interpolation = FALSE;   /* should be changed */

	startidx = offset + ampl_index;
	startidx -= db_getl( trcptr1, EL_DSPFST, NULL );
	wdwlth = Nint( l_phase.period / (2.0*dt) );  /* mark half period only */

	i = mg_trcnum( sel->phtrc[0] );
	if  (i == 0)  {
		*status = CB2E_PROGRAM_BUG;
		err_setcontext( " ## plot_bold: phase not on display\n" );
		return;
	} /*endif*/
	mg_plot_bold( wm, ws, ctrl, wdwno, i, startidx, wdwlth, &interp, status );
	if  (Severe(status))  return;

} /* end of cl2h_ampl_surface_selection */



/*---------------------------------------------------------------------------*/



/* Routine to find 20sec maximum in LP filtered data
 * F. Krueger, 27-Sep-95
 */



#define n1 1000
#define n4 50000
static void  get_20s_max ( float data[], int n, float dt, float *absbest,
	float *period, float *time )
   {
/*
 *  this program searches in a given data array the maximum min/max
 *  amplitude in the period window 18.0 - 22.0 sec. 
 *  It is used for
 *  Ms determinations. 
 *
 *  It is assumed that the data are SRO-LP filtered data, i.e. 
 *  that they are smooth and contain no high frequencies, spikes
 *  etc.
 *
 *  INPUT:   data
 *           npun
 *           dt
 *
 *  OUTPUT:  time, period and amplitude
 *
 */
    int     nn;                          /* no of min/max */
    int     i,ii;                        /* counters */
    int     nmax,nmin;                   /* no of maximas, no of minimas */
    float   diffminper[n1];              /* periods */
    float   diffmaxper[n1];
    float   minpos[n1];                  /* pos. of min. */
    float   maxpos[n1];                  /* pos. of max. */
    float   max[n1];                     /* ampl. of max. */
    float   min[n1];                     /* ampl. of min. */
    float   median;                      /* average of data */
    float   absmax;                      /* dummy */
    float   absmin;                      /* dummy */
    float   absdiffmax;                  /* the following are values */
    float   absdiffmin;                  /* which i need for the 2 cases */
    float   maxtime,mintime;             /* handled: (1)1st element is a max.*/
    float   maxperiod,minperiod;         /*          (2) 1st el. is a min. */
/*
 *  subtract median
 */
#ifdef XXX
    for ( i=0; i<n; i++ )
       {
        median+= data[i]/(float)n;
       }
    for ( i=0; i<n; i++ )
       {
        data[i] = data[i] - median;
       }
#endif
    for ( i=0; i<n1; i++ )
       {
        max[i] = 0.0;
        maxpos[i] = 0.0;
        min[i] = 0.0;
        minpos[i] = 0.0;
        diffmaxper[i] = 0.0;
        diffminper[i] = 0.0;
       }
    *period = 0.0;
    *absbest = 0.0;
    *time = 0.0;
/*
 *  search maximas 
 */
    ii = 0;
    for ( i=0; i<n-2; i++ )
       {
        if((data[i]<data[i+1]) && (data[i+1]>data[i+2]))
          {
           max[ii] = data[i+1];
           maxpos[ii] = (float)(i+1)*dt;
           ii++;
           if ( ii > n1 )
             {
              return;
             }
          }
       }
    nmax = ii;
/* 
 *  search minimas 
 */
    ii = 0; 
    for ( i=0; i<n-2; i++ ) 
       { 
        if((data[i]>data[i+1]) && (data[i+1]<data[i+2])) 
          { 
           min[ii] = data[i+1]; 
           minpos[ii] = (float)(i+1)*dt; 
           ii++; 
           if ( ii > n1 ) 
             { 
              return;
             } 
          }    
       } 
    nmin = ii;
/*
 *  where are the 20 s ?
 */
    if ( nmax < nmin )           /* take the smaller one */
      {
       nn = nmax;
      }
    else
      {
       nn = nmin;
      }
    if ( maxpos[0] < minpos[0] )
      {
       for ( i=0; i<nn; i++ )
          {
           diffmaxper[i] = fabs((double)(2.0*(maxpos[i]-minpos[i]))) - 20.0;
          }
       for ( i=0; i<nn; i++ )
          {
           diffminper[i] = fabs((double)(2.0*(maxpos[i+1]-minpos[i]))) - 20.0;
          }
      }
    else
      {
       for ( i=0; i<nn; i++ )
          {
           diffmaxper[i] = fabs((double)(2.0*(maxpos[i]-minpos[i+1]))) - 20.0;
          }
       for ( i=0; i<nn; i++ )
          {
           diffminper[i] = fabs((double)(2.0*(maxpos[i]-minpos[i]))) - 20.0;
          }  
      }
/*
 *  Now we must differentiate between 2 cases: (1) a maximum comes
 *  as the first in time (2) a minimum comes as the first one.
 *  Also both possibilities of min/max combination have to be checked
 *  in each case.
 */
    absmax = 0.0;
    absmin = 0.0;
    for ( i=0; i<nn; i++ )
       {
        if ( maxpos[0] < minpos[0] )
          {
           if ( fabs((double)diffmaxper[i]) <= 2.0 )
             {
              if ( fabs((double)max[i]) + fabs((double)min[i]) > absmax )
                {
                 maxperiod = diffmaxper[i] + 20.0;
                 absmax = fabs((double)(max[i])) + fabs((double)min[i]);
                 maxtime = maxpos[i];
                }
             }
           if ( fabs((double)diffminper[i]) <= 2.0 ) 
             { 
              if ( fabs((double)max[i+1]) + fabs((double)min[i]) > absmin ) 
                { 
                 minperiod = diffminper[i] + 20.0; 
                 absmin = fabs((double)max[i+1]) + fabs((double)min[i]); 
                 mintime = minpos[i]; 
                } 
             } 
          }
        else
          {
           if ( fabs((double)diffmaxper[i]) <= 2.0 )
             {
              if ( fabs((double)max[i]) + fabs((double)min[i+1]) > absmax )
                {
                 maxperiod = diffmaxper[i] + 20.0;
                 absmax = fabs((double)(max[i])) + fabs((double)min[i+1]);
                 maxtime = maxpos[i];
                }
             }    
           if ( fabs((double)diffminper[i]) <= 2.0 )
             {
              if ( fabs((double)max[i]) + fabs((double)min[i]) > absmin )
                { 
                 minperiod = diffminper[i] + 20.0; 
                 absmin = fabs((double)max[i]) + fabs((double)min[i]);
                 mintime = minpos[i]; 
                }  
             }    
          }
       }
/*
 *  take the best one and return
 */
    if ( absmax > absmin )
      {
       *period = maxperiod;
       *absbest = absmax;
       *time = maxtime;
      }
    else
      {
       *period = minperiod;
       *absbest = absmin;
       *time = mintime;
      } 
    return;
   }

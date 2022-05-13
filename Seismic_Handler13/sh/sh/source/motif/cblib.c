
/* file cblib.c
 *      =======
 *
 * version 42, 9-Nov-2006
 *
 * library of utility routines for callbacks
 * K. Stammler, 24-Jun-93
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
#include <Xm/Xm.h>
#include <Xm/Text.h>
#undef BC_DEFINE_TRUE_FALSE
#include BC_SYSBASE
#include "infoidx.h"
#include "tcusrdef.h"
#include "ptusrdef.h"
#include "fctxdm.h"
#include "fctxsl.h"
#include "rfusrdef.h"
#include "shvars.h"
#include "utusrdef.h"
#include "motifgraph.h"
#include "phaseinf.h"
#include "phasemgr.h"
#include "cbutil.h"
#include "cblib.h"
#include "shm_widgets.h"
#include "sysext.h"
#include "globalparams.h"


/* local types */

typedef struct _clt_statphase {
	struct _clt_statphase *next;              /* pointer to next info or NULL */
	char           station[BC_SHORTSTRLTH+1]; /* name of station */
	char           comp;                      /* component */
	TSyBoolean     used;                      /* phase used */
	TPiPhaseList   *plist;                    /* phase list */	
} CLT_STATPHASE;

typedef struct _auto_filter {
	struct _auto_filter *next;                /* pointer to next autofilter */
	char                *sname;               /* pointer to stream name */
	char                *filter;              /* pointer to filter */
} CLT_AUTOFILTER;



/* global variables */
static unsigned clv_file_select_mode;  /* used in cl_file_select_...-routines */
static CLT_STATPHASE *clv_phase_root=NULL;  /* root pointer of phase info */
static char clv_path[CLC_PATH_NUMBER][BC_FILELTH+1];  /* path pointers (FSB) */
static char clv_mask[CLC_PATH_NUMBER][BC_FILELTH+1];  /* file masks (FSB) */
static CLT_AUTOFILTER *clv_afilter_root=NULL; /* root pointer for autofilters */




/*---------------------------------------------------------------------------*/



void cl_fit_depth( float distance, float *depth, float *depth_err,
	int *depth_type, STATUS *status )

/* fits depth from specified depth phases
 *
 * parameters of routine
 * float      distance;    input; distance in deg
 * float      *depth;      output; depth in km
 * float      *depth_err;  output; depth error in km
 * int        *depth_type; output; depth type (CUC_DEPTH_...)
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	TPiPhaseList *plist;            /* phase list */
	TPiPhaseRoot *proot;            /* phase root pointer */
	STATUS   locstat;               /* local status */
	char     dphase[BC_LINELTH+1];  /* current depth phase */
	char     *sp, *dp;              /* moving pointers */
	TPiPhase *mps, *dps;            /* main & depth phase structures */
	float    tdiff;                 /* time difference */
	float    curr_depth;            /* current depth */
	float    mean_depth;            /* average depth */
	float    min_depth;             /* minimum depth */
	float    max_depth;             /* maximum depth */
	int      depth_count;           /* number of depth determined */
	TSyBoolean quit;                /* quit loop */

	/* executable code */

	if  (distance <= 1.0)  {
		*status = CLE_ILLEGALDIST;
		return;
	} /*endif*/

	depth_count = 0;
	mean_depth = 0.0;
	min_depth = max_depth = -1.0;
	*depth_type = CUC_DEPTH_LESSWELL;

	/* for each phaselist in memory */
	proot = NULL;
	FOREVER  {

		/* get next phase root */
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;

		/* get phase list pointer of trace */
		plist = PiPhaseListOfRoot( proot );
		if  (plist == NULL)  continue;

		/* for each depth phase in depth phase list */
		sp = GpGetString( cGpS_depth_phase_list );
		dp = dphase;
		quit = FALSE;
		while  (!quit)  {
			if  (*sp != ',' && *sp != '\0')  {
				*dp++ = *sp++;
				continue;
			} /*endif*/
			if  (*sp == '\0')  quit = TRUE;
			*dp = '\0';
			dps = PiFindPhase( plist, dphase );
			if  (dps != NULL)
				mps = PiFindPhase( plist, dphase+1 );
			if  (dps != NULL && mps != NULL)  {
				if  (GpGetInt(cGpI_debug_level) > 1)
					printf("SHM-dbg2: found depth phase %s in trace\n", dphase );
				tdiff = tc_tdiff( dps->onset,mps->onset,status);
				if (Severe(status))  return;
				locstat = BC_NOERROR;
				curr_depth = pt_depth( dphase+1, dphase,
					tdiff, distance, &locstat );
				if  (locstat == BC_NOERROR)  {
					depth_count++;
					mean_depth += curr_depth;
					if  (min_depth < 0.0)  {
						min_depth = curr_depth;
						max_depth = curr_depth;
					} /*endif*/
					if  (curr_depth < min_depth)
						min_depth = curr_depth;
					if  (curr_depth > max_depth)
						max_depth = curr_depth;
				} else {
					printf( "*SHM: could not fit depth\n" );
				} /*endif*/
			} /*endif*/
			dp = dphase;
			sp++;
		} /*endwhile*/

	} /*endfor*/

	if  (depth_count == 0)  {
		*status = CLE_NODEPTHPHASE;
		return;
	} /*endif*/

	mean_depth /= (float)depth_count;
	if  (GpGetInt(cGpI_debug_level) > 1)  {
		printf( "SHM-dbg2: found %d depth phases\n", depth_count );
		printf( "SHM-dbg2: mean, min, max depth: %f %f %f\n",
			mean_depth, min_depth, max_depth );
	} /*endif*/

	*depth = mean_depth;
	*depth_err = mean_depth - min_depth;
	if  ((max_depth-mean_depth) > *depth_err)
		*depth_err = max_depth - mean_depth;

} /* end of cl_fit_depth */



/*---------------------------------------------------------------------------*/



void cl_file_select_init( Widget w, unsigned mode, STATUS *status )

/* initializes FileSelectorDialog
 *
 * parameters of routine
 * Widget     w;       input; Widget ID of FileSelectorBox
 * unsigned   mode;    input; kind of file to return
 * STATUS     *status; output; return status
 */
{
	/* local variables */
	static   TSyBoolean is_init=FALSE;      /* initialization done */
	unsigned n;                             /* counter */
	Arg      arg[6];                        /* arguments */

	/* executable code */

	if  (mode >= CLC_PATH_NUMBER)  {
		*status = CLE_ILLPATHMODE;
		return;
	} /*endif*/
	clv_file_select_mode = mode;

	/* initialize static variables if empty */
	if  (!is_init)  {
		if  (clv_path[CLC_PATH_FILTER][0] == '\0')  {
			if  (GpGetStringElem(cGpL_defpath_filter,1) == NULL)  {
				strcpy( clv_path[CLC_PATH_FILTER], "/" );
			} else {
				strcpy( clv_path[CLC_PATH_FILTER],
					GpGetStringElem(cGpL_defpath_filter,1) );
			} /*endif*/
		} /*endif*/
		if  (clv_path[CLC_PATH_EVENTS][0] == '\0')
			strcpy( clv_path[CLC_PATH_EVENTS], GpGetString(cGpS_defpath_events) );
		if  (clv_path[CLC_PATH_GSE][0] == '\0')
			strcpy( clv_path[CLC_PATH_GSE], GpGetString(cGpS_defpath_gse) );
		if  (clv_path[CLC_PATH_GSE2][0] == '\0')
			strcpy( clv_path[CLC_PATH_GSE2], GpGetString(cGpS_defpath_gse) );
		if  (clv_path[CLC_PATH_AH][0] == '\0')
			strcpy( clv_path[CLC_PATH_AH], GpGetString(cGpS_defpath_ah) );
		if  (clv_path[CLC_PATH_Q][0] == '\0')
			strcpy( clv_path[CLC_PATH_Q], GpGetString(cGpS_defpath_q) );
		if  (clv_path[CLC_PATH_SAVE][0] == '\0')
			strcpy( clv_path[CLC_PATH_SAVE], shd_scratch );
		if  (clv_path[CLC_PATH_EVT][0] == '\0')
			strcpy( clv_path[CLC_PATH_EVT], GpGetString(cGpS_defpath_evtout) );
		if  (clv_path[CLC_PATH_SFD][0] == '\0')
			strcpy( clv_path[CLC_PATH_SFD], GpGetString(cGpS_defpath_data) );
		if  (clv_mask[CLC_PATH_FILTER][0] == '\0')
			strcpy( clv_mask[CLC_PATH_FILTER], "*.FL?" );
		if  (clv_mask[CLC_PATH_EVENTS][0] == '\0')
			strcpy( clv_mask[CLC_PATH_EVENTS], "*.t*" );
		if  (clv_mask[CLC_PATH_GSE][0] == '\0')
			strcpy( clv_mask[CLC_PATH_GSE], "*" );
		if  (clv_mask[CLC_PATH_GSE2][0] == '\0')
			strcpy( clv_mask[CLC_PATH_GSE2], "*" );
		if  (clv_mask[CLC_PATH_AH][0] == '\0')
			strcpy( clv_mask[CLC_PATH_AH], "*.AH" );
		if  (clv_mask[CLC_PATH_Q][0] == '\0')
			strcpy( clv_mask[CLC_PATH_Q], "*.QBN" );
		if  (clv_mask[CLC_PATH_SAVE][0] == '\0')
			strcpy( clv_mask[CLC_PATH_SAVE], "*.sav" );
		if  (clv_mask[CLC_PATH_EVT][0] == '\0')
			strcpy( clv_mask[CLC_PATH_EVT], "*.evt" );
		if  (clv_mask[CLC_PATH_SFD][0] == '\0')
			strcpy( clv_mask[CLC_PATH_SFD], "*.sfd" );
		is_init = TRUE;
	} /*endif*/

	n = 0;
	XtSetArg( arg[n], XmNdirectory, XmStringCreateLocalized(clv_path[mode]));n++;
	XtSetArg( arg[n], XmNdirSpec, XmStringCreateLocalized(clv_path[mode]) ); n++;
	XtSetArg( arg[n], XmNfileTypeMask, XmFILE_REGULAR ); n++;
	XtSetArg( arg[n], XmNpattern, XmStringCreateLocalized(clv_mask[mode]) ); n++;
	XtSetValues( w, arg, n );
	/* don't know whether I have to free the XmString's above ??? */

	if  (XtIsManaged(w))  {
		XtUnmanageChild( w );
	} else {
		XtManageChild( w );
	} /*endif*/

} /* end of cl_file_select_init */



/*---------------------------------------------------------------------------*/



void cl_file_select_callback( Widget w[],
	XmFileSelectionBoxCallbackStruct *dat, int maxlth, char fname[], int *mode )

/* puts selected filename to appropriate place
 *
 * parameters of routine
 * Widget     w[];     input; widget array of all widgets
 * XmFileSelectionBoxCallbackStruct *dat;  input; return values of callback
 * int        maxlth;  input; maximum length of output filename
 * char       fname[]; output; filename selected
 * int        *mode;   output; which kind of file
 */
{
	/* local variables */
	char     *file;                       /* filename selected */
	int      slen;                        /* string length */
	char     fpath[cBcFileLth+1];         /* file path */
	int      i;                           /* counter */
	char     shellcmd[cBcLongStrLth+1];   /* shell command */
	TSyBoolean redraw;                    /* not used */
	TSyStatus  status;                    /* not used */

	/* executable code */

	*mode = clv_file_select_mode;

	XmStringGetLtoR( dat->mask, XmSTRING_DEFAULT_CHARSET, &file );
	if  (strlen(file) < BC_FILELTH)  strcpy( clv_mask[*mode], file );
	XmStringGetLtoR( dat->dir, XmSTRING_DEFAULT_CHARSET, &file );
	if  (strlen(file) < BC_FILELTH)  strcpy( clv_path[*mode], file );
	XmStringGetLtoR( dat->value, XmSTRING_DEFAULT_CHARSET, &file );
	/* do I have to free file ??? */

	if  (strlen(file) > maxlth)  {
		printf( "*SHM: filename truncated ***\n" );
		strncpy( fname, file, maxlth );
		fname[maxlth] = '\0';
	} else {
		strcpy( fname, file );
	} /*endif*/

#	ifdef XXX
	if  (XtIsManaged(w[k_widget_filesel]))
		XtUnmanageChild( w[k_widget_filesel] );
#	endif

	switch (clv_file_select_mode)  {
	case CLC_PATH_EVENTS:
		slen = strlen( GpGetString(cGpS_defpath_events) );
		if  (strncmp(GpGetString(cGpS_defpath_events),file,slen) == 0)  {
			file += slen;
			if  (*file == '/')  file++;
		} /*endif*/
		cu_set_string( w[k_widget_read_grsn_eventfile_text], file );
		break;
	case CLC_PATH_GSE:
	case CLC_PATH_GSE2:
	case CLC_PATH_AH:
	case CLC_PATH_Q:
	case CLC_PATH_RFMT:
		/* reading of GSE/AH/Q files is done in caller of this routine */
		break;
	case CLC_PATH_SAVE:
	case CLC_PATH_EVT:
		/* also done in xm_call_file_selection */
		break;
	case CLC_PATH_SFD:
		if  (strlen(file) > cBcFileLth)  {
			printf( "*SHM: cl_file_select_callback: output string too short\n" );
			return;
		} /*endif*/
		/* determine path from filename */
		strcpy( fpath, file );
		i = strlen(fpath) - 1;
		while  (i > 0)  {
			if  (fpath[i] == '/')  {
				fpath[i] = '\0';
				break;
			} /*endif*/
			i--;
		} /*endwhile*/
		strcat( fpath, "/" );
		/* set path in dialog box */
		cu_set_string( w[k_widget_read_grsn_device], fpath );
		/* execute statlist_build.sh on sfdfile selected */
		GpSetString( cGpS_defpath_data, fpath, NULL );
		strcpy( shellcmd, "$SH_UTIL/statlist_build.sh " );
		strcat( shellcmd, file );
		system( shellcmd );
		/* read in STATLIST_SFD.STX created by statlist_build.sh */
		strcpy( shellcmd, "SHM_CMD_READ_DIALOG STATLIST_SFD.STX" );
		status = cBcNoError;
		callsh( shellcmd, &redraw, &status );
		system( "\\rm STATLIST_SFD.STX" );
		break;
	default:
		printf( "*SHM: cl_file_select_callback: this cannot happen\n" );
		return;
	} /*endswitch*/

} /* end of cl_file_select_callback */



/*---------------------------------------------------------------------------*/



void cl_auto_beam_pick( Widget wm, Widget ws, char phasename[],
	TSyBoolean delete, TSyStatus *status )

/* performs automatic beam picking
 *
 * parameters of routine
 * Widget     wm, ws;      input; widget ID's of main and single window
 * char       phase[];     input; phase name
 * TSyBoolean delete;      input; delete existing phases before
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	int      ref_trace;          /* number of reference trace */
	float    time_lo, time_hi;   /* time window of search */
	TSyBoolean ok;               /* operation ok */
	TPmTrace *trc;               /* trace pointer */
	TPmTrace *reftrc;            /* reference trace */
	long     start, lth;         /* search window in samples */
	float    *smp;               /* pointer to sample data */
	float    min, max;           /* minimum & maximum */
	long     minpos, maxpos;     /* positions of minimum & maximum */
	TSyBoolean take_max;         /* take maximum or minimum */
	long     smp_pos;            /* sample position of minimum/maximum */
	float    exact_pos;          /* interpolated position */
	TSyStatus locstat;           /* local status */
	TPiPhase *phase;             /* phase info block pointer */
	TPiPhase autopick;           /* automatic pick */
	float    dt;                 /* sample distance in sec */
	TIME     t_begin, t_onset;   /* start and onset time */
	float    tmp;                /* scratch */

	/* executable code */

	mg_get_drag_trace( &ref_trace, &ok );
	if  (!ok)  {*status = CLE_NODRAGWDW; return;}
	mg_get_drag_window( &time_lo, &time_hi, &ok );
	if  (!ok)  {*status = CLE_NODRAGWDW; return;}

	/* delete all phases of name "phasename" from all traces if requested */
	if  (delete)  cu_delete_all_phases( wm, ws, phasename, TRUE );

	/* default values on autopick phase */
	cu_reset_phase( &autopick );
	if  (strlen(phasename) > cPiMaxPhaseLth)  {*status = CLE_STROVFL; return;}
	strcpy( autopick.name, phasename );
	/*autopick.phasetrc = mg_trcptr( ref_trace );*/
	autopick.use = FALSE;
	autopick.source = cPiSourceAuto;

	/* process reference trace */
	trc = mg_trcptr( ref_trace );
	reftrc = trc;
	start = dm_getsample( trc, time_lo, TRUE );
	lth = dm_getsample( trc, time_hi, TRUE ) - start + 1;
	if  (lth <= 1)  {*status = CLE_ZEROWDW; return;}
	smp = (float *)db_getp( trc, EP_DATA, status );
	if  (Severe(status))  return;
	sl_findmaxpos( smp+start, lth, &min, &max, &minpos, &maxpos );
	max = Abs( max );
	min = Abs( min );
	if  (GpGetBoolean(cGpB_autopick_first))  {
		take_max = maxpos < minpos;   /* this takes the first minimum/maximum */
	} else {
		take_max = max > min;         /* this takes the bigger amplitude */
	} /*endif*/
	if  (take_max && (maxpos == 0 || maxpos == lth-1))  take_max = FALSE;
	if  (!take_max && (minpos == 0 || minpos == lth-1))  take_max = TRUE;
	if  (take_max && (maxpos == 0 || maxpos == lth-1))  {
		*status = CLE_NOEXTREMUM;
		return;
	} /*endif*/
	smp_pos = (take_max) ? maxpos : minpos;
	smp_pos += start;
	/* do interpolation, there is nocheck for existance of smp[smp_pos+,-1] */
	exact_pos = (float)smp_pos;
	tmp = 2.0*smp[smp_pos] - smp[smp_pos-1] - smp[smp_pos+1];
	if  (Abs(tmp) > 1.0e-10)
		exact_pos += 0.5 * (smp[smp_pos+1]-smp[smp_pos-1]) / tmp;
	/* check phase */
	phase = PmFindPhase( trc, phasename );
	if  (phase != NULL)  {*status = CLE_DOUBLEAUTO; return;}
	/* compute onset time */
	dt = db_getr( trc, ER_DELTA, status );
	if  (Severe(status))  return;
	db_gett( trc, ET_START, &t_begin, status );
	if  (Severe(status))  return;
	tc_aadd( &t_begin, exact_pos*dt, &t_onset );
	tc_a2t( &t_onset, autopick.onset, status );
	if  (Severe(status))  return;
	/* put phase on screen */
	cu_accept_phase( wm, ws, &autopick, trc, FALSE, status );
	if  (Severe(status))  return;

	/* process all other traces */
	trc = NULL;
	FOREVER  {
		locstat = cBcNoError;
		trc = db_getp( trc, EP_DSPN, &locstat );
		if  (trc == NULL)  break;
		if  (trc == reftrc)  continue;
		locstat = cBcNoError;
		phase = PmFindPhase( trc, phasename );
		if  (phase != NULL)  continue;
		start = dm_getsample( trc, time_lo, TRUE );
		lth = dm_getsample( trc, time_hi, TRUE ) - start + 1;
		if  (lth <= 1)  {printf("*SHM: zero wdw on trace\n"); continue;}
		smp = (float *)db_getp( trc, EP_DATA, status );
		if  (Severe(status))  return;
		sl_findmaxpos( smp+start, lth, &min, &max, &minpos, &maxpos );
		smp_pos = (take_max) ? maxpos : minpos;
		if  (smp_pos == 0 || smp_pos == lth-1)  continue;
		smp_pos += start;
		/* do interpolation, no check for existance of smp[smp_pos+,-1] */
		exact_pos = (float)smp_pos;
		tmp = 2.0*smp[smp_pos] - smp[smp_pos-1] - smp[smp_pos+1];
		if  (Abs(tmp) > 1.0e-10)
			exact_pos += 0.5 * (smp[smp_pos+1]-smp[smp_pos-1]) / tmp;
		/* compute onset time */
		dt = db_getr( trc, ER_DELTA, status );
		if  (Severe(status))  return;
		db_gett( trc, ET_START, &t_begin, status );
		if  (Severe(status))  return;
		tc_aadd( &t_begin, exact_pos*dt, &t_onset );
		tc_a2t( &t_onset, autopick.onset, status );
		if  (Severe(status))  return;
		/* put phase on screen */
		/*autopick.phasetrc = trc;*/
		cu_accept_phase( wm, ws, &autopick, trc, FALSE, status );
		if  (Severe(status))  return;
	} /*endfor*/

} /* end of cl_auto_beam_pick */



/*---------------------------------------------------------------------------*/



void cl_correlation_pick(  Widget wm, Widget ws, char phasename[],
	TSyStatus *status )

/* Makes picks by crosscorrelation
 *
 * parameters of routine
 * Widget     wm, ws;      input; widget ID's of main and single window
 * char       phasename[]; input; phase name
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	int      ref_trace;          /* number of reference trace */
	TPmTrace *trc;               /* trace pointer */
	TPiPhase *phase;             /* phase info block pointer */
	TPiPhase autopick;           /* new phase */
	char     comp;               /* component */
	char     reftime[cBcTimeLth+1]; /* refernce onset time */
	float    time_lo, time_hi;   /* time window of search */
	TSyBoolean ok;               /* operation ok */
	char     shcmd[cBcLongStrLth+1]; /* SH command string */
	TSyBoolean redraw;           /* redraw necessary (ignored) */
	char     resfile[cBcFileLth+1];  /* name of result file */
	char     *env;               /* pointer to environment variable */
	FILE     *fp;                /* pointer to result file */
	char     line[cBcLineLth+1]; /* current line of file */
	char     station[cBcLineLth+1]; /* station name */
	float    reltime;            /* relative onset time */
	float    corrcoeff;          /* correlation coefficient */

	/* excutable code */

	mg_get_drag_trace( &ref_trace, &ok );
	if  (!ok)  {*status = CLE_NODRAGWDW; return;}
	mg_get_drag_window( &time_lo, &time_hi, &ok );
	if  (!ok)  {*status = CLE_NODRAGWDW; return;}

	trc = mg_trcptr( ref_trace );
	phase = PmFindPhase( trc, phasename );
	if  (phase == NULL)  {
		*status = CLE_NOPHASE;
		return;
	} /*endif*/
	strcpy( reftime, phase->onset );
	comp = db_getc( trc, EC_COMP, NULL );

	sprintf( shcmd, "SHM_CMD_CORRPICK %d %8.3f %8.3f",
		ref_trace, time_lo, time_hi );
	callsh( shcmd, &redraw, status );
	if  (SySevere(status))  return;

	/* get result file */
	env = (char *)getenv( "SH_SCRATCH" );
	if  (strlen(env) > cBcFileLth-13)  {
		*status = CLE_STROVFL;
		return;
	} /*endif*/
	strcpy( resfile, env );
	strcat( resfile, "CORRPICK.STX" );
	fp = fopen( resfile, "r" );
	if  (fp == NULL)  {
		*status = CLE_FILENOTFOUND;
		return;
	} /*endif*/

	/* default values on autopick phase */
	cu_reset_phase( &autopick );
	if  (strlen(phasename) > cPiMaxPhaseLth)  {*status = CLE_STROVFL; return;}
	strcpy( autopick.name, phasename );
	autopick.use = TRUE;
	autopick.source = cPiSourceAuto;

	/* read through file */
	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		if  (*line == '\0' || *line == '!')  continue;
		if  (sscanf(line,"%s %f %f",station,&reltime,&corrcoeff) != 3)  {
			printf( "*SHM: syntax error in line of %s\n", resfile );
			printf( "%s", line );
			continue;
		} /*endif*/
		tc_tadd( reftime, reltime, autopick.onset, status );
		if  (SySevere(status))  return;
		printf( "*SHM  station %5s  reltime %8.3f  abstime %s\n",
			station, reltime, autopick.onset );
		trc = cl_find_station_trc( station, comp );
		if  (trc == NULL)  {
			printf( "*SHM: cl_correlation_pick: this should not happen\n" );
			continue;
		} /*endif*/
		phase = PmFindPhase( trc, phasename );
		if  (phase != NULL)  {
			PmRemovePhase( trc, phase, status );
			mg_mark_one_phase( wm, ws, phase, trc, FALSE );
			if  (SySevere(status))  return;
		} /*endif*/
		PmAddPhase( trc, &autopick, status );
		if  (SySevere(status))  return;
		mg_mark_one_phase( wm, ws, &autopick, trc, FALSE );
	} /*endwhile*/

	fclose( fp );

} /* end of cl_correlation_pick */



/*---------------------------------------------------------------------------*/



TPmTrace *cl_find_station_trc( char station[], char comp )

/* Returns trace pointer of first trace found with given station and
 * component or returns NULL.
 *
 * parameters of routine
 * char       station[];    input; station name to search
 * char       comp;         input; component
 */
{
	/* local variables */
	TPmTrace *trc;                     /* trace pointer */
	char     cstat[cBcShortStrLth+1];  /* current station name */
	char     ccomp;                    /* current component */
	TSyStatus locstat;                 /* local status */

	/* executable code */

	locstat = cBcNoError;
	trc = NULL;
	FOREVER {
		trc = db_getp( trc, EP_DSPN, NULL );
		if  (trc == NULL)  break;
		db_gets( trc, ES_STATION, cBcShortStrLth, cstat, &locstat );
		if  (SySevere(&locstat))  return NULL;
		ccomp = db_getc( trc, EC_COMP, &locstat );
		if  (SySevere(&locstat))  return NULL;
		if  (strcmp(station,cstat) == 0 && comp == ccomp)  return trc;
	} /*endfor*/

	return trc;

} /* end of cl_find_station_trc */



/*---------------------------------------------------------------------------*/



void cl_manage_filter_bounds( Widget w[], int mode, TSyStatus *status )

/* changes filter bounds after activation of arrow buttons
 *
 * parameters of routine
 * Widget     w[];      input; widget array
 * int        mode;     input; which button selected
 * TSyStatus  *status;  output; return status
 */
{
	/* local variables */
	char     *val;                   /* pointer to string value */
	char     str[BC_LINELTH+1];      /* scratch string */
	char     fmt[BC_SHORTSTRLTH+1];  /* format string */
	float    number;                 /* number from string */
	float    step;                   /* step size */
	int      slen;                   /* string length */
	TSyBoolean in_hz;                /* units in Hz or s */
	float    lo_frq, hi_frq;         /* frequency bounds */
	int      order;                  /* order of filter */
	TSyBoolean increment;            /* increment number or decrement */

	/* executable code */

	if  (mode == CLC_BUTPAR_ORDER_UP || mode == CLC_BUTPAR_ORDER_DOWN)  {
		val = cu_get_string( w[k_widget_filter_butpar_order_text] );
		if  (*val == '\0')  {
			cu_set_string( w[k_widget_filter_butpar_order_text], "1" );
			return;
		} /*endif*/
		if  (sscanf(val,"%d",&order) != 1)  {
			*status = CLE_READINT;
			return;
		} /*endif*/
		if  (mode == CLC_BUTPAR_ORDER_UP)  {
			if  (++order > 5)  order = 5;
		} else {
			if  (--order < 1)  order = 1;
		} /*endif*/
		sprintf( str, "%d", order );
		cu_set_string( w[k_widget_filter_butpar_order_text], str );
		return;
	} /*endif*/

	step = 1.1;

	/* get text */
	switch  (mode)  {
	case CLC_BUTPAR_LO_UP:
	case CLC_BUTPAR_LO_DOWN:
		val = cu_get_string( w[k_widget_filter_butpar_lo_text] );
		if  (*val == '\0')  {
			cu_set_string( w[k_widget_filter_butpar_lo_text], "100s" );
			return;
		} /*endif*/
		break;
	case CLC_BUTPAR_HI_UP:
	case CLC_BUTPAR_HI_DOWN:
		val = cu_get_string( w[k_widget_filter_butpar_hi_text] );
		if  (*val == '\0')  {
			cu_set_string( w[k_widget_filter_butpar_hi_text], "5Hz" );
			return;
		} /*endif*/
		break;
	default:
		printf( "*SHM: cl_manage_filter_bounds: this cannot happen\n" );
		return;
	} /*endswitch*/

	/* get number */
	slen = (int)strlen( val );
	if  (slen > BC_LINELTH)  {
		*status = CLE_STROVFL;
		return;
	} /*endif*/
	strcpy( str, val );
	if  (str[slen-1] == 's')  {
		in_hz = FALSE;
		if  (sscanf(str,"%fs",&number) != 1)  {
			*status = CLE_READFLOAT;
			return;
		} /*endif*/
	} else {
		in_hz = TRUE;
		if  (sscanf(str,"%fHz",&number) != 1)  {
			*status = CLE_READFLOAT;
			return;
		} /*endif*/
	} /*endif*/

	/* change value */
#ifdef XXX
	switch  (mode)  {
	case CLC_BUTPAR_LO_UP:
	case CLC_BUTPAR_HI_UP:
		number *= step;
		break;
	case CLC_BUTPAR_LO_DOWN:
	case CLC_BUTPAR_HI_DOWN:
		number /= step;
		break;
	} /*endswitch*/
#endif
	increment = (mode == CLC_BUTPAR_LO_UP || mode == CLC_BUTPAR_HI_UP);
	if  (number > 30.0 || (increment && number == 30.0))  {
		number = (increment) ? number+10.0 : number-10.0;
		strcpy( fmt, "%3.0f" );
	} else if  (number > 10.0 || (increment && number == 10.0))  {
		number = (increment) ? number+5.0 : number-5.0;
		strcpy( fmt, "%3.0f" );
	} else if  (number > 5.0 || (increment && number == 5.0))  {
		number = (increment) ? number+1.0 : number-1.0;
		strcpy( fmt, "%3.0f" );
	} else if  (number > 3.0 || (increment && number == 3.0))  {
		number = (increment) ? number+0.5 : number-0.5;
		strcpy( fmt, "%5.1f" );
	} else if  (number > 2.0 || (increment && number == 2.0))  {
		number = (increment) ? number+0.2 : number-0.2;
		strcpy( fmt, "%5.1f" );
	} else {
		number = (increment) ? number+0.1 : number-0.1;
		strcpy( fmt, "%5.1f" );
	} /*endif*/

	/* check number */
	if  (number == 0.0)  number = 0.001;
	if  (number < 1.0 && number > 1.0e-10)  {
		in_hz = !in_hz;
		number = 1.0 / number;
	} /*endif*/

	/* put value back to string */
	if  (in_hz)  {
		strcat( fmt, "Hz" );
		sprintf( str, fmt, number );
	} else {
		strcat( fmt, "s" );
		sprintf( str, fmt, number );
	} /*endif*/

	switch  (mode)  {
	case CLC_BUTPAR_LO_UP:
	case CLC_BUTPAR_LO_DOWN:
		lo_frq = (in_hz) ? number : 1.0 / number;
		val = cu_get_string( w[k_widget_filter_butpar_hi_text] );
		if  (*val == '\0')  {
			cu_set_string( w[k_widget_filter_butpar_lo_text], str );
			break;
		} /*endif*/
		slen = strlen( val );
		sscanf( val, "%f", &hi_frq );
		if  (val[slen-1] == 's' && hi_frq > 1.0e-10)
			hi_frq = 1.0 / hi_frq;
		if  (lo_frq < hi_frq)
			cu_set_string( w[k_widget_filter_butpar_lo_text], str );
		break;
	case CLC_BUTPAR_HI_UP:
	case CLC_BUTPAR_HI_DOWN:
		hi_frq = (in_hz) ? number : 1.0 / number;
		val = cu_get_string( w[k_widget_filter_butpar_lo_text] );
		if  (*val == '\0')  {
			cu_set_string( w[k_widget_filter_butpar_hi_text], str );
			break;
		} /*endif*/
		slen = strlen( val );
		sscanf( val, "%f", &lo_frq );
		if  (val[slen-1] == 's' && lo_frq > 1.0e-10)
			lo_frq = 1.0 / lo_frq;
		if  (lo_frq < hi_frq)
			cu_set_string( w[k_widget_filter_butpar_hi_text], str );
		break;
	} /*endswitch*/

} /* end of cl_manage_filter_bounds */



/*---------------------------------------------------------------------------*/



/* maximum number of different sample rates on display */
#define MAX_DIFF_DT 10



void cl_create_filter( Widget w[], int mode, TSyStatus *status )

/* creates filter file and puts name into filter text widget
 *
 * parameters of routine
 * Widget     w[];        input; widget ID
 * int        mode;       input; kind of filter
 * TSyStatus  *status;    output; return status
 */
{
	/* local variables */
	char     filnam_p[BC_FILELTH+1];   /* filter name with sample prefix */
	char     filnam[BC_FILELTH+1];     /* filter name without sample prefix */
	char     *val_lo, *val_hi, *val_o; /* string values from dialog box */
	int      pos=1;                    /* position of filter */
	TSyStatus locstat;                 /* local status */
	float    frq_lo, frq_hi;           /* frequency window */
	char     cmdline[BC_LONGSTRLTH+1]; /* command line */
	int      slen;                     /* string length */
	int      order;                    /* order of filter */
	float    dt;                       /* sample distance */
	char     smprate[BC_SHORTSTRLTH+1];/* sample rate string */
	char     *flist[1];                /* filter list */
	float    dt_hist[MAX_DIFF_DT];     /* history list of dt's */
	int      dt_hist_lth;              /* length of history list */
	void     *trc;                     /* trace pointer */
	int      i;                        /* counter */
	TSyBoolean dt_done;                /* dt already processed */

	/* executable code */

	/* get sample rate */
	if  (mg_dsptrcs() == 0)  {
		*status = CLE_NOTRACES;
		return;
	} /*endif*/

	/* loop all traces on display */
	dt_hist_lth = 0;
	trc = NULL;
	FOREVER {

		/* get sample rate of next trace */
		locstat = cBcNoError;
		trc = db_getp( trc, EP_DSPN, &locstat );
		if  (trc == NULL)  break;
		dt = db_getr( trc, ER_DELTA, status );
		if  (Severe(status))  return;

		/* check for dt in history list */
		dt_done = FALSE;  /* if dt_hist_lth==0 */
		for  (i=0; i<dt_hist_lth; i++)  {
			dt_done = (dt == dt_hist[i]);
			if  (dt_done)  break;
		} /*endfor*/
		if  (dt_done)  continue;

		/* append new dt to history list */
		if  (dt_hist_lth < MAX_DIFF_DT)
			dt_hist[dt_hist_lth++] = dt;
		/* now process dt */

		/* sprintf( smprate, "%4.1fHz_", 1.0/dt ); */
		cl_filter_prefix( trc, BC_SHORTSTRLTH, smprate );
	
		val_lo = cu_get_string( w[k_widget_filter_butpar_lo_text] );
		val_hi = cu_get_string( w[k_widget_filter_butpar_hi_text] );
		val_o = cu_get_string( w[k_widget_filter_butpar_order_text] );
		if  (strlen(val_lo) > (BC_FILELTH-9)/2)  {
			*status = CLE_STROVFL;
			return;
		} else if  (strlen(val_hi) > (BC_FILELTH-9)/2)  {
			*status = CLE_STROVFL;
			return;
		} else if  (strlen(val_o) > 2)  {
			*status = CLE_STROVFL;
			return;
		} else if  (*val_o == '\0')  {
			*status = CLE_NOVALUE;
			return;
		} /*endif*/
		while  (*val_lo == ' ')  val_lo++;
		while  (*val_hi == ' ')  val_hi++;
		while  (*val_o == ' ')  val_o++;
	
		/* build filter name */
		strcpy( filnam, "SHM_" );
		switch  (mode)  {
		case CLC_CREFIL_BUT_BP:
			if  (*val_lo == '\0')  {*status = CLE_NOVALUE; return;}
			if  (*val_hi == '\0')  {*status = CLE_NOVALUE; return;}
			strcat( filnam, "BP_" );
			strcat( filnam, val_lo );
			strcat( filnam, "_" );
			strcat( filnam, val_hi );
			strcat( filnam, "_" );
			strcat( filnam, val_o );
			break;
		case CLC_CREFIL_BUT_LP:
			if  (*val_lo == '\0')  {*status = CLE_NOVALUE; return;}
			strcat( filnam, "LP_" );
			strcat( filnam, val_lo );
			strcat( filnam, "_" );
			strcat( filnam, val_o );
			break;
		case CLC_CREFIL_BUT_HP:
			if  (*val_hi == '\0')  {*status = CLE_NOVALUE; return;}
			strcat( filnam, "HP_" );
			strcat( filnam, val_hi );
			strcat( filnam, "_" );
			strcat( filnam, val_o );
			break;
		default:
			printf( "*SHM: cl_create_filter: this cannot happen\n" );
			return;
		} /*endswitch*/
		ut_cap( filnam );
		if  (GpGetChar(cGpC_filter_type) == 'R'
			|| GpGetChar(cGpC_filter_type) == 'r')  {
			strcpy( filnam_p, smprate );
			strcat( filnam_p, filnam );
		} else {
			/* filter type t not supported here */
			strcpy( filnam_p, filnam );
		} /*endif*/
	
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: filter file %s\n", filnam_p );
		cu_set_string( w[k_widget_filter_edit_text], filnam ); /* no prefix here*/
	
		locstat = BC_NOERROR;
		flist[0] = filnam_p;
		rf_filter_input( 1, flist, &pos, &locstat );
		if  (locstat == BC_NOERROR)  continue;   /* filter exists, fine */
	
		if  (sscanf(val_o,"%d",&order) != 1)  {
			*status = CLE_READINT;
			return;
		} /*endif*/
	
		switch  (mode)  {
		case CLC_CREFIL_BUT_BP:
			if  (sscanf(val_lo,"%f",&frq_lo) != 1)  {
				*status = CLE_READFLOAT;
				return;
			} /*endif*/
			slen = strlen( val_lo );
			if  (val_lo[slen-1] == 's' && frq_lo > 1.0e-10)
				frq_lo = 1.0 / frq_lo;
			if  (sscanf(val_hi,"%f",&frq_hi) != 1)  {
				*status = CLE_READFLOAT;
				return;
			} /*endif*/
			slen = strlen( val_hi );
			if  (val_hi[slen-1] == 's' && frq_hi > 1.0e-10)
				frq_hi = 1.0 / frq_hi;
			if  (GpGetChar(cGpC_filter_type) == 'R'
				|| GpGetChar(cGpC_filter_type) == 'r')  {
				sprintf( cmdline, "%sbutrec %f bp f %f %f -o=%d -f=%s",
					GpGetString(cGpS_defpath_extprog), dt, frq_lo, frq_hi, order,
					filnam_p );
			} else {
				/* filter type t not supported here */
				sprintf( cmdline, "%sbutfreq bp f %f %f -o=%d -f=%s",
					GpGetString(cGpS_defpath_extprog), frq_lo, frq_hi, order,
					filnam_p );
			} /*endif*/
			break;
		case CLC_CREFIL_BUT_LP:
			if  (sscanf(val_lo,"%f",&frq_lo) != 1)  {
				*status = CLE_READFLOAT;
				return;
			} /*endif*/
			slen = strlen( val_lo );
			if  (val_lo[slen-1] == 's' && frq_lo > 1.0e-10)
				frq_lo = 1.0 / frq_lo;
			if  (GpGetChar(cGpC_filter_type) == 'R'
				|| GpGetChar(cGpC_filter_type) == 'r')  {
				sprintf( cmdline, "%sbutrec %f lp f %f -o=%d -f=%s",
					GpGetString(cGpS_defpath_extprog), dt, frq_lo, order, filnam_p );
			} else {
				/* filter type t not supported here */
				sprintf( cmdline, "%sbutfreq lp f %f -o=%d -f=%s",
					GpGetString(cGpS_defpath_extprog), frq_lo, order, filnam_p );
			} /*endif*/
			break;
		case CLC_CREFIL_BUT_HP:
			if  (sscanf(val_hi,"%f",&frq_hi) != 1)  {
				*status = CLE_READFLOAT;
				return;
			} /*endif*/
			slen = strlen( val_hi );
			if  (val_hi[slen-1] == 's' && frq_hi > 1.0e-10)
				frq_hi = 1.0 / frq_hi;
			if  (GpGetChar(cGpC_filter_type) == 'R'
				|| GpGetChar(cGpC_filter_type) == 'r')  {
				sprintf( cmdline, "%sbutrec %f hp f %f -o=%d -f=%s",
					GpGetString(cGpS_defpath_extprog), dt, frq_hi, order, filnam_p );
			} else {
				sprintf( cmdline, "%sbutfreq hp f %f -o=%d -f=%s",
					GpGetString(cGpS_defpath_extprog), frq_hi, order, filnam_p );
			} /*endif*/
			break;
		} /*endswitch*/

		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "shm-dbg2: execute command: %s\n", cmdline );
		system( cmdline );

	} /*endfor*/

} /* end of cl_create_filter */



/*---------------------------------------------------------------------------*/



void cl_calc_align_times( char phasename[], int maxlth, char trcstr[],
	TSyStatus *status )

/* computes shift times for traces to align given phase "phasename".  Results
 * are put in info entry OPINFO
 *
 * parameters of routine
 * char       phasename[];     input; name of phase to be aligned
 * int        maxlth;          input; maximum length of trcstr string
 * char       trcstr[];        output; trace list string (where phase is found)
 * TSyStatus  *status;         output; return status
 */
{
	/* local variables */
	int      t;                         /* trace number */
	int      trcno;                     /* number of traces on display */
	TSyStatus locstat;                  /* local status */
	TPmTrace *trc;                      /* trace pointer */
	TPiPhase *phase;                    /* phase info */
	char     station[BC_SHORTSTRLTH+1]; /* station name */
	char     reftime[BC_TIMELTH+1];     /* reference time */
	char     str[BC_TIMELTH+1];         /* scratch string */
	float    tmp;                       /* scratch */
	int      slen;                      /* string length */

	/* executable code */

	trcno = mg_dsptrcs();
	if  (trcno == 0)  return;

	if  (GpGetInt(cGpI_debug_level) > 1)
		if  (strcmp(GpGetString(cGpS_refstation),"CENTRE") == 0
			|| strcmp(GpGetString(cGpS_refstation),"CENTER") == 0)
			printf( "SHM-dbg2: align: reference station is first trace\n" );

	/* put absolute onset time to OPINFO entry */
	*reftime = '\0';
	t = 0;
	trc = NULL;
	FOREVER  {
		locstat = cBcNoError;
		trc = db_getp( trc, EP_DSPN, &locstat );
		if  (trc == NULL)  break;
		phase = PmFindPhase( trc, phasename );
		if  (phase == NULL)  {
			db_sets( trc, ES_OPINFO, "0.0", status );
			if  (Severe(status))  return;
			continue;
		} /*endif*/
		if  (*reftime == '\0')  strcpy( reftime, phase->onset );
		locstat = BC_NOERROR;
		db_gets( trc, ES_STATION, BC_SHORTSTRLTH, station, &locstat );
		if  (Severe(&locstat))  *station = '\0';
		if  (strcmp(station,GpGetString(cGpS_refstation)) == 0)
			strcpy( reftime, phase->onset );
		db_sets( trc, ES_OPINFO, phase->onset, status );
		if  (Severe(status))  return;
	} /*endfor*/

	if  (*reftime == '\0')  {
		printf( "*SHM: cl_calc_align_times: this cannot happen\n" );
		return;
	} /*endif*/

	/* now compute shift times and write trcstr */
	*trcstr = '\0';
	trc = NULL;
	FOREVER  {
		locstat = cBcNoError;
		trc = db_getp( trc, EP_DSPN, &locstat );
		if  (trc == NULL)  break;
		db_gets( trc, ES_OPINFO, BC_TIMELTH, str, status );
		if  (Severe(status))  return;
		if  (strcmp(str,"0.0") == 0)  continue;
		tmp = tc_tdiff( str, reftime, status );
		if  (Severe(status))  return;
		sprintf( str, "%e", tmp );
		db_sets( trc, ES_OPINFO, str, status );
		if  (Severe(status))  return;
		slen = strlen( trcstr );
		if  (slen >= maxlth-4)  {      /* more than 1000 traces */
			*status = CLE_STROVFL;      /* are not likely        */
			return;
		} /*endif*/
		sprintf( trcstr+slen, "%d", ++t );
		strcat( trcstr, "," );
	} /*endfor*/

	slen = strlen( trcstr ) - 1;
	if  (trcstr[slen] == ',')  trcstr[slen] = '\0';

} /* end of  cl_calc_align_times */



/*---------------------------------------------------------------------------*/



void cl_filter_prefix( TPmTrace *trc, int maxlth, char prefix[] )

/* generates prefix for filter filename from sample distance delta of given
 * trace on display.
 *
 * parameters of routine
 * void       *trc;         input; pointer to trace or NULL for first trace
 * int        maxlth;       input; maximum length of output string
 * char       prefix[];     output; created file prefix
 */
{
	/* local variables */
	float      delta;      /* sample distance in sec */
	float      frq;        /* sample frequency */
	TSyStatus  locstat;    /* local status */

	/* executable code */

	if  (maxlth < 7)  {
		printf( "*SHM: filter name prefix overflow\n" );
		*prefix = '\0';
		return;
	} /*endif*/

	/* get delta t of trace */
	if  (trc == NULL)
		trc = db_getp( NULL, EP_DSPN, NULL );
	if  (trc == NULL)  {
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: cl_filter_prefix: no traces\n" );
		*prefix = '\0';
		return;
	} /*endif*/
	locstat = BC_NOERROR;
	delta = db_getr( trc, ER_DELTA, &locstat );
	if  (Severe(&locstat))  delta = 0.0;

	/* check delta and generate prefix */
	if  (Abs(delta) < 1.0e-6)  {
		strcpy( prefix, "INFHZ_" );
		return;
	} /*endif*/

	frq = 1.0 / delta;
	if  (frq < 1.0)  {
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: low frequent sampling\n" );
		strcpy( prefix, "LOHZ_" );
	} else if  (frq > 100.0)  {
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: high frequent sampling\n" );
		strcpy( prefix, "HIHZ_" );
	} /*endif*/

	sprintf( prefix, "%d", Nint(frq) );
	strcat( prefix, "HZ_" );

} /* end of cl_filter_prefix */



/*---------------------------------------------------------------------------*/



char *cl_unique_name( void )

/* Returns pointer to unique name each time it is called.  Produced filename
 * has no extension.
 */
{
	/* local variables */
	static char      u_name[BC_FILELTH+1];    /* unique file name */
	static int       fcnt=0;                  /* file counter */

	/* executable code */

	if  (++fcnt > 999)  fcnt = 1;

	strcpy( u_name, id_shv );
	sprintf( u_name+strlen(u_name), "%03d", fcnt );

	return u_name;

} /* end of cl_unique_name */



/*---------------------------------------------------------------------------*/



TSyBoolean cl_is_solaris_2( void )

/* Returns TRUE if OS is Solaris 2
 *
 * no parameters
 */
{
	/* local variables */
	char     fname[cBcFileLth+1];   /* name of output file */
	char     cmd[cBcLongStrLth+1];  /* shell command */
	char     *env;                  /* pointer to environment variable */
	FILE     *fp;                   /* pointer to file */

	/* executable code */

	/* get name of OS file */
	strcpy( fname, shd_scratch );
	strcat( fname, CLC_OS_RELEASE_FILE );

	/* create new file */
	sprintf( cmd, "\\rm %s\n", fname );
	system( cmd );
	sprintf( cmd, "uname -r >%s\n", fname );
	system( cmd );

	/* read file */
	fp = sy_fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "SHM: error opening file %s\n", fname );
		return FALSE;
	} /*endif*/
	fgets( cmd, cBcLongStrLth, fp );
	sy_fclose( fp );

	/* find version */
	return (*cmd >= '5');

} /* end of cl_is_solaris_2 */



/*----------------------------------------------------------------------------*/



void cl_set_simfilter_active( Widget w[], TSyBoolean onoff )

/* Sets sensitivity of simulation filter buttons on or off
 *
 * parameters of routine
 * Widget        w[];         input; widget array
 * TSyBoolean    onoff;       input; TRUE=activate, FALSE=deactivate
 */
{
	/* executable code */

	XtSetSensitive( w[k_widget_filter_wwssn_sp],    onoff );
	XtSetSensitive( w[k_widget_filter_wwssn_lp],    onoff );
	XtSetSensitive( w[k_widget_filter_lrsm_sp],     onoff );
	XtSetSensitive( w[k_widget_filter_kirnos],      onoff );
	XtSetSensitive( w[k_widget_filter_woodand],     onoff );
	XtSetSensitive( w[k_widget_filter_standard_bp], onoff );
	XtSetSensitive( w[k_widget_filter_sro_lp], onoff );
	XtSetSensitive( w[k_widget_filter_displace],    onoff );

} /* end of cl_set_simfilter_active */



/*---------------------------------------------------------------------------*/



void cl_read_autofilters( void )

/* Reads autofilter entries from parameter file
 *
 * no parameters
 */
{
	/* local variables */
	char     *env;                   /* pointer to environment */
	char     parfile[cBcFileLth+1];  /* name of parameter file */
	FILE     *fp;                    /* pointer to parameter file */
	char     line[cBcLineLth+1];     /* current line of file */
	char     stream[cBcLineLth+1];   /* stream name */
	char     afilter[cBcLineLth+1];  /* autofilter */
	CLT_AUTOFILTER *af;              /* pointer to autofilter */

	/* executable code */

	/* get parameter file */
	env = (char *)getenv( "SH_USER_PARAM" );
	if  (env == NULL)  {
		env = (char *)getenv( "SH_INPUTS" );
		if  (strlen(env) < cBcFileLth-15)  {
			strcpy( parfile, env );
			strcat( parfile, "/shm-config.txt" );
		} else {
			fprintf( stderr,
				"Configuration filename too long.  Autofilters disabled.\n" );
			return;
		} /*endif*/
	} else {
		if  (strlen(env) < cBcFileLth)  {
			strcpy( parfile, env );
		} else {
			fprintf( stderr,
				"Environment SH_USER_PARAM too long.  Autofilters disabled.\n" );
			return;
		} /*endif*/
	} /*endif*/

	fp = fopen( parfile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "cannot open parameter file, Autofilters disabled\n" );
		return;
	} /*endif*/

	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		if  (strncmp(line,"v$autofilter",12) != 0)  continue;
		if  (sscanf(line+12,"%s %s",stream,afilter) != 2)  {
			fprintf( stderr, "Error reading line %s.  Ignore\n", line );
			continue;
		} /*endif*/
		if  (clv_afilter_root == NULL)  {
			clv_afilter_root =
				(CLT_AUTOFILTER *)malloc( (int)sizeof(CLT_AUTOFILTER) );
			if  (clv_afilter_root == NULL)  {
				fprintf( stderr, "Error allocating memory.  Abort\n" );
				exit( 1 );
			} /*endif*/
			af = clv_afilter_root;
		} else {
			af->next = 
				(CLT_AUTOFILTER *)malloc( (int)sizeof(CLT_AUTOFILTER) );
			if  (af->next == NULL)  {
				fprintf( stderr, "Error allocating memory.  Abort\n" );
				exit( 1 );
			} /*endif*/
			af = af->next;
		} /*endif*/
		af->next = NULL;
		af->sname = (char *)malloc( (strlen(stream)+1)*(int)sizeof(char) );
		if  (af->sname == NULL)  {
			fprintf( stderr, "autofilter: mem alloc error.  Abort.\n" );
			exit( 1 );
		} /*endif*/
		strcpy( af->sname, stream );
		af->filter = (char *)malloc( (strlen(afilter)+1)*(int)sizeof(char) );
		if  (af->filter == NULL)  {
			fprintf( stderr, "autofilter: mem alloc error.  Abort.\n" );
			exit( 1 );
		} /*endif*/
		strcpy( af->filter, afilter );
	} /*endwhile*/

	fclose( fp );

	if  (GpGetInt(cGpI_debug_level) > 3)  {
		af = clv_afilter_root;
		while  (af != NULL)  {
			printf( "SHM-dbg4: autofilter (%s,%s)\n", af->sname, af->filter );
			af = af->next;
		} /*endwhile*/
	} /*endif*/

} /* end of cl_read_autofilters */



/*---------------------------------------------------------------------------*/



void cl_set_autofilters( void )

/* sets autofilter names to filter entry of traces
 *
 * no parameters
 */
{
	/* local variables */
	void     *trc;                   /* trace pointer */
	TSyStatus locstat;               /* local status */
	unsigned filent;                 /* filter info entry */
	char     stream[cBcLineLth+1];   /* stream name */
	int      i;                      /* counter */
	CLT_AUTOFILTER *af;              /* pointer to autofilter */
	float    dt;                     /* delta t */
	char     tmpstr[cBcLineLth+1];   /* temp string */

	/* executable code */

	if  (clv_afilter_root == NULL)  return;

	/* get index number of filter entry */
	locstat = cBcNoError;
	db_ident( "FILTER", &filent, &locstat );
	if  (SySevere(&locstat))  return;

	/* loop all traces */
	trc = NULL;
	FOREVER  {
		locstat = cBcNoError;
		trc = db_getp( trc, EP_NEXT, NULL );
		if  (trc == NULL)  break;
		/* get stream string */
		db_gets( trc, ES_STATION, cBcLineLth, stream, &locstat );
		if  (SySevere(&locstat))  continue;
		strcat( stream, "-" );
		i = strlen( stream );
		stream[i++] = db_getc( trc, EC_CHAN1, &locstat );
		if  (SySevere(&locstat))  continue;
		stream[i++] = db_getc( trc, EC_CHAN2, &locstat );
		if  (SySevere(&locstat))  continue;
		stream[i++] = '-';
		stream[i++] = db_getc( trc, EC_COMP, &locstat );
		if  (SySevere(&locstat))  continue;
		stream[i] = '\0';
		ut_uncap( stream );
		af = clv_afilter_root;
		while  (af != NULL)  {
			if  (strcmp(stream,af->sname) == 0)  {
				if  (GpGetChar(cGpC_filter_type) == 'R')  {
					dt = db_getf( trc, ER_DELTA, NULL );
					if  (dt > 0.0)  {
						sprintf( tmpstr, "%dHZ_", Nint(1.0/dt) );
						strcat( tmpstr, af->filter );
						db_sets( trc, filent, tmpstr, &locstat );
					} /*endif*/
				} else {
					db_sets( trc, filent, af->filter, &locstat );
				} /*endif*/
				break;
			} /*endif*/
			af = af->next;
		} /*endwhile*/
	} /*endfor*/

} /* end of cl_set_autofilters */



/*---------------------------------------------------------------------------*/

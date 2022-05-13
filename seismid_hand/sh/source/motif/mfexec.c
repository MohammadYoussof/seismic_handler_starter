
/* file mfexec.c
 *      ========
 *
 * version 63, 21-Nov-2007
 *
 * execution of SH commands
 * K. Stammler, 25-Feb-93
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
#undef BC_DEFINE_TRUE_FALSE
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include <Xm/Xm.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include BC_SYSBASE
#include "erusrdef.h"
#include "motifgraph.h"
#include "station_no.h"
#include "cbutil.h"
#include "mfexec.h"
#include "cblib.h"
#include "cblib3.h"
#include "callsh.h"
#include "infoidx.h"
#include "utusrdef.h"
#include "shvars.h"
#include "glusrdef.h"
#include "phaseinf.h"
#include "phasemgr.h"
#include "trcselect.h"
#include "shm_widgets.h"
#include "sysext.h"
#include "seismics.h"
#include "globalparams.h"


/*
 * the following definitions must match the 'entry define shm_...' commands
 * in SHM_CMD_STARTUP.SHC
 */
#define MXC_NUM_VIEW 0
#define MXC_NUM_HIDE 1
#define MXC_NUM_FILTER 2
#define MXC_NUM_ROT 3
#define MXC_EF_VIEW   (MXC_NUM_VIEW|EF_TYPE)
#define MXC_EF_HIDE   (MXC_NUM_HIDE|EF_TYPE)
#define MXC_EF_FILTER (MXC_NUM_FILTER|EF_TYPE)
#define MXC_EF_ROT    (MXC_NUM_ROT|EF_TYPE)
#define MXC_FLAG_VIEW   (1<<MXC_NUM_VIEW)
#define MXC_FLAG_HIDE   (1<<MXC_NUM_HIDE)
#define MXC_FLAG_FILTER (1<<MXC_NUM_FILTER)
#define MXC_FLAG_ROT    (1<<MXC_NUM_ROT)


/* imported variables */
extern Widget       xmv_w[];         /* widget array */
extern MX_CMD_READG xmv_cmd_readg;   /* to write on it in external_routine */


static int mxv_display_state=0;   /* display status */



/* prototypes of local routines */
static void mxh_read_file_list( Widget w, MGT_DSPCTRL *ctrl, char filenames[],
	int format, STATUS *status );
static void mx_prepare_display( void );
static void mxh_filter_separately( Widget w, MGT_DSPCTRL *ctrl, char filter[],
	float autocut, STATUS *status );
static BOOLEAN mxh_rotation_is_possible( void );
static void mxh_get_vax_grflist( MX_CMD_READG *rg, char liststr[] );
static void mxh_read_statlist( char fname[], Widget w[], MX_STATLIST *sl,
	STATUS *status );
static void mxh_mount_cdrom( MX_CMD_READG *par, STATUS *status );
static void mxh_set_entry_name( Widget ew, char text[] );
void mx_remove_double_elements( char *str );
void mxh_compute_azimuth( char cstation[], float lat, float lon,
	float *azim, TSyStatus *status );
static void mxh_read_stations_from_evt( char autoevt[], char addlist[] );



/*--------------------------------------------------------------------------*/



void mx_readg( MX_CMD_READG *par, Widget wm, Widget ws, MGT_DSPCTRL *ctrl,
	BOOLEAN keep_phases, char autoevt[], STATUS *status )

/* executes readg command
 *
 * parameters of routine
 * XM_CMD_READG  *par;        input; command parameters
 * Widget        wm;          input; main DrawingArea Widget
 * Widget        ws;          input; single trace window
 * MGT_DSPCTRL   *ctrl;       input; display control parameters
 * TSyBoolean    keep_phases; input; keep phases in memory
 * char          *autoevt     input; name of evt file for retrieving stat. names
 * TSyStatus     *status;     output; return status
 */
{
	/* local variables */
	static int gse_cnt=0;               /* GSE counter */
	char     line[cBcVeryLongStrLth+1]; /* command line */
	char     statlist[cBcVeryLongStrLth+1]; /* station list */
	char     varname[MXC_STATSTR_LTH+2]; /* variable name */
	char     addlist[cBcLongStrLth+1];  /* parsed station string */
	int      i;                         /* bit count */
	char     *eptr;                     /* pointer to char for getenv */
	TSyBoolean redraw;                  /* dummy */
	TSyBoolean ok;                      /* return code */
	TSyStatus  locstat;                 /* local status */
	char     gsename[cBcFileLth+1];     /* name of GSE file */

	/* executable code */

	if  (par->channum == 0 && par->format == MXC_FORMAT_SEED)  {
		*status = MXE_NOCHANNELS;
		return;
	} /*endif*/

	mg_do_drag( wm, MGC_DRAG_CLEAR, 0, 0 );
	mg_plot_drag_window( ws, ctrl->show_phase_acc, status );
	if  (Severe(status))  return;

	/* translate device name if possible */
	if  (strcmp(par->device,"CD") == 0)  {
		mxh_mount_cdrom( par, status );
		if  (Severe(status))  return;
	} else {
		eptr = getenv( par->device );
		if  (eptr != NULL)  {
			if  (strlen(eptr) >= BC_FILELTH)  {
				*status = MXE_STROVFL;
				return;
			} /*endif*/
			strcpy( par->device, eptr );
			strcat( par->device, "/" );
		} /*endif*/
	} /*endif*/

	if  (par->keep)  {
		mx_exec_sh( wm, ctrl, "nr" );
		mx_exec_sh( wm, ctrl, "display all_dh" );
		mx_exec_sh( wm, ctrl, "del _shm_filter(y)" );
		mx_exec_sh( wm, ctrl, "del _shm_rot(y)" );
		mx_exec_sh( wm, ctrl, "rd" );
	} else {
		mx_exec_sh( wm, ctrl, "del all_dh" );
	} /*endif*/
	if  (Severe(status))  return;
	mx_exec_sh( wm, ctrl, "dtw" );
	/* mx_exec_sh( wm, ctrl, "rd" ); */
	/* mx_exec_sh( wm, ctrl, "nr" ); */

	/* create station list and component list */
	*statlist = '\0';
	for  (i=0; i<MXC_STATLIST_LTH; i++)  {
		if  ((1<<i) & par->stations)  {
			if  (*statlist != '\0')  strcat( statlist, "," );
			if  (par->sl.code[i][0] == '$')  {
				varname[0] = 'v';
				strcpy( varname+1, par->sl.code[i] );
				GpReadParameter( varname, cBcLongStrLth, addlist, &ok );
				if  (!ok)  {
					*status = MXE_ILL_SUBSET;
					err_setcontext( " ## variable: " );
					err_setcontext( varname );
					return;
				} /*endif*/
				if  (strlen(statlist)+strlen(addlist) < cBcVeryLongStrLth)  {
					strcat( statlist, addlist );
				} else {
					*status = MXE_STROVFL;
					return;
				} /*endif*/
			} else if  (strcmp(par->sl.code[i],":AE") == 0)  {
				if  (autoevt == NULL)  {
					printf( "*SHM: cannot translate :AE, autoevt is NULL\n" );
				} else if  (*autoevt == '\0')  {
					printf( "*SHM: cannot translate :AE, autoevt is empty\n" );
				} else {
					printf( "get statlist\n" );
					mxh_read_stations_from_evt( autoevt, addlist );
					if  (strlen(statlist)+strlen(addlist) < cBcVeryLongStrLth)  {
						strcat( statlist, addlist );
					} else {
						*status = MXE_STROVFL;
						return;
					} /*endif*/
					printf( "statlist: %s\n", statlist );
				} /*endif*/
			} else {
				if  (strlen(statlist)+strlen(par->sl.code[i]) < cBcVeryLongStrLth) {
					strcat( statlist, par->sl.code[i] );
				} else {
					*status = MXE_STROVFL;
					return;
				} /*endif*/
			} /*endif*/
		} /*endif*/
	} /*endfor*/
	if  (strlen(statlist) > cBcVeryLongStrLth-55)  {
		printf( "*SHM: mx_readg: station list too long\n" );
		printf( "                field length exceeded, abort program\n" );
		exit( 1 );
	} /*endif*/
	if  (*statlist != '\0')
		mx_remove_double_elements( statlist );

	switch  (par->format)  {
	case MXC_FORMAT_SEED:
		if  (par->use_readk)  {

			/* read from VAX WORMS: (completely outdated code, no more k-files) */
			/* check whether GRF or GRSN data are requested */
			if  (strncmp(statlist,"GRA",3) == 0
				|| strncmp(statlist,"GRB",3) == 0
				|| strncmp(statlist,"GRC",3) == 0)  {
				/* these are GRF data ... */
				;
			} else {
				/* these are GRSN data ... */
				if  (par->channum > 1)
					printf( "*SHM: take only first channel selected\n" );
				/* generate filename */
				sprintf( gsename, "grn2gse_%02d.gse", ++gse_cnt );
				strcpy( line, GpGetString(cGpS_defpath_extprog) );
				sprintf( line+strlen(line),
					"grn2gse %s %s %f %s %s %s %s",
					par->device, par->start, par->seclth, statlist,
					par->comp, par->chanstr[0], gsename );
			} /*endif*/
			if  (GpGetInt(cGpI_debug_level) > 1)
				printf( "SHM-dbg2: exec: %s\n", line );
			system( line );
			sprintf( line, "@READGSE %s ALL", gsename );
			if  (GpGetInt(cGpI_debug_level) > 1)
				printf( "SHM-dbg2: sh exec: %s\n", line );
			mx_exec_sh( wm, ctrl, line );
			/* now delete input GSE file */
			/* sy_fdelete( gsename ); */

		} else {

			/* read from Mini-SEED files */
			/* to be added: check length of line */

			/* make backslashes out of slashes */
			eptr = par->device - 1;
			while  (*(++eptr) != '\0')
				if  (*eptr == '/')  *eptr = '\\';
			/* loop all channels */
			for  (i=0; i<(par->channum); i++)  {
#ifdef SH_SETUP_LINUX
				if  (par->reads_invhdr)  {
					sprintf( line, "@READS/NOSWAP/INVHDR/SFD=%s %s %s %f %s %s %s",
						par->sfdfile, par->device, par->start, par->seclth,
						statlist, par->comp, par->chanstr[i] );
				} else {
					sprintf( line, "@READS/NOSWAP/SFD=%s %s %s %f %s %s %s",
						par->sfdfile, par->device, par->start, par->seclth,
						statlist, par->comp, par->chanstr[i] );
				} /*endif*/
#else
				if  (par->reads_invhdr)  {
					sprintf( line, "@READS/INVHDR/SFD=%s %s %s %f %s %s %s",
						par->sfdfile, par->device, par->start, par->seclth,
						statlist, par->comp, par->chanstr[i] );
				} else {
					sprintf( line, "@READS/SFD=%s %s %s %f %s %s %s",
						par->sfdfile, par->device, par->start, par->seclth,
						statlist, par->comp, par->chanstr[i] );
				} /*endif*/
#endif
				if  (GpGetInt(cGpI_debug_level) > 1)
					printf( "SHM-dbg2: execute: >%s< length %d\n", line, strlen(line) );
				mx_exec_sh( wm, ctrl, line );
			} /*endfor*/

		} /*endif*/
		break;

	case MXC_FORMAT_GSE:
	case MXC_FORMAT_GSE2:
	case MXC_FORMAT_AH:
	case MXC_FORMAT_Q:
		mxh_read_file_list( wm, ctrl, par->filename, par->format, status );
		if  (Severe(status))  return;
		break;

	default:

		printf( "*SHM: mx_readg: program error\n" );
		*status = MXE_PROG_BUG;
		return;

	} /*endswitch*/

	cl_set_autofilters();

	mx_exec_sh( wm, ctrl, "SHM_CMD_TIMEAL" );

	if  (keep_phases)  {
		mg_plot_phases( wm, ctrl->show_phase_acc, status );
	} /*endif*/
	cl3_uppercase_names();

} /* end of mx_readg */



/*--------------------------------------------------------------------------*/



void mx_filter( MX_CMD_FILTER *filter, Widget w, MGT_DSPCTRL *ctrl,
	STATUS *status )

/* performs filtering
 *
 * parameters of routine
 * MX_CMD_FILTER *filter;       input; filter command parameters
 * Widget        w;             input; DrawingArea Widget
 * MGT_DSPCTRL   *ctrl;         input; display control parameters
 * TSyStatus     *status;       output; return status
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];            /* scratch string */
	char     filprefix[BC_SHORTSTRLTH+1];  /* filter prefix */
	char     filter_typ;                   /* recursive or FFT, 'R' or 'F' */
	TSyBoolean same_filter;                /* same filter for all traces */
	unsigned filent;                       /* filter info entry */
	int      trcnum;                       /* trace number */
	void     *trc;                         /* trace pointer */

	/* executable code */

	/* if no traces on display return error status */
	if  (mg_dsptrcs() == 0)  {
		*status = MXE_NO_TRACES;
		return;
	} /*endif*/

	/* get filter type */
#ifdef XXX
	filter_typ = (shv_global.use_rec_filters) ? 'R' : 'F';
	if  (shv_global.filter_type != ' ')  filter_typ = shv_global.filter_type;
#endif
	filter_typ = GpGetChar( cGpC_filter_type );

	/* if filtering is switched off it's quick and easy */
	if  (filter->name[0] == '\0')  {
		/* delete filter entry on selected traces */
		if  (ts_some_trace_selected())  {
			/* get index number of filter entry */
			db_ident( "FILTER", &filent, status );
			if  (SySevere(status))  return;
			/* loop all traces and delete filter entry */
			trc = NULL;
			trcnum = 0;
			FOREVER  {
				trc = db_getp( trc, EP_NEXT, NULL );
				if  (trc == NULL)  break;
				trcnum++;
				if  (ts_is_selected(trcnum))
					db_sets( trc, filent, "", status );
			} /*endfor*/
			ts_clear_selections();
		} /*endif*/
		/* remove the filter */
		mxv_display_state &= ~MXC_FLAG_FILTER;
		sprintf( str, "shm_cmd_filter %c ;; NONE %f %d",
			filter_typ, filter->autocut,
			(GpGetInt(cGpI_trace_normalisation)==cGp_NORM_CONST) );
		mx_exec_sh( w, ctrl, str );
		return;
	} /*endif*/

	/* filtering is switched on */
	mxv_display_state |= MXC_FLAG_FILTER;

	/* now let's check which filter(s) must be used */
	cl3_check_filter( filter->name, (filter_typ == 'R'),
		filprefix, &same_filter, status );
	if  (Severe(status))  return;

	/* if all traces need the same filter do it with the old SH-cp and quit */
	if  (same_filter && !ts_some_trace_selected())  {
		if  (filter_typ == 'R')  {
			sprintf( str, "shm_cmd_filter r;%s;%s %f %d",
				filprefix, filter->name, filter->autocut,
				(GpGetInt(cGpI_trace_normalisation)==cGp_NORM_CONST) );
		} else {
			sprintf( str, "shm_cmd_filter %c;%s;%s %f %d",
				filter_typ, filprefix, filter->name, filter->autocut,
				(GpGetInt(cGpI_trace_normalisation)==cGp_NORM_CONST) );
		} /*endif*/
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: execute >%s<\n", str );
		mx_exec_sh( w, ctrl, str );
		return;
	} /*endif*/

	/* now this is the hard way */
	mxh_filter_separately( w, ctrl, filter->name, filter->autocut, status );

} /* end of mx_filter */



/*--------------------------------------------------------------------------*/



void mx_exec_sh( Widget w, MGT_DSPCTRL *ctrl, char line[] )

/* executes command line
 *
 * parameters of routine
 * Widget        w;          input; widget of drawing area
 * MGT_DSPCTRL   *ctrl;      input; display control parameters
 * char          line[];     input; command line
 */
{
	/* local variables */
	TSyStatus   status;             /* return status */
	TSyBoolean  redraw;             /* redraw flag */

	/* executable code */

	status = BC_NOERROR;
	callsh( line, &redraw, &status );
	if  (Severe(&status))  {
		cu_alert( status );
	} else {
		if  (redraw)
			mg_tracedisplay( w, ctrl, &status );
		if  (Severe(&status))  {
			cu_alert( status );
		} /*endif*/
	} /*endif*/

} /* end of mx_exec_sh */



/*--------------------------------------------------------------------------*/



void mx_stw( Widget wm, Widget ws, MGT_DSPCTRL *ctrl )

/* executes stw command
 *
 * parameters of routine
 * Widget        wm;        input; widget ID of main DrawingArea
 * Widget        ws;        input; widget ID of single trace DrawingArea
 * MGT_DSPCTRL   *ctrl;     input; display control parameters
 */
{
	/* local variables */
	char        cmd[BC_LINELTH+1];    /* command line */
	float       start, end;           /* time window */
	TSyBoolean  set;                  /* time window set */
	TSyStatus   locstat=BC_NOERROR;   /* local status */

	/* executable code */

	mg_get_drag_window( &start, &end, &set );
	if  (!set)  return;
	sprintf( cmd, "stw %e %e", start, end );
	mx_exec_sh( wm, ctrl, cmd );
	mg_do_drag( wm, MGC_DRAG_CLEAR, 0, 0 );
	mg_plot_drag_window( ws, ctrl->show_phase_acc, &locstat );

} /* end of mx_stw */


 
/*---------------------------------------------------------------------------*/



void mx_handle_beam( Widget wm, Widget ws, BOOLEAN do_beam, MGT_DSPCTRL *ctrl,
	CUT_PARAMS *par, char alignphase[], STATUS *status )

/* creates or deletes beam trace (toggle routine)
 *
 * parameters of routine
 * Widget     wm;            input; Widget ID of main window
 * Widget     ws;            input; Widget ID of single trace window
 * TSyBoolean do_beam;       input; do beam (not align)
 * MGT_DSPCTRL *ctrl;        input; display control
 * CUT_PARAMS *par;          input; analysis parameters
 * char       alignphase[];  input; name of phase to be aligned
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	int        t;                    /* trace counter */
	TSyStatus  locstat;              /* local status */
	char       str[BC_LINELTH+1];    /* scratch */
	char       trcstr[BC_LINELTH+1]; /* trace list string */
	TSyBoolean create_beam;          /* create or delete beam */

	/* executable code */

	/* look for BEAM trace */
	create_beam = TRUE;
	for  (t=1; t<=mg_dsptrcs(); t++)  {
		locstat = BC_NOERROR;
		db_gets( mg_trcptr(t), ES_STATION, BC_LINELTH, str, &locstat );
		if  (locstat == BC_NOERROR && strcmp(str,"BEAM") == 0)  {
			create_beam = FALSE;
			break;
		} else if  (locstat == BC_NOERROR && strcmp(str,"ALIGN") == 0)  {
			create_beam = FALSE;
			break;
		} /*endif*/
	} /*endfor*/

	mg_do_drag( wm, MGC_DRAG_CLEAR, 0, 0 );
	locstat = BC_NOERROR;
	mg_plot_drag_window( ws, ctrl->show_phase_acc, &locstat );
	if  (create_beam)  {
		if  (do_beam)  {
			sprintf( str, "shm_cmd_make_beam %e %e %s",
				par->b_azimuth, par->b_slowness, GpGetString(cGpS_refstation) );
			if  (cu_get_exec_flag(CUC_F_EXEC_RESIDCORR))  strcat( str, " P" );
			mx_exec_sh( wm, ctrl, str );
		} else {
			cl_calc_align_times( alignphase, BC_LINELTH-19, trcstr, status );
			/* this is the textlength in the sprintf cmd ^ */
			if  (Severe(status))  return;
			if  (*trcstr == '\0')  {
				*status = MXE_NO_TRACES;
				return;
			} /*endif*/
			sprintf( str, "shm_cmd_make_align %s", trcstr );
			mx_exec_sh( wm, ctrl, str );
		} /*endif*/
	} else {
		mx_exec_sh( wm, ctrl, "shm_cmd_delete_beam" );
	} /*endif*/

} /* end of mx_handle_beam */



/*---------------------------------------------------------------------------*/


#define MAX_NEW_TRC 3
#define NEW_TRC_R 0
#define NEW_TRC_T 1
#define NEW_TRC_Z 2



void mx_rotation( Widget wm, MGT_DSPCTRL *ctrl, float azimuth,
	float lat, float lon, TSyStatus *status )

/* Performs rotation of traces with given azimuth of, if 0.0, computing
 * azimuth by given location
 *
 * parameters of routine
 * Widget     wm;        input; widget ID of main window
 * MGT_DSPCTRL *ctrl;    input; display control
 * float      azimuth;   input; rotation angle
 * float      lat, lon;  input; event location
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	void       *trc;                        /* running index */
	void       *trc_z, *trc_n, *trc_e;      /* trace pointers */
	char       comp;                        /* component */
	char       station[BC_SHORTSTRLTH+1];   /* station name */
	char        cstation[BC_SHORTSTRLTH+1]; /* current station name */
	TSyBoolean  flush;                      /* do rotation */
	TSyStatus   locstat;                    /* local status */
	char        cmd[BC_LINELTH+1];          /* command line for SH */
	void        *newtrc[MAX_NEW_TRC];       /* new created traces */
	int         newtrclth;                  /* number of new traces */
	float       xazim;                      /* local copy of azimuth */
	TSyBoolean  dmy;                        /* dummy */

	/* executable code */

	/* check whether rotated traces are there */
	locstat = BC_NOERROR;
	trc = NULL;
	FOREVER  {
		/* get next trace */
		trc = db_getp( trc, EP_NEXT, NULL );
		if  (trc == NULL)  break;
		if  (db_getf(trc,MXC_EF_ROT,&locstat))  {
			mxv_display_state &= ~MXC_FLAG_ROT;
			mx_exec_sh( wm, ctrl, "shm_cmd_rotate unrotate" );
			return;
		} /*endif*/
	} /*endfor*/
	/* no rotated traces there -> continue and make them */

	/* initialize */
	mxv_display_state |= MXC_FLAG_ROT;
	trc_z = trc_n = trc_e = NULL;
	*station = '\0';
	mx_exec_sh( wm, ctrl, "shm_cmd_rotate begin" );

	/* loop all traces in memory */
	flush = FALSE;
	trc = NULL;
	do  {
		/* get next trace */
		trc = db_getp( trc, EP_NEXT, NULL );
		/* get station and component */
		locstat = BC_NOERROR;
		flush = (trc == NULL);
		if  (!flush)  {
			db_gets( trc, ES_STATION, BC_SHORTSTRLTH, cstation, &locstat );
			if  (Severe(&locstat))  {*cstation = '\0'; locstat = BC_NOERROR;}
			comp = db_getc( trc, EC_COMP, &locstat );
			if  (Severe(&locstat))  {comp = ' '; locstat = BC_NOERROR;}
			flush = (strcmp(station,cstation) != 0);
		} /*endif*/
		if  (flush)  {
			/* rotate if traces are there */
			if  (trc_n != NULL && trc_e != NULL)  {
				/* compute azimuth if equal to 0.0 */
				if  (azimuth == 0.0)  {
					mxh_compute_azimuth( cstation, lat, lon, &xazim, status );
					if  (SySevere(status)) return;
					if  (GpGetInt(cGpI_debug_level) > 3)
						printf( "SHM-dbg5: rotate: azimuth %f for %s\n",
							xazim, cstation );
				} else {
					xazim = azimuth;
					if  (GpGetInt(cGpI_debug_level) > 3)
						printf( "SHM-dbg5: rotate: constant azimuth %f for %s\n",
							xazim, cstation );
				} /*endif*/
				/* generate SH command line and execute it */
				if  (trc_z == NULL)  {
					sprintf( cmd, "shm_cmd_rotate ;; a:%lx a:%lx %g",
						trc_n, trc_e, xazim );
				} else {
					sprintf( cmd, "shm_cmd_rotate a:%lx a:%lx a:%lx %g",
						trc_z, trc_n, trc_e, xazim );
				} /*endif*/
				callsh( cmd, &dmy, status );
				if  (Severe(status))  return;
				/* get trace pointers of new traces */
				db_createdlist( NULL, &newtrclth );
				if  (newtrclth != 2)  {
					printf( "*SHM: mx_rotation: this cannot happen (1)\n" );
					*status = MXE_PROG_BUG;
					return;
				} /*endif*/
				db_createdlist( newtrc, &newtrclth );
				newtrc[NEW_TRC_Z] = (trc_z == NULL)
					? NULL : db_getp(newtrc[NEW_TRC_R],EP_PREV,NULL);
#				ifdef XPHASE
				/* copy phases to output traces */
				if  (trc_z != NULL)
					db_setp( newtrc[NEW_TRC_Z], EP_USRX,
						pi_copy_phaselist(db_getp(trc_z,EP_USRX,NULL),status),status);
				if  (Severe(status))  return;
				db_setp( newtrc[NEW_TRC_R], EP_USR2,
					pi_copy_phaselist(db_getp(trc_n,EP_USRX,NULL),status), status );
				if  (Severe(status))  return;
				db_setp( newtrc[NEW_TRC_T], EP_USR2,
					pi_copy_phaselist(db_getp(trc_e,EP_USRX,NULL),status), status );
				if  (Severe(status))  return;
#				endif
			} /*endif*/
			/* reset variables for next rotation */
			strcpy( station, cstation );
			trc_z = trc_n = trc_e = NULL;
		} /*endif*/
		if  (Cap(comp) == 'Z')  trc_z = trc;
		else if  (Cap(comp) == 'N')  trc_n = trc;
		else if  (Cap(comp) == 'E')  trc_e = trc;
	}  while (trc != NULL);

	/* cleanup */
	mx_exec_sh( wm, ctrl, "shm_cmd_rotate finish" );

} /* end of mx_rotation */


#undef MAX_NEW_TRC
#undef NEW_TRC_R
#undef NEW_TRC_T
#undef NEW_TRC_Z



/*---------------------------------------------------------------------------*/



BOOLEAN mx_need_rotation( void )

/* Returns whether rotation mode is switched on.  If no rotation is possible
 * it switches off rotation mode.
 *
 * no input parameters
 */
{
	/* local variables */
	TSyBoolean redraw;      /* redraw display (not used) */
	TSyStatus  locstat;     /* local status */

	/* executable code */

	if  (MXC_FLAG_ROT & mxv_display_state)  {
		if  (mxh_rotation_is_possible())  {
			return TRUE;
		} else {
			mx_clear_rotation();
			locstat = BC_NOERROR;
			callsh( "external_routine prepare_display", &redraw, &locstat );
			callsh( "hide all", &redraw, &locstat );
			callsh( "display _shm_view(y)", &redraw, &locstat );
			callsh( "rd r", &redraw, &locstat );
			if  (GpGetInt(cGpI_debug_level) > 1)
				printf( "SHM-dbg2: rotation mode switched off by mx_need_rotation\n" );
			return FALSE;
		} /*endif*/
	} else {
		return FALSE;
	} /*endif*/

} /* end of mx_need_rotation */



/*---------------------------------------------------------------------------*/



static BOOLEAN mxh_rotation_is_possible( void )

/* checks whether a rotation is possible on the traces in memory
 *
 * no parameters
 */
{
	/* local variables */
	TPmTrace   *trc;                        /* pointer to current trace */
	int        t;                           /* trace counter */
	TSyStatus  locstat;                     /* local status */
	char       station[BC_SHORTSTRLTH+1];   /* station name */
	char       c_station[BC_SHORTSTRLTH+1]; /* current station */
	char       c_comp;                      /* current component */
	TSyBoolean z_found, n_found, e_found;   /* components found */

	/* executable code */

	/* loop over all traces in memory */
	trc = NULL;
	station[0] = '\0';
	z_found = n_found = e_found = FALSE;
	for  (t=0; t<db_lstlth(); t++)  {
		/* get next trace pointer */
		locstat = BC_NOERROR;
		trc = db_getp( trc, EP_NEXT, &locstat );
		if  (Severe(&locstat))  {printf("*SHM: mx-error(10) ***\n"); continue;}
		/* get current station and component */
		db_gets( trc, ES_STATION, BC_SHORTSTRLTH, c_station, &locstat );
		if  (Severe(&locstat))  continue;
		c_comp = db_getc( trc, EC_COMP, &locstat );
		if  (Severe(&locstat))  continue;
		if  (strcmp(station,c_station) != 0)  {
			z_found = n_found = e_found = FALSE;
			strcpy( station, c_station );
		} /*endif*/
		switch (c_comp)  {
			case 'z':  case 'Z':  z_found = TRUE; break;
			case 'n':  case 'N':  n_found = TRUE; break;
			case 'e':  case 'E':  e_found = TRUE; break;
		} /*endswitch*/
		if  (z_found && n_found && e_found)  return TRUE;
	} /*endfor*/

	return FALSE;

} /* end of mxh_rotation_is_possible */



/*---------------------------------------------------------------------------*/



void mx_trclist_command( Widget wm, Widget ws, MGT_DSPCTRL *ctrl, char cmd[],
	STATUS *status )

/* executes SH command on trace list
 *
 * parameters of routine
 * Widget     wm, ws;      input; widget ID's of drawing areas
 * MGT_DSPCTRL *ctrl;      input; display control
 * char       cmd[];       input; command to be executed
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];     /* scratch string */
	char     cmdline[BC_LINELTH+1]; /* command string */

	/* executable code */

	ts_get_selection_string( BC_LINELTH, str );
	if  (*str == '\0' && *cmd >= 'a' && *cmd <= 'z')  return;
	if  (*str == '\0')  strcpy( str, "all" );

	if  (strlen(cmd)+strlen(str) > BC_LINELTH-1)  {
		*status = MXE_STROVFL;
		return;
	} /*endif*/
	sprintf( cmdline, cmd, str );

	if  (GpGetInt(cGpI_debug_level) > 3)
		printf( "SHM-dbg4: executing: >%s<\n", cmdline );
	mx_exec_sh( wm, ctrl, cmdline );

	ts_clear_selections();

	mg_do_drag( wm, MGC_DRAG_CLEAR, 0, 0 );
	mg_plot_drag_window( ws, ctrl->show_phase_acc, status );

} /* end of mx_trclist_command */



/*----------------------------------------------------------------------------*/



void mx_trclist_refml( STATUS *status )

/* stores reference ml in parameter list
 *
 * parameters of routine
 * TSyStatus     *status;      output; return status 
 */
{
	/* local variables */
	char       selstr[BC_LINELTH+1];        /* selection string */
	int        trcnum;                      /* trace number */
	TPmTrace   *trc;                        /* pointer to trace */
	char       station[BC_SHORTSTRLTH+1];   /* station name of ref ml */
	char       c_station[BC_SHORTSTRLTH+1]; /* current station */
	TPiPhaseRoot *proot;                    /* poiner to phase root */
	TPiPhaseList *plist;                    /* phase list */
	TPiPhase   *phase;                      /* pointer to phase info */
	int        p;                           /* phase counter */
	TSyStatus  locstat;                     /* local status */
	TSyBoolean ml_done;                     /* found ml entry on ref. station */
	TSyBoolean is_ml_station;               /* curr. station is ml ref station */

	/* executable code */

	ts_get_selection_string( BC_LINELTH, selstr );
	/* ts_clear_selections(); */  /* clear selection done in shm.c */
	if  (*selstr == '\0')  {
		*status = MXE_NONE_SELECT;
		return;
	} else if  (strchr(selstr,',') != NULL)  {
		*status = MXE_MANY_SELECT;
		err_setcontext( " ## too many traces selected: " );
		err_setcontext( selstr );
		return;
	} /*endif*/

	/* get station name of selected trace */
	trcnum = 0;
	sscanf( selstr, "%d", &trcnum );
	trc = mg_trcptr( trcnum );
	if  (trc == NULL)  {
		*status = MXE_PROG_BUG;
		err_setcontext( " ## selected trace resulted in NULL pointer" );
		return;
	} /*endif*/
	db_gets( trc, ES_STATION, BC_SHORTSTRLTH, station, status );
	if  (Severe(status))  return;

	/* loop all phases and look for ml entries */
	ml_done = FALSE;
	proot = NULL;
	FOREVER  {
		/* get next phase root pointer */
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		/* get phase list and station name */
		plist = PiPhaseListOfRoot( proot );
		if  (plist == NULL)  continue;
		strcpy( c_station, PiStationOfRoot(proot) );
		is_ml_station = (strcmp(c_station,station) == 0);
		/* loop all phases of trace */
		for  (p=0; p<PiPhaseListLength(plist); p++)  {
			phase = PiGetPhase( plist, p, status );
			if  (Severe(status))  return;
			if  (phase->magn_source == cPiMagnMl)  {
				/* found an ml entry */
				if  (is_ml_station)  {
					/* set magnitude flag */
					phase->flags |= fPiAttribMagn;
					ml_done = TRUE;
				} else {
					/* clear magnitude flag */
					phase->flags &= ~fPiAttribMagn;
				} /*endif*/
			} /*endif*/
		} /*endfor*/
	} /*endfor*/

	/* no ml entry found */
	if  (!ml_done)
		*status = MXE_NO_MLENTRY;

} /* end of mx_trclist_refml */



/*----------------------------------------------------------------------------*/



void mx_analyst_name( Widget w[], MGT_DSPCTRL *ctrl, STATUS *status )

/* Reads analyst's initials from dialog box checks it and closes dialog
 * box if everything is ok.
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * TSyStatus  *status;    output; return status
 */
{
	/* local variables */
	char       *initials;                  /* pointer to initials */
	char       analyst[BC_SHORTSTRLTH+1];  /* copy of initials */
	TSyBoolean found;                      /* name found */
	TGpTextList txtlst;                    /* text list with analysts initals */
	int        i;                          /* counter */

	/* executable code */

	initials = cu_get_string( w[k_widget_analyst_name_text] );
	if  (initials == NULL)  {*status = MXE_ILL_ANALYST; return;}
	if  (*initials == '\0')  {*status = MXE_ILL_ANALYST; return;}
	if  (strlen(initials) > 4)  {*status = MXE_ILL_ANALYST; return;}
	strcpy( analyst, initials );
	/*ut_cap( analyst );*/

	/* get list of analysts from setup file */
	GpParseTextList( GpGetString(cGpS_list_of_analysts), &txtlst );

	found = FALSE;
	for  (i=0; i<txtlst.numelem; i++)  {
		found = (strcmp(txtlst.elem[i],analyst) == 0);
		if  (found)  break;
	} /*endwhile*/

	GpFreeTextList( &txtlst );

	if  (found)  {
		XtUnmanageChild( w[k_widget_analyst_box] );
		GpSetString( cGpS_analyst, analyst, NULL );
	} else {
		*status = MXE_ILL_ANALYST;
	} /*endif*/

} /* end of mx_analyst_name */



/*--------------------------------------------------------------------------*/



void mx_filter_box_defaults( Widget w[], MX_CMD_FILTER *fil )

/* resets filter box
 *
 * parameters of routine
 * Widget     w[];       input; widget array
 * MX_CMD_FILTER *fil;   input; filter parameters
 */
{
	/* local variables */
	Arg      al[1];        /* argument list */
	int      cut_w_no;     /* widget number of cutoff time */
	int      i;            /* counter */

	/* executable code */

	if  (fil->autocut >= 600.0)       cut_w_no = k_widget_filter_autocut_10min;
	else if  (fil->autocut >= 300.0)  cut_w_no = k_widget_filter_autocut_5min;
	else if  (fil->autocut >= 180.0)  cut_w_no = k_widget_filter_autocut_3min;
	else if  (fil->autocut >= 60.0)   cut_w_no = k_widget_filter_autocut_1min;
	else if  (fil->autocut >= 20.0)   cut_w_no = k_widget_filter_autocut_20sec;
	else if  (fil->autocut >= 5.0)    cut_w_no = k_widget_filter_autocut_5sec;
	else                              cut_w_no = k_widget_filter_autocut_0;
	XtSetArg( al[0], XmNmenuHistory, w[cut_w_no] );
	XtSetValues( w[k_widget_filter_autocut_option], al, 1 );

	for  (i=k_widget_filter_none; i<=k_widget_filter_sro_lp; i++)
		if  (i != k_widget_filter_lrsm_lp)
			XmToggleButtonSetState( w[i], FALSE, FALSE );
	if  (strcmp(fil->name,"GRF_STANDARD_BP") == 0)
		XmToggleButtonSetState( w[k_widget_filter_standard_bp], TRUE, FALSE );
	else if  (strcmp(fil->name,"WWSSN_SP") == 0)
		XmToggleButtonSetState( w[k_widget_filter_wwssn_sp], TRUE, FALSE );
	else if  (strcmp(fil->name,"WWSSN_LP") == 0)
		XmToggleButtonSetState( w[k_widget_filter_wwssn_lp], TRUE, FALSE );
	else if  (strcmp(fil->name,"LRSM_SP") == 0)
		XmToggleButtonSetState( w[k_widget_filter_lrsm_sp], TRUE, FALSE );
	else if  (strcmp(fil->name,"LRSM_LP") == 0)
		XmToggleButtonSetState( w[k_widget_filter_lrsm_lp], TRUE, FALSE );
	else if  (strcmp(fil->name,"KIRNOS") == 0)
		XmToggleButtonSetState( w[k_widget_filter_kirnos], TRUE, FALSE );
	else if  (strcmp(fil->name,"WOODAND") == 0)
		XmToggleButtonSetState( w[k_widget_filter_woodand], TRUE, FALSE );
	else if  (strcmp(fil->name,"SRO_LP") == 0)
		XmToggleButtonSetState( w[k_widget_filter_sro_lp], TRUE, FALSE );

	cu_set_string( w[k_widget_filter_edit_text], fil->name );

} /* end of mx_filter_box_defaults */



/*----------------------------------------------------------------------------*/



void mx_sh_external_routine( char cmd[], STATUS *status )

/* external routine for SH
 *
 * parameters of routine
 * char       cmd[];         input; switch to subcommands
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */

	/* executable code */

	if  (strcmp(cmd,"PREPARE_DISPLAY") == 0)  {
		mx_prepare_display();
	} else if  (strcmp(cmd,"SAVE_DSP_PHASES") == 0)  {
		/*cl3_save_phase_display( status );*/
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: external_routine: SAVE_DSP_PHASES disabled\n" );
	} else if  (strcmp(cmd,"RESTORE_DSP_PHASES") == 0)  {
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: external_routine: RESTORE_DSP_PHASES disabled\n" );
	} else if  (strcmp(cmd,"RESET_DISPLAY_FLAGS") == 0)  {
		mxv_display_state = 0;
	} else if  (strcmp(cmd,"PHASE_CHECK") == 0)  {
		PiPhaseDump();
	} else if  (strncmp(cmd,"BTN_READS",9) == 0)  {
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: external_routine: btn_reads ignored\n" );
	} else if  (strncmp(cmd,"READ_DIALOG:",12) == 0)  {
		mxh_read_statlist( cmd+12, xmv_w, &xmv_cmd_readg.sl, status );
	} else if  (strncmp(cmd,"LOC_EXTERNAL:",13) == 0)  {
		mxh_set_entry_name( xmv_w[k_widget_param_hypoloc], cmd+13 );
	} else if  (strncmp(cmd,"TABLE_ML_SIGMA:",15) == 0)  {
		si_read_table( cmd+15, "ml-sigma", status );
	} else if  (strcmp(cmd,"DUMP_GLOBALS") == 0)  {
		GpDumpParams();
	} else if  (strncmp(cmd,"MINMAXFMT:",10) == 0)  {
		/* disabled */
		if  (GpGetInt(cGpI_debug_level) > 0)
			printf( "SHM-dbg1: function minmaxfmt obsolete\n" );
	} else {
		*status = MXE_ILL_EXTROUT;
		return;
	} /*endif*/

} /* end of mx_sh_external_routine */



/*----------------------------------------------------------------------------*/



static void mx_prepare_display( void )

/* selects traces to be displayed on next redraw (sets EF_VIEW flag).
 *
 */
{
	/* local variables */
	int        t;             /* trace counter */
	TPmTrace   *trc;          /* pointer to trace info */
	TSyStatus  locstat;       /* local status */
	TSyBoolean dsp;           /* do display trace */
	TSyBoolean dsp_filtered;  /* display filtered traces */
	TSyBoolean dsp_rotated;   /* display rotated traces */

	/* executable code */

	dsp_filtered = (MXC_FLAG_FILTER & mxv_display_state);
	dsp_rotated = (MXC_FLAG_ROT & mxv_display_state);

	/* loop over all traces in memory */
	trc = NULL;
	for  (t=0; t<db_lstlth(); t++)  {
		/* get next trace pointer */
		locstat = BC_NOERROR;
		trc = db_getp( trc, EP_NEXT, &locstat );
		if  (Severe(&locstat))  {printf("*SHM: mx-error(1) ***\n"); continue;}
		dsp = (
			!db_getf(trc,MXC_EF_HIDE,&locstat)
			&& db_getf(trc,MXC_EF_FILTER,&locstat) == dsp_filtered
			&& db_getf(trc,MXC_EF_ROT,&locstat) == dsp_rotated
		);
		db_setf( trc, MXC_EF_VIEW, dsp, &locstat );
		if  (Severe(&locstat))  {printf("*SHM: mx-error(2) ***\n"); continue;}
	} /*endfor*/

} /* end of mx_prepare_display */



/*----------------------------------------------------------------------------*/



void mx_clear_rotation( void )

/* resets rotation state
 *
 * no parameters
 */
{
	/* executable code */

	mxv_display_state &= ~MXC_FLAG_ROT;

} /* end of mx_clear_rotation */



/*----------------------------------------------------------------------------*/



void mx_get_chanlist( Widget w[], MX_CMD_READG *rg )


/* reads list of channels from dialog box into channel array
 *
 * parameters of routine
 * Widget        w[];          input; widget array
 * MX_CMD_READG  *rg;          output; channel array (chanstr)
 */
{
	/* local variables */
	int      i;                 /* channel counter */
	int      cwno[MXC_MAXCHAN]; /* channel widget numbers */
	char     *str;              /* pointer to string */

	/* executable code */

	cwno[0] = k_widget_read_grsn_1hz;
	cwno[1] = k_widget_read_grsn_20hz;
	cwno[2] = k_widget_read_grsn_80hz;
	cwno[3] = k_widget_read_grsn_edit_hz;

	/* reset channel counter */
	rg->channum = 0;

	for (i=0; i<MXC_MAXCHAN; i++)  {
		if  (XmToggleButtonGetState(w[cwno[i]]))  {
			strcpy( rg->chanstr[rg->channum], rg->sl.channel[i] );
			if  (strcmp(rg->chanstr[rg->channum],"ed") == 0)  {
				str = cu_get_string( w[k_widget_read_grsn_hz_text] );
				if  (strlen(str) <= MXC_CHANLTH && strlen(str) > 0)  {
					strcpy( rg->chanstr[rg->channum], str );
				} else {
					printf( "*SHM: illegal channel string %s\n", str );
				} /*endif*/
			} /*endif*/
			(rg->channum)++;
		} /*endif*/
	} /*endfor*/

#ifdef XXX
	/* check buttons */
	if  (XmToggleButtonGetState(w[k_widget_read_grsn_1hz]))  {
		strcpy( rg->chanstr[rg->channum], "LH" );
		(rg->channum)++;
	} /*endif*/
	if  (XmToggleButtonGetState(w[k_widget_read_grsn_20hz]))  {
		strcpy( rg->chanstr[rg->channum], "BH" );
		(rg->channum)++;
	} /*endif*/
	if  (XmToggleButtonGetState(w[k_widget_read_grsn_80hz]))  {
		strcpy( rg->chanstr[rg->channum], "HH" );
		(rg->channum)++;
	} /*endif*/
	if  (XmToggleButtonGetState(w[k_widget_read_grsn_edit_hz]))  {
		str = cu_get_string( w[k_widget_read_grsn_hz_text] );
		if  (strlen(str) <= MXC_CHANLTH && strlen(str) > 0)  {
			strcpy( rg->chanstr[rg->channum], str );
		} else {
			printf( "*SHM: illegal channel string %s\n", str );
		} /*endif*/
		(rg->channum)++;
	} /*endif*/
#endif

} /* end of mx_get_chanlist */



/*----------------------------------------------------------------------------*/



static void mxh_read_statlist( char fname[], Widget w[], MX_STATLIST *sl,
	STATUS *status )

/* reads in station list from file 'fname'
 *
 * parameters of routine
 * char        fname[];        input; name of input file
 * Widget      w[];            input; widget array (widgets will be modified)
 * MX_STATLIST *sl;            output; station list for dialog box
 * TSyStatus   *status;        output; return status
 */
{
	/* local variables */
	FILE     *fp;                /* pointer to input file */
	char     line[BC_LINELTH+1]; /* current line */
	int      s;                  /* station counter */
	int      c;                  /* channel counter */
	int      cwno[MXC_MAXCHAN];  /* channel widget numbers */
	int      slen;               /* string length */
	char     *ch;                /* char pointer */
	Arg      args[2];            /* argument list */
	int      n;                  /* argument counter */
	XmString xmstr;              /* Xm string */

	/* executable code */

	fp = sy_fopen( fname, "r" );
	if  (fp == NULL)  {
		*status = MXE_OPENREAD;
		err_setcontext( " ## file " );  err_setcontext( fname );
		return;
	} /*endif*/

	/* initialize output record */
	for  (s=0; s<MXC_STATLIST_LTH; s++)  strcpy( sl->code[s], "---" );
	strcpy( sl->set1code, "---" );
	strcpy( sl->set2code, "---" );
	sl->set1start = sl->set1end = sl->set2start = sl->set2end = 0;
	strcpy( sl->channel[0], "LH" );
	strcpy( sl->channel[1], "BH" );
	strcpy( sl->channel[2], "HH" );
	strcpy( sl->channel[3], "ed" );

	s = c = 0;
	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		slen = strlen( line ) - 1;
		if  (line[slen] == '\n')  line[slen] = '\0';
		if  (strncmp(line,"set1:",5) == 0)  {
			if  (sscanf(line+5,"%d %d %s",
				&(sl->set1start),&(sl->set1end),sl->set1code) != 3)
				fprintf( stderr,
					"*SHM: mx_read_statlist: error reading subset 1\n" );
		} else if  (strncmp(line,"set2:",5) == 0)  {
			if  (sscanf(line+5,"%d %d %s",
				&(sl->set2start),&(sl->set2end),sl->set2code) != 3)
				fprintf( stderr,
					"*SHM: mx_read_statlist: error reading subset 2\n" );
		} else if  (*line == '*')  {
			if  (s == MXC_STATLIST_LTH)  {
				fprintf( stderr, "*SHM: mx_read_statlist: too many stations\n" );
			} else {
				ch = line + 1;
				while  (*ch == ' ' && *ch != '\0')  ch++;
				if  (strlen(ch) > MXC_STATSTR_LTH)  {
					fprintf( stderr,
						"*SHM: mx_read_statlist: station name %s too long\n", ch );
				} else {
					strcpy( sl->code[s++], ch );
				} /*endif*/
			} /*endif*/
		} else if  (*line == 'c')  {
			if  (c < MXC_MAXCHAN)  {
				sl->channel[c][0] = line[2];
				sl->channel[c][1] = line[3];
				sl->channel[c][2] = '\0';
				c++;
			} else {
				fprintf( stderr, "*SHM: mx_read_statlist: too many channels\n" );
			} /*endif*/
		} else {
			fprintf( stderr, "*SHM: mx_read_statlist: illegal line:\n%s", line );
		} /*endif*/
	} /*endwhile*/

	sy_fclose( fp );

	/* set dialog box */
	for  (s=0; s<MXC_STATLIST_LTH; s++)  {
		xmstr = XmStringCreateLtoR( sl->code[s], "" );
		n = 0;
		XtSetArg( args[n], XmNlabelString, xmstr ); n++;
		XtSetValues( w[k_widget_read_grsn_station+s], args, n );
		XtSetSensitive( w[k_widget_read_grsn_station+s], (sl->code[s][0] != '-'));
	} /*endfor*/

	xmstr = XmStringCreateLtoR( sl->set1code, "" );
	n = 0;
	XtSetArg( args[n], XmNlabelString, xmstr ); n++;
	XtSetValues( w[k_widget_read_grsn_grf], args, n );
	XtSetSensitive( w[k_widget_read_grsn_grf], (sl->set1code[0] != '-') );
	xmstr = XmStringCreateLtoR( sl->set2code, "" );
	n = 0;
	XtSetArg( args[n], XmNlabelString, xmstr ); n++;
	XtSetValues( w[k_widget_read_grsn_grsn], args, n );
	XtSetSensitive( w[k_widget_read_grsn_grsn], (sl->set2code[0] != '-') );

	cwno[0] = k_widget_read_grsn_1hz;
	cwno[1] = k_widget_read_grsn_20hz;
	cwno[2] = k_widget_read_grsn_80hz;
	cwno[3] = k_widget_read_grsn_edit_hz;
	for  (c=0; c<MXC_MAXCHAN; c++)  {
		xmstr = XmStringCreateLtoR( sl->channel[c], "" );
		n = 0;
		XtSetArg( args[n], XmNlabelString, xmstr ); n++;
		XtSetValues( w[cwno[c]], args, n );
		XtSetSensitive( w[cwno[c]], (sl->channel[c][0] != '-'));
	} /*endfor*/

} /* end of mxh_read_statlist */



/*----------------------------------------------------------------------------*/



void mx_name_read_buttons( Widget w[], int dialbox_num, TSyStatus *status )

/* reads in station list from parameter file.  Original from mxh_read_statlist.
 *
 * parameters of routine
 * Widget      w[];            input; widget array (widgets will be modified)
 * int         dialbox_num;    input; number of dialog box to be modified
 * MX_STATLIST *sl;            output; station list for dialog box
 * TSyStatus   *status;        output; return status
 */
{
	/* local variables */
	char     parname[cBcLineLth+1];   /* name of parameter (dialog box number) */
	char     parval[cBcLongStrLth+1]; /* list of stations */
	int      ok;                      /* parameter ok? */
	TGpTextList slist;                /* parsed station list */
	int      s;                       /* station counter */
	int      setcnt;                  /* set counter */
	int      c;                       /* channel counter */
	int      cwno[MXC_MAXCHAN];       /* channel widget numbers */
	Arg      args[2];                 /* argument list */
	int      n;                       /* argument counter */
	XmString xmstr;                   /* Xm string */
	MX_STATLIST *sl;                  /* station list for dialog box */
	int      slen;                    /* string length */

	/* executable code */

	sl = &(xmv_cmd_readg.sl);

	/* get station parameter from parameter file */
	sprintf( parname, "v$read_dialog_stations_%02d", dialbox_num );
	GpReadParameter( parname, cBcLongStrLth, parval, &ok );
	if  (!ok)  {*status = MXE_NOT_FOUND;return;}
	GpParseTextList( parval, &slist );

	/* initialize output record */
	for  (s=0; s<MXC_STATLIST_LTH; s++)  strcpy( sl->code[s], "---" );
	strcpy( sl->set1code, "---" );
	strcpy( sl->set2code, "---" );
	sl->set1start = sl->set1end = sl->set2start = sl->set2end = 0;
	strcpy( sl->channel[0], "LH" );
	strcpy( sl->channel[1], "BH" );
	strcpy( sl->channel[2], "HH" );
	strcpy( sl->channel[3], "ed" );

	setcnt = 0;
	for  (s=0; s<slist.numelem; s++)  {
		slen = strlen( slist.elem[s] );
		if  (slen > MXC_STATSTR_LTH)  {
			fprintf( stderr,
				"*SHM: mx_name_read_buttons: station name %s too long\n",
				slist.elem[s] );
			continue;
		} /*endif*/
		if  (s == MXC_STATLIST_LTH)  {
			strcpy( sl->set1code, slist.elem[s] );
		} else if  (s == MXC_STATLIST_LTH+1)  {
			strcpy( sl->set2code, slist.elem[s] );
		} else if  (s < MXC_STATLIST_LTH)  {
			if  (slist.elem[s][0] == '[')  {
				setcnt++;
				if  (setcnt == 1)  {
					sl->set1start = s;
				} else if  (setcnt == 2)  {
					sl->set2start = s;
				} /*endif*/
				strcpy( sl->code[s], slist.elem[s]+1 );
			} else if  (slist.elem[s][slen-1] == ']')  {
				if  (setcnt == 1)  {
					sl->set1end = s;
				} else if  (setcnt == 2)  {
					sl->set2end = s;
				} /*endif*/
				strcpy( sl->code[s], slist.elem[s] );
				sl->code[s][slen-1] = '\0';
			} else {
				strcpy( sl->code[s], slist.elem[s] );
			} /*endif*/
		} /*endif*/
	} /*endfor*/
	if  (setcnt > 0 && sl->set1code[0] == '-')  strcpy( sl->set1code, "SET1" );
	if  (setcnt > 1 && sl->set2code[0] == '-')  strcpy( sl->set2code, "SET2" );

	GpFreeTextList( &slist );

	/* get channel parameter from parameter file */
	sprintf( parname, "v$read_dialog_channels_%02d", dialbox_num );
	GpReadParameter( parname, cBcLongStrLth, parval, &ok );
	if  (ok)  {
		GpParseTextList( parval, &slist );
		c = 0;
		for  (s=0; s<slist.numelem; s++)
			if  (s < MXC_MAXCHAN && strlen(slist.elem[s]) <= MXC_CHANLTH)
				strcpy( sl->channel[c++], slist.elem[s] );
		GpFreeTextList( &slist );
	} /*endif*/

	/* set dialog box */
	for  (s=0; s<MXC_STATLIST_LTH; s++)  {
		xmstr = XmStringCreateLtoR( sl->code[s], "" );
		n = 0;
		XtSetArg( args[n], XmNlabelString, xmstr ); n++;
		XtSetValues( w[k_widget_read_grsn_station+s], args, n );
		XtSetSensitive( w[k_widget_read_grsn_station+s], (sl->code[s][0] != '-'));
		XmToggleButtonSetState( w[k_widget_read_grsn_station+s], FALSE, TRUE );
	} /*endfor*/

	xmstr = XmStringCreateLtoR( sl->set1code, "" );
	n = 0;
	XtSetArg( args[n], XmNlabelString, xmstr ); n++;
	XtSetValues( w[k_widget_read_grsn_grf], args, n );
	XtSetSensitive( w[k_widget_read_grsn_grf], (sl->set1code[0] != '-') );
	xmstr = XmStringCreateLtoR( sl->set2code, "" );
	n = 0;
	XtSetArg( args[n], XmNlabelString, xmstr ); n++;
	XtSetValues( w[k_widget_read_grsn_grsn], args, n );
	XtSetSensitive( w[k_widget_read_grsn_grsn], (sl->set2code[0] != '-') );

	cwno[0] = k_widget_read_grsn_1hz;
	cwno[1] = k_widget_read_grsn_20hz;
	cwno[2] = k_widget_read_grsn_80hz;
	cwno[3] = k_widget_read_grsn_edit_hz;
	for  (c=0; c<MXC_MAXCHAN; c++)  {
		xmstr = XmStringCreateLtoR( sl->channel[c], "" );
		n = 0;
		XtSetArg( args[n], XmNlabelString, xmstr ); n++;
		XtSetValues( w[cwno[c]], args, n );
		XtSetSensitive( w[cwno[c]], (sl->channel[c][0] != '-'));
	} /*endfor*/

} /* end of mx_name_read_buttons */



/*----------------------------------------------------------------------------*/



static void mxh_read_file_list( Widget w, MGT_DSPCTRL *ctrl, char filenames[],
	int format, STATUS *status )

/* reads in all GSE files matching the wildcard string "filenames"
 *
 * parameters of routine
 * Widget     w;                 input; widget ID of drawing area
 * MGT_DSPCTRL *ctrl;            input; display control params
 * char       filenames[];       input; wild card string for filenames
 * int        format;            input; format code
 * TSyStatus  *status;           output; return status
 */
{
	/* local variables */
	char        currfile[BC_FILELTH+1];      /* current file */
	char        altfile[cBcFileLth+1];       /* another filename */
	int         filecnt;                     /* file counter */
	char        cmd[BC_LINELTH+1];           /* comand line */
	char        *cp;                         /* moving pointer */
	TSyBoolean  redraw;                      /* dummy */
	int         i;                           /* counter */

	/* executable code */

	mx_exec_sh( w, ctrl, "nr" );
	filecnt = 0;
	FOREVER {

		/* try to find next matching file */
		sy_findfile( SYC_FF_NAME|SYC_FF_DIR|SYC_FF_EXT, filenames, currfile );
		if  (*currfile == '\0')  {
			if  (filecnt == 0)  {
				*status = MXE_NO_GSE_MATCH;
				break;
			} /*endif*/
			break;
		} /*endif*/

		/* put backslashes instead if slashes because of SH command line */
		cp = currfile - 1;
		while  (*(++cp) != '\0')
			if  (*cp == '/')
				*cp = '\\';

		/* process file currfile */
		if  (strlen(currfile)+12 > BC_LINELTH)  {
			*status = MXE_STROVFL;
			break;
		} /*endif*/
		switch  (format)  {
		case MXC_FORMAT_GSE: sprintf( cmd, "@READGSE_MERGE %s ALL", currfile ); break;
		case MXC_FORMAT_GSE2: sprintf( cmd, "@READGSE_MERGE %s ALL", currfile ); break;
		case MXC_FORMAT_AH: sprintf( cmd, "@READAH %s ALL", currfile ); break;
		case MXC_FORMAT_Q:
			strcpy( altfile, currfile );
			i = strlen( altfile ) - 4;
			if  (i > 1 && strcmp(altfile+i,".QBN") == 0)  {
				altfile[i] = '\0';
				sprintf( cmd, "@READ %s ALL", altfile );
			} else {
				sprintf( cmd, "! @READ %s ALL", altfile );
			} /*endif*/
			break;
		default:
			printf( "*SHM: mxh_read_file_list: this should not happen\n" );
			break;
		} /*endswitch*/
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: executing command: %s\n", cmd );
		callsh( cmd, &redraw, status );
		if  (Severe(status))  {
			/* should print out error in a more elaborate way */
			printf( "*SHM: readgse error %d\n", *status );
			*status = BC_NOERROR;
		} /*endif*/
		/* mx_exec_sh( w, ctrl, cmd ); */

		/* increment file counter */
		filecnt++;

	} /*endfor*/
	mx_exec_sh( w, ctrl, "rd" );

} /* end of mxh_read_file_list */



/*----------------------------------------------------------------------------*/


#define MAX_S_LTH 8


static void mxh_filter_separately( Widget w, MGT_DSPCTRL *ctrl, char filter[],
	float autocut, STATUS *status )

/* Filters traces on display separately.
 *
 * parameters of routine
 * Widget     w;             input; widget ID of drawing window
 * MGT_DSPCTRL *ctrl;        input; display control
 * char       filter[];      input; name of filter (without prefix)
 * float      autocut;       input; autocut of traces in sec
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	void     *trc;                         /* trace pointer */
	int      trcnum;                       /* trace number on display */
	char     complete_name[BC_FILELTH+1];  /* complete filter name */
	char     tfname[BC_FILELTH+1];         /* transfer function name */
	char     last_name[BC_FILELTH+1];      /* last filter */
	BOOLEAN  simulation;                   /* is simulation filter */
	char     station[BC_SHORTSTRLTH+1];    /* station name */
	char     cmd[BC_LINELTH+1];            /* SH command */
	char     filter_typ, filter_typ_save;  /* filter type */
	void     *newtrc;                      /* pointer to new trace */
	void     *tmp;                         /* scratch pointer */
	void     *lasttrc;                     /* last trace at beginning */
	TIME     ttime;                        /* start time of trace */
	char     strtime[cBcTimeLth+1];        /* converted start time */
	int      trcflags;                     /* trace flags for filtered traces */
	TSyStatus locstat;                     /* local status */
	BOOLEAN  dmy;                          /* dummy */
	unsigned filent;                       /* filter entry */
	char     singfilt[BC_FILELTH+1];       /* single filter name */
	char     filprefix[BC_FILELTH+1];      /* filter prefix on rec. filters */

	/* executable code */

	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: mx_filter: different filters used\n" );

#ifdef XXX
	filter_typ = (shv_global.use_rec_filters) ? 'R' : 'F';
	if  (shv_global.filter_type != ' ')  filter_typ = shv_global.filter_type;
#endif
	filter_typ = GpGetChar(cGpC_filter_type);
	filter_typ_save = filter_typ;
	simulation = (strchr(filter,'+') != NULL);
	if  (strlen(filter) > BC_FILELTH-2*MAX_S_LTH)  {
		*status = MXE_STROVFL;
		return;
	} /*endif*/

	/* get index number of filter entry */
	db_ident( "FILTER", &filent, status );
	if  (SySevere(status))  return;

	/* prepare traces */
	mx_exec_sh( w, ctrl, "shm_cmd_sepfil_begin" );

	/* set newtrc to last trace in memory */
	newtrc = NULL;
	FOREVER  {
		tmp = db_getp( newtrc, EP_NEXT, NULL );
		if  (tmp == NULL)  break;
		newtrc = tmp;
	} /*endfor*/
	if  (newtrc == NULL)  {
		printf( "*SHM: mx_filter: no traces\n" );
		*status = MXE_PROG_BUG;
		return;
	} /*endif*/
	lasttrc = newtrc;

	/* loop all traces on display */
	trcnum = 0;
	*last_name = '\0';
	trc = NULL;
	do  {
		filter_typ = filter_typ_save; /* in case it has been changed by FORCE_T */
		trcnum++;
		trcflags = 0;
		trc = db_getp( trc, EP_NEXT, NULL );
		if  (trc == NULL)  {
			printf( "*SHM: mx_filter: this is impossible (10)\n" );
			*status = MXE_PROG_BUG;
			return;
		} /*endif*/
		if  (!ts_some_trace_selected() || ts_is_selected(trcnum))  {
			*complete_name = '\0';
			if  (filter_typ == 'R')  {
				cl_filter_prefix( trc, MAX_S_LTH, complete_name );
				strcpy( filprefix, complete_name );
			} /*endif*/
			if  (simulation)  {
				/*db_gets( trc, ES_STATION, BC_SHORTSTRLTH, station, status );*/
				/*if  (Severe(status))  return;*/
				locstat = cBcNoError;
				db_gett( trc, ET_START, &ttime, &locstat );
				tc_a2t( &ttime, strtime, &locstat );
				cu_get_stream_string( trc, station, NULL );
				cu_lookup_filter( station, strtime, tfname, &trcflags, status );
				if  (Severe(status))  return;
				if  (trcflags & CUC_F_TRC_FORCE_F)  {
					strcpy( complete_name, tfname );
					filter_typ = 'F';
				} else if  (trcflags & CUC_F_TRC_FORCE_T)  {
					strcpy( complete_name, tfname );
					filter_typ = 'T';
				} else {
					strcat( complete_name, tfname );
				} /*endif*/
				strcat( complete_name, "_" );
			} /*endif*/
			strcat( complete_name, filter );
			if  (ts_is_selected(trcnum))  {
				/* take filter from dialog box into filter info entry */
				if  (GpGetInt(cGpI_debug_level) > 3)
					printf( "shm-dbg4: set single filter %s on trace %d\n",
						filter, trcnum );
				if  (filter_typ == 'R')  {
					strcpy( singfilt, filprefix );
					strcat( singfilt, filter );
				} else {
					strcpy( singfilt, filter );
				} /*endif*/
				db_sets( trc, filent, singfilt, status );
				if  (Severe(status))  return;
			} else  {
				locstat = cBcNoError;
				db_gets( trc, filent, BC_FILELTH, singfilt, &locstat );
				if  (locstat == cBcNoError && *singfilt != '\0')  {
					strcat( complete_name, ";" );
					strcat( complete_name, singfilt );
				} /*endif*/
			} /*endif*/
			if  (strcmp(complete_name,last_name) != 0)  {
				sprintf( cmd, "fili %c %s", filter_typ, complete_name );
				callsh( cmd, &dmy, status );
				if  (Severe(status))  {
					*status = BC_NOERROR;
					if  (GpGetInt(cGpI_debug_level) > 1)
						printf( "SHM-dbg2: filter, %s not available, use dummy filter\n",
							complete_name );
					sprintf( cmd, "fili %c dummy", filter_typ );
					callsh( cmd, &dmy, status );
					if  (Severe(status))  return;
				} /*endif*/
				strcpy( last_name, complete_name );
			} /*endif*/
			sprintf( cmd, "filter %c a:%lx", filter_typ, trc );
			callsh( cmd, &dmy, status );
			if  (Severe(status))  return;
			newtrc = db_getp( newtrc, EP_NEXT, NULL );
			if  (newtrc == NULL)  {
				*status = MXE_PROG_BUG;
				printf( "*SHM: mx_filter: no trace created\n" );
				return;
			} /*endif*/
			sprintf( cmd, "flt:%s", filter );
			db_sets( newtrc, ES_COMMENT, cmd, status );
			if  (Severe(status))  return;
			db_setf( newtrc, MXC_EF_FILTER, TRUE, status );
			if  (Severe(status))  return;
		} else {
			sprintf( cmd, "copy a:%lx", trc );
			callsh( cmd, &dmy, status );
			if  (Severe(status))  return;
			newtrc = db_getp( newtrc, EP_NEXT, NULL );
			if  (newtrc == NULL)  {
				*status = MXE_PROG_BUG;
				printf( "*SHM: mx_filter: no trace created\n" );
				return;
			} /*endif*/
			sprintf( cmd, "flt:NONE" );
			db_sets( newtrc, ES_COMMENT, cmd, status );
			if  (Severe(status))  return;
			db_setf( newtrc, MXC_EF_FILTER, TRUE, status );
			if  (Severe(status))  return;
		} /*endif*/

		/* make trace red on alert flag */
		if  (CUC_F_TRC_ALERT & trcflags)
			mg_make_alert_trace( w, newtrc, status );

	}  while (trc != lasttrc);

	/* cleanup */
	sprintf( cmd, "shm_cmd_sepfil_finish %s %f %d",
		filter, autocut, (GpGetInt(cGpI_trace_normalisation)==cGp_NORM_CONST) );
	mx_exec_sh( w, ctrl, cmd );

	ts_clear_selections();

} /* end of mxh_filter_separately */


#undef MAX_S_LTH



/*----------------------------------------------------------------------------*/


#ifdef XXX

#define GRFNUM 19



static void mxh_get_vax_grflist( MX_CMD_READG *rg, char liststr[] )

/* returns list string for readk command
 *
 * parameters of routine
 * MX_CMD_READG *rg;          input; read parameters
 * char       liststr[];      output; list string
 */
{
	/* local variables */
	long     statmap;         /* station map */
	int      compmap;         /* component map */
	int      idx[GRFNUM];     /* list of GRF numbers */
	int      cnt;             /* number of stations */
	int      i;               /* counter */

	/* executable code */

	if  (strcmp(rg->sl.code[0],"GRA1") != 0)  {
		strcpy( liststr, "NONE" );
		return;
	} /*endif*/

	statmap = 0;
	compmap = rg->comp;

	/* make own station map */
	for  (i=0; i<MXC_STATLIST_LTH; i++)  {
		if  ((1<<i) & rg->stations)  {
			if  (strncmp(rg->sl.code[i],"GR",2) != 0)  continue;
			if  (strcmp(rg->sl.code[i],"GRA1") == 0)     statmap |= (1<<STC_GRA1);
			else if (strcmp(rg->sl.code[i],"GRA2") == 0) statmap |= (1<<STC_GRA2);
			else if (strcmp(rg->sl.code[i],"GRA3") == 0) statmap |= (1<<STC_GRA3);
			else if (strcmp(rg->sl.code[i],"GRA4") == 0) statmap |= (1<<STC_GRA4);
			else if (strcmp(rg->sl.code[i],"GRB1") == 0) statmap |= (1<<STC_GRB1);
			else if (strcmp(rg->sl.code[i],"GRB2") == 0) statmap |= (1<<STC_GRB2);
			else if (strcmp(rg->sl.code[i],"GRB3") == 0) statmap |= (1<<STC_GRB3);
			else if (strcmp(rg->sl.code[i],"GRB4") == 0) statmap |= (1<<STC_GRB4);
			else if (strcmp(rg->sl.code[i],"GRB5") == 0) statmap |= (1<<STC_GRB5);
			else if (strcmp(rg->sl.code[i],"GRC1") == 0) statmap |= (1<<STC_GRC1);
			else if (strcmp(rg->sl.code[i],"GRC2") == 0) statmap |= (1<<STC_GRC2);
			else if (strcmp(rg->sl.code[i],"GRC3") == 0) statmap |= (1<<STC_GRC3);
			else if (strcmp(rg->sl.code[i],"GRC4") == 0) statmap |= (1<<STC_GRC4);
		} /*endif*/
	} /*endfor*/

	/* create list of GRF numbers */
	cnt = 0;
	if  ((1<<STC_GRA1) & statmap && (1<<STC_COMP_Z) & compmap)  idx[cnt++] = 1;
	if  ((1<<STC_GRA1) & statmap && (1<<STC_COMP_N) & compmap)  idx[cnt++] = 2;
	if  ((1<<STC_GRA1) & statmap && (1<<STC_COMP_E) & compmap)  idx[cnt++] = 3;
	if  ((1<<STC_GRA2) & statmap && (1<<STC_COMP_Z) & compmap)  idx[cnt++] = 4;
	if  ((1<<STC_GRA3) & statmap && (1<<STC_COMP_Z) & compmap)  idx[cnt++] = 5;
	if  ((1<<STC_GRA4) & statmap && (1<<STC_COMP_Z) & compmap)  idx[cnt++] = 6;
	if  ((1<<STC_GRB1) & statmap && (1<<STC_COMP_Z) & compmap)  idx[cnt++] = 7;
	if  ((1<<STC_GRB1) & statmap && (1<<STC_COMP_N) & compmap)  idx[cnt++] = 8;
	if  ((1<<STC_GRB1) & statmap && (1<<STC_COMP_E) & compmap)  idx[cnt++] = 9;
	if  ((1<<STC_GRB2) & statmap && (1<<STC_COMP_Z) & compmap)  idx[cnt++] = 10;
	if  ((1<<STC_GRB3) & statmap && (1<<STC_COMP_Z) & compmap)  idx[cnt++] = 11;
	if  ((1<<STC_GRB4) & statmap && (1<<STC_COMP_Z) & compmap)  idx[cnt++] = 12;
	if  ((1<<STC_GRB5) & statmap && (1<<STC_COMP_Z) & compmap)  idx[cnt++] = 13;
	if  ((1<<STC_GRC1) & statmap && (1<<STC_COMP_Z) & compmap)  idx[cnt++] = 14;
	if  ((1<<STC_GRC1) & statmap && (1<<STC_COMP_N) & compmap)  idx[cnt++] = 15;
	if  ((1<<STC_GRC1) & statmap && (1<<STC_COMP_E) & compmap)  idx[cnt++] = 16;
	if  ((1<<STC_GRC2) & statmap && (1<<STC_COMP_Z) & compmap)  idx[cnt++] = 17;
	if  ((1<<STC_GRC3) & statmap && (1<<STC_COMP_Z) & compmap)  idx[cnt++] = 18;
	if  ((1<<STC_GRC4) & statmap && (1<<STC_COMP_Z) & compmap)  idx[cnt++] = 19;

	/* put this list into a string */
	if  (cnt == 0)  { strcpy( liststr, "NONE" ); return; }
	sprintf( liststr, "%d", idx[0] );
	for  (i=1; i<cnt; i++)
		sprintf( liststr+strlen(liststr), ",%d", idx[i] );

} /* end of mxh_get_vax_grflist */

#endif

/*----------------------------------------------------------------------------*/



static void mxh_mount_cdrom( MX_CMD_READG *par, STATUS *status )

/* mounts cdrom for reading
 *
 * parameters of routine
 * MX_CMD_READG        *par;    input/modify; read command parameters
 * STATUS              *status; output; return status
 */
{
	/* local variables */
	int         i;                        /* counter */
	char        stream[BC_LINELTH+1];     /* stream string */
	char        cmdline[BC_LONGSTRLTH+1]; /* command line */
	char        *env;                     /* pointer to environment */
	TSyBoolean  redraw;                   /* not used */
	TSyStatus   locstat;                  /* not used */
	TSyBoolean  is_sol2;                  /* is Solaris 2 machine */

	/* executable code */

	if  (par->channum == 0)  {
		*status = MXE_NOCHANNELS;
		return;
	} else if  (par->channum > 1)  {
		printf( "*SHM: take only first channel selected\n" );
	} /*endif*/

	/* find first station in list */
	for  (i=0; i<MXC_STATLIST_LTH; i++)
		if  ((1<<i) & par->stations)  break;

	/* if no station selected return */
	if  (i == MXC_STATLIST_LTH)  {
		*status = MXE_NOSTATIONS;
		return;
	} /*endif*/

	/* build stream name from first station */
	strcpy( stream, par->sl.code[i] );
	strcat( stream, "-" );
	strcat( stream, par->chanstr[0] );
	strcat( stream, "-z" );
	ut_uncap( stream );

	is_sol2 = cl_is_solaris_2();

	/* close SEED files */
	locstat = BC_NOERROR;
	callsh( "reads/close", &redraw, &locstat );

	/* build and execute command line */
	if  (is_sol2)  {
		/* *cmdline = '\0'; */
		strcpy( cmdline, "cmdtool " );
	} else {
		strcpy( cmdline, "cmdtool " );
	} /*endif*/
	env = getenv( "SEED_PROG" );
	if  (env != NULL)  {
		strcat( cmdline, env );
		strcat( cmdline, "/" );
	} /*endif*/
	sprintf( cmdline+strlen(cmdline), "szgrf_cd_mount.csh %s %s",
		stream, par->start );
	system( cmdline );

	if  (is_sol2)  {
		strcpy( par->device, "/cdrom/cdrom0" );
	} else {
		strcpy( par->device, "/cdrom" );
	} /*endif*/

} /* end of mxh_mount_cdrom */



/*----------------------------------------------------------------------------*/



static void mxh_set_entry_name( Widget ew, char text[] )

/* Sets menu entry text
 *
 * parameters of routine
 * Widget     ew;          input; menu entry widget
 * char       text[];      input; new text for widget
 */
{
	/* local variables */
	char     ltext[cBcLineLth+1]; /* local copy of text to remove '_' */
	Arg      args[2];             /* argument list */
	int      n;                   /* argument counter */
	XmString xmstr;               /* Xm string */

	/* executable code */

	if  (strlen(text) >= cBcLineLth)  {
		fprintf( stderr, "*SHM: mxh_set_entry_name: name too long\n" );
		return;
	} /*endif*/
	strcpy( ltext, text );
	for  (n=0; n<strlen(ltext); n++)
		if  (ltext[n] == '_')  ltext[n] = ' ';

	xmstr = XmStringCreateLtoR( ltext, "" );
	n = 0;
	XtSetArg( args[n], XmNlabelString, xmstr ); n++;
	XtSetValues( ew, args, n );

} /* end of mxh_set_entry_name */



/*----------------------------------------------------------------------------*/



void mx_remove_double_elements( char str[] )

/* removes double elements from comma separated list
 *
 * parameters of routine
 * char       str[]; modify; list
 */
{
	/* local variables */
	TGpTextList tl;       /* text list */
	int      i, j;        /* counters */

	/* executable code */

	GpParseTextList( str, &tl );

	for  (i=0; i<tl.numelem; i++)
		for  (j=(i+1); j<tl.numelem; j++)
			if  (strcmp(tl.elem[i],tl.elem[j]) == 0)
				tl.elem[j][0] = '\0';

	strcpy( str, tl.elem[0] );
	for  (i=1; i<tl.numelem; i++)  {
		if  (tl.elem[i][0] != '\0')  {
			strcat( str, "," );
			strcat( str, tl.elem[i] );
		} /*endif*/
	} /*endfor*/

	GpFreeTextList( &tl );

} /* end of mx_remove_double_elements */



/*----------------------------------------------------------------------------*/



void mxh_compute_azimuth( char station[], float lat, float lon,
	float *azim, TSyStatus *status )

/* Computes azimuth from station and epicentre location
 *
 * parameters of routine
 * char       cstation[];  input; name of station
 * float      lat, lon;    input; epicentre location
 * float      *azim;       output; azimuth computed
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	GLT_STATINF *statinf;    /* station info */
	double      d_dist, d_azim, d_bazim;

	/* executable code */

	statinf = gl_store_station( station, TRUE, status );
	if  (Severe(status))  return;
	mb_locdiff( statinf->lat, statinf->lon, lat, lon,
		&d_dist, &d_azim, &d_bazim );
	*azim = d_bazim;

} /* end of mxh_compute_azimuth */



/*----------------------------------------------------------------------------*/


static void mxh_read_stations_from_evt( char autoevt[], char addlist[] )

/* Reads station list from evtfile
 *
 * parameters of routine
 * char       autoevt[];      input: name of evt file
 * char       addlist[];      output; list of station names
 */
{
	/* local variables */
	FILE       *fp;                          /* pointer to file */
	char       tmpfile[cBcFileLth+1];        /* temporary file */
	static int fcnt=1;                       /* temp file counter */
	char       shellcmd[cBcLongStrLth+1];    /* shell command */
	char       line[cBcLineLth+1];           /* current line in file */
	int        i;                            /* counter */

	/* executable code */

	*addlist = '\0';
	sprintf( tmpfile, "/tmp/ae_stations_%d.000", fcnt++ );
	sprintf( shellcmd,
		"grep 'Station code           :' %s | awk '{print $4}' | sort -u >%s",
		autoevt, tmpfile );
	printf( "executing: %s\n", shellcmd );
	system( shellcmd );
	fp = fopen( tmpfile, "r" );
	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		i = strlen( line );
		if  (line[i-1] == '\n')  line[i-1] = '\0';
		if  (strlen(addlist) > cBcLongStrLth-2)  {
			fclose( fp );
			printf( "*SHM: evt station list truncated\n" );
			return;
		} /*endif*/
		if  (*addlist != '\0')  strcat( addlist, "," );
		strcat( addlist, line );
	} /*endwhile*/
	fclose( fp );
}


/*----------------------------------------------------------------------------*/

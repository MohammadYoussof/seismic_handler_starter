
/* file cblib4.c
 *      =======
 *
 * version 50, 12-Nov-2007
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/ToggleB.h>
#undef BC_DEFINE_TRUE_FALSE
#ifdef BC_INC_UNISTD
#include BC_INC_UNISTD
#endif
#include "sysbase.h"
#include "shvars.h"
#include "infoidx.h"
#include "tcusrdef.h"
#include "utusrdef.h"
#include "erusrdef.h"
#include "ptusrdef.h"
#include "earthloc.h"
#include "fctxdm.h"
#include "station_no.h"
#include "../seed_io/seedcfg.h"
#include "../seed_io/seed_lib.h"
#include "motifgraph.h"
#include "cbutil.h"
#include "mfexec.h"
#include "cblib4.h"
#include "phaseinf.h"
#include "phasemgr.h"
#include "shm_widgets.h"
#include "trcselect.h"
#include "residual.h"
#include "globalparams.h"



#define CL4C_HELP_EXT ".shm"



/* prototype of get_geo */
void xxx_get_geo(float *elat, float *elon, int *num_id, char *qnam);

/* prototypes of local routines */
void cl4h_difftime_distance( char phase1[], char phase2[], float depth,
	float *distance, int *pairs, STATUS *status );

/* global constants */
#define SN_NOISE_S  0
#define SN_NOISE_E  1
#define SN_SIGNAL_S 2
#define SN_SIGNAL_E 3
#define SN_NUM_PTS  4


/* global variables */

/* names of dummy phases */
static char cl4v_sn_pnames[SN_NUM_PTS][cPiMaxPhaseLth+1] = {
	"b1", "b2", "b3", "b4"
};


/*---------------------------------------------------------------------------*/



void cl4_help( Widget w, char title[] )

/* Puts help text from file to help window.  The help files are found in the
 * help directory 'shd_help' with the extension '.SHM'.
 *
 * parameters of routine
 * Widget     w;           input; widget ID of help window
 * char       title[];     input; help title
 */
{
	/* local variables */
	char     fname[BC_FILELTH+1];      /* name of help file */
	FILE     *hlp;                     /* pointer to help file */
	char     line[BC_LINELTH+1];       /* current line */
	XmString xm_line;                  /* text for widget */
	int      slen;                     /* string length */

	/* executable code */

	if  (strlen(shd_help)+strlen(title)+strlen(CL4C_HELP_EXT) > BC_FILELTH)  {
		fprintf( stderr, "*SHM: help: string overflow in help filename ***\n" );
		return;
	} /*endif*/

	XmListDeleteAllItems( w );

	strcpy( fname, shd_help );
	strcat( fname, title );
	strcat( fname, CL4C_HELP_EXT );
	hlp = sy_fopen( fname, "r" );
	if  (hlp == NULL)  {
		sprintf( line, "*** help: couldn't find help item '%s' ***", title );
		xm_line = XmStringLtoRCreate( line, "" );
		XmListAddItem( w, xm_line, 0 );
		return;
	} /*endif*/

	while  (fgets(line,BC_LINELTH,hlp) != NULL)  {
		if  (*line == '!')  continue;
		slen = strlen( line ) - 1;
		if  (line[slen] == '\n')  line[slen] = '\0';
		xm_line = XmStringLtoRCreate( line, "" );
		XmListAddItem( w, xm_line, 0 );
	} /*endwhile*/

	sy_fclose( hlp );

} /* end of cl4_help */



/*---------------------------------------------------------------------------*/



#define SPECCMDNAME "special_commands.txt"
#define MAXCMDCNT 15



void cl4_read_special_commands( Widget w[] )

/* Reads in names of special commands from file $SH_INPUTS/special_commands.txt
 *
 * parameters of routine
 * Widget     w[];         input; widget array
 */
{
	/* local variables */
	char     fname[BC_FILELTH+1];     /* name of input file */
	FILE     *fp;                     /* pointer to input file */
	char     line[BC_LINELTH+1];      /* current line of file */
	int      line_cnt;                /* line counter */
	Widget   tw;                      /* text widget */
	int      slen;                    /* string length */

	/* executable code */

	if  (strlen(shd_inputs)+strlen(SPECCMDNAME) > BC_FILELTH)  {
		fprintf( stderr,
			"*SHM: string overflow in reading of special commands\n" );
		return;
	} /*endif*/
	strcpy( fname, shd_inputs );
	strcat( fname, SPECCMDNAME );

	fp = sy_fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "*SHM: special commands file %s not found\n", fname );
		return;
	} /*endif*/

	line_cnt = 0;
	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		if  (line_cnt == MAXCMDCNT)  {
			sy_fclose( fp );
			fprintf( stderr,
				"*SHM: too many special commands in file %s\n", fname );
			return;
		} /*endif*/
		slen = strlen( line ) - 1;
		if  (line[slen] == '\n')  line[slen] = '\0';
		tw = w[k_widget_speccmd_1_text+line_cnt];
		cu_set_string( tw, line );
		line_cnt++;
	} /*endwhile*/

	sy_fclose( fp );

} /* end of cl4_read_special_commands */



/*---------------------------------------------------------------------------*/



void cl4_do_vespa( Widget w[], MGT_DSPCTRL *ctrl, float azim,
	TSyBoolean on_off, TSyStatus *status )

/* Shows vespagram fo array data
 *
 * parameters of routine
 * Widget     w[];         input; widget array
 * MGT_DSPCTRL *ctrl;      input; display control
 * float      azim;        input; azimuth for vespagram
 * TSyBoolean on_off;      input; switch vespa on or off
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	char       cmd[BC_LINELTH+1];      /* SH command */
	int        trcno;                  /* trace number */
	TPmTrace   *trc;                   /* pointer to trace */
	TSyStatus  locstat;                /* local status */

	/* executable code */

	if  (on_off)  {
		/* look for BEAM or ALIGN traces */
		trcno = mg_dsptrcs();
		if  (trcno == 0)  { *status = MXE_NO_TRACES; return; }
		trc = mg_trcptr( trcno );
		locstat = BC_NOERROR;
		db_gets( trc, ES_STATION, BC_LINELTH, cmd, &locstat );
		if  (!Severe(&locstat))  {
			if  (strcmp(cmd,"BEAM") == 0 || strcmp(cmd,"ALIGN") == 0)  {
				*status = CL4E_ILL_BEAM_TRACE;
				return;
			} /*endif*/
		} /*endif*/
		/* enable/disable vespa buttons */
		XtSetSensitive( w[k_widget_vespa_ok], FALSE );
		XtSetSensitive( w[k_widget_vespa_cancel], FALSE );
		XtSetSensitive( w[k_widget_vespa_undo], TRUE );
		/* execute SH command */
		sprintf( cmd, "shm_cmd_vespa %s %s %s %6.2f %s",
			cu_get_string( w[k_widget_vespa_slolo_text] ),
			cu_get_string( w[k_widget_vespa_slohi_text] ),
			cu_get_string( w[k_widget_vespa_slostep_text] ),
			azim,
			cu_get_string( w[k_widget_vespa_power_text] ) );
		mx_exec_sh( w[k_widget_draw], ctrl, cmd );
	} else {
		XtSetSensitive( w[k_widget_vespa_ok], TRUE );
		XtSetSensitive( w[k_widget_vespa_cancel], TRUE );
		XtSetSensitive( w[k_widget_vespa_undo], FALSE );
		mx_exec_sh( w[k_widget_draw], ctrl, "del all" );
		mx_exec_sh( w[k_widget_draw], ctrl, "external_routine prepare_display" );
		mx_exec_sh( w[k_widget_draw], ctrl, "display _shm_view(y)" );
	} /*endif*/

} /* end of cl4_do_vespa */



/*---------------------------------------------------------------------------*/



void cl4_phase_difference( char phaselist[], float depth, float *distance,
	TSyStatus *status )

/* Computes distance in degrees from travel time differences of phase pairs.
 * The phase pairs are listed in 'phaselist' (Example "S-P,PP-P,SS-S").
 * Each phase pair is used at several stations if selected.  The resulting
 * distance is a mean value of all computed distances.
 *
 * parameters of routine
 * char       phaselist[];       input; phase list (Example "S-P,PP-P,SS-S")
 * float      depth;             input; depth of event
 * float      *distance;         output; mean value of distances
 * STATUS     *status;           output; return status
 */
{
	/* local variables */
	char       phase1[BC_LINELTH+1];    /* phase 1 */
	char       phase2[BC_LINELTH+1];    /* phase 1 */
	char       *src, *dst;              /* moving pointers */
	float      cdist;                   /* current distance */
	int        cpairs;                  /* current number of pairs */
	int        totpairs;                /* total number of pairs */
	TSyStatus  locstat;                 /* local status */
	int        n_phases;                /* number of phase pairs */

	/* executable code */

	totpairs = n_phases = 0;
	src = phaselist;
	*distance = 0.0;

	while  (*src != '\0')  {

		/* get phase pair */
		dst = phase1;
		while  (*src != '-' && *src != '\0')  *dst++ = *src++;
		*dst = '\0';
		if  (*src == '-')  src++;
		dst = phase2;
		while  (*src != ',' && *src != '\0')  *dst++ = *src++;
		*dst = '\0';
		if  (*src == ',')  src++;

		/* get distance from this phase pair */
		locstat = BC_NOERROR;
		cl4h_difftime_distance( phase2, phase1, depth, &cdist, &cpairs, &locstat);
		if  (locstat == CL4E_NO_PHASES)  continue;
		if  (Severe(&locstat))  {*status = locstat; return;}
		totpairs += cpairs;

		/* print result */
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: diff %5s - %5s: distance %5.1f deg (%d pairs)\n",
				phase1, phase2, cdist, cpairs );
		/* compute mean value */
		n_phases++;
		*distance = ((float)(n_phases-1)*(*distance) + cdist) / (float)n_phases;

	} /*endwhile*/

	if  (GpGetInt(cGpI_debug_level) > 0)
		printf( "SHM-dbg1: mean distance: %5.1f deg (%d:%d phase pairs)\n",
			*distance, totpairs, n_phases );

} /* end of cl4_phase_difference */



/*---------------------------------------------------------------------------*/



void cl4_loc_from_dist( CUT_PARAMS *par, TSyStatus *status )

/* Computes new location (latitude & longitude) from location of reference
 * point (or reference station if par->ref_lat & par->ref_lon is empty)
 * and distance and azimuth.
 *
 * parameters of routine
 * CUT_PARAMS    *par;      modify; analysis parameters
 * TSyStatus     *status;   output; return status
 */
{
	/* local variables */
	float    rlat, rlon;     /* reference location */
	float    deg_dist;       /* distance in degrees */
	float    backaz;         /* back-azimuth */
	GLT_STATINF *statinf;    /* pointer to station info */

	/* executable code */

	if  (par->ref_lat == 0.0 && par->ref_lon == 0.0)  {
		if  (strcmp(GpGetString(cGpS_refstation),"CENTRE") == 0
			|| strcmp(GpGetString(cGpS_refstation),"CENTER") == 0)  {
			*status = CL4E_NO_REFERENCE;
			return;
		} /*endif*/
		statinf = gl_store_station( GpGetString(cGpS_refstation), TRUE, status );
		if  (Severe(status))  return;
		rlat = statinf->lat;
		rlon = statinf->lon;
	} else {
		rlat = par->ref_lat;
		rlon = par->ref_lon;
	} /*endif*/

	deg_dist = (par->dist_unit == CUC_DIST_UNIT_DEG) ? par->distance :
		par->distance / CUC_DEG_TO_KM;
	backaz = (par->l_azimuth != 0.0) ? par->l_azimuth : par->b_azimuth;

	mb_sphereadd( rlat, rlon, deg_dist, backaz,
		&(par->source_lat), &(par->source_lon) );

	if  (cu_german_event(par->source_lat,par->source_lon))  {
		xxx_get_geo( &(par->source_lat), &(par->source_lon),
			&(par->reg_id), par->regname );
		if  (par->reg_id == -1)  {
			*status = CL4E_GET_GEO_ERR;
			return;
		} /*endif*/
		par->table_number = CUC_REGTABLE_GERGEO;
	} else {
		mb_ferindex( par->source_lat, par->source_lon, &(par->reg_id), status );
		if  (Severe(status))  return;
		par->table_number = CUC_REGTABLE_FLINNENG;
		mb_fername( par->reg_id, BC_LINELTH, par->regname, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: loc (%5.1f,%5.1f) %s\n", par->source_lat,
			par->source_lon, par->regname );

} /* end of loc_from_dist */



/*---------------------------------------------------------------------------*/



void cl4h_difftime_distance( char phase1[], char phase2[], float depth,
	float *distance, int *pairs, TSyStatus *status )

/* Computes distance (deg) of two given phases.  The two phases are searched
 * on each trace and, if found, a distance is computed.  The resulting distance
 * is the mean value of all these single-trace-distances.  If no phase pair
 * is found an error status is returned.
 *
 * parameters of routine
 * char       phase1[];       input; name of first phase
 * char       phase2[];       input; name of second phase
 * float      depth;          input; event depth in km
 * float      *distance;      output; mean distance in deg
 * TSyStatus  *status         output; return status
 */
{
	/* local variables */
	float      curr_dist;      /* current distance */
	int        found_cnt;      /* number of distances computed */
	float      mean_dist;      /* mean distance */
	TPiPhaseRoot *proot;       /* pointer to phase root */
	TPiPhaseList *plist;       /* phase list */
	TPiPhase   *p1, *p2;       /* pointers to both phases */
	TSyStatus  locstat;        /* local status */
	float      difftime;       /* time difference between phases */
	char       station[BC_SHORTSTRLTH+1];    /* processed station */
	char       c_station[BC_SHORTSTRLTH+1];  /* current station */
	TSyBoolean stat_processed; /* station already processed */

	/* executable code */

	found_cnt = 0;
	mean_dist = 0.0;
	stat_processed = FALSE;
	*station = *c_station = '\0';
	p1 = p2 = NULL;

	/* loop all traces */
	proot = NULL;
	FOREVER  {
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		plist = PiPhaseListOfRoot( proot );
		if  (plist == NULL)  continue;
		strcpy( c_station, PiStationOfRoot(proot) );
		if  (strcmp(station,c_station) == 0)  {
			if  (stat_processed)  continue;
		} else {
			/* reset values */
			stat_processed = FALSE;
			p1 = p2 = NULL;
			strcpy( station, c_station );
		} /*endif*/
		if  (p1 == NULL)
			p1 = PiFindPhase( plist, phase1 );
		if  (p1 == NULL)  continue;
		if  (p2 == NULL)
			p2 = PiFindPhase( plist, phase2 );
		if  (p2 == NULL)  continue;
		difftime = tc_tdiff( p2->onset, p1->onset, status );
		if  (Severe(status))  return;
		if  (difftime < 0.0)  {*status = CL4E_ILL_DIFFTIME; return;}
		curr_dist = pt_distance_pd( phase1, phase2, difftime, depth, status );
		if  (SySevere(status))  return;
		mean_dist += curr_dist;
		found_cnt++;
		stat_processed = TRUE;
	} /*endfor*/

	*pairs = found_cnt;
	if  (found_cnt == 0)  {
		*status = CL4E_NO_PHASES;
		return;
	} /*endif*/

	if  (GpGetInt(cGpI_debug_level) > 2)
		printf( "SHM-dbg3: %d phase pairs found\n", found_cnt );
	*distance = mean_dist / (float)found_cnt;

} /* end of cl4h_difftime_distance */



/*---------------------------------------------------------------------------*/



#define SFDNAME "sfdfile.sfd"



void cl4_select_all_possible_stations( Widget w[], MX_CMD_READG *rg,
	TSyStatus *status )

/* Selects all possible stations in read dialog box for specified stream
 * and time span.
 *
 * parameters of routine
 * Widget       w[];        input; widget array
 * MX_CMD_READG *rg;        modify; read parameters
 * TSyStatus    *status;    output; return status
 */
{
	/* local variables */
	long     i;                       /* counter */
	char     sfdfile[BC_FILELTH+1];   /* sfd-file */
	char     *cptr;                   /* pointer to char */
	char     stream[BC_SHORTSTRLTH+1];/* stream string */
	int      slen;                    /* string length */
	char     endtime[BC_TIMELTH+1];   /* end of time span */
	char     cmp;                     /* component */
	INT32    statbits;                /* station bits */
	TSyBoolean set_btn;               /* set button */

	/* executable code */

	if  (rg->channum == 0)  {
		printf( "*SHM: no channels selected\n" );
		return;
	} else if  (rg->channum > 1)  {
		printf( "*SHM: take only first channel selected\n" );
	} /*endif*/

	/* deselect all if toggle button is released */
	if  (!XmToggleButtonGetState(w[k_widget_read_grsn_all]))  {
		for  (i=(rg->sl.set1start); i<(rg->sl.set1end); i++)
			XmToggleButtonSetState( w[k_widget_read_grsn_station+i], FALSE, TRUE );
		for  (i=(rg->sl.set2start); i<(rg->sl.set2end); i++)
			XmToggleButtonSetState( w[k_widget_read_grsn_station+i], FALSE, TRUE );
		return;
	} /*endif*/

	/* update rg-parameters */
	cptr = cu_get_string( w[k_widget_read_grsn_date] );
	strcpy( rg->start, cptr );
	strcat( rg->start, "_" );
	cptr = cu_get_string( w[k_widget_read_grsn_time] );
	strcat( rg->start, cptr );
	cptr = cu_get_string( w[k_widget_read_grsn_length] );
	if  (sscanf(cptr,"%f",&(rg->seclth)) != 1)  {
		printf( "*SHM: length conversion error\n" );
		return;
	} /*endif*/
	rg->seclth *= 60.0;
	if  (XmToggleButtonGetState(w[k_widget_read_grsn_edit_hz]))  {
		cptr = cu_get_string( w[k_widget_read_grsn_hz_text] );
		if  (strlen(cptr) <= MXC_CHANLTH && strlen(cptr) > 0)  {
			strcpy( rg->chanstr[0], cptr );
		} else {
			printf( "*SHM: illegal channel string %s\n", cptr );
			return;
		} /*endif*/
	} /*endif*/
	cptr = cu_get_string( w[k_widget_read_grsn_device] );
	strcpy( rg->device, cptr );
	rg->format = MXC_FORMAT_SEED;

	/* get name of sfd-file */
	cptr = getenv( rg->device );
	if  (cptr != NULL)  {
		if  (strlen(cptr)+strlen(SFDNAME) > BC_FILELTH-1)  {
			*status = CL4E_STROVFL;
			return;
		} /*endif*/
		strcpy( sfdfile, cptr );
		strcat( sfdfile, "/" );
		strcat( sfdfile, SFDNAME );
	} else {
		strcpy( sfdfile, rg->device );
		strcat( sfdfile, SFDNAME );
	} /*endif*/

	tc_tadd( rg->start, rg->seclth, endtime, status );
	if  (Severe(status))  return;

	cmp = rg->comp[0];
	if  (cmp < 'A')  return;

	SeedInquireTimeList( sfdfile, rg->sl.code[0], MXC_STATSTR_LTH+1,
		MXC_STATLIST_LTH, rg->chanstr[0], cmp, rg->start, endtime, &statbits,
		status );
	if  (Severe(status))  return;

	/* select button if data available */
	for  (i=0; i<MXC_STATLIST_LTH; i++)  {
		set_btn = (((1L<<i) & statbits) == (1L<<i));
		XmToggleButtonSetState(
			w[k_widget_read_grsn_station+i], set_btn, TRUE );
		if  (GpGetInt(cGpI_debug_level) > 3)
			printf( "SHM-dbg4: station %s %d\n", rg->sl.code[i], set_btn );
	} /*endfor*/

} /* end of cl4_select_all_possible_stations */



/*---------------------------------------------------------------------------*/



#define FREE_AND_RETURN \
	sy_deallocmem( trclist ); \
	sy_deallocmem( inf ); \
	return;



void cl4_sort_traces( Widget w, MGT_DSPCTRL *ctrl, char infoname[],
	CUT_PARAMS *par, TSyStatus *status )

/* Sorts traces on display by given information (mainly distance & azimuth)
 *
 * parameters of routine
 * Widget     w;                 input; widget ID of drawing area
 * MGT_DSPCTRL *ctrl;            input; display control
 * char       infoname[];        input; name of information
 * CUT_PARAMS *par;              input; analysis parameters
 * TSyStatus  *status;           output; return status
 */
{
	/* local variables */
	int         dsplth;                    /* number of traces on display */
	TPmTrace    *trc;                      /* pointer to trace */
	TPmTrace    **trclist;                 /* pointer to list of traces */
	float       *inf;                      /* pointer to list of infos */
	int         i, j;                      /* counters */
	int         maxidx;                    /* trace index of maximum info value*/
	float       maxval;                    /* maximum info value */
	GLT_STATINF *statinf;                  /* station info */
	STATLOC     rlat, rlon;                /* reference location */
	float       elat, elon;                /* epicenter coordinates */
	float       dist;                      /* distance */
	double      d_dist, d_azim, d_bazim;   /* distance and azimuth */
	char        station[BC_SHORTSTRLTH+1]; /* station name */
	TSyBoolean  sort_dist;                 /* sort by distance (not azimuth) */
	char        str[BC_SHORTSTRLTH+1];     /* scratch string */

	/* executable code */

	sort_dist = (strcmp(infoname,"DISTANCE") == 0);

	/* compute epicenter if not already determined */
	if  (par->source_lat == 0.0 && par->source_lon == 0.0)  {
		dist = par->distance;
		if  (par->dist_unit == CUC_DIST_UNIT_KM)
			dist /= CUC_DEG_TO_KM;
		if  (dist == 0.0)  dist = 60.0; /* distance is not really important */
		cu_reference_location( &rlat, &rlon, NULL,NULL,NULL, status );
		if  (Severe(status))  return;
		mb_sphereadd( rlat, rlon, dist, par->b_azimuth, &elat, &elon );
	} else {
		elat = par->source_lat;
		elon = par->source_lon;
	} /*endif*/

	/* check number of traces */
	/* get length of display */
	dsplth = 0;
	trc = NULL;
	do  {
		trc = db_getp( trc, EP_DSPN, NULL );
		if  (trc != NULL)  dsplth++;
	}  while (trc != NULL);

	/* allocate memory for lists */
	trclist = (TPmTrace **)sy_allocmem( dsplth, (int)sizeof(void *), status );
	if  (Severe(status))  return;
	inf = (float *)sy_allocmem( dsplth, (int)sizeof(float), status );
	if  (Severe(status))  {sy_deallocmem(trclist); return;}

	/* compute, distance and azimuth, */
	/* fill lists with trace pointers and info value */
	trc = NULL;
	for  (i=0; i<dsplth; i++)  {
		trc = db_getp( trc, EP_DSPN, status );
		if  (Severe(status))  { FREE_AND_RETURN }
		if  (trc == NULL)  { fprintf( stderr, "*SHM: xyz1\n" ); FREE_AND_RETURN }
		db_gets( trc, ES_STATION, BC_SHORTSTRLTH, station, status );
		if  (Severe(status))  { FREE_AND_RETURN }
		statinf = gl_store_station( station, TRUE, status );
		if  (Severe(status))  { FREE_AND_RETURN }
		mb_locdiff( statinf->lat, statinf->lon, elat, elon,
			&d_dist, &d_azim, &d_bazim );
		trclist[i] = trc;
		inf[i] = sort_dist ? (float)d_dist : (float)d_bazim;
		/* put result to opinfo entry */
		sprintf( str, "%e", inf[i] );
		db_sets( trc, ES_OPINFO, str, status );
		if  (Severe(status))  { FREE_AND_RETURN }
	} /*endfor*/

	/* hide all traces of list */
	for (i=0; i<dsplth; i++)
		db_delist( gc, trclist[i] );

	/* redisplay in order of value */
	for  (i=0; i<dsplth; i++)  {
		maxidx = -1;
		maxval = -1.0e37;
		for  (j=0; j<dsplth; j++)  {
			if  (trclist[j] != NULL)  {
				if  (inf[j] > maxval)  {
					maxval = inf[j];
					maxidx = j;
				} /*endif*/
			} /*endif*/
		} /*endfor*/
		if  (maxidx == -1)  {
			fprintf( stderr, "*SHM: sort: prog bug (y)\n" );
		} else {
			db_enlist( trclist[maxidx], 1 /*0x7fff*/ );
			trclist[maxidx] = NULL;
		} /*endif*/
	} /*endfor*/

	/* free memory of lists */
	sy_deallocmem( trclist );
	sy_deallocmem( inf );

	mx_exec_sh( w, ctrl, "rd" );

} /* end of cl4_sort_traces */



#undef FREE_AND_RETURN



/*---------------------------------------------------------------------------*/



#define CMDNAME "shm_grsn_request.txt"

#ifdef XXX

void cl4_request_data( Widget w[], MX_CMD_READG *rg, TSyStatus *status )

/* Requests data from GRSN stations
 *
 * parameters of routine
 * Widget       w[];               input; widget array
 * MX_CMD_READG *rg;               input; read parameters
 * TSyStatus    *status;           output; return status
 */
{
	/* local variables */
	char     cmdfile[BC_FILELTH+1];     /* command file */
	FILE     *fp;                       /* pointer to file */
	int      s;                         /* station counter */
	int      c;                         /* component counter */
	int      i;                         /* counter */
	char     *cptr;                     /* pointer to char */
	char     str[BC_LINELTH+1];         /* scratch string */
	char     substr[BC_LINELTH+1];      /* substring of request */
	char     datname[BC_FILELTH+1];     /* name of output file */
	char     lstat[BC_SHORTSTRLTH+1];   /* station name in lowercase */
	TSyBoolean sfd_special;             /* SFD environment is set */
	int      s_start, s_end;            /* index window for station loop */

	/* executable code */

	if  (rg->channum == 0)  {
		printf( "*SHM: no channels selected\n" );
		return;
	} else if  (rg->channum > 1)  {
		printf( "*SHM: take only first channel selected\n" );
	} /*endif*/

	strcpy( cmdfile, shd_scratch );
	strcat( cmdfile, CMDNAME );
	fp = sy_fopen( cmdfile, "w" );
	if  (fp == NULL)  {
		*status = CL4E_OPENWRITE;
		err_setcontext( " ## file " ); err_setcontext( cmdfile );
		return;
	} /*endif*/

	/* put output directory to 'str' */
	sfd_special = FALSE;
	cptr = getenv( "SFD" );
	if  (cptr == NULL)  {
		strcpy( str, shd_scratch );
	} else if  (strlen(cptr) > BC_FILELTH-1)  {
		strcpy( str, shd_scratch );
	} else {
		strcpy( str, cptr );
		strcat( str, "/" );
		sfd_special = TRUE;
	} /*endif*/

	/* print start of command file */
	fprintf( fp, "#! /bin/csh\n" );
	fprintf( fp, "setenv QUIET_STARTUP 1\n" );
	fprintf( fp,
		"if  (-e /usr/local/common_startup)  source /usr/local/common_startup\n");
	fprintf( fp, "unsetenv QUIET_STARTUP\n" );
	fprintf( fp, "cd %s\n", str );
	if  (!sfd_special)
		fprintf( fp, "\\rm shm_grsn_req_*\n" );

	/* get time and length info */
	cptr = cu_get_string( w[k_widget_read_grsn_date] );
	strcpy( substr, cptr );
	strcat( substr, "_" );
	cptr = cu_get_string( w[k_widget_read_grsn_time] );
	strcat( substr, cptr );
	strcat( substr, " " );
	cptr = cu_get_string( w[k_widget_read_grsn_length] );
	strcpy( str, cptr );
	i = 0;
	while  (str[i] != '\0' && str[i] != '.')  i++;
	str[i] = '\0';
	strcat( substr, str );
	strcat( substr, "m " );
	strcpy( str, rg->chanstr[0] );
	ut_uncap( str );
	if  (strcmp(str,"lh") == 0)  strcpy( str, "lp" );
	if  (strcmp(str,"bh") == 0)  strcpy( str, "vbb" );
	if  (strcmp(str,"hh") == 0)  strcpy( str, "vsp" );
	strcat( substr, str );
	ut_uncap( substr );

	/* print request commands to command file */
	s_start = rg->sl.set2start;
	s_end = rg->sl.set2end;
	if  (s_start == s_end)  {
		s_start = rg->sl.set1start;
		s_end = rg->sl.set1end;
	} /*endif*/
	for  (s=s_start; s<=s_end; s++)  {
		if  (rg->sl.code[s][0] == '-')  continue;
		if  (!XmToggleButtonGetState(w[k_widget_read_grsn_station+s]))  continue;
		strcpy( lstat, rg->sl.code[s] );
		ut_uncap( lstat );
		for  (cptr=rg->comp; (*cptr)!='\0'; cptr++)  {
			if  (cl4_station_read_in(lstat,*cptr))  {
				if  (GpGetInt(cGpI_debug_level) > 2)
					printf( "SHM-dbg3: station %s-%c already read in\n",
						lstat, cptr );
				continue;
			} /*endif*/
			fprintf( fp, "echo copying %s-%s-%c ...\n",
				lstat, rg->chanstr[0], cptr );
			if  (sfd_special)  {
				sprintf( datname, "shm_grsn_req_%s.%c.$$", lstat, cptr );
			} else {
				sprintf( datname, "shm_grsn_req_%s.%c", lstat, cptr );
			} /*endif*/
			fprintf( fp, "drm_get_data %s %s %c 1 %s\n",
				lstat, substr, cptr, datname );
			fprintf( fp, "chmod a+w %s\n", datname );
		} /*endfor*/
	} /*endfor*/

	/* print trailer commands to command file */
	fprintf( fp, "sfdlist 'shm_grsn_req_*' `pwd` sfdfile.sfd\n" );
	if  (sfd_special)  {
		fprintf( fp, "\\mv sfdfile.sfd sfdfile_local.sfd\n" );
		fprintf( fp, "$DPROG/concat_sfd.csh >sfdfile.sfd\n" );
	} /*endif*/

	sy_fclose( fp );

	sprintf( str, "chmod a+x %s", cmdfile );
	system( str );
	if  (shv_global.remreqhost[0] > ' ')  {
		sprintf( str, "ssh %s %s", shv_global.remreqhost, cmdfile );
	} else {
		strcpy( str, cmdfile );
	} /*endif*/
	system( str );

} /* end of cl4_request_data */

#endif

#undef CMDNAME



/*---------------------------------------------------------------------------*/



BOOLEAN cl4_station_read_in( char station[], char comp )

/* checks whether station and component is already read in
 *
 * parameters of routine
 * char       station[];       input; station to be checked
 * char       comp;            input; component to be checked
 *                             returns TRUE if station found in memory
 */
{
	/* local variables */
	void     *trc;                     /* trace pointer */
	char     cstat[BC_SHORTSTRLTH+1];  /* current station */
	char     refstat[BC_SHORTSTRLTH+1];/* check station in uppercase */
	STATUS   locstat;                  /* local status */

	/* executable code */

	strncpy( refstat, station, BC_SHORTSTRLTH );
	ut_cap( refstat );

	trc = NULL;
	FOREVER  {
		trc = db_getp( trc, EP_NEXT, NULL );
		if  (trc == NULL)  break;
		locstat = BC_NOERROR;
		db_gets( trc, ES_STATION, BC_SHORTSTRLTH, cstat, &locstat );
		if  (Severe(&locstat))  continue;
		if  (strcmp(cstat,refstat) == 0)
			if  (db_getc(trc,EC_COMP,&locstat) == Cap(comp))
				return TRUE;
	} /*endfor*/

	return FALSE;

} /* end of cl4_station_read_in */



/*---------------------------------------------------------------------------*/



void cl4_manage_sfdfile( Widget w[], MX_CMD_READG *readg )

/* Manages sfdfiles used in reads commands
 *
 * parameters of routine
 * Widget     w[];          input; widget array
 * MX_CMD_READG *readg;     modify; readg-parameters (esp. sfdfile)
 */
{
	/* local variables */
	static char old_datadir[BC_FILELTH+1]; /* last value of data dir */
	char     cmd[BC_LONGSTRLTH+1];         /* shell command */

	/* executable code */

	if  (XmToggleButtonGetState(w[k_widget_read_grsn_todaysfd]))  {
		/* create new today-sfd file */
		sprintf( cmd, "%supdate_today_sfd.csh today %stoday.sfd",
			GpGetString(cGpS_defpath_extprog), shd_scratch );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: execute >%s<\n", cmd );
		system( cmd );
		/* change values of sfdfile and sfd directory */
		strcpy( readg->sfdfile, "today.sfd" );
		strcpy( old_datadir, cu_get_string(w[k_widget_read_grsn_device]) );
		cu_set_string( w[k_widget_read_grsn_device], "SH_SCRATCH" );
	} else {
		/* reset values of sfdfile and sfd directory */
		strcpy( readg->sfdfile, "sfdfile.sfd" );
		cu_set_string( w[k_widget_read_grsn_device], old_datadir );
	} /*endif*/

} /* end of cl4_manage_sfdfile */



/*---------------------------------------------------------------------------*/



void cl4_del_magnitude( Widget wd, int magntype )

/* loops all phases in memory and deletes all magnitudes of given type.
 * If traces are selected, only magnitudes of selected stations are deleted.
 *
 * parameters of routine
 * Widget     wd;                  input; main drawing area
 * int        magntype;            input; type of magnitude (cPiMagn...)
 */
{
	/* local variables */
	TSyStatus     locstat;       /* local status */
	TPiPhaseRoot  *proot;        /* pointer to phase root */
	TPiPhaseList  *plist;        /* phase list */
	TPiPhase      *phase;        /* pointer to phase */
	int           p;             /* phase counter */
	char          station[cBcLineLth+1];  /* station name */
	TSyBoolean    delall;        /* delete all magnitudes (not only selected) */

	/* executable code */

	ts_get_selection_string( cBcLineLth, station );
	delall = (*station == '\0');

	/* loop all phases */
	proot = NULL;
	FOREVER  {
		/* get next phase root */
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		/* get phase list */
		plist = PiPhaseListOfRoot( proot );
		if  (plist == NULL)  continue;
		if  (!delall)  {  /* if not all magnitudes to delete check for station */
			strcpy( station, PiStationOfRoot(proot) );
			if  (!cl4_station_is_selected(station))  continue;
		} /*endif*/
		/* loop all phases of trace */
		for  (p=0; p<PiPhaseListLength(plist); p++)  {
			phase = PiGetPhase( plist, p, NULL );
			if  (phase == NULL)  break;
			if  (phase->magn_source == magntype)  {
				phase->magn_source = cPiMagnUndefined;
				phase->magnitude = 0.0;
			} /*endif*/
		} /*endfor*/
	} /*endfor*/

	if  (!delall)  mg_clear_selections( wd );

} /* end of cl4_del_magnitude */



/*---------------------------------------------------------------------------*/



void cl4_read_refstation_list( Widget w[], TSyStatus *status )

/* reads list of reference stations and puts names to option buttons
 *
 * parameters of routine
 * Widget     w[];           input; list of all widgets
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	int      listlth;                 /* number of stations to read */
	int      i, n;                    /* counters */
	int      statno;                  /* station counter */
	Arg      args[2];                 /* argument list */
	XmString xmstr;                   /* Xm string */
	TGpTextList refstatlist;          /* list of reference stations */

	/* executable code */

	/* number of stations to read */
	listlth = k_widget_refstat_station_last - k_widget_refstat_station_1 + 1;

	GpParseTextList( GpGetString(cGpS_list_of_refstations), &refstatlist );

	/* read all stations from list */
	statno = 0;
	for  (i=0; i<refstatlist.numelem; i++)  {
		if  (strlen(refstatlist.elem[i]) > 8)  {
			printf( "*SHM: illegal station in ref list: %s\n",
				refstatlist.elem[i] );
			continue;
		} /*endif*/
		if  (statno >= listlth)  {
			printf( "*SHM: too many stations in reference list\n" );
			continue;
		} /*endif*/
		ut_cap( refstatlist.elem[i] );
		xmstr = XmStringCreateLtoR( refstatlist.elem[i], "" );
		n = 0;
		XtSetArg( args[n], XmNlabelString, xmstr ); n++;
		XtSetValues( w[k_widget_refstat_station_1+statno], args, n );
		XtSetSensitive( w[k_widget_refstat_station_1+statno],
			(refstatlist.elem[i][0] != '-') );
		statno++;
	} /*endwhile*/

	GpFreeTextList( &refstatlist );

} /* end of cl4_read_refstation_list */



/*---------------------------------------------------------------------------*/



void cl4_read_refstation( int number, char station[], TSyStatus *status )

/* returns name of reference station number 'number'
 *
 * parameters of routine
 * int        number;           input; number of station
 * char       station[];        output; name of station
 * TSyStatus  *status;          output; return status
 */
{
	/* local variables */
	int      listlth;                 /* number of stations to read */
	int      i;                       /* counters */
	int      statno;                  /* station counter */
	TGpTextList refstatlist;          /* list of reference stations */

	/* executable code */

	/* number of stations to read */
	listlth = k_widget_refstat_station_last - k_widget_refstat_station_1 + 1;
	*station = '\0';
	if  (number > listlth)  {
		*status = CL4E_ILLREFNUMBER;
		return;
	} /*endif*/

	GpParseTextList( GpGetString(cGpS_list_of_refstations), &refstatlist );

	/* read all stations from file */
	for  (statno=0; statno<refstatlist.numelem; statno++)  {
		if  ((statno+1) == number)  {
			strcpy( station, refstatlist.elem[statno] );
			GpFreeTextList( &refstatlist );
			return;
		} /*endif*/
	} /*endwhile*/

	GpFreeTextList( &refstatlist );

} /* end of cl4_read_refstation */



/*---------------------------------------------------------------------------*/



#define SN_WINDOW_FILE "sn_window_file.txt"



void cl4_signoise_set_windows( Widget w[], char phasename[], char currfilter[],
	TSyStatus *status )

/* sets windows for determination of signal/noise ratios
 *
 * parameters of routine
 * Widget     w[];               input; widget array
 * char       phasename[];       input; name of phase to determine S/N
 * char       currfilter[];      input; current filter applied to traces
 * TSyStatus  *status;           output; return status
 */
{
	/* local variables */
	float        timept[SN_NUM_PTS];     /* time points of S wdw and N wdw */
	TPmTrace     *trc;                   /* trace pointer */
	TPiPhase     *phase;                 /* current phase */
	TPiPhase     pseudo;                 /* pseudo-phases of time windows */
	TPiPhase     *old_pseudo;            /* pointer to old pseudo phase */
	int          i;                      /* counter */
	TSyStatus    locstat;                /* local status */
	TSyBoolean   done_something;         /* at least one phase found */

	/* executable code */

	timept[SN_NOISE_S]  = GpGetFloat( cGpF_sn_noise_start );
	timept[SN_NOISE_E]  = GpGetFloat( cGpF_sn_noise_end );
	timept[SN_SIGNAL_S] = GpGetFloat( cGpF_sn_signal_start );
	timept[SN_SIGNAL_E] = GpGetFloat( cGpF_sn_signal_end );

	if  (*phasename == 'b')  {
		*status = CL4E_DUMMY_PHASE;
		err_setcontext( " ## phase " );
		err_setcontext( phasename );
		return;
	} /*endif*/

	done_something = FALSE;

	cu_reset_phase( &pseudo );
	pseudo.source = cPiSourceAuto;

	/* loop all traces on display */
	trc = NULL;
	FOREVER  {
		trc = db_getp( trc, EP_DSPN, NULL );
		if  (trc == NULL)  break;
		locstat = cBcNoError;
		phase = PmFindPhase( trc, phasename );
		if  (phase == NULL)  continue;
		if  (strcmp(phase->filter,currfilter) != 0)  {
			*status = CL4E_FLT_MISMATCH;
			err_setcontext( " ## curr fil " );
			err_setcontext( currfilter );
			err_setcontext( ", phase fil " );
			err_setcontext( phase->filter );
			return;
		} /*endif*/
		/* define all pseudo-phases */
		for  (i=0; i<SN_NUM_PTS; i++)  {
			/* delete possibly existing pseudo phases */
			old_pseudo = PmFindPhase( trc, cl4v_sn_pnames[i] );
			if  (old_pseudo != NULL)  {
            mg_mark_one_phase( w[k_widget_draw], w[k_widget_single_draw],
					old_pseudo, trc, FALSE );
				PmRemovePhase( trc, old_pseudo, status );
				if  (SySevere(status))  return;
			} /*endif*/
			strcpy( pseudo.name, cl4v_sn_pnames[i] );
			tc_tadd( phase->onset, timept[i], pseudo.onset, status );
			if  (SySevere(status))  return;
			/*pseudo.phasetrc = trc;*/
			cu_accept_phase( w[k_widget_draw], w[k_widget_single_draw],
				&pseudo, trc, FALSE, status );
			done_something = TRUE;
		} /*endfor*/
	} /*endfor*/

	if  (!done_something)  {
		*status = CL4E_NO_PHASES;
		err_setcontext( " ## phase " );
		err_setcontext( phasename );
		return;
	} /*endif*/

} /* end of cl4_signoise_set_windows */



/*---------------------------------------------------------------------------*/



void cl4_signoise_values( char phasename[], TSyStatus *status )

/* scans all traces on display and determines S/N values where it finds
 * the pseudo-phases b1, b2, b3 and b4.
 *
 * parameters of routine
 * char       phasename[];   input; name of phase to determine S/N
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	TPmTrace     *trc;                       /* trace pointer */
	TPiPhase     *pseudo[SN_NUM_PTS];        /* pointer to pseudo-phases */
	TPiPhase     *ampl_phase;                /* pointer to phase of S/N */
	TSyStatus    locstat;                    /* local status */
	int          i;                          /* counter */
	TSyBoolean   found;                      /* pseudo phases found */
	float        *datptr;                    /* pointer to sample data */
	long         datlth;                     /* length of data in samples */
	long         smp[SN_NUM_PTS];            /* window bounds in samples */
	float        noise_min, noise_max;       /* noise values */
	float        signal_min, signal_max;     /* signal values */
	float        dt;                         /* sample distance in sec */
	TIME         atime;                      /* absolute time */
	char         trctime[cBcTimeLth+1];      /* start time of trace */
	float        tdiff;                      /* difference time */
	float        noise_ampl, signal_ampl;    /* noise and signal amplitudes */
	char         station[cBcShortStrLth+1];  /* station name */
	char         comp;                       /* component */

	/* executable code */

	if  (*phasename == 'b')  {
		*status = CL4E_DUMMY_PHASE;
		err_setcontext( " ## phase " );
		err_setcontext( phasename );
		return;
	} /*endif*/

	trc = NULL;
	FOREVER  {
		trc = db_getp( trc, EP_DSPN, NULL );
		if  (trc == NULL)  break;
		ampl_phase = PmFindPhase( trc, phasename );
		if  (ampl_phase == NULL)  continue;
		found = TRUE;
		/* get station name and component */
		db_gets( trc, ES_STATION, cBcShortStrLth, station, status );
		if  (Severe(status))  return;
		comp = db_getc( trc, EC_COMP, status );
		if  (Severe(status))  return;
		for  (i=0; i<SN_NUM_PTS; i++)  {
			pseudo[i] = PmFindPhase( trc, cl4v_sn_pnames[i] );
			if  (pseudo[i] == NULL)  {found = FALSE; break;}
		} /*endfor*/
		if  (!found)  {
			ampl_phase->signoise = 0.0;
			/*cu_update_phase( station, comp, ampl_phase, status );*/
			if  (Severe(status))  return;
			continue;
		} /*endif*/
		/* get trace info (ptr,length,dt,start-time) */
		datptr = db_getp( trc, EP_DATA, status );
		if  (SySevere(status))  return;
		datlth = db_getl( trc, EL_LENGTH, status );
		if  (SySevere(status))  return;
		dt = db_getr( trc, ER_DELTA, status );
		if  (SySevere(status))  return;
		db_gett( trc, ET_START, &atime, status );
		if  (SySevere(status))  return;
		tc_a2t( &atime, trctime, status );
		if  (SySevere(status))  return;
		/* find window bounds in samples */
		for  (i=0; i<SN_NUM_PTS; i++)  {
			tdiff = tc_tdiff( pseudo[i]->onset, trctime, status );
			if  (SySevere(status))  return;
			smp[i] = Nint( tdiff / dt );
			if  (smp[i] < 0 || smp[i] > datlth)  {
				*status = CL4E_ILLIDX;
				return;
			} /*endif*/
		} /*endfor*/
		if  (GpGetInt(cGpI_debug_level) > 3)
			printf( "SHM-dbg4: smp: %d,%d,%d,%d\n", smp[0],smp[1],smp[2],smp[3] );
		/* check order of window bounds */
		if  (smp[SN_NOISE_S] >= smp[SN_NOISE_E] ||
			smp[SN_NOISE_E] >= smp[SN_SIGNAL_S]  ||
			smp[SN_SIGNAL_S] >= smp[SN_SIGNAL_E])  {
			*status = CL4E_ILLORDER;
			return;
		} /*endif*/
		/* find noise min-max */
		noise_min = noise_max = datptr[smp[SN_NOISE_S]];
		for  (i=smp[SN_NOISE_S]+1; i <= smp[SN_NOISE_E]; i++)  {
			if  (datptr[i] > noise_max)  noise_max = datptr[i];
			if  (datptr[i] < noise_min)  noise_min = datptr[i];
		} /*endfor*/
		/* find signal min-max */
		signal_min = signal_max = datptr[smp[SN_SIGNAL_S]];
		for  (i=smp[SN_SIGNAL_S]+1; i <= smp[SN_SIGNAL_E]; i++)  {
			if  (datptr[i] > signal_max)  signal_max = datptr[i];
			if  (datptr[i] < signal_min)  signal_min = datptr[i];
		} /*endfor*/
		noise_ampl = noise_max - noise_min;
		signal_ampl = signal_max - signal_min;
		if  (noise_ampl > signal_ampl*1.0e-10)  {
			ampl_phase->signoise = signal_ampl / noise_ampl;
		} else {
			ampl_phase->signoise = 0.0;
		} /*endif*/
		/*cu_update_phase( station, comp, ampl_phase, status );*/
		if  (Severe(status))  return;
	} /*endfor*/

} /* end of cl4_signoise_values */



/*---------------------------------------------------------------------------*/



void cl4_delete_signoise_values( char phasename[], TSyStatus *status )

/* scans all phases and deletes S/N values on the 'phasename'-phases
 *
 * parameters of routine
 * char       phasename[];   input; name of phase to determine S/N
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	TPiPhaseRoot  *proot;                /* pointer to phase root */
	TPiPhaseList  *plist;                /* phase list */
	TPiPhase      *snr_phase;            /* pointer to phase of S/N */

	/* executable code */

	if  (*phasename == 'b')  {
		*status = CL4E_DUMMY_PHASE;
		err_setcontext( " ## phase " );
		err_setcontext( phasename );
		return;
	} /*endif*/

	proot = NULL;
	FOREVER  {
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		plist = PiPhaseListOfRoot( proot );
		if  (plist == NULL)  continue;
		snr_phase = PiFindPhase( plist, phasename );
		if  (snr_phase == NULL)  continue;
		snr_phase->signoise = 0.0;
	} /*endfor*/

} /* end of cl4_delete_signoise_values */



/*---------------------------------------------------------------------------*/



#define MAX_SELNO 50
	/* maximum number of selected traces */



void cl4_delete_traces( void )

/* Deletes traces of specified (selected) station and component
 * (all traces in memory)
 *
 * parameters of routine
 * none
 */
{
	/* local variables */
	char       stream[MAX_SELNO][cBcShortStrLth+1]; /* station names */
	char       cstream[cBcShortStrLth+1];           /* current stream */
	int        selno;                               /* # of selected traces */
	TPmTrace   *trc;                                /* pointer to trace */
	TSyStatus  locstat;                             /* local status */
	int        i;                                   /* counter */
	int        trcno;                               /* trace counter */

	/* executable code */

	/* get station names from traces on display */
	selno = 0;
	trcno = 1;
	trc = NULL;
	FOREVER {
		trc = db_getp( trc, EP_DSPN, &locstat );
		if  (trc == NULL)  break;
		if  (selno == MAX_SELNO)  {
			printf( "*SHM: too many selections; truncated\n" );
			break;
		} /*endif*/
		if  (ts_is_selected(trcno))  {
			locstat = cBcNoError;
			cu_get_stream_string( trc, stream[selno], NULL );
			if  (locstat == cBcNoError)  selno++;
		} /*endif*/
		trcno++;
	} /*endfor*/

	ts_clear_selections();

	/* loop all station/comp pairs */
	for  (i=0; i<selno; i++)  {
		/* loop all traces in memory */
		trc = NULL;
		FOREVER  {
			trc = db_getp( trc, EP_NEXT, &locstat );
			if  (trc == NULL)  break;
			locstat = cBcNoError;
			cu_get_stream_string( trc, cstream, NULL );
			if  (strcmp(cstream,stream[i]) == 0)
				cu_delete_trace( trc );
		} /*endfor*/
	} /*endfor*/

} /* end of cl4_delete_traces */



/*---------------------------------------------------------------------------*/



#define VESPA_EXEC "shm_exec_vespa"
#define VESPA_FNAME "vespa_signal.dat"



void cl4_vespa_export( Widget w[], TSyStatus *status )

/* Writes vespagram traces to ASCII file and call display programs
 *
 * parameters of routine
 * Widget     w[];          input; widget array
 * TSyStatus  *status;      output; return status
 */
{
	/* local variables */
	char     fname[cBcFileLth+1];  /* name of output file */
	FILE     *fp;                  /* pointer to output file */
	TPmTrace *trc;                 /* trace pointer */
	TSyStatus locstat;             /* local status */
	int      trcno;                /* number of traces */
	int      maxtrclth;            /* maximum trace length */
	int      trclth;               /* current trace length */
	float    *smp;                 /* pointer to sample data */
	int      zeros_pad1;           /* number of zeros to pad before data */
	int      zeros_pad2;           /* number of zeros to pad after data */
	float    dt;                   /* sample distance in sec */
	float    wdw1, wdw2;           /* time window in sec */
	float    torig;                /* time origin */
	int      start_smp;            /* start sample to write out */
	int      i;                    /* counter */
	char     cmd[cBcLongStrLth+1]; /* shell command */
	int      colnum;               /* number of colors */
	float    slolo, slohi;         /* slowness window */
	float    timewdw;              /* time window in sec */
	char     abstime[cBcTimeLth+1];/* absolute time of window start */
	TIME     atime;                /* absolute time in binary format */

	/* executable code */

	/* build filename and open */
	strcpy( fname, shd_scratch );
	strcat( fname, VESPA_FNAME );
	fp = sy_fopen( fname, "w" );
	if  (fp == NULL)  {
		*status = CL4E_OPENWRITE;
		return;
	} /*endif*/

	/* loop all traces on display (find number of traces and max length) */
	trcno = maxtrclth = 0;
	trc = NULL;
	dt = 0.0;
	FOREVER {
		locstat = cBcNoError;
		trc = db_getp( trc, EP_DSPN, &locstat );
		if  (trc == NULL)  break;
		trcno++;
		trclth = db_getl( trc, EL_DSPCNT, status );
		if  (SySevere(status))  {sy_fclose(fp); return;}
		if  (trclth > maxtrclth)  maxtrclth = trclth;
		if  (dt == 0.0)  dt = db_getr( trc, ER_DELTA, status );
		if  (SySevere(status))  {sy_fclose(fp); return;}
	} /*endfor*/
	dm_get_timewdw( &wdw1, &wdw2 );
	timewdw = (float)maxtrclth * dt;

	/* read out dialog boxes */
	sscanf( cu_get_string(w[k_widget_fk_colnum_text]), "%d", &colnum );
	sscanf( cu_get_string(w[k_widget_vespa_slolo_text]), "%f", &slolo );
	sscanf( cu_get_string(w[k_widget_vespa_slohi_text]), "%f", &slohi );

	/* write info */
	fprintf( fp, "!* time wdw (sec): %6.2f\n", timewdw );
	fprintf( fp, "!* start slowness: %6.2f\n", slolo );
	fprintf( fp, "!* end slowness  : %6.2f\n", slohi );

	/* write size of matrix */
	fprintf( fp, "%d %d\n", maxtrclth, trcno );

	/* again loop all traces and write samples */
	*abstime = '\0';
	trc = NULL;
	FOREVER {
		locstat = cBcNoError;
		trc = db_getp( trc, EP_DSPN, &locstat );
		if  (trc == NULL)  break;
		smp = db_getp( trc, EP_DATA, status );
		if  (SySevere(status))  {sy_fclose(fp); return;}
		trclth = db_getl( trc, EL_DSPCNT, status );
		if  (SySevere(status))  {sy_fclose(fp); return;}
		torig = db_getr( trc, ER_TORIG, status );
		if  (SySevere(status))  {sy_fclose(fp); return;}
		start_smp = Nint((wdw1-torig)/dt);
		if  (*abstime == '\0')  {
			db_gett( trc, ET_START, &atime, status );
			if  (SySevere(status))  {sy_fclose(fp); return;}
			tc_aadd( &atime, wdw1-torig, &atime );
			tc_a2t( &atime, abstime, status );
			if  (SySevere(status))  {sy_fclose(fp); return;}
		} /*endif*/
		if  (start_smp < 0)  {
			zeros_pad1 = -start_smp;
			start_smp = 0;
		} else {
			zeros_pad1 = 0;
		} /*endif*/
		zeros_pad2 = maxtrclth - trclth - zeros_pad1;
		if  (zeros_pad2 < 0)  {
			printf( "*SHM: program bug in cl4_vespa_export: zeros_pad2 %d\n",
				zeros_pad2 );
			trclth += zeros_pad2;
			zeros_pad2 = 0;
		} /*endif*/
		for  (i=0; i<zeros_pad1; i++)
			fprintf( fp, "0.0\n" );
		for  (i=start_smp; i<(start_smp+trclth); i++)
			fprintf( fp, "%f\n", smp[i] );
		for  (i=0; i<zeros_pad2; i++)
			fprintf( fp, "0.0\n" );
	} /*endfor*/

	sy_fclose( fp );

	if  (GpGetInt(cGpI_debug_level) > 2)
		printf( "SHM-dbg3: file %s written\n", fname );

	sprintf( cmd, "%s%s %s %d %s %f %f %f &",
		GpGetString(cGpS_defpath_extprog), VESPA_EXEC, fname, colnum,
		abstime, timewdw, slolo, slohi );
	system( cmd );

} /* end of cl4_vespa_export */



/*---------------------------------------------------------------------------*/



BOOLEAN cl4_station_is_selected( char station[] )

/* Returns whether station is selected at least one time
 *
 * parameters of routine
 * char       station[];       input; station name
 */
{
	/* local variables */
	TPmTrace *trc;                 /* pointer to trace */
	TSyStatus locstat;             /* return status */
	int      trccnt;               /* trace counter */
	char     cstat[cBcLineLth+1];  /* current station name */

	/* executable code */

	trccnt = 1;
	trc = NULL;
	FOREVER {
		locstat = cBcNoError;
		trc = db_getp( trc, EP_DSPN, &locstat );
		if  (trc == NULL)  break;
		db_gets( trc, ES_STATION, cBcLineLth, cstat, &locstat );
		if  (SySevere(&locstat))  {trccnt++; continue;}
		if  (strcmp(station,cstat) != 0)  {trccnt++; continue;}
		if  (ts_is_selected(trccnt))  return TRUE;
		trccnt++;
	} /*endfor*/

	return FALSE;

} /* end of cl4_station_is_selected */



/*---------------------------------------------------------------------------*/



void cl4_onset_pick( Widget w[], MGT_DSPCTRL *ctrl, char phasename[],
	TSyBoolean delete, TSyStatus *status )

/* Calls automatic onset picker
 *
 * parameters of routine
 * Widget     w[];            input; widget array
 * MGT_DSPCTRL *ctrl;         input; display control
 * char       phasename[];    input; name of autopick-phase
 * TSyBoolean delete;         input; delete existing phases 'phasename'
 * TSyStatus  *status         output; return status
 */
{
	/* local variables */
	TPmTrace *trc;                    /* pointer to trace */
	TSyStatus locstat;                /* local status */
	float     time_lo, time_hi;       /* time window */
	TSyBoolean ok;                    /* time window ok */
	TPiPhase  pickphase;              /* pick phase */
	char      timestr[cBcLineLth+1];  /* onset time */
	char      shcmd[cBcLineLth+1];    /* SH command */

	/* executable code */

#ifdef XXX
	/* should be implemented, too */
	mg_get_drag_trace( &ref_trace, &ok );
	if  (!ok)  {*status = CLE_NODRAGWDW; return;}
#endif

	mg_get_drag_window( &time_lo, &time_hi, &ok );
	if  (!ok)  {*status = CL4E_NODRAGWDW; return;}

	/* delete all phases of name "phasename" from all traces if requested */
	if  (delete)  cu_delete_all_phases( w[k_widget_draw],
		w[k_widget_single_draw], phasename, TRUE );

	sprintf( shcmd, "shm_cmd_onsetpick %7.2f %7.2f %s %s %s", time_lo, time_hi,
		cu_get_string(w[k_widget_opick_duration_text]),
		cu_get_string(w[k_widget_opick_break_text]),
		cu_get_string(w[k_widget_opick_thresh_text]) );
	if  (GpGetInt(cGpI_debug_level) > 2)
		printf( "SHM-dbg3: executing: %s\n", shcmd );
	mx_exec_sh( w[k_widget_draw], ctrl, shcmd );

	cu_reset_phase( &pickphase );
	if  (strlen(phasename) > cPiMaxPhaseLth)  {*status = CL4E_STROVFL; return;}
	strcpy( pickphase.name, phasename );
	pickphase.source = cPiSourceAuto;

	/* loop all traces and display picks */
	trc = NULL;
	FOREVER {
		locstat = cBcNoError;
		trc = db_getp( trc, EP_DSPN, &locstat );
		if  (trc == NULL)  break;
		*timestr = '\0';
		db_gets( trc, ES_OPINFO, cBcLineLth, timestr, &locstat );
		if  (*timestr == '\0' || SySevere(&locstat))  continue;
		strcpy( pickphase.onset, timestr );
		cu_accept_phase( w[k_widget_draw], w[k_widget_single_draw], &pickphase,
			trc, FALSE, status );
		if  (SySevere(status))  return;
	} /*endfor*/

} /* end of cl4_onset_pick */



/*---------------------------------------------------------------------------*/



void cl4_fix_beam( Widget w[], MGT_DSPCTRL *ctrl, char filter[],
	TSyStatus *status )

/* Throws out all traces but the beam trace and renames this to B-<refstation>.
 *
 * parameters of routine
 * Widget     w[];            input; widget array
 * MGT_DSPCTRL *ctrl;         input; display control
 * char       filter[];       input; name of current filter
 * TSyStatus  *status         output; return status
 */
{
	/* local variables */
	char     cmd[cBcLineLth+1];      /* SH command */
	int      trcnum;                 /* number of traces on display */
	TPmTrace   *trc;                 /* pointer to trace */
	char     statname[cBcLineLth+1]; /* station name */

	/* executable code */

	/* check for unfiltered traces */
	if  (*filter != '\0')  {
		printf( "*SHM: current filter %s, unfiltered required\n", filter );
		*status = CL4E_NO_FILTER_REQ;
		return;
	} /*endif*/

	trcnum = mg_dsptrcs();
	if  (trcnum < 3)  {
		*status = CL4E_TOO_LESS_TRCS;
		return;
	} /*endif*/
	trc = mg_trcptr( trcnum );
	db_gets( trc, ES_STATION, cBcLineLth, statname, status );
	if  (SySevere(status))  return;
	if  (strcmp(statname,"BEAM") != 0)  {
		printf( "*SHM: no beam trace found, abort command\n" );
		*status = CL4E_NO_BEAM;
		return;
	} /*endif*/

	mx_exec_sh( w[k_widget_draw], ctrl, "nr" );
	sprintf( cmd, "del 1-%d", trcnum-1 );
	mx_exec_sh( w[k_widget_draw], ctrl, cmd );
	sprintf( cmd, "set 1 station b@%s", GpGetString(cGpS_refstation) );
	mx_exec_sh( w[k_widget_draw], ctrl, cmd );
	mx_exec_sh( w[k_widget_draw], ctrl, "rd" );

} /* end of cl4_fix_beam */



/*---------------------------------------------------------------------------*/



void cl4_resid_corr( TSyBoolean setval, float slowness, float azimuth,
	TSyStatus *status )

/* computes or deletes residual correction for all phases picked
 *
 * parameters of routine
 * TSyBoolean setval;         input; TRUE=compute values, FALSE=delete values
 * float      slowness;       input; slowness in s/deg
 * float      azimuth;        input; back-azimuth in deg
 * TSyStatus  *status;        output; return status
 */
{
	/* local variables */
	TPiPhaseRoot *proot;             /* pointer to phase root */
	TPiPhaseList *phaselist;         /* phase list of trace */
	TPiPhase *t_phase;               /* phase pointer */
	int      p;                      /* phase counter */
	char     station[cBcLineLth+1];  /* station name */

	/* executable code */

	proot = NULL;

	FOREVER  {

		/* get phase list */
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		strcpy( station, PiStationOfRoot(proot) );
		phaselist = PiPhaseListOfRoot( proot );
		if  (phaselist == NULL)  continue;
		/* loop all phases of trace */
		for  (p=0; p<PiPhaseListLength(phaselist); p++)  {
			t_phase = PiGetPhase( phaselist, p, status );
			if  (Severe(status))  return;
			if  (t_phase == NULL)  break;
			if  (t_phase->source == cPiSourceTheo)  continue;
			if  (t_phase->name[0] == 'b')  continue;
			t_phase->resid_corr = setval ? RsResidual( station, t_phase->name,
				slowness, azimuth, status ) : cPiResidCorrEmpty;
			if  (Severe(status))  return;
		} /*endfor*/

	} /*endfor*/

} /* end of cl4_compute_resid_corr */



/*---------------------------------------------------------------------------*/


#define SETUPFILE "autodrm_request_setup.dat"
#define OUTFILE "autodrm_request_mail.txt"


void cl4_adrm_request( Widget w[], char stime[], float seclth,
	char datafile[], TSyStatus *status )

/* Manages requests to AutoDRMs
 *
 * parameters of routine
 * Widget     w[];       input; Widget array
 * char       stime[];   input; start time
 * float      seclth;    input; time window in sec
 * char       datafile[];output; data file created
 * TSyStatus  *status;   output; return status
 */
{
	/* local variables */
	static int msgcnt=0;                /* message counter */
	int      id;                        /* AutoDRM ID */
	char     setupfile[cBcFileLth+1];   /* setup file */
	char     *env;                      /* pointer to environment */
	FILE     *fp;                       /* pointer to setup file */
	char     line[cBcLineLth+1];        /* current line of file */
	char     idstr[cBcLineLth+1];       /* ID string (DRM-?? ) */
	int      idlth;                     /* length of ID string */
	char     outfile[cBcFileLth+1];     /* name of output file */
	FILE     *out;                      /* pointer to output file */
	char     email[cBcLineLth+1];       /* autodrm e-mail address */
	int      i;                         /* counter */
	char     cmd[cBcLineLth+1];         /* substitute command */
	NTIME    st, et;                    /* start and end time of req interval */
	char     msgid[cBcLineLth+1];       /* message id */
	int      wait;                      /* wait time */
	char     mailfile[cBcFileLth+1];    /* name of mailfile for arriving mail */
	char     mailhost[cBcLineLth+1];    /* name of mailhost */
	char     *edtext;                   /* edit text */

	/* executable code */

	/* find AutoDRM ID by finding the first selected button */
	for  (id=0; id<CUC_ADRM_NUMBER; id++)  {
		if  (XmToggleButtonGetState(w[k_widget_adrm_1+id]))  {
			break;
		} /*endif*/
	} /*endfor*/
	if  (id >= CUC_ADRM_NUMBER)  {
		*status = CL4E_ILL_ADRM_ID;
		return;
	} /*endif*/

	/* make ID string */
	sprintf( idstr, "DRM-%02d ", id+1 );
	idlth = strlen( idstr );

	/* make message id */
	sprintf( msgid, "%s%03d", id_shv, ++msgcnt );

	/* prepare numerical start and end times */
	edtext = cu_get_string( w[k_widget_adrm_reqtime_text] );
	if  (strcmp(edtext,"from-display") == 0)  {
		tc_t2n( stime, &st, status );
		if  (SySevere(status))  return;
		tc_nadd( &st, seclth, &et, status );
		if  (SySevere(status))  return;
	} else {
		tc_t2n( edtext, &st, status );
		if  (SySevere(status))  return;
		edtext = cu_get_string( w[k_widget_adrm_readlth_text] );
		if  (sscanf(edtext,"%f",&seclth) != 1)  {
			*status = CL4E_ILL_READLTH;
			return;
		} /*endif*/
		seclth *= 60.0;
		tc_nadd( &st, seclth, &et, status );
		if  (SySevere(status))  return;
	} /*endif*/

	/* get name of setup file, first search private directory, then $SH_INPUTS */
	if  (strlen(GpGetString(cGpS_defpath_userdir))+strlen(SETUPFILE)
		> cBcFileLth)  {
		*status = CL4E_STROVFL;
		return;
	} /*endif*/
	strcpy( setupfile, GpGetString(cGpS_defpath_userdir) );
	strcat( setupfile, SETUPFILE );
	fp = fopen( setupfile, "r" );
	if  (fp == NULL)  {
		if  (strlen(shd_inputs)+strlen(SETUPFILE) > cBcFileLth)  {
			*status = CL4E_STROVFL;
			return;
		} /*endif*/
		strcpy( setupfile, shd_inputs );
		strcat( setupfile, SETUPFILE );
	} else {
		fclose( fp );
	} /*endif*/

	/* name of output file */
	env = getenv( "SH_SCRATCH" );
	if  (env == NULL)  {
		*status = CL4E_NO_ENV;
		return;
	} /*endif*/
	if  (strlen(env)+strlen(OUTFILE) > cBcFileLth)  {
		*status = CL4E_STROVFL;
		return;
	} /*endif*/
	strcpy( outfile, env );
	strcat( outfile, OUTFILE );

	fp = fopen( setupfile, "r" );
	if  (fp == NULL)  {
		*status = CL4E_OPENREAD;
		return;
	} /*endif*/
	out = fopen( outfile, "w" );
	if  (out == NULL)  {
		*status = CL4E_OPENWRITE;
		fclose( fp );
		return;
	} /*endif*/

	wait = 0;
	*mailfile = '\0';
	*mailhost = '\0';
	strcpy( datafile, "$SH_SCRATCH/adrmreq.gse" );

	/* read through file */
	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		if  (strncmp(line,idstr,idlth) == 0)  {
			if  (strlen(line) <= idlth+2)  continue;
			printf( "%s", line );
			if  (line[idlth] == 'c')  {
				fprintf( out, "%s", line+idlth+2 );
			} else if  (line[idlth] == 'e')  {
				strcpy( email, line+idlth+2 );
				i = strlen( email ) - 1;
				if  (email[i] == '\n')  email[i] = '\0';
			} else if  (line[idlth] == 'f')  {
				strcpy( mailfile, line+idlth+2 );
				i = strlen( mailfile ) - 1;
				if  (mailfile[i] == '\n')  mailfile[i] = '\0';
			} else if  (line[idlth] == 'o')  {
				strcpy( datafile, line+idlth+2 );
				i = strlen( datafile ) - 1;
				if  (datafile[i] == '\n')  datafile[i] = '\0';
			} else if  (line[idlth] == 'h')  {
				strcpy( mailhost, line+idlth+2 );
				i = strlen( mailhost ) - 1;
				if  (mailhost[i] == '\n')  mailhost[i] = '\0';
			} else if  (line[idlth] == 'w')  {
				if  (sscanf(line+idlth+2,"%d",&wait) != 1)  {
					printf( "*SHM: cannot read wait time: \n%s", line );
					continue;
				} /*endif*/
			} else if  (line[idlth] == 's')  {
				strcpy( cmd, line+idlth+2 );
				i = strlen( cmd ) - 1;
				if  (cmd[i] == '\n')  cmd[i] = '\0';
				if  (strcmp(cmd,"<time>") == 0)  {
					fprintf( out, "TIME %4d/%02d/%02d %02d:%02d:%02d ",
						st.year, st.month, st.day, st.hour, st.min, st.sec );
					fprintf( out, "TO %4d/%02d/%02d %02d:%02d:%02d\n", 
						et.year, et.month, et.day, et.hour, et.min, et.sec );
				} else if  (strcmp(cmd,"<msg-id>") == 0)  {
					fprintf( out, "MSG_ID %s SHMREQ\n", msgid );
				} else {
					printf( "*SHM: invalid keyword in AutoDRM substitution: %s\n",
						cmd );
					continue;
				} /*endif*/
			} /*endif*/
		} /*endif*/
	} /*endwhile*/

	fclose( fp );
	fclose( out );

	if  (*mailhost == '\0')  {
		sprintf( cmd, "mailx %s <%s\n", email, outfile );
	} else {
		sprintf( cmd, "ssh %s mailx %s <%s\n", mailhost, email, outfile );
	} /*endif*/
	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: autodrm request: %s\n", cmd );
	system( cmd );

	/* exit if we do not wait for the mail */
	if  (wait == 0 || *mailfile == '\0')  return;

	sprintf( cmd,
		"$SH_UTIL/shm_exec_read_adrm_mail.csh %s %s %d %s %s\n",
		msgid, mailfile, wait, datafile, mailhost );
	printf( "%s\n", cmd );
	system( cmd );

	fp = fopen( datafile, "r" );
	if  (fp == NULL)  {
		printf( "*SHM: no GSE file created\n" );
		*datafile = '\0';
	} else {
		printf( "*SHM: GSE file created: please read %s\n", datafile );
		fclose( fp );
	} /*endif*/

} /* end of cl4_adrm_request */



/*---------------------------------------------------------------------------*/



void cl4_adrm_configure( Widget w[], TSyStatus *status )

/* Configures Autodrm selection box by calling editor on the local setup file
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * TSyStatus  *status;    output; return status
 */
{
	/* local variables */
	char     setupfile[cBcFileLth+1];       /* name of setup file */
	FILE     *fp;                           /* pointer to setup file */
	char     cmd[cBcLineLth+1];             /* command line */

	/* executable code */

	/* get name of setup file in private directory */
	if  (strlen(GpGetString(cGpS_defpath_userdir))+strlen(SETUPFILE)
		> cBcFileLth)  {
		return;
	} /*endif*/
	strcpy( setupfile, GpGetString(cGpS_defpath_userdir) );
	strcat( setupfile, SETUPFILE );
	fp = fopen( setupfile, "r" );
	if  (fp == NULL)  {
		*status = CL4E_NO_ADRM_SETUP;
		return;
	} /*endif*/
	fclose( fp );

	sprintf( cmd, "%s %s", GpGetString(cGpS_texteditor), setupfile );
	if  (GpGetInt(cGpI_debug_level) > 2)
		printf( "SHM-dbg3: exec: %s\n", cmd );
	system( cmd );

	cl4_init_adrm_buttons( w );

} /* end of cl4_adrm_configure */



/*---------------------------------------------------------------------------*/



void cl4_init_adrm_buttons( Widget w[] )

/* Initializes AutoDRM buttons
 *
 * parameters of routine
 * Widget     w[];   input; widget array
 */
{
	/* local variables */
	char     setupfile[cBcFileLth+1];   /* setup file */
	FILE     *fp;                       /* pointer to setup file */
	int      i, n;                      /* counters */
	char     line[cBcLineLth+1];        /* current line of file */
	XmString xmstr;              /* Xm string */
	Arg      args[2];            /* argument list */

	/* executable code */

	/* disable buttons by default */
	for  (i=0; i<CUC_ADRM_NUMBER; i++)
		XtSetSensitive( w[k_widget_adrm_1+i], FALSE );

	/* get name of setup file, first search private directory, then $SH_INPUTS */
	if  (strlen(GpGetString(cGpS_defpath_userdir))+strlen(SETUPFILE)
		> cBcFileLth)  {
		return;
	} /*endif*/
	strcpy( setupfile, GpGetString(cGpS_defpath_userdir) );
	strcat( setupfile, SETUPFILE );
	fp = fopen( setupfile, "r" );
	if  (fp == NULL)  {
		if  (strlen(shd_inputs)+strlen(SETUPFILE) > cBcFileLth)  {
			return;
		} /*endif*/
		strcpy( setupfile, shd_inputs );
		strcat( setupfile, SETUPFILE );
		fp = fopen( setupfile, "r" );
		if  (fp == NULL)  return;
	} /*endif*/

	/* read through file */
	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		if  (strncmp("DRM-",line,4) != 0)  continue;
		if  (line[7] != 't')  continue;
		i = strlen( line ) - 1;
		if  (line[i] == '\n')  line[i] = '\0';
		sscanf( line+4, "%d", &i );
		i--;
		if  (i < 0 || i >= CUC_ADRM_NUMBER)  continue;
		XtSetSensitive( w[k_widget_adrm_1+i], TRUE );
		xmstr = XmStringCreateLtoR( line+9, "" );
		n = 0;
		XtSetArg( args[n], XmNlabelString, xmstr ); n++;
		XtSetValues( w[k_widget_adrm_1+i], args, n );
	} /*endwhile*/

	fclose( fp );

} /* end of cl4_init_adrm_buttons */



/*---------------------------------------------------------------------------*/

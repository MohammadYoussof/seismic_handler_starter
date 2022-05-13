
/* file cbutil.c
 *      ========
 *
 * version 100, 11-Jan-2007
 *
 * utility routines for callbacks
 * K. Stammler, 3-Mar-93
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
#include <Xm/ToggleB.h>
#include <Xm/Text.h>  /* because XmTextSetString is not defined in Xm.h */
#include <time.h>
#include <math.h>
#undef BC_DEFINE_TRUE_FALSE
#include BC_SYSBASE
#include BC_TCUSRDEF
#include "infoidx.h"
#include "glusrdef.h"
#include "fctxmt.h"
#include "sherrors.h"
#include "earthloc.h"
#include "ptusrdef.h"
#include "ffusrdef.h"
#include "erusrdef.h"
#include "utusrdef.h"
#include "motifgraph.h"
#include "seismics.h"
#include "cbutil.h"
#include "phaseinf.h"
#include "phasemgr.h"
#include "shvars.h"
#include "shm_widgets.h"
#include "globalparams.h"


#define WAIT_UNIT 10000



/* global variables */
static Widget  cuv_alert_widget;      /* widget ID of alert box */
static Widget  cuv_multipli_value;    /* widget ID for multiplic. box (value) */
static Widget  cuv_multipli_box;      /* widget ID for multiplic. box */
static Widget  cuv_multipli_edit;     /* widget ID for multiplic. box (edit) */
static TPiPhase *cuv_multipli_phase=NULL; /* pointer to multiplication value */
static TPiPhase *cuv_multipli_trc=NULL;   /* pointer to multiplication value */
static int     cuv_execflags=0;       /* execution flag memory */




/* prototype of get_geo */
void xxx_get_geo(float *elat, float *elon, int *num_id, char *qnam);

/* prototypes of local routines */
static void cuh_strip_blanks( char str[] );
static void cuh_wait( int cnt );
static void cuh_phase_onset( char phase[], void *trc, char **onset,
	float *residcorr );
static GLT_STATINF *cu_get_statinf( TPmTrace *trc, TSyStatus *status );



/*---------------------------------------------------------------------------*/



void cu_reset_phase( TPiPhase *phase )

/* resets phase info to default values
 *
 * parameters of routine
 * TPiPhase   *phase;      output; structure to reset
 */
{
	/* executable code */

	phase->onset[0] = '\0';
	phase->onset_acc_l = cPiAccEmpty;
	phase->onset_acc_r = cPiAccEmpty;
	phase->spec = cPiSpecEmergent;
	phase->sign = 0;
	phase->source = cPiSourceUndefined;
	phase->quality = GpGetInt( cGpI_default_quality );
	phase->weight = cu_quality2weight( phase->quality );
	phase->ampl = 0.0;
	phase->ampl_time = 0.0;
	phase->ampl_veloc = 0.0;
	phase->ampl_displ = 0.0;
	phase->period = 0.0;
	phase->magnitude = 0.0;
	phase->magn_source = cPiMagnUndefined;
	phase->ap_source = cPiApsrcUndefined;
	phase->bb.bbampl = 0.0;
	phase->bb.bbperiod = 0.0;
	phase->bb.bbampl_time = 0.0;
	phase->bb.mbb = 0.0;
	phase->signoise = 0.0;
	phase->resid = 0.0;
	phase->resid_type = cPiResidUndefined;
	phase->resid_corr = cPiResidCorrEmpty;
	phase->flags = 0;
	phase->filter[0] = '\0';
	phase->comment[0] = '\0';
	phase->reliable = TRUE;

} /* end of cu_reset_phase */



/*---------------------------------------------------------------------------*/



void cu_reset_parameters( CUT_PARAMS *par )

/* resets parameters to default values
 *
 * parameters of routine
 * CUT_PARAMS    *par;     parameters to be reset
 */
{
	/* executable code */

	par->distance = 0.0;
	par->dist_unit = CUC_DIST_UNIT_DEG;
	par->b_slowness = 0.0;
	par->b_slowness_err = 0.0;
	par->b_azimuth = 0.0;
	par->b_azimuth_err = 0.0;
	par->l_slowness = 0.0;
	par->l_azimuth = 0.0;
	par->depth = GpGetFloat( cGpF_default_depth );
	par->depth_type = GpGetInt( cGpI_default_depth_type );
	par->origin[0] = '\0';
	par->source_lat = 0.0;
	par->source_lon = 0.0;
	par->source_type = GpGetInt( cGpI_default_event_type );
	par->center_lat = 0.0;
	par->center_lon = 0.0;
	par->ref_lat = 0;
	par->ref_lon = 0;
	par->reg_id = 0;
	par->regname[0] = '\0';
	par->loc_meth = CUC_LOCMETH_UNDEFINED;
	par->loc_quality = GpGetInt( cGpI_default_loc_quality );
	par->loc_addpar[0] = '\0';
	par->velmod[0] = '\0';
	par->momten[0] = '\0';
	par->momten_descr[0] = '\0';
	par->fps_angles[0] = '\0';
	par->fps_descr[0] = '\0';
	par->mu_descr[0] = '\0';
	par->cornerfreq = 0.0;
	par->lowfreqlevel = 0.0;
	par->m0 = 0.0;
	par->stations_used = 0;
	par->phase[0] = '\0';
	if  (par->comment != NULL)
		sy_deallocmem( par->comment );
	par->comment = NULL;
	par->source[0] = '\0';
	par->soft_change = FALSE;
	par->flags = GpGetInt( cGpI_default_phase_flags );
	par->err.lat = 0.0;
	par->err.lon = 0.0;
	par->err.dep = 0.0;
	par->err.orig = 0.0;
	par->err.smajor = 0.0;
	par->err.sminor = 0.0;
	par->err.majstrike = 0.0;
	par->err.dist_unit = CUC_DIST_UNIT_DEG;

	PiSbClearAll( &(par->slowbox) );

	cu_clear_exec_flags();

} /* end of cu_reset_parameters */



/*--------------------------------------------------------------------------*/



void cu_reset_paramsets( CUT_PARAMSETS *parset )

/* resets all parameter sets
 *
 * parameters of routine
 * CUT_PARAMSETS *parset;        output; parameter to be reset
 */
{
	/* local variables */
	int      i;         /* counter */

	/* executable code */

	/* parset->state = CUC_PARSTATE_PROCESS; */
	parset->evid = 0;
	parset->parno = 0;
	for  (i=0; i<CUC_MAXPARSET; i++)
		cu_reset_parameters( parset->par+i );

} /* end of cu_reset_paramsets */



/*--------------------------------------------------------------------------*/



void cu_set_read_time( Widget date, Widget time, float incr, TSyStatus *status )

/* increments read time by "incr" seconds
 *
 * parameters of routine
 * Widget     date, time;    input; Widget ID's for date and time text
 * float      incr;          input; time increment in seconds
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	char     *str;                   /* string pointer */
	char     timestr[BC_LINELTH+1];  /* time string */
	NTIME    ntime;                  /* numeric time */
	int      loopcnt;                /* loop counter */
	static int last_timestamp=0;     /* last timestamp */
	/* QueryPointer returns */
	Window   root, child;
	int      root_x, root_y;
	int      win_x, win_y;
	unsigned mask;

	/* executable code */

	loopcnt = 0;

	do  {

		str = cu_get_string( date );
		strcpy( timestr, str );
		cuh_strip_blanks( timestr );
		strcat( timestr, "_" );
		str = cu_get_string( time );
		strcat( timestr, str );
		cuh_strip_blanks( timestr );
		if  (incr == CUC_MONTH_INC)  {
			tc_t2n( timestr, &ntime, status );
			if  (Severe(status))  return;
			if  (++(ntime.month) > 12)  {
				ntime.month = 1;
				(ntime.year)++;
			} /*endif*/
			tc_n2t( &ntime, timestr, status );
		} else if  (incr == CUC_MONTH_DEC)  {
			tc_t2n( timestr, &ntime, status );
			if  (Severe(status))  return;
			if  (--(ntime.month) <= 0)  {
				ntime.month = 12;
				(ntime.year)--;
			} /*endif*/
			tc_n2t( &ntime, timestr, status );
		} else if  (incr == CUC_YEAR_INC)  {
			tc_t2n( timestr, &ntime, status );
			if  (Severe(status))  return;
			(ntime.year)++;
			tc_n2t( &ntime, timestr, status );
		} else if  (incr == CUC_YEAR_DEC)  {
			tc_t2n( timestr, &ntime, status );
			if  (Severe(status))  return;
			(ntime.year)--;
			tc_n2t( &ntime, timestr, status );
		} else {
			tc_tadd( timestr, incr, timestr, status );
		} /*endif*/
		if  (Severe(status))  return;
		timestr[11] = '\0';
		timestr[20] = '\0';
		cu_set_string( date, timestr );
		cu_set_string( time, timestr+12 );

		XQueryPointer( XtDisplay(date), XtWindow(date), &root, &child,
			&root_x, &root_y, &win_x, &win_y, &mask );
		if  (!(Button1Mask & mask))  break;

		if  (loopcnt == 0)  {
			cuh_wait( 5*WAIT_UNIT );
		} else if  (loopcnt >= 1)  {
			cuh_wait( WAIT_UNIT );
		} /*endif*/

		XQueryPointer( XtDisplay(date), XtWindow(date), &root, &child,
			&root_x, &root_y, &win_x, &win_y, &mask );

		loopcnt++;

	}  while  (Button1Mask & mask);

} /* end of cu_set_read_time */



/*-----------------------------------------------------------------------*/



void cu_set_read_time_file( Widget w[], int mode, char autoevt[],
	TSyStatus *status )

/* reads next/previous time from file
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * int        mode;       input; search mode
 * char       autoevt[];  output; name of evt-file
 * TSyStatus  *status;    output; return status
 */
{
	/* local variables */
	static int curr_line=0;         /* current line count */
	FILE     *fp;                   /* file pointer */
	char     line[cBcLongStrLth+1]; /* current line */
	char     *c, *cp, *cc;          /* pointers to char */
	int      i;                     /* line counter */
	char     timestr[BC_TIMELTH+1]; /* time string */
	char     file[BC_FILELTH+1];    /* input file */
	int      slen;                  /* string length */
	int      loopcnt;               /* loop counter */
	/* QueryPointer returns */
	Window   root, child;
	int      root_x, root_y;
	int      win_x, win_y;
	unsigned mask;

	/* executable code */

	/* build filename */
	c = cu_get_string( w[k_widget_read_grsn_eventfile_text] );
	if  (strlen(c) > BC_FILELTH)  {
		*status = CUE_STROVFL;
		return;
	} /*endif*/

	/* check whether filename is complete */
	fp = sy_fopen( c, "r" );
	if  (fp == NULL)  {
		/* no directory specified, take default directory */
		slen = (int)strlen( GpGetString(cGpS_defpath_events) );
		if  (slen+strlen(c)+1 > BC_FILELTH)  {
			*status = CUE_STROVFL;
			return;
		} /*endif*/
		strcpy( file, GpGetString(cGpS_defpath_events) );
		if  (file[slen-1] != '/')
			strcat( file, "/" );
		strcat( file, c );
		/* open file */
		fp = sy_fopen( file, "r" );
		if  (fp == NULL)  {
			printf( "*SHM: file %s not found\n", c );
			return;
		} /*endif*/
	} else {
		/* directory is there, take it as it is */
		strcpy( file, c );
	} /*endif*/

	loopcnt = 0;
	do  {

		fseek( fp, 0, 0 );

		if  (mode == CUC_LIST_NEXT)  {
			curr_line++;
		} else {
			curr_line--;
		} /*endif*/
		if  (curr_line <= 0)  curr_line = 1;

		i = 0;
		*line = '\0';
		while  (i < curr_line)  {
			if  (fgets(line,cBcLongStrLth,fp) == NULL)
				curr_line--;
			if  (*line != '!')  i++;
		} /*endwhile*/

		sprintf( timestr, "%d", curr_line );
		cu_set_string( w[k_widget_read_grsn_eventno_text], timestr );

		/* read and then remove remainder of line (after time string) */
		*autoevt = '\0';
		if  (*line == '\0')  {sy_fclose( fp ); return;}
		i = (int)strlen( line ) - 1;
		if  (line[i] == '\n')  line[i] = '\0';
		c = line;
		while  (*c != '\0' && *c == ' ')
			c++;
		cp = c;
		while  (*cp != '\0' && *cp != ' ')
			cp++;
		/* remove remainder and put it to autoevt */
		if  (*cp != '\0')  {
			*cp++ = '\0';
			while  (*cp != '\0' && *cp == ' ')  cp++;
			cc = cp;
			while  (*cc != ' ' && *cc != '\0')  cc++;
			if  (*cp != '\0')   {
				if  ((cc-cp) > BC_FILELTH)  {
					*status = CUE_STROVFL;
					sy_fclose( fp );
					return;
				} /*endif*/
				sscanf( cp, "%s", autoevt );
			} /*endif*/
		} /*endif*/

		/* reformat time string */
		tc_tadd( c, 0.0, timestr, status );
		if  (Severe(status)) {sy_fclose( fp ); return;}

		timestr[11] = '\0';
		timestr[20] = '\0';
		cu_set_string( w[k_widget_read_grsn_date], timestr );
		cu_set_string( w[k_widget_read_grsn_time], timestr+12 );

		if  (loopcnt == 0)  {
			cuh_wait( 5*WAIT_UNIT );
		} else if  (loopcnt >= 1)  {
			cuh_wait( WAIT_UNIT );
		} /*endif*/

		XQueryPointer( XtDisplay(w[k_widget_read_grsn_date]),
			XtWindow(w[k_widget_read_grsn_date]), &root, &child,
			&root_x, &root_y, &win_x, &win_y, &mask );

		loopcnt++;

	}  while  (Button1Mask & mask);

	sy_fclose( fp );

} /* end of cu_set_read_time_file */



/*-----------------------------------------------------------------------*/



void cu_current_time( char str[] )

/* returns current time.  Minimum length of str is 21
 *
 * parameter of routine
 * char      str[];      output; time string
 */
{
	/* local variable */
	char     s[81];
	time_t   ct;

	/* executable code */

	ct = time( NULL );
	strcpy( s, ctime(&ct) );
	strncpy( str, s+8, 2 );     /* dd */
	str[2] = '-';               /* dd- */
	strncpy( str+3, s+4, 3 );   /* dd-mmm */
	str[6] = '-';               /* dd-mmm- */
	strncpy( str+7, s+20, 4 );  /* dd-mmm-yyyy */
	str[11] = '_';              /* dd-mmm-yyyy_ */
	strncpy( str+12, s+11, 8 ); /* dd-mmm-yyyy_hh:mm:ss */
	str[20] = '\0';

} /* end of cu_current_time */



/*--------------------------------------------------------------------------*/



static void cuh_strip_blanks( char str[] )

/* removes blanks at beginning and end of string
 *
 * parameters of routine
 * char       str[];    modify; string to remove blanks
 */
{
	/* local variables */
	int     slen;      /* string length */

	/* executable code */

	while  (*str == ' ')
		strcpy( str, str+1 );

	slen = strlen( str ) - 1;
	while  (str[slen] == ' ')
		str[slen--] = '\0';

} /* end of cuh_strip_blanks */



/*--------------------------------------------------------------------------*/



static void cuh_wait( int cnt )

/* wait loop
 *
 * parameters of routine
 * int        cnt;     input; loop counter
 */
{
	/* local variables */
	float    dmy;

	/* executable code */

#ifdef XXX
	dmy = 1000.0;
	while  (cnt-- > 0)  {
		dmy = sqrt( dmy );
		dmy = pow( dmy, 2.0 );
	} /*endwhile*/
#endif

	usleep( 3*cnt );

} /* end of cuh_wait */



/*--------------------------------------------------------------------------*/



void cu_accept_phase( Widget wm, Widget ws, TPiPhase *phase, TPmTrace *phtrc,
	TSyBoolean acc, TSyStatus *status )

/* accepts a phase from dialog box
 *
 * parameters of routine
 * Widget     wm;      input; main window
 * Widget     ws;      input; single trace window
 * TPiPhase   *phase;  input; pointer to phase info
 * TPmTrace   *phtrc;  input; pointer to trace
 * TSyBoolean acc;     input; show accuracies of phases
 * TSyStatus  *status; output; returns status if no NULL
 */
{
	/* local variables */
	TSyStatus    locstat=BC_NOERROR;  /* local status */

	/* executable code */

	PmAddPhase( phtrc, phase, status );
	if  (SySevere(status))  return;

	mg_do_phase( (Widget)0, MGC_WDW_LAST, MGC_PHASE_CLEAR,
		0, 0, NULL, NULL, NULL );
	mg_mark_one_phase( wm, ws, phase, phtrc, acc );

} /* end of cu_accept_phase */



/*--------------------------------------------------------------------------*/



void cu_phase_box_defaults( Widget w[], TPiPhase *phase )

/* sets button states of phase box
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * TPiPhase   *phase;     input; phase info
 */
{
	/* local variables */
	Arg      al[1];       /* argument list */
	int      qual_widget; /* quality button */

	/* executable code */

	cu_set_string( w[k_widget_phase_name_text], phase->name );
	XmToggleButtonSetState( w[k_widget_phase_spec_e],
		(phase->spec == cPiSpecEmergent), FALSE );
	XmToggleButtonSetState( w[k_widget_phase_spec_i],
		(phase->spec == cPiSpecImpulsive), FALSE );
	XmToggleButtonSetState( w[k_widget_phase_sign_p],
		(phase->sign > 0), FALSE );
	XmToggleButtonSetState( w[k_widget_phase_sign_m],
		(phase->sign < 0), FALSE );
	XmToggleButtonSetState( w[k_widget_phase_sign_0],
		(phase->sign == 0), FALSE );
	XmToggleButtonSetState( w[k_widget_phase_reliab_yes],
		phase->reliable, FALSE );
	XmToggleButtonSetState( w[k_widget_phase_reliab_no],
		!(phase->reliable), FALSE );

	if  (phase->quality < 1)  phase->quality = 1;
	if  (phase->quality > 9)  phase->quality = 9;
	qual_widget = k_widget_phase_qual_0 + phase->quality;
	XtSetArg( al[0], XmNmenuHistory, w[qual_widget] );
	XtSetValues( w[k_widget_phase_weight_option], al, 1 );

} /* end of cu_phase_box_defaults */



/*--------------------------------------------------------------------------*/



#define SH_DEBUG

#define CLEAR_AND_RETURN \
	sy_deallocmem( delay ); \
	sy_deallocmem( lat ); \
	sy_deallocmem( xcp ); \
	sy_deallocmem( code ); \
	return;

#define CODELTH 7



void cu_localization( char phasename[], int do_flags, CUT_PARAMS *par,
	TSyStatus *status )

/* performs localization of event, using marked phases of type "phasename".
 * If CUF_DOLOC_SLOAZ is specified in "do_flags" than slowness and azimuth
 * is determined from a plane wave approximation.  If CUF_DOLOC_DISTANCE
 * is specified the distance in degrees for the given phase and the current
 * slowness value is computed using the appropriate travel time table.
 *
 * parameters of routine
 * char       phasename[];  input; name of phase to be used
 * int        do_flags;     input; CUF_DOLOC_SLOAZ and/or CUF_DOLOC_DISTANCE
 * CUT_PARAMS *par;         input/output; analysis parameters
 * TSyStatus  *status;      output; return status
 */
{
	/* local variables */
	int      alloclth;                /* array length to allocate */
	float    *delay;                  /* pointer to time delays */
	float    *x, *y;                  /* pointer to relative coordinate */
	float    *shift;                  /* time shifts */
	float    *resid;                  /* station residuals to plane wave */
	STATLOC  *lat, *lon;              /* pointer to station locations */
	char     *code;                   /* station codes */
	int      codepos;                 /* code string position */
	TSyBoolean same_array;            /* use relative or absolute locations */
	int      listlth;                 /* number of phases found */
	int      t;                       /* trace counter */
	void     *trc;                    /* current trace pointer */
	char     station[BC_LINELTH+1];   /* name of current station */
	GLT_STATINF *statinf;             /* pointer to station info */
	int      arrcode;                 /* array code */
	char     *onset;                  /* pointer to onset time */
	char     firsttime[BC_TIMELTH+1]; /* onset time of first trace */
	char     reftime[BC_TIMELTH+1];   /* reference time */
	float    dt;                      /* sample distance in sec */
	TSyBoolean same_dt;               /* same sample distance */
	float    travtime;                /* trave time */
	int      loc_method;              /* location method */
	TSyBoolean german_event;          /* this is a german event (roughly) */
	float    ref_shift;               /* time shift betw. first and ref. trace */
	STATLOC  clat, clon;              /* location of center */
	float    u_azim;                  /* used azimuth */
	double   dbl_dist, dbl_azim, dbl_bazim;  /* double results of locdiff */
	float    *xcp, *ycp, *tcp;        /* backup values */
	float    residcorr;               /* residual correction */
	int      ii;                      /* counter */
	TPiPhaseRoot *proot;              /* pointer to phase root */
	TPiPhaseList *plist;              /* pointer to phase list */
	TPiPhase *resphase;               /* pointer to phase */
	TSyStatus locstat;                /* local status */
	float    residrms;                /* root mean square residual */

	/* executable code */

	alloclth = mg_dsptrcs();
	if  (alloclth == 0)  {
		*status = CUE_NOTRACES;
		return;
	} /*endif*/

	/* allocate memory */
	delay = (float *)sy_allocmem( (alloclth+1)*5, (int)sizeof(float), status );
	if  (Severe(status))  return;
	lat = (STATLOC *)sy_allocmem( (alloclth+1)*2, (int)sizeof(STATLOC), status );
	if  (Severe(status))  {sy_deallocmem(delay); return;}
	x = delay + alloclth+1;
	y = x + alloclth+1;
	shift = y + alloclth+1;
	resid = shift + alloclth+1;
	lon = lat + alloclth+1;
	xcp = (float *)sy_allocmem( (alloclth+1)*3, (int)sizeof(float), status );
	if  (Severe(status))  {sy_deallocmem(delay); sy_deallocmem(lat); return;}
	ycp = xcp + alloclth+1;
	tcp = ycp + alloclth+1;
	code = (char *)sy_allocmem( (alloclth+1)*(CODELTH+1),
		(int)sizeof(char), status );
	if  (Severe(status))  {sy_deallocmem(delay); sy_deallocmem(lat);
		sy_deallocmem(xcp); return;}

	/* get delays and positions */
	listlth = 0;
	same_array = TRUE;
	*firsttime = *reftime = '\0';
	dt = 0.0;
	same_dt = TRUE;
	arrcode = -1;
	clat = clon = 0.0;
	codepos = 0;
	trc = NULL;
	for  (t=0; t<alloclth; t++)  {
		trc = db_getp( trc, EP_DSPN, status );
		if  (SySevere(status))  {CLEAR_AND_RETURN}
		/* get name of station */
		db_gets( trc, ES_STATION, BC_LINELTH, station, status );
		if  (Severe(status))  {CLEAR_AND_RETURN}
		strncpy( code+codepos, station, CODELTH );
		codepos += CODELTH+1;
		/* check for phase onset on BEAM if reference station is CENTRE */
		if  (strcmp(station,"BEAM") == 0 || strcmp(station,"ALIGN") == 0)  {
			if  (strcmp(GpGetString(cGpS_refstation),"CENTRE") != 0
				&& strcmp(GpGetString(cGpS_refstation),"CENTER") != 0)
				continue;
			cuh_phase_onset( phasename, trc, &onset, &residcorr );
			if  (onset == NULL)  continue;
			strcpy( reftime, onset );
			continue;
		} /*endif*/
		/* retrieve station info */
		statinf = (GLT_STATINF *)db_getp( trc, EP_STAT, status );
		if  (statinf == NULL)  {
			*status = BC_NOERROR;
			statinf = gl_store_station( station, TRUE, status );
			if  (Severe(status))  {CLEAR_AND_RETURN}
			db_setp( trc, EP_STAT, statinf, status );
			if  (Severe(status))  {CLEAR_AND_RETURN}
		} /*endif*/
		/* get phase onset time */
		cuh_phase_onset( phasename, trc, &onset, &residcorr );
		if  (onset == NULL)  continue;
		/* if reference station is in selected list, store onset */
		if  (strcmp(station,GpGetString(cGpS_refstation)) == 0)
			strcpy( reftime, onset );
		/* store station location */
		lat[listlth] = statinf->lat;
		lon[listlth] = statinf->lon;
		x[listlth] = statinf->xrel;
		y[listlth] = statinf->yrel;
		clat += statinf->lat;
		clon += statinf->lon;
		/* check whether to use relative or absolute location */
		if  (arrcode < 0)  arrcode = statinf->arr_id;
		if  (statinf->arr_id != arrcode) same_array = FALSE;
		/* store onset time on first trace and relative delays */
		if  (*firsttime == '\0')  {
			strcpy( firsttime, onset );
			delay[listlth] = 0.0;
		} else {
			delay[listlth] = tc_tdiff( onset, firsttime, status );
			if  (Severe(status))  {CLEAR_AND_RETURN}
		} /*endif*/
		if  (par->loc_meth == CUC_LOCMETH_RESIDCORR)  {
			delay[listlth] -= residcorr;
			/* printf( "*SHM: apply residual correction\n" ); */
		} /*endif*/
		/* check for the sample rate */
		if  (dt == 0.0)  {
			dt = db_getr( trc, ER_DELTA, NULL );
		} else {
			if  (db_getr(trc,ER_DELTA,NULL) != dt)
				same_dt = FALSE;
		} /*endif*/
		listlth++;
	} /*endfor*/

	if  (arrcode == 0)
		same_array = FALSE;

	/* get position of reference station */
	cu_reference_location( lat+listlth, lon+listlth, x+listlth,
		y+listlth, &arrcode, status );
	if  (Severe(status))  {CLEAR_AND_RETURN}

#ifdef XXX
	if  (!same_array)  {
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: rel. location from (lat,lon) recomputed.\n" );
		mt_deg_to_km( listlth+1, lat, lon, x, y );
	} /*endif*/
#endif
	mb_deg_to_km( listlth+1, lat, lon, x, y );

	for  (t=0; t<=listlth; t++)
		printf( "%lf %lf %f %f\n", lat[t], lon[t], x[t], y[t] );


	if  (listlth < 3 && (CUF_DOLOC_SLOAZ & do_flags))  {

		*status = CUE_TOOLESS;
		CLEAR_AND_RETURN

	} else if  (CUF_DOLOC_SLOAZ & do_flags)  {

		/* check delta-t if only three traces */
		if  (listlth == 3 && !same_dt)  {
			*status = SHE_DIFFSAMP;
			CLEAR_AND_RETURN
		} /*endif*/
 
		/* perform location */
		/* it does not matter that "delay" is relative to */
		/* "firsttime" instead of "reftime" */
		/* save values */
		for  (ii=0; ii<listlth; ii++)  {
			tcp[ii] = delay[ii];
			xcp[ii] = x[ii];
			ycp[ii] = y[ii];
		} /*endfor*/
#		ifdef SH_DEBUG
		mt_locate_old( listlth, delay, x, y, dt,
			&(par->b_azimuth), &(par->b_azimuth_err), &(par->b_slowness),
			&(par->b_slowness_err), status );
		/* restore values */
		for  (ii=0; ii<listlth; ii++)  {
			delay[ii] = tcp[ii];
			x[ii] = xcp[ii];
			y[ii] = ycp[ii];
		} /*endfor*/
		mt_locate_svd( listlth, delay, x, y, dt,
			&(par->b_azimuth), &(par->b_azimuth_err), &(par->b_slowness),
			&(par->b_slowness_err), status );
		/* restore values */
		for  (ii=0; ii<listlth; ii++)  {
			delay[ii] = tcp[ii];
			x[ii] = xcp[ii];
			y[ii] = ycp[ii];
		} /*endfor*/
#		endif
		par->l_slowness = par->l_azimuth = 0.0;
		mt_locate( listlth, delay, x, y, dt,
			&(par->b_azimuth), &(par->b_azimuth_err), &(par->b_slowness),
			&(par->b_slowness_err), status );
		if  (Severe(status))  {CLEAR_AND_RETURN}
		/* restore values */
		for  (ii=0; ii<listlth; ii++)  {
			delay[ii] = tcp[ii];
			x[ii] = xcp[ii];
			y[ii] = ycp[ii];
		} /*endfor*/
		mt_locate_old_error( listlth, delay, x, y,
			par->b_azimuth, par->b_slowness, resid );
		/* par->b_azimuth -= 180.0; */
		/* if  (par->b_azimuth < 0.0)  par->b_azimuth += 360.0; */
		par->l_slowness = 0.0;
		par->l_azimuth = 0.0;
		par->stations_used = listlth;
		clat /= (STATLOC)listlth;
		clon /= (STATLOC)listlth;
		par->center_lat = clat;
		par->center_lon = clon;

		/* insert residuals, loop all phases */
		codepos = 0;
		proot = NULL;
		residrms = 0.0;
		ii = 0;
		FOREVER  {
			proot = PiNextPhaseRoot( proot );
			if  (proot == NULL)  break;
			plist = PiPhaseListOfRoot( proot );
			if  (plist == NULL)  continue;
			strcpy( station, PiStationOfRoot(proot) );
			codepos = 0;
			for  (t=0; t<alloclth; t++)  {
				if  (strcmp(code+codepos,station) == 0)  {
					resphase = PiFindPhase( plist, phasename );
					if  (resphase == NULL)  break;
					resphase->resid = resid[t];
					resphase->resid_type = cPiResidPlaneWave;
					residrms += (resid[t])*(resid[t]);
					ii++;
					break;
				} /*endif*/
				codepos += CODELTH+1;
			} /*endfor*/
		} /*endfor*/
		residrms = sqrt( residrms / (float)ii );
		printf( "*SHM: mean root square residual: %5.2f\n", residrms );

	} else {

	} /*endif*/

	/* store phase in parameter block */
	strcpy( par->phase, phasename );

	if  (!(CUF_DOLOC_DISTANCE & do_flags))  {CLEAR_AND_RETURN}

	if  (*phasename == 'b')  {
		/* the following doesn't make sense for beam selections */
		*status = CUE_DUMMY_PHASE;
		err_setcontext( " ## phase "); err_setcontext( phasename );
		CLEAR_AND_RETURN
	} /*endif*/

	/* compute distance */
	if  (par->loc_meth == CUC_LOCMETH_RESIDCORR)  {
		loc_method = CUC_LOCMETH_RESIDCORR;
	} else {
		loc_method = (par->l_slowness == 0.0 && par->l_azimuth == 0.0) ?
			CUC_LOCMETH_BEAMUNCORR : CUC_LOCMETH_BEAMCORR;
	} /*endif*/
	if  (loc_method == CUC_LOCMETH_BEAMUNCORR
		|| loc_method == CUC_LOCMETH_RESIDCORR)  {
		par->distance = pt_distance( phasename, par->b_slowness,
			par->depth, status );
		if  (*reftime == '\0')
			mt_beamshift( listlth+1, x,y, par->b_azimuth, par->b_slowness, shift );
	} else {
		par->distance = pt_distance( phasename, par->l_slowness,
			par->depth, status );
		if  (*reftime == '\0')
			mt_beamshift( listlth+1, x,y, par->l_azimuth, par->l_slowness, shift );
	} /*endif*/
	par->dist_unit = CUC_DIST_UNIT_DEG;
	if  (Severe(status))  {par->distance = 0.0; CLEAR_AND_RETURN}
	par->loc_meth = loc_method;
	if  (*reftime == '\0')  ref_shift = *shift - shift[listlth];

	/* get location of reference station */
	par->ref_lat = lat[listlth];
	par->ref_lon = lon[listlth];
 
	sy_deallocmem( delay );
	sy_deallocmem( lat );
	sy_deallocmem( xcp );
	sy_deallocmem( code );

	/* get coordinates of source */
	if  (GpGetInt(cGpI_debug_level) > 2)
		printf( "SHM-dbg3: source region computed on sphere !\n" );
	if  (par->loc_meth == CUC_LOCMETH_BEAMUNCORR
		|| par->loc_meth == CUC_LOCMETH_RESIDCORR)  {
		mb_sphereadd( par->ref_lat, par->ref_lon, par->distance,
			par->b_azimuth, &(par->source_lat), &(par->source_lon) );
	} else {
		mb_sphereadd( par->ref_lat, par->ref_lon, par->distance,
			par->l_azimuth, &(par->source_lat), &(par->source_lon) );
	} /*endif*/
#ifdef XXXNEW
	if  (par->center_lat == 0.0 && par->center_lon == 0.0)  {
		*status = CUE_NOCENTER;
		return;
	} /*endif*/
	u_azim = (par->loc_meth == CUC_LOCMETH_BEAMUNCORR
		|| par->loc_meth == CUC_LOCMETH_RESIDCORR) ?
		par->b_azimuth : par->l_azimuth;
	mb_sphereadd( par->center_lat, par->center_lon, par->distance,
		u_azim, &(par->source_lat), &(par->source_lon) );
	mb_locdiff( par->ref_lat, par->ref_lon, par->source_lat, par->source_lon,
		&dbl_dist, &dbl_azim, &dbl_bazim );
	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: center (%5.2f,%5.2f)  ref (%5.2lf,%5.2lf)\n",
			par->distance, u_azim, dbl_dist, dbl_bazim ):
	/* distance & azimuth in 'par' setzen */
#endif

	/* get name of source region */
	german_event = cu_german_event( par->source_lat, par->source_lon );
	if  (german_event)  {
		xxx_get_geo( &(par->source_lat), &(par->source_lon),
			&(par->reg_id), par->regname );
		if  (par->reg_id < 0)  {
			*status = CUE_NOGETGEO;
			err_setcontext( " ## getgeo error " );
			err_setcontext_l( par->reg_id );
			return;
		} /*endif*/
		par->table_number = CUC_REGTABLE_GERGEO;
	} else {
		mb_ferindex( par->source_lat, par->source_lon, &(par->reg_id), status );
		if  (Severe(status))  return;
		mb_fername( par->reg_id, BC_LINELTH, par->regname, status );
		if  (Severe(status))  return;
		par->table_number = CUC_REGTABLE_FLINNENG;
	} /*endif*/
	par->loc_quality = CUC_LOCQ_RELIABLE;

	/* reset source of information */
	par->source[0] = '\0';

	if  (!(CUF_DOLOC_ORIGIN & do_flags))  return;

	/* get origin time, onset time on reference station is in reftime */
	travtime = pt_travel( phasename, par->distance, par->depth, status );
	if  (Severe(status))  return;
	if  (*reftime == '\0')  {
		tc_tadd( firsttime, ref_shift, reftime, status );
	} /*endif*/
	tc_tadd( reftime, -travtime, par->origin, status );
	if  (Severe(status))  return;

} /* end of cu_localization */



#undef CLEAR_AND_RETURN



/*--------------------------------------------------------------------------*/



static void cuh_phase_onset( char phase[], TPmTrace *trc, char **onset,
	float *residcorr )

/* checks whether phase is marked in trc, returns onset time or NULL
 *
 * parameters of routine
 * char       phase[];        input; phase name
 * TPmTrace   *trc;           input; pointer to trace info
 * char       **onset;        output; pointer to onset time
 * float      *residcorr;     output; residual correction (or cPiResidCorrEmpty)
 */
{
	/* local variables */
	TPiPhaseList    *phaselist;         /* pointer to phase list */
	TPiPhase        *c_phase;           /* pointer to current phase */
	TSyStatus       locstat=BC_NOERROR; /* local status */
	int             p;                  /* phase counter */

	/* executable code */

	phaselist = PmGetPhaseList( trc );
	if  (phaselist == NULL)  {
		*onset = NULL;
		return;
	} /*endif*/
	for  (p=0; p<PiPhaseListLength(phaselist); p++)  {
		c_phase = PiGetPhase( phaselist, p, NULL );
		if  (strcmp(c_phase->name,phase) == 0
			&& c_phase->source != cPiSourceTheo)  {
			*onset = c_phase->onset;
			*residcorr = c_phase->resid_corr;
			return;
		} /*endif*/
	} /*endfor*/
	*onset = NULL;
	return;

} /* end of cuh_phase_onset */



/*--------------------------------------------------------------------------*/



void cu_set_param_values( Widget w[], CUT_PARAMS *par, int parno )

/* sets values of parameter block to dialog box.  If distance in deg is
 * smaller than 5 it is recomputed into km.
 *
 * parameters of routine
 * Widget     w[];       input; widget array
 * CUT_PARAMS *par;      input/modify; parameter values
 * int        parno;     input; number of parameter set
 */
{
	/* local variables */
	Arg      al[1];                  /* argument list */
	int      w_locq;                 /* location quality widget */
	char     str[BC_LINELTH+1];      /* scratch string */
	int      i;                      /* counter */

	/* executable code */

	/* check distance */
	if  (par->dist_unit == CUC_DIST_UNIT_DEG && par->distance < 5.0)  {
		par->dist_unit = CUC_DIST_UNIT_KM;
		par->distance *= CUC_DEG_TO_KM;
	} /*endif*/

	sprintf( str, "%5.1f", par->b_azimuth );
	cu_set_string( w[k_widget_param_azimuth_text], str );
	sprintf( str, "%5.1f", par->distance );
	if  (par->dist_unit == CUC_DIST_UNIT_KM)
		strcat( str, "km" );
	cu_set_string( w[k_widget_param_distance_text], str );
	sprintf( str, "%5.1f", par->depth );
	cu_set_string( w[k_widget_param_depth_text], str );
	sprintf( str, "%5.2f", par->b_slowness );
	cu_set_string( w[k_widget_param_slowness_text], str );
	sprintf( str, "%6.3f", par->source_lat );
	cu_set_string( w[k_widget_param_lat_text], str );
	sprintf( str, "%6.3f", par->source_lon );
	cu_set_string( w[k_widget_param_lon_text], str );

	/* set time string */
	if  (par->origin[0] == '\0')  {
		/* writing an empty string "" to the edit strings when dialog box
		 * was unmapped crashed the program (Bad Pixmap) on mapping the
		 * dialog box afterwards
		 */
		cu_set_string( w[k_widget_param_origin_d_text], " " );
		cu_set_string( w[k_widget_param_origin_t_text], " " );
	} else {
		i = 0;
		while  (par->origin[i] != '_')  {
			i++;
			if  (par->origin[i] == '\0' || i > BC_TIMELTH)  {
				cu_set_string( w[k_widget_param_origin_d_text], " " );
				cu_set_string( w[k_widget_param_origin_t_text], " " );
				printf( "*SHM: cu_set_param_values: error in time string\n" );
				return;
			} /*endif*/
		} /*endwhile*/
		strncpy( str, par->origin, i );  str[i] = '\0';
		cu_set_string( w[k_widget_param_origin_d_text], str );
		strcpy( str, (par->origin)+i+1 );
		cu_set_string( w[k_widget_param_origin_t_text], str );
	} /*endif*/

	/* set quality option menu */
	switch  (par->loc_quality)  {
	case CUC_LOCQ_INCOHERENT: w_locq = k_widget_param_locq_incoherent; break;
	case CUC_LOCQ_NOBEARING:  w_locq = k_widget_param_locq_nobearing; break;
	case CUC_LOCQ_REGION:     w_locq = k_widget_param_locq_region; break;
	case CUC_LOCQ_RELIABLE:   w_locq = k_widget_param_locq_reliable; break;
	case CUC_LOCQ_UNDEFINED:  w_locq = k_widget_param_locq_undefined; break;
	default:                  w_locq = k_widget_param_locq_undefined; break;
	} /*endswitch*/
	XtSetArg( al[0], XmNmenuHistory, w[w_locq] );
	XtSetValues( w[k_widget_param_locq_option], al, 1 );

	/* set event type option menu */
	switch  (par->source_type)  {
	case CUC_TYPE_LOCAL_QUAKE: w_locq = k_widget_phase_type_local; break;
	case CUC_TYPE_REGIO_QUAKE: w_locq = k_widget_phase_type_regio; break;
	case CUC_TYPE_TELE_QUAKE:  w_locq = k_widget_phase_type_tele;  break;
	case CUC_TYPE_NUCLEAR:     w_locq = k_widget_phase_type_nuclear; break;
	case CUC_TYPE_BLAST:       w_locq = k_widget_phase_type_blast; break;
	case CUC_TYPE_MINING:      w_locq = k_widget_phase_type_mining; break;
	default:                   w_locq = k_widget_phase_type_other; break;
	} /*endswitch*/
	XtSetArg( al[0], XmNmenuHistory, w[w_locq] );
	XtSetValues( w[k_widget_phase_type_option], al, 1 );

	/* set parameter set option menu */
	XtSetArg( al[0], XmNmenuHistory, w[k_widget_param_set_1+parno] );
	XtSetValues( w[k_widget_param_set_option], al, 1 );

} /* end of cu_set_param_values */



/*--------------------------------------------------------------------------*/



void cu_print_param_values( CUT_PARAMS *par )

/* prints out parameters
 *
 * parameters of routine
 * CUT_PARAMS *par;        input; analysis parameters
 */
{
	/* executable code */

	if  (par->distance == 0.0)  {
		printf( "   distance     : not specified\n" );
	} else {
		printf( "   distance     : %5.1f\n", par->distance );
	} /*endif*/
	if  (par->b_slowness == 0.0 && par->b_azimuth == 0.0)  {
		printf( "   beam-slowness: not specified\n" );
		printf( "   beam-azimuth : not specified\n" );
	} else {
		printf( "   beam-slowness: %5.1f +/- %5.1f  (%s)\n",
			par->b_slowness, par->b_slowness_err, par->phase );
		printf( "   beam-azimuth : %5.1f +/- %5.1f  (%s)\n",
			par->b_azimuth, par->b_azimuth_err, par->phase );
	} /*endif*/
	if  (par->l_slowness == 0.0 && par->l_azimuth == 0.0)  {
		printf( "   epi-slowness : not specified\n" );
		printf( "   epi-azimuth  : not specified\n" );
	} else {
		printf( "   epi-slowness : %5.1f  (%s)\n",
			par->l_slowness, par->phase );
		printf( "   epi-azimuth  : %5.1f  (%s)\n",
			par->l_azimuth, par->phase );
	} /*endif*/
	printf( "   depth        : %5.1f +/- %5.1f\n",
		par->depth, par->err.dep );
	if  (par->ref_lat == 0.0 && par->ref_lon == 0.0)  {
		printf( "   reference    : %s\n", GpGetString(cGpS_refstation) );
	} else {
		printf( "   reference    : %s, %7.2f lat  %7.2f lon\n",
			GpGetString(cGpS_refstation), par->ref_lat, par->ref_lon );
	} /*endif*/
	if  (par->loc_meth == CUC_LOCMETH_BEAMUNCORR)  {
		printf( "   location determined from beam\n" );
	} else if  (par->loc_meth == CUC_LOCMETH_BEAMCORR)  {
		printf( "   location determined from corrected slowness & azimuth\n" );
	} else if  (par->loc_meth == CUC_LOCMETH_HYPO)  {
		printf( "   location determined by Hypoellipse\n" );
	} /*endif*/
	if  (par->origin[0] == '\0')  {
		printf( "   origin time  : not specified\n" );
	} else {
		printf( "   origin time  : %s\n", par->origin );
	} /*endif*/
	if  (par->source_lat == 0.0 && par->source_lon == 0.0)  {
		printf( "   epicenter    : not specified\n" );
	} else {
		printf( "   epicenter    : %7.2f lat  %7.2f lon\n",
			par->source_lat, par->source_lon );
	} /*endif*/
	if  (par->regname[0] == '\0')  {
		printf( "   FE region    : not specified\n" );
	} else {
		printf( "   FE region    : %s\n", par->regname );
	} /*endif*/
#ifdef XXX
	if  (par->evid == 0)  {
		printf( "   Event ID     : not specified\n" );
	} else {
		printf( "   Event ID     : %09ld\n", par->evid );
	} /*endif*/
#endif

} /* end of cu_print_param_values */



/*--------------------------------------------------------------------------*/



void cu_get_param_values( Widget w[], CUT_PARAMS *par )

/* copies values from dialog box to parameter block
 *
 * parameters of routine
 * Widget     w[];         input; widget array
 * CUT_PARAMS *par;        output; parameter values
 */
{
	/* local variables */
	char     *str1, *str2;     /* pointers to strings */
	int      slen1, slen2;     /* string lengths */
	STATUS   locstat;          /* local status */
	char        *txt;          /* text pointer */
	int         slen;          /* string length */

	/* executable code */

	if  (sscanf( cu_get_string(w[k_widget_param_azimuth_text]), "%f",
		&(par->b_azimuth) ) != 1)  {
		par->b_azimuth = 0.0;
		if  (GpGetInt(cGpI_debug_level) > 0)
			printf( "SHM-dbg1: cu_get_param_values: couldn't read azimuth\n" );
	} /*endif*/
	txt = cu_get_string( w[k_widget_param_distance_text] );
	if  (sscanf( txt, "%f", &(par->distance) ) != 1)  {
		par->distance = 0.0;
		if  (GpGetInt(cGpI_debug_level) > 0)
			printf( "SHM-dbg1: cu_get_param_values: couldn't read distance\n" );
	} /*endif*/
	par->dist_unit = CUC_DIST_UNIT_DEG;
	slen = strlen( txt );
	if  (slen > 2 && txt[slen-2] == 'k' && txt[slen-1] == 'm')
		par->dist_unit = CUC_DIST_UNIT_KM;
	if  (sscanf( cu_get_string(w[k_widget_param_depth_text]), "%f",
		&(par->depth) ) != 1)  {
		par->depth = 0.0;
		if  (GpGetInt(cGpI_debug_level) > 0)
			printf( "SHM-dbg1: cu_get_param_values: couldn't read depth\n" );
	} /*endif*/
	if  (sscanf( cu_get_string(w[k_widget_param_slowness_text]), "%f",
		&(par->b_slowness) ) != 1)  {
		par->b_slowness = 0.0;
		if  (GpGetInt(cGpI_debug_level) > 0)
			printf( "SHM-dbg1: cu_get_param_values: couldn't read slowness\n" );
	} /*endif*/
	str1 = cu_get_string( w[k_widget_param_origin_d_text] );
	str2 = cu_get_string( w[k_widget_param_origin_t_text] );
	slen1 = strlen( str1 );
	slen2 = strlen( str2 );
	if  (slen1 > 6 && slen2 > 0)  {
		if  (slen1+slen2+1 <= BC_TIMELTH)  {
			sprintf( par->origin, "%s_%s", str1, str2 );
			/* check time */
			locstat = BC_NOERROR;
			tc_tadd( par->origin, 0.0, par->origin, &locstat );
			if  (Severe(&locstat))  {
				if  (GpGetInt(cGpI_debug_level) > 0)
					printf( "SHM-dbg1: cu_get_param_values: couldn't read origin time\n" );
				par->origin[0] = '\0';
			} /*endif*/
		} /*endif*/
	} else {
		par->origin[0] = '\0';
	} /*endif*/
	if  (sscanf( cu_get_string(w[k_widget_param_lat_text]), "%f",
		&(par->source_lat) ) != 1)  {
		par->source_lat = 0.0;
		if  (GpGetInt(cGpI_debug_level) > 0)
			printf( "SHM-dbg1: cu_get_param_values: couldn't read source lat\n" );
	} /*endif*/
	if  (sscanf( cu_get_string(w[k_widget_param_lon_text]), "%f",
		&(par->source_lon) ) != 1)  {
		par->source_lon = 0.0;
		if  (GpGetInt(cGpI_debug_level) > 0)
			printf( "SHM-dbg1: cu_get_param_values: couldn't read source lon\n" );
	} /*endif*/

} /* end of cu_get_param_values */



/*--------------------------------------------------------------------------*/



void cu_get_display_control( Widget w[], MGT_DSPCTRL *ctrl )

/* puts display controls values from dialog box to control structure
 *
 * parameters of routine
 * Widget        w[];     input; Widget array of dialog box elements
 * MGT_DSPCTRL   *ctrl;   output; values retrieved
 */
{
	/* local variables */

	/* executable code */
/*
	sscanf( cu_get_string(w[k_widget_ctrl_zoom_text]),
		"%f", &(ctrl->zoom) );
*/
} /* end of cu_get_display_control */



/*--------------------------------------------------------------------------*/



void cu_theo_arrivals( Widget wm, Widget ws, CUT_PARAMS *par, STATUS *status )

/* marks theoretical arrivals on all traces on display
 *
 * parameters of routine
 * Widget     wm;               input; widget ID of main window
 * Widget     ws;               input; widget ID of single trace window
 * CUT_PARAMS *par;             input; analysis parameters
 * STATUS     *status;          output; return status
 */
{
	/* local variables */
	TPmTrace *trc;                      /* pointer to trace */
	char     curr_phase[BC_LINELTH+1];  /* current theo phase */
	GLT_STATINF *statinf;               /* pointer to station info */
	float    distance;                  /* distance in deg */
	float    azimuth;                   /* back azimuth in deg */
	float    travtime;                  /* trave time in sec */
	TPiPhase theo_phase;                /* theoretical phase */
	int      slen;                      /* string length */
	TSyStatus locstat;                  /* local status */
	char     *sc, *dc;                  /* moving pointers */
	TSyBoolean quit;                    /* quit loop */

	/* executable code */

	if  (par->distance == 0.0)  {
		*status = CUE_NODISTANCE;
		return;
	} /*endif*/

	/* delete all theoretical phases from all traces */
	cu_delete_all_phases( wm, ws, "--theo--", TRUE );

	/* compute and mark new phases */
	sc = GpGetString( cGpS_theo_phase_list );
	dc = curr_phase;
	quit = FALSE;
	while  (!quit)  {

		if  (*sc != ',' && *sc != '\0')  {
			*dc++ = *sc++;
			continue;
		} /*endif*/
		if  (*sc++ == '\0')  quit = TRUE;
		*dc = '\0';
		dc = curr_phase;

		/* if comment line or depth phase at depth 0, do nothing */
		if  (par->depth == 0.0 &&
			(*curr_phase == 'p' || *curr_phase == 's'))  continue;

		/* process current phase for each trace */
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: phase %s\n", curr_phase );
		trc = NULL;
		FOREVER  {
			locstat = cBcNoError;
			trc = db_getp( trc, EP_DSPN, &locstat );
			if  (trc == NULL)  break;
			/* compute traveltime and setup phase descr. */
			statinf = cu_get_statinf( trc, status );
			if  (*status == CUE_BEAMSTATION)  {*status=BC_NOERROR; continue;}
			if  (SySevere(status))  return;
			mb_spherediff( statinf->lat, statinf->lon,
				par->source_lat, par->source_lon,
				&distance, &azimuth );
			locstat = cBcNoError;
			travtime = pt_travel( curr_phase, distance,
				par->depth, &locstat );
			if  (locstat != cBcNoError)  continue;
			if  (Severe(status))  return;
			cu_reset_phase( &theo_phase );
			strcpy( theo_phase.name, curr_phase );
			tc_tadd( par->origin, travtime,
				theo_phase.onset, status );
			if  (SySevere(status))  return;
			theo_phase.spec = cPiSpecEmergent;
			/*theo_phase.phasetrc = mg_trcptr( t+1 );*/
			theo_phase.sign = 0;
			theo_phase.source = cPiSourceTheo;
			/* put phase on screen */
			cu_accept_phase( wm, ws, &theo_phase, trc, FALSE, status );
			if  (SySevere(status))  return;
		} /*endfor*/

	} /*endwhile*/

} /* end of cu_theo_arrivals */



/*--------------------------------------------------------------------------*/



void cu_delete_all_phases( Widget wm, Widget ws, char name[],
	TSyBoolean allmem )

/* All phases with name "name" are deleted.  If name=="--theo--", all
 * theoretical phases will be deleted, if name="--pseudo--", all pseudo-phases
 * starting with 'b' are deleted.
 *
 * parameters of routine
 * Widget     wm;       input; widget ID of main drawing area
 * Widget     ws;       input; widget ID of single draw window
 * char       name;     input; name of phases to be deleted
 * TSyBoolean allmem;   input; all phases in memory (TRUE) or on display only
 */
{
	/* local variables */
	TSyStatus   locstat;            /* local status */
	int         trcno;              /* number of traces */
	TSyBoolean  do_theo;            /* delete theo phases */
	TSyBoolean  do_pseudo;          /* delete all pseudo phases */
	TPiPhaseRoot  *proot;           /* pointer to phase root */
	TPiPhaseList  *plist;           /* pointer to phase list */
	int         plist_lth;          /* length of phase list */
	int         p;                  /* phase counter */
	TPiPhase    *rem_phase;         /* phase to be removed */
	TPmTrace    *trc;               /* pointer to trace */

	/* executable code */

	do_theo = (strcmp(name,"--theo--") == 0);
	do_pseudo = (strcmp(name,"--pseudo--") == 0);

	/* loop over traces on display */
	proot = NULL;
	FOREVER  {
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		plist = PiPhaseListOfRoot( proot );
		if  (plist == NULL)  continue;
		plist_lth = PiPhaseListLength( plist );
		for  (p=0; p<plist_lth; p++)  {
			locstat = cBcNoError;
			rem_phase = PiGetPhase( plist, p, &locstat );
			if  (locstat != cBcNoError)  continue;
			if  (do_pseudo && rem_phase->name[0] != 'b')  continue;
			if  (do_theo && rem_phase->source != cPiSourceTheo)  continue;
			if  (strcmp(name,"*") != 0)
				if  (!do_pseudo && !do_theo && (strcmp(rem_phase->name,name) != 0))
					continue;
			/* remove phase(s) from display */
			trc = NULL;
			FOREVER  {
				trc = PmFindTrace( trc, PiDescrOfRoot(proot) );
				if  (trc == NULL)  break;
				mg_mark_one_phase( wm, ws, rem_phase, trc, FALSE );
			} /*endfor*/
			PiDeletePhase( plist, rem_phase, &locstat );
			p--;
			plist_lth--;
		} /*endfor*/
	} /*endfor*/

} /* end of cu_delete_all_phases */



/*--------------------------------------------------------------------------*/



static GLT_STATINF *cu_get_statinf( TPmTrace *trc, TSyStatus *status )

/* returns pointer to static station info block of trace number "trcno"
 *
 * parameters of routine
 * TPmTrace   *trc;           input; pointer to trace
 * TSyStatus  *status;        output; return status
 *                            returns static pointer to station info
 */
{
	/* local variables */
	GLT_STATINF *statinf;               /* pointer to station info */
	TSyStatus   locstat;                /* local status */
	char        station[BC_LINELTH+1];  /* station name */

	/* executable code */

	locstat = cBcNoError;
	statinf = (GLT_STATINF *)db_getp( trc, EP_STAT, &locstat );
	if  (statinf == NULL)  {
		db_gets( trc, ES_STATION, BC_LINELTH, station, status );
		if  (Severe(status))  return NULL;
		if  (strcmp(station,"BEAM") == 0 || strcmp(station,"ALIGN") == 0)  {
			*status = CUE_BEAMSTATION;
			return NULL;
		} /*endif*/
		statinf = gl_store_station( station, TRUE, status );
		if  (Severe(status))  return NULL;
                db_setp( trc, EP_STAT, statinf, status );
		if  (Severe(status))  return NULL;
	} /*endif*/

	return statinf;

} /* end of cu_get_statinf */



/*--------------------------------------------------------------------------*/



void cu_alert_message( Widget w, char text [] )

/* puts alert box on screen
 *
 * parameters of routine
 * Widget     w;         input; alert box widget ID
 * char       text[];    input; alert message
 */
{
	/* local variables */
	Arg      arg[3];    /* argument list */
	int      n;         /* arg counter */
	static XmString   xstr;             /* x string */
	static TSyBoolean xstr_init=FALSE;  /* x string created */

	/* executable code */

	if  (XtIsManaged(w))  XtUnmanageChild( w );

	/*if  (xstr_init)  XtFree( (char *)xstr );*/
	xstr = XmStringLtoRCreate( text, "" );
	xstr_init = TRUE;

	n = 0;
	XtSetArg( arg[n], XmNmessageString, xstr );  n++;
	XtSetValues( w, arg, n );

	XtManageChild( w );

} /* end of cu_alert_message */



/*--------------------------------------------------------------------------*/



void cu_alert( TSyStatus status )

/* puts alert message on screen
 *
 * parameters of routine
 * TSyStatus     status;   input; status code
 */
{
	/* local variables */
	char     str[BC_LONGSTRLTH+1];   /* scratch string */
	char     context[BC_LINELTH+1];  /* error context */

	/* executable code */

	err_msg( status, str );
	err_getcontext( context );
	if  (*context != '\0')  {
		if  (strlen(str)+strlen(context)+1 < BC_LONGSTRLTH)  {
			strcat( str, "\n" );
			strcat( str, context );
		} /*endif*/
		err_clearcontext();
	} /*endif*/

	if  (cuv_alert_widget == (Widget)0)  {
		printf( "*SHM: alert widget not yet set: ***\n" );
		printf( "%s\n", str );
		return;
	} /*endif*/

	cu_alert_message( cuv_alert_widget, str );

} /* end of cu_alert */



/*--------------------------------------------------------------------------*/



void cu_set_alert_widget( Widget w )

/* sets alert widget
 *
 * parameters of routine
 * Widget     w;     input; widget ID of alert box
 */
{
	/* local variables */

	/* executable code */

	cuv_alert_widget = w;  /* used for status message output */

} /* end of cu_set_alert_widget */



/*--------------------------------------------------------------------------*/



void cu_set_multiplication_widget( Widget box, Widget label, Widget edit )

/* sets widget ID's of multiplication box
 *
 * parameters of routine
 * Widget     box;     input; widget ID of whole box
 * Widget     label;   input; widget ID of title label
 * Widget     edit;    input; widget ID of value editable text
 */
{
	/* executable code */

	cuv_multipli_box = box;
	cuv_multipli_value = label;
	cuv_multipli_edit = edit;

} /* end of cu_set_multiplication_widget */



/*--------------------------------------------------------------------------*/



void cu_multiplication_dialog( TPmTrace *phtrc, TPiPhase *phase )

/* let the user multiply the value
 *
 * parameters of routine
 * TPmTrace   *phtrc  input; phase pointer
 * TPiPhase   *phase;   modify; phase amplitudes to update
 */
{
	/* local variables */
	static XmString val;            /* value text */
	static BOOLEAN val_init=FALSE;  /* value is initalized */
	Arg      a[1];                  /* argument */
	char     str[BC_LINELTH+1];     /* scratch string */

	/* executable code */

	cuv_multipli_phase = phase;
	cuv_multipli_trc = phtrc;
	sprintf( str, "value %f", phase->ampl );

	if  (XtIsManaged(cuv_multipli_box))
		XtUnmanageChild( cuv_multipli_box );
	if  (val_init)  XtFree( (char *)val );
	val = XmStringLtoRCreate( str, "" );
	val_init = TRUE;
	XtSetArg( a[0], XmNlabelString, val );
	XtSetValues( cuv_multipli_value, a, 1 );
	XtManageChild( cuv_multipli_box );

} /* end of cu_multiplication_dialog */



/*--------------------------------------------------------------------------*/



void cu_do_multiplication( float fac, TSyStatus *status )

/* performs multiplication prepared by cu_multiplication_dialog
 *
 * parameters of routine
 * float      fac;         input; factor
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	char       *valptr;     /* pointer to value */
	float      factor;      /* multiplication factor */
	TSyStatus  locstat;     /* local status */

	/* executable code */

	if  (cuv_multipli_phase == NULL)  {
		*status = CUE_NOMULTIPLI;
		return;
	} else if  (cuv_multipli_trc == NULL)  {
		*status = CUE_NOMULTIPLI;
		return;
	} /*endif*/

	if  (fac == CUC_MULTIPLI_EDIT_MUL || fac == CUC_MULTIPLI_EDIT_DIV)  {
		valptr = cu_get_string( cuv_multipli_edit );
		if  (sscanf( valptr, "%f", &factor ) != 1)  {
			*status = CUE_CNV_FLOAT;
			err_setcontext( " ## number: " );
			err_setcontext( valptr );
			return;
		} /*endif*/
		if  (fac == CUC_MULTIPLI_EDIT_DIV)
			factor = 1.0 / factor;
	} else {
		factor = fac;
	} /*endif*/

	cuv_multipli_phase->period *= factor;
	locstat = BC_NOERROR;
	cu_ampl_restitution( cuv_multipli_trc,
			cuv_multipli_phase->ampl,
			cuv_multipli_phase->period, "DSP",
			&(cuv_multipli_phase->ampl_displ), cuv_multipli_phase->filter,
			&locstat );
	if  (locstat != BC_NOERROR)  cuv_multipli_phase->ampl_displ = 0.0;
	locstat = BC_NOERROR;
	cu_ampl_restitution( cuv_multipli_trc,
			cuv_multipli_phase->ampl,
			cuv_multipli_phase->period, "VEL",
			&(cuv_multipli_phase->ampl_veloc), cuv_multipli_phase->filter,
			&locstat );
	if  (locstat != BC_NOERROR)  cuv_multipli_phase->ampl_veloc = 0.0;
	cu_print_phase( cuv_multipli_phase );
	cuv_multipli_phase = NULL;

	if  (XtIsManaged(cuv_multipli_box))
		XtUnmanageChild( cuv_multipli_box );

} /* end of cu_do_multiplication */



/*--------------------------------------------------------------------------*/



void cu_print_phase( TPiPhase *phase )

/* prints out phase information
 *
 * parameters of routine
 * TPiPhase   *phase;      input; phase info
 */
{
	/* local variables */
	TPmTrace   *trcptr;            /* trace pointer */
	char       str[BC_LINELTH+1];  /* scratch string */
	TSyStatus  locstat=cBcNoError; /* local status */

	/* executable code */

	printf( "\n" );
	printf( "   phase : %s   ", phase->name );
	if  (phase->spec == cPiSpecEmergent)   printf( "emergent  " );
	if  (phase->spec == cPiSpecImpulsive)  printf( "impulsive " );
	if  (phase->sign == 1)   printf( "compression " );
	if  (phase->sign == -1)  printf( "dilatation  " );
	if  (phase->source == cPiSourceTheo)  printf( "theoretical" );
	if  (phase->source == cPiSourceAuto)  printf( "automatic" );
	printf( "\n" );
	printf( "   onset : %s\n", phase->onset );
	if  (phase->period > 0.0)  {
		printf( "   period: %5.2f\n", phase->period );
	} else {
		printf( "   period: not specified\n" );
	} /*endif*/
	if  (phase->filter[0] != '\0')  {
		printf( "   filter: %s\n", phase->filter );
	} else {
		printf( "   filter: none\n" );
	} /*endif*/
	if  (phase->ampl > 0.0)  {
		printf( "   amplitude (plain): %6.1f\n", phase->ampl );
	} else {
		printf( "   amplitude (plain): not specified\n" );
	} /*endif*/
	if  (phase->ampl_veloc > 0.0)  {
		printf( "   amplitude (veloc): %6.1f\n", phase->ampl_veloc );
	} else {
		printf( "   amplitude (veloc): not specified\n" );
	} /*endif*/
	if  (phase->ampl_displ > 0.0)  {
		printf( "   amplitude (displ): %6.1f\n", phase->ampl_displ );
	} else {
		printf( "   amplitude (displ): not specified\n" );
	} /*endif*/
	if  (phase->bb.bbampl > 0.0)  {
		printf( "   amplitude (BB)   : %6.1f\n", phase->bb.bbampl );
	} else {
		printf( "   amplitude (BB)   : not specified\n" );
	} /*endif*/
	if  (phase->ampl_time > 0.0)  {
		printf( "   amplitude time   : %5.2f\n", phase->ampl_time );
	} else {
		printf( "   amplitude time   : not specified\n" );
	} /*endif*/
	if  (phase->comment[0] != '\0')
		printf( "   comment: %s\n", phase->comment );

#	ifdef XXX
	trcptr = phase->phasetrc;
	if  (trcptr != NULL)  {
		db_gets( trcptr, ES_STATION, BC_LINELTH, str, &locstat );
		if  (locstat == BC_NOERROR)
			printf( "   station: %s\n", str );
	} /*endif*/
#	endif
	printf( "\n" );

} /* end of cu_print_phase */



/*--------------------------------------------------------------------------*/



void cu_ampl_restitution( TPmTrace *trcptr, float ampl, float period,
	char kind[], float *rest_ampl, char filter[], TSyStatus *status )

/* returns restituted amplitude, velocity or displacement, depending on
 * parameter "kind" ("VEL" or "DSP", respectively).  The filter variable
 * must have length BC_LINELTH+1.
 *
 * parameters of routine
 * void       *trcptr;         input; pointer to trace info
 * float      ampl;            input; input amplitude
 * float      period;          input; period of signal in sec
 * char       kind[];          input; either "VEL" or "DSP"
 * float      *rest_ampl;      output; restituted amplitude
 * char       filter[];        output; filter name (min. length BC_LINELTH+1)
 * TSyStatus  *status;         output; return status
 *                             returns restituted amplitude
 */
{
	/* local variables */
	char       str[BC_LINELTH+1];      /* scratch string */
	TSyStatus  locstat;                /* local status */
	char       *filptr;                /* pointer to filter name */
	float      frq;                    /* frequency of signal in Hz */
	float      tffac;                  /* transfer function factor */
	char       lfilname[BC_LINELTH+1]; /* local filter name */
	TIME       ttime;                  /* trace time */
	char       strtime[cBcTimeLth+1];  /* string time */
	int        trcflags;               /* trace flags for filtered traces */

	/* executable code */

	if  (trcptr == NULL)  {
		*status = CUE_BUG;
		printf(
			"*SHM: cu_ampl_restitution: NULL ptr (illegal phase->phasetrc)\n" );
		return;
	} /*endif*/

	if  (period < 1.e-8)  {
		*status = CUE_ZERO_PERIOD;
		return;
	} /*endif*/
	frq = 1/period;

	locstat = BC_NOERROR;
	db_gets( trcptr, ES_COMMENT, BC_LINELTH, str, &locstat );
	if  (locstat == BC_NOERROR)  {
		filptr = strstr( str, "FLT:" );
		if  (filptr == NULL)  filptr = strstr( str, "flt:" );
		if  (filptr != NULL)  {
			sscanf( filptr+4, "%s", filter );
		} else {
			*filter = '\0';
		} /*endif*/
	} else {
		*filter = '\0';
	} /*endif*/
	strcpy( lfilname, filter );

	if  (*lfilname == '\0')  {
		locstat = cBcNoError;
		db_gett( trcptr, ET_START, &ttime, &locstat );
		tc_a2t( &ttime, strtime, &locstat );
		cu_get_stream_string( trcptr, str, status );
		if  (Severe(status))  return;
		cu_lookup_filter( str, strtime, lfilname, &trcflags, status );
		if  (Severe(status))  return;
	} /*endif*/

	/* build filter name & read amplitude */
	strcpy( str, "TF_" );
	strcat( str, kind );
	strcat( str, "_" );
	strcat( str, lfilname );
	tffac = ff_filter_amplitude( str, frq, status );

	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: filter %s, frq %f, factor %f\n", str, frq, tffac );

	if  (tffac < 1.e-10)  {
		*status = CUE_ZERO_TRANSF;
		return;
	} /*endif*/
	*rest_ampl = ampl / tffac;

} /* end of cu_ampl_restitution */



/*--------------------------------------------------------------------------*/



void cu_get_tf_name( char station[], int maxlth, char filter[],
	TSyStatus *status )

/* returns name of transfer function derived from station name
 *
 * parameters of routine
 * char       station[];        input; station name
 * int        maxlth;           input; maximum length of output string
 * char       filter[];         output; derived filter name
 * TSyStatus  *status;          output; return status
 */
{
	/* executable code */

	if  (maxlth < 7)  {
		*status = CUE_STROVFL;
		return;
	} /*endif*/

	if  (strncmp(station,"GR",2) == 0 && station[2] >= 'A' &&
		station[2] <= 'C' && station[3] >= '1' && station[3] <= '5')  {
		strcpy( filter, "GRF" );
	} else if  (strcmp(station,"GEC2B") == 0 || strcmp(station,"GEC2") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"GEC2A") == 0)  {
		strcpy( filter, "GERESS" );
	} else if  (strncmp(station,"GE",2) == 0 && station[2] >= 'A' &&
		station[2] <= 'D' && station[3] >= '0' && station[3] <= '9')  {
		strcpy( filter, "GERESS" );
	} else if  (strcmp(station,"BFO") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"BRG") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"BRNL") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"BUG") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"CLL") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"CLZ") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"FUR") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"GSH") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"HAM") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"IBB") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"LID") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"MOX") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"SEG") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"STU") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"TNS") == 0)  {
		strcpy( filter, "GRSN" );
	} else if  (strcmp(station,"WET") == 0)  {
		strcpy( filter, "GRSN" );
	} else {
		strcpy( filter, station );
	} /*endif*/

} /* end of cu_get_tf_name */



/*--------------------------------------------------------------------------*/



void cu_print_info( char str[] )

/* prints out informational message
 *
 * parameters of routine
 * char       str[];       input; message to print
 */
{
	/* executable code */

	printf( "SHM-i! %s\n", str );

} /* end of cu_print_info */



/*--------------------------------------------------------------------------*/



void cu_set_string( Widget w, char str[] )

/* sets string in text widget str.  Checks whether str is empty, in this case
 * a single blank is inserted because of bug in Motif software
 *
 * parameters of routine
 * Widget     w;         input; widget ID
 * char       str[];     input; text to be set
 */
{
	/* local variables */

	/* executable code */

	if  (*str == '\0')  {
		XmTextSetString( w, " " );
	} else {
		XmTextSetString( w, str );
	} /*endif*/

} /* end of cu_set_string */



/*--------------------------------------------------------------------------*/



char *cu_get_string( Widget w )

/* gets string from text widget str.  Checks whether string in text widget is
 * a single blank, in this case a pointer to an empty string is returned
 * because of bug in Motif software
 *
 * parameters of routine
 * Widget     w;         input; widget ID
 *                       returns pointer to static string
 */
{
	/* local variables */
	static char  zero='\0';
	char         *ptr;         /* pointer to widget string */

	/* executable code */

	ptr = (char *)XmTextGetString( w );
	if  (ptr[0] == ' ' && ptr[1] == '\0')  {
		return &zero;
	} else {
		return ptr;
	} /*endif*/

} /* end of cu_set_string */



/*---------------------------------------------------------------------------*/



#define ON_ERROR_RETURN \
	if  (Severe(status))  { \
		err_setcontext( " ## station " ); \
		err_setcontext( station ); \
		return; \
	}



void cu_reference_location( STATLOC *lat, STATLOC *lon, float *xrel,
	float *yrel, int *arrcode, STATUS *status )

/* returns location of reference point
 *
 * parameters of routine
 * STATLOC    *lat, *lon;        output; reference location (if not NULL)
 * float      *xrel, *yrel;      output; relative location in km (if not NULL)
 * int        *arrcode;          output; array code (if not NULL)
 * STATUS     *status;           output; return status
 */
{
	/* local variables */
	TPmTrace *trc;                       /* pointer to trace */
	char     station[BC_SHORTSTRLTH+1];  /* current station name */
	int      dsplth;                     /* number of traces on display */
	STATUS   locstat;                    /* local status */
	GLT_STATINF *statinf;                /* pointer to station info */
	STATLOC  mean_lat, mean_lon;         /* mean location */
	float    mean_x, mean_y;             /* mean relative location */
	int      mean_arrcode;               /* array code of all */

	/* executable code */

	/* check for first trace */
	if  (strcmp(GpGetString(cGpS_refstation),"FIRST") == 0)  {
		trc = db_getp( NULL, EP_DSPN, NULL );
		if  (trc == NULL)  {*status = CUE_NOTRACES; return;}
		db_gets( trc, ES_STATION, BC_SHORTSTRLTH, station, status );
		ON_ERROR_RETURN
		statinf = gl_store_station( station, TRUE, status );
		ON_ERROR_RETURN
		if  (lat != NULL)  *lat = statinf->lat;
		if  (lon != NULL)  *lon = statinf->lon;
		if  (xrel != NULL)  *xrel = statinf->xrel;
		if  (yrel != NULL)  *yrel = statinf->yrel;
		if  (arrcode != NULL)  *arrcode = statinf->arr_id;
		return;
	} /*endif*/

	/* check for other station than center */
	if  (strcmp(GpGetString(cGpS_refstation),"CENTRE") != 0
		&& strcmp(GpGetString(cGpS_refstation),"CENTER") != 0)  {
		statinf = gl_store_station( GpGetString(cGpS_refstation), TRUE, status );
		ON_ERROR_RETURN
		if  (lat != NULL)  *lat = statinf->lat;
		if  (lon != NULL)  *lon = statinf->lon;
		if  (xrel != NULL)  *xrel = statinf->xrel;
		if  (yrel != NULL)  *yrel = statinf->yrel;
		if  (arrcode != NULL)  *arrcode = statinf->arr_id;
		return;
	} /*endif*/

	/* loop all traces on display and compute center */
	/* !!! actually this is not quite correct, fix it later !!! */
	dsplth = 0;
	mean_lat = mean_lon = 0.0;
	mean_x = mean_y = 0.0;
	mean_arrcode = -1;
	trc = NULL;
	FOREVER  {
		trc = db_getp( trc, EP_DSPN, NULL );
		if  (trc == NULL)  break;
		locstat = BC_NOERROR;
		db_gets( trc, ES_STATION, BC_SHORTSTRLTH, station, status );
		ON_ERROR_RETURN
		if  (strcmp(station,"BEAM") == 0 || strcmp(station,"ALIGN") == 0)
			continue;
		statinf = gl_store_station( station, TRUE, status );
		ON_ERROR_RETURN
		mean_lat += statinf->lat;
		mean_lon += statinf->lon;
		mean_x += statinf->xrel;
		mean_y += statinf->yrel;
		if  (mean_arrcode == -1)  mean_arrcode = statinf->arr_id;
		else if  (mean_arrcode != 0)
			if  (mean_arrcode != statinf->arr_id)
				mean_arrcode = 0;
		dsplth++;
	} /*endfor*/

	if  (dsplth == 0)  {
		*status= CUE_NOTRACES;
		return;
	} /*endif*/

	if  (lat != NULL)  *lat = mean_lat / (float)dsplth;
	if  (lon != NULL)  *lon = mean_lon / (float)dsplth;
	if  (xrel != NULL)  *xrel = mean_x / (float)dsplth;
	if  (yrel != NULL)  *yrel = mean_y / (float)dsplth;
	if  (arrcode != NULL)  *arrcode = mean_arrcode;

} /* end of cu_reference_location */



#undef ON_ERROR_RETURN



/*--------------------------------------------------------------------------*/



void cu_delete_trace( void *trc )

/* deletes trace from memory
 *
 * parameters of routine
 * void       *trc;       input; pointer to trace
 */
{
	/* local variables */

	/* executable code */

	sy_deallocmem( db_getp(trc,EP_DATA,NULL) );
	if  (db_getp(trc,EP_USR1,NULL) != NULL)
		sy_deallocmem( db_getp(trc,EP_USR1,NULL) );
	if  (db_getp(trc,EP_USR2,NULL) != NULL)
		sy_deallocmem( db_getp(trc,EP_USR2,NULL) );
	db_delist( gc, trc );
	db_delete( trc );

} /* end of cu_delete_trace */



/*--------------------------------------------------------------------------*/



void cu_get_stream_string( TPmTrace *trc, char stream[], STATUS *status )

/* Returns stream string of specified trace.  If status != NULL then
 * error codes are returned otherwise 'X'-characters are copied to the
 * output string.  Minimum length of output string is cBcShortStrLth.
 *
 * parameters of routine
 * TPmTrace   *trc;         input; pointer to trace
 * char       stream[];     output; stream string (e.g. BFO-BH-Z)
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	char     str[cBcLineLth+1];     /* scratch string */
	STATUS   locstat;               /* local status */
	char     comp, ch1, ch2;        /* channel and component info */

	/* executable code */

	locstat = cBcNoError;
	db_gets( trc, ES_STATION, BC_LINELTH, str, &locstat );
	if  (SySevere(&locstat))  {
		if  (status != NULL)  {*status = locstat; return;}
		strcpy( str, "XXX" );
	} /*endif*/
	if  (strlen(str) >= cBcShortStrLth-5)  str[cBcShortStrLth-5] = '\0';

	locstat = cBcNoError;
	comp = db_getc( trc, EC_COMP, &locstat );
	if  (SySevere(&locstat))  {
		if  (status != NULL)  {*status = locstat; return;}
		comp = 'X';
	} /*endif*/

	locstat = cBcNoError;
	ch1 = db_getc( trc, EC_CHAN1, &locstat );
	if  (SySevere(&locstat))  {
		if  (status != NULL)  {*status = locstat; return;}
		ch1 = 'X';
	} /*endif*/

	locstat = cBcNoError;
	ch2 = db_getc( trc, EC_CHAN2, &locstat );
	if  (SySevere(&locstat))  {
		if  (status != NULL)  {*status = locstat; return;}
		ch2 = 'X';
	} /*endif*/

	sprintf( stream, "%s-%c%c-%c", str, ch1, ch2, comp );

} /* end cu_get_stream_string */



/*--------------------------------------------------------------------------*/



void cu_trace_check( int ptrop )

/* Loops all traces in list (specified by ptrop) and prints station names
 * and components
 *
 * parameters of routine
 * int        ptrop;       input; pointer name
 */
{
	/* local variables */
	void     *trc;                   /* pointer to trace */
	TSyStatus locstat;               /* return status */
	char     station[cBcFileLth+1];  /* current station name */
	int      t;                      /* trace counter */

	/* executable code */

	t = 0;
	trc = NULL;
	FOREVER {
		locstat = cBcNoError;
		trc = db_getp( trc, ptrop, &locstat );
		if  (trc == NULL)  break;
		locstat = cBcNoError;
		db_gets( trc, ES_STATION, cBcLineLth, station, &locstat );
		printf( "trcchk: %02d: %s\n", ++t, station );
	} /*endfor*/

} /* end of cu_trace_check */



/*---------------------------------------------------------------------------*/



void cu_lookup_filter( char station[], char ttime[], char tfname[],
	int *trcflags, TSyStatus *status )

/* Tries to find transfer function name for given station.  If not found
 * station name is returned in tfname.  'station' and 'tfname' may be the
 * same variable.
 *
 * parameters of routine
 * char       station[];   input; station name
 * char       ttime[];     input; trace time
 * char       tfname[];    output; filter name
 * int        *trcflags    output; trace flags to be set on created traces
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	char     *env;                       /* pointer to value of SH_INPUTS */
	char     lookup_file[cBcFileLth+1];  /* name of lookup table */
	FILE     *look;                      /* pointer to lookup table file */
	char     line[cBcLineLth+1];         /* current line of file */
	char     stat[cBcLineLth+1];         /* local copy of station name */
	char     l_station[cBcLineLth+1];    /* current station */
	char     l_filter[cBcLineLth+1];     /* current filter */
	char     t_from[cBcLineLth+1];       /* time interval start */
	char     t_until[cBcLineLth+1];      /* time interval end */
	char     flagstr[cBcLineLth+1];      /* additional info */
	float    tdiff;                      /* difference time */
	int      readret;                    /* number of items read */
	int      filecnt;                    /* file counter */
	char     *fl;                        /* pointer to lookup file name */
	TSyStatus locstat;                   /* return status */

	/* executable code */

	/* copy station name */
	if  (strlen(station) > cBcLineLth)  {
		*status = CUE_STROVFL;
		return;
	} /*endif*/
	if  (strncmp(station,"B@",2) == 0)  {
		strcpy( stat, station+2 );
	} else {
		strcpy( stat, station );
	} /*endif*/

	*trcflags = 0;

	/* search built-in table (faster) */
	if  (stat[0] == 'G' && stat[1] == 'R')  {
		if  (strncmp(stat,"GRA1-BH-",8) == 0)  {
			locstat = cBcNoError;
			tdiff = tc_tdiff( ttime, "26-may-2006_20:00", &locstat );
			if  (tdiff < 0.0)  {
				strcpy(tfname,"GRF"); return;
			} else {
				strcpy(tfname,"GRSN"); return;
			} /*endif*/
		} /*endif*/
		if  (strncmp(stat,"GRA2-BH-",8) == 0)  {
			locstat = cBcNoError;
			tdiff = tc_tdiff( ttime, "09-Oct-2006_9:00", &locstat );
			if  (tdiff < 0.0)  {
				strcpy(tfname,"GRF"); return;
			} else {
				strcpy(tfname,"GRSN"); return;
			} /*endif*/
		} /*endif*/
		if  (strncmp(stat,"GRA3-BH-",8) == 0)  {
			locstat = cBcNoError;
			tdiff = tc_tdiff( ttime, "11-Oct-2006_9:00", &locstat );
			if  (tdiff < 0.0)  {
				strcpy(tfname,"GRF"); return;
			} else {
				strcpy(tfname,"GRSN"); return;
			} /*endif*/
		} /*endif*/
		if  (strncmp(stat,"GRA4-BH-",8) == 0)  {
			locstat = cBcNoError;
			tdiff = tc_tdiff( ttime, "25-Sep-2006_8:00", &locstat );
			if  (tdiff < 0.0)  {
				strcpy(tfname,"GRF"); return;
			} else {
				strcpy(tfname,"GRSN"); return;
			} /*endif*/
		} /*endif*/
		if  (strncmp(stat,"GRB1-BH-",8) == 0)  {
			locstat = cBcNoError;
			tdiff = tc_tdiff( ttime, "15-Sep-2006_8:00", &locstat );
			if  (tdiff < 0.0)  {
				strcpy(tfname,"GRF"); return;
			} else {
				strcpy(tfname,"GRSN"); return;
			} /*endif*/
		} /*endif*/
		if  (strncmp(stat,"GRB2-BH-",8) == 0)  {
			locstat = cBcNoError;
			tdiff = tc_tdiff( ttime, "21-jun-2006_9:07", &locstat );
			if  (tdiff < 0.0)  {
				strcpy(tfname,"GRF"); return;
			} else {
				strcpy(tfname,"GRSN"); return;
			} /*endif*/
		} /*endif*/
		if  (strncmp(stat,"GRB3-BH-",8) == 0)  {
			locstat = cBcNoError;
			tdiff = tc_tdiff( ttime, "19-oct-2006_9:00", &locstat );
			if  (tdiff < 0.0)  {
				strcpy(tfname,"GRF"); return;
			} else {
				strcpy(tfname,"GRSN"); return;
			} /*endif*/
		} /*endif*/
		if  (strncmp(stat,"GRB4-BH-",8) == 0)  {
			locstat = cBcNoError;
			tdiff = tc_tdiff( ttime, "28-Sep-2006_10:00", &locstat );
			if  (tdiff < 0.0)  {
				strcpy(tfname,"GRF"); return;
			} else {
				strcpy(tfname,"GRSN"); return;
			} /*endif*/
		} /*endif*/
		if  (strncmp(stat,"GRB5-BH-",8) == 0)  {
			locstat = cBcNoError;
			tdiff = tc_tdiff( ttime, "7-aug-2006_9:00", &locstat );
			if  (tdiff < 0.0)  {
				strcpy(tfname,"GRF"); return;
			} else {
				strcpy(tfname,"GRSN"); return;
			} /*endif*/
		} /*endif*/
		if  (strncmp(stat,"GRC1-BH-",8) == 0)  {
			locstat = cBcNoError;
			tdiff = tc_tdiff( ttime, "15-mar-2005_9:00", &locstat );
			if  (tdiff < 0.0)  {
				strcpy(tfname,"GRF"); return;
			} else {
				strcpy(tfname,"GRSN"); return;
			} /*endif*/
		} /*endif*/
		if  (strncmp(stat,"GRC2-BH-",8) == 0)  {
			locstat = cBcNoError;
			tdiff = tc_tdiff( ttime, "9-may-2006_9:44", &locstat );
			if  (tdiff < 0.0)  {
				strcpy(tfname,"GRF"); return;
			} else {
				strcpy(tfname,"GRSN"); return;
			} /*endif*/
		} /*endif*/
		if  (strncmp(stat,"GRC3-BH-",8) == 0)  {
			locstat = cBcNoError;
			tdiff = tc_tdiff( ttime, "26-jul-2006_9:00", &locstat );
			if  (tdiff < 0.0)  {
				strcpy(tfname,"GRF"); return;
			} else {
				strcpy(tfname,"GRSN"); return;
			} /*endif*/
		} /*endif*/
		if  (strncmp(stat,"GRC4-BH-",8) == 0)  {
			locstat = cBcNoError;
			tdiff = tc_tdiff( ttime, "19-Aug-2006_9:00", &locstat );
			if  (tdiff < 0.0)  {
				strcpy(tfname,"GRF"); return;
			} else {
				strcpy(tfname,"GRSN"); return;
			} /*endif*/
		} /*endif*/
	} else if  (stat[0] == 'G' && stat[1] == 'E')  {
		if  (strcmp(stat,"GEA0-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEA0-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEA0-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEA1-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEA1-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEA1-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEA2-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEA2-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEA2-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEA3-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEA3-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEA3-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB1-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB1-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB1-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB2-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB2-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB2-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB3-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB3-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB3-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB4-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB4-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB4-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB5-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB5-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEB5-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC1-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC1-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC1-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC2-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC2-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC2-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC3-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC3-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC3-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC4-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC4-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC4-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC5-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC5-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC5-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC6-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC6-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC6-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC7-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC7-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GEC7-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED1-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED1-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED1-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED2-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED2-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED2-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED3-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED3-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED3-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED4-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED4-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED4-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED5-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED5-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED5-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED6-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED6-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED6-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED7-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED7-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED7-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED8-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED8-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED8-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED9-SH-Z") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED9-SH-N") == 0)  {strcpy(tfname,"GERESS"); return;}
		if  (strcmp(stat,"GED9-SH-E") == 0)  {strcpy(tfname,"GERESS"); return;}
	} /*endif*/
	if  (strcmp(stat,"BFO-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BFO-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BFO-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BRG-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BRG-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BRG-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BRNL-BH-Z") == 0) {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BRNL-BH-N") == 0) {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BRNL-BH-E") == 0) {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BSEG-BH-Z") == 0) {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BSEG-BH-N") == 0) {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BSEG-BH-E") == 0) {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BUG-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BUG-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"BUG-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"CLL-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"CLL-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"CLL-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"CLZ-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"CLZ-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"CLZ-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"FUR-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"FUR-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"FUR-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"GSH-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"GSH-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"GSH-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"HAM-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"HAM-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"HAM-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"IBB-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"IBB-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"IBB-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"IBBN-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"IBBN-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"IBBN-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"LID-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"LID-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"LID-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"MOX-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"MOX-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"MOX-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"TNS-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"TNS-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"TNS-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"RGN-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"RGN-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"RGN-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"STU-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"STU-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"STU-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"WET-BH-Z") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"WET-BH-N") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"WET-BH-E") == 0)  {strcpy(tfname,"GRSN"); return;}
	if  (strcmp(stat,"GRFO-BH-Z") == 0) {strcpy(tfname,"GRFO"); return;}
	if  (strcmp(stat,"GRFO-BH-N") == 0) {strcpy(tfname,"GRFO"); return;}
	if  (strcmp(stat,"GRFO-BH-E") == 0) {strcpy(tfname,"GRFO"); return;}

	/* search lookup table files */
	for  (filecnt=0;;filecnt++)  {

		/* get next filter lookup file and open it */
		fl = GpGetStringElem( cGpL_filter_lookup_table, filecnt );
		if  (fl == NULL)  break;
		if  (strlen(fl) > cBcFileLth)  {
			*status = CUE_STROVFL;
			return;
		} /*endif*/
		strcpy( lookup_file, fl );
		if  (GpGetInt(cGpI_debug_level) > 6)
			printf( "SHM-dbg7: open filter_lookup %s for %s at %s\n",
				lookup_file, station, ttime );
		look = sy_fopen( lookup_file, "r" );
		if  (look == NULL)  {
			*status = CUE_OPENREAD;
			err_setcontext( " ## file " ); err_setcontext( lookup_file );
			return;
		} /*endif*/

		/* read through this filter lookup file */
		while  (fgets(line,cBcLineLth,look) != NULL)  {
			if  (*line == '!' || *line == '\n')  continue;
			readret = sscanf( line, "%s %s %s %s %s", l_station, l_filter,
				t_from, t_until, flagstr );
			if  (readret < 2 || readret == 3)  {
				/* at least stream name and translation name (filter) must be specified */
				*status = CUE_READ_INPUT;
				err_setcontext( " ## file " ); err_setcontext( lookup_file );
				sy_fclose( look );
				return;
			} /*endif*/
			if  (strcmp(l_station,stat) == 0)  {
				/* check whether a time interval is specified and if time is within this */
				if  (readret >= 4)  {
					if  (strcmp(t_from,"...") != 0)  {
						tdiff = tc_tdiff( ttime, t_from, status );
						if  (SySevere(status))  {sy_fclose(look); return;}
						if  (tdiff < 0.0)  continue;  /* time before start time */
					} /*endif*/
					if  (strcmp(t_until,"...") != 0)  {
						tdiff = tc_tdiff( ttime, t_until, status );
						if  (SySevere(status))  {sy_fclose(look); return;}
						if  (tdiff > 0.0)  continue;  /* time after end time */
					} /*endif*/
				} /*endif*/
				/* now check for flags */
				if  (readret > 4)  {
					if  (strstr(flagstr,"-ALERT-") != NULL)
						*trcflags |= CUC_F_TRC_ALERT;
					if  (strstr(flagstr,"-FORCEFFT-") != NULL)
						*trcflags |= CUC_F_TRC_FORCE_F;
					if  (strstr(flagstr,"-FORCETAB-") != NULL)
						*trcflags |= CUC_F_TRC_FORCE_T;
				} /*endif*/
				strcpy( tfname, l_filter );
				sy_fclose( look );
				return;
			} /*endif*/
		} /*endwhile*/

	} /*endfor*/

	/* nothing found, return station name itself */
	if  (tfname != stat)  strcpy( tfname, stat );
	sy_fclose( look );

	/* replace hyphens '-' by colons ':' */
	env = tfname-1;
	while  (*(++env) != '\0')
		if  (*env == '-')  *env = ':';

} /* end of cu_lookup_filter */



/*--------------------------------------------------------------------------*/

#ifdef XXX

/* this is the old version reading via external_routine */

void cu_next_read_dialog( void )

/* Displays next station list in dialog box
 *
 * parameters of routine
 */
{
	/* local variables */
	static int cnt=0;                /* set counter */
	char     shcmd[cBcLineLth+1];    /* sh command string */
	TSyBoolean  redraw;              /* not used */
	TSyStatus   status;              /* return status */

	/* executable code */

	sprintf( shcmd, "shm_cmd_read_dialog statlist_%02d.stx", ++cnt );
	status = cBcNoError;
	callsh( shcmd, &redraw, &status );

	if  (status != cBcNoError)  {
		status = cBcNoError;
		cnt=1;
		callsh( "shm_cmd_read_dialog statlist_01.stx", &redraw, &status );
	} /*endif*/

} /* end of cu_next_dialog */

#endif

/*--------------------------------------------------------------------------*/



void cu_next_read_dialog( Widget w[] )

/* Displays next station list in dialog box
 *
 * parameters of routine
 * Widget     w[]; input; widget array
 */
{
	/* local variables */
	static int cnt=0;                /* set counter */
	TSyStatus   status;              /* return status */

	/* executable code */

	status = cBcNoError;
	mx_name_read_buttons( w, ++cnt, &status );
	if  (status != cBcNoError)  {
		status = cBcNoError;
		cnt=1;
		mx_name_read_buttons( w, cnt, &status );
	} /*endif*/

} /* end of cu_next_dialog */



/*--------------------------------------------------------------------------*/



void cu_set_exec_flag( int flag, TSyBoolean value )

/* Sets/Clears execution flags
 *
 * parameters of routine
 * int        flag;       input; which flag to set/clear
 * TSyBoolean val;        input; new value of flag
 */
{
	/* executable code */

	if  (value)  {
		cuv_execflags |= flag;
	} else {
		cuv_execflags &= ~flag;
	} /*endif*/

} /* end of cu_set_exec_flag */



/*--------------------------------------------------------------------------*/



TSyBoolean cu_get_exec_flag( int flag )

/* Returns flag status
 *
 * parameters of routine
 * int        flag;      input; flag to return status
 */
{
	/* executable code */

	return (cuv_execflags & flag);

} /* end of cu_get_exec_flag */



/*--------------------------------------------------------------------------*/



void cu_clear_exec_flags( void )

/* Clears all execution flags
 *
 * no parameters
 */
{
	/* executable code */

	cuv_execflags = 0;

} /* end of cu_clear_exec_flags */



/*--------------------------------------------------------------------------*/



void cu_rename_phase( Widget w[] )

/* renames phase from or to unknown phase '???'
 *
 * parameters of routine
 * Widget     w[];     input widget array
 */
{
	/* local variables */
	char     pname[cBcLineLth+1];   /* current phase name */
	int      cnt;                   /* change counter */

	/* executable code */

	strcpy( pname, cu_get_string(w[k_widget_phase_name_text]) );
	if  (PiPhaseIsPicked("???"))  {
		PiRenamePhase( "???", pname, &cnt );
	} else {
		PiRenamePhase( pname, "???", &cnt );
	} /*endif*/

} /* end of cu_rename_phase */



/*--------------------------------------------------------------------------*/




/* file cblib3.c
 *      ========
 *
 * version 104, 12-Nov-2007
 *
 * more library routines
 * K. Stammler, 7-Mar-94
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
#include <Xm/ToggleB.h>
#include <Xm/Scale.h>
#undef BC_DEFINE_TRUE_FALSE
#include BC_SYSBASE
#include "sysext.h"
#include "shvars.h"
#include "infoidx.h"
#include "erusrdef.h"
#include "station_no.h"
#include "glusrdef.h"
#include "utusrdef.h"
#include "earthloc.h"
#include "callsh.h"
#include "../util/eventdsc.h"
#include "shm_widgets.h"
#include "event_id.h"
#include "phaseinf.h"
#include "motifgraph.h"
#include "seismics.h"
#include "cbutil.h"
#include "mfexec.h"
#include "cblib.h"
#include "cblib3.h"
#include "globalparams.h"


#define ALTEXTPROCFILE "external_processes.txt"
#define EXTPROCFILE "plugins.txt"
#define CL3C_MAX_PLUGIN 15

/* #define SAVE_VERSION_ID 1325001L, changed 14-Apr-94, xmv_analpar->xmv_par */
/* #define SAVE_VERSION_ID 1325002L, changed 15-feb-95, err's in CUT_PARAMS */
/* #define SAVE_VERSION_ID 1325003L, changed 10-Feb-97, completely new */
#define SAVE_VERSION_ID 1325004L

typedef struct {
	char        pluginname[cBcFileLth+1];   /* name of plugin */
	char        progname[cBcFileLth+1];     /* program to start */
	char        shname[cBcFileLth+1];       /* SH program to start */
	char        evtfile[cBcFileLth+1];      /* evt file to create */
	char        evtinput[cBcFileLth+1];     /* evt file to create */
	char        waveform[cBcFileLth+1];     /* waveform files to create */
	char        currwindow[cBcFileLth+1];   /* file for current time window */
	TSyBoolean  shell_bg;                   /* run shell in background */
	TSyBoolean  clear_phases;               /* clear all phases and params */
	TSyBoolean  clear_theo;                 /* clear all theoretical phases */
	TSyBoolean  reread_plugins;             /* reread plugin definitions */
} TExtProc;


/* external variable */
extern char *xmv_thpname[];    /* names of theo phases */


/* global variables */
static TExtProc cl3v_extproc[CL3C_PLUGIN_MAXPROC];
XmStringCharSet cl3v_char_set=XmSTRING_DEFAULT_CHARSET;      /* default character set */
#ifdef XPHASE
static cPiPhaselist  **cl3v_plist_root;     /* pointer to phase lists */
static int           cl3v_plist_length;     /* number of phase lists */
#endif



/* prototype of get_geo */
void xxx_get_geo(float *elat, float *elon, int *num_id, char *qnam);

/* prototypes of local routines */
static void cl3h_hypo_station_info( FILE *fp, STATUS *status );
static void cl3h_read_locsat_file( char inpfile[], char reftime[],
	BOOLEAN fixed_depth, CUT_PARAMS *par, STATUS *status );
static void cl3h_hypo_set_phase_flags( STATUS *status );
static void cl3_find_output_phase( void );
static int cl3_phase_ranking( char phase[] );
static void cl3_write_extproc_file( Widget w[] );
static int cl3_next_free_plugin( void );
static void cb3_complete_event_info( CUT_PARAMS *par, EvEventT *event );



/*---------------------------------------------------------------------------*/



#define CLEAR_AND_RETURN \
	*status = CL3E_WRITE_ERR; \
	sy_fclose( fp ); \
	err_setcontext( " ## file " ); err_setcontext( l_file ); \
	return;


void cl3_save_parameters( int mode, CUT_PARAMS *par, TSyStatus *status )

/* Saves analysis parameters and all phases to automatically generated filename
 *
 * parameters of routine
 * int        mode;        input; save mode (CUC_SAVEMODE_...) or event_id
 * CUT_PARAMS *par;        input; analysis parameters
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	static int save_cnt=0;               /* save file counter */
	FILE     *fp;                        /* pointer to file */
	char     l_file[BC_FILELTH+1];       /* local file name */
	long     write_cnt;                  /* number of items written */
	int      p;                          /* phase counter */
	TPiPhaseRoot *proot;                 /* pointer to phase root */
	TPiPhaseList *plist;                 /* pointer to phase list */
	long     version_id;                 /* version ID number */
	long     cmt_length;                 /* length of comment */
	TSyBoolean finalpar;                 /* call to final parameters */
	char     cmd[cBcLineLth+1];          /* shell command */

	/* executable code */

	finalpar = FALSE;

	/* get filename for save output */
	switch  (mode)  {
	case CUC_SAVEMODE_INC:    /* incremental mode */
		if  (++save_cnt == 20)  save_cnt = 0;
		sprintf( l_file, "%sshm_inc_%02d.sav", shd_scratch, save_cnt );
		break;
	case CUC_SAVEMODE_A:
		sprintf( l_file, "%sshm_save_a.sav", shd_scratch );
		break;
	case CUC_SAVEMODE_B:
		sprintf( l_file, "%sshm_save_b.sav", shd_scratch );
		break;
	case CUC_SAVEMODE_C:
		sprintf( l_file, "%sshm_save_c.sav", shd_scratch );
		break;
	default:
		finalpar = TRUE;
		sprintf( l_file, "%sshm_%09d.sav", shd_scratch, mode );
		break;
	} /*endswitch*/

	/* open output file */
	fp = sy_fopen( l_file, "w" );
	if  (fp == NULL)  {
		*status = CL3E_OPEN_WRITE;
		err_setcontext( " ## file " ); err_setcontext( l_file );
		return;
	} /*endif*/

	/* write version ID */
	version_id = SAVE_VERSION_ID;
	write_cnt = fwrite( (char *)(&version_id), sizeof(long), 1, fp );
	if  (write_cnt != 1)  {CLEAR_AND_RETURN}

	/* save parameters */
	write_cnt = fwrite( (char *)par, sizeof(CUT_PARAMS), 1, fp );
	if  (write_cnt != 1)  {CLEAR_AND_RETURN}

	/* write comment */
	cmt_length = (par->comment == NULL) ? 0 : strlen(par->comment)+1;
	write_cnt = fwrite( (char *)(&cmt_length), sizeof(long), 1, fp );
	if  (write_cnt != 1)  {CLEAR_AND_RETURN}
	if  (cmt_length > 0)  {
		write_cnt = fwrite( par->comment, sizeof(char), cmt_length, fp );
		if  (write_cnt != cmt_length)  {CLEAR_AND_RETURN}
	} /*endif*/

	/* save phases */
	proot = NULL;
	FOREVER  {
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		plist = PiPhaseListOfRoot( proot );
		for  (p=0; p<PiListLength(plist); p++)  {
			write_cnt = fwrite( PiDescrOfRoot(proot), sizeof(TPiTrcDescr), 1, fp );
			if  (write_cnt != 1)  {CLEAR_AND_RETURN};
			write_cnt = fwrite( (char *)PiGetPhase(plist,p,status),
				sizeof(TPiPhase), 1, fp );
			if  (write_cnt != 1)  {CLEAR_AND_RETURN}
			if  (Severe(status))  {CLEAR_AND_RETURN}
		} /*endfor*/
	} /*endfor*/

	sy_fclose( fp );

#ifdef XXX
	if  (finalpar && strcmp(GpGetString(cGpS_final_proc),"undefined") != 0)  {
		sprintf( cmd, "%s %s", GpGetString(cGpS_final_proc), l_file );
		printf( "*SHM: calling %s\n", cmd );
		system( cmd );
	} /*endif*/
#endif

} /* end of cl3_save_parameters */



#undef CLEAR_AND_RETURN



/*---------------------------------------------------------------------------*/



#define CLEAR_AND_RETURN \
	*status = CL3E_READ_ERR; \
	sy_fclose( fp ); \
	err_setcontext( " ## file " ); err_setcontext( file ); \
	return;


void cl3_restore_parameters( char file[], CUT_PARAMS *par, TSyStatus *status )

/* Restores analysis parameters and all phases from specified file.  In the
 * 'par'-structure the event number remains unchanged.
 *
 * parameters of routine
 * char       file[];      input; name of output file
 * CUT_PARAMS *par;        output (!); analysis parameters
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	char     l_file[BC_FILELTH+1];       /* local filename */
	FILE     *fp;                        /* pointer to file */
	long     read_cnt;                   /* number of items read */
	TSyStatus locstat;                   /* local status */
	TPiTrcDescr trcdscr;                 /* trace descriptor */
	TPiPhase curr_phase;                 /* current phase */
	int      found_cnt;                  /* found trace counter for phase list */
	long     version_id;                 /* version ID number */
	long     cmt_length;                 /* length of comment */
	void     *old_phase_ptr;             /* pointer to old phases */

	/* executable code */

	/* check filename */
	if  (strcmp(file,"a") == 0)  {
		sprintf( l_file, "%sshm_save_a.sav", shd_scratch );
	} else if  (strcmp(file,"b") == 0)  {
		sprintf( l_file, "%sshm_save_b.sav", shd_scratch );
	} else if  (strcmp(file,"c") == 0)  {
		sprintf( l_file, "%sshm_save_c.sav", shd_scratch );
	} else {
		if  (strlen(file) > BC_FILELTH)  {
			*status = CL3E_STROVFL;
			err_setcontext( " ## file " ); err_setcontext( file );
			return;
		} /*endif*/
		strcpy( l_file, file );
	} /*endif*/

	fp = sy_fopen( l_file, "r" );
	if  (fp == NULL)  {
		*status = CL3E_OPEN_READ;
		err_setcontext( " ## file " ); err_setcontext( l_file );
		return;
	} /* endif*/

	/* get version ID */
	read_cnt = fread( (char *)(&version_id), sizeof(long), 1, fp );
	if  (read_cnt != 1)  {CLEAR_AND_RETURN}
	if  (version_id != SAVE_VERSION_ID)  {
		sy_fclose( fp );
		err_setcontext( " ## file " ); err_setcontext( l_file );
		*status = CL3E_SAVE_VERSION;
		return;
	} /*endif*/

	/* restore parameters except event number */
	read_cnt = fread( (char *)par, sizeof(CUT_PARAMS), 1, fp );
	if  (read_cnt != 1)  {CLEAR_AND_RETURN}

	/* read comment */
	read_cnt = fread( (char *)(&cmt_length), sizeof(long), 1, fp );
	if  (read_cnt != 1)  {CLEAR_AND_RETURN}
	if  (cmt_length == 0)  {
		par->comment = NULL;
	} else {
		par->comment = (char *)sy_allocmem( cmt_length, sizeof(char), status );
		if  (Severe(status))  {sy_fclose(fp); return;}
		read_cnt = fread( par->comment, sizeof(char), cmt_length, fp );
		if  (read_cnt != cmt_length)  {
			sy_deallocmem( par->comment );
			CLEAR_AND_RETURN
		} /*endif*/
	} /*endif*/

	/* on error, memory for par->comment is not freed !!! */

	/* clear existing phases from all traces in memory */
	PiClearAllPhases();

	/* read phases from file */
	while  (!feof(fp))  {

		/* get next phase list from file */
		read_cnt = fread( &trcdscr, sizeof(TPiTrcDescr), 1, fp );
		if  (read_cnt == 0)  break;  /* end of file */
		if  (read_cnt != 1)  {CLEAR_AND_RETURN};
		read_cnt = fread( &curr_phase, sizeof(TPiPhase), 1, fp );
		if  (read_cnt != 1)  {CLEAR_AND_RETURN}
		PiTrcInsertPhase( &trcdscr, &curr_phase, status );
		if  (SySevere(status))  {
			sy_fclose( fp );
			return;
		} /*endif*/

	} /*endwhile*/

	sy_fclose( fp );

	/* setup correct pointers in phases read from file */
	/*cl3_phasetrc_reset();*/

} /* end of cl3_restore_parameters */



#undef CLEAR_AND_RETURN



/*---------------------------------------------------------------------------*/



void cl3_check_filter( char filnam[], BOOLEAN rec, char prefix[],
	BOOLEAN *same_filter, STATUS *status )

/* checks whether the same filter may be used for all traces on display.
 * Returns prefix for filter if rec==TRUE and transfer filter name if
 * a simulation filter is specified in 'filnam'.  Both output strings
 * must have length BC_SHORTSTRLTH+1.  'same_filter' is set to TRUE if all
 * traces on display may be filtered with the same filter.
 *
 * parameters of routine
 * char       filnam[];         input; name of filter
 * BOOLEAN    rec;              input; recursive filter to be used
 * char       prefix[];         output; prefix for filters
 * BOOLEAN    *same_filter;     output; all traces with the same filter
 * STATUS     *status;          output; return status
 */
{
	/* local variables */
	float    dt;                    /* sample distance in sec */
	float    dt_diff;               /* dt difference */
	float    dt_tol;                /* dt tolerance */
	void     *trc;                  /* pointer to trace */
	BOOLEAN  sim;                   /* filter is simulation filter */
	char     tf1[BC_SHORTSTRLTH+1]; /* transfer filter of first trace */
	char     tfc[BC_SHORTSTRLTH+1]; /* transfer filter of current trace */
	char     sta[BC_SHORTSTRLTH+1]; /* station of current trace */
	TIME     ttime;                 /* time values */
	char     strtime[cBcTimeLth+1]; /* start time of trace */
	STATUS   locstat;               /* local status */
	int      trcflags;              /* trace flags for filtered traces */

	/* executable code */

	*prefix = '\0';
	*same_filter = TRUE;

	/* if recursive filters, check sample rates of traces */
	if  (rec)  {
		dt = 0.0;
		trc = NULL;
		FOREVER  {
			trc = db_getp( trc, EP_NEXT, NULL );  /* check all in memory */
			if  (trc == NULL)  break;
			if  (dt == 0.0)  {
				dt = db_getr( trc, ER_DELTA, status );
				if  (Severe(status))  return;
				dt_tol = dt / 100.0;
			} else {
				dt_diff = db_getr( trc, ER_DELTA, status ) - dt;
				if  (Abs(dt_diff) > dt_tol)  {
					*same_filter = FALSE;
					break;
				} /*endif*/
			} /*endif*/
		} /*endfor*/
		if  (*same_filter)
			cl_filter_prefix( NULL, BC_SHORTSTRLTH, prefix );
	} /*endif*/

	/* check seismometers if simulation filter */
	*tf1 = '\0';
	sim = (strchr(filnam,'+') != NULL);
	if  (sim)  {
		trc = NULL;
		FOREVER  {
			trc = db_getp( trc, EP_NEXT, NULL );  /* check all in memory */
			if  (trc == NULL)  break;
			locstat = BC_NOERROR;
			db_gett( trc, ET_START, &ttime, &locstat );
			if  (SySevere(&locstat))  {
				strcpy( strtime, "1-Jul-1991" );
				locstat = cBcNoError;
			} else {
				tc_a2t( &ttime, strtime, &locstat );
			} /*endif*/
			cu_get_stream_string( trc, sta, NULL );
			if  (Severe(status))  {*same_filter = FALSE; break;}
			if  (strncmp(sta,"ALIGN",5) == 0)  continue;
			if  (strncmp(sta,"BEAM",4) == 0)  continue;
			if  (*tf1 == '\0')  {
				cu_lookup_filter( sta, strtime, tf1, &trcflags, status );
				/* cu_get_tf_name( sta, BC_SHORTSTRLTH-1, tf1, status ); */
				if  (Severe(status))  return;
			} else {
				cu_lookup_filter( sta, strtime, tfc, &trcflags, status );
				/* cu_get_tf_name( sta, BC_SHORTSTRLTH, tfc, status ); */
				if  (Severe(status))  return;
				if  (strcmp(tfc,tf1) != 0)  {
					*tf1 = '\0';
					*same_filter = FALSE;
					break;
				} /*endif*/
			} /*endif*/
			if  (trcflags & CUC_F_TRC_ALERT)  *same_filter = FALSE;
			if  (trcflags & (CUC_F_TRC_FORCE_F|CUC_F_TRC_FORCE_T))  {
				*same_filter = FALSE;
				rec = FALSE;
				*prefix = '\0';
			} /*endif*/
		} /*endfor*/
	} /*endif*/

	/* append transfer name and underscore to prefix */
	if  (*tf1 != '\0')  {
		if  (rec)  {
			if  (strlen(prefix)+strlen(tf1)+1 > BC_SHORTSTRLTH)  {
				*status = CL3E_STROVFL;
				return;
			} /*endif*/
			strcat( prefix, tf1 );
			strcat( prefix, "_" );
		} else {
			strcpy( prefix, tf1 );
			strcat( prefix, "_" );
		} /*endif*/
	} /*endif*/

} /* end of cl3_check_filter */



/*---------------------------------------------------------------------------*/



void cl3_readbox_reset( Widget w[], MX_STATLIST *sl )

/* resets read box objects
 *
 * parameters of routine
 * Widget      w[];        input; widget array
 * MX_STATLIST *sl;        input; station list descriptor
 */
{
	/* local variables */
	int      i;                       /* counter */

	/* executable code */

	for  (i=(sl->set1start); i<=(sl->set1end); i++)
		if  (sl->code[i][0] != '-')
			XmToggleButtonSetState( w[k_widget_read_grsn_station+i], TRUE, TRUE );
	for  (i=(sl->set2start); i<=(sl->set2end); i++)
		if  (sl->code[i][0] != '-')
			XmToggleButtonSetState( w[k_widget_read_grsn_station+i], FALSE, TRUE );
	XmToggleButtonSetState( w[k_widget_read_grsn_grf], TRUE, FALSE );
	XmToggleButtonSetState( w[k_widget_read_grsn_grsn], FALSE, FALSE );
	XmToggleButtonSetState( w[k_widget_read_grsn_1hz], FALSE, TRUE );
	XmToggleButtonSetState( w[k_widget_read_grsn_20hz], TRUE, TRUE );
	XmToggleButtonSetState( w[k_widget_read_grsn_80hz], FALSE, TRUE );
	XmToggleButtonSetState( w[k_widget_read_grsn_comp_z], TRUE, TRUE );
	XmToggleButtonSetState( w[k_widget_read_grsn_comp_n], TRUE, TRUE );
	XmToggleButtonSetState( w[k_widget_read_grsn_comp_e], TRUE, TRUE );
	XmToggleButtonSetState( w[k_widget_read_grsn_comp_edit], FALSE, FALSE );

	cu_set_string( w[k_widget_read_grsn_length], "6.0" );
	XmScaleSetValue( w[k_widget_read_grsn_length_scale], 6 );

} /* end of cl3_readbox_reset */



/*---------------------------------------------------------------------------*/



#define WFOUT_FILE "fk_signal.dat"
#define FKHDR_FILE "fk_signal.hdr"
#define FKRES_FILE "fk_signal.out"
#define FK_EXEC "shm_exec_fk"



void cl3_perform_fk( Widget w[], TSyBoolean unit_in_deg, TSyBoolean backgr,
	char oname[], STATUS *status )

/* performs fk analysis
 *
 * parameters of routine
 * Widget     w[];          input; widget array
 * TSyBoolean unit_in_deg;  input; distance unit in degrees (FALSE=km)
 * TSyBoolean backgr;       input; run job in background
 * char       oname[];      input; if not "" then create only files of this name
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	char     wfout[BC_FILELTH+1];       /* waveform output file */
	char     cmd[BC_LONGSTRLTH+1];      /* SH command */
	char     station[BC_SHORTSTRLTH+1]; /* station name */
	BOOLEAN  redraw;                    /* dummy */
	FILE     *hdr;                      /* pointer to header file */
	int      t;                         /* trace counter */
	void     *trc;                      /* trace pointer */
	GLT_STATINF *statinf;               /* station info */
	float    elev;                      /* elevation in m */
	float    xpos, ypos;                /* relative position in m */
	char     *ch;                       /* char pointer */
	float    frqlo, frqhi;              /* frequency bounds */
	float    max_slowness;              /* maximum slowness */
   int      resolution;                /* slowness resolution */
	int      colnum;                    /* number of colors */
	int      decimation;                /* decimation factor */
   int      verbosity;                 /* verbosity */
	int      no_of_trcs;                /* number of traces on display */
	TIME     atime;                     /* absolute start time */
	long     lth;                       /* sample length */
	float    dt;                        /* sample distance in sec */
	char     timeinf[BC_LINELTH+1];     /* time info string */

	/* executable code */

	if  (db_lstlth() <= 1 )  {
		*status = CL3E_LESS_TRACES;
		return;
	} /*endif*/

	/* get time information from first trace */
	trc = db_getp( NULL, EP_DSPN, status );
	if  (trc == NULL)  {*status = CL3E_PROGRAM_BUG; return;}
	db_gett( trc, ET_START, &atime, status );
	if  (Severe(status))  return;
	lth = db_getl( trc, EL_DSPFST, status );
	if  (Severe(status))  return;
	dt = db_getr( trc, ER_DELTA, status );
	if  (Severe(status))  return;
	tc_aadd( &atime, dt*(float)lth, &atime );
	tc_a2t( &atime, timeinf, status );
	if  (Severe(status))  return;
	strcat( timeinf, "\n!* width " );
	lth = db_getl( trc, EL_DSPCNT, status );
	if  (Severe(status))  return;
	sprintf( timeinf+strlen(timeinf), "%5.2f", dt*(float)lth );
	strcat( timeinf, " sec" );

	/* get parameters from dialog box */
	sscanf( cu_get_string(w[k_widget_fk_frqlo_text]), "%f", &frqlo );
	sscanf( cu_get_string(w[k_widget_fk_frqhi_text]), "%f", &frqhi );
	sscanf( cu_get_string(w[k_widget_fk_slowness_text]), "%f", &max_slowness );
	sscanf( cu_get_string(w[k_widget_fk_resol_text]), "%d", &resolution );
	sscanf( cu_get_string(w[k_widget_fk_colnum_text]), "%d", &colnum );
	sscanf( cu_get_string(w[k_widget_fk_decim_text]), "%d", &decimation );
	verbosity = 0;

	/* write header file */
	no_of_trcs = db_dsplth( 0 );
	if  (*oname == '\0')  {
		strcpy( wfout, shd_scratch );
		strcat( wfout, FKHDR_FILE );
	} else {
		strcpy( wfout, oname );
		strcat( wfout, ".hdr" );
	} /*endif*/
	hdr = sy_fopen( wfout, "w" );
	if  (hdr == NULL)  {
		*status = CL3E_OPEN_WRITE;
		err_setcontext( " ## file " ); err_setcontext( wfout );
		return;
	} /*endif*/
	fprintf( hdr, "! header file automatically created by program SHM\n" );
	fprintf( hdr, "!* %s\n", timeinf );
	fprintf( hdr, "%d\n", no_of_trcs );
	trc = NULL;
	for  (t=0; t<no_of_trcs; t++)  {
		trc = db_getp( trc, EP_DSPN, status );
		if  (Severe(status))  return;
		db_gets( trc, ES_STATION, BC_SHORTSTRLTH, station, status );
		if  (Severe(status)) return;
		statinf = gl_store_station( station, TRUE, status );
		if  (Severe(status))  return;
		if  (GpGetInt(cGpI_debug_level) > 3)
			printf( "SHM-dbg4: rloc: %lf %lf\n", statinf->xrel, statinf->yrel );
		if  (!gl_valid_number(statinf->xrel) || !gl_valid_number(statinf->yrel)) {
			*status = CL3E_NO_RELINFO;
			err_setcontext( " ## station " ); err_setcontext( station );
			return;
		} /*endif*/
		xpos = statinf->xrel * 1000.0;
		ypos = statinf->yrel * 1000.0;
		if  (gl_valid_number(statinf->elevation))  {
			elev = statinf->elevation;
		} else {
			elev = 0.0;
			if  (GpGetInt(cGpI_debug_level) > 0)
				printf( "SHM-dbg1: warning: no elevation found at %s\n", station );
		} /*endif*/
		fprintf( hdr, "%10.2f%10.2f%10.2f %s\n", xpos, ypos, elev, station );
		/* fprintf( hdr, " %9.4f %9.4f %9.2f %s\n", xpos, ypos, elev, station );*/
	} /*endfor*/
	fprintf( hdr, " %7.2f %7.2f\n", frqlo, frqhi );
	fprintf( hdr, "%f\n", max_slowness );
	fprintf( hdr, "%d\n", resolution );
	fprintf( hdr, "%d\n", verbosity );
	fprintf( hdr, "%d\n", unit_in_deg );
	sy_fclose( hdr );

	if  (*oname == '\0')  {
		strcpy( wfout, shd_scratch );
		strcat( wfout, WFOUT_FILE );
	} else {
		strcpy( wfout, oname );
		strcat( wfout, ".dat" );
	} /*endif*/
	sy_fdelete( wfout );
	/* change slashes to backslashes */
	ch = wfout - 1;
	while  (*(++ch) != '\0')  if  (*ch == '/')  *ch = '\\';
	if  (cu_get_exec_flag(CUC_F_EXEC_RESIDCORR))  {
		float    slo, az;
		sscanf( cu_get_string(w[k_widget_param_slowness_text]), "%f", &slo );
		sscanf( cu_get_string(w[k_widget_param_azimuth_text]), "%f", &az );
		sprintf( cmd, "BEAM/RESID=P ALL %6.2f %6.2f", az, slo );
		if  (decimation > 1)  printf( "*** decimation not implemented here ***\n" );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: execute command: %s\n", cmd );
		callsh( cmd, &redraw, status );
		if  (Severe(status))  return;
		sprintf( cmd, "BEAM/NEG ALL %6.2f %6.2f", az, slo );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: execute command: %s\n", cmd );
		callsh( cmd, &redraw, status );
		if  (Severe(status))  return;
		sprintf( cmd, "@WRITEA %s ALL", wfout );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: execute command: %s\n", cmd );
		callsh( cmd, &redraw, status );
		if  (Severe(status))  return;
		sprintf( cmd, "TRESET", az, slo );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: execute command: %s\n", cmd );
		callsh( cmd, &redraw, status );
		if  (Severe(status))  return;
	} else if  (decimation > 1)  {
		sprintf( cmd, "copy all" );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: execute command: %s\n", cmd );
		callsh( cmd, &redraw, status );
		sprintf( cmd, "decimate _created %d", decimation );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: execute command: %s\n", cmd );
		callsh( cmd, &redraw, status );
		sprintf( cmd, "@WRITEA %s _CREATED", wfout );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: execute command: %s\n", cmd );
		callsh( cmd, &redraw, status );
		sprintf( cmd, "del _created" );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: execute command: %s\n", cmd );
		callsh( cmd, &redraw, status );
	} else {
		sprintf( cmd, "@WRITEA %s ALL", wfout );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: execute command: %s\n", cmd );
		callsh( cmd, &redraw, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (*oname != '\0')  return;

	sprintf( cmd, "%s%s %s %s %s %d %f", GpGetString(cGpS_defpath_extprog),
		FK_EXEC, FKHDR_FILE, WFOUT_FILE, FKRES_FILE, colnum, max_slowness );
	if  (backgr)  strcat( cmd, " &" );
	if  (GpGetInt(cGpI_debug_level) > 2)
		printf( "SHM-dbg3: executing: %s\n", cmd );
	system( cmd );

} /* end of cl3_perform_fk */



/*---------------------------------------------------------------------------*/



void cl3_fk_get_values( CUT_PARAMS *par, STATUS *status )

/* Reads azimuth and slowness values from output file of FK
 *
 * parameters of routine
 * CUT_PARAMS *par;         modify; output for azimtuh and slowness
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	char     fname[BC_FILELTH+1];  /* file name */
	FILE     *fp;                  /* pointer to file */
	char     line[BC_LINELTH+1];   /* current line */
	BOOLEAN  slowness_found;       /* slowness found */
	BOOLEAN  azimuth_found;        /* azimuth found */

	/* executable code */

	if  (strlen(shd_scratch)+strlen(FKRES_FILE) > BC_FILELTH)  {
		*status = CL3E_STROVFL;
		return;
	} /*endif*/
	strcpy( fname, shd_scratch );
	strcat( fname, FKRES_FILE );
	fp = sy_fopen( fname, "r" );
	if  (fp == NULL)  {
		*status = CL3E_OPEN_READ;
		err_setcontext( " ## file " ); err_setcontext( fname );
		return;
	} /*endif*/

	slowness_found = azimuth_found = FALSE;
	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (strncmp(line,"!* slowness   ",14) == 0)  {
			slowness_found = TRUE;
			sscanf( line+20, "%f", &(par->b_slowness) );
		} /*endif*/
		if  (strncmp(line,"!* azimuth    ",14) == 0)  {
			azimuth_found = TRUE;
			sscanf( line+20, "%f", &(par->b_azimuth) );
		} /*endif*/
		if  (slowness_found && azimuth_found)  break;
	} /*endif*/
	if  (GpGetInt(cGpI_debug_level) > 1)
		if  (!slowness_found)  printf( "SHM-dbg2: no slowness found\n" );
	if  (GpGetInt(cGpI_debug_level) > 1)
		if  (!azimuth_found)  printf( "SHM-dbg2: no azimuth found\n" );
	par->l_slowness = par->l_azimuth = 0.0;

	sy_fclose( fp );

} /* end of cl3_fk_get_values */



#undef WFOUT_FILE
#undef FKHDR_FILE
#undef FKRES_FILE
#undef FK_EXEC



/*---------------------------------------------------------------------------*/



void cl3_preset_fk_values( Widget w[], char filter[] )

/* Preset fk box with reasonable values, depending on filter.  Values are
 * changed only if the filter changes from shortperiod to longperiod or
 * back.
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * char       filter[];   input; name of current filter
 */
{
	/* local variables */
	TSyBoolean  lpfil;                    /* current filter is LP */
	static TSyBoolean  last_lpfil=FALSE;  /* last filter was LP */

	/* executable code */

	/* long period filters are SRO_LP or WWSSN_LP */
	lpfil = (strstr(filter,"SRO_LP") != NULL);
	if  (!lpfil)  lpfil = (strstr(filter,"WWSSN_LP") != NULL);

	/* if filter type did not change, return */
	if  (lpfil == last_lpfil)  return;

	if  (lpfil)  {
		cu_set_string( w[k_widget_fk_frqlo_text], "0.05" );
		cu_set_string( w[k_widget_fk_frqhi_text], "0.2" );
		cu_set_string( w[k_widget_fk_slowness_text], "50.0" );
		cu_set_string( w[k_widget_fk_decim_text], "4" );
	} else {
		cu_set_string( w[k_widget_fk_frqlo_text], "0.4" );
		cu_set_string( w[k_widget_fk_frqhi_text], "3.0" );
		cu_set_string( w[k_widget_fk_slowness_text], "15.0" );
		cu_set_string( w[k_widget_fk_decim_text], "1" );
	} /*endif*/

	last_lpfil = lpfil;

} /* end of cl3_preset_fk_values */



/*---------------------------------------------------------------------------*/



void cl3_uppercase_names( void )

/* Changes all station names to uppercase
 *
 * no parameters
 */
{
	/* local variables */
	void     *trc;                       /* pointer to trace */
	int      t;                          /* trace counter */
	STATUS   locstat;                    /* local status */
	char     statname[BC_SHORTSTRLTH+1]; /* station name */

	/* executable code */

	trc = NULL;
	for  (t=0; t<db_lstlth(); t++)  {
		/* get next trace pointer */
		locstat = BC_NOERROR;
		trc = db_getp( trc, EP_NEXT, &locstat );
		if  (Severe(&locstat))  {printf("*SHM: strange error x1\n");return;}
		db_gets( trc, ES_STATION, BC_SHORTSTRLTH, statname, &locstat );
		if  (Severe(&locstat))  continue;
		ut_cap( statname );
		db_sets( trc, ES_STATION, statname, &locstat );
	} /*endfor*/

} /* end of cl3_uppercase_names */



/*---------------------------------------------------------------------------*/



void cl3_set_theo_phases( Widget w[], STATUS *status )

/* Changes set of theoretical phases computed by cu_theo_arrivals.
 *
 * parameters of routine
 * Widget     w[];         input; widget array
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	char    plist[BC_LONGSTRLTH+1];    /* new phase list */
	int     slen;                      /* string length */
	int     i;                         /* counter */
	BOOLEAN first;                     /* first name in list */
	BOOLEAN redraw;                    /* redraw flag (not used) */
	char    *addtext;                  /* additional phases */

	/* executable code */

	*plist = '\0';
	slen = 0;
	first = TRUE;
	i = 0;
	while  (xmv_thpname[i][0] != '\0')  {
		if  (XmToggleButtonGetState(w[k_widget_theo_phase_P+i]))  {
			if  (!first)  strcat( plist, "," );
			first = FALSE;
			strcat( plist, xmv_thpname[i] );
		} /*endif*/
		i++;
	} /*endwhile*/

	addtext = cu_get_string( w[k_widget_theo_phase_edit] );

	if  (first && *addtext == '\0')  {
		*status = CL3E_NO_PHASE_SEL;
		return;
	} /*endif*/

	if  (*addtext != '\0')  {
		if  (!first)  strcat( plist, "," );
		strcat( plist, addtext );
	} /*endif*/

	GpSetString( cGpS_theo_phase_list, plist, NULL );

} /* end of cl3_set_theo_phases */



/*--------------------------------------------------------------------------*/



int cl3_number_of_stations( void )

/* returns number of stations with phases picked and quality above 1
 *
 * no input parameters
 */
{
	/* local variables */
	TPiPhaseRoot *proot;             /* pointer to phase root */
	TPiPhaseList *phaselist;         /* phase list */
	TPiPhase *t_phase;               /* current phase */
	TSyBoolean count_station;        /* count this station */
	int      q;                      /* phase counter */
	char     statlist[cBcLongStrLth+1]; /* station list */
	char     station[cBcLineLth+1];  /* current station */
	int      statcnt;                /* station counter */

	/* executable code */

	strcpy( statlist, "," );

	/* loop all phase roots */
	statcnt = 0;
	proot = NULL;
	FOREVER  {
		count_station = FALSE;
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		phaselist = PiPhaseListOfRoot( proot );
		/* if no phases dont' count this station */
		if  (phaselist == NULL)  continue;
		/* check whether at least one of the phases is not theo and has qual>1 */
		for  (q=0; q<PiPhaseListLength(phaselist); q++)  {
			t_phase = PiGetPhase( phaselist, q, NULL );
			if  (t_phase->source != cPiSourceTheo && t_phase->name[0] != 'b'
				&& t_phase->quality > 1)  {
				count_station = TRUE;
				break;
			} /*endif*/
		} /*endfor*/
		/* count station if not yet in list */
		if  (count_station)  {
			strcpy( station, "," );
			strcat( station, PiStationOfRoot(proot) );
			strcat( station, "," );
			if  (strstr(statlist,station) == NULL)  {
				if  (strlen(station)+strlen(statlist)-1 < cBcLongStrLth)  {
					statcnt++;
					strcat( statlist, PiStationOfRoot(proot) );
					strcat( statlist, "," );
				} /*endif*/
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	return statcnt;

} /* end of cl3_number_of_stations */



/*--------------------------------------------------------------------------*/



void cl3_dump_parameters( CUT_PARAMSETS *parset, char infofile[],
	TSyBoolean append, TSyBoolean screen_output, TSyBoolean do_edit,
	TSyStatus *status )

/* prints out all information.  Sets event ID in "par"-structure if a final
 * file is to be created (infofile=="--automatic--").
 *
 * parameters of routine
 * CUT_PARAMS *parset;       modify; analysis parameters
 * char       infofile[];    input; name of info file to be created
 * TSyBoolean append;        input; append info to file / create new file
 * TSyBoolean screen_output; input; print info to screen
 * TSyBoolean do_edit;       input; open output file with editor
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	int      q;                      /* phase counter */
	TPiPhaseRoot *proot;             /* pointer to phase root */
	TPiPhaseList *phaselist;         /* phase list */
	TPiPhase *t_phase;               /* current phase */
	TPiPhase *l_phase;               /* last phase */
	TSyStatus locstat;               /* local status */
	int      max_no_phases;          /* maximum number of phases */
	char     station[BC_SHORTSTRLTH+1];      /* current station */
	char     last_station[BC_SHORTSTRLTH+1]; /* previous station */
	EvEventT event;                  /* event parameters */
	TSyBoolean first;                /* first output */
	int      onset_count;            /* number of onsets following */
	char     evtfile[BC_FILELTH+1];  /* output file for parameters */
	char     viewfile[BC_FILELTH+1]; /* view file */
	char     editfile[BC_FILELTH+1]; /* edit file */
	char     edcmd[BC_LONGSTRLTH+1]; /* edit command */
	TSyBoolean is_main_phase;        /* is the phase which was used to locate */
	TSyBoolean ref_station_found;    /* reference station found */
	TSyBoolean is_ref_station;       /* current station is reference station */
	TSyBoolean print_info;           /* print special info */
	TSyBoolean final_file;           /* create final parameter file */
	TSyBoolean evfile_created;       /* event file created */
	TSyBoolean first_at_station;     /* first phase at station */
	TSyBoolean slowfound;            /* phase slowness found */
	char     compname;               /* name of component */
	unsigned dist_entry;             /* info entry 'DISTANCE' */
	float    tmp;                    /* scratch */
	CUT_PARAMS *par;                 /* pointer to current parameters */
	double   d_dist, d_azim, d_bazim;/* output of mb_locdiff */
	GLT_STATINF *statinf;            /* station info */
	char     *cmtptr;                /* pointer to start of comment */
	char     *cptr;                  /* moving char pointer */
	float    cslow, cazim;           /* corrected slowness and azimuth */

	/* executable code */

	cl3_find_output_phase();

	par = parset->par + parset->parno;

	max_no_phases = 0;
	ref_station_found = FALSE;
	proot = NULL;
	FOREVER  {
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		phaselist = PiPhaseListOfRoot( proot );
		if  (phaselist == NULL)  continue;
		strcpy( station, PiStationOfRoot(proot) );
		if  (!ref_station_found
			&& strcmp(station,GpGetString(cGpS_refstation)) == 0)
			ref_station_found = TRUE;
		if  (PiPhaseListLength(phaselist) > max_no_phases)
			max_no_phases = PiPhaseListLength( phaselist );
		if  (screen_output)
			for  (q=0; q<PiPhaseListLength(phaselist); q++)  {
				t_phase = PiGetPhase( phaselist, q, NULL );
				if  (t_phase->source != cPiSourceTheo && t_phase->name[0] != 'b')
					cu_print_phase( t_phase );
			} /*endfor*/
	} /*endfor*/

	/* return if no phases found */
	if  (max_no_phases == 0)  {
		*status = CL3E_NO_PHASES;
		return;
	} /*endif*/

	onset_count = 0;    /* this must be changed !!! ??? */

	/* get info entry 'DISTANCE' */
	db_ident( "DISTANCE", &dist_entry, status );
	if  (Severe(status))  return;

	/* is this a final parameter file ? */
	final_file = (strcmp(infofile,"--automatic--") == 0);

	/* rest of routine puts it to ".evt"-file: */
	first = TRUE;
	*evtfile = '\0';
	evfile_created = FALSE;

	/* loop all traces on display */
	*station = *last_station = '\0';
	proot = NULL;
	FOREVER  {

		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		strcpy( station, PiStationOfRoot(proot) );
		compname = PiCompOfRoot( proot );
		phaselist = PiPhaseListOfRoot( proot );
		if  (phaselist == NULL)  continue;
		is_ref_station = (strcmp(station,GpGetString(cGpS_refstation)) == 0);

		if  (strcmp(station,last_station) != 0)
			first_at_station = TRUE;

		/* loop all phases of trace */
		l_phase = NULL;
		for  (q=0; q<PiPhaseListLength(phaselist); q++)  {

			/* t_phase = PiGetPhase( phaselist, q, status ); */
			t_phase = PiNextPhase( phaselist, l_phase, status );
			l_phase = t_phase;
			if  (Severe(status))  return;
			if  (t_phase == NULL)  break;

			/* ignore theo phases and pseudo-phases (starting with "b..") */
			if  (t_phase->source == cPiSourceTheo)  continue;
			if  (t_phase->name[0] == 'b' ||
				(t_phase->name[0] == '(' && t_phase->name[1] == 'b'))  continue;

			/* set event ID */
			if  (parset->evid == 0 && t_phase->onset[0] != '\0'
				&& final_file)  {
				locstat = cBcNoError;
				EiSetPath( GpGetString(cGpS_defpath_evid), &locstat );
				if  (locstat == cBcNoError)
					parset->evid = EiGetEventID( t_phase->onset, &locstat );
			} /*endif*/

			/* create filename for output if necessary */
			if  (*evtfile == '\0' && final_file)  {
				if  (strlen(GpGetString(cGpS_defpath_evtout)) > BC_FILELTH-17)  {
					*status = CUE_STROVFL;
					return;
				} /*endif*/
				strcpy( evtfile, GpGetString(cGpS_defpath_evtout) );
				strcat( evtfile, "shm_" );
				sprintf( evtfile+strlen(evtfile), "%09ld", parset->evid );
				strcat( evtfile, ".evt" );
			} else if  (*evtfile == '\0')  {
				strcpy( evtfile, infofile );
			} /*endif*/

			/* first write comment if specified */
			if  (first && par->comment != NULL)  {
				EvInitializeEvent( &event );
				if  (parset->evid > 0)
					event.evid = parset->evid;
				locstat = BC_NOERROR;
				cmtptr = par->comment;
				while  (*cmtptr == '\n' || *cmtptr == ' ' && *cmtptr != '\0')
					cmtptr++;
				EvAddComment( &event.comment, cmtptr, &locstat );
				EvCreateEventfile( evtfile, &event, &locstat );
				evfile_created = TRUE;
			} /*endif*/

			/* copy all information to EvEventT structure */
			EvInitializeEvent( &event );
			if  (parset->evid > 0)
				event.evid = parset->evid;
			locstat = BC_NOERROR;
			event.component = compname;
			strcpy( event.analyst, GpGetString(cGpS_analyst) );
			/* event.onset_count = onset_count; */
			if  (t_phase->name[0] != '\0')
				if  (t_phase->reliable)  {
					strcpy( event.phase, t_phase->name );
				} else {
					sprintf( event.phase, "(%s)", t_phase->name );
				} /*endif*/
			is_main_phase = ( (strcmp(par->phase,t_phase->name) == 0)
				|| (par->phase[0] == '\0' && first_at_station)
				|| (par->phase[0] == 'b' && first_at_station) );
			/*
			print_info = (ref_station_found) ? is_main_phase && is_ref_station :
				is_main_phase && first;
			*/
			print_info = (t_phase->flags & fPiAttribOutput);
			if  (t_phase->filter[0] != '\0' &&
				strlen(t_phase->filter) < EvFILTERLTH)  {
				if  (strncmp(t_phase->filter,"S+",2) == 0)  {
					strcpy( event.filter, (t_phase->filter)+2 );
				} else {
					strcpy( event.filter, t_phase->filter );
				} /*endif*/
			} /*endif*/
			if  (t_phase->period > 0.0)
				event.period = t_phase->period;
			if  (t_phase->ampl_displ > 0.0)
				event.amplitude = t_phase->ampl_displ;
			if  (t_phase->ampl_veloc > 0.0)
				event.amplitude_vel = t_phase->ampl_veloc;
			if  (t_phase->ampl_time > 0.0)
				event.amplitude_time = t_phase->ampl_time;
			if  (t_phase->bb.bbampl > 0.0)
				event.bb_amplitude = t_phase->bb.bbampl;
			if  (t_phase->bb.bbperiod > 0.0)
				event.bb_period = t_phase->bb.bbperiod;
			if  (t_phase->bb.mbb > 0.0)
				event.mag[EvcMagMbb] = t_phase->bb.mbb;
			if  (t_phase->onset_acc_l != cPiAccEmpty)
				event.onset_wdw_l = t_phase->onset_acc_l;
			if  (t_phase->onset_acc_r != cPiAccEmpty)
				event.onset_wdw_r = t_phase->onset_acc_r;
			if  (t_phase->ap_source != cPiApsrcUndefined)
				switch  (t_phase->ap_source)  {
				case cPiApsrcDirect:  event.ap_source = EvcApSourceDirect; break;
				case cPiApsrcBeam:    event.ap_source = EvcApSourceBeam;   break;
				case cPiApsrcAlign:   event.ap_source = EvcApSourceAlign;  break;
				default:              event.ap_source = EvcApSourceUndefined;
				} /*endswitch*/
			if  (par->slowbox.lth > 0)  {
				PiSbGetSlowness( &(par->slowbox), t_phase->name,
					&(event.b_slowness), &(event.b_azimuth), &cslow, &cazim,
					&slowfound );
				if  (cslow > 0.0 || cazim > 0.0)  {
					event.l_slowness = cslow;
					event.l_azimuth = cazim;
				} /*endif*/
			} else {
				if  ((par->l_slowness > 0.0 || par->l_azimuth > 0.0)
					&& si_is_first_onset("tele",t_phase->name))  {
					event.l_slowness = par->l_slowness;
					event.l_azimuth = par->l_azimuth;
				} /*endif*/
				if  ((par->b_slowness > 0.0 || par->b_azimuth > 0.0)
					&& si_is_first_onset("tele",t_phase->name))  {
					event.b_slowness = par->b_slowness;
					event.b_azimuth = par->b_azimuth;
				} /*endif*/
			} /*endif*/
			if  (print_info)  {
				cb3_complete_event_info( par, &event );
				switch  (par->loc_meth)  {
				case CUC_LOCMETH_BEAMCORR:
					event.loc_method = EvcLocMethCorrBeam;
					break;
				case CUC_LOCMETH_BEAMUNCORR:
					event.loc_method = EvcLocMethUncorrBeam;
					break;
				case CUC_LOCMETH_RESIDCORR:
					event.loc_method = EvcLocMethResidCorr;
					break;
				case CUC_LOCMETH_HYPO:
					event.loc_method = EvcLocMethHypo;
					break;
				case CUC_LOCMETH_LOCSAT:
					event.loc_method = EvcLocMethLocsat;
					break;
				case CUC_LOCMETH_HYPOCENTER:
					event.loc_method = EvcLocMethHypocenter;
					break;
				case CUC_LOCMETH_HYPO71:
					event.loc_method = EvcLocMethHypo71;
					break;
				case CUC_LOCMETH_HYPOSAT:
					event.loc_method = EvcLocMethHyposat;
					break;
				case CUC_LOCMETH_RELTRAV:
					event.loc_method = EvcLocMethRelTrav;
					break;
				default:
					event.loc_method = EvcLocMethUndefined;
					break;
				} /*endswitch*/
				switch  (par->loc_quality)  {
				case CUC_LOCQ_TOOWEAK:
					event.loc_quality = EvcLocQualTooWeak;
					break;
				case CUC_LOCQ_INCOHERENT:
					event.loc_quality=EvcLocQualIncoherent;
					break;
				case CUC_LOCQ_NOBEARING:
					event.loc_quality=EvcLocQualNoBearing;
					break;
				case CUC_LOCQ_REGION:
					event.loc_quality=EvcLocQualRegion;
					break;
				case CUC_LOCQ_RELIABLE:
					event.loc_quality=EvcLocQualReliable;
					break;
				default:
					event.loc_quality = EvcLocQualUndefined;
					break;
				} /*endswitch*/
				if  (par->mu_descr[0] != '\0')
					strcpy( event.mu_descr, par->mu_descr );
				if  (par->cornerfreq > 0.0)  event.cornerfreq = par->cornerfreq;
				if  (par->lowfreqlevel > 0.0)  event.lowfreqlevel = par->lowfreqlevel;
				if  (par->m0 > 0.0)  event.m0 = par->m0;
				if  (par->velmod[0] != '\0')  strcpy( event.velmod, par->velmod );
				if  (par->loc_addpar[0] != '\0')  strcpy( event.loc_addpar, par->loc_addpar );
				if  (par->momten[0] != '\0')  strcpy( event.momten, par->momten );
				if  (par->momten_descr[0] != '\0')  strcpy( event.momten_descr, par->momten_descr );
				if  (par->fps_angles[0] != '\0')  strcpy( event.fps_angles, par->fps_angles );
				if  (par->fps_descr[0] != '\0')  strcpy( event.fps_descr, par->fps_descr );
			} /*endif*/
			if  (par->stations_used > 0 && print_info)
				event.stations_used = par->stations_used;
			if  (first_at_station)  {
				if  (strcmp(station,"ALIGN") == 0
					|| strcmp(station,"BEAM") == 0)  {
					/* don't put distance to station */
				} else if  (par->source_lat != 0.0 || par->source_lon != 0.0)  {
					statinf = gl_store_station( station, TRUE, status );
					if  (Severe(status))  return;
					mb_locdiff( statinf->lat, statinf->lon, par->source_lat,
						par->source_lon, &d_dist, &d_azim, &d_bazim );
					event.distance_deg = (float)d_dist;
					event.theo_azim = (float)d_azim;
					event.theo_back_azim = (float)d_bazim;
					if  (d_dist < 5.0)
						event.distance_km = (float)(d_dist * CUC_DEG_TO_KM);
				} else if  (par->distance > 0.0 && print_info)  {
					if  (par->dist_unit == CUC_DIST_UNIT_DEG)
						event.distance_deg = par->distance;
					else
						event.distance_km = par->distance;
				} /*endif*/
			} /*endif*/
			if  (par->depth_type != CUC_DEPTH_UNDEFINED && print_info)  {
				event.depth = par->depth;
				switch  (par->depth_type)  {
				case CUC_DEPTH_PRESET:    event.depth_type=EvcDepthPreset; break;
				case CUC_DEPTH_ESTIMATED: event.depth_type=EvcDepthEstimated; break;
				case CUC_DEPTH_FREE:      event.depth_type=EvcDepthFree; break;
				case CUC_DEPTH_POOR:      event.depth_type=EvcDepthPoor; break;
				case CUC_DEPTH_LESSWELL:  event.depth_type=EvcDepthLessWell; break;
				case CUC_DEPTH_RELIABLE:  event.depth_type=EvcDepthReliable; break;
				case CUC_DEPTH_EXTERNAL:  event.depth_type=EvcDepthExternal; break;
				default:                  event.depth_type=EvcDepthUndefined; break;
				} /*endswitch*/
			} /*endif*/
			if  ((par->source_lat != 0.0 || par->source_lon != 0.0)
				&& print_info)  {
				event.latitude = par->source_lat;
				event.longitude = par->source_lon;
			} /*endif*/
			switch  (par->source_type)  {
			case CUC_TYPE_TELE_QUAKE:
				event.event_type = EvcEventTypeTeleQuake;
				break;
			case CUC_TYPE_NUCLEAR:
				event.event_type = EvcEventTypeNuclear;
				break;
			case CUC_TYPE_REGIO_QUAKE:
				event.event_type = EvcEventTypeRegioQuake;
				break;
			case CUC_TYPE_LOCAL_QUAKE:
				event.event_type = EvcEventTypeLocalQuake;
				break;
			case CUC_TYPE_BLAST:
				event.event_type = EvcEventTypeBlast;
				break;
			case CUC_TYPE_MINING:
				event.event_type = EvcEventTypeMining;
				break;
			default:
				event.event_type = EvcEventTypeUndefined;
				break;
			} /*endswitch*/
			if  (par->origin[0] != '\0' && print_info)
				strcpy( event.origin_time, par->origin );
			if  (par->regname[0] != '\0' && strlen(par->regname) < EvREGIONLTH
				&& print_info)
				strcpy( event.region, par->regname );
			if  (par->reg_id > 0 && print_info)  {
				event.region_id = par->reg_id;
				event.region_table = (par->table_number == CUC_REGTABLE_GERGEO) ?
					EvcRegionTableGermanGeo : EvcRegionTableFlinnEngdahl;
			} /*endif*/
			strcpy( event.station, station );
			if  (t_phase->onset[0] != '\0')
				strcpy( event.onset_time, t_phase->onset );
			if  (print_info)
				strcpy( event.ref_name, GpGetString(cGpS_refstation) );
			if  ((par->ref_lat != 0.0 || par->ref_lon != 0.0) && print_info)  {
				event.ref_latitude = par->ref_lat;
				event.ref_longitude = par->ref_lon;
			} /*endif*/
			if  (print_info && strlen(par->source) < EvSOURCELTH)  {
				strcpy( event.source, par->source );
				if  (event.source[0] == '\0')
					strcpy( event.source, GpGetString(cGpS_default_source) );
			} /*endif*/
			switch  (t_phase->spec)  {
			case cPiSpecEmergent:  event.onset_type = EvcOnsetEmergent;  break;
			case cPiSpecImpulsive: event.onset_type = EvcOnsetImpulsive; break;
			default:               event.onset_type = EvcOnsetUndefined; break;
			} /*endswitch*/
			switch (t_phase->source)  {
			case cPiSourceManually: event.pick_type = EvcPickTypeManual;     break;
			case cPiSourceAuto:     event.pick_type = EvcPickTypeAuto;       break;
			case cPiSourceTheo:     event.pick_type = EvcPickTypeTheo;       break;
			default:                event.pick_type = EvcPickTypeUndefined;  break;
			} /*endswitch*/
			switch  (t_phase->sign)  {
			case -1:       event.sign = EvcSignNegative;  break;
			case 1:        event.sign = EvcSignPositive;  break;
			default:       event.sign = EvcSignUndefined; break;
			} /*endswitch*/
			event.weight = t_phase->weight;
			event.quality = t_phase->quality;
			if  (t_phase->signoise != 0.0)
				event.signoise = t_phase->signoise;
			if  (t_phase->resid != 0.0)
				event.residual = t_phase->resid;
			if  (t_phase->resid_corr != 0.0)
				event.resid_corr = t_phase->resid_corr;
			if  (t_phase->magnitude > 0.0)  {
				if  (t_phase->magn_source == cPiMagnMl)  {
					event.mag[EvcMagMl] = t_phase->magnitude;
				} else if  (t_phase->magn_source == cPiMagnMb)  {
					event.mag[EvcMagMb] = t_phase->magnitude;
				} else if  (t_phase->magn_source == cPiMagnMbb)  {
					event.mag[EvcMagMbb] = t_phase->magnitude;
				} else if  (t_phase->magn_source == cPiMagnMw)  {
					event.mag[EvcMagMw] = t_phase->magnitude;
				} else if  (t_phase->magn_source == cPiMagnMu)  {
					event.mag[EvcMagMu] = t_phase->magnitude;
				} else {
					event.mag[EvcMagMs] = t_phase->magnitude;
				} /*endif*/
			} /*endif*/
			locstat = BC_NOERROR;
			if  (t_phase->comment[0] != '\0')
				EvAddComment( &(event.comment), t_phase->comment,
					&locstat );
			/* parameter error values */
			if  (print_info && par->err.lat != 0.0)
				event.err.lat_km = (par->err.dist_unit == CUC_DIST_UNIT_KM)
					? par->err.lat : par->err.lat * CUC_DEG_TO_KM;
			if  (print_info && par->err.lon != 0.0)
				event.err.lon_km = (par->err.dist_unit == CUC_DIST_UNIT_KM)
					? par->err.lon : par->err.lon * CUC_DEG_TO_KM;
			if  (print_info && (par->err.dep != 0.0
				|| par->depth_type == CUC_DEPTH_FREE))
				event.err.dep = par->err.dep;
			if  (print_info && par->err.orig != 0.0)
				event.err.orig = par->err.orig;
			if  (print_info && par->err.smajor != 0.0)
				event.err.smajor = (par->err.dist_unit == CUC_DIST_UNIT_DEG)
					? par->err.smajor : par->err.smajor / CUC_DEG_TO_KM;
			if  (print_info && par->err.sminor != 0.0)
				event.err.sminor = (par->err.dist_unit == CUC_DIST_UNIT_DEG)
					? par->err.sminor : par->err.sminor / CUC_DEG_TO_KM;
			if  (print_info && par->err.majstrike != 0.0)
				event.err.majstrike = par->err.majstrike;
			if  (print_info && par->err.azim_max_gap != 0.0)
				event.err.azim_max_gap = par->err.azim_max_gap;
			if  (print_info && par->err.resid_rms != 0.0)
				event.err.resid_rms = par->err.resid_rms;
			/* phase flags */
			cptr = event.phase_flags;
			*cptr = EvEOS;
			if  (t_phase->flags & fPiAttribMagn)  {
				*cptr++ = EvFLAG_MAGNITUDE;
				*cptr = EvEOS;
			} /*endif*/
			if  (t_phase->flags & fPiAttribLoc)  {
				*cptr++ = EvFLAG_LOCATION;
				*cptr = EvEOS;
			} /*endif*/
			if  (t_phase->flags & fPiAttribBeam)  {
				*cptr++ = EvFLAG_BEAM;
				*cptr = EvEOS;
			} /*endif*/
			if  (par->flags & CUC_F_EVENT_CALIB)  {
				*cptr++ = EvFLAG_CALIBEVENT;
				*cptr = EvEOS;
			} /*endif*/
			if  (par->flags & CUC_F_EVENT_IGNORE)  {
				*cptr++ = EvFLAG_IGNORE;
				*cptr = EvEOS;
			} else {
				if  (par->flags & CUC_F_EVENT_TELEX_ALL || is_ref_station)  {
					*cptr++ = EvFLAG_TELEX;
					*cptr = EvEOS;
				} /*endif*/
			} /*endif*/

			/* write to output file */
			locstat = BC_NOERROR;
			if  (append || evfile_created)  {
				EvAppendEventfile( evtfile, &event, &locstat );
			} else {
				EvCreateEventfile( evtfile, &event, &locstat );
				evfile_created = TRUE;
			} /*endif*/
			first = FALSE;
			first_at_station = FALSE;

		} /*endfor*/
	} /*endfor*/

	if  (do_edit)  {
		strcpy( editfile, evtfile );
		/* prepare output evt-file for viewing if requested */
		cptr = GpGetString( cGpS_evtview_proc );
		if  (cptr[0] != '\0')  {
			sprintf( viewfile, "%s%s", shd_scratch, "evt_view.txt" );
			sprintf( edcmd, "%s%s %s %s", GpGetString(cGpS_defpath_extprog),
				cptr, evtfile, viewfile );
			system( edcmd );
			strcpy( editfile, viewfile );
		} /*endif*/
		sprintf( edcmd, "%s %s", GpGetString(cGpS_texteditor), editfile );
		strcat( edcmd, " &" );
		system( edcmd );
	} /*endif*/

	cptr = GpGetString( cGpS_event_check_proc );
	if  (final_file && cptr[0] != '\0')  {
		sprintf( edcmd, "%s %s &", cptr, evtfile );
		system( edcmd );
	} /*endif*/

} /* end of cl3_dump_parameters */



/*--------------------------------------------------------------------------*/



void cl3_regio_localization( Widget w[], char phasename[],
	CUT_PARAMSETS *parset, TSyStatus *status )

/* interface to regional localization using external program
 *
 * parameters of routine
 * Widget     w[];              input; all widgets
 * char       phasename[];      input; phasename to be used (currently ignored)
 * CUT_PARAMSETS *parset;       output; localization parameters
 * TSyStatus  *status;          output; return status
 */
{
	/* local variables */
	char     cmd[BC_LONGSTRLTH+1];   /* system command */
	char     root[BC_FILELTH+1];     /* root path for output */
	char     evtfile[BC_FILELTH+1];  /* input event file */
	char     header[BC_FILELTH+1];   /* hypoellipse header file */
	char     result[BC_FILELTH+1];   /* translated hypoellipse output file */
	FILE     *fp;                    /* pointer to result file */
	EvEventT event;                  /* event info */
	TSyBoolean eof;                  /* end of file found */
	char     *depth_val;             /* pointer to depth value */
	TSyBoolean use_s;                /* use S waves */
	CUT_PARAMS *par;                 /* pointer to current parameters */
	TSyBoolean german_event;         /* this is a german event */

	/* executable code */

	par = parset->par + parset->parno;

	strcpy( evtfile, shd_scratch );
	strcat( evtfile, "locate_in.evt" );
	strcpy( result, shd_scratch );
	strcat( result, "locate_out.evt" );

	cl3_dump_parameters( parset, evtfile, FALSE, FALSE, FALSE, status );
	if  (Severe(status))  return;

	/* build system command */
	sprintf( cmd, "%s/%s %s", GpGetString(cGpS_defpath_extprog),
		"locate_external.csh", evtfile );

	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: locate_external: executed command is: %s\n", cmd );
	system( cmd );

	/* read in translated output file of hypoellipse */
	fp = sy_fopen( result, "r" );
	if  (fp == NULL)  {
		fprintf( stderr,
			"*SHM: locate_external: couldn't find file %s\n", result );
		*status = CUE_OPENREAD;
		return;
	} /*endif*/
	EvGetEvent( fp, &event, &eof, status );
	sy_fclose( fp );

	if  (eof || Severe(status))  {
		fprintf( stderr, "regio_loc: input error on file %s\n", result );
		if  (eof)  *status = CUE_READ_INPUT;
		return;
	} /*endif*/

	/* put new values */
	/* cu_reset_parameters( par ); */
	par->depth = event.depth;
	switch  (event.depth_type)  {
	case EvcDepthPreset   :  par->depth_type = CUC_DEPTH_PRESET;  break;
	case EvcDepthEstimated:  par->depth_type = CUC_DEPTH_ESTIMATED;  break;
	case EvcDepthFree     :  par->depth_type = CUC_DEPTH_FREE;  break;
	case EvcDepthPoor     :  par->depth_type = CUC_DEPTH_POOR;  break;
	case EvcDepthLessWell :  par->depth_type = CUC_DEPTH_LESSWELL;  break;
	case EvcDepthReliable :  par->depth_type = CUC_DEPTH_RELIABLE;  break;
	case EvcDepthExternal :  par->depth_type = CUC_DEPTH_EXTERNAL;  break;
	default:
		if  (par->depth_type == CUC_DEPTH_UNDEFINED)
			par->depth_type = CUC_DEPTH_ESTIMATED;
	} /*endswitch*/
	/* strcpy( par->origin, event.origin_time ); */
	par->source_lat = event.latitude;
	par->source_lon = event.longitude;
	german_event = (event.latitude >= 47.0 && event.latitude <= 55.0
		&& event.longitude >= 5.0 && event.longitude <= 15.0);
	if  (german_event)  {
		xxx_get_geo( &(par->source_lat), &(par->source_lon),
			&(par->reg_id), par->regname );
		if  (par->reg_id < 0)  {
			*status = CUE_NOGETGEO;
			return;
		} /*endif*/
		par->table_number = CUC_REGTABLE_GERGEO;
	} else {
		mb_ferindex( par->source_lat, par->source_lon, &(par->reg_id), status );
		if  (Severe(status))  return;
		mb_fername( par->reg_id, BC_LINELTH, par->regname, status );
		par->loc_meth = CUC_LOCMETH_RELTRAV;
		if  (Severe(status))  return;
		par->table_number = CUC_REGTABLE_FLINNENG;
	} /*endif*/
	if  (event.origin_time[0] != '\0')
		strcpy( par->origin, event.origin_time );

	switch  (event.loc_method)  {
	case EvcLocMethUndefined   :  par->loc_meth = CUC_LOCMETH_UNDEFINED;  break;
	case EvcLocMethCorrBeam    :  par->loc_meth = CUC_LOCMETH_BEAMCORR;   break;
	case EvcLocMethUncorrBeam  :  par->loc_meth = CUC_LOCMETH_BEAMUNCORR; break;
	case EvcLocMethResidCorr   :  par->loc_meth = CUC_LOCMETH_RESIDCORR;  break;
	case EvcLocMethHypo        :  par->loc_meth = CUC_LOCMETH_HYPO;       break;
	case EvcLocMethLocsat      :  par->loc_meth = CUC_LOCMETH_LOCSAT;     break;
	case EvcLocMethHypocenter  :  par->loc_meth = CUC_LOCMETH_HYPOCENTER; break;
	case EvcLocMethHypo71      :  par->loc_meth = CUC_LOCMETH_HYPO71;     break;
	case EvcLocMethHyposat     :  par->loc_meth = CUC_LOCMETH_HYPOSAT;    break;
	case EvcLocMethRelTrav     :  par->loc_meth = CUC_LOCMETH_RELTRAV;    break;
	case EvcLocMethExternal    :  par->loc_meth = CUC_LOCMETH_EXTERNAL;   break;
	default:
		par->loc_meth = CUC_LOCMETH_UNDEFINED;
	} /*endswitch*/

	par->stations_used = cl3_number_of_stations();

} /* end of cl3_regio_localization */



/*--------------------------------------------------------------------------*/



#ifdef XXX

#define HYPOE_PREFIX "hypoe"
#define HYPOE_EVTFILE "_event.evt"
#define HYPOE_HEADER "hypoe.header"
#define HYPOE_EXEC "shm_exec_hypoellipse"
#define HYPOE_TRANSL_EXT ".evt"


void cl3_regio_localization( Widget w[], char phasename[],
	CUT_PARAMSETS *parset, TSyStatus *status )

/* interface to regional localization using HYPOELLIPSE
 *
 * parameters of routine
 * Widget     w[];              input; all widgets
 * char       phasename[];      input; phasename to be used (currently ignored)
 * CUT_PARAMSETS *parset;       output; localization parameters
 * TSyStatus  *status;          output; return status
 */
{
	/* local variables */
	char     cmd[BC_LONGSTRLTH+1];   /* system command */
	char     root[BC_FILELTH+1];     /* root path for output */
	char     evtfile[BC_FILELTH+1];  /* input event file */
	char     header[BC_FILELTH+1];   /* hypoellipse header file */
	char     result[BC_FILELTH+1];   /* translated hypoellipse output file */
	FILE     *fp;                    /* pointer to result file */
	EvEventT event;                  /* event info */
	TSyBoolean eof;                  /* end of file found */
	char     *depth_val;             /* pointer to depth value */
	TSyBoolean use_s;                /* use S waves */
	CUT_PARAMS *par;                 /* pointer to current parameters */
	TSyBoolean german_event;         /* this is a german event */

	/* executable code */

	par = parset->par + parset->parno;

	strcpy( root, shd_scratch );
	strcat( root, HYPOE_PREFIX );
	strcpy( evtfile, root );
	strcat( evtfile, HYPOE_EVTFILE );
	strcpy( header, shd_inputs );
	strcat( header, HYPOE_HEADER );
	strcpy( result, root );
	strcat( result, HYPOE_TRANSL_EXT );

	/* get value of depth and use S flag */
	depth_val = cu_get_string( w[k_widget_hypo_depth_text] );
	use_s = XmToggleButtonGetState( w[k_widget_hypo_use_s_yes] );

	cl3_dump_parameters( parset, evtfile, FALSE, FALSE, FALSE, status );
	if  (Severe(status))  return;
	cl3h_hypo_set_phase_flags( status );
	if  (Severe(status))  return;

	/* number of stations used (par->stations_used) should be determined here */

	/* build system command */
	sprintf( cmd, "%s/%s %s %s %s %s %s %d", GpGetString(cGpS_defpath_extprog),
		HYPOE_EXEC, header, evtfile, GpGetString(cGpS_defpath_extprog), root,
		depth_val, use_s );

	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: regio_loc: executed command is: %s\n", cmd );
	system( cmd );

	/* read in translated output file of hypoellipse */
	fp = sy_fopen( result, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "*SHM: regio_loc: couldn't find file %s\n", result );
		*status = CUE_OPENREAD;
		return;
	} /*endif*/
	EvGetEvent( fp, &event, &eof, status );
	/* cl3h_hypo_station_info( fp, status ); */
	/* ^-- this is not done any more, because this information can't be */
	/*     deleted.  The distance will be computed from the epicenter location */
	/*     at print time. */
	sy_fclose( fp );

	if  (eof || Severe(status))  {
		fprintf( stderr, "regio_loc: input error on file %s\n", result );
		if  (eof)  *status = CUE_READ_INPUT;
		return;
	} /*endif*/

	/* reset parameters and put new values */
	cu_reset_parameters( par );
	par->depth = event.depth;
	par->err.dep = 0.0;
	par->depth_type = (Cap(*depth_val) == 'F') ? CUC_DEPTH_FREE
		: CUC_DEPTH_ESTIMATED;
	strcpy( par->origin, event.origin_time );
	par->source_lat = event.latitude;
	par->source_lon = event.longitude;
	german_event = (event.latitude >= 47.0 && event.latitude <= 55.0
		&& event.longitude >= 5.0 && event.longitude <= 15.0);
	if  (german_event)  {
		xxx_get_geo( &(par->source_lat), &(par->source_lon),
			&(par->reg_id), par->regname );
		if  (par->reg_id < 0)  {
			*status = CUE_NOGETGEO;
			return;
		} /*endif*/
		par->table_number = CUC_REGTABLE_GERGEO;
	} else {
		mb_ferindex( par->source_lat, par->source_lon, &(par->reg_id), status );
		if  (Severe(status))  return;
		mb_fername( par->reg_id, BC_LINELTH, par->regname, status );
		par->loc_meth = CUC_LOCMETH_HYPO;
		if  (Severe(status))  return;
		par->table_number = CUC_REGTABLE_FLINNENG;
	} /*endif*/

	/* the textedit command is now called in the shell script HYPOE_EXEC */
#	ifdef YYY
	/* open text editor on hypoellipse output file */
	sprintf( cmd, "%s %s", GpGetString(cGpS_texteditor),
		"$SH_SCRATCH/hypoe.out" );
	strcat( cmd, " &" );
	system( cmd );
#	endif

} /* end of cl3_regio_localization */



#undef HYPOE_PREFIX
#undef HYPOE_EVTFILE
#undef HYPOE_HEADER
#undef HYPOE_EXEC
#undef HYPOE_TRANSL_EXT

#endif


/*--------------------------------------------------------------------------*/



static void cl3h_hypo_station_info( FILE *fp, TSyStatus *status )

/* reads station info from translated Hypoellipse file and puts it to
 * info entries of correspondend traces
 *
 * parameters of routine
 * FILE       *fp;        input; input file
 * TSyStatus  *status;    output; return status
 */
{
	/* local variables */
	char     line[BC_LINELTH+1];         /* current line */
	char     station[BC_SHORTSTRLTH+1];  /* station name */
	char     cstation[BC_SHORTSTRLTH+1]; /* current station name */
	float    resid;                      /* residuum in sec */
	float    distance;                   /* distance in km */
	float    azimuth;                    /* azimuth in deg */
	TPmTrace *trc;                       /* trace info pointer */
	TSyStatus locstat;                   /* local status */
	unsigned dist_entry;                 /* distance entry */

	/* executable code */

	db_ident( "DISTANCE", &dist_entry, status );
	if  (SySevere(status))  return;

	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (strncmp(line,"!SI:",4) != 0)  continue;
		sscanf( line+4, "%s %f %f %f", station, &resid, &distance, &azimuth );
		ut_cap( station );
		trc = NULL;
		FOREVER  {
			/* get next trace pointer */
			locstat = cBcNoError;
			trc = db_getp( trc, EP_NEXT, &locstat );
			if  (trc == NULL)  break;
			/* retrieve station info */
			locstat = cBcNoError;
			db_gets( trc, ES_STATION, BC_LINELTH, cstation, &locstat );
			if  (Severe(&locstat))  continue;
			if  (strcmp(station,cstation) != 0)  continue;
			db_setr( trc, dist_entry, distance, status );
			if  (Severe(status))  return;
		} /*endfor*/
	} /*endwhile*/

} /* end of cl3h_hypo_station_info */



/*--------------------------------------------------------------------------*/



#define LOCSAT_PREFIX "locsat"
#define LOCSAT_EXEC "shm_exec_locsat"



void cl3_locsat_interface( Widget w[], CUT_PARAMSETS *parset,
	TSyStatus *status )

/* interface to regional localization using LocSAT
 *
 * parameters of routine
 * Widget     w[];              input; all widgets
 * CUT_PARAMSETS *parset;       output; localization parameters
 * TSyStatus  *status;          output; return status
 */
{
	/* local variables */
	char     cmd[cBcVeryLongStrLth+1];/* system command */
	char     root[cBcFileLth+1];     /* root path for output */
	char     stafile[cBcFileLth+1];  /* input event file */
	char     cntfile[cBcFileLth+1];  /* hypoellipse header file */
	char     datfile[cBcFileLth+1];  /* translated hypoellipse output file */
	char     resfile[cBcFileLth+1];  /* translated hypoellipse output file */
	FILE     *sta, *cnt, *dat, *res; /* pointers to files */
	char     str[cBcLineLth+1];      /* scratch string */
	char     *cptr;                  /* pointer to string */
	CUT_PARAMS *par;                 /* pointer to current parameters */
	TSyBoolean german_event;         /* this is a german event */
	TPiPhaseRoot *proot;             /* pointer to phase root */
	TPiPhaseList *phaselist;         /* phase list of trace */
	TPiPhase *t_phase;               /* phase pointer */
	int      p;                      /* phase counter */
	char     station[cBcShortStrLth+1]; /* station name */
	char     comp;                   /* component */
	GLT_STATINF *statinf;            /* pointer to station info */
	float    elev;                   /* station elevation in km */
	NTIME    ntime;                  /* numeric time */
	int      no_of_phases;           /* total number of phases */
	int      arrival_id;             /* arrival ID */
	char     first_arr[cBcTimeLth+1];/* time of first arrival */
	float    tdiff;                  /* time difference */
	float    fix_depth;              /* fixed depth value (if any) */
	char     use_fixed_depth;        /* use fixed depth: 'y' or 'n' */
	char     auto_start;             /* automatic start loc: 'y' or 'n' */
	int      degfree;                /* degrees of freedom */
	int      maxiter;                /* maximum number of iterations */
	char     first_stat[cBcShortStrLth+1]; /* name of first arrival station */
	float    init_lat, init_lon;     /* initial location */
	TSyBoolean brief_output;         /* use brief output */

	/* executable code */

	par = parset->par + parset->parno;

	/* generate filenames */
	strcpy( root, shd_scratch );
	strcat( root, LOCSAT_PREFIX );
	strcpy( stafile, root );
	strcpy( cntfile, root );
	strcpy( datfile, root );
	strcpy( resfile, root );
	strcat( stafile, "_s.txt" );
	strcat( cntfile, "_c.txt" );
	strcat( datfile, "_d.txt" );
	strcat( resfile, "_r.txt" );

	/* get depth value from dialog box */
	cptr = cu_get_string( w[k_widget_locsat_depth_text] );
	use_fixed_depth = (strcmp(cptr,"free") == 0) ? 'n' : 'y';
	if  (use_fixed_depth == 'y')  {
		if  (sscanf( cptr, "%f", &fix_depth ) != 1)  {
			*status = CL3E_CONVERT_NUM;
			err_setcontext( " ## depth value " );
			err_setcontext( cptr );
			return;
		} /*endif*/
	} else {
		fix_depth = 0.0;
	} /*endif*/

	/* use brief output or not ? get it from dialog box */
	brief_output = XmToggleButtonGetState(w[k_widget_locsat_output_yes]);

	/* get auto start value from dialog box */
	auto_start = XmToggleButtonGetState(w[k_widget_locsat_auto_yes]) ? 'y' : 'n';

	/* get degrees of freedom and max. number of iterations from dialog box */
	if  (sscanf(cu_get_string(w[k_widget_locsat_freedom_text]),"%d",&degfree)
		!= 1)  {
		*status = CL3E_CONVERT_NUM;
		err_setcontext( " ## deg. of freedom " );
		err_setcontext( cu_get_string(w[k_widget_locsat_freedom_text]) );
		return;
	} /*endif*/
	if  (sscanf(cu_get_string(w[k_widget_locsat_iterations_text]),"%d",&maxiter)
		!= 1)  {
		*status = CL3E_CONVERT_NUM;
		err_setcontext( " ## max. iterations " );
		err_setcontext( cu_get_string(w[k_widget_locsat_iterations_text]) );
		return;
	} /*endif*/

	/* create control file */
	cnt = sy_fopen( cntfile, "w" );
	if  (cnt == NULL)  {
		err_setcontext( " ## file " );  err_setcontext( cntfile );
		*status = CL3E_OPEN_WRITE;
		return;
	} /*endif*/
	/* line 1: path to travel-time-table and prefix of names */
	cptr = getenv( "SH_LOCSAT" );
	if  (cptr == NULL)  {
		err_setcontext( " ## env. variable SH_LOCSAT no set" );
		*status = CL3E_ENV_ERROR;
		sy_fclose( cnt );
		return;
	} /*endif*/
	strcpy( str, cptr );
	strcat( str, "/tables" );
	if  (strlen(str) > 30)  {
		*status = CL3E_STROVFL;
		err_setcontext( " ## path to LocSAT tables too long" );
		sy_fclose( cnt );
		return;
	} /*endif*/
	fprintf( cnt, "%s ", str );
	fprintf( cnt, "%s\n", cu_get_string(w[k_widget_locsat_prefix_text]) );
	/* line 2: path to station corrections and correction types */
	strcpy( str, cptr );
	strcat( str, "/scorr" );
	if  (strlen(str) > 30)  {
		*status = CL3E_STROVFL;
		err_setcontext( " ## path to LocSAT station corrections too long" );
		sy_fclose( cnt );
		return;
	} /*endif*/
	fprintf( cnt, "%s ", str );
	strcpy( str, "TT" );   /* !!! should be variable */
	fprintf( cnt, "%s\n", str );
	/* line 3: */
	fprintf( cnt, " %c", auto_start );        /* use_location */
	fprintf( cnt, " %c", use_fixed_depth );   /* fix_depth */
	fprintf( cnt, " %c", 'y' );               /* verbose */
	fprintf( cnt, " %5.2f", 0.9 );            /* conf_level */
	fprintf( cnt, " %6.2f", -1.0 );           /* damp */
	fprintf( cnt, " %5.2f", 1.0 );            /* est_std_err */
	fprintf( cnt, " %5d", degfree );          /* num_dof */
	fprintf( cnt, " %3d\n", maxiter );        /* max_iterations */
	sy_fclose( cnt );

	/* loop all traces and phases to find number of phases and first arrival */
	proot = NULL;
	no_of_phases = 0;
	*first_arr = '\0';
	*first_stat = '\0';
	FOREVER  {

		/* get phase list */
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		phaselist = PiPhaseListOfRoot( proot );
		if  (phaselist == NULL)  continue;
		/* loop all phases of trace */
		for  (p=0; p<PiPhaseListLength(phaselist); p++)  {
			t_phase = PiGetPhase( phaselist, p, status );
			if  (Severe(status))  return;
			if  (t_phase == NULL)  break;
			if  (t_phase->source == cPiSourceTheo)  continue;
			if  (t_phase->name[0] == 'b')  continue;
			if  (*first_arr == '\0')  {
				strcpy( first_stat, PiStationOfRoot(proot) );
				strcpy( first_arr, t_phase->onset );
			} else {
				if  (tc_tdiff(first_arr,t_phase->onset,status) > 0.0)  {
					strcpy( first_stat, PiStationOfRoot(proot) );
					strcpy( first_arr, t_phase->onset );
				} /*endif*/
				if  (Severe(status))  return;
			} /*endif*/
			if  (t_phase->quality > 1)
				no_of_phases++;
		} /*endfor*/
	} /*endfor*/

	/* open stations file and data file */
	sta = sy_fopen( stafile, "w" );
	if  (sta == NULL)  {
		err_setcontext( " ## file " );  err_setcontext( stafile );
		*status = CL3E_OPEN_WRITE;
		return;
	} /*endif*/
	dat = sy_fopen( datfile, "w" );
	if  (dat == NULL)  {
		err_setcontext( " ## file " );  err_setcontext( datfile );
		*status = CL3E_OPEN_WRITE;
		return;
	} /*endif*/

	/* first line of data file */
#ifdef XXX
	if  (par->origin[0] == '\0')  {
#endif
		tc_t2n( first_arr, &ntime, status );
#ifdef XXX
	} else {
		tc_t2n( par->origin, &ntime, status );
	} /*endif*/
#endif
	if  (Severe(status))  {
		sy_fclose( sta );
		sy_fclose( dat );
		err_setcontext( " ## couldn't convert origin time " );
		err_setcontext( first_arr );
		return;
	} /*endif*/
	if  (ntime.year > 1900)  ntime.year -= 1900;
	if  (ntime.year >= 100)  ntime.year -= 100;
	/* get inital location */
	if  (par->source_lat == 0.0 && par->source_lon == 0.0)  {
		init_lat = init_lon = 0.0;
		statinf = gl_store_station( first_stat, TRUE, status );
		if  (Severe(status))  {
			sy_fclose( sta );
			sy_fclose( dat );
			err_setcontext( " ## closest station not found" );
			return;
		} /*endif*/
		if  (gl_valid_number(statinf->lat))  init_lat = statinf->lat;
		if  (gl_valid_number(statinf->lon))  init_lon = statinf->lon;
	} else {
		init_lat = par->source_lat;
		init_lon = par->source_lon;
	} /*endif*/
	fprintf( dat, "%02d%02d%02d %02d%02d%3d.%02d", ntime.year, ntime.month,
		ntime.day, ntime.hour, ntime.min, ntime.sec, ntime.ms/10 );
	fprintf( dat, " %f %f %f 0.0 %d\n", init_lat, init_lon, fix_depth,
		no_of_phases );

	/* loop again all traces and phases */
	arrival_id = 0;
	proot = NULL;
	FOREVER  {

		/* get phase list and station name */
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		phaselist = PiPhaseListOfRoot( proot );
		if  (phaselist == NULL)  continue;
		/* retrieve station info and print it to stations file */
		strcpy( station, PiStationOfRoot(proot) );
		statinf = gl_store_station( station, TRUE, status );
		if  (Severe(status))  {
			err_setcontext( " ## station " ); err_setcontext( station );
			sy_fclose( sta );  sy_fclose( dat );
			return;
		} /*endif*/
		elev = gl_valid_number(statinf->elevation) ? statinf->elevation : 0.0;
		elev /= 1000.0;
		fprintf( sta, "%6s%9.4lf%9.4lf%9.4f\n", station,
			statinf->lat, statinf->lon, elev);
		comp = PiCompOfRoot( proot );

		/* loop all phases of trace */
		for  (p=0; p<PiPhaseListLength(phaselist); p++)  {
			t_phase = PiGetPhase( phaselist, p, status );
			if  (Severe(status))  {sy_fclose(sta); sy_fclose(dat); return;}
			if  (t_phase->source == cPiSourceTheo)  continue;
			if  (t_phase->quality == 1 || t_phase->name[0] == 'b')  {
				/* don't use this phase for location */
				t_phase->flags &= ~fPiAttribLoc;
			} else {
				tdiff = tc_tdiff( t_phase->onset, first_arr, status );
				if  (Severe(status))  {sy_fclose(sta); sy_fclose(dat); return;}
				fprintf( dat, "%8d %6s %8s %4s%2s %f %f\n", ++arrival_id, station,
					t_phase->name, "t", "d", tdiff, 1.0 );
				t_phase->flags |= fPiAttribLoc;
			} /*endif*/
		} /*endfor*/

	} /*endfor*/

	sy_fclose( sta );
	sy_fclose( dat );

	/* execute LocSAT */
	sprintf( cmd, "%s%s %s %s %s %s %s", GpGetString(cGpS_defpath_extprog),
		LOCSAT_EXEC, stafile, datfile, cntfile, resfile, first_arr );
	if  (brief_output)
		strcat( cmd, " brief" );
	system( cmd );

	cl3h_read_locsat_file( resfile, first_arr, (use_fixed_depth=='y'),
		par, status );
	if  (Severe(status))  return;

	par->stations_used = cl3_number_of_stations();

} /* end of cl3_locsat_interface */



#undef LOCSAT_PREFIX
#undef LOCSAT_EXEC



/*---------------------------------------------------------------------------*/



static void cl3h_read_locsat_file( char inpfile[], char reftime[],
	TSyBoolean fixed_depth, CUT_PARAMS *par, TSyStatus *status )

/* reads locsat result file and put values to 'par'-structure.  Also determines
 * region name
 *
 * parameters of routine
 * char       inpfile[];      input; name of locsat output file to read
 * char       reftime[];      input; reference time
 * TSyBoolean fixed_depth;    input; fixed depth used
 * CUT_PARAMS *par;           output; location results
 * TSyStatus  *status;        output; return status
 */
{
	/* local variables */
	FILE       *fp;                 /* pointer to input file */
	char       line[BC_LINELTH+1];  /* current line of file */
	char       statname[cBcLineLth+1]; /* station name */
	char       phasename[cBcLineLth+1]; /* phase name */
	float      residual;            /* residual time of phase */
	int        residcnt;            /* residual counter */
	TSyBoolean resvalid;            /* result is valid */
	float      rel_orig;            /* relative origin time */
	TSyBoolean german_event;        /* german event found */
	int        i;                   /* counter */
	TPiPhase   *phaseptr;           /* pointer to phase */
	char       complist[6]="znert"; /* possible components */

	/* executable code */

	/* open file */
	fp = sy_fopen( inpfile, "r" );
	if  (fp == NULL)  {
		*status = CL3E_OPEN_READ;
		err_setcontext( " ## file " );
		err_setcontext( inpfile );
		return;
	} /*endif*/

	/* reset parameters */
	cu_reset_parameters( par );

	resvalid = FALSE;
	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (!resvalid)  resvalid = (strstr(line,"Converged!") != NULL);
		if  (!resvalid)  continue;
		if  (strncmp(line," Final location estimate",24) == 0)  {
			fgets( line, BC_LINELTH, fp );
			sscanf( line+16, "%f", &(par->source_lat) );
			if  (line[30] == 'S')  par->source_lat *= -1.0;
			sscanf( line+36, "%f", &(par->err.lat) );
			par->err.dist_unit = (line[46] == 'k')
				? CUC_DIST_UNIT_KM : CUC_DIST_UNIT_DEG;
			fgets( line, BC_LINELTH, fp );
			sscanf( line+16, "%f", &(par->source_lon) );
			if  (line[30] == 'W')  par->source_lon *= -1.0;
			sscanf( line+36, "%f", &(par->err.lon) );
			fgets( line, BC_LINELTH, fp );
			sscanf( line+16, "%f", &(par->depth) );
			sscanf( line+36, "%f", &(par->err.dep) );
			fgets( line, BC_LINELTH, fp );
			sscanf( line+16, "%f", &rel_orig );
			sscanf( line+36, "%f", &(par->err.orig) );
			/* read error ellipsis */
			fgets( line, BC_LINELTH, fp );
			fgets( line, BC_LINELTH, fp );
			fgets( line, BC_LINELTH, fp );
			fgets( line, BC_LINELTH, fp );
			sscanf( line+20, "%f", &(par->err.smajor) );
			fgets( line, BC_LINELTH, fp );
			sscanf( line+20, "%f", &(par->err.sminor) );
			fgets( line, BC_LINELTH, fp );
			sscanf( line+20, "%f", &(par->err.majstrike) );
			/* skip 9 lines until azimuthal gap value */
			for  (i=0; i<10; i++)
				fgets( line, BC_LINELTH, fp );
			if  (strncmp(line,"    Maximum azimuthal",21) == 0)
				sscanf( line+27, "%f", &(par->err.azim_max_gap) );
			/* skip 5 lines until phase data with residuals are listed */
			for  (i=0; i<6; i++)
				fgets( line, BC_LINELTH, fp );
			par->err.resid_rms = 0.0;
			residcnt = 0;
			while  (line[2] != '=')  {
				if  (feof(fp))  break;  /* for safety */
				sscanf( line+31, "%f", &residual );
				par->err.resid_rms += residual*residual;
				residcnt++;
				sscanf( line+9, "%s", statname );
				sscanf( line+16, "%s", phasename );
				for  (i=0; i<6; i++)  {
					phaseptr = PmFindPhaseV2( statname, complist[i], phasename );
					if  (phaseptr != NULL)  break;
				} /*endfor*/
				if  (phaseptr != NULL)  {
					phaseptr->resid = residual;
				} /*endif*/
				fgets( line, BC_LINELTH, fp );
			} /*endwhile*/
			if  (par->err.resid_rms > 0.0)
				par->err.resid_rms = sqrt( par->err.resid_rms/(float)residcnt );
			break;
		} /*endif*/
	} /*endwhile*/

	sy_fclose( fp );

	if  (GpGetInt(cGpI_debug_level) > 3)
		printf( "SHM-dbg4: found %f,%f, depth %f\n", par->source_lat,
			par->source_lon, par->depth );

	tc_tadd( reftime, rel_orig, par->origin, status );
	if  (Severe(status))  return;

	par->depth_type = (fixed_depth) ? CUC_DEPTH_ESTIMATED : CUC_DEPTH_FREE;
	german_event = (par->source_lat >= 47.0 && par->source_lat <= 55.0
		&& par->source_lon >= 5.0 && par->source_lon <= 15.0);
	if  (german_event)  {
		xxx_get_geo( &(par->source_lat), &(par->source_lon),
			&(par->reg_id), par->regname );
		if  (par->reg_id < 0)  {
			*status = CUE_NOGETGEO;
			return;
		} /*endif*/
		par->table_number = CUC_REGTABLE_GERGEO;
	} else {
		mb_ferindex( par->source_lat, par->source_lon, &(par->reg_id), status );
		if  (Severe(status))  return;
		mb_fername( par->reg_id, BC_LINELTH, par->regname, status );
		par->loc_meth = CUC_LOCMETH_LOCSAT;
		if  (Severe(status))  return;
		par->table_number = CUC_REGTABLE_FLINNENG;
	} /*endif*/

} /* end of cl3h_read_locsat_file */



/*---------------------------------------------------------------------------*/



static void cl3h_hypo_set_phase_flags( TSyStatus *status )

/* sets location flags on passed phases to hypoellipse
 *
 * parameters of routine
 * TSyStatus     *status;          output; return status
 */
{
	/* local variables */
	TSyStatus    locstat;                     /* local status */
	TPiPhaseRoot *proot;                      /* pointer to phase root */
	TPiPhaseList *plist;                      /* phase list of trace */
	TPiPhase     *phase;                      /* pointer to current phase */
	TPiPhase     *first_p;                    /* first P */
	TPiPhase     *first_s;                    /* first S */
	int          p;                           /* phase counter */
	char         station[BC_SHORTSTRLTH+1];   /* station processed */
	char         c_station[BC_SHORTSTRLTH+1]; /* current station */

	/* executable code */

	first_p = first_s = NULL;
	*station = *c_station = '\0';

	/* loop all traces and phases and look for ml entries */
	proot = NULL;
	FOREVER  {

		/* get phase list and station name */
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		plist = PiPhaseListOfRoot( proot );
		if  (plist == NULL)  continue;
		strcpy( c_station, PiStationOfRoot(proot) );
		if  (strcmp(station,c_station) != 0)  {
			/* new station, set flags and then reset parameters */
			if  (first_p != NULL)  first_p->flags |= fPiAttribLoc;
			if  (first_s != NULL)  first_s->flags |= fPiAttribLoc;
			strcpy( station, c_station );
			first_p = first_s = NULL;
		} /*endif*/
		/* loop all phases of trace */
		for  (p=0; p<PiPhaseListLength(plist); p++)  {
			phase = PiGetPhase( plist, p, status );
			if  (Severe(status))  return;
			/* clear location flag */
			phase->flags &= ~fPiAttribLoc;
			if  (phase->name[0] == 'P')  {
				if  (first_p == NULL)  {
					first_p = phase; 
				} else {
					if  (tc_tdiff(phase->onset,first_p->onset,status) < 0.0)
						first_p = phase;
					if  (Severe(status))  return;
				} /*endif*/
			} else if  (phase->name[0] == 'S')  {
				if  (first_s == NULL)  {
					first_s = phase; 
				} else {
					if  (tc_tdiff(phase->onset,first_s->onset,status) < 0.0)
						first_s = phase;
					if  (Severe(status))  return;
				} /*endif*/
			} /*endif*/
		} /*endfor*/
	} /*endfor*/

	if  (first_p != NULL)  first_p->flags |= fPiAttribLoc;
	if  (first_s != NULL)  first_s->flags |= fPiAttribLoc;

} /* end of cl3h_hypo_set_phase_flags */



/*---------------------------------------------------------------------------*/



void cl3_restore_from_evt( char evtfile[], CUT_PARAMS *par, long *eventid,
	TSyStatus *status )

/* restores event & phase parameters from evt-file
 *
 * parameters of routine
 * char       evtfile[];      input; name of evt-file
 * CUT_PARAMS *par;           output; params read in
 * long       *eventid;       output; event ID if found in evt file (ptr may be NULL)
 * TSyStatus  *status;        output; return status
 */
{
	/* local variables */
	FILE       *evt;             /* pointer to input file */
	EvEventT   event;            /* event (phase) info */
	TSyBoolean eof;              /* end of file found */
	TPiPhase   phase;            /* phase info */
	char       *cptr;            /* moving pointer */
	TSyBoolean german_event;     /* German event ? */

	/* executable code */

	cu_reset_parameters( par );
	if  (eventid != NULL)  *eventid = 0;

	/* try to open input evt-file */
	evt = sy_fopen( evtfile, "r" );
	if  (evt == NULL)  {
		*status = CL3E_OPEN_READ;
		err_setcontext( " ## file " ); err_setcontext( evtfile );
		return;
	} /*endif*/

	/* read all phases from file */
	FOREVER  {

		/* get next phase info */
		EvGetEvent( evt, &event, &eof, status );
		if  (eof)  break;
		if  (SySevere(status))  {
			*status = CL3E_READ_ERR;
			err_setcontext( " ## file " ); err_setcontext( evtfile );
			return;
		} /*endif*/

		/* first process event information */
		if  (event.distance_deg != EvEMPTY_DISTANCE)  {
			par->distance = event.distance_deg;
			par->dist_unit = CUC_DIST_UNIT_DEG;
		} /*endif*/
		if  (event.distance_km != EvEMPTY_DISTANCE)  {
			par->distance = event.distance_km;
			par->dist_unit = CUC_DIST_UNIT_KM;
		} /*endif*/
		if  (event.b_slowness != EvEMPTY_SLOWNESS)
			par->b_slowness = event.b_slowness;
		if  (event.b_azimuth != EvEMPTY_AZIMUTH)
			par->b_azimuth = event.b_azimuth;
		if  (event.l_slowness != EvEMPTY_SLOWNESS)
			par->l_slowness = event.l_slowness;
		if  (event.l_azimuth != EvEMPTY_AZIMUTH)
			par->l_azimuth = event.l_azimuth;
		if  (event.depth != EvEMPTY_DEPTH)
			par->depth = event.depth;
		switch  (event.depth_type)  {
		case EvcDepthPreset:    par->depth_type = CUC_DEPTH_PRESET; break;
		case EvcDepthEstimated: par->depth_type = CUC_DEPTH_ESTIMATED; break;
		case EvcDepthFree:      par->depth_type = CUC_DEPTH_FREE; break;
		case EvcDepthPoor:      par->depth_type = CUC_DEPTH_POOR; break;
		case EvcDepthLessWell:  par->depth_type = CUC_DEPTH_LESSWELL; break;
		case EvcDepthReliable:  par->depth_type = CUC_DEPTH_RELIABLE; break;
		case EvcDepthExternal:  par->depth_type = CUC_DEPTH_EXTERNAL; break;
		} /*endswitch*/
		if  (event.origin_time[0] != EvEOS)
			strcpy( par->origin, event.origin_time );
		if  (event.latitude != EvEMPTY_LATITUDE)
			par->source_lat = event.latitude;
		if  (event.longitude != EvEMPTY_LONGITUDE)
			par->source_lon = event.longitude;
#ifdef XXX
		/* removed 21-Mar-2003 */
		if  (event.region_id > 0)  {
			par->reg_id = event.region_id;
			if  (event.region_table == EvcRegionTableGermanGeo)  {
				par->table_number = CUC_REGTABLE_GERGEO;
				xxx_get_geo( &(par->source_lat), &(par->source_lon),
					&(par->reg_id), par->regname );
			} else {
				par->table_number = CUC_REGTABLE_FLINNENG;
				mb_fername( par->reg_id, BC_LINELTH, par->regname, status );
			} /*endif*/
		} /*endif*/
#endif
		/* compute name of location in case it is not specified */
		german_event = (par->source_lat >= 47.0 && par->source_lat <= 55.0
			&& par->source_lon >= 5.0 && par->source_lon <= 15.0);
		if  (german_event)  {
			xxx_get_geo( &(par->source_lat), &(par->source_lon),
				&(par->reg_id), par->regname );
			if  (par->reg_id < 0)  {
				*status = CUE_NOGETGEO;
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
		if  (event.event_type == EvcEventTypeTeleQuake)
			par->source_type = CUC_TYPE_TELE_QUAKE;
		if  (event.event_type == EvcEventTypeNuclear)
			par->source_type = CUC_TYPE_NUCLEAR;
		if  (event.event_type == EvcEventTypeRegioQuake)
			par->source_type = CUC_TYPE_REGIO_QUAKE;
		if  (event.event_type == EvcEventTypeLocalQuake)
			par->source_type = CUC_TYPE_LOCAL_QUAKE;
		if  (event.event_type == EvcEventTypeBlast)
			par->source_type = CUC_TYPE_BLAST;
		if  (event.event_type == EvcEventTypeMining)
			par->source_type = CUC_TYPE_MINING;
		if  (event.loc_method != EvcLocMethUndefined)  {
			switch  (event.loc_method)  {
			case EvcLocMethCorrBeam:  par->loc_meth = CUC_LOCMETH_BEAMCORR; break;
			case EvcLocMethUncorrBeam: par->loc_meth =CUC_LOCMETH_BEAMUNCORR;break;
			case EvcLocMethResidCorr: par->loc_meth = CUC_LOCMETH_RESIDCORR;break;
			case EvcLocMethHypo:  par->loc_meth =     CUC_LOCMETH_HYPO; break;
			case EvcLocMethLocsat:  par->loc_meth =   CUC_LOCMETH_LOCSAT; break;
			case EvcLocMethHypocenter: par->loc_meth =CUC_LOCMETH_HYPOCENTER;break;
			case EvcLocMethHypo71: par->loc_meth =    CUC_LOCMETH_HYPO71;break;
			case EvcLocMethHyposat: par->loc_meth =   CUC_LOCMETH_HYPOSAT;break;
			case EvcLocMethRelTrav: par->loc_meth =   CUC_LOCMETH_RELTRAV;break;
			case EvcLocMethExternal:  par->loc_meth = CUC_LOCMETH_UNDEFINED; break;
			} /*endswitch*/
		} /*endif*/
		if  (event.loc_quality != EvcLocQualUndefined)  {
			switch  (event.loc_quality)  {
			case EvcLocQualTooWeak: par->loc_quality =   CUC_LOCQ_TOOWEAK; break;
			case EvcLocQualIncoherent: par->loc_quality =CUC_LOCQ_INCOHERENT;break;
			case EvcLocQualNoBearing: par->loc_quality = CUC_LOCQ_NOBEARING; break;
			case EvcLocQualRegion: par->loc_quality =    CUC_LOCQ_REGION; break;
			case EvcLocQualReliable: par->loc_quality =  CUC_LOCQ_RELIABLE; break;
			} /*endswitch*/
		} /*endif*/
		if  (event.mu_descr[0] != EvEOS)  strcpy( par->mu_descr, event.mu_descr );
		if  (event.velmod[0]   != EvEOS)  strcpy( par->velmod,   event.velmod );
		if  (event.loc_addpar[0] != EvEOS)  strcpy( par->loc_addpar, event.loc_addpar );
		if  (event.momten[0] != EvEOS)  strcpy( par->momten, event.momten );
		if  (event.momten_descr[0] != EvEOS)  strcpy( par->momten_descr, event.momten_descr );
		if  (event.fps_angles[0] != EvEOS)  strcpy( par->fps_angles, event.fps_angles );
		if  (event.fps_descr[0] != EvEOS)  strcpy( par->fps_descr, event.fps_descr );
		if  (event.cornerfreq != EvEMPTY_FREQ)  par->cornerfreq = event.cornerfreq;
		if  (event.lowfreqlevel != EvEMPTY_FREQ)  par->lowfreqlevel = event.lowfreqlevel;
		if  (event.m0 != EvEMPTY_MAGNITUDE)  par->m0 = event.m0;
		if  (event.phase[0] != EvEOS)  strcpy( par->phase, event.phase );
		if  (event.comment.length > 0)  {
			par->comment = (char *)sy_allocmem( event.comment.length,
				(int)sizeof(char), status );
			if  (Severe(status))  return;
			strcpy( par->comment, event.comment.text );
		} /*endif*/
		if  (event.source[0] != EvEOS)  strcpy( par->source, event.source );

		/* now read phase parameters */
		cu_reset_phase( &phase );
		if  (event.phase[0] != EvEOS)
			strcpy( phase.name, event.phase );
		if  (event.onset_time[0] != EvEOS)
			strcpy( phase.onset, event.onset_time );
		if  (event.onset_wdw_l != EvEMPTY_ONSET_WDW)
			phase.onset_acc_l = event.onset_wdw_l;
		if  (event.onset_wdw_r != EvEMPTY_ONSET_WDW)
			phase.onset_acc_r = event.onset_wdw_r;
		if  (event.onset_type == EvcOnsetEmergent)
			phase.spec = cPiSpecEmergent;
		if  (event.onset_type == EvcOnsetImpulsive)
			phase.spec = cPiSpecImpulsive;
		if  (event.sign == EvcSignNegative)
			phase.sign = -1;
		if  (event.sign == EvcSignPositive)
			phase.sign = 1;
		phase.reliable = (event.phase[0] != '(');
		if  (!phase.reliable)  {
			/* take away parantheses */
			cptr = phase.name+1;
			while  (*cptr != '\0' && *cptr != ')')  {
				*(cptr-1) = *cptr;
				cptr++;
			} /*endwhile*/
			*(cptr-1) = '\0';
		} /*endif*/
		phase.source = cPiSourceManually;
		if  (event.weight != EvEMPTY_WEIGHT)
			phase.weight = event.weight;
		if  (event.quality != EvEMPTY_QUALITY)
			phase.quality = event.quality;
		if  (event.amplitude_time != EvEMPTY_AMPLITUDE_TIME)
			phase.ampl_time = event.amplitude_time;
		if  (event.amplitude_vel != EvEMPTY_AMPLITUDE)
			phase.ampl_veloc = event.amplitude_vel;
		if  (event.amplitude != EvEMPTY_AMPLITUDE)
			phase.ampl_displ = event.amplitude;
		if  (event.period != EvEMPTY_PERIOD)
			phase.period = event.period;
		if  (event.mag[EvcMagMl] != EvEMPTY_MAGNITUDE)  {
			phase.magnitude = event.mag[EvcMagMl];
			phase.magn_source = cPiMagnMl;
		} /*endif*/
		if  (event.mag[EvcMagMs] != EvEMPTY_MAGNITUDE)  {
			phase.magnitude = event.mag[EvcMagMs];
			phase.magn_source = cPiMagnMsPlain;
		} /*endif*/
		if  (event.mag[EvcMagMb] != EvEMPTY_MAGNITUDE)  {
			phase.magnitude = event.mag[EvcMagMb];
			phase.magn_source = cPiMagnMb;
		} /*endif*/
		if  (event.mag[EvcMagMw] != EvEMPTY_MAGNITUDE)  {
			phase.magnitude = event.mag[EvcMagMw];
			phase.magn_source = cPiMagnMw;
		} /*endif*/
		if  (event.mag[EvcMagMbb] != EvEMPTY_MAGNITUDE)  {
			phase.bb.mbb = event.mag[EvcMagMbb];
		} /*endif*/
		if  (event.mag[EvcMagMu] != EvEMPTY_MAGNITUDE)  {
			phase.magnitude = event.mag[EvcMagMu];
			phase.magn_source = cPiMagnMu;
		} /*endif*/
		if  (event.ap_source == EvcApSourceDirect)
			phase.ap_source = cPiApsrcDirect;
		if  (event.ap_source == EvcApSourceBeam)
			phase.ap_source = cPiApsrcBeam;
		if  (event.ap_source == EvcApSourceAlign)
			phase.ap_source = cPiApsrcAlign;
		if  (event.signoise != EvEMPTY_SIGNOISE)
			phase.signoise = event.signoise;
		if  (strchr(event.phase_flags,EvFLAG_MAGNITUDE))
			phase.flags |= fPiAttribMagn;
		if  (strchr(event.phase_flags,EvFLAG_LOCATION))
			phase.flags |= fPiAttribLoc;
		if  (strchr(event.phase_flags,EvFLAG_BEAM))
			phase.flags |= fPiAttribBeam;
		if  (event.filter[0] != EvEOS)
			strcpy( phase.filter, event.filter );
		switch  (event.pick_type)  {
		case EvcPickTypeManual: phase.source = cPiSourceManually; break;
		case EvcPickTypeAuto:   phase.source = cPiSourceAuto;     break;
		case EvcPickTypeTheo:   phase.source = cPiSourceTheo;     break;
		default:                phase.source = cPiSourceManually; break;
		} /*endswitch*/

		/* restore event ID if configured in parameter file */
		if  (eventid != NULL)
			if  (event.evid > 0)  *eventid = event.evid;

		/* insert phase */
		if  (event.station[0] > ' ')  {
			PmAddPhaseV2( event.station, event.component, &phase, status );
			if  (Severe(status))  return;
		} /*endif*/

	} /*endfor*/

	/* close evt file */
	sy_fclose( evt );

	/* setup correct pointers in phases read from file */
	/*cl3_phasetrc_reset();*/

} /* end of cl3_restore_from_evt */



/*--------------------------------------------------------------------------*/



static void cl3_find_output_phase( void )

/* Finds phase where all location info is attached to.
 *
 * parameters of routine
 */
{
	/* local variables */
	int      q;                      /* phase counter */
	TPiPhaseRoot *proot;             /* pointer to phase root */
	TPiPhaseList *phaselist;         /* phase list */
	TPiPhase *t_phase;               /* current phase */
	int      *last_flags;            /* pointer to last flags */
	int      last_ranking;           /* last ranking */
	int      ranking;                /* ranking of this phase */
	TSyBoolean is_ref_station;       /* current station is reference station */

	/* executable code */

	last_flags = NULL;

	proot = NULL;
	FOREVER  {
		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;
		phaselist = PiPhaseListOfRoot( proot );
		if  (phaselist == NULL)  continue;
		if  (strcmp(PiStationOfRoot(proot),GpGetString(cGpS_refstation)) == 0)
			is_ref_station = TRUE;
		for  (q=0; q<PiPhaseListLength(phaselist); q++)  {
			t_phase = PiGetPhase( phaselist, q, NULL );
			if  (t_phase->source != cPiSourceTheo && t_phase->name[0] != 'b')  {
				if  (last_flags == NULL)  {
					/* mark first phase found, to have any */
					last_ranking = cl3_phase_ranking( t_phase->name );
					t_phase->flags |= fPiAttribOutput;
					last_flags = &(t_phase->flags);
					if  (GpGetInt(cGpI_debug_level) > 2)
						printf( "SHM-dbg3: output phase, first is %s at %s\n",
							t_phase->name, PiStationOfRoot(proot) );
				} else {
					ranking = cl3_phase_ranking( t_phase->name );
					if  (ranking < last_ranking || (ranking == last_ranking
						&& is_ref_station))  {
						/* better phase found, unmark previous and use this */
						*last_flags &= ~fPiAttribOutput;
						last_ranking = ranking;
						t_phase->flags |= fPiAttribOutput;
						last_flags = &(t_phase->flags);
						if  (GpGetInt(cGpI_debug_level) > 2)
							printf( "SHM-dbg3: output phase, better is %s at %s\n",
								t_phase->name, PiStationOfRoot(proot) );
					} /*endif*/
				} /*endif*/
			} /*endif*/
		} /*endfor*/
	} /*endfor*/


} /* end of cl3_find_output_phase */



/*---------------------------------------------------------------------------*/



static int cl3_phase_ranking( char phase[] )

/* Returns ranking of phase
 *
 * parameters of routine
 * char       phase[];      input; name of phase
 *                          returns phase ranking
 */
{
	/* local variables */
	static char *ranktable[] =
		{ "P", "PKPdf", "PKPab", "PKPbc", "Pn", "Pg", "PP", "-" };
	char     *ptr;           /* moving pointer */
	int      rank;           /* ranking */

	/* executable code */

	rank = 0;
	ptr = *ranktable;
	FOREVER  {
		if  (strcmp(ptr,phase) == 0)  return rank;
		ptr = ranktable[++rank];
		if  (strcmp(ptr,"-") == 0)  return rank+1;
	} /*endfor*/

} /* end of cl3_phase_ranking */



/*---------------------------------------------------------------------------*/



void cl3_init_extproc_entries( Widget w[] )

/* Initializes menu entry for external processes
 *
 * parameters of routine
 * Widget     w[];      input; widget array
 */
{
	/* local variables */
	char     *env;                    /* pointer to environment variable */
	char     fname[cBcFileLth+1];     /* file name with ext proc definitions */
	FILE     *fp;                     /* pointer to file */
	char     line[cBcLineLth+1];      /* current line of file */
	char     text[cBcLineLth+1];      /* parsed text */
	int      procnum;                 /* entry number */
	Arg      args[5];             /* argument list */
	int      n;                   /* argument counter */
	int      widx;                /* widget index */
	XmString xmstr;               /* Xm string */

	/* executable code */

	/* initialize extproc */
	for  (n=0; n<CL3C_PLUGIN_MAXPROC; n++)  {
		cl3v_extproc[n].pluginname[0] = '\0';
		cl3v_extproc[n].progname[0] = '\0';
		cl3v_extproc[n].shname[0] = '\0';
		cl3v_extproc[n].evtfile[0] = '\0';
		cl3v_extproc[n].evtinput[0] = '\0';
		cl3v_extproc[n].waveform[0] = '\0';
		cl3v_extproc[n].currwindow[0] = '\0';
		cl3v_extproc[n].shell_bg = FALSE;
		cl3v_extproc[n].clear_phases = FALSE;
		cl3v_extproc[n].clear_theo = FALSE;
		cl3v_extproc[n].reread_plugins = FALSE;
	} /*endfor*/

	env = ( char *)getenv( "SH_USERDIR" );
	if  (env == NULL)  return;
	if  (strlen(env)+strlen(EXTPROCFILE) > cBcFileLth)  return;
	strcpy( fname, env );
	strcat( fname, EXTPROCFILE );
	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		if  (strlen(env)+strlen(ALTEXTPROCFILE) > cBcFileLth)  return;
		strcpy( fname, env );
		strcat( fname, ALTEXTPROCFILE );
		fp = fopen( fname, "r" );
	} /*endif*/
	if  (fp == NULL)  {
		env = ( char *)getenv( "SH_INPUTS" );
		if  (env == NULL)  return;
		if  (strlen(env)+strlen(EXTPROCFILE) > cBcFileLth)  return;
		strcpy( fname, env );
		strcat( fname, EXTPROCFILE );
		fp = fopen( fname, "r" );
	} /*endif*/
	if  (fp == NULL)  return;

	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: external-processes: %s\n", fname );

	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		if  (*line == '\n' || *line == '!')  continue;
		if  (strncmp(line,"label",5) == 0)  {
			if  (sscanf( line+5, "%d %s", &procnum, text ) != 2)  {
				fprintf( stderr, "*SHM: error reading %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (procnum <= 0 || procnum > CL3C_PLUGIN_MAXPROC)  {
				fprintf( stderr, "*SHM: illegal process number in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (procnum <= CL3C_PLUGIN_MAXMENUPROC || procnum == CL3C_PLUGIN_LOCPROC)  {
				if  (strlen(text) > cBcFileLth)  {
					fprintf( stderr, "*SHM: name too long in %s\n", fname );
					fprintf( stderr, "*SHM: %s", line );
					fclose( fp );
					return;
				} /*endif*/
				strcpy( cl3v_extproc[procnum-1].pluginname, text );
				widx = (procnum == CL3C_PLUGIN_LOCPROC)
					? k_widget_button_locsat_hypo : k_widget_eproc1+procnum-1;
				for  (n=0; n<strlen(text); n++)
					if  (text[n] == '_')  text[n] = ' ';
				xmstr = XmStringCreateLtoR( text, "" );
				n = 0;
				XtSetArg( args[n], XmNlabelString, xmstr ); n++;
				XtSetValues( w[widx], args, n );
				XtSetSensitive( w[widx], TRUE );
			} /*endif*/
		} else if  (strncmp(line,"shell",5) == 0)  {
			if  (sscanf( line+5, "%d %s", &procnum, text ) != 2)  {
				fprintf( stderr, "*SHM: illegal call in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (procnum <= 0 || procnum > CL3C_PLUGIN_MAXPROC)  {
				fprintf( stderr, "*SHM: illegal process number in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (strlen(text) > cBcFileLth)  {
				fprintf( stderr, "*SHM: call too long in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (*text == '/')  {
				strcpy( cl3v_extproc[procnum-1].progname, text );
			} else {
				env = (char *)getenv( "SH_USERDIR" );
				if  (strlen(text)+strlen(env) > cBcFileLth)  {
					fprintf( stderr, "*SHM: call too long in %s\n", fname );
					fprintf( stderr, "*SHM: %s", line );
					fclose( fp );
					return;
				} /*endif*/
				strcpy( cl3v_extproc[procnum-1].progname, env );
				strcat( cl3v_extproc[procnum-1].progname, text );
			} /*endif*/
		} else if  (strncmp(line,"bgshell",7) == 0)  {
			if  (sscanf( line+7, "%d", &procnum ) != 1)  {
				fprintf( stderr, "*SHM: illegal shellbg in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (procnum <= 0 || procnum > CL3C_PLUGIN_MAXPROC)  {
				fprintf( stderr, "*SHM: illegal process number in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			cl3v_extproc[procnum-1].shell_bg = TRUE;
		} else if  (strncmp(line,"resetpar",8) == 0)  {
			if  (sscanf( line+8, "%d", &procnum ) != 1)  {
				fprintf( stderr, "*SHM: illegal resetpar in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (procnum <= 0 || procnum > CL3C_PLUGIN_MAXPROC)  {
				fprintf( stderr, "*SHM: illegal process number in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			cl3v_extproc[procnum-1].clear_phases = TRUE;
		} else if  (strncmp(line,"remtheo",7) == 0)  {
			if  (sscanf( line+7, "%d", &procnum ) != 1)  {
				fprintf( stderr, "*SHM: illegal remtheo in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (procnum <= 0 || procnum > CL3C_PLUGIN_MAXPROC)  {
				fprintf( stderr, "*SHM: illegal process number in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			cl3v_extproc[procnum-1].clear_theo = TRUE;
		} else if  (strncmp(line,"writeevt",8) == 0)  {
			if  (sscanf( line+8, "%d %s", &procnum, text ) != 2)  {
				fprintf( stderr, "*SHM: illegal evt-entry in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (procnum <= 0 || procnum > CL3C_PLUGIN_MAXPROC)  {
				fprintf( stderr, "*SHM: illegal process number in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (strlen(text) > cBcFileLth)  {
				fprintf( stderr, "*SHM: evtname too long in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (*text == '/')  {
				strcpy( cl3v_extproc[procnum-1].evtfile, text );
			} else {
				env = (char *)getenv( "SH_SCRATCH" );
				if  (strlen(text)+strlen(env) > cBcFileLth)  {
					fprintf( stderr, "*SHM: evtname too long in %s\n", fname );
					fprintf( stderr, "*SHM: %s", line );
					fclose( fp );
					return;
				} /*endif*/
				strcpy( cl3v_extproc[procnum-1].evtfile, env );
				strcat( cl3v_extproc[procnum-1].evtfile, text );
			} /*endif*/
		} else if  (strncmp(line,"readevt",7) == 0)  {
			if  (sscanf( line+7, "%d %s", &procnum, text ) != 2)  {
				fprintf( stderr, "*SHM: illegal readevt-entry in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (procnum <= 0 || procnum > CL3C_PLUGIN_MAXPROC)  {
				fprintf( stderr, "*SHM: illegal process number in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (strlen(text) > cBcFileLth)  {
				fprintf( stderr, "*SHM: evtname too long in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (*text == '/')  {
				strcpy( cl3v_extproc[procnum-1].evtinput, text );
			} else {
				env = (char *)getenv( "SH_SCRATCH" );
				if  (strlen(text)+strlen(env) > cBcFileLth)  {
					fprintf( stderr, "*SHM: evtname too long in %s\n", fname );
					fprintf( stderr, "*SHM: %s", line );
					fclose( fp );
					return;
				} /*endif*/
				strcpy( cl3v_extproc[procnum-1].evtinput, env );
				strcat( cl3v_extproc[procnum-1].evtinput, text );
			} /*endif*/
		} else if  (strncmp(line,"waveform",8) == 0)  {
			if  (sscanf( line+8, "%d %s", &procnum, text ) != 2)  {
				fprintf( stderr, "*SHM: illegal waveform-entry in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (procnum <= 0 || procnum > CL3C_PLUGIN_MAXPROC)  {
				fprintf( stderr, "*SHM: illegal process number in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (strlen(text) > cBcFileLth)  {
				fprintf( stderr, "*SHM: waveform too long in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (*text == '/')  {
				strcpy( cl3v_extproc[procnum-1].waveform, text );
			} else {
				env = (char *)getenv( "SH_SCRATCH" );
				if  (strlen(text)+strlen(env) > cBcFileLth)  {
					fprintf( stderr, "*SHM: evtname too long in %s\n", fname );
					fprintf( stderr, "*SHM: %s", line );
					fclose( fp );
					return;
				} /*endif*/
				strcpy( cl3v_extproc[procnum-1].waveform, env );
				strcat( cl3v_extproc[procnum-1].waveform, text );
			} /*endif*/
		} else if  (strncmp(line,"currwindow",10) == 0)  {
			if  (sscanf( line+10, "%d %s", &procnum, text ) != 2)  {
				fprintf( stderr, "*SHM: illegal currwindow-entry in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (procnum <= 0 || procnum > CL3C_PLUGIN_MAXPROC)  {
				fprintf( stderr, "*SHM: illegal process number in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (strlen(text) > cBcFileLth)  {
				fprintf( stderr, "*SHM: currwindow name too long in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (*text == '/')  {
				strcpy( cl3v_extproc[procnum-1].currwindow, text );
			} else {
				env = (char *)getenv( "SH_SCRATCH" );
				if  (strlen(text)+strlen(env) > cBcFileLth)  {
					fprintf( stderr, "*SHM: currwindow name too long in %s\n", fname );
					fprintf( stderr, "*SHM: %s", line );
					fclose( fp );
					return;
				} /*endif*/
				strcpy( cl3v_extproc[procnum-1].currwindow, env );
				strcat( cl3v_extproc[procnum-1].currwindow, text );
			} /*endif*/
		} else if  (strncmp(line,"shcfile",7) == 0)  {
			if  (sscanf( line+7, "%d %s", &procnum, text ) != 2)  {
				fprintf( stderr, "*SHM: illegal shcfile in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (procnum <= 0 || procnum > CL3C_PLUGIN_MAXPROC)  {
				fprintf( stderr, "*SHM: illegal process number in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (strlen(text) > cBcFileLth)  {
				fprintf( stderr, "*SHM: shc name too long in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			strcpy( cl3v_extproc[procnum-1].shname, text );
		} else if  (strncmp(line,"reread",6) == 0)  {
			if  (sscanf( line+6, "%d", &procnum ) != 1)  {
				fprintf( stderr, "*SHM: illegal reread in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			if  (procnum <= 0 || procnum > CL3C_PLUGIN_MAXPROC)  {
				fprintf( stderr, "*SHM: illegal process number in %s\n", fname );
				fprintf( stderr, "*SHM: %s", line );
				fclose( fp );
				return;
			} /*endif*/
			cl3v_extproc[procnum-1].reread_plugins = TRUE;
		} else {
			fprintf( stderr, "*SHM: unknown command in %s\n", fname );
			fprintf( stderr, "*SHM: %s", line );
		} /*endif*/
	} /*endwhile*/

	fclose( fp );

} /* end of cl3_init_extproc_entries */



/*---------------------------------------------------------------------------*/



void cl3_call_extproc( int num, Widget w[], MGT_DSPCTRL *ctrl,
	CUT_PARAMSETS *parset, TPiPhase *phase, TSyStatus *status )

/* Calls external process
 *
 * parameters of routine
 * int           num;             input; process number
 * Widget        w[];             input; widget array
 * MGT_DSPCTRL   *ctrl;           input; display control parameters
 * CUT_PARAMSETS *parset;         modify; analysis parameters
 * TPiPhase      *phase;          modify; phase prototype
 * TSyStatus     *status;         output; return status
 */
{
	/* local variables */
	char     cmd[cBcLongStrLth+1];     /* command to execute */
	char     shcmd[cBcLongStrLth+1];   /* SH command to execute */
	TSyBoolean redraw;                 /* not used */
	char     *cptr;                    /* pointer to char */
	CUT_PARAMS *par;                   /* pointer to current parameters */
	int      sysret;                   /* system return */
	char     stime[cBcTimeLth+1];      /* start  time of window */
	float    wdwwidth;                 /* window width in s */
	FILE     *fp;                      /* pointer to file */
	int      widx;                     /* widget counter */
	int      n;                        /* argument counter */
	Arg      args[5];                  /* argument list */
	XmString xmstr;                    /* Xm string */
	char     text[cBcLineLth+1];       /* menu text */

	/* executable code */

	if  (num <= 0 || num > CL3C_PLUGIN_MAXPROC)  {
		fprintf( stderr, "*SHM: illegal process number, program bug\n" );
		return;
	} /*endif*/

	/* updating location parameters (for external location programs) */
	par = parset->par + parset->parno;
	strcpy( par->velmod, cu_get_string(w[k_widget_locsat_prefix_text]) );
	strcpy( par->loc_addpar, cu_get_string(w[k_widget_locsat_iterations_text]) );

	*cmd = '\0';
	if  (cl3v_extproc[num-1].progname[0] != '\0')
		strcpy( cmd, cl3v_extproc[num-1].progname );

	*shcmd = '\0';
	if  (cl3v_extproc[num-1].shname[0] != '\0')  {
		strcpy( shcmd, "@" );
		strcat( shcmd, cl3v_extproc[num-1].shname );
	} /*endif*/

	/* create file with current time window */
	if  (cl3v_extproc[num-1].currwindow[0] != '\0')  {
		mg_current_time_window( stime, &wdwwidth );
		fp = fopen( cl3v_extproc[num-1].currwindow, "w" );
		if  (fp == NULL)  {
			fprintf( stderr, "*SHM: error writing current window file %s\n",
				cl3v_extproc[num-1].currwindow );
		} else {
			fprintf( fp, "%s\n%g\n", stime, wdwwidth );
			fclose( fp );
		} /*endif*/
	} /*endif*/

	/* create evtfile if requested and append it as parameter to commands */
	if  (cl3v_extproc[num-1].evtfile[0] != '\0')  {
		cl3_dump_parameters( parset, cl3v_extproc[num-1].evtfile,
			FALSE, FALSE, FALSE, status );
		if  (SySevere(status))  {
			fprintf( stderr, "*SHM: error writing evtfile\n" );
			return;
		} /*endif*/
		/* append it to shell command, if any */
		if  (*cmd != '\0')  {
			strcat( cmd, " " );
			strcat( cmd, cl3v_extproc[num-1].evtfile );
		} /*endif*/
		/* append it to SH command if any */
		if  (*shcmd != '\0')  {
			strcat( shcmd, " " );
			strcat( shcmd, cl3v_extproc[num-1].evtfile );
			for  (cptr=shcmd; *cptr != '\0'; cptr++)
				if  (*cptr == '/')  *cptr = '\\';
		} /*endif*/
	} else {
		if  (*shcmd != '\0')  strcat( shcmd, ";;" );
	} /*endif*/

	/* create waveform files if requested and append */
	if  (cl3v_extproc[num-1].waveform[0] != '\0')  {
		cl3_perform_fk( w, TRUE, TRUE, cl3v_extproc[num-1].waveform, status );
		if  (SySevere(status))  {
			fprintf( stderr, "*SHM: error writing waveforms\n" );
			return;
		} /*endif*/
		/* append it to shell command, if any */
		if  (*cmd != '\0')  {
			strcat( cmd, " " );
			strcat( cmd, cl3v_extproc[num-1].waveform );
			strcat( cmd, ".hdr " );
			strcat( cmd, cl3v_extproc[num-1].waveform );
			strcat( cmd, ".dat" );
		} /*endif*/
		/* append it to SH command if any */
		if  (*shcmd != '\0')  {
			strcat( cmd, " " );
			strcat( cmd, cl3v_extproc[num-1].waveform );
			strcat( cmd, ".hdr " );
			strcat( cmd, cl3v_extproc[num-1].waveform );
			strcat( cmd, ".dat" );
			for  (cptr=shcmd; *cptr != '\0'; cptr++)
				if  (*cptr == '/')  *cptr = '\\';
		} /*endif*/
	} else {
		if  (*shcmd != '\0')  strcat( shcmd, ";;" );
	} /*endif*/

	if  (*shcmd != '\0')  {
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: calling SH: %s\n", shcmd );
		callsh( shcmd, &redraw, status );
		if   (SySevere(status))  return;
	} /*endif*/

	if  (*cmd != '\0')  {
		if  (cl3v_extproc[num-1].shell_bg)  strcat( cmd, " &" );
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: calling: %s\n", cmd );
		sysret = system( cmd );
		if  (sysret != 0)  {
			if  (GpGetInt(cGpI_debug_level) > 1)
				printf( "SHM.dbg2: shell command returned %d.  Abort.\n", sysret );
			return;
		} /*endif*/
	} /*endif*/

	if  (cl3v_extproc[num-1].clear_theo)  {
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: clearing theoretical phases\n" );
		cu_delete_all_phases( w[k_widget_draw], w[k_widget_single_draw],
			"--theo--", TRUE );
		callsh( "rd", &redraw, status );
		if   (SySevere(status))  return;
	} /*endif*/

	if  (cl3v_extproc[num-1].clear_phases)  {
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: clearing phases and parameters\n" );
		PiClearAllPhases();
		cu_reset_phase( phase );
		callsh( "rd", &redraw, status );
		if   (SySevere(status))  return;
	} /*endif*/

	if  (cl3v_extproc[num-1].evtinput[0] != '\0')  {
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: reading evt file %s\n",
				cl3v_extproc[num-1].evtinput );
		par = parset->par + parset->parno;
		cl3_restore_from_evt( cl3v_extproc[num-1].evtinput, par, NULL, status );
		if   (SySevere(status))  return;
		par->soft_change = TRUE;
		cu_set_param_values( w, par, parset->parno );
		par->soft_change = FALSE;
		mg_tracedisplay( w[k_widget_draw], ctrl, status );
		if   (SySevere(status))  return;
	} /*endif*/

	if  (cl3v_extproc[num-1].reread_plugins)  {
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: reread plugin definitions\n" );
		/* disable all menu entries */
		XtSetSensitive( w[k_widget_button_locsat_hypo], FALSE );
		for  (widx=k_widget_eproc1; widx<(k_widget_eproc1+CL3C_PLUGIN_MAXMENUPROC);
			widx++)  {
			sprintf( text, "Plugin %d", widx-k_widget_eproc1+1 );
			xmstr = XmStringCreateLtoR( text, "" );
			n = 0;
			XtSetArg( args[n], XmNlabelString, xmstr ); n++;
			XtSetValues( w[widx], args, n );
			XtSetSensitive( w[widx], FALSE );
		} /*endfor*/
		cl3_init_extproc_entries( w );
	} /*endif*/

} /* end of cl3_call_extproc */



/*---------------------------------------------------------------------------*/



static void cl3_write_extproc_file( Widget w[] )

/* writes plugins.txt file
 *
 * parameters of routine
 * Widget     w[]; input; widget array
 */
{
	/* local variables */
	char     *env;                 /* pointer to environment */
	char     fname[cBcFileLth+1];  /* name of output file */
	char     str[cBcLineLth+1];    /* scratch string */
	FILE     *fp, *hd;             /* pointer to output/input files */
	int      i;                    /* counter */
	int      widx;                 /* widget index */
	int      slen;                 /* string length */
	XmString mstr;                 /* Xm text string */
	char     *cstr;                /* C string pointer */
	Arg      al[5];                /* argument list */
	int      ac;                   /* argument counter */

	/* executable code */

	env = ( char *)getenv( "SH_USERDIR" );
	if  (env == NULL)  return;
	if  (strlen(env)+strlen(EXTPROCFILE) > cBcFileLth)  return;
	strcpy( fname, env );
	strcat( fname, EXTPROCFILE );
	/* for testing */
	/*strcat( fname, "_test" );*/

	fp = fopen( fname, "w" );
	if  (fp == NULL)  return;

	/* copy header text */
	env = (char *)getenv( "SH_ROOT" );
	if  (env == NULL)  return;
	if  (strlen(env) > cBcFileLth-24)  return;
	strcpy( fname, env );
	strcat( fname, "/plugins/header_text.txt" );
	hd = fopen( fname, "r" );
	if  (hd != NULL)
		while  (fgets(str,cBcLineLth,hd) != NULL)
			fprintf( fp, "%s", str );

	for  (i=0; i<CL3C_PLUGIN_MAXPROC; i++)  {

		/* write out only active entries */
		if  (cl3v_extproc[i].progname[0]     == '\0'
			&& cl3v_extproc[i].shname[0]      == '\0'
			&& cl3v_extproc[i].evtfile[0]     == '\0'
			&& cl3v_extproc[i].evtinput[0]    == '\0'
			&& cl3v_extproc[i].waveform[0]    == '\0'
			&& cl3v_extproc[i].currwindow[0]  == '\0'
			&& cl3v_extproc[i].shell_bg       == FALSE
			&& cl3v_extproc[i].clear_phases   == FALSE
			&& cl3v_extproc[i].clear_theo     == FALSE
			&& cl3v_extproc[i].reread_plugins == FALSE)
			continue;

		/* get label name */
		switch (i+1)  {
		case CL3C_PLUGIN_LOCPROC: widx = k_widget_button_locsat_hypo; break;
		case CL3C_PLUGIN_INIPROC:
		case CL3C_PLUGIN_ENDPROC: widx = -1; break;
		default: widx = k_widget_eproc1+i;
		} /*endswitch*/
		if  (widx > 0)  {
			ac=0;
			XtSetArg( al[ac], XmNlabelString, &mstr ); ac++;
			XtGetValues( w[widx], al, ac );
			XmStringGetLtoR( mstr, cl3v_char_set, &cstr );
			if  (strlen(cstr) < cBcLineLth)  strcpy( str, cstr );
			/* to avoid memory leaks, release the unwanted buffer space */
			XmStringFree(mstr);
			XtFree(cstr);
			cstr = str;
			while  (*cstr != '\0')  {
				if  (*cstr == ' ')  *cstr = '_';
				cstr++;
			} /*endif*/
		} else {
			strcpy( str, "unused" );
		} /*endif*/
		fprintf( fp, "label      %2d %s\n", i+1, str );
		if  (cl3v_extproc[i].currwindow[0] != '\0')  {
			env = (char *)getenv( "SH_SCRATCH" );
			slen = (env == NULL) ? 0 : strlen(env);
			if  (strncmp(env,cl3v_extproc[i].currwindow,slen) != 0)  slen = 0;
			fprintf( fp, "currwindow %2d %s\n", i+1, cl3v_extproc[i].currwindow+slen );
		} /*endif*/
		if  (cl3v_extproc[i].evtfile[0] != '\0')  {
			env = (char *)getenv( "SH_SCRATCH" );
			slen = (env == NULL) ? 0 : strlen(env);
			if  (strncmp(env,cl3v_extproc[i].evtfile,slen) != 0)  slen = 0;
			fprintf( fp, "writeevt   %2d %s\n", i+1, cl3v_extproc[i].evtfile+slen );
		} /*endif*/
		if  (cl3v_extproc[i].waveform[0] != '\0')  {
			env = (char *)getenv( "SH_SCRATCH" );
			slen = (env == NULL) ? 0 : strlen(env);
			if  (strncmp(env,cl3v_extproc[i].waveform,slen) != 0)  slen = 0;
			fprintf( fp, "waveform   %2d %s\n", i+1, cl3v_extproc[i].waveform+slen );
		} /*endif*/
		if  (cl3v_extproc[i].shname[0] != '\0')  {
			env = (char *)getenv( "SH_USERDIR" );
			slen = (env == NULL) ? 0 : strlen(env);
			if  (strncmp(env,cl3v_extproc[i].shname,slen) != 0)  slen = 0;
			fprintf( fp, "shcfile    %2d %s\n", i+1, cl3v_extproc[i].shname+slen );
		} /*endif*/
		if  (cl3v_extproc[i].progname[0] != '\0')  {
			env = (char *)getenv( "SH_USERDIR" );
			slen = (env == NULL) ? 0 : strlen(env);
			if  (strncmp(env,cl3v_extproc[i].progname,slen) != 0)  slen = 0;
			fprintf( fp, "shell      %2d %s\n", i+1, cl3v_extproc[i].progname+slen );
		} /*endif*/
		if  (cl3v_extproc[i].shell_bg)
			fprintf( fp, "bgshell    %2d\n", i+1 );
		if  (cl3v_extproc[i].clear_theo)
			fprintf( fp, "remtheo    %2d\n", i+1 );
		if  (cl3v_extproc[i].clear_phases)
			fprintf( fp, "resetpar   %2d\n", i+1 );
		if  (cl3v_extproc[i].evtinput[0] != '\0')  {
			env = (char *)getenv( "SH_SCRATCH" );
			slen = (env == NULL) ? 0 : strlen(env);
			if  (strncmp(env,cl3v_extproc[i].evtinput,slen) != 0)  slen = 0;
			fprintf( fp, "readevt    %2d %s\n", i+1, cl3v_extproc[i].evtinput+slen );
		} /*endif*/
		if  (cl3v_extproc[i].reread_plugins)
			fprintf( fp, "reread     %2d\n", i+1 );
		fprintf( fp, "\n" );

	} /*endif*/

	fclose( fp );

} /* end of cl3_write_extproc_file */



/*---------------------------------------------------------------------------*/



#define CONTENTFILE "plugin_content.txt"


void cl3_request_extproc( Widget w[], TSyStatus *status )

/* Request list of available plugins
 *
 * parameters of routine
 * Widget     w[]; input; widget array
 * TSyStatus  *status; output; return status
 */
{
	/* local variables */
	char     *env;                    /* pointer to environment */
	char     contfile[cBcFileLth+1];  /* content file */
	char     cmd[cBcLongStrLth+1];    /* shell command */
	FILE     *fp;                     /* pointer to input file */
	char     line[cBcLineLth+1];      /* current line of file */
	int      btnnum;                  /* number of button */
	int      i;                       /* counter */
	TSyBoolean have_already;          /* have this already? */
	Arg      args[5];                 /* Xt arguments */
	XmString xmstr;                   /* Xm string */

	/* executable code */

	/* create filename for content file */
	env = (char *)getenv( "SH_SCRATCH" );
	if  (env == NULL)  {
		*status = CL3E_NOENVIRONMENT;
		return;
	} /*endif*/
	if  (strlen(env)+strlen(id_shv)+strlen(CONTENTFILE) > cBcFileLth)  {
		*status = CL3E_STROVFL;
		return;
	} /*endif*/
	strcpy( contfile, env );
	strcat( contfile, id_shv );
	strcat( contfile, CONTENTFILE );

	/* get content file from source address */
	env = (char *)getenv( "SH_UTIL" );
	if  (env == NULL)  {
		*status = CL3E_NOENVIRONMENT;
		return;
	} /*endif*/
	if  (strlen(env)+strlen(GpGetString(cGpS_ftp_address))
		+strlen(GpGetString(cGpS_ftp_path))+strlen(contfile) > cBcLongStrLth-44) {
		*status = CL3E_STROVFL;
		return;
	} /*endif*/
	sprintf( cmd, "%s/shm_get_plugin_content.csh ftp://%s/%s/plugins %s",
		env, GpGetString(cGpS_ftp_address), GpGetString(cGpS_ftp_path), contfile);
	if  (GpGetInt(cGpI_debug_level) > 2)
		printf( "SHM-dbg3: execute command >%s<\n", cmd );
	system( cmd );

	fp = fopen( contfile, "r" );
	if  (fp == NULL)  {
		*status = CL3E_PLUGIN_CONTENT;
		return;
	} /*endif*/

	btnnum = 0;
	while  (fgets(line,cBcLineLth,fp) != NULL)  {

		i = strlen(line);
		if  (line[i-1] == '\n')  line[i-1] = '\0';

		have_already = FALSE;
		for  (i=0; i<CL3C_PLUGIN_MAXPROC; i++)
			if  (strcmp(line,cl3v_extproc[i].pluginname) == 0)  {
				have_already = TRUE;
				break;
			} /*endif*/

		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: got plugin content line: %s (%d)\n",
				line, have_already );

		if  (!have_already && btnnum < CL3C_MAX_PLUGIN)  {
			xmstr = XmStringCreateLtoR( line, "" );
			i = 0;
			XtSetArg( args[i], XmNlabelString, xmstr ); i++;
			XtSetValues( w[k_widget_plugin_1+btnnum], args, i );
			XtSetSensitive( w[k_widget_plugin_1+btnnum], TRUE );
			XmStringFree( xmstr );			
			btnnum++;
		} /*endif*/

	} /*endwhile*/

	fclose( fp );

	if  (btnnum == 0)  {
		*status = CL3E_NO_NEW_PLUGIN;
		return;
	} /*endif*/

	while  (btnnum < CL3C_MAX_PLUGIN)  {
		XtSetSensitive( w[k_widget_plugin_1+btnnum], FALSE );
		btnnum++;
	} /*endif*/

	if  (XtIsManaged(w[k_widget_add_plugin_box]))  {
		XtUnmanageChild( w[k_widget_add_plugin_box] );
	} else {
		XtManageChild( w[k_widget_add_plugin_box] );
	} /*endif*/

} /* end of cl3_request_extproc */



/*---------------------------------------------------------------------------*/



void cl3_add_extproc( Widget w[], TSyStatus *status )

/* Adds plugin to menu
 *
 * parameters of routine
 * Widget     w[]; input; widget array
 * TSyStatus  *status; output; return status
 */
{
	/* local variables */
	int      i;                    /* counter */
	char     cmd[cBcLongStrLth];   /* shell command */
	char     *env;                 /* pointer to environment */
	int      freeplugin;           /* free plugin index */
	XmString mstr;                 /* Xm text string */
	char     *cstr;                /* C string pointer */
	Arg      al[5];                /* argument list */
	int      ac;                   /* argument counter */

	/* executable code */

	for  (i=0; i<CL3C_MAX_PLUGIN; i++)  {
		/* request plugin if button selected */
		if  (XmToggleButtonGetState(w[k_widget_plugin_1+i]))  {
			/* read string from button */
			ac=0;
			XtSetArg( al[ac], XmNlabelString, &mstr ); ac++;
			XtGetValues( w[k_widget_plugin_1+i], al, ac );
			XmStringGetLtoR( mstr, cl3v_char_set, &cstr );
			if  (GpGetInt(cGpI_debug_level) > 1)
				printf( "SHM-dbg2: request plugin %s\n", cstr );
			/* build command line */
			env = (char *)getenv( "SH_UTIL" );
			if  (env == NULL)  {
				*status = CL3E_NOENVIRONMENT;
				return;
			} /*endif*/
			if  (strlen(env)+strlen(GpGetString(cGpS_ftp_address))+strlen(cstr)
				+strlen(GpGetString(cGpS_ftp_path)) > cBcLongStrLth-28)  {
				*status = CL3E_STROVFL;
				XmStringFree(mstr);
				XtFree(cstr);
				return;
			} /*endif*/
			freeplugin = cl3_next_free_plugin();
			if  (freeplugin < 0)  {
				*status = CL3E_NO_MORE_PLUGIN;
				XmStringFree(mstr);
				XtFree(cstr);
				return;
			} /*endif*/
			sprintf( cmd, "%s/shm_install_plugin.csh ftp://%s/%s/plugins %s %d",
				env, GpGetString(cGpS_ftp_address), GpGetString(cGpS_ftp_path),
				cstr, freeplugin );
			XmStringFree(mstr);
			XtFree(cstr);
			/* execute command to retrieve plugin tarball, extract it,
			 * append plugin info to plugins.txt */
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SHM-dbg3: execute >%s<\n", cmd );
			system( cmd );
			/* read in changed plugin definitions */
			cl3_init_extproc_entries( w );
			/* rewrite plugin definitions (incl. new plugins, reordered) */
			cl3_write_extproc_file( w );
		} /*endif*/
	} /*endfor*/

} /*end of cl3_add_extproc */



/*---------------------------------------------------------------------------*/



static int cl3_next_free_plugin( void )

/* returns next free plugin number
 *
 * no parameters passed
 */
{
	/* local variables */
	int      i;      /* counter */

	/* executable code */

	for  (i=0; i<CL3C_PLUGIN_MAXMENUPROC; i++)  {
		if  (cl3v_extproc[i].progname[0]     == '\0'
			&& cl3v_extproc[i].shname[0]      == '\0'
			&& cl3v_extproc[i].evtfile[0]     == '\0'
			&& cl3v_extproc[i].evtinput[0]    == '\0'
			&& cl3v_extproc[i].waveform[0]    == '\0'
			&& cl3v_extproc[i].currwindow[0]  == '\0'
			&& cl3v_extproc[i].shell_bg       == FALSE
			&& cl3v_extproc[i].clear_phases   == FALSE
			&& cl3v_extproc[i].clear_theo     == FALSE
			&& cl3v_extproc[i].reread_plugins == FALSE)
			return (i+1);
	} /*endfor*/

	/* nothing found */
	return (-1);

} /* end of cl3_next_free_plugin */



/*---------------------------------------------------------------------------*/



static void cb3_complete_event_info( CUT_PARAMS *par, EvEventT *event )

/* complete event information for output into evt-file.  Computes mean
 * magnitudes and missing azimuth/back-zimuth values, if possible
 *
 * parameters of routine
 * CUT_PARAMS    *par;        input; analysis parameters
 * EvEventT      *event;      modify; event params for output
 */
{
	/* local variables */
	TPiPhaseRoot *proot;                 /* pointer to phase root */
	TPiPhaseList *plist;                 /* pointer to phase list */
	TPiPhase *t_phase;                   /* phase pointer */
	int      p;                          /* phase counter */
	int      i;                          /* counter */
	int      cnt[EvcMagLast];            /* magnitude counter */
	float    avmag[EvcMagLast];          /* mean magnitudes */
	TSyStatus locstat;                   /* local status */

	/* executable code */

	for  (i=0; i<EvcMagLast; i++)  {cnt[i] = 0; avmag[i] = 0.0;}
	locstat = cBcNoError;

	proot = NULL;
	FOREVER  {

		proot = PiNextPhaseRoot( proot );
		if  (proot == NULL)  break;

		plist = PiPhaseListOfRoot( proot );
		t_phase = NULL;
		for  (p=0; p<PiListLength(plist); p++)  {
			t_phase = PiNextPhase( plist, t_phase, &locstat );
			if  (SySevere(&locstat))  return;
			if  (t_phase == NULL)  return;  /* should not happen */

			/* sum up magnitudes for mean value */
			if  (t_phase->magnitude != 0.0)  {
				switch  (t_phase->magn_source)  {
				case cPiMagnMb:       i = EvcMagMb;  break;
				case cPiMagnMsPlain:
				case cPiMagnMsCNa:
				case cPiMagnMsCEu:
				case cPiMagnMsCO:
				case cPiMagnMsO:      i = EvcMagMs;  break;
				case cPiMagnMl:       i = EvcMagMl;  break;
				case cPiMagnMw:       i = EvcMagMw;  break;
				case cPiMagnMbb:      i = EvcMagMbb; break;
				case cPiMagnMu:       i = EvcMagMu;  break;
				default:              i = EvcMagLast;
				} /*endswitch*/
				if  (i != EvcMagLast)  {
					cnt[i]++;
					avmag[i] += t_phase->magnitude;
				} /*endif*/
			} /*endif*/
			if  (t_phase->bb.mbb != 0.0)  {
				cnt[EvcMagMbb]++;
				avmag[EvcMagMbb] += t_phase->bb.mbb;
			} /*endif*/

		} /*endfor*/

	} /*endfor*/

	for  (i=0; i<EvcMagLast; i++)
		if  (cnt[i] >= 1)
			event->meanmag[i] = avmag[i] / (float)cnt[i];

} /* end of cb3_complete_event_info */



/*---------------------------------------------------------------------------*/

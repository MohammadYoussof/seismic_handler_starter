
/* file globalparams.c
 *      ==============
 *
 * version 25, 2-Mar-2007
 *
 * Management of global parameters
 * K. Stammler, 9-May-2006
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1992,  Klaus Stammler, Federal Institute for Geosciences
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
#include "utusrdef.h"
#include "globalparams.h"


/* local types */
typedef struct _string_elem {
	struct _string_elem   *next;  /* pointer to next string element */
	char             *string;     /* string values */
	int              strlth;      /* string length excl. term null */
} TGpStringElem;



/* integer variables
 * -----------------
 */

static char *gpv_i_name[] = {
	"debug_level",
	"parfile_version",
	"parfile_subversion",
	"min_drag_box_width",
	"double_click_time",
	"x_max_drawlth",
	"drag_box_rubber_val",
	"default_quality",
	"default_event_type",
	"default_phase_flags",
	"default_depth_type",
	"default_loc_quality",
	"max_cursor_form",
	"top_margin",
	"parameter_box_x",
	"parameter_box_y",
	"parameter_box_w",
	"parameter_box_h",
	"phase_box_x",
	"phase_box_y",
	"phase_box_w",
	"phase_box_h",
	"window_main_x",
	"window_main_y",
	"window_main_w",
	"window_main_h",
	"single_trace_box_x",
	"single_trace_box_y",
	"single_trace_box_w",
	"single_trace_box_h",
	"window_border",
	"draw_area_width",
	"draw_area_height",
	"trace_normalisation",
	"spectrogram_width",
	"spectrogram_step",
	"keysym_arrow_up",
	"keysym_arrow_down",
	"keysym_arrow_left",
	"keysym_arrow_right",
	"i_last_parameter"
};

/* default values if no parameter file is found */
static int gpv_i_glob[cGpI_last+1] = {
	0,    /* debug_level */
	0,    /* parfile_version */
	0,    /* parfile_subversion */
	7,    /* min_drag_box_width */
	200,  /* double_click_time */
	4096, /* x_max_drawlth */
	10,   /* drag_box_rubber_val */
	2,    /* default_quality */
	0,    /* default_phase_type */
	0,    /* default_phase_flags */
	0,    /* default_depth_type */
	0,    /* default_loc_quality */
	5,    /* max_cursor_form */
	20,   /* top margin */
	93,   /* parameter_box_x */
	0,    /* parameter_box_y */
	125,  /* parameter_box_w */
	700,  /* parameter_box_h */
	-38,  /* phase_box_x */
	0,    /* phase_box_y */
	155,  /* phase_box_w */
	720,  /* phase_box_h */
	224,  /* window_main_x */
	220,  /* window_main_y */
	1039, /* window_main_w */
	516,  /* window_main_h */
	224,  /* single_trace_box_x */
	0,    /* single_trace_box_y */
	1039, /* single_trace_box_w */
	203,  /* single_trace_box_h */
	3,    /* window_border */
	1035, /* draw_area_width */
	485,  /* draw_area_height */
	0,    /* trace_normalisation */
	1024, /* spectrogram_width */
	4,    /* spectrogram_step */
	268762962, /* keysym_arrow_up */
	268762964, /* keysym_arrow_down */
	268762961, /* keysym_arrow_left */
	268762963, /* keysym_arrow_right */
	0     /* last parameter */
};


/* float variables
 * -----------------
 */

static char *gpv_f_name[] = {
	"close_phase_resol",
	"phase_ampl_diff",
	"trace_zoom_base",
	"trace_zoom_exp",
	"area_zoom_base",
	"area_zoom_exp",
	"move_wdw_step",
	"default_depth",
	"colour_fg_red",
	"colour_fg_green",
	"colour_fg_blue",
	"colour_bg_red",
	"colour_bg_green",
	"colour_bg_blue",
	"colour_dialog_fg_red",
	"colour_dialog_fg_green",
	"colour_dialog_fg_blue",
	"colour_dialog_bg0_red",
	"colour_dialog_bg0_green",
	"colour_dialog_bg0_blue",
	"colour_dialog_bg1_red",
	"colour_dialog_bg1_green",
	"colour_dialog_bg1_blue",
	"colour_dialog_bg2_red",
	"colour_dialog_bg2_green",
	"colour_dialog_bg2_blue",
	"colour_mark_red",
	"colour_mark_green",
	"colour_mark_blue",
	"colour_mark0_red",
	"colour_mark0_green",
	"colour_mark0_blue",
	"colour_theo_red",
	"colour_theo_green",
	"colour_theo_blue",
	"colour_auto_red",
	"colour_auto_green",
	"colour_auto_blue",
	"colour_crsr_red",
	"colour_crsr_green",
	"colour_crsr_blue",
	"colour_alert_red",
	"colour_alert_green",
	"colour_alert_blue",
	"colour_addfil_red",
	"colour_addfil_green",
	"colour_addfil_blue",
	"calib_wdw_width",
	"calib_wdw_height",
	"calib_azimuth_grid",
	"calib_slowness_grid",
	"sn_noise_start",
	"sn_noise_end",
	"sn_signal_start",
	"sn_signal_end",
	"idphases_tol_trav",
	"idphases_tol_travsurf",
	"idphases_tol_slow",
	"idphases_tol_azim",
	"axis_label_rounding",
	"f_last_parameter"
};

static float gpv_f_glob[cGpF_last+1] = {
	8.0,       /* close_phase_resol */
	60.0,      /* phase_ampl_diff */
	10.0,      /* trace_zoom_base */
	3.3333333, /* trace_zoom_exp */
	10.0,      /* area_zoom_base */
	25.0,      /* area_zoom_exp */
	1.0,       /* move_wdw_step */
	33.0,      /* default_depth */
	0.0,       /* colour_fg_red */
	0.0,       /* colour_fg_green */
	0.0,       /* colour_fg_blue */
	1.0,       /* colour_bg_red */
	1.0,       /* colour_bg_green */
	1.0,       /* colour_bg_blue */
	0.3882,    /* colour_dialog_fg_red */
	0.1569,    /* colour_dialog_fg_green */
	0.1647,    /* colour_dialog_fg_blue */
	0.9686,    /* colour_dialog_bg0_red */
	0.8706,    /* colour_dialog_bg0_green */
	0.7412,    /* colour_dialog_bg0_blue */
	0.9490,    /* colour_dialog_bg1_red */
	0.6941,    /* colour_dialog_bg1_green */
	0.4314,    /* colour_dialog_bg1_blue */
	0.3882,    /* colour_dialog_bg2_red */
	0.1569,    /* colour_dialog_bg2_green */
	0.1647,    /* colour_dialog_bg2_blue */
	1.0,       /* colour_mark_red */
	0.0,       /* colour_mark_green */
	0.0,       /* colour_mark_blue */
	1.0,       /* colour_mark0_red */
	0.7,       /* colour_mark0_green */
	0.7,       /* colour_mark0_blue */
	0.0,       /* colour_theo_red */
	1.0,       /* colour_theo_green */
	0.0,       /* colour_theo_blue */
	1.0,       /* colour_auto_red */
	0.5,       /* colour_auto_green */
	0.5,       /* colour_auto_blue */
	0.0,       /* colour_crsr_red */
	0.0,       /* colour_crsr_green */
	1.0,       /* colour_crsr_blue */
	0.9,       /* colour_alert_red */
	0.0,       /* colour_alert_green */
	0.0,       /* colour_alert_blue */
	0.6,       /* colour_addfil_red */
	0.0,       /* colour_addfil_green */
	0.25,      /* colour_addfil_blue */
	2.5,       /* calib_wdw_width */
	2.5,       /* calib_wdw_height */
	10.0,      /* calib_azimuth_grid */
	0.5,       /* calib_slowness_grid */
	-13.0,     /* sn_noise_start */
	-3.0,      /* sn_noise_end */
	-1.0,      /* sn_signal_start */
	9.0,       /* sn_signal_end */
	20.0,      /* idphases_tol_trav */
	1200.0,    /* idphases_tol_travsurf */
	1.5,       /* idphases_tol_slow */
	20.0,      /* idphases_tol_azim */
	0.01,      /* axis_label_rounding */
	0.0        /* last parameter */
};


/* char variables
 * -----------------
 */

static char *gpv_c_name[] = {
	"filter_type",
	"c_last_parameter"
};

static char gpv_c_glob[cGpC_last+1] = {
	' ',       /* filter_type */
	' '        /* last parameter */
};


/* string variables
 * -----------------
 */

static char *gpv_s_name[] = {
	"defpath_events",
	"defpath_gse",
	"defpath_ah",
	"defpath_q",
	"defpath_evid",
	"defpath_evtout",
	"defpath_data",
	"defpath_help",
	"defpath_errors",
	"defpath_userdir",
	"defpath_extprog",
	"analyst",
	"list_of_analysts",
	"texteditor",
	"refstation",
	"list_of_refstations",
	"default_filter",
	"minmax_format",
	"default_source",
	"auto_phase",
	"depth_phase_list",
	"theo_phase_list",
	"diff_phase_list",
	"event_check_proc",
	"screendump_proc",
	"evtview_proc",
	"reformat_proc",
	"final_proc",
	"motif_log",
	"ftp_address",
	"ftp_path",
	"exclusive_agency",
	"sfdb_command",
	"sfdb_exec_qual",
	"s_last_parameter"
};

static char gpv_s_glob[cGpS_last+1][cGp_TEXTVARLTH+1] = {
	"default",                       /* defpath_events */
	"default",                       /* defpath_gse */
	"default",                       /* defpath_ah */
	"default",                       /* defpath_q */
	"default",                       /* defpath_evid */
	"default",                       /* defpath_evtout */
	"default",                       /* defpath_data */
	"default",                       /* defpath_help */
	"default",                       /* defpath_errors */
	"default",                       /* defpath_userdir */
	"default",                       /* defpath_extprog */
	"",                              /* analyst */
	"",                              /* list_of_analysts */
	"xedit",                         /* texteditor */
	"CENTRE",                        /* refstation */
	"GRA1,MOX,GRFO,GEC2,CENTRE,---", /* list_of_refstations */
	"",                              /* default_filter*/
	"",                              /* minmax_format */
	"UNDEF",                         /* default_source */
	"beam",                          /* auto_phase */
	"pP,sP,pS,sS",                   /* depth_phase_list */
	"P,S,pP,pS,sP,sS,ScS,PcP,PP,SS", /* theo_phase_list */
	"S-P,PP-P,Pg-Pn",                /* diff_phase_list */
	"",                              /* event_check_proc */
	"screendump.csh",                /* screendump_proc */
	"ev2view",                       /* evtview_proc */
	"undefined",                     /* reformat_proc */
	"",                              /* final_proc */
	"",                              /* motif_log */
	"ftp.szgrf.bgr.de",              /* ftp_address */
	"pub/software/shm",              /* ftp_path */
	"",                              /* exclusive_agency */
	"mysql sfdb",                    /* sfdb_command */
	"-B -e",                         /* sfdb_exec_qual */
	"s_last_parameter"               /* last parameter */
};


/* boolean variables
 * -----------------
 */

static char *gpv_b_name[] = {
	"top_down_order",
	"auto_pick_first",
	"auto_scaling",
	"prompt_analyst",
	"reverse_xors",
	"full_phase_names",
	"own_accelerators",
	"small_menu_font",
	"recover_evid",
	"overwrite_string_lists",
	"ignore_data_lock",
	"reads_swap",
	"reads_invhdr",
	"b_last_parameter"
};

static long gpv_b_glob =
	(1 << cGpB_autopick_first)         |
	(1 << cGpB_auto_scaling)           |
	(1 << cGpB_overwrite_string_lists) |
	(1 << cGpB_own_accelerators)       |
	(1 << cGpB_reads_swap)
;


/* string list variables
 * ---------------------
 */

static char *gpv_l_name[] = {
	"station_info_file",
	"filter_lookup_table",
	"sensitivity_file",
	"defpath_filter",
	"defpath_command",
	"defpath_globals",
	"l_last_parameter"
};

static TGpStringElem gpv_l_glob[cGpL_last+1] = {
	{NULL,"default",7},            /* station_info_file */
	{NULL,"default",7},            /* filter_lookup_table */
	{NULL,"default",7},            /* sensitivity_file */
	{NULL,"default",7},            /* defpath_filter */
	{NULL,"default",7},            /* defpath_command */
	{NULL,"default",7},            /* defpath_globals */
	{NULL,"",0}                    /* last parameter */
};


/* prototypes of local routines */
static void GpReadParametersFromFile( char fname[] );
static void GpEvaluateVar( char val[] );


/*----------------------------------------------------------------------------*/



void GpReadParfile( void )

/* Reads parameters from file
 *
 * no parameters passed
 */
{
	/* local variables */
	char     *env;               /* pointer to environment */
	char     parfile[cBcFileLth+1]; /* name of parameter file */
	char     *cptr;              /* char pointer */
	int      i;                  /* counter */

	/* executable code */

	/* get parameter file */
	env = (char *)getenv( "SH_USER_PARAM" );
	if  (env == NULL)  {
		env = (char *)getenv( "SH_INPUTS" );
		if  (strlen(env) < cBcFileLth-15)  {
			strcpy( parfile, env );
			strcat( parfile, "/shm-config.txt" );
		} else {
			fprintf( stderr, "configuration filename too long.  Abort.\n" );
			exit( 1 );
		} /*endif*/
	} else {
		if  (strlen(env) < cBcFileLth)  {
			strcpy( parfile, env );
		} else {
			fprintf( stderr, "environment SH_USER_PARAM too long.  Abort.\n" );
			exit( 1 );
		} /*endif*/
	} /*endif*/

	GpReadParametersFromFile( parfile );

	/* set 'default' strings */
	if  (strcmp(gpv_s_glob[cGpS_defpath_events],"default") == 0)  {
		cptr = (char *)getenv( "HOME" );
		if  (cptr == NULL)  {printf("HOME not defined\n"); exit(1);}
		if  (strlen(cptr) < cGp_TEXTVARLTH)
			strcpy( gpv_s_glob[cGpS_defpath_events], cptr );
	} /*endif*/
	if  (strcmp(gpv_s_glob[cGpS_defpath_gse],"default") == 0)  {
		cptr = (char *)getenv( "SH_ROOT" );
		if  (cptr == NULL)  {printf("SH_ROOT not defined\n"); exit(1);}
		if  (strlen(cptr) < cGp_TEXTVARLTH-21)  {
			strcpy( gpv_s_glob[cGpS_defpath_gse], cptr );
			strcat( gpv_s_glob[cGpS_defpath_gse], "/data-examples/gse" );
		} /*endif*/
	} /*endif*/
	if  (strcmp(gpv_s_glob[cGpS_defpath_ah],"default") == 0)  {
		cptr = (char *)getenv( "HOME" );
		if  (cptr == NULL)  {printf("HOME not defined\n"); exit(1);}
		if  (strlen(cptr) < cGp_TEXTVARLTH)
			strcpy( gpv_s_glob[cGpS_defpath_ah], cptr );
	} /*endif*/
	if  (strcmp(gpv_s_glob[cGpS_defpath_q],"default") == 0)  {
		cptr = (char *)getenv( "HOME" );
		if  (cptr == NULL)  {printf("HOME not defined\n"); exit(1);}
		if  (strlen(cptr) < cGp_TEXTVARLTH)
			strcpy( gpv_s_glob[cGpS_defpath_q], cptr );
	} /*endif*/
	if  (strcmp(gpv_s_glob[cGpS_defpath_evid],"default") == 0)  {
		cptr = (char *)getenv( "SH_PRIVATE" );
		if  (cptr == NULL)  cptr = (char *)getenv( "HOME" );
		if  (cptr == NULL)  {printf("HOME not defined\n"); exit(1);}
		if  (strlen(cptr) < cGp_TEXTVARLTH-6)  {
			strcpy( gpv_s_glob[cGpS_defpath_evid], cptr );
			strcat( gpv_s_glob[cGpS_defpath_evid], "/evid/" );
		} /*endif*/
	} /*endif*/
	if  (strcmp(gpv_s_glob[cGpS_defpath_evtout],"default") == 0)  {
		cptr = (char *)getenv( "SH_PRIVATE" );
		if  (cptr == NULL)  cptr = (char *)getenv( "HOME" );
		if  (cptr == NULL)  {printf("HOME not defined\n"); exit(1);}
		if  (strlen(cptr) < cGp_TEXTVARLTH-8)  {
			strcpy( gpv_s_glob[cGpS_defpath_evtout], cptr );
			strcat( gpv_s_glob[cGpS_defpath_evtout], "/evtout/" );
		} /*endif*/
	} /*endif*/
	if  (strcmp(gpv_s_glob[cGpS_defpath_help],"default") == 0)  {
		cptr = (char *)getenv( "SH_ROOT" );
		if  (cptr == NULL)  cptr = (char *)getenv( "HOME" );
		if  (cptr == NULL)  {printf("HOME not defined\n"); exit(1);}
		if  (strlen(cptr) < cGp_TEXTVARLTH-6)  {
			strcpy( gpv_s_glob[cGpS_defpath_help], cptr );
			strcat( gpv_s_glob[cGpS_defpath_help], "/help/" );
		} /*endif*/
	} /*endif*/
	if  (strcmp(gpv_s_glob[cGpS_defpath_errors],"default") == 0)  {
		cptr = (char *)getenv( "SH_ROOT" );
		if  (cptr == NULL)  cptr = (char *)getenv( "HOME" );
		if  (cptr == NULL)  {printf("HOME not defined\n"); exit(1);}
		if  (strlen(cptr) < cGp_TEXTVARLTH-8)  {
			strcpy( gpv_s_glob[cGpS_defpath_errors], cptr );
			strcat( gpv_s_glob[cGpS_defpath_errors], "/errors/" );
		} /*endif*/
	} /*endif*/
	if  (strcmp(gpv_s_glob[cGpS_defpath_userdir],"default") == 0)  {
		/* the usage of SH_USERDIR is for backward compatibility */
		cptr = (char *)getenv( "SH_USERDIR" );
		if  (cptr == NULL)  {
			cptr = (char *)getenv( "HOME" );
			if  (cptr == NULL)  {printf("HOME not defined\n"); exit(1);}
			if  (strlen(cptr) < cGp_TEXTVARLTH-17)  {
				strcpy( gpv_s_glob[cGpS_defpath_userdir], cptr );
				strcat( gpv_s_glob[cGpS_defpath_userdir], "/shfiles/private/" );
			} /*endif*/
		} else {
			if  (strlen(cptr) < cGp_TEXTVARLTH)
				strcpy( gpv_s_glob[cGpS_defpath_userdir], cptr );
		} /*endif*/
	} /*endif*/
	if  (strcmp(gpv_s_glob[cGpS_defpath_extprog],"default") == 0)  {
		/* the usage of SH_UTIL is for backward compatibility */
		cptr = (char *)getenv( "SH_UTIL" );
		if  (cptr == NULL)  {
			cptr = (char *)getenv( "HOME" );
			if  (cptr == NULL)  {printf("HOME not defined\n"); exit(1);}
			if  (strlen(cptr) < cGp_TEXTVARLTH-17)  {
				strcpy( gpv_s_glob[cGpS_defpath_extprog], cptr );
				strcat( gpv_s_glob[cGpS_defpath_extprog], "/util/" );
			} /*endif*/
		} else {
			if  (strlen(cptr) < cGp_TEXTVARLTH)
				strcpy( gpv_s_glob[cGpS_defpath_extprog], cptr );
		} /*endif*/
	} /*endif*/

	/* 'default' in string lists */
	if  (strcmp(gpv_l_glob[cGpL_station_info_file].string,"default") == 0)  {
		cptr = (char *)getenv( "SH_INPUTS" );
		if  (cptr != NULL)  {
			i = strlen( cptr );
			/* "default" value is set by variable initialisation, can't free it */
			/*free( gpv_l_glob[cGpL_station_info_file].string );*/
			gpv_l_glob[cGpL_station_info_file].string =
				(char *)malloc( (int)sizeof(char)*(i+12) );
			strcpy( gpv_l_glob[cGpL_station_info_file].string, cptr );
			strcat( gpv_l_glob[cGpL_station_info_file].string, "STATINF.DAT" );
			gpv_l_glob[cGpL_station_info_file].strlth = i+11;
		} /*endif*/
	} /*endif*/
	if  (strcmp(gpv_l_glob[cGpL_filter_lookup_table].string,"default") == 0)  {
		cptr = (char *)getenv( "SH_INPUTS" );
		if  (cptr != NULL)  {
			i = strlen( cptr );
			/*free( gpv_l_glob[cGpL_filter_lookup_table].string );*/
			gpv_l_glob[cGpL_filter_lookup_table].string =
				(char *)malloc( (int)sizeof(char)*(i+18) );
			strcpy( gpv_l_glob[cGpL_filter_lookup_table].string, cptr );
			strcat( gpv_l_glob[cGpL_filter_lookup_table].string, "filter_lookup.txt" );
			gpv_l_glob[cGpL_filter_lookup_table].strlth = i+17;
		} /*endif*/
	} /*endif*/
	if  (strcmp(gpv_l_glob[cGpL_sensitivity_file].string,"default") == 0)  {
		cptr = (char *)getenv( "SH_INPUTS" );
		if  (cptr != NULL)  {
			i = strlen( cptr );
			/*free( gpv_l_glob[cGpL_sensitivity_file].string );*/
			gpv_l_glob[cGpL_sensitivity_file].string =
				(char *)malloc( (int)sizeof(char)*(i+18) );
			strcpy( gpv_l_glob[cGpL_sensitivity_file].string, cptr );
			strcat( gpv_l_glob[cGpL_sensitivity_file].string, "sensitivities.txt" );
			gpv_l_glob[cGpL_sensitivity_file].strlth = i+17;
		} /*endif*/
	} /*endif*/
	if  (strcmp(gpv_l_glob[cGpL_defpath_filter].string,"default") == 0)
		GpParseStringList( cGpL_defpath_filter, ".,$SH_ROOT/filter" );
	if  (strcmp(gpv_l_glob[cGpL_defpath_command].string,"default") == 0)
		GpParseStringList( cGpL_defpath_command, ".,$SH_ROOT/command" );
	if  (strcmp(gpv_l_glob[cGpL_defpath_globals].string,"default") == 0)
		GpParseStringList( cGpL_defpath_globals, ".,$SH_ROOT/globals/" );

	/* make some strings uppercase */
	ut_cap( gpv_s_glob[cGpS_refstation] );
	ut_cap( gpv_s_glob[cGpS_list_of_refstations] );

	/* check for setting of normalisation */
	if  (gpv_i_glob[cGpI_trace_normalisation] == cGp_NORM_UNDEF)  {
		/* trace_normalisation not set take it from 'old' auto_scaling */
		printf( "--globalparams: warning: auto_scaling obsolete, please set trace_normalisation\n" );
		gpv_i_glob[cGpI_trace_normalisation] =
			(gpv_b_glob & cGpB_auto_scaling) ? cGp_NORM_SW : cGp_NORM_AW;
	} else {
		switch  (gpv_i_glob[cGpI_trace_normalisation])  {
		case cGp_NORM_CONST:  GpSetBoolean( cGpB_auto_scaling, 0 ); break;
		case cGp_NORM_AW:     GpSetBoolean( cGpB_auto_scaling, 0 ); break;
		case cGp_NORM_SW:     GpSetBoolean( cGpB_auto_scaling, 1 ); break;
		default:
			printf( "--globalparams: illegal value of trace_normalisation, please correct\n" );
			GpSetBoolean( cGpB_auto_scaling, 0 );
			gpv_i_glob[cGpI_trace_normalisation] = cGp_NORM_AW;
		} /*endswitch*/
	} /*endif*/

	if  (gpv_i_glob[cGpI_parfile_version] == 0)
		printf( "--globalparams: warning: no version of parameter file found\n" );

} /* end of GpReadParfile */



/*----------------------------------------------------------------------------*/



static void GpReadParametersFromFile( char fname[] )

/* reads parametes from file, calls itself on 'include' statement
 *
 * parameters of routine
 * char       fname[];    input; name of file to read
 */
{
	/* local variables */
	FILE     *fp;                /* pointer to input file */
	char     line[cBcVeryLongStrLth+1]; /* current line of file */
	char     name[cBcLineLth+1]; /* variable name */
	int      namelth;            /* length of name */
	int      itmp;               /* integer scratch */
	float    ftmp;               /* float scratch */
	char     ctmp;               /* char scratch */
	char     stmp[cBcVeryLongStrLth+1]; /* string scratch */
	TGpIntIndex  icnt;           /* integer counter */
	TGpFloatIndex  fcnt;         /* float counter */
	TGpCharIndex  ccnt;          /* char counter */
	TGpStringIndex  scnt;        /* string counter */
	TGpBooleanIndex  bcnt;       /* boolean counter */
	TGpStringListIndex lcnt;     /* string list counter */
	int      found;              /* (boolean) parameter found */
	char     *cptr;              /* char pointer */
	char     *env;               /* pointer to environment */

	/* executable code */

	if  (GpGetInt(cGpI_debug_level) > 3)
		printf( "SHM-dbg3: reading parameters from parfile %s\n", fname );

	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		if  (GpGetInt(cGpI_debug_level) > 2)
			printf( "SHM-dbg3: parfile %s not found\n", fname );
		return;
	} /*endif*/

	while  (fgets(line,cBcVeryLongStrLth,fp) != NULL)  {

		if  (*line == '!' || *line == '#' || *line == '\n')  continue;
		if  (line[1] == '$')  continue;
		if  (sscanf(line,"%s",name) != 1)  continue;
		namelth = strlen( name );
		if  (strlen(line) < namelth)  {
			printf( "--globalparams: empty entry %s, ignore\n", name );
			continue;
		} /*endif*/
		found = 0;

		/* check for include statement */
		if  (strcmp(name,"include") == 0)  {
			if  (GpGetInt(cGpI_debug_level) > 3)
				printf( "SHM-dbg4: include statement found\n", fname );
			if  (sscanf(line+namelth,"%s",stmp) != 1)  {
				printf( "--globalparams: error reading include in parameter file %s\n",
					fname );
			} else {
				if  (strlen(stmp) > cGp_TEXTVARLTH)  {
					printf( "--globalparams: include filename %s too long\n", name );
				} else {
					/* reentrant call, read from $SH_INPUTS if not absolute */
					if  (*stmp == '/')  {
						/* absolute name, dont't change */
						strcpy( line, stmp );
					} else {
						env = (char *)getenv( "SH_INPUTS" );
						if  (strlen(env)+strlen(stmp)+1 > cBcVeryLongStrLth)  {
							printf( "--globalparams: string overflow in include statement\n" );
							continue;
						} /*endif*/
						sprintf( line, "%s/%s", env, stmp );
					} /*endif*/
					GpReadParametersFromFile( line );
				} /*endif*/
			} /*endif*/
			continue;
		} /*endif*/

		/* check for version number */
		if  (strcmp(name,"file_version") == 0)  {
			if  (sscanf(line+namelth,"%s",stmp) != 1)  {
				printf( "--globalparams: error reading version in parameter file %s\n",
					fname );
			} else {
				if  (strlen(stmp) > cGp_TEXTVARLTH)  {
					printf( "--globalparams: include filename %s too long\n", name );
				} else {
					if  (sscanf(stmp,"%d.%d",&itmp,&icnt) == 2)  {
						gpv_i_glob[cGpI_parfile_version] = itmp;
						gpv_i_glob[cGpI_parfile_subversion] = icnt;
						if  (gpv_i_glob[cGpI_parfile_version] > cGp_VERSION
							|| gpv_i_glob[cGpI_parfile_subversion] > cGp_SUBVERSION)  {
							printf( "--globalparams: warning: file version too new; ");
							printf( "expect problems while reading\n" );
							printf( "--globalparams: version %d.%d is expected here\n",
								cGp_VERSION, cGp_SUBVERSION );
						} else if (gpv_i_glob[cGpI_parfile_version] < cGp_VERSION) {
							printf( "--globalparams: warning: old parameter file version; ");
							printf( "consider creating a new one\n" );
							printf( "--globalparams: version %d.%d is expected here\n",
								cGp_VERSION, cGp_SUBVERSION );
						} /*endif*/
					} else {
						printf( "--globalparams: error reading parameter version\n" );
					} /*endif*/
				} /*endif*/
			} /*endif*/
			continue;
		} /*endif*/

		/* integer variables */
		for  (icnt=0; icnt<cGpI_last; icnt++)
			if  (strcmp(gpv_i_name[icnt],name) == 0)  {
				if  (sscanf(line+namelth,"%d",&itmp) != 1)  {
					printf( "--globalparams: error reading parameter %s\n", name );
				} else {
					gpv_i_glob[icnt] = itmp;
					found = 1;
				} /*endif*/
				break;
			} /*endif*/
		if  (found)  continue;

		/* float variables */
		for  (fcnt=0; fcnt<cGpF_last; fcnt++)
			if  (strcmp(gpv_f_name[fcnt],name) == 0)  {
				if  (sscanf(line+namelth,"%f",&ftmp) != 1)  {
					printf( "--globalparams: error reading parameter %s\n", name );
				} else {
					gpv_f_glob[fcnt] = ftmp;
					found = 1;
				} /*endif*/
				break;
			} /*endif*/
		if  (found)  continue;

		/* char variables */
		for  (ccnt=0; ccnt<cGpC_last; ccnt++)
			if  (strcmp(gpv_c_name[ccnt],name) == 0)  {
				/* read first character after blanks and tabs */
				cptr = line + namelth;
				while  (*cptr == ' ' || *cptr == '\t')  cptr++;
				if  (*cptr != '\n' && *cptr != '\0')  {
					gpv_c_glob[ccnt] = *cptr;
					found = 1;
				} /*endif*/
				break;
			} /*endif*/
		if  (found)  continue;

		/* string variables */
		for  (scnt=0; scnt<cGpS_last; scnt++)
			if  (strcmp(gpv_s_name[scnt],name) == 0)  {
				/* skip blanks after keyword and take remaining line as value */
				cptr = line + namelth;
				while  (*cptr == ' ' || *cptr == '\t')  cptr++;
				strcpy( stmp, cptr );
				/* drop the linefeed */
				itmp = strlen( stmp );
				if  (itmp > 0 && stmp[itmp-1] == '\n')  stmp[itmp-1] = '\0';
				if  (*stmp == '\0')  {
					printf( "--globalparams: error reading parameter %s\n", name );
				} else {
					if  (strlen(stmp) > cGp_TEXTVARLTH)  {
						printf( "--globalparams: string parameter %s too long\n", name );
					} else {
						if  (strcmp(stmp,"<NULL>") == 0 || strcmp(stmp,"NULL") == 0
							|| strcmp(stmp,"<null>") == 0 || strcmp(stmp,"null") == 0)  {
							gpv_s_glob[scnt][0] = '\0';
						} else {
							while  (stmp[0] == '$')
								GpEvaluateVar( stmp );
							strcpy( gpv_s_glob[scnt], stmp );
						} /*endif*/
						found = 1;
					} /*endif*/
				} /*endif*/
				break;
			} /*endif*/

		/* boolean variables */
		for  (bcnt=0; bcnt<cGpB_last; bcnt++)
			if  (strcmp(gpv_b_name[bcnt],name) == 0)  {
				if  (sscanf(line+namelth,"%s",&stmp) != 1)  {
					printf( "--globalparams: error reading parameter %s\n", name );
				} else {
					if  (strcmp(stmp,"TRUE") == 0 || strcmp(stmp,"true") == 0
						|| strcmp(stmp,"True") == 0 || strcmp(stmp,"1") == 0)  {
						gpv_b_glob |= (1 << bcnt);
						found = 1;
					} else if  (strcmp(stmp,"FALSE") == 0 || strcmp(stmp,"false") == 0
						|| strcmp(stmp,"False") == 0 || strcmp(stmp,"0") == 0)  {
						gpv_b_glob &= ~(1 << bcnt);
						found = 1;
					} else {
						printf( "--globalparams: illegal boolean value at %s\n", name );
					} /*endif*/
				} /*endif*/
				break;
			} /*endif*/
		if  (found)  continue;

		/* string list variables */
		for  (lcnt=0; lcnt<cGpL_last; lcnt++)
			if  (strcmp(gpv_l_name[lcnt],name) == 0)  {
				GpParseStringList( lcnt, line+namelth );
				found = 1;
			} /*endif*/

		if  (!found)
			printf( "--globalparams: illegal parameter %s in parameter file\n", name );

	} /*endwhile*/

	fclose( fp );

} /* end of GpReadParametersFromFile */



/*----------------------------------------------------------------------------*/



int GpGetInt( TGpIntIndex idx )

/* Returns integer valued global
 *
 * parameters of routine
 * TGpIntIndex   idx;       input; index of variable
 *                          returns value of variable
 */
{
	/* executable code */

	if  (idx >= 0 && idx < cGpI_last)  {
		return gpv_i_glob[idx];
	} else {
		printf( "--globalparams: illegal index %d in GpGetInt\n", idx );
		return 0;
	} /*endif*/

} /* end of GpGetInt */



/*----------------------------------------------------------------------------*/



float GpGetFloat( TGpFloatIndex idx )

/* Returns float valued global
 *
 * parameters of routine
 * TGpFloatIndex   idx;     input; index of variable
 *                          returns value of variable
 */
{
	/* executable code */

	if  (idx >= 0 && idx < cGpF_last)  {
		return gpv_f_glob[idx];
	} else {
		printf( "--globalparams: illegal index %d in GpGetFloat\n", idx );
		return 0;
	} /*endif*/

} /* end of GpGetFloat */



/*----------------------------------------------------------------------------*/



char GpGetChar( TGpCharIndex idx )

/* Returns char valued global
 *
 * parameters of routine
 * TGpCharIndex   idx;      input; index of variable
 *                          returns value of variable
 */
{
	/* executable code */

	if  (idx >= 0 && idx < cGpC_last)  {
		return gpv_c_glob[idx];
	} else {
		printf( "globalparams: illegal index %d in GpGetChar\n", idx );
		return 0;
	} /*endif*/

} /* end of GpGetChar */



/*----------------------------------------------------------------------------*/



char *GpGetString( TGpStringIndex idx )

/* Returns pointer to string valued global
 *
 * parameters of routine
 * TGpStringIndex   idx;    input; index of variable
 *                          returns value of variable
 */
{
	/* executable code */

	if  (idx >= 0 && idx < cGpS_last)  {
		return gpv_s_glob[idx];
	} else {
		printf( "--globalparams: illegal index %d in GpGetString\n", idx );
		return 0;
	} /*endif*/

} /* end of GpGetString */



/*----------------------------------------------------------------------------*/



int GpGetBoolean( TGpBooleanIndex idx )

/* Returns boolean valued global
 *
 * parameters of routine
 * TGpBooleanIndex   idx;   input; index of variable
 *                          returns value of variable
 */
{
	/* executable code */

	if  (idx >= 0 && idx < cGpB_last)  {
		if  ((gpv_b_glob & (1 << idx)) == 0)  {
			return 0;
		} else {
			return 1;
		} /*endif*/
	} else {
		printf( "--globalparams: illegal index %d in GpGetBoolean\n", idx );
		return 0;
	} /*endif*/

} /* end of GpGetBoolean */



/*----------------------------------------------------------------------------*/



char *GpGetStringElem( TGpStringListIndex idx, int elemnum )

/* returns string element number elemnum from string list idx
 *
 * parameters of routine
 * TGpStringListIndex idx;     input; index numer of string list
 * int                elemnum; input; number of string in list (start with 0)
 *                             returns string address or NULL
 */
{
	/* local variables */
	TGpStringElem *sel;     /* pointer to string element */

	/* executable code */

	if  (idx < 0 || idx >= cGpL_last)  return NULL;

	sel = gpv_l_glob+idx;

	while  (elemnum-- > 0)  {
		if  (sel->next == NULL)  return NULL;
		sel = sel->next;
	} /*endwhile*/

	return sel->string;

} /* end of GpGetStringElem */



/*----------------------------------------------------------------------------*/



void GpSetInt( TGpIntIndex idx, int value )

/* Sets integer valued global
 *
 * parameters of routine
 * TGpIntIndex   idx;       input; index of variable
 * int           value;     input; new value of variable
 */
{
	/* executable code */

	if  (idx >= 0 && idx < cGpI_last)  {
		gpv_i_glob[idx] = value;
	} else {
		printf( "--globalparams: illegal index %d in GpSetInt\n", idx );
	} /*endif*/

} /* end of GpSetInt */



/*----------------------------------------------------------------------------*/



void GpSetFloat( TGpFloatIndex idx, float value )

/* Sets float valued global
 *
 * parameters of routine
 * TGpFloatIndex   idx;     input; index of variable
 * float           value;   input; new value of variable
 */
{
	/* executable code */

	if  (idx >= 0 && idx < cGpF_last)  {
		gpv_f_glob[idx] = value;
	} else {
		printf( "--globalparams: illegal index %d in GpSetFloat\n", idx );
	} /*endif*/

} /* end of GpSetFloat */



/*----------------------------------------------------------------------------*/



void GpSetChar( TGpCharIndex idx, char value )

/* Sets char valued global
 *
 * parameters of routine
 * TGpCharIndex   idx;      input; index of variable
 * char           value;    input; new value of variable
 */
{
	/* executable code */

	if  (idx >= 0 && idx < cGpC_last)  {
		gpv_c_glob[idx] = value;
	} else {
		printf( "--globalparams: illegal index %d in GpSetChar\n", idx );
	} /*endif*/

} /* end of GpSetChar */



/*----------------------------------------------------------------------------*/



void GpSetString( TGpStringIndex idx, char str[], int *ok )

/* Sets value of string valued global variable
 *
 * parameters of routine
 * TGpStringIndex   idx;    input; index of variable
 * char             str[];  input; new value of variable
 * int              *ok;    output (if not NULL); 1 = ok, 0 = failure
 */
{
	/* executable code */

	if  (strlen(str) > cGp_TEXTVARLTH)  {
		if  (ok != NULL)  *ok = 0;
		return;
	} /*endif*/

	if  (idx >= 0 && idx < cGpS_last)  {
		strcpy( gpv_s_glob[idx], str );
		if  (ok != NULL)  *ok = 1;
	} else {
		if  (ok != NULL)  *ok = 0;
	} /*endif*/

} /* end of GpSetString */



/*----------------------------------------------------------------------------*/



void GpSetBoolean( TGpBooleanIndex idx, int value )

/* Sets boolean valued global
 *
 * parameters of routine
 * TGpBooleanIndex   idx;   input; index of variable
 * int               value; input; new value of variable
 */
{
	/* executable code */

	if  (idx >= 0 && idx < cGpB_last)  {
		if  (value)  {
			gpv_b_glob |= (1 << idx);
		} else {
			gpv_b_glob &= ~(1 << idx);
		} /*endif*/
	} else {
		printf( "--globalparams: illegal index %d in GpSetBoolean\n", idx );
	} /*endif*/

} /* end of GpSetBoolean */



/*----------------------------------------------------------------------------*/



void GpReadParameter( char parname[], int maxlth, char value[], int *ok )

/* Reads parameter of name parname from parameter file.
 *
 * parameters of routine
 * char       parname[]; input, name of parameter
 * int        maxlth; input; maximum length of output string
 * char       value[]; output; value of parameter
 * int        *ok; output; 1=ok, 0=not found or string too short
 */
{
	/* local variables */
	char     *env;                   /* pointer to environment */
	char     parfile[cBcFileLth+1];  /* name of parameter file */
	FILE     *fp;                    /* pointer to parameter file */
	char     line[cBcVeryLongStrLth+1];  /* current line of file */
	int      namelth;                /* length of parameter name */
	char     *vptr;                  /* pointer to parameter value */
	int      slth;                   /* string length */

	/* executable code */

	/* get parameter file */
	env = (char *)getenv( "SH_USER_PARAM" );
	if  (env == NULL)  {
		env = (char *)getenv( "SH_INPUTS" );
		if  (strlen(env) < cBcFileLth-15)  {
			strcpy( parfile, env );
			strcat( parfile, "/shm-config.txt" );
		} else {
			fprintf( stderr, "configuration filename too long.  Abort.\n" );
			exit( 1 );
		} /*endif*/
	} else {
		if  (strlen(env) < cBcFileLth)  {
			strcpy( parfile, env );
		} else {
			fprintf( stderr, "environment SH_USER_PARAM too long.  Abort.\n" );
			exit( 1 );
		} /*endif*/
	} /*endif*/

	namelth = strlen( parname );

	*ok = 0;
	fp = fopen( parfile, "r" );
	if  (fp == NULL)  return;

	while  (fgets(line,cBcVeryLongStrLth,fp) != NULL)  {

		if  (*line == '!' || *line == '#' || *line == '\n')  continue;
		if  (strncasecmp(line,parname,namelth) == 0
			&& (line[namelth] == ' ' || line[namelth] == '\t'))  {
			vptr = line + namelth;
			while  (*vptr == ' ')  vptr++;
			slth = strlen( vptr );
			if  (vptr[slth-1] == '\n')  vptr[--slth] = '\0';
			*ok = (strlen(vptr) < maxlth);
			if  (*ok)  strcpy( value, vptr );
			break;
		} /*endif*/

	} /*endwhile*/

	fclose( fp );

} /* end of GpReadParameter */



/*----------------------------------------------------------------------------*/



void GpParseTextList( char str[], TGpTextList *tl )

/* Parses comma separated text string and returns pointers to elements of list.
 * Data structure returned must be freed after use (GpFreeTextList).
 *
 * parameters of routine
 * char       str[];    input; input string to be parsed
 * GpTextList *tl;      output; elements of list
 */
{
	/* local variables */
	int      strlth;       /* length of input string */
	int      i, j;         /* counters */

	/* executable code */

	strlth = strlen( str );
	if  (strlth == 0)  {
		tl->numelem = 0;
		tl->elem = NULL;
		tl->mem = NULL;
		return;
	} /*endif*/

	tl->mem = (char *)malloc( strlth+1 );
	if  (tl->mem == NULL)  {
		fprintf( stderr, "GpParseTextList: error allocating memory.  Abort.\n" );
		exit( 1 );
	} /*endif*/
	strcpy( tl->mem, str );

	/* count elements */
	tl->numelem = (*str == '\0') ? 0 : 1;
	for  (i=0; i<strlth; i++)
		if  (str[i] == ',')  (tl->numelem)++;

	tl->elem = (char **)malloc( (int)sizeof(char *)*(tl->numelem) );
	if  (tl->elem == NULL)  {
		fprintf( stderr, "GpParseTextList: error allocating memory.  Abort.\n" );
		exit( 1 );
	} /*endif*/

	j = 1;
	tl->elem[0] = tl->mem;
	for  (i=0; i<strlth; i++)  {
		if  (tl->mem[i] == ',')  {
			tl->mem[i] = '\0';
			tl->elem[j++] = tl->mem+i+1;
		} /*endif*/
	} /*endfor*/

	/* debug output */
	/*
	for  (j=0; j<tl->numelem; j++)
		printf( "debug-textlist: %2d >%s<\n", j+1, tl->elem[j] );
	*/

} /* end of GpParseTextList */



/*----------------------------------------------------------------------------*/



void GpFreeTextList( TGpTextList *tl )

/* Frees memory of textlist
 *
 * parameters of routine
 * TGpTextList         modify; text list to be freed
 */
{
	/* executable code */

	if  (tl->numelem == 0)  return;

	if  (tl->elem != NULL)  free( tl->elem );
	if  (tl->mem != NULL)  free( tl->mem );

} /* end of GpFreeTextList */



/*----------------------------------------------------------------------------*/



void GpDumpParams( void )

/* write all parameters out to stdout
 *
 * no parameters passed
 *
 */
{
	/* local variables */
	int    i;    /* counter */
	int    j;    /* string counter */
	char   *s;   /* string pointer */

	/* executable code */

	for  (i=0; i<cGpI_last; i++)
		printf( "%s: %d\n", gpv_i_name[i], gpv_i_glob[i] );
	for  (i=0; i<cGpF_last; i++)
		printf( "%s: %g\n", gpv_f_name[i], gpv_f_glob[i] );
	for  (i=0; i<cGpC_last; i++)
		printf( "%s: %c\n", gpv_c_name[i], gpv_c_glob[i] );
	for  (i=0; i<cGpS_last; i++)
		printf( "%s: %s\n", gpv_s_name[i], gpv_s_glob[i] );
	for  (i=0; i<cGpB_last; i++)
		printf( "%s: %d\n", gpv_b_name[i], ((1<<i) & gpv_b_glob) );
	for  (i=0; i<cGpL_last; i++)  {
		for  (j=0;;j++)  {
			s = GpGetStringElem( i, j );
			if  (s == NULL)  break;
			printf( "%s (%d): %s\n", gpv_l_name[i], j, s );
		} /*endfor*/
	} /*endfor*/

} /* end of GpDumpParams */



/*----------------------------------------------------------------------------*/



void GpParseStringList( TGpStringListIndex idx, char line[] )

/* parses string list and puts result to list number idx
 *
 * parameters of routine
 * TGpStringListIndex idx;    input; string list variable index
 * char               line[]; input; line to be parsed
 */
{
	/* local variables */
	int      i, j;                /* counters */
	TGpStringElem *sel;           /* pointer to string element */
	char     val[cBcLineLth+1];   /* string value */

	/* excutable code */

	sel = gpv_l_glob+idx;

	/* !!! should free list before (if not "default") */

	sel->next = NULL;
	sel->string = '\0';
	sel->strlth = 0;

	while  (*line != '\0')  {

		/* skip blanks */
		while  (*line == ' ' || *line == '\t')  line++;
		if  (*line == '\0' || *line == '\n')  break;

		/* allocate next StringElem if necessary */
		if  (sel->string != '\0')  {
			sel->next = (TGpStringElem *)malloc( (int)sizeof(TGpStringElem) );
			if  (sel->next == NULL)  {
				printf( "--globalparams: error allocating memory (1). Abort\n" );
				exit( 1 );
			} /*endif*/
			sel = sel->next;
			sel->next = NULL;
			sel->string = '\0';
			sel->strlth = 0;
		} /*endif*/

		/* get next string length */
		i = 0;
		while  (line[i] != '\0' && line[i] != ' ' && line[i] != '\n'
			&& line[i] != '\t' && line[i] != ',' && line[i] != ';')
			i++;
		if  (i >= cBcLineLth-1)  {
			printf( "--globalparams: string element (idx: %d) too long\n", idx );
			return;
		} /*endif*/

		/* copy string to StringElem structure */
		if  (i > 0)  {

			/* copy string and translate if necessary */
			strncpy( val, line, i );
			val[i] = '\0';

			while  (*val == '$')
				GpEvaluateVar( val );

			j = strlen( val );
			sel->string = (char *)malloc( (int)sizeof(char)*(j+1) );
			if  (sel->string == NULL)  {
				printf( "--globalparams: error allocating memory (2). Abort\n" );
				exit( 1 );
			} /*endif*/
			strcpy( sel->string, val );
			sel->strlth = j;

		} /*endif*/

		if  (line[i] == '\0' || *line == '\n')  break;
		line += i+1;

	} /*endwhile*/

} /* end of GpParseStringList */



/*----------------------------------------------------------------------------*/



static void GpEvaluateVar( char val[] )

/* Evaluates value of variable
 *
 * parameters of routine
 * char       val[];     modify; input: variable name, output: value
 */
{
	/* local variable */
	char     vname[cBcLineLth+1];     /* variable name */
	char     appdx[cBcLineLth+1];     /* appendix (after '/') */
	int      j;                       /* counter */
	int      ok;                      /* string value found */
	char     *env;                    /* pointer to environment */

	/* executable code */

	if  (*val != '$')  return;  /* leave unchanged */

	/* try 'v$'-variable in configuration file */
	vname[0] = 'v';
	j = 0;
	while  (val[j] != '/' && val[j] != '\0')  {
		vname[j+1] = val[j];
		j++;
	} /*endwhile*/
	vname[j+1] = '\0';
	*appdx = '\0';
	if  (val[j] == '/')  strcpy( appdx, val+j );
	GpReadParameter( vname, cBcLineLth, val, &ok );

	if  (!ok)  {
		/* try to find environment variable */
		env = (char *)getenv( vname+2 );  /* skip v$ */
		if  (env == NULL)  {
			strcpy( val, vname+2 );  /* take away '$', results in an error later */
		} else if  (strlen(env) < cBcLineLth)  {
			strcpy( val, env );
		} else {
			strcpy( val, vname+2 );  /* results in an error later */
		} /*endif*/
	} /*endif*/

	if  (*appdx != '\0')
		if  (strlen(val)+strlen(appdx) < cBcLineLth)
			strcat( val, appdx );

} /* end of GpEvaluateVar */



/*----------------------------------------------------------------------------*/

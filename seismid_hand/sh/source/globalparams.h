
/* file globalparams.h
 *      ==============
 *
 * version 25, 1-Mar-2007
 *
 * Management of global parameters
 * K. Stammler, 9-May-2006
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


#define cGp_TEXTVARLTH 131
#define cGp_VERSION 1
#define cGp_SUBVERSION 0

/* normalisation modes */
#define cGp_NORM_UNDEF  0
#define cGp_NORM_CONST  1
#define cGp_NORM_AW     2
#define cGp_NORM_SW     3
#define cGp_NORM_MAXVAL 3


/* integer variables
 * -----------------
 */

typedef enum {
	cGpI_debug_level,
	cGpI_parfile_version,
	cGpI_parfile_subversion,
	cGpI_min_drag_box_width,
	cGpI_double_click_time,
	cGpI_x_max_drawlth,
	cGpI_drag_box_rubber_val,
	cGpI_default_quality,
	cGpI_default_event_type,
	cGpI_default_phase_flags,
	cGpI_default_depth_type,
	cGpI_default_loc_quality,
	cGpI_max_cursor_form,
	cGpI_top_margin,
	cGpI_parameter_box_x,
	cGpI_parameter_box_y,
	cGpI_parameter_box_w,
	cGpI_parameter_box_h,
	cGpI_phase_box_x,
	cGpI_phase_box_y,
	cGpI_phase_box_w,
	cGpI_phase_box_h,
	cGpI_window_main_x,
	cGpI_window_main_y,
	cGpI_window_main_w,
	cGpI_window_main_h,
	cGpI_single_trace_box_x,
	cGpI_single_trace_box_y,
	cGpI_single_trace_box_w,
	cGpI_single_trace_box_h,
	cGpI_window_border,
	cGpI_draw_area_width,
	cGpI_draw_area_height,
	cGpI_trace_normalisation,
	cGpI_spectrogram_width,
	cGpI_spectrogram_step,
	cGpI_keysym_arrow_up,
	cGpI_keysym_arrow_down,
	cGpI_keysym_arrow_left,
	cGpI_keysym_arrow_right,
	cGpI_last
} TGpIntIndex;


/* float variables
 * -----------------
 */

typedef enum {
	cGpF_close_phase_resol,
	cGpF_phase_ampl_diff,
	cGpF_trace_zoom_base,
	cGpF_trace_zoom_exp,
	cGpF_area_zoom_base,
	cGpF_area_zoom_exp,
	cGpF_move_wdw_step,
	cGpF_default_depth,
	cGpF_colour_fg_red,
	cGpF_colour_fg_green,
	cGpF_colour_fg_blue,
	cGpF_colour_bg_red,
	cGpF_colour_bg_green,
	cGpF_colour_bg_blue,
	cGpF_colour_dialog_fg_red,
	cGpF_colour_dialog_fg_green,
	cGpF_colour_dialog_fg_blue,
	cGpF_colour_dialog_bg0_red,
	cGpF_colour_dialog_bg0_green,
	cGpF_colour_dialog_bg0_blue,
	cGpF_colour_dialog_bg1_red,
	cGpF_colour_dialog_bg1_green,
	cGpF_colour_dialog_bg1_blue,
	cGpF_colour_dialog_bg2_red,
	cGpF_colour_dialog_bg2_green,
	cGpF_colour_dialog_bg2_blue,
	cGpF_colour_mark_red,
	cGpF_colour_mark_green,
	cGpF_colour_mark_blue,
	cGpF_colour_mark0_red,
	cGpF_colour_mark0_green,
	cGpF_colour_mark0_blue,
	cGpF_colour_theo_red,
	cGpF_colour_theo_green,
	cGpF_colour_theo_blue,
	cGpF_colour_auto_red,
	cGpF_colour_auto_green,
	cGpF_colour_auto_blue,
	cGpF_colour_crsr_red,
	cGpF_colour_crsr_green,
	cGpF_colour_crsr_blue,
	cGpF_colour_alert_red,
	cGpF_colour_alert_green,
	cGpF_colour_alert_blue,
	cGpF_colour_addfil_red,
	cGpF_colour_addfil_green,
	cGpF_colour_addfil_blue,
	cGpF_calib_wdw_width,
	cGpF_calib_wdw_height,
	cGpF_calib_azimuth_grid,
	cGpF_calib_slowness_grid,
	cGpF_sn_noise_start,
	cGpF_sn_noise_end,
	cGpF_sn_signal_start,
	cGpF_sn_signal_end,
	cGpF_idphases_tol_trav,
	cGpF_idphases_tol_travsurf,
	cGpF_idphases_tol_slow,
	cGpF_idphases_tol_azim,
	cGpF_axis_label_rounding,
	cGpF_last
} TGpFloatIndex;


/* char variables
 * -----------------
 */

typedef enum {
	cGpC_filter_type,
	cGpC_last
} TGpCharIndex;


/* string variables
 * -----------------
 */

typedef enum {
	cGpS_defpath_events,
	cGpS_defpath_gse,
	cGpS_defpath_ah,
	cGpS_defpath_q,
	cGpS_defpath_evid,
	cGpS_defpath_evtout,
	cGpS_defpath_data,
	cGpS_defpath_help,
	cGpS_defpath_errors,
	cGpS_defpath_userdir,
	cGpS_defpath_extprog,
	cGpS_analyst,
	cGpS_list_of_analysts,
	cGpS_texteditor,
	cGpS_refstation,
	cGpS_list_of_refstations,
	cGpS_default_filter,
	cGpS_minmax_format,
	cGpS_default_source,
	cGpS_auto_phase,
	cGpS_depth_phase_list,
	cGpS_theo_phase_list,
	cGpS_diff_phase_list,
	cGpS_event_check_proc,
	cGpS_screendump_proc,
	cGpS_evtview_proc,
	cGpS_reformat_proc,
	cGpS_final_proc,
	cGpS_motif_log,
	cGpS_ftp_address,
	cGpS_ftp_path,
	cGpS_exclusive_agency,
	cGpS_sfdb_command,
	cGpS_sfdb_exec_qual,
	cGpS_last
} TGpStringIndex;


/* boolean variables
 * -----------------
 */

typedef enum {
	cGpB_top_down_order,
	cGpB_autopick_first,
	cGpB_auto_scaling,
	cGpB_prompt_analyst,
	cGpB_reverse_xors,
	cGpB_full_phase_names,
	cGpB_own_accelerators,
	cGpB_small_menu_font,
	cGpB_recover_evid,
	cGpB_overwrite_string_lists,
	cGpB_ignore_data_lock,
	cGpB_reads_swap,
	cGpB_reads_invhdr,
	cGpB_last
} TGpBooleanIndex;


/* string list variables
 * ---------------------
 */

typedef enum {
	cGpL_station_info_file,
	cGpL_filter_lookup_table,
	cGpL_sensitivity_file,
	cGpL_defpath_filter,
	cGpL_defpath_command,
	cGpL_defpath_globals,
	cGpL_last
} TGpStringListIndex;


/* text list definition */

typedef struct {
	int      numelem; /* number of elements */
	char     **elem;  /* elements of list*/
	char     *mem;    /* memory for all elements of text list */
} TGpTextList;



/*----------------------------------------------------------------------------*/


void GpReadParfile( void );

/* Reads parameters from file
 *
 * no parameters passed
 */


/*----------------------------------------------------------------------------*/


int GpGetInt( TGpIntIndex idx );

/* Returns integer valued global
 *
 * parameters of routine
 * TGpIntIndex   idx;       input; index of variable
 *                          returns value of variable
 */


/*----------------------------------------------------------------------------*/


float GpGetFloat( TGpFloatIndex idx );

/* Returns float valued global
 *
 * parameters of routine
 * TGpFloatIndex   idx;     input; index of variable
 *                          returns value of variable
 */


/*----------------------------------------------------------------------------*/


char GpGetChar( TGpCharIndex idx );

/* Returns char valued global
 *
 * parameters of routine
 * TGpCharIndex   idx;      input; index of variable
 *                          returns value of variable
 */


/*----------------------------------------------------------------------------*/


char *GpGetString( TGpStringIndex idx );

/* Returns pointer to string valued global
 *
 * parameters of routine
 * TGpStringIndex   idx;    input; index of variable
 *                          returns value of variable
 */


/*----------------------------------------------------------------------------*/


int GpGetBoolean( TGpBooleanIndex idx );

/* Returns boolean valued global
 *
 * parameters of routine
 * TGpBooleanIndex   idx;   input; index of variable
 *                          returns value of variable
 */


/*----------------------------------------------------------------------------*/


char *GpGetStringElem( TGpStringListIndex idx, int elemnum );

/* returns string element number elemnum from string list idx
 *
 * parameters of routine
 * TGpStringListIndex idx;     input; index numer of string list
 * int                elemnum; input; number of string in list (start with 0)
 *                             returns string address or NULL
 */


/*----------------------------------------------------------------------------*/


void GpSetInt( TGpIntIndex idx, int value );

/* Sets integer valued global
 *
 * parameters of routine
 * TGpIntIndex   idx;       input; index of variable
 * int           value;     input; new value of variable
 */


/*----------------------------------------------------------------------------*/


void GpSetFloat( TGpFloatIndex idx, float value );

/* Sets float valued global
 *
 * parameters of routine
 * TGpFloatIndex   idx;     input; index of variable
 * float           value;   input; new value of variable
 */


/*----------------------------------------------------------------------------*/


void GpSetChar( TGpCharIndex idx, char value );

/* Sets char valued global
 *
 * parameters of routine
 * TGpCharIndex   idx;      input; index of variable
 * char           value;    input; new value of variable
 */


/*----------------------------------------------------------------------------*/


void GpSetString( TGpStringIndex idx, char str[], int *ok );

/* Sets value of string valued global variable
 *
 * parameters of routine
 * TGpStringIndex   idx;    input; index of variable
 * char             str[];  input; new value of variable
 * int              *ok;    output (if not NULL); 1 = ok, 0 = failure
 */


/*----------------------------------------------------------------------------*/


void GpSetBoolean( TGpBooleanIndex idx, int value );

/* Sets boolean valued global
 *
 * parameters of routine
 * TGpBooleanIndex   idx;   input; index of variable
 * int               value; input; new value of variable
 */


/*----------------------------------------------------------------------------*/


void GpReadParameter( char parname[], int maxlth, char value[], int *ok );

/* Reads parameter of name parname from parameter file.
 *
 * parameters of routine
 * char       parname[]; input, name of parameter
 * int        maxlth; input; maximum length of output string
 * char       value[]; output; value of parameter
 * int        *ok; output; 1=ok, 0=not found or string too short
 */


/*----------------------------------------------------------------------------*/


void GpParseTextList( char str[], TGpTextList *tl );

/* Parses comma separated text string and returns pointers to elements of list.
 * Returns data structure must be freed after use (GpFreeTextList).
 *
 * parameters of routine
 * char       str[];    input; input string to be parsed
 * GpTextList *tl;      output; elements of list
 */


/*----------------------------------------------------------------------------*/


void GpParseStringList( TGpStringListIndex idx, char line[] );

/* parses string list and puts result to list number idx
 *
 * parameters of routine
 * TGpStringListIndex idx;    input; string list variable index
 * char               line[]; input; line to be parsed
 */


/*----------------------------------------------------------------------------*/


void GpFreeTextList( TGpTextList *tl );

/* Frees memory of textlist
 *
 * parameters of routine
 * TGpTextList         modify; text list to be freed
 */


/*----------------------------------------------------------------------------*/


void GpDumpParams( void );

/* write all parameters out to stdout
 *
 * no parameters passed
 *
 */


/*----------------------------------------------------------------------------*/

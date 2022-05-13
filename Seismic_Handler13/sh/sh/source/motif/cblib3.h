
/* file cblib3.h
 *      ========
 *
 * version 25, 25-Jul-2006
 *
 * header file of module cblib3.c
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



/*
 * error codes
 * -----------
 */

#define CL3E_OFFSET 5800
#define CL3E_PSAVE_AGAIN    (CL3E_OFFSET+1)    /* save phase list twice */
#define CL3E_LTH_MISMATCH   (CL3E_OFFSET+2)    /* phase list length mismatch */
#define CL3E_STROVFL        (CL3E_OFFSET+3)    /* string overflow */
#define CL3E_WRITE_ERR      (CL3E_OFFSET+4)    /* write error on file */
#define CL3E_READ_ERR       (CL3E_OFFSET+5)    /* read error on file */
#define CL3E_OPEN_READ      (CL3E_OFFSET+6)    /* open error on input file */
#define CL3E_OPEN_WRITE     (CL3E_OFFSET+7)    /* open error on output file */
#define CL3E_SAVE_VERSION   (CL3E_OFFSET+8)    /* wrong version of save file */
#define CL3E_PROGRAM_BUG    (CL3E_OFFSET+9)    /* program bug */
#define CL3E_LESS_TRACES    (CL3E_OFFSET+10)   /* too less traces */
#define CL3E_NO_RELINFO     (CL3E_OFFSET+11)   /* no relative station loc. */
#define CL3E_NO_PHASE_SEL   (CL3E_OFFSET+12)   /* no theo phase selected */
#define CL3E_ENV_ERROR      (CL3E_OFFSET+13)   /* error in environment vars */
#define CL3E_CONVERT_NUM    (CL3E_OFFSET+14)   /* error converting number */
#define CL3E_NO_PHASES      (CL3E_OFFSET+15)   /* no phases found */
#define CL3E_NOENVIRONMENT  (CL3E_OFFSET+16)   /* SH environment incomplete */
#define CL3E_PLUGIN_CONTENT (CL3E_OFFSET+17)   /* error getting plugin content */
#define CL3E_NO_MORE_PLUGIN (CL3E_OFFSET+18)   /* no more plugin available */
#define CL3E_NO_NEW_PLUGIN  (CL3E_OFFSET+19)   /* all avail. plugins installed */


/*
 * constants
 * ---------
 */

#define CL3C_PLUGIN_MAXPROC 23
#define CL3C_PLUGIN_MAXMENUPROC 20
#define CL3C_PLUGIN_LOCPROC 21
#define CL3C_PLUGIN_INIPROC 22
#define CL3C_PLUGIN_ENDPROC 23


/*
 * prototypes
 * ----------
 */

/*---------------------------------------------------------------------------*/


void cl3_save_parameters( int mode, CUT_PARAMS *par, STATUS *status );

/* Saves analysis parameters and all phases to automatically generated filename
 *
 * parameters of routine
 * int        mode;        input; save mode (CUC_SAVEMODE_...)
 * CUT_PARAMS *par;        input; analysis parameters
 * STATUS     *status;     output; return status
 */


/*---------------------------------------------------------------------------*/


void cl3_restore_parameters( char file[], CUT_PARAMS *par, STATUS *status );

/* Restores analysis parameters and all phases from specified file.  In the
 * 'par'-structure the event number remains unchanged.
 *
 * parameters of routine
 * char       file[];      input; name of output file
 * CUT_PARAMS *par;        output (!); analysis parameters
 * STATUS     *status;     output; return status
 */


/*---------------------------------------------------------------------------*/


void cl3_check_filter( char filnam[], BOOLEAN rec, char prefix[],
	BOOLEAN *same_filter, STATUS *status );

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


/*---------------------------------------------------------------------------*/


void cl3_readbox_reset( Widget w[], MX_STATLIST *sl );

/* resets read box objects
 *
 * parameters of routine
 * Widget      w[];        input; widget array
 * MX_STATLIST *sl;        input; station list descriptor
 */


/*---------------------------------------------------------------------------*/


void cl3_perform_fk( Widget w[], TSyBoolean unit_in_deg, TSyBoolean backgr,
	char oname[], STATUS *status );

/* performs fk analysis
 *
 * parameters of routine
 * Widget     w[];          input; widget array
 * TSyBoolean unit_in_deg;  input; distance unit in degrees (FALSE=km)
 * TSyBoolean backgr;       input; run job in background
 * char       oname[];      input; if not "" then create only files of this name
 * STATUS     *status;      output; return status
 */


/*---------------------------------------------------------------------------*/


void cl3_fk_get_values( CUT_PARAMS *par, STATUS *status );

/* Reads azimuth and slowness values from output file of FK
 *
 * parameters of routine
 * CUT_PARAMS *par;         modify; output for azimtuh and slowness
 * STATUS     *status;      output; return status
 */


/*---------------------------------------------------------------------------*/


void cl3_preset_fk_values( Widget w[], char filter[] );

/* Preset fk box with reasonable values, depending on filter.  Values are
 * changed only if the filter changes from shortperiod to longperiod or
 * back.
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * char       filter[];   input; name of current filter
 */


/*---------------------------------------------------------------------------*/


void cl3_uppercase_names( void );

/* Changes all station names to uppercase on all traces on display
 *
 * no parameters
 */


/*---------------------------------------------------------------------------*/


void cl3_set_theo_phases( Widget w[], STATUS *status );

/* Changes set of theoretical phases computed by cu_theo_arrivals.  Uses
 * command 'shmsetup theo_phase_list'.
 *
 * parameters of routine
 * Widget     w[];         input; widget array
 * STATUS     *status;     output; return status
 */


/*--------------------------------------------------------------------------*/


void cl3_dump_parameters( CUT_PARAMSETS *parset, char infofile[],
	BOOLEAN append, BOOLEAN screen_output, BOOLEAN do_edit, STATUS *status );

/* prints out all information
 *
 * parameters of routine
 * CUT_PARAMS *parset;       input; analysis parameters
 * char       infofile[];    input; name of info file to be created
 * BOOLEAN    append;        input; append info to file / create new file
 * BOOLEAN    screen_output; input; print info to screen
 * BOOLEAN    do_edit;       input; open output file with texteditor
 * STATUS     *status;       output; return status
 */


/*--------------------------------------------------------------------------*/


void cl3_regio_localization( Widget w[], char phasename[],
	CUT_PARAMSETS *parset, STATUS *status );

/* interface to regional localization using HYPOELLIPSE
 *
 * parameters of routine
 * Widget     w[];              input; all widgets
 * char       phasename[];      input; phasename to be used (currently ignored)
 * CUT_PARAMSETS *parset;       output; localization parameters
 * STATUS     *status;          output; return status
 */


/*--------------------------------------------------------------------------*/


void cl3_locsat_interface( Widget w[], CUT_PARAMSETS *parset, STATUS *status );

/* interface to regional localization using LocSAT
 *
 * parameters of routine
 * Widget     w[];              input; all widgets
 * CUT_PARAMSETS *parset;       output; localization parameters
 * STATUS     *status;          output; return status
 */


/*---------------------------------------------------------------------------*/


void cl3_restore_from_evt( char evtfile[], CUT_PARAMS *par, long *eventid,
	STATUS *status );

/* restores event & phase parameters from evt-file
 *
 * parameters of routine
 * char       evtfile[];      input; name of evt-file
 * CUT_PARAMS *par;           output; params read in
 * long       *eventid;       output; event ID if found, ptr may be NULL
 * STATUS     *status;        output; return status
 */


/*---------------------------------------------------------------------------*/


void cl3_init_extproc_entries( Widget w[] );

/* Initializes menu entry for external processes
 *
 * parameters of routine
 * Widget     w[];      input; widget array
 */


/*---------------------------------------------------------------------------*/


void cl3_call_extproc( int num,  Widget w[], MGT_DSPCTRL *ctrl,
	CUT_PARAMSETS *parset, TPiPhase *phase, TSyStatus *status );

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


/*---------------------------------------------------------------------------*/


void cl3_request_extproc( Widget w[], TSyStatus *status );

/* Request list of available plugins
 *
 * parameters of routine
 * Widget     w[]; input; widget array
 * TSyStatus  *status; output; return status
 */


/*---------------------------------------------------------------------------*/


void cl3_add_extproc( Widget w[], TSyStatus *status );

/* Adds plugin to menu
 *
 * parameters of routine
 * Widget     w[]; input; widget array
 * TSyStatus  *status; return status
 */


/*---------------------------------------------------------------------------*/


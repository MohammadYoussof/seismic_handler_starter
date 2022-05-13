
/* file cbutil.h
 *      ========
 *
 * version 73, 7-Jun-2007
 *
 * header file of module cbutil.c
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


#include "glusrdef.h"
#include "phaseinf.h"
#include "phasemgr.h"


/* error codes */

#define CUE_OFFSET       4600
#define CUE_NOTRACES     (CUE_OFFSET+1)    /* no traces specified */
#define CUE_OPENREAD     (CUE_OFFSET+2)    /* error opening input file */
#define CUE_TOOLESS      (CUE_OFFSET+3)    /* too less traces in locate */
#define CUE_NOSTATINF    (CUE_OFFSET+4)    /* no station info available */
#define CUE_NOPHASE      (CUE_OFFSET+5)    /* phase not found */
#define CUE_NOINITIATOR  (CUE_OFFSET+6)    /* no initiator found */
#define CUE_NOZEROLINE   (CUE_OFFSET+7)    /* no zeroes found in window */
#define CUE_CANTHAPPEN   (CUE_OFFSET+8)    /* this cannot happen */
#define CUE_NOMULTIPLI   (CUE_OFFSET+9)    /* no multiplication value */
#define CUE_CNV_FLOAT    (CUE_OFFSET+10)   /* error converting float */
#define CUE_NO_TRANSF    (CUE_OFFSET+11)   /* no transfer function found */
#define CUE_ZERO_PERIOD  (CUE_OFFSET+12)   /* zero period input */
#define CUE_ZERO_TRANSF  (CUE_OFFSET+13)   /* zero transfer function */
#define CUE_STROVFL      (CUE_OFFSET+14)   /* string overflow */
#define CUE_NODISTANCE   (CUE_OFFSET+15)   /* no distance determined */
#define CUE_BEAMSTATION  (CUE_OFFSET+16)   /* beam station name found */
#define CUE_NO_REFSTAT   (CUE_OFFSET+17)   /* reference station not in list */
#define CUE_READ_INPUT   (CUE_OFFSET+18)   /* input error on file */
#define CUE_UPDATE_PHASE (CUE_OFFSET+19)   /* update phase error */
#define CUE_SAVE_FIRST   (CUE_OFFSET+20)   /* save/cancel parameters before */
#define CUE_DUMMY_PHASE  (CUE_OFFSET+21)   /* no dummy phase allowed */
#define CUE_USE_READNEW  (CUE_OFFSET+22)   /* use Read New button */
#define CUE_NO_WOODAND   (CUE_OFFSET+23)   /* need Wood-Anderson seismograms */
#define CUE_BUG          (CUE_OFFSET+24)   /* program bug */
#define CUE_NOGETGEO     (CUE_OFFSET+25)   /* get_geo input files not found */
#define CUE_TOUCH_PHASE  (CUE_OFFSET+26)   /* user should touch phase */
#define CUE_NO_TRACES    (CUE_OFFSET+27)   /* no traces found */



/* define foreign source code */
#define xxx_get_geo get_geo



/* constants */

#define CUC_MONTH_INC 1.0e6
#define CUC_MONTH_DEC -1.0e6
#define CUC_YEAR_INC 1.0e7
#define CUC_YEAR_DEC -1.0e7

#define CUC_LIST_NEXT 1
#define CUC_LIST_PREV 2

/* conversion degrees -> km */
#define CUC_DEG_TO_KM 111.19

/* time/phase selection definitions */
#define CUC_MAX_SELECTIONS 2
#define CUC_SELTYPE_NONE 0
#define CUC_SELTYPE_AMPLPER_P 1
#define CUC_SELTYPE_AMPLPER_Z 2
#define CUC_SELTYPE_AMPL_MAN 3
#define CUC_SELTYPE_PER_MAN 4
#define CUC_SELTYPE_AMPL_SURFACE 5
#define CUC_SELTYPE_MAGN_ML 6
#define CUC_SELTYPE_ONSET_ACC 7

#define CUC_SELTEXT_AMPLPER_P "Selection: Ampl&Period peak-peak"
#define CUC_SELTEXT_AMPLPER_Z "Selection: Ampl&Period zero-peak"
#define CUC_SELTEXT_AMPL_MAN "Selection: Amplitude manually"
#define CUC_SELTEXT_PER_MAN "Selection: Period manually"
#define CUC_SELTEXT_AMPL_SURFACE "Selection: Surface Ampl. auto"
#define CUC_SELTEXT_MAGN_ML "Selection: Magnitude Window"
#define CUC_SELTEXT_ONSET_ACC "Selection: Onset Accuracy"

/* multiplication factors from widget */
#define CUC_MULTIPLI_EDIT_MUL 10000.0
#define CUC_MULTIPLI_EDIT_DIV 20000.0

/* location methods */
#define CUC_LOCMETH_UNDEFINED   0
#define CUC_LOCMETH_BEAMCORR    1
#define CUC_LOCMETH_BEAMUNCORR  2
#define CUC_LOCMETH_RESIDCORR   3
#define CUC_LOCMETH_HYPO        4
#define CUC_LOCMETH_LOCSAT      5
#define CUC_LOCMETH_HYPOCENTER  6
#define CUC_LOCMETH_HYPO71      7
#define CUC_LOCMETH_HYPOSAT     8
#define CUC_LOCMETH_RELTRAV     9
#define CUC_LOCMETH_EXTERNAL   10

/* cu_localization execution flags */
#define CUF_DOLOC_SLOAZ 1
#define CUF_DOLOC_DISTANCE 2
#define CUF_DOLOC_ORIGIN 4

/* CUT_PARAMS state values */
#define CUC_PARSTATE_UNDEFINED (-1)
#define CUC_PARSTATE_INITIAL 0
#define CUC_PARSTATE_PROCESS 1
#define CUC_PARSTATE_FINAL 2
#ifdef XXX
#define CUC_PARSTATE_CANCELLED 3
#endif

/* long period phase name */
#define CUC_LP_PHASE_NAME "L"

/* distance units */
#define CUC_DIST_UNIT_DEG 1
#define CUC_DIST_UNIT_KM 2

/* depth types */
#define CUC_DEPTH_UNDEFINED 0
#define CUC_DEPTH_PRESET 1
#define CUC_DEPTH_ESTIMATED 2
#define CUC_DEPTH_FREE 3
#define CUC_DEPTH_POOR 4
#define CUC_DEPTH_LESSWELL 5
#define CUC_DEPTH_RELIABLE 6
#define CUC_DEPTH_EXTERNAL 7

/* location quality */
#define CUC_LOCQ_UNDEFINED 0
#define CUC_LOCQ_TOOWEAK 1
#define CUC_LOCQ_INCOHERENT 2
#define CUC_LOCQ_NOBEARING 3
#define CUC_LOCQ_REGION 4
#define CUC_LOCQ_RELIABLE 5

#define CUC_TYPE_OTHER 0
#define CUC_TYPE_TELE_QUAKE 1
#define CUC_TYPE_NUCLEAR 2
#define CUC_TYPE_REGIO_QUAKE 3
#define CUC_TYPE_LOCAL_QUAKE 4
#define CUC_TYPE_BLAST 5
#define CUC_TYPE_MINING 6

/* parameter save modes */
#define CUC_SAVEMODE_INC   0
#define CUC_SAVEMODE_A     1
#define CUC_SAVEMODE_B     2
#define CUC_SAVEMODE_C     3
#define CUC_SAVEMODE_EVENT 4

/* number of parameter sets */
#define CUC_MAXPARSET 3

/* number of AutoDRM request buttons */
#define CUC_ADRM_NUMBER 15

/* region table numbers */
#define CUC_REGTABLE_FLINNENG 1
#define CUC_REGTABLE_GERGEO 2
#define CUC_REGTABLE_GERSEIS 3

/* phase accuracy */
#define CUC_ACCURACY_NONE 1
#define CUC_ACCURACY_QUERY 2
#define CUC_ACCURACY_DISPLAY 3

/* event flags */
#define CUC_F_EVENT_CALIB     0x01
#define CUC_F_EVENT_IGNORE    0x02
#define CUC_F_EVENT_TELEX_ALL 0x04

/* execution flags */
#define CUC_F_EXEC_CALIB      0x01
#define CUC_F_EXEC_RESIDCORR  0x02

/* trace flags */
#define CUC_F_TRC_ALERT       0x01
#define CUC_F_TRC_FORCE_F     0x02
#define CUC_F_TRC_FORCE_T     0x04

/* some string lengths */
#define CUC_LTH_VELMOD 40
#define CUC_LTH_LOCADDPAR 80
#define CUC_LTH_MOMTEN 80
#define CUC_LTH_MOMTEN_DESCR 80
#define CUC_LTH_FPS_ANGLES 80
#define CUC_LTH_FPS_DESCR 80
#define CUC_LTH_MU_DESCR 40



/* types */

#define CUT_TRACE void

typedef struct {
	float     lat;                        /* error in latitude (km or deg) */
	float     lon;                        /* error in longitude (km or deg) */
	float     dep;                        /* depth error (km) */
	float     orig;                       /* error in origin time (sec) */
	float     smajor;                     /* semi major axis (km or deg) */
	float     sminor;                     /* semi minor axis (km or deg) */
	float     majstrike;                  /* major axis strike (deg) */
	int       dist_unit;                  /* unit of distances (km or deg) */
	float     azim_max_gap;               /* maximum azimuthal gap (deg) */
	float     resid_rms;                  /* RMS of residuals (sec) */
} CUT_PARERRS;

typedef struct {
	float     distance;                   /* distance in degress */
	int       dist_unit;                  /* distance unit */
	float     b_slowness;                 /* beam slowness in sec/deg */
	float     b_slowness_err;             /* beam slowness deviation */
	float     b_azimuth;                  /* beam azimuth in deg */
	float     b_azimuth_err;              /* beam azimuth deviation */
	float     l_slowness;                 /* (to) epicenter slowness */
	float     l_azimuth;                  /* (to) epicenter (back) azimuth */
	float     depth;                      /* depth in km */
	int       depth_type;                 /* type of depth determination */
	char      origin[BC_TIMELTH+1];       /* origin time */
	float     source_lat;                 /* latitude of source */
	float     source_lon;                 /* longitude of source */
	int       source_type;                /* event type */
	STATLOC   ref_lat;                    /* location of reference station */
	STATLOC   ref_lon;                    /* location of reference station */
	STATLOC   center_lat;                 /* location of center of array */
	STATLOC   center_lon;                 /* location of center of array */
	int       table_number;               /* number of region table */
	int       reg_id;                     /* region number of above table*/
	char      regname[BC_LINELTH+1];      /* name of region */
	int       loc_meth;                   /* location method */
	int       loc_quality;                /* quality of location */
	char      loc_addpar[CUC_LTH_LOCADDPAR+1]; /* add. params for location */
	char      velmod[CUC_LTH_VELMOD+1];   /* velocity model */
	int       stations_used;              /* number of stations used for loc. */
	char      phase[cPiMaxPhaseLth+1];    /* phase (for slowness, azimuth) */
	char      *comment;                   /* comment on event */
	char      mu_descr[CUC_LTH_MU_DESCR+1]; /* user magnitude description */
	char      source[BC_SHORTSTRLTH+1];   /* source of info */
	float     cornerfreq;                 /* corner frequency of spectrum */
	float     lowfreqlevel;               /* low frequency level */
	char      momten[CUC_LTH_MOMTEN+1];   /* moment tensor elements */
	char      momten_descr[CUC_LTH_MOMTEN_DESCR+1]; /* descr. of moment tensor */
	char      fps_angles[CUC_LTH_FPS_ANGLES+1]; /* fault plane solution */
	char      fps_descr[CUC_LTH_FPS_DESCR+1]; /* descr. of fault plane params */
	float     m0;                         /* scalar moment */
	BOOLEAN   soft_change;                /* parameter change by software */
	int       flags;                      /* event flags */
	CUT_PARERRS err;                      /* errors of values */
	TPiSlowBox  slowbox;                  /* slowness box */
} CUT_PARAMS;

typedef struct {
	int       state;                      /* state: process, final, cancelled */
	long      evid;                       /* event ID */
	int       parno;                      /* current parameter set */
	CUT_PARAMS par[CUC_MAXPARSET];        /* parameter sets */
} CUT_PARAMSETS;

typedef struct {
	int       waiting;         /* total number of selections */
	int       processed;       /* selections already processed */
	int       initiator;       /* initiator ID */
	char      infotext[BC_LINELTH+1];      /* info output on screen */
	TPiPhase  select[CUC_MAX_SELECTIONS];  /* selections */
	TPmTrace  *phtrc[CUC_MAX_SELECTIONS];  /* phase pointers */
	void      *addparam;       /* pointer to additional parameter(s) */
} CUT_SELECTIONS;



/* macros */

#define cu_quality2weight(q) (4 - ((q)-1)/2)
#define cu_german_event(a,b) \
	((a) >= 47.0 && (a) <= 55.0 && (b) >= 5.0 && (b) <= 15.0)



/*---------------------------------------------------------------------------*/


void cu_reset_phase( TPiPhase *phase );

/* resets phase info to default values
 *
 * parameters of routine
 * TPiPhase   *phase;      output; structure to reset
 */


/*---------------------------------------------------------------------------*/


void cu_reset_parameters( CUT_PARAMS *par );

/* resets parameters to default values
 *
 * parameters of routine
 * CUT_PARAMS    *par;     parameters to be reset
 */


/*--------------------------------------------------------------------------*/


void cu_reset_paramsets( CUT_PARAMSETS *parset );

/* resets all parameter sets
 *
 * parameters of routine
 * CUT_PARAMSETS *parset;        output; parameter to be reset
 */


/*--------------------------------------------------------------------------*/


void cu_set_read_time( Widget date, Widget time, float incr, STATUS *status );

/* increments read time by "incr" seconds
 *
 * parameters of routine
 * Widget     date, time;    input; Widget ID's for date and time text
 * float      incr;          input; time increment in seconds
 * STATUS     *status;       output; return status
 */


/*-----------------------------------------------------------------------*/


void cu_set_read_time_file( Widget w[], int mode, char autoevt[],
	STATUS *status );

/* reads next/previous time from file
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * int        mode;       input; search mode
 * char       autoevt[];  output; name of evt-file
 * STATUS     *status;    output; return status
 */


/*--------------------------------------------------------------------------*/


void cu_current_time( char str[] );

/* returns current time.  Minimum length of str is 21
 *
 * parameter of routine
 * char      str[];      output; time string
 */


/*--------------------------------------------------------------------------*/


void cu_accept_phase( Widget wm, Widget ws, TPiPhase *phase, TPmTrace *phtrc,
	TSyBoolean acc, TSyStatus *status );

/* accepts a phase from dialog box
 *
 * parameters of routine
 * Widget     wm;      input; main window
 * Widget     ws;      input; single trace window
 * TPiPhase   *phase;  input; pointer to phase info
 * TPmTrace   *phtrc;  input; pointer to trace
 * TSyBoolean acc;     input; show accuracies of phases
 * TSyStatus  *status; output; returns status if not NULL
 */


/*--------------------------------------------------------------------------*/


void cu_phase_box_defaults( Widget w[], TPiPhase *phase );

/* sets button states of phase box
 *
 * parameters of routine
 * Widget     w[];        input; widget ID of phase box elements
 * TPiPhase   *phase;     input; phase info
 */


/*--------------------------------------------------------------------------*/


void cu_theo_arrivals( Widget wm, Widget ws, CUT_PARAMS *par, STATUS *status );

/* marks theoretical arrivals on all traces on display.  Phase names are
 * taken from global parameter shv_global.theo_phase_list
 *
 * parameters of routine
 * Widget     wm;               input; widget ID of main window
 * Widget     ws;               input; widget ID of single trace window
 * CUT_PARAMS *par;             input; analysis parameters
 * STATUS     *status;          output; return status
 */


/*--------------------------------------------------------------------------*/


void cu_delete_all_phases( Widget wm, Widget ws, char name[], BOOLEAN allmem );

/* All phases with name "name" are deleted.  If name=="--theo--", all
 * theoretical phases will be deleted
 *
 * parameters of routine
 * Widget     wm;       input; widget ID of main drawing area
 * Widget     ws;       input; widget ID of single draw window
 * char       name;     input; name of phases to be deleted
 * BOOLEAN    allmem;   input; all phases in memory (TRUE) or on display only
 */


/*--------------------------------------------------------------------------*/


void cu_localization( char phasename[], int do_flags, CUT_PARAMS *par,
	STATUS *status );

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
 * STATUS     *status;      output; return status
 */


/*--------------------------------------------------------------------------*/


void cu_set_param_values( Widget w[], CUT_PARAMS *par, int parno );

/* sets values of parameter block to dialog box
 *
 * parameters of routine
 * Widget     w[];       input; widget array of dialogbox elements
 * CUT_PARAMS *par;      input; parametere values
 * int        parno;     input; number of parameter set
 */


/*--------------------------------------------------------------------------*/


void cu_get_param_values( Widget w[], CUT_PARAMS *par );

/* sets values of parameter block to dialog box
 *
 * parameters of routine
 * Widget     w[];         input; widget array of dialogbox elements
 * CUT_PARAMS *par;        output; parameter values
 */


/*--------------------------------------------------------------------------*/


void cu_print_param_values( CUT_PARAMS *par );

/* prints out parameters
 *
 * parameters of routine
 * CUT_PARAMS *par;        input; analysis parameters
 */


/*--------------------------------------------------------------------------*/


void cu_get_display_control( Widget w[], MGT_DSPCTRL *ctrl );

/* puts display controls values from dialog box to control structure
 *
 * parameters of routine
 * Widget        w[];     input; Widget array of dialog box elements
 * MGT_DSPCTRL   *ctrl;   output; values retrieved
 */


/*--------------------------------------------------------------------------*/


void cu_alert_message( Widget w, char text [] );

/* puts alert box on screen
 *
 * parameters of routine
 * Widget     w;         input; alert box widget ID
 * char       text[];    input; alert message
 */


/*--------------------------------------------------------------------------*/


void cu_alert( STATUS status );

/* puts alert message on screen
 *
 * parameters of routine
 * STATUS     status;   input; status code
 */


/*--------------------------------------------------------------------------*/


void cu_set_alert_widget( Widget w );

/* sets alert widget
 *
 * parameters of routine
 * Widget     w;     input; widget ID of alert box
 */


/*--------------------------------------------------------------------------*/


void cu_print_phase( TPiPhase *phase );

/* prints out phase information
 *
 * parameters of routine
 * TPiPhase      *phase;      input; phase info
 */


/*--------------------------------------------------------------------------*/


void cu_set_multiplication_widget( Widget box, Widget label, Widget edit );

/* sets widget ID's of multiplication box
 *
 * parameters of routine
 * Widget     box;     input; widget ID of whole box
 * Widget     label;   input; widget ID of title label
 * Widget     edit;    input; widget ID of value editable text
 */


/*--------------------------------------------------------------------------*/


void cu_multiplication_dialog( TPmTrace *phtrc, TPiPhase *phase );

/* let the user multiply the period of phase
 *
 * parameters of routine
 * TPmTrace   *phtrace  input; phase pointer
 * TPiPhase   *phase;   modify; period to multiplicate and amplitudes to update
 */


/*--------------------------------------------------------------------------*/


void cu_do_multiplication( float fac, STATUS *status );

/* performs multiplication prepared by cu_multiplication_dialog
 *
 * parameters of routine
 * float      fac;         input; factor
 * STATUS     *status;     output; return status
 */


/*--------------------------------------------------------------------------*/


void cu_get_tf_name( char station[], int maxlth, char filter[], STATUS *status);

/* returns name of transfer function derived from station name
 *
 * parameters of routine
 * char       station[];        input; station name
 * int        maxlth;           input; maximum length of output string
 * char       filter[];         output; derived filter name
 * STATUS     *status;          output; return status
 */


/*--------------------------------------------------------------------------*/


void cu_ampl_restitution( void *trcptr, float ampl, float period,
	char kind[], float *rest_ampl, char filter[], STATUS *status );

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
 * STATUS     *status;         output; return status
 *                             returns restituted amplitude
 */


/*--------------------------------------------------------------------------*/


void cu_update_phase( char f_station[], char f_comp, TPiPhase *ref_phase,
	TSyStatus *status );

/* Updates all phases in memory matching station & component of given
 * phase
 *
 * parameters of routine
 * char       f_station[];  input; station to search for
 * char       f_comp;       input; component to find (if=BLANK take all)
 * TPiPhase   *ref_phase;   input; phase info
 * TSyStatus  *status;      output; return status
 */


/*--------------------------------------------------------------------------*/


void cu_insert_phase( char f_station[], char f_comp, TPiPhase *ins_phase,
	TSyStatus *status );

/* Inserts given phase on all traces matching station & component
 *
 * parameters of routine
 * char       f_station[];  input; station to search for
 * char       f_comp;       input; component to find (if=BLANK take all)
 * TPiPhase   *ins_phase;   input; phase info
 * TSyStatus  *status;      output; return status
 */


/*--------------------------------------------------------------------------*/


void cu_fix_phase_info( TPiPhase *ref_phase, TSyStatus *status );

/* Updates all phases in memory with name "ref_phase->name" to values of
 * phase dialog box.
 *
 * parameters of routine
 * TPiPhase   *ref_phase;   input; phase info
 * TSyStatus  *status;      output; return status
 */


/*--------------------------------------------------------------------------*/


TPiPhase *cu_find_first_phase( char f_station[], char f_comp, char f_name[],
	TSyStatus *status );

/* finds first phase on all traces in memory that matches station, component
 * and phase name
 *
 * parameters of routine
 * char       f_station[];       input; station name
 * char       f_comp;            input; component
 * char       f_name[];          input; phase name
 * TSyStatus  *status;           output; return status
 *                               returns pointer to phase found or NULL
 */


/*--------------------------------------------------------------------------*/


void cu_print_info( char str[] );

/* prints out informational message
 *
 * parameters of routine
 * char       str[];       input; message to print
 */


/*--------------------------------------------------------------------------*/


void cu_set_string( Widget w, char str[] );

/* sets string in text widget str.  Checks whether str is empty, in this case
 * a single blank is inserted because of bug in Motif software
 *
 * parameters of routine
 * Widget     w;         input; widget ID
 * char       str[];     input; text to be set
 */


/*--------------------------------------------------------------------------*/


char *cu_get_string( Widget w );

/* gets string from text widget str.  Checks whether string in text widget is
 * a single blank, in this case a pointer to an empty string is returned
 * because of bug in Motif software
 *
 * parameters of routine
 * Widget     w;         input; widget ID
 *                       returns pointer to static string
 */


/*---------------------------------------------------------------------------*/


void cu_reference_location( STATLOC *lat, STATLOC *lon, float *xrel,
	float *yrel, int *arrcode, TSyStatus *status );

/* returns location of reference point
 *
 * parameters of routine
 * STATLOC    *lat, *lon;        output; reference location (if not NULL)
 * float      *xrel, *yrel;      output; relative location in km (if not NULL)
 * int        *arrcode;          output; array code (if not NULL)
 * TSyStatus  *status;           output; return status
 */


/*--------------------------------------------------------------------------*/


void cu_delete_trace( TPmTrace *trc );

/* deletes trace from memory
 *
 * parameters of routine
 * void       *trc;       input; pointer to trace
 */


/*--------------------------------------------------------------------------*/


void cu_get_stream_string( TPmTrace *trc, char stream[], TSyStatus *status );

/* Returns stream string of specified trace.  If status != NULL then
 * error codes are returned otherwise 'X'-characters are copied to the
 * output string.  Minimum length of output string is cBcShortStrLth.
 *
 * parameters of routine
 * TPmTrace   *trc;         input; pointer to trace
 * char       stream[];     output; stream string (e.g. BFO-BH-Z)
 * TSyStatus  *status;      output; return status
 */


/*--------------------------------------------------------------------------*/


void cu_trace_check( int ptrop );

/* Loops all traces in list (specified by ptrop) and prints station names
 * and components
 *
 * parameters of routine
 * int        ptrop;       input; pointer name
 */


/*---------------------------------------------------------------------------*/


void cu_lookup_filter( char station[], char ttime[], char tfname[],
	int *trcflags, TSyStatus *status );

/* Tries to find transfer function name for given station.  If not found
 * station name is returned in tfname.  'station' and 'tfname' may be the
 * same variable.
 *
 * parameters of routine
 * char       station[];   input; station name
 * char       ttime[];     input; time of trace
 * char       tfname[];    output; filter name
 * int        *trcflags;   output; trace flags to be set on created traces
 * TSyStatus  *status;     output; return status
 */


/*--------------------------------------------------------------------------*/


void cu_next_read_dialog( Widget w[] );

/* Displays next station list in dialog box
 *
 * parameters of routine
 * Widget        w[]; input widget array
 */


/*--------------------------------------------------------------------------*/


void cu_set_exec_flag( int flag, TSyBoolean value );

/* Sets/Clears execution flags
 *
 * parameters of routine
 * int        flag;       input; which flag to set/clear
 * TSyBoolean val;        input; new value of flag
 */


/*--------------------------------------------------------------------------*/


TSyBoolean cu_get_exec_flag( int flag );

/* Returns flag status
 *
 * parameters of routine
 * int        flag;      input; flag to return status
 */


/*--------------------------------------------------------------------------*/


void cu_clear_exec_flags( void );

/* Clears all execution flags
 *
 * no parameters
 */


/*--------------------------------------------------------------------------*/


void cu_rename_phase( Widget w[] );

/* renames phase from or to unknown phase '???'
 *
 * parameters of routine
 * Widget     w[];     input widget array
 */


/*--------------------------------------------------------------------------*/

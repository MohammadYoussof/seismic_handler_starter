
/* file cblib.h
 *      =======
 *
 * version 21, 16-Oct-2006
 *
 * header file of module cblib.c
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


#ifndef __CBLIB
#define __CBLIB


/* error codes */
#define CLE_OFFSET         4700
#define CLE_NODEPTHPHASE   (CLE_OFFSET+1)   /* no depth phases found */
#define CLE_ILLPATHMODE    (CLE_OFFSET+2)   /* illegal path mode */
#define CLE_NODRAGWDW      (CLE_OFFSET+3)   /* no drag window selected */
#define CLE_ZEROWDW        (CLE_OFFSET+4)   /* zero window specified */
#define CLE_DOUBLEAUTO     (CLE_OFFSET+5)   /* reference trace has already b. */
#define CLE_STROVFL        (CLE_OFFSET+6)   /* string overflow */
#define CLE_READFLOAT      (CLE_OFFSET+7)   /* couldn't read floating number */
#define CLE_READINT        (CLE_OFFSET+8)   /* couldn't read integer number */
#define CLE_NOVALUE        (CLE_OFFSET+9)   /* no value in dialog box */
#define CLE_NOTRACES       (CLE_OFFSET+10)  /* no traces on screen */
#define CLE_PHASEROOT      (CLE_OFFSET+11)  /* already phases in memory */
#define CLE_NOEXTREMUM     (CLE_OFFSET+12)  /* no extremum found in window */
#define CLE_ILLEGALDIST    (CLE_OFFSET+13)  /* illegal distance */
#define CLE_FILENOTFOUND   (CLE_OFFSET+14)  /* file not found */
#define CLE_NOPHASE        (CLE_OFFSET+15)  /* no phase found */


/* constants */
#define CLC_PATH_FILTER 0
#define CLC_PATH_EVENTS 1
#define CLC_PATH_GSE 2
#define CLC_PATH_GSE2 3
#define CLC_PATH_AH 4
#define CLC_PATH_Q 5
#define CLC_PATH_RFMT 6
#define CLC_PATH_SAVE 7
#define CLC_PATH_EVT 8
#define CLC_PATH_SFD 9
#define CLC_PATH_NUMBER 10

#define CLC_CREFIL_NONE 0
#define CLC_CREFIL_BUT_BP 1
#define CLC_CREFIL_BUT_LP 2
#define CLC_CREFIL_BUT_HP 3

#define CLC_BUTPAR_LO_UP 1
#define CLC_BUTPAR_LO_DOWN 2
#define CLC_BUTPAR_HI_UP 3
#define CLC_BUTPAR_HI_DOWN 4
#define CLC_BUTPAR_ORDER_UP 5
#define CLC_BUTPAR_ORDER_DOWN 6

#define CLC_OS_RELEASE_FILE "os_release.txt"



/* prototypes */


/*---------------------------------------------------------------------------*/


void cl_fit_depth( float distance, float *depth, float *depth_err,
	int *depth_type, STATUS *status );

/* fits depth from specified depth phases
 *
 * parameters of routine
 * float      distance;    input; distance in deg
 * float      *depth;      output; depth in km
 * float      *depth_err;  output; depth error in km
 * int        *depth_type; output; depth type (CUC_DEPTH_...)
 * STATUS     *status;     output; return status
 */


/*---------------------------------------------------------------------------*/


void cl_file_select_init( Widget w, unsigned mode, STATUS *status );

/* initializes FileSelectorDialog
 *
 * parameters of routine
 * Widget     w;       input; Widget ID of FileSelectorBox
 * unsigned   mode;    input; kind of file to return
 * STATUS     *status; output; return status
 */


/*---------------------------------------------------------------------------*/


void cl_file_select_callback( Widget w[],
	XmFileSelectionBoxCallbackStruct *dat, int maxlth, char fname[], int *mode );

/* puts selected filename to appropriate place
 *
 * parameters of routine
 * Widget     w[];     input; widget array of all widgets
 * XmFileSelectionBoxCallbackStruct *dat;  input; return values of callback
 * int        maxlth;  input; maximum length of output filename
 * char       fname[]; output; filename selected
 * int        *mode;   output; which kind of file
 */


/*---------------------------------------------------------------------------*/


void cl_auto_beam_pick( Widget wm, Widget ws, char phasename[],
	BOOLEAN delete, STATUS *status );

/* performs automatic beam picking
 *
 * parameters of routine
 * Widget     wm, ws;      input; widget ID's of main and single window
 * char       phase[];     input; phase name
 * BOOLEAN    delete;      input; delete existing phases before
 * STATUS     *status;     output; return status
 */


/*---------------------------------------------------------------------------*/



void cl_correlation_pick(  Widget wm, Widget ws, char phasename[],
	TSyStatus *status );

/* Makes picks by crosscorrelation
 *
 * parameters of routine
 * Widget     wm, ws;      input; widget ID's of main and single window
 * char       phasename[]; input; phase name
 * TSyStatus  *status;     output; return status
 */


/*---------------------------------------------------------------------------*/


TPmTrace *cl_find_station_trc( char station[], char comp );

/* Returns trace pointer of first trace found with given station and
 * component or returns NULL.
 *
 * parameters of routine
 * char       station[];    input; station name to search
 * char       comp;         input; component


/*---------------------------------------------------------------------------*/


void cl_manage_filter_bounds( Widget w[], int mode, STATUS *status );

/* changes filter bounds after activation of arrow buttons
 *
 * parameters of routine
 * Widget     w[];      input; widget array
 * int        mode;     input; which button selected
 * STATUS     *status;  output; return status
 */


/*---------------------------------------------------------------------------*/


void cl_create_filter( Widget w[], int mode, STATUS *status );

/* creates filter file and puts name into filter text widget
 *
 * parameters of routine
 * Widget     w[];        input; widget ID
 * int        mode;       input; kind of filter
 * STATUS     *status;    output; return status
 */


/*---------------------------------------------------------------------------*/


void cl_calc_align_times( char phasename[], int maxlth, char trcstr[],
	STATUS *status );

/* computes shift times for traces to align given phase "phasename".  Results
 * are put in info entry OPINFO
 *
 * parameters of routine
 * char       phasename[];     input; name of phase to be aligned
 * int        maxlth;          input; maximum length of trcstr string
 * char       trcstr[];        output; trace list string (where phase is found)
 * STATUS     *status;         output; return status
 */


/*---------------------------------------------------------------------------*/


void cl_filter_prefix( void *trc, int maxlth, char prefix[] );

/* generates prefix for filter filename from sample distance delta of given
 * trace on display.
 *
 * parameters of routine
 * void       *trc;         input; trace pointer or NULL for first trace.
 * int        maxlth;       input; maximum length of output string
 * char       prefix[];     output; created file prefix
 */


/*---------------------------------------------------------------------------*/


char *cl_unique_name( void );

/* Returns pointer to unique name each time it is called.  Produced filename
 * has no extension.
 */



/*---------------------------------------------------------------------------*/


BOOLEAN cl_is_solaris_2( void );

/* Returns TRUE if OS is Solaris 2
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void cl_set_simfilter_active( Widget w[], TSyBoolean onoff );

/* Sets sensitivity of simulation filter buttons on or off
 *
 * parameters of routine
 * Widget        w[];         input; widget array
 * TSyBoolean    onoff;       input; TRUE=activate, FALSE=deactivate
 */


/*---------------------------------------------------------------------------*/


void cl_read_autofilters( void );

/* Reads autofilter entries from parameter file
 *
 * no parameters
 */


/*---------------------------------------------------------------------------*/


void cl_set_autofilters( void );

/* sets autofilter names to filter entry of traces
 *
 * no parameters
 */


/*---------------------------------------------------------------------------*/



#endif /* __CBLIB */


/* file cblib2.h
 *      =======
 *
 * version 22, 22-May-97
 *
 * header file of module cblib2.c
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


#ifndef __CBLIB2
#define __CBLIB2


/* error codes */
#define CB2E_OFFSET           4500
#define CB2E_NO_LOCATION      (CB2E_OFFSET+1)  /* no epicenter location */
#define CB2E_NO_MAGNITUDE     (CB2E_OFFSET+2)  /* no magnitude determined */
#define CB2E_PROGRAM_BUG      (CB2E_OFFSET+3)  /* program bug */
#define CB2E_WRONG_COMP       (CB2E_OFFSET+4)  /* wrong component selected */
#define CB2E_ML_NEED_2        (CB2E_OFFSET+5)  /* need N&E component for ml */
#define CB2E_DIFF_SMP         (CB2E_OFFSET+6)  /* different sample rates */
#define CB2E_ZERO_WDW         (CB2E_OFFSET+7)  /* zero window selected */
#define CB2E_ILL_VALUE        (CB2E_OFFSET+8)  /* illegal value */
#define CB2E_OPEN_WRITE       (CB2E_OFFSET+9)  /* error opening output file */
#define CB2E_OPEN_READ        (CB2E_OFFSET+10) /* error opening input file */
#define CB2E_AMPLTIME_DIFF    (CB2E_OFFSET+11) /* amplitude too far from phase*/
#define CB2E_READ_ERR         (CB2E_OFFSET+12) /* read error on file */
#define CB2E_WRITE_ERR        (CB2E_OFFSET+13) /* write error on file */
#define CB2E_STROVFL          (CB2E_OFFSET+14) /* string overflow */
#define CB2E_ILL_DIFFTIME     (CB2E_OFFSET+15) /* illegal phase difference */
#define CB2E_NO_PHASES        (CB2E_OFFSET+16) /* no phases found */
#define CB2E_SAVE_VERSION     (CB2E_OFFSET+17) /* wrong version(can't restore)*/
#define CB2E_NOMAGPHASE       (CB2E_OFFSET+18) /* no phase for magnitude found*/
#define CB2E_NO_TRACES        (CB2E_OFFSET+19) /* no traces on display */
#define CB2E_NO_REFSTATION    (CB2E_OFFSET+20) /* reference station not found */
#define CB2E_L_ILLIDX         (CB2E_OFFSET+21) /* illegal index in L selection*/
#define CB2E_L_NOTFOUND       (CB2E_OFFSET+22) /* no L waveform found */




/*---------------------------------------------------------------------------*/


void cl2_magnitude( int type, CUT_PARAMS *par, TSyStatus *status );

/* Determines magnitude mb and MS.  "type" specifies type of magnitude
 * to be determined.  Searches all P-phases selected on Z-component
 * and computes magnitude.  Result is put to P-phase info "magnitude".
 *
 * parameters of routine
 * int        type;            input; type of magnitude
 * CUT_PARAMS *par;            input; analysis parameters
 * TSyStatus  *status;         output; return status
 */


/*---------------------------------------------------------------------------*/


void cl2_magnitude_ml_selection( CUT_PARAMS *par, CUT_SELECTIONS *sel,
	TSyStatus *status );

/* Reads selected time window for ml determination, finds correspondend
 * E & N components, searches for maximum amplitudes and computes ml.
 *
 * parameters of routine
 * CUT_PARAMS     *par;           input; analysis parameters
 * CUT_SELECTIONS *sel;           input; window selection
 * TSyStatus      *status;        output; return status
 */


/*---------------------------------------------------------------------------*/


void cl2_magnitude_ml_selection( CUT_PARAMS *par, CUT_SELECTIONS *sel,
	TSyStatus *status );

/* Reads selected time window for ml determination, finds correspondend
 * E & N components, searches for maximum amplitudes and computes ml.
 *
 * parameters of routine
 * CUT_PARAMS      *par;           input; analysis parameters
 * CUT_SELECTIONS  *sel;           input; window selection
 * TSyStatus       *status;        output; return status
 */


/*--------------------------------------------------------------------------*/


void cl2_process_selection( Widget w[], MGT_DSPCTRL *ctrl,
	int wdwno, CUT_PARAMS *par, CUT_SELECTIONS *sel, TPiPhase *curr,
	TPmTrace *phtrc, TSyStatus *status );

/* puts selection into list and calls routine which initiated the selections
 * if all selections are already made
 *
 * parameters of routine
 * Widget        w[];        input; widget array
 * MGT_DSPCTRL   *ctrl;      input; display control
 * int           wdwno;      input; window number
 * CUT_PARAMS    *par;       input; analysis parameters
 * CUT_SELECTIONS *sel;      modify; selections
 * TPiPhase      *curr;      input; current phase
 * TPmTrace      *phtrc;     input; phase trace
 * TSyStatus     *status;    output; return status
 */


/*---------------------------------------------------------------------------*/


void cl2_extern_theo( Widget wm, Widget ws, CUT_PARAMS *par, BOOLEAN acc,
	TSyStatus *status );

/* Calls external program to compute theoretical travel times and puts the
 * phases to the traces on screen.
 *
 * parameters of routine
 * Widget     wm, ws;      input; widget ID's of main and single trace window
 * CUT_PARAMS *par;        input; analysis parameters
 * BOOLEAN    acc;         input; show accuracy on phases
 * TSyStatus  *status;     output; return status
 */


/*---------------------------------------------------------------------------*/


void cl2_modify_comment( char **comment, TSyStatus *status );

/* Let the user change comment text '*comment'.  If *comment==NULL, current
 * comment text is empty.  The input comment memory is freed and new
 * memory allocated for new comment.
 *
 * parameters of routine
 * char       *comment;      modify; pointer to comment text
 */


/*---------------------------------------------------------------------------*/


void *cl2_station_on_display( char station[], char comp );

/* returns pointer to reference station or NULL if reference station is not
 * found
 *
 * parameters of routine
 * char       station[];     input; name of reference station
 * char       comp;          input; component
 */


/*---------------------------------------------------------------------------*/


void cl2_display_busy( Widget w[], BOOLEAN on );

/* If program is busy (on=TRUE) menus are disabled and "computing ..."-Text
 * is written in window
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * BOOLEAN    on;         input; switch busy on or off
 */


/*---------------------------------------------------------------------------*/


void cl2_param_box_defaults( Widget w[], CUT_PARAMS *par );

/* sets values of parameter dialog box
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * CUT_PARAMS *par;       input; parameter values
 */


/*---------------------------------------------------------------------------*/



#endif /* __CBLIB2 */



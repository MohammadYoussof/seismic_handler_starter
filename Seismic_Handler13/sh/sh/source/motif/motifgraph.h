
/* file motifgraph.h
 *      ============
 *
 * version 30, 11-Jan-2007
 *
 * header file of module motifgraph.c
 * K. Stammler, 18-Feb-93
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

#include "phaseinf.h"
#include "phasemgr.h"



/* constants */

#define MGC_DRAG_START 1
#define MGC_DRAG_RUBBER_BOX 2
#define MGC_DRAG_MOVE_BOX 3
#define MGC_DRAG_CONTINUE 4
#define MGC_DRAG_END 5
#define MGC_DRAG_CLEAR 6
#define MGC_DRAG_REFRESH 7

/* X cursors */
#define MGC_XCRSR_NORMAL 0
#define MGC_XCRSR_CROSS 1
#define MGC_XCRSR_BUSY 2
#define MGC_XCRSR_WAVE 3
#define MGC_XCRSR_LEFT 4
#define MGC_XCRSR_RIGHT 5
#define MGC_XCRSR_MAX 5

/* own cursors */
#define MGC_CRSR_NOCURSOR 1
#define MGC_CRSR_CROSSHAIR 2
#define MGC_CRSR_WAVEFORM 3
#define MGC_CRSR_WAVEFORM_NEG 4
#define MGC_CRSR_WAVEFORM_HILB 5
#define MGC_CRSR_WAVEFORM_NEGHILB 6
#define MGC_CRSR_CONTINUE 10
#define MGC_CRSR_CLEAR 11
#define MGC_CRSR_REFRESH 12
#define MGC_CRSR_OFF 13
#define MGC_CRSR_FIRST MGC_CRSR_NOCURSOR
#define MGC_CRSR_LAST MGC_CRSR_WAVEFORM_HILB

#define MGC_PHASE_START 1
#define MGC_PHASE_CLEAR 2
#define MGC_PHASE_REFRESH 3

#define MGC_PHASE_KIND_THEO 1
#define MGC_PHASE_KIND_AUTO 2
#define MGC_PHASE_KIND_MANUALLY 3
#define MGC_PHASE_KIND_MANUALLY_0 4

#define MGC_WDW_MAIN 1
#define MGC_WDW_SINGLE 2
#define MGC_WDW_LAST 3

#define MGC_NORM_AF 1
#define MGC_NORM_SF 2

#define MGC_RUBBER_END 0
#define MGC_RUBBER_CONT_R 1
#define MGC_RUBBER_CONT_L 2
#define MGC_RUBBER_START 3



/* types */

typedef struct {
	float       zoom;        /* zoom factor */
	int         norm;        /* normalization type */
	BOOLEAN     show_phase_acc; /* show phase accuracies */
} MGT_DSPCTRL;

typedef struct {
	BOOLEAN     do_interpolation;    /* do graphic interpolation */
	float       frac_1;              /* shift of first sample (in samples)*/
	float       frac_e;              /* shift of last sample (in samples) */
	float       ampl_1;              /* amplitude of first sample */
	float       ampl_e;              /* amplitude of last sample */
} MGT_GRAPHIC_INTERP;


/*----------------------------------------------------------------------------*/


void mg_init_tracedisplay( Widget w, int r, int l, int t, int b );

/* initializes display parameters.  Input r,l,t,b valid only if >=0.
 *
 * parameters of routine
 * Widget     w;           input; Widget of trace display (DrawingArea)
 * int        r, l, t, b;  input; right, left, top, bottom margin in pixel
 */


/*----------------------------------------------------------------------------*/


void mg_set_reverse_xors( BOOLEAN val );

/* on some screens XOR GC's should use reversed background/foreground
 *
 * parameters of routine
 * BOOLEAN    val;       input; reverse XOR's or not
 */


/*----------------------------------------------------------------------------*/


void mg_show_3_traces( BOOLEAN val );

/* Switch on/off 3 trace display in single trace window
 *
 * parameters of routine
 * BOOLEAN    val;       input; value of switch
 */


/*----------------------------------------------------------------------------*/


void mg_tracedisplay( Widget w, MGT_DSPCTRL *ctrl, STATUS *status );

/* draws traces on DrawingArea "w"
 *
 * parameters of routine
 * Widget       w;       input; DrawingArea Widget
 * MGT_DSPCTRL  *ctrl;   input; display control parameters
 * STATUS       *status; output; return status
 */


/*----------------------------------------------------------------------------*/


void mg_plot_phases( Widget wm, BOOLEAN acc, STATUS *status );

/* marks all phases
 *
 * parameters of routine
 * Widget     wm;      input; widget ID of main window
 * BOOLEAN    acc;     input; show accuracies
 * STATUS     *status; output; return status
 */


/*----------------------------------------------------------------------------*/


void mg_get_time_and_trace( int x, int y, float *time, int *trcno,
	BOOLEAN *ok );

/* returns time and trace selected by pixels (x,y)
 *
 * parameters of routine
 * int        x, y;        input; pixel coordinates
 * float      *time;       output; time in sec
 * int        *trcno;      output; trace number
 * BOOLEAN    *ok;         output; validitation
 */


/*----------------------------------------------------------------------------*/


void mg_get_time_and_trace_single( int x, int y, float *time,
	int *trcno, BOOLEAN *ok );

/* returns time and trace selected by pixels (x,y) in single trace window
 *
 * parameters of routine
 * int        x, y;        input; pixel coordinates
 * float      *time;       output; time in sec
 * int        *trcno;      output; trace number
 * BOOLEAN    *ok;         output; validitation
 */


/*----------------------------------------------------------------------------*/


void mg_get_pixelpos( int trcno, float time, int *x, int *y, BOOLEAN *ok );

/* returns pixel position of trace "trcno" at time "time" (y-coordinate is
 * given at zero line
 *
 * parameters of routine
 * int        trcno;     input; trace number on display
 * float      time;      input; time position
 * int        *x, *y;    output; pixel position
 * BOOLEAN    *ok;       output; validitation
 */


/*----------------------------------------------------------------------------*/


void mg_get_drag_window( float *start, float *end, BOOLEAN *set );

/* returns current drag window in sec
 *
 * parameters of routine
 * float      *start;      output; start of time window
 * float      *end;        output; end of time window
 * BOOLEAN    *set;        output; time window set
 */


/*----------------------------------------------------------------------------*/


void mg_get_drag_trace( int *trcno, BOOLEAN *set );

/* returns trace number which is selected by drag window
 *
 * parameters of routine
 * int        *trcno;  output; trace number on display (valid only if *set=TRUE)
 * BOOLEAN    *set;    output; drag window is set
 */


/*----------------------------------------------------------------------------*/


void mg_mark_trace( Widget wm, Widget ws, int trcno, float time, char label[],
	int kind, float left_acc, float right_acc );

/* marks trace "trcno" at time "time"
 *
 * parameters of routine
 * Widget     wm;         input; widget ID of main window
 * Widget     ws;         input; widget ID of single trace window
 * int        trcno;      input; trace number on display
 * float      time;       input; time position
 * char       label[];    input; label text
 * int        kind;       input; phase kind
 * float      left_acc;   input; left accuracy in sec
 * float      right_acc;  input; right accuracy in sec
 */


/*----------------------------------------------------------------------------*/


void mg_plot_bold( Widget wm, Widget ws, MGT_DSPCTRL *ctrl, int wdwno,
	int trcno, long startidx, long wdwlth, MGT_GRAPHIC_INTERP *ip,
	STATUS *status );

/* plots part of a trace bold in drawing area
 *
 * parameters of routine
 * Widget        wm;       input; widget ID of main drawing area
 * Widget        ws;       input; widget ID of single trace drawing area
 * MGT_DSPCTRL   *ctrl;    input; display control
 * int           wdwno;    input window number
 * int           trcno;    input; trace number
 * long          startidx; input; start index of trace
 * long          wdwlth;   input; window length in samples
 * MGT_GRAPHIC_INTERP *ip; input; graphic interpolation
 * STATUS        *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void mg_do_drag( Widget w, int mode, int x, int y );

/* draws dragging lines and boxes
 *
 * parameters of routine
 * Widget     w;         input; widget ID
 * int        mode;      input; which item
 * int        x, y;      input; new position
 */


/*----------------------------------------------------------------------------*/


void mg_plot_single( Widget w, int trcno, float t_start, float t_end,
	BOOLEAN acc, int pixoff, STATUS *status );

/* plots a single trace into the single trace window
 *
 * parameters of routine
 * Widget     w;         input; widget ID of single trace window
 * int        trcno;     input; trace number
 * float      t_start;   input; window start (sec)
 * float      t_end;     input; window end (sec)
 * BOOLEAN    acc;       input; show accuracies on phases
 * int        pixoff;    input; pixel offset; if 0 then main trace is plotted
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void mg_plot_drag_window( Widget w, BOOLEAN acc, STATUS *status );

/* plots time window selected by drag box into single trace window
 *
 * parameters of routine
 * Widget     w;        input; widget ID of single trace window
 * BOOLEAN    acc;      input; show accuracies on phases
 * STATUS     *status;  output; return status */


/*----------------------------------------------------------------------------*/


void mg_draw_cursor( Widget w, int wdwno, MGT_DSPCTRL *ctrl, int mode,
	int x, int y, BOOLEAN *ok );

/* plots graphic cursor
 *
 * parameters of routine
 * Widget     w;       input; widget ID of window
 * int        wdwno;   input; window number
 * MGT_DSPCTRL *ctrl;  input; display control
 * int        mode;    input; cursor mode
 * int        x, y;    input; cursor position
 * BOOLEAN    *ok;     output; if != NULL, cursor display succeeded ?
 */


/*----------------------------------------------------------------------------*/


void mg_do_phase( Widget w, int wdwno, int mode, int x, int y, char onset[],
	int *no, TPiPhase *close );

/* manages selecting, moving and deleting of phases
 *
 * parameters of routine
 * Widget     w;      input; widget of trace window
 * int        wdwno;  input; window number
 * int        mode;   input; selection mode
 * int        x, y;   input; position of pointer
 * char       onset[];output; returns onset time if != NULL
 * int        *no;    output; returns trace number if != NULL
 * TPiPhase   *close; output; returns close phase if != NULL
 */


/*----------------------------------------------------------------------------*/


void mg_mark_one_phase( Widget wm, Widget ws, TPiPhase *phase, TPmTrace *phtrc,
	TSyBoolean acc );

/* marks a phase on a specified trace
 *
 * parameters of routine
 * Widget     wm;        input; widget of main window
 * Widget     ws;        input; widget of single trace window
 * TPiPhase   *phase;    input; pointer to phase info
 * TPmtrace   *phtrc;    input; pointer to phase trace
 * TSyBoolean acc;       input; draw accuracies
 */


/*----------------------------------------------------------------------------*/


BOOLEAN mg_toogle_trace_selection( Widget w, int x, int y );

/* selects trace if mouse position on button event is in left margin of main
 * window
 *
 * parameters of routine
 * Widget     w;         input; drawing area widget
 * int        x, y;      input; position of mouse
 *                       returns TRUE if a trace was selected
 */


/*----------------------------------------------------------------------------*/


BOOLEAN mg_select_trace( Widget w, int x, int y );

/* selects trace if mouse position on button event is in left margin of main
 * window
 *
 * parameters of routine
 * Widget     w;         input; drawing area widget
 * int        x, y;      input; position of mouse
 *                       returns TRUE if a trace was selected
 */


/*----------------------------------------------------------------------------*/


BOOLEAN mg_deselect_trace( Widget w, int x, int y );

/* deselects trace if mouse position on button event is in left margin of main
 * window
 *
 * parameters of routine
 * Widget     w;         input; drawing area widget
 * int        x, y;      input; position of mouse
 *                       returns TRUE if a trace was selected
 */


/*----------------------------------------------------------------------------*/


int mg_dsptrcs( void );

/* returns number of traces on display
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void *mg_trcptr( int trcno );

/* returns TRACE pointer of trace number "trcno"
 *
 * parameters of routine
 * int        trcno;       input; trace number on display
 *                         returns pointer to trace info
 */


/*----------------------------------------------------------------------------*/


int mg_trcnum( void *trcptr );

/* returns trace number on display from pointer to trace.  Returns 0 if trace
 * is not on display
 *
 * parameters of routine
 * void       *trcptr;        input; pointer to trace
 */


/*----------------------------------------------------------------------------*/


void mg_print_time( Widget w, int wdwno, int x, int y );

/* prints absolute time in window
 *
 * parameters of routine
 * Widget     w;         input; Widget ID of window where to write time string
 * int        wdwno;     input; number of window which caused MotionNotify
 * int        x, y;      input; cursor position
 */


/*----------------------------------------------------------------------------*/


void mg_print_busy( Widget w, BOOLEAN busy );

/* prints busy message on window
 *
 * parameters of routine
 * Widget     w;         input; widget of DrawingArea
 * BOOLEAN    busy;      input; busy flag on/off
 */


/*----------------------------------------------------------------------------*/



void mg_print_filter( Widget w, char filter[] );

/* prints busy message on window
 *
 * parameters of routine
 * Widget     w;         input; widget of DrawingArea
 * char       filter[];  input; name of filter
 */


/*----------------------------------------------------------------------------*/


void mg_print_status( Widget w, char text[], BOOLEAN on_off );

/* prints/clears status message on window
 *
 * parameters of routine
 * Widget     w;         input; widget of DrawingArea
 * char       text[];    input; text to print
 * BOOLEAN    on_off;    input; print/clear
 */


/*----------------------------------------------------------------------------*/



void mg_print_lastcmd( Widget w, char lastcmd[] );

/* prints last command in window
 *
 * parameters of routine
 * Widget     w;          input; widget of DrawingArea
 * char       lastcmd[];  input; name of filter
 */


/*----------------------------------------------------------------------------*/



void mg_print_sortinfo( Widget w, char sortinfo[] );

/* prints sort info in window
 *
 * parameters of routine
 * Widget     w;          input; widget of DrawingArea
 * char       sortinfo[]; input; sort info
 */


/*----------------------------------------------------------------------------*/


void mg_rubber_line( Widget w, int wdwno, int mode, int x, int y );

/* Draws rubber line
 *
 * parameters of routine
 * Widget     w;             input; widget ID of drawing area
 * int        wdwno;         input; number of window
 * int        mode;          input; mode (START,CONT,END)
 * int        x, y;          input; mouse position
 */


/*----------------------------------------------------------------------------*/


void mg_clear_selections( Widget w );

/* Clears all selections on display
 *
 * parameters of routine
 * Widget     w;        input; display widget
 */


/*----------------------------------------------------------------------------*/


void mg_selected_wave( float **wavptr, int *lth, char stime[], float *dt,
	char station[], char chan[], char *comp );

/* Returns pointer to selected waveform or NULL if nothing selected.
 * Memory is allocated for the samples.  It must be freed after use with 'free'.
 *
 * parameters of routine
 * float      **wavptr;    output; pointer to waveform or NULL
 * int        *lth;        output; length of trace in samples
 * char       stime[];     output; start time of waveform
 * float      *dt;         output; sample distance of trace
 * char       station[];   output; station name (min length cBcShortStrLth)
 * char       chan[];      output; channel name (min length 3)
 * char       *comp;       output; component (min length 1)
 */


/*----------------------------------------------------------------------------*/


void mg_get_windowsize( Widget w, int *width, int *height );

/* returns size of window in pixels
 *
 * parameters of routine
 * Widget     w;         input; widget ID
 * int        *width;    output; window width
 * int        *height;   output; window height
 */


/*----------------------------------------------------------------------------*/


void mg_set_cursor_widgets( Widget w[], int num );

/* Sets widgets of drawing areas where cursor form may be changed
 *
 * parameters of routine
 * Widget        w[];       input; drawing are widgets
 * int           num;       input; length of above array (<= MAXCRSRDSP)
 */


/*----------------------------------------------------------------------------*/


void mg_add_cursor_widget( Widget w );

/* Adds widget to list of drawing areas where cursor form may be changed
 *
 * parameters of routine
 * Widget        w;       input; drawing area widgets
 * int           num;       input; length of above array (<= MAXCRSRDSP)
 */


/*----------------------------------------------------------------------------*/



void mg_set_cursor( int cursor );

/* Sets cursor form in all windows passed in mg_set_cursor_widgets.
 *
 * parameters of routine
 * Display    *dsp;        input; pointer to display
 * Window     w;           input; window of cursor
 * int        cursor;      input; corsor ID
 */


/*----------------------------------------------------------------------------*/


void mg_find_3_traces( int tmain, int *ta, int *tb );

/* Finds associated traces to tmain (traces of same station and different
 * components.  Returns -1 if no traces found
 *
 * parameters of routine
 * int        tmain;        input; number of main trace
 * int        *ta, *tb;     output; associated traces
 */



/*----------------------------------------------------------------------------*/


void mg_current_time_window( char stime[], float *width );

/* returns current time window of main display (using first trace of display)
 *
 * parameters of routine
 * char       stime[];     output; absolute start time of window
 * float      *width;      output; width of display in s
 */


/*----------------------------------------------------------------------------*/


void mg_make_alert_trace( Widget w, void *trcptr, TSyStatus *status );

/* makes alert trace
 *
 * parameters of routine
 * Widget     w;        input; widget of drawing area
 * void       *trcptr;  input; pointer to trace
 * TSyStatus  *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void mg_get_last_drag_box( TSyBoolean *valid,
	int *x1, int *y1, int *x2, int *y2 );

/* returns position and size of last dragbox
 *
 * parameters of routine
 * TSyBoolean    *valid;    output; drag box visible? if FALSE x,y's are void
 * int           *x1, *y1;  output; lower left corner
 * int           *x2, *y2;  output; upper right corner
 */


/*----------------------------------------------------------------------------*/

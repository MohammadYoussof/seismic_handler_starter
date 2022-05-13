
/* file cblib4.h
 *      ========
 *
 * version 25, 22-May-2006
 *
 * prototypes of module cblib4.c
 * K. Stammler, 5-Aug-95
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

#define CL4E_OFFSET 5900
#define CL4E_NO_TRACES      (CL4E_OFFSET+1)    /* no traces on display */
#define CL4E_ILL_BEAM_TRACE (CL4E_OFFSET+2)    /* beam/align not allowed */
#define CL4E_ILL_DIFFTIME   (CL4E_OFFSET+3)    /* negative phase difference */
#define CL4E_NO_PHASES      (CL4E_OFFSET+4)    /* no phase pairs found */
#define CL4E_NO_REFERENCE   (CL4E_OFFSET+5)    /* no reference location */
#define CL4E_GET_GEO_ERR    (CL4E_OFFSET+6)    /* error in get_geo */
#define CL4E_STROVFL        (CL4E_OFFSET+7)    /* string overflow */
#define CL4E_OPENWRITE      (CL4E_OFFSET+8)    /* error opening output file */
#define CL4E_OPENREAD       (CL4E_OFFSET+9)    /* error opening input file */
#define CL4E_ILLREFNUMBER   (CL4E_OFFSET+10)   /* illegal ref station number */
#define CL4E_FLT_MISMATCH   (CL4E_OFFSET+11)   /* filter mismatch */
#define CL4E_DUMMY_PHASE    (CL4E_OFFSET+12)   /* no dummy phase allowed */
#define CL4E_ILLIDX         (CL4E_OFFSET+13)   /* illegal sample index */
#define CL4E_ILLORDER       (CL4E_OFFSET+14)   /* illegal order of markers */
#define CL4E_NODRAGWDW      (CL4E_OFFSET+15)   /* no drag window */
#define CL4E_NO_FILTER_REQ  (CL4E_OFFSET+16)   /* unfiltered traces required */
#define CL4E_NO_BEAM        (CL4E_OFFSET+17)   /* no beam trace found */
#define CL4E_TOO_LESS_TRCS  (CL4E_OFFSET+18)   /* too less traces */
#define CL4E_NO_ENV         (CL4E_OFFSET+19)   /* no environment defined */
#define CL4E_ILL_ADRM_ID    (CL4E_OFFSET+20)   /* illegal DRM id */
#define CL4E_NO_ADRM_SETUP  (CL4E_OFFSET+21)   /* no private AutoDRM setup */
#define CL4E_ILL_READLTH    (CL4E_OFFSET+22)   /* illegal read length */



/*---------------------------------------------------------------------------*/


void cl4_help( Widget w, char title[] );

/* Puts help text from file to help window.  The help files are found in the
 * help directory 'shd_help' with the extension '.SHM'.
 *
 * parameters of routine
 * Widget     w;           input; widget ID of help window
 * char       title[];     input; help title
 */


/*---------------------------------------------------------------------------*/


void cl4_read_special_commands( Widget w[] );

/* Reads in names of special commands from file $SH_INPUTS/special_commands.txt
 *
 * parameters of routine
 * Widget     w[];         input; widget array
 */


/*---------------------------------------------------------------------------*/


void cl4_do_vespa( Widget w[], MGT_DSPCTRL *ctrl, float azim, BOOLEAN on_off,
	STATUS *status );

/* Shows vespagram fo array data
 *
 * parameters of routine
 * Widget     w[];         input; widget array
 * MGT_DSPCTRL *ctrl;      input; display control
 * float      azim;        input; azimuth for vespagram
 * BOOLEAN    on_off;      input; switch vespa on or off
 * STATUS     *status;     output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_phase_difference( char phaselist[], float depth, float *distance,
	STATUS *status );

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


/*---------------------------------------------------------------------------*/


void cl4_loc_from_dist( CUT_PARAMS *par, STATUS *status );

/* Computes new location (latitude & longitude) from location of reference
 * station and distance and azimuth
 *
 * parameters of routine
 * CUT_PARAMS    *par;      modify; analysis parameters
 * STATUS        *status;   output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_select_all_possible_stations( Widget w[], MX_CMD_READG *rg,
	STATUS *status );

/* Selects all possible stations in read dialog box for specified stream
 * and time span.
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * MX_CMD_READG *rg;      input; read parameters
 * STATUS     *status;    output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_sort_traces( Widget w, MGT_DSPCTRL *ctrl, char infoname[],
	CUT_PARAMS *par, STATUS *status );

/* Sorts traces on display by given information (mainly distance & azimuth)
 *
 * parameters of routine
 * Widget     w;                 input; widget ID of drawing area
 * MGT_DSPCTRL *ctrl;            input; display control
 * char       infoname[];        input; name of information
 * CUT_PARAMS *par;              input; analysis parameters
 * STATUS     *status;           output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_request_data( Widget w[], MX_CMD_READG *rg, STATUS *status );

/* Requests data from GRSN stations
 *
 * parameters of routine
 * Widget     w[];               input; widget array
 * MX_CMD_READG *rg;             input; read parameters
 * STATUS     *status;           output; return status
 */


/*---------------------------------------------------------------------------*/


BOOLEAN cl4_station_read_in( char station[], char comp );

/* checks whether station and component is already read in
 *
 * parameters of routine
 * char       station[];       input; station to be checked
 * char       comp;            input; component to be checked
 *                             returns TRUE if station found in memory
 */


/*---------------------------------------------------------------------------*/


void cl4_manage_sfdfile( Widget w[], MX_CMD_READG *readg );

/* Manages sfdfiles used in reads commands
 *
 * parameters of routine
 * Widget     w[];          input; widget array
 * MX_CMD_READG *readg;     modify; readg-parameters (esp. sfdfile)
 */


/*---------------------------------------------------------------------------*/


void cl4_del_magnitude( Widget wd, int magntype );

/* loops all phases in memory and deletes all magnitudes of given type
 *
 * parameters of routine
 * Widget     wd;                  input; main drawing area
 * int        magntype;            input; type if magnitude (PIC_MAGN_...)
 */


/*---------------------------------------------------------------------------*/


void cl4_read_refstation_list( Widget w[], STATUS *status );

/* reads list of reference stations and puts names to option buttons
 *
 * parameters of routine
 * Widget     w[];           input; list of all widgets
 * STATUS     *status;       output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_read_refstation( int number, char station[], STATUS *status );

/* returns name of reference station number 'number'
 *
 * parameters of routine
 * int        number;           input; number of station
 * char       station[];        output; name of station
 * STATUS     *status;          output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_signoise_set_windows( Widget w[], char phasename[], char currfilter[],
	TSyStatus *status );

/* sets windows for determination of signal/noise ratios
 *
 * parameters of routine
 * Widget     w[];               input; widget array
 * char       phasename[];       input; name of phase to determine S/N
 * char       currfilter[];      input; current filter applied to traces
 * TSyStatus  *status;           output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_signoise_values( char phasename[], TSyStatus *status );

/* scans all traces on display and determines S/N values where it finds
 * the pseudo-phases b1, b2, b3 and b4.
 *
 * parameters of routine
 * char       phasename[];   input; name of phase to determine S/N
 * TSyStatus  *status;       output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_delete_signoise_values( char phasename[], TSyStatus *status );

/* scans all traces on display and deletes S/N values on the 'phasename'-phases
 *
 * parameters of routine
 * char       phasename[];   input; name of phase to determine S/N
 * TSyStatus  *status;       output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_delete_traces( void );

/* Deletes traces of specified (selected) station and component
 * (all traces in memory)
 *
 * parameters of routine
 */


/*---------------------------------------------------------------------------*/


void cl4_vespa_export( Widget w[], STATUS *status );

/* Writes vespagram traces to ASCII file and call display programs
 *
 * parameters of routine
 * Widget     w[];          input; widget array
 * STATUS     *status;      output; return status
 */


/*---------------------------------------------------------------------------*/


BOOLEAN cl4_station_is_selected( char station[] );

/* Returns whether station is selected at least one time
 *
 * parameters of routine
 * char       station[];       input; station name
 */


/*---------------------------------------------------------------------------*/


void cl4_onset_pick( Widget w[], MGT_DSPCTRL *ctrl, char phasename[],
	TSyBoolean delete, TSyStatus *status );

/* Calls automatic onset picker
 *
 * parameters of routine
 * Widget     w[];            input; widget array
 * MGT_DSPCTRL *ctrl;         input; display control
 * char       phasename[];    input; name of autopick-phase
 * TSyBoolean delete;         input; delete existing phases 'phasename'
 * TSyStatus  *status         output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_fix_beam( Widget w[], MGT_DSPCTRL *ctrl, char filter[],
	TSyStatus *status );

/* Throws out all traces but the beam trace and renames this to B-<refstation>.
 *
 * parameters of routine
 * Widget     w[];            input; widget array
 * MGT_DSPCTRL *ctrl;         input; display control
 * char       filter[];       input; name of current filter
 * TSyStatus  *status         output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_resid_corr( TSyBoolean setval, float slowness, float azimuth,
	TSyStatus *status );

/* computes or deletes residual correction for all phases picked
 *
 * parameters of routine
 * TSyBoolean setval;         input; TRUE=compute values, FALSE=delete values
 * float      slowness;       input; slowness in s/deg
 * float      azimuth;        input; back-azimuth in deg
 * TSyStatus  status;         output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_adrm_request( Widget w[], char stime[], float seclth,
	char datafile[], TSyStatus *status );

/* Manages requests to AutoDRMs
 *
 * parameters of routine
 * Widget     w[];       input; Widget array
 * char       stime[];   input; start time
 * float      seclth;    input; time window in sec
 * char       datafile[];output; data file created
 * TSyStatus  *status;   output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_adrm_configure( Widget w[], TSyStatus *status );

/* Configures Autodrm selection box by calling editor on the local setup file
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * TSyStatus  *status;    output; return status
 */


/*---------------------------------------------------------------------------*/


void cl4_init_adrm_buttons( Widget w[] );

/* Initializes AutoDRM buttons
 *
 * parameters of routine
 * Widget     w[];   input; widget array
 */


/*---------------------------------------------------------------------------*/

/* file mfexec.h
 *      ========
 *
 * version 29, 21-Nov-2007
 *
 * execution of SH commands (header file)
 * K. Stammler, 25-Feb-93
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


/* error codes */
#define MXE_OFFSET       4400
#define MXE_PROG_BUG     (MXE_OFFSET+1)      /* program bug */
#define MXE_NOT_IMPL     (MXE_OFFSET+2)      /* not implemented */
#define MXE_NO_GSE_MATCH (MXE_OFFSET+3)      /* no GSE files found */
#define MXE_STROVFL      (MXE_OFFSET+4)      /* string overflow */
#define MXE_ILL_ANALYST  (MXE_OFFSET+5)      /* illegal analyst */
#define MXE_NO_TRACES    (MXE_OFFSET+6)      /* no traces found */
#define MXE_ILL_EXTROUT  (MXE_OFFSET+7)      /* illegal par to ext. rout. */
#define MXE_OPENREAD     (MXE_OFFSET+8)      /* error opening input file */
#define MXE_MANY_SELECT  (MXE_OFFSET+9)      /* too many traces selected */
#define MXE_NONE_SELECT  (MXE_OFFSET+10)     /* no traces selected */
#define MXE_NO_PHASES    (MXE_OFFSET+11)     /* no phases found */
#define MXE_NO_MLENTRY   (MXE_OFFSET+12)     /* no ml found on trace */
#define MXE_NOSTATIONS   (MXE_OFFSET+13)     /* no stations selected */
#define MXE_NOCHANNELS   (MXE_OFFSET+14)     /* no channel selected */
#define MXE_NOT_FOUND    (MXE_OFFSET+15)     /* not found */
#define MXE_ILL_SUBSET   (MXE_OFFSET+16)     /* undefined station subset */


/* constants */

/* read formats */
#define MXC_FORMAT_SEED 1
#define MXC_FORMAT_GSE 2
#define MXC_FORMAT_GSE2 3
#define MXC_FORMAT_AH 4
#define MXC_FORMAT_Q 5

#define MXC_CHANLTH 2
#define MXC_MAXCHAN 4

#define MXC_STATLIST_LTH 30
#define MXC_STATSTR_LTH 9

#define MXC_COMP_STRLTH 10


typedef struct {
	char     code[MXC_STATLIST_LTH][MXC_STATSTR_LTH+1];  /* station codes */
	char     set1code[MXC_STATSTR_LTH+1];      /* name of first subset */
	char     set2code[MXC_STATSTR_LTH+1];      /* name of second subset */
	int      set1start;                        /* first station of subset 1 */
	int      set1end;                          /* last station of subset 1 */
	int      set2start;                        /* first station of subset 2 */
	int      set2end;                          /* last station of subset 2 */
	char     channel[MXC_MAXCHAN][MXC_CHANLTH+1]; /* channel codes */
} MX_STATLIST;


/* readg */
typedef struct {
	int      format;                 /* format type: MXC_FORMAT_... */
   BOOLEAN  keep;                   /* keep traces */
   char     device[BC_FILELTH+1];   /* directory */
	/* following for MXC_FORMAT_SEED */
   char     start[BC_LINELTH+1];    /* start time */
   float    seclth;                 /* length in seconds */
   long     stations;               /* station list bits */
   char     comp[MXC_COMP_STRLTH+1];/* component chars */
	char     chanstr[MXC_MAXCHAN][MXC_CHANLTH+1]; /* channel strings */
	int      channum;                /* number of channels used */
	BOOLEAN  use_reads;              /* use reads command */
	BOOLEAN  use_readk;              /* use readk command on VAX */
	BOOLEAN  reads_invhdr;           /* use qualifier INVHDR on reads */
	char     sfdfile[BC_FILELTH+1];  /* name of sfd-file used */
	/* following for other formats */
	char     filename[BC_FILELTH+1]; /* file names (wildcards possible) */
	/* station list of dialog box */
	MX_STATLIST sl;                  /* station list of dialog box */
} MX_CMD_READG;

typedef struct {
	char   name[BC_FILELTH+1];    /* name of filter */
	float  autocut;               /* auto cut window */
} MX_CMD_FILTER;



/*--------------------------------------------------------------------------*/


void mx_exec_sh( Widget w, MGT_DSPCTRL *ctrl, char line[] );

/* executes command line
 *
 * parameters of routine
 * Widget        w;          input; widget of drawing area
 * MGT_DSPCTRL   *ctrl;      input; display control parameters
 * char          line[];     input; command line
 */


/*--------------------------------------------------------------------------*/


void mx_readg( MX_CMD_READG *par, Widget wm, Widget ws, MGT_DSPCTRL *ctrl,
	BOOLEAN keep_phases, char autoevt[], STATUS *status );

/* executes readg command
 *
 * parameters of routine
 * XM_CMD_READG  *par;        input; command parameters
 * Widget        wm;          input; main DrawingArea Widget
 * Widget        ws;          input; single trace window
 * MGT_DSPCTRL   *ctrl;       input; display control parameters
 * BOOLEAN       keep_phases; input; keep phases in memory
 * char          *autoevt     input; name of evt file for retrieving stat. names
 * STATUS        *status;     output; return status
 */


/*--------------------------------------------------------------------------*/


void mx_filter( MX_CMD_FILTER *filter, Widget w, MGT_DSPCTRL *ctrl,
	STATUS *status );

/* performs filtering
 *
 * parameters of routine
 * MX_CMD_FILTER *filter;       input; filter command parameters
 * Widget        w;             input; DrawingArea Widget
 * MGT_DSPCTRL   *ctrl;         input; display control parameters
 * STATUS        *status;       output; return status
 */


/*--------------------------------------------------------------------------*/


void mx_stw( Widget wm, Widget ws, MGT_DSPCTRL *ctrl );

/* executes stw command
 *
 * parameters of routine
 * Widget        wm;        input; widget ID of main DrawingArea
 * Widget        ws;        input; widget ID of single trace DrawingArea
 * MGT_DSPCTRL   *ctrl;     input; display control parameters
 */


/*---------------------------------------------------------------------------*/


void mx_handle_beam( Widget wm, Widget ws, BOOLEAN do_beam, MGT_DSPCTRL *ctrl,
	CUT_PARAMS *par, char alignphase[], STATUS *status );

/* creates or deletes beam trace (toggle routine)
 *
 * parameters of routine
 * Widget     wm;            input; Widget ID of main window
 * Widget     ws;            input; Widget ID of single trace window
 * BOOLEAN    do_beam;       input; do beam (not align)
 * MGT_DSPCTRL *ctrl;        input; display control
 * CUT_PARAMS *par;          input; analysis parameters
 * char       alignphase[];  input; name of phase to be aligned
 * STATUS     *status;       output; return status
 */


/*---------------------------------------------------------------------------*/


void mx_rotation( Widget wm, MGT_DSPCTRL *ctrl, float azimuth, float lat,
	float lon, STATUS *status );

/* Performs rotation of traces with given azimuth of, if 0.0, computing
 * azimuth by given location
 *
 * parameters of routine
 * Widget     wm;        input; widget ID of main window
 * MGT_DSPCTRL *ctrl;    input; display control
 * float      azimuth;   input; rotation angle
 * float      lat, lon;  input; event location
 * STATUS     *status;   output; return status
 */


/*---------------------------------------------------------------------------*/


BOOLEAN mx_need_rotation( void );

/* returns whether rotation mode is switched on
 *
 * no input parameters
 */


/*---------------------------------------------------------------------------*/


void mx_trclist_command( Widget w, Widget ws, MGT_DSPCTRL *ctrl, char cmd[],
	STATUS *status );

/* executes SH command on trace list
 *
 * parameters of routine
 * Widget     wm, ws;      input; widget ID of drawing areas
 * MGT_DSPCTRL *ctrl;      input; display control
 * char       cmd[];       input; command to be executed
 * STATUS     *status;     output; return status
 */


/*----------------------------------------------------------------------------*/


void mx_trclist_refml( STATUS *status );

/* stores reference ml in parameter list
 *
 * parameters of routine
 * STATUS     *status;      output; return status 
 */


/*----------------------------------------------------------------------------*/


void mx_analyst_name( Widget w[], MGT_DSPCTRL *ctrl, STATUS *status );

/* Reads analyst's initials from dialog box checks it and closes dialog
 * box if evereything is ok.
 *
 * parameters of routine
 * Widget     w[];        input; widget array
 * STATUS     *status;    output; return status
 */


/*--------------------------------------------------------------------------*/


void mx_filter_box_defaults( Widget w[], MX_CMD_FILTER *fil );

/* resets filter box
 *
 * parameters of routine
 * Widget     w[];       input; widget array
 * MX_CMD_FILTER *fil;   input; filter parameters
 */


/*----------------------------------------------------------------------------*/


void mx_sh_external_routine( char cmd[], STATUS *status );

/* external routine for SH
 *
 * parameters of routine
 * char       cmd[];         input; switch to subcommands
 * STATUS     *status;       output; return status
 */


/*----------------------------------------------------------------------------*/


void mx_clear_rotation( void );

/* resets roatation state
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void mx_get_chanlist( Widget w[], MX_CMD_READG *rg );


/* reads list of channels from dialog box into channel array
 *
 * parameters of routine
 * Widget        w[];          input; widget array
 * MX_CMD_READG  *rg;          output; channel array (chanstr)
 */


/*----------------------------------------------------------------------------*/


void mx_name_read_buttons( Widget w[], int dialbox_num, TSyStatus *status );

/* reads in station list from parameter file.  Original from mxh_read_statlist.
 *
 * parameters of routine
 * Widget      w[];            input; widget array (widgets will be modified)
 * int         dialbox_num;    input; number of dialog box to be modified
 * MX_STATLIST *sl;            output; station list for dialog box
 * TSyStatus   *status;        output; return status
 */


/*--------------------------------------------------------------------------*/

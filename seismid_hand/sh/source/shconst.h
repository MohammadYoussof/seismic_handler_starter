
/* file SHCONST.H
 *      =========
 *
 * version 80, 29-Mar-2007
 *
 * constants of seismhandler program
 * K. Stammler, 22-MAY-91
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


#ifndef __SHCONST
#define __SHCONST

#define SHC_VERSION "5.0c 04-Jan-2007"
	/* version number */
#define TW 1
	/* text window */
#define GW 2
	/* graphics window */
#ifdef BC_SUN
#define SHC_FILE_PREFIX "SH_"
#else
#define SHC_FILE_PREFIX "SH$"
#endif
	/* file prefix of all SeismicHandler files */
#define SHC_FILE_PREFIX_LENGTH 3
	/* length of file prefix */
#define SHC_DE_TEXT ".STX"
	/* default extension of echo files */
#define SHC_DE_HELP ".HLP"
	/* default extension of help files */
#define SHC_DE_SAVE ".SSV"
	/* default extension of save files */
#define SHC_DE_CMD ".SHC"
	/* default command file extension */
#define SHC_DE_VEL ".VEL"
	/* default velocity file extension */
#define SHC_DE_ERROR ".MSG"
   /* default extension of error message files */
#define SHC_DE_RFILT ".FLR"
	/* default extension for recursive filter files */
#define SHC_DE_FFILT ".FLF"
	/* default extension for FFT filter files */
#define SHC_DE_DFILT ".FLT"
	/* default extension for tabulated filter files */
#define SHC_DE_CURVE ".CRV"
	/* default extension of curve files */
#define SHC_CHWRITE '&'
	/* write access character */
#define SHC_SYMGLB 1
	/* global symbol set number */
#define SHC_SYMLOC 0
	/* local symbol set number */
#define SHC_ILISTLTH 650
	/* max length of integer lists */
#define SHC_CRSR '*'
	/* cursor input */
#define SHC_ABORTCH 'X'
	/* cursor input aborted */
#define SHC_EXITCH 'E'
	/* cursor input end */
#define SHC_TIMEDEFAULT "1-JUL-1970_12:00:00.00"
	/* trace start time default */
#define SHC_PI 3.14159265358979323846
	/* pi */
#define SHC_EPSILON 1.0e-25
	/* epsilon value */
#define SHC_RAD_TO_DEG (180.0/SHC_PI)
	/* conversion factor from radians to degrees */
#define SHC_DEG_TO_KM 111.19
	/* conversion factor degrees to km */
/* #define SHC_CURRDSP -1 */
	/* take trace from current display list */
/* #define SHC_HIDDENLIST -2 */
	/* take trace from hidden list */
#define SHC_MAXWDW 7
	/* maximum number of windows (same as in gcusrdef.h) */
#define SHC_WDWMASK 7
	/* window mask */
#define SHC_MAXDLN (SHC_MAXWDW+3)
	/* maximum display list number */
#define SHC_CARRIAGE_RETURN (char)13
	/* carriage return */
#define SHC_CHATTXT "   !i:"
	/* intro string for CHATTY output line */
#define SHC_STATNAMELTH 11
	/* maximum length of station name */

/* style numbers */
#define SHC_TITLESTYLE 8
	/* title style */
#define SHC_TRCINFOSTYLE 9
	/* style of trace info text */
#define SHC_ZEROTRCSTYLE 8
	/* style of zero trace lines */
#define SHC_MARKSTYLE 6
	/* style of markers */
#define SHC_PMSTYLE 5
	/* particle motion style */
#define SHC_TIMEAXISSTYLE 7
	/* style block of time axis */

/* #define SHC_HCFILE_WITH_RANDOM */

/* normalization types */
#define SHC_N_SF 0
#define SHC_N_SW 1
#define SHC_N_AF 2
#define SHC_N_AW 3
#define SHC_N_LF 4
#define SHC_N_LW 5
#define SHC_N_C  6

/* rotation types */
#define SHC_ROT_ZNE_TO_LQT 1
#define SHC_ROT_ZNE_TO_UVW 2
#define SHC_ROT_UVW_TO_ZNE 3

#define SHC_FILELTH 179
	/* maximum length of filenames */

/* flags */
typedef int SHFLAGS;
#define SHF_LOGCMD     0x1
#define SHF_ECHO       0x2
#define SHF_CAPCNV     0x4
#define SHF_STEP       0x8
#define SHF_VERIFY     0x10
#define SHF_CMDERRSTOP 0x20
#define SHF_SHERRSTOP  0x40
#define SHF_NOERRMSG   0x80
#define SHF_CHATTY     0x100
#define SHF_STARTUP    0x200


/* =========================== */
/* definitions of global types */
/* =========================== */

typedef float SAMPLE;   /* data type of seismogram samples */
typedef float REAL;     /* general floating point type */
/* typedef void TRACE; */    /* trace type (only used as pointer) */
#define TRACE void
#define SHENTRY unsigned
#define SHC_SAMPLEFMT "%e"
#define SHC_REALFMT "%e"

typedef struct sh_complex {
	REAL    re;    /* real part */
	REAL    im;    /* imaginary part */
} COMPLEX;



/* ---------------------------------------------------- */
/* the following type is used in the motif version only */
/* ---------------------------------------------------- */


#ifdef XXX

typedef struct {
	float     red;       /* red fraction */
	float     green;     /* green fraction */
	float     blue;      /* blue fraction,  all fractions are between 0 and 1 */
} SHT_COLOR;


/* definition of structure for global parameter set */
typedef struct {
	char      depth_phase_list[BC_LINELTH+1];   /* list of depth phases */
	char      theo_phase_list[BC_LINELTH+1];    /* list of theo. phases */
	char      diff_phase_list[BC_LINELTH+1];    /* list of diff. phases */
	float     close_phase_resol;     /* close phase resolution in pixel */
	int       min_drag_box_width; /* minimum width of drag box in pixel */
	char      defpath_filter[BC_FILELTH+1]; /* default path for filters */
	char      defpath_events[BC_FILELTH+1]; /* def.path for event files */
	char      defpath_gse[BC_FILELTH+1];     /* def. path for GSE files */
	char      defpath_gse2[BC_FILELTH+1];   /* def. path for GSE2 files */
	char      defpath_ah[BC_FILELTH+1];       /* def. path for AH files */
	char      defpath_q[BC_FILELTH+1];         /* def. path for Q files */
	char      defpath_evid[BC_FILELTH+1];    /* path for event ID files */
	char      defpath_evtout[BC_FILELTH+1];   /* event info output path */
	char      defpath_data[BC_FILELTH+1];     /* def.path for SEED data */
   BOOLEAN   top_down_order;       /* arrange trace from top to bottom */
	long      double_click_time;  /* maximum time diff. of clicks in ms */
	float     trace_zoom_base;            /* base of trace zoom formula */
	float     trace_zoom_exp;  /* exponent factor of trace zoom formula */
	SHT_COLOR color_mark;                               /* marker color */
	SHT_COLOR color_theo;                  /* theoretical phase markers */
	SHT_COLOR color_auto;           /* automatically picked phase color */
	SHT_COLOR color_crsr;                               /* cursor color */
	char      refstation[BC_SHORTSTRLTH+1];        /* reference station */
	BOOLEAN   autopick_first;         /* take first or bigger amplitude */
	char      edit_cmd[BC_LINELTH+1];     /* call to system text editor */
	float     calib_wdw_width;           /* width of calibration window */
	float     calib_wdw_height;         /* height of calibration window */
	float     calib_azimuth_grid;      /* azimuth grid in calib diagram */
	float     calib_slowness_grid;    /* slowness grid in calib diagram */
	BOOLEAN   auto_scaling;    /* scale traces on display automatically */
	BOOLEAN   use_rec_filters;  /* use recursive filters instead of FFT */
	char      analyst[BC_SHORTSTRLTH+1];      /* analyst's abbreviation */
	BOOLEAN   prompt_analyst;              /* prompt for analyst's name */
	char      motif_log[BC_FILELTH+1];         /* motif action log file */
	float     phase_ampl_diff;  /* max. seconds between phase and ampl. */
	BOOLEAN   reverse_xors;         /* reverse back/foreground on XOR's */
	int       x_max_drawlth;    /* max. no of samples in one XDrawLines */
	int       default_quality;           /* default phase quality value */
	int       drag_box_rubber_val; /* no of pixels to enable rubber box */
	char      default_filter[BC_FILELTH+1];   /* def. filter after read */
	int       default_phase_type;      /* default phase type, like tele */
	int       default_phase_flags;               /* default phase flags */
	int       default_depth_type;                 /* default depth type */
	float     default_depth;                     /* default depth in km */
	int       default_loc_quality;          /* default location quality */
	int       max_cursor_form;                /* maximum cursor form ID */
	char      event_check_proc[BC_FILELTH+1]; /* routine to check event */
	char      screendump_proc[BC_FILELTH+1];      /* screendump routine */
	char      evtview_proc[BC_FILELTH+1];  /* evt result view prcoedure */
	char      remreqhost[BC_SHORTSTRLTH+1];      /* request remote host */
	BOOLEAN   full_phase_names;             /* display full phase names */
	char      default_source[BC_SHORTSTRLTH+1];  /* default info source */
	float     move_wdw_step;        /* step size of move window command */
	int       top_margin;             /* top margin of display in pixel */
	char      auto_phase[cBcShortStrLth+1];   /* name of autopick phase */
	int       draw_area_height;      /* height of drawing area in pixel */
	int       draw_area_width;        /* width of drawing area in pixel */
	float     area_zoom_base;              /* drawing area resize, base */
	float     area_zoom_exp;           /* drawing area resize, exponent */
	char      reformat_proc[cBcFileLth+1];  /* routine to reformat data */
	char      final_proc[cBcFileLth+1];/* postprocessing 'final params' */
	BOOLEAN   own_accelerators;             /* use own accelerator code */
	char      filter_type;     /* filter type, replaces use_rec_filters */
} SHT_GLOBAL_SET;

#endif


#endif /* __SHCONST */

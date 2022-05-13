
/* file readgrn.h
 *      =========
 *
 * version 15, 22-May-2006
 *
 * header file of module readgrn
 * K. Stammler, 19-Aug-92
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


/* include time utilities if not yet defined */
#ifndef __BASECNST
#include BASECNST
#endif
#ifndef __TCUSRDEF
#include BC_TCUSRDEF
#endif


#ifdef BC_SUN
#define SBYTE signed char
#else
#define SBYTE char
#endif



/* error codes */
#define GRNE_OFFSET       5000
#define GRNE_FOPEN        (GRNE_OFFSET+1)  /* error opening GRN file */
#define GRNE_FREAD        (GRNE_OFFSET+2)  /* error reading GRN file */
#define GRNE_NODATARECORD (GRNE_OFFSET+3)  /* no data record */
#define GRNE_TIMEERROR    (GRNE_OFFSET+4)  /* specified time before file start */
#define GRNE_TOOSHORT     (GRNE_OFFSET+5)  /* file too short */
#define GRNE_SETPOS       (GRNE_OFFSET+6)  /* error in file positioning */
#define GRNE_MEMOVFL      (GRNE_OFFSET+7)  /* memory overflow */
#define GRNE_BUG          (GRNE_OFFSET+8)  /* bug in program */
#define GRNE_ZEROLTH      (GRNE_OFFSET+9)  /* zero length array */
#define GRNE_EOF          (GRNE_OFFSET+10) /* end of file detected */
#define GRNE_ILLNAME      (GRNE_OFFSET+11) /* illegal GRN filename */
#define GRNE_OPNGLS       (GRNE_OFFSET+12) /* error opening WORM directory */
#define GRNE_ILLSMPRATE   (GRNE_OFFSET+13) /* illegal sample rate */
#define GRNE_WRONGVOL     (GRNE_OFFSET+14) /* time not on volume */
#define GRNE_SMPSTATNF    (GRNE_OFFSET+15) /* station,sample rate not found */
#define GRNE_STROVFL      (GRNE_OFFSET+16) /* string overflow */
#define GRNE_NOLABELFILE  (GRNE_OFFSET+17) /* label file not found */
#define GRNE_LABNOTIME    (GRNE_OFFSET+18) /* time not found in label file */
#define GRNE_UKSTATION    (GRNE_OFFSET+19) /* unknown station */
#define GRNE_NOTIMPL      (GRNE_OFFSET+20) /* not implemented */
#define GRNE_DSCFORMAT    (GRNE_OFFSET+21) /* format error in DSC file */



/* constants */
#define GRNC_MAXSTATION 100
	/* maximum number of stations in station list */
#define GRNC_STATNAMELTH 10
	/* maximum length of station name */
#define GRN_FRAMELTH 64
	/* frame length in bytes */
#define GRN_RECLTH 4096
	/* record length in bytes */
#define GRN_FRAMERECS (GRN_RECLTH/GRN_FRAMELTH)
	/* number of frames in one record */
#define GRN_MEANSMPREC 3743.71
	/* mean samples per record */
#define GRN_MAXSMPREC 3772.0
	/* maximum number of samples per record */
#define GRN_MINSMPREC 943.0
	/* minimum number of samples per record */
#define GRN_FRAMEDIFFS 16
	/* number of difference longwords in one frame */
#define GRN_MAXFRAMESMP 3772
	/* maximum number of differences in one frame */
#define GRN_LISTFILEEXT ".gls"
	/* extension of WORM directory file */
#define GRN_MAXFLIST 5
	/* maximum number of files returned from grn_findgrnfiles */
#define GRN_GLSDIR "GRN_GLSDIR:"
	/* directory of GLS files (default) */
	/* The default works on VMS only, sorry */
#define GRN_LABELFILE "GRN_GLSDIR:WORM_LABELS.LIS"
	/* file with all GRN labels (default) */
	/* The default works on VMS only, sorry */
#define GRN_GRSN_DEFAULT_CALIB 1.67
	/* default calibration of GRSN stations */



/* types */

/* grn_readfile returns this structure with info's about the
 * trace read
 */
typedef struct {
	char     station[BC_SHORTSTRLTH+1];   /* station name */
	char     netname[BC_SHORTSTRLTH+1];   /* network name if available */
	char     comp;                        /* component */
	char     tstart[BC_LINELTH+1];        /* start time */
	float    dt;                          /* sample distance */
	long     missing;                     /* missing samples */
	float    calib;                       /* calibration constant */
} GRN_TRCINFO;

/* This is the binary time structure used in the SEED data records.
 * I didn't design it, therefore it is a bit different from the
 * NTIME structure used in the utility module "timeconv".  The
 * quite simple routines "grn_ntime_to_btime" and "grn_btime_to_ntime"
 * do the conversion between the two.  Don't care
 * about it if you use "grn_read" or "grn_readfile" only.
 */
typedef struct {
	unsigned  year, julday;     /* date */
	unsigned  hour, min, sec;   /* time */
	unsigned  ms10;             /* 0.0001 seconds */
	unsigned  align;            /* time align */
} BTIME;

/* This is for the header in each SEED data record.  Don't care
 * about it if you use "grn_read" or "grn_readfile" only.
 */
#define GRN_SHORTSTR 10
typedef struct {
	long     seqno;                    /* sequence number */
	char     station[GRN_SHORTSTR+1];  /* station name */
	char     channel[GRN_SHORTSTR+1];  /* channel name */
	BTIME    stime;                    /* start time */
	unsigned no_of_samples;            /* number of samples */
	unsigned activity;                 /* activity flags */
	int      smprate_factor;           /* sample rate factor */
	int      smprate_mult;             /* sample rate multiplier */
	long     correction;               /* time correction */
	unsigned databegin;                /* begin of data */
	unsigned firstblockette;           /* first blockette */
} GRN_DATHDR;



/* prototypes */
/*----------------------------------------------------------------------------*/


void grn_read( char device[], char label[], char tstart[], float seclength,
	char station[], char comp, int smprate, BOOLEAN swap, long *length,
	long **dat, GRN_TRCINFO *inf, STATUS *status );

/* Reads trace from GRN file.  If the GRN files are on an (already mounted)
 * WORM disk, then the device name should be specified in "device" and the
 * label of the disk is passed in "label" (The disk label may be determined
 * automatically from a given start time by the routine "grn_getlabel").
 * In this case the GRN file list is read from the appropriate label file
 * in directory "grnv_glsdir" (set by routine "grn_setpaths").  The paths
 * on the WORM disk are created automatically as well as the complete
 * GRN filename.
 *   If the files are available on a non-archive directory (usually on a
 * directory on some user disk) "device" should contain the complete
 * path name of the GRN files and "label" must be an empty string (pointer
 * to a zero byte).  This data directory must also contain a GRN file list
 * named "dirfile.gls".  The syntax of this file list is documented in routine
 * "grn_findgrnfiles".
 *   The start time may be passed in the format "13-Jan-1992_5:28:00.000"
 * (which may be abbreviated to "13-jan-92_5:28") or as a list of integers:
 * "13,1,1992,5,28,0,0" or "13,1,92,5,28".
 *
 * parameters of routine
 * char       device[];      input; name of WORM device
 * char       label[];       input; label of WORM disk
 * char       tstart[];      input; start time
 * float      seclength;     input; number of seconds to read
 * char       station[];     input; station name
 * char       comp;          input; component (Z,N,E)
 * int        smprate;       input; sample rate (1,20,80)
 * BOOLEAN    swap;          input; swap bytes
 * long       *length;       output; length of array in samples
 * long       **dat;         output; data array read
 * GRN_TRCINFO *inf;         output; trace information
 * STATUS     *status;       output; return status
 */


/*----------------------------------------------------------------------------*/


void grn_readfile( char grnfile[], char tstart[], float seclength,
	BOOLEAN swap, int firstrec, long *length, long **dat,
	GRN_TRCINFO *inf, STATUS *status );

/* reads GRN data file
 *
 * parameters of routine
 * char       grnfile[];     input; name of input file
 * char       tstart[];      input; start time
 * float      seclength;     input; number of seconds to be read
 * BOOLEAN    swap;          input; swap bytes
 * int        firstrec;      input; start search at record firstrec
 * long       *length;       output; length of array in samples;
 * long       **dat;         output; data array read
 * STATUS     *status;       output; return status
 */


/*----------------------------------------------------------------------------*/



void grn_name_to_time( char station[], char grnname[], TIME *time,
	STATUS *status );

/* converts GRN filename to time value
 *
 * parameters of routine
 * char       station[];     input; station name
 * char       grnname[];     input; GRN filename
 * TIME       *time;         output; start time
 * STATUS     *status;       output; return status
 */


/*--------------------------------------------------------------------*/


void grn_findgrnfiles( char device[], char label[], char station[], char chan[],
	TIME *start, int filenum, char flist[][BC_FILELTH+1], STATUS *status );

/* Finds GRN files on a disk labeled "label".  GRN file flist[0] contains
 * start time "start" and flist[1] to flist[filenum-1] are the
 * subsequent files on the volume.
 *   If the label is an empty string, device is assumed to hold the complete
 * path of the data directory.  In this case the list file is searched on
 * "grnv_glsdir" with the name "device"_dirfile.gls (VAX) or on the data
 * directory with the name dirfile.gls (all others).
 *   The syntax of the list file is explained in the following example
 * (please remind that the first three characters in each line " * " are
 * indicating comment lines within this text and that they are not part
 * of the list file):
 *
 * *BUG LHZ
 * *BUG LHN
 * *BUG LHE
 * *BUG BHZ
 * BUG920715.BHZ
 * BUG920716.BHZ
 * BUG920717.BHZ
 * *BUG BHN
 * BUG920715.BHN
 * BUG920716.BHN
 * BUG920717.BHN
 * *BUG BHE
 * BUG920715.BHE
 * BUG920716.BHE
 * BUG920717.BHE
 * *CLZ LHZ
 * *CLZ LHN
 * *CLZ LHE
 * *CLZ BHZ
 * CLZ920715.BHZ
 * CLZ920716.BHZ
 * *CLZ BHN
 * CLZ920715.BHN
 * CLZ920716.BHN
 * CLZ920717.BHN
 * *CLZ BHE
 * CLZ920715.BHE
 * CLZ920716.BHE
 * CLZ920717.BHE
 *  :
 *  :
 *
 * Each line starting with "*" indicates a new station & component & sample
 * rate combination.  All files of the directory matching this combination
 * must be listed in the subsequent lines in chronological order.  This
 * example shows day files only.  This is not required.  It is also possible
 * to have a list of event files with large time gaps in between.
 *
 * parameters of routine
 * char       device[];   input; WORM input device name
 * char       label[];    input; disk label
 * char       station[];  input; name of station
 * char       chan[];     input; data channel (for example HHZ)
 * TIME       *start;     input; start time to be searched
 * int        filenum;    input; number of files to be returned
 * char       flist[][];  output; list of files
 * STATUS     *status;    output; return status
 */


/*----------------------------------------------------------------------------*/


void grn_getlabel( char timestr[], int maxlth, char label[], STATUS *status );

/* Determines label from input time.  This routine may be used to determine
 * the required "label" parameter in routine "grn_read".  It is also useful
 * for programs which do automatic mounting of GRN disks.
 *
 * parameters of routine
 * char       timestr[];    input; time string
 * int        maxlth;       input; maximum length of output string
 * char       label[];      output; WORM label
 * STATUS     *status;      output; return status
 */


/*----------------------------------------------------------------------------*/


void grn_parse_stations( char list[],
	char stat[GRNC_MAXSTATION][GRNC_STATNAMELTH+1], int *statno );

/* Parses station list string "list" and returns list of stations "stat".
 * This is a utility routine which is not called within this module.  It is
 * useful if a station list supplied by the user is to be processed.  The
 * "grn_read" routine may be called in a loop over the "stat[i]" values.
 *
 * parameters of routine
 * char       list[];     input; station list (separator ",")
 * char       stat[][];   output; parsed station names
 * int        *statno;    output; number of names found
 */


/*----------------------------------------------------------------------------*/


void grn_analyse_header( SBYTE stream[], BOOLEAN swap, GRN_DATHDR *hdr,
	STATUS *status );

/* extracts header information from data stream
 *
 * parameters of routine
 * SBYTE       stream[];        input; data stream
 * BOOLEAN    swap;            input; swap bytes
 * GRN_DATHDR *hdr;            output; data header
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------------*/


void grn_btime_to_ntime( BTIME *btime, NTIME *ntime, STATUS *status );

/* converts BTIME to NTIME
 *
 * parameters of routine
 * BTIME      *bt;      input; SEED binary time
 * NTIME      *nt;      output; KS numeric time
 * STATUS     *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void grn_ntime_to_btime( NTIME *ntime, BTIME *btime, STATUS *status );

/* converts NTIME to BTIME
 *
 * parameters of routine
 * NTIME      *nt;      input; KS numeric time
 * BTIME      *bt;      output; SEED binary time
 * STATUS     *status;  output; return status
 */


/*----------------------------------------------------------------------------*/


void grn_get_station_id( char station[], int maxlth, char array[], int *arrid,
	int *id, STATUS *status );

/* determines name of array if possible.  Error occurs if output string length
 * is too short.
 *
 * parameters of routine
 * char       station[];     input; station name
 * int        maxlth;        input; maximum length of output string
 * char       array[];       output; name of array or '\0' if not found
 * int        *arrid;        output; array ID number
 * int        *id;           output; station number
 * STATUS     *status;       output; return status
 */


/*----------------------------------------------------------------------------*/


void grn_set_vax_label_file( char fname[] );

/* changes VAX name of VAX label file
 *
 * parameters of routine
 * char       fname[];         input; name of label file
 */


/*----------------------------------------------------------------------------*/


/* file readgrn.c
 *      =========
 *
 * version 49, 22-May-2006
 *
 * reads GRN data files
 * K. Stammler, 18-Aug-92
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1992,  Klaus Stammler, Federal Institute for Geosciences
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



#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "basecnst.h"
#include "sysbase.h"
#include "shvars.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "tcusrdef.h"
#include "utusrdef.h"
#include "station_no.h"  /* station numbers */
#include "readgrn.h"  /* this is the local header file (types and "grn_..."-routines) */


/* separation character between root path and GRN subdirectories: */
#ifdef BC_VAX
#define EODEV ':'
#else
#ifdef BC_ATARI
#define EODEV '\\'
#else
#define EODEV '/'
#endif
#endif



/* entries of the fixed section of SEED data header */
#define DHFS_P_SEQUENCE        0
#define DHFS_L_SEQUENCE        6
#define DHFS_B_INDICATOR       6
#define DHFS_P_STATION         8
#define DHFS_L_STATION         5
#define DHFS_P_CHANNEL        15
#define DHFS_L_CHANNEL         3
#define DHFS_P_STARTTIME      20
#define DHFS_L_STARTTIME      10
#define DHFS_W_SAMPLENO       30
#define DHFS_W_RATEFACTOR     32
#define DHFS_W_RATEMULT       34
#define DHFS_B_ACTIVITY       36
#define DHFS_B_IO             37
#define DHFS_B_QUALITY        38
#define DHFS_B_BLOCKETTES     39
#define DHFS_L_CORRECTION     40
#define DHFS_W_DATABEGIN      44
#define DHFS_W_FIRSTBLOCKETTE 46


/* types */

typedef struct {   /* station header */
	BOOLEAN       valid;                    /* information is valid */
	float         calib[STC_NUM_COMP];      /* calibration constants */
} GRNT_STATHEAD;



/* prototypes of local routines (not exported) */
static unsigned short int grn_get2bytes( SBYTE b[], BOOLEAN swap );
static unsigned long int grn_get4bytes( SBYTE b[], BOOLEAN swap );
static int grn_cmp_btime( BTIME *t1, BTIME *t2 );
static void grn_warning( char text[] );
static void grn_jump_records( FILE *grn, BOOLEAN swap, int firstrec,
	BTIME *reqbtime, float *dt, STATUS *status );
static void grn_decode_frame( SBYTE *frame, BOOLEAN swap, long diff[], int *lth );
static void grn_decode( SBYTE rec1[], SBYTE rec2[], FILE *grn, BOOLEAN swap, float dt,
	BTIME *reqbtime, long length, long smp[], long *outlth,
	BTIME *rettime, STATUS *status );
static void grn_strip_blanks( char str[] );
static void grn_printhdr( GRN_DATHDR *hdr );
static char *grn_findchar( char str[], char c );
static void grn_read_descriptor( char station[], char datafile[], STATUS *status );



/* global variables, there are currently no routines to change them.
 * They are practically constants.
 */
static char      grnv_glsdir[BC_FILELTH+1]={GRN_GLSDIR};
	/* directory of .gls files */
static char      grnv_labelfile[BC_FILELTH+1]={GRN_LABELFILE};
	/* file with all GRN labels */
static BOOLEAN   grnv_logchars=TRUE;    /* log read operations on screen */
static GRNT_STATHEAD grnv_head[STC_NUM_ALL];  /* header info of stations */



/* macros */
#define grn_readlog(s)  if (grnv_logchars) printf(s)
	/* print read log chars if requested */



/*----------------------------------------------------------------------------*/



void grn_read( char device[], char label[], char tstart[], float seclength,
	char station[], char comp, int smprate, BOOLEAN swap, long *length,
	long **dat, GRN_TRCINFO *inf, STATUS *status )

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
 * char       device[];      input; name of WORM device (or disk directory)
 * char       label[];       input; label of WORM disk (or empty string)
 * char       tstart[];      input; requested start time
 * float      seclength;     input; number of seconds to read
 * char       station[];     input; station name
 * char       comp;          input; component ('Z', 'N' or 'E')
 * int        smprate;       input; sample rate (1, 20 or 80)
 * BOOLEAN    swap;          input; swap bytes
 * long       *length;       output; length of array in samples
 * long       **dat;         output; data array read
 * GRN_TRCINFO *inf;         output; trace information
 * STATUS     *status;       output; return status (zero is ok)
 */
{
	/* local variables */
	char     ldev[BC_FILELTH+1];        /* copy of "device", to complete terminator */
	char     chan[BC_SHORTSTRLTH+1];    /* channel code */
	char     upstat[BC_SHORTSTRLTH+1];  /* uppercase station name */
	TIME     start;                     /* start time */
	char     cstart[BC_LINELTH+1];      /* current start time */
	char     flist[GRN_MAXFLIST][BC_FILELTH+1];  /* file list */
	int      listlth=3;                 /* list length */
	int      fcnt;                      /* file counter */
	float    cseclth;                   /* current length in secs */
	long     clth[GRN_MAXFLIST];        /* length read */
	long     *cdat[GRN_MAXFLIST];       /* data */
	float    rdsec;                     /* seconds read from file */
	int      i, j;                      /* counters */
	long     *mp;                       /* moving pointer */
	GRN_TRCINFO linf;                   /* local info block */

	/* executable code */

	/* make station and component uppercase */
	comp = Cap( comp );
	if  (strlen(station) > BC_SHORTSTRLTH)  {
		*status = GRNE_STROVFL;
		return;
	} /*endif*/
	strcpy( upstat, station );
	ut_cap( upstat );

	/* create channel code */
	if  (smprate != 1 && smprate != 20 && smprate != 80)  {
		*status = GRNE_ILLSMPRATE;
		return;
	} /*endif*/
	if  (smprate == 80)  *chan = 'H';
	else if  (smprate == 20)  *chan = 'B';
	else  *chan = 'L';
	chan[1] = 'H';
	chan[2] = comp;
	chan[3] = '\0';

	tc_t2a( tstart, &start, status );
	if  (Severe(status))  return;

	/* check separation character in device name */
	strcpy( ldev, device );       /* "device" is an input parameter. */
	i = (int)strlen(ldev) - 1;    /* This is not changed.  Therefore */
	if  (ldev[i] != EODEV)  {     /* a copy is necessary             */
		ldev[i+1] = EODEV;
		ldev[i+2] = '\0';
		/* was before: strcat( ldev, ":" ); */
	} /*endif*/

	/* now get list of file names for this start time */
	grn_findgrnfiles( ldev, label, station, chan, &start, listlth,
		flist, status );
	if  (Severe(status)) return;
	if  (flist[0][0] == '\0')  {
		sprintf( cstart, "\n--- station %s %s not found", station, chan );
		grn_warning( cstart );
		*length = 0; *dat = NULL;
		return;
	} /*endif*/

	/* read descriptor file of first datafile */
	grn_read_descriptor( upstat, flist[0], status );
	if  (Severe(status))  {
		*status = BC_NOERROR;
		printf( "--> no descriptor file found; use counts as amplitude\n" );
	} /*endif*/

	/* start in first file and read all files until the requested time */
	/* span is read in (most often the first file is enough).          */
	strcpy( cstart, tstart );
	cseclth = seclength;
	for  (fcnt=0; fcnt<listlth; fcnt++)  {
		if  (flist[fcnt][0] == '\0')  {
			grn_warning( "\n--- no more files on volume" );
			fcnt--;       /* would need some more data, but there is no more */
			break;        /* file.  That's life.                             */
		} /*endif*/
		grn_readfile( flist[fcnt], cstart, cseclth, swap, -1,
			clth+fcnt, cdat+fcnt, &linf, status );
		if  (fcnt == 0) *inf = linf;
		if  (Severe(status))  {                             /* Something didn't work */
			for  (i=0; i<fcnt; i++)  free( cdat[i] );   /* Free all previous     */
			*length = 0; *dat = NULL;                   /* parts and give up     */
			return;
		} /*endif*/
		if  (linf.missing == 0)  {  /* everything read */
			if  (fcnt == 0)  {
				*length = clth[0];    /* Got the whole thing in one go. */
				*dat = cdat[0];       /* Don't have to mess around with */
				return;               /* concatenation.  Just return.   */
			} /*endif*/
			break;                   /* There are several parts, some more work... */
		} /*endif*/
		rdsec = (float)(clth[fcnt]) * linf.dt;
		tc_tadd( cstart, rdsec, cstart, status );
		cseclth -= rdsec;
	} /*endfor*/

	/* now either all samples read or no more filename available */
	if  (linf.missing > 0)  {
		sprintf( cstart, "read %d files, still %ld samples missing",
			fcnt+1, linf.missing );
		grn_warning( cstart );
	} /*endif*/
	inf->missing = linf.missing;

	/* total length of output is sum of all parts */
	*length = 0;
	for  (i=0; i<=fcnt; i++)
		*length += clth[i];

	/* concatenate all parts: get memory for total trace */
	*dat = (long *)malloc( (*length)*sizeof(long) );
	if  (*dat == NULL)  {
		for  (i=0; i<=fcnt; i++)
			free( cdat[i] );
		*status = GRNE_MEMOVFL;
		return;
	} /*endif*/

	/* do concatenation */
	mp = *dat;
	for  (i=0; i<=fcnt; i++)  {
		for  (j=0; j<clth[i]; j++)
			*mp++ = cdat[i][j];
		free( cdat[i] );
	} /*endfor*/

} /* end of grn_read */



/*----------------------------------------------------------------------------*/



void grn_readfile( char grnfile[], char tstart[], float seclength,
	BOOLEAN swap, int firstrec, long *length, long **dat, GRN_TRCINFO *inf,
	STATUS *status )

/* Reads GRN data file.  This is one level below "grn_read".  Here you have
 * to pass the complete GRN filename and the start time of the requested data.
 * Normally "grn_read" does this for you, but it needs the GRN file lists
 * for it.  If firstrec < 0, then the standard search routine is used to find the
 * start record, otherwise it starts searching at the specified record.
 *
 * parameters of routine
 * char       grnfile[];     input; name of input file
 * char       tstart[];      input; start time
 * float      seclength;     input; number of seconds to be read
 * BOOLEAN    swap;          input; swap bytes
 * int        firstrec;      input; start search at record "firstrec"
 * long       *length;       output; length of array in samples;
 * long       **dat;         output; data array read
 * GRN_TRCINFO *inf;         output; trace info
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	FILE     *grn;                      /* file pointer */
	SBYTE     datrec[2][GRN_RECLTH];    /* data records */
	long     ptsread;                   /* items read */
	GRN_DATHDR hdr;                     /* data header */
	int      reccnt;                    /* record counter */
	NTIME    reqntime;                  /* requested (numeric) time */
	BTIME    reqbtime;                  /* requested time */
	BTIME    retbtime;                  /* returned time */
	int      curr_rec, prev_rec;        /* current and previous record */
	float    dt;                        /* sample distance in sec */
	float    tmp;                       /* scratch */
	BOOLEAN  eof_found;                 /* found end of file */
	STATUS   locstat;                   /* local status */
	int      station_num;               /* station number */
	int      comp_id;                   /* component ID */

	/* executable code */

	eof_found = FALSE;

	/* make NTIME from time string */
	tc_t2n( tstart, &reqntime, status );
	if  (Severe(status))  return;
	grn_ntime_to_btime( &reqntime, &reqbtime, status );

	/* open data file in binary mode */
	grn = sy_fopen( grnfile, "rb" );
	if  (grn == NULL)  {*status = GRNE_FOPEN; return;}

	/* find approximate start record */
	/* printf( "---> start positioning\n" ); */
	grn_jump_records( grn, swap, firstrec, &reqbtime, &dt, status );
	if  (Severe(status))  {fclose(grn); return;}

	/* get output length */
	*length = Nlong( seclength / dt );

	/* now find correct record */
	curr_rec = 1;
	prev_rec = 0;
	reccnt = 0;
	do  {

		/* read header */
		grn_readlog( "." );
		ptsread = fread( datrec[curr_rec], 1, GRN_RECLTH, grn );
		if  (ptsread == 0)  {  /* end of file found */
			eof_found = TRUE;
		} else if  (ptsread != GRN_RECLTH)  {
			*status = GRNE_FREAD;
			fclose( grn );
			return;
		} /*endif*/
		/* extract info */
		if  (!eof_found)  {
			grn_analyse_header( datrec[curr_rec], swap, &hdr, status );
			if  (Severe(status))  {fclose( grn ); return;}
		} /*endif*/

		/* flip record pointers */
		curr_rec = (curr_rec == 1) ? 0 : 1;
		prev_rec = (prev_rec == 1) ? 0 : 1;
		reccnt++;

	}  while  (grn_cmp_btime(&(hdr.stime),&reqbtime) <= 0 && !eof_found);

	/* now "curr_rec" points to the first record and */
	/* "prev_rec" to the next (sorry about this) */

	/* if first record is after requested time, time is not in this file */
	if  (reccnt == 1)  {
		/* *status = GRNE_TIMEERROR; */
		/* fclose( grn ); */
 		/* return; */
		/* above statement about curr_rec & prev_rec is not true here */
		curr_rec = (curr_rec == 1) ? 0 : 1;
		prev_rec = (prev_rec == 1) ? 0 : 1;
		grn_readlog( "." );
		ptsread = fread( datrec[prev_rec], 1, GRN_RECLTH, grn );
		if  (ptsread == 0)  {
			eof_found = TRUE;
		} else if  (ptsread != GRN_RECLTH)  {
			*status = GRNE_FREAD;
			fclose( grn );
			return;
		} /*endif*/
	} /*endif*/

	/* check whether "curr_rec" contains requested time */
	grn_analyse_header( datrec[curr_rec], swap, &hdr, status );
	tmp = hdr.no_of_samples * dt;  /* time interval of record in sec */
	grn_btime_to_ntime( &(hdr.stime), &reqntime, status );
	if  (Severe(status))  {fclose( grn ); return;}
	tc_nadd( &reqntime, tmp, &reqntime, status );
	if  (Severe(status))  {fclose( grn ); return;}
	grn_ntime_to_btime( &reqntime, &retbtime, status );
	if  (Severe(status))  {fclose( grn ); return;}
	if  (grn_cmp_btime(&retbtime,&reqbtime) < 0)  {
		/* time gap at requested time: take beginning of next record */
		grn_warning( "\n--- time gap at requested time: take next record"  );
		if  (eof_found)  {
			/* seems to be too complicated for the beginning */
			grn_warning( "--- >>> and EOF found, terminate" );
			*status = GRNE_EOF;
			fclose( grn );
			return;
		} /*endif*/
		curr_rec = (curr_rec == 1) ? 0 : 1;
		prev_rec = (prev_rec == 1) ? 0 : 1;
		grn_analyse_header( datrec[curr_rec], swap, &hdr, status );
		reqbtime = hdr.stime;
		grn_printhdr( &hdr );
		/* read next record to "prev_rec" */
		grn_readlog( "." );
		ptsread = fread( datrec[prev_rec], 1, GRN_RECLTH, grn );
		if  (ptsread != GRN_RECLTH)  {
			*status = GRNE_FREAD;
			fclose( grn );
			return;
		} /*endif*/
	} /*endif*/

	/* allocate memory and get samples */
	*dat = (long *)malloc( (*length)*sizeof(long) );
	if  (*dat == NULL)  {
		*status = GRNE_MEMOVFL;
		fclose( grn );
		return;
	} /*endif*/
	/* printf( "\n---> start reading\n" ); */
	if  (eof_found)  {
		grn_decode( datrec[curr_rec], NULL, grn, swap, dt,
			&reqbtime, *length, *dat, &ptsread, &retbtime, status );
	} else {
		grn_decode( datrec[curr_rec], datrec[prev_rec], grn, swap, dt,
			&reqbtime, *length, *dat, &ptsread, &retbtime, status );
	} /*endif*/
	grn_readlog( "\n" );
	/* printf( "---> end of reading\n" ); */
	if  (*status == GRNE_EOF && ptsread <= *length)
		*status = BC_NOERROR;
	if  (Severe(status))  {fclose( grn ); free( *dat ); return;}

	fclose( grn );

	/* return station info */
	strcpy( inf->station, hdr.station );
	inf->comp = hdr.channel[2];
	grn_btime_to_ntime( &retbtime, &reqntime, status );
	tc_n2t( &reqntime, inf->tstart, status );
	if  (Severe(status))  {
		*status = BC_NOERROR;
		strcpy( inf->tstart, "******" );
	} /*endif*/
	inf->dt = dt;
	inf->missing = *length - ptsread;
	*length = ptsread;
	locstat = BC_NOERROR;
	grn_get_station_id( inf->station, BC_SHORTSTRLTH, inf->netname,
		NULL, &station_num, &locstat );
	if  (inf->comp == 'N')       comp_id = STC_COMP_N;
	else if  (inf->comp == 'E')  comp_id = STC_COMP_E;
	else                         comp_id = STC_COMP_Z;
	if  (locstat == BC_NOERROR)  {
		inf->calib = grnv_head[station_num].valid ?
			grnv_head[station_num].calib[comp_id] : 1.0;
	} else {
		inf->calib = 1.0;
	} /*endif*/

} /* end of grn_readfile */



/*----------------------------------------------------------------------------*/



void grn_analyse_header( SBYTE stream[], BOOLEAN swap, GRN_DATHDR *hdr,
	STATUS *status )

/* extracts header information from data stream
 *
 * parameters of routine
 * SBYTE      stream[];        input; data stream
 * BOOLEAN    swap;            input; swap bytes
 * GRN_DATHDR *hdr;            output; data header
 * STATUS     *status;         output; return status
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];   /* scratch string */

	/* executable code */

	strncpy( str, (char *)stream+DHFS_P_SEQUENCE, DHFS_L_SEQUENCE );
	str[DHFS_L_SEQUENCE] = '\0';
	sscanf( str, "%ld", &hdr->seqno );
	if  (stream[DHFS_B_INDICATOR] != 'D')  {
		*status = GRNE_NODATARECORD;
		return;
	} /*endif*/
	strncpy( hdr->station, (char *)stream+DHFS_P_STATION, DHFS_L_STATION );
	hdr->station[DHFS_L_STATION] = '\0';
	grn_strip_blanks( hdr->station );
	strncpy( hdr->channel, (char *)stream+DHFS_P_CHANNEL, DHFS_L_CHANNEL );
	hdr->channel[DHFS_L_CHANNEL] = '\0';
	hdr->stime.year = grn_get2bytes( stream+DHFS_P_STARTTIME, swap );
	hdr->stime.julday = grn_get2bytes( stream+DHFS_P_STARTTIME+2, swap );
	hdr->stime.hour = (unsigned)stream[DHFS_P_STARTTIME+4];
	hdr->stime.min = (unsigned)stream[DHFS_P_STARTTIME+5];
	hdr->stime.sec = (unsigned)stream[DHFS_P_STARTTIME+6];
	hdr->stime.align = (unsigned)stream[DHFS_P_STARTTIME+7];
	hdr->stime.ms10 = grn_get2bytes( stream+DHFS_P_STARTTIME+8, swap );
	hdr->no_of_samples = grn_get2bytes( stream+DHFS_W_SAMPLENO, swap );
	hdr->smprate_factor = (signed short int)grn_get2bytes(
		stream+DHFS_W_RATEFACTOR, swap );
	hdr->smprate_mult = (signed short int)grn_get2bytes(
		stream+DHFS_W_RATEMULT, swap );
	hdr->activity = (int)stream[DHFS_B_ACTIVITY];
	hdr->correction = grn_get4bytes( stream+DHFS_L_CORRECTION, swap );

} /* end of grn_analyse_header */



/*----------------------------------------------------------------------------*/



static unsigned short int grn_get2bytes( SBYTE b[], BOOLEAN swap )

/* returns word from byte stream
 *
 * parameters of routine
 * SBYTE       b[];     input;   byte stream
 * BOOLEAN    swap;    input;   swap bytes
 */
{
	/* local variables */
	unsigned short int   word;

	/* executable code */

	if  (swap)  {
		word = ((unsigned short)(b[0]) & 0xff) * 0x100;
		word += (unsigned short)(b[1]) & 0xff;
	} else {
		word = ((unsigned short)(b[1]) & 0xff) * 0x100;
		word += (unsigned short)(b[0]) & 0xff;
	} /*endif*/
	return word;

} /* end of grn_get2bytes */



/*----------------------------------------------------------------------------*/



static unsigned long int grn_get4bytes( SBYTE b[], BOOLEAN swap )

/* returns word from byte stream
 *
 * parameters of routine
 * SBYTE       b[];     input;   byte stream
 * BOOLEAN    swap;    input;   swap bytes
 */
{
	/* local variables */
	unsigned long int   lword;

	/* executable code */

	if  (swap)  {
		lword = ((unsigned long)(b[0]) & 0xffL) * 0x1000000L;
		lword += ((unsigned long)(b[1]) & 0xffL) * 0x10000L;
		lword += ((unsigned long)(b[2]) & 0xffL) * 0x100;
		lword += (unsigned long)b[3] & 0xffL;
	} else {
		lword = ((unsigned long)(b[3]) & 0xffL) * 0x1000000L;
		lword += ((unsigned long)(b[2]) & 0xffL) * 0x10000L;
		lword += ((unsigned long)(b[1]) & 0xffL) * 0x100;
		lword += (unsigned long)b[0] & 0xffL;
	} /*endif*/
	return lword;

} /* end of grn_get4bytes */



/*----------------------------------------------------------------------------*/



static int grn_cmp_btime( BTIME *t1, BTIME *t2 )

/* compares to BTIME's.  returns -1 if t1 < t2, returns +1 if t1 > t2 and
 * returns 0 if t1 = t2
 *
 * parameters of routine
 * BTIME      *t1, *t2;    input; times to be compared
 */
{
	/* local variables */

	/* executable code */

	if  (t1->year < t2->year)  return -1;
	if  (t1->year > t2->year)  return 1;
	if  (t1->julday < t2->julday)  return -1;
	if  (t1->julday > t2->julday)  return 1;
	if  (t1->hour < t2->hour)  return -1;
	if  (t1->hour > t2->hour)  return 1;
	if  (t1->min < t2->min)  return -1;
	if  (t1->min > t2->min)  return 1;
	if  (t1->sec < t2->sec)  return -1;
	if  (t1->sec > t2->sec)  return 1;
	if  (t1->ms10 < t2->ms10)  return -1;
	if  (t1->ms10 > t2->ms10)  return 1;
	return 0;

} /* end of grn_cmp_btime */



/*----------------------------------------------------------------------------*/



void grn_btime_to_ntime( BTIME *btime, NTIME *ntime, STATUS *status )

/* converts BTIME to NTIME
 *
 * parameters of routine
 * BTIME      *bt;      input; SEED binary time
 * NTIME      *nt;      output; KS numeric time
 * STATUS     *status;  output; return status
 */
{
	/* executable code */

	ntime->year = btime->year;
	tc_dayofmn( btime->year, btime->julday, &(ntime->month),
		&(ntime->day), status );
	if  (Severe(status))  return;
	ntime->hour = btime->hour;
	ntime->min = btime->min;
	ntime->sec = btime->sec;
	ntime->ms = btime->ms10/10;

} /* end of grn_btime_to_ntime */



/*----------------------------------------------------------------------------*/



void grn_ntime_to_btime( NTIME *ntime, BTIME *btime, STATUS *status )

/* converts NTIME to BTIME
 *
 * parameters of routine
 * NTIME      *nt;      input; KS numeric time
 * BTIME      *bt;      output; SEED binary time
 * STATUS     *status;  output; return status
 */
{
	/* executable code */

	btime->year = ntime->year;
	btime->julday = tc_julian( ntime->year, ntime->month,
		ntime->day, status );
	if  (Severe(status))  return;
	btime->hour = ntime->hour;
	btime->min = ntime->min;
	btime->sec = ntime->sec;
	btime->ms10 = ntime->ms*10;

} /* end of grn_ntime_to_btime */



/*----------------------------------------------------------------------------*/



static void grn_warning( char text[] )

/* Prints warning message.  Currently just a printout on stdout.  May be used
 * get a log file as well.
 *
 * parameter of routine
 * char      text[];   input; warning message
 */
{
	/* executable code */

	printf( "%s\n", text );

} /* end of grn_warning */



/*----------------------------------------------------------------------------*/



static void grn_jump_records( FILE *grn, BOOLEAN swap, int firstrec,
	BTIME *reqbtime, float *dt, STATUS *status )

/* jumps to an approximate start record (before requested start time) if
 * firstrec < 0, otherwise it jumps to record firstrec
 *
 * parameters of routine
 * FILE       *grn;        input; GRN file pointer
 * BOOLEAN    swap;        input; swap bytes
 * int        firstrec;    input; start at record firstrec
 * BTIME      *reqbtime;   input; requested start time
 * float      *dt;         output; sample distance in sec
 * STATUS     *status;     output; return status
 */
#define TESTLTH 50
{
	/* local variables */
	SBYTE     datrec[GRN_RECLTH];  /* logical data record */
	long     ptsread;              /* points read from file */
	NTIME    curntime;             /* current (numerical) time */
	NTIME    reqntime;             /* requested (numerical) time */
	GRN_DATHDR hdr;                /* data header */
	float    tmp;                  /* scratch */
	long     setpos;               /* file positions */
	int      fstat;                /* file I/O status */
	float    meansmp;              /* mean samples per record */
	NTIME    nt2;                  /* time values of test record */
	long     testlth;              /* number of records for test span */
#	ifdef BC_VAX                   /* need this on VAX only: */
	char     str[BC_FILELTH+1];    /* file name to re-open file */
#	endif

	/* executable code */

	testlth = TESTLTH;
	if  (firstrec >= 0)  {
		grn_readlog( "." );
		fstat = fseek( grn, testlth*GRN_RECLTH, 0 );
#		ifdef BC_ATARI                 /* for ATARI special test            */
		if  (feof(grn))  fstat = -1;   /* because fseek here also returns 0 */
#		endif                          /*                                   */
		if  (fstat != 0)  {
			*status = GRNE_FREAD;
			return;
		} /*endif*/
		grn_analyse_header( datrec, swap, &hdr, status );
		if  (Severe(status))  return;
		if  (hdr.smprate_factor > 0 && hdr.smprate_mult > 0)  {
			*dt = 1.0 / (float)(hdr.smprate_factor*hdr.smprate_mult);
		} else if  (hdr.smprate_factor > 0 && hdr.smprate_mult < 0)  {
			*dt = -1.0 / ((float)(hdr.smprate_factor)/(float)(hdr.smprate_mult));
		} else if  (hdr.smprate_factor < 0 && hdr.smprate_mult > 0)  {
			*dt = -1.0 / ((float)(hdr.smprate_mult)/(float)(hdr.smprate_factor));
		} else if  (hdr.smprate_factor < 0 && hdr.smprate_mult < 0)  {
			*dt = 1.0 / ((float)(hdr.smprate_mult)/(float)(hdr.smprate_factor));
		} /*endif*/
		fstat = fseek( grn, testlth*GRN_RECLTH, 0 );
		return;
	} /*endif*/

	/* read first record and check time */
	grn_readlog( "." );
	ptsread = fread( datrec, 1, GRN_RECLTH, grn );
	if  (ptsread != GRN_RECLTH)  {*status = GRNE_FREAD; return;}
	/* extract info */
	grn_analyse_header( datrec, swap, &hdr, status );
	if  (Severe(status))  return;
	grn_btime_to_ntime( &(hdr.stime), &curntime, status );
	if  (Severe(status))  return;
	grn_btime_to_ntime( reqbtime, &reqntime, status );
	if  (Severe(status))  return;
	if  (hdr.smprate_factor > 0 && hdr.smprate_mult > 0)  {
		*dt = 1.0 / (float)(hdr.smprate_factor*hdr.smprate_mult);
	} else if  (hdr.smprate_factor > 0 && hdr.smprate_mult < 0)  {
		*dt = -1.0 / ((float)(hdr.smprate_factor)/(float)(hdr.smprate_mult));
	} else if  (hdr.smprate_factor < 0 && hdr.smprate_mult > 0)  {
		*dt = -1.0 / ((float)(hdr.smprate_mult)/(float)(hdr.smprate_factor));
	} else if  (hdr.smprate_factor < 0 && hdr.smprate_mult < 0)  {
		*dt = 1.0 / ((float)(hdr.smprate_mult)/(float)(hdr.smprate_factor));
	} /*endif*/
	tmp = tc_ndiff( &reqntime, &curntime, status ); /* diff. in seconds */
	if  (Severe(status))  return;
	if  (tmp < 0.0)  {
		/* *status = GRNE_TIMEERROR; */
		grn_warning( "\n--- jump: negative offset, set position to 0" );
		grn_printhdr( &hdr );
		grn_readlog( "-" );
		fseek( grn, 0L, 0 );
		return;
	} /*endif*/

	/* determine mean samples per record */
	testlth = TESTLTH;
	grn_readlog( "-" );
	fstat = fseek( grn, testlth*GRN_RECLTH, 0 );
#	ifdef BC_ATARI                 /* for ATARI special test            */
	if  (feof(grn))  fstat = -1;   /* because fseek here also returns 0 */
#	endif                          /*                                   */
	if  (fstat != 0)  {
#		ifdef BC_VAX                /* VAX-routine does not recover from */
		fgetname( grn, str );       /* this status.  Therefore close and */
		fclose( grn );              /* reopen GRN file                   */
		grn = sy_fopen( str, "r" ); /*                                   */
#		endif                       /*                                   */
		testlth = 20;
		grn_readlog( "t" );
		fstat = fseek( grn, testlth*GRN_RECLTH, 0 );
#		ifdef BC_ATARI               /* Again special ATARI test          */
		if  (feof(grn))  fstat = -1; /* because fseek here also returns 0 */
#		endif                        /*                                   */
		if  (fstat != 0)  {          /* very short file, just rewind it   */
			grn_readlog( "s" );
#			ifdef BC_VAX                 /* Again close and reopen on VAX   */
			sy_fclose( grn );            /* Silly routine !                 */
			grn = sy_fopen( str, "r" );  /*                                 */
#			else                         /*                                 */
			fseek( grn, 0, 0 );          /* on other machines a simple      */
#			endif                        /* rewind should be ok             */
			return;
		} /*endif*/
	} /*endif*/
	if  (fstat != 0)  {
#		ifdef BC_VAX                 /* No more comment on VAX          */
		fgetname( grn, str );        /* routine ...                     */
		sy_fclose( grn );            /*                                 */
		grn = sy_fopen( str, "r" );  /*                                 */
#		endif                        /*                                 */
		meansmp = GRN_MEANSMPREC;
		grn_readlog( "m" );
		fseek( grn, 0L, 0 );
	} else {
		grn_readlog( "." );
		ptsread = fread( datrec, 1, GRN_RECLTH, grn );
		if  (ptsread != GRN_RECLTH)  {
			meansmp = GRN_MEANSMPREC;
			grn_readlog( "-" );
			fseek( grn, 0L, 0 );
		} else {
			grn_analyse_header( datrec, swap, &hdr, status );
			if  (Severe(status))  {
				meansmp = GRN_MEANSMPREC;
				grn_readlog( "-" );
				fseek( grn, 0L, 0 );
				*status = BC_NOERROR;
			} else {
				grn_btime_to_ntime( &(hdr.stime), &nt2, status );
				if  (Severe(status))  return;
				meansmp = tc_ndiff( &nt2, &curntime, status );
				if  (Severe(status))  return;
				meansmp /= (*dt);  /* diff in samples */
				meansmp /= (float)TESTLTH;   /* diff in records */
				/* printf( "---> meansmp: %f\n", meansmp ); */
			} /*endif*/
		} /*endif*/
	} /*endif*/

	if  (meansmp < GRN_MINSMPREC)  {
		grn_warning( "\n--- mean samples/record too small\n" );
		meansmp = GRN_MEANSMPREC;
	} else if  (meansmp > GRN_MAXSMPREC)  {
		grn_warning( "\n--- mean samples/record too large\n" );
		meansmp = GRN_MEANSMPREC;
	} /*endif*/

	tmp /= (*dt);  /* difference in samples */
	tmp /= meansmp;  /* difference in records */

	if  (tmp < 1.0)  {grn_readlog("-"); fseek(grn,0L,0); return;} /* already positioned */
	setpos = (long)(tmp-1)*GRN_RECLTH;
	grn_readlog( "-" );
	fstat = fseek( grn, setpos, 0 );
	/* if  (fstat != 0)  {*status = GRNE_TOOSHORT; return;} */
	if  (fstat != 0)  {
#		ifdef BC_VAX                 /* No more comment on VAX          */
		fgetname( grn, str );        /* routine ...                     */
		sy_fclose( grn );            /*                                 */
		grn = sy_fopen( str, "r" );  /*                                 */
#		endif                        /*                                 */
		setpos = (long)((tmp*0.8)-1)*GRN_RECLTH;
		grn_readlog( "_" );
		fstat = fseek( grn, setpos, 0 );
		if  (fstat != 0)  {*status = GRNE_TOOSHORT; return;}
	} /*endif*/
#	ifdef XXX
	if  (fstat != 0)  { /*{*status = GRNE_TOOSHORT; return;}*/
		grn_warning( "\n--- jump: file too short" );
		do  {
			setpos -= GRN_RECLTH;
			if  (setpos < 0)  {
				*status = GRNE_TOOSHORT;
				return;
			} /*endif*/
			grn_readlog( "-" );
			fstat = fseek( grn, setpos, 0 );
		}  while (fstat != 0);
		grn_warning( "    jump: now found last record" );
	} /*endif*/
#	endif

	/* try this record */
	grn_readlog( "." );
	ptsread = fread( datrec, 1, GRN_RECLTH, grn );
	while  (ptsread == 0)  {  /* might be at end of file */
		setpos -= GRN_RECLTH;
		grn_readlog( "-" );
		fstat = fseek( grn, setpos, 0 );
		grn_readlog( "." );
		ptsread = fread( datrec, 1, GRN_RECLTH, grn );
	} /*endwhile*/
	if  (ptsread != GRN_RECLTH)  {*status = GRNE_FREAD; return;}
	/* extract info */
	grn_analyse_header( datrec, swap, &hdr, status );
	if  (Severe(status))  return;
	if  (grn_cmp_btime(&(hdr.stime),reqbtime) < 0)  {
		/* printf( "--->>> jump is short, %02d:%02d  %02d:%02d\n",
			hdr.stime.hour, hdr.stime.min, reqbtime->hour, reqbtime->min ); */
		grn_readlog( "-" );
		fseek( grn, setpos, 0 );
		return;
	} /*endif*/

	/* go back */
	/* printf( "--->>> jump is long\n" ); */
	grn_btime_to_ntime( &(hdr.stime), &curntime, status );
	if  (Severe(status))  return;
	tmp = tc_ndiff( &curntime, &reqntime, status );
	if  (Severe(status))  return;
	tmp /= (*dt);  /* difference in samples */
	tmp /= meansmp;  /* difference in records */
	setpos -= (long)(tmp+1)*GRN_RECLTH;
	if  (setpos < 0)  setpos = 0;
	grn_readlog( "-" );
	fstat = fseek( grn, setpos, 0 );
	if  (fstat != 0)  {*status = GRNE_SETPOS; return;}

	/* try this record */
	grn_readlog( "." );
	ptsread = fread( datrec, 1, GRN_RECLTH, grn );
	if  (ptsread != GRN_RECLTH)  {*status = GRNE_FREAD; return;}
	/* extract info */
	grn_analyse_header( datrec, swap, &hdr, status );
	if  (Severe(status))  return;
	if  (grn_cmp_btime(&(hdr.stime),reqbtime) < 0)  {
		grn_readlog( "-" );
		fseek( grn, setpos, 0 );
		return;
	} /*endif*/

	/* couldn't find start, give up and rewind file         */
	/* maybe I should make some more efforts at this place, */
	/* but not today ...                                    */
	grn_warning( "couldn't jump to record, rewinding file" );
	grn_readlog( "-" );
	fseek( grn, 0L, 0 );

} /* end of grn_jump_records */



/*----------------------------------------------------------------------------*/



static void grn_decode( SBYTE rec1[], SBYTE rec2[], FILE *grn, BOOLEAN swap, float dt,
	BTIME *reqbtime, long length, long smp[], long *outlth,
	BTIME *rettime, STATUS *status )

/* decodes data.  If rec2 == NULL then there is only one record (rec1) in this
 * file available
 *
 * parameters of routine
 * SBYTE       rec1[];    input; first logical record
 * SBYTE       rec2[];    input; second logical record
 * FILE       *grn;      input; data file (for all following records)
 * BOOLEAN    swap;      input; swap bytes
 * float      dt;        input; sample distance in sec
 * BTIME      *reqbtime; input; requested time
 * long       length;    input; number of samples to read
 * long       smp[];     output; samples
 * long       *outlth;   output; number of samples read
 * BTIME      *rettime;  output; actual time of first sample returned
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	SBYTE       datrec[GRN_RECLTH];   /* logical data record */
	GRN_DATHDR hdr;        /* data header */
	NTIME      reqntime;   /* requested (numerical) time */
	NTIME      curntime;   /* current (numerical) time */
	NTIME      endntime;   /* end of current record */
	float      tmp;        /* scratch */
	long       firstsmp;   /* first sample to copy to output array */
	SBYTE      *frame;     /* frame pointer */
	int        framecnt;   /* frame counter */
	long       reccnt;     /* record counter */
	long       prev_seq;   /* previous sequence number */
	long       x0, xn;     /* integration constants */
	static long prev_xn;   /* previous xn */
	int        declth;     /* length after decoding */
	long       diffs[GRN_MAXFRAMESMP];  /* differences of one frame */
	long       rsampleno;  /* number of sample in record */
	long       fsampleno;  /* number of sample in frame */
	long       cursmp;     /* current sample */
	long       ptsread;    /* number of points read */
	char       str[BC_LINELTH+1]; /* scratch string */
	long       i;          /* counter */

	/* executable code */

	if  (length <= 0)  {*status = GRNE_ZEROLTH; return;}
	*outlth = 0;

	grn_analyse_header( rec1, swap, &hdr, status );
	if  (Severe(status))  return;
	grn_btime_to_ntime( reqbtime, &reqntime, status );
	if  (Severe(status))  return;
	grn_btime_to_ntime( &(hdr.stime), &curntime, status );
	if  (Severe(status))  return;
	tmp = tc_ndiff( &reqntime, &curntime, status ); /* time diff */
	if  (Severe(status))  return;
	firstsmp = Nlong(tmp/dt);
	if  (firstsmp > 0 && firstsmp > hdr.no_of_samples)  {
		*status = GRNE_BUG;
		return;
	}/*endif*/

	if  (firstsmp < 0)  {
		sprintf( str, "\n--- requested time before start, padding %ld zeroes",
			-firstsmp );
		grn_warning( str );
		while  (firstsmp++ < 0)  {  /* pad zeroes */
			*smp++ = 0;
			if  (++(*outlth) == length)  {
				*rettime = *reqbtime;
				return;
			} /*endif*/
		} /*endwhile*/
		firstsmp = 0;
		*rettime = *reqbtime;
	} else {
		/* get start time of first returned sample */
		tmp = (float)firstsmp*dt;   /* time wdw to first returned sample */
		tmp += (float)hdr.correction/10000.0;
		tc_nadd( &curntime, tmp, &reqntime, status );
		if  (Severe(status))  return;
		grn_ntime_to_btime( &reqntime, rettime, status );
		if  (Severe(status))  return;
	} /*endif*/

	prev_xn = 0xfffffffL;
	prev_seq = 0;
	reccnt = 1;

	grn_readlog( "\b\b**" );

	FOREVER  {

		if  (reccnt == 1)  {
			frame = rec1;
		} else if  (reccnt == 2)  {
			if  (rec2 == NULL)  {*status = GRNE_EOF; return;}
			frame = rec2;
			grn_analyse_header( rec2, swap, &hdr, status );
			if  (Severe(status))  return;
		} else {
			grn_readlog( "*" );
			ptsread = fread( datrec, 1, GRN_RECLTH, grn );
			if  (ptsread == 0)  {*status = GRNE_EOF; return;}
			if  (ptsread != GRN_RECLTH)  {*status = GRNE_FREAD; return;}
			/* extract info */
			grn_analyse_header( datrec, swap, &hdr, status );
			if  (Severe(status))  return;
			frame = datrec;
		} /*endif*/
		rsampleno = 0;

		/* check sequence number */
		if  (prev_seq != 0)  {
			if  (prev_seq+1 != hdr.seqno)  {
				sprintf( str, "\n--- sequence error %ld follows %ld",
					hdr.seqno, prev_seq );
				grn_warning( str );
				grn_printhdr( &hdr );
			} /*endif*/
		} /*endif*/

		/* check absolute time of record */
		grn_btime_to_ntime( &(hdr.stime), &curntime, status );
		if  (Severe(status))  return;
		if  (reccnt > 1)  {
			tmp = tc_ndiff( &curntime, &endntime, status );
			if  (Severe(status))  return;
			if  (Abs(tmp) >= dt)  {
				if  (tmp > 0)  {
					sprintf( str, "\n--- time gap of %f s between records %ld and %ld",
						tmp, prev_seq, hdr.seqno );
					grn_warning( str );
					grn_printhdr( &hdr );
					/* insert samples */
					ptsread = Nlong( tmp/dt );
					sprintf( str, "--- inserting %ld zero samples", ptsread );
					grn_warning( str );
					for  (i=0; i<ptsread; i++)  {
						*smp++ = 0;
						if  (++(*outlth) == length)  {
							sprintf( str, "--- insertion aborted after %ld zeroes",
								i+1 );
							grn_warning( str );
							return;
						} /*endif*/
					} /*endfor*/
				} else {
					sprintf( str, "\n--- time overlap of %f s between records %ld and %ld",
						tmp, prev_seq, hdr.seqno );
					grn_warning( str );
					grn_printhdr( &hdr );
					ptsread = Nlong( -tmp/dt );
					sprintf( str, "--- deleting %ld samples", ptsread );
					grn_warning( str );
					if  (*outlth > ptsread)  {     /* remove "ptsread" points from samples */
						smp -= ptsread;
						*outlth -= ptsread;
					} else {                       /* remove all available samples */
						smp -= *outlth;
						*outlth = 0;
					} /*endif*/
				} /*endif*/
			} /*endif*/
		} /*endif*/
		tc_nadd( &curntime, (float)hdr.no_of_samples*dt, &endntime, status );
		if  (Severe(status))  return;

		/* loop over all frames in record (first frame is header) */
		for  (framecnt=1; framecnt<GRN_FRAMERECS; framecnt++)  {

			frame += GRN_FRAMELTH;

			/* get integration constants */
			if  (framecnt == 1)  {
				x0 = grn_get4bytes( frame+4, swap );
				xn = grn_get4bytes( frame+8, swap );
			} /*endif*/

			/* get all differences of this frame */
			grn_decode_frame( frame, swap, diffs, &declth );

			/* check difference to last record */
			if  (framecnt == 1)  {
				if  (prev_xn+diffs[0] != x0  &&
					prev_xn != 0xfffffffL)  {
					sprintf( str, "\n--- first sample in record %ld: wrong diff",
						hdr.seqno );
					grn_warning( str );
					sprintf( str, "\n    should be %d, is %d", x0, prev_xn+diffs[0] );
					grn_warning( str );
					grn_printhdr( &hdr );
				} /*endif*/
				cursmp = x0 - diffs[0];
			} /*endif*/

			/* integrate samples and copy to output array */
			for  (fsampleno=0; fsampleno<declth; fsampleno++)  {
				cursmp += diffs[fsampleno];
				if  (firstsmp <= rsampleno++)  {
					*smp++ = cursmp;
					if  (++(*outlth) == length)  return;
				} /*endif*/
				if  (rsampleno == hdr.no_of_samples)  break;
			} /*endfor*/

			/* if all samples of this record are decoded, exit this record */
			if  (rsampleno == hdr.no_of_samples)  break;

		} /*endfor*/

		if  (cursmp != xn)  {
			sprintf( str, "\n--- checksum not ok in record %ld", hdr.seqno );
			grn_warning( str );
			grn_printhdr( &hdr );
		} /*endif*/

		if  (rsampleno != hdr.no_of_samples)  {
			sprintf( str, "\n--- %ld samples decoded instead of %d, record %ld",
				rsampleno, hdr.no_of_samples, hdr.seqno );
			grn_warning( str );
			grn_printhdr( &hdr );
		} /*endif*/

		prev_xn = xn;
		prev_seq = hdr.seqno;
		firstsmp = 0;
		reccnt++;

	} /*endfor*/

} /* end of grn_decode */



/*----------------------------------------------------------------------------*/



static void grn_decode_frame( SBYTE *frame, BOOLEAN swap, long diff[], int *lth )

/* decodes differences of a single frame
 *
 * parameters of routine
 * SBYTE       *frame;     input; byte stream
 * long       diff[];     output; decoded differences
 * int        *lth;       output; number of differences
 */
{
	/* local variables */
	unsigned long   nibbles;    /* 2-bit nibbles */
	int             diffcnt;    /* difference counter */
	int             shift;      /* bit shift length */
	int             diff_id;    /* difference 2-bit ID (0,1,2,3) */
	short int       si;         /* scratch */

	/* executable code */

	nibbles = grn_get4bytes( frame, swap );
	*lth = 0;
	for  (diffcnt=0; diffcnt<GRN_FRAMEDIFFS; diffcnt++)  {
		shift = 30 - diffcnt*2;
		diff_id = (int)((nibbles >> shift) & 3);
		switch  (diff_id)  {
		case 1:
			diff[(*lth)++] = *frame++;
			diff[(*lth)++] = *frame++;
			diff[(*lth)++] = *frame++;
			diff[(*lth)++] = *frame++;
			break;
		case 2:
			si = grn_get2bytes( frame, swap );
			diff[(*lth)++] = si;
			frame += 2;
			si = grn_get2bytes( frame, swap );
			diff[(*lth)++] = si;
			frame += 2;
#			ifdef XXX
			diff[(*lth)++] = (int)grn_get2bytes( frame, swap );
			frame += 2;
			diff[(*lth)++] = (int)grn_get2bytes( frame, swap );
			frame += 2;
#			endif
			break;
		case 3:
			diff[(*lth)++] = grn_get4bytes( frame, swap );
			frame += 4;
			break;
		default:
			frame += 4;  /* no data */
		} /*endswitch*/
	} /*endfor*/

} /* end of grn_decode_frame */



/*----------------------------------------------------------------------------*/



void grn_name_to_time( char station[], char grnname[], TIME *time,
	STATUS *status )

/* converts GRN filename to time value
 *
 * parameters of routine
 * char       station[];     input; station name
 * char       grnname[];     input; GRN filename
 * TIME       *time;         output; start time
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	int      slen;                    /* string length */
	char     str[BC_SHORTSTRLTH+1];   /* scratch string */
	NTIME    ntime;                   /* numeric time */

	/* executable code */

#	ifdef XXX
	if  (strncmp(grnname,"gr",2) == 0 || strncmp(grnname,"GR",2) == 0)  {
		grnname += 4;
	} else {
		while  (isalpha(*grnname))
			grnname++;
	} /*endif*/
#	endif

	slen = (int)strlen( station );
	if  (slen > 10)  {
		printf( "--> *** warning: long station name: length is %d ***\n", slen );
	} else if  (slen > 4)  {
		printf( "--> warning: station name length is %d\n", slen );
	} /*endif*/
	grnname += slen;

	if  (*grnname == '_')  grnname++;

	str[2] = '\0';
	str[0] = *grnname++;  str[1] = *grnname++;
	sscanf( str, "%d", &(ntime.year) );
	ntime.year += 1900;
	if  (ntime.year < 1950)  ntime.year += 100;
	str[0] = *grnname++;  str[1] = *grnname++;
	sscanf( str, "%d", &(ntime.month) );
	str[0] = *grnname++;  str[1] = *grnname++;
	sscanf( str, "%d", &(ntime.day) );

	if  (*grnname == '.')  {
		ntime.hour = 0;
		ntime.min = 0;
		ntime.sec = 0;
	} else {
		if  (*grnname == '_')  grnname++;
		str[0] = *grnname++;  str[1] = *grnname++;
		sscanf( str, "%d", &(ntime.hour) );
		str[0] = *grnname++;  str[1] = *grnname++;
		sscanf( str, "%d", &(ntime.min) );
		if  (*grnname == '.')  {
			ntime.sec = 0;
		} else {
			str[0] = *grnname++;  str[1] = *grnname++;
			sscanf( str, "%d", &(ntime.sec) );
		} /*endif*/
	} /*endif*/
	ntime.ms = 0;

	tc_n2a( &ntime, time, status );

} /* end of grn_name_to_time */



/*--------------------------------------------------------------------*/



void grn_findgrnfiles( char device[], char label[], char station[], char chan[],
	TIME *start, int filenum, char flist[][BC_FILELTH+1], STATUS *status )

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
 * char       device[];   input; WORM device name
 * char       label[];    input; disk label
 * char       station[];  input; name of station
 * char       chan[];     input; data channel (for example HHZ)
 * TIME       *start;     input; start time to be searched
 * int        filenum;    input; number of files to be returned
 * char       flist[][];  output; list of files
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	char     glsname[BC_FILELTH+1];  /* name of directory file */
	FILE     *fp;                    /* file pointer */
	char     line[BC_FILELTH+1];     /* current line */
	TIME     ctime;                  /* current time */
	int      i;                      /* counter */
	char     cstat[BC_SHORTSTRLTH+1];/* current station name */
	char     cchan[BC_SHORTSTRLTH+1];/* current channel */
	BOOLEAN  no_file_found;          /* no file found */

	/* executable code */

	if  (*chan != 'H' && *chan != 'B' && *chan != 'L')  {
		*status = GRNE_ILLSMPRATE;
		return;
	} /*endif*/

	if  (*label == '\0')  {
#		ifdef BC_VAX                                   /* on the VAX, the list */
		strcpy( glsname, grnv_glsdir );                /* file is on the same  */
		strcat( glsname, device );                     /* directory as all     */
		i = (int)strlen( glsname ) - 1;                /* other list files     */
		if  (glsname[i] == EODEV)  glsname[i] = '\0';  /*                      */
		strcat( glsname, "_dirfile" );                 /*                      */
#		else
		strcpy( glsname, device );               /* this is different on other */
		i = (int)strlen( glsname ) - 1;          /* machines.  Here the list   */
		if  (glsname[i] != EODEV &&              /* file is on the data        */
			glsname[i] != ':')  {            /* directory                  */          
			glsname[i+1] = EODEV;            /*                            */
			glsname[i+2] = '\0';             /*                            */
		} /*endif*/                              /*                            */
		strcat( glsname, "dirfile" );            /*                            */
#		endif
		strcat( glsname, GRN_LISTFILEEXT );
	} else {
		strcpy( glsname, grnv_glsdir );
		strcat( glsname, label );
		strcat( glsname, GRN_LISTFILEEXT );
	} /*endif*/
	fp = sy_fopen( glsname, "r" );
	if  (fp == NULL)  {
		*status = GRNE_OPNGLS;
		return;
	} /*endif*/

	for  (i=0; i<filenum; i++)
		flist[i][0] = '\0';

	/* find station and channel */
	FOREVER  {
		if  (fgets(line,BC_FILELTH,fp) == NULL)  {
			sy_fclose( fp );
			*status = GRNE_SMPSTATNF;
			return;
		} /*endif*/
		if  (*line == '*')  {
			sscanf( line+1, "%s %s", cstat, cchan );
			if  (strcmp(station,cstat) == 0  &&
				strcmp(chan,cchan) == 0)
				break;
		} /*endif*/
	} /*endfor*/

	/* find files */
	no_file_found = FALSE;
	*line = '\0';
	do  {
		strcpy( flist[0], line );
		if  (fgets(line,BC_FILELTH,fp) == NULL)  {
			no_file_found = TRUE;
			break;
		} else if  (*line == '*')  {
			no_file_found = TRUE;
			break;
		} /*endif*/
		grn_name_to_time( station, line, &ctime, status );
		if  (Severe(status))  {sy_fclose(fp); return;}
	}  while (tc_adiff(start,&ctime) >= 0.0);

	if  (flist[0][0] == '\0')  {
		if  (!no_file_found)
			*status = GRNE_WRONGVOL;
		sy_fclose( fp );
		return;
	} /*endif*/
	ut_uncap( flist[0] );

	if  (!no_file_found)  {
		/* line contains first subsequent files */
		strcpy( flist[1], line );
		ut_uncap( flist[1] );
		/* read all other subsequent files */
		for  (i=2; i<filenum; i++)
			if  (fgets(flist[i],BC_FILELTH,fp) == NULL)  {
				flist[i][0] = '\0';   /* EOF found */
			} else if  (flist[i][0] == '*')  {
				flist[i][0] = '\0';   /* next station&smprate */
			} else {
				ut_uncap( flist[i] );
			} /*endif*/
	} /*endif*/

	sy_fclose( fp );

	/* put device and directory at the beginning of filenames */
	strcpy( glsname, device );
	if  (*label != '\0')  {  /* on WORM disk the path must be generated */
#		ifdef BC_VAX
		if  (*chan == 'H')  strcat( glsname, "[GRN_80." );
		else if  (*chan == 'B')  strcat( glsname, "[GRN_20." );
		else  strcat( glsname, "[GRN_01." );
		strcat( glsname, station );
		strcat( glsname, "]" );
#		else    /* different file syntax in UNIX */
		if  (*chan == 'H')  strcat( glsname, "grn_80/" );
		else if  (*chan == 'B')  strcat( glsname, "grn_20/" );
		else  strcat( glsname, "grn_01/" );
		strcat( glsname, station );
		strcat( glsname, "/" );
#		endif   /* no need for ATARI here */
	} /*endif*/
	for  (i=0; i<filenum; i++)  {
		if  (flist[i][0] != '\0')  {
			grn_strip_blanks( flist[i] );
			strcpy( line, flist[i] );
			strcpy( flist[i], glsname );
			strcat( flist[i], line );
		} /*endif*/
	} /*endfor*/

} /* end of grn_findgrnfiles */



/*----------------------------------------------------------------------------*/



static void grn_strip_blanks( char str[] )

/* removes blanks at the end of "str"
 *
 * parameter of routine
 * char      str[];     modify; string to remove blanks at the end
 */
{
	/* local variables */
	int      slen;       /* string length */

	/* executable code */

	slen = (int)strlen( str ) - 1;
	while  (str[slen] <= ' ')  {
		str[slen--] = '\0';
		if  (slen < 0)  return;
	} /*endwhile*/

} /* end of grn_strip_blanks */



/*----------------------------------------------------------------------------*/



static void grn_printhdr( GRN_DATHDR *hdr )

/* Prints out header information.  Used in error messages.
 *
 * parameter of routine
 * GRN_DATHDR   *hdr;    input; data header to dump
 */
{
	/* local variables */
	char     line[BC_LINELTH+1];     /* scratch string */
	char     timestr[BC_LINELTH+1];  /* time string */
	NTIME    ntime;                  /* numeric time */
	STATUS   locstat=BC_NOERROR;     /* local status */

	/* executable code */

	grn_btime_to_ntime( &(hdr->stime), &ntime, &locstat );
	if  (locstat == BC_NOERROR)  {
		tc_n2t( &ntime, timestr, &locstat );
		if  (locstat != BC_NOERROR)
			strcpy( timestr, "couldn't decode time" );
	} else {
		strcpy( timestr, "couldn't decode time" );
	} /*endif*/
	sprintf( line, "   hdr: no %ld, station %s, channel %s, time %s",
		hdr->seqno, hdr->station, hdr->channel, timestr );
	grn_warning( line );

} /* end of grn_printheader */



/*----------------------------------------------------------------------------*/



void grn_getlabel( char timestr[], int maxlth, char label[], STATUS *status )

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
{
	/* local variables */
	FILE     *fp;                 /* file pointer */
	char     line[BC_LINELTH+1];  /* current line */
	char     ts1[BC_LINELTH+1];   /* start time of volume */
	char     ts2[BC_LINELTH+1];   /* end time of volume */
	char     loclab[BC_LINELTH+1];/* local label string */
	TIME     tsearch;             /* time to be searched */
	TIME     t1, t2;              /* volume limits */

	/* executable code */

	tc_t2a( timestr, &tsearch, status );
	if  (Severe(status))  return;

	fp = sy_fopen( grnv_labelfile, "r" );
	if  (fp == NULL)  {
		*status = GRNE_NOLABELFILE;
		return;
	} /*endif*/

	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		sscanf( line, "%s %s %s", ts1, ts2, loclab );
		tc_t2a( ts1, &t1, status );
		if  (Severe(status))  {sy_fclose(fp); return;}
		tc_t2a( ts2, &t2, status );
		if  (Severe(status))  {sy_fclose(fp); return;}
		if  ((tc_adiff(&t1,&tsearch) <= 0.0)
			&& (tc_adiff(&t2,&tsearch) >= 0.0))  {
			if  (strlen(loclab) > maxlth)  {
				*status = GRNE_STROVFL;
				return;
			} /*endif*/
			sy_fclose( fp );
			strcpy( label, loclab );
			return;
		} /*endif*/
	} /*endwhile*/

	*status = GRNE_LABNOTIME;
	sy_fclose( fp );

} /* end of grn_getlabel */



/*----------------------------------------------------------------------------*/



void grn_parse_stations( char list[],
	char stat[GRNC_MAXSTATION][GRNC_STATNAMELTH+1], int *statno )

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
{
	/* local variables */
	char     llist[cBcVeryLongStrLth+1];  /* local copy of list */
	char     *llp;                 /* pointer to local list */
	char     *cptr;                /* pointer to comma */
	char     lfname[cBcFileLth+1]; /* name of list file */
	FILE     *fp;                  /* pointer to file */
	char     line[cBcLineLth+1];   /* current line of file */
	int      slen;                 /* string length */

	/* executable code */

	if  (strlen(list) > cBcVeryLongStrLth)  {
		strcpy( llist, "LIST TOO LONG" );
	} else {
		strcpy( llist, list );
		ut_cap( llist );
	} /*endif*/
	llp = llist;

	if  (strcmp(llist,"ALL") == 0)  {
		*statno = 0;
		strcpy( stat[(*statno)++], "BFO" );
		strcpy( stat[(*statno)++], "BRG" );
		strcpy( stat[(*statno)++], "BRNL" );
		strcpy( stat[(*statno)++], "BSEG" );
		strcpy( stat[(*statno)++], "BUG" );
		strcpy( stat[(*statno)++], "CLL" );
		strcpy( stat[(*statno)++], "CLZ" );
		strcpy( stat[(*statno)++], "FBE" );
		strcpy( stat[(*statno)++], "FUR" );
		strcpy( stat[(*statno)++], "GEC2" );
		strcpy( stat[(*statno)++], "GRFO" );
		strcpy( stat[(*statno)++], "GSH" );
		strcpy( stat[(*statno)++], "GTTN" );
		strcpy( stat[(*statno)++], "GUNZ" );
		strcpy( stat[(*statno)++], "HAM" );
		strcpy( stat[(*statno)++], "HLG" );
		strcpy( stat[(*statno)++], "IBBN" );
		strcpy( stat[(*statno)++], "LID" );
		strcpy( stat[(*statno)++], "MANZ" );
		strcpy( stat[(*statno)++], "MOX" );
		strcpy( stat[(*statno)++], "NEUB" );
		strcpy( stat[(*statno)++], "NOTT" );
		strcpy( stat[(*statno)++], "NRDL" );
		strcpy( stat[(*statno)++], "RGN" );
		strcpy( stat[(*statno)++], "ROTZ" );
		strcpy( stat[(*statno)++], "RUE" );
		strcpy( stat[(*statno)++], "STU" );
		strcpy( stat[(*statno)++], "TANN" );
		strcpy( stat[(*statno)++], "TNS" );
		strcpy( stat[(*statno)++], "UBBA" );
		strcpy( stat[(*statno)++], "WERN" );
		strcpy( stat[(*statno)++], "WERD" );
		strcpy( stat[(*statno)++], "WET" );
		return;
	} else if  (strcmp(llist,"GRSN") == 0)  {
		*statno = 0;
		strcpy( stat[(*statno)++], "BFO" );
		strcpy( stat[(*statno)++], "BRG" );
		strcpy( stat[(*statno)++], "BRNL" );
		strcpy( stat[(*statno)++], "BSEG" );
		strcpy( stat[(*statno)++], "BUG" );
		strcpy( stat[(*statno)++], "CLL" );
		strcpy( stat[(*statno)++], "CLZ" );
		strcpy( stat[(*statno)++], "FUR" );
		strcpy( stat[(*statno)++], "GEC2" );
		strcpy( stat[(*statno)++], "GRFO" );
		strcpy( stat[(*statno)++], "GSH" );
		strcpy( stat[(*statno)++], "GTTN" );
		strcpy( stat[(*statno)++], "HAM" );
		strcpy( stat[(*statno)++], "HLG" );
		strcpy( stat[(*statno)++], "IBBN" );
		strcpy( stat[(*statno)++], "LID" );
		strcpy( stat[(*statno)++], "MOX" );
		strcpy( stat[(*statno)++], "NOTT" );
		strcpy( stat[(*statno)++], "NRDL" );
		strcpy( stat[(*statno)++], "RGN" );
		strcpy( stat[(*statno)++], "RUE" );
		strcpy( stat[(*statno)++], "STU" );
		strcpy( stat[(*statno)++], "TNS" );
		strcpy( stat[(*statno)++], "UBBA" );
		strcpy( stat[(*statno)++], "WET" );
		return;
	} else if  (strcmp(llist,"SXNET") == 0)  {
		*statno = 0;
		strcpy( stat[(*statno)++], "GUNZ" );
		strcpy( stat[(*statno)++], "NEUB" );
		strcpy( stat[(*statno)++], "TANN" );
		strcpy( stat[(*statno)++], "WERD" );
		return;
	} else if  (strcmp(llist,"GOOD") == 0)  {
		*statno = 0;
		strcpy( stat[(*statno)++], "BFO" );
		strcpy( stat[(*statno)++], "BRG" );
		strcpy( stat[(*statno)++], "BUG" );
		strcpy( stat[(*statno)++], "CLL" );
		strcpy( stat[(*statno)++], "CLZ" );
		strcpy( stat[(*statno)++], "FUR" );
		strcpy( stat[(*statno)++], "GEC2" );
		strcpy( stat[(*statno)++], "TNS" );
		strcpy( stat[(*statno)++], "WET" );
		return;
	} else if  (strcmp(llist,"BEST") == 0)  {
		*statno = 0;
		strcpy( stat[(*statno)++], "BFO" );
		strcpy( stat[(*statno)++], "CLZ" );
		strcpy( stat[(*statno)++], "TNS" );
		strcpy( stat[(*statno)++], "WET" );
		return;
	} else if  (strcmp(llist,"GRF") == 0)  {
		*statno = 0;
		strcpy( stat[(*statno)++], "GRA1" );
		strcpy( stat[(*statno)++], "GRA2" );
		strcpy( stat[(*statno)++], "GRA3" );
		strcpy( stat[(*statno)++], "GRA4" );
		strcpy( stat[(*statno)++], "GRB1" );
		strcpy( stat[(*statno)++], "GRB2" );
		strcpy( stat[(*statno)++], "GRB3" );
		strcpy( stat[(*statno)++], "GRB4" );
		strcpy( stat[(*statno)++], "GRB5" );
		strcpy( stat[(*statno)++], "GRC1" );
		strcpy( stat[(*statno)++], "GRC2" );
		strcpy( stat[(*statno)++], "GRC3" );
		strcpy( stat[(*statno)++], "GRC4" );
		return;
	} else if  (*llist == '_')  {
		*statno = 0;
		/* build list filename */
		sprintf( lfname, "%s%s.STA", shd_globals, llist );
		fp = fopen( lfname, "r" );
		if  (fp == NULL)  {
			fprintf( stderr, "*SH: input file %s not found\n", lfname );
			return;
		} /*endif*/
		while  (fgets(line,cBcLineLth,fp) != NULL)  {
			slen = strlen( line ) - 1;
			if  (line[slen] == '\n')  line[slen] = '\0';
			if  (strlen(line) > GRNC_STATNAMELTH)  {
				fprintf( stderr, "*SH: station name %s too long; ignored\n", line );
				continue;
			} /*endif*/
			if  (*statno == GRNC_MAXSTATION)  {
				fprintf( stderr, "*SH: too many stations in %s\n", lfname );
				*statno = 0;
				fclose( fp );
				return;
			} /*endif*/
			strcpy( stat[(*statno)++], line );
		} /*endwhile*/
		fclose( fp );
		return;
	} /*endif*/

	*statno = 0;
	FOREVER  {
		cptr = grn_findchar(llp,',');
		if  (cptr == NULL) break;
		*cptr = '\0';
		if  (strlen(llp) <= GRNC_STATNAMELTH)  {
			if  (*statno == GRNC_MAXSTATION)  {
				printf( "--> grn_parse_stations: too many stations\n" );
			} else {
				strcpy( stat[(*statno)++], llp );
			} /*endif*/
		} /*endif*/
		llp = cptr + 1;
	} /*endwhile*/

	if  (*statno < GRNC_MAXSTATION)  {
		if  (strlen(llp) <= GRNC_STATNAMELTH && strlen(llp) > 0)
			strcpy( stat[(*statno)++], llp );
	} else {
		printf( "--> grn_parse_stations: too many stations\n" );
	} /*endif*/

} /* end of grn_parse_stations */



/*----------------------------------------------------------------------------*/



static char *grn_findchar( char str[], char c )

/* finds character in string
 *
 * parameters of routine
 * char       str[];     input; string to be searched
 *                       returns pointer to character or NULL
 */
{
	/* local variables */

	/* executable code */

	while  (*str != c && *str != '\0')
		str++;

	if  (*str == '\0')  return NULL;
	return str;

} /* end of grn_findchar */



/*----------------------------------------------------------------------------*/



void grn_get_station_id( char station[], int maxlth, char array[], int *arrid,
	int *id, STATUS *status )

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
{
	/* executable code */

	if  (maxlth < 5 && array != NULL)  {
		*status = GRNE_STROVFL;
		return;
	} /*endif*/

	if  (strncmp(station,"GR",2) == 0)  {
		if  (array != NULL)  strcpy( array, "GRF" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRF;
		if  (id == NULL)  return;
		if  (strcmp(station,"GRA1") == 0)  {
			*id = STC_GRA1;
		} else if  (strcmp(station,"GRA2") == 0)  {
			*id = STC_GRA2;
		} else if  (strcmp(station,"GRA3") == 0)  {
			*id = STC_GRA3;
		} else if  (strcmp(station,"GRA4") == 0)  {
			*id = STC_GRA4;
		} else if  (strcmp(station,"GRB1") == 0)  {
			*id = STC_GRB1;
		} else if  (strcmp(station,"GRB2") == 0)  {
			*id = STC_GRB2;
		} else if  (strcmp(station,"GRB3") == 0)  {
			*id = STC_GRB3;
		} else if  (strcmp(station,"GRB4") == 0)  {
			*id = STC_GRB4;
		} else if  (strcmp(station,"GRB5") == 0)  {
			*id = STC_GRB5;
		} else if  (strcmp(station,"GRC1") == 0)  {
			*id = STC_GRC1;
		} else if  (strcmp(station,"GRC2") == 0)  {
			*id = STC_GRC2;
		} else if  (strcmp(station,"GRC3") == 0)  {
			*id = STC_GRC3;
		} else if  (strcmp(station,"GRC4") == 0)  {
			*id = STC_GRC4;
		} else {
			*status = GRNE_BUG;
			return;
		} /*endif*/
	} else if  (strcmp(station,"BFO") == 0)  {
		if  (array != NULL)  strcpy( array, "GRN" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRSN;
		if  (id != NULL)  *id = STC_BFO;
	} else if  (strcmp(station,"BRG") == 0)  {
		if  (array != NULL)  strcpy( array, "GRN" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRSN;
		if  (id != NULL)  *id = STC_BRG;
	} else if  (strcmp(station,"BUG") == 0)  {
		if  (array != NULL)  strcpy( array, "GRN" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRSN;
		if  (id != NULL)  *id = STC_BUG;
	} else if  (strcmp(station,"CLL") == 0)  {
		if  (array != NULL)  strcpy( array, "GRN" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRSN;
		if  (id != NULL)  *id = STC_CLL;
	} else if  (strcmp(station,"CLZ") == 0)  {
		if  (array != NULL)  strcpy( array, "GRN" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRSN;
		if  (id != NULL)  *id = STC_CLZ;
	} else if  (strcmp(station,"FUR") == 0)  {
		if  (array != NULL)  strcpy( array, "GRN" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRSN;
		if  (id != NULL)  *id = STC_FUR;
	} else if  (strcmp(station,"HAM") == 0)  {
		if  (array != NULL)  strcpy( array, "GRN" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRSN;
		if  (id != NULL)  *id = STC_HAM;
	} else if  (strcmp(station,"MOX") == 0)  {
		if  (array != NULL)  strcpy( array, "GRN" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRSN;
		if  (id != NULL)  *id = STC_MOX;
	} else if  (strcmp(station,"TNS") == 0)  {
		if  (array != NULL)  strcpy( array, "GRN" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRSN;
		if  (id != NULL)  *id = STC_TNS;
	} else if  (strcmp(station,"WET") == 0)  {
		if  (array != NULL)  strcpy( array, "GRN" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRSN;
		if  (id != NULL)  *id = STC_WET;
	} else if  (strcmp(station,"LID") == 0)  {
		if  (array != NULL)  strcpy( array, "GRN" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRSN;
		if  (id != NULL)  *id = STC_LID;
	} else if  (strcmp(station,"BRNL") == 0)  {
		if  (array != NULL)  strcpy( array, "GRN" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRSN;
		if  (id != NULL)  *id = STC_BRNL;
	} else if  (strcmp(station,"GRFO") == 0)  {
		if  (array != NULL)  strcpy( array, "GRN" );
		if  (arrid != NULL)  *arrid = STC_ARRAY_GRSN;
		if  (id != NULL)  *id = STC_GRFO;
	} else {
		if  (array != NULL)  *array = '\0';
		*status = GRNE_UKSTATION;
	} /*endif*/

} /* end of grn_get_station_id */



/*----------------------------------------------------------------------------*/



static void grn_read_descriptor( char station[], char datafile[], STATUS *status )

/* reads in appropriate descriptor file to given data file "datafile".
 * Information is stored in "grnv_head" - array.  If no descriptor file
 * is found the validitation of the grnv_head elements is set to FALSE.
 * If the same descriptor file than the last time should be read in then
 * the routine just returns
 *
 * parameters of routine
 * char       station[];       input; station name
 * char       datafile[];      input; name of data file
 * STATUS     *status;         output; return status
 */
{
	/* local variables */
	char     arrname[BC_LINELTH+1];         /* array name */
	int      station_num;                   /* station id */
	int      array_num;                     /* array number */
	int      statlth;                       /* length of station name */
	char     dscname[BC_FILELTH+1];         /* name of descriptor file */
	static char last_dscname[BC_FILELTH+1]; /* last descriptorfile */
	FILE     *fp;                           /* file pointer */
	int      i;                             /* counter */
	char     line[BC_LINELTH+1];            /* current line */
	char     statnam[BC_LINELTH+1];         /* current station */
	char     lo_statnam[BC_LINELTH+1];      /* station name in lowercase */
	float    lat, lon;                      /* latitude & longitude of station */
	float    elev;                          /* elevation of station */
	float    gen;                           /* generator constant */
	/* float    x, y;     */                /* ??? may be relative location */
	char     *infoptr;                      /* pointer to info in line */
	char     *namptr;                       /* pointer to station name */
	BOOLEAN  try_insert;                    /* try to insert values in grnv_head */
	int      curr_comp;                     /* current component */
	float    d1, d2, d3, d4;                /* dummy */

	/* executable code */

	grn_get_station_id( station, BC_LINELTH, arrname, &array_num,
		&station_num, status );
	if  (Severe(status))  {
		/* reset station header entries */
		if  (array_num == STC_ARRAY_GRF)  {
			for  (i=STC_FIRST_GRF; i<=STC_LAST_GRF; i++)
				grnv_head[i].valid = FALSE;
		} else if  (array_num == STC_ARRAY_GRSN)  {
			for  (i=STC_FIRST_GRSN; i<=STC_LAST_GRSN; i++)
				grnv_head[i].valid = FALSE;
		} /*endif*/
		return;
	} /*endif*/


	statlth = strlen( station );
	if  (strlen(datafile)+strlen(arrname)-statlth > BC_FILELTH)  {
		*status = GRNE_STROVFL;
		return;
	}  else if  (strlen(station) > BC_LINELTH)  {
		*status = GRNE_STROVFL;
		return;
	} /*endif*/

	/* build descriptor name */
	ut_uncap( arrname );
	strcpy( lo_statnam, station );
	ut_uncap( lo_statnam );
	infoptr = strstr( datafile, lo_statnam );
	if  (infoptr == NULL)  {
		printf( "*** grn_read_descriptor: this cannot happen (1) ***\n" );
		*status = GRNE_BUG;
		return;
	} /*endif*/
	i = infoptr - datafile;
	if  (i > BC_LINELTH)  {
		printf( "*** grn_read_descriptor: this cannot happen (2) ***\n" );
		*status = GRNE_BUG;
		return;
	} /*endif*/
	strncpy( dscname, datafile, i );
	dscname[i] = '\0';
	strcat( dscname, arrname );
	strcat( dscname, infoptr+statlth );
	infoptr = dscname + strlen(dscname) - 1;
	while  (*infoptr != '.')
		if  (--infoptr < dscname)  {
			*status = GRNE_BUG;
			printf( "*** no period found in name %s ***\n", dscname );
			return;
		} /*endif*/
	strcpy( infoptr, ".dsc" );
	if  (strcmp(dscname,last_dscname) == 0)  return;  /* already read in */

	strcpy( last_dscname, dscname );

	/* reset station header entries */
	if  (array_num == STC_ARRAY_GRF)  {
		for  (i=STC_FIRST_GRF; i<=STC_LAST_GRF; i++)
			grnv_head[i].valid = FALSE;
	} else if  (array_num == STC_ARRAY_GRSN)  {
		for  (i=STC_FIRST_GRSN; i<=STC_LAST_GRSN; i++)
			grnv_head[i].valid = FALSE;
	} /*endif*/

	/* try to open descriptor file */
	fp = sy_fopen( dscname, "r" );
	if  (fp == NULL)  {
		if  (array_num == STC_ARRAY_GRSN)  {
			printf( "   no descriptor file: take default GRSN calibration\n" );
			for  (i=STC_FIRST_GRSN; i<=STC_LAST_GRSN; i++)  {
				grnv_head[i].valid = TRUE;
				grnv_head[i].calib[STC_COMP_Z] = GRN_GRSN_DEFAULT_CALIB;
				grnv_head[i].calib[STC_COMP_N] = GRN_GRSN_DEFAULT_CALIB;
				grnv_head[i].calib[STC_COMP_E] = GRN_GRSN_DEFAULT_CALIB;
			} /*endfor*/
		} else {
			printf( "   no descriptor file: use counts as amplitude\n" );
		} /*endif*/
		return;
	} /*endif*/

	/* read it ... */
	*statnam = '\0';
	gen = 0.0;
	try_insert = FALSE;
	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (NULL != strstr(line,"#STAT"))  {
			*statnam = '\0';
			gen = 0.0;
			try_insert = FALSE;
			curr_comp = STC_COMP_Z;
		} else if  (NULL != (infoptr = strstr(line,"#LOCA")))  {
			namptr = strchr( infoptr, ';' );
			if  (namptr++ == NULL)  {*status=GRNE_DSCFORMAT; sy_fclose(fp); return;}
			infoptr = strchr( namptr, ';' );
			if  (infoptr == NULL)  {*status=GRNE_DSCFORMAT; sy_fclose(fp); return;}
			i = infoptr - namptr;
			if  (i < 0)  {*status=GRNE_DSCFORMAT; sy_fclose(fp); return;}
			strncpy( statnam, namptr, i );
			statnam[i] = '\0';
			sscanf( infoptr+1, "%f;%f;%f", &lat, &lon, &elev );
			if  (gen != 0.0)  try_insert = TRUE;
		} else if  (NULL != (infoptr = strstr(line,"#CNTRL")))  {
			switch  (infoptr[9])  {
			case 'Z':   curr_comp = STC_COMP_Z;  break;
			case 'N':   curr_comp = STC_COMP_N;  break;
			case 'E':   curr_comp = STC_COMP_E;  break;
			default:
				printf( "*** grn_read_descriptor: component error ***\n" );
				*status = GRNE_DSCFORMAT;
				sy_fclose( fp );
				return;
			} /*endswitch*/
		} else if  (NULL != (infoptr = strstr(line,"#SEIS_N")))  {
			sscanf( infoptr, "#SEIS_N{%f;%f;%f;%f;%f",
				&d1, &d2, &d3, &d4, &gen );
			if  (gen <= 0.0)  {
				printf( "*** zero or negative calibration found ***\n" );
				gen = 1.0;
			} /*endif*/
			if  (*statnam != '\0')  try_insert = TRUE;
		} /*endif*/
		if  (try_insert)  {
			grn_get_station_id( statnam, 0, NULL, NULL,
				&station_num, status );
			if  (Severe(status))  {sy_fclose(fp); return;}
			if  (array_num == STC_ARRAY_GRF)  {
				grnv_head[station_num].valid = TRUE;
				grnv_head[station_num].calib[curr_comp] = 477.3 / gen;
			} else {
				printf( "--> this is not yet implemented\n" );
				*status = GRNE_NOTIMPL;
				sy_fclose( fp );
				return;
			} /*endif*/
			try_insert = FALSE;
		} /*endif*/
	} /*endif*/

	sy_fclose( fp );

} /* end of grn_read_descriptor */



/*----------------------------------------------------------------------------*/



void grn_set_vax_label_file( char fname[] )

/* changes VAX name of VAX label file
 *
 * parameters of routine
 * char       fname[];         input; name of label file
 */
{
	/* execitable code */

	if  (strlen(fname) < BC_FILELTH)
		strcpy( grnv_labelfile, fname );

} /* end of grn_set_vax_label_file */



/*----------------------------------------------------------------------------*/

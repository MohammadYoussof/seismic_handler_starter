
/* file q2gse.c
 *      =======
 *
 * version 13, 8-Feb-2003
 *
 * converts q-files into GSE file format
 * K. Stammler, 30-Oct-92
 *
 * v 8: 14-Mar-96, include calib-period in output
 * v 9: 12-Aug-96, change channel default 'B' to sample rate dependend value
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



#include <stdio.h>
#include <string.h>
#include <math.h>
#include "basecnst.h"
#ifdef BC_STDLIB_EX
#include <stdlib.h>
#endif
#include "sysbase.h"
#include "cpar.h"
#include "qfusrdef.h"
#include "qferrors.h"
#include "dfqentry.h"
#include "tcusrdef.h"
#include "erusrdef.h"
#include "glusrdef.h"
#include "inpfiles.h"



/* constants */
#define QFCALIB (26|QFC_RTYPE)
	/* calib entry in q-files */

#define PI 3.14159265358979323846


/* types */
typedef struct {
	char     fmt[BC_SHORTSTRLTH+1];   /* output format */
	char     channel;                 /* channel char (S,L,B) */
	int      diff;                    /* 0,1,2 differences */
	int      npl;                     /* numbers per line */
	float    excalib;                 /* external calibration */
	float    mul;                     /* multiplication factor */
	BOOLEAN  autocalib;               /* automatic calibration */
	char     wkind;                   /* 0=veloc, 1=displac, 2=accel */
	char     instr[BC_SHORTSTRLTH+1]; /* instrument */
} PARAMS;



/* prototypes of local routines */
static void qg_errstop( STATUS status );
static void qg_reformat( char qfile[], int trccnt, BOOLEAN dolog,
	PARAMS *par, FILE *gse, STATUS *status );
static float get_step( float dat[], long lth );
static void qg_include_file( FILE *gse, char ifile[] );



/*------------------------------------------------------------------*/



int main( int argc, char *argv[] )
{
	/* local variables */
	char     qfile[BC_FILELTH+1];      /* q-file name */
	char     gsefile[BC_FILELTH+1];    /* GSE file name */
	STATUS   status;                   /* return status */
	long     qsize;                    /* size of q-file */
	int      trcno;                    /* number of traces in q-file */
	int      trccnt;                   /* trace counter */
	FILE     *gse;                     /* pointer to GSE file */
	PARAMS   par;                      /* parameters */
	BOOLEAN  dolog;                    /* log traces */
	char     path[BC_FILELTH+1];       /* path to inputs directory */
	char     str[BC_LINELTH+1];        /* scratch string */
	char     ifile[BC_FILELTH+1];      /* include file */
#	ifdef BC_SUN
	char     *eptr;                    /* pointer to environment */
#	endif

	/* executable code */

	status = BC_NOERROR;

#	ifdef BC_SUN
	eptr = getenv( "SH_INPUTS" );
	if  (eptr != NULL)  {
		strcpy( path, eptr );
	} else {
		*path = '\0';
	} /*endif*/
#	else
	strcpy( path, IF_PATH );
#	endif

	strcpy( str, path );
	strcat( str, IF_STATINFFILE );
	gl_locfile_name( str );
	status = BC_NOERROR;

	/* parameter defaults */
	strcpy( par.fmt, "INTV" );
	par.channel = ' ';  /* changed later when sample rate is known */
	par.diff = 0;
	par.npl = 8;
	par.excalib = 1.0;
	par.mul = 1.0;
	par.autocalib = FALSE;
	par.wkind = '1';
	dolog = FALSE;

	pa_init( argc, argv );
	if  (pa_pnumber() != 2)  {
		printf( "*** Usage: q2gse <input> <output> ***\n" );
		printf( "    Qualifiers:\n" );
		printf( "      -a          autocalibration on\n" );
		printf( "      -m=<mul>    multiplication factor <mul> (default 1.0)\n" );
		printf( "      -e=<excal>  external calibration <excal> (default 1.0)\n" );
		printf( "      -c=<chan>   channel code <chan> (default B)\n" );
		printf( "      -n=<npl>    numbers/line (default 8)\n" );
		printf( "      -d=<diff>   differences (0,1,2, default 0)\n" );
		printf( "      -f=<fmt>    output format <fmt>\n" );
		printf( "      -k=<kind>   0=veloc, 1=displ, 2=accel, x=empty\n" );
		printf( "      -i=<file>   include file <file>\n" );
		printf( "      -s=<system> seismometer type <system>\n" );
		printf( "      -l          log current trace\n" );
		return 0;
	} /*endif*/

	strcpy( qfile, pa_pvalue(1) );
	strcpy( gsefile, pa_pvalue(2) );

	if  (pa_qspecified("-a"))
		par.autocalib = TRUE;

	if  (pa_qspecified("-m"))
		sscanf( pa_qvalue("-m"), "%f", &(par.mul) );

	if  (pa_qspecified("-e"))
		sscanf( pa_qvalue("-e"), "%f", &(par.excalib) );

	if  (pa_qspecified("-c"))
		par.channel = *pa_qvalue("-c");

	if  (pa_qspecified("-n"))
		sscanf( pa_qvalue("-n"), "%d", &(par.npl) );

	if  (pa_qspecified("-d"))
		sscanf( pa_qvalue("-d"), "%d", &(par.diff) );

	if  (pa_qspecified("-k"))  {
		par.wkind = *pa_qvalue("-k");
		if  (par.wkind == 'x' || par.wkind == 'X')
			par.wkind = ' ';
	} /*endif*/

	if  (pa_qspecified("-f"))
		strcpy( par.fmt, pa_qvalue("-f") );

	if  (pa_qspecified("-s"))  {
		strcpy( par.instr, pa_qvalue("-s") );
		if  (strlen(par.instr) > 6)  {
			par.instr[6] = '\0';
			printf( "*** instrument name truncated to %s ***\n",
				par.instr );
		} /*endif*/
	} else {
		strcpy( par.instr, "      " );
	} /*endif*/

	*ifile = '\0';
	if  (pa_qspecified("-i"))
		strcpy( ifile, pa_qvalue("-i") );

	dolog = pa_qspecified("-l");

	qf_size( qfile, &qsize, &trcno, &trccnt, &status );
	if  (Severe(&status))  qg_errstop( status );

	gse = sy_fopen( gsefile, "w" );
	if  (gse == NULL)  {
		printf( "*** couldn't open GSE file %s ***\n", gsefile );
		qg_errstop( status );
	} /*endif*/
	fprintf( gse, "XW01\n\n" );

	/* include file if specified */
	if  (*ifile != '\0')  {
		if  (dolog)
			printf( "   <include file %s>\n", ifile );
		qg_include_file( gse, ifile );
	} /*endif*/

	for  (trccnt=1; trccnt<=trcno; trccnt++)  {
		if  (dolog)
			printf( "   file %s, trace %d  ", qfile, trccnt );
		qg_reformat( qfile, trccnt, dolog, &par, gse, &status );
		if  (dolog)
			printf( "\n" );
		if  (Severe(&status))
			qg_errstop( status );
	} /*endfor*/

	fprintf( gse, "\nSTOP\n" );
	sy_fclose( gse );

	return 0;

} /* end of main */



/*------------------------------------------------------------------*/



static void qg_errstop( STATUS status )

/* displays error message and aborts program
 *
 * parameters of routine
 * STATUS     status;     input; status code
 */
{
	/* local variables */
	char     msg[BC_LINELTH+1];    /* error message */

	/* executable code */

	err_msg( status, msg );
	printf( "%s\n", msg );
	exit( 1 );

} /* end of qg_errstop */



/*------------------------------------------------------------------*/



static void qg_reformat( char qfile[], int trccnt, BOOLEAN dolog,
	PARAMS *par, FILE *gse, STATUS *status )

/* reads trace "trccnt" from q-file "qfile" and writes to file
 * "gse" in GSE format
 *
 * parameters of routine
 * char       qfile[];      input; q-file name
 * int        trccnt;       input; number of trace in q-file
 * BOOLEAN    dolog;        input; print info
 * PARAMS     *par;         input; parameters
 * FILE       *gse;         input; pointer to output file
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	DATATYPE *datptr;      /* pointer to data array */
	char     *str;         /* pointer to string */
	NTIME    ntime;        /* numeric time */
	int      doy;          /* day of year */
	int      i, j;         /* counters */
	char     comp;         /* component */
	float    dt;           /* sample distance in sec */
	BOOLEAN  intfmt;       /* integer format */
	float    tmp;          /* scratch */
	long     trclth;       /* trace length */
	long     sc;           /* sample counter */
	long     smp;          /* sample value */
	long     icheck;       /* integer checksum */
	float    fcheck;       /* floating checksum */
	float    calib;        /* calibration factor */
	float    calper;       /* calibration period */
	GLT_STATINF inf;       /* station info */
	char     wfmt[BC_LINELTH+1]; /* format string */
	char     stat[BC_LINELTH+1]; /* station name */
	int      statlth;      /* length of station name */
	BOOLEAN  grfmul;       /* calibration found in q-header */
	STATUS   locstat;      /* return status */

	/* executable code */

	qf_read( qfile, trccnt, TRUE, &datptr, status );
	if  (Severe(status))  return;

	/* find and print start time (pos 6:26) */
	str = qf_gets( QEC_T_START, status );
	if  (*status == QFE_EMPTY)  {
		*status = BC_NOERROR;
		doy = 0;
		str = NULL;
	} /*endif*/
	if  (Severe(status))  return;
	if  (str == NULL)  {
		printf( "--- warning: no start time found in trace %d ---\n",
			trccnt );
		fprintf( gse, "WID1  xxxxyyy hh mm ss fff " );
	} else {
		tc_t2n( str, &ntime, status );
		if  (Severe(status))  return;
		doy = tc_julian( ntime.year, ntime.month, ntime.day, status );
		if  (Severe(status))  return;
		fprintf( gse, "WID1  %4d%03d %02d %02d %02d %03d ", ntime.year,
			doy, ntime.hour, ntime.min, ntime.sec, ntime.ms );
	} /*endif*/

	/* print length of trace in samples (pos 28:35) */
	trclth = qf_getl( QEC_L_DATALTH, status );
	if  (Severe(status))  return;
	fprintf( gse, "%8ld ", trclth );

	/* print station name (pos 37:42) */
	str = qf_gets( QEC_S_STATION, status );
	if  (*status == QFE_EMPTY)  {
		*status = BC_NOERROR;
		str = NULL;
	} /*endif*/
	if  (Severe(status))  return;
	if  (strlen(str) > BC_LINELTH)  {
		printf( "*** station name %s too long ***\n", str );
		*status = 1;
		return;
	} /*endif*/
	if  (str == NULL)  {
		printf( "--- no station name found in trace %d ---\n", trccnt );
		fprintf( gse, "XXXXXX " );
		*stat = '\0';
	} else {
		/* rename GRF-xx to GRxx */
		strcpy( stat, str );
		if  (strncmp(stat,"GRF-",4) == 0)  {
			stat[2] = stat[4];
			stat[3] = stat[5];
			stat[4] = '\0';
		} /*endif*/
		statlth = (int)strlen( stat );
		if  (statlth > 6)  {
			printf( "--- warning: station name %s truncated ---\n", stat );
			fprintf( gse, "%6s ", stat );
		} else {
			fprintf( gse, "%s ", stat );
			j = 6 - statlth;
			for  (i=0; i<j; i++)
				fprintf( gse, " " );
		} /*endif*/
	} /*endif*/

	/* get calibration period */
	calper = -1.0;  /* default is: don't know */
	if  (stat[0] == 'G' && stat[1] == 'R' && stat[2] >= 'A' && stat[2] <= 'C')
		calper = 1.0;
	if  (calper < 0.0)  {
		if  (strcmp(stat,"BFO") == 0)  calper = 1.0;
		else if  (strcmp(stat,"BRG") == 0)  calper = 1.0;
		else if  (strcmp(stat,"BRNL") == 0)  calper = 1.0;
		else if  (strcmp(stat,"BSEG") == 0)  calper = 1.0;
		else if  (strcmp(stat,"BUG") == 0)  calper = 1.0;
		else if  (strcmp(stat,"CLL") == 0)  calper = 1.0;
		else if  (strcmp(stat,"CLZ") == 0)  calper = 1.0;
		else if  (strcmp(stat,"FUR") == 0)  calper = 1.0;
		else if  (strcmp(stat,"GSH") == 0)  calper = 1.0;
		else if  (strcmp(stat,"GRFO") == 0)  calper = 1.0;
		else if  (strcmp(stat,"GTTG") == 0)  calper = 1.0;
		else if  (strcmp(stat,"GUNZ") == 0)  calper = 1.0;
		else if  (strcmp(stat,"HAM") == 0)  calper = 1.0;
		else if  (strcmp(stat,"HLG") == 0)  calper = 1.0;
		else if  (strcmp(stat,"IBBN") == 0)  calper = 1.0;
		else if  (strcmp(stat,"LID") == 0)  calper = 1.0;
		else if  (strcmp(stat,"MANZ") == 0)  calper = 1.0;
		else if  (strcmp(stat,"MOX") == 0)  calper = 1.0;
		else if  (strcmp(stat,"ROTZ") == 0)  calper = 1.0;
		else if  (strcmp(stat,"RUE") == 0)  calper = 1.0;
		else if  (strcmp(stat,"PST") == 0)  calper = 1.0;
		else if  (strcmp(stat,"PLN") == 0)  calper = 1.0;
		else if  (strcmp(stat,"RGN") == 0)  calper = 1.0;
		else if  (strcmp(stat,"STU") == 0)  calper = 1.0;
		else if  (strcmp(stat,"TANN") == 0)  calper = 1.0;
		else if  (strcmp(stat,"TNS") == 0)  calper = 1.0;
		else if  (strcmp(stat,"TRIB") == 0)  calper = 1.0;
		else if  (strcmp(stat,"WERD") == 0)  calper = 1.0;
		else if  (strcmp(stat,"WERN") == 0)  calper = 1.0;
		else if  (strcmp(stat,"WET") == 0)  calper = 1.0;
	} /*endif*/

	/* channel ID, (here same as channel) (pos 44:51) */
	comp = qf_getc( QEC_C_COMP, status );
	if  (*status == QFE_EMPTY)  {
		*status = BC_NOERROR;
		comp = 'X';
	} /*endif*/
	/* is printed after dt is computed */

	/* samples/sec (pos:56:66) */
	dt = qf_getr( QEC_R_DELTA, status );
	if  (Severe(status))  return;
	if  (dt == 0.0)  {
		printf( "*** Illegal sample rate zero ***\n" );
		exit( 1 );
	} /*endif*/
	/* dt is printed after channel info */

	/* now print channel info (try to read it from q-file, otherwise */
	/* use reasonable defaults) */
	locstat = cBcNoError;
	par->channel = qf_getc( QEC_C_CHAN1, &locstat );
	if  (SySevere(&locstat))  {
		par->channel = ' ';
		locstat = cBcNoError;
	} /*endif*/
	if  (par->channel == ' ')  {
		if  (Abs(dt-1.0) < 0.1)  {
			par->channel = 'L';
		} else if  (Abs(dt-0.025) < 0.0025)  {
			par->channel = 'H';
		} else {
			par->channel = 'B';
		} /*endif*/
	} /*endif*/
	fprintf( gse, "%s%c%c ", stat, par->channel, comp );
	j = 6 - statlth;
	for  (i=0; i<j; i++)
		fprintf( gse, " " );
	/* channel (pos 53:54) */
	fprintf( gse, "%c%c ", par->channel, comp );

	/* now print dt */
	fprintf( gse, "%11.7f ", 1.0/dt );

	/* system type (pos 68:73) */
	fprintf( gse, "%6s ", par->instr );

	/* data format type */
	if  (strlen(par->fmt) != 4)  {
		printf( "*** Illegal format (length) %s ***\n", par->fmt );
		exit( 1 );
	} /*endif*/
	fprintf( gse, "%s ", par->fmt );
	if  (strcmp(par->fmt,"INTV") == 0)  {
		strcpy( wfmt, " %ld" );
		intfmt = TRUE;
	} else if  (strncmp(par->fmt,"INT",3) == 0)  {
		comp = par->fmt[3];
		sprintf( wfmt, "%%%cld", comp );
		intfmt = TRUE;
	} else if  (strcmp(par->fmt,"FLTV") == 0)  {
		strcpy( wfmt, " %f" );
		intfmt = FALSE;
	} else if  (strncmp(par->fmt,"FLT",3) == 0)  {
		comp = par->fmt[3];
		sprintf( wfmt, "%%%cf", comp );
		intfmt = FALSE;
	} else {
		printf( "*** Illegal format %s ***\n", par->fmt );
		exit( 1 );
	} /*endif*/

	/* differences (pos 80) */
	if  (par->diff < 0 || par->diff > 2)  {
		printf( "*** Illegal difference value %d ***\n", par->diff );
		exit( 1 );
	} /*endif*/
	fprintf( gse, "%d\n", par->diff );

	/* line 2 */

	/* calibration gain */
	calib = qf_getr( QFCALIB, status );
	if  (Severe(status))  {
		calib = 1.0;
		grfmul = FALSE;
		*status = BC_NOERROR;
	} else {
		grfmul = TRUE;
		if  (dolog)
			printf( "q-calib %e  ", calib );
	} /*endif*/
	if  (dolog && par->excalib != 1.0)
		printf( "ex-calib %e  ", par->excalib );
	calib *= par->excalib;
	if  (par->autocalib)  {
		tmp = get_step( datptr, trclth );
		if  (dolog)
			printf( "auto-calib %e  ", tmp );
		calib *= tmp;
	} /*endif*/
	for  (sc=0; sc<trclth; sc++)
		datptr[sc] /= calib;
	calib *= par->mul;
#ifdef XXX
	/* multiply calibration of GRF stations by 1000 */
	if  (strncmp(stat,"GR",2) == 0 && grfmul)
		calib *= 1000.0;
#endif
	if  (par->wkind == '0' && calper > 0.0)  calib /= 2.0 * PI * calper;
	if  (par->wkind == '2' && calper > 0.0)  calib *= 2.0 * PI * calper;
	if  (calib > 0.01 && calib < 100.0)  {
		fprintf( gse, "%9.6f", calib );
	} else {
		i = Nint( log10(calib) );
		if  (Abs(i) > 9)  {
			printf( "*** calibration %e too large ***\n", calib );
			*status = 1;
			return;
		} /*endif*/
		tmp = calib * pow(10.0,(float)(-i));
		if  (i > 0)  {
			fprintf( gse, "%7.5fe%d", tmp, i );
		} else {
			fprintf( gse, "%6.4fe%d", tmp, i );
		} /*endif*/
	} /*endif*/

	/* waveform kind */
	fprintf( gse, "%c", par->wkind );

	/* get station info */
	str = qf_gets( QEC_S_STATION, status );
	if  (Severe(status))  {
		fprintf( stderr, "*** no station name found (status %d) ***\n", *status );
		*status = BC_NOERROR;
		inf.lat = -999.0;
		inf.lon = -999.0;
	} else {
		gl_statinf( str, &inf, status );
		if  (Severe(status))  {
			fprintf( stderr,
				"*** no station info from file found (status %d) ***\n", *status );
			*status = BC_NOERROR;
			inf.lat = -999.0;
			inf.lon = -999.0;
		} /*endif*/
	} /*endif*/

	/* write calibration period */
	fprintf( gse, "%7.4f ", calper );

	/* write station info */
	fprintf( gse, "%9.4lf %9.4lf ", inf.lat, inf.lon );

	/* no station elevation info */
	if  (!gl_valid_number(inf.elevation))
		inf.elevation = -999.0;
	fprintf( gse, "%9.4f ", inf.elevation );

	/* no other info */
	fprintf( gse, "%9.4f %7.2f %7.2f %6.1f\n",
		-999.0, -1.0, -1.0, -1.0 );

	/* print data */

	fprintf( gse, "DAT1\n" );
	if  (intfmt)  {
		icheck = 0;
		for  (sc=0; sc<trclth; sc++)  {
			smp = Nlong( datptr[sc] );
			icheck += smp;
			fprintf( gse, wfmt, smp );
			if  ((sc+1) % par->npl == 0)
				fprintf( gse, "\n" );
		} /*endfor*/
	} else {
		fcheck = 0.0;
		for  (sc=0; sc<trclth; sc++)  {
			fcheck += datptr[sc];
			fprintf( gse, wfmt, datptr[sc] );
			if  ((sc+1) % par->npl == 0)
				fprintf( gse, "\n" );
		} /*endfor*/
	} /*endif*/

	/* print checksum */
	if  (sc % par->npl != 0)
		fprintf( gse, "\n" );
	if  (intfmt)  {
		fprintf( gse, "CHK1      %ld\n\n", icheck );
	} else {
		fprintf( gse, "CHK1      %f\n\n", fcheck );
	} /*endif*/

} /* end of qg_reformat */



/*------------------------------------------------------------------*/



static float get_step( float dat[], long lth )

/* finds minimum step of digitation
 *
 * parameters of routine
 * float      dat[];     input; sample data
 * long       lth;       input; length of array
 */
{
	/* local variables */
	float    mindist;     /* minimum distance */
	float    maxval;      /* maximum value */
	float    currsmp;     /* modulus of current sample */
	float    currdist;    /* modulus of current distance */
	long     i;           /* counter */
	int      red;         /* information loss counter */

	/* executable code */

	/* find minimum distance & maximum value */
	mindist = maxval = 0.0;
	for  (i=0; i<lth; i++)  {
		currdist = (i > 0) ? dat[i]-dat[i-1] : 0.0;
		currdist = Abs( currdist );
		if  (mindist == 0.0 || (currdist != 0.0 && currdist < mindist))
			mindist = currdist;
		currsmp = Abs( dat[i] );
		if  (currsmp > maxval)
			maxval = currsmp;
	} /*endfor*/

	/* find calibration */
	if  ((log10(maxval)-log10(mindist)) > 30.0)  {
		printf( "*** couldn't find auto calibration ***\n" );
		return 1.0;
	} /*endif*/

	red = 0;
	while  ((maxval/mindist) > (float)SYC_MAXLONG)  {
		mindist *= 2.0;
		red++;
	} /*endwhile*/

	if  (red > 0)
		printf( "--- info loss %d ---\n", red );

	return mindist;

} /* end of get_step */



/*------------------------------------------------------------------*/



static void qg_include_file( FILE *gse, char ifile[] )

/* copies file "ifile" to output stream "gse".  Errors produce output on
 * standard channel
 *
 * parameters of routine
 * FILE       *gse;      input; output stream
 * char       ifile[];   input; file to be included
 */
{
	/* local variables */
	FILE     *inc;             /* pointer to include file */
	char     line[BC_LINELTH]; /* current line */

	/* executable code */

	inc = sy_fopen( ifile, "r" );
	if  (inc == NULL)  {
		printf( "*** could not find include file %s ***\n", ifile );
		return;
	} /*endif*/

	while  (fgets(line,BC_LINELTH,inc) != NULL)
		fprintf( gse, "%s", line );

	sy_fclose( inc );

} /* end of qg_include_file */



/*------------------------------------------------------------------*/

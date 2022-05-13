
/* file sh_ah.c
 *      =======
 *
 * version 13, 25-Dec-2001
 *
 * AH reader for SH
 * K. Stammler, 15-Dec-92
 */


#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include BASECNST
#include BC_SYSBASE
#include BC_TCUSRDEF
#include "sh_ah.h"


#ifdef BC_FRGN_AH
#include <rpc/rpc.h>
#include "ahhead.h"




/* global variables */
static char      ahv_fname[BC_FILELTH+1];    /* name of current AH file */
static int       ahv_pos;                    /* current read position */
static FILE      *ahv_fp=NULL;               /* pointer to open AH file */
static float     *ahv_datptr=NULL;           /* pointer to data array */
static XDR       ahv_xdr;                    /* XDR handle */
static ahhed     ahv_hd;                     /* current AH header */


/* this is dummy, because of link reference in ahio library */
char progname[]="This is dummy";



/*----------------------------------------------------------------------------*/



void sh_ah_trace( char file[], int rec, STATUS *status )

/* reads trace number "rec" from AH file "file" into memory
 *
 * parameters of routine
 * char       file[];      input; name of AH file
 * int        rec;         input; position of trace in file
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	int      ahret;       /* return value of AH routines */
	double   *dptr;       /* pointer to double array */
	long     i;           /* counter */

	/* executable code */

	if  (*ahv_fname == '\0' || strcmp(ahv_fname,file) != 0
		|| ahv_fp == NULL)  {
		/* clean up with old file, if any */
		if  (ahv_fp != NULL)
			sy_fclose( ahv_fp );
		if  (ahv_datptr != NULL)  {
			cfree( ahv_datptr );
			ahv_datptr = NULL;
		} /*endif*/
		/* open new AH file */
		ahv_fp = sy_fopen( file, "r" );
		if  (ahv_fp == NULL)  {
			*status = AHE_OPENRD;
			return;
		} /*endif*/
		strcpy( ahv_fname, file );
		ahv_pos = 1;
		xdrstdio_create( &ahv_xdr, ahv_fp, XDR_DECODE );
	} /*endif*/
	/*
	if  (ahv_pos > 1 && rec == ahv_pos-1)  return;
	*/
	/* position file */
	if  (ahv_pos > rec)  {
		fseek( ahv_fp, 0, 0 );
		ahv_pos = 1;
	} /*endif*/
	if  (ahv_pos != rec)  {
		ahret = xdr_tohead( rec-ahv_pos, &ahv_xdr );
		if  (ahret < 0)  {
			*status = AHE_SKIPERR;
			fseek( ahv_fp, 0, 0 );
			ahv_pos = 1;
			return;
		} /*endif*/
	} /*endif*/

	/* read record */
	ahret = xdr_gethead( &ahv_hd, &ahv_xdr );
	if  (ahret != 1)  {
		*status = AHE_NOMOREHD;
		fseek( ahv_fp, 0, 0 );
		ahv_pos = 1;
		return;
	} /*endif*/
	if  (ahv_datptr != NULL)  cfree( ahv_datptr );
	ahv_datptr = (float *)mkdatspace( &ahv_hd );
	if  (ahv_datptr == NULL)  {
		*status = AHE_MEMOVFL;
		fseek( ahv_fp, 0, 0 );
		ahv_pos = 1;
		return;
	} /*endif*/
	ahret = xdr_getdata( &ahv_hd, ahv_datptr, &ahv_xdr );
	if  (ahret == -1)  {
		*status = AHE_READERR;
		fseek( ahv_fp, 0, 0 );
		ahv_pos = 1;
		cfree( ahv_datptr );
		return;
	} /*endif*/

	/* check type, accept FLOAT and DOUBLE only */
	if  (ahv_hd.record.type == DOUBLE)  {
		printf( "*** truncating precision: double -> float ***\n" );
		dptr = (double *)ahv_datptr;
		ahv_datptr = sy_allocmem( ahv_hd.record.ndata, sizeof(float), status );
		if  (Severe(status))  {
			fseek( ahv_fp, 0, 0 );
			ahv_pos = 1;
			cfree( ahv_datptr );
			return;
		} /*endif*/
		for  (i=0; i<ahv_hd.record.ndata; i++)
			ahv_datptr[i] = (float)dptr[i];
		cfree( dptr );
	} else if  (ahv_hd.record.type == VECTOR || ahv_hd.record.type == TENSOR)  {
		*status = AHE_ILLAHTYPE;
		fseek( ahv_fp, 0, 0 );
		ahv_pos = 1;
		return;
	} else if  (ahv_hd.record.type != FLOAT)  {
		*status = AHE_UKAHTYPE;
		fseek( ahv_fp, 0, 0 );
		ahv_pos = 1;
		return;
	} /*endif*/

	ahv_pos++;

} /* end of sh_ah_trace */



/*----------------------------------------------------------------------------*/



void sh_ah_geti( char entryname[], long *info, STATUS *status )

/* return integer info entry value
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * long       *info;           output; info returned
 * STATUS     *status;         ouput; return status
 */
{
	/* executable code */

	if  (strcmp(entryname,"LENGTH") == 0)  {
		*info = ahv_hd.record.ndata;
	} else {
		printf( "*** AH: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_ah_geti */



/*----------------------------------------------------------------------------*/



void sh_ah_getr( char entryname[], float *info, STATUS *status )

/* return float info entry value
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * float      *info;           output; info returned
 * STATUS     *status;         ouput; return status
 */
{
	/* executable code */

	if  (strcmp(entryname,"DELTA") == 0)  {
		*info = ahv_hd.record.delta;
	} else if  (strcmp(entryname,"LAT") == 0)  {
		*info = ahv_hd.event.lat;
	} else if  (strcmp(entryname,"LON") == 0)  {
		*info = ahv_hd.event.lon;
	} else if  (strcmp(entryname,"DEPTH") == 0)  {
		*info = ahv_hd.event.dep;
	} else if  (strcmp(entryname,"CALIB") == 0)  {
		if  (fabs(ahv_hd.station.DS) < 1.0e-12)  {
			*info = 1.0;
		} else {
			*info = 1.0e9 / ahv_hd.station.DS;
		} /*endif*/
	} else {
		printf( "*** AH: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_ah_getr */



/*----------------------------------------------------------------------------*/



void sh_ah_gets( char entryname[], int maxlth, char info[], STATUS *status )

/* return string info entry value
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * int        maxlth;          input; maximum length of output string
 * char       info[];          output; info returned
 * STATUS     *status;         ouput; return status
 */
{
	/* local variables */
	NTIME    ntime;     /* numeric time */
	int      cnt;       /* char counter */

	/* executable code */

	if  (strcmp(entryname,"START") == 0)  {
		if  (maxlth < 25)  {strcpy(info,"@"); return;}
		ntime.year = ahv_hd.record.abstime.yr;
		ntime.month = ahv_hd.record.abstime.mo;
		ntime.day = ahv_hd.record.abstime.day;
		ntime.hour = ahv_hd.record.abstime.hr;
		ntime.min = ahv_hd.record.abstime.mn;
		ntime.sec = (int)(floor(ahv_hd.record.abstime.sec));
		ntime.ms = Nint(
			(ahv_hd.record.abstime.sec-(float)ntime.sec) * 1000.0 );
		tc_n2t( &ntime, info, status );
	} else if  (strcmp(entryname,"ORIGIN") == 0)  {
		if  (maxlth < 25)  {strcpy(info,"@"); return;}
		ntime.year = ahv_hd.event.ot.yr;
		ntime.month = ahv_hd.event.ot.mo;
		ntime.day = ahv_hd.event.ot.day;
		ntime.hour = ahv_hd.event.ot.hr;
		ntime.min = ahv_hd.event.ot.mn;
		ntime.sec = (int)(floor(ahv_hd.event.ot.sec));
		ntime.ms = Nint(
			(ahv_hd.event.ot.sec-(float)ntime.sec) * 1000.0 );
		tc_n2t( &ntime, info, status );
	} else if  (strcmp(entryname,"COMMENT") == 0)  {
		if  (maxlth < strlen(ahv_hd.record.rcomment))  {
			strncpy( info, ahv_hd.record.rcomment, maxlth );
		} else {
			strcpy( info, ahv_hd.record.rcomment );
		} /*endif*/
	} else if  (strcmp(entryname,"STATION") == 0)  {
		if  (maxlth < strlen(ahv_hd.station.code))  {
			strncpy( info, ahv_hd.station.code, maxlth );
		} else {
			strcpy( info, ahv_hd.station.code );
		} /*endif*/
		cnt = (int)strlen( info ) - 1;
		while  (cnt > 0  &&  info[cnt] == ' ')  {
			info[cnt] = '\0';
			cnt--;
		} /*endwhile*/
	} else {
		printf( "*** AH: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_ah_gets */



/*----------------------------------------------------------------------------*/



void sh_ah_getc( char entryname[], char *info, STATUS *status )

/* return single char info entry value
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * char       *info;           output; info returned
 * STATUS     *status;         ouput; return status
 */
{
	/* executable code */

	if  (strcmp(entryname,"COMP") == 0)  {
		*info = Cap( ahv_hd.station.chan[2] );
		if  (!isalpha(*info))
			*info = Cap( ahv_hd.station.chan[0] );
	} else if  (strcmp(entryname,"CHAN1") == 0)  {
		*info = Cap( ahv_hd.station.chan[0] );
	} else if  (strcmp(entryname,"CHAN2") == 0)  {
		*info = Cap( ahv_hd.station.chan[1] );
	} else {
		printf( "*** AH: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_ah_getc */



/*----------------------------------------------------------------------------*/



void sh_ah_read( float smp[] )

/* returns sample data
 *
 * parameters of routine
 * float      smp[];     output; sample data
 */
{
	/* local variables */
	long     i;      /* counter */

	/* executable code */

	/* printf( "--> A0,DS: %f, %f\n", ahv_hd.station.A0, ahv_hd.station.DS ); */

	if  (fabs(ahv_hd.station.DS) < 1.0e-12)  {
		printf( "sh_ah_read: zero calibration found in AH file header\n" );
		for  (i=0; i<ahv_hd.record.ndata; i++)
			*smp++ = ahv_datptr[i];
	} else {
		for  (i=0; i<ahv_hd.record.ndata; i++)
			*smp++ = ahv_datptr[i]/ahv_hd.station.DS * 1.0e9;
	} /*endif*/

} /* end of sh_ah_read */



/*----------------------------------------------------------------------------*/


#endif /* BC_FRGN_AH */

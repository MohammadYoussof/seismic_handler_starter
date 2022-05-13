
/* file gsedata.c
 *      =========
 *
 * version 12, 16-Feb-2000
 *
 * GSE 2.0 data input
 * K. Stammler, 8-Dec-94
 *
 * v 4: 14-Mar-96, rescale dsp-calibs to vel, i.e. multiply by 2*pi*calper
 * v 16, 16-Feb-2000, fix zero calibration and read seconds without leading 0
 */


#include <stdio.h>
#include <string.h>
#include BASECNST
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include "lineparse.h"
#include "gsedata.h"
#include "f2c.h"


#define offsetof(t,i) (int)(&( ((t *)0)->i ))

#define CHKSUM_MOD 100000000L

#define PI 3.14159265358979323846


/* exported variables (used as constants only)*/

/* description of WID2-line */
SLpEntry vgse_struct_wid2[] = {
	{  0,  4, nLpTypeString, offsetof(SGseWid2,id)},
	{  5, 23, nLpTypeString, offsetof(SGseWid2,date_time)},
	{ 29,  5, nLpTypeString, offsetof(SGseWid2,station)},
	{ 35,  3, nLpTypeString, offsetof(SGseWid2,channel)},
	{ 39,  4, nLpTypeString, offsetof(SGseWid2,auxid)},
	{ 44,  3, nLpTypeString, offsetof(SGseWid2,datatype)},
	{ 48,  8, nLpTypeInt,    offsetof(SGseWid2,samps)},
	{ 57, 11, nLpTypeFloat,  offsetof(SGseWid2,samprat)},
	{ 69, 10, nLpTypeFloat,  offsetof(SGseWid2,calib)},
	{ 80,  7, nLpTypeFloat,  offsetof(SGseWid2,calper)},
	{ 88,  6, nLpTypeString, offsetof(SGseWid2,instype)},
	{ 95,  5, nLpTypeFloat,  offsetof(SGseWid2,hang)},
	{101,  5, nLpTypeFloat,  offsetof(SGseWid2,vang)}
};

SLpEntry vgse_struct_wid1_l1[] = {
	{  0,  4, nLpTypeString, offsetof(SGseWid1,id)},
	{  5,  8, nLpTypeInt,    offsetof(SGseWid1,day_year)},
	{ 14,  2, nLpTypeInt,    offsetof(SGseWid1,hour)},
	{ 17,  2, nLpTypeInt,    offsetof(SGseWid1,min)},
	{ 20,  2, nLpTypeInt,    offsetof(SGseWid1,sec)},
	{ 23,  3, nLpTypeInt,    offsetof(SGseWid1,ms)},
	{ 27,  8, nLpTypeInt,    offsetof(SGseWid1,samps)},
	{ 36,  6, nLpTypeString, offsetof(SGseWid1,station)},
	{ 43,  8, nLpTypeString, offsetof(SGseWid1,channel_id)},
	{ 52,  2, nLpTypeString, offsetof(SGseWid1,channel)},
	{ 55, 11, nLpTypeFloat,  offsetof(SGseWid1,samprat)},
	{ 67,  6, nLpTypeString, offsetof(SGseWid1,instype)},
	{ 74,  4, nLpTypeString, offsetof(SGseWid1,datatype)},
	{ 79,  1, nLpTypeChar,   offsetof(SGseWid1,diff_flag)}
};
SLpEntry vgse_struct_wid1_l2[] = {
	{  0,  9, nLpTypeFloat,  offsetof(SGseWid1,calib)},
	{  9,  1, nLpTypeChar,   offsetof(SGseWid1,unit)},
	{ 10,  7, nLpTypeFloat,  offsetof(SGseWid1,calper)},
	{ 18,  9, nLpTypeFloat,  offsetof(SGseWid1,statlat)},
	{ 28,  9, nLpTypeFloat,  offsetof(SGseWid1,statlon)},
	{ 38,  9, nLpTypeFloat,  offsetof(SGseWid1,statelev)},
	{ 48,  9, nLpTypeFloat,  offsetof(SGseWid1,sensdepth)},
	{ 58,  7, nLpTypeFloat,  offsetof(SGseWid1,beamaz)},
	{ 66,  7, nLpTypeFloat,  offsetof(SGseWid1,beamslo)},
	{ 74,  6, nLpTypeFloat,  offsetof(SGseWid1,horient)}
};

/* global variables */
static long vgse_alloc_size=32768;     /* allocation block size */
static FILE *vgse_fp=NULL;             /* pointer to current file */



/* prototypes of local routines */
static void GseStoreBytes( char new[], long newlth, SGseRawData *raw,
	TSyStatus *status );
static void GseIntegrateTrace( long smp[], long lth );
static long GseChecksum( long smp[], long lth );
static float GseFChecksum( float smp[], long lth );



/*----------------------------------------------------------------------------*/



void GseReadTrace( FILE *fp, SGseTrace *gsetrc, TSyStatus *status )

/* Reads next GSE trace from file
 *
 * parameters of routine
 * FILE       *fp;           input; pointer to GSE file
 * SGseTrace  *gsetrc;       output; GSE trace read
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	char     line[cGseMaxLineLth+1];     /* current line of file */
	int      lout, ierr;                 /* fortran parameters */
	int      i;                          /* counter */
	int      no_of_integrations;         /* number of integrations */
	long     chksum;                     /* actual checksum */
	long     chkval;                     /* checksum of CHK2 */
	float    fchksum;                    /* floating point checksum */
	float    fchkval;                    /* floating point value of CHK1/2 */
	char     version;                    /* version number */
	char     idstr[cBcShortStrLth+1];    /* id string */
	char     chkstr[cBcShortStrLth+1];   /* checksum string */
	char     tmpdatatype[cBcShortStrLth+1]; /* data type */
	long     *lptr;                      /* pointer to long */
	float    *fptr;                      /* pointer to float */
	long     no_of_samples;              /* number of samples */
	int      hasfloatvalues;             /* has floating point samples */

	/* executable code */

	hasfloatvalues = 0;

	/* find next WID2-line */
	do  {
		do  {
			if  (fgets(line,cGseMaxLineLth,fp) == NULL)  {
				*status = eGseReadErr;
				return;
			} /*endif*/
		}  while  (strncmp(line,"WID",3) != 0);
	}  while  (line[3] != '1' && line[3] != '2');
	version = line[3];
	gsetrc->version = (int)(version - '0');

	/* parse this line */
	if  (version == '1')  {
		LpParseLine( line, vgse_struct_wid1_l1, cGseWid1Line1Num,
			&(gsetrc->wid1), status );
		fgets( line, cGseMaxLineLth, fp );
		LpParseLine( line, vgse_struct_wid1_l2, cGseWid1Line2Num,
			&(gsetrc->wid1), status );
		strcpy( idstr, "DAT1" );
		/* now change calib to velocity if necessary */
		no_of_integrations = (int)(gsetrc->wid1.diff_flag - '0');
		if  (gsetrc->wid1.unit != '1')  {
			if  (gsetrc->wid1.calper <= 0)  {
				fprintf( stderr, "gsedata: Illegal calib-per. Change to 1 sec\n" );
				gsetrc->wid1.calper = 1.0;
			} /*endif*/
			if  (gsetrc->wid1.unit == '0')
				gsetrc->wid1.calib *= (2.0 * PI / gsetrc->wid1.calper);
			if  (gsetrc->wid1.unit == '2')
				gsetrc->wid1.calib /= (2.0 * PI / gsetrc->wid1.calper);
			gsetrc->wid1.unit = '1';
		} /*endif*/
		if  (gsetrc->wid1.calib == 0.0)  {
			fprintf( stderr,
				"gsedata: Illegal zero calibration. Change to 1 (vel.)\n" );
			gsetrc->wid1.calib = 1.0;
		} /*endif*/
		strcpy( tmpdatatype, gsetrc->wid1.datatype );
		no_of_samples = gsetrc->wid1.samps;
	} else {
		no_of_integrations = 2;
		LpParseLine( line, vgse_struct_wid2, cGseWid2StructNum,
			&(gsetrc->wid), status );
		if  (SySevere(status))  return;
		strcpy( idstr, "DAT2" );
		/* check for reasonable calibration period */
		if  (gsetrc->wid.calper <= 0)  {
			fprintf( stderr, "gsedata: Illegal calib-per. Change to 1 sec\n" );
			gsetrc->wid.calper = 1.0;
		} /*endif*/
		/* check for nonzero calibration */
		if  (gsetrc->wid.calib == 0.0)  {
			fprintf( stderr,
				"gsedata: Illegal zero calibration. Change to 1 (vel.)\n" );
			gsetrc->wid.calib = gsetrc->wid.calper/(2.0*PI);
		} /*endif*/
		/* now change calib to velocity */
		gsetrc->wid.calib *= (2.0 * PI / gsetrc->wid.calper);
		/* set '0' before decimal point in time string */
		if  (gsetrc->wid.date_time[19] == '.'
			&& gsetrc->wid.date_time[18] == ' ')  {
			gsetrc->wid.date_time[18] = '0';
		} /*endif*/
		strcpy( tmpdatatype, gsetrc->wid.datatype );
		no_of_samples = gsetrc->wid.samps;
	} /*endif*/

	/* find next DAT1/2-line */
	do  {
		if  (fgets(line,cGseMaxLineLth,fp) == NULL)  {
			*status = eGseReadErr;
			return;
		} /*endif*/
	}  while  (strncmp(line,idstr,4) != 0);

	gsetrc->raw.mem = NULL;
	gsetrc->raw.alloc = 0;
	gsetrc->raw.used = 0;
	gsetrc->smp = NULL;

	/* set checksum string */
	if  (version == '1')  strcpy(chkstr,"CHK1"); else strcpy(chkstr,"CHK2");

	/* if data are not encoded ... */
	if  (strncmp(tmpdatatype,"INT",3) == 0)  {

		/* ... then read data directly from file (integers) */

		gsetrc->smp = (long *)sy_allocmem( no_of_samples, sizeof(long), status );
		if  (SySevere(status))  return;
		lptr = gsetrc->smp;
		for  (i=0; i<no_of_samples; i++)
			fscanf( fp, "%ld\n", lptr++ );

		/* read until CHK1/2 */
		do  {
			if  (fgets(line,cGseMaxLineLth,fp) == NULL)  {
				*status = eGseReadErr;
				return;
			} /*endif*/
		}  while  (strncmp(line,chkstr,4) != 0);

		sscanf( line+4, "%ld", &chkval );
		chkval %= CHKSUM_MOD;

		if  (version == '2')  no_of_integrations = 0;
		hasfloatvalues = 0;

	} else if  (strncmp(tmpdatatype,"FLT",3) == 0)  {

		/* ... then read data directly from file (floating point) */

		gsetrc->smp = (long *)sy_allocmem( no_of_samples, sizeof(float), status );
		if  (SySevere(status))  return;
		fptr = (float *)gsetrc->smp;
		for  (i=0; i<no_of_samples; i++)
			fscanf( fp, "%f\n", fptr++ );

		/* read until CHK1/2 */
		do  {
			if  (fgets(line,cGseMaxLineLth,fp) == NULL)  {
				*status = eGseReadErr;
				return;
			} /*endif*/
		}  while  (strncmp(line,chkstr,4) != 0);

		sscanf( line+4, "%f", &fchkval );

		if  (version == '2')  no_of_integrations = 0;
		hasfloatvalues = 1;

	} else {

		/* ... else read compressed data into memory */

		/* read until CHK1/2 */
		FOREVER  {
			if  (fgets(line,cGseMaxLineLth,fp) == NULL)  {
				*status = eGseReadErr;
				return;
			} /*endif*/
			if  (strncmp(line,chkstr,4) == 0)  break;
			i = strlen( line );
			if  (line[i-1] == '\n')  line[--i] = '\0';
			GseStoreBytes( line, i, &(gsetrc->raw), status );
			if  (SySevere(status))  return;
		} /*endfor*/

		sscanf( line+4, "%ld", &chkval );
		chkval %= CHKSUM_MOD;

		/* append a blank to the end of data */
		strcpy( line, " " );
		GseStoreBytes( line, 1, &(gsetrc->raw), status );
		if  (SySevere(status))  return;

		gsetrc->smp = (long *)sy_allocmem( no_of_samples+1, sizeof(long),
			status );
		if  (SySevere(status))  return;

		lout = no_of_samples;
		dcomp6_( &(gsetrc->raw.used), gsetrc->raw.mem, &lout, gsetrc->smp, &ierr,
			lout );
		if  (ierr != 0)
			fprintf( stderr, "GseReadTrace: dcomp6_: ierr = %d\n", ierr );

		hasfloatvalues = 0;

	} /*endif*/

	/* integrate trace */
	for  (i=0; i<no_of_integrations; i++)
		GseIntegrateTrace( gsetrc->smp, no_of_samples );

	if  (hasfloatvalues)  {
		fchksum = GseFChecksum( (float *)(gsetrc->smp), no_of_samples );
		fchkval = Abs( fchkval );
		if  (Abs(fchksum-fchkval) > fchkval/20.0)  {
			if  (version == '1')  {
				fprintf( stderr, "GseReadTrace: checksum error (%f,%f) at %s %s\n",
					fchksum, fchkval, gsetrc->wid1.station, gsetrc->wid1.channel );
			} else {
				fprintf( stderr, "GseReadTrace: checksum error (%f,%f) at %s %s\n",
					fchksum, fchkval, gsetrc->wid.station, gsetrc->wid.channel );
			} /*endif*/
		} /*endif*/
	} else {
		chksum = GseChecksum( gsetrc->smp, no_of_samples );
		chksum = Abs( chksum );
		chksum %= CHKSUM_MOD;
		chkval = Abs( chkval );
		if  (chksum != chkval)  {
			if  (version == '1')  {
				fprintf( stderr, "GseReadTrace: checksum error (%ld,%ld) at %s %s\n",
					chksum, chkval, gsetrc->wid1.station, gsetrc->wid1.channel );
			} else {
				fprintf( stderr, "GseReadTrace: checksum error (%ld,%ld) at %s %s\n",
					chksum, chkval, gsetrc->wid.station, gsetrc->wid.channel );
			} /*endif*/
		} /*endif*/
	} /*endif*/

	/* free memory of raw data */
	if  (gsetrc->raw.mem != NULL)  sy_deallocmem( gsetrc->raw.mem );
	gsetrc->raw.mem = NULL;
	gsetrc->raw.alloc = 0;
	gsetrc->raw.used = 0;

} /* end of GseReadTrace */



/*----------------------------------------------------------------------------*/



static void GseStoreBytes( char new[], long newlth, SGseRawData *raw,
	TSyStatus *status )

/* Appends new data to buffer
 *
 * parameters of routine
 * char       new[];         input; new data to append
 * long       newlth;        input; number of bytes
 * SGseRawData *raw;         modify; raw data buffer
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	char     *newbuf;         /* pointer to new buffer */

	/* executable code */

	/* if step size is too small abort (not very likely) */
	if  (newlth > vgse_alloc_size)  {
		*status = eGseSmallAllocBlock;
		return;
	} /*endif*/

	/* if not enough memory allocate another piece and recopy data */
	if  (newlth+raw->used > raw->alloc)  {
		raw->alloc += vgse_alloc_size;
		newbuf = (char *)sy_allocmem( raw->alloc, sizeof(char), status );
		if  (SySevere(status))  {
			if  (raw->mem != NULL)  sy_deallocmem( raw->mem );
			raw->mem = NULL;
			raw->alloc = raw->used = 0;
			return;
		} /*endif*/
		/* copy data to new buffer */
		if  (raw->mem != NULL)  {
			memcpy( newbuf, raw->mem, raw->used );
			sy_deallocmem( raw->mem );
		} /*endif*/
		raw->mem = newbuf;
	} /*endif*/

	/* now append new data */
	memcpy( raw->mem+raw->used, new, newlth );
	raw->used += newlth;
	raw->mem[raw->used] = '\0';

} /* end of GseStoreBytes */



/*----------------------------------------------------------------------------*/



static void GseIntegrateTrace( long smp[], long lth )

/* Integrates trace
 *
 * parameters of routine
 * long       smp[];          modify; trace to be integrated
 * long       lth;            input; length of trace
 */
{
	/* local variables */
	long     i;          /* counter */

	/* executable code */

	for  (i=1; i<lth; i++)
		smp[i] += smp[i-1];

} /* end of GseIntegrateTrace */



/*----------------------------------------------------------------------------*/



static long GseChecksum( long smp[], long lth )

/* Computes checksum trace
 *
 * parameters of routine
 * long       smp[];          input; trace to be checked
 * long       lth;            input; length of trace
 */
{
	/* local variables */
	long     i;          /* counter */
	long     chksum;     /* checksum */

	/* executable code */

	chksum = 0;
	for  (i=0; i<lth; i++)  {
		chksum += smp[i];
		if  (Abs(chksum) > CHKSUM_MOD)  chksum %= CHKSUM_MOD;
	} /*endfor*/

	return chksum;

} /* end of GseChecksum */



/*----------------------------------------------------------------------------*/



static float GseFChecksum( float smp[], long lth )

/* Computes checksum trace for floating point values
 *
 * parameters of routine
 * float       smp[];         input; trace to be checked
 * long       lth;            input; length of trace
 */
{
	/* local variables */
	long     i;          /* counter */
	float    chksum;     /* checksum */

	/* executable code */

	chksum = 0.0;
	for  (i=0; i<lth; i++)  {
		chksum += smp[i];
	} /*endfor*/

	return chksum;

} /* end of GseFChecksum */



/*----------------------------------------------------------------------------*/



void GseOpenFile( char file[], TSyStatus *status )

/* Opens GSE file
 *
 * parameters of routine
 * char       file[];         input; name of GSE file
 * TSyStatus  *status;        output; return status
 */
{
	/* executable code */

	if  (vgse_fp != NULL)  SyFclose( vgse_fp );
	vgse_fp = NULL;

	vgse_fp = SyFopen( file, "r" );
	if  (vgse_fp == NULL)  {
		*status = eGseOpenInput;
		return;
	} /*endif*/

} /* end of GseOpenFile */



/*----------------------------------------------------------------------------*/



void GseCloseFile( void )

/* Closes GSE file
 *
 * no parameters
 */
{
	/* executable code */

	if  (vgse_fp != NULL)  SyFclose( vgse_fp );
	vgse_fp = NULL;

} /* end of GseCloseFile */



/*----------------------------------------------------------------------------*/



void GseNextTrace( SGseTrace *gsetrc, TSyStatus *status )

/* Reads next trace from file
 *
 * parameters of routine
 * SGseTrace  *gstrc;         output; trace descriptor
 * TSyStatus  *status;        output; return status
 */
{
	/* executable code */

	if  (vgse_fp == NULL)  {
		*status = eGseNotOpen;
		return;
	} /*endif*/

	GseReadTrace( vgse_fp, gsetrc, status );

} /* end of GseNextTrace */



/*----------------------------------------------------------------------------*/



void GseSkipTrace( TSyStatus *status )

/* Skips one trace in file
 *
 * parameters of routine
 * TSyStatus  *status;        output; return status
 */
{
	/* local variables */
	char     line[cGseMaxLineLth+1];     /* current line of file */

	/* executable code */

	/* find next DAT1/2-line */
	do  {
		if  (fgets(line,cGseMaxLineLth,vgse_fp) == NULL)  {
			*status = eGseReadErr;
			return;
		} /*endif*/
	}  while  (strncmp(line,"DAT",3) != 0 || (line[3] != '1' && line[3] != '2'));

	/* find next CHK1/2-line */
	do  {
		if  (fgets(line,cGseMaxLineLth,vgse_fp) == NULL)  {
			*status = eGseReadErr;
			return;
		} /*endif*/
	}  while  (strncmp(line,"CHK",3) != 0 || (line[3] != '1' && line[3] != '2'));

} /* end of GseSkipTrace */



/*----------------------------------------------------------------------------*/


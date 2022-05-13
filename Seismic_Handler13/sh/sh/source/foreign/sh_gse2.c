
/* file sh_gse2.c
 *      ========
 *
 * version 4, 11-Sep-2007
 *
 * GSE 2.0 reader for SH (foreign format input)
 * v4, 11-Sep-2007, implemented FLT data type
 * K. Stammler, 9-Dec-94
 *
 */



#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "tcusrdef.h"
#include "gsedata.h"
#include "sh_gse2.h"



/* constants */


/* global variables */
static char     vgs2_gsefile[BC_FILELTH+1]; /* name of current GSE file */
static int      vgs2_currtrc=0;             /* next trace to read */
static SGseTrace vgs2_trc;                  /* trace descriptor */


/*----------------------------------------------------------------------------*/



void ShGs2Trace( char file[], int trcno, TSyStatus *status )

/* reads specified trace and holds it in memory
 *
 * parameters of routine
 * char       file[];    input; name of GSE file
 * int        trcno;     input; number of record to read
 * TSyStatus  *status;   output; return status
 */
{
	/* local variables */

	/* executable code */

	/* if new file, initialize */
	if  (strcmp(file,vgs2_gsefile) != 0)  {
		GseOpenFile( file, status );
		if  (SySevere(status))  return;
		vgs2_currtrc = 1;
		strcpy( vgs2_gsefile, file );
	} /*endif*/

	/* rewind file if previous trace requested */
	if  (trcno < vgs2_currtrc)  {
		GseCloseFile();
		GseOpenFile( file, status );
		if  (SySevere(status))  {GseCloseFile(); return;}
		vgs2_currtrc = 1;
	} /*endif*/

	/* find trace */
	while  (vgs2_currtrc < trcno)  {
		GseSkipTrace( status );
		if  (SySevere(status))  {GseCloseFile(); return;}
		vgs2_currtrc++;
	} /*endwhile*/

	/* free memory of previous trace (if any) */
	if  (vgs2_trc.raw.mem != NULL)  SyDeallocMem( vgs2_trc.raw.mem );
	if  (vgs2_trc.smp != NULL)  SyDeallocMem( vgs2_trc.smp );
	vgs2_trc.raw.mem = NULL;
	vgs2_trc.smp = NULL;

	GseNextTrace( &vgs2_trc, status );
	if  (SySevere(status))  GseCloseFile();
	vgs2_currtrc++;

} /* end of ShGs2Trace */



/*----------------------------------------------------------------------------*/



void ShGs2GetI( char entryname[], long *info, TSyStatus *status )

/* returns integer value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * long       *info;           output; returned info
 * TSyStatus  *status;         output; return status
 */
{
	/* executable code */

	if  (strcmp(entryname,"LENGTH") == 0)  {
		*info = (vgs2_trc.version == 1) ?
			vgs2_trc.wid1.samps : vgs2_trc.wid.samps;
	} else {
		printf( "*** GSE: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of ShGs2GetI */



/*----------------------------------------------------------------------------*/



void ShGs2GetR( char entryname[], float *info, TSyStatus *status )

/* returns float value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * float      *info;           output; returned info
 * TSyStatus  *status;         output; return status
 */
{
	/* executable code */

	if  (strcmp(entryname,"DELTA") == 0)  {
		*info = (vgs2_trc.version == 1) ?
			1.0 / vgs2_trc.wid1.samprat : 1.0 / vgs2_trc.wid.samprat;
	} else if  (strcmp(entryname,"CALIB") == 0)  {
		*info = (vgs2_trc.version == 1) ?
			vgs2_trc.wid1.calib : vgs2_trc.wid.calib;
	} else {
		printf( "*** GSE: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of ShGs2GetR */



/*----------------------------------------------------------------------------*/



void ShGs2GetS( char entryname[], int maxlth, char info[], TSyStatus *status )

/* returns string value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * int        maxlth;          input; maximum length of output string
 * char       info[];          output; returned info
 * TSyStatus  *status;         output; return status
 */
{
	/* local variables */
	STATUS   locstat;          /* local status */
	NTIME    ntime;            /* numeric time */
	int      julday;           /* julian day */

	/* executable code */

	if  (maxlth < cBcLineLth)  {
		*status = eGs2StrOvfl;
		return;
	} /*endif*/

	if  (strcmp(entryname,"START") == 0)  {
		if  (vgs2_trc.version == 1)  {
			julday = vgs2_trc.wid1.day_year % 1000;
			ntime.year = vgs2_trc.wid1.day_year / 1000;
			locstat = cBcNoError;
			tc_dayofmn( ntime.year, julday, &ntime.month, &ntime.day, &locstat );
			ntime.hour = vgs2_trc.wid1.hour;
			ntime.min = vgs2_trc.wid1.min;
			ntime.sec = vgs2_trc.wid1.sec;
			ntime.ms = vgs2_trc.wid1.ms;
			tc_n2t( &ntime, info, &locstat );
		} else {
			strcpy( info, vgs2_trc.wid.date_time );
		} /*endif*/
	} else if  (strcmp(entryname,"STATION") == 0)  {
		if  (vgs2_trc.version == 1)  {
			strcpy( info, vgs2_trc.wid1.station );
		} else {
			strcpy( info, vgs2_trc.wid.station );
		} /*endif*/
	} else {
		printf( "*** GSE: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of ShGs2GetS */



/*----------------------------------------------------------------------------*/



void ShGs2GetC( char entryname[], char *info, TSyStatus *status )

/* returns character value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * char       *info;           output; returned info
 * TSyStatus  *status;         output; return status
 */
{
	/* executable code */

	if  (strcmp(entryname,"COMP") == 0)  {
		*info = (vgs2_trc.version == 1) ?
			Cap(vgs2_trc.wid1.channel[cGseWid1ChannelLth-1]) :
			Cap(vgs2_trc.wid.channel[cGseWid2ChannelLth-1]);
	} else if  (strcmp(entryname,"CHAN1") == 0)  {
		*info = (vgs2_trc.version == 1) ?
			Cap(vgs2_trc.wid1.channel[0]) : Cap(vgs2_trc.wid.channel[0]);
	} else if  (strcmp(entryname,"CHAN2") == 0)  {
		*info = (vgs2_trc.version == 1) ?
			'H' /*!!!*/ : Cap(vgs2_trc.wid.channel[1]);
	} else {
		printf( "*** GSE: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of ShGs2GetC */



/*----------------------------------------------------------------------*/



void ShGs2Read( float smp[] )

/* returns sample data
 *
 * parameter of routine
 * float     smp[];     output; sample data
 */
{
	/* local variables */
	long     *s, *end;   /* sample pointers */
	float    *fs, *fend; /* floating point version pointers */

	/* executable code */

	if  (vgs2_trc.version == 1)  {

		if  (strncmp(vgs2_trc.wid1.datatype,"FLT",3) == 0)  {
			fs = (float *)vgs2_trc.smp;
			fend = (float *)vgs2_trc.smp + vgs2_trc.wid1.samps;
			while  (fs < fend)
				*smp++ = (*fs++) * vgs2_trc.wid1.calib;
		} else {
			s = vgs2_trc.smp;
			end = vgs2_trc.smp + vgs2_trc.wid1.samps;
			while  (s < end)
				*smp++ = (float)(*s++) * vgs2_trc.wid1.calib;
		} /*endif*/

	} else {

		if  (strncmp(vgs2_trc.wid.datatype,"FLT",3) == 0)  {
			fs = (float *)vgs2_trc.smp;
			fend = (float *)vgs2_trc.smp + vgs2_trc.wid.samps;
			while  (fs < fend)
				*smp++ = (*fs++) * vgs2_trc.wid.calib;
		} else {
			s = vgs2_trc.smp;
			end = vgs2_trc.smp + vgs2_trc.wid.samps;
			while  (s < end)
				*smp++ = (float)(*s++) * vgs2_trc.wid.calib;
		} /*endif*/

	} /*endif*/

} /* end of ShGs2Read */




/*----------------------------------------------------------------------------*/


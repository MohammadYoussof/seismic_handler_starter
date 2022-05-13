
/* file seedgaps.c
 *      ==========
 *
 * version 9, 1-Sep-2003
 *
 * Finds gaps in SEED streams.
 * K. Stammler, 16-Jan-94
 */




#include <stdio.h>
#include <string.h>
#include BASECNST
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_CPAR
#include BC_ERUSRDEF
#include BC_TCUSRDEF
#include "seedcfg.h"
#include "seed_lib.h"



#define SEEDCHAN 0
#define MAXERRCNT 100



int main( int argc, char *argv[] )
{
	/* local variables */
	char     stream[BC_LINELTH];         /* stream name */
	char     from_time[BC_TIMELTH+1];    /* from time */
	char     to_time[BC_TIMELTH+1];      /* to time */
	char     str[BC_LINELTH+1];          /* scratch string */
	char     str2[BC_LINELTH+1];         /* another scratch string */
	NTIME    endtime;                    /* to_time in NTIME format */
	NTIME    begtime;                    /* begin time of record */
	NTIME    currtime;                   /* current time */
	NTIME    lasttime;                   /* end time of previous record */
	NTIME    bt, ct;                     /* scratch values */
	char     sfdfile[BC_FILELTH+1];      /* fsd file */
	STATUS   status;                     /* return status */
	SeedSbyteT *seedrec;                 /* pointer to SEED record */
	float    dt;                         /* sample distance in sec */
	float    tdiff;                      /* time difference */
	BOOLEAN  hdr_swapped;                /* header was swapped */
	int      errcnt;                     /* error counter */
	TSyBoolean nogaps;                   /* no gaps found */
	TSyBoolean print_nogaps;             /* write info out */
	TSyBoolean fussy;                    /* warn also on very small gaps */

	/* executable code */

	status = BC_NOERROR;

	pa_init( argc, argv );
	if  (pa_pnumber() != 3)  {
		printf( "Usage: %s <stream> <from> <to> ***\n", argv[0] );
		printf( "       qualifers:\n" );
		printf( "       -sfdfile=<sfdfile>   sfd file\n" );
		printf( "       -i                   info on 'no gaps'\n" );
		printf( "       -fussy               list also very small gaps\n" );
		return 1;
	} /*endif*/

	/* get parameters */
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	strcpy( stream, pa_pvalue(1) );
	strcpy( from_time, pa_pvalue(2) );
	strcpy( to_time, pa_pvalue(3) ),
	strcpy( sfdfile, "sfdfile.sfd" );
	if  (pa_qspecified("-sfdfile"))
		strcpy( sfdfile, pa_qvalue("-sfdfile") );
	print_nogaps = FALSE;
	if  (pa_qspecified("-i"))
		print_nogaps = TRUE;
	fussy = FALSE;
	if  (pa_qspecified("-fussy"))
		fussy = TRUE;

	/* convert stop time to NTIME format */
	tc_t2n( to_time, &endtime, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* allocate memory for SEED record */
	seedrec = (SeedSbyteT *)sy_allocmem( 1, Seed_C_MAX_RECLTH, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* position file and get time */
	SeedSearchPosition( SEEDCHAN, sfdfile, stream, from_time,
		str, &status );
	if  (status == SeedERR_NEXTTIMEPOS)  {
		status = BC_NOERROR;
		tdiff = tc_tdiff( str, from_time, &status );
		printf( "gap %s %5.3f sec, %s to %s file %s (%d)\n",
			stream, tdiff, from_time, str, SeedGetFilename(SEEDCHAN), status );
		if  (Severe(&status))  {printf("seedgaps: tdiff err (1)\n");return 0;}
		nogaps = FALSE;
		tdiff = tc_tdiff( str, to_time, &status );
		if  (Severe(&status))  {printf("seedgaps: tdiff err (2)\n");return 0;}
		if  (tdiff > 0.0)  {
			printf( "no data in requested time window\n" );
			return 0;
		} /*endif*/
	} else if (status == SeedERR_SFD_NOTFOUND)  {
		tdiff = tc_tdiff( to_time, from_time, &status );
		printf( "gap %s %5.3f sec, %s to %s file %s (%d)\n",
			stream, tdiff, from_time, to_time, SeedGetFilename(SEEDCHAN), status );
		return 0;
	} /*endif*/
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	tc_t2n( str, &lasttime, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* read each record until stop time */
	nogaps = TRUE;
	errcnt = 0;
	dt = 0.0;
	*str = '\0';
	do  {
		if  (errcnt > MAXERRCNT)  break;
		SeedReadNextRecord( SEEDCHAN, seedrec, &hdr_swapped, &status );
		if  (status == SeedERR_NEXTTIMEPOS)  status = BC_NOERROR;
		if  (status == SeedERR_SFD_NOTFOUND)  {
			status = BC_NOERROR;
			tc_n2t( &lasttime, str, &status );
			if  (Severe(&status))  {
				err_writemsg( status, str, FALSE );
				status = cBcNoError;
				errcnt++;
				continue;
			} /*endif*/
			tdiff = tc_tdiff( to_time, str, &status );
			if  (Severe(&status))  {
				err_writemsg( status, str, FALSE );
				status = cBcNoError;
				errcnt++;
				continue;
			} /*endif*/
			printf( "gap %s %5.3f sec, %s to %s file %s (%d)\n",
				stream, tdiff, str, to_time, SeedGetFilename(SEEDCHAN), status );
			nogaps = FALSE;
			break;
		} /*endif*/
		if  (Severe(&status))  {
			err_writemsg( status, str, FALSE );
			status = cBcNoError;
			errcnt++;
			continue;
		} /*endif*/
		SeedRecordTime( seedrec, &bt, &ct, &status );
		if  (Severe(&status))  {
			err_writemsg( status, str, FALSE );
			status = cBcNoError;
			errcnt++;
			continue;
		} /*endif*/
		tdiff = tc_ndiff( &bt, &lasttime, &status );
		if  (Severe(&status))  {
			err_writemsg( status, str, FALSE );
			status = cBcNoError;
			errcnt++;
			continue;
		} /*endif*/
		if  (dt == 0.0)  {  /* first loop */
			begtime = bt;
			currtime = ct;
			dt = SeedGetSampleDist( (SeedDataHeaderT *)seedrec );
			if  (tdiff >= dt)  {
				tc_n2t( &lasttime, str, &status );
				if  (Severe(&status))  {
					err_writemsg( status, str, FALSE );
					status = cBcNoError;
					errcnt++;
					continue;
				} /*endif*/
				tc_n2t( &begtime, str2, &status );
				if  (Severe(&status))  {
					err_writemsg( status, str, FALSE );
					status = cBcNoError;
					errcnt++;
					continue;
				} /*endif*/
				printf( "gap %s %5.3f sec, %s to %s file %s (%d)\n",
					stream, tdiff, str, str2, SeedGetFilename(SEEDCHAN), status );
				nogaps = FALSE;
			} /*endif*/
			lasttime = currtime;
			continue;
		} /*endif*/
		if  ((!fussy && (Abs(tdiff) > dt))
			|| (fussy && (Abs(tdiff) > dt/1000.0)))  {
			tc_n2t( &lasttime, str, &status );
			if  (Severe(&status))  {
				err_writemsg( status, str, FALSE );
				status = cBcNoError;
				errcnt++;
				continue;
			} /*endif*/
			tc_n2t( &bt, str2, &status );
			if  (Severe(&status))  {
				err_writemsg( status, str, FALSE );
				status = cBcNoError;
				errcnt++;
				continue;
			} /*endif*/
			printf( "gap %s %5.3f sec, %s to %s file %s (%d)\n",
				stream, tdiff, str, str2, SeedGetFilename(SEEDCHAN), status );
			nogaps = FALSE;
		} /*endif*/
		begtime = bt;
		currtime = ct;
		lasttime = currtime;
	}  while (tc_ndiff(&endtime,&currtime,&status) > 0.0 && errcnt < MAXERRCNT);

	if  (print_nogaps && nogaps)  printf( "no gaps\n" );

	return 0;

} /* end of main */



/*--------------------------------------------------------------------*/


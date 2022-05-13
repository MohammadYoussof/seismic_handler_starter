
/* file q2seed.c
 *      ========
 *
 * version 1, 27-Dec-93
 *
 * Writes SEED data records from input q-files
 * K. Stammler, 27-Dec-93
 */



#include <stdio.h>
#include <string.h>
#include BASECNST
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_QFUSRDEF
#include BC_TCUSRDEF
#include BC_CPAR
#include BC_DFQENTRY
/* to be removed */
#include "steimtmp.h"
/* only this ^ */
#include "wrsteim.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char     qfile[BC_FILELTH+1];   /* q-file name */
	long     qsize;                 /* q-file size */
	int      qtrcno;                /* number of traces in q-file */
	int      qtrclin;               /* number of header lines per trace */
	STATUS   status;                /* return status */
	int      i;                     /* counter */
	float    *datptr;               /* pointer to data */
	char     *starttime;            /* pointer to start time of trace */
	NTIME    ntime;                 /* numeric time */
	char     seedfile[BC_FILELTH+1];/* SEED output file */
	char     *station;              /* station name */
	char     comp;                  /* component */
	float    dt;                    /* sample distance */
	SeedBtimeT btime;               /* SEED btime */
	SeedDataHeaderT hdr;            /* SEED data header */

	/* executable code */

	pa_init( argc, argv );
	if  (pa_pnumber() != 1)  {
		printf( "*** Usage: q2seed <q-file> ***\n" );
		return 0;
	} /*endif*/
	strcpy( qfile, pa_pvalue(1) );

	status = BC_NOERROR;

	qf_size( qfile, &qsize, &qtrcno, &qtrclin, &status );
	if  (Severe(&status))  {
		printf( "*** error in qf_size, file %s\n", qfile );
		printf( "*** status %d\n", status );
		exit( 1 );
	} /*endif*/

	for  (i=0; i<qtrcno; i++)  {
		qf_read( qfile, i+1, TRUE, &datptr, &status );
		if  (Severe(&status))  {
			printf( "*** error in qf_read, trace %d\n", i+1 );
			printf( "*** status %d\n", status );
			exit( 1 );
		} /*endif*/
		starttime = qf_gets( QEC_T_START, &status );
		if  (Severe(&status))  {
			printf( "*** no start time, trace %d\n", i+1 );
			printf( "*** status %d\n", status );
			exit( 1 );
		} /*endif*/
		tc_t2n( starttime, &ntime, &status );
		if  (Severe(&status))  {
			printf( "*** error converting time, time %s\n", starttime );
			printf( "*** status %d\n", status );
			exit( 1 );
		} /*endif*/
		if  (ntime.year > 1900)  ntime.year -= 1900;
		if  (ntime.year > 100)  ntime.year -= 100;
		station = qf_gets( QEC_S_STATION, &status );
		if  (Severe(&status))  {
			printf( "*** no station, trace %d\n", i+1 );
			printf( "*** status %d\n", status );
			exit( 1 );
		} /*endif*/
		comp = qf_getc( QEC_C_COMP, &status );
		if  (Severe(&status))  {
			printf( "*** no start component, trace %d\n", i+1 );
			printf( "*** status %d\n", status );
			exit( 1 );
		} /*endif*/
		dt = qf_getr( QEC_R_DELTA, &status );
		if  (Severe(&status))  {
			printf( "*** no dt, trace %d\n", i+1 );
			printf( "*** status %d\n", status );
			exit( 1 );
		} /*endif*/
		sprintf( seedfile, "%s_%02d%02d%02d_%02d%02d.bh%c",
			station, ntime.year, ntime.month, ntime.day,
			ntime.min, ntime.sec, comp );
		btime.year = ntime.year;
		btime.day = tc_julian( ntime.year, ntime.month, ntime.day,
			&status );
		btime.hours = ntime.hour;
		btime.minutes = ntime.min;
		btime.seconds = ntime.sec;
		btime.frac_secs = ntime.ms * 10;
		strcpy( hdr.seqno, "0000001" );
		hdr.indicator = 'D';
		hdr.Reserved_bytes_A = 0;
		strcpy( hdr.station, station );
		for  (i=(int)strlen(hdr.station); i<5; i++)  hdr.station[i] = ' ';
		strcpy( hdr.locid, "  " );
		strcpy( hdr.channel, "BH" );
		hdr.channel[2] = comp;
		hdr.Reserved_bytes_B[0] = 0;
		hdr.Reserved_bytes_B[1] = 0;
		hdr.starttime = btime;
		hdr.no_of_samples = 0;
		hdr.smprate_fact = Nint( 1.0 / dt );
		hdr.smprate_mult = 1;
		hdr.activity = 0;
		hdr.ioflags = 0;
		hdr.quality = 0;
		hdr.no_of_blockettes = 0;
		hdr.correction = 0;
		hdr.databegin = sizeof(SeedDataHeaderT);
		hdr.first = 0;
		SteimHeaderPrototype( &hdr );
		printf( "writing file %s\n", seedfile );
		SteimWriteFloatData( datptr, qf_getl(QEC_L_DATALTH,&status),
			seedfile, &status );
		if  (Severe(&status))  {
			printf( "*** error writing SEED, trace %d\n", i+1 );
			printf( "*** status %d\n", status );
			exit( 1 );
		} /*endif*/
		free( datptr );
	} /*endfor*/

	return 0;

} /* end of main */


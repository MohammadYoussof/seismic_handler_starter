
/* file gse2_stream_calib.c
 *      ===================
 *
 * version 7, 15-Mar-2005
 *
 * returns calibration CAL2 & PAZ2 of stream at a given time
 * K. Stammler, 12-Mar-96
 */



#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include "sysbase.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "cpar.h"
#include "erusrdef.h"
#include "utusrdef.h"
#include "tcusrdef.h"
#include "seedcfg.h"
#include "seed_lib.h"


#define PI 3.14159265358979323846


int main( int argc, char *argv[] )
{
	/* local variables */
	STATUS   status;                   /* return status */
	float    calib;                    /* calibration value */
	char     stream[cBcLineLth+1];     /* stream string */
	char     ttime[cBcLineLth+1];      /* time */
	char     inpdir[cBcFileLth+1];     /* input directory */
	char     pazfile[cBcFileLth+1];    /* name of paz file */
	char     line[cBcLineLth+1];       /* current line */
	char     str[cBcLineLth+1];        /* scratch string */
	char     station[cBcLineLth+1];    /* station code */
	char     fdsn_chan[cBcLineLth+1];  /* FDSN channel code */
	char     *cptr, *dst;              /* pointers to char */
	char     instr[cBcLineLth+1];      /* instrument type */
	char     idstr[cBcLineLth+1];      /* net ID string */
	float    calper;                   /* calibration period in sec */
	float    smprate;                  /* sample rate in Hz */
	FILE     *fp;                      /* pointer to paz file */
	BOOLEAN  first;                    /* flag */
	NTIME    ntime;                    /* numeric time */
	float    tdiff;                    /* time difference */

	/* executable code */

	pa_init( argc, argv );
	if  (pa_pnumber() != 2)  {
		fprintf( stderr, "Usage: %s <stream> <time>\n", pa_progname() );
		return 1;
	} /*endif*/

	/* get parameters from command line */
	strcpy( stream, pa_pvalue(1) );
	strcpy( ttime, pa_pvalue(2) );

	/* get SEED_INPUTS */
	cptr = getenv( "SEED_INPUTS" );
	if  (cptr == NULL)  {
		fprintf( stderr, "%s: env SEED_INPUTS not defined\n", pa_progname() );
		return 1;
	} /*endif*/
	strcpy( inpdir, cptr );
	strcat( inpdir, "/gse/" );
	strcpy( pazfile, inpdir );

	status = cBcNoError;
	calib = SeedFindStreamCalibration( stream, ttime, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );

	/* get station code and FDSN channel from stream string */
	strcpy( str, stream );
	ut_cap( str );
	cptr = str;
	dst = station;
	first = TRUE;
	while  (*cptr != '\0')
		if  (*cptr == '-')  {
			if  (first)  {
				*dst = '\0';
				dst = fdsn_chan;
				first = FALSE;
			} /*endif*/
			cptr++;
		} else {
			*dst++ = *cptr++;
		} /*endif*/
	*dst = '\0';
	fdsn_chan[3] = '\0';  /* truncate channel */

	*idstr = '\0';
	if  (stream[0] == 'g' && stream[1] == 'r' && stream[2] >= 'a'
		&& stream[2] <= 'c' && stream[5] == 'b' && stream[6] == 'h')  {
		status = cBcNoError;
		if  (strncmp(stream,"gra1",4) == 0)  {
			tdiff = tc_tdiff( ttime, "10-may-2006", &status );
			if  (tdiff < 0.0)  {
				strcpy( idstr, "GRF-BH" );
			} else {
				strcpy( idstr, "GRSN-BH" );
			} /*endif*/
		} else if  (strncmp(stream,"gra2",4) == 0)  {
			tdiff = tc_tdiff( ttime, "9-oct-2006", &status );
			if  (tdiff < 0.0)  {
				strcpy( idstr, "GRF-BH" );
			} else {
				strcpy( idstr, "GRSN-BH" );
			} /*endif*/
		} else if  (strncmp(stream,"gra3",4) == 0)  {
			tdiff = tc_tdiff( ttime, "11-oct-2006", &status );
			if  (tdiff < 0.0)  {
				strcpy( idstr, "GRF-BH" );
			} else {
				strcpy( idstr, "GRSN-BH" );
			} /*endif*/
		} else if  (strncmp(stream,"gra4",4) == 0)  {
			tdiff = tc_tdiff( ttime, "25-sep-2006", &status );
			if  (tdiff < 0.0)  {
				strcpy( idstr, "GRF-BH" );
			} else {
				strcpy( idstr, "GRSN-BH" );
			} /*endif*/
		} else if  (strncmp(stream,"grb1",4) == 0)  {
			tdiff = tc_tdiff( ttime, "14-sep-2006", &status );
			if  (tdiff < 0.0)  {
				strcpy( idstr, "GRF-BH" );
			} else {
				strcpy( idstr, "GRSN-BH" );
			} /*endif*/
		} else if  (strncmp(stream,"grb2",4) == 0)  {
			tdiff = tc_tdiff( ttime, "21-jun-2006", &status );
			if  (tdiff < 0.0)  {
				strcpy( idstr, "GRF-BH" );
			} else {
				strcpy( idstr, "GRSN-BH" );
			} /*endif*/
		} else if  (strncmp(stream,"grb3",4) == 0)  {
			tdiff = tc_tdiff( ttime, "19-oct-2006", &status );
			if  (tdiff < 0.0)  {
				strcpy( idstr, "GRF-BH" );
			} else {
				strcpy( idstr, "GRSN-BH" );
			} /*endif*/
		} else if  (strncmp(stream,"grb4",4) == 0)  {
			tdiff = tc_tdiff( ttime, "28-sep-2006", &status );
			if  (tdiff < 0.0)  {
				strcpy( idstr, "GRF-BH" );
			} else {
				strcpy( idstr, "GRSN-BH" );
			} /*endif*/
		} else if  (strncmp(stream,"grb5",4) == 0)  {
			tdiff = tc_tdiff( ttime, "7-aug-2006", &status );
			if  (tdiff < 0.0)  {
				strcpy( idstr, "GRF-BH" );
			} else {
				strcpy( idstr, "GRSN-BH" );
			} /*endif*/
		} else if  (strncmp(stream,"grc1",4) == 0)  {
			tdiff = tc_tdiff( ttime, "15-mar-2005", &status );
			if  (tdiff < 0.0)  {
				strcpy( idstr, "GRF-BH" );
			} else {
				strcpy( idstr, "GRSN-BH" );
			} /*endif*/
		} else if  (strncmp(stream,"grc2",4) == 0)  {
			tdiff = tc_tdiff( ttime, "9-may-2006", &status );
			if  (tdiff < 0.0)  {
				strcpy( idstr, "GRF-BH" );
			} else {
				strcpy( idstr, "GRSN-BH" );
			} /*endif*/
		} else if  (strncmp(stream,"grc3",4) == 0)  {
			tdiff = tc_tdiff( ttime, "26-jul-2006", &status );
			if  (tdiff < 0.0)  {
				strcpy( idstr, "GRF-BH" );
			} else {
				strcpy( idstr, "GRSN-BH" );
			} /*endif*/
		} else if  (strncmp(stream,"grc4",4) == 0)  {
			tdiff = tc_tdiff( ttime, "19-sep-2006", &status );
			if  (tdiff < 0.0)  {
				strcpy( idstr, "GRF-BH" );
			} else {
				strcpy( idstr, "GRSN-BH" );
			} /*endif*/
		} else {
			strcpy( idstr, "GRF-BH" );
		} /*endif*/
	} else if  (strcmp(station,"BFO") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"BRG") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"BRNL") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"BSEG") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"BUG") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"CLL") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"CLZ") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"FBE") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "SXNET-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "SXNET-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "SXNET-LH" );
	} else if  (strcmp(station,"FUR") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"GEC2") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
	} else if  (strcmp(station,"GRFO") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRFO-BH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRFO-LH" );
	} else if  (strcmp(station,"GSH") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"GTTG") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"GUNZ") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "SXNET-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "SXNET-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "SXNET-LH" );
	} else if  (strcmp(station,"HAM") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"HLG") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"IBBN") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"LID") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"MANZ") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"MOX") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"NEUB") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"NRDL") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"PLN") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "SXNET-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "SXNET-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "SXNET-LH" );
	} else if  (strcmp(station,"PST") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "SXNET-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "SXNET-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "SXNET-LH" );
	} else if  (strcmp(station,"RGN") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"RJOB") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"ROHR") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "SXNET-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "SXNET-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "SXNET-LH" );
	} else if  (strcmp(station,"ROTZ") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"RUE") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"SCHF") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "SXNET-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "SXNET-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "SXNET-LH" );
	} else if  (strcmp(station,"STU") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"TANN") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"TNS") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"TRIB") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "SXNET-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "SXNET-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "SXNET-LH" );
	} else if  (strcmp(station,"UBBA") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} else if  (strcmp(station,"WERD") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "SXNET-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "SXNET-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "SXNET-LH" );
	} else if  (strcmp(station,"WERN") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "SXNET-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "SXNET-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "SXNET-LH" );
	} else if  (strcmp(station,"WET") == 0)  {
		if  (fdsn_chan[0] == 'B')  strcpy( idstr, "GRSN-BH" );
		if  (fdsn_chan[0] == 'H')  strcpy( idstr, "GRSN-HH" );
		if  (fdsn_chan[0] == 'L')  strcpy( idstr, "GRSN-LH" );
	} /*endif*/

	/* set parameters for each stream */
	if  (strcmp(idstr,"GRF-BH") == 0)  {
		strcat( pazfile, "gse2_sts1.paz" );
		strcpy( instr, "STS-1" );
		calper = 1.0;
		smprate = 20.0;
	} else if  (strcmp(idstr,"GRSN-BH") == 0)  {
		strcat( pazfile, "gse2_sts2.paz" );
		strcpy( instr, "STS-2" );
		calper = 1.0;
		smprate = 20.0;
	} else if  (strcmp(idstr,"GRSN-LH") == 0)  {
		strcat( pazfile, "gse2_sts2.paz" );
		strcpy( instr, "STS-2" );
		calper = 10.0;
		smprate = 1.0;
	} else if  (strcmp(idstr,"GRFO-BH") == 0)  {
		strcat( pazfile, "gse2_ks36000i.paz" );
		strcpy( instr, "KS360i" );
		calper = 1.0;
		smprate = 20.0;
	} else if  (strcmp(idstr,"SXNET-BH") == 0)  {
		strcat( pazfile, "gse2_sxnet.paz" );
		strcpy( instr, "LE3D5S" );
		calper = 1.0;
		smprate = 20.0;
	} else {
		fprintf( stderr, "%s: illegal stream %s\n", pa_progname(), stream );
		return 1;
	} /*endif*/

	/* this is a velocity calibration value, so divide by 2*pi*calper */
	calib /= 2.0*PI*calper;

	/* numeric time */
	status = cBcNoError;
	tc_t2n( ttime, &ntime, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );

	printf(
		"CAL2 %5s %3s      %6s %10.4e %7.3f %10.5f %04d/%02d/%02d %02d:%02d\n",
		station, fdsn_chan, instr, calib, calper, smprate, ntime.year,
		ntime.month, ntime.day, ntime.hour, ntime.min );

	/* print paz file */
	fp = fopen( pazfile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: couldn't open paz file %s\n",
			pa_progname(), pazfile );
		return 1;
	} /*endif*/
	while  (fgets(line,cBcLineLth,fp) != NULL)  printf( "%s", line );
	fclose( fp );

	return 0;

} /* end of main */

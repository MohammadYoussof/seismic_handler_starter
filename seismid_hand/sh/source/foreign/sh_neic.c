
/* file SH_NEIC.C
 *      =========
 *
 * version 4, 26-Aug-92
 *
 * SH interface to ASCII files created by program sonic1
 * K. Stammler, 2-AUG-91
 */


#include <stdio.h>
#include <string.h>
#include <math.h>
#include BASECNST
#include BC_SYSBASE
#include BC_TCUSRDEF
#include BC_QFUSRDEF
#include BC_DFQENTRY
#include "sh_neic.h"

#define MAXORDER 100
#define STATLTH 8
#define MAXDATLTH 30000
#define SCRATCHFILE "POLES-ZEROES.000"


/* global variables */
static float    ncv_dat[MAXDATLTH];           /* data array */
static char     ncv_starttime[BC_LINELTH+1];  /* start time */
static long     ncv_datlth;                   /* length of data array */
static float    ncv_dt;                       /* sample distance in sec */
static char     ncv_comp;                     /* component */
static char     ncv_station[STATLTH+1];       /* station string */


/*----------------------------------------------------------------------------*/



void sh_neic_trace( char file[], int rec, STATUS *status )

/* reads NEIC ASCII file
 *
 * parameters of routine
 * char       file[];     input; ASCII filename
 * int        rec;        input; record (not used)
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	char     line[BC_LONGSTRLTH+1];/* current line */
	FILE     *inf;                 /* file pointer */
	int      slth;                 /* string length */
	float    rpole[MAXORDER];      /* poles & zeroes */
	float    ipole[MAXORDER];
	float    rzero[MAXORDER];
	float    izero[MAXORDER];
	int      no_of_poles;          /* number of poles */
	int      no_of_zeroes;         /* number of zeroes */
	NTIME    nstart;               /* numeric start time */
	int      julday;               /* julian day */
	float    rsec;                 /* seconds in floating format */
	int      i;                    /* counter */
	long     section;              /* length of current data section */
	float    *dp;                  /* moving pointer */
	float    dmy1, dmy2, dmy3;

	/* executable code */

	inf = fopen( file, "r" );
	if  (inf == NULL)  {
		*status = 8001;
		return;
	} /*endif*/
	if  (fgets( line, BC_LONGSTRLTH, inf ) == NULL)  {
		*status = 8002;
		return;
	} /*endif*/

	strncpy( ncv_station, line, 4 );
	if  (ncv_station[3] == ' ')  ncv_station[3] = '\0';
	ncv_comp = line[7];

	sscanf( line+9, " %f %f %f %d %d %d %d %f %f",
		&dmy1, &dmy2, &dmy3, &nstart.year, &julday, &nstart.hour,
		&nstart.min, &rsec, &ncv_dt );
	ncv_dt = 1.0 / ncv_dt;
	tc_dayofmn( nstart.year, julday, &nstart.month, &nstart.day, status );
	if  (Severe(status))  return;
	nstart.sec = floor( rsec );
	nstart.ms = Nint( (rsec-(float)nstart.sec)*1000.0 );
	/* check special time: 60 sec (occurred at least once) */
	if  (nstart.sec == 60)  {
		nstart.sec = 0;
		tc_nadd( &nstart, 1.0, &nstart, status );
		if  (Severe(status))  return;
	} /*endif*/
	tc_n2t( &nstart, ncv_starttime, status );
	if  (Severe(status))  return;

	/* reading poles and zeroes */
	fscanf( inf, "%d\n", &no_of_poles );
	if  (no_of_poles > MAXORDER)  return;
	for  (i=0; i<no_of_poles; i++)
		fscanf( inf, "%f\n%f\n", rpole+i, ipole+i );
	fscanf( inf, "%d\n", &no_of_zeroes );
	if  (no_of_zeroes > MAXORDER)  return;
	for  (i=0; i<no_of_zeroes; i++)
		fscanf( inf, "%f\n%f\n", rzero+i, izero+i );

	ncv_datlth = 0;
	dp = ncv_dat;
	for  (;;)  {
		if  (feof(inf))  break;
		if  (fscanf( inf, "%d\n", &section ) != 1)  {
			/* *status = 8005; */
			printf( "*** data read error ***\n" );
			break;
		} /*endif*/
		ncv_datlth += section;
		if  (ncv_datlth > MAXDATLTH)  {
			*status = 8003;
			printf( "*** too many samples ***\n" );
			return;
		} /*endif*/
		for  (i=0; i<section; i++)
			fscanf( inf, "%f\n", dp++ );
	} /*endfor*/

	fclose( inf );

} /* end of sh_neic_trace */



/*----------------------------------------------------------------------------*/



void sh_neic_geti( char entryname[], long *info, STATUS *status )

/* returns integer value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * long       *info;           output; returned info
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	if  (strcmp(entryname,"LENGTH") == 0)  {
		*info = ncv_datlth;
	} else {
		printf( "*** NEIC: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_neic_geti */



/*----------------------------------------------------------------------------*/



void sh_neic_getr( char entryname[], float *info, STATUS *status )

/* returns float value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * float      *info;           output; returned info
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	if  (strcmp(entryname,"DELTA") == 0)  {
		*info = ncv_dt;
	} else {
		printf( "*** NEIC: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_neic_getr */



/*----------------------------------------------------------------------------*/



void sh_neic_gets( char entryname[], int maxlth, char info[], STATUS *status )

/* returns string value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * int        maxlth;          input; maximum length of output string
 * char       info[];          output; returned info
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	if  (maxlth < BC_LINELTH)  {
		*status = 8004;
		return;
	} /*endif*/

	if  (strcmp(entryname,"START") == 0)  {
		strcpy( info, ncv_starttime );
	} else if  (strcmp(entryname,"STATION") == 0)  {
		strcpy( info, ncv_station );
	} else {
		printf( "*** NEIC: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_neic_gets */



/*----------------------------------------------------------------------------*/



void sh_neic_getc( char entryname[], char *info, STATUS *status )

/* returns character value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * char       *info;           output; returned info
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	if  (strcmp(entryname,"COMP") == 0)  {
		*info = ncv_comp;
	} else {
		printf( "*** NEIC: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_neic_getc */



/*----------------------------------------------------------------------*/



void sh_neic_read( float smp[] )

/* returns sample data
 *
 * parameter of routine
 * float     smp[];     output; sample data
 */
{
	/* local variables */
	float    *s, *end;   /* sample pointers */

	/* executable code */
	s = ncv_dat;
	end = ncv_dat + ncv_datlth;
	while  (s < end)
		*smp++ = *s++;

} /* end of sh_neic_read */



/*----------------------------------------------------------------------------*/


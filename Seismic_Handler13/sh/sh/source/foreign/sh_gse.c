
/* file sh_gse.c
 *      ========
 *
 * version 10, 10-Jul-96
 *
 * GSE reader for SH (foreign format input)
 * K. Stammler, 27-Aug-92
 *
 * version 2 (9-Sep-92): changed header input
 * version 9 (14-Mar-96): correct handling of displacement calibrations
 */



#include <stdio.h>
#include <string.h>
#include BASECNST
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_TCUSRDEF
#include "sh_gse.h"



/* constants */

#define GSC_MAXTRC 40
	/* maximum number of traces in one GSE file */
#define GSC_NAMELTH 10
	/* maximum length of station name */
#define PI    3.14159265358979323846
	/* guess ... */


/* prototypes of local routines */
static void sh_read_single_trace( FILE *fp, int trcno, STATUS *status );


/* global variables */
static char     gsv_gsefile[BC_FILELTH+1]; /* GSE file */
static int      gsv_currtrc;               /* current trace in process */
static int      gsv_traceno;               /* number of traces */
static float    *gsv_dat[GSC_MAXTRC];      /* pointer to sample data */
static long     gsv_length[GSC_MAXTRC];    /* length of traces */
static char     gsv_station[GSC_MAXTRC][GSC_NAMELTH+1];  /* station names */
static char     gsv_comp[GSC_MAXTRC];      /* components */
static char     gsv_chan1[GSC_MAXTRC];     /* first channel character */
static char     gsv_chan2[GSC_MAXTRC];     /* second channel character */
static float    gsv_dt[GSC_MAXTRC];        /* sample rates */
static float    gsv_calib[GSC_MAXTRC];     /* calibration constants */
static TIME     gsv_start[GSC_MAXTRC];     /* start time of traces */


/*----------------------------------------------------------------------------*/



void sh_gse_trace( char file[], int rec, STATUS *status )

/* prepares GSE file for passing to SH
 *
 * parameters of routine
 * char       file[];    input; name of GSE file
 * int        rec;       input; number of record to read
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	FILE     *fp;                  /* pointer to GSE file */
	int      t;                    /* counter */

	/* executable code */

	/* check if already read in */
	gsv_currtrc = rec-1;
	if  (strcmp(file,gsv_gsefile) == 0)  {
		if  (gsv_currtrc >= gsv_traceno || gsv_currtrc < 0)  {
			*status = GSE_ILLTRC;
			return;
		} /*endif*/
		if  (gsv_length[gsv_currtrc] == 0)  {
			*status = GSE_READTWICE;
			return;
		} /*endif*/
		return;
	} /*endif*/

	/* store new file name and reset GSE variables */
	if  (strlen(file) > BC_FILELTH)  {
		*status = GSE_NAMEOVFL;
		return;
	} /*endif*/
	strcpy( gsv_gsefile, file );
	for  (t=0; t<gsv_traceno; t++)  {
		if  (gsv_length[t] != 0)
			sy_deallocmem( gsv_dat[t] );
		gsv_length[t] = 0;
		gsv_dt[t] = 0.0;
		gsv_station[t][0] = '\0';
		gsv_comp[t] = '\0';
		gsv_chan1[t] = '\0';
		gsv_chan2[t] = '\0';
	} /*endfor*/
	gsv_traceno = 0;

	/* open GSE file */
	fp = sy_fopen( file, "r" );
	if  (fp == NULL)  {
		*status = GSE_OPENGSE;
		return;
	} /*endif*/

	/* read traces until end of file */
	do  {
		if  (gsv_traceno == GSC_MAXTRC)  {
			*status = GSE_TOOMANY;
			break;
		} /*endif*/
		sh_read_single_trace( fp, gsv_traceno++, status );
		if  (Severe(status))  break;
	}  while (gsv_length[gsv_traceno-1] != 0);

	gsv_traceno--;
	sy_fclose( fp );
	if  (Severe(status))  return;

	if  (gsv_currtrc >= gsv_traceno || gsv_currtrc < 0)  {
		*status = GSE_ILLTRC;
		return;
	} /*endif*/

} /* end of sh_gse_trace */



/*----------------------------------------------------------------------------*/



void sh_gse_geti( char entryname[], long *info, STATUS *status )

/* returns integer value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * long       *info;           output; returned info
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	if  (gsv_length[gsv_currtrc] == 0)  {
		*status = GSE_READTWICE;
		return;
	} /*endif*/

	if  (strcmp(entryname,"LENGTH") == 0)  {
		*info = gsv_length[gsv_currtrc];
	} else {
		printf( "*** GSE: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_gse_geti */



/*----------------------------------------------------------------------------*/



void sh_gse_getr( char entryname[], float *info, STATUS *status )

/* returns float value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * float      *info;           output; returned info
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	if  (gsv_length[gsv_currtrc] == 0)  {
		*status = GSE_READTWICE;
		return;
	} /*endif*/

	if  (strcmp(entryname,"DELTA") == 0)  {
		*info = gsv_dt[gsv_currtrc];
	} else if  (strcmp(entryname,"CALIB") == 0)  {
		*info = gsv_calib[gsv_currtrc];
	} else {
		printf( "*** GSE: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_gse_getr */



/*----------------------------------------------------------------------------*/



void sh_gse_gets( char entryname[], int maxlth, char info[], STATUS *status )

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

	if  (gsv_length[gsv_currtrc] == 0)  {
		*status = GSE_READTWICE;
		return;
	} /*endif*/

	if  (maxlth < BC_LINELTH)  {
		*status = GSE_NAMEOVFL;
		return;
	} /*endif*/

	if  (strcmp(entryname,"START") == 0)  {
		tc_a2t( gsv_start+gsv_currtrc, info, status );
	} else if  (strcmp(entryname,"STATION") == 0)  {
		strcpy( info, gsv_station[gsv_currtrc] );
	} else {
		printf( "*** GSE: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_gse_gets */



/*----------------------------------------------------------------------------*/



void sh_gse_getc( char entryname[], char *info, STATUS *status )

/* returns character value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * char       *info;           output; returned info
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	if  (gsv_length[gsv_currtrc] == 0)  {
		*status = GSE_READTWICE;
		return;
	} /*endif*/

	if  (strcmp(entryname,"COMP") == 0)  {
		*info = Cap( gsv_comp[gsv_currtrc] );
	} else if  (strcmp(entryname,"CHAN1") == 0)  {
		*info = Cap( gsv_chan1[gsv_currtrc] );
	} else if  (strcmp(entryname,"CHAN2") == 0)  {
		*info = Cap( gsv_chan2[gsv_currtrc] );
	} else {
		printf( "*** GSE: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_gse_getc */



/*----------------------------------------------------------------------*/



void sh_gse_read( float smp[] )

/* returns sample data
 *
 * parameter of routine
 * float     smp[];     output; sample data
 */
{
	/* local variables */
	float    *s, *end;   /* sample pointers */

	/* executable code */

	s = gsv_dat[gsv_currtrc];
	end = gsv_dat[gsv_currtrc] + gsv_length[gsv_currtrc];
	while  (s < end)
		*smp++ = *s++;

	/* reset trace */
	sy_deallocmem( gsv_dat[gsv_currtrc] );
	gsv_length[gsv_currtrc] = 0;
	gsv_dt[gsv_currtrc] = 0.0;
	gsv_station[gsv_currtrc][0] = '\0';
	gsv_comp[gsv_currtrc] = '\0';
	gsv_chan1[gsv_currtrc] = '\0';
	gsv_chan2[gsv_currtrc] = '\0';

} /* end of sh_gse_read */




/*----------------------------------------------------------------------------*/



static void sh_read_single_trace( FILE *fp, int trcno, STATUS *status )

/* reads a single trace from GSE file.  The header information and sample data
 * are stored in the global variables.  If gsv_length[trcno] == 0, then no
 * data are read.
 *
 * parameters of routine
 * FILE       *fp;      input; pointer to GSE file
 * int        trcno;    input; current trace number
 * STATUS     *status;  output; return status
 */
{
	/* local variables */
	char     line[BC_LINELTH+1];   /* current line of GSE file */
	NTIME    nstart;               /* numerical start time */
	int      julday;               /* julian day */
	char     str[BC_LINELTH+1];    /* scratch string */
	char     str2[BC_LINELTH+1];   /* scratch string */
	char     str4[BC_LINELTH+1];   /* scratch string */
	long     s;                    /* sample counter */
	float    *p;                   /* moving pointer */
	long     checksum;             /* checksum */
	long     smpsum;               /* sum of samples */
	int      integration_cnt;      /* number of integrations */
	char     calib_unit;           /* calibration unit (0=dsp,1=vel,2=acc) */
	float    calib_per;            /* calibration period */

	/* executable code */

	/* read off lines until WID1 string */
	do  {
		if  (fgets(line,BC_LINELTH,fp) == NULL)  {
			gsv_length[trcno] = 0;
			return;
		} /*endif*/
	}  while (strncmp(line,"WID1",4) != 0);

	/* read date and other info from this line */
#	ifdef XXX
	strncpy( str, line+5, 4 );
	str[4] = '\0';
	sscanf( str, "%d", &(nstart.year) );
	strncpy( str, line+9, 3 );
	str[3] = '\0';
	sscanf( str, "%d", &julday );
	tc_dayofmn( nstart.year, julday, &(nstart.month), &(nstart.day), status );
	if  (Severe(status))  return;
	sscanf( line+14, "%d\n%d\n%d\n%d\n%ld\n%s",
		&(nstart.hour), &(nstart.min), &(nstart.sec), &(nstart.ms),
		gsv_length+trcno, str );
#	endif
	s = 4;
	while  (line[s] == ' ')  s++;
	sscanf( line+s, "%s\n%d\n%d\n%d\n%d\n%ld\n%s",
		str2, &(nstart.hour), &(nstart.min), &(nstart.sec),
		&(nstart.ms), gsv_length+trcno, str );
	if  (strlen(str) > GSC_NAMELTH)  {
		*status = GSE_NAMEOVFL;
		return;
	} /*endif*/
	if  (gsv_length[trcno] == 0)  {
		*status = GSE_ZEROLTH;
		return;
	} /*endif*/
	sscanf( str2+4, "%d", &julday );
	str2[4] = '\0';
	sscanf( str2, "%d", &(nstart.year) );
	tc_dayofmn( nstart.year, julday, &(nstart.month), &(nstart.day), status );
	if  (Severe(status))  return;
	tc_n2a( &nstart, gsv_start+trcno, status );
	if  (Severe(status))  return;
	strcpy( gsv_station[trcno], str );
	sscanf( line+52, "%s", str4 );
	s = strlen( str4 ) - 1;
	gsv_comp[trcno] = Cap( str4[s] );
	if  (s > 1)  gsv_chan1[trcno] = Cap( str4[s-2] );
	if  (s > 0)  gsv_chan2[trcno] = Cap( str4[s-1] );
	if  (sscanf( line+55, "%f", gsv_dt+trcno ) != 1)  {
		*status = GSE_NOSMPRATE;
		return;
	} /*endif*/
	if  (gsv_dt[trcno] <= 0.0)  {
		*status = GSE_ZEROSMP;
		return;
	} /*endif*/
	gsv_dt[trcno] = 1.0 / gsv_dt[trcno];

	/* get integration count */
	if  (strlen(line) < 80)  {
		integration_cnt = 0;
	} else if  (line[79] < '0' || line[79] > '9')  {
		integration_cnt = 0;
	} else {
		integration_cnt = line[79] - '0';
	} /*endif*/

	/* get calibration from second line */
	if  (fgets(line,BC_LINELTH,fp) == NULL)  {
		*status = GSE_READGSE;
		return;
	} /*endif*/
	calib_unit = line[9];  /* store unit descriptor */
	line[9] = '\0';        /* now terminate number */
	/* read calibration value from line */
	if  (sscanf( line, "%f", gsv_calib+trcno ) != 1)  {
		*status = GSE_NOCALIB;
		return;
	} /*endif*/
	if  (calib_unit < '0' || calib_unit > '2')  {
		fprintf( stderr, "sh_gse: No calibration unit specified. " );
		fprintf( stderr, "Leave calib %f unchanged\n", gsv_calib[trcno] );
	} else if  (calib_unit != '1')  {
		/* get calibration period */
		line[17] = '\0';
		if  (sscanf(line+10,"%f",&calib_per) != 1)  {
			fprintf( stderr, "sh_gse: No calib-period found.  Assume 1 sec\n" );
			calib_per = 1.0;
		} /*endif*/
		if  (calib_per <= 0.0)  {
			fprintf( stderr, "sh_gse: Illegal calib-period.  Change to 1 sec\n" );
			calib_per = 1.0;
		} /*endif*/
		if  (calib_unit == '0')  gsv_calib[trcno] *= 2.0 * PI * calib_per;
		if  (calib_unit == '2')  gsv_calib[trcno] /= 2.0 * PI * calib_per;
	} /*endif*/

	/* read off lines until DAT1 string */
	do  {
		if  (fgets(line,BC_LINELTH,fp) == NULL)  {
			*status = GSE_READGSE;
			return;
		} /*endif*/
	}  while (strncmp(line,"DAT1",4) != 0);

	/* allocate memory */
	gsv_dat[trcno] = (float *)sy_allocmem( gsv_length[trcno],
		(int)sizeof(float), status );
	if  (Severe(status))  return;

	/* read samples */
	p = gsv_dat[trcno];
	for  (s=0; s<gsv_length[trcno]; s++)
		fscanf( fp, "%f\n", p++ );

	/* read checksum */
	do  {
		if  (fgets(line,BC_LINELTH,fp) == NULL)  {
			*status = GSE_READGSE;
			return;
		} /*endif*/
		if  (strncmp(line,"WID1",4) == 0)  {
			*status = GSE_NOCHECKSUM;
			return;
		} /*endif*/
	}  while (strncmp(line,"CHK1",4) != 0);
	sscanf( line+4, "%ld", &checksum );

	/* integrate data if requested */
	p = gsv_dat[trcno];
	while  (integration_cnt > 0)  {
		for  (s=integration_cnt; s<gsv_length[trcno]; s++)
			p[s] += p[s-1];
		integration_cnt--;
	} /*endwhile*/

	/* check checksum */
	smpsum = 0;
	for  (s=0; s<gsv_length[trcno]; s++)  {
		smpsum += Nlong( *p );
		p++;
	} /*endfor*/
	if  (smpsum != checksum)  {
		printf( "*** checksum is: %ld,  should be: %ld ***\n", smpsum, checksum );
		/* *status = GSE_WRONGCHECK; */
		/* return; */
	} /*endif*/

	/* apply calibration */
	if  (gsv_calib[trcno] != 0.0 && gsv_calib[trcno] != 1.0)  {
		p = gsv_dat[trcno];
		for  (s=0; s<gsv_length[trcno]; s++)
			*p++ *= gsv_calib[trcno];
	} /*endif*/

} /* end of sh_read_single_trace */



/*----------------------------------------------------------------------------*/


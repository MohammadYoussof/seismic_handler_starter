
/* file demultiplex.c
 *      =============
 *
 * version 4, 24-Jan-96
 *
 * demultiplexes Mini-SEED files, one file per stream
 * K. Stammler, 23-Dec-94
 */


#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include BASECNST
#include BC_SYSBASE
#include BC_CPAR
#include BC_ERUSRDEF
#include "seedcfg.h"
#include "seed_lib.h"


#define MAXSTREAM 20
typedef struct {
	FILE       *fp;
	int        reccnt;
	char       name[BC_FILELTH+1];
} FileList;


#define FFN_TOO_MANY -1
#define FFN_ILL_NAME -2
#define FFN_ERR_OPEN -3



/* global variables */
static SeedSetupParT seedv_setup;           /* SEED setup parameters */
static FileList      seedv_fl[MAXSTREAM];   /* file list */



/* prototypes of local routines */
static int FindFileNumber( char station[], char chan[] );



int main( int argc, char *argv[] )
{
	/* local variables */
	STATUS   status;                    /* return status */
	SeedSbyteT *seedrec;                /* pointer to SEED record */
	SeedDataHeaderT *seedhdr;           /* pointer to seed data header */
	char     inpfile[BC_FILELTH+1];     /* name of input file */
	char     outfile[BC_FILELTH+1];     /* name of output file */
	FILE     *inp;                      /* file pointer to input file */
	int      fsize;                     /* size of file in bytes */
	int      numrecs;                   /* number of SEED records */
	int      i, j;                      /* counter */
	int      read_ret;                  /* return value of fread */
	int      write_ret;                 /* return value of fwrite */
	char     curr_station[BC_LINELTH+1];/* current station */
	char     curr_chan[BC_LINELTH+1];   /* current channel */
	char     ctmp;                      /* scratch */
	int      fileno;                    /* file number */
	int      skiprec_cnt;               /* counts records skipped */
	int      readrec_cnt;               /* counts records read */
	int      writerec_cnt;              /* counts records written */
	BOOLEAN  print_info;                /* print info */

	/* executable code */

	for  (i=0; i<MAXSTREAM; i++)  {
		seedv_fl[i].fp = NULL;
		seedv_fl[i].reccnt = 0;
		seedv_fl[i].name[0] = '\0';
	} /*endfor*/

	status = BC_NOERROR;

	pa_init( argc, argv );
	if  (pa_pnumber() != 1)  {
		fprintf( stderr, "*** Usage: %s <inpfile> ***\n", argv[0] );
		return 1;
	} /*endif*/
	strcpy( inpfile, pa_pvalue(1) );
	print_info = FALSE;
	if  (pa_qspecified("-l"))  print_info = TRUE;

	/* get other parameters */
	SeedSetup( &seedv_setup, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* allocate memory for SEED record */
	seedrec = (SeedSbyteT *)sy_allocmem( 1, seedv_setup.seed_rec_size,
		&status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	seedhdr = (SeedDataHeaderT *)seedrec;

	inp = fopen( inpfile, "rb" );
	if  (inp == NULL)  {
		fprintf( stderr, "*** %s: file %s not found\n", argv[0], inpfile );
		return 1;
	} /*endif*/

	/* get length of file */
	fseek( inp, 0, 2 );
	fsize = ftell( inp );
	if  (fsize % seedv_setup.seed_rec_size != 0)  {
		fprintf( stderr, "*** %s: illegal size %ld bytes of SEED file %s\n",
			fsize, inpfile );
		fclose( inp );
		return 1;
	} /*endif*/
	numrecs = (int)(fsize / (long)seedv_setup.seed_rec_size);
	fseek( inp, 0, 0 );

	skiprec_cnt = readrec_cnt = writerec_cnt = 0;

	/* copy records */
	for  (i=0; i<numrecs; i++)  {
		/* get next record */
		read_ret = fread( (char *)seedrec, seedv_setup.seed_rec_size, 1, inp );
		if  (read_ret != 1)  {
			fprintf( stderr, "*** %s: read error on file %s\n",
				argv[0], inpfile );
			fclose( inp );
			return 1;
		} /*endif*/
		readrec_cnt++;
		/* skip records with no data */
		if  (seedrec[6] != 'D')  {skiprec_cnt++; continue;}
		/* get station and channel name */
		strncpy( curr_station, seedhdr->statcode, 5 );
		curr_station[5] = '\0';
		j = 4;  while (curr_station[j] <= ' ')  {curr_station[j] = '\0'; j--;}
		strncpy( curr_chan, seedhdr->channel, 3 );
		curr_chan[3] = '\0';
		j = 2;  while (curr_chan[j] <= ' ')  {curr_chan[j] = '\0'; j--;}
		/* get file number */
		fileno = FindFileNumber( curr_station, curr_chan );
		if  (fileno < 0)  {
			if  (fileno == FFN_TOO_MANY)  {
				fprintf( stderr, "%s: too many streams\n", pa_progname() );
			} else if  (fileno == FFN_ERR_OPEN)  {
				fprintf( stderr, "%s: error opening file\n", pa_progname() );
			} else if  (fileno == FFN_ILL_NAME)  {
				fprintf( stderr, "%s: illegal stream name\n", pa_progname() );
			} else {
				fprintf( stderr, "%s: illegal status code\n", pa_progname() );
			} /*endif*/
			continue;
		} /*endif*/
		/* update record number and write record to file */
		ctmp = seedrec[6];
		sprintf( (char *)seedrec, "%06d", ++(seedv_fl[fileno].reccnt) );
		seedrec[6] = ctmp;
		write_ret = fwrite( (char *)seedrec, seedv_setup.seed_rec_size, 1,
			seedv_fl[fileno].fp );
		if  (write_ret != 1)  {
			fprintf( stderr, "*** %s: write error on file %s\n",
				argv[0], seedv_fl[fileno].name );
			fclose( inp );
			fclose( seedv_fl[fileno].fp );
			return 1;
		} /*endif*/
		writerec_cnt++;
	} /*endif*/

	fclose( inp );
	for  (i=0; i<MAXSTREAM; i++)
		if  (seedv_fl[i].fp != NULL)
			fclose( seedv_fl[i].fp );

	if  (print_info)
		printf( "records read %d, written %d, skipped %d\n",
			readrec_cnt, writerec_cnt, skiprec_cnt );

	return 0;

} /* end of main */



/*----------------------------------------------------------------------------*/



static int FindFileNumber( char station[], char chan[] )

/* returns file number of correspondend file.  Opens new file if necessary.
 * Returns FFN_... error codes on error.
 *
 * parameters of routine
 * char       station[];     input; station name
 * char       chan[];        input; channel name
 */
{
	/* local variables */
	int      i;                    /* counter */
	char     fname[BC_FILELTH+1];  /* filename */

	/* executable code */

	i = 0;
	while  (station[i] != '\0')  {
		if  (!isalnum(station[i]))  return FFN_ILL_NAME;
		i++;
	} /*endwhile*/
	i = 0;
	while  (chan[i] != '\0')  {
		if  (!isalnum(chan[i]))  return FFN_ILL_NAME;
		i++;
	} /*endwhile*/

	if  (chan[0] > ' ')  {
		sprintf( fname, "%s.%s", station, chan );
	} else {
		sprintf( fname, "%s.blank", station );
	} /*endif*/

	for  (i=0; i<MAXSTREAM; i++)  {
		if  (seedv_fl[i].fp == NULL)  break;
		if  (strcmp(fname,seedv_fl[i].name) == 0)  return i;
	} /*endfor*/
	if  (i == MAXSTREAM)  return FFN_TOO_MANY;

	strcpy( seedv_fl[i].name, fname );
	seedv_fl[i].fp = fopen( fname, "wb" );
	if  (seedv_fl[i].fp == NULL)  {
		fprintf( stderr, "demultiplex: error opening file %s\n", fname );
		return FFN_ERR_OPEN;
	} /*endif*/
	seedv_fl[i].reccnt = 0;
	return i;

} /* end of FindFileNumber */



/*----------------------------------------------------------------------------*/


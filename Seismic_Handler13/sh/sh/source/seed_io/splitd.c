
/* file splitd.c
 *      ========
 *
 * version 1, 11-Feb-2000
 *
 * Splits MiniSEED files into single record files.
 * Parameters: record size, sleeptime, output path,
 *    input files, start record numbers, debug
 * K. Stammler, 11-Feb-2000
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>


#define STRLTH 132
#define MAXFILES 10
#define MAXRECSIZE 8192

typedef struct _param {
	int        debug;             /* debug flag */
	int        recsize;           /* seed record size in bytes */
	int        sleeptime;         /* sleep time in sec */
	char       outpath[STRLTH+1]; /* output path */
	int        fileno;            /* number of files */
	char       fname[MAXFILES][STRLTH+1]; /* filenames */
	int        startrec[MAXFILES];/* start records */
} SdParamT;



/* prototypes of local routines */
void SdReadParameters( char parfile[], SdParamT *par );
void SdProcFile( char fname[], int *rec, int recsize, char outpath[],
	int debug );



int main( int argc, char *argv[] )
{
	/* local variables */
	char     parfile[STRLTH+1];   /* parameter file */
	SdParamT par;                 /* split file parameters */
	int      i;                   /* counter */

	/* executable code */

	if  (argc != 2)  {
		fprintf( stderr, "Usage: %s <parfile>\n", argv[0] );
		return 1;
	} /*endif*/

	strcpy( parfile, argv[1] );

	SdReadParameters( parfile, &par );

	if  (par.debug > 0)  {
		printf( "debug: recsize %d\n", par.recsize );
		printf( "debug: sleeptime %d\n", par.sleeptime );
		printf( "debug: outpath %s\n", par.outpath );
		for  (i=0; i<par.fileno; i++)
			printf( "debug: file %s start %d\n", par.fname[i], par.startrec[i] );
	} /*endif*/

	/* loop forever */
	for  (;;)  {
		for  (i=0; i<par.fileno; i++)  {
			SdProcFile( par.fname[i], &(par.startrec[i]),
				par.recsize, par.outpath, par.debug );
		} /*endfor*/
		if  (par.debug)  printf( "debug: sleep %d s\n", par.sleeptime );
		usleep( par.sleeptime * 1000000 );
	} /*endfor*/

} /* end of main */



/*----------------------------------------------------------------------------*/



void SdReadParameters( char parfile[], SdParamT *par )

/* Reads parameters from file
 *
 * parameters of routine
 * char       parfile[];      input; name of parameter file
 * SdParamT   *sd;            output; parameters read from file
 */
{
	/* local variables */
	FILE     *fp;            /* pointer to input file */
	char     line[STRLTH+1]; /* current line of file */

	/* executable code */

	/* initialize */
	par->debug = 0;
	par->recsize = 512;
	par->sleeptime = 60;
	par->outpath[0] = '\0';
	par->fileno = 0;

	fp = fopen( parfile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "splitd: Input file %s not found.  Abort.\n", parfile );
		exit( 1 );
	} /*endif*/

	while  (fgets(line,STRLTH,fp) != NULL)  {
		if  (strncmp(line,"record size: ",13) == 0)  {
			sscanf( line+13, "%d", &(par->recsize) );
			if  (par->recsize > MAXRECSIZE)  {
				fprintf( stderr, "splitd: record size too big.\n" );
				fclose( fp );
				exit( 1 );
			} /*endif*/
		} else if  (strncmp(line,"sleep time: ",12) == 0)  {
			sscanf( line+12, "%d", &(par->sleeptime) );
		} else if  (strncmp(line,"debug: ",7) == 0)  {
			sscanf( line+7, "%d", &(par->debug) );
		} else if  (strncmp(line,"file: ",6) == 0)  {
			if  (par->fileno == MAXFILES)  {
				fprintf( stderr, "splitd: too many files (> %d)\n", MAXFILES );
				fclose( fp );
				exit( 1 );
			} /*endif*/
			sscanf( line+6, "%s", par->fname[par->fileno] );
			par->startrec[par->fileno] = -1;
			(par->fileno)++;
		} else if  (strncmp(line,"output path: ",13) == 0)  {
			sscanf( line+13, "%s", par->outpath );
		} /*endif*/
	} /*endwhile*/

	fclose( fp );

} /* end of SdReadParameters */



/*----------------------------------------------------------------------------*/


void SdProcFile( char fname[], int *rec, int recsize, char outpath[],
	int debug )

/* Process file
 *
 * parameters of routine
 * char       fname[];     input; name of file
 * int        *rec;        modify; record number
 * int        recsize;     input; record size
 * char       outpath[];   input; name of output path
 * int        debug;       input; debug flag
 */
{
	/* local variables */
	FILE     *fp, *out;            /* pointer to data file */
	char     seedrec[MAXRECSIZE];  /* seed record */
	char     outname[STRLTH+1];    /* name of output file */
	char     station[STRLTH+1];    /* station name */
	char     chan[STRLTH+1];       /* channel name */
	int      recno;                /* record number of seed record */
	int      i;                    /* counter */

	/* executable code */

	if  (debug)  printf( "debug: process file %s\n", fname );

	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		if  (debug)  printf( "debug: file %s not found\n", fname );
		return;
	} /*endif*/

	if  (*rec < 0)  {
		/* determine size of file */
		fseek( fp, 0, SEEK_END );
		*rec = ftell( fp ) / recsize;
		if  (debug)  printf( "debug: found last record %d in file %s\n",
			*rec, fname );
	} /*endif*/

	fseek( fp, (*rec)*recsize, SEEK_SET );
	while  (fread( seedrec, recsize, 1, fp ) == 1)  {
		/* read info from record */
		sscanf( seedrec, "%06dD", &recno );
		for  (i=7; i<11; i++)  if  (seedrec[i] > ' ')  break;
		strncpy( station, seedrec+i, 12-i );
		strncpy( chan, seedrec+15, 3 );
		for  (i=0; i<3; i++)  if  (chan[i] < ' ')  chan[i] = 'X';
		sprintf( outname, "%s%s-%06d.%s", outpath, station, recno, chan );
		if  (debug)  printf( "debug: record %d on %s -> %s\n",
			recno, fname, outname );
		out = fopen( outname, "w" );
		fwrite( seedrec, recsize, 1, out );
		fclose( out );
		(*rec)++;
	} /*while*/

	fclose( fp );

} /* end of SdProcFile */



/*----------------------------------------------------------------------------*/


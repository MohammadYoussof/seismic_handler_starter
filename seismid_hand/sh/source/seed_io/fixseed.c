
/* file fixseed.c
 *      =========
 *
 * version 2, 21-Nov-2005
 *
 * Fixes corrupted MiniSEED files by throwing away incomplete blocks
 * at the bginning or end of file.
 * K. Stammler, 19-Aug-99
 */


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "cpar.h"



/* prototypes of local routines */
TSyBoolean IsSeedRecord( char rec[] );



int main ( int argc, char *argv[] )
{
	/* local variables */
	char     fname[cBcFileLth+1];    /* name of input file */
	FILE     *fp;                    /* pointer to input file */
	FILE     *out;                   /* pointer to output file */
	int      recsize;                /* SEED record size */
	int      skipsize;               /* bytes to skip at beginning */
	char     outname[cBcFileLth+1];  /* name of output file */
	char     *rec;                   /* pointer to record */
	int      rcnt;                   /* record counter */

	/* executable code */

	pa_init( argc, argv );
	if  (pa_pnumber() < 1)  {
		fprintf( stderr,
			"Usage: %s <seedfile> [<recsize>] [<skipsize>] [<outfile>]\n",
			pa_progname() );
		return 1;
	} /*endif*/

	/* get parameters */
	strcpy( fname, pa_pvalue(1) );
	if  (pa_pnumber() >= 2)  {
		if  (sscanf( pa_pvalue(2), "%d", &recsize) != 1)  recsize = 4096;
	} else {
		recsize = 4096;
	} /*endif*/
	if  (pa_pnumber() >= 3)  {
		if  (sscanf( pa_pvalue(3), "%d", &skipsize) != 1)  skipsize = -1;
	} else {
		skipsize = -1;
	} /*endif*/
	if  (pa_pnumber() >= 4)  {
		strcpy( outname, pa_pvalue(4) );
		if  (*outname == '\0')  {
			strcpy( outname, fname );
			strcat( outname, ".fix" );
		} /*endif*/
	} else {
		strcpy( outname, fname );
		strcat( outname, ".fix" );
	} /*endif*/

	/* allocate memory */
	rec = (char *)malloc( recsize );
	if  (rec == NULL)  {
		fprintf( stderr, "%s: error allocating memory\n", pa_progname() );
		return 1;
	} /*endif*/

	/* open input file */
	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: error opening input file %s\n",
			pa_progname(), fname );
		return 1;
	} /*endif*/

	/* find start position */
	if  (skipsize < 0)  skipsize = 0;
	if  (skipsize > 0)  fseek( fp, skipsize, SEEK_SET );
	if  (fread( rec, recsize, 1, fp ) != 1)  {
		fprintf( stderr, "%s: cannot read one record in %f\n",
			pa_progname(), fname );
		fclose( fp );
		free( rec );
		return 1;
	} /*endif*/
	while  (!IsSeedRecord(rec))  {
		if  (skipsize == 0)  {
			skipsize = recsize;
		} else {
			skipsize /= 2;
			if  (skipsize <= 2)  {
				fprintf( stderr, "%s: cannot find start position in %s\n",
					pa_progname(), fname );
			} /*endif*/
		} /*endif*/
		fseek( fp, skipsize, SEEK_SET );
		if  (fread( rec, recsize, 1, fp ) != 1)  {
			fprintf( stderr, "%s: cannot read one record in %f\n",
				pa_progname(), fname );
			fclose( fp );
			free( rec );
			return 1;
		} /*endif*/
	} /*endwhile*/

	out = fopen( outname, "w" );
	if  (out == NULL)  {
		fprintf( stderr, "%s: error opening output file %s\n",
			pa_progname(), outname );
		fclose( fp );
		free( rec );
		return 1;
	} /*endif*/

	fseek( fp, skipsize, SEEK_SET );
	rcnt = 0;
	while  (fread( rec, recsize, 1, fp ) == 1)  {
		fwrite( rec, recsize, 1, out );
		rcnt++;
	} /*endwhile*/

	fclose( out );
	fclose( fp );
	free( rec );

	printf( "%s: wrote %d records to file %s\n", pa_progname(), rcnt, outname );
	if  (skipsize > 0)
		printf( "   %d bytes skipped at beginning of file\n", skipsize );

	return 0;

} /* end of main */



/*----------------------------------------------------------------------------*/



TSyBoolean IsSeedRecord( char rec[] )

/* Returns TRUE if passed data is beginning of a SEED data record
 *
 * parameters of routine
 * char       rec[];        input; start of record
 */
{
	/* local variables */
	int      i;           /* counter */

	/* executable code */

	for  (i=0; i<6; i++)
		if  (rec[i] < '0' || rec[i] > '9')  return FALSE;

	if  (rec[6] != 'D')  return FALSE;
	return TRUE;

} /* end of IsSeedRecord */



/*----------------------------------------------------------------------------*/


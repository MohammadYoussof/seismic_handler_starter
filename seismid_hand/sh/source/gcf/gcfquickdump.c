
/* file gcftest.c
 *      =========
 *
 * version 1, 18-Oct-2003
 *
 * test module for GCF data
 * K. Stammler, 18-Oct-2003
 */


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include "sysbase.h"
#include "tcusrdef.h"
#include "gcflib.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char     gcffile[cBcFileLth+1];  /* input GCF file */
	FILE     *gcf;                   /* pointer to GCF file */
	GcfRawHeaderT rhdr;              /* raw header */
	GcfHeaderT hdr;                  /* decoded header */
	int      i;                      /* counter */
	char     str[cBcLineLth+1];      /* text string */
	char     recend[cBcTimeLth+1];   /* end time of block */
	char     lasttime[cBcTimeLth+1]; /* last block end time */
	float    gaplth;                 /* length of data gap */
	TSyStatus status;                /* return status */

	/* executable code */

	if  (argc != 2)  {
		fprintf( stderr, "Usage: %s <inpfile>\n", argv[0] );
		return 1;
	} /*endif*/

	/* get parameters */
	strcpy ( gcffile, argv[1] );

	status = cBcNoError;

	GcfSetTapcode( 2, "HH", &status );
	if  (SySevere(&status))  err_writemsg( status, "setting tapcode", TRUE );

	gcf = fopen( gcffile, "r" );
	if  (gcf == NULL)  {
		fprintf( stderr, "%s: cannot open input file %s\n", argv[0], gcffile );
		return 1;
	} /*endif*/

	*lasttime = '\0';
	gaplth = 0.0;

	FOREVER  {

		GcfReadRawHeader( gcf, &rhdr, &status );
		if  (status == GcfERR_EOF_FOUND)  {
			status = cBcNoError;
			break;
		} /*endif*/
		if  (SySevere(&status))  err_writemsg( status, "error reading header", TRUE );

		GcfDecodeHeader( &rhdr, &hdr, &status );
		if  (SySevere(&status))
			err_writemsg( status, "error reading header", TRUE );

		/* check for data gap */
		tc_tadd( hdr.blktime, (float)(hdr.numrec*hdr.cmpcode/hdr.smprate),
			recend, &status );
		if  (SySevere(&status))
			err_writemsg( status, "error computing end time", TRUE );
		if  (*lasttime != '\0')
			gaplth = tc_tdiff( hdr.blktime, lasttime, &status ); 

		GcfPrintHeader( stdout, &hdr );
		if  (gaplth == 0.0)  {
			printf( " cont" );
		} else {
			printf( " %4.1f", gaplth );
		} /*endif*/
		printf( "\n" );

		GcfSkipDataBlock( gcf, &rhdr );

		strcpy( lasttime, recend );

	} /*endwhile*/

	fclose( gcf );

	return 0;

} /* end of main */

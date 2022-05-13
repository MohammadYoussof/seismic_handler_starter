
/* file sfd_extract_times.c
 *      ===================
 *
 * version 1, 8-Aug-94
 *
 * extracts start times of specified channel in sfd-file
 * K. Stammler, 8-Aug-94
 */



#include <stdio.h>
#include <string.h>
#include "../basecnst.h"
#include BC_SYSBASE
#include "seedcfg.h"
#include "seed_lib.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char     sfdfile[BC_FILELTH+1];    /* name of sfd-file */
	char     stream[BC_SHORTSTRLTH+1]; /* stream string */
	FILE     *sfd;                     /* pointer to sfd-file */
	char     line[Seed_C_SFDLINELTH+1];/* current sfd-line */
	SeedFileDescrT dsc;                /* sfd descriptor */
	STATUS   status;                   /* return status */

	/* executable code */

	if  (argc != 3)  {
		fprintf( stderr, "Usage: %s <sfd-file> <stream>\n", argv[0] );
		return 1;
	} /*endif*/

	strcpy( sfdfile, argv[1] );
	strcpy( stream, argv[2] );

	status = BC_NOERROR;

	/* open sfd-file */
	sfd = fopen( sfdfile, "r" );
	if  (sfd == NULL)  {
		fprintf( stderr, "%s: couldn't open sfd-file %s\n", argv[0], sfdfile );
		return 1;
	} /*endif*/

	/* loop all lines of sfd-file */
	while  (fgets(line,Seed_C_SFDLINELTH,sfd) != NULL)  {
		if  (*line == '\n' || *line == '!')  continue;
		SeedParseSfdLine( line, &dsc, &status );
		if  (strcmp(stream,dsc.stream) != 0)  continue;
		printf( "%s\n", dsc.t_start );
	} /*endwhile*/

	fclose( sfd );

} /* end of main */

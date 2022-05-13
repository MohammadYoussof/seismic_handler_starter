
/* file reformat_locsat_out.c
 *      =====================
 *
 * version 1, 10-Nov-94
 *
 * reformats output of LocSAT to brief list
 * K. Stammler, 10-Nov-94
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1996,  Klaus Stammler, Federal Institute for Geosciences
 *                                       and Natural Resources (BGR), Germany
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */


#include <stdio.h>
#include <string.h>
#include BASECNST
#include BC_SYSBASE
#include BC_TCUSRDEF



#define cLcFileLth BC_FILELTH
#define cLcLineLth BC_LINELTH



int main( int argc, char *argv[] )
{
	/* local variables */
	char     fname[cLcFileLth+1];    /* name of input file */
	FILE     *fp;                    /* pointer to input file */
	char     line[cLcLineLth+1];     /* current line of file */
	int      i;                      /* counter */
	float    rel_orig;               /* relative origin time */
	char     reftime[BC_TIMELTH+1];  /* reference time */
	char     origtime[BC_TIMELTH+1]; /* origin time */
	STATUS   status;                 /* return status */

	/* executable code */

	if  (argc != 3)  {
		fprintf( stderr, "Usage: %s <locsat-file> <ref-time>\n", argv[0] );
		return 1;
	} /*endif*/

	strcpy( fname, argv[1] );
	strcpy( reftime, argv[2] );

	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: file %s not found\n", argv[0], fname );
		return 1;
	} /*endif*/

	while  (fgets(line,cLcLineLth,fp) != NULL)  {
		if  (strstr(line,"iterations"))  {
			printf( "%s\n", line );
		} else if  (strstr(line,"Latitude:"))  {
			printf( "%s", line );
			for  (i=0; i<2; i++)  {
				fgets( line, cLcLineLth, fp );
				printf( "%s", line );
			} /*endif*/
			fgets( line, cLcLineLth, fp );
			sscanf( line+16, "%f", &rel_orig );
			status = BC_NOERROR;
			tc_tadd( reftime, rel_orig, origtime, &status );
			printf( "   origin time: %s\n", origtime );
		} else if  (strncmp(line,"                       Data",27) == 0)  {
			printf( "\n\n%s", line );
			while  (*line != '\n')  {
				fgets( line, cLcLineLth, fp );
				printf( "%s", line );
			} /*endif*/
		} /*endif*/
	} /*endwhile*/

	return 0;

} /* end of main */

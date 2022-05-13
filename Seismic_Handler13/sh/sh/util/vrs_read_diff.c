
/* file vrs_read_diff.c
 *      ===============
 *
 * version 2, 30-Nov-97
 *
 * reads difference file of the two version list files and writes out
 * file to copy
 * K. Stammler, 11-Aug-94
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


int main( int argc, char *argv[] )
{
	/* local variables */
	char     inpfile[BC_FILELTH+1];    /* name of input file */
	FILE     *fp;                      /* pointer to input file */
	char     line[BC_LONGSTRLTH+1];    /* current line of file */
	char     *src, *dst;               /* moving pointers */
	char     fname[BC_FILELTH+1];      /* filename to be copied */

	/* executable code */

	if  (argc != 2)  {
		fprintf( stderr, "Usage: %s <diff-input_new-old>\n", argv[0] );
		return 1;
	} /*endif*/
	strcpy( inpfile, argv[1] );

	/* open file */
	fp = fopen( inpfile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: can't open input file %s\n", argv[0], inpfile );
		return 1;
	} /*endif*/

	while  (fgets(line,BC_LONGSTRLTH,fp) != NULL)  {
		if  (*line != '<')  continue;
		if  (strlen(line) < 3)  continue;
		src = line+2;
		dst = fname;
		while  (*src != ',' && *src != '\0')  {
			if  (dst >= fname+BC_FILELTH)  {
				fprintf( stderr, "%s: filename too long (line %s)\n",
					argv[0], line );
				break;
			} /*endif*/
			*dst++ = *src++;
		} /*endwhile*/
		*dst = '\0';
		if  (*src == ',')  {
			printf( "%s\n", fname );
		} else {
			fprintf( stderr, "%s: no separation comma for file %s\n",
				argv[0], fname );
		} /*endif*/
	} /*endwhile*/

	fclose( fp );
	return 0;

} /* end of main */

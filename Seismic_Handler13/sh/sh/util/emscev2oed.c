
/* file emscev2oed.c
 *      ============
 *
 * version 1, 1-Oct-2005
 *
 * Convert output of EMSC page (get_latest_emsc_evemts.csh) to
 * SH/SHM oed format.
 * K. Stammler, 1-Oct-2005
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

#define STRLTH 132


int main( int argc, char *argv[] )
{
	/* local variables */
	char     fname[STRLTH+1];     /* name of input file */
	char     line[STRLTH+1];      /* current line of file */
	FILE     *fp;                 /* pointer to input file */

	/* executable code */

	if  (argc != 2)  {
		fprintf( stderr, "Usage: emscev2oed <inputfile>\n" );
		return 1;
	} /*endif*/

	/* get parameters */
	if  (strlen(argv[1]) > STRLTH)  {
		fprintf( stderr, "%s: filename too long.  Abort.\n", argv[0] );
		return 1;
	} /*endif*/
	strcpy( fname, argv[1] );

	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: cannot open input file %s.  Abort.\n",
			argv[0], fname );
		return 1;
	} /*endif*/

	while  (fgets(line,STRLTH,fp) != NULL)  {
		if  (strlen(line) < 54)  continue;
		if  (line[4] != '/')  continue;
		if  (line[7] != '/')  continue;
		if  (line[10] == ' ')  line[10] = '_';
		if  (line[23] == ' ')  line[23] = '0';
		if  (line[29] == 'S')  line[22] = '-';
		line[29] = ' ';
		if  (line[31] == ' ')  line[31] = '0';
		if  (line[32] == ' ')  line[32] = '0';
		if  (line[38] == 'W')  line[30] = '-';
		line[38] = ' ';
		line[43] = ' ';
		line[44] = line[45];
		line[45] = line[46];
		line[46] = ' ';
		if  (line[42] == ' ')  line[41] = line[42] = '3';
		fprintf( stdout, "%s", line );
	} /*endwhile*/

	fclose( fp );

	return 0;

} /* end of main */

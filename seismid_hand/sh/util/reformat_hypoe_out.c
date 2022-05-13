
/* file reformat_hypoe_out.c
 *      ====================
 *
 * version 1, 21-Jun-94
 *
 * reformats output file of hypoellipse to a more readable format
 * K. Stammler, 21-Jun-94
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


#define LINELTH 256
#define FILELTH 128


int main( int argc, char *argv[] )
{
	/* local variables */
	char     inpfile[FILELTH+1];    /* name of input file */
	char     outfile[FILELTH+1];    /* name of output file */
	FILE     *inp, *out;            /* pointers to files */
	char     line[LINELTH+1];       /* current line */
	int      tmp;                   /* scratch */
	float    lat, lon;              /* location */
	float    depth;                 /* depth of event */

	/* executable code */

	if  (argc != 3)  {
		fprintf( stderr, "*** Usage: %s <input> <output> ***\n", argv[0] );
		return 1;
	} /*endif*/
	strcpy( inpfile, argv[1] );
	strcpy( outfile, argv[2] );

	inp = fopen( inpfile, "r" );
	if  (inp == NULL)  {
		fprintf( stderr, "*** %s: couldn't open input file %s ***\n",
			argv[0], inpfile );
		return 1;
	} /*endif*/
	out = fopen( outfile, "w" );
	if  (out == NULL)  {
		fprintf( stderr, "*** %s: couldn't open output file %s ***\n",
			argv[0], outfile );
		fclose( inp );
		return 1;
	} /*endif*/

	/* read off beginning of file */
	while  (fgets(line,LINELTH,inp) != NULL)  {
		if  (strncmp(line,"  date    origin",16) == 0)  break;
	} /*endif*/
	if  (fgets(line,LINELTH,inp) == NULL)  {fclose(inp);fclose(out);return 1;}

	/* get origin time */
	fprintf( out, "origin time: %c%c-%c%c-%c%c ",
		line[1], line[2], line[3], line[4], line[5], line[6] );
	fprintf( out, "%c%c:%c%c:%c%c%c.%c%c\n",
		line[8], line[9], line[10], line[11],
		(line[12] == ' ') ? '0' : line[12], (line[13] == ' ') ? '0' : line[13],
		line[14], line[16], line[17] );

	/* get location */
	sscanf( line+18, "%d", &tmp );
	sscanf( line+22, "%f", &lat );
	lat = (float)tmp + lat/60.0;
	if  (line[21] == 's')  lat = -lat;
	sscanf( line+28, "%d", &tmp );
	sscanf( line+32, "%f", &lon );
	lon = (float)tmp + lon/60.0;
	if  (line[31] == 'w')  lon = -lon;
	fprintf( out, "location: %7.2f %7.2f\n", lat, lon );

	/* get depth */
	sscanf( line+38, "%f", &depth );
	fprintf( out, "depth: %6.1f\n", depth );

	while  (fgets(line,LINELTH,inp) != NULL)  {
		if  (strncmp(line,"  stn c pha remk",16) == 0)  break;
	} /*endif*/
	line[56] = '\0';
	fprintf( out, "%s\n", line );
	while  (fgets(line,LINELTH,inp) != NULL)  {
		if  (strlen(line) < 56)  break;
		line[56] = '\0';
		fprintf( out, "%s\n", line );
	} /*endif*/

	fclose( inp );
	fclose( out );

	return 0;

} /* end of main */

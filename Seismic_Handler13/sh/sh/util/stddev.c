
/* file stddev.c
 *      ========
 *
 * version 3, 21-Nov-2005
 *
 * Retuns mean value and standard deviation of a series of numbers in a file
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
#include <math.h>


#define NAMELTH 132
#define MAXNUM 5000


int main( int argc, char *argv[] )
{
	/* local variables */
	static float buf[MAXNUM];      /* number buffer */
	char     fname[NAMELTH+1];     /* name of input file */
	FILE     *fp;                  /* pointer to input file */
	char     line[NAMELTH+1];      /* current line */
	float    num;                  /* current number */
	int      lth;                  /* number of numbers in file */
	float    mean;                 /* mean value */
	float    dev;                  /* standard deviation */
	float    sum;                  /* sum */
	int      i;                    /* counter */

	/* executable code */

	if  (argc != 2)  {
		fprintf( stderr, "Usage: %s <file>\n", argv[0] );
		return 1;
	} /*endif*/

	strcpy( fname, argv[1] );
	if  (strcmp(fname,"-") == 0)  {
		fp = stdin;
	} else {
		fp = fopen( fname, "r" );
		if  (fp == NULL)  {
			fprintf( stderr, "%s: error opening file %s\n", argv[0], fname );
			return 1;
		} /*endif*/
	} /*endif*/

	lth = 0;
	sum = 0.0;
	while  (fgets(line,NAMELTH,fp) != NULL)  {
		sscanf( line, "%f", &num );
		sum += num;
		if  (lth == MAXNUM)  {printf( "MAXNUM 500\n" ); return 1;}
		buf[lth++] = num;
	} /*endif*/
	mean = sum / (float)lth;

	sum = 0.0;
	i = 0;
	for  (i=0; i<lth; i++)  {
		num = buf[i] - mean;
		sum += num*num;
	} /*endfor*/
	dev = sqrt(sum/(float)(lth-1));

	printf( "%d numbers found   mean: %f   stddev %f\n", lth, mean, dev );

	if  (fp != stdin)
		fclose( fp );

	return 0;

} /* end of main */


/* file find_next_loc.c
 *      ===============
 *
 * version 1, 24-Jun-2003
 *
 * Finds next location to a given location out of a list
 * K. Stammler, 24-Jun-2003
 *
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
#include "basecnst.h"
#include "sysbase.h"
#include "utusrdef.h"
#include "glusrdef.h"
#include "earthloc.h"
#include "ptusrdef.h"



/* global variable */
char shd_errors[BC_FILELTH+1];



int main( int argc, char *argv[] )
{
	/* local variables */
	double  lat1, lon1;             /* first location */
	double  lat2, lon2;             /* second location */
	double  distance, azim, bazim;  /* output values */
	float   fdist, fbazim;          /* float output */
	float   flat1, flon1, flat2, flon2;  /* float scratch values */
	char    fname[cBcFileLth+1];    /* name of location list */
	FILE    *fp;                    /* pointer to input file */
	char    line[cBcLineLth+1];     /* current line of file */
	STATUS  status;                 /* return status */
	BOOLEAN sphere;                 /* sphere computation */
	float   mindist, minlat, minlon;/* values of closest location */

	/* executable code */

	/* read parameters from command line */
	if  (argc != 4)  {
		printf( "Usage: %s <lat> <lon> <loclist>\n", argv[0] );
		return 0;
	} /*endif*/

	if  (sscanf(argv[1],"%f",&flat1) != 1)  {
		fprintf( stderr, "%s: error readinf <lat>\n", argv[0] );
		return 1;
	} /*endif*/
	if  (sscanf(argv[2],"%f",&flon1) != 1)  {
		fprintf( stderr, "%s: error readinf <lon>\n", argv[0] );
		return 1;
	} /*endif*/
	strcpy( fname, argv[3] );
	lat1 = flat1;
	lon1 = flon1;

	/* open input file and loop lines */
	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: cannot open input file %s\n", argv[0], fname );
		return 1;
	} /*endif*/

	sphere = TRUE;
	minlat = minlon = 0.0;
	mindist = 1.0e6;

	while (!feof(fp))  {

		if  (fgets(line,cBcLineLth,fp) == NULL)  break;
		if  (*line == '!')  continue;
		if  (*line == '\n')  continue;
		if  (sscanf(line,"%f %f",&flat2,&flon2) != 2)  {
			fprintf( stderr, "%s: read error on file %s, line\n%s\n",
				argv[0], fname, line );
			fclose( fp );
			return 1;
		} /*endif*/

		if  (sphere)  {
			mb_spherediff( flat2, flon2, flat1, flon1, &fdist, &fbazim );
		} else {
			lat2 = flat2;
			lon2 = flon2;
			mb_locdiff( lat2, lon2, lat1, lon1, &distance, &azim, &bazim );
			fdist = distance;
		} /*endif*/

		if  (fdist < mindist)  {
			mindist = fdist;
			minlon = flon2;
			minlat = flat2;
		} /*endif*/

	} /*endwhile*/

	fclose( fp );

	printf( "%7.3f deg  %5.1f km  loc: %7.3f %7.3f\n", mindist, mindist*111.19,
		minlat, minlon );

	return 0;

} /* end of main */


/* file find_topo30_id.c
 *      ================
 *
 * version 1, 6-Feb-2002
 *
 * Returns file id from grdraster.info containing specified location.
 * Returns -1 on error.
 * K. Stammler, 6-Feb-2002
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

#define STRLTH 256
#define DEFINFO "/programs/sol2/gmt-3.3.4/share/dbase/grdraster.info"

int main( int argc, char *argv[] )
{
	/* local variables */
	float    lat, lon;         /* location */
	char     infile[STRLTH+1]; /* info file */
	FILE     *fp;              /* pointer to info file */
	char     str[STRLTH+1];    /* current line of file */
	int      i;                /* counter */
	char     *p;               /* pointer to area info */
	float    lata, latb, lona, lonb; /* area bounds */
	int      id;               /* file id */

	/* executable code */

	if  (argc <  3)  {
		fprintf( stderr, "Usage: %s <lat> <lon> [<grdinfofile>]\n", argv[0] );
		return -1;
	} /*endif*/

	if  (sscanf(argv[1],"%f",&lat) != 1)  {
		fprintf( stderr, "error reading latitude\n" );
		return -1;
	} /*endif*/
	if  (sscanf(argv[2],"%f",&lon) != 1)  {
		fprintf( stderr, "error reading latitude\n" );
		return -1;
	} /*endif*/
	if  (argc >= 4)  {
		strcpy( infile, argv[3] );
	} else {
		strcpy( infile, DEFINFO );
	} /*endif*/

	fp = fopen( infile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: cannot open input file %s\n", argv[0], infile );
		return -1;
	} /*endif*/

	/* read through file */
	while  (fgets(str,STRLTH,fp) != NULL)  {
		if  (*str == '#' || *str == '\n')  continue;
		i = strlen( str );
		if  (i < 20)  continue;
		if  (strncmp(str+3,"30sec",5) != 0  &&  strncmp(str+4,"30sec",5) != 0)
			continue;
		p = strstr( str, "-R" );
		if  (p == NULL)  continue;
		if  (sscanf(p,"-R%f/%f/%f/%f", &lona, &lonb, &lata, &latb) != 4)
			continue;
		if  (lata > lat)  continue;
		if  (latb < lat)  continue;
		if  (lona > lon)  continue;
		if  (lonb < lon)  continue;
		if  (sscanf(str,"%d",&id) != 1)  continue;
		printf( "%d\n", id );
	} /*endwhile*/

	fclose( fp );

	return 0;

} /* end of main */

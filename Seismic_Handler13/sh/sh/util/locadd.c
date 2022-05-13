
/* file locadd.c
 *      ========
 *
 * version 3, 1-Dec-94
 *
 * computes new location from given location and distance,azimuth
 * K. Stammler, 5-Aug-92
 *
 * Usage:  locadd <station1> <distance> <azimuth>
 *    or:  locadd <lat> <lon> <distance> <azimuth>
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
#ifdef BC_STDLIB_EX
#include <stdlib.h>
#endif
#include BC_SYSBASE
#include BC_CPAR
#include BC_UTUSRDEF
#include BC_GLUSRDEF
#include BC_EARTHLOC
#include BC_INPFILES


/* global variable */
char shd_errors[BC_FILELTH+1];


int main( int argc, char *argv[] )
{
	/* local variables */
	double  lat1, lon1;             /* first location */
	double  lat2, lon2;             /* second location */
	float   distance, azim;         /* input values */
	float   flat1, flon1, flat2, flon2;
	STATUS  status;                 /* return status */
	char    stat[BC_LINELTH+1];     /* name of station */
	char    str[BC_LINELTH+1];      /* scratch */
	int     feridx;                 /* FER index */
	BOOLEAN sphere;                 /* sphere computation */
	char    path[BC_FILELTH+1];     /* path to input files */
	GLT_STATINF inf;                /* station info */
#	ifdef BC_SUN
	char    *eptr;                  /* environment pointer */
#	endif

	/* executable code */

#	ifdef BC_SUN
	eptr = getenv( "SH_ERRORS" );
	if  (eptr != NULL)  strcpy( shd_errors, eptr );
	eptr = getenv( "SH_INPUTS" );
	if  (eptr != NULL)  {
		strcpy( path, eptr );
	} else {
		*path = '\0';
	} /*endif*/
#	else
	strcpy( path, IF_PATH );
#	endif

	status = BC_NOERROR;
	strcpy( str, path );
	strcat( str, IF_STATINFFILE );
	gl_locfile_name( str );
	strcpy( str, path );
	strcat( str, IF_FERINDEXFILE );
	mb_setindexfile( str, &status );
	strcpy( str, path );
	strcat( str, IF_FERNAMEFILE );
	mb_setfernamefile( str, &status );

	pa_init( argc, argv );
	if  (pa_pnumber() < 3 ||  pa_pnumber() > 4)  {
		printf( "   \n" );
		printf( "   locadd determines new location from an input location\n" );
		printf( "   and a given distance (deg) and direction (azimuth in deg)\n" );
		printf( "   Usage:  locadd <lat1> <lon1> <distance> <azimuth>\n" );
		printf( "      or:  locadd <station> <distance> <azimuth>\n" );
		return 0;
	} else if  (pa_pnumber() == 3)  {
		strcpy( stat, pa_pvalue(1) );
		ut_cap( stat );
		/* gl_statloc( stat, &lat1, &lon1, &status ); */
		gl_statinf( stat, &inf, &status );
		if  (status != BC_NOERROR)  {
			printf( "*** couldn't get station location of %s\n", stat );
			return 0;
		} /*endif*/
		lat1 = inf.lat;
		lon1 = inf.lon;
		strcpy( str, pa_pvalue(2) );
		if  (sscanf( str, "%f", &distance ) != 1)  {
			printf( "*** couldn't read distance %s\n", str );
			return 0;
		} /*endif*/
		strcpy( str, pa_pvalue(3) );
		if  (Cap(*str) == 'W')  *str = '-';
		if  (Cap(*str) == 'E')  *str = '+';
		if  (sscanf( str, "%f", &azim ) != 1)  {
			printf( "*** couldn't read azimuth %s\n", str );
			return 0;
		} /*endif*/
	} else {
		strcpy( str, pa_pvalue(1) );
		if  (Cap(*str) == 'S')  *str = '-';
		if  (Cap(*str) == 'N')  *str = '+';
		if  (sscanf( str, "%lf", &lat1 ) != 1)  {
			printf( "*** couldn't read latitude %s\n", str );
			return 0;
		} /*endif*/
		strcpy( str, pa_pvalue(2) );
		if  (Cap(*str) == 'W')  *str = '-';
		if  (Cap(*str) == 'E')  *str = '+';
		if  (sscanf( str, "%lf", &lon1 ) != 1)  {
			printf( "*** couldn't read longitude %s\n", str );
			return 0;
		} /*endif*/
		strcpy( str, pa_pvalue(3) );
		if  (sscanf( str, "%f", &distance ) != 1)  {
			printf( "*** couldn't read distance %s\n", str );
			return 0;
		} /*endif*/
		strcpy( str, pa_pvalue(4) );
		if  (Cap(*str) == 'W')  *str = '-';
		if  (Cap(*str) == 'E')  *str = '+';
		if  (sscanf( str, "%f", &azim ) != 1)  {
			printf( "*** couldn't read azimuth %s\n", str );
			return 0;
		} /*endif*/
	} /*endif*/

	/* sphere = pa_qspecified( "-s" ); */
	/* no ellipticity correction available */

	flat1 = lat1; flon1 = lon1;
	mb_sphereadd( flat1, flon1, distance, azim, &flat2, &flon2 );

	printf( "\n" );
	printf( "   --- no correction for ellipticity of earth ! ---\n" );
	printf( "   new location is\n" );
	printf( "   latitude:  %f deg\n", flat2 );
	printf( "   longitude: %f deg\n", flon2 );
	mb_ferindex( flat2, flon2, &feridx, &status );
	if  (status != BC_NOERROR)  {
		printf( "*** couldn't determine FER index ***\n" );
		return 0;
	} /*endif*/
	mb_fername( feridx, BC_LINELTH, str, &status );
	if  (status != BC_NOERROR)  {
		printf( "*** couldn't determine FER name from index ***\n" );
		return 0;
	} /*endif*/
	printf( "   FER:       %s\n\n", str );

	/*return 0;*/

} /* end of main */

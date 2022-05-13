
/* file locdiff.c
 *      =========
 *
 * version 4, 12-Dec-94
 *
 * computes distance and azimuth between two locations
 * K. Stammler, 5-Aug-92
 *
 * Usage:  locdiff <station1> <station2>
 *    or:  locdiff <lat> <lon> <station>
 *    or:  locdiff <lat1> <lon1> <lat2> <lon2>
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
#include BC_PTUSRDEF
#include BC_INPFILES



/* global variable */
char shd_errors[BC_FILELTH+1];



int main( int argc, char *argv[] )
{
	/* local variables */
	double  lat1, lon1;             /* first location */
	double  lat2, lon2;             /* second location */
	double  distance, azim, bazim;  /* output values */
	float   p_slowness;             /* P slowness */
	float   depth;                  /* depth of event */
	char    *depth_val;             /* string of depth */
	float   fdist, fbazim;          /* float output */
	float   flat1, flon1, flat2, flon2;
	STATUS  status;                 /* return status */
	char    stat1[BC_LINELTH+1];    /* name of first station */
	char    stat2[BC_LINELTH+1];    /* name of second station */
	char    str[BC_LINELTH+1];      /* scratch */
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

	strcpy( str, path );
	strcat( str, IF_STATINFFILE );
	gl_locfile_name( str );
	pt_settabledir( path, &status );
	status = BC_NOERROR;
	pt_settabledir( path, &status );
	if  (Severe(&status))  {
		printf( "*** couldn't set table dir %s ***\n", path );
		return 1;
	} /*endif*/

	pa_init( argc, argv );
	if  (pa_pnumber() < 2 ||  pa_pnumber() > 4)  {
		printf( "   \n" );
		printf( "   locdiff determines distance and azimuth between\n" );
		printf( "   two locations.  The location must be given either\n" );
		printf( "   explicitely or by station names.\n" );
		printf( "   Usage:  locdiff <lat1> <lon1> <lat2> <lon2>\n" );
		printf( "      or:  locdiff <lat> <lon> <station>\n" );
		printf( "      or:  locdiff <station1> <station2>\n\n" );
		return 0;
	} else if  (pa_pnumber() == 2)  {
		strcpy( stat1, pa_pvalue(1) );
		ut_cap( stat1 );
		/* gl_statloc( stat1, &lat1, &lon1, &status ); */
		gl_statinf( stat1, &inf, &status );
		if  (status != BC_NOERROR)  {
			printf( "*** couldn't get station location of %s\n", stat1 );
			return 0;
		} /*endif*/
		lat1 = inf.lat;
		lon1 = inf.lon;
		strcpy( stat2, pa_pvalue(2) );
		ut_cap( stat2 );
		/* gl_statloc( stat2, &lat2, &lon2, &status ); */
		gl_statinf( stat2, &inf, &status );
		if  (status != BC_NOERROR)  {
			printf( "*** couldn't get station location of %s\n", stat2 );
			return 0;
		} /*endif*/
		lat2 = inf.lat;
		lon2 = inf.lon;
	} else if  (pa_pnumber() == 3)  {
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
		strcpy( stat1, pa_pvalue(3) );
		ut_cap( stat1 );
		/* gl_statloc( stat1, &lat2, &lon2, &status ); */
		gl_statinf( stat1, &inf, &status );
		if  (status != BC_NOERROR)  {
			printf( "*** couldn't get station location of %s\n", stat1 );
			return 0;
		} /*endif*/
		lat2 = inf.lat;
		lon2 = inf.lon;
	} else {
		strcpy( str, pa_pvalue(1) );
		if  (Cap(*str) == 'S')  *str = '-';
		if  (Cap(*str) == 'N')  *str = '+';
		if  (sscanf( str, "%lf", &lat1 ) != 1)  {
			printf( "*** couldn't read latitude 1 %s\n", str );
			return 0;
		} /*endif*/
		strcpy( str, pa_pvalue(2) );
		if  (Cap(*str) == 'W')  *str = '-';
		if  (Cap(*str) == 'E')  *str = '+';
		if  (sscanf( str, "%lf", &lon1 ) != 1)  {
			printf( "*** couldn't read longitude 1 %s\n", str );
			return 0;
		} /*endif*/
		strcpy( str, pa_pvalue(3) );
		if  (Cap(*str) == 'S')  *str = '-';
		if  (Cap(*str) == 'N')  *str = '+';
		if  (sscanf( str, "%lf", &lat2 ) != 1)  {
			printf( "*** couldn't read latitude 2 %s\n", str );
			return 0;
		} /*endif*/
		strcpy( str, pa_pvalue(4) );
		if  (Cap(*str) == 'W')  *str = '-';
		if  (Cap(*str) == 'E')  *str = '+';
		if  (sscanf( str, "%lf", &lon2 ) != 1)  {
			printf( "*** couldn't read longitude 2 %s\n", str );
			return 0;
		} /*endif*/
	} /*endif*/

	sphere = pa_qspecified( "-s" );
	depth = 0.0;
	if  (NULL != (depth_val = pa_qvalue("-d")))  {
		sscanf( depth_val, "%f", &depth );
	} /*endif*/

	if  (sphere)  {
		flat1 = lat1; flon1 = lon1;
		flat2 = lat2; flon2 = lon2;
		mb_spherediff( flat2, flon2, flat1, flon1, &fdist, &fbazim );
		printf( "\n   distance:      %f deg\n", fdist );
		printf( "   backazimuth:   %f deg\n", fbazim );
		distance = fdist;
	} else {
		mb_locdiff( lat2, lon2, lat1, lon1, &distance, &azim, &bazim );
		printf( "\n   distance:      %lf deg\n", distance );
		printf( "   azimuth:       %lf deg\n", azim );
		printf( "   backazimuth:   %lf deg\n", bazim );
	} /*endif*/

	p_slowness = pt_slowness( "P", distance, depth, &status );
	if  (Severe(&status))  {
		printf( "   P-slowness:    not computed\n\n" );
	} else {
		printf( "   P-slowness:    %4.1f (depth %4.1fkm)\n\n",
			p_slowness, depth );
	} /*endif*/

	/*return 0;*/

} /* end of main */

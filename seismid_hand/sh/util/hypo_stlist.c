
/* file hypo_stlist.c
 *      =============
 *
 * version 1, 6-Sep-93
 *
 * writes station information in HYPOELLIPSE format.  Input is STATINF.DAT
 * file of SH.  Output is appended to specified output file or printed
 * to stdout if omitted.
 * Usage: hypo_stlist <station-name> [<output-file>]
 * K. Stammler, 6-Sep-93
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
#include BASECNST
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_GLUSRDEF
#include BC_CPAR
#include BC_UTUSRDEF
#include BC_INPFILES


/* global variables */
char      shd_errors[BC_FILELTH+1];    /* error message path */


int main( int argc, char *argv[] )
{
	/* local variables */
	char     station[BC_SHORTSTRLTH+1];    /* station name */
	char     outfile[BC_FILELTH+1];        /* output filename */
	char     path[BC_FILELTH+1];           /* input file path */
	char     str[BC_LINELTH+1];            /* scratch string */
	FILE     *out;                         /* pointer to output file */
	GLT_STATINF inf;                       /* station info */
	STATUS   status;                       /* return status */
	int      deglat, deglon;               /* latitude and longitude degrees */
	float    minlat, minlon;               /* minutes fraction of lat and lon */
	char     signlat, signlon;             /* sign of latitude E or W, N or S */
#	ifdef BC_SUN
	char     *eptr;                        /* pointer to environment */
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
	status = BC_NOERROR;

	pa_init( argc, argv );
	if  (pa_pnumber() < 1 || pa_pnumber() > 2)  {
		printf( "*** Usage: hypo_stlist <station-name> [<output-file>] ***\n" );
		return 0;
	} /*endif*/

	strcpy( station, pa_pvalue(1) );
	if  (pa_pnumber() > 1)  {
		strcpy( outfile, pa_pvalue(2) );
		out = sy_fopen( outfile, "a" );
		if  (out == NULL)  {
			printf( "*** output file %s couldn't be opened ***\n", outfile );
			return 1;
		} /*endif*/
	} else {
		out = stdout;
	} /*endif*/

	status = BC_NOERROR;
	ut_cap( station );
	gl_statinf( station, &inf, &status );
	if  (Severe(&status))  {
		printf( "*** station %s not found ***\n", station );
		return status;
	} /*endif*/

	deglat = Nint( floor(inf.lat) );
	deglon = Nint( floor(inf.lon) );
	signlat = (deglat > 0) ? 'N' : 'S';
	signlon = (deglon > 0) ? 'E' : 'W';
	deglat = Abs( deglat );
	deglon = Abs( deglon );
	minlat = (inf.lat - (STATLOC)deglat) * 60.0;
	minlon = (inf.lon - (STATLOC)deglon) * 60.0;

	fprintf( out, "%4s%2d%c%5.2f %3d%c%5.2f %4d\n%4s*\n", station, deglat,
		signlat, minlat, deglon, signlon, minlon, Nint(inf.elevation), station );

	if  (out != stdout)
		sy_fclose( out );

	return 0;

} /* end of main */

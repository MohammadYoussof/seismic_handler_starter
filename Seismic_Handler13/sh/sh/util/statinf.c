
/* file statinf.c
 *      =========
 *
 * version 4, 21-Nov-2005
 *
 * Output station info in various formats
 * K. Stammler, 13-Feb-2003
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
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "cpar.h"
#include "glusrdef.h"


int main( int argc, char *argv[] )
{
	/* local variables */
	char     station[cBcLineLth+1];       /* station name */
	char     *inputs;                     /* pointer to environment */
	char     str[cBcLineLth+1];           /* scratch string */
	int      i;                           /* counter */
	int      slth;                        /* string length */
	GLT_STATINF   statinf;                /* station info */
	STATLOC  tmp;                         /* scratch */
	TSyStatus     status;                 /* return status */
	int      lat_deg, lat_min, lon_deg, lon_min;  /* location values */
	float    lat_sec, lon_sec;                    /* location values */
	char     lat_sign, lon_sign;          /* signs */
	float    elev;                        /* elevation */

	/* executable code */

	pa_init( argc, argv );
	if  (pa_pnumber() != 1)  {
		fprintf( stderr,
			"Usage: %s <station>\n", pa_progname() );
		return 1;
	} /*endif*/

	strcpy( station, pa_pvalue(1) );
	slth = strlen( station );
	for  (i=0; i<slth; i++)  station[i] = Cap( station[i] );

	inputs = (char *)getenv( "SH_INPUTS" );
	if  (inputs == NULL)  {
		fprintf( stderr, "%s: environment SH_INPUTS not set\n", pa_progname() );
		return 1;
	} /*endif*/
	/* station location file */
	strcpy( str, inputs );
	strcat( str, "/" );
	strcat( str, "STATINF.DAT" );
	gl_locfile_name( str );

	status = cBcNoError;
	gl_statinf( station, &statinf, &status );
	if  (SySevere(&status))  {
		fprintf( stderr, "%s: problem %d on station %s\n", pa_progname(), status,
			station );
		return 1;
	} /*endif*/

	if  (gl_valid_number(statinf.elevation))  {
		elev = statinf.elevation;
	} else {
		elev = 0.0;
	} /*endif*/

	if  (pa_qspecified("-hyposat"))  {

		tmp = statinf.lat;
		lat_sign = (tmp < 0 ? 'S' : 'N');
		tmp = fabs( tmp );
		lat_deg = (int)(tmp);
		tmp -= (STATLOC)lat_deg;
		tmp *= 60.0;
		lat_min = (int)(tmp);
		tmp -= (STATLOC)lat_min;
		lat_sec = (float)(tmp*60);

		tmp = statinf.lon;
		lon_sign = (tmp < 0 ? 'W' : 'E');
		tmp = fabs( tmp );
		lon_deg = (int)(tmp);
		tmp -= (STATLOC)lon_deg;
		tmp *= 60.0;
		lon_min = (int)(tmp);
		tmp -= (STATLOC)lon_min;
		lon_sec = (float)(tmp*60);

		printf( "%-5s %02d%02d%04.1f%c%3d%02d%04.1f%c %6.1f%s\n", station,
			lat_deg, lat_min, lat_sec, lat_sign,
			lon_deg, lon_min, lon_sec, lon_sign, elev, statinf.name );

	} else if  (pa_qspecified("-hypo71"))  {

		tmp = statinf.lat;
		lat_sign = (tmp < 0 ? 'S' : 'N');
		tmp = fabs( tmp );
		lat_deg = (int)(tmp);
		tmp -= (STATLOC)lat_deg;
		lat_sec = tmp * 60.0;

		tmp = statinf.lon;
		lon_sign = (tmp < 0 ? 'W' : 'E');
		tmp = fabs( tmp );
		lon_deg = (int)(tmp);
		tmp -= (STATLOC)lon_deg;
		lon_sec = tmp * 60.0;

		printf( "%-6s%02d%5.2f%c%03d%5.2f%c%4.0f  0.00\n", station,
			lat_deg, lat_sec, lat_sign,
			lon_deg, lon_sec, lon_sign, elev );

	} else if  (pa_qspecified("-hypocenter"))  {

		tmp = statinf.lat;
		lat_sign = (tmp < 0 ? 'S' : 'N');
		tmp = fabs( tmp );
		lat_deg = (int)(tmp);
		tmp -= (STATLOC)lat_deg;
		lat_sec = tmp * 60.0;

		tmp = statinf.lon;
		lon_sign = (tmp < 0 ? 'W' : 'E');
		tmp = fabs( tmp );
		lon_deg = (int)(tmp);
		tmp -= (STATLOC)lon_deg;
		lon_sec = tmp * 60.0;

		printf( "  %-4s%02d%5.2f%c%03d%5.2f%c%4.0f  0.00   0.\n", station,
			lat_deg, lat_sec, lat_sign,
			lon_deg, lon_sec, lon_sign, elev );

	} else if  (pa_qspecified("-gemini"))  {

		printf( "      03 %4s      GR         %8.4f     %8.4f      %5.1f   %s\n",
			station, statinf.lat, statinf.lon, statinf.elevation, statinf.name );

	} else {

		printf( "%s %10.7lf %10.7lf", station, statinf.lat, statinf.lon );
		if  (gl_valid_number(statinf.elevation))
			printf( "  elev: %6.1f", statinf.elevation );
		printf( "\n" );

	} /*endif*/

} /* end of main */


/* file fereg.c
 *      =======
 *
 * version 4, 13-Feb-97
 *
 * determines Flinn-Engdahl region from location
 * K. Stammler, 5-Aug-92
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_CPAR
#include BC_EARTHLOC
#include BC_INPFILES

#define INDEXFILE IF_FERINDEXFILE
#define NAMEFILE IF_FERNAMEFILE


int main( int argc, char *argv[] )
{
	/* local variables */
	float    lat, lon;                /* location */
	int      feridx;                  /* FER index */
	STATUS   status;                  /* return status */
	char     fername[BC_LINELTH+1];   /* FER name */
	char     str[BC_LINELTH+1];       /* scratch */
	char     *eptr;                   /* return value of getenv */

	/* executable code */

	status = BC_NOERROR;
	pa_init( argc, argv );
	if  (pa_pnumber() != 2)  {
		printf( "   \n" );
		printf( "   Determines Flinn-Endahl region of a given location\n" );
		printf( "   Usage:  fereg <lat> <lon>\n\n" );
		printf( "                 -n    omits region number\n" );
		return 0;
	} /*endif*/

#	ifdef BC_SUN
	/* get input file path */
	eptr = (char *)getenv( "SH_INPUTS" );
	if  (eptr == NULL)  {
		strcpy( fername, IF_PATH );
	} else {
		strcpy( fername, eptr );
	} /*endif*/
	strcpy( str, fername );
	strcat( str, INDEXFILE );
	mb_setindexfile( str, &status );
	strcpy( str, fername );
	strcat( str, NAMEFILE );
	mb_setfernamefile( str, &status );
#	else
	strcpy( str, IF_PATH );
	strcat( str, INDEXFILE );
	mb_setindexfile( str, &status );
	strcpy( str, IF_PATH );
	strcat( str, NAMEFILE );
	mb_setfernamefile( str, &status );
#	endif

	strcpy( str, pa_pvalue(1) );
	if  (Cap(*str) == 'S')  *str = '-';
	if  (Cap(*str) == 'N')  *str = '+';
	if  (sscanf( str, "%f", &lat ) != 1)  {
		printf( "*** couldn't read latitude %s ***\n", str );
		return 0;
	} /*endif*/
	strcpy( str, pa_pvalue(2) );
	if  (Cap(*str) == 'W')  *str = '-';
	if  (Cap(*str) == 'E')  *str = '+';
	if  (sscanf( str, "%f", &lon ) != 1)  {
		printf( "*** couldn't read longitude %s ***\n", str );
		return 0;
	} /*endif*/

	mb_ferindex( lat, lon, &feridx, &status );
	if  (status != BC_NOERROR)  {
		printf( "*** couldn't determine FER index ***\n" );
		return 0;
	} /*endif*/
	mb_fername( feridx, BC_LINELTH, fername, &status );
	if  (status != BC_NOERROR)  {
		printf( "*** couldn't determine FER name from index ***\n" );
		return 0;
	} /*endif*/

	if  (pa_qspecified("-n"))  {
		printf( "%s\n", fername );
	} else {
		printf( "%d %s\n", feridx, fername );
	} /*endif*/

	/*return 0;*/

} /* end of main */

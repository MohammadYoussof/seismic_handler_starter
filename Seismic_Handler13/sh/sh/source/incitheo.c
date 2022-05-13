
/* file INCITHEO.C
 *      ==========
 *
 * version 5, 22-May-2006
 *
 * returns theoretical angle of incidence for P-waves
 * K. Stammler, 29-OCT-91
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1992,  Klaus Stammler, Federal Institute for Geosciences
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
#include BC_SYSBASE
#include "erusrdef.h"
#include "itusrdef.h"


/* global variables */
static char     itv_table[BC_FILELTH+1];


/*------------------------------------------------------------------------*/



void it_settable( char table[], STATUS *status )

/* sets new table name
 *
 * parameters of routine
 * char       table[];      input; new table name
 * STATUS     *status;      output; return status
 */
{
	/* executable code */

	if  (strlen(table) > BC_FILELTH)  {
		*status = ITE_STROVFL;
		return;
	} /*endif*/
	strcpy( itv_table, table );

} /* end of it_settable */



/*------------------------------------------------------------------------*/



float it_getinci( float distance, float depth, STATUS *status )

/* returns angle of incidence in degrees.  Deviation from table of
 * Hoang-Trong and L. Behe is less than 0.1 deg.
 *
 * parameters of routine
 * float      distance;     input; distance of event in degrees
 * float      depth;        input; depth of event in km
 * STATUS     *status;      output; return status
 *                          returns angle of incidence of P in degrees
 */
{
	/* local variables */
	FILE     *tf;                  /* table file */
	char     line[BC_LINELTH+1];   /* current line */
	float    currdist;             /* current depth */
	float    loinci, hiinci;       /* inci on depth0 and 800 km */
	float    inci;                 /* return value */

	/* executable code */

	if  (distance < 0.0  ||  distance >= 100.0)  {
		*status = ITE_DISTRANGE;
		err_setcontext( " ## distance " ); err_setcontext_r( distance );
		return 0.0;
	} else if  (depth < 0.0  ||  depth >= 800.0)  {
		*status = ITE_DEPTHRANGE;
		err_setcontext( " ## depth " ); err_setcontext_r( depth );
		return 0.0;
	} /*endif*/

	tf = sy_fopen( itv_table, "r" );
	if  (tf == NULL)  {
		*status = ITE_OPNRD;
		err_setcontext( " ## file " ); err_setcontext( itv_table );
		return 0.0;
	} /*endif*/

	/* read off comment lines */
	*line = '!';
	while  (*line == '!')
		if  (fgets( line, BC_LINELTH, tf ) == NULL)  {
			*status = ITE_FILREAD;
			err_setcontext( " ## file " ); err_setcontext( itv_table );
			fclose( tf );
			return 0.0;
		} /*endif*/

	sscanf( line, "%f", &currdist );
	while  (currdist < distance)  {
		if  (fgets( line, BC_LINELTH, tf ) == NULL)  {
			*status = ITE_FILREAD;
			err_setcontext( " ## file " ); err_setcontext( itv_table );
			fclose( tf );
			return 0.0;
		} /*endif*/
		sscanf( line, "%f", &currdist );
	} /*endif*/
	fclose( tf );

	if  (sscanf(line+6,"%f",&loinci) != 1)  {*status=ITE_FCNV;return 0.0;}
	if  (sscanf(line+36,"%f",&hiinci) != 1)  {*status=ITE_FCNV;return 0.0;}
	inci = (hiinci-loinci)/800.0;   /* deg/km */
	inci = loinci + depth*inci;
	return inci;

} /* end of it_getinci */



/*------------------------------------------------------------------------*/


/* file evshow.c
 *      ========
 *
 * version 2, 19-Feb-96
 *
 * shows event file
 * K. Stammler, 26-Jul-93
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
#include BC_SYSBASE
#include "eventdsc.h"



int main( void )
{
	/* local variables */
	EvStatusT status;         /* return status */

	/* executable code */

	EvShowEmptyValues( FALSE );
	status = EveNOERROR;
	EvReformatEventfile( "testx.evt", "TT", EvGetEvent,
		EvPutEvent, &status );

	return 0;

} /* end of main */

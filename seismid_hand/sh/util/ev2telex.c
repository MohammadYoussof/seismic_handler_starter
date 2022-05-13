
/* file ev2telex.c
 *      ==========
 *
 * version 5, 14-Oct-96
 *
 * reformats event to telex format
 * K. Stammler, 28-Jul-93
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
#include "eventdsc.h"
#include "evtelex.h"
#include "cpar.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char      infile[BC_FILELTH+1];    /* input file */
	char      outfile[BC_FILELTH+1];   /* output file */
	char      header[BC_LINELTH+1];    /* header line */
	char      station[BC_LINELTH+1];   /* station name */
	EvStatusT status;                  /* return status */
	BOOLEAN   ignoreflag=FALSE;        /* ignore telex flag */

	/* executable code */

	pa_init( argc, argv );
	if  (pa_pnumber() != 4)  {
		fprintf( stderr, "Usage: %s <input> <output> <header> <station>\n",
			pa_progname() );
		fprintf( stderr, "    Options:    -ignoreflag    ignores telex flag\n" );
		return 1;
	} /*endif*/

	/* get parameters */
	strcpy( infile, pa_pvalue(1) );
	strcpy( outfile, pa_pvalue(2) );
	strcpy( header, pa_pvalue(3) );
	strcpy( station, pa_pvalue(4) );
	ignoreflag = pa_qspecified( "-ignoreflag" );

	if  (ignoreflag)  EfSetIgnoreTelexFlag( TRUE );
	EfSetTelexHeader( header );
	EfSetTelexStation( station );
	status = EveNOERROR;
	EvReformatEventfile( infile, outfile, EvGetEvent,
		EfPutTelex, &status );

#	ifndef BC_VAX
	return 0;
#	endif

} /* end of main */

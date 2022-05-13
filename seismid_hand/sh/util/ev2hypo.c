
/* file ev2hypo.c
 *      =========
 *
 * version 3, 4-Nov-93
 *
 * reformats event to HYPOELLIPSE format
 * K. Stammler, 8-Sep-93
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
#include "eventdsc.h"
#include "evhypo.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char      infile[BC_FILELTH+1];    /* input file */
	char      outfile[BC_FILELTH+1];   /* output file */
	EvStatusT status;                  /* return status */
	BOOLEAN   fix_depth;               /* use fixed depth */
	char      *val;                    /* pointer to value */
	float     depth;                   /* value of fixed depth */

	/* executable code */

	pa_init( argc, argv );

	if  (pa_pnumber() < 1 || *pa_pvalue(1) == EvEOS)  {
		printf( "   input filename: " );
		EvReadLineInteractive( stdin, BC_FILELTH, infile );
	} else {
		strcpy( infile, pa_pvalue(1) );
	} /*endif*/

	if  (pa_pnumber() < 2 || *pa_pvalue(2) == EvEOS)  {
		printf( "   output filename: " );
		EvReadLineInteractive( stdin, BC_FILELTH, outfile );
	} else {
		strcpy( outfile, pa_pvalue(2) );
	} /*endif*/

	fix_depth = pa_qspecified( "-d" );
	if  (fix_depth)  {
		val = pa_qvalue( "-d" );
		if  (val == NULL || strcmp(val,"free") == 0)  {
			fix_depth = FALSE;
			depth = 0.0;
		} else if  (sscanf( pa_qvalue("-d"), "%f", &depth ) != 1)  {
			printf( "*** ev2hypo: couldn't read depth ***\n" );
			exit( 1 );
		} /*endif*/
	} /*endif*/

	EfSetHypoUseS( pa_qspecified( "-s" ) );
	EfSetHypoDepth( fix_depth, depth );

	status = EveNOERROR;
	EvReformatEventfile( infile, outfile, EvGetEvent,
		EfPutHypo, &status );

#	ifndef BC_VAX
	return 0;
#	endif

} /* end of main */


/* file catflf.c
 *      ========
 *
 * version 1, 14-May-2003
 *
 * Multiplies / divides two filters and writes result to stdout
 * K. Stammler, 14-May-2003
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
#include "ffusrdef.h"
#include "erusrdef.h"
#include "globalparams.h"


/* prototypes of local routines */
static void ctf_writeflf( FFT_RATFCT *fil );


/* global variables */
char shd_filter[BC_FILELTH+1];
char shd_scratch[BC_FILELTH+1];


int main( int argc, char *argv[] )
{
	/* local variables */
	char     oper[cBcLineLth+1];     /* operation */
	char     filnam1[cBcFileLth+1];  /* filter filename 1 */
	char     filnam2[cBcFileLth+1];  /* filter filename 2 */
	char     *eptr;                  /* pointer to environment */
	TSyStatus status;                /* return status */
	FFT_RATFCT  fil1;                /* filter 1 */
	FFT_RATFCT  fil2;                /* filter 2 */
	FFT_RATFCT  filr;                /* resulting filter */
	int      i, j;                   /* counters */

	/* executable code */

	if  (argc != 4)  {
		fprintf( stderr, "Usage: %s <filter1> <operation> <filter2>\n", argv[0] );
		return 1;
	} /*endif*/

	GpReadParfile();

	eptr = (char *)getenv( "SH_FILTER" );
	if  (eptr != NULL)  {
		strcpy( shd_filter, eptr );
	} else {
		*shd_filter = '\0';
	} /*endif*/

	/* get parameters */
	strcpy( filnam1, argv[1] );
	strcpy( oper, argv[2] );
	strcpy( filnam2, argv[3] );

	status = cBcNoError;
	ff_read_filter( filnam1, 1, &fil1, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );
	ff_read_filter( filnam2, 1, &fil2, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );

	if  (strcmp(oper,"mul") == 0)  {
		if  (fil1.no_of_zeroes + fil2.no_of_zeroes > FFC_MAXDEGREE ||
			fil1.no_of_poles + fil2.no_of_poles > FFC_MAXDEGREE)  {
			fprintf( stderr, "%s: too many poles/zeros in output filter\n",
				argv[0] );
			return 1;
		} /*endif*/
		filr.norm = fil1.norm * fil2.norm;
		filr.no_of_zeroes = fil1.no_of_zeroes + fil2.no_of_zeroes;
		i = 0;
		for  (j=0; j<fil1.no_of_zeroes; j++)  filr.zero[i++] = fil1.zero[j];
		for  (j=0; j<fil2.no_of_zeroes; j++)  filr.zero[i++] = fil2.zero[j];
		filr.no_of_poles = fil1.no_of_poles + fil2.no_of_poles;
		i = 0;
		for  (j=0; j<fil1.no_of_poles; j++)   filr.pole[i++] = fil1.pole[j];
		for  (j=0; j<fil2.no_of_poles; j++)   filr.pole[i++] = fil2.pole[j];
	} else if  (strcmp(oper,"div") == 0)  {
		if  (fil1.no_of_zeroes + fil2.no_of_poles > FFC_MAXDEGREE ||
			fil1.no_of_poles + fil2.no_of_zeroes > FFC_MAXDEGREE)  {
			fprintf( stderr, "%s: too many poles/zeros in output filter\n",
				argv[0] );
			return 1;
		} /*endif*/
		filr.norm = fil1.norm / fil2.norm;
		filr.no_of_zeroes = fil1.no_of_zeroes + fil2.no_of_poles;
		i = 0;
		for  (j=0; j<fil1.no_of_zeroes; j++)  filr.zero[i++] = fil1.zero[j];
		for  (j=0; j<fil2.no_of_poles; j++)   filr.zero[i++] = fil2.pole[j];
		filr.no_of_poles = fil1.no_of_poles + fil2.no_of_zeroes;
		i = 0;
		for  (j=0; j<fil1.no_of_poles; j++)   filr.pole[i++] = fil1.pole[j];
		for  (j=0; j<fil2.no_of_zeroes; j++)  filr.pole[i++] = fil2.zero[j];
	} else {
		fprintf( stderr, "%s: illegal operation %s\n", argv[0], oper );
		return 1;
	} /*endif*/

	ff_shorten_zeroes( &filr, 1 );

	fprintf( stdout, "! filter produced by catflf\n" );
	fprintf( stdout, "! %s %s %s\n", filnam1, oper, filnam2 );
	ctf_writeflf( &filr );

	return 0;

} /* end of main */


/*----------------------------------------------------------------------------*/



static void ctf_writeflf( FFT_RATFCT *fil )

/* writes filter to stdout
 *
 * parameters of routine
 * FFT_RATFCT   *fil;       input; filte to be written
 */
{
	/* local variables */
	int      i;           /* counter */

	/* executable code */

	fprintf( stdout, "1357913578\n" );
	fprintf( stdout, "1\n" );
	fprintf( stdout, "%g\n", fil->norm );
	fprintf( stdout, "%d\n", fil->no_of_zeroes );
	for  (i=0; i<(fil->no_of_zeroes); i++)
		fprintf( stdout, "(%g,%g)\n", fil->zero[i].re, fil->zero[i].im );
	fprintf( stdout, "%d\n", fil->no_of_poles );
	for  (i=0; i<(fil->no_of_poles); i++)
		fprintf( stdout, "(%g,%g)\n", fil->pole[i].re, fil->pole[i].im );

} /* end of ctf_writeflf */



/*----------------------------------------------------------------------------*/

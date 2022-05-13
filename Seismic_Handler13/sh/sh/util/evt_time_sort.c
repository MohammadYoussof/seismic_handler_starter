
/* file evt_time_sort.c
 * ====================
 *
 * version 2, 30-Jun-95
 *
 * sorts phases in evt-file by time
 * K. Stammler, 24-Jun-95
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
#include "tcusrdef.h"
#include "cpar.h"
#include "eventdsc.h"
#include "timelist.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char     inpfile[cBcFileLth+1];  /* name of input file */
	FILE     *fp;                    /* pointer to input file */
	EvEventT *event;                 /* pointer event (phase) info */
	BOOLEAN  eof;                    /* eof found */
	EvStatusT status;                /* return status */
	TLT_ELEMENT *elm;                /* time list element */
	TLT_ELEMENT *root;               /* time list root */

	/* executable code */

	status = cBcNoError;
	pa_init( argc, argv );

	if  (pa_pnumber() != 1)  {
		fprintf( stderr, "Usage: %s <inpfile>\n", pa_progname() );
		return 1;
	} /*endif*/
	strcpy( inpfile, pa_pvalue(1) );

	/* open input file */
	fp = sy_fopen( inpfile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: input file %s not found\n",
			pa_progname(), inpfile );
		return 1;
	} /*endif*/

	/* loop all phases on input */
	FOREVER  {
		event = (EvEventT *)sy_allocmem( 1, (int)sizeof(EvEventT), &status );
		if  (Severe(&status))  return 1;
		EvGetEvent( fp, event, &eof, &status );
		if  (eof)  break;
		if  (Severe(&status))  return 1;
		tl_enlist( event->onset_time, event, &status );
		if  (Severe(&status))  return 1;
	} /*endfor*/

	sy_fclose( fp );

	elm = root = tl_get_root();
	do  {
		EvPutEvent( stdout, elm->info, &status );
		if  (Severe(&status))  return 1;
		elm = elm->next;
	} while  (elm != root);

	return 0;

} /* end of main */


/* file evt_copy_and_split.c
 *      ====================
 *
 * version 2, 11-Oct-95
 *
 * reads evt-file and copies it to another directory.  The new output file
 * has the station name as new prefix.  If several stations are in one
 * file, the file is split up.
 *
 * K. Stammler, 30-Jun-95
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
#include "cpar.h"
#include "utusrdef.h"
#include "eventdsc.h"
#include "erusrdef.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char     evtfile[cBcFileLth+1];     /* name of evt-file */
	char     inpdir[cBcFileLth+1];      /* input directory */
	char     outdir[cBcFileLth+1];      /* output directory */
	char     fname[cBcFileLth+1];       /* complete filename */
	FILE     *inp, *out;                /* pointers to input/output files */
	EvEventT event;                     /* event (phase) info */
	TSyBoolean eof;                     /* eof found */
	TSyStatus status;                   /* return status */
	char     cstation[cBcLineLth+1];    /* current station */
	char     fstation[cBcLineLth+1];    /* file station */

	/* executable code */

	status = cBcNoError;

	/* check command line */
	pa_init( argc, argv );
	if  (pa_pnumber() != 3)  {
		fprintf( stderr, "Usage: %s <evt-file> <inp-dir> <out-dir>\n",
			pa_progname() );
		return 1;
	} /*endif*/

	/* get parameters */
	strcpy( evtfile, pa_pvalue(1) );
	strcpy( inpdir, pa_pvalue(2) );
	strcpy( outdir, pa_pvalue(3) );

	/* concat input filename and open it */
	if  (strlen(inpdir)+strlen(evtfile)+1 > cBcFileLth)  {
		fprintf( stderr, "%s: input name too long %s %s\n", pa_progname(),
			inpdir, evtfile );
		return 1;
	} /*endif*/
	strcpy( fname, inpdir );
	strcat( fname, "/" );
	strcat( fname, evtfile );
	inp = sy_fopen( fname, "r" );
	if  (inp == NULL)  {
		fprintf( stderr, "%s: error opening input file %s\n", pa_progname(),
			fname );
		return 1;
	} /*endif*/

	/* loop on phases in file */
	*cstation = *fstation = '\0';
	out = NULL;
	FOREVER {
		EvGetEvent( inp, &event, &eof, &status );
		if  (eof)  break;
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
		strcpy( cstation, event.station );
		ut_uncap( cstation );
		/* if current station is different, close opened file */
		if  (strcmp(fstation,cstation) != 0)
			if  (out != NULL)  {
				sy_fclose( out );
				out = NULL;
				*fstation = '\0';
			} /*endif*/
		/* open output file if closed */
		if  (*fstation == '\0')  {
			/* build filename and open it */
			strcpy( fstation, cstation );
			if  (strlen(fstation)+strlen(outdir)+strlen(evtfile)+2 > cBcFileLth)  {
				fprintf( stderr, "%s: output name too long\n", pa_progname() );
				return 1;
			} /*endif*/
			strcpy( fname, outdir );
			strcat( fname, "/" );
			strcat( fname, fstation );
			strcat( fname, "_" );
			strcat( fname, evtfile );
			out = sy_fopen( fname, "a" );
			if  (out == NULL)  {
				fprintf( stderr, "%s: error opening output file %s\n",
					pa_progname(), fname );
				return 1;
			} /*endif*/
		} /*endif*/
		/* write to output file */
		EvPutEvent( out, &event, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
	} /*endfor*/

	sy_fclose( inp );
	if  (out != NULL)  sy_fclose( out );

	return 0;

} /* end of main */

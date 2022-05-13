
/* file onset_convert.c
 *      ===============
 *
 * version 1, 8-Dec-93
 *
 * Converts onset times computed by program 'onset' to SHM-readable format
 * K. Stammler, 8-Dec-93
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
#include BC_TCUSRDEF


#define MAXSEL 20


/* global variables */
static char selphase[MAXSEL][BC_SHORTSTRLTH+1];


int main( int argc, char *argv[] )
{
	/* local variables */
	char     station[BC_SHORTSTRLTH+1];/* station name */
	char     origin[BC_TIMELTH+1];     /* origin time */
	char     selfile[BC_FILELTH+1];    /* phase selection file */
	char     inpfile[BC_FILELTH+1];    /* name of input file */
	char     outfile[BC_FILELTH+1];    /* name of output file */
	FILE     *inp, *out;               /* pointers to files */
	char     line[BC_LINELTH+1];       /* current line */
	char     phase[BC_SHORTSTRLTH+1];  /* phase name */
	BOOLEAN  do_times;                 /* times found */
	int      d1, d2;                   /* dummy */
	float    df;                       /* dummy */
	float    travtime;                 /* travel time of phase in sec */
	STATUS   locstat;                  /* local status */
	char     onset[BC_TIMELTH+1];      /* onset time of phase */
	int      num_sel;                  /* number of selections */
	int      slen;                     /* string length */
	int      i;                        /* counter */
	BOOLEAN  phase_ok;                 /* put phase to output file */

	/* executable code */

	/* get parameters */
	if  (argc != 6)  {
		printf( "*** Usage: onset_convert <station> <origin>" );
		printf( " <select-file> <input> <output> ***\n" );
		return 1;
	} /*endif*/
	strcpy( station, argv[1] );
	strcpy( origin, argv[2] );
	strcpy( selfile, argv[3] );
	strcpy( inpfile, argv[4] );
	strcpy( outfile, argv[5] );

	/* read selected phases */
	inp = sy_fopen( selfile, "r" );
	if  (inp == NULL)  {
		printf( "*** onset_convert: couldn't find phase selection file %s ***\n",
			selfile );
		return 1;
	} /*endif*/
	num_sel = 0;
	while  (fgets(line,BC_LINELTH,inp) != NULL)  {
		if  (*line == '!')  continue;
		if  (num_sel == MAXSEL)  {
			printf( "*** onset_convert: only %d selected phases allowed ***\n",
				MAXSEL );
			return 1;
		} /*endif*/
		slen = (int)strlen( line ) - 1;
		if  (line[slen] == '\n')  line[slen] = '\0';
		if  (slen > BC_SHORTSTRLTH)  {
			printf( "*** onset_convert: phase name %s too long; ignored. ***\n",
				line );
			continue;
		} /*endif*/
		strcpy( selphase[num_sel++], line );
	} /*endwhile*/
	sy_fclose( inp );

	/* open files */
	inp = sy_fopen( inpfile, "r" );
	if  (inp == NULL)  {
		printf( "*** onset_convert: couldn't find input file %s ***\n", inpfile );
		return 1;
	} /*endif*/
	out = sy_fopen( outfile, "a" );
	if  (out == NULL)  {
		printf( "*** onset_convert: couldn't find output file %s ***\n", outfile);
		sy_fclose( inp );
		return 1;
	} /*endif*/

	do_times = FALSE;
	while  (fgets(line,BC_LINELTH,inp) != NULL)  {
		if  (!do_times)  {
			if  (strncmp(line,"    PHASE   HH  MM   SEC",24) == 0)
				do_times = TRUE;
			continue;
		} /*endif*/
		sscanf( line, "%s %d %d %f %f", phase, &d1,&d2,&df, &travtime );
		/* check phase */
		phase_ok = FALSE;
		for  (i=0; i<num_sel; i++)  {
			if  (strcmp(phase,selphase[i]) == 0)  {
				phase_ok = TRUE;
				break;
			} /*endif*/
		} /*endfor*/
		if  (!phase_ok)  continue;
		locstat = BC_NOERROR;
		tc_tadd( origin, travtime, onset, &locstat );
		fprintf( out, "%4s %8s %s\n", station, phase, onset );
	} /*endwhile*/

	/* close files */
	sy_fclose( out );
	sy_fclose( inp );

	return 0;

} /* end of main */

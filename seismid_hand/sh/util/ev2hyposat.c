
/* file ev2hyposat.c
 *      ============
 *
 * version 1, 13-Feb-2003
 * 
 * Converts evt-File to HypoSAT input
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
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "tcusrdef.h"
#include "cpar.h"
#include "eventdsc.h"


#define FLAG_ATIME 0
#define FLAG_AZIM  1
#define FLAG_SLOW  2
#define FLAG_DTIME 3
#define FLAG_CORR  4
#define FLAG_MODEL 5

#define FVAL_ATIME 'T'
#define FVAL_AZIM  'A'
#define FVAL_SLOW  'S'
#define FVAL_DTIME 'D'
#define FVAL_CORR  'R'


static char efv_refstation[cBcLineLth+1] = "GRA1";


/* prototypes of local routines */
void PutHyposat( FILE *hypo, EvEventT *event, EvStatusT *status );



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

#ifdef XXX
	EfSetHypoUseS( pa_qspecified( "-s" ) );
	EfSetHypoDepth( fix_depth, depth );
#endif

	status = EveNOERROR;
	EvReformatEventfile( infile, outfile, EvGetEvent,
		PutHyposat, &status );

#	ifndef BC_VAX
	return 0;
#	endif

} /* end of main */



/*-------------------------------------------------------------------*/



void PutHyposat( FILE *hypo, EvEventT *event, EvStatusT *status )

/* Prints event "event" in HypoSAT format to file "hypo".  This
 * routine may be used as a "put_routine" in "EvReformatEvenfile".
 *
 * parameters of routine
 * FILE       *hypo;          input; output file
 * EvEventT   *event;         input; event information
 * EvStatusT  *status;        output; return status
 */
{
	/* local variables */
	NTIME     ntime;         /* numeric time */
	float     stddev_onset;  /* standard deviation of onset time */
	float     stddev_azim;   /* standard deviation of back azimuth */
	float     stddev_slow;   /* standard deviation of slowness */
	float     azim;          /* back azimuth in deg */
	float     slow;          /* slowness in s/deg */
	char      flags[cBcShortStrLth+1];  /* exec flags */

	/* executable code */

	if  (event == NULL)  return;
	if  ((event->station[0] == '\0' || event->onset_time[0] == '\0'))  return;

	azim = 0.0;
	slow = 0.0;
	stddev_azim = 0.0;
	stddev_slow = 0.0;
	strcpy( flags, "T__DR_" );

	tc_t2n( event->onset_time, &ntime, status );
	if  (SySevere(status))  {
		fprintf( stderr, "ev2hyposat: cannot read onset time %s.  Ignored.\n",
			event->onset_time );
		return;
	} /*endif*/

	if  (strcmp(event->phase,"P") == 0 || strncmp(event->phase,"PKP",3) == 0
		|| strcmp(event->phase,"Pn") == 0 || strcmp(event->phase,"Pg") == 0)  {
		stddev_onset = 0.2;
	} else {
		stddev_onset = 1.0;
	} /*endif*/

	if  (strcmp(event->station,efv_refstation) == 0)  {
		if  (event->l_slowness != EvEMPTY_SLOWNESS
			&& event->l_azimuth != EvEMPTY_AZIMUTH)  {
			slow = event->l_slowness;
			azim = event->l_azimuth;
			stddev_slow = 0.5;
			stddev_azim = 5.0;
			flags[FLAG_AZIM] = FVAL_AZIM;
			flags[FLAG_SLOW] = FVAL_SLOW;
		} else if  (event->b_slowness != EvEMPTY_SLOWNESS
			&& event->b_azimuth != EvEMPTY_AZIMUTH)  {
			slow = event->b_slowness;
			azim = event->b_azimuth;
			stddev_slow = 0.5;
			stddev_azim = 5.0;
			flags[FLAG_AZIM] = FVAL_AZIM;
			flags[FLAG_SLOW] = FVAL_SLOW;
		} /*endif*/
	} /*endif*/

	fprintf( hypo, "%-5s %8s %4d %02d %02d %02d %02d %6.3f %5.3f %6.2f %5.2f %5.2f %5.2f %6s\n",
		event->station, event->phase, ntime.year, ntime.month, ntime.day,
		ntime.hour, ntime.min, (float)(ntime.sec + (ntime.ms/1000.0)),
		stddev_onset, azim, stddev_azim, slow, stddev_slow, flags );

} /* end of PutHyposat */



/*-------------------------------------------------------------------*/

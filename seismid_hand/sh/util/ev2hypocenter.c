
/* file ev2hypocenter.c
 *      ===============
 *
 * version 2, 12-Mar-2003
 * 
 * Converts evt-File to Hypocenter input
 * K. Stammler, 6-Mar-2003
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

#define EVTEXTLTH 4000
#define MAXPHASE 10


static float hcv_srcdepth;   /* source depth */


/* prototypes of local routines */
void PutHypocenter( FILE *hypo, EvEventT *event, EvStatusT *status );
void PutHypocenterMem( FILE *hypo, EvEventT *event, EvStatusT *status );
void PrintMulti( FILE *hypo, char evtext[] );



int main( int argc, char *argv[] )
{
	/* local variables */
	char      infile[BC_FILELTH+1];    /* input file */
	char      outfile[BC_FILELTH+1];   /* output file */
	EvStatusT status;                  /* return status */
	BOOLEAN   fix_depth;               /* use fixed depth */

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
		sscanf( pa_qvalue( "-d" ), "%f", &hcv_srcdepth );
	} else {
		hcv_srcdepth = -100.0;
	} /*endif*/

#ifdef XXX
	EfSetHypoUseS( pa_qspecified( "-s" ) );
	EfSetHypoDepth( fix_depth, depth );
#endif

	status = EveNOERROR;
	if  (pa_qspecified("-a"))  {
		EvReformatEventfile( infile, outfile, EvGetEvent,
			PutHypocenterMem, &status );
	} else {
		EvReformatEventfile( infile, outfile, EvGetEvent,
			PutHypocenter, &status );
	} /*endif*/

#	ifndef BC_VAX
	return 0;
#	endif

} /* end of main */



/*-------------------------------------------------------------------*/



void PutHypocenter( FILE *hypo, EvEventT *event, EvStatusT *status )

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
	static TSyBoolean newevent=TRUE;    /* new event started ? */
	NTIME     ntime;                    /* numeric time */
	float     depth;                    /* source depth used for inversion */
	char      depthflag;                /* depth flag */
	char      onstype;                  /* onset type */

	/* executable code */

	if  (event == NULL)  {newevent=TRUE; return;}
	if  ((event->station[0] == '\0' || event->onset_time[0] == '\0'))  return;

	tc_t2n( event->onset_time, &ntime, status );
	if  (SySevere(status))  {
		fprintf( stderr, "ev2hypocenter: cannot read onset time %s.  Ignored.\n",
			event->onset_time );
		return;
	} /*endif*/

	if  (newevent)  {
		depthflag = (hcv_srcdepth < 0) ? 'F' : 'S';
		depth = (hcv_srcdepth < 0.0) ? 5.0 : hcv_srcdepth;
		fprintf( hypo, " %4d %02d%02d %02d%02d %4.1f L                %5.1f%c                                   1\n",
			ntime.year, ntime.month, ntime.day, ntime.hour, ntime.min,
			(float)ntime.sec+(float)(ntime.ms)/1000.0, depth, depthflag );
		newevent = FALSE;
	} /*endif*/

	onstype = (event->onset_type == EvcOnsetImpulsive) ? 'I' : 'E';

	fprintf( hypo, " %-4s I%c %c%-8s%02d%02d %5.2f\n",
		event->station, event->component, onstype, event->phase,
		ntime.hour, ntime.min, (float)(ntime.sec) + (float)ntime.ms/1000.0 );

} /* end of PutHypocenter */



/*-------------------------------------------------------------------*/



void PutHypocenterMem( FILE *hypo, EvEventT *event, EvStatusT *status )

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
	static char evtext[EVTEXTLTH+1]=""; /* event text */
	static TSyBoolean newevent=TRUE;    /* new event started ? */
	NTIME     ntime;                    /* numeric time */
	float     depth;                    /* source depth used for inversion */
	char      depthflag;                /* depth flag */
	char      onstype;                  /* onset type */
	char      line[cBcLineLth+1];       /* current line */
	char      lphase[cBcLineLth+1];     /* phase name */

	/* executable code */

	if  (event == NULL)  {
		if  (*evtext != '\0')  strcat( evtext, "\n" );
		PrintMulti( hypo, evtext );
		*evtext = '\0';
		newevent=TRUE; 
		return;
	} /*endif*/

	if  ((event->station[0] == '\0' || event->onset_time[0] == '\0'))  return;

	tc_t2n( event->onset_time, &ntime, status );
	if  (SySevere(status))  {
		fprintf( stderr, "ev2hypocenter: cannot read onset time %s.  Ignored.\n",
			event->onset_time );
		return;
	} /*endif*/

	if  (newevent)  {
		depthflag = (hcv_srcdepth < 0.0) ? 'S' : 'F';
		depth = (hcv_srcdepth < 0.0) ? 5.0 : hcv_srcdepth;
		sprintf( line, " %4d %02d%02d %02d%02d %4.1f L                %5.1f%c                                   1\n",
			ntime.year, ntime.month, ntime.day, ntime.hour, ntime.min,
			(float)ntime.sec+(float)(ntime.ms)/1000.0, depth, depthflag );
		strcat( evtext, line );
		newevent = FALSE;
	} /*endif*/

	onstype = (event->onset_type == EvcOnsetImpulsive) ? 'I' : 'E';

	strcpy( lphase, event->phase );
	if  (strcmp(lphase,"Px") == 0)  strcpy( lphase, "P@" );
	if  (strcmp(lphase,"Sx") == 0)  strcpy( lphase, "S@" );
	sprintf( line, " %-4s I%c %c%-8s%02d%02d %5.2f\n",
		event->station, event->component, onstype, lphase,
		ntime.hour, ntime.min, (float)(ntime.sec) + (float)ntime.ms/1000.0 );
	strcat( evtext, line );

} /* end of PutHypocenterMem */



/*-------------------------------------------------------------------*/



void PrintMulti( FILE *hypo, char evtext[] )

/* Prints all variations of Pg,Pn,Sg,SN phases
 *
 * parameters of routine
 * FILE       *hypo;       input; pointer to file
 * char       evtext[];    input; event phases
 */
{
	/* local variables */
	int      tpos[MAXPHASE];      /* positions of '@' */
	int      tnum;                /* number of positions */
	int      pow;                 /* 2 ** tnum */
	int      i, j, n;             /* counter */
	int      slen;                /* string length */

	/* executable code */

	/* store all positions of '@'s */
	tnum = 0;
	slen = strlen( evtext );
	for  (i=0; i<slen; i++)  {
		if  (evtext[i] == '@')  {
			if  (tnum == MAXPHASE)  {
				fprintf( stderr, "ev2hypocenter: too many unspecified phases\n" );
			} else {
				tpos[tnum++] = i;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	if  (tnum == 0)  {
		pow = 1;
	} else {
		pow = 2;
		for  (i=1; i<tnum; i++)
			pow *= 2;
	} /*endif*/

	/* run through all possibilities */
	for  (n=0; n<pow; n++)  {
		/* loop all bits */
		for  (i=0; i<tnum; i++)  {
			evtext[tpos[i]] = ((n>>i) & 1) ? 'g' : 'n';
		} /*endif*/
		fprintf( hypo, "%s", evtext );
	} /*endfor*/

} /* end of PrintMulti */



/*-------------------------------------------------------------------*/

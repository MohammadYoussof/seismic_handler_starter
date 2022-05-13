
/* file ev4hypocenter.c
 *      ===============
 *
 * version 2, 21-Nov-2005
 *
 * Reads output files of hypocenter program
 * K. Stammler, 11-Mar-2003
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
#include "eventdsc.h"


int main( int argc, char *argv[] )
{
	/* local variables */
	TSyStatus status;                  /* return status */
	char     hypoutnam[cBcFileLth+1];  /* hypocenter output file */
	char     hypsumnam[cBcFileLth+1];  /* hypocenter summary file */
	char     evtout[cBcFileLth+1];     /* name of evt output file */
	char     viewnam[cBcFileLth+1];    /* view file */
	FILE     *hypout;                  /* pointer to hypocenter output file */
	FILE     *hypsum;                  /* pointer to summary file */
	FILE     *evf;                     /* pointer to evt file */
	FILE     *view;                    /* pointer to view file */
	char     line[cBcLineLth+1];       /* crrent line of file */
	int      evnum;                    /* event number */
	int      optevnum;                 /* best event number */
	float    rms;                      /* current rms value */
	float    optrms;                   /* best rms value */
	char     tmpstr[cBcLineLth+1];     /* scratch string */
	NTIME    ntime;                    /* origin time */
	char     origtime[cBcTimeLth+1];   /* origin time */
	float    lat, lon, depth;          /* hypocenter */
	int      itmp;                     /* scratch integer */
	float    tmp;                      /* scratch float */
	EvEventT evt;                      /* phase info */
	int      phasenum;                 /* phase number */
	int      currev;                   /* current event number */
	int      phasecnt;                 /* current number of phases */
	int      maxphasecnt;              /* maximum number of phases */
	TSyBoolean freedepth;              /* depth free or fixed */

	/* executable code */

	/* get parameters */
	if  (argc != 5)  {
		fprintf( stderr, "Usage: %s <hypout> <hypsum> <evtout> <view>\n",
			argv[0] );
		fprintf( stderr, "   Example %s print.out hypsum.out hypout.evt\n",
			argv[0] );
		return 1;
	} /*endif*/
	strcpy( hypoutnam, argv[1] );
	strcpy( hypsumnam, argv[2] );
	strcpy( evtout, argv[3] );
	strcpy( viewnam, argv[4] );

	evf = fopen( evtout, "w" );
	if  (evf == NULL)  {
		fprintf( stderr, "%s: cannot open output file %s.  Abort.\n",
			argv[0], evtout );
		exit( 1 );
	} /*endif*/

	view = fopen( viewnam, "w" );
	if  (view == NULL)  {
		fprintf( stderr, "%s: cannot open output file %s.  Abort.\n",
			argv[0], viewnam );
		exit( 1 );
	} /*endif*/

	hypsum = fopen( hypsumnam, "r" );
	if  (hypsum == NULL)  {
		fprintf( stderr, "%s: file %s not found.  Abort.\n", argv[0], hypsumnam );
		exit( 1 );
	} /*endif*/

	/* read through summary file */
	evnum = 1;
	rms = optrms = -1.0;
	maxphasecnt = 0;
	while  (fgets(line,cBcLineLth,hypsum) != NULL)  {
		if  (strncmp(line," Date",5) == 0)  continue;
		/* get rms value and number of phases */
		strncpy( tmpstr, line+62, 5 );
		tmpstr[5] = '\0';
		sscanf( tmpstr, "%f", &rms );
		sscanf( line+51, "%d", &phasecnt );
		/*printf( "phasecnt: %d  rms: %5.2f    ", phasecnt, rms );*/
		if  ((phasecnt > maxphasecnt)
			|| (phasecnt == maxphasecnt && (optrms < 0.0 || rms < optrms)))  {
			/*printf( "+" );*/
			optevnum = evnum;
			optrms = rms;
			maxphasecnt = phasecnt;
			/* read event info from line */
			/* read date */
			sscanf( line, "%d", &itmp );
			ntime.year = itmp / 10000;
			itmp -= ntime.year*10000;
			ntime.month = itmp / 100;
			itmp -= ntime.month*100;
			ntime.day = itmp;
			if  (ntime.year < 55)  {
				ntime.year += 2000;
			} else {
				ntime.year += 1900;
			} /*endif*/
			/* read time */
			sscanf( line+7, "%d", &itmp );
			ntime.hour = itmp / 100;
			itmp -= ntime.hour*100;
			ntime.min = itmp;
			/* read seconds */
			sscanf( line+12, "%f", &tmp );
			ntime.sec = (int)tmp;
			tmp -= (float)ntime.sec;
			ntime.ms = Nint(tmp*1000);
			status= cBcNoError;
			tc_n2t( &ntime, origtime, &status );
			if  (SySevere(&status))  {
				fprintf( stderr, "%s: error converting time.  Abort.\n", argv[0] );
				exit( 1 );
			} /*endif*/
			/* read latitude */
			sscanf( line+17, "%d", &itmp );
			sscanf( line+21, "%f", &tmp );
			lat = (float)itmp + tmp / 60.0;
			/* read longitude */
			sscanf( line+26, "%d", &itmp );
			sscanf( line+31, "%f", &tmp );
			lon = (float)itmp + tmp / 60.0;
			sscanf( line+37, "%f", &depth );
		} /*endif*/
		/*printf( "\n" );*/
		evnum++;
	} /*endwhile*/

	fclose( hypsum );

	/* open hypout file */
	hypout = fopen( hypoutnam, "r" );
	if  (hypout == NULL)  {
		fprintf( stderr, "%s: file %s not found.  Abort.\n", argv[0], hypoutnam );
		exit( 1 );
	} /*endif*/

	/* read through hypout file, count only valid locations */
	freedepth = TRUE;
	currev = 0;
	while  (fgets(line,cBcLineLth,hypsum) != NULL)  {
		if  (strncmp(line," ***** depth fixed by operator",30) == 0)
			freedepth = FALSE;
		if  (strncmp(line," stn   dist",11) == 0)  currev++;
		if  (currev == optevnum)  break;
	} /*endwhile*/
	fprintf( view, "%s", line );
	phasenum = 1;
	while  (fgets(line,cBcLineLth,hypsum) != NULL)  {
		EvInitializeEvent( &evt );
		fprintf( view, "%s", line );
		if  (*line == '\n' || line[1] == ' ')  break;
		strncpy( tmpstr, line+41, 2 );
		tmpstr[2] = '\0';
		sscanf( tmpstr, "%d", &(ntime.hour) );
		strncpy( tmpstr, line+43, 2 );
		tmpstr[2] = '\0';
		sscanf( tmpstr, "%d", &(ntime.min) );
		sscanf( line+46, "%f", &tmp );
		ntime.sec = (int)tmp;
		tmp -= (float)ntime.sec;
		ntime.ms = Nint(tmp*1000);
		status= cBcNoError;
		tc_n2t( &ntime, evt.onset_time, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error converting time.  Abort.\n", argv[0] );
			exit( 1 );
		} /*endif*/
		strncpy( evt.station, line+1, 5 );
		evt.station[5] = '\0';
		if  (evt.station[4] == ' ')  evt.station[4] = '\0';
		if  (evt.station[3] == ' ')  evt.station[3] = '\0';
		strncpy( evt.phase, line+25, 5 );
		if  (evt.phase[4] == ' ')  evt.phase[4] = '\0';
		if  (evt.phase[3] == ' ')  evt.phase[3] = '\0';
		if  (evt.phase[2] == ' ')  evt.phase[2] = '\0';
		if  (evt.phase[1] == ' ')  evt.phase[1] = '\0';
		if  (evt.phase[0] == 'S')  {
			evt.component = 'N';
		} else {
			evt.component = 'Z';
		} /*endif*/
		evt.pick_type = EvcPickTypeManual;
		evt.phase[5] = '\0';
		if  (phasenum == 1)  {
			/* print out location */
			evt.latitude = lat;
			evt.longitude = lon;
			evt.depth = depth;
			evt.depth_type = (freedepth) ? EvcDepthFree : EvcDepthEstimated;
			evt.loc_method = EvcLocMethHypocenter;
			strcpy( evt.origin_time, origtime );
		} /*endif*/
		phasenum++;
		EvPutEvent( evf, &evt, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error writing evtfile.  Abort.\n", argv[0] );
			exit( 1 );
		} /*endif*/
	} /*endwhile*/

	/* copy two more lines */
	fgets( line, cBcLineLth, hypsum );
	fprintf( view, "%s", line );
	fgets( line, cBcLineLth, hypsum );
	fprintf( view, "%s", line );

	fclose( hypout );
	fclose( evf );
	fclose( view );

	printf( "optev %d, rms %f %s (%5.2f,%6.2f) %4.1f\n",
		optevnum, optrms, origtime, lat, lon, depth );

	return 0;

} /* end of main */


/* file relonset2slo.c
 *      ==============
 *
 * version 7, 21-Nov-2005
 *
 * Computes slowness and azimuth from relative onset times given in
 * input file.  Format:  <station> <rel-time> ...    (one entry per line)
 *
 * K. Stammler, 28-Nov-96
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
#include "cpar.h"
#include "erusrdef.h"
#include "glusrdef.h"
#include "earthloc.h"
#include "fctxmt.h"


#define MAXSTATION 50
#define DEG_TO_KM 111.19
#define RAD_TO_DEG (180.0/M_PI)


/* global variables */
char shd_scratch[cBcFileLth+1];      /* dummy (needed in shmath, forgot why) */



int main( int argc, char *argv[] )
{
	/* local variables */
	static char  scode[MAXSTATION][cBcShortStrLth+1];  /* station names */
	static float relonset[MAXSTATION];    /* array of onset times */
	static float corr[MAXSTATION];        /* correlation coefficients */
	static float relx[MAXSTATION];        /* relative x-locations */
	static float rely[MAXSTATION];        /* relative y-locations */
	static float slat[MAXSTATION];        /* abs station location */
	static float slon[MAXSTATION];        /* abs station location */
	static float thonset[MAXSTATION];     /* theoretical onsets */
	char     fname[cBcFileLth+1];    /* name of input file */
	FILE     *fp;                    /* pointer to input file */
	float    dt;                     /* sample distance in sec */
	char     line[cBcLineLth+1];     /* current line of file */
	char     station[cBcLineLth+1];  /* station name */
	float    reltime;                /* relative time read from file */
	float    corrcoeff;              /* correlation coeff read from file */
	int      statno;                 /* number of stations read in */
	GLT_STATINF statinf;             /* station info */
	STATUS   status;                 /* return status */
	char     *env;                   /* pointer to environment variable */
	char     locfile[cBcFileLth+1];  /* name of station info file */
	int      i;                      /* counter */
	float    azim, azimerr;          /* azimuth with error */
	float    slo, sloerr;            /* slowness with error */
	float    sx, sy;                 /* cartesian coordinates */
	float    meanth, meanobs;        /* mean values */
	BOOLEAN  show_resid;             /* show residuals */
	float    xslo, xazim;            /* rescaled values */
	char     *newsloaz;              /* pointer to input slowness and azimuth */
	float    elat, elon;             /* optional epicenter location */
	TSyBoolean extloc;               /* external location give (use elat,elon) */
	double   d_dist, d_az, d_baz;    /* result of locdiff */
	int      arrid;                  /* array id */

	/* executable code */

	status = cBcNoError;

	pa_init( argc, argv );
	if  (pa_pnumber() != 1)  {
		fprintf( stderr, "Usage: %s <rel-file>\n", pa_progname() );
		fprintf( stderr, "      -dt=<sampledist>      sample distance in sec\n" );
		fprintf( stderr, "      -resid[=<slo,baz>]    prints redidual times\n" );
		fprintf( stderr, "      -loc=<lat,lon>        external location\n" );
		return 1;
	} /*endif*/
	strcpy( fname, pa_pvalue(1) );
	dt = 0.05;
	if  (pa_qspecified("-dt"))
		sscanf( pa_qvalue("-dt"), "%f", &dt );
	show_resid = pa_qspecified( "-resid" );
	newsloaz = NULL;
	if  (show_resid)  newsloaz = pa_qvalue( "-resid" );
	extloc = FALSE;
	if  (pa_qspecified("-loc"))  {
		extloc = TRUE;
		sscanf( pa_qvalue("-loc"), "%f,%f", &elat, &elon );
	} /*endif*/

	/* set station info file */
	env = (char *)getenv( "SH_INPUTS" );
	if  (env == NULL)  {
		fprintf( stderr, "%s: environment SH_INPUTS not set\n", pa_progname() );
		return 1;
	} /*endif*/
	strcpy( locfile, env );
	strcat( locfile, "/STATINF.DAT" );
	gl_locfile_name( locfile );

	/* open input file */
	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: cannot open input file %s\n", pa_progname(), fname);
		return 1;
	} /*endif*/

	/* read lines of file */
	arrid = 0;
	statno = 0;
	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		if  (*line == '!')  continue;
		if  (sscanf( line, "%s %f %f", station, &reltime, &corrcoeff ) != 3)  {
			fprintf( stderr, "%s: syntax error in file %s\n",
				pa_progname(), fname );
			fclose( fp );
			return 1;
		} /*endif*/
		if  (statno >= MAXSTATION)  {
			fprintf( stderr, "%s: too many stations\n", pa_progname() );
			fclose( fp );
			return 1;
		} /*endif*/
		gl_statinf( station, &statinf, &status );
		if  (SySevere(&status))  err_writemsg( status, station, TRUE );
		if  (!gl_valid_number(statinf.xrel) || !gl_valid_number(statinf.yrel))  {
			fprintf( stderr, "%s: no relative location for station %s\n",
				pa_progname(), station );
			fclose( fp );
			return 1;
		} /*endif*/
		if  (arrid == 0)  {
			arrid = statinf.arr_id;
		} else {
			if  (statinf.arr_id != arrid)  {
				fprintf( stderr, "%s: wrong array ID %d for station %s\n",
					pa_progname(), statinf.arr_id, station );
				fclose( fp );
				return 1;
			} /*endif*/
		} /*endif*/
		relonset[statno] = reltime;
		corr[statno] = corrcoeff;
		relx[statno] = statinf.xrel;
		rely[statno] = statinf.yrel;
		slat[statno] = statinf.lat;
		slon[statno] = statinf.lon;
		strcpy( scode[statno], station );
		statno++;
	} /*endwhile*/

	fclose( fp );

#ifdef XXX
	for  (i=0; i<statno; i++)  {
		printf( "%f %f %f\n", relx[i], rely[i], relonset[i] );
	} /*endfor*/
#endif

	mt_locate( statno, relonset, relx, rely, dt, &azim, &azimerr,
		&slo, &sloerr, &status );
	if  (SySevere(&status))  err_writemsg( status, "", TRUE );

	printf( "%f %f %f %f\n", slo, azim, sloerr, azimerr );

	if  (!show_resid)  return 0;

	/* read input slowness and azimuth if specified */
	if  (newsloaz != NULL)  {
		if  (sscanf(newsloaz,"%f,%f",&slo,&azim) != 2)  {
			fprintf( stderr, "could not read from %s\n", newsloaz );
			return 1;
		} /*endif*/
	} /*endif*/

	/* compute residuals */
	xslo = slo / DEG_TO_KM;
	xazim = azim / RAD_TO_DEG;
	sx = -xslo * sin( xazim );
	sy = -xslo * cos( xazim );

	meanth = meanobs = 0.0;
	for  (i=0; i<statno; i++)  {
		thonset[i] = sx*relx[i] + sy*rely[i];
		meanth += thonset[i];
		meanobs += relonset[i];
	} /*endfor*/
	meanth /= (float)statno;
	meanobs /= (float)statno;
	for  (i=0; i<statno; i++)  {
		thonset[i] -= meanth;
		relonset[i] -= meanobs;
	} /*endfor*/
	for  (i=0; i<statno; i++)  {
		if  (extloc)  {
			mb_locdiff( slat[i], slon[i], elat, elon, &d_dist, &d_az, &d_baz );
			printf( "%s %6.3f %6.3f %8.3f %4.2f\n",
				scode[i], relonset[i]-thonset[i], slo, (float)d_baz, corr[i] );
		} else {
			printf( "%s %6.3f %6.3f %8.3f %4.2f\n",
				scode[i], relonset[i]-thonset[i], slo, azim, corr[i] );
		} /*endif*/
	} /*endfor*/

	return 0;

} /* end of main */

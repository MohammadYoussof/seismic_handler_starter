
/* file evt_correct_aniso.c
 * ========================
 *
 * version 2, 3-Dec-2007
 *
 * Corrects Pn phase onset times for anisotropy effects
 * K. Stammler, 17-Jul-2006
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
#include <stdlib.h>
#include <math.h>
#include "basecnst.h"
#include "sysbase.h"
#include "tcusrdef.h"
#include "earthloc.h"
#include "glusrdef.h"
#include "cpar.h"
#include "eventdsc.h"
#include "timelist.h"

#define NUMLAYERS 4
#define DEFAULT_DEPTH 10.0


/* model parameters */
typedef struct {
	float      vmin;        /* minimum velocity (slow direction) */
	float      vmax;        /* maximum velocity (fast direction) */
	float      tet;         /* direction of fast velocity axis in deg */
	float      z;           /* layer thickness */
	float      v[NUMLAYERS];/* velocity layers */
} TCa_ModelPar;


/* prototypes of local routines */
void CorrectOnset( TCa_ModelPar *mp, EvEventT *ph, EvStatusT *status );


/* global variables */
float     cav_epilat=0.0;         /* epicentre latitude */
float     cav_epilon=0.0;         /* epicentre longitude */



int main( int argc, char *argv[] )
{
	/* local variables */
	char     inpfile[cBcFileLth+1];  /* name of input file */
	char     *inputs;                /* pointer to environment variables */
	char     str[cBcLineLth+1];      /* scratch string */
	TCa_ModelPar mp;                 /* model parameters */
	FILE     *fp;                    /* pointer to input file */
	EvEventT *event;                 /* pointer event (phase) info */
	BOOLEAN  eof;                    /* eof found */
	EvStatusT status;                /* return status */
	TLT_ELEMENT *elm;                /* time list element */
	TLT_ELEMENT *root;               /* time list root */

	/* executable code */

	status = cBcNoError;
	pa_init( argc, argv );

	if  (pa_pnumber() != 1 && pa_pnumber() != 9)  {
		fprintf( stderr, "Usage: %s <inpfile> [<vmin> <vmax> <tet> <z> <v1> <v2> <v3> <v4>]\n", pa_progname() );
		return 1;
	} /*endif*/
	strcpy( inpfile, pa_pvalue(1) );
	if  (pa_pnumber() == 9)  {
		if  (sscanf(pa_pvalue(2),"%f",&mp.vmin) != 1)  {
			fprintf( stderr, "%s: error reading vmin\n", pa_progname() );
			return 1;
		} /*endif*/
		if  (sscanf(pa_pvalue(3),"%f",&mp.vmax) != 1)  {
			fprintf( stderr, "%s: error reading vmax\n", pa_progname() );
			return 1;
		} /*endif*/
		if  (sscanf(pa_pvalue(4),"%f",&mp.tet) != 1)  {
			fprintf( stderr, "%s: error reading tet\n", pa_progname() );
			return 1;
		} /*endif*/
		if  (sscanf(pa_pvalue(5),"%f",&mp.z) != 1)  {
			fprintf( stderr, "%s: error reading z\n", pa_progname() );
			return 1;
		} /*endif*/
		if  (sscanf(pa_pvalue(6),"%f",&(mp.v[0])) != 1)  {
			fprintf( stderr, "%s: error reading v1\n", pa_progname() );
			return 1;
		} /*endif*/
		if  (sscanf(pa_pvalue(7),"%f",&(mp.v[1])) != 1)  {
			fprintf( stderr, "%s: error reading v2\n", pa_progname() );
			return 1;
		} /*endif*/
		if  (sscanf(pa_pvalue(8),"%f",&(mp.v[2])) != 1)  {
			fprintf( stderr, "%s: error reading v3\n", pa_progname() );
			return 1;
		} /*endif*/
		if  (sscanf(pa_pvalue(9),"%f",&(mp.v[3])) != 1)  {
			fprintf( stderr, "%s: error reading v4\n", pa_progname() );
			return 1;
		} /*endif*/
	} else {
		mp.vmin = 7.8;
		mp.vmax = 8.2;
		mp.tet = 28.0;
		mp.z = 10.0;
		mp.v[0] = 5.94;
		mp.v[1] = 6.06;
		mp.v[2] = 6.13;
		mp.v[3] = 8.0;
	} /*endif*/

	/* initialization of libraries */
	inputs = (char *)getenv( "SH_INPUTS" );
	if  (inputs == NULL)  {
		fprintf( stderr, "%s: environment SH_INPUTS not set\n", pa_progname() );
		return 1;
	} /*endif*/
	/* station location file */
	strcpy( str, inputs );
	strcat( str, "/" );
	strcat( str, "STATINF.DAT" );
	gl_locfile_name( str );

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
		if  (event->latitude > EvEMPTY_LATITUDE)  {
			cav_epilat = event->latitude;
			cav_epilon = event->longitude;
		} /*endif*/
		if  (Severe(&status))  return 1;
	} /*endfor*/

	sy_fclose( fp );

	if  (cav_epilat == 0.0 && cav_epilon == 0.0)  {
		fprintf( stderr, "%s: no epicentre found in %s\n", pa_progname(), inpfile );
		return 1;
	} /*endif*/

	elm = root = tl_get_root();
	do  {
		CorrectOnset( &mp, elm->info, &status );
		if  (Severe(&status))  return 1;
		EvPutEvent( stdout, elm->info, &status );
		if  (Severe(&status))  return 1;
		elm = elm->next;
	} while  (elm != root);

	return 0;

} /* end of main */



/*----------------------------------------------------------------------------*/



void CorrectOnset( TCa_ModelPar *mp, EvEventT *ph, EvStatusT *status )

/* Correct onset time of phase
 *
 * parameters of routine
 * EvEventT   *ph;      modify; change onset time
 * EvStatus   *status;  output; return status
 */
{
	/* local variables */
	GLT_STATINF statinf;                  /* station info */
	double   d_dist, d_azim, d_bazim;     /* epicentral distance, azimuths */
	float    tcorr;                       /* correction time */
	float    ttrav;                       /* travel time */
	float    depth;                       /* source depth in km */
	/* scratch variables for computation */
	float    vmax2, vmin2;                /* squared velocities */
	float    phi;                         /* azimuth of station */
	float    vani;                        /* anisotropic velocity */
	float    tmpx, tmpy;                  /* scratch values */
	float    z;                           /* layer thicknes */
	float    v1, v2, v3, v4;              /* layer velocities */
	float    x;                           /* distance in km */
	float    tl1, tl2, tl3, tl4, tani;
	float    cosconst, tpn, tiso, delta_t, delta_x;

	/* executable code */

	if  (strcmp(ph->phase,"Pn") != 0)  return;

	gl_statinf( ph->station, &statinf, status );
	if  (Severe(status))  return;
	mb_locdiff( statinf.lat, statinf.lon, (double)cav_epilat, (double)cav_epilon,
		&d_dist, &d_azim, &d_bazim );

	depth = (ph->depth == EvEMPTY_DEPTH) ? DEFAULT_DEPTH : ph->depth;
	tmpx = /*vc*/6.0 / /*vm*/8.0;
	tmpy = sqrt( 1 - tmpx*tmpx );
	delta_x = mp->z * (tmpx/tmpy);

	/*
	printf( "--> found phase Pn at %s, distance %f, azim %f, bazim %f\n",
		ph->station, (float)d_dist, (float)d_azim, (float)d_bazim );
	*/

	ttrav = 0.0; /* temporaere Variable */
	tcorr = 0.0;

	z = mp->z;
	v1 = mp->v[0];
	v2 = mp->v[1];
	v3 = mp->v[2];
	v4 = mp->v[3];

	vmax2=(mp->vmax)*(mp->vmax);
	vmin2=(mp->vmin)*(mp->vmin);

	phi = d_azim;  /* phi is azimuth of station */

	tmpx = (mp->vmin)*cos((phi-(mp->tet))*M_PI/180.);
	tmpy = (mp->vmax)*sin((phi-(mp->tet))*M_PI/180.);
	vani = (vmax2 * vmin2) / ( tmpx*tmpx + tmpy*tmpy );
	vani = sqrt( vani );
	/*
	 * compute pn travel time curve tiso(x) for mks2007 model 
	 * input: z (layer thickness), v1,..., v4 (=^ vani)
	 */
	x = (d_dist * 111.19) + delta_x; /* epicentral distance in km */

	tl1 = 2.0*z/v1;
	tmpx = v1 / v4;
	cosconst = sqrt( 1-(tmpx*tmpx) );
	tl1 = tl1*cosconst;
	tl2 = 2.0*z/v2;
	tmpx = v2 / v4;
	cosconst = sqrt( 1-(tmpx*tmpx) );
	tl2 = tl2*cosconst;
	tl3 = 2.0*z/v3;
	tmpx = v3 / v4;
	cosconst = sqrt( 1-(tmpx*tmpx) );
	tl3 = tl3*cosconst;
	tpn = x/v4;
	/* x-abhaengigkeit */
	tiso = tpn + tl1 + tl2 + tl3;

	/* now the same for tani (replace v4 by vani) */
	v4=vani;

	tl1 = 2.0*z/v1;
	tmpx = v1 / v4;
	cosconst = sqrt( 1-(tmpx*tmpx) );
	tl1 = tl1*cosconst;
	tl2 = 2.0*z/v2;
	tmpx = v2 / v4;
	cosconst = sqrt( 1-(tmpx*tmpx) );
	tl2 = tl2*cosconst;
	tl3 = 2.0*z/v3;
	tmpx = v3 / v4;
	cosconst = sqrt( 1-(tmpx*tmpx) );
	tl3 = tl3*cosconst;
	tani = (x/v4) + tl1 + tl2 + tl3;

	/* delta_t */
	delta_t = tiso-tani; /* delta_t has to be added to the observed pn travel-time */
	tc_tadd( ph->onset_time, delta_t, ph->onset_time, status );

	/*fprintf( stderr, "# tiso %f, tani %f, delta_t %f, distance %f, delta_x %f, azim %f\n",
		tiso, tani, delta_t, x, delta_x, phi );*/

} /* end of CorrectOnset */



/*----------------------------------------------------------------------------*/


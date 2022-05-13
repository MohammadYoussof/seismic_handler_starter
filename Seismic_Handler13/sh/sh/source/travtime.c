
/* file TRAVTIME.C
 *      ==========
 *
 * version 8, 22-May-2006
 *
 * routines for travel time computations in layered media
 * K. Stammler, 15-SEP-1990
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1992,  Klaus Stammler, Federal Institute for Geosciences
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
#include BC_SYSBASE
#include "numres.h"
#include "erusrdef.h"
#include "ttusrdef.h"
#include "tterrors.h"

typedef float VEL;

typedef struct {
	int       lth;      /* length of table */
	BOOLEAN   efa;      /* earth flattening transformation applied */
	float     *depth;   /* pointer to depth array */
	VEL       *pvel;    /* pointer to P velocity array */
	VEL       *svel;    /* pointer to S velocity array */
} MANTLE_VEL;

typedef float (*DIFFPROC)( float depth, float p, float s,
	float vp, float vs );



/* global constants */
#define TTC_MAGIC 135792468L
	/* magic number of velocity files */
#define TTC_EARTH_RADIUS 6378.2
	/* radius of earth in km */
#define TTC_DEPTH_STEPS 20
	/* maximum number of depth steps */
#define TTC_MAXDIST 104.0
	/* maximum distance in degrees !!! */
#define TTC_IP_ORDER 6
	/* interpolation order */



/* global variables */
static MANTLE_VEL    ttv_vel;                     /* velocity table */
static char          ttv_tablename[BC_FILELTH+1]; /* name of table file */
static float         ttv_delta_dist=1.0;          /* distance interval */



/* prototypes of local routines */
void tth_cart_step( float prev_phi, float d, float v, float next_v,
	float *phi, float *dt, float *dx );
static float tt_mantle_psdiff( float convdepth, float slowness, STATUS *status );
static float tt_mantle_p_ppp( float convdepth, float slowness, STATUS *status );
static float tt_mantle_x( float convdepth, float slowness, 
	DIFFPROC diffproc, STATUS *status );
static void tth_snellius( float *phi, float vin, float vout );
static float tth_p_ppp( float width, float phi, float v );
static float tth_p_pps( float width, float phi_p, float phi_s, float vp,
	float vs );
static float tth_psdiff( float width, float phi_p, float phi_s, float vp,
	float vs );
static float tth_ps_xdiff( float width, float phi_p, float phi_s,
	float vp, float vs );
int tth_get_comment_lines( int phase, STATUS *status );
void tth_get_depth_steps( float depth[] );
void tth_readline( FILE *tf, float *dist, int tt[], STATUS *status );



/*---------------------------------------------------------------------*/



void tt_read_mantle_vel( char velfile[], STATUS *status )

/* reads velocity distribution
 *
 * parameters of routine
 * char       velfile[];      input; name of velocity file
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	FILE     *vf;                  /* velocity file pointer */
	char     line[BC_LINELTH+1];   /* current read line */
	long     magic;                /* magic number of velocity file */
	int      i;                    /* counter */

	/* executable code */

	vf = sy_fopen( velfile, "r" );
	if  (vf == NULL)  {
		*status = TTE_FOPNRD;
		err_setcontext( " ## file " ); err_setcontext( velfile );
		return;
	} /*endif*/

	/* read off comment lines */
	do  {
		if  (fgets(line,BC_LINELTH,vf) == NULL)  {
			*status = TTE_FREAD;
			return;
		} /*endif*/
	}  while  (*line == '!');

	/* check magic number and EFA */
	if  (sscanf(line,"%ld,%d",&magic,&(ttv_vel.efa)) != 2)  {
		*status = TTE_NOVELFIL;
		return;
	} else if  (magic != TTC_MAGIC)  {
		*status = TTE_NOVELFIL;
		return;
	} /*endif*/

	/* read length of velocity table */
	if  (fscanf(vf,"%d\n",&(ttv_vel.lth)) != 1)  {
		*status = TTE_FREAD;
		return;
	} /*endif*/

	/* free memory, if already read in */
	if  (ttv_vel.depth != NULL)  sy_deallocmem( ttv_vel.depth );
	if  (ttv_vel.pvel != NULL)  sy_deallocmem( ttv_vel.pvel );
	if  (ttv_vel.svel != NULL)  sy_deallocmem( ttv_vel.svel );

	/* allocate memory */
	ttv_vel.depth = (float *)sy_allocmem( (long)ttv_vel.lth+1,
		(int)sizeof(float), status );
	if  (Severe(status))  return;
	ttv_vel.pvel = (VEL *)sy_allocmem( (long)ttv_vel.lth,
		(int)sizeof(VEL), status );
	if  (Severe(status))  {
		sy_deallocmem( ttv_vel.depth );
		return;
	} /*endif*/
	ttv_vel.svel = (VEL *)sy_allocmem( (long)ttv_vel.lth,
		(int)sizeof(VEL), status );
	if  (Severe(status))  {
		sy_deallocmem( ttv_vel.depth );
		sy_deallocmem( ttv_vel.pvel );
		return;
	} /*endif*/

	/* read velocities */
	ttv_vel.depth[0] = 0.0;
	for  (i=0; i<ttv_vel.lth; i++)
		if  (fscanf( vf, "%f %f %f\n", ttv_vel.depth+i+1,
			ttv_vel.pvel+i, ttv_vel.svel+i ) != 3)  {
			*status = TTE_FREAD;
			return;
		} /*endif*/

	fclose( vf );

} /* end of tt_read_mantle_vel */



/*---------------------------------------------------------------------*/



void tt_change_mantle_vel( BOOLEAN change_p, float maxdepth, float fac,
	STATUS *status )

/* multiplies either all P-velocities (change_p=TRUE) or all
 * S-velocities (change_p=FALSE) with a constant factor "fac"
 *
 * parameters of routine
 * BOOLEAN    change_p;    input; change P-velocities (TRUE) or S (FALSE)
 * float      maxdepth;    input; maximum depth to be changed
 * float      fac;         input; multiplication factor
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	/* check wether velocities are already read */
	if  (ttv_vel.pvel == NULL)  {
		*status = TTE_NOTREADIN;
		return;
	} /*endif*/

	if  (ttv_vel.efa)
		maxdepth = -TTC_EARTH_RADIUS
			* log((TTC_EARTH_RADIUS-maxdepth)/TTC_EARTH_RADIUS);

	/* change them */
	if  (change_p)  {
		for  (i=0; i<ttv_vel.lth; i++)
			if  (ttv_vel.depth[i] <= maxdepth)
				ttv_vel.pvel[i] *= fac;
	} else {
		for  (i=0; i<ttv_vel.lth; i++)
			if  (ttv_vel.depth[i] <= maxdepth)
				ttv_vel.svel[i] *= fac;
	} /*endif*/

} /* end of tt_change_mantle_vel */



/*---------------------------------------------------------------------*/



float tt_cart_psdiff( float convdepth, float slowness, STATUS *status )

/* computes travel time difference of P and S through velocity
 * layers from "convdepth" km to surface.
 *
 * parameters of routine
 * float      convdepth;     input; conversion depth (in km)
 * float      slowness;      input; slowness of wave (in sec/degree)
 * STATUS     *status;       output; return status
 *                           returns travel time difference (in sec)
 */
{
	/* local variables */
	float    dt;          /* travel time step (sec) */
	float    tp, ts;      /* total travel times of P and S */
	float    p_phi_in;    /* sine of P input angle */
	float    p_phi_out;   /* sine of P output angle */
	float    s_phi_in;    /* sine of S input angle */
	float    s_phi_out;   /* sine of S output angle */
	int      n;           /* conversion depth in velocity table steps */
	int      i;           /* counter */
	float    dx_p, dx_s;  /* x-propagation of P and S */
	float    dx;          /* scratch */
	float    start_p_phi; /* sine of start P angle */
	float    cnvdpth;     /* EFA transformed conversion depth */
	float    vp, vs;      /* back-transformed velocities of P and S */
	float    width;       /* width of layer */

	/* executable code */

	if  (Severe(status))  return 0.0;  /* check whether slowness was found */
	if  (ttv_vel.pvel == NULL)  {
		*status = TTE_NOTREADIN;
		return 0.0;
	} /*endif*/

	cnvdpth = (ttv_vel.efa) ?
		-TTC_EARTH_RADIUS *
			log((TTC_EARTH_RADIUS-convdepth)/TTC_EARTH_RADIUS) :
		convdepth;

	/* find depth step */
	n = ttv_vel.lth - 1;
	while  (ttv_vel.depth[n] >= cnvdpth)
		if  (--n < 0)  return 0.0;
	if  (n == (ttv_vel.lth-1))  {
		*status = TTE_DEEPCNV;
		return 0.0;
	} /*endif*/

	/* transform velocities back for angle computation */
	if  (ttv_vel.efa)  {
		dx = (TTC_EARTH_RADIUS-convdepth) / TTC_EARTH_RADIUS;
		vp = ttv_vel.pvel[n] * dx;
		vs = ttv_vel.svel[n] * dx;
	} else {
		vp = ttv_vel.pvel[n];
		vs = ttv_vel.svel[n];
	} /*endif*/

	/* initialisation, travel partly through lowermost layer n */
	start_p_phi = slowness*180.0/BC_PI * vp/(TTC_EARTH_RADIUS-convdepth);
	if  (start_p_phi > 1.0)  {
		*status = TTE_ILANGLE;
		return 0.0;
	} /*endif*/
	p_phi_in = start_p_phi;
	tth_cart_step( start_p_phi, cnvdpth-ttv_vel.depth[n],
		ttv_vel.pvel[n], ttv_vel.pvel[n], &p_phi_out, &tp, &dx_p );
	p_phi_in = p_phi_out;
	s_phi_in = slowness*180.0/BC_PI * vs/(TTC_EARTH_RADIUS-convdepth);
	tth_cart_step( s_phi_in, cnvdpth-ttv_vel.depth[n],
		ttv_vel.svel[n], ttv_vel.svel[n], &s_phi_out, &ts, &dx_s );
	s_phi_in = s_phi_out;

	/* travel to surface */
	for  (i=n-1; i>=0; i--)  {
		width = ttv_vel.depth[i+1] - ttv_vel.depth[i];
		tth_cart_step( p_phi_in, width, ttv_vel.pvel[i],
			ttv_vel.pvel[i+1], &p_phi_out, &dt, &dx );
		tp += dt;
		dx_p += dx;
		p_phi_in = p_phi_out;
		tth_cart_step( s_phi_in, width, ttv_vel.svel[i],
			ttv_vel.svel[i+1], &s_phi_out, &dt, &dx );
		ts += dt;
		dx_s += dx;
		s_phi_in = s_phi_out;
	} /*endfor*/

	/* compute time difference */
	dx = (dx_p-dx_s) * start_p_phi;
	dt = dx / ttv_vel.pvel[n];
	return (ts - tp + dt);

} /* end of tt_cart_psdiff */



/*---------------------------------------------------------------------*/



void tth_cart_step( float low_phi, float width, float v, float low_v,
	float *phi, float *dt, float *dx )

/* computes travel time, new incidence angle & x-propagation of a
 * wave travelling through a single velocity layer (cartesian coordinates)
 *
 * parameters of routine
 * float      low_phi;     input; sine of inc. angle in next lower layer
 * float      width;       input; width of layer in km
 * float      v;           input; velocity in layer
 * float      low_v;       input; velocity in next lower layer
 * float      *phi;        output; sine of incidence angle in current layer
 * float      *dt;         output; travel time through current layer
 * float      *dx;         output; x-propagation in current layer
 */
{
	/* local variables */
	float    r;        /* total travel distance through layer */

	/* executable code */

	*phi = v/low_v * low_phi;
	r = width / sqrt(1-(*phi)*(*phi));
	*dt = r / v;
	*dx = r * (*phi);

} /* end of tth_cart_step */



/*---------------------------------------------------------------------*/



float tt_mantle_diff( int diff_phase, float convdepth, float slowness,
	STATUS *status )

/* computes travel time differences between different phases
 *
 * parameters of routine
 * int        diff_phase;     input; phase difference ID
 * float      convdepth;      input; conversion depth in km
 * float      slowness;       input; slowness in sec/deg
 * STATUS     *status;        output; return status
 *                            returns travel time difference in sec
 */
{
	/* executable code */

	switch  (diff_phase)  {
	case TTC_P_S_DIFF:
		return tt_mantle_psdiff(convdepth,slowness,status);
		/* or tt_mantle_x(convdepth,slowness,tth_psdiff,status); */
	case TTC_P_Ppp_DIFF:
		return tt_mantle_p_ppp(convdepth,slowness,status);
	case TTC_P_Pps_DIFF:
		return tt_mantle_x(convdepth,slowness,tth_p_pps,status);
	case TTC_P_S_xDIFF:
		return tt_mantle_x(convdepth,slowness,tth_ps_xdiff,status);
	default:
		*status = TTE_UK_DIFF_PHASE;
		return 0.0;
	} /*endswitch*/

} /* end of tt_mantle_diff */



/*----------------------------------------------------------------------------*/



static float tt_mantle_psdiff( float convdepth, float slowness, STATUS *status )

/* computes travel time difference of P and S through velocity
 * layers from "convdepth" km to surface. (new version)
 *
 * parameters of routine
 * float      convdepth;     input; conversion depth (in km)
 * float      slowness;      input; slowness of wave (in sec/degree)
 * STATUS     *status;       output; return status
 *                           returns travel time difference (in sec)
 */
{
	/* local variables */
	float    dt;          /* travel time step (sec) */
	float    phi_p;       /* sine of P angle of incidence */
	float    phi_s;       /* sine of S angle of incidence */
	float    tmp, vp, vs; /* scratch for computation of start angles */
	int      n;           /* conversion depth in velocity table steps */
	int      i;           /* counter */
	float    cnvdpth;     /* EFA transformed conversion depth */

	/* executable code */

	if  (Severe(status))  return 0.0;  /* check whether slowness was found */
	if  (ttv_vel.pvel == NULL)  {
		*status = TTE_NOTREADIN;
		return 0.0;
	} /*endif*/

	cnvdpth = (ttv_vel.efa) ?
		-TTC_EARTH_RADIUS *
			log((TTC_EARTH_RADIUS-convdepth)/TTC_EARTH_RADIUS) :
		convdepth;

	/* find depth step */
	n = ttv_vel.lth - 1;
	while  (ttv_vel.depth[n] >= cnvdpth)
		if  (--n < 0)  return 0.0;
	if  (n == (ttv_vel.lth-1))  {
		*status = TTE_DEEPCNV;
		return 0.0;
	} /*endif*/

	/* transform velocities back for angle computation */
	if  (ttv_vel.efa)  {
		tmp = (TTC_EARTH_RADIUS-convdepth) / TTC_EARTH_RADIUS;
		vp = ttv_vel.pvel[n] * tmp;
		vs = ttv_vel.svel[n] * tmp;
	} else {
		vp = ttv_vel.pvel[n];
		vs = ttv_vel.svel[n];
	} /*endif*/

	/* initialisation, travel partly through lowermost layer n */
	phi_p = slowness*180.0/BC_PI * vp/(TTC_EARTH_RADIUS-convdepth);
	if  (phi_p > 1.0)  {
		*status = TTE_ILANGLE;
		return 0.0;
	} /*endif*/
	phi_s = slowness*180.0/BC_PI * vs/(TTC_EARTH_RADIUS-convdepth);

	/* loop over layers */
	dt = tth_psdiff( cnvdpth-ttv_vel.depth[n], phi_p, phi_s,
		ttv_vel.pvel[n], ttv_vel.svel[n] );
	for  (i=n; i>0; i--)  {
		tth_snellius( &phi_p, ttv_vel.pvel[i], ttv_vel.pvel[i-1] );
		tth_snellius( &phi_s, ttv_vel.svel[i], ttv_vel.svel[i-1] );
		dt += tth_psdiff( ttv_vel.depth[i]-ttv_vel.depth[i-1], phi_p, phi_s,
			ttv_vel.pvel[i-1], ttv_vel.svel[i-1] );
	} /*endfor*/

	return dt;

} /* end of tt_mantle_psdiff */



/*----------------------------------------------------------------------------*/



static float tt_mantle_x( float convdepth, float slowness,
	DIFFPROC diffproc, STATUS *status )

/* computes travel time difference of P and Pps through velocity
 * layers from "convdepth" km to surface. (new version)
 *
 * parameters of routine
 * float      convdepth;     input; conversion depth (in km)
 * float      slowness;      input; slowness of wave (in sec/degree)
 * STATUS     *status;       output; return status
 *                           returns travel time difference (in sec)
 */
{
	/* local variables */
	float    dt;          /* travel time step (sec) */
	float    phi_p;       /* sine of P angle of incidence */
	float    phi_s;       /* sine of S angle of incidence */
	float    tmp, vp, vs; /* scratch for computation of start angles */
	int      n;           /* conversion depth in velocity table steps */
	int      i;           /* counter */
	float    cnvdpth;     /* EFA transformed conversion depth */

	/* executable code */

	if  (Severe(status))  return 0.0;  /* check whether slowness was found */
	if  (ttv_vel.pvel == NULL)  {
		*status = TTE_NOTREADIN;
		return 0.0;
	} /*endif*/

	cnvdpth = (ttv_vel.efa) ?
		-TTC_EARTH_RADIUS *
			log((TTC_EARTH_RADIUS-convdepth)/TTC_EARTH_RADIUS) :
		convdepth;

	/* find depth step */
	n = ttv_vel.lth - 1;
	while  (ttv_vel.depth[n] >= cnvdpth)
		if  (--n < 0)  return 0.0;
	if  (n == (ttv_vel.lth-1))  {
		*status = TTE_DEEPCNV;
		return 0.0;
	} /*endif*/

	/* transform velocities back for angle computation */
	if  (ttv_vel.efa)  {
		tmp = (TTC_EARTH_RADIUS-convdepth) / TTC_EARTH_RADIUS;
		vp = ttv_vel.pvel[n] * tmp;
		vs = ttv_vel.svel[n] * tmp;
	} else {
		vp = ttv_vel.pvel[n];
		vs = ttv_vel.svel[n];
	} /*endif*/

	/* initialisation, travel partly through lowermost layer n */
	phi_p = slowness*180.0/BC_PI * vp/(TTC_EARTH_RADIUS-convdepth);
	if  (phi_p > 1.0)  {
		*status = TTE_ILANGLE;
		return 0.0;
	} /*endif*/
	phi_s = slowness*180.0/BC_PI * vs/(TTC_EARTH_RADIUS-convdepth);

	/* loop over layers */
	dt = (*diffproc)( cnvdpth-ttv_vel.depth[n], phi_p, phi_s,
		ttv_vel.pvel[n], ttv_vel.svel[n] );
	for  (i=n; i>0; i--)  {
		tth_snellius( &phi_p, ttv_vel.pvel[i], ttv_vel.pvel[i-1] );
		tth_snellius( &phi_s, ttv_vel.svel[i], ttv_vel.svel[i-1] );
		dt += (*diffproc)( ttv_vel.depth[i]-ttv_vel.depth[i-1], phi_p,
			phi_s, ttv_vel.pvel[i-1], ttv_vel.svel[i-1] );
	} /*endfor*/

	return dt;

} /* end of tt_mantle_x */



/*---------------------------------------------------------------------*/



static float tt_mantle_p_ppp( float convdepth, float slowness, STATUS *status )

/* computes travel time difference of P and Ppp through velocity
 * layers from "convdepth" km to surface.
 *
 * parameters of routine
 * float      convdepth;     input; conversion depth (in km)
 * float      slowness;      input; slowness of wave (in sec/degree)
 * STATUS     *status;       output; return status
 *                           returns travel time difference (in sec)
 */
{
	/* local variables */
	float    dt;          /* travel time step (sec) */
	float    phi_p;       /* sine of P angle of incidence */
	float    tmp, vp;     /* scratch for computation of start angles */
	int      n;           /* conversion depth in velocity table steps */
	int      i;           /* counter */
	float    cnvdpth;     /* EFA transformed conversion depth */

	/* executable code */

	if  (Severe(status))  return 0.0;  /* check whether slowness was found */
	if  (ttv_vel.pvel == NULL)  {
		*status = TTE_NOTREADIN;
		return 0.0;
	} /*endif*/

	cnvdpth = (ttv_vel.efa) ?
		-TTC_EARTH_RADIUS *
			log((TTC_EARTH_RADIUS-convdepth)/TTC_EARTH_RADIUS) :
		convdepth;

	/* find depth step */
	n = ttv_vel.lth - 1;
	while  (ttv_vel.depth[n] >= cnvdpth)
		if  (--n < 0)  return 0.0;
	if  (n == (ttv_vel.lth-1))  {
		*status = TTE_DEEPCNV;
		return 0.0;
	} /*endif*/

	/* transform velocities back for angle computation */
	if  (ttv_vel.efa)  {
		tmp = (TTC_EARTH_RADIUS-convdepth) / TTC_EARTH_RADIUS;
		vp = ttv_vel.pvel[n] * tmp;
	} else {
		vp = ttv_vel.pvel[n];
	} /*endif*/

	/* initialisation, travel partly through lowermost layer n */
	phi_p = slowness*180.0/BC_PI * vp/(TTC_EARTH_RADIUS-convdepth);
	if  (phi_p > 1.0)  {
		*status = TTE_ILANGLE;
		return 0.0;
	} /*endif*/

	/* loop over layers */
	dt = tth_p_ppp( cnvdpth-ttv_vel.depth[n], phi_p, ttv_vel.pvel[n] );
	for  (i=n; i>0; i--)  {
		tth_snellius( &phi_p, ttv_vel.pvel[i], ttv_vel.pvel[i-1] );
		dt += tth_p_ppp( ttv_vel.depth[i]-ttv_vel.depth[i-1], phi_p,
			ttv_vel.pvel[i-1] );
	} /*endfor*/

	return dt;

} /* end of tt_mantle_p_ppp */



/*----------------------------------------------------------------------------*/



static void tth_snellius( float *phi, float vin, float vout )

/* computes new angle *phi
 *
 * parameters of routine
 * float      *phi;    modify; input: sine of old angle; output: new
 * float      vin;     input; current velocity
 * float      vout;    input; new velocity
 */
{
	/* executable code */

	*phi *= vout/vin;

} /* end of tth_snellius */



/*----------------------------------------------------------------------------*/



static float tth_psdiff( float width, float phi_p, float phi_s, float vp,
	float vs )

/* returns time delay in one layer
 *
 * parameters of routine
 * float      width;     input; width of layer in km
 * float      phi_p;     input; sine of P angle
 * float      phi_s;     input; sine of S angle
 * float      vp;        input; P velocity
 * float      vs;        input; S velocity
 */
{
	/* local variables */
	float    cp, cs;    /* cosines of angles */

	/* executable code */

	cp = sqrt( 1 - phi_p*phi_p );
	cs = sqrt( 1 - phi_s*phi_s );
	return (width * (cs/vs - cp/vp));

} /* end of tth_psdiff */



/*----------------------------------------------------------------------------*/



static float tth_p_pps( float width, float phi_p, float phi_s, float vp,
	float vs )

/* returns time delay in one layer
 *
 * parameters of routine
 * float      width;     input; width of layer in km
 * float      phi_p;     input; sine of P angle
 * float      phi_s;     input; sine of S angle
 * float      vp;        input; P velocity
 * float      vs;        input; S velocity
 */
{
	/* local variables */
	float    cp, cs, ts;    /* cosines of angles */

	/* executable code */

	cp = sqrt( 1 - phi_p*phi_p );
	cs = sqrt( 1 - phi_s*phi_s );
	ts = phi_s/cs;  /* tangens */
	return (width * (cp-phi_p*ts/vp + 1.0/(vs*cs)));

} /* end of tth_p_pps */



/*----------------------------------------------------------------------------*/



static float tth_p_ppp( float width, float phi, float v )

/* returns travel time in one layer
 *
 * parameters of routine
 * float      width;     input; width of layer in km
 * float      phi;       input; sine of angle
 * float      v;         input; velocity
 */
{
	/* local variables */
	float    c;    /* cosine of angle */

	/* executable code */

	c = sqrt( 1 - phi*phi );
	return (2.0*width/v * c);

} /* end of tth_p_ppp */



/*----------------------------------------------------------------------------*/



static float tth_ps_xdiff( float width, float phi_p, float phi_s,
	float vp, float vs )

/* returns difference in x-propagation between P and S-wave
 *
 * parameters of routine
 * float      width;     input; width of layer in km
 * float      phi_p;     input; sine of P angle
 * float      phi_s;     input; sine of S angle
 * float      vp;        input; P velocity
 * float      vs;        input; S velocity
 */
{
	/* local variables */
	float    tp, ts;    /* tangens of angles */
	float    tmp;

	/* executable code */

	if  (vp || vs)  {
		/* just to touch vp and vs */
	} /*endif*/

	tmp = phi_p * phi_p;
	tp = sqrt( tmp / (1.0-tmp) );
	tmp = phi_s * phi_s;
	ts = sqrt( tmp / (1.0-tmp) );
	return (width * (tp-ts));

} /* end of tth_ps_xdiff */



/*---------------------------------------------------------------------
 *
 * schematic figure of layers
 *
 *   (surface) d[0]  ------------------------------------------------
 *                       w[0], a[0], v[0]         layer 0
 *             d[1]  ------------------------------------------------
 *                       w[1], a[1], v[1]         layer 1
 *             d[2]  ------------------------------------------------
 *                       w[2], a[2], v[2]         layer 2
 *             d[3]  ------------------------------------------------
 *                              :
 *                              :
 *           d[n-1]  ------------------------------------------------
 *                       w[n-1], a[n-1], v[n-1]   layer n-1
 *             d[n]  ------------------------------------------------
 *
 *                           X <-- conversion depth d, a[n], v[n]
 *
 *
 *   d[i]    layer boundaries
 *   w[i]    width of layer i
 *   a[i]    angle of incidence of wave in layer i
 *   v[i]    velocity in layer i
 *   r[i]    total travel distance in layer i
 *   x[i]    x-propagation in layer i
 *   t[i]    total travel time in layer i
 *   d       conversion depth
 *   x       total x propagation,  x = sum_{x[i],i=0,..,n}
 *   t       total travel time,  t = sum_{t[i],i=0,..,n}
 *
 *   given: d[i] (i=0,..,n), v[i] (i=0,..,n-1), a[n] (by slowness)
 *   wanted: travel time difference between P and S, dt
 *
 *   sin(a[i])     sin(a[i+1])              \
 *   ---------  =  -----------   (Snellius) |
 *      v[i]          v[i+1]                |
 *                                          |
 *   w[i] = d[i+1] - d[i]  (d[n+1] := d)     >  i = n, n-1, ..., 0
 *   r[i] = w[i] / cos(a[i])                |   (for P and S)
 *   x[i] = r[i] * sin(a[i])                |
 *   t[i] = r[i] / v[i]                     |
 *                                          /
 *
 *   additional ray path for the P wave which will be converted to
 *   S at the discontinuity (velocity vp[n]):
 *      l = dx * sin(a[n])
 *      dx = xp - xs   (difference of total x propagations of P and S)
 *      tl = l / vp[n] (travel time for additional ray path)
 *
 *   dt = ts - tp + tl
 *-------------------------------------------------------------------*/



float tt_travel( int phase, float distance, float depth,
	STATUS *status )

/* returns travel time in seconds for different phases
 *
 * parameters of routine
 * int        phase;       input; phase ID
 * float      distance;    input; distance of source in degrees
 * float      depth;       input; depth of source in km
 * STATUS     *status;     output; return status
 *                         returns travel time of phase in sec
 */
{
	/* local variables */
	FILE     *tf;                   /* table file */
	int      cmtlines;              /* number of comment lines */
	float    dep[TTC_DEPTH_STEPS];  /* depths given in table */
	int      tt_lo[TTC_DEPTH_STEPS]; /* travel times of closest distance */
	int      tt_hi[TTC_DEPTH_STEPS]; /* travel times of closest distance */
	int      i;                      /* counter */
	char     str[BC_LINELTH+1];      /* scratch */
	float    currdist;               /* current distance */
	float    prevdist;               /* previous distance read */
	int      depidx;                 /* depth index */
	float    deriv_dep;              /* depth derivation */
	float    deriv_dist;             /* distance derivation */
	float    delta_dep;              /* depth interpolation step */
	float    delta_dist;             /* distance interpolation step */
	float    ttime;                  /* travel time returned */

	/* executable code */

	cmtlines = tth_get_comment_lines( phase, status );
	if  (Severe(status))  return 0.0;

	tth_get_depth_steps( dep );

	/* check bounds */
	if  ((depth < *dep) || (depth > dep[TTC_DEPTH_STEPS-1]))  {
		*status = TTE_DEPTHOOR;
		return 0.0;
	} /*endif*/
	if  ((distance < 0.0) || (distance > TTC_MAXDIST))  {
		*status = TTE_DISTOOR;
		return 0.0;
	} /*endif*/

	/* open table file */
	tf = sy_fopen( ttv_tablename, "r" );
	if  (tf == NULL)  {
		*status = TTE_FOPNRD;
		err_setcontext( " ## file " ); err_setcontext( ttv_tablename );
		return 0.0;
	} /*endif*/

	/* skip comment lines */
	for  (i=0; i<cmtlines; i++)
		if  (fgets(str,BC_LINELTH,tf) == NULL)  {
			*status = TTE_FREAD;
			return 0.0;
		} /*endif*/

	tth_readline( tf, &prevdist, tt_lo, status );
	if  (Severe(status))  return 0.0;
	FOREVER  {
		tth_readline( tf, &currdist, tt_hi, status );
		if  (Severe(status))  return 0.0;
		if  (currdist > distance)  break;
		for  (i=0; i<TTC_DEPTH_STEPS; i++)
			tt_lo[i] = tt_hi[i];
		prevdist = currdist;
	} /*endfor*/

	fclose( tf );

	depidx = 1;
	while  (dep[depidx] < depth)
		depidx++;

	/* linear interpolation in two dimensions */
	/* gradient of travel time */
	deriv_dep = ((float)tt_lo[depidx] - (float)tt_lo[depidx-1]) /
		(dep[depidx] - dep[depidx-1]);
	deriv_dist = ((float)tt_hi[depidx-1] - (float)tt_lo[depidx-1]) /
		(currdist - prevdist);
	/* difference vector */
	delta_dep = depth - dep[depidx-1];
	delta_dist = distance - prevdist;
	/* Taylor of 1. order */
	ttime = (float)tt_lo[depidx-1] + deriv_dep*delta_dep +
		deriv_dist*delta_dist;

	ttv_delta_dist = currdist - prevdist;  /* store distance step */

	return (ttime / 10.0);  /* values in table are tenths of seconds */

} /* end of tt_travel */



/*-------------------------------------------------------------------*/

#ifdef XXX

float tt_travel( int phase, float distance, float depth,
	STATUS *status )

/* returns travel time in seconds for different phases
 *
 * parameters of routine
 * int        phase;       input; phase ID
 * float      distance;    input; distance of source in degrees
 * float      depth;       input; depth of source in km
 * STATUS     *status;     output; return status
 *                         returns travel time of phase in sec
 */
{
	/* local variables */
	FILE     *tf;                    /* table file */
	int      cmtlines;               /* number of comment lines */
	float    dep[TTC_DEPTH_STEPS];   /* depths given in table */
	int      tt_ln[TTC_DEPTH_STEPS]; /* travel times of current line */
	float    tt_ip[TTC_IP_ORDER];    /* tt interpolation array */
	float    dst_ip[TTC_IP_ORDER];   /* distance interpolation array */
	int      i;                      /* counter */
	char     str[BC_LINELTH+1];      /* scratch */
	float    currdist;               /* current distance */
	int      depidx;                 /* depth index */
	int      dstidx;                 /* distance index to compare */
	float    depthstep;              /* depth step size */
	float    ttime;                  /* travel time returned */
	float    err;                    /* interpolation deviation */
	STATUS   locstat=BC_NOERROR;     /* local status */

	/* executable code */

	cmtlines = tth_get_comment_lines( phase, status );
	if  (Severe(status))  return 0.0;

	tth_get_depth_steps( dep );

	/* check bounds */
	if  ((depth < *dep) || (depth > dep[TTC_DEPTH_STEPS-1]))  {
		*status = TTE_DEPTHOOR;
		return 0.0;
	} /*endif*/
	if  ((distance < 0.0) || (distance > TTC_MAXDIST))  {
		*status = TTE_DISTOOR;
		return 0.0;
	} /*endif*/

	/* open table file */
	tf = sy_fopen( ttv_tablename, "r" );
	if  (tf == NULL)  {
		*status = TTE_FOPNRD;
		err_setcontext( " ## file " ); err_setcontext( ttv_tablename );
		return 0.0;
	} /*endif*/

	/* skip comment lines */
	for  (i=0; i<cmtlines; i++)
		if  (fgets(str,BC_LINELTH,tf) == NULL)  {
			*status = TTE_FREAD;
			return 0.0;
		} /*endif*/

	/* find depth index */
	depidx = 1;
	while  (dep[depidx] < depth)
		depidx++;
	depthstep = dep[depidx] - dep[depidx-1];
	dstidx = TTC_IP_ORDER/2;

	/* read at least TTC_IP_ORDER depths */
	for  (i=0; i<TTC_IP_ORDER; i++)  {
		tth_readline( tf, dst_ip+i, tt_ln, status );
		/* depth interpolation */
		tt_ip[i] = tt_ln[depidx-1] + (dep[depidx-1]-depth)
			* (tt_ln[depidx]-tt_ln[depidx-1]) / depthstep;
		if  (Severe(status))  return 0.0;
	} /*endfor*/

	FOREVER  {
		if  (dst_ip[dstidx] > distance)  break;
		tth_readline( tf, &currdist, tt_ln, &locstat );
		if  (Severe(&locstat))  break;
		/* shift arrays */
		for  (i=0; i<TTC_IP_ORDER-1; i++)  {
			tt_ip[i] = tt_ip[i+1];
			dst_ip[i] = dst_ip[i+1];
		} /*endfor*/
		dst_ip[TTC_IP_ORDER-1] = currdist;
		tt_ip[TTC_IP_ORDER-1] = (float)(tt_ln[depidx-1]) + (dep[depidx-1]-depth)
			* (float)(tt_ln[depidx]-tt_ln[depidx-1]) / depthstep;
	} /*endfor*/

	fclose( tf );

	/* polynomial interpolation */
	nr_polint( dst_ip, tt_ip, TTC_IP_ORDER, distance, &ttime, &err );

	ttv_delta_dist = dst_ip[dstidx] - dst_ip[dstidx-1]; /* store distance step */

	return (ttime / 10.0);  /* values in table are tenths of seconds */

} /* end of tt_travel */

#endif /* XXX */

/*-------------------------------------------------------------------*/



float tt_slowness( int phase, float distance, float depth,
	STATUS *status )

/* returns slowness of "phase"
 *
 * parameters of routine
 * int        phase;     input; phase ID
 * float      distance;  input; distance of source in degrees
 * float      depth;     input; depth of source in km
 * STATUS     *status;   output; return status
 *                       returns slowness of phase in sec/degree
 */
{
	/* local variables */
	float    lo_dist;          /* lower distance */
	float    curr_delta_dist;  /* distance steps at beginning of routine */

	/* executable code */

	curr_delta_dist = ttv_delta_dist;
	lo_dist = (distance < (curr_delta_dist/2.0)) ? 0.0 :
		distance - curr_delta_dist/2.0;

	return  ((tt_travel(phase,lo_dist+curr_delta_dist,depth,status) -
		tt_travel(phase,lo_dist,depth,status)) / curr_delta_dist);
	

} /* end of tt_slowness */



/*-------------------------------------------------------------------*/



void tt_settable( char tablename[], STATUS *status )

/* sets new table name
 *
 * parameters of routine
 * char       tablename[];   input; new file name of tt table
 * STATUS     *status;       output; return status
 */
{
	/* executable code */

	if  (strlen(tablename) > BC_FILELTH)  {
		*status = TTE_STROVFL;
		return;
	} /*endif*/
	strcpy( ttv_tablename, tablename );

} /* end of tt_settable */



/*-------------------------------------------------------------------*/



int tth_get_comment_lines( int phase, STATUS *status )

/* returns number of lines to be skipped for specified phase
 *
 * parameters of routine
 * int        phase;      input; phase ID
 * STATUS     *status;    output; return status
 *                        returns skip lines
 */
{
	/* executable code */

	switch  (phase)  {
	case TTC_PHASE_P:   return 100;
	case TTC_PHASE_S:   return 218;
	case TTC_PHASE_pP:  return 336;
	case TTC_PHASE_sP:  return 454;
	case TTC_PHASE_pS:  return 572;
	case TTC_PHASE_sS:  return 690;
	default:
		*status = TTE_UKPHASE;
		return 0.0;
	} /*endswitch*/

} /* end of tth_get_comment_lines */




/*-------------------------------------------------------------------*/



void tth_get_depth_steps( float depth[] )

/* sets the depth steps of the travel time table
 *
 * parameter of routine
 * float     depth[];      output; depth array
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	*depth = 0.0;
	for  (i=1; i<TTC_DEPTH_STEPS; i++)   /* !!! */
		depth[i] = ((float)(i-1)/100.0 + 0.005) * TTC_EARTH_RADIUS;

} /* end of tth_get_depth_steps */



/*-------------------------------------------------------------------*/



void tth_readline( FILE *tf, float *dist, int tt[], STATUS *status )

/* reads one line of tt table
 *
 * parameters of routine
 * FILE       *tf;      input; table file pointer
 * float      *dist;    output; distance read
 * int        tt[];     output; travel times read
 * STATUS     *status;  output; return status
 */
{
	/* local variables */
	int      i;                     /* counter */
	char     scratch[BC_LINELTH+1]; /* dummy string */

	/* executable code */

#	ifdef XXX  /* old version */
	if  (fscanf( tf, "%f ", dist ) != 1)  {
		*status = TTE_FREAD;
		return;
	} /*endif*/

	for  (i=0; i<14; i++)
		if  (fscanf(tf,"%d ",tt+i) != 1)  {
			*status = TTE_FREAD;
			return;
		} /*endif*/
#	endif

	if  (fscanf( tf, "%5f ", dist ) != 1)  {
		*status = TTE_FREAD;
		return;
	} /*endif*/

	for  (i=0; i<14; i++)
		if  (fscanf(tf,"%5d",tt+i) != 1)  {
			*status = TTE_FREAD;
			return;
		} /*endif*/

	/* forget end of line */
	fgets( scratch, BC_LINELTH, tf );

} /* end of tth_readline */



/*-------------------------------------------------------------------*/

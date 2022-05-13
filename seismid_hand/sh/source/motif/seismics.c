
/* file seismics.c
 *      ==========
 *
 * version 34, 16-Jun-2006
 *
 * seismic utilities
 * K. Stammler, 25-Mar-93
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
#include "erusrdef.h"
#include "tcusrdef.h"
#include "ptusrdef.h"
#include "earthloc.h"
#include "glusrdef.h"
#include "seismics.h"
#include "globalparams.h"



/* for identify phases, two operation modes */
#define cSiModeSearch 1
#define cSiModeSelect 2

/* maximum length of source string in char */
#define cSiSrcLength 7

/* actual tolerance values for identify_phase, values taken from parameter file */
static float siv_tol_trav=(-1.0);
static float siv_tol_slow=(-1.0);
static float siv_tol_azim=(-1.0);
/* optimum travel time fit for identify_phase */
static float siv_trav_dt=1000.0;
static char  siv_opt_phase[cBcShortStrLth+1];  /* best fitting phase */



static char *siv_evlists[] = {
	/*"neic-finger",*/ "sed-redpuma", "emsc", "isc", ""
};

#ifdef XXX
static char *siv_infosrc[] = {
	/*"neic-a",*/ "sed-rp", "emsc", ""
};
#endif

/* phase list for phase identification */
static char *siv_idphases[] = {
	"P", "Pdiff", "PcP", "PP", "PPP", "PKPdf", "PKPab", "PKPbc", "SP",
	"PKKPdf", "PKKPbc", "PKKPab", "SKPdf", "SKPab", "SKKPdf",
	"S", "Sdiff", "ScS", "SS", "SKSdf", "SSS", "SKKSac", "SKKSdf", ""
};

/* ml distance correction (C.F.RICHTER (1958): Elementary 
 * Seismology. p 342, up to 600 km epicentral distance,
 * G.SCHNEIDER (1975): Erdbeben. p.338, from 700 up to 1000 km
 * epicentral distance)
 */
static float siv_ml_sigma[] = {
	1.4,1.4,1.5,1.6,1.7,1.9,2.1,2.3,2.4,2.5,2.6,2.7,2.8,2.8,2.8,
	2.8,2.9,2.9,3.0,3.0,3.0,3.1,3.1,3.2,3.2,3.3,3.3,3.4,3.4,3.5,
	3.5,3.6,3.65,3.7,3.7,3.8,3.8,3.9,3.9,4.0,4.0,4.1,4.1,4.2,4.2,
	4.3,4.3,4.3,4.4,4.4,4.5,4.5,4.5,4.6,4.6,4.6,4.6,4.7,4.7,4.7,
	4.7,4.8,4.8,4.8,4.8,4.8,4.9,4.9,4.9,4.9,4.9,5.2,5.4,5.5,5.7
};
#define cSiMlSigmaLth 75


/* prototypes of local routines */
static void si_identify_phase_step( int mode, char onset[], char station[],
	float slowness, float azimuth, char phase[], float *lat, float *lon,
	float *dep, char origin[], char infosrc[], TSyStatus *status );
static float si_get_maxtol( char phase[] );
static void si_search_events( char type[], TSyBoolean newlist, char onset[],
	float backsec, float trav[], float lat[], float lon[], float depth[],
	char infsrc[][cSiSrcLength+1], int *evno, TSyStatus *status );


/*---------------------------------------------------------------------------*/



void si_fit_distance( char phase[], float slowness, float depth,
	float tol, float *distance, STATUS *status )

/* fits distance from slowness and depth
 *
 * parameters of routine
 * char       phase[];      input; phase name
 * float      slowness;     input; slowness in sec/deg
 * float      depth;        input; depth in km
 * float      tol;          input; tolerance in slowness
 * float      *distance     output; distance in deg
 */
{
	/* local variables */
	float    min_dist, max_dist;   /* distance window to search */
	float    curr_dist;            /* current distance */
	float    curr_slow;            /* current slowness */
	int      i;                    /* loop counter */

	/* executable code */

	min_dist = 2.0;
	max_dist = 105.0;  /* for P only useful */

	i = 0;
	FOREVER  {
		curr_dist = (min_dist + max_dist) / 2.0;
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: curr dist %5.1f\n", curr_dist );
		curr_slow = pt_slowness( phase, curr_dist, depth, status );
		if  (Severe(status))  return;
		if  (Abs(curr_slow-slowness) <= tol)  break;
		if  (curr_slow > slowness)  {
			min_dist = curr_dist;
		} else {
			max_dist = curr_dist;
		} /*endif*/
		if  (++i > 50)  {
			*status = SIE_NOCONVERG;
			return;
		} /*endif*/
	} /*endfor*/
	*distance = curr_dist;

} /* end of si_fit_distance */



/*---------------------------------------------------------------------------*/



#define MAGN_ML_MIN_DIST 16
#define MAGN_ML_MAX_DIST 120



float si_magn_mb( float ampl, float period, float distance, STATUS *status )

/* Returns magnitude mb.  Passed amplitude and period must origin from a
 * Z component.
 *
 * parameters of routine
 * float      ampl;        input; amplitude in nm
 * float      period;      input; amplitude in sec
 * float      distance;    input; epicentral distance in deg
 * STATUS     *status;     output; return status
 *                         returns value of mb
 */
{

	/* local data array */
	static float mb_sigma[] = {
		5.9,5.9,5.9,6.0,6.0,6.1,6.2,6.3,6.3,6.5,6.4,6.5,6.6,
		6.6,6.6,6.7,6.7,6.7,6.7,6.7,6.6,6.5,6.5,6.4,6.4,6.5,6.5,6.5,6.5,
		6.7,6.8,6.9,6.9,6.8,6.7,6.7,6.7,6.7,6.8,6.8,6.8,6.8,6.8,6.8,6.8,
		6.6,7.0,6.9,7.0,7.0,7.0,7.0,7.0,7.0,6.9,6.9,6.9,6.9,6.8,6.8,6.9,
		6.9,6.9,6.8,6.7,6.8,6.9,7.0,7.0,7.0,6.9,6.9,7.1,7.0,7.0,7.1,7.1,
		7.2,7.1,7.2,7.3,7.4,7.5,7.5,7.4,7.3,7.4,7.5,7.6,7.7,7.8,7.9,7.9,
		8.0,8.1,8.2,8.2,8.4,8.6,8.7,8.8,8.9,9.0,9.0,9.0
	};

	/* local variables */
	int      idist;     /* rounded distance */
	float    tmp;       /* scratch */

	/* executable code */

	idist = Nint( distance );
	if  (idist < MAGN_ML_MIN_DIST || idist > MAGN_ML_MAX_DIST)  {
		*status = SIE_MAGN_DIST_OOR;
		return 0.0;
	} /*endif*/

	if  (ampl <= 0.0 || period <= 0.0)  {
		*status = SIE_VALUE_OOR;
		return 0.0;
	} /*endif*/

	/* rescale to microns */
	ampl /= 1000.0;

	tmp = log10( (double)(ampl/period) ) + mb_sigma[idist-MAGN_ML_MIN_DIST];

	if  (GpGetInt(cGpI_debug_level) > 1)
		printf( "SHM-dbg2: mb %3.1f, distance %5.1f, period %5.2f, ampl %f\n",
			tmp, distance, period, ampl );
	return tmp;

	/* this didn't work (in dbxtool):
	return ( log10(ampl/period) + mb_sigma[idist-MAGN_ML_MIN_DIST] );
	*/

} /* end of si_magn_mb */



#undef MAGN_ML_MIN_DIST
#undef MAGN_ML_MAX_DIST



/*---------------------------------------------------------------------------*/



float si_magn_ms_plain( float ampl, float period, float distance,
	STATUS *status )

/* returns MS magnitude, using Prague formula
 *
 * parameters of routine
 * float      ampl;        input; amplitude in nm
 * float      period;      input; amplitude in sec
 * float      distance;    input; epicentral distance in deg
 * STATUS     *status;     output; return status
 *                         returns value of MS
 */
{
	/* local data array */
	float ms_min_period[] = {
		4,4,5,5,5,6,6,7,7,9,10,12,13,14,15,16,16,16,17,17,18,18
	};

	/* local variables */
	float    tmp;       /* scratch */
	int      index;     /* index in ms_min_period */
	int      idist;     /* rounded distance */
	float    minper;    /* minimum period */

	/* executable code */

	/* check distance & period */
	idist = Nint( distance );
	if  (idist < 2)  {
		*status = SIE_MAGN_DIST_OOR;
		return 0.0;
	} else if  (idist > 140)  {
		minper = 18.0;
	} else {
		index = (idist > 10) ? Nint(distance/10.0)+7 : Nint(distance)-2;
		minper = ms_min_period[index];
	} /*endif*/
	if  (period < minper)  {
		*status = SIE_VALUE_OOR;
		return 0.0;
	} /*endif*/

	/* check amplitude */
	if  (ampl <= 0.0)  {
		*status = SIE_VALUE_OOR;
		return 0.0;
	} /*endif*/

	/* rescale to microns */
	ampl /= 1000.0;

	tmp = log10((double)(ampl/period)) + 1.66*log10((double)distance) + 3.3;

	if  (GpGetInt(cGpI_debug_level) > 0)
		printf( "SHM-dbg1: MS %3.1f, distance %5.1f, period %5.2f, ampl %f\n",
			tmp, distance, period, ampl );
	return tmp;

} /* end of si_magn_ms_plain */



/*---------------------------------------------------------------------------*/



#define MAGN_MS_MIN_DIST 1
#define MAGN_MS_MAX_DIST 99
#define MAGN_MS_MIN_PER 10
#define MAGN_MS_MAX_PER 40



float si_magn_ms_m_b( int type, float ampl, float period, float distance,
	STATUS *status )

/* returns MS magnitude, using Marshall-Basham formula
 *
 * parameters of routine
 * int        type;        input; path type
 * float      ampl;        input; amplitude in nm
 * float      period;      input; amplitude in sec
 * float      distance;    input; epicentral distance in deg
 * STATUS     *status;     output; return status
 *                         returns value of MS
 */
{
	/* local data arrays */
	/* Marshal-Basham MS: distance correction table */
	float ms_delta[] = {
		0.17,0.35,0.57,0.67,0.78,0.84,0.90,0.95,0.98,1.02,
		1.05,1.08,1.11,1.13,1.15,1.17,1.19,1.21,1.23,1.25,
		1.27,1.29,1.31,1.32,1.34,1.36,1.38,1.40,1.41,1.43,
		1.44,1.45,1.47,1.48,1.50,1.52,1.54,1.55,1.56,1.57,
		1.59,1.61,1.62,1.64,1.65,1.66,1.68,1.70,1.71,1.72,
		1.74,1.75,1.76,1.77,1.78,1.80,1.82,1.83,1.84,1.85,
		1.87,1.89,1.90,1.91,1.92,1.93,1.94,1.95,1.96,1.97,
		1.98,1.99,2.01,2.02,2.03,2.04,2.05,2.06,2.07,2.08,
		2.09,2.10,2.11,2.12,2.13,2.14,2.15,2.16,2.17,2.18,
		2.18,2.19,2.20,2.21,2.22,2.23,2.24,2.25,2.26
	};

	/* Marshal Basham MS: path correction */
	float ms_path[][31] = {
		/* path: c-na */
		{-0.75,-0.67,-0.61,-0.53,-0.46,-0.38,-0.30,-0.24,
		 -0.16,-0.08, 0.00, 0.01, 0.03, 0.04, 0.05, 0.07,
		  0.09, 0.11, 0.13, 0.14, 0.16, 0.17, 0.18, 0.20,
		  0.21, 0.23, 0.25, 0.27, 0.28, 0.29, 0.31},
		/* path: c-eu */
		{-0.30,-0.27,-0.24, 0.21,-0.18,-0.15,-0.13,-0.10,
		 -0.07,-0.04, 0.00, 0.03, 0.05, 0.07, 0.11, 0.14,
		  0.18, 0.22, 0.24, 0.27, 0.30, 0.32, 0.33, 0.34,
		  0.35, 0.36, 0.37, 0.38, 0.39, 0.40, 0.41},
		/* path: c-o */
		{ 0.00, 0.01, 0.03, 0.04, 0.05, 0.07, 0.08, 0.09,
		  0.10, 0.12, 0.13, 0.15, 0.16, 0.17, 0.18, 0.20,
		  0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28,
		  0.29, 0.30, 0.31, 0.32, 0.33, 0.34, 0.35},
		/* path: 0 */
		{ 0.50, 0.45, 0.38, 0.33, 0.27, 0.20, 0.15, 0.09,
		  0.03,-0.03,-0.09,-0.08,-0.07,-0.06,-0.05,-0.04,
		 -0.03,-0.03,-0.02,-0.01, 0.00, 0.01, 0.02, 0.03,
		  0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10}
	};


	/* local variables */
	int      idist;        /* rounded distance */
	int      iper;         /* rounded period */
	int      path_idx;     /* path index */
	float    tmp;          /* scratch */

	/* executable code */

	/* check distance */
	idist = Nint( distance );
	if  (idist < MAGN_MS_MIN_DIST || idist > MAGN_MS_MAX_DIST)  {
		*status = SIE_MAGN_DIST_OOR;
		return 0.0;
	} /*endif*/

	/* check period */
	iper = Nint( period );
	if  (iper < MAGN_MS_MIN_PER || iper > MAGN_MS_MAX_PER)  {
		*status = SIE_VALUE_OOR;
		return 0.0;
	} /*endif*/

	/* check amplitude */
	if  (ampl <= 0.0)  {
		*status = SIE_VALUE_OOR;
		return 0.0;
	} /*endif*/

	/* get path index */
	switch  (type)  {
	case SIC_MAGN_MS_C_NA:  path_idx = 0;   break;
	case SIC_MAGN_MS_C_EU:  path_idx = 1;   break;
	case SIC_MAGN_MS_C_O:   path_idx = 2;   break;
	case SIC_MAGN_MS_O:     path_idx = 3;   break;
	default:  *status = SIE_ILL_MAGN_TYPE;  return 0.0;
	} /*endswitch*/

	/* rescale to microns */
	ampl /= 1000.0;

	tmp = log10((double)ampl) + ms_delta[idist-MAGN_MS_MIN_DIST]
		+ ms_path[path_idx][iper-MAGN_MS_MIN_PER] + 3.0;

	if  (GpGetInt(cGpI_debug_level) > 0)
		printf( "SHM-dbg1: MS %3.1f, distance %5.1f, period %5.2f, ampl %f\n",
			tmp, distance, period, ampl );
	return tmp;

} /* end of si_magn_ms_m_b */



#undef MAGN_MS_MIN_DIST
#undef MAGN_MS_MAX_DIST
#undef MAGN_MS_MIN_PER
#undef MAGN_MS_MAX_PER



/*---------------------------------------------------------------------------*/



#define MAGN_ML_MIN_DIST 1.0
#define MAGN_ML_MAX_DIST 1000.0



float si_magn_ml( float ampl, float distance, STATUS *status )

/* Returns magnitude ml.  Amplitude and period must be read from a WOOD-ANDERSON
 * simulated seismogram, horizontal component.
 *
 * parameters of routine
 * float      ampl;        input; amplitude of WOOD-ANDERSON simulation
 * float      distance;    input; epicentral distance in km
 * STATUS     *status;     output; return status
 *                         returns value of ml
 */
{
	/* ml distance correction (C.F.RICHTER (1958): Elementary 
	 * Seismology. p 342, up to 600 km epicentral distance,
	 * G.SCHNEIDER (1975): Erdbeben. p.338, from 700 up to 1000 km
	 * epicentral distance)
	 */

	/* local variables */
	int      idist;        /* rounded distance */
	float    delta;        /* step size */
	int      ioffset;      /* index offset */
	float    grad;         /* gradient */
	int      itmp;         /* scratch */
	float    tmp;          /* scratch */

	/* executable code */

	/* check distance */
	if  (distance < MAGN_ML_MIN_DIST || distance >= MAGN_ML_MAX_DIST)  {
		*status = SIE_MAGN_DIST_OOR;
		return 0.0;
	} /*endif*/

	/* get step size */
	if  (distance > 600.0)  {
		delta = 100.0;
		ioffset = 64;
	} else if  (distance > 100.0)  {
		delta = 10.0;
		ioffset = 10;
	} else {
		delta = 5.0;
		ioffset = 0;
	} /*endif*/

	itmp = Nint( floor(distance/delta) );
	idist = itmp + ioffset;
	grad = ( siv_ml_sigma[idist+1] - siv_ml_sigma[idist] ) / delta;
	tmp = distance - (float)itmp*delta;
	tmp = siv_ml_sigma[idist] + tmp*grad;

	tmp += log10((double)ampl);

	if  (GpGetInt(cGpI_debug_level) > 0)
		printf( "SHM-dbg1: ml %3.1f, distance %5.1f, ampl %f\n",
			tmp, distance, ampl );

	return tmp;

} /* end of si_magn_ml */



#undef MAGN_ML_MIN_DIST
#undef MAGN_ML_MAX_DIST



/*---------------------------------------------------------------------------*/



void si_ext_location( float lat, float lon, float depth, char refstation[],
	float *distance, float *azimuth, float *p_slowness, STATUS *status )

/* Returns distance, back-azimuth & P-slowness (if possible) of given
 * hypocenter to refstation.
 *
 * parameters of routine
 * float      lat, lon;       input; epicenter of event;
 * float      depth;          input; event depth in km
 * char       refstation[];   input; name of receiving station
 * float      *distance;      output; distance in deg
 * float      *azimuth;       output; back-azimuth in deg
 * float      *p_slowness;    output; P-slowness or 0.0
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	GLT_STATINF *statinf;    /* station info */
	double      d_dist, d_azim, d_bazim;
	float       p_slow;     /* local value of P-slowness */

	/* executable code */

	statinf = gl_store_station( refstation, TRUE, status );
	if  (Severe(status))  return;
	mb_locdiff( statinf->lat, statinf->lon, lat, lon,
		&d_dist, &d_azim, &d_bazim );
	*distance = d_dist;
	*azimuth = d_bazim;
	p_slow = pt_slowness( "P", *distance, depth, status );
	if  (Severe(status))  {
		*status = BC_NOERROR;
		if  (GpGetInt(cGpI_debug_level) > 1)
			printf( "SHM-dbg2: P-slowness could not be computed\n" );
	} else {
		*p_slowness = p_slow;
	} /*endif*/

} /* end of si_ext_location */



/*---------------------------------------------------------------------------*/



void si_get_location( float distance, float azimuth, char refstation[],
	float *lat, float *lon, int *ferid, char fername[], STATUS *status )

/* computes location from given distance and azimuth relative to given
 * reference station.  'fername' must have minimum length BC_LINELTH
 *
 * parameters of routine
 * float     distance;       input; distance in deg
 * float     azimuth;        input; back-azimuth in deg
 * char      refstation[];   input; name of reference station
 * float     *lat, *lon;     output; computed source location
 * int       *ferid;         output; Flinn-Engdahl region number
 * char      fername[];      output; name of Flinn-Engdahl region(>= BC_LINELTH)
 * STATUS    *status;        output; return status
 */
{
	/* local variables */
	GLT_STATINF *statinf;    /* station info */

	/* executable code */

	statinf = gl_store_station( refstation, TRUE, status );
	if  (Severe(status))  return;

	mb_sphereadd( statinf->lat, statinf->lon, distance, azimuth, lat, lon );
	mb_ferindex( *lat, *lon, ferid, status );
	if  (Severe(status))  return;
	mb_fername( *ferid, BC_LINELTH, fername, status );

} /* end of si_get_location */



/*---------------------------------------------------------------------------*/



TSyBoolean si_is_first_onset( char mode[], char phase[] )

/* Returns TRUE if 'phase' is a first onset phase.
 *
 * parameters of routine
 * char       mode[];       input; 'tele' or not
 * char       phase[];      input; name of phase
 *                          returns TRUE if phase is first onset
 */
{
	/* local variables */

	/* executable code */

	if  (strcmp(phase,"P") == 0)  return TRUE;
	if  (strcmp(phase,"PKPab") == 0)  return TRUE;
	if  (strcmp(phase,"PKPbc") == 0)  return TRUE;
	if  (strcmp(phase,"PKPdf") == 0)  return TRUE;
	if  (strcmp(phase,"PKP") == 0)  return TRUE;

	if  (strcmp(mode,"tele") == 0)  return FALSE;

	if  (strcmp(phase,"Pg") == 0)  return TRUE;
	if  (strcmp(phase,"Pn") == 0)  return TRUE;

	return FALSE;

} /* end of si_is_first_onset */



/*---------------------------------------------------------------------------*/


/* length of search window in sec */
#define SEARCHWDW 7200.0
/* maximum number of event expected in search window */
#define MAXEVNO 30


void si_match_location( char type[], char phase[], char onset[], char station[],
	float slowness, float azimuth, TSyBoolean newlist, float *lat, float *lon,
	float *dep, char origin[], char infsrc[], TSyStatus *status )

/* Finds matching locations in external event lists
 *
 * parameters of routine
 * char       type[];      input; which event list
 * char       phase[];     input; name of phase
 * char       onset[];     input; onset time of phase
 * char       station[];   input; station of phase reading
 * float      slowness;    input; slowness of phase
 * float      azimuth;     input; azimuth of phase
 * TSyBoolean newlist;     input; retrieve new event list or use old one
 * float      *lat;        output; latitude found
 * float      *lon;        output; longitude found
 * float      *dep;        output; depth found
 * char       origin[];    output; origin time
 * char       infsrc[];    output; information source
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	float    trav[MAXEVNO];            /* travel time of phase */
	float    evlat[MAXEVNO];           /* latitude of events */
	float    evlon[MAXEVNO];           /* longitude of events */
	float    depth[MAXEVNO];           /* depth of events */
	char     infsrc_l[MAXEVNO][cSiSrcLength+1]; /* info source */
	int      evno;                     /* number of events found */
	int      i;                        /* counter */
	GLT_STATINF *statinf;    /* station info */
	double   d_dist, d_azim, d_bazim;  /* difference epi - station */
	float    f_dist, f_azim;           /* difference epi - station */
	float    ctrav, cslow;             /* current traveltime and slowness */
	TSyStatus locstat;                 /* local status */
	TSyBoolean found;                  /* event found */
	float    diff_trav, diff_azim, diff_slow; /* differences */
	float    obs_azimuth;              /* observed azimuth, +180 deg ? */

	/* executable code */

	/* initialise tolerance values if not yet done */
	if  (siv_tol_trav < 0.0)  siv_tol_trav = GpGetFloat( cGpF_idphases_tol_trav );
	if  (siv_tol_slow < 0.0)  siv_tol_slow = GpGetFloat( cGpF_idphases_tol_slow );
	if  (siv_tol_azim < 0.0)  siv_tol_azim = GpGetFloat( cGpF_idphases_tol_azim );

	/* find events in search window */
	si_search_events( type, newlist, onset, SEARCHWDW, trav, evlat, evlon,
		depth, infsrc_l, &evno, status );
	if  (SySevere(status))  return;

	statinf = gl_store_station( station, TRUE, status );
	if  (SySevere(status))  return;

	/* may change siv_tol_trav for surface waves, since LR is not searched
    * in identify_phase */
	if  (strcmp(phase,"LR") == 0)
		siv_tol_trav = GpGetFloat( cGpF_idphases_tol_travsurf );

	found = FALSE;
	for  (i=0; i<evno; i++)  {

		/* may have been changed in last loop */
		obs_azimuth = azimuth;

		/* compute traveltime, azimuth and slowness for each possible event */
		mb_locdiff( statinf->lat, statinf->lon, evlat[i], evlon[i],
			&d_dist, &d_azim, &d_bazim );
		f_dist = d_dist; f_azim = d_bazim;
		locstat = cBcNoError;
		if  (strcmp(phase,"PKP") == 0)  {
			ctrav = pt_travel( "PKPdf", f_dist, depth[i], &locstat );
			if  (locstat != cBcNoError)
				ctrav = pt_travel( "PKPbc", f_dist, depth[i], &locstat );
			if  (locstat != cBcNoError)
				ctrav = pt_travel( "PKPab", f_dist, depth[i], &locstat );
			if  (locstat != cBcNoError)  ctrav = -100.0;
			cslow = 0.0;
		} else {
			ctrav = pt_travel( phase, f_dist, depth[i], &locstat );
			if  (locstat != cBcNoError)  ctrav = -100.0;
			cslow = pt_slowness( phase, f_dist, depth[i], &locstat );
			if  (locstat != cBcNoError)  cslow = -100.0;
			if  (cslow < 0.0 && cslow > -99.0)  {
				obs_azimuth = azimuth + 180.0;
				if  (obs_azimuth > 360.0)  obs_azimuth -= 360.0;
				cslow = -cslow;
			} /*endif*/
		} /*endif*/

		/* check for matching parameters */
		/* difference in travel time */
		if  (ctrav == -100.0)  {
			/* if no travel time could be computed, set to large value */
			diff_trav=9999.9;
		} else {
			diff_trav = fabs( ctrav - trav[i] );
		} /*endif*/
		/* difference in slowness */
		diff_slow = 0.0;
		if  (strcmp(phase,"PKP") != 0 && strcmp(phase,"LR") != 0)
			if  (cslow == -100.0)  {
				/* if no slowness could be computed, set to large value */
				diff_slow = 99.9;
			} else {
				diff_slow = fabs( cslow - slowness );
			} /*endif*/
		/* difference in azimuth */
		if  (slowness < 2.5)  {
			/* there is no resolution in azimuth on very steep incidence angles */
			diff_azim = 0.0;
		} else {
			diff_azim = fabs( (float)f_azim - obs_azimuth );
			if  (diff_azim > 180.0)  diff_azim = fabs( diff_azim - 360.0 );
		} /*endif*/
		if  (siv_tol_trav == 0.0)  {
			printf( "   %11s %5s: dt=%6.1f da=%5.1f ds=%4.1f",
				type, phase, diff_trav, diff_azim, diff_slow );
			printf( " ev%d: (%6.2f,%7.2f) %5.1f\n",
				i, evlat[i], evlon[i], depth[i] );
		} /*endif*/

#ifdef XXX
		/* debug output */
		printf( "%f %f %5.f %s: t:%f a:%f\n", evlat[i], evlon[i], depth[i],
			infsrc_l[i], diff_trav, diff_azim );
#endif

		/* store best diff_trav, if slowness and azimuth match */
		if  (diff_slow < siv_tol_slow && diff_azim < siv_tol_azim)
			if  (diff_trav < siv_trav_dt)  {
				siv_trav_dt = diff_trav;
				strcpy( siv_opt_phase, phase );
			} /*endif*/

		if  (diff_trav < siv_tol_trav && diff_slow < siv_tol_slow 
			&& diff_azim < siv_tol_azim)  {
			if  (found)  {
				*status = SIE_MORE_MATCHES;
				return;
			} /*endif*/
			found = TRUE;
			*lat = evlat[i];
			*lon = evlon[i];
			*dep = depth[i];
			strcpy( infsrc, infsrc_l[i] );
			tc_tadd( onset, -trav[i], origin, status );
			if  (SySevere(status))  return;
			printf( "   %s: accept phase %s on event %s (%5.2f,%6.2f) %5.1f from %s\n",
				type, phase, origin, *lat, *lon, *dep, infsrc );
			break;
		} /*endif*/

	} /*endfor*/

	if  (!found)  *status = SIE_NO_EV_FOUND;

} /* end of si_match_location */



/*---------------------------------------------------------------------------*/


#define TMPFILE "search_ev.dat"
#define SEDFILE "sed_redpuma.dat"
#define EMSCFILE "search_emsc.dat"
#define ISCFILE "search_isc.dat"
#define FINGERADDRESS "quake@ghtftp.cr.usgs.gov"
#define SEDPAGE "http://www.seismo.ethz.ch/redpuma/redpuma_ami_list.html"
/* #define EMSCPAGE "http://www.emsc-csem.org/cgi-bin/ALERT_all_messages.sh" */


void si_search_events( char type[], TSyBoolean newlist, char onset[],
	float backsec, float trav[], float lat[], float lon[], float depth[],
	char infsrc[][cSiSrcLength+1], int *evno, TSyStatus *status )

/* Searches for events in given event lists within a time window
 *
 * parameters of routine
 * char       type[];         input; which list
 * TSyBoolean newlist;        input; retrieve new list or use old one
 * char       onset[];        input; end of search window (onset time)
 * float      backsec;        input; time window in sec backward from onset
 * float      trav[];         output; travel time (onset - origin)
 * float      lat[];          output; latitudes
 * float      lon[];          output; longitudes
 * float      depth[];        output; depths
 * char       infsrc[][cSiSrcLength+1]; output; information source
 * int        *evno;          output; number of events
 * TSyStatus  *status;        output; return status
 */
{
	/* local variables */
	char     cmd[cBcLongStrLth+1];     /* shell command */
	char     tmpfile[cBcFileLth+1];    /* scratch file */
	char     *env;                     /* pointer to environment */
	FILE     *fp;                      /* pointer to file */
	char     line[cBcLongStrLth+1];    /* current line of file */
	char     *lptr;                    /* point to char in line */
	TSyBoolean datalines;              /* line is a data line */
	char     origtime[cBcTimeLth+1];   /* origin time string */
	float    curtrav;                  /* travel time for current event */
	char     tmpstr[cBcLineLth+1];     /* scratch string */
	char     tmpstr2[cBcLineLth+1];    /* second scratch string */
	char     excl_agency[cBcLineLth+1];/* exclusive agency */
	char     tchar;                    /* scratch character */
	float    curlat, curlon;           /* epicentre of current line */
	float    curdep;                   /* current depth */
	float    curmag;                   /* current magnitude */
	int      i;                        /* counter */

	/* executable code */

	*evno = 0;

	strcpy( excl_agency, GpGetString(cGpS_exclusive_agency) );

	if  (strcmp(type,"neic-finger") == 0)  {

		/* name of scratch file */
		env = (char *)getenv( "SH_SCRATCH" );
		sprintf( tmpfile, "%s/%s", env, TMPFILE );
		if  (newlist)  {
			sprintf( cmd, "rm %s", tmpfile );
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SHM-dbg3: executing: %s\n", cmd );
			system( cmd );
			sprintf( cmd, "finger %s > %s", FINGERADDRESS, tmpfile );
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SHM-dbg3: executing: %s\n", cmd );
			system( cmd );
		} /*endif*/
		fp = fopen( tmpfile, "r" );
		if  (fp == NULL)  {
			*status = SIE_OPEN_READ;
			err_setcontext( " ## file " );
			err_setcontext( tmpfile );
			return;
		} /*endif*/
		datalines = FALSE;
		while  (fgets(line,cBcLongStrLth,fp) != NULL)  {
			if  (strncmp(line,"yy/mm/dd hh:mm:ss",17) == 0)  {
				datalines = TRUE;
				continue;
			} /*endif*/
			if  (!datalines)  continue;
			if  (*line == '\n' || *line == ' ')  continue;
			strncpy( origtime, line, 17 );
			origtime[17] = '\0';
			origtime[8] = '_';
			curtrav = tc_tdiff( onset, origtime, status );
			if  (SySevere(status))  {fclose(fp); return;}
			if  (curtrav >= 0.0 && curtrav <= backsec)  {
				if  (*evno == MAXEVNO)  {
					*status = SIE_TOO_MANY_EV;
					fclose( fp );
					return;
				} /*endif*/
				trav[*evno] = curtrav;
				strncpy( tmpstr, line+19, 5 );
				tmpstr[5] = '\0';
				sscanf( tmpstr, "%f", lat+(*evno) );
				if  (line[24] == 'S')  lat[*evno] = -lat[*evno];
				strncpy( tmpstr, line+26, 6 );
				tmpstr[6] = '\0';
				sscanf( tmpstr, "%f", lon+(*evno) );
				if  (line[32] == 'W')  lon[*evno] = -lon[*evno];
				strncpy( tmpstr, line+34, 5 );
				tmpstr[5] = '\0';
				sscanf( tmpstr, "%f", depth+(*evno) );
				strcpy( infsrc[*evno], "neic-f" );
				(*evno)++;
			} /*endif*/
		} /*endwhile*/
		fclose( fp );

	} else if  (strcmp(type,"sed-redpuma") == 0)  {

		/* name of scratch file */
		env = (char *)getenv( "SH_SCRATCH" );
		sprintf( tmpfile, "%s/%s", env, SEDFILE );
		if  (newlist)  {
			sprintf( cmd, "rm %s", tmpfile );
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SHM-dbg3: executing: %s\n", cmd );
			system( cmd );
			env = (char *)getenv( "SH_UTIL" );
			sprintf( cmd, "%s/get_html_text.csh %s %s", env, SEDPAGE, tmpfile );
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SHM-dbg3: executing: %s\n", cmd );
			system( cmd );
		} /*endif*/
		fp = fopen( tmpfile, "r" );
		if  (fp == NULL)  {
			*status = SIE_OPEN_READ;
			err_setcontext( " ## file " );
			err_setcontext( tmpfile );
			return;
		} /*endif*/
		datalines = FALSE;
		while  (fgets(line,cBcLongStrLth,fp) != NULL)  {
			if  (strncmp(line," D a t e  Time (UTC)",20) == 0)  {
				datalines = TRUE;
				continue;
			} /*endif*/
			if  (!datalines)  continue;
			if  (*line == '\n')  continue;
			if  (strlen(line) < 40)  continue;
			if  (line[0] < '0' || line[0] > '9')  continue;
			if  (line[9] != ' ')  continue;
			origtime[0] = line[0];
			origtime[1] = line[1];
			origtime[2] = '-';
			origtime[3] = line[2];
			origtime[4] = line[3];
			origtime[5] = line[4];
			origtime[6] = '-';
			strncpy( origtime+7, line+5, 4 );
			origtime[11] = '_';
			strncpy( origtime+12, line+10, 10 );
			origtime[22] = '\0';
			strncpy( tmpstr, line+44, 5);
			tmpstr[5] = '\0';
			if  (*excl_agency != '\0' && strcasecmp(excl_agency,tmpstr) != 0)  {
				/* if exclusive agency is set, accept only this */
				curtrav = -1.0;
			} else if  (strcmp(tmpstr,"A*NOR") == 0  || strcmp(tmpstr,"A*SED") == 0)  {
				curtrav = -1.0;  /* throw away */
			} else {
				curtrav = tc_tdiff( onset, origtime, status );
			} /*endif*/
			if  (SySevere(status))  {fclose(fp); return;}
			if  (curtrav >= 0.0 && curtrav <= backsec)  {
				if  (*evno == MAXEVNO)  {
					*status = SIE_TOO_MANY_EV;
					fclose( fp );
					return;
				} /*endif*/
				/* reformat source string */
				tchar = *tmpstr;
				if  (strcmp(tmpstr+2,"EMS") == 0)  {
					sprintf( tmpstr, "EMSC-%c", tchar );
				} else if  (strcmp(tmpstr+2,"GSR") == 0)  {
					sprintf( tmpstr, "GSRC-%c", tchar );
				} else if  (strcmp(tmpstr+2,"NEI") == 0)  {
					sprintf( tmpstr, "NEIC-%c", tchar );
				} else if  (strcmp(tmpstr+2,"SED") == 0)  {
					sprintf( tmpstr, "SED-%c", tchar );
				} else if  (strcmp(tmpstr+2,"ODC") == 0)  {
					sprintf( tmpstr, "ODC-%c", tchar );
				} /*endif*/
				strcpy( infsrc[*evno], tmpstr );
				trav[*evno] = curtrav;
				strncpy( tmpstr, line+21, 4 );
				tmpstr[4] = '\0';
				sscanf( tmpstr, "%f", lat+(*evno) );
				if  (line[25] == 'S')  lat[*evno] = -lat[*evno];
				strncpy( tmpstr, line+27, 5 );
				tmpstr[5] = '\0';
				sscanf( tmpstr, "%f", lon+(*evno) );
				if  (line[32] == 'W')  lon[*evno] = -lon[*evno];
				strncpy( tmpstr, line+33, 3 );
				tmpstr[3] = '\0';
				sscanf( tmpstr, "%f", depth+(*evno) );
				(*evno)++;
			} /*endif*/
		} /*endwhile*/
		fclose( fp );

	} else if  (strcmp(type,"emsc") == 0)  {

		/* name of scratch file */
		env = (char *)getenv( "SH_SCRATCH" );
		sprintf( tmpfile, "%s/%s", env, EMSCFILE );
		if  (newlist)  {
			sprintf( cmd, "rm %s", tmpfile );
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SHM-dbg3: executing: %s\n", cmd );
			system( cmd );
			env = (char *)getenv( "SH_UTIL" );
			sprintf( cmd, "%s/get_latest_emsc_pages.csh %s %s",
				env, tmpfile, onset );
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SHM-dbg3: executing: %s\n", cmd );
			system( cmd );
		} /*endif*/
		fp = fopen( tmpfile, "r" );
		if  (fp == NULL)  {
			*status = SIE_OPEN_READ;
			err_setcontext( " ## file " );
			err_setcontext( tmpfile );
			return;
		} /*endif*/
		/*datalines = FALSE; */
		datalines = TRUE;  /* only data lines now, after preprocessing of files */
		while  (fgets(line,cBcLongStrLth,fp) != NULL)  {
#ifdef XXX
			if  (strncmp(line,"  DATE       TIME",17) == 0)  {
				datalines = TRUE;
				continue;
			} /*endif*/
#endif
			if  (!datalines)  continue;
			lptr = line;
			while  (*lptr == ' ')  lptr++;
			if  (*lptr == '\n')  continue;
			if  (strlen(lptr) < 56)  continue;
			if  (lptr[0] < '0' || lptr[0] > '9')  continue;
			if  (lptr[4] != '-')  continue;
			if  (lptr[7] != '-')  continue;
			if  (lptr[10] != ' ')  continue;
			if  (lptr[11] != ' ')  continue;
			if  (lptr[17] != ':')  continue;
			/* origin time is in a fixed format */
			origtime[0] = lptr[8];
			origtime[1] = lptr[9];
			origtime[2] = ',';
			origtime[3] = lptr[5];
			origtime[4] = lptr[6];
			origtime[5] = ',';
			origtime[6] = lptr[0];
			origtime[7] = lptr[1];
			origtime[8] = lptr[2];
			origtime[9] = lptr[3];
			origtime[10] = ',';
			origtime[11] = lptr[12];
			origtime[12] = lptr[13];
			origtime[13] = ',';
			origtime[14] = lptr[15];
			origtime[15] = lptr[16];
			origtime[16] = ',';
			origtime[17] = lptr[18];
			origtime[18] = lptr[19];
			origtime[19] = ',';
			origtime[20] = lptr[21];
			origtime[21] = '0';
			origtime[22] = '0';
			origtime[23] = '\0';
			lptr += 23;   /* now at latitude */
			sscanf( lptr, "%f", &curlat );
			while  (*lptr != ' ' && *lptr != '\n')  lptr++;  /* skip over latitude */
			while  (*lptr == ' ')  lptr++;  /* now at latitude sign */
			if  (*lptr == 'S' || *lptr == 's')  curlat *= -1.0;
			lptr++;
			while  (*lptr == ' ')  lptr++;  /* now at longitude */
			sscanf( lptr, "%f", &curlon );
			while  (*lptr != ' ' && *lptr != '\n')  lptr++;  /* skip over longitude */
			while  (*lptr == ' ')  lptr++;  /* now at longitude sign */
			if  (*lptr == 'W' || *lptr == 'w')  curlon *= -1.0;
			lptr++;
			while  (*lptr == ' ')  lptr++;  /* now see what we find here */
			if  (*lptr >= '0' && *lptr <= '9')  {
				/* here we found depth */
				sscanf( lptr, "%f", &curdep );
				while  (*lptr != ' ' && *lptr != '\n')  lptr++;  /* skip over depth */
				while  (*lptr == ' ')  lptr++;
				if  (*lptr == 'G' && lptr[1] == ' ')  lptr += 2;
				if  (*lptr == 'f' && lptr[1] == ' ')  lptr += 2;
			} else {
				curdep = 33.0;
			} /*endif*/
			if  (lptr[0] == 'm' && lptr[1] == 'b' && lptr[2] == ' ')  {
				lptr += 3;
				sscanf( lptr, "%f", &curmag );
				while  (*lptr != ' ' && *lptr != '\n')  lptr++;  /* skip over magnitude */
				while  (*lptr == ' ')  lptr++;
			} /*endif*/
			if  (lptr[0] == 'M' && lptr[1] == 'L' && lptr[2] == ' ')  {
				lptr += 3;
				sscanf( lptr, "%f", &curmag );
				while  (*lptr != ' ' && *lptr != '\n')  lptr++;  /* skip over magnitude */
				while  (*lptr == ' ')  lptr++;
			} /*endif*/
			if  (lptr[0] == 'M' && lptr[1] == 'S' && lptr[2] == ' ')  {
				lptr += 3;
				sscanf( lptr, "%f", &curmag );
				while  (*lptr != ' ' && *lptr != '\n')  lptr++;  /* skip over magnitude */
				while  (*lptr == ' ')  lptr++;
			} /*endif*/
			if  (lptr[0] == 'M' && lptr[1] == ' ' && lptr[2] >= '0' && lptr[2] <= '9')  {
				lptr += 2;
				sscanf( lptr, "%f", &curmag );
				while  (*lptr != ' ' && *lptr != '\n')  lptr++;  /* skip over magnitude */
				while  (*lptr == ' ')  lptr++;
			} /*endif*/
			tchar = *lptr;
			if  (tchar == 'A')  { 
				/* && (
				strcmp(tmpstr,"SED") == 0 ||
				strcmp(tmpstr,"ODC") == 0 ||
				strcmp(tmpstr,"EMSC") == 0 ||
				strcmp(tmpstr,"GFZ") == 0 ))  {  */
				curtrav = -1.0;  /* do not trust these automatic solutions */
			} else {
				curtrav = tc_tdiff( onset, origtime, status );
			} /*endif*/
			if  (SySevere(status))  {fclose(fp); return;}
			if  (curtrav >= 0.0 && curtrav <= backsec)  {
				if  (*evno == MAXEVNO)  {
					*status = SIE_TOO_MANY_EV;
					fclose( fp );
					return;
				} /*endif*/
				lptr = line + strlen(line) - 1;
				while  (*lptr == ' ' || *lptr == '\n')  lptr--;
				i = (int)(lptr - line);
				while  (lptr > line && *lptr != ' ')  lptr--;
				lptr++;
				i -= (int)(lptr - line);
				i++;
				if  (i > cSiSrcLength-2)  i = cSiSrcLength-2;
				strncpy( infsrc[*evno], lptr, i );
				infsrc[*evno][i] = '\0';
				/* if exclusive agency is set, accept only this */
				if  (*excl_agency != '\0'
					&& strcasecmp(excl_agency,infsrc[*evno]) != 0)
					continue;
				sprintf( tmpstr, "-%c", tchar );
				strcat( infsrc[*evno], tmpstr );
				trav[*evno] = curtrav;
				lat[*evno] = curlat;
				lon[*evno] = curlon;
				depth[*evno] = curdep;
				(*evno)++;
			} /*endif*/
		} /*endwhile*/
		fclose( fp );

	} else if  (strcmp(type,"isc") == 0)  {

		/* name of scratch file */
		env = (char *)getenv( "SH_SCRATCH" );
		sprintf( tmpfile, "%s/%s", env, ISCFILE );
		if  (newlist)  {
			sprintf( cmd, "rm %s", tmpfile );
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SHM-dbg3: executing: %s\n", cmd );
			system( cmd );
			env = (char *)getenv( "SH_UTIL" );
			tc_tadd( onset, -backsec, tmpstr2, status );
			sprintf( cmd, "%s/get_isc_events.csh %s %s %s 4.0",
				env, tmpstr2, onset, tmpfile );
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SHM-dbg3: executing: %s\n", cmd );
			system( cmd );
		} /*endif*/
		fp = fopen( tmpfile, "r" );
		if  (fp == NULL)  {
			*status = SIE_OPEN_READ;
			err_setcontext( " ## file " );
			err_setcontext( tmpfile );
			return;
		} /*endif*/
		while  (fgets(line,cBcLongStrLth,fp) != NULL)  {
			/* data lines start with the year, so either 19.. or 20.. */
			if  (*line < '1' || *line > '2')  continue;
			origtime[0] = line[8];
			origtime[1] = line[9];
			origtime[2] = ',';
			origtime[3] = line[5];
			origtime[4] = line[6];
			origtime[5] = ',';
			origtime[6] = line[0];
			origtime[7] = line[1];
			origtime[8] = line[2];
			origtime[9] = line[3];
			origtime[10] = ',';
			origtime[11] = line[11];
			origtime[12] = line[12];
			origtime[13] = ',';
			origtime[14] = line[14];
			origtime[15] = line[15];
			origtime[16] = ',';
			origtime[17] = line[17];
			origtime[18] = line[18];
			origtime[19] = ',';
			origtime[20] = (line[20] == ' ' ? '0' : line[20]);
			origtime[21] = (line[21] == ' ' ? '0' : line[21]);
			origtime[22] = '0';
			origtime[23] = '\0';
			if  (sscanf(line+36,"%f",&curlat) != 1)  continue;
			if  (sscanf(line+45,"%f",&curlon) != 1)  continue;
			strncpy( tmpstr2, line+71, 6 );
			tmpstr2[6] = '\0';
			if  (sscanf(tmpstr2,"%f",&curdep) != 1)  curdep = 33.0;
			curtrav = tc_tdiff( onset, origtime, status );
			if  (SySevere(status))  {fclose(fp); return;}
			if  (curtrav >= 0.0 && curtrav <= backsec)  {
				if  (*evno == MAXEVNO)  {
					*status = SIE_TOO_MANY_EV;
					fclose( fp );
					return;
				} /*endif*/
				lptr = line + 118;
				while  (*lptr == ' ' || *lptr != '\n')  lptr++;
				i = strlen( lptr );
				if  (lptr[i] == '\n')  lptr[i--] = '\0';
				if  (i > cSiSrcLength)  i = cSiSrcLength;
				strncpy( infsrc[*evno], lptr, i );
				infsrc[*evno][i] = '\0';
				/* if exclusive agency is set, accept only this */
				if  (*excl_agency != '\0'
					&& strcasecmp(excl_agency,infsrc[*evno]) != 0)
					continue;
				trav[*evno] = curtrav;
				lat[*evno] = curlat;
				lon[*evno] = curlon;
				depth[*evno] = curdep;
				(*evno)++;
			} /*endif*/
		} /*endwhile*/
		fclose( fp );
	} else {
		*status = SIE_ILL_EVLIST;
		return;
	} /*endif*/

} /* end of si_search_events */



/*---------------------------------------------------------------------------*/



void si_identify_phase( char onset[], char station[],
	float slowness, float azimuth, char phase[], float *lat, float *lon,
	float *dep, char origin[], char infosrc[], TSyStatus *status )

/* identifies phase from onset time using external event lists
 *
 * parameters of routine
 * char       onset[];    input; onset time of phase
 * char       station[];  input; station name
 * float      slowness;   input; slowness of phase
 * float      azimuth;    input; back azimuth of phase
 * char       phase[];    output; phase name
 * float      *lat;       output; epicenter latitude
 * float      *lon;       output; epicenter longitude
 * float      *dep;       output; source depth
 * char       origin[];   output; origin time
 * char       infosrc[];  output; information source (which epi list)
 * TSyStatus  *status;    output; return status
 */
{
	/* local variables */
	TSyStatus   locstat;    /* local status */

	/* executable code */

	locstat = cBcNoError;
	si_identify_phase_step( cSiModeSearch, onset, station, slowness, azimuth,
		phase, lat, lon, dep, origin, infosrc, &locstat );
	si_identify_phase_step( cSiModeSelect, onset, station, slowness, azimuth,
		phase, lat, lon, dep, origin, infosrc, status );

} /* end of si_identify_phase */



/*---------------------------------------------------------------------------*/



static void si_identify_phase_step( int mode, char onset[], char station[],
	float slowness, float azimuth, char phase[], float *lat, float *lon,
	float *dep, char origin[], char infosrc[], TSyStatus *status )

/* Called by si_identify_phase, identifies phase from onset time using
 * external event lists
 *
 * parameters of routine
 * int        mode;       input; operation mode
 * char       onset[];    input; onset time of phase
 * char       station[];  input; station name
 * float      slowness;   input; slowness of phase
 * float      azimuth;    input; back azimuth of phase
 * char       phase[];    output; phase name
 * float      *lat;       output; epicenter latitude
 * float      *lon;       output; epicenter longitude
 * float      *dep;       output; source depth
 * char       origin[];   output; origin time
 * char       infosrc[];  output; information source (which epi list)
 * TSyStatus  *status;    output; return status
 */
{
	/* local variables */
	static TSyBoolean lists_created=FALSE;  /* lists are on scratch disk */
	int      i, l;         /* counters */
	TSyBoolean newlist;    /* create new list */
	TSyStatus  locstat;    /* local status */
	float      maxtol;     /* maximum tolerance time */

	/* executable code */

	if  (mode == cSiModeSearch)
		printf( "*SHM: identify onset %s on %s, slo %4.1f, baz %5.1f\n",
			onset, station, slowness, azimuth );

	/* set travel time tolerance according to operation mode */
	if  (mode == cSiModeSearch)  {
		/* if in search mode, set tolerance to zero, so nothing is accepted */
		siv_tol_trav = 0.0;
		siv_trav_dt = 1000.0;
		*siv_opt_phase = '\0';
	} else {
		/* if a reasonably fitting event was found, set the tolerance to the
		   best one found */
		maxtol = si_get_maxtol( siv_opt_phase );
		if  (siv_trav_dt < maxtol)  {
			siv_tol_trav = siv_trav_dt + 0.1;
		} else {
			siv_tol_trav = GpGetFloat( cGpF_idphases_tol_trav );
		} /*endif*/
	} /*endif*/
	if  (GpGetInt(cGpI_debug_level) > 2)
		printf( "SHM-dbg3: dt tol set to %4.1f\n", siv_tol_trav );

	/* loop all available lists from different agencies */
	l = 0;
	while  (siv_evlists[l][0] != '\0')  {

		/* check all phases */
		i = 0;
		if  (strcmp(siv_evlists[l],"isc") == 0)  {
			/* ISC has a list over many years, each event may need a new request */
			newlist = (mode == cSiModeSearch);
		} else {
			/* from redpuma and EMSC only recent events are copied, one request
          * per SHM session is enough */
			newlist = !lists_created;
		} /*endif*/
		while  (siv_idphases[i][0] != '\0')  {
			locstat = cBcNoError;
			si_match_location( siv_evlists[l], siv_idphases[i], onset, station,
				slowness, azimuth, newlist, lat, lon, dep, origin, infosrc,
				&locstat );
			if  (locstat == cBcNoError)  {
				/* phase found */
				strcpy( phase, siv_idphases[i] );
				/*strcpy( infosrc, siv_infosrc[l] );*/
				/* return only unspecified PKP and SKS */
				if  (strncmp(phase,"PKP",3) == 0)  phase[3] = '\0';
				if  (strncmp(phase,"PKKP",4) == 0)  phase[4] = '\0';
				if  (strncmp(phase,"SKKP",4) == 0)  phase[4] = '\0';
				if  (strncmp(phase,"SKS",3) == 0)  phase[3] = '\0';
				siv_tol_trav = GpGetFloat( cGpF_idphases_tol_trav );
				return;
			} /*endif*/
			newlist = FALSE;
			i++;
		} /*endwhile*/

		l++;

	} /*endwhile*/

	lists_created = TRUE;

	/* nothing found */
	*status = locstat;
	siv_tol_trav = GpGetFloat( cGpF_idphases_tol_trav );

} /* end of si_identify_phase_step */



/*---------------------------------------------------------------------------*/



static float si_get_maxtol( char phase[] )

/* Returns maximum tolerance time dependent on phase
 *
 * parameters of routine
 * char       phase[];   input; phase to give tolerance time
 */
{
	/* executable code */

	if  (strcmp(phase,"P") == 0)  {
		return 10.0;
	} else if  (strcmp(phase,"Pdiff") == 0)  {
		return 20.0;
	} else if  (strcmp(phase,"PcP") == 0)  {
		return 10.0;
	} else if  (strcmp(phase,"PP") == 0)  {
		return 10.0;
	} else if  (strcmp(phase,"PPP") == 0)  {
		return 20.0;
	} else if  (strcmp(phase,"SP") == 0)  {
		return 30.0;
	} else if  (strncmp(phase,"PKP",3) == 0)  {
		return 15.0;
	} else if  (strcmp(phase,"S") == 0)  {
		return 30.0;
	} else if  (strcmp(phase,"Sdiff") == 0)  {
		return 40.0;
	} else if  (strcmp(phase,"ScS") == 0)  {
		return 40.0;
	} else if  (strcmp(phase,"SS") == 0)  {
		return 40.0;
	} else if  (strncmp(phase,"SKS",3) == 0)  {
		return 50.0;
	} else if  (strcmp(phase,"SSS") == 0)  {
		return 50.0;
	} else {
		return GpGetFloat( cGpF_idphases_tol_trav );
	} /*endif*/

} /* end of si_get_maxtol */



/*---------------------------------------------------------------------------*/


#define ISC_END_DATE "31-Dec-2004"


void si_lookup_agency( char ponset[], char agency[] )

/* returns agency (either EMS or ISC) where a phase with onset time ponset
 * can be looked for.  Currently very simple solution: all phases before
 * ISC_END_DATE are searched at ISC more recent ones on EMSC
 *
 *
 * parameters of routine
 * char       ponset[];     input; phase onset time
 * char       agency[];     output; name of agency
 */
{
	/* local variables */
	TSyStatus locstat;   /* local status */

	/* excutable code */

	locstat = cBcNoError;
	if  (tc_tdiff(ponset,ISC_END_DATE,&locstat) < 0.0)  {
		strcpy( agency, "isc" );
	} else {
		strcpy( agency, "emsc" );
	} /*endif*/

} /* end of si_lookup_agency */



/*---------------------------------------------------------------------------*/



void si_read_table( char fname[], char tablename[], TSyStatus *status )

/* Read a table from a file, e.g. sigma-table for ml
 *
 * parameters of routine
 * char       fname[];     input; name of input file
 * char       tablename[]; input; name of table
 * TSyStatus  *status;     output; return status
 */
{
	/* local variables */
	float    *table;              /* pointer to table */
	int      length;              /* length of table */
	FILE     *fp;                 /* pointer to input file */
	char     line[cBcLineLth+1];  /* current line of file */
	int      i;                   /* line counter */
	float    xval;                /* xvalue, not checked */
	char     locfname[cBcFileLth+1]; /* local filename */
	char     *env;                /* pointer to environment variable */

	/* executable code */

	if  (strcmp(tablename,"ml-sigma") == 0)  {
		table = siv_ml_sigma;
		length = cSiMlSigmaLth;
	} else {
		*status = SIE_UNKNOWN_TABLE;
		err_setcontext( tablename );
		return;
	} /*endif*/

	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		env = (char *)getenv( "SH_INPUTS" );
		if  (env == NULL)  {*status = SIE_OPEN_TABLE; fclose(fp); return;}
		if  (strlen(env)+strlen(fname) > cBcFileLth)  {
			*status = SIE_OPEN_TABLE;
			err_setcontext( fname );
		} /*endif*/
		strcpy( locfname, env );
		strcat( locfname, fname );
		fp = fopen( locfname, "r" );
		if  (fp == NULL)  {
			*status = SIE_OPEN_TABLE;
			err_setcontext( fname );
			return;
		} /*endif*/
	} /*endif*/

	i = 0;
	while  (i < length)  {
		if  (fgets(line,cBcLineLth,fp) == NULL)  {
			*status = SIE_READ_TABLE;
			fclose( fp );
			return;
		} /*endif*/
		if  (*line == '!' || *line == '\n')  continue;
		if  (sscanf( line, "%f %f", &xval, table+i ) != 2)  {
			*status = SIE_READ_TABLE;
			fclose( fp );
			return;
		} /*endif*/
		i++;
	} /*endwhile*/

	fclose( fp );

} /* end of si_read_table */



/*---------------------------------------------------------------------------*/

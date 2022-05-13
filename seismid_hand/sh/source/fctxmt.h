
/* file FCTXMT.H
 *      ========
 *
 * version 17, 22-May-2006
 *
 * prototypes of module SHMATH.C
 * K. Stammler, 16-MAY-1990
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


#ifndef __FCTXMT
#define __FCTXMT

#ifndef __SHCONST
#include "shconst.h"
#endif
#ifndef __GLUSRDEF
#include "glusrdef.h"
#endif


/*--------------------------------------------------------------------------*/


void mt_cspike( float *a, long lth, float dt, float pos, float ampl );

/* creates spike trace, spike at position "pos", amplitude "ampl" */

/* parameters of routine */
/* float		*a;			output; array created */
/* long		lth;			input; length of array */
/* float		dt;			input; sample distance */
/* float		pos;			input; position of peak */
/* float		ampl;			input; amplitude of peak */


/*--------------------------------------------------------------------------*/


void mt_cgauss( float *a, long lth, float dt, float pos,
	float ampl, float width );

/* creates gauss peak of width "width" */

/* parameters of routine */
/* float		*a;			output; array created */
/* long		lth;			input; length of array */
/* float		dt;			input; sample distance */
/* float		pos;			input; position of peak */
/* float		ampl;			input; amplitude of peak */
/* float		width;		input; width of peak */


/*--------------------------------------------------------------------------*/


void mt_cexp( float *a, long lth, float dt, float pos,
	float ampl, float width );

/* creates exponential peak of width "width" */

/* parameters of routine */
/* float		*a;			output; array created */
/* long		lth;			input; length of array */
/* float		dt;			input; sample distance */
/* float		pos;			input; position of peak */
/* float		ampl;			input; amplitude of peak */
/* float		width;		input; width of peak */


/*--------------------------------------------------------------------------*/


void mt_csharp( float *a, long lth, float dt, float pos,
	float ampl, float w0, float w1 );

/* creates time function g(t) = exp(-w0(t-pos)) * sin(w1*(t-pos))/w1
	(explosive point source) */

/* parameters of routine */
/* float		*a;			output; array created */
/* long		lth;			input; length of array */
/* float		dt;			input; sample distance */
/* float		pos;			input; position of peak */
/* float		ampl;			input; amplitude of peak */
/* float		w0;			input; decay */
/* float		w1;			input; sine oscillation frequency */


/*--------------------------------------------------------------------------*/


void mt_crandom( float *a, long lth, float ampl );

/* creates random trace */

/* parameters of routine */
/* float		*a;			output; array created */
/* long		lth;			input; length of array */
/* float		ampl;			input; amplitude of peak */


/*--------------------------------------------------------------------------*/


void mt_ckuepper( float a[], long lth, float dt, float pos, float ampl,
   int m, float width );

/* creates Kuepper impulse
 *
 * parameters of routine
 * float      a[];     output; created trace
 * long       lth;     input; length of trace
 * float      dt;      input; sample distance (in sec.)
 * float      pos;     input; position of impulse
 * float      ampl;    input; amplitude
 * int        m;       input; order of impule
 * float      width;   input; width of impulse
 */


/*--------------------------------------------------------------------------*/


void mt_csin( SAMPLE dat[], long lth, REAL dt, REAL frq, float ampl,
	float phase );

/* creates sine trace
 *
 * parameters of routine
 * SAMPLE     dat[];      output; created trace
 * long       lth;        input; length of trace
 * REAL       dt;         input; sample distance in sec
 * REAL       frq;        input; frequency in Hz
 * float      ampl;       input; amplitude
 * float      phase;      input; phase
 */


/*--------------------------------------------------------------------------*/


void mt_rot2( SAMPLE *n, SAMPLE *e, long lth, REAL angle,
	SAMPLE *r, SAMPLE *t );

/* 2-dimensional rotation
 *
 * parameter of routine
 * SAMPLE    *n, *e;      input; north & east component
 * long      lth;         input; length of input & output traces
 * REAL      angle;       input; rotation angle
 * SAMPLE    *r, *t;      output; rotated traces
 */


/*--------------------------------------------------------------------------*/


void mt_rot3( SAMPLE *z, SAMPLE *n, SAMPLE *e, long lth,
	REAL azim, REAL inci, int type, SAMPLE *l, SAMPLE *q, SAMPLE *t );

/* 2-dimensional rotation
 *
 * parameter of routine
 * SAMPLE    *z, *n, *e;  input; vertical, north & east component
 * long      lth;         input; length of input & output traces
 * REAL      azim, inci;  input; rotation angles
 * int       type;        input; type of rotation
 * SAMPLE    *l, *q, *t;  output; rotated traces
 */


/*--------------------------------------------------------------------------*/


void mt_sum( SAMPLE *ptr[], REAL wgt[], int do_norm, int sumlth,
	long smplth, SAMPLE *res );

/* sums "sumlth" traces with weights "wgt". result is "res"
 *
 * parameters of routine
 * SAMPLE     *ptr[];   input; array of pointers to input traces
 * REAL       wgt[];    input; weights of traces
 * int        do_norm;  input; perform normalisation (divide by #)
 * int        sumlth;   input; number of input traces
 * long       smplth;   input; number of samples per trace
 * SAMPLE     *res;     output; result trace
 */


/*--------------------------------------------------------------------------*/


void mt_sum_phases( SAMPLE *ptr[], REAL wgt[], int sumlth,
	long smplth, SAMPLE *res );

/* sums "sumlth" traces. result is "res"
 *
 * parameters of routine
 * SAMPLE     *ptr[];   input; array of pointers to input traces
 * REAL       wgt[];    input; weights of traces
 * int        sumlth;   input; number of input traces
 * long       smplth;   input; number of samples per trace
 * SAMPLE     *res;     output; result trace
 */


/*--------------------------------------------------------------------------*/


void mt_stddev( SAMPLE *ptr[], int sumlth, long smplth, SAMPLE *res );

/* computes mean deviation trace of input traces "ptr"
 *
 * parameters of routine
 * SAMPLE     *ptr[];   input; array of pointers to input traces
 * int        sumlth;   input; number of input traces
 * long       smplth;   input; number of samples per trace
 * SAMPLE     *res;     output; result trace
 */


/*--------------------------------------------------------------------------*/


void mt_mulcmplx( COMPLEX *res, COMPLEX *fac1, COMPLEX *fac2 );

/* multiplies two complex numbers
 *
 * parameters of routine
 * COMPLEX    *rex;         output; result of multiplication
 * COMPLEX    *fac1, *fac2; input; factors
 */


/*--------------------------------------------------------------------------*/


void mt_divcmplx( COMPLEX *res, COMPLEX *num, COMPLEX *denom );

/* divides two complex numbers (*res = *num / *denom)
 *
 * parameters of routine
 * COMPLEX    *res;         output; result of multiplication
 * COMPLEX    *num, *denom; input; numerator & denominator
 */


/*--------------------------------------------------------------------------*/


void mt_rmulcmplx( COMPLEX *res, COMPLEX *fac1, REAL fac2 );

/* multiplies a complex number with a real number
 *
 * parameters of routine
 * COMPLEX    *rex;         output; result of multiplication
 * COMPLEX    *fac1;        input; complex factor
 * REAL       fac2;         input; real factor
 */


/*--------------------------------------------------------------------------*/


void mt_imexp( COMPLEX *res, REAL im_arg );

/* imaginary exponent *res = exp( i * phi )
 *
 * parameters of routine
 * COMPLEX    *res;    output; result of exponentiation
 * REAL       *im_arg; input; imaginary exponent
 */


/*--------------------------------------------------------------------------*/

#ifdef XXX
void mt_fft( long lx, COMPLEX cx[], REAL signi );

/* fast fourier transformation of array "cx" of length "lx"
 * Source code was transferred directly from an old-fashioned
 * FORTRAN routine.
 *
 * parameters of routine
 * long       lx;    input; length of complex array
 * COMPLEX    cx[];  modify; array to be fourier transformed
 * REAL       signi; input; sign of transformation
 */
#endif /* XXX */

/*--------------------------------------------------------------------------*/


void mt_deg_to_km( int listlth, STATLOC lat[], STATLOC lon[],
	REAL x[], REAL y[] );

/* coordinate transformation from earths surface (lat,lon) to
 * plane coo-system (x,y).  This approximation is permitted only
 * if the stations are close together compared with the earths
 * radius
 *
 * parameters of routine
 * int        listlth;     input; length of arrays
 * STATLOC    lat[];       input; surface coordinates (degrees)
 * REAL       x[], y[];    output; plane coordinates (km)
 */


/*--------------------------------------------------------------------------*/


void mt_locate( int listlth, REAL time[], REAL x[], REAL y[],
	REAL dt, REAL *azimuth, REAL *azimerr, REAL *slowness,
	REAL *slowerr, int *status );

/* Computes azimuth and slowness of an event from arrival times
 * at different locations.  Returns also an average deviation
 * for both results.
 *
 * parameters of routine
 * int        listlth;     input; number of locations
 * REAL       time[];      modify; in: relative time, out: scratch
 * REAL       x[];         modify; in: x-location (km), out: scratch
 * REAL       y[];         modify; in: y-location (km), out: scratch
 * REAL       dt;          input; sample distance in seconds
 * REAL       *azimuth;    output; computed azimuth
 * REAL       *azimerr;    output; average deviation of azimuth
 * REAL       *sloness;    output; computed slowness
 * REAL       *slowerr;    output; average deviation of slowness
 * int        *status;     output; return status
 */


/*--------------------------------------------------------------------------*/


void mt_locate_old( int listlth, REAL time[], REAL x[], REAL y[],
	REAL dt, REAL *azimuth, REAL *azimerr, REAL *slowness,
	REAL *slowerr, int *status );

/* Computes azimuth and slowness of an event from arrival times
 * at different locations.  Returns also an average deviation
 * for both results.
 *
 * This the old version with a station as reference point.  Gives inaccurate
 * results.
 *
 * parameters of routine
 * int        listlth;     input; number of locations
 * REAL       time[];      modify; in: relative time, out: scratch
 * REAL       x[];         modify; in: x-location (km), out: scratch
 * REAL       y[];         modify; in: y-location (km), out: scratch
 * REAL       dt;          input; sample distance in seconds
 * REAL       *azimuth;    output; computed azimuth (in degrees)
 * REAL       *azimerr;    output; average deviation of azimuth
 * REAL       *sloness;    output; computed slowness (in sec/degree)
 * REAL       *slowerr;    output; average deviation of slowness
 * int        *status;     output; return status
 */


/*--------------------------------------------------------------------------*/


void mt_locate_svd( int listlth, REAL time[], REAL x[], REAL y[],
	REAL dt, REAL *azimuth, REAL *azimerr, REAL *slowness,
	REAL *slowerr, int *status );

/* Computes azimuth and slowness of an event from arrival times
 * at different locations.  Returns also an average deviation
 * for both results.
 *
 * parameters of routine
 * int        listlth;     input; number of locations
 * REAL       time[];      modify; in: relative time, out: scratch
 * REAL       x[];         modify; in: x-location (km), out: scratch
 * REAL       y[];         modify; in: y-location (km), out: scratch
 * REAL       dt;          input; sample distance in seconds
 * REAL       *azimuth;    output; computed azimuth (in degrees)
 * REAL       *azimerr;    output; average deviation of azimuth
 * REAL       *slowness;   output; computed slowness (in sec/degree)
 * REAL       *slowerr;    output; average deviation of slowness
 * int        *status;     output; return status
 */


/*--------------------------------------------------------------------------*/


void mt_locate_old_error( int listlth, REAL time[], REAL x[], REAL y[],
	REAL azimuth, REAL slowness, REAL *resid );

/* Prints deviations of mt_locate_old and station residuals
 *
 * parameters of routine
 * int        listlth;     input; number of locations
 * REAL       time[];      modify; in: relative time, out: scratch
 * REAL       x[];         modify; in: x-location (km), out: scratch
 * REAL       y[];         modify; in: y-location (km), out: scratch
 * REAL       azimuth;     input; computed azimuth (in degrees)
 * REAL       slowness;    input; computed slowness (in sec/degree)
 * REAL       *resid;      output; station residuals (if not NULL)
 */


/*--------------------------------------------------------------------------*/


void mt_beamshift( int listlth, REAL x[], REAL y[], REAL azimuth,
	REAL slowness, REAL delay[] );

/* computes delays of arrival times according to the location (x,y)
 * (in km) and the given azimuth and slowness
 *
 * parameters of routine
 * int        listlth;      input; number of locations
 * REAL       x[], y[];     input; locations (in km, relative to reference point)
 * REAL       azimuth;      input; azimuth of event (in degrees)
 * REAL       slowness;     input; slowness of event
 * REAL       delay[];      output; computed delays
 */


/*--------------------------------------------------------------------------*/


void mt_fold( long la, REAL a[], long lb, REAL b[], REAL c[] );

/* folds traces a[0..la-1] and b[0..lb-1] to trace c[0..la+lb-2]
 *
 * parameters of routine
 * long       la;       input; length of trace a
 * REAL       a[];      input; trace a
 * long       lb;       input; length of trace b
 * REAL       b[];      input; trace b
 * REAL       c[];      output; result trace (length la+lb-1)
 */


/*----------------------------------------------------------------------------*/


REAL mt_random( REAL ampl );

/* creates uniform random number
 *
 * parameter of routine
 * float     ampl;     input; maximum amplitude
 */


/*----------------------------------------------------------------------------*/


REAL mt_gauss_random( REAL width );

/* creates normal distributed random number
 *
 * parameter of routine
 * REAL      width;     input; width of distribution
 */


/*----------------------------------------------------------------------------*/


void mt_randomstr( int lth, char str[] );

/* creates random string of digits of length "lth"
 *
 * parameters of routine
 * int        lth;       input; length of output string
 * char       str[];     output; random string
 */


/*----------------------------------------------------------------------------*/


void mt_despike( REAL smp[], long lth, REAL critmul, FILE *log,
	long *scnt, long *dcnt );

/* removes spikes from array "smp".  A spike is recognized if the
 * difference between smp[i] and smp[i-1] is larger then "critmul"
 * times the average distance.
 *
 * parameters of routine
 * REAL       smp[];       modify; array to be despiked
 * long       lth;         input; length of array
 * REAL       critmul;     input; detection factor
 * FILE       *log         input; log (if NULL no log)
 * long       *scnt;       output; number of spikes detected
 * long       *dcnt;       output; number of discontinuities (not removed)
 */


/*----------------------------------------------------------------------------*/


void mt_mend( SAMPLE smp[], long lth, long loff, long roff, int w,
	REAL dt, BOOLEAN pol, STATUS *status );

/* mends trace
 *
 * parameters of routine
 * SAMPLE     smp[];     modify; trace to be mended
 * long       lth;       input; length of trace
 * long       loff;      input; left offset
 * long       roff;      input; right offset
 * int        w;         input; widening of samples
 * REAL       dt;        input; sample distance
 * BOOLEAN    pol;       input; polynomial or rational interpolation
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void mt_levinson( SAMPLE r[], SAMPLE g[], SAMPLE f[], int m, STATUS *status );

/* Levinson algorithm for symmetric toeplitz-matrix "r[0..m-1]".
 *
 * parameters of routine
 * SAMPLE     r[];     input; symmetric toeplitz-matrix r[0..m-1]
 * SAMPLE     g[];     input; correlation of input with desired trace
 * SAMPLE     f[];     output; computed inverse filter
 * int        m;       input; length of all traces
 * STATUS     *status; output; return status
 */


/*----------------------------------------------------------------------------*/


void mt_arp( int order, REAL coeff[], REAL start[], SAMPLE dat[], long lth );

/* generates autoregressive process of order "order":
 * dat[k]  = sum{l=1,order}[coeff[l]*dat[k-l]] + dat[k]
 *
 * parameters of routine
 * int        order;     input; order of process
 * REAL       coeff[];   input; coefficients of process
 * REAL       start[];   input; start values
 * SAMPLE     dat[];     modify; trace to be processed
 * long       lth;       input; length of data array
 */


/*----------------------------------------------------------------------------*/


void mt_multiply_array( SAMPLE s1[], SAMPLE s2[], long lth, SAMPLE out[] );

/* multiplies two arrays and stres result in "out"
 *
 * parameters of routine
 * SAMPLE     s1[], s2[];    input; input arrays
 * long       lth;           input; length of all (input and output) arrays
 * SAMPLE     out[];         output; result
 */


/*----------------------------------------------------------------------------*/


void mt_divide_array( SAMPLE s1[], SAMPLE s2[], long lth, SAMPLE out[] );

/* divides two arrays and stores result in "out"
 *
 * parameters of routine
 * SAMPLE     s1[], s2[];    input; input arrays
 * long       lth;           input; length of all (input and output) arrays
 * SAMPLE     out[];         output; result
 */


/*----------------------------------------------------------------------------*/


void mt_locdiff( REAL lat1, REAL lon1, REAL lat2, REAL lon2,
	REAL *dist, REAL *azim );

/* returns distance in degrees and azimuth of two locations given
 * by (lat1,lon1) and (lat2,lon2)
 *
 * parameters of routine
 * REAL       lat1, lon1;    input; first location in degrees
 * REAL       lat2, lon2;    input; second location in degrees
 * REAL       *dist;         output; distance in degrees
 * REAL       *azim;         output; azimuth from loc1 to loc2 (in deg)
 */


/*----------------------------------------------------------------------------*/


void mt_locadd( REAL lat, REAL lon, REAL dist, REAL azim,
	REAL *latx, REAL *lonx );

/* computes location (*latx,*lonx) from point (lat,lon) and
 * a distance "dist" in direction "azim"
 *
 * parameters of routine
 * REAL       lat, lon;      input; input location in degrees
 * REAL       dist;          input; distance in degrees
 * REAL       azim;          input; direction
 * REAL       *latx, *lonx;  output; output location
 */


/*----------------------------------------------------------------------------*/


void mt_fit_gauss( REAL x[], SAMPLE y[], int ndata, REAL ampl,
	int maxiter, REAL *pos, REAL *width, int *iter, STATUS *status );

/* fits a gauss function of position "*pos" and width "*width" to 
 * the data given in x[0..ndata-1] and y[0..ndata-].
 *
 * parameters of routine
 * REAL       x[], y[];      input; data to be fitted
 * int        ndata;         input; number of samples in x and y
 * REAL       ampl;          input; amplitude of gauss peak
 * int        maxiter;       input; maximum number of iterations
 * REAL       *pos;          modify; position of gauss peak (input is start)
 * REAL       *width;        modify; half-width of gauss peak (input is start)
 * int        *iter;         output; number of iterations
 * STATUS     *status;       output; return status
 */


/*----------------------------------------------------------------------------*/


void mt_average_smoothing( SAMPLE inp[], long trclth, int avlth, SAMPLE out[] );

/* averages input trace, takes "avlth" samples and takes mean as new output
 * sample, then moves average window by 1 sample
 *
 * parameters of routine
 * SAMPLE     inp[];         input; input array
 * long       trclth;        input; length of input/output traces in samples
 * int        avlth;         input; length of average window in samples
 * SAMPLE     out[];         output; output array
 */


/*----------------------------------------------------------------------------*/


void mt_elevation_correction( REAL slowness, int lth, REAL elev[],
	REAL veloc[], REAL shift[], STATUS *status );

/* applies corrections to given shift times according to station elevation
 *
 * parameters of routine
 * REAL       slowness;  input; slowness in sec/deg
 * int        lth;       input; length of arrays
 * REAL       elev[];    input; station elevation in m
 * REAL       veloc[];   input; average velocities at stations in km/sec
 * REAL       shift[];   modify; shift time to be corrected
 * STATUS     *status;   output; return status
 */


/*----------------------------------------------------------------------------*/


void mt_find_gap( int cnt_limit, SAMPLE dat[], long lth, char inf[],
	char start[], REAL dt, FILE *log );

/* finds zero-valued gaps in passed trace "dat".  If found it logs it to file
 *
 * parameters of routine
 * int        cnt_limit;   input; minimum number of equal samples for error
 * SAMPLE     dat[];       input; trace to look for gaps
 * long       lth;         input; length of trace in samples
 * char       inf[];       input; info text
 * char       start[];     input; start time of trace
 * REAL       dt;          input; sample distance of trace
 * FILE       *log;        input; pointer to log file
 */

/*----------------------------------------------------------------------------*/


void mt_fix_gap( SAMPLE dat[], long lth, REAL const_value, BOOLEAN konst, 
               BOOLEAN one);

/* fix constant(zero)-valued gaps in passed trace "dat"  by straight
 * line interpolation or arithmetic mean of edge values
 *
 * parameters of routine
 * SAMPLE     dat[];       input; trace to look for gaps
 * long       lth;         input; length of trace in samples
 * REAL       const_value; input; constant value to find (0.0)
 * BOOLEAN    konst;       input; introduce constant values
 * BOOLEAN    one;         input; replace even single samples
 */

/*--------------------------------------------------------------------------*/

#endif /* __FCTXMT */

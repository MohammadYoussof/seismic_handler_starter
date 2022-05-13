
/* file SHMATH.C
 *      ========
 *
 * version 34, 22-May-2006
 *
 * mathematical subroutines
 * K. Stammler, 16-MAY-1990
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_TCUSRDEF
#include "shconst.h"
#include "shvars.h"
#include "glusrdef.h"
#include "fctxmt.h"
#include "numres.h"
#include "sherrors.h"

#define SQRT3_2 0.8660254
	/* half of square root of 3 */
#define SQRT3 1.7320508
	/* square root of 3 */

#define SQRT1_3 5.77350269e-1
#define SQRT1_2 7.07106781e-1
#define SQRT2_3 8.16496581e-1


/* prototypes of local routines */
static void mth_init_seed( void );
static void mth_eval_gauss( REAL x, REAL a[], SAMPLE *yfit,
	SAMPLE dyda[], int ma );



/* global variables */
static long     mtv_seed;               /* random seed number */
static BOOLEAN  mtv_seed_is_init=FALSE; /* seed is initialised */
static REAL     mtv_fitampl;            /* amplitude of fit functions */


/*--------------------------------------------------------------------------*/



void mt_cspike( float *a, long lth, float dt, float pos, float ampl )

/* creates spike trace, spike at position "pos", amplitude "ampl" */

/* parameters of routine */
/* float      *a;         output; array created */
/* long      lth;         input; length of array */
/* float      dt;         input; sample distance */
/* float      pos;         input; position of peak */
/* float      ampl;         input; amplitude of peak */

{
	/* local variables */
	long      i;         /* counter */
	float      *b;      /* peak pointer */

	/* executable code */

	b = a + (long)(pos/dt);
	for  (i=0;i<lth;i++)  *a++ = 0.0;
	*b = ampl;

} /* end of mt_cspike */



/*--------------------------------------------------------------------------*/



void mt_cgauss( float *a, long lth, float dt, float pos,
	float ampl, float width )

/* creates gauss peak of width "width" */

/* parameters of routine */
/* float      *a;         output; array created */
/* long      lth;         input; length of array */
/* float      dt;         input; sample distance */
/* float      pos;         input; position of peak */
/* float      ampl;         input; amplitude of peak */
/* float      width;      input; width of peak */

{
	/* local variables */
	long      i;         /* counter */
	float      t;         /* current x-position */

	/* executable code */

	width *= width;
	for  (i=0;i<lth;i++)  {
		t = (float)i * dt - pos;
		*a++ = ampl * exp( -t*t / (2.*width) );
	} /*endfor*/

} /* end of mt_cgauss */



/*--------------------------------------------------------------------------*/



void mt_cexp( float *a, long lth, float dt, float pos,
	float ampl, float width )

/* creates exponential peak of width "width" */

/* parameters of routine */
/* float      *a;         output; array created */
/* long      lth;         input; length of array */
/* float      dt;         input; sample distance */
/* float      pos;         input; position of peak */
/* float      ampl;         input; amplitude of peak */
/* float      width;      input; width of peak */

{
	/* local variables */
	long      i;         /* counter */
	long      start;   /* peak position in samples */
	float      t;         /* current x-position */

	/* executable code */

	start = Nlong( pos/dt );
	if  (start > lth)  start = lth;

	for  (i=0;i<start;i++)  *a++ = 0;

	for  (i=start;i<lth;i++)  {
		t = (float)i * dt - pos;
		*a++ = ampl * exp( -t / width );
	} /*endfor*/

} /* end of mt_cexp */



/*--------------------------------------------------------------------------*/



void mt_csharp( float *a, long lth, float dt, float pos,
	float ampl, float w0, float w1 )

/* creates time function g(t) = exp(-w0(t-pos)) * sin(w1*(t-pos))/w1
	(explosive point source) */

/* parameters of routine */
/* float      *a;         output; array created */
/* long       lth;        input; length of array */
/* float      dt;         input; sample distance */
/* float      pos;        input; position of peak */
/* float      ampl;       input; amplitude of peak */
/* float      w0;         input; decay */
/* float      w1;         input; sine oscillation frequency */

{
	/* local variables */
	long     i;       /* counter */
	long     start;   /* peak position in samples */
	float    t;       /* current x-position */
	float    max;     /* maximum */
	float    *acop;   /* copy of a */

	/* executable code */

	max = 0.0;
	start = Nlong( pos/dt );
	if  (start > lth)  start = lth;

	for  (i=0;i<start;i++)  *a++ = 0.;

	acop = a;
	for  (i=start;i<lth;i++)  {
		t = (float)i * dt - pos;
		*a = exp(-w0*t) * sin(w1*t)/w1;
		if  (Abs(*a) > max)  max = *a;
		a++;
	} /*endfor*/

	max = ampl / max;
	for  (i=start;i<lth;i++)
		*acop++ *= max;

} /* end of mt_csharp */



/*--------------------------------------------------------------------------*/



void mt_crandom( float *a, long lth, float ampl )

/* creates random trace */

/* parameters of routine */
/* float      *a;         output; array created */
/* long       lth;        input; length of array */
/* float      ampl;       input; amplitude of peak */

{
	/* local variables */
	long      i;         /* counter */

	/* executable code */

	mt_random( 0.0 );  /* new seed */
	for  (i=0;i<lth;i++)  *a++ = mt_random( ampl );

} /* end of mt_crandom */



/*--------------------------------------------------------------------------*/



void mt_ckuepper( float a[], long lth, float dt, float pos, float ampl,
   int m, float width )

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
{
	/* local variables */
	float    *acop;    /* pointer copy */
	float    t;        /* time */
	long     i;        /* counter */
	long     start;    /* start index */
	long     end;      /* end index */
	float    max;      /* maximum value */

	/* executable code */

	start = Nlong( pos/dt );
	if  (start > lth)  start = lth;
	end = Nlong( (pos+width)/dt );
	if  (end > lth)  end = lth;

	for  (i=0;i<start;i++)  *a++ = 0.;
	acop = a;
	max = 0.;
	for  (i=start;i<end;i++)  {
		t = (float)(i-start) * dt;
		*a = sin((float)m*SHC_PI/width*t) - (float)m/(float)(m+2) *
			sin((float)(m+2)*SHC_PI/width*t);
		if  (Abs(*a) > max)  max = Abs(*a);
		a++;
	} /*endfor*/
	for  (i=end;i<lth;i++)  *a++ = 0.;
	if  (max > 0.)  max = ampl/max;

	a = acop;
	for  (i=start; i<end; i++)  *a++ *= max;

} /* end of mt_ckuepper */



/*--------------------------------------------------------------------------*/



void mt_csin( SAMPLE dat[], long lth, REAL dt, REAL frq, float ampl,
	float phase )

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
{
	/* local variables */
	SAMPLE   *a;       /* moving pointer */
	REAL     t;        /* current time */

	/* executable code */

	phase /= SHC_RAD_TO_DEG;
	frq *= 2.*SHC_PI;
	t = 0.;
	a = dat;
	dat += lth;
	while  (a < dat)  {
		*a++ = ampl * sin( frq*t + phase );
		t += dt;
	} /*endwhile*/

} /* end of mt_csin */



/*--------------------------------------------------------------------------*/



void mt_rot2( SAMPLE *n, SAMPLE *e, long lth, REAL angle,
	SAMPLE *r, SAMPLE *t )

/* 2-dimensional rotation
 *
 * parameter of routine
 * SAMPLE    *n, *e;      input; north & east component
 * long      lth;         input; length of input & output traces
 * REAL      angle;       input; rotation angle
 * SAMPLE    *r, *t;      output; rotated traces
 */
{
	/* local variables */
	REAL     ann, ane, aen, aee;  /* rotation matrix */
	long     i;                   /* counter */

	/* executable code */

	angle *= (SHC_PI / 180.);
	ann = cos( angle );
	ane = -sin( angle );
	aen = -ane;
	aee = ann;

	for  (i=0;i<lth;i++)  {
		*r++ = ann * *n + ane * *e;
		*t++ = aen * *n + aee * *e;
		e++; n++;
	} /*endfor*/

} /* end of mt_rot2 */



/*--------------------------------------------------------------------------*/



void mt_rot3( SAMPLE *z, SAMPLE *n, SAMPLE *e, long lth,
	REAL azim, REAL inci, int type, SAMPLE *l, SAMPLE *q, SAMPLE *t )

/* 3-dimensional rotation
 *
 * parameter of routine
 * SAMPLE    *z, *n, *e;  input; vertical, north & east component
 * long      lth;         input; length of input & output traces
 * REAL      azim, inci;  input; rotation angles
 * int       type;        input; type of rotation
 * SAMPLE    *l, *q, *t;  output; rotated traces
 */
{
	/* local variables */
	REAL     azz, azn, aze;  /* rotation matrix */
	REAL     anz, ann, ane;
	REAL     aez, aen, aee;
	REAL     cosi, cosa, sini, sina;
	long     i;                   /* counter */

	/* executable code */

	azim *= (SHC_PI / 180.);
	cosa = cos( azim );
	sina = sin( azim );
	if  (type == SHC_ROT_ZNE_TO_LQT)  {
		inci *= (SHC_PI / 180.);
		cosi = cos( inci );
		sini = sin( inci );
	} /*endif*/

	if  (type == SHC_ROT_ZNE_TO_LQT)  {
		azz = cosi;         azn = -sini*cosa;     aze = -sini*sina;
		anz = sini;         ann =  cosi*cosa;     ane =  cosi*sina;
		aez = 0.0;          aen =  sina;          aee = -cosa;
	} else if  (type == SHC_ROT_ZNE_TO_UVW)  {
#		ifdef XXX
		if  (sina != 0.0)   sina = 1.0 / sina;
		if  (cosa != 0.0)   cosa = 1.0 / cosa;
		azz = cosa/3.0;     anz = cosa/3.0;       aez = cosa/3.0;
		azn = 0.;           ann = sina/SQRT3;     aen = -sina/SQRT3;
		aze = -sina*2.0/3.0;ane = sina/3.0;       aee = sina/3.0;
#		endif
		azz = SQRT1_3;      anz = SQRT1_3;        aez = SQRT1_3;
		azn = 0.0;          ann = SQRT1_2;        aen = -SQRT1_2;
		aze = -SQRT2_3;     ane = 0.5*SQRT2_3;    aee = 0.5*SQRT2_3;
	} else if  (type == SHC_ROT_UVW_TO_ZNE)  {
#		ifdef XXX
		azz = cosa;         anz = cosa;           aez = cosa;
		azn = 0.;           ann = sina*SQRT3_2;   aen = -sina*SQRT3_2;
		aze = -sina;        ane = sina*0.5;       aee = sina*0.5;
#		endif
		azz = SQRT1_3;      azn = SQRT1_3;        aze = SQRT1_3;
		anz = 0.0;          ann = SQRT1_2;        ane = -SQRT1_2;
		aez = -SQRT2_3;     aen = 0.5*SQRT2_3;    aee = 0.5*SQRT2_3;
	} else {
		azz = azn = aze = anz = ann = ane = aez = aen = aee = 0.0;
	} /*endif*/

	for  (i=0;i<lth;i++)  {
		*l++ = azz * *z  +  azn * *n  +  aze * *e;
		*q++ = anz * *z  +  ann * *n  +  ane * *e;
		*t++ = aez * *z  +  aen * *n  +  aee * *e;
		z++; n++; e++;
	} /*endfor*/

} /* end of mt_rot3 */



/*--------------------------------------------------------------------------*/



void mt_sum( SAMPLE *ptr[], REAL wgt[], int do_norm, int sumlth,
	long smplth, SAMPLE *res )

/* sums "sumlth" traces. result is "res"
 *
 * parameters of routine
 * SAMPLE     *ptr[];   input; array of pointers to input traces
 * REAL       wgt[];    input; weights of traces
 * int        do_norm;  input; normalise traces (divide by # of trcs)
 * int        sumlth;   input; number of input traces
 * long       smplth;   input; number of samples per trace
 * SAMPLE     *res;     output; result trace
 */
{
	/* local variables */
	long     s;          /* sample counter */
	int      t;          /* trace counter */
	SAMPLE   *src, *dst; /* moving pointers */
	REAL     weight;     /* weight of current trace */
	REAL     norm;       /* normalisation */

	/* executable code */

	norm = (do_norm) ? (float)sumlth : 1.0;

	/* first sum trace */
	if  (sumlth <= 0)  return;
	src = ptr[0];
	dst = res;
	weight = wgt[0] / norm;
	for  (s=0;s<smplth;s++)
		*dst++ = *src++ * weight;

	/* rest of sum traces */
	for  (t=1;t<sumlth;t++)  {
		src = ptr[t];
		dst = res;
		weight = wgt[t] / norm;
		for  (s=0;s<smplth;s++)  {
			*dst++ +=  *src++ * weight;
		} /*endfor*/
		sy_sharecpu();
	} /*endfor*/

} /* end of mt_sum */



/*--------------------------------------------------------------------------*/



void mt_sum_phases( SAMPLE *ptr[], REAL wgt[], int sumlth,
	long smplth, SAMPLE *res )

/* sums "sumlth" traces. result is "res"
 *
 * parameters of routine
 * SAMPLE     *ptr[];   input; array of pointers to input traces
 * REAL       wgt[];    input; weights of traces
 * int        sumlth;   input; number of input traces
 * long       smplth;   input; number of samples per trace
 * SAMPLE     *res;     output; result trace
 */
{
	/* local variables */
	long     s;          /* sample counter */
	int      t;          /* trace counter */
	SAMPLE   *dst;       /* moving pointer */
	REAL     norm;       /* normalisation */
	REAL     re, im;     /* real and imaginary part */

	/* executable code */

	norm = (float)sumlth;

	/* loop all samples */
	if  (sumlth <= 0)  return;
	dst = res;
	for  (s=0; s<smplth; s++)  {
		/* loop traces */
		re = im = 0.0;
		for  (t=0; t<sumlth; t++)  {
			re += cos( ptr[t][s] / SHC_RAD_TO_DEG ) * wgt[t];
			im += sin( ptr[t][s] / SHC_RAD_TO_DEG ) * wgt[t];
		} /*endfor*/
		re /= norm;
		im /= norm;
		*dst++ = sqrt( re*re + im*im );
	} /*endfor*/

} /* end of mt_sum_phases */



/*--------------------------------------------------------------------------*/



void mt_stddev( SAMPLE *ptr[], int sumlth, long smplth, SAMPLE *res )

/* computes mean deviation trace of input traces "ptr"
 *
 * parameters of routine
 * SAMPLE     *ptr[];   input; array of pointers to input traces
 * int        sumlth;   input; number of input traces
 * long       smplth;   input; number of samples per trace
 * SAMPLE     *res;     output; result trace
 */
{
	/* local variables */
	long     s;          /* sample counter */
	int      t;          /* trace counter */
	SAMPLE   *src, *dst; /* moving pointers */
	REAL     norm;       /* normalisation */
	SAMPLE   *mean;      /* mean value */
	SAMPLE   *mp;        /* mean pointer */
	SAMPLE   tmp;        /* scratch */
	STATUS   locstat=SHE_NOERROR;

	/* executable code */

	mean = sy_allocmem( smplth, (int)sizeof(SAMPLE), &locstat );
	if  (Severe(&locstat))  return;

	/* compute mean value */
	if  (sumlth <= 1)  return;
	norm = 1.0 / (float)sumlth;
	/* first sum trace */
	src = ptr[0];
	dst = mean;
	for  (s=0;s<smplth;s++)
		*dst++ = *src++;
	/* rest of sum traces */
	for  (t=1;t<sumlth;t++)  {
		src = ptr[t];
		dst = mean;
		for  (s=0;s<smplth;s++)  {
			*dst++ +=  *src++;
		} /*endfor*/
		sy_sharecpu();
	} /*endfor*/
	dst = mean;
	for  (s=0;s<smplth;s++)
		*dst++ *= norm;

	/* norm = 1.0 / (float)(sumlth-1); */ /* standard deviation */
	norm = 1.0 / (float)((sumlth-1)*sumlth);  /* standard error */
	/* first stddev trace */
	src = ptr[0];
	dst = res;
	mp = mean;
	for  (s=0;s<smplth;s++)  {
		tmp = *src++ - *mp++;
		*dst++ = tmp*tmp;
	} /*endfor*/
	/* rest of stddev traces */
	for  (t=1;t<sumlth;t++)  {
		src = ptr[t];
		dst = res;
		mp = mean;
		for  (s=0;s<smplth;s++)  {
			tmp = *src++ - *mp++;
			*dst++ += tmp*tmp;
		} /*endfor*/
	} /*endfor*/
	dst = res;
	for  (s=0;s<smplth;s++)  {
		*dst = sqrt( (*dst)*norm );
		dst++;
	} /*endfor*/

	sy_deallocmem( mean );

} /* end of mt_stddev */



/*--------------------------------------------------------------------------*/



void mt_mulcmplx( COMPLEX *res, COMPLEX *fac1, COMPLEX *fac2 )

/* multiplies two complex numbers
 *
 * parameters of routine
 * COMPLEX    *res;         output; result of multiplication
 * COMPLEX    *fac1, *fac2; input; factors
 */
{
	/* executable code */

	res->re = fac1->re * fac2->re - fac1->im * fac2->im;
	res->im = fac1->re * fac2->im + fac1->im * fac2->re;

} /* end of mt_mulcmplx */



/*--------------------------------------------------------------------------*/



void mt_divcmplx( COMPLEX *res, COMPLEX *num, COMPLEX *denom )

/* divides two complex numbers (*res = *num / *denom)
 *
 * parameters of routine
 * COMPLEX    *res;         output; result of multiplication
 * COMPLEX    *num, *denom; input; numerator & denominator
 */
{
	/* local variables */
	REAL     absval;    /* squared absolute value of denominator */

	/* executable code */

	absval = denom->re * denom->re + denom->im * denom->im;
	res->re = (num->re * denom->re + num->im * denom->im) / absval;
	res->im = (num->im * denom->re - num->re * denom->im) / absval;

} /* end of mt_divcmplx */



/*--------------------------------------------------------------------------*/



void mt_rmulcmplx( COMPLEX *res, COMPLEX *fac1, REAL fac2 )

/* multiplies a complex number with a real number
 *
 * parameters of routine
 * COMPLEX    *res;         output; result of multiplication
 * COMPLEX    *fac1;        input; complex factor
 * REAL       fac2;         input; real factor
 */
{
	/* executable code */

	res->re = fac1->re * fac2;
	res->im = fac1->im * fac2;

} /* end of mt_rmulcmplx */



/*--------------------------------------------------------------------------*/



void mt_imexp( COMPLEX *res, REAL im_arg )

/* imaginary exponent *res = exp( i * phi )
 *
 * parameters of routine
 * COMPLEX    *res;    output; result of exponentiation
 * REAL       *im_arg; input; imaginary exponent
 */
{
	/* executable code */

	res->re = cos( im_arg );
	res->im = sin( im_arg );

} /* end of mt_imexp */



/*--------------------------------------------------------------------------*/

#ifdef XXX

void mt_fft( long lx, COMPLEX cx[], REAL signi )

/* fast fourier transformation of array "cx" of length "lx"
 * Source code was transferred directly from an old-fashioned
 * FORTRAN routine.
 *
 * parameters of routine
 * long       lx;    input; length of complex array
 * COMPLEX    cx[];  modify; array to be fourier transformed
 * REAL       signi; input; sign of transformation
 */
{
	/* local variables */
	long     i, j, m, l;   /* counters */
	long     istep;
	REAL     sc;
	COMPLEX  cw, ctemp;

	/* executable code */

	cx--;   /* fortan array start with 1 */

	j = 1;
	sc = sqrt( 1.0 / (float)lx );
	for  (i=1; i<=lx; i++)  {
		if  (i > j)  goto m2;
		mt_rmulcmplx( &ctemp, cx+j, sc );
		mt_rmulcmplx( cx+j, cx+i, sc );
		cx[i] = ctemp;
m2:   m = lx/2;
m3:   if  (j <= m)  goto m5;
		j -= m;
		m /= 2;
		if  (m >= 1)  goto m3;
m5:   j += m;
	} /*endfor*/
	l = 1;
m6: istep = 2*l;
	for  (m=1; m<=l; m++)  {
		mt_imexp( &cw, (SHC_PI * signi * (float)(m-1) / (float)l) );
		for  (i=m; i<=lx; i += istep)  {
			mt_mulcmplx( &ctemp, &cw, cx+i+l );
			cx[i+l].re = cx[i].re - ctemp.re;
			cx[i+l].im = cx[i].im - ctemp.im;
			cx[i].re += ctemp.re;
			cx[i].im += ctemp.im;
		} /*endfor*/
	} /*endfor*/
	l = istep;
	if  (l < lx)  goto m6;

} /* end of mt_fft */

#endif /* XXX */

/*--------------------------------------------------------------------------*/



void mt_deg_to_km( int listlth, STATLOC lat[], STATLOC lon[],
	REAL x[], REAL y[] )

/* coordinate transformation from earths surface (lat,lon) to
 * plane coo-system (x,y).  This approximation is permitted only
 * if the stations are close together compared with the earths
 * radius
 *
 * parameters of routine
 * int        listlth;     input; length of arrays
 * STATLOC    lat[];       input; surface coordinates (degrees)
 * STATLOC    lon[];       input;
 * REAL       x[], y[];    output; plane coordinates (km)
 */
{
	/* local variables */
	STATLOC  ref_lat, ref_lon;       /* reference location */

	/* executable code */

#ifdef XXX
	/* this was changed at 20-Dec-2004 */
	/* reference location is first element */
	ref_lat = *lat;
	ref_lon = *lon;
#endif
	/* reference location is last element */
	ref_lat = lat[listlth-1];
	ref_lon = lon[listlth-1];

	while  (listlth-- > 0)  {
		*x++ = (REAL)((*lon++ - ref_lon) * 
			SHC_DEG_TO_KM * cos((*lat)/SHC_RAD_TO_DEG));
		*y++ = (REAL)((*lat++ - ref_lat) * SHC_DEG_TO_KM);
	} /*endwhile*/

} /* end of mt_deg_to_km */



/*--------------------------------------------------------------------------*/

/* #define SH_DEBUG */

void mt_locate( int listlth, REAL time[], REAL x[], REAL y[],
	REAL dt, REAL *azimuth, REAL *azimerr, REAL *slowness,
	REAL *slowerr, int *status )

/* Computes azimuth and slowness of an event from arrival times
 * at different locations.  Returns also an average deviation
 * for both results.
 *
 * Formula used:
 *
 *    (xi,yi)   relative location for n points
 *    ti        observed relative travel times
 *
 *    axx := sum_i{xi*xi};  axy := sum_i{xi*yi};  ayy := sum_i{yi*yi};
 *    taux := sum_i{ti*xi};  tauy := sum_i{ti*yi};
 *    wanted:  sx, sy
 *
 *      /  axx    axy   \   / sx \   / taux \
 *      |               | * |    | = |      | ,   or  A*s = tau
 *      \  axy    ayy   /   \ sy /   \ tauy /
 *
 *    ==>   s = (1/A)*tau
 *
 *
 * Important:
 *                              _
 *    Only the geometric center x = (1/n)*(sum_i{xi},sum_i{yi}) as origin is
 *    stable against a constant time shift dt in observed times:
 *
 *    ti' := ti + dt   for all i
 *    then            _                 _
 *    tau' = tau + dt*x       = tau for x = 0
 *    For other origins the slowness is warped by time residuals on the
 *    reference station.
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
{
	/* local variables */
	int      i;                   /* counter */
	REAL     tmean, xmean, ymean; /* mean values of arrays */
	REAL     tx, ty;              /* time vector */
	REAL     sx, sy;              /* slowness vector */
	REAL     sx2, sy2, sxy;       /* squares */
	REAL     axx, axy, ayx, ayy;  /* trafo matrix A */
	REAL     tq;                  /* t square */
	REAL     det;                 /* determinant of matrix A */
	REAL     norm;                /* normalisation */
	REAL     r, scr1, scr2;       /* scratch */
	REAL     saz, caz;            /* sine & cosine of azimuth */

	/* executable code */

	if  (listlth < 3)  {
		*status = SHE_SPECERROR+1;
		return;
	} /*endif*/

	/* compute mean values */
	tmean = xmean = ymean = 0.0;
	for  (i=0; i<listlth; i++)  {
		tmean += time[i];
		xmean += x[i];
		ymean += y[i];
	} /*endfor*/
	tmean /= (float)listlth;
	xmean /= (float)listlth;
	ymean /= (float)listlth;
	/* printf( "--> new location routine\n" ); */

	/* coordinate shift to center*/
	for  (i=0;i<listlth;i++)  {
		time[i] -= tmean;
		x[i] -= xmean;
		y[i] -= ymean;
	} /*endfor*/

#	ifdef SH_DEBUGXXX
	for  (i=0; i<listlth; i++)
		printf( "--sh-debug->  %f %f %f\n", x[i], y[i], time[i] );
#	endif

	tx = ty = tq = 0.0;
	axx = axy = ayx = ayy = 0.0;

	if  (listlth == 3)  {
		tx = time[1];
		ty = time[2];
		axx = x[1];
		axy = y[1];
		ayx = x[2];
		ayy = y[2];
	} else {
		for  (i=0;i<listlth;i++)  {
			tq += time[i]*time[i];
			tx += time[i]*x[i];
			ty += time[i]*y[i];
			axx += x[i]*x[i];
			axy += x[i]*y[i];
			ayy += y[i]*y[i];
		} /*endfor*/
		ayx = axy;
	} /*endif */

	/* compute determinant, Cramer's rule */
	det = axx*ayy - axy*ayx;
	if  (Abs(det) < 1.e-10)  {
		*status = SHE_SPECERROR+2;
		return;
	} /*endif*/
	sx = (tx*ayy - ty*axy) / det;
	sy = (ty*axx - tx*ayx) / det;
#	ifdef SH_DEBUG
	printf( "--sh-debug-> (new) axx: %e, axy: %e, ayy: %e, D: %e\n",
		axx, axy, ayy, det );
	printf( "--sh-debug-> (new) sx: %f, sy: %f\n", sx, sy );
#	endif

	if  (sy == 0.0)  {
		*azimuth = (sx < 0.0) ? 90.0 : 270.0;
	} else {
		*azimuth = atan( sx/sy ) * SHC_RAD_TO_DEG;
		if  (sy > 0.0)  {
			*azimuth += 180.0;
		} else if  (sx > 0.0)  {
			*azimuth += 360;
		} /*endif*/
	} /*endif*/

	norm = sqrt( sx*sx + sy*sy );
	*slowness = SHC_DEG_TO_KM * norm;
	if  (norm < 1.e-10)  {
		*status = SHE_SPECERROR+3;
		return;
	} /*endif*/
	norm = 1.0 / norm;

	/* deviations */
	det = Abs(det);
	if  (listlth == 3)  {
		/* dtc = 2.0 * dt; ??? */
		r = atan( sx/sy );
		saz = sin( r );
		caz = cos( r );
		r = norm * dt/det;
		scr1 = saz*axx + caz*axy;
		scr2 = saz*ayx + caz*ayy;
		*azimerr = r * sqrt( scr1*scr1 + scr2*scr2 ) * SHC_RAD_TO_DEG;
		scr1 = caz*axx - saz*axy;
		scr2 = caz*ayx - saz*ayy;
		r *= norm * sqrt( scr1*scr1 + scr2*scr2 );
		*slowerr = r * SHC_DEG_TO_KM / (norm*norm);
	} else {
		r = sqrt( fabs(tq-tx*sx-ty*sy) / (REAL)(listlth-3) );
		sx2 = sx*sx;
		sy2 = sy*sy;
		sxy = sx*sy;
		*slowerr = r * sqrt( (ayy*sx2 - 2.0*axy*sxy + axx*sy2) / det ) *
			norm * SHC_DEG_TO_KM;
		*azimerr = r * sqrt( (ayy*sy2 + 2.0*axy*sxy + axx*sx2) / det ) *
			norm * norm * SHC_RAD_TO_DEG;
	} /*endif*/

#	ifdef SH_DEBUG
	printf( "--sh-debug-> (new) slow: %5.2f +- %5.2f\n", *slowness, *slowerr );
	printf( "--sh-debug-> (new) azim: %5.2f +- %5.2f\n", *azimuth, *azimerr );
#	endif

} /* end of mt_locate */



/*--------------------------------------------------------------------------*/



void mt_locate_old( int listlth, REAL time[], REAL x[], REAL y[],
	REAL dt, REAL *azimuth, REAL *azimerr, REAL *slowness,
	REAL *slowerr, int *status )

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
{
	/* local variables */
	int      i;                   /* counter */
	REAL     tx, ty;              /* time vector */
	REAL     sx, sy;              /* slowness vector */
	REAL     sx2, sy2, sxy;       /* squares */
	REAL     axx, axy, ayx, ayy;  /* trafo matrix A */
	REAL     tq;                  /* t square */
	REAL     det;                 /* determinant of matrix A */
	REAL     norm;                /* normalisation */
	REAL     r, scr1, scr2;       /* scratch */
	REAL     saz, caz;            /* sine & cosine of azimuth */

	/* executable code */

	if  (listlth < 3)  {
		*status = SHE_SPECERROR+1;
		return;
	} /*endif*/

	/* coordinate shift */
	for  (i=1;i<listlth;i++)  {
		time[i] -= *time;
		x[i] -= *x;
		y[i] -= *y;
	} /*endfor*/

	tx = ty = tq = 0.0;
	axx = axy = ayx = ayy = 0.0;

	if  (listlth == 3)  {
		tx = time[1];
		ty = time[2];
		axx = x[1];
		axy = y[1];
		ayx = x[2];
		ayy = y[2];
	} else {
		for  (i=1;i<listlth;i++)  {
			tq += time[i]*time[i];
			tx += time[i]*x[i];
			ty += time[i]*y[i];
			axx += x[i]*x[i];
			axy += x[i]*y[i];
			ayy += y[i]*y[i];
		} /*endfor*/
		ayx = axy;
	} /*endif */

	/* compute determinant, Cramer's rule */
	det = axx*ayy - axy*ayx;
	if  (Abs(det) < 1.e-10)  {
		*status = SHE_SPECERROR+2;
		return;
	} /*endif*/
	sx = (tx*ayy - ty*axy) / det;
	sy = (ty*axx - tx*ayx) / det;
#	ifdef SH_DEBUG
	printf( "--sh-debug-> (old) axx: %e, axy: %e, ayy: %e, D: %e\n",
		axx, axy, ayy, det );
	printf( "--sh-debug-> (old) sx: %f, sy: %f\n", sx, sy );
#	endif

	if  (sy == 0.0)  {
		*azimuth = (sx < 0.0) ? 90.0 : 270.0;
	} else {
		*azimuth = atan( sx/sy ) * SHC_RAD_TO_DEG;
		if  (sy > 0.0)  {
			*azimuth += 180.0;
		} else if  (sx > 0.0)  {
			*azimuth += 360;
		} /*endif*/
	} /*endif*/

	norm = sqrt( sx*sx + sy*sy );
	*slowness = SHC_DEG_TO_KM * norm;
	if  (norm < 1.e-10)  {
		*status = SHE_SPECERROR+3;
		return;
	} /*endif*/
	norm = 1.0 / norm;

	/* deviations */
	det = Abs(det);
	if  (listlth == 3)  {
		/* dtc = 2.0 * dt; ??? */
		r = atan( sx/sy );
		saz = sin( r );
		caz = cos( r );
		r = norm * dt/det;
		scr1 = saz*axx + caz*axy;
		scr2 = saz*ayx + caz*ayy;
		*azimerr = r * sqrt( scr1*scr1 + scr2*scr2 ) * SHC_RAD_TO_DEG;
		scr1 = caz*axx - saz*axy;
		scr2 = caz*ayx - saz*ayy;
		r *= norm * sqrt( scr1*scr1 + scr2*scr2 );
		*slowerr = r * SHC_DEG_TO_KM / (norm*norm);
	} else {
		r = sqrt( fabs(tq-tx*sx-ty*sy) / (REAL)(listlth-3) );
		sx2 = sx*sx;
		sy2 = sy*sy;
		sxy = sx*sy;
		*slowerr = r * sqrt( (ayy*sx2 - 2.0*axy*sxy + axx*sy2) / det ) *
			norm * SHC_DEG_TO_KM;
		*azimerr = r * sqrt( (ayy*sy2 + 2.0*axy*sxy + axx*sx2) / det ) *
			norm * norm * SHC_RAD_TO_DEG;
	} /*endif*/

#	ifdef SH_DEBUG
	printf( "--sh-debug-> (old) slow: %5.2f +- %5.2f\n", *slowness, *slowerr );
	printf( "--sh-debug-> (old) azim: %5.2f +- %5.2f\n", *azimuth, *azimerr );
#	endif

} /* end of mt_locate_old */



/*--------------------------------------------------------------------------*/



void mt_locate_svd( int listlth, REAL time[], REAL x[], REAL y[],
	REAL dt, REAL *azimuth, REAL *azimerr, REAL *slowness,
	REAL *slowerr, int *status )

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
{
	/* local variables */
	float    **locmatrix;   /* location matrix */
	float    **evmatrix;    /* eigenvector matrix */
	float    eval[2];       /* eigenvalues */
	float    s[2];          /* Sx, Sy */
	int      i;             /* counter */
	int      imax, imin;    /* index of maximum and minimum eigenvalue */

	/* executable code */

	locmatrix = nr_matrix( 1, listlth, 1, 2 );
	evmatrix = nr_matrix( 1, 2, 1, 2 );

	/* copy input values */
	for  (i=0; i<listlth; i++)  {
		locmatrix[i+1][1] = x[i];
		locmatrix[i+1][2] = y[i];
	} /*endfor*/

	/* perform decomposition */
	nr_svdcmp( locmatrix, listlth, 2, eval-1, evmatrix );

	/* check eigenvalues */
	imax = (eval[0] > eval[1]) ? 0 : 1;
	imin = (eval[0] > eval[1]) ? 1 : 0;
	if  (eval[imin] < 1.0e-6*eval[imax])
		eval[imin] = 0.0;

	nr_svbksb( locmatrix, eval-1, evmatrix, listlth, 2, time-1, s-1 );

	nr_free_matrix( locmatrix, 1, listlth, 1, 2 );
	nr_free_matrix( evmatrix, 1, 2, 1, 2 );

	*slowness = SHC_DEG_TO_KM * sqrt( s[0]*s[0] + s[1]*s[1] );
	*azimuth = atan( s[0]/s[1] ) * SHC_RAD_TO_DEG;
	*slowerr = *azimerr = 0.0;

#	ifdef SH_DEBUG
	printf( "--sh-debug-> (svd) slow: %5.2f\n", *slowness );
	printf( "--sh-debug-> (svd) azim: %5.2f\n", *azimuth );
#	endif

} /* end of mt_locate_svd */



/*--------------------------------------------------------------------------*/



void mt_locate_old_error( int listlth, REAL time[], REAL x[], REAL y[],
	REAL azimuth, REAL slowness, REAL *resid )

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
{
	/* local variables */
	int      i;                   /* counter */
	int      refstat;             /* index of reference station */
	REAL     tx, ty;              /* time vector */
	REAL     sx, sy;              /* slowness vector */
	REAL     wsx, wsy;            /* 'wrong' slowness vector */
	REAL     sx2, sy2, sxy;       /* squares */
	REAL     axx, axy, ayx, ayy;  /* trafo matrix A */
	REAL     det;                 /* determinant of matrix A */
	REAL     norm;                /* normalisation */
	REAL     r, scr1, scr2;       /* scratch */
	REAL     saz, caz;            /* sine & cosine of azimuth */
	REAL     reft, refx, refy;    /* reference values */
	REAL     xc, yc;              /* coordinates of center */
	REAL     dkx, dky;            /* residual */

	/* executable code */

	if  (listlth < 3)  {
		printf( "--> mt_locate_error: less than 3 traces\n" );
		return;
	} /*endif*/

	/* compute sx,sy */
	sx = -(slowness/SHC_DEG_TO_KM) * sin( azimuth/SHC_RAD_TO_DEG );
	sy = -(slowness/SHC_DEG_TO_KM) * cos( azimuth/SHC_RAD_TO_DEG );
#	ifdef SH_DEBUG
	printf( "--sh-debug-> (err) sx,sy: %f,%f\n", sx, sy );
#	endif
	
	/* compute center */
	xc = yc = 0.0;
	for  (i=0; i<listlth; i++)  {
		xc += x[i];
		yc += y[i];
	} /*endfor*/
	xc /= (float)listlth;
	yc /= (float)listlth;

	for  (refstat=0; refstat<listlth; refstat++)  {

		/* coordinate shift */
		reft = time[refstat];
		refx = x[refstat];
		refy = y[refstat];
		for  (i=0;i<listlth;i++)  {
			time[i] -= reft;
			x[i] -= refx;
			y[i] -= refy;
		} /*endfor*/
		xc -= refx;
		yc -= refy;
	
		tx = ty = 0.0;
		axx = axy = ayx = ayy = 0.0;
	
		if  (listlth == 3)  {
			tx = time[1];
			ty = time[2];
			axx = x[1];
			axy = y[1];
			ayx = x[2];
			ayy = y[2];
		} else {
			for  (i=0;i<listlth;i++)  {
				tx += time[i]*x[i];
				ty += time[i]*y[i];
				axx += x[i]*x[i];
				axy += x[i]*y[i];
				ayy += y[i]*y[i];
			} /*endfor*/
			ayx = axy;
		} /*endif */
	
		/* compute determinant, Cramer's rule */
		det = axx*ayy - axy*ayx;
		if  (Abs(det) < 1.e-10)  {
			printf( "--> mt_locate_error: zero determinant\n" );
			return;
		} /*endif*/
		wsx = (tx*ayy - ty*axy) / det;
		wsy = (ty*axx - tx*ayx) / det;
#		ifdef SH_DEBUG
		/*printf( "--sh-debug-> (ref%02d) axx: %e, axy: %e, ayy: %e, D: %e\n",
			refstat, axx, axy, ayy, det );*/
		printf( "--sh-debug-> (ref%02d) wsx: %f, wsy: %f\n", refstat, wsx, wsy );
#		endif

		dkx = (axx*sx + axy*sy - tx)/((float)listlth*xc);
		dky = (axy*sx + ayy*sy - ty)/((float)listlth*yc);

		if  (resid != NULL)  *resid++ = dkx;
	
#		ifdef SH_DEBUG
		printf( "--sh-debug-> (ref%02d) dkx: %f, dky: %f\n", refstat, dkx, dky );
#		endif

	} /*endfor*/

} /* end of mt_locate_old_error */



/*--------------------------------------------------------------------------*/



void mt_beamshift( int listlth, REAL x[], REAL y[], REAL azimuth,
	REAL slowness, REAL delay[] )

/* computes delays of arrival times according to the location (x,y)
 * (in km) and the given azimuth and slowness.  Negative slownesses
 * yield negative delays, but see also mt_elevation_correction: there
 * negative slownesses give delays for downcoming phases.
 *
 * parameters of routine
 * int        listlth;      input; number of locations
 * REAL       x[], y[];     input; locations (in km, relative to reference point)
 * REAL       azimuth;      input; azimuth of event (in degrees)
 * REAL       slowness;     input; slowness of event (in sec/degree)
 * REAL       delay[];      output; computed delays
 */
{
	/* local variables */

	/* executable code */

	/* azimuth in mathematical coordinate system */
	azimuth = (90.0 - azimuth) / SHC_RAD_TO_DEG;

	/* compute slowness in sec/km */
	slowness /= SHC_DEG_TO_KM;

	/* compute delays */
	while  (listlth-- > 0)
		*delay++ = ((*x++) * cos(azimuth) + (*y++) * sin(azimuth)) * slowness;

} /* end of mt_beamshift */



/*--------------------------------------------------------------------------*/



void mt_fold( long la, REAL a[], long lb, REAL b[], REAL c[] )

/* folds traces a[0..la-1] and b[0..lb-1] to trace c[0..la+lb-2]
 *
 * parameters of routine
 * long       la;       input; length of trace a
 * REAL       a[];      input; trace a
 * long       lb;       input; length of trace b
 * REAL       b[];      input; trace b
 * REAL       c[];      output; result trace (length la+lb-1)
 */
{
	/* local variables */
	long     i, j, k, lc;     /* counters */

	/* executable code */

	/* create offset-1 traces */
	a--; b--; c--;
	lc = la+lb-1;

	/* clear output array */
	for  (i=1; i<=lc; c[i++] = 0.0 ) {}

	/* compute convolution */
	for  (i=1; i<=la; i++)  {
		for  (j=1; j<=lb; j++)  {
			k = i+j-1;
			c[k] += a[i]*b[j];
		} /*endfor*/
		sy_sharecpu();
	} /*endfor*/

} /* end of mt_fold */



/*----------------------------------------------------------------------------*/



REAL mt_random( float ampl )

/* creates uniform random number with maximum amplitude "ampl"
 *
 * parameter of routine
 * float     ampl;     input; maximum amplitude
 */
{

	/* executable code */

	if  (!mtv_seed_is_init)  {
		mth_init_seed();
		mtv_seed_is_init = TRUE;
	} /*endif*/

	if  (ampl == 0.0)  {  /* initialise */
		if   (--mtv_seed > 0)  mtv_seed = -mtv_seed;
		nr_ran2( &mtv_seed );
		return 0.0;
	} else {
		return  ( ampl * (2.0*nr_ran2(&mtv_seed) - 1.0) );
	} /*endif*/

} /* end of mt_random */



/*----------------------------------------------------------------------------*/



REAL mt_gauss_random( REAL width )

/* creates normal distributed random number
 *
 * parameter of routine
 * REAL      width;     input; width of distribution
 */
{
	/* local variables */
	static int      gauss_seed;   /* integer seed */
	static BOOLEAN  gauss_is_init=FALSE; /* gauss seed is initialised */

	/* executable code */

	if  (!mtv_seed_is_init)  {
		mth_init_seed();
		mtv_seed_is_init = TRUE;
	} /*endif*/
	if  (!gauss_is_init)  {
		gauss_seed = (int)mtv_seed;
		gauss_is_init = TRUE;
	} /*endif*/

	return (nr_gasdev(&gauss_seed) * width);

} /* end of mt_gauss_random */



/*----------------------------------------------------------------------------*/



void mt_randomstr( int lth, char str[] )

/* creates random string of digits of length "lth"
 *
 * parameters of routine
 * int        lth;       input; length of output string
 * char       str[];     output; random string
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	mt_random( 0.0 );
	for  (i=0; i<lth; i++)  {
		str[i] = (char)fabs(mt_random(10.0)) + '0';
		if  (str[i] > '9')  str[i] = '9';
	} /*endfor*/

} /* end of mt_randomstr */



/*----------------------------------------------------------------------------*/



void mt_despike( REAL smp[], long lth, REAL critmul, FILE *log,
	long *scnt, long *dcnt )

/* removes spikes from array "smp".  A spike is recognized if the
 * difference between smp[i] and smp[i-1] is larger then "critmul"
 * times the average distance.
 *
 * parameters of routine
 * REAL       smp[];       modify; array to be despiked
 * long       lth;         input; length of array
 * REAL       critmul;     input; detection factor
 * FILE       *log;        input; log file (if NULL no log)
 * long       *scnt;       output; number of spikes detected
 * long       *dcnt;       output; number of discontinuities (not removed)
 */
{
	/* local variables */
	long     i;        /* counter */
	REAL     diff;     /* difference */
	REAL     mean;     /* mean difference */
	BOOLEAN  isspike;  /* last sample was spike */

	/* executable code */

	mean = 0.;
	for  (i=0; i<lth-1; i++)  {
		diff = smp[i+1] - smp[i];
		mean += Abs( diff );
	} /*endfor*/
	mean /= (REAL)(lth-1);

	isspike = TRUE;
	*scnt = *dcnt = 0;
	for  (i=0; i<lth-1; i++)  {
		diff = smp[i+1] - smp[i];
		if  (Abs(diff) > critmul*mean)  {
			if  (isspike)  {
				smp[i] = (i>0) ? (smp[i-1]+smp[i+1])/2.0 : smp[1];
				(*scnt)++;
				isspike = FALSE;
				if  (log != NULL)
					fprintf( log, "spike %ld: sample %ld\n",
						*scnt, i );
			} else {
				isspike = TRUE;
			} /*endif*/
		} else {
			if  (isspike  &&  i != 0)  {
				(*dcnt)++;
				if  (log != NULL)
					fprintf( log, "disc %ld: sample %ld\n",
						*dcnt, i );
			} /*endif*/
			isspike = FALSE;
		} /*endif*/
	} /*endfor*/

} /* end of mt_despike */



/*----------------------------------------------------------------------------*/



void mt_mend( SAMPLE smp[], long lth, long loff, long roff, int w,
	REAL dt, BOOLEAN pol, STATUS *status )

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
{
	/* local variables */
	long     i, j;     /* counters */
	long     k;
	int      order;    /* order of interpolation */
	REAL     *x;       /* x-array pointer */
	SAMPLE   *y;       /* y-array pointer */
	SAMPLE   err;

	/* executable code */

	order = (int)(loff+roff-2)/w + 2;

	x = (REAL *)sy_allocmem( order, (int)sizeof(REAL), status );
	if  (Severe(status))  return;
	y = (SAMPLE *)sy_allocmem( order, (int)sizeof(SAMPLE), status );
	if  (Severe(status))  {sy_deallocmem(x); return;}

	for  (i=0,j=0; i<loff; i += w,j++)  {
		y[j] = smp[i];
		x[j] = (float)i * dt;
	} /*endfor*/
	for  (i=0; i<roff; i += w,j++)  {
		k = lth - roff + i;
		/* y[loff+j] = smp[k]; */
		/* x[loff+j] = (float)k * dt; */
		y[j] = smp[k];
		x[j] = (float)k * dt;
	} /*endfor*/
	if  (pol)  {
		for  (i=loff; i<(lth-roff); i++)
			nr_polint( x, y, order, (float)i*dt, smp+i, &err );
	} else {
		for  (i=loff; i<(lth-roff); i++)
			nr_ratint( x, y, order, (float)i*dt, smp+i, &err );
	} /*endif*/

	sy_deallocmem( y );
	sy_deallocmem( x );

} /* end of mt_mend */



/*----------------------------------------------------------------------------*/



void mt_levinson( SAMPLE r[], SAMPLE g[], SAMPLE f[], int m, STATUS *status )

/* Levinson algorithm for symmetric toeplitz-matrix "r[0..m-1]".
 *
 * parameters of routine
 * SAMPLE     r[];     input; symmetric toeplitz-matrix r[0..m-1]
 * SAMPLE     g[];     input; correlation of input with desired trace
 * SAMPLE     f[];     output; computed inverse filter
 * int        m;       input; length of all traces
 * STATUS     *status; output; return status
 */
{
	/* local variables */
	SAMPLE   *a, *b;    /* pointer to workspace */
	int      i, j, ii;  /* counter */
	SAMPLE   gn, z1, z2;

	/* executable code */

	a = (SAMPLE *)sy_allocmem( 2L*(long)m, (int)sizeof(SAMPLE), status );
	if  (Severe(status))  return;
	b = a + m;

	/* create offset-1 arrays */
	r--; g--; f--; a--; b--;

	f[1] = g[1] / r[1];
	a[1] = r[2] / r[1];
	for  (i=2; i<=m; i++)  {
		gn = r[1];
		z1 = (i==m) ? 0. : r[i+1];
		z2 = g[i];
		for  (j=2; j<=i; j++)  {
			gn -= r[j]*a[j-1];
			z1 -= r[j]*a[i-j+1];
			z2 -= r[j]*f[i-j+1];
		} /*endfor 300*/
		sy_sharecpu();
		a[i] = z1/gn;
		f[i] = z2/gn;
		ii = i - 1;
		for  (j=1; j<=ii; j++)  {
			b[j] = a[j] - a[i]*a[ii-j+1];
			f[j] -= f[i]*a[ii-j+1];
		} /*endfor 400*/
		sy_sharecpu();
		for  (j=1; j<=ii; j++)
			a[j] = b[j];
	} /*endfor 200*/

	sy_deallocmem( a+1 );

} /* end of mt_levinson */



/*----------------------------------------------------------------------------*/



void mt_arp( int order, REAL coeff[], REAL start[], SAMPLE dat[], long lth )

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
{
	/* local variables */
	long     k, l;      /* counters */

	/* executable code */

	/* offset-1 arrays */
	dat--; coeff--; start--;

	/* start procedure */
	for  (k=1; k<=order; k++)
		for  (l=1; l<=order; l++)
			dat[k] += (k>l) ? coeff[l]*dat[k-l] : coeff[l]*start[l];

	/* normal ARP */
	for  (k=order+1; k<=lth; k++)  {
		for  (l=1; l<=order; l++)
			dat[k] += coeff[l]*dat[k-l];
		sy_sharecpu();
	} /*endfor*/

} /* end of mt_arp */



/*----------------------------------------------------------------------------*/



void mt_multiply_array( SAMPLE s1[], SAMPLE s2[], long lth, SAMPLE out[] )

/* multiplies two arrays and stores result in "out"
 *
 * parameters of routine
 * SAMPLE     s1[], s2[];    input; input arrays
 * long       lth;           input; length of all (input and output) arrays
 * SAMPLE     out[];         output; result
 */
{
	/* local variables */
	long     i;      /* counter */

	/* executable code */

	for  (i=0; i<lth; i++)
		*out++ = (*s1++) * (*s2++);

} /* end of mt_multiply_array */



/*----------------------------------------------------------------------------*/



void mt_divide_array( SAMPLE s1[], SAMPLE s2[], long lth, SAMPLE out[] )

/* divides two arrays and stores result in "out"
 *
 * parameters of routine
 * SAMPLE     s1[], s2[];    input; input arrays
 * long       lth;           input; length of all (input and output) arrays
 * SAMPLE     out[];         output; result
 */
{
	/* local variables */
	long     i;      /* counter */

	/* executable code */

	for  (i=0; i<lth; i++)  {
		if  (fabs(*s1) > 1.0e20 || fabs(*s2) < 1.0e-20)  {
			*out++ = 0.;
			s1++; s2++;
		} else {
			*out++ = (*s1++) / (*s2++);
		} /*endif*/
	} /*endfor*/

} /* end of mt_divide_array */



/*----------------------------------------------------------------------------*/

#ifdef XXX

void mt_locdiff( REAL lat1, REAL lon1, REAL lat2, REAL lon2,
	REAL *dist, REAL *azim )

/* returns distance in degrees and azimuth of two locations given
 * by (lat1,lon1) and (lat2,lon2)
 *
 * parameters of routine
 * REAL       lat1, lon1;    input; first location in degrees
 * REAL       lat2, lon2;    input; second location in degrees
 * REAL       *dist;         output; distance in degrees
 * REAL       *azim;         output; azimuth from loc1 to loc2 (in deg)
 */
{
	/* local variables */
	double     b1, b2, dl, d, alpha;

	/* executable code */

	b1 = (double)lat1 / SHC_RAD_TO_DEG;
	b2 = (double)lat2 / SHC_RAD_TO_DEG;

	b1 = BC_PI/2.0 - b1;
	b2 = BC_PI/2.0 - b2;
	dl = Abs((double)lon1-(double)lon2) / SHC_RAD_TO_DEG;
	if  (dl > BC_PI)  dl = 2.0*BC_PI - dl;
	d = acos( cos(b1)*cos(b2) + sin(b1)*sin(b2)*cos(dl) );
	if  (Abs(b1) < SHC_EPSILON  ||  Abs(dl) < SHC_EPSILON)  {
		alpha = 0.0;
	} else {
		alpha = acos( (cos(b2)-cos(b1)*cos(d)) / (sin(b1)*sin(d)) );
	} /*endif*/

	*dist = (REAL)(d*SHC_RAD_TO_DEG);
	*azim = (REAL)((BC_PI/2.0 - alpha)*SHC_RAD_TO_DEG);

} /* end of mt_locdiff */



/*----------------------------------------------------------------------------*/



void mt_locadd( REAL lat, REAL lon, REAL dist, REAL azim,
	REAL *latx, REAL *lonx )

/* computes location (*latx,*lonx) from point (lat,lon) and
 * a distance "dist" in direction "azim"
 *
 * parameters of routine
 * REAL       lat, lon;      input; input location in degrees
 * REAL       dist;          input; distance in degrees
 * REAL       azim;          input; direction
 * REAL       *latx, *lonx;  output; output location
 */
{
	/* local variables */
	double   b, d, bx, cosbx, alpha, sinbx, sinb, dl;

	/* executable code */

	b = BC_PI/2.0 - (double)lat/SHC_RAD_TO_DEG;
	d = (double)dist / SHC_RAD_TO_DEG;
	alpha = BC_PI/2.0 - (double)azim/SHC_RAD_TO_DEG;

	sinb = sin(b);
	cosbx = cos(b)*cos(d) + sinb*sin(d)*cos(alpha);
	sinbx = sqrt( 1.0 - cosbx*cosbx );

	if  (Abs(sinbx) < SHC_EPSILON)  {
		bx = 0.0;
		dl = 0.0;
	} else if  (Abs(sinb) < SHC_EPSILON)  {
		bx = d;
		dl = 0.0;
	} else {
		dl = acos( (cos(d) - cos(b)*cosbx) / (sinbx*sinb) );
		bx = acos( cosbx );
	} /*endif*/

	bx = BC_PI/2.0 - bx;
	dl += (double)lon/SHC_RAD_TO_DEG;

	*latx = (REAL)(bx*SHC_RAD_TO_DEG);
	*lonx = (REAL)(dl*SHC_RAD_TO_DEG);

} /* end of mt_locadd */

#endif

/*----------------------------------------------------------------------------*/



void mt_fit_gauss( REAL x[], SAMPLE y[], int ndata, REAL ampl,
	int maxiter, REAL *pos, REAL *width, int *iter, STATUS *status )

/* fits a gauss function of position "*pos" and width "*width" to 
 * the data given in x[0..ndata-1] and y[0..ndata-].
 *
 * parameters of routine
 * REAL       x[], y[];      input; data to be fitted
 * int        ndata;         input; number of samples in x and y
 * REAL       ampl;          input; amplitude of gauss peak
 * int        maxiter;       input; maximum number of iterations
 * REAL       *pos;          modify; position of gauss peak
 * REAL       *width;        modify; half-width of gauss peak
 * int        *iter;         output; number of iterations
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	REAL     *sig;     /* standard deviations of data points */
	int      i;        /* counter */
	REAL     a[2];     /* fitting parameters */
	int      lista[2]; /* enumeration of parameters */
	BOOLEAN  quit;     /* quit iteration loop */
	REAL     **covar, **alpha; /* scratch */
	REAL     chisq, last_chisq;
	REAL     alambda, last_alambda;

	/* executable code */

	/* setup fitting parameters */
	a[0] = *pos;     /* start value of position */
	a[1] = *width;   /* start value of width */
	lista[0] = 1;
	lista[1] = 2;

	covar = nr_matrix( 1, 2, 1, 2 );
	alpha = nr_matrix( 1, 2, 1, 2 );

	/* allocate and setup standard deviations */
	sig = (REAL *)sy_allocmem( (long)ndata, (int)sizeof(REAL), status );
	if  (Severe(status))  return;
	for  (i=0; i<ndata; i++)
		sig[i] = 1.0; /*0.1*ampl;*/

	/* do iteration */
	*iter = 0;
	mtv_fitampl = ampl;
	alambda = -1.0;
	last_alambda = -1.0;
	last_chisq = -100.0;
	quit = FALSE;
	FOREVER  {
		nr_mrqmin( x-1, y-1, sig-1, ndata, a-1, 2, lista-1, 2, covar,
			alpha, &chisq, mth_eval_gauss, &alambda, status );
		if  (Severe(status))  break;
		sy_sharecpu();
		(*iter)++;
		if  (quit || --maxiter <= 0)  break;
		if  (fabs(chisq-last_chisq) < SHC_EPSILON  &&
			alambda >= last_alambda)  {
			quit = TRUE;
			alambda = 0.0;
		} /*endif*/
		last_chisq = chisq;
		last_alambda = alambda;
	} /*endwhile*/

	nr_free_matrix( alpha, 1, 2, 1, 2 );
	nr_free_matrix( covar, 1, 2, 1, 2 );
	sy_deallocmem( sig );

	*pos = a[0];
	*width = a[1];

} /* end of mt_fit_gauss */



/*----------------------------------------------------------------------------*/



static void mth_init_seed( void )

/* initialises global seed value "mtv_seed" for random generator
 *
 * no parameters
 */
{
	/* local variables */
	char     rfile[BC_FILELTH+1];  /* seed file */
	FILE     *fp;                  /* seed file pointer */

	/* executable code */

	strcpy( rfile, shd_scratch );
	strcat( rfile, "RANDSEED.CNT" );
	fp = sy_fopen( rfile, "r" );
	if  (fp == NULL)  {
		mtv_seed = -100;
	} else {
		fscanf( fp, "%ld", &mtv_seed );
		fclose( fp );
	} /*endif*/
	mtv_seed -= 3;
	if   (mtv_seed > 0)  mtv_seed = -mtv_seed;
	nr_ran2( &mtv_seed );
	fp = sy_fopen( rfile, "w" );
	if  (fp == NULL)  return;
	fprintf( fp, "%ld\n", mtv_seed );
	fclose( fp );

} /* end of mth_init_seed */



/*----------------------------------------------------------------------------*/



static void mth_eval_gauss( REAL x, REAL a[], SAMPLE *yfit,
	SAMPLE dyda[], int ma )

/* evaluates gauss function and derivatives dx/da
 * y(x) = ampl * exp( -(x-a[0])^2/(2*a[1]^2]) )
 *
 * parameters of routine
 * REAL       x;          input; evaluation point
 * REAL       a[1..2];    input; function parameters (pos & width)
 * SAMPLE     *yfit;      output; function value
 * SAMPLE     dyda[1..2]; output; partial derivatives
 * int        ma;         input; number of parameters
 */
{
	/* local variables */
	REAL     tmp, tmp2;      /* scratch */

	/* executable code */

	if  (ma != 2)  {
		sy_alert( "*** mth_eval_gauss: this cannot happen ***" );
		return;
	} /*endif*/

	tmp = x - a[1];
	tmp2 = a[2]*a[2];
	*yfit = mtv_fitampl * exp( -tmp*tmp / (2.0*tmp2) );
	dyda[1] = *yfit * tmp / tmp2;  /* dy/d(a[1])[x] */
	dyda[2] = *yfit * tmp*tmp / (tmp2*a[2]);

} /* mth_eval_gauss */



/*----------------------------------------------------------------------------*/



void mt_average_smoothing( SAMPLE inp[], long trclth, int avlth, SAMPLE out[] )

/* averages input trace, takes "avlth" samples and takes mean as new output
 * sample, then moves average window by 1 sample
 *
 * parameters of routine
 * SAMPLE     inp[];         input; input array
 * long       trclth;        input; length of input/output traces in samples
 * int        avlth;         input; length of average window in samples
 * SAMPLE     out[];         output; output array
 */
{
	/* local variables */
	SAMPLE   sum;        /* summation */
	int      curr_avlth; /* current average length */
	long     i, j;       /* sample counter */

	/* executable code */

	sum = 0.0;
	if  ((long)avlth > trclth)
		avlth = (int)trclth;

	curr_avlth = (avlth % 2) ? avlth/2 + 1 : avlth/2;
	for  (i=0; i<curr_avlth; i++)
		sum += inp[i];

	j = 1;
	for  (i=0; i<trclth; i++)  {
		out[i] = sum/(float)curr_avlth;
		if  (curr_avlth < avlth)  {
			sum += inp[curr_avlth];
			j = ++curr_avlth;
		} else {
			if  (j < trclth)  {
				sum += inp[j] - inp[j-avlth];
			} else {
				sum -= inp[j-avlth];
				curr_avlth--;
			} /*endif*/
			j++;
		} /*endif*/
	} /*endfor*/

} /* end of mt_average_smoothing */



/*----------------------------------------------------------------------------*/



void mt_elevation_correction( REAL slowness, int lth, REAL elev[],
	REAL veloc[], REAL shift[], STATUS *status )

/* applies corrections to given shift times according to station elevation.
 * Negative slownesses give downgoing phases, im mt_beamshift it also gives
 * negtive delays.
 *
 * parameters of routine
 * REAL       slowness;  input; slowness in sec/deg
 * int        lth;       input; length of arrays
 * REAL       elev[];    input; station elevation in m
 * REAL       veloc[];   input; average velocities at stations in km/sec
 * REAL       shift[];   modify; shift time to be corrected
 */
{
	/* local variables */
	int      i;          /* counter */
	REAL     vel2;       /* current velocity squared */
	REAL     time_corr;  /* time correction */
	REAL     tmp;        /* scratch */
	BOOLEAN  slowsign;   /* sign of slowness */

	/* executable code */

	/* rescale slowness to sec/km */
	slowsign = ( slowness < 0.0 );
	slowness /= SHC_DEG_TO_KM;
	/* square slowness */
	slowness *= slowness;

	for  (i=0; i<lth; i++)  {
		if  (veloc[i] == GLC_INVALID_NUMBER)  {
			*status = SHE_NO_STATVEL;
			return;
		} else if  (elev[i] == GLC_INVALID_NUMBER)  {
			*status = SHE_NO_STATELEV;
			return;
		} /*endif*/
		vel2 = veloc[i]*veloc[i];
		tmp = 1.0/vel2 - slowness;
		if  (tmp < 0.0)  {
			tmp = 0.0;
			printf( "*** mt_elevation_correction: sqrt " );
			printf( "domain error: %f ***\n", tmp );
		} /*endif*/
		time_corr = elev[i]/1000.0 * sqrt( tmp );
		/* printf( "--> trace %d corrected by %f sec\n", i+1, -time_corr ); */
		if  (slowsign)  shift[i] += time_corr;
		else            shift[i] -= time_corr;
	} /*endfor*/

} /* end of mt_elevation_correction */



/*----------------------------------------------------------------------------*/



void mt_find_gap( int cnt_limit, SAMPLE dat[], long lth, char inf[],
	char start[], REAL dt, FILE *log )

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
{
	/* local variables */
	SAMPLE   last_value;           /* value of last sample */
	int      value_counter;        /* counter of equal values */
	BOOLEAN  is_constant;          /* values are constant */
	char     ctime[BC_TIMELTH+1];  /* current time */
	long     s;                    /* sample counter */
	STATUS   locstat;              /* local status */
	long     start_cnt;            /* start sample counter */

	/* executable code */

	value_counter = 1;
	last_value = *dat++;
	is_constant = FALSE;
	start_cnt = 0;
	s = 1;
	while  (s++ < lth)  {
		if  (is_constant)  {
			if  (*dat == last_value)  {
				value_counter++;
			} else {
				if  (value_counter >= cnt_limit)  {
					/* printout */
					locstat = BC_NOERROR;
					tc_tadd( start, (REAL)start_cnt*dt, ctime, &locstat );
					fprintf( log, "%s time %s, %4d samples of value %f\n",
						inf, ctime, value_counter, last_value );
				} /*endif*/
				value_counter = 1;
				is_constant = FALSE;
			} /*endif*/
		} else {
			if  (*dat == last_value)  {
				value_counter = 2;
				is_constant = TRUE;
				start_cnt = s - 2;  /* because of the 's++' above */
			} /*endif*/
		} /*endif*/
		last_value = *dat++;
	} /*endwhile*/

	if  (is_constant && value_counter >= cnt_limit)  {
		/* printout */
		locstat = BC_NOERROR;
		tc_tadd( start, (REAL)start_cnt*dt, ctime, &locstat );
		fprintf( log, "%s time %s, %4d samples of value %f (EOT found)\n",
			inf, ctime, value_counter, last_value );
	} /*endif*/

} /* end of mt_find_gap */



/*----------------------------------------------------------------------------*/

/* fix_gap routine contributed by K.Koch
 *
 * version 1.0, 11-Jan-2000
 */

/*--------------------------------------------------------------------------*/


void mt_fix_gap( SAMPLE dat[], long lth, REAL const_value, BOOLEAN konst, 
               BOOLEAN one)

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
{
	/* local variables */
	SAMPLE   last_value;           /* value of last sample */
	SAMPLE   next_value;           /* value of last sample */
	REAL     slope;                /* slope between gap edge samples */
	REAL     epsilon;              /* epsilon values for value comp */
	int      value_counter;        /* counter of equal values */
	int      i;                    /* loop counter */
	BOOLEAN  is_constant;          /* values are constant */
	long     s;                    /* sample counter */
	long     start_cnt;            /* start sample counter */

	/* executable code */

	epsilon=0.000001*fabs(const_value);
	value_counter = 0;
	last_value = dat[0];
	is_constant = FALSE;
	if  (last_value==const_value)  {
		is_constant= TRUE;
		value_counter++;
	}  /* endif */
	start_cnt = 1;
	if (is_constant) start_cnt = 0;
	s = 1;
	while  (s++ < lth)  {
		if  (is_constant)  {
			if  (fabs(dat[s]-const_value) <= (double) epsilon) {
				value_counter++;
			} else {
				next_value = dat[s];
				if (start_cnt==0) last_value=next_value;
				slope=(next_value - last_value)/(REAL)(value_counter+1); 
				if (konst) {
					slope=0.0;
					last_value=(next_value + last_value)/2.0; 
				}  /*endif*/
            if (value_counter> (int) !one)
					for (i=0; i<value_counter; i++) 
						dat[start_cnt++] = last_value + (REAL)(i+1)*slope;
				value_counter = 0;
				is_constant = FALSE;
				start_cnt = s;
			   last_value = dat[s-1];
			} /*endif*/
		} else {
			if  (fabs(dat[s]-const_value) <= (double) epsilon) {
				value_counter = 1;
				is_constant = TRUE;
				start_cnt = s;
			   last_value = dat[s-1];
			} /*endif*/
		} /*endif*/
	} /*endwhile*/

	if (start_cnt >= lth) return;

	if  (is_constant)  {
		next_value = last_value;
		slope=(next_value - last_value)/(REAL)(value_counter);
		if (konst) {
			slope=0.0;
			last_value=(next_value + last_value)/2.0; 
		}  /*endif*/
      if (value_counter> (int) !one)
			for (i=1; i<value_counter; i++) {
				dat[start_cnt++]= last_value + (REAL)(i)*slope;
		} /*endfor*/
	} /*endif*/

} /* end of mt_fix_gap */


/*----------------------------------------------------------------------------*/

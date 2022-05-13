
/* file POLFILTR.C
 *      ==========
 *
 * version 8, 22-May-2006
 *
 * polarisation filter of seismhandler
 * K. Stammler, 7-APR-91
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
#include "shconst.h"
#include "mxusrdef.h"


#define COVMTXLTH 6
	/* length of covariance matrix array */
#define XX 0
#define YY 1
#define ZZ 2
#define XY 3
#define YZ 4
#define XZ 5
   /* index names of covariance matrix */

typedef REAL COVMTX[COVMTXLTH];
	/* covariance matrix */

/* prototypes of local routines */
static void pf_addcm( COVMTX a, COVMTX b );
static void pf_subcm( COVMTX a, COVMTX b );
static void pf_clrcm( COVMTX a );
static void pf_buildcm( REAL x, REAL y, REAL z, COVMTX a );
static void pf_evalcm( COVMTX c, REAL *l1, REAL *l2, REAL *l3 );
static void pf_calccm( REAL x[], REAL y[], REAL z[], long li,
	long hi, COVMTX c );
static void pf_diagcm( COVMTX c, REAL l[MXC_DIM],
	REAL trafo[MXC_DIM][MXC_DIM], int *big, STATUS *status );



/*-----------------------------------------------------------------------*/



void pf_filter( REAL xi[], REAL yi[], REAL zi[], REAL xo[],
	REAL yo[], REAL zo[], long lth, int n, REAL p, int cmreset )

/* filters 3-dim seismogram (x,y,z)
 *
 * parameters of routine
 * REAL       xi[], yi[], zi[];   input; 3-dim input seismogram
 * REAL       xo[], yo[], zo[];   output; filtered 3-dim seismogram
 * long       lth;                input; length of traces
 * int        n;                  input; coherence length
 * REAL       p;                  input; exponent of filter factor
 * int        cmreset;            input; the covariance matrix is
 *                                       recomputed after cmreset steps
 */
{
	/* local variables */
	long     li, hi, mi; /* sample counter */
	COVMTX   cm;         /* covariance matrix */
	COVMTX   lm;         /* current matrix */
	REAL     l1, l2, l3; /* eigenvalues */
	REAL     fac;        /* filter factor */

	/* executable code */

	pf_clrcm( cm );
	for  (li=(-n),hi=0; li<lth; li++,hi++)  {
		if  (li >= 0)  {
			pf_buildcm( xi[li], yi[li], zi[li], lm );
			pf_subcm( cm, lm );
		} /*endif*/
		if  (hi < lth)  {
			pf_buildcm( xi[hi], yi[hi], zi[hi], lm );
			pf_addcm( cm, lm );
		} /*endif*/
		mi = (li+hi)/2;
		if  (mi >=0  &&  mi < lth)  {
			if  ((mi+1) % cmreset == 0)      /* recompute cm to avoid */
				pf_calccm( xi, yi, zi, li>(-1) ? li+1 : 0, /* roundoff */
					hi<lth ? hi : lth-1, cm );
			pf_evalcm( cm, &l1, &l2, &l3 );
			if  (l1 > SHC_EPSILON)  {
				fac = 1. - l2/l1;
				if  (p != 1.)  fac = pow(fac,p);
				xo[mi] = xi[mi]*fac;
				yo[mi] = yi[mi]*fac;
				zo[mi] = zi[mi]*fac;
			} else {
				xo[mi] = 0.;
				yo[mi] = 0.;
				zo[mi] = 0.;
			} /*endif*/
		} /*endfor*/
		sy_sharecpu();
	} /*endfor*/

} /* end of pf_filter */



/*-----------------------------------------------------------------------*/



void pf_vecfilter( REAL xi[], REAL yi[], REAL zi[], REAL xo[],
	REAL yo[], REAL zo[], long lth, int n, REAL p, int cmreset, STATUS *status )

/* filters 3-dim seismogram (x,y,z)
 *
 * parameters of routine
 * REAL       xi[], yi[], zi[];   input; 3-dim input seismogram
 * REAL       xo[], yo[], zo[];   output; filtered 3-dim seismogram
 * long       lth;                input; length of traces
 * int        n;                  input; coherence length
 * REAL       p;                  input; exponent of filter factor
 * int        cmreset;            input; the covariance matrix is
 *                                       recomputed after cmreset steps
 * STATUS     *status;            output; return status
 */
{
	/* local variables */
	long     li, hi, mi; /* sample counter */
	COVMTX   cm;         /* covariance matrix */
	COVMTX   lm;         /* current matrix */
	REAL     fac;        /* filter factor */
	REAL     l[MXC_DIM]; /* eigenvalues */
	REAL     evec[MXC_DIM][MXC_DIM];   /* eigenvectors */
	int      big;        /* index of biggest eigenvalue */
	REAL     nvec[MXC_DIM]; /* normalized eigenvector of biggest value */
	REAL     norm;       /* normalization constant */

	/* executable code */

	pf_clrcm( cm );
	for  (li=(-n),hi=0; li<lth; li++,hi++)  {
		if  (li >= 0)  {
			pf_buildcm( xi[li], yi[li], zi[li], lm );
			pf_subcm( cm, lm );
		} /*endif*/
		if  (hi < lth)  {
			pf_buildcm( xi[hi], yi[hi], zi[hi], lm );
			pf_addcm( cm, lm );
		} /*endif*/
		mi = (li+hi)/2;
		if  (mi >=0  &&  mi < lth)  {
			if  ((mi+1) % cmreset == 0)      /* recompute cm to avoid */
				pf_calccm( xi, yi, zi, li>(-1) ? li+1 : 0, /* roundoff */
					hi<lth ? hi : lth-1, cm );
			pf_diagcm( cm, l, evec, &big, status );
			if  (Severe(status))  return;
			if  (l[big] > SHC_EPSILON)  {
				/* eigenvector is evec[i][big], i=0,1,2 */
				/* normalize eigenvector */
				norm = sqrt( evec[0][big]*evec[0][big] +
					evec[1][big]*evec[1][big] + evec[2][big]*evec[2][big] );
				if  (norm > SHC_EPSILON)  {
					nvec[0] = fabs( evec[0][big] / norm );
					nvec[1] = fabs( evec[1][big] / norm );
					nvec[2] = fabs( evec[2][big] / norm );
				} else {
					nvec[0] = nvec[1] = nvec[2] = 0.0;
				} /*endif*/
				if  (p != 1.)  {
					nvec[0] = pow( nvec[0], p );
					nvec[1] = pow( nvec[1], p );
					nvec[2] = pow( nvec[2], p );
				} /*endif*/
				xo[mi] = xi[mi]*nvec[0];
				yo[mi] = yi[mi]*nvec[1];
				zo[mi] = zi[mi]*nvec[2];
			} else {
				xo[mi] = 0.;
				yo[mi] = 0.;
				zo[mi] = 0.;
			} /*endif*/
		} /*endfor*/
		sy_sharecpu();
	} /*endfor*/

} /* end of pf_vecfilter */



/*-----------------------------------------------------------------------*/



void pf_eigenvector( REAL xi[], REAL yi[], REAL zi[], REAL xo[],
	REAL yo[], REAL zo[], long lth, int n, REAL p, int cmreset, STATUS *status )

/* Returns normalized and weighted largest eigenvector for each sample position
 *
 * parameters of routine
 * REAL       xi[], yi[], zi[];   input; 3-dim input seismogram
 * REAL       xo[], yo[], zo[];   output; largest eigenvector
 * long       lth;                input; length of traces
 * int        n;                  input; coherence length
 * REAL       p;                  input; exponent of filter factor
 * int        cmreset;            input; the covariance matrix is
 *                                       recomputed after cmreset steps
 * STATUS     *status;            output; return status
 */
{
	/* local variables */
	long     li, hi, mi; /* sample counter */
	COVMTX   cm;         /* covariance matrix */
	COVMTX   lm;         /* current matrix */
	REAL     fac;        /* filter factor */
	REAL     l[MXC_DIM]; /* eigenvalues */
	REAL     evec[MXC_DIM][MXC_DIM];   /* eigenvectors */
	int      big;        /* index of biggest eigenvalue */
	int      idx2, idx3; /* index of 2nd and 3rd largest eigenvector */
	REAL     nvec[MXC_DIM]; /* normalized eigenvector of biggest value */
	REAL     norm;       /* normalization constant */

	/* executable code */

	pf_clrcm( cm );
	for  (li=(-n),hi=0; li<lth; li++,hi++)  {
		if  (li >= 0)  {
			pf_buildcm( xi[li], yi[li], zi[li], lm );
			pf_subcm( cm, lm );
		} /*endif*/
		if  (hi < lth)  {
			pf_buildcm( xi[hi], yi[hi], zi[hi], lm );
			pf_addcm( cm, lm );
		} /*endif*/
		mi = (li+hi)/2;
		if  (mi >=0  &&  mi < lth)  {
			if  ((mi+1) % cmreset == 0)      /* recompute cm to avoid */
				pf_calccm( xi, yi, zi, li>(-1) ? li+1 : 0, /* roundoff */
					hi<lth ? hi : lth-1, cm );
			pf_diagcm( cm, l, evec, &big, status );
			if  (Severe(status))  return;
			switch  (big)  {
			case 0:  idx2 = 1;  idx3 = 2;  break;
			case 1:  idx2 = 0;  idx3 = 2;  break;
			default: idx2 = 0;  idx3 = 1;  break;
			} /*endswitch*/
			if  (fabs(l[idx2]) < fabs(l[idx3]))  idx2 = idx3;
			if  (fabs(l[big]) > SHC_EPSILON)  {
				/* eigenvector is evec[i][big], i=0,1,2 */
				/* normalize eigenvector */
				norm = sqrt( evec[0][big]*evec[0][big] +
					evec[1][big]*evec[1][big] + evec[2][big]*evec[2][big] );
				if  (norm > SHC_EPSILON)  {
					nvec[0] = fabs( evec[0][big] / norm );
					nvec[1] = fabs( evec[1][big] / norm );
					nvec[2] = fabs( evec[2][big] / norm );
				} else {
					nvec[0] = nvec[1] = nvec[2] = 0.0;
				} /*endif*/
				/* weight eigenvecor by polarization */
				norm = 1.0 - l[idx2]/l[big];
				if  (p != 1.)
					norm = pow( norm, p );
				xo[mi] = norm * nvec[0];
				yo[mi] = norm * nvec[1];
				zo[mi] = norm * nvec[2];
			} else {
				xo[mi] = 0.;
				yo[mi] = 0.;
				zo[mi] = 0.;
			} /*endif*/
		} /*endfor*/
		sy_sharecpu();
	} /*endfor*/

} /* end of pf_eigenvector */



/*-----------------------------------------------------------------------*/



void pf_poldir( REAL xi[], REAL yi[], REAL zi[], REAL xo[],
	REAL yo[], REAL zo[], long lth, SAMPLE poldir[], REAL p )

/* computes scalar product of each data point of the 3-dim input trace
 * and a given vector poldir (both vectors are normalized before).  The
 * data point then is weighted with the result powered by 'p'.
 *
 * parameters of routine
 * REAL       xi[], yi[], zi[];   input; 3-dim input seismogram
 * REAL       xo[], yo[], zo[];   output; filtered 3-dim seismogram
 * long       lth;                input; length of traces
 * int        poldir[];           input; polarization direction
 * REAL       p;                  input; exponent of filter factor
 */
{
	/* local variables */
	REAL     norm;                 /* normalization constant */
	SAMPLE   pol_z, pol_n, pol_e;  /* normalized polarization vector */
	SAMPLE   cur_z, cur_n, cur_e;  /* normalized observed vector */
	long     i;                    /* sample counter */

	/* executable code */

	/* normalize input vector */
	norm = sqrt( poldir[0]*poldir[0] + poldir[1]*poldir[1]
		+ poldir[2]*poldir[2] );
	if  (norm > SHC_EPSILON)  {
		pol_z = poldir[0] / norm;
		pol_n = poldir[1] / norm;
		pol_e = poldir[2] / norm;
	} else {
		pol_z = pol_n = pol_e = 0.0;
	} /*endif*/

	for  (i=0; i<lth; i++)  {

		norm = sqrt( xi[i]*xi[i] + yi[i]*yi[i] + zi[i]*zi[i] );
		if  (norm > SHC_EPSILON)  {
			cur_z = xi[i] / norm;
			cur_n = yi[i] / norm;
			cur_e = zi[i] / norm;
		} else {
			cur_z = cur_n = cur_e = 0.0;
		} /*endif*/

		/* compute powered scalar product */
		norm = fabs( cur_z*pol_z + cur_n*pol_n + cur_e*pol_e );
		if  (p != 1.0)  norm = pow( norm, p );

		xo[i] = xi[i] * norm;
		yo[i] = yi[i] * norm;
		zo[i] = zi[i] * norm;

	} /*endfor*/

} /* end of pf_poldir */



/*-----------------------------------------------------------------------*/



void pf_bazdir( REAL xi[], REAL yi[], REAL zi[], REAL xo[],
	REAL yo[], REAL zo[], long lth, REAL baz, REAL p )

/* computes scalar product of each data point of the N,E input trace
 * and a given back-azimuth (both vectors are normalized before).  The
 * 3-dim data point then is weighted with the result powered by 'p'.
 *
 * parameters of routine
 * REAL       xi[], yi[], zi[];   input; 3-dim input seismogram
 * REAL       xo[], yo[], zo[];   output; filtered 3-dim seismogram
 * long       lth;                input; length of traces
 * REAL       baz;                input; back-azimuth in deg
 * REAL       p;                  input; exponent of filter factor
 */
{
	/* local variables */
	REAL     norm;                 /* normalization constant */
	SAMPLE   pol_n, pol_e;         /* normalized polarization vector */
	SAMPLE   cur_n, cur_e;         /* normalized observed vector */
	long     i;                    /* sample counter */

	/* executable code */

	/* get 2-dim input vector */
	pol_n = cos( baz/SHC_RAD_TO_DEG );
	pol_e = sin( baz/SHC_RAD_TO_DEG );

	for  (i=0; i<lth; i++)  {

		norm = sqrt( yi[i]*yi[i] + zi[i]*zi[i] );
		if  (norm > SHC_EPSILON)  {
			cur_n = yi[i] / norm;
			cur_e = zi[i] / norm;
		} else {
			cur_n = cur_e = 0.0;
		} /*endif*/

		/* compute powered scalar product */
		norm = fabs( cur_n*pol_n + cur_e*pol_e );
		if  (p != 1.0)  norm = pow( norm, p );

		xo[i] = xi[i] * norm;
		yo[i] = yi[i] * norm;
		zo[i] = zi[i] * norm;

	} /*endfor*/

} /* end of pf_bazdir */



/*-----------------------------------------------------------------------*/



static void pf_addcm( COVMTX a, COVMTX b )

/* adds matrix b to a
 *
 * parameters of routine
 * COVMTX     a;       modify; b is added to this matrix
 * COVMTX     b;       input; to be added
 */
{
	/* local variables */
	int      m;        /* element counter */

	/* executable code */

	for  (m=0; m<COVMTXLTH; m++)
		a[m] += b[m];

} /* end of pf_addcm */



/*-----------------------------------------------------------------------*/



static void pf_subcm( COVMTX a, COVMTX b )

/* subtracts matrix b from a
 *
 * parameters of routine
 * COVMTX     a;       modify; b is subtracted from this matrix
 * COVMTX     b;       input; to be subtracted
 */
{
	/* local variables */
	int      m;        /* element counter */

	/* executable code */

	for  (m=0; m<COVMTXLTH; m++)
		a[m] -= b[m];

} /* end of pf_subcm */



/*-----------------------------------------------------------------------*/



static void pf_clrcm( COVMTX a )

/* resets matrix a
 *
 * parameters of routine
 * COVMTX     a;       output; to be reset
 */
{
	/* local variables */
	int      m;        /* element counter */

	/* executable code */

	for  (m=0; m<COVMTXLTH; m++)
		a[m] = 0.;

} /* end of pf_clrcm */



/*-----------------------------------------------------------------------*/



static void pf_buildcm( REAL x, REAL y, REAL z, COVMTX a )

/* computes covariance matrix from 3-dim point coordinates
 *
 * parameters of routine
 * REAL       x, y, z; input; coordinates of point
 * COVMTX     a;       output; to be reset
 */
{
	/* executable code */

	a[XX] = x*x;
	a[XY] = x*y;   a[YY] = y*y;
	a[XZ] = x*z;   a[YZ] = y*z;   a[ZZ] = z*z;

} /* end of pf_buildcm */



/*-----------------------------------------------------------------------*/



static void pf_calccm( REAL x[], REAL y[], REAL z[], long li,
	long hi, COVMTX c )

/* computes covariance matrix in sample window li..hi (inclusive
 * both sample bounds)
 *
 * parameters of routine
 * REAL       x[], y[], z[];    input; seismogram
 * long       li, hi;           input; sample window
 * COVMTX     c;                output; covariance matrix
 */
{
	/* local variables */
	long     i;           /* sample counter */
	COVMTX   lc;          /* dyade */

	/* executable code */

	pf_clrcm( c );
	for  (i=li; i<=hi; i++)  {
		pf_buildcm( x[i], y[i], z[i], lc );
		pf_addcm( c, lc );
	} /*endif*/

} /* end of pf_calccm */



/*-----------------------------------------------------------------------*/


static void pf_evalcm( COVMTX c, REAL *l1, REAL *l2, REAL *l3 )

/* computes eigenvalues of matrix a
 *
 * parameters of routine
 * COVMTX     c;             input; covariance matrix to be diagonalized
 * REAL       *l1, *l2, *l3; output; eigenvalues
 */
{
	/* local variables */
	REAL     cp0, cp1, cp2;  /* coefficients of characteristic polynomial */
	REAL     p, q;           /* corefficients of reduced equation */
	REAL     D;              /* discriminante */
	REAL     rho, phi;       /* scratch */

	/* executable code */

	/* characteristic polynomial: p(l) = l^3 + cp2*l^2 + cp1*l + cp0 */
	cp2 = -c[XX] - c[YY] - c[ZZ];
	cp1 = c[XX]*c[YY] + c[XX]*c[ZZ] + c[YY]*c[ZZ] -
			c[XY]*c[XY] - c[XZ]*c[XZ] - c[YZ]*c[YZ];
	cp0 = c[XX]*c[YZ]*c[YZ] + c[YY]*c[XZ]*c[XZ] + c[ZZ]*c[XY]*c[XY] -
			c[XX]*c[YY]*c[ZZ] - 2.0*c[XY]*c[YZ]*c[XZ];

	cp2 /= 3.;

	/* reduced equation: l^3 + p*l  + q = 0 */
	p = cp1 - 3.*cp2*cp2;
	q = 2.*cp2*cp2*cp2 - cp2*cp1 + cp0;
	if  (p > 0.)  {p = -p; printf( "*** this cannot happen ***\n" );}

	rho = sqrt( -p );
	rho = rho*rho*rho/5.1961524;    /* sqrt(27.) */
	D = (rho > SHC_EPSILON) ? -q/(2.*rho) : 0.;
	if  (D > 1.)  D = 1.;
	if  (D < -1.)  D = -1.;
	phi = acos( D );

	/* compute eigenvalues */
	D = (rho > 0.) ? 2.*pow(rho,1./3.) : 0.;
	phi /= 3.;
	*l1 = D * cos(phi);
	*l2 = D * cos(phi+2.*SHC_PI/3.);
	*l3 = D * cos(phi+4.*SHC_PI/3.);

	/* undo reduction */
	*l1 -= cp2;
	*l2 -= cp2;
	*l3 -= cp2;

	/* sort ev's by size */
	if  (*l1 < *l2)  {D = *l2; *l2 = *l1; *l1 = D;}
	if  (*l2 < *l3)  {D = *l3; *l3 = *l2; *l2 = D;}
	if  (*l1 < *l2)  {D = *l2; *l2 = *l1; *l1 = D;}

} /* end of pf_evalcm */



/*-----------------------------------------------------------------------*/



static void pf_diagcm( COVMTX c, REAL l[MXC_DIM],
	REAL trafo[MXC_DIM][MXC_DIM], int *big, STATUS *status )

/* Returns eiganvalues and eigenvectors of symmetrical matrix c.
 *
 * parameters of routine
 * COVMTX     c;                        input; covariance matrix
 * REAL       l[MXC_DIM];               output; eigenvalues
 * REAL       trafo[MXC_DIM][MXC_DIM];  output; eigenvectors
 * int        *big;                     output; index of biggest value
 * STATUS     *status;                  output; return status
 */
{
	/* local variables */
	REAL     sym_m[MXC_DIM][MXC_DIM];   /* symmetric input matrix */

	/* executable code */

	/* create input matrix */
	sym_m[0][0] = c[XX];  sym_m[0][1] = c[XY];  sym_m[0][2] = c[XZ];
	sym_m[1][0] = c[XY];  sym_m[1][1] = c[YY];  sym_m[1][2] = c[YZ];
	sym_m[2][0] = c[XZ];  sym_m[2][1] = c[YZ];  sym_m[2][2] = c[ZZ];

	/* diagonalize */
	mx_diag_matrix( sym_m, l, trafo, status );

	/* find biggest eigenvalue */
	*big = 0;
	if  (l[1] > l[*big])  *big = 1;
	if  (l[2] > l[*big])  *big = 2;

} /* end of pf_diagcm */



/*-----------------------------------------------------------------------*/

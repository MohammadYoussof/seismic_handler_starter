
/* file MATRIXHD.C
 *      ==========
 *
 * version 2, 22-MAY-2006
 *
 * matrix routines
 * K. Stammler, 9-JUL-1990
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
#include <math.h>
#include "basecnst.h"
#include BC_SYSBASE
#include "shconst.h"
#include "mxusrdef.h"


#define MXE_NODIAG 2201

/* global constants */
#define TINY 1.0e-20
#define MAX_ITERATION 300

#define Sign(r) (((r) > 0.0) ? 1.0 : -1.0)

/*------------------------------------------------------------------------*/



void mx_unit_matrix( REAL m[MXC_DIM][MXC_DIM] )

/* sets "m" to 1
 *
 * parameter of routine
 * REAL      m[MXC_DIM][MXC_DIM];  output; unit matrix
 */
{
	/* local variables */
	int      l, c;     /* counters */

	/* executable code */

	for  (l=0;l<MXC_DIM;l++)  {
		for  (c=0;c<MXC_DIM;c++)
			m[l][c] = 0.0;
		m[l][l] = 1.0;
	} /*endfor*/

} /* end of mx_unit_matrix */



/*------------------------------------------------------------------------*/



void mx_transpose_matrix( REAL m[MXC_DIM][MXC_DIM], REAL m_t[MXC_DIM][MXC_DIM] )

/* transpones the matrix "m".  Result is matrix "m_t"
 *
 * parameters of routine
 * REAL       m[MXC_DIM][MXC_DIM];   input; input matrix
 * REAL       m_t[MXC_DIM][MXC_DIM]; output; transponed matrix
 */
{
	/* local variables */
	int      l, c;     /* counters */

	/* executable code */

	for  (l=0;l<MXC_DIM;l++)
		for  (c=0;c<MXC_DIM;c++)
			m_t[l][c] = m[c][l];

} /* end of mx_transpose_matrix */



/*------------------------------------------------------------------------*/



void mx_copy_matrix( REAL m[MXC_DIM][MXC_DIM], REAL m_c[MXC_DIM][MXC_DIM] )

/* copies the matrix "m" to "m_c".
 *
 * parameters of routine
 * REAL       m[MXC_DIM][MXC_DIM];   input; input matrix
 * REAL       m_t[MXC_DIM][MXC_DIM]; output; copy of "m"
 */
{
	/* local variables */
	int      l, c;     /* counters */

	/* executable code */

	for  (l=0;l<MXC_DIM;l++)
		for  (c=0;c<MXC_DIM;c++)
			m_c[l][c] = m[l][c];

} /* end of mx_copy_matrix */



/*------------------------------------------------------------------------*/



void mx_mul_matrix( REAL m1[MXC_DIM][MXC_DIM],
	REAL m2[MXC_DIM][MXC_DIM], REAL res[MXC_DIM][MXC_DIM] )

/* multiplies "m1" and "m2".  Result is "res"
 *
 * parameters of routine
 * REAL       m1[MXC_DIM][MXC_DIM];   input; first factor
 * REAL       m2[MXC_DIM][MXC_DIM];   input; second factor
 * REAL       res[MXC_DIM][MXC_DIM];  output; result
 */
{
	/* local variables */
	int      l, c;     /* counters */
	int      i;        /* summation index */

	/* executable code */

	for  (l=0;l<MXC_DIM;l++)  {
		for  (c=0;c<MXC_DIM;c++)  {
			res[l][c] = 0.0;
			for  (i=0;i<MXC_DIM;i++)
				res[l][c] += m1[l][i]*m2[i][c];
		} /*endfor*/
	} /*endfor*/

} /* end of mx_mul_matrix */



/*------------------------------------------------------------------------*/



void mx_diag_matrix( REAL sym_m[MXC_DIM][MXC_DIM], REAL eval[MXC_DIM],
	REAL trafo[MXC_DIM][MXC_DIM], int *status )

/* diagonalises symmetrical matrix "sym_m".  Result is "eval" containing
 * the eigenvalues of "sym_m", and "trafo" containing the transformation
 * matrix, i.e. the eigenvectors.  To the eigenvalue "eval[i]"
 * corresponds the "i"-th column of the transformation matrix "trafo"
 * as an eigenvector (evec[j] = trafo[j][i], j = 1,..,MXC_DIM)
 * For diagonalising the Jacobi method is used.
 *
 * parameters of routine
 * REAL       sym_m[MXC_DIM][MXC_DIM];  input; input matrix
 * REAL       eval[MXC_DIM];            output; eigenvalues of "sym_m"
 * REAL       trafo[MXC_DIM][MXC_DIM];  output; transformation matrix
 * int        *status;                  output; return status
 */
{
	/* local variables */
	REAL     sum_limit;            /* exit criterion */
	REAL     m[MXC_DIM][MXC_DIM];  /* copy of "sym_m" */
	int      p, q;                 /* counters */
	REAL     rot[MXC_DIM][MXC_DIM]; /* elementary Jacobi rotation matrix */
	REAL     rot_t[MXC_DIM][MXC_DIM]; /* transponed of "rot" */
	REAL     diff;                    /* scratch */
	REAL     sum;                     /* convergence criterion */
	REAL     phi;                     /* angle of elementary rotation */
	int      loop_cnt;                /* iteration counter */
	REAL     help[MXC_DIM][MXC_DIM];  /* scratch matrix */

	/* executable code */

	/* clear eigenvalues and trafo */
	for  (p=0;p<MXC_DIM;p++)
		eval[p] = 0.0;
	mx_unit_matrix( trafo );

	mx_copy_matrix( sym_m, m );

	/* find sum limit */
	sum_limit = 0.0;
	for  (p=0;p<MXC_DIM;p++)
		for  (q=p;q<MXC_DIM;q++)
			if  (Abs(sym_m[p][q]) > sum_limit)
				sum_limit = Abs(sym_m[p][q]);
	sum_limit *= 1.0e-6;
	if  (sum_limit == 0.0)  return;

	loop_cnt = 0;
	FOREVER  {

		if  (++loop_cnt > MAX_ITERATION)  {
			*status = MXE_NODIAG;
			return;
		} /*endif*/

		/* test convergence */
		sum = 0.0;
		for  (p=0; p<(MXC_DIM-1); p++)
			for  (q=p+1; q<MXC_DIM ;q++)
				sum += m[p][q] * m[p][q];
		if  (sum < sum_limit)  break;

		/* elementary Jacobi rotation */
		for  (p=0; p<(MXC_DIM-1); p++)  {
			for  (q=p+1; q<MXC_DIM; q++)  {
				diff = -m[p][p] + m[q][q];
				if  (Abs(diff) > TINY)  {
					phi = 0.5 * atan( 2.0*m[p][q] / diff );
				} else {
					if  (Abs(m[p][q]) < TINY)  {
						phi = 0.0;
					} else {
						phi = Sign(m[p][q]) * Sign(diff) * SHC_PI * 0.25;
					} /*endif*/
				} /*endif*/
				mx_unit_matrix( rot );
				rot[p][p] = cos( phi );
				rot[p][q] = sin( phi );
				rot[q][p] = -rot[p][q];
				rot[q][q] = rot[p][p];
				mx_transpose_matrix( rot, rot_t );
				mx_mul_matrix( rot_t, m, help );
				mx_mul_matrix( help, rot, m );
				mx_mul_matrix( trafo, rot, help );
				mx_copy_matrix( help, trafo );
			} /*endfor*/
		} /*endfor*/

	} /*endfor*/

	for  (p=0;p<MXC_DIM;p++)
		eval[p] = m[p][p];

} /* end of mx_diag_matrix */



/*------------------------------------------------------------------------*/

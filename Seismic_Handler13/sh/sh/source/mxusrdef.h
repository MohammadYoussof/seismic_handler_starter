
/* file MXUSRDEF.H
 *      ==========
 *
 * version 2, 22-MAY-2006
 *
 * prototypes of module MATRIXHD.C
 * K. Stammler, 8-JUL-1990
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


#ifndef __MXUSRDEF
#define __MXUSRDEF

#ifndef __SHCONST
#include "shconst.h"
#endif


#define MXC_DIM 3
	/* dimension of space */


/*------------------------------------------------------------------------*/


void mx_unit_matrix( REAL m[MXC_DIM][MXC_DIM] );

/* sets "m" to 1
 *
 * parameter of routine
 * REAL      m[MXC_DIM][MXC_DIM];  output; unit matrix
 */


/*------------------------------------------------------------------------*/


void mx_transpose_matrix( REAL m[MXC_DIM][MXC_DIM], REAL m_t[MXC_DIM][MXC_DIM] );

/* transpones the matrix "m".  Result is matrix "m_t"
 *
 * parameters of routine
 * REAL       m[MXC_DIM][MXC_DIM];   input; input matrix
 * REAL       m_t[MXC_DIM][MXC_DIM]; output; transponed matrix
 */


/*------------------------------------------------------------------------*/


void mx_copy_matrix( REAL m[MXC_DIM][MXC_DIM], REAL m_c[MXC_DIM][MXC_DIM] );

/* copies the matrix "m" to "m_c".
 *
 * parameters of routine
 * REAL       m[MXC_DIM][MXC_DIM];   input; input matrix
 * REAL       m_t[MXC_DIM][MXC_DIM]; output; copy of "m"
 */


/*------------------------------------------------------------------------*/


void mx_mul_matrix( REAL m1[MXC_DIM][MXC_DIM],
	REAL m2[MXC_DIM][MXC_DIM], REAL res[MXC_DIM][MXC_DIM] );

/* multiplies "m1" and "m2".  Result is "res"
 *
 * parameters of routine
 * REAL       m1[MXC_DIM][MXC_DIM];   input; first factor
 * REAL       m2[MXC_DIM][MXC_DIM];   input; second factor
 * REAL       res[MXC_DIM][MXC_DIM];  output; result
 */


/*------------------------------------------------------------------------*/


void mx_diag_matrix( REAL sym_m[MXC_DIM][MXC_DIM], REAL eval[MXC_DIM],
	REAL trafo[MXC_DIM][MXC_DIM], int *status );

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


/*------------------------------------------------------------------------*/

#endif /* __MXUSRDEF */

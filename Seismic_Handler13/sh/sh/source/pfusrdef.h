
/* file PFUSRDEF.H
 *      ==========
 *
 * version 6, 22-May-2006
 *
 * prototypes of module POLFILTR.C
 * K. Stammler, 7-APR-91
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


#ifndef __PFUSRDEF
#define __PFUSRDEF


/*-----------------------------------------------------------------------*/


void pf_filter( REAL xi[], REAL yi[], REAL zi[], REAL xo[],
	REAL yo[], REAL zo[], long lth, int n, REAL p, int cmreset );

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


/*-----------------------------------------------------------------------*/


void pf_vecfilter( REAL xi[], REAL yi[], REAL zi[], REAL xo[],
	REAL yo[], REAL zo[], long lth, int n, REAL p, int cmreset, STATUS *status );

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


/*-----------------------------------------------------------------------*/


void pf_eigenvector( REAL xi[], REAL yi[], REAL zi[], REAL xo[],
	REAL yo[], REAL zo[], long lth, int n, REAL p, int cmreset, STATUS *status );

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


/*-----------------------------------------------------------------------*/


void pf_poldir( REAL xi[], REAL yi[], REAL zi[], REAL xo[],
	REAL yo[], REAL zo[], long lth, SAMPLE poldir[], REAL p );

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


/*-----------------------------------------------------------------------*/


void pf_bazdir( REAL xi[], REAL yi[], REAL zi[], REAL xo[],
	REAL yo[], REAL zo[], long lth, REAL baz, REAL p );

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


/*-----------------------------------------------------------------------*/

#endif /* __PFUSRDEF */

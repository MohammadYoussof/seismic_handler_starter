
/* file FCTXPM.H
 *      ========
 *
 * version 4, 22-May-2006
 *
 * prototypes of module SHPM
 * K. Stammler, 3-NOV-1990
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


#ifndef __FCTXPM
#define __FCTXPM


#define PMC_MAXPM 25
	/* maximum number of diagrams per window */


/*---------------------------------------------------------------------*/


void pm_setoutput( CHMAP wdw );

/* sets output window
 *
 * parameter of routine
 * CHMAP     wdw;       input; window channel(s)
 */


/*---------------------------------------------------------------------*/


void pm_fixedwdw( CHMAP ch, SAMPLE *x[], SAMPLE *y[], int trcno,
	long lth, char *cmt[], float zoom[], REAL begcirc, STATUS *status );

/* draws fixed window particle motion diagram
 *
 * parameters of routine
 * CHMAP      ch;         input; current graphic channel(s)
 * SAMPLE     *x[];       input; x coordinates
 * SAMPLE     *y[];       input; y coordinates
 * int        trcno;      input; number of diagrams
 * long       lth;        input; length of arrays in samples
 * char       *cmt[];     input; comment lines
 * float      zoom[];     input; zoom factors of traces
 * REAL       begcirc;    input; radius of start circle (drawn if > 0)
 * STATUS     *status;    output; return status
 */


/*---------------------------------------------------------------------*/


void pm_varwdw( CHMAP ch, SAMPLE *x[], SAMPLE *y[], int trcno,
	REAL mintime, REAL maxtime, REAL origtime, REAL dt, char *cmt[],
	float zoom[], REAL loy, REAL hiy, REAL begcirc, STATUS *status );

/* draws variable window particle motion diagrams
 *
 * parameters of routine
 * CHMAP      ch;         input; current graphic channel(s)
 * SAMPLE     *x[];       input; x coordinates
 * SAMPLE     *y[];       input; y coordinates
 * int        trcno;      input; number of diagrams
 * REAL       mintime;    input; minimum time (start time of traces)
 * REAL       maxtime;    input; maximum time (start time of traces)
 * REAL       origtime;   input; start of pm window
 * REAL       dt;         input; sample distance
 * char       *cmt[];     input; comment lines
 * float      zoom[];     input; zoom factors of traces
 * REAL       loy, hiy;   input; y range in trace window "ch"
 * REAL       begcirc;    input; radius of start circle (drawn if > 0)
 * STATUS     *status;    output; return status
 */


/*---------------------------------------------------------------------*/


void pm_movewdw( CHMAP ch, SAMPLE *x[], SAMPLE *y[], int trcno,
	REAL mintime, REAL maxtime, REAL width, REAL dt, char *cmt[],
	float zoom[], REAL loy, REAL hiy, REAL begcirc, STATUS *status );

/* draws variable window particle motion diagrams
 *
 * parameters of routine
 * CHMAP      ch;         input; current graphic channel(s)
 * SAMPLE     *x[];       input; x coordinates
 * SAMPLE     *y[];       input; y coordinates
 * int        trcno;      input; number of diagrams
 * REAL       mintime;    input; minimum time (start time of traces)
 * REAL       maxtime;    input; maximum time (start time of traces)
 * REAL       width;      input; width of pm window
 * REAL       dt;         input; sample distance
 * char       *cmt[];     input; comment lines
 * float      zoom[];     input; zoom factor of traces
 * REAL       loy, hiy;   input; y range in trace window "ch"
 * REAL       begcirc;    input; radius of start circle (drawn if > 0)
 * STATUS     *status;    output; return status
 */


/*---------------------------------------------------------------------*/

#endif /* __FCTXPM */

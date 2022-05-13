
/* file RFUSRDEF.H
 *      ==========
 *
 * version 4, 22-May-2006
 *
 * prototypes of module RECFILTR.C
 * K. Stammler, 22-MAY-91
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


#ifndef __RFUSRDEF
#define __RFUSRDEF

#ifndef __SHCONST
#include "shconst.h"
#endif

typedef struct {
	BOOLEAN     use_lpc;          /* use lin pred coding */
	int         pred_length;      /* prediction length in samples */
	int         pred_order;       /* prediction order */
	int         src_length;       /* length of source array */
	BOOLEAN     demean;           /* remove mean value on each stage */
} RFT_STARTUP;

#define RFC_MAXFILTER 8
	/* maximum number of filters */

/*------------------------------------------------------------------------*/


void rf_filter_input( int list_lth, char *flt_list[], int pos_list[],
	int *status );

/* reads one or more filters into memory
 *
 * paramters of routine
 * int       list_lth;       input; number of filters to read
 * char      *flt_list[];    input; list of filter names
 * int       pos_list[];     input; positions of filters in files
 * int       *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void rf_filter( SAMPLE src[], long trclth, REAL dt, RFT_STARTUP *lpc,
	SAMPLE dst[], STATUS *status );

/* filters the seismogram "src" from index "start" to index "end"
 * with all the filters in rfv_fct.  The result is returned in "dst"
 *
 * parameters of routine
 * SAMPLE     src[];     input; input signal to be filtered
 * long       trclth;    input length of trace in samples
 * REAL       dt;        input; sample distance in sec
 * RFT_STARTUP *lpc;     input; use special startup routine
 * SAMPLE     dst[];     output; output signal
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/

#endif /* __RFUSRDEF */

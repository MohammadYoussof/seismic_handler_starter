
/* file FCTXMNI.H
 *      =========
 *
 * version 6, 22-May-2006
 *
 * prototypes of module SHMENUIO.C
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


#ifndef __FCTXMNI
#define __FCTXMNI

#ifndef __CPUSRDEF
#include "cpusrdef.h"
#endif


/*------------------------------------------------------------------------*/


void mni_read( PARAM *par, int *status );

/* reads traces from q-file */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mni_write( PARAM *par, int *status );

/* writes traces to q-file
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * int      *status;        output; return status
 *
 * 1. par:  q-filename
 * 2. par:  trace list
 */


/*------------------------------------------------------------------------*/


void mni_writea( PARAM *par, int *status );

/* writes ascii file
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * int      *status;        output; return status
 *
 * 1. par:  ascii-filename
 * 2. par:  trace list
 * 3. - N. par:  additional info
 */


/*------------------------------------------------------------------------*/


void mni_reada( PARAM *par, STATUS *status );

/* reads ascii file
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * int      *status;        output; return status
 *
 * 1. par:  ascii-filename
 */


/*------------------------------------------------------------------------*/


void mni_readf( PARAM *par, STATUS *status );

/* reads foreign file
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * int      *status;        output; return status
 *
 * 1. par:  filename of foreign file
 * 2. par:  record
 */


/*----------------------------------------------------------------------------*/


void mni_reads( PARAM *par, STATUS *status );

/* reads traces from GRN files
 * par 1:  directory
 * par 2:  start time
 * par 3:  length in seconds
 * par 4:  station list
 * par 5:  component list
 * par 6:  stream name
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * int      *status;        output; return status
 */


/*------------------------------------------------------------------------*/

#endif /* __FCTXMNI */

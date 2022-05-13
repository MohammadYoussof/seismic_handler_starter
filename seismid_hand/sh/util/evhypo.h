
/* file evhypo.h
 *      ========
 *
 * version 3, 15-Oct-93
 *
 * header file of module evhypo.c
 * K. Stammler, 8-Sep-93
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


/*-------------------------------------------------------------------*/


void EfPutHypo( FILE *hypo, EvEventT *event, EvStatusT *status );

/* Prints event "event" in HYPOELLIPSE format to file "hypo".  This
 * routine may be used as a "put_routine" in "EvReformatEvenfile".
 *
 * parameters of routine
 * FILE       *hypo;          input; output file
 * EvEventT   *event;         input; event information
 * EvStatusT  *status;        output; return status
 */


/*-------------------------------------------------------------------*/


void EfSetHypoDepth( BOOLEAN fixed, float depth );

/* Sets fixed or free depth for instruction record.  "depth" is valid
 * only if "fixed" is TRUE.
 *
 * parameters of routine
 * BOOLEAN    fixed;        input; fixed depth if TRUE
 * float      depth;        input value of fixed depth in km
 */


/*-------------------------------------------------------------------*/


void EfSetHypoUseS( BOOLEAN use );

/* Enables/disables usage of S waves for location
 *
 * parameters of routine
 * BOOLEAN    use;        input; if TRUE, S waves are use
 */


/*-------------------------------------------------------------------*/


/* file ITUSRDEF.H
 *      ==========
 *
 * version 2, 22-May-2006
 *
 * error codes and prototypes of module INCITHEO
 * K. Stammler, 29-OCT-91
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


#ifndef __ITUSRDEF
#define __ITUSRDEF

#define ITE_OFFSET 3400

#define ITE_STROVFL     (ITE_OFFSET+1)    /* string overflow */
#define ITE_DISTRANGE   (ITE_OFFSET+2)    /* distance out of range */
#define ITE_DEPTHRANGE  (ITE_OFFSET+3)    /* depth out of range */
#define ITE_OPNRD       (ITE_OFFSET+4)    /* error opening input file */
#define ITE_FILREAD     (ITE_OFFSET+5)    /* error reading file */
#define ITE_FCNV        (ITE_OFFSET+6)    /* error converting float string */ 


/*------------------------------------------------------------------------*/


void it_settable( char table[], STATUS *status );

/* sets new table name
 *
 * parameters of routine
 * char       table[];      input; new table name
 * STATUS     *status;      output; return status
 */


/*------------------------------------------------------------------------*/


float it_getinci( float distance, float depth, STATUS *status );

/* returns angle of incidence in degrees
 *
 * parameters of routine
 * float      distance;     input; distance of event in degrees
 * float      depth;        input; depth of event in km
 * STATUS     *status;      output; return status
 *                          returns angle of incidence of P in degrees
 */


/*------------------------------------------------------------------------*/

#endif /* __ITUSRDEF */

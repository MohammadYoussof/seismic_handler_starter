
/* file TTERRORS.H
 *      ==========
 *
 * version 2, 22-MAY-2006
 *
 * error codes of module TRAVTIME
 * K. Stammler, 15-SEP-1990
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


#ifndef __TTERRORS
#define __TTERRORS

#define TTE_OFFSET         2700
#define TTE_FOPNRD         (TTE_OFFSET+1)  /* error opening input file */
#define TTE_FREAD          (TTE_OFFSET+2)  /* error reading file */
#define TTE_NOVELFIL       (TTE_OFFSET+3)  /* not a velocity file */
#define TTE_DEEPCNV        (TTE_OFFSET+4)  /* conversion too deep for table */
#define TTE_ILANGLE        (TTE_OFFSET+5)  /* illegal start angle */
#define TTE_NOTREADIN      (TTE_OFFSET+6)  /* no velocity table read in */
#define TTE_UKPHASE        (TTE_OFFSET+7)  /* unknown phase */
#define TTE_DEPTHOOR       (TTE_OFFSET+8)  /* depth out of range */
#define TTE_DISTOOR        (TTE_OFFSET+9)  /* distance out of range */
#define TTE_STROVFL        (TTE_OFFSET+10) /* string overflow */
#define TTE_UK_DIFF_PHASE  (TTE_OFFSET+11) /* unknown difference phase ID */

#endif /* __TTERRORS */


/* file PTERRORS.H
 *      ==========
 *
 * version 7, 22-May-2006
 *
 * error codes of module PTRAVTIM
 * K. Stammler, 18-Dec-91
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


#ifndef __PTERRORS
#define __PTERRORS

#define PTE_OFFSET     3500
#define PTE_STROVFL    (PTE_OFFSET+1)   /* string overflow */
#define PTE_FOPENRD    (PTE_OFFSET+2)   /* error opening input file */
#define PTE_NOTTTFILE  (PTE_OFFSET+3)   /* this is no TTT file */
#define PTE_FRDERR     (PTE_OFFSET+4)   /* read error on TTT file */
#define PTE_STEPOVFL   (PTE_OFFSET+5)   /* too many depth steps */
#define PTE_DISTOOR    (PTE_OFFSET+6)   /* distance out of range */
#define PTE_DEPTHOOR   (PTE_OFFSET+7)   /* depth out of range */
#define PTE_ZEROTIME   (PTE_OFFSET+8)   /* illegal depth-distance combination */
#define PTE_TTTERROR   (PTE_OFFSET+9)   /* error in TTT-file */
#define PTE_SLOWOOR    (PTE_OFFSET+10)  /* slowness out if range */
#define PTE_DIFFSTEPS  (PTE_OFFSET+11)  /* different depth steps in files */
#define PTE_DEPTHFIT   (PTE_OFFSET+12)  /* could not fit depth */
#define PTE_BUG        (PTE_OFFSET+13)  /* bug in program (should not happen) */
#define PTE_INCOMPTT   (PTE_OFFSET+14)  /* incompatible travel time tables */
#define PTE_DISTFIT    (PTE_OFFSET+15)  /* could not fit distance */

#endif /* __PTERRORS */

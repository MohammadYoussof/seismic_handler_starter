
/* file CPERRORS.H
 *      ==========
 *
 * version 2, 22-MAY-2006
 *
 * error codes of module CMDPARSE
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


#ifndef __CPERRORS
#define __CPERRORS

#define CPE_NOERROR 0

#define CPE_OFFSET     1100
#define CPE_POVFL      (CPE_OFFSET+1)   /* too many parameters */
#define CPE_QOVFL      (CPE_OFFSET+2)   /* too many qualifiers */
#define CPE_NOQUALTRM  (CPE_OFFSET+3)   /* no qualifier term. specified */
#define CPE_TOOMNYTRM  (CPE_OFFSET+4)   /* too many terminators */
#define CPE_PNUMEXC    (CPE_OFFSET+5)   /* number of parameters exceeded */
#define CPE_PNOTEX     (CPE_OFFSET+6)   /* attempt to access not exist. p */
#define CPE_STROVFL    (CPE_OFFSET+7)   /* output string too short */
#define CPE_CNVFLOAT   (CPE_OFFSET+8)   /* float conversion error */
#define CPE_CNVINT     (CPE_OFFSET+9)   /* integer conversion error */
#define CPE_VFOVFL     (CPE_OFFSET+10)  /* verify output too long */

#endif /* __CPERRORS */

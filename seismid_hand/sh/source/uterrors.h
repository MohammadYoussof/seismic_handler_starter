
/* file UTERRORS.H
 *      ==========
 *
 * version 3, 22-May-2006
 *
 * error codes of module UTILITY
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


#ifndef __UTERRORS
#define __UTERRORS

#define UTE_NOERROR   0
#define UTE_OFFSET    2100
#define UTE_ILNUMLTH  (UTE_OFFSET+1)   /* number string too long */
#define UTE_ILOVFL    (UTE_OFFSET+2)   /* too many integers */
#define UTE_ILCNV     (UTE_OFFSET+3)   /* conversion error */
#define UTE_ILMISS    (UTE_OFFSET+4)   /* two consecutive separators */
#define UTE_ILILLCH   (UTE_OFFSET+5)   /* illegal char in list string */
#define UTE_ILEND     (UTE_OFFSET+6)   /* string ends with separator */
#define UTE_STROVFL   (UTE_OFFSET+7)   /* string overflow */

#endif /* __UTERRORS */

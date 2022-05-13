
/* file QIERRORS.H
 *      ==========
 *
 * version 2, 22-MAY-2006
 *
 * errors codes of module QFINTRFC
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


#ifndef __QIERRORS
#define __QIERRORS

#define QIE_NOERROR     0
#define QIE_OFFSET      2500
#define QIE_TYPMSMCH    (QIE_OFFSET+1)   /* type mismatch */
#define QIE_ILENTRY     (QIE_OFFSET+2)   /* illegal entry number */
#define QIE_ILTYPE      (QIE_OFFSET+3)   /* illegal type */
#define QIE_NOTEMPTY    (QIE_OFFSET+4)   /* entry already used */

#endif /* __QIERRORS */

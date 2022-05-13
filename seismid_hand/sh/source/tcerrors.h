
/* file TCERRORS.H
 *      ==========
 *
 * version 2, 22-MAY-2006
 *
 * error codes of module TIMECONV
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


#ifndef __TCERRORS
#define __TCERRORS

#define TCE_NOERROR  0
#define TCE_OFFSET   1900
#define TCE_ILTERM   (TCE_OFFSET+1)   /* illegal terminator */
#define TCE_ILNAME   (TCE_OFFSET+2)   /* illegal month name */
#define TCE_NCONV    (TCE_OFFSET+3)   /* int conversion error */
#define TCE_ADDCH    (TCE_OFFSET+4)   /* additional chars at end of text */
#define TCE_TOOACC   (TCE_OFFSET+5)   /* too many fraction digits */
#define TCE_TYOOR    (TCE_OFFSET+6)   /* year value out of range */
#define TCE_TMOOR    (TCE_OFFSET+7)   /* month value out of range */
#define TCE_TDOOR    (TCE_OFFSET+8)   /* day value out of range */
#define TCE_THOOR    (TCE_OFFSET+9)   /* hour value out of range */
#define TCE_TIOOR    (TCE_OFFSET+10)  /* minute value out of range */
#define TCE_TSOOR    (TCE_OFFSET+11)  /* second value out of range */
#define TCE_TCOOR    (TCE_OFFSET+12)  /* millisecond value out of range */
#define TCE_INT7     (TCE_OFFSET+13)  /* too many integers in list */
#define TCE_NOSEP    (TCE_OFFSET+14)  /* no separator found */

#endif /* __TCERRORS */

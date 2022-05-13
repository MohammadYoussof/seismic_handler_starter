
/* file FLERRORS.H
 *      ==========
 *
 * version 4, 22-May-2006
 *
 * error codes of modules FFTFILTR & RECFILTR
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


#ifndef __FLERRORS
#define __FLERRORS

#define FLE_OFFSET 2000
#define FLE_NOERROR 0
#define FLE_TOOMANY   (FLE_OFFSET+1)   /* too many filters */
#define FLE_STROVFL   (FLE_OFFSET+2)   /* string overflow */
#define FLE_OPNREAD   (FLE_OFFSET+3)   /* input file open error */
#define FLE_FREAD     (FLE_OFFSET+4)   /* file read error */
#define FLE_NOMAGIC   (FLE_OFFSET+5)   /* not a filter file */
#define FLE_NORECFIL  (FLE_OFFSET+6)   /* not a recursive filter */
#define FLE_DEGOVFL   (FLE_OFFSET+7)   /* degree to large */
#define FLE_ZEROLTH   (FLE_OFFSET+8)   /* zero length trace as input */
#define FLE_NOFILTER  (FLE_OFFSET+9)   /* no filter read in */
#define FLE_DTMISMCH  (FLE_OFFSET+10)  /* sample distance of filter does not match */
#define FLE_NORATFCT  (FLE_OFFSET+11)  /* no zero & pole filter file */
#define FLE_EOFF      (FLE_OFFSET+12)  /* end of filter file */
#define FLE_COMPRESS  (FLE_OFFSET+13)  /* filter compressing not possible */
#define FLE_NOFILFUNC (FLE_OFFSET+14)  /* no digital filter function */
#define FLE_READERR   (FLE_OFFSET+15)  /* read error on file */
#define FLE_FILFRANGE (FLE_OFFSET+16)  /* range error in digital filter */
#define FLE_STARTUP   (FLE_OFFSET+17)  /* error in parameters for startup */

#endif /* __FLERRORS */

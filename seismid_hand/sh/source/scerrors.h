
/* file SCERRORS.H
 *      ==========
 *
 * version 2, 22-MAY-2006
 *
 * error codes of module SHCORR
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


#ifndef __SCERRORS
#define __SCERRORS

#define SCE_OFFSET   2400
#define SCE_NOERROR  0
#define SCE_ZEROWDW  (SCE_OFFSET+1)   /* zero window specified */
#define SCE_SHORTTRC (SCE_OFFSET+2)   /* 2. correlation trace too short */
#define SCE_UKCCMODE (SCE_OFFSET+3)   /* unknown correlation mode */
#define SCE_MXDIM    (SCE_OFFSET+4)   /* wrong dimension in MATRIXHD.C */

#endif /* __SCERRORS */


/* file SYERRORS.H
 *      ==========
 *
 * version 5, 22-May-2006
 *
 * error codes of module SYSCALL
 * K. Stammler, 16-NOV-91
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

#define SYE_NOERROR  0
#define SYE_OFFSET   1500
#define SYE_NOLONG   (SYE_OFFSET+1)    /* only word length implemented */
#define SYE_MEMOVFL  (SYE_OFFSET+2)    /* memory overlow */
#define SYE_OSCALL   (SYE_OFFSET+3)    /* error calling operating system */
#define SYE_NOTIMPL  (SYE_OFFSET+4)    /* not implemented */
#define SYE_LOCALINF (SYE_OFFSET+5)    /* error in localinf routine */
#define SYE_STROVFL  (SYE_OFFSET+6)    /* string overflow */
#define SYE_ZEROALLOC (SYE_OFFSET+7)   /* zero allocation */
#define SYE_SUBMIT   (SYE_OFFSET+8)    /* error submitting job */

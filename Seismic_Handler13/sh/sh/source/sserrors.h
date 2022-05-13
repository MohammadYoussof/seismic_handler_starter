
/* file SSERRORS.H
 *      ==========
 *
 * version 2, 22-MAY-2006
 *
 * error codes of module SHSYMBOL
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


#ifndef __SSERRORS
#define __SSERRORS

#define SSE_NOERROR   0
#define SSE_OFFSET    2300
#define SSE_ILSET     (SSE_OFFSET+1)   /* illegal set number */
#define SSE_OVFL      (SSE_OFFSET+2)   /* no more symbols definable */
#define SSE_UDSYM     (SSE_OFFSET+3)   /* undefined symbol */
#define SSE_VALLTH    (SSE_OFFSET+4)   /* symbol value too long */
#define SSE_NAMLTH    (SSE_OFFSET+5)   /* symbol name too long */
#define SSE_RETLTH    (SSE_OFFSET+6)   /* value output string too short */
#define SSE_CPOVFL    (SSE_OFFSET+7)   /* not existing command parameter */
#define SSE_CNVNUM    (SSE_OFFSET+8)   /* error converting number */
#define SSE_PUSH      (SSE_OFFSET+9)   /* error saving cmdpar's & symbols */
#define SSE_POP       (SSE_OFFSET+10)  /* error popping cmdpar's & symbols*/
#define SSE_DEFAULT   (SSE_OFFSET+11)  /* illegal par-no in default cmd */
#define SSE_DEFLTH    (SSE_OFFSET+12)  /* default string too long */

#endif /* __SSERRORS */

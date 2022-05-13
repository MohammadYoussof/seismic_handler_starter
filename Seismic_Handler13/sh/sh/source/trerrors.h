
/* file TRERRORS.H
 *      ==========
 *
 * version 2, 22-MAY-2006
 *
 * error codes of module SHTRANSL
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


#ifndef __TRERRORS
#define __TRERRORS

#define TRE_NOERROR    0
#define TRE_OFFSET     2200
#define TRE_ILIDX      (TRE_OFFSET+1)    /* illegal index string */
#define TRE_PRADD      (TRE_OFFSET+2)    /* additional chars */
#define TRE_CNVIDX     (TRE_OFFSET+3)    /* index conversion error */
#define TRE_OPNRD      (TRE_OFFSET+4)    /* input file open error */
#define TRE_RDFIL      (TRE_OFFSET+5)    /* error reading file (or EOF) */
#define TRE_ILTYPE	  (TRE_OFFSET+6)	  /* illegal entry type */
#define TRE_STROVFL	  (TRE_OFFSET+7)	  /* string overflow */
#define TRE_UKINTERN   (TRE_OFFSET+8)    /* unknown internal variable */
#define TRE_CCOVFL     (TRE_OFFSET+9)    /* too many concatenations */
#define TRE_HEXCHAR    (TRE_OFFSET+10)   /* illegal HEXCHAR expression */
#define TRE_ILWDW      (TRE_OFFSET+11)   /* illegal window number */

#endif /* __TRERRORS */

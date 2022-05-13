
/* file QFERRORS.H
 *      ==========
 *
 * version 5, 22-May-2006
 *
 * error code of module QFILEIO
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


#ifndef __QFERRORS
#define __QFERRORS

#define QFE_NOERROR  0
#define QFE_OFFSET   1300
#define QFE_NAMLTH   (QFE_OFFSET+1)   /* q-file name too long */
#define QFE_OPNHDR   (QFE_OFFSET+2)   /* header file open error */
#define QFE_RDHDR    (QFE_OFFSET+3)   /* read error in header file */
#define QFE_NOQFILE  (QFE_OFFSET+4)   /* no q-file */
#define QFE_HDRPARSE (QFE_OFFSET+5)   /* error parsing header */
#define QFE_IBOVFL   (QFE_OFFSET+6)   /* info block overflow */
#define QFE_LNGENT   (QFE_OFFSET+7)   /* info entry too long */
#define QFE_UDENT    (QFE_OFFSET+8)   /* undefined entry type */
#define QFE_HDROVFL  (QFE_OFFSET+9)   /* header overflow */
#define QFE_TRCOVFL  (QFE_OFFSET+10)  /* not enough trace lines */
#define QFE_OPNBIN   (QFE_OFFSET+11)  /* binary file open error */
#define QFE_MEMOVFL  (QFE_OFFSET+12)  /* memory overflow (calloc failed) */
#define QFE_RDBIN    (QFE_OFFSET+13)  /* read error on binary file */
#define QFE_ILLTRCNO (QFE_OFFSET+14)  /* illegal trace number */
#define QFE_MISSENT  (QFE_OFFSET+15)  /* missing pos or length entry */
#define QFE_EOFH     (QFE_OFFSET+16)  /* end of file found in q-header */
#define QFE_EOFB     (QFE_OFFSET+17)  /* end of file found in binary file */
#define QFE_OPNTWICE (QFE_OFFSET+18)  /* file descriptor not free */
#define QFE_WRTLNG   (QFE_OFFSET+19)  /* data array too long */
#define QFE_WRBIN    (QFE_OFFSET+20)  /* write error on binary file */
#define QFE_EMPTY    (QFE_OFFSET+21)  /* empty info entry */
#define QFE_ILLIDX   (QFE_OFFSET+22)  /* illegal info index */
#define QFE_TYPERR   (QFE_OFFSET+23)  /* type error in index */
#define QFE_UDTYP    (QFE_OFFSET+24)  /* undefined type in index */
#define QFE_CLSFIL   (QFE_OFFSET+25)  /* error closing file */
#define QFE_OPNCMT   (QFE_OFFSET+26)  /* error opening comment file */
#define QFE_OPNOUT   (QFE_OFFSET+27)  /* error opening output file */
#define QFE_ILSMPWDW (QFE_OFFSET+28)  /* illegal window in qf_read_wdw */

#endif /* __QFERRORS */

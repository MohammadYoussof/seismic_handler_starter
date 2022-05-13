
/* file SHERRORS.H
 *      ==========
 *
 * version 9, 22-May-2006
 *
 * error codes of seismhandler program
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


#ifndef __SHERRORS
#define __SHERRORS

#define SHE_NOERROR     0
#define SHE_OFFSET      1700
#define SHE_SPECERROR   1800             /* special errors 1801..1899 */
#define SHE_MEMOVFL     (SHE_OFFSET+1)   /* memory allocation error */
#define SHE_TYPMTCH     (SHE_OFFSET+2)   /* type index mismatch */
#define SHE_NOINFO      (SHE_OFFSET+3)   /* info not found */
#define SHE_STRENT      (SHE_OFFSET+4)   /* entry string overflow */
#define SHE_DSPZERO     (SHE_OFFSET+5)   /* zero display length */
#define SHE_NOHELP      (SHE_OFFSET+6)   /* no help available for this item */
#define SHE_OPNWR       (SHE_OFFSET+7)   /* output file open error */
#define SHE_NOSYM       (SHE_OFFSET+8)   /* symbol name expected */
#define SHE_CALC        (SHE_OFFSET+9)   /* calc parameter error */
#define SHE_ILTYPE      (SHE_OFFSET+10)  /* illegal type */
#define SHE_LESSPAR     (SHE_OFFSET+11)  /* too less parameters passed */
#define SHE_ILPAR       (SHE_OFFSET+12)  /* illegal parameter */
#define SHE_QUIT        (SHE_OFFSET+13)  /* quit command not interactive */
#define SHE_CPATH       (SHE_OFFSET+14)  /* command path name too long */
#define SHE_REQENT      (SHE_OFFSET+15)  /* required info entry missing */
#define SHE_UDNAME      (SHE_OFFSET+16)  /* undefined info entry name */
#define SHE_INFNAM      (SHE_OFFSET+17)  /* info entry name too long */
#define SHE_UKTYPE      (SHE_OFFSET+18)  /* unknown info entry type */
#define SHE_UKFCT       (SHE_OFFSET+19)  /* unknown FCT subfunction */
#define SHE_ZWDW        (SHE_OFFSET+20)  /* zero time window set */
#define SHE_UKAXIS      (SHE_OFFSET+21)  /* unknown axis name */
#define SHE_UKITEM      (SHE_OFFSET+22)  /* unknown axis item */
#define SHE_UKCREAT     (SHE_OFFSET+23)  /* unknown synthetic type */
#define SHE_TLOVFL      (SHE_OFFSET+24)  /* trace list overflow */
#define SHE_STROVFL     (SHE_OFFSET+25)  /* string overflow */
#define SHE_UKFLAG      (SHE_OFFSET+26)  /* unknown flag */
#define SHE_UKKEY       (SHE_OFFSET+27)  /* unknown keyword */
#define SHE_ABORT       (SHE_OFFSET+28)  /* command aborted */
#define SHE_ILDEFLT     (SHE_OFFSET+29)  /* illegal default value */
#define SHE_ILSMP       (SHE_OFFSET+30)  /* illegal sample index */
#define SHE_DIFFSAMP    (SHE_OFFSET+31)  /* operation not allowed for diff dt */
#define SHE_ENTTWICE    (SHE_OFFSET+32)  /* entry name already used */
#define SHE_ENTOVFL     (SHE_OFFSET+33)  /* no more entry name definable */
#define SHE_WMODIF      (SHE_OFFSET+34)  /* write to q-file of modified trace */
#define SHE_NOTRC       (SHE_OFFSET+35)  /* no trace at this position */
#define SHE_OPNRD       (SHE_OFFSET+36)  /* input file open error */
#define SHE_INTCNV      (SHE_OFFSET+37)  /* error converting integer */
#define SHE_ERDONLY     (SHE_OFFSET+38)  /* info entry is read-only */
#define SHE_NOLIST      (SHE_OFFSET+39)  /* no list specified */
#define SHE_REALCNV     (SHE_OFFSET+40)  /* error converting real */
#define SHE_BUG         (SHE_OFFSET+41)  /* program bug detected */
#define SHE_EXIT        (SHE_OFFSET+42)  /* selection exit */
#define SHE_EMPTYLST    (SHE_OFFSET+43)  /* empty display list */
#define SHE_PMWDW       (SHE_OFFSET+44)  /* illegal pm window */
#define SHE_NOQFILE     (SHE_OFFSET+45)  /* source file is not a q-file */
#define SHE_FILREAD     (SHE_OFFSET+46)  /* error reading file */
#define SHE_SMPOVFL     (SHE_OFFSET+47)  /* too many samples */
#define SHE_NOTIMPL     (SHE_OFFSET+48)  /* not implemented */
#define SHE_SHORTTRC    (SHE_OFFSET+49)  /* trace too short */
#define SHE_LOWRCASE    (SHE_OFFSET+50)  /* lowercase command */
#define SHE_CRSRSEL     (SHE_OFFSET+51)  /* cursor selection requested */
#define SHE_NO_STATVEL  (SHE_OFFSET+52)  /* no station velocity specified */
#define SHE_NO_STATELEV (SHE_OFFSET+53)  /* no station elevation specified */
#define SHE_NO_REFSTAT  (SHE_OFFSET+54)  /* reference station not found */
#define SHE_SYSCMDFAIL  (SHE_OFFSET+55)  /* error in system command */

#endif /* __SHERRORS */

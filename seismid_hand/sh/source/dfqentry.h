/* file DFQENTRY.H
 *      ==========
 *
 * version 5, 22-May-2006
 *
 * default q-entry numbers
 * K. Stammler, 28-SEP-1990
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

#ifndef __DFQENTRY
#define __DFQENTRY

#ifndef __QFUSRDEF
#include BC_QFUSRDEF
#endif

#define QEC_L_DATALTH    (1|QFC_LTYPE)
#define QEC_I_SIGN       (11|QFC_ITYPE)
#define QEC_I_EVENTNO    (12|QFC_ITYPE)
#define QEC_I_RESERVED   (13|QFC_ITYPE)
#define QEC_I_MARK       (14|QFC_ITYPE)

#define QEC_R_DELTA      (0|QFC_RTYPE)
#define QEC_R_DISTANCE   (11|QFC_RTYPE)
#define QEC_R_AZIMUTH    (12|QFC_RTYPE)
#define QEC_R_INCIDENCE  (13|QFC_RTYPE)
#define QEC_R_DEPTH      (14|QFC_RTYPE)
#define QEC_R_MAGNITUDE  (15|QFC_RTYPE)
#define QEC_R_LATITUDE   (16|QFC_RTYPE)
#define QEC_R_LONGITUDE  (17|QFC_RTYPE)
#define QEC_R_SLOWNESS   (18|QFC_RTYPE)
#define QEC_R_DATAZ      (19|QFC_RTYPE)
#define QEC_R_DATINCI    (20|QFC_RTYPE)
#define QEC_R_RESERVED   (21|QFC_RTYPE)
#define QEC_R_SIGNOISE   (22|QFC_RTYPE)

#define QEC_S_COMMENT    (0|QFC_STYPE)
#define QEC_S_STATION    (1|QFC_STYPE)
#define QEC_S_FILTER     (11|QFC_STYPE)

#define QEC_C_COMP       (0|QFC_CTYPE)
#define QEC_C_CHAN1      (1|QFC_CTYPE)
#define QEC_C_CHAN2      (2|QFC_CTYPE)

#define QEC_T_START      (21|QFC_STYPE)
#define QEC_T_PONSET     (22|QFC_STYPE)
#define QEC_T_SONSET     (23|QFC_STYPE)
#define QEC_T_ORIGIN     (24|QFC_STYPE)

#endif /* __DFQENTRY */



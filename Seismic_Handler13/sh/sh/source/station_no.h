
/* file station_no.h
 *      ============
 *
 * version 4, 22-May-2006
 *
 * station numbers for GRF and GRSN
 * K. Stammler, 3-Mar-93
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


#define STC_GRA1     0
#define STC_GRA2     1
#define STC_GRA3     2
#define STC_GRA4     3
#define STC_GRB1     4
#define STC_GRB2     5
#define STC_GRB3     6
#define STC_GRB4     7
#define STC_GRB5     8
#define STC_GRC1     9
#define STC_GRC2    10
#define STC_GRC3    11
#define STC_GRC4    12
#define STC_BFO     13
#define STC_BRNL    14
#define STC_BUG     15
#define STC_CLZ     16
#define STC_FUR     17
#define STC_MOX     18
#define STC_TNS     19
#define STC_HAM     20
#define STC_WET     21
#define STC_CLL     22
#define STC_BRG     23
#define STC_LID     24
#define STC_GRFO    25
#define STC_RESVD4  26
#define STC_RESVD5  27
#define STC_RESVD6  28

#define STC_LASTSTATION (STC_GRFO+1)
#define STC_FIRST_GRSN STC_BFO
#define STC_LAST_GRSN STC_GRFO
#define STC_NUM_GRSN (STC_LAST_GRSN-STC_FIRST_GRSN+1)
#define STC_FIRST_GRF STC_GRA1
#define STC_LAST_GRF STC_GRC4
#define STC_NUM_GRF (STC_LAST_GRF-STC_FIRST_GRF+1)
#define STC_NUM_ALL (STC_GRFO+1)

#define STC_ARRAY_GRF 0
#define STC_ARRAY_GRSN 1

#define STC_COMP_Z   0
#define STC_COMP_N   1
#define STC_COMP_E   2
#define STC_NUM_COMP 3


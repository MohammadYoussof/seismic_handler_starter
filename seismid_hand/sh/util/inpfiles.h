
/* file inpfiles.h
 *      ==========
 *
 * version 5, 6-Sep-93
 *
 * input files for several programs
 * K. Stammler, 5-Aug-92
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



#ifdef BC_ATARI
#define IF_PATH "e:\\pc\\sh\\inputs\\"
#define IF_FERINDEXFILE "fereg.dat"
#define IF_FERNAMEFILE "fername.dat"
#define IF_STATLOCFILE "statloc.dat"
#endif

#ifdef BC_VAX
#define IF_PATH "shc_inputs:"
#define IF_FERINDEXFILE "fereg.dat"
#define IF_FERNAMEFILE "fername.dat"
#define IF_STATLOCFILE "statloc.dat"
#define IF_STATINFFILE "statinf.dat"
#endif

#ifdef BC_SUN
#define IF_PATH "/home/grfsun/klaus/sh/inputs/"
#define IF_FERINDEXFILE "FEREG.DAT"
#define IF_FERNAMEFILE "FERNAME.DAT"
#define IF_STATLOCFILE "STATLOC.DAT"
#define IF_STATINFFILE "STATINF.DAT"
#define IF_PDEDIR "/home/grfsun/klaus/sh/pde/"
#define IF_TRAVELDIR "/home/grfsun/klaus/sh/inputs/"
#endif


/* file SHDIRS.H
 *      ========
 *
 * version 8, 15-Dec-2006
 *
 * default values of directory strings
 * K. Stammler, 8-JUL-91
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


#include "basecnst.h"


/*=================================================================*/
#ifdef BC_ATARI



#define DD_SCRATCH "SHC_SCRATCH:"
#define DD_HELP "SHC_HELP:"
#define DD_CMD  "SHC_COMMAND:"
#define DD_FILTER "SHC_FILTER:"
#define DD_GLOBALS "SHC_GLOBALS:"
#define DD_ERRORS "SHC_ERRORS:"
#define DD_INPUTS "SHC_INPUTS:"
#define DD_EXTPROG "SHC_EXTPROG:"



#endif /* BC_ATARI */
/*=================================================================*/
#ifdef BC_VAX



#define DD_SCRATCH "SYS$LOGIN:"
#define DD_HELP "SHC_HELP:"
#define DD_CMD  "SHC_COMMAND:"
#define DD_FILTER "SHC_FILTER:"
#define DD_GLOBALS "SHC_GLOBALS:"
#define DD_ERRORS "SHC_ERRORS:"
#define DD_INPUTS "SHC_MAIN:"
#define DD_EXTPROG "SHC_EXTPROG:"



#endif /* BC_VAX */
/*=================================================================*/
#ifdef BC_SUN



/* These values are not really needed.  The SH_... environment variables
 * have precedence over these definitions
 */

#define DD_SCRATCH "/home/geo/ks/shc/shscratch/"
#define DD_HELP "/home/geo/ks/shc/help/"
#define DD_CMD  "/home/geo/ks/shc/command/"
#define DD_FILTER "/home/geo/ks/shc/filter/"
#define DD_GLOBALS "/home/geo/ks/shc/globals/"
#define DD_ERRORS "/home/geo/ks/shc/errors/"
#define DD_INPUTS "/home/geo/ks/shc/inputs/"
#define DD_EXTPROG "/home/geo/ks/shc/util/"



#endif /* BC_SUN */
/*=================================================================*/

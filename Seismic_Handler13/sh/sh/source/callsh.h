
/* file callsh.h
 *      ========
 *
 * version 2, 22-May-2006
 *
 * header file of module callsh.c
 * K. Stammler, 22-Feb-93
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


/*--------------------------------------------------------------------------*/


void callsh( char cmdline[], BOOLEAN *redraw, STATUS *status );

/* executes command line
 *
 * parameters of routine
 * char       cmdline[];     input; SH command line
 * BOOLEAN    *redraw;       output; redraw display
 * STATUS     *status;       output; return status
 */


/*--------------------------------------------------------------------------*/

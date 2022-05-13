
/* file FCTXHL.H
 *      ========
 *
 * version 2, 22-MAY-2006
 *
 * prototypes of module SHHELP.C
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


#ifndef __FCTXHL
#define __FCTXHL

/* prototypes of module SHHELP.C */

/*------------------------------------------------------------------------*/


void hl_full( int ch, char *dir, char *cmds, int *status );

/* displays full help file matching the wild card string "cmds" */

/* parameters of routine */
/* int      ch;             input; output channel(s) */
/* char     *dir;           input; directory string to look for */
/* char     *cmds;          input; command wild card string */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void hl_hdr( int ch, char *dir, char *cmds, int *status );

/* displays header line of help file matching the wild card string "cmds" */

/* parameters of routine */
/* int      ch;             input; output channel(s) */
/* char     *dir;           input; directory string to look for */
/* char     *cmds;          input; command wild card string */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void hl_dir( int ch, char *dir, char *cmds, int linewidth, int *status );

/* displays directory of help file matching the wild card string "cmds" */

/* parameters of routine */
/* int      ch;             input; output channel(s) */
/* char     *dir;           input; directory string to look for */
/* char     *cmds;          input; command wild card string */
/* int      linewidth;      input; linewidth in characters */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void hl_key( int ch, char *dir, char *cmds, int linewidth, int *status );

/* displays key line of help file matching the wild card string "cmds" */

/* parameters of routine */
/* int      ch;             input; output channel(s) */
/* char     *dir;           input; directory string to look for */
/* char     *cmds;          input; command wild card string */
/* int      linewidth;      input; width of line in chars */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/

#endif /* __FCTXHL */

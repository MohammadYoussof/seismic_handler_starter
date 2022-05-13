
/* file UTUSRDEF.H
 *      ==========
 *
 * version 7, 22-May-2006
 *
 * prototypes of module UTILITY.C
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


#ifndef __UTUSRDEF
#define __UTUSRDEF


void ut_ilist( char *liststr, int maxlth, int *list, int *listlth, int *status );

/* converts the list string "liststr" to a list of integers.  The numbers
   must be separated by commas "," or hyphens "-" (for contiguous blocks
   of numbers). */

/* parameters of routine */
/* char     *liststr;       input; list string */
/* int      maxlth;         input; maximum length of list */
/* int      *list;          output; list of integer */
/* int      *listlth;       output; length of list */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void ut_nlist( char *liststr, char *seps, int maxlth, int *list, int *listlth, int *status );

/* converts the list string "liststr" to a list of integers.  The numbers
   must be separated by separators listed in "seps" */

/* parameters of routine */
/* char     *liststr;       input; list string */
/* char     *seps;          input; permitted separators */
/* int      maxlth;         input; maximum length of string */
/* int      *list;          output; list of integer */
/* int      *listlth;       output; length of list */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


int ut_chinstr( char *str, char ch );

/* checks whether "ch" is in "str" or not */

/* parameters of routine */
/* char     *str;           input; search string */
/* char     ch;             input; char to look for */


/*------------------------------------------------------------------------*/


void ut_cap( char *str );

/* capitalizes str */

/* parameters of routine */
/* char     *str;           modify; string to be capitalized */


/*------------------------------------------------------------------------*/


void ut_uncap( char *str );

/* uncapitalizes str
 *
 * parameters of routine
 * char       *str;           modify; string to be uncapitalized
 */


/*------------------------------------------------------------------------*/


void ut_defext( char *file, int maxlth, char *defext, int *status );

/* appends default extension "defext" to filename "file", if there
 * is no extension specified
 *
 * parameters of routine
 * char       *file;    modify; filename where to check extension
 * int        maxlth;   input; maximum length of filename
 * char       *defext;  input; default extension to append (incl ".")
 * int        *status;  output; return status
 */


/*-------------------------------------------------------------------------*/


long ut_next2pow( long l );

/* returns next power of 2
 *
 * parameter of routine
 * long      l;      input; input number
 */


/*------------------------------------------------------------------------*/


void ut_replacechar( char str[], char fch, char rch );

/* replaces char fch by char rch in string str
 *
 * parameters of routine
 * char       str[];     modify; string to replace fch by rch
 * char       fch;       input; find character
 * char       rch;       input; replace character
 */


/*------------------------------------------------------------------------*/

#endif /* __UTUSRDEF */


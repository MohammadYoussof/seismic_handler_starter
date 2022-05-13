
/* file FOUSRDEF.H
 *      ==========
 *
 * version 5, 22-May-2006
 *
 * prototypes of module FILEOPEN.C
 * K. Stammler, 4-Jan-92
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


/* default logical name table */
#define FOC_DEFAULTLOGICALS "e:\\pc\\sh\\shpaths.lnt"
#define FOC_ALTERNATELOGICALS "e:\\pc\\sh\\inputs\\shpathsm.lnt"

/* fo_translate result codes */
#define FOC_NOMATCH  1
#define FOC_ANOTHER  2
#define FOC_LAST     3
#define FOC_OVFL     4


/*----------------------------------------------------------------------*/


void fo_readtable( char table[], STATUS *status );

/* initializes global variables
 *
 * parameters of routine
 * char       table[];     input; logical name table
 * STATUS     *status;     output; return status
 */


/*----------------------------------------------------------------------*/


FILE *fo_fopen( char fname[], char access[] );

/* opens a file, using the paths of the actual logical name table
 *
 * parameters of routine
 * char       fname[];     input; name of file to be opened
 * char       access[];    input; access string, as in usual fopen
 *                         returns file pointer
 */


/*----------------------------------------------------------------------*/


void fo_translate( char in[], BOOLEAN first, int maxlth, char out[],
	int *result );

/* If first=TRUE the filename is translated using logical name table.
 * Otherwise the parameter "in" is ignored and the next possible
 * translation of the previous call is returned.  "*result" can have
 * the values FOC_NOTMATCH (no matching logical name found, ->
 * out=in), FOC_ANOTHER (logical name was translated and it exists
 * another translation for the same logical name) and FOC_LAST (the
 * logical name was translated, there exist no more equivalences).
 * If the resulting string is too long FOC_OVFL is returned.
 *
 * parameters of routine
 * char       in[];      input; input filename
 * BOOLEAN    first;     input; first equivalence or not
 * int        maxlth;    input; maximum length of output string
 * char       out[];     output; translated filename
 * int        *result;   output; status message
 */


/*----------------------------------------------------------------------*/

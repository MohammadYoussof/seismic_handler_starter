
/* file ERUSRDEF.H
 *      ==========
 *
 * version 3, 22-May-2006
 *
 * prototypes of module ERRMSG.C
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


/* extensions of errors files */
#define EMC_ERRFILEEXT ".MSG"



/*-------------------------------------------------------------------*/


void err_seterrdir( char path[] );

/* sets path to error messages
 *
 * parameters of routine
 * char       path[];    input; directory of error messages
 */


/*-------------------------------------------------------------------*/


void err_msg( STATUS status, char msg[] );

/* returns error message of error number "status"
 *
 * parameters of routine
 * STATUS        status;     input; error number
 * char          msg[];      output; error message
 */


/*-------------------------------------------------------------------*/


void err_setcontext( char context[] );

/* sets status context
 *
 * parameters of routine
 * char       char context[];    input; status context
 */


/*-------------------------------------------------------------------*/


void err_setcontext_l( long l );

/* adds long number to error context
 *
 * parameter of routine
 * long      l;      input; number
 */


/*-------------------------------------------------------------------*/


void err_setcontext_r( float r );

/* adds long number to error context
 *
 * parameter of routine
 * long      l;      input; number
 */


/*-------------------------------------------------------------------*/


void err_getcontext( char context[] );

/* returns status context.  Maximum length of "context" is 81
 * characters (including terminator).
 *
 * parameters of routine
 * char       context[];  output; context returned
 */


/*-------------------------------------------------------------------*/


void err_clearcontext( void );

/* clears context string
 *
 * no parameters
 */


/*-------------------------------------------------------------------*/


void err_writemsg( STATUS status, char text[], BOOLEAN abort );

/* Write error message to standard output, also additional 'text'.
 * Aborts program if 'abort is TRUE.
 *
 * parameters of routine
 * STATUS     status;        input; status code
 * char       text[];        input; additional text
 * BOOLEAN    abort;         input; if TRUE abort program
 */


/*-------------------------------------------------------------------*/

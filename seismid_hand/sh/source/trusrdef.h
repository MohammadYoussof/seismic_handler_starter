
/* file TRUSRDEF.H
 *      ==========
 *
 * version 2, 22-MAY-2006
 *
 * prototypes of module SHTRANSL.C
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


#ifndef __TRUSRDEF
#define __TRUSRDEF

#ifndef __CPUSRDEF
#include "cpusrdef.h"
#endif

/*------------------------------------------------------------------------*/


void tr_partrans( PARAM *par, int *status );

/* translates all parameters in "par" */

/* parameters of routine */
/* PARAM    *par;           modify; parameters to be translated */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void tr_translate( char *str, int maxlth, int *status );

/* translates string */

/* parameters of routine */
/* char     *str;           modify; string to be translated */
/* int      maxlth;         maximum string length */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void tr_parse( char *str, char *chid, char *name, char *idxstr, int *status );

/* extracts from string char ID, variable name & index string */

/* parameters of routine */
/* char     *str;           input; string to be parsed */
/* char     *chid;          output; char ID */
/* char     *name;          output; name of variable */
/* char     *idxstr;        output; index string */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void tr_rdline( char *name, char *idxstr, int maxlth, char *str, int *status );

/* reads line number "idxstr" from file "name" */

/* parameters of routine */
/* char     *name;          input; name of file */
/* char     *idxstr;        input; line number */
/* int      maxlth;         input; maximum string length (of str) */
/* char     *str;           output; line read */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void tr_infent( char *name, char *trcno, int maxlth, char *str, int *status );

/* reads info "name" from trace "trcno" */

/* parameters of routine */
/* char     *name;          input; name of file */
/* char     *trcno;         input; trace position number */
/* int      maxlth;         input; maximum string length (of str) */
/* char     *str;           output; line read */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void tr_setruntrc( void *trc, int idx );

/* sets running trace pointer
 *
 * parameter of routine
 * void      *trc;      input; new trace pointer for X
 * int       idx;       input; position number
 */


/*------------------------------------------------------------------------*/


void tr_intern( char *name, char *idxstr, int maxlth, char *str, int *status );

/* returns internal variable 
 *
 * parameters of routine
 * char       *name;     input; name of variable
 * char       *idxstr;   input; index string
 * int        maxlth;    input; maximum length of output string
 * char       *str;      output; translated string
 * int        *status;   output; return status
 */


/*------------------------------------------------------------------------*/

#endif /* __TRUSRDEF */

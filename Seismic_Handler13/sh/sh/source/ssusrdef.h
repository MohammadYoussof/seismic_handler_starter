
/* file SSUSRDEF.H
 *      ==========
 *
 * version 2, 22-MAY-2006
 *
 * prototypes of module SHSYMBOL.C
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


#ifndef __SSUSRDEF
#define __SSUSRDEF

#ifndef __SHCONST
#include "shconst.h"
#endif
#ifndef __CPUSRDEF
#include "cpusrdef.h"
#endif

/*------------------------------------------------------------------------*/


void ss_define( unsigned set, char *name, char *value, int *status );

/* creates a new symbol "name" in set "set" and sets it to value "value" */

/* parameters of routine */
/* unsigned int  set;       input; set number */
/* char          *name;     input; name of new symbol */
/* char          *value;    input; value of new symbol */
/* int           *status;   output; return status */


/*------------------------------------------------------------------------*/


void ss_delete( unsigned set, char *name, int *status );

/* deletes an existing symbol */

/* parameters of routine */
/* unsigned int  set;       input; set number */
/* char          *name;     input; name of new symbol */
/* int           *status;   output; return status */


/*------------------------------------------------------------------------*/


void ss_delall( unsigned set, int *status );

/* deletes all symbol in set */

/* parameters of routine */
/* unsigned int  set;       input; set number */
/* int           *status;   output; return status */


/*------------------------------------------------------------------------*/


void ss_change( unsigned set, char *name, char *value, int *status );

/* changes the value of an existing symbol "name" to "value" */

/* parameters of routine */
/* unsigned int  set;       input; set number */
/* char          *name;     input; name of new symbol */
/* char          *value;    input; value of new symbol */
/* int           *status;   output; return status */


/*------------------------------------------------------------------------*/


void ss_getval( unsigned set, char *name, int maxlth, char *value, int *status );

/* returns value of existing symbol */

/* parameters of routine */
/* unsigned int  set;       input; set number */
/* char          *name;     input; name of symbol */
/* int           maxlth;    input; maximum length of output string */
/* char          *value;    output; value of symbol */
/* int           *status;   output; return status */


/*------------------------------------------------------------------------*/


int ss_exist( unsigned set, char *name );

/* checks whether "name" is defined in "set" or not */

/* parameters of routine */
/* unsigned int  set;       input; symbol set */
/* char          *name;     input; name of symbol */
                         /* returns TRUE if symbol defined */

/*------------------------------------------------------------------------*/


void ss_setpar( PARAM *par, int inputlev );

/* sets current command parameters */

/* parameters of routine */
/* PARAM    *par;           input; new command parameters */
/* int      inputlev;       input; input level */


/*------------------------------------------------------------------------*/


void ss_default( unsigned no, char *value, int *status );

/* sets default value of parameter number "no" to "value" */

/* parameters of routine */
/* unsigned int  no;        input; parameter number */
/* char          *value;    input; default value */
/* int           *status;   output; return status */


/*------------------------------------------------------------------------*/


void ss_getpar( char *nostr, int maxlth, char *value, int *status );

/* returns the value of the desired command parameter */

/* parameters of routine */
/* char     *nostr;         input; number of parameter */
/* int      maxlth;         input; maximum string length */
/* char     *value;         output; value returned */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


int ss_query( void );

/* returns query value (default parameters are queried or not) */


/*------------------------------------------------------------------------*/


int ss_inplev( void );

/* returns current input level */


/*------------------------------------------------------------------------*/


void ss_push( int ltc, int lgc, int lcc, SHFLAGS flg, int *status );

/* saves all command parameters and symbols of set 0 to a file
 *
 * parameters of routine
 * int      ltc             input; local text channels
 * int      lgc             input; local graphic channels
 * int      lcc             input; local console channels
 * SHFLAGS  flg;            input; local flags
 * int      *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void ss_pop( int *ltc, int *lgc, int *lcc, SHFLAGS *flg, int *status );

/* restores all command parameters and symbols of set 0 from a file
 *
 * parameters of routine
 * int      *ltc;           output; local text channels
 * int      *lgc;           output; local graphic channels
 * int      *lcc;           output; local console channels
 * SHFLAGS  *flg;           output; local processing flags
 * int      *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void ss_crefilnam( int cnt, char *name );

/* creates save file name */

/* parameters of routine */
/* int      cnt;            input; number of file */
/* char     *name;          output; filename created */


/*------------------------------------------------------------------------*/


void ss_dump( FILE *fp, int set );

/* dumps symbols or cmdpars to file */

/* parameters of routine */
/* FILE     *fp;        input; output file */
/* int      set;        input; set number (if -1 dump cmdpars) */


/*------------------------------------------------------------------------*/

#endif /* __SSUSRDEF */

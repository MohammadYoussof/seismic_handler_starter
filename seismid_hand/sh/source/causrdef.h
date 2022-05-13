
/* file CAUSRDEF.H
 *      ==========
 *
 * version 3, 22-May-2006
 *
 * prototypes of module CMDARGS.C
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


#ifndef __CAUSRDEF
#define __CAUSRDEF

/*------------------------------------------------------------------------*/


void ca_prepare( int argnum, char *arg[] );

/* prepares arguments for processing
 *
 * parameters of routine
 * int      argnum;         input; number of arguments
 * char     *arg[];         modify; arguments
 */


/*------------------------------------------------------------------------*/


void ca_qint( int argnum, char *arg[], char name[], int cnt,
	int *ires, int *cntread );

/* get "cnt" integers or less from parameter
 *
 * parameters of routine
 * int      argnum;         input; number of arguments
 * char     *arg[];         input; arguments passed to program
 * char     name[];         input; name of integer field
 * int      cnt;            input; number of integers to read
 * int      *ires;          output; integers read from parameter
 * int      *cntread;       output; return status
 */


/*------------------------------------------------------------------------*/


void ca_qreal( int argnum, char *arg[], char name[], int cnt,
	float *rres, int *cntread );

/* get "cnt" reals or less from parameter
 *
 * parameters of routine
 * int      argnum;         input; number of arguments
 * char     *arg[];         input; arguments passed to program
 * char     name[];         input; name of integer field
 * int      cnt;            input; number of integers to read
 * float    *rres;          output; reals read from parameter
 * int      *cntread;       output; return status
 */


/*------------------------------------------------------------------------*/


void ca_qstr( int argnum, char *arg[], char name[], char str[] );

/* get string from parameter
 *
 * parameters of routine
 * int      argnum;        input; number of arguments
 * char     *arg[];        input; arguments passed to program
 * char     name[];        input; name of integer field
 * char     str[];         output; string read from parameter
 */


/*------------------------------------------------------------------------*/

#endif /* __CAUSRDEF */

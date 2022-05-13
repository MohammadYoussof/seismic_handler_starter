
/* file CPAR.H
 *      ======
 *
 * version 4, 22-May-2006
 *
 * include file of module CPAR.C
 * K. Stammler, 9-May-92
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


/* global constants */
#define PAC_MAXPAR 20
	/* maximum number of parameters */
#define PAC_MAXQUAL 20
	/* maximum number of qualifers */


/*--------------------------------------------------------------------*/


void pa_init( int argc, char *argv[] );

/* intializes pa-variables.  Determines number of parameters and
 * number of qualifers
 *
 * parameters of routine
 * int        argc;       input; number of items (quals and pars)
 * char       *argv[];    input; argument string
 */


/*--------------------------------------------------------------------*/


int pa_pnumber( void );

/* returns number of parameters
 *
 * no parameters
 */


/*--------------------------------------------------------------------*/


int pa_qnumber( void );

/* returns number of qualifiers
 *
 * no parameters
 */


/*--------------------------------------------------------------------*/


char *pa_pvalue( int number );

/* returns value of parameter number "number"
 *
 * parameters of routine
 * int        number;    number of parameter
 */


/*--------------------------------------------------------------------*/


BOOLEAN pa_qspecified( char name[] );

/* checks whether a particular qualifier is specified in the command
 * line
 *
 * parameter of the routine
 * char      name[];      input; name of the qualifier
 *                        returns TRUE if the qualifier was specified
 */


/*--------------------------------------------------------------------*/


char *pa_qvalue( char name[] );

/* returns value of a particular qualifier
 *
 * parameter of the routine
 * char      name[];      input; name of the qualifier
 *                        returns TRUE if the qualifier was specified
 */


/*--------------------------------------------------------------------*/


int pa_qunchecked( char first[] );

/* returns the number of unchecked qualifers.  If the number is
 * greater than zero, "first" contains the first unchecked qualifier
 *
 * parameters of routine
 * char       first[];    output; first unchecked qualifier
 *                        returns number of unchecked qualifiers
 */


/*--------------------------------------------------------------------*/


char *pa_progname( void );

/* returns name of program without path
 *
 * no parameters
 */


/*--------------------------------------------------------------------*/

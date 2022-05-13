
/* file FCTXMN0.H
 *      =========
 *
 * version 2, 22-MAY-2006
 *
 * prototypes of module SHMENU0.C
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


#ifndef __FCTXMN0
#define __FCTXMN0

#ifndef __CPUSRDEF
#include "cpusrdef.h"
#endif

/* prototypes of module SHMENU0.C */

/*------------------------------------------------------------------------*/


void mn0_callproc( PARAM *par, int *status );

/* calls a command procedure. not a menu option */

/* parameters of routine */
/* PARAM    *par;           input; name & parameters */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_cmdreturn( int *status );

/* return from command procedure. not a menu option */

/* parameters of routine */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_fct( PARAM *par, int *status );

/* multi function command */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_ecch( PARAM *par, int *status );

/* selects echo channel */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_echo( PARAM *par, int *status );

/* writes to the current echo channel */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_sdef( PARAM *par, int *status );

/* defines a symbol */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_sdel( PARAM *par, int *status );

/* deletes a symbol */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_calc( PARAM *par, int *status );

/* simple calculations */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_goto( PARAM *par, int *status );

/* jumps to a label (valid only in command procedures) */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_if( PARAM *par, int *status );

/* conditional command (valid only in command procedures) */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_switch( PARAM *par, SHFLAGS *flags, int *status );

/* changes flags */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* SHFLAGS  *flags;         output; global flags */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_default( PARAM *par, int *status );

/* sets default value of command parameters */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_enter( PARAM *par, int *status );

/* reads a symbol value from parent command level */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_quit( PARAM *par, int *quit, int *status );

/* quit seismhandler command */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *quit;          output; quit flag */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_system( PARAM *par, int *status );

/* system command */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void mn0_help( PARAM *par, int *status );

/* help command */

/* parameters of routine */
/* PARAM    *par;           input; command parameter */
/* int      *status;        output; return status */


/*-------------------------------------------------------------------------*/


void mn0_wdw( PARAM *par, int *status );

/* sets window parameter
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  keyword
 */


/*-------------------------------------------------------------------------*/

#endif /* __FCTXMN0 */


/* file seusrdef.h
 *      ==========
 *
 * version 23 22-May-2006
 *
 * prototypes of module shexec.c
 * K. Stammler, 10-Feb-93
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


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include BC_SYSBASE
#include BC_GCUSRDEF
#include BC_SHCONST
#include "cpusrdef.h"


/*------------------------------------------------------------------------*/


void se_initialize( int argc, char *argv[], STATUS *status );

/* initializes SH program:
 * (i)    set session ID string
 * (ii)   UNIX path names or path list file (set shd_... variables)
 * (iii)  set input/output dirs for graphic (gc_...) routines
 * (iv)   set global vars: shflags_shv, shglbflags_shv, graphic channels
 * (v)    open protocol file
 * (vi)   initialize q-file interface
 * (vii)  set default names for earthloc input files (mb_...)
 * (viii) open startup file (set ui_level to 1)
 *
 * parameters of routine
 * int        argc;      input; command line parameters
 * char       argv[];    input; -- " --
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void se_terminate( void );

/* closes all files and renames log file
 *
 * no parameters
 */


/*------------------------------------------------------------------------*/


void se_execute_command( PARAM *cmd, char cmdlin[], char execstr[],
	BOOLEAN *quit, BOOLEAN *redraw, BOOLEAN *iscmdproc, int *rdlevel,
	char prompt[], STATUS *status );

/* executes the command "cmd"
 *
 * parameters of routine
 * PARAM      *cmd;      input; command verb & parameters
 * char       cmdlin[];  input; command line
 * char       execstr[]; output; next command line (from EXEC command)
 * BOOLEAN    *quit;     output; quit program
 * BOOLEAN    *redraw;   output; redraw screen after command
 * int        *rdlevel;  modify; redraw level
 * char       prompt[];  output; prompt string
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void se_cmdabort( PARAM *par, char cmdline[], STATUS status );

/* creates error file, aborts command procedure and returns to level 0
 *
 * parameters of routine
 * PARAM    *par;           input; parameter block
 * char     cmdline[];      input; user input
 * STATUS   status;         input; error number
 */


/*------------------------------------------------------------------------*/


void se_check_qual( CHMAP ch, PARAM *par );

/* prints unchecked qualifiers
 *
 * parameters of routine
 * CHMAP     ch;      input; output channel(s)
 * PARAM     *par;    input; parameter block
 */


/*------------------------------------------------------------------------*/


void se_do_step( char stepcmd, STATUS *status );

/* processes step command entered by user
 *
 * parameters of routine
 * char       stepcmd;     input; step command
 * STATUS     *status;     output; return status
 */


/*------------------------------------------------------------------------*/


void se_dsplymsg( CHMAP wdw, STATUS status );

/* displays error message
 *
 * parameters of routine
 * CHAMP      wdw;        input; window number
 * STATUS     status;     input; error number
 */


/*------------------------------------------------------------------------*/


void se_readshpaths( char file[], BOOLEAN *ok );

/* reads SH paths from file
 *
 * parameters of routine
 * char       file[];      input; name of input file
 * BOOLEAN    *ok;         output; file found and read
 */


/*------------------------------------------------------------------------*/


#ifdef BC_SUN

void se_get_sh_environment( void );

/* reads environment variables and copies directories to shd_... variables
 *
 * no parameters
 */

#endif


/*------------------------------------------------------------------------*/

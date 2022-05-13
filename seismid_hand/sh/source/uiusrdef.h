
/* file UIUSRDEF.H
 *      ==========
 *
 * version 5, 4-Jul-2006
 *
 * constants and prototypes of module USRINPW.C
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


#ifndef __UIUSRDEF
#define __UIUSRDEF

#include BC_GCUSRDEF

#define UIF_DOPROT 0x1
#define UIF_ECHO   0x2
#define UIF_CAPCNV 0x4
#define UIF_STEP   0x8


/*------------------------------------------------------------------------*/


void ui_initialize( char prot_file[], STATUS *status );

/* initializes ui-routines.  Call once at the beginning of your program
 *
 * parameters of routine
 * char      prot_file[];  input; name of protocol file without extension
 * STATUS    *status;      output; return status
 */


/*------------------------------------------------------------------------*/


void ui_exit( char cmdfile[] );

/* exits from UI routines.  Call once before program exit
 *
 * parameter of routine
 * char         cmdfile[];   input; name of command file to create
 */


/*------------------------------------------------------------------------*/


void ui_switch( char cmdfile[], STATUS *status );

/* switches input to file "cmdfile"
 *
 * parameters of routine
 * char      cmdfile[];     input; name of command file to process
 * STATUS    *status;       output; return status
 */


/*------------------------------------------------------------------------*/


int ui_level( void );

/* returns current command level */


/*------------------------------------------------------------------------*/


void ui_setflag( int flag, BOOLEAN val );

/* sets or clears bits set in flag in flags_uiv
 *
 * parameters of routine
 * int      flag;    flags to be set or cleared
 * BOOLEAN  val;     TRUE=set flag  FALSE=clear flag
 */


/*------------------------------------------------------------------------*/


void ui_absflag( int flags );

/* sets flags directly
 *
 * parameter of routine
 * int       flags;    input; new flag values
 */


/*------------------------------------------------------------------------*/


void ui_setconsole( CHMAP con );

/* sets console channel.  Needed at step prompts
 *
 * parameters of routine
 * CHMAP      con;    input; console channel
 */


/*------------------------------------------------------------------------*/


void ui_read( int wdw, char str[], int maxlth, STATUS *status );

/* reads "str" from current input stream
 *
 * parameter of routine
 * int     wdw;       input; window number
 * char    str[];     output; string read from input stream
 * int     maxlth;    input; maximum length of input string
 * STATUS  *status;   output; return status
 */


/*------------------------------------------------------------------------*/


char ui_readchar( int wdw, STATUS *status );

/* reads "str" from current input stream
 *
 * parameter of routine
 * int     wdw;       input; window number
 * STATUS  *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void ui_getloc( int wdw, float *x, float *y, char *key, STATUS *status );

/* gets location from current input stream
 *
 * parameters of routine
 * int     wdw;       input; window number (if interactive)
 * float   *x, *y;    output; location returned
 * char    *key;      output; ASCII code of key pressed
 * STATUS  *status;   output; return status
 */

/*------------------------------------------------------------------------*/


void ui_goto( char label[], BOOLEAN dorewind, STATUS *status );

/* jumps to a label in a command file
 *
 * parameters of routine
 * char     label[];        input; label name
 * BOOLEAN  dorewind;       input; rewind file before searching
 * STATUS   *status;        output; return status
 */


/*------------------------------------------------------------------------*/


int *ui_lines( void );

/* returns array of line counters (only for reading) */


/*------------------------------------------------------------------------*/


char *ui_levelname( int level );

/* returns name of level "level" (only for reading)
 *
 * parameters of routine
 * int        level;      input; level
 *                        returns pointer to level name
 */


/*------------------------------------------------------------------------*/



void ui_shift( int level );

/* decreases input level to "level" or if negative by "level"
 *
 * parameters of routine
 * int      level;          input; input level for following input
 */


/*------------------------------------------------------------------------*/


void ui_unshift( void );

/* resets ui_shift call */


/*------------------------------------------------------------------------*/


char ui_stepflag( void );

/* returns character entered at STEP
 *
 * no input parameters
 */


/*------------------------------------------------------------------------*/


int ui_lastsocket( void );

/* returns socket ID of last connection
 *
 * no input parameters
 */

/*------------------------------------------------------------------------*/

#endif /* __UIUSRDEF */

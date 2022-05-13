
/* file trcselect.h
 *      ===========
 *
 * version 4, 22-May-2006
 *
 * header file of module trcselect.c
 * K. Stammler, 17-Aug-93
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




/* constants */
#define TSC_MAPLTH 5
#define TSC_LONGBITNO 32
#define TSC_MAXBIT (TSC_LONGBITNO*TSC_MAPLTH)



/*----------------------------------------------------------------------------*/


void ts_clear_selections( void );

/* clears all selections
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/


void ts_toggle_selection( unsigned number );

/* toggles selection number "number"
 *
 * parameters of routine
 * unsigned   number;      input; number of selection to be toggled
 */


/*----------------------------------------------------------------------------*/


void ts_select( unsigned number );

/* select number "number"
 *
 * parameters of routine
 * unsigned   number;      input; number to be selected
 */


/*----------------------------------------------------------------------------*/


void ts_deselect( unsigned number );

/* deselect number "number"
 *
 * parameters of routine
 * unsigned   number;      input; number to be deselected
 */


/*----------------------------------------------------------------------------*/


BOOLEAN ts_is_selected( unsigned number );

/* returns whether "number" is selected
 *
 * parameters of routine
 * unsigned   number;       input; selection number to be checked
 */


/*----------------------------------------------------------------------------*/


void ts_get_selection_string( int maxlth, char str[] );

/* returns selection string.  This is list of selected numbers (from 1 upward)
 * separated by commas
 *
 * parameters of routine
 * int        maxlth;         input; maximum length of output string
 * char       str[];          output; selection list
 */


/*----------------------------------------------------------------------------*/


TSyBoolean ts_some_trace_selected( void );

/* returns whether at least one trace is selected on display
 *
 * no parameters
 */


/*----------------------------------------------------------------------------*/

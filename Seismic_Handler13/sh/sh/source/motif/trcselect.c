
/* file trcselect.c
 *      ===========
 *
 * version 6, 22-May-2006
 *
 * manages trace selections
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


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include BC_SYSBASE
#include "trcselect.h"



static long tsv_map[TSC_MAPLTH];    /* selection bits */



/*----------------------------------------------------------------------------*/



void ts_clear_selections( void )

/* clears all selections
 *
 * no parameters
 */
{
	/* local variables */
	int      i;         /* counter */

	/* executable code */

	for  (i=0; i<TSC_MAPLTH; i++)
		tsv_map[i] = 0;

} /* end of ts_clear_selections */



/*----------------------------------------------------------------------------*/



void ts_toggle_selection( unsigned number )

/* toggles selection number "number"
 *
 * parameters of routine
 * unsigned   number;      input; number of selection to be toggled
 */
{
	/* local variables */
	unsigned wordidx;     /* number of longword */
	unsigned bitidx;      /* number of bit */

	/* executable code */

	number--;

	wordidx = number / TSC_LONGBITNO;
	bitidx = number % TSC_LONGBITNO;
	if  (wordidx >= TSC_MAPLTH)  return;
	if  ((1<<bitidx) & tsv_map[wordidx])  {
		tsv_map[wordidx] &= ~(1<<bitidx);
	} else {
		tsv_map[wordidx] |= (1<<bitidx);
	} /*endif*/

} /* end of ts_toggle_selection */



/*----------------------------------------------------------------------------*/



void ts_select( unsigned number )

/* select number "number"
 *
 * parameters of routine
 * unsigned   number;      input; number to be selected
 */
{
	/* local variables */
	unsigned wordidx;     /* number of longword */
	unsigned bitidx;      /* number of bit */

	/* executable code */

	number--;

	wordidx = number / TSC_LONGBITNO;
	bitidx = number % TSC_LONGBITNO;
	if  (wordidx >= TSC_MAPLTH)  return;
	tsv_map[wordidx] |= (1<<bitidx);

} /* end of ts_select */



/*----------------------------------------------------------------------------*/



void ts_deselect( unsigned number )

/* deselect number "number"
 *
 * parameters of routine
 * unsigned   number;      input; number to be deselected
 */
{
	/* local variables */
	unsigned wordidx;     /* number of longword */
	unsigned bitidx;      /* number of bit */

	/* executable code */

	number--;

	wordidx = number / TSC_LONGBITNO;
	bitidx = number % TSC_LONGBITNO;
	if  (wordidx >= TSC_MAPLTH)  return;
	tsv_map[wordidx] &= ~(1<<bitidx);

} /* end of ts_deselect */



/*----------------------------------------------------------------------------*/



BOOLEAN ts_is_selected( unsigned number )

/* returns whether "number" is selected
 *
 * parameters of routine
 * unsigned   number;       input; selection number to be checked
 */
{
	/* local variables */
	unsigned wordidx;       /* longword index */
	unsigned bitidx;        /* bit index */

	/* executable code */

	number--;

	wordidx = number / TSC_LONGBITNO;
	bitidx = number % TSC_LONGBITNO;
	if  (wordidx >= TSC_MAPLTH)  return FALSE;
	return (tsv_map[wordidx] & (1<<bitidx));

} /* end of ts_is_selected */



/*----------------------------------------------------------------------------*/



void ts_get_selection_string( int maxlth, char str[] )

/* returns selection string.  This is list of selected numbers (from 1 upward)
 * separated by commas
 *
 * parameters of routine
 * int        maxlth;         input; maximum length of output string
 * char       str[];          output; selection list
 */
{
	/* local variables */
	BOOLEAN  first;       /* first number */
	unsigned i;           /* bit counter */
	char     *ch;         /* current string pointer */
	char     *maxch;      /* end of string pointer */

	/* executable code */

	*str = '\0';
	ch = str;
	maxch = str + maxlth;
	first = TRUE;
	for  (i=0; i<TSC_MAXBIT; i++)  {
		if  (ts_is_selected(i+1))  {
			if  (ch > maxch-4)  {
				printf( "*SHM: ts_get_selection_string: string too short ***\n" );
				return;
			} /*endif*/
			if  (!first)
				*ch++ = ',';
			first = FALSE;
			sprintf( ch, "%d", i+1 );
			ch += strlen( ch );
		} /*endif*/
	} /*endfor*/

} /* end of ts_get_selection_string */



/*----------------------------------------------------------------------------*/



TSyBoolean ts_some_trace_selected( void )

/* returns whether at least one trace is selected on display
 *
 * no parameters
 */
{
	/* local variables */
	BOOLEAN  first;       /* first number */
	unsigned i;           /* bit counter */
	char     *ch;         /* current string pointer */
	char     *maxch;      /* end of string pointer */

	/* executable code */

	first = TRUE;
	for  (i=0; i<TSC_MAXBIT; i++)  {
		if  (ts_is_selected(i+1))  {
			return TRUE;
		} /*endif*/
	} /*endfor*/
	return FALSE;

} /* end of ts_some_trace_selected */



/*----------------------------------------------------------------------------*/

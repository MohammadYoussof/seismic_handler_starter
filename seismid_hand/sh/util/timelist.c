
/* file timelist.c
 *      ==========
 *
 * version 2, 30-Jun-93
 *
 * builds timelist
 * K. Stammler, 13-Jun-93
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "timelist.h"



/* global variables */
static TLT_ELEMENT   *tlv_root=NULL;    /* root pointer */
static TLT_ELEMENT   *tlv_curr=NULL;    /* current element */
static int           tlv_total=0;       /* number of elements */




/*------------------------------------------------------------------*/



TLT_ELEMENT *tl_enlist( char timestr[], void *inf, STATUS *status )

/* enlists element at time t with info inf
 *
 * parameters of routine
 * char       timestr[];     input; time of element
 * void       *inf;          input; pointer to info
 * STATUS     *status;       output; return status
 *                           returns pointer to new element
 */
{
	/* local variables */
	TLT_TIME      at;         /* absolute time */
	TLT_ELEMENT   *new;       /* pointer to new element */
	TLT_ELEMENT   *find;      /* search pointer */
	float         tdiff;      /* time difference */

	/* executable code */

	tc_t2a( timestr, &at, status );
	if  (Severe(status))  return NULL;

	new = (TLT_ELEMENT *)sy_allocmem( 1, (int)sizeof(TLT_ELEMENT),
		status );
	if  (Severe(status))  return NULL;
	new->time = at;
	new->info = inf;

	if  (tlv_total == 0)  {
		if  (tlv_root != NULL || tlv_curr != NULL)  {
			*status = TLE_BUG;
			sy_deallocmem( new );
			return NULL;
		} /*endif*/
		tlv_root = new;
		new->next = new->prev = new;
	} else {
		find = tlv_curr;
		tdiff = tc_adiff( &at, &(find->time) );
		if  (tdiff >= 0.0)  {
			/* find next higher time */
			while  (tc_adiff(&at,&(find->next->time)) >= 0.0 &&
				find->next != tlv_root)
				find = find->next;
			/* insert between find and find->next */
			new->prev = find;
			new->next = find->next;
		} else {
			/* find last lower time */
			while  (tc_adiff(&at,&(find->prev->time)) < 0.0 &&
				find->prev != tlv_root)
				find = find->prev;
			if  (find->prev == tlv_root &&
				tc_adiff(&at,&(tlv_root->time)) < 0.0)  {
				/* go 1 further back & change root pointer */
				find = find->prev;
				tlv_root = new;
			} /*endif*/
			/* insert between find and find->prev */
			new->next = find;
			new->prev = find->prev;
		} /*endif*/
		new->next->prev = new;
		new->prev->next = new;
	} /*endif*/

	tlv_curr = new;
	tlv_total++;
	return new;

} /* end of tl_enlist */



/*------------------------------------------------------------------*/



void tl_delist( TLT_ELEMENT *e, STATUS *status )

/* removes e from list.  Allocated memory is freed, i.e. "e" can't be
 * accessed any more after this call.
 *
 * parameters of routine
 * TLT_ELEMENT   *e;     modify; element to be removed from list
 */
{
	/* local variables */
	TLT_ELEMENT   *find;      /* search pointer */

	/* executable code */

	find = tlv_root;
	while  (find != e)  {
		if  (find->next == tlv_root)  {
			*status = TLE_NOTLISTED;
			return;
		} /*endif*/
		find = find->next;
	} /*endwhile*/

	if  (tlv_total == 1)  {
		if  (e->next != tlv_root || e->prev != tlv_root)  {
			/* this cannot happen */
			*status = TLE_BUG;
			return;
		} /*endif*/
		sy_deallocmem( e );
		tlv_root = tlv_curr = NULL;
		tlv_total = 0;
	} else {
		if  (tlv_root == e)  tlv_root = e->next;
		if  (tlv_curr == e)  tlv_curr = e->next;
		e->next->prev = e->prev;
		e->prev->next = e->next;
		sy_deallocmem( e );
		tlv_total--;
	} /*endif*/

} /* end of tl_delist */



/*------------------------------------------------------------------*/



void tl_find_element( char timestr[], TLT_ELEMENT *e, float *tdiff,
	STATUS *status )

/* returns element next to given time and time difference if tdiff
 * is not NULL
 *
 * parameters of routine
 * char          timestr[]; input; time to be searched
 * TLT_ELEMENT   *e;        output; element found or NULL
 * float         *tdiff;    output; time difference to given time
 * STATUS        *status;   output; return status
 */
{
	/* local variables */
	TLT_ELEMENT   *find;               /* search pointer */
	float         diff, last_diff;     /* time differences */
	TLT_TIME      at;                  /* absolute time */

	/* executable code */

	tc_t2a( timestr, &at, status );
	if  (Severe(status))  return;

	if  (tlv_curr == NULL)  {
		*status = TLE_EMPTYLIST;
		e = NULL;
		if  (tdiff != NULL)  *tdiff = 0.0;
		return;
	} else if  (tlv_total == 1)  {
		e = tlv_root;
		if  (tdiff != NULL)  *tdiff = tc_adiff( &at, &(tlv_root->time) );
		return;
	} /*endif*/

	find = tlv_curr;
	diff = tc_adiff( &at, &(find->time) );
	if  (diff > 0.0)  {
		FOREVER  {
			if  (find->next == tlv_root)  {
				/* take highest time possible (this here) */
				e = find;
				if  (tdiff != NULL)  *tdiff = diff;
				tlv_curr = find;
				return;
			} /*endif*/
			find = find->next;
			last_diff = diff;
			diff = tc_adiff( &at, &(find->time) );
			if  (diff <= 0.0)  {
				if  (-diff < last_diff)  {
					e = find;
					if  (tdiff != NULL)  *tdiff = diff;
				} else {
					e = find->prev;
					if  (tdiff != NULL)  *tdiff = last_diff;
				} /*endif*/
				tlv_curr = e;
				return;
			} /*endif*/
		} /*endfor*/
	} else if  (diff < 0.0)  {
		FOREVER  {
			if  (find == tlv_root)  {
				/* take lowest time possible (this here) */
				e = find;
				if  (tdiff != NULL)  *tdiff = diff;
				tlv_curr = find;
				return;
			} /*endif*/
			find = find->prev;
			last_diff = diff;
			diff = tc_adiff( &at, &(find->time) );
			if  (diff >= 0.0)  {
				if  (diff < -last_diff)  {
					e = find;
					if  (tdiff != NULL)  *tdiff = diff;
				} else {
					e = find->next;
					if  (tdiff != NULL)  *tdiff = last_diff;
				} /*endif*/
				tlv_curr = e;
				return;
			} /*endif*/
		} /*endfor*/
	} else {
		e = find;
		if  (tdiff != NULL)  *tdiff = 0.0;
	} /*endif*/

} /* end of tl_find_element */



/*------------------------------------------------------------------*/



TLT_ELEMENT *tl_get_root( void )

/* returns root pointer
 *
 * no parameters
 */
{
	/* executable code */

	return tlv_root;

} /* end of tl_get_root */



/*------------------------------------------------------------------*/

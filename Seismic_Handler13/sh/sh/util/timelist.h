
/* file timelist.h
 *      ==========
 *
 * version 1, 13-Jun-93
 *
 * header file of module timelist.c
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



#include BC_TCUSRDEF


/* error codes */
#define TLE_OFFSET     5400
#define TLE_BUG        (TLE_OFFSET+1)     /* bug in program */
#define TLE_NOTLISTED  (TLE_OFFSET+2)     /* element not listed */
#define TLE_EMPTYLIST  (TLE_OFFSET+3)     /* empty list */


/* types */

#define TLT_TIME TIME

typedef struct _tlt_element {
	TLT_TIME              time;
	struct _tlt_element   *next;
	struct _tlt_element   *prev;
	void                  *info;
} TLT_ELEMENT;




/*------------------------------------------------------------------*/


TLT_ELEMENT *tl_enlist( char timestr[], void *inf, STATUS *status );

/* enlists element at time t with info inf
 *
 * parameters of routine
 * char       timestr[];     input; time of element
 * void       *inf;          input; pointer to info
 * STATUS     *status;       output; return status
 *                           returns pointer to new element
 */


/*------------------------------------------------------------------*/


void tl_delist( TLT_ELEMENT *e, STATUS *status );

/* removes e from list.  Allocated memory is freed, i.e. "e" can't be
 * accessed any more after this call.
 *
 * parameters of routine
 * TLT_ELEMENT   *e;     modify; element to be removed from list
 */


/*------------------------------------------------------------------*/


void tl_find_element( char timestr[], TLT_ELEMENT *e, float *tdiff,
	STATUS *status );

/* returns element next to given time and time difference if tdiff
 * is not NULL
 *
 * parameters of routine
 * char          timestr[]; input; time to be searched
 * TLT_ELEMENT   *e;        output; element found or NULL
 * float         *tdiff;    output; time difference to given time
 * STATUS        *status;   output; return status
 */


/*------------------------------------------------------------------*/


TLT_ELEMENT *tl_get_root( void );

/* returns root pointer
 *
 * no parameters
 */


/*------------------------------------------------------------------*/

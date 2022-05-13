
/* file event_id.h
 *      ==========
 *
 * version 4, 17-Jul-2006
 *
 * header file of module event_id.c
 * K. Stammler, 12-Sep-93
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



/* error codes */
#define EieOFFSET         4800
#define EieSTROVFL        (EieOFFSET+1)     /* string overflow */
#define EieILL_LINE       (EieOFFSET+2)     /* illegal line in data file */
#define EieTABLE_OVFL     (EieOFFSET+3)     /* too many entries */
#define EieOPEN_OUTPUT    (EieOFFSET+4)     /* error opening output file */
#define EieEVID_OVFL      (EieOFFSET+5)     /* daily count overflow */



/* constants */
#define EiEV_FILE_PREFIX "evid"
	/* name prefix to event-id data files */
#define EiPAR_FILE_PREFIX "par"
	/* name prefix to parameter-id data files */
#define EiFILE_EXT ".txt"
	/* data file extension */
#define EiEV_FILE_LTH 12
	/* length of filenames (prefix + 4[year] + extension) */
#define EiPAR_FILE_LTH 11
	/* length of filenames (prefix + 4[year] + extension) */
#define EiCOMMENT_CHAR '!'
	/* comment character at beginning of line */



/*------------------------------------------------------------------*/


void EiSetPath( char path[], STATUS *status );

/* Sets path for event ID files
 *
 * parameters of routine
 * char       path[];       input; path name
 * STATUS     *status;      output; return status
 */


/*------------------------------------------------------------------*/


long EiGetEventID( char onset[], STATUS *status );

/* Returns unique event ID for event
 *
 * parameters of routine
 * char       onset[];       input; onset time of event
 * STATUS     *status;       output; return status
 *                           returns event ID
 */


/*------------------------------------------------------------------*/


long EiGetParameterID( char onset[], STATUS *status );

/* Returns unique parameter ID.
 *
 * parameters of routine
 * char       onset[];       input; onset time of event
 * STATUS     *status;       output; return status
 *                           returns event ID
 */


/*------------------------------------------------------------------*/

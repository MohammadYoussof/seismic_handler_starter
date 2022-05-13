/* file evtelex.h
 *      =========
 *
 * version 4, 17-Feb-2000
 *
 * header file of module evtelex.c
 * K. Stammler, 28-Jul-93
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



/* formats */

#define EfFORMAT_PERIOD "%3.1f"
#define EfFORMAT_AMPLITUDE_SP "%1.0f"
#define EfFORMAT_AMPLITUDE_LP "%3.1f"
#define EfFORMAT_SLOWNESS "%3.1f"
#define EfFORMAT_AZIMUTH "%1.0f"
#define EfFORMAT_DISTANCE "%1.0f"
#define EfFORMAT_MAGNITUDE "%3.1f"



/*-------------------------------------------------------------------*/


void EfPutTelex( FILE *telex, EvEventT *event, EvStatusT *status );

/* Prints event "event" in telex format to file "telex".  This
 * routine may be used as a "put_routine" in "EvReformatEvenfile".
 *
 * parameters of routine
 * FILE       *telex;        input; output file
 * EvEventT   *event;        input; event information
 * EvStatusT  *status;       output; return status
 */


/*-------------------------------------------------------------------*/


void EfSetTelexHeader( char header[] );

/* Sets header text of seismo message
 *
 * parameters of routine
 * char       header[];      input; header text
 */


/*-------------------------------------------------------------------*/


void EfSetTelexStation( char station[] );

/* Sets station name in seismo message
 *
 * parameters of routine
 * char       station[];      input; header text
 */


/*-------------------------------------------------------------------*/


void EfTelexPhasename( char name[] );

/* replaces lowercase "s" and "p" (in depth phases) to "X" and "A",
 * respectively
 *
 * parameters of routine
 * char       name[];      modify; name to be translated
 */



/*-------------------------------------------------------------------*/


void EfSetIgnoreTelexFlag( BOOLEAN flag );

/* Sets value of ignore telex flag
 *
 * parameters of routine
 * BOOLEAN    flag;        input; new value of flag
 */


/*-------------------------------------------------------------------*/

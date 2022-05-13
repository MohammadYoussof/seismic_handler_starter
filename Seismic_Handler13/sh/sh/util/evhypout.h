
/* file evhypout.h
 *      ==========
 *
 * version 2, 2-Nov-93
 *
 * header file of module evhypout.c
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



/*------------------------------------------------------------------*/


void EfGetHypoOut( FILE *file, EvEventT *event, BOOLEAN *eof,
	EvStatusT *status );

/* Reads event structure from output file of HYPOELLIPSE.  Currently
 * it is assumed that there is only one event written in the
 * HYPOELLIPSE file.  This means that the second call to this routine
 * on the same file always returns *eof == TRUE.
 *
 * parameters of routine
 * FILE       *file;       input; pointer to input file
 * EvEventT   *event;      output; event read from file
 * BOOLEAN    *eof;        output; end of file found
 * EvStatusT  *status;     output; return status
 */


/*------------------------------------------------------------------*/


void EfAppendStationInfo( char fname[] );

/* Appends station info of most recent Hypoellipse file read to given
 * file.
 *
 * parameters of routine
 * char       fname[];      input; filename to append info
 */


/*------------------------------------------------------------------*/

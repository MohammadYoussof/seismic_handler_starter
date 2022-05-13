/* file lgusrdef.h
 *      ==========
 *
 * version 2, 22-May-2006
 *
 * prototypes of module logplot.c
 * K. Stammler, 5-Sep-92
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
#define LGE_OFFSET       5200
#define LGE_ILLSCALE     (LGE_OFFSET+1)   /* illegal scaling */


/* constants */
#define LGC_NOCHANGE 1000.0
	/* no change in lg_labshift parameter */



/*---------------------------------------------------------------------*/


void lg_setwindow( CHMAP wdw );

/* sets output window for logplot
 *
 * parameter of routine
 * CHMAP     wdw;      input; output window
 */


/*---------------------------------------------------------------------*/


void lg_labshift( REAL ha_hs, REAL ha_vs, REAL va_hs, REAL va_vs );

/* sets label positions
 *
 * parameters of routine
 * REAL       ha_hs;   input; horizontal pos. at horizontal axis
 * REAL       ha_vs;   input; vertical pos. at horizontal axis
 * REAL       va_hs;   input; horizontal pos. at vertical axis
 * REAL       va_vs;   input; vertical pos. at vertical axis
 */


/*---------------------------------------------------------------------*/


void lg_scale( int style, REAL lofrq, REAL hifrq, REAL loamp,
	REAL hiamp, STATUS *status );

/* set scale of plots
 *
 * parameters of routine
 * int        style;             input; drawing style
 * REAL       lofrq, hifrq;      input; frequency bounds
 * REAL       loamp, hiamp;      input; amplitude bounds
 * STATUS     *status;           output; return status
 */


/*---------------------------------------------------------------------*/


void lg_logplot( int style, SAMPLE smp[], long lth, REAL offset,
	REAL delta );

/* plot trace into logplot window
 *
 * parameters of routine
 * int        style;        input; style number
 * SAMPLE     smp[];        input; trace to plot
 * long       lth;          input; length of trace
 * REAL       offset;       input; frq offset of trace
 * REAL       delta;        input; x-increment
 */


/*---------------------------------------------------------------------*/

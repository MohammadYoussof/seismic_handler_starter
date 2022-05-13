/* file logplot.c
 *      =========
 *
 * version 4, 22-May-2006
 *
 * Logarithmic plot routine for SH
 * K. Stammler, 5-Sep-92
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1992,  Klaus Stammler, Federal Institute for Geosciences
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
#include <math.h>
#include "basecnst.h"
#include BC_SYSBASE
#include BC_GCUSRDEF
#include BC_SHCONST
#include "lgusrdef.h"


/* constants */
#define MARGIN 0.2



/* global variables */
static CHMAP     lgv_wdw;       /* output window */
static REAL      lgv_lg_lofrq;  /* logarithm of lower frq bound */
static REAL      lgv_lg_hifrq;  /* logarithm of upper frq bound */
static REAL      lgv_lg_loamp;  /* logarithm of lower amplitude bound */
static REAL      lgv_lg_hiamp;  /* logarithm of upper amplitude bound */
static REAL      lgv_ha_hshift; /* horiz. pos. of horiz. labelling */
static REAL      lgv_ha_vshift; /* vert. pos. of horiz. labelling */
static REAL      lgv_va_hshift; /* horiz. pos. of vert. labelling */
static REAL      lgv_va_vshift; /* vert. pos. of vert. labelling */



/*---------------------------------------------------------------------*/



void lg_setwindow( CHMAP wdw )

/* sets output window for logplot
 *
 * parameter of routine
 * CHMAP     wdw;      input; output window
 */
{
	/* executable code */

	lgv_wdw = wdw;

} /* end of lg_setwindow */



/*---------------------------------------------------------------------*/



void lg_labshift( REAL ha_hs, REAL ha_vs, REAL va_hs, REAL va_vs )

/* sets label positions
 *
 * parameters of routine
 * REAL       ha_hs;   input; horizontal pos. at horizontal axis
 * REAL       ha_vs;   input; vertical pos. at horizontal axis
 * REAL       va_hs;   input; horizontal pos. at vertical axis
 * REAL       va_vs;   input; vertical pos. at vertical axis
 */
{
	/* executable code */

	if  (ha_hs != LGC_NOCHANGE)  lgv_ha_hshift = ha_hs;
	if  (ha_vs != LGC_NOCHANGE)  lgv_ha_vshift = ha_vs;
	if  (va_hs != LGC_NOCHANGE)  lgv_va_hshift = va_hs;
	if  (va_vs != LGC_NOCHANGE)  lgv_va_vshift = va_vs;

} /* end of lg_labshift */



/*---------------------------------------------------------------------*/



void lg_scale( int style, REAL lofrq, REAL hifrq, REAL loamp,
	REAL hiamp, STATUS *status )

/* set scale of plots
 *
 * parameters of routine
 * int        style;             input; drawing style
 * REAL       lofrq, hifrq;      input; frequency bounds
 * REAL       loamp, hiamp;      input; amplitude bounds
 * STATUS     *status;           output; return status
 */
{
	/* local variables */
	REAL     w_x, w_y;          /* window lower left coordinates */
	REAL     w_width;           /* window width */
	REAL     w_height;          /* window height */
	REAL     tmp;               /* scratch */
	REAL     tickl, ltickl;     /* tick lengths */
	REAL     curr;              /* current position */
	int      i;                 /* counter */
	char     str[BC_LINELTH+1]; /* scratch string */

	/* executable code */

	if  (lofrq >= hifrq || loamp >= hiamp)  {
		*status = LGE_ILLSCALE;
		return;
	} else if  (lofrq <= 0.0 || loamp <= 0.0)  {
		*status = LGE_ILLSCALE;
		return;
	} /*endif*/

	lgv_lg_lofrq = floor( log10(lofrq)+0.01 );
	lgv_lg_hifrq = ceil( log10(hifrq)-0.01 );
	lgv_lg_loamp = floor( log10(loamp)+0.01 );
	lgv_lg_hiamp = ceil( log10(hiamp)-0.01 );

	w_width = lgv_lg_hifrq - lgv_lg_lofrq;
	w_height = lgv_lg_hiamp - lgv_lg_loamp;
	tmp = w_width * MARGIN;
	w_x = lgv_lg_lofrq - tmp;
	w_width += 2.0*tmp;
	tmp = w_height * MARGIN;
	w_y = lgv_lg_loamp - tmp;
	w_height += 2.0*tmp;

	gc_erase( lgv_wdw );
	gc_setcoo( lgv_wdw, w_x, w_y, w_width, w_height, status );
	if  (Severe(status))  return;

	/* draw axes */
	gc_moveto( lgv_wdw, lgv_lg_lofrq, lgv_lg_hiamp );
	gc_drawto( lgv_wdw, style, lgv_lg_lofrq, lgv_lg_loamp );
	gc_drawto( lgv_wdw, style, lgv_lg_hifrq, lgv_lg_loamp );

	/* draw ticks of horizontal axis */
	tickl = w_height / 40.0;
	ltickl = 2.0*tickl;
	curr = lgv_lg_lofrq;
	do  {
		for  (i=0; i<10; i++)  {
			tmp = curr + log10( 1.0+(float)i );
			gc_moveto( lgv_wdw, tmp, lgv_lg_loamp );
			if  (i == 0)  {
				gc_drawto( lgv_wdw, style, tmp, lgv_lg_loamp-ltickl );
				sprintf( str, "%1.0f", curr );
				gc_text( lgv_wdw, style, tmp+lgv_ha_hshift*w_width,
					lgv_lg_loamp-ltickl+lgv_ha_vshift*w_height, str );
			} else {
				gc_drawto( lgv_wdw, style, tmp, lgv_lg_loamp-tickl );
			} /*endif*/
		} /*endfor*/
		curr += 1.0;
	} while (curr < lgv_lg_hifrq);

	/* draw ticks of vertical axis */
	tickl = w_width / 40.0;
	ltickl = 2.0*tickl;
	curr = lgv_lg_loamp;
	do  {
		for  (i=0; i<10; i++)  {
			tmp = curr + log10( 1.0+(float)i );
			gc_moveto( lgv_wdw, lgv_lg_lofrq, tmp );
			if  (i == 0)  {
				gc_drawto( lgv_wdw, style, lgv_lg_lofrq-ltickl, tmp );
				sprintf( str, "%1.0f", curr );
				gc_text( lgv_wdw, style, lgv_lg_lofrq-ltickl+
					lgv_va_hshift*w_width, tmp+lgv_va_vshift*w_height, str );
			} else {
				gc_drawto( lgv_wdw, style, lgv_lg_lofrq-tickl, tmp );
			} /*endif*/
		} /*endfor*/
		curr += 1.0;
	} while (curr < lgv_lg_hiamp);

	gc_flushbuffers();

} /* end of lg_scale */



/*---------------------------------------------------------------------*/



void lg_logplot( int style, SAMPLE smp[], long lth, REAL offset,
	REAL delta )

/* plot trace into logplot window
 *
 * parameters of routine
 * int        style;        input; style number
 * SAMPLE     smp[];        input; trace to plot
 * long       lth;          input; length of trace
 * REAL       offset;       input; frq offset of trace
 * REAL       delta;        input; x-increment
 */
{
	/* local variables */
	REAL     x;           /* current x position */
	REAL     logx, logy;  /* logarithm of position */
	long     i, istart;   /* counter */
	BOOLEAN  move;        /* move, not draw */

	/* executable code */

	/* find onset */
	istart = 0;
	x = offset;
	while  (x <= 0.0)  {
		x += delta;
		istart++;
	} /*endwhile */

	/* plot it */
	move = TRUE;
	for  (i=istart; i<lth; i++)  {
		logx = log10( x );
		logy = log10( smp[i] );
		if  (lgv_lg_lofrq <= logx && logx <= lgv_lg_hifrq &&
			lgv_lg_loamp <= logy && logy <= lgv_lg_hiamp)  {
			if  (move)  {
				gc_moveto( lgv_wdw, logx, logy );
				move = FALSE;
			} else {
				gc_drawto( lgv_wdw, style, logx, logy );
			} /*endif*/
		} else {
			move = TRUE;
		} /*endif*/
		x += delta;
	} /*endfor*/

	gc_flushbuffers();

} /* end of lg_logplot */



/*---------------------------------------------------------------------*/

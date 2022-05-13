
/* file SHPM.C
 *      ======
 *
 * version 4, 22-May-2006
 *
 * particle motion diagrams
 * K. Stammler, 3-NOV-1990
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
#include <math.h>
#include "basecnst.h"
#include BC_SYSBASE
#include "shconst.h"
#include BC_GCUSRDEF
#include "fctxsl.h"
#include "fctxcr.h"
#include "fctxpm.h"
#include "sherrors.h"



/* global variables */
static CHMAP     pmv_ch=0;          /* output window */
static REAL      pmv_pmsep=0.25;    /* separation space */
static REAL      pmv_boxsep=0.03;   /* separation from box */


/* prototypes of local routines */
static void pmh_drawcurve( CHMAP ch, REAL x[], REAL y[],
	REAL xorig, REAL yorig, REAL norm, long lth );
static void pmh_drawcircle( CHMAP ch, REAL ox, REAL oy, REAL r );



/*---------------------------------------------------------------------*/



void pm_setoutput( CHMAP wdw )

/* sets output window
 *
 * parameter of routine
 * CHMAP     wdw;       input; window channel(s)
 */
{
	/* executable code */

	pmv_ch = wdw;

} /* end of pm_setoutput */



/*---------------------------------------------------------------------*/



void pm_fixedwdw( CHMAP ch, SAMPLE *x[], SAMPLE *y[], int trcno,
	long lth, char *cmt[], float zoom[], REAL begcirc, STATUS *status )

/* draws fixed window particle motion diagram
 *
 * parameters of routine
 * CHMAP      ch;         input; current graphic channel(s)
 * SAMPLE     *x[];       input; x coordinates
 * SAMPLE     *y[];       input; y coordinates
 * int        trcno;      input; number of diagrams
 * long       lth;        input; length of arrays in samples
 * char       *cmt[];     input; comment lines
 * float      zoom[];     input; zoom factors of traces
 * REAL       begcirc;    input; radius of start circle (drawn if > 0)
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	SAMPLE   trcmax;       /* maximum value in one traces */
	SAMPLE   trcmin;       /* minimum value in one traces */
	SAMPLE   totmax;       /* absolute maximum value in all traces */
	int      t;            /* trace counter */
	SAMPLE   norm;         /* normalisation */
	int      line_pm;      /* pm diagrams per line */
	REAL     displaysize;  /* size of display */
	REAL     xorig[PMC_MAXPM];  /* x origins */
	REAL     yorig[PMC_MAXPM];  /* y origins */
	int      l, c;         /* line & column counters */
	REAL     sep;          /* separation */
	REAL     xtext, ytext; /* text position */
	float    r;            /* scratch */

	/* executable code */

	/* find maximum value & normalisation */
	totmax = 0.0;
	for  (t=0; t<trcno; t++)  {
		sl_findmax( x[t], lth, &trcmin, &trcmax );
		trcmin = Abs(trcmin);
		trcmax = Abs(trcmax);
		if  (trcmin > trcmax)
			trcmax = trcmin;
		if  (trcmax > totmax)
			totmax = trcmax;
		sl_findmax( y[t], lth, &trcmin, &trcmax );
		trcmin = Abs(trcmin);
		trcmax = Abs(trcmax);
		if  (trcmin > trcmax)
			trcmax = trcmin;
		if  (trcmax > totmax)
			totmax = trcmax;
	} /*endfor*/
	if  (totmax <= 0.0)  return;
	norm = 0.5 / totmax;

	/* compute number of pm's per line & size of display*/
	line_pm = 1;
	while  ((line_pm*line_pm) < trcno)
		line_pm++;
	displaysize = line_pm + (line_pm+1)*pmv_pmsep;

	/* compute origins */
	for  (t=0; t<trcno; t++)  {
		l = t / line_pm + 1;
		c = t % line_pm + 1;
		xorig[t] = (float)c * pmv_pmsep + ((float)c - 0.5);
		yorig[t] = displaysize - (float)l * pmv_pmsep - ((float)l - 0.5);
	} /*endfor*/

	/* draw it */
	if  (pmv_ch != 0)  ch = pmv_ch;
	gc_erase( ch );
	r = gc_aspectratio( ch );
	if  (r >= 1.0)  {
		gc_setcoo( ch, 0.0, 0.0, r*displaysize, displaysize, status );
	} else {
		gc_setcoo( ch, 0.0, 0.0, displaysize, displaysize/r, status );
	} /*endif*/
	if  (Severe(status))  return;
	for  (t=0; t<trcno; t++)  {
		if  (begcirc > 0.)
			pmh_drawcircle( ch, xorig[t]+x[t][0]*norm*zoom[t],
			yorig[t]+y[t][0]*norm*zoom[t], begcirc );
		pmh_drawcurve( ch, x[t], y[t], xorig[t], yorig[t], norm*zoom[t], lth );
		sep = 0.5 + pmv_boxsep;
		gc_moveto( ch, xorig[t]-sep, yorig[t]-sep );
		gc_drawto( ch, 0, xorig[t]+sep, yorig[t]-sep );
		gc_drawto( ch, 0, xorig[t]+sep, yorig[t]+sep );
		gc_drawto( ch, 0, xorig[t]-sep, yorig[t]+sep );
		gc_drawto( ch, 0, xorig[t]-sep, yorig[t]-sep );
	} /*endfor*/

	/* write comments */
	if  (cmt != NULL)  {
		xtext = -0.5 - pmv_boxsep;
		ytext = -0.5 - pmv_pmsep/2.0;
		for  (t=0; t<trcno; t++)
			gc_text( ch, SHC_PMSTYLE, xorig[t]+xtext, yorig[t]+ytext, cmt[t] );
	} /*endif*/

	gc_flushbuffers();

} /* end of pm_fixedwdw */



/*---------------------------------------------------------------------*/



void pm_varwdw( CHMAP ch, SAMPLE *x[], SAMPLE *y[], int trcno,
	REAL mintime, REAL maxtime, REAL origtime, REAL dt, char *cmt[],
	float zoom[], REAL loy, REAL hiy, REAL begcirc, STATUS *status )

/* draws variable window particle motion diagrams
 *
 * parameters of routine
 * CHMAP      ch;         input; current graphic channel(s)
 * SAMPLE     *x[];       input; x coordinates
 * SAMPLE     *y[];       input; y coordinates
 * int        trcno;      input; number of diagrams
 * REAL       mintime;    input; minimum time (start time of traces)
 * REAL       maxtime;    input; maximum time (start time of traces)
 * REAL       origtime;   input; start of pm window
 * REAL       dt;         input; sample distance
 * char       *cmt[];     input; comment lines
 * float      zoom[];     input; zoom factors of traces
 * REAL       loy, hiy;   input; y range in trace window "ch"
 * REAL       begcirc;    input; radius of start circle (drawn if > 0)
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	long     trclth;       /* length of traces */
	SAMPLE   trcmax;       /* maximum value in one traces */
	SAMPLE   trcmin;       /* minimum value in one traces */
	SAMPLE   totmax;       /* absolute maximum value in all traces */
	int      t;            /* trace counter */
	SAMPLE   norm;         /* normalisation */
	int      line_pm;      /* pm diagrams per line */
	REAL     displaysize;  /* size of display */
	REAL     xorig[PMC_MAXPM];  /* x origins */
	REAL     yorig[PMC_MAXPM];  /* y origins */
	int      l, c;         /* line & column counters */
	REAL     sep;          /* separation */
	REAL     xtext, ytext; /* text position */
	REAL     r;            /* scratch */
	REAL     currtim;      /* current time */
	REAL     oldtim;       /* last time */
	long     si, se;       /* start & end sample */
	long     s;            /* sample counter */
	int      chnohc;       /* ch without hardcopy channel */

	/* executable code */

	if  (ch == pmv_ch || pmv_ch == 0)  {
		*status = SHE_PMWDW;
		return;
	} /*endif*/
	chnohc = ch & ~GCF_HARDCOPY;

	/* find maximum value & normalisation */
	trclth = Nlong((maxtime-mintime)/dt);
	totmax = 0.0;
	for  (t=0; t<trcno; t++)  {
		sl_findmax( x[t], trclth, &trcmin, &trcmax );
		trcmin = Abs(trcmin);
		trcmax = Abs(trcmax);
		if  (trcmin > trcmax)
			trcmax = trcmin;
		if  (trcmax > totmax)
			totmax = trcmax;
		sl_findmax( y[t], trclth, &trcmin, &trcmax );
		trcmin = Abs(trcmin);
		trcmax = Abs(trcmax);
		if  (trcmin > trcmax)
			trcmax = trcmin;
		if  (trcmax > totmax)
			totmax = trcmax;
	} /*endfor*/
	if  (totmax <= 0.0)  return;
	norm = 0.5 / totmax;

	/* compute number of pm's per line & size of display*/
	line_pm = 1;
	while  ((line_pm*line_pm) < trcno)
		line_pm++;
	displaysize = line_pm + (line_pm+1)*pmv_pmsep;

	/* compute origins */
	for  (t=0; t<trcno; t++)  {
		l = t / line_pm + 1;
		c = t % line_pm + 1;
		xorig[t] = (float)c * pmv_pmsep + ((float)c - 0.5);
		yorig[t] = displaysize - (float)l * pmv_pmsep - ((float)l - 0.5);
	} /*endfor*/

	/* draw it */
	gc_erase( pmv_ch );
	r = gc_aspectratio( pmv_ch );
	if  (r >= 1.0)  {
		gc_setcoo( pmv_ch, 0.0, 0.0, r*displaysize, displaysize, status );
	} else {
		gc_setcoo( pmv_ch, 0.0, 0.0, displaysize, displaysize/r, status );
	} /*endif*/
	if  (Severe(status))  return;
	sep = 0.5 + pmv_boxsep;
	xtext = -0.5 - pmv_boxsep;
	ytext = -0.5 - pmv_pmsep/2.0;

	/* loop over selections */
	oldtim = origtime;
	FOREVER  {

		/* get new selected position, return if aborted */
		cr_getloc( MM_NOMARK, &currtim, NULL, NULL, status );
		if  (Severe(status))  {
			if  (*status == SHE_EXIT)  *status = SHE_NOERROR;
			return;
		} /*endif*/
		if  (currtim < mintime)  currtim = mintime;
		if  (currtim > maxtime)  currtim = maxtime;

		/* if location changed compared to last selection */
		if  (currtim != oldtim)  {

			/* unmark old time, mark new selected time */
			gc_setstyle( chnohc, SHC_MARKSTYLE, "WRMODE", "XOR", status );
			if  (Severe(status))  return;
			if  (oldtim != origtime)  {
				gc_moveto( chnohc, oldtim, loy );
				gc_drawto( chnohc, SHC_MARKSTYLE, oldtim, hiy );
			} /*endif*/
			gc_moveto( chnohc, currtim, loy );
			gc_drawto( chnohc, SHC_MARKSTYLE, currtim, hiy );
			gc_setstyle( chnohc, SHC_MARKSTYLE, "WRMODE", "REPLACE", status );
			if  (Severe(status))  return;

			/* draw pm diagrams */
			gc_erase( pmv_ch );
			si = Nlong((origtime-mintime)/dt);
			se = Nlong((currtim-mintime)/dt);
			for  (t=0; t<trcno; t++)  {
				/* frame box */
				gc_moveto( pmv_ch, xorig[t]-sep, yorig[t]-sep );
				gc_drawto( pmv_ch, 0, xorig[t]+sep, yorig[t]-sep );
				gc_drawto( pmv_ch, 0, xorig[t]+sep, yorig[t]+sep );
				gc_drawto( pmv_ch, 0, xorig[t]-sep, yorig[t]+sep );
				gc_drawto( pmv_ch, 0, xorig[t]-sep, yorig[t]-sep );
				/* info text */
				if  (cmt != NULL)
					gc_text( pmv_ch, SHC_PMSTYLE, xorig[t]+xtext,
						yorig[t]+ytext, cmt[t] );
				/* pm's */
				if  (begcirc > 0.)
					pmh_drawcircle( pmv_ch, xorig[t]+x[t][si]*norm*zoom[t],
						yorig[t]+y[t][si]*norm*zoom[t], begcirc );
				gc_moveto( pmv_ch, xorig[t]+x[t][si]*norm*zoom[t],
					yorig[t]+y[t][si]*norm*zoom[t] );
				if  (se > si)  {
					for  (s=si; s<=se; s++)
						gc_drawto( pmv_ch, SHC_PMSTYLE,
							xorig[t]+x[t][s]*norm*zoom[t],
							yorig[t]+y[t][s]*norm*zoom[t] );
				} else {
					for  (s=si; s>=se; s--)
						gc_drawto( pmv_ch, SHC_PMSTYLE,
							xorig[t]+x[t][s]*norm*zoom[t],
							yorig[t]+y[t][s]*norm*zoom[t] );
				} /*endif*/
			} /*endfor*/

			gc_flushbuffers();
			/* store selected time */
			oldtim = currtim;

		} /*endif*/  /* location changed */

	} /*endfor*/  /* FOREVER loop end */

} /* end of pm_varwdw */



/*---------------------------------------------------------------------*/



void pm_movewdw( CHMAP ch, SAMPLE *x[], SAMPLE *y[], int trcno,
	REAL mintime, REAL maxtime, REAL width, REAL dt, char *cmt[],
	float zoom[], REAL loy, REAL hiy, REAL begcirc, STATUS *status )

/* draws moving window particle motion diagrams
 *
 * parameters of routine
 * CHMAP      ch;         input; current graphic channel(s)
 * SAMPLE     *x[];       input; x coordinates
 * SAMPLE     *y[];       input; y coordinates
 * int        trcno;      input; number of diagrams
 * REAL       mintime;    input; minimum time (start time of traces)
 * REAL       maxtime;    input; maximum time (start time of traces)
 * REAL       width;      input; width of pm window
 * REAL       dt;         input; sample distance
 * char       *cmt[];     input; comment lines
 * float      zoom[];     input; zoom factors of traces
 * REAL       loy, hiy;   input; y range in trace window "ch"
 * REAL       begcirc;    input; radius of start circle (drawn if > 0)
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	long     trclth;       /* length of traces */
	SAMPLE   trcmax;       /* maximum value in one traces */
	SAMPLE   trcmin;       /* minimum value in one traces */
	SAMPLE   totmax;       /* absolute maximum value in all traces */
	int      t;            /* trace counter */
	SAMPLE   norm;         /* normalisation */
	int      line_pm;      /* pm diagrams per line */
	REAL     displaysize;  /* size of display */
	REAL     xorig[PMC_MAXPM];  /* x origins */
	REAL     yorig[PMC_MAXPM];  /* y origins */
	int      l, c;         /* line & column counters */
	REAL     sep;          /* separation */
	REAL     xtext, ytext; /* text position */
	REAL     r;            /* scratch */
	REAL     currtim;      /* current time */
	REAL     oldtim;       /* last time */
	long     si, se;       /* start & end sample */
	long     s;            /* sample counter */
	int      chnohc;       /* ch without hardcopy channel */

	/* executable code */

	if  (ch == pmv_ch || pmv_ch == 0)  {
		*status = SHE_PMWDW;
		return;
	} else if  (maxtime-mintime < width)  {
		*status = SHE_SPECERROR+10;
		return;
	} /*endif*/
	chnohc = ch & ~GCF_HARDCOPY;

	/* find maximum value & normalisation */
	trclth = Nlong((maxtime-mintime)/dt);
	totmax = 0.0;
	for  (t=0; t<trcno; t++)  {
		sl_findmax( x[t], trclth, &trcmin, &trcmax );
		trcmin = Abs(trcmin);
		trcmax = Abs(trcmax);
		if  (trcmin > trcmax)
			trcmax = trcmin;
		if  (trcmax > totmax)
			totmax = trcmax;
		sl_findmax( y[t], trclth, &trcmin, &trcmax );
		trcmin = Abs(trcmin);
		trcmax = Abs(trcmax);
		if  (trcmin > trcmax)
			trcmax = trcmin;
		if  (trcmax > totmax)
			totmax = trcmax;
	} /*endfor*/
	if  (totmax <= 0.0)  return;
	norm = 0.5 / totmax;

	/* compute number of pm's per line & size of display*/
	line_pm = 1;
	while  ((line_pm*line_pm) < trcno)
		line_pm++;
	displaysize = line_pm + (line_pm+1)*pmv_pmsep;

	/* compute origins */
	for  (t=0; t<trcno; t++)  {
		l = t / line_pm + 1;
		c = t % line_pm + 1;
		xorig[t] = (float)c * pmv_pmsep + ((float)c - 0.5);
		yorig[t] = displaysize - (float)l * pmv_pmsep - ((float)l - 0.5);
	} /*endfor*/

	/* draw it */
	gc_erase( pmv_ch );
	r = gc_aspectratio( pmv_ch );
	if  (r >= 1.0)  {
		gc_setcoo( pmv_ch, 0.0, 0.0, r*displaysize, displaysize, status );
	} else {
		gc_setcoo( pmv_ch, 0.0, 0.0, displaysize, displaysize/r, status );
	} /*endif*/
	if  (Severe(status))  return;
	xtext = -0.5 - pmv_boxsep;
	ytext = -0.5 - pmv_pmsep/2.0;
	sep = 0.5 + pmv_boxsep;

	/* loop over selections */
	oldtim = mintime;
	FOREVER  {

		/* get new selected position, return if aborted */
		cr_getloc( MM_NOMARK, &currtim, NULL, NULL, status );
		if  (Severe(status))  {
			if  (*status == SHE_EXIT)  *status = SHE_NOERROR;
			return;
		} /*endif*/
		if  (currtim-width/2.0 < mintime)  currtim = mintime+width/2.0;
		if  (currtim+width/2.0 > maxtime)  currtim = maxtime-width/2.0;

		/* if location changed compared to last selection */
		if  (currtim != oldtim)  {

			/* unmark old time, mark new selected time */
			gc_setstyle( chnohc, SHC_MARKSTYLE, "WRMODE", "XOR", status );
			if  (Severe(status))  return;
			if  (oldtim != mintime)  {
				gc_moveto( chnohc, oldtim-width/2.0, loy );
				gc_drawto( chnohc, SHC_MARKSTYLE, oldtim-width/2.0, hiy );
				gc_moveto( chnohc, oldtim+width/2.0, loy );
				gc_drawto( chnohc, SHC_MARKSTYLE, oldtim+width/2.0, hiy );
			} /*endif*/
			gc_moveto( chnohc, currtim-width/2.0, loy );
			gc_drawto( chnohc, SHC_MARKSTYLE, currtim-width/2.0, hiy );
			gc_moveto( chnohc, currtim+width/2.0, loy );
			gc_drawto( chnohc, SHC_MARKSTYLE, currtim+width/2.0, hiy );
			gc_setstyle( chnohc, SHC_MARKSTYLE, "WRMODE", "REPLACE", status );
			if  (Severe(status))  return;

			/* draw pm diagrams */
			gc_erase( pmv_ch );
			si = Nlong((currtim-mintime-width/2.0)/dt);
			se = Nlong((currtim-mintime+width/2.0)/dt);
			for  (t=0; t<trcno; t++)  {
				/* frame box */
				gc_moveto( pmv_ch, xorig[t]-sep, yorig[t]-sep );
				gc_drawto( pmv_ch, 0, xorig[t]+sep, yorig[t]-sep );
				gc_drawto( pmv_ch, 0, xorig[t]+sep, yorig[t]+sep );
				gc_drawto( pmv_ch, 0, xorig[t]-sep, yorig[t]+sep );
				gc_drawto( pmv_ch, 0, xorig[t]-sep, yorig[t]-sep );
				/* info text */
				if  (cmt != NULL)
					gc_text( pmv_ch, SHC_PMSTYLE, xorig[t]+xtext,
						yorig[t]+ytext, cmt[t] );
				/* pm's */
				if  (begcirc > 0.)
					pmh_drawcircle( pmv_ch, xorig[t]+x[t][si]*norm*zoom[t],
						yorig[t]+y[t][si]*norm*zoom[t], begcirc );
				gc_moveto( pmv_ch, xorig[t]+x[t][si]*norm*zoom[t],
					yorig[t]+y[t][si]*norm*zoom[t] );
				for  (s=si; s<=se; s++)
					gc_drawto( pmv_ch, SHC_PMSTYLE,
						xorig[t]+x[t][s]*norm*zoom[t],
						yorig[t]+y[t][s]*norm*zoom[t] );
			} /*endfor*/

			gc_flushbuffers();
			/* store selected time */
			oldtim = currtim;

		} /*endif*/  /* location changed */

	} /*endfor*/  /* FOREVER loop end */

} /* end of pm_movewdw */



/*---------------------------------------------------------------------*/



static void pmh_drawcurve( CHMAP ch, REAL x[], REAL y[],
	REAL xorig, REAL yorig, REAL norm, long lth )

/* draws one particle motion curve
 *
 * parameters of routine
 * CHMAP      ch;       input; channel map
 * REAL       x[];      input; x coordinates
 * REAL       y[];      input; y coordinates
 * REAL       xorig;    input; x origin
 * REAL       yorig;    input; y origin
 * REAL       norm;     input; normalisation
 * long       lth;      input; length of data arrays
 */
{
	/* executable code */

	gc_moveto( ch, xorig+norm*(*x++), yorig+norm*(*y++) );
	while  (--lth > 0)
		gc_drawto( ch, 0, xorig+norm*(*x++), yorig+norm*(*y++) );

} /* end of pmh_drawcurve */



/*---------------------------------------------------------------------*/



static void pmh_drawcircle( CHMAP ch, REAL ox, REAL oy, REAL r )

/* draws circle at centre (ox,oy) and radius r
 *
 * parameters of routine
 * CHMAP      ch;        input; output window
 * REAL       ox, oy;    input; centre of circle
 * REAL       r;         input; radius of circle
 */
{
#	define STEPNO 20
	/* local variables */
	int      i;        /* counter */
	REAL     angstep;  /* angle step */

	/* executable code */

	angstep = 2.0*BC_PI/(REAL)STEPNO;

	gc_moveto( ch, ox+r, oy );
	for  (i=1; i<=STEPNO; i++)
		gc_drawto( ch, 0, ox+r*cos((REAL)i*angstep),
			oy+r*sin((REAL)i*angstep) );

#	undef STEPNO
} /* end of pmh_drawcircle */



/*---------------------------------------------------------------------*/

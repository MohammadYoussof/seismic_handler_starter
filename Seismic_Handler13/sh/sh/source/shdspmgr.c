
/* File SHDSPMGR.C
 *      ==========
 *
 * version 16, 1-Sep-2006
 *
 * display manager of seismhandler program
 * K. Stammler, 12-MAR-1990
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
#include "basecnst.h"
#ifdef BC_STDLIB_EX
#include <stdlib.h>
#endif /* BC_STDLIB_EX */
#include <math.h>
#include BC_SYSBASE
#include "shconst.h"
#include "shvars.h"
#include BC_GCUSRDEF
#include "cpusrdef.h"
#include "trusrdef.h"
#include "tcusrdef.h"
#include "infoidx.h"
#include "fctxdm.h"
#include "fctxsl.h"
#include "sherrors.h"


/* prototypes of local routines */
static void dmh_findoverlay( TRACE *trc, float *ypos, BOOLEAN *found );
static void dmh_do_amplicut( SAMPLE dat[], long lth, SAMPLE cut );


#define TRCINFOLTH 100
#define TITLELINES 3


/* axis descriptor */
typedef struct axdescr {
	BOOLEAN  plot;          /* plot axis ? */
	float    vpos;          /* vertical position */
	float    line_s;        /* start position of axis line */
	float    line_e;        /* end position of line */
	float    tick_s;        /* position of first tick */
	float    tick_intv;     /* interval between ticks */
	float    lab1val;       /* value of first label */
	float    lab_intv;      /* label interval */
	int      lab1cnt;       /* number of first labelled tick */
	int      labcnt;        /* every "labcnt"-th tick is labelled */
	float    tick_lth;      /* length of an unlabelled tick (dspw_dmv) */
	float    ltick_lth;     /* length of a labelled tick (dspw_dmv) */
	float    labshift;      /* tick-relative (x-) position of label */
	float    labdist;       /* distance of label from axis */
	int      style;         /* text attribute block */
	char     labfmt[16];    /* format string of label */
	char     labeltext[BC_SHORTSTRLTH+1]; /* label text */
	float    labtxtpos_x;   /* x-position of label text */
	float    labtxtpos_y;   /* y-position of label text */
} AXIS;

typedef struct _plottitle {
	char     text[BC_LINELTH+1];   /* title text */
	float    x, y;                 /* position of title in */
} TITLE;                               /* units of width and height */



static float     dspx_dmv[SHC_MAXDLN]  /* display x-position */
	={0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0};
static float     dspy_dmv[SHC_MAXDLN]  /* display y-position */
	={0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0};
static float     dspw_dmv[SHC_MAXDLN]  /* display width */
	={100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0};
static float     dsph_dmv[SHC_MAXDLN]  /* display height */
	={10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0};
static float     trcsep_dmv={0.2};     /* vertical separation space betw. trcs */
static float     loff_dmv[SHC_MAXDLN]  /* left offset */
	={0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2};
static float     roff_dmv[SHC_MAXDLN]  /* right offset */
	={0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2};
static float     uoff_dmv[SHC_MAXDLN]  /* upper offset */
	={0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2};
static float     doff_dmv[SHC_MAXDLN]  /* lower offset */
	={0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2};
static unsigned  yentry_dmv[SHC_MAXDLN] /* y-position entry or NONE */
	={E_NONE,E_NONE,E_NONE,E_NONE,E_NONE,E_NONE,E_NONE,E_NONE,E_NONE};
static float     normsize_dmv[SHC_MAXDLN] /* ampl. size rel. to y-coo */
	={1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0};
static int       twdwset_dmv={0}; /* (bitmap) time windows set ? */
static int       ywdwset_dmv={0}; /* (bitmap) y-windows set ? */
static AXIS      timeaxis_dmv;    /* time axis descriptor */
static AXIS      yaxis_dmv;       /* y-axis descriptor */
static char      trcinfo_dmv[TRCINFOLTH+1];  /* trace info */
static float     trcinfx_dmv;     /* x-position factor of trace info */
static float     trcinfy_dmv;     /* y-position factor of trace info */
static int       norm_dmv[SHC_MAXDLN] /* normalisation type */
	={SHC_N_AF,SHC_N_AF,SHC_N_AF,SHC_N_AF,SHC_N_AF,SHC_N_AF,SHC_N_AF,
	  SHC_N_AF,SHC_N_AF,SHC_N_AF};
static TITLE     title_dmv[SHC_MAXDLN][TITLELINES];/* title of plots */
static SAMPLE    dmv_amplicut=2.0;/* amplitude cut value */

/* overlay variables */
#define OVLMAXLIST 80
	/* number of overlay lists */
#define OVLLISTLTH 10
	/* maximum length of a single overlay list */
static TRACE     *ovltrc_dmv[OVLMAXLIST][OVLLISTLTH]; /* overlay lists */
static int       ovllth_dmv[OVLMAXLIST];         /* lengths of lists */
static int       ovlnum_dmv;                     /* number of lists */



/* local routines */
void dm_haxis( AXIS *ax );
void dm_vaxis( AXIS *ax );
void dm_autoaxis( float start, float end, int labelmul, float *ticks,
	float *ticki, float *labs, float *labi, int *labelcnt );


/*------------------------------------------------------------------------*/



void dm_setup( STATUS *status )

/* sets up display parameters. Called before each redraw
 *
 * parameters of routine
 * STATUS        *status;       output; return status
 */

{
	/* local variables */
	int      dln;                  /* display list number */
	int      no_of_trcs;           /* number of traces on display */
	int      no_of_pos;            /* number of positions */
	void     *trc;                 /* info block pointer */
	int      i;                    /* counter */
	long     sample;               /* sample index */
	float    min_t, max_t;         /* time interval */
	float    from, to;             /* current time window */
	float    min_y, max_y, this_y; /* y-positions */
	int      l;                    /* list counter */
	float    ypos;                 /* overlay ypos */
	BOOLEAN  found;                /* overlay position found */
	long     ldmy;                 /* dummy */

	/* executable code */

	/* get display list number */
	dln = db_getdln( gc );
	if  (dln < 0)  return;

	/* get number of traces and number of trace positions */
	no_of_trcs = db_dsplth( gc );
	if  (no_of_trcs == 0)  return;
	if  (ovlnum_dmv > 0)  {  /* some traces are overlayed */
		no_of_pos = 0;
		for  (l=0; l<OVLMAXLIST; l++)
			no_of_pos += ovllth_dmv[l];
		no_of_pos = no_of_trcs - no_of_pos + ovlnum_dmv;
	} else {  /* each traces has its own position */
		no_of_pos = no_of_trcs;
	} /*endif*/
	if  (no_of_trcs == 0)  return;

	trc = db_dspfirst( gc, NULL );

	/* handle time window */
	if  (twdwset_dmv & (1 << dln))  {   /* set display sample indices */
		for  (i=0;i<no_of_trcs;i++)  {
			sample = dm_getsample( trc, dspx_dmv[dln], TRUE );
			db_setl( trc, EL_DSPFST, sample, status );
			if  (*status != SHE_NOERROR)  return;
			db_setl( trc, EL_DSPCNT, dm_getsample(trc,
				dspx_dmv[dln]+dspw_dmv[dln],TRUE)-sample+1, status );
			if  (*status != SHE_NOERROR)  return;
			trc = db_getp( trc, EP_DSPN, NULL );
		} /*endfor*/
	} else {   /* set new window coo */
		min_t = db_getr( trc, ER_TORIG, NULL );
		/* max_t = (float)db_getl( trc, EL_LENGTH, status ); */
		ldmy = db_getl( trc, EL_LENGTH, status );
		db_setl( trc, EL_DSPFST, 0L, NULL );
		db_setl( trc, EL_DSPCNT, ldmy, NULL );
		max_t = (float)ldmy;
		max_t = min_t + max_t * db_getr(trc,ER_DELTA,NULL);
		for  (i=1;i<no_of_trcs;i++)  {
			trc = db_getp( trc, EP_DSPN, NULL );
			ldmy = db_getl( trc, EL_LENGTH, status );
			db_setl( trc, EL_DSPFST, 0L, NULL );
			db_setl( trc, EL_DSPCNT, ldmy, NULL );
			from = db_getr( trc, ER_TORIG, NULL );
			if  (from < min_t)  min_t = from;
			to = (float)db_getl( trc, EL_LENGTH, status );
			to = from + to * db_getr(trc,ER_DELTA,NULL);
			if  (to > max_t)  max_t = to;
		} /*endfor*/
		if  ((max_t-min_t) < 1.e-10)  {
			*status = SHE_DSPZERO;
			return;
		} /*endif*/
		dspx_dmv[dln] = min_t;
		dspw_dmv[dln] = max_t - min_t;
	} /*endif*/

	/* handle y-window */
	if  (yentry_dmv[dln] == E_NONE)  {
		if  (ovlnum_dmv > 0)  {  /* reset all ES_SORIG's */
			trc = NULL;
			for  (i=0;i<no_of_trcs;i++)  {
				if  (trc == NULL)  {
					trc = db_dspfirst( gc, status );
				} else {
					trc = db_getp( trc, EP_DSPN, status );
				} /*endif*/
				if  (*status != SHE_NOERROR)  return;
				db_setr( trc, ER_SORIG, 0.0, NULL );
			} /*endfor*/
		} /*endif*/
		/* compute all ER_SORIG's new */
		min_y = 0.0;
		max_y = (float)no_of_pos + (float)(no_of_pos-1)*trcsep_dmv;
		this_y = 0.5;
		trc = NULL;
		for  (i=0;i<no_of_trcs;i++)  {
			if  (trc == NULL)  {
				trc = db_dspfirst( gc, status );
			} else {
				trc = db_getp( trc, EP_DSPN, status );
			} /*endif*/
			if  (*status != SHE_NOERROR)  return;
			if  (db_getf(trc,EF_OVERLAY,NULL))  {
				dmh_findoverlay( trc, &ypos, &found );
				if  (found)  {
					db_setr( trc, ER_SORIG, ypos, NULL );
				} else {
					db_setr( trc, ER_SORIG, this_y, NULL );
					this_y += 1. + trcsep_dmv;
				} /*endif*/
			} else {
				db_setr( trc, ER_SORIG, this_y, NULL );
				this_y += 1. + trcsep_dmv;
			} /*endif*/
		} /*endfor*/
		normsize_dmv[dln] = 1.0;
	} else {
		trc = NULL;
		for  (i=0;i<no_of_trcs;i++)  {
			if  (trc == NULL)  {
				trc = db_dspfirst( gc, status );
			} else {
				trc = db_getp( trc, EP_DSPN, status );
			} /*endif*/
			if  (*status != SHE_NOERROR)  return;
			this_y = db_getr(trc,yentry_dmv[dln],status);
			if  (*status != SHE_NOERROR)  return;
			db_setr( trc, ER_SORIG, this_y, NULL );
			if  (i == 0)  {
				min_y = this_y;
				max_y = this_y;
			} else {
				if  (this_y < min_y)  min_y = this_y;
				if  (this_y > max_y)  max_y = this_y;
			} /*endif*/
		} /*endfor*/
		normsize_dmv[dln] = (max_y-min_y)/(float)no_of_trcs;
	} /*endif*/

	if  (!(ywdwset_dmv & (1 << dln)))  {   /* y-window set already */
		dspy_dmv[dln] = min_y;
		dsph_dmv[dln] = max_y - min_y;
	} /*endif*/

} /* end of dm_setup */



/*------------------------------------------------------------------------*/



void dm_redraw( STATUS *status )

/* redraws display
 *
 * parameters of routine
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	int      dln;      /* display list number */
	void     *trc;     /* info block pointer */
	int      i;        /* counter */
	char     str[BC_LINELTH+1];  /* output string */
	float    x, y;     /* text position */
	int      reduct;   /* reduction factor */
	REAL     zoom;     /* zoom factor */
	SAMPLE   *swdw;    /* pointer to sample window */
	SAMPLE   *sp;      /* moving pointer */
	long     scnt;     /* sample counter */

	/* executable code */

	dln = db_getdln( gc );
	if  (dln < 0)  return;

	gc_erase( gc );
	if  (db_dsplth(gc) == 0)  return;
	gc_setcoo( gc, dspx_dmv[dln]-loff_dmv[dln]*dspw_dmv[dln],
		dspy_dmv[dln]-doff_dmv[dln]*dsph_dmv[dln],
		dspw_dmv[dln]*(1.+roff_dmv[dln]+loff_dmv[dln]),
		dsph_dmv[dln]*(1.+uoff_dmv[dln]+doff_dmv[dln]), status );
	if  (*status != SHE_NOERROR)  return;

	dm_norm();

	/* plot time axis */
	dm_haxis( &timeaxis_dmv );

	/* plot y-axis */
	dm_vaxis( &yaxis_dmv );

	trc = db_dspfirst( gc, status );
	if  (*status != SHE_NOERROR)  return;
	for  (i=0;i<db_dsplth(gc);i++)  {

		/* get reduction factor */
		reduct = db_geti( trc, EI_REDUCT, NULL );
		if  (reduct <= 0)  {
			reduct = 0;
			while  ((db_getl(trc,EL_DSPCNT,NULL)/(long)(++reduct)) > 
				-(long)db_geti(trc,EI_REDUCT,NULL)) {}
		} /*endif*/

		/* plot trace data */
		zoom = db_getr( trc, ER_ZOOM, NULL );
		if  (zoom == 0.0)  {
			zoom = db_getr(trc,ER_TORIG,NULL) +
				(float)db_getl(trc,EL_DSPFST,NULL)*
				db_getr(trc,ER_DELTA,NULL);
			gc_moveto( gc, zoom, db_getr(trc,ER_SORIG,NULL) );
			gc_drawto( gc, SHC_ZEROTRCSTYLE, zoom +
				(float)db_getl(trc,EL_DSPCNT,NULL)*
				db_getr(trc,ER_DELTA,NULL), db_getr(trc,ER_SORIG,NULL) );
		} else {
			if  (db_getf(trc,EF_AMPLICUT,NULL))  {
				swdw = 	(SAMPLE *)sy_allocmem( db_getl(trc,EL_DSPCNT,NULL),
					(int)sizeof(SAMPLE), status );
				if  (Severe(status))  return;
				sp = (SAMPLE *)db_getp( trc, EP_DATA, NULL ) +
					db_getl( trc, EL_DSPFST, NULL );
				for  (scnt=0; scnt<db_getl(trc,EL_DSPCNT,NULL); scnt++)
					swdw[scnt] = *sp++;
				dmh_do_amplicut( swdw,
					db_getl(trc,EL_DSPCNT,NULL), dmv_amplicut );
				gc_arrayplot( gc, db_geti(trc,EI_ATTRIB,NULL),
					db_getl(trc,EL_DSPCNT,NULL), reduct,
					db_getr(trc,ER_TORIG,NULL) +
					(float)db_getl(trc,EL_DSPFST,NULL)*
					db_getr(trc,ER_DELTA,NULL),
					db_getr(trc,ER_DELTA,NULL),
					db_getr(trc,ER_SORIG,NULL),
					swdw,
					db_getr(trc,ER_NORM,NULL)*zoom, status );
				sy_deallocmem( swdw );
			} else {
				gc_arrayplot( gc, db_geti(trc,EI_ATTRIB,NULL),
					db_getl(trc,EL_DSPCNT,NULL), reduct,
					db_getr(trc,ER_TORIG,NULL) +
					(float)db_getl(trc,EL_DSPFST,NULL)*
					db_getr(trc,ER_DELTA,NULL),
					db_getr(trc,ER_DELTA,NULL),
					db_getr(trc,ER_SORIG,NULL),
					(float *)db_getp(trc,EP_DATA,NULL) + db_getl(trc,EL_DSPFST,NULL),
					db_getr(trc,ER_NORM,NULL)*zoom, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
		} /*endif*/

		/* print trace info */
		if  (*trcinfo_dmv != '\0')  {
			x = dspx_dmv[dln] + trcinfx_dmv * dspw_dmv[dln];
			y = /*dspy_dmv[dln] +*/ db_getr(trc,ER_SORIG,NULL) +
				trcinfy_dmv * dsph_dmv[dln];
			tr_setruntrc( trc, i+1 );
			*str = '\0';  /* first call to dm_infstr */
			while  (dm_infstr(BC_LINELTH,str,status))  {
				if  (*status != SHE_NOERROR)  return;
				gc_text( gc, SHC_TRCINFOSTYLE, x, y, str );
				y -= gc_chheight( gc );
			} /*endwhile*/
			tr_setruntrc( NULL, 0 );
		} /*endif*/

		/* next trace */
		trc = db_getp( trc, EP_DSPN, NULL );

	} /*endfor*/

	/* plot titles */
	for  (i=0;i<TITLELINES;i++)
		if  (title_dmv[dln][i].text[0] != '\0')
			gc_text( gc, SHC_TITLESTYLE,
				dspx_dmv[dln] + title_dmv[dln][i].x*dspw_dmv[dln],
				dspy_dmv[dln] + title_dmv[dln][i].y*dsph_dmv[dln],
				title_dmv[dln][i].text );

	gc_flushbuffers();

} /* end of dm_redraw */



/*------------------------------------------------------------------------*/



void dm_timewdw( float lo_t, float hi_t, STATUS *status )

/* sets time window in display
 *
 * parameters of routine
 * float      lo_t;         input; lower bound of time window
 * float      hi_t;         input; upper bound of time window
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	int      dln;      /* display list number */

	/* executable code */

	dln = db_getdln( gc );
	if  (dln < 0)  return;

	if  ((lo_t == 0.0) && (hi_t == 0.0))  {
		twdwset_dmv &= ~(1 << dln);
		return;
	} else if  (lo_t >= hi_t)  {
		*status = SHE_ZWDW;
		return;
	} /*endif*/

	dspx_dmv[dln] = lo_t;
	dspw_dmv[dln] = hi_t - lo_t;
	twdwset_dmv |= (1 << dln);

} /* end of dm_timewdw */



/*------------------------------------------------------------------------*/



void dm_get_timewdw( float *lo_t, float *hi_t )

/* returns time window in display
 *
 * parameters of routine
 * float      *lo_t;         input; lower bound of time window
 * float      *hi_t;         input; upper bound of time window
 */
{
	/* local variables */
	int      dln;      /* display list number */

	/* executable code */

	*lo_t = *hi_t = 0.0;

	dln = db_getdln( gc );
	if  (dln < 0)  return;

	if  (!(1<<dln) & twdwset_dmv)  return;
	*lo_t = dspx_dmv[dln];
	*hi_t = *lo_t + dspw_dmv[dln];

} /* end of dm_get_timewdw */



/*------------------------------------------------------------------------*/



void dm_ywdw( float lo_y, float hi_y, STATUS *status )

/* sets y-window in display
 *
 * parameters of routine
 * float      lo_y;         input; lower bound of y-window
 * float      hi_y;         input; upper bound of y-window
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	int      dln;      /* display list number */

	/* executable code */

	dln = db_getdln( gc );
	if  (dln < 0)  return;

	if  ((lo_y == 0.0) && (hi_y == 0.0))  {
		ywdwset_dmv &= ~(1 << dln);
		return;
	} else if  (lo_y >= hi_y)  {
		*status = SHE_ZWDW;
		return;
	} /*endif*/

	dspy_dmv[dln] = lo_y;
	dsph_dmv[dln] = hi_y - lo_y;
	ywdwset_dmv |= (1 << dln);

} /* end of dm_ywdw */



/*------------------------------------------------------------------------*/



void dm_setyentry( unsigned ientry, STATUS *status )

/* sets y-entry for y-display arrangement
 *
 * parameters of routine
 * unsigned   ientry;      input; y-entry or E_NONE
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	int      dln;      /* display list number */

	/* executable code */

	dln = db_getdln( gc );
	if  (dln < 0)  return;

	if  (((ientry & E_TYPMASK) != ER_TYPE) && (ientry != E_NONE))  {
		*status = SHE_ILTYPE;
		return;
	} /*endif*/

	yentry_dmv[dln] = ientry;

} /* end of dm_setyentry */



/*------------------------------------------------------------------------*/



void dm_inftext( char *str, STATUS *status )

/* sets the info text of all traces
 *
 * parameter of routine
 * char      *str;      input; new info text
 * STATUS    *status;   output; return status
 */
{
	/* executable code */

	if  (strlen(str) > TRCINFOLTH)  {
		*status = SHE_STROVFL;
		return;
	} /*endif*/
	strcpy( trcinfo_dmv, str );

} /* end of dm_inftext */



/*------------------------------------------------------------------------*/



void dm_infpos( float xfac, float yfac )

/* sets the position factors of trace info text
 *
 * parameters of routine
 * float      xfac;      input; x-position factor, units of dspw_dmv
 * float      yfac;      input; y-position factor, units of dsph_dmv
 */
{
	trcinfx_dmv = xfac;
	trcinfy_dmv = yfac;

} /* end of dm_infpos */



/*------------------------------------------------------------------------*/



long dm_getsample( TRACE *ptr, float time, BOOLEAN check )

/* returns the sample point located at time. If check then array bounds are
 * corrected
 *
 * parameters of routine
 * char     *ptr;           input; info block pointer
 * float    time;           input; time position
 * BOOLEAN  check;          input; array bound check flag
 */
{
	/* local variables */
	long     idx;         /* sample index */
	long     lth;         /* length of array */

	/* executable code */

	idx = Nlong( (time-db_getr(ptr,ER_TORIG,NULL)) /
		db_getr(ptr,ER_DELTA,NULL) );
	if  (!check)  return idx;

	if  (idx < 0)  {
		idx = 0;
	} /*endif*/
	lth = db_getl( ptr, EL_LENGTH, NULL );
	if  (idx >= lth)  {
		idx = lth - 1;
	} /*endif*/
	return idx;

} /* end of dm_getsample */



/*------------------------------------------------------------------------*/



long dm_sgetsample( TRACE *ptr, REAL time, STATUS *status )

/* returns the sample point located at time.  If the sample point
 * is out of range an error status is returned.
 *
 * parameters of routine
 * TRACE    *ptr;           input; trace pointer
 * float    time;           input; time position
 * STATUS   *status;        input; array bound check flag
 *                          returns sample point index
 */
{
	/* local variables */
	long     idx;         /* sample index */

	/* executable code */

	idx = Nlong( (time-db_getr(ptr,ER_TORIG,NULL)) /
		db_getr(ptr,ER_DELTA,NULL) );

	if  ((idx < 0) || (idx >= db_getl(ptr,EL_LENGTH,NULL)))  {
		TSyStatus locstat=cBcNoError;      /* local status */
		char      stat[cBcShortStrLth+1];  /* station name */
		db_gets( ptr, ES_STATION, cBcShortStrLth, stat, &locstat );
		if  (locstat == cBcNoError)  {
			printf( "--> idx %ld  time %e  origin %e  delta %e\n", idx, time,
				db_getr(ptr,ER_TORIG,NULL), db_getr(ptr,ER_DELTA,NULL) );
			err_setcontext( " ## station: " );
			err_setcontext( stat );
			err_setcontext( " t:" );
			err_setcontext_r( time );
			err_setcontext( " o:" );
			err_setcontext_r( db_getr(ptr,ER_TORIG,&locstat) );
			if  (SySevere(&locstat))  err_setcontext( "%%%" );
			err_setcontext( " idx " );
		} else {
			err_setcontext( " ## idx " );
		} /*endif*/
		err_setcontext_l( idx );
		*status = SHE_ILSMP;
	} /*endif*/
	return idx;

} /* end of dm_sgetsample */



/*------------------------------------------------------------------------*/



void *dm_trace( int dln, REAL ypos, STATUS *status )

/* returns trace pointer; input is y-position
 *
 * paramters of routine
 * int       dln;        input; display list number
 * REAL      ypos;       input; y-position selected
 * STATUS    *status;    output; return status
 *                       returns trace pointer
 */
{
	/* local variables */
	void     *trc;      /* current trace */
	int      i;         /* counter */
	REAL     pos;       /* y-position of current trace */

	/* executable code */

	trc = NULL;
	for  (i=0;i<db_dsplth(dln);i++)  {
				  if  (trc == NULL)  {
					  trc = db_dspfirst( dln, status );
				  } else {
					  trc = db_getp( trc, EP_DSPN, status );
				  } /*endif*/
		if  (*status != SHE_NOERROR)  return NULL;
		pos = db_getr( trc, ER_SORIG, NULL );
		if  ((ypos >= (pos-0.5)) && (ypos <= (pos+0.5)))  return trc;
	} /*endfor*/

	*status = SHE_NOTRC;
	return NULL;

} /* end of dm_trace */



/*------------------------------------------------------------------------*/



void dm_norm( void )

/* sets normalization constants of each trace
 *
 * no parameters
 */
{
	/* local variables */
	int      dln;        /* display list number */
	int      i;          /* counter */
	void     *trc;       /* trace pointer */
	float    amax;       /* absolute maximum in one trace */
	float    cmin, cmax; /* current min, max */
	SAMPLE   totmax;     /* total maximum of all traces */
	int      locstat;    /* local status */

	/* executable code */

	if  (db_dsplth(gc) == 0)  return;
	dln = db_getdln( gc );
	if  (dln < 0)  return;
	locstat = SHE_NOERROR;

	switch  (norm_dmv[dln])  {
	case SHC_N_SF:
		trc = NULL;
		for  (i=0;i<db_dsplth(gc);i++)  {
				  if  (trc == NULL)  {
					  trc = db_dspfirst( gc, &locstat );
				  } else {
					  trc = db_getp( trc, EP_DSPN, NULL );
				  } /*endif*/
			cmin = db_getr( trc, ER_MINVAL, NULL );
			cmax = db_getr( trc, ER_MAXVAL, NULL );
			amax = (Abs(cmin) > Abs(cmax)) ? Abs(cmin) : Abs(cmax);
			if  (amax > SHC_EPSILON)  {
				db_setr( trc, ER_NORM, normsize_dmv[dln]/(2.*amax), NULL );
			} else {
				db_setr( trc, ER_NORM, 0.0, NULL );
			} /*endif*/
		} /*endfor*/
		break;
	case SHC_N_SW:
		trc = NULL;
		for  (i=0;i<db_dsplth(gc);i++)  {
				  if  (trc == NULL)  {
					  trc = db_dspfirst( gc, &locstat );
				  } else {
					  trc = db_getp( trc, EP_DSPN, NULL );
				  } /*endif*/
			sl_findmax( (SAMPLE *)db_getp(trc,EP_DATA,NULL) +
				db_getl(trc,EL_DSPFST,NULL), db_getl(trc,EL_DSPCNT,NULL),
					&cmin, &cmax );
			amax = (Abs(cmin) > Abs(cmax)) ? Abs(cmin) : Abs(cmax);
			if  (amax > SHC_EPSILON)  {
				db_setr( trc, ER_NORM, normsize_dmv[dln]/(2.*amax), NULL );
			} else {
				db_setr( trc, ER_NORM, 0.0, NULL );
			} /*endif*/
		} /*endfor*/
		break;
	case SHC_N_AF:
		totmax = 0.0;
		trc = NULL;
		for  (i=0;i<db_dsplth(gc);i++)  {
				  if  (trc == NULL)  {
					  trc = db_dspfirst( gc, &locstat );
				  } else {
					  trc = db_getp( trc, EP_DSPN, NULL );
				  } /*endif*/
			cmin = db_getr( trc, ER_MINVAL, NULL );
			cmax = db_getr( trc, ER_MAXVAL, NULL );
			amax = (Abs(cmin) > Abs(cmax)) ? Abs(cmin) : Abs(cmax);
			if  (amax > totmax)  totmax = amax;
		} /*endfor*/
		if  (totmax == 0.0)  return;
		trc = NULL;
		for  (i=0;i<db_dsplth(gc);i++)  {
			if  (trc == NULL)  {
				trc = db_dspfirst( gc, &locstat );
			} else {
				trc = db_getp( trc, EP_DSPN, NULL );
			} /*endif*/
			if  (totmax > SHC_EPSILON)  {
				db_setr( trc, ER_NORM, normsize_dmv[dln]/(2.*totmax), NULL );
			} else {
				db_setr( trc, ER_NORM, 0.0, NULL );
			} /*endif*/
		} /*endfor*/
		break;
	case SHC_N_AW:
		totmax = 0.0;
		trc = NULL;
		for  (i=0;i<db_dsplth(gc);i++)  {
			if  (trc == NULL)  {
				trc = db_dspfirst( gc, &locstat );
			} else {
				trc = db_getp( trc, EP_DSPN, NULL );
			} /*endif*/
			sl_findmax( (SAMPLE *)db_getp(trc,EP_DATA,NULL) +
				db_getl(trc,EL_DSPFST,NULL), db_getl(trc,EL_DSPCNT,NULL),
				&cmin, &cmax );
			amax = (Abs(cmin) > Abs(cmax)) ? Abs(cmin) : Abs(cmax);
			if  (amax > totmax)  totmax = amax;
		} /*endfor*/
		if  (totmax == 0.0)  return;
		trc = NULL;
		for  (i=0;i<db_dsplth(gc);i++)  {
			if  (trc == NULL)  {
				trc = db_dspfirst( gc, &locstat );
			} else {
				trc = db_getp( trc, EP_DSPN, NULL );
			} /*endif*/
			if  (totmax > SHC_EPSILON)  {
				db_setr( trc, ER_NORM, normsize_dmv[dln]/(2.*totmax), NULL );
			} else {
				db_setr( trc, ER_NORM, 0.0, NULL );
			} /*endif*/
		} /*endfor*/
		break;
	case SHC_N_C:
		trc = NULL;
		for  (i=0;i<db_dsplth(gc);i++)  {
			if  (trc == NULL)  {
				trc = db_dspfirst( gc, &locstat );
			} else {
				trc = db_getp( trc, EP_DSPN, NULL );
			} /*endif*/
			db_setr( trc, ER_NORM, normsize_dmv[dln], NULL );
		} /*endfor*/
		break;
	default:
		gc_write( TW, "*** program bug in dm_norm ***\n" );
		abort();
	} /*endswitch*/

} /* end of dm_norm */



/*------------------------------------------------------------------------*/



void dm_setnorm( int norm )

/* sets normalisation type
 *
 * parameter of routine
 * int       norm;        input; normalisation type
 */
{
	/* local variables */
	int      dln;      /* display list number */

	/* executable code */

	dln = db_getdln( gc );
	if  (dln < 0)  return;
	norm_dmv[dln] = norm;

} /* end of dm_setnorm */



/*------------------------------------------------------------------------*/



void dm_haxis( AXIS *ax )

/* draws a horizontal axis
 *
 * parameters of routine
 * AXIS      ax;            input; axis parameter
 */
{
	/* local variables */
	int      dln;           /* display list number */
	char     label[30];     /* labelling of axis */
	float    currpos;       /* current position */
	float    ticklth;       /* current tick length */
	float    currval;       /* current label value */
	int      labelcnt;      /* label counter */
	float    lshift;        /* label shift */
	float    ldist;         /* label distance */
	float    lines, linee;  /* line start & end position */
	float    ticks, ticki;  /* tick start & interval */
	float    labs, labi;    /* label start & interval */
	float    vertpos;       /* vertical position */

	/* executable code */

	if  (!(ax->plot))  return;

	dln = db_getdln( gc );
	if  (dln < 0)  return;

	if  (yentry_dmv[dln] == E_NONE)  {
		vertpos = ax->vpos * dsph_dmv[dln];
	} else {
		vertpos = dspy_dmv[dln] /* - normsize_dmv[dln]/2.0 */ +
			ax->vpos * dsph_dmv[dln];
	} /*endif*/

	if  ((ax->line_s == 0.0) && (ax->line_e == 0.0))  {
		lines = dspx_dmv[dln];
		linee = dspx_dmv[dln] + dspw_dmv[dln];
	} else {
		lines = ax->line_s;
		linee = ax->line_e;
	} /*endif*/
	if  ((ax->tick_intv == 0.0) && (ax->lab_intv == 0.0))  {
		dm_autoaxis( lines, linee, ax->labcnt, &ticks, &ticki,
			&labs, &labi, &labelcnt );
	} else {
		if  (ax->tick_intv == 0.0)  {
			ticks = lines;
			ticki = (linee-lines) / 10.;
		} else {
			ticks = ax->tick_s;
			ticki = ax->tick_intv;
		} /*endif*/
		if  (ax->lab_intv == 0.0)  {
			labs = ticks;
			labi = ticki;
		} else {
			labs = ax->lab1val;
			labi = ax->lab_intv;
		} /*endif*/
		labelcnt = ax->lab1cnt;
	} /*endif*/

	gc_moveto( gc, lines, vertpos );
	gc_drawto( gc, ax->style, linee, vertpos );

	lshift = ax->labshift * dspw_dmv[dln];
	ldist = ax->labdist * dsph_dmv[dln];
	currval = labs;

	for  (currpos=ticks;currpos<=linee; currpos += ticki)  {
		if  (--labelcnt == 0)  {  /* labelled tick */
			ticklth = ax->ltick_lth * dsph_dmv[dln];
			labelcnt = ax->labcnt;
			sprintf( label, ax->labfmt, currval );
			gc_text( gc, ax->style, currpos+lshift, vertpos+ldist, label );
		} else {
			ticklth = ax->tick_lth * dsph_dmv[dln];
		} /*endif*/
		gc_moveto( gc, currpos, vertpos );
		gc_drawto( gc, ax->style, currpos, vertpos-ticklth );
		currval += labi;
	} /*endfor*/

	if  (ax->labeltext[0] != '\0')
		gc_text( gc, ax->style, linee+(ax->labtxtpos_x)*dspw_dmv[dln],
			vertpos+(ax->labtxtpos_y)*dsph_dmv[dln], ax->labeltext );

} /* end of dm_haxis */



/*------------------------------------------------------------------------*/



void dm_vaxis( AXIS *ax )

/* draws a vertical axis
 *
 * parameters of routine
 * AXIS      ax;            input; axis parameter
 */
{
	/* local variables */
	int      dln;           /* display list number */
	char     label[30];     /* labelling of axis */
	float    currpos;       /* current position */
	float    ticklth;       /* current tick length */
	float    currval;       /* current label value */
	int      labelcnt;      /* label counter */
	float    lshift;        /* label shift */
	float    ldist;         /* label distance */
	float    lines, linee;  /* line start & end position */
	float    ticks, ticki;  /* tick start & interval */
	float    labs, labi;    /* label start & interval */
	float    vertpos;       /* vertical position */

	/* executable code */

	if  (!(ax->plot))  return;

	dln = db_getdln( gc );
	if  (dln < 0)  return;

	vertpos = dspx_dmv[dln] + ax->vpos * dspw_dmv[dln];

	if  ((ax->line_s == 0.0) && (ax->line_e == 0.0))  {
		lines = dspy_dmv[dln];
		linee = dspy_dmv[dln] + dsph_dmv[dln];
	} else {
		lines = ax->line_s;
		linee = ax->line_e;
	} /*endif*/
	if  ((ax->tick_intv == 0.0) && (ax->lab_intv == 0.0))  {
		dm_autoaxis( lines, linee, ax->labcnt, &ticks, &ticki,
			&labs, &labi, &labelcnt );
	} else {
		if  (ax->tick_intv == 0.0)  {
			ticks = lines;
			ticki = (linee-lines) / 10.;
		} else {
			ticks = ax->tick_s;
			ticki = ax->tick_intv;
		} /*endif*/
		if  (ax->lab_intv == 0.0)  {
			labs = ticks;
			labi = ticki;
		} else {
			labs = ax->lab1val;
			labi = ax->lab_intv;
		} /*endif*/
		labelcnt = ax->lab1cnt;
	} /*endif*/

	gc_moveto( gc, vertpos, lines );
	gc_drawto( gc, ax->style, vertpos, linee );

	lshift = ax->labshift * dsph_dmv[dln];
	ldist = ax->labdist * dspw_dmv[dln];
	currval = labs;

	for  (currpos=ticks;currpos<=linee; currpos += ticki)  {
		if  (--labelcnt == 0)  {  /* labelled tick */
			ticklth = ax->ltick_lth * dspw_dmv[dln];
			labelcnt = ax->labcnt;
			sprintf( label, ax->labfmt, currval );
			gc_text( gc, ax->style, vertpos+ldist, currpos+lshift, label );
		} else {
			ticklth = ax->tick_lth * dspw_dmv[dln];
		} /*endif*/
		gc_moveto( gc, vertpos, currpos );
		gc_drawto( gc, ax->style, vertpos-ticklth, currpos );
		currval += labi;
	} /*endfor*/

} /* end of dm_vaxis */



/*------------------------------------------------------------------------*/



void dm_setaxis( char *name, char *item, char *value, STATUS *status )

/* sets item in axis descriptor "name"
 *
 * parameters of routine
 * char      *name;      input; name of descriptor
 * char      *item;      input; name of item
 * char      *value;     input; new value
 * STATUS    *status;    output; return status
 */
{
	/* local variables */
	AXIS     *axptr;       /* pointer to axis descriptor */

	/* executable code */

	if  (strcmp(name,"TIMEAXIS") == 0)  {
		axptr = &timeaxis_dmv;
	} else if  (strcmp(name,"Y-AXIS") == 0)  {
		axptr = &yaxis_dmv;
	} else {
		*status = SHE_UKAXIS;
		return;
	} /*endif*/

	if  (strcmp(item,"VPOS") == 0)  {
		sscanf( value, "%f", &(axptr->vpos) );
	} else if  (strcmp(item,"LINE_S") == 0)  {
		sscanf( value, "%f", &(axptr->line_s) );
	} else if  (strcmp(item,"LINE_E") == 0)  {
		sscanf( value, "%f", &(axptr->line_e) );
	} else if  (strcmp(item,"TICK_S") == 0)  {
		sscanf( value, "%f", &(axptr->tick_s) );
	} else if  (strcmp(item,"TICK_INTV") == 0)  {
		sscanf( value, "%f", &(axptr->tick_intv) );
	} else if  (strcmp(item,"LAB1VAL") == 0)  {
		sscanf( value, "%f", &(axptr->lab1val) );
	} else if  (strcmp(item,"LAB_INTV") == 0)  {
		sscanf( value, "%f", &(axptr->lab_intv) );
	} else if  (strcmp(item,"LAB1CNT") == 0)  {
		sscanf( value, "%d", &(axptr->lab1cnt) );
	} else if  (strcmp(item,"LABCNT") == 0)  {
		sscanf( value, "%d", &(axptr->labcnt) );
	} else if  (strcmp(item,"TICK_LTH") == 0)  {
		sscanf( value, "%f", &(axptr->tick_lth) );
	} else if  (strcmp(item,"LTICK_LTH") == 0)  {
		sscanf( value, "%f", &(axptr->ltick_lth) );
	} else if  (strcmp(item,"LABSHIFT") == 0)  {
		sscanf( value, "%f", &(axptr->labshift) );
	} else if  (strcmp(item,"LABDIST") == 0)  {
		sscanf( value, "%f", &(axptr->labdist) );
	} else if  (strcmp(item,"STYLE") == 0)  {
		sscanf( value, "%d", &(axptr->style) );
	} else if  (strcmp(item,"LABFMT") == 0)  {
		sl_prepformatstr( value );
		strcpy( axptr->labfmt, value );
	} else if  (strcmp(item,"LABELTEXT") == 0)  {
		strncpy( axptr->labeltext, value, BC_SHORTSTRLTH );
	} else if  (strcmp(item,"LABTXTPOS_X") == 0)  {
		sscanf( value, "%f", &(axptr->labtxtpos_x) );
	} else if  (strcmp(item,"LABTXTPOS_Y") == 0)  {
		sscanf( value, "%f", &(axptr->labtxtpos_y) );
	} else if  (strcmp(item,"PLOT") == 0)  {
		if  (strcmp(value,"ON") == 0)  {
			axptr->plot = TRUE;
		} else if  (strcmp(value,"OFF") == 0)  {
			axptr->plot = FALSE;
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else {
		*status = SHE_UKITEM;
		return;
	} /*endif*/

} /* end of dm_setaxis */



/*------------------------------------------------------------------------*/



void dm_autoaxis( float start, float end, int labelmul, float *ticks,
	float *ticki, float *labs, float *labi, int *labelcnt )

/* computes axis parameters
 *
 * parameters of routine
 * float      start;      input; start coo
 * float      end;        input; end coo
 * int        labelmul;   input; every "labelmul"-th tick is labelled
 * float      *ticks;     output; position of first tick
 * float      *ticki;     output; interval of ticks
 * float      *labs;      output; value of first label
 * float      *labi;      output; label interval
 * int        *labelcnt;  output; number of first labelled tick
 */
{
	/* local variables */
	float    width;    /* width of axis */
	int      labnum, ticknum;

	/* executable code */

	/* compute labelled intervals */
	width = end - start;
	*labi = pow( 10., floor(log10(width)) );
	while  ((width / *labi) < 3.0)
		*labi /= 2.0;
	*ticki = *labi / (float)labelmul;

	if  (start == 0.0)  {
		*labs = 0.0;
		*labelcnt = 1;
	} else if  ((start < 0.0) && (end > 0.0))  {
		labnum = Nint(floor((-start+0.01) / *labi));
		ticknum = Nint(floor((-start+0.01) / *ticki));
		*labelcnt = ticknum - labelmul*labnum + 1;
		*labs = -(float)ticknum * (*ticki);
	} else if  (start > 0.0)  {
		labnum = 1;
		while  (((float)labnum * *labi) < start)
			labnum++;
		ticknum = labnum * labelmul;
		while  (((float)ticknum * *ticki) >= start)
			ticknum--;
		*labelcnt = labelmul*labnum - (++ticknum) + 1;
		*labs = (float)ticknum * (*ticki);
	} else {
		labnum = -1;
		while  (((float)labnum * *labi) >= start)
			labnum--;
		ticknum = ++labnum * labelmul;
		while  (((float)ticknum * *ticki) >= start)
			ticknum--;
		*labelcnt = labelmul*labnum - (++ticknum) + 1;
		*labs = (float)ticknum * (*ticki);
	} /*endif*/

	*labi /= (float)labelmul;
	*ticki = *labi;
	*ticks = *labs;

} /* end of dm_autoaxis */



/*------------------------------------------------------------------------*/



void dm_dspcoo( float *x, float *y, float *w, float *h )

/* returns current display coordinates
 *
 * parameters of routine
 * float *x, *y, *w, *h;    output; x-pos, y-pos, width & height
 */
{
	/* local variables */
	int      dln;      /* display list number */

	/* executable code */

	dln = db_getdln( gc );
	if  (dln < 0)  dln = 0;
	if  (x != NULL)  *x = dspx_dmv[dln];
	if  (y != NULL)  *y = dspy_dmv[dln];
	if  (w != NULL)  *w = dspw_dmv[dln];
	if  (h != NULL)  *h = dsph_dmv[dln];

} /* end of dm_dspcoo */



/*------------------------------------------------------------------------*/



void dm_setmargin( int dln, char id, float margin, STATUS *status )

/* sets new value of margin
 *
 * parameters of routine
 * int        dln;     input; display list number
 * char       id;      input; L(eft), R(ight), T(op), B(ottom)
 * float      margin;  input; margin value
 * STATUS     *status; output; return status;
 */
{
	/* executable code */

	dln = db_getdln( dln );
	if  (dln < 0)  return;

	switch  (id)  {
	case 'L':   loff_dmv[dln] = margin;  break;
	case 'R':   roff_dmv[dln] = margin;  break;
	case 'T':   uoff_dmv[dln] = margin;  break;
	case 'B':   doff_dmv[dln] = margin;  break;
	default:    *status = SHE_ILPAR;     break;
	} /*endswitch*/

} /* end of dm_setmargin */



/*------------------------------------------------------------------------*/



void dm_settitle( int dln, unsigned lineno, char text[], STATUS *status )

/* sets new title text of line "lineno"
 *
 * parameters of routine
 * int        dln;          input; display list number
 * unsigned   lineno;       input; line number
 * char       text[];       input; new text of title line
 * STATUS     *status;      output; return status
 */
{
	/* executable code */

	dln = db_getdln( dln );
	if  (dln < 0)  return;

	if  (lineno >= TITLELINES)  {
		*status = SHE_SPECERROR+7;
		return;
	} else if  (strlen(text) > BC_LINELTH)  {
		*status = SHE_STROVFL;
		return;
	} /*endif*/

	strcpy( title_dmv[dln][lineno].text, text );

} /* end of dm_settitle */



/*------------------------------------------------------------------------*/



void dm_settitlepos( int dln, unsigned lineno, float xpos, float ypos,
	STATUS *status )

/* sets new position of title line "lineno" in units of width and
 * height of display
 *
 * parameters of routine
 * int        dln;          input; display list number
 * unsigned   lineno;       input; line number
 * float      xpos, ypos;   input; new position of title
 * STATUS     *status;      output; return status
 */
{
	/* executable code */

	dln = db_getdln( dln );
	if  (dln < 0)  return;

	if  (lineno >= TITLELINES)  {
		*status = SHE_SPECERROR+7;
		return;
	} /*endif*/

	title_dmv[dln][lineno].x = xpos;
	title_dmv[dln][lineno].y = ypos;

} /* end of dm_settitle */



/*------------------------------------------------------------------------*/



void dm_amplicut( SAMPLE cut )

/* sets cut off value for amplitudes
 *
 * parameters of routine
 * float      cut;    input; cut off value
 */
{
	/* executable code */

	dmv_amplicut = cut;

} /* end of dm_amplicut */



/*------------------------------------------------------------------------*/



BOOLEAN dm_infstr( int maxlth, char *str, STATUS *status )

/* returns info string to display
 *
 * parameters of routine
 * int       maxlth;    input; maximum length of string
 * char      *str;      output; info string
 * STATUS    *status;   output; return status
 *                      returns whether a string is available
 */
{
	/* local variables */
	static PARAM par;      /* parameter block */
	static int   i;        /* counter */
	char         s[BC_LINELTH+1];  /* scratch string */
	int          strlth;   /* length of output string */

	/* executable code */

	if  (*str == '\0')  {  /* first request */
		cp_parse( trcinfo_dmv, &par, status );
		if  (*status != SHE_NOERROR)  return FALSE;
		tr_partrans( &par, status );
		if  (*status == SHE_NOINFO)  *status = SHE_NOERROR;
		if  (Severe(status))  return FALSE;
		i = 1;
		if  (cp_pnum(&par) == 0)  return FALSE;
	} else if  (i > cp_pnum(&par))  {
		return FALSE;
	} /*endif*/

	*str = '\0';
	strlth = 0;
	while  (i<=cp_pnum(&par))  {
		cp_getstr( &par, i, 0, "", BC_LINELTH, s, status );
		if  (*status != SHE_NOERROR)  return FALSE;
		if  (*s == '\\')  {
			if  (*str == '\0')  return FALSE;
			i++;
			return TRUE;
		} /*endif*/
		strlth += (int)strlen( s ) + 1;
		if  (strlth > maxlth)  { *status = SHE_STROVFL; return FALSE; }
		strcat( str, s );
		if  (i++ < cp_pnum(&par))  strcat( str, " " );
	} /*endwhile*/

	return (*str != '\0');

} /* end of dm_infstr */



/*------------------------------------------------------------------------*/



void dm_setoverlay( TRACE *list[], int lth, STATUS *status )

/* defines new overlay list
 *
 * parameters of routine
 * TRACE      *list[];    input; list to be added
 * int        lth;        input; length of list
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	int      t;        /* trace counter */
	int      l;        /* list counter */

	/* executable code */

	if  (ovlnum_dmv == OVLMAXLIST)  {
		*status = SHE_SPECERROR+11;
		return;
	} else if  (lth > OVLLISTLTH)  {
		*status = SHE_SPECERROR+12;
		return;
	} /*endif*/

	/* check OVERLAY bit */
	for  (t=0; t<lth; t++)
		if  (db_getf(list[t],EF_OVERLAY,NULL))  {
			*status = SHE_SPECERROR+13;
			return;
		} /*endif*/

	/* set overlay bit */
	for  (t=0; t<lth; t++)  {
		db_setf( list[t], EF_OVERLAY, TRUE, status );
		if  (Severe(status))  return;
	} /*endfor*/

	/* increment list counter & find list number */
	ovlnum_dmv++;
	l = 0;
	while  (ovllth_dmv[l] > 0)
		l++;

	/* copy list */
	ovllth_dmv[l] = lth;
	for  (t=0; t<lth; t++)
		ovltrc_dmv[l][t] = list[t];

} /* end of dm_setoverlay */



/*------------------------------------------------------------------------*/



void dm_deloverlays( void )

/* deletes all overlays
 * no parameters
 */
{
	/* local variables */
	int      l, t;        /* list & trace counter */

	/* executable code */

	ovlnum_dmv = 0;
	for  (l=0; l<OVLMAXLIST; l++)  {
		for  (t=0; t<ovllth_dmv[l]; t++)
			db_setf( ovltrc_dmv[l][t], EF_OVERLAY, FALSE, NULL );
		ovllth_dmv[l] = 0;
	} /*endfor*/

} /* end of dm_deloverlays */



/*------------------------------------------------------------------------*/



static void dmh_findoverlay( TRACE *trc, float *ypos, BOOLEAN *found )

/* tries to find overlay position for trace "trc".  A valid y-position
 * is found, when a trace in the same overlay list as "trc" and has
 * a y-position different from zero.
 *
 * parameters of routine
 * TRACE      *trc;         input; trace pointer
 * float      *ypos;        output; y-position found
 * BOOLEAN    *found;       output; y-position found
 */
{
	/* local variables */
	int      l, t;     /* list & trace counters */

	/* executable code */

	/* find overlay list */
	*found = FALSE;
	for  (l=0; l<OVLMAXLIST; l++)  {
		for  (t=0; t<ovllth_dmv[l]; t++)  {
			*found = (ovltrc_dmv[l][t] == trc);
			if  (*found)  break;
		} /*endfor*/
		if  (*found)  break;
	} /*endfor*/
	if  (!(*found))  {
		sy_alert( "*** programming error in overlay lists ***" );
		return;
	} /*endif*/

	/* find y-position */
	*found = FALSE;
	for  (t=0; t<ovllth_dmv[l]; t++)  {
		*ypos = db_getr( ovltrc_dmv[l][t], ER_SORIG, NULL );
		*found = (*ypos != 0.0);
		if  (*found)  return;
	} /*endfor*/

} /* end of dmh_findoverlay */



/*------------------------------------------------------------------------*/



static void dmh_do_amplicut( SAMPLE dat[], long lth, SAMPLE cut )

/* cut all amplitudes larger than "cut"
 *
 * parameters of routine
 * SAMPLE     dat[];     modify; data array to be cut
 * long       lth;       input; length of data array
 * SAMPLE     cut;       input; maximum amplitude
 */
{
	/* executable code */

	while  (lth > 0)  {
		if  (Abs(*dat) > cut)
			*dat = (*dat > 0.0) ? cut : -cut;
		dat++;
		lth--;
	} /*endwhile*/

} /* end of dmh_do_amplicut */



/*------------------------------------------------------------------------*/


/* file SHMENU2.C
 *      =========
 *
 * version 22, 22-May-2006
 *
 * menu routines of seismhandler
 * K. Stammler, 2-JUL-1990
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "shconst.h"
#include "shvars.h"
#include "cpusrdef.h"
#include "rfusrdef.h"
#include "ffusrdef.h"
#include "scusrdef.h"
#include "ssusrdef.h"
#include "erusrdef.h"
#include "qiusrdef.h"
#include "qfusrdef.h"
#include "uiusrdef.h"
#include "gcusrdef.h"
#include "tcusrdef.h"
#include "infoidx.h"
#include "fctxcr.h"
#include "fctxdm.h"
#include "fctxsl.h"
#include "fctxml.h"
#include "fctxmn2.h"
#include "sherrors.h"



/*-------------------------------------------------------------------------*/



void mn2_fili( PARAM *par, int *status )

/* reads one or more filter files into memory
 * 1. param:  F or R  (FFT filters or recursive filters)
 * 2.-N. param:  filter names
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	char     ch[4];                 /* kind of filter */
	char     *flist[RFC_MAXFILTER]; /* filter list */
	int      plist[RFC_MAXFILTER];  /* position list */
	int      listlth;               /* length of lists */
	int      i;                     /* counter */
	BOOLEAN  recursive;             /* recursive filter or FFT ? */
	BOOLEAN  digital;               /* digital filter */

	/* executable code */

	cp_getstr( par, 1, tc, "   R(ecursive) or F(ft): ", 3, ch, status );
	if  (*status != SHE_NOERROR)  return;

	recursive = (*ch == 'R');
	digital = (*ch == 'T');
	if  (!recursive && !digital && (*ch != 'F'))  {
		err_setcontext( " ## filter type " ); err_setcontext( ch );
		*status = SHE_ILPAR; return;
	} /*endif*/

	listlth = cp_pnum( par ) - 1;
	for  (i=2; i<=cp_pnum(par); i++)  {
		flist[i-2] = par->p[i];
		sl_prepfilnam( flist[i-2], plist+i-2, status );
		if  (*status != SHE_NOERROR)  return;
	} /*endfor*/

	if  (recursive)  {
		rf_filter_input( listlth, flist, plist, status );
	} else if  (digital)  {
		if  (cp_pnum(par) > 2)  {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
		ff_filfunc_input( flist[0], NULL, status );
	} else {
		ff_filter_input( listlth, flist, plist, status );
		if  (*status != SHE_NOERROR)  return;
		if  (cp_qexist(par,"COMPRESS"))  ff_compress( status );
	} /*endif*/

} /* end of mn2_fili */



/*-------------------------------------------------------------------------*/



void mn2_filter( PARAM *par, int *status )

/* filters one or more traces with filters read with FILI
 * 1. param:  F,R,A,H,M  (FFT or Recursive filters, Attenuation,
 *                        Hilbert, Minimum delay)
 * 2. param:  trace list
 * 3. param:  lower bound of time window
 * 4. param:  upper bound of time window
 * 5. param:  if attenuation, t* in sec
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];  /* trace list */
	int      listlth;             /* list length */
	TRACE    *new;                /* new trace */
	SAMPLE   *datptr;             /* pointer to sample data */
	char     kind[4];             /* filter type */
	REAL     lowdw, hiwdw;        /* time window */
	long     loidx, hiidx;        /* trace window in samples */
	long     trclth;              /* length of trace */
	int      fulltrace;           /* take full trace ? */
	REAL     attenuation;         /* attenuation (t* in sec) */
	REAL     dt;                  /* sample distance of current trace */
	int      t;                   /* trace counter */
	float    taper;               /* taper window */
	int      inplev;              /* input level */
	RFT_STARTUP lpc;              /* special startup for rec filters */
	char     str[BC_LINELTH+1];   /* scratch string */
	int      phasemode;           /* mode of hilbert phase computation */

	/* executable code */

	if  (cp_pnexc(par,5,status))  return;

	if  (!sl_qualr(par,"TAPER",&taper,status))  {
		if  (Severe(status))  return;
		taper = 1.0;
	} /*endif*/
	lpc.demean = sl_quals( par, "DEMEAN", BC_LINELTH, str, status );
	lpc.use_lpc = sl_quals( par, "LPC", BC_LINELTH, str, status );
	if  (Severe(status))  return;
	if  (lpc.use_lpc)  {
		if  (sscanf(str,"%d,%d,%d",&lpc.pred_length,
			&lpc.pred_order,&lpc.src_length) != 3)  {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} /*endif*/

	if  (cp_qexist(par,"PARENT"))  {
		inplev = -1;
	} else if  (cp_qexist(par,"INTERACTIVE"))  {
		inplev = shv_maininput;
	} else {
		inplev = ss_inplev();
	} /*endif*/

	cp_getstr( par, 1, tc,
		"   R(ecursive), F(ft), A(ttenuation), H(ilbert) or M(inimum delay) <RFAHMTC>: ",
		3, kind, status );
	if  (*status != SHE_NOERROR)  return;
	if  ((*kind != 'R') && (*kind != 'F') && (*kind != 'A') &&
		(*kind != 'H') && (*kind != 'M') && (*kind != 'T')  &&
		(*kind != 'C') && (*kind != 'P')) {
		err_setcontext( " ## filter type " ); err_setcontext( kind );
		*status = SHE_ILPAR;
		return;
	} /*endif*/

	ml_gettrcpar( par, 2, tc, "   trace list: ", trc, &listlth, status );
	if  (*status != SHE_NOERROR)  return;

	fulltrace = !cp_pentered( par, 3, status );
	if  (*status != SHE_NOERROR)  return;

	if  (!fulltrace)  {
		ui_shift( inplev );
		ml_windowpars( par, 3, tc, "   lo bound: ", "   hi bound: ",
			&lowdw, &hiwdw, status );
		ui_unshift();
		if  (Severe(status))  return;
	} /*endif*/

	if  (*kind == 'A')  {
		cp_getfloat( par, 5, tc, "   attenuation: ", &attenuation, status );
		if  (*status != SHE_NOERROR)  return;
	} else if  (*kind == 'M' || *kind == 'C')  {
		cp_getfloat( par, 5, tc, "   offset: ", &attenuation, status );
		if  (*status != SHE_NOERROR)  return;
	} /*endif*/

	db_newlist();

	for  (t=0; t<listlth; t++)  {

		/* get trace window */
		dt = db_getr( trc[t], ER_DELTA, NULL );
		if  (fulltrace)  {
			loidx = 0;
			hiidx = db_getl( trc[t], EL_LENGTH, NULL ) - 1;
			lowdw = db_getr(trc[t],ER_TORIG,NULL);
		} else {
			loidx = dm_getsample( trc[t], lowdw, TRUE );
			hiidx = dm_getsample( trc[t], hiwdw, TRUE );
		} /*endif*/
		trclth = hiidx - loidx + 1;

		/* create new trace */
		new = db_create( status );
		if  (*status != SHE_NOERROR)  return;
		datptr = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
		if  (*status != SHE_NOERROR)  {
			db_delete( new );
			return;
		} /*endif*/

		/* perform filtering */
		if  (*kind == 'R')  {
			rf_filter( (SAMPLE *)db_getp(trc[t],EP_DATA,NULL)+loidx,
				trclth, dt, &lpc, datptr, status );
		} else if  (*kind == 'F')  {
			ff_filter( FALSE, (SAMPLE *)db_getp(trc[t],EP_DATA,NULL)+loidx,
				trclth, dt, taper, datptr, status );
		} else if  (*kind == 'T')  {
			ff_filter( TRUE, (SAMPLE *)db_getp(trc[t],EP_DATA,NULL)+loidx,
				trclth, dt, taper, datptr, status );
		} else if  (*kind == 'H')  {
			ff_hilbert( (SAMPLE *)db_getp(trc[t],EP_DATA,NULL)+loidx,
				trclth, datptr, status );
		} else if  (*kind == 'P')  {
			phasemode = FFC_GET_PHASE;
			if  (cp_qexist(par,"REAL"))  phasemode = FFC_GET_PHASE_R;
			if  (cp_qexist(par,"IMAG"))  phasemode = FFC_GET_PHASE_I;
			ff_hilbphase( phasemode, (SAMPLE *)db_getp(trc[t],EP_DATA,NULL)+loidx,
				trclth, datptr, status );
		} else if  (*kind == 'M' || *kind == 'C')  {
			ff_mindelay( (SAMPLE *)db_getp(trc[t],EP_DATA,NULL)+loidx,
				trclth, Nlong(attenuation/db_getr(trc[t],ER_DELTA,NULL)),
				(*kind == 'C'), datptr, status );
		} else {
			ff_attenuate( (SAMPLE *)db_getp(trc[t],EP_DATA,NULL)+loidx,
				trclth, dt, attenuation, datptr, status );
		} /*endif*/
		if  (*status != SHE_NOERROR)  {
			sy_deallocmem( datptr ); db_delete( new );
			return;
		} /*endif*/

		/* set trace info */
		ml_cpyinfo( trc[t], new, status );
		if  (Severe(status))  {
			sy_deallocmem( datptr ); db_delete( new );
			return;
		} /*endif*/
		ml_inittrc( new, datptr, trclth, dt );
		if  (lowdw > db_getr(trc[t],ER_TORIG,NULL))
			db_setr( new, ER_TORIG, lowdw, NULL );
		db_setf( new, EF_MODIF, TRUE, NULL );
		ml_newstarttime( trc[t], new, lowdw, status );
		if  (Severe(status))  {
			sy_deallocmem( datptr ); db_delist( gc, new ); db_delete( new );
			return;
		} /*endif*/

	} /*endfor*/

} /* end of mn2_filter */



/*-------------------------------------------------------------------------*/



void mn2_cmd( PARAM *par, int *status )

/* creates command file containing all previously entered command
 * (since start of session or last CMD command).
 * 1. param:  name of command file (no extension)
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	char     fnam[BC_LINELTH+1];   /* new filename */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	cp_getstr( par, 1, tc, "   name of command file: ",
		BC_LINELTH-4, fnam, status );
	if  (Severe(status))  return;

	ui_exit( fnam );
	ui_initialize( protfile_shv, status );

} /* end of mn2_cmd */



/*-------------------------------------------------------------------------*/



void mn2_time( PARAM *par, int *status )

/* returns time value of selected position
 * 1. param:  output var - absolute time position
 * 2. param:  output var - relative time position
 * 3. param:  output var - y value
 * 4. param:  output var - trace address
 * 5. param:  output var - key pressed
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	REAL     x, y;     /* selected position */
	REAL     oldx;     /* previous x position */
	char     ch;       /* key pressed */
	char     str[BC_LINELTH+1];       /* scratch */
	char     timstr[TCC_TIMSTRLTH+1]; /* time string */
	TIME     starttime;               /* start time of trace */
	TRACE    *trc;                    /* selected trace */
	int      first;                   /* first loop ? */
	int      selmode;                 /* select mode */
	int      inplev;                  /* input level */
	BOOLEAN  exit_key;                /* exit_key pressed */
	STATUS   locstat;                 /* local status */

	/* executable code */

	exit_key = FALSE;
	if  (cp_pnexc(par,5,status))  return;
	selmode = MM_TRCMARK;
	if  (cp_qexist(par,"NOMARK"))  selmode = MM_NOMARK;

	if  (cp_qexist(par,"PARENT"))  {
		inplev = -1;
	} else if  (cp_qexist(par,"INTERACTIVE"))  {
		inplev = shv_maininput;
	} else {
		inplev = ss_inplev();
	} /*endif*/

	if  (cp_pnum(par) == 0)  {
		oldx = 0;
		first = TRUE;
		gc_write( tc, "   select time points\n" );
		FOREVER  {
			ui_shift( inplev );
			cr_gettrctime( selmode, &trc, &x, status );
			ui_unshift();  /* switch back to current level */
			if  (*status == SHE_EXIT)  {*status = SHE_NOERROR; return;}
			if  (*status == SHE_NOTRC)  {
				*status = SHE_NOERROR;
				strcpy( timstr, "*** no trace selected ***" );
			} else {
				if  (Severe(status))  return;
				locstat = SHE_NOERROR;
				db_gett( trc, ET_START, &starttime, &locstat );
				if  (locstat == SHE_NOERROR)  {;
					tc_aadd( &starttime, x-db_getr(trc,ER_TORIG,NULL), &starttime );
					tc_a2t( &starttime, timstr, status );
					if  (*status != SHE_NOERROR)  return;
				} else {
					strcpy( timstr, "*** no start time ***" );
				} /*endif*/
			} /*endif*/
			if  (!first)  {
				sprintf( str, "   abs: %s  rel: %f  diff: %f\n",
					timstr, x, x-oldx );
			} else {
				sprintf( str, "   abs: %s  rel: %f\n", timstr, x );
			} /*endif*/
			gc_write( tc, str );
			oldx = x;
			first = FALSE;
		} /*endfor*/
	} /*endif*/

	ui_shift( inplev );
	cr_getloc( selmode, &x, &y, &ch, status );
	ui_unshift();  /* switch back to current level */
	if  (*status == SHE_EXIT)  {
		*status = SHE_NOERROR;
		exit_key = TRUE;
	} /*endif*/
	if  (Severe(status))  return;
	trc = NULL;

	if  (cp_pentered(par,1,status))  {
		cp_getstr( par, 1, tc, "@@", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		trc = dm_trace( gc, y, status );
		if  (*status != SHE_NOERROR)  return;
		db_gett( trc, ET_START, &starttime, status );
		if  (*status != SHE_NOERROR)  return;
		tc_aadd( &starttime, x-db_getr(trc,ER_TORIG,NULL), &starttime );
		tc_a2t( &starttime, timstr, status );
		if  (*status != SHE_NOERROR)  return;
		sl_setsymbol( str, timstr, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (cp_pentered(par,2,status))  {
		cp_getstr( par, 2, tc, "@@", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		sprintf( timstr, "%e", x );
		sl_setsymbol( str, timstr, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (cp_pentered(par,3,status))  {
		cp_getstr( par, 3, tc, "@@", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		sprintf( timstr, "%e", y );
		sl_setsymbol( str, timstr, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (cp_pentered(par,4,status))  {
		cp_getstr( par, 4, tc, "@@", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (exit_key)  {
			strcpy( timstr, "****" );
		} else {
			if  (trc == NULL)
				trc = dm_trace( gc, y, status );
			if  (Severe(status))  return;
			/* old version: sprintf( timstr, "A:%lp", trc ); */
			sprintf( timstr, "%d", db_gettrcpos( trc ) );
		} /*endif*/
		sl_setsymbol( str, timstr, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (cp_pentered(par,5,status))  {
		cp_getstr( par, 5, tc, "@@", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		*timstr = ch;
		timstr[1] = '\0';
		sl_setsymbol( str, timstr, status );
		if  (Severe(status))  return;
	} /*endif*/

} /* end of mn2_time */



/*-------------------------------------------------------------------------*/



void mn2_mark( PARAM *par, int *status )

/* marks time positions on a trace
 * 1. param:  trace position number
 * 2. param:  time position (absolute or relative)
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	TRACE    *trc;                   /* trace pointer */
	float    time;                   /* time to be marked */
	char     timstr[TCC_TIMSTRLTH+1];/* time string */
	TIME     mtime;                  /* mark time */
	TIME     stime;                  /* start time of trace */
	char     label[BC_LINELTH+1];    /* label of mark */
	REAL     labpos;                 /* position of label (from 0.0 to 1.0) */
	REAL     labsize;                /* size of label (from 0.0 to 1.0) */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;

	trc = ml_get1trcpar( par, 1, tc, "   trace to be marked: ", status );
	if  (Severe(status))  return;

	if  (cp_qexist(par,"ABS"))  {
		cp_getstr( par, 2, tc, "   abs. time: ", TCC_TIMSTRLTH, timstr, status );
		if  (Severe(status))  return;
		tc_t2a( timstr, &mtime, status );
		if  (Severe(status))  return;
		db_gett( trc, ET_START, &stime, status );
		if  (Severe(status))  return;
		time = tc_adiff( &mtime, &stime ) + db_getr( trc, ER_TORIG, NULL );
	} else {   /* try relative time */
		cp_getfloat( par, 2, tc, "   time: ", &time, status );
		if  (Severe(status))  return;
		if  (cp_qexist(par,"TRCREL"))
			time += db_getr( trc, ER_TORIG, NULL );
	} /*endif*/

	if  (!sl_qualr(par,"SIZE",&labsize,status))
		labsize = 1.0;

	if  (sl_quals(par,"LABEL",BC_LINELTH,label,status))  {
		if  (!sl_qualr(par,"POS",&labpos,status))
			labpos = 1.0;
		cr_trcmark_label( trc, time, label, labpos, labsize );
	} else {
		cr_trcmark( trc, time, labsize );
	} /*endif*/

} /* end of mn2_mark */



/*-------------------------------------------------------------------------*/



void mn2_corr( PARAM *par, int *status )

/* computes crosscorrelation of two traces
 * 1. param:  trace number of correlation wavelet
 * 2. param:  wavelet window start
 * 3. param:  wavelet window end
 * 4. param:  2. correlation trace number
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	TRACE    *wave;         /* wavelet trace pointer */
	TRACE    *btrc;         /* 2. correlation trace pointer */
	TRACE    *new;          /* result trace */
	REAL     lowdw, hiwdw;  /* wavelet window in sec */
	long     losmp, hismp;  /* wavelet window in samples */
	long     trclth;        /* length of result trace */
	REAL     dt;            /* sample distance in sec */
	SAMPLE   *datptr;       /* pointer to sample data of result trace */

	/* executable code */

	if  (cp_pnexc(par,4,status))  return;

	wave = ml_get1trcpar( par, 1, tc, "   wavelet trace: ", status );
	if  (*status != SHE_NOERROR)  return;
	cp_getfloat( par, 2, tc, "   wavelet start: ", &lowdw, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getfloat( par, 3, tc, "   wavelet end: ", &hiwdw, status );
	if  (*status != SHE_NOERROR)  return;
	btrc = ml_get1trcpar( par, 4, tc, "   2. corr trace: ", status );
	if  (*status != SHE_NOERROR)  return;

	if  (cp_qexist(par,"M"))  {
		cr_trcmark( wave, lowdw, 1.0 );
		cr_trcmark( wave, hiwdw, 1.0 );
	} /*endif*/

	dt = db_getr( wave, ER_DELTA, NULL );
	if  (dt != db_getr(btrc,ER_DELTA,NULL))  {
		*status = SHE_DIFFSAMP;
		return;
	} /*endif*/

	losmp = dm_sgetsample( wave, lowdw, status );
	if  (*status != SHE_NOERROR)  return;
	hismp = dm_sgetsample( wave, hiwdw, status );
	if  (*status != SHE_NOERROR)  return;

	trclth = sc_restrclth( dt );

	db_newlist();
	new = db_create( status );
	if  (*status != SHE_NOERROR)  return;
	datptr = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
	if  (*status != SHE_NOERROR)  {db_delete(new); return;}

	sc_do_corr( db_getp(wave,EP_DATA,NULL), losmp, hismp,
		db_getp(btrc,EP_DATA,NULL), db_getl(btrc,EL_LENGTH,NULL),
		dt, datptr, status );
	if  (*status != SHE_NOERROR)  {
		sy_deallocmem( datptr );
		db_delete( new );
		return;
	} /*endif*/

	ml_cpyinfo( wave, new, status );
	if  (*status != SHE_NOERROR)  {
		sy_deallocmem( datptr ); db_delete( new );
		return;
	} /*endif*/
	ml_inittrc( new, datptr, trclth, dt );
	db_setr( new, ER_TORIG, 0.0, NULL );

} /* end of mn2_corr */



/*-------------------------------------------------------------------------*/



void mn2_corrl( PARAM *par, int *status )

/* sets correlation length
 * 1. param:  start of correlation window
 * 2. param:  end of correlation window
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	REAL     lowdw, hiwdw;   /* correlation window */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;

	cp_getfloat( par, 1, tc, "   lo bound: ", &lowdw, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getfloat( par, 2, tc, "   hi bound: ", &hiwdw, status );
	if  (*status != SHE_NOERROR)  return;

	sc_set_corrl( lowdw, hiwdw, status );

} /* end of mn2_corrl */



/*-------------------------------------------------------------------------*/



void mn2_mdir( PARAM *par, int *status )

/* computation of direction of polarisation
 * 1. param:  trace list (2 or 3 traces)
 * 2. param:  start of correlation window
 * 3. param:  end of correlation window
 * 4. param:  output var - 2-dim angle or azimuth (3-dim)
 * 5. param:  output var - angle of incidence (3-dim)
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];   /* trace list */
	int      listlth;              /* length of trace list */
	REAL     lowdw, hiwdw;         /* correlation window */
	long     start[3];             /* start samples of traces */
	long     trclth;               /* length of traces */
	int      i;                    /* counter */
	REAL     azim, inci;           /* result angles */
	int      display_res;          /* display results */
	char     symbol[BC_LINELTH+1]; /* symbol for result */
	char     resstr[cBcLineLth+1]; /* string for result angle(s) */
	int      inplev;               /* input level */
	REAL     eigval[3];            /* eigenvalues found */

	/* executable code */

	if  (cp_pnexc(par,5,status))  return;

	if  (cp_qexist(par,"PARENT"))  {
		inplev = -1;
	} else if  (cp_qexist(par,"INTERACTIVE"))  {
		inplev = shv_maininput;
	} else {
		inplev = ss_inplev();
	} /*endif*/

	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (*status != SHE_NOERROR)  return;
	if  ((listlth < 2) || (listlth > 3))  {
		*status = SHE_ILPAR;
		err_setcontext( " ## list length is " ); err_setcontext_l( listlth );
		return;
	} /*endif*/

	ui_shift( inplev );
	ml_windowpars( par, 2, tc, "   lo bound: ", "   hi bound: ",
		&lowdw, &hiwdw, status );
	ui_unshift();
	if  (Severe(status))  return;

	for  (i=0;i<listlth;i++)  {
		start[i] = dm_sgetsample( trc[i], lowdw, status );
		if  (*status != SHE_NOERROR)  return;
		if  (i == 0)  {
			trclth = dm_sgetsample( trc[i], hiwdw, status ) - start[0] + 1;
			if  (*status != SHE_NOERROR)  return;
		} else {
			if  ((dm_sgetsample(trc[i],hiwdw,status)-start[i]+1) != trclth)  {
				*status = SHE_ILSMP;
				return;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	if  (listlth == 2)  {
		sc_polar2( (SAMPLE *)db_getp(trc[0],EP_DATA,NULL)+start[0],
			(SAMPLE *)db_getp(trc[1],EP_DATA,NULL)+start[1], trclth, &azim,
			eigval );
		/* put eigenvalues to opinfo entry */
		sprintf( resstr, "%f %f", eigval[0], eigval[1] );
	} else {
		sc_polar3( (SAMPLE *)db_getp(trc[0],EP_DATA,NULL)+start[0],
			(SAMPLE *)db_getp(trc[1],EP_DATA,NULL)+start[1],
			(SAMPLE *)db_getp(trc[2],EP_DATA,NULL)+start[2],
			trclth, &azim, &inci, eigval, status );
		if  (*status != SHE_NOERROR)  return;
		sprintf( resstr, "%f %f %f", eigval[0], eigval[1], eigval[2] );
	} /*endif*/
	db_sets( trc[0], ES_OPINFO, resstr, status );
	if  (*status != SHE_NOERROR)  return;

	display_res = TRUE;
	if  (cp_pentered(par,4,status))  {
		display_res = FALSE;
		cp_getstr( par, 4, tc, "@@", BC_LINELTH, symbol, status );
		if  (*status != SHE_NOERROR)  return;
		sprintf( resstr, "%e", azim );
		sl_setsymbol( symbol, resstr, status );
		if  (*status != SHE_NOERROR)  return;
	} /*endif*/

	if  (cp_pentered(par,5,status))  {
		if  (listlth != 3)  {
			cp_pnexc(par,4,status);
			return;
		} /*endif*/
		display_res = FALSE;
		cp_getstr( par, 5, tc, "@@", BC_LINELTH, symbol, status );
		if  (*status != SHE_NOERROR)  return;
		sprintf( resstr, "%e", inci );
		sl_setsymbol( symbol, resstr, status );
		if  (*status != SHE_NOERROR)  return;
	} /*endif*/

	if  (display_res)  {
		if  (listlth == 2)  {
			sprintf( resstr, "   angle: %f\n", azim );
		} else {
			sprintf( resstr, "    azim: %f,  inci: %f\n", azim, inci );
		} /*endif*/
		gc_write( tc, resstr );
	} /*endif*/

} /* end of mn2_mdir */



/*-------------------------------------------------------------------------*/



void mn2_trcfct( PARAM *par, int *status )

/* trace functions
 * 1. param:  function name
 * 2. param:  ...
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH]; /* trace list */
	int      listlth;            /* list length */
	char     fct[BC_LINELTH+1];  /* function name */
	REAL     fac;                /* factor */
	SAMPLE   *x;                 /* moving pointer */
	long     lth;                /* length of trace */
	long     i, j;               /* sample counters */
	int      t;                  /* trace counter */
	SAMPLE   min, max;           /* new minimum & maximum */
	SAMPLE   av;                 /* average value */

	/* executable code */

	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getstr( par, 2, tc, "   function: ", BC_LINELTH, fct, status );
	if  (*status != SHE_NOERROR)  return;

	if  (strcmp(fct,"REMAV") == 0)  {
		for  (t=0;t<listlth;t++)
			sl_remav( db_getp(trc[t],EP_DATA,NULL),
				db_getl(trc[t],EL_LENGTH,NULL) );
	} else if  (strcmp(fct,"MUL") == 0)  {
		if  (cp_pnexc(par,3,status))  return;
		cp_getfloat( par, 3, tc, "   factor: ", &fac, status );
		if  (*status != SHE_NOERROR)  return;
		for  (t=0;t<listlth;t++)  {
			x = db_getp( trc[t], EP_DATA, NULL );
			lth = db_getl( trc[t], EL_LENGTH, NULL );
			for  (i=0;i<lth;i++)
				*x++ *= fac;
			sy_sharecpu();
		} /*endfor*/
	} else if  (strcmp(fct,"ADD") == 0)  {
		if  (cp_pnexc(par,3,status))  return;
		cp_getfloat( par, 3, tc, "   offset: ", &fac, status );
		if  (*status != SHE_NOERROR)  return;
		for  (t=0;t<listlth;t++)  {
			x = db_getp( trc[t], EP_DATA, NULL );
			lth = db_getl( trc[t], EL_LENGTH, NULL );
			for  (i=0;i<lth;i++)
				*x++ += fac;
			sy_sharecpu();
		} /*endfor*/
	} else if  (strcmp(fct,"SQUARE") == 0)  {
		if  (cp_pnexc(par,2,status))  return;
		for  (t=0;t<listlth;t++)  {
			x = db_getp( trc[t], EP_DATA, NULL );
			lth = db_getl( trc[t], EL_LENGTH, NULL );
			for  (i=0;i<lth;i++)  {
				*x *= *x;
				x++;
			} /*endfor*/
			sy_sharecpu();
		} /*endfor*/
	} else if  (strcmp(fct,"SSQUARE") == 0)  {
		if  (cp_pnexc(par,2,status))  return;
		for  (t=0;t<listlth;t++)  {
			x = db_getp( trc[t], EP_DATA, NULL );
			lth = db_getl( trc[t], EL_LENGTH, NULL );
			for  (i=0;i<lth;i++)  {
				*x *= (*x > 0) ? *x : -(*x);
				x++;
			} /*endfor*/
			sy_sharecpu();
		} /*endfor*/
	} else if  (strcmp(fct,"SQRT") == 0)  {
		if  (cp_pnexc(par,2,status))  return;
		for  (t=0;t<listlth;t++)  {
			x = db_getp( trc[t], EP_DATA, NULL );
			lth = db_getl( trc[t], EL_LENGTH, NULL );
			for  (i=0;i<lth;i++)  {
				*x = (*x > 0.) ? sqrt(*x) : -sqrt(-(*x));
				x++;
			} /*endfor*/
			sy_sharecpu();
		} /*endfor*/
	} else if  (strcmp(fct,"LN") == 0)  {
		if  (cp_pnexc(par,2,status))  return;
		for  (t=0;t<listlth;t++)  {
			x = db_getp( trc[t], EP_DATA, NULL );
			lth = db_getl( trc[t], EL_LENGTH, NULL );
			for  (i=0;i<lth;i++)  {
				if  (*x != 0.)
					*x = (*x > 0.) ? log(*x) : -log(-(*x));
				x++;
			} /*endfor*/
			sy_sharecpu();
		} /*endfor*/
	} else if  (strcmp(fct,"SPOWER") == 0)  {
		if  (cp_pnexc(par,3,status))  return;
		cp_getfloat( par, 3, tc, "   power: ", &fac, status );
		if  (*status != SHE_NOERROR)  return;
		for  (t=0;t<listlth;t++)  {
			x = db_getp( trc[t], EP_DATA, NULL );
			lth = db_getl( trc[t], EL_LENGTH, NULL );
			for  (i=0;i<lth;i++)  {
				*x = (*x > 0.) ? pow(*x,fac) : -pow(-(*x),fac);
				x++;
			} /*endfor*/
			sy_sharecpu();
		} /*endfor*/
	} else if  (strcmp(fct,"ABS") == 0)  {
		if  (cp_pnexc(par,2,status))  return;
		for  (t=0;t<listlth;t++)  {
			x = db_getp( trc[t], EP_DATA, NULL );
			lth = db_getl( trc[t], EL_LENGTH, NULL );
			for  (i=0;i<lth;i++)  {
				*x = Abs(*x);
				x++;
			} /*endfor*/
			sy_sharecpu();
		} /*endfor*/
	} else if  (strcmp(fct,"SIGN") == 0)  {
		if  (cp_pnexc(par,2,status))  return;
		for  (t=0;t<listlth;t++)  {
			x = db_getp( trc[t], EP_DATA, NULL );
			lth = db_getl( trc[t], EL_LENGTH, NULL );
			for  (i=0;i<lth;i++)  {
				*x = (*x > 0) ? 1.0 : -1.0;
				x++;
			} /*endfor*/
			sy_sharecpu();
		} /*endfor*/
	} else if  (strcmp(fct,"MEANREPL") == 0)  {
		if  (cp_pnexc(par,3,status))  return;
		cp_getfloat( par, 3, tc, "   value to replace: ", &fac, status );
		for  (t=0;t<listlth;t++)  {
			x = db_getp( trc[t], EP_DATA, NULL );
			lth = db_getl( trc[t], EL_LENGTH, NULL );
			/* get mean value */
			av = 0.0;
			j = lth;
			for  (i=0;i<lth;i++)  {
				if  (x[i] == fac)  {
					j--;
				} else {
					av += x[i];
				} /*endif*/
			} /*endfor*/
			if  (j > 0)  av /= (float)j;
			for  (i=0;i<lth;i++)
				if  (x[i] == fac)
					x[i] = av;
			sy_sharecpu();
		} /*endfor*/
	} else {
		*status = SHE_ILPAR;
		err_setcontext( " ## subfct " ); err_setcontext( fct );
		return;
	} /*endif*/

	for  (t=0;t<listlth;t++)  {
		x = db_getp( trc[t], EP_DATA, NULL );
		lth = db_getl( trc[t], EL_LENGTH, NULL );
		sl_findmax( x, lth, &min, &max );
		db_setr( trc[t], ER_MINVAL, min, status );
		if  (*status != SHE_NOERROR)  return;
		db_setr( trc[t], ER_MAXVAL, max, status );
		if  (*status != SHE_NOERROR)  return;
	} /*endfor*/

} /* end of mn2_trcfct */



/*-------------------------------------------------------------------------*/



void mn2_hide( PARAM *par, int *status )

/* hides traces (remove from display, keep in memeory)
 * 1. param:  trace list
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH]; /* trace list */
	int      listlth;            /* list length */
	int      t;                  /* trace counter */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (*status != SHE_NOERROR)  return;

	for  (t=0; t<listlth; db_delist(gc,trc[t++])) {}

} /* end of mn2_hide */



/*-------------------------------------------------------------------------*/



void mn2_display( PARAM *par, int *status )

/* displays traces at position
 * 1. param:  trace list
 * 2. param:  start position
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH]; /* trace list */
	int      listlth;            /* list length */
	int      pos;                /* display position */
	int      t;                  /* trace counter */
	STATUS   locstat=SHE_NOERROR;/* local status */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;

	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	cp_getint( par, 2, tc, "   pos: ", &pos, &locstat );
	if  (locstat != SHE_NOERROR)  pos = 1;

	for  (t=0; t<listlth; t++)  {
		if  (db_islisted(gc,trc[t]))  db_delist( gc, trc[t] );
		db_enlist( trc[t], pos++ );
	} /*endfor*/

} /* end of mn2_display */



/*-------------------------------------------------------------------------*/



void mn2_extract( PARAM *par, int *status )

/* extracts info from q-file
 * 1. param:  q-file name
 * 2. param:  record number
 * 3. param:  info entry description string
 * 4. param:  output variable
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	char     qfile[BC_LINELTH+1];   /* q-file name */
	int      recno;                 /* record number */
	char     symbol[BC_LINELTH+1];  /* name of output variable */
	unsigned infent;                /* info entry number */
	long     l_info;                /* long info */
	int      i_info;                /* integer info */
	float    r_info;                /* float info */
	char     s_info[BC_LINELTH+1];  /* info entry string / str info */
	char     *sptr;                 /* string pointer */

	/* executable code */

	if  (cp_pnexc(par,4,status))  return;

	cp_getstr( par, 1, tc, "   q-file: ", BC_LINELTH, qfile, status );
	if  (Severe(status))  return;
	cp_getint( par, 2, tc, "   record: ", &recno, status );
	if  (Severe(status))  return;
	cp_getstr( par, 3, tc, "   info entry: ", BC_LINELTH, s_info, status );
	if  (Severe(status))  return;
	cp_getstr( par, 4, tc, "   output var: ", BC_LINELTH, symbol, status );
	if  (Severe(status))  return;

	if  (strcmp(s_info,"NO_OF_RECS") == 0)  {
		qf_size( qfile, &l_info, &i_info, &recno, status );
		if  (Severe(status))  return;
		sprintf( s_info, "%d", i_info );
		sl_setsymbol( symbol, s_info, status );
		return;
	} /*endif*/

	db_ident( s_info, &infent, status );
	if  (Severe(status))  return;
	qf_read( qfile, recno, FALSE, NULL, status );
	if  (Severe(status))  return;

	switch  (infent & E_TYPMASK)  {
	case EL_TYPE:
		l_info = qf_getl( qi_cnvlidx(infent,status), status );
		if  (Severe(status))  return;
		sprintf( s_info, "%ld", l_info );
		break;
	case EI_TYPE:
		i_info = qf_geti( qi_cnviidx(infent,status), status );
		if  (Severe(status))  return;
		sprintf( s_info, "%d", i_info );
		break;
	case EB_TYPE:
		i_info = qf_geti( qi_cnvbidx(infent,status), status );
		if  (Severe(status))  return;
		sprintf( s_info, "%d", i_info );
		break;
	case ER_TYPE:
		r_info = qf_getr( qi_cnvridx(infent,status), status );
		if  (Severe(status))  return;
		sprintf( s_info, "%e", r_info );
		break;
	case ES_TYPE:
		sptr = qf_gets(qi_cnvsidx(infent,status), status );
		if  (Severe(status))  return;
		sl_strcpy( s_info, sptr, BC_LINELTH, status );
		if  (Severe(status))  return;
		break;
	case EC_TYPE:
		*s_info = qf_getc( qi_cnvcidx(infent,status), status );
		if  (Severe(status))  return;
		s_info[1] = '\0';
		break;
	case ET_TYPE:
		sptr = qf_gets(qi_cnvtidx(infent,status), status );
		if  (Severe(status))  return;
		sl_strcpy( s_info, sptr, BC_LINELTH, status );
		if  (Severe(status))  return;
		break;
	case EF_TYPE:
		i_info = qf_geti( qi_cnvfidx(infent,status), status );
		if  (Severe(status))  return;
		sprintf( s_info, "%d", i_info );
		break;
	default:
		*status = SHE_ILTYPE;
		return;
	} /*endswitch */

	sl_setsymbol( symbol, s_info, status );

} /* end of mn2_extract */



/*-------------------------------------------------------------------------*/



void mn2_pick( PARAM *par, int *status )

/* Picks onset on trace
 * 1. param:  trace number
 * 2. param:  start of time window
 * 3. param:  end of time window
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	int      inplev;            /* input level */
	TRACE    *trc;              /* trace pointer */
	BOOLEAN  fulltrace;         /* use full trace */
	REAL     lowdw, hiwdw;      /* time window on trace */
	REAL     dt;                /* sample distance */
	long     loidx, hiidx;      /* time window in samples */
	long     trclth;            /* number of selected samples */
	TRACE    *sftrc, *cftrc;    /* trace pointers to sf and cf functions */
	SAMPLE   *src;              /* source trace data */
	SAMPLE   *wrk;              /* modfied copy of original sample array */
	SAMPLE   *sf, *cf;          /* data pointers of sftrc and cftrc */
	int      smp;               /* sample counter */
	int      i;                 /* counter */
	REAL     sum_sq;            /* square sum */
	REAL     sum_dersq;         /* sqaure sum of derivative */
	REAL     deriv;             /* current derivative, then squared */
	REAL     val;               /* current value, then squared */
	REAL     sf_mean;           /* mean value of sf */
	REAL     sf_sigma;          /* standard deviation of sf */
	REAL     sum_sigma;         /* sum used for sigma */
	REAL     sum_sigma_add;     /* additional for sum_sigma */
	int      sum_sigma_cnt;     /* counter for sigma computation */
	REAL     thresh;            /* threshold value */
	REAL     duration;          /* duration of ampl over threshold */
	int      durasmp;           /* duration in samples */
	REAL     breaklth;          /* maximum break length in duration */
	int      breaksmp;          /* break length in samples */
	int      pickindex;         /* index of picked amplitude or -1 */
	int      breakindex;        /* start of break */
	TIME     onset;             /* onset time (binary) */
	char     timestr[cBcLineLth+1]; /* onset time */
	char     symbol[cBcLineLth+1];  /* output variable */
	REAL     orig_min, orig_max; /* min, max value of original trace */

	/* executable code */

	if  (cp_pnexc(par,4,status))  return;

	if  (cp_qexist(par,"PARENT"))  {
		inplev = -1;
	} else if  (cp_qexist(par,"INTERACTIVE"))  {
		inplev = shv_maininput;
	} else {
		inplev = ss_inplev();
	} /*endif*/

	if  (!sl_qualr(par,"DURATION",&duration,status))
		duration = 1.0;
	if  (SySevere(status))  return;
	if  (!sl_qualr(par,"THRESHOLD",&thresh,status))
		thresh = 10.0;
	if  (SySevere(status))  return;
	if  (!sl_qualr(par,"BREAKLTH",&breaklth,status))
		breaklth = 0.0;
	if  (SySevere(status))  return;

	trc = ml_get1trcpar( par, 1, tc, "   wavelet trace: ", status );
	if  (SySevere(status))  return;

	fulltrace = !cp_pentered( par, 3, status );
	if  (*status != SHE_NOERROR)  return;

	if  (!fulltrace)  {
		ui_shift( inplev );
		ml_windowpars( par, 2, tc, "   lo bound: ", "   hi bound: ",
			&lowdw, &hiwdw, status );
		ui_unshift();
		if  (SySevere(status))  return;
	} /*endif*/

	/* get trace window */
	dt = db_getr( trc, ER_DELTA, NULL );
	if  (fulltrace)  {
		loidx = 0;
		hiidx = db_getl( trc, EL_LENGTH, NULL ) - 1;
		lowdw = db_getr(trc,ER_TORIG,NULL);
	} else {
		loidx = dm_getsample( trc, lowdw, TRUE );
		hiidx = dm_getsample( trc, hiwdw, TRUE );
	} /*endif*/
	trclth = hiidx - loidx + 1;
	src = (SAMPLE *)db_getp(trc,EP_DATA,NULL) + loidx;
	durasmp = Nint( duration / dt );
	breaksmp = Nint( breaklth / dt );

	/* copy original data, demean and normalize */
	wrk = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
	if  (SySevere(status))  return;
	sf_mean = 0.0;    /* sf_mean used later again */
	orig_min = orig_max = src[0];
	for  (smp=0; smp<trclth; smp++)  {
		wrk[smp] = src[smp];
		sf_mean = smp > 0
			? (sf_mean + src[smp]/(REAL)(smp))*((REAL)(smp)/(REAL)(smp+1))
			: src[0];
		if  (wrk[smp] > orig_max)  orig_max = wrk[smp];
		if  (wrk[smp] < orig_min)  orig_min = wrk[smp];
	} /*endfor*/
	orig_min -= sf_mean;
	orig_max -= sf_mean;
	orig_min = Abs( orig_min );
	orig_max = Abs( orig_max );
	if  (orig_min > orig_max)  orig_max = orig_min;
	for  (smp=0; smp<trclth; smp++)
		wrk[smp] = (wrk[smp]-sf_mean) / orig_max;

	/* create new traces */
	sftrc = db_create( status );
	if  (SySevere(status))  return;
	sf = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
	if  (SySevere(status))  {
		sy_deallocmem( wrk );
		db_delete( sftrc );
		return;
	} /*endif*/
	cftrc = db_create( status );
	if  (SySevere(status))  return;
	cf = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
	if  (SySevere(status))  {
		sy_deallocmem( wrk );
		sy_deallocmem( sf );
		db_delete( sftrc );
		db_delete( cftrc );
		return;
	} /*endif*/

	/* compute pick traces */
	/* initialize */
	sum_sq = sum_dersq = 0.0;
	sf_mean = 0.0;
	sum_sigma = 0.0;
	sum_sigma_cnt = 0;
	pickindex = breakindex = -1;
	/* loop all samples */
	for  (smp=0; smp<trclth; smp++)  {
		/* compute squared enevelope */
		val = wrk[smp]*wrk[smp];
		deriv = smp > 0 ? (wrk[smp]-wrk[smp-1])/dt : 0.0;
		deriv *= deriv;
		sum_sq += val;
		sum_dersq += deriv;
		if  (sum_dersq > SHC_EPSILON)  {
			sf[smp] = val + deriv*sum_sq/sum_dersq;
			sf[smp] *= sf[smp];
		} else {
			sf[smp] = 0.0;
		} /*endif*/
		/* mean value of sq. envelope */
		sf_mean = smp > 0
			? (sf_mean + sf[smp]/(REAL)(smp))*((REAL)(smp)/(REAL)(smp+1))
			: sf[0];
		/* standard deviation (with 'old' mean values) */
		sum_sigma_cnt++;
		sum_sigma_add = sf[smp] - sf_mean;
		sum_sigma_add *= sum_sigma_add;
		sum_sigma += sum_sigma_add;
		sf_sigma = sqrt( sum_sigma / (REAL)sum_sigma_cnt );
		if  (sf_sigma > SHC_EPSILON)  {
			cf[smp] = (sf[smp] - sf_mean) / sf_sigma;
		} else {
			cf[smp] = smp > 0 ? cf[smp-1] : 0.0;
		} /*endif*/
		/* remove this sample from sigma if over 2*threshold */
		if  (cf[smp] > 2.0*thresh)  {
			sum_sigma -= sum_sigma_add;
			sum_sigma_cnt--;
		} /*endif*/
		/* if over/under threshold check for pick to set/clear */
		if  (cf[smp] > thresh)  {
			/* store sample index if not already done, clear break flag */
			if  (pickindex < 0)  pickindex = smp;
			breakindex = -1;
		} /*endif*/
		if  (cf[smp] < thresh && pickindex != -1)  {
			/* only serious if shorter than duration */
			if  (smp-pickindex < durasmp)  {
				if  (breaksmp == 0)  {
					/* no breaks allowed */
					pickindex = -1;
				} else if  (breakindex == -1)  {
					/* new break found, store break index */
					breakindex = smp;
				} else if  (smp-breakindex > breaksmp)  {
					/* break longer than allowed, clear pick- and break-flag */
					pickindex = -1;
					breakindex = -1;
				} /*endif*/
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	sy_deallocmem( wrk );

	/* set trace info */
	ml_cpyinfo( trc, sftrc, status );
	if  (! SySevere(status))
		ml_cpyinfo( trc, cftrc, status );
	if  (SySevere(status))  {
		sy_deallocmem( sf ); db_delete( sftrc );
		sy_deallocmem( cf ); db_delete( cftrc );
		return;
	} /*endif*/
	ml_inittrc( sftrc, sf, trclth, dt );
	if  (lowdw > db_getr(trc,ER_TORIG,NULL))
		db_setr( sftrc, ER_TORIG, lowdw, NULL );
	db_setf( sftrc, EF_MODIF, TRUE, NULL );
	ml_newstarttime( trc, sftrc, lowdw, status );
	if  (Severe(status))  {
		sy_deallocmem( sf ); db_delist( gc, sftrc ); db_delete( sftrc );
		return;
	} /*endif*/
	ml_inittrc( cftrc, cf, trclth, dt );
	if  (lowdw > db_getr(trc,ER_TORIG,NULL))
		db_setr( cftrc, ER_TORIG, lowdw, NULL );
	db_setf( cftrc, EF_MODIF, TRUE, NULL );
	ml_newstarttime( trc, cftrc, lowdw, status );
	if  (Severe(status))  {
		sy_deallocmem( cf ); db_delist( gc, cftrc ); db_delete( cftrc );
		return;
	} /*endif*/

	if  (pickindex >= 0)  {
		db_gett( cftrc, ET_START, &onset, status );
		if  (SySevere(status))  return;
		tc_aadd( &onset, (REAL)pickindex*dt, &onset );
		tc_a2t( &onset, timestr, status );
		if  (SySevere(status))  return;
	} else {
		*timestr = '\0';
	} /*endif*/

	if  (cp_pentered(par,4,status))  {
		cp_getstr( par, 4, tc, "@@", cBcLineLth, symbol, status );
		if  (SySevere(status))  return;
		sl_setsymbol( symbol, timestr, status );
		if  (SySevere(status))  return;
	} /*endif*/
	db_sets( trc, ES_OPINFO, timestr, status );
	if  (SySevere(status))  return;

	/* delete sf and cf if no KEEP qualifier specified */
	if  (!cp_qexist(par,"KEEP"))  {
		sy_deallocmem( sf ); db_delist( gc, sftrc ); db_delete( sftrc );
		sy_deallocmem( cf ); db_delist( gc, cftrc ); db_delete( cftrc );
	} /*endif*/

	if  (pickindex >= 0)
		cr_trcmark( trc,
			(REAL)(loidx+pickindex)*dt+db_getr(trc,ER_TORIG,NULL), 1.0 );

} /* end of mn2_pick */



/*-------------------------------------------------------------------------*/

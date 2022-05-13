
/* file SHMENU1.C
 *      =========
 *
 * version 15, 12-Jan-2007
 *
 * menu routines of seismhandler
 * K. Stammler, 6-MAY-1990
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
#include "shconst.h"
#include "shvars.h"
#include "cpusrdef.h"
#include "uiusrdef.h"
#include BC_GCUSRDEF
#include "qfusrdef.h"
#include "qiusrdef.h"
#include "ssusrdef.h"
#include "erusrdef.h"
#include "tcusrdef.h"
#include "infoidx.h"
#include "fctxml.h"
#include "fctxsl.h"
#include "fctxdm.h"
#include "fctxmt.h"
#include "fctxcr.h"
#include "sherrors.h"
#include "qferrors.h"


/*------------------------------------------------------------------------*/



void mn1_zoom( PARAM *par, int *status )

/* zoom amplitudes */

/* parameters of routine */
/* PARAM    *par;           input; name & parameters */
/* int      *status;        output; return status */

{
	/* local variables */
	void     *trclist[SHC_ILISTLTH];     /* trace list */
	int      listlth;                    /* length of list */
	int      i;                          /* counter */
	REAL     zoom;                       /* zoom factor */
	REAL     zabs;                       /* absolute zoom factor */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trclist, &listlth, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getfloat( par, 2, tc, "   zoom factor: ", &zoom, status );
	if  (*status != SHE_NOERROR)  return;

	if  (cp_qexist(par,"REL"))  {
		for  (i=0;i<listlth;i++)  {
			zabs = zoom * db_getr( trclist[i], ER_ZOOM, status );
			if  (*status != SHE_NOERROR)  return;
			db_setr( trclist[i], ER_ZOOM, zabs, status );
			if  (*status != SHE_NOERROR)  return;
		} /*endfor*/
	} else {
		for  (i=0;i<listlth;i++)  {
			db_setr( trclist[i], ER_ZOOM, zoom, status );
			if  (*status != SHE_NOERROR)  return;
		} /*endfor*/
	} /*endif*/

	if  (cp_qexist(par,"DEFAULT"))
		ml_defaults( ER_ZOOM, &zoom, status );

} /* end of mn1_zoom */




/*-------------------------------------------------------------------------*/



void mn1_set( PARAM *par, int *status )

/* changes info values of traces */

/* parameters of routine */
/* PARAM    *par;           input; name & parameters */
/* int      *status;        output; return status */

{
	/* local variables */
	void     *trclist[SHC_ILISTLTH]; /* trace list */
	int      recno[SHC_ILISTLTH];    /* list of record numbers */
	int      listlth;                /* length of list */
	char     qfile[BC_LINELTH+1];    /* name of q-file */
	char     str[BC_LINELTH+1];      /* info string */
	unsigned infent;                 /* info entry */
	unsigned qfentry;                /* q-file entry */
	int      i;                      /* counter */
	BOOLEAN  tofile;                 /* write info to file ? */
	BOOLEAN  samefile;               /* all traces on same file ? */
	BOOLEAN  wmodif;                 /* allow to write on modified trace */
	void     *infptr;                /* pointer to info */
	long     l_info;
	int      i_info;
	BYTE     b_info;
	float    r_info;
	TIME     t_info;

	/* executable code */

	if  (cp_pnexc(par,3,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trclist, &listlth, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getstr( par, 2, tc, "   entry name: ", BC_LINELTH, str, status );
	if  (*status != SHE_NOERROR)  return;

	db_ident( str, &infent, status );
	if  (*status != SHE_NOERROR)  return;

	/* check if entry is read-only */
	if  ((infent & E_RDONLY) && !cp_qexist(par,"PRIV"))  {
		*status = SHE_ERDONLY;
		err_setcontext( " ## entry " ); err_setcontext( str );
		return;
	} /*endif*/

	tofile = cp_qexist( par, "FILE" );
	wmodif = cp_qexist( par, "MOD" );
	if  (tofile)  {
		samefile = TRUE;
		for  (i=0;i<listlth;i++)  {
			if  (db_getf(trclist[i],EF_MODIF,NULL) && !wmodif)  {
				*status = SHE_WMODIF;
				return;
			} else if  (!db_getf(trclist[i],EF_FROMQ,NULL))  {
				*status = SHE_NOQFILE;
				return;
			} /*endif*/
			if  (i == 0)  {
				db_gets( trclist[i], ES_FILE, BC_LINELTH, qfile, status );
			} else {
				db_gets( trclist[i], ES_FILE, BC_LINELTH, str, status );
				if  (strcmp(str,qfile) != 0)  samefile = FALSE;
			} /*endif*/
			if  (*status != SHE_NOERROR)  return;
			recno[i] = db_geti( trclist[i], EI_RECNO, status );
			if  (*status != SHE_NOERROR)  return;
		} /*endfor*/
	} /*endif*/

	switch  (infent & E_TYPMASK)  {
		case EL_TYPE:
			cp_getlong( par, 3, tc, "   long info: ", &l_info, status );
			if  (*status != SHE_NOERROR)  return;
			if  (tofile)  {
				infptr = &l_info;
				qfentry = qi_cnvlidx( infent, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
			for  (i=0;i<listlth;i++)  {
				db_setl( trclist[i], infent, l_info, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endfor*/
			if  (cp_qexist(par,"DEFAULT"))
				ml_defaults( infent, &l_info, status );
			break;
		case EI_TYPE:
			cp_getint( par, 3, tc, "   int info: ", &i_info, status );
			if  (*status != SHE_NOERROR)  return;
			if  (tofile)  {
				infptr = &i_info;
				qfentry = qi_cnviidx( infent, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
			for  (i=0;i<listlth;i++)  {
				db_seti( trclist[i], infent, i_info, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endfor*/
			if  (cp_qexist(par,"DEFAULT"))
				ml_defaults( infent, &i_info, status );
			break;
		case EB_TYPE:
			cp_getint( par, 3, tc, "   byte info: ", &i_info, status );
			b_info = (BYTE)i_info;
			if  (*status != SHE_NOERROR)  return;
			if  (tofile)  {
				infptr = &b_info;
				qfentry = qi_cnvbidx( infent, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
			for  (i=0;i<listlth;i++)  {
				db_setb( trclist[i], infent, b_info, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endfor*/
			break;
		case ER_TYPE:
			cp_getfloat( par, 3, tc, "   real info: ", &r_info, status );
			if  (*status != SHE_NOERROR)  return;
			if  (tofile)  {
				infptr = &r_info;
				qfentry = qi_cnvridx( infent, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
			for  (i=0;i<listlth;i++)  {
				db_setr( trclist[i], infent, r_info, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endfor*/
			if  (cp_qexist(par,"DEFAULT"))
				ml_defaults( infent, &r_info, status );
			break;
		case ES_TYPE:
			cp_getstr( par, 3, tc, "   string info: ", BC_LINELTH, str, status );
			if  (*status != SHE_NOERROR)  return;
			if  (tofile)  {
				infptr = str;
				qfentry = qi_cnvsidx( infent, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
			for  (i=0;i<listlth;i++)  {
				db_sets( trclist[i], infent, str, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endfor*/
			if  (infent == ES_STATION)
				for  (i=0; i<listlth; i++)  {
					db_setp( trclist[i], EP_STAT, NULL, status );
					if  (Severe(status))  return;
				} /*endfor*/
			break;
		case EC_TYPE:
			cp_getstr( par, 3, tc, "   char info: ", 1, str, status );
			if  (*status != SHE_NOERROR)  return;
			if  (tofile)  {
				infptr = str;
				qfentry = qi_cnvcidx( infent, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
			for  (i=0;i<listlth;i++)  {
				db_setc( trclist[i], infent, *str, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endfor*/
			break;
		case ET_TYPE:
			cp_getstr( par, 3, tc, "   time info: ", BC_LINELTH, str, status );
			if  (*status != SHE_NOERROR)  return;
			tc_t2a( str, &t_info, status );
			if  (*status != SHE_NOERROR)  return;
			if  (tofile)  {
				infptr = str;
				qfentry = qi_cnvtidx( infent, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
			for  (i=0;i<listlth;i++)  {
				db_sett( trclist[i], infent, &t_info, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endfor*/
			break;
		case EF_TYPE:
			cp_getstr( par, 3, tc, "   flag info: ", 1, str, status );
			if  (*status != SHE_NOERROR)  return;
			if  (tofile)  {
				infptr = str;
				qfentry = qi_cnvfidx( infent, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
			for  (i=0;i<listlth;i++)  {
				db_setf( trclist[i], infent, *str=='Y', status );
				if  (*status != SHE_NOERROR)  return;
			} /*endfor*/
			break;
		default:
			*status = SHE_UKTYPE;
			return;
	} /*endswitch*/

	if  (tofile)  {
		if  (samefile)  {
			qf_change( qfile, recno, listlth, qfentry, infptr, 0, status );
			if  (*status == QFE_TRCOVFL)  {
				*status = SHE_NOERROR;
				qf_change( qfile, recno, listlth, qfentry, infptr, 1, status );
			} /*endif*/
		} else {
			for  (i=0;i<listlth;i++)  {
				db_gets( trclist[i], ES_FILE, BC_LINELTH, qfile, status );
				i_info = recno[i];
				qf_change( qfile, &i_info, 1, qfentry, infptr, 0, status );
				if  (*status == QFE_TRCOVFL)  {
					*status = SHE_NOERROR;
					qf_change( qfile, &i_info, 1, qfentry, infptr, 1, status );
				} /*endif*/
				if  (Severe(status))  return;
			} /*endfor*/
		} /*endif*/
	} /*endif*/

} /* end of mn1_set */



/*-------------------------------------------------------------------------*/



void mn1_stw( PARAM *par, int *status )

/* sets time window in display */

/* parameters of routine */
/* PARAM    *par;           input; name & parameters */
/* int      *status;        output; return status */

{
	/* local variables */
	float      lo_t, hi_t;      /* time window */
	int        inplev;          /* input level */

	/* executable code */

        if  (cp_qexist(par,"PARENT"))  {
                inplev = -1;
        } else if  (cp_qexist(par,"INTERACTIVE"))  {
                inplev = shv_maininput;
        } else {
                inplev = ss_inplev();
        } /*endif*/

	if  (cp_pnum(par) == 0)  {
		ui_shift( inplev );
		cr_window( MM_PTMARK, &lo_t, &hi_t, status );
		ui_unshift();
		if  (*status != SHE_NOERROR)  return;
	} else {
		if  (cp_pnexc(par,2,status))  return;
		cp_getfloat( par, 1, tc, "   lower bound: ", &lo_t, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 2, tc, "   upper bound: ", &hi_t, status );
		if  (*status != SHE_NOERROR)  return;
	} /*endif*/
	dm_timewdw( lo_t, hi_t, status );

} /* end of mn1_stw */



/*-------------------------------------------------------------------------*/



void mn1_create( PARAM *par, int *status )

/* creates a synthetic trace */

/* parameters of routine */
/* PARAM    *par;           input; name & parameters */
/* int      *status;        output; return status */

/* 1. par:  type of trace */
/* 2. par:  sample distance */
/* 3. par:  length of trace in seconds */
/* 4. par:  max. amplitude */
/* 5. par:  ... */

{
	/* local variables */
	char     type[BC_LINELTH+1];  /* type of trace */
	float    dt;                  /* sample distance */
	float    lth_sec;             /* length of trace in seconds */
	float    ampl;                /* maximum amplitude */
	MEMBLC   *blcptr;             /* pointer to trace descriptor */
	float    *datptr;             /* pointer to data */
	float    pos;                 /* position */
	float    rpar[2];             /* float parameters */
	int      ipar;                /* integer parameter */
	long     lth_smp;             /* length of trace in samples */
	TIME     start;               /* start time */

	/* executable code */

	cp_getstr( par, 1, tc, "   type: ", BC_LINELTH, type, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getfloat( par, 2, tc, "   sample distance: ", &dt, status );
	if  (*status != SHE_NOERROR)  return;
	if  (dt <= 0.0)  {*status = SHE_ILPAR; return;}
	cp_getfloat( par, 3, tc, "   length in sec: ", &lth_sec, status );
	if  (*status != SHE_NOERROR)  return;
	if  (lth_sec <= 0.0)  {*status = SHE_ILPAR; return;}
	cp_getfloat( par, 4, tc, "   amplitude: ", &ampl, status );
	if  (*status != SHE_NOERROR)  return;

	if  (strcmp(type,"SPIKE") == 0)  {
		cp_getfloat( par, 5, tc, "   position: ", &pos, status );
		if  (*status != SHE_NOERROR)  return;
	} else if  (strcmp(type,"GAUSS") == 0)  {
		cp_getfloat( par, 5, tc, "   position: ", &pos, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 6, tc, "   width: ", rpar, status );
		if  (*status != SHE_NOERROR)  return;
	} else if  (strcmp(type,"EXP") == 0)  {
		cp_getfloat( par, 5, tc, "   position: ", &pos, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 6, tc, "   width: ", rpar, status );
		if  (*status != SHE_NOERROR)  return;
		if  (*rpar <= 0.)  {*status = SHE_ILPAR; return;}
	} else if  (strcmp(type,"SHARP") == 0)  {
		cp_getfloat( par, 5, tc, "   position: ", &pos, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 6, tc, "   decay: ", rpar, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 7, tc, "   sine freq.: ", rpar+1, status );
		if  (*status != SHE_NOERROR)  return;
	} else if  (strcmp(type,"KUEPPER") == 0)  {
		cp_getfloat( par, 5, tc, "   position: ", &pos, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 6, tc, "   width: ", rpar, status );
		if  (*status != SHE_NOERROR)  return;
		if  (*rpar <= 0.)  {*status = SHE_ILPAR; return;}
		cp_getint( par, 7, tc, "   order: ", &ipar, status );
		if  (*status != SHE_NOERROR)  return;
	} else if  (strcmp(type,"SIN") == 0)  {
		cp_getfloat( par, 5, tc, "   frequency (Hz): ", &pos, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 6, tc, "   phase (deg): ", rpar, status );
		if  (*status != SHE_NOERROR)  return;
	} else if  (strcmp(type,"RANDOM") == 0)  {
	} else {
		*status = SHE_UKCREAT;
		err_setcontext( " ## key " ); err_setcontext( type );
		return;
	} /*endif*/

	db_newlist();

	blcptr = db_create( status );
	if  (*status != SHE_NOERROR)  return;

	lth_smp = Nlong(lth_sec/dt);
	if  (lth_smp <= 0L)  {*status = SHE_ILPAR; db_delete(blcptr); return;}

	datptr = (float *)sy_allocmem( lth_smp, (int)sizeof(float), status );
	if  (*status != SHE_NOERROR)  {db_delete(blcptr); return;}

	if  (strcmp(type,"SPIKE") == 0)  {
		mt_cspike( datptr, lth_smp, dt, pos, ampl );
	} else if  (strcmp(type,"GAUSS") == 0)  {
		mt_cgauss( datptr, lth_smp, dt, pos, ampl, rpar[0] );
	} else if  (strcmp(type,"EXP") == 0)  {
		mt_cexp( datptr, lth_smp, dt, pos, ampl, rpar[0] );
	} else if  (strcmp(type,"SHARP") == 0)  {
		mt_csharp( datptr, lth_smp, dt, pos, ampl, rpar[0], rpar[1] );
	} else if  (strcmp(type,"KUEPPER") == 0)  {
		mt_ckuepper( datptr, lth_smp, dt, pos, ampl, ipar, rpar[0] );
	} else if  (strcmp(type,"RANDOM") == 0)  {
		mt_crandom( datptr, lth_smp, ampl );
	} else if  (strcmp(type,"SIN") == 0)  {
		mt_csin( datptr, lth_smp, dt, pos, ampl, rpar[0] );
	} /*endif*/

	ml_inittrc( blcptr, datptr, lth_smp, dt );

	tc_t2a( SHC_TIMEDEFAULT, &start, status );
	if  (*status != SHE_NOERROR)  return;

	db_setr( blcptr, ER_TORIG, 0.0, NULL );
	db_sets( blcptr, ES_FILE, "", NULL );
	db_sett( blcptr, ET_START, &start, NULL );
	db_setf( blcptr, EF_MODIF, FALSE, NULL );
	db_setf( blcptr, EF_FROMQ, FALSE, NULL );

} /* end of mn1_create */



/*-------------------------------------------------------------------------*/



void mn1_del( PARAM *par, int *status )

/* deletes traces */

/* parameters of routine */
/* PARAM    *par;           input; name & parameters */
/* int      *status;        output; return status */

{
	/* local variables */
	void     *trclist[SHC_ILISTLTH];   /* trace list */
	int      listlth;                  /* length of list */
	int      i;                        /* counter */
	char     liststr[BC_LINELTH+1];    /* list string */
	int      wdw;                      /* window number */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trclist, &listlth, status );
	if  (Severe(status)) return;
	if  (cp_pentered(par,1,status))  {
		cp_getstr( par, 1, tc, "@@", BC_LINELTH, liststr, status );
		wdw = ml_listwdw( liststr, status );
		if  (Severe(status)) return;
	} else {
		wdw = gc;
	} /*endif*/

	for  (i=0;i<listlth;i++)  {
		sy_deallocmem( db_getp(trclist[i],EP_DATA,NULL) );
		if  (db_getp(trclist[i],EP_USR1,NULL) != NULL)
			sy_deallocmem( db_getp(trclist[i],EP_USR1,NULL) );
		if  (db_getp(trclist[i],EP_USR2,NULL) != NULL)
			sy_deallocmem( db_getp(trclist[i],EP_USR2,NULL) );
		db_delist( wdw, trclist[i] );
		db_delete( trclist[i] );
	} /*endfor*/

} /* end of mn1_del */



/*-------------------------------------------------------------------------*/



void mn1_trctxtp( PARAM *par, int *status )

/* sets position of trace info text */

/* parameters of routine */
/* PARAM    *par;           input; name & parameters */
/* int      *status;        output; return status */

/* 1. param:  x-position factor, units of display width */
/* 2. param:  y-position factor, units of display height */

{
	/* local variables */
	float    x, y;     /* position factors */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;
	cp_getfloat( par, 1, tc, "   x: ", &x, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getfloat( par, 2, tc, "   y: ", &y, status );
	if  (*status != SHE_NOERROR)  return;
	dm_infpos( x, y );

} /* end of mn1_trctxtp */



/*-------------------------------------------------------------------------*/



void mn1_copy( PARAM *par, int *status )

/* copies one or more traces
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  trace list
 * 2. param:  wdw-lo
 * 3. param:  wdw-hi
 */
{
	/* local variables */
	TRACE    *trclist[SHC_ILISTLTH];   /* trace list */
	int      listlth;                  /* length of list */
	REAL     lowdw, hiwdw;             /* trace window in sec */
	long     loidx, lth_smp;           /* trace window in samples */
	int      i;                        /* counter */
	TRACE    *new;                     /* pointer to new trace */
	SAMPLE   *datptr, *dstptr;         /* pointer to data array */
	SAMPLE   *srcptr;                  /* pointer to source data */
	long     s;                        /* sample counter */

	/* executable code */

	if  (cp_pnexc(par,3,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trclist, &listlth, status );
	if  (Severe(status))  return;
	if  (cp_pentered(par,2,status))  {
		ml_windowpars( par, 2, tc, "   lo-bound: ", "   hi-bound: ",
			&lowdw, &hiwdw, status );
	} else {
		lowdw = hiwdw = 0.;
	} /*endif*/
	if  (Severe(status))  return;
	db_newlist();

	for  (i=0;i<listlth;i++)  {

		new = db_create( status );
		if  (Severe(status))  return;
		if  (lowdw == hiwdw)  {
			loidx = 0;
			lth_smp = db_getl( trclist[i], EL_LENGTH, NULL );
		} else {
			loidx = dm_getsample( trclist[i], lowdw, TRUE );
			lth_smp = dm_getsample( trclist[i], hiwdw, TRUE ) - loidx + 1;
			if  (lth_smp <= 0)  {
				db_delete( new );
				*status = SHE_ZWDW;
				return;
			} /*endif*/
		} /*endif*/
		datptr = (SAMPLE *)sy_allocmem( lth_smp, (int)sizeof(SAMPLE), status );
		if  (Severe(status))  {db_delete(new); return;}

		/* copy data */
		srcptr = (SAMPLE *)db_getp( trclist[i], EP_DATA, NULL ) + loidx;
		dstptr = datptr;
		for  (s=0;s<lth_smp;s++)
			*dstptr++ = *srcptr++;

		ml_cpyinfo( trclist[i], new, status );
		if  (*status != SHE_NOERROR)  {
			sy_deallocmem( datptr );
			db_delete( new );
		} else {
			ml_inittrc( new, datptr, lth_smp, db_getr(trclist[i],ER_DELTA,NULL) );
		} /*endif*/
		db_setr( new, ER_TORIG, db_getr(trclist[i],ER_TORIG,NULL)+
			(REAL)loidx*db_getr(trclist[i],ER_DELTA,NULL), NULL );

	} /*endfor*/

} /* end of mn1_copy */



/*-------------------------------------------------------------------------*/



void mn1_rot( PARAM *par, int *status )

/* rotates 2 or 3 traces
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  trace list
 * if 2-dim
 *    2. param:  angle
 *    3. param:  lo-wdw
 *    4. param:  hi-wdw
 * if 3-dim
 *    2. param:  azimuth
 *    3. param:  angle of incidence
 *    4. param:  lo-wdw
 *    5. param:  hi-wdw
 */
{
	/* local variables */
	void     *trclist[SHC_ILISTLTH];   /* trace list */
	int      listlth;                  /* length of list */
	int      i;                        /* counter */
	REAL     azim, inci;               /* rotation angles */
	REAL     lowdw, hiwdw;             /* rotation window */
	long     start[3];                 /* start sample */
	long     lth;                      /* length in samples */
	long     end;                      /* end sample */
	void     *new[3];                  /* new traces */
	SAMPLE   *datptr[3];               /* pointer to sample data */
	int      d3type;                   /* type of 3d rotation */
	REAL     dt;                       /* sample distance */
	char     str[BC_LINELTH+1];        /* scratch string */
	TSyBoolean lowdw_spec, hiwdw_spec; /* window borders specified */

	/* executable code */

	dm_dspcoo( &lowdw, NULL, &hiwdw, NULL );
	hiwdw += lowdw;
	lowdw_spec = hiwdw_spec = FALSE;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trclist, &listlth, status );
	if  (*status != SHE_NOERROR)  return;

	if  (listlth == 2)  {
		if  (cp_pnexc(par,4,status))  return;
		cp_getfloat( par, 2, tc, "   angle: ", &azim, status );
		if  (*status != SHE_NOERROR)  return;
		if  (cp_pentered(par,3,status))  {
			cp_getfloat( par, 3, tc, "   lo-wdw: ", &lowdw, status );
			lowdw_spec = TRUE;
		} /*endif*/
		if  (*status != SHE_NOERROR)  return;
		if  (cp_pentered(par,4,status))  {
			cp_getfloat( par, 4, tc, "   hi-wdw: ", &hiwdw, status );
			hiwdw_spec = TRUE;
		} /*endif*/
		if  (*status != SHE_NOERROR)  return;
	} else if  (listlth == 3)  {
		d3type = SHC_ROT_ZNE_TO_LQT;
		if  (cp_qexist(par,"ZNE-UVW"))  d3type = SHC_ROT_ZNE_TO_UVW;
		if  (cp_qexist(par,"UVW-ZNE"))  d3type = SHC_ROT_UVW_TO_ZNE;
		if  (cp_pnexc(par,5,status))  return;
		cp_getfloat( par, 2, tc, "   azimuth: ", &azim, status );
		if  (*status != SHE_NOERROR)  return;
		if  (d3type == SHC_ROT_ZNE_TO_LQT)  {
			cp_getfloat( par, 3, tc, "   angle of incidence: ", &inci, status );
			if  (*status != SHE_NOERROR)  return;
		} /*endif*/
		if  (cp_pentered(par,4,status))  {
			cp_getfloat( par, 4, tc, "   lo-wdw: ", &lowdw, status );
			lowdw_spec = TRUE;
		} /*endif*/
		if  (*status != SHE_NOERROR)  return;
		if  (cp_pentered(par,5,status))  {
			cp_getfloat( par, 5, tc, "   hi-wdw: ", &hiwdw, status );
			hiwdw_spec = TRUE;
		} /*endif*/
		if  (*status != SHE_NOERROR)  return;
	} else {
		*status = SHE_ILPAR;
		err_setcontext( " ## list length is " ); err_setcontext_l( listlth );
		return;
	} /*endif*/

	/* decrease window size by 2 samples if no window given */
	dt = db_getr( trclist[0], ER_DELTA, NULL );
	if  (!hiwdw_spec)  hiwdw -= dt;
	if  (!lowdw_spec)  lowdw += dt*1.5;  /* center time window start in dt */

	/* get start indices and lengths of traces */
	for  (i=0;i<listlth;i++)  {
		start[i] = dm_sgetsample( trclist[i], lowdw, status );
		/* start[i] = dm_getsample( trclist[i], lowdw, TRUE ); */
		if  (*status != SHE_NOERROR)  return;
		end = dm_getsample( trclist[i], hiwdw, TRUE );
		if  (i == 0)  {
			lth = end - start[i] + 1;
			dt = db_getr( trclist[i], ER_DELTA, NULL );
		} else {
			if  ((end-start[i]+1) < lth)  lth = end-start[i]+1;
			if  (fabs(dt-db_getr(trclist[i],ER_DELTA,NULL)) > dt/10.0)  {
				*status = SHE_DIFFSAMP;
				return;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	/* create new traces */
	db_newlist();
	for  (i=0;i<listlth;i++)  {
		new[i] = db_create( status );
		if  (*status != SHE_NOERROR)  return;
		datptr[i] = (SAMPLE *)sy_allocmem( lth, (int)sizeof(SAMPLE), status );
		if  (*status != SHE_NOERROR)  {db_delete(new[i]); return;}
	} /*endfor*/

	/* perform rotation */
	if  (listlth == 2)  {
		mt_rot2( (SAMPLE *)db_getp(trclist[0],EP_DATA,NULL)+start[0],
			(SAMPLE *)db_getp(trclist[1],EP_DATA,NULL)+start[1], lth,
			azim, datptr[0], datptr[1] );
		sprintf( str, "a: %3.1f", azim );
	} else {
		mt_rot3( (SAMPLE *)db_getp(trclist[0],EP_DATA,NULL)+start[0],
			(SAMPLE *)db_getp(trclist[1],EP_DATA,NULL)+start[1],
			(SAMPLE *)db_getp(trclist[2],EP_DATA,NULL)+start[2], lth,
			azim, inci, d3type, datptr[0], datptr[1], datptr[2] );
		sprintf( str, "a: %3.1f, i: %3.1f", azim, inci );
	} /*endif*/

	/* set trace info */
	for  (i=0;i<listlth;i++)  {
		ml_cpyinfo( trclist[i], new[i], status );
		if  (*status == SHE_NOERROR)
			ml_inittrc( new[i], datptr[i], lth, db_getr(trclist[i],ER_DELTA,NULL) );
		if  (*status == SHE_NOERROR)
			ml_newstarttime( trclist[i], new[i], lowdw, status );
		if  ((*status == SHE_NOERROR) &&
			(lowdw > db_getr(trclist[i],ER_TORIG,NULL)))
			db_setr( new[i], ER_TORIG, lowdw, NULL );
		if  (*status == SHE_NOERROR)
			db_setc( new[i], EC_COMP, 'A'+(char)i, status );
		if  (*status == SHE_NOERROR)
			db_sets( new[i], ES_OPINFO, str, status );
		if  (*status != SHE_NOERROR)  {
			sy_deallocmem( datptr[i] );
			db_delist( gc, new[i] );
			db_delete( new[i] );
		} /*endif*/
	} /*endfor*/

} /* end of mn1_rot */



/*-------------------------------------------------------------------------*/



void mn1_norm( PARAM *par, int *redraw, int *status )

/* selects type of normalisation
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *redraw;        output; redraw display
 * int      *status;        output; return status
 *
 * 1. param:  normalisation type
 */
{
	/* local variables */
	char     ntype[5];   /* normalisation type */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	cp_getstr( par, 1, tc, "   norm type: ", 4, ntype, status );
	if  (*status != SHE_NOERROR)  return;

	*redraw = TRUE;
	if  (strcmp(ntype,"SF") == 0)  {
		dm_setnorm( SHC_N_SF );
	} else if  (strcmp(ntype,"SW") == 0)  {
		dm_setnorm( SHC_N_SW );
	} else if  (strcmp(ntype,"AF") == 0)  {
		dm_setnorm( SHC_N_AF );
	} else if  (strcmp(ntype,"AW") == 0)  {
		dm_setnorm( SHC_N_AW );
	} else if  (strcmp(ntype,"C") == 0)  {
		dm_setnorm( SHC_N_C );
	} else {
		*status = SHE_ILPAR;
		err_setcontext( " ## key " ); err_setcontext( ntype );
		*redraw = FALSE;
		return;
	} /*endif*/

} /* end of mn1_norm */



/*-------------------------------------------------------------------------*/



void mn1_rd( PARAM *par, int *rdlevel, int *status )

/* selects redraw level
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *rdlevel;       output; redraw level
 * int      *status;        output; return status
 *
 * 1. param:  force redraw
 */
{
	/* local variables */
	char     force[5];   /* reset redraw counter ? */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	if  (cp_pentered(par,1,status))  {
		cp_getstr( par, 1, tc, "@@", 4, force, status );
		if  (*status != SHE_NOERROR)  return;
		if  (*force == 'R')  {
			*rdlevel = 0;
		} else {
			*status = SHE_ILPAR;
			err_setcontext( " ## rd par " ); err_setcontext( force );
		} /*endif*/
	} else {
		if  (*rdlevel > 0)  (*rdlevel)--;
	} /*endif*/

} /* end of mn1_rd */



/*-------------------------------------------------------------------------*/



void mn1_sum( PARAM *par, int *status )

/* sums traces
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  trace list
 * 2. param:  lo-wdw
 * 3. param:  hi-wdw
 */
{
	/* local variables */
	void     *trclist[SHC_ILISTLTH];   /* trace list */
	int      listlth;                  /* length of list */
	int      i;                        /* counter */
	REAL     lowdw, hiwdw;             /* rotation window */
	long     lth;                      /* length in samples */
	long     end;                      /* end sample */
	void     *new;                     /* new trace */
	long     firstsamp;                /* first sample */
	SAMPLE   *start[SHC_ILISTLTH];     /* start pointer */
	SAMPLE   *datptr;                  /* pointer new data */
	REAL     wgt[SHC_ILISTLTH];        /* weights of traces */
	REAL     dt;                       /* sample distance */
	char     symbol[BC_LINELTH+1];     /* symbol name */
	char     str[BC_SHORTSTRLTH+1];    /* digit string */

	/* executable code */

	if  (cp_pnexc(par,4,status))  return;
	dm_dspcoo( &lowdw, NULL, &hiwdw, NULL );
	hiwdw += lowdw;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trclist, &listlth, status );
	if  (Severe(status))  return;
	if  (cp_pentered(par,2,status))
		cp_getfloat( par, 2, tc, "   lo-wdw: ", &lowdw, status );
	if  (Severe(status))  return;
	if  (cp_pentered(par,3,status))
		cp_getfloat( par, 3, tc, "   hi-wdw: ", &hiwdw, status );
	if  (Severe(status))  return;
	if  (cp_pentered(par,4,status))  {
		cp_getstr( par, 4, tc, "@@", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		sprintf( str, "%d", listlth );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} /*endif*/

	/* check lower bound of time window */
	for  (i=0;i<listlth;i++)
		if  (db_getr(trclist[i],ER_TORIG,NULL) > lowdw)
			lowdw = db_getr( trclist[i], ER_TORIG, NULL );

	/* get start indices and lengths of traces */
	for  (i=0;i<listlth;i++)  {
		firstsamp = dm_sgetsample( trclist[i], lowdw, status );
		if  (*status != SHE_NOERROR)  return;
		wgt[i] = db_getr( trclist[i], ER_WEIGHT, NULL );
		start[i] = (SAMPLE *)db_getp(trclist[i],EP_DATA,NULL) + firstsamp;
		end = dm_getsample( trclist[i], hiwdw, TRUE );
		if  (i == 0)  {
			lth = end - firstsamp + 1;
			dt = db_getr( trclist[i], ER_DELTA, NULL );
		} else {
			if  ((end-firstsamp+1) < lth)  lth = end-firstsamp+1;
			if  (fabs(dt-db_getr(trclist[i],ER_DELTA,NULL)) > dt/10.0)  {
				*status = SHE_DIFFSAMP;
				return;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	/* create new trace */
	db_newlist();
	if  (listlth <= 0)  return;
	new = db_create( status );
	if  (*status != SHE_NOERROR)  return;
	datptr = (SAMPLE *)sy_allocmem( lth, (int)sizeof(SAMPLE), status );
	if  (*status != SHE_NOERROR)  {db_delete(new); return;}

	/* perform summation */
	if  (cp_qexist(par,"STDDEV"))  {
		mt_stddev( start, listlth, lth, datptr );
	} else if (cp_qexist(par,"PHASES"))  {
		mt_sum_phases( start, wgt, listlth, lth, datptr );
	} else {
		mt_sum( start, wgt, !cp_qexist(par,"NONORM"), listlth, lth, datptr );
	} /*endif*/

	/* set trace info */
	ml_cpyinfo( trclist[0], new, status );
	if  (*status == SHE_NOERROR)
		db_sets( new, ES_STATION, "SUM", status );
	if  (*status == SHE_NOERROR)
		ml_inittrc( new, datptr, lth, db_getr(trclist[0],ER_DELTA,NULL) );
	if  (*status == SHE_NOERROR)
		ml_newstarttime( trclist[0], new, lowdw, status );
	if  (*status != SHE_NOERROR)  {
		sy_deallocmem( datptr ); db_delist( gc, new ); db_delete( new );
		return;
	} /*endif*/
	if  (lowdw > db_getr(trclist[0],ER_TORIG,NULL))
		db_setr( new, ER_TORIG, lowdw, NULL );

	if  (SHF_CHATTY & shflags_shv)  {
		sprintf( str, "%s %d traces summed\n", SHC_CHATTXT, listlth );
		gc_write( cc, str );
	} /*endif*/
	sprintf( str, "%d traces summed", listlth );
	db_sets( new, ES_OPINFO, str, status );

} /* end of mn1_sum */



/*-------------------------------------------------------------------------*/



void mn1_entry( PARAM *par, int *status )

/* entry manipulations
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  keyword
 * 2. param:  ...
 * 3. param:  ...
 */
{
	/* local variables */
	char     keywd[BC_LINELTH+1];   /* key word */
	char     entstr[BC_LINELTH+1];  /* entry string */
	char     type[4];               /* type of entry */
	int      shidx, qfidx;          /* new  indices */
	FILE     *lst;                  /* list file */

	/* executable code */

	cp_getstr( par, 1, tc, "   keyword: ", BC_LINELTH, keywd, status );
	if  (*status != SHE_NOERROR)  return;

	if  (strcmp(keywd,"DEFINE") == 0)  {
		if  (cp_pnexc(par,5,status))  return;
		cp_getstr( par, 2, tc, "   new entry string: ", BC_LINELTH, entstr, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 3, tc, "   type: ", 3, type, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getint( par, 4, tc, "   sh-idx: ", &shidx, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getint( par, 5, tc, "   qf-idx: ", &qfidx, status );
		if  (*status != SHE_NOERROR)  return;
		switch  (*type)  {
		case  'L':
			shidx |= EL_TYPE;
			break;
		case  'I':
			shidx |= EI_TYPE;
			break;
		case  'B':
			shidx |= EB_TYPE;
			break;
		case  'R':
			shidx |= ER_TYPE;
			break;
		case  'S':
			shidx |= ES_TYPE;
			break;
		case  'C':
			shidx |= EC_TYPE;
			break;
		case  'T':
			shidx |= ET_TYPE;
			break;
		case  'P':
			shidx |= EP_TYPE;
			break;
		case  'F':
			shidx |= EF_TYPE;
			break;
		default:
			*status = SHE_UKTYPE;
			err_setcontext( " ## type " ); err_setcontext( type );
			return;
		} /*endswitch*/
		qi_define( shidx, qfidx, status );
		if  (*status != SHE_NOERROR)  return;
		db_define( entstr, shidx, status );
		if  (*status != SHE_NOERROR)  return;
	} else if  (strcmp(keywd,"RENAME") == 0)  {
		cp_getstr( par, 2, tc, "   old name: ", BC_LINELTH, entstr, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 3, tc, "   new name: ", BC_LINELTH, keywd, status );
		if  (*status != SHE_NOERROR)  return;
		db_rename( entstr, keywd, status );
	} else if  (strcmp(keywd,"LIST") == 0)  {
		if  (cp_pnexc(par,2,status))  return;
		cp_getstr( par, 2, tc, "   list file: ", BC_LINELTH, entstr, status );
		if  (*status != SHE_NOERROR)  return;
		lst = sy_fopen( entstr, "w" );
		if  (lst == NULL)  {
			*status = SHE_OPNWR;
			err_setcontext( " ## file " ); err_setcontext( entstr );
			return;
		} /*endif*/
		db_listentries( lst, status );
		fclose( lst );
		if  (*status != SHE_NOERROR)  return;
		lst = sy_fopen( entstr, "r" );
		if  (lst == NULL)  {
			*status = SHE_OPNRD;
			err_setcontext( " ## file " ); err_setcontext( entstr );
			return;
		} /*endif*/
		FOREVER  {
			if  (fgets(keywd,BC_LINELTH,lst) == NULL)  break;
			gc_write( tc, keywd );
		} /*endfor*/
		fclose( lst );
	} else {
		*status = SHE_UKKEY;
		err_setcontext( " ## key " ); err_setcontext( keywd );
		return;
	} /*endif*/

} /* end of mn1_entry */



/*-------------------------------------------------------------------------*/



void mn1_maxampl( PARAM *par, int *status )

/* cuts off amplitudes larger than a given value
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  trace list
 * 2. param:  maximum value
 * 3. param:  wdw-lo
 * 4. param:  wdw-hi
 */
{
	/* local variables */
	TRACE    *trclist[SHC_ILISTLTH];   /* trace list */
	int      listlth;                  /* length of list */
	REAL     lowdw, hiwdw;             /* trace window in sec */
	long     loidx, lth_smp;           /* trace window in samples */
	SAMPLE   maxampl;                  /* maximum amplitude */
	int      i;                        /* counter */
	SAMPLE   *srcptr;                  /* pointer to source data */
	long     s;                        /* sample counter */

	/* executable code */

	if  (cp_pnexc(par,4,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trclist, &listlth, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 2, tc, "   maxampl: ", &maxampl, status );
	if  (*status != SHE_NOERROR)  return;
	if  (cp_pentered(par,3,status))  {
		ml_windowpars( par, 3, tc, "   lo-bound: ", "   hi-bound: ",
			&lowdw, &hiwdw, status );
	} else {
		lowdw = hiwdw = 0.;
	} /*endif*/
	if  (Severe(status))  return;

	for  (i=0;i<listlth;i++)  {

		if  (lowdw == hiwdw)  {
			loidx = 0;
			lth_smp = db_getl( trclist[i], EL_LENGTH, NULL );
		} else {
			loidx = dm_getsample( trclist[i], lowdw, TRUE );
			lth_smp = dm_getsample( trclist[i], hiwdw, TRUE ) - loidx + 1;
			if  (lth_smp <= 0)  {
				*status = SHE_ZWDW;
				return;
			} /*endif*/
		} /*endif*/

		/* loop data */
		srcptr = (SAMPLE *)db_getp( trclist[i], EP_DATA, NULL ) + loidx;
		for  (s=0;s<lth_smp;s++)  {
			if  (*srcptr > maxampl)  *srcptr = maxampl;
			if  (*srcptr < -maxampl)  *srcptr = -maxampl;
			srcptr++;
		} /*endfor*/

	} /*endfor*/

} /* end of mn1_maxampl */



/*-------------------------------------------------------------------------*/


/* File SHMENUIO.C
 *      ==========
 *
 * version 71, 4-May-2007
 *
 * menu routines (trace input/output) of seismhandler program
 * K. Stammler, 8-MAR-1990
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
#include <ctype.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_GCUSRDEF
#include "shconst.h"
#include "shvars.h"
#include "cpusrdef.h"
#include "qfusrdef.h"
#include "qiusrdef.h"
#include "utusrdef.h"
#include "erusrdef.h"
#include "tcusrdef.h"
#include "glusrdef.h"
#include "infoidx.h"
#include "fctxml.h"
#include "fctxsl.h"
#include BC_FOREIGN
#include "sherrors.h"
#include "seed_io/seedcfg.h"
#include "seed_io/seed_lib.h"
#include "seed_io/seed_cd.h"
#include "gcf/gcflib.h"
#include "sqliface.h"
#include "globalparams.h"
#ifdef SH_SOCKET
#include "port_io.h"
#endif


/* some sample distances */
#define VH_DT 10.0
#define LH_DT 1.0
#define BH_DT 0.05
#define HH_DT 0.0125


/* local prototypes */
#ifdef SH_SOCKET
void mni_reada_socket( PARAM *par, STATUS *status );
#endif


#define mni_isblank(c) ((c) == ' ' || (c) == '\t')


/*------------------------------------------------------------------------*/



void mni_read( par, status )

/* reads traces from q-file */

/* parameters of routine */
PARAM    *par;           /* input; command parameter */
int      *status;        /* output; return status */

{
	/* local variables */
	char     file[BC_LINELTH+1];     /* q-file name */
	char     recstr[BC_LINELTH+1];   /* list string */
	int      rec[SHC_ILISTLTH];      /* list of record numbers */
	char     str[BC_LINELTH+1];      /* scratch string */
	int      listlth;                /* lengt of list */
	int      reccnt;                 /* record counter */
	SAMPLE   *qdatptr;               /* data pointer from q-file */
	SAMPLE   *sdatptr;               /* data pointer after conversion */
	long     datlth;                 /* length of data array */
	BOOLEAN  q_is_ascii;             /* q-file contains ascii data */
	int      i;                      /* counter */
	int      locstat;                /* local status */
	MEMBLC   *blcptr;                /* info block pointer */
	char     *txt;                   /* text pointer */
	REAL     dt;                     /* original sample distance */
	REAL     newdt;                  /* dt after resampling */
	TIME     time;                   /* time info */
	BOOLEAN  cut_traces;             /* cut traces */
	REAL     lo_cut, hi_cut;         /* cut time window */
	char     timeinfo[BC_LINELTH+1]; /* time info string */
	unsigned timeentry;              /* time entry */
	REAL     vel;                    /* velocity */
	long     ldmy;
	char     ch;                     /* scratch */

	/* executable code */

	if  (cp_pnexc(par,4,status))  return;
	cp_getstr( par, 1, tc, "  q-file: ", BC_LINELTH, file, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getstr( par, 2, tc, "  records: ", BC_LINELTH, recstr, status );
	if  (*status != SHE_NOERROR)  return;

	/* look for cutting windows */
	cut_traces = cp_pentered(par,3,status);
	if  (Severe(status))  return;
	if  (cut_traces)  {
		cp_getfloat( par, 3, tc, "@@", &lo_cut, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "    hi-bound: ", &hi_cut, status );
		if  (Severe(status))  return;
		if  (sl_quals(par,"POS",BC_LINELTH,timeinfo,status))  {
			db_ident( timeinfo, &timeentry, status );
			if  (Severe(status))  return;
			i = timeentry & E_TYPMASK;
			if  (i != ET_TYPE  &&  i != ER_TYPE)  {
				*status = SHE_ILTYPE;
				return;
			} /*endif*/
		} else {
			if  (Severe(status))  return;
			*timeinfo = '\0';
			timeentry = 0;
		} /*endif*/
		if  (!sl_qualr(par,"VEL",&vel,status))
			vel = 6.0;
		if  (Severe(status))  return;
	} /*endif*/

	/* check swapping */
	qf_byteswap( cp_qexist(par,"SWAP") );
	if  (!sl_qualr(par,"DT",&newdt,status))
		newdt = 0.0;

	/* check ascii */
	q_is_ascii = cp_qexist( par, "ASCII" );

	/* get record list */
	if  (strcmp(recstr,"ALL") == 0)  {
		qf_size( file, &ldmy, &listlth, &reccnt, status );
		if  (*status != SHE_NOERROR)  return;
		if  (listlth > SHC_ILISTLTH)  {
			fprintf( stderr, "*SH: list length truncted to %d\n", SHC_ILISTLTH );
			listlth = SHC_ILISTLTH;
		} /*endif*/
		for  (i=1; i<=listlth; i++) {rec[i-1] = i;}
	} else {
		ut_ilist( recstr, SHC_ILISTLTH, rec, &listlth, status );
		if  (*status != SHE_NOERROR)  return;
	} /*endif*/
	locstat = SHE_NOERROR;
	db_newlist();

	/* read traces */
	for  (reccnt=0;reccnt<listlth;reccnt++)  {

		blcptr = db_create( status );
		if  (*status != SHE_NOERROR)  return;

		if  (cp_qexist(par,"LOG"))  {
			sprintf( str, "-> reading trace %d   \r", rec[reccnt] );
			gc_write( tc, str );
		} /*endif*/
		qf_read( file, rec[reccnt], TRUE, &qdatptr, status );
		if  (Severe(status))  {db_delete( blcptr ); return;}

		for  (i=0;i<ENF_LONG;i++)  {
			db_setl( blcptr, i|EL_TYPE,
				qf_getl(qi_cnvlidx(i|EL_TYPE,status),&locstat), status );
			if  (*status != SHE_NOERROR)  {qf_rclose(rec);return;}
		} /*endfor*/
		for  (i=0;i<ENF_INT;i++)  {
			db_seti( blcptr, i|EI_TYPE,
				qf_geti(qi_cnviidx(i|EI_TYPE,status),&locstat), status );
			if  (*status != SHE_NOERROR)  {qf_rclose(rec);return;}
		} /*endfor*/
		for  (i=0;i<ENF_BYTE;i++)  {
			db_setb( blcptr, i|EB_TYPE,
				(BYTE)qf_geti(qi_cnvbidx(i|EB_TYPE,status),&locstat), status );
			if  (*status != SHE_NOERROR)  {qf_rclose(rec);return;}
		} /*endfor*/
		for  (i=0;i<ENF_REAL;i++)  {
			db_setr( blcptr, i|ER_TYPE,
				qf_getr(qi_cnvridx(i|ER_TYPE,status),&locstat), status );
			if  (*status != SHE_NOERROR)  {qf_rclose(rec);return;}
		} /*endfor*/
		for  (i=0;i<ENF_STR;i++)  {
			txt = qf_gets( qi_cnvsidx(i|ES_TYPE,status), &locstat );
			if  (txt != NULL)  db_sets( blcptr, i|ES_TYPE, txt, status );
			if  (*status != SHE_NOERROR)  {qf_rclose(rec);return;}
		} /*endfor*/
		for  (i=0;i<ENF_CHAR;i++)  {
			db_setc( blcptr, i|EC_TYPE,
				qf_getc(qi_cnvcidx(i|EC_TYPE,status),&locstat), status );
			if  (*status != SHE_NOERROR)  {qf_rclose(rec);return;}
		} /*endfor*/
		for  (i=0;i<ENF_TIME;i++)  {
			txt = qf_gets( qi_cnvtidx(i|ET_TYPE,status), &locstat );
			if  (txt != NULL)  {
				tc_t2a( txt, &time, status );
				if  (*status != SHE_NOERROR)  {qf_rclose(rec);return;}
				db_sett( blcptr, i|ET_TYPE, &time, status );
				if  (*status != SHE_NOERROR)  {qf_rclose(rec);return;}
			} /*endif*/
		} /*endfor*/
		for  (i=0;i<ENF_FLAG;i++)  {
			ch = qf_getc( qi_cnvfidx(i|EF_TYPE,status), &locstat );
			locstat = (ch == 'Y') ? 1 : 0;
			db_setf( blcptr, i|EF_TYPE, locstat, status );
			if  (*status != SHE_NOERROR)  {qf_rclose(rec);return;}
		} /*endfor*/

		dt = db_getr(blcptr,ER_DELTA,status);
		if  (*status != SHE_NOERROR)  {
			qf_rclose( status );
			*status = SHE_REQENT;
			err_setcontext( " ## delta missing" );
			return;
		} /*endif*/

		/* convert ascii data if necessary */
		if  (q_is_ascii)  {
			sl_asciitobin( (char *)qdatptr,
				qf_getl(QFCL_LTH,NULL)*sizeof(SAMPLE),	(BYTE **)(&sdatptr),
				&datlth, (int)sizeof(DATATYPE), status );
			sy_deallocmem( qdatptr );
			if  (Severe(status))  {
				sy_deallocmem( sdatptr );
				qf_rclose( status );
			} /*endif*/
			datlth /= sizeof(SAMPLE);
		} else {
			sdatptr = qdatptr;
			datlth = qf_getl( QFCL_LTH, NULL );
		} /*endif*/
	
		ml_inittrc( blcptr, sdatptr, datlth, dt );

		db_setr( blcptr, ER_TORIG, 0.0, NULL );
		db_sets( blcptr, ES_FILE, file, NULL );
		db_seti( blcptr, EI_RECNO, rec[reccnt], NULL );
		db_setf( blcptr, EF_MODIF, FALSE, NULL );
		db_setf( blcptr, EF_FROMQ, TRUE, NULL );

		if  (cp_qexist(par,"ZERO"))
			db_setr( blcptr, ER_ZOOM, 0.0, NULL );

		db_gett( blcptr, ET_START, &time, &locstat );
		if  (tc_isempty(&time))  {
			tc_t2a( SHC_TIMEDEFAULT, &time, status );
			if  (*status != SHE_NOERROR)  {qf_rclose(rec);return;}
			db_sett( blcptr, ET_START, &time, status );
			if  (*status != SHE_NOERROR)  return;
		} /*endif*/

		/* resample if requested */
		if  (newdt > 0.0)  {
			ml_resample( blcptr, newdt, status );
			if  (Severe(status))  return;
		} /*endif*/

		/* cut if requested */
		if  (cut_traces)  {
			ml_cut( blcptr, lo_cut, hi_cut, timeentry, vel, status );
			if  (Severe(status))  return;
			db_setr( blcptr, ER_TORIG, 0.0, NULL );
		} /*endif*/

	} /*endfor*/

	qf_rclose( status );
	if  (*status != SHE_NOERROR)  return;

} /* end of mni_read */



/*------------------------------------------------------------------------*/



void mni_write( PARAM *par, int *status )

/* writes traces to q-file
 * old version in file _OLD.C !
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * int      *status;        output; return status
 *
 * 1. par:  q-filename
 * 2. par:  trace list
 */
{
	/* local variables */
	void     *trclist[SHC_ILISTLTH];     /* trace list */
	int      listlth;                    /* list length */
	void     *trc;                       /* current trace */
	char     qname[BC_LINELTH+1];        /* name of q-file */
	unsigned ientry;                     /* current info entry */
	int      qfentry;                    /* q-file entry */
	int      t;                          /* counter */
	SAMPLE   *datptr;                    /* pointer to data */
	long     datlth;                     /* length of data */
	BOOLEAN  q_is_ascii;                 /* write ascii data */
	BOOLEAN  save_origin;                /* save t-origin time */
	long     l_info;                     /* long info */
	int      i_info;                     /* integer info */
	REAL     r_info;                     /* real info */
	char     s_info[BC_VERYLONGSTRLTH+1];/* string info */
	TIME     t_info;                     /* time info */
	int      locstat;                    /* local status */
	int      first_rec, last_rec;        /* first and last q-record of traces */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;

	cp_getstr( par, 1, tc, "   q-file: ", BC_LINELTH, qname, status );
	if  (*status != SHE_NOERROR)  return;
	ml_gettrcpar( par, 2, tc, "   trace list: ", trclist, &listlth, status );
	if  (*status != SHE_NOERROR)  return;

	q_is_ascii = cp_qexist( par, "ASCII" );
	save_origin = cp_qexist( par, "SAVE_ORIGIN" );

	for  (t=0;t<listlth;t++)  {

		trc = trclist[t];
		qf_inihdr();

		if  (q_is_ascii)  {
			sl_bintoascii( (BYTE *)((SAMPLE *)db_getp(trc,EP_DATA,NULL)+
				db_getl(trc,EL_DSPFST,NULL)),
				db_getl(trc,EL_DSPCNT,NULL)*sizeof(SAMPLE),
				(char **)(&datptr), &datlth, (int)sizeof(DATATYPE), 36, status );
			if  (Severe(status))  return;
			datlth /= sizeof( SAMPLE );
		} else {
			datptr = (SAMPLE *)db_getp( trc, EP_DATA, NULL ) +
				db_getl( trc, EL_DSPFST, NULL );
			datlth = db_getl( trc, EL_DSPCNT, NULL );
		} /*endif*/

		db_nextentry( EL_TYPE );
		FOREVER  {
			ientry = db_nextentry( E_NEXTENTRY );
			if  (ientry == E_EOLIST)  break;
			qfentry = qi_cnvlidx( ientry, status );
			if  (*status != SHE_NOERROR)  return;
			if  (qfentry != QIC_RSRVD)   {
				locstat = SHE_NOERROR;
				if  (ientry == EL_LENGTH)  {
					l_info = datlth;
				} else {
					l_info = db_getl( trc, ientry, &locstat );
				} /*endif*/
				if  (locstat == SHE_NOERROR)  {
					qf_setl( qfentry, l_info, status );
					if  (*status != SHE_NOERROR)  return;
				} /*endif*/
			} /*endif*/
		} /*endfor*/

		db_nextentry( EI_TYPE );
		FOREVER  {
			ientry = db_nextentry( E_NEXTENTRY );
			if  (ientry == E_EOLIST)  break;
			qfentry = qi_cnviidx( ientry, status );
			if  (*status != SHE_NOERROR)  return;
			if  (qfentry != QIC_RSRVD)   {
				locstat = SHE_NOERROR;
				i_info = db_geti( trc, ientry, &locstat );
				if  (locstat == SHE_NOERROR)  {
					qf_seti( qfentry, i_info, status );
					if  (*status != SHE_NOERROR)  return;
				} /*endif*/
			} /*endif*/
		} /*endfor*/

		db_nextentry( EB_TYPE );
		FOREVER  {
			ientry = db_nextentry( E_NEXTENTRY );
			if  (ientry == E_EOLIST)  break;
			qfentry = qi_cnvbidx( ientry, status );
			if  (*status != SHE_NOERROR)  return;
			if  (qfentry != QIC_RSRVD)   {
				locstat = SHE_NOERROR;
				i_info = (int)db_getb( trc, ientry, &locstat );
				if  (locstat == SHE_NOERROR)  {
					qf_seti( qfentry, i_info, status );
					if  (*status != SHE_NOERROR)  return;
				} /*endif*/
			} /*endif*/
		} /*endfor*/

		db_nextentry( ER_TYPE );
		FOREVER  {
			ientry = db_nextentry( E_NEXTENTRY );
			if  (ientry == E_EOLIST)  break;
			qfentry = qi_cnvridx( ientry, status );
			if  (*status != SHE_NOERROR)  return;
			if  (qfentry != QIC_RSRVD)   {
				locstat = SHE_NOERROR;
				r_info = db_getr( trc, ientry, &locstat );
				if  (locstat == SHE_NOERROR)  {
					qf_setr( qfentry, r_info, status );
					if  (*status != SHE_NOERROR)  return;
				} /*endif*/
			} /*endif*/
		} /*endfor*/

		db_nextentry( ES_TYPE );
		FOREVER  {
			ientry = db_nextentry( E_NEXTENTRY );
			if  (ientry == E_EOLIST)  break;
			qfentry = qi_cnvsidx( ientry, status );
			if  (*status != SHE_NOERROR)  return;
			if  (qfentry != QIC_RSRVD)   {
				locstat = SHE_NOERROR;
				db_gets( trc, ientry, BC_LINELTH, s_info, &locstat );
				if  (locstat == SHE_NOERROR)  {
					qf_sets( qfentry, s_info, status );
					if  (*status != SHE_NOERROR)  return;
				} /*endif*/
			} /*endif*/
		} /*endfor*/

		db_nextentry( EC_TYPE );
		FOREVER  {
			ientry = db_nextentry( E_NEXTENTRY );
			if  (ientry == E_EOLIST)  break;
			qfentry = qi_cnvcidx( ientry, status );
			if  (*status != SHE_NOERROR)  return;
			if  (qfentry != QIC_RSRVD)   {
				locstat = SHE_NOERROR;
				*s_info = db_getc( trc, ientry, &locstat );
				if  (locstat == SHE_NOERROR)  {
					qf_setc( qfentry, *s_info, status );
					if  (*status != SHE_NOERROR)  return;
				} /*endif*/
			} /*endif*/
		} /*endfor*/

		db_nextentry( ET_TYPE );
		FOREVER  {
			ientry = db_nextentry( E_NEXTENTRY );
			if  (ientry == E_EOLIST)  break;
			qfentry = qi_cnvtidx( ientry, status );
			if  (*status != SHE_NOERROR)  return;
			if  (qfentry != QIC_RSRVD)   {
				locstat = SHE_NOERROR;
				db_gett( trc, ientry, &t_info, &locstat );
				if  (locstat == SHE_NOERROR)  {
					if  (ientry == ET_START)  {
						if  (db_getl(trc,EL_DSPFST,NULL) > 0)
							tc_aadd( &t_info, db_getr(trc,ER_DELTA,NULL)*
								(REAL)db_getl(trc,EL_DSPFST,NULL), &t_info );
						if  (save_origin)
							tc_aadd( &t_info, db_getr(trc,ER_TORIG,NULL),
								&t_info );
					} /*endif*/
					tc_a2t( &t_info, s_info, status );
					if  (*status != SHE_NOERROR)  return;
					qf_sets( qfentry, s_info, status );
					if  (*status != SHE_NOERROR)  return;
				} /*endif*/
			} /*endif*/
		} /*endfor*/

		db_nextentry( EF_TYPE );
		FOREVER  {
			ientry = db_nextentry( E_NEXTENTRY );
			if  (ientry == E_EOLIST)  break;
			qfentry = qi_cnvfidx( ientry, status );
			if  (*status != SHE_NOERROR)  return;
			if  (qfentry != QIC_RSRVD)   {
				locstat = SHE_NOERROR;
				i_info = db_getf( trc, ientry, &locstat );
				if  (locstat == SHE_NOERROR)  {
					*s_info = (i_info == 0) ? 'N' : 'Y';
					qf_setc( qfentry, *s_info, status );
					if  (*status != SHE_NOERROR)  return;
				} /*endif*/
			} /*endif*/
		} /*endfor*/

		qf_write( qname, datptr, &last_rec, status );
		if  (t == 0)  first_rec = last_rec;
		if  (q_is_ascii)  sy_deallocmem( datptr );
		if  (*status != SHE_NOERROR)  return;
		db_sets( trc, ES_FILE, qname, status );
		if  (*status != SHE_NOERROR)  return;
		db_seti( trc, EI_RECNO, last_rec, status );
		if  (*status != SHE_NOERROR)  return;
		db_setf( trc, EF_MODIF, FALSE, status );
		if  (*status != SHE_NOERROR)  return;
		db_setf( trc, EF_FROMQ, TRUE, status );
		if  (*status != SHE_NOERROR)  return;
	} /*endfor*/

	qf_wclose( status );

	if  (SHF_CHATTY & shflags_shv)  {
		if  (first_rec == last_rec)  {  /* only one trace */
			sprintf( s_info, "%s trace written to position %d\n",
				SHC_CHATTXT, last_rec );
			gc_write( cc, s_info );
		} else {
			sprintf( s_info, "%s traces written from pos %d to pos %d\n",
				SHC_CHATTXT, first_rec, last_rec );
			gc_write( cc, s_info );
		} /*endif*/
	} /*endif*/

} /* end of mni_write */



/*------------------------------------------------------------------------*/



void mni_writea( PARAM *par, STATUS *status )

/* writes ascii file
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * int      *status;        output; return status
 *
 * 1. par:  ascii-filename
 * 2. par:  trace list
 * 3. - N. par:  additional info
 */
{
	/* local variables */
	TRACE    *trclist[SHC_ILISTLTH];/* trace list */
	int      listlth;               /* list length */
	TRACE    *trc;                  /* current trace */
	int      t;                     /* trace counter */
	int      p;                     /* parameter counter */
	char     aname[BC_LINELTH+1];   /* name of ascii-file */
	char     fmt[BC_SHORTSTRLTH+1]; /* format of number output */
	FILE     *af;                   /* ascii file pointer */
	unsigned ientry;                /* entry number */
	STATUS   locstat;               /* local status */
	long     datlth;                /* data length to be written */
	long     i;                     /* sample counter */
	int      nums_per_line;         /* numbers per line */
	SAMPLE   *ptr;                  /* sample pointer */
	long     start;                 /* start sample */
	long     l_info;                /* long info */
	int      i_info;                /* int info */
	REAL     r_info;                /* real info */
	TIME     t_info;                /* time info */
	TSyBoolean to_socket=FALSE;     /* write to socket instead of file */
	int      socket;                /* socket ID */
	char     sockmsg[cBcLineLth+1]; /* message written on socket */

	/* executable code */

	cp_getstr( par, 1, tc, "   ascii file: ", BC_LINELTH, aname, status );
	if  (Severe(status))  return;
	ml_gettrcpar( par, 2, tc, "   trace list: ", trclist, &listlth, status );
	if  (Severe(status))  return;

	if  (!sl_quali(par,"NPL",&nums_per_line,status))
		nums_per_line = 4;
	if  (Severe(status))  return;
	if  (sl_quals(par,"FMT",BC_SHORTSTRLTH,fmt,status))  {
		sl_prepformatstr( fmt );
	} else {
		strcpy( fmt, "%e " );
		if  (Severe(status))  return;
	} /*endif*/

#ifdef SH_SOCKET
	if  (strcmp(aname,"SOCKET") == 0)  to_socket = TRUE;
#endif

	if  (to_socket)  {
		af = NULL;
		socket = ui_lastsocket();
	} else {
		af = sy_fopen( aname, "a" );
		if  (af == NULL)  {
			*status = SHE_OPNWR;
			err_setcontext( " ## file " ); err_setcontext( aname );
			return;
		} /*endif*/
	} /*endif*/

	for  (t=0; t<listlth; t++)  {
		trc = trclist[t];
		start = db_getl( trc, EL_DSPFST, NULL );
		datlth = db_getl( trc, EL_DSPCNT, NULL );
		r_info = db_getr( trc, ER_DELTA, NULL );
		if  (to_socket)  {
			sprintf( sockmsg, "%s: %e\n", "DELTA", r_info );
#ifdef SH_SOCKET
			pio_write_to_client( socket, sockmsg );
			sprintf( sockmsg, "%s: %ld\n", "LENGTH", datlth );
			pio_write_to_client( socket, sockmsg );
#endif
		} else {
			fprintf( af, "%s: %e\n", "DELTA", r_info );
			fprintf( af, "%s: %ld\n", "LENGTH", datlth );
		} /*endif*/
		for  (p=3; p<=cp_pnum(par); p++)  {
			cp_getstr( par, p, tc, "@@", BC_LINELTH, aname, status );
			if  (Severe(status))  return;
			if  (to_socket)  {
				sprintf( sockmsg, "%s: ", aname );
#ifdef SH_SOCKET
				pio_write_to_client( socket, sockmsg );
#endif
			} else {
				fprintf( af, "%s: ", aname );
			} /*endif*/
			locstat = SHE_NOERROR;
			db_ident( aname, &ientry, &locstat );
			if  (Severe(&locstat))  {
				if  (to_socket)  {
					sprintf( sockmsg, "******\n" );
#ifdef SH_SOCKET
					pio_write_to_client( socket, sockmsg );
#endif
				} else {
					fprintf( af, "******\n" );
				} /*endif*/
				locstat = SHE_NOERROR;
			} else {
				switch  (ientry & E_TYPMASK)  {
				case EL_TYPE:
					l_info = db_getl( trc, ientry, &locstat );
					if  (Severe(&locstat))  {
						if  (!to_socket)  fprintf( af, "******\n" );
					} else {
						if  (to_socket)  {
							sprintf( sockmsg, "%ld\n", l_info );
#ifdef SH_SOCKET
							pio_write_to_client( socket, sockmsg );
#endif
						} else {
							fprintf( af, "%ld\n", l_info );
						} /*endif*/
					} /*endif*/
					break;
				case EI_TYPE:
					i_info = db_geti( trc, ientry, &locstat );
					if  (Severe(&locstat))  {
						if  (!to_socket)  fprintf( af, "******\n" );
					} else {
						if  (to_socket)  {
							sprintf( sockmsg, "%d\n", i_info );
#ifdef SH_SOCKET
							pio_write_to_client( socket, sockmsg );
#endif
						} else {
							fprintf( af, "%d\n", i_info );
						} /*endif*/
					} /*endif*/
					break;
				case EB_TYPE:
					i_info = (int)db_getb( trc, ientry, &locstat );
					if  (Severe(&locstat))  {
						if  (!to_socket)  fprintf( af, "******\n" );
					} else {
						if  (to_socket)  {
							sprintf( sockmsg, "%d\n", i_info );
#ifdef SH_SOCKET
							pio_write_to_client( socket, sockmsg );
#endif
						} else {
							fprintf( af, "%d\n", i_info );
						} /*endif*/
					} /*endif*/
					break;
				case ER_TYPE:
					r_info = db_getr( trc, ientry, &locstat );
					if  (Severe(&locstat))  {
						if  (!to_socket)  fprintf( af, "******\n" );
					} else {
						if  (to_socket)  {
							sprintf( sockmsg, "%e\n", r_info );
#ifdef SH_SOCKET
							pio_write_to_client( socket, sockmsg );
#endif
						} else {
							fprintf( af, "%e\n", r_info );
						} /*endif*/
					} /*endif*/
					break;
				case ES_TYPE:
					db_gets( trc, ientry, BC_LINELTH, aname, &locstat );
					if  (Severe(&locstat))  {
						if  (!to_socket)  fprintf( af, "******\n" );
					} else {
						if  (to_socket)  {
							sprintf( sockmsg, "%s\n", aname );
#ifdef SH_SOCKET
							pio_write_to_client( socket, sockmsg );
#endif
						} else {
							fprintf( af, "%s\n", aname );
						} /*endif*/
					} /*endif*/
					break;
				case EC_TYPE:
					*aname = db_getc( trc, ientry, &locstat );
					if  (Severe(&locstat))  {
						if  (!to_socket)  fprintf( af, "******\n" );
					} else {
						if  (to_socket)  {
							sprintf( sockmsg, "%c\n", *aname );
#ifdef SH_SOCKET
							pio_write_to_client( socket, sockmsg );
#endif
						} else {
							fprintf( af, "%c\n", *aname );
						} /*endif*/
					} /*endif*/
					break;
				case ET_TYPE:
					db_gett( trc, ientry, &t_info, &locstat );
					if  (Severe(&locstat))  {
						if  (!to_socket)  fprintf( af, "******\n" );
					} else {
						if  (ientry == ET_START)
							if  (db_getl(trc,EL_DSPFST,NULL) > 0)
								tc_aadd( &t_info, db_getr(trc,ER_DELTA,NULL)*
									(REAL)db_getl(trc,EL_DSPFST,NULL), &t_info );
						tc_a2t( &t_info, aname, &locstat );
						if  (to_socket)  {
							sprintf( sockmsg, "%s\n", aname );
#ifdef SH_SOCKET
							pio_write_to_client( socket, sockmsg );
#endif
						} else {
							fprintf( af, "%s\n", aname );
						} /*endif*/
					} /*endif*/
					break;
				case EF_TYPE:
					i_info = db_getf( trc, ientry, &locstat );
					if  (Severe(&locstat))  {
						if  (!to_socket)  fprintf( af, "******\n" );
					} else {
						if  (to_socket)  {
							sprintf( sockmsg, "%d\n", i_info );
#ifdef SH_SOCKET
							pio_write_to_client( socket, sockmsg );
#endif
						} else {
							fprintf( af, "%d\n", i_info );
						} /*endif*/
					} /*endif*/
					break;
				case EX_TYPE:
					db_getx( trc, ientry, BC_LINELTH, aname, &locstat );
					if  (Severe(&locstat))  {
						if  (!to_socket)  fprintf( af, "******\n" );
					} else {
						if  (to_socket)  {
							sprintf( sockmsg, "%s\n", aname );
#ifdef SH_SOCKET
							pio_write_to_client( socket, sockmsg );
#endif
						} else {
							fprintf( af, "%s\n", aname );
						} /*endif*/
					} /*endif*/
					break;
				default:
					if  (!to_socket)  fprintf( af, "******\n" );
					break;
				} /*endswitch*/
				locstat = SHE_NOERROR;
			} /*endif*/
		} /*endfor*/
		ptr = (SAMPLE *)db_getp( trc, EP_DATA, NULL ) + start;
		if  (to_socket)  {
			*sockmsg = '\0';
			for  (i=0; i<datlth; i++)  {
				if  (strlen(sockmsg) > cBcLineLth-15)  break; /* to be improved */
				sprintf( sockmsg+strlen(sockmsg), fmt, ptr[i] );
				if  (((i+1) % nums_per_line) == 0)  {
					strcat( sockmsg, "\n" );
#ifdef SH_SOCKET
					pio_write_to_client( socket, sockmsg );
#endif
					*sockmsg = '\0';
				} else {
					strcat( sockmsg, " " );
				} /*endif*/
			} /* endfor*/
			strcat( sockmsg, "\n\n" );
#ifdef SH_SOCKET
			pio_write_to_client( socket, sockmsg );
#endif
		} else {
			for  (i=0; i<datlth; i++)  {
				fprintf( af, fmt, ptr[i] );
				if  (((i+1) % nums_per_line) == 0)
					fprintf( af, "\n" );
			} /* endfor*/
			fprintf( af, "\n\n" );
		} /*endif*/
	} /*endfor*/

	if  (!to_socket)  fclose( af );

} /* end of mni_writea */



/*------------------------------------------------------------------------*/



void mni_reada( PARAM *par, STATUS *status )

/* reads ascii file
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * int      *status;        output; return status
 *
 * 1. par:  ascii-filename
 */
{
	/* local variables */
	char     aname[BC_FILELTH+1];   /* ascii file name */
	int      infocnt;               /* info counter */
	REAL     dt;                    /* sample distance in sec */
	long     length;                /* length of trace */
	FILE     *af;                   /* pointer to ASCII file */
	char     str[BC_LONGSTRLTH+1];  /* current line */
	int      i;                     /* counter */
	int      skiplines;             /* number of lines to be skipped */
	unsigned ientry;                /* info entry */
	TRACE    *trc;                  /* trace pointer */
	STATUS   locstat;               /* local status */
	SAMPLE   *datptr;               /* pointer to data */
	long     s;                     /* sample counter */
	int      column;                /* line start to read a sample */
	long     l_info;
	int      i_info;
	REAL     r_info;
	TIME     t_info;

	/* executable code */

	dt = 0.0;
	length = 0;

	if  (cp_pnexc(par,3,status))  return;
	cp_getstr( par, 1, tc, "   ASCII file: ", BC_FILELTH, aname, status );
	if  (Severe(status))  return;
#ifdef SH_SOCKET
	if  (strcmp(aname,"SOCKET") == 0)  {
		mni_reada_socket( par, status );
		return;
	} /*endif*/
#endif
	if  (!sl_quali(par,"SKIP",&skiplines,status))
		skiplines = 0;
	if  (Severe(status))  return;
	if  (!sl_quali(par,"COLUMN",&column,status))
		column = -1;
	column--;

	af = sy_fopen( aname, "r" );
	if  (af == NULL)  {
		*status = SHE_OPNRD;
		err_setcontext( " ## file " ); err_setcontext( aname );
		return;
	} /*endif*/
	for  (i=0; i<skiplines; i++)
		if  (fgets(str,BC_LONGSTRLTH,af) == NULL)  {
			*status = SHE_FILREAD;
			err_setcontext( " ## file " ); err_setcontext( aname );
			fclose( af );
			return;
		} /*endif*/

	db_newlist();
	trc = db_create( status );
	if  (Severe(status))  return;

	infocnt = 0;
	FOREVER  {
		if  (fgets(str,BC_LONGSTRLTH,af) == NULL)   {
			*status = SHE_FILREAD;
			err_setcontext( " ## file " ); err_setcontext( aname );
			db_delete( trc );
			fclose( af );
			return;
		} /*endif*/
		i = (int)strlen( str ) - 1;
		if  (str[i] == '\n')  str[i] = '\0';
		if  (!isalpha(*str))  break;
		infocnt++;
		/* find blank or colon */
		i = 0;
		while  (str[i] != ' ' && str[i] != ':')  {
			if  (str[i] == '\0')  {
				*status = SHE_SPECERROR+15;
				db_delete( trc );
				fclose( af );
				return;
			} /*endif*/
			i++;
		} /*endif*/
		str[i++] = '\0';
		while  (str[i] == ' ')  i++;
		db_ident( str, &ientry, status );
		if  (Severe(status))  {fclose(af); db_delete(trc); return;}
		switch  (ientry & E_TYPMASK)  {
		case EL_TYPE:
			sscanf( str+i, "%ld", &l_info );
			db_setl( trc, ientry, l_info, status );
			if  (Severe(status))  {fclose(af); db_delete(trc); return;}
			break;
		case EI_TYPE:
			sscanf( str+i, "%d", &i_info );
			db_seti( trc, ientry, i_info, status );
			if  (Severe(status))  {fclose(af); db_delete(trc); return;}
			break;
		case EB_TYPE:
			sscanf( str+i, "%d", &i_info );
			db_setb( trc, ientry, (BYTE)i_info, status );
			if  (Severe(status))  {fclose(af); db_delete(trc); return;}
			break;
		case ER_TYPE:
			sscanf( str+i, "%f", &r_info );
			db_setr( trc, ientry, r_info, status );
			if  (Severe(status))  {fclose(af); db_delete(trc); return;}
			break;
		case ES_TYPE:
			db_sets( trc, ientry, str+i, status );
			if  (Severe(status))  {fclose(af); db_delete(trc); return;}
			break;
		case EC_TYPE:
			db_setc( trc, ientry, str[i], status );
			if  (Severe(status))  {fclose(af); db_delete(trc); return;}
			break;
		case ET_TYPE:
			tc_t2a( str+i, &t_info, status );
			if  (Severe(status))  {fclose(af); db_delete(trc); return;}
			db_sett( trc, ientry, &t_info, status );
			if  (Severe(status))  {fclose(af); db_delete(trc); return;}
			break;
		case EF_TYPE:
			db_setf( trc, ientry, (Cap(str[i]) == 'Y'), status );
			if  (Severe(status))  {fclose(af); db_delete(trc); return;}
			break;
		default:
			*status = SHE_ILTYPE;
			db_delete( trc );
			fclose( af );
			return;
		} /*endif*/
	} /*endfor*/

	locstat = SHE_NOERROR;
	dt = db_getr( trc, ER_DELTA, &locstat );
	if  (locstat != SHE_NOERROR)  {
		cp_getfloat( par, 2, tc, "   delta t (sec): ", &dt, status );
		if  (Severe(status))  {fclose(af); db_delete(trc); return;}
		db_setr( trc, ER_DELTA, dt, status );
		if  (Severe(status))  {fclose(af); db_delete(trc); return;}
	} /*endif*/
	locstat = SHE_NOERROR;
	length = db_getl( trc, EL_LENGTH, &locstat );
	if  (locstat != SHE_NOERROR)  {
		cp_getlong( par, 3, tc, "   length (samples): ", &length, status );
		if  (Severe(status))  {fclose(af); db_delete(trc); return;}
		db_setl( trc, EL_LENGTH, length, status );
		if  (Severe(status))  {fclose(af); db_delete(trc); return;}
	} /*endif*/

	/* read again until data start */
	fseek( af, 0L, 0 );
	for  (i=0; i<(infocnt+skiplines); i++)
		fgets( str, BC_LONGSTRLTH, af );
	datptr = (SAMPLE *)sy_allocmem( length, (int)sizeof(SAMPLE), status );
	if  (Severe(status))  {fclose(af); db_delete(trc); return;}

	/* read samples */
	if  (column < 0)  {
		for  (s=0; s<length; s++)
			fscanf( af, "%f\n", datptr+s );
	} else {
		for  (s=0; s<length; s++)  {
			fgets( str, BC_LONGSTRLTH, af );
			sscanf( str+column, "%f", datptr+s );
		} /*endfor*/
	} /*endif*/
	fclose( af );

	ml_inittrc( trc, datptr, length, dt );
	db_setr( trc, ER_TORIG, 0.0, NULL );
	db_sets( trc, ES_FILE, aname, NULL );
	db_seti( trc, EI_RECNO, 0, NULL );
	db_setf( trc, EF_MODIF, FALSE, NULL );
	db_setf( trc, EF_FROMQ, FALSE, NULL );

} /* end of mni_reada */



/*------------------------------------------------------------------------*/


#ifdef SH_SOCKET


void mni_reada_socket( PARAM *par, STATUS *status )

/* reads ascii file from socket
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * int      *status;        output; return status
 *
 * 1. par:  ascii-filename
 */
{
	/* local variables */
	int      socket;                /* socket to read from */
	int      timosec=3;             /* timeout seconds */
	int      timeout;               /* timeout occurred */
	int      eof;                   /* end of transmission found */
	int      bytelth;               /* byte length read */
	char     aname[BC_FILELTH+1];   /* ascii file name */
	int      infocnt;               /* info counter */
	REAL     dt;                    /* sample distance in sec */
	long     length;                /* length of trace */
	char     str[BC_LONGSTRLTH+1];  /* current line */
	int      i;                     /* counter */
	int      skiplines;             /* number of lines to be skipped */
	unsigned ientry;                /* info entry */
	TRACE    *trc;                  /* trace pointer */
	STATUS   locstat;               /* local status */
	SAMPLE   *datptr;               /* pointer to data */
	long     s;                     /* sample counter */
	char     *cptr;                 /* char pointer */
	long     l_info;
	int      i_info;
	REAL     r_info;
	TIME     t_info;

	/* executable code */

	printf( "--> in mni_reada_socket\n" );

	socket = ui_lastsocket();

	dt = 0.0;
	length = 0;

	if  (cp_pnexc(par,3,status))  return;
	cp_getstr( par, 1, tc, "   ASCII file: ", BC_FILELTH, aname, status );
	if  (Severe(status))  return;
	/* aname not used here, already identified as 'SOCKET' before */
	if  (!sl_quali(par,"SKIP",&skiplines,status))
		skiplines = 0;
	if  (Severe(status))  return;

	for  (i=0; i<skiplines; i++)  {
		eof = 0;
		pio_read_from_client_timeout( socket, BC_LONGSTRLTH, timosec, str,
			&bytelth, &timeout );
		if  (!timeout)  eof = pio_term_found( str, &bytelth );
		if  (eof || timeout)  {
			*status = SHE_FILREAD;
			err_setcontext( " ## reading from socket " );
			return;
		} /*endif*/
	} /*endfor*/

	db_newlist();
	trc = db_create( status );
	if  (Severe(status))  return;

	infocnt = 0;
	FOREVER  {
		eof = 0;
		pio_read_from_client_timeout( socket, BC_LONGSTRLTH, timosec, str,
			&bytelth, &timeout );
		if  (!timeout)  eof = pio_term_found( str, &bytelth );
		if  (eof || timeout)   {
			*status = SHE_FILREAD;
			err_setcontext( " ## reading from socket " );
			db_delete( trc );
			return;
		} /*endif*/
		printf( "--> mni_reada: got line %s (length %d)\n", str, bytelth );
		printf( "--> " ); fwrite( str, 1, bytelth, stdout );
		i = (int)strlen( str ) - 1;
		if  (str[i] == '\n')  str[i] = '\0';
		if  (!isalpha(*str))  break;
		infocnt++;
		/* find blank or colon */
		i = 0;
		while  (str[i] != ' ' && str[i] != ':')  {
			if  (str[i] == '\0')  {
				*status = SHE_SPECERROR+15;
				db_delete( trc );
				return;
			} /*endif*/
			i++;
		} /*endif*/
		str[i++] = '\0';
		while  (str[i] == ' ')  i++;
		db_ident( str, &ientry, status );
		if  (Severe(status))  {db_delete(trc); return;}
		switch  (ientry & E_TYPMASK)  {
		case EL_TYPE:
			sscanf( str+i, "%ld", &l_info );
			db_setl( trc, ientry, l_info, status );
			if  (Severe(status))  {db_delete(trc); return;}
			break;
		case EI_TYPE:
			sscanf( str+i, "%d", &i_info );
			db_seti( trc, ientry, i_info, status );
			if  (Severe(status))  {db_delete(trc); return;}
			break;
		case EB_TYPE:
			sscanf( str+i, "%d", &i_info );
			db_setb( trc, ientry, (BYTE)i_info, status );
			if  (Severe(status))  {db_delete(trc); return;}
			break;
		case ER_TYPE:
			sscanf( str+i, "%f", &r_info );
			db_setr( trc, ientry, r_info, status );
			if  (Severe(status))  {db_delete(trc); return;}
			break;
		case ES_TYPE:
			db_sets( trc, ientry, str+i, status );
			if  (Severe(status))  {db_delete(trc); return;}
			break;
		case EC_TYPE:
			db_setc( trc, ientry, str[i], status );
			if  (Severe(status))  {db_delete(trc); return;}
			break;
		case ET_TYPE:
			tc_t2a( str+i, &t_info, status );
			if  (Severe(status))  {db_delete(trc); return;}
			db_sett( trc, ientry, &t_info, status );
			if  (Severe(status))  {db_delete(trc); return;}
			break;
		case EF_TYPE:
			db_setf( trc, ientry, (Cap(str[i]) == 'Y'), status );
			if  (Severe(status))  {db_delete(trc); return;}
			break;
		default:
			*status = SHE_ILTYPE;
			db_delete( trc );
			return;
		} /*endif*/
	} /*endfor*/

	locstat = SHE_NOERROR;
	dt = db_getr( trc, ER_DELTA, &locstat );
	if  (locstat != SHE_NOERROR)  {
		cp_getfloat( par, 2, tc, "   delta t (sec): ", &dt, status );
		if  (Severe(status))  {db_delete(trc); return;}
		db_setr( trc, ER_DELTA, dt, status );
		if  (Severe(status))  {db_delete(trc); return;}
	} /*endif*/
	locstat = SHE_NOERROR;
	length = db_getl( trc, EL_LENGTH, &locstat );
	if  (locstat != SHE_NOERROR)  {
		cp_getlong( par, 3, tc, "   length (samples): ", &length, status );
		if  (Severe(status))  {db_delete(trc); return;}
		db_setl( trc, EL_LENGTH, length, status );
		if  (Severe(status))  {db_delete(trc); return;}
	} /*endif*/

	datptr = (SAMPLE *)sy_allocmem( length, (int)sizeof(SAMPLE), status );
	if  (Severe(status))  {db_delete(trc); return;}

	s = 0;
	FOREVER  {

		/* read samples from string */
		cptr = str;
		while  (mni_isblank(*cptr) && cptr < (str+bytelth))  cptr++;
		/*while  (sscanf(cptr,"%f",datptr+s) == 1)  {*/
		while  (cptr < (str+bytelth))  {
			printf( "--> reading number %d from %s\n", s, cptr );
			if  (sscanf(cptr,"%f",datptr+s) != 1)  break;
			if  (++s == length)  break;
			while  (!mni_isblank(*cptr) && cptr < (str+bytelth))  cptr++;
			while  (mni_isblank(*cptr) && cptr < (str+bytelth))  cptr++;
		} /*endif*/

		if  (s == length)  {printf( "--> length of %d reached\n", length ); break;}

		/* read next line */
		eof = 0;
		pio_read_from_client_timeout( socket, BC_LONGSTRLTH, timosec, str,
			&bytelth, &timeout );
		if  (!timeout)  eof = pio_term_found( str, &bytelth );
		if  (eof || timeout)   {
			*status = SHE_FILREAD;
			err_setcontext( " ## reading from socket " );
			db_delete( trc );
			return;
		} /*endif*/

	} /*endif*/

	ml_inittrc( trc, datptr, length, dt );
	db_setr( trc, ER_TORIG, 0.0, NULL );
	db_sets( trc, ES_FILE, aname, NULL );
	db_seti( trc, EI_RECNO, 0, NULL );
	db_setf( trc, EF_MODIF, FALSE, NULL );
	db_setf( trc, EF_FROMQ, FALSE, NULL );

} /* end of mni_reada_socket */


#endif   /* SH_SOCKET */


/*----------------------------------------------------------------------------*/



void mni_reads( PARAM *par, STATUS *status )

/* reads traces from MiniSEED files
 * par 1:  directory
 * par 2:  start time
 * par 3:  length in seconds
 * par 4:  station list
 * par 5:  component list
 * par 6:  stream name
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * int      *status;        output; return status
 */
{
	/* local variables */
	static SeedSetupParT seed_setup;   /* SEED setup */
	static BOOLEAN seed_init=FALSE;    /* SEED initialized */
	char     directory[BC_FILELTH+1];  /* directory name */
	char     tstart[BC_LINELTH+1];     /* start time */
	REAL     seclength;                /* length in seconds */
	char     statlist[cBcVeryLongStrLth+1];   /* station list */
	char     cmpl[BC_SHORTSTRLTH+1];   /* component list */
	char     stream[BC_SHORTSTRLTH+1]; /* stream name */
	char     stream_str[BC_LINELTH+1]; /* complete stream name */
	float    dt;                       /* sample distance in sec */
	char     stations[SHC_ILISTLTH][SHC_STATNAMELTH+1]; /*station list*/
	int      statno;                   /* number of stations */
	int      cmpno;                    /* number of components */
	int      s, c;                     /* station and component counters */
	INT32    length;                   /* length of trace in samples */
	INT32    *ldatptr;                 /* pointer to data array */
	float    *fdat;                    /* floating array pointer */
	INT32    i, j;                     /* counters */
	TRACE    *trc;                     /* trace pointer */
	TIME     atime;                    /* absolute start time */
	char     msg[BC_LINELTH+1];        /* error message */
	BOOLEAN  swap;                     /* swap data bytes */
	char     sfdfile[BC_FILELTH+1];    /* sfd file name */
	char     sfdcfile[BC_FILELTH+1];   /* file name including path */
	float    calib;                    /* calibration constant */
	char     act_start[BC_TIMELTH+1];  /* actual start time of trace */
	BOOLEAN  force_zeroes;             /* fake traces (zeroes) if unavailable */
	float    last_dt;                  /* sample distance of last traces */
	float    tdiff;                    /* time difference */
	float    default_dt;               /* default dt */
	BOOLEAN  inv_swap;                 /* invert swap header */
	BOOLEAN  inv_swap_store;           /* store old inv swap header value */
	BOOLEAN  read_jukebox;             /* read from jukebox */
	BOOLEAN  autoread;                 /* find path automatically */
	char     cdlabel[cBcShortStrLth+1]; /* label of CD (for jukebox reading) */
	char     cdmagic[cBcLineLth+1];    /* CD magic string (for jukebox reading)*/
	char     jkpath[cBcLineLth+1];     /* jukebox root path (for jkbox reading)*/
	STATUS   locstat;                  /* local status */
	char     *env;                     /* pointer to environment */
	char     shellcmd[cBcLineLth+1];   /* shell command */
	FILE     *flagf;                   /* pointer to flag file */
	int      flags;                    /* data flags */
#ifdef XXX
	BOOLEAN  no_bugfix;                /* do not call jukebox bugfix */
#endif

	/* executable code */

	if  (!seed_init)  {
		SeedSetup( &seed_setup, status );
		if  (Severe(status))  return;
		SeedLibInitialize( status );
		if  (Severe(status))  return;
		SeedSetDecodeErrorAbort( FALSE );
		seed_init = TRUE;
	} /*endif*/

	/* check for closing SEED file switch */
	if  (cp_qexist(par,"CLOSE"))  {
		SeedCloseFiles();
		return;
	} /*endif*/

	/* get parameters */
	if  (cp_pnexc(par,6,status))  return;
	cp_getstr( par, 1, tc, "  directory: ", BC_FILELTH, directory, status );
	if  (Severe(status))  return;
	cp_getstr( par, 2, tc, "  start time: ", BC_LINELTH, tstart, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 3, tc, "  length in seconds: ", &seclength, status );
	if  (Severe(status))  return;
	cp_getstr( par, 4, tc, "  station list: ", cBcVeryLongStrLth, statlist,
		status );
	if  (Severe(status))  return;
	cp_getstr( par, 5, tc, "  comp list: ", BC_SHORTSTRLTH, cmpl, status );
	if  (Severe(status))  return;
	cp_getstr( par, 6, tc, "  stream name: ", BC_SHORTSTRLTH, stream, status );
	if  (Severe(status))  return;
	force_zeroes = cp_qexist( par, "FORCE" );
	default_dt = BH_DT;
	if  (strcmp(stream,"VH") == 0)  default_dt = VH_DT;
	if  (strcmp(stream,"LH") == 0)  default_dt = LH_DT;
	if  (strcmp(stream,"BH") == 0)  default_dt = BH_DT;
	if  (strcmp(stream,"HH") == 0)  default_dt = HH_DT;

	/* check for jukebox reading */
	read_jukebox = (strcmp(directory,"JK") == 0 || strcmp(directory,"JK:") == 0);
	autoread = (strcmp(directory,"AUTO") == 0 || strcmp(directory,"AUTO:") == 0);

	/* check end of directory string */
	i = strlen( directory );
	if  (i > 0 && i < BC_FILELTH)  {
		if  (directory[i-1] != '/' && directory[i-1] != ':'
			&& directory[i-1] != '[')  {
			directory[i] = '/';
			directory[i+1] = '\0';
		} /*endif*/
	} /*endif*/

	/* get qualifiers */
	/*swap = TRUE;*/   /* changed 1.3.2007, K.S. */
	swap = GpGetBoolean( cGpB_reads_swap );   /* default */
	if  (cp_qexist(par,"NOSWAP"))  swap = FALSE;
	if  (cp_qexist(par,"SWAP"))  swap = TRUE;
	if  (!sl_quals(par,"SFD",BC_FILELTH,sfdfile,status) && *sfdfile == '\0')
		strcpy( sfdfile, "sfdfile.sfd" );
	if  (Severe(status))  return;
	/* check for inverting swap header info */
	inv_swap = GpGetBoolean( cGpB_reads_invhdr );   /* default */
	if  (cp_qexist( par, "INVHDR" )) inv_swap = TRUE;
	if  (cp_qexist( par, "NOINVHDR" )) inv_swap = FALSE;
	if  (cp_qexist(par,"TIMECORR"))  SeedUseTimeCorrection( TRUE );
	if  (cp_qexist(par,"NOTIMECORR"))  SeedUseTimeCorrection( FALSE );

	/* put together complete sfd filename, check for database request */
	if  ((strcmp(directory,"DB/") == 0) || strcmp(directory,"DB:") == 0)  {
		/* call an SQL 'select' and use the DB output instead of sfdfile */
		SqlSelectDataFiles( tstart, seclength, sfdcfile, status );
		if  (SySevere(status))  return;
	} else {
		if  (strlen(directory)+strlen(sfdfile) > BC_FILELTH)  {
			*status = SHE_STROVFL;
			return;
		} /*endif*/
		strcpy( sfdcfile, directory );
		strcat( sfdcfile, sfdfile );
	} /*endif*/

	sl_parse_stations( statlist, stations, &statno );
	ut_cap( cmpl );
	cmpno = (int)strlen( cmpl );
	db_newlist();

	/* remove commas from component list */
	j = 0;
	for  (i=0; i<cmpno; i++)  {
		if  (cmpl[i] != ',')
			if  (i == j)  {
				j++;
			} else {
				cmpl[j++] = cmpl[i];
			} /*endif*/
	} /*endfor*/
	cmpl[j] = '\0';
	cmpno = (int)j;

	/* check for online data */
	if  (strncmp(tstart,"ONLINE",6) == 0)  {
		*tstart = '\0';
		for  (s=0; s<statno; s++)  {
			for  (c=0; c<cmpno; c++)  {
				if  (cmpl[c] == 'N' || cmpl[c] == 'E')  {
					/*if  (strcmp(stations[s],"GRA2") == 0)  continue;*/
					/*if  (strcmp(stations[s],"GRA3") == 0)  continue;*/
					/*if  (strcmp(stations[s],"GRA4") == 0)  continue;*/
					/*if  (strcmp(stations[s],"GRB2") == 0)  continue;*/
					/*if  (strcmp(stations[s],"GRB3") == 0)  continue;*/
					/*if  (strcmp(stations[s],"GRB4") == 0)  continue;*/
					/*if  (strcmp(stations[s],"GRB5") == 0)  continue;*/
					/*if  (strcmp(stations[s],"GRC2") == 0)  continue;*/
					/*if  (strcmp(stations[s],"GRC3") == 0)  continue;*/
					/*if  (strcmp(stations[s],"GRC4") == 0)  continue;*/
				} /*endif*/
				sprintf( stream_str, "%s-%s-%c", stations[s], stream, cmpl[c] );
				SeedGetTimeSpan( sfdcfile, stream_str, msg, act_start, status );
				if  (*act_start == '\0')  {*status = BC_NOERROR; continue;}
				if  (Severe(status))  return;
				if  (*tstart == '\0')  {
					strcpy( tstart, act_start );
				} else {
					tdiff = tc_tdiff( act_start, tstart, status );
					if  (tdiff < 0.0 && tdiff > -300.0) /* take first time within */
						strcpy( tstart, act_start );     /* 'natural' deviations   */
				} /*endif*/
			} /*endfor*/
		} /*endfor*/
		if  (*tstart == '\0')  {*status = SHE_EMPTYLST; return;}
		tc_tadd( tstart, -seclength-1.0, tstart, status );
		if  (Severe(status))  return;
		/* printf( "--> reading time %s\n", tstart ); */
	} /*endif*/

	last_dt = 0.0;
	for  (s=0; s<statno; s++)  {
		for  (c=0; c<cmpno; c++)  {
			if  (cmpl[c] == 'N' || cmpl[c] == 'E')  {
				/*if  (strcmp(stations[s],"GRA2") == 0)  continue;*/
				/*if  (strcmp(stations[s],"GRA3") == 0)  continue;*/
				/*if  (strcmp(stations[s],"GRA4") == 0)  continue;*/
				/*if  (strcmp(stations[s],"GRB2") == 0)  continue;*/
				/*if  (strcmp(stations[s],"GRB3") == 0)  continue;*/
				/*if  (strcmp(stations[s],"GRB4") == 0)  continue;*/
				/*if  (strcmp(stations[s],"GRB5") == 0)  continue;*/
				/*if  (strcmp(stations[s],"GRC2") == 0)  continue;*/
				/*if  (strcmp(stations[s],"GRC3") == 0)  continue;*/
				/*if  (strcmp(stations[s],"GRC4") == 0)  continue;*/
			} /*endif*/
			sprintf( stream_str, "%s-%s-%c", stations[s], stream, cmpl[c] );
			/* build sfd-name if reading from jukebox */
			if  (read_jukebox || autoread)  {
				locstat = cBcNoError;
				if  (!CdDataLocked(stream_str,tstart,seclength,&locstat))
					CdFindLabel( stream_str, tstart, jkpath, cdlabel, cdmagic,
						&locstat );
				if  (SySevere(&locstat))  {
					if  (autoread)  {
						locstat = cBcNoError;
						strcpy( sfdcfile, "AUTO:" );
						strcat( sfdcfile, sfdfile );
					} else {
						err_msg( locstat, msg ); gc_write( cc, "\n" );
							gc_write( cc, msg ); gc_write( cc, "\n" );
						err_getcontext( msg );
						if  (*msg != '\0')  {
							gc_write( cc, msg );
							gc_write( cc, "\n" );
							err_clearcontext();
						} /*endif*/
						/* take last sfdcfile, but it will result in an error anyway */
						*sfdcfile = '\0';
					} /*endif*/
				} else {
					sprintf( sfdcfile, "%s/%s/sfdfile.sfd", jkpath, cdlabel );
				} /*endif*/
				if  (SHF_CHATTY & shflags_shv && *sfdcfile != '\0')
					printf( "%s reading %s from CD %s\n",
						SHC_CHATTXT, stream_str, cdlabel );
			} else {
				if  (SHF_CHATTY & shflags_shv)
					printf( "%s reading %s\n", SHC_CHATTXT, stream_str );
			} /*endif*/
			if  (inv_swap)  inv_swap_store = SeedSetInvertSwapHeader( TRUE );
			ldatptr = NULL;
			if  (*sfdcfile != '\0')
				GcfOrSeedReadStream( 0, sfdcfile, stream_str, swap, tstart,
					seclength, &ldatptr, &length, act_start, &dt, &calib,
					&flags, status );
			if  (inv_swap)  SeedSetInvertSwapHeader( inv_swap_store );
			if  (Severe(status))  {
				err_msg( *status, msg ); gc_write( cc, "\n" );
				gc_write( cc, msg ); gc_write( cc, "\n" );
				err_getcontext( msg );
				if  (*msg != '\0')  {
					gc_write( cc, msg );
					gc_write( cc, "\n" );
					err_clearcontext();
				} /*endif*/
				length = 0;
				*status = BC_NOERROR;
				if  (force_zeroes)  {
					dt = last_dt;
					if  (dt == 0.0)  {
						if  (strncmp(stations[s],"GR",2) != 0)  {
							/* *status = SHE_BUG; */
							/* return; */
							dt = default_dt;
						} else {
							dt = default_dt;
						} /*endif*/
					} /*endif*/
					length = Nlong( seclength / (float)dt );
					ldatptr = sy_allocmem( length, (int)sizeof(long), status );
					if  (Severe(status))  return;
					for  (i=0; i<length; ldatptr[i++] = 0) {}
					strcpy( act_start, tstart );
					calib = 1.0;
				} /*endif*/
			} /*endif*/
			if  (length > 0 && ldatptr != NULL)  {
				/* copy to real array */
				fdat = (float *)ldatptr;
				if  (cp_qexist(par,"COUNTS"))  {
					for  (i=0; i<length; i++)
						fdat[i] = (float)ldatptr[i];
				} else {
					for  (i=0; i<length; i++)
						fdat[i] = (float)ldatptr[i] * calib;
				} /*endif*/
				trc = db_create( status );
				if  (Severe(status))  return;
				ml_inittrc( trc, fdat, length, dt );
				db_sets( trc, ES_STATION, stations[s], NULL );
				db_setc( trc, EC_COMP, Cap(cmpl[c]), NULL );
				db_setc( trc, EC_CHAN1, Cap(stream[0]), NULL );
				db_setc( trc, EC_CHAN2, Cap(stream[1]), NULL );
				tc_t2a( act_start, &atime, status );
				if  (Severe(status))  return;
				db_sett( trc, ET_START, &atime, status );
				if  (Severe(status))  return;
				db_setr( trc, ER_TORIG, 0.0, NULL );
				db_sets( trc, ES_FILE, "FROM-SEED-FILE", NULL );
				db_seti( trc, EI_RECNO, 0, NULL );
				db_setf( trc, EF_MODIF, FALSE, NULL );
				db_setf( trc, EF_FROMQ, FALSE, NULL );
				db_setf( trc, EF_NOCALIB, (flags&Seed_F_QUAL_NOCALIB), NULL );
				db_setf( trc, EF_QUAL, (flags & ~Seed_F_QUAL_NOCALIB), NULL );
				db_setr( trc, ER_CALIB, calib, status );
				if  (Severe(status))  return;
				sprintf( msg, "%f", calib );
				db_sets( trc, ES_OPINFO, msg, status );
				if  (Severe(status))  return;
				/* check for existance of station information */
				locstat = cBcNoError;
				gl_store_station( stations[s], TRUE, &locstat );
				if  (SySevere(&locstat))  {
					printf( "*SHM: station %s unknown, set quality flag\n",
						stations[s] );
					db_setf( trc, EF_QUAL, TRUE, NULL );
				} /*endif*/
			} /*endif*/
			last_dt = dt;
		} /*endfor*/
	} /*endfor*/

} /* end of mni_reads */



/*------------------------------------------------------------------------*/


/* UN/COMPRESS support added by K.KOch 18-Nov-1999 */

void mni_readf( PARAM *par, STATUS *status )

/* reads foreign file
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * int      *status;        output; return status
 *
 * 1. par:  filename of foreign file
 * 2. par:  record
 */
{
	/* local variables */
	char     fname[BC_FILELTH+1];   /* filename of foreign file */
	int      rec[SHC_ILISTLTH];     /* list of record numbers */
	int      listlth;               /* length of list */
	int      t;                     /* trace counter */
	char     entfile[BC_FILELTH+1]; /* entry file */
	char     entstr[BC_LINELTH+1];  /* current entry line */
	int      format;                /* format ID */
	REAL     dt=0.0;                /* sample distance in sec */
	long     trclth=0;              /* length of trace in samples */
	TRACE    *trc;                  /* pointer to new trace */
	SAMPLE   *datptr;               /* pointer to sample data */
	SHENTRY  ientry;                /* entry number */
	FILE     *ef;                   /* pointer to entry file */
	int      slth;                  /* string length */
	BOOLEAN  readall;               /* read all traces */
	BOOLEAN  gunzip;                /* 'gunzip' file before reading */
	BOOLEAN  uncompress;            /* 'uncompress' file before reading */
	char     gzname[cBcFileLth+1];  /* new name for unzipped file */
	char     syscmd[cBcLongStrLth+1]; /* shell command */
	int      i;                     /* counter */
	long     l_info=0;
	REAL     r_info=0.;
	char     s_info[BC_LINELTH+1]="***";
	TIME     t_info;

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;
	cp_getstr( par, 1, tc, "   foreign file: ", BC_FILELTH, fname, status );
	if  (Severe(status))  return;
	cp_getstr( par, 2, tc, "   pos list: ", BC_LINELTH, entstr, status );
	if  (Severe(status))  return;
	readall = (strcmp(entstr,"ALL") == 0);
	if  (readall)  {
		listlth = SHC_ILISTLTH;
		for  (t=0; t<listlth; t++)
			rec[t] = t+1;
	} else {
		ut_ilist( entstr, SHC_ILISTLTH, rec, &listlth, status );
		if  (Severe(status))  return;
	} /*endif*/

	/* gunzip file if it is compressed */
	slth = strlen( fname );
	gunzip = (strcmp(fname+slth-3,".gz") == 0);
	uncompress = (strcmp(fname+slth-2,".Z") == 0);
	if  (gunzip || uncompress)  {
		strcpy( gzname, shd_scratch );
		strcat( gzname, "/GZ" );
		strcat( gzname, id_shv );
		strcat( gzname, ".DAT" );
		if (gunzip) sprintf( syscmd, "cp %s %s.gz", fname, gzname );
		if (uncompress) sprintf( syscmd, "cp %s %s.Z", fname, gzname );
		i = -1;
		while  (syscmd[++i] != '\0')
			if  (syscmd[i] == '\\')  syscmd[i] = '/';
		if  (system(syscmd) != 0)  {
			*status = SHE_SYSCMDFAIL;
			err_setcontext( " ## command: " );
			err_setcontext( syscmd );
			return;
		} /*endif*/
		if (gunzip) sprintf( syscmd, "gzip -d %s", gzname );
		if (uncompress) sprintf( syscmd, "uncompress %s", gzname );
		if  (system(syscmd) != 0)  {
			*status = SHE_SYSCMDFAIL;
			err_setcontext( " ## command: " );
			err_setcontext( syscmd );
			return;
		} /*endif*/
		strcpy( fname, gzname );
	} /*endif*/

	if  (!sl_quali(par,"FMT",&format,status))
		format = 1;
	if  (Severe(status))  return;
	if  (!sl_quals(par,"ENTRIES",BC_FILELTH,entfile,status))  {
		strcpy( entfile, shd_inputs );
		strcat( entfile, "SHFRGN.TXT" );
	} /*endif*/
	if  (Severe(status))  return;

	ef = sy_fopen( entfile, "r" );
	if  (ef == NULL)  {
		sprintf( entstr, "   no entry file %s found\n", entfile );
		gc_write( tc, entstr );
	} else {
		sprintf( entstr, "   entry file %s opened\n", entfile );
		gc_write( tc, entstr );
	} /*endif*/

	for  (t=0; t<listlth; t++)  {

		sh_frgn_trace( format, fname, rec[t], status );
		if  (Severe(status) && readall && (t > 0))  {
			*status = BC_NOERROR;
			break;
		} /*endif*/
		if  (Severe(status))  {if (ef != NULL) sy_fclose(ef); return;}
	
		db_newlist();
		trc = db_create( status );
		if  (Severe(status))  {if (ef != NULL) sy_fclose(ef); return;}
	
		if  (ef != NULL)  {
			while  (fgets(entstr,BC_LINELTH,ef) != NULL)  {
				slth = (int)strlen(entstr) - 1;
				if  (entstr[slth] == '\n')  entstr[slth] = '\0';
				db_ident( entstr, &ientry, status );
				if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
				switch  (ientry & E_TYPMASK)  {
				case EL_TYPE:
					sh_frgn_geti( entstr, &l_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					db_setl( trc, ientry, l_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					break;
				case EI_TYPE:
					sh_frgn_geti( entstr, &l_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					db_seti( trc, ientry, (int)l_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					break;
				case EB_TYPE:
					sh_frgn_geti( entstr, &l_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					db_setb( trc, ientry, (BYTE)l_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					break;
				case ER_TYPE:
					sh_frgn_getr( entstr, &r_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					db_setr( trc, ientry, r_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					break;
				case ES_TYPE:
					sh_frgn_gets( entstr, BC_LINELTH, s_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					db_sets( trc, ientry, s_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					break;
				case EC_TYPE:
					sh_frgn_getc( entstr, s_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					db_setc( trc, ientry, *s_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					break;
				case ET_TYPE:
					sh_frgn_gets( entstr, BC_LINELTH, s_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					tc_t2a( s_info, &t_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					db_sett( trc, ientry, &t_info, status );
					if  (Severe(status))  {if (ef != NULL) fclose(ef); db_delete(trc); return;}
					break;
				default:
					*status = SHE_ILTYPE;
					db_delete( trc );
					if (ef != NULL) fclose( ef );
					return;
				} /*endswitch*/
			} /*endwhile*/
			fseek( ef, 0, 0 );
		} /*endif*/
	
		sh_frgn_geti( "LENGTH", &trclth, status );
		if  (Severe(status))  {db_delete(trc); return;}
		sh_frgn_getr( "DELTA", &dt, status );
		if  (Severe(status))  {db_delete(trc); return;}
	
		datptr = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
		if  (Severe(status))  {db_delete(trc); return;}
		sh_frgn_read( datptr );
	
		ml_inittrc( trc, datptr, trclth, dt );
		db_setr( trc, ER_TORIG, 0.0, NULL );
		db_sets( trc, ES_FILE, fname, NULL );
		db_seti( trc, EI_RECNO, rec[t], NULL );
		db_setf( trc, EF_MODIF, FALSE, NULL );
		db_setf( trc, EF_FROMQ, FALSE, NULL );

	} /*endfor*/

	if  (ef != NULL)
		sy_fclose( ef );

	if  (gunzip || uncompress)  {
		/* delete scratch file */
		sprintf( syscmd, "\\rm %s", gzname );
		if  (system(syscmd) != 0)  {
			*status = SHE_SYSCMDFAIL;
			err_setcontext( " ## command: " );
			err_setcontext( syscmd );
			return;
		} /*endif*/
	} /*endif*/

} /* end of mni_readf */


/*------------------------------------------------------------------------*/

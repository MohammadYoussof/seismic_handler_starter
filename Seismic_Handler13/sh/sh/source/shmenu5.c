
/* file SHMENU5.C
 *      =========
 *
 * version 73, 3-Nov-2006
 *
 * seismhandler menu routines
 * K. Stammler, 20-Feb-1992
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
#include <ctype.h>
#include "basecnst.h"
#include BC_SYSBASE
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "shconst.h"
#include "shvars.h"
#include "cpusrdef.h"
#include "utusrdef.h"
#include "ffusrdef.h"
#include "erusrdef.h"
#include BC_GCUSRDEF
#include "infoidx.h"
#include "fctxml.h"
#include "fctxmt.h"
#include "fctxsl.h"
#include "fctxdm.h"
#include "fctxmn5.h"
#include "numres.h"
#include "lgusrdef.h"
#include "sherrors.h"


/* global variables */
#ifdef XXX
SHT_GLOBAL_SET *mn5v_global=NULL;      /* for use in motif version only */
#endif
void (*mn5v_external_routine)( char cmd[], STATUS *status)=NULL;
	/* external routine */



/*--------------------------------------------------------------------*/



void mn5_smooth( PARAM *par, STATUS *status )

/* smooths traces; modifies existing traces
 * par 1:  trace list
 * par 2:  order of smoothing
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];   /* trace list */
	int      listlth;              /* length of trace list */
	float    order;                /* order of smoothing */
	int      t;                    /* trace counter */
	long     alloclth;             /* allocation length */
	long     trclth;               /* current trace length */
	REAL     *dat;                 /* pointer to array */
	REAL     *s, *d;               /* moving pointers */
	SAMPLE   min, max;             /* new minimum and maximum */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 2, tc, "   order: ", &order, status );
	if  (Severe(status))  return;

	/* get maximum length */
	alloclth = 0;
	for  (t=0; t<listlth; t++)
		if  (db_getl(trc[t],EL_LENGTH,NULL) > alloclth)
			alloclth = db_getl( trc[t], EL_LENGTH, NULL );
	alloclth = ut_next2pow( alloclth+2*(long)(order+.5) );

	/* allocate memory */
	dat = (REAL *)sy_allocmem( alloclth, (int)sizeof(REAL), status );
	if  (Severe(status))  return;

	/* do it */
	for  (t=0; t<listlth; t++)  {
		/* copy data and smooth */
		trclth = db_getl( trc[t], EL_LENGTH, NULL );
		s = (REAL *)db_getp( trc[t], EP_DATA, NULL );
		d = dat;
		while  (d<dat+trclth)
			*d++ = *s++;
		nr_smooft( dat-1, trclth, order );
		/* copy back and update trace info */
		sl_findmax( dat, trclth, &min, &max );
		d = (REAL *)db_getp( trc[t], EP_DATA, NULL );
		s = dat;
		while  (s<dat+trclth)
			*d++ = *s++;
		db_setr( trc[t], ER_MINVAL, min, status );
		if  (Severe(status))  {sy_deallocmem(dat); return;}
		db_setr( trc[t], ER_MAXVAL, max, status );
		if  (Severe(status))  {sy_deallocmem(dat); return;}
	} /*endfor*/

	sy_deallocmem( dat );

} /* end of mn5_smooth */



/*--------------------------------------------------------------------*/



void mn5_specdiv( PARAM *par, STATUS *status )

/* divides spectra of two traces
 * par 1:  trace 1
 * par 2:  trace 2
 * par 3:  lo window
 * par 4:  hi window
 * par 5:  water level
 * par 6:  width of gauss peak
 * par 7:  time offset of output
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc1, *trc2;    /* pointers to input traces */
	TRACE    *new;            /* output trace */
	SAMPLE   *datptr;         /* pointer to sample data of output */
	REAL     wlevel;          /* water level */
	REAL     alpha;           /* width of gauss peak */
	REAL     dt;              /* sample distance */
	REAL     lowdw, hiwdw;    /* time window */
	REAL     offset;          /* time offset of output */
	long     losmp1, hismp1;  /* sample window on trace 1 */
	long     losmp2;          /* start sample on trace 2 */
	long     trclth;          /* length of output trace */

	/* executable code */

	if  (cp_pnexc(par,7,status))  return;
	trc1 = ml_get1trcpar( par, 1, tc, "   trace 1: ", status );
	if  (Severe(status))  return;
	trc2 = ml_get1trcpar( par, 2, tc, "   trace 2: ", status );
	if  (Severe(status))  return;
	ml_windowpars( par, 3, tc, "   lo wdw: ", "   hi wdw: ",
		&lowdw, &hiwdw, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 5, tc, "   water level: ", &wlevel, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 6, tc, "   alpha: ", &alpha, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 7, tc, "   offset (sec): ", &offset, status );
	if  (Severe(status))  return;

	dt = db_getr( trc1, ER_DELTA, NULL );
	if  (dt != db_getr(trc2,ER_DELTA,NULL))  {
		*status = SHE_DIFFSAMP;
		return;
	} /*endif*/

	losmp1 = dm_sgetsample( trc1, lowdw, status );
	if  (Severe(status))  return;
	hismp1 = dm_sgetsample( trc1, hiwdw, status );
	if  (Severe(status))  return;
	losmp2 = dm_sgetsample( trc2, lowdw, status );
	if  (Severe(status))  return;
	trclth = hismp1-losmp1+1;

	db_newlist();
	new = db_create( status );
	if  (Severe(status))  return;
	datptr = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
	if  (Severe(status))  return;

	ff_specdiv( (SAMPLE *)db_getp(trc1,EP_DATA,NULL)+losmp1,
		(SAMPLE *)db_getp(trc2,EP_DATA,NULL)+losmp2, trclth, dt,
		wlevel, alpha, offset, datptr, status );
	if  (Severe(status))  {
		sy_deallocmem( datptr );
		db_delete( new );
		return;
	} /*endif*/

	ml_cpyinfo( trc1, new, status );
	if  (Severe(status))  {
		sy_deallocmem( datptr ); db_delete( new );
		return;
	} /*endif*/
	ml_inittrc( new, datptr, trclth, dt );
	/* db_setr( new, ER_TORIG, 0.0, NULL ); */

} /* end of mn5_specdiv */



/*--------------------------------------------------------------------*/



void mn5_prompt( PARAM *par, int maxlth, char prompt[], STATUS *status )

/* changes prompt of SH
 * par 1:  new prompt text
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * int        maxlth;    input; maximum length of output string
 * char       prompt[];  output; new prompt
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	char     ptext[BC_LINELTH+1];   /* prompt parameter */
	char     str[BC_LINELTH+1];     /* scratch string */
	char     *cp;                   /* char pointer */
	char     *strstart;             /* start of last expression */
	int      currlth;               /* length of current string */
	int      plth;                  /* length of prompt */
	int      ctrl;                  /* control char */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	cp_getstr( par, 1, tc, "   prompt: ", BC_LINELTH, ptext, status );
	if  (Severe(status))  return;

	if  (*ptext == '\0')  {
		strcpy( prompt, "|sh> " );
		return;
	} /*endif*/

	/* find first comma in ptext */
	cp = ptext;
	while  (*cp != '\0'  &&  *cp != ',')
		cp++;

	/* if no comma in ptext just copy it to the output parameter */
	if  (*cp == '\0')  {
		if  (strlen(ptext) > maxlth)  {
			*status = SHE_STROVFL;
			return;
		} /*endif*/
		strcpy( prompt, ptext );
		return;
	} /*endif*/

	plth = 0;
	*prompt = '\0';
	strstart = ptext;
	FOREVER  {
		currlth = (int)(cp-strstart);
		if  (currlth > 0)  {
			strncpy( str, strstart, currlth );
			str[currlth] = '\0';
			if  (isdigit(*str))  {
				sscanf( str, "%d", &ctrl );
				currlth = 1;
				if  (++plth > maxlth)  {
					*status = SHE_STROVFL;
					return;
				} /*endif*/
				prompt[plth-1] = (char)ctrl;
				prompt[plth] = '\0';
			} else {
				plth += currlth;
				if  (plth > maxlth)  {
					*status = SHE_STROVFL;
					return;
				} /*endif*/
				strcat( prompt, str );
			} /*endif*/
		} /*endif*/
		/* find next comma */
		if  (*cp == '\0')  break;
		strstart = ++cp;
		if  (*cp == '\0')  break;
		while  (*cp != '\0'  &&  *cp != ',')
			cp++;
	} /*endfor*/

} /* end of mn5_prompt */



/*--------------------------------------------------------------------*/



void mn5_log( PARAM *par, STATUS *status )

/* logarithmic plot routine
 * par 1:  subfunction
 * par 2-n:  depending on subfunction
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	char     subfct[BC_SHORTSTRLTH+1];    /* subfunction */
	char     str[BC_LINELTH+1];           /* scratch string */
	CHMAP    ch;                          /* graphic channel */
	REAL     lf, hf, la, ha;              /* logplot bounds */
	TRACE    *trc[SHC_ILISTLTH];          /* trace list */
	int      listlth;                     /* list length */
	int      t;                           /* trace counter */

	/* executable code */

	cp_getstr( par, 1, tc, "   subfct: ", BC_SHORTSTRLTH, subfct, status );
	if  (Severe(status))  return;

	if  (strcmp(subfct,"WDW") == 0)  {
		cp_getstr( par, 2, tc, "   window: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		sl_cnvgcflags( str, &ch, status );
		if  (Severe(status))  return;
		lg_setwindow( ch );
	} else if  (strcmp(subfct,"LABSHIFT") == 0)  {
		if  (cp_pentered(par,2,status))
			cp_getfloat( par, 2, tc, "   horiz.pos. at horiz.axis: ", &lf, status );
		else
			lf = LGC_NOCHANGE;
		if  (Severe(status))  return;
		if  (cp_pentered(par,3,status))
			cp_getfloat( par, 3, tc, "   vert.pos at horiz.axis: ", &hf, status );
		else
			hf = LGC_NOCHANGE;
		if  (Severe(status))  return;
		if  (cp_pentered(par,4,status))
			cp_getfloat( par, 4, tc, "   horiz.pos at vert.axis: ", &la, status );
		else
			la = LGC_NOCHANGE;
		if  (Severe(status))  return;
		if  (cp_pentered(par,5,status))
			cp_getfloat( par, 5, tc, "   vert.pos at vert.axis: ", &ha, status );
		else
			ha = LGC_NOCHANGE;
		if  (Severe(status))  return;
		lg_labshift( lf, hf, la, ha );
	} else if  (strcmp(subfct,"SCALE") == 0)  {
		cp_getfloat( par, 2, tc, "   lo-frq: ", &lf, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   hi-frq: ", &hf, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   lo-amp: ", &la, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 5, tc, "   hi-amp: ", &ha, status );
		if  (Severe(status))  return;
		lg_scale( 0, lf, hf, la, ha, status );
		if  (Severe(status))  return;
	} else if  (strcmp(subfct,"PLOT") == 0)  {
		ml_gettrcpar( par, 2, tc, "   trace list: ", trc, &listlth, status );
		if  (Severe(status))  return;
		for  (t=0; t<listlth; t++)  {
			lg_logplot( db_geti(trc[t],EI_ATTRIB,NULL),
				(SAMPLE *)db_getp(trc[t],EP_DATA,NULL),
				db_getl(trc[t],EL_LENGTH,NULL),
				db_getr(trc[t],ER_TORIG,NULL),
				db_getr(trc[t],ER_DELTA,NULL) );
		} /*endfor*/
	} else {
		*status = SHE_UKKEY;
		return;
	} /*endif*/

} /* end of mn5_log */



/*----------------------------------------------------------------------------*/

#ifdef XXX

void mn5_readg( PARAM *par, STATUS *status )

/* reads traces from GRN files
 * par 1:  device
 * par 2:  start time
 * par 3:  length in seconds
 * par 4:  station list
 * par 5:  component list
 * par 6:  sample rate
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * int      *status;        output; return status
 */
{
	/* local variables */
	char     device[BC_FILELTH+1];     /* device name */
	char     tstart[BC_LINELTH+1];     /* start time */
	REAL     seclength;                /* length in seconds */
	char     statlist[BC_LINELTH+1];   /* station list */
	char     cmpl[BC_SHORTSTRLTH+1];   /* component list */
	int      smprate;                  /* sample rate */
	char     stations[GRNC_MAXSTATION][GRNC_STATNAMELTH+1]; /*station list*/
	int      statno;                   /* number of stations */
	int      cmpno;                    /* number of components */
	int      s, c;                     /* station and component counters */
	char     label[BC_FILELTH+1];      /* volume label */
	long     length;                   /* length of trace in samples */
	long     *ldatptr;                 /* pointer to data array */
	GRN_TRCINFO inf;                   /* trace info */
	float    *fdat;                    /* floating array pointer */
	long     i, j;                     /* counters */
	TRACE    *trc;                     /* trace pointer */
	TIME     atime;                    /* absolute start time */
	char     msg[BC_LINELTH+1];        /* error message */
	BOOLEAN  swap;                     /* swap data bytes */
	int      firstrec;                 /* first records */
	char     grnfile[BC_FILELTH+1];    /* specified file name */

	/* executable code */

	/* get parameters */
	if  (cp_pnexc(par,6,status))  return;
	cp_getstr( par, 1, tc, "  device: ", BC_FILELTH, device, status );
	if  (Severe(status))  return;
	cp_getstr( par, 2, tc, "  start time: ", BC_LINELTH, tstart, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 3, tc, "  length in seconds: ", &seclength, status );
	if  (Severe(status))  return;
	cp_getstr( par, 4, tc, "  station list: ", BC_LINELTH, statlist, status );
	if  (Severe(status))  return;
	cp_getstr( par, 5, tc, "  comp list: ", BC_SHORTSTRLTH, cmpl, status );
	if  (Severe(status))  return;
	cp_getint( par, 6, tc, "  sample rate: ", &smprate, status );
	if  (Severe(status))  return;

	swap = TRUE;   /* default */
	if  (cp_qexist(par,"NOSWAP"))  swap = FALSE;
	if  (cp_qexist(par,"SWAP"))  swap = TRUE;
	if  (!sl_quals(par,"FILE",BC_FILELTH,grnfile,status))
		*grnfile = '\0';
	if  (Severe(status))  return;
	if  (!sl_quali(par,"FIRST",&firstrec,status))
		firstrec = -1;
	if  (Severe(status))  return;

	if  (cp_qexist(par,"CURRENT") || *grnfile != '\0')  {
		*label = '\0';
	} else if  (!sl_quals(par,"LABEL",BC_FILELTH,label,status))  {
		if  (Severe(status))  return;
		grn_getlabel( tstart, BC_FILELTH, label, status );
		if  (*status == GRNE_LABNOTIME)  {
			if  (SHF_CHATTY & shflags_shv)
				printf( "%s no WORM available, try disk %s\n",
					SHC_CHATTXT, device );
			*label = '\0';
			*status = BC_NOERROR;
		} else {
#			ifdef BC_SUN
			*status = BC_NOERROR;
#			else
			if  (Severe(status))  return;
#			endif
			if  (SHF_CHATTY & shflags_shv)
				printf( "%s label is %s\n", SHC_CHATTXT, label );
		} /*endif*/
	} /*endif*/

	grn_parse_stations( statlist, stations, &statno );
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

	if  (*grnfile != '\0' && (statno != 1 || cmpno != 1))  {
		gc_write( cc, "*** only 1 station and 1 component makes sense ***\n" );
		return;
	} /*endif*/

	for  (s=0; s<statno; s++)  {
		for  (c=0; c<cmpno; c++)  {
			if  (cmpl[c] == 'N' || cmpl[c] == 'E')  {
				if  (strcmp(stations[s],"GRA2") == 0)  continue;
				if  (strcmp(stations[s],"GRA3") == 0)  continue;
				if  (strcmp(stations[s],"GRA4") == 0)  continue;
				/*if  (strcmp(stations[s],"GRB2") == 0)  continue;*/
				if  (strcmp(stations[s],"GRB3") == 0)  continue;
				if  (strcmp(stations[s],"GRB4") == 0)  continue;
				if  (strcmp(stations[s],"GRB5") == 0)  continue;
				/*if  (strcmp(stations[s],"GRC2") == 0)  continue;*/
				if  (strcmp(stations[s],"GRC3") == 0)  continue;
				if  (strcmp(stations[s],"GRC4") == 0)  continue;
			} /*endif*/
			if  (SHF_CHATTY & shflags_shv)
				printf( "%s reading %s-%c ",  /* no cr/lf */
					SHC_CHATTXT, stations[s], cmpl[c] );
			if  (*grnfile == '\0')  {
				grn_read( device, label, tstart, seclength, stations[s],
					cmpl[c], smprate, swap, &length, &ldatptr,
					&inf, status );
			} else {
				grn_readfile( grnfile, tstart, seclength, swap,
					firstrec, &length, &ldatptr, &inf, status );
			} /*endif*/
			if  (Severe(status))  {
				err_msg( *status, msg ); gc_write( cc, "\n" );
				gc_write( cc, msg ); gc_write( cc, "\n" );
				length = 0;
				*status = BC_NOERROR;
			} /*endif*/
			if  (length > 0)  {
				/* copy to real array */
				fdat = (float *)ldatptr;
				for  (i=0; i<length; i++)
					fdat[i] = (float)ldatptr[i] * inf.calib;
				trc = db_create( status );
				if  (Severe(status))  return;
				ml_inittrc( trc, fdat, length, inf.dt );
				ut_cap( inf.station );
				db_sets( trc, ES_STATION, inf.station, NULL );
				db_setc( trc, EC_COMP, cmpl[c], NULL );
				tc_t2a( inf.tstart, &atime, status );
				if  (Severe(status))  return;
				db_sett( trc, ET_START, &atime, status );
				if  (Severe(status))  return;
				db_setr( trc, ER_TORIG, 0.0, NULL );
				db_sets( trc, ES_FILE, "FROM-GRN-FILE", NULL );
				db_seti( trc, EI_RECNO, 0, NULL );
				db_setf( trc, EF_MODIF, FALSE, NULL );
				db_setf( trc, EF_FROMQ, FALSE, NULL );
				db_setr( trc, ER_CALIB, inf.calib, status );
				if  (Severe(status))  return;
				sprintf( msg, "%f", inf.calib );
				db_sets( trc, ES_OPINFO, msg, status );
				if  (Severe(status))  return;
			} /*endif*/
		} /*endfor*/
	} /*endfor*/

} /* end of mn5_readg */

#endif

/*----------------------------------------------------------------------------*/



void mn5_decimate( PARAM *par, STATUS *status )

/* decimates traces using mean values
 * 1. param:  trace list
 * 2. param:  integer decimation factor
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];  /* trace pointer */
	int      listlth;             /* length of trace list */
	int      decimation;          /* new sample distance in sec */
	int      i;                   /* counter */
	REAL     min, max;            /* new minimum and maximum */
	BOOLEAN  nomean;              /* no mean value, plain resampling */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;

	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	cp_getint( par, 2, tc, "   decimation factor (integer): ",
		&decimation, status );
	if  (Severe(status))  return;
	if  (decimation <= 0)  {
		*status = SHE_ILPAR;
		return;
	} /*endif*/
	if  (decimation == 1)  return;

	nomean = cp_qexist( par, "NOMEAN" );

	for  (i=0; i<listlth; i++ )  {
		ml_decimate( trc[i], decimation, nomean, status );
		if  (Severe(status)) return;
		sl_findmax( db_getp(trc[i],EP_DATA,NULL),
			db_getl(trc[i],EL_LENGTH,NULL), &min, &max );
		db_setr( trc[i], ER_MINVAL, min, status );
		if  (Severe(status))  return;
		db_setr( trc[i], ER_MAXVAL, max, status );
		if  (Severe(status))  return;
	} /*endfor*/

} /* end of mn5_decimate */



/*--------------------------------------------------------------------*/



void mn5_sample( PARAM *par, STATUS *status )

/* returns sum (mean) of samples within a selected time window
 * par 1:  trace
 * par 2:  lo time
 * par 3:  hi time
 * par 4:  output value
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc;                   /* trace pointer */
	REAL     lowdw, hiwdw;           /* time wdw in sec */
	long     loidx, hiidx;           /* time wdw in samples */
	long     i;                      /* sample counter */
	SAMPLE   sum;                    /* sum of samples */
	SAMPLE   *dat;                   /* data pointer */
	BOOLEAN  mean;                   /* compute mean */
	char     symbol[BC_LINELTH+1];   /* symbol name */
	char     str[BC_LINELTH+1];      /* scratch string */

	/* executable code */

	if  (cp_pnexc(par,4,status))  return;
	trc = ml_get1trcpar( par, 1, tc, "   trace number: ", status );
	if  (Severe(status))  return;

	if  (cp_pentered(par,2,status))  {
		ml_windowpars( par, 2, tc, "   lower bound: ", "   upper bound: ",
			&lowdw, &hiwdw, status );
		if  (Severe(status))  return;
		loidx = dm_getsample( trc, lowdw, TRUE );
		hiidx = dm_getsample( trc, hiwdw, TRUE );
	} else {
		loidx = 0;
		hiidx = db_getl( trc, EL_LENGTH, NULL ) - 1;
	} /*endif*/

	if  (loidx > hiidx)  {
		*status = SHE_ZWDW;
		return;
	} /*endif*/

	mean = TRUE;
	if  (cp_qexist(par,"MEAN"))  mean = TRUE;
	if  (cp_qexist(par,"SUM"))  mean = FALSE;

	dat = (SAMPLE *)db_getp( trc, EP_DATA, status );
	if  (Severe(status))  return;
	sum = 0.0;
	for  (i=loidx; i<=hiidx; i++)
		sum += dat[i];

	if  (mean)
		sum /= (SAMPLE)(hiidx-loidx+1);

	if  (cp_pentered(par,4,status))  {
		cp_getstr( par, 4, tc, "@@", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		sprintf( str, "%e", sum );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else {
		if  (mean)  {
			sprintf( str, "   mean value: %e\n", sum );
		} else {
			sprintf( str, "   sum: %e\n", sum );
		} /*endif*/
		gc_write( cc, str );
	} /*endif*/

} /* end of mn5_sample */



/*--------------------------------------------------------------------*/



void mn5_mean( PARAM *par, STATUS *status )

/* returns moving average
 * par 1:  trace list
 * par 2:  no of samples
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];     /* trace list */
	int      listlth;                /* length of trace list */
	int      avlth;                  /* average length */
	TRACE    *new;                   /* new trace */
	SAMPLE   *datptr;                /* data pointer */
	int      t;                      /* trace counter */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	cp_getint( par, 2, tc, "   average length (samples): ",
		&avlth, status );
	if  (Severe(status))  return;
	if  (avlth <= 0)  {
		*status = SHE_ILPAR;
		return;
	} /*endif*/

	for  (t=0; t<listlth; t++)  {
		new = db_create( status );
		if  (Severe(status))  return;
		datptr = (SAMPLE *)sy_allocmem( db_getl(trc[t],EL_LENGTH,NULL),
			(int)sizeof(SAMPLE), status );
		if  (Severe(status))  {db_delete(new); return;}
		mt_average_smoothing( db_getp(trc[t],EP_DATA,NULL),
			db_getl(trc[t],EL_LENGTH,NULL), avlth, datptr );
		ml_cpyinfo( trc[t], new, status );
		if  (Severe(status))  {
			sy_deallocmem( datptr );
			db_delete( new );
			return;
		} /*endif*/
		ml_inittrc( new, datptr, db_getl(trc[t],EL_LENGTH,NULL),
			db_getr(trc[t],ER_DELTA,NULL) );
	} /*endfor*/

} /* end of mn5_mean */



/*--------------------------------------------------------------------*/



void mn5_maximum( PARAM *par, STATUS *status )

/* computes a maximum trace and a value trace from a set of input
 * traces.  For each sample it takes the maximum amplitude of all
 * input traces as the new output sample on the maximum trace and
 * the value of a specified info entry on the value trace.
 *
 * par 1:       list of input traces
 * par 2:       info entry
 * par 3 & 4:   time window (optional)
 *
 * parameters of routine
 * PARAM      *par;          input; SH command parameters
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];    /* input trace list */
	SAMPLE   *smp[SHC_ILISTLTH];    /* pointer to samples */
	REAL     val[SHC_ILISTLTH];    /* value array */
	int      listlth;               /* length of trace list */
	char     infname[BC_LINELTH+1]; /* name of info entry */
	unsigned infentry;              /* info entry */
	REAL     lo_time, hi_time;      /* time window parameters */
	BOOLEAN  tw_set;                /* time window set */
	int      t;                     /* trace counter */
	REAL     dt;                    /* sample distance in sec */
	REAL     min_time, max_time;    /* time window for computation */
	REAL     curr_min_time;         /* current time window */
	REAL     curr_max_time;         /*   --- " --- */
	long     trclth;                /* length of output trace in samples */
	TRACE    *maxtrc;               /* maximum trace */
	TRACE    *valtrc;               /* value trace */
	SAMPLE   *maxptr;               /* pointer to maximum array */
	SAMPLE   *valptr;               /* pointer to value array */
	long     i;                     /* sample counter */

	/* executable code */

	/* get parameters */
	if  (cp_pnexc(par,4,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list :", trc, &listlth, status );
	if  (Severe(status))  return;
	cp_getstr( par, 2, tc, "   info entry: ", BC_LINELTH, infname, status );
	if  (Severe(status))  return;

	if  (cp_pentered(par,3,status))  {
		cp_getfloat( par, 3, tc, "   lo-time: ", &lo_time, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   hi-time: ", &hi_time, status );
		if  (Severe(status))  return;
		tw_set = TRUE;
	} else {
		tw_set = FALSE;
	} /*endif*/

	/* identify info entry */
	if  (strcmp(infname,"NUMBER") == 0)  {
		infentry = 0;
	} else {
		db_ident( infname, &infentry, status );
		if  (Severe(status))  return;
	} /*endif*/

	/* setup arrays */
	dt = db_getr( trc[0], ER_DELTA, NULL );
	smp[0] = (SAMPLE *)db_getp( trc[0], EP_DATA, NULL );
	min_time = db_getr( trc[0], ER_TORIG, NULL );
	max_time = min_time + dt *
		(float)(db_getl(trc[0],EL_LENGTH,NULL) - 1);
	val[0] = (infentry == 0) ? 1.0 : db_getr(trc[0],infentry,status);
	if  (Severe(status))  return;
	for  (t=1; t<listlth; t++)  {
		if  (db_getr( trc[t], ER_DELTA, NULL ) != dt)  {
			*status = SHE_DIFFSAMP;
			return;
		} /*endif*/
		smp[t] = (SAMPLE *)db_getp( trc[t], EP_DATA, NULL );
		curr_min_time = db_getr( trc[t], ER_TORIG, NULL );
		curr_max_time = curr_min_time + dt *
			(float)(db_getl(trc[t],EL_LENGTH,NULL) - 1);
		if  (curr_min_time > min_time)
			min_time = curr_min_time;
		if  (curr_max_time < max_time)
			max_time = curr_max_time;
		val[t] = (infentry == 0) ? (REAL)(t+1) :
			db_getr(trc[t],infentry,status);
		if  (Severe(status))  return;
	} /*endfor*/

	if  (tw_set)  {
		min_time = lo_time;
		max_time = hi_time;
	} /*endif*/

	if  (min_time >= max_time)  {
		*status = SHE_ZWDW;
		return;
	} /*endif*/
	trclth = Nlong( (max_time-min_time) / dt ) + 1;

	/* adjust sample arrays */
	for  (t=0; t<listlth; t++)  {
		smp[t] += dm_sgetsample( trc[t], min_time, status );
		if  (Severe(status))  return;
	} /*endfor*/

	/* create traces */
	db_newlist();  /* check other mn5_... routines */
	maxtrc = db_create( status );
	if  (Severe(status))  return;
	valtrc = db_create( status );
	if  (Severe(status))  {
		db_delete( maxtrc );
		return;
	} /*endif*/
	maxptr = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE),
		status );
	if  (Severe(status))  {
		db_delete( maxtrc );
		db_delete( valtrc );
		return;
	} /*endif*/
	valptr = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE),
		status );
	if  (Severe(status))  {
		sy_deallocmem( maxptr );
		db_delete( maxtrc );
		db_delete( valtrc );
		return;
	} /*endif*/

	/* find maxima */
	for  (i=0; i<trclth; i++)  {
		maxptr[i] = smp[0][i];
		valptr[i] = val[0];
		for  (t=1; t<listlth; t++)  {
			if  (smp[t][i] > maxptr[i])  {
				maxptr[i] = smp[t][i];
				valptr[i] = val[t];
			} /*endif*/
		} /*endfor*/
	} /*endfor*/

	if  (Severe(status))  {
		sy_deallocmem( maxptr );
		sy_deallocmem( valptr );
		db_delete( maxtrc );
		db_delete( valtrc );
		return;
	} /*endif*/

	ml_inittrc( maxtrc, maxptr, trclth, dt );
	ml_inittrc( valtrc, valptr, trclth, dt );
	db_setr( maxtrc, ER_TORIG, min_time, status );
	db_setr( valtrc, ER_TORIG, min_time, status );

} /* end of mn5_maximum */



/*--------------------------------------------------------------------*/

#ifdef XXX

void mn5_set_shmsetup_pointer( SHT_GLOBAL_SET *ptr )

/* special routine to set pointer to global variable of motif modules
 *
 * parameters of routine
 * SHT_GLOBAL_SET *ptr;      input; specifies address of global paramters
 */
{
	/* executable code */

	mn5v_global = ptr;

} /* end of mn5_shmsetup_pointer */

#endif

/*--------------------------------------------------------------------*/



void mn5_shmsetup( PARAM *par, STATUS *status )

/* setup routine for global parameters of motif modules
 * par 1:       item
 * par 2:       value
 *
 * parameters of routine
 * PARAM      *par;          input; SH command parameters
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
#ifdef XXX
	char     item[BC_LINELTH+1];    /* item name */
	char     str[BC_LINELTH+1];     /* scratch string */
	char     path[BC_FILELTH+1];    /* path name */
	int      itmp;                  /* scratch */
#endif

	/* executable code */

	printf( "*SHM: command SHMSETUP obsolete, please use 'Configure' instead\n" );
	return;

#ifdef XXX
	if  (mn5v_global == NULL)  {
		*status = SHE_NOTIMPL;
		return;
	} /*endif*/

	if  (cp_pnexc(par,2,status))  return;
	cp_getstr( par, 1, tc, "   item name :", BC_LINELTH, item, status );
	if  (Severe(status))  return;

	if  (strcmp(item,"DEPTH_PHASE_LIST") == 0)  {
		cp_getstr( par, 2, tc, "   phase list: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strchr(str,' ') != NULL)  {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
		strcpy( mn5v_global->depth_phase_list, str );
	} else if  (strcmp(item,"THEO_PHASE_LIST") == 0)  {
		cp_getstr( par, 2, tc, "   phase list: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strchr(str,' ') != NULL)  {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
		strcpy( mn5v_global->theo_phase_list, str );
	} else if  (strcmp(item,"DIFF_PHASE_LIST") == 0)  {
		cp_getstr( par, 2, tc, "   phase list: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strchr(str,' ') != NULL)  {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
		strcpy( mn5v_global->diff_phase_list, str );
	} else if  (strcmp(item,"CLOSE_PHASE_RESOL") == 0)  {
		cp_getfloat( par, 2, tc, "   resol. in pixels: ",
			&(mn5v_global->close_phase_resol), status );
	} else if  (strcmp(item,"MIN_DRAG_BOX_WIDTH") == 0)  {
		cp_getint( par, 2, tc, "   width in pixels: ",
			&(mn5v_global->min_drag_box_width), status );
	} else if  (strcmp(item,"DEFPATH_FILTER") == 0)  {
		cp_getstr( par, 2, tc, "   path name: ", BC_FILELTH, path, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->defpath_filter, path );
	} else if  (strcmp(item,"DEFPATH_EVENTS") == 0)  {
		cp_getstr( par, 2, tc, "   path name: ", BC_FILELTH, path, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->defpath_events, path );
	} else if  (strcmp(item,"DEFPATH_GSE") == 0)  {
		cp_getstr( par, 2, tc, "   path name: ", BC_FILELTH, path, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->defpath_gse, path );
	} else if  (strcmp(item,"DEFPATH_GSE2") == 0)  {
		cp_getstr( par, 2, tc, "   path name: ", BC_FILELTH, path, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->defpath_gse2, path );
	} else if  (strcmp(item,"DEFPATH_AH") == 0)  {
		cp_getstr( par, 2, tc, "   path name: ", BC_FILELTH, path, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->defpath_ah, path );
	} else if  (strcmp(item,"DEFPATH_Q") == 0)  {
		cp_getstr( par, 2, tc, "   path name: ", BC_FILELTH, path, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->defpath_q, path );
	} else if  (strcmp(item,"DEFPATH_EVID") == 0)  {
		cp_getstr( par, 2, tc, "   path name: ", BC_FILELTH, path, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->defpath_evid, path );
	} else if  (strcmp(item,"DEFPATH_EVTOUT") == 0)  {
		cp_getstr( par, 2, tc, "   path name: ", BC_FILELTH, path, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->defpath_evtout, path );
	} else if  (strcmp(item,"DEFPATH_DATA") == 0)  {
		cp_getstr( par, 2, tc, "   path name: ", BC_FILELTH, path, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->defpath_data, path );
	} else if  (strcmp(item,"TOP_DOWN_ORDER") == 0)  {
		cp_getstr( par, 2, tc, "   TRUE/FALSE: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strcmp(str,"TRUE") == 0)  {
			mn5v_global->top_down_order = TRUE;
		} else if  (strcmp(str,"FALSE") == 0)  {
			mn5v_global->top_down_order = FALSE;
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else if  (strcmp(item,"DOUBLE_CLICK_TIME") == 0)  {
		cp_getint( par, 2, tc, "   time in ms: ", &itmp, status );
		mn5v_global->double_click_time = (long)itmp;
	} else if  (strcmp(item,"TRACE_ZOOM_BASE") == 0)  {
		cp_getfloat( par, 2, tc, "   base: ",
			&(mn5v_global->trace_zoom_base), status );
	} else if  (strcmp(item,"TRACE_ZOOM_EXP") == 0)  {
		cp_getfloat( par, 2, tc, "   exponent: ",
			&(mn5v_global->trace_zoom_exp), status );
	} else if  (strcmp(item,"COLOR_MARK") == 0)  {
		cp_getstr( par, 2, tc, "   color values: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strchr(str,' ') != NULL)  {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
		if  (sscanf(str,"%f,%f,%f",&(mn5v_global->color_mark.red),
			&(mn5v_global->color_mark.green),&(mn5v_global->color_mark.blue)) != 3)
			*status = SHE_REALCNV;
	} else if  (strcmp(item,"COLOR_THEO") == 0)  {
		cp_getstr( par, 2, tc, "   color values: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strchr(str,' ') != NULL)  {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
		if  (sscanf(str,"%f,%f,%f",&(mn5v_global->color_theo.red),
			&(mn5v_global->color_theo.green),&(mn5v_global->color_theo.blue)) != 3)
			*status = SHE_REALCNV;
	} else if  (strcmp(item,"COLOR_AUTO") == 0)  {
		cp_getstr( par, 2, tc, "   color values: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strchr(str,' ') != NULL)  {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
		if  (sscanf(str,"%f,%f,%f",&(mn5v_global->color_auto.red),
			&(mn5v_global->color_auto.green),&(mn5v_global->color_auto.blue)) != 3)
			*status = SHE_REALCNV;
	} else if  (strcmp(item,"COLOR_CRSR") == 0)  {
		cp_getstr( par, 2, tc, "   color values: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strchr(str,' ') != NULL)  {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
		if  (sscanf(str,"%f,%f,%f",&(mn5v_global->color_crsr.red),
			&(mn5v_global->color_crsr.green),&(mn5v_global->color_crsr.blue)) != 3)
			*status = SHE_REALCNV;
	} else if  (strcmp(item,"REFSTATION") == 0)  {
		cp_getstr( par, 2, tc, "   reference station: ", BC_LINELTH, str, status);
		if  (Severe(status))  return;
		if  (strlen(str) > BC_SHORTSTRLTH)  {
			*status = SHE_STROVFL;
			return;
		} /*endif*/
		strcpy( mn5v_global->refstation, str );
	} else if  (strcmp(item,"AUTOPICK_FIRST") == 0)  {
		cp_getstr( par, 2, tc, "   TRUE/FALSE: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strcmp(str,"TRUE") == 0)  {
			mn5v_global->autopick_first = TRUE;
		} else if  (strcmp(str,"FALSE") == 0)  {
			mn5v_global->autopick_first = FALSE;
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else if  (strcmp(item,"CALIB_WDW_WIDTH") == 0)  {
		cp_getfloat( par, 2, tc, "   calib wdw width: ",
			&(mn5v_global->calib_wdw_width), status );
	} else if  (strcmp(item,"CALIB_WDW_HEIGHT") == 0)  {
		cp_getfloat( par, 2, tc, "   calib wdw height: ",
			&(mn5v_global->calib_wdw_height), status );
	} else if  (strcmp(item,"CALIB_SLOWNESS_GRID") == 0)  {
		cp_getfloat( par, 2, tc, "   calib slowness grid: ",
			&(mn5v_global->calib_slowness_grid), status );
	} else if  (strcmp(item,"CALIB_AZIMUTH_GRID") == 0)  {
		cp_getfloat( par, 2, tc, "   calib azimuth grid: ",
			&(mn5v_global->calib_azimuth_grid), status );
	} else if  (strcmp(item,"AUTO_SCALING") == 0)  {
		cp_getstr( par, 2, tc, "   TRUE/FALSE: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strcmp(str,"TRUE") == 0)  {
			mn5v_global->auto_scaling = TRUE;
		} else if  (strcmp(str,"FALSE") == 0)  {
			mn5v_global->auto_scaling = FALSE;
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else if  (strcmp(item,"USE_REC_FILTERS") == 0)  {
		cp_getstr( par, 2, tc, "   TRUE/FALSE: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strcmp(str,"TRUE") == 0)  {
			mn5v_global->use_rec_filters = TRUE;
		} else if  (strcmp(str,"FALSE") == 0)  {
			mn5v_global->use_rec_filters = FALSE;
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else if  (strcmp(item,"ANALYST") == 0)  {
		cp_getstr( par, 2, tc, "   analyst: ", BC_LINELTH, str, status);
		if  (Severe(status))  return;
		if  (strlen(str) > BC_SHORTSTRLTH)  {
			*status = SHE_STROVFL;
			return;
		} /*endif*/
		strcpy( mn5v_global->analyst, str );
	} else if  (strcmp(item,"EDIT_CMD") == 0)  {
		cp_getstr( par, 2, tc, "   edit_cmd: ", BC_LINELTH, str, status);
		if  (Severe(status))  return;
		strcpy( mn5v_global->edit_cmd, str );
		if  (strncmp(mn5v_global->edit_cmd,"DEFAULT",7) == 0
			|| strncmp(mn5v_global->edit_cmd,"default",7) == 0)  {
			/* local variables */
			char *env;      /* pointer to environment variable */
			/* executable code */
			env = getenv( "SH_TEXTEDIT" );
			if  (env != NULL && strlen(env) < cBcLineLth-3)
				sprintf( mn5v_global->edit_cmd, "%s %%s", env );
		} /*endif*/
	} else if  (strcmp(item,"PROMPT_ANALYST") == 0)  {
		cp_getstr( par, 2, tc, "   TRUE/FALSE: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strcmp(str,"TRUE") == 0)  {
			mn5v_global->prompt_analyst = TRUE;
		} else if  (strcmp(str,"FALSE") == 0)  {
			mn5v_global->prompt_analyst = FALSE;
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else if  (strcmp(item,"MOTIF_LOG") == 0)  {
		cp_getstr( par, 2, tc, "   motif log: ", BC_LINELTH, str, status);
		if  (Severe(status))  return;
		if  (strlen(str) > BC_FILELTH)  {
			*status = SHE_STROVFL;
			return;
		} /*endif*/
		strcpy( mn5v_global->motif_log, str );
	} else if  (strcmp(item,"PHASE_AMPL_DIFF") == 0)  {
		cp_getfloat( par, 2, tc, "   phase-ampl diff (sec): ",
			&(mn5v_global->phase_ampl_diff), status );
	} else if  (strcmp(item,"REVERSE_XORS") == 0)  {
		cp_getstr( par, 2, tc, "   TRUE/FALSE: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strcmp(str,"TRUE") == 0)  {
			mn5v_global->reverse_xors = TRUE;
		} else if  (strcmp(str,"FALSE") == 0)  {
			mn5v_global->reverse_xors = FALSE;
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else if  (strcmp(item,"X_MAX_DRAWLTH") == 0)  {
		cp_getint( par, 2, tc, "   max draw length: ",
			&(mn5v_global->x_max_drawlth), status );
	} else if  (strcmp(item,"DEFAULT_QUALITY") == 0)  {
		cp_getint( par, 2, tc, "   default quality: ",
			&(mn5v_global->default_quality), status );
		if  (mn5v_global->default_quality < 1)  mn5v_global->default_quality = 1;
		if  (mn5v_global->default_quality > 9)  mn5v_global->default_quality = 9;
	} else if  (strcmp(item,"DRAG_BOX_RUBBER_VAL") == 0)  {
		cp_getint( par, 2, tc, "   drag box rubber val: ",
			&(mn5v_global->drag_box_rubber_val), status );
	} else if  (strcmp(item,"DEFAULT_FILTER") == 0)  {
		cp_getstr( par, 2, tc, "   default_filter: ", BC_LINELTH, str, status);
		if  (Severe(status))  return;
		if  (strlen(str) > BC_FILELTH)  {
			*status = SHE_STROVFL;
			return;
		} /*endif*/
		strcpy( mn5v_global->default_filter, str );
	} else if  (strcmp(item,"DEFAULT_PHASE_TYPE") == 0)  {
		cp_getstr( par, 2, tc, "   def. phase type: ", BC_LINELTH, str, status);
		if  (Severe(status))  return;
		if  (strcmp(str,"OTHER") == 0)  {
			mn5v_global->default_phase_type = 0;  /* PIC_TYPE_OTHER */
		} else if  (strcmp(str,"TELE_QUAKE") == 0)  {
			mn5v_global->default_phase_type = 1;  /* PIC_TYPE_TELE_QUAKE */
		} else if  (strcmp(str,"NUCLEAR") == 0)  {
			mn5v_global->default_phase_type = 2;  /* PIC_TYPE_NUCLEAR */
		} else if  (strcmp(str,"REGIO_QUAKE") == 0)  {
			mn5v_global->default_phase_type = 3;  /* PIC_TYPE_REGIO_QUAKE */
		} else if  (strcmp(str,"LOCAL_QUAKE") == 0)  {
			mn5v_global->default_phase_type = 4;  /* PIC_TYPE_LOCAL_QUAKE */
		} else if  (strcmp(str,"BLAST") == 0)  {
			mn5v_global->default_phase_type = 5;  /* PIC_TYPE_BLAST */
		} else if  (strcmp(str,"MINING") == 0)  {
			mn5v_global->default_phase_type = 6;  /* PIC_TYPE_MINING */
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else if  (strcmp(item,"DEFAULT_PHASE_FLAGS") == 0)  {
		cp_getstr( par, 2, tc, "   def. phase flags: ", BC_LINELTH, str, status);
		if  (Severe(status))  return;
		if  (strcmp(str,"CLEAR") == 0)  {
			mn5v_global->default_phase_flags = 0;
		} else if  (strcmp(str,"CALIB") == 0)  {
			mn5v_global->default_phase_flags |= 0x01; /* CUC_F_EVENT_CALIB */
		} else if  (strcmp(str,"IGNORE") == 0)  {
			mn5v_global->default_phase_flags |= 0x02; /* CUC_F_EVENT_IGNORE */
		} else if  (strcmp(str,"TELEX_ALL") == 0)  {
			mn5v_global->default_phase_flags |= 0x04; /* CUC_F_EVENT_TELEX_ALL */
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else if  (strcmp(item,"DEFAULT_DEPTH_TYPE") == 0)  {
		cp_getstr( par, 2, tc, "   def. depth type: ", BC_LINELTH, str, status);
		if  (Severe(status))  return;
		if  (strcmp(str,"UNDEFINED") == 0)  {
			mn5v_global->default_depth_type = 0;  /* CUC_DEPTH_UNDEFINED */
		} else if  (strcmp(str,"PRESET") == 0)  {
			mn5v_global->default_depth_type = 1;  /* CUC_DEPTH_PRESET */
		} else if  (strcmp(str,"ESTIMATED") == 0)  {
			mn5v_global->default_depth_type = 2;  /* CUC_DEPTH_ESTIMATED */
		} else if  (strcmp(str,"FREE") == 0)  {
			mn5v_global->default_depth_type = 3;  /* CUC_DEPTH_FREE */
		} else if  (strcmp(str,"POOR") == 0)  {
			mn5v_global->default_depth_type = 4;  /* CUC_DEPTH_POOR */
		} else if  (strcmp(str,"LESSWELL") == 0)  {
			mn5v_global->default_depth_type = 5;  /* CUC_DEPTH_LESSWELL */
		} else if  (strcmp(str,"RELIABLE") == 0)  {
			mn5v_global->default_depth_type = 6;  /* CUC_DEPTH_RELIABLE */
		} else if  (strcmp(str,"EXTERNAL") == 0)  {
			mn5v_global->default_depth_type = 7;  /* CUC_DEPTH_EXTERNAL */
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else if  (strcmp(item,"DEFAULT_DEPTH") == 0)  {
		cp_getfloat( par, 2, tc, "   default depth (km): ",
			&(mn5v_global->default_depth), status );
	} else if  (strcmp(item,"DEFAULT_LOC_QUALITY") == 0)  {
		cp_getstr( par, 2, tc, "   def. loc qual.: ", BC_LINELTH, str, status);
		if  (Severe(status))  return;
		if  (strcmp(str,"UNDEFINED") == 0)  {
			mn5v_global->default_loc_quality = 0;  /* CUC_LOCQ_UNDEFINED */
		} else if  (strcmp(str,"TOOWEAK") == 0)  {
			mn5v_global->default_loc_quality = 1;  /* CUC_LOCQ_TOOWEAK */
		} else if  (strcmp(str,"INCOHERENT") == 0)  {
			mn5v_global->default_loc_quality = 2;  /* CUC_LOCQ_INCOHERENT */
		} else if  (strcmp(str,"NOBEARING") == 0)  {
			mn5v_global->default_loc_quality = 3;  /* CUC_LOCQ_NOBEARING */
		} else if  (strcmp(str,"REGION") == 0)  {
			mn5v_global->default_loc_quality = 4;  /* CUC_LOCQ_REGION */
		} else if  (strcmp(str,"RELIABLE") == 0)  {
			mn5v_global->default_loc_quality = 5;  /* CUC_LOCQ_RELIABLE */
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else if  (strcmp(item,"MAX_CURSOR_FORM") == 0)  {
		cp_getstr( par, 2, tc, "   max crsr form: ", BC_LINELTH, str, status);
		if  (Severe(status))  return;
		if  (strcmp(str,"CROSSHAIR") == 0)  {
			mn5v_global->max_cursor_form = 2;  /* MGC_CRSR_CROSSHAIR */
		} else if  (strcmp(str,"WAVEFORM") == 0)  {
			mn5v_global->max_cursor_form = 3;  /* MGC_CRSR_WAVEFORM */
		} else if  (strcmp(str,"WAVEFORM_NEG") == 0)  {
			mn5v_global->max_cursor_form = 4;  /* MGC_CRSR_WAVEFORM_NEG */
		} else if  (strcmp(str,"WAVEFORM_HILB") == 0)  {
			mn5v_global->max_cursor_form = 5;  /* MGC_CRSR_WAVEFORM_HILB */
		} else if  (strcmp(str,"WAVEFORM_NEGHILB") == 0)  {
			mn5v_global->max_cursor_form = 6;  /* MGC_CRSR_WAVEFORM_NEGHILB */
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else if  (strcmp(item,"EVENT_CHECK_PROC") == 0)  {
		cp_getstr( par, 2, tc, "   event check proc: ", BC_FILELTH, str, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->event_check_proc, str );
	} else if  (strcmp(item,"SCREENDUMP_PROC") == 0)  {
		cp_getstr( par, 2, tc, "   screendump proc: ", BC_FILELTH, str, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->screendump_proc, str );
	} else if  (strcmp(item,"EVTVIEW_PROC") == 0)  {
		cp_getstr( par, 2, tc, "   evtview proc: ", BC_FILELTH, str, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->evtview_proc, str );
	} else if  (strcmp(item,"REFORMAT_PROC") == 0)  {
		cp_getstr( par, 2, tc, "   reformat proc: ", BC_FILELTH, str, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->reformat_proc, str );
	} else if  (strcmp(item,"FINAL_PROC") == 0)  {
		cp_getstr( par, 2, tc, "   postprocessing proc: ", BC_FILELTH, str, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->final_proc, str );
	} else if  (strcmp(item,"REMREQHOST") == 0)  {
		cp_getstr( par, 2, tc, "   remreqhost: ", BC_SHORTSTRLTH, str, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->remreqhost, str );
	} else if  (strcmp(item,"FULL_PHASE_NAMES") == 0)  {
		cp_getstr( par, 2, tc, "   TRUE/FALSE: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strcmp(str,"TRUE") == 0)  {
			mn5v_global->full_phase_names = TRUE;
		} else if  (strcmp(str,"FALSE") == 0)  {
			mn5v_global->full_phase_names = FALSE;
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else if  (strcmp(item,"DEFAULT_SOURCE") == 0)  {
		cp_getstr( par, 2, tc, "   default source: ", BC_SHORTSTRLTH,
			str, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->default_source, str );
	} else if  (strcmp(item,"AUTO_PHASE") == 0)  {
		cp_getstr( par, 2, tc, "   auto phase: ", cBcShortStrLth, str, status );
		if  (Severe(status))  return;
		strcpy( mn5v_global->auto_phase, str );
	} else if  (strcmp(item,"MOVE_WDW_STEP") == 0)  {
		cp_getfloat( par, 2, tc, "   move wdw step: ",
			&(mn5v_global->move_wdw_step), status );
	} else if  (strcmp(item,"TOP_MARGIN") == 0)  {
		cp_getint( par, 2, tc, "   top margin: ",
			&(mn5v_global->top_margin), status );
	} else if  (strcmp(item,"DRAW_AREA_HEIGHT") == 0)  {
		cp_getint( par, 2, tc, "   draw area height: ",
			&(mn5v_global->draw_area_height), status );
	} else if  (strcmp(item,"DRAW_AREA_WIDTH") == 0)  {
		cp_getint( par, 2, tc, "   draw area width: ",
			&(mn5v_global->draw_area_width), status );
	} else if  (strcmp(item,"AREA_ZOOM_BASE") == 0)  {
		cp_getfloat( par, 2, tc, "   area zoom base: ",
			&(mn5v_global->area_zoom_base), status );
	} else if  (strcmp(item,"AREA_ZOOM_EXP") == 0)  {
		cp_getfloat( par, 2, tc, "   area zoom exponent: ",
			&(mn5v_global->area_zoom_exp), status );
	} else if  (strcmp(item,"OWN_ACCELERATORS") == 0)  {
		cp_getstr( par, 2, tc, "   TRUE/FALSE: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strcmp(str,"TRUE") == 0)  {
			mn5v_global->own_accelerators = TRUE;
		} else if  (strcmp(str,"FALSE") == 0)  {
			mn5v_global->own_accelerators = FALSE;
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else if  (strcmp(item,"FILTER_TYPE") == 0)  {
		cp_getstr( par, 2, tc, "   filter type: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		mn5v_global->filter_type = *str;
	} else {
		/* *status = SHE_UKKEY; */
		printf( "SH: SHMSETUP item %s unknown.  Command ignored.\n", item );
		return;
	} /*endif*/
#endif

} /* end of mn5_shmsetup */



/*--------------------------------------------------------------------*/



void mn5_replace( PARAM *par, STATUS *status )

/* replaces part of trace by another trace
 * par 1:       trace to be modified
 * par 2:       trace to be inserted
 *
 * parameters of routine
 * PARAM      *par;          input; SH command parameters
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	TRACE    *trcmod, *trcins;     /* trace pointers */
	TIME     timmod, timins;       /* start times of traces */
	long     lthmod, lthins;       /* lengths of traces */
	SAMPLE   *smpmod, *smpins;     /* pointer to sample data */
	long     insert_sample;        /* sample number to start insertion */
	long     i, cnt;               /* sample counter */

	/* executable code */

	/* get parameters from command line */
	if  (cp_pnexc(par,2,status))  return;
	trcmod = ml_get1trcpar( par, 1, tc, "   trace to modify: ", status );
	if  (Severe(status))  return;
	trcins = ml_get1trcpar( par, 2, tc, "   trace to insert: ", status );
	if  (Severe(status))  return;

	/* get start time, length and data pointers of both traces */
	db_gett( trcmod, ET_START, &timmod, status );
	if  (Severe(status))  return;
	db_gett( trcins, ET_START, &timins, status );
	if  (Severe(status))  return;
	lthmod = db_getl( trcmod, EL_LENGTH, status );
	if  (Severe(status))  return;
	lthins = db_getl( trcins, EL_LENGTH, status );
	if  (Severe(status))  return;
	smpmod = (SAMPLE *)db_getp( trcmod, EP_DATA, status );
	if  (Severe(status))  return;
	smpins = (SAMPLE *)db_getp( trcins, EP_DATA, status );
	if  (Severe(status))  return;

	/* get start sample */
	insert_sample = dm_sgetsample( trcmod, tc_adiff(&timins,&timmod), status );
	if  (Severe(status))  return;
	if  (insert_sample+lthins >= lthmod)  {
		*status = SHE_ILSMP;
		return;
	} /*endif*/

	/* do some checking */
	if  (smpmod[insert_sample-1] == 0.0)
		printf( "--> zero samples before insert position\n" );
	cnt = 0;
	for  (i=0; i<lthins; i++)
		if  (smpmod[insert_sample+i] != 0.0)  cnt++;
	if  (cnt > 0)
		printf( "--> there are %ld non-zero samples in replace window\n", cnt );

	/* replace it */
	for  (i=0; i<lthins; i++)
		smpmod[insert_sample+i] = smpins[i];

} /* end of mn5_replace */



/*--------------------------------------------------------------------*/



void mn5_findgap( PARAM *par, STATUS *status )

/* replaces part of trace by another trace
 * par 1:       trace list
 * par 2:       name of output file or TT
 * par 3:       minimum number of constant values
 * par 4:       lower window bound
 * par 5:       upper window bound
 *
 * parameters of routine
 * PARAM      *par;          input; SH command parameters
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];     /* input trace list */
	int      listlth;                /* length of trace list */
	char     outfile[BC_FILELTH+1];  /* name of output file */
	FILE     *fp;                    /* pointer to output file */
	int      t;                      /* trace counter */
	char     inftxt[BC_LINELTH+1];   /* info text */
	char     tstart[BC_TIMELTH+1];   /* start time */
	TIME     atstart;                /* absolute start time */
	char     comp;                   /* component name */
	STATUS   locstat;                /* local status */
	int      slen;                   /* string length */
	int      minzero;                /* minimum number of zero values */
	char     reqstart[BC_TIMELTH+1]; /* requested start time */
	REAL     start_diff;             /* difference in start times */
	REAL     dt;                     /* sample distance of trace in sec */
	TSyBoolean write_chan;           /* write channel info */
	int      inplev;              /* input level */
	int      fulltrace;              /* take full trace ? */
	REAL     lowdw, hiwdw;        /* time window */
	long     loidx, hiidx;        /* trace window in samples */
	long     trclth;              /* length of trace */

	/* exeutable code */

	if  (cp_qexist(par,"PARENT"))  {
		inplev = -1;
	} else if  (cp_qexist(par,"INTERACTIVE"))  {
		inplev = shv_maininput;
	} else {
		inplev = ss_inplev();
	} /*endif*/

	/* get parameters from command line */
	if  (cp_pnexc(par,5,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list :", trc, &listlth, status );
	if  (Severe(status))  return;
	cp_getstr( par, 2, tc, "   output file (or TT): ", BC_LINELTH, outfile,
		status );
	if  (Severe(status))  return;
	cp_getint( par, 3, tc, "   no of samples: ", &minzero, status );
	if  (!sl_quals(par,"REQSTART",BC_TIMELTH,reqstart,status))
		*reqstart = '\0';
	if  (Severe(status))  return;
	write_chan = cp_qexist( par, "WRITECHAN" );

	fulltrace = !cp_pentered( par, 4, status );
	if  (Severe(status))  return;
	if  (!fulltrace)  {
		ui_shift( inplev );
		ml_windowpars( par, 4, tc, "   lo bound: ", "   hi bound: ",
			&lowdw, &hiwdw, status );
		ui_unshift();
		if  (Severe(status))  return;
	} /*endif*/

	/* open output file */
	if  (strcmp(outfile,"TT") == 0 || *outfile == '\0')  {
		fp = stdout;
	} else {
		fp = sy_fopen( outfile, "a" );
		if  (fp == NULL)  {
			*status = SHE_OPNWR;
			return;
		} /*endif*/
	} /*endif*/

	/* loop over all traces */
	for  (t=0; t<listlth; t++)  {
		/* get actual start time of trace */
		locstat = BC_NOERROR;
		db_gett( trc[t], ET_START, &atstart, &locstat );
		if  (Severe(&locstat))  {
			strcpy( tstart, "1-Jul-1970_5:0:0" );
			locstat = BC_NOERROR;
		} else {
			tc_a2t( &atstart, tstart, status );
			if  (Severe(status))  return;
		} /*endif*/
		/* get station name */
		db_gets( trc[t], ES_STATION, BC_LINELTH, inftxt, &locstat );
		if  (Severe(&locstat))  {
			strcpy( inftxt, "- no station -" );
			locstat = BC_NOERROR;
		} /*endif*/
		comp = db_getc( trc[t], EC_COMP, &locstat );
		if  (Severe(&locstat))  comp = ' ';
		dt = db_getr( trc[t], ER_DELTA, status );
		if  (Severe(status))  return;
		if  (fulltrace)  {
			loidx = 0;
			hiidx = db_getl( trc[t], EL_LENGTH, NULL ) - 1;
			lowdw = db_getr(trc[t],ER_TORIG,NULL);
		} else {
			loidx = dm_getsample( trc[t], lowdw, TRUE );
			hiidx = dm_getsample( trc[t], hiwdw, TRUE );
			tc_tadd( tstart, (float)loidx*dt, tstart, status );
			if  (Severe(status))  return;
		} /*endif*/
		trclth = hiidx - loidx + 1;
		/* setup info text for error output */
		slen = (int)strlen( inftxt );
		inftxt[slen] = '-';
		if  (write_chan)  {
			inftxt[slen+1] = db_getc( trc[t], EC_CHAN1, status );
			if  (Severe(status))  return;
			inftxt[slen+2] = db_getc( trc[t], EC_CHAN2, status );
			if  (Severe(status))  return;
			inftxt[slen+3] = '-';
			inftxt[slen+4] = comp;
			inftxt[slen+5] = '\0';
		} else {
			inftxt[slen+1] = comp;
			inftxt[slen+2] = '\0';
		} /*endif*/
		strcat( inftxt, ":  " );
		/* compare requested start time with actual start time */
		if  (*reqstart != '\0')  {
			start_diff = tc_tdiff( tstart, reqstart, status );
			if  (fabs(start_diff) >= dt)
				fprintf( fp, "%s time %s, %4d samples not found\n",
					inftxt, reqstart, Nint(start_diff/dt) );
		} /*endif*/
		/* check for gaps in data sequence */
		mt_find_gap( minzero, (SAMPLE *)db_getp(trc[t],EP_DATA,&locstat)+loidx,
			trclth, inftxt, tstart, dt, fp );
	} /*endfor*/

	/* close output file */
	if  (fp != stdout)
		sy_fclose( fp );

} /* end of mn5_findgap */



/*--------------------------------------------------------------------*/



void mn5_set_external_routine( void (*ext_rout)(char cmd[],STATUS *status) )

/* sets external routine
 *
 * parameters of routine
 * void       (*ext_rout)(char cmd[],STATUS *status)  input; external routine
 */
{
	/* executable code */

	mn5v_external_routine = ext_rout;

} /* end of mn5_set_external_routine */


/*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*/
/*
 * seismhandler menu routines
 * fixgap routine by
 * K. Koch, 22-NOV-1999
*/
/*--------------------------------------------------------------------*/


void mn5_fixgap( PARAM *par, STATUS *status )

/* removes  zeros from traces introducing a straight line 
	connecting non-zero values
 * par 1:    trace list
 * par 2:    value to be replaced (default 0)
 * par 3:    lo time bound
 * par 4:    hi time bound
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];   /* trace list */
	int      listlth;              /* list length */
	REAL     gapvalue;             /* value defining gap */
	int      t;                    /* trace counter */
	REAL     lowdw, hiwdw;         /* time window */
	long     loidx, hiidx;         /* sample window */
	BOOLEAN  const_trace;          /* replace with constant values */
	BOOLEAN  one_samp;             /* replace even a single value */
	BOOLEAN  fulltrace;            /* take full traces */
	SAMPLE   *start;               /* pointer to trace start */
	long     trclth;               /* length of trace */
	char     str[BC_LINELTH+1];    /* scratch */

	/* executable code */

	if  (cp_pnexc(par,5,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 2, tc, "   value to replace: ", &gapvalue, status );
	if  (Severe(status))  return;
	fulltrace = !cp_pentered( par, 3, status );
	if  (!fulltrace)  {
		ml_windowpars( par, 3, tc, "@@", "   upper bound: ", &lowdw,
			&hiwdw, status );
		if  (Severe(status))  return;
	} /*endif*/

/* What about using a constant line?? */
	const_trace=FALSE;
	if  (cp_qexist(par,"CONST")) const_trace=TRUE;

/* Fix gaps for even a single sample */
	one_samp=FALSE;
	if  (cp_qexist(par,"ONE")) one_samp=TRUE;

	for  (t=0; t<listlth; t++)  {
		start = (SAMPLE *)db_getp( trc[t], EP_DATA, NULL );
		if  (fulltrace)    {
			trclth = db_getl( trc[t], EL_LENGTH, NULL );
		} else {
			loidx = dm_getsample( trc[t], lowdw, TRUE );
			hiidx = dm_getsample( trc[t], hiwdw, TRUE );
			start += loidx;
			trclth = hiidx - loidx + 1;
		} /*endif*/
		mt_fix_gap( start, trclth, gapvalue, const_trace, one_samp);
	} /*endfor*/

} /* end of mn5_fixgap */


/*--------------------------------------------------------------------*/


void mn5_external_routine( PARAM *par, STATUS *status )

/* calls external routine mn5v_external_routine
 *
 * parameters of routine
 * PARAM      *par;          input; SH command parameters
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	char     text[BC_LINELTH+1];    /* text parameter */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	cp_getstr( par, 1, tc, "   parameter: ", BC_LINELTH, text, status );
	if  (Severe(status))  return;
	if  (mn5v_external_routine != NULL)
		(*mn5v_external_routine)( text, status );

} /* end of mn5_external_routine */



/*--------------------------------------------------------------------*/

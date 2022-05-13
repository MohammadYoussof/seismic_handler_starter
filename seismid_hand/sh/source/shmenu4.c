
/* file SHMENU4.C
 *      =========
 *
 * version 25, 3-Dec-2006
 *
 * seismhandler menu routines
 * K. Stammler, 5-APR-1991
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
#include BC_SYSBASE
#include "shconst.h"
#include "shvars.h"
#include "cpusrdef.h"
#include "mxusrdef.h"
#include "pfusrdef.h"
#include "erusrdef.h"
#include "ssusrdef.h"
#include "uiusrdef.h"
#include BC_GCUSRDEF
#include "infoidx.h"
#include "fctxml.h"
#include "fctxmt.h"
#include "fctxsl.h"
#include "fctxdm.h"
#include "fctxmn4.h"
#include "numres.h"
#include "sherrors.h"
#include "globalparams.h"


/* prototypes of local routines */
static float mn4_taper( int idx, int maxidx );
static int mn4_constnum( SAMPLE s[], int lth );


/*--------------------------------------------------------------------*/



void mn4_append( PARAM *par, STATUS *status )

/* appends trace to another
 * par 1:  trace to append
 * par 2:  destination trace
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *src, *dst;    /* source and destination trace */
	REAL     min;           /* scratch */
	long     newlth;        /* new length of destination trace in samples */
	long     lth1, lth2;    /* lengths of input traces */
	SAMPLE   *newptr;       /* pointer to new data array */
	SAMPLE   *s, *d;        /* moving pointers */
	SAMPLE   *endptr;       /* pointer to last sample+1 */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;
	src = ml_get1trcpar( par, 1, tc, "   source trace: ", status );
	if  (Severe(status))  return;
	dst = ml_get1trcpar( par, 2, tc, "   destination trace: ", status );
	if  (Severe(status))  return;

	/* check sample rate */
	min = db_getr(src,ER_DELTA,NULL) - db_getr(dst,ER_DELTA,NULL);
	if  (Abs(min) > SHC_EPSILON)  {
		*status = SHE_DIFFSAMP;
		return;
	} /*endif*/

	lth1 = db_getl(src,EL_LENGTH,NULL);
	lth2 = db_getl(dst,EL_LENGTH,NULL);
	newlth = lth1 + lth2;
	newptr = sy_allocmem( newlth, (int)sizeof(SAMPLE), status );
	if  (Severe(status))  return;

	/* copy data */
	d = newptr;
	s = (SAMPLE *)db_getp(dst,EP_DATA,NULL);
	endptr = s + lth2;
	while  (s < endptr)
		*d++ = *s++;
	s = (SAMPLE *)db_getp(src,EP_DATA,NULL);
	endptr = s + lth1;
	while  (s < endptr)
		*d++ = *s++;

	/* update info values of destination trace */
	s = (SAMPLE *)db_getp(dst,EP_DATA,NULL);
	db_setl( dst, EL_ALLOC, newlth, NULL );
	db_setl( dst, EL_LENGTH, newlth, NULL );
	db_setp( dst, EP_DATA, newptr, NULL );
	sy_deallocmem( s );
	db_setr( dst, ER_MINVAL,
		(db_getr(src,ER_MINVAL,NULL) < db_getr(dst,ER_MINVAL,NULL)) ?
		db_getr(src,ER_MINVAL,NULL) : db_getr(dst,ER_MINVAL,NULL), NULL );
	db_setr( dst, ER_MAXVAL,
		(db_getr(src,ER_MAXVAL,NULL) > db_getr(dst,ER_MAXVAL,NULL)) ?
		db_getr(src,ER_MAXVAL,NULL) : db_getr(dst,ER_MAXVAL,NULL), NULL );

} /* end of mn4_append */



/*--------------------------------------------------------------------*/



void mn4_merge( PARAM *par, STATUS *status )

/* merges two traces together, inserting zeroes to account for correct
 * timing.
 * par 1:  trace1
 * par 2:  trace2
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *t1, *t2;      /* pointers to the two traces */
	TRACE    *new;          /* new trace */
	TIME     tim1, tim2;    /* start times of traces */
	REAL     dt;            /* sample distance */
	REAL     tdiff;         /* difference time */
	long     difflth;       /* above difference time in samples */
	long     newlth;        /* new length of destination trace in samples */
	long     lth1, lth2;    /* lengths of input traces */
	long     i;             /* counter */
	SAMPLE   *newptr;       /* pointer to new data array */
	SAMPLE   *s, *d;        /* moving pointers */
	SAMPLE   *endptr;       /* pointer to last sample+1 */
	int      cnst1, cnst2;  /* constant counters */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;
	t1 = ml_get1trcpar( par, 1, tc, "   trace1: ", status );
	if  (Severe(status))  return;
	t2 = ml_get1trcpar( par, 2, tc, "   trace2: ", status );
	if  (Severe(status))  return;

	if  (t1 == t2)  {
		*status = SHE_ILPAR;
		return;
	} /*endif*/

	/* check sample rate */
	dt = db_getr( t1, ER_DELTA, NULL );
	tdiff = dt - db_getr(t2,ER_DELTA,NULL);
	if  (Abs(tdiff) > SHC_EPSILON)  {
		*status = SHE_DIFFSAMP;
		return;
	} /*endif*/

	db_gett( t1, ET_START, &tim1, status );
	if  (Severe(status))  return;
	db_gett( t2, ET_START, &tim2, status );
	if  (Severe(status))  return;
	tdiff = tc_adiff( &tim2, &tim1 );

	/* sort by time to have t1 < t2 */
	if  (tdiff < 0.0)  {
		new = t1;
		t1 = t2;
		t2 = new;
		tdiff = (-tdiff);
	} /*endif*/

	difflth = Nlong(tdiff / dt);
	if  (difflth > 10000000)  {
		*status = SHE_SMPOVFL;
		err_setcontext( " ## ridiculous large time difference rejected " );
		return;
	} /*endif*/

	lth1 = db_getl(t1,EL_LENGTH,NULL);
	lth2 = db_getl(t2,EL_LENGTH,NULL);

	if  (lth1 > difflth+lth2)  {
		/* trace 2 is completely contained in trace 1, output is a copy of trc1 */
		newlth = lth1;
	} else {
		/* both traces contribute to the output trace */
		newlth = difflth + lth2;
	} /*endif*/

	newptr = sy_allocmem( newlth, (int)sizeof(SAMPLE), status );
	if  (Severe(status))  return;

	/* copy data */
	d = newptr;
	s = (SAMPLE *)db_getp(t1,EP_DATA,NULL);
	if  (lth1 > difflth+lth2)  {
		if  (GpGetInt(cGpI_debug_level) > 3)
			printf( "SH-dbg4: merge: short trace completely contained\n" );
		/* just copy trace 1 */
		endptr = d + newlth;
		while  (d < endptr)
			*d++ = *s++;
		/* copy short trace into long if less constant values */
		d = (SAMPLE *)db_getp(t1,EP_DATA,NULL) + difflth;
		s = (SAMPLE *)db_getp(t2,EP_DATA,NULL);
		cnst1 = mn4_constnum( d, lth2 );
		cnst2 = mn4_constnum( s, lth2 );
		if  (GpGetInt(cGpI_debug_level) > 3)
			printf( "SH-dbg4: merge: cnst1 %d  cnst2 %d, insert %d\n",
				cnst1, cnst2, (cnst2 < cnst1) );
		if  (cnst2 < cnst1)  {
			d = newptr + difflth;
			endptr = d + lth2;
			while  (d < endptr)
				*d++ = *s++;
		} /*endif*/
	} else if  (difflth <= lth1)  {
		if  (GpGetInt(cGpI_debug_level) > 3)
			printf( "SH-dbg4: merge: traces intersect\n" );
		/* copy part of trace 1 and append trace 2 */
		for  (i=0; i<difflth; i++)
			*d++ = *s++;
		cnst1 = mn4_constnum( s, lth1-difflth );
		cnst2 = mn4_constnum( (SAMPLE *)db_getp(t2,EP_DATA,NULL), lth1-difflth );
		if  (cnst1 > cnst2)  {
			/* take complete second trace */
			s = (SAMPLE *)db_getp(t2,EP_DATA,NULL);
			for  (i=0; i<lth2; i++)
				*d++ = *s++;
		} else {
			/* take complete first trace */
			for  (i=0; i<(lth1-difflth); i++)
				*d++ = *s++;
			s = (SAMPLE *)db_getp(t2,EP_DATA,NULL) + lth1-difflth;
			for  (i=0; i<(lth2-lth1+difflth); i++)  /* !!! */
				*d++ = *s++;
		} /*endif*/
	} else {
		if  (GpGetInt(cGpI_debug_level) > 3)
			printf( "SH-dbg4: merge: traces do not intersect\n" );
		/* copy trace 1 */
		for  (i=0; i<lth1; i++)
			*d++ = *s++;
		/* fill gap with first value of trace 2 */
		s = (SAMPLE *)db_getp(t2,EP_DATA,NULL);
		for  (i=0; i<(difflth-lth1); i++)
			*d++ = *s;
		/* copy trace 2 */
		for  (i=0; i<lth2; i++)
			*d++ = *s++;
	} /*endif*/

	/* setup new trace */
	new = db_create( status );
	if  (Severe(status))  {
		sy_deallocmem( newptr );
		return;
	} /*endif*/
	ml_cpyinfo( t1, new, status );
	if  (Severe(status))  {
		sy_deallocmem( newptr ); db_delete( new );
		return;
	} /*endif*/
	ml_inittrc( new, newptr, newlth, dt );

} /* end of mn4_merge */



/*--------------------------------------------------------------------*/



void mn4_merge_pair( PARAM *par, STATUS *status )

/* returns trace numbers for traces to be merged if any found.  Looks for
 * traces with the same station name and the same comp, chan1 & chan2 entries
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	int      map;                        /* display list number */
	int      trcno;                      /* number of traces on display */
	TRACE    *xtrc;                      /* single trace pointer */
	TRACE    **trc;                      /* trace pointers */
	int      i, j;                       /* counters */
	int      slen;                       /* string length */
	STATUS   locstat;                    /* local status */
	char     streama[cBcLineLth+1];      /* stream name */
	char     streamb[cBcLineLth+1];      /* stream name to compare */
	TSyBoolean found;                    /* pair found */
	char     sym1[cBcLineLth+1];         /* output symbol 1 */
	char     sym2[cBcLineLth+1];         /* output symbol 2 */
	char     value[cBcLineLth+1];        /* symbol value */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;

	/* get variable names */
	cp_getstr( par, 1, tc, "   trace 1 (output): ", cBcLineLth,
		sym1, status );
	if  (Severe(status))  return;
	cp_getstr( par, 2, tc, "   trace 2 (output): ", cBcLineLth,
		sym2, status );
	if  (Severe(status))  return;

	map = gc;
	trcno = 0;
	locstat = cBcNoError;
	found = FALSE;

	xtrc = db_dspfirst( map, &locstat );
	while  (!SySevere(&locstat))  {
		trcno++;
		xtrc = db_getp( xtrc, EP_DSPN, &locstat );
	} /*endwhile*/

	if  (trcno == 0)  {
		if  (*sym1 != '\0')  sl_setsymbol( sym1, "0", status );
		if  (*sym2 != '\0')  sl_setsymbol( sym2, "0", status );
		return;
	} /*endif*/

	trc = sy_allocmem( trcno, (int)sizeof(TRACE *), status );
	if  (SySevere(status))  return;

	trc[0] = db_dspfirst( map, status );
	if  (SySevere(status))  {sy_deallocmem(trc); return;}
	for  (i=1; i<trcno; i++)  {
		trc[i] = db_getp( trc[i-1], EP_DSPN, status );
		if  (SySevere(status))  {sy_deallocmem(trc); return;}
	} /*endfor*/

	for  (i=0; i<trcno; i++)  {

		/* get reference stream name */
		db_gets( trc[i], ES_STATION, cBcLineLth, streama, status );
		if  (SySevere(status))  {sy_deallocmem(trc); return;}
		slen = strlen( streama );
		if  (slen > cBcLineLth-5)  {
			*status = SHE_STROVFL;
			sy_deallocmem( trc );
			return;
		} /*endif*/
		streama[slen++] = '-';
		streama[slen++] = db_getc( trc[i], EC_CHAN1, status );
		if  (SySevere(status))  {sy_deallocmem(trc); return;}
		streama[slen++] = db_getc( trc[i], EC_CHAN2, status );
		if  (SySevere(status))  {sy_deallocmem(trc); return;}
		streama[slen++] = db_getc( trc[i], EC_COMP, status );
		if  (SySevere(status))  {sy_deallocmem(trc); return;}
		streama[slen++] = '\0';
		/*printf( "--> %2d: %s\n", i+1, streama );*/

		/* compare with all traces with higher numbers */
		for  (j=(i+1); j<trcno; j++)  {
			/* get stream name to compare */
			db_gets( trc[j], ES_STATION, cBcLineLth, streamb, status );
			if  (SySevere(status))  {sy_deallocmem(trc); return;}
			slen = strlen( streamb );
			if  (slen > cBcLineLth-5)  {
				*status = SHE_STROVFL;
				sy_deallocmem( trc );
				return;
			} /*endif*/
			streamb[slen++] = '-';
			streamb[slen++] = db_getc( trc[j], EC_CHAN1, status );
			if  (SySevere(status))  {sy_deallocmem(trc); return;}
			streamb[slen++] = db_getc( trc[j], EC_CHAN2, status );
			if  (SySevere(status))  {sy_deallocmem(trc); return;}
			streamb[slen++] = db_getc( trc[j], EC_COMP, status );
			if  (SySevere(status))  {sy_deallocmem(trc); return;}
			streamb[slen++] = '\0';
			if  (strcasecmp(streama,streamb) == 0)  {
				found = TRUE;
				break;
			} /*endif*/
		} /*endfor*/

		if  (found)  break;

	} /*endif*/

	sy_deallocmem( trc );

	if  (!found)  i = j = -1;
	if  (*sym1 != '\0')  {
		sprintf( value, "%d", i+1 );
		sl_setsymbol( sym1, value, status );
		if  (Severe(status))  return;
	} /*endif*/
	if  (*sym2 != '\0')  {
		sprintf( value, "%d", j+1 );
		sl_setsymbol( sym2, value, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (found && GpGetInt(cGpI_debug_level) > 3)
		printf( "SH-dbg4: pair: %d,%d (%s,%s)\n", i+1, j+1, streama, streamb );

} /* end of mn4_merge_pair */


 
/*--------------------------------------------------------------------*/



void mn4_int( PARAM *par, STATUS *status )

/* integrates traces
 * par 1:  trace list
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* integration modes */
#	define MODE_SUM 1
#	define MODE_MEAN 2
#	define MODE_INT 3

	/* local variables */
	char     modestr[BC_LINELTH+1];    /* integration mode */
	int      mode;                     /* integration mode */
	TRACE    *trc[SHC_ILISTLTH];       /* trace list */
	int      listlth;                  /* length of trace list */
	int      t;                        /* counter */
	TRACE    *new;                     /* new trace */
	SAMPLE   *datptr;                  /* pointer to sample array */
	SAMPLE   *olddat;                  /* source data array */
	long     trclth;                   /* length of trace */
	REAL     dt;                       /* sample distance */
	SAMPLE   sum;                      /* sample sum */
	long     i;                        /* sample counter */
	REAL     nrm;                      /* normalization */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;

	if  (sl_quals(par,"MODE",BC_LINELTH,modestr,status))  {
		if  (strcmp(modestr,"SUM") == 0)  {
			mode = MODE_SUM;
		} else if  (strcmp(modestr,"MEAN") == 0)  {
			mode = MODE_MEAN;
		} else if  (strcmp(modestr,"INT") == 0)  {
			mode = MODE_INT;
		} else {
			*status = SHE_ILPAR;
			err_setcontext( " ## key " ); err_setcontext( modestr );
			return;
		} /*endif*/
	} else {
		mode = MODE_SUM;
	} /*endif*/

	db_newlist();

	for  (t=0; t<listlth; t++)  {

		/* get input trace parameters */
		trclth = db_getl( trc[t], EL_LENGTH, NULL );
		dt = db_getr( trc[t], ER_DELTA, NULL );
		olddat = (SAMPLE *)db_getp( trc[t], EP_DATA, NULL );

		/* create new trace */
		new = db_create( status );
		if  (Severe(status))  return;
		datptr = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
		if  (Severe(status))  {db_delete( new );return;}

		/* compute integral */
		switch  (mode)  {
			case MODE_SUM:   nrm = 1.0;  break;
			case MODE_MEAN:  nrm = 1.0/(REAL)trclth;  break;
			case MODE_INT:   nrm = dt;  break;
			default:         nrm = 1.0; break;
		} /*endswitch*/
		sum = 0.;
		for  (i=0; i<trclth; i++)  {
			sum += olddat[i];
			datptr[i] = sum*nrm;
		} /*endfor*/

		/* setup new trace */
		ml_cpyinfo( trc[t], new, status );
		if  (Severe(status))  {
			sy_deallocmem( datptr ); db_delete( new );
			return;
		} /*endif*/
		ml_inittrc( new, datptr, trclth, dt );

	} /*endfor*/

} /* end of mn4_int */



/*--------------------------------------------------------------------*/



void mn4_derive( PARAM *par, STATUS *status )

/* computes derivatives of traces
 * par 1:  trace list
 * par 2:  algorithm ID
 *           1: y[i] = 1/(2*h) (-y[i-1] + y[i+1])
 *           2: y[i] = 1/(12*h) (y[i-2] - 8*y[i-1] + 8y[i+1] - y[i+2])
 *           3: y[i] = 1/h (-y[i] + y[i+1])
 *           4: y[i] = 1/(2*h) (-3*y[i] + 4*y[i+1] - y[i+2])
 *           5: y[i] = 1/(12*h) (-3*y[i-1] - 10*y[i] + 18*y[i+1] -
 *                         6*y[i+2] + y[i+3])
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];       /* trace list */
	int      listlth;                  /* length of trace list */
	int      mode;                     /* algorithm ID */
	int      t;                        /* counter */
	TRACE    *new;                     /* new trace */
	SAMPLE   *datptr;                  /* pointer to sample array */
	SAMPLE   *y;                       /* source data array */
	long     trclth;                   /* length of trace */
	REAL     dt;                       /* sample distance */
	REAL     norm;                     /* normalisation */
	long     i;                        /* sample counter */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	cp_getint( par, 2, tc, "   algorithm ID: ", &mode, status );
	if  (Severe(status))  return;

	db_newlist();

	for  (t=0; t<listlth; t++)  {

		/* get input trace parameters */
		trclth = db_getl( trc[t], EL_LENGTH, NULL );
		dt = db_getr( trc[t], ER_DELTA, NULL );
		y = (SAMPLE *)db_getp( trc[t], EP_DATA, NULL );
		if  (trclth < 4)  {
			*status = SHE_SPECERROR+19;
			return;
		} /*endif*/

		/* create new trace */
		new = db_create( status );
		if  (Severe(status))  return;
		datptr = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
		if  (Severe(status))  {db_delete( new );return;}

		/* compute derivative */
		switch  (mode)  {
		case 1:  /* y[i] = 1/(2*h) (-y[i-1] + y[i+1]) */
			norm = 1.0 / (2.0*dt);
			datptr[0] = norm * y[1];
			for  (i=1; i<trclth-1; i++)
				datptr[i] = norm * (y[i+1] - y[i-1]);
			datptr[trclth-1] = -norm * y[trclth-2];
			break;
		case 2:  /* y[i]=1/(12h)*(y[i-2]-8y[i-1]+8y[i+1]-y[i+2]) */
			norm = 1.0 / (12.0*dt);
			datptr[0] = norm * (8.0*y[1] - y[2]);
			datptr[1] = norm * (-8.0*y[0] + 8.0*y[2] - y[3]);
			for  (i=2; i<trclth-2; i++)
				datptr[i] = norm * (y[i-2]-8.*y[i-1]+8.*y[i+1]-y[i+2]);
			datptr[trclth-2] = norm * (y[trclth-4] - 8.*y[trclth-3] +
				8.*y[trclth-1]);
			datptr[trclth-1] = norm * (y[trclth-3] - 8.*y[trclth-2]);
			break;
		case 3:  /* y[i] = 1/h (-y[i] + y[i+1]) */
			norm = 1./dt;
			for  (i=0; i<trclth-1; i++)
				datptr[i] = norm * (y[i+1] - y[i]);
			datptr[trclth-1] = -norm * y[trclth-1];
			break;
		case 4:  /* 1/(2*h) (-3*y[i] + 4*y[i+1] - y[i+2]) */
			norm = 1. / (2.*dt);
			for  (i=0; i<trclth-2; i++)
				datptr[i] = norm * (-3.*y[i] + 4.*y[i+1] - y[i+2]);
			datptr[trclth-2] = norm * (-3.*y[trclth-2] + 4.*y[trclth-1]);
			datptr[trclth-1] = -norm * 3.*y[trclth-1];
			break;
		case 5:  /* 1/(12*h)(-3y[i-1]-10y[i]+18y[i+1]-6y[i+2]+y[i+3]) */
			norm = 1. / (12.*dt);
			datptr[0] = norm * (-10.*y[0] + 18.*y[1] - 6.*y[2] + y[3]);
			for  (i=1; i<trclth-3; i++)
				datptr[i] = norm * (-3.*y[i-1] - 10.*y[i] + 18.*y[i+1] -
					6.*y[i+2] + y[i+3]);
			datptr[trclth-3] = norm * (-3.*y[trclth-4] - 10.*y[trclth-3] +
				18.*y[trclth-2] - 6.*y[trclth-1]);
			datptr[trclth-2] = norm * (-3.*y[trclth-3] - 10.*y[trclth-2] +
				18.*y[trclth-1]);
			datptr[trclth-1] = norm * (-3.*y[trclth-2] - 10.*y[trclth-1]);
			break;
		default:
			sy_deallocmem( datptr );
			db_delete( new );
			*status = SHE_ILPAR;
			err_setcontext( " ## deriv type " ); err_setcontext_l( mode );
			return;
		} /*endswitch*/

		/* setup new trace */
		ml_cpyinfo( trc[t], new, status );
		if  (Severe(status))  {
			sy_deallocmem( datptr ); db_delete( new );
			return;
		} /*endif*/
		ml_inittrc( new, datptr, trclth, dt );

	} /*endfor*/

} /* end of mn4_derive */



/*--------------------------------------------------------------------*/



void mn4_demean( PARAM *par, STATUS *status )

/* removes mean value from list of traces
 * par 1:  trace list
 * par 2:  lo time
 * par 3:  hi time
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];   /* trace list */
	int      listlth;              /* length of trace list */
	REAL     lowdw, hiwdw;         /* time window bounds */
	int      t;                    /* trace counter */
	long     loidx, hiidx;         /* index window */
	long     i;                    /* counter */
	long     trclth;               /* length of full trace */
	SAMPLE   sum;                  /* sample sum */
	SAMPLE   *s;                   /* sample pointer */
	char     str[BC_SHORTSTRLTH+1];/* scratch string */

	/* executable code */

	if  (cp_pnexc(par,3,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	if  (cp_pentered(par,2,status))  {
		ml_windowpars( par, 2, tc, "   lower bound: ", "   upper bound: ",
			&lowdw, &hiwdw, status );
		if  (Severe(status))  return;
	} else {
		if  (Severe(status))  return;
		dm_dspcoo( &lowdw, NULL, &hiwdw, NULL );
		hiwdw += lowdw;
	} /*endif*/

	for  (t=0; t<listlth; t++)  {
		loidx = dm_getsample( trc[t], lowdw, TRUE );
		hiidx = dm_getsample( trc[t], hiwdw, TRUE );
		if  (hiidx > loidx)  {
			s = (SAMPLE *)db_getp( trc[t], EP_DATA, NULL );
			sum = 0.;
			for  (i=loidx; i<=hiidx; sum += s[i++] )  {}
			sum /= (float)(hiidx-loidx+1);
			trclth = db_getl( trc[t], EL_LENGTH, NULL );
			for  (i=0; i<trclth; s[i++] -= sum )  {}
			db_setr( trc[t], ER_MINVAL,
				db_getr(trc[t],ER_MINVAL,NULL)-sum, NULL );
			db_setr( trc[t], ER_MAXVAL,
				db_getr(trc[t],ER_MAXVAL,NULL)-sum, NULL );
			sprintf( str, "%e", sum );
			db_sets( trc[t], ES_OPINFO, str, status );
			if  (Severe(status))  return;
		} /*endif*/
	} /*endfor*/

} /* end of mn4_demean */



/*--------------------------------------------------------------------*/



void mn4_polfil( PARAM *par, STATUS *status )

/* polarisation filter
 * par 1:  trace list
 * par 2:  coherence length
 * par 3:  lo time
 * par 4:  hi time
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];    /* trace list */
	int      listlth;               /* length of trace list */
	REAL     cohlth;                /* coherence length */
	REAL     power;                 /* power of factor */
	REAL     lowdw, hiwdw;          /* time window in sec */
	long     start[3];              /* start indices */
	long     end[3];                /* end indices */
	int      t, j;                  /* trace counter */
	long     trclth;                /* length of output traces */
	SAMPLE   *idat[3];              /* input data */
	SAMPLE   *odat[3];              /* output data */
	TRACE    *new[3];               /* output traces */
	REAL     dt;                    /* sample distance in sec */
	int      cmreset;               /* cm recomputed after cmreset steps */
	int      inplev;                /* input level */
	BOOLEAN  do_vec;                /* vector/linearity filter */
	SAMPLE   poldir[3];             /* wanted polarisation direction (Z,N,E) */
	BOOLEAN  do_poldir;             /* use given polarization direction vector */
	BOOLEAN  do_bazdir;             /* use given backazimuth */
	BOOLEAN  do_eigen;              /* return weighted eigenvector */
	REAL     baz;                   /* back azimuth in deg */
	char     tmpstr[BC_LINELTH+1];  /* direction vector expression */

	/* executable code */

	if  (cp_pnexc(par,4,status))  return;
	if  (cp_qexist(par,"PARENT"))  {
		inplev = -1;
	} else if  (cp_qexist(par,"INTERACTIVE"))  {
		inplev = shv_maininput;
	} else {
		inplev = ss_inplev();
	} /*endif*/

	do_vec = cp_qexist( par, "VECTOR" );
	do_eigen = cp_qexist( par, "EIGEN" );
	do_poldir = sl_quals( par, "POLVEC", BC_LINELTH, tmpstr, status );
	if  (do_poldir)  {
		if  (sscanf(tmpstr,"%f,%f,%f",poldir,poldir+1,poldir+2) != 3)  {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} /*endif*/
	if  (Severe(status))  return;
	do_bazdir = sl_quals( par, "BAZDIR", BC_LINELTH, tmpstr, status );
	if  (do_bazdir)  {
		if  (sscanf(tmpstr,"%f",&baz) != 1)  {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} /*endif*/
	if  (Severe(status))  return;

	ml_gettrcpar( par, 1, tc, "   Z,N,E traces: ", trc, &listlth, status);
	if  (Severe(status))  return;
	if  (listlth != 3)  {
		*status = SHE_ILPAR;
		err_setcontext( " ## list length is " ); err_setcontext_l( listlth );
		return;
	} /*endif*/
	if  (!do_poldir && !do_bazdir)
		cp_getfloat( par, 2, tc, "   coherence length (sec): ", &cohlth, status );
	if  (Severe(status))  return;
	if  (cp_pentered(par,3,status))  {
		ui_shift( inplev );
		ml_windowpars( par, 3, tc, "   lower bound: ", "   upper bound: ",
			&lowdw, &hiwdw, status );
		ui_unshift();
		if  (Severe(status))  return;
	} else {
		if  (Severe(status))  return;
		dm_dspcoo( &lowdw, NULL, &hiwdw, NULL );
		hiwdw += lowdw;
	} /*endif*/

	if  (!sl_qualr(par,"POWER",&power,status))  {
		if  (Severe(status))  return;
		power = 1.;
	} /*endif*/
	if  (!sl_quali(par,"CMRESET",&cmreset,status))  {
		if  (Severe(status))  return;
		cmreset = 100;
	} /*endif*/

	dt = db_getr( trc[0], ER_DELTA, NULL );
	for  (t=0; t<3; t++)  {
		start[t] = dm_sgetsample( trc[t], lowdw, status );
		if  (Severe(status))  return;
		end[t] = dm_getsample( trc[t], hiwdw, TRUE );
		if  (t == 0)  {
			trclth = *end - *start + 1;
		} else {
			if  ((end[t]-start[t]+1) < trclth)
				trclth = end[t] - start[t] + 1;
		} /*endif*/
		idat[t] = (SAMPLE *)db_getp(trc[t],EP_DATA,NULL) + start[t];
		if  (db_getr(trc[t],ER_DELTA,NULL) != dt)  {
			*status = SHE_DIFFSAMP;
			return;
		} /*endif*/
	} /*endfor*/

	/* allocate new traces */
	db_newlist();
	for  (t=0; t<3; t++)  {
		new[t] = db_create( status );
		if  (Severe(status))  {
			for  (j=0; j<t; j++)  {
				sy_deallocmem( odat[j] );
				db_delete( new[j] );
			} /*endfor*/
			return;
		} /*endif*/
		odat[t] = sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
		if  (Severe(status))  {
			for  (j=0; j<t; j++)  {
				sy_deallocmem( odat[j] );
				db_delete( new[j] );
			} /*endfor*/
			db_delete( new[t] );
			return;
		} /*endif*/
	} /*endfor*/

	if  (do_poldir)  {
		pf_poldir( idat[0], idat[1], idat[2], odat[0], odat[1],
			odat[2], trclth, poldir, power );
	} else if  (do_bazdir)  {
		pf_bazdir( idat[0], idat[1], idat[2], odat[0], odat[1],
			odat[2], trclth, baz, power );
	} else if  (do_vec)  {
		pf_vecfilter( idat[0], idat[1], idat[2], odat[0], odat[1],
			odat[2], trclth, Nint(cohlth/dt), power, cmreset, status );
	} else if  (do_eigen)  {
		pf_eigenvector( idat[0], idat[1], idat[2], odat[0], odat[1],
			odat[2], trclth, Nint(cohlth/dt), power, cmreset, status );
	} else {
		pf_filter( idat[0], idat[1], idat[2], odat[0], odat[1],
			odat[2], trclth, Nint(cohlth/dt), power, cmreset );
	} /*endif*/

	for  (t=0; t<3; t++)  {
		ml_cpyinfo( trc[t], new[t], status );
		if  (Severe(status))  {
			sy_deallocmem( odat[t] ); db_delete( new[t] );
			return;
		} /*endif*/
		ml_inittrc( new[t], odat[t], trclth, dt );
		if  (lowdw > db_getr(trc[t],ER_TORIG,NULL))
			db_setr( new[t], ER_TORIG, lowdw, NULL );
		db_setf( new[t], EF_MODIF, TRUE, NULL );
		ml_newstarttime( trc[t], new[t], lowdw, status );
		if  (Severe(status))  {
			sy_deallocmem( odat[t] );
			db_delist( gc, new[t] );
			db_delete( new[t] );
			return;
		} /*endif*/
	} /*endfor*/

} /* end of mn4_polfil */



/*--------------------------------------------------------------------*/



void mn4_spectrum( PARAM *par, STATUS *status )

/* computes power spectrum
 * par 1:  trace list
 * par 2:  order of approximation (number of poles)
 * par 3:  lo time
 * par 4:  hi time
 * par 5:  lo frq bound
 * par 6:  hi frq bound
 * par 7:  frq step
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];     /* trace list */
	int      listlth;                /* list length */
	int      order;                  /* order of approximation */
	REAL     lotime, hitime;         /* time window of input traces */
	REAL     lofrq, hifrq;           /* frequency window of output */
	double   lofrqd, hifrqd;         /* double precision bounds */
	long     frqsteps;               /* number of frequency steps */
	REAL     dfrq;                   /* delta frq */
	int      t;                      /* trace counter */
	long     loidx, trclth;          /* current sample window */
	SAMPLE   *tptr;                  /* pointer to data array */
	float    *coeff;                 /* MEM coefficients */
	float    coeff0;                 /* numerator */
	SAMPLE   *fptr;                  /* pointer to frq data */
	TRACE    *ftrc;                  /* output trace */
	long     i;                      /* sample counter */
	SAMPLE   *fp;                    /* moving pointer */
	REAL     dt;                     /* current sample distance */
	REAL     cf;                     /* current frequency */
	REAL     tmp;                    /* scratch */
	double   cfd, dfd;               /* current frq, increment factor */
	BOOLEAN  oo_nyquist;             /* out of nyquist range */
	BOOLEAN  logdisp;                /* logarithmic display */

	/* executable code */

	if  (cp_pnexc(par,7,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	cp_getint( par, 2, tc, "   order of approximation: ", &order, status );
	if  (Severe(status))  return;
	if  (cp_pentered(par,3,status))  {
		ml_windowpars( par, 3, tc, "   lower bound: ", "   upper bound: ",
			&lotime, &hitime, status );
		if  (Severe(status))  return;
	} else {
		dm_dspcoo( &lotime, NULL, &hitime, NULL );
		hitime += lotime;
	} /*endif*/
	cp_getfloat( par, 5, tc, "   lower frq bound: ", &lofrq, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 6, tc, "   upper frq bound: ", &hifrq, status );
	if  (Severe(status))  return;
	cp_getlong( par, 7, tc, "   # of frq steps: ", &frqsteps, status );
	if  (Severe(status))  return;
	if  (frqsteps <= 1)  {*status = SHE_ILPAR; err_setcontext( " ## too less frq steps" ); return;}
	logdisp = cp_qexist( par, "LOG" );
	if  ((lofrq >= hifrq) || (logdisp && lofrq <= 0.))  {
		*status = SHE_ILPAR;
		err_setcontext( " ## frq boundaries " );
		err_setcontext_r( lofrq ); err_setcontext_r( hifrq );
		return;
	} /*endif*/

	/* get memory for MEM coefficients */
	coeff = (float *)sy_allocmem( (long)order, (int)sizeof(float), status );
	coeff--;
	if  (Severe(status))  return;

	if  (logdisp)  {
		/* find new frq bounds */
		t = Nint( log10(lofrq) );
		lofrqd = pow( 10., (double)t );
		t = Nint( log10(hifrq) );
		hifrqd = pow( 10., (double)t );
		t = Nint( log10(hifrqd/lofrqd) );
		i = Nlong((float)frqsteps/(float)t) * t;
		if  (i < frqsteps)  i += t;
		frqsteps = i;
		dfd = pow( 10.0, 1.0/(double)(frqsteps/t) );
		dfrq = 1.0/(double)(frqsteps/t);
	} else {
		dfrq = (hifrq-lofrq)/(float)(frqsteps-1);
	} /*endif*/
	db_newlist();

	for  (t=0; t<listlth; t++)  {
		/* compute sample window and get input trace info's */
		loidx = dm_getsample( trc[t], lotime, TRUE );
		trclth = dm_getsample( trc[t], hitime, TRUE );
		trclth -= loidx - 1;
		tptr = (SAMPLE *)db_getp( trc[t], EP_DATA, NULL ) + loidx;
		if  (trclth > SYC_MAXINT)  {
			*status = SHE_SMPOVFL;
			return;
		} /*endif*/
		dt = db_getr( trc[t], ER_DELTA, NULL );
		/* compute MEM coefficients */
		nr_memcof( tptr-1, (int)trclth, order, &coeff0, coeff );
		/* create output trace */
		ftrc = db_create( status );
		if  (Severe(status))  {sy_deallocmem(coeff+1); return;}
		fptr = (SAMPLE *)sy_allocmem( frqsteps, (int)sizeof(SAMPLE), status );
		if  (Severe(status))  {
			db_delete( ftrc );
			sy_deallocmem( coeff+1 );
			return;
		} /*endif*/
		fp = fptr;
		oo_nyquist = FALSE;
		if  (logdisp)  {
			cfd = lofrqd;
			for  (i=0; i<frqsteps; i++)  {
				tmp = cfd*dt;
				if  (Abs(tmp) > 0.501 /*rounding error 0.001*/)  oo_nyquist = TRUE;
				*fp++ = nr_evlmem( tmp, coeff, order, coeff0 );
				cfd *= dfd;
			} /*endfor*/
		} else {
			cf = lofrq;
			for  (i=0; i<frqsteps; i++)  {
				tmp = cf*dt;
				if  (Abs(tmp) > 0.501 /*rounding error 0.001*/)  oo_nyquist = TRUE;
				*fp++ = nr_evlmem( tmp, coeff, order, coeff0 );
				cf += dfrq;
			} /*endfor*/
		} /*endif*/
		if  (oo_nyquist)
			gc_write( cc, "*** spectrum out of nyquist range ***\n" );
		ml_cpyinfo( trc[t], ftrc, status );
		if  (Severe(status))  {
			db_delete( ftrc );
			sy_deallocmem( coeff+1 );
			sy_deallocmem( fptr );
			return;
		} /*endif*/
		/* normalization */
		tmp = dfrq*dt*(float)trclth*(float)trclth/2.0;
		for  (i=0; i<frqsteps; i++)  {
			fptr[i] *= tmp;
			if  (fptr[i] >= 0.0)  {
				fptr[i] = sqrt( fptr[i] );
			} else {
				fptr[i] = 0.0;
			} /*endif*/
		} /*endfor*/
		ml_inittrc( ftrc, fptr, frqsteps, dfrq );
		if  (logdisp)  {
			db_setr( ftrc, ER_TORIG, log10(lofrqd), NULL );
		} else {
			db_setr( ftrc, ER_TORIG, lofrq, NULL );
		} /*endif*/
	} /*endfor*/

	sy_deallocmem( coeff+1 );

} /* end of mn4_spectrum */



/*--------------------------------------------------------------------*/



void mn4_fft( PARAM *par, STATUS *status )

/* computes power spectrum with FFT
 * par 1:  trace list
 * par 2:  lowdw
 * par 3:  hiwdw
 * par 4:  k
 * par 5:  m
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];   /* trace list */
	int      listlth;              /* length of list */
	int      k, m;                 /* FFT parameter */
	BOOLEAN  overlap;              /* overlap windows */
	long     fftlth;               /* length of complete FFT trace */
	REAL     lowdw, hiwdw;         /* time window */
	long     start, trclth;        /* sample window */
	int      t;                    /* trace counter */
	SAMPLE   *smp;                 /* sample pointer */
	SAMPLE   *a, *b;               /* moving pointers */
	long     i;                    /* sample counter */
	TRACE    *new;                 /* pointer to new trace */
	SAMPLE   *datptr;              /* pointer to new data array */
	REAL     dfrq;                 /* frequency sampling distance (Hz) */
	char     str[BC_LINELTH+1];    /* scratch string */
	REAL     tmp, dt;

	/* executable code */

	if  (cp_pnexc(par,5,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	if  (cp_pentered(par,2,status))  {
		ml_windowpars( par, 2, tc, "   lower bound: ", "   upper bound: ",
			&lowdw, &hiwdw, status );
		if  (Severe(status))  return;
	} else {
		dm_dspcoo( &lowdw, NULL, &hiwdw, NULL );
		hiwdw += lowdw;
	} /*endif*/
	cp_getint( par, 4, tc, "   number of windows: ", &k, status );
	if  (Severe(status))  return;
	if  (cp_pentered(par,5,status))  {
		cp_getint( par, 5, tc, "   window length (power of 2): ", &m, status );
		if  (Severe(status))  return;
		/* check power of 2 */
		t = 1; i = 0;
		while  (i == 0 && t != 0)  {
			if  (t == m)  i = 1;
			t <<= 1;
		} /*endwhile*/
		if  (i == 0)  {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
	} else {
		m = 0;
	} /*endif*/

	overlap = cp_qexist( par, "OVERLAP" );
	fftlth = (overlap) ? (2*k+1)*m : 4*k*m;

	/* loop over traces */
	db_newlist();
	for  (t=0; t<listlth; t++)  {

		dt = db_getr( trc[t], ER_DELTA, NULL );
		/* get input trace window */
		start = dm_getsample( trc[t], lowdw, TRUE );
		trclth = dm_getsample( trc[t], hiwdw, TRUE ) - start + 1;
		/* determine m automatically if not specified */
		if  (m == 0)  {
			i = (overlap) ? trclth/(2*k+1) : trclth/(4*k);
			if  (i > SYC_MAXINT)  {*status = SHE_SMPOVFL; return;}
			m = (int)i;
			i = 1;
			while  (i < m)  i <<= 1;
			if  (i > SYC_MAXINT)  {*status = SHE_SMPOVFL; return;}
			m = (int)i;
			if  (shflags_shv & SHF_CHATTY)  {
				sprintf( str, "%s window length is %d\n", SHC_CHATTXT, m );
				gc_write( cc, str );
			} /*endif*/
			fftlth = (overlap) ? (2*k+1)*m : 4*k*m;
		} /*endif*/
		/* copy input trace if too short */
		if  (fftlth > trclth)  {
			smp = (SAMPLE *)sy_allocmem( fftlth, (int)sizeof(SAMPLE), status );
			if  (Severe(status))  return;
			a = (SAMPLE *)db_getp(trc[t],EP_DATA,NULL) + start;
			b = smp;
			for  (i=0; i<trclth; i++)
				*b++ = *a++;
			for  (i=trclth; i<fftlth; i++)
				*b++ = 0.;
			if  (shflags_shv & SHF_CHATTY)  {
				sprintf( str, "%s %ld zeroes padded\n", SHC_CHATTXT,
					fftlth-trclth );
				gc_write( cc, str );
			} /*endif*/
		} else {
			smp = (SAMPLE *)db_getp(trc[t],EP_DATA,NULL) + start;
		} /*endif*/

		/* create new trace */
		new = db_create( status );
		if  (Severe(status))  {
			if  (fftlth > trclth)  sy_deallocmem( smp );
			return;
		} /*endif*/
		datptr = sy_allocmem( m, (int)sizeof(SAMPLE), status );
		if  (Severe(status))  {
			if  (fftlth > trclth)  sy_deallocmem( smp );
			db_delete( new );
			return;
		} /*endif*/

		/* compute FFT */
		nr_spctrm( smp-1, datptr-1, m, k, overlap );
		/*         ^--- this is ok, I made it compatible to other     */ 
		/*              array arguments                               */
		/*              originally this data array was read from file */

#ifdef XXX
		/* normalization */
		tmp = (float)trclth;
		for  (i=0; i<m; i++)  {
			datptr[i] *= tmp;
			if  (datptr[i] >= 0)  {
				datptr[i] = sqrt( datptr[i] ) * dt;
			} else {
				datptr[i] = 0.0;
			} /*endif*/
		} /*endfor*/
#endif

		/* initialise trace parameters */
		ml_cpyinfo( trc[t], new, status );
		if  (Severe(status))  {
			db_delete( new );
			sy_deallocmem( datptr );
			if  (fftlth > trclth)  sy_deallocmem( smp );
			return;
		} /*endif*/
		dfrq = 1./(REAL)(2*m) / db_getr(trc[t],ER_DELTA,NULL);
		ml_inittrc( new, datptr, m, dfrq );

		if  (fftlth > trclth)
			sy_deallocmem( smp );
	} /*endfor*/

} /* end of mn4_fft */



/*--------------------------------------------------------------------*/



void mn4_spectrogram( PARAM *par, STATUS *status )

/* computes power spectrum with FFT
 * par 1:  trace list
 * par 2:  lowdw
 * par 3:  hiwdw
 * par 4:  specwidth
 * par 5:  specstep
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc;                   /* trace pointer */
	int      spg_length;             /* length of each fft trace */
	int      spg_step;               /* step size in samples */
	int      spglines;               /* number of spectrogram lines */
	int      fftlth;                 /* length of complete FFT trace */
	REAL     lowdw, hiwdw;           /* time window */
	long     start, trclth;          /* sample window */
	int      i, j;                   /* counters */
	char     fname[cBcFileLth+1];    /* name of output file */
	char     spgtime[cBcTimeLth+1];  /* start time of spectrogram */
	TIME     atime;                  /* absolute start time of trace */
	REAL     dt;                     /* sample distance */
	REAL     df;                     /* frequency sampling */
	float    *ftmp;                  /* pointer to temporary array */
	float    *dsrc;                  /* pointer to FFT input data */
	FILE     *fp;                    /* pointer to file */
	SAMPLE   *datptr;                /* pointer to data */
	char     infoline[cBcLineLth+1]; /* station info */
	TSyStatus locstat;               /* local status */

	/* executable code */

	if  (cp_pnexc(par,6,status))  return;
	trc = ml_get1trcpar( par, 1, tc, "   source trace: ", status );
	if  (Severe(status))  return;
	if  (cp_pentered(par,2,status))  {
		ml_windowpars( par, 2, tc, "   lower bound: ", "   upper bound: ",
			&lowdw, &hiwdw, status );
		if  (Severe(status))  return;
	} else {
		dm_dspcoo( &lowdw, NULL, &hiwdw, NULL );
		hiwdw += lowdw;
	} /*endif*/
	cp_getint( par, 4, tc, "   spectrogram width (in samples, power of 2): ",
		&spg_length, status );
	if  (Severe(status))  return;
	cp_getint( par, 5, tc, "   step size (samples): ", &spg_step, status );
	if  (Severe(status))  return;
	cp_getstr( par, 6, tc, "   output file: ", cBcFileLth, fname, status );
	if  (Severe(status))  return;

	/* check power of 2 */
	j = 1; i = 0;
	while  (i == 0 && j != 0)  {
		if  (j == spg_length)  i = 1;
		j <<= 1;
	} /*endwhile*/
	if  (i == 0)  {
		*status = SHE_ILPAR;
		return;
	} /*endif*/

	/* get start time of trace */
	db_gett( trc, ET_START, &atime, status );
	if  (Severe(status))  return;
	tc_aadd( &atime, (float)(spg_length/2)*dt, &atime );
	tc_a2t( &atime, spgtime, status );
	if  (Severe(status))  return;

	dt = db_getr( trc, ER_DELTA, NULL );
	/* get input trace window */
	start = dm_getsample( trc, lowdw, TRUE );
	trclth = dm_getsample( trc, hiwdw, TRUE ) - start + 1;

	spglines = ((trclth-spg_length)/spg_step)+1;
	datptr = (SAMPLE *)db_getp( trc, EP_DATA, NULL ) + start;

	if  (trclth <= spg_length)  {
		*status = SHE_ILPAR;
		return;
	} /*endif*/

	/* allocate memory for FFT */
	ftmp = (float *)sy_allocmem( spg_length, (int)sizeof(float), status );
	if  (SySevere(status))  return;

	fftlth = spg_length / 2;
	df = 1./(float)(spg_length) / dt;

	fp = fopen( fname, "w" );
	if  (fp == NULL)  {
		*status = SHE_OPNWR;
		return;
	} /*endif*/

	locstat = cBcNoError;
	db_gets( trc, ES_STATION, cBcLineLth, infoline, &locstat );
	if  (locstat != cBcNoError)  strcpy( infoline, "XXX" );
	i = strlen( infoline );
	if  (i < cBcLineLth-5)  {
		infoline[i++] = '-';
		locstat = cBcNoError;
		infoline[i] = db_getc( trc, EC_CHAN1, &locstat );
		if  (locstat == cBcNoError)  i++;
		locstat = cBcNoError;
		infoline[i] = db_getc( trc, EC_CHAN2, &locstat );
		if  (locstat == cBcNoError)  i++;
		infoline[i++] = '-';
		locstat = cBcNoError;
		infoline[i] = db_getc( trc, EC_COMP, &locstat );
		if  (locstat == cBcNoError)  i++;
		infoline[i++] = '\0';
	} /*endif*/

	fprintf( fp, "! spectrogram file created by SHM\n" );
	fprintf( fp, "!* %s\n", infoline );
	fprintf( fp, "!  DELTA:   %g\n", dt*spg_step );
	fprintf( fp, "!  START:   %s\n", spgtime );
	fprintf( fp, "!  LENGTH:  %d\n", fftlth );
	fprintf( fp, "!  LINES:   %d\n", spglines );
	fprintf( fp, "!  DF:      %f\n", df );
	fprintf( fp, "\n" );

	dsrc = datptr;
	for  (i=0; i<spglines; i++)  {
		if  (dsrc+spg_length > datptr+trclth)
			printf( "--> bug in spc_compute_spectrogram (%d)\n", i );
		for  (j=0; j<spg_length; j++)
			ftmp[j] = dsrc[j] * mn4_taper( j, spg_length );
		nr_realft( ftmp-1, spg_length/2, 1 );
		fprintf( fp, "%e\n", ftmp[0] );
		for  (j=1; j<fftlth; j++)
			fprintf( fp, "%e\n",
				sqrt( ftmp[2*j]*ftmp[2*j] + ftmp[2*j+1]*ftmp[2*j+1] ) );
		fprintf( fp, "\n" );
		dsrc += spg_step;
	} /*endfor*/

	sy_deallocmem( ftmp );
	fclose( fp );

} /* end of mn4_spectrogram */



/*--------------------------------------------------------------------*/



void mn4_connect( PARAM *par, STATUS *status )

/* connects two traces
 * par 1:  subfunction:  MUL, DIV
 * par 2:  first input trace
 * par 3:  second input trace
 * par 4:  lowdw
 * par 5:  hiwdw
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	char     subfct[BC_LINELTH+1]; /* subfunction */
	TRACE    *t1, *t2;             /* input traces */
	TRACE    *new;                 /* output trace */
	SAMPLE   *datptr;              /* pointer to output data */
	REAL     lowdw, hiwdw;         /* window in seconds */
	SAMPLE   *s1, *s2;             /* start samples of input traces */
	long     trclth;               /* length of output trace */
	long     firstsample;          /* index of first sample point */
	long     end;                  /* last sample + 1 */

	/* executable code */

	if  (cp_pnexc(par,5,status))  return;
	cp_getstr( par, 1, tc, "   subfunction: ", BC_LINELTH, subfct, status );
	if  (Severe(status))  return;
	if  (*subfct != 'D' && *subfct != 'M')  {
		*status = SHE_ILPAR;
		err_setcontext( " ## key " ); err_setcontext( subfct );
		return;
	} /*endif*/
	t1 = ml_get1trcpar( par, 2, tc, "   first trace: ", status );
	if  (Severe(status))  return;
	t2 = ml_get1trcpar( par, 3, tc, "   second trace: ", status );
	if  (Severe(status))  return;

	if  (db_getr(t1,ER_DELTA,NULL) != db_getr(t2,ER_DELTA,NULL))  {
		*status = SHE_DIFFSAMP;
		return;
	} /*endif*/

	dm_dspcoo( &lowdw, NULL, &hiwdw, NULL );
	hiwdw += lowdw;
	if  (cp_pentered(par,4,status))
		cp_getfloat( par, 4, tc, "   lo-wdw: ", &lowdw, status );
	if  (Severe(status))  return;
	if  (cp_pentered(par,5,status))
		cp_getfloat( par, 5, tc, "   hi-wdw: ", &hiwdw, status );
	if  (Severe(status))  return;

	if  (db_getr(t1,ER_TORIG,NULL) > lowdw)
		lowdw = db_getr( t1, ER_TORIG, NULL );
	if  (db_getr(t2,ER_TORIG,NULL) > lowdw)
		lowdw = db_getr( t2, ER_TORIG, NULL );

	/* compute trace window */
	firstsample = dm_sgetsample( t1, lowdw, status );
	if  (Severe(status))  return;
	s1 = (SAMPLE *)db_getp( t1, EP_DATA, NULL ) + firstsample;
	end = dm_getsample( t1, hiwdw, TRUE );
	trclth = end - firstsample + 1;
	firstsample = dm_sgetsample( t2, lowdw, status );
	if  (Severe(status))  return;
	s2 = (SAMPLE *)db_getp( t2, EP_DATA, NULL ) + firstsample;
	end = dm_getsample( t2, hiwdw, TRUE );
	if  ((end-firstsample+1) < trclth)
		trclth = end - firstsample + 1;

	/* create new trace */
	db_newlist();
	new = db_create( status );
	if  (Severe(status))  return;
	datptr = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
	if  (Severe(status))  {db_delete(new); return;}

	/* perform multiplication or division */
	if  (*subfct == 'D')  {
		mt_divide_array( s1, s2, trclth, datptr );
	} else {
		mt_multiply_array( s1, s2, trclth, datptr );
	} /*endif*/

	/* set trace info */
	ml_cpyinfo( t1, new, status );
	if  (*status == SHE_NOERROR)  db_sets(new,ES_STATION,"MUL",status);
	if  (*status == SHE_NOERROR)
		ml_inittrc( new, datptr, trclth, db_getr(t1,ER_DELTA,NULL) );
	if  (*status == SHE_NOERROR)  ml_newstarttime( t1, new, lowdw, status );
	if  (Severe(status))  {
		sy_deallocmem( datptr );
		db_delist( gc, new );
		db_delete( new );
		return;
	} /*endif*/
	if  (lowdw > db_getr(t1,ER_TORIG,NULL))
		db_setr( new, ER_TORIG, lowdw, NULL );

} /* end of mn4_connect */



/*--------------------------------------------------------------------*/



void mn4_trend( PARAM *par, STATUS *status )

/* removes trend in traces
 * par 1:  trace list
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];  /* trace list */
	int      listlth;             /* length of list */
	int      t;                   /* trace counter */
	SAMPLE   *datptr;             /* pointer to sample data */
	REAL     *time;               /* pointer to x-coordinates */
	long     trclth;              /* length of current trace */
	REAL     m, b;                /* line parameters y=mx+b */
	long     i;                   /* sample counter */
	REAL     siga, sigb, chi2, q; /* not used */
	SAMPLE   min, max;            /* new minimum and maximum of trace */
	REAL     dt;                  /* sample distance of current trace */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;

	for  (t=0; t<listlth; t++)  {
		datptr = (SAMPLE *)db_getp( trc[t], EP_DATA, NULL );
		trclth = db_getl( trc[t], EL_LENGTH, NULL );
		dt = db_getr( trc[t], ER_DELTA, NULL );
		time = (REAL *)sy_allocmem( trclth, (int)sizeof(REAL), status );
		if  (Severe(status))  return;
		for  (i=0; i<trclth; i++)
			time[i] = (REAL)i * dt;
		nr_fit( time-1, datptr-1, trclth, NULL, FALSE, &b, &m, &siga, &sigb,
			&chi2, &q );
		for  (i=0; i<trclth; i++)
			datptr[i] -= m*time[i] + b;
		sy_deallocmem( time );
		sl_findmax( datptr, trclth, &min, &max );
		db_setr( trc[t], ER_MINVAL, min, status );
		if  (Severe(status))  return;
		db_setr( trc[t], ER_MAXVAL, max, status );
		if  (Severe(status))  return;
	} /*endfor*/

} /* end of mn4_trend */



/*--------------------------------------------------------------------*/



void mn4_fit( PARAM *par, STATUS *status )

/* fits a trace to a given wavelet
 * par 1:  trace to be fitted
 * par 2:  lo-bound
 * par 3:  hi-bound
 * par 4:  fit function
 * par 5:  amplitude of fit function
 * par 6..: outputs
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc;                  /* fit trace */
	REAL     lowdw, hiwdw;          /* trace window in sec */
	long     loidx, trclth;         /* trace window in samples */
	char     fct[BC_SHORTSTRLTH+1]; /* name fo fit function */
	char     symbol[BC_LINELTH+1];  /* output symbol */
	char     value[BC_LINELTH+1];   /* symbol value */
	int      maxiter;               /* maximum number of iterations */
	REAL     *xarr;                 /* pointer to x-array */
	long     i;                     /* counter */
	REAL     ampl;                  /* amplitude of fit function */
	REAL     torig, dt;             /* trace origin and sample distance */
	REAL     fit1, fit2;            /* fitted parameters (result of fitting) */
	int      iter;                  /* number of iterations used */

	/* executable code */

	trc = ml_get1trcpar( par, 1, tc, "   fit trace: ", status );
	if  (Severe(status))  return;
	ml_windowpars( par, 2, tc, "   lower bound: ", "   upper bound: ",
		&lowdw, &hiwdw, status );
	if  (Severe(status))  return;
	cp_getstr( par, 4, tc, "   name of fit function: ", BC_SHORTSTRLTH,
		fct, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 5, tc, "   amplitude of fit function: ", &ampl, status );
	if  (Severe(status))  return;

	if  (!sl_quali(par,"ITER",&maxiter,status))
		maxiter = 50;

	loidx = dm_getsample( trc, lowdw, TRUE );
	trclth = dm_getsample( trc, hiwdw, TRUE ) - loidx + 1;
	if  (trclth < 3)  {
		*status = SHE_SHORTTRC;
		return;
	} else if  (trclth > SYC_MAXINT)  {
		*status = SHE_SMPOVFL;
		return;
	} /*endif*/
	torig = db_getr( trc, ER_TORIG, NULL );
	dt = db_getr( trc, ER_DELTA, NULL );

	/* setup x-array */
	xarr = (REAL *)sy_allocmem( trclth, (int)sizeof(REAL), status );
	if  (Severe(status))  return;
	for  (i=0; i<trclth; i++)
		xarr[i] = torig + (i+loidx)*dt;

	if  (strcmp(fct,"GAUSS") == 0)  {  /* fit gauss function */

		/* get start values */
		if  (sl_quals(par,"START",BC_LINELTH,value,status))  {
			if  (sscanf(value,"%f,%f",&fit1,&fit2) != 2)  {
				*status = SHE_REALCNV;
				err_setcontext( " ## numstr " ); err_setcontext( value );
				return;
			} /*endif*/
			if  (fit2 <= 0.0)  {
				*status = SHE_ILPAR;
				sy_deallocmem( xarr );
				return;
			} /*endif*/
		} else {
			fit1 = torig + ((REAL)trclth*dt)/2.0;
			fit2 = 1.0;
		} /*endif*/

		/* perform fitting */
		mt_fit_gauss( xarr, (SAMPLE *)db_getp(trc,EP_DATA,NULL)+loidx,
			(int)trclth, ampl, maxiter, &fit1, &fit2, &iter, status );
			/* fit1=pos, fit2=width */
		if  (Severe(status))  {sy_deallocmem(xarr); return;}

		/* get position of gauss peak */
		cp_getstr( par, 6, tc, "   pos (output): ", BC_LINELTH,
			symbol, status );
		if  (Severe(status))  return;
		if  (*symbol != '\0')  {
			sprintf( value, "%e", fit1 );
			sl_setsymbol( symbol, value, status );
			if  (Severe(status))  return;
		} /*endif*/

		/* get width of gauss peak */
		cp_getstr( par, 7, tc, "   width (output): ", BC_LINELTH,
			symbol, status );
		if  (Severe(status))  return;
		if  (*symbol != '\0')  {
			sprintf( value, "%e", fit2 );
			sl_setsymbol( symbol, value, status );
			if  (Severe(status))  return;
		} /*endif*/

		/* get number of iterations */
		if  (cp_pentered(par,8,status))  {
			cp_getstr( par, 8, tc, "@@", BC_LINELTH, symbol, status );
			if  (Severe(status))  return;
			sprintf( value, "%d", iter );
			sl_setsymbol( symbol, value, status );
			if  (Severe(status))  return;
		} /*endif*/

		if  (shflags_shv & SHF_CHATTY)  {
			sprintf( value, "%s pos: %e, width: %e, iter: %d\n",
				SHC_CHATTXT, fit1, fit2, iter );
			gc_write( cc, value );
		} /*endif*/

	} else {
		sy_deallocmem( xarr );
		*status = SHE_ILPAR;
		return;
	} /*endif*/

	sy_deallocmem( xarr );

} /* end of mn4_fit */



/*--------------------------------------------------------------------*/



void mn4_stalta( PARAM *par, STATUS *status )

/* applies STA/LTA algorith to trace
 * par 1:  detection trace
 * par 2:  sta length in samples
 * par 3:  lta length in samples
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc;                 /* fit trace */
	TRACE    *new;                 /* output trace */
	int      lsta, llta;           /* lengths of STA and LTA windows (samples) */
	SAMPLE   *idat, *odat;         /* input & output data */
	int      trclth;               /* length of traces in samples */
	int      i, j;                 /* counters */
	double   sta, lta, old_lta;    /* sta and lta values (plain sums) */
	double   norm;                 /* normalization of sums */
	int      l1, l2;               /* lta indices */
	int      s1, s2;               /* sta indices */
	float    maxampl;              /* maximum amplitude */

	/* get parameters */
	trc = ml_get1trcpar( par, 1, tc, "   detection trace: ", status );
	if  (Severe(status))  return;
	cp_getint( par, 2, tc, "   sta length: ", &lsta, status );
	if  (Severe(status))  return;
	cp_getint( par, 3, tc, "   lta length: ", &llta, status );
	if  (Severe(status))  return;

	if  (!sl_qualr(par,"MAX",&maxampl,status))  {
		if  (Severe(status))  return;
		maxampl = -1.0;
	} /*endif*/

	/* get trace length and input data */
	trclth = db_getl( trc, EL_LENGTH, NULL );
	idat = (SAMPLE *)db_getp( trc, EP_DATA, NULL );

	/* check parameters */
	if  (lsta <= 1 || llta <= lsta || (lsta+llta) > trclth)  {
		*status = SHE_ILPAR;
		return;
	} /*endif*/

	/* create new trace */
	db_newlist();
	new = db_create( status );
	if  (Severe(status))  return;
	odat = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
	if  (Severe(status))  {db_delete(new); return;}

	/* apply sta/lta algorithm */

	/* get first sta sum */
	sta = 0.0;
	for  (i=0; i<lsta; i++)  sta += fabs(idat[i]);
	/* the first 'lsta' samples are set to 1.0 */
	for  (i=0; i<lsta; i++)  odat[i] = 1.0;
	/* the next 'llta' samples have incomplete lta window */
	s1 = 0;
	s2 = lsta-1;
	lta = 0.0;
	for  (i=lsta; i<(lsta+llta); i++)  {
		++s2;
		sta += fabs( idat[s2] );
		sta -= fabs( idat[s1] );
		s1++;
		lta += fabs( idat[i] );
		if  (lta < SHC_EPSILON)  {
			odat[i] = 1.0;
		} else {
			odat[i] = (sta/(float)lsta) / (lta/(float)(i-lsta+1));
		} /*endif*/
		if  (maxampl > 0.0 && odat[i] > maxampl)  odat[i] = maxampl;
	} /*endif*/
	/* now everything goes straightforward */
	norm = (double)llta/(double)lsta;
	l1 = 0;
	l2 = s1 - 1;
	for  (i=lsta+llta; i<trclth; i++)  {
		++s2;
		sta += fabs( idat[s2] );
		sta -= fabs( idat[s1] );
		s1++;
		++l2;
		lta += fabs( idat[l2] );
		lta -= fabs( idat[l1] );
		l1++;
		/* recompute lta and sta to stabilize numerics */
		/* printf( "%f\n", lta );*/
		if  (i % 100 == 0)  {
			old_lta = lta;
			lta = 0.0;
			for  (j=l1; j<=l2; j++)  lta += fabs( idat[j] );
			sta = 0.0;
			for  (j=s1; j<=s2; j++)  sta += fabs( idat[j] );
			if  (fabs(old_lta-lta)/lta > 0.01)  {
				fprintf( stderr, "stalta: recomputed at i=%d, ltadev=%f\n",
					i, fabs(old_lta-lta)/lta );
			} /*endif*/
		} /*endif*/
		if  (lta < SHC_EPSILON)  {
			odat[i] = 1.0;
		} else {
			odat[i] = norm * sta / lta;
		} /*endif*/
		if  (maxampl > 0.0 && odat[i] > maxampl)  odat[i] = maxampl;
	} /*endif*/

	/* set trace info */
	ml_cpyinfo( trc, new, status );
	if  (*status == SHE_NOERROR)
		ml_inittrc( new, odat, trclth, db_getr(trc,ER_DELTA,NULL) );

} /* end of mn4_stalta */



/*--------------------------------------------------------------------*/



void mn4_stability( PARAM *par, STATUS *status )

/* applies stability algorithm to trace
 * par 1:  detection trace
 * par 2:  tolerance
 * par 3:  add number
 * par 4:  factor
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc;                 /* fit trace */
	TRACE    *new;                 /* output trace */
	SAMPLE   *idat, *odat;         /* input & output data */
	int      trclth;               /* length of traces in samples */
	REAL     tol;                  /* tolerance */
	SAMPLE   add, mul;             /* multiplicator, add number */
	int      i;                    /* counter */
	SAMPLE   refdiff;              /* reference difference */
	SAMPLE   currval;              /* current output value */
	TSyBoolean limfound;           /* limit found */
	SAMPLE   limit;                /* amplitude limit */
	SAMPLE   maxval;               /* maximum value */

	/* get parameters */
	trc = ml_get1trcpar( par, 1, tc, "   detection trace: ", status );
	if  (Severe(status))  return;
	cp_getfloat( par, 2, tc, "   tolerance: ", &tol, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 3, tc, "   add number: ", &add, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 4, tc, "   multiplicator: ", &mul, status );
	if  (Severe(status))  return;

	/* get trace length and input data */
	trclth = db_getl( trc, EL_LENGTH, NULL );
	idat = (SAMPLE *)db_getp( trc, EP_DATA, NULL );

	limfound = sl_qualr( par, "LIMIT", &limit, status );
	if  (Severe(status))  return;
	if  (!sl_qualr(par,"MAX",&maxval,status))
		maxval = 0.0;

	/* create new trace */
	db_newlist();
	new = db_create( status );
	if  (Severe(status))  return;
	odat = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
	if  (Severe(status))  {db_delete(new); return;}

	/* get reference difference */
	refdiff = 0.0;
	for  (i=0; i<(trclth-1); i++)
		refdiff += fabs(idat[i]-idat[i+1]);
	refdiff /= (float)(trclth-1);

	/* apply algorithm, scan backward through trace */
	odat[trclth-1] = 0.0;
	currval = 0.0;
	for  (i=trclth-2; i>=0; i--)  {
		if  (limfound && fabs(idat[i]) > limit)  {
			odat[i] = currval = 0.0;
		} else if  (fabs(idat[i]-idat[i+1])/refdiff < tol)  {
			if  (maxval > 0.0 && currval > maxval)  {
				odat[i] = currval = maxval;
			} else {
				currval = currval*mul + add;
				odat[i] = currval;
			} /*endif*/
		} else {
			odat[i] = currval = 0.0;
		} /*endif*/
	} /*endfor*/

	/* set trace info */
	ml_cpyinfo( trc, new, status );
	if  (*status == SHE_NOERROR)
		ml_inittrc( new, odat, trclth, db_getr(trc,ER_DELTA,NULL) );

} /* end of mn4_stability */



/*--------------------------------------------------------------------*/



void mn4_leveldetec( PARAM *par, STATUS *status )

/* level detector, writes out times of values above certain level
 * par 1:  trace
 * par 2:  amplitude level
 * par 3:  output file
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc;                 /* fit trace */
	SAMPLE   level;                /* amplitude level */
	char     fname[cBcFileLth+1];  /* name of output file */
	SAMPLE   *dat;                 /* pointer to data array */
	int      trclth;               /* length of trace */
	float    dt;                   /* sample distance */
	int      i;                    /* counter */
	FILE     *fp;                  /* file pointer */
	TSyBoolean highmark;           /* currently above level */
	TSyBoolean absolute;           /* write absolute times */
	float    torig;                /* time of origin */
	TIME     stime, dtime;         /* start & det time */
	char     dettime[cBcTimeLth+1];  /* text time */
	float    maxampl;              /* maximum amplitude found */

	/* executable code */

	/* get parameters */
	trc = ml_get1trcpar( par, 1, tc, "   trace: ", status );
	if  (Severe(status))  return;
	cp_getfloat( par, 2, tc, "   level: ", &level, status );
	if  (Severe(status))  return;
	cp_getstr( par, 3, tc, "   output file: ", cBcFileLth, fname, status );
	absolute = cp_qexist( par, "ABS" );

	trclth = db_getl( trc, EL_LENGTH, NULL );
	dat = (SAMPLE *)db_getp( trc, EP_DATA, NULL );
	dt = db_getr( trc, ER_DELTA, NULL );

	if  (absolute)  {
		torig = db_getr( trc, ER_TORIG, NULL );
		db_gett( trc, ET_START, &stime, status );
		if  (Severe(status))  return;
	} /*endif*/


	if  (strcmp(fname,"TT") == 0)  {
		fp = stdout;
	} else {
		fp = sy_fopen( fname, "w" );
		if  (fp == NULL)  {
			*status = SHE_OPNWR;
			return;
		} /*endif*/
	} /*endif*/

	highmark = FALSE;
	for  (i=0; i<trclth; i++)  {
		if  (dat[i] > level)  {
			if  (highmark)  {
				if  (dat[i] > maxampl)  maxampl = dat[i];
			} else {
				maxampl = dat[i];
				if  (absolute)  {
					tc_aadd( &stime, torig+((float)i*dt), &dtime );
					tc_a2t( &dtime, dettime, status );
					if  (Severe(status))  {if  (fp != NULL)  fclose(fp); return;}
					fprintf( fp, "%s ", dettime );
				} else {
					fprintf( fp, "%f ", (float)i*dt );
				} /*endif*/
			} /*endif*/
			highmark = TRUE;
		} else  {
			if  (highmark)  {
				fprintf( fp, "%e\n", maxampl );
				highmark= FALSE;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	if  (highmark)  fprintf( fp, "%e\n", maxampl );

	if  (fp != stdout)  fclose( fp );

} /* end of mn4_leveldetec */



/*----------------------------------------------------------------------------*/



static float mn4_taper( int idx, int maxidx )

/* returns taper value
 *
 * parameters of routine
 * int        idx;       input; current sample index (0<=idx<maxidx)
 * int        maxidx;    input; number of indices
 *                       returns taper value
 */
{
	/* local variables */

	/* executable code */

	return (cos( ((2.0*(float)idx/(float)maxidx) - 1.0)*BC_PI ) + 1.0)/2.0;

} /* end of mn4_taper */



/*----------------------------------------------------------------------------*/



static int mn4_constnum( SAMPLE s[], int lth )

/* returns maximum number of constant values found
 *
 * parameters of routine
 * SAMPLE     s[];      input; pointer to array
 * int        lth;      input; length of array
 */
{
	/* local variables */
	int      i;           /* counter */
	int      cnt;         /* constant counter */
	int      maxcnt;      /* max counter */

	/* executable code */

	cnt = maxcnt = 0;
	for  (i=1; i<lth; i++)  {
		if  (s[i] == s[i-1])  {
			cnt++;
			if  (cnt > maxcnt)  maxcnt = cnt;
		} else {
			cnt = 0;
		} /*endif*/
	} /*endfor*/
	return maxcnt;

} /* end of mn4_constnum */



/*----------------------------------------------------------------------------*/


/* file SHMENU3.C
 *      =========
 *
 * version 32, 22-May-2006
 *
 * seismhandler menu routines
 * K. Stammler, 10-NOV-1990
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
#include "ttusrdef.h"
#include "ptusrdef.h"
#include "itusrdef.h"
#include "utusrdef.h"
#include "erusrdef.h"
#include "glusrdef.h"
#include "ffusrdef.h"
#include BC_GCUSRDEF
#include "infoidx.h"
#include "fctxml.h"
#include "fctxmt.h"
#include "fctxsl.h"
#include "fctxdm.h"
#include "fctxmn3.h"
#include "sherrors.h"
#include "earthloc.h"



/*--------------------------------------------------------------------*/



void mn3_overlay( PARAM *par, STATUS *status )

/* overlay traces
 * 1. param:  trace list to be overlayed
 *
 * parameters of routine
 * PARAM      *par;     input; menu parameters
 * STATUS     *status   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];   /* trace list */
	int      listlth;              /* length of list */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	if  (cp_qexist(par,"DEL"))  {
		dm_deloverlays();
		if  (cp_pnum(par) == 0)  return;
	} /*endif*/
	ml_gettrcpar( par, 1, tc, "   overlay list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	dm_setoverlay( trc, listlth, status );

} /* end of mn3_overlay */



/*--------------------------------------------------------------------*/



void mn3_call( PARAM *par, STATUS *status )

/* special functions
 * 1. param:  name of function
 *
 * parameters of routine
 * PARAM      *par;     input; menu parameters
 * STATUS     *status   output; return status
 */
{
	/* local variables */
	char     fct[BC_LINELTH+1];      /* function name */
	char     str[BC_LINELTH+1];      /* scratch string */
	char     symbol[BC_LINELTH+1];   /* scratch string */
	int      i;
	float    r[4], res, res2, res3;
	double   d[3];
	GLT_STATINF *statinf;            /* pointer to station info */

	/* executable code */

	cp_getstr( par, 1, tc, "   function: ", BC_LINELTH, fct, status );
	if  (Severe(status))  return;

	if  (strcmp(fct,"SLOWNESS_OLD") == 0)  {
		cp_getstr( par, 2, tc, "   phase: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strcmp(str,"P") == 0)  {
			i = TTC_PHASE_P;
		} else if  (strcmp(str,"S") == 0)  {
			i = TTC_PHASE_S;
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
		cp_getfloat( par, 3, tc, "   distance: ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   depth: ", r+1, status );
		if  (Severe(status))  return;
		cp_getstr( par, 5, tc, "   symbol: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		res = tt_slowness( i, r[0], r[1], status );
		if  (Severe(status))  return;
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"SLOWNESS") == 0)  {
		cp_getstr( par, 2, tc, "   phase: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   distance: ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   depth: ", r+1, status );
		if  (Severe(status))  return;
		cp_getstr( par, 5, tc, "   symbol: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		res = pt_slowness( str, r[0], r[1], status );
		if  (Severe(status))  return;
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"TRAVEL_OLD") == 0)  {
		cp_getstr( par, 2, tc, "   phase: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		if  (strcmp(str,"P") == 0)  {
			i = TTC_PHASE_P;
		} else if  (strcmp(str,"S") == 0)  {
			i = TTC_PHASE_S;
		} else {
			*status = SHE_ILPAR;
			return;
		} /*endif*/
		cp_getfloat( par, 3, tc, "   distance: ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   depth: ", r+1, status );
		if  (Severe(status))  return;
		cp_getstr( par, 5, tc, "   symbol: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		res = tt_travel( i, r[0], r[1], status );
		if  (Severe(status))  return;
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"TRAVEL") == 0)  {
		cp_getstr( par, 2, tc, "   phase: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   distance: ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   depth: ", r+1, status );
		if  (Severe(status))  return;
		cp_getstr( par, 5, tc, "   symbol: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		res = pt_travel( str, r[0], r[1], status );
		if  (Severe(status))  return;
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"DISTANCE") == 0)  {
		cp_getstr( par, 2, tc, "   phase: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   slowness (sec/deg): ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   depth (km): ", r+1, status );
		if  (Severe(status))  return;
		cp_getstr( par, 5, tc, "   symbol: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		res = pt_distance( str, r[0], r[1], status );
		if  (Severe(status))  return;
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"DEPTH") == 0)  {
		cp_getstr( par, 2, tc, "   main phase: ", BC_LINELTH,
			str, status );
		if  (Severe(status))  return;
		cp_getstr( par, 3, tc, "   depth phase: ", BC_LINELTH,
			fct, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   t-diff (sec): ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 5, tc, "   distance (deg): ", r+1, status );
		if  (Severe(status))  return;
		cp_getstr( par, 6, tc, "   symbol: ", BC_LINELTH,
			symbol, status );
		if  (Severe(status))  return;
		res = pt_depth( str, fct, r[0], r[1], status );
		if  (Severe(status))  return;
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"DISTANCE_PD") == 0)  {
		cp_getstr( par, 2, tc, "   phase 1: ", BC_LINELTH,
			str, status );
		if  (Severe(status))  return;
		cp_getstr( par, 3, tc, "   phase 2: ", BC_LINELTH,
			fct, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   t-diff (sec): ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 5, tc, "   depth (km): ", r+1, status );
		if  (Severe(status))  return;
		cp_getstr( par, 6, tc, "   symbol: ", BC_LINELTH,
			symbol, status );
		if  (Severe(status))  return;
		res = pt_distance_pd( str, fct, r[0], r[1], status );
		if  (Severe(status))  return;
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strncmp(fct,"INCI",4) == 0)  {
		cp_getfloat( par, 2, tc, "   distance: ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   depth: ", r+1, status );
		if  (Severe(status))  return;
		cp_getstr( par, 4, tc, "   symbol: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		res = it_getinci( r[0], r[1], status );
		if  (Severe(status))  return;
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"STATLOC") == 0)  {
		cp_getstr( par, 2, tc, "   station: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		cp_getstr( par, 3, tc, "   lat-out: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		/* gl_full_statloc( str, d, d+1, &i, r, r+1, BC_LINELTH, fct, status ); */
		statinf = gl_store_station( str, TRUE, status );
		if  (Severe(status))  return;
		sprintf( str, "%le", statinf->lat /*d[0]*/ );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
		cp_getstr( par, 4, tc, "   lon-out: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		sprintf( str, "%le", statinf->lon /*d[1]*/ );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
		if  (cp_pentered(par,5,status))  {
			cp_getstr( par, 5, tc, "   xrel: ", BC_LINELTH, symbol, status );
			if  (Severe(status))  return;
			sprintf( str, "%e", statinf->xrel );
			sl_setsymbol( symbol, str, status );
			if  (Severe(status))  return;
		} /*endif*/
		if  (cp_pentered(par,6,status))  {
			cp_getstr( par, 6, tc, "   yrel: ", BC_LINELTH, symbol, status );
			if  (Severe(status))  return;
			sprintf( str, "%e", statinf->yrel );
			sl_setsymbol( symbol, str, status );
			if  (Severe(status))  return;
		} /*endif*/
		if  (cp_pentered(par,7,status))  {
			cp_getstr( par, 7, tc, "   elevation: ", BC_LINELTH, symbol, status );
			if  (Severe(status))  return;
			sprintf( str, "%e", statinf->elevation );
			sl_setsymbol( symbol, str, status );
			if  (Severe(status))  return;
		} /*endif*/
	} else if  (strcmp(fct,"LOCDIFF") == 0)  {
		cp_getfloat( par, 2, tc, "   lat1: ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   lon1: ", r+1, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   lat2: ", r+2, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 5, tc, "   lon2: ", r+3, status );
		if  (Severe(status))  return;
		cp_getstr( par, 6, tc, "   dist-out: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		mb_locdiff( r[0], r[1], r[2], r[3], d, d+1, d+2 );
		res = d[0];  res2 = d[2]; res3 = d[1];
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
		cp_getstr( par, 7, tc, "   b-azim-out: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		sprintf( str, "%e", res2 );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
		if  (cp_pentered(par,8,status))  {
			cp_getstr( par, 8, tc, "   azim-out: ", BC_LINELTH, symbol, status );
			if  (Severe(status))  return;
			sprintf( str, "%e", res3 );
			sl_setsymbol( symbol, str, status );
			if  (Severe(status))  return;
		} /*endif*/
	} else if  (strcmp(fct,"SPHEREDIFF") == 0)  {
		cp_getfloat( par, 2, tc, "   lat1: ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   lon1: ", r+1, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   lat2: ", r+2, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 5, tc, "   lon2: ", r+3, status );
		if  (Severe(status))  return;
		cp_getstr( par, 6, tc, "   dist-out: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		mb_spherediff( r[0], r[1], r[2], r[3], r, r+1 );
		sprintf( str, "%e", r[0] );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
		cp_getstr( par, 7, tc, "   azim-out: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		sprintf( str, "%e", r[1] );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"SPHEREADD") == 0)  {
		cp_getfloat( par, 2, tc, "   lat: ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   lon: ", r+1, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   dist: ", r+2, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 5, tc, "   azim: ", r+3, status );
		if  (Severe(status))  return;
		cp_getstr( par, 6, tc, "   lat-out: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		mb_sphereadd( r[0], r[1], r[2], r[3], &res, &res2 );
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
		cp_getstr( par, 7, tc, "   lon-out: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		sprintf( str, "%e", res2 );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"FER") == 0)  {
		cp_getfloat( par, 2, tc, "   lat (deg): ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   lon (deg): ", r+1, status );
		if  (Severe(status))  return;
		cp_getstr( par, 4, tc, "   variable: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		mb_getlocname( r[0], r[1], BC_LINELTH, str, status );
		if  (Severe(status))  return;
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"CORRECT") == 0)  {
		cp_getfloat( par, 2, tc, "   slowness (sec/deg): ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   azimuth (deg): ", r+1, status );
		if  (Severe(status))  return;
		cp_getstr( par, 4, tc, "   slo-out: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		mb_statcorr( r[0], r[1], r, r+1, r+2, r+3, status );
		if  (Severe(status))  return;
		sprintf( str, "%e", r[0] );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
		cp_getstr( par, 5, tc, "   az-out: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		sprintf( str, "%e", r[1] );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
		if  (cp_pentered(par,6,status))  {
			cp_getstr( par, 6, tc, "   slo-dist: ", BC_LINELTH, symbol, status );
			if  (Severe(status))  return;
			sprintf( str, "%e", r[2] );
			sl_setsymbol( symbol, str, status );
		} /*endif*/
		if  (Severe(status))  return;
		if  (cp_pentered(par,7,status))  {
			cp_getstr( par, 7, tc, "   az-dist: ", BC_LINELTH, symbol, status );
			if  (Severe(status))  return;
			sprintf( str, "%e", r[3] );
			sl_setsymbol( symbol, str, status );
		} /*endif*/
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"PHASEDIFF") == 0)  {
		cp_getstr( par, 2, tc, "   phase: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   conversion depth (km): ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   slowness (sec/deg): ", r+1, status );
		if  (Severe(status))  return;
		cp_getstr( par, 5, tc, "   output: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		if  (strcmp(str,"XS-P") == 0)  {
			res = tt_mantle_diff( TTC_P_S_xDIFF, r[0], r[1], status );
		} else if  (strcmp(str,"S-P") == 0)  {
			res = tt_mantle_diff( TTC_P_S_DIFF, r[0], r[1], status );
		} else if  (strcmp(str,"Ppp-P") == 0)  {
			res = tt_mantle_diff( TTC_P_Ppp_DIFF, r[0], r[1], status );
		} else if  (strcmp(str,"Pps-P") == 0)  {
			res = tt_mantle_diff( TTC_P_Pps_DIFF, r[0], r[1], status );
		} else {
			*status = SHE_ILPAR;
			err_setcontext( " ## phase " ); err_setcontext( str );
			return;
		} /*endif*/
		if  (Severe(status))  return;
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"RANDOM") == 0)  {
		cp_getfloat( par, 2, tc, "   lo-bound: ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   hi-bound: ", r+1, status );
		if  (Severe(status))  return;
		cp_getstr( par, 4, tc, "   output: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		r[2] = (r[0]+r[1]) / 2.0;   /* offset */
		r[3] = (r[1]-r[0]) / 2.0;   /* amplitude */
		res = r[2] + mt_random( r[3] );
		if  (Severe(status))  return;
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"GRANDOM") == 0)  {
		cp_getfloat( par, 2, tc, "   offset: ", r, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   width: ", r+1, status );
		if  (Severe(status))  return;
		cp_getstr( par, 4, tc, "   output: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		res = r[0] + mt_gauss_random( r[1] );
		if  (Severe(status))  return;
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else if  (strcmp(fct,"FILTER-AMPL") == 0)  {
		cp_getstr( par, 2, tc, "   filter: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   freq (Hz): ", r, status );
		if  (Severe(status))  return;
		cp_getstr( par, 4, tc, "   output: ", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		res = ff_filter_amplitude( str, r[0], status );
		if  (Severe(status))  return;
		sprintf( str, "%e", res );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;		
	} else {
		*status = SHE_UKKEY;
		err_setcontext( " ## subfct " ); err_setcontext( fct );
		return;
	} /*endif*/

} /* end of mn3_call */



/*--------------------------------------------------------------------*/



void mn3_unit( PARAM *par, STATUS *status )

/* normalise traces to 1 (maximum amplitude of all traces in given window
 * is set to 1).
 * 1. param:  trace list
 * 2. param:  time window start
 * 3. param:  time window end
 *
 * parameters of routine
 * PARAM      *par;     input; menu parameters
 * STATUS     *status   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];    /* trace list */
	int      listlth;               /* length of trace list */
	REAL     lowdw, hiwdw;          /* time window in sec */
	long     startidx, trclth;      /* trace window in samples */
	int      t;                     /* trace counter */
	SAMPLE   absmax;                /* absolute maximum */
	SAMPLE   min, max;
	SAMPLE   *s, *ptr;

	/* executable code */

	if  (cp_pnexc(par,3,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	dm_dspcoo( &lowdw, NULL, &hiwdw, NULL );
	hiwdw += lowdw;
	if  (cp_pentered(par,2,status))
		ml_windowpars( par, 2, tc, "   lower bound: ", "   upper bound: ",
			&lowdw, &hiwdw, status );

	absmax = 0.0;
	for  (t=0; t<listlth; t++)  {
		startidx = dm_getsample( trc[t], lowdw, TRUE );
		trclth = dm_getsample( trc[t], hiwdw, TRUE ) - startidx;
		if  (trclth > 0)  {
			sl_findmax( (SAMPLE *)db_getp(trc[t],EP_DATA,NULL)+startidx,
			trclth, &min, &max );
			min = fabs( min );
			max = fabs( max );
			if  (min > max)  max = min;
			if  (max > absmax)  absmax = max;
		} /*endif*/
	} /*endfor*/

	if  (absmax < SHC_EPSILON)  return;
	absmax = 1.0 / absmax;

	for  (t=0; t<listlth; t++)  {
		ptr = db_getp( trc[t], EP_DATA, NULL );
		trclth = db_getl( trc[t], EL_LENGTH, NULL );
		db_setr( trc[t], ER_MINVAL,
			db_getr(trc[t],ER_MINVAL,NULL)*absmax, NULL );
		db_setr( trc[t], ER_MAXVAL,
			db_getr(trc[t],ER_MAXVAL,NULL)*absmax, NULL );
		db_setf( trc[t], EF_MODIF, TRUE, NULL );
		for  (s=ptr; s<ptr+trclth; *s++ *= absmax ) {}
	} /*endfor*/

} /* end of mn3_unit */



/*----------------------------------------------------------------------------*/



void mn3_fold( PARAM *par, STATUS *status )

/* folds traces
 * par 1:    trace list to be folded
 * par 2, 3: time window
 * par 4:    filter trace
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];   /* trace list */
	int      listlth;              /* length of trace list */
	TRACE    *fil;                 /* filter trace */
	TRACE    *new;                 /* new trace */
	SAMPLE   *datptr;              /* pointer to new sample data */
	REAL     lowdw, hiwdw;         /* time window */
	long     loidx, hiidx;         /* trace window in samples */
	long     itrclth;              /* length of input trace */
	long     otrclth;              /* length of output trace */
	BOOLEAN  fulltrace;            /* take full traces */
	REAL     dt;                   /* sample distance in sec */
	REAL     fildt;                /* sample distance of filter */
	int      t;                    /* trace counter */
	char     str[BC_LINELTH+1];    /* scratch */
	STATUS   locstat;              /* local status */
	REAL     spikepos;             /* spike position */
	TIME     stime;                /* start time */
	long     flth;

	/* executable code */

	if  (cp_pnexc(par,4,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	fulltrace = !cp_pentered( par, 2, status );
	if  (Severe(status))  return;

	if  (fulltrace)  {
		lowdw = hiwdw = 0;
	} else {
		cp_getfloat( par, 2, tc, "@@", &lowdw, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   upper bound: ", &hiwdw, status );
		if  (Severe(status))  return;
	} /*endif*/

	fil = ml_get1trcpar( par, 4, tc, "   filter: ", status );
	if  (Severe(status))  return;
	fildt = db_getr( fil, ER_DELTA, NULL );

	db_newlist();

	for  (t=0; t<listlth; t++)  {

		/* get trace window */
		if  (fulltrace)  {
			loidx = 0;
			hiidx = db_getl( trc[t], EL_LENGTH, NULL ) - 1;
		} else {
			loidx = dm_getsample( trc[t], lowdw, TRUE );
			hiidx = dm_getsample( trc[t], hiwdw, TRUE );
		} /*endif*/
		flth = db_getl( fil, EL_LENGTH, NULL );
		itrclth = hiidx - loidx + 1;
		otrclth = itrclth + flth - 1;
		dt = db_getr( trc[t], ER_DELTA, NULL );
		if  (dt != fildt)  {
			*status = SHE_DIFFSAMP;
			return;
		} /*endif*/

		/* create new trace */
		new = db_create( status );
		if  (Severe(status))  return;
		datptr = (SAMPLE *)sy_allocmem( otrclth, (int)sizeof(SAMPLE), status );
		if  (Severe(status))  {
			db_delete( new );
			return;
		} /*endif*/

		/* perform filtering */
		mt_fold( itrclth, (SAMPLE *)db_getp(trc[t],EP_DATA,NULL)+loidx, 
			flth, db_getp(fil,EP_DATA,NULL), datptr );

		/* set trace info */
		ml_cpyinfo( trc[t], new, status );
		if  (Severe(status))  {
			sy_deallocmem( datptr );
			db_delete( new );
			return;
		} /*endif*/
		ml_inittrc( new, datptr, otrclth, dt );
		if  (lowdw > db_getr(trc[t],ER_TORIG,NULL))
			db_setr( new, ER_TORIG, lowdw, NULL );
		db_setf( new, EF_MODIF, lowdw, NULL );
		ml_newstarttime( trc[t], new, lowdw, status );
		locstat = BC_NOERROR;
		db_gets( fil, ES_OPINFO, BC_LINELTH, str, &locstat );
		if  (locstat == BC_NOERROR)
			if  (sscanf(str,"spiking %f",&spikepos) == 1)  {
				db_gett( new, ET_START, &stime, &locstat );
				if  (locstat == BC_NOERROR)  {
					tc_aadd( &stime, -spikepos, &stime );
					db_sett( new, ET_START, &stime, &locstat );
				} /*endif*/
			} /*endif*/
		if  (Severe(status))  {
			sy_deallocmem( datptr );
			db_delist( gc, new );
			db_delete( new );
			return;
		} /*endif*/

	} /*endfor*/

} /* end of mn3_fold */



/*----------------------------------------------------------------------------*/



void mn3_cut( PARAM *par, STATUS *status )

/* cuts traces
 * par 1:    trace list to be cut
 * par 2, 3: time window
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];    /* trace list */
	int      listlth;               /* length of trace list */
	REAL     hiwdw, lowdw;          /* time window */
	int      t;                     /* trace counter */
	char     tinfo[BC_LINELTH+1];   /* time info */
	unsigned tentry;                /* time entry */
	REAL     vel;                   /* velocity */

	/* executable code */

	if  (cp_pnexc(par,3,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	ml_windowpars( par, 2, tc, "   lower bound: ", "   upper bound: ",
		&lowdw, &hiwdw, status );
	if  (Severe(status))  return;

	if  (sl_quals(par,"POS",BC_LINELTH,tinfo,status))  {
		db_ident( tinfo, &tentry, status );
		if  (Severe(status))  return;
		t = tentry & E_TYPMASK;
		if  (t != ET_TYPE  &&  t != ER_TYPE)  {
			*status = SHE_ILTYPE;
			return;
		} /*endif*/
		if  (!sl_qualr(par,"VEL",&vel,status))
			vel = 6.0;
		if  (Severe(status))  return;
	} else {
		if  (Severe(status))  return;
		*tinfo = '\0';
		tentry = 0;
	} /*endif*/

	for  (t=0; t<listlth; t++)  {
		ml_cut( trc[t], lowdw, hiwdw, tentry, vel, status );
		if  (Severe(status))  return;
	} /*endfor*/

} /* end of mn3_cut */



/*--------------------------------------------------------------------*/



void mn3_mirror( PARAM *par, STATUS *status )

/* mirrors traces
 * par 1:    trace list to be mirrored
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];   /* trace list */
	int      listlth;              /* length of trace list */
	int      t;                    /* trace counter */
	SAMPLE   *srcptr;              /* source pointer */
	SAMPLE   *dstptr;              /* destination pointer */
	SAMPLE   tmp;                  /* scratch */
	long     datlth;               /* length of data */
	long     i;                    /* sample counter */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;

	for  (t=0; t<listlth; t++)  {
		datlth = db_getl( trc[t], EL_LENGTH, NULL );
		srcptr = (SAMPLE *)db_getp( trc[t], EP_DATA, NULL );
		dstptr = srcptr + datlth - 1;
		datlth /= 2;
		for  (i=0; i<datlth; i++)  {
			tmp = *dstptr;
			*dstptr-- = *srcptr;
			*srcptr++ = tmp;
		} /*endfor*/
	} /*endfor*/

} /* end of mn3_mirror */



/*--------------------------------------------------------------------*/



void mn3_curve( PARAM *par, STATUS *status )

/* draws curve into trace display
 * par 1:    name of curve file
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	char     curvefilename[BC_FILELTH+1]; /* name of file */
	char     str[BC_LINELTH+1];           /* scratch */
	FILE     *cf;                         /* curve file pointer */
	REAL     x, y;                        /* coordinate pair */
	REAL     xp, yp;                      /* previous pair */
	BOOLEAN  move;                        /* move to (x,y) */
	int      style;                       /* style block */
	REAL     xlo, xhi, ylo, yhi;          /* display bounds */
	BOOLEAN  neg;                         /* use negative y-values */
	BOOLEAN  clip;                        /* clip output */
	int      slth;                        /* string length */
	REAL     vredf;                       /* reduction velocity in file */
	REAL     vred;                        /* desired reduction velocity */
	BOOLEAN  do_vred;                     /* recompute reduction velocity */
	REAL     tmp;                         /* scratch */

#	define OUTOFDSP(v,w) ((x)<xlo || (x)>xhi || (y)<ylo || (y)>yhi)

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	cp_getstr( par, 1, tc, "   curve file: ", BC_FILELTH, curvefilename, status );
	if  (Severe(status))  return;
	neg = cp_qexist(par,"NEG");
	clip = cp_qexist(par,"CLIP");
	do_vred = sl_qualr( par, "VEL", &vred, status );
	ut_defext( curvefilename, BC_FILELTH, SHC_DE_CURVE, status );
	if  (Severe(status))  return;
	cf = sy_fopen( curvefilename, "r" );
	if  (cf == NULL)  {
		if  ((strlen(shd_inputs)+strlen(curvefilename)) > BC_FILELTH)  {
			*status = SHE_STROVFL;
			err_setcontext( " ## filename too long" );
			return;
		} /*endif*/
		strcpy( str, curvefilename );
		strcpy( curvefilename, shd_inputs );
		strcat( curvefilename, str );
		cf = sy_fopen( curvefilename, "r" );
		if  (cf == NULL)  {
			*status = SHE_OPNRD;
			err_setcontext( " ## file " ); err_setcontext( curvefilename );
			return;
		} /*endif*/
	} /*endif*/

	dm_dspcoo( &xlo, &ylo, &xhi, &yhi );
	xhi += xlo;
	yhi += ylo;

	*str = '\n';
	while  (*str == '\n')  {
		if  (fgets(str,BC_LINELTH,cf) == NULL)  {
			*status = SHE_FILREAD;
			err_setcontext( " ## file " ); err_setcontext( curvefilename );
			fclose( cf );
			return;
		} /*endif*/
	} /*endwhile*/
	if  (strncmp(str,"SH_CRV",6) != 0)  {
		*status = SHE_SPECERROR+14;
		fclose( cf );
	} /*endif*/

	vredf = 0.0;
	style = SHC_MARKSTYLE;
	move = TRUE;
	x = y = 0.0;
	while  (fgets(str,BC_LINELTH,cf) != NULL)  {
		xp = x; yp = y;
		if  (sscanf(str,"%f %f",&y,&x) == 2)  {
			if  (neg)  y = -y;
			if  (do_vred && (vred != vredf))  {
				/* get unreduced time */
				tmp = (vredf == 0.0) ? x : x + Abs(y)/vredf;
				/* get new reduced time */
				x = (vred == 0.0) ? tmp : tmp - Abs(y)/vred;
			} /*endif*/
			if  (move)  {
				gc_moveto( gc, x, y );
				move = (clip && OUTOFDSP(x,y));
			} else {
				if  (clip && OUTOFDSP(x,y))  {
					move = TRUE;
				} else {
					gc_drawto( gc, style, x, y );
				} /*endif*/
			} /*endif*/
		} else {
			move = TRUE;
			ut_cap( str );
			slth = (int)strlen( str ) - 1;
			if  (str[slth] == '\n')  str[slth] = '\0';
			if  (strncmp(str,"TEXT: ",6) == 0)  {
				if  (strlen(str) > 6)
					gc_text( gc, style, xp, yp, str+6 );
			} else if  (strncmp(str,"VRED: ",6) == 0)  {
				if  (strlen(str) > 6)
					sscanf( str+6, "%f", &vredf );
			} else {
			} /*endif*/
		} /*endif*/
	} /*endwhile*/
	gc_flushbuffers();

	fclose( cf );

#	undef OUTOFDSP

} /* end of mn3_curve */



/*--------------------------------------------------------------------*/



void mn3_despike( PARAM *par, STATUS *status )

/* despikes traces
 * par 1:    trace list
 * par 2:    detection factor
 * par 3:    lo time bound
 * par 4:    hi time bound
 * par 5:    output: number of spikes
 * par 6:    output: number of discontinuities
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];   /* trace list */
	int      listlth;              /* list length */
	REAL     critmul;              /* detection factor */
	int      t;                    /* trace counter */
	REAL     lowdw, hiwdw;         /* time window */
	long     loidx, hiidx;         /* sample window */
	BOOLEAN  fulltrace;            /* take full traces */
	SAMPLE   *start;               /* pointer to trace start */
	long     trclth;               /* length of trace */
	long     spikeno, lspno;       /* number of spikes founds */
	long     dcno, ldcno;          /* number of discontinuities */
	char     str[BC_LINELTH+1];    /* scratch */
	SAMPLE   min, max;             /* extreme values */
	char     symbol[BC_LINELTH+1]; /* output symbol name */
	char     logfile[BC_FILELTH+1];/* log file name */
	FILE     *log;                 /* log file */

	/* executable code */

	if  (cp_pnexc(par,6,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 2, tc, "   detection factor: ", &critmul, status );
	if  (Severe(status))  return;
	fulltrace = !cp_pentered( par, 3, status );
	if  (!fulltrace)  {
		ml_windowpars( par, 3, tc, "@@", "   upper bound: ", &lowdw,
			&hiwdw, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (!sl_quals( par, "LOG", BC_FILELTH, logfile, status ))
		*logfile = '\0';
	if  (Severe(status))  return;
	if  (*logfile != '\0')  {
		log = sy_fopen( logfile, "w" );
		if  (log == NULL)  {
			*status = SHE_OPNWR;
			err_setcontext( " ## file " );
			err_setcontext( logfile );
			return;
		} /*endif*/
	} else {
		log = NULL;
	} /*endif*/

	spikeno = dcno = 0;
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
		if  (log != NULL)
			fprintf( log, "trace %d\n", t+1 );
		mt_despike( start, trclth, critmul, log, &lspno, &ldcno );
		spikeno += lspno;
		dcno += ldcno;
		if  (shflags_shv & SHF_CHATTY)  {
			sprintf( str, "%s %ld spikes removed in trace %d\n",
				SHC_CHATTXT, lspno, t+1 );
			gc_write( cc, str );
			if  (ldcno > 0)  {
				sprintf( str, "%s %ld discontinuties found in trace %d\n",
					SHC_CHATTXT, ldcno, t+1 );
				gc_write( cc, str );
			} /*endif*/
		} /*endif*/
		sl_findmax( db_getp(trc[t],EP_DATA,NULL),
			db_getl(trc[t],EL_LENGTH,NULL), &min, &max );
		db_setr( trc[t], ER_MINVAL, min, status );
		if  (Severe(status))  {
			if  (log != NULL)  fclose( log );
			return;
		} /*endif*/
		db_setr( trc[t], ER_MAXVAL, max, status );
		if  (Severe(status))  {
			if  (log != NULL)  fclose( log );
			return;
		} /*endif*/
	} /*endfor*/

	if  (log != NULL)
		fclose( log );

	/* set output symbols */
	if  (cp_pentered(par,5,status))  {
		cp_getstr( par, 5, tc, "@@", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		sprintf( str, "%ld", spikeno );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} /*endif*/
	if  (cp_pentered(par,6,status))  {
		cp_getstr( par, 6, tc, "@@", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		sprintf( str, "%ld", dcno );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} /*endif*/

} /* end of mn3_despike */



/*--------------------------------------------------------------------*/



void mn3_mend( PARAM *par, STATUS *status )

/* mends trace
 * par 1:    trace
 * par 2:    order
 * par 3:    lo time bound
 * par 4:    hi time bound
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc;                 /* trace */
	int      order;                /* order */
	REAL     lowdw, hiwdw;         /* time window */
	long     loidx, hiidx;         /* sample window */
	long     loff, roff;           /* offsets */
	SAMPLE   *start;               /* pointer to trace start */
	long     trclth;               /* length of trace */
	SAMPLE   min, max;             /* extreme values */
	BOOLEAN  pol;                  /* polynomial or rational interpolation */
	char     str[BC_LINELTH+1];    /* scratch */
	int      widening;             /* widening factor */

	/* executable code */

	if  (cp_pnexc(par,4,status))  return;
	trc = ml_get1trcpar( par, 1, tc, "   trace list: ", status );
	if  (Severe(status))  return;
	cp_getint( par, 2, tc, "   order: ", &order, status );
	if  (Severe(status))  return;
	ml_windowpars( par, 3, tc, "   lower bound: ", "   upper bound: ",
		&lowdw, &hiwdw, status );
	if  (Severe(status))  return;
	pol = !cp_qexist( par, "RATIONAL" );

	if  (!sl_quali(par,"WIDENING",&widening,status))
		widening = 1.;

	if  (order < 2)  {*status = SHE_SPECERROR+17; return;}
	start = (SAMPLE *)db_getp( trc, EP_DATA, NULL );
	loidx = dm_getsample( trc, lowdw, TRUE );
	hiidx = dm_getsample( trc, hiwdw, TRUE );
	start += loidx;
	trclth = hiidx - loidx + 1;
	loff = order / 2;
	roff = order - loff;
	loff = (long)widening*(loff-1) + 1;
	roff = (long)widening*(roff-1) + 1;
	if  (loidx < loff)  {*status = SHE_SPECERROR+17; return;}
	if  (hiidx+roff > db_getl(trc,EL_LENGTH,NULL))
		{*status = SHE_SPECERROR+17; return;}
	start -= loff;
	trclth += roff+loff;
	mt_mend( start, trclth, loff, roff, widening, 
		db_getr(trc,ER_DELTA,NULL), pol, status );

	sl_findmax( db_getp(trc,EP_DATA,NULL),
		db_getl(trc,EL_LENGTH,NULL), &min, &max );
	db_setr( trc, ER_MINVAL, min, status );
	if  (Severe(status))  return;
	db_setr( trc, ER_MAXVAL, max, status );
	if  (Severe(status))  return;

	if  (shflags_shv & SHF_CHATTY)  {
		sprintf( str, "%s %ld samples replaced\n", SHC_CHATTXT, trclth-loff-roff );
		gc_write( cc, str );
	} /*endif*/

} /* end of mn3_mend */



/*--------------------------------------------------------------------*/



void mn3_spikefil( PARAM *par, STATUS *status )

/* computes spiking filter
 * par 1:    trace
 * par 2:    lo time bound
 * par 3:    hi time bound
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	TRACE    *trc;          /* input trace */
	TRACE    *new;          /* new created trace */
	REAL     lowdw, hiwdw;  /* time window */
	float    reg;           /* control parameter */
	long     loidx, hiidx;  /* sample window */
	REAL     t0pos;         /* position of spike in sec */
	REAL     lthfac;        /* length factor */
	int      trclth;        /* length of window in samples */
	int      outlth;        /* length of output trace in samples */
	SAMPLE   *f, *ac, *ccr; /* pointers to data arrays */
	SAMPLE   *mov;          /* moving pointer */
	SAMPLE   *start;        /* start of sample window */
	SAMPLE   tmp;           /* scratch */
	int      d, i;          /* counter */
	int      t0;            /* position of spike */
	char     posstr[BC_LINELTH+1]; /* position qualifier value */

	/* executable code */

	if  (cp_pnexc(par,4,status))  return;
	trc = ml_get1trcpar( par, 1, tc, "   trace: ", status );
	if  (Severe(status))  return;
	ml_windowpars( par, 2, tc, "   lo-bound: ", "   hi-bound: ",
		&lowdw, &hiwdw, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 4, tc, "   control: ", &reg, status );
	if  (Severe(status))  return;
	loidx = dm_getsample( trc, lowdw, TRUE );
	hiidx = dm_getsample( trc, hiwdw, TRUE );
	if  (loidx >= hiidx)  {
		*status = SHE_ZWDW;
		return;
	} /*endif*/
	trclth = (int)(hiidx - loidx + 1);
	start = (SAMPLE *)db_getp( trc, EP_DATA, NULL ) + loidx;

	/* find position of spike */
	t0 = 0;
	if  (!sl_quals(par,"POS",BC_LINELTH,posstr,status))  {
		if  (Severe(status))  return;
		t0 = 2;  /* position not specified, take center */
	} else if  (strcmp(posstr,"MAX") == 0)  {
		t0 = 1;  /* take position on maximum */
	} else if  (strcmp(posstr,"CM") == 0)  {
		t0 = 2;  /* take center of mass */
	} else if  (strcmp(posstr,"END") == 0)  {
		t0 = 3;  /* take end of wavelet */
	} else if  (sscanf(posstr,"%f",&t0pos) != 1)  {
		*status = SHE_ILPAR;  /* illegal position */
		err_setcontext( " ## position " ); err_setcontext( posstr );
		return;
	} /*endif*/
	if  (t0 == 0)  {  /* position specified directly */
		t0 = Nint( t0pos / db_getr(trc,ER_DELTA,NULL) );
	} else if  (t0 == 1)  {   /* set t0 at maximum */
		tmp = *start;
		t0 = 0;
		for  (i=1; i<trclth; i++)
			if  (Abs(start[i]) > tmp)  {
				tmp = Abs(start[i]);
				t0 = i;
			} /*endif*/
	} else if  (t0 == 3)  {   /* take end of wavelet */
		t0 = trclth-1;
	} else {  /* "center of mass" */
		tmp = 0.;
		t0pos = 0.;
		for  (i=0; i<trclth; i++)  {
			tmp += (float)i * Abs(start[i]);
			t0pos += Abs(start[i]);
		} /*endfor*/
		t0 = Nint( tmp / t0pos );
	} /*endif*/
	if  (shflags_shv & SHF_CHATTY)  {
		t0pos = (float)t0*db_getr(trc,ER_DELTA,NULL);
		sprintf( posstr, "%s took spike at %f\n", SHC_CHATTXT, t0pos );
		gc_write( cc, posstr );
		sprintf( posstr, "spiking %g", t0pos );
	} /*endfor*/

	if  (!sl_qualr(par,"LTH",&lthfac,status))  {
		if  (Severe(status))  return;
		outlth = trclth/2;
	} else {
		outlth = Nint( (float)trclth*lthfac );
	} /*endif*/

	/* create new trace */
	db_newlist();
	new = db_create( status );
	if  (Severe(status))  return;
	f = (SAMPLE *)sy_allocmem( (long)outlth, (int)sizeof(SAMPLE), status );
	if  (Severe(status))  {
		db_delete( new );
		return;
	} /*endif*/
	ac = (SAMPLE *)sy_allocmem( 2L*(long)outlth, (int)sizeof(SAMPLE), status );
	if  (Severe(status))  {
		sy_deallocmem( f );
		db_delete( new );
		return;
	} /*endif*/
	ccr = ac + outlth;

	/* compute autocorrelation */
	mov = ac-1;
	for  (d=0; d<outlth; d++)  {  /* shift counter */
		*(++mov) = 0.;
		for  (i=d; i<trclth; i++)     /* sample counter */
			*mov += start[i]*start[i-d];
	} /*endfor*/
	*ac *= reg+1.0;

	/* compute correlation of start[0..trclth-1] with spike at pos t0 */
	if  (t0 < 0)  t0 = 0;
	if  (t0 >= outlth)  t0 = outlth-1;
	mov = ccr;
	for  (i=0; i<=t0; i++)
		*mov++ = start[t0-i];
	for  (i=t0+1; i<outlth; i++)
		*mov++ = 0.;

#	ifdef XXX
	for  (i=0; i<outlth; ccr[i++] = 0.) {}
	ccr[t0] = 1.0;
#	endif

	mt_levinson( ac, ccr, f, outlth, status );
	if  (Severe(status))  {
		sy_deallocmem( ac );
		sy_deallocmem( f );
		db_delete( new );
		return;
	} /*endif*/

	/* set trace info */
	ml_cpyinfo( trc, new, status );
	if  (Severe(status))  {
		sy_deallocmem( ac );
		sy_deallocmem( f );
		db_delete( new );
		return;
	} /*endif*/
	ml_inittrc( new, f, outlth, db_getr(trc,ER_DELTA,NULL) );
	db_sets( new, ES_OPINFO, posstr, status );

	sy_deallocmem( ac );

} /* end of mn3_spikefil */



/*--------------------------------------------------------------------*/



void mn3_arp( PARAM *par, STATUS *status )

/* autoregressive process
 * par 1:               trace list
 * par 2:               order of process N
 * par 3 .. 3+N-1:      process coefficients
 * par 3+N .. 3+2*N-1:  start values (default 0.)
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * STATUS     *status;   output; return status
 */
{
	/* local constants */
#	define MAXORDER 6

	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];    /* traces to be processed */
	int      listlth;               /* length of trace list */
	int      order;                 /* order of process */
	int      i;                     /* counter */
	REAL     coeff[MAXORDER];       /* ARP coefficients */
	REAL     start[MAXORDER];       /* start values */
	char     str[BC_LINELTH+1];     /* scratch string */
	REAL     min, max;              /* minimum and maximum of modified trace */

	/* executable code */

	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	cp_getint( par, 2, tc, "   order of process: ", &order, status );
	if  (Severe(status))  return;
	if  (order > MAXORDER)  {
		*status = SHE_SPECERROR+18;
		err_setcontext( " ## order " ); err_setcontext_l( order );
		return;
	} /*endif*/

	/* get process coefficients */
	for  (i=0; i<order; i++)  {
		sprintf( str, "   %d. ARP coeff: ", i+1 );
		cp_getfloat( par, i+3, tc, str, coeff+i, status );
		if  (Severe(status))  return;
	} /*endfor*/

	/* get start values */
	for  (i=0; i<order; i++)  {
		if  (cp_pentered(par,i+3+order,status))  {
			cp_getfloat( par, i+3+order, tc, "@@", start+i, status);
		} else{
			start[i] = 0.;
		} /*endif*/
		if  (Severe(status))  return;
	} /*endfor*/

	/* do ARP */
	for  (i=0; i<listlth; i++)  {
		mt_arp( order, coeff, start, db_getp(trc[i],EP_DATA,NULL),
			db_getl(trc[i],EL_LENGTH,NULL) );
		db_setf( trc[i], EF_MODIF, TRUE, NULL );
		sl_findmax( db_getp(trc[i],EP_DATA,NULL),
			db_getl(trc[i],EL_LENGTH,NULL), &min, &max );
		db_setr( trc[i], ER_MINVAL, min, NULL );
		db_setr( trc[i], ER_MAXVAL, max, NULL );
	} /*endfor*/

#	undef MAXORDER

} /* end of mn3_arp */



/*--------------------------------------------------------------------*/

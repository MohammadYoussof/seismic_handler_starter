
/* file SHMENUX.C
 *      =========
 *
 * version 33, 26-Jul-2006
 *
 * menu routines for seismhandler
 * K. Stammler, 30-JUL-1990
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "shconst.h"
#include "shvars.h"
#include "cpusrdef.h"
#include "glusrdef.h"
#include "uiusrdef.h"
#include "ssusrdef.h"
#include BC_GCUSRDEF
#include "ttusrdef.h"
#include "tcusrdef.h"
#include "infoidx.h"
#include "fctxsl.h"
#include "fctxdm.h"
#include "fctxml.h"
#include "fctxcr.h"
#include "fctxmt.h"
#include "fctxpm.h"
#include "residual.h"
#include "sherrors.h"
#ifdef SH_SOCKET
#include "port_io.h"
#endif



/*----------------------------------------------------------------------------*/



void mnx_locate( PARAM *par, int *status )

/* locates events using arrival times at array stations
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  trace list
 * 2. param:  azimuth variable
 * 3. param:  slowness variable
 * 4. param:  azimuth-error variable
 * 5. param:  slowness-error variable
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];       /* trace list */
	REAL     shift[SHC_ILISTLTH];      /* shift time in sec */
	REAL     x[SHC_ILISTLTH];          /* plane x-positions */
	REAL     y[SHC_ILISTLTH];          /* plane y-positions */
	STATLOC  lat[SHC_ILISTLTH];        /* surface latitude positions */
	STATLOC  lon[SHC_ILISTLTH];        /* surface longitude positions */
	int      arrcode[SHC_ILISTLTH];    /* array codes */
	REAL     elev[SHC_ILISTLTH];       /* station elevations */
	REAL     veloc[SHC_ILISTLTH];      /* average velocities */
	int      listlth;                  /* list length */
	BOOLEAN  same_array;               /* all traces from same array */
	int      i;                        /* counter */
	int      selmode;                  /* select mode */
	char     station[BC_SHORTSTRLTH+1];/* current station name */
	REAL     azimuth, azimerr;         /* azimuth & error */
	REAL     slowness, slowerr;        /* slowness & error */
	REAL     dt;                       /* sample distance */
	char     str[BC_LINELTH+1];        /* scratch */
	char     symbol[BC_SHORTSTRLTH+1]; /* symbol name */
	int      inplev;                   /* input level */
	REAL     default_vel;              /* default velocity */
	GLT_STATINF *statinf;              /* pointer to station info */

	/* executable code */

	if  (cp_pnexc(par,5,status))  return;

	selmode = (cp_qexist(par,"NOMARK")) ? MM_NOMARK : MM_TRCMARK;
	if  (cp_qexist(par,"PARENT"))  {
		inplev = -1;
	} else if  (cp_qexist(par,"INTERACTIVE"))  {
		inplev = shv_maininput;
	} else {
		inplev = ss_inplev();
	} /*endif*/

	if  (!sl_qualr(par,"VELOC",&default_vel,status))
		default_vel = 0.0;

	if  (cp_pentered(par,1,status))  {
		ml_gettrcpar( par, 1, tc, "@@", trc, &listlth, status );
		if  (*status != SHE_NOERROR)  return;
		for  (i=0;i<listlth;i++)
			shift[i] = -db_getr( trc[i], ER_TORIG, NULL );
	} else {   /* select time points by cursor */
		if  (*status != SHE_NOERROR)  return;
		ui_shift( inplev );
		ml_crsrlst_time( selmode, trc, shift, &listlth, status );
		ui_unshift();
		for  (i=0;i<listlth;i++)
			shift[i] = -shift[i];
	} /*endif*/

	same_array = TRUE;
	for  (i=0;i<listlth;i++)  {
		db_gets( trc[i], ES_STATION, BC_SHORTSTRLTH, station, status );
		if  (*status != SHE_NOERROR)  return;
		/* gl_full_statloc( station, lat+i, lon+i, arrcode+i, x+i, y+i,
			0, NULL, status ); */
		statinf = (GLT_STATINF *)db_getp( trc[i], EP_STAT, status );
		if  (statinf == NULL)  {
			*status = BC_NOERROR;
			statinf = gl_store_station( station, TRUE, status );
			if  (Severe(status))  return;
			db_setp( trc[i], EP_STAT, statinf, status );
			if  (Severe(status))  return;
		} /*endif*/
		lat[i] = statinf->lat;
		lon[i] = statinf->lon;
		arrcode[i] = statinf->arr_id;
		x[i] = statinf->xrel;
		y[i] = statinf->yrel;
		if  ((i > 0) && (arrcode[i] != *arrcode))  same_array = FALSE;
		elev[i] = statinf->elevation;
		veloc[i] = default_vel == 0.0 ? statinf->velocity : default_vel;
	} /*endfor*/
	if  (same_array)  {
		if  (*arrcode == 0)  same_array = FALSE;
		if  (cp_qexist(par,"NOARRAY"))  same_array = FALSE;
	} /*endif*/

	if  (!same_array)
		mt_deg_to_km( listlth, lat, lon, x, y );

	/* check delta-t if only three traces */
	if  (listlth == 3)  {
		dt = db_getr( trc[0], ER_DELTA, NULL );
		if  ((db_getr(trc[1],ER_DELTA,NULL) != dt)  ||
			(db_getr(trc[2],ER_DELTA,NULL) != dt))  {
			*status = SHE_DIFFSAMP;
			return;
		} /*endif*/
	} /*endif*/

	/* perform location */
	mt_locate( listlth, shift, x, y, dt,
		&azimuth, &azimerr, &slowness, &slowerr, status );
	if  (Severe(status))  return;
	azimuth -= 180.0;
	if  (azimuth < 0.0)  azimuth += 360.0;

	if  (cp_qexist(par,"ELEVATION"))  {
		mt_elevation_correction( slowness, listlth, elev, veloc,
			shift, status );
		if  (Severe(status))  return;
		/* do it again */
		mt_locate( listlth, shift, x, y, dt,
			&azimuth, &azimerr, &slowness, &slowerr, status );
		if  (Severe(status))  return;
		azimuth -= 180.0;
		if  (azimuth < 0.0)  azimuth += 360.0;
	} /*endif*/

	/* store or display results */
	if  (cp_pnum(par) <= 1)  {  /* no output variables -> display */
		sprintf( str, "   azimuth : %+7.2f  +- %6.2f\n", azimuth, azimerr );
		gc_write( tc, str );
		sprintf( str, "   slowness: %5.2f    +- %5.2f\n", slowness, slowerr );
		gc_write( tc, str );
	} else {
		if  (cp_pentered(par,2,status))  {
			cp_getstr( par, 2, tc, "@@", BC_LINELTH, symbol, status );
			if  (*status != SHE_NOERROR)  return;
			sprintf( str, "%e", azimuth );
			sl_setsymbol( symbol, str, status );
		} /*endif*/
		if  (cp_pentered(par,3,status))  {
			cp_getstr( par, 3, tc, "@@", BC_LINELTH, symbol, status );
			if  (*status != SHE_NOERROR)  return;
			sprintf( str, "%e", slowness );
			sl_setsymbol( symbol, str, status );
		} /*endif*/
		if  (cp_pentered(par,4,status))  {
			cp_getstr( par, 4, tc, "@@", BC_LINELTH, symbol, status );
			if  (*status != SHE_NOERROR)  return;
			sprintf( str, "%e", azimerr );
			sl_setsymbol( symbol, str, status );
		} /*endif*/
		if  (cp_pentered(par,5,status))  {
			cp_getstr( par, 5, tc, "@@", BC_LINELTH, symbol, status );
			if  (*status != SHE_NOERROR)  return;
			sprintf( str, "%e", slowerr );
			sl_setsymbol( symbol, str, status );
		} /*endif*/
	} /*endif*/

} /* end of mnx_locate */



/*----------------------------------------------------------------------------*/



#define CLEAR_AND_RETURN \
	sy_deallocmem( shift ); \
	sy_deallocmem( lat ); \
	sy_deallocmem( arrcode ); \
	return;



void mnx_beam( PARAM *par, STATUS *status )

/* shift traces according to their station location and given azimuth
 * and slowness
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * STATUS   *status;        output; return status
 *
 * 1. param:  trace list
 * 2. param:  azimuth
 * 3. param:  slowness
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];    /* trace list */
	REAL     *shift;                /* shift time in sec */
	REAL     *x;                    /* plane x-positions */
	REAL     *y;                    /* plane y-positions */
	STATLOC  *lat;                  /* surface latitude positions */
	STATLOC  *lon;                  /* surface longitude positions */
	REAL     *elev;                 /* station elevations */
	REAL     *veloc;                /* average velocities */
	REAL     *residcorr;            /* residual corrections */
	int      *arrcode;              /* array codes */
	int      listlth;               /* list length */
	BOOLEAN  same_array;            /* all traces from same array */
	int      i;                     /* counter */
	char     station[BC_SHORTSTRLTH+1]; /* current station name */
	REAL     azimuth;               /* azimuth */
	REAL     slowness;              /* slowness */
	REAL     minshift;              /* minimum shift time */
	char     str[BC_LINELTH+1];     /* scratch string */
	REAL     default_vel;           /* default velocity */
	int      refidx;                /* reference index */
	char     refstat[BC_SHORTSTRLTH+1]; /* reference station name */
	GLT_STATINF  *statinf;          /* pointer to station info */
	BOOLEAN  use_refstation;        /* reference station is specified */
	char     residph[BC_SHORTSTRLTH+1]; /* residual phase */

	/* executable code */

	if  (cp_pnexc(par,3,status))  return;

	ml_gettrcpar( par, 1, tc, "   beam traces: ", trc, &listlth, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getfloat( par, 2, tc, "   azimuth: ", &azimuth, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getfloat( par, 3, tc, "   slowness: ", &slowness, status );
	if  (*status != SHE_NOERROR)  return;

	if  (!sl_quals(par,"RESID",BC_SHORTSTRLTH,residph,status))
		*residph = '\0';
	if  (SySevere(status))  return;

	/* allocate memory for station info, listlth is increased by 1: */
	/*    index 'listlth' contains info about reference station   */
	shift = (REAL *)sy_allocmem( (listlth+1)*6, (int)sizeof(REAL), status );
	if  (Severe(status))  return;
	lat = (STATLOC *)sy_allocmem( (listlth+1)*2, (int)sizeof(STATLOC), status );
	if  (Severe(status))  {sy_deallocmem(shift); return;}
	arrcode = (int *)sy_allocmem( listlth+1, (int)sizeof(int), status );
	if  (Severe(status))  {sy_deallocmem(shift); sy_deallocmem(lat); return;}
	x = shift + listlth+1;
	y = x + listlth+1;
	elev = y + listlth+1;
	veloc = elev + listlth+1;
	residcorr = elev + listlth+1;
	lon = lat + listlth+1;

	/* rescale input if sx,sy is given */
	if  (cp_qexist(par,"CART"))  {
		/* compute azimuth & slowness from cartesian input */
		minshift = BC_PI/2.0 - atan2( azimuth, slowness );
		slowness = sqrt( azimuth*azimuth + slowness*slowness );
		azimuth = minshift;
	} /*endif*/

	if  (!sl_qualr(par,"VELOC",&default_vel,status))
		default_vel = 0.0;
	if  (SySevere(status))  {CLEAR_AND_RETURN}

	/* get reference station, refidx is set to 'listlth' */
	refidx = listlth;
	use_refstation = FALSE;
	if  (sl_quals(par,"REF",BC_SHORTSTRLTH,refstat,status))  {
		if  (strcmp(refstat,"CENTRE") == 0 || strcmp(refstat,"CENTER") == 0)  {
			/* take center of array */
			*refstat = '\0';
		} else {
			/* retrieve station info about reference station */
			statinf = gl_store_station( refstat, TRUE, status );
			if  (Severe(status))  {CLEAR_AND_RETURN}
			lat[refidx] = statinf->lat;
			lon[refidx] = statinf->lon;
			arrcode[refidx] = statinf->arr_id;
			x[refidx] = statinf->xrel;
			y[refidx] = statinf->yrel;
			use_refstation = TRUE;
		} /*endif*/
	} else {
		/* take center of array */
		*refstat = '\0';
	} /*endif*/
	if  (Severe(status))  {CLEAR_AND_RETURN}

	/* retrieve station info of all traces and store it in the arrays */
	same_array = TRUE;
	for  (i=0;i<listlth;i++)  {
		db_gets( trc[i], ES_STATION, BC_SHORTSTRLTH, station, status );
		if  (refidx < 0)  /* reference station not yet found */
			if  (strcmp(station,refstat) == 0)
				refidx = i;
		if  (Severe(status))  {CLEAR_AND_RETURN}
		statinf = (GLT_STATINF *)db_getp( trc[i], EP_STAT, status );
		if  (statinf == NULL)   {
			*status = BC_NOERROR;
			statinf = gl_store_station( station, TRUE, status );
			if  (Severe(status))  {CLEAR_AND_RETURN}
			db_setp( trc[i], EP_STAT, statinf, status );
			if  (Severe(status))  {CLEAR_AND_RETURN}
		} /*endif*/
		lat[i] = statinf->lat;
		lon[i] = statinf->lon;
		arrcode[i] = statinf->arr_id;
		x[i] = statinf->xrel;
		y[i] = statinf->yrel;
		if  ((i > 0) && (arrcode[i] != *arrcode))  same_array = FALSE;
		elev[i] = statinf->elevation;
		veloc[i] = default_vel == 0.0 ? statinf->velocity : default_vel;
		if  (*residph != '\0')  {
			residcorr[i] = RsResidual( station, residph, slowness, azimuth,
				status );
			if  (SySevere(status))  {CLEAR_AND_RETURN}
		} /*endif*/
	} /*endfor*/

	/* check for use of absolute or relative locations */
	if  (same_array && use_refstation && *arrcode != arrcode[refidx])
		same_array = FALSE;
	if  (same_array)  {
		if  (*arrcode == 0)  same_array = FALSE;
		if  (cp_qexist(par,"NOARRAY"))  same_array = FALSE;
	} /*endif*/

	/* relative locations from absolute if relative loc's are not already given*/
	if  (!same_array)  {
		if  (use_refstation)  {
			mt_deg_to_km( listlth+1, lat, lon, x, y );
		} else {
			mt_deg_to_km( listlth, lat, lon, x, y );
		} /*endif*/
	} /*endif*/

	/* for center reference find mean value position */
	if  (!use_refstation)  {
		x[refidx] = y[refidx] = 0.0;
		for  (i=0; i<listlth; i++)  {
			x[refidx] += x[i];
			y[refidx] += y[i];
		} /*endfor*/
		x[refidx] /= (float)listlth;
		y[refidx] /= (float)listlth;
		elev[refidx] = 0.0;
	} /*endif*/

	/* dump out relative locations if requested */
	if  (cp_qexist(par,"DUMPREL"))
		for  (i=0; i<listlth; i++)
			printf( "%f %f\n", x[i]-x[refidx], y[i]-y[refidx] );

	/* compute shift times from relative locations */
	mt_beamshift( listlth+1, x, y, azimuth, slowness, shift );
	if  (cp_qexist(par,"ELEVATION"))
		mt_elevation_correction( slowness, listlth+1, elev, veloc,
			shift, status );
	if  (Severe(status))  {CLEAR_AND_RETURN}
	if  (*residph != '\0')
		for  (i=0; i<listlth; i++)
			shift[i] -= residcorr[i];

	/* reverse sign if requested */
	if  (cp_qexist(par,"NEG"))
		for  (i=0; i<=listlth; i++)
			shift[i] = -shift[i];

	/* set reference delay */
	minshift = shift[refidx];

	/* now do the shift operation */
	sprintf( str, "%e %e %e", x[refidx], y[refidx], (*shift)-minshift );
	db_sets( trc[0], ES_OPINFO, str, status );
	if  (Severe(status))  {CLEAR_AND_RETURN}
	if  (cp_qexist(par,"ABS"))  {
		for  (i=0;i<listlth;i++)  {
			db_setr( trc[i], ER_TORIG, shift[i]-minshift, status );
			if  (*status != SHE_NOERROR)  {CLEAR_AND_RETURN}
		} /*endfor*/
	} else {
		for  (i=0;i<listlth;i++)  {
			db_setr( trc[i], ER_TORIG,
				db_getr(trc[i],ER_TORIG,NULL)+shift[i]-minshift, status );
			if  (*status != SHE_NOERROR)  {CLEAR_AND_RETURN}
		} /*endfor*/
	} /*endif*/

	CLEAR_AND_RETURN

} /* end of mnx_beam */



#undef CLEAR_AND_RETURN



/*----------------------------------------------------------------------------*/



void mnx_shift( PARAM *par, int *status )

/* shift traces
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  trace list
 * 2. param:  time (explicit shift)
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];     /* trace list */
	int      listlth;                /* list length */
	int      i;                      /* counter */
	REAL     orig, shift;            /* shift time(s) */
	int      selmode;                /* selection mode */
	char     shiftcmd[BC_LINELTH+1]; /* shift command */
	REAL     cnvdepth;               /* conversion depth */
	char     infstr[BC_LINELTH+1];   /* info entry string */
	unsigned infent;                 /* info entry */
	REAL     shiftoff;               /* shift offset */
	int      diff_phase;             /* phase difference */
	BOOLEAN  negative;               /* negative shift */
	int      inplev;                 /* input level */

	/* executable code */

	if  (cp_pnexc(par,4,status))  return;
	if  (cp_qexist(par,"PARENT"))  {
		inplev = -1;
	} else if  (cp_qexist(par,"INTERACTIVE"))  {
		inplev = shv_maininput;
	} else {
		inplev = ss_inplev();
	} /*endif*/

	negative = cp_qexist( par, "NEG" );

	if  (cp_pnum(par) == 0)  {   /* shift by graphic cursor */

		selmode = (cp_qexist(par,"NOMARK")) ? MM_NOMARK : MM_TRCMARK;
		ui_shift( inplev );
		cr_gettrctime( selmode, trc, &orig, status );
		ui_unshift();
		if  (*status != SHE_NOERROR)  return;
		ui_shift( inplev );
		cr_gettrctime( selmode, trc+1, &shift, status );
		ui_unshift();
		if  (*status != SHE_NOERROR)  return;
		db_setr( trc[0], ER_TORIG,
			db_getr(trc[0],ER_TORIG,NULL)+shift-orig, status );

	} else {

		ml_gettrcpar( par, 1, tc, "   traces: ", trc, &listlth, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 2, tc, "   shift command: ", BC_LINELTH,
			shiftcmd, status );
		if  (Severe(status))  return;

		if  (strcmp(shiftcmd,"PS_DELAY") == 0)  {
			diff_phase = TTC_P_S_DIFF;
		} else if  (strcmp(shiftcmd,"P_PPP_DELAY") == 0)  {
			diff_phase = TTC_P_Ppp_DIFF;
		} else if  (strcmp(shiftcmd,"P_PPS_DELAY") == 0)  {
			diff_phase = TTC_P_Pps_DIFF;
		} else {
			diff_phase = 0;
		} /*endif*/

		if  (diff_phase != 0)  {
			cp_getfloat( par, 3, tc, "   conv depth (km): ", &cnvdepth,
				status );
			if  (Severe(status))  return;
			cp_getstr( par, 4, tc, "   info entry: ", BC_LINELTH,
				infstr, status );
			if  (Severe(status))  return;
			db_ident( infstr, &infent, status );
			if  (Severe(status))  return;
			shiftoff = tt_mantle_diff( diff_phase, cnvdepth,
				6.4, status );
			for  (i=0;i<listlth;i++)  {
				shift = tt_mantle_diff( diff_phase, cnvdepth,
					db_getr(trc[i],infent,status), status ) -
					shiftoff;
				if  (Severe(status))  return;
				if  (negative)  shift = -shift;
				db_setr( trc[i], ER_TORIG,
					db_getr(trc[i],ER_TORIG,NULL)-shift, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endfor*/
		} else {
			if  (strcmp(shiftcmd,"RED_TIME") == 0)  {
				cp_getfloat( par, 3, tc, "   velocity: ", &shift, status );
				if  (Severe(status))  return;
				ml_reduced_time( listlth, trc, shift, status );
				if  (Severe(status))  return;
			} else if  (strcmp(shiftcmd,"TIME_AL") == 0)  {
				if  (cp_pentered(par,3,status))  {
					cp_getstr( par, 3, tc, "@@", BC_LINELTH,
						infstr, status );
				} else {
					*infstr = '\0';
				} /*endif*/
				if  (Severe(status))  return;
				ml_time_align( listlth, trc, infstr, status );
				if  (Severe(status))  return;
			} else if  (strcmp(shiftcmd,"SD_DELAY") == 0)  {
				cp_getfloat( par, 3, tc, "   slowness: ", &shiftoff, status );
				if  (Severe(status))  return;
				cp_getstr( par, 4, tc, "   distance entry: ", BC_LINELTH,
					infstr, status );
				if  (Severe(status))  return;
				ml_sddelay( listlth, trc, shiftoff, infstr, status );
				if  (Severe(status))  return;
			} else {
				cp_getfloat( par, 2, tc, "   time (sec): ", &shift, status );
				if  (*status != SHE_NOERROR)  return;
				if  (negative)  shift = -shift;
				for  (i=0;i<listlth;i++)  {
					db_setr( trc[i], ER_TORIG,
						db_getr(trc[i],ER_TORIG,NULL)+shift, status );
					if  (*status != SHE_NOERROR)  return;
				} /*endfor*/
			} /*endif*/
		} /*endif*/

	} /*endif*/

} /* end of mnx_shift */



/*-------------------------------------------------------------------------*/



void mnx_al( PARAM *par, int *status )

/* align traces
 * 1. param:  trace list
 * 2. param:  time info entry
 * 3. param:  time position (default 0)
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	REAL     timpos;              /* time position of aligning */
	void     *trc[SHC_ILISTLTH];  /* trace list */
	REAL     shift[SHC_ILISTLTH]; /* shift time in sec */
	int      listlth;             /* list length */
	int      i;                   /* counter */
	int      selmode;             /* select mode */
	unsigned tentry;              /* time entry */
	TIME     ttime, start;        /* info time & start time of trace */
	char     infstr[BC_SHORTSTRLTH+1];  /* info entry string */
	int      inplev;              /* input level, for mouse selections */

	/* executable code */

	if  (cp_pnexc(par,3,status))  return;

	if  (cp_pentered(par,3,status))  {
		cp_getfloat( par, 3, tc, "@@", &timpos, status );
		if  (*status != SHE_NOERROR)  return;
	} else {
		timpos = 0.0;
	} /*endif*/

	/* get input level, for mouse selections only */
   if  (cp_qexist(par,"PARENT"))  {
      inplev = -1;
   } else if  (cp_qexist(par,"INTERACTIVE"))  {
      inplev = shv_maininput;
   } else {
      inplev = ss_inplev();
   } /*endif*/

	if  (cp_pentered(par,1,status))  {  /* time points from info entry */

		/* get trace list from 1. parameter and time info from 2. par */
		ml_gettrcpar( par, 1, tc, "@@", trc, &listlth, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 2, tc, "   time info entry: ", BC_SHORTSTRLTH,
			infstr, status );
		if  (*status != SHE_NOERROR)  return;

		/* check info entry string */
		db_ident( infstr, &tentry, status );
		if  (*status != SHE_NOERROR)  return;
		if  ((tentry & E_TYPMASK) != ET_TYPE)  {
			*status = SHE_ILTYPE;
			return;
		} /*endif*/

		/* get time offsets from info entry */
		if  (tentry == ET_START)  {
			for  (i=0;i<listlth;i++)  {
				db_setr( trc[i], ER_TORIG, timpos, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endfor*/
		} else {
			for  (i=0;i<listlth;i++)  {
				db_gett( trc[i], tentry, &ttime, status );
				if  (*status != SHE_NOERROR)  return;
				db_gett( trc[i], ET_START, &start, status );
				if  (*status != SHE_NOERROR)  return;
				db_setr( trc[i], ER_TORIG,
					timpos+tc_adiff(&start,&ttime), status );
				if  (*status != SHE_NOERROR)  return;
			} /*endfor*/
		} /*endif*/

	} else {   /* trace & delay selection by graphic cursor */

		/* select traces and time points by graphic cursor */
		selmode = (cp_qexist(par,"NOMARK")) ? MM_NOMARK : MM_TRCMARK;
		ui_shift( inplev );
		ml_crsrlst_time( selmode, trc, shift, &listlth, status );
		ui_unshift();
		if  (*status != SHE_NOERROR)  return;

		/* shift traces */
		for  (i=0;i<listlth;i++)  {
			db_setr( trc[i], ER_TORIG,
				timpos+db_getr(trc[i],ER_TORIG,NULL)-shift[i], status );
			if  (*status != SHE_NOERROR)  return;
		} /*endfor*/
	
	} /*endif*/

} /* end of mnx_al */



/*-------------------------------------------------------------------------*/



void mnx_syw( PARAM *par, int *status )

/* sets y-window in display
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	float      lo_y, hi_y;      /* y-window */

	/* executable code */

	if  (cp_pnum(par) == 0)  {
		cr_ywindow( MM_PTMARK, &lo_y, &hi_y, status );
		if  (*status != SHE_NOERROR)  return;
	} else {
		if  (cp_pnexc(par,2,status))  return;
		cp_getfloat( par, 1, tc, "   lower bound: ", &lo_y, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 2, tc, "   upper bound: ", &hi_y, status );
		if  (*status != SHE_NOERROR)  return;
	} /*endif*/
	dm_ywdw( lo_y, hi_y, status );

} /* end of mnx_syw */



/*-------------------------------------------------------------------------*/



void mnx_styw( PARAM *par, int *status )

/* sets time & y-window in display
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	float      lo_y, hi_y;      /* y-window */
	float      lo_t, hi_t;      /* time window */

	/* executable code */

	if  (cp_pnum(par) == 0)  {
		cr_tywindow( MM_PTMARK, &lo_t, &hi_t, &lo_y, &hi_y, status );
		if  (*status != SHE_NOERROR)  return;
	} else {
		if  (cp_pnexc(par,4,status))  return;
		cp_getfloat( par, 1, tc, "   lower t-bound: ", &lo_t, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 2, tc, "   upper t-bound: ", &hi_t, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 3, tc, "   lower y-bound: ", &lo_y, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 4, tc, "   upper y-bound: ", &hi_y, status );
		if  (*status != SHE_NOERROR)  return;
	} /*endif*/
	dm_timewdw( lo_t, hi_t, status );
	dm_ywdw( lo_y, hi_y, status );

} /* end of mnx_styw */



/*----------------------------------------------------------------------------*/



void mnx_yinfo( PARAM *par, STATUS *status )

/* sets y-info entry in display
 * 1. param:   info entry name
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	unsigned ientry;               /* info entry */
	char     infstr[BC_LINELTH+1]; /* info entry name */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	cp_getstr( par, 1, tc, "   info entry or NONE: ", BC_LINELTH,
		infstr, status );
	if  (Severe(status))  return;
	if  (strcmp(infstr,"NONE") == 0)  {
		ientry = E_NONE;
	} else {
		db_ident( infstr, &ientry, status );
		if  (Severe(status))  return;
	} /*endif*/
	dm_setyentry( ientry, status );

} /* end of yinfo */



/*----------------------------------------------------------------------------*/



void mnx_am( PARAM *par, int *status )

/* determine amplitudes
 * 1. param:  trace
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	TRACE    *trc;                 /* trace pointer */
	REAL     lo_time, hi_time;     /* time window */
	int      selmode;              /* selection mode */
	REAL     r;
	long     start, lth;           /* trace window */
	long     minpos, maxpos;       /* positions of min & max */
	SAMPLE   min, max;             /* minimum & maximum */
	char     str[BC_LINELTH+1];    /* scratch string */
	char     symbol[BC_LINELTH+1]; /* symbol name */
	REAL     dt;                   /* sample distance */
	int      inplev;               /* input level, for cursor selections */

	/* executable code */

	if  (cp_pnexc(par,7,status))  return;
	selmode = (cp_qexist(par,"NOMARK")) ? MM_NOMARK : MM_TRCMARK;

	/* get input level, for mouse selections only */
   if  (cp_qexist(par,"PARENT"))  {
      inplev = -1;
   } else if  (cp_qexist(par,"INTERACTIVE"))  {
      inplev = shv_maininput;
   } else {
      inplev = ss_inplev();
   } /*endif*/

	if  (cp_pnum(par) == 0)  {
		ui_shift( inplev );
		cr_gettrctime( selmode, &trc, &lo_time, status );
		if  (Severe(status))  {ui_unshift(); return;}
		cr_getloc( selmode, &hi_time, NULL, NULL, status );
		if  (Severe(status))  return;
		ui_unshift();
	} else {
		trc = ml_get1trcpar( par, 1, tc, "@@", status );
		if  (Severe(status))  return;
		if  (cp_pnum(par) > 1)  {
			cp_getfloat( par, 2, tc, "   lo-bound: ", &lo_time, status );
			if  (Severe(status))  return;
			cp_getfloat( par, 3, tc, "   hi-bound: ", &hi_time, status );
			if  (Severe(status))  return;
		} else {
			cr_window( selmode, &lo_time, &hi_time, status );
			if  (Severe(status))  return;
		} /*endif*/
	} /*endif*/
	if  (hi_time < lo_time)  {
		r = hi_time;
		hi_time = lo_time;
		lo_time = r;
	} /*endif*/
	start = dm_getsample( trc, lo_time, TRUE );
	lth = dm_getsample( trc, hi_time, TRUE ) - start + 1;

	sl_findmaxpos( (SAMPLE *)db_getp(trc,EP_DATA,NULL)+start, lth,
		&min, &max, &minpos, &maxpos );

	if  (cp_qexist(par,"ABS"))  {
		min = Abs( min );
		max = Abs( max );
		if  (min > max)  max = min;
		min = max;
	} /*endif*/

	if  (cp_pnum(par) <= 3)  {
		sprintf( str, "   min: %e    max: %e\n", min, max );
		gc_write( cc, str );
	} else {
		dt = db_getr( trc, ER_DELTA, NULL );
		if  (cp_pentered(par,4,status))  {
			cp_getstr( par, 4, tc, "@@", BC_LINELTH, symbol, status );
			if  (Severe(status))  return;
			sprintf( str, "%e", min );
			sl_setsymbol( symbol, str, status );
			if  (Severe(status))  return;
		} /*endif*/
		if  (cp_pentered(par,5,status))  {
			cp_getstr( par, 5, tc, "@@", BC_LINELTH, symbol, status );
			if  (Severe(status))  return;
			sprintf( str, "%e", max );
			sl_setsymbol( symbol, str, status );
			if  (Severe(status))  return;
		} /*endif*/
		if  (cp_pentered(par,6,status))  {
			cp_getstr( par, 6, tc, "@@", BC_LINELTH, symbol, status );
			if  (Severe(status))  return;
			sprintf( str, "%e", db_getr(trc,ER_TORIG,NULL) +
				start*dt + (REAL)minpos*dt );
			sl_setsymbol( symbol, str, status );
			if  (Severe(status))  return;
		} /*endif*/
		if  (cp_pentered(par,7,status))  {
			cp_getstr( par, 7, tc, "@@", BC_LINELTH, symbol, status );
			if  (Severe(status))  return;
			sprintf( str, "%e", db_getr(trc,ER_TORIG,NULL) +
				start*dt + (REAL)maxpos*dt );
			sl_setsymbol( symbol, str, status );
			if  (Severe(status))  return;
		} /*endif*/
	} /*endif*/

} /* end of mnx_am */



/*----------------------------------------------------------------------------*/



void mnx_resample( PARAM *par, int *status )

/* resamples traces
 * 1. param:  trace list
 * 2. param:  new sample distance (in sec)
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];  /* trace pointer */
	int      listlth;             /* length of trace list */
	REAL     new_dt;              /* new sample distance in sec */
	int      i;                   /* counter */
	REAL     min, max;            /* new minimum and maximum */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;

	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	cp_getfloat( par, 2, tc, "   new sample distance (sec): ",
		&new_dt, status );
	if  (Severe(status))  return;
	if  (new_dt <= 0.0)  {
		*status = SHE_ILPAR;
		return;
	} /*endif*/

	for  (i=0; i<listlth; i++ )  {
		ml_resample( trc[i], new_dt, status );
		if  (Severe(status)) return;
		sl_findmax( db_getp(trc[i],EP_DATA,NULL),
			db_getl(trc[i],EL_LENGTH,NULL), &min, &max );
		db_setr( trc[i], ER_MINVAL, min, status );
		if  (Severe(status))  return;
		db_setr( trc[i], ER_MAXVAL, max, status );
		if  (Severe(status))  return;
	} /*endfor*/

} /* end of mnx_resample */



/*----------------------------------------------------------------------------*/



void mnx_title( PARAM *par, STATUS *status )

/* sets new title
 * 1. param:  title line
 * 2. param:  text of title
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      lineno;             /* line number */
	char     text[BC_LINELTH+1]; /* new text of title */
	float    x, y;               /* title position */
	int      wdw;                /* window number */

	/* executable code */

	if  (cp_pnexc(par,4,status))  return;
	cp_getint( par, 1, tc, "   title line: ", &lineno, status );
	if  (Severe(status))  return;
	cp_getstr( par, 2, tc, "   title text: ", BC_LINELTH, text, status );
	if  (Severe(status))  return;

	if  (!sl_quali(par,"WDW",&wdw,status))
		wdw = gc;
	if  (Severe(status))  return;

	dm_settitle( wdw, --lineno, text, status );
	if  (Severe(status))  return;

	if  (cp_pentered(par,3,status))  {
		cp_getfloat( par, 3, tc, "@@", &x, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   y-pos: ", &y, status );
		if  (Severe(status))  return;
		dm_settitlepos( wdw, lineno, x, y, status );
	} /*endif*/

} /* end of mnx_title */



/*----------------------------------------------------------------------------*/



void mnx_hc( PARAM *par, STATUS *status )

/* makes hardcopy of window
 * 1. param:  channel map of destination channel
 * 2. param:  channel map of source channel
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     mapstr[BC_LINELTH+1];    /* map string */
	CHMAP    src, dst;                /* channel (bit)map */
	static CHMAP default_out=0;       /* default output channel */
	char     outfile[BC_FILELTH+1];   /* name of output file */
	char     symbol[BC_LINELTH+1];    /* symbol name */

	/* executable code */

	if  (cp_pnexc(par,3,status))  return;

	if  (sl_quals(par,"DEFAULT",BC_LINELTH,mapstr,status))  {
		sl_cnvgcflags( mapstr, &default_out, status );
		return;
	} /*endif*/
	if  (Severe(status))  return;

	if  (cp_pentered(par,1,status))  {
		cp_getstr( par, 1, tc, "   source channel: ", BC_LINELTH,
			mapstr, status );
		if  (Severe(status))  return;
		sl_cnvgcflags( mapstr, &src, status );
		if  (Severe(status))  return;
	} else {
		src = gc;
	} /*endif*/
	if  (cp_pentered(par,2,status))  {
		cp_getstr( par, 2, tc, "   dest channel(s): ", BC_LINELTH,
			mapstr, status );
		if  (Severe(status))  return;
		sl_cnvgcflags( mapstr, &dst, status );
		if  (Severe(status))  return;
	} else {
		dst = default_out;
	} /*endif*/

	if  (src == 0)  {
		gc_closeplot( dst, outfile, status );
	} else {
		gc_playback( src, dst, outfile, status );
	} /*endif*/

	if  (cp_pentered(par,3,status))  {
		cp_getstr( par, 3, tc, "   file symbol: ", BC_LINELTH,
			symbol, status );
		if  (Severe(status))  return;
		sl_setsymbol( symbol, outfile, status );
	} /*endif*/

#ifdef SH_SOCKET
	if  (ui_lastsocket() > 0)
		pio_send_file_to_socket( outfile, ui_lastsocket(), status );
#endif

} /* end of mnx_hc */



/*----------------------------------------------------------------------------*/



void mnx_pmch( PARAM *par, STATUS *status )

/* selects window for particle motion diagrams
 * 1. param:  window number (channel map)
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     mapstr[BC_LINELTH+1];    /* map string */
	int      map;                     /* channel (bit)map */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;

	if  (cp_pentered(par,1,status))  {
		cp_getstr( par, 1, tc, "   window: ", BC_LINELTH,
			mapstr, status );
		if  (Severe(status))  return;
		sl_cnvgcflags( mapstr, &map, status );
		if  (Severe(status))  return;
	} else {
		map = 0;
	} /*endif*/
	pm_setoutput( map );

} /* end of mnx_pmch */



/*----------------------------------------------------------------------------*/



void mnx_pm( PARAM *par, STATUS *status )

/* draws particle motion diagrams
 * 1. param: trace list
 * 2. param: lo bound of time window [FIXED]
 * 3. param: hi bound of time window [FIXED]
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	TRACE    *trc[SHC_ILISTLTH];       /* trace list */
	int      listlth;                  /* length of list */
	REAL     lowdw, hiwdw;             /* time window */
	REAL     loy, hiy;                 /* y range */
	REAL     dt;                       /* sample distance */
	REAL     origtime;                 /* origin time */
	int      t;                        /* trace counter */
	SAMPLE   *xstart[PMC_MAXPM];       /* x coo start samples */
	SAMPLE   *ystart[PMC_MAXPM];       /* y coo start samples */
	long     lth;                      /* length of pm in samples */
	float    tzoom[PMC_MAXPM];         /* trace zoom factors */
	float    zoom;                     /* general zoom */
	int      mode;                     /* 0=fixed, 1=varwdw, 2=movewdw */
	char     inftxt[PMC_MAXPM][BC_SHORTSTRLTH+1];   /* info texts */
	char     *inftxtptr[PMC_MAXPM];    /* pointer to info texts */
	char     cmp[2];                   /* component */
	STATUS   locstat;                  /* local status */
	int      inplev;                   /* input level */
	REAL     begcirc;                  /* radius of start circle */

	/* executable code */

	mode = 0;
	if  (cp_qexist(par,"VWDW"))  mode = 1;
	if  (cp_qexist(par,"MWDW"))  mode = 2;

	if  (cp_qexist(par,"PARENT"))  {
		inplev = -1;
	} else if  (cp_qexist(par,"INTERACTIVE"))  {
		inplev = shv_maininput;
	} else {
		inplev = ss_inplev();
	} /*endif*/

	if  (!sl_qualr(par,"CIRCLE",&begcirc,status))
		begcirc = 0.;
	if  (Severe(status))  return;

	if  (cp_pnexc(par,3,status))  return;
	ml_gettrcpar( par, 1, tc, "   trace list: ", trc, &listlth, status );
	if  (Severe(status))  return;
	if  (mode == 0)  {
		cp_getfloat( par, 2, tc, " lo-wdw: ", &lowdw, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, " hi-wdw: ", &hiwdw, status );
		if  (Severe(status))  return;
	} else if  (mode == 2)  {
		cp_getfloat( par, 2, tc, " width: ", &origtime, status );
		if  (Severe(status))  return;
	} /*endif*/

	if  (!sl_qualr(par,"ZOOM",&zoom,status))
		zoom = 1.0;

	/* set info texts */
	cmp[1] = '\0';
	for  (t=0; t<(listlth/2); t++)  {
		inftxt[t][0] = '\0';
		inftxtptr[t] = inftxt[t];
		db_gets( trc[t*2], ES_STATION, BC_SHORTSTRLTH-4, inftxt[t], status );
		if  (*status == SHE_NOINFO)  *status = SHE_NOERROR;
		if  (Severe(status))  return;
		strcat( inftxt[t], " " );
		locstat = SHE_NOERROR;
		*cmp = db_getc( trc[t*2], EC_COMP, &locstat );
		if  (locstat == SHE_NOERROR)  strcat( inftxt[t], cmp );
		strcat( inftxt[t], "-" );
		*cmp = db_getc( trc[t*2+1], EC_COMP, &locstat );
		if  (locstat == SHE_NOERROR)  strcat( inftxt[t], cmp );
	} /*endfor*/

	if  (listlth & 1)  {
		*status = SHE_SPECERROR+8;
		return;
	} else if  (listlth > (2*PMC_MAXPM))  {
		*status = SHE_SPECERROR+9;
		return;
	} /*endif*/

	if  (mode == 0)  {
		dt = 0.0;
		for  (t=0; t<listlth; t++)  {
			if  (dt == 0.0)
				dt = db_getr( trc[t], ER_DELTA, NULL );
			if  (dt != db_getr(trc[t],ER_DELTA,NULL))  {
				*status = SHE_DIFFSAMP;
				return;
			} /*endif*/
			if  (t & 1)  {
				ystart[t/2] = (SAMPLE *)db_getp( trc[t], EP_DATA, NULL ) +
					dm_sgetsample( trc[t], lowdw, status );
			} else {
				xstart[t/2] = (SAMPLE *)db_getp( trc[t], EP_DATA, NULL ) +
					dm_sgetsample( trc[t], lowdw, status );
				tzoom[t/2] = db_getr(trc[t],ER_ZOOM,NULL)*zoom;
			} /*endif*/
			if  (Severe(status))  return;
			if  (t == 0)
				lth = (SAMPLE *)db_getp( trc[t], EP_DATA, NULL ) +
					dm_sgetsample( trc[t], hiwdw, status ) - xstart[0];
			if  (Severe(status))  return;
		} /*endfor*/
		ui_shift( inplev );
		pm_fixedwdw( gc, xstart, ystart, listlth/2, lth, inftxtptr,
			tzoom, begcirc, status );
		ui_unshift();
	} else {
		if  (mode == 1)  {
			ui_shift( inplev );
			cr_getloc( MM_NOMARK, &origtime, NULL, NULL, status );
			ui_unshift();
			if  (Severe(status))  {
				if  (*status == SHE_EXIT)  *status = SHE_NOERROR;
				return;
			} /*endif*/
			for  (t=0; t<listlth; t++)
				cr_trcmark( trc[t], origtime, 1.0 );
		} /*endif*/
		dm_dspcoo( &lowdw, &loy, &hiwdw, &hiy );
		hiwdw += lowdw;
		hiy += loy;
		dt = 0.0;
		for  (t=0; t<listlth; t++)  {
			if  (dt == 0.0)
				dt = db_getr( trc[t], ER_DELTA, NULL );
			if  (dt != db_getr(trc[t],ER_DELTA,NULL))  {
				*status = SHE_DIFFSAMP;
				return;
			} /*endif*/
			if  (t & 1)  {
				ystart[t/2] = (SAMPLE *)db_getp( trc[t], EP_DATA, NULL ) +
					dm_sgetsample( trc[t], lowdw, status );
			} else {
				xstart[t/2] = (SAMPLE *)db_getp( trc[t], EP_DATA, NULL ) +
					dm_sgetsample( trc[t], lowdw, status );
				tzoom[t/2] = db_getr(trc[t],ER_ZOOM,NULL)*zoom;
			} /*endif*/
			if  (Severe(status))  return;
			dm_sgetsample( trc[t], hiwdw, status );
			while  (Severe(status))  {
				if  (hiwdw <= lowdw)  return;
				*status = SHE_NOERROR;
				hiwdw -= dt;
				dm_sgetsample( trc[t], hiwdw, status );
			} /*endwhile*/
		} /*endfor*/
		ui_shift( inplev );
		if  (mode == 1)  {
			pm_varwdw( gc, xstart, ystart, listlth/2, lowdw, hiwdw, origtime,
				dt, inftxtptr, tzoom, loy, hiy, begcirc, status );
		} else {
			pm_movewdw( gc, xstart, ystart, listlth/2, lowdw, hiwdw, origtime,
				dt, inftxtptr, tzoom, loy, hiy, begcirc, status );
		} /*endif*/
		ui_unshift();
	} /*endif*/

} /* end of mnx_pm */



/*----------------------------------------------------------------------------*/



void mnx_rms( PARAM *par, STATUS *status )

/* Computes RMS of samples of trace
 * 1. param: trace
 * 2. param: lo bound of time window [FIXED]
 * 3. param: hi bound of time window [FIXED]
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	TRACE    *trc;                   /* trace pointer */
	REAL     lowdw, hiwdw;           /* time wdw in sec */
	long     loidx, hiidx;           /* time wdw in samples */
	long     i;                      /* sample counter */
	SAMPLE   rms;                    /* sum of samples */
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

	dat = (SAMPLE *)db_getp( trc, EP_DATA, status );
	if  (Severe(status))  return;
	rms = 0.0;
	for  (i=loidx; i<=hiidx; i++)
		rms += dat[i]*dat[i];

	if  (mean)
		rms = sqrt(rms / (SAMPLE)(hiidx-loidx+1) );

	if  (cp_pentered(par,4,status))  {
		cp_getstr( par, 4, tc, "@@", BC_LINELTH, symbol, status );
		if  (Severe(status))  return;
		sprintf( str, "%e", rms );
		sl_setsymbol( symbol, str, status );
		if  (Severe(status))  return;
	} else {
		sprintf( str, "   rms: %e\n", rms );
		gc_write( cc, str );
	} /*endif*/

} /* end of mnx_rms */



/*----------------------------------------------------------------------------*/

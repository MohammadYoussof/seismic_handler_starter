
/* file SHMIDLEV.C
 *      ==========
 *
 * version 18, 7-Sep-2006
 *
 * supporting routines for menu handling of seismhandler
 * K. Stammler, 27-JUL-1990
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
#include "shconst.h"
#include "shvars.h"
#include "cpusrdef.h"
#include "utusrdef.h"
#include "tcusrdef.h"
#include "infoidx.h"
#include "fctxsl.h"
#include "fctxcr.h"
#include "fctxml.h"
#include "fctxdm.h"
#include "sherrors.h"


typedef struct defaults {
	int    attrib;     /* line attribute block */
	int    reduct;     /* reduction factor */
	REAL   norm;       /* normalisation */
	REAL   zoom;       /* amplitude zoom factor */
	REAL   weight;     /* sum weight factor */
} DEFAULTS;

/* variables */
DEFAULTS     defaults_mlv     /* default values for sl_inittrc */
					= { 0, 1, 1.0, 1.0, 1.0 };


/* prototypes of local routines */
void ml_scanlist( SHENTRY ientry, char lostr[], char histr[],
	char cmp_op, BOOLEAN neg, TRACE *trclist[], int *listlth,
	STATUS *status );


/*--------------------------------------------------------------------------*/



void ml_newstarttime( TRACE *src, TRACE *dst, REAL wdwstart, STATUS *status )

/* sets starttime of "dst" trace using starttime of "src" trace and
 * time window start "wdwstart"
 *
 * parameters of routine
 * TRACE      *src;     input; source trace
 * TRACE      *dst;     modify; trace which starttime is to be set
 * REAL       wdwstart; input; start time of window
 * STATUS     *status;  output; return status
 */
{
	/* local variables */
	TIME     time;     /* start time */

	/* executable code */

	db_gett( src, ET_START, &time, status );
	if  (*status == SHE_NOERROR)  {
		wdwstart -= db_getr( src, ER_TORIG, NULL );
		if  (wdwstart > 0.0)
			tc_aadd( &time, wdwstart, &time );
	} else {
		*status = SHE_NOERROR;
		tc_t2a( SHC_TIMEDEFAULT, &time, status );
	} /*endif*/
	db_sett( dst, ET_START, &time, status );

} /* end of ml_newstarttime */



/*------------------------------------------------------------------------*/



void ml_trclist( char *liststr, TRACE *trclist[],
	int *listlth, STATUS *status )

/* creates list of trace pointers from character string. Maximum
 * length of trace list is SHC_ILISTLTH
 *
 * parameters of routine
 * char      *liststr;      input; list string (position numbers)
 * void      *trclist[];    output; list of trace pointers
 * int       *listlth;      output; length of trace list
 * STATUS    *status;       output; return status
 */
{
	/* local variables */
	int      map;                        /* display list number */
	int      poslist[SHC_ILISTLTH];      /* position list */
	TRACE    *trc;                       /* current trace pointer */
	int      cnt;                        /* position list counter */
	int      prevpos;                    /* previous counter value */
	char     cmp_op;                     /* compare operation */
	char     infoname[BC_SHORTSTRLTH+1]; /* name of info entry */
	char     lostr[BC_SHORTSTRLTH+1];    /* lo bound string string */
	char     histr[BC_SHORTSTRLTH+1];    /* hi bound string string */
	BOOLEAN  neg;                        /* condition negated */
	SHENTRY  ientry;                     /* entry code */
	int      prevtrc, nexttrc;           /* previous & next trace */
	char     *cptr;                      /* search pointer */

	/* executable code */

	if  (*liststr == 'W')  {    /* not default window */
		cnt = 0;
		liststr++;
		while  (isdigit(*liststr) && (cnt <= BC_SHORTSTRLTH))
			lostr[cnt++] = *liststr++;
		lostr[cnt] = '\0';
		if  (*liststr != ':')  {
			*status = SHE_SPECERROR+6;
			return;
		} /*endif*/
		liststr++;
		if  (sscanf( lostr, "%d", &map ) != 1)  {
			*status = SHE_SPECERROR+6;
			return;
		} /*endif*/
		prevtrc = EP_DSPP;
		nexttrc = EP_DSPN;
	} else if  (strcmp(liststr,"H:ALL") == 0)  {
		*listlth = 0;
		for  (cnt=0; cnt<db_lstlth(); cnt++)  {
			if  (cnt == 0)  {
				trc = db_getp( NULL, EP_NEXT, status );
			} else {
				trc = db_getp( trc, EP_NEXT, status );
			} /*endif*/
			if  (Severe(status))  return;
			if  (trc == NULL)  return;
			if  (db_hidden(trc))  {
				if  (*listlth == SHC_ILISTLTH)  {
					*status = SHE_TLOVFL;
					return;
				} /*endif*/
				trclist[(*listlth)++] = trc;
			} /*endif*/
		} /*endfor*/
		return;
#  ifdef XXX
	} else if  (*liststr == 'H')  {
		if  (liststr[1] == ':')  {
			map = SHC_HIDDENLIST;
			liststr += 2;
			prevtrc = EP_PREV;
			nexttrc = EP_NEXT;
		} else {
			prevtrc = EP_DSPP;
			nexttrc = EP_DSPN;
			map = gc;
		} /*endif*/
#  endif  /* XXX */
	} else {
		map = gc;
		prevtrc = EP_DSPP;
		nexttrc = EP_DSPN;
	} /*endif*/

	if  (isdigit(*liststr))  {

		ut_ilist( liststr, SHC_ILISTLTH, poslist, listlth, status );
		if  (*status != SHE_NOERROR)  return;

		prevpos = 0;
		trc = NULL;
		for  (cnt=0;cnt<*listlth;cnt++)  {
			if  (prevpos <= poslist[cnt])  {
				for  (;prevpos<poslist[cnt];prevpos++)
					if  (trc == NULL)  {
						trc = db_dspfirst( map, status );
					} else {
						trc = db_getp( trc, nexttrc, status );
					} /*endif*/
					if  (*status != SHE_NOERROR)  return;
			} else {
				for  (;prevpos>poslist[cnt];prevpos--)
					if  (trc == NULL)  {
						trc = db_dspfirst( map, status );
					} else {
						trc = db_getp( trc, prevtrc, status );
					} /*endif*/
					if  (*status != SHE_NOERROR)  return;
			} /*endif*/
			trclist[cnt] = trc;
		} /*endfor*/

	} else if  (strcmp(liststr,"ALL") == 0)  {

		*listlth = db_dsplth(map);
		if  (*listlth > SHC_ILISTLTH)  {
			*status = SHE_TLOVFL;
			return;
		} /*endif*/
		for  (cnt=0;cnt< *listlth;cnt++)  {
			if  (cnt == 0)  {
				trc = db_dspfirst( map, NULL );
			} else {
				trc = db_getp( trc, nexttrc, NULL );
			} /*endif*/
			trclist[cnt] = trc;
		} /*endfor*/

	} else if  (strcmp(liststr,"ALL_DH") == 0)  {

		*listlth = db_lstlth();
		if  (*listlth > SHC_ILISTLTH)  {
			*status = SHE_TLOVFL;
			return;
		} /*endif*/
		trc = NULL;
		for  (cnt=0;cnt< *listlth;cnt++)  {
			trc = db_getp( trc, EP_NEXT, NULL );
			trclist[cnt] = trc;
		} /*endfor*/

	} else if  (strcmp(liststr,"ALL_H") == 0)  {

		*listlth = 0;
		trc = NULL;
		for  (cnt=0;cnt<db_lstlth();cnt++)  {
			trc = db_getp( trc, EP_NEXT, NULL );
			if  (db_hidden(trc))  {
				if  (*listlth >= SHC_ILISTLTH)  {
					*status = SHE_TLOVFL;
					return;
				} /*endif*/
				trclist[(*listlth)++] = trc;
			} /*endif*/
		} /*endfor*/

	} else if  (strncmp(liststr,"A:",2) == 0)  {

		*listlth = 0;
		liststr += 2;
		while  (sscanf( liststr, "%lx", trclist+(*listlth) ) == 1)  {
			if  (++(*listlth) >= SHC_ILISTLTH)  {
				*status = SHE_TLOVFL;
				return;
			} /*endif*/
			cptr = strchr( liststr, ',' );
			if  (cptr == NULL)  break;
			if  (strncmp(cptr,",A:",3) != 0)  break;
			liststr = cptr + 3;
		} /*endwhile*/

		if  (*listlth == 0)  *status = SHE_ILPAR;

	} else if  (strcmp(liststr,"_CREATED") == 0)  {

		db_createdlist( trclist, listlth );

	} else if  (strcmp(liststr,"*") == 0)  {

		*status = SHE_CRSRSEL;
		return;

	} else {

		sl_parselist( liststr, BC_SHORTSTRLTH, infoname,
			lostr, histr, &cmp_op, &neg, status );
		if  (*status != SHE_NOERROR)  return;

		db_ident( infoname, &ientry, status );
		if  (*status != SHE_NOERROR)  return;
		ml_scanlist( ientry, lostr, histr, cmp_op, neg, trclist,
			listlth, status );

	} /*endif*/

} /* end of ml_trclist */



/*------------------------------------------------------------------------*/



TRACE *ml_trc( int map, int pos, STATUS *status )

/* returns trace pointer of "pos"-th trace in display
 *
 * parameters of routine
 * int      map;           input; display list number
 * int      pos;           input; position of trace
 * STATUS   *status;       output; return status
 */
{
	/* local variables */
	void      *trc;                     /* current trace pointer */
	int      nexttrc;                /* next trace */

	/* executable code */

	nexttrc = EP_DSPN; /* (map == SHC_HIDDENLIST) ? EP_NEXT : EP_DSPN; */

	trc = db_dspfirst( map, status );
	if  (*status != SHE_NOERROR)  return NULL;
	pos--;
	for  (;pos>=1;pos--)  {
		trc = db_getp( trc, nexttrc, status );
		if  (*status != SHE_NOERROR)  return NULL;
	} /*endfor*/

	return trc;

} /* end of ml_trc */



/*------------------------------------------------------------------------*/



void ml_gettrcpar( PARAM *par, int pos, int wdw, char *prompt,
				  void *trclist[], int *listlth, STATUS *status )

/* extracts trace list from parameter number "pos".  Calls cursor
 * selection routine if no string is available or if the string
 * cannot be converted to a trace list.
 *
 * parameters of routine
 * PARAM      *par;            input; menu parameter 
 * int        pos;             input; parameter number
 * int        wdw;             input; window number for prompting
 * char       *prompt;         input; prompt string
 * void       *trclist[];      output; trace list
 * int        *listlth;        output; length of list
 * STATUS     *status;         output; return status
 */
{
	/* local variables */
	char      str[BC_LINELTH+1];      /* parameter string */

	/* executable code */

	cp_getstr( par, pos, wdw, prompt, BC_LINELTH, str, status );
	if  (*status == SHE_NOERROR)  {
		ml_trclist( str, trclist, listlth, status );
		if  (*status == SHE_NOERROR)  return;
	} /*endif*/

	if  (*status == SHE_CRSRSEL)  {
		*status = SHE_NOERROR;
		ml_crsrlst( trclist, listlth, status );
	} /*endif*/

} /* end of ml_gettrcpar */



/*------------------------------------------------------------------------*/



TRACE *ml_get1trcpar( PARAM *par, int pos, int wdw, char *prompt,
	STATUS *status )

/* returns trace pointer obtained from "pos"-th parameter of "par".
 * If the parameter is not available the cursor selection routine
 * is called.
 *
 * parameters of routine
 * PARAM      *par;      input; command line parameters
 * int        pos;       input; number of parameter
 * int        wdw;       input; window for prompting
 * char       *prompt;   input; prompt string
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	int      trcpos;             /* position number of trace */
	char     str[BC_LINELTH+1];  /* scratch */
	TRACE    *tptr;              /* trace pointer */

	/* executable code */

	cp_getint( par, pos, wdw, prompt, &trcpos, status );
	if  (*status != SHE_NOERROR)  { /* select by graphic cursor */
		*status = SHE_NOERROR;
		cp_getstr( par, pos, wdw, prompt, BC_LINELTH, str, status );
		if  (Severe(status) || strncmp(str,"A:",2) != 0)  {
			*status = SHE_NOERROR;
			return cr_gettrc(status);
		} /*endif*/
		if  (sscanf(str+2,"%lp",&tptr) == 1)  {
			return tptr;
		} else {
			return cr_gettrc(status);
		} /*endif*/
	} /*endif*/
	return ml_trc( gc, trcpos, status );

} /* end of ml_get1trcpar */



/*------------------------------------------------------------------------*/



void ml_windowpars( PARAM *par, int pos, int wdw, char loprompt[],
	char hiprompt[], REAL *lo, REAL *hi, STATUS *status )

/* returns window selected by user in command line "par" at positions
 * "pos" and "pos+1" or by graphic cursor
 *
 * parameters of routine
 * PARAM      *par;       input; command line
 * int        pos;        input; number of lo-wdw parameter (hi is pos+1)
 * CHMAP      wdw;        input; prompt window
 * char       loprompt[]; input; prompt string for lower bound
 * char       hiprompt[]; input; prompt string for upper bound
 * REAL       *lo, *hi;   output; selected window
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];   /* scratch */

	/* executable code */

	cp_getstr( par, pos, wdw, loprompt, BC_LINELTH, str, status );
	if  (Severe(status))  return;
	if  (*str == SHC_CRSR)  {
		cr_window( MM_TRCMARK, lo, hi, status );
	} else {
		if  (sscanf(str,"%f",lo) != 1)  {
			*status = SHE_REALCNV;
			return;
		} /*endif*/
		cp_getfloat( par, pos+1, wdw, hiprompt, hi, status );
		if  (Severe(status))  return;
	} /*endif*/

} /* end of ml_windowpars */



/*------------------------------------------------------------------------*/



void ml_crsrlst_time( int mode, TRACE *trc[], REAL time[],
	int *listlth, STATUS *status )

/* let user select a list of traces by graphic cursor.  The selected
 * time positions of each trace is returned in "time"-array
 *
 * parameters of routine
 * int        mode;      input; selection mode (mark time points)
 * TRACE      *trc[];    output; trace list
 * REAL       time[];    output; selected time positions
 * int        *listlth;  output; number of traces selected
 * STATUS     *status;   output; return status
 */
{
	/* executable code */

	*listlth = 0;
	FOREVER  {
		if  (*listlth == SHC_ILISTLTH)  {*status = SHE_TLOVFL; return;}
		cr_gettrctime( mode, trc+(*listlth), time+(*listlth), status );
		if  (*status == SHE_NOTRC)  {
			printf( "%c", (char)7 );
			printf( "SH: no trace at this position\n" );
			*status = SHE_NOERROR;
			continue;
		} /*endif*/
		if  (*status == SHE_EXIT)  {
			*status = SHE_NOERROR;
			break;
		} /*endif*/
		if  (*status != SHE_NOERROR)  return;
		(*listlth)++;
	} /*endfor*/

} /* end of ml_crsrlst_time */



/*------------------------------------------------------------------------*/



void ml_crsrlst( TRACE *trc[], int *listlth, STATUS *status )

/* let user select a list of traces by graphic cursor.
 *
 * parameters of routine
 * TRACE      *trc[];    output; trace list
 * int        *listlth;  output; number of traces selected
 * STATUS     *status;   output; return status
 */
{
	/* executable code */

	*listlth = 0;
	FOREVER  {
		if  (*listlth == SHC_ILISTLTH)  {*status = SHE_TLOVFL; return;}
		trc[*listlth] = cr_gettrc( status );
		if  (*status == SHE_EXIT)  {
			*status = SHE_NOERROR;
			break;
		} /*endif*/
		if  (*status != SHE_NOERROR)  return;
		(*listlth)++;
	} /*endfor*/

} /* end of ml_crsrlst */



/*------------------------------------------------------------------------*/



void ml_cpyinfo( TRACE *src, TRACE *dst, STATUS *status )

/* copies info from source to destination trace
 * old version in _OLD.C !
 *
 * parameters of routine
 * TRACE      *src;         input; source trace pointer
 * TRACE      *dst;         input; destination trace pointer
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	SHENTRY      ientry;    /* current entry */
	long         l_info;
	int          i_info;
	BYTE         b_info;
	REAL         r_info;
	char         s_info[BC_LINELTH+1];
	TIME         t_info;
	int          locstat;    /* local status */

	/* executable code */

	db_nextentry( EL_TYPE );  /* initialise entry counter */
	FOREVER  {
		ientry = db_nextentry( E_NEXTENTRY );
		if  (ientry == E_EOLIST)  break;
		if  (!(ientry & E_BNOCOPY) && ((ientry & E_IDXMASK) < EMAX_LONG))  {
			locstat = SHE_NOERROR;
			l_info = db_getl( src, ientry, &locstat );
			if  (locstat == SHE_NOERROR)  {
				db_setl( dst, ientry, l_info, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	db_nextentry( EI_TYPE );  /* initialise entry counter */
	FOREVER  {
		ientry = db_nextentry( E_NEXTENTRY );
		if  (ientry == E_EOLIST)  break;
		if  (!(ientry & E_BNOCOPY) && ((ientry & E_IDXMASK) < EMAX_INT))  {
			locstat = SHE_NOERROR;
			i_info = db_geti( src, ientry, &locstat );
			if  (locstat == SHE_NOERROR)  {
				db_seti( dst, ientry, i_info, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	db_nextentry( EB_TYPE );  /* initialise entry counter */
	FOREVER  {
		ientry = db_nextentry( E_NEXTENTRY );
		if  (ientry == E_EOLIST)  break;
		if  (!(ientry & E_BNOCOPY) && ((ientry & E_IDXMASK) < EMAX_BYTE))  {
			locstat = SHE_NOERROR;
			b_info = db_getb( src, ientry, &locstat );
			if  (locstat == SHE_NOERROR)  {
				db_setb( dst, ientry, b_info, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	db_nextentry( ER_TYPE );  /* initialise entry counter */
	FOREVER  {
		ientry = db_nextentry( E_NEXTENTRY );
		if  (ientry == E_EOLIST)  break;
		if  (!(ientry & E_BNOCOPY) && ((ientry & E_IDXMASK) < EMAX_REAL))  {
			locstat = SHE_NOERROR;
			r_info = db_getr( src, ientry, &locstat );
			if  (locstat == SHE_NOERROR)  {
				db_setr( dst, ientry, r_info, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	db_nextentry( ES_TYPE );  /* initialise entry counter */
	FOREVER  {
		ientry = db_nextentry( E_NEXTENTRY );
		if  (ientry == E_EOLIST)  break;
		if  (!(ientry & E_BNOCOPY) && ((ientry & E_IDXMASK) < EMAX_STR))  {
			locstat = SHE_NOERROR;
			db_gets( src, ientry, BC_LINELTH, s_info, &locstat );
			if  (locstat == SHE_NOERROR)  {
				db_sets( dst, ientry, s_info, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	db_nextentry( EC_TYPE );  /* initialise entry counter */
	FOREVER  {
		ientry = db_nextentry( E_NEXTENTRY );
		if  (ientry == E_EOLIST)  break;
		if  (!(ientry & E_BNOCOPY) && ((ientry & E_IDXMASK) < EMAX_CHAR))  {
			locstat = SHE_NOERROR;
			*s_info = db_getc( src, ientry, &locstat );
			if  (locstat == SHE_NOERROR)  {
				db_setc( dst, ientry, *s_info, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	db_nextentry( ET_TYPE );  /* initialise entry counter */
	FOREVER  {
		ientry = db_nextentry( E_NEXTENTRY );
		if  (ientry == E_EOLIST)  break;
		if  (!(ientry & E_BNOCOPY) && ((ientry & E_IDXMASK) < EMAX_TIME))  {
			locstat = SHE_NOERROR;
			db_gett( src, ientry, &t_info, &locstat );
			if  (locstat == SHE_NOERROR)  {
				db_sett( dst, ientry, &t_info, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	db_nextentry( EF_TYPE );  /* initialise entry counter */
	FOREVER  {
		ientry = db_nextentry( E_NEXTENTRY );
		if  (ientry == E_EOLIST)  break;
		if  (!(ientry & E_BNOCOPY) && ((ientry & E_IDXMASK) < EMAX_FLAG))  {
			locstat = SHE_NOERROR;
			i_info = db_getf( src, ientry, &locstat );
			if  (locstat == SHE_NOERROR)  {
				db_setf( dst, ientry, i_info, status );
				if  (*status != SHE_NOERROR)  return;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

} /* end of ml_cpyinfo */



/*------------------------------------------------------------------------*/



void ml_inittrc( TRACE *trc, SAMPLE *datptr, long lth, float dt )

/* sets the required info entries of "trc"
 *
 * parameters of routine
 * TRACE      *trc;         input; trace pointer
 * SAMPLE     *datptr;      input; data array
 * long       lth;          input; length of data array
 * float      dt;           input; sample distance
 */
{
	/* local variables */
	float      min, max;      /* minimum & maximum of trace */

	/* executable code */

	sl_findmax( datptr, lth, &min, &max );
	db_setl( trc, EL_ALLOC, lth, NULL );
	db_setl( trc, EL_LENGTH, lth, NULL );
	db_setr( trc, ER_NORM, defaults_mlv.norm, NULL );
	db_setr( trc, ER_ZOOM, defaults_mlv.zoom, NULL );
	db_setr( trc, ER_MINVAL, min, NULL );
	db_setr( trc, ER_MAXVAL, max, NULL );
	db_setp( trc, EP_DATA, datptr, NULL );
	db_seti( trc, EI_ATTRIB, defaults_mlv.attrib, NULL );
	db_seti( trc, EI_REDUCT, defaults_mlv.reduct, NULL );
	db_setr( trc, ER_DELTA, dt, NULL );
	db_setr( trc, ER_WEIGHT, defaults_mlv.weight, NULL );

	db_enlist( trc, 0x7fff );

} /* end of ml_inittrc */



/*------------------------------------------------------------------------*/



void ml_defaults( SHENTRY ientry, void *infptr, STATUS *status )

/* sets new default values
 *
 * parameters of routine
 * SHENTRY    ientry;     input; info entry number
 * void       *infptr;    input; pointer to new info value
 * STATUS     *status;    output; return status
 */
{
	/* executable code */

	switch  (ientry)  {
	case  EI_ATTRIB:
		defaults_mlv.attrib = *(int *)infptr;
		break;
	case  EI_REDUCT:
		defaults_mlv.reduct = *(int *)infptr;
		break;
	case  ER_NORM:
		defaults_mlv.norm = *(REAL *)infptr;
		break;
	case  ER_ZOOM:
		defaults_mlv.zoom = *(REAL *)infptr;
		break;
	case  ER_WEIGHT:
		defaults_mlv.weight = *(REAL *)infptr;
		break;
	default:
		*status = SHE_ILDEFLT;
		return;
	} /*endswitch*/

} /* end of ml_defaults */



/*--------------------------------------------------------------------------*/



void ml_scanlist( SHENTRY ientry, char lostr[], char histr[], char cmp_op,
	BOOLEAN neg, TRACE *trclist[], int *listlth, STATUS *status )

/* creates list of traces matching the condition
 *    lo-bound <= entry-value <= hi-bound
 *
 * parameters of routine
 * SHENTRY    ientry;      input; entry code
 * char       lostr[];     input; string of lower bound
 * char       histr[];     input; string of upper bound
 * char       cmp_op;      input; compare operation ">", "<", "=", "B"
 * BOOLEAN    neg;         input; negate condition
 * TRACE      *trclist[];  output; trace list
 * int        *listlth;    output; length of list
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	long     l_lo, l_hi, l_curr;   /* long bounds */
	int      i_lo, i_hi, i_curr;   /* int bounds */
	REAL     r_lo, r_hi, r_curr;   /* real bounds */
	TIME     t_lo, t_hi, t_curr;   /* time bounds */
	void     *p_lo, *p_hi, *p_curr;/* pointer bounds */
	char     s_curr[BC_LINELTH+1]; /* current string value */
	BOOLEAN  f_cmp, f_curr;        /* flags */
	TRACE    *trc;                 /* trace pointer */
	int      cnt;                  /* trace counter */
	BOOLEAN  takeit;               /* take trace */
	int      locstat;              /* local status */

	/* executable code */

	switch  (ientry & E_TYPMASK)  {

	case EL_TYPE:
		if  (*lostr != '\0')
			if  (sscanf(lostr,"%ld",&l_lo) != 1)  {
				*status = SHE_INTCNV;
				return;
			} /*endif*/
		if  (*histr != '\0')
			if  (sscanf(histr,"%ld",&l_hi) != 1)  {
				*status = SHE_INTCNV;
				return;
			} /*endif*/
		*listlth = 0;
		trc = NULL;
		for  (cnt=0;cnt<db_lstlth();cnt++)  {
			trc = db_getp( trc, EP_NEXT, NULL );
			locstat = SHE_NOERROR;
			l_curr = db_getl( trc, ientry, &locstat );
			if  (locstat == SHE_NOERROR  ||  cmp_op == ' ')  {
				switch  (cmp_op)  {
				case 'B':  takeit = ((l_lo <= l_curr) && (l_curr <= l_hi)); break;
				case '>':  takeit = (l_lo <= l_curr); break;
				case '<':  takeit = (l_curr <= l_hi); break;
				case '=':  takeit = (l_curr == l_lo); break;
				case ' ':  takeit = (locstat != SHE_NOERROR); break;
				default: *status = SHE_BUG; return;
				} /*endswitch*/
				if  (neg)  takeit = !takeit;
				if  (takeit)  trclist[(*listlth)++] = trc;
			} /*endif*/
		} /*endfor*/
		break;

	case EI_TYPE:
		if  (*lostr != '\0')
			if  (sscanf(lostr,"%d",&i_lo) != 1)  {
				*status = SHE_INTCNV;
				return;
			} /*endif*/
		if  (*histr != '\0')
			if  (sscanf(histr,"%d",&i_hi) != 1)  {
				*status = SHE_INTCNV;
				return;
			} /*endif*/
		*listlth = 0;
		trc = NULL;
		for  (cnt=0;cnt<db_lstlth();cnt++)  {
			trc = db_getp( trc, EP_NEXT, NULL );
			locstat = SHE_NOERROR;
			i_curr = db_geti( trc, ientry, &locstat );
			if  (locstat == SHE_NOERROR  ||  cmp_op == ' ')  {
				switch  (cmp_op)  {
				case 'B':  takeit = ((i_lo <= i_curr) && (i_curr <= i_hi)); break;
				case '>':  takeit = (i_lo <= i_curr); break;
				case '<':  takeit = (i_curr <= i_hi); break;
				case '=':  takeit = (i_curr == i_lo); break;
				case ' ':  takeit = (locstat != SHE_NOERROR); break;
				default: *status = SHE_BUG; return;
				} /*endswitch*/
				if  (neg)  takeit = !takeit;
				if  (takeit)  trclist[(*listlth)++] = trc;
			} /*endif*/
		} /*endfor*/
		break;

	case EB_TYPE:
		if  (*lostr != '\0')
			if  (sscanf(lostr,"%d",&i_lo) != 1)  {
				*status = SHE_INTCNV;
				return;
			} /*endif*/
		if  (*histr != '\0')
			if  (sscanf(histr,"%d",&i_hi) != 1)  {
				*status = SHE_INTCNV;
				return;
			} /*endif*/
		*listlth = 0;
		trc = NULL;
		for  (cnt=0;cnt<db_lstlth();cnt++)  {
			trc = db_getp( trc, EP_NEXT, NULL );
			locstat = SHE_NOERROR;
			i_curr = (int)db_getb( trc, ientry, &locstat );
			if  (locstat == SHE_NOERROR  ||  cmp_op == ' ')  {
				switch  (cmp_op)  {
				case 'B':  takeit = ((i_lo <= i_curr) && (i_curr <= i_hi)); break;
				case '>':  takeit = (i_lo <= i_curr); break;
				case '<':  takeit = (i_curr <= i_hi); break;
				case '=':  takeit = (i_curr == i_lo); break;
				case ' ':  takeit = (locstat != SHE_NOERROR); break;
				default: *status = SHE_BUG; return;
				} /*endswitch*/
				if  (neg)  takeit = !takeit;
				if  (takeit)  trclist[(*listlth)++] = trc;
			} /*endif*/
		} /*endfor*/
		break;

	case ER_TYPE:
		if  (*lostr != '\0')
			if  (sscanf(lostr,SHC_REALFMT,&r_lo) != 1)  {
				*status = SHE_INTCNV;
				return;
			} /*endif*/
		if  (*histr != '\0')
			if  (sscanf(histr,SHC_REALFMT,&r_hi) != 1)  {
				*status = SHE_INTCNV;
				return;
			} /*endif*/
		*listlth = 0;
		trc = NULL;
		for  (cnt=0;cnt<db_lstlth();cnt++)  {
			trc = db_getp( trc, EP_NEXT, NULL );
			locstat = SHE_NOERROR;
			r_curr = db_getr( trc, ientry, &locstat );
			if  (locstat == SHE_NOERROR  ||  cmp_op == ' ')  {
				switch  (cmp_op)  {
				case 'B':  takeit = ((r_lo <= r_curr) && (r_curr <= r_hi)); break;
				case '>':  takeit = (r_lo <= r_curr); break;
				case '<':  takeit = (r_curr <= r_hi); break;
				case '=':  takeit = (r_curr == r_lo); break;
				case ' ':  takeit = (locstat != SHE_NOERROR); break;
				default: *status = SHE_BUG; return;
				} /*endswitch*/
				if  (neg)  takeit = !takeit;
				if  (takeit)  trclist[(*listlth)++] = trc;
			} /*endif*/
		} /*endfor*/
		break;

	case ES_TYPE:
		*listlth = 0;
		trc = NULL;
		for  (cnt=0;cnt<db_lstlth();cnt++)  {
			trc = db_getp( trc, EP_NEXT, NULL );
			locstat = SHE_NOERROR;
			db_gets( trc, ientry, BC_LINELTH, s_curr, &locstat );
			if  (locstat == SHE_NOERROR  ||  cmp_op == ' ')  {
				switch  (cmp_op)  {
				case 'B':  takeit = ((strcmp(lostr,s_curr) <= 0) &&
							  (strcmp(s_curr,histr) <= 0)); break;
				case '>':  takeit = (strcmp(lostr,s_curr) <= 0); break;
				case '<':  takeit = (strcmp(s_curr,histr) <= 0); break;
				case '=':  takeit = (strcmp(s_curr,lostr) == 0); break;
				case ' ':  takeit = (locstat != SHE_NOERROR); break;
				default: *status = SHE_BUG; return;
				} /*endswitch*/
				if  (neg)  takeit = !takeit;
				if  (takeit)  trclist[(*listlth)++] = trc;
			} /*endif*/
		} /*endfor*/
		break;

	case EC_TYPE:
		*listlth = 0;
		trc = NULL;
		for  (cnt=0;cnt<db_lstlth();cnt++)  {
			trc = db_getp( trc, EP_NEXT, NULL );
			locstat = SHE_NOERROR;
			*s_curr = db_getc( trc, ientry, &locstat );
			if  (locstat == SHE_NOERROR  ||  cmp_op == ' ')  {
				switch  (cmp_op)  {
				case 'B':  takeit = ((*lostr <= *s_curr) &&
							  (*s_curr <= *histr)); break;
				case '>':  takeit = (*lostr <= *s_curr); break;
				case '<':  takeit = (*s_curr <= *histr); break;
				case '=':  takeit = (*s_curr == *lostr); break;
				case ' ':  takeit = (locstat != SHE_NOERROR); break;
				default: *status = SHE_BUG; return;
				} /*endswitch*/
				if  (neg)  takeit = !takeit;
				if  (takeit)  trclist[(*listlth)++] = trc;
			} /*endif*/
		} /*endfor*/
		break;

	case ET_TYPE:
		if  (*lostr != '\0')
			tc_t2a( lostr, &t_lo, status );
		if  (*status != SHE_NOERROR)  return;
		if  (*histr != '\0')
			tc_t2a( histr, &t_hi, status );
		if  (*status != SHE_NOERROR)  return;
		*listlth = 0;
		trc = NULL;
		for  (cnt=0;cnt<db_lstlth();cnt++)  {
			trc = db_getp( trc, EP_NEXT, NULL );
			locstat = SHE_NOERROR;
			db_gett( trc, ientry, &t_curr, &locstat );
			if  (locstat == SHE_NOERROR  ||  cmp_op == ' ')  {
				switch  (cmp_op)  {
				case 'B':
					r_lo = tc_adiff( &t_curr, &t_lo );
					r_hi = tc_adiff( &t_hi, &t_curr );
					takeit = ((r_lo >= 0.0) && (r_hi >= 0.0));
					break;
				case '>':  takeit = (tc_adiff(&t_curr,&t_lo) >= 0.0); break;
				case '<':  takeit = (tc_adiff(&t_hi,&t_curr) >= 0.0); break;
				case '=':  takeit = (tc_adiff(&t_lo,&t_curr) == 0.0); break;
				case ' ':  takeit = (locstat != SHE_NOERROR); break;
				default: *status = SHE_BUG; return;
				} /*endswitch*/
				if  (neg)  takeit = !takeit;
				if  (takeit)  trclist[(*listlth)++] = trc;
			} /*endif*/
		} /*endfor*/
		break;

	case EP_TYPE:
		if  (*lostr != '\0')
			if  (sscanf(lostr,"%p",&p_lo) != 1)  {
				*status = SHE_INTCNV;
				return;
			} /*endif*/
		if  (*histr != '\0')
			if  (sscanf(histr,"%p",&p_hi) != 1)  {
				*status = SHE_INTCNV;
				return;
			} /*endif*/
		*listlth = 0;
		trc = NULL;
		for  (cnt=0;cnt<db_lstlth();cnt++)  {
			trc = db_getp( trc, EP_NEXT, NULL );
			locstat = SHE_NOERROR;
			p_curr = db_getp( trc, ientry, &locstat );
			if  (locstat == SHE_NOERROR  ||  cmp_op == ' ')  {
				switch  (cmp_op)  {
				case 'B':  takeit = ((p_lo <= p_curr) && (p_curr <= p_hi)); break;
				case '>':  takeit = (p_lo <= p_curr); break;
				case '<':  takeit = (p_curr <= p_hi); break;
				case '=':  takeit = (p_curr == p_lo); break;
				case ' ':  takeit = (locstat != SHE_NOERROR); break;
				default: *status = SHE_BUG; return;
				} /*endswitch*/
				if  (neg)  takeit = !takeit;
				if  (takeit)  trclist[(*listlth)++] = trc;
			} /*endif*/
		} /*endfor*/
		break;

	case EF_TYPE:
		f_cmp = (*lostr == 'Y');
		*listlth = 0;
		trc = NULL;
		for  (cnt=0;cnt<db_lstlth();cnt++)  {
			trc = db_getp( trc, EP_NEXT, NULL );
			locstat = SHE_NOERROR;
			f_curr = (db_getf( trc, ientry, &locstat ) != FALSE);
			if  (locstat == SHE_NOERROR  ||  cmp_op == ' ')  {
				switch  (cmp_op)  {
				case 'B':
				case '>':
				case '<':  *status = SHE_ILPAR; return;
				case '=':  takeit = (f_curr == f_cmp); break;
				case ' ':  takeit = (locstat != SHE_NOERROR); break;
				default: *status = SHE_BUG; return;
				} /*endswitch*/
				if  (neg)  takeit = !takeit;
				if  (takeit)  trclist[(*listlth)++] = trc;
			} /*endif*/
		} /*endfor*/
		break;

	default:
		*status = SHE_UKTYPE;
		return;
	} /*endswitch*/

} /* ml_scanlist*/



/*--------------------------------------------------------------------------*/



int ml_listwdw( char liststr[], STATUS *status )

/* returns window of list string
 *
 * parameters of routine
 * char       liststr[];    input; list string
 * STATUS     *status;      output; return status
 *                          returns window number
 */
{
	/* local variables */
	int      wdw;                      /* window number */
	int      cnt;                      /* counter */
	char     numstr[BC_SHORTSTRLTH+1]; /* number string */

	/* executable code */

	wdw = gc;
	if  (*liststr == 'W')  {
		cnt = 0;
		liststr++;
		while  (isdigit(*liststr) && (cnt <= BC_SHORTSTRLTH))
			numstr[cnt++] = *liststr++;
		numstr[cnt] = '\0';
		if  (*liststr != ':')  {
			*status = SHE_SPECERROR+6;
			return gc;
		} /*endif*/
		if  (sscanf( numstr, "%d", &wdw ) != 1)  {
			*status = SHE_SPECERROR+6;
			return gc;
		} /*endif*/
	} /*endif*/

	return wdw;

} /* end of ml_listwdw */



/*--------------------------------------------------------------------------*/



void ml_resample( TRACE *trc, REAL new_dt, STATUS *status )

/* peforms resampling of a trace
 *
 * parameters of routine
 * TRACE      *trc;      input; trace pointer
 * REAL       new_dt;    input; new delta t
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	REAL     old_dt;      /* current delta t */
	SAMPLE   *old_ptr;    /* current data pointer */
	SAMPLE   *new_ptr;    /* new data pointer */
	long     old_lth;     /* current array length */
	long     new_lth;     /* new array length */

	/* executable code */

	old_dt = db_getr( trc, ER_DELTA, NULL );
	old_ptr = (SAMPLE *)db_getp( trc, EP_DATA, NULL );
	old_lth = db_getl( trc, EL_LENGTH, NULL );
	new_lth = Nlong( (REAL)old_lth * old_dt / new_dt );
	new_ptr = (SAMPLE *)sy_allocmem( new_lth, (int)sizeof(SAMPLE),
		status );
	if  (Severe(status))  return;
	sl_resample( old_dt, old_lth, old_ptr, new_dt, new_lth, new_ptr );
	db_setl( trc, EL_LENGTH, new_lth, NULL );
	db_setl( trc, EL_ALLOC, new_lth, NULL );
	db_setp( trc, EP_DATA, new_ptr, NULL );
	db_setr( trc, ER_DELTA, new_dt, NULL );
	db_setf( trc, EF_MODIF, TRUE, NULL );
	sy_deallocmem( old_ptr );

} /* end of ml_resample */



/*--------------------------------------------------------------------------*/



void ml_decimate( TRACE *trc, int decimation, BOOLEAN nomean, STATUS *status )

/* peforms decimation of a trace using mean values
 *
 * parameters of routine
 * TRACE      *trc;        input; trace pointer
 * int        decimation;  input; decimation factor
 * BOOLEAN    nomean;      input; take no mean value, plain resampling
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	REAL     old_dt;      /* current delta t */
	SAMPLE   *old_ptr;    /* current data pointer */
	SAMPLE   *new_ptr;    /* new data pointer */
	long     old_lth;     /* current array length */
	long     new_lth;     /* new array length */
	REAL     new_dt;      /* new sample rate */
	TIME     start;       /* start time */

	/* executable code */

	old_dt = db_getr( trc, ER_DELTA, NULL );
	old_ptr = (SAMPLE *)db_getp( trc, EP_DATA, NULL );
	old_lth = db_getl( trc, EL_LENGTH, NULL );
	new_dt = old_dt * (REAL)decimation;
	new_lth = Nlong( (REAL)old_lth * old_dt / new_dt );
	new_ptr = (SAMPLE *)sy_allocmem( new_lth, (int)sizeof(SAMPLE),
		status );
	if  (Severe(status))  return;
	sl_decimate( old_lth, old_ptr, decimation, nomean, new_lth, new_ptr );
	db_setl( trc, EL_LENGTH, new_lth, NULL );
	db_setl( trc, EL_ALLOC, new_lth, NULL );
	db_setp( trc, EP_DATA, new_ptr, NULL );
	db_setr( trc, ER_DELTA, new_dt, NULL );
	db_setf( trc, EF_MODIF, TRUE, NULL );
	sy_deallocmem( old_ptr );

	/* change start time */
	db_gett( trc, ET_START, &start, status );
	if  (Severe(status))  {
		*status = BC_NOERROR;  /* doesn't matter */
		return;                /* start time doesn't exist */
	} /*endif*/
	tc_aadd( &start, new_dt/2.0, &start );
	db_sett( trc, ET_START, &start, status );

} /* end of ml_decimate */



/*--------------------------------------------------------------------------*/



void ml_cut( TRACE *trc, REAL lowdw, REAL hiwdw, SHENTRY entry,
	REAL vel, STATUS *status )

/* cuts a trace at (relative) times "lowdw" and "hiwdw" if entry=0.
 * if entry is of time-type, the time window bounds refer to the
 * time info entry of the trace, if entry is of real type it is
 * assumed to describe a distance and "vel" is used to compute
 * a reduced velocity time window.
 *
 * parameters of routine
 * TRACE      *trc;          input; trace pointer
 * REAL       lowdw, hiwdw;  input; time window
 * SHENTRY    entry;         input; info entry or 0
 * REAL       vel;           input; velocity for reduced velocity cut
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	SAMPLE   *old_ptr;    /* current data pointer */
	SAMPLE   *new_ptr;    /* new data pointer */
	long     new_lth;     /* new array length */
	long     new_start;   /* start sample */
	TIME     starttime;   /* start time */
	SAMPLE   *cs, *cd;    /* sample pointers */
	TIME     tstart, tpos;           /* times on trace */
	REAL     relpos;                 /* relative position time */
	REAL     min, max;               /* minimum & maximum */
	STATUS   locstat=SHE_NOERROR;   /* local status */

	/* executable code */

	if  ((entry & E_TYPMASK) == ET_TYPE)  {
		db_gett( trc, ET_START, &tstart, status );
		if  (Severe(status))  return;
		db_gett( trc, entry, &tpos, status );
		if  (Severe(status))  return;
		relpos = tc_adiff( &tpos, &tstart );
		lowdw += relpos;
		hiwdw += relpos;
	} else if  ((entry & E_TYPMASK) == ER_TYPE)  {
		if  (vel == 0.0)  return;
		relpos = db_getr( trc, entry, status );
		if  (Severe(status))  return;
		relpos = Abs(relpos) / vel;
		lowdw += relpos;
		hiwdw += relpos;
	} /*endif*/

	old_ptr = (SAMPLE *)db_getp( trc, EP_DATA, NULL );
	new_start = dm_getsample( trc, lowdw, TRUE );
	new_lth = dm_getsample( trc, hiwdw, TRUE );
	new_lth = new_lth - new_start + 1;
	new_ptr = (SAMPLE *)sy_allocmem( new_lth, (int)sizeof(SAMPLE),
		status );
	if  (Severe(status))  return;

	/* copy data */
	for  (cs=old_ptr+new_start,cd=new_ptr; cd<new_ptr+new_lth;
		*cd++ = *cs++) {}

	lowdw = (REAL)new_start * db_getr(trc,ER_DELTA,NULL);
	db_gett( trc, ET_START, &starttime, &locstat );
	if  (locstat == SHE_NOERROR)  {
		tc_aadd( &starttime, lowdw, &starttime );
		db_sett( trc, ET_START, &starttime, status );
		if  (Severe(status))  return;
	} /*endif*/
	sl_findmax( new_ptr, new_lth, &min, &max );
	db_setl( trc, EL_LENGTH, new_lth, NULL );
	db_setl( trc, EL_ALLOC, new_lth, NULL );
	db_setp( trc, EP_DATA, new_ptr, NULL );
	db_setr( trc, ER_TORIG, db_getr(trc,ER_TORIG,NULL)+lowdw, NULL );
	db_setr( trc, ER_MINVAL, min, NULL );
	db_setr( trc, ER_MAXVAL, max, NULL );
	db_setf( trc, EF_MODIF, TRUE, NULL );
	sy_deallocmem( old_ptr );

} /* end of ml_cut */



/*--------------------------------------------------------------------------*/



void ml_reduced_time( int listlth, TRACE *trc[], REAL vel, STATUS *status )

/* makes reduced time shifts
 *
 * parameters of routine
 * int        listlth;      input; length of trace list
 * TRACE      *trc[];       input; trace list
 * REAL       vel;          input; reduction velocity
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	SHENTRY       distent;    /* distance entry */
	REAL          distance;   /* current distance value */
	int           t;          /* trace counter */

	/* executable code */

	if  (Abs(vel) < SHC_EPSILON)  return;

	db_ident( "DISTANCE", &distent, status );
	if  (Severe(status))  return;

	for  (t=0; t<listlth; t++)  {
		distance = db_getr( trc[t], distent, status );
		distance = Abs( distance ) / vel;
		db_setr( trc[t], ER_TORIG,
			db_getr(trc[t],ER_TORIG,NULL)-distance, NULL );
		if  (Severe(status))  return;
	} /*endfor*/

} /* end of ml_reduced_time */



/*--------------------------------------------------------------------------*/



void ml_time_align( int listlth, TRACE *trc[], char *altime, STATUS *status )

/* shifts traces to fit absolute times
 *
 * parameters of routine
 * int        listlth;      input; length of trace list
 * TRACE      *trc[];       input; trace list
 * char       *altime;      input; shift reference time string
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	int      t;           /* trace counter */
	TIME     ttime;       /* trace start time */
	TIME     atime;       /* reference time */
	REAL     delay;       /* delay time */
	REAL     mindelay;    /* minimum delay */
	TSyBoolean timefixed; /* absolute time given by command */

	/* executable code */

	if  (listlth == 0)  {
		*status = SHE_EMPTYLST;
		return;
	} /*endif*/

	if  (*altime == '\0')  {  /* take start time of first trace */
		db_gett( trc[0], ET_START, &atime, status );
		timefixed = FALSE;
	} else {
		tc_t2a( altime, &atime, status );
		timefixed = TRUE;
	} /*endif*/
	if  (Severe(status))  return;

	mindelay = 0.0;
	for  (t=0; t<listlth; t++)  {
		db_gett( trc[t], ET_START, &ttime, status );
		if  (Severe(status))  return;
		delay = tc_adiff( &atime, &ttime );
		if  (delay > mindelay)  mindelay = delay;
		db_setr( trc[t], ER_TORIG, db_getr(trc[t],ER_TORIG,NULL)-delay, NULL );
		if  (Severe(status))  return;
	} /*endfor*/

	/* set minimum t-orig to zero if time not fixed by command */
	if  (!timefixed && mindelay > 0.0)  {
		for  (t=0; t<listlth; t++)  {
			db_setr( trc[t], ER_TORIG,
				db_getr(trc[t],ER_TORIG,NULL)+mindelay, NULL );
		} /*endfor*/
	} /*endif*/

} /* end of ml_time_align */



/*--------------------------------------------------------------------------*/



void ml_sddelay( int listlth, TRACE *trc[], REAL slowness,
	char diststr[], STATUS *status )

/* adds slowness-distance delay to traces listed in trc[0..listlth-1]
 *
 * parameters of routine
 * int        listlth;     input; length of trace list
 * TRACE      *trc[];      input; trace list
 * REAL       slowness;    input; slowness
 * char       diststr[];   input; distance info entry descr. string
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	SHENTRY  distentry;     /* distance entry number */
	REAL     delay;         /* delay time in sec */
	int      t;             /* trace counter */

	/* executable code */

	db_ident( diststr, &distentry, status );
	if  (Severe(status))  return;
	if  ((distentry & E_TYPMASK) != ER_TYPE)  {
		*status = SHE_ILTYPE;
		return;
	} /*endif*/

	for  (t=0; t<listlth; t++)  {
		delay = slowness * db_getr(trc[t],distentry,status);
		if  (Severe(status))  return;
		db_setr( trc[t], ER_TORIG, db_getr(trc[t],ER_TORIG,NULL)+delay,
			status );
		if  (Severe(status))  return;
	} /*endif*/

} /* end of ml_sddelay */



/*--------------------------------------------------------------------------*/

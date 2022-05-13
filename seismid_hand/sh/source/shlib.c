
/* file SHLIB.C
 *      =======
 *
 * version 22, 13-Feb-2007
 *
 * library routines of seismhandler
 * K. Stammler, 5-MAY-1990
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
#include <math.h>
#include "basecnst.h"
#include BC_SYSBASE
#include "shconst.h"
#include "utusrdef.h"
#include "cpusrdef.h"
#include "ssusrdef.h"
#include "erusrdef.h"
#include "trusrdef.h"
#include "tcusrdef.h"
#include BC_GCUSRDEF
#include "fctxsl.h"
#include "sserrors.h"
#include "sherrors.h"
#include "globalparams.h"
#include "seed_io/seedcfg.h"
#include "sqliface.h"




/*------------------------------------------------------------------------*/



void sl_findmax( float *dat, long lth, float *min, float *max )

/* searches for maximum and minimum value in array "dat" */

/* parameters of routine */
/* float      *dat;      input; data array to be checked */
/* long        lth;      input; length of array */
/* float        *min;      output; minimum found */
/* float        *max;      output; maximum found */

{
	/* executable code */

	*min = *dat;
	*max = *dat;
	for  (;lth>1;lth--)  {
		if  (*(++dat) > *max)  *max = *dat;
		if  (*dat < *min)  *min = *dat;
	} /*endfor*/

} /* end of sl_findmax */



/*------------------------------------------------------------------------*/



void sl_findmaxpos( float *dat, long lth, float *min, float *max,
       long *minpos, long *maxpos )

/* searches for maximum and minimum value in array "dat"
 *
 * parameters of routine
 * float      *dat;     input; data array to be checked
 * long       lth;      input; length of array
 * float      *min;     output; minimum found
 * float      *max;     output; maximum found
 * long       *minpos;  output; position of minimum
 * long       *maxpos;  output; position of maximum
 */
{
	/* local variables */
	SAMPLE   *ptr;

	/* executable code */

	*minpos = *maxpos = 0;
	*min = *max = *dat;
	ptr = dat;
	for  (;lth>1;lth--)  {
		if  (*(++dat) > *max)  {
			*max = *dat;
			*maxpos = dat-ptr;
		} /*endif*/
		if  (*dat < *min)  {
			*min = *dat;
			*minpos = dat-ptr;
		} /*endif*/
	} /*endfor*/

} /* end of sl_findmaxpos */



/*------------------------------------------------------------------------*/



void sl_cnvshflags( char *flagstr, SHFLAGS *flagbits, int *status )

/* converts flag string into bitmap (SH status flags)
 *
 * parameters of routine
 * char       *flagstr;       input; flag string
 * SHFLAGS    *flagbits;      output; flag bitmap
 * int        *status;        output; return status
 */
{
	/* executable code */

	*flagbits = 0;
	while  (*flagstr != '\0')  {
		switch  (*flagstr++)  {
		case 'E':
			*flagbits |= SHF_ECHO;
			break;
		case 'T':
			*flagbits |= SHF_STEP;
			break;
		case 'P':
			*flagbits |= SHF_LOGCMD;
			break;
		case 'C':
			*flagbits |= SHF_CAPCNV;
			break;
		case 'V':
			*flagbits |= SHF_VERIFY;
			break;
		case 'X':
			*flagbits |= SHF_CMDERRSTOP;
			break;
		case 'A':
			*flagbits |= SHF_SHERRSTOP;
			break;
		case 'Q':
			*flagbits |= SHF_NOERRMSG;
			break;
		case 'I':
			*flagbits |= SHF_CHATTY;
			break;
		case 'F':
			*flagbits |= SHF_STARTUP;
			break;
		default:
			*status = SHE_UKFLAG;
			err_setcontext( " ## flagstr " ); err_setcontext( flagstr );
			return;
		} /*endswitch*/
	} /*endwhile*/

} /* end of sl_cnvshflags */



/*------------------------------------------------------------------------*/



void sl_cnvgcflags( char *flagstr, int *flagbits, int *status )

/* converts flag string into bitmap (graphic channel flags)
 *
 * parameters of routine
 * char       *flagstr;       input; flag string
 * int        *flagbits;      output; graphic channel bitmap
 * int        *status;        output; return status
 */
{
	/* executable code */

	*flagbits = 0;
	while  (*flagstr != '\0')  {
		switch  (*flagstr)  {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7':
			*flagbits &= 0xfff8;
			*flagbits |= (*flagstr - '0');
			break;
		case 'H':  *flagbits |= GCF_HARDCOPY;  break;
		case 'S':  *flagbits |= GCF_STDCH;     break;
		case 'F':  *flagbits |= GCF_FILE;      break;
		case 'V':  *flagbits |= GCF_VWS;       break;
		case 'X':  *flagbits |= GCF_XWDW;      break;
		case 'C':  *flagbits |= GCF_CALCOMP;   break;
		case 'T':  *flagbits |= GCF_TEK;       break;
		case 'P':  *flagbits |= GCF_PSF;       break;
		case 'L':  *flagbits |= GCF_HPGL;      break;
		case 'D':  *flagbits |= GCF_DJ;        break;
		case 'G':  *flagbits |= GCF_GEM;       break;
		case 'B':  *flagbits |= GCF_BGI;       break;
		case 'E':  *flagbits |= GCF_EXTERN;    break;
		default:
			*status = SHE_UKFLAG;
			err_setcontext( " ## flagstr " ); err_setcontext( flagstr );
			return;
		} /*endswitch*/
		flagstr++;
	} /*endwhile*/

} /* end of sl_cnvgcflags */



/*------------------------------------------------------------------------*/



void sl_cnvwdwflags( char *flagstr, int *flagbits, int *status )

/* converts flag string into bitmap (window attributes)
 *
 * parameters of routine
 * char       *flagstr;       input; flag string
 * int        *flagbits;      output; graphic channel bitmap
 * int        *status;        output; return status
 */
{
	/* executable code */

	*flagbits = 0;
	while  (*flagstr != '\0')  {
		switch  (*flagstr)  {
		case 'N':
			*flagbits |= GCF_WNAME;
			break;
		case 'C':
			*flagbits |= GCF_WCLOSER;
			break;
		case 'F':
			*flagbits |= GCF_WFULLER;
			break;
		case 'M':
			*flagbits |= GCF_WMOVER;
			break;
		case 'I':
			*flagbits |= GCF_WINFO;
			break;
		case 'S':
			*flagbits |= GCF_WSIZER;
			break;
		default:
			*status = SHE_UKFLAG;
			err_setcontext( " ## flagstr " ); err_setcontext( flagstr );
			return;
		} /*endswitch*/
		flagstr++;
	} /*endwhile*/

} /* end of sl_cnvwdwflags */



/*------------------------------------------------------------------------*/



void sl_setoutput( PARAM *par, int *tcl, int *tcg, int *gcl, int *gcg,
	int *ccl, int *ccg, int *mainch, int *status )

/* changes global & local output streams
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * int        *tcl;      output; local text streams
 * int        *tcg;      output; global text streams
 * int        *gcl;      output; local graphic streams
 * int        *gcg;      output; global graphic streams
 * int        *ccl;      output; local console streams
 * int        *ccg;      output; global console streams
 * int        *mainch;   output; main channel changed
 * int        *status;   output; return status
 */
{
	/* local variables */
	char     flagstr[BC_LINELTH+1];   /* flag string */
	int      flagbits;                /* flag bitmap */
	int      qglobal, qmain;          /* qualifiers */

	/* executable code */

	qglobal = cp_qexist( par, "GLOBAL" );
	qmain = cp_qexist( par, "MAIN" );
	if  (qmain)  *mainch = TRUE;

	/* text streams, absolute */
	if  (cp_qstr(par,"DC",BC_LINELTH,flagstr,status))  {
		sl_cnvgcflags( flagstr, &flagbits, status );
		if  (*status != SHE_NOERROR)  return;
		if  (qglobal || qmain)  *tcg = flagbits;
		if  (!qmain)  *tcl = flagbits;
	} /*endif*/
	if  (*status != SHE_NOERROR)  return;

	/* text streams, set channels */
	if  (cp_qstr(par,"DC+",BC_LINELTH,flagstr,status))  {
		sl_cnvgcflags( flagstr, &flagbits, status );
		if  (*status != SHE_NOERROR)  return;
		if  (qglobal)  {
			*tcg |= flagbits;
			*tcl = *tcg;
		} else if  (qmain)  {
			*tcg |= flagbits;
		} else {
			*tcl |= flagbits;
		} /*endif*/
	} /*endif*/
	if  (*status != SHE_NOERROR)  return;

	/* text streams, clear channels */
	if  (cp_qstr(par,"DC-",BC_LINELTH,flagstr,status))  {
		sl_cnvgcflags( flagstr, &flagbits, status );
		if  (*status != SHE_NOERROR)  return;
		if  (qglobal)  {
			*tcg &= ~flagbits;
			*tcl = *tcg;
		} else if  (qmain)  {
			*tcg &= ~flagbits;
		} else {
			*tcl &= ~flagbits;
		} /*endif*/
	} /*endif*/
	if  (*status != SHE_NOERROR)  return;

	/* graphic streams, absolute */
	if  (cp_qstr(par,"GC",BC_LINELTH,flagstr,status))  {
		sl_cnvgcflags( flagstr, &flagbits, status );
		if  (*status != SHE_NOERROR)  return;
		if  (qglobal || qmain)  *gcg = flagbits;
		if  (!qmain)  *gcl = flagbits;
	} /*endif*/
	if  (*status != SHE_NOERROR)  return;

	/* graphic streams, set channels */
	if  (cp_qstr(par,"GC+",BC_LINELTH,flagstr,status))  {
		sl_cnvgcflags( flagstr, &flagbits, status );
		if  (*status != SHE_NOERROR)  return;
		if  (qglobal)  {
			*gcg |= flagbits;
			*gcl = *gcg;
		} else if  (qmain)  {
			*gcg |= flagbits;
		} else {
			*gcl |= flagbits;
		} /*endif*/
	} /*endif*/
	if  (*status != SHE_NOERROR)  return;

	/* graphic streams, clear channels */
	if  (cp_qstr(par,"GC-",BC_LINELTH,flagstr,status))  {
		sl_cnvgcflags( flagstr, &flagbits, status );
		if  (*status != SHE_NOERROR)  return;
		if  (qglobal)  {
			*gcg &= ~flagbits;
			*gcl = *gcg;
		} else if  (qmain)  {
			*gcg &= ~flagbits;
		} else {
			*gcl &= ~flagbits;
		} /*endif*/
	} /*endif*/
	if  (*status != SHE_NOERROR)  return;

	/* console streams, absolute */
	if  (cp_qstr(par,"IC",BC_LINELTH,flagstr,status))  {
		sl_cnvgcflags( flagstr, &flagbits, status );
		if  (*status != SHE_NOERROR)  return;
		if  (qglobal || qmain)  *ccg = flagbits;
		if  (!qmain)  *ccl = flagbits;
	} /*endif*/
	if  (*status != SHE_NOERROR)  return;

	/* graphic streams, set channels */
	if  (cp_qstr(par,"IC+",BC_LINELTH,flagstr,status))  {
		sl_cnvgcflags( flagstr, &flagbits, status );
		if  (*status != SHE_NOERROR)  return;
		if  (qglobal)  {
			*ccg |= flagbits;
			*ccl = *ccg;
		} else if  (qmain)  {
			*ccg |= flagbits;
		} else {
			*ccl |= flagbits;
		} /*endif*/
	} /*endif*/
	if  (*status != SHE_NOERROR)  return;

	/* graphic streams, clear channels */
	if  (cp_qstr(par,"IC-",BC_LINELTH,flagstr,status))  {
		sl_cnvgcflags( flagstr, &flagbits, status );
		if  (*status != SHE_NOERROR)  return;
		if  (qglobal)  {
			*ccg &= ~flagbits;
			*ccl = *ccg;
		} else if  (qmain)  {
			*ccg &= ~flagbits;
		} else {
			*ccl &= ~flagbits;
		} /*endif*/
	} /*endif*/
	if  (*status != SHE_NOERROR)  return;

} /* end of sl_setoutput */



/*------------------------------------------------------------------------*/



void sl_updatewdwnames( int tc, int gc, int cc )

/* updates names of windows
 *
 * parameters of routine
 * int        tc;        input; text stream(s)
 * int        gc;        input; graphic stream(s)
 * int        cc;        input; console stream(s)
 */
{
	/* local variables */
	static int   old_tc, old_gc, old_cc;   /* old windows */
	char         str[BC_LINELTH+1];        /* scratch string */

	/* executable code */
/*
	tc &= SHC_WDWMASK;
	gc &= SHC_WDWMASK;
	cc &= SHC_WDWMASK;
*/
	/* reset old window names */
	if  ((old_tc != tc) && (old_tc != gc) && (old_tc != cc))  {
		sprintf( str, " Wdw %d ", old_tc & SHC_WDWMASK );
		gc_setwdwname( old_tc, str );
	} /*endif*/
	if  ((old_gc != tc) && (old_gc != gc) && (old_gc != cc))  {
		sprintf( str, " Wdw %d ", old_gc & SHC_WDWMASK );
		gc_setwdwname( old_gc, str );
	} /*endif*/
	if  ((old_cc != tc) && (old_cc != gc) && (old_cc != cc))  {
		sprintf( str, " Wdw %d ", old_cc & SHC_WDWMASK );
		gc_setwdwname( old_cc, str );
	} /*endif*/

	if  ((tc == gc) && (tc == cc))  {  /* all 3 same window */
		sprintf( str, " Wdw %d (Dialog,Graphic,Info) ",
			tc & SHC_WDWMASK );
		gc_setwdwname( tc, str );
	} else if  (tc == cc)  {
		sprintf( str, " Wdw %d (Dialog,Info) ", tc & SHC_WDWMASK );
		gc_setwdwname( tc, str );
		sprintf( str, " Wdw %d (Graphic) ", gc & SHC_WDWMASK );
		gc_setwdwname( gc, str );
	} else if  (gc == tc)  {
		sprintf( str, " Wdw %d (Dialog,Graphic) ", tc & SHC_WDWMASK );
		gc_setwdwname( tc, str );
		sprintf( str, " Wdw %d (Info) ", cc & SHC_WDWMASK );
		gc_setwdwname( cc, str );
	} else if  (cc == gc)  {
		sprintf( str, " Wdw %d (Graphic,Info) ", gc & SHC_WDWMASK );
		gc_setwdwname( gc, str );
		sprintf( str, " Wdw %d (Dialog) ", tc & SHC_WDWMASK );
		gc_setwdwname( tc, str );
	} else {
		sprintf( str, " Wdw %d (Dialog) ", tc & SHC_WDWMASK );
		gc_setwdwname( tc, str );
		sprintf( str, " Wdw %d (Graphic) ", gc & SHC_WDWMASK );
		gc_setwdwname( gc, str );
		sprintf( str, " Wdw %d (Info) ", cc & SHC_WDWMASK );
		gc_setwdwname( cc, str );
	} /*endif*/

	old_tc = tc;
	old_gc = gc;
	old_cc = cc;

} /* end of sl_updatewdwnames */



/*------------------------------------------------------------------------*/



void sl_chgflags( PARAM *par, SHFLAGS *lflags, SHFLAGS *gflags,
	int *status )

/* changes global & local flags
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * SHFLAGS    *lflags;   output; local flags
 * SHFLAGS    *gflags;   output; global flags
 * int        *status;   output; return status
 */
{
	/* local variables */
	char     flagstr[BC_LINELTH+1];   /* flag string */
	SHFLAGS  flagbits;                /* flag bitmap */

	/* executable code */

	/* absolute */
	if  (cp_qstr(par,"FLAGS",BC_LINELTH,flagstr,status))  {
		sl_cnvshflags( flagstr, &flagbits, status );
		if  (*status != SHE_NOERROR)  return;
		if  (cp_qexist(par,"GLOBAL"))  *gflags = flagbits;
		*lflags = flagbits;
	} /*endif*/
	if  (*status != SHE_NOERROR)  return;

	/* set flags */
	if  (cp_qstr(par,"FLAGS+",BC_LINELTH,flagstr,status))  {
		sl_cnvshflags( flagstr, &flagbits, status );
		if  (*status != SHE_NOERROR)  return;
		if  (cp_qexist(par,"GLOBAL"))  {
			*gflags |= flagbits;
			*lflags = *gflags;
		} else {
			*lflags |= flagbits;
		} /*endif*/
	} /*endif*/
	if  (*status != SHE_NOERROR)  return;

	/* clear flags */
	if  (cp_qstr(par,"FLAGS-",BC_LINELTH,flagstr,status))  {
		sl_cnvshflags( flagstr, &flagbits, status );
		if  (*status != SHE_NOERROR)  return;
		if  (cp_qexist(par,"GLOBAL"))  {
			*gflags &= ~flagbits;
			*lflags = *gflags;
		} else {
			*lflags &= ~flagbits;
		} /*endif*/
	} /*endif*/
	if  (*status != SHE_NOERROR)  return;

} /* end of sl_chgflags */



/*------------------------------------------------------------------------*/



void sl_setsymbol( char *symbol, char *value, int *status )

/* sets new value of symbol.  Tries first local symbol then (if not
 * found) global symbol.
 *
 * parameters of routine
 * char       *symbol;   input; name of symbol to be changed (with "&")
 * char       *value;    input; new value of symbol
 * int        *status;   output; return status
 */
{
	/* executable code */

	if  (*symbol != SHC_CHWRITE)  {
		*status = SHE_NOSYM;
		err_setcontext( " ## symbol " ); err_setcontext( symbol );
		return;
	} /*endif*/

	ss_change( SHC_SYMLOC, symbol+1, value, status );
	if  (*status == SSE_UDSYM)  {
		*status = SSE_NOERROR;
		ss_change( SHC_SYMGLB, symbol+1, value, status );
	} /*endif*/

} /* end of sl_setsymbol */



/*------------------------------------------------------------------------*/



void sl_prepfilnam( char fil[], int *pos, int *status )

/* determines position of filter from filter name (position is either
 * omitted - this result in position number 0 - or the position number
 * is appended to the name by a hyphen ("-").
 *
 * parameters of routine
 * char       fil[];       modify; filter name (position number is removed)
 * int        *pos;        output; position number
 * int        *status;     output; return status
 */
{
	/* executable code */

	*pos = 0;
	while  (*fil != '\0')  {
		if  (*fil == '-')  {
			*fil = '\0';
			if  (*(++fil) == '\0')  {
				*status = SHE_INTCNV;
				return;
			} /*endif*/
			if  (sscanf(fil,"%d",pos) != 1)  {
				*status = SHE_INTCNV;
				return;
			} /*endif*/
			return;
		} /*endif*/
		fil++;
	} /*endwhile*/

} /* end of sl_prepfilnam */



/*------------------------------------------------------------------------*/



void sl_remav( REAL a[], long lth )

/* removes mean value from trace
 *
 * parameters of routine
 * REAL       a[];      modify; trace to remove mean value
 * long       lth;      input; length of trace
 */
{
	/* local variables */
	REAL     *r;       /* moving pointer */
	REAL     mean;     /* mean value */

	/* executable code */

	mean = 0.0;
	for  (r=a; r<(a+lth); mean += *r++) {}
	mean /= (REAL)lth;
	for  (r=a; r<(a+lth); *r++ -= mean) {}

} /* end of sl_remav */



/*------------------------------------------------------------------------*/



void sl_parselist( char liststr[], int maxstrlth, char infoname[],
	char lostr[], char histr[], char *cmp_op, BOOLEAN *neg, int *status )

/* parses list string and extracts info entry name, lo & hi bounds,
 * compare operation and whether or not the condition is negated
 *
 * parameters of routine
 * char       liststr[];     input; list string to be parsed
 * int        maxstrlth;     input; maximum length of output strings
 * char       infoname[];    output; name of info entry
 * char       lostr[];       output; lo bound string
 * char       histr[];       output; hi bound string
 * char       *cmp_op;       output; compare operation
 * BOOLEAN    *neg;          output; condition must be negated
 * int        *status;       output; return status
 */
{
	/* local variables */
	char     name[BC_LINELTH+1];    /* name of info entry */
	char     index[BC_LINELTH+1];   /* index string */
	char     *c;                    /* search pointer */
	int      slth;                  /* string length */

	/* executable code */

	/* parse string (name is hopefully shorter than BC_LINELTH) */
	tr_parse( liststr, cmp_op, name, index, status );
	if  (*status != SHE_NOERROR)  return;

	/* get name of info entry */
	slth = (int)strlen( name );
	*neg = (name[slth-1] == '~');
	if  (*neg)  name[--slth] = '\0';
	if  (*cmp_op != '_')  {
		*status = SHE_NOLIST;
		return;
	} /*endif*/

	if  (!sl_strcpy( infoname, name, maxstrlth, status ))  return;

	/* find colon */
	c = index;
	while  ((*c != '\0') && (*c != ':'))
		c++;

	/* determine compare operation and bound strings */
	slth = (int)strlen( index );
	if  (*c == ':')  {                     /* colon exists */
		slth--;
		if  (*index == ':')  {              /* no lower bound */
			*cmp_op = '<';
			if  (!sl_strcpy( histr, index+1, maxstrlth, status ))  return;
			*lostr = '\0';
		} else if  (index[slth] == ':')  {  /* no upper bound */
			*cmp_op = '>';
			index[slth] = '\0';
			if  (!sl_strcpy( lostr, index, maxstrlth, status ))  return;
			*histr = '\0';
		} else {
			*cmp_op = 'B';                   /* both bounds */
			*c++ = '\0';
			if  (!sl_strcpy( lostr, index, maxstrlth, status ))  return;
			if  (!sl_strcpy( histr, c, maxstrlth, status ))  return;
		} /*endif*/
	} else if  (slth == 0)  {
		*cmp_op = ' ';
		*lostr = '\0';
		*histr = '\0';
	} else {                               /* no colon */
		*cmp_op = '=';
		if  (!sl_strcpy( lostr, index, maxstrlth, status ))  return;
		*histr = '\0';
	} /*endif*/

	if  (*lostr != '\0')  {
		tr_translate( lostr, maxstrlth, status );
		if  (*status != SHE_NOERROR)  return;
	} /*endif*/
	if  (*histr != '\0')  {
		tr_translate( histr, maxstrlth, status );
		if  (*status != SHE_NOERROR)  return;
	} /*endif*/

} /* end of sl_parselist */



/*------------------------------------------------------------------------*/



BOOLEAN sl_strcpy( char dst[], char src[], int maxlth, int *status )

/* copies string "src" to "dst", if the length of "dst" doesn't
 * exceed the length of "src" "maxlth".  If the source string is
 * too long the string is not copied, "*status" is set to an error
 * status and returns FALSE
 *
 * parameters of routine
 * char       dst[];      output; destination string
 * char       src[];      input; source string
 * int        maxlth;     input; maximum length of destination string
 * int        *status;    output; return status
 *                        returns TRUE if string is copied
 */
{

	/* executable code */

	if  (strlen(src) > maxlth)  {
		*status = SHE_STROVFL;
		return FALSE;
	} /*endif*/
	strcpy( dst, src );
	return TRUE;

} /* end of sl_strcpy */



/*------------------------------------------------------------------------*/



void sl_resample( REAL old_dt, long old_lth, SAMPLE old[],
	REAL new_dt, long new_lth, SAMPLE new[] )

/* resamples data from "old" array to output array "new"
 *
 * parameters of routine
 * REAL       old_dt;       input; sample distance (sec) of input data
 * long       old_lth;      input; length of input data
 * SAMPLE     old[];        input; input data to be resampled
 * REAL       new_dt;       input; sample distance of output data
 * long       new_lth;      input; length of output data
 * SAMPLE     new[];        output; resampled data
 */
{
	/* local variables */
	long     i;              /* sample counter */
	REAL     curr_time;      /* current time */
	REAL     old_pos;        /* current position in old_dt units */
	long     old_idx;        /* next (lower) sample index of input-array */

	/* executable code */

	*new++ = *old;
	for  (i=1; i<new_lth; i++)  {
		curr_time = (REAL)i * new_dt;
		old_pos = curr_time / old_dt;
		old_idx = Nlong(floor(old_pos));
		old_pos -= floor(old_pos);
		if  (old_idx >= (old_lth-1))  {
			*new++ = old[old_lth-1];
		} else {
			*new++ = old[old_idx] + (old[old_idx+1]-old[old_idx])*old_pos;
		} /*endif*/
	} /*endfor*/

} /* end of sl_resample */



/*------------------------------------------------------------------------*/



void sl_decimate( long old_lth, SAMPLE old[], int decimation, BOOLEAN nomean,
	long new_lth, SAMPLE new[] )

/* decimates data from "old" array to output array "new" using mean values
 *
 * parameters of routine
 * long       old_lth;      input; length of input data
 * SAMPLE     old[];        input; input data to be resampled
 * int        decimation;   input; decimation factor
 * BOOLEAN    nomean;       input; take no mean value, plain resampling
 * long       new_lth;      input; length of output data
 * SAMPLE     new[];        output; resampled data
 */
{
	/* local variables */
	long     ncnt, ocnt;     /* sample counters of new and old trace */
	int      i;              /* decimation counter */
	int      misscnt;        /* counter of missing samples */
	SAMPLE   vmin, vmax;     /* min max values */

	/* executable code */

	if  (nomean)  {
#ifdef XXX
		/* this is the plain decimation, is not recommended */
		ocnt = 0;
		for  (ncnt=0; ncnt<new_lth; ncnt++)  {
			new[ncnt] = old[ocnt];
			ocnt += decimation;
			if  (ocnt >= old_lth)  ocnt -= decimation;
		} /*endfor*/
#endif
		/* take here the maximum value out of the decimation window */
		ocnt = 0;
		for  (ncnt=0; ncnt<new_lth; ncnt++)  {
			vmin = vmax = old[ocnt++];
			for  (i=1; i<decimation; i++)  {
				if  (ocnt < old_lth)  {
					if  (old[ocnt] < vmin)  vmin = old[ocnt];
					if  (old[ocnt] > vmax)  vmax = old[ocnt];
				} /*endif*/
				ocnt++;
			} /*endfor*/
			if  (fabs(vmax) > fabs(vmin))  {
				new[ncnt] = vmax;
			} else {
				new[ncnt] = vmin;
			} /*endif*/
		} /*endfor*/
	} else {
		ocnt = 0;
		for  (ncnt=0; ncnt<new_lth; ncnt++)  {
			misscnt = 0;
			new[ncnt] = 0.0;
			for  (i=0; i<decimation; i++)  {
				if  (ocnt < old_lth)  {
					new[ncnt] += old[ocnt];
				} else {
					misscnt++;
				} /*endif*/
				ocnt++;
			} /*endfor*/
			if  (misscnt == decimation)  {
				printf( "*** sl_decimate: this cannot happen ***\n" );
				misscnt = 0;
			} /*endif*/
			new[ncnt] /= (REAL)(decimation-misscnt);
		} /*endfor*/
	} /*endif*/

} /* end of sl_decimate */



/*------------------------------------------------------------------------*/



void sl_getlowercase( char instr[], char lowop, char outstr[] )

/* converts uppercase input string to mixedcase outstr.  A character "lowop"
 * found in "instr" makes the next character lowercase
 *
 * parameters of routine
 * char       instr[];     input; uppercase string with "lowop's"
 * char       lowop;       input; lowercase operator
 * char       outstr[];    output; output string
 */
{
	/* executable code */

	while  (*instr != '\0')  {
		if  (*instr == lowop)  {
			if  (*(++instr) != '\0')  {
				*outstr++ = tolower( *instr );
				instr++;
			} /*endif*/
		} else {
			*outstr++ = *instr++;
		} /*endif*/
	} /*endwhile*/
	*outstr = '\0';

} /* end of sl_getlowercase */



/*------------------------------------------------------------------------*/



void sl_makelowercase( char instr[], char outstr[] )

/* converts uppercase input string to lowercase
 *
 * parameters of routine
 * char       instr[];     input; uppercase string
 * char       outstr[];    output; output string
 */
{
	/* executable code */

	while  (*instr != '\0')  {
		*outstr++ = tolower( *instr );
		instr++;
	} /*endwhile*/
	*outstr = '\0';

} /* end of sl_getlowercase */



/*----------------------------------------------------------------------------*/



void sl_prepformatstr( char fmt[] )

/* prepares format string to be used in a printf command
 *
 * parameters of routine
 * char       fmt[];         modify; format string
 */
{
	/* local variables */
	int      slth;     /* string length */

	/* executable code */

	sl_getlowercase( fmt, '@', fmt );
	if  (*fmt == '<')  {
		strcpy( fmt, fmt+1 );
		slth = (int)strlen( fmt ) - 1;
		if  (fmt[slth] == '>')
			fmt[slth] = '\0';
	   while  (--slth >= 0)
			if  (fmt[slth] == '_')
				fmt[slth] = ' ';
	} /*endif*/

} /* end of sl_prepformatstr */



/*----------------------------------------------------------------------------*/



void sl_make_qname( char station[], char start[], int maxlth, char qname[],
	STATUS *status )

/* creates q-filename from station name & start time
 *
 * parameters of routine
 * char       station[];       input; station name
 * char       start[];         input; start time string
 * int        maxlth;          input; maximum length of output string
 * char       qname[];         output; created q-filename
 * STATUS     *status;         output; return status
 */
{
	/* local variables */
	char     numstr[BC_LINELTH+1];   /* number string */
	NTIME    ntime;                  /* numeric time */
	int      yr;                     /* year */

	/* executable code */

	/* created name Q_sss(s)_yymmdd_hhmm */
	/*              01234 5 678901234567 */

	*qname = '\0';
	if  ((strlen(station)+14) > maxlth)  {
		*status = SHE_STROVFL;
		return;
	} /*endif*/

	tc_t2n( start, &ntime, status );
	if  (Severe(status))  return;
	yr = ntime.year - 1900;
	if  (yr >= 100)  yr -= 100;
	sprintf( numstr, "%02d", yr );
	sprintf( numstr+2, "%02d", ntime.month );
	sprintf( numstr+4, "%02d", ntime.day );
	numstr[6] = '_';
	sprintf( numstr+7, "%02d", ntime.hour );
	sprintf( numstr+9, "%02d", ntime.min );

	strcpy( qname, "Q_" );
	strcat( qname, station );
	strcat( qname, "_" );
	strcat( qname, numstr );

} /* end of sl_make_qname */



/*----------------------------------------------------------------------------*/



void sl_bintoascii( BYTE bin[], long binlth, char **asc, long *asclth,
	int size, int bytesperline, STATUS *status )

/* converts binary array bin[0..binlth-1] to ASCII *asc[0..*asclth-1]
 *
 * parameters of routine
 * BYTE       bin[];        input; binary array
 * long       binlth;       input; length of binary array in BYTEs
 * char       **asc;        output; pointer to ascii array
 * long       *asclth;      output; length of ascii array in chars
 * int        size;         input; align size
 * int        bytesperline; input; bytes per line
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	int      alignchars;      /* additional chars */
	char     *c;              /* moving pointer */
   long     bcnt;            /* byte counter */
	unsigned tmp;             /* scratch */

	/* executable code */

	*asclth = binlth*2 + binlth/bytesperline;
	alignchars = size - (int)(*asclth % (long)size);
	*asclth += (long)alignchars;
	*asc = (char *)sy_allocmem( *asclth, (int)sizeof(char), status );
	if  (Severe(status))  return;
	c = *asc;

	for  (bcnt=1; bcnt<=binlth; bcnt++)  {
		tmp = (BYTE)(*bin++);
		sprintf( c, "%02x", tmp );
		c += 2;
		if  ((bcnt % bytesperline) == 0)
			*c++ = '\n';
	} /*endfor*/
	while  (alignchars-- > 0)
		*c++ = '\n';

} /* end of sl_bintoascii */



/*----------------------------------------------------------------------------*/



void sl_asciitobin( char asc[], long asclth, BYTE **bin, long *binlth,
	int size, STATUS *status )

/* converts ascii array back to binary array (inverse of sl_bintoascii)
 *
 * parameters of routine
 * char       asc[];      input; ascii array
 * long       asclth;     input; length of ascii array in chars
 * BYTE       **bin;      output; pointer to binary data
 * long       *binlth;    output; length of binary array in bytes
 * int        size;       input; align length in bytes
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	char     *c;       /* moving pointer */
	char     num[3];   /* digit string */
	int      i;        /* counter */
	BYTE     *b;       /* moving BYTE pointer */
	int      tmp;      /* scratch */

	/* executable code */

	/* count bytes not equal to '\n' */
	*binlth = 0;
	for  (c=asc; c<(asc+asclth); c++)
		if  (*c != '\n')
			(*binlth)++;

	if  (*binlth % (2*size))  {
		*status = SHE_SPECERROR+15;  /* doesn't fit for elements of */
		return;                      /* size "size" */
	} /*endif*/

	*binlth /= 2;
	*bin = (BYTE *)sy_allocmem( *binlth, (int)sizeof(BYTE), status );
	if  (Severe(status))  return;

	c = asc-- + asclth;
	num[2] = '\0';
	i = 0;
	b = *bin;
	while  (++asc < c)   {
		if  (*asc != '\n')  {
			num[i] = *asc;
			if  (i == 0)  {
				i = 1;
			} else {
				i = 0;
				sscanf( num, "%x", &tmp );
				*b++ = (BYTE)tmp;
			} /*endif*/
		} /*endif*/
	} /*endwhile*/

} /* end of sl_asciitobin */



/*------------------------------------------------------------------------*/



BOOLEAN sl_quali( PARAM *par, char *qname, int *value, STATUS *status )

/* gets value of qualifier "qname", returns whether qualifier was found
 * translates value if possible.
 */

/* parameters of routine
 * PARAM      *par;    input; command parameter
 * char       *qname;  input; name of qualifier
 * int        *value;  output; value of qualifier
 * STATUS     *status; output; return status
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];  /* find string */

	/* executable code */

	if  (!cp_qstr(par,qname,BC_LINELTH,str,status))
		return FALSE;
	tr_translate( str, BC_LINELTH, status );
	if  (Severe(status))  return FALSE;
	if  (sscanf(str,"%d",value) != 1)  {
		*status = SHE_INTCNV;
		err_setcontext( " ## numstr " ); err_setcontext( str );
		return FALSE;
	} /*endif*/
	return TRUE;

} /* end of sl_quali */



/*------------------------------------------------------------------------*/



BOOLEAN sl_qualr( PARAM *par, char *qname, float *value, STATUS *status )

/* gets value of qualifier "qname", returns whether qualifier was found.
 * translates value if possible.
 */

/* parameters of routine
 * PARAM      *par;    input; command parameter
 * char       *qname;  input; name of qualifier
 * float      *value;  output; value of qualifier
 * STATUS     *status; output; return status
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];  /* find string */

	/* executable code */

	if  (!cp_qstr(par,qname,BC_LINELTH,str,status))
		return FALSE;
	tr_translate( str, BC_LINELTH, status );
	if  (Severe(status))  return FALSE;
	if  (sscanf(str,"%f",value) != 1)  {
		*status = SHE_REALCNV;
		err_setcontext( " ## numstr " ); err_setcontext( str );
		return FALSE;
	} /*endif*/
	return TRUE;

} /* end of sl_qualr */



/*------------------------------------------------------------------------*/



BOOLEAN sl_quals( PARAM *par, char *qname, int maxlth, char *value,
	STATUS *status )

/* gets value of qualifier "qname", returns whether qualifier was found.
 * translates value if possible.
 */

/* parameters of routine
 * PARAM      *par;    input; command parameter
 * char       *qname;  input; name of qualifier
 * float      *value;  output; value of qualifier
 * STATUS     *status; output; return status
 */
{
	/* executable code */

	if  (!cp_qstr(par,qname,maxlth,value,status))
		return FALSE;
	tr_translate( value, BC_LINELTH, status );
	if  (Severe(status))  return FALSE;
	return TRUE;

} /* end of sl_quals */



/*----------------------------------------------------------------------------*/



void sl_make_timestr( char numstr[], char timestr[], STATUS *status )

/* converts number string "<year> <month> <day> <hour> <min> <sec> <ms>"
 * to SH time string
 *
 * parameters of routine
 * char       numstr[];     input; number string
 * char       timestr[];    output; time string
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	NTIME    t;        /* numeric time */

	/* executable code */

	if  (sscanf(numstr,"%d %d %d %d %d %d %d",
		&t.year,&t.month,&t.day,&t.hour,&t.min,&t.sec,&t.ms) != 7)  {
		*status = SHE_INTCNV;
		err_setcontext( " ## timestr " ); err_setcontext( numstr );
		return;
	} /*endif*/
	tc_n2t( &t, timestr, status );

} /* end of sl_make_timestr */



/*----------------------------------------------------------------------------*/



void sl_printname( CHMAP map )

/* ...
 *
 * parameter of routine
 * CHMAP     map;      input; output window
 */
{
	/* local variables */
	static char    aname[4][50];
	static BOOLEAN init=FALSE;
	int            i, j;

	/* executable code */

	if  (!init)  {
		strcpy( aname[0], "Bvuips;!!!Lmbvt!Tubnnmfs" );
		strcpy( aname[1], "Beesftt;!!Tfjtnpmphjtdifs![fousbmpctfswbupsjvn" );
		strcpy( aname[2], "!!!!!!!!!!Lsbolfoibvttus/!2.4" );
		strcpy( aname[3], "!!!!!!!!!!E.9631!Fsmbohfo-!GSH" );
		for  (i=0; i<4; i++)  {
			j = 0;
			while  (aname[i][j] != '\0')
				(aname[i][j++])--;
			aname[i][j] = '\n';
			aname[i][j+1] = '\0';
		} /*endfor*/
		init = TRUE;
	} /*endif*/

	for  (i=0; i<4; i++)
		gc_write( map, aname[i] );

} /* end of sl_printname */



/*----------------------------------------------------------------------------*/



void sl_cnvphasename( char phase[], int maxlth, char fname[],
	STATUS *status )

/* converts phase name to uppercase name
 *
 * parameters of routine
 * char       phase[];     input; input phase name
 * int        maxlth;      input; maximum length of output string
 * char       fname[];     output; uppercase name
 */
{
	/* local variables */
	int      i, j;      /* counters */

	/* executable code */

	i = j = 0;
	do  {
		if  (j >= maxlth)  {*status = SHE_STROVFL; return;}
		if  (islower(phase[i]))  {
			fname[j++] = 'V';
			fname[j++] = Cap( phase[i] );
		} else {
			fname[j++] = phase[i];
		} /*endif*/
	}  while (phase[i++] != '\0');

} /* end of sl_cnvphasename */



/*----------------------------------------------------------------------------*/



void sl_oscall( char cmd[], char par1[], char par2[], STATUS *status )

/* call to special functions of operating systems
 *
 * parameters of routine
 * char       cmd[];       input; command verb
 * char       par1[];      input; command parameter 1
 * char       par2[];      input; command parameter 2
 */
{
	/* executable code */

	if  (strcmp(cmd,"FDELETE") == 0)  {
		sy_fdelete( par1 );
	} else if  (strcmp(cmd,"FRENAME") == 0)  {
		sy_frename( par1, par2 );
	} else {
		*status = SHE_UKKEY;
		return;
	} /*endif*/

} /* end of sl_oscall */



/*----------------------------------------------------------------------------*/



void sl_parse_stations( char list[],
	char stat[SHC_ILISTLTH][SHC_STATNAMELTH+1], int *statno )

/* Parses station list string "list" and returns list of stations "stat".
 *
 * parameters of routine
 * char       list[];     input; station list (separator ",")
 * char       stat[][];   output; parsed station names
 * int        *statno;    output; number of names found
 */
{
	/* local variables */
	char     llist[cBcVeryLongStrLth+1];  /* local copy of list */
	char     *llp;                 /* pointer to local list */
	char     *cptr;                /* pointer to comma */
	char     lfname[cBcFileLth+1]; /* name of list file */
	FILE     *fp;                  /* pointer to file */
	char     line[cBcLineLth+1];   /* current line of file */
	int      slen;                 /* string length */
	char     *globpath;            /* pointer to globals path */
	int      pathcnt;              /* path counter */
	TSyStatus locstat;             /* local status */

	/* executable code */

	/* first check database and call this routine recursively */
	if  (strncmp(list,"DB:",3) == 0 || strncmp(list,"db:",3) == 0)  {
		locstat = cBcNoError;
		SqlTranslateStationList( list, cBcVeryLongStrLth, llist, &locstat );
		if  (SySevere(&locstat))  {*statno = 0; return;}
		sl_parse_stations( llist, stat, statno );
		return;
	} /*endif*/

	if  (strlen(list) > cBcVeryLongStrLth)  {
		strcpy( llist, "LIST TOO LONG" );
	} else {
		strcpy( llist, list );
		ut_cap( llist );
	} /*endif*/
	llp = llist;

	if  (strcmp(llist,"ALL") == 0)  {
		*statno = 0;
		strcpy( stat[(*statno)++], "BFO" );
		strcpy( stat[(*statno)++], "BRG" );
		strcpy( stat[(*statno)++], "BRNL" );
		strcpy( stat[(*statno)++], "BSEG" );
		strcpy( stat[(*statno)++], "BUG" );
		strcpy( stat[(*statno)++], "CLL" );
		strcpy( stat[(*statno)++], "CLZ" );
		strcpy( stat[(*statno)++], "FBE" );
		strcpy( stat[(*statno)++], "FUR" );
		strcpy( stat[(*statno)++], "GEC2" );
		strcpy( stat[(*statno)++], "GRFO" );
		strcpy( stat[(*statno)++], "GSH" );
		strcpy( stat[(*statno)++], "GTTG" );
		strcpy( stat[(*statno)++], "GUNZ" );
		strcpy( stat[(*statno)++], "HAM" );
		strcpy( stat[(*statno)++], "HLG" );
		strcpy( stat[(*statno)++], "IBBN" );
		strcpy( stat[(*statno)++], "LID" );
		strcpy( stat[(*statno)++], "MANZ" );
		strcpy( stat[(*statno)++], "MOX" );
		strcpy( stat[(*statno)++], "NEUB" );
		strcpy( stat[(*statno)++], "NOTT" );
		strcpy( stat[(*statno)++], "NRDL" );
		strcpy( stat[(*statno)++], "RGN" );
		strcpy( stat[(*statno)++], "ROTZ" );
		strcpy( stat[(*statno)++], "RUE" );
		strcpy( stat[(*statno)++], "STU" );
		strcpy( stat[(*statno)++], "TANN" );
		strcpy( stat[(*statno)++], "TNS" );
		strcpy( stat[(*statno)++], "UBBA" );
		strcpy( stat[(*statno)++], "WERN" );
		strcpy( stat[(*statno)++], "WERD" );
		strcpy( stat[(*statno)++], "WET" );
		return;
	} else if  (strcmp(llist,"GRSN") == 0)  {
		*statno = 0;
		strcpy( stat[(*statno)++], "BFO" );
		strcpy( stat[(*statno)++], "BRG" );
		strcpy( stat[(*statno)++], "BRNL" );
		strcpy( stat[(*statno)++], "BSEG" );
		strcpy( stat[(*statno)++], "BUG" );
		strcpy( stat[(*statno)++], "CLL" );
		strcpy( stat[(*statno)++], "CLZ" );
		strcpy( stat[(*statno)++], "FUR" );
		strcpy( stat[(*statno)++], "GEC2" );
		strcpy( stat[(*statno)++], "GRFO" );
		strcpy( stat[(*statno)++], "GSH" );
		strcpy( stat[(*statno)++], "GTTG" );
		strcpy( stat[(*statno)++], "HAM" );
		strcpy( stat[(*statno)++], "HLG" );
		strcpy( stat[(*statno)++], "IBBN" );
		strcpy( stat[(*statno)++], "LID" );
		strcpy( stat[(*statno)++], "MOX" );
		strcpy( stat[(*statno)++], "NOTT" );
		strcpy( stat[(*statno)++], "NRDL" );
		strcpy( stat[(*statno)++], "RGN" );
		strcpy( stat[(*statno)++], "RUE" );
		strcpy( stat[(*statno)++], "STU" );
		strcpy( stat[(*statno)++], "TNS" );
		strcpy( stat[(*statno)++], "UBBA" );
		strcpy( stat[(*statno)++], "WET" );
		return;
	} else if  (strcmp(llist,"SXNET") == 0)  {
		*statno = 0;
		strcpy( stat[(*statno)++], "GUNZ" );
		strcpy( stat[(*statno)++], "NEUB" );
		strcpy( stat[(*statno)++], "TANN" );
		strcpy( stat[(*statno)++], "WERD" );
		return;
	} else if  (strcmp(llist,"GOOD") == 0)  {
		*statno = 0;
		strcpy( stat[(*statno)++], "BFO" );
		strcpy( stat[(*statno)++], "BRG" );
		strcpy( stat[(*statno)++], "BUG" );
		strcpy( stat[(*statno)++], "CLL" );
		strcpy( stat[(*statno)++], "CLZ" );
		strcpy( stat[(*statno)++], "FUR" );
		strcpy( stat[(*statno)++], "GEC2" );
		strcpy( stat[(*statno)++], "TNS" );
		strcpy( stat[(*statno)++], "WET" );
		return;
	} else if  (strcmp(llist,"BEST") == 0)  {
		*statno = 0;
		strcpy( stat[(*statno)++], "BFO" );
		strcpy( stat[(*statno)++], "CLZ" );
		strcpy( stat[(*statno)++], "TNS" );
		strcpy( stat[(*statno)++], "WET" );
		return;
	} else if  (strcmp(llist,"GRF") == 0)  {
		*statno = 0;
		strcpy( stat[(*statno)++], "GRA1" );
		strcpy( stat[(*statno)++], "GRA2" );
		strcpy( stat[(*statno)++], "GRA3" );
		strcpy( stat[(*statno)++], "GRA4" );
		strcpy( stat[(*statno)++], "GRB1" );
		strcpy( stat[(*statno)++], "GRB2" );
		strcpy( stat[(*statno)++], "GRB3" );
		strcpy( stat[(*statno)++], "GRB4" );
		strcpy( stat[(*statno)++], "GRB5" );
		strcpy( stat[(*statno)++], "GRC1" );
		strcpy( stat[(*statno)++], "GRC2" );
		strcpy( stat[(*statno)++], "GRC3" );
		strcpy( stat[(*statno)++], "GRC4" );
		return;
	} else if  (*llist == '_')  {
		*statno = 0;
		/* build list filename and try to open */
		for  (pathcnt=0;;pathcnt++)  {
			globpath = GpGetStringElem( cGpL_defpath_globals, pathcnt );
			if  (globpath == NULL)  break;
			if  (strlen(globpath)+strlen(llist)+5 > cBcFileLth)  {
				fprintf( stderr, "*SH: string overflow in parse_stations\n" );
				*statno = 0;
				return;
			} /*endif*/
			sprintf( lfname, "%s/%s.STA", globpath, llist );
			fp = fopen( lfname, "r" );
			if  (fp != NULL)  break;
		} /*endfor*/
		if  (fp == NULL)  {
			fprintf( stderr, "*SH: input file %s not found\n", lfname );
			return;
		} /*endif*/
		while  (fgets(line,cBcLineLth,fp) != NULL)  {
			slen = strlen( line ) - 1;
			if  (line[slen] == '\n')  line[slen] = '\0';
			if  (strlen(line) > SHC_STATNAMELTH)  {
				fprintf( stderr, "*SH: station name %s too long; ignored\n", line );
				continue;
			} /*endif*/
			if  (*statno == SHC_ILISTLTH)  {
				fprintf( stderr, "*SH: too many stations in %s\n", lfname );
				*statno = 0;
				fclose( fp );
				return;
			} /*endif*/
			strcpy( stat[(*statno)++], line );
		} /*endwhile*/
		fclose( fp );
		return;
	} /*endif*/

	*statno = 0;
	FOREVER  {
		cptr = strchr( llp, ',' );
		if  (cptr == NULL) break;
		*cptr = '\0';
		if  (strlen(llp) <= SHC_STATNAMELTH)  {
			if  (*statno == SHC_ILISTLTH)  {
				printf( "--> sl_parse_stations: too many stations\n" );
			} else {
				strcpy( stat[(*statno)++], llp );
			} /*endif*/
		} /*endif*/
		llp = cptr + 1;
	} /*endwhile*/

	if  (*statno < SHC_ILISTLTH)  {
		if  (strlen(llp) <= SHC_STATNAMELTH && strlen(llp) > 0)
			strcpy( stat[(*statno)++], llp );
	} else {
		printf( "--> sl_parse_stations: too many stations\n" );
	} /*endif*/

} /* end of sl_parse_stations */



/*----------------------------------------------------------------------------*/

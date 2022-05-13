
/* File SHTRANSL.C
 *      ==========
 *
 * version 14, 13-Nov-2006
 *
 * SeismicHandler translation routines for parameters
 * K. Stammler, 29-MAR-1990
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
#include BC_SYSBASE
#include "shconst.h"
#include "shvars.h"
#include "cpusrdef.h"
#include "erusrdef.h"
#include "trusrdef.h"
#include "ssusrdef.h"
#include "tcusrdef.h"
#include "infoidx.h"
#include "fctxsl.h"   /* include loop */
#include "fctxml.h"
#include "fctxdm.h"   /* include loop */
#include "trerrors.h"
#include "utusrdef.h"
#include "sserrors.h"
#include "sherrors.h"
#include "globalparams.h"

#define PCFREAD '%'
#define PCSYMBOL '\"'
#define PCCMDPAR '#'
#define PCINFENT '^'
#define PCINTERN '$'
#define PCCONCAT '|'
#define MAXCONCAT 10

#define tr_isvar(c) (((c) >= '\"') && ((c) <= '%') || ((c) == '^') || ((c) == '|'))


static void   *runtrc_trv;    /* running trace pointer (for PCINFENT) */
static int    runidx_trv;     /* running index number */


/* prototypes of local routines */
static void tr_concat( char str[], int maxlth, int *status );
static void tr_fillstr( char ch, char *idxstr, int maxlth, char *str, int *status );


/*------------------------------------------------------------------------*/



void tr_partrans( PARAM *par, STATUS *status )

/* translates all parameters in "par"
 *
 * parameters of routine
 * PARAM    *par;           modify; parameters to be translated
 * int      *status;        output; return status
 */
{
	/* local variables */
	int      i;        /* counter */
	BOOLEAN  noinfo;   /* info not found */

	/* executable code */

	noinfo = FALSE;
	for  (i=0;i<=par->pno;i++)  {
		if  (tr_isvar(par->p[i][0]))  {
			tr_translate( par->p[i], CPC_LINELTH, status );
			if  (*status == SHE_NOINFO)  {
				noinfo = TRUE;
				*status = TRE_NOERROR;
			} /*endif*/
			if  (*status != TRE_NOERROR)  return;
		} /*endif*/
	} /*endfor*/
	if  (noinfo)  *status = SHE_NOINFO;

} /* end of tr_partrans */



/*------------------------------------------------------------------------*/



void tr_translate( char str[], int maxlth, STATUS *status )

/* translates string
 *
 * parameters of routine
 * char     *str;           modify; string to be translated
 * int      maxlth;         maximum string length
 * int      *status;        output; return status
 */
{
	/* local variables */
	char     chid;                  /* char ID */
	char     name[CPC_LINELTH+1];   /* name of variable */
	char     idxstr[CPC_LINELTH+1]; /* index string */

	/* executable code */

	if  (!tr_isvar(*str))  return;

	if  (*str == PCCONCAT)  {
		tr_concat( str, maxlth, status );
	} else {
		tr_parse( str, &chid, name, idxstr, status );
		if  (*status != TRE_NOERROR)  return;
		if  (tr_isvar(*name))  {
			tr_translate( name, CPC_LINELTH, status );
			if  (*status != TRE_NOERROR)  return;
		} /*endif*/
		if  (tr_isvar(*idxstr))  {
			tr_translate( idxstr, CPC_LINELTH, status );
			if  (*status != TRE_NOERROR)  return;
		} /*endif*/
		switch  (chid)  {
			case PCFREAD:
				tr_rdline( name, idxstr, maxlth, str, status );
				break;
			case PCSYMBOL:
				ss_getval( SHC_SYMLOC, name, maxlth, str, status );
				if  (*status == SSE_UDSYM)  {
					*status = SSE_NOERROR;
					ss_getval( SHC_SYMGLB, name, maxlth, str, status );
				} /*endif*/
				break;
			case PCCMDPAR:
				ss_getpar( name, maxlth, str, status );
				if  (Severe(status))  return;
				while  (tr_isvar(*str))  {
					tr_translate( str, maxlth, status );
					if  (Severe(status))  return;
				} /*endif*/
				break;
			case PCINFENT:
				tr_infent( name, idxstr, maxlth, str, status );
				if  (*status == SHE_NOINFO)  strncpy( str, "-?-", maxlth);
				break;
			case PCINTERN:
				tr_intern( name, idxstr, maxlth, str, status );
				break;
			default:
				break;
		} /*endswitch*/
	} /*endif*/

} /* end of tr_translate */



/*------------------------------------------------------------------------*/



void tr_parse( char str[], char *chid, char name[], char idxstr[],
	STATUS *status )

/* extracts from string char ID, variable name & index string
 *
 * parameters of routine
 * char     str[];          input; string to be parsed
 * char     *chid;          output; char ID
 * char     name[];         output; name of variable
 * char     idxstr[];       output; index string
 * int      *status;        output; return status
 */
{
	/* local variables */

	/* executable code */

	*chid = *str++;
	*name = '\0';
	*idxstr = '\0';

	while  (*str != '\0'  &&  *str != '(')
		*name++ = *str++;

	if  (*(name-1) == *chid)  name--;
	*name = '\0';
	if  (*str == *chid)  str++;
	if  (*str == '\0')  return;
	if  (*str++ != '(')  {
		*status = TRE_ILIDX;
		return;
	} /*endif*/

	while  (*str != '\0'  &&  *str != ')')
		*idxstr++ = *str++;

	*idxstr = '\0';
	if  (*str == ')')
		if  (*(++str) != '\0')  *status = TRE_PRADD;

} /* end of tr_parse */



/*------------------------------------------------------------------------*/



void tr_rdline( char name[], char idxstr[], int maxlth, char str[],
	STATUS *status )

/* reads line number "idxstr" from file "name"
 *
 * parameters of routine
 * char     name[];         input; name of file
 * char     idxstr[];       input; line number
 * int      maxlth;         input; maximum string length (of str)
 * char     str[];          output; line read
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      lineno;               /* line number */
	FILE     *fp;                  /* file pointer */
	char     fname[BC_FILELTH+1];  /* filename */
	char     *globpath;            /* pointer to globals path */
	int      pathcnt;              /* path counter */

	/* executable code */

	if  (*idxstr == '\0')  {
		lineno = 1;
	} else if  (sscanf(idxstr,"%d",&lineno) != 1)  {
		*status = TRE_CNVIDX;
		err_setcontext( " ## number " ); err_setcontext( idxstr );
		return;
	} /*endif*/

	/* open file, try all globals paths if no absolute path */
	if  (*name == '/' || *name == '\\')  {
		fp = sy_fopen( name, "r" );
	} else {
		for  (pathcnt=0;;pathcnt++)  {
			globpath = GpGetStringElem( cGpL_defpath_globals, pathcnt );
			if  (globpath == NULL)  break;
			if  ((strlen(fname)+strlen(globpath)+1) > BC_FILELTH)  {
				*status = TRE_STROVFL;
				err_setcontext( " ## filename too long" );
				return;
			} /*endif*/
			strcpy( fname, globpath );
			strcat( fname, "/" );
			strcat( fname, name );
			ut_defext( fname, BC_FILELTH, SHC_DE_TEXT, status );
			if  (*status != TRE_NOERROR)  return;
			fp = sy_fopen( fname, "r" );
			if  (fp != NULL)  break;
		} /*endfor*/
	} /*endif*/
	if  (fp == NULL)  {
		*status = TRE_OPNRD;
		err_setcontext( " ## file " ); err_setcontext( fname );
		return;
	} /*endif*/

	/* if lineno == 0, determine length of file */
	if  (lineno == 0)  {
		while  (fgets(str,maxlth,fp) != NULL)
			lineno++;
		sprintf( str, "%d", lineno );
		fclose( fp );
		return;
	} /*endif*/

	/* read specified line */
	while  (--lineno >= 0)  {
		if  (fgets(str,maxlth,fp) == NULL)  {
			*status = TRE_RDFIL;
			fclose( fp );
			err_setcontext( " ## file " ); err_setcontext( fname );
			return;
		} /*endif*/
	} /*endwhile*/
	fclose( fp );

	/* drop control chars at end */
	lineno = (int)strlen( str );
	while  (str[--lineno] < ' ')
		str[lineno] = '\0';

} /* tr_rdline */



/*------------------------------------------------------------------------*/



void tr_infent( char name[], char idxstr[], int maxlth, char str[],
	STATUS *status )

/* gets info "name" from trace number "idxstr"
 *
 * parameters of routine
 * char     name[];         input; name of info
 * char     idxstr[];       input; trace number
 * int      maxlth;         input; maximum string length (of str)
 * char     str[];          output; info read
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      trcno;      /* trace number */
	long     l_inf;      /* long info */
	int      i_inf;      /* int & byte info */
	float    r_inf;      /* real info */
	void     *p_inf;     /* ptr info */
	TIME     t_inf;      /* time info */
	unsigned ientry;     /* info entry */
	void     *trc;       /* trace pointer */
	int      dln;        /* display list number */
	int      cnt;        /* counter */
	char     wstr[BC_SHORTSTRLTH+1]; /* number of window (num string) */

	/* executable code */

	if  (*idxstr == 'W')  {    /* not default window */
		cnt = 0;
		idxstr++;
		while  (isdigit(*idxstr) && (cnt <= BC_SHORTSTRLTH))
			wstr[cnt++] = *idxstr++;
		wstr[cnt] = '\0';
		if  (*idxstr != ':')  {
			*status = TRE_ILWDW;
			return;
		} /*endif*/
		idxstr++;
		if  (sscanf( wstr, "%d", &dln ) != 1)  {
			*status = TRE_ILWDW;
			return;
		} /*endif*/
#  ifdef XXX
	} else if  (*idxstr == 'H')  {
		if  (idxstr[1] == ':')  {
			dln = SHC_HIDDENLIST;
			idxstr += 2;
		} else {
			dln = gc;
		} /*endif*/
#  endif  /* XXX */
	} else {
		dln = gc; /* (gc == 0) ? SHC_HIDDENLIST : gc; */
	} /*endif*/

	if  (*idxstr == '\0')  {
		trc = ml_trc( dln, 1, status );
		if  (*status != TRE_NOERROR)  return;
	} else if  (*idxstr == 'X' || strcmp(idxstr,"$X") == 0)  {
		if  (runtrc_trv == NULL)  return;  /* don't translate */
		trc = runtrc_trv;
	} else if  (*idxstr == 'A' && idxstr[1] == ':') {
		if  (sscanf( idxstr+2, "%lp", &trc ) != 1)  {
			*status = TRE_CNVIDX;
			err_setcontext( " ## numstr " ); err_setcontext( idxstr+2 );
			return;
		} /*endif*/
	} else if  (sscanf(idxstr,"%d",&trcno) != 1)  {
		*status = TRE_CNVIDX;
		err_setcontext( " ## numstr " ); err_setcontext( idxstr );
		return;
	} else {
		trc = ml_trc( dln, trcno, status );
		if  (*status != TRE_NOERROR)  return;
	} /*endif*/

	db_ident( name, &ientry, status );
	if  (*status != TRE_NOERROR)  return;

	switch  (ientry & E_TYPMASK)  {
	case EL_TYPE:
		l_inf = db_getl( trc, ientry, status );
		if  (*status != TRE_NOERROR)  return;
		sprintf( str, "%ld", l_inf );
		break;
	case EI_TYPE:
		i_inf = db_geti( trc, ientry, status );
		if  (*status != TRE_NOERROR)  return;
		sprintf( str, "%d", i_inf );
		break;
	case EB_TYPE:
		i_inf = db_geti( trc, ientry, status );
		if  (*status != TRE_NOERROR)  return;
		sprintf( str, "%d", i_inf );
		break;
	case ER_TYPE:
		r_inf = db_getr( trc, ientry, status );
		if  (*status != TRE_NOERROR)  return;
		sprintf( str, "%e", r_inf );
		break;
	case ES_TYPE:
		db_gets( trc, ientry, maxlth, str, status );
		break;
	case EC_TYPE:
		*str = db_getc( trc, ientry, status );
		str[1] = '\0';
		if  (*status != TRE_NOERROR)  return;
		break;
	case ET_TYPE:
		db_gett( trc, ientry, &t_inf, status );
		if  (*status != TRE_NOERROR)  return;
		if  (maxlth < 30)  { *status = TRE_STROVFL; return; }
		tc_a2t( &t_inf, str, status );
		break;
	case EP_TYPE:
		p_inf = db_getp( trc, ientry, status );
		if  (*status != TRE_NOERROR)  return;
		sprintf( str, "%ld", p_inf );
		break;
	case EF_TYPE:
		i_inf = db_getf( trc, ientry, status );
		if  (*status != TRE_NOERROR)  return;
		*str = (i_inf) ? 'Y' : 'N';
		str[1] = '\0';
		break;
	case EX_TYPE:
		db_getx( trc, ientry, maxlth, str, status );
		break;
	default:
		*status = TRE_ILTYPE;
		return;
	} /*endswitch*/

} /* tr_infent */



/*------------------------------------------------------------------------*/



void tr_intern( char name[], char idxstr[], int maxlth, char str[],
	STATUS *status )

/* returns internal variable 
 *
 * parameters of routine
 * char       name[];    input; name of variable
 * char       idxstr[];  input; index string
 * int        maxlth;    input; maximum length of output string
 * char       str[];     output; translated string
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	int      num;       /* number */
	char     numstr[3]; /* number string */
	float    r, r1;     /* scratch */

	/* executable code */

	if  (strcmp(name,"DSPTRCS") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		if  (maxlth < 5)  {*status = TRE_STROVFL; return;}
		sprintf( str, "%d", db_dsplth(gc) );
	} else if  (strcmp(name,"TOTTRCS") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		if  (maxlth < 5)  {*status = TRE_STROVFL; return;}
		sprintf( str, "%d", db_lstlth() );
	} else if  (strcmp(name,"STATUS") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		if  (maxlth < 6)  {*status = TRE_STROVFL; return;}
		sprintf( str, "%d", shv_last_status );
	} else if  (strcmp(name,"SYSTIME") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		if  (maxlth < 25)  {*status = TRE_STROVFL; return;}
		sy_gettime( str );
	} else if  (strcmp(name,"X") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		if  (maxlth < 6)  {*status = TRE_STROVFL; return;}
		if  (runidx_trv == 0)  return;  /* don't translate */
		sprintf( str, "%d", runidx_trv );
	} else if  (strcmp(name,"VERSION") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		if  (maxlth < 17)  {*status = TRE_STROVFL; return;}
		strcpy( str, SHC_VERSION );
	} else if  (strcmp(name,"DSP_X") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		dm_dspcoo( &r, NULL, NULL, NULL );
		sprintf( str, "%e", r );
	} else if  (strcmp(name,"DSP_Y") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		dm_dspcoo( NULL, &r, NULL, NULL );
		sprintf( str, "%e", r );
	} else if  (strcmp(name,"DSP_W") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		dm_dspcoo( NULL, NULL, &r, NULL );
		sprintf( str, "%e", r );
	} else if  (strcmp(name,"DSP_H") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		dm_dspcoo( NULL, NULL, NULL, &r );
		sprintf( str, "%e", r );
	} else if  (strcmp(name,"DSP_XMAX") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		dm_dspcoo( &r, NULL, &r1, NULL );
		r += r1;
		sprintf( str, "%e", r );
	} else if  (strcmp(name,"DSP_YMAX") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		dm_dspcoo( NULL, &r, NULL, &r1 );
		r += r1;
		sprintf( str, "%e", r );
	} else if  (strcmp(name,"BLANK") == 0)  {
		tr_fillstr( ' ', idxstr, maxlth, str, status );
	} else if  (strcmp(name,"EXCLAMATION") == 0)  {
		tr_fillstr( '!', idxstr, maxlth, str, status );
	} else if  (strcmp(name,"QUOTES") == 0)  {
		tr_fillstr( '"', idxstr, maxlth, str, status );
	} else if  (strcmp(name,"DOLLAR") == 0)  {
		tr_fillstr( '$', idxstr, maxlth, str, status );
	} else if  (strcmp(name,"PERCENT") == 0)  {
		tr_fillstr( '%', idxstr, maxlth, str, status );
	} else if  (strcmp(name,"HAT") == 0)  {
		tr_fillstr( '^', idxstr, maxlth, str, status );
	} else if  (strcmp(name,"BAR") == 0)  {
		tr_fillstr( '|', idxstr, maxlth, str, status );
	} else if  (strcmp(name,"SLASH") == 0)  {
		tr_fillstr( '/', idxstr, maxlth, str, status );
	} else if  (strcmp(name,"NUMBER") == 0)  {
		tr_fillstr( '#', idxstr, maxlth, str, status );
	} else if  (strcmp(name,"TITLESTYLE") == 0)  {
		sprintf( str, "%d", SHC_TITLESTYLE );
	} else if  (strcmp(name,"TRCINFOSTYLE") == 0)  {
		sprintf( str, "%d", SHC_TRCINFOSTYLE );
	} else if  (strcmp(name,"ZEROTRCSTYLE") == 0)  {
		sprintf( str, "%d", SHC_ZEROTRCSTYLE );
	} else if  (strcmp(name,"TIMEAXISSTYLE") == 0)  {
		sprintf( str, "%d", SHC_TIMEAXISSTYLE );
	} else if  (strcmp(name,"MARKSTYLE") == 0)  {
		sprintf( str, "%d", SHC_MARKSTYLE );
	} else if  (strcmp(name,"PMSTYLE") == 0)  {
		sprintf( str, "%d", SHC_PMSTYLE );
	} else if  (strncmp(name,"HEXCHAR",7) == 0)  {
		if  (strlen(name) != 9)  {   *status = TRE_HEXCHAR; return; }
		*numstr = name[7];
		numstr[1] = name[8];
		numstr[2] = '\0';
		if  (sscanf(numstr,"%x",&num) != 1)  { *status=TRE_HEXCHAR; return; }
		tr_fillstr( (char)num, idxstr, maxlth, str, status );
	} else if  (strcmp(name,"SH_ID") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		if  (maxlth < 10)  {*status = TRE_STROVFL; return;}
		strcpy( str, id_shv );
	} else if  (strcmp(name,"PI") == 0)  {
		if  (*idxstr != '\0')  {*status = TRE_ILIDX; return;}
		if  (maxlth < 23)  {*status = TRE_STROVFL; return;}
		strcpy( str, "3.14159265358979323846" );
	} else {
		*status = TRE_UKINTERN;
		err_setcontext( " ## internal var " ); err_setcontext( name );
		return;
	} /*endif*/

} /* end of tr_intern */



/*------------------------------------------------------------------------*/



static void tr_concat( char str[], int maxlth, STATUS *status )

/* concatenates parameters
 *
 * parameters of routine
 * char       str[];    modify; concatenated expression
 * int        maxlth;   input; maximum length of string
 * STATUS     *status;  output; return status
 */
{
	/* local variables */
	int      ccpos[MAXCONCAT];     /* positions of string elements */
	int      cccnt;                /* number of string elements */
	int      i;                    /* counter */
	char     ccres[CPC_LINELTH+1]; /* result of concatenation */
	int      ccspace;              /* remaining length of result string */
	char     *ccptr;               /* pointer to result string */
	int      lth;                  /* string length */

	/* executable code */

	cccnt = 0;
	i = 0;
	while  (str[i] != '\0')  {
		if  (str[i] == PCCONCAT)  {
			str[i] = '\0';
			if  (str[i+1] == '\0')  break;
			if  (cccnt == (MAXCONCAT-1))  {
				*status = TRE_CCOVFL;
				return;
			} /*endif*/
			ccpos[cccnt++] = i+1;
		} /*endif*/
		i++;
	} /*endwhile*/

	*ccres = '\0';
	ccptr = ccres;
	ccspace = (maxlth < CPC_LINELTH) ? maxlth : CPC_LINELTH;
	for  (i=0; i<cccnt; i++)  {
		if  (strlen(str+ccpos[i]) > ccspace)  {
			*status = TRE_STROVFL;
			return;
		} /*endif*/
		strcpy( ccptr, str+ccpos[i] );
		tr_translate( ccptr, ccspace, status );
		if  (*status == SHE_NOINFO)
			*status = TRE_NOERROR;
		if  (Severe(status))  return;
		lth = (int)strlen( ccptr );
		ccptr += lth;
		ccspace -= lth;
	} /*endfor*/

	strcpy( str, ccres );

} /* end of tr_concat */



/*------------------------------------------------------------------------*/



void tr_setruntrc( TRACE *trc, int idx )

/* sets running trace pointer */

{ runtrc_trv = trc; runidx_trv = idx; }



/*------------------------------------------------------------------------*/



static void tr_fillstr( char ch, char idxstr[], int maxlth, char str[],
	STATUS *status )

/* fills string with "idxstr" characters "ch"
 *
 * parameters of routine
 * char       ch;        input; fill character
 * char       idxstr[];  input; number (string) of fill chars
 * int        maxlth;    input; maximum length of output string
 * char       str[];     output; result string
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	int      chno;      /* number of fill chars */
	int      i;         /* counter */

	/* executable code */

	if  (*idxstr == '\0')  {
		*str = ch;
		str[1] = '\0';
		return;
	} /*endif*/

	if  (sscanf(idxstr,"%d",&chno) != 1)  {
		*status = TRE_CNVIDX;
		err_setcontext( " ## numstr " ); err_setcontext( idxstr );
		return;
	} /*endif*/

	if  (chno > maxlth)  {
		*status = TRE_STROVFL;
		return;
	} /*endif*/

	for  (i=0;i<chno; str[i++] = ch) {}
	str[chno] = '\0';

} /* end of tr_fillstr */



/*------------------------------------------------------------------------*/

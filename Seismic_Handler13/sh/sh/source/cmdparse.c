
/* File CMDPARSE.C
 *      ==========
 *
 * version 6, 22-May-2006
 *
 * Routines to parse and translate command lines
 * K. Stammler, 15-OCT-1989
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
#include "basecnst.h"
#include BC_SYSBASE
#include "erusrdef.h"
#include "cpusrdef.h"
#include "cperrors.h"
#include "uiusrdef.h"
#include BC_GCUSRDEF

/* global variables */
char      term_cpv[CPC_LINELTH] = " /;";  /* termination characters */
char      cmt_cpv = '!';                  /* comment character */

/* prototypes of local routines */
BOOLEAN is_term( char ch );

/*------------------------------------------------------------------------*/



void cp_parse( char line[], PARAM *par, STATUS *status )

/* parses a line
 *
 * parameters of routine
 * char     line[];   input; line to be parsed
 * PARAM    *par;     output; parsed parameters
 * STATUS   *status;  output; return status
 */
{
	/* local constants */
#   define BLANK_TERM 0
#   define QUAL_TERM 1

	/* local variables */
	int      i;           /* counter */
	int      line_s;      /* start character position of line */
	int      line_e;      /* end character position of line */
	char     last_term;   /* last termination character */
	char     *currch;     /* pointer to current copy position */
	char     *currch_end; /* pointer to last element of string */
	char     *linech;     /* pointer to current line position */
	int      in_word;     /* word in process (parse mode) */ 
	char     blank;       /* blank character */
	int      isqual;      /* term character is qualifer separator */

	/* executable code */

	blank = term_cpv[BLANK_TERM];

	/* initialize output variables */
	*status = CPE_NOERROR;
	par->pno = -1;
	for  (i=0;i<CPC_MAXPARAM;i++)  {
		par->p[i][0] = '\0';
	} /*endfor*/
	par->qno = -1;
	for  (i=0;i<CPC_MAXQUAL;i++)  {
		par->q[i][0] = '\0';
	} /*endfor*/
	par->query = FALSE;
	par->qchecked = 0;

	/* determine start & end positions of line */
	line_e = 0;
	linech = line;
	while  ((*linech != '\0') && (*linech != cmt_cpv))  {
		line_e++;
		linech++;
	} /*endwhile*/
	/* line_e = (int)strlen( line ) - 1; */
	if  (--line_e == -1)  return;
	for  (line_s = 0; (line[line_s] == ' ') || (line[line_s] == '\t');
		line_s++)  ;
	if  (line_s > line_e)  return;

	/* setup loop variables */
	isqual = FALSE;
	last_term = blank;
	in_word = TRUE;
	currch = par->p[++(par->pno)];
	currch_end = currch + CPC_LINELTH;

	/* loop over input characters */
	for  (linech=line+line_s;linech<=line+line_e;linech++)  {
		if  (in_word)  {
			if  (is_term(*linech))  {            /* current word ends */
				in_word = FALSE;                  /* toggle parse mode */
				last_term = *linech;              /* store term character */
				if  (currch > currch_end)  {*status = CPE_STROVFL; return;}
				*currch = '\0';                   /* terminate current string */
				isqual = (*linech == term_cpv[QUAL_TERM]); /* next is qual. ? */
			} else {
				if  (currch > currch_end)  {*status = CPE_STROVFL; return;}
				*currch++ = *linech;             /* copy char to current word */
			} /*endif*/
		} else {
			if  (is_term(*linech))  {  /* more than one consec. term char */
				if  (*linech != blank)  {        /* significant terminator */
					if  (last_term != blank)  {   /* empty parameter/qualif.*/
						if  (isqual)  {                   /* empty qualifier */
							if  (par->qno == CPC_MAXQUAL-1)  {
								*status = CPE_QOVFL;       /* too many qualifiers */
								return;
							} /*endif*/
							(par->qno)++;           /* increment qualif. counter */
						} else {                                 /* empty parameter */
							if  (par->pno == CPC_MAXPARAM-1)  {
								*status = CPE_POVFL;       /* too many parameters */
								return;
							} /*endif*/
							(par->pno)++;         /* increment parameter counter */
						} /*endif*/
					} /*endif*/
					isqual = (*linech == term_cpv[QUAL_TERM]); /* is next qual?*/
				} /*endif*/
				last_term = *linech;                       /* store term char */
			} else {                                           /* start new word */
				if  (isqual)  {                           /* new qualifier */
					if  (par->qno == CPC_MAXQUAL-1)  {
						*status = CPE_QOVFL;             /* too many qualifiers */
						return;
					} /*endif*/
					currch = par->q[++(par->qno)];  /* initialize copy pointer */
					currch_end = currch + CPC_LINELTH;
				} else {
					if  (par->pno == CPC_MAXPARAM-1)  {
						*status = CPE_POVFL;             /* too many parameters */
						return;
					} /*endif*/
					currch = par->p[++(par->pno)];  /* initialize copy pointer */
					currch_end = currch + CPC_LINELTH;
				} /*endif*/
				in_word = TRUE;                          /* toggle parse mode */
				if  (currch > currch_end)  {*status = CPE_STROVFL; return;}
				*currch++ = *linech;                       /* take first char */
			} /*endif*/
		} /*endif*/
	} /*endfor*/

	/* set check flags */
	for  (i=0; i<=par->qno; i++)
		par->qchecked |= (1 << i);

	if  (currch > currch_end)  {*status = CPE_STROVFL; return;}
	*currch = '\0';                                 /* terminate last word */
	par->query = (par->pno == 0);

}   /* end of cp_parse */



/*------------------------------------------------------------------------*/



void cp_terminators( char term[], STATUS *status )

/* changes terminators for line parsing.
 * term contains all termination characters which should be used
 * *term[0] contains the blank terminator (usually SPACE).
 *    multiple blank terminators are treated as a single terminator.
 * *term[1] contains qualifier terminator.
 *    qualifiers are treated separately from parameters.
 *
 * Default term string is " /;".
 *
 * parameters of routine
 * char     term[];   input; pointer to termination characters
 * STATUS   *status;  output; return status
 */
{
	/* executable code */

	if  (strlen(term) < 2)  {
		*status = CPE_NOQUALTRM;
		return;
	} else if  (strlen(term) >= CPC_LINELTH)  {
		*status = CPE_TOOMNYTRM;
		return;
	} /*endif*/

	strcpy( term_cpv, term );

}   /* end of cp_terminators */



/*------------------------------------------------------------------------*/



void cp_commentchar( char cmt )

/* sets new comment character
 *
 * parameter of routine
 * char      cmt;       input; comment character
 */
{
	/* executable code */

	cmt_cpv = cmt;

} /* end of cp_commentchar */



/*------------------------------------------------------------------------*/


int cp_pnexc( PARAM *par, int max, STATUS *status )

/* checks whether the maximum number of parameters is exceeded
 *
 * parameters of routine
 * PARAM    *par;           input; cmd parameters
 * int      max;            input; maximum number permitted
 * STATUS   *status;        ouput; return status
 */
{
	/* executable code */

	if  (par->pno > max)  *status = CPE_PNUMEXC;

	return (*status != CPE_NOERROR);

}   /* end of cp_pnexc */



/*------------------------------------------------------------------------*/



int cp_pnum( PARAM *par )

/* returns number of parameters passed
 *
 * parameters of routine
 * PARAM    *par;   input; menu parameter
 */
{
	/* executable code */

	return (par->pno);

}   /* end of cp_pnum */



/*------------------------------------------------------------------------*/



BOOLEAN cp_cmdverb( PARAM *par, char verb[] )

/* checks whether "par" contains the command verb "verb"
 *
 * parameters of routine
 * PARAM      *par;      input; command to be checked
 * char       verb[];    input; check verb
 *                       returns TRUE if command-verb equals "verb"
 */
{
	/* executable code */
	return  (strcmp(par->p[0],verb) == 0);

}   /* end of cp_cmdverb */



/*------------------------------------------------------------------------*/



void cp_getfloat( PARAM *par, int pos, int wdw, char *prompt,
	float *num, STATUS *status )

/* gets float parameter from parameter block par at position pos
 *
 * parameters of routine
 * PARAM    *par;           input; menu parameter
 * int      pos;            input; position of parameter
 * int      wdw;            input; window number
 * char     *prompt;        input; prompt string on parameter request
 * float    *num;           output; result
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((pos < 0) || (pos >= CPC_MAXPARAM))  {
		*status = CPE_PNOTEX;
		return;
	} /*endif*/

	if  (par->query)  {  /* read string */
		gc_write( wdw, prompt );
		ui_read( wdw, par->p[pos], CPC_LINELTH, status );
		if  (*status != CPE_NOERROR)  return;
	} else if  (strcmp(par->p[pos],"") == 0)  {  /* read string */
		*status = CPE_CNVFLOAT;
		return;
	} /*endif*/

	if  (sscanf( par->p[pos], "%f", num ) != 1)  {
		*status = CPE_CNVFLOAT;
		err_setcontext( " ## string " ); err_setcontext( par->p[pos] );
	} /*endif*/

}   /* end of cp_getfloat */



/*------------------------------------------------------------------------*/



void cp_getint( PARAM *par, int pos, int wdw, char prompt[],
	int *num, STATUS *status )

/* gets integer parameter from parameter block par at position pos
 *
 * parameters of routine
 * PARAM    *par;           input; menu parameter
 * int      pos;            input; position of parameter
 * int      wdw;            input; window number
 * char     prompt[];       input; prompt string on parameter request
 * int      *num;           output; result
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((pos < 0) || (pos >= CPC_MAXPARAM))  {
		*status = CPE_PNOTEX;
		return;
	} /*endif*/

	if  (par->query)  {  /* read string */
		gc_write( wdw, prompt );
		ui_read( wdw, par->p[pos], CPC_LINELTH, status );
		if  (*status != CPE_NOERROR)  return;
	} else if  (strcmp(par->p[pos],"") == 0)  {
		*status = CPE_CNVINT;
		return;
	} /*endif*/

	if  (sscanf( par->p[pos], "%d", num ) != 1)  {
		*status = CPE_CNVINT;
		err_setcontext( " ## string " ); err_setcontext( par->p[pos] );
	} /*endif*/

}   /* end of cp_getint */



/*------------------------------------------------------------------------*/



void cp_getlong( PARAM *par, int pos, int wdw, char prompt[],
	long *num, STATUS *status )

/* gets long parameter from parameter block par at position pos
 *
 * parameters of routine
 * PARAM    *par;           input; menu parameter
 * int      pos;            input; position of parameter
 * int      wdw;            input; window number
 * char     prompt[];       input; prompt string on parameter request
 * long     *num;           output; result
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((pos < 0) || (pos >= CPC_MAXPARAM))  {
		*status = CPE_PNOTEX;
		return;
	} /*endif*/

	if  (par->query)  {  /* read string */
		gc_write( wdw, prompt );
		ui_read( wdw, par->p[pos], CPC_LINELTH, status );
		if  (*status != CPE_NOERROR)  return;
	} else if  (strcmp(par->p[pos],"") == 0)  {
		*status = CPE_CNVINT;
		return;
	} /*endif*/

	if  (sscanf( par->p[pos], "%ld", num ) != 1)  {
		*status = CPE_CNVINT;
		err_setcontext( " ## string " ); err_setcontext( par->p[pos] );
	} /*endif*/

}   /* end of cp_getlong */



/*------------------------------------------------------------------------*/



void cp_getstr( PARAM *par, int pos, int wdw, char prompt[],
	int maxlth, char str[], STATUS *status )

/* gets string parameter from parameter block par at position pos
 *
 * parameters of routine
 * PARAM    *par;           input; menu parameter
 * int      pos;            input; position of parameter
 * int      wdw;            input; window number
 * char     prompt[];       input; prompt string on parameter request
 * int      maxlth;         input; maximum length of output string
 * char     str[];          output; result
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((pos < 0) || (pos >= CPC_MAXPARAM))  {
		*status = CPE_PNOTEX;
		return;
	} /*endif*/

	if  (par->query)  {  /* read string */
		gc_write( wdw, prompt );
		ui_read( wdw, str, maxlth, status );
	} else {
		if  (strlen(par->p[pos]) > maxlth)  {
			*status = CPE_STROVFL;
			return;
		} /*endif*/
		strcpy( str, par->p[pos] );
	} /*endif*/

}   /* end of cp_getstr */



/*------------------------------------------------------------------------*/



BOOLEAN cp_pentered( PARAM *par, int pos, STATUS *status )

/* checks whether parameter number "pos" was entered or not
 *
 * parameters of routine
 * PARAM    *par;           input; menu parameter
 * int      pos;            input; position of parameter
 * STATUS   *status;        output; return status
 */
{
	/* executable code */

	if  ((pos < 0) || (pos >= CPC_MAXPARAM))  {
		*status = CPE_PNOTEX;
		return FALSE;
	} /*endif*/

	return (par->p[pos][0] != '\0');

} /* end of cp_pentered */



/*------------------------------------------------------------------------*/



void cp_verify( PARAM *par, int maxlth, char str[], STATUS *status )

/* returns string containing command, qualifiers & parameters
 *
 * parameters of routine
 * PARAM    *par;           input; parameter block
 * int      maxlth;         input; maximum length of output string
 * char     str[];          output; command verify string
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      i;        /* counter */
	char     blank[2]; /* blank string */
	char     qual[2];  /* qualifier separator */
	int      totlth;   /* total string length */

	/* executable code */

	*str = '\0';
	if  (par->pno == -1)  return;

	*blank = *term_cpv;
	blank[1] = '\0';
	*qual = term_cpv[1];
	qual[1] = '\0';
	strcpy( str, par->p[0] );
	totlth = (int)strlen( str );

	for  (i=0;i<=par->qno;i++)  {
		totlth += (int)strlen( par->q[i] ) + 1;
		if  (totlth > maxlth)  {
			*status = CPE_VFOVFL;
			return;
		} /*endif*/
		strcat( str, qual );
		strcat( str, par->q[i] );
	} /*endfor*/
	for  (i=1;i<=par->pno;i++)  {
		totlth += (int)strlen( par->p[i] ) + 1;
		if  (totlth > maxlth)  {
			*status = CPE_VFOVFL;
			return;
		} /*endif*/
		strcat( str, blank );
		strcat( str, par->p[i] );
	} /*endfor*/

} /* end of cp_verify */



/*------------------------------------------------------------------------*/



BOOLEAN cp_qexist( PARAM *par, char qname[] )

/* returns whether qualifier qname is entered or not
 *
 * parameters of routine
 * PARAM    *par;           input; parameter block
 * char     qname[];        input; qualifier name to be searched
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	for  (i=0;i<=par->qno;i++)
		if  (strcmp(qname,par->q[i]) == 0)  {
			par->qchecked &= ~(1 << i);
			return TRUE;
		} /*endif*/

	return FALSE;

}  /* end of cp_qexist */



/*------------------------------------------------------------------------*/



BOOLEAN cp_qstr( PARAM *par, char qname[], int maxlth, char value[],
	STATUS *status )

/* gets value of qualifier "qname", returns whether qualifier was found
 *
 * parameters of routine
 * PARAM      *par;      input; command parameter
 * char       qname[];   input; name of qualifier
 * int        maxlth;    input; maximum length of output string
 * char       value[];   output; value of qualifier
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	int      i;        /* counter */
	int      qlth;     /* length of qualifier name */
	char     findstr[CPC_LINELTH+1];  /* find string */

	/* executable code */

	qlth = (int)strlen( qname ) + 1;
	if  (qlth > CPC_LINELTH)  { *status = CPE_STROVFL; return FALSE; }
	strcpy( findstr, qname );
	strcat( findstr, "=" );
	for  (i=0;i<=par->qno;i++)  {
		if  (strncmp(par->q[i],findstr,qlth) == 0)  {
			if  (strlen((par->q[i])+qlth) > maxlth)  {
				*value = '\0';
				*status = CPE_STROVFL;
				return FALSE;
			} /*endif*/
			strcpy( value, (par->q[i])+qlth );
			par->qchecked &= ~(1 << i);
			return TRUE;
		} /*endif*/
	} /*endfor*/

	*value = '\0';
	return cp_qexist(par,qname);

} /* end of cp_qstr */



/*------------------------------------------------------------------------*/



char *cp_uncheckedqual( PARAM *par )

/* returns pointer to unchecked qualifier or NULL
 *
 * parameters of routine
 * PARAM      *par;       input; parameter block
 *                        returns first unchecked qualifier or NULL
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	if  (par->qchecked == 0)
		return NULL;

	for  (i=0; i<=par->qno; i++)
		if  ((1 << i) & par->qchecked)
			return par->q[i];

	return NULL;

} /* end of cp_uncheckedqual */



/*------------------------------------------------------------------------*/
/*                             local routines                             */
/*------------------------------------------------------------------------*/



BOOLEAN is_term( char ch )

/* checks whether ch is termination character
 *
 * parameters of routine
 * char     ch;    input; character to be checked
 */
{
	/* local variables */
	char     *chp;      /* character pointer */

	/* executable code */

	chp = term_cpv;
	while  (*chp != '\0')  {
		if  (*chp++ == ch)  return (TRUE);
	} /*endwhile*/

	return (FALSE);

} /* end of int is_term */



/*------------------------------------------------------------------------*/


/* file CPUSRDEF.H
 *      ==========
 *
 * version 7, 01-May-2007
 *
 * prototypes and types of module CMDPARSE.C
 * K. Stammler, 22-MAY-91
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1996,  Klaus Stammler, Federal Institute for Geosciences
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


#ifndef __CPUSRDEF
#define __CPUSRDEF

#define CPC_MAXPARAM 16
#define CPC_MAXQUAL  5
#define CPC_LINELTH  2048

typedef struct param {
   int    pno;                            /* number of parameters */
   char   p[CPC_MAXPARAM][CPC_LINELTH+1]; /* list of parameters */
   int    qno;                            /* number of qualifiers */
   char   q[CPC_MAXQUAL][CPC_LINELTH+1];  /* list of qualifiers */
   int    query;                          /* request parameters */
	int    qchecked;                       /* check flags of qulifiers */
} PARAM;

#define cp_qnum(p) ((p)->qno+1)


/*------------------------------------------------------------------------*/


void cp_parse( char line[], PARAM *par, STATUS *status );

/* parses a line
 *
 * parameters of routine
 * char     line[];   input; line to be parsed
 * PARAM    *par;     output; parsed parameters
 * STATUS   *status;  output; return status
 */


/*------------------------------------------------------------------------*/


void cp_terminators( char term[], STATUS *status );

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


/*------------------------------------------------------------------------*/


void cp_commentchar( char cmt );

/* sets new comment character
 *
 * parameter of routine
 * char      cmt;       input; comment character
 */


/*------------------------------------------------------------------------*/


int cp_pnexc( PARAM *par, int max, STATUS *status );

/* checks whether the maximum number of parameters is exceeded
 *
 * parameters of routine
 * PARAM    *par;           input; cmd parameters
 * int      max;            input; maximum number permitted
 * STATUS   *status;        ouput; return status
 */


/*------------------------------------------------------------------------*/


int cp_pnum( PARAM *par );

/* returns number of parameters passed
 *
 * parameters of routine
 * PARAM    *par;   input; menu parameter
 */


/*------------------------------------------------------------------------*/


BOOLEAN cp_cmdverb( PARAM *par, char verb[] );

/* checks whether "par" contains the command verb "verb"
 *
 * parameters of routine
 * PARAM      *par;      input; command to be checked
 * char       verb[];    input; check verb
 *                       returns TRUE if command-verb equals "verb"
 */


/*------------------------------------------------------------------------*/


void cp_getfloat( PARAM *par, int pos, int wdw, char *prompt,
	float *num, STATUS *status );

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


/*------------------------------------------------------------------------*/


void cp_getint( PARAM *par, int pos, int wdw, char prompt[],
	int *num, STATUS *status );

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


/*------------------------------------------------------------------------*/



void cp_getlong( PARAM *par, int pos, int wdw, char prompt[],
	long *num, STATUS *status );

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


/*------------------------------------------------------------------------*/


void cp_getstr( PARAM *par, int pos, int wdw, char prompt[],
	int maxlth, char str[], STATUS *status );

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


/*------------------------------------------------------------------------*/


BOOLEAN cp_pentered( PARAM *par, int pos, STATUS *status );

/* checks whether parameter number "pos" was entered or not
 *
 * parameters of routine
 * PARAM    *par;           input; menu parameter
 * int      pos;            input; position of parameter
 * STATUS   *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void cp_verify( PARAM *par, int maxlth, char str[], STATUS *status );

/* returns string containing command, qualifiers & parameters
 *
 * parameters of routine
 * PARAM    *par;           input; parameter block
 * int      maxlth;         input; maximum length of output string
 * char     str[];          output; command verify string
 * STATUS   *status;        output; return status
 */


/*------------------------------------------------------------------------*/


BOOLEAN cp_qexist( PARAM *par, char qname[] );

/* returns whether qualifier qname is entered or not
 *
 * parameters of routine
 * PARAM    *par;           input; parameter block
 * char     qname[];        input; qualifier name to be searched
 */


/*------------------------------------------------------------------------*/


BOOLEAN cp_qstr( PARAM *par, char qname[], int maxlth, char value[],
	STATUS *status );

/* gets value of qualifier "qname", returns whether qualifier was found
 *
 * parameters of routine
 * PARAM      *par;      input; command parameter
 * char       qname[];   input; name of qualifier
 * int        maxlth;    input; maximum length of output string
 * char       value[];   output; value of qualifier
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


char *cp_uncheckedqual( PARAM *par );

/* returns pointer to unchecked qualifier or NULL
 *
 * parameters of routine
 * PARAM      *par;       input; parameter block
 *                        returns first unchecked qualifier or NULL
 */


/*------------------------------------------------------------------------*/


#endif /* __CPUSRDEF */

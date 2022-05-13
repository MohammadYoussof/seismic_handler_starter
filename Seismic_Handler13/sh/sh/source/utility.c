
/* File UTILITY.C
 *      =========
 *
 * version 6, 22-May-2006
 *
 * some useful basic routines
 * K. Stammler, 27-MAR-1990
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
#include <ctype.h>
#include <string.h>
#include "basecnst.h"
#include BC_SYSBASE
#include "uterrors.h"
#include "utusrdef.h"



/*------------------------------------------------------------------------*/



#ifdef CLASSIC

void ut_ilist( liststr, maxlth, list, listlth, status )

/* converts the list string "liststr" to a list of integers.  The numbers
	must be separated by commas "," or hyphens "-" (for contiguous blocks
	of numbers). */

/* parameters of routine */
char     *liststr;       /* input; list string */
int      maxlth;         /* input; maximum length of list */
int      *list;          /* output; list of integer */
int      *listlth;       /* output; length of list */
int      *status;        /* output; return status */

#else

void ut_ilist( char *liststr, int maxlth, int *list, int *listlth,
					int *status )

#endif

{
	/* local constants */
#  define MAXNUMLTH 15

	/* local variables */
	char     numstr[MAXNUMLTH+1];    /* conversion string */
	char     *chptr;                 /* current write copy pos */
	int      lastnum;                /* previous read number */
	int      currnum;                /* current number */
	int      dash;                   /* dash separator flag */
	int      i;                      /* counter */

	/* executable code */

	*listlth = 0;
	lastnum = 0;
	dash = FALSE;

	while  (*liststr != '\0')  {

		/* skip leading blanks */
		while  (*liststr == ' ')  {
			if  (*(++liststr) == '\0')  return;
		} /*endwhile*/

		/* copy next number to numstr */
		chptr = numstr;
		while  ((*liststr != '\0')  &&  isdigit(*liststr))  {
			*chptr++ = *liststr++;
			if  (chptr > (numstr+MAXNUMLTH))  {
				*status = UTE_ILNUMLTH;
				return;
			} /*endif*/
		} /*endwhile*/
		*chptr = '\0';

		/* convert numstr to integer */
		if  (chptr > numstr)  {
			if  (sscanf( numstr, "%d", &currnum )  !=  1)  {
				*status = UTE_ILCNV;
				return;
			} /*endif*/
		} else {
			*status = UTE_ILMISS;
			return;
		} /*endif*/

		/* insert in integer list */
		if  (dash)  {
			if  (*listlth > 0)  lastnum = list[(*listlth)-1];
			if  ((*listlth)+Abs(currnum-lastnum) > maxlth)  {
				*status = UTE_ILOVFL;
				return;
			} /*endif*/
			if  (currnum < lastnum)  {
				for  (i=lastnum-1;i>=currnum;list[(*listlth)++]=i--) {}
			} else {
				for  (i=lastnum+1;i<=currnum;list[(*listlth)++]=i++) {}
			} /*endif*/
		} else {
			if  (*listlth >= maxlth)  {
				*status = UTE_ILOVFL;
				return;
			} /*endif*/
			list[(*listlth)++] = currnum;
		} /*endif*/

		/* return if finished */
		if  (*liststr == '\0')  return;

		/* read separator */
		if  ((*liststr != ' ') && (*liststr != ',') && (*liststr != '-'))  {
			*status = UTE_ILILLCH;
			return;
		} /*endif*/
		dash = (*liststr++ == '-');
		if  (*liststr == '\0')  {
			*status = UTE_ILEND;
			return;
		} /*endif*/

	} /*endwhile*/

} /* end of ut_ilist */



/*------------------------------------------------------------------------*/



#ifdef CLASSIC

void ut_nlist( liststr, seps, maxlth, list, listlth, status )

/* converts the list string "liststr" to a list of integers.  The numbers
	must be separated by separators listed in "seps" */

/* parameters of routine */
char     *liststr;       /* input; list string */
char     *seps;          /* input; permitted separators */
int      maxlth;         /* input; maximum length of string */
int      *list;          /* output; list of integer */
int      *listlth;       /* output; length of list */
int      *status;        /* output; return status */

#else

void ut_nlist( char *liststr, char *seps, int maxlth, int *list,
					int *listlth, int *status )

#endif

{
	/* local constants */
#  define MAXNUMLTH 15

	/* local variables */
	char     numstr[MAXNUMLTH+1];    /* conversion string */
	char     *chptr;                 /* current write copy pos */
	int      currnum;                /* current number */

	/* executable code */

	*listlth = 0;

	while  (*liststr != '\0')  {

		/* skip leading blanks */
		while  (*liststr == ' ')  {
			if  (*(++liststr) == '\0')  return;
		} /*endwhile*/

		/* copy next number to numstr */
		chptr = numstr;
		while  ((*liststr != '\0')  &&  isdigit(*liststr))  {
			*chptr++ = *liststr++;
			if  (chptr > (numstr+MAXNUMLTH))  {
				*status = UTE_ILNUMLTH;
				return;
			} /*endif*/
		} /*endwhile*/
		*chptr = '\0';

		/* convert numstr to integer */
		if  (chptr > numstr)  {
			if  (sscanf( numstr, "%d", &currnum )  !=  1)  {
				*status = UTE_ILCNV;
				return;
			} /*endif*/
		} else {
			*status = UTE_ILMISS;
			return;
		} /*endif*/

		/* insert in integer list */
		if  (*listlth >= maxlth)  {
			*status = UTE_ILOVFL;
			return;
		} /*endif*/
		list[(*listlth)++] = currnum;

		/* return if finished */
		if  (*liststr == '\0')  return;

		/* read separator */
		if  (!ut_chinstr(seps,*liststr))  {
			*status = UTE_ILILLCH;
			return;
		} /*endif*/

		if  (*(++liststr) == '\0')  {
			*status = UTE_ILEND;
			return;
		} /*endif*/

	} /*endwhile*/

} /* end of ut_nlist */



/*------------------------------------------------------------------------*/



#ifdef CLASSIC

int ut_chinstr( str, ch )

/* checks whether "ch" is in "str" or not */

/* parameters of routine */
char     *str;           /* input; search string */
char     ch;             /* input; char to look for */

#else

int ut_chinstr( char *str, char ch )

#endif

{
	/* executable code */

	while  (*str != '\0')
		if  (*str++ == ch)  return TRUE;

	return FALSE;

} /* end of ut_chinstr */



/*------------------------------------------------------------------------*/



void ut_cap( char *str )

/* capitalizes str
 *
 * parameters of routine
 * char       *str;           modify; string to be capitalized
 */
{
	/* executable code */
	
	if  (*str == '@')  {  /* do not capitalize */
		*str = ' ';
		return;
	} /*endif*/

	while  (*str != '\0')  {
		*str = Cap( *str );
		str++;
	} /*endwhile*/
	return;

} /* end of ut_cap */



/*------------------------------------------------------------------------*/



void ut_uncap( char *str )

/* uncapitalizes str
 *
 * parameters of routine
 * char       *str;           modify; string to be uncapitalized
 */
{
	/* executable code */
	
	while  (*str != '\0')  {
		*str = Uncap( *str );
		str++;
	} /*endwhile*/
	return;

} /* end of ut_uncap */



/*------------------------------------------------------------------------*/



void ut_defext( char *file, int maxlth, char *defext, int *status )

/* appends default extension "defext" to filename "file", if there
 * is no extension specified
 *
 * parameters of routine
 * char       *file;    modify; filename where to check extension
 * int        maxlth;   input; maximum length of filename
 * char       *defext;  input; default extension to append (incl ".")
 * int        *status;  output; return status
 */
{
	/* local variables */
	char     *c;       /* search pointer */
	char     lastp;    /* last non-letter char */

	/* executable code */

	lastp = ' ';
	c = file;
	while  (*c != '\0')  {
		if  (ispunct(*c) && (*c != '-') && (*c != '_'))  lastp= *c;
		c++;
	} /*endwhile*/

	if  (lastp != '.')  {  /* no extension specified */
		if  ((strlen(file)+strlen(defext)) > maxlth)  {
			*status = UTE_STROVFL;
			return;
		} /*endif*/
		strcat( file, defext );
	} /*endif*/

} /* end of ut_defext */



/*-------------------------------------------------------------------------*/



long ut_next2pow( long l )

/* returns next power of 2
 *
 * parameter of routine
 * long      l;      input; input number
 */
{
	/* local variables */
	long     next;     /* next power of 2 */

	/* executable code */

	next = 1;
	while  (next < l)
		next *= 2;

	return next;

} /* end of ut_next2pow */



/*------------------------------------------------------------------------*/



void ut_replacechar( char str[], char fch, char rch )

/* replaces char fch by char rch in string str
 *
 * parameters of routine
 * char       str[];     modify; string to replace fch by rch
 * char       fch;       input; find character
 * char       rch;       input; replace character
 */
{
	/* executable code */
	
	while  (*str != '\0')  {
		if  (*str == fch)  *str = rch;
		str++;
	} /*endwhile*/

} /* end of ut_replacechar */



/*------------------------------------------------------------------------*/


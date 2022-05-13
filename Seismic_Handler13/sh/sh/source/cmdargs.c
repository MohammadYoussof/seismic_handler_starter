
/* File CMDARGS.C
 *      =========
 *
 * version 5, 10-SEP-91
 *
 * access to command line parameters
 * K. Stammler, 3-MAY-1990
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

#define MAXLISTLTH 5
		  /* maximum length of item list per argument */

/* prototypes of local routines */
static void ca_strupcs( char str[] );


/*------------------------------------------------------------------------*/



void ca_prepare( int argnum, char *arg[] )

/* prepares arguments for processing
 *
 * parameters of routine
 * int      argnum;         input; number of arguments
 * char     *arg[];         modify; arguments
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	for  (i=0;i<argnum;i++)  {
		if  (*(arg[i]) == '/')  (arg[i])++;
		ca_strupcs( arg[i] );
	} /*endfor*/

} /* end of ca_prepare */



/*------------------------------------------------------------------------*/



void ca_qint( int argnum, char *arg[], char name[], int cnt,
	int *ires, int *cntread )

/* get "cnt" integers or less from parameter
 *
 * parameters of routine
 * int      argnum;         input; number of arguments
 * char     *arg[];         input; arguments passed to program
 * char     name[];         input; name of integer field
 * int      cnt;            input; number of integers to read
 * int      *ires;          output; integers read from parameter
 * int      *cntread;       output; return status
 */
{
	/* local variables */
	int      i, j;             /* counters */
	int      iarr[MAXLISTLTH]; /* numbers read */
	static char ifmt[MAXLISTLTH][MAXLISTLTH*3+1] =
		{"=%d","=%d,%d","=%d,%d,%d","=%d,%d,%d,%d","=%d,%d,%d,%d,%d"};
		/* format strings for conversion */

	/* executable code */

	*cntread = 0;
	if  (cnt <= 0)  return;
	if  (cnt > MAXLISTLTH)  cnt = MAXLISTLTH;

	for  (i=0;i<argnum;i++)  {
		if  (strncmp(arg[i],name,strlen(name)) == 0)  {
			*cntread = sscanf( arg[i]+strlen(name), ifmt[cnt-1],
				iarr, iarr+1, iarr+2, iarr+3, iarr+4, iarr+5 );
			if  (*cntread > cnt)  *cntread = cnt;
			for  (j=0;j<(*cntread);j++)  ires[j] = iarr[j];
			return;
		} /*endif*/
	} /*endfor*/

	*cntread = 0;

} /* end of ca_qint */



/*------------------------------------------------------------------------*/



void ca_qreal( int argnum, char *arg[], char name[], int cnt,
	float *rres, int *cntread )

/* get "cnt" reals or less from parameter
 *
 * parameters of routine
 * int      argnum;         input; number of arguments
 * char     *arg[];         input; arguments passed to program
 * char     name[];         input; name of integer field
 * int      cnt;            input; number of integers to read
 * float    *rres;          output; reals read from parameter
 * int      *cntread;       output; return status
 */
{
	/* local variables */
	int      i, j;             /* counters */
	float    rarr[MAXLISTLTH]; /* numbers read */
	static char rfmt[MAXLISTLTH][MAXLISTLTH*3+1] = 
		{ "=%f", "=%f,%f", "=%f,%f,%f", "=%f,%f,%f,%f", "=%f,%f,%f,%f,%f" };
		/* format strings for conversion */

	/* executable code */

	*cntread = 0;
	if  (cnt <= 0)  return;
	if  (cnt > MAXLISTLTH)  cnt = MAXLISTLTH;

	for  (i=0;i<argnum;i++)  {
		if  (strncmp(arg[i],name,strlen(name)) == 0)  {
			*cntread = sscanf( arg[i]+strlen(name), rfmt[cnt-1],
				rarr, rarr+1, rarr+2, rarr+3, rarr+4, rarr+5 );
			if  (*cntread > cnt)  *cntread = cnt;
			for  (j=0;j<(*cntread);j++)  rres[j] = rarr[j];
			return;
		} /*endif*/
	} /*endfor*/

	*cntread = 0;

} /* end of ca_qreal */



/*------------------------------------------------------------------------*/



void ca_qstr( int argnum, char *arg[], char name[], char str[] )

/* get string from parameter
 *
 * parameters of routine
 * int      argnum;        input; number of arguments
 * char     *arg[];        input; arguments passed to program
 * char     name[];        input; name of integer field
 * char     str[];         output; string read from parameter
 */
{
	/* local constants */
#  define MAXCNT 6

	/* local variables */
	int      i, j;         /* counters */

	/* executable code */

	*str = '\0';
	if  (*name == '\0')  {   /* find first argument without "=" */
		for  (i=1;i<argnum;i++)  {
			if  (arg[i][0] != '\0')  {
				j = 0;
				while  (arg[i][j] != '\0'  &&  arg[i][j] != '=')
					j++;
				if  (j == (int)strlen(arg[i]))  {  /* found it */
					strcpy( str, arg[i] );
					return;
				} /*endif*/
			} /*endif*/
		} /*endfor*/
	} else {
		for  (i=0;i<argnum;i++)  {
			if  (strncmp(arg[i],name,strlen(name)) == 0)  {
				strcpy( str, arg[i]+strlen(name) );
				return;
			} /*endif*/
		} /*endfor*/
	} /*endif*/

} /* end of ca_qstr */



/*------------------------------------------------------------------------*/



static void ca_strupcs( char str[] )

/* converts all characters in str to uppercase
 *
 * parameters of routine
 * char     str[];    modify; string to convert
 */
{

	/* executable code */

	for  (;*str != '\0';str++)  {
		if  (islower(*str))  *str = Cap( *str );
	} /*endfor*/

} /* end of ca_strupcs */



/*------------------------------------------------------------------------*/

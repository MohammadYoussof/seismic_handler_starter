
/* file CPAR.C
 *      ======
 *
 * version 5, 22-May-2006
 *
 * handles command line parameters of C
 * K. Stammler, 9-May-92
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
#include "cpar.h"


#ifdef BC_SUN
#define QUALCHAR1 '-'
#define QUALCHAR2 '-'
#else
#define QUALCHAR1 '-'
#define QUALCHAR2 '/'
#endif



/* global variables */
static char    pav_pname[BC_FILELTH+1]="PA:undefined"; /* name of program */
static int     pa_parnum;             /* number of parameters */
static int     pa_qualnum;            /* number of qualifers */
static char    *pa_par[PAC_MAXPAR];   /* pointers to parameters */
static char    *pa_qual[PAC_MAXQUAL]; /* pointers to qualifers */
static long    pa_qmap;               /* qualifier map */
                                      /* parameters and qualifiers */
                                      /* are numbered from 1 to N */


#ifdef XXXYYY
int main( int argc, char *argv[] )
{
	/* local variables */
	int      i;        /* counter */
	char     *ptr;     /* string pointer */
	char     str[BC_LINELTH+1];

	/* executable code */

	pa_init( argc, argv );

	printf( "parameters\n" );
	for  (i=1; i<=pa_pnumber(); i++)
		printf( "%2d: %s\n", i, pa_pvalue(i) );

	printf( "\n%d qualifiers\n", pa_qnumber() );

	if  (pa_qspecified("-x"))  {
		ptr = pa_qvalue( "-x" );
		if  (ptr == NULL)  {
			printf( "\n   -x (no value)\n" );
		} else {
			printf( "\n   -x value >%s<\n", ptr );
		} /*endif*/
	} else {
		printf( "\n   -x not specified\n" );
	} /*endif*/

	if  (pa_qspecified("-test"))  {
		ptr = pa_qvalue( "-test" );
		if  (ptr == NULL)  {
			printf( "\n   -test (no value)\n" );
		} else {
			printf( "\n   -test value >%s<\n", ptr );
		} /*endif*/
	} else {
		printf( "\n   -test not specified\n" );
	} /*endif*/

	if  (pa_qunchecked(str) > 0)
		printf( "\n   %d unchecked qualifers.   first: %s\n",
			pa_qunchecked(str), str );

	return 0;

} /* end of main */
#endif



/*--------------------------------------------------------------------*/



void pa_init( int argc, char *argv[] )

/* intializes pa-variables.  Determines number of parameters and
 * number of qualifers
 *
 * parameters of routine
 * int        argc;       input; number of items (quals and pars)
 * char       *argv[];    input; argument string
 */
{
	/* local variables */
	int      i;              /* counter */
	char     *cp, *cp1;      /* pointers to char */

	/* executable code */

	pa_parnum = 0;
	pa_qualnum = 0;
	pa_qmap = 0L;

	for  (i=1; i<argc; i++)  {
		if  (argv[i][0] == QUALCHAR1 || argv[i][0] == QUALCHAR2)  {
			if  (pa_qualnum == PAC_MAXQUAL)  {
				printf( "*** module CPAR:  too many qualifers ***\n" );
				return;
			} /*endif*/
			pa_qual[pa_qualnum++] = argv[i];
		} else {
			if  (pa_parnum == PAC_MAXPAR)  {
				printf( "*** module CPAR:  too many parameters ***\n" );
				return;
			} /*endif*/
			pa_par[pa_parnum++] = argv[i];
		} /*endif*/
	} /*endfor*/

	/* find out name of program */
	cp = cp1 = argv[0] + strlen(argv[0]) - 1;
	while  (cp > argv[0] && *cp != '/' && *cp != '\\' && *cp != ']')  cp--;
	if  (cp > argv[0] && cp < cp1)  cp++;
	if  (strlen(cp) < BC_FILELTH)  strcpy( pav_pname, cp );

} /* end of pa_init */



/*--------------------------------------------------------------------*/



int pa_pnumber( void )

/* returns number of parameters
 *
 * no parameters
 */
{
	/* executable code */

	return pa_parnum;

} /* end of pa_pnumber */



/*--------------------------------------------------------------------*/



int pa_qnumber( void )

/* returns number of qualifiers
 *
 * no parameters
 */
{
	/* executable code */

	return pa_qualnum;

} /* end of pa_qnumber */



/*--------------------------------------------------------------------*/



char *pa_pvalue( int number )

/* returns value of parameter number "number"
 *
 * parameters of routine
 * int        number;    number of parameter
 */
{
	/* executable code */

	if  (number < 1 || number > pa_parnum)
		return NULL;
	return pa_par[number-1];

} /* end of pa_pvalue */



/*--------------------------------------------------------------------*/



BOOLEAN pa_qspecified( char name[] )

/* checks whether a particular qualifier is specified in the command
 * line
 *
 * parameter of the routine
 * char      name[];      input; name of the qualifier
 *                        returns TRUE if the qualifier was specified
 */
{
	/* local variables */
	int      i;                  /* counter */
	int      slth;               /* string length */
	char     str[BC_LINELTH+1];  /* name with "=" character */

	/* executable code */

	slth = (int)strlen( name );
	if  (slth >= BC_LINELTH-1)  {
		printf( "*** module CPAR:  qualifier name too long ***\n" );
		return FALSE;
	} /*endif*/

	strcpy( str, name );
	strcat( str, "=" );

	for  (i=0; i<pa_qualnum; i++)
		if  (strcmp(pa_qual[i],name) == 0  ||
			strncmp(pa_qual[i],str,slth+1) == 0)  {
			pa_qmap |= (1<<i);
			return TRUE;
		} /*endif*/
	return FALSE;

} /* end of pa_qspecified */



/*--------------------------------------------------------------------*/



char *pa_qvalue( char name[] )

/* returns value of a particular qualifier
 *
 * parameter of the routine
 * char      name[];      input; name of the qualifier
 *                        returns TRUE if the qualifier was specified
 */
{
	/* local variables */
	int      i;                  /* counter */
	int      slth;               /* string length */
	char     str[BC_LINELTH+1];  /* name with "=" character */

	/* executable code */

	slth = (int)strlen( name );
	if  (slth >= BC_LINELTH-1)  {
		printf( "*** module CPAR:  qualifier name too long ***\n" );
		return NULL;
	} /*endif*/

	strcpy( str, name );
	strcat( str, "=" );

	for  (i=0; i<pa_qualnum; i++)
		if  (strncmp(pa_qual[i],str,slth+1) == 0)  {
			pa_qmap |= (1<<i);
			return (pa_qual[i]+slth+1);
		} /*endif*/
	return NULL;

} /* end of pa_qvalue */



/*--------------------------------------------------------------------*/



int pa_qunchecked( char first[] )

/* returns the number of unchecked qualifers.  If the number is
 * greater than zero, "first" contains the first unchecked qualifier
 *
 * parameters of routine
 * char       first[];    output; first unchecked qualifier
 *                        returns number of unchecked qualifiers
 */
{
	/* local variables */
	int      i;        /* counter */
	int      cnt;      /* counter of unchecked qualifiers */

	/* executable code */

	cnt = 0;
	*first = '\0';
	for  (i=0; i<pa_qualnum; i++)
		if  (((1<<i) & pa_qmap) == 0)
			if  (++cnt == 1)
				strcpy( first, pa_qual[i] );

	return cnt;

} /* end of pa_qunchecked */



/*--------------------------------------------------------------------*/



char *pa_progname( void )

/* returns name of program without path
 *
 * no parameters
 */
{
	/* executable code */

	return pav_pname;

} /* end of pa_progname */



/*--------------------------------------------------------------------*/

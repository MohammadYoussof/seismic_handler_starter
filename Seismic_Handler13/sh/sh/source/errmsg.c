
/* file ERRMSG.C
 *      ========
 *
 * version 17, 3-Nov-2006
 *
 * error messages of SeismicHandler
 * Klaus Stammler, 7-JUL-91
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
#ifdef BC_STDLIB_EX
#include <stdlib.h>
#endif
#include BC_SYSBASE
#include "erusrdef.h"
#include "globalparams.h"


/* prototypes of local routines */
static void err_getline( char fname[], int line, char msg[] );


/* global variables */
char      emv_status_context[BC_LINELTH+1];  /* status context */
char      emv_errdir[BC_FILELTH+1];          /* directory of error files */


/*-------------------------------------------------------------------*/



void err_seterrdir( char path[] )

/* sets path to error messages
 *
 * parameters of routine
 * char       path[];    input; directory of error messages
 */
{
	/* executable code */

	if  (strlen(path) < BC_FILELTH)  {
		strcpy( emv_errdir, path );
	} else {
		printf( "*** error path too long: %s\n", path );
	} /*endif*/

} /* end of err_seterrdir */



/*-------------------------------------------------------------------*/



void err_msg( STATUS status, char msg[] )

/* returns error message of error number "status"
 *
 * parameters of routine
 * STATUS        status;     input; error number
 * char          msg[];      output; error message
 */
{
	/* local variables */
	int      errmain;                /* main number */
	int      errline;                /* line number */
	char     errfile[BC_LINELTH+1];  /* error file */
#	ifdef BC_SUN                     /* needed on sun only */
	char     *eptr;                  /* pointer to environment variable */
#	endif

	/* executable code */

	/* check error path */
	if  (*emv_errdir == '\0')  {
#		ifdef BC_VAX
		strcpy( emv_errdir, "shc_errors:" );
#		endif
#		ifdef BC_SUN
		eptr = GpGetString( cGpS_defpath_errors );
		if  (eptr == NULL)  {
			printf( "*** error path not set and SH_ERRORS not available ***\n" );
			return;
		} else if  (strlen(eptr) > BC_FILELTH)  {
			printf( "*** default error path too long: %s ***\n", eptr );
			return;
		} else {
			strcpy( emv_errdir, eptr );
		} /*endif*/
#		endif
#		ifdef BC_ATARI
		strcpy( emv_errdir, "e:\\pc\\sh\\errors\\" );
#		endif
	} /*endif*/

	errmain = (status / 100) * 100;
	errline = status % 100;

	sprintf( msg, "%d", errmain );
	strcpy( errfile, emv_errdir );
	strcat( errfile, "ERR_" );
	strcat( errfile, msg );
	strcat( errfile, EMC_ERRFILEEXT );

	err_getline( errfile, errline, msg );

} /* end of err_msg */



/*-------------------------------------------------------------------*/



static void err_getline( char fname[], int line, char msg[] )

/* returns line number "line" of file "fname" in parameter "msg"
 *
 * parameters of routine
 * char       fname[];      input; filename
 * int        line;         input; line number
 * char       msg[];        output; returned line
 */
{
	/* local variables */
	FILE     *ef;      /* file pointer */
	int      l;        /* line counter */

	/* executable code */

	l = line;
	ef = sy_fopen( fname, "r" );
	if  (ef == NULL)  {
		sprintf( msg, "*** error file %s (%02d) not found ***",
			fname, line );
		return;
	} /*endif*/
	while  (l > 0)  {
		if  (fgets(msg,BC_LINELTH,ef) == NULL)  {
			sprintf( msg, "*** line %d in file %s not available ***",
				line, fname );
			return;
		} /*endif*/
		if  (*msg != '!')  l--;
	} /*endwhile*/
	fclose( ef );

	/* remove line terminator */
	l = (int)strlen( msg ) - 1;
	if  (msg[l] == '\n')  msg[l] = '\0';

} /* end of err_getline */



/*-------------------------------------------------------------------*/



void err_setcontext( char context[] )

/* sets status context
 *
 * parameters of routine
 * char       char context[];    input; status context
 */
{
	/* executable code */

	if  (*emv_status_context == '\0')  {
		strncpy( emv_status_context, context, BC_LINELTH );
	} else {
		if  (strlen(emv_status_context)+strlen(context) <= BC_LINELTH)  {
			strcat( emv_status_context, context );
		} else if  (strlen(emv_status_context) < BC_LINELTH)  {
			strcat( emv_status_context, "+" );
		} else {
			emv_status_context[BC_LINELTH-1] = '+';
		} /*endif*/
	} /*endif*/

} /* end of err_setcontext */



/*-------------------------------------------------------------------*/



void err_setcontext_l( long l )

/* adds long number to error context
 *
 * parameter of routine
 * long      l;      input; number
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];

	/* executable code */

	sprintf( str, "%ld", l );
	err_setcontext( str );

} /* end of err_set_context_l */



/*-------------------------------------------------------------------*/



void err_setcontext_r( float r )

/* adds long number to error context
 *
 * parameter of routine
 * long      l;      input; number
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];

	/* executable code */

	sprintf( str, "%g", r );
	err_setcontext( str );

} /* end of err_set_context_r */



/*-------------------------------------------------------------------*/



void err_getcontext( char context[] )

/* returns status context.  Maximum length of "context" is 81
 * characters (including terminator).
 *
 * parameters of routine
 * char       context[];  output; context returned
 */
{
	/* executable code */

	strcpy( context, emv_status_context );

} /* end of err_getcontext */



/*-------------------------------------------------------------------*/



void err_clearcontext( void )

/* clears context string
 *
 * no parameters
 */
{
	/* executable code */

	*emv_status_context = '\0';

} /* end of err_clearcontext */



/*-------------------------------------------------------------------*/



void err_writemsg( STATUS status, char text[], BOOLEAN abort )

/* Write error message to standard output, also additional 'text'.
 * Aborts program if 'abort is TRUE.
 *
 * parameters of routine
 * STATUS     status;        input; status code
 * char       text[];        input; additional text
 * BOOLEAN    abort;         input; if TRUE abort program
 */
{
	/* local variables */
	char     msg[BC_LINELTH+1];       /* error message */

	/* executable code */

	err_msg( status, msg );
	fprintf( stderr, "%s\n", msg );
	err_getcontext( msg );
	if  (*msg != '\0')
		fprintf( stderr, "%s\n", msg );
	if  (*text != '\0')
		fprintf( stderr, "%s\n", text );
	err_clearcontext();
	if  (abort)   exit(1);

} /* end of err_writemsg */



/*-------------------------------------------------------------------*/

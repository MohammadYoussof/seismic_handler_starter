
/* file FILEOPEN.C
 *      ==========
 *
 * version 5, 2-May-2006
 *
 * Opens files. Uses logical names
 * K. Stammler, 4-Jan-92
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "fousrdef.h"
#include "foerrors.h"


/* constants */
#define MAXSTRLTH 300
#define MAXLOGNAME 20
#define MAXEQUIV 5
#define LNONE -1


/* global variables */
static char      fov_log[MAXLOGNAME][MAXSTRLTH+1];   /* logical name table */
static int       fov_offset[MAXLOGNAME][MAXEQUIV];   /* name offsets */
static unsigned  fov_logcnt;                         /* number of defined names */
static BOOLEAN   fov_init=FALSE;                     /* initialisation done */



/* prototypes of local routines */



/*----------------------------------------------------------------------*/

#ifdef XXX
int main( void )
{
	STATUS s;
	char   str[200];
	int    i;

	fo_readtable( "shpaths.lnt", &s );
	fo_translate( "SHC_INPUTS:s.shc", TRUE, 199, str, &i );
	while  (i == FOC_ANOTHER)
		fo_translate( "x", FALSE, 199, str, &i );
	return 0;
}
#endif


/*----------------------------------------------------------------------*/



void fo_readtable( char table[], STATUS *status )

/* initializes global variables
 *
 * parameters of routine
 * char       table[];     input; logical name table
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	unsigned  i, j;      /* counters */
	char      *ls;       /* pointer to logical name string */
	FILE      *fp;       /* pointer to table file */
	BOOLEAN   inword;    /* is in a word */

	/* executable code */

	for  (i=0; i<MAXLOGNAME; i++)
		for  (j=0; j<MAXEQUIV; j++)
			fov_offset[i][j] = LNONE;

	fov_logcnt = 0;
	fov_init = TRUE;

	fp = fopen( table, "r" );
	if  (fp == NULL)  {
		*status = FOE_FOPNIN;
		fov_init = FALSE;
		return;
	} /*endif*/

	while  (fgets(fov_log[fov_logcnt],MAXSTRLTH,fp) != NULL)  {
		ls = fov_log[fov_logcnt];
		if  (*ls > '!')  {  /* accept logical name */
			i = j = 0;
			inword = TRUE;
			while  (ls[i] != '\0')  {
				if  (ls[i] == ' ' || ls[i] == '\n')  {
					ls[i] = '\0';
					inword = FALSE;
				} else {
					if  (!inword)  {
						fov_offset[fov_logcnt][j++] = i;
						inword = TRUE;
					} /*endif*/
				} /*endif*/
				i++;
			} /*endwhile*/
			fov_logcnt++;
		} /*endif*/
	} /*endif*/

	fclose( fp );

} /* end of fo_readtable */



/*----------------------------------------------------------------------*/



FILE *fo_fopen( char fname[], char access[] )

/* opens a file, using the paths of the actual logical name table
 *
 * parameters of routine
 * char       fname[];     input; name of file to be opened
 * char       access[];    input; access string, as in usual fopen
 *                         returns file pointer
 */
{
	/* local variables */
	char     str[MAXSTRLTH+1];     /* translated filename */
	char     logname[BC_FILELTH+1];/* logical name */
	int      slth;                 /* string length */
	int      pos;                  /* string position */
	int      i, j;                 /* counters */
	FILE     *fp;                  /* file pointer */
	char     *cptr;                /* pointer to char */

	/* executable code */

	if  (!fov_init)  return fopen( fname, access );

	/* search colon */
	pos = 0;
	while  (fname[pos] != '\0' && fname[pos] != ':')
		pos++;
	if  (fname[pos] == '\0')   /* no colon */
		return fopen( fname, access );

	if  (pos > BC_FILELTH)  return NULL;
	strncpy( logname, fname, pos );
	logname[pos++] = '\0';
	slth = (int)strlen( fname ) - pos;
	/* the filename starts at fname+pos and contains slth chars */

	/* find logical name and try to open file */
	i = 0;
	while  (fov_offset[i][0] != LNONE)  {
		if  (strcmp(fov_log[i],logname) == 0)  {  /* name found */
			/* try all definitions */
			j = 0;
			while  (fov_offset[i][j] != LNONE)  {
				strcpy( str, fov_log[i]+fov_offset[i][j] );
				if  (strlen(str)+slth > MAXSTRLTH)  return NULL;
				strcat( str, fname+pos );
				fp = fopen( str, access );
				if  (fp != NULL)  return fp;
				j++;
			} /*endwhile*/
		} /*endif*/
		i++;
	} /*endwhile*/

#	ifdef BC_SUN
	cptr = getenv( logname );
	if  (cptr != NULL)  {
		if  (strlen(cptr)+slth > MAXSTRLTH-1)  return NULL;
		strcpy( str, cptr );
		strcat( str, "/" );
		strcat( str, fname+pos );
		fp = fopen( str, access );
		if  (fp != NULL)  return fp;
	} /*endif*/
#	endif

	/* not found, open without translation */
	return fopen( fname, access );

} /* end of fo_fopen */



/*----------------------------------------------------------------------*/



void fo_translate( char in[], BOOLEAN first, int maxlth, char out[],
	int *result )

/* If first=TRUE the filename is translated using logical name table.
 * Otherwise the parameter "in" is ignored and the next possible
 * translation of the previous call is returned.  "*result" can have
 * the values FOC_NOTMATCH (no matching logical name found, ->
 * out=in), FOC_ANOTHER (logical name was translated and it exists
 * another translation for the same logical name) and FOC_LAST (the
 * logical name was translated, there exist no more equivalences).
 * If the resulting string is too long FOC_OVFL is returned.
 *
 * parameters of routine
 * char       in[];      input; input filename
 * BOOLEAN    first;     input; first equivalence or not
 * int        maxlth;    input; maximum length of output string
 * char       out[];     output; translated filename
 * int        *result;   output; status message
 */
{
	/* local variables */
	char     logname[BC_FILELTH+1];  /* logical name */
	int      pos;                    /* string position */
	static int  tcnt;                /* translation counter */
	static int  lcnt;                /* logical name counter */
	static int  slth;                /* string length */
	static char fname[BC_FILELTH+1]; /* rest of filename */

	/* executable code */

	if  (!fov_init)  {
		if  (strlen(in) > maxlth)  {
			*result = FOC_OVFL;
			return;
		} /*endif*/
		strcpy( out, in );
		*result = FOC_NOMATCH;
		return;
	} /*endif*/

	if  (!first)  {
		if  (++tcnt >= MAXEQUIV || fov_offset[lcnt][tcnt] == LNONE)  {
			*result = FOC_NOMATCH;
			return;
		} /*endif*/
		if  (strlen(fov_log[lcnt]+fov_offset[lcnt][tcnt])+slth > maxlth)  {
			*result = FOC_OVFL;
			return;
		} /*endif*/
		strcpy( out, fov_log[lcnt]+fov_offset[lcnt][tcnt] );
		strcat( out, fname );
		if  (tcnt < MAXEQUIV-1 && fov_offset[lcnt][tcnt+1] != LNONE)  {
			*result = FOC_ANOTHER;
		} else {
			*result = FOC_LAST;
		} /*endif*/
		return;
	} /*endif*/

	/* search colon */
	pos = 0;
	while  (in[pos] != '\0' && in[pos] != ':')
		pos++;
	if  (in[pos] == '\0')  {   /* no colon */
		if  (strlen(in) > maxlth)  {
			*result = FOC_OVFL;
			return;
		} /*endif*/
		strcpy( out, in );
		*result = FOC_NOMATCH;
		return;
	} /*endif*/

	if  (pos > BC_FILELTH)  {*result = FOC_OVFL; return;}
	strncpy( logname, in, pos );
	logname[pos++] = '\0';
	slth = (int)strlen( in ) - pos;
	if  (slth > BC_FILELTH)  {
		*result = FOC_OVFL;
		return;
	} /*endif*/
	strcpy( fname, in+pos );

	/* find logical name and try to open file */
	lcnt = 0;
	while  (fov_offset[lcnt][0] != LNONE)  {
		if  (strcmp(fov_log[lcnt],logname) == 0)  {  /* name found */
			tcnt = 0;
			if  (strlen(fov_log[lcnt]+fov_offset[lcnt][0])+slth > maxlth)  {
				*result = FOC_OVFL;
				return;
			} /*endif*/
			strcpy( out, fov_log[lcnt]+fov_offset[lcnt][0] );
			strcat( out, fname );
			if  (tcnt < MAXEQUIV-1 && fov_offset[lcnt][tcnt+1] != LNONE)  {
				*result = FOC_ANOTHER;
			} else {
				*result = FOC_LAST;
			} /*endif*/
			return;
		} /*endif*/
		lcnt++;
	} /*endwhile */

	/* not found */
	if  (strlen(in) > maxlth)  {
		*result = FOC_OVFL;
		return;
	} /*endif*/
	strcpy( out, in );
	*result = FOC_NOMATCH;

} /* end of fo_translate */



/*----------------------------------------------------------------------*/


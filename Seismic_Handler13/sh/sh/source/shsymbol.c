
/* File SHSYMBOL.C
 *      ==========
 *
 * version 8, 29-Mar-2007
 *
 * symbol handling of seismhandler
 * K. Stammler, 30-MAR-1990
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
#include "erusrdef.h"
#include "cpusrdef.h"
#include "sserrors.h"
#include "ssusrdef.h"


#define MAXNAMELTH 15
		  /* maximum length of symbol name */
#define MAXSYMNUM  20
		  /* maximum number of symbols */
#define MAXSET     2
		  /* number of symbol sets */
#define EMPTY_DESCRIPTOR "@#EMPTY"
		  /* empty descriptor */
#define MAXVALLTH  511
        /* maximum length of symbol value */


/* global variables */
static char  name_ssv[MAXSET][MAXSYMNUM][MAXNAMELTH];  /* symbol names */
static char  val_ssv[MAXSET][MAXSYMNUM][MAXVALLTH];    /* symbol values */
static int   savecnt_ssv={0};                          /* save file counter */


/* prototypes of local routines */
void ssh_writeline( FILE *fp, char str[] );
void ssh_readline( FILE *fp, char str[] );



/*------------------------------------------------------------------------*/



void ss_define( set, name, value, status )

/* creates a new symbol "name" in set "set" and sets it to value "value" */

/* parameters of routine */
unsigned int  set;       /* input; set number */
char          *name;     /* input; name of new symbol */
char          *value;    /* input; value of new symbol */
int           *status;   /* output; return status */

{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	if  (set >= MAXSET)  {
		*status = SSE_ILSET;
		err_setcontext( " ## set number " ); err_setcontext_l( set );
		return;
	} /*endif*/

	i = 0;
	while  (name_ssv[set][i][0] != '\0')  {
		if  (i++ >= MAXSYMNUM)  {
			*status = SSE_OVFL;
			return;
		} /*endif*/
	} /*endwhile*/

	if  (strlen(name) > MAXNAMELTH)  {
		*status = SSE_NAMLTH;
		err_setcontext( " ## symbol too long " ); err_setcontext( name );
		return;
	} /*endif*/
	strcpy( name_ssv[set][i], name );
	if  (strlen(value) > MAXVALLTH)  {
		*status = SSE_VALLTH;
		return;
	} /*endif*/
	strcpy( val_ssv[set][i], value );

} /* end of ss_define */



/*------------------------------------------------------------------------*/



void ss_delete( set, name, status )

/* deletes an existing symbol */

/* parameters of routine */
unsigned int  set;       /* input; set number */
char          *name;     /* input; name of new symbol */
int           *status;   /* output; return status */

{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	if  (set >= MAXSET)  {
		*status = SSE_ILSET;
		err_setcontext( " ## set number " ); err_setcontext_l( set );
		return;
	} /*endif*/

	for  (i=0;i<MAXSYMNUM;i++)  {
		if  (strcmp(name_ssv[set][i],name) == 0)  {
			name_ssv[set][i][0] = '\0';
			val_ssv[set][i][0] = '\0';
			return;
		} /*endif*/
	} /*endfor*/

	*status = SSE_UDSYM;
	err_setcontext( " ## symbol " ); err_setcontext( name );

} /* end of ss_delete */



/*------------------------------------------------------------------------*/



void ss_delall( set, status )

/* deletes all symbol in set */

/* parameters of routine */
unsigned int  set;       /* input; set number */
int           *status;   /* output; return status */

{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	if  (set >= MAXSET)  {
		*status = SSE_ILSET;
		err_setcontext( " ## set number " ); err_setcontext_l( set );
		return;
	} /*endif*/

	for  (i=0;i<MAXSYMNUM;i++)  {
		name_ssv[set][i][0] = '\0';
		val_ssv[set][i][0] = '\0';
	} /*endfor*/

} /* end of ss_delall */



/*------------------------------------------------------------------------*/


void ss_change( set, name, value, status )

/* changes the value of an existing symbol "name" to "value" */

/* parameters of routine */
unsigned int  set;       /* input; set number */
char          *name;     /* input; name of new symbol */
char          *value;    /* input; value of new symbol */
int           *status;   /* output; return status */

{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	if  (set >= MAXSET)  {
		*status = SSE_ILSET;
		err_setcontext( " ## set number " ); err_setcontext_l( set );
		return;
	} /*endif*/

	for  (i=0;i<MAXSYMNUM;i++)  {
		if  (strcmp(name_ssv[set][i],name) == 0)  {
			if  (strlen(value) > MAXVALLTH)  {
				*status = SSE_VALLTH;
				return;
			} /*endif*/
			strcpy( val_ssv[set][i], value );
			return;
		} /*endif*/
	} /*endfor*/

	*status = SSE_UDSYM;
	err_setcontext( " ## symbol " ); err_setcontext( name );

} /* end of ss_change */



/*------------------------------------------------------------------------*/



void ss_getval( set, name, maxlth, value, status )

/* returns value of existing symbol */

/* parameters of routine */
unsigned int  set;       /* input; set number */
char          *name;     /* input; name of symbol */
int           maxlth;    /* input; maximum length of output string */
char          *value;    /* output; value of symbol */
int           *status;   /* output; return status */

{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	if  (set >= MAXSET)  {
		*status = SSE_ILSET;
		err_setcontext( " ## set number " ); err_setcontext_l( set );
		return;
	} /*endif*/

	for  (i=0;i<MAXSYMNUM;i++)  {
		if  (strcmp(name_ssv[set][i],name) == 0)  {
			if  (strlen(val_ssv[set][i]) > maxlth)  {
				*status = SSE_RETLTH;
				err_setcontext( " ## value " ); err_setcontext( val_ssv[set][i] );
				return;
			} /*endif*/
			strcpy( value, val_ssv[set][i] );
			return;
		} /*endif*/
	} /*endfor*/

	*status = SSE_UDSYM;
	err_setcontext( " ## symbol " ); err_setcontext( name );

} /* end of ss_getval */



/*------------------------------------------------------------------------*/



int ss_exist( set, name )

/* checks whether "name" is defined in "set" or not */

/* parameters of routine */
unsigned int  set;       /* input; symbol set */
char          *name;     /* input; name of symbol */
								 /* returns TRUE if symbol defined */

{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	if  (set >= MAXSET)  return FALSE;

	for  (i=0;i<MAXSYMNUM;i++)
		if  (strcmp(name_ssv[set][i],name) == 0)  return TRUE;

	return FALSE;

} /* end of ss_exist */



/*------------------------------------------------------------------------*/



/* handling of command parameters */

/* current command parameters */
static PARAM  cmdpar_ssv =  { -1, {'\0'}, -1, {'\0'} };
static int    inplev_ssv;   /* input level for parameter request */



/*------------------------------------------------------------------------*/



void ss_setpar( par, inputlev )

/* sets current command parameters */

/* parameters of routine */
PARAM    *par;           /* input; new command parameters */
int      inputlev;       /* input; input level */

{
	/* executable code */
	cmdpar_ssv = *par;
	inplev_ssv = inputlev;

} /* end of ss_setpar */



/*------------------------------------------------------------------------*/



void ss_default( no, value, status )

/* sets default value of parameter number "no" to "value" */

/* parameters of routine */
unsigned int  no;        /* input; parameter number */
char          *value;    /* input; default value */
int           *status;   /* output; return status */

{
	/* local variables */

	/* executable code */

	if  (no > (cmdpar_ssv.pno+1))  {
		*status = SSE_DEFAULT;
		err_setcontext( " ## parno " ); err_setcontext_l( no );
		return;
	} else if  (no == (cmdpar_ssv.pno+1))  {
		if  (no >= CPC_MAXPARAM)  {
			*status = SSE_DEFAULT;
			err_setcontext( " ## parno " ); err_setcontext_l( no );
			return;
		} else if  (strlen(value) > CPC_LINELTH)  {
			*status = SSE_DEFLTH;
			return;
		} /*endif*/
		(cmdpar_ssv.pno)++;
		strcpy( cmdpar_ssv.p[no], value );
	} else {
		if  (cmdpar_ssv.p[no][0] == '\0')  {
			if  (strlen(value) > CPC_LINELTH)  {
				*status = SSE_DEFLTH;
				return;
			} /*endif*/
			strcpy( cmdpar_ssv.p[no], value );
		} /*endif*/
	} /*endif*/

} /* end of ss_default */



/*------------------------------------------------------------------------*/



void ss_getpar( nostr, maxlth, value, status )

/* returns the value of the desired command parameter */

/* parameters of routine */
char     *nostr;         /* input; number of parameter */
int      maxlth;         /* input; maximum string length */
char     *value;         /* output; value returned */
int      *status;        /* output; return status */

{
	/* local variables */
	unsigned int       no;  /* param number */

	/* executable code */

	if  (!isdigit(*nostr))  {
		if  (maxlth < 11)  {
			*status = SSE_RETLTH;
			return;
		} /*endif*/
		if  (strcmp(nostr,"PARAMS") == 0)  {
			sprintf( value, "%d", cmdpar_ssv.pno );
		} else if  (cp_qstr(&cmdpar_ssv,nostr,maxlth,value,status))  {
			if  (*value == '\0')
				strcpy( value, "_NOVALUE_" );
		} else {
			strcpy( value, "_EXISTSNOT_" );
		} /*endif*/
		return;
	} /*endif*/

	if  (sscanf(nostr,"%d",&no) != 1)  {
		*status = SSE_CNVNUM;
		err_setcontext( " ## numstr " ); err_setcontext( nostr );
		return;
	} else if  (no > cmdpar_ssv.pno)  {
		*status = SSE_CPOVFL;
		err_setcontext( " ## parno " ); err_setcontext_l( no );
		return;
	} else if  (strlen(cmdpar_ssv.p[no]) > maxlth)  {
		*status = SSE_RETLTH;
		err_setcontext( " ## string " ); err_setcontext( cmdpar_ssv.p[no] );
		return;
	} /*endif*/

	strcpy( value, cmdpar_ssv.p[no] );

} /* ss_getpar */



/*------------------------------------------------------------------------*/



int ss_query()

/* returns query value (default parameters are queried or not) */

{ return cmdpar_ssv.query; }



/*------------------------------------------------------------------------*/



int ss_inplev()

/* returns current input level */

{ return inplev_ssv; }



/*------------------------------------------------------------------------*/



void ss_push( int ltc, int lgc, int lcc, SHFLAGS flg, int *status )

/* saves all command parameters and symbols of set 0 to a file
 *
 * parameters of routine
 * int      ltc             input; local text channels
 * int      lgc             input; local graphic channels
 * int      lcc             input; local console channels
 * SHFLAGS  flg;            input; local flags
 * int      *status;        output; return status
 */
{
	/* local variables */
	char     savename[BC_LINELTH+1];  /* save filename */
	FILE     *fp;                     /* file pointer */
	int      i;                       /* counter */
	int      symcnt;                  /* number of symbols */

	/* executable code */

	/* count symbols */
	symcnt = 0;
	for  (i=0;i<MAXSYMNUM;i++)
		if  (name_ssv[0][i][0] != '\0')  symcnt++;

	ss_crefilnam( ++savecnt_ssv, savename );
	fp = sy_fopen( savename, "w" );
	if  (fp == NULL)  {
		*status = SSE_PUSH;
		err_setcontext( " ## file " ); err_setcontext( savename );
		return;
	} /*endif*/
	fprintf( fp, "%d\n", cmdpar_ssv.pno );
	for  (i=0; i<=cmdpar_ssv.pno; ssh_writeline(fp,cmdpar_ssv.p[i++]) )  {}
	fprintf( fp, "%d\n", cmdpar_ssv.qno );
	for  (i=0; i<=cmdpar_ssv.qno; ssh_writeline(fp,cmdpar_ssv.q[i++]) )  {}
	fprintf( fp, "%d\n", symcnt );
	for  (i=0;i<MAXSYMNUM;i++)  {
		if  (name_ssv[0][i][0] != '\0')  {
			ssh_writeline( fp, name_ssv[0][i] );
			ssh_writeline( fp, val_ssv[0][i] );
		} /*endif*/
	} /*endfor*/
	fprintf( fp, "%d\n", cmdpar_ssv.query );
	fprintf( fp, "%d\n", inplev_ssv );
	fprintf( fp, "%d,%d,%d,%d\n", ltc, lgc, lcc, flg );
	fclose( fp );

} /* end of ss_push */



/*------------------------------------------------------------------------*/



void ss_pop( int *ltc, int *lgc, int *lcc, SHFLAGS *flg, int *status )

/* restores all command parameters and symbols of set 0 from a file
 *
 * parameters of routine
 * int      *ltc;           output; local text channels
 * int      *lgc;           output; local graphic channels
 * int      *lcc;           output; local console channels
 * SHFLAGS  *flg;           output; local processing flags
 * int      *status;        output; return status
 */
{
	/* local variables */
	char     savename[BC_LINELTH+1];  /* save filename */
	FILE     *fp;                     /* file pointer */
	int      i;                       /* counter */
	int      symcnt;                  /* number of symbols */

	/* executable code */

	for  (i=0;i<MAXSYMNUM;i++)  {
		name_ssv[0][i][0] = '\0';
		val_ssv[0][i][0] = '\0';
	} /*endfor*/

	ss_crefilnam( savecnt_ssv--, savename );
	fp = sy_fopen( savename, "r" );
	if  (fp == NULL)  {
		*status = SSE_POP;
		err_setcontext( " ## file " ); err_setcontext( savename );
		return;
	} /*endif*/
	fscanf( fp, "%d\n", &(cmdpar_ssv.pno) );
	for  (i=0; i<=cmdpar_ssv.pno; ssh_readline(fp,cmdpar_ssv.p[i++]) )  {}
	fscanf( fp, "%d\n", &(cmdpar_ssv.qno) );
	for  (i=0; i<=cmdpar_ssv.qno; ssh_readline(fp,cmdpar_ssv.q[i++]) )  {}
	fscanf( fp, "%d\n", &symcnt );
	for  (i=0;i<symcnt;i++)  {
		ssh_readline( fp, name_ssv[0][i] );
		ssh_readline( fp, val_ssv[0][i] );
	} /*endfor*/
	fscanf( fp, "%d\n", &(cmdpar_ssv.query) );
	fscanf( fp, "%d\n", &inplev_ssv );
	fscanf( fp, "%d,%d,%d,%d\n", ltc, lgc, lcc, flg );
	fclose( fp );

	sy_fdelete( savename );

} /* end of ss_pop */



/*------------------------------------------------------------------------*/



void ss_crefilnam( cnt, name )

/* creates save file name */

/* parameters of routine */
int      cnt;            /* input; number of file */
char     *name;          /* output; filename created */

{
	/* local variables */
	char     numstr[11];    /* digit string */

	/* executable code */

	sprintf( numstr, "%d", cnt );
	strcpy( name, shd_scratch );
	strcat( name, id_shv );
	strcat( name, "SAV" );
	strcat( name, numstr );
	strcat( name, SHC_DE_SAVE );

} /* end of ss_crefilnam */



/*------------------------------------------------------------------------*/



void ss_dump( fp, set )

/* dumps symbols or cmdpars to file */

/* parameters of routine */
FILE     *fp;        /* input; output file */
int      set;        /* input; set number (if -1 dump cmdpars) */

{
	/* local variables */
	int  i;

	/* executable code */

	if  (set >= MAXSET || set < -1)  return;

	if  (set == -1)  {
		fprintf( fp, "command parameter block\n" );
		for  (i=0;i<=cmdpar_ssv.pno;i++)
			fprintf( fp, "p%2d: %s\n", i, cmdpar_ssv.p[i] );
		for  (i=0;i<=cmdpar_ssv.qno;i++)
			fprintf( fp, "q%2d: %s\n", i, cmdpar_ssv.q[i] );
	} else {
		fprintf( fp, "symbols set %d\n", set );
		for  (i=0;i<MAXSYMNUM;i++)
			if  (name_ssv[set][i][0] != '\0')
				fprintf( fp, "%s: %s\n", name_ssv[set][i], val_ssv[set][i] );
	} /*endif*/

} /* end of ss_dump */



/*------------------------------------------------------------------------*/



void ssh_writeline( FILE *fp, char str[] )

/* writes one line to a file. If str is empty an empty-descriptor string is
 * written instead
 *
 * parameters of routine
 * FILE       *fp;      input; file pointer
 * char       str[];    input; string to be written
 */
{
	/* executable code */

	if  (*str == '\0')  {
		fprintf( fp, "%s\n", EMPTY_DESCRIPTOR );
	} else {
		fprintf( fp, "%s\n", str );
	} /*endif*/

} /* end of ssh_writeline */



/*------------------------------------------------------------------------*/



void ssh_readline( FILE *fp, char str[] )

/* read one line from a file. If an empty-descriptor string is read *str is
 * set to '\0'.
 *
 * parameters of routine
 * FILE       *fp;      input; file pointer
 * char       str[];    output; string read
 */
{
	/* local variables */
	int      strlth;

	/* executable code */

	/* fscanf( fp, "%s\n", str ); */
	fgets( str, 200, fp );
	strlth = (int)strlen(str) - 1;
	if  (str[strlth] == '\n')
		str[strlth] = '\0';
	if  (strcmp(str,EMPTY_DESCRIPTOR) == 0)
		*str = '\0';

} /* end of ssh_readline */



/*------------------------------------------------------------------------*/

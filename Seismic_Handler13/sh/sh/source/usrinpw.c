
/* File USRINPW.C
 *      =========
 *
 * version 18, 4-Jul-2006
 *
 * Routines for interactive user input in windows.  All inputs are stored
 * on a protocol file and can be recalled.
 *
 * K. Stammler, 9-OCT-1989
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
#include BC_GCUSRDEF
#include "erusrdef.h"
#include "utusrdef.h"
#include "uiusrdef.h"
#include "uierrors.h"
#include "globalparams.h"

/* global constants */
#define MAX_LEVEL 10
#define DEFEXT ".SHC"
#define MAXCMDFLTH BC_FILELTH

/* global variables */
static FILE    *pu_uiv;                  /* protocol file unit */
static FILE    *iu_uiv[MAX_LEVEL];       /* current input unit */
static int     currlin_uiv[MAX_LEVEL];   /* current line of input file */
static int     currlev_uiv;              /* current level */
static char    protfile_uiv[BC_LINELTH]; /* name of protocol file */
static int     flags_uiv;                /* global flags */
static int     fromfile_uiv;             /* input from file */
static int     shiftlev_uiv={-1};        /* shift level (if -1 no shift) */
static char    cmdfnam_uiv[MAX_LEVEL][MAXCMDFLTH+1];  /* cmdfile names */
static CHMAP   console_uiv;              /* console channel */
static char    stepflag_uiv;             /* user input at step */
static int     lastsock_uiv=(-1);        /* socket ID of last connection */
static int     rdport_uiv=0;             /* IP port for command reading */


/* prototypes of local routines */
char *ui_getstr( int wdw, char *str, int maxlth, FILE *fp );
void ui_wstr( int wdw, char *str );


/*------------------------------------------------------------------------*/



void ui_initialize( char prot_file[], STATUS *status )

/* initializes ui-routines.  Call once at the beginning of your program
 *
 * parameters of routine
 * char      prot_file[];  input; name of protocol file without extension
 * STATUS    *status;      output; return status
 */
{
	/* local variables */
	int      i;        /* counter */
#ifdef SH_SOCKET
	char     *env;     /* pointer to environment */
#endif

	/* executable code */

	*status = UIE_NOERROR;

	if  (strlen(prot_file) > BC_LINELTH-4)  {
		*status = UIE_NAMLTH;
		return;
	} /*endif*/

	strcpy( protfile_uiv, prot_file );
	strcat( protfile_uiv, DEFEXT );
	flags_uiv = UIF_ECHO | UIF_CAPCNV;
	currlev_uiv = 0;
	strncpy( cmdfnam_uiv[0], "CONSOLE", MAXCMDFLTH );

	for  (i=0;i<MAX_LEVEL;i++)  {
		currlin_uiv[i] = 0;
	} /* end for */

	iu_uiv[0] = stdin;

	pu_uiv = sy_fopen( protfile_uiv, "w" );

	if  (pu_uiv == NULL)  {
		*status = UIE_OPENFILE;
		err_setcontext( " ## file " ); err_setcontext( protfile_uiv );
	} else {
		flags_uiv = flags_uiv | UIF_DOPROT;
	} /*endif*/

	fromfile_uiv = FALSE;
	lastsock_uiv = -1;

#ifdef SH_SOCKET
	env = (char *)getenv( "SH_READ_PORT" );
	if  (env == NULL)  {
		rdport_uiv = 0;
	} else {
		if  (sscanf(env,"%d",&rdport_uiv) != 1)  rdport_uiv = 0;
		if  (rdport_uiv > 0)  pio_init_main_socket( rdport_uiv );
	} /*endif*/
	if  (GpGetInt(cGpI_debug_level) > 1)
		if  (rdport_uiv > 0)
			printf( "SH-dbg2: reading from port %d\n", rdport_uiv );
#endif

} /* end of ui_initialize */



/*------------------------------------------------------------------------*/



void ui_exit( char cmdfile[] )

/* exits from UI routines.  Call once before program exit
 *
 * parameter of routine
 * char         cmdfile[];   input; name of command file to create
 */
{
	/* local variables */
	int      i;        /* counter */
	char     fnam[BC_LINELTH+1];  /* new filename */

	/* executable code */
	for  (i=currlev_uiv;i>0;i--)  {
		if  (iu_uiv[i] != stdin)  fclose( iu_uiv[i] );
	} /* end for */
	currlev_uiv = 0;

	fclose( pu_uiv );

	if  (*cmdfile == '\0')  {
		sy_fdelete( protfile_uiv );
	} else {
		strcpy( fnam, cmdfile );
		strcat( fnam, DEFEXT );
		sy_fdelete( fnam );   /* delete file if it exists already */
		sy_frename( protfile_uiv, fnam );
	} /*endif*/

}  /* end of ui_exit */



/*------------------------------------------------------------------------*/



void ui_switch( char cmdfile[], STATUS *status )

/* switches input to file "cmdfile"
 *
 * parameters of routine
 * char      cmdfile[];     input; name of command file to process
 * STATUS    *status;       output; return status
 */
{
	/* local variables */
	char   cmdf[BC_FILELTH+1]; /* command file with extension */
	int    namlth;             /* length of filename */
	int    levstore;           /* level store */

	/* executable code */

	*status = UIE_NOERROR;

	namlth = (int)strlen(cmdfile);
	if  (namlth > BC_FILELTH-4)  {
		*status = UIE_NAMLTH;
		return;
	} /*endif*/

	levstore = 0;
	if  (shiftlev_uiv != -1)  {   /* level shifted, do unshift */
		levstore = currlev_uiv;
		currlev_uiv = shiftlev_uiv;
		shiftlev_uiv = -1;
	} /*endif*/

	if  (namlth > 0)  {
		if  (currlev_uiv == MAX_LEVEL-1)  {
			*status = UIE_LEVOVFL;
		} else {
			if  (strcmp(cmdfile,"TT") == 0)  {
				iu_uiv[++currlev_uiv] = stdin;
			} else {
				strcpy( cmdf, cmdfile );
				ut_defext( cmdf, BC_FILELTH, DEFEXT, status );
				if  (Severe(status))  return;
				iu_uiv[++currlev_uiv] = sy_fopen( cmdf, "r" );
				if  (iu_uiv[currlev_uiv] == NULL)  {
					*status = UIE_OPENFILE;
					err_setcontext( " ## file " ); err_setcontext( cmdf );
					currlev_uiv--;
				} else {
					currlin_uiv[currlev_uiv] = 0;
					strncpy( cmdfnam_uiv[currlev_uiv], cmdf, MAXCMDFLTH );
				} /*endif*/
			} /*endif*/
		} /*endif*/
	} else {
		if  (currlev_uiv == 0)  {
			*status = UIE_LEVUDRFL;
		} else {
			if  (iu_uiv[currlev_uiv] != stdin)
				fclose( iu_uiv[currlev_uiv] );
			currlev_uiv--;
		} /*endif*/
	} /*endif*/

	/* if level was shifted, restore shift */
	if  (levstore != 0)  {
		shiftlev_uiv = currlev_uiv;
		currlev_uiv = levstore;
	} /*endif*/

	fromfile_uiv = (currlev_uiv > 0);

}  /* end of ui_switch */



/*------------------------------------------------------------------------*/



int ui_level( void )

/* returns current command level */

{
	return currlev_uiv;

} /* end of ui_level */



/*------------------------------------------------------------------------*/



void ui_setflag( int flag, BOOLEAN val )

/* sets or clears bits set in flag in flags_uiv
 *
 * parameters of routine
 * int      flag;    flags to be set or cleared
 * BOOLEAN  val;     TRUE=set flag  FALSE=clear flag
 */
{
	/* executable code */

	if  (val)  {
		flags_uiv |= flag;
	} else {
		flags_uiv &= ~flag;
	} /*endif*/

}  /* end of ui_setflag */



/*------------------------------------------------------------------------*/



void ui_absflag( int flags )

/* sets flags directly
 *
 * parameter of routine
 * int       flags;    input; new flag values
 */
{  flags_uiv = flags;

} /* end of ui_absflag */



/*------------------------------------------------------------------------*/



void ui_setconsole( CHMAP con )

/* sets console channel.  Needed at step prompts
 *
 * parameters of routine
 * CHMAP      con;    input; console channel
 */
{
   /* executable code */

   console_uiv = con;

} /* end of ui_setconsole */



/*------------------------------------------------------------------------*/



void ui_read( int wdw, char str[], int maxlth, STATUS *status )

/* reads "str" from current input stream
 *
 * parameter of routine
 * int     wdw;       input; window number
 * char    str[];     output; string read from input stream
 * int     maxlth;    input; maximum length of input string
 * STATUS  *status;   output; return status
 */
{
	/* local variables */
	char     ch[3];       /* dummy chars */

	/* executable code */

	*status = UIE_NOERROR;
	stepflag_uiv = '\0';
	currlin_uiv[currlev_uiv]++;

	if  (ui_getstr( wdw, str, maxlth, iu_uiv[currlev_uiv] ) == NULL)  {
		/* ui_switch( "", status ); */  /* made by main program */
		*status = UIE_READFILE;
		return;
	} /*endif*/

	/* *(str+strlen(str)-1) = '\0'; */
	if  (fromfile_uiv)  {
		if  (flags_uiv & UIF_ECHO)  {
			ui_wstr( wdw, str );
			ui_wstr( wdw, "\n" );
		} /*endif*/
		if  (flags_uiv & UIF_STEP)  {
			ui_wstr( console_uiv, "UISTEP: " );
			ui_wstr( console_uiv, str );
			gc_read( console_uiv, 3, ch );
			stepflag_uiv = *ch;
			if  (*ch == '?')  *status = UIE_STEPABORT;
		} /*endif*/
	} else {
		if  (flags_uiv & UIF_DOPROT)  fprintf(pu_uiv,"%s\n",str);
	} /*endif*/

	if  (flags_uiv & UIF_CAPCNV)  ut_cap( str );

}  /* end of ui_read */



/*------------------------------------------------------------------------*/



char ui_readchar( int wdw, STATUS *status )

/* reads "str" from current input stream
 *
 * parameter of routine
 * int     wdw;       input; window number
 * STATUS  *status;   output; return status
 */
{
	/* local variables */
	int      rdchar;      /* character read */
	char     ch[3];       /* dummy chars */

	/* executable code */

	*status = UIE_NOERROR;
	stepflag_uiv = '\0';
	currlin_uiv[currlev_uiv]++;

	rdchar = ui_getchar( wdw, iu_uiv[currlev_uiv] );
	if  (rdchar == EOF)  {
		*status = UIE_READFILE;
		return (char)'\0';
	} else if  (rdchar == 0)  {
		*status = UIE_NOTIMPL;
		return (char)'\0';
	} /*endif*/

	if  (fromfile_uiv)  {
		*status = UIE_HKFILE;
		return (char)'\0';
	} else {
		if  (flags_uiv & UIF_DOPROT)  fprintf(pu_uiv,"%c",(char)rdchar);
	} /*endif*/

	if  (flags_uiv & UIF_CAPCNV)  {
		return Cap((char)rdchar);
	} else {
		return (char)rdchar;
	} /*endif*/

}  /* end of ui_readchar */



/*------------------------------------------------------------------------*/



void ui_getloc( int wdw, float *x, float *y, char *key, STATUS *status )

/* gets location from current input stream
 *
 * parameters of routine
 * int     wdw;       input; window number (if interactive)
 * float   *x, *y;    output; location returned
 * char    *key;      output; ASCII code of key pressed
 * STATUS  *status;   output; return status
 */
{
	/* local variables */
	char     ch[3];       /* scratch */
	int      ascii;       /* ascii code */

	/* executable code */

	*status = UIE_NOERROR;
	currlin_uiv[currlev_uiv]++;

	if  (fromfile_uiv)  {
		if  (fscanf(iu_uiv[currlev_uiv],"%e,%e,%d\n",x,y,&ascii) != 3)  {
			/* ui_switch( "", status ); */ /* made in main module */
			*status = UIE_READFILE;
			return;
		} /*endif*/
		*key = (char)ascii;
		if  (flags_uiv & UIF_STEP)  {
			ui_wstr( console_uiv, "UISTEP: location" );
			gc_read( console_uiv, 3, ch );
			if  (*ch == '?')  *status = UIE_STEPABORT;
		} /*endif*/
	} else {
		gc_getloc( wdw, x, y, key );
		if  (flags_uiv & UIF_DOPROT)
			fprintf( pu_uiv, "%e,%e,%d\n", *x, *y, (char)(*key) );
	} /*endif*/

} /* end of ui_getloc */



/*------------------------------------------------------------------------*/



void ui_goto( char label[], BOOLEAN dorewind, STATUS *status )

/* jumps to a label in a command file
 *
 * parameters of routine
 * char     label[];        input; label name
 * BOOLEAN  dorewind;       input; rewind file before searching
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     cmp[BC_LINELTH+1];   /* line read from file */
	char     *start;              /* start pointer */
	int      lablth;              /* length of label */

	/* executable code */

	if  (currlev_uiv == 0)  {
		*status = UIE_IAJ;
		return;
	} /*endif*/

	if  (dorewind)  {
		fseek( iu_uiv[currlev_uiv], 0L, 0 );
		currlin_uiv[currlev_uiv] = 0;
	} /*endif*/

	lablth = (int)strlen( label );

	FOREVER  {

		/* read next line */
		currlin_uiv[currlev_uiv]++;
		if  (fgets(cmp,BC_LINELTH,iu_uiv[currlev_uiv]) == NULL)  {
			/* ui_switch( "", status ); */ /* made in main module */
			*status = UIE_LABNF;
			return;
		} /*endif*/

		/* skip blanks */
		start = cmp;
		while  (*start != '\0' && (*start == ' '|| *start == '\t'))
			start++;

		/* skip line terminator & capitalize */
		start[strlen(start)-1] = '\0';
		ut_cap( start );

		/* if line equals label -> finished */
		if  (strncmp(start,label,lablth) == 0)  return;

	} /*endfor*/

} /* end of ui_goto */



/*------------------------------------------------------------------------*/



int *ui_lines( void )

/* returns array of line counters (only for reading) */

{
	return currlin_uiv;
} /* end of ui_lines */



/*------------------------------------------------------------------------*/



char *ui_levelname( int level )

/* returns name of level "level" (only for reading)
 *
 * parameters of routine
 * int        level;      input; level
 *                        returns pointer to level name
 */
{
	/* executable code */

	if  ((level < 0) || (level >= MAX_LEVEL))  return NULL;
	return cmdfnam_uiv[level];

} /* end of ui_levelname */




/*------------------------------------------------------------------------*/



void ui_shift( int level )

/* decreases input level to "level" or if negative by "level"
 *
 * parameters of routine
 * int      level;          input; input level for following input
 */
{
	/* executable code */

	if  (shiftlev_uiv != -1)  return;  /* only one shift */
	if  (level < 0)  {  /* relative shift */
		if  ((currlev_uiv+level) >= 0)  {
			shiftlev_uiv = currlev_uiv;
			currlev_uiv += level;
		} /*endif*/
	} else if  (currlev_uiv > level)  {  /* absolute shift */
		shiftlev_uiv = currlev_uiv;
		currlev_uiv = level;
	} /*endif*/
	fromfile_uiv = (currlev_uiv > 0);

} /* end of ui_shift */



/*------------------------------------------------------------------------*/



void ui_unshift( void )

/* resets ui_shift call */

{
	/* executable code */

	if  (shiftlev_uiv == -1)  return;
	currlev_uiv = shiftlev_uiv;
	shiftlev_uiv = -1;
	fromfile_uiv = (currlev_uiv > 0);

} /* end of ui_unshift */



/*------------------------------------------------------------------------*/



char ui_stepflag( void )

/* returns character entered at STEP
 *
 * no input parameters
 */
{
	return stepflag_uiv;

} /* end of ui_stepflag */



/*------------------------------------------------------------------------*/
/*                                local routines                          */
/*------------------------------------------------------------------------*/



char *ui_getstr( int wdw, char str[], int maxlth, FILE *fp )

/* reads a string from terminal
 *
 * parameters of routine
 * int      wdw;            input; window number
 * char     str[];          output; string read
 * int      maxlth;         input; maximum length of string
 * FILE     *fp;            file pointer
 */
{
	/* local variables */
	char     *ptr;

	/* executable code */

	if  ((wdw == 0) || (fp != stdin))  {
#		ifdef BC_ATARI
		if  (fp == stdin)  {
			sy_alert( "*** no GEM input.  Abort ***" );
			return NULL;
		} /*endif*/
#		endif
		ptr = fgets(str,maxlth,fp);
		if  (ptr != NULL)  *(str+strlen(str)-1) = '\0';
#ifdef SH_SOCKET
		lastsock_uiv = -1;
#endif
		return ptr;
	} else if  (wdw & GCF_STDCH)  {
#ifdef SH_SOCKET
		if  (rdport_uiv > 0)  {
			int    cmdlth;   /* length of command line */
			pio_read( maxlth-1, str, &cmdlth, &lastsock_uiv );
			if  (GpGetInt(cGpI_debug_level) > 2)
				printf( "SH-dgb3: got cmd >%s< from socket %d\n", str, lastsock_uiv );
			ptr = str;
		} else {
#endif
			ptr = fgets(str,maxlth,stdin);
			if  (ptr != NULL)  *(str+strlen(str)-1) = '\0';
#ifdef SH_SOCKET
			lastsock_uiv = -1;
		} /*endif*/
#endif
		return ptr;
	} else {
		gc_read( wdw, maxlth, str );
		return str;
	} /*endif*/

} /* end of ui_getstr */



/*------------------------------------------------------------------------*/



int ui_getchar( int wdw, FILE *fp )

/* reads a character from terminal
 *
 * parameters of routine
 * int      wdw;            input; window number
 * FILE     *fp;            file pointer
 * returns character read or EOF
 */
{
	/* local variables */
	int      rd;      /* read char */

	/* executable code */

	if  ((wdw == 0) || (fp != stdin))  {
#		ifdef BC_ATARI
		if  (fp == stdin)  {
			sy_alert( "*** no GEM input.  Abort ***" );
			return NULL;
		} /*endif*/
#		endif
		rd = fgetc( fp );
		return rd;
	} else if  (wdw == 0 || wdw & GCF_STDCH)  {
		rd = fgetc( stdin );
		return rd;
	} else {
		return 0;
		/*gc_readchar( wdw, maxlth, str ); */
	} /*endif*/

} /* end of ui_getchar */



/*------------------------------------------------------------------------*/



void ui_wstr( int wdw, char str[] )

/* write string to window
 *
 * parameters of routine
 * int      wdw;            input; window number
 * char     *str;           input; output string
 */
{
	/* executable code */

	if  (wdw == 0)  {
		/* printf( "%s", str ); */
	} else {
		gc_write( wdw, str );
	} /*endif*/

} /* end of ui_wstr */



/*------------------------------------------------------------------------*/



int ui_lastsocket( void )

/* returns socket ID of last connection
 *
 * no input parameters
 */
{
	/* executable code */

	return lastsock_uiv;

} /* end of ui_lastsocket */



/*------------------------------------------------------------------------*/


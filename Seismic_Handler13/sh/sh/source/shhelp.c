
/* File SHHELP.C
 *      ========
 *
 * version 5, 22-May-2006
 *
 * HELP routines of seismhandler
 *
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
#include "basecnst.h"
#include "ascii.h"
#include BC_SYSBASE
#include "shconst.h"
#include BC_GCUSRDEF
#include "fctxhl.h"
#include "fctxsl.h"
#include "utusrdef.h"
#include "sherrors.h"

#define ITEMWIDTH 15
#define BLANKS "               "
#define KEYWIDTH  35
#define KEYSEP 3
#define KEYSEPSTR "   "
#define KEYWDLTH 4
#define KEYNAME "key:"
#define DOTS " ..................................."


/* prototypes of local routines */
void hlh_wait( int ch );


/*------------------------------------------------------------------------*/



void hl_full( ch, dir, cmds, status )

/* displays full help file matching the wild card string "cmds" */

/* parameters of routine */
int      ch;             /* input; output channel(s) */
char     *dir;           /* input; directory string to look for */
char     *cmds;          /* input; command wild card string */
int      *status;        /* output; return status */

{
	/* local variables */
	char     name[BC_LINELTH+1];      /* name of help file */
	char     wild[BC_LINELTH+1];      /* wild card string */
	char     file[BC_LINELTH+1];      /* full file name */
	FILE     *fp;                     /* file pointer */
	int      done;                    /* at least one file found */
	int      linecnt, maxline;        /* line counter & max line */

	/* executable code */

	*wild = '\0';
	if  (dir != NULL)  strcpy( wild, dir );
	strcat( wild, cmds );
	ut_defext( wild, BC_LINELTH, SHC_DE_HELP, status );
	if  (Severe(status))  return;

	done = FALSE;
	FOREVER  {
		sy_findfile( SYC_FF_NAME|SYC_FF_EXT, wild, name );
		if  (*name == '\0')  {
			if  (!done)  *status = SHE_NOHELP;
			sy_findfile( SYC_FF_NAME, "", name );
			return;
		} /*endif*/
		*file = '\0';
		if  (dir != NULL)  strcpy( file, dir );
		strcat( file, name );
		if  (Severe(status)) return;
		maxline = gc_txtheight( ch ) - 2;
		linecnt = 0;
		fp = sy_fopen( file, "r" );
		while  (fgets(file,BC_LINELTH,fp) != NULL)  {
			gc_write( ch, file );
			if  (++linecnt == maxline)  {
				hlh_wait( ch );
				linecnt = 0;
			} /*endif*/
		} /*endwhile*/
		fclose( fp );
		done = TRUE;
	} /*endfor*/

} /* end of hl_full */



/*------------------------------------------------------------------------*/



void hl_hdr( ch, dir, cmds, status )

/* displays header line of help file matching the wild card string "cmds" */

/* parameters of routine */
int      ch;             /* input; output channel(s) */
char     *dir;           /* input; directory string to look for */
char     *cmds;          /* input; command wild card string */
int      *status;        /* output; return status */

{
	/* local variables */
	char     name[BC_LINELTH+1];      /* name of help file */
	char     wild[BC_LINELTH+1];      /* wild card string */
	char     file[BC_LINELTH+1];      /* full file name */
	FILE     *fp;                     /* file pointer */
	int      done;                    /* at least one file found */
	int      linecnt, maxline;        /* line counter, max line */

	/* executable code */

	*wild = '\0';
	if  (dir != NULL)  strcpy( wild, dir );
	strcat( wild, cmds );
	ut_defext( wild, BC_LINELTH, SHC_DE_HELP, status );

	maxline = gc_txtheight( ch ) - 2;
	linecnt = 0;

	done = FALSE;
	FOREVER  {
		sy_findfile( SYC_FF_NAME|SYC_FF_EXT, wild, name );
		if  (*name == '\0')  {
			if  (!done)  *status = SHE_NOHELP;
			sy_findfile( SYC_FF_NAME|SYC_FF_EXT, "", name );
			gc_write( ch, "\n" );
			return;
		} /*endif*/
		*file = '\0';
		if  (dir != 0)  strcpy( file, dir );
		strcat( file, name );
		fp = sy_fopen( file, "r" );
		while  (fgets(file,BC_LINELTH,fp) != NULL)  {
			if  (strncmp(file,"command",7) == 0)  {
				gc_write( ch, file );
				if  (++linecnt == maxline)  {
					hlh_wait( ch );
					linecnt = 0;
				} /*endif*/
				done = TRUE;
				break;
			} /*endif*/
		} /*endwhile*/
		fclose( fp );
	} /*endfor*/

} /* end of hl_hdr */



/*------------------------------------------------------------------------*/



void hl_dir( ch, dir, cmds, linewidth, status )

/* displays directory of help file matching the wild card string "cmds" */

/* parameters of routine */
int      ch;             /* input; output channel(s) */
char     *dir;           /* input; directory string to look for */
char     *cmds;          /* input; command wild card string */
int      linewidth;      /* input; linewidth in characters */
int      *status;        /* output; return status */

{
	/* local variables */
	char     name[BC_LINELTH+1];      /* name of help file */
	char     wild[BC_LINELTH+1];      /* wild card string */
	int      done;                    /* at least one file found */
	int      cnt;                     /* item counter */
	int      maxcnt;                  /* maximum no of items per line */
	int      linecnt, maxline;        /* line counter & max line */

	/* executable code */

	*wild = '\0';
	if  (dir != NULL)  strcpy( wild, dir );
	strcat( wild, cmds );
	ut_defext( wild, BC_LINELTH, SHC_DE_HELP, status );

	maxline = gc_txtheight( ch ) - 2;
	linecnt = 0;
	maxcnt = linewidth / ITEMWIDTH;
	cnt = 0;
	done = FALSE;
	FOREVER  {
		sy_findfile( SYC_FF_NAME, wild, name );
		if  (*name == '\0')  {
			if  (!done)  *status = SHE_NOHELP;
			sy_findfile( SYC_FF_NAME, "", name );
			gc_write( ch, "\n\n" );
			return;
		} /*endif*/
		if  (++cnt > maxcnt)  {
			gc_write( ch, "\n" );
			if  (++linecnt == maxline)  {
				hlh_wait( ch );
				linecnt = 0;
			} /*endif*/
			cnt = 1;
		} /*endif*/
		strcat( name, BLANKS );
		name[ITEMWIDTH] = '\0';
		gc_write( ch, name );
		done = TRUE;
	} /*endfor*/

} /* end of hl_dir */



/*------------------------------------------------------------------------*/



void hl_key( ch, dir, cmds, linewidth, status )

/* displays key line of help file matching the wild card string "cmds" */

/* parameters of routine */
int      ch;             /* input; output channel(s) */
char     *dir;           /* input; directory string to look for */
char     *cmds;          /* input; command wild card string */
int      linewidth;      /* input; width of line in chars */
int      *status;        /* output; return status */

{
	/* local variables */
	char     name[BC_LINELTH+1];      /* name of help file */
	int      namlth;                  /* length of name */
	char     wild[BC_LINELTH+1];      /* wild card string */
	char     file[BC_LINELTH+1];      /* full file name */
	int      keylth;                  /* length of key string */
	FILE     *fp;                     /* file pointer */
	int      done;                    /* at least one file found */
	int      keyfound;                /* key found in help file */
	int      cnt;                     /* item counter */
	int      maxcnt;                  /* maximum no of items per line */
	int      linecnt, maxline;        /* line count & max line */

	/* executable code */

	*wild = '\0';
	if  (dir != NULL)  strcpy( wild, dir );
	strcat( wild, cmds );
	strcat( wild, SHC_DE_HELP );

	maxline = gc_txtheight( ch ) - 2;
	linecnt = 0;
	maxcnt = (linewidth+KEYSEP) / (KEYWIDTH+KEYSEP);
	cnt = 0;
	done = FALSE;
	FOREVER  {
		sy_findfile( SYC_FF_NAME, wild, name );
		if  (*name == '\0')  {
			if  (!done)  *status = SHE_NOHELP;
			sy_findfile( SYC_FF_NAME, "", name );
			gc_write( ch, "\n\n" );
			return;
		} /*endif*/
		*file = '\0';
		if  (dir != NULL)  strcpy( file, dir );
		strcat( file, name );
		strcat( file, SHC_DE_HELP );
		fp = sy_fopen( file, "r" );
		keyfound = FALSE;
		while  (fgets(file,BC_LINELTH,fp) != NULL)  {
			if  (strncmp(file,KEYNAME,KEYWDLTH) == 0)  {
				done = TRUE;
				keyfound = TRUE;
				break;
			} /*endif*/
		} /*endwhile*/
		fclose( fp );
		if  (!keyfound)  {
			strcpy( file, KEYNAME );
			strcat( file, "--- no key entered ---" );
		} /*endif*/
		if  (++cnt > maxcnt)  {
			gc_write( ch, "\n" );
			if  (++linecnt == maxline)  {
				hlh_wait( ch );
				linecnt = 0;
			} /*endif*/
			cnt = 1;
		} /*endif*/
		namlth = (int)strlen( name );
		keylth = (int)strlen( file+KEYWDLTH );
		file[KEYWDLTH+(keylth--)-1] = '\0';
		if  (keylth > (KEYWIDTH-namlth-1))  {
			file[KEYWDLTH+KEYWIDTH-namlth] = '\0';
			keylth = KEYWIDTH-namlth-1;
		} /*endif*/
		strcat( name, DOTS );
		name[KEYWIDTH-keylth] = '\0';
		strcat( name, file+KEYWDLTH );
		if  (cnt > 1)  gc_write( ch, KEYSEPSTR );
		gc_write( ch, name );
	} /*endfor*/

} /* end of hl_key */



/*------------------------------------------------------------------------*/


void hlh_wait( int ch )

/* waits for key to be pressed
 *
 * parameter of routine
 * int       ch;       input; channel map
 */
{
	/* local variables */
	char     dmy[4];

	/* executable code */

	gc_write( ch, "   <CR> ..." );
	gc_read( ch|GCF_NOECHO, 3, dmy );
	gc_wrtch( ch, CR );
	gc_write( ch, "           " );
	gc_wrtch( ch, CR );

} /* end of hlh_wait */



/*------------------------------------------------------------------------*/

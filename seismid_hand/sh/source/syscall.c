
/* File SYSCALL.C
 *      =========
 *
 * version 37, 7-Dec-2006
 *
 * calls of system dependent routines
 * this file contains all available implementations
 * K. Stammler, 27-FEB-1990
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


#include "basecnst.h"



/******************************************************************
 ***                        ATARI version                       ***
 ******************************************************************/



#ifdef BC_ATARI


#include <tos.h>
#include <aes.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#define __FROM_SYSCALL
#include "sysbase.h"
#include "syerrors.h"


#ifdef BC_KS_PRIVATE
#include BC_GCUSRDEF
/* #define DEBUG_SHARE_CPU */
#ifdef DEBUG_SHARE_CPU
#include "e:\pc\util\mailbuf.h"
#endif
#define SYSDIR "e:\\pc\\sh\\COMMAND\\COMMAND.TOS"
#define Mfork(a,b) gemdos(112,a,0x5aa7,b)
#define ap_count _GemParBlk.global[1]
extern GEMPARBLK _GemParBlk;
static BOOLEAN syv_multigem_installed;
#endif /* BC_KS_PRIVATE */


/* one global variable: Application ID */
int    syv_apid;

/* another global variable: wait length */
static int syv_sharecounter;


/*------------------------------------------------------------------------*/



void sy_initprocess( void )

/* initialises process
 *
 * no parameters
 */
{
	/* local variables */
	STATUS   s=BC_NOERROR;

	/* executable code */

	syv_apid = appl_init();
	sy_lognames( FOC_DEFAULTLOGICALS, &s );
#	ifdef BC_KS_PRIVATE
	syv_multigem_installed = (ap_count > 1);
	if  ((1<<(int)('M'-'A')) & Drvmap())  /* if drive M available */
		sy_lognames( FOC_ALTERNATELOGICALS, &s );
#	endif /* BC_KS_PRIVATE */
#	ifdef BC_SHARE_CPU
#	ifdef DEBUG_SHARE_CPU
	mb_init();
	mb_setcounter( 0, 0 );
#	endif /* DEBUG_SHARE_CPU */
#	endif /* BC_SHARE_CPU */

} /* end of sy_initprocess */



/*------------------------------------------------------------------------*/


#ifdef BC_SHARE_CPU


void sy_sharecpu( void )

/* let other process get some CPU time
 *
 * no parameters
 */
{
	/* executable code */

#	ifdef DEBUG_SHARE_CPU
	mb_inccounter( 0 );
#	endif /* DEBUG_SHARE_CPU */
	evnt_timer( syv_sharecounter, 0 );

} /* end of sy_sharecpu */



/*------------------------------------------------------------------------*/



char *sy_getmessage( void )

/* returns mailbox message
 *
 * no parameters
 */
{
	/* local variables */
#	ifdef DEBUG_SHARE_CPU
	static char   symsg[MBC_STRLTH+1];  /* message */
	char          *cptr;

	/* executable code */

	cptr = mb_getstring(1);
	if  (cptr == NULL)  return NULL;
	if  (*cptr == '\0')  return NULL;
	strcpy( symsg, mb_getstring(1) );
	mb_setstring( 1, "" );
	return symsg;
#	else
	return NULL;
#	endif /* DEBUG_SHARE_CPU */

} /* end of sy_getmessage */



/*------------------------------------------------------------------------*/



void sy_debuginfo( char info[] )

/* transfers debug information
 *
 * parameters of routine
 * char          info[];   input; info string
 */
{
	/* executable code */

#	ifdef DEBUG_SHARE_CPU
	mb_setstring( 0, info );
#	endif /* DEBUG_SHARE_CPU */

} /* end of sy_debuginfo */

#endif /* BC_SHARE_CPU */

/*------------------------------------------------------------------------*/



void sy_gettime( char timstr[] )

/* returns current time in time string.  The time string needs 25
 * characters (including the termination byte)
 *
 * parameters of routine
 */
{
	/* local variables */
	time_t       t;
	struct tm    *timeptr;

	/* executable code */

	t = time( NULL );
	timeptr = localtime( &t );
	strftime( timstr, 25, "%d-%b-%Y_%H:%M:%S", timeptr );

} /* end of sy_gettime */



/*------------------------------------------------------------------------*/



void sy_alert( char text[] )

/* displays message text
 *
 * parameters of display
 * char       text[];     input; text to be displayed
 */
{
	/* local variable */
	char     msg[BC_LONGSTRLTH];       /* text string */
	char     str[BC_LONGSTRLTH];       /* alert parameter */
	char     *c, *strt;                /* moving pointer */

	/* executable code */

	strncpy( msg, text, BC_LONGSTRLTH-9 );
	if  (strlen(msg) > 40)  {
		strt = msg;
		do  {
			c = strt+39;
			while  (*c != ' '  &&  c > strt)
				c--;
			if  (c == strt)  {
				strt[39] = '|';
				strt += 40;
			} else {
				*c = '|';
				strt = c+1;
			} /*endif*/
		}  while  (strlen(strt) > 40);
	} /*endif*/
	sprintf( str, "[1][%s][OK]", msg );
	form_alert( 1, str );

} /* end of sy_alert */



/*------------------------------------------------------------------------*/



void *sy_allocmem( long cnt, int size, STATUS *status )

/* allocates memory ("cnt" objects of size "size") 
 *
 * parameters of routine
 * long     cnt;            input; number of objects
 * int      size;           input; size of each object
 * STATUS   *status;        output; return status
 */
{
   /* local variables */
   void     *ptr;     /* pointer to allocated memory */

   /* executable code */

	ptr = malloc( cnt*size );
   if  (ptr == NULL)  *status = SYE_MEMOVFL;
   return ptr;

} /* end of sy_allocmem */



/*------------------------------------------------------------------------*/



void sy_deallocmem( void *ptr )

/* deallocates memory
 *
 * parameter of routine
 * void     *ptr;        input; pointer to be freed
 */
{
   /* executable code */

   free( ptr );

} /* end of sy_deallocmem */



/*------------------------------------------------------------------------*/



long sy_fbread( void *strbuf, int size, long n, BFILE fp )

/* reads "n" objects of size "size" from binary file "fp"
 *
 * parameters of routine
 * void     *strbuf;        output; buffer to read to
 * int      size;           input; size of each object
 * long     n;              input; number of objects
 * BFILE    fp;             input; file descriptor
 */
{
   /* executable code */

   return (Fread(fp,(long)size*n,strbuf)/(long)size);

} /* end of sy_fbread */



/*------------------------------------------------------------------------*/



long sy_fbwrite( void *strbuf, int size, long n, BFILE fp )

/* writes "n" objects of size "size" to file "fp"
 *
 * parameters of routine
 * void     *strbuf;        input; buffer to read to
 * int      size;           input; size of each object
 * long     n;              input; number of objects
 * BFILE    fp;             input; file descriptor
 */
{
   /* executable code */

   return (Fwrite(fp,(long)size*n,strbuf)/(long)size);

} /* end of sy_fbwrite */



/*------------------------------------------------------------------------*/



int sy_fbseek( BFILE fp, long pos, int mode )

/* changes file position
 *
 * parameters of routine
 * BFILE      fp;    input; file pointer
 * long       pos;   input; new position
 * int        mode;  input; search mode
 */
{

	return (int)Fseek( pos, fp, mode );

} /* end of sy_fbseek */



/*------------------------------------------------------------------------*/



BFILE sy_fbopen( const char *file, const char *access )

/* opens a binary file
 *
 * parameters of routine
 * const char    *file;    input; filename
 * const char    *access;  input; access mode
 */
{
	/* local variables */
	int      handle;

	/* executable code */

	if  (*access == 'r')  {
		return Fopen( file, FO_READ );
	} else if  (*access == 'w')  {
		return Fcreate( file, 0 );
	} else if  (*access == 'a')  {
		handle = Fopen( file, FO_READ );
		if  (handle > 0)  {
			Fseek( 0L, handle, 2 );
			return handle;
		} /*endif*/
		return Fcreate( file, 0 );
	} /*endif*/

	return -1;

} /* end of sy_fbopen */



/*------------------------------------------------------------------------*/



int sy_fbclose( BFILE fp )

/* closes binary file
 *
 * parameter of routine
 * BFILE     fp;      input; file pointer
 */
{
	return Fclose( fp );

} /* end of sy_fbclose */



/*------------------------------------------------------------------------*/



void sy_fdelete( char fname[] )

/* deletes file "fname"
 *
 * parameters of routine
 * char       fname[];     input; name of file to be deleted
 */
{
	/* local variables */
	char     tfname[BC_FILELTH+1];      /* translated filename */
	int      res;                       /* translation result */

	/* executable code */

	fo_translate( fname, TRUE, BC_FILELTH, tfname, &res );
	if  (res == FOC_OVFL)  return;
	if  (!Fdelete(tfname))  return;
	while  (res == FOC_ANOTHER)  {
		fo_translate( "", FALSE, BC_FILELTH, tfname, &res );
		if  (res == FOC_OVFL)  return;
		if  (!Fdelete(tfname))  return;
	} /*endwhile*/

} /* end of sy_fdelete */



/*------------------------------------------------------------------------*/



void sy_frename( char from[], char to[] )

/* renames a file from "from" to "to"
 *
 * parameters of routine
 * char       from[];    input; old filename
 * char       to[];      input; new filename
 */
{
	/* local variables */
	char     tfrom[BC_FILELTH+1];  /* translated "from" */
	char     tto[BC_FILELTH+1];    /* translated "to" */
	int      resto, resfrom;       /* translation results */
	int      i;                    /* counter */
	int      cnt;                  /* counts "to" translations */

	/* executable code */

	cnt = 0;
	FOREVER  {

		/* translate "to" "cnt+1" times */
		fo_translate( to, TRUE, BC_FILELTH, tto, &resto );
		if  (resto == FOC_OVFL)  return;
		for  (i=0; i<cnt; i++)
			fo_translate( to, FALSE, BC_FILELTH, tto, &resto );

		/* now try all translations of "from" */
		fo_translate( from, TRUE, BC_FILELTH, tfrom, &resfrom );
		if  (resfrom == FOC_OVFL)  return;
		if  (!Frename( 0, tfrom, tto ))  return;
		while  (resfrom == FOC_ANOTHER)  {
			fo_translate( "", FALSE, BC_FILELTH, tfrom, &resfrom );
			if  (resfrom == FOC_OVFL)  return;
			if  (!Frename( 0, tfrom, tto ))  return;
		} /*endwhile*/

		/* if still not found, check whether another "to" is available */
		if  (resto != FOC_ANOTHER)  return;
		cnt++;

	} /*endfor*/

} /* end of sy_frename */



/*------------------------------------------------------------------------*/



void sy_findfile( int request, char wild[], char filename[] )

/* looks for files matching wild card string "wild". Returns filename of
 *   file found (no directory, no extension) or "\0" if not found.
 *
 * parameters of routine
 * int      request;        input; what elements (name,dir,ext)
 * char     *wild;          input; wild card string
 * char     *filename;      output; next file found
 */
{
   /* local variables */
   static char   currwild[BC_FILELTH+1];  /* current wild card str */
	static DTA    dta;                     /* DTA */
	char     twild[BC_FILELTH+1];          /* translated wild card */
	int      res;                          /* translation result */
   long     dosret;                       /* GEMDOS return value */
	int      dirlen;                       /* length of directory string */

   /* executable code */

   if  (*wild == '\0')  {
      *currwild = '\0';
      return;
   } /*endif*/

   if  (*currwild == '\0')  {   /* first call of routine */
		Fsetdta( &dta );
      strcpy( currwild, wild );
		fo_translate( wild, TRUE, BC_FILELTH, twild, &res );
		if  (res == FOC_OVFL)  return;
      dosret = Fsfirst( twild, 0 );
   } else if  (strcmp(currwild,wild) == 0)  {  /* not first call */
      dosret = Fsnext();
   } else {  /* changed wild card string */
      strcpy( currwild, wild );
		fo_translate( wild, TRUE, BC_FILELTH, twild, &res );
		if  (res == FOC_OVFL)  return;
      dosret = Fsfirst( twild, 0 );
   } /*endif*/

   if  (dosret != 0L)  {
		fo_translate( wild, FALSE, BC_LINELTH, twild, &res );
		if  (res != FOC_ANOTHER && res != FOC_LAST)  {
	      *filename = '\0';
   	   return;
		} /*endif*/
		dosret = Fsfirst( twild, 0 );
		if  (dosret != 0)  {
	      *filename = '\0';
   	   return;
		} /*endif*/
   } /*endif*/

	if  (SYC_FF_DIR & request)  {
		dirlen = (int)strlen( twild ) - 1;
		while  (twild[dirlen] != '\\' && twild[dirlen] != '\0')
			dirlen--;
		if  (twild[dirlen++] == '\\')  {
			if  (strlen(dta.d_fname)+dirlen > BC_FILELTH)  {
				strcpy( filename, "*** overflow ***" );
				return;
			} /*endif*/
			strncpy( filename, twild, dirlen );
			filename[dirlen] = '\0';
		   strcat( filename, dta.d_fname );
		} else {
		   strcpy( filename, dta.d_fname );
		} /*endif*/
	} else {
	   strcpy( filename, dta.d_fname );
	} /*endif*/

   /* drop extension */
	if  (!(SYC_FF_EXT & request))  {
	   while  (*filename != '\0')  {
	      if  (*filename == '.')  {
	         *filename = '\0';
	         return; 
	      } /*endif*/
	      filename++;
	   } /*endwhile*/
	} /*endif*/

} /* end of sy_findfile */



/*------------------------------------------------------------------------*/



void sy_system( char cmd[], STATUS *status )

/* call to operating system
 *
 * parameters of routine
 * char     *cmd;              input; system command
 * STATUS   *status;           output; return status
 */
{
   /* local variables */
#	ifdef BC_KS_PRIVATE
	int      i;                     /* counter */
	int      slth;                  /* length of string */
	static char     cverb[BC_LINELTH+1];   /* command verb */
	static char     parm[BC_LINELTH+1];    /* parameters */
#	endif /* BC_KS_PRIVATE */

   /* executable code */

#	ifdef BC_KS_PRIVATE
	if  (syv_multigem_installed)  {
		slth = (int)strlen( cmd );
		i = 0;
		while  (cmd[i] != ' '  &&  i < slth)
			i++;
		if  (i >= slth-1)  {
			strcpy( cverb, cmd );
			*parm = '\0';
			slth = 0;
		} else {
			if  (i > BC_LINELTH)  {
				*status = SYE_STROVFL;
				return;
			} /*endif*/
			strncpy( cverb, cmd, i );
			cverb[i] = '\0';
			slth = (int)strlen( cmd+i );
			if  (slth+9 > BC_LINELTH)  {
				*status = SYE_STROVFL;
				return;
			} /*endif*/
			strcpy( parm+1, cmd+i );
		} /*endif*/
		/* strcat( parm+1, " -a" ); */  /* excluded 7.12.91 */
		/* sprintf( parm+1+slth+3, "%d", syv_apid ); */
		*parm = (char)(strlen(parm+1));
		if  (*cverb != '\0')  {
			shel_write( TRUE, TRUE, 1, cverb, parm );
			if  (Mfork( cverb, parm ) != 0)
				*status = SYE_SUBMIT;
		} /*endif*/
	} else {
		printf( "\n--> executing COMMAND.PRG\n" );
		if  (Pexec( 0, SYSDIR, "", cmd ) != 0L)  *status = SYE_OSCALL;
	} /*endif*/
#	else
	if  (system( cmd ) == 0)
		*status = SYE_OSCALL;
#	endif /* BC_KS_PRIVATE */

} /* end of sy_system */




/*------------------------------------------------------------------------*/



double sy_random( double ampl )

/* creates random number with absolute value less than (or equal to)
	"amp" */

/* parameters of routine */
/* double		ampl;		input; max. amplitude; if zero, a new random */
/* 									 series is started */

{
	/* local constants */
#	define MAXRAND 32767.0

	/* local variables */
	int		rdm;			/* integer random result */

	/* executable code */

	if  (ampl == 0.0)  {
		srand( (unsigned)clock() );
		return 0.0;
	} else {
		rdm = rand();
		return  ( (((double)rdm * 2.0) / MAXRAND - 1.0) * ampl );
	} /*endif*/

} /* end of sy_random */



/*------------------------------------------------------------------------*/



DIFFTIME sy_difftime( void )

/* returns time since last sy_difftime call
 */
{
#ifdef BC_CLOCK_T_AVAILABLE
	/* local variables */
	static clock_t  diff;    /* time mark */
	clock_t         curr;    /* current time */
	clock_t         tmp;     /* scratch */

	/* executable code */

	curr = clock();
	tmp = curr - diff;
	diff = curr;
	return ((DIFFTIME)tmp/200.0);
#else
	return 0.0;
#endif
} /* end of sy_difftime */



/*------------------------------------------------------------------------*/



void sy_localinf( char item[], char value[], STATUS *status )

/* returns local info in "value"
 *
 * parameters of routine
 * char       item[];     input; info item
 * char       value[];    output; return value
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	void     *l;
	long     lval;
	int      i;
	char     str[BC_LINELTH+1];

	/* executable code */

	if  (strcmp(item,"SHOW_MEMORY") == 0)  {
		l = Malloc( -1L );
		sprintf( value, "   free memory: %ld bytes\n", l );
	} else if  (strncmp(item,"SHARE_COUNTER",13) == 0)  {
		sscanf( item+13, "%d", &syv_sharecounter );
	} else if  (strncmp(item,"WAIT",4) == 0)  {
		sscanf( item+4, "%ld", &lval );
		evnt_timer( (int)lval, (int)(lval>>16) );
	} else if  (strncmp(item,"SETDRIVE",4) == 0)  {
		Dsetdrv( (int)(*value-'A') );
	} else if  (strncmp(item,"SETPATH",4) == 0)  {
		Dsetpath( value );
	} else if  (strncmp(item,"EXEC",4) == 0)  {
		if  (strlen(value) > BC_LINELTH-1)  {
			*status = SYE_STROVFL;
			return;
		} /*endif*/
		strcpy( str+1, value );
		str[0] = (char)strlen( value );
		lval = Pexec( 0, item+5, str, NULL );
		if  (lval < 0)  *status = SYE_SUBMIT;
	} else if  (strncmp(item,"DRIVE_",6) == 0)  {
		i = item[6] - 'A';  /* Bitnumber */
		strcpy( value, "N" );
		if  ((1<<i) & Drvmap())  *value = 'Y';
	} else if  (strncmp(item,"PXHC",4) == 0)  {
		sscanf( item+4, "%d", &i );
		gc_pixhardcopy( i, 1, value );
	} else {
		*status = SYE_LOCALINF;
	} /*endif*/

} /* end of sy_localinf */



/*------------------------------------------------------------------------*/


#endif /* BC_ATARI */



/******************************************************************
 ***                        IBM PC version                      ***
 ******************************************************************/



#ifdef BC_IBM



#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include "sysbase.h"
#include "syerrors.h"
#include "shvars.h"

#define MAXFFCMDLTH 300

/*------------------------------------------------------------------------*/



void *sy_allocmem( long cnt, int size, STATUS *status )

/* allocates memory ("cnt" objects of size "size)
 *
 * parameters of routine
 * long     cnt;            input; number of objects
 * int      size;           input; size of each object
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	void     *ptr;     /* pointer to allocated memory */

	/* executable code */

	ptr = (void *)malloc( cnt*size );
	if  (ptr == NULL)  *status = SYE_MEMOVFL;
	return ptr;

} /* end of sy_allocmem */



/*------------------------------------------------------------------------*/



void sy_findfile( int request, char wild[], char filename[] )

/* looks for files matching wild card string "wild". Returns filename of
 * file found (with/without directory and/or extension, depending on "request") or
 * "\0" if not found.
 *
 * parameters of routine
 * int      request;        input; what elements (name,dir,ext)
 * char     *wild;          input; wild card string
 * char     *filename;      output; next file found
 */
{
	/* local variables */
	static char   currwild[BC_FILELTH+1];  /* current wild card string */
	static char   dirfile[BC_FILELTH+1];   /* output filename */
	static char   errfile[BC_FILELTH+1];   /* error output filename */
	static FILE   *df;                     /* directory file */
	char          cmd[MAXFFCMDLTH+1];    /* command string */
	int           filpos;                  /* position of file name */
	int           extpos;                  /* position of extension */
	char          *c;                      /* moving pointer */

	/* executable code */

	if  (*dirfile == '\0')  {
		strcpy( dirfile, shd_scratch );
		strcat( dirfile, id_shv );
		strcpy( errfile, dirfile );
		strcat( dirfile, "DIR.TMP" );
		strcat( errfile, "ERR.TMP" );
	} /*endif*/

	if  (*wild == '\0')  {
		*currwild = '\0';
		if  (df != NULL)  fclose( df );
		df = NULL;
		return;
	} /*endif*/

	if  (strcmp(currwild,wild) != 0)  {  /* new wild card string */
		if  (df != NULL)  fclose( df );
		strcpy( currwild, wild );
		strcpy( cmd, "dir " );
		strcat( cmd, wild );
		strcat( cmd, " >" );
		strcat( cmd, dirfile );
		strcat( cmd, " 2>" );
		strcat( cmd, errfile );
		system( cmd );
		df = fopen( dirfile, "r" );
		if  (df == NULL)  {
			*currwild = '\0';
			*filename = '\0';
			return;
		} /*endif*/
	} /*endif*/

	if  (fgets(filename,BC_FILELTH,df) == NULL) {
		if  (df != NULL)  fclose( df );
		*currwild = '\0';
		*filename = '\0';
		return;
	} /*endif*/

	filpos = strlen( filename ) - 1;
	if  (filename[filpos] == '\n')  filename[filpos] = '\0';

	if  (request == (SYC_FF_NAME|SYC_FF_EXT|SYC_FF_DIR))  return;

	filpos = extpos = 0;
	c = filename;
	while  (*c != '\0')  {
		if  (*c == '.')  {
			extpos = c-filename;
		} else if (*c == '\\')  {
			filpos = c-filename;
		} /*endif*/
		c++;
	} /*endwhile*/

	if  (!(SYC_FF_EXT & request)  &&  extpos > 0)
		filename[extpos] = '\0';

	if  (!(SYC_FF_DIR & request)  &&  filpos > 0)
		strcpy( filename, filename+filpos+1 );

} /* end of sy_findfile */



/*------------------------------------------------------------------------*/



double sy_random( double ampl )

/* creates random number with absolute value less than (or equal to)
 * "amp"
 *
 * parameters of routine
 * double      ampl;      input; max. amplitude; if zero, a new random
 *                               series is started
 */
{
	/* local constants */
#	define MAXRAND 32767.0 

	/* local variables */
	int      rdm;         /* integer random result */

	/* executable code */

	if  (ampl == 0.0)  {
		srand( (unsigned)clock() );
		return 0.0;
	} else {
		rdm = rand();
		return  ( (((double)rdm * 2.0) / MAXRAND - 1.0) * ampl );
	} /*endif*/

} /* end of sy_random */



/*------------------------------------------------------------------------*/



void sy_randomstr( int lth, char str[] )

/* creates random string of length "lth"
 *
 * parameters of routine
 * int        lth;      input; length of output string
 * char       str[];    output; random string
 */
{
#ifdef XXX
	/* local variables */
	time_t  timev;      /* current time */
	struct tms ts;
	char     minstr[6]; /* minimum length string */
	int      i;         /* counter */

	/* executable code */

	if  (lth < 3)  {
		*str = '\0';
		return;
	} /*endif*/
	*str++ = '_';
	lth -= 2;

	times(&ts);
	timev = ts.tms_utime;
	timev %= 10000;
	sprintf( minstr, "%04d", timev );
	if  (lth < 4)  {
		for  (i=0;i<lth;i++)
			str[i] = minstr[i];
	} else {
		strcpy( str, minstr );
		for  (i=4; i<lth; i++)
			str[i] = '0';
	} /*endif*/
	str[lth] = '_';
	str[lth+1] = '\0';
#endif /* XXX */
	strncpy( str, "XXXXXXXXXXX", lth ); str[lth] = '\0';
} /* end of sy_randomstr */



/*------------------------------------------------------------------------*/



#endif /* BC_IBM */



/******************************************************************
 ***                         VAX version                        ***
 ******************************************************************/



#ifdef BC_VAX


#include <stdio.h>
#include <string.h>
#include <descrip.h>
#include <rmsdef.h>
#include <dvidef.h>
#include <time.h>
#include "basecnst.h"
#ifdef BC_STDLIB_EX
#include <stdlib.h>
#endif
#include "sysbase.h"
#include "syerrors.h"
#include "shc_main:shconst.h"
#include "shc_main:shvars.h"


/*------------------------------------------------------------------------*/



void *sy_allocmem( long cnt, int size, STATUS *status )

/* allocates memory ("cnt" objects of size "size)
 *
 * parameters of routine
 * long     cnt;            input; number of objects
 * int      size;           input; size of each object
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	void     *ptr;     /* pointer to allocated memory */

	/* executable code */

	if  (cnt <= 0)  {
		*status = SYE_ZEROALLOC;
		return NULL;
	} /*endif*/
	ptr = calloc( cnt, size );
	if  (ptr == NULL)  *status = SYE_MEMOVFL;
	return ptr;

} /* end of sy_allocmem */



/*------------------------------------------------------------------------*/



void sy_deallocmem( void *ptr )

/* deallocates memory
 *
 * parameter of routine
 * void     *ptr;        input; pointer to be freed
 */
{
	/* executable code */

	free( ptr );

} /* end of sy_deallocmem */



/*------------------------------------------------------------------------*/



long sy_fread( void *strbuf, int size, long n, FILE *fp )

/* reads "n" objects of size "size" from file "fp"
 *
 * parameters of routine
 * void     *strbuf;        output; buffer to read to
 * int      size;           input; size of each object
 * long     n;              input; number of objects
 * FILE     *fp;            input; file descriptor
 */
{
	/* executable code */

	return ((long)fread(strbuf,size,(int)n,fp));

} /* end of sy_fread */



/*------------------------------------------------------------------------*/



long sy_fwrite( void *strbuf, int size, long n, FILE *fp )

/* writes "n" objects of size "size" to file "fp"
 *
 * parameters of routine
 * void     *strbuf;        input; buffer to read to
 * int      size;           input; size of each object
 * long     n;              input; number of objects
 * FILE     *fp;            input; file descriptor
 */
{
	/* executable code */

	return ((long)fwrite(strbuf,size,(int)n,fp));

} /* end of sy_fwrite */



/*------------------------------------------------------------------------*/



long sy_fbread( void *strbuf, int size, long n, BFILE fp )

/* reads "n" objects of size "size" from binary file "fp"
 *
 * parameters of routine
 * void     *strbuf;        output; buffer to read to
 * int      size;           input; size of each object
 * long     n;              input; number of objects
 * BFILE    fp;             input; file descriptor
 */
{
	/* executable code */

	return fread(strbuf,size,n,fp);

} /* end of sy_fbread */



/*------------------------------------------------------------------------*/



long sy_fbwrite( void *strbuf, int size, long n, BFILE fp )

/* writes "n" objects of size "size" to file "fp"
 *
 * parameters of routine
 * void     *strbuf;        input; buffer to read to
 * int      size;           input; size of each object
 * long     n;              input; number of objects
 * BFILE    fp;             input; file descriptor
 */
{
	/* executable code */

	return fwrite(strbuf,size,n,fp);

} /* end of sy_fbwrite */



/*------------------------------------------------------------------------*/



int sy_fbseek( BFILE fp, long pos, int mode )

/* changes file position
 *
 * parameters of routine
 * BFILE      fp;    input; file pointer
 * long       pos;   input; new position
 * int        mode;  input; search mode
 */
{

	return fseek( fp, pos, mode );

} /* end of sy_fbseek */



/*------------------------------------------------------------------------*/



BFILE sy_fbopen( const char *file, const char *access )

/* opens a binary file
 *
 * parameters of routine
 * const char    *file;    input; filename
 * const char    *access;  input; access mode
 */
{
	/* local variables */
	int      handle;

	/* executable code */

	return fopen( file, access );

} /* end of sy_fbopen */



/*------------------------------------------------------------------------*/



int sy_fbclose( BFILE fp )

/* closes binary file
 *
 * parameter of routine
 * BFILE     fp;      input; file pointer
 */
{
	return fclose( fp );

} /* end of sy_fbclose */



/*------------------------------------------------------------------------*/

#ifdef XXX

void sy_findfile_vax( int request, char wild[], char filename[] )

/* looks for files matching wild card string "wild". Returns filename of
 *   file found (no directory, no extension) or "\0" if not found.
 *
 * parameters of routine
 * int      request;        input; what elements (name,dir,ext)
 * char     *wild;          input; wild card string
 * char     *filename;      output; next file found
 */
{
	/* local variables */
	static char   currwild[BC_FILELTH+1];  /* current wild card str */
	static void   *fab;                    /* FAB address */
	int           sy_status;               /* system service return status */
	int           extpos;                  /* poistion of extension */
	int           filpos;                  /* position of filename */
	char          *c;                      /* moving pointer */
	$DESCRIPTOR( in_f, " " );
	$DESCRIPTOR( out_f, " " );

	/* executable code */

	if  (*wild == '\0')  {
		*currwild = '\0';
		if  (fab != NULL)  {
			lib$find_file_end( &fab );
			fab == NULL;
		} /*endif*/
		return;
	} /*endif*/

	if  (strcmp(currwild,wild) != 0)  {
		if  (fab != NULL)  {
			lib$find_file_end( &fab );
			fab = NULL;
		} /*endif*/
		strcpy( currwild, wild );
	} /*endif*/

	in_f.dsc$w_length = strlen( currwild );
	in_f.dsc$a_pointer = currwild;
	out_f.dsc$w_length = BC_FILELTH;
	out_f.dsc$a_pointer = filename;
	sy_status = lib$find_file( &in_f, &out_f, &fab );

	if  (sy_status != RMS$_NORMAL)  {
		*filename = '\0';
		lib$find_file_end( &fab );
		fab == NULL;
	} /*endif*/

	/* delete blanks at end of string */
	c = filename + strlen( filename ) - 1;
	while  (*c == ' '  &&  c >= filename)
		*c-- = '\0';

	if  ((SYC_FF_NAME|SYC_FF_EXT|SYC_FF_DIR) == request)  return;

	/* drop extension and/or directory */
	filpos = 0;
	extpos = 0;
	c = filename;
	while  (*c != '\0')  {
		if  (*c == '.')  {
			extpos = c-filename;
		} else if  ((*c == ']') || (*c == ':'))  {
			filpos = c-filename;
		} /*endif*/
		c++;
	} /*endwhile*/

	if  (!(SYC_FF_EXT & request))
		filename[extpos] = '\0';

	if  (!(SYC_FF_DIR & request) && filpos > 0)
		strcpy( filename, filename+filpos+1 );

} /* end of sy_findfile_vax */

#endif  /* XXX */

/*------------------------------------------------------------------------*/

#define MAXFFCMDLTH 200

void sy_findfile( int request, char wild[], char filename[] )

/* looks for files matching wild card string "wild". Returns filename of
 * file found (with/without directory and/or extension, depending on "request") or
 * "\0" if not found.
 *
 * parameters of routine
 * int      request;        input; what elements (name,dir,ext)
 * char     *wild;          input; wild card string
 * char     *filename;      output; next file found
 */
{
	/* local variables */
	static char   currwild[BC_FILELTH+1];  /* current wild card string */
	static char   dirfile[BC_FILELTH+1];   /* output filename */
	static char   errfile[BC_FILELTH+1];   /* error output filename */
	static FILE   *df;                     /* directory file */
	char          cmd[MAXFFCMDLTH+1];      /* command string */
	int           filpos;                  /* position of file name */
	int           extpos;                  /* position of extension */
	char          *c;                      /* moving pointer */
	int           i;                       /* counter */
	char          dirstr[BC_FILELTH+1];    /* directory string */

	/* executable code */

	if  (*dirfile == '\0')  {
		strcpy( dirfile, shd_scratch );
		strcat( dirfile, id_shv );
		strcpy( errfile, dirfile );
		strcat( dirfile, "DIR.TMP" );
		strcat( errfile, "ERR.TMP" );
	} /*endif*/

	if  (*wild == '\0')  {
		*currwild = '\0';
		if  (df != NULL)  fclose( df );
		df = NULL;
		return;
	} /*endif*/

	if  (strcmp(currwild,wild) != 0)  {  /* new wild card string */
		if  (df != NULL)  fclose( df );
		strcpy( currwild, wild );
		sy_fdelete( dirfile );
		strcpy( cmd, "$ dir/width=(file:70)/out=" );
		strcat( cmd, dirfile );
		strcat( cmd, " " );
		strcat( cmd, wild );
		system( cmd );
		printf( "                                               \r" );
		df = fopen( dirfile, "r" );
		if  (df == NULL)  {
			*currwild = '\0';
			*filename = '\0';
			return;
		} /*endif*/
		for  (i=0; i<3; i++)  fgets(filename,BC_FILELTH,df);
	} /*endif*/

	if  (fgets(filename,BC_FILELTH,df) == NULL) {
		if  (df != NULL)  fclose( df );
		*currwild = '\0';
		*filename = '\0';
		return;
	} else if  (*filename == '\n')  {
		if  (df != NULL)  fclose( df );
		*currwild = '\0';
		*filename = '\0';
		return;
	} /*endif*/

	filpos = 0;
	while  (filename[filpos] != ';')
		filpos++;
	if  (filpos > BC_FILELTH)  {
		printf( "*** this cannot happen (sy_findfile) ***\n" );
		exit( 0 );
	} /*endif*/
	filename[filpos] = '\0';

	if  (request == (SYC_FF_NAME|SYC_FF_EXT))  return;

	if  (request & SYC_FF_DIR)  {
		strcpy( dirstr, filename );
		filpos = strlen( wild ) - 1;
		while  (filpos > 0  &&
			(wild[filpos] != ']' && wild[filpos] != ':'))  {
			filpos--;
		} /*endwhile*/
		if  (filpos > 0)  {
			strncpy( filename, wild, filpos+1 );
			strcat( filename, dirstr );
		} /*endif*/
	} /*endif*/

	if  (request & SYC_FF_EXT)  return;

	extpos = 0;
	c = filename;
	while  (*c != '\0')  {
		if  (*c == '.')
			extpos = c-filename;
		c++;
	} /*endwhile*/

	filename[extpos] = '\0';

} /* end of sy_findfile */



/*------------------------------------------------------------------------*/



void sy_system( char *cmd, STATUS *status )

/* call to operating system
 *
 * parameters of routine
 * char     *cmd;           input; system command
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      sy_status;     /* system service return status */
	$DESCRIPTOR( cmd_dscr, " " );

	/* executable code */

	cmd_dscr.dsc$w_length = strlen( cmd );
	cmd_dscr.dsc$a_pointer = cmd;
	sy_status = lib$spawn( &cmd_dscr );
	if  (!sy_status)  lib$signal( sy_status );

} /* end of sy_system */




/*------------------------------------------------------------------------*/



double sy_random( double ampl )

/* creates random number with absolute value less than (or equal to)
 * "amp"
 *
 * parameters of routine
 * double      ampl;      input; max. amplitude; if zero, a new random
 *                               series is started
 */
{
	/* local constants */
#   define MAXRAND 32767.0

	/* local variables */
	int      rdm;         /* integer random result */

	/* executable code */

	if  (ampl == 0.0)  {
		srand( (unsigned)clock() );
		return 0.0;
	} else {
		rdm = rand();
		return  ( (((double)rdm * 2.0) / MAXRAND - 1.0) * ampl );
	} /*endif*/

} /* end of sy_random */



/*------------------------------------------------------------------------*/



DIFFTIME sy_difftime( void )

/* returns time since last sy_difftime call
 */
{
#ifdef BC_CLOCK_T_AVAILABE
	/* local variables */
	static clock_t  diff;    /* time mark */
	clock_t         curr;    /* current time */
	clock_t         tmp;     /* scratch */

	/* executable code */

	curr = clock();
	tmp = curr - diff;
	diff = curr;
	return ((DIFFTIME)tmp/200.0);
#else
	return 0.0;
#endif
} /* end of sy_difftime */



/*------------------------------------------------------------------------*/



void sy_randomstr( int lth, char str[] )

/* creates random string of length "lth"
 *
 * parameters of routine
 * int        lth;      input; length of output string
 * char       str[];    output; random string
 */
{
	/* local variables */
#	ifdef BC_CLOCK_T_AVAILABLE
	clock_t  currtime;  /* current time */
#	else
	long     currtime;
#	endif
	char     minstr[6]; /* minimum length string */
	int      i;         /* counter */

	/* executable code */

	if  (lth < 3)  {
		*str = '\0';
		return;
	} /*endif*/
	*str++ = '$';
	lth -= 2;

#	ifdef BC_CLOCK_T_AVAILABLE
	currtime = clock();
#	else
	currtime = time( NULL );
#	endif
	currtime %= 10000L;
	sprintf( minstr, "%04d", time );
	if  (lth < 4)  {
		for  (i=0;i<lth;i++)
			str[i] = minstr[i];
	} else {
		strcpy( str, minstr );
		for  (i=4; i<lth; i++)
			str[i] = '0';
	} /*endif*/
	str[lth] = '_';
	str[lth+1] = '\0';

} /* end of sy_randomstr */



/*------------------------------------------------------------------------*/



void sy_fdelete( char file[] )

/* deletes file
 *
 * parameters of routine
 * char       file[];    input; name of file to be deleted
 */
{
	/* local variables */
	char     str[BC_FILELTH+2];   /* copy of file name */
	$DESCRIPTOR( fd, " " );

	/* executable code */

	strncpy( str, file, BC_FILELTH );
	strcat( str, ";*" );
	fd.dsc$w_length = strlen( str );
	fd.dsc$a_pointer = str;
	lib$delete_file( &fd );

} /* end of sy_fdelete */



/*----------------------------------------------------------------------------*/



void sy_frename( char from[], char to[] )

/* renames filename from "from" to "to"
 *
 * parameters of routine
 * char       from[];      input; old filename
 * char       to[];        input; new filename
 */
{
	/* local variables */
	$DESCRIPTOR( src, " " );
	$DESCRIPTOR( dst, " " );
	int      systat;

	/* executable code */

	src.dsc$w_length = strlen( from );
	src.dsc$a_pointer = from;
	dst.dsc$w_length = strlen( to );
	dst.dsc$a_pointer = to;
	systat = lib$rename_file( &src, &dst );
	if  (systat != 1)  {
		printf( "\n-> rename %s to %s failed\n", from, to );
		exit( 1 );
	} /*endif*/

} /* end of sy_frename */



/*------------------------------------------------------------------------*/



void sy_localinf( char item[], char value[], STATUS *status )

/* returns local info in "value"
 *
 * parameters of routine
 * char       item[];     input; info item
 * char       value[];    output; return value
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];
	$DESCRIPTOR( str_dsc, str );
	char     volname[BC_LINELTH+1];     /* name of volume */
	$DESCRIPTOR( volname_dsc, volname );
	int      i;                         /* integer */

	/* executable code */

	if  (strncmp(item,"GET_VOLNAME:",12) == 0)  {
		strcpy( str, item+12 );  /* get device name */
		i = strlen( str ) - 1;
		if  (str[i] != ':')  strcat( str, ":" );
		i = DVI$_VOLNAM;
		volname_dsc.dsc$w_length = BC_FILELTH;
		lib$getdvi( &i, NULL, &str_dsc, NULL, &volname_dsc, NULL );
		/* printf( "--> lth: %d\n", strlen(volname) ); */
		/* printf( "--> str: >%s<\n", volname ); */
		if  (strlen(volname) == BC_LINELTH+1)  {
			volname[BC_LINELTH] = '\0';
		} else if  (strlen(volname) > BC_LINELTH)  {
			printf( "*** *** string overflow happened *** ***\n" );
			*status = SYE_STROVFL;
			return;
		} /*endif*/
		/* remove blanks */
		i = strlen( volname ) - 1;
		if  (i >= BC_FILELTH)
			i = BC_FILELTH-1;
		while  (volname[i] == ' ')
			i--;
		if  (i >= -1 && i <= BC_LINELTH)
			volname[i+1] = '\0';
		strcpy( value, volname );
	} else {
		*status = SYE_LOCALINF;
	} /*endif*/

} /* end of sy_localinf */



/*----------------------------------------------------------------------------*/


#endif /* BC_VAX */



/******************************************************************
 ***                         SUN version                        ***
 ******************************************************************/



#ifdef BC_SUN



/* SUN version 1, 18-JUN-91
 * originally implemented in Dublin, DIAS
 * K. Stammler, 27-FEB-1990
 */

#include <stdio.h>
#include <string.h>
#ifdef BC_STDLIB_EX
#include <stdlib.h>
#endif
#include <sys/types.h>
#include <sys/times.h>
#include "sysbase.h"
#include "syerrors.h"
/* #include "shvars.h" */

#define MAXFFCMDLTH 300

/*------------------------------------------------------------------------*/



void *sy_allocmem( long cnt, int size, STATUS *status )

/* allocates memory ("cnt" objects of size "size)
 *
 * parameters of routine
 * long     cnt;            input; number of objects
 * int      size;           input; size of each object
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	void     *ptr;     /* pointer to allocated memory */

	/* executable code */

	ptr = (void *)malloc( cnt*size );
	if  (ptr == NULL)  {
		printf( "--> couldn't allocate %d * %d bytes\n", cnt, size );
		*status = SYE_MEMOVFL;
	} /*endif*/
	return ptr;

} /* end of sy_allocmem */



/*------------------------------------------------------------------------*/



FILE *sy_fopen( char file[], char access[] )

/* opens file.  Backslashes "\" in the filename are converted to slashes "/"
 *
 * parameters of routine
 * char       file[];        input; filename of file to be opened
 * char       access[];      input; access string
 *                           returns pointer to file or NULL
 */
{
	/* local variables */
	char     locname[BC_FILELTH+1];     /* local filename */
	char     *cp;                       /* character pointer */

	/* executable code */

	if  (strlen(file) > BC_FILELTH)  {
		printf( "--> sy_fopen: filename too long: not modified\n" );
		return fo_fopen( file, access );
	} /*endif*/

	cp = locname;
	while  (*file != '\0')  {
		if  (*file == '\\')  {
			*cp++ = '/';
			file++;
		} else {
			*cp++ = *file++;
		} /*endif*/
	} /*endwhile*/
	*cp = '\0';

	return fo_fopen( locname, access );

} /* end of sy_fopen */



/*------------------------------------------------------------------------*/



void sy_findfile( int request, char wild[], char filename[] )

/* looks for files matching wild card string "wild". Returns filename of
 * file found (with/without directory and/or extension, depending on "request") or
 * "\0" if not found.
 *
 * parameters of routine
 * int      request;        input; what elements (name,dir,ext)
 * char     *wild;          input; wild card string
 * char     *filename;      output; next file found
 */
{
	/* local variables */
	static char   currwild[BC_FILELTH+1]="";  /* current wild card string */
	static char   dirfile[BC_FILELTH+1]="";   /* output filename */
	static char   errfile[BC_FILELTH+1]="";   /* error output filename */
	static FILE   *df=NULL;                     /* directory file */
	char          cmd[MAXFFCMDLTH+1];      /* command string */
	int           filpos;                  /* position of file name */
	int           extpos;                  /* position of extension */
	char          *c;                      /* moving pointer */
	static int    callcnt=0;               /* call counter */

	/* executable code */

	if  (*dirfile == '\0')  {
		c = getenv( "SH_SCRATCH" );
		if  (c == NULL)  c = getenv( "HOME" );
		if  (c != NULL && strlen(c) < BC_FILELTH-11)  {
			strcpy( dirfile, c );
			filpos = (int)strlen( dirfile ) - 1;
			if  (dirfile[filpos] != '/')
				strcat( dirfile, "/" );
		} else {
			*dirfile = '\0';
		} /*endif*/
		sprintf( dirfile+strlen(dirfile), "%03d", ++callcnt );
		strcpy( errfile, dirfile );
		strcat( dirfile, "DIR.TMP" );
		strcat( errfile, "ERR.TMP" );
	} /*endif*/

	if  (*wild == '\0')  {
		*currwild = '\0';
		if  (df != NULL)  fclose( df );
		df = NULL;
		return;
	} /*endif*/

	if  (strcmp(currwild,wild) != 0)  {  /* new wild card string */
		if  (df != NULL)  fclose( df );
		strcpy( currwild, wild );
		strcpy( cmd, "ls -1 " );
		strcat( cmd, wild );
		strcat( cmd, " >" );
		strcat( cmd, dirfile );
		strcat( cmd, " 2>" );
		strcat( cmd, errfile );
		system( cmd );
		df = fopen( dirfile, "r" );
		if  (df == NULL)  {
			*currwild = '\0';
			*filename = '\0';
			return;
		} /*endif*/
	} /*endif*/

	if  (fgets(filename,BC_FILELTH,df) == NULL) {
		if  (df != NULL)  fclose( df );
		df = NULL;
		*currwild = '\0';
		*filename = '\0';
		return;
	} /*endif*/

	filpos = strlen( filename ) - 1;
	if  (filename[filpos] == '\n')  filename[filpos] = '\0';

	if  (request == (SYC_FF_NAME|SYC_FF_EXT|SYC_FF_DIR))  return;

	filpos = extpos = 0;
	c = filename;
	while  (*c != '\0')  {
		if  (*c == '.')  {
			extpos = c-filename;
		} else if (*c == '/')  {
			filpos = c-filename;
		} /*endif*/
		c++;
	} /*endwhile*/

	if  (!(SYC_FF_EXT & request)  &&  extpos > 0)
		filename[extpos] = '\0';

	if  (!(SYC_FF_DIR & request)  &&  filpos > 0)
		strcpy( filename, filename+filpos+1 );

} /* end of sy_findfile */



/*------------------------------------------------------------------------*/



double sy_random( double ampl )

/* creates random number with absolute value less than (or equal to)
 * "amp"
 *
 * parameters of routine
 * double      ampl;      input; max. amplitude; if zero, a new random
 *                               series is started
 */
{
	/* local constants */
#	define MAXRAND 32767.0 

	/* local variables */
	int      rdm;         /* integer random result */

	/* executable code */

#ifdef XXX
	if  (ampl == 0.0)  {
		srand( (unsigned)clock() );
		return 0.0;
	} else {
		rdm = rand();
		return  ( (((double)rdm * 2.0) / MAXRAND - 1.0) * ampl );
	} /*endif*/
#endif

	return (drand48()*ampl);

} /* end of sy_random */



/*------------------------------------------------------------------------*/



void sy_randomstr( int lth, char str[] )

/* creates random string of length "lth"
 *
 * parameters of routine
 * int        lth;      input; length of output string
 * char       str[];    output; random string
 */
{
	/* local variables */
	time_t  timev;      /* current time */
#	ifndef cBc_OS_AIX
	struct tms ts;
#	endif
	char     minstr[6]; /* minimum length string */
	int      i;         /* counter */

	/* executable code */

	if  (lth < 3)  {
		*str = '\0';
		return;
	} /*endif*/
	*str++ = '_';
	lth -= 2;

#	ifndef cBc_OS_AIX
	times(&ts);
	timev = ts.tms_utime;
	timev %= 10000;
#	else
	timev = 4786;  /* o.k., not very sophisticated ... */
#	endif
	sprintf( minstr, "%04d", timev );
	if  (lth < 4)  {
		for  (i=0;i<lth;i++)
			str[i] = minstr[i];
	} else {
		strcpy( str, minstr );
		for  (i=4; i<lth; i++)
			str[i] = '0';
	} /*endif*/
	str[lth] = '_';
	str[lth+1] = '\0';

} /* end of sy_randomstr */



/*------------------------------------------------------------------------*/



void sy_localinf( char item[], char value[], STATUS *status )

/* returns local info in "value"
 *
 * parameters of routine
 * char       item[];     input; info item
 * char       value[];    output; return value
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	char     *eptr;

	/* executable code */

	if  (strncmp(item,"GETENV:",7) == 0)  {
		eptr = getenv( item+7 );
		if  (eptr == NULL)  {
			*value = '\0';
			return;
		} /*endif*/
		if  (strlen(eptr) > BC_LINELTH)  {
			*status = SYE_STROVFL;
			*value = '\0';
			return;
		} /*endif*/
		strcpy( value, eptr );
	} else {
		*status = SYE_LOCALINF;
	} /*endif*/

} /* end of sy_localinf */



/*----------------------------------------------------------------------------*/



void sy_byteorder( char bo[5] )

/* parameters of routine
 *
 * int        bo[5]; output; byte order in longword, like 1,2,3,4
 */
{
	/* local variables */
	char     fname[cBcFileLth+1];     /* name of scratch output file */
	char     *env;                    /* pointer to environment value */
	char     rstr[cBcShortStrLth+1];  /* random string */
	FILE     *fp;                     /* pointer to output file */
	long     lw;                      /* longword */
	int      i;                       /* counter */

	/* executable code */

	/* find output file */
	sy_randomstr( 4, rstr );
	env = (char *)getenv( "HOME" );
	if  (env == NULL)  {
		strcpy( fname, "/tmp" );
	} else {
		strcpy( fname, env );
	} /*endif*/
	strcat( fname, "/" );
	strcat( fname, "bo_" );
	strcat( fname, rstr );
	strcat( fname, ".000" );

	fp = sy_fopen( fname, "w" );
	if  (fp == NULL)  {
		strcpy( bo, "0000" );
		return;
	} /*endif*/

	lw = (0x31 << 24) + (0x32 << 16) + (0x33 << 8) + 0x34;
	fwrite( &lw, 4, 1, fp );

	sy_fclose( fp );

	fp = sy_fopen( fname, "r" );
	if  (fp == NULL)  {
		strcpy( bo, "0000" );
		return;
	} /*endif*/

	for  (i=0; i<4; i++)
		bo[i] = fgetc( fp );
	bo[5] = '\0';

	sy_fclose( fp );

	sy_fdelete( fname );

} /* sy_byteorder */



/*----------------------------------------------------------------------------*/



#endif /* BC_SUN */



/******************************************************************
 ***                              END                           ***
 ******************************************************************/

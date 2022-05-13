
/* file SYSBASE.H
 *      =========
 *
 * version 24, 22-May-2006
 *
 * v 21: 28-Nov-94, K. Stammler: new naming conventions on sun
 *
 * machine dependent definitions
 * this file conatins all available implementations
 * K. Stammler, 2-MAY-90
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


#ifndef __SYSBASE
#define __SYSBASE

#ifndef __BASECNST
#include "basecnst.h"
#endif



/******************************************************************
 ***                        ATARI version                       ***
 ******************************************************************/



#ifdef BC_ATARI


/* one byte integer */
#define BYTE char

/* boolean type */
#define BOOLEAN int

/* boolean values TRUE & FALSE */
#ifdef BC_DEFINE_TRUE_FALSE
#define TRUE (1)
#define FALSE (0)
#endif

/* infinite loop declaration   */
#define FOREVER for(;;)

/* status type */
typedef int STATUS;    /* status value */
#define Severe(s) (*(s) != 0)

/* nearest integer number to floating number */
#define Nint(x) (int)(((x)>0)?(x)+0.5:(x)-0.5)
#define Nint32(x) (int)(((x)>0)?(x)+0.5:(x)-0.5)

/* nearest long number to floating number */
#define Nlong(x) (long)(((x)>0)?(x)+0.5:(x)-0.5)

/* capitalize character */
#define Cap(c) (((c)>='a' && (c)<='z') ? ((c)-32) : (c))
#define Uncap(c) (((c)>='A' && (c)<='Z') ? ((c)+32) : (c))

/* NULL address */
/* #define NULL ( ( void * ) 0L ) */

/* absolute value of number */
#define Abs(x) ((x)<0?-(x):(x))

/* sy_findfile parameter */
#define SYC_FF_NAME 1
#define SYC_FF_DIR 2
#define SYC_FF_EXT 4

/* binary file type, uses GEMDOS file handle, because of mistake in FILE */
typedef int BFILE;

/* open existing file for writing, is not yet used */
#define SYC_OPEN_OVWR "w"

/* read logical name table */
#define sy_lognames(f,s) fo_readtable(f,s)

/* open text file */
#define sy_fopen(f,a) fo_fopen(f,a)

/* close text file */
#define sy_fclose(f) fclose(f)

/* read from text file, is not yet used */
#define sy_fread fread

/* write to text file, is not yet used */
#define sy_fwrite fwrite

/* binary file operation failed */
#define sy_fbfailed(f) ((f)<0)

/* delete file                   */
/* call: sy_fdelete( filename )  */
/*       char   *filename;  input; file to be deleted */
/* #define sy_fdelete(a) Fdelete(a) */
/* int Fdelete( const char fname[] ); */
/* is now implemented as routine: */
void sy_fdelete( char fname[] );

/* rename file                                     */
/* call: sy_frename( from, to )                    */
/*       char   *from;   input; file to be renamed */
/*       char   *to;     input; new name of file   */
/* #define sy_frename(a,b) Frename(0,a,b) */
/* int Frename( int zero, const char oldname[], const char newname[] ); */
/* is now implemented as routine: */
void sy_frename( char from[], char to[] );

/* random string generator, not implemented */
#define sy_randomstr(l,s)


/* system constants */
/* ---------------- */

/* maximum unsigned */
#define SYC_MAXUNSG 0xffffL

/* maximum integer */
#define SYC_MAXINT 0x7fffL

/* minimum integer */
#define SYC_MININT 0x8000L

/* maximum long */
#define SYC_MAXLONG 0x7fffffffL

/* minimum long */
#define SYC_MINLONG 0x80000000L


/* system specific types */
/* --------------------- */

/* difference time, to measure time differences */
typedef float DIFFTIME;


/* one global variable: Application ID */
#ifndef __FROM_SYSCALL
extern int syv_apid;
#endif

/* include fileopen */
#ifndef __FOUSRDEF
#include BC_FOUSRDEF
#endif



/*------------------------------------------------------------------------*/
/* 					prototypes of routines of module SYSCALL.C 				  */
/*------------------------------------------------------------------------*/


void sy_initprocess( void );

/* initialises process (ATARI AES, global memory area)
 *
 * no parameters
 */


/*------------------------------------------------------------------------*/


#ifdef BC_SHARE_CPU


/*------------------------------------------------------------------------*/



void sy_sharecpu( void );

/* let other process get some CPU time (call to evnt_timer)
 *
 * no parameters
 */


/*------------------------------------------------------------------------*/


void sy_debuginfo( char info[] );

/* transfers debug information
 *
 * parameters of routine
 * char          info[];   input; info string
 */


/*------------------------------------------------------------------------*/


char *sy_getmessage( void );

/* returns message string from external source
 *
 * no parameters
 */


/*------------------------------------------------------------------------*/


#else  /* BC_SHARE_CPU */


#define sy_sharecpu()
#define sy_debuginfo(i)
#define sy_getmessage() NULL


#endif /* BC_SHARE_CPU */


/*------------------------------------------------------------------------*/


void sy_gettime( char timstr[] );

/* returns current time in time string.  The time string needs 25
 * characters (including the termination byte)
 *
 * parameters of routine
 */


/*------------------------------------------------------------------------*/


void sy_alert( char text[] );

/* displays alert message text
 *
 * parameters of display
 * char       text[];     input; text to be displayed
 */


/*------------------------------------------------------------------------*/


void *sy_allocmem( long cnt, int size, STATUS *status );

/* allocates memory ("cnt" objects of size "size)
 *
 * parameters of routine
 * long     cnt;            input; number of objects
 * int      size;           input; size of each object
 * STATUS   *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void sy_deallocmem( void *ptr );

/* deallocates memory, allocated by sy_allocmem
 *
 * parameter of routine
 * void     *ptr;        input; pointer to be freed
 */


/*------------------------------------------------------------------------*/


long sy_fbread( void *strbuf, int size, long n, BFILE fp );

/* reads "n" objects of size "size" from binary file "fp"
 *
 * parameters of routine
 * void     *strbuf;        output; buffer to read to
 * int      size;           input; size of each object
 * long     n;              input; number of objects
 * BFILE    fp;             input; file descriptor
 */


/*------------------------------------------------------------------------*/


long sy_fbwrite( void *strbuf, int size, long n, BFILE fp );

/* writes "n" objects of size "size" to file "fp"
 *
 * parameters of routine
 * void     *strbuf;        input; buffer to read to
 * int      size;           input; size of each object
 * long     n;              input; number of objects
 * BFILE    fp;             input; file descriptor
 */


/*------------------------------------------------------------------------*/


int sy_fbseek( BFILE fp, long pos, int mode );

/* changes file position
 *
 * parameters of routine
 * BFILE      fp;    input; file pointer
 * long       pos;   input; new position
 * int        mode;  input; search mode
 */


/*------------------------------------------------------------------------*/


BFILE sy_fbopen( const char *file, const char *access );

/* opens a binary file
 *
 * parameters of routine
 * const char    *file;    input; filename
 * const char    *access;  input; access mode
 */


/*------------------------------------------------------------------------*/


int sy_fbclose( BFILE fp );

/* closes binary file
 *
 * parameter of routine
 * BFILE     fp;      input; file pointer
 */


/*------------------------------------------------------------------------*/


void sy_findfile( int request, char wild[], char filename[] );

/* looks for files matching wild card string "wild". Returns filename of
 * file found or "\0" if not found.  "request" controls the returned
 * elements.  For example the value (SYC_FF_DIR|SYC_FF_NAME|SYC_FF_EXT)
 * means, that the returned string contains directory, file name and
 * extension.
 *
 * parameters of routine
 * int      request;        input; what elements (name,dir,extension)
 * char     *wild;          input; wild card string
 * char     *filename;      output; next file found
 */


/*------------------------------------------------------------------------*/


void sy_system( char *cmd, STATUS *status );

/* call to the operating system
 *
 * parameters of routine
 * char     *cmd;           input; system command
 * STATUS   *status;        output; return status
 */


/*------------------------------------------------------------------------*/



double sy_random( double ampl );

/* creates random number with absolute value less than (or equal to)
 * "amp"
 *
 * parameters of routine
 * double		ampl;		input; max. amplitude; if zero, a new random
 * 									 series is started
 */


/*------------------------------------------------------------------------*/


DIFFTIME sy_difftime( void );

/* returns time since last sy_difftime call
 */


/*------------------------------------------------------------------------*/


void sy_localinf( char item[], char value[], STATUS *status );

/* machine dependend routine
 *
 * parameters of routine
 * char       item[];     input; info item
 * char       value[];    input/output; additional parameter
 * STATUS     *status;    output; return status
 */


/*------------------------------------------------------------------------*/



#endif /* BC_ATARI */



/******************************************************************
 ***                        IBM PC version                      ***
 ******************************************************************/



#ifdef BC_IBM



/* includes */
#include <alloc.h>

/* short integer */
#define BYTE unsigned char

/* boolean type */
#define BOOLEAN int

/* boolean values TRUE & FALSE */
#ifdef BC_DEFINE_TRUE_FALSE
#define TRUE (1)
#define FALSE (0)
#endif

/* infinite loop declaration */
#define FOREVER for(;;)

/* status type */
#ifndef STATUS
#define STATUS int    /* status value */
#define Severe(s) (*(s) != 0)
#endif /* STATUS */

/* nearest integer number to floating number */
#define Nint(x) ((x)>0 ? (int)((x)+0.5) : (int)((x)-0.5))
#define Nint32(x) ((x)>0 ? (int)((x)+0.5) : (int)((x)-0.5))

/* nearest long number to floating number */
#define Nlong(x) ((x)>0 ? (long)((x)+0.5) : (long)((x)-0.5))

/* capitalize character */
#define Cap(c) (((c)>='a' && (c)<='z') ? ((c)-32) : (c))
#define Uncap(c) (((c)>='A' && (c)<='Z') ? ((c)+32) : (c))

/* absolute value of number */
#define Abs(x) ((x)<0?-(x):(x))

/* binary file type, here the same as text files */
typedef FILE *BFILE;

/* sy_findfile parameter */
#define SYC_FF_NAME 1
#define SYC_FF_DIR 2
#define SYC_FF_EXT 4

/* deallocate memory, allocated by sy_allocmem */
#define sy_deallocmem(p) free(p)

/* open text file */
#define sy_fopen(f,a) fopen(f,a)

/* close text file */
#define sy_fclose(f) fclose(f)

/* read from text file, this is not yet used */
#define sy_fread(b,s,n,f) fread(b,s,n,f)

/* write to text file, this is not yet used */
#define sy_fwrite(b,s,n,f) fwrite(b,s,n,f)

/* read from binary file */
#define sy_fbread(b,s,n,f) fread(b,s,n,f)

/* write to binary file */
#define sy_fbwrite(b,s,n,f) fwrite(b,s,n,f)

/* seek on binary file */
#define sy_fbseek(f,p,m) fseek(f,p,m)

/* open binary file */
#define sy_fbopen(f,a) fopen(f,a)

/* close binary file */
#define sy_fbclose(f) fclose(f)

/* binary file operation failed */
#define sy_fbfailed(f) ((f)==NULL)

/* routine doing machine dependend things, not used here */
#define sy_localinf(a,b,c)

/* call operating system */
#define sy_system(c,s) system(c)

/* delete file */
#define sy_fdelete(f) unlink(f)

/* rename file */
#define sy_frename(f,t) rename(f,t)

/* get time differences in sec, not implemented here */
#define sy_difftime() 1.0

/* system constants */
/* ---------------- */

/* maximum unsigned */
#define SYC_MAXUNSG 0xffffffffL

/* maximum integer */
#define SYC_MAXINT 0x7fffffffL

/* minimum integer */
#define SYC_MININT 0x80000000L

/* maximum long */
#define SYC_MAXLONG 0x7fffffffL

/* minimum long */
#define SYC_MINLONG 0x80000000L

/* open existing file to overwrite, not yet used */
#define SYC_OPEN_OVWR "r+"

/* system specific types */
/* --------------------- */

/* difference time, to measure difference times */
typedef float DIFFTIME;


#define sy_alert(t) printf("%s",t)
#define sy_debuginfo(l)
#define sy_initprocess()
#define sy_sharecpu()
#define sy_gettime(t)
#define sy_getmessage() NULL


/*------------------------------------------------------------------------*/
/*    prototypes of routines of module SYSCALL.C                          */
/*------------------------------------------------------------------------*/


void *sy_allocmem( long cnt, int size, STATUS *status );

/* allocates memory ("cnt" objects of size "size)
 *
 * parameters of routine
 * long     cnt;            input; number of objects
 * int      size;           input; size of each object
 * STATUS   *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void sy_findfile( int request, char wild[], char filename[] );

/* looks for files matching wild card string "wild". Returns filename of
 * file found or "\0" if not found.  "request" controls the returned
 * elements.  For example the value (SYC_FF_DIR|SYC_FF_NAME|SYC_FF_EXT)
 * means, that the returned string contains directory, file name and
 * extension.
 *
 * parameters of routine
 * int      request;        input; what elements (name,dir,extension)
 * char     *wild;          input; wild card string
 * char     *filename;      output; next file found
 */


/*------------------------------------------------------------------------*/


double sy_random( double ampl );

/* creates random number with absolute value less than (or equal to)
 * "amp"
 *
 * parameters of routine
 * double		ampl;		input; max. amplitude; if zero, a new random
 * 									 series is started
 */


/*------------------------------------------------------------------------*/


void sy_randomstr( int lth, char str[] );

/* creates random string of length "lth"
 *
 * parameters of routine
 * int        lth;      input; length of output string
 * char       str[];    output; random string
 */


/*------------------------------------------------------------------------*/



#endif /* BC_IBM */



/******************************************************************
 ***                         VAX version                        ***
 ******************************************************************/



#ifdef BC_VAX



/* short integer */
#define BYTE char

/* boolean type */
#define BOOLEAN int

/* boolean values TRUE & FALSE */
#ifdef BC_DEFINE_TRUE_FALSE
#define TRUE (1)
#define FALSE (0)
#endif

/* infinite loop declaration */
#define FOREVER for(;;)

/* status type */
#ifndef STATUS
#define STATUS int
#define Severe(s) (*(s) != 0)
#endif /* STATUS */

/* nearest integer number to floating number */
#define Nint(x) ((x)>0 ? (int)((x)+0.5) : (int)((x)-0.5))
#define Nint32(x) ((x)>0 ? (int)((x)+0.5) : (int)((x)-0.5))

/* nearest long number to floating number */
#define Nlong(x) ((x)>0 ? (long)((x)+0.5) : (long)((x)-0.5))

/* capitalize character */
#define Cap(c) (((c)>='a' && (c)<='z') ? ((c)-32) : (c))
#define Uncap(c) (((c)>='A' && (c)<='Z') ? ((c)+32) : (c))

/* absolute value of number */
#define Abs(x) ((x)<0?-(x):(x))

/* read logical name table, not implemented */
#define sy_lognames(f,s)

/* open text file */
#define sy_fopen(f,a) fopen(f,a)

/* close text file */
#define sy_fclose(f) fclose(f)

/* binary file type, here the same as text file */
typedef FILE *BFILE;

/* binary file operation failed */
#define sy_fbfailed(f) ((f)==NULL)

/* sy_findfile parameter */
#define SYC_FF_NAME 1
#define SYC_FF_DIR 2
#define SYC_FF_EXT 4



/* system constants */
/* ---------------- */

/* maximum unsigned */
#define SYC_MAXUNSG 0xffffffffL

/* maximum integer */
#define SYC_MAXINT 0x7fffffffL

/* minimum integer */
#define SYC_MININT 0x80000000L

/* maximum long */
#define SYC_MAXLONG 0x7fffffffL

/* minimum long */
#define SYC_MINLONG 0x80000000L

/* open existing file to overwrite */
#define SYC_OPEN_OVWR "r+"

/* system specific types */
/* --------------------- */

/* difference time */
typedef float DIFFTIME;


/* not implemented routines */
#define sy_initprocess()
#define sy_sharecpu()
#define sy_debuginfo(s)
#define sy_alert(t) printf( "\n%s\n", t )
#define sy_getmessage() NULL
#define sy_gettime(s) strcpy(s,SHC_TIMEDEFAULT)



/*------------------------------------------------------------------------*/
/*    prototypes of routines of module SYSCALL.C                          */
/*------------------------------------------------------------------------*/


void *sy_allocmem( long cnt, int size, STATUS *status );

/* allocates memory ("cnt" objects of size "size)
 *
 * parameters of routine
 * long     cnt;            input; number of objects
 * int      size;           input; size of each object
 * STATUS   *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void sy_deallocmem( void *ptr );

/* deallocates memory
 *
 * parameter of routine
 * void     *ptr;        input; pointer to be freed
 */


/*------------------------------------------------------------------------*/


long sy_fread( void *strbuf, int size, long n, FILE *fp );

/* reads "n" objects of size "size" from file "fp"
 *
 * parameters of routine
 * void     *strbuf;        output; buffer to read to
 * int      size;           input; size of each object
 * long     n;              input; number of objects
 * FILE     *fp;            input; file descriptor
 */


/*------------------------------------------------------------------------*/


long sy_fwrite( void *strbuf, int size, long n, FILE *fp );

/* writes "n" objects of size "size" to file "fp"
 *
 * parameters of routine
 * void     *strbuf;        input; buffer to read to
 * int      size;           input; size of each object
 * long     n;              input; number of objects
 * FILE     *fp;            input; file descriptor
 */


/*------------------------------------------------------------------------*/


long sy_fbread( void *strbuf, int size, long n, BFILE fp );

/* reads "n" objects of size "size" from binary file "fp"
 *
 * parameters of routine
 * void     *strbuf;        output; buffer to read to
 * int      size;           input; size of each object
 * long     n;              input; number of objects
 * BFILE    fp;             input; file descriptor
 */


/*------------------------------------------------------------------------*/


long sy_fbwrite( void *strbuf, int size, long n, BFILE fp );

/* writes "n" objects of size "size" to binary file "fp"
 *
 * parameters of routine
 * void     *strbuf;        input; buffer to read to
 * int      size;           input; size of each object
 * long     n;              input; number of objects
 * BFILE    fp;             input; file descriptor
 */


/*------------------------------------------------------------------------*/


int sy_fbseek( BFILE fp, long pos, int mode );

/* changes read/write position in binary file
 *
 * parameters of routine
 * BFILE      fp;    input; file pointer
 * long       pos;   input; new position
 * int        mode;  input; search mode
 */


/*------------------------------------------------------------------------*/


BFILE sy_fbopen( const char *file, const char *access );

/* opens a binary file
 *
 * parameters of routine
 * const char    *file;    input; filename
 * const char    *access;  input; access mode
 */


/*------------------------------------------------------------------------*/


int sy_fbclose( BFILE fp );

/* closes binary file
 *
 * parameter of routine
 * BFILE     fp;      input; file pointer
 */


/*------------------------------------------------------------------------*/


void sy_findfile( int request, char wild[], char filename[] );

/* looks for files matching wild card string "wild". Returns filename of
 * file found or "\0" if not found.  "request" controls the returned
 * elements.  For example the value (SYC_FF_DIR|SYC_FF_NAME|SYC_FF_EXT)
 * means, that the returned string contains directory, file name and
 * extension.
 *
 * parameters of routine
 * int      request;        input; what elements (name,dir,extension)
 * char     *wild;          input; wild card string
 * char     *filename;      output; next file found
 */


/*------------------------------------------------------------------------*/


void sy_system( char *cmd, STATUS *status );

/* call to the operating system
 *
 * parameters of routine
 * char     *cmd;           input; system command
 * STATUS   *status;        output; return status
 */


/*------------------------------------------------------------------------*/



double sy_random( double ampl );

/* creates random number with absolute value less than (or equal to)
 * "amp"
 *
 * parameters of routine
 * double		ampl;		input; max. amplitude; if zero, a new random
 * 									 series is started
 */


/*------------------------------------------------------------------------*/


DIFFTIME sy_difftime( void );

/* returns time since last sy_difftime call
 */


/*------------------------------------------------------------------------*/


void sy_randomstr( int lth, char str[] );

/* creates random string of length "lth"
 *
 * parameters of routine
 * int        lth;      input; length of output string
 * char       str[];    output; random string
 */


/*------------------------------------------------------------------------*/


void sy_fdelete( char file[] );

/* deletes file
 *
 * parameters of routine
 * char       file[];    input; name of file to be deleted
 */


/*----------------------------------------------------------------------------*/


void sy_frename( char from[], char to[] );

/* renames filename from "from" to "to"
 *
 * parameters of routine
 * char       from[];      input; old filename
 * char       to[];        input; new filename
 */


/*------------------------------------------------------------------------*/


void sy_localinf( char item[], char value[], STATUS *status );

/* returns local info in "value"
 *
 * parameters of routine
 * char       item[];     input; info item
 * char       value[];    output; return value
 * STATUS     *status;    output; return status
 */


/*------------------------------------------------------------------------*/



#endif /* BC_VAX */



/******************************************************************
 ***                         SUN version                        ***
 ******************************************************************/



#ifdef BC_SUN



/* short integer */
#define BYTE unsigned char
#define TSyByte BYTE
#define TSyWord short int
#define TSyUword unsigned short int

/* boolean type */
#define BOOLEAN int
#define TSyBoolean BOOLEAN

/* boolean values TRUE & FALSE */
#ifdef BC_DEFINE_TRUE_FALSE
#define TRUE (1)
#define FALSE (0)
#endif
#define cSyTrue (1)
#define cSyFalse (0)

/* infinite loop declaration */
#define FOREVER for(;;)
#define TSyForever FOREVER

/* status type */
#ifndef STATUS
#define STATUS int
#define Severe(s) (*(s) != 0)
#define TSyStatus STATUS
#define SySevere(s) (*(s) != 0)
#endif /* TSyStatus */

/* nearest integer number to floating number */
#define Nint(x) ((x)>0 ? (int)((x)+0.5) : (int)((x)-0.5))
#define Nint32(x) ((x)>0 ? (int)((x)+0.5) : (int)((x)-0.5))
#define SyNint(x) ((x)>0 ? (int)((x)+0.5) : (int)((x)-0.5))

/* nearest long number to floating number */
#define Nlong(x) ((x)>0 ? (long)((x)+0.5) : (long)((x)-0.5))
#define SyNlong(x) ((x)>0 ? (long)((x)+0.5) : (long)((x)-0.5))

/* capitalize character */
#define Cap(c) (((c)>='a' && (c)<='z') ? ((c)-32) : (c))
#define SyCap(c) (((c)>='a' && (c)<='z') ? ((c)-32) : (c))
#define Uncap(c) (((c)>='A' && (c)<='Z') ? ((c)+32) : (c))
#define SyUncap(c) (((c)>='A' && (c)<='Z') ? ((c)+32) : (c))

/* absolute value of number */
#define Abs(x) ((x)<0?-(x):(x))
#define SyAbs(x) ((x)<0?-(x):(x))

/* binary file type, here the same as text file */
typedef FILE *BFILE;
#define TSyBfile BFILE

/* sy_findfile parameter */
#define SYC_FF_NAME 1
#define SYC_FF_DIR 2
#define SYC_FF_EXT 4
#define cSyFfName 1
#define cSyFfDir 2
#define cSyFfExt 4

/* deallocate memory */
#define sy_deallocmem(p) free(p)
#define SyDeallocMem(p) free(p)

/* read logical name table */
#define sy_lognames(f,s) fo_readtable(f,s)
#define SyLogNames(f,s) fo_readtable(f,s)

/* open text file */
/* #define sy_fopen(f,a) fo_fopen(f,a) */
/* routine implemented for it */

/* close text file */
#define sy_fclose(f) fclose(f)
#define SyFclose(f) fclose(f)

/* read from text file */
#define sy_fread(b,s,n,f) fread(b,s,n,f)
#define SyFread(b,s,n,f) fread(b,s,n,f)

/* write to text file */
#define sy_fwrite(b,s,n,f) fwrite(b,s,n,f)
#define SyFwrite(b,s,n,f) fwrite(b,s,n,f)

/* read from binary file */
#define sy_fbread(b,s,n,f) fread(b,s,n,f)
#define SyFbread(b,s,n,f) fread(b,s,n,f)

/* write to binary file */
#define sy_fbwrite(b,s,n,f) fwrite(b,s,n,f)
#define SyFbwrite(b,s,n,f) fwrite(b,s,n,f)

/* seek on binary file */
#define sy_fbseek(f,p,m) fseek(f,p,m)
#define SyFbseek(f,p,m) fseek(f,p,m)

/* open binary file */
#define sy_fbopen(f,a) sy_fopen(f,a)
#define SyFbopen(f,a) sy_fopen(f,a)

/* close binary file */
#define sy_fbclose(f) fclose(f)
#define SyFbclose(f) fclose(f)

/* binary file operation failed */
#define sy_fbfailed(f) ((f)==NULL)
#define SyFbfailed(f) ((f)==NULL)

/* call to operating system */
#define sy_system(c,s) system(c)
#define SySystem(c,s) system(c)

/* delete file */
#define sy_fdelete(f) unlink(f)
#define SyFdelete(f) unlink(f)

/* rename file */
#define sy_frename(f,t) rename(f,t)
#define SyFrename(f,t) rename(f,t)

/* difference time, not implemented */
#define sy_difftime() 1.0
#define SyDifftime() 1.0


/* system constants */
/* ---------------- */

/* maximum unsigned */
#define SYC_MAXUNSG 0xffffffffL
#define cSyMaxUnsg SYC_MAXUNSG

/* maximum integer */
#define SYC_MAXINT 0x7fffffffL
#define cSyMaxInt SYC_MAXINT

/* minimum integer */
#define SYC_MININT 0x80000000L
#define cSyMinInt SYC_MININT

/* maximum long */
#define SYC_MAXLONG 0x7fffffffL
#define cSyMaxLong SYC_MAXLONG

/* minimum long */
#define SYC_MINLONG 0x80000000L
#define cSyMinLong SYC_MINLONG

/* open existing file to overwrite */
#define SYC_OPEN_OVWR "r+"
#define cSyOpenOvwr SYC_OPEN_OVWR


/* system specific types */
/* --------------------- */

/* difference time */
typedef float DIFFTIME;
typedef float TSyDifftime;


/* not implemented routines */
#define sy_gettime(s) strcpy(s,SHC_TIMEDEFAULT)
#define SyGetTime(s) strcpy(s,SHC_TIMEDEFAULT)
#define sy_sharecpu()
#define SyShareCpu()
#define sy_alert(t) printf("%s",t)
#define SyAlert(t) printf("%s",t)
#define sy_initprocess()
#define SyInitProcess()


/* include fileopen */
#ifndef __FOUSRDEF
#include BC_FOUSRDEF
#endif


/*------------------------------------------------------------------------*/
/*    prototypes of routines of module SYSCALL.C                          */
/*------------------------------------------------------------------------*/


#define SyAllocMem sy_allocmem

void *sy_allocmem( long cnt, int size, STATUS *status );

/* allocates memory ("cnt" objects of size "size)
 *
 * parameters of routine
 * long     cnt;            input; number of objects
 * int      size;           input; size of each object
 * STATUS   *status;        output; return status
 */


/*------------------------------------------------------------------------*/


#define SyFindFile sy_findfile

void sy_findfile( int request, char wild[], char filename[] );

/* looks for files matching wild card string "wild". Returns filename of
 * file found or "\0" if not found.  "request" controls the returned
 * elements.  For example the value (SYC_FF_DIR|SYC_FF_NAME|SYC_FF_EXT)
 * means, that the returned string contains directory, file name and
 * extension.
 *
 * parameters of routine
 * int      request;        input; what elements (name,dir,extension)
 * char     *wild;          input; wild card string
 * char     *filename;      output; next file found
 */


/*------------------------------------------------------------------------*/


#define SyFopen sy_fopen

FILE *sy_fopen( char file[], char access[] );

/* opens file.  Backslashes "\" in the filename are converted to slashes "/"
 *
 * parameters of routine
 * char       file[];        input; filename of file to be opened
 * char       access[];      input; access string
 *                           returns pointer to file or NULL
 */


/*------------------------------------------------------------------------*/


#define SyRandom sy_random

double sy_random( double ampl );

/* creates random number with absolute value less than (or equal to)
 * "amp"
 *
 * parameters of routine
 * double		ampl;		input; max. amplitude; if zero, a new random
 * 									 series is started
 */


/*------------------------------------------------------------------------*/


#define SyRandomStr sy_randomstr

void sy_randomstr( int lth, char str[] );

/* creates random string of length "lth"
 *
 * parameters of routine
 * int        lth;      input; length of output string
 * char       str[];    output; random string
 */


/*------------------------------------------------------------------------*/


#define SyLocalInf sy_localinf

void sy_localinf( char item[], char value[], STATUS *status );

/* returns local info in "value"
 *
 * parameters of routine
 * char       item[];     input; info item
 * char       value[];    output; return value
 * STATUS     *status;    output; return status
 */


/*------------------------------------------------------------------------*/



#endif /* BC_SUN */



/******************************************************************
 ***                              END                           ***
 ******************************************************************/

#endif /* __SYSBASE */


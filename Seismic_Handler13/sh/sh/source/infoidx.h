
/* file INFOIDX.H
 *      =========
 *
 * version 16, 22-May-2006
 *
 * entry constants and prototypes of module SHDATABA
 * K. Stammler, 22-MAY-91
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


#ifndef __INFOIDX
#define __INFOIDX

#ifndef __SHCONST
#include "shconst.h"
#endif
#ifndef __TCUSRDEF
#include "tcusrdef.h"
#endif
#ifndef __GCUSRDEF
#include BC_GCUSRDEF
#endif

/* infos stored in memory */
#ifdef XXX
/* old values up to 15-Mar-2006 */
#define EMAX_LONG  5
#define EMAX_INT   6
#define EMAX_BYTE  2
#define EMAX_REAL  12
#define EMAX_STR   4
#define EMAX_CHAR  4
#define EMAX_TIME  3
#define EMAX_PTR   8
#define EMAX_FLAG  16
#endif

#define EMAX_LONG  30
#define EMAX_INT   30
#define EMAX_BYTE  20
#define EMAX_REAL  60
#define EMAX_STR   20
#define EMAX_CHAR  40
#define EMAX_TIME  15
#define EMAX_PTR   8
#define EMAX_FLAG  16

/* info not read from file */
#ifdef XXX
/* old values up to 15-Mar-2006 */
#define ENF_LONG   2
#define ENF_INT    2
#define ENF_BYTE   0
#define ENF_REAL   5
#define ENF_STR    3
#define ENF_CHAR   3
#define ENF_TIME   3
#define ENF_PTR    0
#define ENF_FLAG   6
#endif

#define ENF_LONG   27
#define ENF_INT    26
#define ENF_BYTE   20
#define ENF_REAL   53
#define ENF_STR    19
#define ENF_CHAR   39
#define ENF_TIME   15
#define ENF_PTR    0
#define ENF_FLAG   4

#define E_IDXMASK 0x03ff
#define E_TYPMASK 0xf000
#define E_BNOCOPY 0x0800
#define E_RDONLY  0x0400

#define EL_TYPE    0x1000
#define EI_TYPE    0x2000
#define EB_TYPE    0x6000
#define ER_TYPE    0x3000
#define ES_TYPE    0x4000
#define EC_TYPE    0x5000
#define ET_TYPE    0x7000
#define EP_TYPE    0x8000
#define EF_TYPE    0x9000
#define EX_TYPE    0xa000

#define E_EOLIST   0
#define E_NEXTENTRY 0
#define E_NONE 0

#define EL_LENGTH   (1|EL_TYPE|E_BNOCOPY|E_RDONLY)
#define EL_ALLOC    ((ENF_LONG+0)|EL_TYPE|E_BNOCOPY|E_RDONLY)
#define EL_DSPFST   ((ENF_LONG+1)|EL_TYPE|E_BNOCOPY)
#define EL_DSPCNT   ((ENF_LONG+2)|EL_TYPE|E_BNOCOPY)

#define EI_RECNO    ((ENF_INT+0)|EI_TYPE|E_RDONLY)
#define EI_ATTRIB   ((ENF_INT+1)|EI_TYPE)
#define EI_REDUCT   ((ENF_INT+2)|EI_TYPE)

#define ER_DELTA    (0|ER_TYPE|E_BNOCOPY|E_RDONLY)
#define ER_CALIB    (1|ER_TYPE|E_BNOCOPY|E_RDONLY)
	/* the CALIB entry is defined in SHSTRTUP.SHC not in shdataba.c */
#define ER_MAXVAL   ((ENF_REAL+0)|ER_TYPE|E_BNOCOPY|E_RDONLY)
#define ER_MINVAL   ((ENF_REAL+1)|ER_TYPE|E_BNOCOPY|E_RDONLY)
#define ER_NORM     ((ENF_REAL+2)|ER_TYPE|E_BNOCOPY)
#define ER_ZOOM     ((ENF_REAL+3)|ER_TYPE)
#define ER_TORIG    ((ENF_REAL+4)|ER_TYPE)
#define ER_SORIG    ((ENF_REAL+5)|ER_TYPE|E_BNOCOPY)
#define ER_WEIGHT   ((ENF_REAL+6)|ER_TYPE)

#define ES_COMMENT  (0|ES_TYPE)
#define ES_STATION  (1|ES_TYPE)
#define ES_OPINFO   (2|ES_TYPE|E_BNOCOPY|E_RDONLY)
#define ES_FILE     ((ENF_STR+0)|ES_TYPE|E_RDONLY)

#define EC_COMP     (0|EC_TYPE)
#define EC_CHAN1    (1|EC_TYPE)
#define EC_CHAN2    (2|EC_TYPE)

#define ET_START    (0|ET_TYPE)

#define EP_NEXT     ((ENF_PTR+0)|EP_TYPE|E_BNOCOPY|E_RDONLY)
#define EP_PREV     ((ENF_PTR+1)|EP_TYPE|E_BNOCOPY|E_RDONLY)
#define EP_DSPN     ((ENF_PTR+2)|EP_TYPE|E_BNOCOPY|E_RDONLY)
#define EP_DSPP     ((ENF_PTR+3)|EP_TYPE|E_BNOCOPY|E_RDONLY)
#define EP_DATA     ((ENF_PTR+4)|EP_TYPE|E_BNOCOPY|E_RDONLY)
#define EP_USR1     ((ENF_PTR+5)|EP_TYPE|E_BNOCOPY|E_RDONLY)
#define EP_USR2     ((ENF_PTR+6)|EP_TYPE|E_BNOCOPY|E_RDONLY)
#define EP_STAT     ((ENF_PTR+7)|EP_TYPE|E_RDONLY)

#define EF_QUAL     ((ENF_FLAG+0)|EF_TYPE|E_RDONLY)
#define EF_NOCALIB  ((ENF_FLAG+1)|EF_TYPE|E_RDONLY)
#define EF_MODIF    ((ENF_FLAG+2)|EF_TYPE|E_BNOCOPY|E_RDONLY)
#define EF_FROMQ    ((ENF_FLAG+3)|EF_TYPE|E_RDONLY)
#define EF_OVERLAY  ((ENF_FLAG+4)|EF_TYPE|E_BNOCOPY|E_RDONLY)
#define EF_AMPLICUT ((ENF_FLAG+5)|EF_TYPE)
#define EF_USFLG1   ((ENF_FLAG+6)|EF_TYPE|E_BNOCOPY)
#define EF_USFLG2   ((ENF_FLAG+7)|EF_TYPE|E_BNOCOPY)

#define EX_DATE     (0|EX_TYPE)
#define EX_TIME     (1|EX_TYPE)

#define EPN_NEXT    0
#define EPN_PREV    1
#define EPN_DSPN    2
#define EPN_DSPP    3
#define EPN_DATA    4

#define MSTRLTH     131

typedef struct memblc {
   long      lm[EMAX_LONG];
   int       im[EMAX_INT];
   BYTE      bm[EMAX_BYTE];
   REAL      rm[EMAX_REAL];
   TIME      tm[EMAX_TIME];
   char      sm[EMAX_STR][MSTRLTH+1];
   char      cm[EMAX_CHAR];
   void	    *pm[EMAX_PTR];
   int       fm;
} MEMBLC;


/* prototypes of module SHDATABA.C */
/*------------------------------------------------------------------------*/


void db_newlist( void );

/* resets list of created traces
 *
 * no parameters
 */


/*------------------------------------------------------------------------*/


MEMBLC *db_create( int *status );

/* enlists new info block */

/* parameters of routine */
/* int      *status;     output; return status */
/* 							 returns info block pointer */


/*------------------------------------------------------------------------*/


void db_enlist( MEMBLC *p, int pos );

/* enlists info block in display list */

/* parameters of routine */
/* MEMBLC   *p;             input; info block to enlist */
/* int      pos;            input; list position */


/*------------------------------------------------------------------------*/


void db_delete( MEMBLC *ptr );

/* removes info block from list */

/* parameters of routine */
/* MEMBLC   *ptr;           pointer to block to be removed */


/*------------------------------------------------------------------------*/


void db_delist( CHMAP map, MEMBLC *ptr );

/* removes info block "*ptr" from display list
 *
 * parameters of routine
 * CHMAP         map;       input; channel map
 * MEMBLC        *ptr;      input; info block to be removed
 */

/*------------------------------------------------------------------------*/


void db_createdlist( void *trclist[], int *listlth );

/* returns list of created traces
 *
 * parameters of routine
 * void       *trclist;    output; list of created traces
 * int        *listlth;    output; length of list
 */


/*------------------------------------------------------------------------*/


BOOLEAN db_islisted( CHMAP map, MEMBLC *ptr );

/* checks whether "ptr" is in the display list "dln" or not */

/* parameters of routine */
/* CHMAP        map;        input; channel map */
/* MEMBLC		*ptr;		input; trace pointer */

/*------------------------------------------------------------------------*/


BOOLEAN db_hidden( MEMBLC *ptr );

/* checks whether "ptr" is in any display list or not
 *
 * parameter of routine
 * MEMBLC		*ptr;		input; trace pointer
 */


/*------------------------------------------------------------------------*/


int db_gettrcpos( MEMBLC *ptr );

/* returns position number of "ptr" in display list of "gc".  If "trc"
 * is not found, 0 is returned
 *
 * parameters of routine
 * MEMBLC      *ptr;      input; trace pointer
 */


/*------------------------------------------------------------------------*/


int db_dsplth( CHMAP map );

/* returns number of traces in display
 *
 * parameter of routine
 * CHMAP     map;      input; channel map
 */


/*------------------------------------------------------------------------*/


int db_lstlth( void );

/* returns number of traces in memory */


/*------------------------------------------------------------------------*/


MEMBLC *db_dspfirst( CHMAP map, STATUS *status );

/* returns first trace of display list "dln"
 *
 * parameters of routine
 * CHMAP      map;       input; channel map
 * STATUS     *status;   output; return status
 */


/*------------------------------------------------------------------------*/


int db_getdln( CHMAP map );

/* returns display list number from channel map
 * parameters of routine
 * CHMAP      map;      input; channel map
 *                      returns display list number (-1 = no display list)
 */


/*------------------------------------------------------------------------*/


long db_getl( MEMBLC *ptr, unsigned ientry, int *status );

/* returns long info value */

/* parameters of routine */
/* MEMBLC   *ptr;           input; info block pointer */ 
/* unsigned ientry;         input; info entry */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


int db_geti( MEMBLC *ptr, unsigned ientry, int *status );

/* returns int info value */

/* parameters of routine */
/* MEMBLC   *ptr;           input; info block pointer */ 
/* unsigned ientry;         input; info entry */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


BYTE db_getb( MEMBLC *ptr, unsigned ientry, int *status );

/* returns byte info value */

/* parameters of routine */
/* MEMBLC   *ptr;           input; info block pointer */ 
/* unsigned ientry;         input; info entry */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


REAL db_getr( MEMBLC *ptr, unsigned ientry, int *status );

/* returns REAL info value */

/* parameters of routine */
/* MEMBLC   *ptr;           input; info block pointer */ 
/* unsigned ientry;         input; info entry */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void db_gets( MEMBLC *ptr, unsigned ientry, int maxlth, char *str, int *status );

/* returns long info value */

/* parameters of routine */
/* MEMBLC   *ptr;           input; info block pointer */ 
/* unsigned ientry;         input; info entry */
/* int		maxlth;			 input; maximum length of output string */
/* char		*str;				 output; string returned */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


char db_getc( MEMBLC *ptr, unsigned ientry, int *status );

/* returns char info value */

/* parameters of routine */
/* MEMBLC   *ptr;           input; info block pointer */ 
/* unsigned ientry;         input; info entry */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void db_gett( MEMBLC *ptr, unsigned ientry, TIME *time, int *status );

/* returns time info value */

/* parameters of routine */
/* MEMBLC   *ptr;           input; info block pointer */ 
/* unsigned ientry;         input; info entry */
/* TIME		*time;			 output; time returned */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void *db_getp( MEMBLC *ptr, unsigned ientry, int *status );

/* returns pointer info value */

/* parameters of routine */
/* MEMBLC   *ptr;           input; info block pointer */ 
/* unsigned ientry;         input; info entry */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


int db_getf( MEMBLC *ptr, unsigned ientry, int *status );

/* returns flag value */

/* parameters of routine */
/* MEMBLC   *ptr;           input; info block pointer */ 
/* unsigned ientry;         input; info entry */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void db_getx( MEMBLC *ptr, unsigned ientry, int maxlth, char str[],
	STATUS *status );

/* returns special info
 *
 * parameters of routine
 * MEMBLC   *ptr;           input; info block pointer
 * unsigned ientry;         input; info entry
 * int      maxlth;         input; maximum length of output string
 * char     str[];          output; output string
 * STATUS   *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void db_setl( MEMBLC *ptr, unsigned ientry, long value, int *status );

/* sets long value in info block "*ptr" */

/* parameters of routine */
/* MEMBLC   *ptr;           modify; info block to be changed */
/* unsigned ientry;         input; info entry number */
/* long     value;          input; new value */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void db_seti( MEMBLC *ptr, unsigned ientry, int value, int *status );

/* sets int value in info block "*ptr" */

/* parameters of routine */
/* MEMBLC   *ptr;           modify; info block to be changed */
/* unsigned ientry;         input; info entry number */
/* int      value;          input; new value */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void db_setb( MEMBLC *ptr, unsigned ientry, BYTE value, int *status );

/* sets short value in info block "*ptr" */

/* parameters of routine */
/* MEMBLC   *ptr;           modify; info block to be changed */
/* unsigned ientry;         input; info entry number */
/* BYTE     value;          input; new value */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void db_setr( MEMBLC *ptr, unsigned ientry, REAL value, int *status );

/* sets real value in info block "*ptr" */

/* parameters of routine */
/* MEMBLC   *ptr;           modify; info block to be changed */
/* unsigned ientry;         input; info entry number */
/* REAL     value;          input; new value */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void db_sets( MEMBLC *ptr, unsigned ientry, char *value, int *status );

/* sets string value in info block "*ptr" */

/* parameters of routine */
/* MEMBLC   *ptr;           modify; info block to be changed */
/* unsigned ientry;         input; info entry number */
/* char     *value;         input; new value */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void db_setc( MEMBLC *ptr, unsigned ientry, char value, int *status );

/* sets char value in info block "*ptr" */

/* parameters of routine */
/* MEMBLC   *ptr;           modify; info block to be changed */
/* unsigned ientry;         input; info entry number */
/* char     value;          input; new value */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/



void db_sett( MEMBLC *ptr, unsigned ientry, TIME *value, int *status );

/* sets pointer value in info block "*ptr" */

/* parameters of routine */
/* MEMBLC   *ptr;           modify; info block to be changed */
/* unsigned ientry;         input; info entry number */
/* TIME     *value;         input; new value */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void db_setp( MEMBLC *ptr, unsigned ientry, void *value, int *status );

/* sets pointer value in info block "*ptr" */

/* parameters of routine */
/* MEMBLC   *ptr;           modify; info block to be changed */
/* unsigned ientry;         input; info entry number */
/* void     *value;         input; new value */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void db_setf( MEMBLC *ptr, unsigned ientry, int value, int *status );

/* sets flag value in info block "*ptr" */

/* parameters of routine */
/* MEMBLC   *ptr;           modify; info block to be changed */
/* unsigned ientry;         input; info entry number */
/* int      value;          input; new value */
/* int      *status;        output; return status */


/*------------------------------------------------------------------------*/


void db_ident( char *name, unsigned *ientry, int *status );

/* identifies name of info entry and returns info index and info type */

/* parameters of routine */
/* char		*name;		input; name of info entry */
/* int		*ientry;		output; info index */
/* int		*status;		output; return status */


/*------------------------------------------------------------------------*/


char *db_entryname( unsigned ientry );

/* returns pointer to name of entry (don't change string, use as input only).
 * If entry is not defined NULL isreturned
 *
 * parameters of routine
 * unsigned      ientry;     input; entry number
 *                           returns pointer to entry name
 */


/*------------------------------------------------------------------------*/


void db_rename( char *oldname, char *newname, int *status );

/* renames info entry */

/* parameters of routine */
/* char		*oldname;		input; old info name */
/* char		*newname;		input; new info name */
/* int		*status;			output; return status */


/*------------------------------------------------------------------------*/


void db_define( char *name, unsigned sh_entry, int *status );

/* defines new info entry "sh_entry"
 *
 * parameters of routine
 * char     *name;     input; name of new entry
 * unsigned sh_entry;  input; type & number of entry
 * int      *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void db_listentries( FILE *f, int *status );

/* lists all defined info entries in stream "f"
 *
 * parameter of routine
 * FILE      *f;      input; output file pointer
 * int       *status; output; return status
 */


/*------------------------------------------------------------------------*/


unsigned int db_nextentry( unsigned int type );

/* returns next entry of same type, if "type" equals E_NEXTENTRY.
 * If "type" is a type code (e.g. EL_TYPE) then the counter is initialised
 *
 * parameter of routine
 * unsigned int type;    input; type code or E_NEXTENTRY
 *                       returns next entry number of same type
 */


/*------------------------------------------------------------------------*/

#endif /* __INFOIDX */

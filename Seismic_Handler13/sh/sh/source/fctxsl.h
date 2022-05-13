
/* file FCTXSL.H
 *      ========
 *
 * version 11, 3-Nov-2006
 *
 * prototypes of module SHLIB.C
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


#ifndef __FCTXSL
#define __FCTXSL

#ifndef __SHCONST
#include "shconst.h"
#endif
#ifndef __CPUSRDEF
#include "cpusrdef.h"
#endif


/*------------------------------------------------------------------------*/


void sl_findmax( float *dat, long lth, float *min, float *max );

/* searches for maximum and minimum value in array "dat" */

/* parameters of routine */
/* float 	  *dat;		input; data array to be checked */
/* long		  lth;		input; length of array */
/* float		  *min;		output; minimum found */
/* float		  *max;		output; maximum found */


/*------------------------------------------------------------------------*/


void sl_findmaxpos( float *dat, long lth, float *min, float *max,
       long *minpos, long *maxpos );

/* searches for maximum and minimum value in array "dat"
 *
 * parameters of routine
 * float      *dat;     input; data array to be checked
 * long       lth;      input; length of array
 * float      *min;     output; minimum found
 * float      *max;     output; maximum found
 * long       *minpos;  output; position of minimum
 * long       *maxpos;  output; position of maximum
 */


/*------------------------------------------------------------------------*/


void sl_cnvshflags( char *flagstr, SHFLAGS *flagbits, int *status );

/* converts flag string into bitmap (SH status flags)
 *
 * parameters of routine
 * char       *flagstr;       input; flag string
 * SHFLAGS    *flagbits;      output; flag bimap
 * int        *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void sl_cnvgcflags( char *flagstr, int *flagbits, int *status );

/* converts flag string into bitmap (graphic channel flags)
 *
 * parameters of routine
 * char       *flagstr;       input; flag string
 * int        *flagbits;      output; graphic channel bitmap
 * int        *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void sl_cnvwdwflags( char *flagstr, int *flagbits, int *status );

/* converts flag string into bitmap (window attributes)
 *
 * parameters of routine
 * char       *flagstr;       input; flag string
 * int        *flagbits;      output; graphic channel bitmap
 * int        *status;        output; return status
 */


/*------------------------------------------------------------------------*/


void sl_setoutput( PARAM *par, int *tcl, int *tcg, int *gcl, int *gcg,
	int *ccl, int *ccg, int *mainch, int *status );

/* changes global & local output streams
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * int        *tcl;      output; local text streams
 * int        *tcg;      output; global text streams
 * int        *gcl;      output; local graphic streams
 * int        *gcg;      output; global graphic streams
 * int        *ccl;      output; local console streams
 * int        *ccg;      output; global console streams
 * int        *mainch;   output; main channel changed
 * int        *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void sl_updatewdwnames( int tc, int gc, int cc );

/* updates names of windows
 *
 * parameters of routine
 * int        tc;        input; text stream(s)
 * int        gc;        input; graphic stream(s)
 * int        cc;        input; console stream(s)
 */


/*------------------------------------------------------------------------*/


void sl_chgflags( PARAM *par, SHFLAGS *lflags, SHFLAGS *gflags,
	int *status );

/* changes global & local flags
 *
 * parameters of routine
 * PARAM      *par;      input; command parameters
 * SHFLAGS    *lflags;   output; local flags
 * SHFLAGS    *gflags;   output; global flags
 * int        *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void sl_setsymbol( char *symbol, char *value, int *status );

/* sets new value of symbol.  Tries first local symbol then (if not
 * found) global symbol.
 *
 * parameters of routine
 * char       *symbol;   input; name of symbol to be changed (with "&")
 * char       *value;    input; new value of symbol
 * int        *status;   output; return status
 */


/*------------------------------------------------------------------------*/


void sl_prepfilnam( char fil[], int *pos, int *status );

/* determines position of filter from filter name (position is either
 * omitted - this result in position number 0 - or the position number
 * is appended to the name by a hyphen ("-").
 *
 * parameters of routine
 * char       fil[];       modify; filter name (position number is removed)
 * int        *pos;        output; position number
 * int        *status;     output; return status
 */


/*------------------------------------------------------------------------*/


void sl_remav( REAL a[], long lth );

/* removes mean value from trace
 *
 * parameters of routine
 * REAL       a[];      modify; trace to remove mean value
 * long       lth;      input; length of trace
 */


/*------------------------------------------------------------------------*/


void sl_parselist( char liststr[], int maxstrlth, char infoname[],
	char lostr[], char histr[], char *cmp_op, BOOLEAN *neg, int *status );

/* parses list string and extracts info entry name, lo & hi bounds,
 * compare operation and whether or not the condition is negated
 *
 * parameters of routine
 * char       liststr[];     input; list string to be parsed
 * int        maxstrlth;     input; maximum length of output strings
 * char       infoname[];    output; name of info entry
 * char       lostr[];       output; lo bound string
 * char       histr[];       output; hi bound string
 * char       *cmp_op;       output; compare operation
 * BOOLEAN    *neg;          output; condition must be negated
 * int        *status;       output; return status
 */


/*------------------------------------------------------------------------*/


BOOLEAN sl_strcpy( char dst[], char src[], int maxlth, int *status );

/* copies string "src" to "dst", if the length of "dst" doesn't
 * exceed the length of "src" "maxlth".  If the source string is
 * too long the string is not copied, "*status" is set to an error
 * status and returns FALSE
 *
 * parameters of routine
 * char       dst[];      output; destination string
 * char       src[];      input; source string
 * int        maxlth;     input; maximum length of destination string
 * int        *status;    output; return status
 *                        returns TRUE if string is copied
 */


/*------------------------------------------------------------------------*/


void sl_resample( REAL old_dt, long old_lth, SAMPLE old[],
	REAL new_dt, long new_lth, SAMPLE new[] );

/* resamples data from "old" array to output array "new"
 *
 * parameters of routine
 * REAL       old_dt;       input; sample distance (sec) of input data
 * long       old_lth;      input; length of input data
 * SAMPLE     old[];        input; input data to be resampled
 * REAL       new_dt;       input; sample distance of output data
 * long       new_lth;      input; length of output data
 * SAMPLE     new[];        output; resampled data
 */


/*------------------------------------------------------------------------*/


void sl_decimate( long old_lth, SAMPLE old[], int decimation, BOOLEAN nomean,
	long new_lth, SAMPLE new[] );

/* decimates data from "old" array to output array "new" using mean values
 *
 * parameters of routine
 * long       old_lth;      input; length of input data
 * SAMPLE     old[];        input; input data to be resampled
 * int        decimation;   input; decimation factor
 * BOOLEAN    nomean;       input; take no mean value, plain resampling
 * long       new_lth;      input; length of output data
 * SAMPLE     new[];        output; resampled data
 */


/*------------------------------------------------------------------------*/


void sl_getlowercase( char instr[], char lowop, char outstr[] );

/* converts uppercase input string to mixedcase outstr.  A character "lowop"
 * found in "instr" makes the next character lowercase
 *
 * parameters of routine
 * char       instr[];     input; uppercase string with "lowop's"
 * char       lowop;       input; lowercase operator
 * char       outstr[];    output; output string
 */


/*------------------------------------------------------------------------*/


void sl_makelowercase( char instr[], char outstr[] );

/* converts uppercase input string to lowercase
 *
 * parameters of routine
 * char       instr[];     input; uppercase string
 * char       outstr[];    output; output string
 */


/*----------------------------------------------------------------------------*/


void sl_prepformatstr( char fmt[] );

/* prepares format string to be used in a printf command
 *
 * parameters of routine
 * char       fmt[];         modify; format string
 */


/*----------------------------------------------------------------------------*/


void sl_make_qname( char station[], char start[], int maxlth, char qname[],
	STATUS *status );

/* creates q-filename from station name & start time
 *
 * parameters of routine
 * char       station[];       input; station name
 * char       start[];         input; start time string
 * int        maxlth;          input; maximum length of output string
 * char       qname[];         output; created q-filename
 * STATUS     *status;         output; return status
 */


/*----------------------------------------------------------------------------*/


void sl_bintoascii( BYTE bin[], long binlth, char **asc, long *asclth,
	int size, int bytesperline, STATUS *status );

/* converts binary array bin[0..binlth-1] to ASCII *asc[0..*asclth-1]
 *
 * parameters of routine
 * BYTE       bin[];        input; binary array
 * long       binlth;       input; length of binary array in BYTEs
 * char       **asc;        output; pointer to ascii array
 * long       *asclth;      output; length of ascii array in chars
 * int        size;         input; align size
 * int        bytesperline; input; bytes per line
 * STATUS     *status;      output; return status
 */


/*----------------------------------------------------------------------------*/


void sl_asciitobin( char asc[], long asclth, BYTE **bin, long *binlth,
	int size, STATUS *status );

/* converts ascii array back to binary array (inverse of sl_bintoascii)
 *
 * parameters of routine
 * char       asc[];      input; ascii array
 * long       asclth;     input; length of ascii array in chars
 * BYTE       **bin;      output; pointer to binary data
 * long       *binlth;    output; length of binary array in bytes
 * int        size;       input; align length in bytes
 * STATUS     *status;    output; return status
 */


/*------------------------------------------------------------------------*/


BOOLEAN sl_quali( PARAM *par, char *qname, int *value, STATUS *status );

/* gets value of qualifier "qname", returns whether qualifier was found
 * translates value if possible.
 *
 * parameters of routine
 * PARAM      *par;    input; command parameter
 * char       *qname;  input; name of qualifier
 * int        *value;  output; value of qualifier
 * STATUS     *status; output; return status
 */


/*------------------------------------------------------------------------*/


BOOLEAN sl_qualr( PARAM *par, char *qname, float *value, STATUS *status );

/* gets value of qualifier "qname", returns whether qualifier was found.
 * translates value if possible.
 *
 * parameters of routine
 * PARAM      *par;    input; command parameter
 * char       *qname;  input; name of qualifier
 * float      *value;  output; value of qualifier
 * STATUS     *status; output; return status
 */


/*------------------------------------------------------------------------*/


BOOLEAN sl_quals( PARAM *par, char *qname, int maxlth, char *value,
	STATUS *status );

/* gets value of qualifier "qname", returns whether qualifier was found.
 * translates value if possible.
 *
 * parameters of routine
 * PARAM      *par;    input; command parameter
 * char       *qname;  input; name of qualifier
 * float      *value;  output; value of qualifier
 * STATUS     *status; output; return status
 */


/*----------------------------------------------------------------------------*/


void sl_make_timestr( char numstr[], char timestr[], STATUS *status );

/* converts number string "<year> <month> <day> <hour> <min> <sec> <ms>"
 * to SH time string
 *
 * parameters of routine
 * char       numstr[];     input; number string
 * char       timestr[];    output; time string
 * STATUS     *status;      output; return status
 */


/*----------------------------------------------------------------------------*/


void sl_printname( CHMAP map );

/* ...
 *
 * parameter of routine
 * CHMAP     map;      input; output window
 */


/*----------------------------------------------------------------------------*/


void sl_cnvphasename( char phase[], int maxlth, char fname[],
	STATUS *status );

/* converts phase name to uppercase name
 *
 * parameters of routine
 * char       phase[];     input; input phase name
 * int        maxlth;      input; maximum length of output string
 * char       fname[];     output; uppercase name
 */


/*----------------------------------------------------------------------------*/


void sl_oscall( char cmd[], char par1[], char par2[], STATUS *status );

/* call to special functions of operating systems
 *
 * parameters of routine
 * char       cmd[];       input; command verb
 * char       par1[];      input; command parameter 1
 * char       par2[];      input; command parameter 2
 */


/*----------------------------------------------------------------------------*/


void sl_parse_stations( char list[],
	char stat[SHC_ILISTLTH][SHC_STATNAMELTH+1], int *statno );

/* Parses station list string "list" and returns list of stations "stat".
 *
 * parameters of routine
 * char       list[];     input; station list (separator ",")
 * char       stat[][];   output; parsed station names
 * int        *statno;    output; number of names found
 */


/*------------------------------------------------------------------------*/

#endif /* __FCTXSL */

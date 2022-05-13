
/* file RECFILTR.C
 *      ==========
 *
 * version 15, 1-Nov-2006
 *  (corrected rf_filter_single since version 2)
 *  (normalization error corrected in version 7, 2-Jun-92)
 *  (startup error in rf_filter_single corrected in version 8, 21-Jan-94)
 *  (removing of mean value on every stage since version 10, 26-Jan-94)
 *  (filter dt == -1.0 allows all sample rates, 21-Jun-94)
 *  very slow if input values are constant due to underflow exceptions
 *     introduced constant sample counter, 5-Dec-2001
 *
 * recursive filtering of traces
 * K. Stammler, 3-JUL-1990
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include "shconst.h"
#include "shvars.h"
#include "erusrdef.h"
#include "numres.h"
#include "rfusrdef.h"
#include "flerrors.h"
#include "globalparams.h"


/* global constants */
#define MAXDEGREE 15
	/* maximum number of coefficients */
#define FILMAGIC 1357913578L
	/* magic number in filter files */
#define FILID_COEFF 3
	/* recursive filter file ID */
#define FILTERMCH '@'
	/* termination character between filters */
#define FILCOMMENTCH '!'
	/* comment character */
#define FORALL_DT -1.0
	/* dt in filter valid for all sample rates */
#define PI 3.141592653
	/* pi */

/* filter type */
typedef struct rf_coeff {
	int       deg_input;        /* degree of input coefficients */
	REAL      ic[MAXDEGREE];    /* input coefficients */
	int       deg_output;       /* degree of output coefficients */
	REAL      oc[MAXDEGREE];    /* output coefficients */
	REAL      norm;             /* normalisation */
	REAL      dt;               /* sample distance in sec */
} RECFILTER;

/* global variables */
static RECFILTER    rfv_fct[RFC_MAXFILTER];   /* filter functions */
static int          rfv_no_of_fil = 0;        /* number of filters */


/* prototypes of local routines */
void rf_readcoeff( FILE *f, int *no, REAL coeff[], int *status );
void rf_read_filter( char *file, int pos, RECFILTER *filter, int *status );
void rf_filter_single( RECFILTER *filter, RFT_STARTUP *startup,
	REAL delta_t, long arrlth, SAMPLE in[], SAMPLE out[],
	STATUS *status );
static void rfh_demean( SAMPLE smp[], long lth );



/*------------------------------------------------------------------------*/



void rf_filter_input( int list_lth, char *flt_list[], int pos_list[],
	int *status )

/* reads one or more filters into memory
 *
 * paramters of routine
 * int       list_lth;       input; number of filters to read
 * char      *flt_list[];    input; list of filter names
 * int       pos_list[];     input; positions of filters in files
 * int       *status;        output; return status
 */
{
	/* local variables */
	int      i;        /* counter */
	int      f;        /* filter counter */
	int      pos;      /* position in file */

	/* executable code */

	if  (list_lth > RFC_MAXFILTER)  {
		*status = FLE_TOOMANY;
		err_setcontext( " ## number " ); err_setcontext_l( list_lth );
		return;
	} /*endif*/

	f = 0;
	for  (i=0;i<list_lth;i++)  {
		if  (pos_list[i] == 0)  {   /* read all filters of file */
			pos = 1;
			*status = FLE_NOERROR;
			while  (*status == FLE_NOERROR)  {
				if  (f == RFC_MAXFILTER)  {
					rfv_no_of_fil = RFC_MAXFILTER;
					*status = FLE_TOOMANY;
					return;
				} /*endif*/
				rf_read_filter( flt_list[i], pos++, rfv_fct+f, status );
				if  (*status == FLE_NOERROR)  f++;
			} /*endwhile*/
			if (*status == FLE_EOFF)  *status = FLE_NOERROR;
		} else {
			rf_read_filter( flt_list[i], pos_list[i], rfv_fct+f, status );
			if  (*status == FLE_NOERROR)  f++;
		} /*endif*/
		if  (*status != FLE_NOERROR)  return;
	} /*endfor*/

	rfv_no_of_fil = f;

} /* end of rf_filter_input */



/*------------------------------------------------------------------------*/



void rf_read_filter( char *file, int pos, RECFILTER *filter, int *status )

/* reads single filter from file "file" at position "pos".  Negative
 * position means that filter is inverted after reading
 *
 * parameters of routine
 * char       *file;      input; name of filter file
 * int        pos;        input; position number (negative = invert)
 * RECFILTER  *filter;    output; filter read from file
 * int        *status;    output; return status
 */
{
	/* local variables */
	FILE     *f;       /* filter file pointer */
	char     *filpath; /* pointer to filter path */
	int      pathcnt;  /* path counter */
	int      i;        /* counter */
	int      id;       /* store ID */
	long     magic;    /* magic longword */
	char     str[BC_LONGSTRLTH+1];  /* scratch */
	REAL     b0;       /* normalisation */

	/* executable code */

	/* open file */
	f = NULL;
	for  (pathcnt=0;;pathcnt++)  {
		filpath = GpGetStringElem( cGpL_defpath_filter, pathcnt );
		if  (filpath == NULL)  break;
		if  ((strlen(str)+strlen(filpath)+1) > BC_LONGSTRLTH)  {
			*status = FLE_STROVFL;
			err_setcontext( " ## filename too long" );
			return;
		} /*endif*/
		strcpy( str, filpath );
		strcat( str, "/" );
		strcat( str, file );
		strcat( str, SHC_DE_RFILT );
		f = sy_fopen( str, "r" );
		if  (f != NULL)  break;
	} /*endfor*/
	if  (f == NULL)  {
		*status = FLE_OPNREAD;
		err_setcontext( " ## file " ); err_setcontext( str );
		return;
	} /*endif*/

	/* read off comments */
	do  {
		if  (fgets(str,BC_LONGSTRLTH,f) == NULL)  {
			*status = FLE_EOFF; fclose( f ); return;
		} /*endif*/
	} while (*str == FILCOMMENTCH);

	/* read magic longword */
	i = sscanf( str, "%ld\n", &magic );
	if  ((i != 1) || (magic != FILMAGIC))  {
		*status = FLE_NOMAGIC; fclose( f ); return;
	} /*endif*/

	/* count position */
	for  (i=2;i<=Abs(pos);i++)  {
		do  {
			if  (fgets(str,BC_LONGSTRLTH,f) == NULL)  {
				*status = FLE_EOFF; fclose( f ); return;
			} /*endif*/
		} while (*str != FILTERMCH);
	} /*endfor*/

	/* check store ID */
	i = fscanf( f, "%d\n", &id );
	if  ((i != 1) || (id != FILID_COEFF))  {
		*status = FLE_NORECFIL; fclose( f ); return;
	} /*endif*/

	/* read sample distance */
	if  (fscanf( f, "%e\n", &(filter->dt) ) != 1)  {
		*status = FLE_FREAD; fclose( f ); return;
	} /*endif*/

	/* read normalisation */
	if  (fscanf( f, "%e\n", &(filter->norm) ) != 1)  {
		*status = FLE_FREAD; fclose( f ); return;
	} /*endif*/

	/* read input (output) coefficients */
	if  (pos > 0)  {
		rf_readcoeff( f, &(filter->deg_input), filter->ic, status );
		if  (*status != FLE_NOERROR)  return;
		rf_readcoeff( f, &(filter->deg_output), filter->oc, status );
		if  (*status != FLE_NOERROR)  return;
	} else {
		rf_readcoeff( f, &(filter->deg_output), filter->oc, status );
		if  (*status != FLE_NOERROR)  return;
		rf_readcoeff( f, &(filter->deg_input), filter->ic, status );
		if  (*status != FLE_NOERROR)  return;
	} /*endif*/

	fclose( f );

	/* divide all coefficients by b[0] */
	if  (filter->oc[0] != 1.0)  {
		b0 = filter->oc[0];
		filter->oc[0] = 1.0;
		for  (i=0;i<filter->deg_input;i++)
			filter->ic[i] /= b0;
		for  (i=1;i<filter->deg_output;i++)
			filter->oc[i] /= b0;
	} /*endif*/

} /* end of rf_read_filter */




/*------------------------------------------------------------------------*/



void rf_readcoeff( FILE *f, int *no, REAL coeff[], int *status )

/* read coefficients from file
 * 
 * parameters of routine
 * FILE       *f;       input; file pointer
 * int        *no;      output; number of coefficients
 * REAL       coeff[];  output; coefficients read
 * int        *status;  output; return status
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	if  (fscanf( f, "%d,\n", no ) != 1)  {
		*status = FLE_FREAD; fclose( f ); return;
	} /*endif*/
	if  (*no > MAXDEGREE)  {
		*status = FLE_DEGOVFL; fclose( f ); return;
	} /*endif*/
	for  ( i = 0; i < *no; i++ )  {
		if  (fscanf( f, "%e\n", coeff+i ) != 1)  {
			*status = FLE_FREAD; fclose( f ); return;
		} /*endif*/
	} /*endfor*/

} /* end of rf_readcoeff */



/*------------------------------------------------------------------------*/



void rf_filter( SAMPLE src[], long trclth, REAL dt, RFT_STARTUP *lpc,
	SAMPLE dst[], STATUS *status )

/* filters the seismogram "src" from index "start" to index "end"
 * with all the filters in rfv_fct.  The result is returned in "dst"
 *
 * parameters of routine
 * SAMPLE     src[];     input; input signal to be filtered
 * long       trclth;    input length of trace in samples
 * REAL       dt;        input; sample distance in sec
 * RFT_STARTUP *lpc;     input; use special startup routine
 * SAMPLE     dst[];     output; output signal
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	SAMPLE   *work;        /* pointer to workspace */
	int      i;            /* counter */

	/* executable code */

	if  (trclth <= 0)  {
		*status = FLE_ZEROLTH;
		return;
	} else if  (rfv_no_of_fil <= 0)  {
		*status = FLE_NOFILTER;
		return;
	} /*endif*/

	if  (rfv_no_of_fil == 1)  {  /* only one filter */

		rf_filter_single( rfv_fct, lpc, dt, trclth, src, dst, status );

	} else {

		/* allocate memory */
		work = (SAMPLE *)sy_allocmem( trclth, (int)sizeof(SAMPLE), status );
		if  (*status != FLE_NOERROR)  return;

		if  ((rfv_no_of_fil % 2) == 0)  {  /* even number of filters */
			/* first output trace is workspace */
			rf_filter_single( rfv_fct, lpc, dt, trclth, src, work, status );
			if  (Severe(status))  {sy_deallocmem( work ); return;}
			if  (lpc->demean)  rfh_demean( work, trclth );
			for  ( i = 1; i <= (rfv_no_of_fil/2)-1; i++ )  {
				rf_filter_single( rfv_fct+(2*i-1), lpc, dt, trclth, work, dst,
					status );
				if  (Severe(status))  {sy_deallocmem( work ); return;}
				if  (lpc->demean)  rfh_demean( dst, trclth );
				rf_filter_single( rfv_fct+(2*i), lpc, dt, trclth, dst, work,
					status );
				if  (Severe(status))  {sy_deallocmem( work ); return;}
				if  (lpc->demean)  rfh_demean( work, trclth );
			} /*endfor*/
		} else {   /* odd number of filters */
			/* first output is dst */
			rf_filter_single( rfv_fct, lpc, dt, trclth, src, dst, status );
			if  (Severe(status))  {sy_deallocmem( work ); return;}
			if  (lpc->demean)  rfh_demean( dst, trclth );
			rf_filter_single( rfv_fct+1, lpc, dt, trclth, dst, work, status );
			if  (Severe(status))  {sy_deallocmem( work ); return;}
			if  (lpc->demean)  rfh_demean( work, trclth );
			for  ( i = 1; i <= ((rfv_no_of_fil-1)/2)-1; i++ )  {
				rf_filter_single( rfv_fct+(2*i), lpc, dt, trclth, work, dst,
					status );
				if  (Severe(status))  {sy_deallocmem( work ); return;}
				if  (lpc->demean)  rfh_demean( dst, trclth );
				rf_filter_single( rfv_fct+(2*i+1), lpc, dt, trclth, dst, work,
					status );
				if  (Severe(status))  {sy_deallocmem( work ); return;}
				if  (lpc->demean)  rfh_demean( work, trclth );
			} /*endfor*/
		} /*endif*/

		rf_filter_single( rfv_fct+rfv_no_of_fil-1, lpc, dt, trclth, work, dst,
			status );

		/* deallocate memory */
		sy_deallocmem( work );

	} /*endif*/

} /* end of rf_filter */



/*------------------------------------------------------------------------*/



void rf_filter_single( RECFILTER *filter, RFT_STARTUP *startup,
	REAL delta_t, long arrlth,	SAMPLE in[], SAMPLE out[],
	STATUS *status )

/* traces are filtered using following formula
 * r[n] = a[0]*f[n] + a[1]*f[n-1] + ... + a[k]*f[n-k] -
 *        b[1]*r[n-1] - b[2]*r[n-2] - ... - b[l]*r[n-l]
 *
 * where
 *   f[n]   input signal at time n*dt  (dt = sample distance)
 *   r[n]   output signal at time n*dt
 *   a[i]   coefficients of numerator polynomial of z-transform of filter
 *   b[i]   coefficients of denominator polynomial of z-transform of filter
 *
 * these variables are represented in the routine by
 *   k     =  filter->deg_input - 1
 *   a[i]  =  filter->ic[i]
 *   l     =  filter->deg_output - 1
 *   b[i]  =  filter->oc[i]
 *   f[n]  =  in[n]
 *   r[n]  =  out[n]
 *
 * The startup procedure is described in "startup".  If
 * startup->use_lpc is FALSE then ordinary startup is executed.  That is
 * all unknown values (indices smaller than 0) are set to zero.
 * To decrease ringing of filtered trace the unknown trace values
 * with indices < 0 are guessed by a prediction backwards in time
 * (startup->use_lpc == TRUE).  The startup->pred_length specifies
 * the prediction length in samples, startup->pred_order the order
 * of the spectral estimation function (maximum entropy method) and
 * startup->src_length gives the number of samples used as input
 * for spectral estimation.
 * 
 * If the input trace has constant values underflow exception occur
 * and slow down speed considerably (on zero-sum filter coefficients).
 * Introduced constant sample counter 5-Dec-2001. K.S.
 *
 *
 * parameters of routine
 * RECFILTER  *filter;    input; filter to be applied to "in"
 * RFT_STARTUP *startup;  input; startup procedure
 * REAL       delta_t;    input; sample distance of input trace in seconds
 * long       arrlth;     input; length of input (and output) trace
 * SAMPLE     in[];       modify; input signal
 * SAMPLE     out[];      output; output signal
 * int        *status;    output; return status
 */
{
	/* local variables */
	REAL     r;        /* scratch */
	long     init;     /* index limit of start procedure */
	long     n;        /* sample counter */
	long     index;    /* scratch index */
	int      i;        /* coeff counter */
	SAMPLE   constval; /* constant value */
	int      constcnt; /* constant value counter */
	/* lpc variables */
	float    dummy;    /* scratch */
	float    *mc;             /* poles for approximation */
	SAMPLE   *prd;            /* backward predicted values */
	SAMPLE   *fprd;           /* filtered predicted values */
	SAMPLE   *strt;           /* input for prediction */

	/* executable code */

	r = Abs( delta_t - filter->dt) * 100.0;
	if  (filter->dt != FORALL_DT && r > delta_t)  {
		*status = FLE_DTMISMCH;
		return;
	} /*endif*/

	init = (long)filter->deg_input - 1;
	if  ((filter->deg_output-1) > init)  init = filter->deg_output-1;

	/* start procedure */
	if  (startup->use_lpc)  {
		/* check parameters */
		if  (startup->src_length < 2 || startup->src_length > arrlth)  {
			*status = FLE_STARTUP;
			return;
		} else if  (startup->pred_length < 0)  {
			*status = FLE_STARTUP;
			return;
		} else if  (startup->pred_order < 1 || startup->pred_order > 100)  {
			*status = FLE_STARTUP;
			return;
		} /*endif*/
		/* get memory */
		prd = (SAMPLE *)sy_allocmem(
			2*(MAXDEGREE+(startup->pred_length)) + startup->src_length,
			(int)sizeof(SAMPLE), status );
		if  (Severe(status))  return;
		fprd = prd + MAXDEGREE + startup->pred_length;
		strt = fprd + MAXDEGREE + startup->pred_length;
		mc = (float *)sy_allocmem( startup->pred_order,
			(int)sizeof(float), status );
		if  (Severe(status))  {sy_deallocmem( prd );return;}
		/* copy reversed input array */
		for  (i=0; i<(startup->src_length); i++)
			strt[i] = in[(startup->src_length)-1-i];
		/* compute poles */
		nr_memcof( strt-1, startup->src_length, startup->pred_order,
			&dummy, mc-1 );
		/* predict previous values */
		nr_predic( strt-1, startup->src_length, mc-1, startup->pred_order,
			prd-1, (int)init+(startup->pred_length)+1 );
		/* copy start values to filtered predicted values */
		for  (i=(startup->pred_length)+(int)init;
			i>(startup->pred_length); i--)
			fprd[i] = prd[i];
		/* do startup */
		for  (n= -(startup->pred_length); n<init; n++)  {
			if  (n > arrlth)  return;
			r = 0.0;
			for  (i=0; i<filter->deg_input; i++ )  {
				index = n - i;
				if  (index >= 0)  {
					r += filter->ic[i]*in[index];
				} else {
					r += filter->ic[i]*prd[-index];
				} /*endif*/
			} /*endfor*/
			for  (i=1; i<filter->deg_output; i++ )  {
				index = n - i;
				if  (index >= 0)  {
					r -= filter->oc[i]*out[index];
				} else {
					r -= filter->oc[i]*fprd[-index];
				} /*endif*/
			} /*endfor*/
			if  (n >= 0)  {
				out[n] = r;
			} else {
				fprd[-n] = r;
			} /*endif*/
		} /*endfor*/
		sy_deallocmem( prd );
		sy_deallocmem( mc );
	} else {
		/* start procedure */
		out[0] = in[0]; /*(in[1] - in[0]) / delta_t;*/
		for  (n=1; n<init; n++)  {
			if  (n > arrlth)  return;
			r = 0.0;
			for  (i=0; i<filter->deg_input; i++ )  {
				index = n - i;
				if  (index >= 0)  r += filter->ic[i]*in[index];
			} /*endfor*/
			for  (i=1; i<filter->deg_output; i++ )  {
				index = n - i;
				if  (index >= 0)  r -= filter->oc[i]*out[index];
			} /*endfor*/
			out[n] = r;
		} /*endfor*/
	} /*endif*/

	constcnt = 0;
	constval = 0.0;

	/* now like in above formula */
	for  (n=init; n<arrlth; n++ )  {
		/* first check for constant input values */
		if  (in[n] == constval)  {
			constcnt++;
		} else {
			constcnt = 0;
			constval = in[n];
		} /*endif*/
		/* if input values are const. assign 0 to output otherwise apply formula*/
		if  (constcnt < 50)  {
			r = 0.0;
			for  (i=0; i<filter->deg_input; i++ )
				r += filter->ic[i]*in[n-i];
			for  (i=1; i<filter->deg_output; i++ ) /* before version 2: */
				r -= filter->oc[i]*out[n-i];        /* for (i=0; i<...   */
			out[n] = r;
		} else {
			out[n] = 0.0;
		} /*endif*/
		if  (!(n&0x3))  sy_sharecpu();
	} /*endfor*/

	if  (filter->norm != 1.0)
		for  (n=0; n<arrlth; out[n++] *= filter->norm ) {}

} /* end of rf_filter_single */



/*------------------------------------------------------------------------*/



static void rfh_demean( SAMPLE smp[], long lth )

/* removes mean value from trace
 *
 * parameters of routine
 * REAL       smp[];      modify; trace to remove mean value
 * long       lth;        input; length of trace
 */
{
	/* local variables */
	SAMPLE   mean;       /* mean value */
	SAMPLE   *s;         /* moving pointer */

	/* executable code */

	mean = 0.0;
	for  (s=smp; s<(smp+lth); mean += *s++) {}
	mean /= (float)lth;
	for  (s=smp; s<(smp+lth); s++)  {*s -= mean;}

} /* end of rfh_demean */



/*------------------------------------------------------------------------*/

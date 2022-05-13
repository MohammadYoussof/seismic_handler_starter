
/* file FFTFILTR.C
 *      ==========
 *
 * version 18, 1-Nov-2006
 *
 * fast fourier filter
 * K. Stammler, 11-JUL-1990
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
#include <math.h>
#include "basecnst.h"
#include BC_SYSBASE
#include "shconst.h"
#include "shvars.h"
#include "erusrdef.h"
#include "ffusrdef.h"
#include "fctxmt.h"
#include "flerrors.h"
#include "numres.h"
#include "globalparams.h"


/* global constants */
#define MAXFILTER 5
	/* maximum number of filters */
#define FILMAGIC 1357913578L
	/* magic longword of filter files */
#define FILTERMCH '@'
	/* termination character between filters */
#define FILCOMMENTCH '!'
	/* comment character */
#define FILID_TFRAT 1
	/* store ID for FFT filters */
#define FILID_FILFUNC 2
	/* store ID for digital filter function */


/* macros */
#define re_mul(ar,ai,br,bi) ((ar)*(br)-(ai)*(bi))
#define im_mul(ar,ai,br,bi) ((ai)*(br)+(ar)*(bi))


/* global variables */
FFT_RATFCT  fct_ffv[MAXFILTER]; /* transfer functions */
int         no_of_fil_ffv;      /* number of transfer functions */
FFT_FILFUNC ffv_filf;           /* digital filter */


/* prototypes of local routines */
void ff_read_filter( char file[], int pos, FFT_RATFCT *filter, STATUS *status );
static void ff_readcoeff( FILE *f, int *no, COMPLEX coeff[], STATUS *status );
static long ff_next2pow( long l );
static void ff_frqmul( long lth, COMPLEX dat[], REAL d_omega, int no_of_fil,
	FFT_RATFCT fil[] );
static void ff_filfmul( long lth, COMPLEX dat[], REAL d_omega,
	FFT_FILFUNC *filf, STATUS *status );
static void ff_attmul( long lth, COMPLEX dat[], REAL d_omega, REAL t, REAL nf );
static void ff_tfvalue( COMPLEX *res, FFT_RATFCT *fct, REAL x );
static void ff_filfvalue( COMPLEX *x, FFT_FILFUNC *filf, REAL f,
	STATUS *status );
static BOOLEAN ff_freq0ok( FFT_RATFCT *fil );
static void ff_lintaper( float taper, long lth, COMPLEX dat[] );
static void ff_costaper( float taper, long lth, COMPLEX dat[] );


/*-------------------------------------------------------------------------*/



void ff_filter_input( int listlth, char *flt_list[], int poslist[],
	STATUS *status )

/* reads a list of filters into memory.  If "poslist[i]" is negative,
 * the i-th filter is inverted, that means poles become zeroes and
 * zeroes become poles.
 *
 * parameters of routine
 * int        listlth;      input; length of filter list
 * char       *flt_list[];  input; filter list
 * int        poslist[];    input; position list
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	int      i;        /* counter */
	int      f;        /* filter counter */
	int      pos;      /* position counter */

	/* executable code */

	if  (listlth > MAXFILTER)  {
		*status = FLE_TOOMANY;
		err_setcontext( " ## number " ); err_setcontext_l( listlth );
		return;
	} /*endif*/

	f = 0;
	for  (i=0;i<listlth;i++)  {
		if  (poslist[i] == 0)  {   /* read all filters of file */
			pos = 1;
			*status = FLE_NOERROR;
			while  (*status == FLE_NOERROR)  {
				if  (f == MAXFILTER)  {
					no_of_fil_ffv = MAXFILTER;
					*status = FLE_TOOMANY;
					err_setcontext( " ## number " ); err_setcontext_l( no_of_fil_ffv );
					return;
				} /*endif*/
				ff_read_filter( flt_list[i], pos++, fct_ffv+f, status );
				if  (*status == FLE_NOERROR)  f++;
			} /*endwhile*/
			if  (*status == FLE_EOFF)  *status = FLE_NOERROR;
		} else {
			ff_read_filter( flt_list[i], poslist[i], fct_ffv+f, status );
			if  (*status == FLE_NOERROR)  f++;
		} /*endif*/
		if  (*status != FLE_NOERROR)  return;
	} /*endfor*/
	no_of_fil_ffv = f;

	/* shorten zeroes in numerator and denominator of all filters read in */
	ff_shorten_zeroes( fct_ffv, no_of_fil_ffv );

} /* end of ff_filter_input */



/*-------------------------------------------------------------------------*/



void ff_read_filter( char file[], int pos, FFT_RATFCT *filter, STATUS *status )

/* reads filter from filter file "file" at position number "pos"
 *
 * parameters of routine
 * char       file[];   input; name of filter file
 * int        pos;      input; position; if negative filter is inverted
 * FFT_RATFCT *filter;  output; filter read from file
 * STATUS     *status;  output; return status
 */
{
	/* local variables */
	FILE     *ff;                   /* filter file */
	char     *filpath;              /* filter path */
	int      pathcnt;               /* path counter */
	char     str[BC_LONGSTRLTH+1];  /* scratch */
	int      i;                     /* counter */
	long     magic;                 /* magic longword */
	int      id;                    /* store ID */

	/* executable code */

	/* open filter file */
	ff = NULL;
	for  (pathcnt=0;;pathcnt++) {
		filpath = GpGetStringElem( cGpL_defpath_filter, pathcnt );
		if  (filpath == NULL)  break;
		if  ((strlen(file)+strlen(filpath)+1) > BC_LONGSTRLTH)  {
			*status = FLE_STROVFL;
			return;
		} /*endif*/
		strcpy( str, filpath );
		strcat( str, "/" );
		strcat( str, file );
		strcat( str, SHC_DE_FFILT );
		ff = sy_fopen( str, "r" );
		if  (ff != NULL)  break;
	} /*endfor*/
	if  (ff == NULL)  {
		*status = FLE_OPNREAD;
		err_setcontext( " ## file " ); err_setcontext( str );
		return;
	} /*endif*/

	/* read off comments */
	do  {
		if  (fgets(str,BC_LONGSTRLTH,ff) == NULL)  {
			*status = FLE_EOFF; fclose( ff ); return;
		} /*endif*/
	} while (*str == FILCOMMENTCH);

	/* read magic longword */
	i = sscanf( str, "%ld\n", &magic );
	if  ((i != 1) || (magic != FILMAGIC))  {
		err_setcontext( " ## file " ); err_setcontext( file );
		*status = FLE_NOMAGIC; fclose( ff ); return;
	} /*endif*/

	/* count position */
	for  (i=2;i<=Abs(pos);i++)  {
		do  {
			if  (fgets(str,BC_LONGSTRLTH,ff) == NULL)  {
				err_setcontext( " ## file " ); err_setcontext( file );
				*status = FLE_EOFF; fclose( ff ); return;
			} /*endif*/
		} while (*str != FILTERMCH);
	} /*endfor*/

	/* check store ID */
	i = fscanf( ff, "%d\n", &id );
	if  ((i != 1) || (id != FILID_TFRAT))  {
		err_setcontext( " ## file " ); err_setcontext( file );
		*status = FLE_NORATFCT; fclose( ff ); return;
	} /*endif*/

	/* read normalisation */
	if  (fscanf( ff, "%e\n", &(filter->norm) ) != 1)  {
		err_setcontext( " ## file " ); err_setcontext( file );
		*status = FLE_FREAD; fclose( ff ); return;
	} /*endif*/
	if  (pos < 0)  filter->norm = 1.0 / filter->norm;

	if  (pos > 0)  {
		ff_readcoeff( ff, &(filter->no_of_zeroes), filter->zero, status );
		if  (*status != FLE_NOERROR)  return;
		ff_readcoeff( ff, &(filter->no_of_poles), filter->pole, status );
		if  (*status != FLE_NOERROR)  return;
	} else {
		ff_readcoeff( ff, &(filter->no_of_poles), filter->pole, status );
		if  (*status != FLE_NOERROR)  return;
		ff_readcoeff( ff, &(filter->no_of_zeroes), filter->zero, status );
		if  (*status != FLE_NOERROR)  return;
	} /*endif*/

	fclose( ff );

} /* end of ff_read_filter */



/*------------------------------------------------------------------------*/



static void ff_readcoeff( FILE *f, int *no, COMPLEX coeff[], STATUS *status )

/* reads complex coefficients from file
 * 
 * parameters of routine
 * FILE       *f;       input; file pointer
 * int        *no;      output; number of coefficients
 * COMPLEX    coeff[];  output; coefficients read
 * STATUS     *status;  output; return status
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	if  (fscanf( f, "%d\n", no ) != 1)  {
		*status = FLE_FREAD; fclose( f ); return;
	} /*endif*/
	if  (*no > FFC_MAXDEGREE)  {
		*status = FLE_DEGOVFL; fclose( f ); return;
	} /*endif*/
	for  ( i = 0; i < *no; i++ )  {
		if  (fscanf( f, "(%e,%e)\n", &(coeff[i].re), &(coeff[i].im) ) != 2)  {
			*status = FLE_FREAD; fclose( f ); return;
		} /*endif*/
	} /*endfor*/

} /* end of ff_readcoeff */



/*-------------------------------------------------------------------------*/



void ff_filfunc_input( char file[], FFT_FILFUNC *filf, STATUS *status )

/* reads digital filter function to specified filter structure "filf" or
 * into internal variable if "filf" is NULL
 *
 * parameters of routine
 * char       file[];         input; name of filter file
 * FFT_FILFUNC *filf;         output; output structure or NULL
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	FFT_FILFUNC *lfilf;             /* pointer to filter structure */
	FILE     *ff;                   /* pointer to filter file */
	char     *filpath;              /* path to filter */
	int      pathcnt;               /* path counter */
	char     str[BC_LONGSTRLTH+1];  /* scratch string */
	long     magic;                 /* magic number */
	int      id;                    /* store ID */
	long     i;                     /* counter */

	/* executable code */

	lfilf = (filf == NULL) ? &ffv_filf : filf;

	/* open filter file */
	ff = NULL;
	for  (pathcnt=0;;pathcnt++)  {
		filpath = GpGetStringElem( cGpL_defpath_filter, pathcnt );
		if  (filpath == NULL)  break;
		if  ((strlen(file)+strlen(filpath)+1) > BC_LONGSTRLTH)  {
			*status = FLE_STROVFL;
			return;
		} /*endif*/
		strcpy( str, filpath );
		strcat( str, "/" );
		strcat( str, file );
		strcat( str, SHC_DE_DFILT );
		ff = sy_fopen( str, "r" );
		if  (ff != NULL)  break;
	} /*endif*/
	if  (ff == NULL)  {
		*status = FLE_OPNREAD;
		err_setcontext( " ## file " ); err_setcontext( str );
		return;
	} /*endif*/

	/* read off comments */
	do  {
		if  (fgets(str,BC_LONGSTRLTH,ff) == NULL)  {
			err_setcontext( " ## file " ); err_setcontext( file );
			*status = FLE_EOFF; fclose( ff ); return;
		} /*endif*/
	} while (*str == FILCOMMENTCH);

	/* read magic longword */
	i = sscanf( str, "%ld\n", &magic );
	if  ((i != 1) || (magic != FILMAGIC))  {
		err_setcontext( " ## file " ); err_setcontext( file );
		*status = FLE_NOMAGIC; fclose( ff ); return;
	} /*endif*/

	/* check store ID */
	i = fscanf( ff, "%d\n", &id );
	if  ((i != 1) || (id != FILID_FILFUNC))  {
		err_setcontext( " ## file " ); err_setcontext( file );
		*status = FLE_NOFILFUNC; fclose( ff ); return;
	} /*endif*/

	/* free previous filter */
	if  (filf == NULL && ffv_filf.lth != 0)
		sy_deallocmem( ffv_filf.frq );

	i = fscanf( ff, "%ld\n", &(lfilf->lth) );
	if  (i != 1)  {
		err_setcontext( " ## file " ); err_setcontext( file );
		*status = FLE_READERR;
		fclose( ff );
		lfilf->lth = 0;
		return;
	} /*endif*/

	/* allocate memory */
	lfilf->frq = (REAL *)sy_allocmem( (lfilf->lth)*3L,
		(int)sizeof(REAL), status );
	if  (Severe(status))  {
		fclose( ff );
		lfilf->lth = 0;
		return;
	} /*endif*/
	lfilf->mod = lfilf->frq + lfilf->lth;
	lfilf->phase = lfilf->mod + lfilf->lth;

	for  (i=0; i<(lfilf->lth); i++)  {
		fscanf( ff, "%f %f %f\n", (lfilf->frq)+i, (lfilf->mod)+i,
			(lfilf->phase)+i );
		lfilf->frq[i] *= 2.0*BC_PI;   /* make omega from f */
	} /*endfor*/

	fclose( ff );

} /* end of ff_filfunc_input */



/*-------------------------------------------------------------------------*/



void ff_filter( BOOLEAN filf, SAMPLE src[], long lth, REAL dt,
	float taper, SAMPLE dst[], STATUS *status )

/* filters array "src" with all filters read in.  The result is
 * stored in "dst".
 *
 * parameters of routine
 * BOOLEAN    filf;        input; digital filter (TRUE) or poles-zeroes
 * SAMPLE     src[];       input; input trace
 * long       lth;         input; length of traces
 * REAL       dt;          input; sample distance in sec
 * float      taper;       input; start of taper window [0..1]
 * SAMPLE     dst[];       output; filtered trace
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	long     serieslth;    /* length of FFT array */
	REAL     *fftarr;      /* pointer to complex FFT array */
	REAL     *c;           /* moving pointer */
	REAL     *r;           /* moving pointer */
	REAL     norm;         /* normalisation */

	/* executable code */

	if  (lth <= 0)  {
		*status = FLE_ZEROLTH;
		return;
	} else if  (no_of_fil_ffv == 0 && !filf)  {
		*status = FLE_NOFILTER;
		return;
	} else if  (ffv_filf.lth == 0 && filf)  {
		*status = FLE_NOFILTER;
		return;
	} /*endif*/

	/* allocate memory */
	serieslth = ff_next2pow( lth );
	fftarr = (REAL *)sy_allocmem( serieslth, (int)sizeof(REAL), status );
	if  (*status != FLE_NOERROR)  return;

	/* copy input data to FFT array */
	for  ( r=src, c=fftarr; c<(fftarr+lth); *c++ = *r++ )  {}
	for  ( c=fftarr+lth; c<(fftarr+serieslth); *c++ = 0.0 )  {}

	/* perform FFT */
	nr_realft( fftarr-1, serieslth/2, 1 );
	for  ( c=fftarr+3; c<(fftarr+serieslth); c += 2 )
		*c = -(*c);

	/* apply filter(s) */
	if  (filf)  {
		ff_filfmul( serieslth/2, (COMPLEX *)fftarr,
			2.0*SHC_PI/((float)serieslth*dt), &ffv_filf, status );
		if  (Severe(status))  {
			sy_deallocmem( fftarr );
			return;
		} /*endif*/
	} else {
		ff_frqmul( serieslth/2, (COMPLEX *)fftarr,
			2.0*SHC_PI/((float)serieslth*dt), no_of_fil_ffv, fct_ffv );
	} /*endif*/

	if  (taper >= 0.0 && taper < 1.0)
		ff_costaper( taper, serieslth/2, (COMPLEX *)fftarr );

	/* back transformation */
	for  ( c=fftarr+3; c<(fftarr+serieslth); c += 2 )
		*c = -(*c);
	nr_realft( fftarr-1, serieslth/2, -1 );

	/* copy results to output array */
	norm = 2.0 / (float)serieslth;
	for  ( r=dst, c=fftarr; c<(fftarr+lth); *dst++ = *c++ * norm )  {}

	/* free memory */
	sy_deallocmem( fftarr );

} /* end of ff_filter */



/*-------------------------------------------------------------------------*/



static void ff_frqmul( long lth, COMPLEX dat[], REAL d_omega,
	int no_of_fil, FFT_RATFCT fil[] )

/* applies filter(s) to "dat" array.  Multiplies the whole array
 * by a filter function "fil".  The array is assumed to be the
 * positive half of a FFT output.  The imaginary part of the zeroth
 * complex number is assumed to be the (real) frequency value at
 * the nyquist frequency.
 *
 * parameters of routine
 * long       lth;        input; length of array to be filtered
 * COMPLEX    dat[];      modify; array to be filtered
 * REAL       d_omega;    input; angular frequency steps
 * int        no_of_fil;  input; number of filters
 * FFT_RATFCT fil[];      input; filters
 */
{
	/* local variables */
	int      f;           /* filter counter */
	long     i;           /* counter */
	long     start;       /* start value of counter */
	COMPLEX  *c;          /* moving pointer */
	COMPLEX  temp, tfval; /* scratch */
	COMPLEX  nyquist;     /* value at nyquist frequency */

	/* executable code */

	nyquist.re = dat[0].im;
	nyquist.im = 0.0;
	dat[0].im = 0.0;

	for  (f=0; f<no_of_fil; f++)  {
		if  (ff_freq0ok(fil+f))  {
			start = 0;
			c = dat;
		} else {
			start = 1;
			dat->re = 0.0;
			dat->im = 0.0;
			c = dat+1;
		} /*endif*/
		for  (i=start; i<lth; i++)  {
			ff_tfvalue( &tfval, fil+f, (float)i*d_omega );
			mt_mulcmplx( &temp, c, &tfval );
			c->re = temp.re;
			(c++)->im = temp.im;
		} /*endfor*/
		ff_tfvalue( &tfval, fil+f, (float)lth*d_omega );
		mt_mulcmplx( &temp, &nyquist, &tfval );
		nyquist = temp;
	} /*endfor*/

	dat[0].im = nyquist.re;

} /* end of ff_frqmul */



/*-------------------------------------------------------------------------*/



static void ff_filfmul( long lth, COMPLEX dat[], REAL d_omega,
	FFT_FILFUNC *filf, STATUS *status )

/* applies digital filter to "dat" array.  Multiplies the whole array
 * by a filter function "fil".  The array is assumed to be the
 * positive half of a FFT output.  The imaginary part of the zeroth
 * complex number is assumed to be the (real) frequency value at
 * the nyquist frequency.
 *
 * parameters of routine
 * long       lth;        input; length of array to be filtered
 * COMPLEX    dat[];      modify; array to be filtered
 * REAL       d_omega;    input; angular frequency steps
 * FFT_FILFUNC *filf;     input; digital filter
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	long     i;           /* counter */
	COMPLEX  *c;          /* moving pointer */
	COMPLEX  temp, tfval; /* scratch */
	COMPLEX  nyquist;     /* value at nyquist frequency */

	/* executable code */

	nyquist.re = dat[0].im;
	nyquist.im = 0.0;
	dat[0].im = 0.0;

	c = dat;
	for  (i=0; i<lth; i++)  {
		ff_filfvalue( &tfval, filf, (float)i*d_omega, status );
		if  (Severe(status))  return;
		mt_mulcmplx( &temp, c, &tfval );
		c->re = temp.re;
		(c++)->im = temp.im;
	} /*endfor*/
	ff_filfvalue( &tfval, filf, (float)lth*d_omega, status );
	if  (Severe(status))  return;
	mt_mulcmplx( &temp, &nyquist, &tfval );
	nyquist = temp;

	dat[0].im = nyquist.re;

} /* end of ff_filfmul */



/*-------------------------------------------------------------------------*/



static void ff_filfvalue( COMPLEX *x, FFT_FILFUNC *filf, REAL f,
	STATUS *status )

/* computes value of digital filter "filf" at frequency "f" by
 * linear interpolation
 *
 * parameters of routine
 * COMPLEX    *x;        output; computed value
 * FFT_FILFUNC *filf;    input; digital filter
 * REAL       f;         input; frequency
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	long     maxidx;    /* maximum index */
	long     idx;       /* index of lower frequency */
	REAL     m, p;      /* modulus and phase */
	REAL     frac;      /* fraction for interpolation */
	REAL     p1, p2;    /* phase values at grid points */

	/* executable code */

	maxidx = filf->lth-1;
	if  (filf->frq[0] > f  ||  filf->frq[maxidx] < f)  {
		*status = FLE_FILFRANGE;
		return;
	} /*endif*/

	idx = 1;
	while  (filf->frq[idx] < f)
		idx++;
	idx--;

	/* interpolate modulus and phase */
	frac = (filf->mod[idx+1] - filf->mod[idx])
		/ (filf->frq[idx+1] - filf->frq[idx]);
	m = filf->mod[idx] + (f - filf->frq[idx])*frac;

	/* phase may contain a 360 deg wrap */
	p1 = filf->phase[idx];
	p2 = filf->phase[idx+1];
	if  (fabs(p1-p2) > 180.0)  {
		if  (p1 < p2)  {
			p1 += 360.0;
		} else {
			p2 += 360.0;
		} /*endif*/
	} /*endif*/
	frac = (p2 - p1)
		/ (filf->frq[idx+1] - filf->frq[idx]);
	p = p1 + (f - filf->frq[idx])*frac;
	p /= SHC_RAD_TO_DEG;

	x->re = m*cos(p);
	x->im = m*sin(p);

} /* end of ff_filfvalue */



/*-------------------------------------------------------------------------*/



void ff_attenuate( SAMPLE src[], long lth, REAL dt, REAL att,
	SAMPLE dst[], STATUS *status )

/* attenuates array "src" with t* of "att".  The result is
 * stored in "dst".
 *
 * parameters of routine
 * SAMPLE     src[];       input; input trace
 * long       lth;         input; length of traces
 * REAL       dt;          input; sample distance in sec
 * REAL       att;         input; attenuation (t* in sec)
 * SAMPLE     dst[];       output; filtered trace
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	long     serieslth;    /* length of FFT array */
	REAL     *fftarr;      /* pointer to complex FFT array */
	REAL     *c;           /* moving pointer */
	REAL     *r;           /* moving pointer */
	REAL     norm;         /* normalisation */

	/* executable code */

	if  (lth <= 0)  {
		*status = FLE_ZEROLTH;
		return;
	} /*endif*/

	/* allocate memory */
	serieslth = ff_next2pow( lth );
	fftarr = (REAL *)sy_allocmem( serieslth, (int)sizeof(REAL), status );
	if  (*status != FLE_NOERROR)  return;

	/* copy input data to FFT array */
	for  ( r=src, c=fftarr; c<(fftarr+lth); *c++ = *r++ )  {}
	for  ( c=fftarr+lth; c<(fftarr+serieslth); *c++ = 0.0 )  {}

	/* perform FFT */
	nr_realft( fftarr-1, serieslth/2, 1 );
	for  ( c=fftarr+3; c<(fftarr+serieslth); c += 2 )
		*c = -(*c);

	/* attenuate */
	ff_attmul( serieslth/2, (COMPLEX *)fftarr,
		2.0*SHC_PI/((float)serieslth*dt), att, SHC_PI/dt );

	/* back transformation */
	for  ( c=fftarr+3; c<(fftarr+serieslth); c += 2 )
		*c = -(*c);
	nr_realft( fftarr-1, serieslth/2, -1 );

	/* copy results to output array */
	norm = 2.0 / (float)serieslth;
	for  ( c=fftarr; c<(fftarr+lth); *dst++ = *c++ * norm )  {}

	/* free memory */
	sy_deallocmem( fftarr );

} /* end of ff_attenuate */



/*-------------------------------------------------------------------------*/



void ff_hilbert( SAMPLE src[], long lth, SAMPLE dst[], STATUS *status )

/* computes Hilbert transformation of "src"
 *
 * parameters of routine
 * SAMPLE     src[];       input; input trace
 * long       lth;         input; length of traces
 * SAMPLE     dst[];       output; filtered trace
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	long     serieslth;    /* length of FFT array */
	REAL     *fftarr;      /* pointer to complex FFT array */
	REAL     *c;           /* moving pointer */
	REAL     *r;           /* moving pointer */
	REAL     tmp;          /* scratch */
	REAL     norm;         /* normalisation of back transformation */

	/* executable code */

	if  (lth <= 0)  {
		*status = FLE_ZEROLTH;
		return;
	} /*endif*/

	/* allocate memory */
	serieslth = ff_next2pow( lth );
	fftarr = (REAL *)sy_allocmem( serieslth, (int)sizeof(REAL), status );
	if  (*status != FLE_NOERROR)  return;

	/* copy input data to FFT array */
	for  ( r=src, c=fftarr; c<(fftarr+lth); *c++ = *r++ )  {}
	for  ( c=fftarr+lth; c<(fftarr+serieslth); *c++ = 0.0 )  {}

	/* perform FFT */
	nr_realft( fftarr-1, serieslth/2, 1 );
	/* for  ( c=fftarr+3; c<(fftarr+serieslth); c += 2 )
		*c = -(*c); */

	for  (c=fftarr+2; c<fftarr+serieslth; c += 2)  {
		tmp = *c;
		*c = *(c+1);
		*(c+1) = -tmp;
	} /*endfor*/
	*fftarr = 0.;
	*(fftarr+1) = 0.;

	/* back transformation */
	/* for  ( c=fftarr+3; c<(fftarr+serieslth); c += 2 )
		*c = -(*c); */
	nr_realft( fftarr-1, serieslth/2, -1 );

	/* copy results to output array */
	norm = 2.0 / (float)serieslth;
	for  ( c=fftarr; c<(fftarr+lth); *dst++ = norm * (*c++) )  {}

	/* free memory */
	sy_deallocmem( fftarr );

} /* end of ff_hilbert */



/*-------------------------------------------------------------------------*/



void ff_hilbphase( int mode, SAMPLE src[], long lth, SAMPLE dst[], STATUS *status )

/* computes Hilbert transformation of "src", uses this as imaginary
 * part of the input function and returns the phase in deg of this
 * complex value.
 *
 * parameters of routine
 * int        mode;        input; mode of computation
 * SAMPLE     src[];       input; input trace
 * long       lth;         input; length of traces
 * SAMPLE     dst[];       output; filtered trace
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	long     serieslth;    /* length of FFT array */
	REAL     *fftarr;      /* pointer to complex FFT array */
	REAL     *c;           /* moving pointer */
	REAL     *r;           /* moving pointer */
	REAL     tmp;          /* scratch */
	REAL     norm;         /* normalisation of back transformation */

	/* executable code */

	if  (lth <= 0)  {
		*status = FLE_ZEROLTH;
		return;
	} /*endif*/

	/* allocate memory */
	serieslth = ff_next2pow( lth );
	fftarr = (REAL *)sy_allocmem( serieslth, (int)sizeof(REAL), status );
	if  (*status != FLE_NOERROR)  return;

	/* copy input data to FFT array */
	for  ( r=src, c=fftarr; c<(fftarr+lth); *c++ = *r++ )  {}
	for  ( c=fftarr+lth; c<(fftarr+serieslth); *c++ = 0.0 )  {}

	/* perform FFT */
	nr_realft( fftarr-1, serieslth/2, 1 );
	/* for  ( c=fftarr+3; c<(fftarr+serieslth); c += 2 )
		*c = -(*c); */

	for  (c=fftarr+2; c<fftarr+serieslth; c += 2)  {
		tmp = *c;
		*c = *(c+1);
		*(c+1) = -tmp;
	} /*endfor*/
	*fftarr = 0.;
	*(fftarr+1) = 0.;

	/* back transformation */
	/* for  ( c=fftarr+3; c<(fftarr+serieslth); c += 2 )
		*c = -(*c); */
	nr_realft( fftarr-1, serieslth/2, -1 );

	/* compute phase of complex number <src> + i*<fft> */
	norm = 2.0 / (float)serieslth;
	for  ( r=src, c=fftarr; c<(fftarr+lth); c++, r++)  {
		if  (mode == FFC_GET_PHASE_I)  {
			*dst++ = sin( atan2( norm*(*c), *r ) );
		} else if  (mode == FFC_GET_PHASE_R)  {
			*dst++ = cos( atan2( norm*(*c), *r ) );
		} else {
			*dst++ = atan2( norm*(*c), *r ) / SHC_PI * 180.0;
		} /*endif*/
	} /*endfor*/

	/* free memory */
	sy_deallocmem( fftarr );

} /* end of ff_hilbphase */



/*-------------------------------------------------------------------------*/



void ff_mindelay( SAMPLE src[], long lth, long offset, BOOLEAN ac,
	SAMPLE dst[], STATUS *status )

/* transforms "src" into a minimum delay wavelet
 *
 * parameters of routine
 * SAMPLE     src[];       input; input trace
 * long       lth;         input; length of traces
 * long       offset;      input; number of offset samples
 * BOOLEAN    ac;          input; input is autocorrelation
 * SAMPLE     dst[];       output; minimum delay wavelet trace
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	long     serieslth;    /* length of FFT array */
	REAL     *fftarr;      /* pointer to complex FFT array */
	REAL     *c;           /* moving pointer */
	REAL     *r;           /* moving pointer */
	REAL     *phase;       /* phase spectrum */
	REAL     *lna;         /* logarithm of amplitude */
	REAL     norm;         /* normalisation of back transformation */
	REAL     shift;        /* shift phase */

	/* executable code */

	if  (lth <= 0)  {
		*status = FLE_ZEROLTH;
		return;
	} /*endif*/

	/* allocate memory */
	serieslth = ff_next2pow( lth );
	fftarr = (REAL *)sy_allocmem( serieslth, (int)sizeof(REAL), status );
	if  (*status != FLE_NOERROR)  return;
	lna = (REAL *)sy_allocmem( serieslth, (int)sizeof(REAL), status );
	if  (*status != FLE_NOERROR)  {
		sy_deallocmem( fftarr );
		return;
	} /*endif*/
	phase = lna + (serieslth/2);

	shift = -(REAL)offset * 2.0*SHC_PI/(REAL)serieslth;

	/* copy input data to FFT array */
	for  ( r=src, c=fftarr; c<(fftarr+lth); *c++ = *r++ )  {}
	for  ( c=fftarr+lth; c<(fftarr+serieslth); *c++ = 0.0 )  {}

	/* perform FFT */
	nr_realft( fftarr-1, serieslth/2, 1 );
	for  ( c=fftarr+3; c<(fftarr+serieslth); c += 2 )
		*c = -(*c);

	/* get amplitude spectrum and compute logarithm */
	for  (r=fftarr,c=lna; r<fftarr+serieslth; r += 2,c++)  {
		*r = sqrt( (*r)*(*r) + (*(r+1))*(*(r+1)) );
		if  (ac)  *r = sqrt( *r );   /* autocorr. has squared amplitudes */
		*c = (*r > 0.) ? log( *r ) : 0.;
	} /*endif*/
	/* phase spectrum is Hilbert transform of ln(A) */
	ff_hilbert( lna, serieslth/2, phase, status );
	if  (Severe(status))  {
		sy_deallocmem( fftarr );
		sy_deallocmem( lna );
		return;
	} /*endif*/
	/* get minimum delay spectrum */
	for  (r=fftarr,c=phase; r<fftarr+serieslth; r += 2,c++)  {
		*c += (REAL)(c-phase)*shift;
		*(r+1) = *r * sin(*c);  /* imaginary part */
		*r *= cos(*c);          /* real part */
	} /*endif*/

	/* back transformation */
	for  ( c=fftarr+3; c<(fftarr+serieslth); c += 2 )
		*c = -(*c);
	nr_realft( fftarr-1, serieslth/2, -1 );

	/* copy results to output array */
	norm = 2.0 / (float)serieslth;
	for  ( c=fftarr; c<(fftarr+lth); *dst++ = norm * (*c++) )  {}

	/* free memory */
	sy_deallocmem( fftarr );
	sy_deallocmem( lna );

} /* end of ff_mindelay */



/*-------------------------------------------------------------------------*/



void ff_specdiv( SAMPLE f1[], SAMPLE f2[], long lth, REAL dt,
	REAL wlevel, REAL alpha, REAL offset, SAMPLE res[], STATUS *status )

/* divides spectrum of f1 by spectrum of f2 and stores the result
 * in res.  The formula is:
 *    res[i] = f1[i]*f2^[i]/phi[i] * G[i],  where
 *    phi[i] = max{f2[i]*f2^[i], wlevel*max[f2[k]*f2^[k]]}
 *    G[i] = exp( -w[i]*w[i]/(4*alpha*alpha) )
 *    w[i]:   circular frequency
 *    ^:      complex conjugate
 *
 * parameters of routine
 * SAMPLE     f1[], f2[];     input; input wavelets
 * long       lth;            input; length of traces
 * REAL       wlevel;         input; water level
 * REAL       alpha;          input; width of gauss peak
 * REAL       offset;         input; time offset of output trace
 * SAMPLE     res[];          output; results trace
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	long     serieslth;         /* length of FFT arrays */
	long     i;                 /* counter */
	REAL     *ff1, *ff2, *ff3;  /* pointers to output arrays */
	REAL     *p1, *p2, *p3;     /* moving pointers */
	REAL     norm;              /* normalization */
	REAL     d_omega;           /* frequency step */
	REAL     maxdiv;            /* maximum of trace f2 */
	REAL     shift;             /* time shift */
	REAL     tmp, tmp2, x1, x2; /* scratch */
	REAL     shiftphase;        /* shift phase */

	/* executable code */

	if  (lth <= 0)  {
		*status = FLE_ZEROLTH;
		return;
	} /*endif*/

	/* allocate memory */
	serieslth = ff_next2pow( lth );
	ff1 = (REAL *)sy_allocmem( serieslth, (int)sizeof(REAL), status );
	if  (Severe(status))  return;
	ff2 = (REAL *)sy_allocmem( serieslth, (int)sizeof(REAL), status );
	if  (Severe(status))  {sy_deallocmem(ff1); return;}
	ff3 = (REAL *)sy_allocmem( serieslth, (int)sizeof(REAL), status );
	if  (Severe(status))  {sy_deallocmem(ff1); sy_deallocmem(ff2); return;}

	d_omega = 2.0*SHC_PI/((float)serieslth*dt);
	alpha *= 4.0*alpha;
	shift = offset/dt * 2.0*SHC_PI/(REAL)serieslth;

	/* copy input arrays */
	for  ( p3=f1, p1=ff1; p1<(ff1+lth); *p1++ = *p3++ )  {}
	for  ( p1=ff1+lth; p1<(ff1+serieslth); *p1++ = 0.0 )  {}
	for  ( p3=f2, p1=ff2; p1<(ff2+lth); *p1++ = *p3++ )  {}
	for  ( p1=ff2+lth; p1<(ff2+serieslth); *p1++ = 0.0 )  {}

	/* perform FFT */
	nr_realft( ff1-1, serieslth/2, 1 );
	nr_realft( ff2-1, serieslth/2, 1 );

	/* find maximum in divisor */
	p2 = ff2+2;
	maxdiv = (*ff2)*(*ff2);
	for  (i=1; i<(serieslth/2); i++)  {
		tmp = (*p2)*(*p2) + (*(p2+1))*(*(p2+1));
		if  (tmp > maxdiv)  maxdiv = tmp;
		p2 += 2;
	} /*endif*/
	maxdiv *= wlevel;

	/* compute result */
	*ff3 = (*ff1)*(*ff2);
	tmp = (*ff2)*(*ff2);
	*ff3 /= (tmp < maxdiv) ? maxdiv : tmp;
	*(ff3+1) = (*(ff1+1))*(*(ff2+1));
	tmp = (*(ff2+1))*(*(ff2+1));
	*(ff3+1) /= (tmp < maxdiv) ? maxdiv : tmp;
	p1 = ff1+2;  p2 = ff2+2;  p3 = ff3+2;
	for  (i=1; i<(serieslth/2); i++)  {
		*p3 = re_mul(*p1,*(p1+1),*p2,-(*(p2+1)));
		*(p3+1) = im_mul(*p1,*(p1+1),*p2,-(*(p2+1)));
		if  (shift != 0.)  {
			shiftphase = (REAL)i*shift;
			tmp = cos( shiftphase );
			tmp2 = sin( shiftphase );
			x1 = re_mul(*p3,*(p3+1),tmp,tmp2);
			x2 = im_mul(*p3,*(p3+1),tmp,tmp2);
			*p3 = x1;
			*(p3+1) = x2;
		} /*endif*/
		tmp = (*p2)*(*p2) + (*(p2+1))*(*(p2+1));
		if  (tmp < maxdiv)  tmp = maxdiv;
		*p3 /= tmp;
		*(p3+1) /= tmp;
		tmp = (float)i*d_omega;
		tmp = exp(-tmp*tmp/alpha);
		*p3 *= tmp;
		*(p3+1) *= tmp;
		p1 += 2;
		p2 += 2;
		p3 += 2;
	} /*endif*/

	/* back transformation */
	nr_realft( ff3-1, serieslth/2, -1 );

	/* copy results to output array */
	norm = 2.0 / (float)serieslth;
	for  ( p3=ff3; p3<(ff3+lth); *res++ = norm * (*p3++) )  {}

	sy_deallocmem( ff1 );
	sy_deallocmem( ff2 );
	sy_deallocmem( ff3 );

} /* end of ff_specdiv */



/*-------------------------------------------------------------------------*/



static long ff_next2pow( long l )

/* returns next power of 2
 *
 * parameter of routine
 * long      l;      input; input number
 */
{
	/* local variables */
	long     next;     /* next power of 2 */

	/* executable code */

	next = 1;
	while  (next < l)
		next *= 2;

	return next;

} /* end of ff_next2pow */



/*-------------------------------------------------------------------------*/



static void ff_attmul( long lth, COMPLEX dat[], REAL d_omega, REAL t, REAL nf )

/* attenuates "dat"-array with t* of "t" sec
 *
 * parameters of routine
 * long       lth;        input; length of array to be attenuated
 * COMPLEX    dat[];      modify; array to be attenuated
 * REAL       d_omega;    input; step size in omega
 * REAL       t;          input; t* in sec
 * REAL       nf;         input; nyquist frequency
 */
{
	/* local variables */
	REAL     omega;       /* current frequency */
	long     i;           /* counter */
	COMPLEX  *c;          /* moving pointer */
	COMPLEX  temp, eval;  /* scratch */
	COMPLEX  nyquist;     /* value at nyquist frequency */

	/* executable code */

	nyquist.re = dat[0].im;
	nyquist.im = 0.0;
	dat[0].im = 0.0;

	c = dat + 1;
	for  (i=1; i<lth; i++)  {
		omega = (float)i * d_omega;
		mt_imexp( &temp, ((omega*t / SHC_PI) * log(omega/nf)) );
		mt_rmulcmplx( &eval, &temp, exp(-omega*t/2.0) );
		mt_mulcmplx( &temp, c, &eval );
		c->re = temp.re;
		(c++)->im = temp.im;
	} /*endfor*/
	omega = (float)lth * d_omega;
	mt_imexp( &temp, ((omega*t / SHC_PI) * log(omega/nf)) );
	mt_rmulcmplx( &eval, &temp, exp(-omega*t/2.0) );
	mt_mulcmplx( &temp, &nyquist, &eval );
	nyquist = temp;

	dat[0].im = nyquist.re;

} /* end of ff_attmul */



/*-------------------------------------------------------------------------*/



static void ff_tfvalue( COMPLEX *res, FFT_RATFCT *fct, REAL x )

/* evaluates function at point "x"
 *
 * parameters of routine
 * COMPLEX    *res;        output; value of function at point "x"
 * FFT_RATFCT *fct;        input; function to be evaluated
 * REAL       x;           input; argument
 */
{
	/* local variables */
	int      degree;    /* max of no_of_poles & no_of_zeroes */
	int      i;           /* degree counter */
	COMPLEX  temp, tmp2;  /* scratch */

	/* executable code */

	degree = (fct->no_of_poles > fct->no_of_zeroes) ?
		fct->no_of_poles : fct->no_of_zeroes;

	res->re = 1.0;
	res->im = 0.0;

	for  (i=0; i<degree; i++)  {
		if  (i < fct->no_of_zeroes)  {
			tmp2.re = - fct->zero[i].re;
			tmp2.im = x - fct->zero[i].im;
			mt_mulcmplx( &temp, res, &tmp2 );
			*res = temp;
		} /*endif*/
		if  (i < fct->no_of_poles)  {
			tmp2.re = - fct->pole[i].re;
			tmp2.im = x - fct->pole[i].im;
			mt_divcmplx( &temp, res, &tmp2 );
			*res = temp;
		} /*endif*/
	} /*endfor*/

	res->re *= fct->norm;
	res->im *= fct->norm;

} /* end of ff_tfvalue */



/*-------------------------------------------------------------------------*/



static BOOLEAN ff_freq0ok( FFT_RATFCT *fil )

/* checks whether or not filter "fil" exists at frequency zero
 *
 * parameter of routine
 * FFT_RATFCT  *fil;     input; filter to be checked
 *                       returns TRUE if filter exists at zero
 */
{
	/* local variables */
	int      p;        /* pole counter */

	/* executable code */

	for  (p=0; p < fil->no_of_poles; p++)  {
		if  (Abs(fil->pole[p].re) < SHC_EPSILON)  return FALSE;
		if  (Abs(fil->pole[p].im) < SHC_EPSILON)  return FALSE;
	} /*endfor*/
	return TRUE;

} /* end of ff_freq0ok */



/*-------------------------------------------------------------------------*/



void ff_shorten_zeroes( FFT_RATFCT fct[], int no )

/* shortens zeroes in all filter functions fct[0..no-1]
 *
 * parameters of routine
 * FFT_RATFCT fct[];      modify; filter functions to be shortened
 * int        no;         input; number of filter functions
 */
{
	/* local variables */
	FFT_RATFCT loc;     /* local function */
	int      f;         /* filter counter */
	int      i;         /* counter */
	int      z_poles;   /* number of poles at zero freq. */
	int      z_zeroes;  /* number of zeroes at zero freq. */

	/* executable code */

	/* count zeroes in numerator and denominator */
	z_poles = 0;
	z_zeroes = 0;
	for  (f=0; f<no; f++)  {
		for  (i=0; i < fct[f].no_of_zeroes; i++)
			if  ((fct[f].zero[i].re == 0.0) && (fct[f].zero[i].im == 0.0))
				z_zeroes++;
		for  (i=0; i < fct[f].no_of_poles; i++)
			if  ((fct[f].pole[i].re == 0.0) && (fct[f].pole[i].im == 0.0))
				z_poles++;
	} /*endfor*/

	/* take smaller number */
	if  (z_poles < z_zeroes)  {
		z_zeroes = z_poles;
	} else {
		z_poles = z_zeroes;
	} /*endif*/
	if  (z_zeroes == 0)  return;

	/* shorten */
	for  (f=0; f<no; f++)  {
		loc.norm = fct[f].norm;
		loc.no_of_zeroes = 0;
		for  (i=0; i < fct[f].no_of_zeroes; i++)  {
			if  ((fct[f].zero[i].re == 0.0) &&
				(fct[f].zero[i].im == 0.0) && (z_zeroes > 0))  {
				z_zeroes--;
			} else {
				loc.zero[loc.no_of_zeroes].re = fct[f].zero[i].re;
				loc.zero[(loc.no_of_zeroes)++].im = fct[f].zero[i].im;
			} /*endif*/
		} /*endfor*/
		loc.no_of_poles = 0;
		for  (i=0; i < fct[f].no_of_poles; i++)  {
			if  ((fct[f].pole[i].re == 0.0) &&
				(fct[f].pole[i].im == 0.0) && (z_poles > 0))  {
				z_poles--;
			} else {
				loc.pole[loc.no_of_poles].re = fct[f].pole[i].re;
				loc.pole[(loc.no_of_poles)++].im = fct[f].pole[i].im;
			} /*endif*/
		} /*endfor*/
		fct[f] = loc;
		if  ((z_zeroes == 0) && (z_poles == 0))  return;
	} /*endfor*/

} /* end of ff_shorten_zeroes */



/*-------------------------------------------------------------------------*/



void ff_compress( STATUS *status )

/* combines a cascade of filters read in to a single filter function
 *
 * parameters of routine
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	FFT_RATFCT loc;     /* local filter */
	int      f;         /* filter counter */
	int      i;         /* counter */

	/* executable code */

	if  (no_of_fil_ffv <= 1)  return;

	loc.norm = 1.0;
	loc.no_of_zeroes = 0;
	loc.no_of_poles = 0;
	for  (f=0; f<no_of_fil_ffv; f++)  {
		loc.norm *= fct_ffv[f].norm;
		for  (i=0; i < fct_ffv[f].no_of_zeroes; i++)  {
			if  (loc.no_of_zeroes >= FFC_MAXDEGREE)  {
				*status = FLE_COMPRESS;
				return;
			} /*endif*/
			loc.zero[loc.no_of_zeroes].re = fct_ffv[f].zero[i].re;
			loc.zero[(loc.no_of_zeroes)++].im = fct_ffv[f].zero[i].im;
		} /*endfor*/
		for  (i=0; i < fct_ffv[f].no_of_poles; i++)  {
			if  (loc.no_of_poles >= FFC_MAXDEGREE)  {
				*status = FLE_COMPRESS;
				return;
			} /*endif*/
			loc.pole[loc.no_of_poles].re = fct_ffv[f].pole[i].re;
			loc.pole[(loc.no_of_poles)++].im = fct_ffv[f].pole[i].im;
		} /*endfor*/
	} /*endfor*/

	fct_ffv[0] = loc;
	no_of_fil_ffv = 1;

} /* end of ff_compress */



/*-------------------------------------------------------------------------*/

#ifdef XXX

static void ff_lintaper( float taper, long lth, COMPLEX dat[] )

/* tapers the end of data array (linear taper)
 *
 * parameters of routine
 * float      taper;       input; taper start [0..1]
 * long       lth;         input; length of data array
 * COMPLEX    dat[];       modify; data array to be tapered
 */
{
	/* local variables */
	long     start;    /* start index */
	long     i;        /* index counter */
	float    step;     /*  */
	float    r;        /* scratch */

	/* executable code */

	start = Nlong( (float)lth * taper );
	if  (start >= (lth-1))  return;
	step = 1.0 / (float)(lth-start-1);

	for  (i=start+1; i<lth-1; i++)  {
		r = 1.0 - (float)(i-start) * step;
		dat[i+1].re *= r;
		dat[i+1].im *= r;
	} /*endfor*/
	dat[0].im = 0.0;

} /* end of ff_lintaper */

#endif /* XXX */

/*-------------------------------------------------------------------------*/



static void ff_costaper( float taper, long lth, COMPLEX dat[] )

/* tapers the end of data array (cosine taper)
 *
 * parameters of routine
 * float      taper;       input; taper start [0..1]
 * long       lth;         input; length of data array
 * COMPLEX    dat[];       modify; data array to be tapered
 */
{
	/* local variables */
	long     start;    /* start index */
	long     i;        /* index counter */
	float    step;     /*  */
	float    r;        /* scratch */

	/* executable code */

	start = Nlong( (float)lth * taper );
	if  (start >= (lth-1))  return;
	step = 1.0 / (float)(lth-start-1) * BC_PI;

	for  (i=start+1; i<lth-1; i++)  {
		r = 0.5 * (1.0 + cos( (float)(i-start) * step ));
		dat[i+1].re *= r;
		dat[i+1].im *= r;
	} /*endfor*/
	dat[0].im = 0.0;

} /* end of ff_costaper */



/*-------------------------------------------------------------------------*/



REAL ff_filter_amplitude_old( char filter[], REAL frq, STATUS *status )

/* returns amplitude of filter "filter" at frequency "frq"
 *
 * parameters of routine
 * char       filter[];      input; name of filter
 * REAL       frq;           input; frequency in Hz (no angular frequency)
 * STATUS     *status;       output; return status
 *                           returns amplitude of filter
 */
{
	/* local variables */
	FFT_FILFUNC  filf;       /* filter function */
	COMPLEX      val;        /* complex filter value */

	/* executable code */

	ff_filfunc_input( filter, &filf, status );
	if  (Severe(status))  return 0.0;
	frq *= 2.0*BC_PI;   /* make angular frequency */
	ff_filfvalue( &val, &filf, frq, status );
	ff_free_filfunc( &filf );
	if  (Severe(status))  return 0.0;
	return (sqrt(val.re*val.re + val.im*val.im));

} /* end of ff_filter_amplitude_old */



/*-------------------------------------------------------------------------*/



REAL ff_filter_amplitude( char filter[], REAL frq, STATUS *status )

/* returns amplitude of filter "filter" at frequency "frq"
 *
 * parameters of routine
 * char       filter[];      input; name of filter
 * REAL       frq;           input; frequency in Hz (no angular frequency)
 * STATUS     *status;       output; return status
 *                           returns amplitude of filter
 */
{
	/* local variables */
	FFT_RATFCT  filf;       /* filter function */
	COMPLEX      val;        /* complex filter value */

	/* executable code */

	ff_read_filter( filter, 1, &filf, status );
	if  (Severe(status))  return 0.0;
	frq *= 2.0*BC_PI;   /* make angular frequency */
	ff_tfvalue( &val, &filf, frq );
	if  (Severe(status))  return 0.0;
	return (sqrt(val.re*val.re + val.im*val.im));

} /* end of ff_filter_amplitude */



/*-------------------------------------------------------------------------*/

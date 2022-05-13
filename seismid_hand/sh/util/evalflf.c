
/* file flf2flt.c
 *      =========
 *
 * version 1, 31-May-93
 *
 * converts FLF files to FLT files
 * K. Stammler, 31-May-93
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


#include <stdio.h>
#include <string.h>
#include <math.h>
#include BASECNST
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_CPAR
#include BC_FLERRORS
#include BC_ERUSRDEF



#define FILCOMMENTCH '!'
#define FILTERMCH '@'
#define REAL float
#define MAXDEGREE 30
#define SHC_DE_FFILT ".FLF"
#define SHC_DE_DFILT ".FLT"
#define FILMAGIC 1357913578L
#define FILID_TFRAT 1


#ifdef sy_alert
#undef sy_alert
#endif
#define sy_alert(s) printf(s);printf("\n");


typedef struct {
	REAL     re, im;
} COMPLEX;

typedef struct ratfct {
	REAL     norm;             /* normalisation */
	int      no_of_zeroes;     /* number of zeroes */
	COMPLEX  zero[MAXDEGREE];  /* zeroes of transfer function */
	int      no_of_poles;      /* number of poles */
	COMPLEX  pole[MAXDEGREE];  /* poles of transfer function */
} RATFCT;



/* global variables */
char shd_filter[BC_FILELTH+1];



/* prototypes of local routines */
void ff_read_filter( char file[], int pos, RATFCT *filter, STATUS *status );
static void ff_readcoeff( FILE *f, int *no, COMPLEX coeff[], STATUS *status );
static void ff_tfvalue( COMPLEX *res, RATFCT *fct, REAL x );
void mt_mulcmplx( COMPLEX *res, COMPLEX *fac1, COMPLEX *fac2 );
void mt_divcmplx( COMPLEX *res, COMPLEX *num, COMPLEX *denom );



int main( int argc, char *argv[] )
{
	/* local variables */
	char     filname[BC_FILELTH+1];    /* filter name */
	int      pos;                      /* position */
	float    freq;                     /* frequency in Hz */
	STATUS   status;                   /* return status */
	RATFCT   fil;                      /* filter function */
	COMPLEX  tf;                       /* value of transfer function */
	float    tfmod, tfphase;           /* amplitude & phase */
#	ifdef BC_SUN
	char     *eptr;                    /* pointer to environment */
#	endif

	/* executable code */

	sy_initprocess();

#	ifdef BC_SUN
	eptr = (char *)getenv( "SH_FILTER" );
	if  (eptr != NULL)  {
		strcpy( shd_filter, eptr );
	} else {
		*shd_filter = '\0';
	} /*endif*/
#	else
	strcpy( shd_filter, "SHC_FILTER:" );
#	endif

	pa_init( argc, argv );
	if  (pa_pnumber() != 2)  {
		sy_alert( "*** Usage: evalflf <filter> <freq> ***" );
		return 1;
	} /*endif*/

	strcpy( filname, pa_pvalue(1) );
	sscanf( pa_pvalue(2), "%f", &freq );
	pos = 1;

	status = BC_NOERROR;
	ff_read_filter( filname, pos, &fil, &status );
	if  (Severe(&status))  {
		sy_alert( "*** error reading filter ***" );
		return 1;
	} /*endif*/

	ff_tfvalue( &tf, &fil, 2.0*BC_PI*freq );
	tfmod = sqrt( tf.re*tf.re + tf.im*tf.im );
	tfphase = atan2( tf.im, tf.re );
	printf( "%e %e\n", tfmod, tfphase );

	return 0;

} /* end of main */



/*-------------------------------------------------------------------------*/



void ff_read_filter( char file[], int pos, RATFCT *filter, STATUS *status )

/* reads filter from filter file "file" at position number "pos"
 *
 * parameters of routine
 * char       file[];   input; name of filter file
 * int        pos;      input; position; if negative filter is inverted
 * RATFCT     *filter;  output; filter read from file
 * STATUS     *status;  output; return status
 */
{
	/* local variables */
	FILE     *ff;                   /* filter file */
	char     str[BC_LONGSTRLTH+1];  /* scratch */
	int      i;                     /* counter */
	long     magic;                 /* magic longword */
	int      id;                    /* store ID */

	/* executable code */

	/* open filter file */
	if  (strlen(file) > (BC_LONGSTRLTH-4))  {
		*status = FLE_STROVFL;
		return;
	} /*endif*/
	strcpy( str, file );
	strcat( str, SHC_DE_FFILT );
	ff = sy_fopen( str, "r" );
	if  (ff == NULL)  {
		if  ((strlen(file)+strlen(shd_filter)) > BC_LONGSTRLTH)  {
			*status = FLE_STROVFL;
			return;
		} /*endif*/
		strcpy( str, shd_filter );
		strcat( str, file );
		strcat( str, SHC_DE_FFILT );
		ff = sy_fopen( str, "r" );
		if  (ff == NULL)  {
			*status = FLE_OPNREAD;
			err_setcontext( " ## file " ); err_setcontext( str );
			return;
		} /*endif*/
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
	if  (*no > MAXDEGREE)  {
		*status = FLE_DEGOVFL; fclose( f ); return;
	} /*endif*/
	for  ( i = 0; i < *no; i++ )  {
		if  (fscanf( f, "(%e,%e)\n", &(coeff[i].re), &(coeff[i].im) ) != 2)  {
			*status = FLE_FREAD; fclose( f ); return;
		} /*endif*/
	} /*endfor*/

} /* end of ff_readcoeff */



/*-------------------------------------------------------------------------*/



static void ff_tfvalue( COMPLEX *res, RATFCT *fct, REAL x )

/* evaluates function at point "x" (circular frequencies !)
 *
 * parameters of routine
 * COMPLEX    *res;        output; value of function at point "x"
 * RATFCT     *fct;        input; function to be evaluated
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



/*--------------------------------------------------------------------------*/



void mt_mulcmplx( COMPLEX *res, COMPLEX *fac1, COMPLEX *fac2 )

/* multiplies two complex numbers
 *
 * parameters of routine
 * COMPLEX    *res;         output; result of multiplication
 * COMPLEX    *fac1, *fac2; input; factors
 */
{
	/* executable code */

	res->re = fac1->re * fac2->re - fac1->im * fac2->im;
	res->im = fac1->re * fac2->im + fac1->im * fac2->re;

} /* end of mt_mulcmplx */



/*--------------------------------------------------------------------------*/



void mt_divcmplx( COMPLEX *res, COMPLEX *num, COMPLEX *denom )

/* divides two complex numbers (*res = *num / *denom)
 *
 * parameters of routine
 * COMPLEX    *res;         output; result of multiplication
 * COMPLEX    *num, *denom; input; numerator & denominator
 */
{
	/* local variables */
	REAL     absval;    /* squared absolute value of denominator */

	/* executable code */

	absval = denom->re * denom->re + denom->im * denom->im;
	res->re = (num->re * denom->re + num->im * denom->im) / absval;
	res->im = (num->im * denom->re - num->re * denom->im) / absval;

} /* end of mt_divcmplx */



/*-------------------------------------------------------------------------*/


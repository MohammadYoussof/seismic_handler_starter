
/* file fconcat.c
 *      =========
 *
 * version 1, 27-Jul-92
 *
 * concatenates two FFT filter files
 * K. Stammler, 27-Jul-92
 *
 * syntax:  fconcat [-i] <fil1> <fil2> <outfil>
 *    if  "-i" is specified the first filter <fil1> is inverted.
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
#include BC_SYSBASE
#include BC_CPAR



/* global type */
typedef struct {
	float      re;
	float      im;
} COMPLEX;

#define MAXDEGREE 30
	/* not larger than 32 */
#define FMAGIC 1357913578L



/* prototypes of local routines */
BOOLEAN cequal( COMPLEX *c1, COMPLEX *c2 );



int main( int argc, char *argv[] )
{
	/* local variables */
	char     f1name[BC_FILELTH+1];    /* name of first filter */
	char     f2name[BC_FILELTH+1];    /* name of second filter */
	char     oname[BC_FILELTH+1];     /* name of output file */
	FILE     *fp;                     /* file pointer */
	BOOLEAN  invert;                  /* invert first filter */
	int      no_of_zeroes;            /* number of zeroes */
	int      no_of_poles;             /* number of poles */
	COMPLEX  zero[MAXDEGREE];         /* zeroes */
	COMPLEX  pole[MAXDEGREE];         /* poles */
	COMPLEX  xzero[MAXDEGREE];        /* zeroes (copy) */
	COMPLEX  xpole[MAXDEGREE];        /* poles (copy) */
	float    norm;                    /* normalization */
	char     str[BC_LINELTH+1];       /* current line */
	long     magic;                   /* magic number */
	int      num;                     /* number */
	int      i;                       /* counter */
	float    tmp;                     /* scratch */
	int      p, z;                    /* counters */
	long     polemap, zeromap;        /* shorten maps */
	BOOLEAN  printlog;                /* print info text */

	/* executable code */

	/* get parameters */
	pa_init( argc, argv );
	if  (pa_pnumber() != 3)  {
		printf( "*** Usage: fconcat [-i] <fil1> <fil2> <outfil> ***\n" );
		return 0;
	} /*endif*/
	strcpy( f1name, pa_pvalue(1) );
	strcat( f1name, ".FLF" );
	strcpy( f2name, pa_pvalue(2) );
	strcat( f2name, ".FLF" );
	strcpy( oname, pa_pvalue(3) );
	strcat( oname, ".FLF" );
	invert = pa_qspecified( "-i" );
	printlog = pa_qspecified( "-l" );

	no_of_zeroes = 0;
	no_of_poles = 0;

	/* read first file */
	fp = sy_fopen( f1name, "r" );
	if  (fp == NULL)  {
		printf( "*** couldn't find input file %s ***\n", f1name );
		return 0;
	} /*endif*/
	/* read off comments */
	do  {
		if  (fgets( str, BC_LINELTH, fp ) == NULL)  {
			printf( "*** read error on input file %s ***\n", f1name );
			sy_fclose( fp );
			return 0;
		} /*endif*/
	} while (*str == '!');
	/* check magic number */
	sscanf( str, "%ld", &magic );
	if  (magic != FMAGIC)  {
		printf( "*** this file is not a filter file ***\n" );
		sy_fclose( fp );
		return 0;
	} /*endif*/
	/* check store ID */
	fscanf( fp, "%d\n", &num );
	if  (num != 1)  {
		printf( "*** this file is not an FFT filter file ***\n" );
		sy_fclose( fp );
		return 0;
	} /*endif*/
	/* read normalization */
	fscanf( fp, "%f\n", &tmp );
	if  (tmp == 0.)  {
		printf( "*** illegal normalization ***\n" );
		sy_fclose( fp );
		return 0;
	} /*endif*/
	norm = (invert) ? 1.0/tmp : tmp;
	/* read poles & zeroes */
	if  (invert)  {
		fscanf( fp, "%d\n", &no_of_poles );
		if  (no_of_poles > MAXDEGREE)  {
			printf( "*** too many zeroes in filter 1 ***\n" );
			sy_fclose( fp );
			return 0;
		} /*endif*/
		for  (i=0; i<no_of_poles; i++)
			fscanf( fp, "(%f,%f)\n", &(pole[i].re), &(pole[i].im) );
		fscanf( fp, "%d\n", &no_of_zeroes );
		if  (no_of_zeroes > MAXDEGREE)  {
			printf( "*** too many poles in filter 1 ***\n" );
			sy_fclose( fp );
			return 0;
		} /*endif*/
		for  (i=0; i<no_of_zeroes; i++)
			fscanf( fp, "(%f,%f)\n", &(zero[i].re), &(zero[i].im) );
	} else {
		fscanf( fp, "%d\n", &no_of_zeroes );
		if  (no_of_zeroes > MAXDEGREE)  {
			printf( "*** too many zeroes in filter 1 ***\n" );
			sy_fclose( fp );
			return 0;
		} /*endif*/
		for  (i=0; i<no_of_zeroes; i++)
			fscanf( fp, "(%f,%f)\n", &(zero[i].re), &(zero[i].im) );
		fscanf( fp, "%d\n", &no_of_poles );
		if  (no_of_poles > MAXDEGREE)  {
			printf( "*** too many poles in filter 1 ***\n" );
			sy_fclose( fp );
			return 0;
		} /*endif*/
		for  (i=0; i<no_of_poles; i++)
			fscanf( fp, "(%f,%f)\n", &(pole[i].re), &(pole[i].im) );
	} /*endif*/
	sy_fclose( fp );

	/* append second file */
	fp = sy_fopen( f2name, "r" );
	if  (fp == NULL)  {
		printf( "*** couldn't open input file %s ***\n" );
		return 0;
	} /*endif*/
	/* read off comments */
	do  {
		if  (fgets( str, BC_LINELTH, fp ) == NULL)  {
			printf( "*** read error on input file %s ***\n", f2name );
			sy_fclose( fp );
			return 0;
		} /*endif*/
	} while (*str == '!');
	/* check magic number */
	sscanf( str, "%ld", &magic );
	if  (magic != FMAGIC)  {
		printf( "*** this file is not a filter file ***\n" );
		sy_fclose( fp );
		return 0;
	} /*endif*/
	/* check store ID */
	fscanf( fp, "%d\n", &num );
	if  (num != 1)  {
		printf( "*** this file is not an FFT filter file ***\n" );
		sy_fclose( fp );
		return 0;
	} /*endif*/
	/* read normalization */
	fscanf( fp, "%f\n", &tmp );
	norm *= tmp;
	/* read zeroes */
	fscanf( fp, "%d\n", &num );
	if  (no_of_zeroes+num > MAXDEGREE)  {
		printf( "*** too many zeroes ***\n" );
		sy_fclose( fp );
		return 0;
	} /*endif*/
	for  (i=0; i<num; i++)  {
		fscanf( fp, "(%f,%f)\n", &(zero[no_of_zeroes].re),
			&(zero[no_of_zeroes].im) );
		no_of_zeroes++;
	} /*endfor*/
	/* read poles */
	fscanf( fp, "%d\n", &num );
	if  (no_of_poles+num > MAXDEGREE)  {
		printf( "*** too many poles ***\n" );
		sy_fclose( fp );
		return 0;
	} /*endif*/
	for  (i=0; i<num; i++)  {
		fscanf( fp, "(%f,%f)\n", &(pole[no_of_poles].re),
			&(pole[no_of_poles].im) );
		no_of_poles++;
	} /*endfor*/
	sy_fclose( fp );

	/* shorten, result in xzero,z, xpole,p */
	polemap = zeromap = 0L;
	for  (z=0; z<no_of_zeroes; z++)  {
		for  (p=0; p<no_of_poles; p++)  {
			if  (cequal(zero+z,pole+p) && !(zeromap & (1<<z)) &&
				!(polemap & (1<<p)))  {
				polemap |= (1<<p);
				zeromap |= (1<<z);
				if  (printlog)
					printf( "(%f,%f) shortened\n", zero[z].re, zero[z].im );
			} /*endif*/
		} /*endfor*/
	} /*endfor*/
	z = 0;
	for  (i=0; i<no_of_zeroes; i++)  {
		if  (!(zeromap & (1<<i)))  {
			xzero[z++] = zero[i];
		} /*endif*/
	} /*endfor*/
	p = 0;
	for  (i=0; i<no_of_poles; i++)  {
		if  (!(polemap & (1<<i)))  {
			xpole[p++] = pole[i];
		} /*endif*/
	} /*endfor*/

	/* write output file */
	fp = sy_fopen( oname, "w" );
	if  (fp == NULL)  {
		printf( "*** couldn't open output file %s ***\n", oname );
		return 0;
	} /*endif*/
	fprintf( fp, "! concatenation of two filter files\n" );
	if  (invert)  {
		fprintf( fp, "! file 1: %s (inverted)\n", f1name );
	} else {
		fprintf( fp, "! file 1: %s\n", f1name );
	} /*endif*/
	fprintf( fp, "! file 2: %s\n", f2name );
	fprintf( fp, "%ld\n", FMAGIC );
	fprintf( fp, "%d\n", 1 );
	fprintf( fp, "%e\n", norm );
	fprintf( fp, "%d\n", z );
	for  (i=0; i<z; i++)
		fprintf( fp, "(%e,%e)\n", xzero[i].re, xzero[i].im );
	fprintf( fp, "%d\n", p );
	for  (i=0; i<p; i++)
		fprintf( fp, "(%e,%e)\n", xpole[i].re, xpole[i].im );
	sy_fclose( fp );

	return 0;

} /* end of main */



/*--------------------------------------------------------------------*/



BOOLEAN cequal( COMPLEX *c1, COMPLEX *c2 )

/* returns whether to complex numbers are equal
 *
 * parameters of routine
 * COMPLEX    *c1, *c2;     input; numbers to be compared
 *                          returns TRUE if numbers are equal
 */
{
	/* executable code */

	return ((fabs(c1->re - c2->re) < 1.0e-4) &&
		(fabs(c1->im - c2->im) < 1.0e-4));

} /* end of cequal */



/*--------------------------------------------------------------------*/


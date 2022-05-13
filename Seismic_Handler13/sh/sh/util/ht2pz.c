
/* file ht2pz.c
 *      =======
 *
 * version 1, 14-Jul-2003
 *
 * Computes zeros from h0,T0
 * K. Stammler, 14-Jul-2003
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
/* #include "basecnst.h" */
/* #include "sysbase.h" */


/* global type */
typedef struct {
	float    re;    /* real part */
	float    im;    /* imaginary part */
} COMPLEX;



/* prototypes of local routines */
void poly_z( float h, float w, COMPLEX *s1, COMPLEX *s2 );



int main( int argc, char *argv[] )
{
	/* local variables */
	float    h0, t0, w0;           /* seismometer constants */
	COMPLEX  zero[2];              /* zeroes */

	/* executable code */

	if  (argc != 3)  {
		printf( "Usage: %s <h> <T>\n", argv[0] );
		printf( "  <h>  damping constant\n" );
		printf( "  <T>  eigenperiod\n" );
		return 0;
	} /*endif*/

	/* read parameters */
	if  (sscanf( argv[1], "%f", &h0 ) != 1)  {
		printf( "*** couldn't read h0 ***\n" );
		return 0;
	} /*endif*/
	if  (sscanf( argv[2], "%f", &t0 ) != 1)  {
		printf( "*** couldn't read t0 ***\n" );
		return 0;
	} /*endif*/

	w0 = 2.0*M_PI/t0;

	poly_z( h0, w0, zero, zero+1 );

	printf( "( %g, %g )\n", zero[0].re, zero[0].im );
	printf( "( %g, %g )\n", zero[1].re, zero[1].im );

	return 0;

} /* end of main */



/*-----------------------------------------------------------------------------*/



void poly_z( float h, float w, COMPLEX *s1, COMPLEX *s2 )

/* Berechnet die Nullstellen des Polynoms  s**2 + 2*h*w*s + w**2
 * nach der Formel
 * NS = -w * (h +- sqrt(h**2-1))
 *
 * parameters of routine
 * real          h       ! Input; (Daempfung)
 * real          w       ! Input; (Eigenfrequenz)
 * FCOMPLEX      *s1     ! Output; 1. Nullstelle
 * FCOMPLEX      *s2     ! Output; 2. Nullstelle
 */
{
	/* local variables */
	float    discrim;       /* Diskriminante */

	/* executable code */

	discrim = h*h - 1.0;

	if  (discrim < 0.0)  {  /* komplexe Nullstellen */
		s1->re = 0.0;
		s1->im = sqrt(-discrim);
		s2->re = 0.0;
		s2->im = -s1->im;
	} else {               /* reelle NS */
		s1->re = sqrt(discrim);
		s1->im = 0.0;
		s2->re = -s1->re;
		s2->im = 0.0;
	} /*endif*/

	s1->re += h;
	s2->re += h;
	s1->re *= -w;
	s1->im *= -w;
	s2->re *= -w;
	s2->im *= -w;

} /* end of poly_z */



/*--------------------------------------------------------------------------*/

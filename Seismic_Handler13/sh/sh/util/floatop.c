
/* file floatop.c
 *      ==========
 *
 * version 6, 4-Apr-98
 *
 * floating operation with two numbers
 * K. Stammler, 23-Dec-94
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
#include "basecnst.h"
#include "sysbase.h"
#include "cpar.h"


int main( int argc, char *argv[] )
{
	/* local variables */
	float    op1, op2;              /* float values */
	char     str1[cBcLineLth+1];    /* first operand string */
	char     str2[cBcLineLth+1];    /* second operand string */
	char     op[cBcLineLth+1];      /* operation */
	char     fmt[cBcLineLth+1];     /* output format */

	/* executable code */

	pa_init( argc, argv );

	if  (pa_pnumber() != 3)  {
		fprintf( stderr, "Usage: %s <op1> <op> <op2>\n", pa_progname() );
		return 1;
	} /*endif*/

	/* care for 'm's as signs on operands */
	strcpy( str1, pa_pvalue(1) );
	strcpy( str2, pa_pvalue(3) );
	if  (*str1 == 'm')  *str1 = '-';
	if  (*str2 == 'm')  *str2 = '-';

	sscanf( str1, "%f", &op1 );
	strcpy( op, pa_pvalue(2) );
	sscanf( str2, "%f", &op2 );

	strcpy( fmt, "%f\n" );
	if  (pa_qspecified("-fmt"))  {
		strcpy( fmt, pa_qvalue("-fmt") );
	} /*endif*/

	if  (strcmp(op,"mul") == 0)  {
		printf( fmt, op1*op2 );
	} else if  (strcmp(op,"div") == 0)  {
		printf( fmt, op1/op2 );
	} else if  (strcmp(op,"+") == 0)  {
		printf( fmt, op1+op2 );
	} else if  (strcmp(op,"minus") == 0)  {
		printf( fmt, op1-op2 );
	} else if  (strcmp(op,"gt") == 0)  {
		printf( "%d\n", (op1 > op2) );
	} else if  (strcmp(op,"gte") == 0)  {
		printf( "%d\n", (op1 >= op2) );
	} else if  (strcmp(op,"lt") == 0)  {
		printf( "%d\n", (op1 < op2) );
	} else if  (strcmp(op,"lte") == 0)  {
		printf( "%d\n", (op1 <= op2) );
	} else if  (strcmp(op,"eq") == 0)  {
		printf( "%d\n", (op1 == op2) );
	} else if  (strcmp(op,"log") == 0)  {
		if  (Nint(op2) == 10)  {
			printf( fmt, log10(op1) );
		} else {
			printf( fmt, log(op1) );
		} /*endif*/
	} else if  (strcmp(op,"pow") == 0)  {
		printf( fmt, pow(op1,op2) );
	} else if  (strcmp(op,"trunc") == 0)  {
		printf( "%d\n", (int)op1 );
	} else if  (strcmp(op,"sin") == 0)  {
		printf( fmt, sin(op1) );
	} else if  (strcmp(op,"cos") == 0)  {
		printf( fmt, cos(op1) );
	} else {
		fprintf( stderr, "%s: illegal operation %s\n", pa_progname(), op );
		return 1;
	} /*endif*/

	return 0;

} /* end of main */


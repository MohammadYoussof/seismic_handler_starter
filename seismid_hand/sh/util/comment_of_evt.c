
/* file comment_of_evt.c
 *      ================
 *
 * version 1, 5-Feb-2002
 *
 * Extracts comment out of evt file.
 * K. Stammler, 5-Feb-2002
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

#define STRLTH 256


int main( int argc, char *argv[] )
{
	/* local variables */
	char     fname[STRLTH+1];     /* name of evt file */
	FILE     *fp;                 /* pointer to input file */
	char     str[STRLTH+1];       /* current line of file */
	int      cont;                /* continue comment */
	int      i;                   /* counter */
	int      ch;                  /* character read */

	/* executable code */

	if  (argc != 2)  {
		fprintf( stderr, "Usage: %s <evtfile>\n", argv[0] );
		return 1;
	} /*endif*/

	strcpy( fname, argv[1] );
	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: input file %s not found.  Abort.\n",
			argv[0], fname );
		return 1;
	} /*endif*/

	cont = 0;
	while  (fgets(str,STRLTH,fp) != NULL)  {
		if  (strncmp("Comment                :",str,24) == 0)  {
			cont = 1;
			i = 24;
			while  (str[i] != '\0')  {
				if  (str[i] == '~')  {
					fputc( '\n', stdout );
					cont = 0;
					break;
				} else {
					fputc( str[i], stdout );
				} /*endif*/
				i++;
			} /*endwhile*/
			break;
		} /*endif*/
	} /*endwhile*/

	if  (cont)  {
		while  (EOF != (ch = fgetc(fp)))  {
			if  (ch == '~')  {
				fputc( '\n', stdout );
				break;
			} else {
				fputc( ch, stdout );
			} /*endif*/
		} /*endwhile*/
	} /*endif*/

	fclose( fp );
	return 0;

} /* end of main */

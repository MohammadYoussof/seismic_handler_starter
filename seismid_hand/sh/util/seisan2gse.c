
/* file seisan2gse.c
 *      ============
 *
 * version 4, 30-Aug-99
 *
 * Reads in seisan-file and writes GSE1.0 output
 *
 * Written at SSB for purpose of converting the local data files.  The seisan
 * format description was not detailed enough, since mostly given as FORTRAN output
 * statements.  Some things had to be guessed or fixed in such a way that the
 * conversion worked just for the existing data files.  In question are the following
 * points:
 * - the file header is ignored, its length is assumed to be fixed at 12 lines of 80
 *   characters each.  In addition there are 2 bytes fortran format control characters
 *   per line and another 2 bytes at the end of the header expected.
 * - the channel header has been found at a length of 1056 bytes instead of 1040.
 *   This is probably due to fortran control characters added to the output.
 * - the calibration constant was set only in a few data files.  For reasons of
 *   compatibility is has been set to 1.0 in all GSE output files.
 * - the data section is assumed to consist of 2-byte integers.
 * - within the data section additional control characters were found after every 65
 *   2-byte samples.  These were thrown out in the hope not to delete any samples
 *   on some occasions.  At the beginning and the end of the data section also two
 *   2-byte values are ignored.
 * - for some reason the last channel header in each file has slightly different
 *   formatting so that the station name and the component are not recognized
 *   properly.
 * K. Stammler, Beijing, 8-Jul-98
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

#define FILELTH 132
#define STRLTH 80
#define CHHDRLTH 1056
#define LTHOFFSET 44
#define RATEOFFSET 36
#define FORTCTRL 65
#define NPL 8
#define CALIBOFFSET 202
#define EOC 999


/* prototypes of local routines */
static void SgSkipFileHeader( FILE *fp );
static void SgPutChars( int c[], char str[] );


/* constants */
static int gse[] = {
	'W', 'I', 'D', '1', ' ', ' ', '1', '9', -10, -11, -13, -14, -15, ' ',
		-23, -24, ' ', -26, -27, ' ', -29, -30, ' ', -32, -33, -34, ' ', ' ',
		-43, -44, -45, -46, -47, -48, -49, ' ', 0, -1, -2, -3, ' ', ' ',
		0, -1, -2, -3, -5, -8, ' ', ' ', ' ', ' ', -5, -8,
		-36, -37, -38, -39, -40, -41, -42,
		' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
		'I', 'N', 'T', 'V', ' ', '0', '\n',
	' ', ' ', ' ', ' ', ' ', ' ', '1', '.', '0', '1',
		'-', '1', '.', '0', '0', '0', '0', ' ', '-', '9', '9', '9', '.', '0', '0',
		'0', '0', ' ', '-', '9', '9', '9', '.', '0', '0', '0', '0', ' ', '-', '9',
		'9', '9', '.', '0', '0', '0', '0', ' ', '-', '9', '9', '9', '.', '0', '0',
		'0', '0', ' ', ' ', ' ', '-', '1', '.', '0', '0', ' ', ' ', ' ', '-', '1',
		'.', '0', '0', ' ', ' ', ' ', '-', '1', '.', '0', '\n',
	'D', 'A', 'T', '1', '\n',
	EOC
};
static int asc[] = {
	'S', 'T', 'A', 'T', 'I', 'O', 'N', ':', ' ', 0, -1, -2, '\n',
	'C', 'O', 'M', 'P', ':', ' ', -8, '\n',
	'S', 'T', 'A', 'R', 'T', ':', ' ', -20, -21, ',', -17, -18, ',', -10, -11, ',',
		-23, -24, ',', -26, -27, ',', -29, -30, ',', -32, -33, -34, '\n',
	EOC
};



int main( int argc, char *argv[] )
{
	/* local variables */
	char     fname[FILELTH+1];      /* name of input file */
	float    calib;                 /* calibration value */
	FILE     *fp;                   /* pointer to input file */
	char     hdr[CHHDRLTH+1];       /* channel header */
	int      res;                   /* bytes read */
	int      smplth;                /* number of samples */
	float    smprate;               /* sample rate */
	int      i, j;                  /* counters */
	short int smp;                  /* sample value */
	short unsigned int     *lw, ltmp;     /* words */
	int      w_asc=0;               /* write ascii file */
	int      chksum;                /* GSE checksum */
	char     str[STRLTH+1];         /* scratch string */
	float    ftmp;                  /* scratch */
	int      ctrlcnt;               /* counter of fortran controls */

	/* executable code */

#ifdef XXX
	if  (sizeof(float) != sizeof(int))  {
		fprintf( stderr, "%s: length of 'int' and 'float' not equal.  Abort.\n",
			argv[0] );
		return 1;
	} /*endif*/
#endif
	lw = (unsigned short int *)(&smp);

	/* check parameters */
	if  (argc < 2 || argc > 3)  {
		fprintf( stderr, "Usage: %s <seisan-file> [<calibration>]\n", argv[0] );
		return 1;
	} /*endif*/
	strcpy( fname, argv[1] );
	calib = 1.0;
	if  (argc > 2)  sscanf( argv[2], "%f", &calib );

	/* open input file */
	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: input file %s not found.  Abort.\n", argv[0], fname );
		return 1;
	} /*endif*/

	/* skip file header */
	SgSkipFileHeader( fp );

	/* read channel header and data */
	while  (!feof(fp))  {
		/* read channel header */
		res = fread( hdr, 1, CHHDRLTH, fp );
		/* if 0 bytes read came to end of file (above 'feof' didn't work) */ 
		if  (res == 0)  break;
		/* if not all bytes read -> read error, abort */
		if  (res != CHHDRLTH)  {
			fprintf( stderr, "%s: only %d bytes read instead of %d.  Abort.\n",
				argv[0], res, CHHDRLTH );
			fclose( fp );
			return 1;
		} /*endif*/
		/* get number of samples and sample rate from header */
		sscanf( hdr+LTHOFFSET, "%d", &smplth );
		sscanf( hdr+RATEOFFSET, "%f", &smprate );
		/* write out header info */
		if  (w_asc)  {
			/* write SH ASCII files */
			printf( "\nLENGTH: %d\n", smplth );
			SgPutChars( asc, hdr );
			if  (smprate > 0)  {
				printf( "DELTA: %f\n", 1.0/smprate );
			} else {
				printf( "DELTA: 1.0\n" );
				fprintf( stderr, "%s: illegal sample rate set to 1.0\n", argv[0] );
			} /*endif*/
		} else {
			/* write GSE1.0 files */
#			ifdef XXX
			/* This has been removed, since not all data files had calibrations set. */
			/* The default 1.0 is set above in the 'gse'-string */
			/* ---------------------- */
			/* multiply calibration constant with 1.0e-9; bit complicated to do here */
			strncpy( str, hdr+CALIBOFFSET, 8 );
			str[8] = '\0';
			ftmp = 0.0;
			sscanf( str, "%f", &ftmp );
			ftmp *= 1.0e-9;
			if  (ftmp == 0.0)  {
				fprintf( stderr, "%s: found zero calibration, set to 1.0\n", argv[0] );
				ftmp = 1.0;
			} /*endif*/
#			endif
			/* write calibration constant to header */
			ftmp = calib;
			sprintf( str, "%9g", ftmp );
			for  (i=0; i<9; i++)
				gse[81+i] = (int)str[i];
			/* write out header */
			SgPutChars( gse, hdr );
		} /*endif*/
		/* read samples and write to output */
		chksum = 0;
		i = j = 0;  /* i=samples, j=read operatons */
		ctrlcnt = 0;
		while  (i < smplth)  {
			fread( &smp, sizeof(short int), 1, fp );
#			ifdef XXX
			/* swap data */
			ltmp = ((*lw) & 0x00ff) << 8;
			*lw = ((*lw & 0xff00) >> 8) | ltmp;
#			endif
			if  (j % FORTCTRL != 0)  {
				if  (++i % NPL == 0)  printf( "\n" );
				printf( "%d ", (int)(smp) );
				chksum += smp;
			} else {
				ctrlcnt++;
#				ifdef XXX
				if  (smp != -32383)
					fprintf( stderr, "%s: Fortran ctrl is %d\n", argv[0], smp );
#				endif
			} /*endif*/
			j++;
		} /*endwhile*/
		/* throw away another 2 bytes after the data section */
		fread( &smp, sizeof(short int), 1, fp );
		/* write checksum in GSE file */
		if  (!w_asc)  printf( "\nCHK1 %d\n\n", chksum );
	} /*endwhile*/

	/* close input file */
	fclose( fp );

	return 0;

} /* end of main */



/*-----------------------------------------------------------------------------------*/


#define FHDRLTH ((12*82)+2)
	/* 12 lines a 80+2 chars (2 fortran controls) */


static void SgSkipFileHeader( FILE *fp )

/* Reads off file header
 *
 * parameters of routine
 * FILE       *fp;           input; pointer to input file
 */
{
	/* executable code */

	/* position file */
	fseek( fp, FHDRLTH, 0 );

} /* end of SgSkipFileHeader */



/*-----------------------------------------------------------------------------------*/



static void SgPutChars( int c[], char str[] )

/* Prints ASCII character or offset character from string
 *
 * parameters of routine
 * signed char   c[];      input; control numbers of ASCII values
 * char          str;      input; output characters
 */
{
	/* local variables */
	int      i;            /* counter */

	/* executable code */

	i = -1;
	while  (c[++i] != EOC)  {
		if  (c[i] > 0)  {
			fputc( c[i], stdout );
		} else {
			fputc( str[-c[i]], stdout );
		} /*endif*/
	} /*endwhile*/

} /* end of SgPutChars */



/*-----------------------------------------------------------------------------------*/

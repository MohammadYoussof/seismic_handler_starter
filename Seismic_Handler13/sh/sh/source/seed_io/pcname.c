
/* file pcname.c
 *      ========
 *
 * version 2, 17-Nov-94
 *
 * returns valid PC name for SEED data file
 * K. Stammler, 15-Nov-94
 */



#include <stdio.h>
#include <string.h>
#include BASECNST
#include BC_SYSBASE
#include BC_TCUSRDEF



int main( int argc, char *argv[] )
{
	/* local variables */
	char     inpname[BC_FILELTH+1];      /* input name */
	char     outdir[BC_FILELTH+1];       /* output directory */
	char     outfile[BC_FILELTH+1];      /* name of output file */
	NTIME    ntime;                      /* numeric time */
	char     *src, *dst;                 /* moving pointers */
	int      itmp;                       /* scratch */
	char     str[BC_LINELTH+1];          /* scratch */

	/* executable code */

	if  (argc != 2)  {
		fprintf( stderr, "Usage: %s <input-name>\n", argv[0] );
		return 1;
	} /*endif*/

	strcpy( inpname, argv[1] );

	if  (strchr(inpname,'_') != NULL)  {

		/* copy station name until '_' */
		src = inpname;
		dst = outdir;
		while  (*src != '\0' && *src != '_')
			*dst++ = *src++;
		*dst++ = '/';

		/* check for illegal input string */
		if  (*src != '_')  {
			fprintf( stderr, "%s: illegal input name %s\n", argv[0], inpname );
			return 1;
		} /*endif*/

		/* read date */
		src++;
		if  (sscanf(src,"%d",&itmp) != 1)  {
			fprintf( stderr, "%s: couldn't read date in %s\n", argv[0], inpname );
			return 1;
		} /*endif*/
		ntime.day = itmp % 100;
		itmp /= 100;
		ntime.month = itmp % 100;
		itmp /= 100;
		ntime.year = itmp + 1900;
		if  (ntime.year < 1950)  ntime.year += 100;

		/* check for illegal input name */
		if  (src[6] != '_')  {
			fprintf( stderr, "%s: couldn't find time in %s\n", argv[0], inpname );
			return 1;
		} /*endif*/
		src += 7;

		sprintf( dst, "%04d", ntime.year );
		sprintf( outfile, "%02d%02d%s", ntime.month, ntime.day, src );
		printf( "%s %s\n", outdir, outfile );

	} else {

		/* copy station name until '[7-0]' */
		src = inpname;
		dst = outdir;
		while  (*src != '\0' && (*src < '7' || *src > '9'))
			*dst++ = *src++;
		*dst++ = '/';

		/* check for illegal input string */
		if  (*src == '\0')  {
			fprintf( stderr, "%s: illegal input name %s\n", argv[0], inpname );
			return 1;
		} /*endif*/

		/* read date */
		strncpy( str, src, 6 );
		str[6] = '\0';
		if  (sscanf( str, "%d", &itmp ) != 1)  {
			fprintf( stderr, "%s: no date found in %s\n", argv[0], inpname );
			return 1;
		} /*endif*/
		ntime.day = itmp % 100;
		itmp /= 100;
		ntime.month = itmp % 100;
		itmp /= 100;
		ntime.year = itmp + 1900;
		if  (ntime.year < 1950)  ntime.year += 100;

		if  (strlen(src) < 6)  {
			fprintf( stderr, "%s: illegal name %s\n", argv[0], inpname );
			return 1;
		} /*endif*/
		src += 6;
		if  (*src == '.')  {
			sprintf( outfile, "%02d%02d0000%s", ntime.month, ntime.day, src );
		} else {
			sprintf( outfile, "%02d%02d", ntime.month, ntime.day );
			strncpy( outfile+4, src, 4 );
			src += 4;
			while  (*src != '\0' && *src != '.')  src++;
			strcpy( outfile+8, src );
		} /*endif*/

		sprintf( dst, "%04d", ntime.year );
		printf( "%s %s\n", outdir, outfile );

	} /*endif*/

	return 0;

} /* end of main */

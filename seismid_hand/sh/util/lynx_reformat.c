
/* file lynx_reformat.c
 *      ===============
 *
 * version 1, 6-Apr-2006
 *
 * Reformats lynx output by replacing all [..]-expressions by newlines
 * and throwing out all other newlines.
 *
 * K. Stammler, 6-Apr-2006
 */

#include <stdio.h>
#include <string.h>

#define STRLTH 265


int main( int argc, char *argv[] )
{
	/* local variables */
	char     fname[STRLTH+1];     /* name of input file */
	FILE     *fp;                 /* pointer to input file */
	char     dataspec[STRLTH+19]; /* data lines begin with this expression */
	int      c;                   /* current character */
	int      bracket;             /* in bracket? */

	/* executable code */

	if  (argc != 2)  {
		fprintf( stderr, "Usage: %s <inpfile>\n", argv[0] );
		fprintf( stderr, "    <inpfile> = - reads from stdin\n" );
		return 1;
	} /*endif*/

	*dataspec = '\0';

	/* get parameters */
	strcpy( fname, argv[1] );
	if  (argc == 3)  strcpy( dataspec, argv[2] );

	if  (strcmp(fname,"-") == 0)  {
		fp = stdin;
	} else {
		fp = fopen( fname, "r" );
		if  (fp == NULL)  {
			fprintf( stderr, "%s: error opening file %s\n", argv[0], fname );
			return 1;
		} /*endif*/
	} /*endif*/

	bracket = 0;
	while  ((c = fgetc(fp)) != EOF)  {
		switch  ((char)c)  {
		case '\n':    fputc( (int)' ', stdout );   break;
		case '[':     bracket=1;  break;
		case ']':     bracket=0;  fputc( (int)'\n', stdout ); break;
		default:      if  (!bracket) fputc( c, stdout );
		} /*endswitch*/
	} /*endwhile*/

	if  (fp != stdin)  fclose( fp );

	return 0;

} /* end of main */

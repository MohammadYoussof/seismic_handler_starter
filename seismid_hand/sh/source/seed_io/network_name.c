
/* file network_name.c
 *      ==============
 *
 * version 1, 3-Nov-95
 *
 * Reads name of stream and returns name of parent network if any.
 * K. Stammler, 3-Nov-95
 */



#include <stdio.h>
#include <string.h>



#define LINELTH 132
#define Cap(c) (((c)>='a' && (c)<='z') ? ((c)-32) : (c))

int main( int argc, char *argv[] )
{
	/* local variables */
	char     stream[LINELTH+1];     /* stream name */
	char     *cptr;                 /* moving pointer */

	/* executable code */

	if  (argc != 2)  {
		fprintf( stderr, "Usage: %s <stream>\n", argv[0] );
		return 1;
	} /*endif*/
	strcpy( stream, argv[1] );

	/* take only station name (separated by hyphen) */
	cptr = stream - 1 ;
	while  (*(++cptr) != '\0')
		if  (*cptr == '-' || *cptr == ',')  {
			*cptr = '\0';
			break;
		} else {
			*cptr = Cap( *cptr );
		} /*endif*/

	if  (stream[0] == 'G' && stream[1] == 'R')  {
		if  (stream[2] >= 'A' && stream[2] <= 'C'
			&& stream[3] >='1' && stream[3] <= '5')  {
			printf( "GRF\n" );
			return 0;
		} /*endif*/
	} /*endif*/

	if  (stream[0] == 'G' && stream[1] == 'E')  {
		if  (stream[2] >= 'A' && stream[2] <= 'D'
			&& stream[3] >='0' && stream[3] <= '9')  {
			printf( "GERESS\n" );
			return 0;
		} /*endif*/
	} /*endif*/

	if  (strcmp(stream,"BFO") == 0)   {printf( "GRSN\n" ); return 0;}
	if  (strcmp(stream,"BRG") == 0)   {printf( "GRSN\n" ); return 0;}
	if  (strcmp(stream,"BRNL") == 0)  {printf( "GRSN\n" ); return 0;}
	if  (strcmp(stream,"BUG") == 0)   {printf( "GRSN\n" ); return 0;}
	if  (strcmp(stream,"CLL") == 0)   {printf( "GRSN\n" ); return 0;}
	if  (strcmp(stream,"CLZ") == 0)   {printf( "GRSN\n" ); return 0;}
	if  (strcmp(stream,"FUR") == 0)   {printf( "GRSN\n" ); return 0;}
	if  (strcmp(stream,"GRFO") == 0)  {printf( "GRSN\n" ); return 0;}
	if  (strcmp(stream,"GSH") == 0)   {printf( "GRSN\n" ); return 0;}
	if  (strcmp(stream,"LID") == 0)   {printf( "GRSN\n" ); return 0;}
	if  (strcmp(stream,"MOX") == 0)   {printf( "GRSN\n" ); return 0;}
	if  (strcmp(stream,"STU") == 0)   {printf( "GRSN\n" ); return 0;}
	if  (strcmp(stream,"TNS") == 0)   {printf( "GRSN\n" ); return 0;}
	if  (strcmp(stream,"WET") == 0)   {printf( "GRSN\n" ); return 0;}

	printf( "%s\n", stream );

	return 0;

} /* end of main */

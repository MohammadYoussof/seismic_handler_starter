
/* file stringop.c
 *      ==========
 *
 * version 10, 23-Dec-2006
 *
 * string operations
 * K. Stammler, 27-Jul-95
 */



#include <stdio.h>
#include <string.h>


#define STRLTH 200



int main( int argc, char *argv[] )
{
	/* local variables */
	char     str[STRLTH+1];    /* input string */
	char     scr[STRLTH+1];    /* scratch string */
	char     fmt[STRLTH+1];    /* format string */
	char     op[STRLTH+1];     /* operation */
	char     *ptr;             /* moving pointer */
	int      slen;             /* string length */
	int      p1, p2;           /* additional parameters */
	int      i;                /* counter */
	int      bool;             /* boolean value */

	/* executable code */

	if  (argc < 2)  {
		fprintf( stderr, "%s <operation> [<string>] [<p1> [<p2>]]\n", argv[0] );
		return 1;
	} /*endif*/
	strcpy( op, argv[1] );
	if  (argc > 2)  {
		strcpy( str, argv[2] );
	} else {
		if  (fgets( str, STRLTH, stdin ) == NULL)  {
			fprintf( stderr, "%s error reading stdin\n", argv[0] );
			return 1;
		} /*endif*/
		i = strlen( str ) - 1;
		if  (i > 0 && str[i] == '\n')  str[i] = '\0';
	} /*endif*/

	if  (strcmp(op,"upper") == 0)  {
		ptr = str;
		while  (*ptr != '\0')  {
			if  (*ptr >= 'a' && *ptr <= 'z')
				*ptr = *ptr - 32;
			ptr++;
		} /*endwhile*/
		printf( "%s\n", str );
	} else if  (strcmp(op,"lower") == 0)  {
		ptr = str;
		while  (*ptr != '\0')  {
			if  (*ptr >= 'A' && *ptr <= 'Z')
				*ptr = *ptr + 32;
			ptr++;
		} /*endwhile*/
		printf( "%s\n", str );
	} else if  (strcmp(op,"extract") == 0)  {
		if  (argc != 5)  {
			fprintf( stderr, "Usage: %s extract <string> <start> <length>\n",
				argv[0] );
			fprintf( stderr, "       negative <length> extracts to the end\n" );
			return 1;
		} /*endif*/
		sscanf( argv[3], "%d", &p1 );
		sscanf( argv[4], "%d", &p2 );
		slen = strlen( str );
		if  (p1 >= slen)  return 0;   /* no output */
		if  (p1+p2 > slen)  p2 = slen - p1;
		if  (p1+p2 < 0)  p2 = -p1;
		str[p1+p2] = '\0';
		printf( "%s\n", str+p1 );
	} else if  (strcmp(op,"align") == 0 || strcmp(op,"align_r") == 0)  {
		if  (argc != 4)  {
			fprintf( stderr, "Usage: %s align <string> <length>\n",
				argv[0] );
			return 1;
		} /*endif*/
		sscanf( argv[3], "%d", &p1 );
		sprintf( fmt, "%%%ds\n", p1 );
		printf( fmt, str );
	} else if  (strcmp(op,"align_l") == 0)  {
		if  (argc != 4)  {
			fprintf( stderr, "Usage: %s align_l <string> <length>\n",
				argv[0] );
			return 1;
		} /*endif*/
		sscanf( argv[3], "%d", &p1 );
		sprintf( fmt, "%%-%ds\n", p1 );
		printf( fmt, str );
	} else if  (strcmp(op,"unbracket") == 0)  {
		if  (*str == '<' || *str == '(' || *str == '[' || *str == '{')  {
			strcpy( scr, str+1 );
		} else {
			strcpy( scr, str );
		} /*endif*/
		p1 = strlen( scr ) - 1;
		if  (p1 < 0)  {printf( "%s\n", scr ); return 0;}
		if  (scr[p1] == '>' || scr[p1] == ')' || scr[p1] == ']'
			|| scr[p1] == '}')
			scr[p1] = '\0';
		printf( "%s\n", scr );
	} else if  (strcmp(op,"find") == 0)  {
		if  (argc != 4)  {
			fprintf( stderr, "Usage: %s find <string> <substring>\n",
				argv[0] );
			return 1;
		} /*endif*/
		strcpy( scr, argv[3] );
		ptr = strstr( str, scr );
		if  (ptr != NULL)  printf( "%s\n", ptr );
	} else if  (strcmp(op,"length") == 0)  {
		printf( "%d\n", strlen(str) );
	} else if  (strcmp(op,"inc") == 0)  {
		printf( "%c\n", (str[0]+1) );
	} else if  (strcmp(op,"isnum") == 0)  {
		bool = 1;
		slen = strlen( str );
		for  (i=0; i<slen; i++)  {
			if  (str[i] < '0' || str[i] > '9')  {
				bool = 0;
				break;
			} /*endif*/
		} /*endfor*/
		printf( "%d\n", bool );
	} else if  (strcmp(op,"textprefix") == 0)  {
		i = strlen( str ) - 1;
		while  (i >= 0)  {
			if  (str[i] >= '0' && str[i] <= '9')  {
				str[i] = '\0';
			} else {
				break;
			} /*endif*/
			i--;
		} /*endwhile*/
		printf( "%s\n", str );
	} else {
		fprintf( stderr, "%s: illegal operation %s\n", argv[0], op );
		return 1;
	} /*endif*/

	return 0;

} /* end of main */

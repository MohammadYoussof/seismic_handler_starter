/* file netsh.c
 *      =======
 *
 * version 1, 9-Jul-2006
 * 
 * Interface to the SH daemon
 *
 * K. Stammler, 9-Jul-2006
 */


#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#define STRLTH          255
#define MAXBUF          512
#define DEFAULT_PORT    18005

#define TERM "@_@_SHDTERM_@_@"
#define TERMLTH 16


/* local prototypes */
void write_to_server( int filedes, char msg[] );
void read_from_server( int fd, int maxbuf, char buffer[], int *nbytes );
void read_from_server_timeout( int fd, int maxbuf, char buffer[],
	int *nbytes, int *timeout );
void init_sockaddr( struct sockaddr_in *name, const char *hostname,
	uint16_t port);
void parse_cmdline( int argc, char *argv[], char host[], int *portno,
	char msg[], char upload[], int *ok );


/*----------------------------------------------------------------------------*/



int main( int argc, char *argv[] )
{

	/* local variables */
	char     hostname[STRLTH+1];     /* hostname */
	char     message[STRLTH+1];      /* message text to send */
	char     tmpstr[STRLTH+1];       /* scratch string */
	char     upfile[STRLTH+1];       /* file to upload */
	int      portno;                 /* port number */
	int      sock;                   /* socket */
	struct sockaddr_in servername;   /* server name structure */
	char     response[MAXBUF+1];     /* response message */
	int      resplth;                /* length of response message */
	int      ok;                     /* return status */
	int      timeout;                /* timeout occurred */
	int      i;                      /* counter */
	int      termfound;              /* termination string found */
	FILE     *fp;                    /* poiner to file */
	char     line[STRLTH+1];         /* line of file */

	/* executable code */

	if  (argc < 2)  {
		fprintf( stderr, "Usage: %s [-h=<host>] [-p=<port>] [-up=<upload>] <message>\n", argv[0] );
		return 1;
	} /*endif*/

	/* get parameters */
	parse_cmdline( argc, argv, hostname, &portno, message, upfile, &ok );
	if  (!ok)  {
		fprintf( stderr, "%s: illegal command parameters.  Abort\n", argv[0] );
		exit( EXIT_FAILURE );
	} /*endif*/

	/* Create the socket. */
	sock = socket( PF_INET, SOCK_STREAM, 0 );
	if (sock < 0)  {
		perror( "create socket (client)" );
		exit( EXIT_FAILURE );
	} /*endif*/

	/* Connect to the server. */
	init_sockaddr( &servername, hostname, portno );
	if  (0 > connect( sock,(struct sockaddr *) &servername,
		sizeof(servername)))  {
		perror( "connect (client)" );
		exit( EXIT_FAILURE );
	} /*endif*/

	/*
	printf( "host: %s\n", hostname );
	printf( "port: %d\n", portno );
	printf( "upload: %s\n", upfile );
	printf( "message: >%s<\n", message );
	*/

	if  (*upfile != '\0')  {
		if  (strlen(message) > STRLTH-3)  {
			fprintf( stderr, "message too long, cannot add upload info\n" );
			exit( EXIT_FAILURE );
		} /*endif*/
		strcat( message, ":U@" );
	} /*endif*/

	/* Send data to the server. */
	write_to_server( sock, message );
	/*printf( "--> waiting for response ...\n" );*/

	if  (*upfile != '\0')  {
		fp = fopen( upfile, "r" );
		if  (fp == NULL)  {
			fprintf( stderr, "netsh: error opening input file %s.  Abort.\n",
				upfile );
			return 1;
		} /*endif*/
		while  (fgets(line,STRLTH,fp) != NULL)  {
			write_to_server( sock, line );
		} /*endwhile*/
		fclose( fp );
		write_to_server( sock, TERM );
	} /*endif*/

	for  (;;)  {
		read_from_server_timeout( sock, MAXBUF, response, &resplth, &timeout );
		if  (resplth <= 0 || timeout)  break;
		/*printf( "resplth %d\n", resplth );*/
		response[resplth] = '\0';
		/* check for termination string at end of buffer */
		termfound = (resplth >= TERMLTH
			&& strcmp(response+resplth-TERMLTH,TERM) == 0);
		/* remove termination string if found */
		if  (termfound)  {
			response[resplth-TERMLTH] = '\0';
			resplth -= TERMLTH;
		} /*endif*/
		i = 0;
		while  (i < resplth)  {
			printf( "%s", response+i );
			i += strlen( response+i ) + 1;
		} /*endif*/
		if  (termfound)  break;
	} /*endif*/
	printf( "\n" );

	close( sock );
	exit( EXIT_SUCCESS );

} /* end of main */



/*----------------------------------------------------------------------------*/



void write_to_server( int filedes, char msg[] )

/* writes message to server
 *
 * parameters of routine
 * int        filedes; input; file descriptor (socket)
 * char       msg[]; input; message text
 */
{
	/* local varaibles */
	int      nbytes;    /* number of bytes */

	/* executale code */

   nbytes = write (filedes, msg, strlen(msg) + 1);
   if  (nbytes < 0)  {
		perror ("write");
		exit( EXIT_FAILURE );
	} /*endif*/

} /* end of write_to_server */



/*----------------------------------------------------------------------------*/



void read_from_server( int fd, int maxbuf, char buffer[], int *nbytes )

/* Read response message from server
 *
 * parameters of routine
 * int        fd;       input; socket ID
 * int        maxbuf;   input; maximum buffer length
 * char       buffer[]; output; message read from socket
 * int        *nbytes;  output; number of bytes read
 */
{
	/* local variables */

	/* executable code */

	*nbytes = read( fd, buffer, maxbuf );

	if  (*nbytes < 0)  {
		/* Read error. */
		perror( "read error of server" );
		exit( EXIT_FAILURE );
	} /*endif*/

} /* end of read_from_server */



/*----------------------------------------------------------------------------*/



void read_from_server_timeout( int fd, int maxbuf, char buffer[],
	int *nbytes, int *timeout )

/* Read response message from server
 *
 * parameters of routine
 * int        fd;       input; socket ID
 * int        maxbuf;   input; maximum buffer length
 * char       buffer[]; output; message read from socket
 * int        *nbytes;  output; number of bytes read
 * int        *timeout; output; timeout occurred (1=yes, 0=no)
 */
{
	/* local variables */
	fd_set   set;             /* file descriptor set */
	struct timeval stimeout;  /* timeout structire */

	/* executable code */

	FD_ZERO( &set );
	FD_SET( fd, &set );
	stimeout.tv_sec = 3;
	stimeout.tv_usec = 0;

	*timeout = !select( FD_SETSIZE, &set, NULL, NULL, &stimeout );

	if  (*timeout)  {
		*nbytes = 0;
	} else {
		*nbytes = read( fd, buffer, maxbuf );
	} /*endif*/

} /* end of read_from_server_timeout */



/*----------------------------------------------------------------------------*/



void init_sockaddr( struct sockaddr_in *name, const char *hostname,
	uint16_t port )

/* Initialise socket address
 *
 * parameters of routine
 * struct sockaddr_in *name; output; socket address to fill
 * const char *hostname; input; name of host to connect to
 * uint16_t   *port;     input; port number to be used
 */
{
	/* local variables */
	struct hostent *hostinfo;  /* host info structure */

	/* executable code */
	name->sin_family = AF_INET;
	name->sin_port = htons( port );
	hostinfo = gethostbyname( hostname );
	if (hostinfo == NULL)  {
		fprintf( stderr, "Unknown host %s.\n", hostname );
		exit( EXIT_FAILURE );
	} /*endif*/
	name->sin_addr = *(struct in_addr *)hostinfo->h_addr;

} /* end of init_sockaddr */



/*----------------------------------------------------------------------------*/


void parse_cmdline( int argc, char *argv[], char host[], int *portno,
	char msg[], char upload[], int *ok )

/* Parses command line and returns parameters
 *
 * parameters of routine
 * int        argc;    input; number of command parameters
 * char       *argv[]; input; parameters passed
 * char       host[];  output; hostname (max. length STRLTH)
 * int        *portno; output; number of port to use
 * char       msg[];   output; text message for server (max. length STRLTH)
 * char       upload[];output; file to upload
 * int        *ok;     output; command parameters ok?
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	/* defaults */
	strcpy( host, "localhost" );
	*portno = DEFAULT_PORT;
	*msg = '\0';
	*upload = '\0';
	*ok = 1;

	for  (i=1; i<argc; i++)  {
		if  (strncmp(argv[i],"-h=",3) == 0)  {
			if  (strlen(argv[i]+3) < STRLTH)  {
				strcpy( host, argv[i]+3 );
			} else {
				*ok = 0;
			} /*endif*/
		} else if  (strncmp(argv[i],"-p=",3) == 0)  {
			*ok = (sscanf(argv[i]+3,"%d",portno) == 1);
		} else if  (strncmp(argv[i],"-up=",4) == 0)  {
			if  (strlen(argv[i]+4) < STRLTH)  {
				strcpy( upload, argv[i]+4 );
			} else {
				*ok = 0;
			} /*endif*/
		} else {
			if  (strlen(argv[i])+strlen(msg)+1 < STRLTH)  {
				if  (*msg != '\0')  strcat( msg, " " );
				strcat( msg, argv[i] );
			} else {
				*ok = 0;
			} /*endif*/
		} /*endif*/
	} /*endfor*/

} /* end of parse_cmdline */



/*----------------------------------------------------------------------------*/

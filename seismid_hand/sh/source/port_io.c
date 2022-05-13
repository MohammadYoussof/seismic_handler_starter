/* file port_io.c
 *      =========
 *
 * version 2, 9-Jul-2006
 * 
 * socket server test program
 *
 * K. Stammler, 28-Jun-2006
 */


#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include "port_io.h"


#define cPio_MAXBUF  512

/* local prototypes */
int pio_make_socket( uint16_t port );


/* global variables */
static int     piov_main_socket;     /* main socket for new connections */
static fd_set  piov_active_fd_set;   /* active file descriptor set */
static fd_set  piov_read_fd_set;     /* current file descriptor set */
static FILE    *piov_log=NULL;       /* file pointer for log messages */


/*----------------------------------------------------------------------------*/



void pio_init_main_socket( int portno )

/* initialises main socket for connections
 *
 * parameters of routine
 * int        portno; input; IP port number
 */
{
	/* local variables */

	/* executable code */

	/* Create the socket and set it up to accept connections. */
	piov_main_socket = pio_make_socket( portno );
	if  (listen(piov_main_socket,1) < 0)  {
		perror( "error on listen command" );
		exit( EXIT_FAILURE );
	} /*endif*/

	/*printf( "--> socket %d initialised\n", piov_main_socket );*/

	/* Initialize the set of active sockets. */
	FD_ZERO( &piov_active_fd_set );
	FD_SET( piov_main_socket, &piov_active_fd_set );

} /* end of pio_init_main_socket */



/*----------------------------------------------------------------------------*/



void pio_read( int maxbuf, char buffer[], int *buflth, int *tmpsock )

/* Read data from port opened with pio_init_main_socket
 *
 * parameters of routine
 * int        maxbuf;      input; maximum length of buffer
 * char       buffer[];    output; data read from socket
 * int        *buflth;     output; number of bytes in the buffer
 * int        *tmpsock;    output; socket from which data was read or -1
 *                                 this argument may be NULL
 */
{
	/* local variables */
	int      i;                          /* counter */
	struct sockaddr_in clientname;       /* name of client */
	size_t   size;                       /* size of clientname structure */

	/* executable code */

	*buffer = '\0';
	*buflth = 0;

	/* Block until input arrives on one or more active sockets. */
	piov_read_fd_set = piov_active_fd_set;
	if  (select(FD_SETSIZE, &piov_read_fd_set, NULL, NULL, NULL) < 0)  {
		perror("error on select comand");
		exit( EXIT_FAILURE );
	} /*endif*/

	/* Service all the sockets with input pending. */
	for (i=0; i<FD_SETSIZE; ++i)
		if (FD_ISSET(i,&piov_read_fd_set))  {
			if  (i == piov_main_socket)  {
				/* Connection request on original socket. */
				int new;  /* new socket ID */
				size = sizeof( clientname );
				new = accept(piov_main_socket, (struct sockaddr *) &clientname,
					&size );
				if  (new < 0)  {
					perror( "error on accept command" );
					exit( EXIT_FAILURE );
				} /*endif*/
				if  (piov_log != NULL)
					fprintf (piov_log, "port_io-Server: connect from host %s, port %hd.\n",
						inet_ntoa(clientname.sin_addr), ntohs(clientname.sin_port) );
				FD_SET( new, &piov_active_fd_set );
				/*printf( "--> new active socket %d\n", new );*/
			} else {
				/* Data arriving on an already-connected socket. */
				pio_read_from_client( i, maxbuf, buffer, buflth );
				/*printf( "--> reading from socket %d, got %d bytes\n", i, *buflth );*/
				if  (*buflth > 0)  {
					if  (*buffer < ' ')  {
						/* ignore lines starting with control chars */
						*buffer = '\0';
						*buflth = 0;
					} /*endif*/
					/*printf( "--> returning from pio_read\n" );*/
					if  (tmpsock != NULL)  *tmpsock = i;
					return;
				} else {
					/* no more data, socket should be closed */
					close( i );
					FD_CLR( i, &piov_active_fd_set );
					*buffer = '\0';
					/*printf( "--> close socket %d\n", i );*/
					if  (tmpsock != NULL)  *tmpsock = -1;
				} /*endif*/
			} /*endif*/
		} /*endif*/

} /* end of pio_read */



/*----------------------------------------------------------------------------*/



void pio_read_from_client( int fd, int maxbuf, char buffer[], int *nbytes )

/* Read message from client
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
		*buffer = '\0';
		*nbytes = 0;
	} else if  (*nbytes == 0)  {
		*buffer = '\0';
	} /*endif*/

} /* end of pio_read_from_client */



/*----------------------------------------------------------------------------*/



void pio_read_from_client_timeout( int fd, int maxbuf, int timeoutsec,
	char buffer[], int *nbytes, int *timeout )

/* Read message from client or timeout
 *
 * parameters of routine
 * int        fd;       input; socket ID
 * int        maxbuf;   input; maximum buffer length
 * int        timeoutsec; input; maximum number of seconds to wait
 * char       buffer[]; output; message read from socket
 * int        *nbytes;  output; number of bytes read
 * int        *timeout; output; timeout occurred
 */
{
	/* local variables */
	fd_set   set;             /* file descriptor set */
	struct timeval stimeout;  /* timeout structure */

	/* executable code */

	if  (fd <= 0)  {*nbytes = 0;  return;}

	FD_ZERO( &set );
	FD_SET( fd, &set );
	stimeout.tv_sec = timeoutsec;
	stimeout.tv_usec = 0;

	*timeout = !select( FD_SETSIZE, &set, NULL, NULL, &stimeout );

	if  (*timeout)  {
		*nbytes = 0;
	} else {
		*nbytes = read( fd, buffer, maxbuf );
	} /*endif*/

} /* end of pio_read_from_client_timeout */



/*----------------------------------------------------------------------------*/



int pio_term_found( char msg[], int *msglth )

/* Finds and removes termination string
 *
 * parameters of routine
 * char       msg[];     modify; message to check, term string removed
 * int        *msglth;   modify; lenght of message
 */
{
	/* local variables */
	int      found;       /* term string fond */

	/* executable code */

	found = (*msglth >= cPio_TERMLTH
				&& strcmp(msg+(*msglth)-cPio_TERMLTH,cPio_TERM) == 0);

	if  (found)  {
		msg[*msglth-cPio_TERMLTH] = '\0';
		*msglth -= cPio_TERMLTH;
	} /*endif*/

	return (found);

} /*end of pio_term_found */



/*----------------------------------------------------------------------------*/



int pio_make_socket( uint16_t port )

/* Creates socket and returns its ID
 *
 * parameters of routine
 * uint16_t   port; input; port number to use for connection
 *                  returns socket ID
 */
{
	/* local variables */
	int      sock;            /* socket */
	struct sockaddr_in name;  /* socket name (address) */

	/* executable code */

	/* Create the socket. */
	sock = socket( PF_INET, SOCK_STREAM, 0 );
	if  (sock < 0)  {
		perror ("pio_make_socket (socket)");
		if  (piov_log != NULL)
			fprintf( piov_log, "pio_make_socket: error creating socket\n" );
		exit( EXIT_FAILURE );
	} /*endif*/

	/* Give the socket a name. */
	name.sin_family = AF_INET;
	name.sin_port = htons( port );
	name.sin_addr.s_addr = htonl( INADDR_ANY );
	if  (bind(sock,(struct sockaddr *) &name,sizeof(name)) < 0)  {
		perror( "pio_make_socket (bind)" );
		if  (piov_log != NULL)
			fprintf( piov_log, "pio_make_socket: error binding socket\n" );
		exit( EXIT_FAILURE );
	} /*endif*/

	return sock;

} /* end of pio_make_socket */



/*----------------------------------------------------------------------------*/



void pio_write_to_client( int fd, char msg[] )

/* writes message to client
 *
 * parameters of routine
 * int        fd; input; file descriptor (socket)
 * char       msg[]; input; message text
 */
{
	/* local variables */
	int      nbytes;    /* number of bytes */

	/* executable code */

   nbytes = write (fd, msg, strlen(msg) + 1);
   if  (nbytes < 0)  {
		perror ("pio_write_to_client");
		if  (piov_log != NULL)
			fprintf( piov_log, "pio_write_to_client: error writing to socket %d\n",
				fd );
	} /*endif*/

} /* end of write_to_server */



/*----------------------------------------------------------------------------*/



void pio_set_logfile( FILE *logf )

/* Sets log file for debug messages
 *
 * parameters for routine
 * FILE       *logf; input; logfile pointer
 */
{
	/* executable code */

	piov_log = logf;

} /* end of pio_set_logfile */



/*----------------------------------------------------------------------------*/



void pio_send_file_to_socket( char fname[], int sock, int *status )

/* Sends specified file to given socket
 *
 * parameters of routine
 * char       fname[]; input; file to send
 * int        sock;    input; socket ID
 * int        *status; output; return status
 */
{
	/* local variables */
	FILE     *fp;                  /* pointer to input file */
	char     line[cPio_MAXBUF+1];  /* current line of file */
	int      slen;                 /* string length */

	/* executable code */

	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		*status = cPioE_FILE_OPEN;
		return;
	} /*endif*/

	while  (fgets(line,cPio_MAXBUF,fp) != NULL)  {
		slen = strlen( line );
		if  (write( sock, line, slen ) != slen)  {
			*status = cPioE_SOCK_WRITE_ERR;
			perror( "pio_send_file_to_socket" );
			return;
		} /*endif*/
	} /*endwhile*/

	fclose( fp );

} /* end of pio_send_file_to_socket */



/*----------------------------------------------------------------------------*/

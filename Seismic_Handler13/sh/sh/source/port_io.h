
/* file port_io.h
 *      =========
 *
 * version 5, 21-Dec-2006
 *
 * Routines for read ing from port
 * K. Stammler, 4-Jul-2006
 */


#define cPio_TERM "@_@_SHDTERM_@_@"
#define cPio_TERMLTH 16


#define cPioE_OFFSET          1200
#define cPioE_FILE_OPEN       (cPioE_OFFSET+1)
#define cPioE_SOCK_WRITE_ERR  (cPioE_OFFSET+2)


/*----------------------------------------------------------------------------*/


void pio_init_main_socket( int portno );

/* initialises main socket for connections
 *
 * parameters of routine
 * int        portno; input; IP port number
 */


/*----------------------------------------------------------------------------*/


void pio_read( int maxbuf, char buffer[], int *buflth, int *tmpsock );

/* Read data from port opened with pio_init_main_socket
 *
 * parameters of routine
 * int        maxbuf;      input; maximum length of buffer
 * char       buffer[];    output; data read from socket
 * int        *buflth;     output; number of bytes in the buffer
 * int        *tmpsock;    output; socket from which data was read or -1
 *                                 this argument may be NULL
 */


/*----------------------------------------------------------------------------*/


void pio_write_to_client( int fd, char msg[] );

/* writes message to client
 *
 * parameters of routine
 * int        fd; input; file descriptor (socket)
 * char       msg[]; input; message text
 */


/*----------------------------------------------------------------------------*/


void pio_read_from_client( int fd, int maxbuf, char buffer[], int *nbytes );

/* Read message from client
 *
 * parameters of routine
 * int        fd;       input; socket ID
 * int        maxbuf;   input; maximum buffer length
 * char       buffer[]; output; message read from socket
 * int        *nbytes;  output; number of bytes read
 */


/*----------------------------------------------------------------------------*/


void pio_read_from_client_timeout( int fd, int maxbuf, int timeoutsec,
	char buffer[], int *nbytes, int *timeout );

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


/*----------------------------------------------------------------------------*/


int pio_term_found( char msg[], int *msglth );

/* Finds and removes termination string
 *
 * parameters of routine
 * char       msg[];     modify; message to check, term string removed
 * int        *msglth;   modify; lenght of message
 */


/*----------------------------------------------------------------------------*/


void pio_set_logfile( FILE *logf );

/* Sets log file for debug messages
 *
 * parameters for routine
 * FILE       *logf; input; logfile pointer
 */


/*----------------------------------------------------------------------------*/


void pio_send_file_to_socket( char fname[], int sock, int *status );

/* Sends specified file to given socket
 *
 * parameters of routine
 * char       fname[]; input; file to send
 * int        sock;    input; socket ID
 * int        *status; output; return status
 */


/*----------------------------------------------------------------------------*/

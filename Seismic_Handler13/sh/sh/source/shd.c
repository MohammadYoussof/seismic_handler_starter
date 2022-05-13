
/* file shd.c
 *      =====
 *
 * version 3, 11-Jul-2006
 * 
 * SH daemon program
 *
 * K. Stammler, 8-Jul-2006
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


#define cShd_MAXBUF  512

/* local prototypes */
void shd_init_vars( char parfile[] );
int shd_hash2port( int hash );
void shd_closeport( int shport );
void shd_init_sockaddr( struct sockaddr_in *name, const char *hostname,
	uint16_t port, int *ok );
void shd_write_to_server( int fd, char msg[], int *ok );
void shd_read_from_server( int fd, int maxbuf, char buffer[], int *nbytes,
	int *ok );
void shd_read_from_server_timeout( int fd, int maxbuf, char buffer[],
	int *nbytes, int *timeout, int *ok );
void shd_send_command( char hostname[], int portno, char cmd[], int respsock,
	int do_upload );
void shd_upload( int src_sock, int dst_sock );

/* global variables */
static int *shdv_port;                     /* port numbers used */
static int *shdv_hash;                     /* hash numbers */
/* global parameters */
static int shdv_portno=18005;              /* port number for daemon */
static int shdv_baseport=19000;            /* base port for local connections */
static int shdv_maxsessions=5;             /* maximum number of SH sessions */
static int shdv_debuglevel=1;              /* debug level */
static FILE *shdv_log=NULL;                /* log file */



/*----------------------------------------------------------------------------*/



int main( int argc, char *argv[] )
{

	/* local variables */
	char     parfile[cShd_MAXBUF];/* parameter file */
	char     buffer[cShd_MAXBUF]; /* read buffer */
	int      buflth;              /* bytes read */
	int      sock;                /* current socket */
	char     cmd[cShd_MAXBUF];    /* command */
	char     msg[cShd_MAXBUF];    /* return message */
	int      hash;                /* hash number */
	int      shport;              /* SH port number for command forwarding */
	int      i;                   /* counter */
	int      do_upload;           /* upload requested */

	/* executable code */

	if  (argc != 2)  {
		fprintf( stderr, "Usage: %s <parfile>\n", argv[0] );
		return 1;
	} /*endif*/

	/* get parameters */
	if  (strlen(argv[1]) >= cShd_MAXBUF-1)  {
		fprintf( stderr, "shd: name of parameter file too long.  Abort.\n" );
		return 1;
	} /*endif*/
	strcpy( parfile, argv[1] );

	shd_init_vars( parfile );

	if  (shdv_debuglevel > 3)
		pio_set_logfile( shdv_log );

	pio_init_main_socket( shdv_portno );

	/* initialise random seed */
	srand( time(0) );

	while (1)  {

		pio_read( cShd_MAXBUF, buffer, &buflth, &sock );

		if  (buflth > 0 && *buffer > '\0')  {

			if  (shdv_debuglevel > 3)
				fprintf( shdv_log, "shd-4: got message: >%s< (buflth %d, socket %d)\n",
					buffer, buflth, sock );

			i = strlen( buffer );
			do_upload = (strcmp(buffer+i-3,":U@") == 0);
			if  (do_upload)  buffer[i-3] = '\0';

			/* read command/session ID */
			if  (sscanf(buffer,"%s",cmd) != 1)  continue;

			if  (strcmp(cmd,"new-session") == 0)  {
				if  (sock > 0)  {
					hash = shd_newhash();
					if  (hash > 0)  {
						shport = shd_hash2port( hash );
						sprintf( msg, "$SH_UTIL/start_sh_at_port.csh %d %d &",
							shport, hash );
						if  (shdv_debuglevel > 5)
							fprintf( shdv_log, "shd-6: exec shell cmd: %s\n", msg );
						system( msg );
						sprintf( msg, "%d is session ID\n", hash );
					} else {
						strcpy( msg, "0 : no more sessions\n" );
					} /*endif*/
					write( sock, msg, strlen(msg)+1 );
					write( sock, cPio_TERM, cPio_TERMLTH );
				} /*endif*/
			} else if  (strcmp(cmd,"active-ports") == 0)  {
				*msg = '\0';
				for  (i=0; i<shdv_maxsessions; i++)
					if  (shdv_hash[i] > 0)  {
						sprintf( msg+strlen(msg), "%d ", shdv_port[i] );
					} /*endif*/
				write( sock, msg, strlen(msg)+1 );
				write( sock, cPio_TERM, cPio_TERMLTH );
			} else if  (strcmp(cmd,"terminate-server") == 0)  {
				for  (i=0; i<shdv_maxsessions; i++)
					if  (shdv_hash[i] > 0)  {
						shd_send_command( "localhost", shdv_port[i], "quit y", 0, do_upload );
					} /*endif*/
				strcpy( msg, "shd: terminated all active SH sessions\n" );
				write( sock, msg, strlen(msg)+1 );
				write( sock, cPio_TERM, cPio_TERMLTH );
				break;
			} else if  (sscanf(cmd,"%d",&hash) == 1)  {
				shport = shd_hash2port( hash );
				i = strlen( cmd );
				while  (buffer[i] == ' ') i++;
				if  (strncasecmp(buffer+i,"quit",4) == 0 && buffer[i+4] <= ' ')  {
					shd_closeport( shport );
					shd_send_command( "localhost", shport, "quit y", 0, do_upload );
					sprintf( msg, "SH at port %d terminated\n", shport );
					write( sock, msg, strlen(msg)+1 );
					write( sock, cPio_TERM, cPio_TERMLTH );
				} else {
					shd_send_command( "localhost", shport, buffer+i, sock, do_upload );
					if  (shdv_debuglevel > 5)
						fprintf( shdv_log, "shd-6: send command >%s< to port %d\n",
							buffer+i, shport );
				} /*endif*/
			} else {
				if  (sock > 0)  {
					if  (strlen(cmd) < cShd_MAXBUF-26)  {
						sprintf( msg, "shd: got illegal command %s\n", cmd );
					} else {
						strcpy( msg, "shd: illegal command too long\n" );
					} /*endif*/
					if  (shdv_debuglevel > 5)
						fprintf( shdv_log, "shd-6: writing response to socket %d\n", sock );
					write( sock, msg, strlen(msg)+1 );
					write( sock, cPio_TERM, cPio_TERMLTH );
				} /*endif*/
			} /*endif*/

		} /*endif*/

	} /*endwhile*/

} /*end of main */



/*----------------------------------------------------------------------------*/



void shd_init_vars( char parfile[] )

/* Initialise global variables and parameters
 *
 * parameters for routine
 * char       parfile[];   input; name of parameter file
 */
{
	/* local variables */
	int      i;                    /* counter */
	FILE     *fp;                  /* pointer to file */
	char     line[cShd_MAXBUF+1];  /* current line of file */
	char     vname[cShd_MAXBUF+1]; /* variable name */
	char     vvalue[cShd_MAXBUF+1];/* variable value */

	/* executable code */

	if  (strcmp(parfile,"default") != 0)  {
		fp = fopen( parfile, "r" );
		if  (fp == NULL)  {
			fprintf( stderr, "shd: error opening parameter file %s.  Abort.\n",
				parfile );
			exit( 1 );
		} /*endif*/
		while  (fgets(line,cShd_MAXBUF,fp) != NULL)  {
			if  (*line == '!' || *line == '#' || *line == '\n')  continue;
			if  (sscanf(line,"%s %s",vname,vvalue) != 2)  {
				fprintf( stderr, "shd: syntax error in parameter file:\n%s", line );
				fclose( fp );
				exit( 1 );
			} /*endif*/
			if  (strcmp(vname,"debug-level") == 0)  {
				if  (sscanf(vvalue,"%d",&shdv_debuglevel) != 1)  {
					fprintf( stderr, "shd: error reading debug level.  Abort.\n" );
					fclose( fp );
					exit( 1 );
				} /*endif*/
			} else if  (strcmp(vname,"logfile") == 0)  {
				if  (strcmp(vvalue,"stderr") == 0)  {
					shdv_log = stderr;
				} else {
					shdv_log = fopen( vvalue, "w" );
					if  (shdv_log == NULL)  {
						fprintf( stderr, "shd: error opening logfile %s.  Abort.\n",
							vvalue );
						fclose( fp );
						exit( 1 );
					} /*endif*/
				} /*endif*/
			} else if  (strcmp(vname,"baseport") == 0)  {
				if  (sscanf(vvalue,"%d",&shdv_baseport) != 1)  {
					fprintf( stderr, "shd: error reading baseport. Abort.\n" );
					fclose( fp );
					exit( 1 );
				} /*endif*/
			} else if  (strcmp(vname,"maxsessions") == 0)  {
				if  (sscanf(vvalue,"%d",&shdv_maxsessions) != 1)  {
					fprintf( stderr, "shd: error reading maxsessions. Abort.\n" );
					fclose( fp );
					exit( 1 );
				} /*endif*/
			} else if  (strcmp(vname,"mainport") == 0)  {
				if  (sscanf(vvalue,"%d",&shdv_portno) != 1)  {
					fprintf( stderr, "shd: error reading mainport. Abort.\n" );
					fclose( fp );
					exit( 1 );
				} /*endif*/
			} else {
				fprintf( stderr, "shd: illegal variable %s in parfile %s\n",
					vname, parfile );
			} /*endif*/
		} /*endwhile*/
		fclose( fp );
	} /*endif*/

	if  (shdv_log == NULL)  shdv_log = stderr;

	/* allocate memory for port and hash arrays */
	shdv_port = (int *)malloc( shdv_maxsessions*(int)sizeof(int) );
	if  (shdv_port == NULL)  {
		fprintf( stderr, "shd: error allocating memory (ports).  Abort.\n" );
		exit( 1 );
	} /*endif*/
	/* allocate memory for port and hash arrays */
	shdv_hash = (int *)malloc( shdv_maxsessions*(int)sizeof(int) );
	if  (shdv_hash == NULL)  {
		fprintf( stderr, "shd: error allocating memory (hash).  Abort.\n" );
		exit( 1 );
	} /*endif*/

	/* port numbers */
	for  (i=0; i<shdv_maxsessions; i++)  {
		shdv_port[i] = shdv_baseport+i;   /* should check for availability */
		shdv_hash[i] = 0;
	} /*endfor*/

} /* end of shd_init_vars */



/*----------------------------------------------------------------------------*/



int shd_newhash( void )

/* Returns new hash number of 0 (no more session available)
 *
 * no parameters
 */
{
	/* local variables */
	int      i, j;         /* counter */
	int      illegal;      /* illegal new hash number */

	/* executable code */

	for  (i=0; i<shdv_maxsessions; i++)
		if  (shdv_hash[i] == 0)  {
			for  (;;)  {
				/* repeat until legal hash number found */
				shdv_hash[i] = rand() % 10000;
				illegal = 0;
				if  (shdv_hash[i] == 0)  illegal = 1;
				for  (j=0; j<shdv_maxsessions; j++)
					if  (j != i && shdv_hash[j] == shdv_hash[i])  illegal = 1;
				if  (!illegal)  break;
			} /*endfor*/
			return shdv_hash[i];
		} /*endif*/

	return 0;

} /* end of shd_newhash */



/*----------------------------------------------------------------------------*/



int shd_hash2port( int hash )

/* get port number from hash number.  A 0 return port means not found.
 *
 * parameters of routine
 * int        hash;    input; hash number
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	for  (i=0; i<shdv_maxsessions; i++)
		if  (shdv_hash[i] == hash)  return shdv_port[i];

	return 0;

} /* end of hash2port */



/*----------------------------------------------------------------------------*/



void shd_closeport( int shport )

/* marks given port as unused
 *
 * parameters of routine
 * int        shport; input; port number to close
 */
{
	/* local variables */
	int      i;     /* counter */

	/* executable code */

	for  (i=0; i<shdv_maxsessions; i++)
		if  (shdv_port[i] == shport)  {
			shdv_hash[i] = 0;
			break;
		} /*endif*/

} /* end of shd_closeport */



/*----------------------------------------------------------------------------*/



void shd_send_command( char hostname[], int portno, char cmd[], int respsock,
	int do_upload )

/* Sends a command  to a given port and returns response message
 *
 * parameters of routine
 * char       hostname[]; input; server host
 * int        portno;     input; port number to use for communication
 * char       cmd[];      input; command text to send
 * int        respsock;   input; socket for writing response
 * int        do_upload;  input; upload data to SH
 */
{
	/* local variables */
	struct sockaddr_in servername;   /* server name structure */
	int      sock;                   /* socket ID for writing */
	int      ok;                     /* return status */
	int      resplth;                /* length of response */
	char     resp[cShd_MAXBUF];      /* local response message */
	char     resp2[cShd_MAXBUF+1];   /* for debugging */
	int      timeout;                /* timeout at reading */
	int      i;                      /* counter */

	/* executable code */

	if  (shdv_debuglevel > 8)
		fprintf( shdv_log, "shd-9: shd_send_command h:%s p:%d c:%s s:%d\n",
			hostname, portno, cmd, respsock );

	/* Create the output socket. */
	sock = socket( PF_INET, SOCK_STREAM, 0 );
	if (sock < 0)  {
		strcpy( resp, "shd: error creating socket\n" );
		return;
	} /*endif*/

	/* Connect to the server. */
	shd_init_sockaddr( &servername, hostname, portno, &ok );
	if  (!ok)  {
		if  (strlen(hostname) < cShd_MAXBUF-20)  {
			sprintf( resp, "shd: unknown host %s.\n", hostname );
		} else {
			strcpy( resp, "shd: unknown host (long name).\n" );
		} /*endif*/
		if  (respsock > 0)  write( respsock, resp, strlen(resp)+1 );
		return;
	} /*endif*/

	if  (0 > connect( sock,(struct sockaddr *) &servername,
		sizeof(servername)))  {
		strcpy( resp, "shd: error connecting to socket\n" );
		perror( "connect (client)" );
		if  (respsock > 0)  write( respsock, resp, strlen(resp)+1 );
		return;
	} /*endif*/

	/* Send data to the server. */
	shd_write_to_server( sock, cmd, &ok );
	if  (!ok)  {
		strcpy( resp, "shd: error writing to SH session\n" );
		perror( "error writing to SH session" );
		if  (respsock > 0)  write( respsock, resp, strlen(resp)+1 );
		return;
	} /*endif*/

	if  (do_upload)  {
		shd_upload( respsock, sock );
	} /*endif*/

	if  (shdv_debuglevel > 5)
		fprintf( shdv_log, "shd-6: waiting for response ...\n" );
	for  (;;)  {
		if  (shdv_debuglevel > 8)
			fprintf( shdv_log,
				"shd-9: awaiting SH response with timeout on socket %d\n",
				sock );
		shd_read_from_server_timeout( sock, cShd_MAXBUF, resp, &resplth,
			&timeout, &ok );
		if  (shdv_debuglevel > 8 && timeout)
			fprintf( shdv_log, "shd-9: got timeout on SH read\n" );
		if  (timeout || !ok)  break;
		if  (ok && !timeout)  {
			if  (shdv_debuglevel > 8)  {
				for  (i=0; i<resplth; i++)
					if  (resp[i] < ' ')  resp2[i] = '@';
					else  resp2[i] = resp[i];
				resp2[resplth] = '\0';
				fprintf( shdv_log, "shd-9: (lth:%d) %s\n", resplth, resp2 );
			} /*endif*/
			if  (respsock > 0)  {
				if  (shdv_debuglevel> 8)
					fprintf( shdv_log, "shd-9: forwarding response %s (length %d)\n",
						resp, resplth );
				write( respsock, resp, resplth );
				if  (shdv_debuglevel > 8)
					fprintf( shdv_log, "shd-9: done\n" );
			} /*endif*/
			if  (resplth >= cPio_TERMLTH
				&& strcmp(resp+resplth-cPio_TERMLTH,cPio_TERM) == 0)  break;
		} /*endif*/
	} /*endfor*/

	close( sock );

} /* end of shd_send_command */



/*----------------------------------------------------------------------------*/



void shd_init_sockaddr( struct sockaddr_in *name, const char *hostname,
	uint16_t port, int *ok )

/* Initialise socket address
 *
 * parameters of routine
 * struct sockaddr_in *name; output; socket address to fill
 * const char *hostname; input; name of host to connect to
 * uint16_t   *port;     input; port number to be used
 * int        *ok;       output; operation ok?
 */
{
	/* local variables */
	struct hostent *hostinfo;  /* host info structure */

	/* executable code */
	*ok = 1;
	name->sin_family = AF_INET;
	name->sin_port = htons( port );
	hostinfo = gethostbyname( hostname );
	if (hostinfo == NULL)  {
		*ok = 0;
		return;
	} /*endif*/
	name->sin_addr = *(struct in_addr *)hostinfo->h_addr;

} /* end of init_sockaddr */



/*----------------------------------------------------------------------------*/



void shd_write_to_server( int fd, char msg[], int *ok )

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

   nbytes = write (fd, msg, strlen(msg) + 1);
	*ok = (nbytes >= 0);

} /* end of shd_write_to_server */



/*----------------------------------------------------------------------------*/



void shd_read_from_server( int fd, int maxbuf, char buffer[], int *nbytes,
	int *ok )

/* Read response message from server
 *
 * parameters of routine
 * int        fd;       input; socket ID
 * int        maxbuf;   input; maximum buffer length
 * char       buffer[]; output; message read from socket
 * int        *nbytes;  output; number of bytes read
 * int        *ok;      output; return status
 */
{
	/* local variables */

	/* executable code */

	*nbytes = read( fd, buffer, maxbuf );
	*ok = (nbytes >= 0);

} /* end of shd_read_from_server */



/*----------------------------------------------------------------------------*/



void shd_read_from_server_timeout( int fd, int maxbuf, char buffer[],
	int *nbytes, int *timeout, int *ok )

/* Read response message from server
 *
 * parameters of routine
 * int        fd;       input; socket ID
 * int        maxbuf;   input; maximum buffer length
 * char       buffer[]; output; message read from socket
 * int        *nbytes;  output; number of bytes read
 * int        *timeout; output; timeout occurred (1=yes, 0=no)
 * int        *ok;      output; return status
 */
{
	/* local variables */
	fd_set   set;             /* file descriptor set */
	struct timeval stimeout;  /* timeout structure */

	/* executable code */

	if  (fd <= 0)  {*ok = 0;  return;}

	FD_ZERO( &set );
	FD_SET( fd, &set );
	stimeout.tv_sec = 3;
	stimeout.tv_usec = 0;

	*timeout = !select( FD_SETSIZE, &set, NULL, NULL, &stimeout );

	if  (*timeout)  {
		*nbytes = 0;
		*ok = 1;
	} else {
		*nbytes = read( fd, buffer, maxbuf );
		*ok = (nbytes >= 0);
	} /*endif*/

} /* end of shd_read_from_server_timeout */



/*----------------------------------------------------------------------------*/



void shd_upload( int src_sock, int dst_sock )

/* read data from src_sock and writes to dst_sock until termination string
 * found
 *
 * parameters of routine
 * int        src_sock;   input; source socket
 * int        dst_sock;   input; destination socket
 */
{
	/* local variables */
	char     buf[cShd_MAXBUF];     /* message buffer */
	int      buflth;               /* number of bytes in buffer */
	int      timeout;              /* timeout occurred */
	int      ok;                   /* read ok */
	int      terminate;            /* terminate upload */
	int      written;              /* bytes written */

	/* executable code */

	for  (;;)  {
		shd_read_from_server_timeout( src_sock, cShd_MAXBUF, buf, &buflth,
			&timeout, &ok );
		if  (timeout)  {
			fprintf( shdv_log, "shd: timeout occurred on upload\n" );
			return;
		} /*endif*/
		if  (!ok)  {
			fprintf( shdv_log, "shd: read error on upload\n" );
			return;
		} /*endif*/
		terminate = pio_term_found( buf, &buflth );
		written = write( dst_sock, buf, buflth );
		if  (written < buflth)  {
			fprintf( shdv_log, "shd: write error on upload\n" );
			perror( "write error on upload" );
		} /*endif*/
		if  (terminate)  break;
	} /*endfor*/

} /* end of shd_upload */



/*----------------------------------------------------------------------------*/

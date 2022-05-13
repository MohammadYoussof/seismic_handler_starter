
/* file unix_io.h
 *      =========
 *
 * version 2, 24-Aug-94
 *
 * correct ANSI prototypes of UNIX IO routines
 * K. Stammler, 30-Dec-93
 */


/* int open( char file[], int flags, ... ); */
int close( int fd );
int read( int fd, char *buf, int size );
int write( int fd, char *buf, int size );
int unlink( char path[] );

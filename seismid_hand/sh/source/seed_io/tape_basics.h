
/* file tape_basics.h
 *      =============
 *
 * version 1, 28-Dec-93
 *
 * prototypes of basic tape routines
 * K. Stammler, 28-Dec-93
 */


/*---------------------------------------------------------------------*/


void TapeBasicsRewind( int tape );

/* Rewinds tape.
 *
 * parameters of routine
 * int        tape;        input; tape file descriptor
 */


/*---------------------------------------------------------------------*/


void TapeBasicsSkipFile( int tape, int count );

/* Skips number of files on tape
 *
 * parameters of routine
 * int        tape;        input; tape file descriptor
 * int        count;       input; number of files to skip
 */


/*---------------------------------------------------------------------*/

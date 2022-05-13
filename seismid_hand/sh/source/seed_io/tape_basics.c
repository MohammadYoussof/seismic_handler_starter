
/* file tape_basics.c
 *      =============
 *
 * version 4, 24-May-96
 *
 * Basic tape functions.  This is not ANSI C but K&R standard.
 * (Maybe to be compiled with "cc").
 * K. Stammler, 28-Dec-93
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/uio.h>
#include "basecnst.h"
#ifdef cBc_OS_SUNOS
#include <sys/ioccom.h>
#endif
#ifndef cBc_OS_AIX
#include <sys/mtio.h>
#endif


#ifdef cBc_OS_AIX

void TapeBasicsDummy()
{
printf( "this is dummy\n" );
}

#else


/*---------------------------------------------------------------------*/



void TapeBasicsRewind( tape )
int tape;

/* Rewinds tape.
 *
 * parameters of routine
 * int        tape;        input; tape file descriptor
 */
{
	/* local variables */
	struct mtop mt_command;

	/* executable code */

	mt_command.mt_op = MTREW;
	mt_command.mt_count = 1;
	ioctl( tape, MTIOCTOP, &mt_command );

} /* end of TapeBasicsRewind */



/*---------------------------------------------------------------------*/



void TapeBasicsSkipFile( tape, count )
int tape;
int count;

/* Skips number of files on tape
 *
 * parameters of routine
 * int        tape;        input; tape file descriptor
 * int        count;       input; number of files to skip
 */
{

	/* local variables */
	struct mtop mt_command;

	/* executable code */

	if  (count == 0)  return;
	if  (count < 0)  {
		mt_command.mt_op = MTBSF;
		mt_command.mt_count = -count;
	} else {
		mt_command.mt_op = MTFSF;
		mt_command.mt_count = count;
	} /*endif*/
	ioctl( tape, MTIOCTOP, &mt_command );

} /* end of TapeBasicsSkip */



/*---------------------------------------------------------------------*/


#endif

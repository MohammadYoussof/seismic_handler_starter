
/* file gcftest.c
 *      =========
 *
 * version 1, 20-Oct-2003
 *
 * GCF test routine
 *
 * K. Stammler, 20-Oct-2003
 */

#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#include "sysbase.h"
#include "tcusrdef.h"
#include "gcflib.h"



int main( int argc, char *argv[] )
{
	/* local variables */
	char     stream_str[cBcLineLth+1];
	char     start[cBcLineLth+1];
	char     act_start[cBcLineLth+1];
	char     gfdfile[cBcLineLth+1];
	long     *ldat;
	long     smplth;
	float    dt;
	float    calib;
	GcfFileDescrT fdescr;
	TSyStatus status;

	/* executable code */

	status = cBcNoError;

	strcpy( stream_str, "ssht-hh-z" );
	strcpy( start, "18-Oct-2003_7:56:00.0" );
	strcpy( gfdfile, "/home/klaus/gcftest/gcf/gfdfile.gfd" );


#ifdef XXX

	/* test GcfFindFileInGfd */

	GcfFindFileInGfd( stream_str, start, gfdfile,
		&fdescr, &status );

	printf( "stream %s\n", fdescr.stream );
	printf( "t_start %s\n", fdescr.t_start );
	printf( "t_end %s\n", fdescr.t_end );
	printf( "name %s\n", fdescr.name );
	printf( "gfdfile %s\n", fdescr.gfdfile );
	printf( "recno %d\n", fdescr.recno );
	printf( "pos %s\n", fdescr.pos );
	printf( "sample %s\n", fdescr.sample );
	printf( "calib %g\n", fdescr.calib );


#endif

	/* test GcfSearchPosition */

	printf( "\n" );
	GcfSearchPosition( 0, gfdfile, stream_str, start, act_start, &status );
	printf( "\n" );

	printf( "act_start %s\n", act_start );
	printf( "status %d\n", status );



#ifdef XXX

	/* test GcfReadStream */

	printf( "\n" );

	GcfReadStream( 0, gfdfile, stream_str, FALSE, start, 300.0, &ldat,
		&smplth, act_start, &dt, &calib, &status );

	printf( "\n" );
	printf( "smplth %d\n", smplth );
	printf( "dt %g\n", dt );
	printf( "calib %g\n", calib );
	printf( "act_start %s\n", act_start );
	printf( "status %d\n", status );
	printf( "\n" );
	printf( "%d %d %d %d %d ...\n", ldat[0], ldat[1], ldat[2], ldat[3], ldat[4] );

#endif


} /* end of main */

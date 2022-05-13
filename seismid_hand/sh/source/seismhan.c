
/* File SEISMHAN.C
 *      ==========
 *
 * version 51, 21-Dec-2006
 *
 * main module of seismhandler program, command line version
 *
 * K. Stammler, 6-MAR-1990
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1992,  Klaus Stammler, Federal Institute for Geosciences
 *                                       and Natural Resources (BGR), Germany
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#ifdef BC_STDLIB_EX
#include <stdlib.h>
#endif /* BC_STDLIB_EX */
#include BC_SYSBASE
#include "seusrdef.h"
#include "shconst.h"
#include "sherrors.h"
#include "uierrors.h"
#include "cpusrdef.h"
#include "uiusrdef.h"
#include "trusrdef.h"
#include BC_GCUSRDEF
#include "erusrdef.h"
#include "fctxmn0.h"
#include "fctxdm.h"
#include "seed_io/seedcfg.h"
#include "seed_io/seed_lib.h"
#include "globalparams.h"
#include "sqliface.h"
#ifdef SH_SOCKET
#include "port_io.h"
#endif

/* if parameters to main program are used */
/*#ifdef BC_MAINARGS*/
#include "causrdef.h"
/*#endif*/ /* BC_MAINARGS */

/* VAX specific routines (usually FORTRAN subroutines) */
#ifdef BC_GRFVAX
#include "[-.local]fctxmngrf.h"
#endif /* BC_GRFVAX */

/* machine dependent path strings */
#include BC_SHDIRS

/* global variables */

int     tc;                 /* local text channels */
int     tc_shv;             /* global text channels */
int     gc;                 /* local graphic channels */
int     gc_shv;             /* global graphic channels */
int     cc;                 /* local console channels */
int     cc_shv;             /* global console channels */
SHFLAGS shflags_shv;        /* local processing flags */
SHFLAGS shglbflags_shv;     /* global processing flags */
STATUS  shv_last_status;    /* status of last command */
int     shv_maininput=0;    /* main input level for CP's */
char    id_shv[11]={SHC_FILE_PREFIX}; /* session ID */

char    protfile_shv[SHC_FILELTH+1]; /* protocol file */
char    shd_scratch[SHC_FILELTH+1] =  {DD_SCRATCH};
char    shd_inputs[SHC_FILELTH+1]  =  {DD_INPUTS};


/* #ifdef BC_MAINARGS */
int main( int argc, char *argv[] )
/*#else*/
/*int main( void )*/
/*#endif */ /*BC_MAINARGS */

{

	/* variables */
	STATUS   status;                /* return status */
	BOOLEAN  quit;                  /* quit program */
	char     cmdlin[cBcLongStrLth+1];  /* command read */
	char     vfstr[BC_LINELTH+1];   /* verify string */
	char     execstr[BC_LINELTH+1]; /* execute command (EXEC) */
	BOOLEAN  exec_unshift;          /* perform EXEC ui_unshift */
	int      exec_tcsave;           /* copy of tc for EXEC command */
	char     prompt[BC_LINELTH+1];  /* prompt string */
	PARAM    cmd;                   /* parsed command */
	BOOLEAN  redraw;                /* redraw flag */
	int      rdlevel = 0;           /* redraw level */
	BOOLEAN  bench;                 /* bench mark */
	BOOLEAN  iscmdproc;             /* command is not a built-in command */
#ifdef SH_SOCKET
	int      respsock;              /* response socket */
#endif

	/* executable code */

	printf( "\n" );
	printf( "SeismicHandler version %s, Copyright (C) 2006\n", SHC_VERSION );
	printf( "Klaus Stammler, Federal Institute for Geosciences and Natural Resources (BGR)\n" );
	printf( "SeismicHandler comes with ABSOLUTELY NO WARRANTY.\n" );
	printf( "This is free software, and you are welcome to redistribute it\n" );
	printf( "under certain conditions; type `HELP GPL' for details.\n" );
	printf( "\n" );

	status = BC_NOERROR;

	sy_initprocess();
	strcpy( prompt, "|sh> " );
	GpReadParfile();

	se_initialize( argc, argv, &status );
	if  (Severe(&status))  {
		se_dsplymsg( 0, status );
		exit( status );
	} /*endif*/

	/* enter main loop */
	*execstr = '\0';
	exec_unshift = FALSE;
	quit = FALSE;
	do  {

		/* read and translate command */
		status = SHE_NOERROR;
		err_clearcontext();
		if  (*execstr == '\0')  {  /* get command line from input stream */
			gc_write( tc, prompt );
			ui_setconsole( cc );
			ui_read( tc, cmdlin, cBcLongStrLth, &status );
		} else {   /* get EXEC command line */
			strcpy( cmdlin, execstr );
			*execstr = '\0';
			ui_shift( shv_maininput );  /* all prompts to the user */
			exec_tcsave = tc;
			tc = cc;
			exec_unshift = TRUE;
		} /*endif*/
		gc_update();
		/* sy_debuginfo( cmdlin ); */
		if  (shflags_shv & SHF_STEP && ui_stepflag() != '\0')
			se_do_step( ui_stepflag(), &status );
		if  (status == BC_NOERROR)  cp_parse( cmdlin, &cmd, &status );
		if  (status == BC_NOERROR)  tr_partrans( &cmd, &status );

		/* if VERIFY flag set, print verification of command */
		if  (shflags_shv & SHF_VERIFY)  {
			cp_verify( &cmd, BC_LINELTH, vfstr, &status );
			if  (status == SHE_NOERROR)  {
				gc_write( cc, vfstr );
				gc_wrtch( cc, '\n' );
			} /*endif*/
		} /*endif*/

		/* if benchmark, store current time */
		bench = cp_qexist( &cmd, "BENCH" );
		if  (bench)  sy_difftime();

		/* execute command */
		redraw = FALSE;
		if  (status == SHE_NOERROR  && cmd.p[0][strlen(cmd.p[0])-1] != ':')
			se_execute_command( &cmd, cmdlin, execstr, &quit, &redraw,
				&iscmdproc, &rdlevel, prompt, &status );
		if  (exec_unshift)  {
			ui_unshift();
			tc = exec_tcsave;
			exec_unshift = FALSE;
		}/*endif*/
		if  (cp_uncheckedqual(&cmd) != NULL && !Severe(&status) && !iscmdproc)
			se_check_qual( cc, &cmd );

		/* if benchmark, print time */
		if  (bench)  {
			sprintf( vfstr, "   time consumed: %f sec\n", sy_difftime() );
			gc_write( cc, vfstr );
		} /*endif*/

		/* redraw display if necessary and if no error occurred */
		if  (status == SHE_NOERROR)  dm_setup( &status );
		if  ((status == SHE_NOERROR) && redraw && (rdlevel == 0))
			dm_redraw( &status );

		/* if an error occurred, display error message */
		if  (status != 0)  {
			if  (!(shflags_shv & SHF_NOERRMSG))
				se_dsplymsg( cc, status );
			if  (ui_level() > 0)
				if  ((shflags_shv & SHF_CMDERRSTOP) || (shflags_shv & SHF_SHERRSTOP))  {
					if  (!quit)  quit = (shflags_shv & SHF_SHERRSTOP);
					se_cmdabort( &cmd, cmdlin, status );
				} else if  (status == UIE_READFILE)  {
					status = SHE_NOERROR;
					mn0_cmdreturn( &status );
				} /*endif*/
			if  (!quit)  quit = (shflags_shv & SHF_SHERRSTOP);
		} /*endif*/
		shv_last_status = status;

#ifdef SH_SOCKET
		respsock = ui_lastsocket();
		if  (respsock > 0)  {
			char  msg[cBcShortStrLth+1];  /* message to client */
			sprintf( msg, "%d SH_status\n", status );
			/*printf( "--> writing response %s to client\n", msg );*/
			pio_write_to_client( respsock, msg );
			pio_write_to_client( respsock, cPio_TERM );
		} /*endif*/
#endif

	} while (!quit);

	se_terminate();
	SqlDeleteScratchFile();

#	ifndef BC_VAX
	return 0;
#	endif /* BC_VAX */

} /* end of main */

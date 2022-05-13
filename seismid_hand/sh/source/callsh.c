
/* File callsh.c
 *      ========
 *
 * version 4, 22-May-2006
 *
 * execute command line of SH
 *
 * K. Stammler, 18-Feb-1993
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
#include "shvars.h"
#include "seusrdef.h"
#include "shconst.h"
#include "sherrors.h"
#include "uierrors.h"
#include "cpusrdef.h"
#include "uiusrdef.h"
#include "utusrdef.h"
#include "trusrdef.h"
#include BC_GCUSRDEF
#include "erusrdef.h"
#include "fctxmn0.h"
#include "fctxdm.h"


void callsh( char cmdline[], BOOLEAN *redraw, STATUS *status )

/* executes command line
 *
 * parameters of routine
 * char       cmdline[];     input; SH command line
 * BOOLEAN    *redraw;       output; redraw display
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	static char  prompt[BC_LINELTH+1];  /* prompt string */
	static char  execstr[BC_LINELTH+1]; /* execute command (EXEC) */
	static int   rdlevel = 0;           /* redraw level */
	char     cmdlin[cBcVeryLongStrLth+1];   /* local string for command line */
	char     vfstr[cBcVeryLongStrLth+1];       /* verify string */
	BOOLEAN  exec_unshift;              /* perform EXEC ui_unshift */
	int      exec_tcsave;               /* copy of tc for EXEC command */
	PARAM    cmd;                       /* parsed command */
	BOOLEAN  locredraw;                 /* local redraw */
	BOOLEAN  quit;                      /* quit program */
	BOOLEAN  bench;                     /* bench mark */
	BOOLEAN  iscmdproc;                 /* command is not a built-in command */
	BOOLEAN  firstloop;                 /* first loop flag */

	/* executable code */

	quit = FALSE;
	*execstr = '\0';
	exec_unshift = FALSE;
	firstloop = TRUE;
	*redraw = FALSE;
	do  {

		/* read and translate command */
		err_clearcontext();
		if  (firstloop)  {  /* take command line from parameter list */
			if  (strlen(cmdline) > cBcVeryLongStrLth)  {
				*status = SHE_STROVFL;
				return;
			} /*endif*/
			strcpy( cmdlin, cmdline );
			if  (SHF_CAPCNV & shflags_shv)
				ut_cap( cmdlin );
		} else if  (*execstr == '\0')  {  /* get command line from input stream */
			gc_write( tc, prompt );
			ui_setconsole( cc );
			ui_read( tc, cmdlin, cBcVeryLongStrLth, status );
		} else {   /* get EXEC command line */
			strcpy( cmdlin, execstr );
			*execstr = '\0';
			ui_shift( shv_maininput );  /* all prompts to the user */
			exec_tcsave = tc;
			tc = cc;
			exec_unshift = TRUE;
		} /*endif*/
		firstloop = FALSE;
		gc_update();

		if  (shflags_shv & SHF_STEP && ui_stepflag() != '\0')
			se_do_step( ui_stepflag(), status );
		if  (*status == BC_NOERROR)  cp_parse( cmdlin, &cmd, status );
		if  (*status == BC_NOERROR)  tr_partrans( &cmd, status );

		/* if VERIFY flag set, print verification of command */
		if  (shflags_shv & SHF_VERIFY)  {
			cp_verify( &cmd, cBcVeryLongStrLth, vfstr, status );
			if  (*status == SHE_NOERROR)  {
				gc_write( cc, vfstr );
				gc_wrtch( cc, '\n' );
			} /*endif*/
		} /*endif*/

		/* if benchmark, store current time */
		bench = cp_qexist( &cmd, "BENCH" );
		if  (bench)  sy_difftime();

		/* execute command */
		locredraw = FALSE;
		if  (*status == SHE_NOERROR  && cmd.p[0][strlen(cmd.p[0])-1] != ':')
			se_execute_command( &cmd, cmdlin, execstr, &quit, &locredraw,
				&iscmdproc, &rdlevel, prompt, status );
		if  (exec_unshift)  {
			ui_unshift();
			tc = exec_tcsave;
			exec_unshift = FALSE;
		}/*endif*/
		if  (cp_uncheckedqual(&cmd) != NULL && !Severe(status) && !iscmdproc)
			se_check_qual( cc, &cmd );

		/* if benchmark, print time */
		if  (bench)  {
			sprintf( vfstr, "   time consumed: %f sec\n", sy_difftime() );
			gc_write( cc, vfstr );
		} /*endif*/

		/* redraw display if necessary and if no error occurred */
		if  (*status == SHE_NOERROR)  dm_setup( status );
		if  ((*status == SHE_NOERROR) && locredraw && (rdlevel == 0))  {
			*redraw = TRUE;
			dm_redraw( status );
		} /*endif*/

		/* if an error occurred, display error message */
		if  (*status != 0)  {
			if  (!(shflags_shv & SHF_NOERRMSG))
				se_dsplymsg( cc, *status );
			if  (ui_level() > 0)
				if  ((shflags_shv & SHF_CMDERRSTOP) || (shflags_shv & SHF_SHERRSTOP))  {
					if  (!quit)  quit = (shflags_shv & SHF_SHERRSTOP);
					se_cmdabort( &cmd, cmdlin, *status );
				} else if  (*status == UIE_READFILE)  {
					*status = SHE_NOERROR;
					mn0_cmdreturn( status );
				} /*endif*/
			if  (!quit)  quit = (shflags_shv & SHF_SHERRSTOP);
		} /*endif*/
		shv_last_status = *status;

	} while (!quit && ui_level() > 0);

} /* end of callsh */

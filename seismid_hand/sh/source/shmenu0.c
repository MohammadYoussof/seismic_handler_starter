
/* File SHMENU0.C
 *      =========
 *
 * version 67, 29-Mar-2007
 *
 * menu routines of seismhandler
 * K. Stammler, 30-MAR-1990
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


#include <string.h>
#include <stdio.h>
#include <math.h>
#include "basecnst.h"
#include "sysbase.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "gcusrdef.h"
#include "shconst.h"
#include "shvars.h"
#include "cpusrdef.h"
#include "uiusrdef.h"
#include "utusrdef.h"
#include "scusrdef.h"
#include "ssusrdef.h"
#include "glusrdef.h"
#include "qfusrdef.h"
#include "ttusrdef.h"
#include "ptusrdef.h"
#include "itusrdef.h"
#include "erusrdef.h"
#include "tcusrdef.h"
#include "infoidx.h"
#include "fctxmn0.h"
#include "fctxmni.h"
#include "fctxhl.h"
#include "fctxsl.h"
#include "fctxdm.h"
#include "sherrors.h"
#include "sserrors.h"
#include "earthloc.h"
#include "globalparams.h"
#include "seed_io/seedcfg.h"
#include "seed_io/seed_lib.h"


static FILE    *echoch_mnv={NULL};    /* pointer to echo channel */
static int     mainchset_mnv={FALSE}; /* main channels changed */



/*------------------------------------------------------------------------*/



void mn0_callproc( PARAM *par, STATUS *status )

/* calls a command procedure. not a menu option
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      locstat;              /* local status */
	char     *cmdpath;             /* pointer to command path */
	int      pathcnt;              /* path counter */
	char     cname[BC_LINELTH+1];  /* path + name */
	int      inplev;               /* input level for command file */

	/* executable code */

	if  (cp_qexist(par,"PARENT"))  {
		inplev = -1;  /* read from parent level */
	} else {
		inplev = shv_maininput;  /* read from main input level */
	} /*endif*/

	if  (par->p[0][0] == '/')  {
		ui_switch( par->p[0], status );
	} else {
		for  (pathcnt=0;;pathcnt++)  {
			cmdpath = GpGetStringElem( cGpL_defpath_command, pathcnt );
			if  (cmdpath == NULL)  break;
			if  ((strlen(cmdpath)+strlen(par->p[0])+1) > BC_LINELTH)  {
				*status = SHE_CPATH;
				return;
			} /*endif*/
			strcpy( cname, cmdpath );
			strcat( cname, "/" );
			strcat( cname, par->p[0] );
			ui_switch( cname, status );
			if  (*status == SHE_NOERROR)  break;
		} /*endfor*/
	} /*endif*/
	if  (Severe(status))  {
		if  (cp_qexist(par,"CHECK"))  *status = SHE_NOERROR;
		return;
	} /*endif*/

	ss_push( tc, gc, cc, shflags_shv, status );
	if  (*status != SHE_NOERROR)  {
		ui_switch( "", &locstat );
		return;
	} /*endif*/

	/* tc = (tc_shv & 0xfff8); */
	tc = 0;
	gc = gc_shv;
	cc = cc_shv;
	shflags_shv = shglbflags_shv;
	if  (cp_qnum(par) > 0)  {
		sl_setoutput( par, &tc, &tc_shv, &gc, &gc_shv, &cc, &cc_shv,
			&mainchset_mnv, status );
		if  (*status != SHE_NOERROR)  {
			ui_switch( "", &locstat );
			ss_pop( &tc, &gc, &cc, &shflags_shv, &locstat );
			return;
		} /*endif*/
		sl_chgflags( par, &shflags_shv, &shglbflags_shv, status );
		if  (*status != SHE_NOERROR)  {
			ui_switch( "", &locstat );
			ss_pop( &tc, &gc, &cc, &shflags_shv, &locstat );
			return;
		} /*endif*/
	} /*endif*/
	ui_absflag( shflags_shv );
	sl_updatewdwnames( tc, gc, cc );

	ss_delall( SHC_SYMLOC, status );
	ss_setpar( par, inplev );

	if  (cp_qexist(par,"PLAYBACK"))  shv_maininput = ui_level();

} /* end of mn0_callproc */



/*------------------------------------------------------------------------*/



void mn0_cmdreturn( STATUS *status )

/* return from command procedure. not a menu option
 *
 * parameters of routine
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      locstat;     /* local status */
	BOOLEAN  startupflg;  /* check startup flag */

	/* executable code */

	startupflg = (shflags_shv & SHF_STARTUP);

	if  (ui_level() == shv_maininput)
		shv_maininput = 0;   /* REPLAY end */

	locstat = 0;
	ui_switch( "", status );
	ss_pop( &tc, &gc, &cc, &shflags_shv, &locstat );
	if  (mainchset_mnv && (ui_level() == 0))  {
		tc = tc_shv;
		gc = gc_shv;
		cc = cc_shv;
		mainchset_mnv = FALSE;
	} /*endif*/
	sl_updatewdwnames( tc, gc, cc );
	if  (locstat != SHE_NOERROR && *status == SHE_NOERROR)  {
		*status = locstat;
		return;
	} /*endif*/
	ui_absflag( shflags_shv );

	if  (startupflg)  {   /* abort program */
		shflags_shv |= SHF_SHERRSTOP;
		*status = SHE_SPECERROR+20;
	} /*endif*/

} /* end of mn0_cmdreturn */



/*------------------------------------------------------------------------*/



void mn0_fct( PARAM *par, STATUS *status )

/* multi function command
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];      /* subfunction */
	char     s_par[BC_LINELTH+1];    /* string parameter */
	char     str2[BC_LINELTH+1];     /* scratch */
	char     longcmd[cBcLongStrLth+1];  /* string list */
	int      i;
	int      i_par[6];
	STATLOC  l1, l2;
	REAL     r_par[4];
	CHMAP    map;                    /* channel map */
	STATUS   locstat=BC_NOERROR;

	/* executable code */
	cp_getstr( par, 1, tc, "  subfunction: ", BC_LINELTH, str, status );
	if  (*status != SHE_NOERROR)  return;

	if  (strcmp(str,"SETSTYLE") == 0)  {
		cp_getint( par, 2, tc, "   style block: ", &i, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 3, tc, "  item: ", BC_LINELTH, str2, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 4, tc, "  value: ", BC_LINELTH, s_par, status );
		if  (*status != SHE_NOERROR)  return;
		gc_setstyle( gc|GCF_ALLCHAN, i, str2, s_par, status );
	} else if  (strcmp(str,"AXIS") == 0)  {
		cp_getstr( par, 2, tc, "   axis name: ", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 3, tc, "   item: ", BC_LINELTH, s_par, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 4, tc, "   value: ", BC_LINELTH, str2, status );
		if  (*status != SHE_NOERROR)  return;
		dm_setaxis( str, s_par, str2, status );
	} else if  (strcmp(str,"PATH") == 0)  {
		cp_getstr( par, 2, tc, "   which item: ", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 3, tc, "   new path: ", BC_LINELTH, s_par, status );
		if  (*status != SHE_NOERROR)  return;
		if  (strcmp(str,"SCRATCH") == 0)  {
			if  (strlen(s_par) > SHC_FILELTH)  {*status = SHE_STROVFL;return;}
			strcpy( shd_scratch, s_par );
			gc_set_outputdir( s_par, status );
		} else if  (strcmp(str,"HELP") == 0)  {
			if  (strlen(s_par) > SHC_FILELTH)  {*status = SHE_STROVFL;return;}
			GpSetString( cGpS_defpath_help, s_par, NULL );
		} else if  (strcmp(str,"COMMAND") == 0)  {
			GpParseStringList( cGpL_defpath_command, s_par );
		} else if  (strcmp(str,"COMMAND2") == 0)  {
			printf(
				"*SH: obsolete command PATH COMMAND2, consider using string list in COMMAND path\n" );
			printf( 
				"*SH: translated in FCT PATH COMMAND |.|,|$SH_COMMAND|,|%s|\n", s_par );
			if  (strlen(s_par) > cBcLongStrLth-14)  {
				*status = SHE_STROVFL;
				return;
			} /*endif*/
			sprintf( longcmd, ". $SH_COMMAND %s", s_par );
			GpParseStringList( cGpL_defpath_command, longcmd );
		} else if  (strcmp(str,"GLOBALS") == 0)  {
			GpParseStringList( cGpL_defpath_globals, s_par );
		} else if  (strcmp(str,"FILTER") == 0)  {
			GpParseStringList( cGpL_defpath_filter, s_par );
		} else if  (strcmp(str,"ERRORS") == 0)  {
			GpSetString( cGpS_defpath_errors, s_par, NULL );
		} else if  (strcmp(str,"INPUTS") == 0)  {
			if  (strlen(s_par) > SHC_FILELTH)  {*status = SHE_STROVFL;return;}
			strcpy( shd_inputs, s_par );
			gc_set_inputdir( shd_inputs, status );
		} else {
			*status = SHE_ILPAR;
			err_setcontext( " ## par " ); err_setcontext( str );
		} /*endif*/
	} else if  (strcmp(str,"GETPATH") == 0)  {
		cp_getstr( par, 2, tc, "   name: ", BC_LINELTH, str, status );
		if  (Severe(status))  return;
		cp_getstr( par, 3, tc, "   variable: ", BC_LINELTH, s_par, status );
		if  (Severe(status))  return;
		if  (strcmp(str,"SCRATCH") == 0)  {
			sl_setsymbol( s_par, shd_scratch, status );
		} else if  (strcmp(str,"HELP") == 0)  {
			sl_setsymbol( s_par, GpGetString(cGpS_defpath_help), status );
		} else if  (strcmp(str,"COMMAND") == 0)  {
			if  (GpGetStringElem(cGpL_defpath_command,1) == NULL)  {
				sl_setsymbol( s_par, "", status );
			} else {
				sl_setsymbol( s_par, GpGetStringElem(cGpL_defpath_command,1), status );
			} /*endif*/
		} else if  (strcmp(str,"COMMAND2") == 0)  {
			if  (GpGetStringElem(cGpL_defpath_command,2) == NULL)  {
				sl_setsymbol( s_par, "", status );
			} else {
				sl_setsymbol( s_par, GpGetStringElem(cGpL_defpath_command,2), status );
			} /*endif*/
		} else if  (strcmp(str,"GLOBALS") == 0)  {
			printf( "obsolete function GETPATH GLOBALS.  Please dont use\n" );
			if  (GpGetStringElem(cGpL_defpath_globals,1) != NULL)
				sl_setsymbol( s_par, GpGetStringElem(cGpL_defpath_globals,1), status );
		} else if  (strcmp(str,"FILTER") == 0)  {
			printf( "obsolete function GETPATH FILTER.  Please dont use\n" );
			sl_setsymbol( s_par, GpGetStringElem(cGpL_defpath_filter,0), status );
		} else if  (strcmp(str,"ERRORS") == 0)  {
			sl_setsymbol( s_par, GpGetString(cGpS_defpath_errors), status );
		} else if  (strcmp(str,"INPUTS") == 0)  {
			sl_setsymbol( s_par, shd_inputs, status );
		} else if  (strcmp(str,"USERDIR") == 0)  {
			sl_setsymbol( s_par, GpGetString(cGpS_defpath_userdir), status );
		} else if  (strcmp(str,"EXTPROG") == 0)  {
			sl_setsymbol( s_par, GpGetString(cGpS_defpath_extprog), status );
		} else {
			*status = SHE_ILPAR;
			err_setcontext( " ## par " ); err_setcontext( str );
		} /*endif*/
	} else if  (strcmp(str,"QDIR") == 0)  {
		cp_getstr( par, 2, tc, "   HDR or BIN: ", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 3, tc, "   dir: ", BC_LINELTH, s_par, status );
		if  (*status != SHE_NOERROR)  return;
		if  (*str == 'H')  {
			if  (*s_par == '\0')  {
				qf_defaultdir( NULL, " ", status );
			} else {
				qf_defaultdir( s_par, " ", status );
			} /*endif*/
		} else {
			if  (*s_par == '\0')  {
				qf_defaultdir( " ", NULL, status );
			} else {
				qf_defaultdir( " ", s_par, status );
			} /*endif*/
		} /*endif*/
	} else if  (strcmp(str,"QLINES") == 0)  {
		cp_getstr( par, 2, tc, "   qfile: ", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getint( par, 3, tc, "   add lines: ", i_par, status );
		if  (*status != SHE_NOERROR)  return;
		qf_change( str, NULL, 0, 0, NULL, *i_par, status );
	} else if  (strcmp(str,"QCOMMENT") == 0)  {
		cp_getstr( par, 2, tc, "   comment file: ", BC_LINELTH, s_par, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 3, tc, "   qfile: ", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		qf_insertcmt( str, s_par, status );
	} else if  (strcmp(str,"CORRMODE") == 0)  {
		cp_getint( par, 2, tc, "   mode: ", i_par, status );
		if  (*status != SHE_NOERROR)  return;
		sc_set_corrm( *i_par, status );
	} else if  (strcmp(str,"AMPLICUT") == 0)  {
		cp_getfloat( par, 2, tc, "   cut off at: ", r_par, status );
		if  (*status != SHE_NOERROR)  return;
		dm_amplicut( *r_par );
	} else if  (strcmp(str,"AUTHOR") == 0)  {
		sl_printname( cc );
	} else if  (strcmp(str,"STATION") == 0)  {
		cp_getstr( par, 2, tc, "   station: ", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 3, tc, "   lat: ", BC_LINELTH, s_par, status );
		if  (*status != SHE_NOERROR)  return;
		if  (sscanf(s_par,"%lf",&l1) != 1)  {
			*status = SHE_REALCNV;
			err_setcontext( " ## numstr " ); err_setcontext( s_par );
			return;
		} /*endif*/
		cp_getstr( par, 4, tc, "   lon: ", BC_LINELTH, s_par, status );
		if  (*status != SHE_NOERROR)  return;
		if  (sscanf(s_par,"%lf",&l2) != 1)  {
			*status = SHE_REALCNV;
			err_setcontext( " ## numstr " ); err_setcontext( s_par );
			return;
		} /*endif*/
		cp_getint( par, 5, tc, "   array code: ", i_par, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 6, tc, "   array x: ", r_par, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 7, tc, "   array y: ", r_par+1, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 8, tc, "   comment: ", BC_LINELTH, s_par, status );
		if  (*status != SHE_NOERROR)  return;
		gl_insert( str, l1, l2, *i_par, r_par[0], r_par[1], s_par, status );
	} else if  (strcmp(str,"LOCFILE") == 0)  {
		cp_getstr( par, 2, tc, "   filename: ", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		gl_locfile_name( str );
	} else if  (strcmp(str,"FERINDEX") == 0)  {
		cp_getstr( par, 2, tc, "   filename: ", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		mb_setindexfile( str, status );
	} else if  (strcmp(str,"FERNAME") == 0)  {
		cp_getstr( par, 2, tc, "   filename: ", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		mb_setfernamefile( str, status );
	} else if  (strcmp(str,"CORRFILE") == 0)  {
		cp_getstr( par, 2, tc, "   filename: ", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		mb_setcorrfilename( str, status );
	} else if  (strcmp(str,"SETHC") == 0 || strcmp(str,"SET_HC") == 0)  {
		cp_getstr( par, 2, tc, "   which item: ", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 3, tc, "   value: ", BC_LINELTH, str2, status );
		if  (*status != SHE_NOERROR)  return;
		gc_sethc( gc, str, str2, status );
	} else if  (strcmp(str,"SETPAR") == 0 || strcmp(str,"SET_PAR") == 0)  {
		cp_getstr( par, 2, tc, "   wdw: ", BC_LINELTH, s_par, status );
		if  (*status != SHE_NOERROR)  return;
		sl_cnvgcflags( s_par, &map, status );
		cp_getstr( par, 3, tc, "   which item: ", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 4, tc, "   value: ", BC_LINELTH, str2, status );
		if  (*status != SHE_NOERROR)  return;
		gc_setpar( map, str, str2, status );
	} else if  (strcmp(str,"READ_VEL") == 0)  {
		cp_getstr( par, 2, tc, "   velocity file: ", BC_LINELTH,
			s_par, status );
		if  (Severe(status))  return;
		ut_defext( s_par, BC_LINELTH, SHC_DE_VEL, status );
		if  (Severe(status))  return;
		tt_read_mantle_vel( s_par, status );
	} else if  (strcmp(str,"CHANGE_VEL") == 0)  {
		cp_getstr( par, 2, tc, "   P or S: ", BC_LINELTH,
			s_par, status );
		*s_par = Cap( *s_par );
		if  (*s_par != 'S'  &&  *s_par != 'P')  {
			*status = SHE_ILPAR;
			err_setcontext( " ## par " ); err_setcontext( s_par );
			return;
		} /*endif*/
		cp_getfloat( par, 3, tc, "   max. depth (km): ", r_par, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 4, tc, "   factor: ", r_par+1, status );
		if  (Severe(status))  return;
		tt_change_mantle_vel( (*s_par == 'P'), *r_par, r_par[1], status );
	} else if  (strcmp(str,"TERMINATORS") == 0)  {
		cp_getstr( par, 2, tc, "   terminators: ", BC_LINELTH, str2, status );
		if  (*status != SHE_NOERROR)  return;
		cp_terminators( str2, status );
	} else if  (strcmp(str,"TT_TABLE_OLD") == 0)  {
		cp_getstr( par, 2, tc, "   table name: ", BC_LINELTH, str2, status );
		if  (*status != SHE_NOERROR)  return;
		tt_settable( str2, status );
	} else if  (strcmp(str,"TT_TABLE") == 0)  {
		cp_getstr( par, 2, tc, "   table dir: ", BC_LINELTH, str2, status );
		if  (*status != SHE_NOERROR)  return;
		pt_settabledir( str2, status );
	} else if  (strcmp(str,"INCI_TABLE") == 0)  {
		cp_getstr( par, 2, tc, "   table name: ", BC_LINELTH, str2, status );
		if  (*status != SHE_NOERROR)  return;
		it_settable( str2, status );
	} else if  (strcmp(str,"LOGICALS") == 0)  {
		cp_getstr( par, 2, tc, "   logical name table: ", BC_LINELTH, str2, status );
		if  (*status != SHE_NOERROR)  return;
		sy_lognames( str2, status );
	} else if  (strcmp(str,"SWAP_DISPLAY") == 0)  {
		cp_getstr( par, 2, tc, "   on|off: ", BC_LINELTH, str2, status );
		if  (*status != SHE_NOERROR)  return;
		if  (strcmp(str2,"ON") == 0)  {
			gc_swap( TRUE );
		} else if  (strcmp(str2,"OFF") == 0)  {
			gc_swap( FALSE );
		} else {
			*status = SHE_UKKEY;
			err_setcontext( " ## key " ); err_setcontext( str2 );
			return;
		} /*endif*/
	} else if  (strcmp(str,"SFD_CAPFILES") == 0)  {
		cp_getstr( par, 2, tc, "   on|off: ", BC_LINELTH, str2, status );
		if  (*status != SHE_NOERROR)  return;
		if  (strcmp(str2,"ON") == 0)  {
			SeedAcceptCapfiles( TRUE );
		} else if  (strcmp(str2,"OFF") == 0)  {
			SeedAcceptCapfiles( FALSE );
		} else {
			*status = SHE_UKKEY;
			err_setcontext( " ## key " ); err_setcontext( str2 );
			return;
		} /*endif*/
	} else if  (strcmp(str,"MARGIN") == 0)  {
		cp_getstr( par, 2, tc, "   side: ", BC_LINELTH, s_par, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 3, tc, "   value: ", r_par, status );
		if  (*status != SHE_NOERROR)  return;
		if  (cp_pentered(par,4,status))  {
			cp_getint( par, 4, tc, "@@", i_par, status );
			if  (*status != SHE_NOERROR)  return;
		} else {
			*i_par = gc;
		} /*endif*/
		dm_setmargin( *i_par, *s_par, *r_par, status );
	} else if  (strcmp(str,"GET_OS") == 0)  {
		cp_getstr( par, 2, tc, "   out-var: ", BC_LINELTH, str2, status );
		if  (*status != SHE_NOERROR)  return;
		strcpy( str, "UNKNOWN" );
#		ifdef BC_VAX
		strcpy( str, "VAX-VMS" );
#		endif
#		ifdef BC_SUN
		strcpy( str, "UNIX" );
#		endif
#		ifdef BC_ATARI
		strcpy( str, "ATARI-TOS" );
#		endif
#		ifdef BC_IBM
		strcpy( str, "MS-DOS" );
#		endif
		sl_setsymbol( str2, str, &locstat );
	} else if  (strcmp(str,"OSCALL") == 0)  {
		cp_getstr( par, 2, tc, "   verb: ", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 3, tc, "   par1: ", BC_LINELTH, s_par, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 4, tc, "   par2: ", BC_LINELTH, str2, status );
		if  (*status != SHE_NOERROR)  return;
		sl_oscall( str, s_par, str2, status );
	} else if  (strcmp(str,"LOCAL") == 0)  {
		cp_getstr( par, 2, tc, "   item: ", BC_LINELTH, s_par, status );
		if  (*status != SHE_NOERROR)  return;
		if  (cp_pentered(par,3,status))  {  /* then use as input */
			cp_getstr( par, 3, tc, "   item: ", BC_LINELTH, str, status );
			if  (*status != SHE_NOERROR)  return;
			strcpy( str2, str );  /* save input */
		} /*endif*/
		sy_localinf( s_par, str, status );
		if  (Severe(status))  return;
		if  (!cp_pentered(par,3,status))  /* because it was input */
			gc_write( cc, str );
		sl_setsymbol( str2, str, &locstat );  /* save output if possible */
	} else {
#ifdef XXX
		*status = SHE_UKFCT;
		err_setcontext( " ## subfct " ); err_setcontext( str );
#endif
		printf( "*SH: illegal fct item %s\n", str );
	} /*endif*/

} /* end of mn0_fct */



/*------------------------------------------------------------------------*/



void mn0_ecch( PARAM *par, STATUS *status )

/* selects echo channel
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     chname[BC_FILELTH+1];  /* name of output channel */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	if  (cp_pnum(par) == 0)  {  /* output to terminal */
		if  (echoch_mnv != NULL)  fclose( echoch_mnv );
		echoch_mnv = NULL;
	} else {
		cp_getstr( par, 1, tc, "  channel: ", BC_FILELTH, chname, status );
		if  (Severe(status))  return;
		if  (echoch_mnv != NULL)  fclose( echoch_mnv );
		if  (*chname == '\0')  {
			echoch_mnv = NULL;
			return;
		} /*endif*/
		ut_defext( chname, BC_FILELTH, SHC_DE_TEXT, status );
		if  (Severe(status))  return;
		if  (cp_qexist(par,"NEW"))  {
			echoch_mnv = sy_fopen( chname, "w" );
		} else if  (cp_qexist(par,"OVWR"))  {
			echoch_mnv = sy_fopen( chname, SYC_OPEN_OVWR );
		} else {
			echoch_mnv = sy_fopen( chname, "a" );
		} /*endif*/
		if  (echoch_mnv == NULL)  {
			*status = SHE_OPNWR;
			err_setcontext( " ## file " ); err_setcontext( chname );
			return;
		} /*endif*/
	} /*endif*/

} /* end of mn0_ecch */



/*------------------------------------------------------------------------*/



void mn0_echo( PARAM *par, STATUS *status )

/* writes to the current echo channel
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char    str[BC_LINELTH+1];    /* scratch */
	int     i;                    /* counter */
	char    term[2];              /* termination character */
#ifdef SH_SOCKET
	int     sock;                 /* socket to write to */
#endif

	/* executable code */

	*term = '\n';
	if  (cp_qexist(par,"NO_LF"))  {
		*term = '\r';
	} else if  (cp_qexist(par,"NO_CRLF"))  {
		*term = '\0';
	} /*endif*/
	term[1] = '\0';

	if  (cp_pnum(par) == 0)  {
		if  (echoch_mnv == NULL)  {
			gc_write( cc, term );
#ifdef SH_SOCKET
			sock = ui_lastsocket();
			if  (sock > 0)  write( sock, "\n", 2 );
#endif
		} else {
			fprintf( echoch_mnv, term );
		} /*endif*/
		return;
	} /*endif*/

	for  (i=1;i<=cp_pnum(par);i++)  {
		cp_getstr( par, i, tc, "@@", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		if  (echoch_mnv == NULL)  {
			if  (i < cp_pnum(par))  {
				strcat( str, " " );
			} else {
				strcat( str, term );
			} /*endif*/
			gc_write( cc, str );
#ifdef SH_SOCKET
			sock = ui_lastsocket();
			if  (sock > 0)  write( sock, str, strlen(str)+1 );
#endif
		} else {
			if  (i < cp_pnum(par))  {
				fprintf( echoch_mnv, "%s ", str );
			} else {
				fprintf( echoch_mnv, "%s%s", str, term );
			} /*endif*/
		} /*endif*/
	} /*endfor*/

} /* end of mn0_echo */



/*------------------------------------------------------------------------*/



void mn0_sdef( PARAM *par, STATUS *status )

/* defines a symbol
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     name[BC_LINELTH+1];    /* name of symbol */
	char     val[BC_LINELTH+1];     /* value of symbol */
	int      set;                   /* symbol set */
	int      locstat;               /* local status */

	/* executable code */

	set = SHC_SYMLOC;
	if  (cp_qexist(par,"GLOBAL"))  set = SHC_SYMGLB;

	if  (cp_pnexc(par,2,status))  return;
	cp_getstr( par, 1, tc, "   name: ", BC_LINELTH, name, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getstr( par, 2, tc, "   value: ", BC_LINELTH, val, status );
	if  (*status != SHE_NOERROR)  return;

	if  (cp_qexist(par,"REPLACE"))  {
		locstat = 0;
		ss_delete( set, name, &locstat );
	} else if  (cp_qexist(par,"CHECK"))  {
		if  (ss_exist(set,name))  return;
	} /*endif*/

	ss_define( set, name, val, status );

} /* end of mn0_sdef */



/*------------------------------------------------------------------------*/



void mn0_sdel( PARAM *par, STATUS *status )

/* deletes a symbol
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     name[BC_LINELTH+1];    /* name of symbol */
	int      set;                   /* symbol set */

	/* executable code */

	set = SHC_SYMLOC;
	if  (cp_qexist(par,"GLOBAL"))  set = SHC_SYMGLB;

	if  (cp_pnexc(par,1,status))  return;
	cp_getstr( par, 1, tc, "   name: ", BC_LINELTH, name, status );
	if  (*status != SHE_NOERROR)  return;
	ss_delete( set, name, status );

} /* end of mn0_sdel */



/*------------------------------------------------------------------------*/



void mn0_calc( PARAM *par, STATUS *status )

/* simple calculations
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * STATUS   *status;        output; return status
 */
{
	/* local constants */
#  define MAXOPLTH 12

	/* local variables */
	char     str[cBcLongStrLth+1];     /* scratch */
	char     type;                  /* operation type */
	char     symbol[BC_LINELTH+1];  /* symbol name for result */
	char     op[MAXOPLTH+1];        /* operation */
	int      parnum;                /* number of params */
	int      op1_i, op2_i;          /* integer operands */
	int      res_i;                 /* integer result */
	float    op1_r, op2_r;          /* floating operands */
	float    res_r;                 /* floating result */
	char     op1_s[cBcLongStrLth+1];/* 1. string operand */
	char     op2_s[cBcLongStrLth+1];/* 2. string operand */
	char     fmt[BC_SHORTSTRLTH+1]; /* format string */
	PARAM    lp;                    /* for line parsing */
	NTIME    ntime;                 /* numeric time */

	/* executable code */

	if  (cp_pnexc(par,7,status))  return;

	if  (sl_quals(par,"FMT",BC_SHORTSTRLTH,fmt,status))  {
		sl_prepformatstr( fmt );
	} else {
		*fmt = '\0';
		if  (Severe(status))  return;
	} /*endif*/

	cp_getstr( par, 1, tc, "   type: ", BC_LINELTH, str, status );
	if  (*status != SHE_NOERROR)  return;
	type = *str;
	cp_getstr( par, 2, tc, "   symbol: ", BC_LINELTH, symbol, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getstr( par, 3, tc, "   = ", BC_LINELTH, str, status );
	if  (*status != SHE_NOERROR)  return;
	if  (*str != '=')  {
		*status = SHE_CALC;
		err_setcontext( " ## must be '='" );
		return;
	} /*endif*/

	parnum = cp_pnum( par );
	if  (cp_pentered(par,5,status))  {
		if  (*status != SHE_NOERROR)  return;
		cp_getstr( par, 5, tc, "   operation: ", MAXOPLTH, op, status );
		if  (*status != SHE_NOERROR)  return;
	} else {
		if  (*status != SHE_NOERROR)  return;
		*op = '\0';
	} /*endif*/

	if  (type == 'I')  {
		cp_getint( par, 4, tc, "   op1: ", &op1_i, status );
		if  (*status != SHE_NOERROR)  return;
		if  (parnum >= 6)  {
			cp_getint( par, 6, tc, "   op2: ", &op2_i, status );
			if  (*status != SHE_NOERROR)  return;
		} /*endif*/
		if  (*op == '\0')  {
			res_i = op1_i;
		} else if  (strcmp(op,"+") == 0)  {
			res_i = op1_i + op2_i;
		} else if  (strcmp(op,"-") == 0)  {
			res_i = op1_i - op2_i;
		} else if  (strcmp(op,"*") == 0)  {
			res_i = op1_i * op2_i;
		} else if  (strcmp(op,"DIV") == 0)  {
			res_i = op1_i / op2_i;
		} else if  (strcmp(op,"MOD") == 0)  {
			res_i = op1_i % op2_i;
		} else if  (strcmp(op,"INT") == 0)  {
			cp_getfloat( par, 4, tc, "   op1: ", &op1_r, status );
			if  (Severe(status))  return;
			res_i = Nint( op1_r );
		} else {
			*status = SHE_CALC;
			err_setcontext( " ## illegal operator " ); err_setcontext( op );
			return;
		} /*endif*/
		if  (*fmt == '\0')  strcpy( fmt, "%d" );
		sprintf( str, fmt, res_i );
	} else if  (type == 'R')  {
		cp_getfloat( par, 4, tc, "   op1: ", &op1_r, status );
		if  (*status != SHE_NOERROR)  return;
		if  (parnum >= 6)  {
			cp_getfloat( par, 6, tc, "   op2: ", &op2_r, status );
			if  (*status != SHE_NOERROR)  return;
		} /*endif*/
		if  (*op == '\0')  {
			res_r = op1_r;
		} else if  (strcmp(op,"+") == 0)  {
			res_r = op1_r + op2_r;
		} else if  (strcmp(op,"-") == 0)  {
			res_r = op1_r - op2_r;
		} else if  (strcmp(op,"*") == 0)  {
			res_r = op1_r * op2_r;
		} else if  (strcmp(op,"DIV") == 0)  {
			res_r = op1_r / op2_r;
		} else if  (strcmp(op,"ABS") == 0)  {
			res_r = Abs( op1_r );
		} else if  (strcmp(op,"ARCSIN") == 0)  {
			res_r = asin( op1_r ) * SHC_RAD_TO_DEG;
		} else if  (strcmp(op,"ARCCOS") == 0)  {
			res_r = acos( op1_r ) * SHC_RAD_TO_DEG;
		} else if  (strcmp(op,"ARCTAN") == 0)  {
			res_r = atan( op1_r ) * SHC_RAD_TO_DEG;
		} else if  (strcmp(op,"ARCTAN2") == 0)  {
			res_r = atan2( op1_r, op2_r ) * SHC_RAD_TO_DEG;
		} else if  (strcmp(op,"COS") == 0)  {
			res_r = cos( op1_r/SHC_RAD_TO_DEG );
		} else if  (strcmp(op,"SIN") == 0)  {
			res_r = sin( op1_r/SHC_RAD_TO_DEG );
		} else if  (strcmp(op,"TAN") == 0)  {
			res_r = tan( op1_r/SHC_RAD_TO_DEG );
		} else if  (strcmp(op,"COSH") == 0)  {
			res_r = cosh( op1_r );
		} else if  (strcmp(op,"SINH") == 0)  {
			res_r = sinh( op1_r );
		} else if  (strcmp(op,"TANH") == 0)  {
			res_r = tanh( op1_r );
		} else if  (strcmp(op,"EXP") == 0)  {
			res_r = exp( op1_r );
		} else if  (strcmp(op,"LN") == 0)  {
			res_r = log( op1_r );
		} else if  (strcmp(op,"LOG") == 0)  {
			res_r = log10( op1_r );
		} else if  (strcmp(op,"POWER") == 0)  {
			res_r = pow( op1_r, op2_r );
		} else if  (strcmp(op,"SQRT") == 0)  {
			res_r = sqrt( op1_r );
		} else {
			*status = SHE_CALC;
			err_setcontext( " ## illegal operator " ); err_setcontext( op );
			return;
		} /*endif*/
		if  (*fmt == '\0')  strcpy( fmt, "%e" );
		sprintf( str, fmt, res_r );
	} else if  (type == 'S')  {
		cp_getstr( par, 4, tc, "   op1: ", cBcLongStrLth, op1_s, status );
		if  (*status != SHE_NOERROR)  return;
		*op2_s = '\0';
		if  (parnum >= 6)  {
			cp_getstr( par, 6, tc, "   op2: ", cBcLongStrLth, op2_s, status );
			if  (*status != SHE_NOERROR)  return;
		} /*endif*/
		if  (*op == '\0')  {
			strcpy( str, op1_s );
		} else if  (strcmp(op,"+") == 0)  {
			if  (strlen(op1_s)+strlen(op2_s)+1 > cBcLongStrLth)  {
				*status = SHE_STROVFL;
				return;
			} /*endif*/
			strcpy( str, op1_s );
			strcat( str, " " );
			strcat( str, op2_s );
		} else if  (strcmp(op,"PARSE") == 0)  {
			cp_getint( par, 6, tc, "@@", &op1_i, status );
			if  (Severe(status))  return;
			cp_parse( op1_s, &lp, status );
			if  (Severe(status))  return;
			op1_i--;
			if  (op1_i < 0 || op1_i > lp.pno)  {
				*status = SHE_ILPAR;
				err_setcontext( " ## not existing parse element" );
				return;
			} /*endif*/
			strcpy( str, lp.p[op1_i] );
		} else if  (strcmp(op,"LOWER") == 0)  {
			if  (*op2_s == '\0')  *op2_s = '.';
			sl_getlowercase( op1_s, *op2_s, str );
		} else if  (strcmp(op,"LOWERCASE") == 0)  {
			sl_makelowercase( op1_s, str );
		} else if  (strcmp(op,"UPPER") == 0)  {
			strcpy( str, op1_s );
			ut_cap( str );
		} else if  (strcmp(op,"PHASEUPPER") == 0)  {
			sl_cnvphasename( op1_s, cBcLongStrLth, str, status );
			if  (Severe(status))  return;
		} else if  (strcmp(op,"DOSLASH") == 0)  {
			strcpy( str, op1_s );
			ut_replacechar( str, '\\', '/' );
		} else if  (strcmp(op,"QNAME") == 0)  {
			sl_make_qname( op1_s, op2_s, cBcLongStrLth, str, status );
			if  (Severe(status))  return;
		} else if  (strcmp(op,"STRLEN") == 0)  {
			sprintf( str, "%d", (int)strlen(op1_s) );
		} else if  (strcmp(op,"EXTRACT") == 0)  {
			cp_getint( par, 6, tc, "@@", &op1_i, status );
			if  (Severe(status))  return;
			cp_getint( par, 7, tc, "@@", &op2_i, status );
			if  (Severe(status))  return;
			if  (op2_i > BC_LINELTH)  {
				*status = SHE_STROVFL;
				return;
			} else if  (op1_i > strlen(op1_s))  {
				*str = '\0';
			} else {
				strncpy( str, op1_s+op1_i-1, op2_i );
				str[op2_i] = '\0';
			} /*endif*/
		} else {
			*status = SHE_CALC;
			err_setcontext( " ## illegal operator " ); err_setcontext( op );
			return;
		} /*endif*/
	} else if  (type == 'T')  {
		if  (strcmp(op,"TDIFF") == 0)  {
			cp_getstr( par, 4, tc, "   t1: ", BC_LINELTH, op1_s, status );
			if  (*status != SHE_NOERROR)  return;
			cp_getstr( par, 6, tc, "   t2: ", BC_LINELTH, op2_s, status );
			if  (*status != SHE_NOERROR)  return;
			res_r = tc_tdiff( op1_s, op2_s, status );
			if  (Severe(status))  return;
			if  (*fmt == '\0')  strcpy( fmt, "%e" );
			sprintf( str, fmt, res_r );
		} else if  (strcmp(op,"TADD") == 0)  {
			cp_getstr( par, 4, tc, "   t1: ", BC_LINELTH, op1_s, status );
			if  (Severe(status))  return;
			cp_getfloat( par, 6, tc, "   add: ", &op1_r, status );
			if  (Severe(status))  return;
			tc_tadd( op1_s, op1_r, str, status );
			if  (Severe(status))  return;
		} else if  (strcmp(op,"JULIAN") == 0)  {
			cp_getstr( par, 4, tc, "   t1: ", BC_LINELTH, op1_s, status );
			if  (Severe(status))  return;
			tc_t2n( op1_s, &ntime, status );
			if  (Severe(status))  return;
			res_i = tc_julian( ntime.year, ntime.month, ntime.day, status );
			if  (Severe(status))  return;
			sprintf( str, "%03d", res_i );
		} else if  (strcmp(op,"CNV_JULIAN") == 0)  {
			cp_getint( par, 4, tc, "   year: ", &op1_i, status );
			if  (Severe(status))  return;
			cp_getint( par, 6, tc, "   julday: ", &op2_i, status );
			if  (Severe(status))  return;
			tc_dayofmn( op1_i, op2_i, (unsigned *)&op1_i,
				(unsigned *)&op2_i, status );
			if  (Severe(status))  return;
			sprintf( str, "%d %d", op1_i, op2_i );
		} else if  (strcmp(op,"MAKE_TIME") == 0)  {
			cp_getstr( par, 4, tc, "   num time: ", BC_LINELTH,
				op1_s, status );
			if  (Severe(status))  return;
			sl_make_timestr( op1_s, str, status );
			if  (Severe(status))  return;
		} else {
			*status = SHE_CALC;
			err_setcontext( " ## illegal operator " ); err_setcontext( op );
			return;
		} /*endif*/
	} else {
		*status = SHE_ILTYPE;
		err_setcontext( " ## illegal result type" );
		return;
	} /*endif*/

	sl_setsymbol( symbol, str, status );

} /* end of mn0_calc */



/*------------------------------------------------------------------------*/



void mn0_goto( PARAM *par, STATUS *status )

/* jumps to a label (valid only in command procedures)
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     label[BC_LINELTH+1];   /* label name */
	int      forward;               /* jump forward */

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	forward = cp_qexist(par,"FORWARD");
	cp_getstr( par, 1, tc, "   label: ", BC_LINELTH, label, status );
	if  (*status != SHE_NOERROR)  return;
	ui_goto( label, (!forward), status );

} /* end of mn0_goto */



/*------------------------------------------------------------------------*/



void mn0_if( PARAM *par, STATUS *status )

/* conditional command (valid only in command procedures)
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * STATUS   *status;        output; return status
 */
{
	/* local constants */
#  undef MAXOPLTH
#  define MAXOPLTH 3

	/* local variables */
	char     op[MAXOPLTH+1];          /* compare operation */
	char     command[BC_LINELTH+1];   /* conditional command */
	char     label[BC_LINELTH+1];     /* label (goto command) */
	int      condition;               /* result of comparison */
	int      forward;                 /* jump forward */
	int      op1_i, op2_i;            /* integer operands */
	float    op1_r, op2_r;            /* floating operands */
	char     op1_s[BC_LINELTH+1];     /* 1. string operand */
	char     op2_s[BC_LINELTH+1];     /* 2. string operand */

	/* executable code */

	if  (cp_pnum(par) < 4)  {
		*status = SHE_LESSPAR;
		return;
	} /*endif*/

	cp_getstr( par, 2, tc, "", MAXOPLTH, op, status );
	if  (*status != SHE_NOERROR)  return;
	forward = cp_qexist(par,"FORWARD");

	switch  (op[2])  {
		case 'I':
			cp_getint( par, 1, tc, "", &op1_i, status );
			if  (*status != SHE_NOERROR)  return;
			cp_getint( par, 3, tc, "", &op2_i, status );
			if  (*status != SHE_NOERROR)  return;
			break;
		case 'R':
			cp_getfloat( par, 1, tc, "", &op1_r, status );
			if  (*status != SHE_NOERROR)  return;
			cp_getfloat( par, 3, tc, "", &op2_r, status );
			if  (*status != SHE_NOERROR)  return;
			break;
		case 'S':
			cp_getstr( par, 1, tc, "", BC_LINELTH, op1_s, status );
			if  (*status != SHE_NOERROR)  return;
			cp_getstr( par, 3, tc, "", BC_LINELTH, op2_s, status );
			if  (*status != SHE_NOERROR)  return;
			break;
		default:
			*status = SHE_ILPAR;
			err_setcontext( " ## illegal operator " ); err_setcontext( op );
			return;
	} /*endswitch*/

	if  (strcmp(op,"EQI") == 0)  {
		condition = (op1_i == op2_i);
	} else if  (strcmp(op,"NEI") == 0)  {
		condition = (op1_i != op2_i);
	} else if  (strcmp(op,"LEI") == 0)  {
		condition = (op1_i <= op2_i);
	} else if  (strcmp(op,"LTI") == 0)  {
		condition = (op1_i < op2_i);
	} else if  (strcmp(op,"GEI") == 0)  {
		condition = (op1_i >= op2_i);
	} else if  (strcmp(op,"GTI") == 0)  {
		condition = (op1_i > op2_i);
	} else if  (strcmp(op,"EQR") == 0)  {
		condition = (op1_r == op2_r);
	} else if  (strcmp(op,"NER") == 0)  {
		condition = (op1_r != op2_r);
	} else if  (strcmp(op,"LER") == 0)  {
		condition = (op1_r <= op2_r);
	} else if  (strcmp(op,"LTR") == 0)  {
		condition = (op1_r < op2_r);
	} else if  (strcmp(op,"GER") == 0)  {
		condition = (op1_r >= op2_r);
	} else if  (strcmp(op,"GTR") == 0)  {
		condition = (op1_r > op2_r);
	} else if  (strcmp(op,"EQS") == 0)  {
		condition = (strcmp(op1_s,op2_s) == 0);
	} else if  (strcmp(op,"NES") == 0)  {
		condition = (strcmp(op1_s,op2_s) != 0);
	} else {
		*status = SHE_ILPAR;
		err_setcontext( " ## illegal cmp " ); err_setcontext( op );
		return;
	} /*endif*/

	if  (!condition)  return;

	cp_getstr( par, 4, tc, "", BC_LINELTH, command, status );
	if  (*status != SHE_NOERROR)  return;

	if  (strcmp(command,"GOTO") == 0)  {
		if  (!cp_pentered(par,5,status))  {
			*status = SHE_LESSPAR;
			return;
		} /*endif*/
		cp_getstr( par, 5, tc, "", BC_LINELTH, label, status );
		if  (*status != SHE_NOERROR)  return;
		ui_goto( label, (!forward), status );
	} else if  (strcmp(command,"RETURN") == 0)  {
		mn0_cmdreturn( status );
	} else {
		*status = SHE_ILPAR;
		err_setcontext( " ## illegal if command " ); err_setcontext( command );
		return;
	} /*endif*/

} /* end of mn0_if */



/*------------------------------------------------------------------------*/



void mn0_switch( PARAM *par, SHFLAGS *flags, STATUS *status )

/* changes flags
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * SHFLAGS  *flags;         output; global flags
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     name[BC_LINELTH+1];  /* name of switch */
	char     val[10];             /* on or off */
	int      value;               /* switch value */
	int      flag;                /* flag to be set or cleared */

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;
	cp_getstr( par, 1, tc, "   switch: ", BC_LINELTH, name, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getstr( par, 2, tc, "   on/off: ", 9, val, status );
	if  (*status != SHE_NOERROR)  return;

	if  (strcmp(val,"ON") == 0)  {
		value = TRUE;
	} else if  (strcmp(val,"OFF") == 0)  {
		value = FALSE;
	} else {
		*status = SHE_ILPAR;
		err_setcontext( " ## illegal value " ); err_setcontext( val );
		return;
	} /*endif*/

	if  (strcmp(name,"VERIFY") == 0)  {
		flag = SHF_VERIFY;
	} else if  (strcmp(name,"CMDERRSTOP") == 0)  {
		flag = SHF_CMDERRSTOP;
	} else if  (strcmp(name,"SHERRSTOP") == 0)  {
		flag = SHF_SHERRSTOP;
	} else if  (strcmp(name,"PROTOCOL") == 0)  {
		flag = SHF_LOGCMD;
	} else if  (strcmp(name,"ECHO") == 0)  {
		flag = SHF_ECHO;
	} else if  (strcmp(name,"CAPCNV") == 0)  {
		flag = SHF_CAPCNV;
	} else if  (strcmp(name,"STEP") == 0)  {
		flag = SHF_STEP;
	} else if  (strcmp(name,"NOERRMSG") == 0)  {
		flag = SHF_NOERRMSG;
	} else if  (strcmp(name,"CHATTER") == 0)  {
		flag = SHF_CHATTY;
	} else if  (strcmp(name,"CHATTY") == 0)  {
		flag = SHF_CHATTY;
	} else if  (strcmp(name,"STARTUP") == 0)  {
		flag = SHF_STARTUP;
	} else {
		*status = SHE_ILPAR;
		err_setcontext( " ## illegal switch " ); err_setcontext( name );
		return;
	} /*endif*/

	*flags = (value) ? (*flags | flag) : (*flags & ~flag);
	ui_absflag( *flags );

	if  (cp_qexist(par,"GLOBAL"))
		shglbflags_shv = (value) ? (shglbflags_shv | flag)
			: (shglbflags_shv & ~flag);

} /* end of mn0_switch */



/*------------------------------------------------------------------------*/



void mn0_default( PARAM *par, STATUS *status )

/* sets default value of command parameters
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      i;                    /* counter */
	int      parno;                /* parameter number */
	char     value[BC_LINELTH+1];  /* default value */
	char     str[BC_LINELTH+1];    /* scratch */
	int      inplev;               /* input level */

	/* executable code */

	cp_getint( par, 1, tc, "   par no: ", &parno, status );
	if  (*status != SHE_NOERROR)  return;
	cp_getstr( par, 2, tc, "   value: ", BC_LINELTH, value, status );
	if  (*status != SHE_NOERROR)  return;

	if  (cp_qexist(par,"PARENT"))  {
		inplev = -1;
	} else if  (cp_qexist(par,"INTERACTIVE"))  {
		inplev = shv_maininput;
	} else {
		inplev = ss_inplev();
	} /*endif*/

	*str = '\0';
	if  (ss_query())  {
		for  (i=3;i<=cp_pnum(par);i++)  {
			cp_getstr( par, i, tc, "", BC_LINELTH, str, status );
			if  (*status != SHE_NOERROR)  return;
			gc_write( cc, str );
			gc_write( cc, " " );
		} /*endfor*/
		if  (*value != '\0')  {
			gc_write( cc, "[default: " );
			gc_write( cc, value );
			gc_write( cc, "]: " );
		} else {
			gc_write( cc, ": " );
		} /*endif*/
		ui_shift( inplev );
		ui_read( cc, str, BC_LINELTH, status );
		ui_unshift();  /* switch back to current level */
		if  (*status != SHE_NOERROR)  return;
	} /*endif*/

	if  (*str == '\0')  {
		ss_default( parno, value, status );
	} else {
		ss_default( parno, str, status );
	} /*endif*/

} /* end of mn0_default */



/*------------------------------------------------------------------------*/



void mn0_enter( PARAM *par, STATUS *status )

/* reads a symbol value from parent command level
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      i;                      /* counter */
	char     symbol[BC_LINELTH+1];   /* symbol name */
	char     str[BC_LINELTH+1];      /* scratch */
	int      inplev;                 /* input level */

	/* executable code */

	cp_getstr( par, 1, tc, "   symbol: ", BC_LINELTH, symbol, status );
	if  (*status != SHE_NOERROR)  return;
	if  (*symbol != SHC_CHWRITE)  {
		*status = SHE_NOSYM;
		err_setcontext( " ## symbol " ); err_setcontext( symbol );
		return;
	} /*endif*/

	if  (cp_qexist(par,"PARENT"))  {
		inplev = -1;
	} else if  (cp_qexist(par,"INTERACTIVE"))  {
		inplev = shv_maininput;
	} else {
		inplev = ss_inplev();
	} /*endif*/

	for  (i=2;i<=cp_pnum(par);i++)  {
		cp_getstr( par, i, tc, "", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		gc_write( cc, str );
		gc_write( cc, " " );
	} /*endfor*/
	ui_shift( inplev );
	ui_read( cc, str, BC_LINELTH, status );
	ui_unshift();  /* switch back to current level */
	if  (*status != SHE_NOERROR)  return;

	ss_change( SHC_SYMLOC, symbol+1, str, status );
	if  (*status == SSE_UDSYM)  {
		*status = SSE_NOERROR;
		ss_change( SHC_SYMGLB, symbol+1, str, status );
	} /*endif*/

} /* end of mn0_enter */



/*------------------------------------------------------------------------*/



void mn0_hotkey( PARAM *par, STATUS *status )

/* reads a character (hotkey)
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	int      i;                      /* counter */
	char     symbol[BC_LINELTH+1];   /* symbol name */
	char     str[2];                 /* scratch */
	int      inplev;                 /* input level */

	/* executable code */

	cp_getstr( par, 1, tc, "   symbol: ", BC_LINELTH, symbol, status );
	if  (*status != SHE_NOERROR)  return;
	if  (*symbol != SHC_CHWRITE)  {
		*status = SHE_NOSYM;
		err_setcontext( " ## symbol " ); err_setcontext( symbol );
		return;
	} /*endif*/

	if  (cp_qexist(par,"PARENT"))  {
		inplev = -1;
	} else if  (cp_qexist(par,"INTERACTIVE"))  {
		inplev = shv_maininput;
	} else {
		inplev = ss_inplev();
	} /*endif*/

	for  (i=2;i<=cp_pnum(par);i++)  {
		cp_getstr( par, i, tc, "", BC_LINELTH, str, status );
		if  (*status != SHE_NOERROR)  return;
		gc_write( cc, str );
		gc_write( cc, " " );
	} /*endfor*/
	ui_shift( inplev );
	*str = ui_readchar( cc, status );
	str[1] = '\0';
	ui_unshift();  /* switch back to current level */
	if  (*status != SHE_NOERROR)  return;

	ss_change( SHC_SYMLOC, symbol+1, str, status );
	if  (*status == SSE_UDSYM)  {
		*status = SSE_NOERROR;
		ss_change( SHC_SYMGLB, symbol+1, str, status );
	} /*endif*/

} /* end of mn0_hotkey */



/*------------------------------------------------------------------------*/



void mn0_quit( PARAM *par, BOOLEAN *quit, STATUS *status )

/* quit seismhandler command
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * BOOLEAN  *quit;          output; quit flag
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     s[5];

	/* executable code */

	if  (cp_pnexc(par,2,status))  return;
	if  (ui_level() == 0)  {
		cp_getstr( par, 1, tc, "   leave program ? [y|N] ", 4, s, status );
		if  (*status != SHE_NOERROR)  return;
		*quit = (*s == 'Y');
	} else {
		mn0_cmdreturn( status );
		*quit = FALSE;
		if  (cp_pentered(par,1,status))  {
			cp_getstr( par, 1, tc, "@@", 4, s, status );
			if  (Severe(status))  return;
			if  (*s == 'Y')  {
				if  (cp_pentered(par,2,status))  {
					cp_getstr( par, 2, tc, "@@", 4, s, status );
					if  (Severe(status))  return;
					*quit = (*s == 'Y');
				} /*endif*/
			} /*endif*/
		} /*endif*/
	} /*endif*/

} /* end of mn0_quit */



/*------------------------------------------------------------------------*/



void mn0_system( PARAM *par, STATUS *status )

/* system command
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * STATUS   *status;        output; return status
 */
{
	/* local variables */
	char     cmd[BC_LINELTH+1];

	/* executable code */

	if  (cp_pnexc(par,1,status))  return;
	cp_getstr( par, 1, tc, "   sys-cmd: ", BC_LINELTH, cmd, status );
	if  (Severe(status))  return;
	if  (cp_qexist(par,"DOSLASH"))
		ut_replacechar( cmd, '\\', '/' );
	if  (cp_qexist(par,"ECHO"))  {
		gc_write( cc, cmd );
		gc_write(cc, "\n" );
	} /*endif*/
	sy_system( cmd, status );

} /* end of mn0_system */



/*------------------------------------------------------------------------*/



void mn0_help( PARAM *par, STATUS *status )

/* help command
 *
 * parameters of routine
 * PARAM    *par;           input; command parameter
 * STATUS   *status;        output; return status
 */
{
	/* local constants */
#   define HELPWDW 7

	/* local variables */
	char      wild[BC_LINELTH+1];      /* name of command(s) */
	int       locstat1, locstat2;     /* local status */
	int      ch;                     /* output channel */

	/* executable code */

	locstat1 = 0;
	locstat2 = 0;
	if  (cp_pnexc(par,1,status))  return;
	if  (cp_pentered(par,1,status))  {
		cp_getstr( par, 1, tc, "@@", BC_LINELTH, wild, status );
	} else {
		strcpy( wild, "*" );
	} /*endif*/
	if  (*status != SHE_NOERROR)  return;

	ch = cc;
	if  (cp_qexist(par,"NW"))   {
		gc_init( HELPWDW, GCF_WNAME|GCF_WCLOSER, 0.0, 0.0, 20.0, 11.9, status );
		if  (*status != SHE_NOERROR)  return;
		ch = HELPWDW;
	} /*endif*/

	if  (cp_qexist(par,"DIR"))  {
		hl_dir( ch, NULL, wild, gc_txtwidth(ch), &locstat1 );
		hl_dir( ch, GpGetString(cGpS_defpath_help), wild, gc_txtwidth(ch),
			&locstat2 );
		if  ((locstat1 != SHE_NOERROR) && (locstat2 != SHE_NOERROR))
			*status = locstat2;
	} else if  (cp_qexist(par,"CALL"))  {
		/* gc_write( tc, "--> help/call\n" ); */
		hl_hdr( ch, NULL, wild, &locstat1 );
		hl_hdr( ch, GpGetString(cGpS_defpath_help), wild, &locstat2 );
		/* printf( "--> after hl_hdr: %d %d\n", locstat1, locstat2 ); */
		if  ((locstat1 != SHE_NOERROR) && (locstat2 != SHE_NOERROR))
			*status = locstat2;
	} else if  (cp_qexist(par,"KEY"))  {
		hl_key( ch, NULL, wild, gc_txtwidth(ch), &locstat1 );
		hl_key( ch, GpGetString(cGpS_defpath_help), wild, gc_txtwidth(ch),
			&locstat2 );
		if  ((locstat1 != SHE_NOERROR) && (locstat2 != SHE_NOERROR))
			*status = locstat2;
	} else {
		if  (cp_pentered(par,1,status))  {
			hl_full( ch, NULL, wild, &locstat1 );
			hl_full( ch, GpGetString(cGpS_defpath_help), wild, &locstat2 );
		} else {
			hl_dir( ch, NULL, wild, gc_txtwidth(ch), &locstat1 );
			hl_dir( ch, GpGetString(cGpS_defpath_help), wild, gc_txtwidth(ch),
				&locstat2 );
		} /*endif*/
		if  ((locstat1 != SHE_NOERROR) && (locstat2 != SHE_NOERROR))
			*status = locstat2;
	} /*endif*/

	if  (ch != cc)  {
		gc_write( ch, "   continue with <CR> ..." );
		gc_read( ch, BC_LINELTH, wild );
		gc_exit( HELPWDW );
	} /*endif*/

} /* end of mn0_help */



/*-------------------------------------------------------------------------*/



void mn0_wdw( PARAM *par, STATUS *status )

/* sets window parameter
 *
 * parameters of routine
 * PARAM    *par;           input; name & parameters
 * int      *status;        output; return status
 *
 * 1. param:  keyword
 */
{
	/* local variables */
	char     keyword[BC_LINELTH+1];  /* keyword */
	int      wdw;           /* window number */
	float    rpar[4];       /* float parameters */
	int      ipar;
	int      attribs=0;     /* window attributes */

	/* executable code */

	if  (cp_qnum(par) > 0)  {
		sl_setoutput( par, &tc, &tc_shv, &gc, &gc_shv, &cc, &cc_shv,
			&mainchset_mnv, status );
		if  (*status != SHE_NOERROR)  return;
		sl_updatewdwnames( tc, gc, cc );
		sl_chgflags( par, &shflags_shv, &shglbflags_shv, status );
		if  (*status != SHE_NOERROR)  return;
		if  (cp_pnum(par) == 0)  return;
	} /*endif*/

	cp_getstr( par, 1, tc, "   keyword: ", BC_LINELTH, keyword, status );
	if  (*status != SHE_NOERROR)  return;

	if  (strcmp(keyword,"SIZE") == 0)  {
		if  (cp_pnexc(par,6,status))  return;
		cp_getstr( par, 2, tc, "   chan: ", BC_LINELTH, keyword, status );
		if  (*status != SHE_NOERROR)  return;
		sl_cnvgcflags( keyword, &wdw, status );
		if  (Severe(status))  return;
		cp_getfloat( par, 3, tc, "   xpos: ", rpar, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 4, tc, "   ypos: ", rpar+1, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 5, tc, "   width: ", rpar+2, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 6, tc, "   height: ", rpar+3, status );
		if  (*status != SHE_NOERROR)  return;
		gc_resizewdw( wdw, rpar[0], rpar[1], rpar[2], rpar[3], status );
		if  (*status != SHE_NOERROR)  return;
		/* gc_redraw( wdw, status ); */
	} else if  (strcmp(keyword,"TOP") == 0)  {
		if  (cp_pnexc(par,2,status))  return;
		cp_getstr( par, 2, tc, "   chan: ", BC_LINELTH, keyword, status );
		if  (*status != SHE_NOERROR)  return;
		sl_cnvgcflags( keyword, &wdw, status );
		if  (Severe(status))  return;
		gc_popwdw( wdw );
	} else if  (strcmp(keyword,"CREATE") == 0)  {
		if  (cp_pnexc(par,6,status))  return;
		cp_getstr( par, 2, tc, "   chan: ", BC_LINELTH, keyword, status );
		if  (*status != SHE_NOERROR)  return;
		sl_cnvgcflags( keyword, &wdw, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 3, tc, "   xpos: ", rpar, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 4, tc, "   ypos: ", rpar+1, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 5, tc, "   width: ", rpar+2, status );
		if  (*status != SHE_NOERROR)  return;
		cp_getfloat( par, 6, tc, "   height: ", rpar+3, status );
		if  (*status != SHE_NOERROR)  return;
		if  (sl_quals(par,"ATTRIBS",BC_LINELTH,keyword,status))
			sl_cnvwdwflags( keyword, &attribs, status );
		if  (*status != SHE_NOERROR)  return;
		gc_init( wdw, attribs, rpar[0], rpar[1], rpar[2], rpar[3], status );
		if  (*status != SHE_NOERROR)  return;
	} else if  (strcmp(keyword,"DELETE") == 0)  {
		if  (cp_pnexc(par,2,status))  return;
		cp_getstr( par, 2, tc, "   chan: ", BC_LINELTH, keyword, status );
		if  (*status != SHE_NOERROR)  return;
		sl_cnvgcflags( keyword, &wdw, status );
		if  (*status != SHE_NOERROR)  return;
		ipar = (wdw & SHC_WDWMASK);
		if  (ipar == (tc & SHC_WDWMASK))  *status = SHE_SPECERROR+4;
		if  (ipar == (gc & SHC_WDWMASK))  *status = SHE_SPECERROR+4;
		if  (ipar == (cc & SHC_WDWMASK))  *status = SHE_SPECERROR+4;
		if  (db_dspfirst(wdw,NULL) != NULL)  *status = SHE_SPECERROR+5;
		if  Severe(status)  return;
		gc_exit( wdw );
	} else {
		*status = SHE_UKKEY;
		err_setcontext( " ## key " ); err_setcontext( keyword );
		return;
	} /*endif*/

} /* end of mn0_wdw */



/*------------------------------------------------------------------------*/


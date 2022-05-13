
/* file shexec.c
 *      ========
 *
 * version 27, 12-Jan-2007
 *
 * main subroutines of SH
 * K. Stammler, 10-Feb-93
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
#include "sysbase.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#ifdef BC_INC_UNISTD
#include BC_INC_UNISTD
#endif
#endif
#include "seusrdef.h"
#include "cpusrdef.h"
#include "fctxmn0.h"
#include "fctxmn1.h"
#include "fctxmn2.h"
#include "fctxmn3.h"
#include "fctxmn4.h"
#include "fctxmn5.h"
#include "fctxmni.h"
#include "fctxmnx.h"
#include "fctxdm.h"
#include "fctxmt.h"
#include "shvars.h"
#include "erusrdef.h"
#include "ssusrdef.h"
#include "uiusrdef.h"
#include "causrdef.h"
#include "qiusrdef.h"
#include "qfusrdef.h"
#include "earthloc.h"
#include "sherrors.h"
#include "residual.h"



/* prototype for readk if used */
#ifdef SH_SETUP_READK
void mngrf_readk( PARAM *cmd, STATUS *status );
#endif



/*------------------------------------------------------------------------*/



void se_initialize( int argc, char *argv[], STATUS *status )

/* initializes SH program:
 * (i)    set session ID string
 * (ii)   UNIX path names or path list file (set shd_... variables)
 * (iii)  set input/output dirs for graphic (gc_...) routines
 * (iv)   set global vars: shflags_shv, shglbflags_shv, graphic channels
 * (v)    open protocol file
 * (vi)   initialize q-file interface
 * (vii)  set default names for earthloc input files (mb_...)
 * (viii) open startup file (set ui_level to 1)
 *
 * parameters of routine
 * int        argc;      input; command line parameters
 * char       argv[];    input; -- " --
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	char     str[BC_LINELTH+1];      /* scratch string */
	BOOLEAN  ok;                     /* status ok ? */
	PARAM    cmd;                    /* SH command block */

	/* executable code */

	/* get path names: either from UNIX environment ... */
#	ifdef BC_SUN
	se_get_sh_environment();
#	endif
   /* get path names: ... or from input file */
#  ifdef BC_MAINARGS
	ca_prepare( argc, argv );
	ca_qstr( argc, argv, "PATHS=", str );
	if  (*str != '\0')  {
		se_readshpaths( str, &ok );
		if  (!ok)
			printf( "*** error reading path names ***\n" );
	} /*endif*/
#  endif /* BC_MAINARGS */

	/* get session ID string */
	strcpy( id_shv, SHC_FILE_PREFIX );
#	ifdef BC_VAX
	mt_randomstr( 4, id_shv+SHC_FILE_PREFIX_LENGTH );
	strcat( id_shv, "_" );
#	endif  /* BC_VAX */
#	ifdef BC_SUN
	mt_randomstr( 4, id_shv+SHC_FILE_PREFIX_LENGTH );
	strcat( id_shv, "_" );
#	endif  /* BC_SUN */
#	ifdef BC_ATARI
	id_shv[SHC_FILE_PREFIX_LENGTH] = '0'+Nint(4.5+mt_random(5.0));
	id_shv[SHC_FILE_PREFIX_LENGTH+1] = '\0';
#	endif /* BC_ATARI */

	strcpy( str, shd_scratch );
#	ifdef SHC_HCFILE_WITH_RANDOM
	strcat( str, id_shv );
#	else
	strcat( str, SHC_FILE_PREFIX );
#	endif
	gc_set_outputdir( str, status );
	gc_set_inputdir( shd_inputs, status );

	/* setup global flags */
	shflags_shv = SHF_LOGCMD|SHF_CAPCNV|SHF_CMDERRSTOP|SHF_CHATTY;
	shglbflags_shv = shflags_shv;

	/* set preliminary output channels */
	gc = gc_shv = tc = tc_shv = cc = cc_shv = 0;

	/* initialize protocol file */
	strcpy( protfile_shv, shd_scratch );
	strcat( protfile_shv, id_shv );
	strcat( protfile_shv, "PROT" );
	ui_initialize( protfile_shv, status );
	if  (Severe(status))  return;
	ui_absflag( shflags_shv );

	/* initialize q-file interface */
	qi_initcnv();

	/* initialize names of input files */
	strcpy( str, shd_inputs );
	strcat( str, "fereg.dat" );
	mb_setindexfile( str, status );
	if  (Severe(status))
		sy_alert( "*** didn't set FER index file name properly ***\n" );
	*status = BC_NOERROR;
	strcpy( str, shd_inputs );
	strcat( str, "fername.dat" );
	mb_setfernamefile( str, status );
	if  (Severe(status))
		sy_alert( "*** didn't set FER name file name properly ***\n" );
	*status = BC_NOERROR;

	/* open startup command file */
#  ifdef BC_MAINARGS
	ca_qstr( argc, argv, "STARTUP=", str );
	if  (*str == '\0')
		ca_qstr( argc, argv, "", str );
	if  (*str == '\0')
		strcpy( str, "SHSTRTUP" );
	strcat( str, "/FLAGS+=AF" );
	cp_parse( str, &cmd, status );
#  else
	cp_parse( "SHSTRTUP/FLAGS+=AF", &cmd, status );
#  endif /* BC_MAINARGS */
	mn0_callproc( &cmd, status );
	if  (Severe(status))  {
		sy_alert( "*** no startup file found ***" );
		return;
	} /*endif*/

	RsReadTables( "default", status );
	if  (Severe(status))  {
		sy_alert( "*** error reading residual tables ***" );
		*status = cBcNoError;
	} /*endif*/

} /* end of se_initialize */



/*------------------------------------------------------------------------*/



void se_terminate( void )

/* closes all files and renames log file
 *
 * no parameters
 */
{
	/* local variables */
	STATUS   status;             /* return status */
	char     str[BC_LINELTH+1];  /* scratch string */

	/* executable code */

	status = BC_NOERROR;
	qf_rclose( &status );
	strcpy( str, shd_scratch );
	strcat( str, SHC_FILE_PREFIX );
	strcat( str, "LAST" );
	strcat( str, SHC_DE_CMD );
	sy_fdelete( str );
	strcpy( str, shd_scratch );
	strcat( str, SHC_FILE_PREFIX );
	strcat( str, "LAST" );
	ui_exit( str );
	gc_finish();

} /* end of se_terminate */



/*------------------------------------------------------------------------*/



void se_execute_command( PARAM *cmd, char cmdlin[], char execstr[],
	BOOLEAN *quit, BOOLEAN *redraw, BOOLEAN *iscmdproc, int *rdlevel,
	char prompt[], STATUS *status )

/* executes the command "cmd"
 *
 * parameters of routine
 * PARAM      *cmd;      input; command verb & parameters
 * char       cmdlin[];  input; command line
 * char       execstr[]; output; next command line (from EXEC command)
 * BOOLEAN    *quit;     output; quit program
 * BOOLEAN    *redraw;   output; redraw screen after command
 * int        *rdlevel;  modify; redraw level
 * char       prompt[];  output; prompt string
 * STATUS     *status;   output; return status
 */
{
	/* executable code */

	*iscmdproc = FALSE;

	switch  (cmd->p[0][0])  {
	case 'A':
		if  (cp_cmdverb(cmd,"AL"))  {
			mnx_al( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"AM"))  {
			mnx_am( cmd, status );
		} else if  (cp_cmdverb(cmd,"APPEND"))  {
			mn4_append( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"ARP"))  {
			mn3_arp( cmd, status );
			*redraw = TRUE;
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'B':
		if  (cp_cmdverb(cmd,"BEAM"))  {
			mnx_beam( cmd, status );
			*redraw = TRUE;
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'C':
		if  (cp_cmdverb(cmd,"CALC"))  {
			mn0_calc( cmd, status );
		} else if  (cp_cmdverb(cmd,"CALL"))  {
			mn3_call( cmd, status );
		} else if  (cp_cmdverb(cmd,"CMD"))  {
			mn2_cmd( cmd, status );
		} else if  (cp_cmdverb(cmd,"CONNECT"))  {
			mn4_connect( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"COPY"))  {
			mn1_copy( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"CORR"))  {
			mn2_corr( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"CORRL"))  {
			mn2_corrl( cmd, status );
		} else if  (cp_cmdverb(cmd,"CREATE"))  {
			mn1_create( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"CURVE"))  {
			mn3_curve( cmd, status );
		} else if  (cp_cmdverb(cmd,"CUT"))  {
			mn3_cut( cmd, status );
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'D':
		if  (cp_cmdverb(cmd,"DECIMATE"))  {
			mn5_decimate( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"DEFAULT"))  {
			mn0_default( cmd, status );
		} else if  (cp_cmdverb(cmd,"DEL"))  {
			mn1_del( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"DEMEAN"))  {
			mn4_demean( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"DERIVE"))  {
			mn4_derive( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"DESPIKE"))  {
			mn3_despike( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"DISPLAY"))  {
			mn2_display( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"DTW"))  {
			dm_timewdw( 0.0, 0.0, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"DYW"))  {
			dm_ywdw( 0.0, 0.0, status );
			*redraw = TRUE;
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'E':
		if  (cp_cmdverb(cmd,"ECHO_CH"))  {
			mn0_ecch( cmd, status );
		} else if  (cp_cmdverb(cmd,"ECHO"))  {
			mn0_echo( cmd, status );
		} else if  (cp_cmdverb(cmd,"ENTER"))  {
			mn0_enter( cmd, status );
		} else if  (cp_cmdverb(cmd,"ENTRY"))  {
			mn1_entry( cmd, status );
		} else if  (cp_cmdverb(cmd,"EXEC"))  {
			if  (cp_pnexc(cmd,1,status))  return;
			cp_getstr( cmd, 1, tc, "   cmd: ", BC_LINELTH, execstr, status );
		} else if  (cp_cmdverb(cmd,"EXTERNAL_ROUTINE"))  {
			mn5_external_routine( cmd, status );
		} else if  (cp_cmdverb(cmd,"EXTRACT"))  {
			mn2_extract( cmd, status );
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'F':
		if  (cp_cmdverb(cmd,"FCT"))  {
			mn0_fct( cmd, status );
		} else if  (cp_cmdverb(cmd,"FFT"))  {
			mn4_fft( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"FILI"))  {
			mn2_fili( cmd, status );
		} else if  (cp_cmdverb(cmd,"FILTER"))  {
			mn2_filter( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"FINDGAP"))  {
			mn5_findgap( cmd, status );
		} else if  (cp_cmdverb(cmd,"FIXGAP"))  {
			mn5_fixgap( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"FIT"))  {
			mn4_fit( cmd, status );
		} else if  (cp_cmdverb(cmd,"FOLD"))  {
			mn3_fold( cmd, status );
			*redraw = TRUE;
#		ifdef BC_GRFVAX
		} else if  (cp_cmdverb(cmd,"FOLDN"))  {
			mngrf_fold( cmd, status );
			*redraw = TRUE;
#		endif /* BC_GRFVAX */
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'G':
		if  (cp_cmdverb(cmd,"GOTO"))  {
			mn0_goto( cmd, status );
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'H':
		if  (cp_cmdverb(cmd,"HC"))  {
			mnx_hc( cmd, status );
		} else if  (cp_cmdverb(cmd,"HELP"))  {
			mn0_help( cmd, status );
		} else if  (cp_cmdverb(cmd,"HIDE"))  {
			mn2_hide( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"HOTKEY"))  {
			mn0_hotkey( cmd, status );
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'I':
		if  (cp_cmdverb(cmd,"IF"))  {
			mn0_if( cmd, status );
		} else if  (cp_cmdverb(cmd,"INT"))  {
			mn4_int( cmd, status );
			*redraw = TRUE;
#		ifdef XXX
		} else if  (cp_cmdverb(cmd,"INVERSE"))  {
			mngrf_inverse( cmd, status );
			*redraw = TRUE;
#		endif /* XXX */
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'J':
	case 'K':
	case 'L':
		if  (cp_cmdverb(cmd,"LEVELDETEC"))  {
			mn4_leveldetec( cmd, status );
		} else if  (cp_cmdverb(cmd,"LOCATE"))  {
			mnx_locate( cmd, status );
		} else if  (cp_cmdverb(cmd,"LOG"))  {
			mn5_log( cmd, status );
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'M':
		if  (cp_cmdverb(cmd,"MARK"))  {
			mn2_mark( cmd, status );
		} else if  (cp_cmdverb(cmd,"MAXAMPL"))  {
			mn1_maxampl( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"MAXIMUM"))  {
			mn5_maximum( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"MDIR"))  {
			mn2_mdir( cmd, status );
		} else if  (cp_cmdverb(cmd,"MEAN"))  {
			mn5_mean( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"MEND"))  {
			mn3_mend( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"MERGE"))  {
			mn4_merge( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"MERGE_PAIR"))  {
			mn4_merge_pair( cmd, status );
			*redraw = FALSE;
		} else if  (cp_cmdverb(cmd,"MIRROR"))  {
			mn3_mirror( cmd, status );
			*redraw = TRUE;
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'N':
		if  (cp_cmdverb(cmd,"NORM"))  {
			mn1_norm( cmd, redraw, status );
		} else if  (cp_cmdverb(cmd,"NOP"))  {
		} else if  (cp_cmdverb(cmd,"NR"))  {
			(*rdlevel)++;
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'O':
		if  (cp_cmdverb(cmd,"OVERLAY"))  {
			mn3_overlay( cmd, status );
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'P':
		if  (cp_cmdverb(cmd,"PICK"))  {
			mn2_pick( cmd, status );
		} else if  (cp_cmdverb(cmd,"PM"))  {
			mnx_pm( cmd, status );
		} else if  (cp_cmdverb(cmd,"PMCH"))  {
			mnx_pmch( cmd, status );
		} else if  (cp_cmdverb(cmd,"POLFIL"))  {
			mn4_polfil( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"PROMPT"))  {
			mn5_prompt( cmd, BC_LINELTH, prompt, status );
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'Q':
		if  (cp_cmdverb(cmd,"QUIT"))  {
			mn0_quit( cmd, quit, status );
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'R':
		if  (cp_cmdverb(cmd,"RD"))  {
			mn1_rd( cmd, rdlevel, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"READ"))  {
			mni_read( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"READA"))  {
			mni_reada( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"READF"))  {
			mni_readf( cmd, status );
			*redraw = TRUE;
#ifdef XXX
		} else if  (cp_cmdverb(cmd,"READG"))  {
			mn5_readg( cmd, status );
			*redraw = TRUE;
#endif
#		ifdef SH_SETUP_READK
		} else if  (cp_cmdverb(cmd,"READK"))  {
			mngrf_readk( cmd, status );
			*redraw = TRUE;
#		endif /* SH_SETUP_READK */
		} else if  (cp_cmdverb(cmd,"READS"))  {
			mni_reads( cmd, status );
			*redraw = TRUE;
#		ifdef BC_GRFVAX
		} else if  (cp_cmdverb(cmd,"READV"))  {
			mngrf_readv( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"READO"))  {
			mngrf_reado( cmd, status );
			*redraw = TRUE;
#		endif /* BC_GRFVAX */
		} else if  (cp_cmdverb(cmd,"REPLACE"))  {
			mn5_replace( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"RESAMPLE"))  {
			mnx_resample( cmd, status );
		} else if  (cp_cmdverb(cmd,"RETURN"))  {
			mn0_cmdreturn( status );
		} else if  (cp_cmdverb(cmd,"RMS"))  {
			mnx_rms( cmd, status );
		} else if  (cp_cmdverb(cmd,"ROT"))  {
			mn1_rot( cmd, status );
			*redraw = TRUE;
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'S':
		if  (cp_cmdverb(cmd,"SAMPLE"))  {
			mn5_sample( cmd, status );
		} else if  (cp_cmdverb(cmd,"SDEF"))  {
			mn0_sdef( cmd, status );
		} else if  (cp_cmdverb(cmd,"SDEL"))  {
			mn0_sdel( cmd, status );
		} else if  (cp_cmdverb(cmd,"SET"))  {
			mn1_set( cmd, status );
		} else if  (cp_cmdverb(cmd,"SHIFT"))  {
			mnx_shift( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"SHMSETUP"))  {
			mn5_shmsetup( cmd, status );
		} else if  (cp_cmdverb(cmd,"SMOOTH"))  {
			mn5_smooth( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"SPECDIV"))  {
			mn5_specdiv( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"SPECTRUM"))  {
			mn4_spectrum( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"SPECTROGRAM"))  {
			mn4_spectrogram( cmd, status );
			*redraw = FALSE;
		} else if  (cp_cmdverb(cmd,"SPIKING"))  {
			mn3_spikefil( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"STABILITY"))  {
			mn4_stability( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"STALTA"))  {
			mn4_stalta( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"STW"))  {
			mn1_stw( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"STYW"))  {
			mnx_styw( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"SYW"))  {
			mnx_syw( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"SUM"))  {
			mn1_sum( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"SWITCH"))  {
			mn0_switch( cmd, &shflags_shv, status );
		} else if  (cp_cmdverb(cmd,"SYSTEM"))  {
			mn0_system( cmd, status );
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'T':
		if  (cp_cmdverb(cmd,"TIME"))  {
			mn2_time( cmd, status );
		} else if  (cp_cmdverb(cmd,"TITLE"))  {
			mnx_title( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"TRCFCT"))  {
			mn2_trcfct( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"TRCTXT"))  {
			dm_inftext( cmdlin, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"TRCTXTP"))  {
			mn1_trctxtp( cmd, status );
			*redraw = TRUE;
		} else if  (cp_cmdverb(cmd,"TREND"))  {
			mn4_trend( cmd, status );
			*redraw = TRUE;
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'U':
		if  (cp_cmdverb(cmd,"UNIT"))  {
			mn3_unit( cmd, status );
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'V':
	case 'W':
		if  (cp_cmdverb(cmd,"WDW"))  {
			mn0_wdw( cmd, status );
		} else if  (cp_cmdverb(cmd,"WRITE"))  {
			mni_write( cmd, status );
		} else if  (cp_cmdverb(cmd,"WRITEA"))  {
			mni_writea( cmd, status );
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'X':
	case 'Y':
		if  (cp_cmdverb(cmd,"YINFO"))  {
			mnx_yinfo( cmd, status );
			*redraw = TRUE;
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case 'Z':
		if  (cp_cmdverb(cmd,"ZOOM"))  {
			mn1_zoom( cmd, status );
			*redraw = TRUE;
		} else {
			mn0_callproc( cmd, status );
			*iscmdproc = TRUE;
		} /*endif*/
		break;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	case '/': case '\\':
		mn0_callproc( cmd, status );
		*iscmdproc = TRUE;
		break;
	default:
		if  (cmd->p[0][0] >= 'a' && cmd->p[0][0] <= 'z')
			*status = SHE_LOWRCASE;
		break;
	} /*endswitch*/

} /* end of se_execute_command */



/*------------------------------------------------------------------------*/



void se_cmdabort( PARAM *par, char cmdline[], STATUS status )

/* creates error file, aborts command procedure and returns to level 0
 *
 * parameters of routine
 * PARAM    *par;           input; parameter block
 * char     cmdline[];      input; user input
 * STATUS   status;         input; error number
 */
{
	/* local variables */
	static int  errnum;              /* error number */
	char     msg[BC_LINELTH+1];      /* error message */
	char     context[BC_LINELTH+1];  /* error context */
	char     fname[BC_LINELTH+1];    /* file name */
	FILE     *err;                   /* file pointers */
	int      *line;                  /* line in command procedure */
	int      i;                      /* counter */
	int      locstat;                /* local status */

	/* executable code */

	/* create error file */

	locstat = 0;
	strcpy( fname, shd_scratch );
	strcat( fname, id_shv );
	strcat( fname, "ERR" );
	strcat( fname, SHC_DE_TEXT );
	strcpy( msg, ">>> status report in error file " );
	strcat( msg, fname );
	strcat( msg, " <<<\n" );
	gc_write( cc, msg );
	err = sy_fopen( fname, "w" );
	if  (err == NULL)  return;

	err_msg( status, msg );
	err_getcontext( context );
	fprintf( err, "STATUS REPORT FILE\n" );
	fprintf( err, "==================\n\n" );
	fprintf( err, "session ID %s, error number %d\n", id_shv, ++errnum );
	fprintf( err, "status code %d\n", status );
	fprintf( err, "error message:\n" );
	fprintf( err, "%s\n", msg );
	if  (*context != '\0')  {
		fprintf( err, "error context:\n" );
		fprintf( err, "%s\n", context );
	} /*endif*/
	fprintf( err, "in source line\n%s\n", cmdline );
	fprintf( err, "source line after translation\n" );
	cp_verify( par, BC_LINELTH, msg, &locstat );
	if  (locstat != SHE_NOERROR)  {
		fprintf( err, ">>>  verify error  <<<\n\n" );
	} else {
		fprintf( err, "%s\n\n", msg );
	} /*endif*/

	fprintf( err, "traceback command levels (current level %d):\n",
		ui_level() );
	line = ui_lines();
	for  (i=0;i<=ui_level();i++)
		fprintf( err, "line %3d   in level %d (file %s)\n",
			*line++, i, ui_levelname(i) );

	fprintf( err, "\nflag status $%04x (global: $%04x)\n",
		shflags_shv, shglbflags_shv );
	fprintf( err, "---------------------------------\n" );
	if  (shflags_shv & SHF_LOGCMD)      fprintf( err, "PROTOCOL\n" );
	if  (shflags_shv & SHF_ECHO)        fprintf( err, "ECHO\n" );
	if  (shflags_shv & SHF_CAPCNV)      fprintf( err, "CAPCNV\n" );
	if  (shflags_shv & SHF_STEP)        fprintf( err, "STEP\n" );
	if  (shflags_shv & SHF_VERIFY)      fprintf( err, "VERIFY\n" );
	if  (shflags_shv & SHF_CMDERRSTOP)  fprintf( err, "CMDERRSTOP\n" );
	if  (shflags_shv & SHF_SHERRSTOP)   fprintf( err, "SHERRSTOP\n" );
	if  (shflags_shv & SHF_NOERRMSG)    fprintf( err, "NOERRMSG\n" );
	if  (shflags_shv & SHF_CHATTY)      fprintf( err, "CHATTY\n" );
	if  (shflags_shv & SHF_STARTUP)     fprintf( err, "STARTUP\n" );

	fprintf( err, "\n" );
	ss_dump( err, -1 );
	fprintf( err, "\n" );
	ss_dump( err, SHC_SYMLOC );
	fprintf( err, "\n" );
	ss_dump( err, SHC_SYMGLB );
	fprintf( err, "\n" );

	fclose( err );

	/* abort command procedure & return to interactive level */
	locstat = SHE_NOERROR;
	while (ui_level() > 0)
		mn0_cmdreturn( &locstat );

} /* end of se_cmdabort */



/*------------------------------------------------------------------------*/



void se_check_qual( CHMAP ch, PARAM *par )

/* prints unchecked qualifiers
 *
 * parameters of routine
 * CHMAP     ch;      input; output channel(s)
 * PARAM     *par;    input; parameter block
 */
{
	/* executable code */

	printf( "%c", (char)7 );
	gc_write( ch, "*** unrecognized qualifier \"" );
	gc_write( ch, cp_uncheckedqual(par) );
	gc_write( ch, "\" ***\n" );

} /* end of se_check_qual */



/*------------------------------------------------------------------------*/



void se_do_step( char stepcmd, STATUS *status )

/* processes step command entered by user
 *
 * parameters of routine
 * char       stepcmd;     input; step command
 * STATUS     *status;     output; return status
 */
{
	/* local variables */
	PARAM    scmd;     /* to call TT */

	/* executable code */

	switch  (stepcmd)  {
	case '@':
		cp_parse( "TT", &scmd, status );
		if  (Severe(status))  return;
		mn0_callproc( &scmd, status );
		break;
	} /*endswitch*/

} /* end of se_do_step */



/*------------------------------------------------------------------------*/



void se_dsplymsg( wdw, status )

/* displays error message */

/* parameters of routine */
int      wdw;            /* window number */
int      status;         /* input; error number */

{
	/* local variables */
	char     msg[BC_LINELTH+1];    /* error message */
	char     str[BC_LONGSTRLTH+1];

	/* executable code */
	err_msg( status, msg );
	printf( "%c", (char)7 );
	if  (wdw == 0)  {
		sy_alert( msg );
	} else {
		gc_write( wdw, msg );
		gc_wrtch( wdw, '\n' );
	} /*endif*/

	err_getcontext( msg );
	if  (*msg != '\0')  {
		if  (wdw == 0)  {
			sprintf( str, ">>> context: %s", msg );
			sy_alert( str );
		} else {
			gc_write( wdw, ">>> context: " );
			gc_write( wdw, msg );
			gc_wrtch( wdw, '\n' );
		} /*endif*/
	} /*endif*/

} /* end of se_dsplymsg */



/*------------------------------------------------------------------------*/



void se_readshpaths( char file[], BOOLEAN *ok )

/* reads SH paths from file
 *
 * parameters of routine
 * char       file[];      input; name of input file
 * BOOLEAN    *ok;         output; file found and read
 */
{
	/* local variables */
	FILE     *pf;                 /* file pointer */
	char     line[BC_LINELTH+1];  /* current line */
	char     item[BC_LINELTH+1];  /* item name */
	char     dir[BC_LINELTH+1];   /* directory string */

	/* executable code */

	*ok = TRUE;
	pf = sy_fopen( file, "r" );
	if  (pf == NULL)  {
		*ok = FALSE;
		return;
	} /*endif*/
	while  (fgets(line,BC_LINELTH,pf) != NULL)  {
		if  (*line != '!')  {
			sscanf( line, "%s %s", item, dir );
			if  (strcmp(item,"SCRATCH:") == 0)  {
				strcpy( shd_scratch, dir );
			} else if (strcmp(item,"INPUTS:") == 0)  {
				strcpy( shd_inputs, dir );
			} else {
				printf( "*** undefined SH path: %s ***\n", item );
			} /*endif*/
		} /*endif*/
	} /*endif*/
	sy_fclose( pf );

} /* end of se_readshpaths */



/*------------------------------------------------------------------------*/


#ifdef BC_SUN


void se_get_sh_environment( void )

/* reads environment variables and copies directories to shd_... variables
 *
 * no parameters
 */
{
	/* local variables */
	char     *eptr;     /* pointer to translated value */

	/* executable code */

	eptr = (char *)getenv( "SH_SCRATCH" );
	if  (eptr != NULL)
		if  (strlen(eptr) < BC_FILELTH)
			strcpy( shd_scratch, eptr );
	eptr = (char *)getenv( "SH_INPUTS" );
	if  (eptr != NULL)
		if  (strlen(eptr) < BC_FILELTH)
			strcpy( shd_inputs, eptr );

} /* end of se_get_sh_environment */


#endif



/*------------------------------------------------------------------------*/

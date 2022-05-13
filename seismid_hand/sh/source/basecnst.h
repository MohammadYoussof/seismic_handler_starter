/* file BASECNST.H
 *      ==========
 *
 * version 47, 1-May-2007
 *
 * v 36: 28-Nov-94, K. Stammler, new naming conventions
 *
 * basic constants for all modules
 * K. Stammler, 22-MAY-91
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1996,  Klaus Stammler, Federal Institute for Geosciences
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


#ifndef __BASECNST
#define __BASECNST


/* some constants */

/* old style names */
#define BC_LINELTH 199
#define BC_SHORTSTRLTH 30
#define BC_LONGSTRLTH 2047
#define BC_VERYLONGSTRLTH 8191
#define BC_TIMELTH 30
#define BC_FILELTH 179
#define BC_NOERROR 0
#define BC_PI 3.14159265358979323846
/* new style names */
#define cBcLineLth BC_LINELTH
#define cBcShortStrLth BC_SHORTSTRLTH
#define cBcLongStrLth BC_LONGSTRLTH
#define cBcVeryLongStrLth BC_VERYLONGSTRLTH
#define cBcTimeLth BC_TIMELTH
#define cBcFileLth BC_FILELTH
#define cBcNoError BC_NOERROR
#define cBcPi BC_PI


/******************************************************************
 ***                  computer type and graphics                ***
 ******************************************************************/

/* possible flags for operating system are
 * SH_SETUP_SUNOS
 * SH_SETUP_LINUX
 * SH_SETUP_AIX
 * SH_SETUP_HPUX
 * SH_SETUP_ATARITOS
 * SH_SETUP_VMS
 * default is SH_SETUP_SUNOS
 */

#if defined(SH_SETUP_SUNOS)
#define cBc_OS_UNIX
#define cBc_OS_SUNOS
#elif defined(SH_SETUP_LINUX)
#define cBc_OS_UNIX
#define cBc_OS_LINUX
#elif defined(SH_SETUP_AIX)
#define cBc_OS_UNIX
#define cBc_OS_AIX
#elif defined(SH_SETUP_HPUX)
#define cBc_OS_UNIX
#define cBc_OS_HPUX
#elif defined(SH_SETUP_VMS)
#define cBc_OS_VMS
#define BC_VAX            /* for compatibility */
#elif defined(SH_SETUP_ATARITOS)
#define cBc_OS_ATARITOS
#define BC_ATARI          /* for compatibility */
#else
#define cBc_OS_UNIX
#define cBc_OS_SUNOS
#endif

/* for compatibility (should be removed some time) */
#ifdef cBc_OS_UNIX
#define BC_SUN
#define cBc_SUN
#endif



/* select graphics */
/* #define BC_G_GEM */
#define BC_G_MEM
#define cBc_G_MEM
/* #define BC_G_BGI */
#define BC_G_POSTSCRIPT
#define cBc_G_POSTSCRIPT
/* #define BC_G_DESKJET */
#define BC_G_XWINDOW
#define cBc_G_XWINDOW
/* #define BC_G_CALCOMP */
#define BC_G_HPGL
#define cBc_G_HPGL
/* #define BC_G_VWS */
/* #define BC_G_TEK */

/* general flags */
/* #define BC_MAINARGS */
/* #define cBc_MAINARGS */
#define BC_STDLIB_EX   /* should not be used any more */

/* for some historical reasons ... */
#define BC_INC_STDLIB <stdlib.h>
#define cBc_INC_STDLIB BC_INC_STDLIB
#define BC_INC_UNISTD <unistd.h>
#define cBc_INC_UNISTD BC_INC_UNISTD

/* compile AH interface ? */
#if  defined(SH_SETUP_AH)
#define BC_FRGN_AH
#define cBc_FRGN_AH
#endif

/* special settings */
/* #define BC_GRFVAX */
	/* special GRF version, only valid on VAX/VMS version */
/* #define BC_USE_READK */
	/* enables readk command, only valid on Sun/UNIX version at SZGRF */
#define BC_REVERSECROSS
#define cBc_REVERSECROSS
	/* crosshair cursor colours reversed (background and foreground) */
#define BC_DEFINE_TRUE_FALSE
#define cBc_DEFINE_TRUE_FALSE
	/* define TRUE and FALSE */
/* #define BC_CLOCK_T_AVAILABLE */
	/* type clock_t is available */




/******************************************************************
 ***                        ATARI version                       ***
 ******************************************************************/



#ifdef BC_ATARI

#define BC_KS_PRIVATE
	/* K.S. private ATARI TT030/8 MultiGEM version */

/* #define BC_SHARE_CPU */
	/* share CPU time by calling AES routine evnt_timer */
	/* only in combination with BC_KS_PRIVATE ! */
	/* only together with DEBUG_SHARE_CPU in module syscall.c, */
   /* because also used in module gemevent.c */

/* include files */
#define BC_SYSBASE "E:\PC\sh\source\SYSBASE.H"
#define BC_SYERRORS "E:\PC\sh\source\SYERRORS.H"
#define BC_GCUSRDEF "E:\PC\sh\source\newgraph\GCUSRDEF.H"
#define BC_FOREIGN "E:\PC\sh\source\SHFRGN.H"
#define BC_SHDIRS "E:\PC\sh\source\SHDIRS.H"
#define BC_ASCII "E:\PC\sh\source\ASCII.H"
#define BC_GRAPHBAS "E:\PC\sh\source\newgraph\GRAPHBAS.H"
#define BC_SHCONST "E:\PC\sh\source\SHCONST.H"
#define BC_FOUSRDEF "e:\PC\sh\source\fousrdef.h"
#define BC_QFUSRDEF "e:\PC\sh\source\qfusrdef.h"
#define BC_QFERRORS "e:\PC\sh\source\qferrors.h"
#define BC_CPAR "e:\PC\util\cpar.h"
#define BC_EARTHLOC "e:\PC\sh\source\earthloc.h"
#define BC_UTUSRDEF "e:\PC\sh\source\utusrdef.h"
#define BC_GLUSRDEF "e:\PC\sh\source\glusrdef.h"
#define BC_TCUSRDEF "e:\pc\sh\source\tcusrdef.h"
#define BC_INPFILES "e:\PC\util\inpfiles.h"
#define BC_DFQENTRY "e:\PC\sh\source\dfqentry.h"
#define BC_ERUSRDEF "e:\PC\sh\source\erusrdef.h"
#define BC_FLERRORS "e:\PC\sh\source\flerrors.h"
#define BC_READGRN "e:\PC\sh\source\readgrn.h"
#define BC_PTUSRDEF "e:\PC\sh\source\ptusrdef.h"


#endif /* BC_ATARI */



/******************************************************************
 ***                        IBM PC version                      ***
 ******************************************************************/



#ifdef BC_IBM

/* include files */
#define BC_SYSBASE "d:\gast\SH\SYSBASE.H"
#define BC_SYERRORS "d:\gast\SH\SYERRORS.H"
#define BC_GCUSRDEF "d:\gast\SH\LOCAL\GCUSRDEF.H"
/* #define BC_GCERRORS "LOCAL\GCERRORS.H" */
#define BC_FOREIGN "d:\gast\SH\SHFRGN.H"
#define BC_SHDIRS "d:\gast\SH\SHDIRS.H"
#define BC_ASCII "d:\gast\SH\ASCII.H"
#define BC_GRAPHBAS "d:\gast\SH\LOCAL\GRAPHBAS.H"
#define BC_SHCONST "d:\gast\SH\SHCONST.H"
#define BC_QFUSRDEF "d:\gast\sh\qfusrdef.h"
#define BC_QFERRORS "d:\gast\sh\qferrors.h"
#define BC_CPAR "d:\gast\util\cpar.h"
#define BC_EARTHLOC "d:\gast\sh\earthloc.h"
#define BC_UTUSRDEF "d:\gast\sh\utusrdef.h"
#define BC_GLUSRDEF "d:\gast\sh\glusrdef.h"
#define BC_INPFILES "d:\gast\util\inpfiles.h"
#define BC_DFQENTRY "d:\gast\sh\dfqentry.h"
#define BC_ERUSRDEF "d:\gast\sh\erusrdef.h"
#define BC_FLERRORS "d:\gast\sh\flerrors.h"
#define BC_READGRN "d:\gast\sh\readgrn.h"
#define BC_PTUSRDEF "d:\gast\sh\ptusrdef.h"


#endif /* BC_IBM */



/******************************************************************
 ***                         VAX version                        ***
 ******************************************************************/



#ifdef BC_VAX


#define BC_SYSBASE "SHC_MAIN:SYSBASE.H"
#define BC_SYERRORS "SHC_MAIN:SYERRORS.H"
#define BC_GCUSRDEF "SHC_newgraph:GCUSRDEF.H"
#define BC_FOREIGN "SHC_FOREIGN:SHFRGN.H"
#define BC_SHDIRS "SHC_MAIN:SHDIRS.H"
#define BC_ASCII "SHC_MAIN:ASCII.H"
#define BC_GRAPHBAS "shc_newgraph:GRAPHBAS.H"
#define BC_SHCONST "SHC_MAIN:SHCONST.H"
#define BC_CPAR "shc_main:cpar.h"
#define BC_EARTHLOC "shc_main:earthloc.h"
#define BC_UTUSRDEF "shc_main:utusrdef.h"
#define BC_GLUSRDEF "shc_main:glusrdef.h"
#define BC_INPFILES "shc_utilsrc:inpfiles.h"
#define BC_TCUSRDEF "shc_main:tcusrdef.h"
#define BC_QFUSRDEF "shc_main:qfusrdef.h"
#define BC_QFERRORS "shc_main:qferrors.h"
#define BC_DFQENTRY "shc_main:dfqentry.h"
#define BC_ERUSRDEF "shc_main:erusrdef.h"
#define BC_FLERRORS "shc_main:flerrors.h"
#define BC_READGRN "shc_main:readgrn.h"
#define BC_PTUSRDEF "shc_main:ptusrdef.h"


#endif /* BC_VAX */


/******************************************************************
 ***                         SUN version                        ***
 ******************************************************************/


#ifdef BC_SUN


/* only old style names */
#define BC_SYSBASE "sysbase.h"
#define BC_SYERRORS "syerrors.h"
#define BC_GCUSRDEF "gcusrdef.h"
#define BC_GCERRORS "gcerrors.h"
#define BC_SHDIRS "shdirs.h"
#define BC_ASCII "ascii.h"
#define BC_GRAPHBAS "graphbas.h"
#define BC_SHCONST "shconst.h"
#define BC_QFUSRDEF "qfusrdef.h"
#define BC_QFERRORS "qferrors.h"
#define BC_CPAR "cpar.h"
#define BC_EARTHLOC "earthloc.h"
#define BC_UTUSRDEF "utusrdef.h"
#define BC_GLUSRDEF "glusrdef.h"
#define BC_INPFILES "inpfiles.h"
#define BC_TCUSRDEF "tcusrdef.h"
#define BC_DFQENTRY "dfqentry.h"
#define BC_FOUSRDEF "fousrdef.h"
#define BC_ERUSRDEF "erusrdef.h"
#define BC_FLERRORS "flerrors.h"
#define BC_READGRN "readgrn.h"
#define BC_PTUSRDEF "ptusrdef.h"

/* if no foreign formats are used: */
/* #define BC_FOREIGN "shfrgn.h" */
/* otherwise: */
#define BC_FOREIGN "shfrgn.h"
#define cBc_FOREIGN BC_FOREIGN



#endif /* BC_SUN */


/******************************************************************
 ***                              END                           ***
 ******************************************************************/


#endif /* __BASECNST */



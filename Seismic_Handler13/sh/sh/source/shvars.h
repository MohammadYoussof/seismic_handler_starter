
/* file SHVARS.H
 *      ========
 *
 * version 9, 9-Nov-2006
 *
 * global variables of seismhandler program
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


#ifndef __SHVARS
#define __SHVARS

#ifndef __SHCONST
#include "shconst.h"
#endif

extern int      tc;                  /* local text channels */
extern int      tc_shv;              /* global text channels */
extern int      gc;                  /* local graphic channels */
extern int      gc_shv;              /* global graphic channels */
extern int      cc;                  /* local console channels */
extern int      cc_shv;              /* global console channels */
extern SHFLAGS  shflags_shv;         /* local processing flags */
extern SHFLAGS  shglbflags_shv;      /* global processing flags */
extern STATUS   shv_last_status;     /* status of last command */
extern int      shv_maininput;       /* main input level for CP's */

extern char     id_shv[11];                  /* session ID */
extern char     protfile_shv[SHC_FILELTH+1]; /* protocol file */

/* directories */
extern char     shd_scratch[SHC_FILELTH+1];  /* scratch directory */
extern char     shd_help[SHC_FILELTH+1];     /* help directory */
extern char     shd_cmd[SHC_FILELTH+1];      /* command directory */
extern char     shd_cmd2[SHC_FILELTH+1];     /* 2nd command directory */
extern char     shd_filter[SHC_FILELTH+1];   /* filter directory */
extern char     shd_globals[SHC_FILELTH+1];  /* globals (%-files) */
extern char     shd_errors[SHC_FILELTH+1];   /* error files */
extern char     shd_inputs[SHC_FILELTH+1];   /* inputs to SH */
/*extern char     shd_extprog[SHC_FILELTH+1];*/  /* external programs */
extern char     shd_userdir[SHC_FILELTH+1];  /* private user directory */

#endif /* __SHVARS */

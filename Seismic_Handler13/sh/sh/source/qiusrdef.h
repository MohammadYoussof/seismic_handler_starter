
/* file QIUSRDEF.H
 *      ==========
 *
 * version 2, 22-MAY-2006
 *
 * prototypes of routines of module QFINTRFC.C
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


#ifndef __QIUSRDEF
#define __QIUSRDEF

#define QIC_EMPTY 0
#define QIC_RSRVD -1

/*---------------------------------------------------------------------------*/


void qi_initcnv( void );

/* initialises the conversion array */


/*---------------------------------------------------------------------------*/


void qi_define( unsigned sh_entry, unsigned qf_entry, int *status );

/* defines new entry
 *
 * parameters of routine
 * unsigned   sh_entry;     input; SH entry number (with type)
 * unsigned   qf_entry;     input; q-file entry (without type)
 * int        *status;      output; return status
 */


/*---------------------------------------------------------------------------*/


int qi_cnvlidx( unsigned ent, int *status );

/* returns long q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */


/*---------------------------------------------------------------------------*/


int qi_cnviidx( unsigned ent, int *status );

/* returns int q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */


/*---------------------------------------------------------------------------*/


int qi_cnvbidx( unsigned ent, int *status );

/* returns byte q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */


/*---------------------------------------------------------------------------*/


int qi_cnvridx( unsigned ent, int *status );

/* returns real q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */


/*---------------------------------------------------------------------------*/


int qi_cnvsidx( unsigned ent, int *status );

/* returns string q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */


/*---------------------------------------------------------------------------*/


int qi_cnvcidx( unsigned ent, int *status );

/* returns char q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */


/*---------------------------------------------------------------------------*/


int qi_cnvtidx( unsigned ent, int *status );

/* returns time q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */


/*---------------------------------------------------------------------------*/


int qi_cnvfidx( unsigned ent, int *status );

/* returns flag q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */


/*---------------------------------------------------------------------------*/

#endif /* __QIUSRDEF */

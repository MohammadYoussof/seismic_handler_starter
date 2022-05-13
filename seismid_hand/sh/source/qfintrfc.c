
/* file QFINTRFC.C
 *      ==========
 *
 * version 4, 22-May-2006
 *
 * seismhandler interface to q-file I/O
 * K. Stammler, 3-JUN-90
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
#include "basecnst.h"
#include BC_SYSBASE
#include "shconst.h"
#include "qfusrdef.h"
#include "qiusrdef.h"
#include "tcusrdef.h"
#include "infoidx.h"
#include "qierrors.h"


static int lcnv_qiv[QFC_LENTRY];     /* long index conversion */
static int icnv_qiv[QFC_IENTRY];     /* int index conversion */
static int bcnv_qiv[QFC_IENTRY];     /* byte index conversion */
static int rcnv_qiv[QFC_RENTRY];     /* real index conversion */
static int scnv_qiv[QFC_SENTRY];     /* string index conversion */
static int ccnv_qiv[QFC_CENTRY];     /* char index conversion */
static int tcnv_qiv[QFC_SENTRY];     /* time index conversion */
static int fcnv_qiv[QFC_CENTRY];     /* flag index conversion */


/*---------------------------------------------------------------------------*/



void qi_initcnv( void )

/* initialises the conversion array */

{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	/* long info's */
	for  (i=0;i<ENF_LONG;i++)            lcnv_qiv[i] = QIC_EMPTY;
	for  (i=ENF_LONG;i<EMAX_LONG;i++)    lcnv_qiv[i] = QIC_RSRVD;
	for  (i=EMAX_LONG;i<QFC_LENTRY;i++)  lcnv_qiv[i] = QIC_EMPTY;
	lcnv_qiv[EL_LENGTH & E_IDXMASK] = 1|QFC_LTYPE; /* length entry */

	/* int info's */
	for  (i=0;i<ENF_INT;i++)             icnv_qiv[i] = QIC_EMPTY;
	for  (i=ENF_INT;i<EMAX_INT;i++)      icnv_qiv[i] = QIC_RSRVD;
	for  (i=EMAX_INT;i<QFC_IENTRY;i++)   icnv_qiv[i] = QIC_EMPTY;

	/* byte info's */
	for  (i=0;i<ENF_BYTE;i++)            bcnv_qiv[i] = QIC_EMPTY;
	for  (i=ENF_BYTE;i<EMAX_BYTE;i++)    bcnv_qiv[i] = QIC_RSRVD;
	for  (i=EMAX_BYTE;i<QFC_IENTRY;i++)  bcnv_qiv[i] = QIC_EMPTY;

	/* real info's */
	for  (i=0;i<ENF_REAL;i++)            rcnv_qiv[i] = QIC_EMPTY;
	for  (i=ENF_REAL;i<EMAX_REAL;i++)    rcnv_qiv[i] = QIC_RSRVD;
	for  (i=EMAX_REAL;i<QFC_RENTRY;i++)  rcnv_qiv[i] = QIC_EMPTY;
	rcnv_qiv[ER_DELTA & E_IDXMASK] = 0|QFC_RTYPE;

	/* string info's */
	for  (i=0;i<ENF_STR;i++)             scnv_qiv[i] = QIC_EMPTY;
	for  (i=ENF_STR;i<EMAX_STR;i++)      scnv_qiv[i] = QIC_RSRVD;
	for  (i=EMAX_STR;i<QFC_SENTRY;i++)   scnv_qiv[i] = QIC_EMPTY;
	scnv_qiv[ES_COMMENT & E_IDXMASK] = 0|QFC_STYPE;
	scnv_qiv[ES_STATION & E_IDXMASK] = 1|QFC_STYPE;
	scnv_qiv[ES_OPINFO & E_IDXMASK]  = 2|QFC_STYPE;

	/* char info's */
	for  (i=0;i<ENF_CHAR;i++)            ccnv_qiv[i] = QIC_EMPTY;
	for  (i=ENF_CHAR;i<EMAX_CHAR;i++)    ccnv_qiv[i] = QIC_RSRVD;
	for  (i=EMAX_CHAR;i<QFC_CENTRY;i++)  ccnv_qiv[i] = QIC_EMPTY;
	ccnv_qiv[EC_COMP & E_IDXMASK] = 0|QFC_CTYPE;
	ccnv_qiv[EC_CHAN1 & E_IDXMASK] = 1|QFC_CTYPE;
	ccnv_qiv[EC_CHAN2 & E_IDXMASK] = 2|QFC_CTYPE;

	/* time info's */
	for  (i=0;i<ENF_TIME;i++)            tcnv_qiv[i] = QIC_EMPTY;
	for  (i=ENF_TIME;i<EMAX_TIME;i++)    tcnv_qiv[i] = QIC_RSRVD;
	for  (i=EMAX_TIME;i<QFC_SENTRY;i++)  tcnv_qiv[i] = QIC_EMPTY;
	tcnv_qiv[ET_START & E_IDXMASK] = 21|QFC_STYPE;

	/* flag info's */
	for  (i=0;i<ENF_FLAG;i++)            fcnv_qiv[i] = QIC_EMPTY;
	for  (i=ENF_FLAG;i<EMAX_FLAG;i++)    fcnv_qiv[i] = QIC_RSRVD;
	for  (i=EMAX_FLAG;i<QFC_CENTRY;i++)  fcnv_qiv[i] = QIC_EMPTY;

} /* end of qi_initcnv */



/*---------------------------------------------------------------------------*/



void qi_define( unsigned sh_entry, unsigned qf_entry, int *status )

/* defines new entry
 *
 * parameters of routine
 * unsigned   sh_entry;     input; SH entry number (with type)
 * unsigned   qf_entry;     input; q-file entry (without type)
 * int        *status;      output; return status
 */
{
	/* local variables */
	int      type;     /* type of entry */

	/* executable code */

	type = (sh_entry & E_TYPMASK);
	sh_entry &= E_IDXMASK;

	switch  (type)  {
	case EL_TYPE:
		if  ((sh_entry >= QFC_LENTRY) || (qf_entry >= QFC_LENTRY))  {
			*status = QIE_ILENTRY;
			return;
		} /*endif*/
		if  (lcnv_qiv[sh_entry] != QIC_EMPTY)  {
			*status = QIE_NOTEMPTY;
			return;
		} /*endif*/
		lcnv_qiv[sh_entry] = (qf_entry|QFC_LTYPE);
		break;
	case EI_TYPE:
		if  ((sh_entry >= QFC_IENTRY) || (qf_entry >= QFC_IENTRY))  {
			*status = QIE_ILENTRY;
			return;
		} /*endif*/
		if  (icnv_qiv[sh_entry] != QIC_EMPTY)  {
			*status = QIE_NOTEMPTY;
			return;
		} /*endif*/
		icnv_qiv[sh_entry] = (qf_entry|QFC_ITYPE);
		break;
	case EB_TYPE:
		if  ((sh_entry >= QFC_IENTRY) || (qf_entry >= QFC_IENTRY))  {
			*status = QIE_ILENTRY;
			return;
		} /*endif*/
		if  (bcnv_qiv[sh_entry] != QIC_EMPTY)  {
			*status = QIE_NOTEMPTY;
			return;
		} /*endif*/
		bcnv_qiv[sh_entry] = (qf_entry|QFC_ITYPE);
		break;
	case ER_TYPE:
		if  ((sh_entry >= QFC_RENTRY) || (qf_entry >= QFC_RENTRY))  {
			*status = QIE_ILENTRY;
			return;
		} /*endif*/
		if  (rcnv_qiv[sh_entry] != QIC_EMPTY)  {
			*status = QIE_NOTEMPTY;
			return;
		} /*endif*/
		rcnv_qiv[sh_entry] = (qf_entry|QFC_RTYPE);
		break;
	case ES_TYPE:
		if  ((sh_entry >= QFC_SENTRY) || (qf_entry >= QFC_SENTRY))  {
			*status = QIE_ILENTRY;
			return;
		} /*endif*/
		if  (scnv_qiv[sh_entry] != QIC_EMPTY)  {
			*status = QIE_NOTEMPTY;
			return;
		} /*endif*/
		scnv_qiv[sh_entry] = (qf_entry|QFC_STYPE);
		break;
	case EC_TYPE:
		if  ((sh_entry >= QFC_CENTRY) || (qf_entry >= QFC_CENTRY))  {
			*status = QIE_ILENTRY;
			return;
		} /*endif*/
		if  (ccnv_qiv[sh_entry] != QIC_EMPTY)  {
			*status = QIE_NOTEMPTY;
			return;
		} /*endif*/
		ccnv_qiv[sh_entry] = (qf_entry|QFC_CTYPE);
		break;
	case ET_TYPE:
		if  ((sh_entry >= QFC_SENTRY) || (qf_entry >= QFC_SENTRY))  {
			*status = QIE_ILENTRY;
			return;
		} /*endif*/
		if  (tcnv_qiv[sh_entry] != QIC_EMPTY)  {
			*status = QIE_NOTEMPTY;
			return;
		} /*endif*/
		tcnv_qiv[sh_entry] = (qf_entry|QFC_STYPE);
		break;
	case EF_TYPE:
		if  ((sh_entry >= QFC_CENTRY) || (qf_entry >= QFC_CENTRY))  {
			*status = QIE_ILENTRY;
			return;
		} /*endif*/
		if  (fcnv_qiv[sh_entry] != QIC_EMPTY)  {
			*status = QIE_NOTEMPTY;
			return;
		} /*endif*/
		fcnv_qiv[sh_entry] = (qf_entry|QFC_CTYPE);
		break;
	default:
		*status = QIE_ILTYPE;
		return;
	} /*endswitch*/

} /* end of qi_define */



/*---------------------------------------------------------------------------*/



int qi_cnvlidx( unsigned ent, int *status )

/* returns long q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */
{
	/* executable code */

	if  ((ent & E_TYPMASK) != EL_TYPE)  {
		*status = QIE_TYPMSMCH;
		return -1;
	} /*endif*/
	ent &= E_IDXMASK;

	if  (ent >= QFC_LENTRY)  {
		*status = QIE_ILENTRY;
		return -1;
	} /*endif*/

	return lcnv_qiv[ent];

} /* end of qi_cnvlidx */



/*---------------------------------------------------------------------------*/



int qi_cnviidx( unsigned ent, int *status )

/* returns int q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */
{
	/* executable code */

	if  ((ent & E_TYPMASK) != EI_TYPE)  {
		*status = QIE_TYPMSMCH;
		return -1;
	} /*endif*/
	ent &= E_IDXMASK;

	if  (ent >= QFC_IENTRY)  {
		*status = QIE_ILENTRY;
		return -1;
	} /*endif*/

	return icnv_qiv[ent];

} /* end of qi_cnviidx */



/*---------------------------------------------------------------------------*/



int qi_cnvbidx( unsigned ent, int *status )

/* returns byte q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */
{
	/* executable code */

	if  ((ent & E_TYPMASK) != EB_TYPE)  {
		*status = QIE_TYPMSMCH;
		return -1;
	} /*endif*/
	ent &= E_IDXMASK;

	if  (ent >= QFC_IENTRY)  {
		*status = QIE_ILENTRY;
		return -1;
	} /*endif*/

	return bcnv_qiv[ent];

} /* end of qi_cnvbidx */



/*---------------------------------------------------------------------------*/



int qi_cnvridx( unsigned ent, int *status )

/* returns real q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */
{
	/* executable code */

	if  ((ent & E_TYPMASK) != ER_TYPE)  {
		*status = QIE_TYPMSMCH;
		return -1;
	} /*endif*/
	ent &= E_IDXMASK;

	if  (ent >= QFC_RENTRY)  {
		*status = QIE_ILENTRY;
		return -1;
	} /*endif*/

	return rcnv_qiv[ent];

} /* end of qi_cnvridx */



/*---------------------------------------------------------------------------*/



int qi_cnvsidx( unsigned ent, int *status )

/* returns string q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */
{
	/* executable code */

	if  ((ent & E_TYPMASK) != ES_TYPE)  {
		*status = QIE_TYPMSMCH;
		return -1;
	} /*endif*/
	ent &= E_IDXMASK;

	if  (ent >= QFC_SENTRY)  {
		*status = QIE_ILENTRY;
		return -1;
	} /*endif*/

	return scnv_qiv[ent];

} /* end of qi_cnvsidx */



/*---------------------------------------------------------------------------*/



int qi_cnvcidx( unsigned ent, int *status )

/* returns char q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */
{
	/* executable code */

	if  ((ent & E_TYPMASK) != EC_TYPE)  {
		*status = QIE_TYPMSMCH;
		return -1;
	} /*endif*/
	ent &= E_IDXMASK;

	if  (ent >= QFC_CENTRY)  {
		*status = QIE_ILENTRY;
		return -1;
	} /*endif*/

	return ccnv_qiv[ent];

} /* end of qi_cnvcidx */



/*---------------------------------------------------------------------------*/



int qi_cnvtidx( unsigned ent, int *status )

/* returns time q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */
{
	/* executable code */

	if  ((ent & E_TYPMASK) != ET_TYPE)  {
		*status = QIE_TYPMSMCH;
		return -1;
	} /*endif*/
	ent &= E_IDXMASK;

	if  (ent >= QFC_SENTRY)  {
		*status = QIE_ILENTRY;
		return -1;
	} /*endif*/

	return tcnv_qiv[ent];

} /* end of qi_cnvtidx */



/*---------------------------------------------------------------------------*/



int qi_cnvfidx( unsigned ent, int *status )

/* returns flag q-index
 *
 * parameters of routine
 * unsigned     ent;       input; sh entry number
 * int          *status;   output; return status
 */
{
	/* executable code */

	if  ((ent & E_TYPMASK) != EF_TYPE)  {
		*status = QIE_TYPMSMCH;
		return -1;
	} /*endif*/
	ent &= E_IDXMASK;

	if  (ent >= QFC_CENTRY)  {
		*status = QIE_ILENTRY;
		return -1;
	} /*endif*/

	return fcnv_qiv[ent];

} /* end of qi_cnvfidx */



/*---------------------------------------------------------------------------*/

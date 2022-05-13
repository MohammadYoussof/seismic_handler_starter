
/* file SHFRGN.C
 *      ========
 *
 * version 8, 9-Dec-94
 *
 * v 8, 9-Dec-94, K. Stammler: included GSE2.0 reader
 *
 * foreign format dispatcher
 * K. Stammler, 2-aug-91
 */


#include <stdio.h>
#include BASECNST
#include BC_SYSBASE
#include BC_SHCONST
#include "shfrgn.h"
#include "sh_neic.h"    /* format 1 */
#include "sh_gse.h"     /* format 2 */
#include "sh_ah.h"      /* format 3 */
#include "sh_len.h"     /* format 4 */
#include "sh_gse2.h"    /* format 5 */


/* global variables */
static int   frv_format;         /* current format */


/*----------------------------------------------------------------------*/


void sh_frgn_trace( int format, char file[], int rec, STATUS *status )

/* switches to new trace
 *
 * parameters of routine
 * int        format;      input; format ID
 * char       file[];      input; name of input file
 * int        rec;         input; record number
 * STATUS     *status;     output; return status
 */
{

	switch  (format)  {
	case 1:  sh_neic_trace( file, rec, status ); break;
	case 2:  sh_gse_trace( file, rec, status ); break;
	case 3:  sh_ah_trace( file, rec, status ); break;
	case 4:  sh_len_trace( file, rec, status ); break;
	case 5:  ShGs2Trace( file, rec, status ); break;
	default:
		*status = 8005;
		return;
	} /*endswitch*/

	frv_format = format;

} /* end of sh_frgn_trace */


/*----------------------------------------------------------------------*/


void sh_frgn_geti( char entryname[], long *info, STATUS *status )

/* returns long integer info entry
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * long       *info;           output; info returned
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	switch (frv_format)  {
	case 1:   sh_neic_geti( entryname, info, status ); break;
	case 2:   sh_gse_geti( entryname, info, status ); break;
	case 3:   sh_ah_geti( entryname, info, status ); break;
	case 4:   sh_len_geti( entryname, info, status ); break;
	case 5:   ShGs2GetI( entryname, info, status ); break;
	default:
		*status = 8005;
		return;
	} /*endswitch*/

} /* end of sh_frgn_geti */


/*----------------------------------------------------------------------*/


void sh_frgn_getr( char entryname[], REAL *info, STATUS *status )

/* returns float info entry
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * REAL       *info;           output; info returned
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	switch (frv_format)  {
	case 1:   sh_neic_getr( entryname, info, status ); break;
	case 2:   sh_gse_getr( entryname, info, status ); break;
	case 3:   sh_ah_getr( entryname, info, status ); break;
	case 4:   sh_len_getr( entryname, info, status ); break;
	case 5:   ShGs2GetR( entryname, info, status ); break;
	default:
		*status = 8005;
		return;
	} /*endswitch*/

} /* end of sh_frgn_getr */


/*----------------------------------------------------------------------*/


void sh_frgn_gets( char entryname[], int maxlth, char info[], STATUS *status )

/* returns long integer info entry
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * int        maxlth;          input; maximum length of output string
 * char       info[];          output; info returned
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	switch (frv_format)  {
	case 1:   sh_neic_gets( entryname, maxlth, info, status ); break;
	case 2:   sh_gse_gets( entryname, maxlth, info, status ); break;
	case 3:   sh_ah_gets( entryname, maxlth, info, status ); break;
	case 4:   sh_len_gets( entryname, maxlth, info, status ); break;
	case 5:   ShGs2GetS( entryname, maxlth, info, status ); break;
	default:
		*status = 8005;
		return;
	} /*endswitch*/

} /* end of sh_frgn_gets */



/*----------------------------------------------------------------------*/


void sh_frgn_getc( char entryname[], char *info, STATUS *status )

/* returns character info entry
 *
 * parameters of routine
 * char       entryname[];     input; name of info entry
 * char       *info;           output; info returned
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	switch (frv_format)  {
	case 1:   sh_neic_getc( entryname, info, status ); break;
	case 2:   sh_gse_getc( entryname, info, status ); break;
	case 3:   sh_ah_getc( entryname, info, status ); break;
	case 4:   sh_len_getc( entryname, info, status ); break;
	case 5:   ShGs2GetC( entryname, info, status ); break;
	default:
		*status = 8005;
		return;
	} /*endswitch*/

} /* end of sh_frgn_getc */



/*----------------------------------------------------------------------*/


void sh_frgn_read( SAMPLE smp[] )

/* returns sample data
 *
 * parameter of routine
 * SAMPLE    smp[];     output; sample data
 */
{
	/* executable code */

	switch (frv_format)  {
	case 1:   sh_neic_read( smp ); break;
	case 2:   sh_gse_read( smp ); break;
	case 3:   sh_ah_read( smp ); break;
	case 4:   sh_len_read( smp ); break;
	case 5:   ShGs2Read( smp ); break;
	default:  return;
	} /*endswitch*/

} /* end of sh_frgn_read */



/*----------------------------------------------------------------------*/

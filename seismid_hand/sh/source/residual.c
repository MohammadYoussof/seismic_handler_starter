
/* file residual.c
 *      ==========
 *
 * version 4, 22-May-2006
 *
 * Reading interpolated residual times from residual tables.
 * K. Stammler, 3-Feb-2001
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
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "erusrdef.h"
#include "utusrdef.h"
#include "residual.h"



/* global variables */
TRsTable    *vrs_roottable=NULL;   /* pointer to root table */


/* prototypes of local routines */
static void RsEnlistTable( TRsTable *tab );
static void RsDumpTable( TRsTable *tab );



/*----------------------------------------------------------------------------*/



void RsReadTables( char fname[], TSyStatus *status )

/* Read in residual tables from file
 *
 * parameters of routine
 * char       file[];       input; filename
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	char     locname[cBcFileLth+1]; /* local file name */
	char     *env;                /* pointer to environment variable */
	FILE     *fp;                 /* pointer to input file */
	char     line[cBcLineLth+1];  /* current line of file */
	char     tmpstr[cBcLineLth+1];/* scratch string */
	TRsTable *curtab;             /* pointer to current table */
	int      i;                   /* counter */

	/* executable code */

	/* generate filename if not given */
	if  (*fname == '\0' || strcmp(fname,"default") == 0)  {
		env = (char *)getenv( "SH_INPUTS" );
		if  ((strlen(env)+strlen(cRsDefaultName)) > cBcFileLth)  {
			*status = eRsLongFilename;
			return;
		} /*endif*/
		strcpy( locname, env );
		strcat( locname, cRsDefaultName );
	} else {
		if  (strlen(fname) > cBcFileLth)  {
			*status = eRsLongFilename;
			return;
		} /*endif*/
		strcpy( locname, fname );
	} /*endif*/

	/* open file */
	fp = fopen( locname, "r" );
	if  (fp == NULL)  {
		*status = eRsFileNotFound;
		return;
	} /*endif*/

	/* read through file */
	curtab = NULL;
	while  (fgets(line,cBcLineLth,fp) != NULL)  {

		/* ignore comment and empty lines */
		if  (*line == '!')  continue;
		if  (*line == '\n')  continue;

		/* create new table on station */
		if  (strncmp(line,"station:",8) == 0)  {
			if  (curtab != NULL)  {
				*status = eRsTableNotClosed;
				sy_deallocmem( curtab );
				return;
			} /*endif*/
			/* create new table */
			curtab = (TRsTable *)sy_allocmem( 1, sizeof(TRsTable), status );
			if  (SySevere(status))  return;
			RsInitTable( curtab );
			sscanf( line, "station: %s\n", tmpstr );
			if  (strlen(tmpstr) > cRsStationLth)  {
				*status = eRsLongName;
				return;
			} /*endif*/
			ut_cap( tmpstr );
			strcpy( curtab->station, tmpstr );
		} /*endif*/

		/* read phase list */
		if  (strncmp(line,"phases:",7) == 0)  {
			if  (curtab == NULL)  {
				*status = eRsSyntaxError;
				return;
			} /*endif*/
			sscanf( line, "phases: %s\n", tmpstr );
			if  (strlen(tmpstr) > cRsPhaseListLth-2)  {
				*status = eRsLongPhaseList;
				return;
			} /*endif*/
			curtab->phases[0] = ',';
			strcpy( curtab->phases+1, tmpstr );
			strcat( curtab->phases, "," );
		} /*endif*/

		/* read slowness range */
		if  (strncmp(line,"slowness:",9) == 0)  {
			if  (curtab == NULL)  {
				*status = eRsSyntaxError;
				return;
			} /*endif*/
			if  (sscanf(line,"slowness:%f\n-%f\n",
				&(curtab->minslow),&(curtab->maxslow)) != 2)  {
				*status = eRsReadSlowness;
				return;
			} /*endif*/
		} /*endif*/

		/* read length, azim & resid */
		if  (strncmp(line,"entries:",8) == 0)  {
			if  (curtab == NULL)  {
				*status = eRsSyntaxError;
				return;
			} /*endif*/
			if  (sscanf(line,"entries:%d\n",&(curtab->tablth)) != 1)  {
				*status = eRsReadTablth;
				return;
			} /*endif*/
			fscanf( fp, "%s", tmpstr );
			if  (strcmp(tmpstr,"azim:") != 0)  {
				*status = eRsSyntaxError;
				err_setcontext( " ## 'entries' must be followed by 'azim' " );
			} /*endif*/
			curtab->azim = (float *)sy_allocmem(
				curtab->tablth, (int)sizeof(float), status );
			if  (SySevere(status))  return;
			for  (i=0; i<(curtab->tablth); i++)  {
				if  (fscanf(fp,"%f\n",(curtab->azim)+i) != 1)  {
					*status = eRsSyntaxError;
					err_setcontext( " ## reading azim list " );
					return;
				} /*endif*/
			} /*endfor*/
			fscanf( fp, "%s", tmpstr );
			if  (strcmp(tmpstr,"resid:") != 0)  {
				*status = eRsSyntaxError;
				err_setcontext( " ## 'azim' must be followed by 'resid' " );
			} /*endif*/
			curtab->resid = (float *)sy_allocmem(
				curtab->tablth, (int)sizeof(float), status );
			if  (SySevere(status))  return;
			for  (i=0; i<(curtab->tablth); i++)  {
				if  (fscanf(fp,"%f\n",(curtab->resid)+i) != 1)  {
					*status = eRsSyntaxError;
					err_setcontext( " ## reading resid list " );
					return;
				} /*endif*/
			} /*endfor*/
		} /*endif*/

		/* insert table when finished */
		if  (strncmp(line,"end",3) == 0)  {
			/* check for completeness */
			if  (curtab == NULL)  {*status = eRsIncompleteTable; return;}
			if  (curtab->station[0] == '\0')
				{*status = eRsIncompleteTable; err_setcontext("station"); return;}
			if  (curtab->phases[0] == '\0')
				{*status = eRsIncompleteTable; err_setcontext("phases"); return;}
			if  (curtab->minslow < 0.0)
				{*status = eRsIncompleteTable; err_setcontext("minslow"); return;}
			if  (curtab->maxslow < 0.0)
				{*status = eRsIncompleteTable; err_setcontext("maxslow"); return;}
			if  (curtab->tablth <= 0)
				{*status = eRsIncompleteTable; err_setcontext("tablth"); return;}
			if  (curtab->azim == NULL)
				{*status = eRsIncompleteTable; err_setcontext("azim"); return;}
			if  (curtab->resid == NULL)
				{*status = eRsIncompleteTable; err_setcontext("resid"); return;}
			RsEnlistTable( curtab );
			curtab = NULL;
		} /*endif*/

	} /*endwhile*/

	if  (curtab != NULL)  {
		*status = eRsIncompleteTable;
		err_setcontext( curtab->station );
		sy_deallocmem( curtab );
		return;
	} /*endif*/

	fclose( fp );

} /* end of RsReadTables */



/*----------------------------------------------------------------------------*/



void RsInitTable( TRsTable *tab )

/* initializes table
 *
 * parameters of routine
 * TRsTable   *tab;      output; table to initialize
 */
{
	/* executable code */

	tab->station[0] = '\0';
	tab->minslow = cRsEmptySlowness;
	tab->maxslow = cRsEmptySlowness;
	tab->tablth = 0;
	tab->azim = NULL;
	tab->resid = NULL;
	tab->next = NULL;

} /* end of RsInitTable */



/*----------------------------------------------------------------------------*/



void RsDumpTables( void )

/* Dumps all tables to stdout
 *
 * no parameters
 */
{
	/* local variables */
	TRsTable *t;     /* moving pointer */

	/* executable code */

	printf( "\nResidual Table List Dump\n\n" );

	t = vrs_roottable;
	while  (t != NULL)  {
		RsDumpTable( t );
		t = t->next;
	} /*endwhile*/

	printf( "\n\n" );

} /* end of RsDumpTables */



/*----------------------------------------------------------------------------*/



float RsResidual( char station[], char phase[], float slowness, float azimuth,
	TSyStatus *status )

/* Returns residual time in s for given station, phase, slowness and
 * back-azimuth
 *
 * parameters of routine
 * char       station[];      input; station name
 * char       phase[];        input; phase name
 * float      slowness;       input; slowness in s/deg
 * float      azimuth;        input; back-azimuth n deg
 * TSyStatus  status;         output; return status
 */
{
	/* local variables */
	char     locstat[cRsStationLth+1]; /* local station name */
	TRsTable *t;           /* matching table */
	TSyBoolean found;      /* found right table */
	int      i;            /* index */
	float    resid;        /* interpolated residual */
	char     pstr[cRsPhaseListLth+1]; /* search string */

	/* executable code */

	/* create phase search string with commas */
	*pstr = ',';
	strcpy( pstr+1, phase );
	strcat( pstr, "," );

	/* create uppercase station name */
	if  (strlen(station) > cRsStationLth)  {
		*status = eRsLongName;
		return;
	} /*endif*/
	strcpy( locstat, station );
	ut_cap( locstat );

	/* find matching table */
	t = vrs_roottable;
	while  (t != NULL)  {
		found = TRUE;
		if  (strcmp(t->station,locstat) != 0)  found = FALSE;
		if  (strstr(t->phases,pstr) == NULL)  found = FALSE;
		if  (slowness < t->minslow)  found = FALSE;
		if  (slowness >= t->maxslow)  found = FALSE;
		if  (found)  break;
		t = t->next;
	} /*endwhile*/

	if  (t == NULL)  {
		*status = eRsNoTableFound;
		err_setcontext( " ## station: " );
		err_setcontext( locstat );
		err_setcontext( "  phase: " );
		err_setcontext( phase );
		err_setcontext( "  slowness: " );
		err_setcontext_r( slowness );
		return 0.0;
	} /*endif*/

	/*
	printf( "\nfound table:\n" );
	RsDumpTable( t );
	*/

	if  (azimuth < t->azim[0] || azimuth > t->azim[(t->tablth)-1])  {
		*status = eRsAzimOutOfRange;
		err_setcontext( " ## station: " );
		err_setcontext( locstat );
		err_setcontext( "  slowness: " );
		err_setcontext_r( slowness );
		err_setcontext( "  azimuth: " );
		err_setcontext_r( azimuth );
		return 0.0;
	} /*endif*/

	i = 1;
	while  (t->azim[i] < azimuth)  {
		i++;
		if  (i >= t->tablth)  {
			*status = eRsProgramBug;
			err_setcontext( " ## find azimuth index" );
			return;
		} /*endif*/
	} /*endwhile*/

	/*
	printf( "azimuth %5.1f is between %5.1f and %5.1f\n",
		azimuth, t->azim[i-1], t->azim[i] );
	*/

	resid = t->resid[i-1] + (azimuth - t->azim[i-1])
		* ((t->resid[i] - t->resid[i-1]) / (t->azim[i] - t->azim[i-1]));

	return resid;

} /* end of RsResidual */



/*----------------------------------------------------------------------------*/



static void RsEnlistTable( TRsTable *tab )

/* Appends table to the table list
 *
 * parameters of routine
 * TRsTable   *tab;       input; table to append
 */
{
	/* local variables */
	TRsTable *t;              /* moving pointer */

	/* executable code */

	if  (vrs_roottable == NULL)  {
		vrs_roottable = tab;
		return;
	} /*endif*/

	t = vrs_roottable;
	while  (t->next != NULL)
		t = t->next;

	t->next = tab;

} /* end of RsEnlistTable */



/*----------------------------------------------------------------------------*/



static void RsDumpTable( TRsTable *tab )

/* Dumps residual table to stdout
 *
 * parameters of routine
 * TRsTable   *tab;        input; table to dump
 */
{
	/* local varaibles */
	int      i;                /* counter */

	/* executable code */

	if  (tab == NULL)  {
		printf( "table == NULL\n" );
		return;
	} /*endif*/

	printf( "station:   >%s<\n", tab->station );
	printf( "phases:    %s\n", tab->phases );
	printf( "slowness:  %4.2f - %4.2f\n", tab->minslow, tab->maxslow );
	printf( "entries:   %d\n", tab->tablth );
	printf( "azim:      " );
	for  (i=0; i<(tab->tablth); i++)
		printf( "%5.1f ", tab->azim[i] );
	printf( "\n" );
	printf( "resid:     " );
	for  (i=0; i<(tab->tablth); i++)
		printf( "%5.2f ", tab->resid[i] );
	printf( "\n\n" );

} /* RsDumpTable */



/*----------------------------------------------------------------------------*/


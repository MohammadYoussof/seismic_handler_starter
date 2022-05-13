
/* file check_evt.c
 *      ===========
 *
 * version 10, 16-Jan-2007
 *
 * Checks consistency of created output file of SHM
 * K. Stammler, 21-Jul-94
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



#include <stdio.h>
#include <string.h>
#include BASECNST
#include BC_SYSBASE
#include "eventdsc.h"


#define MAX_PKP_SLOWNESS 4.5
#define MIN_P_SLOWNESS 4.3

#define EVTYPE_UNDEF 0
#define EVTYPE_LOCAL 1
#define EVTYPE_P     2
#define EVTYPE_PKP   3

/* next error number currently 024 */



/* global variables */
static char    efv_infile[BC_FILELTH+1];  /* name of input file */
static char    efv_uncorrphase[EvPHASELTH+1]; /* name of phase on uncorr beam */
static BOOLEAN efv_first_arrival=FALSE;   /* first arrival phase selected */
static BOOLEAN efv_only_grf=TRUE;         /* only GRF stations used */
static BOOLEAN efv_uncorr_loc=FALSE;      /* uncorrected beam for location */
static BOOLEAN efv_telex_phase=FALSE;     /* telex phase found */
static BOOLEAN efv_mb_found=FALSE;        /* mb determination done */
static BOOLEAN efv_ms_found=FALSE;        /* ms determination done */
static BOOLEAN efv_mbb_found=FALSE;       /* mbb determination done */
static int     efv_subtype=EVTYPE_UNDEF;  /* event type */
static EvEventTypeT efv_evtype=EvcEventTypeUndefined; /* event type */
static float   efv_depth=(-100.0);        /* depth of event */
static float   efv_magn=0.0;              /* max. magnitude found */
static int     efv_pkp_count=0;           /* number of PKP phases */



/* prototypes of local routines */
void EfPutCheck( FILE *out, EvEventT *event, EvStatusT *status );
static BOOLEAN EfPhaseIsFirstArrival( char name[] );
static void EfFinalCheck( FILE *out );
static BOOLEAN EfIsLocalPhase( char phase[] );



int main( int argc, char *argv[] )
{
	/* local variables */
	char      infile[BC_FILELTH+1];    /* input file */
	char      outfile[BC_FILELTH+1];   /* output file */
	EvStatusT status;                  /* return status */
	int       i;                       /* counter */

	/* executable code */

	if  (argc != 3)  {
		fprintf( stderr, "Usage: %s <input> <output>\n", argv[0] );
		return 1;
	} /*endif*/

	strcpy( infile, argv[1] );
	strcpy( outfile, argv[2] );

	/* efv_infile is used for messages, directory will be removed */
	i = strlen( infile ) - 1;
	while  (infile[i] != '/' && i > 0)  i--;
		strcpy( efv_infile, infile+i+1 );

	status = EveNOERROR;
	EvReformatEventfile( infile, outfile, EvGetEvent,
		EfPutCheck, &status );

	return 0;

} /* end of main */



/*---------------------------------------------------------------------------*/



void EfPutCheck( FILE *out, EvEventT *event, EvStatusT *status )

/* Prints error messages of event to file 'out'.
 *
 * parameters of routine
 * FILE       *out;          input; output file
 * EvEventT   *event;        input; event information
 * EvStatusT  *status;       output; return status
 */
{
	/* local variables */
	static BOOLEAN locqual_checked=FALSE; /* location quality checked ? */
	BOOLEAN  amplitude_measured;      /* amplitude measured */
	BOOLEAN  is_l_phase;              /* phase is L (or (L)) */
	BOOLEAN  is_pkp_phase;            /* phase is PKP */
	BOOLEAN  is_p_phase;              /* phase is P or (P) */
	BOOLEAN  loc_given;               /* location given in this record */

	/* executable code */

	/* final check if event is NULL */
	if  (event == NULL)  {
		EfFinalCheck( out );
		locqual_checked = FALSE;
		return;
	} /*endif*/

	/* fprintf( out, "--> check phase %s\n", event->phase ); */

	/* is a location given here ? */
	loc_given = (event->latitude > 0.0 && event->longitude > 0.0);

	/* is this an L phase ? */
	is_l_phase = (strcmp(event->phase,"L") == 0
		|| strcmp(event->phase,"(L)") == 0);

	/* is this a P phase ? */
	is_p_phase = (strcmp(event->phase,"P") == 0
		|| strcmp(event->phase,"(P)") == 0);

	/* is this a PKP phase ? */
	is_pkp_phase = (strstr(event->phase,"PKP") != NULL);

	if  (event->event_type != EvcEventTypeUndefined)
		efv_evtype = event->event_type;

	/* setup global variables used in final check */
	/* ------------------------------------------ */

	/* is it a GRF station ? */
	if  (strncmp(event->station,"GR",2) != 0)
		efv_only_grf = FALSE;

	/* is a first arrival phase there ? */
	if  (EfPhaseIsFirstArrival(event->phase))
		efv_first_arrival = TRUE;

	/* is there a location done with uncorrected beam ? */
	if  ((loc_given && event->b_slowness != EvEMPTY_SLOWNESS &&
		event->l_slowness == EvEMPTY_SLOWNESS
		&& event->loc_quality == EvcLocQualReliable
		&& strcmp(event->source,"SZGRF") == 0)
		|| (event->loc_method == EvcLocMethUncorrBeam))  {
		efv_uncorr_loc = TRUE;
		strcpy( efv_uncorrphase, event->phase );
	} /*endif*/

	/* look for telex phase */
	if  (strchr(event->phase_flags,EvFLAG_TELEX) != NULL)
		efv_telex_phase = TRUE;

	/* look for depth */
	if  (efv_depth < -99.0 && event->depth_type != EvcDepthUndefined)
		efv_depth = event->depth;

	/* find plain mistakes */
	/* ------------------- */

	/* special for L phases */
	if  (is_l_phase)  {
		/* is there an impulsive L phase ? */
		if  (event->onset_type == EvcOnsetImpulsive)
			fprintf( out, "F001 %s: phase L with impulsive onset\n", efv_infile );
		/* is a legal filter used ? */
		if  (strcmp(event->filter,"G_WWSSN_SP") == 0
			|| strcmp(event->filter,"STANDARD_BP") == 0
			|| event->filter[0] == '\0')
			fprintf( out, "F002 %s: L phase reading with illegal filter %s\n",
				efv_infile, event->filter );
		/* is there slowness and azimuth ? if yes that's real nonsense */
		if  (event->b_slowness != EvEMPTY_SLOWNESS
			|| event->b_azimuth != EvEMPTY_AZIMUTH
			|| event->l_slowness != EvEMPTY_SLOWNESS
			|| event->l_azimuth != EvEMPTY_AZIMUTH)
			fprintf( out, "F003 %s: slowness and/or azimuth for phase %s\n",
				efv_infile, event->phase );
		/* is amplitude & period determined ? */
		if  (event->amplitude == EvEMPTY_AMPLITUDE
			|| event->period == EvEMPTY_PERIOD)
			fprintf( out, "F004 %s: no amplitude / period for phase %s\n",
				efv_infile, event->phase );
	} /*endif*/

	if  (is_pkp_phase)  {
		/* check slowness range */
		if  (event->b_slowness != EvEMPTY_SLOWNESS
			&& event->b_slowness > MAX_PKP_SLOWNESS)
			fprintf( out, "F005 %s: illegal beam slowness %3.1f for phase %s\n",
				efv_infile, event->b_slowness, event->phase );
		if  (event->l_slowness != EvEMPTY_SLOWNESS
			&& event->l_slowness > MAX_PKP_SLOWNESS)
			fprintf( out, "F006 %s: illegal corr. slowness %3.1f for phase %s\n",
				efv_infile, event->l_slowness, event->phase );
		efv_subtype = EVTYPE_PKP;
		efv_pkp_count++;
	} /*endif*/

	if  (is_p_phase)  {
		/* check slowness range */
		if  (event->b_slowness != EvEMPTY_SLOWNESS
			&& event->b_slowness < MIN_P_SLOWNESS)
			if  (event->l_slowness == EvEMPTY_SLOWNESS)
				fprintf( out, "F007 %s: illegal beam slowness %3.1f for phase %s\n",
					efv_infile, event->b_slowness, event->phase );
		if  (event->l_slowness != EvEMPTY_SLOWNESS
			&& event->l_slowness < MIN_P_SLOWNESS)
			fprintf( out, "F008 %s: illegal corr. slowness %3.1f for phase %s\n",
				efv_infile, event->l_slowness, event->phase );
		efv_subtype = EVTYPE_P;
	} /*endif*/

	if  (EfIsLocalPhase(event->phase))  efv_subtype = EVTYPE_LOCAL;

	/* amplitude measurement must be complete */
	amplitude_measured = (event->period != EvEMPTY_PERIOD
		|| event->amplitude != EvEMPTY_AMPLITUDE
		|| event->amplitude_time != EvEMPTY_AMPLITUDE_TIME
		|| event->amplitude_vel != EvEMPTY_AMPLITUDE);
	if  (amplitude_measured)
		if  (event->period == EvEMPTY_PERIOD
			|| event->amplitude == EvEMPTY_AMPLITUDE
			|| (event->amplitude_time == EvEMPTY_AMPLITUDE_TIME && !is_l_phase)
			|| event->amplitude_vel == EvEMPTY_AMPLITUDE
			|| event->ap_source == EvcApSourceUndefined)
			fprintf( out,
				"F009 %s: incomplete amplitude determination at phase %s, station %s\n",
				efv_infile, event->phase, event->station );

	/* check for magnitudes determined */
	if  (event->mag[EvcMagMb] != EvEMPTY_MAGNITUDE)  {
		efv_mb_found = TRUE;
		if  (event->mag[EvcMagMb] > efv_magn)  efv_magn = event->mag[EvcMagMb];
	} /*endif*/
	if  (event->mag[EvcMagMs] != EvEMPTY_MAGNITUDE)  {
		efv_ms_found = TRUE;
		if  (event->mag[EvcMagMs] > efv_magn)  efv_magn = event->mag[EvcMagMs];
	} /*endif*/
	if  (event->mag[EvcMagMbb] != EvEMPTY_MAGNITUDE) efv_mbb_found = TRUE;

	/* magnitudes mb,MS are valid only with amplitude */
	if  (event->mag[EvcMagMs] != EvEMPTY_MAGNITUDE
		|| event->mag[EvcMagMb] != EvEMPTY_MAGNITUDE)
		if  (!amplitude_measured)
			fprintf( out, "F010 %s: magnitude without amplitude on phase %s\n",
				efv_infile, event->phase );

	/* a reliable location with PKP is not possible */
	if  (event->loc_quality == EvcLocQualReliable && is_pkp_phase
		&& strcmp(event->source,"SZGRF") == 0 && loc_given)
		fprintf( out, "F011 %s: reliable location with phase %s not possible\n",
			efv_infile, event->phase );

	/* event type tele and local phases Pn, Pg, Sn, Sg */
	if  (event->event_type == EvcEventTypeTeleQuake
		&& EfIsLocalPhase(event->phase))
		fprintf( out, "F012 %s: incompatible event type TELE with phase %s\n",
			efv_infile, event->phase );

	/* is there a location quality specified in the first phase ? */
	if  (loc_given && !locqual_checked && event->phase[0] != '\0')  {
		if  (event->loc_quality == EvcLocQualUndefined)
			fprintf( out, "F013 %s: no location quality specified\n", efv_infile );
		locqual_checked = TRUE;
	} /*endif*/

} /* end of EfPutCheck */



/*---------------------------------------------------------------------------*/



static BOOLEAN EfPhaseIsFirstArrival( char name[] )

/* Checks whether phase is first arrival
 *
 * parameters of routine
 * char       name[];        input; name of phase
 *                           returns TRUE if phase is possible first arrival
 */
{
	/* executable code */

	if  (strcmp(name,"P") == 0)  return TRUE;
	if  (strcmp(name,"(P)") == 0)  return TRUE;
	if  (strncmp(name,"PKP",3) == 0)  return TRUE;
	if  (strncmp(name,"(PKP",4) == 0)  return TRUE;
	if  (strncmp(name,"Pn",2) == 0)  return TRUE;
	if  (strncmp(name,"Pg",2) == 0)  return TRUE;
	if  (strcmp(name,"(Pn)") == 0)  return TRUE;
	if  (strcmp(name,"(Pg)") == 0)  return TRUE;
	if  (strcmp(name,"Pdiff") == 0)  return TRUE;
	if  (strcmp(name,"(Pdiff)") == 0)  return TRUE;
	if  (strcmp(name,"Pdif") == 0)  return TRUE;
	if  (strcmp(name,"(Pdif)") == 0)  return TRUE;

	return FALSE;

} /* end of EfPhaseIsFirstArrival */



/*---------------------------------------------------------------------------*/



static void EfFinalCheck( FILE *out )

/* Prints result of final check to output file 'out'
 *
 * parameters of routine
 * FILE       *out;         input; pointer to output file
 */
{
	/* executable code */

	/* fprintf( out, "--> final check\n" ); */

	/* a first arrival must be specified */
	if  (!efv_first_arrival)
		fprintf( out, "F014 %s: no first arrival phase selected\n", efv_infile );

	/* beam with uncorrected slowness at GRF is at least suspicious */
	if  (efv_only_grf && efv_uncorr_loc)
		fprintf( out, "F015 %s: GRF location with uncorrected beam at phase %s\n",
			efv_infile, efv_uncorrphase );

	/* no telex phase */
	if  (!efv_telex_phase)
		fprintf( out, "F016 %s: no telex phase found\n", efv_infile );

	/* mb determined? */
	if  (efv_subtype == EVTYPE_P && !efv_mb_found)
		fprintf( out, "F017 %s: no Magnitude mb determined for P phase\n",
			efv_infile );

	/* Ms determined? */
	if  (efv_subtype >= EVTYPE_P && efv_depth <= 50.0 && efv_magn > 5.6 &&
		!efv_ms_found)
		fprintf( out, "F018 %s: no Magnitude Ms determined for magn %3.1f event\n",
			efv_infile, efv_magn );
	if  (efv_subtype == EVTYPE_PKP && efv_depth <= 50.0 && efv_pkp_count > 3 &&
		!efv_ms_found)
		fprintf( out, "F019 %s: no Magnitude Ms determined for PKP event\n",
			efv_infile );
	if  (efv_ms_found && efv_depth > 50.0)
		fprintf( out, "F020 %s: Magnitude Ms determined for depth %5.1f event\n",
			efv_infile, efv_depth );

	/* Mbb determined? */
	if  (efv_mb_found && efv_magn > 6.2 && !efv_mbb_found)
		fprintf( out, "F021 %s: no Broadband Magnitude determined for magn %3.1f event\n",
			efv_infile, efv_magn );

	/* depth ok? */
	if  (efv_evtype == EvcEventTypeMining && Abs(efv_depth-1.0) > 0.1)
		fprintf( out, "F022 %s: depth %5.1f given for mining event\n",
			efv_infile, efv_depth );
	if  (efv_evtype == EvcEventTypeBlast && Abs(efv_depth) > 0.1)
		fprintf( out, "F023 %s: depth %5.1f given for blast\n",
			efv_infile, efv_depth );

} /* end of EfFInalCheck */



/*---------------------------------------------------------------------------*/



static BOOLEAN EfIsLocalPhase( char phase[] )

/* checks whether 'phase' is a local phase
 *
 * parameters of routine
 * char       phase[];         input; phase name
 */
{
	/* local variables */

	/* executable code */

	if  (strcmp(phase,"Pg") == 0)  return TRUE;
	if  (strcmp(phase,"Pn") == 0)  return TRUE;
	if  (strcmp(phase,"Sg") == 0)  return TRUE;
	if  (strcmp(phase,"Sn") == 0)  return TRUE;
	if  (strcmp(phase,"(Pg)") == 0)  return TRUE;
	if  (strcmp(phase,"(Pn)") == 0)  return TRUE;
	if  (strcmp(phase,"(Sg)") == 0)  return TRUE;
	if  (strcmp(phase,"(Sn)") == 0)  return TRUE;

	return FALSE;

} /* end of EfIsLocalPhase */



/*---------------------------------------------------------------------------*/


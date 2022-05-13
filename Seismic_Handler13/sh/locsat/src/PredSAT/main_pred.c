
/*
 * NAME
 *	PredSAT ([Pred]ict [S]lowness, [A]zimuth and [T]ravel-time bounds)

 * FILE    
 *	main_pred.c

 * SYNOPSIS
 *	Given an event location with confidence bounds, PredSAT computes 
 *	bounds on the expected travel-times, slownesses, and azimuths 
 *	for desired phases at a given station.

 * DESCRIPTION
 *	Main Program.

 *	Usage:
 *	PredSAT [-i] -e event.file -o output.file -s station.file
 *		-t tttab.pre -w waves

 *		-i: Select interactive mode (missing arguments solicited)
 *		-e: Specify file containing event information
 *		-o: Specify output file
 *		-s: Specify file containing station information
 *		-t: Specify path and prefix for travel-time tables
 *		-w: Specify a list of phases types to use

 *	Information on stations and event parameters are read by PredSAT 
 *	from files formated as follows [Note that one blank space exists
 *	between each attribute]: 

 *	STATION FILE
 *	Each Line:	fscanf (fio, "%6s%f%f%f%*[^\n]",
 *	----------------------------------------------------------------
 *	sites[].sta	%6s	Station code (ID)
 *	sites[].lat	%f	Station latitude (deg)
 *	sites[].lon	%f	Station longitude (deg)
 *	sites[].elev	%f	Station elevation (km)

 *	INPUT EVENT FILE
 *	First Line:	fscanf (fio, "%f%f%f%*[^\n]",
 *	----------------------------------------------------------------
 *	origin->alat	%f	First guess lat (deg, computed if ridiculous)
 *	origin->alon	%f	First guess lon (deg, computed if ridiculous)
 *	origin->depth	%f	Fixed depth (km) [0.0]

 *	Second line:	fscanf (fio, "%f%f%f%f%f%*[^\n]", 
 *	----------------------------------------------------------------
 *	origerr->smajax	%f	Length of semi-major axis of confidence 
 *				ellipse on epicenter (km)
 *	origerr->sminax	%f	Length of semi-minor axis of confidence 
 *				ellipse on epicenter (km)
 *	origerr->strike	%f	Strike of of semi-major axis of confidence 
 *				ellipse on epicenter (deg)
 *	origerr->sdepth	%f	Length of confidence semi-interval on 
 *				depth (km)
 *	origerr->stime	%f	Length of confidence semi-interval on 
 *				origin time (sec)

 *	---- Functions called ----
 *	Local
 *		getargp:	Get and process command line arguments

 *	From libloc
 *		predsat:	Predict slowness, azimuth and time bounds
 
 * DIAGNOSTICS
 *	Complains when input data are bogus or poorly formatted.
 
 * FILES
 *	Read station and event files here.
 
 * NOTES
 *	Currently under-going major changes.
 
 * SEE ALSO
 *
 
 * AUTHORS
 *	Steve Bratt		Created.
 *	Walter Nagy, May 1991.	Converted from FORTRAN to C.
 */
 

#ifdef	SCCSID
static	char	SccsId[]= "@(#)main_pred.c	43.1	9/9/91	Copyright 1991 Science Applications Inte
rnational Corporation.";
#endif
 
#include <stdio.h>
#include <strings.h>
#include <ctype.h>

#include "aesir.h"
#include "db_origerr.h"
#include "db_origin.h"
#include "db_site.h"

#define MAXSTA		150
#define MAXWAV		40
#define	MAX(x,y)	(((x) > (y)) ? (x) : (y))

extern int	setup_tttables ();

FILE	*fio_err;	/* Error messages device */
char	*ermsg;		/* Error return flag/message */

char *USAGE = "Usage: PredSAT [-i] -s Sname -e Ename -o Oname -t Tname -w Wname\n\
		where\n\
		Ename:	Event   filename: contains event data\n\
		Oname:	Output  filename:\n\
		Sname:	Station filename: contains station information\n\
		Tname:	Table   filename: specify directory path and prefix\n\
					  of travel-time tables\n\
		Wname:	Wave    filename: specify list of phases desired\n\
		-i   :	Optional Interactive mode\n\
			(missing arguments solicited)\n";

/* Default command line arguments */

#define TOKE_DEF  "events"
#define TOKO_DEF  "output"
#define TOKS_DEF  "stations"
#define TOKT_DEF  "/nmrd/top/data/tab"
#define TOKW_DEF  "P Pn Pg pP S Sn Lg sP"

struct control
{
	FILE	*fio;
	char	*fmode,
		*fnam,
		*query;
}
mio[5] =
{
	{ NULL, "r", TOKE_DEF,	"Event file name ? "            },
	{ NULL, "a", TOKO_DEF,	"Output file name ? "           },
	{ NULL, "r", TOKS_DEF,	"Station file name ? "          },
	{ NULL, "r", TOKT_DEF,	"T-T table path and prefix ? "  },
	{ NULL, "r", TOKW_DEF,	"Phase list ? "                 },
};

enum { TOKE, TOKO, TOKS, TOKT, TOKW };

#define solowa(a, b, c, d) {printf ("Interactive:%s %s %s\n", a, b, c); \
                            exit (-1);}


main (argc, argv)
int	argc;
char	**argv;
{

	Origerr	*origerr;
	Origin	*origin;
	Site	*sites;

	static	int	num_phases = 0;

	FILE	*fio;
	char	*dummy_ptr, *stem, tchar[2];
	int	i, j, k, n;

	int	loc_err, len_phase_type, print_flag, slow_flag;
	char	*fntab,
		**phase_type_ptr,
		*phase_type,
		station_name[6+1];

	int	arrival_id[MAXSTA][MAXWAV],
		isc[MAXSTA][MAXWAV],
		itc[MAXSTA][MAXWAV],
		slow_err_center, slow_err_max, slow_err_min,
		tt_err_center, tt_err_max, tt_err_min;
	float	azc[MAXSTA][MAXWAV],
		slc[MAXSTA][MAXWAV],
		ttc[MAXSTA][MAXWAV],
		az_center, az_max, az_min,
		dist_center, dist_max, dist_min,
		slow_center, slow_max, slow_min,
		tt_center, tt_max, tt_min;

	int	num_obs;	/* Number of observing stations */
	int	ierr	 = 0,	/* Initialize error codes */
		inter	 = 0,	/* Non-zero for interactive mode */
		num_sta	 = 0,	/* Number of stations in station file */
		num_data = 0;	/* Total number of observations */

	struct control *m_in;		/* Scratch pointer for loops */

	extern	void	predsat ();
	char	*getargp ();		/* Function */


	/* Allocate space for structures */

	origin	= UALLOC(Origin,1);
	origerr	= UALLOC(Origerr,1);
	sites	= UALLOC(Site,MAXSTA);

	fio_err = stderr;

	ermsg = getargp (argc, argv, &inter, &mio[0].fnam, &mio[1].fnam,
			 &mio[2].fnam, &mio[3].fnam, &mio[4].fnam);

	if (ermsg != '\0')
	{
		fprintf (fio_err, "ERROR:getargp:%s\n", ermsg);
		exit (-1);
	}

	/* Open input/output files */

	for (ierr = 0, i = 0; i < 3 && !ierr; i++)
	{
		m_in = mio+i;
		stem = m_in->fnam;
		while (!ierr &&
		       ((m_in->fio = fopen (m_in->fnam, m_in->fmode)) == NULL))
		{
			fprintf (fio_err, "ERR: opening <%s> file <%s>\n",
				 m_in->query, m_in->fnam);
			if (inter)
			{
				solowa(m_in->query, m_in->fnam, stem, &ierr);
			}
			else
				ierr = 1;
		}
		m_in->fnam = stem;
	}

	/* Exit on error */

	if (ierr)
	{
		if (!inter) fprintf (fio_err, "%s", USAGE);
		exit (-1);
	}


	fntab = mio[TOKT].fnam;

	/* Read list of phase types and stuff into the appropriate arrays */

	k = strlen(mio[TOKW].fnam);
	for (i = 0, j = 0, len_phase_type = 0, num_phases = 0; i < k; i++)
	{
		if (! strncmp (mio[TOKW].fnam + i, " ", 1))
		{
			len_phase_type = MAX(len_phase_type, j);
			j = 0;
			++num_phases;
		}
		else
			j++;
	}
	++num_phases;
	len_phase_type = len_phase_type + 1;

	phase_type = (char *)malloc((unsigned)(num_phases * len_phase_type) *
		    (sizeof(char)));
	phase_type_ptr = (char **)malloc((unsigned)num_phases * sizeof(char *));

	for (i = 0, j = 0, num_phases = 0; i < k; i++)
	{
		if (! strncmp (mio[TOKW].fnam + i, " ", 1))
		{
			strncpy (phase_type + num_phases*len_phase_type, 
				 mio[TOKW].fnam + (i - j), j);
			j = 0;
			++num_phases;
		}
		else
			j++;
	}
	strncpy (phase_type + num_phases*len_phase_type,
		 mio[TOKW].fnam + (i - j), j);
	++num_phases;

	for (i = 0; i < num_phases; i++)
	{
		dummy_ptr = phase_type + i*len_phase_type;
		strcpy (dummy_ptr, phase_type + i*len_phase_type);
		phase_type_ptr[i] = dummy_ptr;
	}


	/* Read travel-time tables */

	if ((loc_err = setup_tttables (fntab, phase_type_ptr, num_phases)) != 0)
		printf ("Problems reading travel-time tables\n");

	/* Read station information, one station at a time */

	fio = mio[TOKS].fio;
	for (num_sta = 0, ierr = 4; ierr == 4 && num_sta < MAXSTA; num_sta++)
	{
		ierr = fscanf (fio, "%6s%f%f%f%*[^\n]", sites[num_sta].sta,
				&sites[num_sta].lat, &sites[num_sta].lon,
				&sites[num_sta].elev);
		if (ierr != 4) break;
	}

	if (num_sta > MAXSTA)
		fprintf (fio_err, "%s (%d), %s\n",
			 "Number of stations greater than dimension",
			 MAXSTA, " remaining Stations ignored!");

	/* Read location and confidence bounds for event */
 
	fio = mio[TOKE].fio;
	ierr = fscanf (fio, "%f%f%f%*[^\n]", &origin->lat, &origin->lon, 
					     &origin->depth);
	if (ierr != 3)
		fprintf (fio_err, "Problem reading 1st line of event data\n");

	ierr = fscanf (fio, "%f%f%f%f%f%*[^\n]", 
			&origerr->smajax, &origerr->sminax, &origerr->strike,
			&origerr->sdepth, &origerr->stime);
	if (ierr != 5)
		fprintf (fio_err, "Problem reading 2nd line of event data\n");

	/* Loop over stations and phases */

	slow_flag  = TRUE;
	print_flag = TRUE;

	for (i = 0; i < num_sta; i++)
	{
		for (k = 0; k < num_phases; k++, ++num_data)
		{
			arrival_id[i][k] = num_data;
			predsat (sites, num_sta, origin, origerr, sites[i].sta,
				 phase_type + k*len_phase_type, phase_type, 
				 len_phase_type, num_phases, num_data,
				 slow_flag, print_flag, mio[TOKO].fnam,
				 &dist_min, &dist_max, &dist_center, &tt_min,
				 &tt_max, &tt_center, &az_min, &az_max,
				 &az_center, &slow_min, &slow_max, &slow_center,
				 &tt_err_min, &tt_err_max, &tt_err_center,
				 &slow_err_min, &slow_err_max,
				 &slow_err_center);
			ttc[i][k] = tt_center;
			azc[i][k] = az_center;
			slc[i][k] = slow_center;
			isc[i][k] = slow_err_center;
			itc[i][k] = tt_err_center;
		}
	}

	fio = fopen (mio[TOKO].fnam, "a");
	for (i = 0; i < num_sta; i++)
	{
		for (k = 0; k < num_phases; k++)
		{
			if (itc[i][k] == 0)
			{
				fprintf (fio, "%8d %-6s %-8s time d %16.3f%8.3f\n",
					 arrival_id[i][k], sites[i].sta, 
					 phase_type + k*len_phase_type,
					 ttc[i][k], 1.0); 
				fprintf (fio, "%8d %-6s %-8s azim d %16.3f%8.3f\n",
					 arrival_id[i][k], sites[i].sta, 
					 phase_type + k*len_phase_type,
					 azc[i][k], 10.0);
				if (isc[i][k] == 0)
					fprintf (fio, "%8d %-6s %-8s slow d %16.3f%8.3f\n", 
						 arrival_id[i][k], sites[i].sta,
					 	 phase_type + k*len_phase_type,
						 slc[i][k], 0.5);
			}
		}
	}

	UFREE (phase_type);
	UFREE (phase_type_ptr);

	for (i = 0; i < 3; i++)
		if (fclose (mio[i].fio) != 0)
		{
			fprintf (fio_err, "ERROR closing <%s>\n", mio[i].fnam);
			exit (-1);
		}
	exit (0);

}


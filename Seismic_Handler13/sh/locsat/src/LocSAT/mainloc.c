
/*
 *	Copyright 1991 Science Applications International Corporation.

 * NAME
 *	mainloc -- Main program for driving program, LocSAT

 * FILE    
 *	mainloc.c

 * SYNOPSIS
 *	Compute event locations, confidence bounds, residuals and
 *	importances using arrival times, azimuths, and slowness
 *	measusrements from stations at regional and teleseismic 
 *	distances.

 * DESCRIPTION
 *	Program.  Determine event locations and related statistics via 
 *	least-squares inversion.  LocSAT runs in either a single event
 *	or batch mode.  LocSAT compute event locations, confidence bounds, 
 *	residuals and importances using arrival times, azimuths, and 
 *	slowness measusrements from stations at regional and teleseismic 
 *	distances.

 *	Usage:	LocSAT [-i] -s station.file -d data.file -c control.file
 *			    -o output.file

 *		-i: Select interactive mode (missing arguments solicited)
 *		-s: Specify file containing station information
 *		-d: Specify file containing observed phase data
 *		-c: Specify file containing information on how to perform
 *		    the location
 *		-o: Specify output file

 *	The travel-time and source-specific station correction files are
 *	read in locate_event() as specified in the local control file.  
 *	Information on stations, detections and location parameters are 
 *	read directly from this LocSAT routine using files formated as:

 *	STATION FILE
 *	Each Line:	fscanf (fio, "%6s%f%f%f%*[^\n]",
 *	----------------------------------------------------------------
 *	sites[].sta	%6s	Station code
 *	sites[].lat	%9.4f	Latitude (deg)
 *	sites[].lon	%9.4f	Longitude (deg)
 *	sites[].elev	%9.4f	Elevation (km)

 *	DATA FILE
 *	First line:	fscanf (fio, "%2d%2d%2d %2d%2d%f%f%f%f%f%3d%*[^\n]",
 *	----------------------------------------------------------------
 *	myear		%2d	Last two digits of year term on origin
 *	dt->month	%2d	Month term of origin
 *	dt->day		%2d	Day term of origin
 *	dt->hour	%2d	Hour term of origin
 *	dt->minute	%2d	Minute term of origin
 *	dt->second	%f	Seconds after minute of origin
 *	lat_init	%f	First guess latitude (deg)
 *	lon_init	%f	First guess longitude (deg)
 *	depth_init	%f	Initial depth (km) 
 *	event_mag	%f	Event magnitude
 *	num_data	%3d	Number of observation in data file

 *	Note that lat_init, lon_init, and depth_init are part of the
 *	locator_params structure.  If lat_init or lon_init are set 
 *	to 999.0, then LocSAT will determine its own best guess initial 
 *	location also see effect of variable, use_location).

 *	Following lines: fscanf (fio, "%8d %6s %8s %4s%2s%f%f%*[^\n]",
 *	----------------------------------------------------------------
 *	arrival_id 	%8d 	Arrival ID (put into structure,
 *				assoc[].arid)
 *	station_name	%6s	Station name (put into structure,
 *				arrival[].sta)
 *      phase_type	%8s	Phase type (e.g., Pn, PKP, Lg) 
 *				(put into structure, assoc[].phase)
 *      data_type	%4s	Data type ([t]ime, [a]zim, or [s]low)
 *      arrival_type	%2s	Arrival usage (put into the appropriate 
 *				assoc structure, i.e., assoc[].timedef,
 *				assoc[].azdef and/or assoc[].slodef)
 *				  = d: Defining, used in location
 *				  = n: Non-defining, not used in location
 *	obs_data	%f	Observed datum (sec, deg, or sec/deg)
 *				(put into the appropriate arrival structure,
 *				i.e., arrival[].time, arrival[].azimuth
 *				or arrival[].slow)
 *	std_dev_data	%f	Standard deviation of observed datum (sec,
 *				deg, or sec/deg) (put into the appropriate 
 *				arrival structure, i.e., arrival[].deltim, 
 *				arrival[].delaz, or arrival[].delslo)

 *	A blank line must seperate events when run in batch mode.

 *	CONTROL FILE
 *	Line 1:		fscanf (fio, "%s%s",
 *	----------------------------------------------------------------
 *	tt_dir		%30s	Directory pathway for travel-time tables
 *	tt_prefix	%20s	Prefix for travel-time tables 
 *				(tt_dir and tt_prefix are concatanated
 *				and put into the structure, 
 *				locator_params->prefix)

 *	Line 2:		fscanf (fio, "%s",	fscanf (fio, "%s%1[^\n]",
 *	----------------------------------------------------------------
 *	corr_dir 	%30s	Directory containing station corrections
 *	corr_type	%48s	Station correction types [e.g., TT, AZ, AMP]

 *	Line 3:		fscanf (fio, " %1c %1c %1c%f%f%f%d%d%*[^\n]",
 *	----------------------------------------------------------------
 *	use_location	 %1c	Use location given on the summary line of
 *				the data (phase) file; else let LocSAT
 *				determine it own best initial location
 *	fix_depth	 %1c	Fix-depth flag (y = fix depth, n = free depth)
 *	verbose 	 %1c	Print verbose output (y = yes, n = no)
 *	conf_level	%f	Confidence ellipse level
 *	damp		%f	Percent damping relative to largest 
 *				singular value
 *	est_std_err	%f	A priori variance scale factor
 *	num_dof 	%d	Number of degrees of freedom in sig0
 *	max_iterations	%d	Maximum number of location iterations

 *	The first 3 character arguments in the control file must have 
 *	a blank before them!  All variables on Line 3 of the control file 
 *	are part of the locator_params structure.  

 *	---- Functions called ----
 *	Local
 *		getargs:	Get command line arguments

 *	From libloc
 *		locate_event:	Main location module

 * DIAGNOSTICS
 *	Complains when input data are bogus or poorly formatted.

 * FILES
 *	Read control, data and station files here.

 * NOTES
 *	Currently under-going major changes.  SSSC are internal turned off
 *	in this version.

 * SEE ALSO
 *	Bratt and Bache (1988).  "Locating events with a sparse network
 *	of regional arrays", BSSA, 78, 780-798.  The extended descriptions 
 *	for the database structures along with acceptable bounds is given 
 *	in the CSS database reference manual, "Center for Seismic Studies, 
 *	Version 3 Database: Schema Reference Manual" by Anderson, Farrell,
 *	Garcia, Given and Swanger, 1990.

 * AUTHOR
 *	Walter Nagy, February 1991.
 */


#ifdef  SCCSID
static	char	SccsId[]= "@(#)mainloc.c	44.2	10/4/91	Copyright 1991 Science Applications International Corporation.";
#endif

#include <strings.h>
#include <stdio.h>
#include <ctype.h>

#include "params.h"
#include "aesir.h"
#include "loc_params.h"
#include "db_arrival.h"
#include "db_assoc.h"
#include "db_origerr.h"
#include "db_origin.h"
#include "db_site.h"
#include "csstime.h"

FILE	*fio_err;	/* Error messages device */
char	*ermsg;		/* Error return flag/message */

char *USAGE = "Usage: LocSAT [-i] -s Sname -d Dname -c Cname -o Oname\n\
		where\n\
		Sname:	Station filename: contains station information\n\
		Dname:	Data    filename: contains observed phase data\n\
		Cname:	Control filename: contains information on how\n\
					  to perform the location\n\
		Oname:	Output  filename:\n\
		-i   :	Optional Interactive mode\n\
			(missing arguments solicited)\n";

/* Default command line arguments */

#define TOKS_DEF  "stations"
#define TOKD_DEF  "data"
#define TOKC_DEF  "control"
#define TOKO_DEF  "output"

struct control
{
	FILE	*fio;
	char	*fmode,
		*fnam,
		*query;
}
mio[4] =
{
	{ NULL, "r", TOKC_DEF,  "Control file name ? " },
	{ NULL, "r", TOKD_DEF,  "Data file name ? "    },
	{ NULL, "w", TOKO_DEF,  "Output file name ? "  },
	{ NULL, "r", TOKS_DEF,  "Station file name ? " },
};

enum { TOKC, TOKD, TOKO, TOKS };

#define solowa(a, b, c, d) {printf ("Interactive:%s %s %s\n", a, b, c); \
			    exit (-1);}


main (argc, argv)
int	argc;
char	**argv;
{
	Arrival		*arrival;
	Assoc		*assoc;
	Origerr		*origerr;
	Origin		*origin;
	Site		*sites;
	Locator_params	*locator_params;
	Locator_errors	*locator_errors;
	struct		date_time *dt;

	static int	nwav = 0;
	static char	*newnet = (char *)NULL;

	int	i, n,
		myear,
		status,
		arrival_id;
	FILE	*fio;
	char	*stem, tchar[2];

	char	fntab[50+2],
		corr_type[MAXTYP][8+1],
		use_location[1+1],
		data_type[4+1],
		arrival_type[1+1],
		station_name[6+1],
		phase_type[8+1];
	float	obs_data,
		std_dev_data,
		event_mag;
	double	origin_time_init;

	int	num_obs,		/* Number of observing stations */
		num_data;		/* Total number of observations */
	int	ierr		= 0,	/* Initialize error codes */
		inter		= 0,	/* Non-zero for interactive mode */
		num_sta		= 0,	/* Number of stations in station file */
		num_type	= 0;	/* Number of different types of
					   station correction types desired */

	struct control *m_in;		/* Scratch pointer for loops */

	char	*getargs ();		/* Function */

	/* Allocate space for structures */

	origin		= UALLOC(Origin,1);
	origerr		= UALLOC(Origerr,1);
	sites		= UALLOC(Site,MAXSTA);
	locator_params	= UALLOC(Locator_params,1);
	dt		= UALLOC(struct date_time,1);

	fio_err = stderr;

	/* Get command line arguments */

	ermsg = getargs (argc, argv, &inter, &mio[0].fnam, &mio[1].fnam,
			 &mio[2].fnam, &mio[3].fnam);

	if (ermsg != '\0')
	{
		fprintf (fio_err, "ERROR:getargs:%s\n", ermsg);
		exit (-1);
	}

	/* Open input/output files */

	for (ierr = 0, i = 0; i < 4 && !ierr; i++)
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

	/* Stuff output-file name into locator_params structure */

	m_in = mio + 2;
	locator_params->outfile_name = STRALLOC(m_in->fnam);


	/* Begin reading input files, starting with Control file */

	fio = mio[TOKC].fio;

	/* Read T-T directory name and prep for filename concatenation */

	if (fscanf (fio, "%s%s", fntab, fntab+31) != 2)
	{
		fprintf (fio_err, "ERROR:reading Control line 1\n");
		exit (-1);
	}
	strcat (fntab, "/");
	strcat (fntab, fntab+31);
	locator_params->prefix = STRALLOC(fntab);

	/* Read corr directory name and prepare for filename concatenation */

	if (fscanf (fio, "%s", fntab) != 1)
	{
		fprintf (fio_err, "ERROR:reading Control line 2\n");
		exit (-1);
	}
	locator_params->cor_level = 0;

	/* Read type of source-specific station correction info desired */

	for (num_type = 0; num_type < MAXTYP && 
		fscanf (fio, "%s%1[^\n]", corr_type[num_type++], tchar) == 2;);

	/* Read desired location variables */

	ierr = fscanf (fio, " %1c %1c %1c%f%f%f%d%d%*[^\n]",
			use_location, &locator_params->fix_depth,
			&locator_params->verbose, &locator_params->conf_level,
			&locator_params->damp, &locator_params->est_std_error,
			&locator_params->num_dof,
			&locator_params->max_iterations);
	if (ierr != 8)
	{
		fprintf (fio_err, "ERROR:reading Control line 3\n");
		exit (-1);
	}

	if (strncmp(use_location,"y",1) == 0)
		locator_params->use_location = TRUE;
	else
		locator_params->use_location = FALSE;


	/* Station file */

	fio = mio[TOKS].fio;

	/* Read station info, one station at a time */

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


	/* Data file */

        fio = mio[TOKD].fio;

	/* Read summary line of data file */

again:	ierr = fscanf (fio, "%2d%2d%2d %2d%2d%f%f%f%f%f%3d%*[^\n]",
			&myear, &dt->month, &dt->day, &dt->hour, &dt->minute,
			&dt->second, &locator_params->lat_init,
			&locator_params->lon_init, &locator_params->depth_init,
			&event_mag, &num_data);

	if (ierr == EOF)
	{					 /* Close files */
		for (i = 0; i < 4 ; i++)
		if (fclose (mio[i].fio) != 0)
		{
			fprintf (fio_err, "ERROR closing <%s>\n", mio[i].fnam);
			exit (-1);
		}
		exit (0);
	}

	if (ierr != 11)
	{
		fprintf (fio_err, "ERROR:reading 1st line of Data file\n");
		exit (-1);
	}

	dt->year = 1900 + myear;
	mdtodate(dt);			/* Convert month/day to Julian date */
	htoe(dt);			/* Convert human to epoch time */
	origin_time_init = dt->epoch;

	/* Initialize origin and origerr structures */

	origin->lat	= locator_params->lat_init;
	origin->lon	= locator_params->lon_init;
	origin->depth	= locator_params->depth_init;
	origerr->sdobs	= -1.0;
	origerr->smajax	= -1.0;
	origerr->sminax	= -1.0;
	origerr->strike	= -1.0;
	origerr->stime	= -1.0;
	origerr->sdepth	= -1.0;
	origerr->sxx	= -1.0;
	origerr->sxz	= -1.0;
	origerr->syz	= -1.0;
	origerr->syy	= -1.0;
	origerr->szz	= -1.0;
	origerr->sxy	= -1.0;
	origerr->stt	= -1.0;
	origerr->stx	= -1.0;
	origerr->sty	= -1.0;
	origerr->stz	= -1.0;

	/* Allocate space for arrival, assoc and locator_errors structures */

	arrival		= UALLOC(Arrival,num_data);
	assoc		= UALLOC(Assoc,num_data);
	locator_errors	= UALLOC(Locator_errors,num_data);

	assoc[0].orid = 90000001;
	locator_params->fixing_depth = locator_params->depth_init;

	/* Read detection info, one detection at a time */

	for (num_obs = 0, n = 0, ierr = 7; ierr == 7  && n < num_data; n++)
	{
		ierr = fscanf (fio, "%8d %6s %8s %4s%2s%f%f%*[^\n]",
				&arrival_id, station_name, phase_type,
				data_type, arrival_type, &obs_data,
				&std_dev_data);
		if (ierr != 7)
		{
			fprintf (fio_err, "ERROR:Phase data \n");
			exit (-1);
		}

		if (! strncmp(data_type, "t", 1))
		{
			if (n != 0) ++num_obs;
			arrival[num_obs].time	 = origin_time_init + obs_data;
			arrival[num_obs].azimuth = -1.0;
			arrival[num_obs].slow 	 = -1.0;
			arrival[num_obs].deltim	 = std_dev_data;
			strcpy(assoc[num_obs].timedef, arrival_type);
		}
		else if (! strncmp(data_type, "a", 1))
		{
			arrival[num_obs].azimuth = obs_data;
			arrival[num_obs].delaz	 = std_dev_data;
			strcpy(assoc[num_obs].azdef, arrival_type);
		}
		else if (! strncmp(data_type, "s", 1))
		{
			arrival[num_obs].slow	 = obs_data;
			arrival[num_obs].delslo	 = std_dev_data;
			strcpy(assoc[num_obs].slodef, arrival_type);
		}
		arrival[num_obs].arid = arrival_id;
		assoc[num_obs].arid   = arrival_id;
		strcpy(arrival[num_obs].sta, station_name);
		strcpy(assoc[num_obs].phase, phase_type);

	}
	++num_obs;		/* Number of associations */

	if (n >= MAXDATA)
		fprintf (fio_err, "%s (%d), %s\n",
			"Number of data greater than dimension",
			MAXDATA, " remaining Data ignored!");

	fscanf (fio, "\n");	/* Read a blank line */

	/*
	 * Execute location using travel-time tables, stations and detections 
	 * already read.  Send input data to LocSAT:
	 */

	status = locate_event (newnet, sites, num_sta, arrival, assoc,
			       origin, origerr, locator_params, locator_errors,
			       num_obs);

	dt->epoch = origin->time;
	etoh(dt);		/* Convert epoch time back to human time */
	myear = dt->year - 1900;

	/* Write out new location to file, 'new.out' */

	fio = fopen ("new.out", "a");

	fprintf (fio, "%2d%2d%2d %2d%2d%6.2f%9.4f%10.4f%7.2f%7.2f%3d\n",
			myear, dt->month, dt->day, dt->hour, dt->minute,
			dt->second, origin->lat, origin->lon, origin->depth,
			event_mag, num_data);

	for (n = 0; n < num_obs; n++)
	{
		fprintf (fio, "%8d %-6s %-8s t   %2s%14.3f%8.3f\n",
				arrival[n].arid, arrival[n].sta,
				assoc[n].phase, assoc[n].timedef,
				arrival[n].time - origin->time,
				arrival[n].deltim);
		if (arrival[n].azimuth != -1.0)
			fprintf (fio, "%8d %-6s %-8s a   %2s%14.3f%8.3f\n",
				 arrival[n].arid, arrival[n].sta,
				 assoc[n].phase, assoc[n].azdef,
				 arrival[n].azimuth, arrival[n].delaz);
		if (arrival[n].slow != -1.0)
			fprintf (fio, "%8d %-6s %-8s s   %2s%14.3f%8.3f\n",
				 arrival[n].arid, arrival[n].sta,
				 assoc[n].phase, assoc[n].slodef,
				 arrival[n].slow, arrival[n].delslo);
	}
	fprintf (fio, "\n");	/* Just write a blank line to seperate events */

	fclose (fio);

	UFREE(arrival);
	UFREE(assoc);
	UFREE(locator_errors);

	goto again;		/* End of location loop */

}


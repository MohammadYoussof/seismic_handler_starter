
/* file seismics.h
 *      ==========
 *
 * version 11, 22-May-2005
 *
 * header file of seismics.c (seismic utilities)
 * K. Stammler, 25-Mar-93
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


/* error codes */
#define SIE_OFFSET 4300
#define SIE_NOCONVERG     (SIE_OFFSET+1)     /* no convergence */
#define SIE_MAGN_DIST_OOR (SIE_OFFSET+2)     /* distance out of range */
#define SIE_VALUE_OOR     (SIE_OFFSET+3)     /* value out of range */
#define SIE_ILL_MAGN_TYPE (SIE_OFFSET+4)     /* illegal magnitude type */
#define SIE_ILL_EVLIST    (SIE_OFFSET+5)     /* illegal event list */
#define SIE_OPEN_READ     (SIE_OFFSET+6)     /* error opening file for reading*/
#define SIE_TOO_MANY_EV   (SIE_OFFSET+7)     /* too many events in wdw */
#define SIE_MORE_MATCHES  (SIE_OFFSET+8)     /* more than 1 event matches */
#define SIE_NO_EV_FOUND   (SIE_OFFSET+9)     /* no event found */
#define SIE_READ_TABLE    (SIE_OFFSET+10)    /* error reading table */
#define SIE_OPEN_TABLE    (SIE_OFFSET+11)    /* error opening table */
#define SIE_UNKNOWN_TABLE (SIE_OFFSET+12)    /* unknown table name */


/* constants */

/* magnitude determination */
#define SIC_MAGN_MB 0
#define SIC_MAGN_MS_PLAIN 1
#define SIC_MAGN_MS_C_NA 2
#define SIC_MAGN_MS_C_EU 3
#define SIC_MAGN_MS_C_O 4
#define SIC_MAGN_MS_O 5
#define SIC_MAGN_ML 6


/*---------------------------------------------------------------------------*/


void si_fit_distance( char phase[], float slowness, float depth,
	float tol, float *distance, STATUS *status );

/* fits distance from slowness and depth
 *
 * parameters of routine
 * char       phase[];      input; phase name
 * float      slowness;     input; slowness in sec/deg
 * float      depth;        input; depth in km
 * float      tol;          input; tolerance in slowness
 * float      *distance     output; distance in deg
 */


/*---------------------------------------------------------------------------*/


float si_magn_mb( float ampl, float period, float distance, STATUS *status );

/* Returns magnitude mb.  Passed amplitude and period must origin from a
 * Z component.
 *
 * parameters of routine
 * float      ampl;        input; amplitude in nm
 * float      period;      input; amplitude in sec
 * float      distance;    input; epicentral distance in deg
 *                         returns value of mb
 */


/*---------------------------------------------------------------------------*/


float si_magn_ms_plain( float ampl, float period, float distance,
	STATUS *status );

/* returns MS magnitude, using Prague formula
 *
 * parameters of routine
 * float      ampl;        input; amplitude in nm
 * float      period;      input; amplitude in sec
 * float      distance;    input; epicentral distance in deg
 * STATUS     *status;     output; return status
 *                         returns value of MS
 */


/*---------------------------------------------------------------------------*/


float si_magn_ms_m_b( int type, float ampl, float period, float distance,
	STATUS *status );

/* returns MS magnitude, using Marshall-Basham formula
 *
 * parameters of routine
 * int        type;        input; path type
 * float      ampl;        input; amplitude in nm
 * float      period;      input; amplitude in sec
 * float      distance;    input; epicentral distance in deg
 * STATUS     *status;     output; return status
 *                         returns value of MS
 */


/*---------------------------------------------------------------------------*/


float si_magn_ml( float ampl, float distance, STATUS *status );

/* Returns magnitude ml.  Amplitude and period must be read from a WOOD-ANDERSON
 * simulated seismogram, horizontal component.
 *
 * parameters of routine
 * float      ampl;        input; amplitude of WOOD-ANDERSON simulation
 * float      distance;    input; epicentral distance in km
 * STATUS     *status;     output; return status
 *                         returns value of ml
 */


/*---------------------------------------------------------------------------*/


void si_ext_location( float lat, float lon, float depth, char refstation[],
	float *distance, float *azimuth, float *p_slowness, STATUS *status );

/* Returns distance, back-azimuth & P-slowness (if possible) of given
 * hypocenter to refstation.
 *
 * parameters of routine
 * float      lat, lon;       input; epicenter of event;
 * float      depth;          input; event depth in km
 * char       refstation[];   input; name of receiving station
 * float      *distance;      output; distance in deg
 * float      *azimuth;       output; back-azimuth in deg
 * float      *p_slowness;    output; P-slowness or 0.0
 * STATUS     *status;        output; return status
 */


/*---------------------------------------------------------------------------*/


void si_get_location( float distance, float azimuth, char refstation[],
	float *lat, float *lon, int *ferid, char fername[], STATUS *status );

/* computes location from given distance and azimuth relative to given
 * reference station.  'fername' must have minimum length BC_LINELTH
 *
 * parameters of routine
 * float     distance;       input; distance in deg
 * float     azimuth;        input; back-azimuth in deg
 * char      refstation[];   input; name of reference station
 * float     *lat, *lon;     output; computed source location
 * int       *ferid;         output; Flinn-Engdahl region number
 * char      fername[];      output; name of Flinn-Engdahl region(>= BC_LINELTH)
 * STATUS    *status;        output; return status
 */


/*---------------------------------------------------------------------------*/


TSyBoolean si_is_first_onset( char mode[], char phase[] );

/* Returns TRUE if 'phase' is a first onset phase.
 *
 * parameters of routine
 * char       mode[];       input; 'tele' or not
 * char       phase[];      input; name of phase
 *                          returns TRUE if phase is first onset
 */


/*---------------------------------------------------------------------------*/


void si_match_location( char type[], char phase[], char onset[], char station[],
	float slowness, float azimuth, TSyBoolean newlist, float *lat, float *lon,
	float *dep, char origin[], char infsrc[], TSyStatus *status );

/* Finds matching locations in external event lists
 *
 * parameters of routine
 * char       type[];      input; which event list
 * char       phase[];     input; name of phase
 * char       onset[];     input; onset time of phase
 * char       station[];   input; station of phase reading
 * float      slowness;    input; slowness of phase
 * float      azimuth;     input; azimuth of phase
 * TSyBoolean newlist;     input; retrieve new event list or use old one
 * float      *lat;        output; latitude found
 * float      *lon;        output; longitude found
 * float      *dep;        output; depth found
 * char       origin[];    output; origin time
 * char       infsrc[];    output; information source
 * TSyStatus  *status;     output; return status
 */

/*---------------------------------------------------------------------------*/


void si_identify_phase( char onset[], char station[],
	float slowness, float azimuth, char phase[], float *lat, float *lon,
	float *dep, char origin[], char infosrc[], TSyStatus *status );

/* identifies phase from onset time using external event lists
 *
 * parameters of routine
 * char       onset[];    input; onset time of phase
 * char       station[];  input; station name
 * float      slowness;   input; slowness of phase
 * float      azimuth;    input; back azimuth of phase
 * char       phase[];    output; phase name
 * float      *lat;       output; epicenter latitude
 * float      *lon;       output; epicenter longitude
 * float      *dep;       output; source depth
 * char       origin[];   output; origin time
 * char       infosrc[];  output; information source (which epi list)
 * TSyStatus  *status;    output; return status
 */


/*---------------------------------------------------------------------------*/


void si_read_table( char fname[], char tablename[], TSyStatus *status );

/* Read a table from a file, e.g. sigma-table for ml
 *
 * parameters of routine
 * char       fname[];     input; name of input file
 * char       tablename[]; input; name of table
 * TSyStatus  *status;     output; return status
 */


/*---------------------------------------------------------------------------*/


/* file earthloc.h
 *      ==========
 *
 * version 5, 22-May-2006
 *
 * header file of module earthloc.c
 * K. Stammler, 23-May-92
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



/* error numbers */

#define MBE_OFFSET 3300
#define MBE_ILLLAT    (MBE_OFFSET+1)   /* illegal latitude */
#define MBE_ILLLON    (MBE_OFFSET+2)   /* illegal longitude */
#define MBE_OPNIDX    (MBE_OFFSET+3)   /* error opening index file */
#define MBE_EOF       (MBE_OFFSET+4)   /* enf of file found */
#define MBE_OPNNAM    (MBE_OFFSET+5)   /* error opening name file */
#define MBE_SHORTSTR  (MBE_OFFSET+6)   /* output string too short */
#define MBE_OPNCORR   (MBE_OFFSET+7)   /* error opening correction file */
#define MBE_RDCORR    (MBE_OFFSET+8)   /* error reading correction file */
#define MBE_EMPTYLIST (MBE_OFFSET+9)   /* correction list is empty */




/* prototypes */
/*---------------------------------------------------------------------*/


void mb_locdiff( double slat, double slon, double elat, double elon,
	double *distance, double *azim, double *bazim );

/* returns distance and azimuth in degrees of two locations on
 * earth
 *
 * parameters of routine
 * double     slat, slon;    input; latitude and longitude of station
 * double     elat, elon;    input; latitude and longitude of epicentre
 * double     *distance;     output; distance in degrees
 * double     *azim;         output; azimuth in degrees
 * double     *bazim;        output; back-azimuth in degrees
 */


/*---------------------------------------------------------------------*/


void mb_ferindex( float lat, float lon, int *feridx, STATUS *status );

/* returns FER index from given location (lat,lon) in degrees
 *
 * parameters of routine
 * float      lat, lon;     input; location in degrees
 */


/*---------------------------------------------------------------------*/


void mb_fername( int ferindex, int maxlth, char fername[], STATUS *status );

/* returns name of FER
 *
 * parameters of routine
 * int        ferindex;       input; FER index
 * char       fername[];      output; FER name
 * STATUS     *status;        output; return status
 */


/*---------------------------------------------------------------------*/


void mb_getlocname( float lat, float lon, int maxlth,
	char name[], STATUS *status );

/* returns FER name of location (lat,lon)
 *
 * parameters of routine
 * float      lat, lon;       input; location in degrees
 * int        maxlth;         input; maximum length of output string
 * char       name[];         output; FER name
 * STATUS     *status;        output; return status
 */


/*---------------------------------------------------------------------*/


void mb_setindexfile( char name[], STATUS *status );

/* tells where to find the FER index file
 *
 * parameters of routine
 * char       name[];     input; name (and path) of FER index file
 * STATUS     *status;    output; return status
 */


/*---------------------------------------------------------------------*/


void mb_setfernamefile( char name[], STATUS *status );

/* tells where to find the FER name file
 *
 * parameters of routine
 * char       name[];     input; name (and path) of FER name file
 * STATUS     *status;    output; return status
 */


/*---------------------------------------------------------------------*/


void mb_spherediff( float slat, float slon, float elat, float elon,
	float *dist, float *bazim );

/* computes difference and azimuth of two locations (slat,slon)
 * and (elat,elon)
 *
 * parameters of routine
 * float      slat, slon;    input; (station) location in degrees
 * float      elat, elon;    input; (epicentre) location in degrees
 * float      *dist;         output; distance in degrees
 * float      *bazim;        output; (back-)azimuth in degrees
 */


/*---------------------------------------------------------------------*/


void mb_sphereadd( float slat, float slon, float dist, float bazim,
	float *elat, float *elon );

/* computes new location from input location (slat,slon) and distance
 * "dist" and azimuth "azim"
 *
 * parameters of routine
 * float      slat, slon;    input; (station) input location in degrees
 * float      dist;          input; distance in degrees
 * float      bazim;         input; (back-)azimuth in degrees
 * float      *elat, *elon;  output; (epicentre) location in degrees
 */


/*---------------------------------------------------------------------*/


void mb_statcorr( float slo_in, float az_in, float *slo_out,
	float *az_out, float *slo_dist, float *az_dist, STATUS *status );

/* applies station correction for slowness and azimuth.  The
 * corrections are listed in a correction file mbv_corrfile.
 * The difference vector of the next correction point is
 * added to the input data
 *
 * parameters of routine
 * float      slo_in;    input; input slowness in sec/deg
 * float      az_in;     input; input azimuth in deg
 * float      *slo_out;  output; corrected slowness in sec/deg
 * float      *az_out;   output; corrected azimuth in deg
 * float      *slo_dist; output; slowness distance to the next correction
 * float      *az_dist;  output; azimuth distance to the next correction
 * STATUS     *status;   output; return status
 */


/*---------------------------------------------------------------------*/


void mb_setcorrfilename( char name[], STATUS *status );

/* tells where to find the slowness and azimuth correction file
 *
 * parameters of routine
 * char       name[];     input; name (and path) of correction file
 * STATUS     *status;    output; return status
 */


/*---------------------------------------------------------------------*/


void mb_deg_to_km( int listlth, double lat[], double lon[],
	float x[], float y[] );

/* computes x-y coordinates of points on surface of earth given in lat&lon.
 * The reference point is assumed to be given as last element of the list.
 *
 * parameters of routine
 * int        listlth;       length of following arrays
 * double     lat[], lon[];  input; given locations in deg
 * float      x[], y[];      output; returned locations in km
 */


/*---------------------------------------------------------------------*/

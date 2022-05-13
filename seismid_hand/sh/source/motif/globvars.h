
/* file globvars.h
 *      ==========
 *
 * version 32, 22-May-2006
 *
 * global parameter set of shm
 * K. Stammler, 24-Jun-93
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


#ifndef __GLOBVARS
#define __GLOBVARS


#include "../basecnst.h"
#include "../shconst.h"



/* if included by main program this should declare and initialize
 * the variable set
 */
#ifdef MAINPROG

SHT_GLOBAL_SET shv_global = /* initialization follows */
	{
	"pP,sP,pS,sS",                   /* list of depth phases */
	"P,S,pP,pS,sP,sS,ScS,PcP,PP,SS", /* theoretical traces to be computed */
	"S-P,PP-P,Pg-Pn",                /* list of difference phases */
	8.0,                             /* close phase resolution */
	7,                               /* min. width of drag box */
	"/home/ersn02/klaus/sh/filter",  /* filter directory */
	"/home/ersn02/seismo",           /* event directory */
	"/home/ersn02/klaus",            /* GSE directory */
	"/home/ersn02/klaus",            /* GSE2 directory */
	"/home/ersn02/klaus",            /* AH directory */
	"/home/ersn02/klaus",            /* Q directory */
	"/home/ersn02/klaus/evid/",      /* event ID database */
	"/home/ersn02/klaus/evtout/",    /* event info output path */
	"/home/ersn02/klaus",            /* input path for SEED data */
	FALSE,                           /* from bottom to top */
	200,                             /* double click time in ms */
	10.0,                            /* trace zoom base */
	3.333333333,                     /* trace zoom exponent factor */
	{1.0,0.0,0.0},                   /* marker color */
	{0.0,1.0,0.0},                   /* theo color */
	{1.0,0.5,0.5},                   /* auto color */
	{0.0,0.0,1.0},                   /* cursor color */
	"CENTER",                        /* reference station */
	TRUE,                            /* autopick: take first amplitude */
	"textedit %s",                   /* call to system text editor */
	2.5, 2.5,                        /* calibration window width & height */
	10.0, 0.5,                       /* calibration azimuth & slowness grid */
	TRUE,                            /* automatic scaling */
	TRUE,                            /* use recursive filters */
	"",                              /* analyst's initials */
	FALSE,                           /* prompt for analyst's name */
	"",                              /* motif log file of actions */
	60.0,                            /* max. seconds betw. phase and ampl. */
	FALSE,                           /* reverse xor's */
	4096,                            /* max. drawlth in XDrawLines */
	1,                               /* default quality */
	10,                              /* drag box rubber val */
	"",                              /* default filter after read new */
	0,                               /* default phase type (other) */
	0,                               /* default phase flags */
	0,                               /* default depth type (undefined) */
	0.0,                             /* default depth in km */
	0,                               /* default location quality (undefined) */
	5,                               /* max cursor form (MGC_WAVEFORM_HILB) */
	"",                              /* check event procedure */
	"screendump.csh",                /* screendump routine */
	"ev2view",                       /* ev view procedure */
	"b3sn06",                        /* request remote host */
	FALSE,                           /* full phase names */
	"SZGRF",                         /* default info source */
	1.0,                             /* move window step size */
	20,                              /* top margin in pixels */
	"beam",                          /* autopick phase name */
	593,                             /* drawing area height in pixel */
	896,                             /* drawing area width in pixel */
	10.0,                            /* area zoom base */
	25.0,                            /* area zoom exponent */
	"undefined",                     /* routine to reformat data */
	"undefined",                     /* postprocessing routine for evt files */
	TRUE,                            /* use own accelerators */
	' '                              /* filter type */
	};

/* otherwise just define it external */
#else

extern SHT_GLOBAL_SET shv_global;

#endif


#endif /* __GLOBVARS */

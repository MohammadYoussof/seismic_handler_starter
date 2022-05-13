
/* file PTRAVTIM.C
 *      ==========
 *
 * version 15, 22-May-2006
 *
 * reads travel time tables (TTT-format)
 * K. Stammler, 18-Dec-91
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
#include <ctype.h>
#include "basecnst.h"
#include BC_SYSBASE
#include "erusrdef.h"
#include "ptusrdef.h"
#include "pterrors.h"


/* constants */
#define MAXDEPTHSTEPS 20
	/* maximum number of depth entries per distance */
#define MAXDISTSTEPS 200
	/* maximum number of distances per file (for pth_getdistline) */
#define TTTEXT ".TTT"
	/* extension of TTT file */
#define MAXDISTPTS 300
	/* maximum number of distances in table file */


/* global variables */
static char    ptv_tabdir[BC_FILELTH+1];   /* table directory */
static float   ptv_delta_dist=1.0;         /* distance step */


/* prototypes of local routines */
static void pth_gettablename( char phase[], char fname[],
	STATUS *status );
static void pth_getdepthline( char phase[], float dist,
	int *steps, float dp[], float ttim[], STATUS *status );
static void pth_getdistline( char phase[], float depth,
	int *steps, float ds[], float ttim[], STATUS *status );


/*-------------------------------------------------------------------*/



void pt_settabledir( char tabdir[], STATUS *status )

/* sets directory of table files
 *
 * parameters of routine
 * char       tabdir[];    input; new directory
 * STATUS     *status;     output; return status
 */
{
	/* executable code */

	if  (strlen(tabdir) > BC_FILELTH)  {
		*status = PTE_STROVFL;
		return;
	} /*endif*/
	strcpy( ptv_tabdir, tabdir );

} /* end of pt_settabledir */



/*-------------------------------------------------------------------*/



float pt_travel( char phase[], float distance, float depth,
	STATUS *status )

/* returns travel time in sec of phase "phase"
 *
 * parameters of routine
 * char       phase[];      input; phase to compute travel time
 * float      distance;     input; distance of event in degrees
 * float      depth;        input; depth of event in km
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	char     tabfil[BC_FILELTH+1];  /* name of table file */
	char     str[BC_LONGSTRLTH+1];  /* scratch string */
	FILE     *tf;                   /* pointer to table file */
	int      i;                     /* counter */
	float    mindist, maxdist;      /* distance bounds */
	unsigned depthsteps;            /* number of depth entries */
	float    dep[MAXDEPTHSTEPS];    /* depth values */
	unsigned depidx;                /* depth index */
	float    ttlo[MAXDEPTHSTEPS];   /* lower travel times */
	float    tthi[MAXDEPTHSTEPS];   /* upper travel times */
	float    prevdist, currdist;    /* previous & current distance */
	float    deriv_dep, deriv_dist; /* derivations in depth & distance */
	float    delta_dep, delta_dist; /* difference vectors */
	float    ttime;                 /* travel time in sec */

	/* executable code */

	/* get name of table file and open it */
	pth_gettablename( phase, tabfil, status );
	if  (Severe(status))  return 0.;
	tf = sy_fopen( tabfil, "r" );
	if  (tf == NULL)  {
		*status = PTE_FOPENRD;
		err_setcontext( " ## file " ); err_setcontext( tabfil );
		return 0.;
	} /*endif*/

	/* read off comment lines and check ID */
	do  {
		fgets(str,BC_LONGSTRLTH,tf);
	}  while  (*str == '!');
	i = (int)strlen(str) - 1;
	if  (str[i] == '\n')  str[i] = '\0';
	if  (strcmp(str,"TTT") != 0)  {
		*status = PTE_NOTTTFILE;
		sy_fclose( tf );
		err_setcontext( " ## file " ); err_setcontext( tabfil );
		return 0.;
	} /*endif*/

	/* read distance bounds and depth steps */
	fgets( str, BC_LONGSTRLTH, tf );  /* comment */
	fgets( str, BC_LONGSTRLTH, tf );
	if  (sscanf(str,"%f %f",&mindist,&maxdist) != 2)  {
		*status = PTE_FRDERR;
		err_setcontext( " ## file " ); err_setcontext( tabfil );
		return 0.;
	} /*endif*/
	fgets( str, BC_LONGSTRLTH, tf );  /* comment */
	fscanf( tf, "%d\n", &depthsteps );
	if  (depthsteps > MAXDEPTHSTEPS)  {
		*status = PTE_STEPOVFL;
		err_setcontext( " ## steps " ); err_setcontext_l( depthsteps );
		sy_fclose( tf );
		return 0.;
	} /*endif*/
	for  (i=0; i<depthsteps; i++)
		fscanf( tf, "%f\n", dep+i );

	/* check bounds */
	if  (distance < mindist || distance >= maxdist)  {
		*status = PTE_DISTOOR;
		err_setcontext( " ## phase " ); err_setcontext( phase );
		err_setcontext( ", distance " ); err_setcontext_r( distance );
		sy_fclose( tf );
		return 0.;
	} else if  (depth < dep[0] || depth > dep[depthsteps-1])  {
		*status = PTE_DEPTHOOR;
		err_setcontext( " ## phase " ); err_setcontext( phase );
		err_setcontext( ", depth " ); err_setcontext_r( depth );
		sy_fclose( tf );
		return 0.;
	} /*endif*/

	/* find upper & lower travel times */
	fscanf( tf, "%f\n", &prevdist );
	for  (i=0; i<depthsteps; i++)  fscanf( tf, "%f\n", ttlo+i );
	FOREVER  {
		fscanf( tf, "%f\n", &currdist );
		if  (currdist <= prevdist)  {
			*status = PTE_TTTERROR;
			err_setcontext( " ## file " ); err_setcontext( tabfil );
			return 0.;
		} /*endif*/
		for  (i=0; i<depthsteps; i++)  fscanf( tf, "%f\n", tthi+i );
		if  (currdist > distance)  break;
		for  (i=0; i<depthsteps; i++)  ttlo[i] = tthi[i];
		prevdist = currdist;
	} /*endfor*/

	/* close table file */
	sy_fclose( tf );

	/* find depth index */
	depidx = 1;
	while  (dep[depidx] < depth)
		depidx++;

	/* check for zero times */
	if  (ttlo[depidx] == 0.0 && prevdist > 0.0)  *status = PTE_ZEROTIME;
	if  (ttlo[depidx-1] == 0.0 && prevdist > 0.0)  *status = PTE_ZEROTIME;
	if  (tthi[depidx] == 0.0)  *status = PTE_ZEROTIME;
	if  (tthi[depidx-1] == 0.0)  *status = PTE_ZEROTIME;
	if  (Severe(status))  return 0.0;

	/* linear interpolation in two dimensions */
	/* gradient of travel time */
	deriv_dep = (ttlo[depidx] - ttlo[depidx-1]) /
		(dep[depidx] - dep[depidx-1]);
	deriv_dist = (tthi[depidx-1] - ttlo[depidx-1]) /
		(currdist - prevdist);
	/* difference vector */
	delta_dep = depth - dep[depidx-1];
	delta_dist = distance - prevdist;
	/* Taylor of 1. order */
	ttime = ttlo[depidx-1] + deriv_dep*delta_dep +
		deriv_dist*delta_dist;

	ptv_delta_dist = currdist - prevdist;

	return ttime;

} /* end of pt_travel */



/*-------------------------------------------------------------------*/



float pt_slowness( char phase[], float distance, float depth,
	STATUS *status )

/* returns slowness of "phase"
 *
 * parameters of routine
 * char       phase[];   input; name of phase
 * float      distance;  input; distance of source in degrees
 * float      depth;     input; depth of source in km
 * STATUS     *status;   output; return status
 *                       returns slowness of phase in sec/degree
 */
{
	/* local variables */
	float    lo_dist;          /* lower distance */
	float    curr_delta_dist;  /* distance steps at beginning of routine */

	/* executable code */

	curr_delta_dist = ptv_delta_dist;
	lo_dist = (distance < (curr_delta_dist/2.0)) ? 0.0 :
		distance - curr_delta_dist/2.0;

	return  ((pt_travel(phase,lo_dist+curr_delta_dist,depth,status) -
		pt_travel(phase,lo_dist,depth,status)) / curr_delta_dist);
	

} /* end of pt_slowness */



/*-------------------------------------------------------------------*/



float pt_distance( char phase[], float slowness, float depth,
	STATUS *status )

/* determines distance in degrees of a phase from slowness in sec/deg
 *
 * parameters of routine
 * char       phase[];      input; phase name
 * float      slowness;     input; slowness in sec/deg
 * float      depth;        input; depth in km
 * STATUS     *status;      output; return status
 *                          returns distance in degrees or zero
 */
{
	/* local variables */
	char     tabfil[BC_FILELTH+1];     /* name of table file */
	FILE     *tf;                      /* pointer to table file */
	char     str[BC_LINELTH+1];        /* scratch string */
	int      i, d;                     /* counters */
	float    mindist, maxdist;         /* distance bounds */
	unsigned depthsteps;               /* depth steps */
	float    dep[MAXDEPTHSTEPS];       /* depth values */
	float    ctrav[MAXDEPTHSTEPS];     /* current travel times */
	float    trav[MAXDISTPTS];         /* interpolated travel time points */
	float    slow[MAXDISTPTS];         /* slowness values */
	float    dist[MAXDISTPTS];         /* distance values */
	unsigned depidx;                   /* depth index */
	float    loweight, hiweight;       /* weights of points */

	/* executable code */

	/* get name of table file and open it */
	pth_gettablename( phase, tabfil, status );
	if  (Severe(status))  return 0.;
	tf = sy_fopen( tabfil, "r" );
	if  (tf == NULL)  {
		*status = PTE_FOPENRD;
		err_setcontext( " ## file " ); err_setcontext( tabfil );
		return 0.;
	} /*endif*/

	/* read off comment lines and check ID */
	do  {
		fgets(str,BC_LONGSTRLTH,tf);
	}  while  (*str == '!');
	i = (int)strlen(str) - 1;
	if  (str[i] == '\n')  str[i] = '\0';
	if  (strcmp(str,"TTT") != 0)  {    /* change to SNT in slowness files */
		*status = PTE_NOTTTFILE;
		sy_fclose( tf );
		err_setcontext( " ## file " ); err_setcontext( tabfil );
		return 0.;
	} /*endif*/

	/* read distance bounds and depth steps */
	fgets( str, BC_LONGSTRLTH, tf );  /* comment */
	fgets( str, BC_LONGSTRLTH, tf );
	if  (sscanf(str,"%f %f",&mindist,&maxdist) != 2)  {
		*status = PTE_FRDERR;
		err_setcontext( " ## file " ); err_setcontext( tabfil );
		return 0.;
	} /*endif*/
	fgets( str, BC_LONGSTRLTH, tf );  /* comment */
	fscanf( tf, "%d\n", &depthsteps );
	if  (depthsteps > MAXDEPTHSTEPS)  {
		*status = PTE_STEPOVFL;
		err_setcontext( " ## steps " ); err_setcontext_l( depthsteps );
		sy_fclose( tf );
		return 0.;
	} /*endif*/
	for  (i=0; i<depthsteps; i++)
		fscanf( tf, "%f\n", dep+i );

	/* check bounds */
	if  (depth < dep[0] || depth > dep[depthsteps-1])  {
		*status = PTE_DEPTHOOR;
		err_setcontext( " ## phase " ); err_setcontext( phase );
		err_setcontext( ", depth " ); err_setcontext_r( depth );
		sy_fclose( tf );
		return 0.;
	} /*endif*/

	/* find depth weights */
	depidx = 0;
	while (dep[depidx] < depth)  {
		depidx++;
		if  (depidx == depthsteps)  break;
	} /*endwhile*/
	if  (depidx == 0)  {
		loweight = 0.0;
		hiweight = 1.0;
	} else if  (depidx == depthsteps)  {
		loweight = 1.0;
		hiweight = 0.0;
	} else {
		loweight = (dep[depidx]-depth)/(dep[depidx]-dep[depidx-1]);
		hiweight = 1.0 - loweight;
	} /*endif*/

	/* read and interpolate travel times according to depth */
	d = 0;
	do  {
		fscanf( tf, "%f\n", dist+d );
		for  (i=0; i<depthsteps; i++)  fscanf( tf, "%f\n", ctrav+i );
		if  (loweight == 0.0)  {
			trav[d] = ctrav[depidx];
		} else if  (hiweight == 0.0)  {
			trav[d] = ctrav[depidx-1];
		} else {
			trav[d] = loweight*ctrav[depidx-1] + hiweight*ctrav[depidx];
		} /*endif*/
	}  while(dist[d++] < maxdist);

	sy_fclose( tf );

	/* differentiate travel times */
	slow[0] = (trav[1]-trav[0])/(dist[1]-dist[0]);
	slow[d-1] = (trav[d-1]-trav[d-2])/(dist[d-1]-dist[d-2]);
	for  (i=1; i<d-1; i++)
		slow[i] = (trav[i+1]-trav[i-1])/(dist[i+1]-dist[i-1]);

	/* find slowness value and return distance */
	i = 0;
	while  (slow[i] > slowness || slow[i] == 0.0)  {
		i++;
		if  (i == d)  {
			*status = PTE_SLOWOOR;
			return dist[d-1];
		} /*endif*/
	} /*endwhile*/
	if  (i == 0)  return dist[0];
	loweight = (slow[i]-slowness)/(slow[i]-slow[i-1]);
	hiweight = 1.0 - loweight;
	return  (loweight*dist[i-1] + hiweight*dist[i]);

} /* end of pt_distance */



/*-------------------------------------------------------------------*/



float pt_depth( char phase1[], char phase2[], float tdiff,
	float distance, STATUS *status )

/* fits depth to given time difference "tdiff" of two phases
 * "T(phase2)-T(phase1)" at a fixed distance
 *
 * parameters of routine
 * char       phase1[];       input; name of main phase
 * char       phase2[];       input; name of corresponding depth phase
 * float      tdiff;          input; time difference between phases (sec)
 * float      distance;       input; distance in deg
 * STATUS     *status;        output; return status
 *                            returns fitted depth in km
 */
{
	/* local variables */
	float    dp1[MAXDEPTHSTEPS];    /* depths of phase 1 */
	float    dp2[MAXDEPTHSTEPS];    /* depths of phase 2 */
	float    tt1[MAXDEPTHSTEPS];    /* travel times of phase 1 */
	float    tt2[MAXDEPTHSTEPS];    /* travel times of phase 2 */
	int      steps1, steps2;        /* number of depth steps */
	int      i;                     /* counter */
	float    grad;                  /* gradient of difference function */

	/* executable code */

	pth_getdepthline( phase1, distance, &steps1, dp1, tt1, status );
	if  (Severe(status))  return 0.0;
	pth_getdepthline( phase2, distance, &steps2, dp2, tt2, status );
	if  (Severe(status))  return 0.0;

	/* compare whether both files have the same depth steps */
	if  (steps1 != steps2)  {*status = PTE_DIFFSTEPS; return 0.0;}
	for  (i=0; i<steps2; i++)
		if  (dp1[i] != dp2[i])
			{*status=PTE_DIFFSTEPS; return 0.0;}

	/* take travel time difference */
	for  (i=0; i<steps2; i++)  {
		if  (tt1[i] == 0.0)  {tt2[i] = -1.0; continue;}
		if  (tt2[i] == 0.0)  {tt2[i] = -1.0; continue;}
		tt2[i] -= tt1[i];
	} /*endfor*/

	/* find given time difference */
	i = 0;
	while  (tt2[i] == -1.0)
		if  (++i == steps2)  {
			*status = PTE_DISTOOR;
			err_setcontext( " ## phase " );
			err_setcontext( phase1 );
			err_setcontext( ", distance " );
			err_setcontext_r( distance );
			return 0.;
		} /*endif*/
	if  (tt2[i] > tdiff || i == (steps2-1))  {
		*status = PTE_DEPTHFIT;
		return 0.;
	} /*endif*/
	while  (tt2[++i] < tdiff)
		if  (i == steps2-1 || tt2[i] == -1.0)  {
			*status = PTE_DEPTHFIT;
			return 0.;
		} /*endif*/

	/* now the given time difference lies between tt2[i] and tt2[i-1] */
	if  (tt2[i]-tt2[i-1] < 1.0e-4)  return ((dp2[i]+dp2[i-1])/2.0);
	grad = (dp2[i] - dp2[i-1]) / (tt2[i] - tt2[i-1]);
	return (dp2[i-1] + grad*(tdiff-tt2[i-1]));

} /* end of pt_depth */



/*-------------------------------------------------------------------*/



float pt_distance_pd( char phase1[], char phase2[], float tdiff,
	float depth, STATUS *status )

/* fits distance to given time difference "tdiff" of two phases
 * "T(phase2)-T(phase1)" at a fixed depth
 *
 * parameters of routine
 * char       phase1[];       input; name of first phase
 * char       phase2[];       input; name of second phase
 * float      tdiff;          input; time difference between phases (sec)
 * float      depth;          input; depth in km
 * STATUS     *status;        output; return status
 *                            returns fitted distance in deg
 */
{
	/* local variables */
	float    ds1[MAXDISTSTEPS];     /* distances of phase 1 */
	float    ds2[MAXDISTSTEPS];     /* distances of phase 2 */
	float    tt1[MAXDISTSTEPS];     /* travel times of phase 1 */
	float    tt2[MAXDISTSTEPS];     /* travel times of phase 2 */
	float    *dsp1, *dsp2;          /* pointers to start distance */
	float    *ptt1, *ptt2;          /* pointers to travel times */
	float    *dsp1_max, *dsp2_max;  /* maximum pointer values */
	int      steps1, steps2;        /* number of distance steps */
	int      steps;                 /* common length of travel time arrays */
	int      i;                     /* counter */
	float    grad;                  /* gradient of difference function */

	/* executable code */

	pth_getdistline( phase1, depth, &steps1, ds1, tt1, status );
	if  (Severe(status))  return 0.0;
	pth_getdistline( phase2, depth, &steps2, ds2, tt2, status );
	if  (Severe(status))  return 0.0;

	/* align distance indices */
	dsp1 = ds1; dsp1_max = ds1+steps1;
	dsp2 = ds2; dsp2_max = ds2+steps2;
	ptt1 = tt1;
	ptt2 = tt2;
	if  (*dsp1 < *dsp2)  {
		while  (*dsp1 < *dsp2)  {
			ptt1++;
			if  (++dsp1 == dsp1_max)  {
				*status = PTE_INCOMPTT;
				return 0.0;
			} /*endif*/
		} /*endwhile*/
		steps1 -= (int)(dsp1-ds1);
	} else {
		while  (*dsp2 < *dsp1)  {
			ptt2++;
			if  (++dsp2 == dsp2_max)  {
				*status = PTE_INCOMPTT;
				return 0.0;
			} /*endif*/
		} /*endwhile*/
		steps2 -= (int)(dsp2-ds2);
	} /*endif*/
	steps = (steps1 < steps2) ? steps1 : steps2;
	for  (i=0; i<steps; i++)
		if  (dsp1[i] != dsp2[i])
			{*status=PTE_DIFFSTEPS; return 0.0;}

	/* take travel time difference to ptt2 */
	for  (i=0; i<steps; i++)  {
		if  (ptt1[i] == 0.0)  {ptt2[i] = -1.0; continue;}
		if  (ptt2[i] == 0.0)  {ptt2[i] = -1.0; continue;}
		ptt2[i] -= ptt1[i];
	} /*endfor*/

	/* find given time difference */
	i = 0;
	while  (ptt2[i] == -1.0)
		if  (++i == steps)  {
			*status = PTE_INCOMPTT;
			err_setcontext( " ## phase 1 " );
			err_setcontext( phase1 );
			err_setcontext( ", phase 2 " );
			err_setcontext( phase2 );
			return 0.;
		} /*endif*/
	if  (ptt2[i] > tdiff || i == steps-1)  {
		*status = PTE_DISTFIT;
		return 0.;
	} /*endif*/
	while  (ptt2[++i] < tdiff)
		if  (i == steps-1 || ptt2[i] == -1.0)  {
			*status = PTE_DISTFIT;
			return 0.;
		} /*endif*/

	/* now the given time difference lies between ptt2[i] and ptt2[i-1] */
	if  (ptt2[i]-ptt2[i-1] < 1.0e-4)  return ((dsp2[i]+dsp2[i-1])/2.0);
	grad = (dsp2[i] - dsp2[i-1]) / (ptt2[i] - ptt2[i-1]);
	return (dsp2[i-1] + grad*(tdiff-ptt2[i-1]));

} /* end of pt_distance_pd */



/*-------------------------------------------------------------------*/



static void pth_gettablename( char phase[], char fname[],
	STATUS *status )

/* determines name of table file from phase
 *
 * parameters of routine
 * char       phase[];    input; name of phase
 * char       fname[];    output; table file
 */
{
	/* local variables */
	char     str[BC_FILELTH+1];   /* scratch */
	int      i, j;                /* counters */

	/* executable code */

	i = j = 0;
	do  {
		if  (j >= BC_FILELTH)  {*status = PTE_STROVFL; return;}
		if  (islower(phase[i]))  {
			str[j++] = 'V';
			str[j++] = Cap( phase[i] );
		} else {
			str[j++] = phase[i];
		} /*endif*/
	}  while (phase[i++] != '\0');

	if  (strlen(str)+strlen(ptv_tabdir)+4 > BC_FILELTH)  {
		*status = PTE_STROVFL;
		return;
	} /*endif*/
	strcpy( fname, ptv_tabdir );
	strcat( fname, str );
	strcat( fname, TTTEXT );

} /* end of pth_gettablename */



/*-------------------------------------------------------------------*/



static void pth_getdepthline( char phase[], float dist,
	int *steps, float dp[], float ttim[], STATUS *status )

/* returns travel times of all depths for a given phase at a
 * given distance
 *
 * parameters of routine
 * char       phase[];      input; phase name
 * float      dist;         input; distance in deg
 * int        *steps;       output; number of depth steps
 * float      dp[];         output; depth values (max length is MAXDEPTHSTEPS)
 * float      ttim[];       output; travel times (max length is MAXDEPTHSTEPS)
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	char     tabfil[BC_FILELTH+1];   /* name of phase file */
	FILE     *tf;                    /* pointer to table file */
	char     str[BC_LONGSTRLTH+1];   /* current line */
	int      i;                      /* counter */
	float    mindist, maxdist;       /* distance bounds */
	float    prevdist, currdist;     /* distance values */
	float    ttlo[MAXDEPTHSTEPS];    /* smaller travel times */
	float    tthi[MAXDEPTHSTEPS];    /* larger travel times */
	float    grad;                   /* gradient of travel times */
	float    diststep;               /* distance step */

	/* executable code */

	/* get name of table file and open it */
	pth_gettablename( phase, tabfil, status );
	if  (Severe(status))  return;
	tf = sy_fopen( tabfil, "r" );
	if  (tf == NULL)  {
		*status = PTE_FOPENRD;
		err_setcontext( " ## file " ); err_setcontext( tabfil );
		return;
	} /*endif*/

	/* read off comment lines and check ID */
	do  {
		fgets(str,BC_LONGSTRLTH,tf);
	}  while  (*str == '!');
	i = (int)strlen(str) - 1;
	if  (str[i] == '\n')  str[i] = '\0';
	if  (strcmp(str,"TTT") != 0)  {
		*status = PTE_NOTTTFILE;
		sy_fclose( tf );
		err_setcontext( " ## file " ); err_setcontext( tabfil );
		return;
	} /*endif*/

	/* read distance bounds and depth steps */
	fgets( str, BC_LONGSTRLTH, tf );  /* comment */
	fgets( str, BC_LONGSTRLTH, tf );
	if  (sscanf(str,"%f %f",&mindist,&maxdist) != 2)  {
		*status = PTE_FRDERR;
		err_setcontext( " ## file " ); err_setcontext( tabfil );
		return;
	} /*endif*/
	fgets( str, BC_LONGSTRLTH, tf );  /* comment */
	fscanf( tf, "%d\n", steps );
	if  (*steps > MAXDEPTHSTEPS)  {
		*status = PTE_STEPOVFL;
		err_setcontext( " ## steps " ); err_setcontext_l( *steps );
		sy_fclose( tf );
		return;
	} /*endif*/
	for  (i=0; i<(*steps); i++)
		fscanf( tf, "%f\n", dp+i );

	/* check bounds */
	if  (dist <= mindist || dist >= maxdist)  {
		*status = PTE_DISTOOR;
		err_setcontext( " ## phase " ); err_setcontext( phase );
		err_setcontext( ", distance " ); err_setcontext_r( dist );
		sy_fclose( tf );
		return;
	} /*endif*/

	/* find upper & lower travel times */
	fscanf( tf, "%f\n", &prevdist );
	for  (i=0; i<(*steps); i++)  fscanf( tf, "%f\n", ttlo+i );
	FOREVER  {
		fscanf( tf, "%f\n", &currdist );
		if  (currdist <= prevdist)  {
			*status = PTE_TTTERROR;
			err_setcontext( " ## file " ); err_setcontext( tabfil );
			sy_fclose( tf );
			return;
		} /*endif*/
		for  (i=0; i<(*steps); i++)  fscanf( tf, "%f\n", tthi+i );
		if  (currdist > dist)  break;
		for  (i=0; i<(*steps); i++)  ttlo[i] = tthi[i];
		prevdist = currdist;
	} /*endfor*/

	/* close table file */
	sy_fclose( tf );

	/* interpolate travel times */
	diststep = currdist - prevdist;
	for  (i=0; i<(*steps); i++)  {
		if  (ttlo[i] == 0.0)  {ttim[i] = 0.0; continue;}
		if  (tthi[i] == 0.0)  {ttim[i] = 0.0; continue;}
		grad = (tthi[i] - ttlo[i]) / diststep;
		ttim[i] = ttlo[i] + grad*(dist-prevdist);
	} /*endfor*/

} /* end of pth_getdepthline */



/*-------------------------------------------------------------------*/



static void pth_getdistline( char phase[], float depth,
	int *steps, float ds[], float ttim[], STATUS *status )

/* returns travel times of all distances for a given phase at a
 * given depth
 *
 * parameters of routine
 * char       phase[];      input; phase name
 * float      depth;        input; depth in km
 * int        *steps;       output; number of distance steps
 * float      ds[];         output; distance values (max length is MAXDISTSTEPS)
 * float      ttim[];       output; travel times (max length is MAXDISTSTEPS)
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	char     tabfil[BC_FILELTH+1];   /* name of phase file */
	FILE     *tf;                    /* pointer to table file */
	char     str[BC_LONGSTRLTH+1];   /* current line */
	int      i, j;                   /* counters */
	float    dp[MAXDEPTHSTEPS];      /* depth values */
	int      dpidx;                  /* depth index */
	float    weight;                 /* weight of dp[dpidx] */
	float    ttcur[MAXDEPTHSTEPS];   /* travel times for different depths */
	float    mindist, maxdist;       /* distance bounds */
	int      depth_steps;            /* number of depth steps */

	/* executable code */

	/* get name of table file and open it */
	pth_gettablename( phase, tabfil, status );
	if  (Severe(status))  return;
	tf = sy_fopen( tabfil, "r" );
	if  (tf == NULL)  {
		*status = PTE_FOPENRD;
		err_setcontext( " ## file " ); err_setcontext( tabfil );
		return;
	} /*endif*/

	/* read off comment lines and check ID */
	do  {
		fgets(str,BC_LONGSTRLTH,tf);
	}  while  (*str == '!');
	i = (int)strlen(str) - 1;
	if  (str[i] == '\n')  str[i] = '\0';
	if  (strcmp(str,"TTT") != 0)  {
		*status = PTE_NOTTTFILE;
		sy_fclose( tf );
		err_setcontext( " ## file " ); err_setcontext( tabfil );
		return;
	} /*endif*/

	/* read distance bounds and depth steps */
	fgets( str, BC_LONGSTRLTH, tf );  /* comment */
	fgets( str, BC_LONGSTRLTH, tf );
	if  (sscanf(str,"%f %f",&mindist,&maxdist) != 2)  {
		*status = PTE_FRDERR;
		err_setcontext( " ## file " ); err_setcontext( tabfil );
		return;
	} /*endif*/
	fgets( str, BC_LONGSTRLTH, tf );  /* comment */
	fscanf( tf, "%d\n", &depth_steps );
	if  (depth_steps > MAXDEPTHSTEPS)  {
		*status = PTE_STEPOVFL;
		err_setcontext( " ## depth steps " ); err_setcontext_l( depth_steps );
		sy_fclose( tf );
		return;
	} /*endif*/
	for  (i=0; i<(depth_steps); i++)
		fscanf( tf, "%f\n", dp+i );

	/* check bounds */
	if  (depth < dp[0] || depth > dp[(depth_steps)-1])  {
		*status = PTE_DEPTHOOR;
		err_setcontext( " ## phase " ); err_setcontext( phase );
		err_setcontext( ", depth " ); err_setcontext_r( depth );
		sy_fclose( tf );
		return;
	} /*endif*/

	/* find depth index, given depth must be between dp[dpidx] and dp[dpidx-1] */
	dpidx = 1;  /* index zero is not useful because of dpidx-1 */
	while  (dp[dpidx] < depth)
		if  (++dpidx == depth_steps)  {
			*status = PTE_BUG;
			sy_fclose( tf );
			return;
		} /*endif*/

	/* find weight "weight" of depth dp[dpidx], weight of dpidx-1 is 1-weight */
	weight = (depth - dp[dpidx-1]) / (dp[dpidx] - dp[dpidx-1]);

	/* read travel times */
	*steps = 0;
	do {
		fscanf( tf, "%f\n", ds+(*steps) );
		for  (j=0; j<(depth_steps); j++)  fscanf( tf, "%f\n", ttcur+j );
		ttim[*steps] = ttcur[dpidx]*weight + ttcur[dpidx-1]*(1.0-weight);
		if  (++(*steps) == MAXDISTSTEPS)  {
			*status = PTE_STEPOVFL;
			sy_fclose( tf );
			err_setcontext( " ## file " ); err_setcontext( tabfil );
			return;
		} /*endif*/
	}  while  (ds[(*steps)-1] < maxdist);

	/* close table file */
	sy_fclose( tf );

} /* end of pth_getdistline */



/*-------------------------------------------------------------------*/

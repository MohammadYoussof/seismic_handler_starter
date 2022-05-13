
/* file beamloc.c
 *      =========
 *
 * version 2, 21-Nov-2005
 *
 * event location using phase beams
 * K. Stammler, 12-Mar-2005
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
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "utusrdef.h"
#include "glusrdef.h"
#include "earthloc.h"
#include "ptusrdef.h"
#include "numres.h"


#define STATNAMELTH 6
#define MAXPHASE 10
#define PHASENAMELTH 9
#define MAXDATASET 3


typedef struct {
	int       statidx;      /* station index */
	int       lth;          /* lenth of trace in samples */
	float     *dat;         /* pointer to data */
	char      comp;         /* component */
} BmlTrcT;

typedef struct {
	char      name[PHASENAMELTH+1];    /* name of phase */
	int       datidx;                  /* dataset index */
	char      cmptyp;                  /* component type (.zh) */
} BmlPhaseT;


/* prototypes of local routines */
void BmlReadStations( char fname[], int *statnum, char **statstr, char ***statptr );
void BmlReadBeamlocs( char beamlocfile[], int beamno, float **lat, float **lon );
void BmlReadTraces( char **stat, int statnum, int wdwid, int datidx, int trclth,
	BmlTrcT trc[], int *trcno );
void BmlReadBeams( char **stat, int statnum, char phase[], int beamno, int ***beam );
void BmlReadPhases( char phasefile[], int maxdatidx, BmlPhaseT *ph,
	int *phaseno );
int BmlComputeLocation( void );
void BmlFindMaximum( int bestbeam );
void BmlOpenLogfiles( BmlPhaseT ph[], int phaseno, int wdwid, FILE *fp[] );
void BmlPrintResult( void );



/* global variables */
char      *bmlv_statstr;               /* station strings */
char      **bmlv_station;              /* list of stations */
int       bmlv_statnum;                /* length of above list */
BmlTrcT   *bmlv_trc[MAXDATASET];       /* data traces */
int       bmlv_trcno[MAXDATASET];      /* number of traces */
int       **bmlv_beam[MAXPHASE];       /* beam shifts in samples */
int       bmlv_beamno;                 /* number of beams */
BmlPhaseT bmlv_phase[MAXPHASE];        /* phase names */
int       bmlv_phaseno;                /* number of phases */
float     *bmlv_beam_lat;              /* beam location latitudes */
float     *bmlv_beam_lon;              /* beam location longitudes */
float     bmlv_dt=2.0;                 /* sample distance in sec */
FILE      *bmlv_logfp[MAXPHASE+1];     /* pointer to output files */
float     bmlv_meanval[MAXPHASE+1];    /* mean beam values for each phase */
float     bmlv_maxval[MAXPHASE+1];     /* maximum beam values for each phase */


int main( int argc, char *argv[] )
{
	/* local variables */
	char     statlist[cBcFileLth+1];  /* file name with stations */
	char     phasefile[cBcFileLth+1]; /* file name with phases */
	char     beamlocfile[cBcFileLth+1]; /* beam location file */
	int      wdwid;                   /* window ID number */
	int      datasetno;               /* number of data sets */
	int      trclth[MAXDATASET];      /* length of data traces */
	int      i;                       /* counter */
	int      bestbeam;                /* index of best beam */

	/* excutable code */

	printf( "remember to get dt as parameter\n" );

	if  (argc < 7)  {
		fprintf( stderr,
			"Usage: %s <statlistfile> <phasefile> <beamlocfile> <beamno> <wdwid> <trclth0> [<trclth1> [...]]\n",
			argv[0] );
		return 1;
	} /*endif*/

	/* get parameters */
	if  (strlen(argv[1]) > cBcFileLth) {fprintf(stderr,"ill. par. 1\n"); return 1;}
	strcpy( statlist, argv[1] );
	if  (strlen(argv[2]) > cBcFileLth) {fprintf(stderr,"ill. par. 2\n"); return 1;}
	strcpy( phasefile, argv[2] );
	if  (strlen(argv[3]) > cBcFileLth) {fprintf(stderr,"ill. par. 3\n"); return 1;}
	strcpy( beamlocfile, argv[3] );
	sscanf( argv[4], "%d", &bmlv_beamno );
	sscanf( argv[5], "%d", &wdwid );
	datasetno = 0;
	for  (i=6; i<argc; i++)  {
		if  (datasetno == MAXDATASET)  {
			fprintf( stderr, "beamloc: too many data sets.\n" );
			return 16;
		} /*endif*/
		sscanf( argv[i], "%d", trclth+datasetno );
		datasetno++;
	} /*endif*/

	/* read in beam locations */
	BmlReadBeamlocs( beamlocfile, bmlv_beamno, &bmlv_beam_lat, &bmlv_beam_lon );

	/* read in station names */
	BmlReadStations( statlist, &bmlv_statnum, &bmlv_statstr, &bmlv_station );

	/* allocate memory for data, max 3 components per station */
	for  (i=0; i<datasetno; i++)  {
		bmlv_trc[i] = (BmlTrcT *)malloc( 3 * bmlv_statnum * sizeof(BmlTrcT) );
		if  (bmlv_trc[i] == NULL)  {fprintf(stderr,"memory alloc error (3)\n");return 4;}
	} /*endfor*/

	/* read data traces */
	for  (i=0; i<datasetno; i++)
		BmlReadTraces( bmlv_station, bmlv_statnum, wdwid, i, trclth[i],
			bmlv_trc[i], bmlv_trcno+i );
	for  (i=0; i<datasetno; i++)
		printf( "--> found %d traces\n", bmlv_trcno[i] );

	/* read in phases */
	BmlReadPhases( phasefile, datasetno-1, bmlv_phase, &bmlv_phaseno );

	/* read beam shifts for each station and beam */
	for  (i=0; i<bmlv_phaseno; i++)
		BmlReadBeams( bmlv_station, bmlv_statnum, bmlv_phase[i].name,
			bmlv_beamno, bmlv_beam+i );

	BmlOpenLogfiles( bmlv_phase, bmlv_phaseno, wdwid, bmlv_logfp );

	/* now do computation */
	bestbeam = BmlComputeLocation( );

	BmlFindMaximum( bestbeam );

	/* close logfiles */
	for  (i=0; i<=bmlv_phaseno; i++)
		if  (bmlv_logfp[i] != NULL)
			fclose( bmlv_logfp[i] );

	BmlPrintResult( );

	return 0;

} /* end of main */



/*----------------------------------------------------------------------------*/



void BmlReadBeamlocs( char beamlocfile[], int beamno, float **lat, float **lon )

/* Read in beam locations from file
 *
 * parameters of routine
 * char       beamlocfile[];   input; name of beam location file
 * int        beamno;          input; number of beams
 * float      **lat, **lon;    output; arrays with lat and lon
 */
{
	/* local variables */
	FILE     *fp;                 /* pointer to input file */
	char     line[cBcLineLth+1];  /* current line of file */
	int      i;                   /* counter */

	/* executable code */

	/* allocate memory for arrays */
	*lat = (float *)malloc( beamno*sizeof(float) );
	if  (*lat == NULL)  {fprintf(stderr,"mem alloc err 19\n"); exit(19);}
	*lon = (float *)malloc( beamno*sizeof(float) );
	if  (*lon == NULL)  {fprintf(stderr,"mem alloc err 20\n"); exit(20);}

	fp = fopen( beamlocfile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "beamloc: cannot find input file %s\n", beamlocfile );
		exit( 17 );
	} /*endif*/

	for  (i=0; i<beamno; i++)  {
		if  (fgets(line,cBcLineLth,fp) == NULL)  {
			fprintf( stderr, "beamloc: read error on file %s\n", beamlocfile );
			fclose( fp );
			exit( 21 );
		} /*endif*/
		if  (sscanf(line,"%f %f",(*lat)+i,(*lon)+i) != 2)  {
			fprintf( stderr, "beamloc: error parsing line in file %s\n", beamlocfile );
			fclose( fp );
			exit( 23 );
		} /*endif*/
	} /*endfor*/

	fclose( fp );

} /* end of BmlReadBeamlocs */



/*----------------------------------------------------------------------------*/



void BmlReadStations( char fname[], int *statnum, char **statstr, char ***statptr )

/* read station names from file
 *
 * parameters of routine
 * char       fname[];     input; file name with station names
 * int        *statnum;    output; number of stations
 * char       **statstr;   output; station names in linear string
 * char       ***statptr;  output; list of pointers to names
 */
{
	/* local variables */
	FILE     *fp;                 /* pointer to input file */
	char     line[cBcLineLth+1];  /* current line */
	char     i;                   /* counter */
	int      slen;                /* string length */

	/* executable code */

	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "beamloc: input file %s not found.\n", fname );
		exit( 2 );
	} /*endif*/

	/* count lines */
	*statnum = 0;
	while  (fgets(line,cBcLineLth,fp) != NULL)
		if  (*line >= 'A')  (*statnum)++;

	printf( "--> %d stations found\n", *statnum );

	*statstr = (char *)malloc( (*statnum)*STATNAMELTH*sizeof(char) );
	if  (statstr == NULL)  {fprintf(stderr,"memory alloc error (1)\n");exit(4);}
	*statptr = (char **)malloc( (*statnum)*sizeof(char *) );
	if  (statptr == NULL)  {fprintf(stderr,"memory alloc error (2)\n");exit(5);}
	for  (i=0; i<(*statnum); i++)
		(*statptr)[i] = *statstr + i*STATNAMELTH;

	/* read stations */
	rewind( fp );
	i = 0;
	while  (fgets(line,cBcLineLth,fp) != NULL)
		if  (*line >= 'A')  {
			slen = strlen( line );
			if  (line[slen-1] == '\n')  line[slen-1] = '\0';
			if  (strlen(line) > STATNAMELTH)  {
				fprintf( stderr, "beamloc: station name %s too long.\n", line );
				exit( 3 );
			} /*endif*/
			strcpy( (*statptr)[i], line );
			i++;
		}

	fclose( fp );

} /* end of BmlReadStations */


 
/*----------------------------------------------------------------------------*/



void BmlReadTraces( char **stat, int statnum, int wdwid, int datidx, int trclth,
	BmlTrcT trc[], int *trcno )

/* Reads in data traces
 *
 * parameters of routine
 * char       **stat;     input; station names
 * int        statnum;    input; number of stations
 * int        wdwid;      input; window ID
 * int        datidx;     input data set index
 * int        trclth;     input; trace length in samples
 * BmlTrcT    *trc;       output; data traces
 * int        *trcno;     output; number of traces read
 */
{
	/* local variables */
	int      i, c, s;              /* counters */
	char     cmp[4]="zne";         /* components to read */
	char     fname[cBcFileLth+1];  /* file name */
	FILE     *fp;                  /* pointer to input file */

	/* executable code */

	*trcno = 0;
	for  (i=0; i<statnum; i++)  {
		for  (c=0; c<3; c++)  {

			/* buid filename and open file */
			sprintf( fname, "%04d_%s_%c_%d.dat", wdwid, stat[i], cmp[c], datidx );
			fp = fopen( fname, "r" );
			if  (fp == NULL)  {
				fprintf( stderr, "beamloc: file %s not found.  Continuing.\n", fname );
				continue;
			} /*endif*/

			/* allocate memory and read data */
			trc[*trcno].dat = (float *)malloc( trclth * sizeof(float) );
			if  (trc[*trcno].dat == NULL)  {
				fprintf( stderr, "beamloc: memory allocation error (5)\n" );
				exit( 6 );
			} /*endif*/
			for  (s=0; s<trclth; s++)  {
				if  (fscanf( fp, "%f", &(trc[*trcno].dat[s]) ) != 1)  {
					fprintf( stderr, "read error on file %s\n", fname );
					exit( 7 );
				} /*endif*/
			} /*endfor*/

			trc[*trcno].lth = trclth;
			trc[*trcno].statidx = i;
			trc[*trcno].comp = cmp[c];

			fclose( fp );

			(*trcno)++;

		} /*endfor*/
	} /*endfor*/

} /* end of BmlReadTraces */



/*----------------------------------------------------------------------------*/


#define LARGEVAL 30000


void BmlReadBeams( char **stat, int statnum, char phase[], int beamno,
	int ***beam )

/* Read beam shifts
 *
 * parameters of routine
 * char       **stat;    input; station names
 * int        statnum;   input; number of stations
 * char       phase[];   input; name of phase
 * int        beamno;    input; number of beams
 * int        ***beam;   output; beam shift indices
 */
{
	/* local variables */
	int      i, b;                 /* counters */
	char     fname[cBcFileLth+1];  /* name of input file */
	FILE     *fp;                  /* pointer input file */
	int      *mem;                 /* memory pointer */
	int      minval;               /* minimum value */

	/* executable code */

	/* allocate memory for beams */
	mem = (int *)malloc( beamno*statnum*sizeof(int) );
	if  (mem == NULL)  {fprintf( stderr, "memory alloc error (8)\n" );exit(8);}
	*beam = (int **)malloc( statnum*sizeof(int *) );
	if  (*beam == NULL)  {fprintf(stderr,"mem alloc err (9)\n");exit(9);}
	for  (i=0; i<statnum; i++)
		(*beam)[i] = mem + i*beamno;

	for  (i=0; i<statnum; i++)  {
		sprintf( fname, "beams/beam_%s_%s.dat", stat[i], phase );
		fp = fopen( fname, "r" );
		if  (fp == NULL)  {
			fprintf( stderr, "beamloc: cannot find beam file %s.  Abort\n", fname );
			exit( 7 );
		} /*endif*/
		for  (b=0; b<beamno; b++)
			if  (fscanf(fp,"%d",(*beam)[i]+b) != 1)  {
				fprintf( stderr, "read error on file %s\n", fname );
				exit( 10 );
			} /*endif*/
		fclose( fp );
	} /*endfor*/

	/* normalize beams, set smallest shift to zero for each beam */
	/* A zero original value means that no travel time could be computed. */
	/* This is reset to a large value to be omitted in computation */
	for  (b=0; b<beamno; b++)  {
		/* find minimum */
		minval = (*beam)[0][b];
		for  (i=1; i<statnum; i++)
			if  ((*beam)[i][b] == 0)  {
				(*beam)[i][b] = LARGEVAL;
			}  else  {
			  if  ((*beam)[i][b] < minval)  minval = (*beam)[i][b];
			} /*endif*/
		/* subtract minimum */
		for  (i=1; i<statnum; i++)
			(*beam)[i][b] -= minval;
	} /*endfor*/

#ifdef XXX
	/* dump beams */
	for  (b=0; b<beamno; b++)  {
		printf( "beam no %d: ", b );
		for  (i=1; i<statnum; i++)
			printf( " %d", (*beam)[i][b] );
		printf( "\n\n" );
	} /*endfor*/
#endif

} /* end of BmlReadBeams */



/*----------------------------------------------------------------------------*/



void BmlReadPhases( char phasefile[], int maxdatidx, BmlPhaseT *ph,
	int *phaseno )

/* reads in phase names and data set indices
 *
 * char      phasefile[];   input; name of file with phase data
 * int       maxdatidx;     input; maximum data index permitted
 * BmlPhaseT ph[];          output; phase list
 * int       *phaseno;      output; number of phases
 */
{
	/* local variables */
	FILE     *fp;                 /* pointer to input file */
	char     line[cBcLineLth+1];  /* current line */
	char     pname[cBcLineLth+1]; /* name of phase */

	/* executable code */

	fp = fopen( phasefile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "beamloc: cannot find phase file %s.  Abort.\n", phasefile );
		exit( 11 );
	} /*endif*/

	*phaseno = 0;
	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		if  (*line == '!' || *line == '\n' || *line == '#')  continue;
		if  (*phaseno == MAXPHASE)  {
			fprintf( stderr, "beamloc: too many phases. Take less or increase array bound in code.\n" );
			fclose( fp );
			exit( 12 );
		} /*endif*/
		if  (sscanf( line, "%s %d %c", pname, &(ph[*phaseno].datidx),
			&(ph[*phaseno].cmptyp) ) != 3)  {
			fprintf( stderr, "beamloc: error in phase line of file %s:\n%s",
				phasefile, line );
			fclose( fp );
			exit( 13 );
		} /*endif*/
		if  (strlen(pname) > PHASENAMELTH)  {
			fprintf( stderr, "beamloc: phase name %s too long in file %s\n",
				pname, phasefile );
			fclose( fp );
			exit( 14 );
		} /*endif*/
		if  (ph[*phaseno].datidx > maxdatidx)  {
			fprintf( stderr, "beamloc: illegal data set index %d in %s\n",
				ph[*phaseno].datidx, phasefile );
			exit( 15 );
		} /*endif*/
		strcpy( ph[*phaseno].name, pname );
		(*phaseno)++;
	} /*endwhile*/

	fclose( fp );

} /* end of BmlReadPhases */



/*----------------------------------------------------------------------------*/



void BmlOpenLogfiles( BmlPhaseT ph[], int phaseno, int wdwid, FILE *fp[] )

/* Opens output logfiles for plots
 *
 * parameters of routine
 * BmlPhaseT  ph[];       input; list of phases
 * int        phaseno;    input, number of phases
 * int        wdwid;      input; window ID
 * FILE       fp[];       output; pointer to outptu files
 */
{
	/* local variables */
	int      i;                    /* counter */
	char     fname[cBcFileLth+1];  /* name of output file */

	/* executable code */

#ifdef XXX
	/* if no files are created: */
	for  (i=0; i<=phaseno; i++)
		fp[i] = NULL;
#endif

	/* here really open the files */
	for  (i=0; i<=phaseno; i++)  {
		if  (i < phaseno)  {
			sprintf( fname, "log_%04d_%s.dat", wdwid, ph[i].name );
		} else {
			sprintf( fname, "log_%04d_total.dat", wdwid );
		} /*endif*/
		bmlv_logfp[i] = fopen( fname, "w" );
		if  (bmlv_logfp[i] == NULL)  {
			fprintf( stderr, "beamloc: error opening output file %s\n", fname );
			exit( 29 );
		} /*endif*/
	} /*endfor*/

} /* end of BmlOpenLogfiles */



/*----------------------------------------------------------------------------*/



void BmlPrintResult( void )

/* Prints results of maximum search
 *
 * no parameters
 */
{
	/* local variables */
	int      i;       /* counter */

	/* executable code */

	bmlv_meanval[bmlv_phaseno] = 0.0;
	for  (i=0; i<=bmlv_phaseno; i++)  {
		if  (i<bmlv_phaseno)  {
			printf( " %5s ", bmlv_phase[i].name );
			bmlv_meanval[bmlv_phaseno] += bmlv_meanval[i];
		} else {
			printf( " total " );
		} /*endif*/
		printf( "(%6.2f/%6.2f) %6.2f\n", bmlv_maxval[i], bmlv_meanval[i],
			bmlv_maxval[i]/bmlv_meanval[i] );
	} /*endfor*/

} /* end of BmlPrintResults */



/*----------------------------------------------------------------------------*/



int BmlComputeLocation( void )

/* Performs computation of location
 *
 * no parameters
 */
{
	/* local variables */
	int      ib, ip, is, it;      /* counters */
	int      idx;                 /* data set index */
	int      trclth;              /* current trace length */
	float    sum;                 /* sum of traces */
	float    maxsum;              /* maximum sample of summation trace */
	float    minsum;              /* minimum sample of summation trace */
	float    totsum;              /* sum over all phases */
	float    totmaxsum;           /* biggest of all sums */
	int      totmaxbeam;          /* beam number of biggest sum */
	int      trccnt;              /* trace counter */
	int      i;                   /* index of trace */
	char     cmp, cmptyp;         /* component of trace and phase type */

	/* executable code */

	totmaxbeam = -1;
	totmaxsum = 0.0;

	/* set maximum beam values to zero for each phase */
	for  (ip=0; ip<=bmlv_phaseno; ip++)
		bmlv_maxval[ip] = 0.0;

	/* loop over all beams, counter: ib */
	for  (ib=0; ib<bmlv_beamno; ib++)  {
		totsum = 0.0;

		/* loop over all phases, counter: ip */
		for  (ip=0; ip<bmlv_phaseno; ip++)  {
			idx = bmlv_phase[ip].datidx;
			trclth = bmlv_trc[idx]->lth;
			maxsum = 0.0;
			minsum = 10000.0;

			/* loop over all samples of output trace, counter: is */
			for  (is=0; is<trclth; is++)  {
				sum = 0.0;
				trccnt = 0;

				/* loop over all traces, counter: it */
				for  (it=0; it<bmlv_trcno[idx]; it++)  {
					/* sum all shifted samples for a phase beam */
					/* if bmlv_phase[ip].trctyp not valid continue */
					cmp = bmlv_trc[idx][it].comp;
					cmptyp = bmlv_phase[ip].cmptyp;
					if  (cmptyp == 'z' && cmp != 'z')  continue;
					if  (cmptyp == 'h' && cmp == 'z')  continue;
					i = is + bmlv_beam[ip][bmlv_trc[idx][it].statidx][ib];
					if  (i < trclth)  {
						sum += bmlv_trc[idx][it].dat[i];
						trccnt++;
					} /*endif*/
				} /*endfor*/

				if  (trccnt > 0)  sum /= (float)trccnt;
				if  (sum > maxsum && trccnt > bmlv_trcno[idx]/4)  maxsum = sum;
				if  (sum < minsum && trccnt > bmlv_trcno[idx]/4)  minsum = sum;
			} /*endfor*/

			/* write beam values to output file for each phase */
			if  (bmlv_logfp[ip] != NULL && maxsum > 0.0)
				fprintf( bmlv_logfp[ip], "%6.2f %6.2f %6.2f\n",
					bmlv_beam_lat[ib], bmlv_beam_lon[ib], maxsum );
			/* compute mean values for each phase */
			if  (ib == 0)  {
				bmlv_meanval[ip] = maxsum;
			} else {
				bmlv_meanval[ip] = ( (float)ib / (float)(ib+1) )
					* (bmlv_meanval[ip] + (maxsum/(float)ib));
			} /*endif*/
			/* store maximum values for each phase */
			if  (maxsum > bmlv_maxval[ip])  bmlv_maxval[ip] = maxsum;
			totsum += maxsum;
		} /*endfor*/

		/* write total beam value to output file */
		if  (bmlv_logfp[bmlv_phaseno] != NULL && totsum > 0)
			fprintf( bmlv_logfp[bmlv_phaseno], "%6.2f %6.2f %6.2f\n",
				bmlv_beam_lat[ib], bmlv_beam_lon[ib], totsum );
		if  (totsum > totmaxsum)  {
			totmaxsum = totsum;
			totmaxbeam = ib;
		} /* endif*/

	} /*endfor*/

	printf( "maxbeam: %d (%f, lat:%5.1f  lon:%5.1f)\n", totmaxbeam, totmaxsum,
		bmlv_beam_lat[totmaxbeam], bmlv_beam_lon[totmaxbeam] );

	bmlv_maxval[bmlv_phaseno] = totmaxsum;

	return totmaxbeam;

} /* end of BmlComputeLocation */



/*----------------------------------------------------------------------------*/



float BmlEvalLatLon( float latlon[] )

/* Compute beam sum for give lat & lon
 *
 * parameters of routine
 * float      latlon[];   input; latitude and longitude as indexes 1 and 2
 *                        returns negative beam sum
 */
{
	/* local variables */
	int      i, ip, is, il, it;  /* counters */
	int      trccnt;      /* trace counter */
	int      idx;         /* data set index */
	int      trclth;      /* trace length in samples */
	int      *beam;       /* pointer to beam shifts */
	GLT_STATINF statinf;  /* station info */
	TSyStatus status;     /* return status */
	double   ddist, dazim, dbazim; /* output of locdiff */
	char     *env;        /* environment variable */
	float    trav;        /* travel time in sec */
	float    depth=10.0;  /* depth used */
	int      minbeam;     /* minimum beam shift */
	float    sum, maxsum, totsum;  /* sample sums */
	char     upstat[STATNAMELTH+1]; /* uppercase station name */
	char     locfile[cBcFileLth+1]; /* name of station location file */
	char     cmp, cmptyp; /* component of trace and phase type */

	/* executable code */

	env = (char *)getenv( "SH_INPUTS" );
	status = cBcNoError;
	pt_settabledir( env, &status );
	strcpy( locfile, env );
	strcat( locfile, "STATINF.DAT" );
	gl_locfile_name( locfile );

	beam = (int *)malloc( bmlv_statnum*sizeof(int) );
	if  (beam == NULL)  {fprintf(stderr,"mem alloc err 25\n"); exit(25);}

	totsum = 0.0;
	for  (ip=0; ip<bmlv_phaseno; ip++)  {
		idx = bmlv_phase[ip].datidx;
		trclth = bmlv_trc[idx]->lth;

		/* compute shifts for this phase */
		minbeam = 99999;
		for  (il=0; il<bmlv_statnum; il++)  {
			strcpy( upstat, bmlv_station[il] );
			ut_cap( upstat );
			status = cBcNoError;
			gl_statinf( upstat, &statinf, &status );
			if  (status != cBcNoError)  {
				fprintf( stderr, "beamloc: station %s not found (%d)\n",
					bmlv_station[il], status );
				exit( 26 );
			} /*endif*/
			mb_locdiff( statinf.lat, statinf.lon, latlon[1], latlon[2],
				&ddist, &dazim, &dbazim );
			status = cBcNoError;
			trav = pt_travel( bmlv_phase[ip].name, ddist, depth, &status );
			beam[il] = Nint( trav / bmlv_dt );
			if  (beam[il] == 0)  {
				/* could not compute travel time, so shift out of area */
				beam[il] = LARGEVAL;
			} else {
				if  (beam[il] < minbeam)  minbeam = beam[il];
			} /*endif*/
		} /*endfor*/
		/* shift shifts to have zero minimum shift */
		for  (il=0; il<bmlv_statnum; il++)
			beam[il] -= minbeam;

		/* loop over all samples of output trace, counter: is */
		maxsum = 0.0;
		for  (is=0; is<trclth; is++)  {
			sum = 0.0;
			trccnt = 0;
			/* loop over all traces, counter: it */
			for  (it=0; it<bmlv_trcno[idx]; it++)  {
				/* sum all shifted samples for a phase beam */
				cmp = bmlv_trc[idx][it].comp;
				cmptyp = bmlv_phase[ip].cmptyp;
				if  (cmptyp == 'z' && cmp != 'z')  continue;
				if  (cmptyp == 'h' && cmp == 'z')  continue;
				i = is + beam[bmlv_trc[idx][it].statidx];
				if  (i < trclth)  {
					sum += bmlv_trc[idx][it].dat[i];
					trccnt++;
				} /*endif*/
			} /*endfor*/
			if  (trccnt > 0)  sum /= (float)trccnt;
			/*printf( " %f", sum );*/
			if  (sum > maxsum && trccnt > bmlv_trcno[idx]/4)  maxsum = sum;
		} /*endif*/
		/* store maximum values for each phase */
		if  (maxsum > bmlv_maxval[ip])  bmlv_maxval[ip] = maxsum;
		totsum += maxsum;
		if  (bmlv_logfp[ip] != NULL)
			fprintf( bmlv_logfp[ip], "%6.2f %6.2f %6.2f\n",
				latlon[1], latlon[2], maxsum );
	} /*endfor*/

	free( beam );

	if  (bmlv_logfp[bmlv_phaseno] != NULL)
		fprintf( bmlv_logfp[bmlv_phaseno], "%6.2f %6.2f %6.2f\n",
			latlon[1], latlon[2], totsum );

	if  (totsum > bmlv_maxval[bmlv_phaseno])  bmlv_maxval[bmlv_phaseno] = totsum;

	return -totsum;

} /* end of BmlEvalLatLon */



/*----------------------------------------------------------------------------*/



void BmlFindMaximum( int bestbeam )

/* Finds maximum beam using downhill simplex
 *
 * parameters of routine
 * int        bestbeam;    input; index of best beam
 */
{
	/* local variables */
	float    vec1[3], vec2[3], vec3[3], vec4[3];  /* start points */
	float    *simplex[4], **nrsimplex;    /* pointer to above vectors */
	float    res[4], *nrres;              /* results at start points */
	int      ndim;                        /* number of dimensions (2 or 3) */
	float    ftol;                        /* tolerance */
	float    startstep;                   /* start step size */
	int      nfunc;                       /* number of function calls */

	/* executable code */

	/* setup vector constructions for NR */
	simplex[0] = vec1 - 1;
	simplex[1] = vec2 - 1;
	simplex[2] = vec3 - 1;
	simplex[3] = vec4 - 1;
	nrsimplex = simplex - 1;
	nrres = res - 1;

	ftol = 0.001;
	startstep = 0.5;

	/* setup start points */
	ndim = 2;

	/* need 3 start points */
	vec1[0] = bmlv_beam_lat[bestbeam];
	vec1[1] = bmlv_beam_lon[bestbeam];
	res[0] = BmlEvalLatLon( vec1-1 );
	vec2[0] = bmlv_beam_lat[bestbeam] - startstep;
	vec2[1] = bmlv_beam_lon[bestbeam];
	res[1] = BmlEvalLatLon( vec2-1 );
	vec3[0] = bmlv_beam_lat[bestbeam];
	vec3[1] = bmlv_beam_lon[bestbeam] - startstep;
	res[2] = BmlEvalLatLon( vec3-1 );

	nr_amoeba( nrsimplex, nrres, ndim, ftol, BmlEvalLatLon, &nfunc );

	printf( "--> nfunc %d minsq: %7.3f  lat: %8.2lf  lon: %8.2lf\n",
		nfunc, nrres[1], nrsimplex[1][1], nrsimplex[1][2] );

} /* end of BmlFindMaximum */



/*----------------------------------------------------------------------------*/

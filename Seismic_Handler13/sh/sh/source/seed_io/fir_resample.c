
/* file fir_resample.c
 *      ==============
 *
 * version 11, 13-Dec-2006
 *
 * FIR filter for trace decimation.
 * K. Stammler, 19-Nov-97
 */



#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "tcusrdef.h"
#include "erusrdef.h"
#include "utusrdef.h"
#include "cpar.h"
#include "seedcfg.h"
#include "seed_lib.h"
#include "globalparams.h"


/* constants */
#define EPSILON 0.05
#define MINSAMPLES 4000


/* types */

/* type of samples */
typedef INT32 TFrCounts;

/* command line parameters */
typedef struct {
	char      sfdfile[cBcFileLth+1];      /* sfdfile for reading */
	char      stream[cBcLineLth+1];       /* input stream name */
	char      outchan[cBcShortStrLth+1];  /* output channel name */
	char      starttime[cBcTimeLth+1];    /* start of total time window */
	char      endtime[cBcTimeLth+1];      /* end of total time window */
	char      firfile[cBcFileLth+1];      /* name of FIR filter file */
	float     chunklth;                   /* length of single time window (sec)*/
	TSyBoolean swap;                      /* swap SEED data at reading */
} TFrPars;

/* single FIR filter */
typedef struct {
	int       decfac;                     /* decimation factor */
	int       firlth;                     /* length of FIR filter */
	float     *coeff;                     /* pointer to coefficients */
} TFrFir;

/* FIR cascade */
typedef struct {
	int     stageno;                      /* number of stages */
	TFrFir  *fir;                         /* array of firs */
} TFrFirCasc;

/* data of one stage */
typedef struct {
	int        lth;                       /* number of samples */
	TFrCounts  *ptr;                      /* pointer to data */
} TFrStageData;

/* exported variables */
/* ... */

/* global variables */
static SeedSetupParT frv_setup;          /* SEED setup parameters */



/* prototypes of local routines */
static FrReadFir( char fname[], TFrFirCasc *casc, int *offsetlth );
static void TFrProcessData( TFrStageData *dat, TFrFirCasc *casc );
static void TFrWriteData( TFrStageData *dat, int stageno,
	char outtime[], float dt, char stream[], char outchan[] );


int main( int argc, char *argv[] )
{
	/* local variables */
	STATUS   status;                  /* return status */
	TFrPars  par;                     /* command line parameters */
	TFrFirCasc casc;                  /* FIR cascade */
	TFrStageData *dat;                /* pointer to data of all stages */
	TFrCounts *rdat;                  /* data read in */
	INT32    rlth;                    /* number of samples read in */
	float    dt;                      /* sample distance */
	float    calib;                   /* calibration constant */
	float    time_to_go;              /* remaining time window */
	char     currtime[cBcTimeLth+1];  /* time of current data chunk */
	char     readtime[cBcTimeLth+1];  /* requested read time */
	char     exacttime[cBcTimeLth+1]; /* time actually read in */
	char     outtime[cBcTimeLth+1];   /* start time of output data */
	float    readlth;                 /* read length in sec */
	int      smp_offset;              /* offset in samples */
	float    sec_offset;              /* time offset in sec for reading */
	float    rtdiff;                  /* diff. between req. and actual time */
	float    timeshift;               /* time shift (sec) produced by FIR casc */
	float    stage_dt;                /* dt of one stage */
	int      i;                       /* counter */
	int      flags;                   /* data flags */

	/* executable code */

	/* get parameters */
	GpReadParfile();
	status = cBcNoError;
	SeedSetup( &frv_setup, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	SeedSetDecodeErrorAbort( FALSE );

	/* get parameters from command line */
	pa_init( argc, argv );
	if  (pa_pnumber() != 6)  {
		fprintf( stderr, "Usage: %s <sfdfile> <stream> <start> ", pa_progname() );
		fprintf( stderr, "<end> <firfile> <chunklth>\n" );
#		ifdef NOTUSED
		fprintf( stderr, "   Qualifiers:\n" );
		fprintf( stderr, "   -demean    remove mean value before decimation\n" );
#		endif
		return 1;
	} /*endif*/
	strcpy( par.sfdfile, pa_pvalue(1) );
	strcpy( par.stream, pa_pvalue(2) );
	strcpy( par.starttime, pa_pvalue(3) );
	strcpy( par.endtime, pa_pvalue(4) );
	strcpy( par.firfile, pa_pvalue(5) );
	sscanf( pa_pvalue(6), "%f", &par.chunklth );
#	ifdef SH_SETUP_LINUX
	par.swap = FALSE;
#	else
	par.swap = TRUE;
#	endif
	par.outchan[0] = '\0';
	if  (pa_qspecified("-swap"))  par.swap = TRUE;
	if  (pa_qspecified("-noswap"))  par.swap = FALSE;
	if  (pa_qspecified("-chan"))  strcpy( par.outchan, pa_qvalue("-chan") );
	if  (pa_qspecified("-nowarn"))  SeedPrintWarnings( FALSE );

	/* read FIR coefficients from file */
	FrReadFir( par.firfile, &casc, &smp_offset );

	/* allocate array of stage structures */
	dat = malloc( sizeof(TFrStageData) * (casc.stageno+1) );
	if  (dat == NULL)  {
		fprintf( stderr, "%s: memory allocation error (dat)\n", pa_progname() );
		exit ( 1 );
	} /*endif*/

	/* read in data just to get sample rate for offset */
	i = 0;
	strcpy( readtime, par.starttime );
	FOREVER  {
		status = cBcNoError;
		SeedReadStream( 0, par.sfdfile, par.stream, par.swap, readtime, 60,
			&rdat, &rlth, exacttime, &dt, &calib, &flags, &status );
		if  (!SySevere(&status))  {free(rdat); break;}
		if  (++i > 100)  {
			fprintf( stderr, "%s: no data found for %s at %s.  Abort\n",
				pa_progname(), par.stream, par.starttime );
			err_writemsg( status, "Aborted after 100 trials.", TRUE );
		} /*endif*/
		status = cBcNoError;
		tc_tadd( readtime, 300, readtime, &status );
	} /*endfor*/
	sec_offset = (float)smp_offset * dt;

	/* compute time shift, assuming a symmetric FIR filter */
	timeshift = 0.0;
	stage_dt = dt;
	for  (i=0; i<casc.stageno; i++)  {
		/* timeshift += stage_dt * (float)(casc.fir[i].firlth / 2); */
		if  ((casc.fir[i].firlth % 2) == 0)  {
			timeshift += stage_dt * ((float)(casc.fir[i].firlth / 2) + 0.5);
		} else {
			timeshift += stage_dt * (float)((casc.fir[i].firlth+1) / 2);
		} /*endif*/
		stage_dt *= (float)(casc.fir[i].decfac);
	} /*endfor*/

	/* read data in chunk by chunk, including offset each time */
	strcpy( currtime, par.starttime );
	FOREVER  {

		/* how much remains to be done ? */
		status = cBcNoError;
		time_to_go = tc_tdiff( par.endtime, currtime, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );

		/* exit loop if start time equals or is after end time */
		if  (time_to_go <= EPSILON)  break;

		/* compute length of time chunk and add offset */
		readlth = (time_to_go > par.chunklth) ? par.chunklth : time_to_go;
		readlth += sec_offset;
		tc_tadd( currtime, timeshift-sec_offset, readtime, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );

		printf( "--> reading %s length %f\n", readtime, readlth );
		/* read stream */
		SeedReadStream( 0, par.sfdfile, par.stream, par.swap, readtime, readlth,
			&rdat, &rlth, exacttime, &dt, &calib, &flags, &status );

		/* check for continuous data */
		rtdiff = 0.0;
		if  (!SySevere(&status) || status == SeedERR_NEXTTIMEPOS)
			rtdiff = tc_tdiff( exacttime, readtime, &status );
		if  (rtdiff > dt/2.0 && rtdiff < readlth)  {
			/* local variables */
			TFrCounts *tmpdat;          /* alternate data buffer */
			INT32     smplth;           /* number of samples */
			INT32     c;                /* sample counter */
			INT32     b;                /* start sample */
			/* executable code */
			fprintf( stderr, "%s: handle small gap near %s\n",
				pa_progname(), readtime );
			smplth = Nint( readlth / dt );
			tmpdat = malloc( sizeof(TFrCounts) * smplth );
			if  (tmpdat == NULL)  {
				fprintf( stderr, "%s: memory allocation error (tmpdat)\n",
					pa_progname() );
				exit( 1 );
			} /*endif*/
			/* zero samples */
			for  (c=0; c<smplth; c++)  tmpdat[c] = 0;
			/* copy beginning of data from rdat to tmpdat */
			b = Nint( rtdiff / dt );
			for  (c=b; c<smplth; c++)  tmpdat[c] = rdat[c-b];
			/* use tmpdat as rdat */
			free( rdat );
			rdat = tmpdat;
		} else if  (rtdiff < -dt/2.0)  {
			err_writemsg( status, "should flush SEED output ???", TRUE );
			fprintf( stderr, "%s: cannot handle this condition, ask K.S.\n" );
			exit( 1 );
		} else if  (SySevere(&status))  {
			err_writemsg( status, "should flush SEED output !!!", TRUE );
			fprintf( stderr, "%s: cannot handle this condition, ask K.S.\n" );
			exit( 1 );
		} /*endif*/

		/* filter data and resample */
		dat[0].lth = rlth;
		dat[0].ptr = rdat;
		TFrProcessData( dat, &casc );

		/* write data to output file */
		tc_tadd( exacttime, sec_offset-timeshift, outtime, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
		TFrWriteData( dat, casc.stageno+1, outtime, stage_dt,
			par.stream, par.outchan );

		/* increment time */
		tc_tadd( currtime, par.chunklth, currtime, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );

	} /*endfor*/

	/* flush buffer */
	TFrWriteData( NULL, 0, "", 0.0, par.stream, par.outchan );

	return 0;

} /* end of main */



/*----------------------------------------------------------------------------*/



static FrReadFir( char fname[], TFrFirCasc *casc, int *offsetlth )

/* Reads in FIR cascade.  Aborts on error.  '*offsetlth' returns number of
 * samples to be read in as total offset (before starting time of output)
 * in original data to account for offsets of all stages.
 *
 * parameters of routine
 * char       fname[];        input; name of FIR file
 * TFrFirCasc *casc;          output; FIR cascade read in
 * int        *offsetlth;     output; total offset in samples (see above)
 */
{
	/* local variables */
	FILE     *fp;                  /* pointer to file */
	char     line[cBcLineLth+1];   /* current line of file */
	int      totdecfac;            /* total decimation factor */
	int      s;                    /* stage counter */
	int      i;                    /* sample counter */

	/* executable code */

	fp = fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: cannot open FIR file %s.  Abort.\n",
			pa_progname(), fname );
		exit( 1 );
	} /*endif*/

	/* read off comments and blank lines */
	*line = '!';
	while  (*line == '!' || *line == '\n')
		fgets( line, cBcLineLth, fp );

	/* allocate stages */
	sscanf( line, "%d", &(casc->stageno) );
	if  (casc->stageno <= 0 || casc->stageno > 1000)  {
		fprintf( stderr, "%s: ridiculous number of stages (%d).  Abort.\n",
			pa_progname(), casc->stageno );
		exit( 1 );
	} /*endif*/
	casc->fir = (TFrFir *)malloc( sizeof(TFrFir) * (casc->stageno) );
	if  (casc->fir == NULL)  {
		fprintf( stderr, "%s: memory allocation error (stages).  Abort.\n",
			pa_progname() );
		exit( 1 );
	} /*endif*/

	*offsetlth = 0;
	totdecfac = 1;
	for  (s=0; s<(casc->stageno); s++)  {
		/* read off comments and blank lines */
		*line = '!';
		while  (*line == '!' || *line == '\n')
			fgets( line, cBcLineLth, fp );
		sscanf( line, "%d", &(casc->fir[s].decfac) );
		fgets( line, cBcLineLth, fp );
		sscanf( line, "%d", &(casc->fir[s].firlth) );
		casc->fir[s].coeff = (float *)malloc( sizeof(float)
			* casc->fir[s].firlth );
		if  (casc->fir[s].coeff == NULL)  {
			fprintf( stderr, "%s: memory allocation error (coeff)\n",
				pa_progname() );
			exit( 1 );
		} /*endif*/
		/* add offset for this stage */
		*offsetlth += totdecfac * (casc->fir[s].firlth);
		/* adjust total decimation factor */
		totdecfac *= casc->fir[s].decfac;
		for  (i=0; i<(casc->fir[s].firlth); i++)
			fscanf( fp, "%f\n", (casc->fir[s].coeff)+i );
	} /*endfor*/

	fclose( fp );

} /* end of FrReadFir */



/*----------------------------------------------------------------------------*/



static void TFrProcessData( TFrStageData *dat, TFrFirCasc *casc )

/* Processes data; applies filter, decimates and writes output.
 * Uses elements of rdat from 0 to (casc->stageno).
 *
 * parameters of routine
 * TFrCounts  *dat;                  input; array of samples
 * INT32      rlth;                  input; length of above array
 */
{
	/* local variables */
	int      s;          /* stage counter */
	TFrStageData *s_in;  /* stage input data */
	TFrStageData *s_out; /* stage output data */
	TFrFir       *s_fir; /* current FIR stage */
	TFrCounts    *out;   /* pointer to output samples */
	TFrCounts    *inp;   /* pointer to input samples */
	int      oc;         /* output sample counter */
	int      ic;         /* input sample counter */
	float    smp;        /* current output sample */
	int      fc;         /* FIR sample counter */

	/* executable code */

	for  (s=0; s<(casc->stageno); s++)  {
		s_in = dat + s;
		s_out = dat + s + 1;
		s_fir = casc->fir + s;
		/* allocate samples for output stage */
		if  ((s_in->lth - s_fir->firlth) % s_fir->decfac != 0)  {
			fprintf( stderr,
				"%s: chunk+offset (%d-%d,%d) not a multiple of decimation factor\n",
				pa_progname(), s_in->lth, s_fir->firlth, s_fir->decfac );
			exit( 1 );
		} /*endif*/
		s_out->lth = (s_in->lth - s_fir->firlth) / s_fir->decfac;
		s_out->ptr = malloc( sizeof(TFrCounts) * (s_out->lth) );
		if  (s_out->ptr == NULL)  {
			fprintf( stderr, "%s: memory allocation error (stage %d)\n",
				pa_progname(), s+1 );
			exit( 1 );
		} /*endif*/
		/* compute output samples */
		out = s_out->ptr;
		inp = s_in->ptr;
		ic = s_fir->firlth - 1;
		for  (oc=0; oc<(s_out->lth); oc++)  {
			/* input array should be long enough otherwise I made a mistake */
			if  (ic >= s_in->lth)  {
				fprintf( stderr, "%s: program bug (1).  Abort.\n", pa_progname() );
				exit( 1 );
			} /*endif*/
			/* loop all FIR coefficients */
			smp = 0.0;
			/* following could be improved by converting the input array to */
			/* float before processing FIR multiplication !!! */
			for  (fc=0; fc<(s_fir->firlth); fc++)
				smp += (float)(inp[ic-fc]) * s_fir->coeff[fc];
			/* truncate result to integer */
			out[oc] = Nint( smp );
			ic += s_fir->decfac;
		} /*endfor*/
	} /*endfor*/

} /* end of TFrProcessData */



/*----------------------------------------------------------------------------*/



#define SEEDOUT "fir_resample.seed"



static void TFrWriteData( TFrStageData *dat, int stageno,
	char outtime[], float dt, char stream[], char outchan[] )

/* Writes data to output file.  If 'dat' equals 'NULL' then all remaining
 * samples in the buffer are flushed and the output SEED file is closed.
 *
 * parameters of routine
 * TFrStageData  *dat;           input; data arrays
 * int           stageno;        input; number of arrays
 * char          outtime[];      input; exact start time of first sample
 * float         dt;             input; sample distance in sec
 * char          stream[];       input; name of input stream
 * char          outchan[];      input; name of output channel
 */
{
	/* local variables */
#ifdef XXX
	/*------------------------------------------*/
	FILE     *fp;            /* pointer to output file */
	/*------------------------------------------*/
#endif
	static TSyBoolean first_call=TRUE;/* first call to routine */
	static TSyBoolean new_hdr=TRUE;   /* new header required */
	static SeedDataHeaderT dathdr;    /* data header */
	static FILE            *seed=NULL;/* pointer to SEED file */
	static SeedSbyteT      *wrk;      /* workspace for compressed data */
	static int             wrklth;    /* length of work space in records */
	static INT32           *wdat;     /* output array */
	static int             wdatlth;   /* max length of output array */
	static INT32           wdatidx=0; /* current length of output array */
	static char            exptime[cBcTimeLth+1]="";  /* expected time */
	static INT32           prevsmp=0; /* previous sample for checksum */
	static NTIME           cntime;    /* time of start sample for compressing */
	NTIME    ntime;                   /* numeric start time */
	TSyStatus status;                 /* return status */
	float    smprate;                 /* sample rate */
	int      i_smprate;               /* integer sample rate */
	float    tmp;                     /* scratch */
	int      i;                       /* counter */
	char     station[cBcShortStrLth+1]; /* station name */
	char     chan[cBcShortStrLth+1];  /* channel name */
	char     comp;                    /* component */
	char     *cptr;                   /* pointer to char */
	TFrCounts *smpptr;                /* pointer to input samples */
	int      smplth;                  /* number of input samples */
	char     timestr[cBcTimeLth+1];   /* scratch */
	TSyBoolean isgap;                 /* gap found */

	/* executable code */

	/* check for gaps */
	isgap = (*exptime != '\0' && strcmp(exptime,outtime) != 0);
	if  (isgap || dat == NULL)  {
		/* flush output array */
		if  (seed == NULL)  {
			fprintf( stderr,
				"%s: should not happen: SEED-file not open when flushing.\n",
				pa_progname() );
		} else {
			status = cBcNoError;
			if  (isgap)  {
				/* gap found; complain */
				tc_n2t( &cntime, timestr, &status );
				if  (SySevere(&status))  err_writemsg( status, "", TRUE );
				fprintf( stderr, "%s: gap from %s to %s\n",
					pa_progname(), exptime, timestr );
			} /*endif*/
			SeedEncodeSteim1( fileno(seed), wdat, &prevsmp, &wdatidx, &cntime,
				wrk, wrklth, TRUE, &status );
			if  (SySevere(&status))  err_writemsg( status, "", TRUE );
		} /*endif*/
		new_hdr = TRUE;
		fclose( seed );
		seed = NULL;
		prevsmp = 0;
	} /*endif*/
	/* return if this is final call */
	if  (dat == NULL)  return;

#ifdef XXX
	/*------------------------------------------*/
	/* this will be removed after testing */
	fp = fopen( "resample.dat", "a" );
	if  (fp == NULL)  {
		fprintf( stderr, "error opening test file\n" );
		exit( 1 );
	} /*endif*/
	fprintf( fp, "LENGTH: %d\n", dat[stageno-1].lth );
	fprintf( fp, "DELTA: %e\n", stage_dt );
	fprintf( fp, "START: %s\n", outtime );
	for  (i=0; i<dat[stageno-1].lth; i++)
		fprintf( fp, "%d\n", dat[stageno-1].ptr[i] );
	fclose( fp );
	/*------------------------------------------*/
#endif

	/* initialize header and open output file if not yet done */
	if  (first_call || new_hdr)  {
		/* split stream name into components */
		if  (strlen(stream) > cBcShortStrLth)  {
			fprintf( stderr, "%s: illegal stream name %s\n",
				pa_progname(), stream );
			exit( 1 );
		} /*endif*/
		strcpy( station, stream );
		ut_cap( station );
		cptr = station;
		while  (*cptr != '-' && *cptr != '\0')  cptr++;
		*cptr++ = '\0';
		strcpy( chan, cptr );
		ut_cap( chan );
		cptr = chan;
		while  (*cptr != '-' && *cptr != '\0')  cptr++;
		*cptr++ = '\0';
		comp = Cap( *cptr );
		/* fill out header */
		status = cBcNoError;
		tc_t2n( outtime, &ntime, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
		strcpy( dathdr.seqno, "000001" );
		dathdr.indicator = 'D';
		dathdr.Reserved_bytes_A = '\0';
		for  (i=0; i<5; i++)  dathdr.statcode[i] = ' ';
		strncpy( dathdr.statcode, station, 5 );
		i = strlen( station );
		if  (i < 5)  dathdr.statcode[i] = ' ';
		dathdr.locid[0] = dathdr.locid[1] = ' ';
		dathdr.channel[0] = (*outchan == '\0') ? chan[0] : outchan[0];
		dathdr.channel[1] = (*outchan == '\0') ? chan[1] : outchan[1];
		dathdr.channel[2] = comp;
		SeedNtimeToBtime( &ntime, &dathdr.starttime, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
		cntime = ntime;
		dathdr.no_of_samples = 0;
		smprate = 1.0 / dt;
		i_smprate = Nint( smprate );
		tmp = smprate - (float)i_smprate;
		if  (Abs(tmp) > smprate*1.0e-3)  {
			fprintf( stderr, "%s: odd sample rate %f not supported\n",
				pa_progname(), smprate );
			exit( 1 );
		} /*endif*/
		dathdr.smprate_fact = (WORD)i_smprate;
		dathdr.smprate_mult = 1;
		dathdr.activity = 0;
		dathdr.ioflags = 0;
		dathdr.quality = 0;
		dathdr.no_of_blockettes = 0;
		dathdr.timecorr = 0;
		dathdr.databegin = sizeof( SeedDataHeaderT );
		dathdr.first = 0;
		/* pass header to SEED writer */
		SeedSetHeaderPrototype( &dathdr );
		/* open output file */
		if  (first_call)  {
			seed = fopen( SEEDOUT, "w" );
		} else {
			seed = fopen( SEEDOUT, "a" );
		} /*endif*/
		if  (seed == NULL)  {
			fprintf( stderr, "%s: error opening output file %s\n",
				pa_progname(), SEEDOUT );
			exit( 1 );
		} /*endif*/
		/* allocating memory for work space at first time */
		if  (first_call)  {
			wrklth = dat[stageno-1].lth * 4 / frv_setup.seed_rec_size;
			wrk = (SeedSbyteT *)malloc( wrklth * frv_setup.seed_rec_size );
			if  (wrk == NULL)  {
				fprintf( stderr, "%s: memory allocation error (wrkspc)\n",
					pa_progname() );
				exit( 1 );
			} /*endif*/
			wdatlth = dat[stageno-1].lth * 4;
			wdat = (INT32 *)malloc( wdatlth * sizeof(INT32) );
		} /*endif*/
		new_hdr = FALSE;
		first_call = FALSE;
	} /*endif*/

	/* append input samples to output array */
	smpptr = dat[stageno-1].ptr;
	smplth = dat[stageno-1].lth;
	if  (wdatidx+smplth > wdatlth)  {
		fprintf( stderr, "%s: tmp output array too small.  Abort.\n",
			pa_progname() );
		exit( 1 );
	} /*endif*/
	for  (i=0; i<smplth; i++)
		wdat[wdatidx+i] = smpptr[i];
	wdatidx += smplth;

	/* write output SEED file if enough samples are available */
	if  (wdatidx > MINSAMPLES)  {
		status = cBcNoError;
		SeedEncodeSteim1( fileno(seed), wdat, &prevsmp, &wdatidx, &cntime,
			wrk, wrklth, FALSE, &status );
		if  (SySevere(&status))  err_writemsg( status, "", TRUE );
	} /*endif*/

} /* end of TFrWriteData */



/*----------------------------------------------------------------------------*/

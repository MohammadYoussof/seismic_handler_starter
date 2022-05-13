
/* file k2vax_seed.c
 *      ============
 *
 * version 3, 23-Feb-95
 *
 * reads k-files on VAX and creates Mini-SEED files.  The output files
 * must be swapped on the Sun with the program swap_vax_recs.
 * K. Stammler, 23-Nov-94
 */



#include <stdio.h>
#include <string.h>
#include <descrip.h>
#include "../basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_TCUSRDEF
#include BC_ERUSRDEF
#include BC_CPAR
#include "seedcfg.h"
#include "seed_lib.h"



/* number of channels */
#define TRCNUM 19

/* number of samples per 10sec block */
#define SMPNUM 200

/* length of decompressed channel data buffer */
#define BUFLTH 10000

/* workspace for decompression routine */
#define WRKSIZE 8192

/* minimum length of channel buffer for writing (without flush) */
#define MINBUFLTH 4000

/* time step of 10 sec */
#define SEC10 10.0



/* global variables */
static SeedSetupParT k2v_setup;          /* SEED setup parameters */
static SeedSbyteT    k2v_wrk[WRKSIZE*4]; /* work space */
static int           k2v_wrklth;         /* length of work space in records */
static char          *k2v_statname[TRCNUM] =  /* station names */
{
	"GRA1", "GRA1", "GRA1", "GRA2", "GRA3", "GRA4",
	"GRB1", "GRB1", "GRB1", "GRB2", "GRB3", "GRB4", "GRB5",
	"GRC1", "GRC1", "GRC1", "GRC2", "GRC3", "GRC4"
};
static char          *k2v_channame[TRCNUM] = /* channel ID's */
{
	"BHZ", "BHN", "BHE", "BHZ", "BHZ", "BHZ",
	"BHZ", "BHN", "BHE", "BHZ", "BHZ", "BHZ", "BHZ",
	"BHZ", "BHN", "BHE", "BHZ", "BHZ", "BHZ"
};



/* prototypes of local routines */
static void ReadHeaderFile( char hdrfile[], SeedDataHeaderT *hdr,
	STATUS *status );



int main( int argc, char *argv[] )
{
	/* local variables */
	static int smp[TRCNUM][SMPNUM];     /* sample data returned from rdisc */
	int      *buf[TRCNUM];              /* buffers for traces */
	long     buflth[TRCNUM];            /* length of buffer */
	int      reccnt[TRCNUM];            /* record counters */
	SeedDataHeaderT hdr[TRCNUM];        /* header prototypes */
	int      last_smp[TRCNUM];          /* last sample of channel */
	FILE     *seed[TRCNUM];             /* pointers to mini SEED files */
	char     path[BC_FILELTH+1];        /* directory of data */
	char     t_start[BC_TIMELTH+1];     /* start time */
	char     t_end[BC_TIMELTH+1];       /* end time */
	char     t_curr[BC_TIMELTH+1];      /* current time */
	char     t_req[BC_TIMELTH+1];       /* requested time */
	char     t_exp[BC_TIMELTH+1];       /* expected time */
	char     t_lastblk[BC_TIMELTH+1];   /* time of last block (start) */
	STATUS   status;                    /* return status */
	int      i, j;                      /* counters */
	NTIME    ntime;                     /* numeric time */
	NTIME    ctime[TRCNUM];             /* current time of channel */
	int      short_year;                /* two-digit-year */
	char     str[BC_LINELTH+1];         /* scratch string */
	long     gapflags;                  /* data gap detected */
	BOOLEAN  datagap;                   /* gap in data */
	BOOLEAN  chatty;                    /* more output */
	$DESCRIPTOR( dsc_start, " " );      /* start time descriptor */
	$DESCRIPTOR( dsc_curr, " " );       /* current time descriptor */
	$DESCRIPTOR( dsc_path, " " );       /* path descriptor */

	/* executable code */

	status = BC_NOERROR;

	/* initalize variables */
	for  (i=0; i<TRCNUM; i++)  {
		buf[i] = sy_allocmem( BUFLTH, sizeof(int), &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		buflth[i] = 0;
		reccnt[i] = 1;
		last_smp[i] = 0;
		sprintf( str, "%s-%s.hdr", k2v_statname[i], k2v_channame[i] );
		strncpy( hdr[i].seqno, "000001", 6 );
		hdr[i].indicator = 'D';
		hdr[i].Reserved_bytes_A = '\0';
		strcpy( hdr[i].statcode, k2v_statname[i] );
		strncpy( hdr[i].locid, "  ", 2 );
		strncpy( hdr[i].channel, k2v_channame[i], 3 );
		strncpy( hdr[i].Reserved_bytes_B, "  ", 2 );
		hdr[i].smprate_fact = 20;
		hdr[i].smprate_mult = 1;
		hdr[i].activity = 0;
		hdr[i].ioflags = 0;
		hdr[i].quality = 0;
		hdr[i].timecorr = 0;
		seed[i] = NULL;
	} /*endfor*/

	pa_init( argc, argv );
	if  (pa_pnumber() != 3)  {
		fprintf( stderr, "Usage: %s <dir> <start-time> <end-time> ***\n",
			argv[0] );
		return 1;
	} /*endif*/
	strcpy( path, pa_pvalue(1) );
	strcpy( t_start, pa_pvalue(2) );
	strcpy( t_end, pa_pvalue(3) );
	chatty = pa_qspecified( "-chatty" );

	if  (*t_end == '+')  {
		sscanf( t_end, "%d", &i );
		tc_tadd( t_start, (float)i, t_end, &status );
		if  (Severe(&status))  err_writemsg( status, t_end, TRUE );
	} /*endif*/
	tc_tadd( t_start, 0.0, t_start, &status );  /* reformat */
	strcpy( t_req, t_start );
	tc_tadd( t_end, -SEC10, t_lastblk, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	i = strlen( path ) - 1;
	if  (path[i] != ':' && path[i] != ']')  strcat( path, ":" );

	/* get other parameters */
	SeedSetup( &k2v_setup, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	k2v_wrklth = WRKSIZE*4 / k2v_setup.seed_rec_size;
	dsc_path.dsc$w_length = strlen( path );
	dsc_path.dsc$a_pointer = path;
	dsc_curr.dsc$w_length = BC_TIMELTH;
	dsc_curr.dsc$b_dtype = DSC$K_DTYPE_T;
	dsc_curr.dsc$b_class = DSC$K_CLASS_S;
	dsc_curr.dsc$a_pointer = t_curr;

	strcpy( t_exp, t_start );
	strcpy( t_curr, t_start );
	FOREVER  {

		/* put requested time and read */
		dsc_start.dsc$w_length = strlen( t_req );
		dsc_start.dsc$a_pointer = t_req;
		if  (chatty)
			printf( "-->k2vax_seed:  reading %s\r", t_exp );
		k_read10sec( &dsc_start, &dsc_path, &dsc_curr, smp );

		/* get current start time */
		tc_t2n( t_curr, &ntime, &status );
		if  (Severe(&status))  err_writemsg( status, t_exp, TRUE );
		short_year = ntime.year;
		if  (short_year > 1900)  short_year -= 1900;
		if  (short_year > 100)  short_year -= 100;

		/* open data files and copy start time if necessary */
		for  (i=0; i<TRCNUM; i++)  {
			if  (seed[i] == NULL)  {
				sprintf( str, "%s_%02d%02d%02d_%02d%02d.%s", k2v_statname[i],
					short_year, ntime.month, ntime.day, ntime.hour, ntime.min,
					k2v_channame[i] );
				seed[i] = fopen( str, "w" );
				if  (seed[i] == NULL)  {
					fprintf( "k2vax_seed: error opening output file %s", str );
					return 1;
				} /*endif*/
				ctime[i] = ntime;
			} /*endif*/
		} /*endfor*/

		/* if data gap, flush remaining samples in buffers and reset time */
		datagap = (tc_tdiff(t_curr,t_exp,&status) != 0.0);
		if  (Severe(&status))  err_writemsg( status, t_curr, TRUE );
		if  (datagap)  {
			fprintf( stderr, "k2vax_seed: time gap on all from %s to %s\n",
				t_exp, t_curr );
			for  (i=0; i<TRCNUM; i++)  {
				SeedEncodeSteim1Chan( fileno(seed[i]), buf[i], last_smp+i,
					buflth+i, ctime+i, reccnt+i, hdr+i, k2v_wrk, k2v_wrklth,
					TRUE, &status );
				ctime[i] = ntime;
			} /*endfor*/
		} /*endif*/

		/* check for zeroes */
		gapflags = 0;
		for  (i=0; i<TRCNUM; i++)  {
			for  (j=0; j<SMPNUM; j++)
				if  (smp[i][j] != 0)  break;
			if  (j >= SMPNUM)  {
				fprintf( stderr, "k2vax_seed: 10sec time gap on %s-%s at %s\n",
					k2v_statname[i], k2v_channame[i], t_exp );
				if  (buflth[i] > 0)
					SeedEncodeSteim1Chan( fileno(seed[i]), buf[i], last_smp+i,
						buflth+i, ctime+i, reccnt+i, hdr+i, k2v_wrk, k2v_wrklth,
						TRUE, &status );
				tc_nadd( &ntime, SEC10, ctime+i, &status );
				if  (Severe(&status))  err_writemsg( status, t_curr, TRUE );
				gapflags |= (1<<i);
			} /*endif*/
		} /*endfor*/

		/* append samples to channel buffers */
		for  (i=0; i<TRCNUM; i++)  {
			if  ((gapflags & (1<<i)) == 0)  {
				if  (buflth[i]+SMPNUM > BUFLTH)  {
					fprintf( stderr,
						"k2vax_seed: channel buffer exceeded for %s-%s\n",
						k2v_statname[i], k2v_channame[i] );
					return 1;
				} /*endif*/
				for  (j=0; j<SMPNUM; j++)
					buf[i][(buflth[i])++] = smp[i][j];
			} /*endif*/
		} /*endfor*/

		/* write new data */
		for  (i=0; i<TRCNUM; i++)  {
			if  (buflth[i] > MINBUFLTH && (gapflags & (1<<i)) == 0)  {
				SeedEncodeSteim1Chan( fileno(seed[i]), buf[i], last_smp+i,
					buflth+i, ctime+i, reccnt+i, hdr+i, k2v_wrk, k2v_wrklth,
					FALSE, &status );
				if  (Severe(&status))  err_writemsg( status, t_exp, TRUE );
			} /*endif*/
		} /*endfor*/

		/* if end time reached exit */
		if  (tc_tdiff(t_lastblk,t_curr,&status) <= 0.0)  break;
		if  (Severe(&status))  err_writemsg( status, "(a2)", TRUE );

		/* process counts read */
		tc_tadd( t_curr, SEC10, t_exp, &status );
		if  (Severe(&status))  err_writemsg( status, t_curr, TRUE );
		strcpy( t_req, "    " );

	} /*endfor*/

	/* flush remaining samples */
	for  (i=0; i<TRCNUM; i++)  {
		SeedEncodeSteim1Chan( fileno(seed[i]), buf[i], last_smp+i,
			buflth+i, ctime+i, reccnt+i, hdr+i, k2v_wrk, k2v_wrklth,
			TRUE, &status );
	} /*endfor*/

	/* close files */
	for  (i=0; i<TRCNUM; i++)
		if  (seed[i] != NULL)
			fclose( seed[i] );

} /* end of main */



/*----------------------------------------------------------------------------*/



#define KEYNUM_SEQNO 0
#define KEYNUM_INDICATOR 1
#define KEYNUM_STATION 2
#define KEYNUM_LOCATION 3
#define KEYNUM_CHANNEL 4
#define KEYNUM_STARTTIME 5
#define KEYNUM_SMPFACT 6
#define KEYNUM_SMPMULT 7
#define KEYNUM_ACTIVITY 8
#define KEYNUM_IOFLAGS 9
#define KEYNUM_QUALITY 10
#define KEYNUM_TIMECORR 11
#define KEYWORDNUM 12



static void ReadHeaderFile( char hdrfile[], SeedDataHeaderT *hdr,
	STATUS *status )

/* Reads in data header prototype
 *
 * parameters of routine
 * char       hdrfile[];           input; name of header file
 * SeedDataHeaderT *hdr;           output; header prototype
 * STATUS     *status;             output; return status
 */
{
	/* local variables */
	static char *keyword[] = {
		"sequence number:",
		"indicator:",
		"station:",
		"location ID:",
		"channel:",
		"start time:",
		"sample rate factor:",
		"sample rate multiplier:",
		"activity flags:",
		"IO flags:",
		"quality flags:",
		"time correction:"
	};
	FILE     *hf;                 /* pointer to header file */
	char     line[BC_LINELTH+1];  /* current line in header file */
	int      keynum;              /* keyword number */
	int      slen;                /* string length */
	BOOLEAN  key_found;           /* keyword found */
	int      tmp;                 /* scratch */
	char     str[BC_LINELTH+1];   /* scratch */
	NTIME    ntime;               /* numeric time */

	/* executable code */

	tc_t2n( "1-jun-1970_0:0", &ntime, status );
	if  (Severe(status))  return;

	strcpy( hdr->seqno, "000001" );
	hdr->indicator = 'D';
	hdr->Reserved_bytes_A = '\0';
	strcpy( hdr->statcode, "XXXX" );
	hdr->locid[0] = hdr->locid[1] = ' ';
	hdr->channel[0] = hdr->channel[1] = hdr->channel[2] = 'X';
	hdr->Reserved_bytes_B[0] = hdr->Reserved_bytes_B[1] = '\0';
	SeedNtimeToBtime( &ntime, &(hdr->starttime), status );
	if  (Severe(status))  return;
	hdr->no_of_samples = 0;
	hdr->smprate_fact = 0;
	hdr->smprate_mult = 0;
	hdr->activity = 0;
	hdr->ioflags = 0;
	hdr->quality = 0;
	hdr->no_of_blockettes = 0;
	hdr->timecorr = 0;
	hdr->databegin = sizeof( SeedDataHeaderT );
	hdr->first = 0;

	/* open header file */
	hf = sy_fopen( hdrfile, "r" );
	if  (hf == NULL)  {
		fprintf( stderr, "write_steim1: header file %s not found\n", hdrfile );
		*status = 1;
		return;
	} /*endif*/

	while  (fgets(line,BC_LINELTH,hf) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		key_found = FALSE;
		for  (keynum=0; keynum<KEYWORDNUM; keynum++)  {
			slen = strlen( keyword[keynum] );
			if  (strncmp(keyword[keynum],line,slen) == 0)  {
				key_found = TRUE;
				break;
			} /*endif*/
		} /*endfor*/
		if  (!key_found)  {
			fprintf( stderr, "write_steim1: illegal line in header file:\n%s",
				line );
			continue;
		} /*endif*/
		switch  (keynum)  {
		case KEYNUM_SEQNO:
			sscanf( line+slen, "%d", &tmp );
			if  (tmp > 999999)  tmp = 0;
			*str = hdr->indicator;
			sprintf( hdr->seqno, "%06d", tmp );
			hdr->indicator = *str;
			break;
		case KEYNUM_INDICATOR:
			sscanf( line+slen, "%c", &(hdr->indicator) );
			break;
		case KEYNUM_STATION:
			tmp = hdr->locid[0];
			sscanf( line+slen, "%s", str );
			if  (strlen(str) > 5)  {
				fprintf( stderr, "write_steim1: name %s truncated ", str );
				str[5] = '\0';
				fprintf( stderr, "to %s\n", str );
			} /*endif*/
			strcpy( hdr->statcode, str );
			hdr->locid[0] = tmp;
			break;
		case KEYNUM_LOCATION:
			sscanf( line+slen, "%s", str );
			if  (strlen(str) > 2)  {
				fprintf( stderr, "write_steim1: illegal loc length %s\n", str );
				*status = 1;
				return;
			} /*endif*/
			hdr->locid[0] = str[0]; hdr->locid[1] = str[1];
			break;
		case KEYNUM_CHANNEL:
			sscanf( line+slen, "%s", str );
			if  (strlen(str) > 3)  {
				fprintf( stderr, "write_steim1: illegal channel length %s\n", str );
				*status = 1;
				return;
			} /*endif*/
			hdr->channel[0] = str[0];
			hdr->channel[1] = str[1];
			hdr->channel[2] = str[2];
			break;
		case KEYNUM_STARTTIME:
			sscanf( line+slen, "%s", str );
			tc_t2n( str, &ntime, status );
			if  (Severe(status))  return;
			SeedNtimeToBtime( &ntime, &(hdr->starttime), status );
			if  (Severe(status))  return;
			break;
		case KEYNUM_SMPFACT:
			sscanf( line+slen, "%d", &tmp );
			hdr->smprate_fact = tmp;
			break;
		case KEYNUM_SMPMULT:
			sscanf( line+slen, "%d", &tmp );
			hdr->smprate_mult = tmp;
			break;
		case KEYNUM_ACTIVITY:
			sscanf( line+slen, "%d", &tmp );
			hdr->activity = (UBYTE)tmp;
			break;
		case KEYNUM_IOFLAGS:
			sscanf( line+slen, "%d", &tmp );
			hdr->ioflags = (UBYTE)tmp;
			break;
		case KEYNUM_QUALITY:
			sscanf( line+slen, "%d", &tmp );
			hdr->quality = (UBYTE)tmp;
			break;
		case KEYNUM_TIMECORR:
			sscanf( line+slen, "%d", &tmp );
			hdr->ioflags = tmp;
			break;
		default:
			fprintf( stderr, "write_steim1: program bug (1)\n" );
			return;
		} /*endswitch*/
	} /*endwhile*/

	sy_fclose( hf );

} /* end of ReadHeaderFile */



/*----------------------------------------------------------------------------*/

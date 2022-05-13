
/* file seed_tidy.c
 *      ===========
 *
 * version 40, 16-Feb-2007
 *
 * Tidies up SEED directory: data are organized in day-files/month-files per
 * stream.  The existing records are not changed, they are just merged.  The
 * day files do not exactly start at 0:00 but some seconds later (when the first
 * new block starts).  After executing this program a new sfd-file must be
 * created.
 * seed_tidy doesn't recognize if a file contains gap data of another file,
 * it just leaves the gap data in a separate file.  Use seedmerge to fix
 * this.
 * seed_tidy changes station name IBB to IBBN on all written records.
 * seed_tidy changes streams GRFO-LH-{1,2,3} to GRA1-LH-{Z,N,E}
 * K. Stammler, 1-Aug-94
 */


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#ifdef BC_SUN
#include <fcntl.h>
#include <sys/types.h>
#include <sys/uio.h>
#endif
#include "sysbase.h"
#include "erusrdef.h"
#include "tcusrdef.h"
#include "utusrdef.h"
#include "cpar.h"
#include "seedcfg.h"
#include "seed_lib.h"
#include "unix_io.h"


/* constants */
#define MAX_STREAM 250
#define TOLERANCE_TIME_BH 300.0
#define TOLERANCE_TIME_LH 6000.0
#define TOLERANCE_TIME_VH 60000.0
#define TOLERANCE_TIME_UH 600000.0
#define LIST_CREATED "tidy.created"
#define LIST_READ "tidy.readin"
#define FILE_PREFIX "x."
#define MAX_REC_SMP 6000

/* flags */
#define F_COMPLETE_COPY 0x000001
#define F_IGNORE_LAST   0x000002
#define F_DELETE_OLD    0x000004
#define F_OLDNAMES      0x000008
#define F_FIXCHECK      0x000010
#define F_SWAP          0x000020
#define F_LONGNAMES     0x000040
#define F_NONEGGAPS     0x000080
#define F_LINUXDATA     0x000100
#define F_FIXSTATCODE   0x000200
#define F_GRAX2GRA1     0x000400
#define F_NEWSTAT       0x000800
#define F_SWITCHHOUR    0x001000
#define F_ONEFILE       0x002000
#define F_RELTIMECORR   0x004000
#define F_NEWCHAN       0x008000
#define F_SINGLERECS    0x010000
#define F_SETNETWORK    0x020000
#define F_SETBLOCK1000  0x040000
#define F_SWITCHDAY     0x080000
#define F_YQFIXNAMES    0x100000
#define F_TBFIXNAMES    0x200000


/* global variables */
static char tdv_progname[BC_FILELTH+1];   /* program name */
static SeedSbyteT *tdv_seedrec;           /* pointer to scratch buffer */
static FILE *tdv_created, *tdv_read;      /* pointers to log files */
static float tdv_maxgapfac=12000.0;       /* max. gap lth in dt within a file */
static char tdv_newstation[cBcLineLth+1]=""; /* new station name */
static char tdv_newchan[cBcLineLth+1]=""; /* new channel name */
static char tdv_newnetwork[cBcLineLth+1]=""; /* new network code */
static int tdv_max_recsamp=0;             /* maximum number of samples per rec*/
static int tdv_timecorr=0;                /* time correction */
static float tdv_timeinc=0.0;             /* time increment */
static int tdv_setyear=0;                 /* year to set */
static int tdv_endian=1;                  /* default to bigendian */



/* prototypes of local routines */
static void TdGetStreamList( char sfdfile[],
	char streams[MAX_STREAM][BC_SHORTSTRLTH+1], int *lth, STATUS *status );
static void TdGetSfdList( char sfdfile[], char stream[], SeedFileDescrT **dsc,
	int *dsclth, STATUS *status );
static void TdSortSfdList( SeedFileDescrT *dsc, int dsclth, STATUS *status );
static BOOLEAN TdFileIsTidy( SeedFileDescrT *dsc );
static void TdRecopyData( char sfdfile[], SeedFileDescrT dsc[], int dsclth,
	int flags, STATUS *status );
static void TdLastFile( char sfdfile[], char stream[], SeedFileDescrT *dsc );
static void TdCopyFile( SeedFileDescrT *dsc, int flags );
static void TdPutRecord( SeedSbyteT rec[], int flags );
static void TdDeleteList( char fname[] );
static void TdRenameList( char fname[] );
static void TdFixChecksum( SeedSbyteT rec[], int flags );
static void TdPut4Bytes( unsigned INT32 l, BOOLEAN swap, SeedSbyteT b[] );
static void TdIncrementTime( SeedDataHeaderT *hdr, float inc );
static void TdFixYqnames( char name[] );
static void TdFixTbnames( char name[] );
int TdQsortCompare( const void *d1, const void *d2 );



int main( int argc, char *argv[] )
{
	/* local variables */
	static char streams[MAX_STREAM][BC_SHORTSTRLTH+1];  /* list of streams */
	int      no_of_streams;             /* number of streams */
	STATUS   status;                    /* return status */
	char     sfdfile[BC_FILELTH+1];     /* name of sfd-file */
	int      s;                         /* stream counter */
	SeedFileDescrT *dsc;                /* pointer to list of descriptors */
	int      dsclth;                    /* length of descriptor list */
	int      flags;                     /* some switches */

	/* executable code */

	status = BC_NOERROR;
	strcpy( tdv_progname, argv[0] );

	pa_init( argc, argv );
	if  (pa_pnumber() != 1)  {
		fprintf( stderr, "Usage: %s <sfd-file>\n", tdv_progname );
		fprintf( stderr, "   qualifiers:\n" );
		fprintf( stderr, "   -complete     recopy tidy files as well\n" );
		fprintf( stderr, "   -keep_last    leave last file of chan untouched\n" );
		fprintf( stderr, "   -keep_old     keep old input files\n" );
		fprintf( stderr, "   -oldnames     use old names\n" );
		fprintf( stderr, "   -longnames    use long names\n" );
		fprintf( stderr, "   -fixcheck     fill empty checksums\n" );
		fprintf( stderr, "   -linux        input data are generated on Linux\n" );
		fprintf( stderr, "   -fixstat      clears unprintable chars in statn\n" );
		fprintf( stderr, "   -swap         swap data (for fixcheck only)\n" );
		fprintf( stderr, "   -hours        generate hour-files\n" );
		fprintf( stderr, "   -onefile      generate single output file\n" );
		fprintf( stderr, "   -singlerecs   generate file for each record\n" );
		fprintf( stderr, "   -grax2gra1    changes station from GRAX to GRA1\n" );
		fprintf( stderr, "   -yqfixnames   fixes station name bug of Scream\n" );
		fprintf( stderr, "   -tbfixnames   fixes station name bug of Scream\n" );
		fprintf( stderr, "   -station=<stat> replaces station name by <stat>\n" );
		fprintf( stderr, "   -chan=<chan>  replaces channel by <chan> (bhz)\n" );
		fprintf( stderr, "   -network=<net> replaces network code by <net>\n" );
		fprintf( stderr, "   -b1000        insert blockette 1000 (Sparc=big-endian)\n" );
		fprintf( stderr, "   -b1000l       insert blockette 1000 (intel=little-endian)\n" );
		fprintf( stderr, "   -maxgapfac=<factor> max gap in dt within files\n" );
		fprintf( stderr, "   -maxrecsmp=<number> max no of samples in record\n" );
		fprintf( stderr, "   -abstimecorr=<corr> abs. time correction in ms\n" );
		fprintf( stderr, "   -reltimecorr=<corr> rel. time correction in ms\n" );
		fprintf( stderr, "   -timeinc=<inc> incr. all header times by <inc>\n" );
		fprintf( stderr, "   -setyear=<year> sets year to new value\n" );
		fprintf( stderr, "   -noneggaps    no negative gaps allowed\n" );
		fprintf( stderr, "                 (throws away records back in time)\n");
		return 1;
	} /*endif*/
	strcpy( sfdfile, pa_pvalue(1) );
	flags = F_DELETE_OLD;
	if  (pa_qspecified( "-complete" ))     flags |= F_COMPLETE_COPY;
	if  (pa_qspecified( "-keep_last" ))    flags |= F_IGNORE_LAST;
	if  (pa_qspecified( "-keep_old" ))     flags &= ~F_DELETE_OLD;
	if  (pa_qspecified( "-oldnames" ))     flags |= F_OLDNAMES;
	if  (pa_qspecified( "-longnames" ))    flags |= F_LONGNAMES;
	if  (pa_qspecified( "-fixcheck" ))     flags |= F_FIXCHECK;
	if  (pa_qspecified( "-swap" ))         flags |= F_SWAP;
	if  (pa_qspecified( "-hours" ))        flags |= F_SWITCHHOUR;
	if  (pa_qspecified( "-days" ))         flags |= F_SWITCHDAY;
	if  (pa_qspecified( "-onefile" ))      flags |= F_ONEFILE;
	if  (pa_qspecified( "-singlerecs" ))   flags |= F_SINGLERECS;
	if  (pa_qspecified( "-noneggaps" ))    flags |= F_NONEGGAPS;
	if  (pa_qspecified( "-linux" ))        flags |= F_LINUXDATA;
	if  (pa_qspecified( "-fixstat" ))      flags |= F_FIXSTATCODE;
	if  (pa_qspecified( "-grax2gra1" ))    flags |= F_GRAX2GRA1;
	if  (pa_qspecified( "-yqfixnames" ))   flags |= F_YQFIXNAMES;
	if  (pa_qspecified( "-tbfixnames" ))   flags |= F_TBFIXNAMES;
	if  (pa_qspecified( "-reltimecorr" ))  flags |= F_RELTIMECORR;
	if  (pa_qspecified( "-b1000" ))        flags |= F_SETBLOCK1000;
	if  (pa_qspecified( "-b1000l" ))  {
		flags |= F_SETBLOCK1000;
		tdv_endian=0;
	} /*endif*/
	if  (pa_qspecified( "-station" ))    {
		flags |= F_NEWSTAT;
		strcpy( tdv_newstation, pa_qvalue("-station") );
	} /*endif*/
	if  (pa_qspecified( "-chan" ))    {
		flags |= F_NEWCHAN;
		strcpy( tdv_newchan, pa_qvalue("-chan") );
	} /*endif*/
	if  (pa_qspecified( "-network" ))    {
		flags |= F_SETNETWORK;
		strcpy( tdv_newnetwork, pa_qvalue("-network") );
	} /*endif*/
	if  (pa_qspecified( "-maxgapfac" ))
		sscanf( pa_qvalue("-maxgapfac"), "%f", &tdv_maxgapfac );
	if  (pa_qspecified( "-maxrecsmp" ))
		sscanf( pa_qvalue("-maxrecsmp"), "%d", &tdv_max_recsamp );
	if  (pa_qspecified( "-reltimecorr" ))
		sscanf( pa_qvalue("-reltimecorr"), "%d", &tdv_timecorr );
	if  (pa_qspecified( "-abstimecorr" ))
		sscanf( pa_qvalue("-abstimecorr"), "%d", &tdv_timecorr );
	if  (pa_qspecified( "-timeinc" ))
		sscanf( pa_qvalue("-timeinc"), "%f", &tdv_timeinc );
	if  (pa_qspecified( "-setyear" ))
		sscanf( pa_qvalue("-setyear"), "%d", &tdv_setyear );

	/* initialize seed routines */
	SeedLibInitialize( &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* allocate scratch buffer */
	tdv_seedrec = (SeedSbyteT *)sy_allocmem( 1, Seed_C_MAX_RECLTH, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* get stream list from sfd file */
	TdGetStreamList( sfdfile, streams, &no_of_streams, &status );
	if  (Severe(&status))  err_writemsg( status, "", TRUE );

	/* loop all streams */
	for  (s=0; s<no_of_streams; s++)  {
		TdGetSfdList( sfdfile, streams[s], &dsc, &dsclth, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		TdSortSfdList( dsc, dsclth, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		TdRecopyData( sfdfile, dsc, dsclth, flags, &status );
		if  (Severe(&status))  err_writemsg( status, "", TRUE );
		sy_deallocmem( dsc );
	} /*endfor*/

	return 0;

} /* end of main */



/*---------------------------------------------------------------------------*/



static void TdGetStreamList( char sfdfile[],
	char streams[MAX_STREAM][BC_SHORTSTRLTH+1], int *lth, STATUS *status )

/* Retrieves list of streams from sfd-file
 *
 * parameters of routine
 * char       sfdfile[];            input; name of sfd-file
 * char       streams[MAX_STREAM][BC_SHORTSTRLTH+1]; output; list of streams
 * int        *lth;                 output; number of streams found
 * STATUS     *status;              output; return status
 */
{
	/* local variables */
	FILE     *sfd;                       /* pointer to sfd-file */
	char     line[Seed_C_SFDLINELTH+1];  /* current sfd-line */
	SeedFileDescrT dsc;                  /* SEED file descriptor */
	int      i;                          /* counter */
	BOOLEAN  found;                      /* found stream in list */

	/* executable code */

	/* open input file */
	sfd = fopen( sfdfile, "r" );
	if  (sfd == NULL)  {
		fprintf( stderr, "%s: sfd-file %s not found\n", tdv_progname, sfdfile );
		*status = 1;
		return;
	} /*endif*/

	*lth = 0;
	while  (fgets(line,Seed_C_SFDLINELTH,sfd) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		SeedParseSfdLine( line, &dsc, status );
		if  (Severe(status))  {fclose(sfd); return;}
		/* find name in list */
		found = FALSE;
		for  (i=0; i<(*lth); i++)  {
			found = (strcmp(dsc.stream,streams[i]) == 0);
			if  (found)  break;
		} /*endfor*/
		/* if not found append to list */
		if  (!found)  {
			if  ((*lth) == MAX_STREAM)  {
				fprintf( stderr, "%s: too many streams in sfd-file %s\n",
					tdv_progname, sfdfile );
				*status = 1;  fclose( sfd );
				return;
			} /*endif*/
			strcpy( streams[(*lth)++], dsc.stream );
		} /*endif*/
	} /*endwhile*/

	fclose( sfd );

} /* end of TdGetStreamList */



/*---------------------------------------------------------------------------*/



static void TdGetSfdList( char sfdfile[], char stream[], SeedFileDescrT **dsc,
	int *dsclth, STATUS *status )

/* Retrieves sfd descriptor list from sfd-file.
 *
 * parameters of routine
 * char       sfdfile[];          input; name of sfd-file
 * char       stream[];           input; name of stream
 * SeedFileDescrT **dsc;          output; list of descriptors
 * STATUS     *status;            output; return status
 */
{
	/* local variables */
	FILE     *sfd;                       /* pointer to sfd-file */
	char     line[Seed_C_SFDLINELTH+1];  /* line of sfd-file */
	SeedFileDescrT xdsc;                 /* dummy descriptor */
	int      chan_cnt;                   /* channel counter */

	/* executable code */

	/* open input file */
	sfd = fopen( sfdfile, "r" );
	if  (sfd == NULL)  {
		fprintf( stderr, "%s: sfd-file %s not found\n", tdv_progname, sfdfile );
		*status = 1;
		return;
	} /*endif*/

	/* determine number of descriptors for this channel */
	chan_cnt = 0;
	while  (fgets(line,Seed_C_SFDLINELTH,sfd) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		SeedParseSfdLine( line, &xdsc, status );
		if  (Severe(status))  {fclose(sfd); return;}
		if  (strcmp(stream,xdsc.stream) == 0)  chan_cnt++;
	} /*endwhile*/

	/* allocate memory */
	*dsclth = chan_cnt;
	if  (chan_cnt == 0)  {fclose(sfd); return;}
	*dsc = sy_allocmem( chan_cnt, (int)sizeof(SeedFileDescrT), status );
	if  (Severe(status))  {fclose(sfd); return;}

	/* read descriptors */
	rewind( sfd );
	chan_cnt = 0;
	while  (fgets(line,Seed_C_SFDLINELTH,sfd) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		SeedParseSfdLine( line, (*dsc)+chan_cnt, status );
		if  (Severe(status))  {fclose(sfd); return;}
		if  (strcmp(stream,(*dsc)[chan_cnt].stream) == 0)  chan_cnt++;
		if  (chan_cnt == *dsclth)  break;
	} /*endwhile*/

	fclose( sfd );

} /* end of TdGetSfdList */



/*---------------------------------------------------------------------------*/



static void TdSortSfdList( SeedFileDescrT *dsc, int dsclth, STATUS *status )

/* sorts sfd list by time
 *
 * parameters of routine
 * SeedFileDescrT *dsc;        modify; list of dsecriptors to be sorted
 * int            dsclth;      input; length of list
 * STATUS         *status;     output; return status
 */
{
	/* local variables */
	SeedFileDescrT xdsc;      /* temp buffer */
	BOOLEAN  again;           /* loop again */
	int      i;               /* counter */
	float    tdiff;           /* time difference */

	/* executable code */

	if  (dsclth <= 1)  return;

	qsort( dsc, dsclth, sizeof(SeedFileDescrT), &TdQsortCompare );

#ifdef XXX

	do  {
		again = FALSE;
		for  (i=1; i<dsclth; i++)  {
			tdiff = tc_tdiff( dsc[i].t_start, dsc[i-1].t_start, status );
			if  (Severe(status))  return;
			if  (tdiff < 0.0)  {
				/* exchange i and i-1 and set again */
				xdsc = dsc[i-1];
				dsc[i-1] = dsc[i];
				dsc[i] = xdsc;
				again = TRUE;
			} /*endif*/
		} /*endfor*/
	}  while  (again);

#endif

} /* end of TdSortSfdList */



/*---------------------------------------------------------------------------*/



int TdQsortCompare( const void *d1, const void *d2 )

/* comparsion routine for use in qsort
 *
 * parameters of routine
 * SeedFileDescrT  *d1, *d2;   input; file descriptors to compare
 */
{
	/* local variables */
	SeedFileDescrT   *dsc1, *dsc2;  /* pointer to file descriptors */
	float    tdiff;                 /* time difference */
	TSyStatus locstat;              /* local status variable */

	/* executable code */

	dsc1 = (SeedFileDescrT *)d1;
	dsc2 = (SeedFileDescrT *)d2;
	locstat = cBcNoError;
	tdiff = tc_tdiff( dsc1->t_start, dsc2->t_start, &locstat );
	if  (tdiff < 0.0)  return -1;
	if  (tdiff > 0.0)  return 1;
	return 0;

} /*endif*/



/*---------------------------------------------------------------------------*/



static void TdRecopyData( char sfdfile[], SeedFileDescrT dsc[], int dsclth,
	int flags, STATUS *status )

/* recopies data files.
 *
 * parameters of routine
 * char       sfdfile[];        input; sfd-file
 * SeedFileDescrT dsc[];        input; array of SEED file descriptors
 * int        dsclth;           input; length of above array
 * int        flags;            input; some switches
 * STATUS     *status;          output; return status
 */
{
	/* local variables */
	int      i;               /* counter */
	SeedFileDescrT last_dsc;  /* descriptor of last file */

	/* executable code */

	/* open log files, routines TdCopyFile and TdPutRecord write to them */
	tdv_created = fopen( LIST_CREATED, "w" );
	if  (tdv_created == NULL)  {
		fprintf( stderr, "%s: couldn't open log %s\n",
			tdv_progname, LIST_CREATED );
		return;
	} /*endif*/
	tdv_read = fopen( LIST_READ, "w" );
	if  (tdv_read == NULL)  {
		fprintf( stderr, "%s: couldn't open log %s\n",
			tdv_progname, LIST_READ );
		fclose( tdv_created );
		return;
	} /*endif*/

	/* find last file of channel if requested */
	if  (F_IGNORE_LAST & flags)
		TdLastFile( sfdfile, dsc[0].stream, &last_dsc );

	/* loop all files of descriptor list */
	for  (i=0; i<dsclth; i++)  {
		if  (!(F_COMPLETE_COPY & flags) && TdFileIsTidy(dsc+i))  continue;
		if  (F_IGNORE_LAST & flags)  {
			if  (strcmp(dsc[i].stream,last_dsc.stream) != 0)
				TdLastFile( sfdfile, dsc[i].stream, &last_dsc );
			if  (strcmp(dsc[i].name,last_dsc.name) == 0)  continue;
		} /*endif*/
		TdCopyFile( dsc+i, flags );
	} /*endfor*/

	TdPutRecord( NULL, flags );

	fclose( tdv_created );
	fclose( tdv_read );

	if  (F_DELETE_OLD & flags)  TdDeleteList( LIST_READ );
	TdRenameList( LIST_CREATED );

} /* end of TdRecopyData */



/*---------------------------------------------------------------------------*/



static BOOLEAN TdFileIsTidy( SeedFileDescrT *dsc )

/* checks whether file must be recopied
 *
 * parameters of routine
 * SeedFileDescrT *dsc;       input; SEED file descriptor
 *                            returns TRUE if file doesn't need recopy
 */
{
	/* local variables */
	STATUS   locstat;      /* local status */
	NTIME    ntime;        /* numeric time */
	NTIME    ctime;        /* compare time */
	BOOLEAN  is_bh, is_lh, is_vh, is_uh; /* is BB, 1Hz or 10sec channel */
	float    tol_time;     /* tolerance time */

	/* executable code */

	locstat = BC_NOERROR;

	is_bh = (strstr(dsc->stream,"-bh-") != NULL);
	is_lh = (strstr(dsc->stream,"-lh-") != NULL);
	is_vh = (strstr(dsc->stream,"-vh-") != NULL ||
		strstr(dsc->stream,"-va-") != NULL ||
		strstr(dsc->stream,"-vl-") != NULL ||
		strstr(dsc->stream,"-vd-") != NULL ||
		strstr(dsc->stream,"-mp-") != NULL);
	is_uh = (strstr(dsc->stream,"-uh-") != NULL);
	if  (is_bh)       tol_time = TOLERANCE_TIME_BH;
	else if  (is_lh)  tol_time = TOLERANCE_TIME_LH;
	else if  (is_vh)  tol_time = TOLERANCE_TIME_VH;
	else if  (is_uh)  tol_time = TOLERANCE_TIME_UH;
	else              return TRUE;

	/* check start time */
	tc_t2n( dsc->t_start, &ntime, &locstat );
	if  (Severe(&locstat))  return FALSE;
	tc_nadd( &ntime, -tol_time, &ctime, &locstat );
	if  (Severe(&locstat))  return FALSE;
	if  (is_bh)  {
		if  (ntime.day == ctime.day)  return FALSE;
	} else if  (is_lh || is_vh || is_uh)  {
		if  (ntime.month == ctime.month)  return FALSE;
	} else {
		return TRUE;
	} /*endif*/

	/* check end time */
	tc_t2n( dsc->t_end, &ntime, &locstat );
	if  (Severe(&locstat))  return FALSE;
	tc_nadd( &ntime, -tol_time, &ctime, &locstat );
	if  (Severe(&locstat))  return FALSE;
	if  (is_bh)  {
		if  (ntime.day == ctime.day)  return FALSE;
	} else if  (is_lh || is_vh || is_uh)  {
		if  (ntime.month == ctime.month)  return FALSE;
	} else {
		return TRUE;
	} /*endif*/

	return TRUE;

} /* end of TdFileIsTidy */



/*---------------------------------------------------------------------------*/



static void TdLastFile( char sfdfile[], char stream[], SeedFileDescrT *dsc )

/* checks whether file is last file of channel
 *
 * parameters of routine
 * char           sfdfile[];    input; sfd-file
 * char           stream[];     input; stream string
 * SeedFileDescrT *dsc;         output; SEED file descriptor of last file
 */
{
	/* local variables */
	FILE     *sfd;                      /* pointer to sfd-file */
	char     line[Seed_C_SFDLINELTH+1]; /* current sfd-line */
	SeedFileDescrT xdsc;                /* parsed sfd-line */
	STATUS   locstat;                   /* local status */

	/* executable code */

	locstat = BC_NOERROR;
	dsc->stream[0] = '\0';
	dsc->t_end[0] = '\0';

	/* open input file */
	sfd = fopen( sfdfile, "r" );
	if  (sfd == NULL)  {
		fprintf( stderr, "%s: sfd-file %s not found\n", tdv_progname, sfdfile );
		return;
	} /*endif*/

	while  (fgets(line,Seed_C_SFDLINELTH,sfd) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		SeedParseSfdLine( line, &xdsc, &locstat );
		if  (Severe(&locstat))  {fclose(sfd); return;}
		if  (strcmp(stream,xdsc.stream) != 0)  continue;
		if  (dsc->t_end[0] == '\0')  {
			*dsc = xdsc;
		} else {
			if  (tc_tdiff(dsc->t_end,xdsc.t_end,&locstat) < 0.0)
				*dsc = xdsc;
			locstat = BC_NOERROR;
		} /*endif*/
	} /*endwhile*/

	fclose( sfd );

} /* end of TdLastFile */



/*---------------------------------------------------------------------------*/



static void TdCopyFile( SeedFileDescrT *dsc, int flags )

/* copies file.  Puts records to routine TdPutRecord
 *
 * parameters of routine
 * SeedFileDescrT *dsc;        input; descriptor of file
 * int            flags;       input; some switches
 */
{
	/* local variables */
	int      fd;             /* UNIX file descriptor */
	int      i;              /* counter */

	/* executable code */

	fprintf( tdv_read, "%s\n", dsc->name );

	fd = open( dsc->name, O_RDONLY );
	if  (fd < 0)  {
		fprintf( stderr, "%s: couldn't open data file %s\n",
			tdv_progname, dsc->name );
		return;
	} /*endif*/
	lseek( fd, dsc->byteoff, SEEK_SET );

	for  (i=0; i<(dsc->recno); i++)  {
		if  (read(fd,(char *)tdv_seedrec,dsc->reclth) != dsc->reclth)  {
			fprintf( stderr, "%s: read error on data file %s\n",
				tdv_progname, dsc->name );
			close( fd );
			return;
		} /*endif*/
		SeedStoreReclth( (SeedDataHeaderT *)tdv_seedrec, dsc->reclth );
#ifdef XXX
		if  (F_LINUXDATA & flags)  {
			SeedSwapHeader( (SeedDataHeaderT *)tdv_seedrec );
		} /*endif*/
#endif
		TdPutRecord( tdv_seedrec, flags );
	} /*endif*/

	close( fd );

} /* end of TdCopyFile */



/*---------------------------------------------------------------------------*/



static void TdPutRecord( SeedSbyteT rec[], int flags )

/* puts SEED record to file
 *
 * parameters of routine
 * TdPutRecord   rec[];          input; SEED record
 * int           flags;          input; some switches
 */
{
	/* local variables */
	static NTIME    f_start;            /* file start time */
	static NTIME    l_end;              /* start of last record */
	static int      fd;                 /* UNIX file descriptor */
	static int      recnum=1;           /* record counter */
	static int      sngcnt=1;           /* single record counter */
	NTIME           r_start, r_end;     /* times of current record */
	NTIME           nametime;           /* time used for filename */
	STATUS          locstat;            /* local status */
	SeedDataHeaderT *shdr;              /* SEED data header */
	SeedBlockette1000T *b1000;          /* blockette 1000 */
	char            station[BC_SHORTSTRLTH+1];  /* station name */
	char            chan[BC_SHORTSTRLTH+1];     /* channel name */
	BOOLEAN         switch_days;        /* generate day-files */
	BOOLEAN         switch_hours;       /* generate hour-files */
	BOOLEAN         new_file;           /* create new file */
	BOOLEAN         hdr_swapped;        /* header is swapped */
	BOOLEAN         b1000found;         /* blockette 1000 found */
	char            fname[BC_FILELTH+1];/* name of data file */
	int             year;               /* year */
	int             write_ret;          /* return status of write */
	char            tmp;                /* temp storage */
	float           gaplth;             /* length of time gap in sec */
	float           dt;                 /* sample distance in sec */
	int             i;                  /* counter */
	char            timestr[BC_TIMELTH+1]; /* time string */
	int             reclth;             /* SEED record length in bytes */
	int             rlexp;              /* record length exponent */
	UWORD           *uw;                /* pointer to unsigned word */

	/* executable code */

	locstat = BC_NOERROR;
	hdr_swapped = FALSE;

	if  (rec == NULL)  {
		/* initialize static variables */
		if  (fd > 0)  close( fd );
		fd = 0;
		f_start.year = 0;
		recnum = 1;
		l_end.year = 0;
		return;
	} /*endif*/

	if  (F_FIXCHECK & flags)  TdFixChecksum( rec, flags );

	/* check for 1sec sample rate on GRAX */
	if  (F_GRAX2GRA1 & flags)  {
		if  (strncmp(shdr->statcode,"GRAX",4) == 0)  {
			shdr->smprate_fact = 1200;
			shdr->smprate_mult = -60;
		} /*endif*/
	} /*endif*/

	SeedRecordTime( rec, &r_start, &r_end, &locstat );
	if  (SySevere(&locstat) || r_start.year < 1970 || r_start.year > 2050)  {
		locstat = cBcNoError;
		SeedSwapHeader( (SeedDataHeaderT *)rec );
		SeedRecordTime( rec, &r_start, &r_end, &locstat );
		hdr_swapped = TRUE;
		if  (SySevere(&locstat))  {
			fprintf( stderr, "%s: couldn't read record times\n", tdv_progname );
			return;
		} /*endif*/
	} /*endif*/
	shdr = (SeedDataHeaderT *)rec;
	dt = SeedGetSampleDist( shdr );
	if  (tdv_max_recsamp > 0)  {
		if  (shdr->no_of_samples > tdv_max_recsamp)  {
			fprintf( stderr, "%s: record with illegal number of samples ignored\n",
				tdv_progname );
			return;
		} /*endif*/
	} /*endif*/

	/* check for IBB and change it to IBBN */
	if  (strncmp(shdr->statcode,"IBB",3) == 0 && shdr->statcode[3] <= ' ')  {
		shdr->statcode[3] = 'N';
		shdr->statcode[4] = '\0';
	} /*endif*/
	/* check for GRFO-LH-{1,2,3} and change it to GRA1-LH-{Z,N,E} */
	if  (shdr->channel[2] >= '1' && shdr->channel[2] <= '3'
		&& (strncmp(shdr->channel,"LH",2) == 0)
		&& (strncmp(shdr->statcode,"GRFO",4) == 0))  {
		strncpy( shdr->statcode, "GRA1", 4 );
		if  (shdr->channel[2] == '1')  shdr->channel[2] = 'Z';
		if  (shdr->channel[2] == '2')  shdr->channel[2] = 'N';
		if  (shdr->channel[2] == '3')  shdr->channel[2] = 'E';
	} /*endif*/
	/* check for unprintable chars in station name */
	if  (F_FIXSTATCODE & flags)  {
		if  (shdr->statcode[3] < ' ' || shdr->statcode[3] > 'Z')
			shdr->statcode[3] = '\0';
		if  (shdr->statcode[4] < ' ' || shdr->statcode[4] > 'Z')
			shdr->statcode[4] = '\0';
	} /*endif*/
	/* check for GRAX */
	if  (F_GRAX2GRA1 & flags)  {
		if  (strncmp(shdr->statcode,"GRAX",4) == 0)  {
			shdr->statcode[3] = '1';
			shdr->statcode[4] = '\0';
		} /*endif*/
	} /*endif*/
	if  (F_YQFIXNAMES & flags)  {
		TdFixYqnames( shdr->statcode );
	} /*endif*/
	if  (F_TBFIXNAMES & flags)  {
		TdFixTbnames( shdr->statcode );
	} /*endif*/
	if  (F_NEWSTAT & flags)  {
		strncpy( shdr->statcode, tdv_newstation, 4 );
		shdr->statcode[4] = '\0';
	} /*endif*/
	if  (F_NEWCHAN & flags)  {
		strncpy( shdr->channel, tdv_newchan, 3 );
	} /*endif*/
	if  (F_SETNETWORK & flags)  {
		shdr->network[0] = tdv_newnetwork[0];
		shdr->network[1] = tdv_newnetwork[1];
	} /*endif*/
	/* set time correction if requested */
	if  (tdv_timecorr != 0)  {
		if  (F_RELTIMECORR & flags)  {
			shdr->timecorr += tdv_timecorr;
		} else {
			shdr->timecorr = tdv_timecorr;
		} /*endif*/
	} /*endif*/

	/* getstation and channel code */
	strncpy( station, shdr->statcode, 5 );
	station[5] = '\0';
	for  (i=0; i<5; i++)  if  (station[i] == ' ')  station[i] = '\0';
	strncpy( chan, shdr->channel, 3 );
	chan[3] = '\0';
	for  (i=0; i<3; i++)  if  (chan[i] == ' ')  chan[i] = '\0';
	ut_uncap( station );
	ut_uncap( chan );

	/* insert blockette 1000 if requested */
	if  (F_SETBLOCK1000 & flags)  {
		b1000found = TRUE;
		if  (shdr->no_of_blockettes == 0)  b1000found = FALSE;
		if  (shdr->first == 0)  b1000found = FALSE;
		if  (b1000found)  {
			uw = (UWORD *)((char *)shdr + (int)(shdr->first));
			if  (*uw != 1000)  b1000found = FALSE;
		} /*endif*/
		if  (!b1000found)  {
			reclth = SeedGetReclth( shdr, TRUE );
			i = 1;
			rlexp = 0;
			while  (i < reclth)  {
				i *= 2;
				rlexp++;
			} /*endif*/
			shdr->no_of_blockettes = 1;
			shdr->first = 48;
			b1000 = (SeedBlockette1000T *)((char *)shdr + (unsigned)(shdr->first));
			b1000->type = 1000;
			b1000->nextblock = 0;
			b1000->format = (UBYTE)Seed_C_DcmpSteim1;
			b1000->wordorder = (UBYTE)tdv_endian;
			b1000->reclthexp = (UBYTE)rlexp;
			b1000->reserved = (UBYTE)0;
		} /*endif*/
	} /*endif*/

	/* find out whether to open file at new day or new month */
	switch_days = TRUE;
	switch_hours = (F_SWITCHHOUR & flags);
	if  ((strncmp(chan,"vh",2) == 0 || strncmp(chan,"vl",2) == 0
		|| strncmp(chan,"vd",2) == 0 || strncmp(chan,"lh",2) == 0
		|| strncmp(chan,"uh",2) == 0 || strncmp(chan,"mp",2) == 0
		|| strncmp(chan,"va",2) == 0) && !(F_SWITCHDAY & flags))
		switch_days = FALSE;
	if  (F_ONEFILE & flags)
		switch_days = switch_hours = FALSE;

	/* find out whether to open new file */
	/* new file if file is not yet opened */
	new_file = (f_start.year == 0 || fd <= 0);
	/* new file if new year starts */
	if  (!new_file && !(F_ONEFILE & flags))
		new_file = (f_start.year != r_start.year );
	/* new file if new month starts */
	if  (!new_file && !(F_ONEFILE & flags))
		new_file = (f_start.month != r_start.month );
	/* new file if new day starts and day files should be created */
	if  (!new_file && !(F_ONEFILE & flags))
		new_file = (switch_days && f_start.day != r_start.day );
	/* new file if new hour starts and hour files are requested */
	if  (!new_file && !(F_ONEFILE & flags))
		new_file = (switch_hours && f_start.hour != r_start.hour );
	/* check for continuous time */
	if  (l_end.year > 0)  {
		/* new file if backward time step is detected */
		gaplth = tc_ndiff( &r_start, &l_end, &locstat );
		if  (gaplth < -dt/2.0)  {
			locstat = BC_NOERROR;
			tc_n2t( &l_end, timestr, &locstat );
			if  (F_NONEGGAPS & flags)  {
				fprintf( stderr,
					"%s: %s-%s time step of %5.2f sec backward at %s (deleted)\n",
					tdv_progname, station, chan, gaplth, timestr );
				return;
			} else {
				fprintf( stderr,
					"%s: %s-%s time step of %5.2f sec backward at %s\n",
					tdv_progname, station, chan, gaplth, timestr );
				if  (gaplth < -dt*2.0)
					new_file = TRUE;
				} /*endif*/
		} /*endif*/
		locstat = BC_NOERROR;
		/* new file if big time gap is detected */
		if  (!new_file)
			new_file = (gaplth > tdv_maxgapfac*dt);
	} /*endif*/
	if  (F_SINGLERECS & flags)  new_file = TRUE;

	/* open new file if necessary */
	if  (new_file)  {
		if  (fd > 0)  close( fd );
		fd = 0;
		/* add half a sample distance to the time to account for roundoff */
		locstat = cBcNoError;
		tc_nadd( &r_start, dt/2.0, &nametime, &locstat );
		year = nametime.year;
		if  (year > 1900)  year -= 1900;
		if  (year >= 100)  year -= 100;
		if  (F_SINGLERECS & flags)  {
			/*
			strcpy( fname, FILE_PREFIX );
			strcat( fname, station );
			strcat( fname, "." );
			strncat( fname, rec, 6 );
			strcat( fname, "." );
			strcat( fname, chan );
			*/
			sprintf( fname, "%s%s.%06d.%s", FILE_PREFIX, station, sngcnt++, chan );
		} else if  (F_LONGNAMES & flags)  {
			sprintf( fname, "%s%s_%02d%02d%02d_%02d%02d%02d.%s", FILE_PREFIX,
				station, year, nametime.month, nametime.day, nametime.hour,
				nametime.min, nametime.sec, chan );
		} else if  (F_OLDNAMES & flags)  {
			sprintf( fname, "%s%s%02d%02d%02d%02d%02d%02d.%s", FILE_PREFIX,
				station, year, nametime.month, nametime.day, nametime.hour,
				nametime.min, nametime.sec, chan );
		} else {
			sprintf( fname, "%s%s_%02d%02d%02d_%02d%02d.%s", FILE_PREFIX,
				station, year, nametime.month, nametime.day, nametime.hour,
				nametime.min, chan );
		} /*endif*/
		recnum = 1;
		fd = 1;
		fd = open( fname, O_CREAT|O_WRONLY, 0x1a4 );
		if  (fd <= 0)  {
			fprintf( stderr, "%s: error opening data file %s\n",
				tdv_progname, fname );
			return;
		} /*endif*/
		/* printf( "--> open file %s\n", fname ); */
		fprintf( tdv_created, "%s\n", fname );
		f_start = r_start;
	} /*endif*/

	if  (tdv_timeinc != 0.0 )
		TdIncrementTime( (SeedDataHeaderT *)rec, tdv_timeinc );
	if  (tdv_setyear != 0)  {
		shdr->starttime.year = (UWORD)tdv_setyear;
	} /*endif*/

	tmp = rec[6];
	sprintf( (char *)rec, "%06d", recnum++ );
	rec[6] = tmp;
	reclth = SeedGetReclth( (SeedDataHeaderT *)rec, TRUE ); /* clean info */
	shdr->Reserved_bytes_A = ' ';  /* set space in the reserved byte */
	if  (hdr_swapped)  SeedSwapHeader( (SeedDataHeaderT *)rec );
	write_ret = write( fd, (char *)rec, reclth );
	if  (write_ret != reclth)  {
		fprintf( stderr, "%s: write error on data file, abort\n", tdv_progname );
		exit( 1 );
		return;
	} /*endif*/
	l_end = r_end;

} /* end of TdPutRecord */



/*---------------------------------------------------------------------------*/



static void TdDeleteList( char fname[] )

/* deletes all files listed in fname
 *
 * parameters of routine
 * char       fname[];       input; name of list file
 */
{
	/* local variables */
	FILE     *fp;                 /* pointer to input file */
	char     cfile[BC_FILELTH+1]; /* current file */
	int      slen;                /* string length */

	/* executable code */

	/* open input file */
	fp = sy_fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: couldn' open delete list %s\n",
			tdv_progname, fname );
		return;
	} /*endif*/

	while  (fgets(cfile,BC_FILELTH,fp) != NULL)  {
		if  (*cfile == '!' || *cfile == '\n')  continue;
		slen = strlen( cfile ) - 1;
		if  (cfile[slen] == '\n')  cfile[slen] = '\0';
		sy_fdelete( cfile );
		/* printf( "--> deleting %s\n", cfile ); */
	} /*endwhile*/

	sy_fclose( fp );
	sy_fdelete( fname );

} /* end of TdDeleteList */



/*---------------------------------------------------------------------------*/



static void TdRenameList( char fname[] )

/* renames all files listed in fname to omit prefix
 *
 * parameters of routine
 * char       fname[];       input; name of list file
 */
{
	/* local variables */
	FILE     *fp;                 /* pointer to input file */
	char     cfile[BC_FILELTH+1]; /* current file */
	int      slen;                /* string length */
	int      prefixlth;           /* length of prefix */

	/* executable code */

	prefixlth = strlen( FILE_PREFIX );

	/* open input file */
	fp = sy_fopen( fname, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "%s: couldn' open rename list %s\n",
			tdv_progname, fname );
		return;
	} /*endif*/

	while  (fgets(cfile,BC_FILELTH,fp) != NULL)  {
		if  (*cfile == '!' || *cfile == '\n')  continue;
		slen = strlen( cfile ) - 1;
		if  (cfile[slen] == '\n')  cfile[slen] = '\0';
		if  (slen < prefixlth)  continue;
		sy_frename( cfile, cfile+prefixlth );
		/* printf( "--> renaming %s to %s\n", cfile, cfile+prefixlth ); */
	} /*endwhile*/

	sy_fclose( fp );
	sy_fdelete( fname );

} /* end of TdDeleteList */



/*---------------------------------------------------------------------------*/



static void TdFixChecksum( SeedSbyteT rec[], int flags )

/* Find checksum value (xn) and write it to the first frame
 *
 * parameters of routine
 * SeedSbyteT    rec[];      modify; SEED record to fix
 * int           flags;      input; some flags
 */
{
	/* local variables */
	static INT32 smp[MAX_REC_SMP];   /* decoded samples */
	INT32    smplth;                /* number of decoded samples */
	INT32    last_smp;              /* last sample (xn) */
	unsigned INT32    old_xn;       /* old checksum */

	/* executable code */

	/* check for zero in old xn */
	old_xn = SeedGet4Bytes( rec+Seed_C_FRAMELTH+8, (F_SWAP & flags) );
	if  (old_xn != 0)  fprintf( stderr, "%s: fixcheck: xn is %d\n",
		pa_progname(), old_xn );

	SeedPrintWarnings( FALSE );
	SeedDecodeSteim1( rec, (F_SWAP & flags), MAX_REC_SMP, smp, &smplth );
	SeedPrintWarnings( TRUE );
	if  (smplth <= 0)  return;
	last_smp = smp[smplth-1];
	TdPut4Bytes( last_smp, (F_SWAP & flags), rec+Seed_C_FRAMELTH+8 );

} /* end of TdFixChecksum */



/*----------------------------------------------------------------------------*/



static void TdPut4Bytes( unsigned INT32 l, BOOLEAN swap, SeedSbyteT b[] )

/* puts longword to byte stream
 *
 * parameters of routine
 * unsigned INT32 l;    input; longword to write
 * BOOLEAN    swap;    input; swap bytes
 * SeedSbyteT b[];     output; byte stream
 */
{
	/* local variables */

	/* executable code */

	if  (swap)  {
		b[0] = (SeedSbyteT)((l >> 24) & 0xffL);
		b[1] = (SeedSbyteT)((l >> 16) & 0xffL);
		b[2] = (SeedSbyteT)((l >> 8) & 0xffL);
		b[3] = (SeedSbyteT)(l & 0xffL);
	} else {
		b[0] = (SeedSbyteT)(l & 0xffL);
		b[1] = (SeedSbyteT)((l >> 8) & 0xffL);
		b[2] = (SeedSbyteT)((l >> 16) & 0xffL);
		b[3] = (SeedSbyteT)((l >> 24) & 0xffL);
	} /*endif*/

} /* end of TdPut4Bytes */



/*---------------------------------------------------------------------------*/



static void TdIncrementTime( SeedDataHeaderT *hdr, float inc )

/* Increments time in data header by <inc> seconds.
 *
 * parameters of routine
 * SeedDataHeaderT *hdr;    modify;   header to be changed
 * float           inc;     input; increment time in sec
 */
{
	/* local variables */
	NTIME    ntime;      /* numeric time */
	TSyStatus locstat;   /* return status */

	/* executable code */

	locstat = cBcNoError;
	SeedBtimeToNtime( &(hdr->starttime), &ntime, &locstat );
	if  (SySevere(&locstat))  return;
	tc_nadd( &ntime, inc, &ntime, &locstat );
	if  (SySevere(&locstat))  return;
	SeedNtimeToBtime( &ntime, &(hdr->starttime), &locstat );

} /* end of TdIncrementTime */



/*---------------------------------------------------------------------------*/



static void TdFixYqnames( char name[] )

/* Fixes station name bug in Scream for Yanqing stations
 *
 * parameters of routine
 * char       name[];      modify; station name to be fixed
 */
{
	/* local variables */

	/* executable code */

	if  (strncmp(name,"HTX",3) == 0)  {
		strcpy( name, "XHTT" );
	} else if  (strncmp(name,"JBT",3) == 0)  {
		strcpy( name, "YJBT" );
	} else if  (strncmp(name,"JCT",3) == 0)  {
		strcpy( name, "FJCT" );
	} else if  (strncmp(name,"NHT",3) == 0)  {
		strcpy( name, "DNHT" );
	} else if  (strncmp(name,"SHT",3) == 0)  {
		strcpy( name, "SSHT" );
	} else if  (strncmp(name,"SKT",3) == 0)  {
		strcpy( name, "HSKT" );
	} else if  (strncmp(name,"YFT",3) == 0)  {
		strcpy( name, "XYFT" );
	} else if  (strncmp(name,"YKT",3) == 0)  {
		strcpy( name, "FYKT" );
	} else if  (strncmp(name,"HTT",3) == 0)  {
		strcpy( name, "DHTT" );
	} /*endif*/

} /* end of TdFixYqnames */



/*---------------------------------------------------------------------------*/



static void TdFixTbnames( char name[] )

/* Fixes station name bug in Scream for Yanqing stations
 *
 * parameters of routine
 * char       name[];      modify; station name to be fixed
 */
{
	/* local variables */

	/* executable code */

	if  (strncmp(name,"TDA",3) == 0)  {
		strcpy( name, "MTDA" );
	} else if  (strncmp(name,"JBT",3) == 0)  {
		strcpy( name, "YJBT" );
	} /*endif*/

} /* end of TdFixTbnames */



/*---------------------------------------------------------------------------*/

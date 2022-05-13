
/* file seedbase.c
 *      ==========
 *
 * version 10, 15-Nov-2000
 *
 * SEED tape read routines
 * K. Stammler, 25-Dec-93
 */



#include <stdio.h>
#include <string.h>
#include BASECNST
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include BC_SYSBASE
#include BC_CPAR
#include BC_TCUSRDEF
#include BC_UTUSRDEF
#include BC_ERUSRDEF
#include "seedcfg.h"



/*
 * constants
 * ---------
 */

/* stream flags */
#define Seed_F_SELECTED 0x01
#define Seed_F_TMPFLAG  0x02

/* all times list */
#define Seed_C_NOLIST -1




/*
 * type definitions
 * ----------------
 */

typedef struct {
	SeedStreamT    *stream;     /* stream list */
	int            *twl;        /* numbers of time window lists */
	int            *flag;       /* stream flags */
	int            length;      /* length of stream list */
} SeedStreamListT;




/*
 * global variables
 * ----------------
 */

static SeedSetupParT    seedv_setup;    /* SEED setup parameters */
static SeedStreamListT  seedv_sl;       /* SEED stream list */
static SeedTwlT         *seedv_twl;     /* time window lists */
static int              seedv_no_of_twl;/* number of time window lists */




/*
 * prototypes of local routines
 * ----------------------------
 */

static void SeedSelectStreams( char str[], STATUS *status );
static void SeedAppendTimeList( char str[], SeedTwlT *twl,
	STATUS *status );
static BOOLEAN SeedTwlIntersect( SeedTimeT *start1, SeedTimeT *end1,
	SeedTimeT *start2, SeedTimeT *end2 );
static int SeedStreamIndex( SeedStreamT *str );



/*--------------------------------------------------------------------*/



void SeedReadSetupFile( char fname[], STATUS *status )

/* Reads setup file (file containing basic constants) of SEED
 * processing.  Parameters are read to seedv_setup structure.
 *
 * parameters of routine
 * char       fname[];        input; name of setup file
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	FILE     *fp;                 /* file pointer */
	char     line[BC_LINELTH+1];  /* current line */

	/* executable code */

	/* default values (overwritten by file) */
	seedv_setup.max_no_of_streams = 100;
	seedv_setup.max_no_of_twl = 20;
	seedv_setup.max_lth_of_twl = 20;
	seedv_setup.phys_rec_size = 512;
	seedv_setup.seed_rec_size = 4096;
	seedv_setup.seed_phys_recs = 8;
	seedv_setup.new_seed_names = FALSE;
	seedv_setup.dump_ill_recs = FALSE;
	seedv_setup.eom_eof_cnt = 2;
	seedv_setup.max_errcnt_illegal = 200;
	seedv_setup.max_errcnt_incomplete = 200;
	seedv_setup.max_errcnt_readerr = 200;
	seedv_setup.copy_buf_lth = 8192;

	fp = sy_fopen( fname, "r" );
	if  (fp == NULL)  {
		/*
		*status = SeedERR_OPENINPUT;
		err_setcontext( " ## file " );
		err_setcontext( fname );
		*/
		/* accept missing input file, take program defaults
       * these parameters are complete obsolete anyway, no more tapes in use
       */
		return;
	} /*endif*/

	strcpy( seedv_setup.setupfile, fname );

	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (*line == '!')  continue;
		if  (strncmp(line,"#max_no_of_streams ",19) == 0)  {
			sscanf( line+19, "%d", &seedv_setup.max_no_of_streams );
		} else if  (strncmp(line,"#max_no_of_twl ",15) == 0)  {
			sscanf( line+15, "%d", &seedv_setup.max_no_of_twl );
		} else if  (strncmp(line,"#max_lth_of_twl ",16) == 0)  {
			sscanf( line+16, "%d", &seedv_setup.max_lth_of_twl );
		} else if  (strncmp(line,"#phys_rec_size ",15) == 0)  {
			sscanf( line+15, "%d", &seedv_setup.phys_rec_size );
		} else if  (strncmp(line,"#seed_rec_size ",15) == 0)  {
			sscanf( line+15, "%d", &seedv_setup.seed_rec_size );
		} else if  (strncmp(line,"#new_seed_names ",16) == 0)  {
			sscanf( line+16, "%d", &seedv_setup.new_seed_names );
		} else if  (strncmp(line,"#dump_ill_recs ",15) == 0)  {
			sscanf( line+15, "%d", &seedv_setup.dump_ill_recs );
		} else if  (strncmp(line,"#eom_eof_cnt ",13) == 0)  {
			sscanf( line+13, "%d", &seedv_setup.eom_eof_cnt );
		} else if  (strncmp(line,"#max_errcnt_illegal ",20) == 0)  {
			sscanf( line+20, "%d", &seedv_setup.max_errcnt_illegal );
		} else if  (strncmp(line,"#max_errcnt_incomplete ",23) == 0)  {
			sscanf( line+23, "%d", &seedv_setup.max_errcnt_incomplete );
		} else if  (strncmp(line,"#max_errcnt_readerr ",20) == 0)  {
			sscanf( line+20, "%d", &seedv_setup.max_errcnt_readerr );
		} else if  (strncmp(line,"#copy_buf_lth ",14) == 0)  {
			sscanf( line+14, "%d", &seedv_setup.copy_buf_lth );
		} else {
		} /*endif*/
	} /*endwhile*/

	fclose( fp );

	if  (seedv_setup.seed_rec_size % seedv_setup.phys_rec_size != 0)  {
		fprintf( stderr,
			"*** SEED record size is not a multiple of physical record size\n" );
		*status = SeedERR_ILLRECSIZE;
		return;
	} /*endif*/
	seedv_setup.seed_phys_recs = seedv_setup.seed_rec_size
		/ seedv_setup.phys_rec_size;	

} /* end of SeedReadSetupFile */



/*--------------------------------------------------------------------*/



void SeedChangeSetup( SeedSetupParT *par, long setflags, STATUS *status )

/* Changes setup values.  Only these values are copied whose setup-bit
 * is set in 'setflags'.
 *
 * parameters of routine
 * SeedSetupParT *par;          input; new values of setup parameters
 * long          setflags;      input; setup flags (Seed_F_SET_... - Bits)
 * STATUS        *status;       output; return status
 */
{
	/* executable code */

	if  (Seed_F_SET_MAX_NO_OF_STREAMS & setflags)
		seedv_setup.max_no_of_streams = par->max_no_of_streams;
	if  (Seed_F_SET_MAX_NO_OF_TWL & setflags)
		seedv_setup.max_no_of_twl = par->max_no_of_twl;
	if  (Seed_F_SET_MAX_LTH_OF_TWL & setflags)
		seedv_setup.max_lth_of_twl = par->max_lth_of_twl;
	if  (Seed_F_SET_PHYS_REC_SIZE & setflags)
		seedv_setup.phys_rec_size = par->phys_rec_size;
	if  (Seed_F_SET_SEED_REC_SIZE & setflags)
		seedv_setup.seed_rec_size = par->seed_rec_size;
	if  (Seed_F_SET_NEW_SEED_NAMES & setflags)
		seedv_setup.new_seed_names = par->new_seed_names;
	if  (Seed_F_SET_DUMP_ILL_RECS & setflags)
		seedv_setup.dump_ill_recs = par->dump_ill_recs;
	if  (Seed_F_SET_EOM_EOF_CNT & setflags)
		seedv_setup.eom_eof_cnt = par->eom_eof_cnt;
	if  (Seed_F_SET_MAX_ERRCNT_ILLEGAL & setflags)
		seedv_setup.max_errcnt_illegal = par->max_errcnt_illegal;
	if  (Seed_F_SET_MAX_ERRCNT_INCOMPLETE & setflags)
		seedv_setup.max_errcnt_incomplete = par->max_errcnt_incomplete;
	if  (Seed_F_SET_MAX_ERRCNT_READERR & setflags)
		seedv_setup.max_errcnt_readerr = par->max_errcnt_readerr;
	if  (Seed_F_SET_COPY_BUF_LTH & setflags)
		seedv_setup.copy_buf_lth = par->copy_buf_lth;

	if  (seedv_setup.seed_rec_size % seedv_setup.phys_rec_size != 0)  {
		fprintf( stderr, 
			"*** SEED record size is not a multiple of physical record size\n" );
		*status = SeedERR_ILLRECSIZE;
		return;
	} /*endif*/
	seedv_setup.seed_phys_recs = seedv_setup.seed_rec_size
		/ seedv_setup.phys_rec_size;	

} /* end of SeedChangeSetup */



/*--------------------------------------------------------------------*/



void SeedSetup( SeedSetupParT *setup, STATUS *status )

/* Gets setup file.
 *
 * parameters of routine
 * int        argc;          input; number of command line arguments
 * char       *argv[];       input; command line arguments
 * SeedSetupParT *setup;     output; SEED setup
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	char     *def_dir;                   /* default input directory */
	char     setupfile[BC_FILELTH+1];    /* SEED setup file */
	long     setup_change_flags;         /* setup change flags */
	SeedSetupParT setup_change;          /* setup change */
	int      i;                          /* counter */

	/* executable code */

	def_dir = getenv( "SEED_INPUTS" );
	if  (def_dir == NULL)  {
		strcpy( setupfile, "seed_setup.txt" );
	} else {
		strcpy( setupfile, def_dir );
		i = strlen( def_dir ) - 1;
		if  (def_dir[i] != ':' && def_dir[i] != ']')
			strcat( setupfile, "/" );
		strcat( setupfile, "seed_setup.txt" );
	} /*endif*/
	if  (pa_qspecified("-setup"))
		strcpy( setupfile, pa_qvalue("-setup") );
	setup_change_flags = 0;
	if  (pa_qspecified("-physrec"))  {
		sscanf( pa_qvalue("-physrec"), "%d", &setup_change.phys_rec_size );
		setup_change_flags |= Seed_F_SET_PHYS_REC_SIZE;
	} /*endif*/
	if  (pa_qspecified("-seedrec"))  {
		sscanf( pa_qvalue("-seedrec"), "%d", &setup_change.seed_rec_size );
		setup_change_flags |= Seed_F_SET_SEED_REC_SIZE;
	} /*endif*/
	if  (pa_qspecified("-newnames"))  {
		setup_change.new_seed_names = TRUE;
		setup_change_flags |= Seed_F_SET_NEW_SEED_NAMES;
	} /*endif*/
	if  (pa_qspecified("-oldnames"))  {
		setup_change.new_seed_names = FALSE;
		setup_change_flags |= Seed_F_SET_NEW_SEED_NAMES;
	} /*endif*/
	if  (pa_qspecified("-dump"))  {
		setup_change.dump_ill_recs = TRUE;
		setup_change_flags |= Seed_F_SET_DUMP_ILL_RECS;
	} /*endif*/
	if  (pa_qspecified("-nodump"))  {
		setup_change.dump_ill_recs = FALSE;
		setup_change_flags |= Seed_F_SET_DUMP_ILL_RECS;
	} /*endif*/
	if  (pa_qspecified("-eom"))  {
		sscanf( pa_qvalue("-eom"), "%d", &setup_change.eom_eof_cnt );
		setup_change_flags |= Seed_F_SET_EOM_EOF_CNT;
	} /*endif*/
	if  (pa_qspecified("-err_ill"))  {
		sscanf( pa_qvalue("-err_ill"), "%d", &setup_change.max_errcnt_illegal );
		setup_change_flags |= Seed_F_SET_MAX_ERRCNT_ILLEGAL;
	} /*endif*/
	if  (pa_qspecified("-err_incomp"))  {
		sscanf( pa_qvalue("-err_incomp"), "%d",
		&setup_change.max_errcnt_incomplete );
		setup_change_flags |= Seed_F_SET_MAX_ERRCNT_INCOMPLETE;
	} /*endif*/
	if  (pa_qspecified("-err_read"))  {
		sscanf( pa_qvalue("-err_read"), "%d", &setup_change.max_errcnt_readerr );
		setup_change_flags |= Seed_F_SET_MAX_ERRCNT_READERR;
	} /*endif*/

	SeedReadSetupFile( setupfile, status );
	if  (Severe(status))  err_writemsg( *status, "", TRUE );
	if  (setup_change_flags != 0)
		SeedChangeSetup( &setup_change, setup_change_flags, status );
	if  (Severe(status))  err_writemsg( *status, "", TRUE );
	SeedInitialize( status );
	if  (Severe(status))  err_writemsg( *status, "", TRUE );
	SeedGetSetup( setup );

} /* end of SeedSetup */



/*--------------------------------------------------------------------*/



void SeedSetupF( STATUS *status )

/* Fortran interface to SeedSetup.
 *
 * parameters of routine
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	static SeedSetupParT setup;   /* local setup */

	/* executable code */

	SeedSetup( &setup, status );

} /* endof SeedSetupF */



/*--------------------------------------------------------------------*/



void SeedInitialize( STATUS *status )

/* Allocates memory for stream lists and time window lists.
 * Routine SeedReadSetupFile must be called before.
 *
 * parameters of routine
 * STATUS     *status;      output; return status
 */
{
	/* local variables */
	SeedTimeT    *tp;                 /* pointer to time */
	int          i;                   /* counter */
	FILE         *fp;                 /* pointer to file */
	char         line[BC_LINELTH+1];  /* current line */
	char         str1[BC_LINELTH+1];  /* substring 1 */
	char         str2[BC_LINELTH+1];  /* substring 2 */
	char         str3[BC_LINELTH+1];  /* substring 3 */
	char         str4[BC_LINELTH+1];  /* substring 4 */

	/* executable code */

	if  (seedv_setup.max_no_of_streams == 0)  {
		*status = SeedERR_NOSETUP;
		return;
	} /*endif*/

	/* allocate stream lists */
	seedv_sl.stream = (SeedStreamT *)sy_allocmem(
		(long)seedv_setup.max_no_of_streams, (int)sizeof(SeedStreamT),
		status );
	if  (Severe(status))  return;
	seedv_sl.twl = (int *)sy_allocmem(
		(long)seedv_setup.max_no_of_streams, (int)sizeof(int), status );
	if  (Severe(status))  {
		sy_deallocmem( seedv_sl.stream );
		return;
	} /*endif*/
	seedv_sl.flag = (int *)sy_allocmem(
		(long)seedv_setup.max_no_of_streams, (int)sizeof( int ), status );
	if  (Severe(status))  {
		sy_deallocmem( seedv_sl.stream );
		sy_deallocmem( seedv_sl.twl );
		return;
	} /*endif*/
	for  (i=0; i<seedv_setup.max_no_of_streams; i++)  {
		seedv_sl.stream[i].station[0] = '\0';
		seedv_sl.stream[i].channel[0] = '\0';
		seedv_sl.stream[i].array[0] = '\0';
		seedv_sl.stream[i].comp = '\0';
		seedv_sl.twl[i] = Seed_C_NOLIST;
		seedv_sl.flag[i] = 0;
	} /*endfor*/
	seedv_sl.length = 0;

	/* allocate time lists */
	seedv_twl = (SeedTwlT *)sy_allocmem( (long)seedv_setup.max_no_of_twl,
		(int)sizeof(SeedTwlT), status );
	if  (Severe(status))  {
		sy_deallocmem( seedv_sl.stream );
		sy_deallocmem( seedv_sl.twl );
		sy_deallocmem( seedv_sl.flag );
		return;
	} /*endif*/
	tp = (SeedTimeT *)sy_allocmem( (long)seedv_setup.max_lth_of_twl
		* (long)seedv_setup.max_no_of_twl * 2L, (int)sizeof(SeedTimeT),
		status );
	if  (Severe(status))  {
		sy_deallocmem( seedv_sl.stream );
		sy_deallocmem( seedv_sl.twl );
		sy_deallocmem( seedv_twl );
		sy_deallocmem( seedv_sl.flag );
		seedv_twl = NULL;
		return;
	} /*endif*/

	for  (i=0; i<seedv_setup.max_no_of_twl; i++)  {
		seedv_twl[i].tw_begin = tp + 2*i*seedv_setup.max_lth_of_twl;
		seedv_twl[i].tw_end = tp + (2*i+1)*seedv_setup.max_lth_of_twl;
		seedv_twl[i].twl_length = 0;
	} /*endfor*/
	seedv_no_of_twl = 0;

	fp = sy_fopen( seedv_setup.setupfile, "r" );
	if  (fp == NULL)  {
		/*
		fprintf( stderr, "*** SeedInitialize: setup file not found (2)\n" );
		*status = SeedERR_OPENINPUT;
		err_setcontext( " ## file " );
		err_setcontext( seedv_setup.setupfile );
		*/
		return;
		/* accept missing setup file, all parameters obsolete, no more tapes */
	} /*endif*/

	/* read all possible streams */
	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (*line == '!')  continue;
		if  (strncmp(line,"#stream_list_begin",18) != 0)  continue;
		seedv_sl.length = 0;
		FOREVER {
			if  (fgets(line,BC_LINELTH,fp) == NULL)  {
				*status = SeedERR_EOF_FOUND;
				fclose( fp );
				return;
			} /*endif*/
			if  (strncmp(line,"#stream_list_end",16) == 0)
				break;
			if  (*line == '!')
				continue;
			SeedPrepareStreamString( line );
			if  (sscanf(line,"%s %s %s %s",str1,str2,str3,str4) != 4)  {
				fprintf( stderr, "*** illegal stream line %s\n", line );
				*status = SeedERR_ILLSTREAM;
				fclose( fp );
				return;
			} /*endif*/
			if  (strlen(str1) > Seed_C_SHORTSTRLTH ||
				strlen(str2) > Seed_C_SHORTSTRLTH ||
				strlen(str3) > 1 || strlen(str4) > Seed_C_SHORTSTRLTH)  {
				fprintf( stderr, "*** illegal stream line %s\n", line );
				*status = SeedERR_ILLSTREAM;
				fclose( fp );
				return;
			} /*endif*/
			if  (seedv_sl.length == seedv_setup.max_no_of_streams)  {
				*status = SeedERR_STREAMOVFL;
				fprintf( stderr, "*** SeedInitialize: only %d streams accepted\n",
					seedv_setup.max_no_of_streams );
				fclose( fp );
				return;
			} /*endif*/
			strcpy( seedv_sl.stream[seedv_sl.length].station, str1 );
			strcpy( seedv_sl.stream[seedv_sl.length].channel, str2 );
			seedv_sl.stream[seedv_sl.length].comp = *str3;
			strcpy( seedv_sl.stream[seedv_sl.length].array, str4 );
			seedv_sl.length++;
		} /*endfor*/
	} /*endwhile*/

	fclose( fp );

} /* end of SeedInitialize */



/*--------------------------------------------------------------------*/



void SeedFinish( void )

/* Frees memory of time lists and stream lists
 *
 * no parameters
 */
{
	/* local variables */

	/* executable code */

	/* free stream list */
	sy_deallocmem( seedv_sl.stream );
	sy_deallocmem( seedv_sl.twl );

	/* free time lists */
	sy_deallocmem( seedv_twl[0].tw_begin );
	sy_deallocmem( seedv_twl );
	seedv_twl = NULL;

} /* end of SeedFinish */



/*--------------------------------------------------------------------*/



void SeedReadSelections( char fname[], STATUS *status )

/* Reads selections of streams and time windows from
 * given file.
 *
 * parameters of routine
 * char       fname[];        input; name of setup file
 */
{
	/* local variables */
	FILE     *fp;                 /* file pointer */
	char     line[BC_LINELTH+1];  /* current line */
	BOOLEAN  first;               /* first loop */
	BOOLEAN  all_times;           /* all times selected */
	int      i, j, k;             /* counters */

	/* executable code */

	fp = sy_fopen( fname, "r" );
	if  (fp == NULL)  {
		*status = SeedERR_OPENINPUT;
		err_setcontext( " ## file " );
		err_setcontext( fname );
		return;
	} /*endif*/

	while  (fgets(line,BC_LINELTH,fp) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		if  (strncmp(line,"streams:",8) == 0)  {
			FOREVER {
				if  (fgets(line,BC_LINELTH,fp) == NULL)  {
					*status = SeedERR_EOF_FOUND;
					fprintf( stderr, "*** error in selection file %s\n", fname );
					fclose( fp );
					return;
				} /*endif*/
				if  (*line == '!')  continue;
				if  (*line == '\n')  break;
				if  (strncmp(line,"time window list:",17) == 0)  break;
				SeedPrepareStreamString( line );
				SeedSelectStreams( line, status );
				if  (Severe(status))  {fclose( fp );return;}
			} /*endfor*/
		} /*endif*/
		if  (strncmp(line,"time window list:",17) == 0)  {
			first = TRUE;
			all_times = FALSE;
			FOREVER {
				if  (fgets(line,BC_LINELTH,fp) == NULL)  {
					*status = SeedERR_EOF_FOUND;
					fprintf( stderr, "*** error in selection file %s\n", fname );
					fclose( fp );
					return;
				} /*endif*/
				if  (*line == '!')  continue;
				if  (*line == '\n' || strncmp(line,"end:",4) == 0)  {
					if  (first)  {
						*status = SeedERR_NOTIMELIST;
						fprintf( stderr,
							"*** SeedReadSelections: no time list specified\n" );
					} /*endif*/
					for  (i=0; i<seedv_sl.length; i++)  {
						if  (seedv_sl.flag[i] & Seed_F_TMPFLAG)  {
							seedv_sl.flag[i] &= ~Seed_F_TMPFLAG;
							seedv_sl.flag[i] |= Seed_F_SELECTED;
							seedv_sl.twl[i] = (all_times)
								? Seed_C_NOLIST : seedv_no_of_twl;
						} /*endif*/
					} /*endif*/
					if  (!all_times)  seedv_no_of_twl++;
					break;
				} /*endif*/
				if  (strncmp(line,"all",3) == 0 || *line == '*')  {
					if  (first)  {
						all_times = TRUE;
					} else {
						fprintf( stderr, "*** Warning: time list %s ignored\n", line);
					} /*endif*/
				} else {
					if  (all_times)  {
						fprintf( stderr, "*** Warning: time list %s ignored\n", line);
					} else {
						if  (first
							&& (seedv_no_of_twl == seedv_setup.max_no_of_twl))  {
							fprintf( stderr, "*** only %d time lists accepted\n",
								seedv_setup.max_no_of_twl );
							*status = SeedERR_TWLOVFL;
							fclose( fp );
							return;
						} /*endif*/
						SeedAppendTimeList( line,
							seedv_twl+seedv_no_of_twl, status );
						if  (Severe(status))  {fclose( fp );return;}
					} /*endif*/
				} /*endif*/
				if  (Severe(status))  {fclose( fp );return;}
				first = FALSE;
			} /*endfor*/
		} /*endif*/
	} /*endwhile*/

	fclose( fp );

	/* check intersections */
	for  (i=0; i<seedv_no_of_twl; i++)
		for  (j=1; j<seedv_twl[i].twl_length; j++)
			for  (k=0; k<j; k++)
				if  (SeedTwlIntersect( (seedv_twl[i].tw_begin)+j,
					(seedv_twl[i].tw_end)+j, (seedv_twl[i].tw_begin)+k,
					(seedv_twl[i].tw_end)+k ))
					fprintf( stderr,
						"*** Warning: in time list [%d] are intersections\n", i+1 );

} /* end of SeedReadSelections */



/*--------------------------------------------------------------------*/



void SeedPrepareStreamString( char str[] )

/* replaces the characters "-","/","_" by blanks in given string
 *
 * parameters of routine
 * char       str[];        modify; string to be modified
 */
{
	/* local variables */
	char         *ch;                 /* moving pointer */

	/* executable code */

	ut_uncap( str );
	for  (ch=str; *ch != '\0'; ch++)
		if  (*ch == '-' || *ch == '/' || *ch == '_')
			*ch = ' ';

} /* end of SeedPrepareStreamString */



/*--------------------------------------------------------------------*/



static void SeedSelectStreams( char str[], STATUS *status )

/* Selects streams using given string "str" specifying station, channel
 * and component.  Instead of station name also the array name,
 * "all" or "*" is accepted.  Also as channel names and components
 * "all" and "*" are accepted. "str" must specify station, channel
 * name on component separated by blanks.  Streams are selected
 * by setting bit Seed_F_TMPFLAG.
 *
 * parameters of routine
 * char       str[];      input; selection string
 * STATUS     *status;    output; return status
 */
{
	/* local variables */
	char     sel_station[BC_LINELTH+1];    /* selected station */
	char     sel_channel[BC_LINELTH+1];    /* selected channel name */
	char     s[BC_LINELTH+1];              /* scratch string */
	char     sel_comp;                     /* selected component */
	int      i;                            /* stream counter */

	/* executable code */

	if  (sscanf(str,"%s %s %s",sel_station,sel_channel,s) != 3)  {
		*status = SeedERR_ILLSTREAM;
		fprintf( stderr, "*** illegal selection line %s\n", str );
		return;
	} /*endif*/

	if  (strcmp(s,"all") == 0)  {
		sel_comp = ' ';
	} else if  (strlen(s) > 1)  {
		*status = SeedERR_ILLSTREAM;
		fprintf( stderr, "*** illegal selection line %s\n", str );
		return;
	} else {
		sel_comp = *s;
		if  (sel_comp == '*' || sel_comp == '?')  sel_comp = ' ';
	} /*endif*/

	if  (strcmp(sel_station,"all") == 0)
		*sel_station = '*';
	if  (strcmp(sel_channel,"all") == 0)
		*sel_channel = '*';

	for  (i=0; i<seedv_sl.length; i++)  {
		if  (*sel_station != '*'
			&& strcmp(sel_station,seedv_sl.stream[i].station) != 0
			&& strcmp(sel_station,seedv_sl.stream[i].array) != 0)
			continue;
		if  (*sel_channel != '*'
			&& strcmp(sel_channel,seedv_sl.stream[i].channel) != 0)
			continue;
		if  (sel_comp != ' ' && sel_comp != seedv_sl.stream[i].comp)
			continue;
		if  (seedv_sl.flag[i] & Seed_F_SELECTED)
			fprintf( stderr, "*** Warning: stream %s-%s-%c selected twice\n",
				seedv_sl.stream[i].station, seedv_sl.stream[i].channel,
				seedv_sl.stream[i].comp );
		seedv_sl.flag[i] |= Seed_F_TMPFLAG;
	} /*endfor*/

} /* end of SeedSelectStreams */



/*--------------------------------------------------------------------*/



static void SeedAppendTimeList( char str[], SeedTwlT *twl,
	STATUS *status )

/* Appends time window (given in str) to time window list "twl"
 *
 * parameters of routine
 * char       str[];         input; time window
 * SeedTwlT   *twl;          modify; time window list
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	char     str1[BC_LINELTH+1];      /* substring 1 */
	char     str2[BC_LINELTH+1];      /* substring 2 */

	/* executable code */

	if  (twl->twl_length == seedv_setup.max_lth_of_twl)  {
		fprintf( stderr,
			"*** SeedAppendTimeList: maximum length of time window list is %d\n",
			seedv_setup.max_lth_of_twl );
		*status = SeedERR_TWLLTH;
		return;
	} /*endif*/

	if  (sscanf(str,"%s %s",str1,str2) != 2)  {
		fprintf( stderr, "*** illegal time window %s\n", str );
		*status = SeedERR_ILLTWL;
		return;
	} /*endif*/

	tc_t2a( str1, (twl->tw_begin)+(twl->twl_length), status );
	if  (Severe(status))  return;
	tc_t2a( str2, (twl->tw_end)+(twl->twl_length), status );
	if  (Severe(status))  return;
	(twl->twl_length)++;

} /* end of SeedAppendTimeList */



/*--------------------------------------------------------------------*/



void SeedPrintSelections( void )

/* Prints selections read by SeedReadSelections to standard output
 *
 * no parameters
 */
{
	/* local variables */
	int      i, j;                 /* counters */
	STATUS   locstat;              /* local status */
	char     tstr1[BC_TIMELTH+1];  /* time string 1 */
	char     tstr2[BC_TIMELTH+1];  /* time string 2 */

	/* executable code */

	printf( "!\n!\n! stream and time selections read from file %s\n!\n",
		seedv_setup.setupfile );

	for  (i=0; i<seedv_sl.length; i++)
		if  (seedv_sl.flag[i] & Seed_F_SELECTED)  {
			printf( "! stream %s-%s-%c:\t", seedv_sl.stream[i].station,
				seedv_sl.stream[i].channel, seedv_sl.stream[i].comp );
			if  (seedv_sl.twl[i] == Seed_C_NOLIST)  {
				printf( "all times\n" );
			} else {
				printf( "time list [%d]\n", seedv_sl.twl[i]+1 );
			} /*endif*/
		} /*endif*/

	for  (i=0; i<seedv_no_of_twl; i++)  {
		printf( "!\n! time list [%d]\n", i+1 );
		for  (j=0; j<seedv_twl[i].twl_length; j++)  {
			locstat = BC_NOERROR;
			tc_a2t( (seedv_twl[i].tw_begin)+j, tstr1, &locstat );
			tc_a2t( (seedv_twl[i].tw_end)+j, tstr2, &locstat );
			if  (Severe(&locstat))
				fprintf( stderr, "*** error converting abs time [%d] wdw %d\n",i,j);
			printf( "!  start: %s     end: %s\n", tstr1, tstr2 );
		} /*endfor*/
	} /*endfor*/

} /* end of SeedPrintSelections */



/*--------------------------------------------------------------------*/



void SeedGetSetup( SeedSetupParT *setup )

/* Returns SEED setup parameters.  The values are copied to a passed
 * structure.  Changes to the parameters are not possible.
 *
 * parameters of routine
 * SeedSetupParT *setup;     output; setup parameters
 */
{
	/* executable code */

	*setup = seedv_setup;

} /* end of SeedGetSetup */



/*--------------------------------------------------------------------*/



static BOOLEAN SeedTwlIntersect( SeedTimeT *start1, SeedTimeT *end1,
	SeedTimeT *start2, SeedTimeT *end2 )

/* Checks intersections of two time windows
 *
 * parameters of routine
 * SeedTimeT  *start1, *end1;     input; first time window
 * SeedTimeT  *start2, *end2;     input; second time window
 *                                returns TRUE if windows intersect
 */
{
	/* executable code */

	if  (tc_adiff(start1,end2) >= 0.0)  return FALSE;
	if  (tc_adiff(start2,end1) >= 0.0)  return FALSE;
	return TRUE;

} /* end of SeedTwlIntersect */



/*--------------------------------------------------------------------*/



static int SeedStreamIndex( SeedStreamT *str )

/* Returns index number of specified stream, -1 if not found
 *
 * parameters of routine
 * SeedStreamT   *str;      input; stream to find
 */
{
	/* local variables */
	int      i;            /* counter */

	/* executable code */

	for  (i=0; i<seedv_sl.length; i++)  {
		if  (strcmp(str->station,seedv_sl.stream[i].station) != 0)
			continue;
		if  (strcmp(str->channel,seedv_sl.stream[i].channel) != 0)
			continue;
		if  (str->comp != seedv_sl.stream[i].comp)
			continue;
		return i;
	} /*endfor*/
	return Seed_C_ILLEGAL;

} /* end of SeedStreamIndex */



/*--------------------------------------------------------------------*/



int SeedIsSelected( SeedStreamT *str, char timestr[], STATUS *status )

/* Checks whether specified stream and time is in selection list.  Returns
 * Seed_C_IsSelected, Seed_C_TimeNotSelected or Seed_C_StreamNotSelected
 *
 * parameters of routine
 * SeedStreamT   *str;        input; stream to check
 * char          timestr[];   input; time to check
 * STATUS        *status;     output; return status
 */
{
	/* local variables */
	int        str_idx;           /* stream index */
	int        i;                 /* counter */
	SeedTimeT  stime;             /* absolute time (start) */
	SeedTwlT   *twl;              /* time window list of stream */

	/* executable code */

	tc_t2a( timestr, &stime, status );
	if  (Severe(status))  return Seed_C_TimeNotSelected;
	/* tc_aadd( &stime, Seed_C_SEL_TIMESPAN, &etime ); */

	str_idx = SeedStreamIndex( str );
	if  (str_idx < 0)  return Seed_C_StreamNotSelected;
	if  (!(seedv_sl.flag[str_idx] & Seed_F_SELECTED))
		return Seed_C_StreamNotSelected;
	if  (seedv_sl.twl[str_idx] == Seed_C_NOLIST)  return Seed_C_IsSelected;
	twl = seedv_twl + (seedv_sl.twl[str_idx]);

	for  (i=0; i<twl->twl_length; i++)
		if  (tc_adiff((twl->tw_end)+i,&stime) >= 0.0 &&
			tc_adiff(&stime,(twl->tw_begin)+i) >= 0.0)
			return Seed_C_IsSelected;

	return Seed_C_TimeNotSelected;

} /* end of SeedIsSelected */



/*--------------------------------------------------------------------*/



BOOLEAN SeedWindowIsSelected( SeedStreamT *str, char t_start[],
	char t_end[], STATUS *status )

/* Checks whether specified time window is selected on given stream.
 *
 * parameters of routine
 * SeedStreamT   *str;        input; stream to check
 * char          t_start[];   input; start of time window
 * char          t_end[];       input; time to check
 * STATUS        *status;     output; return status
 */
{
	/* local variables */
	int        str_idx;           /* stream index */
	int        i;                 /* counter */
	SeedTimeT  stime;             /* absolute time (start) */
	SeedTimeT  etime;             /* absolute time (end) */
	SeedTwlT   *twl;              /* time window list of stream */

	/* executable code */

	tc_t2a( t_start, &stime, status );
	if  (Severe(status))  return FALSE;
	tc_t2a( t_end, &etime, status );
	if  (Severe(status))  return FALSE;

	str_idx = SeedStreamIndex( str );
	if  (str_idx < 0)  return FALSE;
	if  (!(seedv_sl.flag[str_idx] & Seed_F_SELECTED))
		return FALSE;
	if  (seedv_sl.twl[str_idx] == Seed_C_NOLIST)  return TRUE;
	twl = seedv_twl + (seedv_sl.twl[str_idx]);

	for  (i=0; i<twl->twl_length; i++)  {
		if  (tc_adiff(&etime,(twl->tw_end)+i) >= 0.0 &&
			tc_adiff((twl->tw_end)+i,&stime) >= 0.0)
			return TRUE;
		if  (tc_adiff(&etime,(twl->tw_begin)+i) >= 0.0 &&
			tc_adiff((twl->tw_begin)+i,&stime) >= 0.0)
			return TRUE;
	} /*endfor*/

	return FALSE;

} /* end of SeedWindowIsSelected */



/*--------------------------------------------------------------------*/



int SeedNextStream( int mode )

/* Returns index next stream or next selected stream depending on 'mode'
 * Possible values of 'mode' are:
 *    Seed_C_NSM_RESET:           start new list, returns illegal value
 *    Seed_C_NSM_NEXT_STREAM:     next stream, whether or not selected
 *    Seed_C_NSM_NEXT_SELECTED:   next selected stream
 *    Seed_C_NSM_TOTAL_NUMBER:    total number of streams
 * If no more stream is available, Seed_C_ILLEGAL is returned
 *
 * parameters of routine
 * int        mode;            input; controls returned value
 *                             returns requested stream index
 */
{
	/* local variables */
	static int   curr_idx;     /* current index */

	/* executable code */

	switch  (mode)  {
	case Seed_C_NSM_RESET:
		curr_idx = -1;
		return Seed_C_ILLEGAL;
	case Seed_C_NSM_NEXT_STREAM:
		if  (++curr_idx >= seedv_sl.length)  {
			return Seed_C_ILLEGAL;
		} else {
			return curr_idx;
		} /*endif*/
	case Seed_C_NSM_NEXT_SELECTED:
		while  (++curr_idx < seedv_sl.length)
			if  (seedv_sl.flag[curr_idx] & Seed_F_SELECTED)
				return curr_idx;
		return Seed_C_ILLEGAL;
	case Seed_C_NSM_TOTAL_NUMBER:
		return seedv_sl.length;
	default:
		fprintf( stderr, "*** SeedNextStream: this is a program bug\n" );
		return Seed_C_ILLEGAL;
	} /*endswitch*/

} /* end of SeedNextStream */



/*--------------------------------------------------------------------*/



int SeedIdentifyStreamString( char str[] )

/* Returns stream index.  'str' is stream string like "bfo-vbb-z".
 *
 * parameters of routine
 * char       str[];           input; stream string
 *                             returns stream index or Seed_C_ILLEGAL
 */
{
	/* local variables */
	SeedStreamT  stream;              /* stream */
	char         lstr[BC_LINELTH+1];  /* local stream string */
	char         str1[BC_LINELTH+1];  /* substring 1 */
	char         str2[BC_LINELTH+1];  /* substring 2 */
	char         str3[BC_LINELTH+1];  /* substring 3 */

	/* executable code */

	if  (strlen(str) > BC_LINELTH)  {
		fprintf( stderr, "*** stream string too long: %s\n", str );
		return Seed_C_ILLEGAL;
	} /*endif*/
	strcpy( lstr, str );
	SeedPrepareStreamString( lstr );
	if  (sscanf(lstr,"%s %s %s",str1,str2,str3) != 3)  {
		fprintf( stderr, "*** illegal stream string: %s\n", str );
		return Seed_C_ILLEGAL;
	} /*endif*/
	if  (strlen(str1) > Seed_C_SHORTSTRLTH
		|| strlen(str2) > Seed_C_SHORTSTRLTH || strlen(str3) > 1)  {
		fprintf( stderr, "*** illegal stream string: %s\n", str );
		return Seed_C_ILLEGAL;
	} /*endif*/
	strcpy( stream.station, str1 );
	strcpy( stream.channel, str2 );
	stream.comp = *str3;
	return SeedStreamIndex( &stream );

} /* end of SeedIdentifyStreamString */



/*--------------------------------------------------------------------*/



void SeedGetStreamString( int stream_idx, char str[] )

/* Returns tream string from stream index.  'str' must be long
 * enough, i.e. 2*Seed_C_SHORTSTRLTH+3+1.
 *
 * parameters of routine
 * int        stream_idx;      input; stream index number
 * char       str[];           output; stream string
 */
{
	/* executable code */

	*str = '\0';
	if  (stream_idx < 0 || stream_idx >= seedv_sl.length)  return;
	sprintf( str, "%s-%s-%c", seedv_sl.stream[stream_idx].station,
		seedv_sl.stream[stream_idx].channel,
		seedv_sl.stream[stream_idx].comp );

} /* end of SeedGetStreamString */



/*--------------------------------------------------------------------*/

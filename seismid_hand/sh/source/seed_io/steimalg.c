/* file steimalg.c
 *      ==========
 *
 * version 6, 7-Feb-2007
 *
 * Steim-1 compression algorithm
 */


/*--------------------------------------------------------------------------*\
    Name:	encode_steim.c

    Purpose:	translate signed long integers to steim compressed format.

    Usage:
		INT32 Steim_comp( INT32 XHUGE * p_dbuf;
				 DATA_HEADER * p_fsdh;
				 UINT32 Number_of_samples ;
				 WORD data_rec_length;
				 INT32 * p_seed_data_records;
             INT32 previous_sample;    by K.S.

		note: XHUGE is used for 80x86 based machines to force
		      normalization of pointers. It is #defined null
		      for use on real hardware.

    Output:	An array of data records containing steim compressed
		data.

    Externals:	char verbose - enables display of processing messages.

    Called by:	main.c, or your favourite program.

    Calls:	memcpy, printf

    Algorithm:	The compressor is implemented as a Deterministic
		Finite Automaton.  A second DFA takes over when
		the input tape (the raw data) ends.  The transition
		table for the DFA is -

	   note: _f signifies a final state.
	   ----------------------------------------------------------
			  | # of    |		     | # of  |
			  | bytes   |		     | DIFS  |
			  | DIF     |		     | to    | DIF
	   Current state  | fits in | New State      | unget | index
	   ---------------+---------+----------------+-------+-------
	   _START_STATE   |  1	    | _D1	     | 0     | 0
	   _START_STATE   |  2	    | _D2	     | 0     | 0
	   _START_STATE   |  4	    | _D4_f	     | 0     | 0
		    _D1   |  1	    | _D1_D1	     | 0     | 1
		    _D1   |  2	    | _D2_D2_f	     | 0     | 1
		    _D1   |  4	    | _D4_f	     | 1     | -1
		    _D2   |  1	    | _D2_D2_f	     | 0     | 1
		    _D2   |  2	    | _D2_D2_f	     | 0     | 1
		    _D2   |  4	    | _D4_f	     | 1     | -1
		 _D1_D1   |  1	    | _D1_D1_D1      | 0     | 2
		 _D1_D1   |  2	    | _D2_D2_f	     | 1     | -1
		 _D1_D1   |  4	    | _D2_D2_f	     | 1     | -1
	      _D1_D1_D1   |  1	    | _D1_D1_D1_D1_f | 0     | 3
	      _D1_D1_D1   |  2	    | _D2_D2_f	     | 2     | -1
	      _D1_D1_D1   |  4	    | _D2_D2_f	     | 2     | -1
	   ----------------------------------------------------------

    Problems:	None known.

    Language:	C, ANSI standard.

    Notes:	This program was developed under OS/2 on a harris 80x86
		machine.  It can be run in DOS or OS/2 large model
		with XHUGE (greater than 1 meg) data segments. To do
		this use -DXHUGE in the cl command line.

    Author:	Guy Stewart, Round Rock TX  (512) 244-9081
		IRIS	     Austin TX	    (512) 471-0405

    Revision:	04/26/1991  G. Stewart	 Initial preliminary release 0.9
		05/01/1991  G. Stewart	 First release of version 1.0

	Modification: 26-May-94  K. Stammler, new parameter 'previous_sample'
		in routine Steim_comp

	Revision: 20-Apr-95  K. Stammler, fixed bug in finishing not completely
      filled data records

   Revision: 7-May-97 K. Stammler, fixed bug in computing record start times
      on Dec, 31st in leap years

\*--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>    /* for memcpy */

#include "steim1.h"

char verbose=0x00 ;

#define TRUE 1
#define FALSE 0

#define _START_STATE	0
#define _D1		1
#define _D2		2
#define _D1_D1		3
#define _D1_D1_D1	4
#define _D4_f		5
#define _D2_D2_f	6
#define _D1_D1_D1_D1_f	7

int final[] = {
    0,
    0,
    0,
    0,
    0,
    1,
    1,
    1
    } ;

typedef struct _TRANSITION {
    int new_state ;
    int unget ;
    int dif_index ;
    } TRANSITION ;


TRANSITION transition[] = {
     _D1	    , 0,  0,
     _D2	    , 0,  0,
     _D4_f	    , 0,  0,
     _D1_D1	    , 0,  1,
     _D2_D2_f	    , 0,  1,
     _D4_f	    , 1,  -1,
     _D2_D2_f	    , 0,  1,
     _D2_D2_f	    , 0,  1,
     _D4_f	    , 1,  -1,
     _D1_D1_D1	    , 0,  2,
     _D2_D2_f	    , 1,  -1,
     _D2_D2_f	    , 1,  -1,
     _D1_D1_D1_D1_f , 0,  3,
     _D2_D2_f	    , 2,  -1,
     _D2_D2_f	    , 2,  -1
    } ;


/*-------------------------------------------------------------------------*\
   DISPLAY HEADER for debugging and profiling
\*-------------------------------------------------------------------------*/
static void display_header( DATA_HEADER *p_fsdh )
    {
    printf("-------------------------------------------\n");
    printf("- SequenceNumber[6]              %6.6s   \n", p_fsdh->SequenceNumber		   );
    printf("- Data_header_indicator          %c      \n", p_fsdh->Data_header_indicator 	   );
    printf("- Reserved_bytes_A               %c      \n", p_fsdh->Reserved_bytes_A		   );
    printf("- Station_identifier_code[5]     %5.5s   \n", p_fsdh->Station_identifier_code	   );
    printf("- Location_identifier[2]         %2.2s   \n", p_fsdh->Location_identifier		   );
    printf("- Channel_identifier[3]          %3.3s   \n", p_fsdh->Channel_identifier		   );
    printf("- Reserved_bytes_B[2]            %2.2s   \n", p_fsdh->Reserved_bytes_B		   );
    printf("- Record_start_time\n" ) ;
    printf("                     -     year: %hu\n", p_fsdh->Record_start_time.year );
    printf("  -                         day: %hu\n", p_fsdh->Record_start_time.day );
    printf("  -                       hours: %hu\n", (short) p_fsdh->Record_start_time.hours );
    printf("  -                     minutes: %hu\n", (short) p_fsdh->Record_start_time.minutes );
    printf("  -                     seconds: %hu\n", (short) p_fsdh->Record_start_time.seconds );
    printf("  -            1/10,000 seconds: %hu\n", p_fsdh->Record_start_time.frac_secs );
    printf("- Number_of_samples              %hu     \n", p_fsdh->Number_of_samples		   );
    printf("- Sample_rate_factor             %hd     \n", p_fsdh->Sample_rate_factor		   );
    printf("- Sample_rate_multiplier         %hd     \n", p_fsdh->Sample_rate_multiplier	   );
    printf("- Activity_flags                 %c      \n", p_fsdh->Activity_flags		   );
    printf("- IO_flags                       %c      \n", p_fsdh->IO_flags			   );
    printf("- Data_quality_flags             %c      \n", p_fsdh->Data_quality_flags		   );
    printf("- Number_of_blockettes_follow    %c      \n", p_fsdh->Number_of_blockettes_follow	   );
    printf("- Time_correction                %ld     \n", p_fsdh->Time_correction		   );
    printf("- Beginning_of_data              %hd     \n", p_fsdh->Beginning_of_data		   );
    printf("- First_blockette                %hd     \n", p_fsdh->First_blockette		   );
    }

/*-------------------------------------------------------------------------*\
   STATISTICS function for debugging profiling
\*-------------------------------------------------------------------------*/
static void statistics( UINT32 stat[4], UINT32 tossed, UINT32 Number_of_samples )
    {
    UINT32 total_bytes ;

    printf("-------------------------------------------\n");
    printf("# of 4:1 = %lu\n", stat[1]);
    printf("# of 2:1 = %lu\n", stat[2]);
    printf("# of 1:1 = %lu\n", stat[3]);

    total_bytes = (stat[1] + stat[2] + stat[3]) * 4 ;
    printf(" total bytes = %lu\n", total_bytes ) ;
    printf("total deltas = %lu\n", stat[1]*4 + stat[2]*2 + stat[3]) ;
    printf("* comp ratio = %f : 1.0\n", (double)((double)Number_of_samples)/((double)total_bytes/4) ) ;
    printf("Slots tossed = %lu\n", tossed ) ;
    printf(" Ideal ratio = %f : 1.0\n", (double)((double)Number_of_samples)/((double)(total_bytes-tossed/4)/4) ) ;
    printf("\n\n* ratio does not include space used by headers, codes or\n");
    printf(    "  integration constants\n");

    }

/*-------------------------------------------------------------------------*\
   EVAL_RATE Compute the Sample rate in SPS (Samples Per Seconds
\*-------------------------------------------------------------------------*/
static double eval_rate( WORD Sample_rate_factor, WORD Sample_rate_multiplier )
    {
    if ((Sample_rate_factor > 0) && (Sample_rate_multiplier > 0)) {
	return (double)Sample_rate_factor * (double)Sample_rate_multiplier ;
	}
    else if ((Sample_rate_factor > 0) && (Sample_rate_multiplier < 0)) {
	return (double)-Sample_rate_factor / (double)Sample_rate_multiplier ;
	}
    else if ((Sample_rate_factor < 0) && (Sample_rate_multiplier > 0)) {
	return (double)-Sample_rate_multiplier / (double)Sample_rate_factor ;
	}
    else if ((Sample_rate_factor < 0) && (Sample_rate_multiplier < 0)) {
	return (double)Sample_rate_multiplier / (double)Sample_rate_factor ;
	}
    }

/*-------------------------------------------------------------------------*\
    UPDATE FSDH TIME - compute ending time of data block using info
		       in the fsdh header.
\*-------------------------------------------------------------------------*/
static void update_fsdh_time( DATA_HEADER *p_fsdh, double sample_rate )
    {
    INT32 delta_secs, delta_frac_secs ;
    double delta_time ; /* change in time in 0.0001 seconds */
    int    daysperyear;    /* K.S.: number of day per year */

    /*-----------------------------------------------------------*\
	Setup delta time in seconds & fraction of seconds
    \*-----------------------------------------------------------*/

    delta_time = (double)((double)(p_fsdh->Number_of_samples)*(double)(10000) / (double)(sample_rate)) ;

    delta_secs = (UINT32) delta_time/10000 ;
    delta_frac_secs = ((double)(delta_time-delta_secs*10000)) ;

    /*-----------------------------------------------------------*\
	bubble delta_time through btime structure
	assigning times to Record start times.
    \*-----------------------------------------------------------*/
    delta_frac_secs += p_fsdh->Record_start_time.frac_secs ;
    p_fsdh->Record_start_time.frac_secs = delta_frac_secs % 10000 ;

    delta_secs += (delta_frac_secs - p_fsdh->Record_start_time.frac_secs)/10000 ;
    delta_secs += p_fsdh->Record_start_time.seconds ;
    p_fsdh->Record_start_time.seconds = delta_secs % 60 ;

    delta_secs = p_fsdh->Record_start_time.minutes + delta_secs / 60 ;
    p_fsdh->Record_start_time.minutes = delta_secs % 60 ;

    delta_secs = p_fsdh->Record_start_time.hours + delta_secs / 60 ;
    p_fsdh->Record_start_time.hours = delta_secs % 24 ;

    /* K.S.: compute number of days per year */
    daysperyear = p_fsdh->Record_start_time.year;
    if  (daysperyear % 4 == 0 &&  daysperyear != 2000)
       daysperyear = 366;
    else
       daysperyear = 365;

    delta_secs = p_fsdh->Record_start_time.day + delta_secs / 24 ;
    p_fsdh->Record_start_time.day = delta_secs % (daysperyear+1) ;

    /* K.S.: */
    if  ((delta_secs / (daysperyear+1)) > 0)
       p_fsdh->Record_start_time.year += delta_secs / (daysperyear+1);


    }


/*-------------------------------------------------------------------------*\
    _DATA_STATE - structure used to store the state of the current
		  fixed section data header.
\*-------------------------------------------------------------------------*/
typedef struct _DATA_STATE {
    int get_new_x0 ; /* set TRUE if new x0 is needed		*/
    INT32 unused ;
    INT32  x0,	     /* forward integration constant (x sub 0)	*/
	  xN ;	     /* reverse integration constant (x sub n)	*/
    INT32 w0 ;	     /* storage for all cks for a frame 	*/
    UINT32 num_data_rec ;
    UINT32 data_rec_length ;
    UINT32 seed_frame ;
    UINT32 seed_index ;
    UINT32 record_offset ;
    int frames_per_record ;
    double sample_rate ;
    } DATA_STATE ;

/*-------------------------------------------------------------------------*\
    FINISH_RECORD - fill in the record fixed section data header.
\*-------------------------------------------------------------------------*/
static void finish_record( INT32 XHUGE *p_seed_data_records,
	DATA_HEADER *p_fsdh, DATA_STATE *ds )
{
    /* copy record header */
    if (verbose) {
	display_header(p_fsdh);
	}

    p_seed_data_records[ds->record_offset+17] = ds->x0 ;
    p_seed_data_records[ds->record_offset+18] = ds->xN ;

    ds->get_new_x0 = TRUE ;

    memcpy(p_seed_data_records+ds->record_offset, p_fsdh, 48);

    /*-----------------------------------------------------------*\
       Set up FSDH for next record ...
    \*-----------------------------------------------------------*/
    update_fsdh_time(p_fsdh, ds->sample_rate);
    p_fsdh->Number_of_samples = 0 ;

    /*-----------------------------------------------------------*\
       Set up _DATA_STATE for next record ...
    \*-----------------------------------------------------------*/
    ds->num_data_rec ++ ;
    ds->record_offset = ds->num_data_rec * ds->data_rec_length/4 ;

    ds->seed_frame = 1 ;   /* start first frame of next record */
    ds->seed_index = 3 ;   /* leave room for x0, xN */
    ds->w0 = 0 ;
}

/*-------------------------------------------------------------------------*\
    ADD_WORD - append a compressed word to the data record.
\*-------------------------------------------------------------------------*/
static void Add_word( INT32 XHUGE *p_seed_data_records,
	DATA_HEADER *p_fsdh, INT32 wk, INT32 ck, DATA_STATE *ds )
    {

    ds->w0 |= ck << ( 2 * ( 15 - ds->seed_index ) ) ;

    p_seed_data_records[ds->record_offset+ds->seed_frame*16+ds->seed_index] = wk ;
    ds->seed_index++;

    if (ds->seed_index > 15) {

	/*-----------------------------------------------------------*\
	   Finish a frame ...
	\*-----------------------------------------------------------*/
	p_seed_data_records[ds->record_offset+ds->seed_frame*16] = ds->w0 ;

	/*-----------------------------------------------------------*\
	   Start next frame ...
	\*-----------------------------------------------------------*/
	ds->seed_index = 1 ;
	ds->seed_frame ++ ;
	ds->w0 = 0;
	if (ds->seed_frame >= ds->frames_per_record) {
	    finish_record( p_seed_data_records, p_fsdh, ds ) ;
	    }
	}
    }

/*--------------------------------------------------------------------------*\
     STEIM COMPRESSION -
\*--------------------------------------------------------------------------*/
INT32 Steim_comp( INT32 XHUGE *p_dbuf,
		DATA_HEADER *p_fsdh,
		UINT32 Number_of_samples,
		WORD data_rec_length,
		INT32 XHUGE *p_seed_data_records,
		INT32 XHUGE previous_sample)
    {
    INT32 d[4] ;
    UINT32 tossed = 0 ;
    UINT32 stat[4];
    INT32 ck ;
    INT32 wk ;
    int token ;
    DATA_HEADER new_fsdh ;   /* buffer for new fixed section data headers */
    DATA_STATE ds ;

    /*-----------------------------------------------------------*\
       Copy all the values from the passed fsdh, then set
       values affected by the steim compression.
    \*-----------------------------------------------------------*/
    memcpy( &new_fsdh, p_fsdh, 48 ) ;
    /* Now set static values in the new fsdh */
    new_fsdh.Data_header_indicator	 = 'D' ;
    new_fsdh.Reserved_bytes_A		 = ' ' ;
    /*memcpy(new_fsdh.Reserved_bytes_B, "  ", 2);*/ /* removed K.S. 7-Feb-2007 */
    new_fsdh.Number_of_samples		 = 0 ;	   /* Gets updated later */
    new_fsdh.Number_of_blockettes_follow = 0 ;
    new_fsdh.Beginning_of_data		 = 64 ;
    new_fsdh.First_blockette		 = 0 ;


    /*-----------------------------------------------------------*\
       Build the seed data record initial state
    \*-----------------------------------------------------------*/
    ds.sample_rate = eval_rate(p_fsdh->Sample_rate_factor,p_fsdh->Sample_rate_multiplier);
    ds.frames_per_record = data_rec_length / 64 ;
    ds.record_offset = 0 ;
    ds.seed_frame = 1 ;
    ds.seed_index = 3 ;
    ds.w0 = 0 ;
    ds.num_data_rec = 0 ;
    ds.data_rec_length = data_rec_length ;
    /*-----------------------------------------------------------*\
       Initialize x sub -1 for use in computing the forward
       and reverse intigration constants x sub 0 and x sub N.
    \*-----------------------------------------------------------*/
    ds.get_new_x0 = TRUE ;
    ds.xN = previous_sample ;  /* inserted K.S. 26-May-94 !!! */

    {
    int i ;
    for (i=0; i<4; stat[i++]=0);
    }

    if (verbose) {
	printf("starting up the compressor ..\n");
	display_header(p_fsdh);
	}


    {
    UINT32 dbuf_index ;
    UINT32 range = Number_of_samples ;
    INT32 dif ;

    int state = _START_STATE ;

    for (dbuf_index=0; dbuf_index<range; dbuf_index++ ) {

	dif = p_dbuf[dbuf_index] - ds.xN ;
	ds.xN = p_dbuf[dbuf_index] ;

	if (ds.get_new_x0) {
	    ds.x0 = p_dbuf[dbuf_index] ;
	    ds.get_new_x0 = FALSE ;
	    }


	/*-----------------------------------------------------------*\
	   Check for a one-byte dif - _D1_
	\*-----------------------------------------------------------*/
	if ((dif <= 127) && (dif >= -128)) {
	    char cx = (char)dif;
	    token = 0 ;
	    }

	/*-----------------------------------------------------------*\
	   Check for a two-byte dif - _D2_
	\*-----------------------------------------------------------*/
	else if ((dif <= 32767L) && (dif > -32768L)) {
	    short sx = (short)dif;
	    token = 1 ;
	    }

	/*-----------------------------------------------------------*\
	   Must be a four-byte dif - _D4_
	\*-----------------------------------------------------------*/
	else {
	    token = 2 ;
	    }

	/*-----------------------------------------------------------*\
	   Make the transition ...
	\*-----------------------------------------------------------*/
	{  /* (begin scope local variable tran_index) */
	int tran_index = state * 3 + token ;

	if (transition[tran_index].unget) {
	    dbuf_index -= transition[tran_index].unget ;
	    ds.xN = p_dbuf[dbuf_index] ;
	    }

	if (-1 < transition[tran_index].dif_index) {
	    d[transition[tran_index].dif_index] = dif ;
	    }
	/* The state assignment must be done last. */
	state = transition[tran_index].new_state ;
	}  /* (end scope local variables) */

	/*-----------------------------------------------------------*\
	   Got to a final state, put values into data section ...
	\*-----------------------------------------------------------*/
	if (final[state]) {
	    switch (state) {
		case _D4_f:
		    /* one 4-byte difference (one 32 bit sample) */
		    wk = d[0];
		    ck = 3 ;
		    new_fsdh.Number_of_samples += 1 ;
		    break;
		case _D2_D2_f:
		    /* two 2-byte differences (two 16 bit samples) */
		    wk = ((d[0]&0xFFFFL) << 16) | (d[1] & 0xFFFFL) ;
		    ck = 2 ;
		    new_fsdh.Number_of_samples += 2 ;
		    break;
		case _D1_D1_D1_D1_f:
		    /* four 1-byte differences (four 8 bit samples) */
		    wk = ((d[0]&0xFFL) << 24) | ((d[1]&0xFFL) << 16) | ((d[2]&0xFFL) << 8) | (d[3]&0xFFL) ;
		    ck = 1 ;
		    new_fsdh.Number_of_samples += 4 ;
		    break;
		}
	    stat[ck]++;
	    state = _START_STATE ;

	    Add_word( p_seed_data_records, &new_fsdh, wk, ck, &ds ) ;

	    }

	}

    /*-----------------------------------------------------------*\
       Ran out of input, decide what to do with
       the data already in the buffer ...
    \*-----------------------------------------------------------*/
    if (!final[state]) {
	switch (state) {
	    case _START_STATE:
		/* nothing !*/
		break;
	    case _D1:
		ck = 3;
		stat[ck]++;
		wk = d[0];
		new_fsdh.Number_of_samples += 1 ;
		break;
	    case _D2:
		ck = 3;
		stat[ck]++;
		wk = d[0];
		new_fsdh.Number_of_samples += 1 ;
		break;
	    case _D1_D1:
		ck = 2;
		stat[ck]++;
		wk = ((d[0]&0xFFFFL) << 16) | (d[1] & 0xFFFFL) ;
		new_fsdh.Number_of_samples += 2 ;
		break;
	    case _D1_D1_D1:
		ck = 2;   /* was: 3; changed 20-Apr-95, K. Stammler */
		stat[ck]++;
		wk = ((d[0]&0xFFFFL) << 16) | (d[1] & 0xFFFFL) ;
		new_fsdh.Number_of_samples += 2 ;
		Add_word( p_seed_data_records, &new_fsdh, wk, ck, &ds ) ;
		ck = 3;   /* was: 2; changed 20-Apr-95, K. Stammler */
		stat[ck]++;
		wk = d[2];
		new_fsdh.Number_of_samples += 1 ;
		break;
	    }
	 Add_word( p_seed_data_records, &new_fsdh, wk, ck, &ds ) ;
	}
    }

    if (new_fsdh.Number_of_samples) {
	/*-----------------------------------------------------------*\
	   Finish the frame ...
	\*-----------------------------------------------------------*/
	p_seed_data_records[ds.record_offset+ds.seed_frame*16] = ds.w0 ;
	/*-----------------------------------------------------------*\
	   Finish the record ...
	\*-----------------------------------------------------------*/
	finish_record( p_seed_data_records, &new_fsdh, &ds ) ;
	}

    /* display_results */
    if (verbose) {
	statistics( stat, tossed, Number_of_samples );
	}

    return ds.num_data_rec ;
    }

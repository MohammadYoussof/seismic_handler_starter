#define UBYTE unsigned char
#define WORD  short
#define UWORD unsigned short
#define INT32  int
#define UINT32 unsigned int

typedef struct _BTIME {
    UWORD year ;	     /* e.g. 1991 */
    UWORD day ; 	     /* 1..366 */
    UBYTE hours ;	     /* 0..23 */
    UBYTE minutes ;	     /* 0..59 */
    UBYTE seconds ;	     /* 0..59, 60 for leap */
    UBYTE alignment_1 ;
    UWORD frac_secs ;		/* 0.0001 seconds, 0..9999 */
    } BTIME ;

typedef struct _DATA_HEADER {
    char   SequenceNumber[6] ;
    char   Data_header_indicator ;
    char   Reserved_bytes_A ;
    char   Station_identifier_code[5] ;
    char   Location_identifier[2] ;
    char   Channel_identifier[3] ;
    char   Reserved_bytes_B[2] ;
    BTIME  Record_start_time ;
    UWORD  Number_of_samples ;
    WORD   Sample_rate_factor ;
    WORD   Sample_rate_multiplier ;
    UBYTE  Activity_flags ;
    UBYTE  IO_flags ;
    UBYTE  Data_quality_flags ;
    UBYTE  Number_of_blockettes_follow ;
    INT32  Time_correction ;
    UWORD  Beginning_of_data ;
    UWORD  First_blockette ;
    } DATA_HEADER ;


#ifdef IBM_PC
#define XHUGE huge
#define GM_ALLOC(NUM, SIZE) halloc(NUM, SIZE)
#define GM_FREE hfree
#else
#define XHUGE
#define GM_ALLOC(NUM, SIZE) malloc((NUM)*(SIZE))
#define GM_FREE free
#endif

#ifdef ANSI_EXTENSIONS
INT32 Steim_comp(INT32 XHUGE * p_dbuf,
		DATA_HEADER * p_fsdh,
		UINT32 Number_of_samples,
		WORD data_rec_length,
		INT32 XHUGE * p_seed_data_records,
		INT32 XHUGE previous_sample) ;
#endif

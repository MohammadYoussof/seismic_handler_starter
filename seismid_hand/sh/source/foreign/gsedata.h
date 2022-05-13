
/* file gsedata.h
 *      =========
 *
 * version 2, 30-Dec-96
 *
 * header file of module gsedata.c
 * K. Stammler, 8-Dec-94
 */



/* error codes */
#define eGseOffset          9200
#define eGseReadErr         (eGseOffset+1)  /* read error on input file */
#define eGseSmallAllocBlock (eGseOffset+2)  /* allocation block size too small*/
#define eGseOpenInput       (eGseOffset+3)  /* error opening file for input */
#define eGseNotOpen         (eGseOffset+4)  /* no GSE file opened */


/* general constants */

#define cGseMaxLineLth 1024


/* definition of WID2-structure */

#define cGseWid2IdLth 4
#define cGseWid2DateTimeLth 29
#define cGseWid2StationLth 5
#define cGseWid2ChannelLth 3
#define cGseWid2AuxIdLth 4
#define cGseWid2DataTypeLth 3
#define cGseWid2InsTypeLth 6

typedef struct {
	char        id[cGseWid2IdLth+1];               /* line ID */
	char        date_time[cGseWid2DateTimeLth+1];  /* date and time string */
	char        station[cGseWid2StationLth+1];     /* station code */
	char        channel[cGseWid2ChannelLth+1];     /* channel name */
	char        auxid[cGseWid2AuxIdLth+1];         /* auxiliary ID */
	char        datatype[cGseWid2DataTypeLth+1];   /* data type descr */
	long        samps;                             /* number of samples */
	float       samprat;                           /* samples per sec */
	float       calib;                             /* calibration (nm/count) */
	float       calper;                            /* calibration period */
	char        instype[cGseWid2InsTypeLth+1];     /* instrument type */
	float       hang;                              /* orientation of sensor */
	float       vang;                              /* orientation of sensor */
} SGseWid2;

#define cGseWid2StructNum 13

#define cGseWid1IdLth 4
#define cGseWid1StationLth 6
#define cGseWid1ChannelIdLth 8
#define cGseWid1ChannelLth 2
#define cGseWid1InsTypeLth 6
#define cGseWid1DataTypeLth 4

typedef struct {
	char        id[cGseWid1IdLth+1];               /* line ID */
	long        day_year;                          /* day and year */
	int         hour;                              /* start hour */
	int         min;                               /* start minute */
	int         sec;                               /* start second */
	int         ms;                                /* millisecond */
	int         samps;                             /* number of samples */
	char        station[cGseWid1StationLth+1];     /* station code */
	char        channel_id[cGseWid1ChannelIdLth+1];/* channel ID */
	char        channel[cGseWid1ChannelLth+1];     /* channel */
	float       samprat;                           /* sample rate */
	char        instype[cGseWid1InsTypeLth+1];     /* instrument type */
	char        datatype[cGseWid1DataTypeLth+1];   /* data type */
	char        diff_flag;                         /* differencing flag */
	float       calib;                             /* calibration gain */
	char        unit;                              /* unit of motion */
	float       calper;                            /* calibration period */
	float       statlat;                           /* station latitude */
	float       statlon;                           /* station longitude */
	float       statelev;                          /* station elevation */
	float       sensdepth;                         /* depth of sensor */
	float       beamaz;                            /* beam azimuth */
	float       beamslo;                           /* beam slowness */
	float       horient;                           /* horizontal orientation */
}SGseWid1;

#define cGseWid1Line1Num 14
#define cGseWid1Line2Num 10

/* GSE trace structure */

typedef struct {
	char        *mem;          /* pointer to start of memory */
	long        alloc;         /* number of allocated bytes */
	long        used;          /* number of used bytes */
} SGseRawData;

typedef struct {
	SGseWid1    wid1;           /* GSE 1 WID (should use union) */
	SGseWid2    wid;            /* waveform ID of trace */
	SGseRawData raw;            /* raw data */
	long        *smp;           /* pointer to samples */
	int         version;        /* GSE version (1 or 2) */
} SGseTrace;



/* prototypes */
void GseReadTrace( FILE *fp, SGseTrace *gsetrc, TSyStatus *status );
void GseOpenFile( char file[], TSyStatus *status );
void GseCloseFile( void );
void GseNextTrace( SGseTrace *gsetrc, TSyStatus *status );
void GseSkipTrace( TSyStatus *status );

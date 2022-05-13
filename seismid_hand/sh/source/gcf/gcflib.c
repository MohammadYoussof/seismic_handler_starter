
/* file gcflib.c
 *      ========
 *
 * version 5, 5-Nov-2006
 *
 * interface for GCF data
 * K. Stammler, 18-Oct-2003
 */


#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "tcusrdef.h"
#include "gcflib.h"
#include "../seed_io/seedcfg.h"
#include "../seed_io/seed_lib.h"

#define GcfMAXTAP 10
#define DEFAULT_BLKLTH 1024
#define ROOTDIR "$ROOT"
#define ROOTDIRLTH 5
#define MAX_SMP_PER_BLK 5000

/* global variables */
static int gcfv_byteorder[4] = { 0, 1, 2, 3 };  /* 4byte order */
static TSyBoolean gcfv_capfiles=FALSE;   /* capitalized filenames in gfd */
static GcfFileDescrT  gcfv_fd[Gcf_C_MAXCHAN];    /* file descriptors */
static TSyBoolean gcfv_logrec=FALSE;
static char gcfv_last_fname[cBcFileLth+1]; /* last file read */

/* default list for translation of tap numbers into channel names: */
static char gcfv_tapcode[GcfMAXTAP][GcfCHAN_LENGTH+1] = {
	"HH", "HH", "HH", "BH", "BH", "BH", "LH", "LH", "LH", "UH"
};


/* prototypes of local routines */
static TSyBoolean GcfIsLeapYear( int year );
static void GcfAdjustFilename( GcfFileDescrT *dsc, TSyStatus *status );



/*----------------------------------------------------------------------------*/



void GcfSetByteorder( char byteorder[], TSyStatus *status )

/* Sets byte order.  Bytes are counted from 1 to 4, Legal strings are
 * e.g. "1234" or "2143".
 *
 * parameters of routine
 * char       byteorder[]; input; byte order string of length 4 (+term char)
 * TSyStatus  *status; output; return status
 */
{
	/* local variables */
	int      chk[4];     /* check occurence of all numbers */
	int      bo[4];      /* new byteorder */
	int      i;          /* counter */

	/* executable code */

	if  (strlen(byteorder) != 4)  {
		*status = GcfERR_ILLBYTEORDER;
		return;
	} /*endif*/

	/* reset check array */
	for  (i=0; i<4; i++)  chk[i] = 0;

	for  (i=0; i<4; i++)  {

		if  (byteorder[i] < '1' || byteorder[i] > '4')  {
			*status = GcfERR_ILLBYTEORDER;
			return;
		} /*endif*/

		bo[i] = byteorder[i] - '1';
		chk[bo[i]] = 1;

	} /*endif*/

	/* check wether all numbers specified */
	for  (i=0; i<4; i++)
		if  (chk[i] != 1)  {
			*status = GcfERR_ILLBYTEORDER;
			return;
		} /*endif*/

	/* copy new byteorder to global variable */
	for  (i=0; i<4; i++)
		gcfv_byteorder[i] = byteorder[i];

} /* end of GcfSetByteorder */



/*----------------------------------------------------------------------------*/



void GcfSetTapcode( int tapnum, char chan[], TSyStatus *status )

/* sets tap code, i.e. mapping from tap number to channel name
 *
 * parameters of routine
 * int        tapnum;     tap number to be set
 * char       chan[];     2 char channel code
 */
{
	/* executable code */

	if  (tapnum < 0 || tapnum >= GcfMAXTAP)  {
		*status = GcfERR_ILLTAP;
		return;
	} /*endif*/

	gcfv_tapcode[tapnum][0] = chan[0];
	gcfv_tapcode[tapnum][1] = chan[1];
	gcfv_tapcode[tapnum][2] = '\0';

} /* end of GcfSetTapcode */



/*----------------------------------------------------------------------------*/



void GcfReadUserTapcodes( char fname[], TSyStatus *status )

/* Reads tap code names from file
 *
 * parameters of routine
 * char       fname[];    input; name of file or NULL
 *                              (then read from $HOME/.tapcodes)
 * TSyStatus  *status;    output; return status
 */
{
	/* local variables */
	char     fn[cBcFileLth+1];   /* name of input file */
	char     *env;               /* pointer to environment */
	FILE     *fp;                /* pointer to input file */
	char     line[cBcLineLth+1]; /* current line of file */
	int      tc;                 /* tapcode */
	char     chan[cBcLineLth+1]; /* channel name */

	/* executable code */

	if  (fname == NULL)  {
		env = (char *)getenv( "HOME" );
		if  (strlen(env) >= cBcFileLth-10)  {
			*status = GcfERR_STROVFL;
			return;
		} /*endif*/
		strcpy( fn, env );
		strcat( fn, "/.tapcodes" );
	} else {
		if  (strlen(fname) >= cBcFileLth)  {
			*status = GcfERR_STROVFL;
			return;
		} /*endif*/
		strcpy( fn, fname );
	} /*endif*/

	fp = fopen( fn, "r" );
	if  (fp == NULL)  {
		fprintf( stderr,
			"gcf: no user defined tapcodes found, using defaults\n" );
		return;
	} /*endif*/

	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		if  (*line == '!' || *line == '#' || *line == '\n')  continue;
		if  (sscanf(line,"%d %s",&tc,chan) != 2)  {
			*status = GcfERR_TAPCODEFILE;
			return;
		} /*endif*/
		if  (tc < 0 || tc > 9)  {
			*status = GcfERR_ILLTAPCODE;
			return;
		} /*endif*/
		if  (strlen(chan) > 2)  {
			*status = GcfERR_TAPCODEFILE;
			return;
		} /*endif*/
		gcfv_tapcode[tc][0] = chan[0];
		gcfv_tapcode[tc][1] = chan[1];
		gcfv_tapcode[tc][2] = '\0';
	} /*endwhile*/

	fclose( fp );

} /* end of GcfReadUserTapcodes */



/*----------------------------------------------------------------------------*/



void GcfReadRawHeader( FILE *gcf, GcfRawHeaderT *rhdr, TSyStatus *status )

/* Reads raw header from GCF file.
 *
 * parameters of routine
 * FILE          *gcf;      input; pointer to input GCF file (should be rewound)
 * GcfRawHeaderT *rhdr;     output; header longwords read from file
 * TSyStatus     *status;   output; return status
 */
{
	/* local variables */

	/* executable code */

	rhdr->system_id = GcfGet4Bytes( gcf, status );
	if  (SySevere(status))  {
		*status = GcfERR_EOF_FOUND;
		return;
	} /*endif*/
	rhdr->stream_id = GcfGet4Bytes( gcf, status );
	if  (SySevere(status))  return;
	rhdr->date_code = GcfGet4Bytes( gcf, status );
	if  (SySevere(status))  return;
	rhdr->data_format = GcfGet4Bytes( gcf, status );
	if  (SySevere(&status))  return;

} /* end of GcfReadRawHeader */



/*----------------------------------------------------------------------------*/



void GcfDecodeHeader( GcfRawHeaderT *rhdr, GcfHeaderT *hdr, TSyStatus *status )

/* Decodes raw header
 *
 * parameters fo routine
 * GcfRawHeaderT *rhdr;       input; raw header
 * GcfHeaderT    *hdr;        output; decoded header
 * TSyStatus     *status;     output; return status
 */
{
	/* local variables */
	int           i;           /* counter */
	NTIME         ntime;       /* numeric start time */

	/* executable code */

	/* decode two base36 strings */
	GcfBase36ToString( rhdr->system_id, hdr->system_id );
	GcfBase36ToString( rhdr->stream_id, hdr->stream_id );

	/* get station, chan, comp */
	strncpy( hdr->station, hdr->stream_id, GcfSTATION_LENGTH );
	i = GcfSTATION_LENGTH;
	while  (i > 0 && hdr->station[i] <= ' ')
		hdr->station[i--] = '\0';
	i = hdr->stream_id[GcfSTATION_LENGTH+1] - '0';
	if  (i < 0 || i >= GcfMAXTAP)  {
		*status = GcfERR_ILLTAP;
		return;
	} /*endif*/
	hdr->chan[0] = gcfv_tapcode[i][0];
	hdr->chan[1] = gcfv_tapcode[i][1];
	hdr->chan[2] = '\0';
	hdr->chan[GcfCHAN_LENGTH] = '\0';
	hdr->comp = hdr->stream_id[GcfSTATION_LENGTH];

	/* get time info */
	GcfDecodeTime( rhdr->date_code, &ntime );
	tc_n2t( &ntime, hdr->blktime, status );
	if  (SySevere(status))  return;
	i = strlen( hdr->blktime ) - 4;
	if  (strcmp(hdr->blktime+i,".000") == 0)  hdr->blktime[i] = '\0';

	/* data format bytes */
	hdr->smprate = (rhdr->data_format & 0x00ff0000) >> 16;
	hdr->cmpcode = (rhdr->data_format & 0x0000ff00) >>  8;
	hdr->numrec  = (rhdr->data_format & 0x000000ff);

} /* end of GcfDecodeHeader */



/*----------------------------------------------------------------------------*/



void GcfPrintHeader( FILE *out, GcfHeaderT *hdr )

/* Prints header infomration in one line to given output stream
 *
 * parameters of routine
 * FILE       *out;      input; output stream
 * GcfHeaderT *hdr;      input; header to be printed
 */
{
	/* local variables */
	int      numbits;       /* number of bits */

	/* executable code */

	switch (hdr->cmpcode)  {
	case GcfCMP_8BIT:
		numbits = 8;
		break;
	case GcfCMP_16BIT:
		numbits = 16;
		break;
	case GcfCMP_32BIT:
		numbits = 32;
		break;
	default:
		numbits = 0;
		break;
	} /*endswitch*/

	fprintf( out, "%6s %6s %4s-%2s-%c %20s %3dHz %2dbit %3dr",
		hdr->system_id, hdr->stream_id, hdr->station, hdr->chan, hdr->comp,
		hdr->blktime, hdr->smprate, numbits, hdr->numrec );

} /* end of GcfPrintHeader */



/*----------------------------------------------------------------------------*/



void GcfSkipDataBlock( FILE *gcf, GcfRawHeaderT *rhdr )

/* skips data block (jumps to next header or EOF) after a raw header has been
 * read
 * 
 * parameters of routine
 * FILE          *gcf; input; GCF data input file
 * GcfRawHeaderT *rhdr; input; raw header previously read
 */
{
	/* local variables */
	int      numrec;     /* number of records (longwords) following */

	/* executable code */

	numrec  = (rhdr->data_format & 0x000000ff);

	/*fseek( gcf, (numrec+2)*sizeof(unsigned INT32), SEEK_CUR );*/
	fseek( gcf, 252*sizeof(unsigned INT32), SEEK_CUR );

} /* GcfSkipDataBlock */



/*----------------------------------------------------------------------------*/



GcfLongT *GcfAllocBlockMem( TSyStatus *status )

/* allocates memory for raw (coded) data block
 *
 * parameters of routine
 * TSyStatus  *status;      output; return status
 *                          returns pointer to block memory
 */
{
	/* local variables */
	GcfLongT *blockmem;

	/* executable code */

	/* 1kB is enough for max 250 records */
	blockmem = (GcfLongT *)malloc( 1024 );

	if  (blockmem == NULL)  *status = GcfERR_ALLOCMEM;
	return blockmem;

} /* end of GcfAllocBlockMem */



/*----------------------------------------------------------------------------*/



int *GcfAllocBlockSamples( TSyStatus *status )

/* allocates memory for decoded samples of a block
 *
 * parameters of routine
 * TSyStatus  *status;      output; return status
 *                          returns pointer to memory for samples
 */
{
	/* local variables */
	int *mem;

	/* executable code */

	/* 4kB is enough for max 250 records of 8bit each */
	mem = (int *)malloc( 4096 );

	if  (mem == NULL)  *status = GcfERR_ALLOCMEM;
	return mem;

} /* end of GcfAllocBlockSamples */



/*----------------------------------------------------------------------------*/



void GcfReadDataBlock( FILE *gcf, GcfRawHeaderT *rhdr, GcfLongT *mem,
	TSyStatus *status )

/* read in single data block from GCF file.  Corresponding header must
 * be already read in.
 *
 * parameters of routine
 * FILE       *gcf;      input; pointer to input GCF file
 * GcfRawHeaderT *rhdr;  input; GCF raw header previously read
 * GcfLongT   *mem;      output; block read in
 * TSyStatus  *status;   output; return status
 */
{
	/* local variables */
	int      i;            /* counter */
	int      numrecs=252;  /* number of records to read */

	/* executable code */

	/* assume 1kB data blocks and header already read in */
	for  (i=0; i<numrecs; i++)  {
		mem[i] = GcfGet4Bytes( gcf, status );
		if  (SySevere(status))  return;
	} /*endif*/

} /* end of GcfReadDataBlock */



/*----------------------------------------------------------------------------*/



void GcfDecodeBlock( GcfRawHeaderT *rhdr, GcfLongT *block, int *samples,
	int *smpnum, TSyStatus *status )

/* Decodes samples of a block.  Memory must be already allocated using
 * appropriate routines
 *
 * paramters of routine
 * GcfRawHeaderT *rhdr;      input; GCF raw header previously read
 * GcfLongT      *block;     input; data block previously read
 * int           *samples;   output; samples decoded
 * int           *smpnum;    output; number of samples
 * TSyStatus     *status;    output; return status
 */
{
	/* local variables */
	int      i, j;     /* counters */
	int      numrec;   /* number of records */
	int      cmpcode;  /* compression code */
	int      cursmp;   /* current sample */
	signed char sbyte; /* byte diff */
	signed short si;   /* 16 bit diff */

	/* executable code */

	cmpcode = (rhdr->data_format & 0x0000ff00) >>  8;
	numrec  = (rhdr->data_format & 0x000000ff);

	cursmp = block[0];

	j = 0;
	if  (cmpcode == GcfCMP_8BIT)  {
		for  (i=1; i<=numrec; i++)  {
			sbyte = (block[i] & 0xff000000) >> 24;
			cursmp += sbyte;
			samples[j++] = cursmp;
			sbyte = (block[i] & 0x00ff0000) >> 16;
			cursmp += sbyte;
			samples[j++] = cursmp;
			sbyte = (block[i] & 0x0000ff00) >>  8;
			cursmp += sbyte;
			samples[j++] = cursmp;
			sbyte = (block[i] & 0x000000ff);
			cursmp += sbyte;
			samples[j++] = cursmp;
		} /*endif*/
	} else if  (cmpcode == GcfCMP_16BIT)  {
		for  (i=1; i<=numrec; i++)  {
			si = (block[i] & 0xffff0000) >> 16;
			cursmp += si;
			samples[j++] = cursmp;
			si = (block[i] & 0x0000ffff);
			cursmp += si;
			samples[j++] = cursmp;
		} /*endif*/
	} else if  (cmpcode == GcfCMP_32BIT)  {
		for  (i=1; i<=numrec; i++)  {
			cursmp += block[i];
			samples[j++] = cursmp;
		} /*endif*/
	} else {
		/* no data block */
	} /*endif*/

	*smpnum = j;

	if  (samples[j-1] != block[numrec+1])
		*status = GcfERR_CHKSUM;

} /* GcfDecodeBlock */



/*---------------------------------------------------------------------*/



void GcfFindFileInGfd( char stream_str[], char start[], char gfdfile[],
	GcfFileDescrT *fdescr, TSyStatus *status )

/* Finds GCF file in gfd-file of given stream and containing given
 * start time.  If no file is found the file with the next possible
 * time is returned
 *
 * parameters of routine
 * char       stream_str[];      input; stream string like "bfo-vbb-z"
 * char       start[];           input; start time
 * char       gfdfile[];         input; GCF file directory
 * GcfFileDescrT *fdescr;        output; GCF file descriptor
 * TSyStatus  *status;           output; return status
 */
{
	/* local variables */
	FILE     *gfd;                          /* pointer to gfd-file */
	char     line[Gcf_C_GFDLINELTH+1];      /* current line */
	float    tdiff;                         /* time difference */
	float    min_tdiff;                     /* minimum tdiff */
	GcfFileDescrT next;                     /* file with next time */
	GcfFileDescrT curr;                     /* current line */
	char     lstream_str[cBcShortStrLth+1]; /* local lowercase stream string */

	/* executable code */

	if  (strlen(stream_str) > cBcShortStrLth)  {
		*status = GcfERR_STROVFL;
		err_setcontext( " ## stream " );
		err_setcontext( stream_str );
		return;
	} /*endif*/

	/* make stream string lowercase */
	strcpy( lstream_str, stream_str );
	ut_uncap( lstream_str );

	/* initialize local descriptors */
	strcpy( next.stream, lstream_str );
	strcpy( curr.stream, lstream_str );
	next.t_start[0] = curr.t_start[0] = '\0';
	next.t_end[0] = curr.t_end[0] = '\0';
	next.name[0] = curr.name[0] = '\0';
	strcpy( next.gfdfile, gfdfile );
	strcpy( curr.gfdfile, gfdfile );
	next.blkno = curr.blkno = 0;
	next.fp = curr.fp = NULL;
	next.pos = curr.pos = 0;
	next.sample = curr.sample = 0;
	next.calib = curr.calib = 0.0;
	next.dataflags = curr.dataflags = 0;
	next.blklth = curr.blklth = DEFAULT_BLKLTH;

	gfd = sy_fopen( gfdfile, "r" );
	if  (gfd == NULL)  {
		*status = GcfERR_OPENINPUT;
		err_setcontext( " ## file " );
		err_setcontext( gfdfile );
		return;
	} /*endif*/

	/* find requested stream and time in gfd-file */
	next.name[0] = '\0';
	min_tdiff = 1.0e10;
	while  (fgets(line,Gcf_C_GFDLINELTH,gfd) != NULL)  {
		if  (*line == '!')  continue;
		GcfParseGfdLine( line, &curr, status );
		if  (SySevere(status))  {fclose(gfd); return;}
		if  (strcmp(curr.stream,lstream_str) != 0)  continue;
		tdiff = tc_tdiff( start, curr.t_start, status );
		if  (SySevere(status))  {fclose(gfd); return;}
		if  (tdiff >= 0.0 && tc_tdiff(start,curr.t_end,status) < 0.0)  {
			/* found it ! */
			*fdescr = curr;
			fclose( gfd );
			if  (fdescr->calib == 0.0)  {
				fdescr->calib =
					GcfFindStreamCalibration( fdescr->stream, start, status );
				if  (*status == GcfERR_NOCALIB)  {
					*status = cBcNoError;
					fdescr->dataflags |= Gcf_F_QUAL_NOCALIB;
				} /*endif*/
			} /*endif*/
			if  (strncmp(fdescr->name,ROOTDIR,ROOTDIRLTH) == 0)
				GcfAdjustFilename( fdescr, status );
			return;
		} else if  (tdiff < 0.0)  {
			/* store file with time next to requested */
			if  (SySevere(status))  {fclose(gfd); return;}
			if  (next.name[0] == '\0' || tdiff > min_tdiff)  {
				min_tdiff = tdiff;
				next = curr;
			} /*endif*/
			if  (next.calib == 0.0)  {
				next.calib =
					GcfFindStreamCalibration( next.stream, start, status );
				if  (*status == GcfERR_NOCALIB)  {
					*status = cBcNoError;
					next.dataflags |= Gcf_F_QUAL_NOCALIB;
				} /*endif*/
			} /*endif*/
		} /*endif*/
		if  (SySevere(status))  {fclose(gfd); return;}
	} /*endwhile*/

	*status = (next.name[0] == '\0') ? GcfERR_GFD_NOTFOUND : GcfERR_GFD_NEXT;
	if  (*status == GcfERR_GFD_NOTFOUND)  {
		err_setcontext( " ## stream " );
		err_setcontext( lstream_str );
		err_setcontext( ", time " );
		err_setcontext( start );
	} /*endif*/
	fclose( gfd );
	*fdescr = next;
	if  (strncmp(fdescr->name,ROOTDIR,ROOTDIRLTH) == 0)
		GcfAdjustFilename( fdescr, status );
	return;

} /* end of GcfFindFileInGfd */



/*---------------------------------------------------------------------*/



void GcfSearchPosition( int chan, char gfdfile[], char stream_str[],
	char req_start[], char act_start[], TSyStatus *status )

/* Searches for given time position in a stream file.  The corresponding
 * GCF file is opened and positioned, its file descriptor stored
 * in an internal variable.  'chan' must range from 0..Gcf_C_MAXCHAN-1.
 * If the channel is already used the previously opened file is closed.
 *
 * parameters of routine
 * int        chan;             input; channel number
 * char       gfdfile[];        input; gfd-file
 * char       stream_str[];     input; stream string
 * char       req_start[];      input; requested start time
 * char       act_start[];      output; actual start time
 * TSyStatus  *status;          output; return status
 */
{
	/* local variables */
	TSyBoolean  next_time;   /* only next time possible */
	float    tdiff_tot;      /* total time window of file */
	float    tdiff_start;    /* difference to request time */
	GcfFileDescrT *fd;       /* pointer to file descriptor */
	INT32    seek_pos;       /* seek position */
	NTIME    req_ntime;      /* requested time in NTIME format */
	NTIME    cur_ntime;      /* start time of current GCF block */
	float    cur_span;       /* time span (sec) of current GCF block */
	float    dt;             /* sample distance in sec */
	int      jump_blk;       /* number of blocks to jump */
	int      new_pos;        /* new block position */
	int      jump_cnt;       /* jump counter */
	GcfRawHeaderT rhdr;      /* GCF raw header */
	GcfHeaderT hdr;          /* GCF decoded header */

	/* executable code */

	if  (chan < 0 || chan >= Gcf_C_MAXCHAN)  {
		*status = GcfERR_CHAN_OOR;
		return;
	} /*endif*/
	fd = gcfv_fd + chan;

	if  (fd->fp != NULL)  {
		fclose( fd->fp );
		fd->fp = NULL;
	} /*endif*/

	GcfFindFileInGfd( stream_str, req_start, gfdfile, fd, status );
	next_time =  (*status == GcfERR_GFD_NEXT);
	if  (next_time)  *status = cBcNoError;
	if  (SySevere(status))  return;

	if  (gcfv_logrec)  printf( "{%s}", fd->name );
	fd->fp = fopen( fd->name, "r" );
	if  (fd->fp == NULL)  {
		*status = GcfERR_OPENINPUT;
		err_setcontext( " ## file " );
		err_setcontext( fd->name );
		fd->fp = NULL;
		return;
	} /*endif*/

	if  (next_time)  {
		strcpy( act_start, fd->t_start );
		*status = GcfERR_NEXTTIMEPOS;
		return;
	} /*endif*/
	tc_t2n( req_start, &req_ntime, status );
	if  (SySevere(status))  return;

	/* compute approximate block position */
	tdiff_tot = tc_tdiff( fd->t_end, fd->t_start, status );
	if  (SySevere(status))  return;
	tdiff_start = tc_tdiff( req_start, fd->t_start, status );
	if  (SySevere(status))  return;
	new_pos = Nint( (float)((fd->blkno)-1)/tdiff_tot * tdiff_start );

	for  (jump_cnt=0; jump_cnt<4; jump_cnt++)  {

		/* position GCF file */
		if  (new_pos != fd->pos)  {
			if  (new_pos >= fd->blkno)  new_pos = fd->blkno-1;
			if  (new_pos < 0)  new_pos = 0;
			fd->pos = new_pos;
			seek_pos = (INT32)(fd->pos) * (INT32)(fd->blklth);
			if  (gcfv_logrec)  printf( "-" );
			if  (fseek(fd->fp,seek_pos,SEEK_SET) != 0/*seek_pos*/)  {
				*status = GcfERR_GCFSEEK;
				err_setcontext( " ## file " );
				err_setcontext( fd->name );
				return;
			} /*endif*/
		} /*endif*/

		/* read current GCF block */
		if  (gcfv_logrec)  printf( "[%d]", fd->pos );  /* log */
		GcfReadRawHeader( fd->fp, &rhdr, status );
		if  (SySevere(status)) {
			err_setcontext( " ## file " );
			err_setcontext( fd->name );
			return;
		} /*endif*/
		GcfSkipDataBlock( fd->fp, &rhdr );
		GcfDecodeHeader( &rhdr, &hdr, status );
		if  (SySevere(status)) {
			err_setcontext( " ## file " );
			err_setcontext( fd->name );
			return;
		} /*endif*/
		(fd->pos)++;
		dt = (hdr.smprate == 0) ? 0.0 : 1.0/(float)hdr.smprate;

		/* check time found (return if time found) */
		tc_t2n( hdr.blktime, &cur_ntime, status );
		if  (SySevere(status))  return;
		tdiff_start = tc_ndiff( &req_ntime, &cur_ntime, status );
		if  (SySevere(status))  return;
		cur_span = (float)(hdr.numrec*hdr.cmpcode/hdr.smprate);
		if  (tdiff_start >= 0.0)  {
			if  (cur_span > tdiff_start)  {
				/* found it ! */
				seek_pos = (INT32)(--(fd->pos)) * (INT32)(fd->blklth);
				fseek( fd->fp, seek_pos, SEEK_SET );
				fd->sample = Nint( tdiff_start/dt );
				if  (fd->sample >/*=*/ (hdr.numrec*hdr.cmpcode))  {
					printf( "! GcfSearchPosition: sample position adjusted (1)\n" );
					fd->sample = hdr.numrec*hdr.cmpcode /*- 1*/;
				} /*endif*/
				tc_nadd( &cur_ntime, (fd->sample)*dt, &req_ntime, status );
				if  (Severe(status))  return;
				tc_n2t( &req_ntime, act_start, status );
				return;
			} /*endif*/
		} /*endif*/

		/* estimate next jump */
		if  (tdiff_start > 0.0)  tdiff_start -= cur_span;
		jump_blk = Nint( (tdiff_start/cur_span) - 0.5 );
		if  (jump_blk >= 0)  jump_blk++;  else  jump_blk--;
		new_pos = fd->pos - 1 + jump_blk;

	} /*endfor*/

	/* if still not found then read block by block */
	if  (gcfv_logrec)  printf( "|" );

	/* go back to time before */
	tdiff_start = tc_ndiff( &req_ntime, &cur_ntime, status );
	while  (tdiff_start < 0.0)  {
		fd->pos -= 2;
		if  (fd->pos < 0)  {
#ifdef XXX
			if  ((tdiff_start > ((-1.5)*(float)(gcfv_blk->timecorr)/TIMECORRUNIT))
				&& gcfv_use_timecorr)  {
				/* This may happen if the gfdfile does not contain possible time  */
				/* corrections of the data.  Try to fix this by positioning to    */
				/* a time shifted by the correction.  The samples before this time*/
				/* are not read.  Call this routine recursively.                  */
				{
					char  newtime[cBcTimeLth+1];    /* new time position */
					tc_tadd( req_start, (float)(gcfv_blk->timecorr)/TIMECORRUNIT,
						newtime, status );
					if  (SySevere(status))  {
						fprintf( stderr,
							"*** GcfSearchPosition: error in time calc\n");
						*status = GcfERR_BUG;
						return;
					} /*endif*/
					fprintf( stderr,
						"GcfSearchPosition: try to fix time correction bug\n" );
					GcfSearchPosition( chan, gfdfile, stream_str, newtime,
						act_start, status );
					return;
				}
			} else {
#endif
				fprintf( stderr, "*** GcfSearchPosition: check gfdfile\n" );
				*status = GcfERR_BUG;
				return;
#ifdef XXX
			} /*endif*/
#endif
		} /*endif*/
		seek_pos = (INT32)(fd->pos) * (INT32)(fd->blklth);
		if  (gcfv_logrec)  printf( "-" );
		if  (fseek(fd->fp,seek_pos,SEEK_SET) != 0/*seek_pos*/)  {
			*status = GcfERR_GCFSEEK;
			err_setcontext( " ## file " );
			err_setcontext( fd->name );
			return;
		} /*endif*/
		if  (gcfv_logrec)  printf( "[%d]", fd->pos );  /* log */
		GcfReadRawHeader( fd->fp, &rhdr, status );
		if  (SySevere(status))  {
			err_setcontext( " ## file " );
			err_setcontext( fd->name );
			return;
		} /*endif*/
		GcfSkipDataBlock( fd->fp, &rhdr );
		GcfDecodeHeader( &rhdr, &hdr, status );
		if  (SySevere(status))  return;
		(fd->pos)++;
		dt = (hdr.smprate == 0) ? 0.0 : 1.0/(float)hdr.smprate;
		tc_t2n( hdr.blktime, &cur_ntime, status );
		if  (Severe(status))  return;
		/* check for time correction, K.S. 16-Mar-99 */
		tdiff_start = tc_ndiff( &req_ntime, &cur_ntime, status );
		if  (Severe(status))  return;
		cur_span = (float)(hdr.numrec*hdr.cmpcode/hdr.smprate);
	} /*endwhile*/

	/* now find forward */
	while  (tdiff_start >= cur_span)  {
		if  (gcfv_logrec)  printf( "[%d]", fd->pos );  /* log */
		GcfReadRawHeader( fd->fp, &rhdr, status );
		if  (SySevere(status))  {
			err_setcontext( " ## file " );
			err_setcontext( fd->name );
			return;
		} /*endif*/
		(fd->pos)++;
		GcfSkipDataBlock( fd->fp, &rhdr );
		GcfDecodeHeader( &rhdr, &hdr, status );
		if  (SySevere(status))  return;
		dt = (hdr.smprate == 0) ? 0.0 : 1.0/(float)hdr.smprate;
		tc_t2n( hdr.blktime, &cur_ntime, status );
		if  (Severe(status))  return;
		tdiff_start = tc_ndiff( &req_ntime, &cur_ntime, status );
		if  (Severe(status))  return;
		cur_span = (float)(hdr.numrec*hdr.cmpcode/hdr.smprate);
	} /*endwhile*/

	/* step one block back to get it again on next read command */
	seek_pos = (INT32)(--(fd->pos)) * (INT32)(fd->blklth);
	fseek( fd->fp, seek_pos, SEEK_SET );

	if  (tdiff_start >= 0.0)  {
		/* now found it */
		fd->sample = Nint( tdiff_start/dt );
		if  (fd->sample >/*=*/ (int)(hdr.numrec*hdr.cmpcode))  {
			printf( "! GcfSearchPosition: sample position adjusted (2)\n" );
			fd->sample = hdr.numrec*hdr.cmpcode /*- 1*/;
		} /*endif*/
		tc_nadd( &cur_ntime, (fd->sample)*dt, &req_ntime, status );
		if  (Severe(status))  return;
		tc_n2t( &req_ntime, act_start, status );
	} else {
		/* time gap */
		/* printf( "*** time gap: next possible time positioned ***\n" ); */
		tc_n2t( &cur_ntime, act_start, status );
		*status = GcfERR_NEXTTIMEPOS;
	} /*endif*/

} /* end of GcfSearchPosition */



/*---------------------------------------------------------------------*/



void GcfReadNextBlock( int chan, GcfRawHeaderT *rhdr, GcfLongT *blk,
	TSyStatus *status )

/* Reads next available block (header and data) from GCF file.
 *
 * parameters of routine
 * int        chan;               input; channel number
 * GcfRawHeaderT *rhdr;           output; coded header of block
 * GcfLongT   *blk;               output; coded data of block
 * TSyStatus  *status;            output; return status
 */
{
	/* local variables */
	GcfFileDescrT *fd;                   /* GCF file descriptor */
	char     l_gfdfile[cBcFileLth+1];    /* local sfdfile */
	char     l_stream[cBcShortStrLth+1]; /* local stream string */
	char     l_t_end[cBcTimeLth+1];      /* end of file time */
	char     act_time[cBcTimeLth+1];     /* positioned time */
	TSyBoolean time_gap;                 /* time gap found */

	/* executable code */

	/* check channel number */
	if  (chan < 0 || chan >= Gcf_C_MAXCHAN)  {
		*status = GcfERR_CHAN_OOR;
		return;
	} /*endif*/
	fd = gcfv_fd + chan;

	if  (fd->fp == NULL)  {
		*status = GcfERR_NOT_OPEN;
		return;
	} /*endif*/

	/* store filename for last used file request */
	strcpy( gcfv_last_fname, fd->name );

	time_gap = FALSE;
	if  (fd->pos >= fd->blkno)  {
		/* is at end of file */
		strcpy( l_gfdfile, fd->gfdfile );
		strcpy( l_stream, fd->stream );
		strcpy( l_t_end, fd->t_end );
		GcfSearchPosition( chan, l_gfdfile, l_stream, l_t_end,
			act_time, status );
		time_gap = (*status == GcfERR_NEXTTIMEPOS);
		if  (time_gap)  *status = cBcNoError;
		if  (SySevere(status))  return;
	} /*endif*/

	if  (gcfv_logrec)  printf( "<%d>", fd->pos );  /* log */
	GcfReadRawHeader( fd->fp, rhdr, status );
	if  (SySevere(status)) return;
	GcfReadDataBlock( fd->fp, rhdr, blk, status );
	if  (SySevere(status)) return;
	(fd->pos)++;

	if  (time_gap)  *status = GcfERR_NEXTTIMEPOS;

} /* end of GcfReadNextBlock */



/*---------------------------------------------------------------------*/



#define ON_ERROR_RETURN  \
	if  (SySevere(status))  { \
		GcfFreeMem(blk); \
		GcfFreeMem(blksmp); \
		return; \
	}



void GcfReadStream( int chan, char gfdfile[], char stream_str[],
	TSyBoolean unused, char req_start[], float seclth, INT32 **ldat,
	INT32 *smplth, char act_start[], float *dt, float *calib,
	int *flags, TSyStatus *status )

/* Reads sample data from GCF file.  The memory for the sample
 * data is allocated by the routine.  The channel number is chosen
 * by the caller of the routine.  It is legal to use always channel
 * number 0 but it is faster to use a separate channel number for
 * each stream used if the streams are read repeatedly.
 *
 * parameters of routine
 * int        chan;         input; channel number (0..Gcf_C_MAXCHAN-1)
 * char       gfdfile[];    input; name of gfd file to use
 * char       stream_str[]; input; stream string (like "bfo-vbb-z")
 * TSyBoolean unused;
 * char       req_start[];  input; requested start time
 * float      seclth;       input; number of seconds to be read
 * INT32      **ldat;       output; sample array read
 * INT32      *smplth;      output; number of samples read
 * char       act_start[];  output; actual start time
 * float      *dt;          output; sample distance in sec
 * float      *calib;       output; calibration constant
 * int        *flags;       output; data flags found
 * TSyStatus  *status;      output; return status
 */
{
	/* local variables */
	GcfFileDescrT *fd;    /* pointer to SEED file descriptor */
	INT32    getlth;      /* number of samples to read */
	INT32    maxblksmp;   /* maximum number of samples per block */
	int      actblksmp;   /* actual number of samples in block */
	int      *lsrc, *ldst;/* moving pointers */
	NTIME    blkstart;    /* block start time */
	NTIME    blkend;      /* block end time */
	NTIME    expectime;   /* expected time */
	float    tdiff;       /* time gap in sec */
	INT32    padcnt;      /* counter for zero padding */
	char     str[cBcTimeLth+1];  /* scratch string for time output */
	int      i;           /* counter */
	GcfLongT *blk;        /* coded block data */
	int      *blksmp;     /* decoded samples of a block */
	GcfRawHeaderT rhdr;   /* coded GCF header */
	GcfHeaderT hdr;       /* decoded GCF header */
	int      lastsmp;     /* value of last sample (for padding) */

	/* executable code */

	GcfReadUserTapcodes( NULL, status );
	if  (SySevere(status))  return;

	/* check channel number */
	if  (chan < 0 || chan >= Gcf_C_MAXCHAN)  {
		*status = GcfERR_CHAN_OOR;
		return;
	} /*endif*/
	fd = gcfv_fd + chan;

	/* find requested position */
	tdiff = 0.0;
	GcfSearchPosition( chan, gfdfile, stream_str, req_start,
		act_start, status );
	if  (*status == GcfERR_NEXTTIMEPOS)  {
		*status = cBcNoError;
		tdiff = tc_tdiff( act_start, req_start, status );
		if  (SySevere(status))  return;
		if  (tdiff >= seclth)  {*status = GcfERR_DATA_UNAVAIL; return;}
		fprintf( stderr,
			"GcfReadStream: time %s not found, start at %s\n",
			req_start, act_start );
	} /*endif*/
	if  (SySevere(status))  return;

	maxblksmp = MAX_SMP_PER_BLK;
	*smplth = 0;
	*ldat = NULL;
	*dt = 0.0;
	expectime.year = 0;
	blk = GcfAllocBlockMem( status );
	if  (SySevere(status))  return;
	blksmp = GcfAllocBlockSamples( status );
	if  (SySevere(status))  {
		GcfFreeMem( blk );
		return;
	} /*endif*/
	lastsmp = 0;

	FOREVER  {

		/* read next block */
		GcfReadNextBlock( chan, &rhdr, blk, status );
		if  (*status == GcfERR_NEXTTIMEPOS)  *status = cBcNoError;
		ON_ERROR_RETURN
		GcfDecodeHeader( &rhdr, &hdr, status );
		ON_ERROR_RETURN

		/* decode GCF block and select samples */
		/* ------------------------------------- */
		if  (*ldat == NULL)  {

			/* this is for first loop only */
			/* --------------------------- */
			*calib = fd->calib;
			*dt = (hdr.smprate == 0) ? 0.0 : 1.0/(float)hdr.smprate;
			getlth = Nint32( seclth / *dt );
			if  (getlth <= 0)  {*status = GcfERR_ZEROLTH; return;}
			*ldat = (INT32 *)sy_allocmem( getlth, (int)sizeof(INT32), status );
			ON_ERROR_RETURN
			ldst = (int *)*ldat;
			/* if time gap at beginning, pad zeroes */
			if  (tdiff > 0.0)  {
				/* pad with zeroes and adjust start time */
				padcnt = Nint32( tdiff / (*dt) );
				fprintf( stderr, "GcfReadStream: stream %s, time %s\n",
					hdr.stream_id, act_start );
				fprintf( stderr, "   gap %5.3f sec, start with %ld zeroes\n",
					tdiff, padcnt );
				tc_tadd( act_start, -(*dt)*(float)padcnt, act_start, status );
				if  (SySevere(status))  *status = cBcNoError; /* ok, not elegant */
				while  (padcnt-- > 0)  {
					ldst[(*smplth)++] = 0;
					if  (*smplth == getlth)  {
						GcfFreeMem( blksmp );
						fprintf( stderr,
							"   padding aborted, %ld zeroes remaining\n", padcnt );
						strcpy( act_start, req_start );
						return;
					} /*endif*/
				} /*endwhile*/
			} /*endif*/
			GcfDecodeBlock( &rhdr, blk, blksmp, &actblksmp, status );
			ON_ERROR_RETURN
#ifdef XXX
			if  (actblksmp != gcfv_blk->no_of_samples)  {
				fprintf( stderr,
					"*** GCF block samples: expected %d, actual: %d\n",
					gcfv_blk->no_of_samples, actblksmp );
				if  (gcfv_decerr_abort)  {
					sy_deallocmem( *ldat );
					*ldat = NULL;
					GcfFreeMem( blksmp );
					GcfFreeMem( blk );
					*status = SeedERR_DECODE_ERR;
					return;
				} else {
					INT32    zeronum;       /* number of sample to zero */
					zeronum = gcfv_blk->no_of_samples;
					if  (zeronum > maxblksmp)  zeronum = maxblksmp;
					fprintf( stderr, "   zero %d samples and continue\n", zeronum );
					for  (i=0; i<zeronum; i++)
						blksmp[i] = 0;
				} /*endif*/
				actblksmp = gcfv_blk->no_of_samples;
			} /*endif*/
#endif
			lsrc = blksmp + fd->sample;
			actblksmp -= fd->sample;
			if  (actblksmp < 0)  {
				sy_deallocmem( *ldat );
				*ldat = NULL;
				GcfFreeMem( blksmp );
				GcfFreeMem( blk );
				*status = GcfERR_BUG;
				fprintf( stderr, "*** GcfReadStream: bug (1) [sample: %d] ***\n",
					fd->sample );
				return;
			} /*endif*/
			tc_t2n( hdr.blktime, &blkstart, status );
			ON_ERROR_RETURN
			tc_nadd( &blkstart, (float)(hdr.numrec*hdr.cmpcode/hdr.smprate),
				&expectime, status );
			ON_ERROR_RETURN

		} else {

			/* this is for all other loops */
			/* --------------------------- */

			/* decode block */
			GcfDecodeBlock( &rhdr, blk, blksmp, &actblksmp, status );
			ON_ERROR_RETURN
#ifdef XXX
			if  (actblksmp != gcfv_blk->no_of_samples)  {
				fprintf( stderr,
					"*** GCF block samples: expected %d, actual: %d\n",
					gcfv_blk->no_of_samples, actblksmp );
				if  (gcfv_decerr_abort)  {
					sy_deallocmem( *ldat );
					*ldat = NULL;
					GcfFreeMem( blksmp );
					GcfFreeMem( blk );
					*status = GcfERR_DECODE_ERR;
					return;
				} else {
					INT32    zeronum;       /* number of sample to zero */
					zeronum = gcfv_blk->no_of_samples;
					if  (zeronum > maxblksmp)  zeronum = maxblksmp;
					fprintf( stderr, "   zero %d samples and continue\n", zeronum );
					for  (i=0; i<zeronum; i++)
						blksmp[i] = 0;
				} /*endif*/
				actblksmp = gcfv_blk->no_of_samples;
			} /*endif*/
#endif
			lsrc = blksmp;

			/* check for time gaps, pad with zeroes */
			tc_t2n( hdr.blktime, &blkstart, status );
			ON_ERROR_RETURN
			tc_nadd( &blkstart, (float)(hdr.numrec*hdr.cmpcode/hdr.smprate),
				&blkend, status );
			ON_ERROR_RETURN
			tdiff = tc_ndiff( &blkstart, &expectime, status );
			ON_ERROR_RETURN
			if  (tdiff <= -(*dt))  {
				/* double data */
				padcnt = Nint32( -tdiff / (*dt) );
				tc_n2t( &blkstart, str, status );
				fprintf( stderr, "SeedReadStream: stream %s, time %s\n",
					hdr.stream_id, str );
				fprintf( stderr, "   double data, %ld samples skipped\n",
					padcnt );
				if  (padcnt > (INT32)(hdr.numrec*hdr.cmpcode))  {
					/*
					*status = GcfERR_DOUBLEDATA;
					GcfFreeMem( blk );
					GcfFreeMem( blksmp );
					return;
					*/
					/* throw way this block */
					continue;
				} /*endif*/
				/* skip samples */
				lsrc += padcnt;
				actblksmp -= padcnt;
			} else if  (tdiff >= (*dt))  {
				/* time gap */
				padcnt = Nint32( tdiff / (*dt) );
				tc_n2t( &blkstart, str, status );
				fprintf( stderr, "GcfReadStream: stream %s, time %s\n",
					hdr.stream_id, str );
				fprintf( stderr,
					"   gap %5.3f sec, padding %ld zeroes at sample %ld\n",
					tdiff, padcnt, *smplth );
				/* pad zeroes */
				while  (padcnt-- > 0)  {
					ldst[(*smplth)++] = lastsmp;
					if  (*smplth == getlth)  {
						GcfFreeMem( blksmp );
						GcfFreeMem( blk );
						fprintf( stderr, 
							"   padding aborted, %ld zeroes remaining\n", padcnt );
						return;
					} /*endif*/
				} /*endwhile*/
			} /*endif*/
			expectime = blkend;

		} /*endif*/

		/* copy samples to output array */
		while  (actblksmp-- > 0)  {
			ldst[(*smplth)++] = *lsrc++;
			if  (*smplth == getlth)  {  /* finished ! */
				GcfFreeMem( blksmp );
				GcfFreeMem( blk );
				return;
			} /*endif*/
		} /*endwhile*/
		if  (*smplth > 0)
			lastsmp = ldst[(*smplth)-1];

	} /*endfor*/

} /* end of GcfReadStream */



/*---------------------------------------------------------------------*/



void GcfOrSeedReadStream( int chan, char gfdfile[], char stream_str[],
	TSyBoolean swap, char req_start[], float seclth, INT32 **ldat,
	INT32 *smplth, char act_start[], float *dt, float *calib,
	int *flags, TSyStatus *status )

/* Reads sample data from GCF or MiniSEED file.  Checks gfdfile for deciding
 * GCF or MiniSEED: if the the first character in gfdfile is 'g', GCF format
 * is read, otherwise MiniSEED format assumed. The memory for the sample
 * data is allocated by the routine.  The channel number is chosen
 * by the caller of the routine.  It is legal to use always channel
 * number 0 but it is faster to use a separate channel number for
 * each stream used if the streams are read repeatedly.
 *
 * parameters of routine
 * int        chan;         input; channel number (0..Gcf_C_MAXCHAN-1)
 * char       gfdfile[];    input; name of gfd file to use
 * char       stream_str[]; input; stream string (like "bfo-vbb-z")
 * TSyBoolean swap;         input; swap data in input
 * char       req_start[];  input; requested start time
 * float      seclth;       input; number of seconds to be read
 * INT32      **ldat;       output; sample array read
 * INT32      *smplth;      output; number of samples read
 * char       act_start[];  output; actual start time
 * float      *dt;          output; sample distance in sec
 * float      *calib;       output; calibration constant
 * TSyStatus  *status;      output; return status
 */
{
	/* local variables */
	FILE     *gfd;          /* pointer to gfdfile */
	char     fc;            /* first character */

	/* executable code */

	if  (strncmp(gfdfile,"DB:",3) == 0 || strncmp(gfdfile,"db:",3) == 0)  {
		/* db: works for seed only */
		fc = 's';
	} else {
		/* check first character in gfdfile */
		gfd = sy_fopen( gfdfile, "r" );
		if  (gfd == NULL)  {
			err_setcontext( " ## file " );
			err_setcontext( gfdfile );
			*status = GcfERR_OPENINPUT;
			return;
		} /*endif*/
		fc = (char)fgetc( gfd );
		sy_fclose( gfd );
	} /*endif*/

	if  (fc == 'g')  {

		GcfReadStream( chan, gfdfile, stream_str, swap, req_start,
			seclth, ldat, smplth, act_start, dt, calib, flags, status );

	} else {

		SeedReadStream( chan, gfdfile, stream_str, swap, req_start,
			seclth, ldat, smplth, act_start, dt, calib, flags, status );

	} /*endif*/

} /* end of GcfOrSeedReadStream */



/*---------------------------------------------------------------------*/



float GcfFindStreamCalibration( char stream[], char stime[], TSyStatus *status )

/* returns calibration value for counts for given stream 'stream' at time
 * 'stime'.
 *
 * parameters of routine
 * char           stream[];     input; stream string
 * char           stime[];      input; time
 * TSyStatus      *status;      output; return status
 *                              returns calibration or 0.0
 */
{
	/* local variables */
	char     calfile[cBcFileLth+1];    /* name of calibration file */
	char     *eptr;                    /* pointer to environment variable */
	FILE     *fp;                      /* pointer to input file */
	char     line[cBcLineLth+1];       /* current line of input file */
	char     v_start[cBcLineLth+1];    /* start of valid time */
	char     v_end[cBcLineLth+1];      /* end of valid time */
	float    lcalib;                   /* current value of calibration */

	/* executable code */

	/* build filename */
	*calfile = '\0';
	eptr = (char *)getenv( "SEED_INPUTS" );
	if  (eptr != NULL)  {
		if  (strlen(eptr)+strlen(stream)+11 > cBcFileLth)  {
			*status = GcfERR_STROVFL;
			return 0.0;
		} /*endif*/
		strcpy( calfile, eptr );
		strcat( calfile, "/" );
	} /*endif*/
	strcat( calfile, "seedcalib_" );
	strcat( calfile, stream );

	/* open file */
	fp = sy_fopen( calfile, "r" );
	if  (fp == NULL)  {
		fprintf( stderr, "--> no calibration for %s, set to 1.0\n", stream );
		*status = GcfERR_NOCALIB;
		return 1.0;
	} /*endif*/

	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		if  (sscanf(line,"%s %s %f",v_start,v_end,&lcalib) != 3)  {
			fprintf( stderr, "--> format error in calibration file %s\n", stream );
			continue;
		} /*endif*/
		if  (tc_tdiff(stime,v_start,status) < 0.0)  continue;
		if  (SySevere(status))  return 0.0;
		if  (tc_tdiff(stime,v_end,status) > 0.0)  continue;
		if  (SySevere(status))  return 0.0;
		sy_fclose( fp );
		return lcalib;
	} /*endwhile*/

	sy_fclose( fp );
	fprintf( stderr, "--> no valid calibration for %s %s, set to 1.0\n",
		stream, stime );
	*status = GcfERR_NOCALIB;
	return 1.0;

} /* end of GcfFindStreamCalibration */



/*---------------------------------------------------------------------*/



#define DO_ABORT \
	*status = GcfERR_GFD_READ; \
	err_setcontext( " ## string " ); err_setcontext( line-1 ); \
	return;




void GcfParseGfdLine( char line[], GcfFileDescrT *fd, TSyStatus *status )

/* parses line of GFD file.  Returns structure elements 'stream',
 * 't_start', 't_end', 'name', 'blkno' and 'calib'.
 *
 * parameters of routine
 * char       line[];       input; GFD line
 * GcfFileDescrT *fd;       output; GCF file descriptor
 * TSyStatus  *status;      output; return status
 */
{
	/* local variables */

	/* executable code */

	fd->t_start[0] = '\0';
	fd->t_end[0] = '\0';
	fd->name[0] = '\0';
	fd->stream[0] = '\0';
	fd->blkno = 0;
	fd->blklth = DEFAULT_BLKLTH;
	fd->calib = 0.0;
	fd->dataflags = 0;

	FOREVER  {

		if  (*line == '>')  line++;
		line = strchr( line, '>' );
		if  (line == NULL)  break;
		switch  (*(line-1))  {
		case Gcf_C_GfdTStart:
			if  (sscanf(line+1,"%s",fd->t_start) != 1)  { DO_ABORT }
			break;
		case Gcf_C_GfdTEnd:
			if  (sscanf(line+1,"%s",fd->t_end) != 1)  { DO_ABORT }
			break;
		case Gcf_C_GfdName:
			if  (sscanf(line+1,"%s",fd->name) != 1)  { DO_ABORT }
			break;
		case Gcf_C_GfdStream:
			if  (sscanf(line+1,"%s",fd->stream) != 1)  { DO_ABORT }
			break;
		case Gcf_C_GfdBlkno:
			if  (sscanf(line+1,"%d",&(fd->blkno)) != 1)  { DO_ABORT }
			break;
		case Gcf_C_GfdBlklth:
			if  (sscanf(line+1,"%d",&(fd->blklth)) != 1)  { DO_ABORT }
			break;
		case Gcf_C_GfdCalib:
			if  (sscanf(line+1,"%f",&(fd->calib)) != 1)  { DO_ABORT }
			break;
		} /*endswitch*/

	} /*endfor*/

} /* end of GcfParseGfdLine */



#undef DO_ABORT



/*----------------------------------------------------------------------------*/



GcfLongT GcfGet4Bytes( FILE *fp, TSyStatus *status )

/* Reads 4 bytes from file
 *
 * parameters of routine
 * FILE       *fp;     input; pointer to file
 * TSyStatus  *status; output; return status
 *                     returns 4 bytes read
 */
{
	/* local variables */
	unsigned char  b[4];             /* bytes read */
	GcfLongT  val;                   /* return value */

	/* executable code */

	if  (fread(b,1,4,fp) != 4)  {
		*status = GcfERR_READFILE;
		return 0;
	} /*endif*/

	val = (b[gcfv_byteorder[0]] << 24) + (b[gcfv_byteorder[1]] << 16)
		+ (b[gcfv_byteorder[2]] << 8) + b[gcfv_byteorder[3]];

	return val;

} /* end of GcfGet4Bytes */



/*----------------------------------------------------------------------------*/



GcfLongT GcfOrder4Bytes( unsigned char b[] )

/* Reorders 4 bytes read from stream to get longword
 *
 * parameters of routine
 * unsigned char b[];  input; 4 bytes read from file as char string
 *                     returns 4 bytes for longword
 */
{
	/* local variables */
	GcfLongT  val;                   /* return value */

	/* executable code */

	val = (b[gcfv_byteorder[0]] << 24) + (b[gcfv_byteorder[1]] << 16)
		+ (b[gcfv_byteorder[2]] << 8) + b[gcfv_byteorder[3]];

	return val;

} /* end of GcfOrder4Bytes */



/*----------------------------------------------------------------------------*/



void GcfDecodeTime( GcfTimeT gcftime, NTIME *ntime )

/* decodes GCF time
 *
 * parameters of routine
 * GcfTimeT   gcftime;      input; GCF encoded time
 * NTIME      *ntime;       output; numeric time
 */
{
	/* local variables */
	unsigned  wday;    /* word containing days */
	unsigned  wsec;    /* word containing seconds */
	int       daycount[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

	/* executable code */

	wsec = gcftime & 0x0001ffff;
	wday = (gcftime & 0xfffe0000) >> 17;

	/* date is referenced from 17-Nov-1989 */
	ntime->year = 1989;
	ntime->month = 10;
	ntime->day = 0;
	wday += 16;
	while  (wday >= daycount[ntime->month])  {
		wday -= daycount[ntime->month];
		ntime->year += (ntime->month + 1) / 12;
		ntime->month = (ntime->month + 1) % 12;
		if  (GcfIsLeapYear(ntime->year))  {
			daycount[1] = 29;
		} else {
			daycount[1] = 28;
		} /*endif*/
	} /*endwhile*/
	(ntime->month)++;
	ntime->day = wday+1;

	ntime->hour = wsec / 3600;
	ntime->min = (wsec % 3600) / 60;
	ntime->sec = wsec % 60;
	ntime->ms = 0;

} /* end of GcfDecodeTime */



/*----------------------------------------------------------------------------*/


void GcfBase36ToString( GcfLongT id, char str[] )

/* Converts base36 number to string.  Routine taken from Guralp manual.
 *
 * parameters of routine
 * unsigned INT32 id;       input; base36 number
 * char          str[];    output; decoded string
 */
{
	/* local variables */
	int      imed;    /* some counter */
	int      i;       /* another counter */

	/* executable code */

	str[6] = '\0';
	for  (i=5; i>=0; i--)  {
		imed = id % 36;
		if  (imed > 9)
			imed += 7;
		str[i] = imed + '0';
		id /= 36;
	} /*endfor*/

} /* end of GcfBase36ToString */



/*---------------------------------------------------------------------*/



void GcfAcceptCapfiles( TSyBoolean on_off )

/* switches on/off whether capitalized filenames on $ROOT-paths are accepted
 *
 * parameters of routine
 * TSyBoolean    on_off;       input; switch
 */
{
	/* executable code */

	gcfv_capfiles = on_off;

} /* end of GcfAcceptCapfiles */



/*----------------------------------------------------------------------------*/



static TSyBoolean GcfIsLeapYear( int year )

/* Returns whether year is leap year or not.
 *
 * parameters of routine
 * int        year;      input; year
 *                       returns TRUE if year is leap year
 */
{
	/* executable code */

	return ((year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0)));

} /* end of GcfIsLeapYear */



/*---------------------------------------------------------------------*/



static void GcfAdjustFilename( GcfFileDescrT *dsc, TSyStatus *status )

/* copies directory of gfd-file to data filename
 *
 * parameters of routine
 * GcfFileDescrT   *dsc;        modify; GCF file descriptor
 * TSyStatus       *status;     output; return status
 */
{
	/* local variables */
	int      i;                      /* counter */
	int      strlth;                 /* string length */
	char     path[cBcFileLth+1];     /* path name */
	BOOLEAN  iscolon;                /* is there a colon ? */
	char     upname[cBcFileLth+1];   /* uppercase filename */

	/* executable code */

	/* find path in gfdfile */
	i = strlen( dsc->gfdfile ) - 1;
	while  (i >= 0 && dsc->gfdfile[i] != '/' && dsc->gfdfile[i] != ':'
		&& dsc->gfdfile[i] != '\\')  i--;
	if  (i > 0)  {
		strncpy( path, dsc->gfdfile, i );
		path[i] = '\0';
		iscolon = (dsc->gfdfile[i] == ':');
	} else {
		*path = '\0';
		iscolon = FALSE;
	} /*endif*/

	if  (strlen(dsc->name)-ROOTDIRLTH+strlen(path) > cBcFileLth)  {
		*status = GcfERR_STROVFL;
		return;
	} /*endif*/

	if  (*path != '\0')  {
		strlth = strlen( path );
		for  (i=0; i<strlth; i++)
			if  (path[i] == '\\')  path[i] = '/';
		if  (iscolon)  {
			strcat( path, ":" );
			if  (gcfv_capfiles)  {
				strcpy( upname, dsc->name+ROOTDIRLTH+1 );
				ut_cap( upname );
				strcat( path, upname );
			} else {
				strcat( path, dsc->name+ROOTDIRLTH+1 );
			} /*endif*/
		} else {
			if  (gcfv_capfiles)  {
				strcpy( upname, dsc->name+ROOTDIRLTH );
				ut_cap( upname );
				strcat( path, upname );
			} else {
				strcat( path, dsc->name+ROOTDIRLTH );
			} /*endif*/
		} /*endif*/
	} else {
		strcpy( path, dsc->name+ROOTDIRLTH+1 );
	} /*endif*/
	strcpy( dsc->name, path );

} /* end of GcfAdjustFilename */



/*----------------------------------------------------------------------------*/

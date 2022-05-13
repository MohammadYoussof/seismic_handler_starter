
/* file seed_cd.c
 *      =========
 *
 * version 19, 27-Oct-2006
 *
 * CD routines
 * K. Stammler, 8-May-95
 */




#include <stdio.h>
#include <string.h>
#include "basecnst.h"
#ifdef BC_INC_STDLIB
#include BC_INC_STDLIB
#endif
#include "sysbase.h"
#include "utusrdef.h"
#include "tcusrdef.h"
#include "erusrdef.h"
#include "seedcfg.h"
#include "seed_lib.h"
#include "seed_cd.h"
#include "../stations.h"


/* global variables */

/* names of channels */
static char *vcd_channame[eCdChan_last+1] = {
	"undefined",
	"grf-bh",
	"grf-lh",
	"grf-mp",
	"grsn-bh",
	"grsn-lh",
	"grsn-hh",
	"geress-bh",
	"geress-lh",
	"geress-hh",
	"geress-sh",
	"gms",
	"yanqing",
	"georgia",
	"station-lookup/",
	"not-used"
};



/* prototypes of local routines */
static ECdChannel CdFindChannelID( char stream[], TSyStatus *status );



/*---------------------------------------------------------------------*/



void CdFindLabel( char stream[], char copytime[], char jkpath[], char label[],
	char magic[], TSyStatus *status )

/* returns label of CD with 'copytime' on it
 *
 * parameters of routine
 * char       stream[];      input; stream name
 * char       copytime[];    input; time to find
 * char       jkpath[];      output; jukebox root path
 * char       label[];       output; label of CD
 * char       magic[];       output; magic string to find in sfd-file
 * TSyStatus  *status;       output; return status
 */
{
	/* local variables */
	static char last_time[cBcTimeLth+1]="";         /* last time requested */
	static char last_label[cBcShortStrLth+1];       /* last label */
	static char last_magic[cBcLineLth+1]="";        /* last magic string */
	static char last_jkpath[cBcLineLth+1]="";       /* last jukebox path */
	static ECdChannel last_chan=eCdChan_undefined;  /* last channel ID */
	ECdChannel curr_chan;                 /* current channel */
	char     dirfile[cBcFileLth+1];       /* directory file */
	char     *env;                        /* pointer to environment */
	FILE     *look;                       /* directory file */
	char     line[cBcLongStrLth+1];       /* current line of file */
	char     time1[cBcLineLth+1];         /* start time */
	char     time2[cBcLineLth+1];         /* end time */
	char     curr_label[cBcLineLth+1];    /* current label */
	char     curr_magic[cBcLineLth+1];    /* current magic string */
	char     curr_jkpath[cBcLineLth+1];   /* current jukebox path */
	BOOLEAN  found;                       /* time/stream found in archive */
	char     sfdfile[cBcFileLth+1];       /* sfdfile to check */
	int      i;                           /* counter */
	char     ctmp;                        /* scratch */
	char     lostream[cBcLineLth+1];      /* lowercase stream name */

	/* executable code */

	/* check length of time string */
	if  (strlen(copytime) > cBcTimeLth)  {
		*status = sCdSTROVFL;
		return;
	} else if  (strlen(stream) > cBcLineLth)  {
		*status = sCdSTROVFL;
		return;
	} /*endif*/

	strcpy( lostream, stream );
	ut_uncap( lostream );

	/* find out and check current channel */
	curr_chan = CdFindChannelID( lostream, status );
	if  (SySevere(status))  return;
	if  (curr_chan == eCdChan_undefined)  {
		*status = sCdILLEGAL_CHAN;
		err_setcontext( " ## stream " );  err_setcontext( lostream );
		return;
	} /*endif*/

	/* if same as last time, return last label */
	if  (strcmp(copytime,last_time) == 0 && curr_chan == last_chan
		&& curr_chan != eCdChan_OTHER)  {
		strcpy( label, last_label );
		strcpy( magic, last_magic );
		strcpy( jkpath, last_jkpath );
		return;
	} /*endif*/

	/* find another label */
	strcpy( last_time, copytime );
	last_chan = curr_chan;

	/* open directory file */
	env = getenv( "DOUTPUT" );
	if  (env == NULL)  {
		*status = sCdMISSENV;
		err_setcontext( "## variable DOUTPUT" );
		return;
	} /*endif*/
	if  (strlen(env)+1+strlen(cCdDIRFILE_PREFIX)
		> cBcFileLth-8)  {
		*status = sCdSTROVFL;
		return;
	} /*endif*/
	if  (curr_chan == eCdChan_OTHER)  {
		sprintf( dirfile, "%s/%s/%s", env, vcd_channame[curr_chan], lostream );
		look = sy_fopen( dirfile, "r" );
		if  (look == NULL)  {
			/* remove component from stream (last two chars) */
			i = strlen( dirfile );
			i--;
			ctmp = dirfile[i];
			dirfile[i] = '\0';
			if  (ctmp != '-')  dirfile[--i] = '\0';
			look = sy_fopen( dirfile, "r" );
		} /*endif*/
		if  (look == NULL)  {
			/* remove channel from stream (last three chars) */
			i--;
			while  (i > 0 && dirfile[i] != '-')
				dirfile[i--] = '\0';
			if  (dirfile[i] == '-')  dirfile[i] = '\0';
			look = sy_fopen( dirfile, "r" );
		} /*endif*/
	} else {
		strcpy( dirfile, env );
		strcat( dirfile, "/" );
		strcat( dirfile, cCdDIRFILE_PREFIX );
		strcat( dirfile, vcd_channame[curr_chan] );
		look = sy_fopen( dirfile, "r" );
	} /*endif*/
	if  (look == NULL)  {
		*status = sCdOPENREAD;
		err_setcontext( " ## file " );
		err_setcontext( dirfile );
		return;
	} /*endif*/

	found = FALSE;
	while  (fgets(line,cBcLongStrLth,look) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		if  (sscanf( line, "%s %s %s %s %s", time1, time2, curr_label,
			curr_magic, curr_jkpath ) != 5)  {
			*status = SeedERR_READ_LOOKUP;
			err_setcontext( " ## file " );
			err_setcontext( dirfile );
			sy_fclose( look );
			return;
		} /*endif*/
		if  (tc_tdiff(copytime,time1,status) >= 0.0 &&
			tc_tdiff(time2,copytime,status) >= 0.0)  {
			/* this CD has the required time on it */
			found = TRUE;
			strcpy( label, curr_label );
			strcpy( magic, curr_magic );
			strcpy( jkpath, curr_jkpath );
			/* now check also next CD */
			if  (fgets(line,cBcLongStrLth,look) == NULL)  break;
			sscanf( line, "%s %s %s %s %s", time1, time2, curr_label,
				curr_magic, curr_jkpath );
			if  (tc_tdiff(copytime,time1,status) >= 0.0 &&
				tc_tdiff(time2,copytime,status) >= 0.0)  {
				/* this has it also, so take the more recent one */
				strcpy( label, curr_label );
				strcpy( magic, curr_magic );
				strcpy( jkpath, curr_jkpath );
			} /*endif*/
			break;
		} /*endif*/
	} /*endwhile*/

	sy_fclose( look );

	if  (!found)  {
		if  (tc_tdiff(copytime,time2,status) > 0.0)  {
			/* check in $SFD/sfdfile.sfd for existing data */
			*sfdfile = '\0';
			env = getenv( "SFD" );
			if  (env != NULL)  {
				strcpy( sfdfile, env );
				strcat( sfdfile, "/" );
			} /*endif*/
			strcat( sfdfile, "sfdfile.sfd" );
			found = SeedInquireTime( sfdfile, lostream, copytime, copytime,
				status );
			if  (SySevere(status))  return;
			if  (found)  {
				strcpy( label, "online" );
				*magic = '\0';
				*status = sCdONLINE;
			} else {
				*label = *magic = '\0';
				*status = sCdNOT_FOUND;
			} /*endif*/
		} else {
			*label = *magic = '\0';
			*status = sCdNOT_FOUND;
		} /*endif*/
		return;
	} /*endif*/

	strcpy( last_label, label );
	strcpy( last_magic, magic );
	strcpy( last_jkpath, jkpath );

} /* end of CdFindLabel */



/*---------------------------------------------------------------------*/



TSyBoolean CdDataLocked( char stream[], char ctime[], float seclth,
	TSyStatus *status )

/* Returns TRUE if data are locked (and also sets *status to sCdDATA_LOCKED).
 * The time window list of locked data windows is in $DOUTPUT/
 *
 * parameters of routine
 * char       stream[];      input; stream name
 * char       ctime[];       input; start time of copy window
 * float      seclth;        input; number of seconds to be copied
 * TSyStatus  *status;       output; return status
 *                           returns TRUE if data are locked
 */
{
	/* local variables */
	static char last_time[cBcTimeLth+1]="";         /* last time requested */
	static float last_length;                       /* last copy length */
	static TSyBoolean last_locked;                  /* result of last request */
	static ECdChannel last_chan=eCdChan_undefined;  /* last channel ID */
	ECdChannel curr_chan;                 /* current channel */
	char     *env;                        /* pointer to environment */
	char     lockfile[cBcFileLth+1];      /* locked window list */
	FILE     *lock;                       /* pointer to above file */
	char     line[cBcLongStrLth+1];       /* current line of file */
	char     time1[cBcLineLth+1];         /* start time of lock window */
	char     time2[cBcLineLth+1];         /* end time of lock window */
	char     etime[cBcLineLth+1];         /* end time of copy window */
	TSyBoolean found;                     /* lock window found */
	char     *sptr;                       /* pointer to start of stream string */
	char     lstream[cBcLineLth+1];       /* local stream variable */

	/* executable code */

	/* check length of time string */
	if  (strlen(ctime) > cBcTimeLth)  {
		*status = sCdSTROVFL;
		return FALSE;
	} /*endif*/

	tc_tadd( ctime, seclth, etime, status );
	if  (SySevere(status))  return FALSE;

	/* find out and check current channel */
	curr_chan = CdFindChannelID( stream, status );
	if  (SySevere(status))  return FALSE;
	if  (curr_chan == eCdChan_undefined)  {
		*status = sCdILLEGAL_CHAN;
		err_setcontext( " ## stream " );  err_setcontext( stream );
		return FALSE;
	} /*endif*/

	/* not very sophisticated !!! */
	if  (curr_chan == eCdChan_YANQING)  return FALSE;
	if  (curr_chan == eCdChan_OTHER)  return FALSE;

	/* if same as last time, return last result */
	if  (strcmp(ctime,last_time) == 0 && seclth == last_length 
		&& curr_chan == last_chan)  {
		if  (last_locked)  *status = sCdDATA_LOCKED;
		return last_locked;
	} /*endif*/

	/* open directory file */
	env = getenv( "DOUTPUT" );
	if  (env == NULL)  {
		*status = sCdMISSENV;
		err_setcontext( "## variable DOUTPUT" );
		return FALSE;
	} /*endif*/
	if  (strlen(env)+1+strlen(cCdLOCKFILE_PREFIX)
		> cBcFileLth-8)  {
		*status = sCdSTROVFL;
		return FALSE;
	} /*endif*/
	strcpy( lockfile, env );
	strcat( lockfile, "/" );
	strcat( lockfile, cCdLOCKFILE_PREFIX );
	strcat( lockfile, vcd_channame[curr_chan] );
	lock = sy_fopen( lockfile, "r" );
	if  (lock == NULL)  {
		*status = sCdOPENREAD;
		err_setcontext( " ## file " );
		err_setcontext( lockfile );
		return FALSE;
	} /*endif*/

	/* found = FALSE; */
	while  (fgets(line,cBcLongStrLth,lock) != NULL)  {
		if  (*line == '!' || *line == '\n')  continue;
		if  (sscanf( line, "%s %s", time1, time2) != 2)  {
			*status = SeedERR_READ_LOOKUP;
			err_setcontext( " ## file " );
			err_setcontext( lockfile );
			return FALSE;
		} /*endif*/
		found = ( tc_tdiff(ctime,time1,status) >= 0.0
			&& tc_tdiff(time2,ctime,status) >= 0.0);
		found = found || ( tc_tdiff(etime,time1,status) >= 0.0
			&& tc_tdiff(time2,etime,status) >= 0.0);
		if  (found)  {
			sptr = strchr( line, '>' );
			if  (sptr != NULL)  {
				sptr++;
				while  (*sptr == ' ')  sptr++;
				if  (*sptr != '*')  {
					if  (strlen(stream) > cBcLineLth)  {
						*status = sCdSTROVFL;
						sy_fclose( lock );
						return TRUE;
					} /*endif*/
					strcpy( lstream, stream );
					ut_uncap( lstream );
					if  (strstr(sptr,lstream) == NULL)  {
						/* time window not valid for this stream */
						sy_fclose( lock );
						return FALSE;
					} /*endif*/
				} /*endif*/
			} /*endif*/
			*status = sCdDATA_LOCKED;
			err_setcontext( " ## stream " );  err_setcontext( stream );
			sy_fclose( lock );
			return TRUE;
		} /*endif*/
	} /*endwhile*/

	sy_fclose( lock );
	return FALSE;

} /* end of CdDataLocked */



/*---------------------------------------------------------------------*/



static ECdChannel CdFindChannelID( char stream[], TSyStatus *status )

/* returns channel ID of given stream name
 *
 * parameters of routine
 * char       stream[];     input; stream name
 * TSyStatus  *status;      output; return status
 *                          returns channel ID or eCdChan_undefined
 */
{
	/* local variables */
	char     str[cBcLineLth+1];      /* copy of stream string */
	char     station[cBcLineLth+1];  /* station name */
	char     channel[cBcLineLth+1];  /* channel name */
	TSyBoolean found;                /* station found in list */
	int      i;                      /* counter */

	/* executable code */

	if  (strlen(stream) > cBcLineLth)  {
		*status = sCdSTROVFL;
		return eCdChan_undefined;
	} /*endif*/

	strcpy( str, stream );
	SeedPrepareStreamString( str );
	sscanf( str, "%s %s", station, channel );

	if  (strncmp(station,"gr",2) == 0 && station[2] >= 'a'
		&& station[2] <= 'c')  {
		if  (strcmp(station,"gra5") == 0)  return eCdChan_OTHER;
		if  (station[3] < '1' || station[3] > '5')  return eCdChan_OTHER;
		if  (strcmp(channel,"bh") == 0)  return eCdChan_GRF_BH;
		if  (strcmp(channel,"lh") == 0)  return eCdChan_GRF_LH;
		if  (strcmp(channel,"mp") == 0)  return eCdChan_GRF_MP;
		return eCdChan_undefined;
	} else if  (strncmp(station,"ge",2) == 0 && station[2] >= 'a'
		&& station[2] <= 'd')  {
		if  (strcmp(channel,"bh") == 0)  return eCdChan_GERESS_BH;
		if  (strcmp(channel,"lh") == 0)  return eCdChan_GERESS_LH;
		if  (strcmp(channel,"hh") == 0)  return eCdChan_GERESS_HH;
		if  (strcmp(channel,"sh") == 0)  return eCdChan_GERESS_SH;
		return eCdChan_undefined;
	} else {
		found = FALSE;
		i = 0;
		while  (stv_grsn_all[i][0] != '\0')  {
			if  (!found && strcmp(station,stv_grsn_all[i]) == 0)  found = TRUE;
			i++;
		} /*endwhile*/
		if  (found)  {
			if  (strcmp(channel,"bh") == 0)  return eCdChan_GRSN_BH;
			if  (strcmp(channel,"lh") == 0)  return eCdChan_GRSN_LH;
			if  (strcmp(channel,"hh") == 0)  return eCdChan_GRSN_HH;
			return eCdChan_undefined;
		} /*endif*/
		found = FALSE;
		i = 0;
		while  (stv_gms[i][0] != '\0')  {
			if  (!found && strcmp(station,stv_gms[i]) == 0)  found = TRUE;
			i++;
		} /*endwhile*/
		if  (found)  return eCdChan_GMS;
		i = 0;
		while  (stv_yanqing[i][0] != '\0')  {
			if  (!found && strcmp(station,stv_yanqing[i]) == 0)  found = TRUE;
			i++;
		} /*endwhile*/
		if  (found)  return eCdChan_YANQING;
		i = 0;
		while  (stv_georgia[i][0] != '\0')  {
			if  (!found && strcmp(station,stv_georgia[i]) == 0)  found = TRUE;
			i++;
		} /*endwhile*/
		if  (found)  return eCdChan_GEORGIA;
		return eCdChan_OTHER;
	} /*endif*/

	/* return eCdChan_undefined; */

} /* end of CdFindChannelID */



/*---------------------------------------------------------------------*/

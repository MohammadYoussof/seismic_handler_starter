
/* file seed_cd.h
 *      =========
 *
 * version 11, 17-Jan-2006
 *
 * header file of module seed_cd.c
 * K. Stammler, 8-May-95
 */



/* error codes */
#define sCdOFFSET        9300
#define sCdMISSENV       (sCdOFFSET+1)     /* missing environment variable */
#define sCdSTROVFL       (sCdOFFSET+2)     /* string overflow */
#define sCdOPENREAD      (sCdOFFSET+3)     /* error opening input file */
#define sCdILLEGAL_CHAN  (sCdOFFSET+4)     /* illegal channel */
#define sCdNOT_FOUND     (sCdOFFSET+5)     /* stream/time not found in archive*/
#define sCdONLINE        (sCdOFFSET+6)     /* data are online */
#define sCdDATA_LOCKED   (sCdOFFSET+7)     /* data are locked */



/* types */
typedef enum {
	eCdChan_undefined,
	eCdChan_GRF_BH,
	eCdChan_GRF_LH,
	eCdChan_GRF_MP,
	eCdChan_GRSN_BH,
	eCdChan_GRSN_LH,
	eCdChan_GRSN_HH,
	eCdChan_GERESS_BH,
	eCdChan_GERESS_LH,
	eCdChan_GERESS_HH,
	eCdChan_GERESS_SH,
	eCdChan_GMS,
	eCdChan_YANQING,
	eCdChan_GEORGIA,
	eCdChan_OTHER,
	eCdChan_last
} ECdChannel;



/* constants */
#define cCdDIRFILE_PREFIX "cd_lookup_"
#define cCdLOCKFILE_PREFIX "cd_lock_"





/*---------------------------------------------------------------------*/


void CdFindLabel( char stream[], char copytime[], char jkpath[], char label[],
	char magic[], TSyStatus *status );

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


/*---------------------------------------------------------------------*/


TSyBoolean CdDataLocked( char stream[], char ctime[], float seclth,
	TSyStatus *status );

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


/*---------------------------------------------------------------------*/

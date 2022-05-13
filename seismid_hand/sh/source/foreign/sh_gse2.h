
/* file sh_gse2.h
 *      =========
 *
 * version 1, 9-Dec-94
 *
 * header file of module sh_gse2.c
 * K. Stammler, 9-Dec-94
 */




/* error codes */
#define eGs2Offset         9800
#define eGs2StrOvfl        (eGs2Offset+1)      /* string overflow */




/* prototypes */
void ShGs2Trace( char file[], int trcno, TSyStatus *status );
void ShGs2GetI( char entryname[], long *info, TSyStatus *status );
void ShGs2GetR( char entryname[], float *info, TSyStatus *status );
void ShGs2GetS( char entryname[], int maxlth, char info[], TSyStatus *status );
void ShGs2GetC( char entryname[], char *info, TSyStatus *status );
void ShGs2Read( float smp[] );

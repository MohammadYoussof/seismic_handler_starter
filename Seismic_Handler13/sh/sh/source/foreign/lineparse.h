
/* file lineparse.h
 *      ===========
 *
 * version 1, 26-Nov-94
 *
 * header file of module lineparse.c
 * K. Stammler, 26-Nov-94
 */



/* error codes */
#define eLpOffset         9400
#define eLpLineTooShort   (eLpOffset+1)    /* line too short */
#define eLpParseInt       (eLpOffset+2)    /* error reading int */
#define eLpStrOvfl        (eLpOffset+3)    /* string overflow */
#define eLpParseFloat     (eLpOffset+4)    /* error reading float */
#define eLpIllegalType    (eLpOffset+5)    /* illegal info type */
#define eLpPrintInt       (eLpOffset+6)    /* error writing integer */
#define eLpPrintFloat     (eLpOffset+7)    /* error writing float */
#define eLpPrintString    (eLpOffset+8)    /* error writing string */




/* constants */

#define cLpEmptyInt 0
	/* empty int return value */
#define cLpEmptyFloat 0.0
	/* empty float return value */
#define cLpEmptyChar ' '
	/* empty char return value */
#define cLpFmtStrLth 9
	/* max length of format string */



/* types */

/* types of entries */
typedef enum {
	nLpTypeInt, nLpTypeFloat, nLpTypeString, nLpTypeChar
} NLpInfo;

typedef struct {
	int       str_pos;        /* position of info in text string */
	int       str_lth;        /* length of info in text string */
	NLpInfo   inf_type;       /* type of info */
	int       inf_pos;        /* byte position of info in structure */
	char      outfmt[cLpFmtStrLth+1]; /* format string for float output */
} SLpEntry;



int LpGetInt( char line[], int pos, int lth, STATUS *status );
float LpGetFloat( char line[], int pos, int lth, STATUS *status );
void LpGetString( char line[], int pos, int lth, char res[], STATUS *status );
char LpGetChar( char line[], int pos, STATUS *status );
void LpParseLine( char line[], SLpEntry entry[], int numentries,
	void *info, STATUS *status );
void LpPutLine( void *info, SLpEntry entry[], int numentries,
	char line[], STATUS *status );

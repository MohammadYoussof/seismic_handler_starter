
/* file lineparse.c
 *      ===========
 *
 * version 2, 9-Dec-94
 *
 * event line parser
 * K. Stammler, 26-Nov-94
 */



#include <stdio.h>
#include <string.h>
#include BASECNST
#include BC_SYSBASE
#include "lineparse.h"




/*------------------------------------------------------------------*/



int LpGetInt( char line[], int pos, int lth, STATUS *status )

/* Reads int value from text line at position pos
 *
 * parameters of routine
 * char       line[];         input; text line
 * int        pos;            input; position of number
 * int        lth;            input; max. length of number
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	char     str[cBcLineLth]; /* scratch string */
	int      number;          /* value read from line */
	char     *ptr;            /* moving pointer */

	/* executable code */

	if  (lth > cBcLineLth)  {
		*status = eLpStrOvfl;
		return cLpEmptyInt;
	} else if  (strlen(line) < pos)  {
		/* *status = eLpLineTooShort; */
		return cLpEmptyInt;
	} /*endif*/

	strncpy( str, line+pos, lth );
	str[lth] = '\0';

	if  (sscanf(str,"%d",&number) != 1)  {
		ptr = str-1;
		while  (*(++ptr) != '\0')
			if  (*ptr > ' ')
				*status = eLpParseInt;
		return cLpEmptyInt;
	} /*endif*/

	return number;

} /* end of LpGetInt */



/*------------------------------------------------------------------*/



float LpGetFloat( char line[], int pos, int lth, STATUS *status )

/* Reads float value from text line at position pos
 *
 * parameters of routine
 * char       line[];         input; text line
 * int        pos;            input; position of number
 * int        lth;            input; max. length of number
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	char     str[cBcLineLth]; /* scratch string */
	float    number;          /* value read from line */
	char     *ptr;            /* moving pointer */

	/* executable code */

	if  (lth > cBcLineLth)  {
		*status = eLpStrOvfl;
		return cLpEmptyFloat;
	} else if  (strlen(line) < pos)  {
		/* *status = eLpLineTooShort; */
		return cLpEmptyFloat;
	} /*endif*/

	strncpy( str, line+pos, lth );
	str[lth] = '\0';

	if  (sscanf(str,"%f",&number) != 1)  {
		ptr = str-1;
		while  (*(++ptr) != '\0')
			if  (*ptr > ' ')
				*status = eLpParseFloat;
		return cLpEmptyFloat;
	} /*endif*/

	return number;

} /* end of LpGetFloat */


/*------------------------------------------------------------------*/



void LpGetString( char line[], int pos, int lth, char res[], STATUS *status )

/* Reads float value from text line at position pos
 *
 * parameters of routine
 * char       line[];         input; text line
 * int        pos;            input; position of number
 * int        lth;            input; max. length of number
 * char       res[];          output; result string
 * STATUS     *status;        output; return status
 */
{
	/* local variables */
	int      i;        /* counter */

	/* executable code */

	if  (strlen(line) < pos)  {
		/* *status = eLpLineTooShort; */
		*res = '\0';
		return;
	} /*endif*/

	strncpy( res, line+pos, lth );
	res[lth] = '\0';

	/* remove trailing blanks */
	i = strlen( res ) - 1;
	while  (i >= 0 && res[i] == ' ')  {
		res[i] = '\0';
		i--;
	} /*endwhile*/

} /* end of LpGetString */


/*------------------------------------------------------------------*/



char LpGetChar( char line[], int pos, STATUS *status )

/* Reads int value from text line at position pos
 *
 * parameters of routine
 * char       line[];         input; text line
 * int        pos;            input; position of number
 * int        lth;            input; max. length of number
 * STATUS     *status;        output; return status
 */
{
	/* local variables */

	/* executable code */

	if  (strlen(line) < pos)  {
		/* *status = eLpLineTooShort; */
		return cLpEmptyChar;
	} /*endif*/

	return line[pos];

} /* end of LpGetChar */



/*------------------------------------------------------------------*/



void LpParseLine( char line[], SLpEntry entry[], int numentries,
	void *info, STATUS *status )

/* Reads information from line
 *
 * parameters of routine
 * char       line[];        input; line to be parsed
 * SLpEntry   entry[];       input; entry information
 * int        numentries;    input; number of entries
 * void       *info;         output; info structure
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	char     *cinfo;             /* byte pointer to info */
	int      i;                  /* counter */

	/* executable code */

	for  (i=0; i<numentries; i++)  {
		cinfo = ((char *)info) + entry[i].inf_pos;
		switch  (entry[i].inf_type)  {
		case nLpTypeInt:
			*(int *)cinfo = LpGetInt( line, entry[i].str_pos,
				entry[i].str_lth, status );
			if  (Severe(status))  return;
			break;
		case nLpTypeFloat:
			*(float *)cinfo = LpGetFloat( line, entry[i].str_pos,
				entry[i].str_lth, status );
			if  (Severe(status))  return;
			break;
		case nLpTypeString:
			LpGetString( line, entry[i].str_pos, entry[i].str_lth,
				cinfo, status );
			if  (Severe(status))  return;
			break;
		case nLpTypeChar:
			*cinfo = LpGetChar( line, entry[i].str_pos, status );
			if  (Severe(status))  return;
			break;
		default:
			*status = eLpIllegalType;
			return;
		} /*endswitch*/
	} /*endfor*/

} /* end of LpParseLine */



/*------------------------------------------------------------------*/



void LpPutLine( void *info, SLpEntry entry[], int numentries,
	char line[], STATUS *status )

/* Prints line with infos
 *
 * parameters of routine
 * void       *info;         input; info structure
 * SLpEntry   entry[];       input; entry list
 * int        numentries;    input; number of entries
 * char       line[];        output; output line with information
 * STATUS     *status;       output; return status
 */
{
	/* local variables */
	int      i, j;                /* counters */
	int      maxpos;              /* mainum position in line */
	char     str[cBcLineLth+1];   /* scratch string */
	char     fmt[cBcLineLth+1];   /* format string */
	char     *cinfo;              /* info pointer */

	/* executable code */

	maxpos = 0;
	for  (i=0; i<numentries; i++)
		if  (entry[i].str_pos > maxpos)  {
			maxpos = entry[i].str_pos;
			j = i;
		} /*endif*/
	maxpos += entry[j].str_lth;
	for  (i=0; i<maxpos; i++)
		line[i] = ' ';
	line[maxpos] = '\0';

	for  (i=0; i<numentries; i++)  {
		cinfo = ((char *)info) + entry[i].inf_pos;
		switch  (entry[i].inf_type)  {
		case nLpTypeInt:
			sprintf( fmt, "%%%dd", entry[i].str_lth );
			sprintf( str, fmt, *(int *)cinfo );
			if  (strlen(str) > entry[i].str_lth)  {
				for  (j=0; j<entry[i].str_lth; j++)  str[j] = '*';
				str[entry[i].str_lth] = '\0';
			} /*endif*/
			strncpy( line+entry[i].str_pos, str, entry[i].str_lth );
			break;
		case nLpTypeFloat:
			if  (entry[i].outfmt[0] == '\0')  {
				sprintf( fmt, "%%%dg", entry[i].str_lth );
			} else {
				strcpy( fmt, entry[i].outfmt );
			} /*endif*/
			sprintf( str, fmt, *(float *)cinfo );
			if  (strlen(str) > entry[i].str_lth)  {
				sprintf( fmt, "%%%df", entry[i].str_lth );
				sprintf( str, fmt, *(float *)cinfo );
				if  (strlen(str) > entry[i].str_lth)  {
					for  (j=0; j<entry[i].str_lth; j++)  str[j] = '*';
					str[entry[i].str_lth] = '\0';
				} /*endif*/
			} /*endif*/
			strncpy( line+entry[i].str_pos, str, entry[i].str_lth );
			break;
		case nLpTypeString:
			sprintf( fmt, "%%%ds", entry[i].str_lth );
			sprintf( str, fmt, cinfo );
			if  (strlen(str) > entry[i].str_lth)  {
				for  (j=0; j<entry[i].str_lth; j++)  str[j] = '*';
				str[entry[i].str_lth] = '\0';
			} /*endif*/
			strncpy( line+entry[i].str_pos, str, entry[i].str_lth );
			break;
		case nLpTypeChar:
			line[entry[i].str_pos] = *cinfo;
			break;
		default:
			*status = eLpIllegalType;
			return;
		} /*endswitch*/
	} /*endfor*/

} /* end of LpPutLine */



/*------------------------------------------------------------------*/

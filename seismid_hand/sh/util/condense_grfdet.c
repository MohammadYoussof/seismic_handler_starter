
/* file condense_grfdet.c
 *      =================
 *
 * version 1, 6-Nov-2006
 *
 * take average over neighbored detections
 * K. Stammler, 6-Nov-2006
 */

#include <stdio.h>
#include <strings.h>
#include "basecnst.h"
#include "sysbase.h"
#include "tcusrdef.h"


int main( int argc, char *argv[] )
{
	/* local variables */
	char     fname[cBcFileLth+1];       /* name of detection file */
	float    maxtdiff;                  /* max. difference time between detections */
	char     line[cBcLineLth+1];        /* current line of file */
	char     dtime[cBcLineLth+1];       /* detection time */
	float    qual, slo, baz, relen, absen;  /* data entries */
	char     ltime[cBcLineLth+1];       /* last detection time */
	char     stime[cBcLineLth+1];       /* start detection time */
	float    aqual, aslo, abaz, arelen, aabsen;  /* data entries */
	float    tdiff;                     /* time difference */
	TSyStatus status;                   /* return status */
	FILE     *fp;                       /* pointer to file */
	int      avcnt;                     /* average counter */

	/* executable code */

	if  (argc != 3)  {
		fprintf( stderr, "Usage: %s <filename> <timedist>\n", argv[0]);
		return 1;
	} /*endif*/

	/* get parameters */
	strcpy( fname, argv[1] );
	if  (sscanf(argv[2],"%f",&maxtdiff) != 1)  {
		fprintf( stderr, "%s: error reading time diff\n", argv[0] );
		return 1;
	} /*endif*/

	if  (strcmp(fname,"-") == 0)  {
		fp = stdin;
	} else {
		fp = fopen( fname, "r" );
		if  (fp == NULL)  {
			fprintf( stderr, "%s: eror opening input file %s\n", argv[0], fname );
			return 1;
		} /*endif*/
	} /*endif*/

	status = cBcNoError;
	aqual = aslo = abaz = arelen = aabsen = 0.0;
	ltime[0] = stime[0] = '\0';
	avcnt = 0;
	while  (fgets(line,cBcLineLth,fp) != NULL)  {
		if  (strlen(line) < 50)  continue;  /* shouldn't happen */
		if  (sscanf(line+4,"%s %f %f %f %f %f",dtime,&qual,&slo,&baz,&relen,&absen) != 6)  {
			fprintf( stderr, "%s: error parsing line.  Abort.\n", argv[0] );
			return 1;
		} /*endif*/
		if  (*ltime == '\0')  {
			aslo = slo;
			abaz = baz;
			arelen = relen;
			aabsen = absen;
			avcnt = 1;
			strcpy( ltime, dtime );
			continue;
		} /*endif*/
		tdiff = tc_tdiff( dtime, ltime, &status );
		if  (SySevere(&status))  {
			fprintf( stderr, "%s: error converting time.  Abort\n", argv[0] );
			return 1;
		} /*endif*/
		if  (tdiff > maxtdiff)  {
			aslo /= (float)avcnt;
			abaz /= (float)avcnt;
			arelen /= (float)avcnt;
			aabsen /= (float)avcnt;
			printf( "%s %f %f %f %f (%d)\n", stime, aslo, abaz, arelen, aabsen, avcnt );
			aslo = slo;
			abaz = baz;
			arelen = relen;
			aabsen = absen;
			strcpy( stime, dtime );
			avcnt = 1;
		} else {
			aslo += slo;
			abaz += baz;
			arelen += relen;
			aabsen += absen;
			avcnt++;
		} /*endif*/
		strcpy( ltime, dtime );
	} /*endwhile*/

	aslo /= (float)avcnt;
	abaz /= (float)avcnt;
	arelen /= (float)avcnt;
	aabsen /= (float)avcnt;
	printf( "%s %f %f %f %f (%d)\n", stime, aslo, abaz, arelen, aabsen, avcnt );

	if  (fp != stdin)  fclose( fp );

	return 0;

} /* end of main */

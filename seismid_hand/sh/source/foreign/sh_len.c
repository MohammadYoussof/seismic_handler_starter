/* file sh_len.c
 *      ========
 *
 * version 4, 07-Jun-95
 *
 * ESSTF Lennartz binary reader for SH (foreign format input)
 *
 * J. Zednik + M.Musil, 15-Jun-93
 * changed 16-Mar-94, added 1900(+100) to nstart.year
 */



#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include BASECNST
#include BC_SYSBASE
#include BC_TCUSRDEF
#include "../utusrdef.h"
#include "sh_len.h"



/* constants */

#define LEN_MAXTRC 15
	/* maximum number of traces in one ESSTF file */
#define LEN_NAMELTH 10
	/* maximum length of station name */
#define ERRBIT 0x40

/* global variables */
static char     gsv_gsefile[BC_FILELTH+1]; /* ESSTF file */
static int      gsv_currtrc;               /* current trace in process */
static int      gsv_traceno;               /* number of traces */
static float    *gsv_dat[LEN_MAXTRC];      /* pointer to sample data */
static long     gsv_length[LEN_MAXTRC];    /* length of traces */
static char     gsv_station[LEN_MAXTRC][LEN_NAMELTH+1];  /* station names */
static char     gsv_comp[LEN_MAXTRC];      /* components */
static float    gsv_dt[LEN_MAXTRC];        /* sample rates */
static TIME     gsv_start[LEN_MAXTRC];     /* start time of traces */


/*----------------------------------------------------------------------------*/



void sh_len_trace( char file[], int rec, STATUS *status )

/* prepares ESSTF file for passing to SH
 *
 * parameters of routine
 * char       file[];    input; name of ESSTF file
 * int        rec;       input; number of record to read
 * STATUS     *status;   output; return status
 */
{
	/* local variables */
	FILE     *fp;                  /* pointer to ESSTF file */
	int      t;                    /* counter */
	int      ipol, a, unts;
	long     length, l, ll, testl;
	float    gain, genco, convert[LEN_MAXTRC], subtime;
	char     temp[20], buffer[2048];
	char     *pointer;
	NTIME    nstart;

	/* executable code */
/*
 * read header info
 */
	if(isalpha(file[0]))
	    sprintf(buffer,"%cHEADER.DAT", file[0]);
	else
	{
	    for(t=0; t<strlen(file); t++)
		if(file[t] == '.')
		    break;
	    if(t >= strlen(file)) {
		*status = LEN_OPENLEN;
		return;
	    }
	    sprintf(buffer,"%cHEADER.DAT", file[t+1]);
	}

	fp=sy_fopen( buffer, "rb" );
	if (fp == NULL)  {
	    *status = LEN_OPENLEN;
	    return;
	}
	fscanf( fp,"%d", &gsv_traceno);
	sy_fclose ( fp );

	if(isalpha(file[0]))
	    sprintf(buffer,"%cSTATIO.DAT", file[0]);
	else
	    sprintf(buffer,"%cSTATIO.DAT", file[t+1]);

	fp=sy_fopen( buffer, "rb" );
	if (fp == NULL)  {
	    *status = LEN_OPENLEN;
	    return;
	}
	for(t=0; t<gsv_traceno; t++)
	{
	    fread(buffer,1,2048,fp);
	    strncpy(gsv_station[t],&buffer[4],4);
	    gsv_comp[t]=buffer[55];
	    ipol=buffer[56]-48;
	    if(ipol != 1)
		ipol=-1;
	    strncpy(temp,&buffer[81],10);
	    temp[10]='\0';
	    gsv_dt[t]= atof(temp)/1000000.;
	    strncpy(temp,&buffer[71],10);
	    temp[10]='\0';
	    gain=atof(temp);
	    strncpy(temp,&buffer[91],10);
	    temp[10]='\0';
	    genco=atof(temp);
	    convert[t]=1.e-6 *ipol *gain /genco;
	}
	sy_fclose ( fp );
/*
 * end of reading header info
 */
	/* check if already read in */
	gsv_currtrc = rec-1;
	if  (strcmp(file,gsv_gsefile) == 0)
	{
		if  (gsv_currtrc >= gsv_traceno || gsv_currtrc < 0)
		{
			*status = LEN_ILLTRC;
			return;
		} /*endif*/
		if  (gsv_length[gsv_currtrc] == 0)
		{
			*status = LEN_READTWICE;
			return;
		} /*endif*/
		return;
	} /*endif*/

	/* store new file name */
	if  (strlen(file) > BC_FILELTH)  {
		*status = LEN_NAMEOVFL;
		return;
	} /*endif*/
	strcpy( gsv_gsefile, file );
/*
 * open ESSTF file
 */
	fp = sy_fopen( file, "rb" );
	if  (fp == NULL)  {
		*status = LEN_OPENLEN;
		return;
	} /*endif*/
	fseek(fp,0L,2);
	l=ftell(fp);
	length=(l-2048)/2048/gsv_traceno;
	fseek(fp,0L,0);
	for  (t=0; t<gsv_traceno; t++)
	{
		if  (gsv_length[t] != 0)
			sy_deallocmem( gsv_dat[t] );
		gsv_length[t] = length*500;
/* 
 * allocate memory 
 */
                gsv_dat[t]=(float *)sy_allocmem( gsv_length[t],
                           (int)sizeof(float), status);
                if(Severe(status))
                {
                    gsv_length[t] = 0;
                    gsv_traceno=t;
                    break; 
                }              
	} /*endfor*/
/* 
 * read traces 
 */
        fread(buffer,1,2048,fp);
        buffer[48]='\0';
        for(l=0,ll=0; l<length; l++,ll+=500)
        {
	    for  (t=0; t<gsv_traceno; t++, ll-=500)
	    {
                fread(buffer,1,2048,fp);
                if((l==0)&&(t==0)) /* time of first sample */
                {
                    strncpy(temp,&buffer[3],2);
                    temp[2]='\0';
                    nstart.year  = atoi(temp);
                    if  (nstart.year < 1900)  nstart.year += 1900;
                    if  (nstart.year < 1970)  nstart.year += 100;
                    strncpy(temp,&buffer[5],2);
                    temp[2]='\0';
                    nstart.month = atoi(temp);    
                    strncpy(temp,&buffer[7],2);
                    temp[2]='\0';
                    nstart.day   = atoi(temp);    
                    strncpy(temp,&buffer[9],2);
                    temp[2]='\0';
                    nstart.hour  = atoi(temp);    
                    strncpy(temp,&buffer[11],2);
                    temp[2]='\0';
                    nstart.min   = atoi(temp);    
                    strncpy(temp,&buffer[13],2);
                    temp[2]='\0';
                    nstart.sec   = atoi(temp); 
                    strncpy(temp,&buffer[15],4);
                    temp[4]='\0';
                    unts         = atoi(temp)-1;
                    subtime=gsv_dt[t]*unts;
                    unts=subtime+1;
                    if((nstart.sec -subtime)>=0)
                    {
                        nstart.sec-=subtime;
                        nstart.ms=(unts-subtime) * 1000;
                    }
                    else if(nstart.min > 0)
                    {
                        nstart.min--;
                        nstart.sec+=60;
                        nstart.sec-=subtime;
                        nstart.ms=(unts-subtime) * 1000;
                    }
                    else if(nstart.hour > 0)
                    {
                        nstart.hour--;
                        nstart.min=59;
                        nstart.sec+=60;
                        nstart.sec-=subtime;
                        nstart.ms=(unts-subtime) * 1000;
                    }
                    else
                    {
                        nstart.day--;
                        nstart.hour=23;
                        nstart.min=59;
                        nstart.sec+=60;
                        nstart.sec-=subtime;
                        nstart.ms=(unts-subtime) * 1000;
                    }
                }
                tc_n2a( &nstart, gsv_start + t, status);
	        for(a=48,pointer = &buffer[48]; a<2048; a+=4, ll++)
	        {
	            testl=(*(pointer++)<<8);
	            testl=testl+(*(pointer++)&0xff);
	            testl=testl<<(*(pointer++));
	            gsv_dat[t][ll]=convert[t] *(float)testl;
	            if((*(pointer++) & ERRBIT) == ERRBIT)
	                if(ll > 0)
	                    gsv_dat[t][ll] = gsv_dat[t][ll-1];
	                else
	                    gsv_dat[t][ll] = 0.0;
 	        } 
	    } 
	}    
	sy_fclose( fp );

	if  (gsv_currtrc >= gsv_traceno || gsv_currtrc < 0)  {
		*status = LEN_ILLTRC;
		return;
	} /*endif*/

} /* end of sh_len_trace */



/*----------------------------------------------------------------------------*/



void sh_len_geti( char entryname[], long *info, STATUS *status )

/* returns integer value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * long       *info;           output; returned info
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	if  (gsv_length[gsv_currtrc] == 0)  {
		*status = LEN_READTWICE;
		return;
	} /*endif*/

	if  (strcmp(entryname,"LENGTH") == 0)  {
		*info = gsv_length[gsv_currtrc];
	} else {
		printf( "*** ESSTF: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_len_geti */



/*----------------------------------------------------------------------------*/



void sh_len_getr( char entryname[], float *info, STATUS *status )

/* returns float value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * float      *info;           output; returned info
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	if  (gsv_length[gsv_currtrc] == 0)  {
		*status = LEN_READTWICE;
		return;
	} /*endif*/

	if  (strcmp(entryname,"DELTA") == 0)  {
		*info = gsv_dt[gsv_currtrc];
	} else {
		printf( "*** ESSTF: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_len_getr */



/*----------------------------------------------------------------------------*/



void sh_len_gets( char entryname[], int maxlth, char info[], STATUS *status )

/* returns string value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * int        maxlth;          input; maximum length of output string
 * char       info[];          output; returned info
 * STATUS     *status;         output; return status
 */
{
	/* local variables */
	int     i;       /* counter */

	/* executable code */

	if  (gsv_length[gsv_currtrc] == 0)  {
		*status = LEN_READTWICE;
		return;
	} /*endif*/

	if  (maxlth < BC_LINELTH)  {
		*status = LEN_NAMEOVFL;
		return;
	} /*endif*/

	if  (strcmp(entryname,"START") == 0)  {
		tc_a2t( gsv_start+gsv_currtrc, info, status );
	} else if  (strcmp(entryname,"STATION") == 0)  {
		strcpy( info, gsv_station[gsv_currtrc] );
		i = strlen( info );
		if  (info[i-1] == ' ')  info[i-1] = '\0';
		ut_cap( info );
	} else {
		printf( "*** LEN: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_len_gets */



/*----------------------------------------------------------------------------*/



void sh_len_getc( char entryname[], char *info, STATUS *status )

/* returns character value
 *
 * parameters of routine
 * char       entryname[];     input; name of entry to be returned
 * char       *info;           output; returned info
 * STATUS     *status;         output; return status
 */
{
	/* executable code */

	if  (gsv_length[gsv_currtrc] == 0)  {
		*status = LEN_READTWICE;
		return;
	} /*endif*/

	if  (strcmp(entryname,"COMP") == 0)  {
		*info = gsv_comp[gsv_currtrc];
	} else {
		printf( "*** LEN: unknown entry %s ***\n", entryname );
	} /*endif*/

} /* end of sh_len_getc */



/*----------------------------------------------------------------------*/



void sh_len_read( float smp[] )

/* returns sample data
 *
 * parameter of routine
 * float     smp[];     output; sample data
 */
{
	/* local variables */
	float    *s, *end;   /* sample pointers */

	/* executable code */

	s = gsv_dat[gsv_currtrc];
	end = gsv_dat[gsv_currtrc] + gsv_length[gsv_currtrc];
	while  (s < end)
		*smp++ = *s++;

	/* reset trace */
	sy_deallocmem( gsv_dat[gsv_currtrc] );
	gsv_length[gsv_currtrc] = 0;
	gsv_dt[gsv_currtrc] = 0.0;
	gsv_station[gsv_currtrc][0] = '\0';
	gsv_comp[gsv_currtrc] = '\0';

} /* end of sh_len_read */


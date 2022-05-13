/* file get_geo.c
 *      =========
 *
 * version 4, 22-May-2006
 *
 * returns geographical region
 * A. Schick
 *
 * changed K. Stammler, 26-Oct-94
 */


/*
 *
 *  SeismicHandler, seismic analysis software
 *  Copyright (C) 1996,  Klaus Stammler, Federal Institute for Geosciences
 *                                       and Natural Resources (BGR), Germany
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

FILE *geo_name;                 /* input file = geographic names  */
FILE *geo_reg;			/* input file = geographic coordinates */

/*	input: elat      latitude
*              elon      longitude
*       output num_id    either region number or if <0 then status
*	       qnam      region name
*
*/

void get_geo(float *elat, float *elon, int *num_id, char *qnam)
{

/*		num_id as return status: -1 cannot open files 		*/
/*					 -2 wrong num_id found          */

    int nreg[125];
    float rlat1,rlat2, rlon1,rlon2, dlat,dlon;
    int idq,nquad,nqmax,mxq,numq;
    int i,j,k,n;
	int m;
	char names_file[200], coors_file[200];
	char *env;

	/* generate filenames */
	env = getenv( "SH_INPUTS" );
	if  (env == NULL)  {
		strcpy( names_file, "ger_geo.names" );
#ifdef SH_SETUP_LINUX
		strcpy( coors_file, "ger_geo_linux.coors" );
#else
		strcpy( coors_file, "ger_geo.coors" );
#endif
	} else {
		strcpy( names_file, env );
		strcpy( coors_file, env );
		strcat( names_file, "ger_geo.names" );
#ifdef SH_SETUP_LINUX
		strcat( coors_file, "ger_geo_linux.coors" );
#else
		strcat( coors_file, "ger_geo.coors" );
#endif
	} /*endif*/

/*--    get input files              */
   if ((geo_name = fopen(names_file,"r"))== NULL)  /* ascii file  */
   {
	*num_id = -1;
	return;
   }
   if ((geo_reg = fopen(coors_file,"r"))== NULL)  /* binary coordinates */
   {
	fclose(geo_name);
	*num_id = -1;
	return;
   }
	
   fread(&i,4,1,geo_reg);               /* get start lat,lon and offsets   */
   for ( j=1; j<=i; fread(&nreg[j++],4,1,geo_reg) );   
   rlat1 = nreg[2] + ((float)nreg[3]/100000.);
   rlat2 = nreg[4] + ((float)nreg[5]/100000.);
   dlat = nreg[6] + ((float)nreg[7]/100000.);
   dlon = nreg[8] + ((float)nreg[9]/100000.);

   if (*elat <= rlat1 && *elat > rlat2)
   {
	n = (int)((rlat1-*elat)/dlat) + 2;
	j=1;
	while (j < n-1)     /* skip records 1 to n-1    */
	{
     		fread(&i,4,1,geo_reg);
		fseek(geo_reg,(long)(4*i),1);    
		j++;
	}
	fread(&i,4,1,geo_reg);                  /*   read record number n   */
        for ( k=1; k<=i; fread(&nreg[k++],4,1,geo_reg) );   
	nquad = nreg[3];
	rlon1 = nreg[1] +  ((float)nreg[2]/100000.);
	nqmax = (nquad-1)*4+4;
	mxq = nquad*4+4;
	rlon2 = nreg[nqmax+2] +  ((float)nreg[nqmax+3]/100000.);
	if (*elon < rlon1)
	{
		*num_id = 1004;
		strcpy(qnam," W of enclosed area");
    		fclose(geo_name);
    		fclose(geo_reg);		
		return;
	}
	else if (*elon > rlon2) 
	{
		*num_id = 1002;
		strcpy(qnam," E of enclosed area");
    		fclose(geo_name);
    		fclose(geo_reg);
		return;	
	}

	i=4;
	while (i <= mxq)
	{
	    rlon2 = nreg[i+2] +  ((float)nreg[i+3]/100000.);
	    if (*elon >= rlon1 && *elon <= rlon2)
	    {
		idq = nreg[i];
		*num_id = nreg[i+1];
		fseek(geo_name,(long)(idq-1)*72L,0);   /* skip idq-1 lines  */
		fscanf(geo_name,"%4d ",&numq);         /* get reg number   */
		fgets(qnam,66,geo_name);               /* get reg name     */
		if (numq != *num_id) 
		{
			*num_id = -2;
    			fclose(geo_name);
    			fclose(geo_reg);
			return;
		}			
		break;
	    }
	    i = i+4;
	}
    }
    else
    {
	if (*elat > rlat1)
	{
	    *num_id = 1001;
	    strcpy(qnam," N of enclosed area");
	    fclose(geo_name);
	    fclose(geo_reg);
	    return;
	}
	else if (*elat <= rlat2)
	{
	    *num_id = 1001;
	    strcpy(qnam," S of enclosed area");
	    fclose(geo_name);
	    fclose(geo_reg);
	    return;
	}
    }
    fclose(geo_name);
    fclose(geo_reg);
	m = strlen( qnam ) - 1;
	while  (m > 0 && (qnam[m] == ' ' || qnam[m] == '\n'))  qnam[m--] = '\0';

}

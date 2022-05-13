
/* file NUMRES.H
 *      ========
 *
 * version 8, 24-Mar-2003
 *
 * prototypes of module NUMRES.C
 * K. Stammler, 22-MAY-91
 */


#ifndef __NUMRES
#define __NUMRES

/* error numbers */
#define NRE_OFFSET     2800
#define NRE_SINGMAT1   (NRE_OFFSET+1)   /* gaussj: singular matrix 1 */
#define NRE_SINGMAT2   (NRE_OFFSET+2)   /* gaussj: singular matrix 2 */

void nr_four1( float data[], long nn, int isign );
void nr_twofft( float data1[], float data2[], float fft1[],
	float fft2[], long n );
void nr_realft( float data[], long n, int isign );
float nr_ran1( int *idum );
float nr_ran2( long *idum );
float nr_ran3( int *idum );
float nr_gasdev( int *idum );
void nr_polint( float xa[], float ya[], int n, float x, float *y, float *dy );
void nr_ratint( float xa[], float ya[], int n, float x, float *y, float *dy );
void nr_memcof( float data[], int n, int m, float *pm, float cof[] );
float nr_evlmem(float fdt, float cof[], int m, float pm );
void nr_spctrm( float dat[], float p[], int m, int k, int ovrlap );
void nr_fit( float x[], float y[], long ndata, float sig[], int mwt,
	float *a, float *b, float *siga, float *sigb, float *chi2, float *q );
float nr_gammq( float a, float x );
void nr_gcf( float *gammcf, float a, float x, float *gln );
void nr_gser( float *gamser, float a, float x, float *gln );
float nr_gammln( float xx );
void nr_mrqmin( float x[], float y[], float sig[], int ndata, float a[],
	int ma, int lista[], int mfit, float **covar, float **alpha, float *chisq,
	void (*funcs)(float,float *,float *,float *,int), float *alamda,
	STATUS *status );
void nr_mrqcof( float x[], float y[], float sig[], int ndata, float a[],
	int ma, int lista[], int mfit, float **alpha, float beta[],
	float *chisq, void (*funcs)(float,float *,float *,float *,int) );
void nr_gaussj( float **a, int n, float **b, int m, STATUS *status );
void nr_covsrt( float **covar, int ma, int lista[], int mfit );
void nr_smooft( float y[], long n, float pts );
float nr_bessj0( float x );
float nr_bessj1( float x );
void nr_predic( float data[], long ndata, float d[], int npoles,
	float future[], int nfut );
float **nr_matrix( int nrl, int nrh, int ncl, int nch );
void nr_free_matrix( float **m, int nrl, int nrh, int ncl, int nch );
void nr_svdcmp( float **a, int m, int n, float *w, float **v );
void nr_svbksb( float **u, float w[], float **v, int m, int n, float b[],
	float x[] );
void nr_amoeba( float **p, float *y, int ndim, float ftol,
	float (*funk)( float *x ), int *nfunk );
float nr_amotry( float **p, float *y, float *psum, int ndim,
	float (*funk)( float *x ), int ihi, int *nfunk, float fac );
void nr_jacobi(float **a, int n, float d[], float **v, int *nrot);

#endif /* __NUMRES */


/* file NUMRES.C
 *      ========
 *
 * version 15, 22-May-2006
 *
 * routines from numerical recipes
 * K. Stammler, 22-MAY-91
 */



#include <math.h>
#include <stdio.h>
#include "basecnst.h"
#include BC_SYSBASE
#ifdef BC_STDLIB_EX
#include <stdlib.h>
#endif /* BC_STDLIB_EX */
#include "numres.h"

/* prototypes of local routines */
float *nrh_vector(int nl, int nh);
void nrh_free_vector(float *v, int nl, int nh);
int *nrh_ivector( int nl, int nh );
void nrh_free_ivector( int *v, int nl, int nh );

#define nr_error(t) printf("%s\n",t)



/*----------------------------------------------------------------------------*/



#define SWAP(a,b) tempr=(a);(a)=(b);(b)=tempr

void nr_four1( float data[], long nn, int isign )

/* float data[];
 * int nn,isign;
 */
{
	long n,mmax,m,j,istep,i;
	double wtemp,wr,wpr,wpi,wi,theta;
	float tempr,tempi;

	n=nn << 1;
	j=1;
	for (i=1;i<n;i+=2) {
		if (j > i) {
			SWAP(data[j],data[i]);
			SWAP(data[j+1],data[i+1]);
		}
		m=n >> 1;
		while (m >= 2 && j > m) {
			j -= m;
			m >>= 1;
		}
		j += m;
	}
	mmax=2;
	while (n > mmax) {
		istep=2*mmax;
		theta=6.28318530717959/(isign*mmax);
		wtemp=sin(0.5*theta);
		wpr = -2.0*wtemp*wtemp;
		wpi=sin(theta);
		wr=1.0;
		wi=0.0;
		for (m=1;m<mmax;m+=2) {
			for (i=m;i<=n;i+=istep) {
				j=i+mmax;
				tempr=wr*data[j]-wi*data[j+1];
				tempi=wr*data[j+1]+wi*data[j];
				data[j]=data[i]-tempr;
				data[j+1]=data[i+1]-tempi;
				data[i] += tempr;
				data[i+1] += tempi;
			}
			wr=(wtemp=wr)*wpr-wi*wpi+wr;
			wi=wi*wpr+wtemp*wpi+wi;
			sy_sharecpu();
		}
		mmax=istep;
	}
}

#undef SWAP



/*----------------------------------------------------------------------------*/



void nr_realft( float data[], long n, int isign )

/* float data[];
 * int n,isign;
 */
{
	long i,i1,i2,i3,i4,n2p3;
	float c1=0.5,c2,h1r,h1i,h2r,h2i;
	double wr,wi,wpr,wpi,wtemp,theta;

	theta=3.141592653589793/(double) n;
	if (isign == 1) {
		c2 = -0.5;
		nr_four1(data,n,1);
	} else {
		c2=0.5;
		theta = -theta;
	}
	wtemp=sin(0.5*theta);
	wpr = -2.0*wtemp*wtemp;
	wpi=sin(theta);
	wr=1.0+wpr;
	wi=wpi;
	n2p3=2*n+3;
	for (i=2;i<=n/2;i++) {
		i4=1+(i3=n2p3-(i2=1+(i1=i+i-1)));
		h1r=c1*(data[i1]+data[i3]);
		h1i=c1*(data[i2]-data[i4]);
		h2r = -c2*(data[i2]+data[i4]);
		h2i=c2*(data[i1]-data[i3]);
		data[i1]=h1r+wr*h2r-wi*h2i;
		data[i2]=h1i+wr*h2i+wi*h2r;
		data[i3]=h1r-wr*h2r+wi*h2i;
		data[i4] = -h1i+wr*h2i+wi*h2r;
		wr=(wtemp=wr)*wpr-wi*wpi+wr;
		wi=wi*wpr+wtemp*wpi+wi;
	}
	sy_sharecpu();
	if (isign == 1) {
		data[1] = (h1r=data[1])+data[2];
		data[2] = h1r-data[2];
	} else {
		data[1]=c1*((h1r=data[1])+data[2]);
		data[2]=c1*(h1r-data[2]);
		nr_four1(data,n,-1);
	}
}



/*----------------------------------------------------------------------------*/



void nr_twofft( float data1[], float data2[], float fft1[],
	float fft2[], long n )
{
	long nn3,nn2,jj,j;
	float rep,rem,aip,aim;

	nn3=1+(nn2=2+n+n);
	for (j=1,jj=2;j<=n;j++,jj+=2) {
		fft1[jj-1]=data1[j];
		fft1[jj]=data2[j];
	}
	nr_four1(fft1,n,1);
	fft2[1]=fft1[2];
	fft1[2]=fft2[2]=0.0;
	for (j=3;j<=n+1;j+=2) {
		rep=0.5*(fft1[j]+fft1[nn2-j]);
		rem=0.5*(fft1[j]-fft1[nn2-j]);
		aip=0.5*(fft1[j+1]+fft1[nn3-j]);
		aim=0.5*(fft1[j+1]-fft1[nn3-j]);
		fft1[j]=rep;
		fft1[j+1]=aim;
		fft1[nn2-j]=rep;
		fft1[nn3-j] = -aim;
		fft2[j]=aip;
		fft2[j+1] = -rem;
		fft2[nn2-j]=aip;
		fft2[nn3-j]=rem;
	}
}



/*----------------------------------------------------------------------------*/



#define M1 259200L
#define IA1 7141
#define IC1 54773L
#define RM1 (1.0/M1)
#define M2 134456L
#define IA2 8121
#define IC2 28411
#define RM2 (1.0/M2)
#define M3 243000L
#define IA3 4561
#define IC3 51349L

float nr_ran1(int *idum)
{
	static unsigned long ix1,ix2,ix3;
	static float r[98];
	float temp;
	static int iff=0;
	int j;

	if (*idum < 0 || iff == 0) {
		iff=1;
		ix1=(IC1-(*idum)) % M1;
		ix1=(IA1*ix1+IC1) % M1;
		ix2=ix1 % M2;
		ix1=(IA1*ix1+IC1) % M1;
		ix3=ix1 % M3;
		for (j=1;j<=97;j++) {
			ix1=(IA1*ix1+IC1) % M1;
			ix2=(IA2*ix2+IC2) % M2;
			r[j]=(ix1+ix2*RM2)*RM1;
		}
		*idum=1;
	}
	ix1=(IA1*ix1+IC1) % M1;
	ix2=(IA2*ix2+IC2) % M2;
	ix3=(IA3*ix3+IC3) % M3;
	j=1 + floor((97*ix3)/M3);
	if (j > 97 || j < 1) nr_error("RAN1: This cannot happen.");
	temp=r[j];
	r[j]=(ix1+ix2*RM2)*RM1;
	return temp;
}

#undef M1
#undef IA1
#undef IC1
#undef RM1
#undef M2
#undef IA2
#undef IC2
#undef RM2
#undef M3
#undef IA3
#undef IC3



/*----------------------------------------------------------------------------*/



#define M 714025L
#define IA 1366
#define IC 150889L

float nr_ran2(long *idum)
{
	static long iy,ir[98];
	static int iff=0;
	int j;

	if (*idum < 0 || iff == 0) {
		iff=1;
		if ((*idum=(IC-(*idum)) % M) < 0) *idum = -(*idum);
		for (j=1;j<=97;j++) {
			*idum=(IA*(*idum)+IC) % M;
			ir[j]=(*idum);
		}
		*idum=(IA*(*idum)+IC) % M;
		iy=(*idum);
	}
	j=1 + floor(97.0*iy/M);
	if (j > 97 || j < 1) {
		nr_error("RAN2: This cannot happen.");
	} /*endif*/
	iy=ir[j];
	*idum=(IA*(*idum)+IC) % M;
	ir[j]=(*idum);
	return (float) iy/M;
}

#undef M
#undef IA
#undef IC



/*----------------------------------------------------------------------------*/



#define MBIG 1000000000L
#define MSEED 161803398L
#define MZ 0
#define FAC (1.0/MBIG)

float nr_ran3(int *idum)
{
	static int inext,inextp;
	static long ma[56];
	static int iff=0;
	long mj,mk;
	int i,ii,k;

	if (*idum < 0 || iff == 0) {
		iff=1;
		mj=MSEED-(*idum < 0 ? -*idum : *idum);
		mj %= MBIG;
		ma[55]=mj;
		mk=1;
		for (i=1;i<=54;i++) {
			ii=(21*i) % 55;
			ma[ii]=mk;
			mk=mj-mk;
			if (mk < MZ) mk += MBIG;
			mj=ma[ii];
		}
		for (k=1;k<=4;k++)
			for (i=1;i<=55;i++) {
				ma[i] -= ma[1+(i+30) % 55];
				if (ma[i] < MZ) ma[i] += MBIG;
			}
		inext=0;
		inextp=31;
		*idum=1;
	}
	if (++inext == 56) inext=1;
	if (++inextp == 56) inextp=1;
	mj=ma[inext]-ma[inextp];
	if (mj < MZ) mj += MBIG;
	ma[inext]=mj;
	return mj*FAC;
}

#undef MBIG
#undef MSEED
#undef MZ
#undef FAC



/*----------------------------------------------------------------------------*/



float nr_gasdev( int *idum )
{
	static int iset=0;
	static float gset;
	float fac,r,v1,v2;

	if  (iset == 0) {
		do {
			v1=2.0*nr_ran1(idum)-1.0;
			v2=2.0*nr_ran1(idum)-1.0;
			r=v1*v1+v2*v2;
		} while (r >= 1.0 || r == 0.0);
		fac=sqrt(-2.0*log(r)/r);
		gset=v1*fac;
		iset=1;
		return v2*fac;
	} else {
		iset=0;
		return gset;
	}
}



/*----------------------------------------------------------------------------*/



void nr_polint( float xa[], float ya[], int n, float x, float *y, float *dy )
{
	int i,m,ns=1;
	float den,dif,dift,ho,hp,w;
	float *c,*d;

	dif=fabs(x-xa[1]);
	c=nrh_vector(1,n);
	d=nrh_vector(1,n);
	for (i=1;i<=n;i++) {
		if ( (dift=fabs(x-xa[i])) < dif) {
			ns=i;
			dif=dift;
		}
		c[i]=ya[i];
		d[i]=ya[i];
	}
	*y=ya[ns--];
	for (m=1;m<n;m++) {
		for (i=1;i<=n-m;i++) {
			ho=xa[i]-x;
			hp=xa[i+m]-x;
			w=c[i+1]-d[i];
			if ( (den=ho-hp) == 0.0) nr_error("Error in routine POLINT");
			den=w/den;
			d[i]=hp*den;
			c[i]=ho*den;
		}
		*y += (*dy=(2*ns < (n-m) ? c[ns+1] : d[ns--]));
	}
	nrh_free_vector(d,1,n);
	nrh_free_vector(c,1,n);
}



/*----------------------------------------------------------------------------*/



#define TINY 1.0e-25
#define FREERETURN {nrh_free_vector(d,1,n);nrh_free_vector(c,1,n);return;}

void nr_ratint(float xa[], float ya[], int n, float x, float *y, float *dy)
{
	int m,i,ns=1;
	float w,t,hh,h,dd,*c,*d;

	c=nrh_vector(1,n);
	d=nrh_vector(1,n);
	hh=fabs(x-xa[1]);
	for (i=1;i<=n;i++) {
		h=fabs(x-xa[i]);
		if (h == 0.0) {
			*y=ya[i];
			*dy=0.0;
			FREERETURN
		} else if (h < hh) {
			ns=i;
			hh=h;
		}
		c[i]=ya[i];
		d[i]=ya[i]+TINY;
	}
	*y=ya[ns--];
	for (m=1;m<n;m++) {
		for (i=1;i<=n-m;i++) {
			w=c[i+1]-d[i];
			h=xa[i+m]-x;
			t=(xa[i]-x)*d[i]/h;
			dd=t-c[i+1];
			if (dd == 0.0) nr_error("Error in routine RATINT");
			dd=w/dd;
			d[i]=c[i+1]*dd;
			c[i]=t*dd;
		}
		*y += (*dy=(2*ns < (n-m) ? c[ns+1] : d[ns--]));
	}
	FREERETURN
}

#undef TINY
#undef FREERETURN



/*----------------------------------------------------------------------------*/



static float sqrarg;
#define SQR(a) (sqrarg=(a),sqrarg*sqrarg)

void nr_memcof( float data[], int n, int m, float *pm, float cof[] )
{
	int k,j,i;
	float p=0.0,*wk1,*wk2,*wkm;

	wk1=nrh_vector(1,n);
	wk2=nrh_vector(1,n);
	wkm=nrh_vector(1,m);
	for (j=1;j<=n;j++) p += SQR(data[j]);
	*pm=p/n;
	wk1[1]=data[1];
	wk2[n-1]=data[n];
	for (j=2;j<=n-1;j++) {
		wk1[j]=data[j];
		wk2[j-1]=data[j];
	}
	for (k=1;k<=m;k++) {
		float num=0.0,denom=0.0;
		for (j=1;j<=(n-k);j++) {
			num += wk1[j]*wk2[j];
			denom += SQR(wk1[j])+SQR(wk2[j]);
		}
		cof[k]=2.0*num/denom;
		*pm *= (1.0-SQR(cof[k]));
		for (i=1;i<=(k-1);i++)
			cof[i]=wkm[i]-cof[k]*wkm[k-i];
		if (k == m) {
			nrh_free_vector(wkm,1,m);
			nrh_free_vector(wk2,1,n);
			nrh_free_vector(wk1,1,n);
			return;
		}
		for (i=1;i<=k;i++) wkm[i]=cof[i];
		for (j=1;j<=(n-k-1);j++) {
			wk1[j] -= wkm[k]*wk2[j];
			wk2[j]=wk2[j+1]-wkm[k]*wk1[j+1];
		}
		sy_sharecpu();
	}
}

#undef SQR



/*----------------------------------------------------------------------------*/



float nr_evlmem( float fdt, float cof[], int m, float pm )
{
	int i;
	float sumr=1.0,sumi=0.0;
	double wr=1.0,wi=0.0,wpr,wpi,wtemp,theta;

	theta=6.28318530717959*fdt;
	wpr=cos(theta);
	wpi=sin(theta);
	for (i=1;i<=m;i++) {
		wr=(wtemp=wr)*wpr-wi*wpi;
		wi=wi*wpr+wtemp*wpi;
		sumr -= cof[i]*wr;
		sumi -= cof[i]*wi;
	}
	return pm/(sumr*sumr+sumi*sumi);
}



/*----------------------------------------------------------------------------*/


#define SQR(a) (sqrarg=(a),sqrarg*sqrarg)
#define WINDOW(j,a,b) (1.0-fabs((((j)-1)-(a))*(b)))       /* Parzen */
/* #define WINDOW(j,a,b) 1.0 */                           /* Square */
/* #define WINDOW(j,a,b) (1.0-SQR((((j)-1)-(a))*(b))) */  /* Welch  */

void nr_spctrm( float dat[], float p[], int m, int k, int ovrlap )
{
	int mm,m44,m43,m4,kk,joffn,joff,j2,j;
	float w,facp,facm,*w1,*w2,sumw=0.0,den=0.0;

	dat++;  /* this to be compatible to other array arguments */

	mm=m+m;
	m43=(m4=mm+mm)+3;
	m44=m43+1;
	w1=nrh_vector(1,m4);
	w2=nrh_vector(1,m);
	facm=m-0.5;
	facp=1.0/(m+0.5);
	for (j=1;j<=mm;j++) sumw += SQR(WINDOW(j,facm,facp));
	for (j=1;j<=m;j++) p[j]=0.0;
	if (ovrlap)
		for (j=1;j<=m;j++) w2[j] = *dat++; /* fscanf(fp,"%f",&w2[j]); */
	for (kk=1;kk<=k;kk++) {
		for (joff = -1;joff<=0;joff++) {
			if (ovrlap) {
				for (j=1;j<=m;j++) w1[joff+j+j]=w2[j];
				for (j=1;j<=m;j++) w2[j] = *dat++; /* fscanf(fp,"%f",&w2[j]); */
				joffn=joff+mm;
				for (j=1;j<=m;j++) w1[joffn+j+j]=w2[j];
			} else {
				for (j=joff+2;j<=m4;j+=2)
					w1[j] = *dat++; /* fscanf(fp,"%f",&w1[j]); */
			}
		}
		for (j=1;j<=mm;j++) {
			j2=j+j;
			w=WINDOW(j,facm,facp);
			w1[j2] *= w;
			w1[j2-1] *= w;
		}
		nr_four1(w1,mm,1);
		p[1] += (SQR(w1[1])+SQR(w1[2]));
		for (j=2;j<=m;j++) {
			j2=j+j;
			p[j] += (SQR(w1[j2])+SQR(w1[j2-1])
				+SQR(w1[m44-j2])+SQR(w1[m43-j2]));
		}
		den += sumw;
		sy_sharecpu();
	}
	den *= m4;
	for (j=1;j<=m;j++) p[j] /= den;
	nrh_free_vector(w2,1,m);
	nrh_free_vector(w1,1,m4);
}

#undef SQR
#undef WINDOW


/*----------------------------------------------------------------------------*/


#define SQR(a) (sqrarg=(a),sqrarg*sqrarg)

void nr_fit( float x[], float y[], long ndata, float sig[], int mwt,
	float *a, float *b, float *siga, float *sigb, float *chi2, float *q )
{
	long i;
	float wt,t,sxoss,sx=0.0,sy=0.0,st2=0.0,ss,sigdat;

	*b=0.0;
	if (mwt) {
		ss=0.0;
		for (i=1;i<=ndata;i++) {
			wt=1.0/SQR(sig[i]);
			ss += wt;
			sx += x[i]*wt;
			sy += y[i]*wt;
		}
	} else {
		for (i=1;i<=ndata;i++) {
			sx += x[i];
			sy += y[i];
		}
		ss=ndata;
	}
	sxoss=sx/ss;
	if (mwt) {
		for (i=1;i<=ndata;i++) {
			t=(x[i]-sxoss)/sig[i];
			st2 += t*t;
			*b += t*y[i]/sig[i];
		}
	} else {
		for (i=1;i<=ndata;i++) {
			t=x[i]-sxoss;
			st2 += t*t;
			*b += t*y[i];
		}
	}
	*b /= st2;
	*a=(sy-sx*(*b))/ss;
	*siga=sqrt((1.0+sx*sx/(ss*st2))/ss);
	*sigb=sqrt(1.0/st2);
	*chi2=0.0;
	if (mwt == 0) {
		for (i=1;i<=ndata;i++)
			*chi2 += SQR(y[i]-(*a)-(*b)*x[i]);
		*q=1.0;
		sigdat=sqrt((*chi2)/(ndata-2));
		*siga *= sigdat;
		*sigb *= sigdat;
	} else {
		for (i=1;i<=ndata;i++)
			*chi2 += SQR((y[i]-(*a)-(*b)*x[i])/sig[i]);
		*q=nr_gammq(0.5*(ndata-2),0.5*(*chi2));
	}
}

#undef SQR

/*----------------------------------------------------------------------------*/



float nr_gammq( float a, float x )
{
	float gamser,gammcf,gln;

	if (x < 0.0 || a <= 0.0) nr_error("Invalid arguments in routine GAMMQ");
	if (x < (a+1.0)) {
		nr_gser(&gamser,a,x,&gln);
		return 1.0-gamser;
	} else {
		nr_gcf(&gammcf,a,x,&gln);
		return gammcf;
	}
}



/*----------------------------------------------------------------------------*/


#define ITMAX 100
#define EPS 3.0e-7

void nr_gcf( float *gammcf, float a, float x, float *gln )
{
	int n;
	float gold=0.0,g,fac=1.0,b1=1.0;
	float b0=0.0,anf,ana,an,a1,a0=1.0;

	*gln=nr_gammln(a);
	a1=x;
	for (n=1;n<=ITMAX;n++) {
		an=(float) n;
		ana=an-a;
		a0=(a1+a0*ana)*fac;
		b0=(b1+b0*ana)*fac;
		anf=an*fac;
		a1=x*a0+anf*a1;
		b1=x*b0+anf*b1;
		if (a1) {
			fac=1.0/a1;
			g=b1*fac;
			if (fabs((g-gold)/g) < EPS) {
				*gammcf=exp(-x+a*log(x)-(*gln))*g;
				return;
			}
			gold=g;
		}
	}
	nr_error("a too large, ITMAX too small in routine GCF");
}

#undef ITMAX
#undef EPS


/*----------------------------------------------------------------------------*/


#define ITMAX 100
#define EPS 3.0e-7

void nr_gser( float *gamser, float a, float x, float *gln )
{
	int n;
	float sum,del,ap;

	*gln=nr_gammln(a);
	if (x <= 0.0) {
		if (x < 0.0) nr_error("x less than 0 in routine GSER");
		*gamser=0.0;
		return;
	} else {
		ap=a;
		del=sum=1.0/a;
		for (n=1;n<=ITMAX;n++) {
			ap += 1.0;
			del *= x/ap;
			sum += del;
			if (fabs(del) < fabs(sum)*EPS) {
				*gamser=sum*exp(-x+a*log(x)-(*gln));
				return;
			}
		}
		nr_error("a too large, ITMAX too small in routine GSER");
		return;
	}
}

#undef ITMAX
#undef EPS


/*----------------------------------------------------------------------------*/



void nr_mrqmin( float x[], float y[], float sig[], int ndata, float a[],
	int ma, int lista[], int mfit, float **covar, float **alpha, float *chisq,
	void (*funcs)(float,float *,float *,float *,int), float *alamda,
	STATUS *status )
{
	int k,kk,j,ihit;
	static float *da,*atry,**oneda,*beta,ochisq;

	if (*alamda < 0.0) {
		oneda=nr_matrix(1,mfit,1,1);
		atry=nrh_vector(1,ma);
		da=nrh_vector(1,ma);
		beta=nrh_vector(1,ma);
		kk=mfit+1;
		for (j=1;j<=ma;j++) {
			ihit=0;
			for (k=1;k<=mfit;k++)
				if (lista[k] == j) ihit++;
			if (ihit == 0)
				lista[kk++]=j;
			else if (ihit > 1) nr_error("Bad LISTA permutation in MRQMIN-1");
		}
		if (kk != ma+1) nr_error("Bad LISTA permutation in MRQMIN-2");
		*alamda=0.001;
		nr_mrqcof(x,y,sig,ndata,a,ma,lista,mfit,alpha,beta,chisq,funcs);
		ochisq=(*chisq);
	}
	for (j=1;j<=mfit;j++) {
		for (k=1;k<=mfit;k++) covar[j][k]=alpha[j][k];
		covar[j][j]=alpha[j][j]*(1.0+(*alamda));
		oneda[j][1]=beta[j];
	}
	nr_gaussj(covar,mfit,oneda,1,status);  /* status: KS 27-AUG-91 */
	if  (Severe(status))  {                /* :                    */
		nrh_free_vector(beta,1,ma);
		nrh_free_vector(da,1,ma);
		nrh_free_vector(atry,1,ma);
		nr_free_matrix(oneda,1,mfit,1,1);
		return;
	} /*endif*/                            /* end of insertion     */
	for (j=1;j<=mfit;j++)
		da[j]=oneda[j][1];
	if (*alamda == 0.0) {
		nr_covsrt(covar,ma,lista,mfit);
		nrh_free_vector(beta,1,ma);
		nrh_free_vector(da,1,ma);
		nrh_free_vector(atry,1,ma);
		nr_free_matrix(oneda,1,mfit,1,1);
		return;
	}
	for (j=1;j<=ma;j++) atry[j]=a[j];
	for (j=1;j<=mfit;j++)
		atry[lista[j]] = a[lista[j]]+da[j];
	nr_mrqcof(x,y,sig,ndata,atry,ma,lista,mfit,covar,da,chisq,funcs);
	if (*chisq < ochisq) {
		*alamda *= 0.1;
		ochisq=(*chisq);
		for (j=1;j<=mfit;j++) {
			for (k=1;k<=mfit;k++) alpha[j][k]=covar[j][k];
			beta[j]=da[j];
			a[lista[j]]=atry[lista[j]];
		}
	} else {
		*alamda *= 10.0;
		*chisq=ochisq;
	}
	return;
}



/*----------------------------------------------------------------------------*/



void nr_mrqcof( float x[], float y[], float sig[], int ndata, float a[],
	int ma, int lista[], int mfit, float **alpha, float beta[],
	float *chisq, void (*funcs)(float,float *,float *,float *,int) )
{
	int k,j,i;
	float ymod,wt,sig2i,dy,*dyda;

	dyda=nrh_vector(1,ma);
	for (j=1;j<=mfit;j++) {
		for (k=1;k<=j;k++) alpha[j][k]=0.0;
		beta[j]=0.0;
	}
	*chisq=0.0;
	for (i=1;i<=ndata;i++) {
		(*funcs)(x[i],a,&ymod,dyda,ma);
		sig2i=1.0/(sig[i]*sig[i]);
		dy=y[i]-ymod;
		for (j=1;j<=mfit;j++) {
			wt=dyda[lista[j]]*sig2i;
			for (k=1;k<=j;k++)
				alpha[j][k] += wt*dyda[lista[k]];
			beta[j] += dy*wt;
		}
		(*chisq) += dy*dy*sig2i;
	}
	for (j=2;j<=mfit;j++)
		for (k=1;k<=j-1;k++) alpha[k][j]=alpha[j][k];
	nrh_free_vector(dyda,1,ma);
}



/*----------------------------------------------------------------------------*/



#define SWAP(a,b) {float temp=(a);(a)=(b);(b)=temp;}

void nr_gaussj( float **a, int n, float **b, int m, STATUS *status )
{
	int *indxc,*indxr,*ipiv;
	int i,icol,irow,j,k,l,ll;
	float big,dum,pivinv;

	indxc=nrh_ivector(1,n);
	indxr=nrh_ivector(1,n);
	ipiv=nrh_ivector(1,n);
	for (j=1;j<=n;j++) ipiv[j]=0;
	for (i=1;i<=n;i++) {
		big=0.0;
		for (j=1;j<=n;j++)
			if (ipiv[j] != 1)
				for (k=1;k<=n;k++) {
					if (ipiv[k] == 0) {
						if (fabs(a[j][k]) >= big) {
							big=fabs(a[j][k]);
							irow=j;
							icol=k;
						}
					} else if (ipiv[k] > 1) {*status = NRE_SINGMAT1; return;}
				}
		++(ipiv[icol]);
		if (irow != icol) {
			for (l=1;l<=n;l++) SWAP(a[irow][l],a[icol][l])
			for (l=1;l<=m;l++) SWAP(b[irow][l],b[icol][l])
		}
		indxr[i]=irow;
		indxc[i]=icol;
		if (a[icol][icol] == 0.0) {*status = NRE_SINGMAT2; return;}
		pivinv=1.0/a[icol][icol];
		a[icol][icol]=1.0;
		for (l=1;l<=n;l++) a[icol][l] *= pivinv;
		for (l=1;l<=m;l++) b[icol][l] *= pivinv;
		for (ll=1;ll<=n;ll++)
			if (ll != icol) {
				dum=a[ll][icol];
				a[ll][icol]=0.0;
				for (l=1;l<=n;l++) a[ll][l] -= a[icol][l]*dum;
				for (l=1;l<=m;l++) b[ll][l] -= b[icol][l]*dum;
			}
	}
	for (l=n;l>=1;l--) {
		if (indxr[l] != indxc[l])
			for (k=1;k<=n;k++)
				SWAP(a[k][indxr[l]],a[k][indxc[l]]);
	}
	nrh_free_ivector(ipiv,1,n);
	nrh_free_ivector(indxr,1,n);
	nrh_free_ivector(indxc,1,n);
}

#undef SWAP



/*----------------------------------------------------------------------------*/



void nr_covsrt( float **covar, int ma, int lista[], int mfit )
{
	int i,j;
	float swap;

	for (j=1;j<ma;j++)
		for (i=j+1;i<=ma;i++) covar[i][j]=0.0;
	for (i=1;i<mfit;i++)
		for (j=i+1;j<=mfit;j++) {
			if (lista[j] > lista[i])
				covar[lista[j]][lista[i]]=covar[i][j];
			else
				covar[lista[i]][lista[j]]=covar[i][j];
		}
	swap=covar[1][1];
	for (j=1;j<=ma;j++) {
		covar[1][j]=covar[j][j];
		covar[j][j]=0.0;
	}
	covar[lista[1]][lista[1]]=swap;
	for (j=2;j<=mfit;j++) covar[lista[j]][lista[j]]=covar[1][j];
	for (j=2;j<=ma;j++)
		for (i=1;i<=j-1;i++) covar[i][j]=covar[j][i];
}



/*----------------------------------------------------------------------------*/



float nr_gammln( float xx )
{
	double x,tmp,ser;
	static double cof[6]={76.18009173,-86.50532033,24.01409822,
		-1.231739516,0.120858003e-2,-0.536382e-5};
	int j;

	x=xx-1.0;
	tmp=x+5.5;
	tmp -= (x+0.5)*log(tmp);
	ser=1.0;
	for (j=0;j<=5;j++) {
		x += 1.0;
		ser += cof[j]/x;
	}
	return -tmp+log(2.50662827465*ser);
}


/*----------------------------------------------------------------------------*/



void nr_smooft( float y[], long n, float pts )
{
	long nmin,m=2,mo2,k,j;
	float yn,y1,rn1,fac,cnst;

	nmin=n+(int) (2.0*pts+0.5);
	while (m < nmin) m *= 2;
	cnst=pts/m,cnst=cnst*cnst;
	y1=y[1];
	yn=y[n];
	rn1=1.0/(n-1);
	for (j=1;j<=n;j++)
		y[j] += (-rn1*(y1*(n-j)+yn*(j-1)));
	for (j=n+1;j<=m;j++) y[j]=0.0;
	mo2=m >> 1;
	nr_realft(y,mo2,1);
	y[1] /= mo2;
	fac=1.0;
	for (j=1;j<mo2;j++) {
		k=2*j+1;
		if (fac) {
			if ( (fac=(1.0-cnst*j*j)/mo2) < 0.0) fac=0.0;
			y[k]=fac*y[k];
			y[k+1]=fac*y[k+1];
		} else  y[k+1]=y[k]=0.0;
	}
	if ( (fac=(1.0-0.25*pts*pts)/mo2) < 0.0) fac=0.0;
	y[2] *= fac;
	nr_realft(y,mo2,-1);
	for (j=1;j<=n;j++)
		y[j] += rn1*(y1*(n-j)+yn*(j-1));
}



/*----------------------------------------------------------------------------*/



float nr_bessj0( float x )
{
	float ax,z;
	double xx,y,ans,ans1,ans2;

	if ((ax=fabs(x)) < 8.0) {
		y=x*x;
		ans1=57568490574.0+y*(-13362590354.0+y*(651619640.7
			+y*(-11214424.18+y*(77392.33017+y*(-184.9052456)))));
		ans2=57568490411.0+y*(1029532985.0+y*(9494680.718
			+y*(59272.64853+y*(267.8532712+y*1.0))));
		ans=ans1/ans2;
	} else {
		z=8.0/ax;
		y=z*z;
		xx=ax-0.785398164;
		ans1=1.0+y*(-0.1098628627e-2+y*(0.2734510407e-4
			+y*(-0.2073370639e-5+y*0.2093887211e-6)));
		ans2 = -0.1562499995e-1+y*(0.1430488765e-3
			+y*(-0.6911147651e-5+y*(0.7621095161e-6
			-y*0.934935152e-7)));
		ans=sqrt(0.636619772/ax)*(cos(xx)*ans1-z*sin(xx)*ans2);
	}
	return ans;
}



/*----------------------------------------------------------------------------*/



float nr_bessj1( float x )
{
	float ax,z;
	double xx,y,ans,ans1,ans2;

	if ((ax=fabs(x)) < 8.0) {
		y=x*x;
		ans1=x*(72362614232.0+y*(-7895059235.0+y*(242396853.1
			+y*(-2972611.439+y*(15704.48260+y*(-30.16036606))))));
		ans2=144725228442.0+y*(2300535178.0+y*(18583304.74
			+y*(99447.43394+y*(376.9991397+y*1.0))));
		ans=ans1/ans2;
	} else {
		z=8.0/ax;
		y=z*z;
		xx=ax-2.356194491;
		ans1=1.0+y*(0.183105e-2+y*(-0.3516396496e-4
			+y*(0.2457520174e-5+y*(-0.240337019e-6))));
		ans2=0.04687499995+y*(-0.2002690873e-3
			+y*(0.8449199096e-5+y*(-0.88228987e-6
			+y*0.105787412e-6)));
		ans=sqrt(0.636619772/ax)*(cos(xx)*ans1-z*sin(xx)*ans2);
		if (x < 0.0) ans = -ans;
	}
	return ans;
}



/*------------------------------------------------------------------*/



void nr_predic( float data[], long ndata, float d[], int npoles,
	float future[], int nfut )
{
	int k,j;
	float sum,discrp,*reg;

	reg=nrh_vector(1,npoles);
	for (j=1;j<=npoles;j++) reg[j]=data[ndata+1-j];
	for (j=1;j<=nfut;j++) {
		discrp=0.0;
		sum=discrp;
		for (k=1;k<=npoles;k++) sum += d[k]*reg[k];
		for (k=npoles;k>=2;k--) reg[k]=reg[k-1];
		future[j]=reg[1]=sum;
	}
	nrh_free_vector(reg,1,npoles);
}



/*----------------------------------------------------------------------------*/



float **nr_matrix(int nrl, int nrh, int ncl, int nch)
{
	int i;
	float **m;

	m=(float **) malloc((unsigned) (nrh-nrl+1)*sizeof(float*));
	if (!m) nr_error("allocation failure 1 in matrix()");
	m -= nrl;

	for(i=nrl;i<=nrh;i++) {
		m[i]=(float *) malloc((unsigned) (nch-ncl+1)*sizeof(float));
		if (!m[i]) nr_error("allocation failure 2 in matrix()");
		m[i] -= ncl;
	}
	return m;
}



/*----------------------------------------------------------------------------*/



void nr_free_matrix(float **m, int nrl, int nrh, int ncl, int nch)
{
	int i;

	for(i=nrh;i>=nrl;i--) free((char*) (m[i]+ncl));
	free((char*) (m+nrl));
}



/*----------------------------------------------------------------------------*/



static float at,bt,ct;
#define PYTHAG(a,b) ((at=fabs(a)) > (bt=fabs(b)) ? \
(ct=bt/at,at*sqrt(1.0+ct*ct)) : (bt ? (ct=at/bt,bt*sqrt(1.0+ct*ct)): 0.0))

static float maxarg1,maxarg2;
#define MAX(a,b) (maxarg1=(a),maxarg2=(b),(maxarg1) > (maxarg2) ?\
	(maxarg1) : (maxarg2))
#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))

void nr_svdcmp( float **a, int m, int n, float *w, float **v )
{
	int flag,i,its,j,jj,k,l,nm;
	float c,f,h,s,x,y,z;
	float anorm=0.0,g=0.0,scale=0.0;
	float *rv1;

	if (m < n) nr_error("SVDCMP: You must augment A with extra zero rows");
	rv1=nrh_vector(1,n);
	for (i=1;i<=n;i++) {
		l=i+1;
		rv1[i]=scale*g;
		g=s=scale=0.0;
		if (i <= m) {
			for (k=i;k<=m;k++) scale += fabs(a[k][i]);
			if (scale) {
				for (k=i;k<=m;k++) {
					a[k][i] /= scale;
					s += a[k][i]*a[k][i];
				}
				f=a[i][i];
				g = -SIGN(sqrt(s),f);
				h=f*g-s;
				a[i][i]=f-g;
				if (i != n) {
					for (j=l;j<=n;j++) {
						for (s=0.0,k=i;k<=m;k++) s += a[k][i]*a[k][j];
						f=s/h;
						for (k=i;k<=m;k++) a[k][j] += f*a[k][i];
					}
				}
				for (k=i;k<=m;k++) a[k][i] *= scale;
			}
		}
		w[i]=scale*g;
		g=s=scale=0.0;
		if (i <= m && i != n) {
			for (k=l;k<=n;k++) scale += fabs(a[i][k]);
			if (scale) {
				for (k=l;k<=n;k++) {
					a[i][k] /= scale;
					s += a[i][k]*a[i][k];
				}
				f=a[i][l];
				g = -SIGN(sqrt(s),f);
				h=f*g-s;
				a[i][l]=f-g;
				for (k=l;k<=n;k++) rv1[k]=a[i][k]/h;
				if (i != m) {
					for (j=l;j<=m;j++) {
						for (s=0.0,k=l;k<=n;k++) s += a[j][k]*a[i][k];
						for (k=l;k<=n;k++) a[j][k] += s*rv1[k];
					}
				}
				for (k=l;k<=n;k++) a[i][k] *= scale;
			}
		}
		anorm=MAX(anorm,(fabs(w[i])+fabs(rv1[i])));
	}
	for (i=n;i>=1;i--) {
		if (i < n) {
			if (g) {
				for (j=l;j<=n;j++)
					v[j][i]=(a[i][j]/a[i][l])/g;
				for (j=l;j<=n;j++) {
					for (s=0.0,k=l;k<=n;k++) s += a[i][k]*v[k][j];
					for (k=l;k<=n;k++) v[k][j] += s*v[k][i];
				}
			}
			for (j=l;j<=n;j++) v[i][j]=v[j][i]=0.0;
		}
		v[i][i]=1.0;
		g=rv1[i];
		l=i;
	}
	for (i=n;i>=1;i--) {
		l=i+1;
		g=w[i];
		if (i < n)
			for (j=l;j<=n;j++) a[i][j]=0.0;
		if (g) {
			g=1.0/g;
			if (i != n) {
				for (j=l;j<=n;j++) {
					for (s=0.0,k=l;k<=m;k++) s += a[k][i]*a[k][j];
					f=(s/a[i][i])*g;
					for (k=i;k<=m;k++) a[k][j] += f*a[k][i];
				}
			}
			for (j=i;j<=m;j++) a[j][i] *= g;
		} else {
			for (j=i;j<=m;j++) a[j][i]=0.0;
		}
		++a[i][i];
	}
	for (k=n;k>=1;k--) {
		for (its=1;its<=30;its++) {
			flag=1;
			for (l=k;l>=1;l--) {
				nm=l-1;
				if ((float)(fabs(rv1[l])+anorm) == anorm) {
					flag=0;
					break;
				}
				if ((float)(fabs(w[nm])+anorm) == anorm) break;
			}
			if (flag) {
				c=0.0;
				s=1.0;
				for (i=l;i<=k;i++) {
					f=s*rv1[i];
					rv1[i]=c*rv1[i];
					if ((float)(fabs(f)+anorm) == anorm) break;
					g=w[i];
					h=PYTHAG(f,g);
					w[i]=h;
					h=1.0/h;
					c=g*h;
					s=(-f*h);
					for (j=1;j<=m;j++) {
						y=a[j][nm];
						z=a[j][i];
						a[j][nm]=y*c+z*s;
						a[j][i]=z*c-y*s;
					}
				}
			}
			z=w[k];
			if (l == k) {
				if (z < 0.0) {
					w[k] = -z;
					for (j=1;j<=n;j++) v[j][k]=(-v[j][k]);
				}
				break;
			}
			if (its == 30) nr_error("No convergence in 30 SVDCMP iterations");
			x=w[l];
			nm=k-1;
			y=w[nm];
			g=rv1[nm];
			h=rv1[k];
			f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y);
			g=PYTHAG(f,1.0);
			f=((x-z)*(x+z)+h*((y/(f+SIGN(g,f)))-h))/x;
			c=s=1.0;
			for (j=l;j<=nm;j++) {
				i=j+1;
				g=rv1[i];
				y=w[i];
				h=s*g;
				g=c*g;
				z=PYTHAG(f,h);
				rv1[j]=z;
				c=f/z;
				s=h/z;
				f=x*c+g*s;
				g=g*c-x*s;
				h=y*s;
				y=y*c;
				for (jj=1;jj<=n;jj++) {
					x=v[jj][j];
					z=v[jj][i];
					v[jj][j]=x*c+z*s;
					v[jj][i]=z*c-x*s;
				}
				z=PYTHAG(f,h);
				w[j]=z;
				if (z) {
					z=1.0/z;
					c=f*z;
					s=h*z;
				}
				f=(c*g)+(s*y);
				x=(c*y)-(s*g);
				for (jj=1;jj<=m;jj++) {
					y=a[jj][j];
					z=a[jj][i];
					a[jj][j]=y*c+z*s;
					a[jj][i]=z*c-y*s;
				}
			}
			rv1[l]=0.0;
			rv1[k]=f;
			w[k]=x;
		}
	}
	nrh_free_vector(rv1,1,n);
}

#undef SIGN
#undef MAX
#undef PYTHAG



/*----------------------------------------------------------------------------*/



void nr_svbksb( float **u, float w[], float **v, int m, int n, float b[],
	float x[] )
{
	int jj,j,i;
	float s,*tmp;

	tmp=nrh_vector(1,n);
	for (j=1;j<=n;j++) {
		s=0.0;
		if (w[j]) {
			for (i=1;i<=m;i++) s += u[i][j]*b[i];
			s /= w[j];
		}
		tmp[j]=s;
	}
	for (j=1;j<=n;j++) {
		s=0.0;
		for (jj=1;jj<=n;jj++) s += v[j][jj]*tmp[jj];
		x[j]=s;
	}
	nrh_free_vector(tmp,1,n);
}



/*----------------------------------------------------------------------------*/



#define NMAX 5000
#define ALPHA 1.0
#define BETA 0.5
#define GAMMA 2.0

#define GET_PSUM for (j=1; j<=ndim; j++)  {for (i=1,sum=0.0; i<=mpts; i++) \
						sum += p[i][j]; psum[j] = sum;}


void nr_amoeba( float **p, float *y, int ndim, float ftol,
	float (*funk)( float *x ), int *nfunk )

/* NR amoeba routine
 *
 * parameters see book
 */
{
	int i,j,ilo,ihi,inhi,mpts=ndim+1;
	float ytry,ysave,sum,rtol,*psum;

	psum=nrh_vector(1,ndim);
	*nfunk=0;
	GET_PSUM
	for (;;) {
		ilo=1;
		ihi = y[1]>y[2] ? (inhi=2,1) : (inhi=1,2);
		for (i=1;i<=mpts;i++) {
			if (y[i] < y[ilo]) ilo=i;
			if (y[i] > y[ihi]) {
				inhi=ihi;
				ihi=i;
			} else if (y[i] > y[inhi])
				if (i != ihi) inhi=i;
		}
		rtol=2.0*fabs(y[ihi]-y[ilo])/(fabs(y[ihi])+fabs(y[ilo]));
		if (rtol < ftol) break;
		if  (*nfunk >= NMAX)  nr_error( "Too many iterations in AMOEBA" );
		ytry=nr_amotry(p,y,psum,ndim,funk,ihi,nfunk,-ALPHA);
		if (ytry <= y[ilo])
			ytry=nr_amotry(p,y,psum,ndim,funk,ihi,nfunk,GAMMA);
		else if (ytry >= y[inhi]) {
			ysave=y[ihi];
			ytry=nr_amotry(p,y,psum,ndim,funk,ihi,nfunk,BETA);
			if (ytry >= ysave) {
				for (i=1; i<=mpts; i++) {
					if (i != ilo) {
						for (j=1; j<=ndim; j++) {
							psum[j] = 0.5*(p[i][j]+p[ilo][j]);
							p[i][j] = psum[j];
						}
						y[i] = (*funk)(psum);
					}
				}
				*nfunk += ndim;
				GET_PSUM
			}
		}
	}
	nrh_free_vector(psum,1,ndim);

} /* end of nr_amoeba */


#undef GET_PSUM
#undef NMAX
#undef ALPHA
#undef BETA
#undef GAMMA



/*----------------------------------------------------------------------------*/



float nr_amotry( float **p, float *y, float *psum, int ndim,
	float (*funk)( float *x ), int ihi, int *nfunk, float fac )

/* NR routine amotry
 *
 * parameters see book
 */
{
	int j;
	float fac1, fac2, ytry, *ptry;

	ptry=nrh_vector(1,ndim);
	fac1=(1.0-fac)/ndim;
	fac2=fac1-fac;
	for (j=1; j<=ndim; j++) ptry[j]=psum[j]*fac1-p[ihi][j]*fac2;
	ytry=(*funk)(ptry);
	++(*nfunk);
	if (ytry < y[ihi]) {
		y[ihi]=ytry;
		for (j=1; j<=ndim; j++) {
			psum[j] += ptry[j]-p[ihi][j];
			p[ihi][j] = ptry[j];
		}
	}
	nrh_free_vector(ptry,1,ndim);
	return ytry;

}



/*----------------------------------------------------------------------------*/



#define ROTATE(a,i,j,k,l) g=a[i][j];h=a[k][l];a[i][j]=g-s*(h+g*tau);\
	a[k][l]=h+s*(g-h*tau);

void nr_jacobi(float **a, int n, float d[], float **v, int *nrot)
{
	int j,iq,ip,i;
	float tresh,theta,tau,t,sm,s,h,g,c,*b,*z;

	b=nrh_vector(1,n);
	z=nrh_vector(1,n);
	for (ip=1;ip<=n;ip++) {
		for (iq=1;iq<=n;iq++) v[ip][iq]=0.0;
		v[ip][ip]=1.0;
	}
	for (ip=1;ip<=n;ip++) {
		b[ip]=d[ip]=a[ip][ip];
		z[ip]=0.0;
	}
	*nrot=0;
	for (i=1;i<=50;i++) {
		sm=0.0;
		for (ip=1;ip<=n-1;ip++) {
			for (iq=ip+1;iq<=n;iq++)
				sm += fabs(a[ip][iq]);
		}
		if (sm == 0.0) {
			nrh_free_vector(z,1,n);
			nrh_free_vector(b,1,n);
			return;
		}
		if (i < 4)
			tresh=0.2*sm/(n*n);
		else
			tresh=0.0;
		for (ip=1;ip<=n-1;ip++) {
			for (iq=ip+1;iq<=n;iq++) {
				g=100.0*fabs(a[ip][iq]);
				if (i > 4 && (float)(fabs(d[ip])+g) == (float)fabs(d[ip])
					&& (float)(fabs(d[iq])+g) == (float)fabs(d[iq]))
					a[ip][iq]=0.0;
				else if (fabs(a[ip][iq]) > tresh) {
					h=d[iq]-d[ip];
					if ((float)(fabs(h)+g) == (float)fabs(h))
						t=(a[ip][iq])/h;
					else {
						theta=0.5*h/(a[ip][iq]);
						t=1.0/(fabs(theta)+sqrt(1.0+theta*theta));
						if (theta < 0.0) t = -t;
					}
					c=1.0/sqrt(1+t*t);
					s=t*c;
					tau=s/(1.0+c);
					h=t*a[ip][iq];
					z[ip] -= h;
					z[iq] += h;
					d[ip] -= h;
					d[iq] += h;
					a[ip][iq]=0.0;
					for (j=1;j<=ip-1;j++) {
						ROTATE(a,j,ip,j,iq)
					}
					for (j=ip+1;j<=iq-1;j++) {
						ROTATE(a,ip,j,j,iq)
					}
					for (j=iq+1;j<=n;j++) {
						ROTATE(a,ip,j,iq,j)
					}
					for (j=1;j<=n;j++) {
						ROTATE(v,j,ip,j,iq)
					}
					++(*nrot);
				}
			}
		}
		for (ip=1;ip<=n;ip++) {
			b[ip] += z[ip];
			d[ip]=b[ip];
			z[ip]=0.0;
		}
	}
	nr_error("Too many iterations in routine jacobi");
}
#undef ROTATE



/*----------------------------------------------------------------------------*/



/* utility routines */

float *nrh_vector(int nl, int nh)
{
	float *v;

	v=(float *)malloc((unsigned) (nh-nl+1)*sizeof(float));
	if (!v) nr_error("allocation failure in nrh_vector()");
	return v-nl;
}

int *nrh_ivector( int nl, int nh )
{
	int *v;

	v=(int *)malloc((unsigned) (nh-nl+1)*sizeof(int));
	if (!v) nr_error("allocation failure in nrh_ivector()");
	return v-nl;
}

void nrh_free_vector(float *v, int nl, int nh)
{
	if  (nh)  {}  /* just to touch nh */
	free((char*) (v+nl));
}

void nrh_free_ivector( int *v, int nl, int nh )
{
	if  (nh)  {}  /* just to touch nh */
	free((char*) (v+nl));
}

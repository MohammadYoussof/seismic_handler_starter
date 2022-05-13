
/* Numerical Recipes Functions */

#include <malloc.h>
#include <stdio.h>


/* Numerical Recipes standard error handler */

void nrerror (error_text)

char	error_text[];
{
	void exit();

	fprintf (stderr, "Numerical Recipes run-time error...\n");
	fprintf (stderr, "%s\n", error_text);
	fprintf (stderr, "...now exiting system...\n");
	exit(1);
}


/* Allocate a float vector with range [nl..nh] */

float	*vector(nl, nh)

int	nh, nl;
{
	float	*v;

	v = (float *)malloc ((unsigned) (nh-nl+1)*sizeof(float));
	if (!v) nrerror ("Allocation failure in vector()");
	return	v-nl;
}


/* Allocate an int vector with range [nl..nh] */

int	*ivector(nl, nh)

int	nh, nl;
{
	int	*v;

	v = (int *)malloc ((unsigned) (nh-nl+1)*sizeof(int));
	if (!v) nrerror ("Allocation failure in ivector()");
	return	v-nl;
}


/* Allocate a double vector with range [nl..nh] */

double	*dvector(nl, nh)

int	nh, nl;
{
	double	*v;

	v = (double *)malloc ((unsigned) (nh-nl+1)*sizeof(double));
	if (!v) nrerror ("Allocation failure in dvector()");
	return	v-nl;
}


/* Allocate a float matrix with range [nrl..nrh][ncl..nch] */

float	**matrix(nrl, nrh, ncl, nch)

int	nrl, nrh, ncl, nch;
{
	int	i;
	float	**m;

	/* Allocate pointers to rows */
	m = (float **)malloc ((unsigned) (nrh-nrl+1)*sizeof(float*));
	if (!m) nrerror ("Allocation failure 1 in matrix()");
	m[i] -= ncl;

	/* Allocate rows and set pointers to them */
	for (i = nrl; i <= nrh; i++)
	{
		m[i] = (float *)malloc ((unsigned) (nch-ncl+1)*sizeof(float));
		if (!m[i]) nrerror ("Allocation failure 2 in matrix()");
		m[i] -= ncl;
	}

	/* Return pointer to array of pointers to rows */

	return	m;
}


/* Allocate a double matrix with range [nrl..nrh][ncl..nch] */

double	**dmatrix(nrl, nrh, ncl, nch)

int	nrl, nrh, ncl, nch;
{
	int	i;
	double	**m;

	/* Allocate pointers to rows */
	m = (double **)malloc ((unsigned) (nrh-nrl+1)*sizeof(double*));
	if (!m) nrerror ("Allocation failure 1 in dmatrix()");
	m[i] -= ncl;

	/* Allocate rows and set pointers to them */
	for (i = nrl; i <= nrh; i++)
	{
		m[i] = (double *)malloc ((unsigned) (nch-ncl+1)*sizeof(double));
		if (!m[i]) nrerror ("Allocation failure 2 in dmatrix()");
		m[i] -= ncl;
	}

	/* Return pointer to array of pointers to rows */

	return	m;
}


/* Allocate an int matrix with range [nrl..nrh][ncl..nch] */

int	**imatrix(nrl, nrh, ncl, nch)

int	nrl, nrh, ncl, nch;
{
	int	i, **m;

	/* Allocate pointers to rows */
	m = (int **)malloc ((unsigned) (nrh-nrl+1)*sizeof(int*));
	if (!m) nrerror ("Allocation failure 1 in imatrix()");
	m[i] -= ncl;

	/* Allocate rows and set pointers to them */
	for (i = nrl; i <= nrh; i++)
	{
		m[i] = (int *)malloc ((unsigned) (nch-ncl+1)*sizeof(int));
		if (!m[i]) nrerror ("Allocation failure 2 in imatrix()");
		m[i] -= ncl;
	}

	/* Return pointer to array of pointers to rows */

	return	m;
}


/* Return with a sub-matrix of range 
 *	[newrl..newrl+(oldrh-oldrl)][newcl..newcl+(oldch-oldcl)]
 * pointing to the existing matrix range 
 *	a[oldrl..oldrh][oldcl..oldch]
 */

float	**submatrix (a, oldrl, oldrh, oldcl, oldch, newrl, newcl)

float	**a;
{
	int	i, j;
	float	**m;

	/* Allocate pointers to rows */
	m = (float **)malloc ((unsigned) (oldrh-oldrl+1)*sizeof(float*));
	if (!m) nrerror ("Allocation failure in submatrix()");
	m -= newrl;

	/* Set pointers to rows */
	for (i = oldrl, j = newrl; i <= oldrh; i++, j++)
		m[j] = a[i] + oldcl - newcl;

	/* Return pointer to array of pointers to rows */

	return	m;
}


/* Free a float vector allocated by vector() */

void free_vector (v, nl, nh)

float	*v;
int	nl, nh;
{
	free ((char*) (v + nl));
}


/* Free an int vector allocated by ivector() */

void free_ivector (v, nl, nh)

int	*v, nl, nh;
{
	free ((char*) (v + nl));
}


/* Free a double vector allocated by dvector() */

void free_dvector (v, nl, nh)

double	*v;
int	nl, nh;
{
	free ((char*) (v + nl));
}


/* Free a float matrix allocated by matrix() */

void free_matrix (m, nrl, nrh, ncl, nch)

float	**m;
int	nrl, nrh, ncl, nch;
{
	int i;

	for (i = nrh; i >= nrl; i--) free ((char*) (m[i] + ncl));
	free ((char*) (m + nrl));
}


/* Free a double matrix allocated by dmatrix() */

void free_dmatrix (m, nrl, nrh, ncl, nch)

double	**m;
int	nrl, nrh, ncl, nch;
{
	int i;

	for (i = nrh; i >= nrl; i--) free ((char*) (m[i] + ncl));
	free ((char*) (m + nrl));
}


/* Free an int matrix allocated by imatrix() */

void free_imatrix (m, nrl, nrh, ncl, nch)

int	**m;
int	nrl, nrh, ncl, nch;
{
	int i;

	for (i = nrh; i >= nrl; i--) free ((char*) (m[i] + ncl));
	free ((char*) (m + nrl));
}


/* Free a sub-matrix allocated by submatrix() */

void free_submatrix (b, nrl, nrh, ncl, nch)

float	**b;
int	nrl, nrh, ncl, nch;
{
	free ((char*) (b + nrl));
}


/* Allocate a float matrix m[nrl..nrh][ncl..nch] that points to the matrix, a,
 * declared in the standard C manner as a[nrow][ncol], where nrow = nrh-nrl+1
 * and ncol = nch-ncl+1.  The routine should be called with the address 
 * &a[0][0] as the first argument.
 */

float **convert_matrix (a, nrl, nrh, ncl, nch)

float	*a;
int	nrl, nrh, ncl, nch;
{
	int	i, j, nrow, ncol;
	float	**m;

	nrow = nrh - nrl + 1;
	ncol = nch - ncl + 1;

	/* Allocate pointers to rows */
	m = (float **) malloc ((unsigned) (nrow)*sizeof(float *));
	if (!m) nrerror ("Allocation failure in convert_matrix()");
	m -= nrl;
	for (i = 0, j = nrl; i <= nrow-1; i++, j++) m[j] = a + ncol*i - ncl;
	return	m;
}


/* Free a matrix allocated by convert_matrix() */

void free_convert_matrix (b, nrl, nrh, ncl, nch)

float	**b;
int	nrl, nrh, ncl, nch;
{
	free ((char*) (b + nrl));
}



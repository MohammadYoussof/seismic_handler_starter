
c NAME
c	dscal -- Scale a vector by a constant.

c FILE
c	dscal.f

c SYNOPSIS
c	LINPACK scalar constant times vector routine.

c DESCRIPTION
c	Subroutine.  Multiply a scalar constant, da, times a vector, dx()
c	of length n.

c	---- On entry ----
c	n:	Length of vector
c	incx:	x-increment loop counter (= 1, if entire loop accessed)
c	da  :	Scalar constant ultimately multiplied to dx()
c	dx():	Vector to which scalar is multiplied

c	---- On return ----
c	dx():	New scaled vector

c DIAGNOSTICS
c

c NOTES
c	Uses unrolled loops for increments equal to 1.

c SEE ALSO
c	LINPACK documentation by John Dongarra.

c AUTHOR
c	John Dongarra, March 1978.


      subroutine  dscal (n, da, dx, incx)
 
      implicit none   ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsdscal/ sccsid
      character*80 sccsid
      data sccsid /'@(#)dscal.f	44.1	9/20/91'/

c     ---- On entry ----

      integer*4 incx, n
      real*8    da

c     ---- On entry and return ----

      real*8    dx(1)

c     ---- Internal variables ----

      integer i, m, mp1, nincx
 

      if (n .le. 0)    return
      if (incx .eq. 1) goto 1010
 
c     Code for increment not equal to 1
 
      nincx = n*incx
      do 1000 i = 1, nincx, incx
 1000 dx(i) = da*dx(i)
      return
 
c     Code for increment equal to 1
 
 1010 m = mod(n,5)
      if (m .eq. 0) goto 1030
      do 1020 i = 1, m
 1020 dx(i) = da*dx(i)
      if (n .lt. 5) return
 1030 mp1 = m + 1
      do 1040 i = mp1, n, 5
         dx(i)     = da*dx(i)
         dx(i + 1) = da*dx(i + 1)
         dx(i + 2) = da*dx(i + 2)
         dx(i + 3) = da*dx(i + 3)
         dx(i + 4) = da*dx(i + 4)
 1040 continue

      return
      end


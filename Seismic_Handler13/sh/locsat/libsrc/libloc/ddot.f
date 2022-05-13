
c NAME
c	ddot -- Compute the dot product of two vectors.

c FILE
c	ddot.f

c SYNOPSIS
c	LINPACK routine to form the dot product of two vectors.

c DESCRIPTION
c	Function.  Given a two vectors dx() and dy() of length n, this
c	routine forms their dot product and returns as a scalar in ddot.

c	---- On entry ----
c	n:	Length of vector
c	incx:	x-storage increment counter (= 1, if entire loop accessed)
c	incy:	y-storage increment counter (= 1, if entire loop accessed)
c	dx():	First vector
c	dy():	Second vector 

c	---- On return ----
c	ddot:	Scalar dot product of dx() and dy()

c DIAGNOSTICS
c

c NOTES
c	Uses unrolled loops for increments equal to 1.

c SEE ALSO
c	LINPACK documentation by John Dongarra.

c AUTHOR
c	John Dongarra, March 1978.


      real*8 function ddot (n, dx, incx, dy, incy)
 
      implicit none   ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsddot/ sccsid
      character*80 sccsid
      data sccsid /'@(#)ddot.f	44.1	9/20/91'/


c     ---- On entry ----

      integer*4 incx, incy, n
      real*8    dx(1), dy(1)
 
c     ---- On return ----

c     real*8 function ddot

c     ---- Internal variables ----

      integer*4 i, ix, iy, m, mp1
      real*8    dtemp

      ddot  = 0.0d0
      dtemp = 0.0d0
      if (n .le. 0) return
      if (incx.eq.1 .and. incy.eq.1) goto 1010
 
c     Code for unequal increments or equal increments not equal to 1
 
      ix = 1
      iy = 1
      if (incx .lt. 0) ix = (-n+1)*incx + 1
      if (incy .lt. 0) iy = (-n+1)*incy + 1
      do 1000 i = 1, n
         dtemp = dtemp + dx(ix)*dy(iy)
         ix = ix + incx
         iy = iy + incy
 1000 continue
      ddot = dtemp
      return
 
c     Code for both increments equal to 1
 
 1010 m = mod(n,5)
      if (m .eq. 0) goto 1030
      do 1020 i = 1, m
 1020 dtemp = dtemp + dx(i)*dy(i)
      if ( n .lt. 5 ) goto 1050
 1030 mp1 = m + 1
      do 1040 i = mp1, n, 5
         dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     &                   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + 
     &                   dx(i + 4)*dy(i + 4)
 1040 continue
 1050 ddot = dtemp

      return
      end


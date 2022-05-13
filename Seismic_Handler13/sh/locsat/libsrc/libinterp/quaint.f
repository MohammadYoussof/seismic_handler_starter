
c NAME
c	quaint -- Monotone, quadratic interpolation with linear derivatives.

c FILE
c	quaint.f

c SYNOPSIS
c	Constrain derivative to be linear during monotone, quadratic 
c	interpolation.

c DESCRIPTION
c	Subroutine.  Perform monotone, quadratic interpolation of function 
c	f(x).  The interpolating function between two points is montone in 
c	value and linear in derivative.

c	---- Indexing ----
c	i = 1, n;

c	---- On entry ----
c	n:	Number of function samples
c	x(i):	Sample values of independent variable; must be ordered: 
c		x(i) >= x(i-1)
c	f(i):	Value of function at x(i)
c	x0:	Value of independent variable for interpolation

c	---- On return ----
c	f0:	Interpolated value of function at x0
c	fp0:	Interpolated value of derivative at x0
c	iext:	Flag indicating whether extrapolation has occurred;
c		  =  0,	No extrapolation
c		  = -1,	Yes, x0 < x(1)
c		  = +1,	Yes, x0 > x(N)

c	---- Subroutines called ----
c	Local
c		- Calls brack and hermit

c DIAGNOSTICS
c

c FILES
c

c NOTES
c	- f(x) may be discontinuous.  A discontinuity is presumed to occur
c	  when x(i) repeats for consecutive i.
c	- If x0 is out of range (iext = -1 or +1), then f0 and fp0 are 
c	  defined through linear extrapolation of function.

c SEE ALSO
c

c AUTHOR
c


      subroutine quaint (n, x, f, x0, f0, fp0, iext)

      implicit none     ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsquaint/ sccsid
      character*80 sccsid
      data sccsid /'@(#)quaint.f	44.1	9/20/91'/


c     ---- On entry ----

      integer*4 n
      real*4    f(n), x(n), x0

c     ---- On return ----

      integer*4 iext
      real*4    f0, fp0

c     ---- Internal variables ----

      integer*4 i1, i2, i3, i4, ileft
      real*4    f1, f2, f3, f4, fac, fp2, fp3, fpdev, fpdev2, fpdev3
      real*4    h12, h23, h34, s12, s23, s34, x1, x2, x3, x4


c     Binary search for samples bounding x0

      call brack (n, x, x0, ileft)

c     x0 < x(1)

      if (ileft.lt.1) then
         if (x(2).gt.x(1)) then
            fp0 = (f(2) - f(1)) / (x(2) - x(1))
         else
            fp0 = 0.0
         end if
         f0   = f(1) + fp0*(x0-x(1))
         iext = -1
         return
      end if

c     x0 > x(n)

      if (ileft.ge.n) then
         if (x(n).gt.x(n-1)) then
            fp0 = (f(n) - f(n-1)) / (x(n) - x(n-1))
         else 
            fp0 = 0.0
         end if
         f0 = f(n) + fp0*(x0-x(n))
         iext = +1
         return
      end if

c     Normal case

c     Define points 1..4, such that x1 <= x2 <= x0 <= x3 <= x4 ----
c     If necessary, make x1 = x2 or x3 = x4

      i1 = max(1,ileft-1)
      i2 = ileft
      i3 = ileft + 1
      i4 = min(n,ileft+2)

      x1 = x(i1)
      x2 = x(i2)
      x3 = x(i3)
      x4 = x(i4)

      f1 = f(i1)
      f2 = f(i2)
      f3 = f(i3)
      f4 = f(i4)

c     Find widths of three intervals
c     Note 'brack' guarantees x(ileft) < x(ileft+1), and thus h23 > 0

      h12 = x2 - x1
      h23 = x3 - x2
      h34 = x4 - x3

c     Set finite-difference derivative in center interval

      s23 = (f3 - f2) / h23

c     Assign a function derivative to point 2; call it fp2.  The derivative 
c     of the parabola fitting points 1, 2 and 3 c (evaluated at x2) is used,  
c     howvever, if h12 is zero, s23 is used.

      if (h12.gt.0.0) then
         s12 = (f2 - f1) / h12
         fp2 = (s23*h12 + s12*h23) / (h12 + h23)
      else
         fp2 = s23
      end if

c     Assign a function derivative to point 3; call it fp3.  The derivative 
c     of the parabola fitting points 2, 3 and 4 (evaluated at x3) is used,  
c     howvever, if h34 is zero, s23 is used.

      if (h34.gt.0.0) then
         s34 = (f4 - f3) / h34
         fp3 = (s23*h34 + s34*h23) / (h34 + h23)
      else
         fp3 = s23
      end if

c     Adjust fp2 and fp3 such that they average to s23, but neither gets 
c     farther from s23

      fpdev2 = s23 - fp2
      fpdev3 = fp3 - s23
      if (fpdev2*fpdev3.le.0.0) then
         fpdev = 0.0
      else if (fpdev2.lt.0.0) then
         fpdev = -min(-fpdev2,-fpdev3)
      else
         fpdev = min(fpdev2,fpdev3)
      end if

c     Adjust derivatives such that Hermite cubic interpolant is monotonic

      if (s23.ne.0.0) then
         fac = abs(fpdev/s23)
         if (fac.gt.1.0) fpdev = fpdev/fac
      end if
      fp2 = s23 - fpdev
      fp3 = s23 + fpdev

c     Now do a straight Hermite cubic interpolation bewteen points 2 and 3

      call hermit (x2, x3, f2, f3, fp2, fp3, x0, f0, fp0)

      iext = 0

      return
      end


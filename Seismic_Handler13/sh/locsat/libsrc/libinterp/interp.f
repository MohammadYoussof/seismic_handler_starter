 
c NAME
c	interp -- Hermite cubic interpolation.

c FILE
c	interp.f

c SYNOPSIS
c	Hermite cubic interpolation routine for function y(x).

c DESCRIPTION
c	Subroutine.  Hermitian cubic interpolation routine to act on a
c	function, y(x).

c	---- Indexing ----
c	i = 1, n;

c	---- On entry ----
c	n:	Number of function samples
c	x(i):	Sample values of independent variable
c	y(i):	Value of function at x(i); y(x(i))
c	yp(i):	Value of derivative at x(i); y'(x(i))
c	x0:	Value of independent variable for interpolation

c	---- On return ----
c	y0:	Interpolated value of function at x0
c	yp0:	Interpolated value of derivative at x0
c	ierr:	Error flag;
c		  = 0,	No error
c		  = 1,	x0 out of range. yp0 and y0 are then defined
c			through linear extrapolation of function

c	---- Subroutines called ----
c	Local
c		- Calls hermit

c DIAGNOSTICS
c

c FILES
c

c NOTES
c

c SEE ALSO
c

c AUTHOR
c

      subroutine interp (n, x, y, yp, x0, y0, yp0, ierr)

      implicit none      ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsinterp/ sccsid
      character*80 sccsid
      data sccsid /'@(#)interp.f	44.1	9/20/91'/


c     ---- On entry ----

      integer*4 n
      real*4    x0, x(n), y(n), yp(n)

c     ---- On return ----

      integer*4 ierr
      real*4    y0, yp0

c     ---- Internal variables ----

      integer*4 i1, i2, ileft


c     Binary search for samples bounding x0

      call brack (n, x, x0, ileft)

c     x0 < x(1)

      if (ileft.lt.1) then
         ierr = 1
         yp0  = yp(1)
         y0   = y(1) + yp0*(x0-x(1))
         return
      end if

c     x0 > x(n)

      if (ileft.ge.n) then
         ierr = 1
         yp0  = yp(n)
         y0   = y(n) + yp0*(x0-x(n))
         return
      end if

c     Normal case

      i1 = ileft
      i2 = ileft + 1
      call hermit (x(i1), x(i2), y(i1), y(i2), yp(i1), yp(i2),
     &             x0, y0, yp0)

      ierr = 0

      return
      end


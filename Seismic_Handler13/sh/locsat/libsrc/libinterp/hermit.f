
c NAME
c	hermit -- Two-point Hermite cubic interpolation routine.

c FILE
c	hermit.f

c SYNOPSIS
c	A simple two-point Hermitian cubic interpolation routine.

c DESCRIPTION
c	Subroutine.  Perform a Hermite cubic interpolation of function y(x) 
c	bewteen two sample points.

c	---- On entry ----
c	x1, x2:		Sample values of independent variable
c	y1, y2:		Values of function at x1 and x2, respectively
c	yp1, yp2:	Values of derivative of function at x1 and x2
c	x0:		Value of independent variable for interpolation

c	---- On return ----
c	y0:		Interpolated value of function at x0
c	yp0:		Interpolated value of derivative at x0

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


      subroutine hermit (x1, x2, y1, y2, yp1, yp2, x0, y0, yp0)

      implicit none     ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccshermit/ sccsid
      character*80 sccsid
      data sccsid /'@(#)hermit.f	44.1	9/20/91'/


c     ---- On entry ----

      real*4   x0, x1, x2, y1, y2, yp1, yp2

c     ---- On return ----

      real*4   y0, yp0

c     ---- Internal variables ----

      real*4   a, b, c, d, df, dx, f1, f2, fp1, fp2, sfp, t


      dx = x2 - x1
      t  = (x0 - x1) / dx
      if (t.le.0.5) then
         f1  = y1
         f2  = y2
         fp1 = yp1
         fp2 = yp2
      else
         t   = 1.0 - t
         dx  = -dx
         f1  = y2
         f2  = y1
         fp1 = yp2
         fp2 = yp1
      end if

      fp1 = fp1*dx
      fp2 = fp2*dx
      df  = f2 - f1
      sfp = fp1 + fp2
      a   = f1
      b   = fp1
      c   = 3.0*df - sfp - fp1
      d   = -2.0*df + sfp

      y0  = ((d*t + c)*t + b)*t + a
      yp0 = ( (3.0*d*t + 2.0*c)*t + b ) / dx

      return
      end


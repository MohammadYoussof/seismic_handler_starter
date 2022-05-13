
c NAME
c	splie2 -- Construct natural splines and second derivatives.

c FILE
c	splie2.f

c SYNOPSIS
c	Perform 1-D natural cubic splines on rows and return the second
c	derivatives.

c DESCRIPTION
c	Subroutine.  Given an m by n tabulated function ya(1..m,1..n), and 
c	tabulated independent variables x1a(1..m) and x2a(1..n), this 
c	routine constructs one-dimensional natural cubic splines of the 
c	rows of ya and returns the second derivatives in the array 
c	y2a(1..m,1..n).

c	---- Subroutines called ----
c		spline:	Return 2nd derivatives of an interpolating function

c DIAGNOSTICS
c	Values returned larger than 1.0e30 signal a natual spline.

c FILES
c

c NOTES
c

c SEE ALSO
c	Press, W.H. et al., 1988, "Numerical Recipes", 94-110.

c AUTHOR
c


      subroutine splie2 (x1a, x2a, ya, m, n, y2a)

      implicit none   ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccssplie2/ sccsid
      character*80 sccsid
      data sccsid /'@(#)bicube.f	44.1	9/20/91'/


c     ---- Parameter declarations ----

      integer*4 maxnode
      parameter (maxnode = 15)

c     ---- On entry ----

      integer*4 m, n
      real*4    x1a(m), x2a(n), ya(maxnode,n)

c     ---- On return ----

      real*4    y2a(maxnode,n)

c     ---- Internal variables ----

      integer*4 j, k
      real*4    ytmp(maxnode), y2tmp(maxnode)


      do 1020 j = 1, m
         do 1000 k = 1, n
            ytmp(k) = ya(j,k)
 1000    continue
         call spline (x2a, ytmp, n, 1.0e30, 1.0e30, y2tmp)
         do 1010 k = 1, n
            y2a(j,k) = y2tmp(k)
 1010    continue
 1020 continue
	
      return
      end 


c NAME
c	splin2 -- Return values of a bi-cubic interpolated function.

c FILE
c	splin2.f

c SYNOPSIS
c	Return an interpolated function value by bi-cubic interpolation.

c DESCRIPTION
c	Subroutine.  Given x1a, x2a, ya, m, n as described in subr. splie2 
c	and y2a as produced by that routine; and given a desired
c	interpolating point x1, x2; this routine returns an interpolated 
c	function value y by bi-cubic spline interpolation.

c	---- Subroutines called ----
c		spline: Return 2nd derivatives of an interpolating function
c		splint: Return a cubic spline interpolated value

c DIAGNOSTICS
c

c FILES
c

c NOTES
c	The above is accomplished by constructing row, then column splines,
c	one-dimension at a time.

c SEE ALSO
c	Press, W.H. et al., 1988, "Numerical Recipes", 94-110.

c AUTHOR
c


      subroutine splin2 (x1a, x2a, ya, y2a, m, n, x1, x2, y)

      implicit none   ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccssplin2/ sccsid
      character*80 sccsid
      data sccsid /'@(#)bicube.f	44.1	9/20/91'/


c     ---- Parameter declarations ----

      integer*4 maxnode
      parameter (maxnode = 15)

c     ---- On entry ----

      integer*4 m, n
      real*4    x1, x1a(m), x2, x2a(n), y2a(maxnode,n)
      real*4    ya(maxnode,n)

c     ---- On return ----

      real*4    y

c     ---- Internal variables ----

      integer*4 j, k
      real*4    y2tmp(maxnode), ytmp(maxnode), yytmp(maxnode)


      do 1010 j = 1, m
         do 1000 k = 1, n
            ytmp(k)  = ya(j,k)
            y2tmp(k) = y2a(j,k)
 1000    continue
         call splint (x2a, ytmp, y2tmp, n, x2, yytmp(j))
 1010 continue
      call spline (x1a, yytmp, m, 1.0e30, 1.0e30, y2tmp)
      call splint (x1a, yytmp, y2tmp, m, x1, y)

      return
      end


c NAME
c	spline -- Return the second derivatives of an interpolating function.

c FILE
c	spline.f

c SYNOPSIS
c	Construct an array of second derivatives based on an interpolating
c	function at given tabulated points.

c DESCRIPTION
c	Subroutine.  Given arrays x(1..n) and y(1..n) containing a tabulated
c	function, i.e., y(i) = f(x(i)), with x(1) < x(2) < x(n), and given
c	values yp1 and ypn for the first derivative of the interpolating
c	function at points 1 and n, respectively, this routine returns an
c	array y2(1..n) that contains the second derivatives of the
c	interpolating function at the tabulated points x(i).  

c DIAGNOSTICS
c	If yp1 and/or ypn are equal to 1.0e30 or larger, the routine is 
c	signalled to set the corresponding boundary condition for a natural 
c	spline, with zero second derivative on that boundary.

c FILES
c

c NOTES
c	Note that this routine only need be called once to process the
c	entire tabulated function in x and y arrays.

c SEE ALSO
c	Press, W.H. et al., 1988, "Numerical Recipes", 94-110.

c AUTHOR
c


      subroutine spline (x, y, n, yp1, ypn, y2)

      implicit none    ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsspline/ sccsid
      character*80 sccsid
      data sccsid /'@(#)bicube.f	44.1	9/20/91'/


c     ---- Parameter declarations ----

      integer*4 nmax
      parameter (nmax = 15)

c     ---- Internal variables ----

      integer*4 i, k, n
      real*4    p, qn, sig, u(nmax), un, x(n), y(n), y2(n), yp1, ypn


      if (yp1.gt.0.99e30) then
         y2(1) = 0.0
         u(1)  = 0.0
      else
         y2(1) = -0.5
         u(1)  = (3.0/(x(2) - x(1)))*((y(2) - y(1))/(x(2) - x(1)) - yp1)
      end if

c     Decomposition loop of a tridiagonal algorithm

      do 1000 i = 2, n-1
         sig = (x(i) - x(i-1))/(x(i+1) - x(i-1))
         p     = sig*y2(i-1) + 2.0
         y2(i) = (sig - 1.0)/p
         u(i)  = (6.0*((y(i+1) - y(i))/(x(i+1) - x(i)) - (y(i) - y(i-1))
     &           /(x(i) - x(i-1)))/(x(i+1) - x(i-1)) - sig*u(i-1))/p
 1000 continue

      if (ypn.gt.0.99e30) then
         qn = 0.0
         un = 0.0
      else
         qn = 0.5
         un = ( 3.0/(x(n) - x(n-1)) )
     &        * ( ypn - (y(n) - y(n-1))/(x(n) - x(n-1)) )
      end if
      y2(n) = (un - qn*u(n-1))/(qn*y2(n-1) + 1.0)

c     Back substituition loop of tridiagonal algorithm

      do 1010 k = n-1, 1, -1
         y2(k) = y2(k)*y2(k+1) + u(k)
 1010 continue
	
      return
      end


c NAME
c	splint -- Return a cubic spline interpolated value.

c FILE
c	splint.f

c SYNOPSIS
c	This routine return a cubic spline interpolated value from an array.

c DESCRIPTION
c	Subroutine.  Given the arrays xa(1..n) and ya(1..n) which tabulate
c	a function (with the xa(i)'s in order), and given the array 
c	y2a(1..n), which is the output from subroutine spline(), and given
c	a value of x, this routine returns a cubic-spline interpolated
c	value of y.

c DIAGNOSTICS
c

c FILES
c

c NOTES
c	The correct position in the table is obtained by means of bi-section.

c SEE ALSO
c	Press, W.H. et al., 1988, "Numerical Recipes", 94-110.

c AUTHOR
c


      subroutine splint (xa, ya, y2a, n, x, y)

      implicit none   ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccssplint/ sccsid
      character*80 sccsid
      data sccsid /'@(#)bicube.f	44.1	9/20/91'/


c     ---- Internal variables ----

      integer*4 k, khi, klo, n
      real*4    a, b, h, x, xa(n), y, ya(n), y2a(n)


      klo = 1
      khi = n

 1000 if (khi-klo.gt.1) then
         k = (khi + klo)/2
         if (xa(k).gt.x) then
            khi = k
         else
            klo = k
         end if
         goto 1000
      end if

c     klo and khi now bracket the input value of x

      h = xa(khi) - xa(klo)
      if (h.eq.0.0) then
         write (*,*) ' Splint: bad input error! '
         return
      end if

      a = (xa(khi) - x)/h
      b = (x - xa(klo))/h
      y = a*ya(klo) + b*ya(khi) + ( (a*(a*a - 1.0))*y2a(klo) 
     &    + (b*(b*b - 1.0))*y2a(khi) )*h*h/6.0

      return
      end


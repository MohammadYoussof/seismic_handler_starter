
c NAME
c	holin2 -- Monotone, quadratic interpolation routine.

c FILE
c	holin2.f

c SYNOPSIS
c	Perfrom a monotone, quadratic interpolation on a function which
c	may have holes.

c DESCRIPTION
c	Subroutine.  Monotone, quadratic interpolation of function f(x,y) 
c	which might have holes (bad values).  Bad function samples are
c	given the value, fbad (see below).

c	---- Indexing ----
c	i = 1, m;	j = 1, n;

c	---- On entry ----
c	m:	Number of x() samples
c	n:	Number of y() samples
c	x(i):	Sample values of x()
c	y(j):	Sample values of y()
c	f(i,j):	Value of function at (x(i), y(j))
c	ldf:	Leading dimension of array f()
c	fbad:	Function value denoting bad sample
c	x0:	Value of x() for interpolation
c	y0:	Value of y() for interpolation

c	---- On return ----
c	f0:	Interpolated value of function at (x0,y0)
c	fx0:	Interpolated value of x-derivative of function at (x0,y0)
c	fy0:	Interpolated value of y-derivative of function at (x0,y0)
c	fxy0:	Interpolated value of x-y-derivative of function at (x0,y0)
c	iext:	Error flag;  0, No error;  -1, x0 < x(1);  1, x0 > x(m)
c	jext:	Error flag;  0, No error;  -1, y0 < Y(1);  1, y0 > y(n)
c	ibad:	Flag indicating whether interopolation point is in a hole;
c		0, No;	1, Yes

c	---- Subroutines called ----
c	Local
c		- Calls brack, holint and quaint directly
c		- Calls brack, fixhol and hermit indirectly

c DIAGNOSTICS
c

c FILES
c

c NOTES
c	- If ibad = 1, f0 is set to fbad and the derivatrives to zero.
c	- If x0 or y0 is out of range (iext != 0 or jext != 0) then, 
c	  f0, fx0, fy0 and fxy0 are defined through linear function
c	  extrapolation.

c SEE ALSO
c

c AUTHOR
c


      subroutine holin2 (m, n, x, y, f, ldf, fbad, x0, y0, f0, fx0, fy0,
     &                   fxy0, iext, jext, ibad)

      implicit none
c     K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsholin2/ sccsid
      character*80 sccsid
      data sccsid /'@(#)holin2.f	44.1	9/20/91'/


c     ---- On entry ----

      integer*4 ldf, m, n
      real*4    f(ldf,n), fbad, x(m), x0, y(n), y0

c     ---- On return ----

      integer*4 ibad, iext, jext
      real*4    f0, fx0, fxy0, fy0

c     ---- Internal variables ----

      integer*4 ileft, imax, imin, j, jleft, jmax, jmin, js, muse, nuse
      real*4    f0s(4), fx0s(4)


c     Bracket x0 -- Find 4 relevant x() samples, or as many as needed

      call brack (m, x, x0, ileft)
      imin = max(1,ileft-1)
      imax = min(m,ileft+2)
      muse = imax - imin + 1

c     Do the same for y()

      call brack (n, y, y0, jleft)
      jmin = max(1,jleft-1)
      jmax = min(n,jleft+2)
      nuse = jmax - jmin + 1

c     Now interpolate to (x0, y(j)), j = jmin, jmax)

      do 100 j = jmin, jmax
         js = j - jmin + 1
100   call holint (muse, x(imin), f(imin,j), fbad, x0, f0s(js),
     &             fx0s(js), iext, ibad)

c     Now interpolate to (x0,y0)

      call holint (nuse, y(jmin), f0s, fbad, y0, f0, fy0, jext, ibad)
      if (ibad.gt.0) then
         fx0  = 0.0
         fxy0 = 0.0
      else
         call quaint (nuse, y(jmin), fx0s, y0, fx0, fxy0, jext)
      end if

c     Find minimum interpolated x-derivative

      return
      end


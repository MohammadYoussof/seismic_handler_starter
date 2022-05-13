
c NAME
c	holint -- Monotone, quadratic interpolation routine.

c FILE
c	holint.f

c SYNOPSIS
c	Perform a monotone, quadratic interpolation, even when holes in the
c	data are present.

c DESCRIPTION
c	Subroutine.  Monotone, quadratic interpolation of function f(x) 
c	which might have holes (bad values).   Bad function samples are 
c	given the value fbad.

c	---- Indexing ----
c	i = 1, n;

c	---- On entry ----
c	n:	Number of function samples
c	x(i):	Sample values of independent variable; Must be ordered: 
c		x(i) >= x(i-1)
c	f(i):	Value of function at x(i)
c	fbad:	Function value denoting bad sample
c	x0:	Value of independent variable for interpolation

c	---- On return ----
c	f0:	Interpolated value of function at x0
c	fp0:	Interpolated value of derivative at x0
c	iext:	Flag indicating whether extrapolation has occurred;
c		  =  0,	No extrapolation
c		  = -1,	Yes, x0 < x(1)
c		  = +1,	Yes, x0 > x(N)
c	ibad:	Flag indicating whether interopolation point is in a hole;
c		  =  0,	No
c		  =  1,	Yes

c	---- Subroutines called ----
c	Local
c		- Calls brack, fixhol and quaint, directly
c		- Calls brack and hermit, indirectly
c

c DIAGNOSTICS
c

c FILES
c

c NOTES
c	- f(x) may be discontinuous.  A discontinuity is presumed to occur
c	  when x(i) repeats for consecutive i.
c	- If x0 is out of range (iext = -1 or +1), then f0 and fp0 are 
c	  defined through linear extrapolation of function.
c	- If f(i) = fbad, then the function is assumed to be fbad in the
c	  intervals on either side of x(i), and then jump discontinuously
c	  to the next good sample value.  See subroutine fixhol for details
c	  on how holes are defined.
c	- If x0 is in a hole (ibad = 1), then f0 is returned fbad and fp0 
c	  is zero.

c SEE ALSO
c

c AUTHOR
c


      subroutine holint (n, x, f, fbad, x0, f0, fp0, iext, ibad)

      implicit none    ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsholint/ sccsid
      character*80 sccsid
      data sccsid /'@(#)holint.f	44.1	9/20/91'/


c     ---- On entry ----

      integer*4 n
      real*4    f(n), fbad, x(n), x0

c     ---- On return ----

      integer*4 ibad, iext
      real*4    f0, fp0

c     ---- Internal variables ----

      integer*4 ileft, imax, imin, nh, nuse
      real*4    fh(6), xh(6)


c     Find four relevant samples and then construct a version of this
c     piece of the function with holes fixed

      call brack (n, x, x0, ileft)
      imin = max(1,ileft-1)
      imax = min(n,ileft+2)
      nuse = imax - imin + 1
      call fixhol (nuse, x(imin), f(imin), fbad, nh, xh, fh)

c     Interpolate fixed function

      if (nh.le.1) then
         f0  = fh(1)
         fp0 = 0.0
      else
         call quaint (nh, xh, fh, x0, f0, fp0, iext)
      endif

c     Now check if interpolation point is in a hole

      if (f0.eq.fbad .and. fp0.eq.0.0) then
         ibad = 1
      else
         ibad = 0
      end if

      return
      end



c NAME
c	fixhol -- Deal with bad values (holes) in function.

c FILE
c	fixhol.f

c SYNOPSIS
c	When "holes" in a given function are found, look for "good" samples.

c DESCRIPTION
c	Subroutine.  Fix up the sampled function f(x) to allow for "holes"
c	with bad values.  A bad function value at a sample point x(i) is 
c	assumed to occur when the function sample f(i) has the value fbad.  
c	For interpolation purposes, the function is assumed then to be fbad 
c	in the intervals surrounding x(i) up to the next "good" samples, 
c	where a jump discontinuity is assumed to occur.

c	Given the original function samples -- x(i), f(i), i = 1, m -- this 
c	routine creates a new set of samples -- xs(i), fs(j), j = 1, ms --
c	in which the intervals of bad values and discontinuities between
c	good and bad values are explicitly sampled.  To create a 
c	discontinuity at a given point, the point is included as two 
c	samples with different function values.

c	---- Example ----
c	x =  0.0  2.0   3.0  7.0  8.5  12.0  14.5  18.0  19.0  20.5 21.5  22.0
c	f =  2.3  1.1  fbad  7.6  4.5  fbad  fbad  12.1  fbad   6.2  4.3  fbad

c	xs =  0.0  2.0   2.0  7.0  7.0   8.5   8.5  20.5 20.5  21.5  21.5
c	fs =  2.3  1.1  fbad fbad  7.6   4.5  fbad  fbad  6.2   4.3  fbad

c	---- Indexing ----
c	i = 1, m;	j = 1, ms;

c	---- On entry ----
c	m:	Number of x() samples
c	x(i):	Sample values of x(); must be ordered
c	f(i):	Value of function at x(i)
c	fbad:	Function value signifying that f(x) is not well-defined (bad)

c	---- On return ----
c	ms:	Number of new x() samples; May be as large as 1 + (4*m)/3
c	xs(j):	New sample values of x()
c	fs(j):	New value of function at x(j)

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


      subroutine fixhol (m, x, f, fbad, ms, xs, fs)

      implicit none    ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsfixhol/ sccsid
      character*80 sccsid
      data sccsid /'@(#)fixhol.f	44.1	9/20/91'/


c     ---- On entry ----

      integer*4 m
      real*4    f(m), fbad, x(m)

c     ---- On return ----

      integer*4 ms
      real*4    fs(*), xs(*)

c     ---- Internal variables ----

      integer*4 i


c     Trivial case
 
      if (m.le.0) then
         ms = 0
         return
      end if

c     Set up first point

      ms = 1
      xs(1) = x(1)
      fs(1) = f(1)

c     Do the rest

      do 1010 i = 2, m
         if (f(i).ne.fbad) then
            if (fs(ms).ne.fbad) then
               if (x(i).eq.xs(ms)) then
                  if (f(i).eq.fs(ms)) go to 1010
               end if
            else
               if (ms.gt.1) ms = ms + 1
               xs(ms) = x(i)
               fs(ms) = fbad
            end if
            ms     = ms + 1
            xs(ms) = x(i)
            fs(ms) = f(i)
                else
            if (fs(ms).ne.fbad) then
               if (ms.gt.1) then
                  if (fs(ms-1).eq.fbad) then
                     ms = max(1,ms-2)
                     go to 1010
                  end if
               end if
               ms     = ms + 1
               xs(ms) = xs(ms-1)
               fs(ms) = fbad
            end if
         end if
 1000    if (ms.gt.2) then
            if (xs(ms).eq.xs(ms-2)) then
               fs(ms-1) = fs(ms)
               ms       = ms - 1
               go to 1000
            end if
         end if
 1010 continue

      return
      end


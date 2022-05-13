
c NAME
c	brack -- Bracket an array of interpolative values.

c FILE
c	brack.f

c SYNOPSIS
c	Using bi-section, brack an array of interpolative values by 
c	performing a binary search.

c DESCRIPTION
c	Subroutine.  Perform a binary search to find those elements of 
c	array x() that bracket x0.  Given the array x(i), i = 1,.,N, in 
c	non-decreasing order, and given the number x0, this routine finds 
c	ileft from 0..n, such that (pretend x(0) = -infinity, 
c	x(n+1) = +infinity):

c		x(ileft) <= x0 <= x(ileft+1)
c		x(ileft) < x(ileft+1)

c	Note that x() may contain duplicate values, but ileft will still 
c	point to a non-zero interval.

c	---- On entry ----
c	n:	Dimension of input vector (array), x()
c	x(n):	One-dimensional input array of values to be bracketed
c	x0:	Value being compared against

c	---- On return ----
c	ileft:	Left bracketed indice

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


      subroutine brack (n, x, x0, ileft)
 
      implicit none     ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsbrack/ sccsid
      character*80 sccsid
      data sccsid /'@(#)brack.f	44.1	9/20/91'/

c     ---- On entry ----

      integer*4 n
      real*4    x(n), x0

c     ---- On return ----

      integer*4 ileft

c     ---- Internal variables ----

      integer*4 i, imid, iright


c     Initialize

      ileft  = 0
      iright = n + 1

 1000 imid = (ileft+iright)/2
      if (imid.eq.ileft) then
         return
      else if (x0.lt.x(imid)) then
         iright = imid
         goto 1000
      else if (x0.gt.x(imid)) then
         ileft = imid
         goto 1000
      end if 
 
c     Special case: The point x(imid) found to equal x0.  Find bracket 
c     [x(ileft),x(ileft+1)], such that x(ileft+1) > x(ileft).

      do 1010 i = imid+1, n
      if (x(i).gt.x0) then
         ileft = i-1
         return
      end if
 1010 continue

      do 1020 i = imid-1, 1, -1
      if (x(i).lt.x0) then
         ileft = i
         return
      end if
 1020 continue

      ileft = 0

      return
      end


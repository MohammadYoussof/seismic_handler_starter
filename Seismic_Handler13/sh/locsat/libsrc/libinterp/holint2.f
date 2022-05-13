
c NAME
c	holint2 -- Hybrid monotone, quadratic inter/extrapolation routine.

c FILE
c	holint2.f

c SYNOPSIS
c	Perfrom a monotone, quadratic interpolation on a function which
c	may have holes.

c DESCRIPTION
c	Subroutine.  Monotone, quadratic interpolation of function f(x,y) 
c	which might have holes (bad values).  Bad function samples are
c	linearly extrapolated as necessary (see below).

c	---- Indexing ----
c	i = 1, m;	j = 1, n;

c	---- On entry ----
c	phase_id:	Phase type ID
c	do_extrap:	Do we want to extrapolate data (0 = TRUE; 1 = FALSE)
c	m:		Number of x() samples
c	n:		Number of y() samples
c	x(i):		Sample values of x()
c	y(j):		Sample values of y()
c	f(i,j):		Value of function at (x(i), y(j))
c	ldf:		Leading dimension of array f()
c	fbad:		Function value denoting bad sample
c	x0:		Value of x() for interpolation
c	y0:		Value of y() for interpolation

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
c	Walter Nagy, September 1991.


      subroutine holint2 (phase_id, do_extrap, m, n, x, y, f, ldf, fbad,
     &                    x0, y0, f0, fx0, fy0, fxy0, idist, idepth,
     &                    ihole)

      implicit none    ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsholint2/ sccsid
      character*80 sccsid
      data sccsid /'@(#)holint2.f	44.2	10/31/91'/


c     ---- On entry ----

      integer*4 do_extrap, ldf, m, n, phase_id
      real*4    f(ldf,n), fbad, x(m), x0, y(n), y0

c     ---- On return ----

      integer*4 idepth, idist, ihole
      real*4    f0, fx0, fxy0, fy0

c     ---- Internal variables ----

      integer*4 i, ibad, ichk, iext, ileft, imax, imin, j, jext, jleft
      integer*4 jmax, jmin, js, k, max_idx, min_idx, muse, nuse
      integer*4 num_samples
      integer*4 extrap_distance, extrap_in_hole
      real*4    f0s(4), fx0s(4), subgrid(181,4)
      real*4    dist_max, dist_min, hold, tt_max, tt_min, vel


      iext   = 0
      jext   = 0
      ibad   = 0
      ihole  = 0
      idist  = 0
      idepth = 0
      num_samples = ldf

c     Should we extrapolate ?

      if ((x0.gt.x(m) .or. x0.lt.x(1)) .and. do_extrap.ne.1) then
         f0 = -1.0
         return
      end if

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

c     Fill in subgrid with valid times where available and fill-in empty 
c     parts of the desired curve with linearly extrapolated values.  For
c     travel-time tables x(i) contains the distance elements, while y(j) 
c     holds the depth samples.

      do 1120 j = jmin, jmax
         do 1110 i = imin, imax
            if (f(i,j).eq.-1.0) then 
               if (do_extrap.ne.1) then
                  f0 = -1.0
                  return
               end if
               extrap_in_hole  = ihole
               extrap_distance = idist
               do 1000 min_idx = 1, num_samples
 1000          if (f(min_idx,j).ne.-1.0) goto 1010
 1010          dist_min = x(min_idx)
               ichk = 0
               do 1020 max_idx = min_idx, num_samples
                  if (f(max_idx,j).eq.-1.0) then
                     if (ichk.eq.0) ichk = max_idx-1
                  else if (max_idx.eq.num_samples) then
                     ichk = max_idx
                  else
                     ichk = 0
                  end if
 1020          continue
               max_idx  = ichk
               dist_max = x(max_idx)

c              Off the high end ?
               if (x(i).gt.dist_max) then
                  do 1030 k = max_idx, 1, -1
 1030             if ((dist_max - x(k)).ge.5.0) goto 1040
 1040             tt_max = f(max_idx,j)
                  vel = (dist_max - x(k)) / (tt_max - f(k,j))
                  if (dist_max.le.110.0 .and. x(i).gt.110.0) then
                     hold = ((110.0 - dist_max)/vel) + tt_max + 238.0
                     vel  = 2.4*vel
                     subgrid(i,j) = ((x(i) - 110.0)/vel) + hold
                  else
                     subgrid(i,j) = ((x(i) - dist_max)/vel) + tt_max
                  end if
                  extrap_distance = 1

c              Off the low end ?
               else if (x(i).lt.dist_min) then
                  do 1050 k = min_idx, num_samples
 1050             if ((x(k) - dist_min).ge.5.0) goto 1060
 1060             tt_min = f(min_idx,j)
                  vel = (x(k) - dist_min) / (f(k,j) - tt_min)
                  subgrid(i,j) = tt_min - ((dist_min - x(i)) / vel)
                  extrap_distance = -1

c              In a hole ?
               else
                  do 1070 k = max_idx, 1, -1
                     if (x(k).lt.x(i)) then
			if (f(k,j).ne.-1.0) then
                           dist_max = x(k)
                           max_idx  = k
                           goto 1080
			end if
                     end if
 1070             continue
 1080             do 1090 k = max_idx, 1, -1
 1090             if ((dist_max - x(k)).ge.5.0) goto 1100
 1100             tt_max = f(max_idx,j)
                  vel = (dist_max - x(k)) / (tt_max - f(k,j))
                  subgrid(i,j) = ((x(i) - dist_max) / vel) + tt_max
                  extrap_in_hole = 1

               end if

               ihole  = extrap_in_hole
               idist  = extrap_distance
            else
               subgrid(i,j) = f(i,j)
            end if
 1110    continue
 1120 continue
      if (y0.gt.y(jmax)) idepth = 1

c     Now interpolate to (x0, y(j)), j = jmin, jmax)

      do 1130 j = jmin, jmax
         js = j - jmin + 1
         call holint (muse, x(imin), subgrid(imin,j), fbad, x0, f0s(js),
     &                fx0s(js), iext, ibad)
 1130 continue

c     Now interpolate to (x0,y0)

      call holint (nuse, y(jmin), f0s, fbad, y0, f0, fy0, jext, ibad)
c     if (ibad.gt.0) then
c        fx0  = 0.0
c        fxy0 = 0.0
c     else
         call quaint (nuse, y(jmin), fx0s, y0, fx0, fxy0, jext)
c     end if

      iext = idist
      jext = idepth
      ibad = ihole

      return
      end


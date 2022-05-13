
c NAME
c	slocal0 -- Compute horizontal slownesses and partial derivatives.

c FILE
c	slocal0.f

c SYNOPSIS
c	Compute horizontal slownesses and their partial derivatives for
c	a fixed hypocenter.

c DESCRIPTION
c	Subroutine.  Slownesses and their partials are determined via 
c	inter/extrapolation of pre-calculated curves.  A point in a hole
c	is rejected.


c       ---- On entry ----
c	radius:		Radius of Earth (km)
c	delta:		Distance from the event to the station (deg)
c	azi:		Forward-azimuth from event to station (deg)
c	zfoc:		Event focal depth (km below sea level)
c	maxtbd:		Maximum dimension of i'th position in tbd(), tbtt()
c	maxtbz:		Maximum dimension of j'th position in tbz(), tbtt()
c	ntbd:		Number of distance samples in tables
c	ntbz:		Number of depth samples in tables
c	tbd(i):		Distance samples for tables (deg)
c	tbz(j):		Depth samples in tables (km)
c	tbtt(i,j):	Travel-time tables (sec)

c	---- On return ----
c	dcalx:	Calculated slownesses (sec/deg)
c	atx(4):	Partial derivatives (sec/km/deg)
c	iterr:	Error code for n'th observation
c		=  0, No problem, normal interpolation
c		= 11, Distance-depth point (x0,z0) in hole of T-T curve
c		= 12, x0 < x(1)
c		= 13, x0 > x(max)
c		= 14, z0 < z(1)
c		= 15, z0 > z(max)
c		= 16, x0 < x(1) and z0 < z(1)
c		= 17, x0 > x(max) and z0 < z(1)
c		= 18, x0 < x(1) and z0 > z(max)
c		= 19, x0 > x(max) and z0 > z(max)
c	[NOTE:	If any of these codes are negative (e.g., iderr = -17),
c		then, the datum was used to compute the event location]

c	---- Subroutines called ----
c	From libinterp
c		brack:		Bracket travel-time data via bisection
c		holint2:	Quadratic interpolation function

c DIAGNOSTICS
c	Currently assumes a constant radius Earth (i.e., it ignores
c	ellipicity of the Earth.

c NOTES
c	Future plans include dealing with ellipicity and higher-order
c	derivatives.

c SEE ALSO
c	Subroutines azcal0, and ttcal0 are parallel routines for computing
c	azimuthal and travl-time partial derivatives, respectively.

c AUTHOR
c	Steve Bratt, December 1988.


      subroutine slocal0 (phase_id, zfoc, radius, delta, azi,
     &                    maxtbd, maxtbz, ntbd, ntbz, tbd, tbz, tbtt,
     &                    dcalx, atx, iterr)
 
      implicit none    ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsslocal0/ sccsid
      character*80 sccsid
      data sccsid /'@(#)slocal0.f	44.2	10/31/91'/


c     ---- Parameter declarations ----

      integer*4 maxparm
      real*4    dtor

c     Convert radians to degrees
      parameter (dtor = 0.017453293)

c     Max. number of permissable parameters
      parameter (maxparm = 4)

c     ---- On entry ----

      integer*4 maxtbd, maxtbz, ntbd, ntbz, phase_id
      real*4    azi, delta, radius, tbd(maxtbd), tbtt(maxtbd,maxtbz)
      real*4    tbz(maxtbz), zfoc

c     ---- On return ----

      integer*4 iterr
      real*4    dcalx
      real*8    atx(maxparm)

c     ---- Internal variables ----

      integer*4 i, ibad, idist, idepth, iext, ihole, ileft, imax, imin
      integer*4 itotd, itotz, j, jext, jleft, jmax, jmin, jz, nz
      integer*4 do_extrap
      real*4    dcross, dslddel, dsldz, dtdz, slow, tbds(4), tbsls(4,4)
      real*4    tbzs(4), ttime
      real*8    azir, cosazi, sinazi

 
      ihole  = 0
      idist  = 0
      idepth = 0

c     Permit extrapolation

      do_extrap = 0

c     Form arrays holding distances and depths around point of interest

      call brack (ntbd, tbd, delta, ileft)
      imin = max(1, ileft-1)
      imax = min(ntbd, ileft+2)

      itotd = 0
      do 1000 i = imin, imax
         itotd       = itotd + 1
         tbds(itotd) = tbd(i)
 1000 continue

      call brack (ntbz, tbz, zfoc, jleft)
      jmin = max(1, jleft-1)
      jmax = min(ntbz, jleft+2)

      itotz = 0
      do 1010 j = jmin, jmax
         itotz       = itotz + 1
         tbzs(itotz) = tbz(j)
 1010 continue

c     Compute travel time and horizontal slownesses for each point in 
c     arrays around the point of interest

c     Find relevant range of table depths
 
      do 1030 i = 1, itotd
         do 1020 j = 1, itotz
            call brack (ntbz, tbz, zfoc, ileft)
            jz = max(1, ileft-1)
            nz = min(ntbz, ileft+2) - jz + 1
 
c           Return travel time and create a mini-table of partials

            call holint2 (phase_id-1, do_extrap, ntbd, nz, tbd, tbz(jz),
     &                    tbtt(1,jz), maxtbd, -1.0, tbds(i), tbzs(j),
     &                    ttime, slow, dtdz, dcross, iext, jext, ibad)
            if (ibad.ne.0) ihole  = ibad
            if (iext.ne.0) idist  = iext
            if (jext.ne.0) idepth = jext
c           if (ibad.ne.0) then
c              tbsls(i,j) = -1.0
c           else
               tbsls(i,j) = slow
c           end if
 1020    continue
 1030 continue

c     Compute slowness and partials at point of interest from mini-table

      call holint2 (phase_id-1, do_extrap, itotd, itotz, tbds, tbzs,
     &              tbsls, 4, -1.0, delta, zfoc, slow, dslddel, dsldz,
     &              dcross, iext, jext, ibad)

      if (ihole.ne.0)  ibad = ihole
      if (idist.ne.0)  iext = idist
      if (idepth.ne.0) jext = idepth

c     Interpolate point in hole of curve -- Value no good
      if (ibad.ne.0) then
         iterr = 11

c     Interpolate point less than first distance point in curve
      else if (iext.lt.0 .and. jext.eq.0) then
         iterr = 12

c     Interpolate point greater than last distance point in curve
      else if (iext.gt.0 .and. jext.eq.0) then
         iterr = 13

c     Interpolate point less than first depth point in curve
      else if (iext.eq.0 .and. jext.lt.0) then
         iterr = 14

c     Interpolate point greater than last depth point in curve
      else if (iext.eq.0 .and. jext.gt.0) then
         iterr = 15

c     Interpolate point less than first distance point in curve and less
c     than first depth point in curve
      else if (iext.lt.0 .and. jext.lt.0) then
         iterr = 16

c     Interpolate point greater than last distance point in curve and less 
c     than first depth point in curve
      else if (iext.gt.0 .and. jext.lt.0) then
         iterr = 17

c     Interpolate point less than first distance point in curve and 
c     greater than first depth point in curve
      else if (iext.lt.0 .and. jext.gt.0) then
         iterr = 18

c     Interpolate point greater than last distance point in curve and
c     greater than first depth point in curve
      else if (iext.gt.0 .and. jext.gt.0) then
         iterr = 19

c     Reset error code to 0 if valid table interpolation
      else 
         iterr = 0
      end if

c     Compute partial derivatives if point is not in a hole

      dcalx   = slow 
      dslddel = dslddel / (dtor*(radius - zfoc))

      if (ibad.eq.0) then
         azir   = azi*dtor
         sinazi = sin(azir)
         cosazi = cos(azir)

c        Axis 1
         atx(1) = 0.0

c        Axis 2 points east
         atx(2) = -dslddel * sinazi

c        Axis 3 points north
         atx(3) = -dslddel * cosazi

c        Axis 4 points up
         atx(4) = -dsldz
      end if

      return
      end


c NAME
c	slocal1 -- Compute horizontal slownesses and partial derivatives.

c FILE
c	slocal1.f

c SYNOPSIS
c	Compute horizontal slownesses and their partial derivatives for
c	a fixed hypocenter.

c DESCRIPTION
c	Subroutine.  Slownesses and their partials are determined via 
c	inter/extrapolation of pre-calculated curves.  A point in a hole
c	is rejected.


c       ---- On entry ----
c	radius:		Radius of Earth (km)
c	delta:		Distance from the event to the station (deg)
c	azi:		Forward-azimuth from event to station (deg)
c	zfoc:		Event focal depth (km below sea level)
c	maxtbd:		Maximum dimension of i'th position in tbd(), tbtt()
c	maxtbz:		Maximum dimension of j'th position in tbz(), tbtt()
c	ntbd:		Number of distance samples in tables
c	ntbz:		Number of depth samples in tables
c	tbd(i):		Distance samples for tables (deg)
c	tbz(j):		Depth samples in tables (km)
c	tbtt(i,j):	Travel-time tables (sec)

c	---- On return ----
c	dcalx:	Calculated slownesses (sec/deg)
c	atx(4):	Partial derivatives (sec/km/deg)
c	iterr:	Error code for n'th observation
c		=  0, No problem, normal interpolation
c		= 11, Distance-depth point (x0,z0) in hole of T-T curve
c		= 12, x0 < x(1)
c		= 13, x0 > x(max)
c		= 14, z0 < z(1)
c		= 15, z0 > z(max)
c		= 16, x0 < x(1) and z0 < z(1)
c		= 17, x0 > x(max) and z0 < z(1)
c		= 18, x0 < x(1) and z0 > z(max)
c		= 19, x0 > x(max) and z0 > z(max)
c	[NOTE:	If any of these codes are negative (e.g., iderr = -17),
c		then, the datum was used to compute the event location]

c	---- Subroutines called ----
c	From libinterp
c		brack:	Bracket travel-time data via bisection
c		holin2:	Quadratic interpolation function

c DIAGNOSTICS
c	Currently assumes a constant radius Earth (i.e., it ignores
c	ellipicity of the Earth.

c NOTES
c	Future plans include dealing with ellipicity and higher-order
c	derivatives.

c SEE ALSO
c	Subroutines azcal0, and ttcal0 are parallel routines for computing
c	azimuthal and travl-time partial derivatives, respectively.

c AUTHOR
c	Steve Bratt, December 1988.


      subroutine slocal1 (zfoc, radius, delta, azi, maxtbd, maxtbz,
     &                    ntbd, ntbz, tbd, tbz, tbtt, dcalx, atx, iterr)
 
      implicit none   ! K.S. 1-Dec-97, changed 'undefined' to 'none'


c     ---- Parameter declarations ----

      integer*4 maxparm
      real*4    dtor

c     Convert radians to degrees
      parameter (dtor = 0.017453293)

c     Max. number of permissable parameters
      parameter (maxparm = 4)

c     ---- On entry ----

      integer*4 maxtbd, maxtbz, ntbd, ntbz
      real*4    azi, delta, radius, tbd(maxtbd), tbtt(maxtbd,maxtbz)
      real*4    tbz(maxtbz), zfoc

c     ---- On return ----

      integer*4 iterr
      real*4    dcalx
      real*8    atx(maxparm)

c     ---- Internal variables ----

      integer*4 i, ibad, iext, ileft, imax, imin, itotd, itotz, j, jext
      integer*4 jleft, jmax, jmin, jz, nz
      real*4    dcross, dslddel, dsldz, dtdz, slow, tbds(4), tbsls(4,4)
      real*4    tbzs(4), ttime
      real*8    azir, cosazi, sinazi

 
c     Form arrays holding distances and depths around point of interest

      call brack (ntbd, tbd, delta, ileft)
      imin = max(1, ileft-1)
      imax = min(ntbd, ileft+2)

      itotd = 0
      do 1000 i = imin, imax
         itotd       = itotd + 1
         tbds(itotd) = tbd(i)
 1000 continue

      call brack (ntbz, tbz, zfoc, jleft)
      jmin = max(1, jleft-1)
      jmax = min(ntbz, jleft+2)

      itotz = 0
      do 1010 j = jmin, jmax
         itotz       = itotz + 1
         tbzs(itotz) = tbz(j)
 1010 continue

c     Compute travel time and horizontal slownesses for each point in 
c     arrays around the point of interest

c     Find relevant range of table depths
 
      do 1030 i = 1, itotd
         do 1020 j = 1, itotz
            call brack (ntbz, tbz, zfoc, ileft)
            jz = max(1, ileft-1)
            nz = min(ntbz, ileft+2) - jz + 1
 
c           Return travel time and create a mini-table of partials

            call holin2 (ntbd, nz, tbd, tbz(jz), tbtt(1,jz), maxtbd,
     &                   -1.0, tbds(i), tbzs(j), ttime, slow, dtdz,
     &                   dcross, iext, jext, ibad)
            if (ibad.ne.0) then
               tbsls(i,j) = -1.0
            else
               tbsls(i,j) = slow
            end if
 1020    continue
 1030 continue

c     Compute slowness and partials at point of interest from nimi-table

      call holin2 (itotd, itotz, tbds, tbzs, tbsls, 4, -1.0, delta,
     &             zfoc, slow, dslddel, dsldz, dcross, iext, jext, ibad)

c     Interpolate point in hole of curve -- Value no good
      if (ibad.ne.0) then
         iterr = 11

c     Interpolate point less than first distance point in curve
      else if (iext.lt.0 .and. jext.eq.0) then
         iterr = 12

c     Interpolate point greater than last distance point in curve
      else if (iext.gt.0 .and. jext.eq.0) then
         iterr = 13

c     Interpolate point less than first depth point in curve
      else if (iext.eq.0 .and. jext.lt.0) then
         iterr = 14

c     Interpolate point greater than last depth point in curve
      else if (iext.eq.0 .and. jext.gt.0) then
         iterr = 15

c     Interpolate point less than first distance point in curve and less
c     than first depth point in curve
      else if (iext.lt.0 .and. jext.lt.0) then
         iterr = 16

c     Interpolate point greater than last distance point in curve and less 
c     than first depth point in curve
      else if (iext.gt.0 .and. jext.lt.0) then
         iterr = 17

c     Interpolate point less than first distance point in curve and 
c     greater than first depth point in curve
      else if (iext.lt.0 .and. jext.gt.0) then
         iterr = 18

c     Interpolate point greater than last distance point in curve and
c     greater than first depth point in curve
      else if (iext.gt.0 .and. jext.gt.0) then
         iterr = 19

c     Reset error code to 0 if valid table interpolation
      else 
         iterr = 0
      end if

c     Compute partial derivatives if point is not in a hole.  The local 
c     cartesian coordinate system is such that

      if (ibad.eq.0) then
         azir   = azi*dtor
         sinazi = sin(azir)
         cosazi = cos(azir)

         dcalx   = slow 
         dslddel = dslddel / (dtor*(radius - zfoc))

c        Axis 1
         atx(1) = 0.0

c        Axis 2 points east
         atx(2) = -dslddel * sinazi

c        Axis 3 points north
         atx(3) = -dslddel * cosazi

c        Axis 4 points up
         atx(4) = -dsldz
      end if

      return
      end


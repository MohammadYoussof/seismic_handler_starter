
c NAME
c	ssscor -- Apply source-specific station correction adjustments.

c FILE
c	ssscor.f

c SYNOPSIS
c	Correct for source-specific station effects in the observed travel
c	times for a given, phase-type/correction-type/station/region group.

c DESCRIPTION
c	Subroutine.  Apply source-specific station corrections here to 
c	partially compensate for lateral velocity heterogeneities.  
c	Distinct corrections exist for every applicable phase-type,
c	correction-type (travel time, azimuth, slowness), station and 
c	region (regional or local).

c	---- On entry ----
c	elat:	Event latitude (decmimal degrees)
c	elon:	Event longitude (decmimal degrees)
c	itype:	Type code for n'th observation
c		  = 1, Arrival time datum
c		  = 2, Azimuth datum
c		  = 3, Slowness datum
c	ista:	Station index for n'th detection
c	iwav:	Wave index for n'th detection

c	---- On return ----
c	correct:	Source-specific station correction (sec)

c DIAGNOSTICS
c	Will complain if incompatabilities are encountered.

c FILES
c	None.

c NOTES
c	Many arrays are passed in common block, corrs, via file, 'rdcor.h'.

c SEE ALSO
c	Source-specific station correction read routine, rdcortab().

c AUTHOR
c	Walter Nagy, October 1990.


      subroutine ssscor (elat, elon, correct, itype, ista, iwav)

      implicit none   ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsssscor/ sccsid
      character*80 sccsid
      data sccsid /'@(#)ssscor.f	44.1	9/20/91'/

      include 'rdcor.h'


c     ---- On entry ----

      integer*4 ista, itype, iwav
      real*4    elat, elon

c     ---- On return ----

      real*4    correct

c     ---- Internal variables ----

      integer*4 i, iarea, ibeg, icnt, iend, ilat, ilon, is, is2, isrc
      integer*4 isrc2, j, ncnt, nlat, nlon
      real*4    dlat, dlon, x1a(maxnode), x2a(maxnode), xln1, xln2
      real*4    y2a(maxnode, maxnode), ya(maxnode, maxnode)


c     Check if this station/correction-type exists

      is = indexstacor(itype,ista,1)
      if (is.eq.0) return

c     Check if this station/correction-type/phase-type exists

      iend = cumulsrcs(itype,is,1,iwav)
      if (iend.eq.0) return
      ibeg = iend - numsrcs(itype,is,1,iwav) + 1

c     Now determine whether event is located within one of the regional
c     source regions -- If not, return w/o a correction

      do 1000 isrc = ibeg, iend
         xln1 = xlon1(itype,is,1,isrc)
         xln2 = xlon2(itype,is,1,isrc)
         dlon = elon
         if (elat.lt.xlat1(itype,is,1,isrc) .and.
     &       elat.gt.xlat2(itype,is,1,isrc)) then
            if (xln1.lt.0.0) xln1 = 360.0 + xln1
            if (xln2.lt.0.0) xln2 = 360.0 + xln2
            if (elon.lt.0.0) dlon = 360.0 + elon
            if (dlon.gt.xln1 .and. dlon.lt.xln2) goto 1010
         end if
 1000 continue
      return

c     Regional source region found, now look for a local source region

 1010 iarea = 1

      is2 = indexstacor(itype,ista,2)
      if (is2.eq.0) goto 1040

      iend = cumulsrcs(itype,is2,2,iwav)
      if (iend.eq.0) goto 1040
      ibeg = iend - numsrcs(itype,is2,2,iwav) + 1

      do 1020 isrc2 = ibeg, iend
         xln1 = xlon1(itype,is2,2,isrc2)
         xln2 = xlon2(itype,is2,2,isrc2)
         if ( elat.lt.xlat1(itype,is2,2,isrc2) .and.
     &        elat.gt.xlat2(itype,is2,2,isrc2) ) then
            if (xln1.lt.0.0) xln1 = 360.0 + xln1
            if (xln2.lt.0.0) xln2 = 360.0 + xln2
            if (elon.lt.0.0) dlon = 360.0 + elon
            if (dlon.gt.xln1 .and. dlon.lt.xln2) then
               is    = is2
               isrc  = isrc2
               iarea = 2
               goto 1040
            end if
         end if
 1020 continue

c     Now determine a regional/local source-specific station correction

 1040 icnt = isrc - 1
      dlat = xlat1(itype,is,iarea,isrc) - elat
      dlon = elon - xlon1(itype,is,iarea,isrc)
      if (dlon.lt.0.0) dlon = dlon + 360.0
      nlat = nlats(itype,is,iarea,isrc)
      nlon = nlons(itype,is,iarea,isrc)
      ncnt = nodecumul(itype,is,iarea,isrc)
      ilat = icnt*(nlat-1)
      ilon = icnt*(nlon-1)

      x1a(1) = 0.0
      x2a(1) = 0.0
      do 1050 i = 2, nlat
 1050 x1a(i) = x1a(i-1) + splat(itype,is,iarea,ilat+i-1)
      do 1060 i = 2, nlon
 1060 x2a(i) = x2a(i-1) + splon(itype,is,iarea,ilon+i-1)
      do 1080 i = 1, nlat
         do 1070 j = 1, nlon
 1070    ya(i,j) = float(stacor(itype,is,iarea,ncnt+j))/100.0
         ncnt = ncnt + nlon
 1080 continue

c     Calculate 2nd derivatives

      call splie2 (x1a, x2a, ya, nlat, nlon, y2a)

c     Determine bi-cubic interpolated value at point (dlat, dlon)

      call splin2 (x1a, x2a, ya, y2a, nlat, nlon, dlat, dlon, correct)

      return
      end


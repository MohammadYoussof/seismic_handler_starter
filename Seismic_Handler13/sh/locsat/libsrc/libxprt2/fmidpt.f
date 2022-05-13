c
c     NAME
c       fmidpt  -   Computes the mid-points between a set of
c                   three of input locations.
c
c     FILE
c       fmidpt.f
c
c     SYNOPSIS
c       Given three sets of latitude and longitude points, midpt
c       computes the latitudes and longitudes of points midway 
c       between the input points.
c
c       Values are returned in single float arrays of dimension (1)
c       so that they may be accepted by Lisp.
c       
c
c     DESCRIPTION
c       function fmidpt (slat1,slon1,slat2,slon2,slat3,slon3,
c                        plat1,plon1,plat2,plon2,plat3,plon3,ierr)
c
c       Input:
c         slat1 = latitude of 1st input point.
c         slon1 = longitude of 1st input point.
c         slat2 = latitude of 2cd input point.
c         slon2 = longitude of 2cd input point.
c         slat3 = latitude of 3rd input point.
c         slon3 = longitude of 3rd input point.
c
c       Output:
c         plat1(1) = latitude of mid-point between inputs 1 and 2.
c         plon1(1) = longitude of mid-point between inputs 1 and 2.
c         plat2(1) = latitude of mid-point between inputs 2 and 3.
c         plon2(1) = longitude of mid-point between inputs 2 and 3.
c         plat3(1) = latitude of mid-point between inputs 3 and 1.
c         plon3(1) = longitude of mid-point between inputs 3 and 1.
c         ierr(1) = error code: = 0 if no problems.
c     			        = 1 if problems.
c
c       Subroutines called by fmidpt:
c         distaz = distance and azimuth from one point to another.
c         latlon  = computes lat and lon of point B given location
c                   of point A and distance and azimuth 
c                   from point A to point B.
c
c     DIAGNOSTICS
c        none
c
c     FILES
c        none
c
c     NOTES
c
c
c     SEE ALSO
c        none
c
c     AUTHOR
c       Steve Bratt
c
c     #END
c
c
c
      function fmidpt (slat1,slon1,slat2,slon2,slat3,slon3,
     &                 plat1,plon1,plat2,plon2,plat3,plon3,ierr)
 
      dimension plat1(1),plon1(1),plat2(1),plon2(1)
      dimension plat3(1),plon3(1),ierr(1)
	common /sccsfmidpt/ sccsid
	character*80 sccsid
	data sccsid /'@(#)fmidpt.f	36.1	3/6/89'/

c Initialize

      fmidpt = 0.0
      plat1(1) = -999.
      plon1(1) = -999.
      plat2(1) = -999.
      plon2(1) = -999.
      plat3(1) = -999.
      plon3(1) = -999.
      ierr(1) = 0
  
      if (abs(slat1).gt.90. .or. abs(slon1).gt.180. .or.
     &    abs(slat2).gt.90. .or. abs(slon2).gt.180. .or.
     &    abs(slat3).gt.90. .or. abs(slon3).gt.180.) then
	  ierr(1) = 1
	  return
      endif

      call distaz (slat1,slon1,slat2,slon2,dist,az,baz)
      call latlon (slat1,slon1,dist/2.,az,plat1(1),plon1(1))
      call distaz (slat2,slon2,slat3,slon3,dist,az,baz)
      call latlon (slat2,slon2,dist/2.,az,plat2(1),plon2(1))
      call distaz (slat3,slon3,slat1,slon1,dist,az,baz)
      call latlon (slat3,slon3,dist/2.,az,plat3(1),plon3(1))
      fmidpt = 1.0
      return
      end

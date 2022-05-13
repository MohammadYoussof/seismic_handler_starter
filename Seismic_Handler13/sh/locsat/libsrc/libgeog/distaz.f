      subroutine distaz (alat1,alon1,alat2,alon2, delta,azi,baz)
c
c Calculate angular distance, azimuth and backazimuth between two
c  points on a sphere.
c
c Input
c
c   ALAT1,ALON1  =  latitude and longitude of point 1.
c   ALAT2,ALON2  =  latitude and longitude of point 2.
c
c Output
c
c   DELTA  =  angular distance between points 1 and 2.
c   AZI    =  azimuth from north of point 2 w.r.t. point 1.
c   BAZ    =  azimuth from north of point 1 w.r.t. point 2.
c
c All arguments are in degrees.
c Latitude, longitude and DELTA are geocentric.
c Latitude is zero at equator and positive north.
c Longitude is positive toward the east.
c AZI and BAZ are positive and measured clockwise from local north.
c
      implicit double precision (a-h,o-z)  ! K.S. 1-Dec-97, moved 1 line up
	common /sccsdistaz/ sccsid
      real alat1,alat2,alon1,alon2,delta,azi,baz
	character*80 sccsid

      parameter
     . (pi = 3.14159 26535 89793 d0, rtod = 1.8d2/pi, dtor = pi/1.8d2)
	data sccsid /'@(#)distaz.f	43.1	9/9/91'/
c
c If we are given the same point twice, don't do the heavy work
c (et) 1/24/89
c
      if ((alat1 .ne. alat2) .or. (alon1 .ne. alon2)) goto 10
      delta = 0.0
      azi = 0.0
      baz = 180.0
      return
c Continue with original code
c
 10   rlat1 = dtor*alat1
      rlat2 = dtor*alat2
      rdlon = dtor*(alon2-alon1)
c
      clat1 = dcos(rlat1)
      clat2 = dcos(rlat2)
      slat1 = dsin(rlat1)
      slat2 = dsin(rlat2)
      cdlon = dcos(rdlon)
      sdlon = dsin(rdlon)
c
      cdel = slat1 * slat2 + clat1 * clat2 * cdlon
      cdel = min(cdel,1.d0)
      cdel = max(cdel,-1.d0)
      yazi = sdlon * clat2
      xazi = clat1 * slat2 - slat1 * clat2 * cdlon
      ybaz = -sdlon * clat1
      xbaz = clat2 * slat1 - slat2 * clat1 * cdlon
c
      delta = rtod * dacos(cdel)
      azi = rtod * datan2(yazi,xazi)
      baz = rtod * datan2(ybaz,xbaz)
      if (azi.lt.0.) azi=azi+360.
      if (baz.lt.0.) baz=baz+360.
c
      return
      end

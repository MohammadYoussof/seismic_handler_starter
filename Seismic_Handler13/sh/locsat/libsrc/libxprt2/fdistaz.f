      function fdistaz (alat1,alon1,alat2,alon2, delta,azi,baz)
c
c Calculate angular distance, azimuth and backazimuth between two
c  points on a sphere.
c
c Input
c
c   alat1,alon1  =  latitude and longitude of point 1.
c   alat2,alon2  =  latitude and longitude of point 2.
c
c Output
c
c   delta(1)  =  angular distance between points 1 and 2.
c   azi(1)    =  azimuth from north of point 2 w.r.t. point 1.
c   baz(1)    =  azimuth from north of point 1 w.r.t. point 2.
c
c All arguments are in degrees.
c Latitude, longitude and distance (delta) are geocentric.
c Latitude is zero at equator and positive north.
c Longitude is positive toward the east.
c azi and baz are positive and measured clockwise from local north.
c
      implicit double precision (a-h,o-z)  ! K.S. 1-Dec-97, moved up 2 lines
	dimension delta(1),azi(1),baz(1)
	common /sccsfdistaz/ sccsid
      real alat1,alat2,alon1,alon2,delta,azi,baz
	character*80 sccsid

      parameter
     . (pi = 3.14159 26535 89793 d0, rtod = 1.8d2/pi, dtor = pi/1.8d2)
	data sccsid /'@(#)fdistaz.f	38.1	9/11/89'/
c
      fdistaz = 0.0
      rlat1 = dtor*alat1
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
      delta(1) = rtod * dacos(cdel)
      azi(1) = rtod * datan2(yazi,xazi)
      baz(1) = rtod * datan2(ybaz,xbaz)
      if (azi(1).lt.0.) azi(1) = azi(1) + 360.
      if (baz(1).lt.0.) baz(1) = baz(1) + 360.
      fdistaz = 1.0
c
      return
      end

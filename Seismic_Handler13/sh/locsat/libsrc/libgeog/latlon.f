      subroutine latlon (alat1,alon1,delta,azi, alat2,alon2)
c
c Find a point on a sphere which is a given distance and azimuth
c  away from another point.
c
c Input
c
c   ALAT1,ALON1  =  latitude and longitude of point 1.
c   DELTA  =  angular distance between points 1 and 2.
c   AZI    =  azimuth from north of point 2 w.r.t. point 1.
c
c Output
c
c   ALAT2,ALON2  =  latitude and longitude of point 2.
c
c Subroutines called
c
c   cart
c   rotate
c   geog
c
c All arguments are in degrees.
c Latitude, longitude and DELTA are geocentric.
c Latitude is zero at equator and positive north.
c Longitude is positive toward the east.
c AZI is measured clockwise from local north.
c
	common /sccslatlon/ sccsid
      dimension x(3)
	character*80 sccsid
	data sccsid /'@(#)latlon.f	43.1	9/9/91'/

c
      call cart (90.0-delta,180.0-azi,0.0,1.0, x)
      call rotate (90.0-alat1,0.0,x)
      call geog (x,1.0, alat2,dlon,z)
      alon2 = alon1+dlon
      if (abs(alon2).gt.180.) alon2 = -sign(360.-abs(alon2),alon2)
c
      return
      end

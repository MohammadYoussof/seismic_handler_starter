      subroutine cart (alat,alon,z,radius, x)
c
c Convert a geographical location to geocentric cartesian coordinates,
c
c  changed by johannes schweitzer for geographical coordinates
c     mar 17, 1992
c    (willmore (1979): manual of seismological practice)
c
c########### ( assuming a spherical earth.)
c
c The cartesian axis are such that
c     - Axis 1 intersects equator at 90 deg longitude (east)
c     - Axis 2 intersects north pole
c     - Axis 3 intersects equator at  0 deg longitude
c
c Input
c
c   ALAT  =  latitude (degrees)
c   ALON  =  longitude (degrees)
c   Z     =  depth
c   RADIUS  =  radius of the earth
c
c Output
c
c   X(1:3)  =  vector of geocentric cartesian coordinates
c              axis 1 intersects equator at  0 deg longitude  
c              axis 2 intersects equator at 90 deg longitude  
c              axis 3 intersects north pole
c
	common /sccscart/ sccsid
	character*80 sccsid

      parameter (dtor = 1./57.2957795)
      dimension x(3)
	data sccsid /'@(#)cart.f	43.1	9/9/91'/
c
c
c     transform geographical coodinates in geocentric (spherical) 
c     coordinates
c
      esq=(1.-1./298.25)**2.
      alat2=atan(tan(dtor*alat)*esq)
      r123 = radius - z
      r13 = r123*cos(alat2)
      x(1) = r13*sin(dtor*alon)
      x(2) = r123*sin(alat2)
      x(3) = r13*cos(dtor*alon)
c
      return
      end

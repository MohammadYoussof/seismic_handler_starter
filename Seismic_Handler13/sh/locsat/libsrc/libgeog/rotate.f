      subroutine rotate (alat,alon,x)
c
c Rotate a 3-vector represented in cartesian coordinates.
c
c The cartesian coordinate system is most easily described
c  in geographic terms.  The origin is at the earth's center.
c  The axes are such that
c     - Axis 1 intersects equator at 90 deg longitude (east)
c     - Axis 2 intersects north pole
c     - Axis 3 intersects equator at  0 deg longitude
c
c On input, the vector to be rotated has components X(i),i=1,2,3
c  in this system.  This procedure rotates the vector
c  in the following two steps:
c    1. Rotation by ALON degrees westward, about the 2-axis.
c    2. Rotation by ALAT degrees southward,about the 1-axis.
c
c On output, X contains the coordinates of the rotated vector.
c
c Another way to interpret X on output:  as the components
c  of the original vector in a rotated coordinate system.
c  Put yourself on the earth's surface at the point having
c  latitude ALAT and longitude ALON.  Call this point P.
c  This rotated system is also geocentric, in which
c     - Axis 1 points east
c     - Axis 2 points north
c     - Axis 3 points up
c  where east, north and up are the directions local to P.
c
	common /sccsrotate/ sccsid
	character*80 sccsid

      parameter (rtod = 57.2957795)
      dimension x(3)
	data sccsid /'@(#)rotate.f	43.1	9/9/91'/
c
c Do it
c
      alatr = alat/rtod
      alonr = alon/rtod
      sinlat = sin(alatr)
      coslat = cos(alatr)
      sinlon = sin(alonr)
      coslon = cos(alonr)
c
      a = x(1) * coslon - x(3) * sinlon
      b = x(2)
      c = x(1) * sinlon + x(3) * coslon
      x(1) = a
      x(2) = b * coslat - c * sinlat
      x(3) = b * sinlat + c * coslat
c
      return
      end

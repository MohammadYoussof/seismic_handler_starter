c Given the locations of two reference points and two azimuths of 
c  great circles passing through those points, the 
c  program computes the location of the crossing of two great circles
c   and the distances from two reference points.
c   -- by Steve Bratt and Donna Williams, June 1986.
c
      subroutine azcros(alat1,alon1,aza,alat2,alon2,azb,
     *  dista,distb,alat,alon,ierr)
c 
c  INPUT:
c  alat1,alon1,alat2,alon2:  locations of reference points
c  aza,azb: azimuths of great circles passing through points points
c           1 and 2, respectively
c  OUTPUT:
c  dista,distb:  distance from points 1 and 2 to point 
c                great circles cross
c  alat,alon:  location of crossing point.
c
c  All distances, azimuths, and locations are input and output
c    in degrees.
c
c  ierr : = 0 all O.K., = 1 lines do not cross within reasonable
c         distance.
c
c  SUBROUTINES CALLED:
c    distaz 
c    latlon, which calls cart 
c                        rotate 
c                        geog
c
c
	common /sccsazcros/ sccsid
	character*80 sccsid
	data sccsid /'@(#)azcros.f	43.1	9/9/91'/

      degtrad=2*asin(1.0)/180.0
c
c Find azimuth, back azimuth and radial distance between
c  stations.
c
      call distaz(alat1,alon1,alat2,alon2,delta,azi,baz)
c
c Find sign (clockwise = +) and value of angle measured from line
c   between two stations to aza and azb.
c
      ra = aza - azi
      if(abs(ra).gt.180.) ra = -sign((360.-abs(ra)),ra)
      rb = azb - baz
      if(abs(rb).gt.180.) rb = -sign((360.-abs(rb)),rb)
c
c If the signs of ra and rb are the same, the great circles along
c  those azimuths will not cross within a "reasonable" distance.
c
      if(sign(1.,ra).eq.sign(1.,rb)) then
	ierr = 1
	return
      endif
c
      ra = abs(ra)
      rb = abs(rb)
c
c If the sum of ra and rb is greater than 180., there will be no 
c  crossing within a reasonable distance.
c
      if((ra+rb).gt.180.) then
	ierr = 1
	return
      endif
c
      ra = ra*degtrad
      rb = rb*degtrad
      rc = delta*degtrad
c
        c1=tan(0.5*rc)
        c2=(ra-rb)*0.5
        c3=(ra+rb)*0.5
c
c          equations for solving for the distances
c
        f=(c1)*sin(c2)
        g=sin(c3)
        h=cos(c2)*(c1)
        e=cos(c3)
c
        c4=atan(f/g)
        c5=atan(h/e)
c
c       Compute distances (lengths of the triangle)
c
        distb=(c4+c5)/degtrad
        dista=(c5-c4)/degtrad
c
	if(dista.lt.0.0.or.distb.lt.0.0) then
	  ierr = 1
	  return
        endif
c
	if(dista.lt.distb) then
	  dist = dista
	  az = aza
	  alatin = alat1
	  alonin = alon1
	  goto 50
        endif
	  dist = distb
	  az = azb
	  alatin = alat2
	  alonin = alon2
50      call latlon(alatin,alonin,dist,az,alat,alon)
	ierr = 0
	return
	end

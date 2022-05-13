c	subroutine rec2pol(lat,lon,latR,lonR,dis,azi)
c
c CREATION DATE: 	9. January 1992
c
c
c		C H A N G E  L O G
c 
c      Date     |       Name     |         Description
c---------------|----------------|-------------------------------------
c 09-01-92      |     Henger	 |  Version 1.0
c---------------|----------------|-------------------------------------
c
c
	subroutine rec2pol(lat,lon,latR,lonR,dis,azi)
c	
c	Program to determine distance and azimuth of a
c	point with coordinates (lat,lon) with respect to
c 	a reference point with coordinates (latR,lonR)
c
c	Distance 'dis' and azimuth 'azi' (counted clockwise
c       from north) are calculated on the basis of non-spheric
c	geometry.
c
c	lat,lon,latR,lonR are given in degrees
c
c-------------------------------------------------------------------
c
	implicit integer (a-z)
	real lat,lon,latR,lonR,dis,azi,r,deg,x,y
	data r/6378.388/, deg/111.1774734/

	y = lat - latR
	x = lon - lonR
	if(x.eq.0..and.y.eq.0.) then			! points identical
	   dis = 0.
	   azi = 0.
	else if(abs(y).lt.1.0e-6.and.x.gt.0.)then	! 90 degrees
	   azi = 90.	   	  
	   dis = abs(x)*deg
	else if(abs(y).lt.1.0e-6.and.x.lt.0.)then	! 270 degrees
	   azi = 270.
	   dis = abs(x)*deg   
	else if(abs(x).lt.1.e-6.and.y.gt.0.) then	! 0 degrees
               	azi = 0.
		dis = abs(y)*deg
	else if(abs(x).lt.1.e-6.and.y.lt.0.) then	! 180 degrees
		azi = 180.
		dis = abs(y)*deg
	else if(abs(x).ge.1.e-6.and.abs(y).ge.1.e-6) then
	   dis = sqrt(x*x+y*y)*deg
           azi = atan2(abs(lat-latR),abs(lon-lonR))
	   azi = azi*180./3.141592654
	   if(x.gt.0..and.y.gt.0.) then			! 1. Quadrant
		azi = 90. - azi
	   else if(x.gt.0..and.y.lt.0.) then		! 2. Quadrant
		azi = 90. + azi
	   else if(x.lt.0..and.y.lt.0.) then	! 3. Quadrant
		azi = 270. - azi
	   else if(x.lt.0..and.y.gt.0.) then	! 4. Quadrant
		azi = 270. + azi
	   endif
	endif
	return
	end

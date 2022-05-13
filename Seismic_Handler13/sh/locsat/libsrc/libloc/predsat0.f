
c NAME
c	predsat0 -- Predict Slowness, Azimuth and Travel-time bounds.

c FILE    
c	predsat0.f
 
c SYNOPSIS
c	Computes maximun and minimum bounds on the travel-time slowness, 
c	and azimuth for a set of data (station-phase-datatype) based on 
c	an event location, confidence ellipse and travel-time curves.
 
c DESCRIPTION
c	Subroutine.  Information on travel-time tables, stations, event 
c	and confidence parameters, the desired stations and phases are 
c	passed to and from PredSAT via the argument list.  The phase and 
c	station names given for each datum (dstaid,dwavid) must match in 
c	the phase and station lists (staid,wavid).  If all that you desire 
c	are the predictions from a point, just input 0.0 for all of the 
c	confidence bounds parameters.  

c	---- Indexing ----
c	i = 1, nsta;	j = 1, nwav;	k = 1, ntbd(j);	m = 1, ntbz(j);
c	n = 1, ndata;

c	---- On entry ----
c	nsta:		Number of stations
c	nwav:		Number of phases in list
c	maxtbd:		Maximum dimension of k'th position in tbd(), tbtt()
c	maxtbz:		Maximum dimension of m'th position in tbz(), tbtt()

c	outfile:	Output file name
c	dstaidx:	Name of station for which prediction to be made  
c	dwavidx:	Name of phase for which prediction to be made
c	staid(i):	List of all station names
c	stalat(i):	Station latitudes (deg)
c	stalon(i):	Station longitudes (deg)
c	wavid(j):	List of accepted phases for arrival times

c	ntbd(j):	Number of distance samples in travel-time tables
c	ntbz(j):	Number of depth samples in travel-time tables
c	tbd(k,j):	Distance to k'th lat/lon travel-time node (deg)
c	tbz(m,j):	Depth to m'th travel-time node (km)
c	tbtt(k,m,j):	Travel-time of k'th lat/lon and m'th depth nodes (sec)

c	sloflg:		Compute slowness bounds (y or n)
c	prtflg:		Verbose printout (y or n)
c	alat:		Event latitude used for prediction (deg)
c	alon:		Event longitude used for prediction (deg)
c	zfoc:		Event focal depth used for prediction (km)
c	epmaj:		Length of semi-major axis for confidence ellipse (km)
c	epmin:		Length of semi-minor axis for confidence ellipse (km)
c	epstr:		Strike of semi-major axis for confidence ellipse (deg)
c	zfint:		Length of focal depth confidence semi-interval (km)
c	toint:		Length of O.T. confidence semi-interval (sec)
c	luout:		Output logical unit number

c	---- On return ----
c	distmin:	Minimum distance from the ellipse to the dstaidx (deg)
c	distmax:	Maximum distance from the ellipse to the dstaidx (deg)
c	distcntr:	Distance from the center of ellipse to the dstaidx (deg)
c	ttmin:  	Minimum travel-time for dwavidx from the ellipse to the
c			dstaidx (sec)
c	ttmax:  	Maximum travel-time for dwavidx from the ellipse to the
c			dstaidx (sec)
c	ttcntr: 	Travel-time for dwavidx from the center of the 
c			ellipse to the dstaidx (sec)
c	azmin:  	Minimum azimuth from dstaidx to the to the station (deg)
c	azmax:  	Maximum azimuth from dstaidx ellipse (deg)
c	azcntr: 	Maximum azimuth from dstaidx to the center of the 
c			ellipse (deg)
c	slomin: 	Minimum slowness for dwavidx from the ellipse to the
c			dstaidx (sec/deg)
c	slomax: 	Maximum slowness for dwavidx from the ellipse to the
c			dstaidx (sec/deg)
c	slocntr:	Slowness for dwavidx from the center of ellipse to the
c			dstaidx (sec/deg)
c	iterrmin, iterrmax, iterrcntr: Error codes for travel-times 
c	iserrmin, iserrmax, iserrcntr: Error codes for slowness
c			  =  0,	No problem, normal prediction
c			  =  1,	No station information for datum
c			  =  2,	No travel-time tables for datum
c			  = 11,	Interpolation point in hole of travel-time curve
c			  = 12,	Interpolation point less than first distance
c				point in curve
c			  = 13,	Interpolation point greater than last 
c				distance point in curve
c			  = 14,	Interpolation point less than first depth 
c				point in curve
c			  = 15,	Interpolation point greater than last depth 
c				point in curve
c			  = 16,	Interpolation point less than first distance 
c				point in curve and less than first depth 
c				point in curve
c			  = 17,	Interpolation point greater than last 
c				distance point in curve and less than first 
c				depth point in curve
c			  = 18,	Interpolation point less than first distance 
c				point in curve and greater than first depth 
c				point in curve
c			  = 19,	Interpolation point greater than last 
c				distance point in curve and greater than 
c				first depth point in curve

c	---- Subroutines called ----
c	Local
c		ttcal1: 	Compute travel-times and their partials
c		slocal1:	Compute horizontal slownesses and partials

c	---- Functions called ----
c	From libgeog
c		distaz2:	Determine the distance between between two
c				lat./lon. pairs
c		latlon2:	Compute a second lat./lon. from first
c				distance and an azimuth
 
c DIAGNOSTICS
c	Complains when input data are bad ...
 
c FILES
c	None.
 
c NOTES
c	None.
 
c SEE ALSO
c	Bratt and Bache (1988) Locating events with a sparse network of
c       regional arrays, BSSA, 78, 780-798.
 
c AUTHOR
c	Steve Bratt, December 1988.


      subroutine predsat0 (dstaidx, dwavidx, idaridx, staid, stalat,
     &                     stalon, nsta, wavid, nwav, maxtbd, maxtbz,
     &                     ntbd, ntbz, tbd, tbz, tbtt, sloflg, prtflg,
     &                     luout, outfile, alat, alon, zfoc, epmaj,
     &                     epmin, epstr, zfint, toint, distmin, distmax,
     &                     distcntr, ttmin, ttmax, ttcntr, azmin, azmax,
     &                     azcntr, slomin, slomax, slocntr, iterrmin,
     &                     iterrmax, iterrcntr, iserrmin, iserrmax,
     &                     iserrcntr)

      implicit none    ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccspredsat0/ sccsid
      character*80 sccsid
      data sccsid /'@(#)predsat0.f	44.1	9/20/91'/


c     ---- Parameter declarations ----

      real*4 deg2km, deg2rad

c     Convert degrees to kilometers for lat./lon. conversion
      parameter (deg2km = 111.195) 

c     Convert degrees to radians
      parameter (deg2rad = 0.017453293)

c     ---- On entry ----

      integer*4 maxtbd, maxtbz, nsta, nwav, idaridx, luout, ntbd(nwav)
      integer*4 ntbz(nwav)
      real*4    alat, alon, epmaj, epmin, epstr, stalat(nsta)
      real*4    stalon(nsta), tbz(maxtbz,nwav), tbd(maxtbd,nwav)
      real*4    tbtt(maxtbd,maxtbz,nwav), toint, zfint, zfoc
      character*(*) wavid(nwav), dwavidx, staid(nsta), dstaidx, prtflg
      character*(*) sloflg, outfile

c     ---- On return ----

      integer*4 iserrcntr, iserrmax, iserrmin, iterrcntr, iterrmax
      integer*4 iterrmin
      real*4    azcntr, azmax, azmin, distcntr, distmax, distmin
      real*4    slocntr, slomax, slomin, ttcntr, ttmax, ttmin

c     ---- Internal variables ----

      integer*4 i, ios, iserr(3), ista, iterr(3), k, k1, k2, kwav
      integer*4 lnblnk, n, ntimes
      real*4    aztt(3), dcalx, dist(3), radius, z(3)
      real*8    angle, atx(3), azdiff, azim(3), azimuth, azneg, azpos
      real*8    baz, distance, e, epj, epn, plat(4), plon(4), scal(3)
      real*8    stalt, staln, tcal(3), x, xp, y, yp
      real*8    a1, a2, azim1, dist1, epr
      logical   lprt(19)


      radius	= 6371.0
      tcal(1)	= -999.0
      tcal(2)	=  999.0
      tcal(3)	= -999.0
      scal(1)	= -999.0
      scal(2)	=  999.0
      scal(3)	= -999.0
      azim(2)	=  999.0
      azim(3)	= -999.0
      dist(2)	=  999.0
      dist(3)	= -999.0
      ttcntr	= -999.0
      slocntr	= -999.0
      distmin	= -999.0
      ttmin	= -999.0
      azmin	= -999.0
      slomin	= -999.0
      distmax	= -999.0
      ttmax	= -999.0
      azmax	= -999.0
      slomax	= -999.0
      azneg	=  999.0
      azpos	= -999.0
      iterr(1)	= 0
      iterr(2)	= 0
      iterr(3)	= 0
      iserr(1)	= 0
      iserr(2)	= 0
      iserr(3)	= 0
      n       	= 0

      a1	= alat
      a2	= alon

c     Open file
 
      open (luout, file = outfile, status = 'unknown', 
     &      access = 'append', iostat = ios)

c     Check that datum's station and phase are valid. If so, assign pointer

      do 1000 i = 1, nsta
         k1 = lnblnk(dstaidx)
         k2 = lnblnk(staid(i))
         if (dstaidx(1:k1).eq.staid(i)(1:k2)) goto 1010
 1000 continue

      write (luout, '(3a)') '? PredSAT: Station ', dstaidx(1:k1),
     &                      ' unknown'
      iterr(1) = 1
      iterr(2) = 1
      iterr(3) = 1
      goto 1060
 1010 ista = i

      do 1020 k = 1, nwav
         k1 = lnblnk(dwavidx)
         k2 = lnblnk(wavid(k))
         if (dwavidx(1:k1).eq.wavid(k)(1:k2)) goto 1030
 1020 continue
      write (luout, '(3a)') '? PredSAT: Wave ', dwavidx(1:k1),
     &                      ' unknown'
      iterr(1) = 2
      iterr(2) = 2
      iterr(3) = 2
      goto 1060
 1030 kwav = k
 
c     Compute distance and azimuth between station and event

      stalt = stalat(ista)
      staln = stalon(ista)
      call distaz2 (stalt, staln, a1, a2, dist1, azim1, baz)
      distcntr	= dist1
      azcntr	= azim1
      dist(1)	= distcntr
      aztt(1)	= azcntr
      z(1)	= zfoc

c     If no confidence ellipse, compute travel-time from station to point
 
      if (epmaj.le.0.0 .or. epmin.le.0.0) then
         ntimes = 1
      else

c        Find points on confidence ellipse that are the minimum and
c        maximum distance from the station.  Assume that these points
c        are at one of the tips of the axes of the confidence ellipse.

         ntimes = 3
         epj    = epmaj/deg2km
         epn    = epmin/deg2km
         epr    = epstr
         call latlon2 (a1, a2, epj, epr, plat(1), plon(1))
         call latlon2 (a1, a2, epj, epr+180.0, plat(2), plon(2))
         call latlon2 (a1, a2, epn, epr+ 90.0, plat(3), plon(3))
         call latlon2 (a1, a2, epn, epr+270.0, plat(4), plon(4))

         do 1040 n = 1, 4
            call distaz2 (stalt, staln, plat(n), plon(n), distance,
     &                    azimuth, baz)

c           Find minimum and maximum distance with associated azimuth

            if (distance.lt.dist(2)) then
               dist(2) = distance
               aztt(2) = azimuth
            end if
            if (distance.gt.dist(3)) then
               dist(3) = distance
               aztt(3) = azimuth
            end if

c           Find minimum and maximum azimuth

            azdiff = azimuth - azcntr
            if (abs(azdiff).gt.180.0) 
     &          azdiff = -sign(360.0 - abs(azdiff), azdiff)
            if (azdiff.lt.azneg) then
               azim(2) = azimuth
               azneg   = azdiff
            else if (azdiff.gt.azpos) then
               azim(3) = azimuth
               azpos   = azdiff
            end if
 1040    continue
 
c        If station is within ellipse, set minimum distance to 0 and 
c        azimuth bounds to 0 and 360 degrees

         if (azcntr.ge.0.0 .and. azcntr.le.90.0) then
            angle = 90.0 - azcntr
         else if (azcntr.gt.90.0 .and. azcntr.le.180.0) then
            angle = 180.0 + azcntr
         else if (azcntr.gt.180.0 .and. azcntr.le.270.0) then
            angle = azcntr
         else
            angle = azcntr - 180.0
         end if
         x = distcntr*deg2km*cos(angle*deg2rad)
         y = distcntr*deg2km*sin(angle*deg2rad)

c        Rotate into ellipsoid coordinate system and check if station is
c        within ellipse

         angle = 90.0 - epstr
         xp    = x*cos(angle) + y*sin(angle)
         yp    = -x*sin(angle) + y*cos(angle)
         e = (xp/epmaj)**2 + (yp/epmin)**2
         if (e.le.1.0) then
            dist(2) = 0.0
            azim(2) = 0.0
            azim(3) = 360.0
         end if

c        Minimum time path will be the deepest epicenter for all but (a)
c        depth phases, where the shallower depth will produce earlier 
c        arrivals, and (b) phases from events who's epicenter-station 
c        distance is less than its depth.
c
         if (dwavidx(1:1).eq.'p' .or. dwavidx(1:1).eq.'s' .or.
     &       dist(2)*deg2km.lt.zfoc) then
            z(2) = zfoc - zfint
            z(3) = zfoc + zfint
         else
            z(2) = zfoc + zfint
            z(3) = zfoc - zfint
         end if
      
         if (z(2).lt.0.0) z(2) = 0.0
         if (z(3).lt.0.0) z(3) = 0.0
 
      end if

      do 1050 n = 1, ntimes
c        call ttime_calc (kwav, atx, aztt(n), dist(n), radius, z(n),
c    &                    dcalx, iterr(n))
         call ttcal1 (z(n), radius, dist(n), aztt(n), maxtbd,
     &                maxtbz, ntbd(kwav), ntbz(kwav), tbd(1,kwav),
     &                tbz(1,kwav), tbtt(1,1,kwav), dcalx, atx, iterr(n))

c        Use only sucessfully interpolated and extrapolated results
c        - Don't use if in a hole (dcalx = -1.0)

         if (dcalx.lt.0.0) then
            tcal(n) = -999.0
         else
            tcal(n) = dcalx
         end if

         if (sloflg.eq.'y' .or. sloflg.eq.'Y') then
c           call slow_calc (kwav-1, atx, aztt(n), dist(n), radius, z(n),
c    &                      dcalx, iserr(n))
            call slocal1 (zfoc, radius, dist(n), aztt(n),
     &                    maxtbd, maxtbz, ntbd(kwav), ntbz(kwav),
     &                    tbd(1,kwav), tbz(1,kwav), tbtt(1,1,kwav),
     &                    dcalx, atx, iserr(n))

c           Use only sucessfully interpolated and extrapolated results
c           - Don't use if in hole (dcalx = -1.0)

            if (dcalx .lt. 0.0) then 
               scal(n) = -999.0
            else
               scal(n) = dcalx
            end if
         end if
 1050 continue

c     Load array data into arguments for return to calling routine

c     Sometimes, the most distant point may not have the largest slowness 

 1060 if (sloflg.eq.'y' .or. sloflg.eq.'Y') then
         slocntr   = scal(1)
         iserrcntr = iserr(1)
         if (scal(3).lt.0.0 .or. scal(2).lt.scal(3)) then
            slomin   = scal(2)
            slomax   = scal(3)
            iserrmin = iserr(2)
            iserrmax = iserr(3)
         else 
            slomin   = scal(3)
            slomax   = scal(2)
            iserrmin = iserr(3)
            iserrmax = iserr(2)
         end if
      end if

      ttcntr    = tcal(1)
      ttmin     = tcal(2)
      ttmax     = tcal(3)
      azmin     = azim(2)
      azmax     = azim(3)
      distmin   = dist(2)
      distmax   = dist(3)
      iterrcntr = iterr(1)
      iterrmin  = iterr(2)
      iterrmax  = iterr(3)

c      Print location results if desired

      if (prtflg.eq.'y' .or. prtflg.eq.'Y') then

         do 1070 i = 1, 19
 1070    lprt(i) = .false.
         write (luout,'(2a)') '===EVENT INFORMATION===================',
     &                        '======================================='
         write (luout, '(3(a,f10.3,a))') ' Latitude:', alat, ' deg,',
     &                                   ' Longitude:', alon, ' deg,',
     &                                   ' Depth:', zfoc, ' km'

         write (luout, '(2(a,f10.1,a))')
     &          ' Semi-major axis:', epmaj, ' km,',
     &          ' Semi-minor axis:', epmin, ' km,'
         write (luout, '(a,f10.1,a)')   ' Major-axis strike:', epstr,
     &                                  ' deg CW from north'
         write (luout, '(2(a,f10.1,a))')' Depth error:', zfint, ' km,',
     &                                  ' Orig. time error:', toint,' s'

 	 write (luout, '(2a)') 
     &          '===PREDICTED TIME/AZIMUTH/SLOWNESS BOUNDS',
     &          '====================================='

	 write (luout, '(a,i10,4a)') '  Ariv ID = ', idaridx,
     &                 '  Station = ', dstaidx, '  Phase = ', dwavidx

	 write (luout, '(a,3f15.2)') 'Min Cntr Max Azim (deg)    : ',
     &                               azmin, azcntr, azmax
	 write (luout, '(a,3f15.2)') 'Min Cntr Max Dist (deg)    : ',
     &                               distmin, distcntr, distmax
	 write (luout, '(a,3f15.2)') 'Min Cntr Max Time (sec)    : ',
     &                               ttmin, ttcntr, ttmax
	 write (luout, '(a,3f15.2)') 'Min Cntr Max Slow (sec/deg): ',
     &                               slomin, slocntr, slomax
	 write (luout, '(a,3i15)')   'Time Errors           : ',
     &                              iterrmin, iterrcntr, iterrmax
 	 write (luout, '(a,3i15)')   'Slowness Errors       : ',
     &                               iserrmin, iserrcntr, iserrmax

         if (iterrmin.gt.0)  lprt(iterrmin)  = .true.
         if (iterrcntr.gt.0) lprt(iterrcntr) = .true.
         if (iterrmax.gt.0)  lprt(iterrmax)  = .true.
         if (iserrmin.gt.0)  lprt(iserrmin)  = .true.
         if (iserrcntr.gt.0) lprt(iserrcntr) = .true.
         if (iserrmax.gt.0)  lprt(iserrmax)  = .true.

         write (luout, *) ' =  0, No problem, normal interpolation'
         if (lprt(1))  write (luout, *) ' =  1, No station information'
         if (lprt(2))  write (luout, *) ' =  2, No travel-time tables'
         if (lprt(3))  write (luout, *) ' =  3, Data type unknown'
         if (lprt(4))  write (luout, *) ' =  4, S.D. <= 0.0'
         if (lprt(11)) write (luout, *) ' = 11, Distance-depth point ',
     &                        '(x0,z0) in hole of travel-time curve'
         if (lprt(12)) write (luout, *) ' = 12, x0 < x(1)'
         if (lprt(13)) write (luout, *) ' = 13, x0 > x(max)'
         if (lprt(14)) write (luout, *) ' = 14, z0 < z(1)'
         if (lprt(15)) write (luout, *) ' = 15, z0 > z(max)'
         if (lprt(16)) write (luout, *) ' = 16, x0 < x(1) & z0 < z(1)'
         if (lprt(17)) write (luout, *) ' = 17, x0 > x(max) & z0 < z(1)'
         if (lprt(18)) write (luout, *) ' = 18, x0 < x(1) & z0 > z(max)'
         if (lprt(19))
     &       write (luout, *) ' = 19, x0 > x(max) & z0 > z(max)'
         write (luout, *)

      end if

      close (luout)

      return
      end


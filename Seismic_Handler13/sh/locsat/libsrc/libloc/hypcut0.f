
c NAME
c	hypcut0 -- Make an initial location guess or prediction.

c FILE
c	hypcut0.f

c SYNOPSIS
c	Make a first-cut educated guess at the epicentral location.

c DESCRIPTION
c	Subroutine.  This subroutine takes a first-cut at the epicentral
c	location using one of the following techniques, in the order 
c	tried,

c	  1.	Attempts to compute an initial location using S-P times 
c	      	for the closest station and the best-determined defining 
c		azimuth for that station (based on the smallest S.D.).
c	  2.	Attempts to compute an intial location from various combi-
c	  	nations of S-P times and P-wave arrival times.  The 
c		searches are preformed in the following order of 
c		importance:
c	  	  A.	Uses the 3 least S-P times and finds a common 
c			crossing point.
c	  	  B.	Uses the smallest P-wave travel time between the 
c			common crossing points of 2 S-P times.
c	  	  C.	Uses a single S-P time to obtain a crude origin 
c			time and then find the nearest crossing points 
c			from two P-wave arrival times.
c	  3.	Attempts to compute an initial location from various combi-
c		nations of azimuth and P-wave arrival time data.  When the
c		difference between azimuths is < 10 deg., then the inter-
c		section will be poorly determined, and is therefore, 
c		ignored.  The searches are preformed in the following order 
c		of importance:
c		  A.	Computes the intersection of 2 great circles given 
c			2 stations with azimuth data from those points.
c			The location formed using the best-defining 
c			azimuths from those stations closest to the 
c			location is then chosen.  
c		  B.	Minimizes two P-wave arrival times constrained by a 
c			single azimuth datum, preferably one with an 
c			arrival time.
c	  4.	Attempts to obtain an initial epicentral location based on
c		an approximate minimization procedure using three or more 
c		P-wave arrival times as data.
c	  5.	Attempts to compute an initial location using a P slowness 
c		and the best-determined azimuth datum at the station with 
c		the P-slowness with the smallest standard error between some 
c		bounds.
c	  6.	Looks for the closest station based of arrival times and 
c		uses an associated azimuth (if available).  
c	  7.	Looks for a station with a good slowness (based on the
c		slowness datum with the smallest a priori data standard 
c		error) and places the initial location near that station.
c	  8.	It looks for a station with an azimuth and places the 
c		initial location near that station at the associated 
c		azimuth. 

c	---- Indexing ----
c       i = 1, nsta;	j = 1, nwav;	k = 1, ntbd(j);	m = 1, ntbz(j);
c	n = 1, ndata;

c	---- On entry ----
c	ndata:	Number of data loaded into arrays
c	nsta:	Number of stations
c	nwav:	Number of phases in list
c	maxtbd:	Maximum dimension of k'th position in tbd(), tbtt()
c	maxtbz:	Maximum dimension of m'th position in tbz(), tbtt()

c	staid(i):	List of all acceptible station names
c	stalat(i):	Station latitudes  (deg)
c	stalon(i):	Station longitudes (deg)
c	dobs(n):	Observed data (sec, deg, sec/deg)
c	dsd(n): 	Standard deviations of observed data
c	dwavid(n):	Phase ID's of data
c	ntbd(j):	Number of distance samples in travel-time tables
c	tbd(k,j):	Angular distance (deg)
c	tbtt(k,m,j):	Travel-time (sec)
c	ipsta(n):	Station index of n'th datum
c	ipwav(n):	Wave index of n'th datum
c	idtyp(n):	Data type of n'th datum
c			  = 0, Data type unknown 
c			  = 1, Arrival time datum 
c			  = 2, Azimuth datum
c			  = 3, Slowness datum
c	iderr(n):	Error code for n'th datum
c			  = 0, Datum OK
c			 != 0, Datum not valid, and not used here
c	atype(n):	Arrival usage 
c			  = d: Defining, used in location
c			  = n: Could be defining, but not used in location
c			  = a: Not to be used in location
c	prtflg:		Verbose printout (y = Yes; n = None)

c	---- On return ----
c	alat0:	First cut at event latitude (deg)
c	alon0:	First cut at event longitude (deg)
c	ierr:	Error flag;
c		  0, No error
c		  1, Too few data to get initial location

c	---- Functions called ----
c	From libgeog
c		azcros2:	Determine crossing points of 2 great circles
c		crossings:	Determine crossing points of 2 small circles
c		distaz2:	Determine the distance between between two
c				lat./lon. pairs
c		latlon2:	Compute a second lat./lon. from the first, a
c				distance and an azimuth

c DIAGNOSTICS
c	Returns if it cannot find an initial educated guess given the 
c	above search conditions.

c NOTES
c	Currently under-going substanitive additions.

c SEE ALSO
c

c AUTHORS
c	Steve Bratt, December 1988;
c	Walter Nagy, July 1990.


      subroutine hypcut0 (staid, stalat, stalon, nsta, dobs, dsd,
     &                    dwavid, ipsta, ipwav, nwav, maxtbd, maxtbz,
     &                    ntbd, tbd, tbtt, idtyp, iderr, atype, ndata,
     &                    luout, prtflg, alat0, alon0, ierr)

      implicit none   ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccshypcut0/ sccsid
      character*80 sccsid
      data sccsid /'@(#)hypcut0.f	44.1	9/20/91'/


c     ---- Parameter declarations ----

      integer*4 maxdist, maxsta
      real*8    defaultazim, defaultdist, deg2rad, fmaxazim, fmaxazcross
      real*8    fmaxslow, fmaxsminusp, fminslow

c     Maximum azimuth setting (deg.) for initialization purposes
      parameter (fmaxazim = 40.0)

c     Maximum allowable stations
      parameter (maxsta = 300)

c     Convert degrees to radians
      parameter (deg2rad = 0.017453293)

c     Default distance (deg.) from the "guessed" event to the station
      parameter (defaultdist = 5.0)

c     Default azimuth (deg.) from the "guessed" event to the station
      parameter (defaultazim = 0.0)

c     Maximum permissable elements for distance(), slowness() arrays
      parameter (maxdist = 19)

c     Maximum cross-azimuth setting (deg.) for initialization purposes
      parameter (fmaxazcross = 80.0)

c     Maximum S-P time setting (sec.) for initialization purposes
      parameter (fmaxsminusp = 690.0)

c     Minimum allowable slowness
      parameter (fminslow = 0.02)

c     Maximum allowable slowness
      parameter (fmaxslow = 19.16)

c     ---- On entry ----

      integer*4 maxtbd, maxtbz, ndata, nsta, nwav
      integer*4 idtyp(ndata), iderr(ndata), ipsta(ndata), ipwav(ndata)
      integer*4 luout, ntbd(nwav)
      real*4    dobs(ndata), dsd(ndata), stalat(nsta), stalon(nsta)
      real*4    tbd(maxtbd,nwav), tbtt(maxtbd,maxtbz,nwav)
      character*(*) atype(ndata), dwavid(ndata), prtflg, staid(nsta)

c     ---- On return ----

      integer*4 ierr
      real*4    alat0, alon0

c     ---- Internal variables ----

      integer*2 indexcompr(maxsta), indexdsd(maxsta), indexdsd2(maxsta)
      integer*2 indexsminusp(maxsta), iwave(maxsta)
      integer*4 i, i1, i1s, i2, i2s, i3s, iazim, ic2, icerr, icompr
      integer*4 icross, ierrx, islow, isminusp, iterr, itimeyet, iusesta
      integer*4 j, j1, j2, k, lnblnk, n, n1, n2
      real*8    alat0x, alon0x, azimsd(maxsta), azi, azisav, baz
      real*8    bestazcross, bestazim(maxsta), bestslow(maxsta)
      real*8    comprtime(maxsta), crosslat(12), crosslon(12), delcross
      real*8    delta, dis(maxsta), dist1, dist1x, dist2, dist2x
      real*8    distance(maxdist), fmaxtime, ordercompr(maxsta)
      real*8	orderdsd(maxsta), orderdsd2(maxsta),ordersminusp(maxsta)
      real*8	res1, res2, sheartime(maxsta), slowness(maxdist)
      real*8	slowsd(maxsta), sminusp(12), sminusptime, smallest
      real*8	tcalc, tmp, torg, useazim, useslow
      real*8	a1, a2, sta1, sta2, sta3, sta4
      logical   goodazim(maxsta), goodcompr(maxsta), goodslow(maxsta)
      logical   goodsminusp(maxsta)
      character wavid*2


c     Slowness and S-P time as a function of distance for a surface event
c     Taken from IASPEI Seismological Tables of B.L.N. Kennett (1991)

      data distance / 0.0,  10.0,  20.0,  30.0,  40.0,  50.0,  60.0,
     &               70.0,  80.0,  90.0, 100.0, 110.0, 120.0, 130.0,
     &              140.0, 150.0, 160.0, 170.0, 180.0 /
      data slowness /19.17, 13.70, 10.90, 8.85, 8.30, 7.60, 6.88, 6.15,
     &                5.40,  4.66,  4.44, 1.96, 1.91, 1.88, 1.79, 1.57,
     &                1.14,  0.59,  0.01 /
      data sminusp  / 0.00, 114.20, 226.76, 300.00, 367.49, 432.64,
     &              494.45, 552.32, 605.82, 654.47, 695.75, 734.60 /


      if (prtflg.eq.'y') write (luout,'(/a/a)')
     &   ' hypcut0: General initial location information :',
     &   ' ==============================================='

c     Local initializations

      do 1000  i = 1, nsta
         azimsd(i)      = fmaxazim
         slowsd(i)      = fmaxslow
         bestslow(i)    = -888888.0
         bestazim(i)    = -888888.0
         goodcompr(i)   = .false.
         goodsminusp(i) = .false.
         goodazim(i)    = .false.
         goodslow(i)    = .false.
 1000 continue

      iusesta     =  0
      ierr        =  0
      itimeyet    =  0
      bestazcross =  fmaxazcross
      sminusptime =  fmaxsminusp
      fmaxtime    =  888888.0

c     Load valid P-wave and S-wave arrival times, P-slownesses and azimuths
c     (use P-type phase or phase with the smallest azimuth standard 
c     deviation) at each station.

      do 1020  n = 1, ndata
         if (iderr(n).eq.0 .and. atype(n)(1:1).eq.'d') then
            i     = ipsta(n)
            wavid = dwavid(n)
            k     = lnblnk(wavid)

c           ---- Arrival times ----
            if (idtyp(n).eq.1) then
               if (itimeyet.eq.0) then
                  fmaxtime = dobs(n) + 888888.0
                  do 1010  j = 1, nsta
                     comprtime(j) = fmaxtime
                     sheartime(j) = fmaxtime
 1010             continue
                  itimeyet = 1
               end if

c              Load valid S and P times into arrays for each i'th station
               if ((wavid(1:k).eq.'P ' .or. wavid(1:k).eq.'Pn' .or.
     &              wavid(1:k).eq.'Pg' .or. wavid(1:k).eq.'Pb') 
     &              .and. dobs(n).lt.comprtime(i)) then
                  comprtime(i) = dobs(n)
                  iwave(i)     = ipwav(n)
               else if ((wavid(1:k).eq.'S ' .or. wavid(1:k).eq.'Sn'
     &             .or. wavid(1:k).eq.'Sb' .or. wavid(1:k).eq.'Lg'
     &             .or. wavid(1:k).eq.'Sg') .and.
     &             dobs(n).lt.sheartime(i)) then
                  sheartime(i) = dobs(n)
               end if
	      
c           ---- Azimuths ----
            else if (idtyp(n).eq.2 .and. dsd(n).gt.0.0) then
               if (dsd(n).lt.azimsd(i)) then
                  azimsd(i)   = dsd(n)
                  bestazim(i) = dobs(n)
               end if

c           ---- Slownesses ----
            else if (idtyp(n).eq.3) then
               if (wavid(1:1).eq.'P' .and. dsd(n).gt.0.0 .and.
     &             dobs(n).gt.fminslow .and. dobs(n).lt.fmaxslow) then
                  if (dsd(n).lt.slowsd(i)) then
                     slowsd(i)   = dsd(n)
                     bestslow(i) = dobs(n)
                  end if
               end if
            end if
         end if
 1020 continue

c     Define good (valid) P-wave and S-wave arrival times, S-P times, 
c     azimuths and slownesses.  Load these valid data in the order() 
c     array and save their station index in the index() array.

      icompr   = 0
      isminusp = 0
      iazim    = 0
      islow    = 0
      do 1030 i = 1, nsta
         goodcompr(i) = (comprtime(i).lt.fmaxtime) 
         if (goodcompr(i)) then
            icompr = icompr + 1
            indexcompr(icompr) = i
            ordercompr(icompr) = comprtime(i)
         end if

         goodsminusp(i) = (goodcompr(i) .and. (sheartime(i).lt.fmaxtime)
     &                     .and. sheartime(i).gt.comprtime(i))
         if (goodsminusp(i)) then
            isminusp = isminusp + 1
            indexsminusp(isminusp) = i
            ordersminusp(isminusp) = sheartime(i) - comprtime(i)
         end if

         goodazim(i) = (bestazim(i).ge.-180.0 .and. 
     &                  bestazim(i).le.360.0)
         if (goodazim(i)) then
            iazim = iazim + 1
            indexdsd(iazim) = i
            orderdsd(iazim) = azimsd(i)
         end if

         goodslow(i) = (bestslow(i).gt.fminslow .and. 
     &                  bestslow(i).lt.fmaxslow)
         if (goodslow(i)) then
            islow = islow + 1
            indexdsd2(islow) = i
            orderdsd2(islow) = slowsd(i)
         end if
 1030 continue

c     Sort P-wave arrival times in descending order (i.e., earliest 
c     arrival-times first)

      if (icompr.gt.0) then
         do 1040 i = (icompr+1)/2, 1, -1
            do 1040 j = 1, icompr-i
               if (ordercompr(j).gt.ordercompr(j+i)) then
                  tmp             = ordercompr(j)
                  n               = indexcompr(j)
                  ordercompr(j)   = ordercompr(j+i)
                  indexcompr(j)   = indexcompr(j+i)
                  ordercompr(j+i) = tmp
                  indexcompr(j+i) = n
               end if
 1040    continue
      end if

c     Sort S-P times in descending order (i.e., earliest times first)

      if (isminusp.gt.0) then
         do 1050 i = (isminusp+1)/2, 1, -1
            do 1050 j = 1, isminusp-i
               if (ordersminusp(j).gt.ordersminusp(j+i)) then
                  tmp               = ordersminusp(j)
                  n                 = indexsminusp(j)
                  ordersminusp(j)   = ordersminusp(j+i)
                  indexsminusp(j)   = indexsminusp(j+i)
                  ordersminusp(j+i) = tmp
                  indexsminusp(j+i) = n
               end if
 1050    continue
      end if

c     Sort azimuths according to their data standard errors in increasing 
c     order (i.e., smallest azimuthal standard errors first)

      if (iazim.gt.0) then
         do 1060 i = (iazim+1)/2, 1, -1
            do 1060 j = 1, iazim-i
               if (orderdsd(j).gt.orderdsd(j+i)) then
                  tmp           = orderdsd(j)
                  n             = indexdsd(j)
                  orderdsd(j)   = orderdsd(j+i)
                  indexdsd(j)   = indexdsd(j+i)
                  orderdsd(j+i) = tmp
                  indexdsd(j+i) = n
               end if
 1060    continue
      end if

c     Sort slownesses in increasing order (i.e., largest slowness is 
c     nearest the event and often can be quite diagnostic)

      if (islow.gt.0) then
         do 1070 i = (islow+1)/2, 1, -1
            do 1070 j = 1, islow-i
               if (orderdsd2(j).gt.orderdsd2(j+i)) then
                  tmp            = orderdsd2(j)
                  n              = indexdsd2(j)
                  orderdsd2(j)   = orderdsd2(j+i)
                  indexdsd2(j)   = indexdsd2(j+i)
                  orderdsd2(j+i) = tmp
                  indexdsd2(j+i) = n
               end if
 1070    continue
      end if

c     Find closest station with smallest S-P time and an azimuth.  Compute 
c     the location from the S-P time and azimuth.
c     First and preferred search procedure !

      if (isminusp.lt.1) goto 1280
      do 1090  i = 1, isminusp
         n = indexsminusp(i)
         if (goodazim(n)) then
            sminusptime = ordersminusp(i)

c           Interpolate slowness to get distance.
            do 1080  j = 1, 11
               if (sminusptime.gt.sminusp(j) .and.
     &             sminusptime.le.sminusp(j+1)) then
                  dis(1) = (distance(j+1) - distance(j))*
     &                     (sminusptime - sminusp(j))/
     &                     (sminusp(j+1) - sminusp(j)) + distance(j)
                  useazim = bestazim(n)
                  iusesta = n
                  if (prtflg.eq.'y') 
     &                write (luout,'(a/2a,3(/a,f7.2,a))')
     &                '    Method: S-P time w/ azimuth at 1 station',
     &                '   Station: ', staid(iusesta),
     &                '  S-P time: ', sminusptime, ' sec.',
     &                '  Distance: ', dis(1), ' deg.',
     &                '   Azimuth: ', useazim, ' deg.'
                     
c                 Done.  Let's go find a lat./lon. pair !
                  sta1 = stalat(iusesta)
                  sta2 = stalon(iusesta)
                  call latlon2 (sta1, sta2, dis(1), useazim, a1, a2)
                  alat0 = a1
                  alon0 = a2
                  if (prtflg.eq.'y') call prtcut (alat0, alon0, luout)
                  return
               end if
 1080       continue
         end if
 1090 continue

c     Look here for multiple S-P times and compute distances

      if (icompr.lt.2) goto 1280
      do 1110 i = 1, isminusp
         sminusptime = ordersminusp(i)

c        Interpolate slowness(es) to get distance(s).
         do 1100  j = 1, 11
            if (sminusptime.gt.sminusp(j) .and.
     &          sminusptime.le.sminusp(j+1)) then
               dis(i) = (distance(j+1) - distance(j))*
     &                  (sminusptime - sminusp(j))/
     &                  (sminusp(j+1) - sminusp(j)) + distance(j)
            end if
 1100    continue
 1110 continue

c     Compute the approximate origin time

      j = indexsminusp(1)
      n = iwave(j)
c     call find_ttime (n-1, dis(1), tcalc, iterr)
c     if (iterr.gt.0) goto 1130
c     torg = ordercompr(j) - tcalc
      if (tbd(1,n).gt.dis(1)) goto 1130
      do 1120 i = 1, ntbd(n)
         if (tbd(i,n).gt.dis(1)) then
            tmp   = (dis(1)-tbd(i-1,n)) / (tbd(i,n)-tbd(i-1,n)) 
            tcalc = tbtt(i-1,1,n) + tmp*(tbtt(i,1,n)-tbtt(i-1,1,n))
            torg  = ordercompr(j) - tcalc
            goto 1130
         end if
 1120 continue

c     Determine the necessary crossing points

 1130 icross = 0
      icerr  = 0
      if (isminusp.gt.1) then
         do 1160 i1 = 2, isminusp
            do 1150 i2 = 1, i1-1
               n1     = indexsminusp(i1)
               n2     = indexsminusp(i2)
               icross = icross + 1
               ic2    = 2*icross
               sta1 = stalat(n2)
               sta2 = stalon(n2)
               sta3 = stalat(n1)
               sta4 = stalon(n1)
               call crossings (sta1, sta2, sta3, sta4, dis(i2), dis(i1),
     &                         crosslat(ic2-1), crosslon(ic2-1),
     &                         crosslat(ic2), crosslon(ic2), icerr)
               if (icerr.gt.0) then
                  icross = icross - 1
                  goto 1150
               end if
               if (icross.gt.1) then
                  if (n2.ne.i1s .and. n2.ne.i2s) then
                     i2s = n2
                  else
                     i2s = n1
                  end if
               else
                  i1s = n2
                  i3s = n1
               end if

c              Find the best crossing from 3 S-P times ?

               if (icross.gt.1) then
                  smallest = 888888.0
                  do 1140 i = 1, 2
                     do 1140 j = 3, 4
                        call distaz2 (crosslat(i), crosslon(i),
     &                                crosslat(j), crosslon(j),
     &                                delcross, azi, baz)
                        if (delcross.lt.smallest) then
                           smallest = delcross
                           azisav   = azi
c                          K.S. 1-Dec-97, abort here because of illegal code
                           stop '*** K.S. ill code in hypcut0.f ***'
c                           i1       = i
c                           i2       = j
                        end if
 1140             continue
                  call latlon2 (crosslat(i1), crosslon(i1),
     &                          smallest/2.0, azisav, a1, a2)
                  alat0 = a1
                  alon0 = a2
                  if (prtflg.eq.'y') write (luout, '(a/4a/a,3f7.2,a)')
     &               '    Method: Nearest crossing of 3 S-P times',
     &               '  Stations: ', staid(i1s), staid(i2s), staid(i3s),
     &               ' Distances: ', dis(1), dis(2), dis(3), ' deg.'

c                 Done.  Exit routine.
                  if (prtflg.eq.'y') call prtcut (alat0, alon0, luout)
                  return
               end if

 1150       continue
 1160    continue

         if (icross.eq.0 .or. icompr.lt.3) goto 1230

c        Use 2 S-P times and the shortest independent arrival time 
c        to determine initial location

         do 1170 i = 1, icompr
            j = indexcompr(i)
            if (j.ne.indexsminusp(1) .and. j.ne.indexsminusp(2)) 
     &         goto 1180
 1170    continue
 1180    n = iwave(j)

c        Calculate theoretical travel times

         sta1 = stalat(j)
         sta2 = stalon(j)
         call distaz2 (sta1, sta2, crosslat(1), crosslon(1),
     &                 dis(1), azi, baz)
c        call find_ttime (n-1, dis(1), tcalc, iterr)
c        res1 = abs(comprtime(j) - torg - tcalc)
         do 1190 i = 1, ntbd(n)
            if (tbd(i,n).gt.dis(1)) then
               tmp   = (dis(1)-tbd(i-1,n)) / (tbd(i,n)-tbd(i-1,n)) 
               tcalc = tbtt(i-1,1,n) + tmp*(tbtt(i,1,n)-tbtt(i-1,1,n))
               res1  = abs(comprtime(j) - torg - tcalc)
               goto 1200
            end if
 1190    continue

 1200    call distaz2 (sta1, sta2, crosslat(2), crosslon(2),
     &                 dis(2), azi, baz)
c        call find_ttime (n-1, dis(2), tcalc, iterr)
c        res2 = abs(comprtime(j) - torg - tcalc)
         do 1210 i = 1, ntbd(n)
            if (tbd(i,n).gt.dis(2)) then
               tmp   = (dis(2)-tbd(i-1,n)) / (tbd(i,n)-tbd(i-1,n)) 
               tcalc = tbtt(i-1,1,n) + tmp*(tbtt(i,1,n)-tbtt(i-1,1,n))
               res2  = abs(comprtime(j) - torg - tcalc)
               goto 1220
            end if
 1210    continue

c        Choose travel time with the smallest residual

 1220    if (res1.lt.res2) then
            alat0 = crosslat(1)
            alon0 = crosslon(1)
         else
            alat0 = crosslat(2)
            alon0 = crosslon(2)
         end if
         if (prtflg.eq.'y') write (luout, '(a/4a/a,2f7.2,a)')
     &       '    Method: S-P crossing and nearest arrival time',
     &       '  Stations: ', staid(i1s), staid(i2s), staid(j),
     &       ' Distances: ', dis(1), dis(2), ' deg.'

c        Done.  Exit routine.
         if (prtflg.eq.'y') call prtcut (alat0, alon0, luout)
         return

      else

c        Determine initial location using 1 S-P time and 2 nearest
c        independent arrival times

 1230    icross = 0
         n1     = indexsminusp(1)
         do 1270 k = 1, icompr
            j = indexcompr(k)
            if (j.eq.n1) goto 1270
            icross = icross + 1
            n      = iwave(j)

c           Calculate distance from station to the origin and then 
c           find the crossing points

            tcalc = ordercompr(k) - torg
c           call find_dist (n-1, tcalc, dist1, iterr)
c           if (iterr.gt.0) goto 1270
            if (tbtt(1,1,n).gt.tcalc) goto 1270
            do 1240 i = 1, ntbd(n)
               if (tbtt(i,1,n).gt.tcalc) then
                  tmp   = (tcalc-tbtt(i-1,1,n)) / 
     &                    (tbtt(i,1,n)-tbtt(i-1,1,n)) 
                  dist1 = tbd(i-1,n) + tmp*(tbd(i,n)-tbd(i-1,n))
                  goto 1250
               end if
 1240       continue

 1250       icerr = 0
            ic2   = 2*icross
            sta1  = stalat(n1)
            sta2  = stalon(n1)
            sta3  = stalat(j)
            sta4  = stalon(j)
            call crossings (sta1, sta2, sta3, sta4, dis(1), dist1,
     &                      crosslat(ic2-1), crosslon(ic2-1),
     &                      crosslat(ic2), crosslon(ic2), icerr)
            if (icerr.lt.1) then
               dis(icross+1) = dist1
               if (icross.gt.1) then
                  j2 = j
                  smallest = 888888.0
                  do 1260 i = 1, 2
                     do 1260 j = 3, 4
                        call distaz2 (crosslat(i), crosslon(i),
     &                                crosslat(j), crosslon(j),
     &                                delcross, azi, baz)
                        if (delcross.lt.smallest) then
                           smallest = delcross
                           azisav   = azi
                           i1       = i
                           i2       = j
                        end if
 1260             continue
                  call latlon2 (crosslat(i1), crosslon(i1),
     &                          smallest/2.0, azisav, a1, a2)
                  alat0 = a1
                  alon0 = a2
                  if (prtflg.eq.'y') write (luout, '(a/4a/a,3f7.2,a)')
     &             '    Method: Nearest crossing of 1 S-P & 2 P- times',
     &             '  Stations: ', staid(n1), staid(j1), staid(j2),
     &             ' Distances: ', dis(1), dis(2), dis(3), ' deg.'

c                 Done!  Exit routine!
                  if (prtflg.eq.'y') call prtcut (alat0, alon0, luout)
                  return
               else
                  j1 = j
               end if
            else
               icross = icross - 1
            end if
 1270    continue

      end if

c     Find crossing point of 2 defining azimuths.  Take the location 
c     that is closest (on average) to the 2 stations
c     Second and next important search procedure !
      
 1280 if (iazim.gt.1) then
         do 1300 i1 = 2, iazim
            do 1290 i2 = 1, i1-1
               n1 = indexdsd(i1)
               n2 = indexdsd(i2)

c              If the difference between 2 azimuths is less than 10 deg., 
c              then the confidence one might put into the computed 
c              crossing point would be quite uncertain, so ignore!

               tmp = abs(bestazim(n1)-bestazim(n2))
               if (tmp.gt.350.0) tmp = 360.0-tmp
               if (tmp.lt.10.0) goto 1290
               sta1 = stalat(n1)
               sta2 = stalon(n1)
               sta3 = stalat(n2)
               sta4 = stalon(n2)
               call azcros2 (sta1, sta2, bestazim(n1), sta3, sta4,
     &                       bestazim(n2), dist1x, dist2x,
     &                       alat0x, alon0x, ierrx)
               if (ierrx.eq.0) then 
                  delta = (dist1x + dist2x)*0.5
c                 dsdwt = 1.0/dsd(n1)*dsd(n2)
c                 delwt = 1.0/delta
c                 totwt = dsdwt*delwt
                  if (delta.lt.bestazcross) then
                     bestazcross = delta
                     i1s   = n1
                     i2s   = n2
                     alat0 = alat0x
                     alon0 = alon0x
                     dist1 = dist1x
                     dist2 = dist2x
                  end if
               end if
 1290       continue
 1300    continue

         if (bestazcross.lt.fmaxazcross) then
            if (prtflg.eq.'y') write (luout,'(a/3a,2(/a,2f7.2,a))')
     &          '    Method: Crossing of azimuths from 2 stations',
     &          '  Stations: ', staid(i1s), staid(i2s),
     &          '  Azimuths: ', bestazim(i1s), bestazim(i2s), ' deg.',
     &          ' Distances: ', dist1, dist2, ' deg.'

c           Done!  Exit routine!
            if (prtflg.eq.'y') call prtcut (alat0, alon0, luout)
            return
         end if
      end if

c     Look here for 1 azimuth and the 2 closest arrvial times

c     if (iazim.eq.1 .and. icompr.gt.1) then
c        n1 = indexdsd(1)

c        Preferably with an arrival time from the azimuth datum
c        do 1310 i = 1, icompr
c           if (n1.eq.indexcompr(i)) then
c              
c           end if
c1310    continue
c     end if


c     Find station with slowness and azimuth, then compute the location 
c     from these data

      if (islow.gt.0 .and. iazim.gt.0) then
         iusesta = indexdsd2(1)
         useslow = bestslow(iusesta)
         useazim = bestazim(iusesta)
c        do 1320  i = 1, nsta
c           if (goodslow(i) .and. goodazim(i) .and.  
c    &          bestslow(i).gt.useslow) then
c              useslow = bestslow(i)
c              useazim  = bestazim(i)
c              iusesta  = i
c           end if
c1320    continue

         do 1330  j = 1, maxdist-1
            if (useslow.lt.slowness(j) .and.
     &          useslow.ge.slowness(j+1)) then
               dis(1) = (distance(j+1) - distance(j))*
     &                  (useslow - slowness(j))/
     &                  (slowness(j+1) - slowness(j)) + distance(j)
               if (prtflg.eq.'y') write (luout,'(a/2a,3(/a,f7.2,a))')
     &             '    Method: P slowness with azimuth at 1 station',
     &             '   Station: ', staid(iusesta),
     &             '  Slowness: ', useslow, ' sec./deg.',
     &             '  Distance: ', dis(1), ' deg.',
     &             '   Azimuth: ', useazim, ' deg.'
                     
c              Done.  Let's go find a lat./lon. pair !
               sta1 = stalat(iusesta)
               sta2 = stalon(iusesta)
               call latlon2 (sta1, sta2, dis(1), useazim, a1, a2)
               alat0 = a1
               alon0 = a2
               if (prtflg.eq.'y') call prtcut (alat0, alon0, luout)
               return
            end if
 1330    continue
      end if

c     # Look here for 3 closest arrvial times

c     Use point near station with earlist arrival time.  Use the azimuth
c     at that station, if there is one.  Probably will become unneccesary!

      if (icompr.gt.0) then
         iusesta = indexcompr(1)
         dis(1)  = defaultdist
         if (goodazim(iusesta)) then
            useazim = bestazim(iusesta)
         else
            useazim = defaultazim
         end if

         if (prtflg.eq.'y') write (luout,'(a/2a,3(/a,f7.2,a))')
     &       '        Method: Station with earliest arrival time',
     &       '       Station: ', staid(iusesta),
     &       ' Earliest time: ', ordercompr(1), ' sec.',
     &       '      Distance: ', dis(1), ' deg.',
     &       '       Azimuth: ', useazim, ' deg.'
                     
c     Use best determied azimuth 

      else if (iazim.gt.0) then
         iusesta = indexdsd(1)
         dis(1)  = defaultdist
         if (goodazim(iusesta)) then
            useazim = bestazim(iusesta)
         else
            useazim = defaultazim
         end if
         if (prtflg.eq.'y') write (luout,'(a/2a,3(/a,f7.2,a))')
     &       '    Method: Station with smallest azimuth S.D.',
     &       '   Station: ', staid(iusesta),
     &       '  Distance: ', dis(1), ' deg.',
     &       '   Azimuth: ', useazim, ' deg.',
     &       ' Best S.D.: ', bestazim(iusesta), ' deg.'
                     

c     Finally try station with the best slowness (as defined by the 
c     smallest s.d.) and the default azimuth

      else if (islow.gt.0) then
         iusesta = indexdsd2(1)
         useslow = bestslow(iusesta)
         do 1340  j = 1, maxdist-1
            if (useslow.lt.slowness(j) .and. useslow.ge.slowness(j+1))
     &         dis(1) = (distance(j+1) - distance(j))*
     &                  (useslow - slowness(j))/
     &                  (slowness(j+1) - slowness(j)) + distance(j)
 1340    continue
         useazim = defaultazim

         if (prtflg.eq.'y') write (luout,'(a/2a,3(/a,f7.2,a))')
     &       '    Method: Station with largest slowness',
     &       '   Station: ', staid(iusesta),
     &       '  Slowness: ', useslow, ' sec./deg.',
     &       '  Distance: ', dis(1), ' deg.',
     &       '   Azimuth: ', useazim, ' deg.'
                     

c     Bail out!

      else
         if (prtflg.eq.'y') write (luout,'(a/a/)') 
     &          ' Initial location procedure failed - Bailing out',
     &          ' ==============================================='
         ierr = 1
         return
      end if

c     Done !

      sta1 = stalat(iusesta)
      sta2 = stalon(iusesta)
      call latlon2 (sta1, sta2, dis(1), useazim, a1, a2)
      alat0 = a1
      alon0 = a2
      if (prtflg.eq.'y') call prtcut (alat0, alon0, luout)

      return
      end


      subroutine prtcut (alat, alon, luout)

      implicit none    ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      integer*4 luout
      real*4    alat, alon
      character ew*2, ns*2

      ew = ' E'
      ns = ' N'
      if (alon.lt.0.0) ew = ' W'
      if (alat.lt.0.0) ns = ' S'
      write (luout,'(2(a,f7.2,2a/),a/)')
     &       '  Latitude: ', abs(alat), ' deg.', ns,
     &       ' Longitude: ', abs(alon), ' deg.', ew,
     &       ' ==============================================='

      return
      end


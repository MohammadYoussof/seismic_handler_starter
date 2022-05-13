c
c     NAME
c       farrloc  -  Computes event location parameters given
c 	            measurements from a teleseismic array.
c
c     FILE
c       farrloc.f
c
c     SYNOPSIS
c       Program computes the latitude, longitude, travel time, and
c       confidence bounds given the azimuth, horizontal slowness,
c       and standard deviations of these
c       parameters measured from one teleseismic P-wave recorded at
c       an array or three component station. A depth for the event
c       is also assumed.  The distance for the event 
c       is computed by interploating on slowness as a function of depth
c       and distance (table found in data statement). The travel-time
c       is also found by interploation on  travel-time as a function of
c       depth and distance.
c       The tables were calculated using the program mkdata, which calls
c       ttcalx.  The depth intervals are (0,33,100,700).  The distance 
c       interval is 5 degrees. For 105 degree distance the value in the
c       travel-time tables gives an error so this was omitted.
c       The slowness values for up-going rays were also change to:
c       for depth = 33.0 (i=2) dtdd(1,2) from 12.54 to 13.75,
c       for depth = 100.0 (i=3) dtdd(1,3) from 6.60 to 13.68,
c       for depth = 700.0 (i=4) dtdd(1,4) from 1.00 to 10.34,
c                               dtdd(2,4) from 7.83 to 10.34,
c                               dtdd(3,4) from 10.20 to 10.34.
c
c       Values are returned in single float arrays of dimension (1)
c       so that they may be accepted by Lisp.
c       
c
c     DESCRIPTION
c      function farrloc (stalat,stalon,staelv,dist,sdtime,
c                        slowns,sdslowns,seaz,sdseaz,depth,pconf,
c                        evlat,evlon,trvltime,smajax,sminax,strike,
c                        szz,stt,sdobs,err)
c
c       Input:
c         phase = phase type.
c         stalat = station latitude (degrees).
c         stalon = station longitude (degrees).
c         staelv = station elevation (km).
c         dist(1) = distance from station to event if available (deg).
c         sdtime = standard deviation in arrival time (sec).
c         slowns = horizontal slowness of P-wave (sec/deg) .
c         sdslows = standard deviation in horizontal slowns (sec/deg).
c         seaz = azimuth measurement of P-wave (degrees).
c         sdseaz = standard deviation in azimuth (degrees).
c         depth = assumed depth.
c         pconf = confidence to which error is calculated (0.0 - 1.0).
c
c       Output:
c         evlat(1) = event latitude.
c         evlon(1) = event longitude.
c         trvltime(1) = P-wave travel time (subtract from P-wave
c                       arrival time to get origin time).
c         smajax(1) = semi-major axis of confidence ellipse (km).
c         sminax(1) = semi-minor axis of confidence ellipse (km).
c         strike(1) = strike of confidence ellipse (degrees).
c         szz(1) = depth uncertainty.
c         stt(1) = origin time uncertainty.
c         sdobs(1) = standard error of location solution.
c         dist(1) = distance from station to event(deg).
c         err(1) = error code: = 0 if no problems.
c             	                 = 1 if problems.
c
c       Subroutines called by farrloc:
c         interpl = linear interpolation of a function.
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
c       Donna Williams, Steve Bratt
c
c     #END
c
c
c
      function farrloc (stalat,stalon,staelv,dist,sdtime,
     &                  slowns,sdslowns,seaz,sdseaz,depth,pconf,
     &                  evlat,evlon,trvltime,smajax,sminax,strike,
     &                  szz,stt,sdobs,err)
c
      real farrloc
      dimension z(4),delta(36),tt(36,4),dtdd(36,4)
      dimension evlat(1),evlon(1),trvltime(1),smajax(1),sminax(1)
      dimension strike(1),szz(1),stt(1),sdobs(1),dist(1) 
      dimension err(1)
c
	common /sccsfarrloc/ sccsid
	character*80 sccsid
	data sccsid /'@(#)farrloc.f	36.1	3/6/89'/
      data z/  0.0, 33.0,100.0,700.0/
      data delta/  0.0,  5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0,
     &            40.0, 45.0, 50.0, 55.0, 60.0, 65.0, 70.0, 75.0,
     &            80.0, 85.0, 90.0, 95.0,100.0,110.0,115.0,120.0,
     &           125.0,130.0,135.0,140.0,145.0,150.0,155.0,160.0,
     &           165.0,170.0,175.0,180.0/
c
      data tt(1,1),tt(2,1),tt(3,1),tt(4,1),tt(5,1),tt(6,1),tt(7,1)
     &/0.00,75.99,144.19,211.48,273.32,324.39,369.51/
      data tt(8,1),tt(9,1),tt(10,1),tt(11,1),tt(12,1),tt(13,1),tt(14,1)
     &/413.34,455.70,496.36,535.20,572.21,607.41,640.91/
      data tt(15,1),tt(16,1),tt(17,1),tt(18,1),tt(19,1),tt(20,1)
     &/672.74,702.63,730.63,756.62,780.73,803.90/
      data tt(21,1),tt(22,1),tt(23,1),tt(24,1),tt(25,1),tt(26,1)
     &/826.73,1111.24,1121.25,1131.15,1140.94,1150.63/
      data tt(27,1),tt(28,1),tt(29,1),tt(30,1),tt(31,1),tt(32,1)
     &/1160.13,1169.32,1178.10,1186.31,1193.63,1199.77/
      data tt(33,1),tt(34,1),tt(35,1),tt(36,1)
     &/1204.67,1208.26,1210.47,1211.29/
c
      data tt(1,2),tt(2,2),tt(3,2),tt(4,2),tt(5,2),tt(6,2),tt(7,2)
     &/5.17,72.84,140.99,208.12,269.38,320.03,365.07/
      data tt(8,2),tt(9,2),tt(10,2),tt(11,2),tt(12,2),tt(13,2),tt(14,2)
     &/408.86,451.17,491.77,530.55,567.52,602.67,636.13/
      data tt(15,2),tt(16,2),tt(17,2),tt(18,2),tt(19,2),tt(20,2)
     &/667.91,697.76,725.72,751.67,775.76,798.91/
      data tt(21,2),tt(22,2),tt(23,2),tt(24,2),tt(25,2),tt(26,2)
     &/821.75,1106.10,1116.12,1126.01,1135.81,1145.50/
      data tt(27,2),tt(28,2),tt(29,2),tt(30,2),tt(31,2),tt(32,2)
     &/1154.99,1164.18,1172.96,1181.17,1188.48,1194.61/
      data tt(33,2),tt(34,2),tt(35,2),tt(36,2)
     &/1199.51,1203.10,1205.30,1206.12/
c
      data tt(1,3),tt(2,3),tt(3,3),tt(4,3),tt(5,3),tt(6,3),tt(7,3)
     &/13.63,72.16,139.49,205.14,264.24,313.74,358.56/
      data tt(8,3),tt(9,3),tt(10,3),tt(11,3),tt(12,3),tt(13,3),tt(14,3)
     &/402.23,444.39,484.83,523.46,560.29,595.32,628.66/
      data tt(15,3),tt(16,3),tt(17,3),tt(18,3),tt(19,3),tt(20,3)
     &/660.33,690.07,717.93,743.78,767.80,790.93/
      data tt(21,3),tt(22,3),tt(23,3),tt(24,3),tt(25,3),tt(26,3)
     &/813.77,1097.74,1107.75,1117.64,1127.44,1137.12/
      data tt(27,3),tt(28,3),tt(29,3),tt(30,3),tt(31,3),tt(32,3)
     &/1146.61,1155.79,1164.56,1172.76,1180.06,1186.18/
      data tt(33,3),tt(34,3),tt(35,3),tt(36,3)
     &/1191.07,1194.65,1196.85,1197.69/
c
      data tt(1,4),tt(2,4),tt(3,4),tt(4,4),tt(5,4),tt(6,4),tt(7,4),
     &tt(8,4),tt(9,4)
     &/78.93,98.45,139.73,185.83,231.05,274.94,317.77,359.25,399.23/
      data tt(10,4),tt(11,4),tt(12,4),tt(13,4),tt(14,4),tt(15,4),
     &tt(16,4),tt(17,4),tt(18,4)
     &/437.60,474.35,509.45,542.98,574.96,605.18,633.62,660.24,684.93/
      data tt(19,4),tt(20,4),tt(21,4),tt(22,4),tt(23,4),tt(24,4),
     &tt(25,4),tt(26,4)
     &/708.37,731.30,754.12,1033.49,1043.47,1053.35,1063.12,1072.77/
      data tt(27,4),tt(28,4),tt(29,4),tt(30,4),tt(31,4),tt(32,4),
     &tt(33,4),tt(34,4)
     &/1082.21,1091.32,1100.02,1108.09,1115.21,1121.18,1125.94,1129.42/
      data tt(35,4),tt(36,4) /1131.56,1132.39/
cc
      data dtdd(1,1),dtdd(2,1),dtdd(3,1),dtdd(4,1),dtdd(5,1),dtdd(6,1),
     &dtdd(7,1),dtdd(8,1),dtdd(9,1),dtdd(10,1)
     &/18.52,13.68,13.57,13.17,11.25,9.33,8.88,8.62,8.30,7.94/
      data dtdd(11,1),dtdd(12,1),dtdd(13,1),dtdd(14,1),dtdd(15,1),
     &dtdd(16,1),dtdd(17,1),dtdd(18,1),dtdd(19,1),dtdd(20,1),
     &dtdd(21,1),dtdd(22,1),dtdd(23,1)
     &/7.58,7.22,6.86,6.53,6.18,5.78,5.40,4.97,4.70,4.58,4.56,2.01,1.99/
      data dtdd(24,1),dtdd(25,1),dtdd(26,1),dtdd(27,1),dtdd(28,1),
     &dtdd(29,1),dtdd(30,1),dtdd(31,1),dtdd(32,1),dtdd(33,1),
     &dtdd(34,1),dtdd(35,1),dtdd(36,1)
     &/1.97,1.95,1.92,1.87,1.80,1.71,1.57,1.35,1.11,0.85,0.58,0.30,0.04/
c
      data dtdd(1,2),dtdd(2,2),dtdd(3,2),dtdd(4,2),dtdd(5,2),dtdd(6,2),
     &dtdd(7,2),dtdd(8,2),dtdd(9,2),dtdd(10,2),dtdd(11,2),dtdd(12,2)
     &/13.75,13.75,13.62,13.16,11.19,9.33,8.91,8.66,8.33,7.97,7.61,7.25/
      data dtdd(13,2),dtdd(14,2),dtdd(15,2),dtdd(16,2),dtdd(17,2),
     &dtdd(18,2),dtdd(19,2),dtdd(20,2),dtdd(21,2),dtdd(22,2),dtdd(23,2),
     &dtdd(24,2),dtdd(25,2)
     &/6.89,6.56,6.20,5.81,5.42,4.99,4.72,4.61,4.58,2.02,2.00,1.98,1.96/
      data dtdd(26,2),dtdd(27,2),dtdd(28,2),dtdd(29,2),dtdd(30,2),
     &dtdd(31,2),dtdd(32,2),dtdd(33,2),dtdd(34,2),dtdd(35,2),dtdd(36,2)
     &/1.93,1.88,1.81,1.71,1.58,1.35,1.11,0.85,0.58,0.30,0.04/
c
      data dtdd(1,3),dtdd(2,3),dtdd(3,3),dtdd(4,3),dtdd(5,3),dtdd(6,3),
     &dtdd(7,3),dtdd(8,3),dtdd(9,3),dtdd(10,3),dtdd(11,3),dtdd(12,3)
     &/13.68,13.68,13.60,12.87,10.95,9.32,8.99,8.72,8.39,8.03,7.66,7.30/
      data dtdd(13,3),dtdd(14,3),dtdd(15,3),dtdd(16,3),dtdd(17,3),
     &dtdd(18,3),dtdd(19,3),dtdd(20,3),dtdd(21,3),dtdd(22,3),dtdd(23,3),
     &dtdd(24,3),dtdd(25,3)
     &/6.94,6.60,6.24,5.85,5.46,5.03,4.76,4.66,4.64,2.04,2.02,2.00,1.98/
      data dtdd(26,3),dtdd(27,3),dtdd(28,3),dtdd(29,3),dtdd(30,3),
     &dtdd(31,3),dtdd(32,3),dtdd(33,3),dtdd(34,3),dtdd(35,3),dtdd(36,3)
     &/1.95,1.90,1.82,1.73,1.59,1.36,1.12,0.86,0.59,0.30,0.04/
c
      data dtdd(1,4),dtdd(2,4),dtdd(3,4),dtdd(4,4),dtdd(5,4),dtdd(6,4),
     &dtdd(7,4),dtdd(8,4),dtdd(9,4),dtdd(10,4),dtdd(11,4),dtdd(12,4)
     &/10.34,10.34,10.34,10.34,9.96,9.75,9.47,9.15,8.80,8.43,8.07,7.70/
      data dtdd(13,4),dtdd(14,4),dtdd(15,4),dtdd(16,4),dtdd(17,4),
     &dtdd(18,4),dtdd(19,4),dtdd(20,4),dtdd(21,4),dtdd(22,4),dtdd(23,4),
     &dtdd(24,4),dtdd(25,4)
     &/7.36,6.99,6.58,6.19,5.76,5.37,5.19,5.13,5.12,2.25,2.23,2.21,2.18/
      data dtdd(26,4),dtdd(27,4),dtdd(28,4),dtdd(29,4),dtdd(30,4),
     &dtdd(31,4),dtdd(32,4),dtdd(33,4),dtdd(34,4),dtdd(35,4),dtdd(36,4)
     &/2.15,2.09,2.00,1.89,1.72,1.47,1.21,0.93,0.63,0.32,0.04/
 
      degtokm = 111.195
      degtorad = 0.017453
      ndepth = 4

      evlon(1) = -999.
      evlat(1) = -999.
      trvltime(1) = -999.
      smajax(1) = -999.
      sminax(1) = -999.
      strike(1) = -999.
      szz(1) = -999.
      stt(1) = -999.
      sdobs(1) = -999.
      farrloc = 0.0
      err(1) = 0
  
      slownx = slowns

c Find depth in tables that is closest to desired depth.

      do 100 i = 1,ndepth-1
         if(depth.ge.z(i) .and. depth.lt.z(i+1)) then
	    if (abs(depth - z(i)) .lt. abs(depth - z(i+1))) then
	      idepth = i
            else
	      idepth = i+1
            endif
	    goto 50
          endif

100   continue
      idepth = ndepth

50    continue

c Check to see if a valid distance exists.
 
      if (dist(1).gt.0.0) then
          goto 200
      endif

c Interpolate to find event distance given phase slowness.

      call interpl(36,dtdd(1,idepth),delta,slownx, dist(1),ierri)
      if (ierri.eq.1) go to  255

c Compute location given distance and azimuth from station.

200   call latlon (stalat,stalon,dist(1),seaz, evlat(1),evlon(1))

c Compute travel time given distance.

      call interpl(36,delta,tt(1,idepth),dist(1), trvltime(1),ierri)
      if (ierri.eq.1) go to  255

c For now, kludge the confidence bounds (FIX).
 
      azierror = 2.*degtokm*sdseaz*sin(degtorad*dist(1))
c
c See Shlien and Toksoz (1973) raderror is approximately equal to 
c  the sd of the slowness divided by the second derivative of time
c  wrt distance (=~ .05 sec**2/deg**2 between 40 and 80 deg distance)
c
      raderror = 2.*degtokm*sdslowns/0.05
      if (azierror.gt.raderror) then
        smajax(1) = azierror
        sminax(1) = raderror
        strike(1) = seaz - 90.
      else
        smajax(1) = raderror
        sminax(1) = azierror
        strike(1) = seaz
      endif
      if (abs(strike(1)).gt.180.) 
     &    strike(1) = -sign(360. - abs(strike(1)),strike(1))
      if (abs(strike(1)).gt.90.) 
     &    strike(1) = -sign(180. - abs(strike(1)),strike(1))
      szz(1) = 20.
      stt(1) = 2.*sdtime
      sdobs(1) = 1.0
      err(1) = 0
      farrloc = 1.0
      return

255   write(6,*) ' Error: No depth or distance found'
      farrloc = 0.0
      err(1) = 1
c
      return
      end

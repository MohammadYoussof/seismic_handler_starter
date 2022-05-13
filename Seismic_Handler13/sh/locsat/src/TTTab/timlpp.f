      function timlpp(dist,deph)
c**********************************************************************
c
c
c        purpose    - to estimate travel times for the .pp phase
c
c        date       - 12 november 1976 (last revision - 1 dec 1977)
c
c        programmer - john lambert
c
c**********************************************************************
c
c        parameters
c
c           input
c
c              dist   - r*4 - event to station distance in degrees
c
c              deph   - r*4 - event depth in kilometers
c
c           output
c
c              timlpp    =    travel time in seconds for phase .pp
c
c
c**********************************************************************
      real*4 c(9),d1(5),d2(5),dp(5)

	common /sccstimlpp/ sccsid
	character*80 sccsid
	data sccsid /'@(#)timlpp.f	31.1	8/26/88'/

      data c/ 3.837437441e-01,-2.077023938e-02, 1.077586036e-02,
     2        4.286477915e-04,-2.517302465e-04, 2.073143175e-05,
     3       -2.731592749e-06, 1.817303722e-06,-2.012145067e-07/
      data d1/34.7,22.0,21.2,16.9,2.0/,d2/0.1,0.02666,0.015,0.05625,0.5/
      data dp/-33.0,+16.6667,+19.0,+13.3625,-17.5/
      timlpp = 0.
CCC GONCZ      if (deph.lt.1.0.or.deph.gt.800.0) go to 40
      if (deph.gt.800.0) go to 40
      if (dist.gt.102.0) go to 40
      if (dist.ge.47.0)  go to 30
      do 10 i=1,5
      if  (dist.ge.d1(i) .and. dist.ge.(d2(i)*deph+dp(i)))  go to 30
 10   continue
      go to 40
 30   x1 = 0.0
         do 35 i=1,9
 35      x1 = x1 + c(i) * func(i,dist,deph)
      x2=dist-x1
      call pastim (1,x1,deph,t1)
      call pastim (1,x2,0.0,t2)
      timlpp = t1 + t2
 40   return
      end

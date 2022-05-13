      subroutine trvltm (delt,deph,nphaz,time)
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
c
c        purpose    - to estimate travel times of core phases and
c                        surface reflected phases by computing
c                        a polynomial with coefficients
c
c        date       - updated june 1978
c
c        programmer - john lambert
c
c        parameters
c
c           input
c
c              delt   - r*4 - event to station distance in degrees
c
c              deph   - r*4 - event depth in kilometers
c
c              nphaz  - i*2 - index of the desired phase
c     1 PG        2 ScP       3 PcP  
c     4 pP        5 sP        6 PP
c     7 P         8 PPP       9 SP
c     10 SPP      11 SKPdf    12 SKPab
c     13 PKPdf    14 PKPab    15 PKPbc
c     16 PKPcd    17 PKKPdf   18 PKKPbc
c     19 PKKPab   20 PKPPKP   21 (none)
c     22 PcS      23 ScS      24 SKSac
c     25 SKSdf    26 S        27 pS 
c     28 sS       29 PS       30 PPS
c     31 SS       32 SSS      33 (none)
c     34 (none)   35 (none)   36 LR-Rayleigh
c     37 LQ-Love  38 Pn       39 Pg
c     40 Sn       41 Lg       42 Rg
c
c The last seven phases are computed with a separate
c  velocity stucture (in file velocity.dat)
c  that may be changed by the user.
c
c Also note that no amplitudes are computed for regional phases
c  at this time.
c
c
c           output
c
c              time   - r*4 - travel time in seconds for the phase
c
c
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      include '../../include/ptabls.h'
      include '../../include/phtbls.h'
      include '../../include/numcon.h'
c
c      integer*2 nphaz,np

	common /sccstrvltm/ sccsid
	character*80 sccsid
	data sccsid /'@(#)trvltm.f	31.1	8/26/88'/

      np=nphaz
      time = 0.0
      if (deph.gt.800.0) go to 999
      if ((np.lt.1).or.(np.gt.32)) go to 999
      dist=delt
      ind = nid(np)
      go to (20,50,50,30,14,10,40,70,70,70,50,50,40,50,50,50,50,50,50,
     *      50,999,50,50,50,50,40,14,12,70,70,70,70,999), np
c
c  *  phase  pP not present at less than 2 degrees
c
 10   if (dist - 2.0) 999,70,70
c
c  *  phase sS not present at less than 5 degrees
c
 12   if (dist - 5.0) 999,14,14
c
c  *  phases sP, pS, and sS not present for surface events and are
c  *     obscured by primary phases at depths less than 1 km
c
 14   if (deph - 1.0) 999,70,70
c
c  *  phase Pg maximum limits
c
 20   if ((30.-deph).lt.(3.845*dist)) go to 999
c20   if ((30.-deph).lt.(3.846*dist)) go to 999
      time = sqrt((deph*km2dg)**2 + delt**2) * 18.53
      go to 999
 30   time = timlpp(dist,deph)
      go to 999
 40   call pastim (ind,dist,deph,time)
      go to 999
c
c  *  minimum and maximum distance limits of core phase
c
 50   if (dist.lt.phlc(ind)) go to 999
      if (dist.gt.phhc(ind)) go to 999
c
c  *  compute core phase travel time
c
         do 60 k=1,7
 60      time = time + phtc(k,ind) * func(k,dist,deph)
      go to 999
c
c  *  maximum distance limit of surface reflected phase at depth deph
c
 70   ctof = phco(7,ind) + phco(8,ind) * deph
      if (dist.gt.ctof) go to 999
c
c  *  minimum distance limit of surface reflected phase at depth deph
c
      ctof = 0.0
      dfcl = 1.0
         do 72 k=1,6
         ctof = ctof + dfcl * phco(k,ind)
 72      dfcl = dfcl * deph
      if (dist.lt.ctof) go to 999
c
c  *  compute length of first segment of surface reflected phase
c
 74   x1 = 0.0
         do 80 k=1,5
 80      x1 = x1 + phdc(k,ind) * func(k,dist,deph)
      if (x1.ge.0.) go to 100
 90   x1 = 0.0
c
c  *  compute travel time of first segment
c
 100  go to (370,360,360,370,370,360,370,360,380,370,370), ind
 360  call pastim (1,x1,deph,t1)
      time = time + t1
      goto390
 370  call pastim (3,x1,deph,t1)
      time = time + t1
      goto390
  380 x0=dist-x1
      call pastim (3,x0,0.0,t2)
      time = time + t2
      ind = 2
      dist=x1
      go to 74
c
c  *  compute lengths and travel times of remaining segment(s)
c
 390  go to (400,400,410,400,410,420,420,420,400,420,430), ind
  400 x2= dist-x1
      call pastim (1,x2,0.0,t2)
      time = time + t2
      go to 998
  410 x2=(dist-x1)/2.0
      call pastim (1,x2,0.0,t2)
      time = time + t2 + t2
      go to 998
  420 x2=dist-x1
      call pastim (3,x2,0.0,t2)
      time = time + t2
      go to 998
  430 x2=(dist-x1)/2.0
      call pastim (3,x2,0.0,t2)
      time = time + t2 + t2
 998  if (t2.le.0.0) time = 0.0
  999 return
      end

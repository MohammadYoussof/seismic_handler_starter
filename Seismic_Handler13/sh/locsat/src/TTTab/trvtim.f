      subroutine trvtim (delt,hhy,time,idph,vtdhy)
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
c        purpose    - to compute travel times for p and pkp phases and
c                       identify the phase.
c
c        programmer - adapted from uxuy and hypo by john lambert
c
c        date       - 23 july 1976
c
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
c        parameters
c
c          input  - glahy  - r*4 - co-latitude of the event in radians.
c                   glohy  - r*4 - east-longitude of the event in
c                                    radians.
c                   hhy    - r*4 - event depth in kilometers
c
c          output - time   - r*4 - travel time in seconds
c                   idph   - i*4 - phase identifier index
c                                    0 - not P or PKP (probably in
c                                          shadow zone)
c                                    7 - P
c                                   13 - PKP
c
c                   vtdhy  - r*4 - apparent velocity from event to
c                                  station in kilometers/sec
c
c
      include '../../include/numcon.h'
      include '../../include/ptabls.h'
      include '../../include/iaapaq.h'
c
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
c
c      integer*2 idxpo,ksave,j
      dimension hvel(2)

	common /sccstrvtim/ sccsid
	character*80 sccsid
	data sccsid /'@(#)trvtim.f	31.1	8/26/88'/

      data iswch/0/
      idxpo = 0
c
c  ****  compute velocity for input depth  ****
c
      call setvel(hhy,ksave,vch,hvel)
c
c  ****  compute travel time  ****
c
      dlshy=delt
      if (dlshy.lt.szmin) go to 100
      if (dlshy.lt.szmax) go to 90
      ki=47
      idph = 13
      go to 110
   90 idph = 0
      time = 0.
      return
  100 ki=3
      idph = 7
  110 do 120 j=ki,56
      if (dlshy.le.dpth(j)) go to 130
  120 continue
  130 call ttime(dlshy,j,idxpo,ksave,hhy,vch,
     5   hvel,ptdhy,pthhy,time)
c
c  ***  addition of apparent velocity (jwl) sept 1979
c
      vtdhy = dg2km/ptdhy
c
      return
      end

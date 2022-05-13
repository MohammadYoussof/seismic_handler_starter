      subroutine setvel (hhy,ksave,vch,hvel)
c
c *** this subroutine computes the velocity for the input depth
c *** necessary for computing travel times.
c
      include '../../include/ptabls.h'
      dimension hvel(2)
c////            - -   n o t e    - -
c        it is assumed that velocity of surface depth is stored
c          stored in hvvth(1,2)
c      integer*2 ksave

	common /sccssetvel/ sccsid
	character*80 sccsid
	data sccsid /'@(#)setvel.f	31.1	8/26/88'/

      if(hhy.ne.0.0)go to 20
      vch=hvvth(1,2)
      ksave=2
      hvel(1)=hvvth(2,2)
      hvel(2)=hvvth(1,2)
      return
   20 do  30 ksave=2,9
      if(hhy.le.depth(ksave))go to  40
   30 continue
   40 do  50 ki=2,17
      if(depth(ksave).le.hvvth(ki,1))go to  60
   50 continue
c****    compute velocity for deeper stored depth
   60 hvel(1)=(depth(ksave)-hvvth(ki-1,1))/(hvvth(ki,1)-hvvth(ki-1,1))
     1  *(hvvth(ki,2)-hvvth(ki-1,2))+hvvth(ki-1,2)
c****    compute velocity for shallower stored depth
      ki=ki-1
      do  70 kj=1,ki
      kk=ki-kj+1
      if(depth(ksave-1).ge.hvvth(kk,1))go to  80
   70 continue
   80 hvel(2)=(depth(ksave-1)-hvvth(kk,1))/(hvvth(kk+1,1)-hvvth(kk,1))
     1  *(hvvth(kk+1,2)-hvvth(kk,2))+hvvth(kk,2)
c****    compute velocity for current depth
      do 90 kk=2,17
      if (hhy.le.hvvth(kk,1)) go to 100
   90 continue
  100 vch=(hhy-hvvth(kk-1,1))/(hvvth(kk,1)-hvvth(kk-1,1))
     1  *(hvvth(kk,2)-hvvth(kk-1,2))+hvvth(kk-1,2)
      return
      end

      subroutine pastim(jump,delt,deph,time)
c****    revised october 1975    ****
c****    revised november 1975    ****
c****    revised november 12, 1976 ***
c        parameters -
c          input - jump  - i*4 - 1 - compute P arrival time
c                                2 - compute PKP arrival time
c                                3 - compute S arrival time
c                  delt  - r*4 - distance in degrees
c                  deph  - r*4 - depth in kilometers
c          output - time - r*4 - travel time in seconds for the
c                                  selected phase
      include '../../include/phtbls.h'
      include '../../include/ptabls.h'
c *
c      integer*2 id,ih
c      integer*2 i,j,k,l,m,n,kt
      dimension x(4),y(4),t(2),n(6)

	common /sccspastim/ sccsid
	character*80 sccsid
	data sccsid /'@(#)pastim.f	31.1	8/26/88'/

      data n/2,3,4,1,2,3/
c
      time = 0.0
      go to (3,1,3), jump
 1    if(delt.lt.dpth(48))  return
      do 2 id=48,55
      if(delt.le.dpth(id))  go to 20
 2    continue
      return
 3    do 10 id=3,44
      if(delt.le.dpth(id))  go to 20
   10 continue
      return
   20 do 30 ih=2,9
      if(deph.le.depth(ih))  go to 40
   30 continue
      ih=9
 40   m = ih - 1
      kt = 1
      xx = delt
 50   continue
      if (m.gt.ih) go to 100
      j = id - 2
      do 80 i=1,4
      x(i) = dpth(j)
      go to (60,60,70), jump
 60   y(i) = tpth(j,m)
      go to 80
 70   y(i) = tsth(j,m)
 80   j = j + 1
      t(kt) = 0.
      do 90 i=1,4
      j = n(i)
      k = n(i+1)
      l = n(i+2)
 90   t(kt) = t(kt) + y(i) * (xx-x(j)) * (xx-x(k)) * (xx-x(l))/
     *                ((x(i)-x(j)) * (x(i)-x(k)) * (x(i) - x(l)))
      kt = 2
      m = m + 1
      go to 50
 100  time = (t(2)-t(1)) * (deph-depth(ih-1))/(depth(ih)-depth(ih-1))
     *       + t(1)
      return
      end

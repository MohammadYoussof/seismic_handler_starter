      subroutine ttime(delta,j,idxpo,ksave,hhy,
     1    vch,hvel,disdv,dphdv, time)
c
c....    parameters
c          delta - distance in degrees
c          j     - index position of the next greater stored distance
c          idxpo - value of index of previous j, set to zero for first
c                    entry
c          ksave - index position of greater depth table
c          hhy   - depth in km. of event
c          vch   - velocity at event@s depth
c          hvel  - 2 dimensioned array for velocities at stored depths
c          disdv - distance derivative in seconds per degree
c          dphdv - depth derivative in seconds per kilometer
c          time  - travel time in seconds for distance delta
c          the following arrays are necessary and in common -
c            dpth - stored distances in degrees
c            tpth - stored travel times for all depths in seconds
c            depth - depths of travel time time tables in kilometers
c
      include '../../include/ptabls.h'
      include '../../include/numcon.h'
      dimension d1(2),d2(2),dh(4),tx(4),hvel(2)
c      integer*2 j,idxpo,ksave

	common /sccsttime/ sccsid
	character*80 sccsid
	data sccsid /'@(#)ttime.f	31.1	8/26/88'/

      x=delta-dpth(j-2)
      if(j.eq.idxpo)go to 20
      idxpo=j
      x1=dpth(j-1)-dpth(j-2)
      x2=dpth(j)-dpth(j-2)
      x3=dpth(j+1)-dpth(j-2)
      dx1=x1-x3
      dx2=x2-x1
      dx3=x3-x2
      xx1=x1*x3
      xx2=x2*x1
      xx3=x3*x2
      den1=-1.0/(xx1*x2)
      den2=-1.0/(x1*dx1*dx2)
      den3=-1.0/(x2*dx2*dx3)
      den4=-1.0/(x3*dx3*dx1)
      a11=(xx1+xx2+xx3)*den1
      a12=xx3*den2
      a13=xx1*den3
      a14=xx2*den4
      a21=(x1+x2+x3)*(-den1)
      a22=(x2+x3)*(-den2)
      a23=(x1+x3)*(-den3)
      a24=(x1+x2)*(-den4)
   20 do 60 l=1,2
      n=ksave-l+1
      b1=a11*tpth(j-2,n)+a12*tpth(j-1,n)+a13*tpth(j,n)+a14*tpth(j+1,n)
      b2=a21*tpth(j-2,n)+a22*tpth(j-1,n)+a23*tpth(j,n)+a24*tpth(j+1,n)
      b3=x*(den1*tpth(j-2,n)+den2*tpth(j-1,n)+den3*tpth(j,n)
     1  +den4*tpth(j+1,n))
      tx(l)=tpth(j-2,n)+x*(b1+x*(b2+b3))
      stor=b2+b2
      b3=b3+b3+b3
      d1(l)=b1+x*(stor+b3)
      d2(l)=stor+b3+b3
      stor=d1(l)*hvel(l)/(radeth-depth(n))*rd2dg
      stor=1.0-stor*stor
      if(stor.gt.0)  go to 40
      dh(l)=0.0
      go to 60
   40 dh(l)=sqrt(stor)/hvel(l)
      if(d2(l).ge.0)  go to 60
      dh(l)=-dh(l)
   60 continue
      x1=(hhy-depth(ksave))/(depth(ksave-1)-depth(ksave))
      disdv=x1*(d1(2)-d1(1))+d1(1)
      stor=(disdv*vch)/(radeth-hhy)*rd2dg
      stor=1.0-stor*stor
      if(stor.gt.0)  go to 80
      dphdv=0.0
      go to 110
   80 dphdv=sqrt(stor)/vch
      if(delta.ge.17.0)go to 100
      stor=x1*(d2(2)-d2(1))+d2(1)
      if(stor.ge.0)  go to 110
  100 dphdv=-dphdv
110   continue
      tx(1)=tx(1)+0.5*(depth(ksave)-hhy)*(dh(1)+dphdv)
      tx(2)=tx(2)-0.5*(hhy-depth(ksave-1))*(dh(2)+dphdv)
      time=x1*(tx(2)-tx(1))+tx(1)
      return
      end

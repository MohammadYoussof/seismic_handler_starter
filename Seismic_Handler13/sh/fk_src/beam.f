c subroutine zur berechnung des optimum-beams
c
c felder:
c   s(nn,lx)   : seismogramme
c                nn=anzahl der stationen
c                lx=anzahl der sample
c   it(nn,nb)  : delay-matrix
c                nb=anzahl der beams
c   vel(nb)    : geschwindigkeit des entsprechenden beams
c   az(nb)     : azimut des entsprechenden beams
c   dt
c   iz         : zeitfenster fuer beamberechnung: (i +-iz)*dt (integrationsbereich)
c   idt        : zeitinkrement der zeitfenster
c   idm        : maximale zeitverschiebung der delay-matrix in sample
c   i1         : feld-dimensionierung
c   i2         : feld-dimensionierung
c   i3         : feld-dimensionierung
c
      subroutine beam (s,nn,lx,it,nb,vel,az,iz,idt,dt,idm,i1,i2,i3)
c
      real s(i1,i2),vel(i3),az(i3),b,dt,aa,bb
      integer it(i1,i3),iz,idm,ia,ie
c
      ia=1+idm+iz
      ie=lx-idm-iz
ct      ia=111
ct      ie=ia
c
      nd=1
ct      write (80,*) nd
ct      write (80,*) nd,nb
c
      do 2 i=ia,ie,idt
        iai=i-iz
        iei=i+iz
        ta=(iai-1)*dt
        te=(iei-1)*dt
        b=0.
        do 3 k=1,nb
          bb=0.
          do 6 n=iai,iei
            aa=0.
            do 4 l=1,nn
              aa=aa+s(l,n+it(l,k))
4           continue
            bb=bb+aa*aa
6         continue
ct          write (80,*) az(k),bb
ct          write (80,*) vel(k),bb
          if (bb.gt.b) then
            b=bb
            ig=k
          end if
3       continue
        write (70,'(f5.1,f6.1,2f3.0,2f8.3,2f3.0)')
     *         vel(ig),az(ig),du,du,ta,te
2     continue
c
      return
      end

c subroutine zur berechnung des optimum-beams
c
c felder:
c   s(nn,lx)   : seismogramme
c                nn=anzahl der stationen
c                lx=anzahl der sample
c   b(lx)      : beamspur
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
      subroutine beamtr (s,nn,lx,ita,ite,b,itv,i1,i2,i3)
c
      real s(i1,i2),b(i2),dt
      integer itv(i1),ita,ite
c
      do 2 i=ita,ite
        do 2 l=1,nn
          ii=i+itv(l)
          if (ii.le.0.or.ii.ge.lx) goto 2
          b(i)=b(i)+s(l,ii)
2     continue
c
      return
      end

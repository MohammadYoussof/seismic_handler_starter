c subroutine zur normierung der spuren
c alle spuren werden auf maximale 
c amplitude von 10000 normiert
c
c variablen
c   s(n)   : seismogramme
c   iptr   : pointer fuer jeweiligen kannal
c   ipts   : anzahl der punkte pro kanal
c
      subroutine norms (s,iptr,ipts)
c
      real s(*)
      integer iptr,ipts
c
      nn=iptr+ipts-1
      am=0.
c
      do 1 i=iptr,nn
        am=max(abs(s(i)),abs(am))
1     continue
c
      do 2 i=iptr,nn
        s(i)=s(i)/am*10000.
2     continue
c
      return
      end

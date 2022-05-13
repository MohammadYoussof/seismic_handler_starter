c subroutine zur normierung der spuren
c
c felder:
c   s(n,lx)   : seismogramme
c                n=nummer der station
c                lx=anzahl der sample
c dimensionierung der felder:
c   s(i1,i2)
c
      subroutine norm (s,lx,n,i1,i2)
c
      real s(i1,i2)
      integer n,lx
c
      am=0.
      do 1 i=1,lx
        am=max(abs(s(n,i)),abs(am))
1     continue
c
      do 2 i=1,lx
        s(n,i)=s(n,i)/am*1000.
2     continue
c
      return
      end

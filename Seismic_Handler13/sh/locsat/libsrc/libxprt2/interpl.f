      subroutine interpl (n,x,y,xold,ynew,ierr)
      dimension x(n),y(n)
      real m
	common /sccsinterpl/ sccsid
	character*80 sccsid
	data sccsid /'@(#)interpl.f	36.1	3/6/89'/
      do 10 i=1,n-1
      	if ((xold.ge.x(i) .and. xold.le.x(i+1)) .or.
     &	    (xold.le.x(i) .and. xold.ge.x(i+1))) then
         m = (y(i+1) - y(i)) / (x(i+1) -x(i))
         b = y(i) - m * x(i)
         ynew = m * xold + b
         ierr = 0
         return
        endif
10    continue
      write(6,*) ' value of xold not found in array x'
      ierr = 1
c     if(xold.lt.x(1)) then
c ynew = y(1)
c        return
c     else if(xold.gt.x(n)) then
c ynew = y(n)
c        return
c     endif
      return
      end

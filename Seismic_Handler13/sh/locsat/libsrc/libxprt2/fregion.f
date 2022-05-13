c Function fregion.f.
c   Computes geographic and seismic region numbers and names
c    given latitude and longitude.
c
c
c  
c   INPUT:	flat - latitude of point or region.
c               flon - longitude of point or region.
c  
c   OUTPUT:     iseisnum(1) - seismic region number. 
c               seisnam(1) - seismic region name.
c               igeognum(1) - geographic region number.
c               geognam(1) - geographic region number.
c               ierr(1) = 0, no problem.
c                       = 1, ridiculous latitude and/or longitude.
c               Output returned in arrays for Lisp.
c 
c   SUBROUTINES CALLED: nmreg
c                       greg
c		        gtos
c      		        sreg
c                       

        function fregion (flat,flon, 
     &           geognam,seisnam,igeognum,iseisnum, ierr)
 

	integer iseisnum(1),igeognum(1),ierr(1)
        character*38 seisnam(1),geognam(1)
	common /sccsfregion/ sccsid
	character*80 sccsid
	data sccsid /'@(#)fregion.f	36.1	3/6/89'/

        ierr(1) = 0

	igeognum(1) = nmreg (flat,flon)

	if (igeognum(1).lt.0) then
	  ierr(1) = 1
	  return
        endif

        call greg (igeognum(1),geognam(1))

        call gtos (igeognum(1),iseisnum(1))

	if (iseisnum(1).lt.0) then
	  ierr(1) = 1
	  return
        endif

        call sreg (iseisnum(1),seisnam(1))

        return
	end

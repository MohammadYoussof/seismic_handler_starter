      subroutine cliget (cline,ierr)
c
c Using Unix utililies -- iargc and getarg -- store
c  an image of the command line arguments into a character string.
c
c Input: none
c
c Output
c
c   cline  : character*(*) string representing command line.
c   ierr   : error flag; 0 no error,
c                        1 command line arguments don't fit in cline
c
      character*(*) cline
	common/sccscliget/ sccsid
	character*80 sccsid
	data sccsid /'@(#)cliget.f	41.1	12/21/90'/
c
      lenc = len(cline)
      narg = iargc(idum)
      jc = 1
      cline = ' '
c
      do 50 i = 1,narg
      if (jc.gt.lenc) then
	ierr = 1
	return
      end if
      call getarg (i,cline(jc:))
      kc = jc + index(cline(jc+1:),' ') - 1
      if (kc.lt.jc) then
	ierr = 1
	return
      end if
      jc = kc + 2
50    continue
c
      ierr = 0
      return
      end

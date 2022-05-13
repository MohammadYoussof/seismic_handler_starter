c
c     NAME
c       fitime  -  Computes human-readable time from an epoch time.
c
c     FILE
c       fitime.f
c
c     SYNOPSIS
c       fitime gets the epoch time from the calling program in 
c       two parts.  This permits it to get information from
c       Lisp-based programs that don't have high enoug precision
c       to handle very large numbers.  It also returns results in
c       floating-point arrays of dimension (1) for Lisp.
c       This program is a varient of itime.f.
c       
c
c     DESCRIPTION
c      function fitime (secchar,secfloat,irefyr,
c                       iyr,ida,ihr,imin,isec,idsec,ierr)
c
c       Input:
c         secchar = character string containing the first digits of
c                the epoch time (if any).
c         secfloat = character string containing the first digits of
c                    the epoch time (if any).
c         irefyr = reference year (integer)
c
c       Output:
c         iyr(1) = year.
c         ida(1) = julian day.
c         ihr(1) = hours.
c         imin(1) = minutes.
c         isec(1) = seconds.
c         idsec(1) = deciseconds.
c         err(1) = error code: = 0 if no problems.
c             	                 = 1 if problems.
c
c       Subroutines called by fitime:
c         itime.f computes human (integer) time from epoch time.
c
c     DIAGNOSTICS
c        none
c
c     FILES
c        none
c
c     NOTES
c
c
c     SEE ALSO
c        none
c
c     AUTHOR
c       Steve Bratt
c
c     #END
c
c
c
      function fitime(secfloat,irefyr,
     &                iyr,ida,ihr,imin,isec,idsec,ierr)
c
      real*8 sec,secfloat
c      character*20 sechold,secall
c      character*3 secchar
      real*4 iyr(1),ida(1),ihr(1),imin(1),isec(1),idsec(1),ierr(1)
      real*4 irefyr
	common /sccsfitime/ sccsid
	character*80 sccsid
	data sccsid /'@(#)fitime.f	36.1	3/6/89'/
c
      ierr(1) = 0
c      secchar = '470'
      firstnum = 470000000.0
      sec = firstnum + secfloat

c Merge character and float times to get real epoch time.

c      write (sechold,'(f15.3)') secfloat
c      call range (sechold,j1,j2)
c      call range (secchar,i1,i2)
c      secall = secchar(i1:i2)//sechold(j1:j2)
c      read (secall,'(f15.3)') sec

      iref = irefyr
c      call itime(sec,iref,
c     &           iyrx,idax,ihrx,iminx,isecx,idsecx)
c      iyr(1) = iyrx
c      ida(1) = idax
c      ihr(1) = ihrx
c      imin(1) = iminx
c      isec(1) = isecx
c      idsec(1) = idsecx
      iyr(1) = 1970
      ida(1) = 1
      ihr(1) = 0
      imin(1) = 0
      isec(1) = 0
      idsec(1) = 0

      return
      end

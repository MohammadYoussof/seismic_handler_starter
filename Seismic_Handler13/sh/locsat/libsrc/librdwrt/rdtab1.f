
c NAME
c	rdtab1 \(em Read travel-time tables
 
c FILE
c	rdtab1.f

c SYNOPSIS
c	Read travel-time and amplitude tables for individual phase-types

c DESCRIPTION
c	Subroutine.  Read travel-time and amplitude tables applicable for
c	the given input phase-types

c	On entry \(em
c	filnam: Name of file to be read
c	maxtbd: Maximum number of distance samples in tables
c	maxtbz: Maximum number of depth samples in tables
c	luerr : Logical unit number for error output
 
c	On return \(em
c	ntbd: Number of distance samples in tables
c	ntbz: Number of depth samples in tables
c	   For i = 1, ntbd: tbd(i): Angular distance of (i,j)'th sample
c	   For j = 1, ntbz: tbz(j): Depth of (i,j)'th sample
c	   For i = 1, ntbd, j = 1, ntbz: tbtt(i,j): 
c	       Travel-time (i,j)'th sample (period for R wave)
c	ierr: Error flag; 0: No error
c			  1: File won't open
c			  2: Unexpected End-Of-File
 
c	Note: If file, filnam will not open, then arrays, ntbd() and ntbz()
c	are returned as zero.
 
c DIAGNOSTICS
c	Will specify when desired input file cannot be opened.

c AUTHOR
c	Steve Bratt


      subroutine rdtab1 (filnam, maxtbd, maxtbz, luerr, ntbd, ntbz,
     &                   tbd, tbz, tbtt, ierr)
 
      implicit none   ! K.S. 1-Dec-97, changed 'implicit' to 'none'

c     On entry

      integer*4 luerr, maxtbd, maxtbz
      character*(*) filnam

c     On return

      integer*4 ierr, ntbd, ntbz
      real*4    tbd(maxtbd), tbtt(maxtbd,maxtbz), tbz(maxtbz)

c     Internal variables

      integer*4 i, ios, j, k, lnblnk, ntbdx, ntbzx
      real*4    dum
      character string*80

      common /sccsrdtab1/ sccsid
      character*80 sccsid
      data sccsid /'@(#)rdtab1.f	36.2	3/17/89'/

 
c     Open file
 
      k = lnblnk(filnam)
      open (11, file = filnam, status = 'old', iostat = ios)
      if (ios.ne.0) then
         write (luerr, '(3a)') '? File ', filnam(1:k), ' will not open'
         ntbd = 0
         ntbz = 0
         ierr = 1
         return
      end if
 
c     Read title of the table.
 
      read (11, '(a)', end = 9000) string
 
c     Read depth sampling
 
      read (11, *, end = 9000) ntbzx
      ntbz = min(maxtbz, ntbzx)
      if (ntbzx.gt.maxtbz) then
         write (luerr, '(2a)') '? Too many depth samples in file ',
     &                         filnam(1:k)
         write (luerr, '(2(a,i4))') '  Number in file:', ntbzx,
     &                              '  Number kept:', maxtbz
      end if
      read (11, *, end = 9000) (tbz(i), i = 1, ntbz), 
     &                         (dum, i = ntbz+1, ntbzx)
 
c     Read distance sampling
 
      read (11, *, end = 9000) ntbdx
      ntbd = min(maxtbd, ntbdx)
      if (ntbdx.gt.maxtbd) then
         write (luerr, '(2a)') '? Too many distance samples in file ',
     &                         filnam(1:k)
         write (luerr, '(2(a,i4))') '  Number in file:', ntbdx,
     &                              '  Number kept:', maxtbd
      end if
      read (11, *, end = 9000) (tbd(i), i = 1, ntbd),
     &                         (dum, i = ntbd+1, ntbdx)
 
c     Read tables
 
      do 1000 j = 1, ntbz
         read (11, '(a)', end = 9000) string
         read (11, *, end = 9000) (tbtt(i,j), i = 1, ntbd),
     &                            (dum, i = ntbd+1, ntbdx)
 1000 continue
 
      close (11)
      ierr = 0
      return
 
c     Error
 
 9000 write (luerr, '(2a)') '? Unexpected End-Of-File for file ',
     &                      filnam(1:k)
      close (11)
      ierr = 2

      return
      end


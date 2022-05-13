
c NAME
c	rdtab -- Establish filename structures for reading tables

c FILE
c	rdtab.f

c SYNOPSIS
c	Create a file structures for reading the travel-time and amplitude 
c	tables.

c DESCRIPTION
c       Subroutine.  Read travel-time and amplitude tables applicable for
c       the given input phase-types for a given input file.  Read 
c	travel-time tables from files into memory.  The filenames to be
c	read have the form root//'.'//wavid(k), where root is the non-blank 
c	part of froot, and where wavid(k) is a wave identifier for k = 1, 
c	nwav; e.g., 'Pg', 'Pn', 'Sn', 'Lg'.  The data read is put into the 
c	appropriate arrays as indicated by the filename suffix.
c	Example: If froot = 'tab' and wavid = 'Pg', 'Lg', then this
c		 routine reads files 'tab.Pg' and 'tab.Lg'.

c       ---- On entry ----
c	froot : Root-name of files to be read
c	nwav  : Number of wave types to be used
c	maxtbd: i'th dimension of tbd(), tbtt() arrays
c	maxtbz: j'th dimension of tbz(), tbtt() arrays
c	wavid(k), k = 1, nwav: Character identifier of k'th wave

c       ---- On entry ----
c	ntbd(k),     k = 1, nwav: Number of distance samples in tables
c	ntbz(k),     k = 1, nwav: Number of depth samples in tables
c	tbd(i,k),    i = 1, ntdb(k), k = 1, nwav: Angular distance (deg)
c	tbz(j,k),    j = 1, ntbz(k), k = 1, nwav: Depth (km)
c	tbtt(i,j,k), i = 1, ntbd(k), j = 1, ntbz(k), k = 1, nwav: 
c		     Travel-time (sec, sec/deg)
c	ierr: Error flag; 0: No error
c			  1: One or more files won't open
c			  2: One or more files have unexpected EOF

c	---- Subroutines called ----
c	From librdwrt
c		rdtab1 - Read the actual travel-time and amplitude tables

c DIAGNOSTICS
c	Will specify if absolutely no input files exist.

c AUTHOR
c	Steve Bratt


      subroutine rdtab (froot, wavid, nwav, maxtbd, maxtbz, ntbd, ntbz,
     &                  tbd, tbz, tbtt, ierr)
 
      implicit none  ! K.S. 1-Dec-97, changed 'undefined' to 'none'

c     Parameter declaration

      integer*4 lenfn
      parameter (lenfn = 100)

c     On entry

      integer*4 maxtbd, maxtbz, nwav
      character*(*) froot, wavid(nwav)

c     On return

      integer*4 ierr, ntbd(nwav), ntbz(nwav)
      real*4    tbd(maxtbd,nwav), tbtt(maxtbd,maxtbz,nwav)
      real*4    tbz(maxtbz,nwav)

c     Internal variables

      integer*4 icnt, js, k, kr, lnblnk, luerr, nfiles
      character filnam*100

      common /sccsrdtab/ sccsid
      character*80 sccsid
      data sccsid /'@(#)rdtab.f	36.2	3/17/89'/
 
c     Set up
 
      kr = lnblnk(froot)
      filnam = froot(1:kr)
      js     = min(kr+1, lenfn-2)
      filnam(js:js) = '.'
      js     = js + 1
      nfiles = 0
 
c     Read tables
 
      icnt = 0
      do 1000 k = 1, nwav
         icnt = icnt + 1
         filnam(js:) = wavid(k)
         call rdtab1 (filnam, maxtbd, maxtbz, luerr, ntbd(k), ntbz(k),
     &                tbd(1,k), tbz(1,k), tbtt(1,1,k), ierr)
         if (ierr.eq.0) nfiles = nfiles + 1
         if (ierr.eq.2) return
 1000 continue
 
c     Error: No files read
 
      if (nfiles.eq.0 .and. icnt.gt.0) then
         write (luerr, '(a)') '? rdtab: No table files can be read'
         ierr = 1
      else
         ierr = 0
      end if

      return
      end


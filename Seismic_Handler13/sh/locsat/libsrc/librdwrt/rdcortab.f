
c NAME
c	rdcortab -- Read station correction tables in succession.

c FILE
c	rdcortab.f

c SYNOPSIS
c	Each station, correction-type, region file encountered will be
c	investigated for the existence of corresponding station corrections.

c DESCRIPTION
c	Subroutine.  Read station correction tables from files into memory.
c	The filenames to be read have the form froot/cortyp(m).STA.AREA, 
c	where froot is the root filename, cortyp(k) is the station
c	correction type identifier, STA is the station name, and AREA is
c	either the regional (*.reg) or local (*.local) station correction 
c	identifier.

c	Example: If cortyp(m) = 'TT' and 'AMP', then this routine reads 
c	files, 'TT.STA.AREA' and 'AMP.STA.AREA' from the froot file prefix
c	descriptor.

c	---- Indexing ----
c	k = 1, nwav;	l = 1, nsta;	m = 1, ntype;

c	---- On entry ----
c	froot:		Root-name of files
c	ntype:		Number of station correction types
c	nsta:		Number of stations
c	nwav:		Number of phases in list
c	staid(l):	Character identifier of l'th station
c	wavid(k):	List of all acceptible phases for arrival time and
c			slowness data
c	cortyp(m):	Character identifier of m'th correction type

c	---- On return ----
c	ierr:		Error flag;
c			  0: No error
c			  1: One or more files won't open
c			  2: One or more files have unexpected EOF
c			  3: File containing directory pointer for SSSC 
c			     is missing'

c	---- Subroutines called ----
c	Local
c		rdcortab1:	Read station correction files one at a time

c DIAGNOSTICS
c	Will specify if absolutely no input files exist.

c FILES
c

c NOTES
c

c SEE ALSO
c

c AUTHOR
c	Walter Nagy, October 1990.


      subroutine rdcortab (froot, cortyp, ntype, staid, wavid, nsta,
     &                     nwav, ierr)
 
      implicit none   ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsrdcortab/ sccsid
      character*80 sccsid
      data sccsid /'@(#)rdcortab.f	40.1	10/16/90'/
 
      include "rdcor.h"


c     On entry

      integer*4     nsta, ntype, nwav
      character*(*) cortyp(ntype), froot, staid(nsta), wavid(nwav)

c     On return

      integer*4 ierr

c     Internal variables

      integer*4 icnt, indx(2), ios, ista, itype, js, jt, jtype, kr
      integer*4 lnblnk, nfiles
      character corr_dir*30, ct*8, filnam*100


      if (ntype.gt.maxtyp) then
         print*, 
     &     'Number of station corr. types attempted > parameter, maxtyp'
         stop
      end if
      kr = lnblnk(froot)
      filnam = froot(1:kr)
      js     = kr + 1
      filnam(js:js+8) = '.corr_dir'

c     Open file pointer to station correction directory

      open (iunit, file = filnam, status = 'old', iostat = ios)
      if (ios.ne.0) then
         ierr = 3
         print*, 'File containing directory pointer for SSSC is missing'
         print*, 'File: ', filnam(1:js+8)
         return
      end if
    
c     Back-track to find current directory location

      do 1001 jt = kr, 1, -1
         if (froot(jt:jt).eq.'/') then
            js = jt + 1
            goto 1002
         end if
 1001 continue

c     Now read location of SSSC directory for these travel-time tables

 1002 read (iunit, '(a)') corr_dir
      filnam(js:) = '         '
      kr = lnblnk(corr_dir)
      filnam(js:) = corr_dir(1:kr)//'/'
      js = js + kr + 1

      close (iunit)

c     Initialize file counter

      nfiles = 0
 
c     Read station correction tables
 
      icnt = 0
      do 1010 itype = 1, ntype
	 icnt = icnt + 1
         kr = lnblnk(cortyp(itype))
         ct = cortyp(itype)
         if (ct(1:kr).eq.'TT') then
            jtype = 1
         else if (ct(1:kr).eq.'AZ') then
            jtype = 2
         else if (ct(1:kr).eq.'AMP') then
            jtype = 3
         else
            print*, 'Illegal station correction type, ', ct(1:kr), ' !'
            stop
         endif
         jt = js + 2*(kr-1+1) + 2
         filnam(js:) = ct(1:kr)//'/'//ct(1:kr)//'.'
         do 1000 ista = 1, nsta
            kr = lnblnk(staid(ista))

c	    Read regional station correction files here
            filnam(jt:) = staid(ista)(1:kr)//'.reg'
            call rdcortab1 (filnam, wavid, nwav, indx, jtype, ista, 1,
     &                      ierr)
            indexstacor(jtype,ista,1) = indx(1)
            if (ierr.eq.0) then
               nfiles = nfiles + 1
            else if (ierr.eq.1) then
               goto 1000 
            else
               return
            end if

c	    Read local station correction files here
            filnam(jt:) = staid(ista)(1:kr)//'.local'
            call rdcortab1 (filnam, wavid, nwav, indx, jtype, ista, 2,
     &                      ierr)
            indexstacor(jtype,ista,2) = indx(2)
            if (ierr.eq.0) nfiles = nfiles + 1
            if (ierr.eq.2) return
 1000    continue
 1010 continue
 
c     Error: Too many stations with correction for parameter setting

      if (indx(1).gt.maxcorrsta) then
         print*, 
     &     'Number of stations with corrections > parameter, maxcorrsta'
         stop
      end if

c     Error: No files read

      if (nfiles.eq.0 .and. icnt.gt.0) then
         write (0, '(2a)') ' rdcortab: No station correction files',
     &                         ' can be read'
         ierr = 1
      else
         ierr = 0
      end if

      return
      end


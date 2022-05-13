c file lnblnk.f
c      ========
c
c version 1, 17-Nov-98
c
c missing function lnblnk
c K. Stammler, 17-Nov-98



c -----------------------------------------------------------------------------



      integer function lnblnk( lin )

c     returns length of string not counting trailing blanks

c     parameters of routine
      character*(*) lin      ! input string

c     local variables
      integer       i        ! counter

c     functions
      integer       len

c     executable code

      i = len( lin )
1234  continue
         if  (lin(i:i) .ne. ' ')  goto 1235
         i = i - 1
         if  (i .eq. 0)  goto 1235
         goto 1234
1235  continue

      lnblnk = i
      return

      end

c -----------------------------------------------------------------------------

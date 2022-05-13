
c NAME
c	drotg -- Construct a Givens plane rotation.

c FILE
c	drotg.f

c SYNOPSIS
c	LINPACK routine applies a Givens plane rotation.

c DESCRIPTION
c	Subroutine.  Construct a Givens plane rotation, a scalar, da and
c	db, at a time.  Called in SVD routine, dsvdc, prior to application
c	of normal plane rotation (subr. drot).

c DIAGNOSTICS
c

c NOTES
c

c SEE ALSO
c	LINPACK documentation by John Dongarra.

c AUTHOR
c	John Dongarra, March 1978.


      subroutine drotg (da, db, c, s)
 
      implicit none    ! K.S. 1-Dec-97, changed 'undefined' to 'none'

      common /sccsdrotg/ sccsid
      character*80 sccsid
      data sccsid /'@(#)drotg.f	44.1	9/20/91'/


c     ---- On entry and return ----

      real*8 c, da, db, s

c     ---- Internal variables ----

      real*8 r, roe, scale, z
 

      roe = db
      if ( dabs(da) .gt. dabs(db) ) roe = da
      scale = dabs(da) + dabs(db)
      if (scale .ne. 0.0d0) goto 1000
      c = 1.0d0
      s = 0.0d0
      r = 0.0d0
      goto 1010
 1000 r = scale*dsqrt((da/scale)**2 + (db/scale)**2)
      r = dsign(1.0d0, roe)*r
      c = da/r
      s = db/r
 1010 z = 1.0d0
      if (dabs(da) .gt. dabs(db)) z = s
      if (dabs(db) .ge. dabs(da) .and. c .ne. 0.0d0) z = 1.0d0/c
      da = r
      db = z

      return
      end


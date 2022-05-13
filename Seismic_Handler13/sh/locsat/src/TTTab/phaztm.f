c
      subroutine phaztm (delt,deph,nphaz4, time,ierr)
c
c
c       Computes the travel time of any phase on
c         the phase list.
c
c           Input
c              delt : event to station distance (degrees)
c              deph : event depth (km)
c              nphaz : phase index of desired phase
c
c     1 PG        2 ScP       3 PcP  
c     4 pP        5 sP        6 PP
c     7 P         8 PPP       9 SP
c     10 SPP      11 SKPdf    12 SKPab
c     13 PKPdf    14 PKPab    15 PKPbc
c     16 PKPcd    17 PKKPdf   18 PKKPbc
c     19 PKKPab   20 PKPPKP   21 (none)
c     22 PcS      23 ScS      24 SKSac
c     25 SKSdf    26 S        27 pS 
c     28 sS       29 PS       30 PPS
c     31 SS       32 SSS      33 (none)
c     34 (none)   35 (none)   36 LR-Rayleigh
c     37 LQ-Love  38 Pn       39 Pg
c     40 Sn       41 Lg       42 Rg
c
c The last seven phases are computed with a separate
c  velocity stucture (in file velocity.dat)
c  that may be changed by the user. This is done in subroutine
c  rgsrtm.f.
c
c
c           Output
c
c              time : travel time of phase in seconds
c                     if error return is other than zero,
c                     travel time is set to 0.0
c
c              ierr   - i*4 - error return
c                                0 - normal return
c                                1 - event to station distance is
c                                       less than one degree
c                                2 - input phase index greater than 29
c                                       is not that of rayleigh or love
c                                3 - requested phase is not
c                                       present at this distance
c                                       and depth
c                                7 - the input phase index of 13 for pk
c                                       was changed to 7 for phase p
c                                       because the distance is less
c                                       than 110 degrees
c                               13 - the input phase index of 7 for p
c                                       was changed to 13 for phase pkp
c                                       because the distance is greater
c                                       than 110 degrees
c                           .gt.13 - input phase index is greater
c                                       than ierr, the number of
c                                       phases on phase list
c
c
c**********************************************************************
c
      include '../../include/numcon.h'
      include '../../include/phtbls.h'
      include '../../include/iaapaq.h'
c      integer*2 nphaz

	common /sccsphaztm/ sccsid
	character*80 sccsid
	data sccsid /'@(#)phaztm.f	31.1	8/26/88'/

      nphaz = nphaz4
      ierr = 0
      time = 0.0
c If phase index is greater than the total number of
c  phases computable here, return.
c
      if (nphaz.gt.32) then
        ierr = nphaz
        return
c
c If the phase is P or PKP, use the most precise travel time.
c
      else if (nphaz.eq.7 .or. nphaz.eq.13) then
        idph = nphaz
        idck = idph
        call trvtim (delt,deph,time,idph,vtdhy)
        vel = vtdhy
c
c At distances greater than the shadow zone, and calls to
c   PKP at distances less than the shadow zone.
c
         if (idph.ne.idck) ierr =idph
         if (idph.eq.0) ierr=3
         return
c
c Other phases except Rayleigh, Love, and regional.
c
      else 
        call trvltm (delt,deph,nphaz,time)
        if (time.le.0.0) ierr = 3
        return
      endif
c
      end

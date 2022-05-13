c
c
c     NAME
c         tttab - main program for computing travel-times
c                 and amplitudes for use in location programs.
c
c     FILE    main.f
c
c     SYNOPSIS
c         Program computes travel-times and amplitudes, then
c         outputs them to tables for use in programs like ipcloc
c         and predtt.  
c
c     DESCRIPTION
c
c        Usage: 
c          tttab [-i] -w wavid -f ofile -v vfile
c                    [-d dmin dmax dinc] [-z z1 z2 ..] -a (y/n)
c
c          where =>
c                 -i : select interactive mode 
c                     (missing arguments solicited)
c                 -w : wave to model is "wavid"
c                 -f : specify output file name
c                 -v : specify vel. struct. file name
c                     (for regional and surface waves)
c                 -d : specify distance limits and increment
c                 -z : specify sequence of increasing depths'
c                 -a : amplitude computed? (y or n)
c
c        Subroutine called:
c            Seismological : phaztm - teleseismic phases
c                            rgsrtm - regional and surface waves
c                            ampcal - amplitudes

c            From libclicip: cliget
c                            clixtr
c                            ciptok
c                            cipnum
c                            range

c     DIAGNOSTICS
c        Program complains when data neeeded are missing or
c        incomplete.
c
c     FILES
c        none
c
c     NOTES
c
c Phases for which arrival times and amplitudes may be computed in
c  program tttab and subroutines --
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
c  velocity stucture. The models are composed of flat 
c  layers over a half space. The file containing this 
c  information is specified by user with the following
c  format (read by free-format reads):
c
c               phase id (see list above)
c               number of layers above half space
c               thickness and velocity of top layer
c               thickness and velocity of next layer
c               etc.
c
c Example:
c               Pn
c               3
c               16.0 6.25
c               10.00 7.367
c               22.0 8.1
c               0  8.30 (half-space)
c
c
c Note that no amplitudes are computed for regional phases
c  at this time.
c
c     SEE ALSO
c        none
c
c     AUTHOR
c        Main program written by Steve Bratt
c        All subroutines are modified from programs by J. Goncz.
c

      parameter (maxphz = 42, maxtbd = 181, maxtbz = 20, maxlayer = 100)

      character*1 toka,dfla
      character*6 wavid(maxphz),dum
      character*40 tokwav,tokd,token,dfld
      character*100 tokf,dflf,tokz,dflz,tokv,dflv
      character*200 cline,alist
      dimension tbd(maxtbd), tbz(maxtbz)
      dimension iptwav(maxphz)
      dimension thickness(maxlayer),velocity(maxlayer+1)

	common /sccsmain/ sccsid
	character*80 sccsid
	data sccsid /'@(#)main.f	31.1	8/26/88'/

c
c Phases available in current compilation of program --
c
      data wavid /'PG    ','ScP   ','PcP   ','pP    ','sP    ',
     &            'PP    ','P     ','PPP   ','SP    ','SPP   ',
     &            'SKPdf ','SKPab ','PKPdf ','PKPab ','PKPbc ',
     &            'PKPcd ','PKKPdf','PKKPbc','PKKPab','PKPPKP',
     &            '      ','PcS   ','ScS   ','SKSac ','SKSdf ',
     &            'S     ','pS    ','sS    ','PS    ','PPS   ',
     &            'SS    ','SSS   ','      ','      ','      ',
     &            'LR    ','LQ    ','Pn    ','Pg    ','Sn    ',
     &            'Lg    ','Rg    '/
c
c Indices of above phases --
c
       data iptwav/1, 2, 3, 4, 5,
     &             6, 7, 8, 9,10,
     &            11,12,13,14,15,
     &            16,17,18,19,20,
     &            21,22,23,24,25,
     &            26,27,28,29,30,
     &            31,32,33,34,35,
     &            36,37,38,39,40,
     &            41,42/
c
c Initialize.
c
      iregsur = 0
      ierrw = 1
      ierrd = 1
      ierrz = 1
      ierrf = 1
      ierra = 1
c
c Default arguments.
c
      dfla = 'n'
      dflf = 'tab.wave'
      dfld = '0 180 1'
      dflz = '0 5 15 30 40 50 75 100 150 200 300 400 500 600 800'
      dflv = 'velocity.dat'
c
c Get command line and extract inputs.
c
      call cliget (cline,ierr)
      if (ierr.gt.0) then
        write (luerr,'(a)') '? Command line too long.'
        stop
      end if
c
      call clixtr (cline,'-i',0, narg,alist,lenal,icl1,icl2)
      inter = 0
      if (narg.ge.0) then
        inter = 1
        cline(icl1:icl2) = ' '
      end if
c
      call clixtr (cline,'-w',1, narg,alist,lenal,icl1,icl2)
      tokwav = alist
      if (narg.ge.0) cline(icl1:icl2) = ' '
c
      call clixtr (cline,'-d',3, narg,alist,lenal,icl1,icl2)
      tokd = alist
      if (narg.ge.0) cline(icl1:icl2) = ' '
c
      call clixtr (cline,'-z',50, narg,alist,lenal,icl1,icl2)
      tokz = alist
      if (narg.ge.0) cline(icl1:icl2) = ' '
c
      call clixtr (cline,'-f',1, narg,alist,lenal,icl1,icl2)
      tokf = alist
      if (narg.ge.0) cline(icl1:icl2) = ' '
c
      call clixtr (cline,'-v',1, narg,alist,lenal,icl1,icl2)
      tokv = alist
      if (narg.ge.0) cline(icl1:icl2) = ' '
c
      call clixtr (cline,'-a',1, narg,alist,lenal,icl1,icl2)
      toka = alist
      if (narg.ge.0) cline(icl1:icl2) = ' '
c
      if (cline.ne.' ') then
        call range (cline,j,k)
        write (luerr,'(2a)')'? Command line inputs ignored: ',cline(j:k)
      end if
c
c Check for missing inputs, if non-interactive
c
      if (inter.eq.0) then
        if (tokwav.eq.' ') go to 900
        if (tokd.eq.' ') tokd = dfld
        if (tokz.eq.' ') tokz = dflz
        if (tokf.eq.' ') tokf = dflf
        if (toka.eq.' ') toka = dfla
        if (tokv.eq.' ') tokv = dflv
      end if
c
c Solicit missing inputs, if interactive
c
10    if (inter.gt.0) then
        if (tokwav.eq.' ') then
          print '(a)', 'Choose wave to be modeled:'
	  print '(10(1x,a6))', (wavid(i), i = 1,maxphz)
	  print '(a,$)', 'Which wave? '
	  read (5,'(a)') tokwav
        end if
c
	if (tokf.eq.' ') then
	  call solowa ('Output file name?',dflf,tokf,iqu)
	  if (iqu.eq.1) goto 1000
        endif
c
        if (tokd.eq.' ') then
          call solmwa ('Distance min max inc?',dfld, tokd,iqu)
          if (iqu.eq.1) go to 1000
        end if
c
        if (tokz.eq.' ') then
          call solmwa ('Depths?',dflz, tokz,iqu)
          if (iqu.eq.1) go to 1000
        end if
c
	if (toka.eq.' ') then
	  call solowa ('Output ampl., aux. info.?',dfla,toka,iqu)
	  if (iqu.eq.1) goto 1000
        endif
      end if
c
c Process inputs
c
      if (ierrw.ne.0) then
        iwav = 0
        do 20 i = 1,maxphz
          call cipscn (wavid(i),tokwav,99,igo)
          if (igo.gt.0) iwav = i
20      continue
        if (iwav.eq.0) then
          write (luerr,'(2a)') '? No valid waves: ',tokwav
          tokwav = ' '
        else
          ierrw = 0
c
c For regional phases and surface waves, read number of 
c  layers, layer thicknesses and  velocities from file tokv.  
c
          call range (wavid(iwav),k3,k4)
          if (wavid(iwav)(k3:k4).eq.'Pn' .or. 
     &        wavid(iwav)(k3:k4).eq.'Pg' .or.
     &        wavid(iwav)(k3:k4).eq.'Sn' .or.
     &        wavid(iwav)(k3:k4).eq.'Lg' .or.
     &        wavid(iwav)(k3:k4).eq.'Rg' .or.
     &        wavid(iwav)(k3:k4).eq.'LR' .or.
     &        wavid(iwav)(k3:k4).eq.'LQ') then
            iregsur = 1
c
c Solicit velocity structure file name if process in interactive
c
	    if (tokv.eq.' ' .and. inter.gt.0) then
	      call solowa 
     &        ('Velocity structure file name?',dflv,tokv,iqu)
	      if (iqu.eq.1) goto 1000
            endif
c
            open (12, file=tokv, status='old', iostat=ios)
	    if (ios.ne.0) then
	      call range (tokv,k1,k2)
	      write (luerr,'(3a)') '? File ',tokv(k1:k2),
     &                             ' will not open'
	      ierrw = 1
	      tokwav = ' '
            endif
	    iread = 0
22          read (12,'(a)',end=24) dum
            call range (dum,k1,k2)
            if (dum(k1:k2).eq.wavid(iwav)(k3:k4)) then
	      iread = 1
              read (12,*) nlayer
              do 23  n = 1,nlayer+1
                read (12,*) thickness(n),velocity(n)
23            continue
            else
              goto 22
            endif
24          continue
	    if (iread.eq.0) write (luerr,'(3a)')
     &      '? Velocity structure for phase ',wavid(iwav)(k3:k4),
     &      ' not found'
c
            close (12)
          end if
c
        end if
      end if
c
      if (ierrd.ne.0) then
        ibp = 0
        call ciptok (tokd,ibp, token)
        call cipnum (token, dmin,ierrd)
        if (ierrd.eq.0) then
          call ciptok (tokd,ibp, token)
          call cipnum (token, dmax,ierrd)
        end if
        if (ierrd.eq.0) then
          call ciptok (tokd,ibp, token)
          call cipnum (token, dinc,ierrd)
        end if
        if (ierrd.eq.0) then
          if (dmin.lt.0.0 .or. dmax.gt.180.0 .or. dinc.le.0.0) then
            ierrd = 1
          else
            ntbd = nint((dmax-dmin)/dinc) + 1
            if (ntbd.gt.maxtbd) then
              write (luerr,'(a,i3)')
     &          '? Too many distances, max is ',maxtbd
              ierrd = 1
            end if
          end if
        end if
        if (ierrd.ne.0) then
          write (luerr,'(2a)') '? Bad distance specification: ',tokd
          tokd = ' '
        else
          ierrd = 0
        end if
      end if
c
      if (ierrz.ne.0) then
        ibp = 0
        j = 0
        ierr = 0
30      call ciptok (tokz,ibp, token)
        if (token.eq.'eoi') go to 40
        if (j.eq.maxtbz) then
          write (luerr,'(a,i3)') '? Too many depths, max is ',maxtbz
          ierrz = 1
          go to 40
        end if
        call cipnum (token, z,ierrz)
        if (ierrz.ne.0) then
          write (luerr,'(2a)') '? Bad depth: ',token
        else if (z.lt.0.0) then
          write (luerr,'(2a)') '? Bad depth: ',token
          ierrz = 1
        else if (j.gt.0) then
          if (z.le.tbz(j)) then
            write (luerr,'(2a)') '? Depth out of sequence: ',token
            ierrz = 1
          end if
        end if
        if (ierrz.eq.0) then
          j = j + 1
          tbz(j) = z
          go to 30
        end if
40      ntbz = j
        if (ierrz.ne.0 .or. ntbz.eq.0) then
          write (luerr,'(2a)') '? Bad distance specification: ',tokz
          tokz = ' '
          ierrz = 1
        end if
      end if
c
      if (ierrf.ne.0) then
	open (11,file=tokf,status='unknown')
	ierrf = 0
      endif
c
      if (ierra.ne.0) then
	if (toka.ne.'y' .and. toka.ne.'Y' .and.
     &      toka.ne.'n' .and. toka.ne.'N') then
	  write (luerr,'(2a)') '? Bad response -- ',toka
	  write (luerr,'(a)')  '  Enter a "Y, N, y, or n"'  
	  toka = ' '
        else
	  ierra = 0
        endif
      endif
c
c Bad inputs? Go back if interactive; stop if not
c
      if (tokwav.eq.' ' .or. tokd.eq.' ' .or. tokz.eq.' ' .or.
     &    tokf.eq.' ' .or. toka.eq.' ') then
        if (inter.eq.0) go to 900
        go to 10
      end if
c
c Form distance array.
c
      do 60 i = 1,ntbd
60    tbd(i) = dmin + (i-1)*dinc
c
c Write wave type, depths and distances used to compute tables.
c
      write (11,'(4a)') toka,' # ',wavid(iwav),
     &                  ' travel-time (and amplitude) tables'
      write (11,'(i2,a)') ntbz,'    # number of depth samples'
      write (11,'(10f8.2)') (tbz(j),j=1,ntbz)
      write (11,'(i3,a)') ntbd,'    # number of distance samples'
      write (11,'(10f8.2)') (tbd(j),j=1,ntbd)
c
      do 110 j = 1,ntbz
        write (11,'(a,f8.2)') '# Travel-time/amplitude for z =',tbz(j)
        jz = nint(tbz(j))
c
c Compute travel time.
c
        do 100 i = 1,ntbd
	  if (iregsur.eq.0) then
            call phaztm (tbd(i),tbz(j),iptwav(iwav), 
     &                   tbtt,ierr1)
          else
	    call rgsrtm (tbd(i),tbz(j),thickness,velocity,nlayer,
     &                   tbtt,ierr2)
	  endif
          if (tbtt.le.0.0 .and. 
     &        (tbd(i).ne.0.0 .or. tbz(j).ne.0.0)) tbtt = -1.0
c
c Compute amplitude (no amplitude calculation for regional phases
c  at this point).
c
          if (toka.eq.'y' .or. toka.eq.'Y') then
	      qf = 0.0
	      if (tbtt.ge.0.0) call ampcal (tbd(i),iptwav(iwav),jz,qf)
              if (qf.eq.0.0) then
                tbam = -1.0
              else
                tbam = 10.0 ** (-qf)
                if (tbam.eq.0.0) tbam = -1.0
              end if
            write (11,'(f12.3,e12.3,2f8.1)')
     &      tbtt,tbam,tbd(i),tbz(j)
	  else
            write (11,'(f12.3)') tbtt
          endif
100     continue
110   continue
c
      close (11)
      print '(/2a)', ' Output file = ',tokf
c
c Done
c
1000  call exit (0)
c
c Straighten out the user
c
900   write (luerr,'(2a)')
     &  'Usage: tttab [-i] -w wavid -f ofile -v vfile',
     &  ' [-d dmin dmax dinc] [-z z1 z2 ..] -a (y/n)'
      write (luerr,'(2a)') '  -i : select interactive mode',
     &                     ' (missing arguments solicited)'
      write (luerr,'(a)') '  -w : wave to model is "wavid"'
      write (luerr,'(a)') '  -f : specify output file name'
      write (luerr,'(2a)')'  -v : specify vel. struct. file name',
     &                    ' (for regional and surface waves)'
      write (luerr,'(a)') '  -d : specify distance limits and increment'
      write (luerr,'(a)') '  -z : specify sequence of increasing depths'
      write (luerr,'(a)') '  -a : specify y/n for output of aux. info.'
      call exit (0)
      end


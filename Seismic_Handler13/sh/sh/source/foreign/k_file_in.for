
c      File K_FILE_IN.FOR
c           =============
c
c      version 4, 2-Jun-93

c      subroutine to read files in K-format
c      K. Stammler, 27-SEP-1989

c -----------------------------------------------------------------------------



       subroutine k_identify( start_time, directory, max_no,
     &    no_of_tracks, track, status )

c      Untersucht, wieviele und welche Stationen im y-File enthalten sind. Die
c      einzelnen Spuren koennen dann mit "y_read" unter Angabe der Nummer in
c      "seq_no" ausgelesen werden ( seq_no aus { 1, .. , no_of_tracks } )

c      Parameter der Routine
       character*(*) start_time      ! input; start time
       character*(*) directory       ! input; directory to look for k-files
       integer       max_no          ! input; length of output array
       integer       no_of_tracks    ! output; number of traces in k-file
       character*80  track(max_no)   ! output; station names
       integer       status          ! output; return status

       include 'if_y_file_in_errors.for'
       include 'if_y_file_in_const.for'

c      lokale Konstanten
       integer       max_stat_lth! Laenge des Datenfelds "stat"
       parameter( max_stat_lth = 20 )

c      lokale Variablen
       integer       y_unit      ! File-Unit
       integer       abs_start   ! start time
       integer       lookup_mode ! lookup mode of rdisc
       integer*2     blk(yc_block_lth)  ! Speicher fuer Datenblock
       integer       pts_read    ! number of points read
       integer       loc_stat    ! rdisc return status
       integer       i, j        ! Zaehler
       integer       i1, i2, i3, i4, i5  ! Dummy
       real          r1, r2  ! Dummy
       integer       stat(max_stat_lth)! Kennummern der Spuren
       real          gen(max_stat_lth) ! Generatorkonstanten (hier nicht gebr.)
       character     ch          ! Komponente
       integer       date(2)     ! date of event (1=year, 2=julian day)
       integer       ii(20)      ! dummy

c      Programmtext

       status = yre_no_error

! get unit number -> common block       call lib$get_lun( y_unit )

       call tim_asc_to_abs( start_time, abs_start, i )
       lookup_mode = 1

       i1 = 3
       do  i = 1,i1
          ii(i) = i
       enddo
       call rdisc( y_unit, blk, abs_start, directory//' ', lookup_mode, 1,
     &    i1, ii, 1, pts_read, i3, loc_stat )
c      dummy call to reset file
       call rdisc( y_unit, blk, abs_start, directory//' ', lookup_mode, 1,
     &    i1, ii, 1, pts_read, i3, loc_stat )

       if  (loc_stat .ne. 0)  then
          status = yre_read_problems
          if  (status .gt. yre_severe)  return
       endif

       call y_rd_header( blk, i1, i2, i3, i4, i5, r1, r2,
     &    max_stat_lth, no_of_tracks, stat, gen, status )
       date(1) = blk(yc_idx_year)
       date(2) = blk(yc_idx_day)
       if  (date(1) .lt. 1900)  date(1) = date(1) + 1900

       if  (no_of_tracks .gt. max_no)  then
          status = yre_more_tracks
          if  (status .gt. yre_severe)  return
       endif

       do  i = 1,no_of_tracks
          call y_get_stat_string( date, stat(i), track(i), ch, status )
          if  (status .gt. yre_severe)  return
          j = 1
          dowhile  (track(i)(j:j) .gt. ' ')
             j = j + 1
          enddo
          track(i)(j:j) = '-'
          track(i)(j+1:j+1) = ch
       enddo

       return
       end ! of subroutine k_identify



c -----------------------------------------------------------------------------



       subroutine k_multi_read_v2( start_time, directory, max_lth, no_of_trc,
     &    seq_no, no_of_blocks, counts, delta_t, data, data_cnt, station, comp,
     &    status )

c      reads k-files.

c      Parameter der Routine
       character*(*) start_time          ! modify; start time
       character*(*) directory           ! input; directory string
       integer       max_lth             ! input; lengths of data arrays
       integer       no_of_trc           ! input; number of traces to be read
       integer       seq_no(no_of_trc)   ! input; position numbers of traces
       integer       no_of_blocks        ! input; number of 10sec-blocks to read
       integer       counts              ! input; return counts if counts .ne. 0
       real          delta_t             ! output; Sampling
       integer       data(no_of_trc)     ! input; pointers to data arrays
       integer       data_cnt(no_of_trc) ! output; number of data points read
       character*(*) station(no_of_trc)  ! output; station names
       character     comp(no_of_trc)     ! output; components
       integer       status              ! output; return status

       include 'if_y_file_in_errors.for' ! Fehlernummern "yre_..."
       include 'if_y_file_in_const.for'  ! Indexnummern "yc_..."

c      local constants
       logical       forever      ! infinite loop
       integer       max_seq_lth  ! max. number of traces
       integer       y_out_max_lth! length of decoded data
       real          cnv_fac      ! Umrechnungsfaktor von counts nach micron/s
       real          std_gen      ! Standard-Generatorkonstante
       parameter( forever = .true., std_gen = 4000.,
     &    max_seq_lth = 20, y_out_max_lth = 200, cnv_fac = 4.7729 )

c      lokale Variablen
       integer       abs_start       ! absolute starttime
       integer       lookup_mode     ! lookup mode of rdisc
       integer       pts_read        ! number of read data
       integer*2     blk(yc_block_lth)  ! Speicher fuer Datenblock
       integer       currtim         ! current time read
       integer       stat            ! rdisc return status
       integer       curr_block      ! current block number
       integer       start_rec       ! Start-Record
       integer       seq(max_seq_lth)! Reihenfolge der Seismometerspuren
       integer       seq_lth         ! gueltige Eintraege in obigem Feld
       real          gen(max_seq_lth)! relative Generatorkonstanten
       real          y_out(y_out_max_lth)! Ausgabedaten von "y_dekod"
       integer       y_out_len       ! Eintraege in "y_out"
       integer       i               ! Zaehler
       real          factor          ! Umrechnungsfaktor-Zwischenspeicher
       integer       d_y, d_m, d_d   ! Datum: Jahr, Monat, Tag
       integer       t_h, t_m        ! Uhrzeit: Stunde, Minute
       real          t_s             ! Uhrzeit: Sekunde
       integer       trc             ! trace counter
       integer       date(2)         ! date for y_get_stat_string
       integer*2     currsec10       ! current 10sec counter
       integer*2     prevsec10       ! previous 10sec counter
       integer       cnt0            ! zero block counter
       integer       errorcnt        ! error counter
       integer       dunit           ! dump unit
       integer       jj              ! counter

       integer*2     blk_sampl       ! Samples per Block (200)
       integer*2     blk_hdr_lth     ! Laenge des Blockheaders (40)

c      global variables
       integer       k_unit          ! File-Unit des y-Files
       common /k_input_unit/ k_unit

c      Programmtext

c      integer*2 - "Konstanten"
       blk_sampl = 200
       blk_hdr_lth = 40

       if  (k_unit .eq. 0)  k_unit = 68  ! call lib$get_lun( k_unit )

       do  trc = 1,no_of_trc
          data_cnt(trc) = 0
       enddo
       call tim_asc_to_abs( start_time, abs_start, i )
       lookup_mode = 1
       curr_block = 1
       errorcnt = 0

       if  (iand(counts,2) .ne. 0)  then
          write (*,*) '   --> dump blocks to file kdump.dat'
          dunit = 69 ! call lib$get_lun( dunit )
          open( dunit, file='kdump.dat', status='new' )
       endif

       dowhile  (forever)  ! Ende mit "return"

c         call routine of "lib_od" library
          call rdisc( k_unit, blk, abs_start, directory//' ', lookup_mode,
     &       1, no_of_trc, seq_no, 1, pts_read, currtim, stat )
          if  (iand(counts,2) .ne. 0)  then
             write (dunit,*)  'block ', abs_start
             if  (iand(counts,4) .ne. 0)  then
                write (dunit,'(1x,10z5)')  (blk(jj),jj=1,yc_block_lth)
             else
                write (dunit,'(1x,10i6)')  (blk(jj),jj=1,yc_block_lth)
             endif
          endif
          if  (stat .eq. 3)  then
             lookup_mode = 1
             i = currtim
             stat = 0
             call rdisc( k_unit, blk, i, directory//' ', lookup_mode,
     &          1, no_of_trc, seq_no, 1, pts_read, currtim, stat )
             if  (iand(counts,2) .ne. 0)  then
		write (*,*) '  dump to file'
                write (dunit,*)  'block ', i
                write (dunit,'(1x,10i6)')  (blk(jj),jj=1,yc_block_lth)
             endif
             write (*,*)  ! --> test output <--
     &          '*** SH-W  K-file time not found.  Next possible time read ***'
          else if  (stat .gt. 0)  then
             write (*,*)  '*** rdisc error ', stat   ! --> test output <--
             errorcnt = errorcnt + 1
             if  (errorcnt .ge. 10)  then
                status = yre_k_rdisc
                if  (iand(counts,2) .ne. 0)  then
                   close( dunit )
                   dunit = 0 ! call lib$free_lun( dunit )
                endif
                return
             endif
             do  i = blk_hdr_lth+1,yc_block_lth
                blk(i) = 0
             enddo
             blk(yc_idx_nseis) = no_of_trc
          else
             errorcnt = 0
          endif
          currsec10 = blk(yc_idx_sec)

c         if first call then set some output variables
          if  (curr_block .eq. 1)  then  ! first call
             if  (currtim .ne. abs_start)  then
                call tim_abs_to_asc( currtim, 0, start_time, i )
             endif
             call y_rd_header( blk, d_d, d_m, d_y, t_h, t_m, t_s, delta_t,
     &                         max_seq_lth, seq_lth, seq, gen, status )
             date(1) = blk(yc_idx_year)
             date(2) = blk(yc_idx_day)
             prevsec10 = currsec10 - 1
             if  (date(1) .lt. 1900)  date(1) = date(1) + 1900
             if  (status .gt. yre_severe)  then
                if  (iand(counts,2) .ne. 0)  then
                   close( dunit )
                   dunit = 0 ! call lib$free_lun( dunit )
                endif
                return
             endif
             do  trc = 1,no_of_trc
                call y_get_stat_string( date, seq(trc), station(trc),
     &             comp(trc), status )
                if  (status .gt. yre_severe)  then
                   if  (iand(counts,2) .ne. 0)  then
                      close( dunit )
                      dunit = 0 ! call lib$free_lun( dunit )
                   endif
                   return
                endif
             enddo
          endif
          lookup_mode = 2

          if  (((prevsec10+1) .ne. currsec10) .and. (currsec10 .ne. 0))  then
             do  cnt0 = 1,currsec10-prevsec10-1
                do  trc = 1,no_of_trc
                   do  i = 1,200
                      data_cnt(trc) = data_cnt(trc) + 1
                      call y_set_value( %val(data(trc)), data_cnt(trc), 0.0 )
                   enddo
                enddo
                curr_block = curr_block + 1
                if  (curr_block .gt. no_of_blocks)  then
                   if  (iand(counts,2) .ne. 0)  then
                      close( dunit )
                      dunit = 0 ! call lib$free_lun( dunit )
                   endif
                   return
                endif
             enddo
          endif
          prevsec10 = currsec10

          do  trc = 1,no_of_trc

             call y_dekod( blk(yc_idx_nseis), blk_sampl, blk_hdr_lth,
     &          trc, blk, y_out_max_lth, y_out, y_out_len )

             if  (y_out_len .eq. 0)  then  ! "y_out"-Ueberlauf
                status = yre_dekod_err
                if  (status .gt. yre_severe)  then
                   if  (iand(counts,2) .ne. 0)  then
                      close( dunit )
                      dunit = 0 ! call lib$free_lun( dunit )
                   endif
                   return
                endif
             endif

             factor = 1.0
             if  (counts .eq. 0)  factor = (cnv_fac / std_gen) * gen(trc)
             do  i = 1,y_out_len
                data_cnt(trc) = data_cnt(trc) + 1
                if  (data_cnt(trc) .gt. max_lth)  then
                   status = yre_data_too_long
                   if  (status .gt. yre_severe)  then
                      if  (iand(counts,2) .ne. 0)  then
                         close( dunit )
                         dunit = 0 ! call lib$free_lun( dunit )
                      endif
                      return
                   endif
                endif
                call y_set_value( %val(data(trc)), data_cnt(trc),
     &             y_out(i)*factor )
             enddo

          enddo

          curr_block = curr_block + 1
          if  (curr_block .gt. no_of_blocks)  then
             if  (iand(counts,2) .ne. 0)  then
                close( dunit )
                dunit = 0 ! call lib$free_lun( dunit )
             endif
             return
          endif

       enddo

       end ! of subroutine k_multi_read_v2



c -----------------------------------------------------------------------------



       subroutine k_multi_read_v3( start_time, directory, max_lth, no_of_trc,
     &    seq_no, no_of_blocks, counts, delta_t, data, data_cnt, calib,
     &    station, comp, status )

c      reads k-files.

c      Parameter der Routine
       character*(80) start_time          ! modify; start time
       character*(80) directory           ! input; directory string
       integer       max_lth             ! input; lengths of data arrays
       integer       no_of_trc           ! input; number of traces to be read
       integer       seq_no(no_of_trc)   ! input; position numbers of traces
       integer       no_of_blocks        ! input; number of 10sec-blocks to read
       integer       counts              ! input; return counts if counts .ne. 0
       real          delta_t             ! output; Sampling
       integer       data(no_of_trc)     ! input; pointers to data arrays
       integer       data_cnt(no_of_trc) ! output; number of data points read
       real          calib(no_of_trc)    ! output; calibration constants
       character*(80) station(no_of_trc)  ! output; station names
       character*80     comp(no_of_trc)     ! output; components
       integer       status              ! output; return status

       include 'if_y_file_in_errors.for' ! Fehlernummern "yre_..."
       include 'if_y_file_in_const.for'  ! Indexnummern "yc_..."

c      local constants
       logical       forever      ! infinite loop
       integer       max_seq_lth  ! max. number of traces
       integer       y_out_max_lth! length of decoded data
       real          cnv_fac      ! Umrechnungsfaktor von counts nach micron/s
       real          std_gen      ! Standard-Generatorkonstante
       parameter( forever = .true., std_gen = 4000.,
     &    max_seq_lth = 20, y_out_max_lth = 200, cnv_fac = 4.7729 )

c      lokale Variablen
       integer       abs_start       ! absolute starttime
       integer       lookup_mode     ! lookup mode of rdisc
       integer       pts_read        ! number of read data
       integer*2     blk(yc_block_lth)  ! Speicher fuer Datenblock
       integer       currtim         ! current time read
       integer       stat            ! rdisc return status
       integer       curr_block      ! current block number
       integer       start_rec       ! Start-Record
       integer       seq(max_seq_lth)! Reihenfolge der Seismometerspuren
       integer       seq_lth         ! gueltige Eintraege in obigem Feld
       real          gen(max_seq_lth)! relative Generatorkonstanten
       real          y_out(y_out_max_lth)! Ausgabedaten von "y_dekod"
       integer       y_out_len       ! Eintraege in "y_out"
       integer       i               ! Zaehler
       real          factor          ! Umrechnungsfaktor-Zwischenspeicher
       integer       d_y, d_m, d_d   ! Datum: Jahr, Monat, Tag
       integer       t_h, t_m        ! Uhrzeit: Stunde, Minute
       real          t_s             ! Uhrzeit: Sekunde
       integer       trc             ! trace counter
       integer       date(2)         ! date for y_get_stat_string
       integer*2     currsec10       ! current 10sec counter
       integer*2     prevsec10       ! previous 10sec counter
       integer       cnt0            ! zero block counter
       integer       errorcnt        ! error counter
       integer       dunit           ! dump unit
       integer       jj              ! counter

       integer*2     blk_sampl       ! Samples per Block (200)
       integer*2     blk_hdr_lth     ! Laenge des Blockheaders (40)

c      global variables
       integer       k_unit          ! File-Unit des y-Files
       common /k_input_unit/ k_unit

c      Programmtext

c      integer*2 - "Konstanten"
       blk_sampl = 200
       blk_hdr_lth = 40

       if  (k_unit .eq. 0)  k_unit = 68 ! call lib$get_lun( k_unit )

       do  trc = 1,no_of_trc
          data_cnt(trc) = 0
       enddo
       call tim_asc_to_abs( start_time, abs_start, i )
       lookup_mode = 1
       curr_block = 1
       errorcnt = 0

       if  (iand(counts,2) .ne. 0)  then
          write (*,*) '   --> dump blocks to file kdump.dat'
          dunit = 69 ! call lib$get_lun( dunit )
          open( dunit, file='kdump.dat', status='new' )
       endif

       dowhile  (forever)  ! Ende mit "return"

c         call routine of "lib_od" library
          call rdisc( k_unit, blk, abs_start, directory//' ', lookup_mode,
     &       1, no_of_trc, seq_no, 1, pts_read, currtim, stat )
          if  (iand(counts,2) .ne. 0)  then
             write (dunit,*)  'block ', abs_start
             if  (iand(counts,4) .ne. 0)  then
                write (dunit,'(1x,10z5)')  (blk(jj),jj=1,yc_block_lth)
             else
                write (dunit,'(1x,10i6)')  (blk(jj),jj=1,yc_block_lth)
             endif
          endif
          if  (stat .eq. 3)  then
             lookup_mode = 1
             i = currtim
             stat = 0
             call rdisc( k_unit, blk, i, directory//' ', lookup_mode,
     &          1, no_of_trc, seq_no, 1, pts_read, currtim, stat )
             if  (iand(counts,2) .ne. 0)  then
		write (*,*) '  dump to file'
                write (dunit,*)  'block ', i
                write (dunit,'(1x,10i6)')  (blk(jj),jj=1,yc_block_lth)
             endif
             write (*,*)  ! --> test output <--
     &          '*** SH-W  K-file time not found.  Next possible time read ***'
          else if  (stat .gt. 0)  then
             write (*,*)  '*** rdisc error ', stat   ! --> test output <--
             errorcnt = errorcnt + 1
             if  (errorcnt .ge. 10)  then
                status = yre_k_rdisc
                if  (iand(counts,2) .ne. 0)  then
                   close( dunit )
                   dunit = 0 ! call lib$free_lun( dunit )
                endif
                return
             endif
             do  i = blk_hdr_lth+1,yc_block_lth
                blk(i) = 0
             enddo
             blk(yc_idx_nseis) = no_of_trc
          else
             errorcnt = 0
          endif
          currsec10 = blk(yc_idx_sec)

c         if first call then set some output variables
          if  (curr_block .eq. 1)  then  ! first call
             if  (currtim .ne. abs_start)  then
                call tim_abs_to_asc( currtim, 0, start_time, i )
             endif
             call y_rd_header( blk, d_d, d_m, d_y, t_h, t_m, t_s, delta_t,
     &                         max_seq_lth, seq_lth, seq, gen, status )
             date(1) = blk(yc_idx_year)
             date(2) = blk(yc_idx_day)
             prevsec10 = currsec10 - 1
             if  (date(1) .lt. 1900)  date(1) = date(1) + 1900
             if  (status .gt. yre_severe)  then
                if  (iand(counts,2) .ne. 0)  then
                   close( dunit )
                   dunit = 0 ! call lib$free_lun( dunit )
                endif
                return
             endif
             do  trc = 1,no_of_trc
                call y_get_stat_string( date, seq(trc), station(trc),
     &             comp(trc), status )
                if  (status .gt. yre_severe)  then
                   if  (iand(counts,2) .ne. 0)  then
                      close( dunit )
                      dunit = 0 ! call lib$free_lun( dunit )
                   endif
                   return
                endif
             enddo
          endif
          lookup_mode = 2

          if  (((prevsec10+1) .ne. currsec10) .and. (currsec10 .ne. 0))  then
             do  cnt0 = 1,currsec10-prevsec10-1
                do  trc = 1,no_of_trc
                   do  i = 1,200
                      data_cnt(trc) = data_cnt(trc) + 1
                      call y_set_value( %val(data(trc)), data_cnt(trc), 0.0 )
                   enddo
                enddo
                curr_block = curr_block + 1
                if  (curr_block .gt. no_of_blocks)  then
                   if  (iand(counts,2) .ne. 0)  then
                      close( dunit )
                      dunit = 0 ! call lib$free_lun( dunit )
                   endif
                   return
                endif
             enddo
          endif
          prevsec10 = currsec10

          do  trc = 1,no_of_trc

             call y_dekod( blk(yc_idx_nseis), blk_sampl, blk_hdr_lth,
     &          trc, blk, y_out_max_lth, y_out, y_out_len )

             if  (y_out_len .eq. 0)  then  ! "y_out"-Ueberlauf
                status = yre_dekod_err
                if  (status .gt. yre_severe)  then
                   if  (iand(counts,2) .ne. 0)  then
                      close( dunit )
                      dunit = 0 ! call lib$free_lun( dunit )
                   endif
                   return
                endif
             endif

             factor = 1.0
             calib(trc) = (cnv_fac / std_gen) * gen(trc)
             if  (counts .eq. 0)  factor = calib(trc)
             do  i = 1,y_out_len
                data_cnt(trc) = data_cnt(trc) + 1
                if  (data_cnt(trc) .gt. max_lth)  then
                   status = yre_data_too_long
                   if  (status .gt. yre_severe)  then
                      if  (iand(counts,2) .ne. 0)  then
                         close( dunit )
                         dunit = 0 ! call lib$free_lun( dunit )
                      endif
                      return
                   endif
                endif
                call y_set_value( %val(data(trc)), data_cnt(trc),
     &             y_out(i)*factor )
             enddo

          enddo

          curr_block = curr_block + 1
          if  (curr_block .gt. no_of_blocks)  then
             if  (iand(counts,2) .ne. 0)  then
                close( dunit )
                dunit = 0 ! call lib$free_lun( dunit )
             endif
             return
          endif

       enddo

       end ! of subroutine k_multi_read_v3



c -----------------------------------------------------------------------------



       subroutine k_read_end( directory )

c      end of read on optical disc

c      parameters of routine
       character*(*) directory       ! input; directory string

c      global variables
       integer       k_unit          ! File-Unit des y-Files
       common /k_input_unit/ k_unit

c      local variables
       integer       seq_no(20), pts_read, currtim, stat, lookup_mode
       integer       abs_start, no_of_trc
       integer*2     blk(1000)
       logical       file_opened

c      executable code

       lookup_mode = 1
       no_of_trc = 1
       seq_no(1) = 1
       abs_start = 99999999

       ! call lib$set_logical( 'SYS$OUTPUT', 'SYS$LOGIN:SH$KKKK' )
       call rdisc( k_unit, blk, abs_start, directory//' ', lookup_mode,
     &    1, no_of_trc, seq_no, 1, pts_read, currtim, stat )
       ! call lib$delete_logical( 'SYS$OUTPUT' )

       inquire( k_unit, opened=file_opened )
       if  (file_opened)  then
          write (*,*) '-->  k-file was not closed'
          close( k_unit )
       endif

       return
       end ! of subroutine k_read_end



c -----------------------------------------------------------------------------

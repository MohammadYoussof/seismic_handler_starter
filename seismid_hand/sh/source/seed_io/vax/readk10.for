
c file readk10.for
c      ===========
c
c version 1, 23-Nov-94
c
c reads 10 second block from disk
c K. Stammler, 23-Nov-94



	subroutine k_read10sec( reqtime, directory, outtime, smp )

c	reads 10 sec block at given time or, if time is empty,
c	reads next block
c
c	constants
	integer  trcnum                  ! number of traces
	parameter( trcnum = 19 )
	integer  smpnum                  ! number of samples per 10 sec
	parameter( smpnum = 200 )
	integer  k_unit                  ! unit number for k-file
	parameter( k_unit = 83 )
c
c	parameters of routine
	character*(*) reqtime            ! input; requested time
	character*(*) directory          ! input; name of input directory
	character*(*) outtime            ! output; actual time
	integer       smp(smpnum,trcnum) ! output; data read

	include 'shd_lib:if_y_file_in_const'

c	local variables
	integer       lookup_mode        ! lookup mode
	integer*2     blk(yc_block_lth)  ! encoded data block
	integer       abs_start          ! absolute start time
	integer       i, j               ! counters
	integer       no_of_trc          ! number of traces
	integer       seqno(trcnum+10)   ! trace list
	integer       pts_read           ! number of points read
	integer       currtim            ! current time
	integer       stat               ! return status
	integer*2     blk_sampl          ! number of samples in block
	integer*2     blk_hdr_lth        ! length of block header
	integer       outlen             ! # of decoded samples
	real          fsmp(smpnum)       ! real samples

	data no_of_trc /19/
	data seqno /1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,10*0/

c	executable code

c	write (*,*) reqtime, directory

	if  (reqtime .ne. ' ')  then
		lookup_mode = 1
		call tim_asc_to_abs( reqtime, abs_start, i )
	endif

	call rdisc( k_unit, blk, abs_start, directory//' ', lookup_mode,
     &		1, no_of_trc, seqno, 1, pts_read, currtim, stat )
	if  (stat .ne. 0)  then
		lookup_mode = 1
		abs_start = currtim
		call rdisc( k_unit, blk, abs_start, directory//' ', lookup_mode,
     &			1, no_of_trc, seqno, 1, pts_read, currtim, stat )
	endif

c	prepare time string for C
	call tim_abs_to_asc( currtim, 0, outtime, i )
	i = 8
	dowhile  (outtime(i:i) .ne. ' ')
		i = i + 1
	enddo
	outtime(i:i) = '_'
	i = i + 1
	if  (outtime(i:i) .eq. ' ')  outtime(i:i) = '0'
	i = 15
	dowhile  (outtime(i:i) .ne. ' ')
		i = i+1
	enddo
	outtime(i:i+1) = '0'//char(0)

c	decode data block
	blk_sampl = smpnum
	blk_hdr_lth = 40
	do  i=1,trcnum
		call y_dekod( blk(yc_idx_nseis), blk_sampl, blk_hdr_lth,
     &			i, blk, smpnum, fsmp, outlen )
		if  (outlen .ne. smpnum)  then
			write (*,*) 'k_read10sec: error in y_dekod', i, outlen,
     &                   blk(yc_idx_nseis), blk_sampl, blk_hdr_lth
			do  j=1,smpnum
				smp(j,i) = 0
			enddo
		else
			do  j=1,smpnum
				smp(j,i) = int( fsmp(j) )
			enddo
		endif
	enddo

c	write (*,*) pts_read, stat

	end ! of subroutine k_read10sec

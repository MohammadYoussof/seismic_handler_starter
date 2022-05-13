
	program fk

	implicit none

c	version 3, 9-Sep-94
c	
c	produces fk matrix
c	K. Stammler, 22-Jun-94

c	constants
	integer           max_trace   ! maximum number of traces
	parameter( max_trace = 45 )
	integer           max_arrlth  ! maximum length of all traces together
	parameter( max_arrlth = max_trace*1000 )
	integer           max_wrkspc  ! maximum length of work space
	parameter( max_wrkspc = 51000 )
	integer           max_clxdim  ! maximum work space of complex numbers
	parameter( max_clxdim = 51000 )

c	variables
	character*80      data_file   ! name of data file
	character*80      hdr_file    ! name of header file
	character*80      out_file    ! name of output file
	integer           ldat        ! logical unit of data file
	integer           lhdr        ! logical unit of header file
	integer           lout        ! logical unit of output file
	character*80      line        ! current line of header file
	integer           chan_num    ! number of channels
	integer           i, j        ! counters
	integer           index       ! index of data array
	real              frq_lo      ! lower frequency limit
	real              frq_hi      ! upper frequency limit
	real              max_slowness! maximum slowness in search
	real              max_slowness_km! maximum slowness in search in sec/km
	logical           unit_in_deg ! unit is in degrees
	integer           grid_pts    ! points in one slowness search row
	integer           verbosity   ! verbosity level
	real              app_veloc   ! apparent velocity
	real              res_slowness! resulting slowness
	real              azimuth     ! azimuth of power maximum
	real              sx_max      ! maximum of Sx
	real              sy_max      ! maximum of Sy
	real              frq_lo_out  ! corrected lower frequency
	real              frq_hi_out  ! corrected upper frequency
	real              power       ! normalized power maximum
	real              abs_power   ! power in dB
	integer           quality     ! quality measure
	character*80      message     ! error message
	character*80      instr       ! scratch
	integer           clxdim      ! scratch
	integer           wrkdim      ! scratch
	real              alpha, beta, theta ! dummy
	real              fdat(max_arrlth)       ! data array
	character*80      station(max_trace)     ! station name
	real              stat_x(max_trace)      ! x position of station
	real              stat_y(max_trace)      ! y position of station
	real              elevation(max_trace)   ! elevation of stations in m
	integer           chan_ptr(max_trace)    ! pointers to start of traces
	integer           chan_lth(max_trace)    ! lengths of traces
	real              chan_dt(max_trace)     ! sample distance of traces
	real              wdw_start(max_trace)   ! window start
	real              wdw_end(max_trace)     ! window end
	real              work_spc(max_wrkspc)   ! work space
	complex           clxbuf(max_clxdim)     ! work space

c	executable code

	ldat = 31
	lhdr = 32
	lout = 33

	data_file = 'fk_signal.dat'
	hdr_file = 'fk_signal.hdr'
	out_file = 'fk_signal.out'

c	open output file
	open( lout, file=out_file(1:20), status='unknown' )

c	read header file
	open( lhdr, status='old', file=hdr_file )
	line(1:1) = '!'
	dowhile (line(1:1) .eq. '!')
		read( lhdr, '(a)' ) line
		if  (line(1:2) .eq. '!*')  write (lout,'(a)') line
	enddo
	read( line, '(i3)' ) chan_num
	if  (chan_num .gt. max_trace)  stop 'too many traces'
	do  i=1,chan_num
           read( lhdr, '(f10,f10,f10,a)' ) stat_x(i), stat_y(i),
     &        elevation(i), station(i)
	enddo
	read( lhdr, '(f8,f8)' ) frq_lo, frq_hi
c	write (*,*) frq_lo, frq_hi
	read( lhdr, '(f9)' ) max_slowness
c	write (*,*) max_slowness
	read( lhdr, '(i4)' ) grid_pts
c	write (*,*) grid_pts
	read( lhdr, '(i4)' ) verbosity
c	write (*,*) verbosity
	read( lhdr, '(i4)' ) unit_in_deg
c	write (*,*) unit_in_deg
	close( lhdr )

	max_slowness_km = max_slowness
	if  (unit_in_deg)  max_slowness_km = max_slowness_km / 111.19

	if  ((grid_pts*grid_pts+grid_pts/2) .gt. max_wrkspc)  stop 'work space small'

c	read data file
	index = 1
	open( ldat, status='old', file=data_file )

	do  i=1,chan_num
		line(1:1) = ' '
		dowhile  (line(1:5) .ne. 'DELTA')
			read( ldat, '(a80)' ) line
		enddo
c		read( line, '(''DELTA: '',f12)' ) chan_dt(i)
		read( line(8:80), '(f12)' ) chan_dt(i)
c		read( ldat, '(''LENGTH: '',i8)' ) chan_lth(i)
		read( ldat, '(8x,i8)' ) chan_lth(i)
		if  ((index+chan_lth(i)) .gt. max_arrlth)  stop 'too many samples'
		read( ldat, * ) (fdat(j),j=index,index+chan_lth(i)-1)
		chan_ptr(i) = index
		index = index + chan_lth(i)
		wdw_start(i) = 0.0
		wdw_end(i) = float(chan_lth(i))*chan_dt(i)
	enddo

	close( ldat )

	if  (verbosity .ge. 2)  then
		do  i=1,chan_num
			write(*,*) station(i)(1:5), chan_lth(i), chan_dt(i),
     &	   chan_ptr(i), stat_x(i), stat_y(i), elevation(i)
		enddo
	endif

c	call fk
	theta = 90.0
	instr = 'VERTICAL'
	clxdim = max_clxdim
	wrkdim = max_wrkspc
	call slbbfk( fdat, chan_ptr, chan_lth, chan_dt, wdw_start, wdw_end,
     &  station, instr, frq_lo, frq_hi, stat_x, stat_y, elevation,
     &  max_slowness_km, chan_num, alpha, beta, theta, grid_pts,
     &  work_spc, wrkdim, clxbuf, clxdim, app_veloc,
     &  azimuth, sx_max, sy_max, frq_lo_out, frq_hi_out,
     &  power, abs_power, quality, message, verbosity )

	if  (unit_in_deg)  then
		app_veloc = app_veloc / 111.19
		sx_max = sx_max * 111.19
		sy_max = sy_max * 111.19
	endif
	res_slowness = 1.0 / app_veloc

	if  (verbosity .gt. 0)  then
		write (*,*) 'apparent velocity ', app_veloc
		write (*,*) 'azimuth ', azimuth
		write (*,*) 'Sx max ', sx_max
		write (*,*) 'Sy max ', sy_max
		write (*,*) 'low frq ', frq_lo_out
		write (*,*) 'high frq ', frq_hi_out
		write (*,*) 'power ', power
		write (*,*) 'abs power ', abs_power
		write (*,*) 'quality ', quality
		write (*,*) 'msg ', message(1:70)
	endif

c	write output file
	write( lout, '(a,f7.2)' )
     & '!* apparent veloc. : ', app_veloc
	write( lout, '(a,f7.2)' )
     & '!* slowness        : ', res_slowness
	write( lout, '(a,f7.1)' )
     & '!* azimuth         : ', azimuth
	write( lout, '(a,f7.2,a,f7.2)' )
     & '!* max. slow.      : ', sx_max, ',', sy_max
	write( lout, '(a,f7.2)' )
     & '!* slowness range  : ', max_slowness
	write( lout, '(a,f7.2,a,f7.2)' )
     & '!* frq. wdw.       : ', frq_lo_out, ',', frq_hi_out
	write( lout, '(a,f7.2)' )
     & '!* rel. power      : ', power
	write( lout, '(a,f7.2)' )
     & '!* abs. power      : ', abs_power
	write( lout, '(a,i7)' )
     & '!* quality         : ', quality
	write( lout, '(i4,i4)' ) grid_pts, grid_pts
c	index = 1
c	do  i=1,grid_pts
c		write( lout, '(g'' '')' ) (work_spc(j),j=index,index+grid_pts-1)
c		index = index + grid_pts
c	enddo
	do  i=1,grid_pts*grid_pts
		write( lout, * ) work_spc(i)
	enddo
	close( lout )

	end ! of program fk

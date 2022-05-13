c
c	PROGRAM DESCRIPTION: 	BEAM_DELAYS
c
c Computation of time delays of seismic array stations necessary 
c for beamforming
c
c CREATION DATE: 	8. January 1992
c
c
c		C H A N G E  L O G
c 
c      Date     |       Name     |         Description
c---------------|----------------|-------------------------------------
c 08-01-92      |     Henger	 |  Version 1.1
c---------------|----------------|-------------------------------------
c
c	FUCTIONS CALLED:    none
c
c	SUBROUTINES CALLED: DMS2DEG (degrees,minutes,seconds to degrees
c	 		    REC2POL (Cartesion coordinates to polar coordinates
c			    SHIFTV  (Time shifts to be applied for beamforming
c---------------------------------------------------------------------
      implicit integer (a-z)
      parameter (nst=50)
      character evfil*60, stpar(nst)*60,stpar0*60,stcode(nst)*60,
     *            stcode0*4,bm_code*5
      real lat(nst),lon(nst),dis(nst),azi(nst),latref,lonref
      real bm_azia,bm_azie,bm_azinc,bm_vela,bm_vele,bm_velinc
      real bm_az,bm_vel,sr,dt,td1000(nst),flow,fup,dum(7)
      dimension itd(nst)
      data dsk/3/,dsk1/11/
c---------------------------------------------------------------------
c      goto 1111
c      write(*,5)
c      read(*,10) len,evfil
       evfil='/home/b3sn04/nico/SEIS/opbeam/datin/grss_koo.dat'
c
5     format(//,' Enter filename containing array coordinates > ',$) 
10    format(q,a)
12    format(q,a)

      open(unit=dsk,file=evfil,status='old',form='formatted')
      n = 1
20    read(dsk,12,err=5000,end=5100)lenp,stpar(n)
      if(stpar(n)(1:1).ne.'C'.and.stpar(n)(1:1).ne.'c') then
c
c......Conversion of array coordinates into degrees and fraction of degrees
c
         stcode(n)(1:4)=stpar(n)(1:4)
         read(stpar(n)(7:8),'(f4.0)',err=5200) dum(1)  	! encode degrees
         read(stpar(n)(10:11),'(f4.0)',err=5200) dum(2) 	! encode minutes
         read(stpar(n)(13:18),'(f6.3)',err=5200) dum(3) 	! encode seconds
         read(stpar(n)(25:26),'(f4.0)',err=5200) dum(4)	! encode degrees
         read(stpar(n)(28:29),'(f4.0)',err=5200) dum(5) 	! encode minutes
         read(stpar(n)(31:36),'(f6.3)',err=5200) dum(6) 	! encode seconds
         read(stpar(n)(40:46),'(f7.2)',err=5200) dum(7) 	! encode station altitute
c
	     call dms2deg(dum(1),dum(2),dum(3),lon(n))	! convert to degrees
	     call dms2deg(dum(4),dum(5),dum(6),lat(n))
c
	     write(*,15) n,stcode(n),(dum(i),i=1,7),lon(n),lat(n)
15	     format(1x,i3,1x,a,2x,2f4.0,f6.3,1x,2f4.0,f6.3,1x,f7.2,2f8.4)
	     n=n+1
	     go to 20
c
      else
         go to 20
      endif
5100	write(*,5020)
5020	format(' end of file reached ')
      close(dsk)
      nmax = n-1
c
c......Select array reference station
c
      write(*,30) 
30    format(1x,//' Enter station code of the array reference station ',
     *  '(GEC2)  > ',$)
      read(*,10) len1,stcode0
      if(len1.eq.0) then
   	     stcode0='GEC2'
      endif
c
      do 40 n=1,nmax
        if(stcode0(1:4).eq.stpar(n)(1:4)) then
           stpar0(1:)=stpar(n)(1:)
           nref=n
        endif
40    continue
      write(*,35) nref,stpar0,lat(nref),lon(nref)
35    format(/,' Reference station parameters',/ 1x,i3,1x,a,2f8.4,/)
c
c.......calculate locations of array stations in polar coordinates with
c       respect to reference station
c
      open(unit=dsk1,file='geress_polar',status='unknown',
     *       form='formatted')
      do 60 n=1,nmax
ct        call rec2pol(lat(n),lon(n),lat(nref),lon(nref),dis(n),azi(n))
        call episub (lat(n),lon(n),lat(nref),lon(nref),
     *               dum(1),dis(n),azi(n))
        write(*,66) n,stcode(n),lat(n),lon(n),lat(nref),lon(nref),
     *              dis(n),azi(n)
        write(dsk1,65,err=5300) n,stcode(n),lat(n),lon(n),
     *                          dis(n),azi(n)
60    continue
c
65    format(i3,1x,a5,1x,2f12.8,2x,f8.4,1x,f8.4)
66    format(1x,i3,1x,a5,1x,2f8.4,2x,2f8.4,2x,f8.4,1x,f8.4)
      close(dsk1)
c---------------------------------------------------------------------
      goto 1111
      open (unit=50,file='/home/b3sn04/nico/SEIS/opbeam/datin/koor1',
     *      status='old')
      read (50,'(a)') dum(1)
      read (50,'(a)') dum(1)
      read (50,'(a)') dum(1)
      read (50,*) nmax
      do 91 i=1,nmax
        read (50,'(a4,1x,f13.3,2x,f13.3)') stcode(i),lat(i),lon(i)
        write (*,'(a4,1x,f13.3,2x,f13.3)') stcode(i),lat(i),lon(i)
91    continue
      read (50,'(a)') dum(1)
      read (50,'(a4,1x,f13.3,2x,f13.3)') stcoderef,latref,lonref
      write (*,'(a4,1x,f13.3,2x,f13.3)') stcoderef,latref,lonref
      do 92 i=1,nmax
        lat(i)=(lat(i)-latref)/1000.
        lon(i)=(lon(i)-lonref)/1000.
        dis(i)=sqrt(lat(i)**2.+lon(i)**2.)
        azi(i)=atan2(lat(i),lon(i))
        write (*,'(a4,4f13.3)') stcode(i),lat(i),lon(i),dis(i),azi(i)
92    continue
1111  continue
c---------------------------------------------------------------------
c
c.......calculation of beam time shifts
c
69    write(*,70)
      read(*,71,err=69) lenc,bm_code
70    format(//,' Enter beamcode (e.g. GER01)     > ',$)
71    format(q,a)
c
73    write(*,72)
      read(*,*,err=73) bm_azia,bm_azie,bm_azinc
72    format(' Enter azimuth range, azimuth increment',
     *       /,' (e.g. 0.,360,30)        > ',$)
      if(bm_azinc.eq.0.) bm_azinc=360.
79    write(*,80)
      read(*,*,err=79) sr, bm_vela,bm_vele,bm_velinc
80    format(' Enter sampling rate, velocity range & velocity increment',
     *       /,' (e.g. 40.,4.0,8.0,1.0)  > ',$)
      if(bm_velinc.eq.0.) bm_velinc=1.
81    write(*,82)
      read(*,*,err=81) flow,fup
82    format(' Enter lower & upper corner frequencies of bandpass',
     *  ' filter',/,' (e.g. 8.0,16.)          > ',$)
c
      open(unit=dsk,file='grss_dly',status='unknown',
     *       form='formatted')
      open(unit=dsk1,file='grss_dly_ix',status='unknown',
     *       form='formatted')
      write(dsk,116,err=5300) (stcode(k),k=1,nmax)
      write(dsk1,116,err=5300) (stcode(k),k=1,nmax)
ct      write(*,('(30(a,1x))'),err=5300) (stcode(k),k=1,nmax)
116   format(' beam srate km/s degree  flow   fup   ',30(a4,2x))
c
      bm_vel= bm_vela - bm_velinc
      dt = 1.0/sr
      i = 0
85    i=i+1
      bm_vel = bm_vel + bm_velinc
      if(bm_vel.le.bm_vele) then
        bm_az = bm_azia - bm_azinc
88      if(bm_az.lt.bm_azie) then
	      bm_az = bm_az + bm_azinc
c
	      call shiftv(dis,azi,nmax,bm_vel,bm_az,td1000,dt,
     *              itd)
c
ct	      write(*,110) bm_code,sr,bm_vel,bm_az,flow,fup
ct	      write(*,112) (td1000(j),j=1,nmax)
110	      format(/,1x,a5,1x,f4.0,1x,f4.1,1x,f5.1,1x,2f6.1,/)
112	      format(1x,12f6.0)
ct	      write(*,115) (itd(j),j=1,nmax)
115	      format(1x,12i6)
c
	      write(dsk,120,err=5300) bm_code,sr,bm_vel,bm_az,flow,fup,
     *                            (td1000(j),j=1,nmax)
120	      format(1x,a5,1x,f4.0,1x,f4.1,1x,f5.1,1x,2f6.1,1x,25f6.0)
	      write(dsk1,125,err=5300) bm_code,sr,bm_vel,bm_az,flow,fup,
     *                             (itd(j),j=1,nmax) 
125	      format(1x,a5,1x,f4.0,1x,f4.1,1x,f5.1,1x,2f6.1,1x,25i6)
	      go to 88
	    endif	   
        goto 85
      endif
      close(dsk)
      close(dsk1)
c
      stop ' end of program beam_delays'
c
5000	write(*,5010) 
5010	format(' file read error !')
        stop
5200	write(*,5210) n
5210	format(' error decoding data of station', i3)
        stop
5300	write(*,5310) 
5310	format(' file write error !')
        stop
        end

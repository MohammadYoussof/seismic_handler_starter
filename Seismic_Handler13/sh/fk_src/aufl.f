c programm: aufl
c
c programm zur berechnung der maximalen aufloesung des
c geress arrays bei vorgegebener kleinster geschwindigkeit
c und darstellung der ergebnisse
c
       real x(200),y(200),rmax,pi,dt,va,ve
       integer lx,nn
       character file*70
c
       rmax=3.925
       pi=acos(-1.)
       dt=0.025
       va=2.
       ve=20.
       lx=100
       nn=1
c
       file='/home/b3sn04/nico/SEIS/opbeam/datout/aufl.dat'
       open (unit=10,file=file,status='unknown',iostat=ierr)
           if (ierr.gt.o) then
             write(luo,'("Fehler beim oeffnen des files opbeam.test!")')
             stop     'error'
           end if
c
       w=180./pi
       dv=(ve-va)/(lx-1)
c
       write (10,*) nn
       write (10,*) nn,lx
c
       do 1 i=1,lx
         x(i)=va+(i-1)*dv
c         y(i)=w*asin(x(i)*dt/rmax)
         y(i)=rmax/(rmax/x(i)-dt)-x(i)
         write (10,*) x(i),y(i)
1      continue
c
       stop      'aufl'
       end

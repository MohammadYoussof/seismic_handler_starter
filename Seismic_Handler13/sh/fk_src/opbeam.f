c     Programm  opbeam 
c     Version:  16.06.93     BGR, N. Gestermann
c
c     Das Programm liest einen GERESS Datensatz (im
c     CSS-Format) und berechnet den optimum beam.
c
c     Die Daten werden jetzt ohne jegliche Beruecksichtigung einer 
c     Vorverstaerkung behandelt. Also Counts bleiben Counts!!
c
c      Unterprogramme:
c
c ----------------------------------------------------------------------------
c Variablendeklaration der Uebergabeparameter an 'getcss'
c und aus dem .wfdisc-file
      integer*4 msamp, mstat,mprefix,sttrac,i1,i2
      Parameter ( msamp = 10000)
      Parameter ( mstat = 40)
      Parameter ( mprefix = 20)
      parameter ( i1 = 25)
      parameter ( i2 = 10000)
      parameter ( i3 = 15000)
      integer*4 idata(msamp),lastsamp,nss
      real*4    rdata(msamp),generatorconst,period,sr,second,dtime
      integer*4 datjul,year,month,day,hour,minute,lui,luo,lut,ierr
      character stationcode*8, channel*4,filewfd*98,
     *          instrumenttype*8,prefix*(mprefix),wfdpath*70,
     *          comment*32,remark*30
      real*8    stime,time1,time2,tim2
      equivalence (idata(msamp),rdata(msamp))
c ----------------------------------------------------------------------------
c Lokale Variablendeklaration
      real*4 s(i1,i2),dt,s1,s2,s3,tanf,tend,se,
     *       sti(mstat),stevla(mstat),stevlo(mstat),stevel(mstat),
     *       vel(i3),az(i3),du,fiu,fio
      integer*4 trimle,idoy,sttrace(mstat),it(i1,i3),iz,idm,
     *          idd,nn,lx,iy,im,ida,ido,ih,imi,iss(5)
      real*8 ntime
      character*12 kname,knameo,sta(mstat)
      character c1*1,c2*10,ident*70,staa*8,ca*4,file*70,cdk*4,
     *          bdfile*20,strinh*20,bsta(mstat)*6
c---------------------------------------------------------------------
c        ieeer=ieee_handler ("set","underflow",myhandler)
c        ieeer=ieee_handler ("set","common",myhandler)
c        if (ieeer.ne.0) write (6,'("Could not establish fp ",
c     *          "signal handler !",)')
c---------------------------------------------------------------------
c Festlegung der Parameter
      data luo,lui,lut/6,5,0/
c     lut:     lut=0 : kein Protokoll
c              lut=6 : Test-Protokoll auf Bildschirm
c              lut=10:  "       "     in File ./proto
c diverses
      data n,nn,in/0,0,1/
      data maxtrace,time1,time2/0,999999999999.,-999999999999./
c ----------------------------------------------------------------------------
c oeffnen eines files zu testzwecken
       open (unit=80,
     *         file='/home/b3sn04/nico/SEIS/opbeam/datout/opbeam.test',
     *         status='unknown',iostat=ierr)
           if (ierr.gt.o) then
             write(luo,'("Fehler beim oeffnen des files opbeam.test!")')
             goto 999
           end if
c ----------------------------------------------------------------------------
c Einlesen der Parameter und Stationsnamen 
       open (unit=20,
     *         file='/home/b3sn04/nico/SEIS/opbeam/bin/opbeam-in',
     *         status='old',iostat=ierr)
           if (ierr.gt.o) then
             write(luo,'("Fehler beim oeffnen des files opbeam-in!")')
             goto 999
           end if
c
        read(20,'(a70)') ident
        read(20,*) tanf,tend
        read(20,*) iz,idt
        read(20,*) iss(1)
        read(20,'(a)') cdk
        read(20,'(a)') bdfile
        read(20,*) fiu,fio
c
      do 79 i=1,mstat
        read (20,'(a1,1x,a10,2f10.5,f10.3)',end=78) c1,c2,s1,s2,s3
        if (c1.ne.'-') then
           nn=nn+1
           sta(nn)=c2
           stevlo(nn)=s1
           stevla(nn)=s2
           stevel(nn)=s3
           sti(nn)=0.
        end if
79    continue
78    close (unit=20)
c-------------------------------------------------------------------------------
c einlesen des maximalen Zeitfensters und der sttraces aus dem .wfdisc file
c get prefix from environment variable  WFDPREFIX
      call getenv('WFDPATH',wfdpath)
      call getenv('WFDPREFIX',strinh)
      if (strinh(1:1) .eq. ' ') then
        write(luo,'("program opbeam, please give prefix for",
     *    " data to read (max 20 chars) >",$)')
        read(lui,*) prefix
      else
        read(strinh,'(a)') prefix
      endif
      write (luo,'(/,"Event Identification: ",a15,/)') prefix
c
c testoutput
      if (lut.eq.10) open (unit=lut,file='proto')
      if (lut.gt.0) write(lut,'("PREFIX :",a)') prefix
c
c open .wfdisc file
      filewfd=wfdpath(1:trimle(wfdpath))//'/'
     *       //prefix(1:trimle(prefix))//'.wfdisc'
      open(unit=60,file=filewfd,err=61)
      goto 71
61    write(luo,'("!! error open .wfdisc-file, return !!")')
      goto 999
c
c read .wfdisc file time window and calculate starttime
71    maxtrace=maxtrace+1
      read (60,'(8x,f16.3,1x,a6,1x,a2,1x,i8,1x,f11.7,123x,a30)',end=70)
     *           stime,staa,ca,nss,sr,remark
      kname=staa(1:trimle(staa))//ca
      do 72 i=1,nn
        if (kname(1:trimle(kname)).eq.sta(i)(1:trimle(sta(i)))) then 
           sti(i)=1.
           if (kname.ne.knameo) then
             n=n+1
             sttrace(n)=maxtrace
             knameo=kname
           end if
        end if
72    continue
      tim2=dreal(float(nss)/sr)+stime
      if(tim2.gt.time2) time2=tim2
      if(stime.lt.time1) time1=stime
      go to 71

70    maxtrace=maxtrace-1
      dtime=sngl(time2-time1)
c
c Anzahl der Stationen ist n (gewueschte Anzahl ist nn)
c rausschmeissen der Stationen, fuer die keine Daten zur Verfuegung stehen
      nk=nn
52    if (sti(in).eq.0) then
        write (*,'("Keine Daten fuer Station: ",a8)') sta(in)
        nk=nk-1
        if (in.eq.nn) goto 51
        do 53 k=in,nn-1
          sta(k)=sta(k+1)
          stevlo(k)=stevlo(k+1)
          stevla(k)=stevla(k+1)
          stevel(k)=stevel(k+1)
          sti(k)=sti(k+1)
53      continue
      else
        in=in+1
      end if
51    if (in.le.nk) goto 52
      nn=nk
      if (nn.eq.0) then
        write (*,'("Es koennen keine Daten eingelesen werden!")')
        stop    'opbeam'
      end if
c
c convert epochal to human time for time variables
      call fetoh(time1,idao,year,month,mname,day,idoy,hour,minute
     *          ,second)
c ----------------------------------------------------------------------------
c Bestimmung der Anfangszeit des plots
      ntime=time1+dble(tanf)
      call fetoh (ntime,idd,iy,im,mnam,ida,ido,ih,imi,se)
c ----------------------------------------------------------------------------
c Stationsschleife
      do 77 jj=1,n
        sttrac=sttrace(jj)
        if(sttrac.gt.maxtrace) then
          write (*,'("sttrace.gt.maxtrace!!")')
          go to 999
        end if
        call rdcss   (idata,int,sttrac,lastsamp,msamp,
     *                prefix,time1,lui,luo,lut,ierr,mstat,
     *                channel,stationcode,instrumenttype,
     *                samprat,period,generatorconst,
     *                stime,datjul,comment)
c
        kname=stationcode(1:trimle(stationcode))//channel
        write (luo,'("Einlesen der Daten von Station: ",a10)') kname
c
        do 3 j=1,nn
          if (kname(1:trimle(kname)).eq.sta(j)(1:trimle(sta(j)))) then 
            dt=1/samprat
            ianf=nint(tanf/dt)+1
            iend=nint(tend/dt)+1
            if (iend.gt.lastsamp) iend=lastsamp
            if (remark(1:4).eq.'theo') ianf=ianf+1
c           s1=generatorconst
            s1=1.
c
            lx=0
            if (int.eq.0) then
              do 8 i=ianf,iend,1    
                lx=lx+1
                s(j,lx)=float(idata(i))*s1
8             continue
            else if (int.eq.1) then
              do 9 i=ianf,iend,1
                lx=lx+1
                s(j,lx)=rdata(i)*s1
9             continue
            end if
          end if
3       continue
77    continue
c
c ----------------------------------------------------------------------------
c oeffnen des output-files
      file='/home/b3sn04/nico/SEIS/opbeam/datout/'//
     *     'opbeam'//cdk(1:trimle(cdk))//'.out'
      open (unit=70,file=file,status='unknown',iostat=ierr)
      if (ierr.ne.0) then
         write (*,'("Fehler beim oeffnen von: ",a)')file
         goto 999
      end if
      write (70,'(a4)') cdk
      write (70,'("100.  140.   4  1   aaw,aew,inta,intza      ",
     *       "   Azimut")')
      write (70,'("  2.   12.   5  2   vaw,vew,intv,intzv      ",
     *       "   Geschwindigkeit")')
      write (70,'("  0.    1.   5  2   paw,pew,intp,intzp      ",
     *       "   Power")')
      write (70,'("  0.  100.  10  2   taw,tew,intt,intzt      ",
     *       "   Zeit")')
      write (70,'("100.  200.  10 10   azwa,azwe,intaw,intzaw  ",
     *       "   Azimut-Verteilung x-Achse")')
      write (70,'("  0.  1.0   10  2   ava,ave,intav,intzav    ",
     *       "   Azimut-Verteilung y-Achse")')
      write (70,'("15.  0. 1 1         daz,mw,is(7),is(8)      ",
     *       "   Mittelwert,Streuung")')
      write (70,'("0.02 2. 40.         sn,azda,azd             ",
     *       "   Azimutverteilung")')
      write (70,'("10 20 0 3 2 2 1     m,pts,is(1)...is(5)     ",
     *       "   Kurve Geschwindigkeit")')
      write (70,'("0.00 4              sw,swq                  ",
     *       "   schwellwerte power,quality")')
      write (70,'("1                   nt                      ",
     *       "   markierte Zeiten")')
      write (70,'("      0.00 ")')
      write (70,'("                    comment                 ",
     *       "   Kommentar")')
      du =0.
      dd=iz*2.*dt
      write (70,'(i4,"-",i3,":",i2,".",i2,".",f6.3,x,a3,5x,
     *       "(beam-matrix)")') iy,ido,ih,imi,se,cdk
      write (70,*) fiu,fio
      write (70,*) du,du
      write (70,*) dd,dd,idt*dt
c ----------------------------------------------------------------------------
c einlesen der beamdelay-matrix
      file='/home/b3sn04/nico/SEIS/opbeam/datin/'
     *     //bdfile(1:trimle(bdfile))
      open (unit=50,file=file,status='old',iostat=ierr)
      if (ierr.ne.0) then
         write (*,'("Fehler beim oeffnen des beamdelay-files !")')
         goto 999
      end if
c einlesen der stationsnamen
      ns=25
      read (50,'(37x,25(a4,1x))') (bsta(i),i=1,ns)
c einlesen der delays
      k=1
13    read (50,'(11x,f5.1,f6.1,14x,25i6)',end=14)
     *       vel(k),az(k),(it(i,k),i=1,ns)
      k=k+1
      goto 13
14    nb=k-1
c vergleich vorhandener stationen mit matrix-stationen
c entsprechende aenderung der matrix
c bestimmung der maximalen verschiebung
      ila=0
      idm=0
      do 15 l=1,nn
        do 16 i=1,ns
          if (bsta(i)(1:4).eq.sta(l)(1:4)) then
            if (ila.ge.i) then
              write (*,'("falsche Stationsreihenfolge!")')
              goto 999
            end if
            do 17 k=1,nb
              it(l,k)=it(i,k)
              idm=max(abs(idm),abs(it(l,k)))
17          continue
            bsta(l)=bsta(i)
            ila=i
            goto 15
          end if
16      continue
        write (*,'("station: ",a4," ist in der Beam-Matrix nicht",
     *         " vorhanden")') sta(l)(1:4)
15    continue
ct      write (*,'(37x,25(a4,1x))') (bsta(i),i=1,ns)
ct      do 88 k=1,nb
ct      write (*,'(11x,f5.1,f6.1,14x,25i6)')
ct     *       vel(k),az(k),(it(i,k),i=1,nn)
ct88    continue
c ----------------------------------------------------------------------------
c normierung der spuren
      if (iss(1).eq.1) then
        do 18 i=1,nn
          call norm (s,lx,i,i1,i2)
18      continue
      end if
c ----------------------------------------------------------------------------
c berechnung des optimum-beams
      write (*,'("Aufruf von beam!")')
      call beam (s,nn,lx,it,nb,vel,az,iz,idt,dt,idm,i1,i2,i3)
c ----------------------------------------------------------------------------
      close (unit=80)
c
999   stop    'opbeam'
      end
c ----------------------------------------------------------------------------

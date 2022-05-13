c     Programm  beamtrace
c     Version:  14.09.93     BGR, N. Gestermann
c
c     Das Programm liest einen GERESS Datensatz (im
c     CSS-Format) und berechnet die optimum beam-spur mit den
c     ergebnissen (geschw. und azimut fuer die einzelnen
c     zeitfenster) des programms opbeam.
c
c     Die Daten (css-Format) werden jetzt ohne jegliche Beruecksichtigung
c     einer Vorverstaerkung behandelt. Also Counts bleiben Counts!!
c
c      Unterprogramme:
c
c---------------------------------------------------------------------
c Variablendeklaration der Uebergabeparameter an 'getcss'
c und aus dem .wfdisc-file
      integer*4 msamp, mstat,mprefix,sttrac,i1,i2
      Parameter ( msamp = 10000)
      Parameter ( mstat = 40)
      Parameter ( mprefix = 20)
      parameter ( i1 = 25)
      parameter ( i2 = 10000)
      parameter ( i3 = 15000)
      parameter (i4 = 400)
      integer*4 idata(msamp),lastsamp,nss
      real*4    rdata(msamp),generatorconst,period,sr,second,dtime,
     *          fe(i4)
      integer*4 datjul,year,month,day,hour,minute,lui,luo,lut,ierr
      character stationcode*8, channel*4,filewfd*98,
     *          instrumenttype*8,prefix*(mprefix),wfdpath*70,
     *          comment*32,remark*30
      real*8    stime,time1,time2,tim2
      equivalence (idata(msamp),rdata(msamp))
c---------------------------------------------------------------------
c Lokale Variablendeklaration
      real*4 s(i1,i2),b(i2),bt(i2),dt,s1,s2,s3,tanf,tend,se,
     *       sti(mstat),stevla(mstat),stevlo(mstat),stevel(mstat),
     *       stdi(mstat),staz(mstat),tv(i1)
      integer*4 tle,idoy,sttrace(mstat),itv(i1),
     *          idd,nn,lx,iy,im,ida,ido,ih,imi,iss(5)
      real*8 ntime
      character*12 kname,knameo,sta(mstat)
      character c1*1,c2*10,ident*70,staa*8,ca*4,file*70,
     *          strinh*20,cid*10,cdu*70
c---------------------------------------------------------------------
c Variablen-Deklaration fuer .wfdisc-file (output)
        integer*4 date,nsamp,chid,wfid,foff,adate
        real*8 time
        real*4 smprat,calib,calper
        character stat*6,chan*2,instyp*6,segtyp*1,dattyp*2,clip*1,
     *            dir*30,dfile*20
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
      pi=acos(-1.)
c ----------------------------------------------------------------------------
c oeffnen eines files zu testzwecken
      open (unit=80,
     *     file='/home/b3sn04/nico/SEIS/opbeam/datout/beamtrace.test',
     *     status='unknown',iostat=ierr)
       if (ierr.gt.0) then
          write(luo,'("Fehler beim oeffnen des files beamtrace.test!")')
          goto 999
       end if
c ----------------------------------------------------------------------------
c Einlesen der Parameter und Stationsnamen 
      open (unit=20,
     *      file='/home/b3sn04/nico/SEIS/opbeam/bin/beamtrace-in',
     *      status='old',iostat=ierr)
      if (ierr.gt.o) then
         write(luo,'("Fehler beim oeffnen des files beamtrace-in!")')
         goto 999
      end if
c
        read(20,'(a)') ident
        read(20,*) tanf,tend
        read(20,'(a)') cid
        read(20,*) iss(1),iss(2),iss(3)
c
      do 79 i=1,mstat
        read (20,'(a1,1x,a10,2f12.8,f10.3,1x,2f9.4)',end=78)
     *             c1,c2,s1,s2,s3,s4,s5
        if (c1.ne.'-') then
           nn=nn+1
           sta(nn)=c2
           stevlo(nn)=s1
           stevla(nn)=s2
           stevel(nn)=s3
           stdi(nn)=s4
           staz(nn)=s5
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
        write(luo,'("program beamtrace, please give prefix for",
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
      filewfd=wfdpath(1:tle(wfdpath))//'/'
     *       //prefix(1:tle(prefix))//'.wfdisc'
      open(unit=60,file=filewfd,status='old',err=61)
      goto 71
61    write(luo,'("!! error open .wfdisc-file, return !!")')
      goto 999
c
c read .wfdisc file time window and calculate starttime
71    maxtrace=maxtrace+1
      read (60,'(8x,f16.3,1x,a6,1x,a2,1x,i8,1x,f11.7,123x,a30)',end=70)
     *           stime,staa,ca,nss,sr,remark
      kname=staa(1:tle(staa))//ca
      do 72 i=1,nn
        if (kname(1:tle(kname)).eq.sta(i)(1:tle(sta(i)))) then 
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
          stdi(k)=stdi(k+1)
          staz(k)=staz(k+1)
          sti(k)=sti(k+1)
53      continue
      else
        in=in+1
      end if
51    if (in.le.nk) goto 52
      nn=nk
      if (nn.eq.0) then
        write (*,'("Es koennen keine Daten eingelesen werden!")')
        stop    'beamtrace'
      end if
c
c convert epochal to human time for time variables
      call fetoh(time1,idao,year,month,mname,day,idoy,hour,minute
     *          ,second)
c ----------------------------------------------------------------------------
c Bestimmung der Anfangszeit
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
        kname=stationcode(1:tle(stationcode))//channel
        write (luo,'("Einlesen der Daten von Station: ",a10)') kname
c
        do 3 j=1,nn
          if (kname(1:tle(kname)).eq.sta(j)(1:tle(sta(j)))) then 
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
c beamspuren auf null setzen
      do 19 i=1,i2
        b(i)=0.
        bt(i)=0.
19    continue
c ----------------------------------------------------------------------------
c normierung der spuren
      if (iss(1).eq.1) then
        write (*,'("Die Spuren werden normiert!")')
        do 18 i=1,nn
          call norm (s,lx,i,i1,i2)
18      continue
      end if
c ----------------------------------------------------------------------------
c einlesen der daten vom file: azvel'cid'.in
      file='/home/b3sn04/nico/SEIS/azvel/datin/azvel'//
     *      cid(1:tle(cid))//'.in'
      open (unit=50,file=file,status='old',iostat=ierr)
      if (ierr.ne.0) then
         write (*,'("Fehler beim oeffnen des files !")')
         goto 999
      end if
c
      read (50,'(a)') (cdu,i=1,11)
      read (50,*) k
      read (50,'(a)') (cdu,i=1,k)
      read (50,'(a)') cdu
      read (50,'(a)') cdu
      write (*,'(a)') cdu
      read (50,'(a)') (cdu,i=1,2)
      read (50,*) rtla,rtle
c bestimmung der fensterfunktion
      if (iss(2).eq.1) then
        write (*,'(a)') 'Fensterung wird durchgefuehrt'
        if (rtla.ne.rtle) then
          write (*,'(a)') 'Keine konstante Fensterlaenge!'
          stop      'error'
        else
          nf=nint(rtla/dt)+1
          nfv=nint(rtla/dt/4.)
          do 62 i=1,nf
            if (i.ge.1.and.i.le.nfv) then
              fe(i)=0.5*(1.-cos(pi*float(i-1)/nfv))
            else if (i.le.nf.and.i.gt.(nf-nfv)) then
              fe(i)=0.5*(1.+cos(pi*float(i-nf+nfv)/nfv))
            else
              fe(i)=1.
            end if
62        continue
        end if
      end if
c ----------------------------------------------------------------------------
c berechnung der beamspur
c     einlesen der werte aus dem azvel-file
21    read (50,*,end=22) vb,azb,rdu1,rdu2,ta,te
ct      write (*,*) vb,azb,rdu1,rdu2,ta,te
      ita=nint(ta/dt)+1
      ite=nint(te/dt)+1
c berechnung des aktuellen beam-vektors
      call shiftv (stdi,staz,nn,vb,azb,tv,dt,itv)
c berechnung des beams fuer das zeitfenster
      call beamtr (s,nn,lx,ita,ite,bt,itv,i1,i2,i3)
c fenstern des zeitfensters
      if (iss(2).eq.1) then
        do 23 i=ita,ite
           bt(i)=bt(i)*fe(i-ita+1)
23      continue
      end if
c addition der beams
      if (iss(3).eq.1) then
        do 20 i=ita,ite
           b(i)=b(i)+bt(i)/nn
20      continue
      else if (iss(3).eq.2) then
        do 24 i=ita,ite
           b(i)=b(i)+(bt(i)/nn)**2.
24      continue
      end if
      goto 21
22    continue
c ----------------------------------------------------------------------------
c ausgabe des beams im css-format
c oeffnen des .wfdisc-files
      file='/home/b3sn04/da/css/obdata/'
     *     //'obspu'//cid(1:tle(cid))//'.wfdisc'
      open (unit=30,file=file,status='unknown',iostat=ierr)
      if (ierr.ne.0) then
         write (*,'("Fehler beim oeffnen des wfdisc-files !")')
         goto 999
      end if
c
c belegung der variablen im .wfdisc-file
      date=idd
      time=ntime
      stat='OBSP'
      chan=channel
      nsamp=lx
      smprat=samprat
      calib=1.
      calper=1.
      instyp=instrumenttype
      segtyp='o'
      dattyp='f4'
      clip='n'
      chid=1
      wfid=1
      dir='./data'
      dfile='obspu'//cid(1:tle(cid))//'.w'
      foff=0
      adate=date
      remark='optimum-beam'
c
c  Test-ausdruck
c       write (luo,'("date       : ",i8)') date
c       write (luo,'("time       : ",f15.3)') time
c       write (luo,'("stat       : ",a6)') stat
c       write (luo,'("chan       : ",a2)') chan
c       write (luo,'("nsamp      : ",i8)') nsamp
c       write (luo,'("smprat     : ",f11.7)') smprat
c       write (luo,'("calib      : ",f9.6)') calib
c       write (luo,'("calper     : ",f7.4)') calper
c       write (luo,'("instyp     : ",a6)') instyp
c       write (luo,'("segtyp     : ",a1)') segtyp
c       write (luo,'("dattyp     : ",a2)') dattyp
c       write (luo,'("clip       : ",a1)') clip
c       write (luo,'("chid       : ",i8)') chid
c       write (luo,'("wfid       : ",i8)') wfid
c       write (luo,'("dir        : ",a30)') dir
c       write (luo,'("dfile      : ",a20)') dfile
c       write (luo,'("foff       : ",i10)') foff
c       write (luo,'("adate      : ",i8)') adate
c       write (luo,'("remark     : ",a30)') remark
c
c  Rausschreiben der wfdisc-Parameter
      write (30,31) date,time,stat,chan,nsamp,smprat,calib,calper
     *             ,instyp,segtyp,dattyp,clip,chid,wfid,dir
     *             ,dfile,foff,adate,remark
31    format (i8,1x,f15.3,1x,a6,1x,a2,1x,i8,1x,f11.7,1x,
     *        f9.6,1x,f7.4,1x,a6,1x,a1,1x,a2,1x,a1,1x,i8,
     *        1x,i8,1x,a30,1x,a20,1x,i10,1x,i8,1x,a30)
c
c
      file='/home/b3sn04/da/css/obdata/data/'//dfile(1:tle(dfile))
      open (unit=40,file=file,status='unknown',form='unformatted',
     *      iostat=ierr)
      if (ierr.ne.0) then
         write (*,'("Fehler beim oeffnen des daten-files !")')
         goto 999
      end if
c
      write (40) (b(i),i=2,lx)
c
      close (unit=30)
      close (unit=40)
      close (unit=50)
c ----------------------------------------------------------------------------
      close (unit=80)
c
999   stop    'beamtrace'
      end
c ----------------------------------------------------------------------------

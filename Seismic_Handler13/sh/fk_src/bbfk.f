c     Programm  bbfk
c     Version:  19.10.93     BGR, N. Gestermann
c
c     Das Programm liest einen GERESS Datensatz (im
c     CSS-Format) und berechnet das f,k-spektrum.
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
      parameter ( i1 = 45)
      parameter ( i2 = 500000)
      integer*4 idata(msamp),lastsamp,nss
      real*4    rdata(msamp),generatorconst,period,sr,second,dtime
      integer*4 datjul,year,month,day,hour,minute,lui,luo,lut,ierr
      character stationcode*8, channel*4,filewfd*98,
     *          instrumenttype*8,prefix*(mprefix),wfdpath*70,
     *          comment*32,remark*30
      real*8    stime,time1,time2,tim2
      equivalence (idata(msamp),rdata(msamp))
c---------------------------------------------------------------------
c variablendeklaration vuer bbfk
      complex*8 clxbuf(i2)
      real*4 wrkbuf(i2),start(i1),stop(i1),delta(i1),
     *       frqlow,frqhig,loref,laref,elref,slwmax
      integer*4 typctl,grdpts
      character instr*20,errmsg*70
c---------------------------------------------------------------------
c Lokale Variablendeklaration
      real*4 s(i2),dt,s1,s2,s3,tanf,tend,se,
     *       sti(mstat),stevla(mstat),stevlo(mstat),stevel(mstat),
     *       stdi(mstat),staz(mstat)
      integer*4 tle,idoy,sttrace(mstat),
     *       idd,nn,lx,iy,im,ida,ido,ih,imi,iss(5),
     *       chnptr(i1),chnpts(i1)
      real*8 ntime
      character*12 kname,knameo,sta(mstat),staref
      character c1*1,c2*10,ident*70,staa*8,ca*4,file*70,
     *       strinh*20,cid*10
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
     *     file='/home/b3sn04/nico/SEIS/opbeam/datout/bbfk.test',
     *     status='unknown',iostat=ierr)
       if (ierr.gt.0) then
          write(luo,'("Fehler beim oeffnen des files bbfk.test!")')
          goto 999
       end if
c ----------------------------------------------------------------------------
c Einlesen der Parameter und Stationsnamen 
      open (unit=20,
     *      file='/home/b3sn04/nico/SEIS/opbeam/bin/bbfk-in',
     *      status='old',iostat=ierr)
      if (ierr.gt.o) then
         write(luo,'("Fehler beim oeffnen des files bbfk-in!")')
         goto 999
      end if
c
        read(20,'(a)') ident
        read(20,*) tanf,tend,tdt,tl
        read(20,*) frqlow,frqhig
        read(20,*) fiu,fio
        read(20,*) slwmax
        read(20,*) grdpts
        read(20,*) alpha,beta,theta
        read(20,*) typctl
        read(20,'(a)') staref
        read(20,'(a)') cid
        read(20,*) iss(1),iss(2)
c
      do 79 i=1,mstat
        read (20,'(a1,1x,a10,3f12.3)',end=78)
     *             c1,c2,s1,s2,s3
        if (c1.ne.'-') then
           nn=nn+1
           sta(nn)=c2
           stevlo(nn)=s1
           stevla(nn)=s2
           stevel(nn)=s3
           sti(nn)=0.
        end if
        if (sta(nn).eq.staref) then
           loref=stevlo(nn)
           laref=stevla(nn)
           elref=stevel(nn)
        end if
79    continue
78    close (unit=20)
c-------------------------------------------------------------------------------
c einlesen des maximalen Zeitfensters und der sttraces aus dem .wfdisc file
c get prefix from environment variable  WFDPREFIX
      call getenv('WFDPATH',wfdpath)
      call getenv('WFDPREFIX',strinh)
      if (strinh(1:1) .eq. ' ') then
        write(luo,'("program bbfk, please give prefix for",
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
      open(unit=60,file=filewfd,err=61)
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
        stop    'bbfk'
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
      lx=0
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
            chnptr(j)=lx+1
            if (int.eq.0) then
              do 8 i=ianf,iend,1    
                lx=lx+1
                s(lx)=float(idata(i))*s1
8             continue
            else if (int.eq.1) then
              do 9 i=ianf,iend,1
                lx=lx+1
                s(lx)=rdata(i)*s1
9             continue
            end if
            chnpts(j)=lx-chnptr(j)+1
          end if
3       continue
77    continue
c
c ----------------------------------------------------------------------------
c normierung der spuren
      if (iss(1).eq.1) then
        write (*,'(/,"Normierung der Spuren!")')
        do 18 i=1,nn
          call norms (s,chnptr(i),chnpts(i))
18      continue
      end if
c ----------------------------------------------------------------------------
c rausschreiben der daten auf file: azvel'cid'.in
      file='/home/b3sn04/nico/SEIS/opbeam/datout/azvel'//
     *      cid(1:tle(cid))//'.in'
      open (unit=50,file=file,status='unknown',iostat=ierr)
      if (ierr.ne.0) then
         write (*,'("Fehler beim oeffnen des files !")')
         goto 999
      end if
      write (50,'(a4)') cid
      write (50,'("100.  140.   4  1   aaw,aew,inta,intza      ",
     *       "   Azimut")')
      write (50,'("  2.   12.   5  2   vaw,vew,intv,intzv      ",
     *       "   Geschwindigkeit")')
      write (50,'("  0.    1.   5  2   paw,pew,intp,intzp      ",
     *       "   Power")')
      write (50,'("  0.  100.  10  2   taw,tew,intt,intzt      ",
     *       "   Zeit")')
      write (50,'("100.  200.  10 10   azwa,azwe,intaw,intzaw  ",
     *       "   Azimut-Verteilung x-Achse")')
      write (50,'("  0.  1.0   10  2   ava,ave,intav,intzav    ",
     *       "   Azimut-Verteilung y-Achse")')
      write (50,'("15.  0. 1 1         daz,mw,is(7),is(8)      ",
     *       "   Mittelwert,Streuung")')
      write (50,'("0.02 2. 40.         sn,azda,azd             ",
     *       "   Azimutverteilung")')
      write (50,'("10 20 0 3 2 2 1     m,pts,is(1)...is(5)     ",
     *       "   Kurve Geschwindigkeit")')
      write (50,'("0.00 4              sw,swq                  ",
     *       "   schwellwerte power,quality")')
      write (50,'("1                   nt                      ",
     *       "   markierte Zeiten")')
      write (50,'("      0.00 ")')
      write (50,'("                    comment                 ",
     *       "   Kommentar")')
      du =0.
      dd=iz*2.*dt
      write (50,'(i4,"-",i3,":",i2,".",i2,".",f6.3,x,a3,5x,
     *       "(f,k analysis)")') iy,ido,ih,imi,se,cid
      write (50,'(2f8.3)') fiu,fio
      write (50,'(2f8.3)') frqlow,frqhig
      write (50,'(3f8.3)') tl,tl,tdt
c ----------------------------------------------------------------------------
c belegung der variablen fuer f,k-berechnung
   
       do 20 i=1,nn
         delta(i)=dt
         stevlo(i)=stevlo(i)-loref
         stevla(i)=stevla(i)-laref
         stevel(i)=stevel(i)-elref
         start(i)=0.
         stop(i)=start(i)+tl
         ta=start(i)+tanf
         te=stop(i)+tanf
20     continue
       instr='VERTICAL'
c ----------------------------------------------------------------------------
c berechnung des f,k-spektrums
21    if (te.lt.tend) then
        call slbbfk ( s, chnptr, chnpts, delta , start,
     +                stop  , sta , instr , frqlow, frqhig,
     +                stevlo, stevla, stevel, slwmax, nn    ,
     +                alpha , beta  , theta , grdpts, wrkbuf,
     +                i2    , clxbuf, i2    , APPVEL, AZIMUT,
     +                SXMAX , SYMAX , OUTLOW, OUTHIG, POWER ,
     +                ABSPOW, IFKQ  , errmsg, typctl)
c ausgabe der ergebnisse
        if (iss(2).ne.0) then
          write (*,*) errmsg
          write (*,*)
          write (*,*) 'apparent velocity  (appvel  : ',appvel
          write (*,*) 'azimuth            (azimut  : ',azimut
          write (*,*) 'rel. power (coh.)  (power)  : ',power
          write (*,*) 'abs. power         (abspow  : ',abspow
          write (*,*) 'quality measure    (ifkq)   : ',ifkq
          write (*,*) 'time window                 : ',tanf,tend
          write (*,*)
          write (*,*) 'incidence          (theta)  : ',theta
          write (*,*) 'p-wave-velocity    (alpha)  : ',alpha
          write (*,*) 's-wave-velocity    (beta)   : ',beta
          write (*,*) 'maximum slowness   (slwmax) : ',slwmax
          write (*,*) 'slowness-points    (grdpts) : ',grdpts
          write (*,*) '                   (sxmax)  : ',sxmax
          write (*,*) '                   (symax)  : ',symax
          write (*,*) 'frequency in       (frqlow) : ',frqlow
          write (*,*) '                   (frqhig) : ',frqhig
          write (*,*) 'frequency out      (outlow) : ',outlow
          write (*,*) '                   (outhig) : ',outhig
          write (*,*) 'number of channnel (nn)     : ',nn
        end if
        write (50,'(f5.2,f7.2,f6.3,i2,2f7.2)') 
     *         appvel,azimut,power,ifkq,ta,te
        do 22 i=1,nn
          start(i)=start(i)+tdt
          stop(i)=start(i)+tl
          ta=start(i)+tanf
          te=stop(i)+tanf
22      continue
        goto 21
      end if
c ----------------------------------------------------------------------------
c ausgabe des f,k-spektrums
      file='/home/b3sn04/da/css/obdata/'
     *     //'obspu'//cid(1:tle(cid))//'.wfdisc'
      open (unit=30,file=file,status='unknown',iostat=ierr)
      if (ierr.ne.0) then
         write (*,'("Fehler beim oeffnen des wfdisc-files !")')
         goto 999
      end if
c
      close (unit=30)
      close (unit=50)
c ----------------------------------------------------------------------------
      close (unit=80)
c
999   stop    'bbfk'
      end
c ----------------------------------------------------------------------------

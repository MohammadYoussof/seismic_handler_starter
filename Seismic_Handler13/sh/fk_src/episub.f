C     PROGRAMM : episub.f (subroutine)
C
C         PROGRAMM COMPUTES :   - EPICENTRAL DISTANCES
C                               - BACKAZIMUTS
C         PROGRAMM PRODUCES     - OUTPUT on SCREEN
C
C
      subroutine episub (sstnp,sstnl,sep,sel,sangi,srange,saz)
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      real sstnp,sstnl,sep,sel,sangi,srange,saz
c
c GERESS Koordinaten:
c      DATA EP,EL/48.84510611D0,13.70155917D0/
      DATA PI/3.14159265358979D0/
C
C        EP   :  GEOGRAPHISCHE BREITE  (NORD : +)
C        EL   :  GEOGRAPHISCHE LAENGE  (OST  : +)
C
      LUO=6
      LUI=5
c
      angi=0.0d0
      range=0.0d0
      az=0.0d0
c
      stnp=dble(sstnp)
      stnl=dble(sstnl)
      ep=dble(sep)
      el=dble(sel)
c      sep=real(ep)
c      sel=real(el)
C 
      if(stnp.gt.90. .or. stnp.lt.-90.) stop 'error 1'
      if(stnl.gt.180. .or. stnl.lt.-180.) stop 'error 2'
C
      DGSPR=180.d0/PI
      RAD=PI/180.d0
      DEGREE=RAD*6371.028d0
C
C
C
C
C **** MAKE ESQ1=0.993215 IF ELLIPSOID IS USED INSTEAD OF SPHERE
C           ESQ1=1.0 FOR SPHERICAL EARTH
C      
C      LAENGENKOORDINATEN: STLC, EPILC
C      WERTEBEREICH LAENGENKOORDINATEN:
C        GEOGRAPHISCH:    -180 --> 0 --> +180
C        KUGELKOORD. :             0 --> +180 --> +360
C           (NEG. GEOGR. KOORDINATEN = POS. KUGELKOORD. > 180)
C
C      BREITENKOORDINATEN: STPC, EPIPC
C      WERTEBEREICH BREITENKOORDINATEN:
C        GEOGRAPHISCH:     -90 -->   0 --> +90
C        KUGEKKOORD. :    +180 --> +90 -->   0
C           (GEMAESS ELLIPTISCHER KORREKTURFORMEL  PR=90*RAD-ATAN(...  )
C        NORDPOL: 0 GRAD     SUEDPOL: 180 GRAD
C
C      E180 IST ELLIPTISCH KORRIGIERTE BREITE DES SUEDPOLS, DIE SPAETER
C      FUER IF-ABFRAEGN BENOETIGT WIRD. E180=180, ENTHAELT JEDOCH DIE 
C      UNGENAUIGKEIT (~2.E-6) DER ELLIPTISCHEN KORREKTURFORMEL.


C
C
C
C
      ESQ1=(1.d0-1.d0/298.25d0)**2.d0
      EPIPR=EP*RAD
c      esq1=1.d0
      EPIPR=90.d0*RAD-dble(ATAN(ESQ1*dtan(EPIPR)))
      EPIPC=EPIPR/RAD
C   WENN EL NEGATIV, EPILC=360.d0+EL
        IF (EL.LT.0.0) THEN
           EPILC=360.d0+EL
        ELSE
           EPILC=EL
        END IF
      EPILR=EPILC*RAD
C   FUER SPAETERE ABFRAGE, OB POLBREITE
      R90=90.d0*RAD
      E180=R90-dble(ATAN(ESQ1*dtan(-R90)))
      E180=E180/RAD
C
C
C **** BERECHNUNG
C
      if(dabs(stnp).eq.90.d0.and.stnl.ne.0.d0) stnl=0.d0
      IF (EP.EQ.stnp.AND.EL.EQ.stnl) GOTO 111
      STPC=stnp*RAD
      STNPR=90.*RAD-dble(ATAN(ESQ1*dtan(STPC)))
      STPC=STNPR/RAD
C   WENN STNL NEGATIV, STLC=360+STNL
           IF (stnl.LT.0.) THEN
              STLC=360.+stnl
           ELSE
              STLC=stnl
           END IF

           IF (STPC.EQ.E180) THEN
              angi=E180-EPIPC
              az=180.
              baz=EL
              GOTO 998
           END IF

           if(stpc.eq.0.) then
              angi=epipc
              az=180.-el
              baz=0.
              go to 998
           endif

           if(epipc.eq.0.) then
              angi=stpc
              baz=0.
              az=180.-stnl
              go to 998
           endif

           if(epipc.eq.e180) then
              angi=e180-stpc
              az=stlc
              baz=180.
              go to 998
           endif


C   CONVERT STA-COORDINATES TO RADIANS
      STNLR=STLC*RAD

C   DETERMINE POLAR ANGLE
      PANG=ABS(STLC-EPILC)
C   WENN PANG GROESSER 180
      IF (PANG.GT.180.) PANG=360.-PANG
      PANG=PANG*RAD
      angi=DCOS(EPIPR)*DCOS(STNPR)+DSIN(EPIPR)*DSIN(STNPR)*DCOS(PANG)
      SNANG=DSQRT(DABS(1.-angi**2))
      angi=dble(ATAN(SNANG/angi))
C   ANGI NEGATIV?
      IF (angi.LT.0.) angi=angi+PI
      az=(DCOS(STNPR)-DCOS(EPIPR)*DCOS(angi))/(DSIN(EPIPR)
     *      *DSIN(angi))
      SNAZ=DSQRT(DABS(1.-az**2))
      az=dble(ATAN(SNAZ/az))
C   AZ NEGATIV ?
      IF (az.LT.0.) az=az+PI
      az=DGSPR*az
      baz=(DCOS(EPIPR)-DCOS(STNPR)*DCOS(angi))/(DSIN(STNPR)
     *       *DSIN(angi))
      SNBAZ=DSQRT(DABS(1.-baz**2))
      baz=dble(ATAN(SNBAZ/baz))
C   BAZ NEGATIV ?
      IF (baz.LT.0.) baz=baz+PI
      baz=baz*DGSPR
      angi=angi*DGSPR

C   ADJUST AZIMUTH AND BACKAZIMUTH
      IF (STLC-EPILC) 72,74,73
 72   IF (STLC-EPILC+180.) 80,77,81
 73   IF (STLC-EPILC-180.) 80,77,81
 74   IF (STPC-EPIPC) 75,78,76

 75   az=0.
      baz=180.
      GOTO 998
 76   az=180.
      baz=0.
      GOTO 998
 77   IF (STPC+EPIPC-E180) 78,78,79
 78   az=0.
      baz=0.
      GOTO 998
 79   az=180.
      baz=180.
      GOTO 998
 80   baz=360.-baz
      GOTO 998
 81   az=360.-az
 
C   CALCULATE DISTANCE IN KILOMETER
 998  range=angi*DEGREE
C
111   sangi=real(angi)
      srange=real(range)
      saz=real(az)
c
      return
      end

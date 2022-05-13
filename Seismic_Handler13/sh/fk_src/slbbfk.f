      SUBROUTINE SLBBFK ( CHNDAT, CHNPTR, CHNPTS, DELTA , START ,
     +                    STOP  , IDENT , INSTR , FRQLOW, FRQHIG,
     +                    XCOORD, YCOORD, ZCOORD, SLWMAX, NCHAN ,
     +                    ALPHA , BETA  , THETA , GRDPTS, WRKBUF,
     +                    WRKDIM, CLXBUF, CLXDIM, APPVEL, AZIMUT,
     +                    SXMAX , SYMAX , OUTLOW, OUTHIG, POWER ,
     +                    ABSPOW, IFKQ  , ERRMSG, TYPCTL)
 
      COMPLEX*8     CLXBUF(*)
      REAL*4        CHNDAT(*), DELTA(*) , START(*) , STOP(*)  , FRQLOW
      REAL*4        FRQHIG   , SLWMAX   , ALPHA    , BETA     , THETA
      REAL*4        WRKBUF(*), APPVEL   , AZIMUT   , SXMAX    , SYMAX
      REAL*4        XCOORD(*), YCOORD(*), ZCOORD(*), OUTLOW   , OUTHIG
      REAL*4        POWER    , ABSPOW
      INTEGER*4     CHNPTR(*), CHNPTS(*), NCHAN    , GRDPTS   , WRKDIM
      INTEGER*4     CLXDIM   , IFKQ     , TYPCTL
      CHARACTER*(*) IDENT(*) , INSTR  !  , ERRMSG
	character*80 errmsg
 
C=======================================================================
C     PURPOSE:
C            To calculate wide-band slowness estimates
C
C
C-----------------------------------------------------------------------
C     INPUT:
C     -----
C
C     Parameters:
C
C     CHNDAT - Array of data
C
C     CHNPTR - Pointer of start of each channel in CHNDAT
C
C     CHNPTS - points for each channel
C
C     DELTA  - Sampling interval for each channel
C
C     START  - Start of analysis interval (seconds), if analysis
C              window .eq. the full data window => START = 0.0
C
C     STOP   - Stop of analysis interval (seconds), if analysis
C              window .eq. the full data window => STOP = CHNPTS*DELTA
C
C     IDENT  - Channel identification (A0Z, A0N, A0E, C1Z, C2Z,...)
C
C     INSTR  - If 'VERTICAL', then only vertical sensors processed,
C
C              if '3COMP', three-comp. processing of P-waves.
C
C     FRQLOW - Lower frequency limit
C
C     FRQHIG - Higher frequency limit
C
C     XCOORD - Relative X-coordinate (EW) of channel in meters
C
C     YCOORD - Relative Y-coordinate (NS) of channel in meters
C
C     ZCOORD - Elevation of channel in meters
C
C     SLWMAX - Maximum slowness in search
C
C     NCHAN  - channels to process
C
C     ALPHA  - P-wave velocity, dummy for INSTR .EQ. 'VERTICAL'
C
C     BETA   - S-wave velocity, dummy for INSTR .EQ. 'VERTICAL'
C
C     THETA  - Angle of incidence for elevation correction, if THETA
C
C              is set to 90.0 ==> no elevation correction
C
C     GRDPTS - points in one slowness search row.
C
C     WRKDIM - dimension of WRKBUF, should be .GE. GRDPTS**2 + GRDPTS/2
C
C     CLXBUF - complex work array for storing fourier transforms
C
C     CLXDIM - dimension of CLXBUF, should be .GT. (NFFT/2 + 1)*NCHAN
C
C     TYPCTL - Verbosity level,i.e. 0=no printout(terminal),1,2,3      
C              more details.
C
C     OUTPUT:
C     ------
C
C     Parameters:
C
C     WRKBUF - array of power estimates in slowness space, stored in
C
C              standard lexicographical order
C
C     APPVEL - Apparent velocity of power maximum, interpolated.
C
C     AZIMUT - Azimuth of power maximum, interpolated
C
C     SXMAX  - Horizontal slowness Sx of power maximum, interpolated
C
C     SYMAX  - Horizontal slowness Sy of power maximum, interpolated
C
C     OUTLOW - Corrected lower frequency
C
C     OUTHIG - Corrected higher frequency
C
C     POWER  - Normalized power maximum
C
C     ABSPOW - Power maximum in dB, comparable to narrow-band F-K
C
C     IFKQ   - F-K quality measure (see FKQUAL FORTRAN)
C
C     ERRMSG - Error message
C
C
C-----------------------------------------------------------------------
C     COMMENT:
C
C
C-----------------------------------------------------------------------
C     PROGRAMMER:
C
C     Tormod Kv{rna                        - December  1, 1986
C
C     NTNF/NORSAR
C     Pb. 51
C     N-2007 Kjeller
C
C     LAST MODIFIED:                       - June      6, 1988
C
C     June 6,1988: Implementation of quality measure
C     Nov   ,1988: Trying to increase performace. Jan Fyen
C
C=======================================================================
 
C----
C   Internal declarations
C----
 
      PARAMETER ( NDIM = 45 )
 
      COMPLEX*8 CSPEC        , CTEMP       , CARR(NDIM), QDIR(3)
      REAL*8    DOTPOW       , DTEMP       , DTEMP1
      REAL*8    DBLTMP
      REAL*4    XX(NDIM)     , YY(NDIM)    , ZZ(NDIM), XREF, YREF, ZREF
      REAL*4    FARR (NDIM)  , WC(2)
      INTEGER*4 INTPTR (NDIM), INTPTS(NDIM), CFTPTR(NDIM)
      INTEGER*4 CFTPTS (NDIM), IERR        , LIMIT        , CENTER
      LOGICAL   INHOM
      character*1 comp
 
C---
C JF set default output values for errors in fk-results.
C---
      SYMAX  = 0.0
      SXMAX  = 0.0
      AZIMUT = 0.0
      APPVEL = 999.8
      POWER  = 0.01
      ABSPOW = 0.01
      OUTLOW = FRQLOW
      OUTHIG = FRQHIG
      IFKQ   = 5
C---
      ERRMSG = ' '
      WC(1)  = 0.0
      WC(2)  = 0.0
      PI     = 3.14159265
      TWOPI  = 2.0*PI
      LIMIT  = GRDPTS / 2
      CENTER = LIMIT + 1
      DELTAS = -SLWMAX / FLOAT (LIMIT)
      THETEN = THETA * PI / 180.0
CJF
      COSTHE = COS(THETEN)
      SINTHE = SIN(THETEN)
      IF( SINTHE .EQ. 0.0  ) THEN
          WRITE(ERRMSG,'(2X,''***> SLBBFK Sin(THETA)  '',2F10.3)')
     +                        SINTHE,THETA
          RETURN
      END IF
      TANTHE = COSTHE/SINTHE
CJF
 
      IF( TYPCTL.GT.0 ) THEN
          WRITE(6,'(2X,''---> SLBBFK Clear WRKBUF  '',I6)') WRKDIM
          WRITE(6,'(7X,''Points,center,deltas'',2I5,E14.6)')
     +              GRDPTS,CENTER,DELTAS
      END IF
      IF( FRQLOW.LE.0.0.OR.
     +    FRQHIG.LE.0.0.OR.
     +    FRQHIG.LE.FRQLOW ) THEN
          WRITE(ERRMSG,'(2X,''***> SLBBFK Wrong freq  '',2F10.3)')
     +                        FRQLOW,FRQHIG
          RETURN
      END IF
 
      CALL CHZERO ( WRKBUF, WRKDIM )
 
C----
C   Prepare input to fourier transform calculation
C----
 
      CALL SLSUBI ( NCHAN , CHNPTR, NCHAN , CHNPTS, NCHAN , START ,
     +              NCHAN , STOP  , NCHAN , NDIM  , INTPTR, INTPTS,
     +              DELTA , ERRMSG, IERR  , TYPCTL )
 
      IF ( ERRMSG .NE. ' ' ) THEN
 
         RETURN
 
      ENDIF
 
C----
C   Fourier transform calculation
C----
 
      CALL SLFOUT ( CHNDAT, NCHAN , INTPTR, NDIM  , INTPTS, NDIM  ,
     +              WRKBUF, WRKDIM, CLXBUF, CLXDIM, CFTPTR, NDIM  ,
     +              CFTPTS, NDIM  , ERRMSG, IERR  , TYPCTL )
 
      IF ( ERRMSG .NE. ' ' ) THEN
 
         RETURN
 
      ENDIF
 
      IF( TYPCTL.GT.0 ) THEN
          WRITE(6,'(2X,''---> SLBBFK Clear WRKBUF'',I6)') WRKDIM
      END IF
      CALL CHZERO ( WRKBUF(1), WRKDIM )
 
C----
C   Calculate centroid of channels
C----
 
      CALL SLCENT ( XCOORD, YCOORD, ZCOORD, XREF, YREF, ZREF, NCHAN )
 
C----
C   Store corrected coordinates in tables
C----
 
      DO 1000 ICHAN = 1, NCHAN
 
         XX(ICHAN) = ( XCOORD(ICHAN) - XREF ) / 1000.0
         YY(ICHAN) = ( YCOORD(ICHAN) - YREF ) / 1000.0
         ZZ(ICHAN) = ( ZCOORD(ICHAN) - ZREF ) / 1000.0
 
 1000 CONTINUE
 
C----
C   Calculate start and stop points of FFT from low and high
C   frequency bounds, assuming all channels to have same sampling rate
C----
 
      XNFFT   = 2.0*FLOAT(CFTPTS(1) - 1)
      K1      = IFIX ( (DELTA(1) * FRQLOW * XNFFT ) + 1.001)
      K2      = IFIX ( (DELTA(1) * FRQHIG * XNFFT ) + 1.001)
 
      THLP    = TWOPI / ( ( XNFFT ) * DELTA(1) )
 
      IF ( K1 .LT. 1 ) THEN
 
         K1 = 1
 
      ENDIF
 
      IF ( K2 .GT. CFTPTS(1) ) THEN
 
         K2 = CFTPTS(1)
 
      ENDIF
 
      OUTLOW  = FLOAT (K1 - 1) * THLP / TWOPI
 
      OUTHIG  = FLOAT (K2 - 1) * THLP / TWOPI
      IF( TYPCTL.GT.0 ) THEN
          WRITE(6,'(2X,''...> SLBBFK K1,K2'',2I5,F7.1,F9.3)')
     +                        K1,K2,XNFFT,THLP
          WRITE(6,'(2X,''...> SLBBFK Outlow,high'',2F9.3)')
     +                        OUTLOW,OUTHIG
      END IF
 
      DOTPOW = 0.0D0
 
      DO 1200 IFRQ = K1, K2
 
         IFRQ1 = IFRQ - 1
         DO 1100 ICHAN = 1, NCHAN
 
            IND    = CFTPTR(ICHAN) + IFRQ1
            DBLTMP = CABS( CLXBUF(IND) )
            DOTPOW = DOTPOW + DBLTMP*DBLTMP
 
 1100    CONTINUE
 
 1200 CONTINUE
 
C----
C   Scaling factor
C----
 
      DTEMP1 = ( DFLOAT (NCHAN) * DOTPOW )
      IF( TYPCTL.GT.0 ) THEN
          WRITE(6,'(2X,''...> SLBBFK Scale DTEMP1,DOTPOW,N'',
     +              2E14.6,I4)')
     +              DTEMP1,DOTPOW,NCHAN
      END IF
C---
C  JF : for bad data, DOTPOW and DTEMP1 may turn zero.
C---
      IF( DTEMP1.LE.0.0.OR.DOTPOW.LE.0.0 ) THEN
          ERRMSG = '***> SLBBFK zero dot power '
 
          RETURN
 
      END IF
C----
C   Now, do the calculation for vertical sensors
C----
 
      IF ( INSTR (1:8) .EQ. 'VERTICAL' ) THEN
 
C--- Loop over Sy
         DO 3000 I = -LIMIT, LIMIT
            YSY = FLOAT(I) * DELTAS
            YS2 = YSY*YSY
c
C--- Loop over Sx
            DO 2900 J = -LIMIT, LIMIT
               XSX = FLOAT(J) * DELTAS
C--- Time element of phase lag (with or without elevation corrections)
               VINV = SQRT ( YS2 + (XSX*XSX) )
               TEMP = VINV*TANTHE

C--- Store coordinate phase contribution in tables
               DO 2100 JK = 1, NCHAN
                  FARR(JK)=((XSX*XX(JK))+(YSY*YY(JK)))+TEMP*ZZ(JK)
 2100          CONTINUE

C   Loop over angular frequency
               DTEMP = 0.0D0
               DO 2300 K = K1, K2
                  OMEGA = THLP * FLOAT (K - 1)

C   Loop over channels
                  CTEMP = CMPLX (0.0, 0.0)
                  DO 2200 L = 1, NCHAN
                     ARG   = OMEGA * FARR(L)
                     CSPEC = CMPLX ( COS(ARG), SIN(ARG) )
                     IND   = CFTPTR(L) + K - 1
                     CTEMP = CTEMP + CLXBUF(IND) * CSPEC
2200              CONTINUE
                  DBLTMP = CABS (CTEMP)
                  DTEMP  = DTEMP + DBLTMP*DBLTMP
2300           CONTINUE

C   And now, store the final value
               I1 = J + CENTER + ( I + CENTER - 1 ) * GRDPTS
               WRKBUF (I1) = DTEMP / DTEMP1
2900        CONTINUE
3000     CONTINUE
 
C----
C   Three-component method comes here
C----
 
      ELSE IF ( INSTR(1:5) .EQ. '3COMP') THEN
 
C----
C   Loop over Sy
C----
 
         DO 5000 I = -LIMIT, LIMIT
 
            YSY = FLOAT(I) * DELTAS
 
C----
C   Loop over Sx
C----
 
            DO 4900 J = -LIMIT, LIMIT
 
               XSX = FLOAT(J) * DELTAS
 
C----
C   Call direction vector estimate for this slowness
C----
 
               CALL SLDIR3 ( XSX, YSY, ALPHA, BETA, QDIR, INHOM )
 
               IF ( INHOM ) THEN
 
                  TEMP4 = 0.0
 
                  GO TO 4700
 
               ENDIF
 
C----
C   Time element of phase lag (with or without elevation corrections)
C----
 
               VINV = SQRT ( (YSY**2) + (XSX**2) )
 
CJF            TEMP = VINV*COS(THETEN) / SIN(THETEN)
               TEMP = VINV*TANTHE
 
C----
C   Store direction vector in tables
C----
 
               GSCAL = 0.0
 
               DO 4100 JK = 1, NCHAN
 
                  NC    = IC0LEN(IDENT(JK))
                  JCOMP = 0
                  COMP  = IDENT(JK)(NC:NC)
C---
C Choose what component from last character of name being 'z','n','e'.
C---
                  IF( COMP.EQ.'z' ) THEN
                      JCOMP = 1
                  ELSE IF( COMP.EQ.'Z' ) THEN
                      JCOMP = 1
                  ELSE IF( COMP.EQ.'n' ) THEN
                      JCOMP = 2
                  ELSE IF( COMP.EQ.'N' ) THEN
                      JCOMP = 2
                  ELSE IF( COMP.EQ.'e' ) THEN
                      JCOMP = 3
                  ELSE IF( COMP.EQ.'E' ) THEN
                      JCOMP = 3
                  END IF


                  IF ( JCOMP.EQ.1 ) THEN
 
                     CARR(JK) = QDIR(1)
                     GSCAL    = GSCAL + CABS ( (QDIR(1) )**2 )

                  ELSE IF ( JCOMP.EQ.2 ) THEN
 
                     CARR(JK) = QDIR(2)
                     GSCAL    = GSCAL + CABS ( (QDIR(2) )**2 )
 
                  ELSE IF ( JCOMP.EQ.3 ) THEN
 
                     CARR(JK) = QDIR(3)
                     GSCAL    = GSCAL + CABS ( (QDIR(3) )**2 )
 
                  ELSE
 
                     ERRMSG = '***> SLBBFK (3-comp): Illegal channel '//
     +                        'id '
                     ERRMSG(43:50) = IDENT(JK)(1:8)
 
                     RETURN
 
                  ENDIF
 
C----
C   Store coordinate phase contribution in tables
C----
 
                  FARR (JK) = ( (XSX * XX (JK) ) + (YSY * YY (JK) ) ) +
     +                        TEMP * ZZ(JK)
 
4100           CONTINUE
 
C----
C   Loop over angular frequency
C----
 
               DTEMP = 0.0D0
 
               DO 4300 K = K1, K2
 
                  OMEGA = THLP * FLOAT (K - 1)
 
C----
C   Loop over channels
C----
 
                  CTEMP = CMPLX (0.0, 0.0)
 
                  DO 4200 L = 1, NCHAN
 
                     ARG   = OMEGA * FARR(L)
 
                     CSPEC = CMPLX ( COS(ARG), SIN(ARG) )
 
                     IND   = CFTPTR(L) + K - 1
 
                     CTEMP = CTEMP + CLXBUF(IND) * CSPEC*
     +                       CONJG ( CARR(L) )
 
4200              CONTINUE
 
                  DTEMP = DTEMP + DBLE ( CABS (CTEMP)**2 )
 
4300           CONTINUE
 
C----
C   And now, store the final value
C----
               TEMP4 = SNGL (DTEMP / DOTPOW) / GSCAL
 
4700           CONTINUE
 
               I1 = J + CENTER + ( I + CENTER - 1 ) * GRDPTS
 
               WRKBUF (I1) = TEMP4
 
4900        CONTINUE
 
5000     CONTINUE
 
         CONTINUE
 
      ENDIF
 
      IF( TYPCTL.GT.0 ) THEN
          WRITE(6,'(2X,''...> SLBBFK That took some time ...   '')')
      END IF
 
C----
C   Measure maximum of slowness matrix
C----
 
      CALL SLFKMS ( WRKBUF, GRDPTS, SLWMAX, WC, 1.0, SYMAX, SXMAX,
     +              AZIMUT, APPVEL, POWER )
 
C----
C   Calculate absolute power, analogue to narrow-band F-K
C----
 
      IF ( INSTR (1:8) .EQ. 'VERTICAL' ) THEN
 
         ABSPOW = ( POWER *DTEMP1 ) / ( FLOAT (K2-K1+1) )
         ABSPOW = ABSPOW / ( FLOAT (NCHAN*NCHAN) )
         ABSPOW = ABSPOW * DELTA(1) / ( FLOAT (INTPTS(1) ) )
 
      ELSE IF ( INSTR (1:5) .EQ. '3COMP' ) THEN
 
         ABSPOW = ( POWER *DOTPOW ) / ( ( FLOAT (K2-K1+1) ) * GSCAL )
         ABSPOW = ABSPOW / ( FLOAT (NCHAN*NCHAN) )
         ABSPOW = ABSPOW * DELTA(1) / ( FLOAT (INTPTS(1) ) )
 
      ENDIF
 
      IF ( ABSPOW .GT. 0.0 ) THEN
         ABSPOW = 10.0 * ALOG10 (ABSPOW)
      ELSE
         ABSPOW = 0.0
      ENDIF
 
C----
C   Calculate F-K quality measure
C----
 
      IPOINT = (GRDPTS*GRDPTS) + 1
 
      XDB = 7.0
 
      CALL SLFKQA (WRKBUF, GRDPTS, POWER, XDB, WRKBUF(IPOINT), IFKQ )
 
C----
C   That's it|
C----
 
      RETURN
 
      END

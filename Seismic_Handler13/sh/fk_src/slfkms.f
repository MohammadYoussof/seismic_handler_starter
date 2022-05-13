C.@PROCESS OPT(3)
C.OPT(3)
C-----------------------------------------------------------------------
C
C                                                          SLFKMS
C
C  SLFKMS -- Code to find spectral FK maximum, then calculate
C            apparent velocity and bearing from its location.
C
C  Author:  Dave Harris
C           L-205
C           Lawrence Livermore National Laboratory
C           Livermore, CA  94550
C
C  Last Modified:  February 15, 1985 (NORSAR)
C  Last Modified:  November   , 1988 (Jan Fyen,performance)
C
C  Input Arguments:
C  ----- ----------
C
C    FKS               REAL*4 array containing estimated FK spectrum
C
C    SAMPLE_SIZE       INTEGER variable containing  samples along
C                      each axis of FK spectrum.
C
C    WAVENUMBER        REAL*4 variable containing maximum wavenumber
C                      along each axis, in cycles/kilometer.
C
C
C    WINDOW_CENTER     REAL*4 array containing the center of the FK
C                      window that is passed.  XY coordinates in
C                      wavenumber.
C
C    FREQUENCY         REAL*4 variable containing analysis frequency.
C
C
C  Output Arguments:
C  ------ ----------
C
C    KN_MAX            REAL*4 variable containing north wavenumber
C                      of spectral maximum.
C
C    KE_MAX            REAL*4 variable containing east wavenumber
C                      of spectral maximum.
C
C    AZIMUTH           REAL*4 variable containing azimuth of event
C
C    V_APPARENT        REAL*4 variable containing apparent velocity
C
C    SPECTRAL_MAX      REAL*4 variable containing spectral maximum
C
C  Replacements
C----
 
 
      SUBROUTINE SLFKMS(FKS, ISMPSZ, WAVNUM, WC,
     &                  FREQ, RKNMAX, RKEMAX, AZIM,
     &                  VAP, SMAX)
 
C----
C  Declarations
C----
        DIMENSION  FKS(*), H(-1:1, -1:1)
        INTEGER CENTER
        REAL*4 WC(2)
 
C----
C  Initializations
C----
 
        PI = 3.14159265
        LIMIT = ISMPSZ/2
        CENTER = LIMIT + 1
 
C----
C  Search for spectral maximum
C
C
C    Find wavenumber of spectral maximum
C----
 
          SMAX = 0.
          IMAX=0
          JMAX=0
 
          DO   19 I= -LIMIT, LIMIT
 
            KKC = CENTER + (I + CENTER - 1) * ISMPSZ
            DO   20 J= -LIMIT, LIMIT
 
              K = J + KKC
              VALUE = FKS(K)
 
              IF (  VALUE .GE. SMAX ) THEN
 
                IMAX=I
                JMAX=J
                SMAX = VALUE
 
              END IF
 
 
   20       CONTINUE
 
 
   19     CONTINUE
 
C----
C    Refine estimate if it is in the interior
C----
 
          DELTAK = WAVNUM/FLOAT(LIMIT)
 
          IF (  IABS(IMAX) .LT. LIMIT .AND. IABS(JMAX) .LT. LIMIT ) THEN
 
            DO   21 I = -1, 1
 
              KKJ = JMAX+CENTER + (IMAX+I+CENTER-1)*ISMPSZ
 
              DO   22 J = -1, 1
 
                K = J+KKJ
                H(I,J) = FKS(K)
 
   22         CONTINUE
 
   21       CONTINUE
 
            CALL SLFIT2(H, YINC, XINC)
 
            RKEMAX = (FLOAT(JMAX) + XINC)*DELTAK + WC(1)
            RKNMAX = (FLOAT(IMAX) + YINC)*DELTAK + WC(2)
 
          ELSE
 
            RKEMAX = DELTAK*FLOAT(JMAX) + WC(1)
            RKNMAX = DELTAK*FLOAT(IMAX) + WC(2)
 
          END IF
 
C----
C    Calculate azimuth and apparent velocity of event
C----
          AZIM = ATAN2(RKEMAX, RKNMAX)/PI*180.
          IF (  AZIM .LT. 0. ) THEN
            AZIM = AZIM + 360.
          END IF
          RADIUS = SQRT(RKNMAX*RKNMAX + RKEMAX*RKEMAX)
          VAP = FREQ/RADIUS
C----
C  Bye
C----
      RETURN
      END
C
C-----------------------------------------------------------------------

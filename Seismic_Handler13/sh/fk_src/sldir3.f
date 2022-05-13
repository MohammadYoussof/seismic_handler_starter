C.@PROCESS OPT(3)
C.OPT(3)
C-----------------------------------------------------------------------
C   SUBROUTINE SLDIR3
C
C   Subroutine to calculate direction vector for P- waves on a
C   3C - station. The direction vector is scaled to unity.
C
C
C
C   Input arguments:
C   ----------------
C
C   SX     - Horizontal wavenumber in EW-direction
C
C   SY     - Horizontal wavenumber in NS-direction
C
C   ALPHA  - Surface P-wave velocity
C
C   BETA   - Surface S-wave velocity
C
C
C   Output arguments:
C   -----------------
C
C   QDIR   - Direction vectors for displacement
C
C   QDIR(1)- Direction vector for Z-comp
C
C   QDIR(2)- Direction vector for NS-comp
C
C   QDIR(3)- Direction vector for EW-comp
C
C----
 
 
 
      SUBROUTINE SLDIR3 ( SX, SY, ALPHA, BETA, QDIR, INHOM )
 
C----
C   Declaration of arguments
C----
 
      REAL*4      SX, SY, ALPHA, BETA
      COMPLEX     QDIR (3)
      LOGICAL     INHOM
 
C----
C   Declaration of internal variables
C----
 
 
      INHOM = .FALSE.
 
      IF ( (SX .LT. 0.0001) .AND. (SY .LT. 0.0001)   .AND.
     %     (SX .GT. -0.0001) .AND. (SY .GT. -0.0001) ) THEN
 
         QDIR (1) = CMPLX (1.0, 0.0)
         QDIR (2) = CMPLX (0.0, 0.0)
         QDIR (3) = CMPLX (0.0, 0.0)
 
      ELSE
 
C----
C   Calculate some terms
C----
 
         SXSY2    = SX**2 + SY**2
 
         P        = SQRT ( SXSY2 )
         PLIMIT   = SQRT ( 1.0 / (2.0 * BETA * BETA) )
 
C----
C   Ray parameter must be less than PLIMIT
C----
 
         IF ( P .LT. PLIMIT ) THEN
 
            PBETA2   = (P**2) * (BETA**2)
 
            TERM1    = (1.0 - 2.0*PBETA2)**2
 
            TERM2    = 4.0 * PBETA2 * (1.0 - PBETA2)
 
            TERM3    = 1.0 / ( SXSY2 * ( 1.0 + ( TERM1 / TERM2 ) ) )
 
            TERM4    = 1.0 / ( 1.0 + ( TERM2 / TERM1 ) )
 
            A        = SQRT (TERM3)
 
            TK1      = SQRT (TERM4)
 
            QDIR(1) = CMPLX (TK1, 0.0)
 
            TK1      = A * SY
 
            QDIR(2) = CMPLX (TK1, 0.0)
 
            TK1      = A * SX
 
            QDIR(3) = CMPLX (TK1, 0.0)
 
         ELSE
 
            QDIR(1) = CMPLX (0.0, 0.0)
 
            QDIR(2) = CMPLX (0.0, 0.0)
 
            QDIR(3) = CMPLX (0.0, 0.0)
 
            INHOM = .TRUE.
 
         ENDIF
 
      ENDIF
 
C----
C   The correct vector is now hopefully calculated
C----
 
      RETURN
 
      END

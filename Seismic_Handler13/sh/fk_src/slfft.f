C.@PROCESS OPT(3)
C.OPT(3)
C---------------------------------------------------------------------
C
C - FFT
C
C  Author:  Dave Harris
C           L-205
C           Lawrence Livermore National Laboratory
C           Livermore, CA  94550
C
C Last modified:  November 14, 1977
C Program to compute the FFT of a sequence.
C XREAL(*)         The real part of the sequence is stored in
C                  this array.
C XIMAG(*)         The imaginary part is stored here.
C TCOS(*),TSIN(*)  Cosine and sine tables.
C N                The length of the sequence.
C LOGN             The base-two logarithm of N.
C IDIR             Variable to indicate the direction of the transform.
C                  For a forward transform IDIR=-1 and for an inverse
C                  transform IDIR=1.  Normalization is performed
C                  for IDIR=1.
C The transform result is returned in XREAL and XIMAG.
C   The initial sequence is destroyed.
C----
 
      SUBROUTINE SLFFT(XREAL, XIMAG, N, IDIR)
      DIMENSION XREAL(*), XIMAG(*), TCOS(2049), TSIN(2049)
      DOUBLE PRECISION FUND, COSINE, SINE
 
      NBY2 = N/2
      LOGN = 1
    3 CONTINUE
        IF (  2**LOGN  .EQ.  N ) THEN
          GO TO    4
        ELSE
          LOGN = LOGN+1
        END IF
      GO TO    3
    4 CONTINUE
C----
C     Table generator
C----
      FUND = 3.141592653589793D0*2.0D0
      FUND = FUND/FLOAT(N)
      COSINE = DCOS(FUND)
      SINE = DSIN(FUND)
      IF (  IDIR .EQ. -1 ) THEN
        SINE = -SINE
      END IF
      TCOS(1) = 1.
      TSIN(1) = 0.
      DO    5 I = 2, NBY2
        TCOS(I) = COSINE*TCOS(I-1) - SINE*TSIN(I-1)
        TSIN(I) = COSINE*TSIN(I-1) + SINE*TCOS(I-1)
    5 CONTINUE
C----
C     Bit reverse code
C----
        NM1 = N-1
        J = 1
        DO    6 I = 1, NM1
        IF (  I .LT. J ) THEN
          TEMP = XREAL(I)
          XREAL(I) = XREAL(J)
          XREAL(J) = TEMP
          TEMP = XIMAG(I)
          XIMAG(I) = XIMAG(J)
          XIMAG(J) = TEMP
        END IF
        K = NBY2
    7   CONTINUE
        IF (.NOT.( K .LT. J )) GO TO    8
          J = J-K
          K = K/2
        GO TO    7
    8   CONTINUE
        J = J+K
    6   CONTINUE
C----
C     Indexing code
C----
        NBLOCK = N
        IREL = 1
        DO    9 NSTAGE = 1, LOGN
          IF (  NSTAGE .GT. 1 ) THEN
            IREL = IREL*2
          END IF
          I = -IREL
          NBLOCK = NBLOCK/2
          DO   10 IBLOCK = 1, NBLOCK
            I = I+IREL
            ITI = 1-NBLOCK
            DO   11 ICOUNT = 1, IREL
              I = I+1
              ITI = ITI+NBLOCK
              I1 = I+IREL
C----
C             Butterfly code
C----
              IF (  NSTAGE  .GT.  1 ) THEN
                SINE = TSIN(ITI)
                COSINE = TCOS(ITI)
                TEMP = XREAL(I1)*COSINE-XIMAG(I1)*SINE
                XIMAG(I1) = XREAL(I1)*SINE+XIMAG(I1)*COSINE
                XREAL(I1) = TEMP
              END IF
              TEMP = XREAL(I)+XREAL(I1)
              XREAL(I1) = XREAL(I)-XREAL(I1)
              XREAL(I) = TEMP
              TEMP = XIMAG(I)+XIMAG(I1)
              XIMAG(I1) = XIMAG(I)-XIMAG(I1)
              XIMAG(I) = TEMP
C
   11       CONTINUE
   10     CONTINUE
    9   CONTINUE
C----
C  If reverse transform, divide through by N
C----
        IF (  IDIR  .EQ.  1 ) THEN
          SCALE = 1./FLOAT(N)
          DO   12 I = 1, N
            XREAL(I) = XREAL(I)*SCALE
            XIMAG(I) = XIMAG(I)*SCALE
   12     CONTINUE
        END IF
C
C  BYE
C
      RETURN
      END

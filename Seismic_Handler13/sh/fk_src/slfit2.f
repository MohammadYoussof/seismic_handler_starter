C-----------------------------------------------------------------------
C
C  Calculates parabolic refinement of estimated peak location from
C  grid values.
C----
C.@PROCESS OPT(3)
      SUBROUTINE SLFIT2(H, X, Y)
 
      DIMENSION H(-1:1, -1:1), PHI(0:2, 0:2)
CJF   INTEGER*4 SLBAS(9)
CJF   DATA SLBAS/1,-1,1,1,0,0,1,1,1/
C----
C  Construct correlations
C----
 
        DO   23 I = 0, 2
          I4       = I+4
          DO   24 J = 0, 2
            J4       = J+4
            PHI(I,J) = 0.
            DO   25 M = -1, 1
              MM = M*3
              DO   26 N = -1, 1
                PHI(I,J) = PHI(I,J) + H(M,N)*SLBAS(M,I)*SLBAS(N,J)
CJF             JFI      = MM+I4
CJF             JFJ      = 3*N+J4
CJF             PHI(I,J) = PHI(I,J) + H(M,N)*SLBAS(JFI)*SLBAS(JFJ)
C.Modified for performance
CJ.Fyen       M  I   SLBAS    N  J   SLBAS         (1+M)*3+I+1=  3M+I+4
C            -1**0   1       -1**0   1        1    1
C            -1**1  -1       -1**1  -1        2    2
C            -1**2   1       -1**2   1        3    3
C             0**0   1        0**0   1        4    4
C             0**1   0        0**1   0        5    5
C             0**2   0        0**2   0        6    6
C             1**0   1        1**0   1        7    7
C             1**1   1        1**1   1        8    8
C             1**2   1        1**2   1        9    9
 
   26         CONTINUE
 
   25       CONTINUE
 
   24     CONTINUE
 
   23   CONTINUE
 
        A = PHI(2,0)/2. - PHI(0,0)/3.
        B = PHI(1,1)/4.
        C = PHI(0,2)/2. - PHI(0,0)/3.
        D = PHI(1,0)/6.
        E = PHI(0,1)/6.
 
        DENOM = B**2 - 4.*A*C
        X = (2.*C*D - B*E)/DENOM
        Y = (2.*A*E - B*D)/DENOM
 
      RETURN
      END
C-----------------------------------------------------------------------
C
C----
CCC
      REAL FUNCTION SLBAS(N,I)
   
   
        IF (  I .EQ. 0 ) THEN
   
          SLBAS = 1.
   
        ELSE
   
          SLBAS = N**I
   
        END IF
   
      RETURN
   
      END
C-----------------------------------------------------------------------

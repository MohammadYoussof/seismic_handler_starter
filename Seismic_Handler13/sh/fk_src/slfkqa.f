C.@PROCESS OPT(2)
      SUBROUTINE SLFKQA ( WRKBUF, GRDPTS, POWER , XDB, Z, IFKQ )
 
      REAL*4        WRKBUF(*), Z(*), POWER
      INTEGER*4     GRDPTS   , XDB, IFKQ
 
C=======================================================================
C     PURPOSE:
C            To calculate F-K quality measure
C     This routine searches for peaks in WRKBUF matrix.
C     It scans the lexicographically stored matrix for for local
C     maxima within XDB dB of the largest peak.
C     The program assigns a quality measure IFKQ as a function of
C     the amplitude AMP of the second highest peak,
C     according to the following criteria.
C
C     6.0 <= AMP        :  IFKQ = 1
C     4.0 <= AMP < 6.0  :  IFKQ = 2
C     1.0 <= AMP < 4.0  :  IFKQ = 3
C
C            AMP < 1.0  :  IFKQ = 3 if double peak (only one secondary
C                                             peak within 1.0 dB).
C            A double peak is characterized by the fact that the second
C            highest peak is less than 1 dB down from the maximum, and
C            the third highest peak is more than 1 dB down from the
C            maximum.  I.e., if more than two peaks within 1 dB,
C            the solution is bad.
C
C                       :  IFKQ = 4 if bad solution
C
C-----------------------------------------------------------------------
C     INPUT:
C     -----
C
C     Parameters:
C
C     WRKBUF - Array of linear power estimates
C
C     GRDPTS - ›points in one slowness search row
C
C     POWER  - Linear power of maximum peak
C
C     XDB    - Searching criterion for secondary peaks
C
C     Z      - Work array (length at least 1600 elements)
C
C     OUTPUT:
C     ------
C
C     Parameters:
C
C     IFKQ   - F-K quality measure
C
C
C
C-----------------------------------------------------------------------
C     COMMENT:
C
C
C-----------------------------------------------------------------------
C     PROGRAMMER:
C
C     Anne Henson                          - August       1986
C
C     SAIC
C     10210 Campus Point Drive
C     San Diego, CA 92121
C
C     LAST MODIFIED:
C
C     Tormod Kv{rna                        - June      6, 1988
C     NTNF/NORSAR
C     Pb. 51
C     N-2007 Kjeller
C
C
C=======================================================================
 
      ISIZ1 = GRDPTS - 1
      IFKQ  = 1
 
C----
C   Search for local maxima and store them
C----
 
      K = 0
 
      DO 1100 I = 2, ISIZ1
         DO 1000 J = 2, ISIZ1
 
            ICENT = J + (I - 1)*GRDPTS
            IF (WRKBUF(ICENT) .LE. 0.0) GO TO 1000
 
            Y = 10.0*ALOG10(POWER / WRKBUF(ICENT))
            IF (Y .GT. XDB) GO TO 1000
 
            I1    = J - 1 + (I - 2)*GRDPTS
            I2    = J +     (I - 2)*GRDPTS
            I3    = J + 1 + (I - 2)*GRDPTS
            I4    = J - 1 + (I - 1)*GRDPTS
            I5    = J + 1 + (I - 1)*GRDPTS
            I6    = J - 1 +       I*GRDPTS
            I7    = J +           I*GRDPTS
            I8    = J + 1 +       I*GRDPTS
 
            X  = WRKBUF(ICENT)
            X1 = WRKBUF(I1)
            X2 = WRKBUF(I2)
            X3 = WRKBUF(I3)
            X4 = WRKBUF(I4)
            X5 = WRKBUF(I5)
            X6 = WRKBUF(I6)
            X7 = WRKBUF(I7)
            X8 = WRKBUF(I8)
 
            IF ( (X1.LT.X).AND.(X2.LT.X).AND.(X3.LT.X).AND.
     +           (X4.LT.X).AND.(X5.LT.X).AND.(X6.LT.X).AND.
     +           (X7.LT.X).AND.(X8.LT.X) )                     THEN
 
 
               K = K + 1
               Z(K) = Y
 
            ENDIF
 
 1000    CONTINUE
 1100 CONTINUE
 
      NK = K
 
C----
C   Distinguish between single, double, and bad solutions
C----
 
      I1 = 0
 
      DO 1200 K = 1, NK
         IF ( Z(K) .LT. 1.0) I1 = I1 + 1
 1200 CONTINUE
 
      IF (I1 .EQ. 1) THEN
         ISOL = 1
      ELSE IF (I1 .EQ. 2 ) THEN
         ISOL = 2
      ELSE
         ISOL = 3
      ENDIF
 
C----
C   Assign F-K quality measure
C----
 
      IF (ISOL .EQ. 1) THEN
 
         DO 1300 K = 1, NK
 
            Y = Z(K)
 
            IF (Y .LT. 1.0) THEN
               GO TO 1300
            ELSE IF ( (Y .GE. 4.0) .AND. (Y .LT. 6.0) ) THEN
               IFKQ = MAX0(IFKQ,2)
            ELSE IF ( (Y .GE. 1.0) .AND. (Y .LT. 4.0) ) THEN
               IFKQ = 3
            ENDIF
 
 1300    CONTINUE
 
      ELSE IF (ISOL .EQ. 2) THEN
         IFKQ = 3
      ELSE
         IFKQ = 4
      ENDIF
 
      RETURN
 
      END

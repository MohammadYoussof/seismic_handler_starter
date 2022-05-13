C.@PROCESS OPT(3)
      SUBROUTINE CHZERO (A, N)
C.======================================================================
C.    PURPOSE
C     Zero one array.                                               CH<<
C.----------------------------------------------------------------------
C.    KEYWORDS  CHNDAT
C.----------------------------------------------------------------------
C.    PACKAGE  CHNxxx
C.    VISIBLE
C.    STANDARD_FORTRAN_77
C.    USE_ONLY
C.----------------------------------------------------------------------
C.    INPUT
C..   N  - Number of elements to zero in A.
C.    OUTPUT
C.----------------------------------------------------------------------
C.    PROGRAMMER    Jan Fyen
C.    CREATION_DATE 21 Apr 1988
C.    MADE_AT  NTNF/NORSAR
C     Pb. 51
C     N-2007 Kjeller
C
C.    MODIFICATION
C.    CORRECTION
C.======================================================================
C.OPT(3)
 
      INTEGER*4  N
      REAL*4     A(*)
 
      IF( N.LE.0 ) RETURN
 
      DO  100 I = 1, N
 
         A(I) = 0.0
 
100   CONTINUE
 
      RETURN
 
      END

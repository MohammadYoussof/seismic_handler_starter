C.@PROCESS OPT(3)
      SUBROUTINE CHMOVE (A, B, N)
C.OPT(3)
C.======================================================================
C.    PURPOSE
C     Make copy of an array. Move from  B to A.                     CH<<
C.----------------------------------------------------------------------
C.    KEYWORDS  CHNDAT
C.----------------------------------------------------------------------
C.    PACKAGE  CHNxxx
C.    VISIBLE
C.    STANDARD_FORTRAN_77
C.    USE_ONLY
C.----------------------------------------------------------------------
C.    INPUT
C..   N  - Number of elements to move from B to A.
C..   B  - Array to copy.
C.    OUTPUT
C..   A  - Resulting copy of array B.
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
 
      INTEGER*4  N
      REAL*4     A(*),B(*)
 
      DO  100 I = 1, N
 
         A(I) = B(I)
 
100   CONTINUE
 
      RETURN
 
      END

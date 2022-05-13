C.@PROCESS OPT(3)
      INTEGER FUNCTION IC0LEN(STRING)
C.======================================================================
C.    PURPOSE
C     Return length of non-blanks in string.                        CI<<
C.----------------------------------------------------------------------
C.    KEYWORDS  Command interpretor
C.----------------------------------------------------------------------
C.    PACKAGE   CI
C.    VISIBLE   Yes
C.    STANDARD_FORTRAN_77
C.    USE_ONLY
C.----------------------------------------------------------------------
C.    INPUT
C..   STRING     - Any character string.
C.    INPUT_IN_COMMON
C
C.    OUTPUT
C     IC0LEN     - Number of non-blank charaters. I.e. look from end of
C                  string to first non-blank character.
C
C.    OUTPUT_IN_COMMON
C.----------------------------------------------------------------------
C.    PROGRAMMER    Jan Fyen
C.    CREATION_DATE    Apr 1988
C.    MADE_AT  NTNF/NORSAR
C     Pb. 51
C     N-2007 Kjeller
C
C.    MODIFICATION
C.    CORRECTION
C.======================================================================
C.OPT(3)
C----     From SSA
C   Function that returns with the length of a string.
C   Blanks are not counted.
C----
 
      CHARACTER*(*) STRING
 
      IC0LEN = LEN(STRING)
 
    6 CONTINUE
 
      IF (.NOT.( STRING(IC0LEN:IC0LEN) .EQ. ' ' )) GO TO    7
 
      IC0LEN = IC0LEN - 1
 
      IF( IC0LEN.GT.0 ) GO TO    6
 
    7 CONTINUE
 
      RETURN
      END

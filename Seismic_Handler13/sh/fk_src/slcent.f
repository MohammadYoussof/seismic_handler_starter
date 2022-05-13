C.@PROCESS OPT(3)
      SUBROUTINE SLCENT (X, Y, Z, XREF, YREF, ZREF, NCHAN)
 
 
      REAL*4    XREF, YREF, ZREF
      REAL*4    X(*), Y(*), Z(*)
      INTEGER*4 NCHAN
 
C.OPT(3)
C=======================================================================
C     PURPOSE:
C            This function returns the centroid of NCHAN given stations
C
C
C
C-----------------------------------------------------------------------
C     INPUT:
C     -----
C
C     Parameters:
C
C     X, Y, Z          - Coordinates of stations in meters
C
C     NCHAN            - Number of channels
C
C
C
C     OUTPUT:
C     ------
C
C     Parameters:
C
C     XREF, YREF, ZREF - Coordinates of reference point in meters
C
C
C-----------------------------------------------------------------------
C     COMMENT:
C
C
C-----------------------------------------------------------------------
C     PROGRAMMER:
C
C     Tormod Kv{rna                        - November 19, 1986
C
C     NTNF/NORSAR
C     Pb. 51
C     N-2007 Kjeller
C
C     LAST MODIFIED:                         November 19, 1986
C
C=======================================================================
 
      XREF = 0.0
      YREF = 0.0
      ZREF = 0.0
 
      DO 100 I = 1, NCHAN
 
         XREF = XREF + X(I)
         YREF = YREF + Y(I)
         ZREF = ZREF + Z(I)
 
100   CONTINUE
 
      ZREF = ZREF / FLOAT (NCHAN)
      YREF = YREF / FLOAT (NCHAN)
      XREF = XREF / FLOAT (NCHAN)
 
      RETURN
 
      END

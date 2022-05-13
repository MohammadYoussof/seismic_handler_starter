C.@PROCESS OPT(3)
      SUBROUTINE SLFOUT ( CHNDAT , NSEQU  , INTPTR ,
     +                    PTRDIM , INTPTS , PTSDIM , WRKBUF ,
     +                    BUFDIM , CFFT   , CFTDIM , CFTPTR ,
     +                    CPRDIM , CFTPTS , CPSDIM , ERRMSG , IERR ,
     +                    TYPCTL)
 
      INTEGER*4  TYPCTL
 
C.OPT(3)
C----
C         Subroutine to compute fast FOUrier TRAnsforms
C   of any number of sequences. ( Only the positive part is output ).
C         Number of sequences are given by NSEQU.
C         Start of (sub)-sequence N is given by INTPTR(N),
C   dimension of INTPTR is PTRDIM.
C         ›points of (sub)-sequence N is given by INTPTS (N),
C   dimension of INTPTS is PTSDIM.
C         As working space we have array WRKBUF with dimension BUFDIM,
C   BUFDIM have to be twice the length of the fourier transform
C   of the longest sequence.
C         The resulting positive FFT's are stored in the
C   one dimensional complex array CFFT with dimension CFTDIM. The
C   positive FFT's are the NFFT/2 + 1 first samples of
C   the total FFT.
C         The first sample of the positive FFT of sequence N is given
C   by CFTPTR (N). Dimension of CFTPTR is CPRDIM.
C         Number of samples of the positive FFT of sequence N is given
C   by CFTPTS (N). Dimension of CFTPTS is CPSDIM.
C         Any reported mistakes is returned through ERRMSG and
C         type of error is given by IERR
C
C
C   Applications so far: Wide-Band Slowness Analysis.
C
C
C   Author       : Tormod Kv{rna
C                  NTNF/NORSAR
C                  Pb. 51 N-2007 Kjeller
C
C   Last modified: 15. August 1986
C
C----
 
C----
C   Input :
C----
      REAL*4        CHNDAT(*)
C                                 - Array containing input sequences.
C----
      INTEGER*4     NSEQU
C                                 - Number of sequences. ( Channels ).
C----
      INTEGER*4     INTPTR(*)
C                                 - Start of (sub)-sequence N in CHNDAT
C----
      INTEGER*4     PTRDIM
C                                 - Dimension of array INTPTR.
C----
      INTEGER*4     INTPTS(*)
C                                 - Number of points for each input
C                                   (sub)-sequence.
C----
      INTEGER*4     PTSDIM
C                                 - Dimension of array INTPTS.
C----
      REAL*4        WRKBUF(*)
C                                 - Working array
C----
      INTEGER*4     BUFDIM
C                                 - Dimension of working array WRKBUF.
C----
C   Output:
C----
      COMPLEX*8     CFFT(*)
C                                 - Output array containing positive
C                                   FFT's of all sequences.
C----
      INTEGER*4     CFTDIM
C                                 - Dimension of array CFFT
C----
      INTEGER*4     CFTPTR(*)
C                                 - Pointer array to start of positive
C                                   FFT's in CFFT.
C----
      INTEGER*4     CPRDIM
C                                 - Dimension of array CFTPTR.
C----
      INTEGER*4     CFTPTS(*)
C                                 - Number of points for each positive
C                                   FFT.
C----
      INTEGER*4     CPSDIM
C                                 - Dimension of array CFTPTS.
C----
      CHARACTER*(*) ERRMSG
C                                 - Error message
C----
      INTEGER*4     IERR
C                                 - IERR =  0 : Everything O.K.
C                                 - IERR =  1 : Warning, calculation
C                                               continues.
C                                 - IERR = -1 : Severe error,
C                                               calculation stopped and
C                                               return from subroutine.
C----
 
      INTEGER*4     PXI,PXR
 
C----
C   Initiate quality variables
C----
 
      IERR   = 0
 
C----
C   Some checks on array dimensions to avoid ABEND CODE
C----
 
C----
C   Check on array CHNPTR
C----
 
      IF (  NSEQU .GT. PTRDIM ) THEN
 
         ERRMSG = '***>SLFOUT: Length of array INTPTR is exceeded'
 
         IERR = -1
 
         RETURN
 
      END IF
 
C----
C   Check on array CHNPNT
C----
 
      IF (  NSEQU .GT. PTSDIM ) THEN
 
         ERRMSG = '***>SLFOUT: Length of array INTPTS is exceeded'
 
         IERR = -1
 
         RETURN
 
      END IF
 
C----
C   Check on array CFTPTR
C----
 
      IF (  NSEQU .GT. CPRDIM ) THEN
 
         ERRMSG = '***>SLFOUT: Length of array CFTPTR is exceeded'
 
         IERR = -1
 
         RETURN
 
      END IF
 
C----
C   Check on array CFTPTS
C----
 
      IF (  NSEQU .GT. CPSDIM ) THEN
 
         ERRMSG = '***>SLFOUT: Length of array CFTPTS is exceeded'
 
         IERR = -1
 
         RETURN
 
      END IF
 
C----
C   Channel loop
C----
 
      DO 1 I = 1, NSEQU
 
C----
C   FFT size must be equal to or larger than the length of the
C   sequences
C----
 
         NFFT =  8
 
    2    CONTINUE
 
         IF ( NFFT .GE. INTPTS(I) ) GO TO    3
 
            NFFT = NFFT * 2
 
         GO TO    2
 
    3    CONTINUE
 
C----
C   Check on array WRKBUF
C----
 
         IF (  NFFT .GT. BUFDIM/2 ) THEN
 
            ERRMSG = '***>SLFOUT: Length of array WRKBUF is exceeded'
 
            IERR = -1
 
            RETURN
 
         END IF
 
C----
C    Store fourier transforms in buffer
C----
 
         PXR =  1
         PXI =  PXR + BUFDIM/2
 
         IF( TYPCTL.GT.2 ) WRITE(6,'(2X,''...> SLFOUT Zero WRKBUF'',
     +                                  2I6)') PXR,NFFT
         CALL CHZERO ( WRKBUF(PXR), NFFT )
         IF( TYPCTL.GT.2 ) WRITE(6,'(2X,''...> SLFOUT Zero WRKBUF'',
     +                                  2I6)') PXI,NFFT
         CALL CHZERO ( WRKBUF(PXI), NFFT )
         IF( TYPCTL.GT.2 ) WRITE(6,'(2X,''...> SLFOUT Move from DAT'',
     +                              I6,'' to WRKBUF'',2I6)')
     +                              INTPTR(I),PXR,INTPTS(I)
         CALL CHMOVE ( WRKBUF(PXR), CHNDAT( INTPTR(I) ), INTPTS(I) )
         IF( TYPCTL.GT.2 ) WRITE(6,'(2X,''...> SLFOUT FFT WRKBUF'',
     +                              I6,'' R,I '',2I6)')
     +                              PXR,PXI,NFFT
         CALL SLFFT  ( WRKBUF(PXR), WRKBUF(PXI), NFFT, -1 )
 
C----
C   Create pointer arrays to CFFT
C----
 
 
         IF ( I .EQ. 1) THEN
 
            CFTPTR (I) = 1
 
         ELSE
 
            CFTPTR (I) = CFTPTR ( I - 1) + CFTPTS ( I - 1 )
 
         ENDIF
 
         CFTPTS (I) = NFFT / 2 + 1
 
C----
C   Check on array CFFT
C----
 
         IF (  CFTPTR(I)+CFTPTS(I) .GT. CFTDIM ) THEN
 
            ERRMSG = '***>SLFOUT: Length of array CFFT is exceeded'
 
            IERR = -1
 
            RETURN
 
         END IF
C---
C   Store positive part of FFT in CFFT
C---
 
         IF( TYPCTL.GT.1 ) WRITE(6,'(2X,''...> SLFOUT Store WRKBUF'',
     +                              2I6,'' to CFFT'',2I6)')
     +                              PXR,PXI,CFTPTR(I),CFTPTS(I)
 
         DO    4 J = 1, CFTPTS(I)
 
            CFFT ( CFTPTR (I) + J - 1 ) = CMPLX( WRKBUF (PXR + J - 1),
     %                                           WRKBUF (PXI + J - 1) )
 
    4    CONTINUE
 
    1 CONTINUE
 
C----
C   That's it|
C----
 
      RETURN
 
      END

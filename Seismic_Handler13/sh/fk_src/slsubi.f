C.@PROCESS OPT(3)
      SUBROUTINE SLSUBI ( NSEQU  , CHNPTR , PTRDIM , CHNPTS , PTSDIM ,
     +                    START  , STRDIM , STOP   , STPDIM , INTDIM ,
     +                    INTPTR , INTPTS , DELTA  , ERRMSG , IERR   ,
     +                    TYPCTL )
 
C.OPT(3)
C----
C    Subroutine to find first physical element of a sequence
C        sub-interval given the start and stop of each sequence.
C    Number of sequences is NSEQU
C    The first sample of sequence N is given by CHNPTR (N),
C        dimension of CHNPTR is PTRDIM.
C    Number of sampels of sequence N is given by CHNPTS (N),
C        dimension of CHNPTS is PTSDIM.
C    The first sample of sub-sequence N in sequence N is given by
C        START(N) with dimension STRDIM.
C    The last sample of sub-sequence N in sequence N is given by
C        STOP(N) with dimension STPDIM.
C    The output of this subroutine is the physical pointers to
C        each sub-sequence given by INTPTR(N).  The dimension of INTPTR
C        is INTDIM.
C    ›points of each sub-sequence is INTPTS(N).  The dimension
C        if INTPTS is also INTDIM
C    Sampling interval is given by DELTA
C    By physical pointers we mean pointers to the one-dimensional
C        array where the sequences are physically stored.
C    Any reported mistakes is returned through ERRMSG and
C        type of error is given by IERR
C
C
C   Applications so far: Preparation of input to subroutine FOUTRA
C
C
C   Author       : Tormod Kv{rna
C                  NTNF/NORSAR
C                  Pb. 51 N-2007 Kjeller
C
C   Last modified:  2. December 1986
C   Last modified:     October  1988 Jan Fyen
C
C --- Input :
C
      INTEGER*4     NSEQU
C                                 - Number of sequences
      INTEGER*4     CHNPTR(*)
C                                 - Array containing pointer to start
C                                   of sequence N.
      INTEGER*4     PTRDIM
C                                 - Dimension of array CHNPTR.
      INTEGER*4     CHNPTS(*)
C                                 - Array containing number of samples
C                                   of sequence N.
      INTEGER*4     PTSDIM
C                                 - Dimension of array CHNPTS.
      REAL*4        START (*)
C                                 - Pointer array to start of each
C                                   sub-sequence, referenced to start
C                                   of each sequence.
      INTEGER*4     STRDIM
C                                 - Dimension of array START
      REAL*4        STOP  (*)
C                                 - Pointer array to stop  of each
C                                   sub-sequence, referenced to start
C                                   of each sequence.
      REAL*4        DELTA  (*)
C                                 - Sampling interval for channel N
      INTEGER*4     STPDIM
C                                 - Dimension of array START
      INTEGER*4     INTDIM
C                                 - Dimension of array INTPTR.
c
C --- Output:
c
      INTEGER*4     INTPTR(*)
C                                 - Pointer array containing physical
C                                   pointers to each sub-sequence
C                                   referenced to start of the one-
C                                   dimensional data array.
      INTEGER*4     INTPTS(*)
C                                 - Array containing ›points
C                                   of each sub-sequence.
      CHARACTER*(*) ERRMSG
C                                 - Error message
      INTEGER*4     IERR  , TYPCTL
C                                 - IERR =  0 : Everything O.K.
C                                 - IERR =  1 : Warning, calculation
C                                               continues.
C                                 - IERR = -1 : Severe error,
C                                               calculation stopped and
C                                               return from subroutine.
c 
c 
C----
C   Initiate quality variables
      IERR   = 0
c
C----
C   Some checks on array dimensions to avoid ABEND CODE
c
C   Check on array CHNPTR
      IF (  NSEQU .GT. PTRDIM ) THEN
         ERRMSG = '***>SLSUBI: Length of array CHNPTR is exceeded'
         IERR = -1
         RETURN
      END IF
c
C   Check on array CHNPTS
      IF (  NSEQU .GT. PTSDIM ) THEN
         ERRMSG = '***>SLSUBI: Length of array CHNPTS is exceeded'
         IERR = -1
         RETURN
      END IF
c
C   Check on array START
      IF (  NSEQU .GT. STRDIM ) THEN
         ERRMSG = '***>SLSUBI: Length of array START is exceeded'
         IERR = -1
         RETURN
      END IF
c
C   Check on array STOP
      IF (  NSEQU .GT. STPDIM ) THEN
         ERRMSG = '***>SLSUBI: Length of array STOP is exceeded'
         IERR = -1
         RETURN
      END IF
c
C   Check on array INTPTR
      IF (  NSEQU .GT. INTDIM ) THEN
         ERRMSG = '***>SLSUBI: Length of array INTPTR is exceeded'
         IERR = -1
         RETURN
      END IF
c
C ---  Channel loop
      DO 1 I = 1, NSEQU
         ISAMP      = 0
         SMPFRQ     = 1.0 / DELTA(I)
         ISAMP      = NINT(START(I)*SMPFRQ)
         INTPTR (I) = CHNPTR(I) + ISAMP
         ISAMP      = 0
         ISAMP      = NINT(STOP(I)*SMPFRQ)
         NRIGHT     = CHNPTR(I) + ISAMP
         INTPTS (I) = NRIGHT - INTPTR(I)
 
         IF( TYPCTL.GT.1 ) THEN
             WRITE(6,'(2X,''...> SLSUBI'', I3,3F9.3,2I6)')
     +                 I,START(I),STOP(I),SMPFRQ,INTPTR(I),INTPTS(I)
         END IF
c
C   Check that start of sub-sequence is within sequence, return
C   with warning.
 
         IF ( INTPTR(I) .GT. CHNPTR(I) + CHNPTS(I) - 1 ) THEN
            ERRMSG = '...>SLSUBI: Start sub-sequence exceeeds sequence'
            IERR = 1
         ENDIF
 
    1 CONTINUE
c 
      RETURN
      END

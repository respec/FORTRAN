      SUBROUTINE SHELR ( A,N,NROW,NCOL,JK,ID)
C	RAIN BLOCK
C	CALLED BY RAIN AT 1159 AND 1161
C=======================================================================
C     SHELL SORT
C     REFERENCES..
C         D.A.SHELL, CACM, VOL 2 (1959), PP 30..32
C         T.N. HIBBERD, SDC RPEORT SP-982
C         J.ROOTHROYD, CACM, ALGORITHM 201
C     THIS IS A FORTRAN VERSION OF ALGORITHM 201.
C     A    = INPUT ARRAY TO BE ARRANGED
C     N    = NUMBER OF ELEMENTS (FILLED ROWS) IN  A
C     NROW = NUMBER OF ROWS IN  A
C     NCOL = NUMBER OF COLUMNS IN  A
C     JK   = ARRAY CONTAINING HIERARCHY OF COLUMNS TO DETERMINE
C            FINAL ARRANGEMENT OF  A (JK(LAST+1)=0)
C     ID   = ARRAY IN 1 TO 1 CORRESPONDANCE WITH JK TO DETERMINE
C            DIRECTION OF SORT.  ID=1...ASCENDING
C                                ID=2...DESCENDING
C=======================================================================
      INCLUDE 'TAPES.INC'
      DIMENSION  A(3*LSTORM),ID(2),JK(2)
C=======================================================================
C     FIND M=ONE LESS THAN LEAST POWER OF 2.GT.N
C=======================================================================
      M = 0
   10 M = M+M+1
      IF(M-N) 10,20,20
C=======================================================================
C     OUTER LOOP. .HALVE M.
C=======================================================================
   20 M = M/2
C=======================================================================
C     TEST FOR END OF OUTER LOOP.
C=======================================================================
      IF (M) 70,70,30
C=======================================================================
C     FIND LIMIT FOR MIDDLE LOOP
C=======================================================================
   30 K = N-M
C=======================================================================
C     BEGIN MIDDLE LOOP
C=======================================================================
      DO 60 J = 1,K
C=======================================================================
C     SETUP FOR INNER LOOP
C=======================================================================
      I = J
C=======================================================================
C     MIDDLE AND INNER LOOP . . COMPARE.
C=======================================================================
   40 L    = I+M
      LJ   = 1
   23 IAD  = NROW*(JK(LJ)-1)
      L1   = L+IAD
      I1   = I+IAD
      IDEC = ID(LJ)
      GO TO (21,22),IDEC
   21 IF (A(I1)- A(L1)) 60,61,50
   22 IF (A(I1)- A(L1)) 50,61,60
   61 LJ = LJ+1
      IF(JK(LJ)) 60,60,23
C=======================================================================
C     NOT IN SEQUENCE, SO SWAP
C=======================================================================
   50 DO 80 IP = 1,NCOL
      NROCO    = NROW*(IP-1)
      NI       = NROCO+I
      NL       = NROCO+L
      TEMP     = A(NI)
      A(NI)    = A(NL)
   80 A(NL)    = TEMP
C=======================================================================
C     GO DOWN INNER LOOP . . ONLY IF SWAP
C=======================================================================
      I = I-M
C=======================================================================
C     TEST FOR END OF INNER LOOP
C=======================================================================
      IF (I) 60,60,40
C=======================================================================
C     END OF INNER AND MIDDLE LOOP.
C=======================================================================
   60 CONTINUE
      GO TO 20
   70 RETURN
      END

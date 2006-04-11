      SUBROUTINE SORT(J)
C=======================================================================
C     THIS SUBROUTINE IS CALLED BY THE STATS BLOCK.
C     IT IS USED TO SORT, INTO DESCENDING ORDER, A SET OF EVENT
C     DATA FOR ANY OF THE TEN PARAMETERS THAT CAN BE ANALYZED.
C     IT UTILIZES AN ARRAY OF DUMMY VARIABLES FOR DATE AND TIME.
C     THUS, THE ORIGINAL SEQUENCE OF DATE/TIME VALUES IS RETAINED
C     THROUGH REPEATED CALLS FOR THE SORTING OF OTHER EVENT PARAMETERS.
C
C     THE ALGORITHM UTILIZED IS CALLED A SHELL SORT (AFTER D.L. SHELL)
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'STCOM.INC'
      PARAMETER (ALN2=1.0/0.69314718, TINY=0.00001)
C=======================================================================
C     RESET DUMMY ARRAYS TO CORRESPOND TO ORIGINAL DATE/TIME SEQUENCE
C=======================================================================
      LOGN2     = INT(ALOG(FLOAT(NEVNTS))*ALN2+TINY)
      DO 50 I   = 1,NEVNTS
      ISRT(I)   = IEVNTB(I)
      TIMSRT(I) = TEVNTB(I)
50    CONTINUE
C=======================================================================
      M         = NEVNTS
      DO 150 NN = 1,LOGN2
      M         = M / 2
      K         = NEVNTS - M
      DO 250 JJ = 1,K
      I         = JJ
100   CONTINUE
      L         = I + M
C=======================================================================
C     SHIFT PARAMETER VALUES TO NEW POSITIONS
C     SHIFT DATE AND TIME TO NEW POSITIONS IN DUMMY ARRAYS
C=======================================================================
      IF(PARAM(L,J).GT.PARAM(I,J)) THEN
                                   TEMP       = PARAM(I,J)
                                   PARAM(I,J) = PARAM(L,J)
                                   PARAM(L,J) = TEMP
                                   ITEMP1     = ISRT(I)
                                   ISRT(I)    = ISRT(L)
                                   ISRT(L)    = ITEMP1
                                   TEMP2      = TIMSRT(I)
                                   TIMSRT(I)  = TIMSRT(L)
                                   TIMSRT(L)  = TEMP2
                                   I          = I - M
                                   IF(I.GE.1) GO TO 100
                                   ENDIF
250   CONTINUE
150   CONTINUE
      RETURN
      END

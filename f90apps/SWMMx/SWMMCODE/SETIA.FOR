      SUBROUTINE SETIA(IARRAY,NROW,NCOL,IVAL)
C=======================================================================
C     Inititalize integer arrays.
C=======================================================================
      INTEGER   IARRAY(NROW,NCOL)
      DO 20    I  = 1,NROW
      DO 20    J  = 1,NCOL
   20 IARRAY(I,J) = IVAL
      RETURN
      END

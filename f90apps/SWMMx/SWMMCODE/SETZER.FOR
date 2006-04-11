      SUBROUTINE SETZER(ARRAY,NROW,NCOL,VAL)
C=======================================================================
C     Inititalize real arrays.
C=======================================================================
      REAL ARRAY(NROW,NCOL),VAL
      DO 20    I = 1,NROW
      DO 20    J = 1,NCOL
   20 ARRAY(I,J) = VAL
      RETURN
      END

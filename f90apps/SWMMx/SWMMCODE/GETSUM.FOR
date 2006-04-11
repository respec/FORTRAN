      SUBROUTINE GETSUM(N,START,DELTA,INTEG,TOP,DFULL,PEXP)
C=======================================================================
C     This subroutine sums the arc length of a power function
C             between the values start and start + N*DELTA.
C     N     = Number of panels.
C     INTEG = Integral approximation.
C     START = Starting Y value.
C=======================================================================
      REAL INTEG
      Y        = START
      INTEG    = 0.0
      DO 100 I = 1,N
      Y        = Y     + DELTA
100   INTEG    = INTEG + GETDEP(DELTA,Y,TOP,DFULL,PEXP)
      RETURN
      END

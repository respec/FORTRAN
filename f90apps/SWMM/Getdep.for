      REAL FUNCTION GETDEP(DELTA,Y,TOP,DFULL,PEXP)
C	CALLED BY GETSUM NEAR LINE 14 (EXTRAN AND TRANSPORT)
C=======================================================================
C     This function calculates the arc length of a power function
C          curve between Y-DELTA and Y.
C=======================================================================
      POW    = 1.0/PEXP
      COEF   = 0.5*TOP/DFULL**POW
      DIFF   = Y - DELTA
      IF(DIFF.LE.0.0) DIFF = 1.0E-05
      X      = COEF*(Y**POW - DIFF**POW)
      GETDEP = SQRT(X**2.0  + DELTA**2.0)
      RETURN
      END

C
C
C
      REAL   FUNCTION   GAUSEX
     I                        (EXPROB)
C
C     + + + PURPOSE + + +
C     This function returns the Gaussian or standard normal deviate
C     with exceedance probability (tail probability) EXPROB.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   EXPROB
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EXPROB - ?????
C
C     + + + LOCAL VARIABLES + + +
      REAL   C0,C1,C2,D1,D2,D3,P,PR,T
C
C     + + + INTRINSICS + + +
      INTRINSIC   SQRT, ALOG
C
C     + + + DATA INITIALIZATION + + +
      DATA  C0,C1,C2/2.51551700, .8028530000, .0103280000/
      DATA  D1,D2,D3/1.432788000, .1892690000, .0013080000/
C
C     + + + END SPECIFICATIONS + + +
C
C     GAUSSIAN PROBABILITY FUNCTIONS   W.KIRBY  JUNE 71
C     GAUSEX=VALUE EXCEEDED WITH PROB EXPROB
C     GAUSCF MODIFIED 740906 WK -- REPLACED ERF FCN REF BY RATIONAL APPR
C     ALSO REMOVED DOUBLE PRECISION FROM GAUSEX AND GAUSAB.
C     76-05-04 WK -- TRAP UNDERFLOWS IN EXP IN GUASCF AND DY.
C
      P=EXPROB
      IF (P.GE.1.0) THEN
C       SET TO MINIMUM
        GAUSEX=-10.
      ELSE IF (P.LE.0.0) THEN
C       SET AT MAXIMUM
        GAUSEX=+10.
      ELSE
C       COMPUTE VALUE
        PR=P
        IF (P.GT.0.5) PR=1.00-PR
        T = SQRT(-2.00*ALOG(PR))
        GAUSEX = T-(C0+T*(C1+T*C2))/(1.D0+T*(D1+T*(D2+T*D3)))
        IF (P.GT.0.5) GAUSEX = -GAUSEX
      END IF
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   GAUSAB
     I                         (CUMPRB)
C
C     + + + PURPOSE + + +
C     GAUSSIAN PROBABILITY FUNCTIONS   W.KIRBY  JUNE 71
C     GAUSEX=VALUE EXCEEDED WITH PROB EXPROB
C     GAUSCF MODIFIED 740906 WK -- REPLACED ERF FCN REF BY RATIONAL APPR
C     ALSO REMOVED DOUBLE PRECISION FROM GAUSEX AND GAUSAB.
C     76-05-04 WK -- TRAP UNDERFLOWS IN EXP IN GUASCF AND DY.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   CUMPRB
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CUMPRB - ?????
C
C     + + + LOCAL VARIABLES + + +
      REAL   C0,C1,C2,D1,D2,D3,P,PR,T
C
C     + + + INTRINSICS + + +
      INTRINSIC   SQRT, ALOG
C
C     + + + DATA INITIALIZATION + + +
      DATA  C0,C1,C2/2.51551700, .8028530000, .0103280000/
      DATA  D1,D2,D3/1.432788000, .1892690000, .0013080000/
C
C     + + + END SPECIFICATIONS + + +
C
      GAUSAB = 0.0
      P = 1.0 - CUMPRB
      IF (P.GE.1.0) THEN
C       SET TO MINIMUM
        GAUSAB=-10.
      ELSE IF (P.LE.0.0) THEN
C       SET AT MAXIMUM
        GAUSAB=+10.
      ELSE
C       COMPUTE VALUE
        PR=P
        IF (P.GT.0.5) PR=1.00-PR
        T = SQRT(-2.00*ALOG(PR))
        GAUSAB = T-(C0+T*(C1+T*C2))/(1.D0+T*(D1+T*(D2+T*D3)))
        IF (P.GT.0.5) GAUSAB = -GAUSAB
      END IF
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   GAUSDY
     I                         (XX)
C
C     + + + PURPOSE + + +
C     GAUSSIAN PROBABILITY FUNCTIONS   W.KIRBY  JUNE 71
C     GAUSEX=VALUE EXCEEDED WITH PROB EXPROB
C     GAUSCF MODIFIED 740906 WK -- REPLACED ERF FCN REF BY RATIONAL APPR
C     ALSO REMOVED DOUBLE PRECISION FROM GAUSEX AND GAUSAB.
C     76-05-04 WK -- TRAP UNDERFLOWS IN EXP IN GUASCF AND DY.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   XX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XX     - ?????
C
C     + + + LOCAL VARIABLES + + +
      REAL   XLIM
C
C     + + + INTRINSICS + + +
      INTRINSIC   EXP, ABS
C
C     + + + DATA INITIALIZATION + + +
      DATA  XLIM  /  18.3 /
C
C     + + + END SPECIFICATIONS + + +
C
      GAUSDY = 0.0
      IF (ABS(XX) .LE. XLIM) THEN
        GAUSDY = 0.3989423*EXP(-0.5*XX*XX)
      END IF
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   GAUSCF
     I                         (XX)
C
C     + + + DUMMY ARGUMENT + + +
      REAL   XX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XX     - ?????
C
C     + + + LOCAL VARIABLES + + +
      REAL   AX, XLIM, T, D
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, EXP
C
C     + + + END SPECIFICATIONS + + +
C
      XLIM = 18.3
      AX = ABS(XX)
      GAUSCF = 1.0
      IF (AX .LE. XLIM) THEN
        T = 1.0/(1.0+0.2316419*AX)
        D = 0.3989423*EXP(-0.5*XX*XX)
        GAUSCF = 1.0 - D*T*((((1.330274*T -1.821256)*T + 1.781478)*T -
     #           0.3565638)*T + 0.3193815)
      END IF
C
      IF (XX .LT. 0.0) GAUSCF = 1.0 - GAUSCF
C
      RETURN
      END

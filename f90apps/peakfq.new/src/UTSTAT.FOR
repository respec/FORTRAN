C
C
C
      REAL FUNCTION   SRMAX
     I                      (NVAL, X)
C
C     + + + PURPOSE + + +
C     This routine determines the maximum value from an array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NVAL
      REAL      X(NVAL)
C
C     + + + ARGUMENT DEFINITION + + +
C     NVAL   - number of values to check
C     X      - array of values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
      REAL      XMAX
C
C     + + + END SPECIFICATIONS + + +
C
      XMAX = -1.0E20
      DO 10 I = 1,NVAL
        IF (X(I) .GT. XMAX) XMAX = X(I)
 10   CONTINUE
C
      SRMAX = XMAX
C
      RETURN
      END
C
C
C
      REAL FUNCTION   SRMIN
     I                      (NVAL, X)
C
C     + + + PURPOSE + + +
C     This routine determines the minimum value from an array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NVAL
      REAL      X(NVAL)
C
C     + + + ARGUMENT DEFINITION + + +
C     NVAL   - number of values to check
C     X      - array of values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
      REAL      XMIN
C
C     + + + END SPECIFICATIONS + + +
C
      XMIN = 1.0E20
      DO 10 I = 1,NVAL
        IF (X(I) .LT. XMIN) XMIN = X(I)
 10   CONTINUE
C
      SRMIN = XMIN
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   SIMAX
     I                         (NVAL, IX)
C
C     + + + PURPOSE + + +
C     This routine determines the maximum value from an array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NVAL
      INTEGER   IX(NVAL)
C
C     + + + ARGUMENT DEFINITION + + +
C     NVAL   - number of values to check
C     IX     - array of values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, IMAX
C
C     + + + END SPECIFICATIONS + + +
C
      IMAX = -100000000
      DO 10 I = 1,NVAL
        IF (IX(I) .GT. IMAX) IMAX = IX(I)
 10   CONTINUE
C
      SIMAX = IMAX
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   SIMIN
     I                         (NVAL, IX)
C
C     + + + PURPOSE + + +
C     This routine determines the maximum value from an array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NVAL
      INTEGER   IX(NVAL)
C
C     + + + ARGUMENT DEFINITION + + +
C     NVAL   - number of values to check
C     IX     - array of values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
      INTEGER   IMIN
C
C     + + + END SPECIFICATIONS + + +
C
      IMIN = 100000000
      DO 10 I = 1,NVAL
        IF (IX(I) .LT. IMIN) IMIN = IX(I)
 10   CONTINUE
C
      SIMIN = IMIN
C
      RETURN
      END
C
C
C
      DOUBLE PRECISION FUNCTION   SDMAX
     I                                  (NVAL, DX)
C
C     + + + PURPOSE + + +
C     This routine determines the maximum value from an array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NVAL
      DOUBLE PRECISION   DX(NVAL)
C
C     + + + ARGUMENT DEFINITION + + +
C     NVAL   - number of values to check
C     DX     - array of values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
      DOUBLE PRECISION  DMAX
C
C     + + + END SPECIFICATIONS + + +
C
      DMAX = -1.0D20
      DO 10 I = 1,NVAL
        IF (DX(I) .GT. DMAX) DMAX = DX(I)
 10   CONTINUE
C
      SDMAX = DMAX
C
      RETURN
      END
C
C
C
      DOUBLE PRECISION FUNCTION   SDMIN
     I                                  (NVAL, DX)
C
C     + + + PURPOSE + + +
C     This routine determines the maximum value from an array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NVAL
      DOUBLE PRECISION   DX(NVAL)
C
C     + + + ARGUMENT DEFINITION + + +
C     NVAL   - number of values to check
C     DX     - array of values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
      DOUBLE PRECISION      DMIN
C
C     + + + END SPECIFICATIONS + + +
C
      DMIN = 1.0D20
      DO 10 I = 1,NVAL
        IF (DX(I) .LT. DMIN) DMIN = DX(I)
 10   CONTINUE
C
      SDMIN = DMIN
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   WILFRT
     I                         (SKU,ZETA,ERRFLG)
C
C     + + + PURPOSE + + +
C     WILFRT -- WILSON-HILFERTY REVISED TRANSFORM
C     PURPOSE -- APPROXIMATE TRANSFORMATION OF GAUSSIAN PERCENTAGE POINT
C        INTO STANDARDIZED PEARSON TYPE III.   THIS VERSION REPRODUCES
C        CORRECT MEAN, VARIANCE, SKEW AND LOWER BOUND OF STANDARDIZED
C        PEARSON-III AT SKEWS UP TO 9.0 AT LEAST.  DIFFERENCES BETWEEN
C        WILFRT PERCENTAGE POINTS AND HARTERS TABLES ARE OF THE ORDER OF
C        A FEW HUNDREDTHS OF A STD. DEVIATION, EXCEPT IN EXTREME POSITIV
C        TAIL (95% OR SO) WHERE ERROR IS OF ORDER OF TENTHS IN MAGNI-
C        TUDE BUT ABOUT 3% IN RELATIVE MAG.
C     USAGE --      X=WILFRT(SKEW,ZETA)*STDDEV+AMEAN
C        SKEW IS INPUT SKEW, MAY BE ZERO OR NEGATIVE OR POSITIVE.
C              IF ABS(SKEW) IS GREATER THAN 9.75, 9.75 IS USED.
C        ZETA IS STANDARD GAUSSIAN VARIATE.   FOR EXAMPLE, GAUSSB(IRAN)
C              YIELDS RANDOM NOS WHILE GAUSAB(PROB) YIELDS THE
C              PROB-TH QUANTILE.
C        STDDEV AND AMEAN ARE DESIRED VALUES OF STD DEVIATION AND
C              MEAN, IF DIFFERENT FROM ONE AND ZERO.
C     NOTE -- EACH INPUT SKEW VALUE IS COMPARED WITH PREVIOUS INPUT
C        VALUE. IF DIFFERENT BY MORE THAN 0.0003, TABLE LOOKUP OF NEW
C        PARAMETERS TAKES PLACE.  THEREFORE, CHAGE THE INPUT SKEW
C        AS SELDOM AS POSSIBLE.
C     WKIRBY  72-02-25
C     REVISED 73-02-09   TO ACCEPT ZERO SKEW.
C     REF -- W.KIRBY, COMPUTER-ORIENTED WILSON-HILFERTY TRANSFORMATION..
C         WATER RESOUR RESCH 8(5)1251-4, OCT 72.
C     REV 6/83 WK FOR PRIME ---- SAVE STTMNT ----
C     REV 7/86 BY AML TO OSW CODING CONVENTION
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ERRFLG
      REAL   SKU, ZETA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SKU     - ?????
C     ZETA    - ?????
C     ERRFLG  - ?????
C
C     + + + LOCAL VARIABLES + + +
      REAL      SKUTOL, ASK
      REAL           A,B,G,H,Z,SIG,FMU
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   WILFRS
C
C     + + + DATA INITIALIZATION + + +
      DATA    SKUTOL /0.0003/
C
C     + + + END SPECIFICATIONS + + +
C
C     FIRST TIME THRU OR NEW SKU (SKEW)
      ASK=ABS(SKU)
      IF (ASK.GE.SKUTOL) THEN
C       NONZERO SKEW
        CALL  WILFRS(ASK,G,H,A,B,ERRFLG)
        SIG=G*.1666667
        FMU=1.-SIG*SIG
        IF (SKU .LT. 0.0) THEN
          SIG=-SIG
          A=-A
        END IF
        Z=FMU+SIG*ZETA
        IF(Z.LT.H)Z=H
        WILFRT=A*(Z*Z*Z-B)
      ELSE
C       ZERO SKEW
        WILFRT=ZETA
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WILFRV
     I                   (SKU,ZETA,NZ,ERRFLG)
C
C     + + + PURPOSE + + +
C     WILFRT -- WILSON-HILFERTY REVISED TRANSFORM
C     PURPOSE -- APPROXIMATE TRANSFORMATION OF GAUSSIAN PERCENTAGE POINT
C        INTO STANDARDIZED PEARSON TYPE III.   THIS VERSION REPRODUCES
C        CORRECT MEAN, VARIANCE, SKEW AND LOWER BOUND OF STANDARDIZED
C        PEARSON-III AT SKEWS UP TO 9.0 AT LEAST.  DIFFERENCES BETWEEN
C        WILFRT PERCENTAGE POINTS AND HARTERS TABLES ARE OF THE ORDER OF
C        A FEW HUNDREDTHS OF A STD. DEVIATION, EXCEPT IN EXTREME POSITIV
C        TAIL (95% OR SO) WHERE ERROR IS OF ORDER OF TENTHS IN MAGNI-
C        TUDE BUT ABOUT 3% IN RELATIVE MAG.
C     USAGE --      X=WILFRT(SKEW,ZETA)*STDDEV+AMEAN
C        SKEW IS INPUT SKEW, MAY BE ZERO OR NEGATIVE OR POSITIVE.
C              IF ABS(SKEW) IS GREATER THAN 9.75, 9.75 IS USED.
C        ZETA IS STANDARD GAUSSIAN VARIATE.   FOR EXAMPLE, GAUSSB(IRAN)
C              YIELDS RANDOM NOS WHILE GAUSAB(PROB) YIELDS THE
C              PROB-TH QUANTILE.
C        STDDEV AND AMEAN ARE DESIRED VALUES OF STD DEVIATION AND
C              MEAN, IF DIFFERENT FROM ONE AND ZERO.
C     NOTE -- EACH INPUT SKEW VALUE IS COMPARED WITH PREVIOUS INPUT
C        VALUE. IF DIFFERENT BY MORE THAN 0.0003, TABLE LOOKUP OF NEW
C        PARAMETERS TAKES PLACE.  THEREFORE, CHAGE THE INPUT SKEW
C        AS SELDOM AS POSSIBLE.
C     WKIRBY  72-02-25
C     REVISED 73-02-09   TO ACCEPT ZERO SKEW.
C     REF -- W.KIRBY, COMPUTER-ORIENTED WILSON-HILFERTY TRANSFORMATION..
C         WATER RESOUR RESCH 8(5)1251-4, OCT 72.
C     REV 6/83 WK FOR PRIME ---- SAVE STTMNT ----
C     REV 7/86 BY AML TO OSW CODING CONVENTION
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ERRFLG, NZ
      REAL   SKU, ZETA(NZ)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SKU    - ?????
C     ZETA   - ?????
C     NZ     - ?????
C     ERRFLG - ?????
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
      REAL      SKUTOL, ASK
      REAL           A,B,G,H,SIG,FMU
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, AMAX1
C
C     + + + EXTERNALS + + +
      EXTERNAL   WILFRS
C
C     + + + DATA INITIALIZATION + + +
      DATA    SKUTOL /0.0003/
C
C     + + + END SPECIFICATIONS + + +
C
C     FIRST TIME THRU OR NEW SKU (SKEW)
      ASK=ABS(SKU)
      IF (ASK.GE.SKUTOL) THEN
C       NONZERO SKEW
        CALL  WILFRS(ASK,G,H,A,B,ERRFLG)
        SIG=G*.1666667
        FMU=1.-SIG*SIG
        IF (SKU .LT. 0.0) THEN
          SIG=-SIG
          A=-A
        END IF
      END IF
C
      DO 20 I = 1,NZ
        ZETA(I) = A*(AMAX1(H,FMU+SIG*ZETA(I))**3 - B)
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   WILFRS
     I                    (SK,G,H,A,B,
     O                     ERRFLG)
C
C     + + + PURPOSE + + +
C     COMPUTES PARAMETERS USED BY WILFRT TRANSFORMATIN
C     USES APPROX FORMULA AND CORRECTION TERMS PREPARED FROM
C     ROUTINE WHMPP (E443-5).  WKIRBY  FEB72
C     PARAMETERS RETURNED TO WILFRT ARE INTENDED TO MAKE
C     WILFRT A STANDARDIZED R.V.  (MEAN=0,STDEV=1) WITH
C     SPECIFIED SKEW AND CORRECT LOWER BOUND
C     REVISED CALC OF CORRECTION TABLE  72-03-03 WK
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ERRFLG
      REAL     SK, G, H, A, B
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SK     - ?????
C     G      - ?????
C     H      - ?????
C     A      - ?????
C     B      - ?????
C     ERRFLG - 0 - ok
C              1 - excessive skew truncated
C
C     + + + SAVE VARIABLES + + +
      SAVE  HALF1, HALF2
      REAL  HALF1(80), HALF2(80)
C
C     + + + LOCAL VARIABLES + + +
      REAL     ROW(4),SA(40),TABLE(40,4),
     #         S, Q, P, TOG
      INTEGER   FLAG, I, K, NROZ
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE(ROW,S),(TABLE,SA,HALF1),(TABLE(1,3),HALF2)
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NROZ / 40/
      DATA HALF1/
     #  0.0     , 0.250000, 0.500000, 0.750000, 1.000000, 1.250000,
     #  1.500000, 1.750000, 2.000000, 2.250000, 2.500000, 2.750000,
     #  3.000000, 3.250000, 3.500000, 3.750000, 4.000000, 4.250000,
     #  4.500000, 4.750000, 5.000000, 5.250000, 5.500000, 5.750000,
     #  6.000000, 6.250000, 6.500000, 6.750000, 7.000000, 7.250000,
     #  7.500000, 7.750000, 8.000000, 8.250000, 8.500000, 8.750000,
     #  9.000000, 9.250000, 9.500000, 9.750000, 0.0     ,-0.000144,
     # -0.001137,-0.003762,-0.008674,-0.011555,-0.010076,-0.006049,
     # -0.000921, 0.004189, 0.008515, 0.011584, 0.013139, 0.013122,
     #  0.010945, 0.007546, 0.002767,-0.003181,-0.010089,-0.017528,
     # -0.025476,-0.033609,-0.042434,-0.050525,-0.058192,-0.065221,
     # -0.071410,-0.076638,-0.080655,-0.083349,-0.084584,-0.084203,
     # -0.082089,-0.078126,-0.072165,-0.064188,-0.054059,-0.041633,
     # -0.027005,-0.010188/
      DATA HALF2           / 0.0     , 0.004614, 0.009159, 0.013553,
     #  0.017753, 0.021764, 0.025834, 0.030406, 0.035710, 0.041730,
     #  0.048321, 0.055309, 0.062538, 0.069873, 0.077334, 0.084682,
     #  0.091926, 0.099028, 0.105967, 0.112695, 0.119245, 0.106551,
     #  0.095488, 0.085671, 0.076990, 0.069290, 0.062443, 0.056349,
     #  0.050908, 0.046047, 0.041702, 0.037815, 0.034339, 0.031229,
     #  0.028445, 0.025964, 0.023753, 0.021782, 0.020043, 0.018528,
     #  0.0     , 0.0     ,-0.000001,-0.000004,-0.000021,-0.000075,
     # -0.000190,-0.000326,-0.000317, 0.000116, 0.000434, 0.000116,
     # -0.000464,-0.000981,-0.001165,-0.000743, 0.000435, 0.002479,
     #  0.005462, 0.009353, 0.014206, 0.019964, 0.026829, 0.034307,
     #  0.042495, 0.051293, 0.060593, 0.070324, 0.080332, 0.090532,
     #  0.100831, 0.111114, 0.121283, 0.131245, 0.140853, 0.150120,
     #  0.158901, 0.167085, 0.174721, 0.181994/
C
C     + + + END SPECIFICATIONS + + +
C
      S=SK
      K=1
      FLAG = 0
      ERRFLG = 0
      I = 1
C     loop from 2 to NROZ
 10   CONTINUE
        I = I + 1
        IF (SA(I) .GT. S) FLAG = 1
        K = I - 1
      IF (I.LT.NROZ .AND. FLAG .EQ. 0) GO TO 10
C
      IF (FLAG .EQ. 0) THEN
        ERRFLG = 1
        DO 80 I=1,4
          ROW(I)=TABLE(NROZ,I)
 80     CONTINUE
      ELSE
        P=(S-SA(K))/(SA(K+1)-SA(K))
        Q=1.-P
        DO 190 I=2,4
          ROW(I)=Q*TABLE(K,I)+P*TABLE(K+1,I)
 190    CONTINUE
      END IF
C
      G=S+ROW(2)
      IF(S.GT.1.)G=G-.063*(S-1.)**1.85
      TOG=2./S
      Q=TOG
      IF(Q.LT..4)Q=.4
      A=Q+ROW(3)
      Q=.12*(S-2.25)
      IF(Q.LT.0.)Q=0.
      B=1.+Q*Q+ROW(4)
      IF ((B-TOG/A) .LT. 0.0) STOP 'WILFRS'
      H=(B-TOG/A)**.3333333
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   STUTP
     I                        (X,NDF)
C
C     + + + PURPOSE + + +
C     STUDENT T PROBABILITY
C     STUTP = PROB( STUDENT T WITH N DEG FR  .LT.  X )
C     NOTE  -  PROB(ABS(T).GT.X) = 2.*STUTP(-X,N) (FOR X .GT. 0.)
C     SUBPGM USED - GAUSCF
C     REF - G.W. HILL, ACM ALGOR 395, OCTOBER 1970.
C     USGS - WK 12/79.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDF
      REAL      X
C
C     + + + ARGUMENT DEFINITIONS + + +
C     X      - ?????
C     NDF    - number of degrees of freedom
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J,NN
      REAL      T, Y, Z, B, RHPI, A
C
C     + + + FUNCTIONS + + +
      REAL   GAUSCF
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG, SQRT, ABS, ATAN
C
C     + + + EXTERNALS + + +
      EXTERNAL   GAUSCF
C
C     + + + DATA INITIALIZATION + + +
      DATA RHPI / 0.63661977 /
C
C     + + + END SPECIFICATIONS + + +
C
      STUTP  =  .5
      IF (NDF .GE. 1) THEN
C
        NN = NDF
        Z = 1.
        T = X**2
        Y = T/NN
        B = 1.0 + Y
C
        IF(NN.GE.20 .AND. T.LT.NN .OR. NN.GT.200) THEN
C         ( OR IF NN NON-INTEGER)
C         ASYMPTOTIC SERIES FOR LARGE OR NONINTEGER N
          IF (Y.GT.1E-6) Y = ALOG(B)
          A = NN - 0.5
          B = 48.*A**2
          Y = A*Y
          Y = (((((-0.4*Y-3.3)*Y-24.)*Y-85.5)/
     #        (0.8*Y**2+100.+B)+Y+3.)/B+1.)*SQRT(Y)
          STUTP = GAUSCF(-Y)
          IF(X.GT.0.) STUTP = 1.-STUTP
        ELSE
C
          IF(NN.LT.20 .AND. T.LT.4.) THEN
C           NESTED SUMMATION OF COSINE SERIES
            Y = SQRT(Y)
            A = Y
            IF(NN.EQ. 1) A = 0.
          ELSE
C
C           TAIL SERIES FOR LARGE T
            A = SQRT(B)
            Y = A*NN
            J = 0
 30         CONTINUE
              J = J + 2
              IF (ABS(A-Z) .LE. 0.0) GO TO 40
              Z = A
              Y = Y*(J-1)/(B*J)
              A = A + Y/(NN+J)
            GO TO 30
 40         CONTINUE
            NN = NN + 2
            Z = 0.
            Y = 0.
            A = -A
          END IF
C
 110      CONTINUE
            NN = NN - 2
            IF (NN.GT.1) A = (NN-1)/(B*NN)*A + Y
          IF (NN .GT. 1) GO TO 110
C
          IF (NN.EQ.0) THEN
            A = A/SQRT(B)
          ELSE
            A = (ATAN(Y)+A/B)*RHPI
          END IF
C
          STUTP  = 0.5*(Z-A)
          IF(X.GT.0.) STUTP = 1.-STUTP
        END IF
      END IF
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   HARTP1
     I                        (SHK, V)
C
C     + + + PURPOSE + + +
C     This function looks up the SHK-th quantile (standardized deviate
C     with non-exceedence probability p) of the standardized Pearson
C     Type III distribution tabulated in the vector V.  The vector V
C     must have been filled by a prior call to HARTIV.  The value of
C     P must be between 0.0001 and 0.9999.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   SHK, V(31)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SHK    - quantile of Pearson Type III distribution
C     V      - vector of skew-interpolated Harter K values from HARTIV
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IB, IE, I
      REAL PROB(31), DV, HARTP
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + DATA INITIALIZATIONS + + +
C     HARTER TABULAR PROBABILITIES OR  GAUSSIAN DEVIATES -------------
      DATA PROB     /   0.00010,
     #   0.00050,  0.00100,  0.00200,  0.00500,  0.01000,  0.02000,
     #   0.02500,  0.04000,  0.05000,  0.10000,  0.20000,  0.30000,
     #   0.40000,  0.429624, 0.50000,  0.570376, 0.60000,  0.70000,
     #   0.80000,  0.90000,  0.95000,  0.96000,  0.97500,  0.98000,
     #   0.99000,  0.99500,  0.99800,  0.99900,  0.99950,  0.99990 /
C
C     + + + END SPECIFICATIONS + + +
C     G387 - LOOK UP K OR P IN HARTERS TABLES.  WKIRBY 9/76.
C     HARTK  - LOOKUP STDIZED HARTER K AT CUMULATIVE PROB P
C     HARTP  - LOOKUP CUM (NONEXCEED) PROB P AT STDIZED HARTER K
C     V      - VECTOR OF SKEW-INTERPOLATED HARTER K VALUES (FROM HARTIV)
C     6/78 WK -- USING LOCAL NOT HARTAB COPY OF HARTER TAB PROBS/GAUSS DEV.
C
      IF (SHK.GT.V(31).OR.SHK.LT.V(1)) THEN
        HARTP=1.
        IF (SHK.LT.V(1))HARTP=0.
      ELSE
        IB=2
        IF (SHK.GE.V(16))IB=17
        IE=IB+13
        DO 10 I=IB,IE
          IF (SHK.LT.V(I)) GO TO 20
 10     CONTINUE
        I=IB+14
 20     DV=V(I)-V(I-1)
        IF (ABS(DV) .GT. 1.0E-20) THEN
          HARTP=PROB(I-1)+(SHK-V(I-1))*(PROB(I)-PROB(I-1))/DV
        ELSE
          HARTP=PROB(I)
        END IF
        IF (ABS(V(1)-V(31)) .LT. 1.0E-20) THEN
          HARTP=1.
          IF (SHK.LT.V(1)) HARTP=0.
        END IF
      END IF
C
      HARTP1 = HARTP
C
      RETURN
      END
C
C
C
      REAL FUNCTION   HARTAK
     I                       (P,V)
C
C     + + + PURPOSE + + +
C     REV 11/85 WK FOR WATSTORE PGM A193 -- TO USE OLD 27-QUANTILE VERSION
C     VERSION OF HARTERS TABLES INCLUDED IN A193.  USED IN COND.PROB
C     ADJ. IN
C
C     G387 - LOOK UP K OR P IN HARTERS TABLES.  WKIRBY 9/76.
C     HARTAK  - LOOKUP STDIZED HARTER K AT CUMULATIVE PROB P
C     HARTAP  - LOOKUP CUM (NONEXCEED) PROB P AT STDIZED HARTER K
C     V       - VECTOR OF SKEW-INTERPOLATED HARTER K VALUES (FROM
C               HARTIV)
C     6/78 WK -- USUNG LOCAL NOT HARTAB COPY OF HARTER TAB PROBS/GAUSS
C                DEV.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL  P, V(27)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     P      - ?????
C     V      - ?????
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, IE, IB
      REAL      HUGE, PROB(27)
C
C  HARTER TABULAR PROBABILITIES (OLD 27-QUANTILE VERSION)  --------
C
C     + + + DATA INITIALIZATIONS + + +
      DATA PROB     /   0.00010,
     #   0.00050,  0.00100,            0.00500,  0.01000,  0.02000,
     #   0.02500,  0.04000,  0.05000,  0.10000,  0.20000,  0.30000,
     #   0.40000,            0.50000,            0.60000,  0.70000,
     #   0.80000,  0.90000,  0.95000,  0.96000,  0.97500,  0.98000,
     #   0.99000,  0.99500,            0.99900,  0.99950,  0.99990 /
      DATA HUGE / 31.0 /
C
C     + + + END SPECIFICATIONS + + +
C
      IF(P.GT.PROB(27) .OR. P.LT.PROB(1)) GO TO 90
      IB=2
      IF(P.GE.0.5)IB=15
      IE=IB+11
      DO10I=IB,IE
      IF(P.LT.PROB(I)) GO TO 20
   10 CONTINUE
      I=IB+12
   20 HARTAK=V(I-1)+(P-PROB(I-1))*(V(I)-V(I-1))/(PROB(I)-PROB(I-1))
      RETURN
C     RETURN + OR - INFINITY (HUGE) IF OUT OF RANGE OF PROB
   90 HARTAK=HUGE
      IF(P.LT.PROB(1))HARTAK=-HUGE
      RETURN
      END
C
C
C
      REAL FUNCTION   HARTAP
     I                       (SHK, V)
C
C     + + + PURPOSE + + +
C  REV 11/85 WK FOR WATSTORE PGM A193 -- TO USE OLD 27-QUANTILE VERSION
C        OF HARTERS TABLES INCLUDED IN A193.   USED IN COND.PROB ADJ. IN
C
C  G387 - LOOK UP K OR P IN HARTERS TABLES.  WKIRBY 9/76.
C  HARTAK  - LOOKUP STDIZED HARTER K AT CUMULATIVE PROB P
C  HARTAP  - LOOKUP CUM (NONEXCEED) PROB P AT STDIZED HARTER K
C  V      - VECTOR OF SKEW-INTERPOLATED HARTER K VALUES (FROM HARTIV)
C  6/78 WK -- USING LOCAL NOT HARTAB COPY OF HARTER TAB PROBS/GAUSS DEV.
C  HARTER TABULAR PROBABILITIES (OLD 27-QUANTILE VERSION)  --------
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   V(27), SHK
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SHK    -
C     V      -
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, IE, IB
      REAL      DV, PROB(27)
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + DATA INITIALIZATION + + +
      DATA PROB     /   0.00010,
     #   0.00050,  0.00100,            0.00500,  0.01000,  0.02000,
     #   0.02500,  0.04000,  0.05000,  0.10000,  0.20000,  0.30000,
     #   0.40000,            0.50000,            0.60000,  0.70000,
     #   0.80000,  0.90000,  0.95000,  0.96000,  0.97500,  0.98000,
     #   0.99000,  0.99500,            0.99900,  0.99950,  0.99990 /
C
C     + + + END SPECIFICATIONS + + +
C
      IF(SHK.GT.V(27).OR.SHK.LT.V(1)) GO TO 190
      IB=2
      IF(SHK.GE.V(16))IB=15
      IE=IB+11
      DO110I=IB,IE
      IF(SHK.LT.V(I)) GO TO 120
  110 CONTINUE
      I=IB+12
  120 DV=V(I)-V(I-1)
      IF(ABS(DV).LE.0.) GO TO 180
      HARTAP=PROB(I-1)+(SHK-V(I-1))*(PROB(I)-PROB(I-1))/DV
      RETURN
  180 HARTAP=PROB(I)
      IF(ABS(V(1)-V(27)).GT.0.)      RETURN
  190 HARTAP=1.
      IF(SHK.LT.V(1))HARTAP=0.
      RETURN
      END
C
C
C
      SUBROUTINE   STVRSN
C
C     + + + PURPOSE + + +
C     Dummy routine to include unix what version information for the
C     stats library.
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*64  VERSN
C
C     + + + END SPECIFICATIONS + + +
C
      INCLUDE 'fversn.inc'
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   HARTP 
     I                        (SHK, V)
C
C     + + + PURPOSE + + +
C     This function looks up the SHK-th quantile (standardized deviate
C     with non-exceedence probability p) of the standardized Pearson
C     Type III distribution tabulated in the vector V.  The vector V
C     must have been filled by a prior call to HARTIV.  The value of
C     P must be between 0.0001 and 0.9999.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   SHK, V(31)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SHK    - quantile of Pearson Type III distribution
C     V      - vector of skew-interpolated Harter K values from HARTIV
C
C     + + + LOCAL VARIABLES + + + 
      REAL TMP
C
C     + + + FUNCTIONS + + +
      REAL   HARTP2
C  
C     + + + EXTERNALS + + + 
      EXTERNAL   HARTP2
C
C     + + + END SPECIFICATIONS + + + 
C
      TMP   = HARTP2 (SHK, V)
      HARTP = TMP
C
      RETURN
      END
C
C
C
      REAL FUNCTION HARTP2 (SHK,V)
C
C     + + + PURPOSE + + +
C     This function looks up the P-th quantile (standardized deviate
C     with non-exceedence probability p) of the standardized Pearson
C     Type III distribution tabulated in the vector V.  The vector V
C     must have been filled by a prior call to HARTIV.  The value of
C     P must be between 0.0001 and 0.9999.
C
C     HARTK2 /HARTP2 -- Look up K or P in Harter tables using probability-
C                    scale interpolation in P (rather than simple linear
C                    as in HARTK/P).  WK 7/90. AML 12/93 (coding convention)
C     Based on HARTK routine.  The table of tabular probabilities in HARTK
C     is replaced by corresponding STD Normal deviates.   The input
C     probability
C     PA IN HARTK IS CONVERTED TO STD NORMAL DEVIATE BEFORE LINEAR INTERPOLATION
C     IS DONE.   IN HARTK2, LINEAR INTERPOLATION WITH RESPECT TO K-VALUES
C     YIELDS A STD NORMAL DEVIATE VALUE, WHICH IS CONVERTED TO PROBABILITY
C     FOR RETURN AS FUNCTION VALUE.
C
C     G387 - LOOK UP K OR P IN HARTERS TABLES.  WKIRBY 9/76.
C     HARTK  - LOOKUP STDIZED HARTER K AT CUMULATIVE PROB P
C     HARTP  - LOOKUP CUM (NONEXCEED) PROB P AT STDIZED HARTER K
C     V      - VECTOR OF SKEW-INTERPOLATED HARTER K VALUES (FROM HARTIV)
C     6/78 WK -- USING LOCAL NOT HARTAB COPY OF HARTER TAB PROBS/GAUSS DEV.
C
C     + + + DUMMY ARGUMENTS + + + 
      REAL   V(31), SHK
C
C     + + + ARGUMENT DEFINITION + + +
C     SHK    - quantile of Pearson Type III distribution
C     V      - vector of skew-interpolated Harter K values (from HARTIV)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, IB, IE
      REAL      PROB(31), HUGE, HARTP, DV  
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + FUNCTIONS + + +
      REAL  GAUSCF 
C
C     + + + EXTERNALS + + + 
      EXTERNAL   GAUSCF
C
C     + + + DATA INITIALIZATIONS + + + 
C     HARTER TABULAR PROBABILITIES OR  GAUSSIAN DEVIATES -------------
      DATA PROB /   -3.71902,
     #  -3.29053, -3.09023, -2.87816, -2.57583, -2.32635, -2.05375,
     #  -1.95996, -1.75069, -1.64485, -1.28155, -0.84162, -0.52440,
     #  -0.25335, -0.17733,  0.0    ,  0.17733,  0.25335,  0.52440,
     #   0.84162,  1.28155,  1.64485,  1.75069,  1.95996,  2.05375,
     #   2.32635,  2.57583,  2.87816,  3.09023,  3.29053,  3.71902 /
Caml  E37 changed to E29 for 5/94 compiler
      DATA HUGE / 1E29/
C
C     + + + END SPECIFICATIONS + + + 
C
      IF(SHK.LE.V(31).AND.SHK.GE.V(1)) THEN   
        IB=2
        IF(SHK.GE.V(16))IB=17
        IE=IB+13
        DO 110 I=IB,IE
          IF(SHK.LT.V(I)) GOTO120
 110    CONTINUE
        I=IB+14
 120    CONTINUE
        DV=V(I)-V(I-1)
        IF(ABS(DV).GT.0.) THEN      
          HARTP=PROB(I-1)+(SHK-V(I-1))*(PROB(I)-PROB(I-1))/DV
          HARTP2 = GAUSCF(HARTP)
        ELSE       
          HARTP=PROB(I)
          HARTP2 = GAUSCF(HARTP)
          IF(ABS(V(1)-V(31)) .GT. 0.)  THEN        
            HARTP2=1.
            IF(SHK.LT.V(1))HARTP2=0.
          END IF
        END IF
      ELSE
        HARTP2=1.
        IF(SHK.LT.V(1))HARTP2=0.
      END IF
C
      RETURN
      END

C
C
C
      SUBROUTINE   HARTIV
     I                    (SKU,NINT,PROB,
     O                     PD)
C
C     + + + PURPOSE + + +
C     Uses EMA module KFXX to calculate output percentage point array,
C     previously done through Harter's Table.
C     Skew must not exceed 9.0.
C     Updated 12/04 for batch version of PEAKFQ,
C     by Paul Hummel, AQUA TERRA Consultants
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NINT
      REAL      SKU, PROB(*), PD(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NINT   - number of probability intervals
C     SKU    - skew coefficient
C     PROB   - array of probabilities
C     PD     - array of percentage points
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
      DOUBLE PRECISION  LSKU,LPROB,LPD
C
C     + + + FUNCTIONS + + +
      DOUBLE PRECISION  KFXX
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS, DBLE
C
C     + + + EXTERNALS + + +
      EXTERNAL  KFXX
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ABS(SKU).GT.9.0) THEN
        DO 10 I = 1,NINT
          PD(I) = 0.0
 10     CONTINUE
      ELSE !calculate output vector
        LSKU = DBLE(SKU)
        DO 20 I = 1,NINT
          LPROB = 1.D0 - DBLE(PROB(I))
          LPD   = KFXX(LSKU,LPROB)
          PD(I) = LPD
 20     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   HARTK1
     I                        (P, V)
C
C     + + + PURPOSE + + +
C     This function looks up the P-th quantile (standardized deviate
C     with non-exceedence probability p) of the standardized Pearson
C     Type III distribution tabulated in the vector V.  The vector V
C     must have been filled by a prior call to HARTIV.  The value of
C     P must be between 0.0001 and 0.9999.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   P, V(31)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     P      - quantile of Pearson Type III distribution
C     V      - vector of skew-interpolated Harter K values from HARTIV
C
C     + + + LOCAL VARIABLES + + +
      REAL PROB(31), HUGE, HARTK
      INTEGER   IB, IE, I
C
C     + + + DATA INITIALIZATIONS + + +
C     HARTER TABULAR PROBABILITIES OR  GAUSSIAN DEVIATES -------------
      DATA PROB     /   0.00010,
     #   0.00050,  0.00100,  0.00200,  0.00500,  0.01000,  0.02000,
     #   0.02500,  0.04000,  0.05000,  0.10000,  0.20000,  0.30000,
     #   0.40000,  0.429624, 0.50000,  0.570376, 0.60000,  0.70000,
     #   0.80000,  0.90000,  0.95000,  0.96000,  0.97500,  0.98000,
     #   0.99000,  0.99500,  0.99800,  0.99900,  0.99950,  0.99990 /
      DATA HUGE / 1E29/
C
C     + + + END SPECIFICATIONS + + +
C
C     G387 - LOOK UP K OR P IN HARTERS TABLES.  WKIRBY 9/76.
C     HARTK  - LOOKUP STDIZED HARTER K AT CUMULATIVE PROB P
C     HARTP  - LOOKUP CUM (NONEXCEED) PROB P AT STDIZED HARTER K
C     V      - VECTOR OF SKEW-INTERPOLATED HARTER K VALUES (FROM HARTIV)
C     6/78 WK -- USING LOCAL NOT HARTAB COPY OF HARTER TAB PROBS/GAUSS DEV.
C
      IF (P.GT.PROB(31) .OR. P.LT.PROB(1)) THEN
C       RETURN + OR - 1E29 IF OUT OF RANGE OF PROB
        HARTK=HUGE
        IF (P.LT.PROB(1))HARTK=-HUGE
      ELSE
        IB=2
        IF (P.GE.0.5)IB=17
        IE=IB+13
        DO10I=IB,IE
          IF (P.LT.PROB(I))GOTO20
 10     CONTINUE
        I=IB+14
 20     HARTK=V(I-1)+(P-PROB(I-1))*(V(I)-V(I-1))/(PROB(I)-PROB(I-1))
      END IF
C
      HARTK1 = HARTK
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   HARTRG
     I                         (R)
C
C     + + + PURPOSE + + +
C     This function returns the value of the skew coefficient
C     corresponding to the given value of the 2-10-100 quantile
C     ratio, QR = (X100-X10)/(X10-X2), in which Xt is the t-yr
C     quantile of a Pearson Type III distribution.  The results
C     are obtained from emperical ploynomial and semi-log formulas
C     that correlate skew-values and quantile-ratios from Harter's
C     tables.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   R
C
C     + + + ARGUMENT DEFINITIONS + + +
C     R      - ratio (X100-X10)/(X10-X2)
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG10, SQRT
C
C     + + + END SPECIFICATIONS + + +
C      COMPUTES SKEW COEFF OF PEARSON TYPE III  DISTN,
C      GIVEN THE RATIO (Q.100 - Q.10)/(Q.10 - Q.2),
C      WHERE Q.T IS THE T-YEAR (1.-1/T - PROBABILITY)
C      QUANTILE.   THE EQUATIONS WERE FOUND BY POLYNOMIAL
C      REGRESSION, ETC., OF SKEW VS RATIO, WHERE THE RATIOS
C      WERE LOOKED UP IN HARTER'S TABLES FOR GIVEN SKEWS.
C         WK 11/80.  FOR WRC BULL 17-B.
C
      IF (R.LT.0.243) THEN
        HARTRG = -4.8
        IF (R.GT.0.) HARTRG = -6.0 + 10.**(0.72609+0.15397*ALOG10(R))
      ELSE IF (R.GT.1.6) THEN
        HARTRG = 7.1+1.6*(R-2.4)-1.4*SQRT((R-2.4)**2+5.1888)
      ELSE
        HARTRG = -2.51898 + R*(3.82069 + R*(-2.31960 +
     $           R*(2.35713 + R*(-0.73870))))
      END IF
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   OUTKGB
     I                           (SIG, N)
C
C     This function returns the value of the Grubbs-Beck (1972)
C     one-sided single-outlier criterion for narmal samples of size N at
C     significance level SIG.  Sample size N must be at least 3.  Available
C     Available significane levels are 1.0, 2.5, 5.0, and 10.0 percent.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   SIG
      INTEGER   N
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SIG    - significance level
C     N      - sample size
C
C     + + + SAVE VARIABLES + + +
      SAVE    OUTG10,OUTG5,OUTG25,OUTG1
      REAL    OUTG10(100),OUTG5(100),OUTG25(100),OUTG1(100)
C
C     + + + LOCAL VARIABLES + + +
      REAL    OUTG(100,4), TAIL(2,4), SIGG
      INTEGER   NN, ISIG
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE(OUTG10(1),OUTG(1,1)),(OUTG5(1),OUTG(1,2)),
     $         (OUTG25(1),OUTG(1,3)), (OUTG1(1),OUTG(1,4))
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, MAX0, MIN0
C
C     + + + DATA INITIALIZATIONS + + +
      DATA OUTG10/       2*-1E29,
     $1.148,1.425,1.602,1.729,1.828,1.909,1.977,            2.036,2.088,
     12.134,2.175,2.213,2.247,2.279,2.309,2.335,2.361,2.385,2.408,2.429,
     22.448,2.467,2.486,2.502,2.519,2.534,2.549,2.563,2.577,2.591,2.604,
     32.616,2.628,2.639,2.650,2.661,2.671,2.682,2.692,2.700,2.710,2.719,
     42.727,2.736,2.744,2.753,2.760,2.768,2.775,2.783,2.790,2.798,2.804,
     52.811,2.818,2.824,2.831,2.837,2.842,2.849,2.854,2.860,2.866,2.871,
     62.877,2.883,2.888,2.893,2.897,2.903,2.908,2.912,2.917,2.922,2.927,
     72.931,2.935,2.940,2.945,2.949,2.953,2.957,2.961,2.966,2.970,2.973,
     82.977,2.981,2.984,2.989,2.993,2.996,3.000,3.003,3.006,3.011,3.014,
     93.017/
      DATA OUTG5 /   2*-1E29,
     $1.153,1.463,1.672,1.822,1.938,2.032,2.110,2.176,2.234,
     12.285,2.331,2.371,2.409,2.443,2.475,2.504,2.532,2.557,2.580,2.603,
     22.624,2.644,2.663,2.681,2.698,2.714,2.730,2.745,2.759,2.773,2.786,
     32.799,2.811,2.823,2.835,2.846,2.857,2.866,2.877,2.887,2.896,2.905,
     42.914,2.923,2.931,2.940,2.948,2.956,2.964,2.971,2.978,2.986,2.992,
     53.000,3.006,3.013,3.019,3.025,3.032,3.037,3.044,3.049,3.055,3.061,
     63.066,3.071,3.076,3.082,3.087,3.092,3.098,3.102,3.107,3.111,3.117,
     73.121,3.125,3.130,3.134,3.139,3.143,3.147,3.151,3.155,3.160,3.163,
     83.167,3.171,3.174,3.179,3.182,3.186,3.189,3.193,3.196,3.201,3.204,
     93.207/
      DATA OUTG25/       2*-1E29,
     $1.155,1.481,1.715,1.887,2.020,2.126,2.215,2.290,2.355,
     12.412,2.462,2.507,2.549,2.585,2.620,2.651,2.681,2.709,2.733,2.758,
     22.781,2.802,2.822,2.841,2.859,2.876,2.893,2.908,2.924,2.938,2.952,
     32.965,2.979,2.991,3.003,3.014,3.025,3.036,3.046,3.057,3.067,3.075,
     43.085,3.094,3.103,3.111,3.120,3.128,3.136,3.143,3.151,3.158,3.166,
     53.172,3.180,3.186,3.193,3.199,3.205,3.212,3.218,3.224,3.230,3.235,
     63.241,3.246,3.252,3.257,3.262,3.267,3.272,3.278,3.282,3.287,3.291,
     73.297,3.301,3.305,3.309,3.315,3.319,3.323,3.327,3.331,3.335,3.339,
     83.343,3.347,3.350,3.355,3.358,3.362,3.365,3.369,3.372,3.377,3.380,
     93.383/
      DATA OUTG1  / 2*-1E29,
     $1.155,1.492,1.749,1.944,2.097,2.221,2.323,2.410,2.485,2.550,2.607,
     $2.659,2.705,2.747,2.785,2.821,2.854,2.884,2.912,2.939,2.963,2.987,
     $3.009,3.029,3.049,3.068,3.085,3.103,3.119,3.135,3.150,3.164,3.178,
     $3.191,3.204,3.216,3.228,3.240,3.251,3.261,3.271,3.282,3.292,3.302,
     $3.310,3.319,3.329,3.336,3.345,3.353,3.361,3.368,3.376,3.383,3.391,
     $3.397,3.405,3.411,3.418,3.424,3.430,3.437,3.442,3.449,3.454,3.460,
     $3.466,3.471,3.476,3.482,3.487,3.492,3.496,3.502,3.507,3.511,3.516,
     $3.521,3.525,3.529,3.534,3.539,3.543,3.547,3.551,3.555,3.559,3.563,
     $3.567,3.570,3.575,3.579,3.582,3.586,3.589,3.593,3.597,3.600  /
      DATA TAIL /3.078,3.144,3.267,3.334,3.444,3.509,3.662,3.727/
C
C     HIGH-OUTLIER CRITERIA FOR NORMAL DTN.  - OUTKGB IS LOW-OUT CRIT.
C     OUTLIER TEST CRITERIA FROM GRUBBS-BECK 11/72 TECHNOMETRICS.
C     SAMPLE SIZES (3,1,100)  SIG LEVELS 10, 5, 2.5, 1  PCT (ONE-SIDED TEST
C     LINEAR INTERPOLATION-EXTRAP FOR N = 101-120-147-180.
C     SIG MAY BE GIVEN AS FRACTION OR PERCENT -- EG, 2.5 OR 0.025.
C     ORIGINAL TABLES FROM CRONSHEY, 7/77.  REVISED AND ENLARGED WK 5,6/78.
C
Caml  all E37's changed to E29's for 5/94 compiler
      SIGG = SIG
      IF (SIGG.GT..99) SIGG = 0.01*SIGG
      ISIG = 0
      IF ( ABS(SIGG -  .10  ) .LE. 0.001)  ISIG =  1
      IF ( ABS(SIGG -  .05  ) .LE. 0.001)  ISIG =  2
      IF ( ABS(SIGG -  .025 ) .LE. 0.001)  ISIG =  3
      IF (ABS(SIGG- .010) .LE. 0.001) ISIG = 4
      OUTKGB = -1E29
      IF (ISIG.NE.0) THEN
        IF (N.LE.120) THEN
          NN = MIN0(100, MAX0(1, N))
          OUTKGB = OUTG(NN,ISIG)
          IF (N.GT.100)OUTKGB=OUTKGB + .05*(N-100)*(TAIL(1,ISIG)-OUTKGB)
        ELSE
          NN = MIN0(N,180)
          OUTKGB=TAIL(1,ISIG)+(TAIL(2,ISIG)-TAIL(1,ISIG))*(NN-120)/27.
        END IF
      END IF
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   STUTX
     I                        (P, N)
C
C     + + + PURPOSE + + +
C     This function returns the P-th quantile of Student's t with n
C     degrees of freedom.  It solves for x  in the equation
C     P{tn < x} = p  for the given value of p.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   N
      REAL   P
C
C     + + + ARGUMENT DEFINITIONS + + +
C     P      - quantile of Student's t
C     N      - degrees of freedom
C
C     + + + LOCAL VARIABLES + + +
C     INTEGER   IER
      REAL   HPI, SIGN, Q, FN, A, B, C, D, X, Y
C
C     + + + FUNCTIONS + + +
      REAL   GAUSAB
C
C     + + + INTRINSICS + + +
      INTRINSIC   TAN, SQRT, EXP
C
C     + + + EXTERNALS + + +
      EXTERNAL   GAUSAB
C
C     + + + DATA INITIALIZATIONS + + +
      DATA HPI / 1.5707963268/
C
C     + + + END SPECIFICATIONS + + +
C     STUDENT T QUANTILES --
C     STUTX(P,N) = X SUCH THAT PROB(STUDENT T WITH N D.F. .LE. X) = P.
C     NOTE - ABS(T) HAS PROB Q OF EXCEEDING STUTX( 1.-Q/2., N ).
C     NOTE -      IER - ERROR FLAG --  1 = F.LT.1.,
C                                  2 = P NOT  IN (0,1),  3 = 1+2
C     SUBPGMS USED -- GAUSAB     (GAUSSIAN ABSCISSA)
C     REF - G. W. HILL (1970) ACM ALGO 396.  COMM ACM 13(10)619-20.
C            REV BY WKIRBY 10/76. 2/79.  10/79.
CAML  E38 changed to E29 for 5/94 compiler
C
      SIGN=1.
      IF (P.LT.0.5)SIGN=-1.
      Q=2.*P
      IF (Q.GT.1.)Q=2.*(1.-P)
      IF (Q .GE. 1.) THEN
        STUTX = 0.
      ELSE
        FN = N
        IF (N.GE.1 .AND. Q.GT.0. .AND. Q.LT.1.) THEN
          IF (N .EQ. 1) THEN
C           1 DEG FR - EXACT
            STUTX=SIGN/TAN(HPI*Q)
C
          ELSE IF (N.EQ.2) THEN
C           2 DEG FR - EXACT
            STUTX = SQRT(2.0/(Q*(2.0-Q))-2.0) * SIGN
C
          ELSE
C           EXPANSION FOR N .GT. 2
            A = 1.0/(FN-0.5)
            B= 48.0/(A*A)
            C=((20700.*A/B-98.)*A-16.)*A+96.36
            D =  ((94.5/(B+C)-3.)/B+1.)*SQRT(A*HPI)*FN
            X = D*Q
            Y = X**(2.0/FN)
            IF (Y .LE. A+.05) THEN
              Y = ((1.0/(((FN+6.)/(FN*Y)-0.089*D-0.822)*(FN+2.)*3.)+
     $             0.5/(FN+4.))*Y-1.)*(FN+1.)/(FN+2.)+1.0/Y
              STUTX = SQRT(FN*Y) * SIGN
C
            ELSE
C             ASYMPTOTIC INVERSE EXPANSION ABOUT NORMAL
              X = GAUSAB(0.5*Q)
              Y =  X*X
              IF (FN .LT. 5.) C = C+0.3*(FN-4.5)*(X+0.6)
              C = (((.05*D*X-5.)*X-7.)*X-2.)*X+B+C
              Y = (((((0.4*Y+6.3)* Y+36.)*Y+94.5)/C-Y-3.)/B+1.)*X
              X = A*Y**2
              Y = X + 0.5*X**2
              IF (X .GT. .002) Y = EXP(X) - 1.0
              STUTX = SQRT(FN*Y) * SIGN
            END IF
          END IF
        ELSE
C         IER never used,  needs to be checked
C         IER = 3
C         IF (N.GE.1) IER = 2
          STUTX = SIGN*1E29
        END IF
      END IF
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   HARTK
     I                        (P, SKU)
C
C     + + + PURPOSE + + +
C     Calculate Harter K value for a give probability and skew
C     using EMA module KFXX.  
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   P, SKU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     P      - quantile of Pearson Type III distribution
C     SKU    - skew
C
C     + + + LOCAL VARIABLES + + +
      DOUBLE PRECISION  LP,LSKU,TMP
C
C     + + + FUNCTIONS + + + 
      DOUBLE PRECISION  KFXX
C
C     + + + EXTERNALS + + +
      EXTERNAL   KFXX
C
C     + + + END SPECIFICATIONS + + + 
C
      IF (ABS(SKU).GT.9.0) THEN
        TMP = 0.0
      ELSE
        LP    = P
        LSKU  = SKU
c       write(*,*) 'HARTK: B4 KFXX-LSKU,LP',LSKU,LP
        TMP   = KFXX(LSKU,LP)
        IF (ABS(TMP).GT.1.0E30) THEN
          TMP = 1.0E30
        END IF
c       write(*,*) 'HARTK: after KFXX-TMP',TMP
      END IF

      HARTK = TMP
C
      RETURN
      END

      REAL FUNCTION HARTK2 (PA,V)
C
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
      REAL   V(31), PA
C
C     + + + ARGUMENT DEFINITION + + +
C     PA     - probability
C     V      - vector of skew-interpolated Harter K values (from HARTIV)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, IB, IE
      REAL      PROB(31), HUGE, HARTK, P
C
C     + + + FUNCTIONS + + +
      REAL  GAUSAB 
C
C     + + + EXTERNALS + + + 
      EXTERNAL   GAUSAB
C
C     + + + DATA INITIALIZATIONS + + + 
C     HARTER TABULAR PROBABILITIES OR  GAUSSIAN DEVIATES -------------
      DATA PROB /   -3.71902,
     #  -3.29053, -3.09023, -2.87816, -2.57583, -2.32635, -2.05375,
     #  -1.95996, -1.75069, -1.64485, -1.28155, -0.84162, -0.52440,
     #  -0.25335, -0.17733,  0.0    ,  0.17733,  0.25335,  0.52440,
     #   0.84162,  1.28155,  1.64485,  1.75069,  1.95996,  2.05375,
     #   2.32635,  2.57583,  2.87816,  3.09023,  3.29053,  3.71902 /
      DATA HUGE / 1E29/
C
C     + + + END SPECIFICATIONS + + + 
Caml  E37 changed to E29 for 5/94 compiler
C
      P = GAUSAB(PA)
      IF(P.LE.PROB(31).AND. P.GE.PROB(1)) THEN 
        IB=2
        IF(P.GE.0.5)IB=17
        IE=IB+13
        DO 10 I=IB,IE
          IF(P.LT.PROB(I)) GOTO20
 10     CONTINUE
        I=IB+14
 20     HARTK=V(I-1)+(P-PROB(I-1))*(V(I)-V(I-1))/(PROB(I)-PROB(I-1))
      ELSE
C       RETURN + OR - 1E29 IF OUT OF RANGE OF PROB
        HARTK=HUGE
        IF(P.LT.PROB(1))HARTK=-HUGE
      END IF
C
      HARTK2 = HARTK
C
      RETURN
      END

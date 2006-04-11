      SUBROUTINE CIRCLE(ALPHA,PS,DN,ID)
C=======================================================================
C     ROUTINE TO COMPUTE HYDRAULIC ELEMENTS OF CIRCULAR PIPE.
C                USES NEWTON-RAPHSON ITERATION TO FIND THETA.
C     PARABOLIC STARTING GUESS VALID ONLY FOR THETA < 1.3 RADIANS,
C                                        (ALPHA < 0.04, PSI < 0.015).
C     WEAKER STARTING GUESSES FOR HIGHER VALUES COULD CONCEIVABLY
C                                         CAUSE CONVERGENCE PROBLEMS.
C     UPDATED (NEW COMMON) BY W.C.H., SEPTEMBER 1981.
C=======================================================================
      INCLUDE 'TAPES.INC'
C=======================================================================
C     ALPHA = A/AFULL
C     PS    = PSI = Q/QFULL
C     DN    = DEPTH/DIAMETER
C     THETA = SUBTENDED ANGLE
C
C     ID = 1, GIVEN ALPHA, COMPUTE THETA THEN PSI.
C     ID = 2, GIVEN ALPHA, COMPUTE THETA THEN DN.
C     ID = 3, GIVEN PSI,   COMPUTE THETA THEN ALPHA.
C=======================================================================
      IF(ID.LE.2) THEN
      IF(ALPHA.LE.0.0.OR.ALPHA.GE.1.0) GO TO 200
      IF(ALPHA.LE.1.0E-5)              GO TO 400
C=======================================================================
C     FIRST CALCULATE THETA GIVEN ALPHA.
C     USE PARABOLIC APPROXIMATION FOR FIRST GUESS FOR ALPHA < 0.04
C=======================================================================
      THETA = 0.031715 - 12.79384 * ALPHA + 8.28479 * SQRT(ALPHA)
      IF(ALPHA.GT.0.04) THETA = 1.2 + 5.08 * (ALPHA - 0.04) / 0.96
      TH1 = THETA
      AP  = 6.283185 * ALPHA
C=======================================================================
      DO 20 K = 1,40
      D       = - (AP - THETA + SIN(THETA)) / (1 - COS(THETA))
      THETA   = THETA - D
      IF(ABS(D).LE.0.0001) GO TO 40
   20 CONTINUE
C=======================================================================
C     IF NO CONVERGENCE, USE FIRST GUESS.
C=======================================================================
      THETA = TH1
      WRITE(N6,35) ID,ALPHA,THETA
C=======================================================================
C     CALCULATE PSI.
C=======================================================================
   40 IF(ID.NE.2) THEN
                  PS = (THETA - SIN(THETA)) ** 1.666667 /
     +                 6.283185 / THETA ** .66667
C=======================================================================
C                 CALCULATE NORMALIZED DEPTH.
C=======================================================================
                  ELSE
                  DN = (1.0 - COS(THETA / 2.0)) / 2.0
                  ENDIF
C=======================================================================
C     END OF ID LE 2
C=======================================================================
      RETURN
      ENDIF
C=======================================================================
C     START OF ID EQ 3
C=======================================================================
      IF(PS.LE.0.0.OR.PS.GE.1.0) GO TO 300
      IF(PS.LE.1.0E-6)           GO TO 500
C=======================================================================
C        FIRST COMPUTE THETA GIVEN PSI.
C        USE PARABOLIC APPROXIMATION FOR FIRST GUESS FOR PS < 0.015
C=======================================================================
      THETA = 0.12103 - 55.5075 * PS + 15.62254 * SQRT(PS)
      IF(PS.GT.0.015) THETA = 1.2 + 1.94 * (PS - 0.015) / 0.485
      IF(PS.GT.0.5)   THETA = 3.14 + 1.03 * (PS - 0.5) / 0.4
      IF(PS.GT.0.90)  THETA = 4.17 + 1.12 * (PS - 0.90) / 0.176
      TH1   = THETA
      AP    = 6.283185 * PS
C=======================================================================
      DO 120 K = 1,40
      THETA    = ABS(THETA)
      TT       = THETA - SIN(THETA)
      TT23     = TT ** 0.666667
      T3       = THETA ** 0.333333
      D        = AP * THETA / T3 - TT * TT23
      D        = D/(AP*.666667/T3-1.666667*TT23*(1.0-COS(THETA)))
      THETA    = THETA - D
      IF(ABS(D).LE.0.0001) GO TO 140
 120  CONTINUE
C=======================================================================
C     IF NO CONVERGENCE, USE FIRST GUESS.
C=======================================================================
      THETA = TH1
      WRITE(N6,35) ID,PS,THETA
C=======================================================================
C     CALCULATE ALPHA
C=======================================================================
  140 ALPHA = (THETA - SIN(THETA)) / 6.283185
      RETURN
C=======================================================================
  200 IF(ALPHA.GE.1.0) THEN
                       PS    = 1.0
                       DN    = 1.0
                       ELSE
                       PS    = 0.0
                       DN    = 0.0
                       ENDIF
      RETURN
C=======================================================================
  300 IF(PS.GE.1.0) THEN
                    ALPHA = 1.0
                    ELSE
                    ALPHA = 0.0
                    ENDIF
      RETURN
C=======================================================================
C     FOR SMALL ALPHA (HENCE THETA), USE EXPANSION FOR SIN & COSINE.
C=======================================================================
  400 THETA          = (37.69911 * ALPHA) ** 0.33333
      IF(ID.EQ.1) PS = THETA ** 4.33333 / 124.4797
      IF(ID.EQ.2) DN = THETA ** 2 / 16.0
      RETURN
C=======================================================================
C     FOR SMALL PS (HENCE THETA), USE EXPANSION FOR SIN & COSINE.
C=======================================================================
  500 THETA = (124.4797 * PS) ** 0.2307692
      ALPHA = THETA ** 3 / 37.69911
      RETURN
C=======================================================================
   35 FORMAT(/,' ===>  WARNING !! NO CONVERGENCE IN SUBROUTINE CIRCLE.
     1. ID =',I8,'   ALPHA OR PSI =',1PE15.7,'   THETA1 =',1PE15.7,/)
C=======================================================================
      END

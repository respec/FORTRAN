C
C
C
      SUBROUTINE   LGPSTX
     I                    ( N, NZI, NUMONS, NQS, XBAR, STD, SKEW,
     I                      LOGARH, ILH, DBG,
     M                      SE, 
     O                      C, CCPA, P, Q, ADP, QNEW, RI,
     O                      RSOUT, RETCOD )
C
C      dll_export LGPSTX
CDEC$ ATTRIBUTES DLLEXPORT :: LGPSTX
C
C     + + + PURPOSE + + +
C     Computes probabilities.  Does conditional probablility
C     adjustment if there are zero events.  Computes flow
C     statistics for selected recurrence intervals.
C
C     + + + HISTORY + + +
C     wrk 01/12/10  correct for problem at lower tail where a negaitve
C                   value may be computed in some instances.
C     wrk 01/12/12  add computation for 0.3333 recurrence interval.
C
C     + + + DUMMY ARGUMENTS + + +
      LOGICAL   DBG
      INTEGER   N, NZI, LOGARH, ILH, NUMONS, NQS, RETCOD
      REAL      SKEW, XBAR, STD, SE(NQS), C(NQS), CCPA(NQS),
     $          ADP(NQS), Q(NQS), QNEW(NQS), P(NQS), RI(NQS), RSOUT(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     N      - number of years
C     NZI    - number of years of zero events
C     NUMONS - number of number of months for statistic
C     NQS    - number of statistics
C              12 - for monthly statistic
C              11 - for high or low statistic
C     XBAR   - mean
C     STD    - standard deviation
C     SKEW   - skewness
C     LOGARH - flag for log transformation (base 10)
C              1 - yes
C              2 - no
C     ILH    - flag for statistics option
C              1 - n-day high flow
C              2 - n-day low flow
C              3 - month
C     SE     - probabilities associated with C flows
C              exceedance if ILH = 1
C              non-exceedance otherwise
C     C      - flow characteristics associated with SE
C     CCPA   - if NZI > 0, then flow characterisitcs C with
C              conditional probablility adjustment,
C              otherwise undefined.
C     P      - exceedance (ILH=1) or non-exceedance (ILH>1)
C              probablility
C     Q      - parameter value
C     ADP    - if NZI > 0, adjusted exceedance (ILH=1) or
C              non-exceedance probablility (ILH>1),
C              otherwise undefined
C     QNEW   - if NZI > 0, adjusted parameter value,
C              otherwise undefined
C     RI     - recurrence interval
C     RSOUT  - recurrence intervals (1:NQS) and parameter
C              values (NQS:*), not adjusted for zero events
C     RETCOD - return code
C              -31 - skew out of range (< -3.3 or > 3.3)
C              -32 - error in interpolation routine
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IK, NTOP, NTOT, I
      REAL      QCPA(100), T, TZI, TNI, PX, PNX,LKF
      DOUBLE PRECISION LSKEW,LPROB 
C
C     + + + FUNCTIONS + + +
      DOUBLE PRECISION KF
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, REAL, INT, DBLE
C
C     + + + EXTERNALS + + +
      EXTERNAL   PA193X, CALQPX, KF
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DBG) THEN 
        OPEN(UNIT=98,FILE="C:\TEST\USGS_SWSTATS_ERROR.FIL",
     1       ACCESS="APPEND")
        WRITE(98,*) "IntelFortranVersion"
        WRITE(98,*) "LGPSTX:entry:NQS:", NQS
      END IF
C
C     initialize variables
      RETCOD = 0
C
      IF (DBG) THEN
        WRITE(98,*) "LOGARH,RETCOD:",LOGARH,RETCOD
        DO 320 I= 1, NQS
          WRITE(98,*) "LGPSTX:I,SE:",I,SE(I)
 320    CONTINUE          
      END IF      
C
      IF (RETCOD .EQ. 0) THEN
        LSKEW = DBLE(SKEW)
        IF (DBG) WRITE(98,*) "LGPSTX:XBAR,STD,SKEW", XBAR, STD, SKEW
        DO 345 I=1,NQS
          LPROB = 1.D0 - DBLE(SE(I))
          LKF = KF(LSKEW,LPROB)
          C(I)= XBAR+ (LKF* STD)
C         Dec 01 - correct for problem at lower tail for negative values
          IF (LOGARH .EQ. 2  .AND.  C(I) .LT. 0.0) C(I) = 0.0
          IF (DBG) WRITE(98,*) "LGPSTX:I:",I,SE(I),C(I),RETCOD
 345    CONTINUE
        IF (DBG) WRITE(98,*) "LGPSTX:done 345",RETCOD
        NTOP=0
        NTOT=N + NZI
        IF (NZI.GT.0) THEN
          IF (DBG) WRITE(98,*) "LGPSTX:conditional prob adj",RETCOD
C         zero events, conditional probability adjustment
          CALL PA193X (C,NTOT,NZI,NTOP,NQS,SE,XBAR,STD,LSKEW,
     O                 CCPA)
          IF (DBG) WRITE(98,*) "LGPSTX:done PA193X",RETCOD
          DO 350 I=1,NQS
C           Dec 01 - first, correct for problem at lower tail for neg values
            IF (LOGARH .EQ. 2  .AND.  CCPA(I) .LT. 0) CCPA(I) = 0.0
            IF (ABS(CCPA(I)+31.0) .GT. 0.001) THEN
C             This logic is to change a very small number as
C             defined in the HARTAK routine used by CPA193
              IF (LOGARH .EQ. 1) THEN
                QCPA(I)=10.0**CCPA(I)
              ELSE
                QCPA(I) = CCPA(I)
              END IF
            ELSE
              QCPA(I)=0.0
            END IF
 350      CONTINUE
          IF (DBG) WRITE(98,*) "LGPSTX:done 350",RETCOD
        END IF
C
        IF (DBG) WRITE(98,*) "LGPSTX:compute flow stats",RETCOD
C       Compute flow statistics for selected recurrence intervals
        CALL CALQPX ( LOGARH, ILH, C, QCPA, NZI, NUMONS, NQS,
     M               SE,
     O               Q, QNEW, P )
        IF (DBG) WRITE(98,*) "LGPSTX:done compute flow stats",RETCOD
        DO 360 I = 1,NQS
          RI(I) = 1.0/P(I)
          RSOUT(I) = REAL(INT(100.0*RI(I)+0.01))/100.0
          RSOUT(I+NQS) = Q(I)
 360    CONTINUE
        IF (DBG) WRITE(98,*) "LGPSTX:done 360",RETCOD
        IF (NZI .GT. 0) THEN
C         zero flows
          T   = N
          TZI = NZI
          TNI = T + TZI
          IF (ILH .GT. 1) THEN
C           n-day low flow or month
            DO 365 I = 1,NQS
              ADP(I) = (T/TNI)*P(I) + TZI/TNI
 365        CONTINUE
            IF (DBG) WRITE(98,*) "LGPSTX:done 365",RETCOD
          ELSE
C           n-day high flows
            DO 370 I = 1,NQS
              ADP(I) = T/TNI*P(I)
 370        CONTINUE
            IF (DBG) WRITE(98,*) "LGPSTX:done 370",RETCOD
          END IF
        END IF
      END IF
      IF (DBG) THEN 
        WRITE(98,*) "LGPSTX:RETCOD:",RETCOD
        DO I = 1,NQS
          WRITE(98,*) "LGPSTX:", I,SE(I),RI(I),ADP(I),
     1                           RSOUT(I),RSOUT(I+NQS),
     2                           P(I),Q(I),QNEW(I)
        END DO
        CLOSE(UNIT=98,ERR=380)
 380    CONTINUE        
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CALQPX
     I                  (LOGARH,ILH,C,QCPA,NZI,NUMONS,NQS,
     M                   SE,
     O                   Q, QNEW,P)
C
C     + + + PURPOSE + + +
C     This routine computes statistics and probabilities for Pearson
C     Type III distribution based on pre-selected probabilities.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LOGARH, ILH, NZI, NUMONS, NQS
      REAL      C(NQS), QCPA(NQS), SE(NQS), Q(NQS), QNEW(NQS), P(NQS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LOGARH - log transformation flag, 1-yes, 2-no
C     ILH    - flag for statistic
C              1 - n-day high flow
C              2 - n-day low flow
C              3 - month
C     C      - statistics for selected recurrence intervals
C     QCPA   - statistics adjusted for zero flows
C     NZI    - number of years of zero events
C     NUMONS -
C     NQS    -
C     SE     - probabilities of selected recurrence intervals
C     Q      - statistics for each specified recurrence interval
C     QNEW   - statistic adjusted for zero flow
C     P      - probability of selected recurrence intervals
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ILH.EQ.2) THEN
C       low flow statistics
        IF (LOGARH .EQ. 1) THEN
          DO 10 I = 1,NQS
            Q(I) = 10.0**C(I)
 10       CONTINUE
        ELSE
          DO 20 I = 1,NQS
            Q(I) = C(I)
 20       CONTINUE
        END IF
        IF (NZI.GT.0) THEN
          DO 30 I = 1,NQS
            QNEW(I) = QCPA(I)
 30       CONTINUE
        END IF
        DO 35  I = 1,NQS
          SE(I) = 1.0-SE(I)
 35     CONTINUE
        DO 40 I = 1,NQS
          P(I) = SE(I)
 40     CONTINUE
      ELSE IF (ILH .EQ. 1) THEN
C       high flow statistics
        IF (LOGARH .EQ. 1) THEN
          DO 50 I = 1,NQS
            Q(I) = 10.0**C(I)
 50       CONTINUE
        ELSE
          DO 60 I = 1,NQS
            Q(I) = C(I)
 60       CONTINUE
        END IF
        IF (NZI.GT.0) THEN
          DO 70 I = 1,NQS
            QNEW(I) = QCPA(I)
 70       CONTINUE
        END IF
        DO 80 I = 1,NQS
          P(I) = SE(I)
 80     CONTINUE
      ELSE
C       for month statistics
        IF (LOGARH .EQ. 1) THEN
          DO 110 I = 1,NQS
            Q(I) = 10.0**C(I)
 110      CONTINUE
        ELSE
          DO 120 I = 1,NQS
            Q(I) = C(I)
 120      CONTINUE
        END IF
        IF (NZI.GT.0) THEN
          DO 130 I = 1,NQS
            QNEW(I) = QCPA(I)
 130      CONTINUE
        END IF
        DO 135  I = 1,NQS
          SE(I) = 1.0-SE(I)
 135    CONTINUE
        DO 140 I = 1,NQS
          P(I) = SE(I)
 140    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PA193X
     I                  (XIN,N,N0,N1,NQS,PROB,XBAR,STD,LSKEW,
     O                   XOUT)
C
C     + + + PURPOSE + + +
C  CONDITIONAL PROBABILITY ADJUSTMENT (CPA) -- VERSION A193 (WATSTORE)
C
C  NOTE -- USES OLD-STYLE HARTER TABLES WITH 27 QUANTILES. (A193)
C
C  CONVERTS A CONDITIONAL PEARSON TYPE III DISTRIBUTION BASED ON
C  A CENSORED SAMPLE (WITH N0,N1 ITEMS CENSORED FROM
C  BOTTOM AND TOP OF A SAMPLE OF TOTAL SIZE N) INTO AN EQUIVALENT
C  UNCONDITIONAL DISTRIBUTION (WHICH IS NOT NECESSARILY OF PEARSON
C  TYPE III).
C
C  THE INPUT CONDITIONAL DISTRIBUTION IS SPECIFIED AS AN INPUT VECTOR XI
C  CONTAINING QUANTILES BASED ON THE CENSORED-SAMPLE MEAN, STD.DEVIATION
C  AND SKEW (XIN = MEAN + STD.DEV * HARTER.K.VALUE).   THE INPUT QUANTIL
C  CORRESPOND TO STANDARD HARTER TABULAR CUMULATIVE (NONEXCEEDANCE) PROB
C
C  THE UNCONDITIONAL DISTRIBUTION IS RETURNED AS A VECTOR OF CPA-ADJUSTE
C  QUANTILES CORRESPONDING TO THE SAME STANDARD CUM. PROBABILITIES
C  TABULATED IN HARTER'S TABLES.  IF ANY TABULAR PCUM IS OUTSIDE
C  THE RANGE OF PROBABILITIES DEFINED BY THE CENSORED SAMPLE (I.E.,
C  IF PCUM.LT.N0/N .OR. .GT.1-N1/N), THEN A VALUE OF - OR + INFINITY
C  (I.E., -+1.01E31)  IS RETURNED.  (THIS IS DONE BECAUSE THE AVAIL-
C  ABLE INFORMATION DEFINES THE.)
C
C  PARAMETERS --
C  XIN   -(INPUT)- VECTOR (DIM 27) OF CONDITIONAL-DISTRIBUTION QUANTILES
C                  BASED ON CENSORED SAMPLE
C  N     -(INPUT)- TOTAL SAMPLE SIZE
C  N0    -(INPUT)- NUMBER OF ITEMS CENSORED OFF LOW END OF SAMPLE
C  N1    -(INPUT)- NUMBER OF ITEMS CENSORED OFF TOP ....
C  XOUT  -(OUTPUT)- VECTOR (DIMENSION 27) OF CPA-ADJUSTED QUANTILES
C
C  METHOD --
C  FOR X-VALUES BETWEEN CENSOR-LIMITS X0,X1, THE TOTAL-PROBABILITY
C  THEOREM STATES THAT --
C      PROB(RV.X .LE. X) = PROB(RV.X .LE. X, GIVEN RV.X W/IN X0,X1)
C                            * (N-N0-N1)/N   +   N0/N
C  THUS, UNCONDITIONAL QUANTILES AT PROBABILITY PCUM ARE FOUND BY LOOKUP
C  IN THE CONDITIONAL DISTRIBUTION AT PROBABILITY
C        PPRIME = (PCUM - N0/N) / (N-N0-N1)/N
C
C
C    WKIRBY, SWB  11/85.
C
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   XIN(NQS), XOUT(NQS), PROB(NQS), XBAR, STD
      DOUBLE PRECISION LSKEW
      INTEGER   N, N0, N1
C
C     + + + ARGUMENT DEFINITION + + +
C     XIN    -
C     N      -
C     N0     -
C     N1     -
C     XOUT   -
C
C     + + + LOCAL VARAIBLES + + +
      DOUBLE PRECISION XK, PPRIME
      REAL      LPROB(50)
      INTEGER   I
C
C     + + + FUNCTIONS + + +
      REAL   HARTKX
      DOUBLE PRECISION KF
C
C     + + + EXTERNALS + + +
      EXTERNAL   HARTKX, KF
C
C     + + + END SPECIFICATIONS + + +
C
      DO 5 I = 1,NQS
        LPROB(I)= 1.0 - PROB(I)
  5   CONTINUE
      DO 10 I = 1,NQS
        PPRIME = (N*LPROB(I)-N0)/(N-N0-N1)
        XK = KF(LSKEW,PPRIME)
        XOUT(I)= XBAR+ (XK*STD)
C       XOUT(I) = HARTKX(NQS, PPRIME, XIN, LPROB)
  10  CONTINUE
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   HARTKX
     I                        (NQS, P, V, PROB)
C
C     + + + PURPOSE + + +
C     This function looks up the P-th quantile (standardized deviate
C     with non-exceedence probability p) of the standardized Pearson
C     Type III distribution tabulated in the vector V.  The vector V
C     must have been filled by a prior call to HARTIV.  The value of
C     P must be between 0.0001 and 0.9999.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER NQS
      REAL   P, V(NQS), PROB(NQS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     P      - quantile of Pearson Type III distribution
C     V      - vector of skew-interpolated Harter K values from HARTIV
C
C     + + + LOCAL VARIABLES + + +
      REAL      HUGE, HARTK
      INTEGER   I
C
C     + + + DATA INITIALIZATIONS + + +
      DATA HUGE / 31.0 /
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NQS .GT. 1 .AND. 
     1    (P.GT.PROB(NQS) .OR. P.LT.PROB(1))) THEN
C       RETURN + OR - 1E29 IF OUT OF RANGE OF PROB
        HARTK=HUGE
        IF (P.LT.PROB(1))HARTK=-HUGE
      ELSE
C        IB=2
C        IF (P.GE.0.5)IB=17
C        IE=IB+13
        DO 10 I=2,NQS-1
          IF (P.LT.PROB(I))GOTO20
 10     CONTINUE
C        I=IB+14
        I=NQS
 20     HARTK=V(I-1)+(P-PROB(I-1))*(V(I)-V(I-1))/(PROB(I)-PROB(I-1))
      END IF
C
      HARTKX = HARTK
C
      RETURN
      END
c
c
c
      double precision function kf(skew,prob)
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
c
c    computes critical points of LP3 distribution 
c    returns "K" value in format used by B17B
c
c    tim cohn........24 Nov 2003
c
c****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      double precision parms(2),prob,skew,fp_z_icdf,fp_g2_icdf  
        
      if(skew .eq. 0.d0) then
          kf = fp_z_icdf(prob)
      else
          parms(1)  = 4.d0/skew**2
        if(skew .lt. 0.d0) then
          parms(2) = -1.d0/sqrt(parms(1))
        else
          parms(2) =  1.d0/sqrt(parms(1))
        endif  
          kf = fp_g2_icdf(prob,parms) - parms(1)*parms(2)
      endif

      return
      end

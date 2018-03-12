C
C
C
      SUBROUTINE   KENT
     I                  (X, N,
     O                   TAU, PLEVEL, SLOPE)
C
C      dll_export KENT
CDEC$ ATTRIBUTES DLLEXPORT :: KENT
C
C     + + + PURPOSE + + +
C     Computes Kendall's tau, the associated p-level, and the
C     slope of the trend line.
C     The p-level is the attained (two-sided) significance level of the
C     test [e.g., if (p-level.le.0.05) reject the null hypothesis
C     of no trend at the 5% level].
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   N
      REAL      X(N), TAU, SLOPE, PLEVEL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     X      - is the time series of equally spaced observations (set
C              missing values to -99999.0)
C     N      - is the number of observations in x (n<=250).
C     TAU    - returned as Kendall's tau.
C     PLEVEL - returned as the probability level of tau.
C     SLOPE  - returned as the estimate of the slope of the trend line.
C
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NCOMP
      REAL      S, VAR, Z
C
C     + + + INTRINSICS + + +
      INTRINSIC   SQRT
C
C     + + + FUNCTIONS + + +
      REAL CDFN
C
C     + + + EXTERNALS + + +
      EXTERNAL   KENS, CDFN
C
C     + + + END SPECIFICATIONS + + +
C
C     Reference:
C     Rank Correlation Methods, M.G.Kendall, Charles Griffin,
C        London, 1975
C
C     Subprograms called:
C        kens to compute Kendall's tau.
C        cdfn to compute the cumulative Normal distribution function.
C
C     Code history:
C        28 May 85  JRSlack  Initial coding.
C        18 Dec 85  JRSlack  Slope calculation corrected.
C        23 Nov 88  AMLumb converted to OSW coding convention and
C                   modified the sort procedures to get the SLOPE
C
C
C     Get the basic statistics.
      CALL KENS(X,N,S,VAR,NCOMP,SLOPE)
C
C     Kendall's tau.
      TAU = S / NCOMP
C     Continuity correction.
      IF (S.GT.0.0) S = S - 1.0
      IF (S.LT.0.0) S = S + 1.0
C     Approximately Normal deviate.
      Z = S / SQRT(VAR)
C     Calculate the p-level.
      IF (Z.LE.0.0) PLEVEL = 2.0 * CDFN(Z)
      IF (Z.GT.0.0) PLEVEL = 2.0 * (1.0 - CDFN(Z))
C
      RETURN
      END
C
C
C
      SUBROUTINE   KENS
     I                      (X, N,
     O                       S,VAR,NCOMP,SLOPE)
C
C     + + + PURPOSE + + +
C     Computes Kendall's S statistic and other information for
C     Kendall's tau test for trend. Sample usage of this
C     routine may be seen in routine kentau.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   N, NCOMP
      REAL      X(N), S, VAR, SLOPE
C
C     + + + ARGUMENT DEFINITION + + +
C     X      - the time series vector of equally spaced observation (set
C              missing values to -99999.0).
C     N      - the number of observations in x (n<=250).
C     S      - returned as Kendall's S statistic.
C     VAR    - returned as the variance of S adjusted for ties.
C     NCOMP  - returned as the number of comparisons made in
C              computing S.
C     SLOPE  - slope of trend (units/year)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NPLUS, NMINUS,I, ISTART, IEND, NTIE, NOK
      REAL      XCHECK, FIXVAR, SMIN, SMAX, YY
      LOGICAL WASTIE(250)
C     wastie is a value previously in a tie?
C
C     + + + INTRINSICS + + +
      INTRINSIC   SQRT, REAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   MEDSLP
C
C     + + + DATA INITIALIZATIONS + + +
      DATA    XCHECK/-99998./
C     Check for missimg values against XCHECK
C
C     + + + END SPECIFICATIONS + + +
C
C     Reference:
C     Rank Correlation Methods, M.G.Kendall, Charles Griffin,
C        London, 1975
C
C     Subprograms called:
C        None.
C
C     Code history:
C        28 May 85  JRSlack  Initial coding.
C        23 Nov 88  AMLumb converted to OSW coding convention and
C                   modified procedures to compute SLOPE
C
      IF (N.GT.250) STOP 'Subroutine kens called with n > 250.'
C
C     Set number of comparisons to zero.
      NCOMP = 0
C
      IF (N.LE.1) THEN
C       Case of n <= 1.,  nil S,  nil variance
        S = 0.0
        VAR = 0.0
      ELSE
C       Case of n > 1.
C
C       Number of up ticks.
        NPLUS = 0
C       Number of down ticks.
        NMINUS = 0
C       Variance correction for ties.
        FIXVAR = 0.0
C
C       Clear wastie array.
        DO 20 I = 1, N
          WASTIE(I) = .FALSE.
 20     CONTINUE
C
C       initialize values to compute min and max
        SMIN = 1.0E20
        SMAX = -1.0E20
C
        DO 40 ISTART = 1, N-1
C         Pick an observation.
C         Skip missing values.
          IF (X(ISTART).GT.XCHECK) THEN
C           A value is always tied with itself.
            NTIE=1
C
            DO 30 IEND = ISTART+1, N
C             Test each later observation.
C             Skip missing values.
              IF (X(IEND).GT.XCHECK) THEN
C               Valid pair.
C               Count comparison.
                NCOMP = NCOMP + 1
C               Adjust for separation.
                YY = (X(IEND) - X(ISTART)) / REAL(IEND - ISTART)
C               write(*,*) 'YY=',YY
                IF (YY.GT.0.0) THEN
C                 Up tick.
                  NPLUS = NPLUS + 1
                ELSE IF (YY.LT.0.0) THEN
C                 Down tics.
                  NMINUS = NMINUS + 1
                ELSE
C                 Tie.
                  NTIE = NTIE + 1
C                 Mark ties.
                  WASTIE(IEND) = .TRUE.
                END IF
                IF (YY .LT. SMIN) SMIN = YY
                IF (YY .GT. SMAX) SMAX = YY
              END IF
 30         CONTINUE
C
C           Update variance correction if tie occured and tie was not
C           counted before.
C
            IF (NTIE.NE.1.AND..NOT.WASTIE(ISTART)) FIXVAR = FIXVAR +
     1               NTIE * (NTIE-1.0) * (2.0*NTIE+5.0) / 18.0
          END IF
 40     CONTINUE
C
C       Compute the variance of the test statistic S.
C       Number of actual values.
        NOK = (1.0 + SQRT(1.0+8.0*NCOMP)) / 2.0
C       Simple variance.
        VAR = NOK * (NOK-1.0) * (2.0*NOK+5.0) / 18.0
C       Adjust for ties.
        VAR = VAR - FIXVAR
C
C       Compute the test statistic S.
        S = NPLUS - NMINUS
C
C       Find median slope
        CALL MEDSLP (N,X,NCOMP,SMIN,SMAX, SLOPE)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MEDSLP
     I                   (N,X,NCOMP,
     M                    SMIN,SMAX,
     O                    SLOPE)
C
C     + + + PURPOSE + + +
C     This routine finds the median slope of pairs in an annual time
C     series array.  The procedure dumps all pairs into 1 of 100 bins,
C     then finds the bin with the median value.  If the number of cases
C     in that bin exceeds 100 the process is repeated with that bin
C     divided into 100 more bins.  When the bin with the median has less
C     than 100 pairs, they are sorted and value of the median found
C     which is returned to the calling program as the SLOPE.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   N, NCOMP
      REAL      X(N), SMIN, SMAX, SLOPE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     N      - number of values in tine series array
C     X      - annual time series array
C     NCOMP  - number of pairs
C     SMIN   - minimum value of slope
C     SMAX   - maximum value of slope
C     SLOPE  - median value of slope
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   BINS(101), ISUM, NSORT, IPOS, MEDCNT, EOFLG, ISTART,
     &          IEND, IB, LOC, L0, L101, DFLG, BACK
      REAL      DELT, XMEDIN(2), ASORT(101), YY, XCHECK
C
C     + + + INTRINSICS + + +
      INTRINSIC   INT, REAL, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   ASRTRP, ZIPI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  L0/0/, L101/101/
      DATA    XCHECK/-99998./
C
C     + + + END SPECIFICATIONS + + +
C
C     set even(0) odd(1) flag for number of pairs
      EOFLG = MOD(NCOMP,2)
      DFLG = 0
C
 45   CONTINUE
C
        DELT = (SMAX - SMIN)/100.0
C
C       put counts in bins
        CALL ZIPI (L101,L0,BINS)
        MEDCNT = 0
        DO 60 ISTART = 1,N-1
          IF (X(ISTART) .GT. XCHECK) THEN
            DO 55 IEND = ISTART+1,N
              IF (X(IEND) .GT. XCHECK) THEN
                YY = (X(IEND) - X(ISTART))/REAL(IEND-ISTART)
                LOC = INT((YY-SMIN)/DELT) + 1
                IF (LOC .GT. 101) LOC = 101
                IF (LOC .LT. 1) LOC = 1
                BINS(LOC) = BINS(LOC) + 1
                MEDCNT = MEDCNT + 1
              END IF
 55         CONTINUE
          END IF
 60     CONTINUE
C
C       calculate count for median, upper value if even
        MEDCNT = MEDCNT/2 + 1
C
C       find bin with median value
        IB = 0
        ISUM = 0
 65     CONTINUE
          IB = IB + 1
          ISUM = ISUM + BINS(IB)
        IF (ISUM .LT. MEDCNT) GO TO 65
C
        IF (BINS(IB) .GT. 100) THEN
C         recompute bins
          SMAX = SMIN + DELT*REAL(IB)
          SMIN = SMAX - DELT
        END IF
        IF (SMAX-SMIN .LT. 0.0004) THEN
C         case when over 100 values are very close
          DFLG = 1
          SLOPE = (SMAX+SMIN)/2.0
        END IF
C       array for sorting can only have up to 100 values
      IF (BINS(IB) .GT. 100 .AND. DFLG .EQ. 0) GO TO 45
C
      IF (DFLG .EQ. 0) THEN
C       SLOPE not computed by the above special case
C       check for special case when NCOMP is even and the 2 median
C       values split the bins
        IF (EOFLG .EQ. 0 .AND. (ISUM-BINS(IB)+1) .EQ. MEDCNT) THEN
C         for special case find minimum of upper bin and max of lower bin
          XMEDIN(1) = -1.0E20
          XMEDIN(2) = 1.0E20
C         find previous non-zero bin
          BACK = 0
 66       CONTINUE
            BACK = BACK + 1
          IF (BINS(IB-BACK) .LE. 0 .AND. IB-BACK .GT. 1) GO TO 66
C
          DO 72 ISTART = 1,N-1
            IF (X(ISTART) .GT. XCHECK) THEN
              DO 70 IEND = ISTART+1,N
                IF (X(IEND) .GT. XCHECK) THEN
                  YY = (X(IEND)-X(ISTART))/REAL(IEND-ISTART)
                  LOC = INT((YY-SMIN)/DELT) + 1
                  IF (LOC .EQ. IB) THEN
C                   find minimum
                    IF (YY .LT. XMEDIN(2)) XMEDIN(2) = YY
                  ELSE IF (LOC .EQ. IB-BACK) THEN
C                   find maximum
                    IF (YY .GT. XMEDIN(1)) XMEDIN(1) = YY
                  END IF
                END IF
 70           CONTINUE
            END IF
 72       CONTINUE
C
        ELSE
C         the more common case
C         find values in the bin with the median
          NSORT = 0
          DO 80 ISTART = 1,N-1
            IF (X(ISTART).GT.XCHECK) THEN
              DO 75 IEND = ISTART+1,N
                IF (X(IEND).GT.XCHECK) THEN
                  YY = (X(IEND)-X(ISTART))/REAL(IEND-ISTART)
                  LOC = INT((YY-SMIN)/DELT) + 1
                  IF (LOC .EQ. IB) THEN
                    NSORT = NSORT + 1
                    ASORT(NSORT) = YY
                  END IF
                END IF
 75           CONTINUE
            END IF
 80       CONTINUE
C
          IF(NSORT.NE.BINS(IB)) WRITE(*,*)
     &         ' PROGRAM ERROR IN ROUTINE KENS',NSORT,BINS(IB)
C
C         sort remaining values
          CALL ASRTRP(NSORT,ASORT)
C
C         position in array for median
          IPOS = MEDCNT - (ISUM-BINS(IB))
          IF (IPOS .LT. 1) WRITE(*,*) ' PROGRAM BUG IN KENS, IPOS=',IPOS
          XMEDIN(2) = ASORT(IPOS)
          IF (EOFLG .EQ. 0) IPOS = IPOS - 1
          XMEDIN(1) = ASORT(IPOS)
        END IF
C
        SLOPE = (XMEDIN(1) + XMEDIN(2)) / 2.0
C
      END IF
C
      RETURN
      END
C
C
C
      REAL FUNCTION CDFN
     I                    (X)
C
C     + + + PURPOSE + + +
C     cumulative distribution function for the
C     normal zero-one distribution.
C     Primary reference is: Abramowitz & Stegun,
C        NBS Handbook of Mathematical Functions, equation 26.2.19
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   X
C
C     + + + ARGUMENT DEFINITION + + +
C     X      - standard deviate?
C
C     + + + LOCAL VARAIBLES + + +
      REAL   T
C
C     + + + END SPECIFICATIONS + + +
C
      IF (X) 10,20,30
C
C     Negative argument.
 10   CONTINUE
        IF (X.LT.-6.0) GO TO 40
        T=-X
        CDFN=      0.5/(1.0+0.0498673470*T+0.0211410061*T**2
     1   +0.0032776263*T**3+0.380036E-4*T**4+0.488906E-4*T**5
     1   +0.53830E-5*T**6)**16
        GO TO 90
C
C     Zero argument.
 20   CONTINUE
        CDFN=0.5
        GO TO 90
C
C     Positive argument.
 30   CONTINUE
        IF (X.GT.6.0) GO TO 50
        CDFN=1.0-0.5/(1.0+0.0498673470*X+0.0211410061*X**2
     1   +0.0032776263*X**3+0.380036E-4*X**4+0.488906E-4*X**5
     1   +0.53830E-5*X**6)**16
        GO TO 90
C
C     Outside the range +-6 the approximation is useless.
 40   CONTINUE
        CDFN=0.0
        GO TO 90
C
 50   CONTINUE
        CDFN=1.0
C
 90   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ASRTRP
     I                    (CNT,
     M                     RVAL)
C
C     + + + PURPOSE + + +
C     sorts decimal numbers in their array
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNT
      REAL        RVAL(CNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNT   - count of decimal numbers to sort
C     RVAL  - array of decimal numbers to sort
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,JPT,JPT1
      REAL        RTMP
C
C     + + + END SPECIFICATIONS + + +
C
C     move decimal numbers to their sorted positions
      DO 20 I= CNT,2,-1
        DO 10 JPT= 1,I-1
          JPT1= JPT+ 1
          IF (RVAL(JPT).GT.RVAL(JPT1)) THEN
            RTMP      = RVAL(JPT)
            RVAL(JPT) = RVAL(JPT1)
            RVAL(JPT1)= RTMP
          END IF
10      CONTINUE
20    CONTINUE
C
      RETURN
      END
C
C
C
c      SUBROUTINE   ZIPI
c     I                  (LEN, ZIP,
c     O                   X)
C
C     + + + PURPOSE + + +
C     Fill the integer array X of size LEN with
C     the given value ZIP.
C
C     + + + DUMMY ARGUMENTS + + +
c      INTEGER     LEN, ZIP
c      INTEGER     X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of array
C     ZIP    - value to fill array
C     X      - output array of size LEN
C
C     + + + LOCAL VARIABLES + + +
c      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
c      DO 100 L = 1, LEN
c         X(L) = ZIP
c  100 CONTINUE
C
c      RETURN
c      END

C
C
C
      SUBROUTINE   RANDOM
     I                   (VAR,MVAR,MVARX,NEMP,NDAT,DIST,
     O                    R,B,
     M                    CN,ISEED)
C
C     + + + PURPOSE + + +
C     generates random numbers, a vector
C     containing the parameters of the distribution is
C     passed into the routine, a vector containing the
C     random numbers is passed out
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 ISEED,MVAR,MVARX
      INTEGER   NEMP,NDAT(MVARX)
      REAL      VAR(MVARX,5),R(MVAR),CN(MVAR),DIST(NEMP,2,MVARX)
      REAL*8    B(MVARX,MVARX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISEED - ???
C     MVAR  - ???
C     MVARX - ???
C     NEMP  - ???
C     NDAT  - ???
C     VAR   - ???
C     R     - ???
C     CN    - ???
C     DIST  - ???
C     B     - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      IERR,INDEX,I
      REAL         W,XM,FXM,FX,A,B2,C
      CHARACTER*80 MESAGE
C
C     + + + FUNCTIONS + + +
      REAL*8       SQRCHK,UNIF,EXPRN
      REAL         NMB
C
C     + + + INTRINSICS + + +
      INTRINSIC    NINT,REAL,DBLE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,EMPCAL,MTPV,TRANSM,SUBOUT,NMB,EXPRN,UNIF,SQRCHK
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'RANDOM'
      CALL SUBIN(MESAGE)
C
  110 CONTINUE
      IERR = 0
      INDEX = 0
        DO 100 I=1,MVAR
          IF(NINT(VAR(I,5)) .EQ. 1 .OR.
     1      NINT(VAR(I,5)) .EQ. 2 .OR.
     2      NINT(VAR(I,5)) .EQ. 5 .OR. NINT(VAR(I,5)) .EQ. 6) THEN
C           normal or lognormal or SB distribution
            R(I) = NMB(ISEED)
C
          ELSEIF(NINT(VAR(I,5)) .EQ. 3) THEN
C           exponential distribution
            R(I) = VAR(I,1) * EXPRN(ISEED)
C
          ELSEIF(NINT(VAR(I,5)) .EQ. 4) THEN
C           uniform distribution
            R(I) = VAR(I,3) + UNIF(ISEED) * (VAR(I,4)-VAR(I,3))
C
          ELSEIF(NINT(VAR(I,5)) .EQ. 7) THEN
C           empirical distribution
            INDEX = INDEX + 1
            CALL EMPCAL(
     I                  NEMP,MVARX,NDAT(INDEX),INDEX,DIST,ISEED,
     O                  R(I))
C
          ELSEIF(NINT(VAR(I,5)) .EQ. 8) THEN
C           triangular distribution
            W = VAR(I,4) - VAR(I,3)
            XM = VAR(I,1) - VAR(I,3)
            FXM = XM/W
            FX = UNIF(ISEED)
            IF(FX .LE. FXM)THEN
              R(I) = VAR(I,3) + REAL(SQRCHK(DBLE(XM*W*FX)))
            ELSE
              A = -1./((1.-FXM)*(W**2))
              B2 = 2./((1.-FXM)*W)
              C = 1. - (1./(1.-FXM)) - FX
              R(I) = (-B2 + REAL(SQRCHK(DBLE(B2**2 - 4.*A*C))))/
     1                (2.*A) + VAR(I,3)
              IF(R(I) .LT. VAR(I,3) .OR. R(I) .GT. VAR(I,4))
     1           R(I) = (-B2 - REAL(SQRCHK(DBLE(B2**2 - 4.*A*C))))/
     2                   (2.*A) + VAR(I,3)
            END IF
C
          ELSEIF(NINT(VAR(I,5)) .EQ. 0) THEN
C           uniform distribution
            R(I) = VAR(I,1)
C
          ENDIF
  100   CONTINUE
C
C       correlate variables
C       multiply vector of random numbers by decomposed correlation matrix
C
        CALL MTPV(
     I            B,R,MVAR,MVARX,
     O            CN)
C
C       transform variables to their proper distributions
        CALL TRANSM(VAR,MVARX,CN,MVAR,R,IERR)
C
      IF(IERR .EQ. 1) GOTO 110
      CALL SUBOUT
      RETURN
      END
C
C
C
      REAL   FUNCTION   NMB
     I                     (IXX)
C
C     + + + PURPOSE + + +
C     generates normal 0,1 random numbers.
C     argument IX is integer seed used by the uniform
C     function UNIF called by NMB.
C     Modification date: 2/18/92 JAM
C
C     + + + ARGUMENTS + + +
      INTEGER*4 IXX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IXX - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL   AV,G,U,U1,U2,V,W
C
C     + + + FUNCTIONS + + +
      REAL   LNCHK
      REAL*8 EXPCHK,SQRCHK,UNIF
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS,REAL,DBLE
C
C     + + + EXTERNALS + + +
      EXTERNAL    EXPCHK,LNCHK,SQRCHK,UNIF
C
C     + + + END SPECIFICATIONS + + +
C
      U=UNIF(IXX)
      IF(U .LE. 0.8638)THEN
C       ???
        NMB=2.0*(UNIF(IXX)+UNIF(IXX)+UNIF(IXX)-1.5)
        RETURN
      END IF
      IF(U .LE. 0.9745)THEN
C       ???
        NMB=1.5*(UNIF(IXX)+UNIF(IXX)-1.0)
        RETURN
      END IF
      IF(U .LE. 0.9973002039)THEN
C       ???
   10   V = 6.0*UNIF(IXX) - 3.0
        AV = ABS(V)
        G = 17.49731196*REAL(EXPCHK(DBLE(-.5*V*V)))
        IF(AV .LT. 1.0)THEN
C         ???
          G = G - 4.73570326*(3.0-V*V)
        ELSE
          G = G - 2.36785163*(3.0-AV)*(3.0-AV)
        END IF
        IF(AV .LT. 1.5)THEN
C         ???
          G = G-2.157875*(1.5-AV)
        ENDIF
        IF(0.358*UNIF(IXX) .GT. G)THEN
C         ???
          GO TO 10
        END IF
        NMB = V
        RETURN
      END IF
C     ???
   20 U1 = 2.0*UNIF(IXX)-1.0
      U2 = 2.0*UNIF(IXX)-1.0
      W = U1*U1 + U2*U2
      IF(W .GE. 1.0)THEN
        GO TO 20
      END IF
      W = REAL(SQRCHK(DBLE((9.0-2.0*LNCHK(W))/W)))
      IF(ABS(U1*W) .GT. 3.0)THEN
         NMB = U1*W
         RETURN
      END IF
      IF (ABS(U2*W) .LE. 3.0)THEN
        GO TO 20
      END IF
      NMB = U2*W
      RETURN
      END
C
C
C
      DOUBLE PRECISION   FUNCTION   UNIF
     I                        (IX)
C
C     from ' A Guide to Simulation' by Paul Bratley,
C     Bennett L. Fox and Linus E. Schrage. Springer-Verlag, New York,
C     1983.
C
C     portable random number generator implementing the recursion:
C     IX = 16807 * IX MOD(2**(31) -1)
C     using only 32 bits, including sign.
C
C     + + + PURPOSE + + +
C     to utilize a random number generator to output a
C     fraction between 0 and 1
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 IX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IX - a new pseudorandom value
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 K1
C
C     + + + END SPECIFICATIONS + + +
C
      K1 = IX/127773
      IX = 16807*(IX - K1*127773) - K1*2836
      IF(IX .LT. 0) IX = IX + 2147483647
      UNIF = IX*4.656612875E-10
      RETURN
      END
C
C
C
      DOUBLE PRECISION   FUNCTION   EXPRN
     I                         (ISEED)
C
C     + + + PURPOSE + + +
C     generates exponentially distributed random numbers
C     Modification date: 2/18/92 JAM
C
C     + + + ARGUMENTS + + +
      INTEGER*4 ISEED
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISEED - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL         X
      INTEGER      IERROR,I
      LOGICAL      FATAL
      CHARACTER*80 MESAGE
C
C     + + + FUNCTIONS + + +
      REAL*8       UNIF
      REAL         LNCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL     ERRCHK,UNIF,LNCHK
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 I=1,100
        X = UNIF(ISEED)
        IF(X .GT. 1.E-20) GO TO 199
  100 CONTINUE
        IERROR = 5020
        MESAGE =
     1    'Uniform random numbers could not be generated for exp. dist.'
        FATAL  = .TRUE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
C
  199 EXPRN = -LNCHK(X)
      RETURN
      END
C
C
C
      SUBROUTINE   EMPCAL
     I                   (NEMP,MCMAX,NDAT,INDEX,DIST,ISEED,
     O                    VALUE)
C
C     + + + PURPOSE + + +
C     generates values from empirical distributions
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 ISEED
      INTEGER   NEMP,MCMAX,NDAT,INDEX
      REAL      DIST(NEMP,2,MCMAX),VALUE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISEED - ???
C     NEMP  - ???
C     MCMAX - ???
C     NDAT  - ???
C     INDEX - ???
C     DIST  - ???
C     VALUE - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I
      REAL         FX,SLOPE
      CHARACTER*80 MESAGE
C
C     + + + FUNCTIONS + + +
      REAL*8       UNIF
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,SUBOUT,UNIF
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'EMPCAL'
      CALL SUBIN(MESAGE)
C
C     generate 0,1 random number
      FX=UNIF(ISEED)
C
C     interpolate X from cumulative probability position FX
      DO 100 I=2,NDAT
        IF(FX.LE.DIST(I,2,INDEX))THEN
          SLOPE=(DIST(I,1,INDEX)-DIST(I-1,1,INDEX))/
     +          (DIST(I,2,INDEX)-DIST(I-1,2,INDEX))
          VALUE=DIST(I,1,INDEX)-SLOPE*(DIST(I,2,INDEX)-FX)
          GO TO 111
        END IF
  100 CONTINUE
C
  111 CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   MTPV
     I                 (A,B,N,NCARLO,
     O                  C)
C
C     + + + PURPOSE + + +
C     multiplies A(N by N) by vector B(N by 1) to give
C     vector C (N by 1)
C     Modification date: 2/7/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4    N,NCARLO
      REAL         B(N),C(N)
      REAL*8       A(NCARLO,NCARLO)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     A      - ???
C     B      - ???
C     C      - ???
C     N      - ???
C     NCARLO - ???
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*80 MESAGE
      INTEGER*4    I,K
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,SUBOUT
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'MTPV'
      CALL SUBIN(MESAGE)
C
      DO 101 I=1,N
        C(I)=0.
        DO 100 K=1,N
          C(I)=A(I,K)*B(K)+C(I)
  100 CONTINUE
  101 CONTINUE
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   TRANSM
     I                   (VAR,MVARX,CN,MVAR,
     O                    R,IERR)
C
C     + + + PURPOSE + + +
C     converts the normally distributed correlated vector
C     CN to the parameter set returned to the model.  each member of the
C     vector is transformed to its input distribution.
C     Modifcation date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 IERR,MVARX,MVAR
      REAL      VAR(MVARX,5),CN(MVAR),R(MVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VAR   - ???
C     MVARX - ???
C     CN    - ???
C     MVAR  - ???
C     R     - ???
C     IERR  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4    ITYPE,K
      CHARACTER*80 MESAGE
C
C     + + + FUNCTIONS + + +
      REAL*8       EXPCHK
C
C     + + + INTRINSICS + + +
      INTRINSIC    NINT,REAL,DBLE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,TRANSB,SUBOUT,EXPCHK
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'TRANSM'
      CALL SUBIN(MESAGE)
      IERR = 0
      ITYPE = 2
C
      DO 100 K=1,MVAR
        IF(NINT(VAR(K,5)).EQ.1) THEN
C
C         normal
          R(K) = CN(K)*VAR(K,2) + VAR(K,1)
        ELSE IF(NINT(VAR(K,5)).EQ.2)THEN
C
C         lognormal
          R(K) = REAL(EXPCHK(DBLE(CN(K)*VAR(K,2) + VAR(K,1))))
        ELSEIF(NINT(VAR(K,5)) .EQ. 5) THEN
C
C         transform johnson SB
          CN(K) = CN(K)*VAR(K,2) + VAR(K,1)
          CALL TRANSB(
     I                VAR(K,3),VAR(K,4),ITYPE,
     O                CN(K),R(K))
        ELSEIF(NINT(VAR(K,5)) .EQ. 6) THEN
C
C         johnson SU
          CN(K) = CN(K)*VAR(K,2) + VAR(K,1)
          R(K) = VAR(K,3) + 0.5*(VAR(K,4) - VAR(K,3))*
     1     (REAL(EXPCHK(DBLE(CN(K))))-REAL(EXPCHK(DBLE(-CN(K)))))
        END IF
C
        IF(R(K) .LT. VAR(K,3)) THEN
          IERR = 1
        ELSEIF(R(K) .GT. VAR(K,4)) THEN
          IERR = 1
        ENDIF
 100  CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   TRANSB(
     I                    A,B,ITYPE,
     O                    Y,X)
C
C     + + + PURPOSE + + +
C     transforms variables from normal
C     space to SB space or vica-versa depending upon the
C     value of ITYPE
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      ITYPE
      REAL         A,B,X,Y
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITYPE - ???
C     A     - ???
C     B     - ???
C     X     - ???
C     Y     - ???
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*80 MESAGE
C
C     + + + FUNCTIONS + + +
      REAL         LNCHK
      REAL*8       EXPCHK
C
C     + + + INTRINSICS + + +
      INTRINSIC    REAL,DBLE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,SUBOUT,LNCHK,EXPCHK
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'TRANSB'
      CALL SUBIN(MESAGE)
C
      IF(ITYPE .EQ. 1) THEN
        Y = LNCHK((X-A)/(B-X))
      ELSE
        X = (B*REAL(EXPCHK(DBLE(Y))) + A)/(1. + REAL(EXPCHK(DBLE(Y))))
      ENDIF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   OUTPUT
     I                   (NVAR,NMAX,NRUNS,NRMAX,NCMAX,IOUT,SNAME,IND1,
     I                    NAVG,STAT,CORR,XCDF,PALPH,INDZ)
C
C     + + + PURPOSE + + +
C     calls other subroutines to write summary statistics and
C     cdfs of variables from Monte Carlo simulations.
C     called after all Monte Carlo runs are completed.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      NVAR,NMAX,NRUNS,NRMAX,NCMAX,IOUT,IND1(NMAX,2),
     1             NAVG(NMAX),INDZ(NMAX,2)
      REAL         PALPH
      REAL*8       STAT(NMAX,6),CORR(NMAX,NMAX),XCDF(NCMAX,NRMAX)
      CHARACTER*20 SNAME(NMAX,3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NVAR  - ???
C     NMAX  - ???
C     NRUNS - ???
C     NRMAX - ???
C     NCMAX - ???
C     IOUT  - ???
C     SNAME - ???
C     IND1  - ???
C     NAVG  - ???
C     STAT  - ???
C     CORR  - ???
C     XCDF  - ???
C     PALPH - ???
C     INDZ  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,ICDF,TABOPT,PLTOPT,LNG
      CHARACTER*80 TITLE,XNAME,VALUE,MESAGE
C
C     + + + FUNCTIONS + + +
      INTEGER      LNGSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,STOUT,LFTJUS,OUTFOR,SUBOUT,LNGSTR
C
C     + + + OUTPUT FORMATS + + +
 1000 FORMAT(I10,':',I3)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'OUTPUT'
      CALL SUBIN(MESAGE)
C
C     print summary statistics
      CALL STOUT(
     I           NVAR,NMAX,NRUNS,IOUT,SNAME,IND1,NAVG,INDZ,
     M           STAT,CORR)
C
C     plot cumulative distributions
      TABOPT = 1
      PLTOPT = 2
      ICDF = 0
      DO 100 I=1,NVAR
        IF(SNAME(I,2)(1:3) .EQ. 'CDF')THEN
          ICDF = ICDF + 1
          VALUE = SNAME(I,1)
          LNG   = LNGSTR(VALUE)
          WRITE(VALUE,1000) IND1(I,2),INDZ(I,2)
          CALL LFTJUS(VALUE)
          XNAME = SNAME(I,1) (1:LNG) // '[' //
     1            VALUE(1:LNGSTR(VALUE)) // ']'
          TITLE = 'CUMULATIVE DISTRIBUTION FOR '// XNAME
          CALL OUTFOR(
     I           NRUNS,XCDF,TITLE,IOUT,TABOPT,PLTOPT,XNAME,
     I           NRMAX,NCMAX,ICDF,PALPH)
        END IF
  100 CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   STATIS
     I                   (NVAR,NMAX,X,SNAME,IRUN,NRMAX,NCMAX,IOUT2,
     M                    XCDF,STAT,CORR)
C
C     + + + PURPOSE + + +
C     performs summations required to calculate
C     summary statistics for variables X(I), stores cdf values, and
C     writes out results for each individual Monte Carlo run.
C     called for each Monte Carlo run after all random numbers
C     are generated and deterministic calculations are completed.
C     Mdofication date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      NVAR,NMAX,IRUN,NRMAX,NCMAX,IOUT2
      REAL*8       X(NVAR),XCDF(NCMAX,NRMAX)
      REAL*8       STAT(NMAX,6),CORR(NMAX,NMAX)
      CHARACTER*20 SNAME(NMAX,3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NVAR  - ???
C     NMAX  - ???
C     X     - ???
C     SNAME - ???
C     IRUN  - ???
C     NRMAX - ???
C     NCMAX - ???
C     IOUT2 - ???
C     XCDF  - ???
C     STAT  - ???
C     CORR  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,NCDF,NWRITE
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,SUBOUT
C
C     + + + OUTPUT FORMATS + + + 
C      
 2000 FORMAT(I10,16G11.3)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'STATIS'
      CALL SUBIN(MESAGE)
C
C     loop through number of variables and increment summations
      NCDF = 0
      DO 100 I=1,NVAR
C
C       store CDF output variables
        IF(SNAME(I,2)(1:3) .EQ. 'CDF')THEN
          NCDF = NCDF + 1
          XCDF(NCDF,IRUN) = X(I)
        END IF
C
C       calculate parameters needed for independent moments
        STAT(I,1) = STAT(I,1) + X(I)
        STAT(I,2) = STAT(I,2) + (X(I))**2
        STAT(I,3) = STAT(I,3) + (X(I))**3
        STAT(I,6) = STAT(I,6) + (X(I))**4
        IF(X(I) .LT. STAT(I,4)) STAT(I,4) = X(I)
        IF(X(I) .GT. STAT(I,5)) STAT(I,5) = X(I)
C
C       calculate covariance summations
        DO 50 J=I,NVAR
          CORR(I,J) = CORR(I,J) + X(I)*X(J)
          CORR(J,I) = CORR(I,J)
   50   CONTINUE
C
  100 CONTINUE
C
C     write selected variables to VAR output file containing
C     results of individual Monte Carlo runs
      NWRITE = 0
      DO 200 I=1,NVAR
        IF(SNAME(I,3)(1:5) .EQ. 'WRITE')THEN
          NWRITE = NWRITE + 1
          X(NWRITE) = X(I)
        END IF
  200 CONTINUE
      IF(NWRITE .GT. 0)WRITE(IOUT2,2000)IRUN,(X(I),I=1,NWRITE)
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   STOUT
     I                  (NVAR,NMAX,NRUNS,IOUT,SNAME,IND1,NAVG,INDZ,
     M                   STAT,CORR)
C
C     + + + PURPOSE + + +
C     converts summations calculated by STATIS to
C     summary statistics (mean, standard deviation, skew, correlation).
C     statistics are then printed out in a summary table.
C     called after all Monte Carlo runs are completed.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      NVAR,NMAX,NRUNS,IOUT,IND1(NMAX,2),NAVG(NMAX),
     1             INDZ(NMAX,2)
      REAL*8       STAT(NMAX,6),CORR(NMAX,NMAX)
      CHARACTER*20 SNAME(NMAX,3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NVAR  - ???
C     NMAX  - ???
C     NRUNS - ???
C     IOUT  - ???
C     SNAME - ???
C     IND1  - ???
C     NAVG  - ???
C     INDZ  - ???
C     STAT  - ???
C     CORR  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL         FN,CVAR
      REAL*8       STT,TEMP
      INTEGER      I,J
      CHARACTER*80 MESAGE
C
C     + + + INTRINSICS + + +
      INTRINSIC    REAL,DSQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,SUBOUT
C
C     + + + OUTPUT FORMATS + + +
 2040 FORMAT(////5X,'SUMMARY STATISTICS OVER ALL MONTE CARLO RUNS',
     1         /5X,'FOR SELECTED VARIABLES:',
     2         /5X,'--------------------------------------------',
     3     ////5X,'VARIABLE',T20,'INDEX',1X,'INDZ',T31,'AVERAGING',T42,
     4         'MEAN',T51, 'STANDARD',T63,'COEFF. OF',
     5          T77,'SKEW',T85,'MINIMUM',T96,'MAXIMUM',T107,'KURTOSIS',
     6        /T33,'PERIOD',
     7        T51,'DEVIATION',T63,'VARIATION',T77,'COEFF.',/5X,120('-'))
 2060 FORMAT(5X,A14,I5,I5,I5,5X,7G11.3)
 2080 FORMAT(///5X,'CORRELATION MATRIX FOR SUMMARY OUTPUT VARIABLES:',
     1         /5X,'------------------------------------------------',
     2       //12X,10(3X,A5))
 2100 FORMAT(/7X,A5,10F8.3)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'STOUT'
      CALL SUBIN(MESAGE)
C
      FN = REAL(NRUNS)
C
C     calculate skew, mean, standard deviation
      DO 100 I = 1,NVAR
C       standard deviation
        IF(FN .GT. 1)THEN
          TEMP = STAT(I,2) - (STAT(I,1)**2)/FN
          IF(TEMP .LE. .0001*STAT(I,2))THEN
              STT = 0.0
          ELSE
              STT = DSQRT(TEMP/(FN-1.))
          END IF
        ELSE
          STT = 0.0
        END IF
C
C       kurtosis
        IF(FN .GT. 3 .AND. STT .GT. 0.0)THEN
          CVAR = STAT(I,1)/FN
          STAT(I,6) = STAT(I,6) - 4.*STAT(I,3)*CVAR +
     1               6.*STAT(I,2)*CVAR**2 - 4*STAT(I,1)*CVAR**3 +
     2               FN*CVAR**4
          STAT(I,6) = (FN**2)*STAT(I,6)/((FN-1)*(FN-2)*(FN-3)*STT**4)
        ELSE
          STAT(I,6) = 0.0
        END IF
C
C       skew coefficient
        IF(FN .GT. 2 .AND. STT .GT. 0.0)THEN
          STAT(I,3) = (FN**2)*STAT(I,3) - 3.*FN*STAT(I,1)*STAT(I,2)
     1                + 2.*(STAT(I,1)**3)
          STAT(I,3) = STAT(I,3)/(FN*(FN-1)*(FN-2)*(STT**3))
        ELSE
          STAT(I,3) = 0.0
        END IF
C
C       mean
        STAT(I,2) = STT
        STAT(I,1) = STAT(I,1)/FN
  100 CONTINUE
C
C     calculate correlation coefficients
      DO 200 I = 1,NVAR
        DO 200 J = I,NVAR
          IF(STAT(I,2) .GT. 0.0 .AND. STAT(J,2) .GT. 0.0)THEN
             CORR(I,J) = (CORR(I,J)-FN*STAT(I,1)*STAT(J,1))
     1                   /((FN-1)*STAT(I,2)*STAT(J,2))
          ELSE
             CORR(I,J) = 0.0
             IF(I .EQ. J)CORR(I,J) = 1.0
          END IF
          CORR(J,I) = CORR(I,J)
  200 CONTINUE
C
C     print out statistics
      WRITE(IOUT,2040)
      DO 300 I=1,NVAR
        IF (STAT(I,1) .GT. 0.0)THEN
          CVAR = STAT(I,2)/STAT(I,1)
        ELSE
          CVAR = 0.0
        END IF
        WRITE(IOUT,2060)SNAME(I,1),IND1(I,2),INDZ(I,2),NAVG(I),
     1  STAT(I,1),STAT(I,2),CVAR,STAT(I,3),STAT(I,4),STAT(I,5),STAT(I,6)
  300 CONTINUE
      WRITE(IOUT,2080)(SNAME(J,1),J=1,NVAR)
      DO 400 I=1,NVAR
        WRITE(IOUT,2100)SNAME(I,1),(CORR(I,J),J=1,NVAR)
  400 CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   OUTFOR
     I                   (NVAL,RVAL,TITLE,TEROFL,TABOPT,PLTOPT,XNAME,
     I                    NRMAX,NCMAX,ICDF,PALPH)
C
C     + + + PURPOSE + + +
C     writes tables and plots of cumulative distributions
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      NVAL,TEROFL,TABOPT,PLTOPT,NRMAX,NCMAX,ICDF
      REAL*8       RVAL(NCMAX,NRMAX)
      REAL         PALPH
      CHARACTER*80 TITLE
      CHARACTER*30 XNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NVAL   - ???
C     RVAL   - ???
C     TITLE  - ???
C     TEROFL - ???
C     TABOPT - ???
C     PLTOPT - ???
C     XNAME  - ???
C     NRMAX  - ???
C     NCMAX  - ???
C     ICDF   - ???
C     PALPH  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL*8       AMX(30),XLAB(11),SUM,SUM2,MEAN,RMEAN,SD,CV,
     1             XLIM(70),PC(70),PCC(70),PCZERO,XMIN,XMAX,AMN,YMAX,
     2             XNVAL,PXMAX, PCCM1(70),FACTOR,PERC80(3),PERC50(3),
     3             PERC90(3),PERC95(3),TEMP,Z,T
      INTEGER      I,J,K,IWRTX,INDEX,POWER
      CHARACTER*80 MESAGE
C
C     + + + FUNCTIONS + + +
      REAL         LNCHK
      REAL*8       SQRCHK
C
C     + + + INTRINSICS + + +
      INTRINSIC    DBLE,NINT,INT,ABS,LOG10
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,FRQTAB,FRQPLT,SUBOUT,LNCHK,SQRCHK
C
C     + + + DATA INITIALIZATIONS + + +
      DATA AMX /.1E-20, .25E-20, .5E-20, .1E-19, .25E-19, .5E-19,
     1     .1E-18,.5E-18,.1E-17,.5E-17,.1E-16,.5E-16,.1E-15,.1E-14,
     2     .1E-13,.1E-12,.1E-11,.1E-10,.1E-9,.1E-8,.1E-7,.1E-6,.1E-5,
     3     .1E-4,.1E-3,.1E-2,.1E-1,.1,1.,10./
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'OUTFOR'
      CALL SUBIN(MESAGE)
C
C     determine stats of data
      XNVAL= NVAL
      SUM  = 0.
      SUM2 = 0.
      XMIN = 1.E20
      XMAX = -1.E20
      DO 10 K= 1, NVAL
        SUM  = SUM + RVAL(ICDF,K)
        SUM2 = SUM2 + RVAL(ICDF,K) * RVAL(ICDF,K)
        IF (XMIN .GT. RVAL(ICDF,K)) XMIN = RVAL(ICDF,K)
        IF (XMAX .LT. RVAL(ICDF,K)) XMAX = RVAL(ICDF,K)
 10   CONTINUE
C
C     order rval from maximum to minimum
      DO 110 I = 1,NVAL
        DO 110 J = I+1,NVAL
          IF(RVAL(ICDF,J) .GT. RVAL(ICDF,I)) THEN
            TEMP = RVAL(ICDF,I)
            RVAL(ICDF,I) = RVAL(ICDF,J)
            RVAL(ICDF,J) = TEMP
          END IF
  110 CONTINUE
C
C     find percentiles and confidence bounds
      T = (1. - .01*PALPH)/2.
      IF(T .GT. 0.5)T = 1. - T
      T = (SQRCHK(DBLE(LNCHK(1./T**2))))
      Z = T - (2.30753 + .27061*T)/(1. + .99229*T + .04481*T**2)
      IF(NVAL .GE. 2) THEN
        INDEX = NINT(NVAL  * 50./100.)
      ELSE
        INDEX = 1
      ENDIF
      PERC50(1) = RVAL(ICDF,INDEX)
      INDEX = 1 + INT(.5*NVAL - Z*(SQRCHK(DBLE(.5*NVAL*(1.-.5)))))
      PERC50(2) = RVAL(ICDF,NVAL-INDEX+1)
      INDEX = 1 + INT(.5*NVAL + Z*(SQRCHK(DBLE(.5*NVAL*(1.-.5)))))
      IF(NVAL-INDEX+1.LE.0) THEN
        PERC50(3) = RVAL(ICDF,1)
      ELSE
        PERC50(3) = RVAL(ICDF,NVAL-INDEX+1)
      ENDIF
      IF(NVAL .GE. 5) THEN
        INDEX = NINT(NVAL * 20./100.)
      ELSE
        INDEX = 1
      ENDIF
      PERC80(1) = RVAL(ICDF,INDEX)
      INDEX = 1 + INT(.8*NVAL - Z*(SQRCHK(DBLE(.8*NVAL*(1.-.8)))))
      PERC80(2) = RVAL(ICDF,NVAL-INDEX+1)
      INDEX = 1 + INT(.8*NVAL + Z*(SQRCHK(DBLE(.8*NVAL*(1.-.8)))))
C
      IF(NVAL-INDEX+1.LE.0) THEN
        PERC80(3) = RVAL(ICDF,1)
      ELSE
        PERC80(3) = RVAL(ICDF,NVAL-INDEX+1)
      ENDIF
      IF(NVAL .GE. 10) THEN
        INDEX =  NINT(NVAL * 10./100.)
      ELSE
        INDEX = 1
      ENDIF
      PERC90(1) = RVAL(ICDF,INDEX)
      INDEX = 1 + INT(.9*NVAL - Z*(SQRCHK(DBLE(.9*NVAL*(1.-.9)))))
      PERC90(2) = RVAL(ICDF,NVAL-INDEX+1)
      INDEX = 1 + INT(.9*NVAL + Z*(SQRCHK(DBLE(.9*NVAL*(1.-.9)))))
      IF(NVAL-INDEX+1.LE.0) THEN
        PERC90(3) = RVAL(ICDF,1)
      ELSE
        PERC90(3) = RVAL(ICDF,NVAL-INDEX+1)
      ENDIF
      IF(NVAL .GE. 20) THEN
        INDEX =  NINT(NVAL * 5. /100.)
      ELSE
        INDEX = 1
      ENDIF
      PERC95(1) = RVAL(ICDF,INDEX)
      INDEX = 1 + INT(.95*NVAL - 
     1            Z*(SQRCHK(DBLE(.95*NVAL*(1.-.95)))))
      PERC95(2) = RVAL(ICDF,NVAL-INDEX+1)
      INDEX = 1 + INT(.95*NVAL + 
     1            Z*(SQRCHK(DBLE(.95*NVAL*(1.-.95)))))
C
      IF(NVAL-INDEX+1.LE.0) THEN
        PERC95(3) = RVAL(ICDF,1)
      ELSE
        PERC95(3) = RVAL(ICDF,NVAL-INDEX+1)
      ENDIF
      MEAN = SUM / XNVAL
      SD   = (SQRCHK(DBLE((XNVAL*SUM2 - SUM*SUM)/XNVAL/
     1           (XNVAL - 1.))))
      RMEAN= MEAN
      IF (MEAN .LE. 0.) RMEAN = -SD
        CV   = SD / RMEAN
C
C     set maximum scale on x-axis
      I= 0
 20   CONTINUE
        I= I+ 1
      IF (XMAX.GE.AMX(I).AND.I.LT.30) GO TO 20
C
C     set maximum value for plotting to maximum value
      PXMAX = XMAX
      IWRTX = 8
C
C     set minimum scale on x-axis
      AMN = XMIN
      IF(ABS(PXMAX) .GT. 1.E-20) THEN
        POWER = INT(LOG10(ABS(PXMAX)))
      ELSE
        POWER = 0
      ENDIF
      FACTOR = 10.**POWER
      DO 30 I = 1, 11
        XLAB(I) = (AMN + (PXMAX - AMN) / 10. * DBLE(I - 1))/FACTOR
 30   CONTINUE
C
C     calculate percent probability
      DO 40 I = 1, 70
        XLIM(I) = AMN + (PXMAX - AMN) / 70. * DBLE(I)
        PC(I)   = 0.
        PCC(I)  = 0.
 40   CONTINUE
C
      PCZERO= 0.
        DO 70  I = 1, NVAL
          J= 0
 50       CONTINUE
            J= J+ 1
            IF (RVAL(ICDF,I).LE.AMN) PCZERO= PCZERO+ 1
          IF (RVAL(ICDF,I).GT.XLIM(J).AND.J.LT.70) GO TO 50
C         PC(J) = PC(J) + 1.
          DO 60 K= J,70
            IF (ABS(RVAL(ICDF,I)-XLIM(J)) .GT. 1.0E-30) THEN
              PCC(K)= PCC(K)+ 1.
            END IF
 60       CONTINUE
 70     CONTINUE
C
C     adjust the percentages
      DO 80 I = 1, 70
        PCC(I)= 100-  PCC(I)/ XNVAL * 100.
        PCCM1(I) = 100 - PCC(I)
 80   CONTINUE
      PCZERO = 100.
C
C     determine percent of time in 10 intervals
      PC(1)= PCZERO- PCC(7)
      DO 90 I = 14, 70, 7
        J= I/7
        PC(J) = PCC(I-7) - PCC(I)
 90   CONTINUE
C
C     determine ymax
      YMAX = 0.
      DO 100 I = 1, 10
        IF (YMAX .LT. PC(I)) YMAX = PC(I)
 100  CONTINUE
C
      IF (TABOPT.GT.0) THEN
C       do tables
        CALL FRQTAB(
     I              TEROFL,PCC,PC,AMN,XLIM,PCZERO,MEAN,SD,CV,XMIN,XMAX,
     I              TITLE,PERC80,PERC50,PERC90,PERC95,NVAL,PALPH)
      END IF
C
      IF (PLTOPT.GT.0) THEN
C       do plots
        CALL FRQPLT(
     I              PLTOPT,TEROFL,YMAX,PCCM1,PC,XLAB,IWRTX,FACTOR,
     I              XNAME)
      END IF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   FRQTAB
     I                   (TEROFL,PCC,PC,AMN,XLIM,PCZERO,MEAN,SD,CV,
     I                    XMIN,XMAX,TITLE,PERC80,PERC50,PERC90,PERC95,
     I                    NVAL,PALPH)
C
C     + + + PURPOSE + + +
C     print tabular frequency output
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      TEROFL,NVAL
      REAL*8       PCC(70),PC(10),AMN,XLIM(70),PCZERO,MEAN,SD,CV,
     1             XMIN,XMAX,PERC80(3),PERC50(3),PERC90(3),PERC95(3)
      REAL         PALPH
      CHARACTER*80 TITLE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TEROFL - ???
C     PCC    - ???
C     PC     - ???
C     AMN    - ???
C     XLIM   - ???
C     PCZERO - ???
C     MEAN   - ???
C     SD     - ???
C     CV     - ???
C     XMIN   - ???
C     XMAX   - ???
C     TITLE  - ???
C     PERC80 - ???
C     PERC50 - ???
C     PERC90 - ???
C     PERC95 - ???
C     NVAL   - ???
C     PALPH  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,SUBOUT
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT (//////////,5X,A80,/5X,55('-'),///,
     A      17X,'N                        = ', I5   ,/,
     1      17X,'MEAN                     = ', G10.3,/,
     2      17X,'STANDARD DEVIATION       = ', G10.3,/,
     3      17X,'COEFFICIENT OF VARIATION = ', G10.3,/,
     4      17X,'MINIMUM VALUE            = ', G10.3,/,
     5      17X,'MAXIMUM VALUE            = ', G10.3,//,
     A       2X,'50th PERCENTILE  = ', G10.3,
     1       2X,F5.1,' % CONF. BOUNDS =',G10.3,'  TO ',G10.3,/,
     B       2X,'80th PERCENTILE  = ', G10.3,
     1       2X,F5.1,' % CONF. BOUNDS =',G10.3,'  TO ',G10.3,/,
     C       2X,'90th PERCENTILE  = ', G10.3,
     1       2X,F5.1,' % CONF. BOUNDS =',G10.3,'  TO ',G10.3,/,
     D       2X,'95th PERCENTILE  = ', G10.3,
     1       2X,F5.1,' % CONF. BOUNDS =',G10.3,'  TO ',G10.3,/,
     4       5X,12('_____'),//
     5       5X,'  VALUE', T20, '% OF TIME EQUALLED',
     6          T40, '% OF TIME IN INTERVAL',/,
     6          T20,            '    OR EXCEEDED   ',/,
     7       5X,12('_____'),/)
 2020 FORMAT (1X, T5, G10.3, T20, F10.3)
 2030 FORMAT (1X, T42, F10.3)
 2040 FORMAT (5X,12('_____')////)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'FRQTAB'
      CALL SUBIN(MESAGE)
C
C     output results
      WRITE (TEROFL,2010)  TITLE, NVAL, MEAN, SD, CV, XMIN,
     1                    XMAX, PERC50(1),PALPH,PERC50(2),PERC50(3),
     2                          PERC80(1),PALPH,PERC80(2),PERC80(3),
     3                          PERC90(1),PALPH,PERC90(2),PERC90(3),
     4                          PERC95(1),PALPH,PERC95(2),PERC95(3)
      WRITE(TEROFL,2020) AMN,PCZERO
      I= 0
10    CONTINUE
        I= I+ 1
        J= I* 7
        WRITE(TEROFL,2030) PC(I)
        WRITE (TEROFL,2020) XLIM(J), PCC(J)
      IF(I .LT. 10 .AND. PCC(J) .GT. 1.0E-4) GOTO 10
C
      WRITE(TEROFL,2040)
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   FRQPLT
     I                   (PLTOPT,TEROFL,YMAX,PCC,PC,XLAB,IWRTX,
     I                    FACTOR,XNAME)
C
C     + + + PURPOSE + + +
C     plots cumulative distributions
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      PLTOPT,TEROFL,IWRTX
      REAL*8       YMAX,PCC(70),PC(10),XLAB(11),FACTOR
      CHARACTER*30 XNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PLTOPT - ???
C     TEROFL - ???
C     YMAX   - ???
C     PCC    - ???
C     PC     - ???
C     XLAB   - ???
C     IWRTX  - ???
C     FACTOR - ???
C     XNAME  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      PLTTYP,NUMPLT,LY,NY,IWRTY,I,J,K,IJ,IX,IY
      REAL         PYMAX
      CHARACTER*80 MESAGE
      CHARACTER*1  GR(80,21), BLK, IVL, IHL, IPL, ISTAR, IPERC,
     1             L0,L1, L2, L3, L4, L5, L6, L8,
     1             LF, LT, LI, LM, LE, LC,
     2             LN, LR, LV, LA, LL, LQ, LU, LYY
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,SUBOUT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLK /' '/
      DATA IVL,IHL,IPL,ISTAR,IPERC/'!','-','+','*','%'/
      DATA LF,LT,LI,LM,LE,LC,LN/'F','T','I','M','E','C','N'/
      DATA LR,LV,LA,LL,LQ,LU,LYY/'R','V','A','L','Q','U','Y'/
      DATA L0,L1,L2,L3,L4,L5,L6,L8/'0','1','2','3','4','5','6','8'/
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT ('1')
 2015 FORMAT (////)
 2020 FORMAT (' ', 80A1)
 2021 FORMAT (/,T30,A30)
 2030 FORMAT (' ', 3X, F6.5, 4(7X,' ',F6.5))
 2040 FORMAT (' ', 3X, F5.4,10(1X,' ',F5.4))
 2050 FORMAT (' ', 3X, F6.3,10(' ',F6.3))
 2060 FORMAT (' ', 4X, F4.2,10(2X,' ',F4.2))
 2070 FORMAT (' ', 4X, F4.1,10(2X,' ',F4.1))
 2080 FORMAT (' ', 4X, F4.0,10(2X,' ',F4.0))
 2090 FORMAT (' ', 3X, F5.0,10(1X,' ',F5.0))
 2100 FORMAT (' ', 3X, F6.0, 5(7X,' ',F6.0))
 2110 FORMAT (' ', 2X, F7.0, 4(6X,' ',F7.0))
 2120 FORMAT (' ', 2X, F8.0, 4(5X,' ',F8.0))
 2130 FORMAT (35X,'* ',E8.1)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'FRQPLT'
      CALL SUBIN(MESAGE)
C
      NUMPLT= 1
      IF (PLTOPT.EQ.1.OR.PLTOPT.EQ.3) THEN
C       interval plot
        PLTTYP= 1
        IF (PLTOPT.EQ.3) THEN
C         cumulative plot
          NUMPLT= 2
        END IF
      ELSE
C       only a cumulative plot
        PLTTYP= 2
      END IF
 10   CONTINUE
C
C       initialize GR as blank
        I= 1680
        DO 20 I = 1,80
          DO 21 IJ = 1,21
            GR(I,IJ) = BLK
   21     CONTINUE
   20   CONTINUE
C
        IF (PLTTYP.EQ.1) THEN
C         set maxinum scale on y-axis and format index
          IF (YMAX. LT. 10.) THEN
            PYMAX= 10.
            IWRTY= 1
          ELSE IF (YMAX. LT. 25.) THEN
            PYMAX= 25.
            IWRTY= 2
          ELSE IF (YMAX .LT. 50.) THEN
            PYMAX= 50.
            IWRTY= 3
          ELSE
            PYMAX= 100.
            IWRTY= 4
          END IF
        ELSE
          PYMAX= 100.
          IWRTY= 4
        END IF
C
C       assign grid lines & tick marks
        DO 180 I = 8, 76
          GR(I,1)  = IHL
          GR(I,5)  = IHL
          GR(I,9)  = IHL
          GR(I,13) = IHL
          GR(I,17) = IHL
          GR(I,21) = IHL
  180   CONTINUE
        DO 190 I = 7, 77, 7
          GR(I,1)  = IPL
          GR(I,5)  = IPL
          GR(I,9)  = IPL
          GR(I,13) = IPL
          GR(I,17) = IPL
          GR(I,21) = IPL
  190   CONTINUE
        DO 200 J = 2, 20
          GR(7,J)  = IVL
          GR(77,J) = IVL
  200   CONTINUE
        DO 210 J = 1, 21, 4
          GR(7,J)  = IPL
          GR(77,J) = IPL
  210   CONTINUE
C
C       assign plot symbols to coordinate points
        IF (PLTTYP.EQ.1) THEN
C         put a value in every seventh position
          K= 10
        ELSE
C         put a value in each position
          K= 70
        END IF
        DO 260 I = 1, K
          IF (PLTTYP.EQ.1) THEN
C           use interval array
            IX = I * 7 + 4
            LY = 21.5 - PC(I) * 20. / PYMAX
C           set NY to LY to delete 'bar' format
            NY = 21
          ELSE
C           use cumulative array
            IX = 7 + I
            LY = 21.5 - PCC(I) * 20. / PYMAX
            NY = LY
          END IF
          DO 250 IY = LY, NY
            GR(IX,IY) = ISTAR
  250     CONTINUE
  260   CONTINUE
C
C       label Y-axis
        IF (PLTTYP.EQ.1) THEN
          GR(1,6)  = LF
          GR(1,7)  = LR
          GR(1,8)  = LE
          GR(1,9)  = LQ
          GR(1,10) = LU
          GR(1,11) = LE
          GR(1,12) = LN
          GR(1,13) = LC
          GR(1,14) = LYY
          GR(1,16) = IPERC
        ELSE
          GR(1,1)  = LC
          GR(1,2)  = LU
          GR(1,3)  = LM
          GR(1,4)  = LU
          GR(1,5)  = LL
          GR(1,6)  = LA
          GR(1,7)  = LT
          GR(1,8)  = LI
          GR(1,9)  = LV
          GR(1,10) = LE
          GR(1,12) = LF
          GR(1,13) = LR
          GR(1,14) = LE
          GR(1,15) = LQ
          GR(1,16) = LU
          GR(1,17) = LE
          GR(1,18) = LN
          GR(1,19) = LC
          GR(1,20) = LYY
        END IF
C
        GR(5,21) = L0
C
        IF (IWRTY.EQ.1) THEN
          GR(4,1)  = L1
          GR(5,1)  = L0
          GR(5,5)  = L8
          GR(5,9)  = L6
          GR(5,13) = L4
          GR(5,17) = L2
        ELSE IF (IWRTY.EQ.2) THEN
          GR(4,1)  = L2
          GR(5,1)  = L5
          GR(4,5)  = L2
          GR(5,5)  = L0
          GR(4,9)  = L1
          GR(5,9)  = L5
          GR(4,13) = L1
          GR(5,13) = L0
          GR(5,17) = L5
        ELSE IF (IWRTY.EQ.3) THEN
          GR(4,1)  = L5
          GR(5,1)  = L0
          GR(4,5)  = L4
          GR(5,5)  = L0
          GR(4,9)  = L3
          GR(5,9)  = L0
          GR(4,13) = L2
          GR(5,13) = L0
          GR(4,17) = L1
          GR(5,17) = L0
        ELSE
          GR(3,1)  = L1
          GR(4,1)  = L0
          GR(5,1)  = L0
          GR(4,5)  = L8
          GR(5,5)  = L0
          GR(4,9)  = L6
          GR(5,9)  = L0
          GR(4,13) = L4
          GR(5,13) = L0
          GR(4,17) = L2
          GR(5,17) = L0
        END IF
C
C       write the whole array of plot symbols
        WRITE (TEROFL,2010)
        WRITE (TEROFL,2020) ((GR(I,J),I=1,80),J=1,21)
C
C       figure
        GO TO (320, 320, 320, 330, 330, 330, 340, 340, 340, 350, 350,
     1    350, 360, 360, 360, 370, 370, 370, 380, 380, 380, 390, 390,
     2    390, 400, 400, 400, 410, 410, 410), IWRTX
C
  320     WRITE (TEROFL,2030) (XLAB(I),I=1,11,2)
          GO TO 420
  330     WRITE (TEROFL,2040) (XLAB(I),I=1,11)
          GO TO 420
  340     WRITE (TEROFL,2050) (XLAB(I),I=1,11)
          GO TO 420
  350     WRITE (TEROFL,2060) (XLAB(I),I=1,11)
          GO TO 420
  360     WRITE (TEROFL,2070) (XLAB(I),I=1,11)
          GO TO 420
  370     WRITE (TEROFL,2080) (XLAB(I),I=1,11)
          GO TO 420
  380     WRITE (TEROFL,2090) (XLAB(I),I=1,11)
          GO TO 420
  390     WRITE (TEROFL,2100) (XLAB(I),I=1,11,2)
          GO TO 420
  400     WRITE (TEROFL,2110) (XLAB(I),I=1,9,2)
          GO TO 420
  410     WRITE (TEROFL,2120) (XLAB(I),I=1,9,2)
  420   CONTINUE
        WRITE(TEROFL,2130) FACTOR
C
C       write title
        WRITE (TEROFL,2021)XNAME
        WRITE (TEROFL,2015)
        IF (NUMPLT.EQ.2) THEN
C         do a cumulative plot
          PLTTYP= 2
        END IF
        NUMPLT= NUMPLT- 1
      IF (NUMPLT.GT.0) GO TO 10
C
      CALL SUBOUT
C
      RETURN
      END


C
C
C
      SUBROUTINE UMACH (N, NUNIT)
C
      INTEGER    N, NUNIT
      LOGICAL    IOPEN
C
      IF (N.GT.0) THEN
        NUNIT = 99
        INQUIRE(UNIT=99,OPENED=IOPEN)
        IF (.NOT.IOPEN) THEN
          OPEN(UNIT=99,FILE='ERROR.FIL')
        END IF
      END IF
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION N1RTY (IOPT)
C
      INTEGER    IOPT
C
      N1RTY(IOPT) = 0
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION N1RGB (IOPT)
C
      INTEGER    IOPT
C
      N1RGB(IOPT) = 0
C
      RETURN
      END
C
C
C
      SUBROUTINE PAGE
C
      RETURN
      END
C
C
C
      SUBROUTINE DBOXP
     I                 (NGROUP,NI,X,TITLE)
C
C     + + + PURPOSE + + +
C     fill common block for box plots
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NGROUP,NI(100)
      REAL*8        X(3000)
      CHARACTER*80  TITLE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NGROUP - total number of groups of samples
C     NI     - number of observations in each group
C     X      - array of data points for each observation in each group
C     TITLE  - plot label
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cestplot.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYD,COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      NBPLOT = NBPLOT + 1
C
      NG(NBPLOT)  = NGROUP
      CBT(NBPLOT) = TITLE
C
      CALL COPYI (100,NI,NIB(1,NBPLOT))
      CALL COPYD (3000,X,DBX(1,NBPLOT))
C
      RETURN
      END
C
C
C
      SUBROUTINE DPLOTP
     I                 (NDATA,NFUN,X,A,LDA,INC,RANGE,SYMBOL,
     I                  XTITLE,YTITLE,TITLE)
C
C     + + + PURPOSE + + +
C     fill common block for plots
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       NDATA,NFUN,LDA,INC
      REAL          RANGE(4)
      REAL*8        X(5000),A(5000,10)
      CHARACTER*10  SYMBOL
      CHARACTER*80  XTITLE,YTITLE,TITLE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDATA  - number of indep variable data points
C     NFUN   - number of sets of points
C     LDA    - leading dimension of a
C     X      - array of indep variable data points
C     A      - array of dependent variable values
C     INC    - increment between elements of data to be used
C     RANGE  - min x, max x, min y, max y
C     SYMBOL - symbol used to plot function
C     XTITLE - x axis label
C     YTITLE - y axis label
C     TITLE  - plot label
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cestplot.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYD
C
C     + + + END SPECIFICATIONS + + +
C
      NPLOT = NPLOT + 1
C
      ND(NPLOT)   = NDATA
      NCRV(NPLOT) = NFUN
      XT(NPLOT)   = XTITLE
      YT(NPLOT)   = YTITLE
      CT(NPLOT)   = TITLE
      SY(NPLOT)   = SYMBOL
      RMM(1)      = RANGE(1)
      RMM(2)      = RANGE(2)
      RMM(3)      = RANGE(3)
      RMM(4)      = RANGE(4)
C
      CALL COPYD (5000,X,DX(1,NPLOT))
      CALL COPYD (5000*NFUN,A,DA(1,1,NPLOT))
C
      RETURN
      END
C
C
C
      REAL*8 FUNCTION DNORIN
     I                      (PALPHA)
C
C     + + + PURPOSE + + +
C     dummy version of imsl function by the same name
C
C     + + + DUMMY ARGUMENTS + + +
      REAL*8 PALPHA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PALPHA -
C
C     + + + LOCAL VARIABLES + + +
      REAL      RTEMP
C
C     + + + FUNCTIONS + + +
      REAL      GAUSAB
C
C     + + + EXTERNAL + + +
      EXTERNAL  GAUSAB
C
C     + + + END SPECIFICATIONS + + +
C
C     use routine gausab from the stats library
      RTEMP  = PALPHA
      DNORIN = GAUSAB(RTEMP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   DSVRGN
     I                   (N,RA,
     O                    RB)
C
C     + + + PURPOSE + + +
C     sort a real array by algebraic value, ascending
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  N
      REAL*8   RA(N),RB(N)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     N      - number of elements in array
C     RA     - input array
C     RB     - output array
C
C     + + + EXTERNAL + + +
      EXTERNAL   COPYD, ASRTDP
C
C     + + + END SPECIFICATIONS + + +
C
C     use hass sorting routine
      CALL COPYD (N,RA,RB)
      CALL ASRTDP (N,
     M             RB)
C
      RETURN
      END
C
C
C
      SUBROUTINE DMRRRR
     #                 (NRA,NCA,A,LDA,NRB,NCB,B,LDB,NRC,NCC,C,LDC)
C
C     + + + PURPOSE + + +
C     multiply a matrix by a matrix
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  NRA,NCA,LDA,NRB,NCB,LDB,NRC,NCC,LDC
      REAL*8   A(LDA,NCA),B(LDB,NCB),C(LDC,NCC)
C
C     + + + LOCAL VARIABLES + + +
      REAL*8      BETA,ALPHA
      CHARACTER*1 TRANSA,TRANSB
C
C     + + + EXTERNAL + + +
      EXTERNAL  DGEMM
C
C     + + + END SPECIFICATIONS + + +
C
      ALPHA = 1.0
      BETA  = 0.0
      TRANSA= 'N'
      TRANSB= 'N'
      CALL DGEMM (TRANSA,TRANSB,NRA,NCB,NCA,
     1            ALPHA,A,LDA,B,LDB,BETA,C,LDC)
C
      RETURN
      END
C
C
C
      SUBROUTINE ISET (N,IVAL,IA,INCX)
C
C     + + + PURPOSE + + +
C     fill integer array with specified value
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   N,INCX,IVAL,IA(N)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
      CALL ZIPI (N, IVAL,
     O           IA)
C
      RETURN
      END
C
C
C
C
      SUBROUTINE DMXTYF
     #                 (NRA,NCA,A,LDA,NRB,NCB,B,LDB,NRC,NCC,C,LDC)
C
C     + + + PURPOSE + + +
C     multiply the transpose of a matrix by a matrix
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  NRA,NCA,LDA,NRB,NCB,LDB,NRC,NCC,LDC
      REAL*8   A(LDA,NCA),B(LDB,NCB),C(LDC,NCC)
C
C     + + + LOCAL VARIABLES + + +
      REAL*8      BETA,ALPHA
      CHARACTER*1 TRANSA,TRANSB
C
C     + + + EXTERNAL + + +
      EXTERNAL  DGEMM
C
C     + + + END SPECIFICATIONS + + +
C
      ALPHA = 1.0
      BETA  = 0.0
      TRANSA= 'T'
      TRANSB= 'N'
      CALL DGEMM (TRANSA,TRANSB,NCA,NCB,NRA,
     1            ALPHA,A,LDA,B,LDB,BETA,C,NRA)
C
      RETURN
      END
C
C
C
      SUBROUTINE   DLINRG
     #                   (N,A,LDA,AINV,LDAINV)
C
C     + + + PURPOSE + + +
C     compute the inverse of a general matrix
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  N,LDA,LDAINV
      REAL*8   A(LDA,LDA),AINV(LDAINV,LDAINV)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     N      - order of the matrix a
C     A      - n by n matrix to be converted
C     LDA    - leading dimension of a
C     AINV   - n by n matrix of inverse
C     LDAINV - leading dimension of ainv
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   W(25)
C
C     + + + EXTERNAL + + +
      EXTERNAL  VERT
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,LDA
        DO 20 J = 1,LDA
          AINV(I,J) = A(I,J)
 20     CONTINUE
 10   CONTINUE
C
      CALL VERT(AINV,LDA,N,W)
      LDAINV = LDA
C
      RETURN
      END
C
C
C
      DOUBLE PRECISION   FUNCTION   DNORDF
     I                                    (ARG)
C
C     + + + PURPOSE + + +
C     dummy version of imsl function by the same name
C
C     + + + DUMMY ARGUMENTS + + +
      DOUBLE PRECISION   ARG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ARG    -
C
C     + + + LOCAL VARIABLES + + +
      REAL      RTEMP
C
C     + + + FUNCTIONS + + +
      REAL      GAUSCF
C
C     + + + EXTERNAL + + +
      EXTERNAL  GAUSCF
C
C     + + + END SPECIFICATIONS + + +
C
C     use routine gauscf from the stats library
      RTEMP  = ARG
      DNORDF = GAUSCF(RTEMP)
C
      RETURN
      END
C
C
C
      DOUBLE PRECISION   FUNCTION   DCHIDF
     I                                    (XSQ,DF)
C
C     + + + PURPOSE + + +
C     evaluate chi square distribution
C
C     + + + DUMMY ARGUMENTS + + +
      DOUBLE PRECISION   XSQ,DF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XSQ    -
C     DF     -
C
C     + + + LOCAL VARIABLES + + +
      INTEGER NU
      REAL*4  CDF,X
C
C     + + + EXTERNALS + + +
      EXTERNAL  CHSCDF
C
C     + + + END SPECIFICATIONS + + +
C
      NU = INT(DF)
      X  = XSQ
C
      CALL CHSCDF(X,NU,CDF)
      DCHIDF = CDF
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION NDAYS (IDAY,IMONTH,IYEAR)
C
C     + + + PURPOSE + + +
C     calculate the number of days since 1/1/1900
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IDAY,IMONTH,IYEAR
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DATE1(6),DATE2(6),TCODE,TSTEP,NVALS
C
C     + + + EXTERNALS + + +
      EXTERNAL  TIMDIF
C
C     + + + END SPECIFICATIONS + + +
C
      DATE1(1) = 1900
      DATE1(2) = 1
      DATE1(3) = 1
      DATE1(4) = 0
      DATE1(5) = 0
      DATE1(6) = 0
      DATE2(1) = IYEAR
      DATE2(2) = IMONTH
      DATE2(3) = IDAY
      DATE2(4) = 0
      DATE2(5) = 0
      DATE2(6) = 0
      TCODE    = 4
      TSTEP    = 1
C
      CALL TIMDIF (DATE1,DATE2,TCODE,TSTEP,
     O             NVALS)
      NDAYS = NVALS
C
      RETURN
      END
C
C
C
      SUBROUTINE VERT(V,LV,N,W)
C     module VERT from package NAPACK.
C      ________________________________________________________
C     |                                                        |
C     |                INVERT A GENERAL MATRIX                 |
C     |                                                        |
C     |    INPUT:                                              |
C     |                                                        |
C     |         V     --ARRAY CONTAINING MATRIX                |
C     |                                                        |
C     |         LV    --LEADING (ROW) DIMENSION OF ARRAY V     |
C     |                                                        |
C     |         N     --DIMENSION OF MATRIX STORED IN ARRAY V  |
C     |                                                        |
C     |         W     --INTEGER WORK ARRAY WITH AT LEAST N-1   |
C     |                      ELEMENTS                          |
C     |                                                        |
C     |    OUTPUT:                                             |
C     |                                                        |
C     |         V     --INVERSE                                |
C     |                                                        |
C     |    BUILTIN FUNCTIONS: ABS                              |
C     |________________________________________________________|
      REAL*8 V(LV,LV),S,T
      INTEGER W(25),I,J,K,L,M,N,P
      IF ( N .EQ. 1 ) GOTO 110
      L = 0
      M = 1
10    IF ( L .EQ. N ) GOTO 90
      K = L
      L = M
      M = M + 1
C     ---------------------------------------
C     |*** FIND PIVOT AND START ROW SWAP ***|
C     ---------------------------------------
      P = L
      IF ( M .GT. N ) GOTO 30
      S = ABS(V(L,L))
      DO 20 I = M,N
           T = ABS(V(I,L))
           IF ( T .LE. S ) GOTO 20
           P = I
           S = T
20    CONTINUE
      W(L) = P
30    S = V(P,L)
      V(P,L) = V(L,L)
      IF ( S .EQ. 0. ) GOTO 120
C     -----------------------------
C     |*** COMPUTE MULTIPLIERS ***|
C     -----------------------------
      V(L,L) = -1.
      S = 1./S
      DO 40 I = 1,N
40         V(I,L) = -S*V(I,L)
      J = L
50    J = J + 1
      IF ( J .GT. N ) J = 1
      IF ( J .EQ. L ) GOTO 10
      T = V(P,J)
      V(P,J) = V(L,J)
      V(L,J) = T
      IF ( T .EQ. 0. ) GOTO 50
C     ------------------------------
C     |*** ELIMINATE BY COLUMNS ***|
C     ------------------------------
      IF ( K .EQ. 0 ) GOTO 70
      DO 60 I = 1,K
60         V(I,J) = V(I,J) + T*V(I,L)
70    V(L,J) = S*T
      IF ( M .GT. N ) GOTO 50
      DO 80 I = M,N
80         V(I,J) = V(I,J) + T*V(I,L)
      GOTO 50
C     -----------------------
C     |*** PIVOT COLUMNS ***|
C     -----------------------
90    L = W(K)
      DO 100 I = 1,N
           T = V(I,L)
           V(I,L) = V(I,K)
100        V(I,K) = T
      K = K - 1
      IF ( K .GT. 0 ) GOTO 90
      RETURN
110   IF ( V(1,1) .EQ. 0. ) GOTO 120
      V(1,1) = 1./V(1,1)
      RETURN
120   WRITE(*,*) 'ERROR: MATRIX HAS NO INVERSE'
      END
C
C
C
      SUBROUTINE CHSCDF(X,NU,CDF)
C
C     module CHSCDF from package DATAPAC.
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE CHI-SQUARED DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM PARAMETER = NU.
C              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X.
C              THE PROBABILITY DENSITY FUNCTION IS GIVEN
C              IN THE REFERENCES BELOW. 
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE NON-NEGATIVE.
C                     --NU     = THE INTEGER NUMBER OF DEGREES
C                                OF FREEDOM.
C                                NU SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF FOR THE CHI-SQUARED DISTRIBUTION
C             WITH DEGREES OF FREEDOM PARAMETER = NU.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE NON-NEGATIVE.
C                 --NU SHOULD BE A POSITIVE INTEGER VARIABLE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NORCDF. 
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DSQRT, DEXP.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 941, FORMULAE 26.4.4 AND 26.4.5.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGE 176,
C                 FORMULA 28, AND PAGE 180, FORMULA 33.1.
C               --OWEN, HANDBOOK OF STATISTICAL TABLES,
C                 1962, PAGES 50-55.
C               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
C                 FOR STATISTICIANS, VOLUME 1, 1954,
C                 PAGES 122-131.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--JUNE      1972. 
C     UPDATED         --MAY       1974. 
C     UPDATED         --SEPTEMBER 1975. 
C     UPDATED         --NOVEMBER  1975. 
C     UPDATED         --OCTOBER   1976. 
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX,PI,CHI,SUM,TERM,AI,DCDFN
      DOUBLE PRECISION DNU
      DOUBLE PRECISION DSQRT,DEXP
      DOUBLE PRECISION DLOG
      DOUBLE PRECISION DFACT,DPOWER
      DOUBLE PRECISION DW
      DOUBLE PRECISION D1,D2,D3
      DOUBLE PRECISION TERM0,TERM1,TERM2,TERM3,TERM4
      DOUBLE PRECISION B11
      DOUBLE PRECISION B21
      DOUBLE PRECISION B31,B32
      DOUBLE PRECISION B41,B42,B43
      DATA NUCUT/1000/
      DATA PI/3.14159265358979D0/
      DATA DPOWER/0.33333333333333D0/
      DATA B11/0.33333333333333D0/
      DATA B21/-0.02777777777778D0/
      DATA B31/-0.00061728395061D0/
      DATA B32/-13.0D0/
      DATA B41/0.00018004115226D0/
      DATA B42/6.0D0/
      DATA B43/17.0D0/
C
      IPR=6
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(NU.LE.0)GOTO50
      IF(X.LT.0.0)GOTO55
      GOTO90
   50 WRITE(IPR,15) 
      WRITE(IPR,47)NU
      CDF=0.0
      RETURN
   55 WRITE(IPR,4)
      WRITE(IPR,46)X
      CDF=0.0
      RETURN
   90 CONTINUE
    4 FORMAT(1H , 96H***** NON-FATAL DIAGNOSTIC--THE FIRST  INPUT ARGUME
     1NT TO THE CHSCDF SUBROUTINE IS NEGATIVE *****)
   15 FORMAT(1H , 91H***** FATAL ERROR--THE SECOND INPUT ARGUMENT TO THE
     1 CHSCDF SUBROUTINE IS NON-POSITIVE *****)
   46 FORMAT(1H , 35H***** THE VALUE OF THE ARGUMENT IS ,E15.8,6H *****)
   47 FORMAT(1H , 35H***** THE VALUE OF THE ARGUMENT IS ,I8   ,6H *****)
C
C-----START POINT-----------------------------------------------------
C
      DX=X
      ANU=NU
      DNU=NU
C
C     IF X IS NON-POSITIVE, SET CDF = 0.0 AND RETURN.
C     IF NU IS SMALLER THAN 10 AND X IS MORE THAN 200
C     STANDARD DEVIATIONS BELOW THE MEAN,
C     SET CDF = 0.0 AND RETURN.
C     IF NU IS 10 OR LARGER AND X IS MORE THAN 100
C     STANDARD DEVIATIONS BELOW THE MEAN,
C     SET CDF = 0.0 AND RETURN.
C     IF NU IS SMALLER THAN 10 AND X IS MORE THAN 200
C     STANDARD DEVIATIONS ABOVE THE MEAN,
C     SET CDF = 1.0 AND RETURN.
C     IF NU IS 10 OR LARGER AND X IS MORE THAN 100
C     STANDARD DEVIATIONS ABOVE THE MEAN,
C     SET CDF = 1.0 AND RETURN.
C
      IF(X.LE.0.0)GOTO105
      AMEAN=ANU
      SD=SQRT(2.0*ANU)
      Z=(X-AMEAN)/SD
      IF(NU.LT.10.AND.Z.LT.-200.0)GOTO105
      IF(NU.GE.10.AND.Z.LT.-100.0)GOTO105
      IF(NU.LT.10.AND.Z.GT.200.0)GOTO107
      IF(NU.GE.10.AND.Z.GT.100.0)GOTO107
      GOTO109
  105 CDF=0.0
      RETURN
  107 CDF=1.0
      RETURN
  109 CONTINUE
C
C     DISTINGUISH BETWEEN 3 SEPARATE REGIONS
C     OF THE (X,NU) SPACE.
C     BRANCH TO THE PROPER COMPUTATIONAL METHOD
C     DEPENDING ON THE REGION.
C     NUCUT HAS THE VALUE 1000.
C
      IF(NU.LT.NUCUT)GOTO1000 
      IF(NU.GE.NUCUT.AND.X.LE.ANU)GOTO2000
      IF(NU.GE.NUCUT.AND.X.GT.ANU)GOTO3000
      IBRAN=1
      WRITE(IPR,99)IBRAN
   99 FORMAT(1H ,42H*****INTERNAL ERROR IN CHSCDF SUBROUTINE--,
     146HIMPOSSIBLE BRANCH CONDITION AT BRANCH POINT = ,I8) 
      RETURN
C
C     TREAT THE SMALL AND MODERATE DEGREES OF FREEDOM CASE
C     (THAT IS, WHEN NU IS SMALLER THAN 1000).
C     METHOD UTILIZED--EXACT FINITE SUM 
C     (SEE AMS 55, PAGE 941, FORMULAE 26.4.4 AND 26.4.5).
C
 1000 CONTINUE
      CHI=DSQRT(DX) 
      IEVODD=NU-2*(NU/2)
      IF(IEVODD.EQ.0)GOTO120
C
      SUM=0.0D0
      TERM=1.0/CHI
      IMIN=1
      IMAX=NU-1
      GOTO130
C
  120 SUM=1.0D0
      TERM=1.0D0
      IMIN=2
      IMAX=NU-2
C
  130 IF(IMIN.GT.IMAX)GOTO160 
      DO100I=IMIN,IMAX,2
      AI=I
      TERM=TERM*(DX/AI)
      SUM=SUM+TERM
  100 CONTINUE
  160 CONTINUE
C
      SUM=SUM*DEXP(-DX/2.0D0) 
      IF(IEVODD.EQ.0)GOTO170
      SUM=(DSQRT(2.0D0/PI))*SUM
      SPCHI=CHI
      CALL NORCDF(SPCHI,CDFN) 
      DCDFN=CDFN
      SUM=SUM+2.0D0*(1.0D0-DCDFN)
  170 CDF=1.0D0-SUM 
      RETURN
C
C     TREAT THE CASE WHEN NU IS LARGE
C     (THAT IS, WHEN NU IS EQUAL TO OR GREATER THAN 1000)
C     AND X IS LESS THAN OR EQUAL TO NU.
C     METHOD UTILIZED--WILSON-HILFERTY APPROXIMATION
C     (SEE JOHNSON AND KOTZ, VOLUME 1, PAGE 176, FORMULA 28).
C
 2000 CONTINUE
      DFACT=4.5D0*DNU
      U=(((DX/DNU)**DPOWER)-1.0D0+(1.0D0/DFACT))*DSQRT(DFACT)
      CALL NORCDF(U,CDFN)
      CDF=CDFN
      RETURN
C
C     TREAT THE CASE WHEN NU IS LARGE
C     (THAT IS, WHEN NU IS EQUAL TO OR GREATER THAN 1000)
C     AND X IS LARGER THAN NU.
C     METHOD UTILIZED--HILL'S ASYMPTOTIC EXPANSION
C     (SEE JOHNSON AND KOTZ, VOLUME 1, PAGE 180, FORMULA 33.1).
C
 3000 CONTINUE
      DW=DSQRT(DX-DNU-DNU*DLOG(DX/DNU)) 
      DANU=DSQRT(2.0D0/DNU)
      D1=DW
      D2=DW**2
      D3=DW**3
      TERM0=DW
      TERM1=B11*DANU
      TERM2=B21*D1*(DANU**2)
      TERM3=B31*(D2+B32)*(DANU**3)
      TERM4=B41*(B42*D3+B43*D1)*(DANU**4)
      U=TERM0+TERM1+TERM2+TERM3+TERM4
      CALL NORCDF(U,CDFN)
      CDF=CDFN
      RETURN
C
      END
C
C
C
      SUBROUTINE NORCDF(X,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE NORMAL (GAUSSIAN)
C              DISTRIBUTION WITH MEAN = 0 AND STANDARD DEVIATION = 1. 
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = (1/SQRT(2*PI))*EXP(-X*X/2). 
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 932, FORMULA 26.2.17.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 40-111.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--JUNE      1972. 
C     UPDATED         --SEPTEMBER 1975. 
C     UPDATED         --NOVEMBER  1975. 
C
C---------------------------------------------------------------------
C
      DATA B1,B2,B3,B4,B5,P/.319381530,-0.356563782,1.781477937,-1.82125
     15978,1.330274429,.2316419/
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C     NO INPUT ARGUMENT ERRORS POSSIBLE 
C     FOR THIS DISTRIBUTION.
C
C-----START POINT-----------------------------------------------------
C
      Z=X 
      IF(X.LT.0.0)Z=-Z
      T=1.0/(1.0+P*Z)
      CDF=1.0-((0.39894228040143  )*EXP(-0.5*Z*Z))*(B1*T+B2*T**2+B3*T**3
     1+B4*T**4+B5*T**5)
      IF(X.LT.0.0)CDF=1.0-CDF 
C
      RETURN
      END 
C
C
C
      SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
C
C     module DMR from package BLAS.
C
C     .. Parameters ..
      DOUBLE PRECISION ONE, ZERO
      PARAMETER        (ONE=1.0D+0,ZERO=0.0D+0)
      INTEGER          MB, NB, KB
      PARAMETER        (MB=64,NB=MB,KB=64)
C     .. Scalar Arguments ..
      DOUBLE PRECISION ALPHA, BETA
      INTEGER          K, LDA, LDB, LDC, M, N
      CHARACTER*1      TRANSA, TRANSB
C     .. Array Arguments ..
C
C     Purpose
C     =======
C
C     DGEMM  performs one of the matrix-matrix operations
C
C     C := alpha*op( A )*op( B ) + beta*C,
C
C     where  op( X ) is one of
C
C     op( X ) = X   or   op( X ) = X',
C
C     alpha and beta are scalars, and A, B and C are matrices, with op( A )
C     an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
C
C     Parameters
C     ==========
C
C     TRANSA - CHARACTER*1.
C     On entry, TRANSA specifies the form of op( A ) to be used in
C     the matrix multiplication as follows:
C
C     TRANSA = 'N' or 'n',  op( A ) = A.
C
C     TRANSA = 'T' or 't',  op( A ) = A'.
C
C     TRANSA = 'C' or 'c',  op( A ) = A'.
C
C     Unchanged on exit.
C
C     TRANSB - CHARACTER*1.
C     On entry, TRANSB specifies the form of op( B ) to be used in
C     the matrix multiplication as follows:
C
C     TRANSB = 'N' or 'n',  op( B ) = B.
C
C     TRANSB = 'T' or 't',  op( B ) = B'.
C
C     TRANSB = 'C' or 'c',  op( B ) = B'.
C
C     Unchanged on exit.
C
C     M      - INTEGER.
C     On entry,  M  specifies  the number  of rows  of the  matrix
C     op( A )  and of the  matrix  C.  M  must  be at least  zero.
C     Unchanged on exit.
C
C     N      - INTEGER.
C     On entry,  N  specifies the number  of columns of the matrix
C     op( B ) and the number of columns of the matrix C. N must be
C     at least zero.
C     Unchanged on exit.
C
C     K      - INTEGER.
C     On entry,  K  specifies  the number of columns of the matrix
C     op( A ) and the number of rows of the matrix op( B ). K must
C     be at least  zero.
C     Unchanged on exit.
C
C     ALPHA  - DOUBLE PRECISION.
C     On entry, ALPHA specifies the scalar alpha.
C     Unchanged on exit.
C
C     A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
C     k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
C     Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
C     part of the array  A  must contain the matrix  A,  otherwise
C     the leading  k by m  part of the array  A  must contain  the
C     matrix A.
C     Unchanged on exit.
C
C     LDA    - INTEGER.
C     On entry, LDA specifies the first dimension of A as declared
C     in the calling (sub) program. When  TRANSA = 'N' or 'n' then
C     LDA must be at least  max( 1, m ), otherwise  LDA must be at
C     least  max( 1, k ).
C     Unchanged on exit.
C
C     B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
C     n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
C     Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
C     part of the array  B  must contain the matrix  B,  otherwise
C     the leading  n by k  part of the array  B  must contain  the
C     matrix B.
C     Unchanged on exit.
C
C     LDB    - INTEGER.
C     On entry, LDB specifies the first dimension of B as declared
C     in the calling (sub) program. When  TRANSB = 'N' or 'n' then
C     LDB must be at least  max( 1, k ), otherwise  LDB must be at
C     least  max( 1, n ).
C     Unchanged on exit.
C
C     BETA   - DOUBLE PRECISION.
C     On entry,  BETA  specifies the scalar  beta.  When  BETA  is
C     supplied as zero then C need not be set on input.
C     Unchanged on exit.
C
C     C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
C     Before entry, the leading  m by n  part of the array  C must
C     contain the matrix  C,  except when  beta  is zero, in which
C     case C need not be set on entry.
C     On exit, the array  C  is overwritten by the  m by n  matrix
C     ( alpha*op( A )*op( B ) + beta*C ).
C
C     LDC    - INTEGER.
C     On entry, LDC specifies the first dimension of C as declared
C     in  the  calling  (sub)  program.   LDC  must  be  at  least
C     max( 1, m ).
C     Unchanged on exit.
C
C
C     Level 3 Blas routine.
C
C     -- Written on 8-February-1989.
C     Jack Dongarra, Argonne National Laboratory.
C     Iain Duff, AERE Harwell.
C     Jeremy Du Croz, Numerical Algorithms Group Ltd.
C     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     This code comes from a report entitled:
*     The IBM RISC System/6000 and Linear Algebra Operations, by
*     Jack J. Dongarra, Peter Mayes, and Giuseppe Radicati di Brozolo,
*     University of Tennessee Computer Science Tech Report: CS - 90 - 122.
C
C
      DOUBLE PRECISION A(LDA,*), B(LDB,*), C(LDC,*)
C     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
C     .. Local Scalars ..
      DOUBLE PRECISION T11, T12, T21, T22
      INTEGER          I, IDEPTH, II, ILEN, INFO, ISPAN, J, JDEPTH, JJ,
     *                 JLEN, JSPAN, L, LL, LSPAN, NROWA, NROWB
      LOGICAL          NOTA, NOTB
C     .. Local Arrays ..
      DOUBLE PRECISION CH(KB,MB), CH1(KB), CH2(KB)
C     .. Executable Statements ..
C
C     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
C     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
C     and  columns of  A  and the  number of  rows  of  B  respectively.
C
      IF (TRANSA.EQ.'N') THEN
        NOTA = .TRUE.
      ELSE
        NOTA = .FALSE.
      END IF
      IF (TRANSB.EQ.'N') THEN
        NOTB = .TRUE.
      ELSE
        NOTB = .FALSE.
      END IF
C
      IF (NOTA) THEN
         NROWA = M
      ELSE
         NROWA = K
      END IF
      IF (NOTB) THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
C
C     Test the input parameters.
C
      INFO = 0
      IF (( .NOT. NOTA) .AND. (TRANSA.NE.'C')
     *    .AND. (TRANSA.NE.'T')) THEN
         INFO = 1
      ELSE IF (( .NOT. NOTB) .AND. (TRANSB.NE.'C')
     *         .AND. (TRANSB.NE.'T')) THEN
         INFO = 2
      ELSE IF (M.LT.0) THEN
         INFO = 3
      ELSE IF (N.LT.0) THEN
         INFO = 4
      ELSE IF (K.LT.0) THEN
         INFO = 5
      ELSE IF (LDA.LT.MAX(1,NROWA)) THEN
         INFO = 8
      ELSE IF (LDB.LT.MAX(1,NROWB)) THEN
         INFO = 10
      ELSE IF (LDC.LT.MAX(1,M)) THEN
         INFO = 13
      END IF
      IF (INFO.NE.0) THEN
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ((M.EQ.0) .OR. (N.EQ.0) .OR. (((ALPHA.EQ.ZERO) .OR. (K.EQ.0))
     *     .AND. (BETA.EQ.ONE))) RETURN
      IF (BETA.EQ.ZERO) THEN
         DO 40 J = 1, N
            DO 20 I = 1, M
               C(I,J) = ZERO
   20       CONTINUE
   40    CONTINUE
      ELSE
         DO 80 J = 1, N
            DO 60 I = 1, M
               C(I,J) = BETA*C(I,J)
   60       CONTINUE
   80    CONTINUE
      END IF
C
C     And if  alpha.eq.zero.
C
      IF (ALPHA.EQ.ZERO) RETURN
C
C     Start the operations.
C
      IF (NOTB) THEN
         IF (NOTA) THEN
C
C           Form  C := C + alpha*A*B.
C
            DO 380 L = 1, K, KB
               LSPAN = MIN(KB,K-L+1)
               DO 360 I = 1, M, MB
                  IDEPTH = 2
                  ISPAN = MIN(MB,M-I+1)
                  ILEN = IDEPTH*(ISPAN/IDEPTH)
                  DO 120 II = I, I + ISPAN - 1
                     DO 100 LL = L, L + LSPAN - 1
                        CH(LL-L+1,II-I+1) = ALPHA*A(II,LL)
  100                CONTINUE
  120             CONTINUE
                  DO 340 J = 1, N, NB
                     JDEPTH = 2
                     JSPAN = MIN(NB,N-J+1)
                     JLEN = JDEPTH*(JSPAN/JDEPTH)
                     DO 220 JJ = J, J + JLEN - 1, JDEPTH
                        DO 160 II = I, I + ILEN - 1, IDEPTH
                           T11 = ZERO
                           T21 = ZERO
                           T12 = ZERO
                           T22 = ZERO
                           DO 140 LL = L, L + LSPAN - 1
                              T11 = T11 + CH(LL-L+1,II-I+1)*B(LL,JJ)
                              T21 = T21 + CH(LL-L+1,II-I+2)*B(LL,JJ)
                              T12 = T12 + CH(LL-L+1,II-I+1)*B(LL,JJ+1)
                              T22 = T22 + CH(LL-L+1,II-I+2)*B(LL,JJ+1)
  140                      CONTINUE
                           C(II,JJ) = C(II,JJ) + T11
                           C(II+1,JJ) = C(II+1,JJ) + T21
                           C(II,JJ+1) = C(II,JJ+1) + T12
                           C(II+1,JJ+1) = C(II+1,JJ+1) + T22
  160                   CONTINUE
                        IF (ILEN.LT.ISPAN) THEN
                           DO 200 II = I + ILEN, I + ISPAN - 1
                              T11 = ZERO
                              T12 = ZERO
                              DO 180 LL = L, L + LSPAN - 1
                                 T11 = T11 + CH(LL-L+1,II-I+1)*B(LL,JJ)
                                 T12 = T12 + CH(LL-L+1,II-I+1)*B(LL,
     *                                 JJ+1)
  180                         CONTINUE
                              C(II,JJ) = C(II,JJ) + T11
                              C(II,JJ+1) = C(II,JJ+1) + T12
  200                      CONTINUE
                        END IF
  220                CONTINUE
                     IF (JLEN.LT.JSPAN) THEN
                        DO 320 JJ = J + JLEN, J + JSPAN - 1
                           DO 260 II = I, I + ILEN - 1, IDEPTH
                              T11 = ZERO
                              T21 = ZERO
                              DO 240 LL = L, L + LSPAN - 1
                                 T11 = T11 + CH(LL-L+1,II-I+1)*B(LL,JJ)
                                 T21 = T21 + CH(LL-L+1,II-I+2)*B(LL,JJ)
  240                         CONTINUE
                              C(II,JJ) = C(II,JJ) + T11
                              C(II+1,JJ) = C(II+1,JJ) + T21
  260                      CONTINUE
                           IF (ILEN.LT.ISPAN) THEN
                              DO 300 II = I + ILEN, I + ISPAN - 1
                                 T11 = ZERO
                                 DO 280 LL = L, L + LSPAN - 1
                                    T11 = T11 + CH(LL-L+1,II-I+1)*B(LL,
     *                                    JJ)
  280                            CONTINUE
                                 C(II,JJ) = C(II,JJ) + T11
  300                         CONTINUE
                           END IF
  320                   CONTINUE
                     END IF
  340             CONTINUE
  360          CONTINUE
  380       CONTINUE
         ELSE
C
C           Form  C := C + alpha*A'*B
C
            DO 680 I = 1, M, MB
               IDEPTH = 2
               ISPAN = MIN(MB,M-I+1)
               ILEN = IDEPTH*(ISPAN/IDEPTH)
               DO 660 L = 1, K, KB
                  LSPAN = MIN(KB,K-L+1)
                  DO 420 II = I, I + ISPAN - 1
                     DO 400 LL = L, L + LSPAN - 1
                        CH(LL-L+1,II-I+1) = ALPHA*A(LL,II)
  400                CONTINUE
  420             CONTINUE
                  DO 640 J = 1, N, NB
                     JDEPTH = 2
                     JSPAN = MIN(NB,N-J+1)
                     JLEN = JDEPTH*(JSPAN/JDEPTH)
                     DO 520 JJ = J, J + JLEN - 1, JDEPTH
                        DO 460 II = I, I + ILEN - 1, IDEPTH
                           T11 = ZERO
                           T21 = ZERO
                           T12 = ZERO
                           T22 = ZERO
                           DO 440 LL = L, L + LSPAN - 1
                              T11 = T11 + CH(LL-L+1,II-I+1)*B(LL,JJ)
                              T21 = T21 + CH(LL-L+1,II-I+2)*B(LL,JJ)
                              T12 = T12 + CH(LL-L+1,II-I+1)*B(LL,JJ+1)
                              T22 = T22 + CH(LL-L+1,II-I+2)*B(LL,JJ+1)
  440                      CONTINUE
                           C(II,JJ) = C(II,JJ) + T11
                           C(II+1,JJ) = C(II+1,JJ) + T21
                           C(II,JJ+1) = C(II,JJ+1) + T12
                           C(II+1,JJ+1) = C(II+1,JJ+1) + T22
  460                   CONTINUE
                        IF (ILEN.LT.ISPAN) THEN
                           DO 500 II = I + ILEN, I + ISPAN - 1
                              T11 = ZERO
                              T12 = ZERO
                              DO 480 LL = L, L + LSPAN - 1
                                 T11 = T11 + CH(LL-L+1,II-I+1)*B(LL,JJ)
                                 T12 = T12 + CH(LL-L+1,II-I+1)*B(LL,
     *                                 JJ+1)
  480                         CONTINUE
                              C(II,JJ) = C(II,JJ) + T11
                              C(II,JJ+1) = C(II,JJ+1) + T12
  500                      CONTINUE
                        END IF
  520                CONTINUE
                     IF (JLEN.LT.JSPAN) THEN
                        DO 620 JJ = J + JLEN, J + JSPAN - 1
                           DO 560 II = I, I + ILEN - 1, IDEPTH
                              T11 = ZERO
                              T21 = ZERO
                              DO 540 LL = L, L + LSPAN - 1
                                 T11 = T11 + CH(LL-L+1,II-I+1)*B(LL,JJ)
                                 T21 = T21 + CH(LL-L+1,II-I+2)*B(LL,JJ)
  540                         CONTINUE
                              C(II,JJ) = C(II,JJ) + T11
                              C(II+1,JJ) = C(II+1,JJ) + T21
  560                      CONTINUE
                           IF (ILEN.LT.ISPAN) THEN
                              DO 600 II = I + ILEN, I + ISPAN - 1
                                 T11 = ZERO
                                 DO 580 LL = L, L + LSPAN - 1
                                    T11 = T11 + CH(LL-L+1,II-I+1)*B(LL,
     *                                    JJ)
  580                            CONTINUE
                                 C(II,JJ) = C(II,JJ) + T11
  600                         CONTINUE
                           END IF
  620                   CONTINUE
                     END IF
  640             CONTINUE
  660          CONTINUE
  680       CONTINUE
         END IF
      ELSE
         IF (NOTA) THEN
C
C           Form  C := C + alpha*A*B'
C
            DO 1000 J = 1, N, NB
               JDEPTH = 2
               JSPAN = MIN(NB,N-J+1)
               JLEN = JDEPTH*(JSPAN/JDEPTH)
               DO 980 L = 1, K, KB
                  LSPAN = MIN(KB,K-L+1)
                  DO 720 JJ = J, J + JSPAN - 1
                     DO 700 LL = L, L + LSPAN - 1
                        CH(LL-L+1,JJ-J+1) = ALPHA*B(JJ,LL)
  700                CONTINUE
  720             CONTINUE
                  DO 960 I = 1, M, MB
                     IDEPTH = 2
                     ISPAN = MIN(MB,M-I+1)
                     ILEN = IDEPTH*(ISPAN/IDEPTH)
                     DO 840 II = I, I + ILEN - 1, IDEPTH
                        DO 740 LL = L, L + LSPAN - 1
                           CH1(LL-L+1) = A(II,LL)
                           CH2(LL-L+1) = A(II+1,LL)
  740                   CONTINUE
                        DO 780 JJ = J, J + JLEN - 1, JDEPTH
                           T11 = ZERO
                           T21 = ZERO
                           T12 = ZERO
                           T22 = ZERO
                           DO 760 LL = L, L + LSPAN - 1
                              T11 = T11 + CH1(LL-L+1)*CH(LL-L+1,JJ-J+1)
                              T21 = T21 + CH2(LL-L+1)*CH(LL-L+1,JJ-J+1)
                              T12 = T12 + CH1(LL-L+1)*CH(LL-L+1,JJ-J+2)
                              T22 = T22 + CH2(LL-L+1)*CH(LL-L+1,JJ-J+2)
  760                      CONTINUE
                           C(II,JJ) = C(II,JJ) + T11
                           C(II+1,JJ) = C(II+1,JJ) + T21
                           C(II,JJ+1) = C(II,JJ+1) + T12
                           C(II+1,JJ+1) = C(II+1,JJ+1) + T22
  780                   CONTINUE
                        IF (JLEN.LT.JSPAN) THEN
                           DO 820 JJ = J + JLEN, J + JSPAN - 1
                              T11 = ZERO
                              T21 = ZERO
                              DO 800 LL = L, L + LSPAN - 1
                                 T11 = T11 + A(II,LL)*CH(LL-L+1,JJ-J+1)
                                 T21 = T21 + A(II+1,LL)*CH(LL-L+1,
     *                                 JJ-J+1)
  800                         CONTINUE
                              C(II,JJ) = C(II,JJ) + T11
                              C(II+1,JJ) = C(II+1,JJ) + T21
  820                      CONTINUE
                        END IF
  840                CONTINUE
                     IF (ILEN.LT.ISPAN) THEN
                        DO 940 II = I + ILEN, I + ISPAN - 1
                           DO 880 JJ = J, J + JLEN - 1, JDEPTH
                              T11 = ZERO
                              T12 = ZERO
                              DO 860 LL = L, L + LSPAN - 1
                                 T11 = T11 + A(II,LL)*CH(LL-L+1,JJ-J+1)
                                 T12 = T12 + A(II,LL)*CH(LL-L+1,JJ-J+2)
  860                         CONTINUE
                              C(II,JJ) = C(II,JJ) + T11
                              C(II,JJ+1) = C(II,JJ+1) + T12
  880                      CONTINUE
                           IF (JLEN.LT.JSPAN) THEN
                              DO 920 JJ = J + JLEN, J + JSPAN - 1
                                 T11 = ZERO
                                 DO 900 LL = L, L + LSPAN - 1
                                    T11 = T11 + A(II,LL)*CH(LL-L+1,
     *                                    JJ-J+1)
  900                            CONTINUE
                                 C(II,JJ) = C(II,JJ) + T11
  920                         CONTINUE
                           END IF
  940                   CONTINUE
                     END IF
  960             CONTINUE
  980          CONTINUE
 1000       CONTINUE
         ELSE
C
C           Form  C := C + alpha*A'*B'
C
            DO 1300 J = 1, N, NB
               JDEPTH = 2
               JSPAN = MIN(NB,N-J+1)
               JLEN = JDEPTH*(JSPAN/JDEPTH)
               DO 1280 L = 1, K, KB
                  LSPAN = MIN(KB,K-L+1)
                  DO 1040 JJ = J, J + JSPAN - 1
                     DO 1020 LL = L, L + LSPAN - 1
                        CH(LL-L+1,JJ-J+1) = ALPHA*B(JJ,LL)
 1020                CONTINUE
 1040             CONTINUE
                  DO 1260 I = 1, M, MB
                     IDEPTH = 2
                     ISPAN = MIN(MB,M-I+1)
                     ILEN = IDEPTH*(ISPAN/IDEPTH)
                     DO 1140 II = I, I + ILEN - 1, IDEPTH
                        DO 1080 JJ = J, J + JLEN - 1, JDEPTH
                           T11 = ZERO
                           T21 = ZERO
                           T12 = ZERO
                           T22 = ZERO
                           DO 1060 LL = L, L + LSPAN - 1
                              T11 = T11 + A(LL,II)*CH(LL-L+1,JJ-J+1)
                              T21 = T21 + A(LL,II+1)*CH(LL-L+1,JJ-J+1)
                              T12 = T12 + A(LL,II)*CH(LL-L+1,JJ-J+2)
                              T22 = T22 + A(LL,II+1)*CH(LL-L+1,JJ-J+2)
 1060                      CONTINUE
                           C(II,JJ) = C(II,JJ) + T11
                           C(II+1,JJ) = C(II+1,JJ) + T21
                           C(II,JJ+1) = C(II,JJ+1) + T12
                           C(II+1,JJ+1) = C(II+1,JJ+1) + T22
 1080                   CONTINUE
                        IF (JLEN.LT.JSPAN) THEN
                           DO 1120 JJ = J + JLEN, J + JSPAN - 1
                              T11 = ZERO
                              T21 = ZERO
                              DO 1100 LL = L, L + LSPAN - 1
                                 T11 = T11 + A(LL,II)*CH(LL-L+1,JJ-J+1)
                                 T21 = T21 + A(LL,II+1)*CH(LL-L+1,
     *                                 JJ-J+1)
 1100                         CONTINUE
                              C(II,JJ) = C(II,JJ) + T11
                              C(II+1,JJ) = C(II+1,JJ) + T21
 1120                      CONTINUE
                        END IF
 1140                CONTINUE
                     IF (ILEN.LT.ISPAN) THEN
                        DO 1240 II = I + ILEN, I + ISPAN - 1
                           DO 1180 JJ = J, J + JLEN - 1, JDEPTH
                              T11 = ZERO
                              T12 = ZERO
                              DO 1160 LL = L, L + LSPAN - 1
                                 T11 = T11 + A(LL,II)*CH(LL-L+1,JJ-J+1)
                                 T12 = T12 + A(LL,II)*CH(LL-L+1,JJ-J+2)
 1160                         CONTINUE
                              C(II,JJ) = C(II,JJ) + T11
                              C(II,JJ+1) = C(II,JJ+1) + T12
 1180                      CONTINUE
                           IF (JLEN.LT.JSPAN) THEN
                              DO 1220 JJ = J + JLEN, J + JSPAN - 1
                                 T11 = ZERO
                                 DO 1200 LL = L, L + LSPAN - 1
                                    T11 = T11 + A(LL,II)*CH(LL-L+1,
     *                                    JJ-J+1)
 1200                            CONTINUE
                                 C(II,JJ) = C(II,JJ) + T11
 1220                         CONTINUE
                           END IF
 1240                   CONTINUE
                     END IF
 1260             CONTINUE
 1280          CONTINUE
 1300       CONTINUE
         END IF
      END IF
C
C
C     End of DGEMM .
C
      RETURN
      END

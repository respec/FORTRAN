C****       
        SUBROUTINE DMRRRR(NRA,NCA,A,LDA,NRB,NCB,B,LDB,NRC,NCC,C,LDC)
C===============================================================================
C
C       SIMPLE MATRIX MULTIPLICATION
C
C
C===============================================================================
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        SAVE

        DIMENSION A(LDA,NCA),B(LDB,NCB),C(LDC,NCC)

        IF( 
     1     ( NCA .NE. NRB ) .OR.
     2     ( NRA .NE. NRC ) .OR.
     3     ( NCB .NE. NCC ) ) THEN
           WRITE(99,*) ' DMRRRR:NON-CONFORMING MATRICES'
           STOP
        ENDIF
        
        DO 10 I=1,NRC
        DO 10 J=1,NCC
            C(I,J)  =  0.D0
          DO 10 K=1,NCA
            C(I,J) = C(I,J) + A(I,K)*B(K,J)
   10   CONTINUE
   
        RETURN
        END
C****       
        SUBROUTINE DMXTYF(NRA,NCA,A,LDA,NRB,NCB,B,LDB,NRC,NCC,C,LDC)
C===============================================================================
C
C       MATRIX MULTIPLICATION TRANS(A)*B
C
C
C===============================================================================
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

        DIMENSION A(LDA,NCA),B(LDB,NCB),C(LDC,NCC)

        IF( 
     1     ( NRA .NE. NRB ) .OR.
     2     ( NCA .NE. NRC ) .OR.
     3     ( NCB .NE. NCC ) ) THEN
           WRITE(99,*) ' DMXTYF:NON-CONFORMING MATRICES'
           STOP
        ENDIF
        
        DO 10 I=1,NRC
        DO 10 J=1,NCC
            C(I,J)  =  0.D0
          DO 10 K=1,NRA
            C(I,J) = C(I,J) + A(K,I)*B(K,J)
   10   CONTINUE
   
        RETURN
        END
C****       
        SUBROUTINE DMXYTF(NRA,NCA,A,LDA,NRB,NCB,B,LDB,NRC,NCC,C,LDC)
C===============================================================================
C
C       MATRIX MULTIPLICATION A*TRANS(B)
C
C
C===============================================================================
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

        DIMENSION A(LDA,NCA),B(LDB,NCB),C(LDC,NCC)

        IF( 
     1     ( NRA .NE. NRB ) .OR.
     2     ( NRA .NE. NRC ) .OR.
     3     ( NRB .NE. NCC ) ) THEN
           WRITE(99,*) ' DMXYTF:NON-CONFORMING MATRICES'
           STOP
        ENDIF
        
        DO 10 I=1,NRC
        DO 10 J=1,NCC
            C(I,J)  =  0.D0
          DO 10 K=1,NRA
            C(I,J) = C(I,J) + A(I,K)*B(J,K)
   10   CONTINUE
   
        RETURN
        END
C****       
        SUBROUTINE DMSUM(NRA,NCA,A,LDA,NRB,NCB,B,LDB,NRC,NCC,C,LDC)

C===============================================================================
C
C       MATRIX ADDITION C = A + B
C
C
C===============================================================================
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        SAVE

        DIMENSION A(LDA,NCA),B(LDB,NCB),C(LDC,NCC)

        IF( 
     1     ( NRA .NE. NRB ) .OR.
     2     ( NCA .NE. NCB ) .OR.
     3     ( NRB .NE. NRC ) .OR.
     3     ( NCB .NE. NCC ) ) THEN
           WRITE(99,*) ' DMSUM:NON-CONFORMING MATRICES'
           STOP
        ENDIF
        
        DO 10 I=1,NRC
        DO 10 J=1,NCC
            C(I,J) = A(I,J) + B(I,J)
   10   CONTINUE
   
        RETURN
        END
C****       
        SUBROUTINE DMMULT(VAL,NRA,NCA,A,LDA,NRC,NCC,C,LDC)

C===============================================================================
C
C       MATRIX ADDITION C = VAL * A
C
C
C===============================================================================
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        SAVE

        DIMENSION A(LDA,NCA),C(LDC,NCC)

        IF( 
     1     ( NRA .NE. NRC ) .OR.
     2     ( NCA .NE. NCC ) ) THEN
           WRITE(99,*) ' DMSUM:NON-CONFORMING MATRICES'
           STOP
        ENDIF
        
        DO 10 I=1,NRC
        DO 10 J=1,NCC
            C(I,J) = VAL*A(I,J)
   10   CONTINUE
   
        RETURN
        END
C****       
        SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
C===============================================================================
C
C       LET Y(I)  = DA * X(I) + Y(I)
C 
C
C
C===============================================================================
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        SAVE

        DIMENSION DX(*),DY(*)

        IF(INCX .GE. 0) THEN
           INDX  =  1
        ELSE
           INDX  =  (1-N)*INCX+1
        ENDIF
        
        IF(INCY .GE. 0) THEN
           INDY  =  1
        ELSE
           INDY  =  (1-N)*INCY+1
        ENDIF
        
        DO 10 I=1,N
           DY(INDY)   =  DY(INDY) + DA*DX(INDX)
           INDX       =  INDX + INCX
           INDY       =  INDY + INCY
   10   CONTINUE
   
        RETURN
        END
C****       
      FUNCTION DLNGAM(XX)
C===============================================================================
C
C       LOGARITHM OF THE GAMMA FUNCTION
C
C       N.B.  VALID ONLY FOR XX>0
C             THIS IS DIFFERENT FROM IMSL ROUTINE
C
C===============================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

      DIMENSION COF(6)
      
      DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0,
     *    -1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/

      DATA HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/
      
        X=XX-ONE
        TMP=X+FPF
        TMP=(X+HALF)*LOG(TMP)-TMP
        SER=ONE
      DO 11 J=1,6
        X=X+ONE
        SER=SER+COF(J)/X
11    CONTINUE
        DLNGAM=TMP+LOG(STP*SER)
      RETURN
      END
C****       
        SUBROUTINE DLINRG(N,A,LDA,AINV,LDAINV)
C===============================================================================
C
C       SIMPLE MATRIX INVERSION
C
C
C===============================================================================
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        SAVE

        DIMENSION A(LDA,LDA),AINV(LDAINV,N)

        DO 10 I=1,N
        DO 10 J=1,N
          AINV(I,J)  =  A(I,J)
   10   CONTINUE
   
        CALL GAUSSJ(AINV,N,LDAINV,B,0,1)
        
        RETURN
        END
C****       
      SUBROUTINE GAUSSJ(A,N,NP,B,M,MP)
C
C     PERFORMS GAUSS-JORDAN ELIMINATION
C
C     ON RETURN A CONTAINS INVERSE OF A, AND, IF B CONTAINS RHS 
C     OF EQUATION 
C               AX = B
C     THEN ON RETURN B WILL CONTAIN SOLUTION X
C
C     TIM COHN   8/22/97
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

      PARAMETER (NMAX=50)
      DIMENSION A(NP,NP),B(NP,MP),IPIV(NMAX),INDXR(NMAX),INDXC(NMAX)
      DO 11 J=1,N
        IPIV(J)=0
11    CONTINUE
      DO 22 I=1,N
        BIG=0.
        DO 13 J=1,N
          IF(IPIV(J).NE.1)THEN
            DO 12 K=1,N
              IF (IPIV(K).EQ.0) THEN
                IF (ABS(A(J,K)).GE.BIG)THEN
                  BIG=ABS(A(J,K))
                  IROW=J
                  ICOL=K
                ENDIF
              ELSE IF (IPIV(K).GT.1) THEN
                PAUSE 'SINGULAR MATRIX'
              ENDIF
12          CONTINUE
          ENDIF
13      CONTINUE
        IPIV(ICOL)=IPIV(ICOL)+1
        IF (IROW.NE.ICOL) THEN
          DO 14 L=1,N
            DUM=A(IROW,L)
            A(IROW,L)=A(ICOL,L)
            A(ICOL,L)=DUM
14        CONTINUE
          DO 15 L=1,M
            DUM=B(IROW,L)
            B(IROW,L)=B(ICOL,L)
            B(ICOL,L)=DUM
15        CONTINUE
        ENDIF
        INDXR(I)=IROW
        INDXC(I)=ICOL
        IF (A(ICOL,ICOL).EQ.0.) PAUSE 'SINGULAR MATRIX.'
        PIVINV=1./A(ICOL,ICOL)
        A(ICOL,ICOL)=1.
        DO 16 L=1,N
          A(ICOL,L)=A(ICOL,L)*PIVINV
16      CONTINUE
        DO 17 L=1,M
          B(ICOL,L)=B(ICOL,L)*PIVINV
17      CONTINUE
        DO 21 LL=1,N
          IF(LL.NE.ICOL)THEN
            DUM=A(LL,ICOL)
            A(LL,ICOL)=0.
            DO 18 L=1,N
              A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
18          CONTINUE
            DO 19 L=1,M
              B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
19          CONTINUE
          ENDIF
21      CONTINUE
22    CONTINUE
      DO 24 L=N,1,-1
        IF(INDXR(L).NE.INDXC(L))THEN
          DO 23 K=1,N
            DUM=A(K,INDXR(L))
            A(K,INDXR(L))=A(K,INDXC(L))
            A(K,INDXC(L))=DUM
23        CONTINUE
        ENDIF
24    CONTINUE
      RETURN
      END
C****       
        SUBROUTINE ISET(N,IVALUE,IVAR,ISPACE)
C===============================================================================
C
C       SIMPLE MATRIX INVERSION
C
C===============================================================================
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        SAVE

        INTEGER IVAR(*)

        DO 10 I=1,N,ISPACE
          IVAR(I) = IVALUE
   10   CONTINUE
        
        RETURN
        END
C****       
        SUBROUTINE DSET(N,FVALUE,FVAR,ISPACE)
C===============================================================================
C
C       SIMPLE MATRIX INVERSION
C
C===============================================================================
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        SAVE

        DIMENSION FVAR(*)

        DO 10 I=1,N,ISPACE
          FVAR(I) = FVALUE
   10   CONTINUE
        
        RETURN
        END
C****       
        SUBROUTINE RSET(N,RVALUE,RVAR,ISPACE)
C===============================================================================
C
C       SIMPLE MATRIX INVERSION
C
C===============================================================================
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        SAVE

        REAL RVAR(*),RVALUE

        DO 10 I=1,N,ISPACE
          RVAR(I) = RVALUE
   10   CONTINUE
        
        RETURN
        END
C****       
        DOUBLE PRECISION FUNCTION DNORDF(X)
C===============================================================================
C
C       NORMAL DISTRIBUTION CDF
C
C===============================================================================
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        SAVE


        X2     =  X * 0.7071068
        DNORDF =  1.D0 - ERFCC(X2)/2.D0
        
      RETURN
      END

C****       
        DOUBLE PRECISION FUNCTION ERFCC(X)
C===============================================================================
C
C       ERROR FUNCTION FOUND IN NUMERICAL RECIPES
C
C===============================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

      
      Z=ABS(X)      
      T=1./(1.+0.5*Z)
      
      IF( Z .LT. 15.D0 ) THEN
        ERFCC=T*EXP(-Z*Z-1.26551223+T*(1.00002368+T*(.37409196+
     *    T*(.09678418+T*(-.18628806+T*(.27886807+T*(-1.13520398+
     *    T*(1.48851587+T*(-.82215223+T*.17087277)))))))))
      ELSE
        ERFCC = 0.D0
      ENDIF
      
      IF (X.LT.0.) ERFCC=2.-ERFCC
      
      RETURN
      END
C****       
        DOUBLE PRECISION FUNCTION DNORIN(P)
C===============================================================================
C
C       NORMAL DISTRIBUTION CDF
C
C===============================================================================
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        SAVE

        REAL*4 P2,PPF
        
          P2  =  P
        
          CALL NORPPF(P2,PPF)
        
          DNORIN = PPF
        
        RETURN
        END

C****       
* ======================================================================
* NIST GUIDE TO AVAILABLE MATH SOFTWARE.
* FULLSOURCE FOR MODULE NORPPF FROM PACKAGE DATAPAC.
* RETRIEVED FROM CAMSUN ON FRI AUG 29 09:02:15 1997.
* ======================================================================
* NORPPF
      SUBROUTINE NORPPF(P,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE NORMAL (GAUSSIAN)
C              DISTRIBUTION WITH MEAN = 0 AND STANDARD DEVIATION = 1. 
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = (1/SQRT(2*PI))*EXP(-X*X/2). 
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION 
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE 
C                                (BETWEEN 0.0 AND 1.0)
C                                AT WHICH THE PERCENT POINT 
C                                FUNCTION IS TO BE EVALUATED.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0, EXCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, ALOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--ODEH AND EVANS, THE PERCENTAGE POINTS
C                 OF THE NORMAL DISTRIBUTION, ALGORTIHM 70, 
C                 APPLIED STATISTICS, 1974, PAGES 96-97.
C               --EVANS, ALGORITHMS FOR MINIMAL DEGREE
C                 POLYNOMIAL AND RATIONAL APPROXIMATION,
C                 M. SC. THESIS, 1972, UNIVERSITY 
C                 OF VICTORIA, B. C., CANADA.
C               --HASTINGS, APPROXIMATIONS FOR DIGITAL
C                 COMPUTERS, 1955, PAGES 113, 191, 192.
C               --NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 933, FORMULA 26.2.23.
C               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY), 1969, PAGES 21-44, 229-231.
C               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
C                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 40-111.
C               --THE KELLEY STATISTICAL TABLES, 1948.
C               --OWEN, HANDBOOK OF STATISTICAL TABLES,
C                 1962, PAGES 3-16.
C               --PEARSON AND HARTLEY, BIOMETRIKA TABLES
C                 FOR STATISTICIANS, VOLUME 1, 1954,
C                 PAGES 104-113.
C     COMMENTS--THE CODING AS PRESENTED BELOW
C               IS ESSENTIALLY IDENTICAL TO THAT
C               PRESENTED BY ODEH AND EVANS
C               AS ALGORTIHM 70 OF APPLIED STATISTICS.
C               THE PRESENT AUTHOR HAS MODIFIED THE
C               ORIGINAL ODEH AND EVANS CODE WITH ONLY
C               MINOR STYLISTIC CHANGES.
C             --AS POINTED OUT BY ODEH AND EVANS
C               IN APPLIED STATISTICS,
C               THEIR ALGORITHM REPRESENTES A
C               SUBSTANTIAL IMPROVEMENT OVER THE
C               PREVIOUSLY EMPLOYED
C               HASTINGS APPROXIMATION FOR THE
C               NORMAL PERCENT POINT FUNCTION--
C               THE ACCURACY OF APPROXIMATION
C               BEING IMPROVED FROM 4.5*(10**-4)
C               TO 1.5*(10**-8).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--JUNE      1972. 
C     UPDATED         --SEPTEMBER 1975. 
C     UPDATED         --NOVEMBER  1975. 
C     UPDATED         --OCTOBER   1976. 
C
C---------------------------------------------------------------------
C
      DATA P0,P1,P2,P3,P4
     1/-.322232431088,-1.0,
     1 -.342242088547,-.204231210245E-1,
     1 -.453642210148E-4/
      DATA Q0,Q1,Q2,Q3,Q4
     1/.993484626060E-1,.588581570495,
     1 .531103462366,.103537752850,
     1 .38560700634E-2/
C
      IPR=6
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(P.LE.0.0.OR.P.GE.1.0)GOTO50
      GOTO90
   50 WRITE(IPR,1)
      WRITE(IPR,46)P
      RETURN
   90 CONTINUE
    1 FORMAT(1H ,115H***** FATAL ERROR--THE FIRST  INPUT ARGUMENT TO THE
     1 NORPPF SUBROUTINE IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL *****)
   46 FORMAT(1H , 35H***** THE VALUE OF THE ARGUMENT IS ,E15.8,6H *****)
C
C-----START POINT-----------------------------------------------------
C
      IF(P.NE.0.5)GOTO150
      PPF=0.0
      RETURN
C
  150 R=P 
      IF(P.GT.0.5)R=1.0-R
      T=SQRT(-2.0*ALOG(R))
      ANUM=((((T*P4+P3)*T+P2)*T+P1)*T+P0)
      ADEN=((((T*Q4+Q3)*T+Q2)*T+Q1)*T+Q0)
      PPF=T+(ANUM/ADEN)
      IF(P.LT.0.5)PPF=-PPF
      RETURN
C
      END 
C****       
      DOUBLE PRECISION FUNCTION DGAMDF(X,A)
C===============================================================================
C
C       GAMMA DISTRIBUTION CDF
C
C===============================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
	
	     X2  = MAX(0.D0,X)
	   IF(A .LE. 0.D0) THEN
C	     WRITE(99,*) ' DGAMDF:A = ',A
	     DGAMDF = 1.D0
	     RETURN
	   ENDIF
        IF (A .LE. 2000.) THEN
           ARG1  =  GAMMP(A,X2)
	ENDIF
	
	IF (A .GE. 1500.D0) THEN
C
C  THIS IS A WILSON-HILFERTY SOLUTION
C
	     Z = -(-1. + 9.*A - 9.*A**(2./3.)*X2**(1./3.))/(3.*SQRT(A))
           ARG2 = DNORDF(Z)
	ENDIF
	   W = MAX(0.D0,MIN((A-1500.)/500.,1.D0))
	DGAMDF = (1.D0-W)*ARG1 + W*ARG2
      
      RETURN
      END
      
C****       
      DOUBLE PRECISION FUNCTION DCHIDF(X,D)
C===============================================================================
C
C       CHI-SQUARE DISTRIBUTION CDF
C
C===============================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

            
      DCHIDF  =  DGAMDF(X/2.D0,D/2.D0)
      
      RETURN
      END
      
C****       
      DOUBLE PRECISION FUNCTION DCHIIN(P,NU)
C===============================================================================
C
C       INVERSE CHI-SQUARED DISTRIBUTION CDF
C
C===============================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

      DOUBLE PRECISION P,NU,X
      REAL*4 GAMMA,P2
      
	IF( (P .GT. 0.0) .AND. (P .LT. 1.D0) ) THEN
        	P2    =  P
        	GAMMA =  NU/2.0
		CALL MGAMINV(NU/2.D0,P,X1,IER)
		X     =  2.D0*X1
	ELSE IF (P .LE. 0.D0) THEN
		X = 0.0
	ELSE 
	        X = 1.D99
	ENDIF
      
      DCHIIN  =  X
      
      RETURN
      END
      
C****       
      DOUBLE PRECISION FUNCTION DRNGAM(N,ALPHA,X)
C===============================================================================
C
C       GENERATES RANDOM 1P GAMMA VARIATES
C
C===============================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

      DOUBLE PRECISION X(*)
      INTEGER N,ISEED
      REAL*4 GAMMA,X2
      
      COMMON /ZZZ889/ISEED
      
      GAMMA = ALPHA
      DO 10 I=1,N
        CALL GAMRAN(1,GAMMA,ISEED,X2)
        X(I) = X2
10    CONTINUE
      
      RETURN
      END
C
C****  NORRAN(N,ISEED,X)     

C****       
      DOUBLE PRECISION FUNCTION DRNORMS()
C===============================================================================
C
C       GENERATES STANDARD NORMAL VARIATE
C
C===============================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

      INTEGER ISEED
      REAL*4 X2
      
      COMMON /ZZZ889/ISEED
      
        CALL NORRAN(1,ISEED,X2)
        DRNORMS = X2
      
      RETURN
      END
C
C****       

      SUBROUTINE RNSET(ISEED2)
C===============================================================================
C
C       INITIALIZED RANDOM NUMBER GENERATOR
C
C===============================================================================
      INTEGER ISEED, ISEED2
      COMMON /ZZZ889/ISEED
      
      ISEED = ISEED2
      RETURN
      END
      
      
C****       
* ======================================================================
* NIST GUIDE TO AVAILABLE MATH SOFTWARE.
* FULLSOURCE FOR MODULE GAMCDF FROM PACKAGE DATAPAC.
* RETRIEVED FROM CAMSUN ON TUE AUG 26 16:39:05 1997.
* ======================================================================
* GAMCDF
      SUBROUTINE GAMCDF(X,GAMMA,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE GAMMA
C              DISTRIBUTION WITH SINGLE PRECISION 
C              TAIL LENGTH PARAMETER = GAMMA.
C              THE GAMMA DISTRIBUTION USED
C              HEREIN HAS MEAN = GAMMA
C              AND STANDARD DEVIATION = SQRT(GAMMA).
C              THIS DISTRIBUTION IS DEFINED FOR ALL POSITIVE X,
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = (1/CONSTANT) * (X**(GAMMA-1)) * EXP(-X)
C              WHERE THE CONSTANT = THE GAMMA FUNCTION EVALUATED
C              AT THE VALUE GAMMA.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE 
C                                AT WHICH THE CUMULATIVE DISTRIBUTION 
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE POSITIVE.
C                     --GAMMA  = THE SINGLE PRECISION VALUE 
C                                OF THE TAIL LENGTH PARAMETER.
C                                GAMMA SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF FOR THE GAMMA DISTRIBUTION 
C             WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
C                 --X SHOULD BE POSITIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DEXP, DLOG.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     ACCURACY--(ON THE UNIVAC 1108, EXEC 8 SYSTEM AT NBS)
C               COMPARED TO THE KNOWN GAMMA = 1 (EXPONENTIAL)
C               RESULTS, AGREEMENT WAS HAD OUT TO 7 SIGNIFICANT
C               DIGITS FOR ALL TESTED X.
C               THE TESTED X VALUES COVERED THE ENTIRE
C               RANGE OF THE DISTRIBUTION--FROM THE 0.00001 
C               PERCENT POINT UP TO THE 99.99999 PERCENT POINT
C               OF THE DISTRIBUTION.
C     REFERENCES--WILK, GNANADESIKAN, AND HUYETT, 'PROBABILITY
C                 PLOTS FOR THE GAMMA DISTRIBUTION',
C                 TECHNOMETRICS, 1962, PAGES 1-15,
C                 ESPECIALLY PAGES 3-5. 
C               --NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 257, FORMULA 6.1.41.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 166-206.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGES 68-73.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--NOVEMBER  1975. 
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX,DGAMMA,AI,TERM,SUM,CUT1,CUT2,CUTOFF,T
      DOUBLE PRECISION Z,Z2,Z3,Z4,Z5,DEN,A,B,C,D,G
      DOUBLE PRECISION DEXP,DLOG
      DIMENSION D(10)
      DATA C/ .918938533204672741D0/
      DATA D(1),D(2),D(3),D(4),D(5)
     1                 /+.833333333333333333D-1,-.277777777777777778D-2,
     1+.793650793650793651D-3,-.595238095238095238D-3,+.8417508417508417
     151D-3/
      DATA D(6),D(7),D(8),D(9),D(10)
     1     /-.191752691752691753D-2,+.641025641025641025D-2,-.2955065359
     147712418D-1,+.179644372368830573D0,-.139243221690590111D1/
C
C
      IPR=6
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(X.LE.0.0)GOTO50
      IF(GAMMA.LE.0.0)GOTO55
      GOTO90
   50 WRITE(IPR,4)
      WRITE(IPR,46)X
      CDF=0.0
      RETURN
   55 WRITE(IPR,15) 
      WRITE(IPR,46)GAMMA
      CDF=0.0
      RETURN
   90 CONTINUE
    4 FORMAT(1H ,100H***** NON-FATAL DIAGNOSTIC--THE FIRST  INPUT ARGUME
     1NT TO THE GAMCDF SUBROUTINE IS NON-POSITIVE *****)
   15 FORMAT(1H , 91H***** FATAL ERROR--THE SECOND INPUT ARGUMENT TO THE
     1 GAMCDF SUBROUTINE IS NON-POSITIVE *****)
   46 FORMAT(1H , 35H***** THE VALUE OF THE ARGUMENT IS ,E15.8,6H *****)
C
C-----START POINT-----------------------------------------------------
C
      DX=X
      DGAMMA=GAMMA
      MAXIT=10000
C
C     COMPUTE THE GAMMA FUNCTION USING THE ALGORITHM IN THE 
C     NBS APPLIED MATHEMATICS SERIES REFERENCE.
C
      Z=DGAMMA
      DEN=1.0D0
  300 IF(Z.GE.10.0D0)GOTO400
      DEN=DEN*Z
      Z=Z+1
      GOTO300
  400 Z2=Z*Z
      Z3=Z*Z2
      Z4=Z2*Z2
      Z5=Z2*Z3
      A=(Z-0.5D0)*DLOG(Z)-Z+C 
      B=D(1)/Z+D(2)/Z3+D(3)/Z5+D(4)/(Z2*Z5)+D(5)/(Z4*Z5)+
     1D(6)/(Z*Z5*Z5)+D(7)/(Z3*Z5*Z5)+D(8)/(Z5*Z5*Z5)+D(9)/(Z2*Z5*Z5*Z5)
      G=DEXP(A+B)/DEN
C
C     COMPUTE T-SUB-Q AS DEFINED ON PAGE 4 OF THE WILK, GNANADESIKAN, 
C     AND HUYETT REFERENCE
C
      SUM=1.0D0/DGAMMA
      TERM=1.0D0/DGAMMA
      CUT1=DX-DGAMMA
      CUT2=DX*10000000000.0D0 
      DO200I=1,MAXIT
      AI=I
      TERM=DX*TERM/(DGAMMA+AI)
      SUM=SUM+TERM
      CUTOFF=CUT1+(CUT2*TERM/SUM)
      IF(AI.GT.CUTOFF)GOTO250 
  200 CONTINUE
      WRITE(IPR,205)MAXIT
      WRITE(IPR,206)X
      WRITE(IPR,207)GAMMA
      WRITE(IPR,208)
      CDF=1.0
      RETURN
C
  250 T=SUM
      CDF=(DX**DGAMMA)*(DEXP(-DX))*T/G
C
  205 FORMAT(1H ,48H*****ERROR IN INTERNAL OPERATIONS IN THE GAMCDF , 
     1 45HSUBROUTINE--THE NUMBER OF ITERATIONS EXCEEDS ,I7) 
  206 FORMAT(1H ,33H     THE INPUT VALUE OF X     IS ,E15.8)
  207 FORMAT(1H ,33H     THE INPUT VALUE OF GAMMA IS ,E15.8)
  208 FORMAT(1H ,48H     THE OUTPUT VALUE OF CDF HAS BEEN SET TO 1.0) 
C
      RETURN
      END 
C****       
      SUBROUTINE DSVRGN(N,RB,RA)
C===============================================================================
C
C       HEAP SORT
C
C===============================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

      DIMENSION RA(N),RB(N)
      
      DO 5 I=1,N
        RA(I)  =  RB(I)
 5    CONTINUE
 
        L=N/2+1
        IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
        ELSE
          RRA=RA(IR)
          RA(IR)=RA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
      GO TO 10
      END
C****       
      INTEGER FUNCTION NDAYS(IDAY, IMONTH, IYEAR)
C===============================================================================
C
C       COMPUTE THE NUMBER OF DAYS SINCE 1 JAN 1900 (=0)
C
C===============================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

      INTEGER IDAY, IMONTH, IYEAR
      
C     NDAYS0(1,1,1900) = 693961
      
      NDAYS = NDAYS0(IDAY,IMONTH,IYEAR) - 693961
      
      RETURN
      END
C****       
      INTEGER FUNCTION NDAYS0(IDAY, IMONTH, IYEAR)
C===============================================================================
C
C       COMPUTE THE NUMBER OF DAYS SINCE 1 JAN 0000 (=0)
C
C===============================================================================
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

      INTEGER IDAY, IMONTH, IYEAR, DM(12)
      
      DATA DM/0,31,59,90,120,151,181,212,243,273,304,334/
      
C
C     LEAP YEAR CORRECTIONS
C       1.  EVERY 4 YEARS
C       2.  EXCEPT EVERY 100 YEARS
C       3.  EXCEPT EVERY 400 YEARS
C           (I.E. LY'S IN 1896, 1904 1908, ..., 1996, 2000, 
C

      IF( IYEAR .LT. 1583) THEN
         WRITE(99,*) ' NDAYS FAILS FOR YEARS PRIOR TO 1583'
         STOP
      ENDIF
      
        NDAYS0 = 
     1    365*IYEAR + DM(IMONTH) + (IDAY - 1) +
     2    IYEAR/4 - IYEAR/100 + IYEAR/400 + 1
     
       IF(MOD(IYEAR,4) .EQ. 0 .AND. IMONTH .LT. 3) THEN
         IF(MOD(IYEAR,100) .NE. 0 .OR. MOD(IYEAR,400) .EQ. 0) THEN
           NDAYS0  = NDAYS0-1
         ENDIF
       ENDIF

      RETURN
      END
C****      
C===============================================================================
C
C       DUMMY GRAPHICS ROUTINES, OUTPUT, ETC.  
C
C===============================================================================
      SUBROUTINE UMACH
        RETURN
        END
      SUBROUTINE DBOXP
        RETURN
        END
      SUBROUTINE PAGE
        RETURN
        END
      SUBROUTINE DPLOTP
        RETURN
        END
C
C===============================================================================

* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module GAMPPF from package DATAPAC.
* Retrieved from CAMSUN on Fri Jul 10 07:59:09 1998.
* ======================================================================
* GAMPPF
      SUBROUTINE GAMPPF(P,GAMMA,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE GAMMA DISTRIBUTION
C              WITH SINGLE PRECISION
C              TAIL LENGTH PARAMETER = GAMMA.
C              THE GAMMA DISTRIBUTION USED
C              HEREIN HAS MEAN = GAMMA
C              AND STANDARD DEVIATION = SQRT(GAMMA).
C              THIS DISTRIBUTION IS DEFINED FOR ALL POSITIVE X,
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = (1/CONSTANT) * (X**(GAMMA-1)) * EXP(-X)
C              WHERE THE CONSTANT = THE GAMMA FUNCTION EVALUATED
C              AT THE VALUE GAMMA.
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 (EXCLUSIVELY)
C                                AND 1.0 (EXCLUSIVELY))
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --GAMMA  = THE SINGLE PRECISION VALUE OF THE
C                                TAIL LENGTH PARAMETER.
C                                GAMMA SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION .
C             VALUE PPF FOR THE GAMMA DISTRIBUTION
C             WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
C                 --P SHOULD BE BETWEEN 0.0 (EXCLUSIVELY)
C                   AND 1.0 (EXCLUSIVELY).
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DEXP, DLOG.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN.
C     ACCURACY--(ON THE UNIVAC 1108, EXEC 8 SYSTEM AT NBS)
C               COMPARED TO THE KNOWN GAMMA = 1 (EXPONENTIAL)
C               RESULTS, AGREEMENT WAS HAD OUT TO 6 SIGNIFICANT
C               DIGITS FOR ALL TESTED P IN THE RANGE P = .001 TO
C               P = .999.  FOR P = .95 AND SMALLER, THE AGREEMENT
C               WAS EVEN BETTER--7 SIGNIFICANT DIGITS.
C               (NOTE THAT THE TABULATED VALUES GIVEN IN THE WILK,
C               GNANADESIKAN, AND HUYETT REFERENCE BELOW, PAGE 20,
C               ARE IN ERROR FOR AT LEAST THE GAMMA = 1 CASE--
C               THE WORST DETECTED ERROR WAS AGREEMENT TO ONLY 3
C               SIGNIFICANT DIGITS (IN THEIR 8 SIGNIFICANT DIGIT TABLE)
C               FOR P = .999.)
C     REFERENCES--WILK, GNANADESIKAN, AND HUYETT, 'PROBABILITY
C                 PLOTS FOR THE GAMMA DISTRIBUTION',
C                 TECHNOMETRICS, 1962, PAGES 1-15,
C                 ESPECIALLY PAGES 3-5.
C               --NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 257, FORMULA 6.1.41.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 166-206.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGES 68-73.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--NOVEMBER  1974.
C     UPDATED         --SEPTEMBER 1975.
C     UPDATED         --NOVEMBER  1975.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DP,DGAMMA
      DOUBLE PRECISION Z,Z2,Z3,Z4,Z5,DEN,A,B,C,D,G
      DOUBLE PRECISION XMIN0,XMIN,AI,XMAX,DX,PCALC,XMID
      DOUBLE PRECISION XLOWER,XUPPER,XDEL
      DOUBLE PRECISION SUM,TERM,CUT1,CUT2,AJ,CUTOFF,T
      DOUBLE PRECISION DEXP,DLOG
      DIMENSION D(10)
      DATA C/ .918938533204672741D0/
      DATA D(1),D(2),D(3),D(4),D(5)
     1                 /+.833333333333333333D-1,-.277777777777777778D-2,
     1+.793650793650793651D-3,-.595238095238095238D-3,+.8417508417508417
     151D-3/
      DATA D(6),D(7),D(8),D(9),D(10)
     1     /-.191752691752691753D-2,+.641025641025641025D-2,-.2955065359
     147712418D-1,+.179644372368830573D0,-.139243221690590111D1/
C
      IPR=6
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(P.LE.0.0.OR.P.GE.1.0)GOTO50
      IF(GAMMA.LE.0.0)GOTO55
      GOTO90
   50 WRITE(IPR,1)
      WRITE(IPR,46)P
      PPF=0.0
      RETURN
   55 WRITE(IPR,15)
      WRITE(IPR,46)GAMMA
      PPF=0.0
      RETURN
   90 CONTINUE
    1 FORMAT(1H ,115H***** FATAL ERROR--THE FIRST  INPUT ARGUMENT TO THE
     1 GAMPPF SUBROUTINE IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL *****)
   15 FORMAT(1H , 91H***** FATAL ERROR--THE SECOND INPUT ARGUMENT TO THE
     1 GAMPPF SUBROUTINE IS NON-POSITIVE *****)
   46 FORMAT(1H , 35H***** THE VALUE OF THE ARGUMENT IS ,E15.8,6H *****)
C
C-----START POINT-----------------------------------------------------
C
      DP=P
      DGAMMA=GAMMA
      MAXIT=10000
C
C     COMPUTE THE GAMMA FUNCTION USING THE ALGORITHM IN THE
C     NBS APPLIED MATHEMATICS SERIES REFERENCE.
C     THIS GAMMA FUNCTION NEED BE CALCULATED ONLY ONCE.
C     IT IS USED IN THE CALCULATION OF THE CDF BASED ON
C     THE TENTATIVE VALUE OF THE PPF IN THE ITERATION.
C
      Z=DGAMMA
      DEN=1.0D0
  150 IF(Z.GE.10.0D0)GOTO160
      DEN=DEN*Z
      Z=Z+1.0D0
      GOTO150
  160 Z2=Z*Z
      Z3=Z*Z2
      Z4=Z2*Z2
      Z5=Z2*Z3
      A=(Z-0.5D0)*DLOG(Z)-Z+C
      B=D(1)/Z+D(2)/Z3+D(3)/Z5+D(4)/(Z2*Z5)+D(5)/(Z4*Z5)+
     1D(6)/(Z*Z5*Z5)+D(7)/(Z3*Z5*Z5)+D(8)/(Z5*Z5*Z5)+D(9)/(Z2*Z5*Z5*Z5)
      G=DEXP(A+B)/DEN
C
C     DETERMINE LOWER AND UPPER LIMITS ON THE DESIRED 100P
C     PERCENT POINT.
C
      ILOOP=1
      XMIN0=(DP*DGAMMA*G)**(1.0D0/DGAMMA)
      XMIN=XMIN0
      ICOUNT=1
  350 AI=ICOUNT
      XMAX=AI*XMIN0
      DX=XMAX
      GOTO1000
  360 IF(PCALC.GE.DP)GOTO370
      XMIN=XMAX
      ICOUNT=ICOUNT+1
      IF(ICOUNT.LE.30000)GOTO350
  370 XMID=(XMIN+XMAX)/2.0D0
C
C     NOW ITERATE BY BISECTION UNTIL THE DESIRED ACCURACY IS ACHIEVED.
C
      ILOOP=2
      XLOWER=XMIN
      XUPPER=XMAX
      ICOUNT=0
  550 DX=XMID
      GOTO1000
  560 IF(PCALC.EQ.DP)GOTO570
      IF(PCALC.GT.DP)GOTO580
      XLOWER=XMID
      XMID=(XMID+XUPPER)/2.0D0
      GOTO590
  580 XUPPER=XMID
      XMID=(XMID+XLOWER)/2.0D0
  590 XDEL=XMID-XLOWER
      IF(XDEL.LT.0.0D0)XDEL=-XDEL
      ICOUNT=ICOUNT+1
      IF(XDEL.LT.0.0000000001D0.OR.ICOUNT.GT.100)GOTO570
      GOTO550
  570 PPF=XMID
      RETURN
C
C********************************************************************
C     THIS SECTION BELOW IS LOGICALLY SEPARATE FROM THE ABOVE.
C     THIS SECTION COMPUTES A CDF VALUE FOR ANY GIVEN TENTATIVE
C     PERCENT POINT X VALUE AS DEFINED IN EITHER OF THE 2
C     ITERATION LOOPS IN THE ABOVE CODE.
C
C     COMPUTE T-SUB-Q AS DEFINED ON PAGE 4 OF THE WILK, GNANADESIKAN,
C     AND HUYETT REFERENCE
C
 1000 SUM=1.0D0/DGAMMA
      TERM=1.0D0/DGAMMA
      CUT1=DX-DGAMMA
      CUT2=DX*10000000000.0D0
      DO700J=1,MAXIT
      AJ=J
      TERM=DX*TERM/(DGAMMA+AJ)
      SUM=SUM+TERM
      CUTOFF=CUT1+(CUT2*TERM/SUM)
      IF(AJ.GT.CUTOFF)GOTO750
  700 CONTINUE
      WRITE(IPR,705)MAXIT
      WRITE(IPR,706)P
      WRITE(IPR,707)GAMMA
      WRITE(IPR,708)
      PPF=0.0
      RETURN
C
  750 T=SUM
      PCALC=(DX**DGAMMA)*(DEXP(-DX))*T/G
      IF(ILOOP.EQ.1)GOTO360
      GOTO560
C
  705 FORMAT(1H ,48H*****ERROR IN INTERNAL OPERATIONS IN THE GAMPPF ,
     1 45HSUBROUTINE--THE NUMBER OF ITERATIONS EXCEEDS ,I7)
  706 FORMAT(1H ,33H     THE INPUT VALUE OF P     IS ,E15.8)
  707 FORMAT(1H ,33H     THE INPUT VALUE OF GAMMA IS ,E15.8)
  708 FORMAT(1H ,48H     THE OUTPUT VALUE OF PPF HAS BEEN SET TO 0.0)
C
      END
* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module DGAMMA from package CMLIB.
* Retrieved from CAMSUN on Thu Jul  9 21:57:50 1998.
* ======================================================================
      DOUBLE PRECISION FUNCTION DGAMMA(X)
C***BEGIN PROLOGUE  DGAMMA
C***DATE WRITTEN   770601   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C7A
C***KEYWORDS  COMPLETE GAMMA FUNCTION,DOUBLE PRECISION,GAMMA FUNCTION,
C             SPECIAL FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Computes the d.p. complete Gamma function.
C***DESCRIPTION
C
C DGAMMA(X) calculates the double precision complete gamma function
C for double precision argument X.
C
C Series for GAM        on the interval  0.          to  1.00000E+00
C                                        with weighted error   5.79E-32
C                                         log weighted error  31.24
C                               significant figures required  30.00
C                                    decimal places required  32.05
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH,D9LGMC,DCSEVL,DGAMLM,DINT,INITDS,XERROR
C***END PROLOGUE  DGAMMA
      DOUBLE PRECISION X, GAMCS(42), DXREL, PI, SINPIY, SQ2PIL, XMAX,
     1  XMIN, Y,  DINT, D9LGMC, DCSEVL, D1MACH
C
      DATA GAM CS(  1) / +.8571195590 9893314219 2006239994 2 D-2      /
      DATA GAM CS(  2) / +.4415381324 8410067571 9131577165 2 D-2      /
      DATA GAM CS(  3) / +.5685043681 5993633786 3266458878 9 D-1      /
      DATA GAM CS(  4) / -.4219835396 4185605010 1250018662 4 D-2      /
      DATA GAM CS(  5) / +.1326808181 2124602205 8400679635 2 D-2      /
      DATA GAM CS(  6) / -.1893024529 7988804325 2394702388 6 D-3      /
      DATA GAM CS(  7) / +.3606925327 4412452565 7808221722 5 D-4      /
      DATA GAM CS(  8) / -.6056761904 4608642184 8554829036 5 D-5      /
      DATA GAM CS(  9) / +.1055829546 3022833447 3182350909 3 D-5      /
      DATA GAM CS( 10) / -.1811967365 5423840482 9185589116 6 D-6      /
      DATA GAM CS( 11) / +.3117724964 7153222777 9025459316 9 D-7      /
      DATA GAM CS( 12) / -.5354219639 0196871408 7408102434 7 D-8      /
      DATA GAM CS( 13) / +.9193275519 8595889468 8778682594 0 D-9      /
      DATA GAM CS( 14) / -.1577941280 2883397617 6742327395 3 D-9      /
      DATA GAM CS( 15) / +.2707980622 9349545432 6654043308 9 D-10     /
      DATA GAM CS( 16) / -.4646818653 8257301440 8166105893 3 D-11     /
      DATA GAM CS( 17) / +.7973350192 0074196564 6076717535 9 D-12     /
      DATA GAM CS( 18) / -.1368078209 8309160257 9949917230 9 D-12     /
      DATA GAM CS( 19) / +.2347319486 5638006572 3347177168 8 D-13     /
      DATA GAM CS( 20) / -.4027432614 9490669327 6657053469 9 D-14     /
      DATA GAM CS( 21) / +.6910051747 3721009121 3833697525 7 D-15     /
      DATA GAM CS( 22) / -.1185584500 2219929070 5238712619 2 D-15     /
      DATA GAM CS( 23) / +.2034148542 4963739552 0102605193 2 D-16     /
      DATA GAM CS( 24) / -.3490054341 7174058492 7401294910 8 D-17     /
      DATA GAM CS( 25) / +.5987993856 4853055671 3505106602 6 D-18     /
      DATA GAM CS( 26) / -.1027378057 8722280744 9006977843 1 D-18     /
      DATA GAM CS( 27) / +.1762702816 0605298249 4275966074 8 D-19     /
      DATA GAM CS( 28) / -.3024320653 7353062609 5877211204 2 D-20     /
      DATA GAM CS( 29) / +.5188914660 2183978397 1783355050 6 D-21     /
      DATA GAM CS( 30) / -.8902770842 4565766924 4925160106 6 D-22     /
      DATA GAM CS( 31) / +.1527474068 4933426022 7459689130 6 D-22     /
      DATA GAM CS( 32) / -.2620731256 1873629002 5732833279 9 D-23     /
      DATA GAM CS( 33) / +.4496464047 8305386703 3104657066 6 D-24     /
      DATA GAM CS( 34) / -.7714712731 3368779117 0390152533 3 D-25     /
      DATA GAM CS( 35) / +.1323635453 1260440364 8657271466 6 D-25     /
      DATA GAM CS( 36) / -.2270999412 9429288167 0231381333 3 D-26     /
      DATA GAM CS( 37) / +.3896418998 0039914493 2081663999 9 D-27     /
      DATA GAM CS( 38) / -.6685198115 1259533277 9212799999 9 D-28     /
      DATA GAM CS( 39) / +.1146998663 1400243843 4761386666 6 D-28     /
      DATA GAM CS( 40) / -.1967938586 3451346772 9510399999 9 D-29     /
      DATA GAM CS( 41) / +.3376448816 5853380903 3489066666 6 D-30     /
      DATA GAM CS( 42) / -.5793070335 7821357846 2549333333 3 D-31     /
      DATA PI / 3.1415926535 8979323846 2643383279 50 D0 /
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
      DATA NGAM, XMIN, XMAX, DXREL / 0, 3*0.D0 /
C***FIRST EXECUTABLE STATEMENT  DGAMMA
      IF (NGAM.NE.0) GO TO 10
      NGAM = INITDS (GAMCS, 42, 0.1*SNGL(D1MACH(3)) )
C
      CALL DGAMLM (XMIN, XMAX)
      DXREL = DSQRT (D1MACH(4))
C
 10   Y = DABS(X)
      IF (Y.GT.10.D0) GO TO 50
C
C COMPUTE GAMMA(X) FOR -XBND .LE. X .LE. XBND.  REDUCE INTERVAL AND FIND
C GAMMA(1+Y) FOR 0.0 .LE. Y .LT. 1.0 FIRST OF ALL.
C
      N = X
      IF (X.LT.0.D0) N = N - 1
      Y = X - DBLE(FLOAT(N))
      N = N - 1
      DGAMMA = 0.9375D0 + DCSEVL (2.D0*Y-1.D0, GAMCS, NGAM)
      IF (N.EQ.0) RETURN
C
      IF (N.GT.0) GO TO 30
C
C COMPUTE GAMMA(X) FOR X .LT. 1.0
C
      N = -N
      IF (X.EQ.0.D0) CALL XERROR ( 'DGAMMA  X IS 0', 14, 4, 2)
      IF (X.LT.0.0 .AND. X+DBLE(FLOAT(N-2)).EQ.0.D0) CALL XERROR ( 'DGAM
     1MA  X IS A NEGATIVE INTEGER', 31, 4, 2)
      IF (X.LT.(-0.5D0) .AND. DABS((X-DINT(X-0.5D0))/X).LT.DXREL) CALL
     1  XERROR ( 'DGAMMA  ANSWER LT HALF PRECISION BECAUSE X TOO NEAR NE
     2GATIVE INTEGER', 68, 1, 1)
C
      DO 20 I=1,N
        DGAMMA = DGAMMA/(X+DBLE(FLOAT(I-1)) )
 20   CONTINUE
      RETURN
C
C GAMMA(X) FOR X .GE. 2.0 AND X .LE. 10.0
C
 30   DO 40 I=1,N
        DGAMMA = (Y+DBLE(FLOAT(I))) * DGAMMA
 40   CONTINUE
      RETURN
C
C GAMMA(X) FOR DABS(X) .GT. 10.0.  RECALL Y = DABS(X).
C
 50   IF (X.GT.XMAX) CALL XERROR ( 'DGAMMA  X SO BIG GAMMA OVERFLOWS',
     1  32, 3, 2)
C
      DGAMMA = 0.D0
      IF (X.LT.XMIN) CALL XERROR ( 'DGAMMA  X SO SMALL GAMMA UNDERFLOWS'
     1  , 35, 2, 1)
      IF (X.LT.XMIN) RETURN
C
      DGAMMA = DEXP ((Y-0.5D0)*DLOG(Y) - Y + SQ2PIL + D9LGMC(Y) )
      IF (X.GT.0.D0) RETURN
C
      IF (DABS((X-DINT(X-0.5D0))/X).LT.DXREL) CALL XERROR ( 'DGAMMA  ANS
     1WER LT HALF PRECISION, X TOO NEAR NEGATIVE INTEGER'  , 61, 1, 1)
C
      SINPIY = DSIN (PI*Y)
      IF (SINPIY.EQ.0.D0) CALL XERROR ( 'DGAMMA  X IS A NEGATIVE INTEGER
     1', 31, 4, 2)
C
      DGAMMA = -PI/(Y*SINPIY*DGAMMA)
C
      RETURN
      END
      FUNCTION INITDS(DOS,NOS,ETA)
C***BEGIN PROLOGUE  INITDS
C***DATE WRITTEN   770601   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C3A2
C***KEYWORDS  CHEBYSHEV,DOUBLE PRECISION,INITIALIZE,
C             ORTHOGONAL POLYNOMIAL,SERIES,SPECIAL FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Initializes the d.p. properly normalized orthogonal
C            polynomial series to determine the number of terms needed
C            for specific accuracy.
C***DESCRIPTION
C
C Initialize the double precision orthogonal series DOS so that INITDS
C is the number of terms needed to insure the error is no larger than
C ETA.  Ordinarily ETA will be chosen to be one-tenth machine precision
C
C             Input Arguments --
C DOS    dble prec array of NOS coefficients in an orthogonal series.
C NOS    number of coefficients in DOS.
C ETA    requested accuracy of series.
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  INITDS
C
      DOUBLE PRECISION DOS(NOS)
C***FIRST EXECUTABLE STATEMENT  INITDS
      IF (NOS.LT.1) CALL XERROR ( 'INITDS  NUMBER OF COEFFICIENTS LT 1',
     1 35, 2, 2)
C
      ERR = 0.
      DO 10 II=1,NOS
        I = NOS + 1 - II
        ERR = ERR + ABS(SNGL(DOS(I)))
        IF (ERR.GT.ETA) GO TO 20
 10   CONTINUE
C
 20   IF (I.EQ.NOS) CALL XERROR ( 'INITDS  ETA MAY BE TOO SMALL', 28,
     1  1, 2)
      INITDS = I
C
      RETURN
      END
      SUBROUTINE XERROR(MESSG,NMESSG,NERR,LEVEL)
C***BEGIN PROLOGUE  XERROR
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  R3C
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Processes an error (diagnostic) message.
C***DESCRIPTION
C     Abstract
C        XERROR processes a diagnostic message, in a manner
C        determined by the value of LEVEL and the current value
C        of the library error control flag, KONTRL.
C        (See subroutine XSETF for details.)
C
C     Description of Parameters
C      --Input--
C        MESSG - the Hollerith message to be processed, containing
C                no more than 72 characters.
C        NMESSG- the actual number of characters in MESSG.
C        NERR  - the error number associated with this message.
C                NERR must not be zero.
C        LEVEL - error category.
C                =2 means this is an unconditionally fatal error.
C                =1 means this is a recoverable error.  (I.e., it is
C                   non-fatal if XSETF has been appropriately called.)
C                =0 means this is a warning message only.
C                =-1 means this is a warning message which is to be
C                   printed at most once, regardless of how many
C                   times this call is executed.
C
C     Examples
C        CALL XERROR('SMOOTH -- NUM WAS ZERO.',23,1,2)
C        CALL XERROR('INTEG  -- LESS THAN FULL ACCURACY ACHIEVED.',
C                    43,2,1)
C        CALL XERROR('ROOTER -- ACTUAL ZERO OF F FOUND BEFORE INTERVAL F
C    1ULLY COLLAPSED.',65,3,0)
C        CALL XERROR('EXP    -- UNDERFLOWS BEING SET TO ZERO.',39,1,-1)
C
C     Latest revision ---  19 MAR 1980
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  XERRWV
C***END PROLOGUE  XERROR
      CHARACTER*(*) MESSG
C***FIRST EXECUTABLE STATEMENT  XERROR
      CALL XERRWV(MESSG,NMESSG,NERR,LEVEL,0,0,0,0,0.,0.)
      RETURN
      END
      SUBROUTINE XERRWV(MESSG,NMESSG,NERR,LEVEL,NI,I1,I2,NR,R1,R2)
C***BEGIN PROLOGUE  XERRWV
C***DATE WRITTEN   800319   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  R3C
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Processes error message allowing 2 integer and two real
C            values to be included in the message.
C***DESCRIPTION
C     Abstract
C        XERRWV processes a diagnostic message, in a manner
C        determined by the value of LEVEL and the current value
C        of the library error control flag, KONTRL.
C        (See subroutine XSETF for details.)
C        In addition, up to two integer values and two real
C        values may be printed along with the message.
C
C     Description of Parameters
C      --Input--
C        MESSG - the Hollerith message to be processed.
C        NMESSG- the actual number of characters in MESSG.
C        NERR  - the error number associated with this message.
C                NERR must not be zero.
C        LEVEL - error category.
C                =2 means this is an unconditionally fatal error.
C                =1 means this is a recoverable error.  (I.e., it is
C                   non-fatal if XSETF has been appropriately called.)
C                =0 means this is a warning message only.
C                =-1 means this is a warning message which is to be
C                   printed at most once, regardless of how many
C                   times this call is executed.
C        NI    - number of integer values to be printed. (0 to 2)
C        I1    - first integer value.
C        I2    - second integer value.
C        NR    - number of real values to be printed. (0 to 2)
C        R1    - first real value.
C        R2    - second real value.
C
C     Examples
C        CALL XERRWV('SMOOTH -- NUM (=I1) WAS ZERO.',29,1,2,
C    1   1,NUM,0,0,0.,0.)
C        CALL XERRWV('QUADXY -- REQUESTED ERROR (R1) LESS THAN MINIMUM (
C    1R2).,54,77,1,0,0,0,2,ERRREQ,ERRMIN)
C
C     Latest revision ---  19 MAR 1980
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  FDUMP,I1MACH,J4SAVE,XERABT,XERCTL,XERPRT,XERSAV,
C                    XGETUA
C***END PROLOGUE  XERRWV
      CHARACTER*(*) MESSG
      CHARACTER*20 LFIRST
      CHARACTER*37 FORM
      DIMENSION LUN(5)
C     GET FLAGS
C***FIRST EXECUTABLE STATEMENT  XERRWV
      LKNTRL = J4SAVE(2,0,.FALSE.)
      MAXMES = J4SAVE(4,0,.FALSE.)
C     CHECK FOR VALID INPUT
      IF ((NMESSG.GT.0).AND.(NERR.NE.0).AND.
     1    (LEVEL.GE.(-1)).AND.(LEVEL.LE.2)) GO TO 10
         IF (LKNTRL.GT.0) CALL XERPRT('FATAL ERROR IN...',17)
         CALL XERPRT('XERROR -- INVALID INPUT',23)
         IF (LKNTRL.GT.0) CALL FDUMP
         IF (LKNTRL.GT.0) CALL XERPRT('JOB ABORT DUE TO FATAL ERROR.',
     1  29)
         IF (LKNTRL.GT.0) CALL XERSAV(' ',0,0,0,KDUMMY)
         CALL XERABT('XERROR -- INVALID INPUT',23)
         RETURN
   10 CONTINUE
C     RECORD MESSAGE
      JUNK = J4SAVE(1,NERR,.TRUE.)
      CALL XERSAV(MESSG,NMESSG,NERR,LEVEL,KOUNT)
C     LET USER OVERRIDE
      LFIRST = MESSG
      LMESSG = NMESSG
      LERR = NERR
      LLEVEL = LEVEL
      CALL XERCTL(LFIRST,LMESSG,LERR,LLEVEL,LKNTRL)
C     RESET TO ORIGINAL VALUES
      LMESSG = NMESSG
      LERR = NERR
      LLEVEL = LEVEL
      LKNTRL = MAX0(-2,MIN0(2,LKNTRL))
      MKNTRL = IABS(LKNTRL)
C     DECIDE WHETHER TO PRINT MESSAGE
      IF ((LLEVEL.LT.2).AND.(LKNTRL.EQ.0)) GO TO 100
      IF (((LLEVEL.EQ.(-1)).AND.(KOUNT.GT.MIN0(1,MAXMES)))
     1.OR.((LLEVEL.EQ.0)   .AND.(KOUNT.GT.MAXMES))
     2.OR.((LLEVEL.EQ.1)   .AND.(KOUNT.GT.MAXMES).AND.(MKNTRL.EQ.1))
     3.OR.((LLEVEL.EQ.2)   .AND.(KOUNT.GT.MAX0(1,MAXMES)))) GO TO 100
         IF (LKNTRL.LE.0) GO TO 20
            CALL XERPRT(' ',1)
C           INTRODUCTION
            IF (LLEVEL.EQ.(-1)) CALL XERPRT
     1('WARNING MESSAGE...THIS MESSAGE WILL ONLY BE PRINTED ONCE.',57)
            IF (LLEVEL.EQ.0) CALL XERPRT('WARNING IN...',13)
            IF (LLEVEL.EQ.1) CALL XERPRT
     1      ('RECOVERABLE ERROR IN...',23)
            IF (LLEVEL.EQ.2) CALL XERPRT('FATAL ERROR IN...',17)
   20    CONTINUE
C        MESSAGE
         CALL XERPRT(MESSG,LMESSG)
         CALL XGETUA(LUN,NUNIT)
         ISIZEI = LOG10(FLOAT(I1MACH(9))) + 1.0
         ISIZEF = LOG10(FLOAT(I1MACH(10))**I1MACH(11)) + 1.0
         DO 50 KUNIT=1,NUNIT
            IUNIT = LUN(KUNIT)
            IF (IUNIT.EQ.0) IUNIT = I1MACH(4)
            DO 22 I=1,MIN(NI,2)
               WRITE (FORM,21) I,ISIZEI
   21          FORMAT ('(11X,21HIN ABOVE MESSAGE, I',I1,'=,I',I2,')   ')
               IF (I.EQ.1) WRITE (IUNIT,FORM) I1
               IF (I.EQ.2) WRITE (IUNIT,FORM) I2
   22       CONTINUE
            DO 24 I=1,MIN(NR,2)
               WRITE (FORM,23) I,ISIZEF+10,ISIZEF
   23          FORMAT ('(11X,21HIN ABOVE MESSAGE, R',I1,'=,E',
     1         I2,'.',I2,')')
               IF (I.EQ.1) WRITE (IUNIT,FORM) R1
               IF (I.EQ.2) WRITE (IUNIT,FORM) R2
   24       CONTINUE
            IF (LKNTRL.LE.0) GO TO 40
C              ERROR NUMBER
               WRITE (IUNIT,30) LERR
   30          FORMAT (15H ERROR NUMBER =,I10)
   40       CONTINUE
   50    CONTINUE
C        TRACE-BACK
         IF (LKNTRL.GT.0) CALL FDUMP
  100 CONTINUE
      IFATAL = 0
      IF ((LLEVEL.EQ.2).OR.((LLEVEL.EQ.1).AND.(MKNTRL.EQ.2)))
     1IFATAL = 1
C     QUIT HERE IF MESSAGE IS NOT FATAL
      IF (IFATAL.LE.0) RETURN
      IF ((LKNTRL.LE.0).OR.(KOUNT.GT.MAX0(1,MAXMES))) GO TO 120
C        PRINT REASON FOR ABORT
         IF (LLEVEL.EQ.1) CALL XERPRT
     1   ('JOB ABORT DUE TO UNRECOVERED ERROR.',35)
         IF (LLEVEL.EQ.2) CALL XERPRT
     1   ('JOB ABORT DUE TO FATAL ERROR.',29)
C        PRINT ERROR SUMMARY
         CALL XERSAV(' ',-1,0,0,KDUMMY)
  120 CONTINUE
C     ABORT
      IF ((LLEVEL.EQ.2).AND.(KOUNT.GT.MAX0(1,MAXMES))) LMESSG = 0
      CALL XERABT(MESSG,LMESSG)
      RETURN
      END
      SUBROUTINE XERSAV(MESSG,NMESSG,NERR,LEVEL,ICOUNT)
C***BEGIN PROLOGUE  XERSAV
C***DATE WRITTEN   800319   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  Z
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Records that an error occurred.
C***DESCRIPTION
C     Abstract
C        Record that this error occurred.
C
C     Description of Parameters
C     --Input--
C       MESSG, NMESSG, NERR, LEVEL are as in XERROR,
C       except that when NMESSG=0 the tables will be
C       dumped and cleared, and when NMESSG is less than zero the
C       tables will be dumped and not cleared.
C     --Output--
C       ICOUNT will be the number of times this message has
C       been seen, or zero if the table has overflowed and
C       does not contain this message specifically.
C       When NMESSG=0, ICOUNT will not be altered.
C
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C     Latest revision ---  19 Mar 1980
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  I1MACH,S88FMT,XGETUA
C***END PROLOGUE  XERSAV
      INTEGER LUN(5)
      CHARACTER*(*) MESSG
      CHARACTER*20 MESTAB(10),MES
      DIMENSION NERTAB(10),LEVTAB(10),KOUNT(10)
      SAVE MESTAB,NERTAB,LEVTAB,KOUNT,KOUNTX
C     NEXT TWO DATA STATEMENTS ARE NECESSARY TO PROVIDE A BLANK
C     ERROR TABLE INITIALLY
      DATA KOUNT(1),KOUNT(2),KOUNT(3),KOUNT(4),KOUNT(5),
     1     KOUNT(6),KOUNT(7),KOUNT(8),KOUNT(9),KOUNT(10)
     2     /0,0,0,0,0,0,0,0,0,0/
      DATA KOUNTX/0/
C***FIRST EXECUTABLE STATEMENT  XERSAV
      IF (NMESSG.GT.0) GO TO 80
C     DUMP THE TABLE
         IF (KOUNT(1).EQ.0) RETURN
C        PRINT TO EACH UNIT
         CALL XGETUA(LUN,NUNIT)
         DO 60 KUNIT=1,NUNIT
            IUNIT = LUN(KUNIT)
            IF (IUNIT.EQ.0) IUNIT = I1MACH(4)
C           PRINT TABLE HEADER
            WRITE (IUNIT,10)
   10       FORMAT (32H0          ERROR MESSAGE SUMMARY/
     1      51H MESSAGE START             NERR     LEVEL     COUNT)
C           PRINT BODY OF TABLE
            DO 20 I=1,10
               IF (KOUNT(I).EQ.0) GO TO 30
               WRITE (IUNIT,15) MESTAB(I),NERTAB(I),LEVTAB(I),KOUNT(I)
   15          FORMAT (1X,A20,3I10)
   20       CONTINUE
   30       CONTINUE
C           PRINT NUMBER OF OTHER ERRORS
            IF (KOUNTX.NE.0) WRITE (IUNIT,40) KOUNTX
   40       FORMAT (41H0OTHER ERRORS NOT INDIVIDUALLY TABULATED=,I10)
            WRITE (IUNIT,50)
   50       FORMAT (1X)
   60    CONTINUE
         IF (NMESSG.LT.0) RETURN
C        CLEAR THE ERROR TABLES
         DO 70 I=1,10
   70       KOUNT(I) = 0
         KOUNTX = 0
         RETURN
   80 CONTINUE
C     PROCESS A MESSAGE...
C     SEARCH FOR THIS MESSG, OR ELSE AN EMPTY SLOT FOR THIS MESSG,
C     OR ELSE DETERMINE THAT THE ERROR TABLE IS FULL.
      MES = MESSG
      DO 90 I=1,10
         II = I
         IF (KOUNT(I).EQ.0) GO TO 110
         IF (MES.NE.MESTAB(I)) GO TO 90
         IF (NERR.NE.NERTAB(I)) GO TO 90
         IF (LEVEL.NE.LEVTAB(I)) GO TO 90
         GO TO 100
   90 CONTINUE
C     THREE POSSIBLE CASES...
C     TABLE IS FULL
         KOUNTX = KOUNTX+1
         ICOUNT = 1
         RETURN
C     MESSAGE FOUND IN TABLE
  100    KOUNT(II) = KOUNT(II) + 1
         ICOUNT = KOUNT(II)
         RETURN
C     EMPTY SLOT FOUND FOR NEW MESSAGE
  110    MESTAB(II) = MES
         NERTAB(II) = NERR
         LEVTAB(II) = LEVEL
         KOUNT(II)  = 1
         ICOUNT = 1
         RETURN
      END
      SUBROUTINE XGETUA(IUNITA,N)
C***BEGIN PROLOGUE  XGETUA
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  R3C
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Returns unit number(s) to which error messages are being
C            sent.
C***DESCRIPTION
C     Abstract
C        XGETUA may be called to determine the unit number or numbers
C        to which error messages are being sent.
C        These unit numbers may have been set by a call to XSETUN,
C        or a call to XSETUA, or may be a default value.
C
C     Description of Parameters
C      --Output--
C        IUNIT - an array of one to five unit numbers, depending
C                on the value of N.  A value of zero refers to the
C                default unit, as defined by the I1MACH machine
C                constant routine.  Only IUNIT(1),...,IUNIT(N) are
C                defined by XGETUA.  The values of IUNIT(N+1),...,
C                IUNIT(5) are not defined (for N .LT. 5) or altered
C                in any way by XGETUA.
C        N     - the number of units to which copies of the
C                error messages are being sent.  N will be in the
C                range from 1 to 5.
C
C     Latest revision ---  19 MAR 1980
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  J4SAVE
C***END PROLOGUE  XGETUA
      DIMENSION IUNITA(5)
C***FIRST EXECUTABLE STATEMENT  XGETUA
      N = J4SAVE(5,0,.FALSE.)
      DO 30 I=1,N
         INDEX = I+4
         IF (I.EQ.1) INDEX = 3
         IUNITA(I) = J4SAVE(INDEX,0,.FALSE.)
   30 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION D1MACH(I)
C***BEGIN PROLOGUE  D1MACH
C***DATE WRITTEN   750101   (YYMMDD)
C***REVISION DATE  910131   (YYMMDD)
C***CATEGORY NO.  R1
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  FOX, P. A., (BELL LABS)
C           HALL, A. D., (BELL LABS)
C           SCHRYER, N. L., (BELL LABS)
C***PURPOSE  Returns double precision machine dependent constants
C***DESCRIPTION
C
C     This is the CMLIB version of D1MACH, the double precision machine
C     constants subroutine originally developed for the PORT library.
C
C     D1MACH can be used to obtain machine-dependent parameters
C     for the local machine environment.  It is a function
C     subprogram with one (input) argument, and can be called
C     as follows, for example
C
C          D = D1MACH(I)
C
C     where I=1,...,5.  The (output) value of D above is
C     determined by the (input) value of I.  The results for
C     various values of I are discussed below.
C
C  Double-precision machine constants
C  D1MACH( 1) = B**(EMIN-1), the smallest positive magnitude.
C  D1MACH( 2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C  D1MACH( 3) = B**(-T), the smallest relative spacing.
C  D1MACH( 4) = B**(1-T), the largest relative spacing.
C  D1MACH( 5) = LOG10(B)
C***REFERENCES  FOX P.A., HALL A.D., SCHRYER N.L.,*FRAMEWORK FOR A
C                 PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOL. 4, NO. 2, JUNE 1978, PP. 177-188.
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  D1MACH
C
      INTEGER SMALL(4)
      INTEGER LARGE(4)
      INTEGER RIGHT(4)
      INTEGER DIVER(4)
      INTEGER LOG10(4)
C
      DOUBLE PRECISION DMACH(5)
C
      EQUIVALENCE (DMACH(1),SMALL(1))
      EQUIVALENCE (DMACH(2),LARGE(1))
      EQUIVALENCE (DMACH(3),RIGHT(1))
      EQUIVALENCE (DMACH(4),DIVER(1))
      EQUIVALENCE (DMACH(5),LOG10(1))
C
C     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
C     3B SERIES AND MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
C     PC 7300), IN WHICH THE MOST SIGNIFICANT BYTE IS STORED FIRST.
C
C === MACHINE = IEEE.MOST-SIG-BYTE-FIRST
C === MACHINE = SUN
C === MACHINE = 68000
C === MACHINE = ATT.3B
C === MACHINE = ATT.7300
cprh       DATA SMALL(1),SMALL(2) /    1048576,          0 /
cprh       DATA LARGE(1),LARGE(2) / 2146435071,         -1 /
cprh       DATA RIGHT(1),RIGHT(2) / 1017118720,          0 /
cprh       DATA DIVER(1),DIVER(2) / 1018167296,          0 /
cprh       DATA LOG10(1),LOG10(2) / 1070810131, 1352628735 /
C
C     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES AND 8087-BASED
C     MICROS, SUCH AS THE IBM PC AND AT&T 6300, IN WHICH THE LEAST
C     SIGNIFICANT BYTE IS STORED FIRST.
C
C === MACHINE = IEEE.LEAST-SIG-BYTE-FIRST
C === MACHINE = 8087
C === MACHINE = IBM.PC
C === MACHINE = ATT.6300
cprh
cprh  try these for lahey
      DATA SMALL(1),SMALL(2) /          0,    1048576 /
      DATA LARGE(1),LARGE(2) /         -1, 2146435071 /
      DATA RIGHT(1),RIGHT(2) /          0, 1017118720 /
      DATA DIVER(1),DIVER(2) /          0, 1018167296 /
      DATA LOG10(1),LOG10(2) / 1352628735, 1070810131 /
C
C     MACHINE CONSTANTS FOR AMDAHL MACHINES.
C
C === MACHINE = AMDAHL
C      DATA SMALL(1),SMALL(2) /    1048576,          0 /
C      DATA LARGE(1),LARGE(2) / 2147483647,         -1 /
C      DATA RIGHT(1),RIGHT(2) /  856686592,          0 /
C      DATA DIVER(1),DIVER(2) /  873463808,          0 /
C      DATA LOG10(1),LOG10(2) / 1091781651, 1352628735 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
C
C === MACHINE = BURROUGHS.1700
C      DATA SMALL(1) / ZC00800000 /
C      DATA SMALL(2) / Z000000000 /
C      DATA LARGE(1) / ZDFFFFFFFF /
C      DATA LARGE(2) / ZFFFFFFFFF /
C      DATA RIGHT(1) / ZCC5800000 /
C      DATA RIGHT(2) / Z000000000 /
C      DATA DIVER(1) / ZCC6800000 /
C      DATA DIVER(2) / Z000000000 /
C      DATA LOG10(1) / ZD00E730E7 /
C      DATA LOG10(2) / ZC77800DC0 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
C
C === MACHINE = BURROUGHS.5700
C      DATA SMALL(1) / O1771000000000000 /
C      DATA SMALL(2) / O0000000000000000 /
C      DATA LARGE(1) / O0777777777777777 /
C      DATA LARGE(2) / O0007777777777777 /
C      DATA RIGHT(1) / O1461000000000000 /
C      DATA RIGHT(2) / O0000000000000000 /
C      DATA DIVER(1) / O1451000000000000 /
C      DATA DIVER(2) / O0000000000000000 /
C      DATA LOG10(1) / O1157163034761674 /
C      DATA LOG10(2) / O0006677466732724 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
C
C === MACHINE = BURROUGHS.6700
C === MACHINE = BURROUGHS.7700
C      DATA SMALL(1) / O1771000000000000 /
C      DATA SMALL(2) / O7770000000000000 /
C      DATA LARGE(1) / O0777777777777777 /
C      DATA LARGE(2) / O7777777777777777 /
C      DATA RIGHT(1) / O1461000000000000 /
C      DATA RIGHT(2) / O0000000000000000 /
C      DATA DIVER(1) / O1451000000000000 /
C      DATA DIVER(2) / O0000000000000000 /
C      DATA LOG10(1) / O1157163034761674 /
C      DATA LOG10(2) / O0006677466732724 /
C
C     MACHINE CONSTANTS FOR THE CONVEX C-120 (NATIVE MODE)
C     WITH OR WITHOUT -R8 OPTION
C
C === MACHINE = CONVEX.C1
C === MACHINE = CONVEX.C1.R8
C      DATA DMACH(1) / 5.562684646268007D-309 /
C      DATA DMACH(2) / 8.988465674311577D+307 /
C      DATA DMACH(3) / 1.110223024625157D-016 /
C      DATA DMACH(4) / 2.220446049250313D-016 /
C      DATA DMACH(5) / 3.010299956639812D-001 /
C
C     MACHINE CONSTANTS FOR THE CONVEX C-120 (IEEE MODE)
C     WITH OR WITHOUT -R8 OPTION
C
C === MACHINE = CONVEX.C1.IEEE
C === MACHINE = CONVEX.C1.IEEE.R8
C      DATA DMACH(1) / 2.225073858507202D-308 /
C      DATA DMACH(2) / 1.797693134862315D+308 /
C      DATA DMACH(3) / 1.110223024625157D-016 /
C      DATA DMACH(4) / 2.220446049250313D-016 /
C      DATA DMACH(5) / 3.010299956639812D-001 /
C
C     MACHINE CONSTANTS FOR THE CYBER 170/180 SERIES USING NOS (FTN5).
C
C === MACHINE = CYBER.170.NOS
C === MACHINE = CYBER.180.NOS
C      DATA SMALL(1) / O"00604000000000000000" /
C      DATA SMALL(2) / O"00000000000000000000" /
C      DATA LARGE(1) / O"37767777777777777777" /
C      DATA LARGE(2) / O"37167777777777777777" /
C      DATA RIGHT(1) / O"15604000000000000000" /
C      DATA RIGHT(2) / O"15000000000000000000" /
C      DATA DIVER(1) / O"15614000000000000000" /
C      DATA DIVER(2) / O"15010000000000000000" /
C      DATA LOG10(1) / O"17164642023241175717" /
C      DATA LOG10(2) / O"16367571421742254654" /
C
C     MACHINE CONSTANTS FOR THE CDC 180 SERIES USING NOS/VE
C
C === MACHINE = CYBER.180.NOS/VE
C      DATA SMALL(1) / Z"3001800000000000" /
C      DATA SMALL(2) / Z"3001000000000000" /
C      DATA LARGE(1) / Z"4FFEFFFFFFFFFFFE" /
C      DATA LARGE(2) / Z"4FFE000000000000" /
C      DATA RIGHT(1) / Z"3FD2800000000000" /
C      DATA RIGHT(2) / Z"3FD2000000000000" /
C      DATA DIVER(1) / Z"3FD3800000000000" /
C      DATA DIVER(2) / Z"3FD3000000000000" /
C      DATA LOG10(1) / Z"3FFF9A209A84FBCF" /
C      DATA LOG10(2) / Z"3FFFF7988F8959AC" /
C
C     MACHINE CONSTANTS FOR THE CYBER 205
C
C === MACHINE = CYBER.205
C      DATA SMALL(1) / X'9000400000000000' /
C      DATA SMALL(2) / X'8FD1000000000000' /
C      DATA LARGE(1) / X'6FFF7FFFFFFFFFFF' /
C      DATA LARGE(2) / X'6FD07FFFFFFFFFFF' /
C      DATA RIGHT(1) / X'FF74400000000000' /
C      DATA RIGHT(2) / X'FF45000000000000' /
C      DATA DIVER(1) / X'FF75400000000000' /
C      DATA DIVER(2) / X'FF46000000000000' /
C      DATA LOG10(1) / X'FFD04D104D427DE7' /
C      DATA LOG10(2) / X'FFA17DE623E2566A' /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C
C === MACHINE = CDC.6000
C === MACHINE = CDC.7000
C      DATA SMALL(1) / 00604000000000000000B /
C      DATA SMALL(2) / 00000000000000000000B /
C      DATA LARGE(1) / 37767777777777777777B /
C      DATA LARGE(2) / 37167777777777777777B /
C      DATA RIGHT(1) / 15604000000000000000B /
C      DATA RIGHT(2) / 15000000000000000000B /
C      DATA DIVER(1) / 15614000000000000000B /
C      DATA DIVER(2) / 15010000000000000000B /
C      DATA LOG10(1) / 17164642023241175717B /
C      DATA LOG10(2) / 16367571421742254654B /
C
C     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
C
C === MACHINE = CRAY.46-BIT-INTEGER
C === MACHINE = CRAY.64-BIT-INTEGER
C      DATA SMALL(1) / 201354000000000000000B /
C      DATA SMALL(2) / 000000000000000000000B /
C      DATA LARGE(1) / 577767777777777777777B /
C      DATA LARGE(2) / 000007777777777777776B /
C      DATA RIGHT(1) / 376434000000000000000B /
C      DATA RIGHT(2) / 000000000000000000000B /
C      DATA DIVER(1) / 376444000000000000000B /
C      DATA DIVER(2) / 000000000000000000000B /
C      DATA LOG10(1) / 377774642023241175717B /
C      DATA LOG10(2) / 000007571421742254654B /
C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C
C     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING LINE -
C     STATIC DMACH(5)
C
C === MACHINE = DATA_GENERAL.ECLIPSE.S/200
C      DATA SMALL/20K,3*0/,LARGE/77777K,3*177777K/
C      DATA RIGHT/31420K,3*0/,DIVER/32020K,3*0/
C      DATA LOG10/40423K,42023K,50237K,74776K/
C
C     ELXSI 6400
C
C === MACHINE = ELSXI.6400
C      DATA SMALL(1), SMALL(2) / '00100000'X,'00000000'X /
C      DATA LARGE(1), LARGE(2) / '7FEFFFFF'X,'FFFFFFFF'X /
C      DATA RIGHT(1), RIGHT(2) / '3CB00000'X,'00000000'X /
C      DATA DIVER(1), DIVER(2) / '3CC00000'X,'00000000'X /
C      DATA LOG10(1), DIVER(2) / '3FD34413'X,'509F79FF'X /
C
C     MACHINE CONSTANTS FOR THE HARRIS 220
C     MACHINE CONSTANTS FOR THE HARRIS SLASH 6 AND SLASH 7
C
C === MACHINE = HARRIS.220
C === MACHINE = HARRIS.SLASH6
C === MACHINE = HARRIS.SLASH7
C      DATA SMALL(1),SMALL(2) / '20000000, '00000201 /
C      DATA LARGE(1),LARGE(2) / '37777777, '37777577 /
C      DATA RIGHT(1),RIGHT(2) / '20000000, '00000333 /
C      DATA DIVER(1),DIVER(2) / '20000000, '00000334 /
C      DATA LOG10(1),LOG10(2) / '23210115, '10237777 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.
C     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
C
C === MACHINE = HONEYWELL.600/6000
C === MACHINE = HONEYWELL.DPS.8/70
C      DATA SMALL(1),SMALL(2) / O402400000000, O000000000000 /
C      DATA LARGE(1),LARGE(2) / O376777777777, O777777777777 /
C      DATA RIGHT(1),RIGHT(2) / O604400000000, O000000000000 /
C      DATA DIVER(1),DIVER(2) / O606400000000, O000000000000 /
C      DATA LOG10(1),LOG10(2) / O776464202324, O117571775714 /
C
C      MACHINE CONSTANTS FOR THE HP 2100
C      3 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C === MACHINE = HP.2100.3_WORD_DP
C      DATA SMALL(1), SMALL(2), SMALL(3) / 40000B,       0,       1 /
C      DATA LARGE(1), LARGE(2), LARGE(3) / 77777B, 177777B, 177776B /
C      DATA RIGHT(1), RIGHT(2), RIGHT(3) / 40000B,       0,    265B /
C      DATA DIVER(1), DIVER(2), DIVER(3) / 40000B,       0,    276B /
C      DATA LOG10(1), LOG10(2), LOG10(3) / 46420B,  46502B,  77777B /
C
C      MACHINE CONSTANTS FOR THE HP 2100
C      4 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C === MACHINE = HP.2100.4_WORD_DP
C      DATA SMALL(1), SMALL(2) /  40000B,       0 /
C      DATA SMALL(3), SMALL(4) /       0,       1 /
C      DATA LARGE(1), LARGE(2) /  77777B, 177777B /
C      DATA LARGE(3), LARGE(4) / 177777B, 177776B /
C      DATA RIGHT(1), RIGHT(2) /  40000B,       0 /
C      DATA RIGHT(3), RIGHT(4) /       0,    225B /
C      DATA DIVER(1), DIVER(2) /  40000B,       0 /
C      DATA DIVER(3), DIVER(4) /       0,    227B /
C      DATA LOG10(1), LOG10(2) /  46420B,  46502B /
C      DATA LOG10(3), LOG10(4) /  76747B, 176377B /
C
C     HP 9000
C
C      D1MACH(1) = 2.8480954D-306
C      D1MACH(2) = 1.40444776D+306
C      D1MACH(3) = 2.22044605D-16
C      D1MACH(4) = 4.44089210D-16
C      D1MACH(5) = 3.01029996D-1
C
C === MACHINE = HP.9000
C      DATA SMALL(1), SMALL(2) / 00040000000B, 00000000000B /
C      DATA LARGE(1), LARGE(2) / 17737777777B, 37777777777B /
C      DATA RIGHT(1), RIGHT(2) / 07454000000B, 00000000000B /
C      DATA DIVER(1), DIVER(2) / 07460000000B, 00000000000B /
C      DATA LOG10(1), LOG10(2) / 07764642023B, 12047674777B /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9, THE SEL SYSTEMS 85/86, AND
C     THE INTERDATA 3230 AND INTERDATA 7/32.
C
C === MACHINE = IBM.360
C === MACHINE = IBM.370
C === MACHINE = XEROX.SIGMA.5
C === MACHINE = XEROX.SIGMA.7
C === MACHINE = XEROX.SIGMA.9
C === MACHINE = SEL.85
C === MACHINE = SEL.86
C === MACHINE = INTERDATA.3230
C === MACHINE = INTERDATA.7/32
C      DATA SMALL(1),SMALL(2) / Z00100000, Z00000000 /
C      DATA LARGE(1),LARGE(2) / Z7FFFFFFF, ZFFFFFFFF /
C      DATA RIGHT(1),RIGHT(2) / Z33100000, Z00000000 /
C      DATA DIVER(1),DIVER(2) / Z34100000, Z00000000 /
C      DATA LOG10(1),LOG10(2) / Z41134413, Z509F79FF /
C
C     MACHINE CONSTANTS FOR THE INTERDATA 8/32
C     WITH THE UNIX SYSTEM FORTRAN 77 COMPILER.
C
C     FOR THE INTERDATA FORTRAN VII COMPILER REPLACE
C     THE Z'S SPECIFYING HEX CONSTANTS WITH Y'S.
C
C === MACHINE = INTERDATA.8/32.UNIX
C      DATA SMALL(1),SMALL(2) / Z'00100000', Z'00000000' /
C      DATA LARGE(1),LARGE(2) / Z'7EFFFFFF', Z'FFFFFFFF' /
C      DATA RIGHT(1),RIGHT(2) / Z'33100000', Z'00000000' /
C      DATA DIVER(1),DIVER(2) / Z'34100000', Z'00000000' /
C      DATA LOG10(1),LOG10(2) / Z'41134413', Z'509F79FF' /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
C
C === MACHINE = PDP-10.KA
C      DATA SMALL(1),SMALL(2) / "033400000000, "000000000000 /
C      DATA LARGE(1),LARGE(2) / "377777777777, "344777777777 /
C      DATA RIGHT(1),RIGHT(2) / "113400000000, "000000000000 /
C      DATA DIVER(1),DIVER(2) / "114400000000, "000000000000 /
C      DATA LOG10(1),LOG10(2) / "177464202324, "144117571776 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
C
C === MACHINE = PDP-10.KI
C      DATA SMALL(1),SMALL(2) / "000400000000, "000000000000 /
C      DATA LARGE(1),LARGE(2) / "377777777777, "377777777777 /
C      DATA RIGHT(1),RIGHT(2) / "103400000000, "000000000000 /
C      DATA DIVER(1),DIVER(2) / "104400000000, "000000000000 /
C      DATA LOG10(1),LOG10(2) / "177464202324, "047674776746 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C === MACHINE = PDP-11.32-BIT
C      DATA SMALL(1),SMALL(2) /    8388608,           0 /
C      DATA LARGE(1),LARGE(2) / 2147483647,          -1 /
C      DATA RIGHT(1),RIGHT(2) /  612368384,           0 /
C      DATA DIVER(1),DIVER(2) /  620756992,           0 /
C      DATA LOG10(1),LOG10(2) / 1067065498, -2063872008 /
C
C      DATA SMALL(1),SMALL(2) / O00040000000, O00000000000 /
C      DATA LARGE(1),LARGE(2) / O17777777777, O37777777777 /
C      DATA RIGHT(1),RIGHT(2) / O04440000000, O00000000000 /
C      DATA DIVER(1),DIVER(2) / O04500000000, O00000000000 /
C      DATA LOG10(1),LOG10(2) / O07746420232, O20476747770 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRAN SUPPORTING
C     16-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).
C
C === MACHINE = PDP-11.16-BIT
C      DATA SMALL(1),SMALL(2) /    128,      0 /
C      DATA SMALL(3),SMALL(4) /      0,      0 /
C      DATA LARGE(1),LARGE(2) /  32767,     -1 /
C      DATA LARGE(3),LARGE(4) /     -1,     -1 /
C      DATA RIGHT(1),RIGHT(2) /   9344,      0 /
C      DATA RIGHT(3),RIGHT(4) /      0,      0 /
C      DATA DIVER(1),DIVER(2) /   9472,      0 /
C      DATA DIVER(3),DIVER(4) /      0,      0 /
C      DATA LOG10(1),LOG10(2) /  16282,   8346 /
C      DATA LOG10(3),LOG10(4) / -31493, -12296 /
C
C      DATA SMALL(1),SMALL(2) / O000200, O000000 /
C      DATA SMALL(3),SMALL(4) / O000000, O000000 /
C      DATA LARGE(1),LARGE(2) / O077777, O177777 /
C      DATA LARGE(3),LARGE(4) / O177777, O177777 /
C      DATA RIGHT(1),RIGHT(2) / O022200, O000000 /
C      DATA RIGHT(3),RIGHT(4) / O000000, O000000 /
C      DATA DIVER(1),DIVER(2) / O022400, O000000 /
C      DATA DIVER(3),DIVER(4) / O000000, O000000 /
C      DATA LOG10(1),LOG10(2) / O037632, O020232 /
C      DATA LOG10(3),LOG10(4) / O102373, O147770 /
C
C     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000
C
C === MACHINE = SEQUENT.BALANCE.8000
C      DATA SMALL(1),SMALL(2) / $00000000,  $00100000 /
C      DATA LARGE(1),LARGE(2) / $FFFFFFFF,  $7FEFFFFF /
C      DATA RIGHT(1),RIGHT(2) / $00000000,  $3CA00000 /
C      DATA DIVER(1),DIVER(2) / $00000000,  $3CB00000 /
C      DATA LOG10(1),LOG10(2) / $509F79FF,  $3FD34413 /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES. FTN COMPILER
C
C === MACHINE = UNIVAC.1100
C      DATA SMALL(1),SMALL(2) / O000040000000, O000000000000 /
C      DATA LARGE(1),LARGE(2) / O377777777777, O777777777777 /
C      DATA RIGHT(1),RIGHT(2) / O170540000000, O000000000000 /
C      DATA DIVER(1),DIVER(2) / O170640000000, O000000000000 /
C      DATA LOG10(1),LOG10(2) / O177746420232, O411757177572 /
C
C     MACHINE CONSTANTS FOR VAX 11/780
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C    *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***
C
C === MACHINE = VAX.11/780
C      DATA SMALL(1), SMALL(2) /        128,           0 /
C      DATA LARGE(1), LARGE(2) /     -32769,          -1 /
C      DATA RIGHT(1), RIGHT(2) /       9344,           0 /
C      DATA DIVER(1), DIVER(2) /       9472,           0 /
C      DATA LOG10(1), LOG10(2) /  546979738,  -805796613 /
C
C    ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSYEMS***
C      DATA SMALL(1), SMALL(2) / Z00000080, Z00000000 /
C      DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C      DATA RIGHT(1), RIGHT(2) / Z00002480, Z00000000 /
C      DATA DIVER(1), DIVER(2) / Z00002500, Z00000000 /
C      DATA LOG10(1), LOG10(2) / Z209A3F9A, ZCFF884FB /
C
C   MACHINE CONSTANTS FOR VAX 11/780 (G-FLOATING)
C     (EXPRESSED IN INTEGER AND HEXADECIMAL)
C    *** THE INTEGER FORMAT SHOULD BE OK FOR UNIX SYSTEMS***
C
C      DATA SMALL(1), SMALL(2) /         16,           0 /
C      DATA LARGE(1), LARGE(2) /     -32769,          -1 /
C      DATA RIGHT(1), RIGHT(2) /      15552,           0 /
C      DATA DIVER(1), DIVER(2) /      15568,           0 /
C      DATA LOG10(1), LOG10(2) /  1142112243, 2046775455 /
C
C    ***THE HEX FORMAT BELOW MAY NOT BE SUITABLE FOR UNIX SYSYEMS***
C      DATA SMALL(1), SMALL(2) / Z00000010, Z00000000 /
C      DATA LARGE(1), LARGE(2) / ZFFFF7FFF, ZFFFFFFFF /
C      DATA RIGHT(1), RIGHT(2) / Z00003CC0, Z00000000 /
C      DATA DIVER(1), DIVER(2) / Z00003CD0, Z00000000 /
C      DATA LOG10(1), LOG10(2) / Z44133FF3, Z79FF509F /
C
C
C***FIRST EXECUTABLE STATEMENT  D1MACH
      IF (I .LT. 1  .OR.  I .GT. 5)
     1   CALL XERROR( 'D1MACH -- I OUT OF BOUNDS',25,1,2)
C
      D1MACH = DMACH(I)
      RETURN
C
      END
      DOUBLE PRECISION FUNCTION D9LGMC(X)
C***BEGIN PROLOGUE  D9LGMC
C***DATE WRITTEN   770601   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C7E
C***KEYWORDS  COMPLETE GAMMA FUNCTION,CORRECTION FACTOR,
C             DOUBLE PRECISION,GAMMA FUNCTION,LOGARITHM,
C             SPECIAL FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Computes the  d.p. log Gamma correction factor for
C            X .GE. 10. so that DLOG(DGAMMA(X)) = DLOG(DSQRT(2*PI)) +
C            (X-5.)*DLOG(X) - X + D9LGMC(X)
C***DESCRIPTION
C
C Compute the log gamma correction factor for X .GE. 10. so that
C DLOG (DGAMMA(X)) = DLOG(DSQRT(2*PI)) + (X-.5)*DLOG(X) - X + D9lGMC(X)
C
C Series for ALGM       on the interval  0.          to  1.00000E-02
C                                        with weighted error   1.28E-31
C                                         log weighted error  30.89
C                               significant figures required  29.81
C                                    decimal places required  31.48
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH,DCSEVL,INITDS,XERROR
C***END PROLOGUE  D9LGMC
      DOUBLE PRECISION X, ALGMCS(15), XBIG, XMAX, DCSEVL, D1MACH
      DATA ALGMCS(  1) / +.1666389480 4518632472 0572965082 2 D+0      /
      DATA ALGMCS(  2) / -.1384948176 0675638407 3298605913 5 D-4      /
      DATA ALGMCS(  3) / +.9810825646 9247294261 5717154748 7 D-8      /
      DATA ALGMCS(  4) / -.1809129475 5724941942 6330626671 9 D-10     /
      DATA ALGMCS(  5) / +.6221098041 8926052271 2601554341 6 D-13     /
      DATA ALGMCS(  6) / -.3399615005 4177219443 0333059966 6 D-15     /
      DATA ALGMCS(  7) / +.2683181998 4826987489 5753884666 6 D-17     /
      DATA ALGMCS(  8) / -.2868042435 3346432841 4462239999 9 D-19     /
      DATA ALGMCS(  9) / +.3962837061 0464348036 7930666666 6 D-21     /
      DATA ALGMCS( 10) / -.6831888753 9857668701 1199999999 9 D-23     /
      DATA ALGMCS( 11) / +.1429227355 9424981475 7333333333 3 D-24     /
      DATA ALGMCS( 12) / -.3547598158 1010705471 9999999999 9 D-26     /
      DATA ALGMCS( 13) / +.1025680058 0104709120 0000000000 0 D-27     /
      DATA ALGMCS( 14) / -.3401102254 3167487999 9999999999 9 D-29     /
      DATA ALGMCS( 15) / +.1276642195 6300629333 3333333333 3 D-30     /
      DATA NALGM, XBIG, XMAX / 0, 2*0.D0 /
C***FIRST EXECUTABLE STATEMENT  D9LGMC
      IF (NALGM.NE.0) GO TO 10
      NALGM = INITDS (ALGMCS, 15, SNGL(D1MACH(3)) )
      XBIG = 1.0D0/DSQRT(D1MACH(3))
      XMAX = DEXP (DMIN1(DLOG(D1MACH(2)/12.D0), -DLOG(12.D0*D1MACH(1))))
C
 10   IF (X.LT.10.D0) CALL XERROR ( 'D9LGMC  X MUST BE GE 10', 23, 1, 2)
      IF (X.GE.XMAX) GO TO 20
C
      D9LGMC = 1.D0/(12.D0*X)
      IF (X.LT.XBIG) D9LGMC = DCSEVL (2.0D0*(10.D0/X)**2-1.D0, ALGMCS,
     1  NALGM) / X
      RETURN
C
 20   D9LGMC = 0.D0
      CALL XERROR ( 'D9LGMC  X SO BIG D9LGMC UNDERFLOWS', 34, 2, 1)
      RETURN
C
      END
      DOUBLE PRECISION FUNCTION DCSEVL(X,A,N)
C***BEGIN PROLOGUE  DCSEVL
C***DATE WRITTEN   770401   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C3A2
C***KEYWORDS  CHEBYSHEV,FNLIB,SPECIAL FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Evaluate the double precision N-term Chebyshev series A
C            at X.
C***DESCRIPTION
C
C Evaluate the N-term Chebyshev series A at X.  Adapted from
C R. Broucke, Algorithm 446, C.A.C.M., 16, 254 (1973).
C W. Fullerton, C-3, Los Alamos Scientific Laboratory.
C
C       Input Arguments --
C X    double precision value at which the series is to be evaluated.
C A    double precision array of N terms of a Chebyshev series.  In
C      evaluating A, only half of the first coefficient is summed.
C N    number of terms in array A.
C***REFERENCES  (NONE)
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  DCSEVL
C
       DOUBLE PRECISION A(N),X,TWOX,B0,B1,B2
C***FIRST EXECUTABLE STATEMENT  DCSEVL
       IF(N.LT.1)CALL XERROR( 'DCSEVL  NUMBER OF TERMS LE 0', 28, 2,2)
       IF(N.GT.1000) CALL XERROR ( 'DCSEVL  NUMBER OF TERMS GT 1000',
     1   31, 3, 2)
       IF ((X.LT.-1.D0) .OR. (X.GT.1.D0)) CALL XERROR ( 'DCSEVL  X OUTSI
     1DE (-1,+1)', 25, 1, 1)
C
       TWOX = 2.0D0*X
       B1 = 0.D0
       B0=0.D0
       DO 10 I=1,N
         B2=B1
         B1=B0
         NI = N - I + 1
         B0 = TWOX*B1 - B2 + A(NI)
 10    CONTINUE
C
       DCSEVL = 0.5D0 * (B0-B2)
C
       RETURN
      END
      SUBROUTINE DGAMLM(XMIN,XMAX)
C***BEGIN PROLOGUE  DGAMLM
C***DATE WRITTEN   770601   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C7A,R2
C***KEYWORDS  COMPLETE GAMMA FUNCTION,DOUBLE PRECISION,GAMMA FUNCTION,
C             LIMITS,SPECIAL FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Computes the d.p. minimum and maximum bounds for X in
C            GAMMA(X).
C***DESCRIPTION
C
C Calculate the minimum and maximum legal bounds for X in gamma(X).
C XMIN and XMAX are not the only bounds, but they are the only non-
C trivial ones to calculate.
C
C             Output Arguments --
C XMIN   double precision minimum legal value of X in gamma(X).  Any
C        smaller value of X might result in underflow.
C XMAX   double precision maximum legal value of X in gamma(X).  Any
C        larger value of X might cause overflow.
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH,XERROR
C***END PROLOGUE  DGAMLM
      DOUBLE PRECISION XMIN, XMAX, ALNBIG, ALNSML, XLN, XOLD, D1MACH
C***FIRST EXECUTABLE STATEMENT  DGAMLM
      ALNSML = DLOG(D1MACH(1))
      XMIN = -ALNSML
      DO 10 I=1,10
        XOLD = XMIN
        XLN = DLOG(XMIN)
        XMIN = XMIN - XMIN*((XMIN+0.5D0)*XLN - XMIN - 0.2258D0 + ALNSML)
     1    / (XMIN*XLN+0.5D0)
        IF (DABS(XMIN-XOLD).LT.0.005D0) GO TO 20
 10   CONTINUE
      CALL XERROR ( 'DGAMLM  UNABLE TO FIND XMIN', 27, 1, 2)
C
 20   XMIN = -XMIN + 0.01D0
C
      ALNBIG = DLOG (D1MACH(2))
      XMAX = ALNBIG
      DO 30 I=1,10
        XOLD = XMAX
        XLN = DLOG(XMAX)
        XMAX = XMAX - XMAX*((XMAX-0.5D0)*XLN - XMAX + 0.9189D0 - ALNBIG)
     1    / (XMAX*XLN-0.5D0)
        IF (DABS(XMAX-XOLD).LT.0.005D0) GO TO 40
 30   CONTINUE
      CALL XERROR ( 'DGAMLM  UNABLE TO FIND XMAX', 27, 2, 2)
C
 40   XMAX = XMAX - 0.01D0
      XMIN = DMAX1 (XMIN, -XMAX+1.D0)
C
      RETURN
      END
      SUBROUTINE FDUMP
C***BEGIN PROLOGUE  FDUMP
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  Z
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Symbolic dump (should be locally written).
C***DESCRIPTION
C        ***Note*** Machine Dependent Routine
C        FDUMP is intended to be replaced by a locally written
C        version which produces a symbolic dump.  Failing this,
C        it should be replaced by a version which prints the
C        subprogram nesting list.  Note that this dump must be
C        printed on each of up to five files, as indicated by the
C        XGETUA routine.  See XSETUA and XGETUA for details.
C
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C     Latest revision ---  23 May 1979
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  FDUMP
C***FIRST EXECUTABLE STATEMENT  FDUMP
      RETURN
      END
      INTEGER FUNCTION I1MACH(I)
C***BEGIN PROLOGUE  I1MACH
C***DATE WRITTEN   750101   (YYMMDD)
C***REVISION DATE  910131   (YYMMDD)
C***CATEGORY NO.  R1
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  FOX, P. A., (BELL LABS)
C           HALL, A. D., (BELL LABS)
C           SCHRYER, N. L., (BELL LABS)
C***PURPOSE  Returns integer machine dependent constants
C***DESCRIPTION
C
C     This is the CMLIB version of I1MACH, the integer machine
C     constants subroutine originally developed for the PORT library.
C
C     I1MACH can be used to obtain machine-dependent parameters
C     for the local machine environment.  It is a function
C     subroutine with one (input) argument, and can be called
C     as follows, for example
C
C          K = I1MACH(I)
C
C     where I=1,...,16.  The (output) value of K above is
C     determined by the (input) value of I.  The results for
C     various values of I are discussed below.
C
C  I/O unit numbers.
C    I1MACH( 1) = the standard input unit.
C    I1MACH( 2) = the standard output unit.
C    I1MACH( 3) = the standard punch unit.
C    I1MACH( 4) = the standard error message unit.
C
C  Words.
C    I1MACH( 5) = the number of bits per integer storage unit.
C    I1MACH( 6) = the number of characters per integer storage unit.
C
C  Integers.
C    assume integers are represented in the S-digit, base-A form
C
C               sign ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C
C               where 0 .LE. X(I) .LT. A for I=0,...,S-1.
C    I1MACH( 7) = A, the base.
C    I1MACH( 8) = S, the number of base-A digits.
C    I1MACH( 9) = A**S - 1, the largest magnitude.
C
C  Floating-Point Numbers.
C    Assume floating-point numbers are represented in the T-digit,
C    base-B form
C               sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C               where 0 .LE. X(I) .LT. B for I=1,...,T,
C               0 .LT. X(1), and EMIN .LE. E .LE. EMAX.
C    I1MACH(10) = B, the base.
C
C  Single-Precision
C    I1MACH(11) = T, the number of base-B digits.
C    I1MACH(12) = EMIN, the smallest exponent E.
C    I1MACH(13) = EMAX, the largest exponent E.
C
C  Double-Precision
C    I1MACH(14) = T, the number of base-B digits.
C    I1MACH(15) = EMIN, the smallest exponent E.
C    I1MACH(16) = EMAX, the largest exponent E.
C
C  To alter this function for a particular environment,
C  the desired set of DATA statements should be activated by
C  removing the C from column 1.  Also, the values of
C  I1MACH(1) - I1MACH(4) should be checked for consistency
C  with the local operating system.
C***REFERENCES  FOX P.A., HALL A.D., SCHRYER N.L.,*FRAMEWORK FOR A
C                 PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOL. 4, NO. 2, JUNE 1978, PP. 177-188.
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  I1MACH
C
      INTEGER IMACH(16),OUTPUT
      EQUIVALENCE (IMACH(4),OUTPUT)
C
C     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
C     3B SERIES, MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
C     PC 7300), AND 8087 BASED MICROS (E.G. IBM PC AND AT&T 6300).
C
C === MACHINE = IEEE.MOST-SIG-BYTE-FIRST
C === MACHINE = IEEE.LEAST-SIG-BYTE-FIRST
C === MACHINE = SUN
C === MACHINE = 68000
C === MACHINE = 8087
C === MACHINE = IBM.PC
C === MACHINE = ATT.3B
C === MACHINE = ATT.7300
C === MACHINE = ATT.6300
       DATA IMACH( 1) /    5 /
       DATA IMACH( 2) /    6 /
       DATA IMACH( 3) /    7 /
       DATA IMACH( 4) /    6 /
       DATA IMACH( 5) /   32 /
       DATA IMACH( 6) /    4 /
       DATA IMACH( 7) /    2 /
       DATA IMACH( 8) /   31 /
       DATA IMACH( 9) / 2147483647 /
       DATA IMACH(10) /    2 /
       DATA IMACH(11) /   24 /
       DATA IMACH(12) / -125 /
       DATA IMACH(13) /  128 /
       DATA IMACH(14) /   53 /
       DATA IMACH(15) / -1021 /
       DATA IMACH(16) /  1024 /
C
C     MACHINE CONSTANTS FOR AMDAHL MACHINES.
C
C === MACHINE = AMDAHL
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   7 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  32 /
C      DATA IMACH( 6) /   4 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /  16 /
C      DATA IMACH(11) /   6 /
C      DATA IMACH(12) / -64 /
C      DATA IMACH(13) /  63 /
C      DATA IMACH(14) /  14 /
C      DATA IMACH(15) / -64 /
C      DATA IMACH(16) /  63 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
C
C === MACHINE = BURROUGHS.1700
C      DATA IMACH( 1) /    7 /
C      DATA IMACH( 2) /    2 /
C      DATA IMACH( 3) /    2 /
C      DATA IMACH( 4) /    2 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   33 /
C      DATA IMACH( 9) / Z1FFFFFFFF /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -256 /
C      DATA IMACH(13) /  255 /
C      DATA IMACH(14) /   60 /
C      DATA IMACH(15) / -256 /
C      DATA IMACH(16) /  255 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
C
C === MACHINE = BURROUGHS.5700
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   7 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  48 /
C      DATA IMACH( 6) /   6 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  39 /
C      DATA IMACH( 9) / O0007777777777777 /
C      DATA IMACH(10) /   8 /
C      DATA IMACH(11) /  13 /
C      DATA IMACH(12) / -50 /
C      DATA IMACH(13) /  76 /
C      DATA IMACH(14) /  26 /
C      DATA IMACH(15) / -50 /
C      DATA IMACH(16) /  76 /
C
C     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
C
C === MACHINE = BURROUGHS.6700
C === MACHINE = BURROUGHS.7700
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   7 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  48 /
C      DATA IMACH( 6) /   6 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  39 /
C      DATA IMACH( 9) / O0007777777777777 /
C      DATA IMACH(10) /   8 /
C      DATA IMACH(11) /  13 /
C      DATA IMACH(12) / -50 /
C      DATA IMACH(13) /  76 /
C      DATA IMACH(14) /  26 /
C      DATA IMACH(15) / -32754 /
C      DATA IMACH(16) /  32780 /
C
C     MACHINE CONSTANTS FOR THE CONVEX C-120 (NATIVE MODE)
C
C === MACHINE = CONVEX.C1
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    0 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   53 /
C      DATA IMACH(15) / -1023 /
C      DATA IMACH(16) /  1023 /
C
C     MACHINE CONSTANTS FOR THE CONVEX (NATIVE MODE)
C     WITH -R8 OPTION
C
C === MACHINE = CONVEX.C1.R8
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /     0 /
C      DATA IMACH( 4) /     6 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     4 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    53 /
C      DATA IMACH(12) / -1023 /
C      DATA IMACH(13) /  1023 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1023 /
C      DATA IMACH(16) /  1023 /
C
C     MACHINE CONSTANTS FOR THE CONVEX C-120 (IEEE MODE)
C
C === MACHINE = CONVEX.C1.IEEE
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    0 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -125 /
C      DATA IMACH(13) /  128 /
C      DATA IMACH(14) /   53 /
C      DATA IMACH(15) / -1021 /
C      DATA IMACH(16) /  1024 /
C
C     MACHINE CONSTANTS FOR THE CONVEX (IEEE MODE)
C     WITH -R8 OPTION
C
C === MACHINE = CONVEX.C1.IEEE.R8
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /     0 /
C      DATA IMACH( 4) /     6 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     4 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    53 /
C      DATA IMACH(12) / -1021 /
C      DATA IMACH(13) /  1024 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1021 /
C      DATA IMACH(16) /  1024 /
C
C     MACHINE CONSTANTS FOR THE CYBER 170/180 SERIES USING NOS (FTN5).
C
C === MACHINE = CYBER.170.NOS
C === MACHINE = CYBER.180.NOS
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   60 /
C      DATA IMACH( 6) /   10 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   48 /
C      DATA IMACH( 9) / O"00007777777777777777" /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   48 /
C      DATA IMACH(12) / -974 /
C      DATA IMACH(13) / 1070 /
C      DATA IMACH(14) /   96 /
C      DATA IMACH(15) / -927 /
C      DATA IMACH(16) / 1070 /
C
C     MACHINE CONSTANTS FOR THE CDC 180 SERIES USING NOS/VE
C
C === MACHINE = CYBER.180.NOS/VE
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /     7 /
C      DATA IMACH( 4) /     6 /
C      DATA IMACH( 5) /    64 /
C      DATA IMACH( 6) /     8 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    63 /
C      DATA IMACH( 9) / 9223372036854775807 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    47 /
C      DATA IMACH(12) / -4095 /
C      DATA IMACH(13) /  4094 /
C      DATA IMACH(14) /    94 /
C      DATA IMACH(15) / -4095 /
C      DATA IMACH(16) /  4094 /
C
C     MACHINE CONSTANTS FOR THE CYBER 205
C
C === MACHINE = CYBER.205
C      DATA IMACH( 1) /      5 /
C      DATA IMACH( 2) /      6 /
C      DATA IMACH( 3) /      7 /
C      DATA IMACH( 4) /      6 /
C      DATA IMACH( 5) /     64 /
C      DATA IMACH( 6) /      8 /
C      DATA IMACH( 7) /      2 /
C      DATA IMACH( 8) /     47 /
C      DATA IMACH( 9) / X'00007FFFFFFFFFFF' /
C      DATA IMACH(10) /      2 /
C      DATA IMACH(11) /     47 /
C      DATA IMACH(12) / -28625 /
C      DATA IMACH(13) /  28718 /
C      DATA IMACH(14) /     94 /
C      DATA IMACH(15) / -28625 /
C      DATA IMACH(16) /  28718 /
C
C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
C
C === MACHINE = CDC.6000
C === MACHINE = CDC.7000
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   60 /
C      DATA IMACH( 6) /   10 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   48 /
C      DATA IMACH( 9) / 00007777777777777777B /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   48 /
C      DATA IMACH(12) / -974 /
C      DATA IMACH(13) / 1070 /
C      DATA IMACH(14) /   96 /
C      DATA IMACH(15) / -927 /
C      DATA IMACH(16) / 1070 /
C
C     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
C     USING THE 46 BIT INTEGER COMPILER OPTION
C
C === MACHINE = CRAY.46-BIT-INTEGER
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /   102 /
C      DATA IMACH( 4) /     6 /
C      DATA IMACH( 5) /    64 /
C      DATA IMACH( 6) /     8 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    46 /
C      DATA IMACH( 9) /  777777777777777777777B /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    47 /
C      DATA IMACH(12) / -8189 /
C      DATA IMACH(13) /  8190 /
C      DATA IMACH(14) /    94 /
C      DATA IMACH(15) / -8099 /
C      DATA IMACH(16) /  8190 /
C
C     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
C     USING THE 64 BIT INTEGER COMPILER OPTION
C
C === MACHINE = CRAY.64-BIT-INTEGER
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /   102 /
C      DATA IMACH( 4) /     6 /
C      DATA IMACH( 5) /    64 /
C      DATA IMACH( 6) /     8 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    63 /
C      DATA IMACH( 9) /  777777777777777777777B /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    47 /
C      DATA IMACH(12) / -8189 /
C      DATA IMACH(13) /  8190 /
C      DATA IMACH(14) /    94 /
C      DATA IMACH(15) / -8099 /
C      DATA IMACH(16) /  8190 /C
C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
C
C === MACHINE = DATA_GENERAL.ECLIPSE.S/200
C      DATA IMACH( 1) /   11 /
C      DATA IMACH( 2) /   12 /
C      DATA IMACH( 3) /    8 /
C      DATA IMACH( 4) /   10 /
C      DATA IMACH( 5) /   16 /
C      DATA IMACH( 6) /    2 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   15 /
C      DATA IMACH( 9) /32767 /
C      DATA IMACH(10) /   16 /
C      DATA IMACH(11) /    6 /
C      DATA IMACH(12) /  -64 /
C      DATA IMACH(13) /   63 /
C      DATA IMACH(14) /   14 /
C      DATA IMACH(15) /  -64 /
C      DATA IMACH(16) /   63 /
C
C     ELXSI  6400
C
C === MACHINE = ELSXI.6400
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /     6 /
C      DATA IMACH( 4) /     6 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     4 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    32 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    24 /
C      DATA IMACH(12) /  -126 /
C      DATA IMACH(13) /   127 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1022 /
C      DATA IMACH(16) /  1023 /
C
C     MACHINE CONSTANTS FOR THE HARRIS 220
C     MACHINE CONSTANTS FOR THE HARRIS SLASH 6 AND SLASH 7
C
C === MACHINE = HARRIS.220
C === MACHINE = HARRIS.SLASH6
C === MACHINE = HARRIS.SLASH7
C      DATA IMACH( 1) /       5 /
C      DATA IMACH( 2) /       6 /
C      DATA IMACH( 3) /       0 /
C      DATA IMACH( 4) /       6 /
C      DATA IMACH( 5) /      24 /
C      DATA IMACH( 6) /       3 /
C      DATA IMACH( 7) /       2 /
C      DATA IMACH( 8) /      23 /
C      DATA IMACH( 9) / 8388607 /
C      DATA IMACH(10) /       2 /
C      DATA IMACH(11) /      23 /
C      DATA IMACH(12) /    -127 /
C      DATA IMACH(13) /     127 /
C      DATA IMACH(14) /      38 /
C      DATA IMACH(15) /    -127 /
C      DATA IMACH(16) /     127 /
C
C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.
C     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
C
C === MACHINE = HONEYWELL.600/6000
C === MACHINE = HONEYWELL.DPS.8/70
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /   43 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / O377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   63 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     3 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C === MACHINE = HP.2100.3_WORD_DP
C      DATA IMACH(1) /      5/
C      DATA IMACH(2) /      6 /
C      DATA IMACH(3) /      4 /
C      DATA IMACH(4) /      1 /
C      DATA IMACH(5) /     16 /
C      DATA IMACH(6) /      2 /
C      DATA IMACH(7) /      2 /
C      DATA IMACH(8) /     15 /
C      DATA IMACH(9) /  32767 /
C      DATA IMACH(10)/      2 /
C      DATA IMACH(11)/     23 /
C      DATA IMACH(12)/   -128 /
C      DATA IMACH(13)/    127 /
C      DATA IMACH(14)/     39 /
C      DATA IMACH(15)/   -128 /
C      DATA IMACH(16)/    127 /
C
C     MACHINE CONSTANTS FOR THE HP 2100
C     4 WORD DOUBLE PRECISION OPTION WITH FTN4
C
C === MACHINE = HP.2100.4_WORD_DP
C      DATA IMACH(1) /      5 /
C      DATA IMACH(2) /      6 /
C      DATA IMACH(3) /      4 /
C      DATA IMACH(4) /      1 /
C      DATA IMACH(5) /     16 /
C      DATA IMACH(6) /      2 /
C      DATA IMACH(7) /      2 /
C      DATA IMACH(8) /     15 /
C      DATA IMACH(9) /  32767 /
C      DATA IMACH(10)/      2 /
C      DATA IMACH(11)/     23 /
C      DATA IMACH(12)/   -128 /
C      DATA IMACH(13)/    127 /
C      DATA IMACH(14)/     55 /
C      DATA IMACH(15)/   -128 /
C      DATA IMACH(16)/    127 /
C
C     HP 9000
C
C === MACHINE = HP.9000
C      DATA IMACH( 1) /     5 /
C      DATA IMACH( 2) /     6 /
C      DATA IMACH( 3) /     6 /
C      DATA IMACH( 4) /     7 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     4 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    32 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    24 /
C      DATA IMACH(12) /  -126 /
C      DATA IMACH(13) /   127 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1015 /
C      DATA IMACH(16) /  1017 /
C
C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
C     THE XEROX SIGMA 5/7/9 AND THE SEL SYSTEMS 85/86 AND
C     THE INTERDATA 3230 AND INTERDATA 7/32.
C
C === MACHINE = IBM.360
C === MACHINE = IBM.370
C === MACHINE = XEROX.SIGMA.5
C === MACHINE = XEROX.SIGMA.7
C === MACHINE = XEROX.SIGMA.9
C === MACHINE = SEL.85
C === MACHINE = SEL.86
C === MACHINE = INTERDATA.3230
C === MACHINE = INTERDATA.7/32
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   7 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  32 /
C      DATA IMACH( 6) /   4 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  31 /
C      DATA IMACH( 9) / Z7FFFFFFF /
C      DATA IMACH(10) /  16 /
C      DATA IMACH(11) /   6 /
C      DATA IMACH(12) / -64 /
C      DATA IMACH(13) /  63 /
C      DATA IMACH(14) /  14 /
C      DATA IMACH(15) / -64 /
C      DATA IMACH(16) /  63 /
C
C     MACHINE CONSTANTS FOR THE INTERDATA 8/32
C     WITH THE UNIX SYSTEM FORTRAN 77 COMPILER.
C
C     FOR THE INTERDATA FORTRAN VII COMPILER REPLACE
C     THE Z'S SPECIFYING HEX CONSTANTS WITH Y'S.
C
C === MACHINE = INTERDATA.8/32.UNIX
C      DATA IMACH( 1) /   5 /
C      DATA IMACH( 2) /   6 /
C      DATA IMACH( 3) /   6 /
C      DATA IMACH( 4) /   6 /
C      DATA IMACH( 5) /  32 /
C      DATA IMACH( 6) /   4 /
C      DATA IMACH( 7) /   2 /
C      DATA IMACH( 8) /  31 /
C      DATA IMACH( 9) / Z'7FFFFFFF' /
C      DATA IMACH(10) /  16 /
C      DATA IMACH(11) /   6 /
C      DATA IMACH(12) / -64 /
C      DATA IMACH(13) /  62 /
C      DATA IMACH(14) /  14 /
C      DATA IMACH(15) / -64 /
C      DATA IMACH(16) /  62 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
C
C === MACHINE = PDP-10.KA
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    5 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / "377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   54 /
C      DATA IMACH(15) / -101 /
C      DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
C
C === MACHINE = PDP-10.KI
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    5 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / "377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   62 /
C      DATA IMACH(15) / -128 /
C      DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
C     32-BIT INTEGER ARITHMETIC.
C
C === MACHINE = PDP-11.32-BIT
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   32 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   31 /
C      DATA IMACH( 9) / 2147483647 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   56 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR PDP-11 FORTRANS SUPPORTING
C     16-BIT INTEGER ARITHMETIC.
C
C === MACHINE = PDP-11.16-BIT
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    7 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   16 /
C      DATA IMACH( 6) /    2 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   15 /
C      DATA IMACH( 9) / 32767 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   24 /
C      DATA IMACH(12) / -127 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   56 /
C      DATA IMACH(15) / -127 /
C      DATA IMACH(16) /  127 /
C
C     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000.
C
C === MACHINE = SEQUENT.BALANCE.8000
C      DATA IMACH( 1) /     0 /
C      DATA IMACH( 2) /     0 /
C      DATA IMACH( 3) /     7 /
C      DATA IMACH( 4) /     0 /
C      DATA IMACH( 5) /    32 /
C      DATA IMACH( 6) /     1 /
C      DATA IMACH( 7) /     2 /
C      DATA IMACH( 8) /    31 /
C      DATA IMACH( 9) /  2147483647 /
C      DATA IMACH(10) /     2 /
C      DATA IMACH(11) /    24 /
C      DATA IMACH(12) /  -125 /
C      DATA IMACH(13) /   128 /
C      DATA IMACH(14) /    53 /
C      DATA IMACH(15) / -1021 /
C      DATA IMACH(16) /  1024 /
C
C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES. FTN COMPILER
C
C === MACHINE = UNIVAC.1100
C      DATA IMACH( 1) /    5 /
C      DATA IMACH( 2) /    6 /
C      DATA IMACH( 3) /    1 /
C      DATA IMACH( 4) /    6 /
C      DATA IMACH( 5) /   36 /
C      DATA IMACH( 6) /    4 /
C      DATA IMACH( 7) /    2 /
C      DATA IMACH( 8) /   35 /
C      DATA IMACH( 9) / O377777777777 /
C      DATA IMACH(10) /    2 /
C      DATA IMACH(11) /   27 /
C      DATA IMACH(12) / -128 /
C      DATA IMACH(13) /  127 /
C      DATA IMACH(14) /   60 /
C      DATA IMACH(15) /-1024 /
C      DATA IMACH(16) / 1023 /
C
C     MACHINE CONSTANTS FOR THE VAX 11/780
C
C === MACHINE = VAX.11/780
C      DATA IMACH(1) /    5 /
C      DATA IMACH(2) /    6 /
C      DATA IMACH(3) /    5 /
C      DATA IMACH(4) /    6 /
C      DATA IMACH(5) /   32 /
C      DATA IMACH(6) /    4 /
C      DATA IMACH(7) /    2 /
C      DATA IMACH(8) /   31 /
C      DATA IMACH(9) /2147483647 /
C      DATA IMACH(10)/    2 /
C      DATA IMACH(11)/   24 /
C      DATA IMACH(12)/ -127 /
C      DATA IMACH(13)/  127 /
C      DATA IMACH(14)/   56 /
C      DATA IMACH(15)/ -127 /
C      DATA IMACH(16)/  127 /
C
C
C***FIRST EXECUTABLE STATEMENT  I1MACH
      IF (I .LT. 1  .OR.  I .GT. 16)
     1   CALL XERROR ( 'I1MACH -- I OUT OF BOUNDS',25,1,2)
C
      I1MACH=IMACH(I)
      RETURN
C
      END
      FUNCTION J4SAVE(IWHICH,IVALUE,ISET)
C***BEGIN PROLOGUE  J4SAVE
C***REFER TO  XERROR
C     Abstract
C        J4SAVE saves and recalls several global variables needed
C        by the library error handling routines.
C
C     Description of Parameters
C      --Input--
C        IWHICH - Index of item desired.
C                = 1 Refers to current error number.
C                = 2 Refers to current error control flag.
C                 = 3 Refers to current unit number to which error
C                    messages are to be sent.  (0 means use standard.)
C                 = 4 Refers to the maximum number of times any
C                     message is to be printed (as set by XERMAX).
C                 = 5 Refers to the total number of units to which
C                     each error message is to be written.
C                 = 6 Refers to the 2nd unit for error messages
C                 = 7 Refers to the 3rd unit for error messages
C                 = 8 Refers to the 4th unit for error messages
C                 = 9 Refers to the 5th unit for error messages
C        IVALUE - The value to be set for the IWHICH-th parameter,
C                 if ISET is .TRUE. .
C        ISET   - If ISET=.TRUE., the IWHICH-th parameter will BE
C                 given the value, IVALUE.  If ISET=.FALSE., the
C                 IWHICH-th parameter will be unchanged, and IVALUE
C                 is a dummy parameter.
C      --Output--
C        The (old) value of the IWHICH-th parameter will be returned
C        in the function value, J4SAVE.
C
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C    Adapted from Bell Laboratories PORT Library Error Handler
C     Latest revision ---  23 MAY 1979
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  J4SAVE
      LOGICAL ISET
      INTEGER IPARAM(9)
      SAVE IPARAM
      DATA IPARAM(1),IPARAM(2),IPARAM(3),IPARAM(4)/0,2,0,10/
      DATA IPARAM(5)/1/
      DATA IPARAM(6),IPARAM(7),IPARAM(8),IPARAM(9)/0,0,0,0/
C***FIRST EXECUTABLE STATEMENT  J4SAVE
      J4SAVE = IPARAM(IWHICH)
      IF (ISET) IPARAM(IWHICH) = IVALUE
      RETURN
      END
      SUBROUTINE XERABT(MESSG,NMESSG)
C***BEGIN PROLOGUE  XERABT
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  R3C
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Aborts program execution and prints error message.
C***DESCRIPTION
C     Abstract
C        ***Note*** machine dependent routine
C        XERABT aborts the execution of the program.
C        The error message causing the abort is given in the calling
C        sequence, in case one needs it for printing on a dayfile,
C        for example.
C
C     Description of Parameters
C        MESSG and NMESSG are as in XERROR, except that NMESSG may
C        be zero, in which case no message is being supplied.
C
C     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
C     Latest revision ---  19 MAR 1980
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  XERABT
      CHARACTER*(*) MESSG
C***FIRST EXECUTABLE STATEMENT  XERABT
      STOP
      END
      SUBROUTINE XERCTL(MESSG1,NMESSG,NERR,LEVEL,KONTRL)
C***BEGIN PROLOGUE  XERCTL
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  R3C
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Allows user control over handling of individual errors.
C***DESCRIPTION
C     Abstract
C        Allows user control over handling of individual errors.
C        Just after each message is recorded, but before it is
C        processed any further (i.e., before it is printed or
C        a decision to abort is made), a call is made to XERCTL.
C        If the user has provided his own version of XERCTL, he
C        can then override the value of KONTROL used in processing
C        this message by redefining its value.
C        KONTRL may be set to any value from -2 to 2.
C        The meanings for KONTRL are the same as in XSETF, except
C        that the value of KONTRL changes only for this message.
C        If KONTRL is set to a value outside the range from -2 to 2,
C        it will be moved back into that range.
C
C     Description of Parameters
C
C      --Input--
C        MESSG1 - the first word (only) of the error message.
C        NMESSG - same as in the call to XERROR or XERRWV.
C        NERR   - same as in the call to XERROR or XERRWV.
C        LEVEL  - same as in the call to XERROR or XERRWV.
C        KONTRL - the current value of the control flag as set
C                 by a call to XSETF.
C
C      --Output--
C        KONTRL - the new value of KONTRL.  If KONTRL is not
C                 defined, it will remain at its original value.
C                 This changed value of control affects only
C                 the current occurrence of the current message.
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  XERCTL
      CHARACTER*20 MESSG1
C***FIRST EXECUTABLE STATEMENT  XERCTL
      RETURN
      END
      SUBROUTINE XERPRT(MESSG,NMESSG)
C***BEGIN PROLOGUE  XERPRT
C***DATE WRITTEN   790801   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  Z
C***KEYWORDS  ERROR,XERROR PACKAGE
C***AUTHOR  JONES, R. E., (SNLA)
C***PURPOSE  Prints error messages.
C***DESCRIPTION
C     Abstract
C        Print the Hollerith message in MESSG, of length NMESSG,
C        on each file indicated by XGETUA.
C     Latest revision ---  19 MAR 1980
C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-
C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,
C                 1982.
C***ROUTINES CALLED  I1MACH,S88FMT,XGETUA
C***END PROLOGUE  XERPRT
      INTEGER LUN(5)
      CHARACTER*(*) MESSG
C     OBTAIN UNIT NUMBERS AND WRITE LINE TO EACH UNIT
C***FIRST EXECUTABLE STATEMENT  XERPRT
      CALL XGETUA(LUN,NUNIT)
      LENMES = LEN(MESSG)
      DO 20 KUNIT=1,NUNIT
         IUNIT = LUN(KUNIT)
         IF (IUNIT.EQ.0) IUNIT = I1MACH(4)
         DO 10 ICHAR=1,LENMES,72
            LAST = MIN0(ICHAR+71 , LENMES)
            WRITE (IUNIT,'(1X,A)') MESSG(ICHAR:LAST)
   10    CONTINUE
   20 CONTINUE
      RETURN
      END

* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module GAMRAN from package DATAPAC.
* Retrieved from CAMSUN on Fri Jul 10 08:30:32 1998.
* ======================================================================
* GAMRAN
      SUBROUTINE GAMRAN(N,GAMMA,ISEED,X)
C     ******STILL NEEDS ALGORITHM WORK ******
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE GAMMA DISTRIBUTION
C              WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
C              THE PROTOTYPE GAMMA DISTRIBUTION USED
C              HEREIN HAS MEAN = GAMMA
C              AND STANDARD DEVIATION = SQRT(GAMMA).
C              THIS DISTRIBUTION IS DEFINED FOR ALL POSITIVE X,
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C              F(X) = (1/CONSTANT) * (X**(GAMMA-1)) * EXP(-X)
C              WHERE THE CONSTANT = THE GAMMA FUNCTION EVALUATED
C              AT THE VALUE GAMMA.
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --GAMMA  = THE SINGLE PRECISION VALUE OF THE
C                                TAIL LENGTH PARAMETER.
C                                GAMMA SHOULD BE POSITIVE.
C                                GAMMA SHOULD BE LARGER
C                                THAN 1/3 (ALGORITHMIC RESTRICTION).
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE GAMMA DISTRIBUTION
C             WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --GAMMA SHOULD BE POSITIVE.
C                 --GAMMA SHOULD BE LARGER
C                   THAN 1/3 (ALGORITHMIC RESTRICTION).
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, NORRAN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT, EXP.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--GREENWOOD, 'A FAST GENERATOR FOR
C                 GAMMA-DISTRIBUTED RANDOM VARIABLES',
C                 COMPSTAT 1974, PROCEEDINGS IN
C                 COMPUTATIONAL STATISTICS, VIENNA,
C                 SEPTEMBER, 1974, PAGES 19-27.
C               --TOCHER, THE ART OF SIMULATION,
C                 1963, PAGES 24-27.
C               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
C                 1964, PAGES 36-37.
C               --WILK, GNANADESIKAN, AND HUYETT, 'PROBABILITY
C                 PLOTS FOR THE GAMMA DISTRIBUTION',
C                 TECHNOMETRICS, 1962, PAGES 1-15.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 166-206.
C               --HASTINGS AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--A HANDBOOK FOR
C                 STUDENTS AND PRACTITIONERS, 1975,
C                 PAGES 68-73.
C               --NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS
C                 SERIES 55, 1964, PAGE 952.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1975.
C     UPDATED         --FEBRUARY  1976.
C     UPDATED         --JUNE      1978.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
C
C---------------------------------------------------------------------
C
CCCCC CHARACTER*4 IFEEDB
CCCCC CHARACTER*4 IPRINT
C
CCCCC COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
CCCCC COMMON /PRINT/IFEEDB,IPRINT
C
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA ATHIRD/0.3333333/
      DATA SQRT3 /1.73205081/
C
      IPR=6
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)GOTO50
      IF(GAMMA.LE.0.0)GOTO60
      IF(GAMMA.LE.0.33333333)GOTO65
      GOTO90
   50 WRITE(IPR, 5)
      WRITE(IPR,47)N
      RETURN
   60 WRITE(IPR,15)
      WRITE(IPR,46)GAMMA
      RETURN
   65 WRITE(IPR,16)
      WRITE(IPR,17)
      WRITE(IPR,46)GAMMA
      RETURN
   90 CONTINUE
    5 FORMAT(1H , 91H***** FATAL ERROR--THE FIRST  INPUT ARGUMENT TO THE
     1 GAMRAN SUBROUTINE IS NON-POSITIVE *****)
   15 FORMAT(1H , 91H***** FATAL ERROR--THE SECOND INPUT ARGUMENT TO THE
     1 GAMRAN SUBROUTINE IS NON-POSITIVE *****)
   16 FORMAT(1H ,114H***** FATAL ERROR--THE SECOND INPUT ARGUMENT TO THE
     1 GAMRAN SUBROUTINE IS SMALLER THAN OR EQUAL TO 0.33333333 *****)
   17 FORMAT(1H , 44H                   (ALGORITHMIC RESTIRCTION))
   46 FORMAT(1H , 35H***** THE VALUE OF THE ARGUMENT IS ,E15.8,6H *****)
   47 FORMAT(1H , 35H***** THE VALUE OF THE ARGUMENT IS ,I8   ,6H *****)
C
C     GENERATE N GAMMA DISTRIBUTION RANDOM NUMBERS
C     USING GREENWOOD'S REJECTION ALGORITHM--
C     1) GENERATE A NORMAL RANDOM NUMBER;
C     2) TRANSFORM THE NORMAL VARIATE TO AN APPROXIMATE
C        GAMMA VARIATE USING THE WILSON-HILFERTY
C        APPROXIMATION (SEE THE JOHNSON AND KOTZ
C        REFERENCE, PAGE 176);
C     3) FORM THE REJECTION FUNCTION VALUE, BASED
C        ON THE PROBABILITY DENSITY FUNCTION VALUE
C        OF THE ACTUAL DISTRIBUTION OF THE PSEUDO-GAMMA
C        VARIATE, AND THE PROBABILITY DENSITY FUNCTION VALUE
C        OF A TRUE GAMMA VARIATE.
C     4) GENERATE A UNIFORM RANDOM NUMBER;
C     5) IF THE UNIFORM RANDOM NUMBER IS LESS THAN
C        THE REJECTION FUNCTION VALUE, THEN ACCEPT
C        THE PSEUDO-RANDOM NUMBER AS A GAMMA VARIATE;
C        IF THE UNIFORM RANDOM NUMBER IS LARGER THAN
C        THE REJECTION FUNCTION VALUE, THEN REJECT
C        THE PSEUDO-RANDOM NUMBER AS A GAMMA VARIATE.
C
      A1=1.0/(9.0*GAMMA)
      B1=SQRT(A1)
      XN0=-SQRT3+B1
      XG0=GAMMA*(1.0-A1+B1*XN0)**3
      DO100I=1,N
  150 CALL NORRAN(1,ISEED,XN)
      XG=GAMMA*(1.0-A1+B1*XN)**3
      IF(XG.LT.0.0)GOTO150
      TERM=(XG/XG0)**(GAMMA-ATHIRD)
      ARG=0.5*XN*XN-XG-0.5*XN0*XN0+XG0
      FUNCT=TERM*EXP(ARG)
      CALL UNIRAN(1,ISEED,U)
      IF(U.LE.FUNCT)GOTO170
      GOTO150
  170 X(I)=XG
  100 CONTINUE
C
      RETURN
      END
* NORRAN
      SUBROUTINE NORRAN(N,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE THE NORMAL (GAUSSIAN)
C              DISTRIBUTION WITH MEAN = 0 AND STANDARD DEVIATION = 1.
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = (1/SQRT(2*PI))*EXP(-X*X/2).
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE NORMAL DISTRIBUTION
C             WITH MEAN = 0 AND STANDARD DEVIATION = 1.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--ALOG, SQRT, SIN, COS.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     METHOD--BOX-MULLER ALGORITHM.
C     REFERENCES--BOX AND MULLER, 'A NOTE ON THE GENERATION
C                 OF RANDOM NORMAL DEVIATES', JOURNAL OF THE
C                 ASSOCIATION FOR COMPUTING MACHINERY, 1958,
C                 PAGES 610-611.
C               --TOCHER, THE ART OF SIMULATION,
C                 1963, PAGES 33-34.
C               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
C                 1964, PAGE 39.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 1970, PAGES 40-111.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--JUNE      1972.
C     UPDATED         --SEPTEMBER 1975.
C     UPDATED         --NOVEMBER  1975.
C     UPDATED         --JULY      1976.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(2)
C
C---------------------------------------------------------------------
C
CCCCC CHARACTER*4 IFEEDB
CCCCC CHARACTER*4 IPRINT
C
CCCCC COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
CCCCC COMMON /PRINT/IFEEDB,IPRINT
C
      IPR=6
C
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA PI/3.14159265359/
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)GOTO50
      GOTO90
   50 WRITE(IPR, 5)
      WRITE(IPR,47)N
      RETURN
   90 CONTINUE
    5 FORMAT(1H , 91H***** FATAL ERROR--THE FIRST  INPUT ARGUMENT TO THE
     1 NORRAN SUBROUTINE IS NON-POSITIVE *****)
   47 FORMAT(1H , 35H***** THE VALUE OF THE ARGUMENT IS ,I8   ,6H *****)
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C     THEN GENERATE 2 ADDITIONAL UNIFORM (0,1) RANDOM NUMBERS
C     (TO BE USED BELOW IN FORMING THE N-TH NORMAL
C     RANDOM NUMBER WHEN THE DESIRED SAMPLE SIZE N
C     HAPPENS TO BE ODD).
C
      CALL UNIRAN(N,ISEED,X)
      CALL UNIRAN(2,ISEED,Y)
C
C     GENERATE N NORMAL RANDOM NUMBERS
C     USING THE BOX-MULLER METHOD.
C
      DO200I=1,N,2
      IP1=I+1
      U1=X(I)
      IF(I.EQ.N)GOTO210
      U2=X(IP1)
      GOTO220
  210 U2=Y(2)
  220 ARG1=-2.0*ALOG(U1)
      ARG2=2.0*PI*U2
      SQRT1=SQRT(ARG1)
      Z1=SQRT1*COS(ARG2)
      Z2=SQRT1*SIN(ARG2)
      X(I)=Z1
      IF(I.EQ.N)GOTO200
      X(IP1)=Z2
  200 CONTINUE
C
      RETURN
      END
* UNIRAN
      SUBROUTINE UNIRAN(N,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE UNIFORM (RECTANGULAR)
C              DISTRIBUTION ON THE UNIT INTERVAL (0,1).
C              THIS DISTRIBUTION HAS MEAN = 0.5
C              AND STANDARD DEVIATION = SQRT(1/12) = 0.28867513.
C              THIS DISTRIBUTION HAS THE PROBABILITY
C              DENSITY FUNCTION F(X) = 1.
C
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --ISEED  = AN INTEGER ISEED VALUE
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE RECTANGULAR DISTRIBUTION ON (0,1).
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER           SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C
C     ALGORITHM--FIBONACCI GENERATOR
C                AS DEFINED BY GEORGE MARSAGLIA.
C
C     NOTE--THIS GENERATOR IS TRANSPORTABLE.
C           IT IS NOT MACHINE-INDEPENDENT
C           IN THE SENSE THAT FOR A GIVEN VALUE
C           OF THE INPUT SEED ISEED AND FOR A GIVEN VALUE
C           OF MDIG (TO BE DEFINED BELOW),
C           THE SAME SEQUENCE OF UNIRFORM RANDOM
C           NUMBERS WILL RESULT ON DIFFERENT COMPUTERS
C           (VAX, PRIME, PERKIN-ELMER, IBM, UNIVAC, HONEYWELL, ETC.)
C
C     NOTE--IF MDIG = 32 AND IF ISEED = 305,
C           THEN THE OUTPUT FROM THIS GENERATOR SHOULD BE AS FOLLOWS--
C           THE FIRST      NUMBER TO RESULT IS .4771580...
C           THE SECOND     NUMBER TO RESULT IS .4219293...
C           THE THIRD      NUMBER TO RESULT IS .6646181...
C           ...
C           THE THOUSANDTH NUMBER TO RESULT IS .2036834...
C
C     NOTE--IF MDIG = 16 AND IF ISEED = 305,
C           THEN THE OUTPUT FROM THIS GENERATOR SHOULD BE AS FOLLOWS--
C           THE FIRST      NUMBER TO RESULT IS .027832881...
C           THE SECOND     NUMBER TO RESULT IS .56102176...
C           THE THIRD      NUMBER TO RESULT IS .41456343...
C           ...
C           THE THOUSANDTH NUMBER TO RESULT IS .19797357...
C
C     NOTE--IT IS RECOMMENDED THAT UPON
C           IMPLEMENTATION OF DATAPLOT, THE OUTPUT
C           FROM UNIRAN BE CHECKED FOR AGREEMENT
C           WITH THE ABOVE SAMPLE OUTPUT.
C           ALSO, THERE ARE MANY ANALYSIS AND DIAGNOSTIC
C           TOOLS IN DATAPLOT THAT WILL ALLOW THE
C           TESTING OF THE RANDOMNESS AND UNIFORMITY
C           OF THIS GENERATOR.
C           SUCH CHECKING IS ESPECIALLY IMPORTANT
C           IN LIGHT OF THE FACT THAT OTHER DATAPLOT RANDOM
C           NUMBER GENERATOR SUBROUTINES (NORRAN--NORMAL,
C           LOGRAN--LOGISTIC, ETC.) ALL MAKE USE OF INTERMEDIATE
C           OUTPUT FROM UNIRAN.
C
C     NOTE--THE OUTPUT FROM THIS SUBROUTINE DEPENDS
C           ON THE INPUT SEED (ISEED) AND ON THE
C           VALUE OF MDIG.
C           MDIG MAY NOT BE SMALLER THAN 16.
C           MDIG MAY NOT BE LARGER THAN MAX INTEGER ON YOUR COMPUTER.
C
C     NOTE--BECAUSE OF THE PREPONDERANCE OF MAINFRAMES
C           WHICH HAVE WORDS OF 32 BITS AND LARGER
C           (E.G, VAX (= 32 BITS), UNIVAC (= 36 BITS), CDC (= 60 BITS), ETC.)
C           MDIG HAS BEEN SET TO 32.
C           THUS THE SAME SEQUENCE OF RANDOM NUMBERS SHOULD RESULT
C           ON ALL OF THESE COMPUTERS.
C
C     NOTE--FOR SMALLER WORD SIZE COMPUTERS (E.G., 24-BIT AND 16-BIT),
C           THE VALUE OF MDIG SHOULD BE CHANGED TO 24 OR 16.
C           IN SUCH CASE, THE OUTPUT WILL NOT BE IDENTICAL TO
C           THE OUTPUT WHEN MDIG = 32.
C
C     NOTE--THE CYCLE OF THE RANDOM NUMBERS DEPENDS ON MDIG.
C           THE CYCLE FROM MDIG = 32 IS LONG ENOUGH FOR MOST
C           PRACTICAL APPLICATIONS.
C           IF A LONGER CYCLE IS DESIRED, THEN INCREASE MDIG.
C
C     NOTE--THE SEED MAY BE ANY POSITIVE INTEGER.
C           NO APPRECIABLE DIFFERENCE IN THE QUALITY
C           OF THE RANDOM NUMBERS HAS BEEN NOTED
C           BY THE CHOICE OF THE SEED.  THERE IS NO
C           NEED TO USE PRIMES, NOR TO USE EXCEPTIONALLY
C           LARGE NUMBERS, ETC.
C
C     REFERENCES--MARSAGLIA G., "COMMENTS ON THE PERFECT UNIFORM RANDOM
C                 NUMBER GENERATOR", UNPUBLISHED NOTES, WASH S. U.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 57-74.
C     WRITTEN BY--JAMES BLUE
C                 SCIENTIFIC COMPUTING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C               --DAVID KAHANER
C                 SCIENTIFIC COMPUTING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C               --GEORGE MARSAGLIA
C                 COMPUTER SCIENCE DEPARTMENT
C                 WASHINGTON STATE UNIVERSITY
C               --JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--JUNE      1972.
C     UPDATED         --AUGUST    1974.
C     UPDATED         --SEPTEMBER 1975.
C     UPDATED         --NOVEMBER  1975.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1984.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
C
      DIMENSION M(17)
C
C---------------------------------------------------------------------
C
CCCCC CHARACTER*4 IFEEDB
CCCCC CHARACTER*4 IPRINT
C
CCCCC COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
CCCCC COMMON /PRINT/IFEEDB,IPRINT
C
C-----SAVE STATEMENTS-------------------------------------------------
C
      SAVE I,J,M,M1,M2
C
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA M(1),M(2),M(3),M(4),M(5),M(6),M(7),M(8),M(9),M(10),M(11),
     1     M(12),M(13),M(14),M(15),M(16),M(17)
     1/    30788,23052,2053,19346,10646,19427,23975,
     1     19049,10949,19693,29746,26748,2796,23890,
     1     29168,31924,16499/
      DATA M1,M2,I,J / 32767,256,5,17 /
C
      IPR=6
C
C-----START POINT-----------------------------------------------------
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.GE.1)GOTO90
      WRITE(IPR,999)
  999 FORMAT(1H )
      WRITE(IPR,51)
   51 FORMAT(1H ,'***** ERROR IN UNIRAN--')
      WRITE(IPR,52)
   52 FORMAT(1H ,'      THE INPUT NUMBER OF OBSERVATIONS IS ',
     1'NON-POSITIVE.')
      WRITE(IPR,53)N
   53 FORMAT(1H ,'      N = ',I8)
      GOTO9000
   90 CONTINUE
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  IF A POSITIVE INPUT SEED HAS BEEN GIVEN,         **
C               **  THEN THIS INDICATES THAT THE GENERATOR           **
C               **  SHOULD HAVE ITS INTERNAL M(.) ARRAY REDEFINED--  **
C               **  DO SO IN THIS SECTION.                           **
C               **  IF A NON-POSITIVE INPUT SEED HAS BEEN GIVEN,     **
C               **  THEN THIS INDICATES THAT THE GENERATOR           **
C               **  SHOULD CONTINUE ON FROM WHERE IT LEFT OFF,       **
C               **  AND THEREFORE THIS SECTION IS SKIPPED.           **
C               *******************************************************
C
      IF(ISEED.LE.0)GOTO290
C
CCCCC MDIG=16
      MDIG=32
C
      M1=2**(MDIG-2)+(2**(MDIG-2)-1)
      M2=2**(MDIG/2)
CCCCC ISEED3=MIN0(IABS(ISEED),M1)
      ISEED3=IABS(ISEED)
      IF(M1.LT.IABS(ISEED))ISEED3=M1
      IF(MOD(ISEED3,2).EQ.0)ISEED3=ISEED3-1
      K0=MOD(9069,M2)
      K1=9069/M2
      J0=MOD(ISEED3,M2)
      J1=ISEED3/M2
C
      DO200I=1,17
      ISEED3=J0*K0
      J1=MOD(ISEED3/M2+J0*K1+J1*K0,M2/2)
      J0=MOD(ISEED3,M2)
      M(I)=J0+M2*J1
  200 CONTINUE
C
      I=5
      J=17
C
  290 CONTINUE
C
C               *************************************
C               **  STEP 3--                       **
C               **  GENERATE THE N RANDOM NUMBERS  **
C               *************************************
C
      DO300L=1,N
      K=M(I)-M(J)
      IF(K.LT.0)K=K+M1
      M(J)=K
      I=I-1
      IF(I.EQ.0)I=17
      J=J-1
      IF(J.EQ.0)J=17
      AK=K
      AM1=M1
      X(L)=AK/AM1
  300 CONTINUE
C
C               *****************************************************
C               **  STEP 4--                                       **
C               **  REGARDLESS OF THE VALUE OF THE INPUT SEED,     **
C               **  REDEFINE THE VALUE OF ISEED UPON EXIT HERE     **
C               **  TO -1 WITH THE NET EFFECT THAT                 **
C               **  IF THE USER DOES NOT REDEFINE THE SEED         **
C               **  VALUE BEFORE THE NEXT CALL TO THIS GENERATOR,  **
C               **  THEN THIS GENERATOR WILL PICK UP               **
C               **  WHERE IT LEFT OFF.                             **
C               *****************************************************
C
      ISEED=(-1)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      RETURN
CCCCC DEBUG TRACE,INIT
CCCCC AT 90
CCCCC TRACE ON
      END

      DOUBLE PRECISION FUNCTION DFAC(N)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE COMBINATORIAL CHOOSE FUNCTION
C
C                 / N \
C           \ K /
C
C     N.B.  N AND K MUST BE INTEGERS;  CHOOSE IS DOUBLE PRECISION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 21, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

        INTEGER N,ISET
cprh        COMMON/ZZZ888/ FACT(0:170),ISET
        DOUBLE PRECISION FACT(0:170)
        DATA ISET/0/,FACT(0)/1/
      
        IF(ISET .EQ. 0) THEN
        DO 10 I=1,170
          FACT(I) = I*FACT(I-1)
10      CONTINUE
	  ISET=2
        ENDIF

        DFAC = FACT(N)
        RETURN
        END
         
        SUBROUTINE MGAMINV(B,PROB,XT,IER)
C======================================================================
C
C       FUNCTION TO COMPUTE THE INVERSE CDF OF THE INCOMPLETE
C       GAMMA FUNCTION
C
C       AUTHOR.......TIM COHN
C       DATE.........SEPT. 2, 1987
C         MODIFIED...OCTOBER 26, 1989
C         MODIFIED...23 JAN 2001 (TAC) -- CODE RE-ARRANGED TO ELIMINATE
C                      REPORTED WARNING DURING COMPILATION WITH G77
C         MODIFIED...5 NOV 2003 (TAC) -- INCREASED NUMBER OF TERMS
C
C       N.B.  CALLS IMSL ROUTINES 
C
C======================================================================

        IMPLICIT DOUBLE PRECISION (A-H,M-Z)

        DIMENSION X(1000),F(1000)

        DATA TOL/1.D-9/,DELTA/1.D-5/,ZM/1.D-9/

        IF((PROB .GT. 1.D0-ZM) .OR. (PROB .LT. ZM) ) GOTO 99

C======================================================================
C
C      USE BOBEE TO GET INITIAL GUESS
C
        XN    =  DNORIN(PROB)
        RTI   = (XN/(3.*B**(1./6.)) - 1./(9.*B**(2./3.)) + B**(1./3.))
        RT    =  RTI**3
        X(1)  =  MAX(DELTA,RT + (RT+1.D0)*DELTA)

          PROBT = DGAMDF(X(1),B)
          F(1)  =  PROBT-PROB
          X(2)  =  MAX(DELTA/10.D0,RT)
        DO 10 I=2,999
          PROBT = DGAMDF(X(I),B)
          F(I)  =  PROBT-PROB
c            write(99,*) 'mg ',i, x(i),f(i)
            IF(ABS(F(I)) .LE. TOL * PROB) THEN
               XT  = X(I)
               IER = 0
               RETURN
            ENDIF
          X(I+1) = X(I)-F(I)*(X(I)-X(I-1))/(F(I)-F(I-1))
	  X(I+1) = MAX(X(I+1),X(I)/10.D0)
   10   CONTINUE

   99   CONTINUE
          XT   =  PROB*1.D99
          WRITE(99,*) 'ERROR IN MGAMINV: P = ',PROB
          IER  =  129
        RETURN
       END
C***********************************************************
C
C     THIS FILE WAS TAKEN FROM NUMERICAL RECIPES AND
C     MODIFIED BY TIM COHN, AUGUST 31, 1998, 
C     FOR DOUBLE PRECISION
C
      DOUBLE PRECISION FUNCTION GAMMP(A,X)
	  IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF(X.LT.0.) THEN
         WRITE(6,*) 'GAMMP: X < 0'
         GAMMP = 0.D0
      ELSE IF (A.LE.0.) THEN
         WRITE(6,*) 'GAMMP: A < 0'
         GAMMP = 1.D0      
      ELSE IF(X.LT.A+1.)THEN
        CALL GSER(GAMSER,A,X,GLN)
        GAMMP=GAMSER
      ELSE
        CALL GCF(GAMMCF,A,X,GLN)
        GAMMP=1.-GAMMCF
      ENDIF
      RETURN
      END
c
      SUBROUTINE GCF(GAMMCF,A,X,GLN)
	  IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ITMAX=10000,EPS=3.E-12)
      GLN=GAMMLN(A)
      GOLD=0.
      A0=1.
      A1=X
      B0=0.
      B1=1.
      FAC=1.
      DO 11 N=1,ITMAX
        AN=N
        ANA=AN-A
        A0=(A1+A0*ANA)*FAC
        B0=(B1+B0*ANA)*FAC
        ANF=AN*FAC
        A1=X*A0+ANF*A1
        B1=X*B0+ANF*B1
        IF(A1.NE.0.)THEN
          FAC=1./A1
          G=B1*FAC
          IF(ABS((G-GOLD)/G).LT.EPS)GO TO 1
          GOLD=G
        ENDIF
11    CONTINUE
      WRITE(6,*) 'GCF: A,X,ITMAX',A,X,ITMAX
      WRITE(6,*) 'A too large, ITMAX too small'
1     GAMMCF=EXP(-X+A*LOG(X)-GLN)*G
      RETURN
      END
C
      SUBROUTINE GSER(GAMSER,A,X,GLN)
	  IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (ITMAX=10000,EPS=3.E-12)
      GLN=GAMMLN(A)
      IF(X.LE.0.)THEN
        IF(X.LT.0.)PAUSE 'GSER: X < 0'
        GAMSER=0.
        RETURN
      ENDIF
      AP=A
      SUM=1./A
      DEL=SUM
      DO 11 N=1,ITMAX
        AP=AP+1.
        DEL=DEL*X/AP
        SUM=SUM+DEL
        IF(ABS(DEL).LT.ABS(SUM)*EPS)GO TO 1
11    CONTINUE
      WRITE(6,*) 'GCF: A,X,ITMAX',A,X,ITMAX
      PAUSE 'A too large, ITMAX too small'
1     GAMSER=SUM*EXP(-X+A*LOG(X)-GLN)
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION GAMMLN(XX)
	  IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*8 COF(6),STP,HALF,ONE,FPF,X,TMP,SER
      DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0,
     *    -1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
      DATA HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/
      X=XX-ONE
      TMP=X+FPF
      TMP=(X+HALF)*LOG(TMP)-TMP
      SER=ONE
      DO 11 J=1,6
        X=X+ONE
        SER=SER+COF(J)/X
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER)
      RETURN
      END
C***********************************************************
* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module DPSI from package CMLIB.
* Retrieved from CAMSUN on Fri Apr  9 09:50:53 1999.
* ======================================================================
      DOUBLE PRECISION FUNCTION DPSI(X)
C***BEGIN PROLOGUE  DPSI
C***DATE WRITTEN   770601   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C7C
C***KEYWORDS  DIGAMMA FUNCTION,DOUBLE PRECISION,PSI FUNCTION,
C             SPECIAL FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Computes the d.p. Psi (or Digamma) function.
C***DESCRIPTION
C
C DPSI calculates the double precision psi (or digamma) function for
C double precision argument X.  PSI(X) is the logarithmic derivative
C of the gamma function of X.
C
C Series for PSI        on the interval  0.          to  1.00000E+00
C                                        with weighted error   5.79E-32
C                                         log weighted error  31.24
C                               significant figures required  30.93
C                                    decimal places required  32.05
C
C
C Series for APSI       on the interval  0.          to  1.00000E-02
C                                        with weighted error   7.75E-33
C                                         log weighted error  32.11
C                               significant figures required  28.88
C                                    decimal places required  32.71
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH,DCOT,DCSEVL,DINT,INITDS,XERROR
C***END PROLOGUE  DPSI
      DOUBLE PRECISION X, PSICS(42), APSICS(16), AUX, DXREL, PI, XBIG,
     1  Y,  DINT, DCOT, DCSEVL, D1MACH
      DATA PSI CS(  1) / -.3805708083 5217921520 4376776670 39 D-1     /
      DATA PSI CS(  2) / +.4914153930 2938712748 2046996542 77 D+0     /
      DATA PSI CS(  3) / -.5681574782 1244730242 8920647340 81 D-1     /
      DATA PSI CS(  4) / +.8357821225 9143131362 7756507478 62 D-2     /
      DATA PSI CS(  5) / -.1333232857 9943425998 0792741723 93 D-2     /
      DATA PSI CS(  6) / +.2203132870 6930824892 8723979795 21 D-3     /
      DATA PSI CS(  7) / -.3704023817 8456883592 8890869492 29 D-4     /
      DATA PSI CS(  8) / +.6283793654 8549898933 6514187176 90 D-5     /
      DATA PSI CS(  9) / -.1071263908 5061849855 2835417470 74 D-5     /
      DATA PSI CS( 10) / +.1831283946 5484165805 7315898103 78 D-6     /
      DATA PSI CS( 11) / -.3135350936 1808509869 0057797968 85 D-7     /
      DATA PSI CS( 12) / +.5372808776 2007766260 4719191436 15 D-8     /
      DATA PSI CS( 13) / -.9211681415 9784275717 8806326247 30 D-9     /
      DATA PSI CS( 14) / +.1579812652 1481822782 2528840328 23 D-9     /
      DATA PSI CS( 15) / -.2709864613 2380443065 4405894097 07 D-10    /
      DATA PSI CS( 16) / +.4648722859 9096834872 9473195295 49 D-11    /
      DATA PSI CS( 17) / -.7975272563 8303689726 5047977727 37 D-12    /
      DATA PSI CS( 18) / +.1368272385 7476992249 2510538928 38 D-12    /
      DATA PSI CS( 19) / -.2347515606 0658972717 3206779807 19 D-13    /
      DATA PSI CS( 20) / +.4027630715 5603541107 9079250062 81 D-14    /
      DATA PSI CS( 21) / -.6910251853 1179037846 5474229747 71 D-15    /
      DATA PSI CS( 22) / +.1185604713 8863349552 9291395257 68 D-15    /
      DATA PSI CS( 23) / -.2034168961 6261559308 1542104842 23 D-16    /
      DATA PSI CS( 24) / +.3490074968 6463043850 3742329323 51 D-17    /
      DATA PSI CS( 25) / -.5988014693 4976711003 0110813934 93 D-18    /
      DATA PSI CS( 26) / +.1027380162 8080588258 3980057122 13 D-18    /
      DATA PSI CS( 27) / -.1762704942 4561071368 3592601053 86 D-19    /
      DATA PSI CS( 28) / +.3024322801 8156920457 4540354901 33 D-20    /
      DATA PSI CS( 29) / -.5188916830 2092313774 2860888746 66 D-21    /
      DATA PSI CS( 30) / +.8902773034 5845713905 0058874879 99 D-22    /
      DATA PSI CS( 31) / -.1527474289 9426728392 8949719040 00 D-22    /
      DATA PSI CS( 32) / +.2620731479 8962083136 3583180799 99 D-23    /
      DATA PSI CS( 33) / -.4496464273 8220696772 5983880533 33 D-24    /
      DATA PSI CS( 34) / +.7714712959 6345107028 9193642666 66 D-25    /
      DATA PSI CS( 35) / -.1323635476 1887702968 1026389333 33 D-25    /
      DATA PSI CS( 36) / +.2270999436 2408300091 2773119999 99 D-26    /
      DATA PSI CS( 37) / -.3896419021 5374115954 4913919999 99 D-27    /
      DATA PSI CS( 38) / +.6685198138 8855302310 6798933333 33 D-28    /
      DATA PSI CS( 39) / -.1146998665 4920864872 5299199999 99 D-28    /
      DATA PSI CS( 40) / +.1967938588 6541405920 5154133333 33 D-29    /
      DATA PSI CS( 41) / -.3376448818 9750979801 9072000000 00 D-30    /
      DATA PSI CS( 42) / +.5793070319 3214159246 6773333333 33 D-31    /
      DATA APSICS(  1) / -.8327107910 6929076017 4456932269 D-3        /
      DATA APSICS(  2) / -.4162518421 9273935282 1627121990 D-3        /
      DATA APSICS(  3) / +.1034315609 7874129117 4463193961 D-6        /
      DATA APSICS(  4) / -.1214681841 3590415298 7299556365 D-9        /
      DATA APSICS(  5) / +.3113694319 9835615552 1240278178 D-12       /
      DATA APSICS(  6) / -.1364613371 9317704177 6516100945 D-14       /
      DATA APSICS(  7) / +.9020517513 1541656513 0837974000 D-17       /
      DATA APSICS(  8) / -.8315429974 2159146482 9933635466 D-19       /
      DATA APSICS(  9) / +.1012242570 7390725418 8479482666 D-20       /
      DATA APSICS( 10) / -.1562702494 3562250762 0478933333 D-22       /
      DATA APSICS( 11) / +.2965427168 0890389613 3226666666 D-24       /
      DATA APSICS( 12) / -.6746868867 6570216374 1866666666 D-26       /
      DATA APSICS( 13) / +.1803453116 9718990421 3333333333 D-27       /
      DATA APSICS( 14) / -.5569016182 4598360746 6666666666 D-29       /
      DATA APSICS( 15) / +.1958679226 0773625173 3333333333 D-30       /
      DATA APSICS( 16) / -.7751958925 2333568000 0000000000 D-32       /
      DATA PI / 3.1415926535 8979323846 2643383279 50 D0 /
      DATA NTPSI, NTAPSI, XBIG, DXREL / 2*0, 2*0.D0 /
C***FIRST EXECUTABLE STATEMENT  DPSI
      IF (NTPSI.NE.0) GO TO 10
      NTPSI = INITDS (PSICS, 42, 0.1*SNGL(D1MACH(3)) )
      NTAPSI = INITDS (APSICS, 16, 0.1*SNGL(D1MACH(3)) )
C
      XBIG = 1.0D0/DSQRT(D1MACH(3))
      DXREL = DSQRT (D1MACH(4))
C
 10   Y = DABS(X)
C
      IF (Y.GT.10.0D0) GO TO 50
C
C DPSI(X) FOR DABS(X) .LE. 2
C
      N = X
      IF (X.LT.0.D0) N = N - 1
      Y = X - DBLE(FLOAT(N))
      N = N - 1
      DPSI = DCSEVL (2.D0*Y-1.D0, PSICS, NTPSI)
      IF (N.EQ.0) RETURN
C
      IF (N.GT.0) GO TO 30
C
      N = -N
      IF (X.EQ.0.D0) CALL XERROR ( 'DPSI    X IS 0', 14, 2, 2)
      IF (X.LT.0.D0 .AND. X+DBLE(FLOAT(N-2)).EQ.0.D0) CALL XERROR ( 'DPS
     1I    X IS A NEGATIVE INTEGER', 31, 3, 2)
      IF (X.LT.(-0.5D0) .AND. DABS((X-DINT(X-0.5D0))/X).LT.DXREL) CALL
     1  XERROR ( 'DPSI    ANSWER LT HALF PRECISION BECAUSE X TOO NEAR NE
     2GATIVE INTEGER', 68, 1, 1)
C
      DO 20 I=1,N
        DPSI = DPSI - 1.D0/(X+DBLE(FLOAT(I-1)))
 20   CONTINUE
      RETURN
C
C DPSI(X) FOR X .GE. 2.0 AND X .LE. 10.0
C
 30   DO 40 I=1,N
        DPSI = DPSI + 1.0D0/(Y+DBLE(FLOAT(I)))
 40   CONTINUE
      RETURN
C
C DPSI(X) FOR DABS(X) .GT. 10.0
C
 50   AUX = 0.D0
      IF (Y.LT.XBIG) AUX = DCSEVL (2.D0*(10.D0/Y)**2-1.D0, APSICS,
     1  NTAPSI)
C
      IF (X.LT.0.D0) DPSI = DLOG(DABS(X)) - 0.5D0/X + AUX
     1  - PI*DCOT(PI*X)
      IF (X.GT.0.D0) DPSI = DLOG(X) - 0.5D0/X + AUX
      RETURN
C
      END
      DOUBLE PRECISION FUNCTION DCOT(X)
C***BEGIN PROLOGUE  DCOT
C***DATE WRITTEN   770601   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C4A
C***KEYWORDS  COTANGENT,DEGREE,DOUBLE PRECISION,ELEMENTARY FUNCTION,
C             TRIGONOMETRIC COSINE
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Computes the d.p. Cotangent.
C***DESCRIPTION
C
C DCOT(X) calculates the double precision trigonometric cotangent
C for double precision argument X.  X is in units of radians.
C
C Series for COT        on the interval  0.          to  6.25000E-02
C                                        with weighted error   5.52E-34
C                                         log weighted error  33.26
C                               significant figures required  32.34
C                                    decimal places required  33.85
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH,DCSEVL,DINT,INITDS,XERROR
C***END PROLOGUE  DCOT
      DOUBLE PRECISION X, COTCS(15), AINTY, AINTY2, PI2REC, SQEPS,
     1  XMAX, XMIN, XSML, Y, YREM, PRODBG,  DINT, DCSEVL, D1MACH
      DATA COT CS(  1) / +.2402591609 8295630250 9553617744 970 D+0    /
      DATA COT CS(  2) / -.1653303160 1500227845 4746025255 758 D-1    /
      DATA COT CS(  3) / -.4299839193 1724018935 6476228239 895 D-4    /
      DATA COT CS(  4) / -.1592832233 2754104602 3490851122 445 D-6    /
      DATA COT CS(  5) / -.6191093135 1293487258 8620579343 187 D-9    /
      DATA COT CS(  6) / -.2430197415 0726460433 1702590579 575 D-11   /
      DATA COT CS(  7) / -.9560936758 8000809842 7062083100 000 D-14   /
      DATA COT CS(  8) / -.3763537981 9458058041 6291539706 666 D-16   /
      DATA COT CS(  9) / -.1481665746 4674657885 2176794666 666 D-18   /
      DATA COT CS( 10) / -.5833356589 0366657947 7984000000 000 D-21   /
      DATA COT CS( 11) / -.2296626469 6464577392 8533333333 333 D-23   /
      DATA COT CS( 12) / -.9041970573 0748332671 9999999999 999 D-26   /
      DATA COT CS( 13) / -.3559885519 2060006400 0000000000 000 D-28   /
      DATA COT CS( 14) / -.1401551398 2429866666 6666666666 666 D-30   /
      DATA COT CS( 15) / -.5518004368 7253333333 3333333333 333 D-33   /
      DATA PI2REC / .01161977236 7581343075 5350534900 57 D0 /
      DATA NTERMS, XMAX, XSML, XMIN, SQEPS /0, 4*0.D0 /
C***FIRST EXECUTABLE STATEMENT  DCOT
      IF (NTERMS.NE.0) GO TO 10
      NTERMS = INITDS (COTCS, 15, 0.1*SNGL(D1MACH(3)) )
      XMAX = 1.0D0/D1MACH(4)
      XSML = DSQRT (3.0D0*D1MACH(3))
      XMIN = DEXP (DMAX1(DLOG(D1MACH(1)), -DLOG(D1MACH(2))) + 0.01D0)
      SQEPS = DSQRT (D1MACH(4))
C
 10   Y = DABS(X)
      IF (Y.LT.XMIN) CALL XERROR (  'DCOT    DABS(X) IS ZERO OR SO SMALL
     1 DCOT OVERFLOWS', 50, 2, 2)
      IF (Y.GT.XMAX) CALL XERROR ( 'DCOT    NO PRECISION BECAUSE DABS(X)
     1 IS BIG', 43, 3, 2)
C
C CAREFULLY COMPUTE Y * (2/PI) = (AINT(Y) + REM(Y)) * (.625 + PI2REC)
C = AINT(.625*Y) + REM(.625*Y) + Y*PI2REC  =  AINT(.625*Y) + Z
C = AINT(.625*Y) + AINT(Z) + REM(Z)
C
      AINTY = DINT (Y)
      YREM = Y - AINTY
      PRODBG = 0.625D0*AINTY
      AINTY = DINT (PRODBG)
      Y = (PRODBG-AINTY) + 0.625D0*YREM + PI2REC*Y
      AINTY2 = DINT (Y)
      AINTY = AINTY + AINTY2
      Y = Y - AINTY2
C
      IFN = DMOD (AINTY, 2.0D0)
      IF (IFN.EQ.1) Y = 1.0D0 - Y
C
      IF (DABS(X).GT.0.5D0 .AND. Y.LT.DABS(X)*SQEPS) CALL XERROR ( 'DCOT
     1    ANSWER LT HALF PRECISION, ABS(X) TOO BIG OR X NEAR N*PI (N.NE.
     20)', 72, 1, 1)
C
      IF (Y.GT.0.25D0) GO TO 20
      DCOT = 1.0D0/X
      IF (Y.GT.XSML) DCOT = (0.5D0 + DCSEVL (32.0D0*Y*Y-1.D0, COTCS,
     1  NTERMS)) / Y
      GO TO 40
C
 20   IF (Y.GT.0.5D0) GO TO 30
      DCOT = (0.5D0 + DCSEVL (8.D0*Y*Y-1.D0, COTCS, NTERMS))/(0.5D0*Y)
      DCOT = (DCOT*DCOT-1.D0)*0.5D0/DCOT
      GO TO 40
C
 30   DCOT = (0.5D0 + DCSEVL (2.D0*Y*Y-1.D0, COTCS, NTERMS))/(.25D0*Y)
      DCOT = (DCOT*DCOT-1.D0)*0.5D0/DCOT
      DCOT = (DCOT*DCOT-1.D0)*0.5D0/DCOT
C
 40   IF (X.NE.0.D0) DCOT = DSIGN (DCOT, X)
      IF (IFN.EQ.1) DCOT = -DCOT
C
      RETURN
      END

* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module DGAMLN from package AMOS.
* Retrieved from NETLIB on Fri Apr  9 10:29:33 1999.
* ======================================================================
      DOUBLE PRECISION FUNCTION DGAMLN(Z,IERR)
C***BEGIN PROLOGUE  DGAMLN
C***DATE WRITTEN   830501   (YYMMDD)
C***REVISION DATE  830501   (YYMMDD)
C***CATEGORY NO.  B5F
C***KEYWORDS  GAMMA FUNCTION,LOGARITHM OF GAMMA FUNCTION
C***AUTHOR  AMOS, DONALD E., SANDIA NATIONAL LABORATORIES
C***PURPOSE  TO COMPUTE THE LOGARITHM OF THE GAMMA FUNCTION
C***DESCRIPTION
C
C               **** A DOUBLE PRECISION ROUTINE ****
C         DGAMLN COMPUTES THE NATURAL LOG OF THE GAMMA FUNCTION FOR
C         Z.GT.0.  THE ASYMPTOTIC EXPANSION IS USED TO GENERATE VALUES
C         GREATER THAN ZMIN WHICH ARE ADJUSTED BY THE RECURSION
C         G(Z+1)=Z*G(Z) FOR Z.LE.ZMIN.  THE FUNCTION WAS MADE AS
C         PORTABLE AS POSSIBLE BY COMPUTIMG ZMIN FROM THE NUMBER OF BASE
C         10 DIGITS IN A WORD, RLN=AMAX1(-ALOG10(R1MACH(4)),0.5E-18)
C         LIMITED TO 18 DIGITS OF (RELATIVE) ACCURACY.
C
C         SINCE INTEGER ARGUMENTS ARE COMMON, A TABLE LOOK UP ON 100
C         VALUES IS USED FOR SPEED OF EXECUTION.
C
C     DESCRIPTION OF ARGUMENTS
C
C         INPUT      Z IS D0UBLE PRECISION
C           Z      - ARGUMENT, Z.GT.0.0D0
C
C         OUTPUT      DGAMLN IS DOUBLE PRECISION
C           DGAMLN  - NATURAL LOG OF THE GAMMA FUNCTION AT Z.NE.0.0D0
C           IERR    - ERROR FLAG
C                     IERR=0, NORMAL RETURN, COMPUTATION COMPLETED
C                     IERR=1, Z.LE.0.0D0,    NO COMPUTATION
C
C
C***REFERENCES  COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT
C                 BY D. E. AMOS, SAND83-0083, MAY, 1983.
C***ROUTINES CALLED  I1MACH,D1MACH
C***END PROLOGUE  DGAMLN
      DOUBLE PRECISION CF, CON, FLN, FZ, GLN, RLN, S, TLG, TRM, TST,
     * T1, WDTOL, Z, ZDMY, ZINC, ZM, ZMIN, ZP, ZSQ, D1MACH
      INTEGER I, IERR, I1M, K, MZ, NZ, I1MACH
      DIMENSION CF(22), GLN(100)
C           LNGAMMA(N), N=1,100
      DATA GLN(1), GLN(2), GLN(3), GLN(4), GLN(5), GLN(6), GLN(7),
     1     GLN(8), GLN(9), GLN(10), GLN(11), GLN(12), GLN(13), GLN(14),
     2     GLN(15), GLN(16), GLN(17), GLN(18), GLN(19), GLN(20),
     3     GLN(21), GLN(22)/
     4     0.00000000000000000D+00,     0.00000000000000000D+00,
     5     6.93147180559945309D-01,     1.79175946922805500D+00,
     6     3.17805383034794562D+00,     4.78749174278204599D+00,
     7     6.57925121201010100D+00,     8.52516136106541430D+00,
     8     1.06046029027452502D+01,     1.28018274800814696D+01,
     9     1.51044125730755153D+01,     1.75023078458738858D+01,
     A     1.99872144956618861D+01,     2.25521638531234229D+01,
     B     2.51912211827386815D+01,     2.78992713838408916D+01,
     C     3.06718601060806728D+01,     3.35050734501368889D+01,
     D     3.63954452080330536D+01,     3.93398841871994940D+01,
     E     4.23356164607534850D+01,     4.53801388984769080D+01/
      DATA GLN(23), GLN(24), GLN(25), GLN(26), GLN(27), GLN(28),
     1     GLN(29), GLN(30), GLN(31), GLN(32), GLN(33), GLN(34),
     2     GLN(35), GLN(36), GLN(37), GLN(38), GLN(39), GLN(40),
     3     GLN(41), GLN(42), GLN(43), GLN(44)/
     4     4.84711813518352239D+01,     5.16066755677643736D+01,
     5     5.47847293981123192D+01,     5.80036052229805199D+01,
     6     6.12617017610020020D+01,     6.45575386270063311D+01,
     7     6.78897431371815350D+01,     7.12570389671680090D+01,
     8     7.46582363488301644D+01,     7.80922235533153106D+01,
     9     8.15579594561150372D+01,     8.50544670175815174D+01,
     A     8.85808275421976788D+01,     9.21361756036870925D+01,
     B     9.57196945421432025D+01,     9.93306124547874269D+01,
     C     1.02968198614513813D+02,     1.06631760260643459D+02,
     D     1.10320639714757395D+02,     1.14034211781461703D+02,
     E     1.17771881399745072D+02,     1.21533081515438634D+02/
      DATA GLN(45), GLN(46), GLN(47), GLN(48), GLN(49), GLN(50),
     1     GLN(51), GLN(52), GLN(53), GLN(54), GLN(55), GLN(56),
     2     GLN(57), GLN(58), GLN(59), GLN(60), GLN(61), GLN(62),
     3     GLN(63), GLN(64), GLN(65), GLN(66)/
     4     1.25317271149356895D+02,     1.29123933639127215D+02,
     5     1.32952575035616310D+02,     1.36802722637326368D+02,
     6     1.40673923648234259D+02,     1.44565743946344886D+02,
     7     1.48477766951773032D+02,     1.52409592584497358D+02,
     8     1.56360836303078785D+02,     1.60331128216630907D+02,
     9     1.64320112263195181D+02,     1.68327445448427652D+02,
     A     1.72352797139162802D+02,     1.76395848406997352D+02,
     B     1.80456291417543771D+02,     1.84533828861449491D+02,
     C     1.88628173423671591D+02,     1.92739047287844902D+02,
     D     1.96866181672889994D+02,     2.01009316399281527D+02,
     E     2.05168199482641199D+02,     2.09342586752536836D+02/
      DATA GLN(67), GLN(68), GLN(69), GLN(70), GLN(71), GLN(72),
     1     GLN(73), GLN(74), GLN(75), GLN(76), GLN(77), GLN(78),
     2     GLN(79), GLN(80), GLN(81), GLN(82), GLN(83), GLN(84),
     3     GLN(85), GLN(86), GLN(87), GLN(88)/
     4     2.13532241494563261D+02,     2.17736934113954227D+02,
     5     2.21956441819130334D+02,     2.26190548323727593D+02,
     6     2.30439043565776952D+02,     2.34701723442818268D+02,
     7     2.38978389561834323D+02,     2.43268849002982714D+02,
     8     2.47572914096186884D+02,     2.51890402209723194D+02,
     9     2.56221135550009525D+02,     2.60564940971863209D+02,
     A     2.64921649798552801D+02,     2.69291097651019823D+02,
     B     2.73673124285693704D+02,     2.78067573440366143D+02,
     C     2.82474292687630396D+02,     2.86893133295426994D+02,
     D     2.91323950094270308D+02,     2.95766601350760624D+02,
     E     3.00220948647014132D+02,     3.04686856765668715D+02/
      DATA GLN(89), GLN(90), GLN(91), GLN(92), GLN(93), GLN(94),
     1     GLN(95), GLN(96), GLN(97), GLN(98), GLN(99), GLN(100)/
     2     3.09164193580146922D+02,     3.13652829949879062D+02,
     3     3.18152639620209327D+02,     3.22663499126726177D+02,
     4     3.27185287703775217D+02,     3.31717887196928473D+02,
     5     3.36261181979198477D+02,     3.40815058870799018D+02,
     6     3.45379407062266854D+02,     3.49954118040770237D+02,
     7     3.54539085519440809D+02,     3.59134205369575399D+02/
C             COEFFICIENTS OF ASYMPTOTIC EXPANSION
      DATA CF(1), CF(2), CF(3), CF(4), CF(5), CF(6), CF(7), CF(8),
     1     CF(9), CF(10), CF(11), CF(12), CF(13), CF(14), CF(15),
     2     CF(16), CF(17), CF(18), CF(19), CF(20), CF(21), CF(22)/
     3     8.33333333333333333D-02,    -2.77777777777777778D-03,
     4     7.93650793650793651D-04,    -5.95238095238095238D-04,
     5     8.41750841750841751D-04,    -1.91752691752691753D-03,
     6     6.41025641025641026D-03,    -2.95506535947712418D-02,
     7     1.79644372368830573D-01,    -1.39243221690590112D+00,
     8     1.34028640441683920D+01,    -1.56848284626002017D+02,
     9     2.19310333333333333D+03,    -3.61087712537249894D+04,
     A     6.91472268851313067D+05,    -1.52382215394074162D+07,
     B     3.82900751391414141D+08,    -1.08822660357843911D+10,
     C     3.47320283765002252D+11,    -1.23696021422692745D+13,
     D     4.88788064793079335D+14,    -2.13203339609193739D+16/
C
C             LN(2*PI)
      DATA CON                    /     1.83787706640934548D+00/
C
C***FIRST EXECUTABLE STATEMENT  DGAMLN
      IERR=0
      IF (Z.LE.0.0D0) GO TO 70
      IF (Z.GT.101.0D0) GO TO 10
      NZ = INT(SNGL(Z))
      FZ = Z - FLOAT(NZ)
      IF (FZ.GT.0.0D0) GO TO 10
      IF (NZ.GT.100) GO TO 10
      DGAMLN = GLN(NZ)
      RETURN
   10 CONTINUE
      WDTOL = D1MACH(4)
      WDTOL = DMAX1(WDTOL,0.5D-18)
      I1M = I1MACH(14)
      RLN = D1MACH(5)*FLOAT(I1M)
      FLN = DMIN1(RLN,20.0D0)
      FLN = DMAX1(FLN,3.0D0)
      FLN = FLN - 3.0D0
      ZM = 1.8000D0 + 0.3875D0*FLN
      MZ = INT(SNGL(ZM)) + 1
      ZMIN = FLOAT(MZ)
      ZDMY = Z
      ZINC = 0.0D0
      IF (Z.GE.ZMIN) GO TO 20
      ZINC = ZMIN - FLOAT(NZ)
      ZDMY = Z + ZINC
   20 CONTINUE
      ZP = 1.0D0/ZDMY
      T1 = CF(1)*ZP
      S = T1
      IF (ZP.LT.WDTOL) GO TO 40
      ZSQ = ZP*ZP
      TST = T1*WDTOL
      DO 30 K=2,22
        ZP = ZP*ZSQ
        TRM = CF(K)*ZP
        IF (DABS(TRM).LT.TST) GO TO 40
        S = S + TRM
   30 CONTINUE
   40 CONTINUE
      IF (ZINC.NE.0.0D0) GO TO 50
      TLG = DLOG(Z)
      DGAMLN = Z*(TLG-1.0D0) + 0.5D0*(CON-TLG) + S
      RETURN
   50 CONTINUE
      ZP = 1.0D0
      NZ = INT(SNGL(ZINC))
      DO 60 I=1,NZ
        ZP = ZP*(Z+FLOAT(I-1))
   60 CONTINUE
      TLG = DLOG(ZDMY)
      DGAMLN = ZDMY*(TLG-1.0D0) - DLOG(ZP) + 0.5D0*(CON-TLG) + S
      RETURN
C
C
   70 CONTINUE
      IERR=1
      RETURN
      END

C
C
C     DUMMY ROUTINES
C
        SUBROUTINE DQDAGS()
        WRITE(6,*) ' THIS IS AN UNEXPECTED CALL TO DQDAGS'
        WRITE(6,*) ' YOU NEED TO LINK WITH A WORKING VERSION OF'
        WRITE(6,*) ' THAT IMSL SUBROUTINE'
        STOP
        END
C  MODIFIED BY TAC 2/5/99 TO USE DOUBLE PRECISION
C  
      SUBROUTINE DIFF(IORD,X0,XMIN,XMAX,F,EPS,ACC,DERIV,ERROR,IFAIL)
C
C             NUMERICAL DIFFERENTIATION OF USER DEFINED FUNCTION
C
C                         DAVID KAHANER, NBS (GAITHERSBURG) 
C
C  THE PROCEDURE DIFFERENTIATE CALCULATES THE FIRST, SECOND OR
C   THIRD ORDER DERIVATIVE OF A FUNCTION BY USING NEVILLE'S PROCESS TO
C   EXTRAPOLATE FROM A SEQUENCE OF SIMPLE POLYNOMIAL APPROXIMATIONS BASED ON
C   INTERPOLATING POINTS DISTRIBUTED SYMMETRICALLY ABOUT X0 (OR LYING ONLY ON
C   ONE SIDE OF X0 SHOULD THIS BE NECESSARY).  IF THE SPECIFIED TOLERANCE IS
C   NON-ZERO THEN THE PROCEDURE ATTEMPTS TO SATISFY THIS ABSOLUTE OR RELATIVE
C   ACCURACY REQUIREMENT, WHILE IF IT IS UNSUCCESSFUL OR IF THE TOLERANCE IS
C   SET TO ZERO THEN THE RESULT HAVING THE MINIMUM ACHIEVABLE ESTIMATED ERROR
C   IS RETURNED INSTEAD.
C
C INPUT PARAMETERS:
C IORD = 1, 2 OR 3 SPECIFIES THAT THE FIRST, SECOND OR THIRD ORDER
C   DERIVATIVE,RESPECTIVELY, IS REQUIRED.
C X0 IS THE POINT AT WHICH THE DERIVATIVE OF THE FUNCTION IS TO BE CALCULATED.
C XMIN, XMAX RESTRICT THE INTERPOLATING POINTS TO LIE IN [XMIN, XMAX], WHICH
C   SHOULD BE THE LARGEST INTERVAL INCLUDING X0 IN WHICH THE FUNCTION IS
C   CALCULABLE AND CONTINUOUS.
C F, A REAL PROCEDURE SUPPLIED BY THE USER, MUST YIELD THE VALUE OF THE
C   FUNCTION AT X FOR ANY X IN [XMIN, XMAX] WHEN CALLED BY F(X).
C EPS DENOTES THE TOLERANCE, EITHER ABSOLUTE OR RELATIVE.  EPS=0 SPECIFIES THAT 
C   THE ERROR IS TO BE MINIMISED, WHILE EPS>0 OR EPS<0 SPECIFIES THAT THE
C   ABSOLUTE OR RELATIVE ERROR, RESPECTIVELY, MUST NOT EXCEED ABS(EPS) IF
C   POSSIBLE.  THE ACCURACY REQUIREMENT SHOULD NOT BE MADE STRICTER THAN
C   NECESSARY, SINCE THE AMOUNT OF COMPUTATION TENDS TO INCREASE AS
C   THE MAGNITUDE OF EPS DECREASES, AND IS PARTICULARLY HIGH WHEN EPS=0.
C ACC DENOTES THAT THE ABSOLUTE (ACC>0) OR RELATIVE (ACC<0) ERRORS IN THE
C   COMPUTED VALUES OF THE FUNCTION ARE MOST UNLIKELY TO EXCEED ABS(ACC), WHICH 
C   SHOULD BE AS SMALL AS POSSIBLE.  IF THE USER CANNOT ESTIMATE ACC WITH
C   COMPLETE CONFIDENCE, THEN IT SHOULD BE SET TO ZERO.
C
C OUTPUT PARAMETERS:
C DERIV IS THE CALCULATED VALUE OF THE DERIVATIVE.
C ERROR IS AN ESTIMATED UPPER BOUND ON THE MAGNITUDE OF THE ABSOLUTE ERROR IN
C   THE CALCULATED RESULT.  IT SHOULD ALWAYS BE EXAMINED, SINCE IN EXTREME CASE 
C   MAY INDICATE THAT THERE ARE NO CORRECT SIGNIFICANT DIGITS IN THE VALUE
C   RETURNED FOR DERIVATIVE.
C IFAIL WILL HAVE ONE OF THE FOLLOWING VALUES ON EXIT:
C   0   THE PROCEDURE WAS SUCCESSFUL.
C   1   THE ESTIMATED ERROR IN THE RESULT EXCEEDS THE (NON-ZERO) REQUESTED
C          ERROR, BUT THE MOST ACCURATE RESULT POSSIBLE HAS BEEN RETURNED.
C   2   INPUT DATA INCORRECT (DERIVATIVE AND ERROR WILL BE UNDEFINED).
C   3   THE INTERVAL [XMIN, XMAX] IS TOO SMALL (DERIVATIVE AND ERROR WILL BE
C          UNDEFINED);
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

      EXTERNAL F
      DOUBLE PRECISION
     + X0,XMIN,XMAX,ACC,DERIV,ERROR,BETA,BETA4,H,H0,H1,H2,
     +NEWH1,NEWH2,HEVAL,HPREV,BASEH,HACC1,HACC2,NHACC1,
     +NHACC2,MINH,MAXH,MAXH1,MAXH2,TDERIV,F0,TWOF0,F1,F2,F3,F4,FMAX,
     +MAXFUN,PMAXF,DF1,DELTAF,PDELTA,Z,ZPOWER,C0F0,C1,C2,C3,DNEW,DPREV,
     +RE,TE,NEWERR,TEMERR,NEWACC,PACC1,PACC2,FACC1,FACC2,ACC0,
     +ACC1,ACC2,RELACC,TWOINF,TWOSUP,S, 
     +D(10),DENOM(10),E(10),MINERR(10),MAXF(0:10),SAVE(0:13),
     +STOREF(-45:45),FACTOR
C
      INTEGER IORD,IFAIL,ETA,INF,SUP,I,J,K,N,NMAX,METHOD,SIGNH,FCOUNT,
     +INIT
      LOGICAL IGNORE(10),CONTIN,SAVED
C
C
C ETA IS THE MINIMUM NUMBER OF SIGNIFICANT BINARY DIGITS (APART FROM THE
C SIGN DIGIT) USED TO REPRESENT THE MANTISSA OF REAL NUMBERS. IT SHOULD
C BE DEVREASED BY ONE IF THE COMPUTER TRUNCATES RATHER THAN ROUNDS.
C INF, SUP ARE THE LARGEST POSSIBLE POSITIVE INTEGERS SUBJECT TO
C 2**(-INF), -2**(-INF), 2**SUP, AND -2**SUP ALL BEING REPRESENTABLE REAL
C NUMBERS.
      ETA=I1MACH(14) - 1
      INF=-I1MACH(12) - 2
      SUP=I1MACH(13)-1
      IF(IORD.LT.1 .OR. IORD.GT.3 .OR. XMAX.LE.XMIN .OR.
     +  X0.GT.XMAX .OR. X0.LT.XMIN) THEN
          IFAIL = 2 
          RETURN
      ENDIF
C
      TWOINF = 2.**(-INF)
      TWOSUP = 2.**SUP
      FACTOR = 2**(FLOAT((INF+SUP))/30.)
      IF(FACTOR.LT.256.)FACTOR=256.
      MAXH1 = XMAX - X0
      SIGNH = 1
      IF(X0-XMIN .LE. MAXH1)THEN
          MAXH2 = X0 - XMIN
      ELSE
          MAXH2 = MAXH1
          MAXH1 = X0 - XMIN
          SIGNH = -1
      ENDIF
      RELACC = 2.**(1-ETA)
      MAXH1 = (1.-RELACC)*MAXH1
      MAXH2 = (1.-RELACC)*MAXH2
      S=128.*TWOINF 
      IF(ABS(X0).GT.128.*TWOINF*2.**ETA) S = ABS(X0)*2.**(-ETA)
      IF(MAXH1.LT.S)THEN
C         INTERVAL TOO SMALL
          IFAIL =3
          RETURN
      ENDIF
      IF(ACC.LT.0.) THEN
          IF(-ACC.GT.RELACC)RELACC = -ACC
          ACC = 0.
      ENDIF
C
C     DETERMINE THE SMALLEST SPACING AT WHICH THE CALCULATED
C     FUNCTION VALUES ARE UNEQUAL NEAR X0.
C
      F0 = F(X0)
      TWOF0 = F0 + F0
      IF(ABS(X0) .GT. TWOINF*2.**ETA) THEN
          H = ABS(X0)*2.**(-ETA)
          Z = 2.
      ELSE
          H = TWOINF
          Z = 64.
      ENDIF
      DF1 = F(X0+SIGNH*H) - F0
   80 IF(DF1 .NE. 0. .OR. Z*H .GT. MAXH1) GOTO 100
      H = Z*H
      DF1 = F(X0+SIGNH*H) - F0
      IF(Z .NE.2.) THEN
          IF(DF1 .NE. 0.) THEN
              H = H/Z
              Z = 2.
              DF1 = 0.
          ELSE
              IF(Z*H .GT. MAXH1) Z = 2. 
          ENDIF
      ENDIF
      GOTO 80
  100 CONTINUE
C
      IF(DF1 .EQ. 0.) THEN
C         CONSTANT FUNCTION
          DERIV = 0.
          ERROR = 0.
          IFAIL = 0 
          RETURN
      ENDIF
      IF(H .GT. MAXH1/128.) THEN
C         MINIMUM H TOO LARGE 
          IFAIL = 3 
          RETURN
      ENDIF
C
      H = 8.*H
      H1 = SIGNH*H
      H0 = H1
      H2 = -H1
      MINH = 2.**(-MIN(INF,SUP)/IORD)
      IF(MINH.LT.H) MINH = H
      IF(IORD.EQ.1) S = 8.
      IF(IORD.EQ.2) S = 9.*SQRT(3.)
      IF(IORD.EQ.3) S = 27.
      IF(MINH.GT.MAXH1/S) THEN
          IFAIL = 3 
          RETURN
      ENDIF
      IF(MINH.GT.MAXH2/S .OR. MAXH2.LT.128.*TWOINF) THEN
          METHOD = 1
      ELSE
          METHOD = 2
      ENDIF
C
C     METHOD 1 USES 1-SIDED FORMULAE, AND METHOD 2 SYMMETRIC.
C         NOW ESTIMATE ACCURACY OF CALCULATED FUNCTION VALUES.
C
      IF(METHOD.NE.2 .OR. IORD.EQ.2) THEN
          IF(X0.NE.0.) THEN
              CALL FACCUR(0.D0,-H1,ACC0,X0,F,TWOINF,F0,F1)
          ELSE
              ACC0 = 0.
          ENDIF
      ENDIF
C
      IF(ABS(H1) .GT. TWOSUP/128.) THEN 
          HACC1 = TWOSUP
      ELSE
          HACC1 = 128.*H1
      ENDIF
C
      IF(ABS(HACC1)/4. .LT. MINH) THEN
          HACC1 = 4.*SIGNH*MINH
      ELSEIF(ABS(HACC1) .GT. MAXH1) THEN
          HACC1 = SIGNH*MAXH1 
      ENDIF
      F1 = F(X0+HACC1)
      CALL FACCUR(HACC1,H1,ACC1,X0,F,TWOINF,F0,F1)
      IF(METHOD.EQ.2) THEN
          HACC2 = -HACC1
          IF(ABS(HACC2) .GT. MAXH2) HACC2 = -SIGNH * MAXH2
          F1 = F(X0 + HACC2)
          CALL FACCUR(HACC2,H2,ACC2,X0,F,TWOINF,F0,F1)
      ENDIF
      NMAX = 8
      IF(ETA.GT.36) NMAX = 10 
      N = -1
      FCOUNT = 0
      DERIV = 0.
      ERROR = TWOSUP
      INIT = 3
      CONTIN = .TRUE.
C
  130 N = N+1
      IF(.NOT. CONTIN) GOTO 800
C
      IF(INIT.EQ.3) THEN
C         CALCULATE COEFFICIENTS FOR DIFFERENTIATION FORMULAE
C             AND NEVILLE EXTRAPOLATION ALGORITHM 
          IF(IORD.EQ.1) THEN
              BETA=2.
          ELSEIF(METHOD.EQ.2)THEN
              BETA = SQRT(2.) 
          ELSE
              BETA = SQRT(3.) 
          ENDIF
          BETA4 = BETA**4.
          Z = BETA
          IF(METHOD.EQ.2) Z = Z**2
          ZPOWER = 1.
          DO 150 K = 1,NMAX
              ZPOWER = Z*ZPOWER
              DENOM(K) = ZPOWER-1
  150     CONTINUE
          IF(METHOD.EQ.2 .AND. IORD.EQ.1) THEN
              E(1) = 5.
              E(2) = 6.3
              DO 160 I = 3,NMAX
  160             E(I) = 6.81 
        ELSEIF((METHOD.NE.2.AND.IORD.EQ.1) .OR. (METHOD.EQ.2.AND.
     +            IORD.EQ.2)) THEN
              E(1) = 10.
              E(2) = 16.
              E(3) = 20.36
              E(4) = 23.
              E(5) = 24.46
              DO 165 I = 6,NMAX
  165             E(I) = 26.
              IF(METHOD.EQ.2.AND.IORD.EQ.2) THEN
                  DO 170 I = 1,NMAX
  170                  E(I)=2*E(I)
              ENDIF 
          ELSEIF(METHOD.NE.2.AND.IORD.EQ.2) THEN
              E(1) = 17.78
              E(2) = 30.06
              E(3) = 39.66
              E(4) = 46.16
              E(5) = 50.26
              DO 175 I = 6,NMAX
  175             E(I) = 55.
          ELSEIF(METHOD.EQ.2.AND.IORD.EQ.3) THEN
              E(1) = 25.97
              E(2) = 41.22
              E(3) = 50.95
              E(4) = 56.4
              E(5) = 59.3
              DO 180 I = 6,NMAX
  180             E(I) = 62.
          ELSE
              E(1) = 24.5
              E(2) = 40.4
              E(3) = 52.78
              E(4) = 61.2
              E(5) = 66.55
              DO 185 I = 6,NMAX
  185             E(I) = 73.
              C0F0 = -TWOF0/(3.*BETA)
              C1 = 3./(3.*BETA-1.)
              C2 = -1./(3.*(BETA-1.))
              C3 = 1./(3.*BETA*(5.-2.*BETA))
          ENDIF
      ENDIF
C
C
      IF(INIT.GE.2) THEN
C         INITIALIZATION OF STEPLENGTHS, ACCURACY AND OTHER 
C             PARAMETERS
C
          HEVAL = SIGNH*MINH
          H = HEVAL 
          BASEH = HEVAL
          MAXH = MAXH2
          IF(METHOD.EQ.1)MAXH = MAXH1
          DO 300 K = 1,NMAX
              MINERR(K) = TWOSUP
              IGNORE(K) = .FALSE.
  300     CONTINUE
          IF(METHOD.EQ.1) NEWACC = ACC1 
          IF(METHOD.EQ.-1) NEWACC = ACC2
          IF(METHOD.EQ.2) NEWACC = (ACC1+ACC2)/2. 
          IF(NEWACC.LT.ACC) NEWACC = ACC
          IF((METHOD.NE.2 .OR. IORD.EQ.2) .AND. NEWACC.LT.ACC0)
     +            NEWACC = ACC0
          IF(METHOD.NE.-1) THEN
              FACC1 = ACC1
              NHACC1 = HACC1
              NEWH1 = H1
          ENDIF
          IF(METHOD.NE.1) THEN
              FACC2 = ACC2
              NHACC2 = HACC2
              NEWH2 = H2
          ELSE
              FACC2 = 0.
              NHACC2 = 0.
          ENDIF
          INIT = 1
          J = 0
          SAVED = .FALSE.
      ENDIF
C
C     CALCULATE NEW OR INITIAL FUNCTION VALUES
C
      IF(INIT.EQ.1 .AND. (N.EQ.0 .OR. IORD.EQ.1) .AND.
     +        .NOT.(METHOD.EQ.2 .AND. FCOUNT.GE.45)) THEN
          IF(METHOD.EQ.2) THEN
              FCOUNT = FCOUNT + 1
              F1 = F(X0+HEVAL)
              STOREF(FCOUNT) = F1
              F2 = F(X0-HEVAL)
              STOREF(-FCOUNT) = F2
          ELSE
              J = J+1
              IF(J.LE.FCOUNT) THEN
                  F1 = STOREF(J*METHOD) 
              ELSE
                  F1 = F(X0+HEVAL)
              ENDIF 
          ENDIF
      ELSE
          F1 = F(X0+HEVAL)
          IF(METHOD.EQ.2) F2 = F(X0-HEVAL)
      ENDIF
      IF(N.EQ.0) THEN
          IF(METHOD.EQ.2 .AND. IORD.EQ.3) THEN
              PDELTA = F1-F2
              PMAXF = (ABS(F1)+ABS(F2))/2.
              HEVAL = BETA*HEVAL
              F1 = F(X0+HEVAL)
              F2 = F(X0-HEVAL)
              DELTAF = F1-F2
              MAXFUN = (ABS(F1)+ABS(F2))/2.
              HEVAL = BETA*HEVAL
              F1 = F(X0+HEVAL)
              F2 = F(X0-HEVAL)
          ELSEIF(METHOD.NE.2 .AND. IORD.GE.2) THEN
              IF(IORD.EQ.2) THEN
                  F3 = F1
              ELSE
                  F4 = F1
                  HEVAL = BETA*HEVAL
                  F3 = F(X0+HEVAL)
              ENDIF 
              HEVAL = BETA*HEVAL
              F2 = F(X0+HEVAL)
              HEVAL = BETA*HEVAL
              F1 = F(X0+HEVAL)
          ENDIF
      ENDIF
C
C     EVALUATE A NEW APPROXIMATION DNEW TO THE DERIVATIVE
C
      IF(N.GT.NMAX) THEN
          N = NMAX
          DO 400 I = 1,N
  400         MAXF(I-1) = MAXF(I)
      ENDIF
      IF(METHOD.EQ.2) THEN
          MAXF(N) = (ABS(F1)+ABS(F2))/2.
          IF(IORD.EQ.1) THEN
              DNEW = (F1-F2)/2.
          ELSEIF(IORD.EQ.2) THEN
              DNEW = F1+F2-TWOF0
          ELSE
              DNEW = -PDELTA
              PDELTA = DELTAF 
              DELTAF = F1-F2
              DNEW = DNEW + .5*DELTAF
              IF(MAXF(N).LT.PMAXF) MAXF(N) = PMAXF
              PMAXF = MAXFUN
              MAXFUN = (ABS(F1)+ABS(F2))/2.
          ENDIF
      ELSE
          MAXF(N) = ABS(F1)
          IF(IORD.EQ.1) THEN
              DNEW = F1-F0
          ELSEIF(IORD.EQ.2) THEN
              DNEW = (TWOF0-3*F3+F1)/3. 
              IF(MAXF(N).LT.ABS(F3)) MAXF(N) = ABS(F3)
              F3 = F2
              F2 = F1
          ELSE
              DNEW = C3*F1+C2*F2+C1*F4+C0F0
              IF(MAXF(N).LT.ABS(F2)) MAXF(N) = ABS(F2)
              IF(MAXF(N).LT.ABS(F4)) MAXF(N) = ABS(F4)
              F4 = F3
              F3 = F2
              F2 = F1
          ENDIF
      ENDIF
      IF(ABS(H).GT.1) THEN
          DNEW = DNEW/H**IORD 
      ELSE
          IF(128.*ABS(DNEW).GT.TWOSUP*ABS(H)**IORD) THEN
              DNEW = TWOSUP/128.
          ELSE
              DNEW = DNEW/H**IORD
          ENDIF
      ENDIF
C
      IF(INIT.EQ.0) THEN
C         UPDATE ESTIMATED ACCURACY OF FUNCTION VALUES
          NEWACC = ACC
          IF((METHOD.NE.2 .OR. IORD.EQ.2) .AND. NEWACC.LT.ACC0)
     +        NEWACC = ACC0
          IF(METHOD.NE.-1 .AND. ABS(NHACC1).LE.1.125*ABS(HEVAL)/BETA4)
     +               THEN
              NHACC1 = HEVAL
              PACC1 = FACC1
              CALL FACCUR(NHACC1,NEWH1,FACC1,X0,F,TWOINF,F0,F1)
              IF(FACC1.LT.PACC1) FACC1=(3*FACC1+PACC1)/4.
          ENDIF
          IF(METHOD.NE.1 .AND. ABS(NHACC2).LE.1.125*ABS(HEVAL)/BETA4) 
     +            THEN
              IF(METHOD.EQ.2) THEN
                  F1 = F2
                  NHACC2 = -HEVAL
              ELSE
                  NHACC2 = HEVAL
              ENDIF 
              PACC2 = FACC2
              CALL FACCUR(NHACC2,NEWH2,FACC2,X0,F,TWOINF,F0,F1)
              IF(FACC2.LT.PACC2) FACC2 = (3*FACC2+PACC2)/4. 
          ENDIF
          IF(METHOD.EQ.1 .AND. NEWACC.LT.FACC1) NEWACC = FACC1
          IF(METHOD.EQ.-1 .AND. NEWACC.LT.FACC2) NEWACC = FACC2
          IF(METHOD.EQ.2 .AND. NEWACC.LT.(FACC1+FACC2)/2.)
     +            NEWACC = (FACC1+FACC2)/2.
      ENDIF
C
C     EVALUATE SUCCESSIVE ELEMENTS OF THE CURRENT ROW IN THE NEVILLE
C     ARRAY, ESTIMATING AND EXAMINING THE TRUNCATION AND ROUNDING
C     ERRORS IN EACH
C
      CONTIN = N.LT.NMAX
      HPREV = ABS(H)
      FMAX = MAXF(N)
      IF((METHOD.NE.2 .OR. IORD.EQ.2) .AND. FMAX.LT.ABS(F0))
     +        FMAX = ABS(F0)
C
      DO 500 K = 1,N
          DPREV = D(K)
          D(K) = DNEW
          DNEW = DPREV+(DPREV-DNEW)/DENOM(K)
          TE = ABS(DNEW-D(K)) 
          IF(FMAX.LT.MAXF(N-K)) FMAX = MAXF(N-K)
          HPREV = HPREV/BETA
          IF(NEWACC.GE.RELACC*FMAX) THEN
              RE = NEWACC*E(K)
          ELSE
              RE = RELACC*FMAX*E(K)
          ENDIF
          IF(RE.NE.0.) THEN
              IF(HPREV.GT.1) THEN
                  RE = RE/HPREV**IORD
              ELSEIF(2*RE.GT.TWOSUP*HPREV**IORD) THEN
                  RE = TWOSUP/2.
              ELSE
                  RE = RE/HPREV**IORD
              ENDIF 
          ENDIF
          NEWERR = TE+RE
          IF(TE.GT.RE) NEWERR = 1.25*NEWERR
          IF(.NOT. IGNORE(K)) THEN
              IF((INIT.EQ.0 .OR. (K.EQ.2 .AND. .NOT.IGNORE(1)))
     +                .AND. NEWERR.LT.ERROR) THEN 
                  DERIV = D(K)
                  ERROR = NEWERR
              ENDIF 
              IF(INIT.EQ.1 .AND. N.EQ.1) THEN
              TDERIV = D(1)
                  TEMERR = NEWERR
              ENDIF 
              IF(MINERR(K).LT.TWOSUP/4) THEN
                  S = 4*MINERR(K)
              ELSE
                  S = TWOSUP
              ENDIF 
              IF(TE.GT.RE .OR. NEWERR.GT.S) THEN
                  IGNORE(K) = .TRUE.
              ELSE
                  CONTIN = .TRUE.
              ENDIF 
              IF(NEWERR.LT.MINERR(K)) MINERR(K) = NEWERR
              IF(INIT.EQ.1 .AND. N.EQ.2 .AND. K.EQ.1 .AND.
     +                .NOT.IGNORE(1)) THEN
                  IF(NEWERR.LT.TEMERR) THEN
                      TDERIV = D(1)
                      TEMERR = NEWERR
                  ENDIF
                  IF(TEMERR.LT.ERROR) THEN
                      DERIV = TDERIV
                      ERROR = TEMERR
                  ENDIF
              ENDIF 
          ENDIF
  500 CONTINUE
C
      IF(N.LT.NMAX) D(N+1) = DNEW
                 IF(EPS.LT.0.) THEN
          S = ABS(EPS*DERIV)
      ELSE
          S = EPS
      ENDIF
      IF(ERROR.LE.S) THEN
          CONTIN = .FALSE.
      ELSEIF(INIT.EQ.1 .AND. (N.EQ.2 .OR. IGNORE(1))) THEN
          IF((IGNORE(1) .OR. IGNORE(2)) .AND. SAVED) THEN
              SAVED = .FALSE. 
              N = 2 
              H = BETA * SAVE(0)
              HEVAL = BETA*SAVE(1)
              MAXF(0) = SAVE(2)
              MAXF(1) = SAVE(3)
              MAXF(2) = SAVE(4)
              D(1) = SAVE(5)
              D(2) = SAVE(6)
              D(3) = SAVE(7)
              MINERR(1) = SAVE(8)
              MINERR(2) = SAVE(9)
              IF(METHOD.EQ.2 .AND. IORD.EQ.3) THEN
                  PDELTA = SAVE(10)
                  DELTAF = SAVE(11)
                  PMAXF = SAVE(12)
                  MAXFUN = SAVE(13)
              ELSEIF(METHOD.NE.2 .AND. IORD.GE.2) THEN
                  F2 = SAVE(10)
                  F3 = SAVE(11)
                  IF(IORD.EQ.3) F4 = SAVE(12)
              ENDIF 
              INIT = 0
              IGNORE(1) = .FALSE.
              IGNORE(2) = .FALSE.
          ELSEIF(.NOT. (IGNORE(1) .OR. IGNORE(2)) .AND. N.EQ.2
     +            .AND. BETA4*FACTOR*ABS(HEVAL).LE.MAXH) THEN
C             SAVE ALL CURRENT VALUES IN CASE OF RETURN TO
C                 CURRENT POINT
              SAVED = .TRUE.
              SAVE(0) = H
              SAVE(1) = HEVAL 
              SAVE(2) = MAXF(0)
              SAVE(3) = MAXF(1)
              SAVE(4) = MAXF(2)
              SAVE(5) = D(1)
              SAVE(6) = D(2)
              SAVE(7) = D(3)
              SAVE(8) = MINERR(1)
              SAVE(9) = MINERR (2)
              IF(METHOD.EQ.2 .AND. IORD.EQ.3) THEN
                  SAVE(10) = PDELTA
                  SAVE(11) = DELTAF
                  SAVE(12) = PMAXF
                  SAVE(13) = MAXFUN
              ELSEIF(METHOD.NE.2 .AND. IORD.GE.2) THEN
                  SAVE(10) = F2
                  SAVE(11) = F3
                  IF(IORD.EQ.3) SAVE(12) = F4
              ENDIF 
              H = FACTOR*BASEH
              HEVAL = H
              BASEH = H
              N = -1
          ELSE
              INIT = 0
              H = BETA*H
              HEVAL = BETA*HEVAL
          ENDIF
      ELSEIF(CONTIN .AND. BETA*ABS(HEVAL).LE.MAXH) THEN
          H = BETA*H
          HEVAL = BETA*HEVAL
      ELSEIF(METHOD.NE.1) THEN
          CONTIN = .TRUE.
          IF(METHOD.EQ.2) THEN
              INIT = 3
              METHOD = -1
              IF(IORD.NE.2) THEN
                  IF(X0.NE.0.) THEN
                      CALL FACCUR(0.D0,-H0,ACC0,X0,F,TWOINF,F0,F1)
                  ELSE
                      ACC0 = 0.
                  ENDIF
              ENDIF 
          ELSE
              INIT = 2
              METHOD = 1
          ENDIF
          N = -1
          SIGNH = -SIGNH
      ELSE
          CONTIN = .FALSE.
      ENDIF
      GOTO 130
  800 IF(EPS.LT.0.) THEN
          S = ABS(EPS*DERIV)
      ELSE
          S = EPS
      ENDIF
      IFAIL = 0
      IF(EPS.NE.0. .AND. ERROR.GT.S) IFAIL = 1
      RETURN
      END 
C*DECK FACCUR
      SUBROUTINE FACCUR(H0,H1,FACC,X0,F,TWOINF,F0,F1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

      DOUBLE PRECISION
     +  H0,H1,FACC,A0,A1,F00,F2,DELTAF,T0,T1,X0,F,DF(5),F0,F1
     +        ,TWOINF
     +         D1
      INTEGER J
      EXTERNAL F
      D1 = 1.
      T0 = 0.
      T1 = 0.
      IF(H0.NE.0.) THEN
          IF(X0+H0.NE.0.) THEN
              F00 = F1
          ELSE
              H0 = 0.875*H0
              F00 = F(X0+H0)
          ENDIF
          IF(ABS(H1) .GE. 32.*TWOINF) H1 = H1/8.
          IF(16.*ABS(H1) .GT. ABS(H0)) H1 = DSIGN(H1,D1)*ABS(H0)/16.
          IF(F(X0+H0-H1) .EQ. F00) THEN 
              IF(256.*ABS(H1) .LE. ABS(H0)) THEN
                  H1 = 2.*H1
   10             IF(F(X0+H0-H1).NE.F00 .OR. 256.*ABS(H1).GT.ABS(H0)) 
     +                    GOTO 20
                  H1 = 2.*H1
                  GOTO 10
   20             H1 = 8.*H1
  
              ELSE
                  H1 = DSIGN(H1,D1)*ABS(H0)/16.
              ENDIF 
          ELSE
              IF(256.*TWOINF .LE. ABS(H0)) THEN
   30             IF(F(X0+H0-H1/2.).EQ.F00 .OR. ABS(H1).LT.4.*TWOINF) 
     +                GOTO 40 
                  H1 = H1/2.
                  GOTO 30
   40             CONTINUE
                  H1 = 8.*H1
                  IF(16.*ABS(H1) .GT. ABS(H0)) H1 = DSIGN(H1,D1)
     +                     *ABS(H0)/16. 
              ELSE
                  H1 = DSIGN(H1,D1)*ABS(H0)/16.
              ENDIF 
          ENDIF
      ELSE
          F00 = F0
      ENDIF
  
      DO 50 J = 1,5 
          F2 = F(X0+H0-FLOAT(2*J-1)*H1) 
          DF(J) = F2 - F00
          T0 = T0+DF(J)
          T1 = T1+FLOAT(2*J-1)*DF(J)
   50 CONTINUE
      A0 = (33.*T0-5.*T1)/73. 
      A1 = (-5.*T0+1.2*T1)/73.
      FACC = ABS(A0)
      DO 70 J = 1,5 
          DELTAF = ABS(DF(J)-(A0+FLOAT(2*J-1)*A1))
          IF(FACC.LT.DELTAF) FACC = DELTAF
   70 CONTINUE
      FACC = 2.*FACC
      RETURN
      END 
* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module DQAGI from package CMLIB.
* Retrieved from CAMSUN on Mon Apr 19 09:33:13 1999.
* ======================================================================
      SUBROUTINE DQAGI(F,BOUND,INF,EPSABS,EPSREL,RESULT,ABSERR,NEVAL,
     1   IER,LIMIT,LENW,LAST,IWORK,WORK)
C***BEGIN PROLOGUE  DQAGI
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***CATEGORY NO.  H2A3A1,H2A4A1
C***KEYWORDS  AUTOMATIC INTEGRATOR,EXTRAPOLATION,GENERAL-PURPOSE,
C             GLOBALLY ADAPTIVE,INFINITE INTERVALS,TRANSFORMATION
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  The routine calculates an approximation result to a given
C            INTEGRAL   I = Integral of F over (BOUND,+INFINITY)
C            OR I = Integral of F over (-INFINITY,BOUND)
C            OR I = Integral of F over (-INFINITY,+INFINITY)
C            Hopefully satisfying following claim for accuracy
C            ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I)).
C***DESCRIPTION
C
C        Integration over infinite intervals
C        Standard fortran subroutine
C
C        PARAMETERS
C         ON ENTRY
C            F      - Double precision
C                     Function subprogram defining the integrand
C                     function F(X). The actual name for F needs to be
C                     declared E X T E R N A L in the driver program.
C
C            BOUND  - Double precision
C                     Finite bound of integration range
C                     (has no meaning if interval is doubly-infinite)
C
C            INF    - Integer
C                     indicating the kind of integration range involved
C                     INF = 1 corresponds to  (BOUND,+INFINITY),
C                     INF = -1            to  (-INFINITY,BOUND),
C                     INF = 2             to (-INFINITY,+INFINITY).
C
C            EPSABS - Double precision
C                     Absolute accuracy requested
C            EPSREL - Double precision
C                     Relative accuracy requested
C                     If  EPSABS.LE.0
C                     and EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
C                     the routine will end with IER = 6.
C
C
C         ON RETURN
C            RESULT - Double precision
C                     Approximation to the integral
C
C            ABSERR - Double precision
C                     Estimate of the modulus of the absolute error,
C                     which should equal or exceed ABS(I-RESULT)
C
C            NEVAL  - Integer
C                     Number of integrand evaluations
C
C            IER    - Integer
C                     IER = 0 normal and reliable termination of the
C                             routine. It is assumed that the requested
C                             accuracy has been achieved.
C                   - IER.GT.0 abnormal termination of the routine. The
C                             estimates for result and error are less
C                             reliable. It is assumed that the requested
C                             accuracy has not been achieved.
C            ERROR MESSAGES
C                     IER = 1 Maximum number of subdivisions allowed
C                             has been achieved. One can allow more
C                             subdivisions by increasing the value of
C                             LIMIT (and taking the according dimension
C                             adjustments into account). However, if
C                             this yields no improvement it is advised
C                             to analyze the integrand in order to
C                             determine the integration difficulties. If
C                             the position of a local difficulty can be
C                             determined (e.g. SINGULARITY,
C                             DISCONTINUITY within the interval) one
C                             will probably gain from splitting up the
C                             interval at this point and calling the
C                             integrator on the subranges. If possible,
C                             an appropriate special-purpose integrator
C                             should be used, which is designed for
C                             handling the type of difficulty involved.
C                         = 2 The occurrence of roundoff error is
C                             detected, which prevents the requested
C                             tolerance from being achieved.
C                             The error may be under-estimated.
C                         = 3 Extremely bad integrand behaviour occurs
C                             at some points of the integration
C                             interval.
C                         = 4 The algorithm does not converge.
C                             Roundoff error is detected in the
C                             extrapolation table.
C                             It is assumed that the requested tolerance
C                             cannot be achieved, and that the returned
C                             RESULT is the best which can be obtained.
C                         = 5 The integral is probably divergent, or
C                             slowly convergent. It must be noted that
C                             divergence can occur with any other value
C                             of IER.
C                         = 6 The input is invalid, because
C                             (EPSABS.LE.0 and
C                              EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28))
C                              or LIMIT.LT.1 or LENIW.LT.LIMIT*4.
C                             RESULT, ABSERR, NEVAL, LAST are set to
C                             zero. Exept when LIMIT or LENIW is
C                             invalid, IWORK(1), WORK(LIMIT*2+1) and
C                             WORK(LIMIT*3+1) are set to ZERO, WORK(1)
C                             is set to A and WORK(LIMIT+1) to B.
C
C         DIMENSIONING PARAMETERS
C            LIMIT - Integer
C                    Dimensioning parameter for IWORK
C                    LIMIT determines the maximum number of subintervals
C                    in the partition of the given integration interval
C                    (A,B), LIMIT.GE.1.
C                    If LIMIT.LT.1, the routine will end with IER = 6.
C
C            LENW  - Integer
C                    Dimensioning parameter for WORK
C                    LENW must be at least LIMIT*4.
C                    If LENW.LT.LIMIT*4, the routine will end
C                    with IER = 6.
C
C            LAST  - Integer
C                    On return, LAST equals the number of subintervals
C                    produced in the subdivision process, which
C                    determines the number of significant elements
C                    actually in the WORK ARRAYS.
C
C         WORK ARRAYS
C            IWORK - Integer
C                    Vector of dimension at least LIMIT, the first
C                    K elements of which contain pointers
C                    to the error estimates over the subintervals,
C                    such that WORK(LIMIT*3+IWORK(1)),... ,
C                    WORK(LIMIT*3+IWORK(K)) form a decreasing
C                    sequence, with K = LAST if LAST.LE.(LIMIT/2+2), and
C                    K = LIMIT+1-LAST otherwise
C
C            WORK  - Double precision
C                    Vector of dimension at least LENW
C                    on return
C                    WORK(1), ..., WORK(LAST) contain the left
C                     end points of the subintervals in the
C                     partition of (A,B),
C                    WORK(LIMIT+1), ..., WORK(LIMIT+LAST) Contain
C                     the right end points,
C                    WORK(LIMIT*2+1), ...,WORK(LIMIT*2+LAST) contain the
C                     integral approximations over the subintervals,
C                    WORK(LIMIT*3+1), ..., WORK(LIMIT*3)
C                     contain the error estimates.
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DQAGIE,XERROR
C***END PROLOGUE  DQAGI
C
      DOUBLE PRECISION ABSERR,BOUND,EPSABS,EPSREL,F,RESULT,WORK
      INTEGER IER,INF,IWORK,LAST,LENW,LIMIT,LVL,L1,L2,L3,NEVAL
C
      DIMENSION IWORK(LIMIT),WORK(LENW)
C
      EXTERNAL F
C
C         CHECK VALIDITY OF LIMIT AND LENW.
C
C***FIRST EXECUTABLE STATEMENT  DQAGI
      IER = 6
      NEVAL = 0
      LAST = 0
      RESULT = 0.0D+00
      ABSERR = 0.0D+00
      IF(LIMIT.LT.1.OR.LENW.LT.LIMIT*4) GO TO 10
C
C         PREPARE CALL FOR DQAGIE.
C
      L1 = LIMIT+1
      L2 = LIMIT+L1
      L3 = LIMIT+L2
C
      CALL DQAGIE(F,BOUND,INF,EPSABS,EPSREL,LIMIT,RESULT,ABSERR,
     1  NEVAL,IER,WORK(1),WORK(L1),WORK(L2),WORK(L3),IWORK,LAST)
C
C         CALL ERROR HANDLER IF NECESSARY.
C
       LVL = 0
10    IF(IER.EQ.6) LVL = 1
      IF(IER.NE.0) CALL XERROR( 'ABNORMAL RETURN FROM DQAGI',26,IER,LVL)
      RETURN
      END
      SUBROUTINE DQAGIE(F,BOUND,INF,EPSABS,EPSREL,LIMIT,RESULT,ABSERR,
     1   NEVAL,IER,ALIST,BLIST,RLIST,ELIST,IORD,LAST)
C***BEGIN PROLOGUE  DQAGIE
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***REVISION DATE  980526   (YYMMDD) Fixed documentation of parameter INF
C***CATEGORY NO.  H2A3A1,H2A4A1
C***KEYWORDS  AUTOMATIC INTEGRATOR,EXTRAPOLATION,GENERAL-PURPOSE,
C             GLOBALLY ADAPTIVE,INFINITE INTERVALS,TRANSFORMATION
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  The routine calculates an approximation result to a given
C            integral   I = Integral of F over (BOUND,+INFINITY)
C            or I = Integral of F over (-INFINITY,BOUND)
C            or I = Integral of F over (-INFINITY,+INFINITY),
C            hopefully satisfying following claim for accuracy
C            ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
C***DESCRIPTION
C
C Integration over infinite intervals
C Standard fortran subroutine
C
C            F      - Double precision
C                     Function subprogram defining the integrand
C                     function F(X). The actual name for F needs to be
C                     declared E X T E R N A L in the driver program.
C
C            BOUND  - Double precision
C                     Finite bound of integration range
C                     (has no meaning if interval is doubly-infinite)
C
C            INF    - Integer
C                     Indicating the kind of integration range involved
C                     INF = 1 corresponds to  (BOUND,+INFINITY),
C                     INF = -1            to  (-INFINITY,BOUND),
C                     INF = 2             to (-INFINITY,+INFINITY).
C
C            EPSABS - Double precision
C                     Absolute accuracy requested
C            EPSREL - Double precision
C                     Relative accuracy requested
C                     If  EPSABS.LE.0
C                     and EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
C                     the routine will end with IER = 6.
C
C            LIMIT  - Integer
C                     Gives an upper bound on the number of subintervals
C                     in the partition of (A,B), LIMIT.GE.1
C
C         ON RETURN
C            RESULT - Double precision
C                     Approximation to the integral
C
C            ABSERR - Double precision
C                     Estimate of the modulus of the absolute error,
C                     which should equal or exceed ABS(I-RESULT)
C
C            NEVAL  - Integer
C                     Number of integrand evaluations
C
C            IER    - Integer
C                     IER = 0 Normal and reliable termination of the
C                             routine. It is assumed that the requested
C                             accuracy has been achieved.
C                   - IER.GT.0 Abnormal termination of the routine. The
C                             estimates for result and error are less
C                             reliable. It is assumed that the requested
C                             accuracy has not been achieved.
C            ERROR MESSAGES
C                     IER = 1 Maximum number of subdivisions allowed
C                             has been achieved. One can allow more
C                             subdivisions by increasing the value of
C                             LIMIT (and taking the according dimension
C                             adjustments into account). However,if
C                             this yields no improvement it is advised
C                             to analyze the integrand in order to
C                             determine the integration difficulties.
C                             If the position of a local difficulty can
C                             be determined (e.g. SINGULARITY,
C                             DISCONTINUITY within the interval) one
C                             will probably gain from splitting up the
C                             interval at this point and calling the
C                             integrator on the subranges. If possible,
C                             an appropriate special-purpose integrator
C                             should be used, which is designed for
C                             handling the type of difficulty involved.
C                         = 2 The occurrence of roundoff error is
C                             detected, which prevents the requested
C                             tolerance from being achieved.
C                             The error may be under-estimated.
C                         = 3 Extremely bad integrand behaviour occurs
C                             at some points of the integration
C                             interval.
C                         = 4 The algorithm does not converge.
C                             Roundoff error is detected in the
C                             extrapolation table.
C                             It is assumed that the requested tolerance
C                             cannot be achieved, and that the returned
C                             result is the best which can be obtained.
C                         = 5 The integral is probably divergent, or
C                             slowly convergent. It must be noted that
C                             divergence can occur with any other value
C                             of IER.
C                         = 6 The input is invalid, because
C                             (EPSABS.LE.0 and
C                              EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
C                             RESULT, ABSERR, NEVAL, LAST, RLIST(1),
C                             ELIST(1) and IORD(1) are set to zero.
C                             ALIST(1) and BLIST(1) are set to 0
C                             and 1 respectively.
C
C            ALIST  - Double precision
C                     Vector of dimension at least LIMIT, the first
C                      LAST  elements of which are the left
C                     end points of the subintervals in the partition
C                     of the transformed integration range (0,1).
C
C            BLIST  - Double precision
C                     Vector of dimension at least LIMIT, the first
C                      LAST  elements of which are the right
C                     end points of the subintervals in the partition
C                     of the transformed integration range (0,1).
C
C            RLIST  - Double precision
C                     Vector of dimension at least LIMIT, the first
C                      LAST  elements of which are the integral
C                     approximations on the subintervals
C
C            ELIST  - Double precision
C                     Vector of dimension at least LIMIT,  the first
C                     LAST elements of which are the moduli of the
C                     absolute error estimates on the subintervals
C
C            IORD   - Integer
C                     Vector of dimension LIMIT, the first K
C                     elements of which are pointers to the
C                     error estimates over the subintervals,
C                     such that ELIST(IORD(1)), ..., ELIST(IORD(K))
C                     form a decreasing sequence, with K = LAST
C                     If LAST.LE.(LIMIT/2+2), and K = LIMIT+1-LAST
C                     otherwise
C
C            LAST   - Integer
C                     Number of subintervals actually produced
C                     in the subdivision process
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH,DQELG,DQK15I,DQPSRT
C***END PROLOGUE  DQAGIE
      DOUBLE PRECISION ABSEPS,ABSERR,ALIST,AREA,AREA1,AREA12,AREA2,A1,
     1  A2,BLIST,BOUN,BOUND,B1,B2,CORREC,DABS,DEFABS,DEFAB1,DEFAB2,
     2  DMAX1,DRES,D1MACH,ELIST,EPMACH,EPSABS,EPSREL,ERLARG,ERLAST,
     3  ERRBND,ERRMAX,ERROR1,ERROR2,ERRO12,ERRSUM,ERTEST,F,OFLOW,RESABS,
     4  RESEPS,RESULT,RES3LA,RLIST,RLIST2,SMALL,UFLOW
      INTEGER ID,IER,IERRO,INF,IORD,IROFF1,IROFF2,IROFF3,JUPBND,K,KSGN,
     1  KTMIN,LAST,LIMIT,MAXERR,NEVAL,NRES,NRMAX,NUMRL2
      LOGICAL EXTRAP,NOEXT
C
      DIMENSION ALIST(LIMIT),BLIST(LIMIT),ELIST(LIMIT),IORD(LIMIT),
     1  RES3LA(3),RLIST(LIMIT),RLIST2(52)
C
      EXTERNAL F
C
C            THE DIMENSION OF RLIST2 IS DETERMINED BY THE VALUE OF
C            LIMEXP IN SUBROUTINE DQELG.
C
C
C            LIST OF MAJOR VARIABLES
C            -----------------------
C
C           ALIST     - LIST OF LEFT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           BLIST     - LIST OF RIGHT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           RLIST(I)  - APPROXIMATION TO THE INTEGRAL OVER
C                       (ALIST(I),BLIST(I))
C           RLIST2    - ARRAY OF DIMENSION AT LEAST (LIMEXP+2),
C                       CONTAINING THE PART OF THE EPSILON TABLE
C                       WICH IS STILL NEEDED FOR FURTHER COMPUTATIONS
C           ELIST(I)  - ERROR ESTIMATE APPLYING TO RLIST(I)
C           MAXERR    - POINTER TO THE INTERVAL WITH LARGEST ERROR
C                       ESTIMATE
C           ERRMAX    - ELIST(MAXERR)
C           ERLAST    - ERROR ON THE INTERVAL CURRENTLY SUBDIVIDED
C                       (BEFORE THAT SUBDIVISION HAS TAKEN PLACE)
C           AREA      - SUM OF THE INTEGRALS OVER THE SUBINTERVALS
C           ERRSUM    - SUM OF THE ERRORS OVER THE SUBINTERVALS
C           ERRBND    - REQUESTED ACCURACY MAX(EPSABS,EPSREL*
C                       ABS(RESULT))
C           *****1    - VARIABLE FOR THE LEFT SUBINTERVAL
C           *****2    - VARIABLE FOR THE RIGHT SUBINTERVAL
C           LAST      - INDEX FOR SUBDIVISION
C           NRES      - NUMBER OF CALLS TO THE EXTRAPOLATION ROUTINE
C           NUMRL2    - NUMBER OF ELEMENTS CURRENTLY IN RLIST2. IF AN
C                       APPROPRIATE APPROXIMATION TO THE COMPOUNDED
C                       INTEGRAL HAS BEEN OBTAINED, IT IS PUT IN
C                       RLIST2(NUMRL2) AFTER NUMRL2 HAS BEEN INCREASED
C                       BY ONE.
C           SMALL     - LENGTH OF THE SMALLEST INTERVAL CONSIDERED UP
C                       TO NOW, MULTIPLIED BY 1.5
C           ERLARG    - SUM OF THE ERRORS OVER THE INTERVALS LARGER
C                       THAN THE SMALLEST INTERVAL CONSIDERED UP TO NOW
C           EXTRAP    - LOGICAL VARIABLE DENOTING THAT THE ROUTINE
C                       IS ATTEMPTING TO PERFORM EXTRAPOLATION. I.E.
C                       BEFORE SUBDIVIDING THE SMALLEST INTERVAL WE
C                       TRY TO DECREASE THE VALUE OF ERLARG.
C           NOEXT     - LOGICAL VARIABLE DENOTING THAT EXTRAPOLATION
C                       IS NO LONGER ALLOWED (TRUE-VALUE)
C
C            MACHINE DEPENDENT CONSTANTS
C            ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C           OFLOW IS THE LARGEST POSITIVE MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENT  DQAGIE
       EPMACH = D1MACH(4)
C
C           TEST ON VALIDITY OF PARAMETERS
C           -----------------------------
C
      IER = 0
      NEVAL = 0
      LAST = 0
      RESULT = 0.0D+00
      ABSERR = 0.0D+00
      ALIST(1) = 0.0D+00
      BLIST(1) = 0.1D+01
      RLIST(1) = 0.0D+00
      ELIST(1) = 0.0D+00
      IORD(1) = 0
      IF(EPSABS.LE.0.0D+00.AND.EPSREL.LT.DMAX1(0.5D+02*EPMACH,0.5D-28))
     1  IER = 6
       IF(IER.EQ.6) GO TO 999
C
C
C           FIRST APPROXIMATION TO THE INTEGRAL
C           -----------------------------------
C
C           DETERMINE THE INTERVAL TO BE MAPPED ONTO (0,1).
C           IF INF = 2 THE INTEGRAL IS COMPUTED AS I = I1+I2, WHERE
C           I1 = INTEGRAL OF F OVER (-INFINITY,0),
C           I2 = INTEGRAL OF F OVER (0,+INFINITY).
C
      BOUN = BOUND
      IF(INF.EQ.2) BOUN = 0.0D+00
      CALL DQK15I(F,BOUN,INF,0.0D+00,0.1D+01,RESULT,ABSERR,
     1  DEFABS,RESABS)
C
C           TEST ON ACCURACY
C
      LAST = 1
      RLIST(1) = RESULT
      ELIST(1) = ABSERR
      IORD(1) = 1
      DRES = DABS(RESULT)
      ERRBND = DMAX1(EPSABS,EPSREL*DRES)
      IF(ABSERR.LE.1.0D+02*EPMACH*DEFABS.AND.ABSERR.GT.ERRBND) IER = 2
      IF(LIMIT.EQ.1) IER = 1
      IF(IER.NE.0.OR.(ABSERR.LE.ERRBND.AND.ABSERR.NE.RESABS).OR.
     1  ABSERR.EQ.0.0D+00) GO TO 130
C
C           INITIALIZATION
C           --------------
C
      UFLOW = D1MACH(1)
      OFLOW = D1MACH(2)
      RLIST2(1) = RESULT
      ERRMAX = ABSERR
      MAXERR = 1
      AREA = RESULT
      ERRSUM = ABSERR
      ABSERR = OFLOW
      NRMAX = 1
      NRES = 0
      KTMIN = 0
      NUMRL2 = 2
      EXTRAP = .FALSE.
      NOEXT = .FALSE.
      IERRO = 0
      IROFF1 = 0
      IROFF2 = 0
      IROFF3 = 0
      KSGN = -1
      IF(DRES.GE.(0.1D+01-0.5D+02*EPMACH)*DEFABS) KSGN = 1
C
C           MAIN DO-LOOP
C           ------------
C
      DO 90 LAST = 2,LIMIT
C
C           BISECT THE SUBINTERVAL WITH NRMAX-TH LARGEST ERROR ESTIMATE.
C
        A1 = ALIST(MAXERR)
        B1 = 0.5D+00*(ALIST(MAXERR)+BLIST(MAXERR))
        A2 = B1
        B2 = BLIST(MAXERR)
        ERLAST = ERRMAX
        CALL DQK15I(F,BOUN,INF,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
        CALL DQK15I(F,BOUN,INF,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
C
C           IMPROVE PREVIOUS APPROXIMATIONS TO INTEGRAL
C           AND ERROR AND TEST FOR ACCURACY.
C
        AREA12 = AREA1+AREA2
        ERRO12 = ERROR1+ERROR2
        ERRSUM = ERRSUM+ERRO12-ERRMAX
        AREA = AREA+AREA12-RLIST(MAXERR)
        IF(DEFAB1.EQ.ERROR1.OR.DEFAB2.EQ.ERROR2)GO TO 15
        IF(DABS(RLIST(MAXERR)-AREA12).GT.0.1D-04*DABS(AREA12)
     1  .OR.ERRO12.LT.0.99D+00*ERRMAX) GO TO 10
        IF(EXTRAP) IROFF2 = IROFF2+1
        IF(.NOT.EXTRAP) IROFF1 = IROFF1+1
   10   IF(LAST.GT.10.AND.ERRO12.GT.ERRMAX) IROFF3 = IROFF3+1
   15   RLIST(MAXERR) = AREA1
        RLIST(LAST) = AREA2
        ERRBND = DMAX1(EPSABS,EPSREL*DABS(AREA))
C
C           TEST FOR ROUNDOFF ERROR AND EVENTUALLY SET ERROR FLAG.
C
        IF(IROFF1+IROFF2.GE.10.OR.IROFF3.GE.20) IER = 2
        IF(IROFF2.GE.5) IERRO = 3
C
C           SET ERROR FLAG IN THE CASE THAT THE NUMBER OF
C           SUBINTERVALS EQUALS LIMIT.
C
        IF(LAST.EQ.LIMIT) IER = 1
C
C           SET ERROR FLAG IN THE CASE OF BAD INTEGRAND BEHAVIOUR
C           AT SOME POINTS OF THE INTEGRATION RANGE.
C
        IF(DMAX1(DABS(A1),DABS(B2)).LE.(0.1D+01+0.1D+03*EPMACH)*
     1  (DABS(A2)+0.1D+04*UFLOW)) IER = 4
C
C           APPEND THE NEWLY-CREATED INTERVALS TO THE LIST.
C
        IF(ERROR2.GT.ERROR1) GO TO 20
        ALIST(LAST) = A2
        BLIST(MAXERR) = B1
        BLIST(LAST) = B2
        ELIST(MAXERR) = ERROR1
        ELIST(LAST) = ERROR2
        GO TO 30
   20   ALIST(MAXERR) = A2
        ALIST(LAST) = A1
        BLIST(LAST) = B1
        RLIST(MAXERR) = AREA2
        RLIST(LAST) = AREA1
        ELIST(MAXERR) = ERROR2
        ELIST(LAST) = ERROR1
C
C           CALL SUBROUTINE DQPSRT TO MAINTAIN THE DESCENDING ORDERING
C           IN THE LIST OF ERROR ESTIMATES AND SELECT THE SUBINTERVAL
C           WITH NRMAX-TH LARGEST ERROR ESTIMATE (TO BE BISECTED NEXT).
C
   30   CALL DQPSRT(LIMIT,LAST,MAXERR,ERRMAX,ELIST,IORD,NRMAX)
        IF(ERRSUM.LE.ERRBND) GO TO 115
        IF(IER.NE.0) GO TO 100
        IF(LAST.EQ.2) GO TO 80
        IF(NOEXT) GO TO 90
        ERLARG = ERLARG-ERLAST
        IF(DABS(B1-A1).GT.SMALL) ERLARG = ERLARG+ERRO12
        IF(EXTRAP) GO TO 40
C
C           TEST WHETHER THE INTERVAL TO BE BISECTED NEXT IS THE
C           SMALLEST INTERVAL.
C
        IF(DABS(BLIST(MAXERR)-ALIST(MAXERR)).GT.SMALL) GO TO 90
        EXTRAP = .TRUE.
        NRMAX = 2
   40   IF(IERRO.EQ.3.OR.ERLARG.LE.ERTEST) GO TO 60
C
C           THE SMALLEST INTERVAL HAS THE LARGEST ERROR.
C           BEFORE BISECTING DECREASE THE SUM OF THE ERRORS OVER THE
C           LARGER INTERVALS (ERLARG) AND PERFORM EXTRAPOLATION.
C
        ID = NRMAX
        JUPBND = LAST
        IF(LAST.GT.(2+LIMIT/2)) JUPBND = LIMIT+3-LAST
        DO 50 K = ID,JUPBND
          MAXERR = IORD(NRMAX)
          ERRMAX = ELIST(MAXERR)
          IF(DABS(BLIST(MAXERR)-ALIST(MAXERR)).GT.SMALL) GO TO 90
          NRMAX = NRMAX+1
   50   CONTINUE
C
C           PERFORM EXTRAPOLATION.
C
   60   NUMRL2 = NUMRL2+1
        RLIST2(NUMRL2) = AREA
        CALL DQELG(NUMRL2,RLIST2,RESEPS,ABSEPS,RES3LA,NRES)
        KTMIN = KTMIN+1
        IF(KTMIN.GT.5.AND.ABSERR.LT.0.1D-02*ERRSUM) IER = 5
        IF(ABSEPS.GE.ABSERR) GO TO 70
        KTMIN = 0
        ABSERR = ABSEPS
        RESULT = RESEPS
        CORREC = ERLARG
        ERTEST = DMAX1(EPSABS,EPSREL*DABS(RESEPS))
        IF(ABSERR.LE.ERTEST) GO TO 100
C
C            PREPARE BISECTION OF THE SMALLEST INTERVAL.
C
   70   IF(NUMRL2.EQ.1) NOEXT = .TRUE.
        IF(IER.EQ.5) GO TO 100
        MAXERR = IORD(1)
        ERRMAX = ELIST(MAXERR)
        NRMAX = 1
        EXTRAP = .FALSE.
        SMALL = SMALL*0.5D+00
        ERLARG = ERRSUM
        GO TO 90
   80   SMALL = 0.375D+00
        ERLARG = ERRSUM
        ERTEST = ERRBND
        RLIST2(2) = AREA
   90 CONTINUE
C
C           SET FINAL RESULT AND ERROR ESTIMATE.
C           ------------------------------------
C
  100 IF(ABSERR.EQ.OFLOW) GO TO 115
      IF((IER+IERRO).EQ.0) GO TO 110
      IF(IERRO.EQ.3) ABSERR = ABSERR+CORREC
      IF(IER.EQ.0) IER = 3
      IF(RESULT.NE.0.0D+00.AND.AREA.NE.0.0D+00)GO TO 105
      IF(ABSERR.GT.ERRSUM)GO TO 115
      IF(AREA.EQ.0.0D+00) GO TO 130
      GO TO 110
  105 IF(ABSERR/DABS(RESULT).GT.ERRSUM/DABS(AREA))GO TO 115
C
C           TEST ON DIVERGENCE
C
  110 IF(KSGN.EQ.(-1).AND.DMAX1(DABS(RESULT),DABS(AREA)).LE.
     1 DEFABS*0.1D-01) GO TO 130
      IF(0.1D-01.GT.(RESULT/AREA).OR.(RESULT/AREA).GT.0.1D+03.
     1OR.ERRSUM.GT.DABS(AREA)) IER = 6
      GO TO 130
C
C           COMPUTE GLOBAL INTEGRAL SUM.
C
  115 RESULT = 0.0D+00
      DO 120 K = 1,LAST
        RESULT = RESULT+RLIST(K)
  120 CONTINUE
      ABSERR = ERRSUM
  130 NEVAL = 30*LAST-15
      IF(INF.EQ.2) NEVAL = 2*NEVAL
      IF(IER.GT.2) IER=IER-1
  999 RETURN
      END
      SUBROUTINE DQELG(N,EPSTAB,RESULT,ABSERR,RES3LA,NRES)
C***BEGIN PROLOGUE  DQELG
C***REFER TO  DQAGIE,DQAGOE,DQAGPE,DQAGSE
C***ROUTINES CALLED  D1MACH
C***REVISION DATE  830518   (YYMMDD)
C***KEYWORDS  CONVERGENCE ACCELERATION,EPSILON ALGORITHM,EXTRAPOLATION
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  The routine determines the limit of a given sequence of
C            approximations, by means of the Epsilon algorithm of
C            P.Wynn. An estimate of the absolute error is also given.
C            The condensed Epsilon table is computed. Only those
C            elements needed for the computation of the next diagonal
C            are preserved.
C***DESCRIPTION
C
C           Epsilon algorithm
C           Standard fortran subroutine
C           Double precision version
C
C           PARAMETERS
C              N      - Integer
C                       EPSTAB(N) contains the new element in the
C                       first column of the epsilon table.
C
C              EPSTAB - Double precision
C                       Vector of dimension 52 containing the elements
C                       of the two lower diagonals of the triangular
C                       epsilon table. The elements are numbered
C                       starting at the right-hand corner of the
C                       triangle.
C
C              RESULT - Double precision
C                       Resulting approximation to the integral
C
C              ABSERR - Double precision
C                       Estimate of the absolute error computed from
C                       RESULT and the 3 previous results
C
C              RES3LA - Double precision
C                       Vector of dimension 3 containing the last 3
C                       results
C
C              NRES   - Integer
C                       Number of calls to the routine
C                       (should be zero at first call)
C***END PROLOGUE  DQELG
C
      DOUBLE PRECISION ABSERR,DABS,DELTA1,DELTA2,DELTA3,DMAX1,D1MACH,
     1  EPMACH,EPSINF,EPSTAB,ERROR,ERR1,ERR2,ERR3,E0,E1,E1ABS,E2,E3,
     2  OFLOW,RES,RESULT,RES3LA,SS,TOL1,TOL2,TOL3
      INTEGER I,IB,IB2,IE,INDX,K1,K2,K3,LIMEXP,N,NEWELM,NRES,NUM
      DIMENSION EPSTAB(52),RES3LA(3)
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C
C           E0     - THE 4 ELEMENTS ON WHICH THE COMPUTATION OF A NEW
C           E1       ELEMENT IN THE EPSILON TABLE IS BASED
C           E2
C           E3                 E0
C                        E3    E1    NEW
C                              E2
C           NEWELM - NUMBER OF ELEMENTS TO BE COMPUTED IN THE NEW
C                    DIAGONAL
C           ERROR  - ERROR = ABS(E1-E0)+ABS(E2-E1)+ABS(NEW-E2)
C           RESULT - THE ELEMENT IN THE NEW DIAGONAL WITH LEAST VALUE
C                    OF ERROR
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           OFLOW IS THE LARGEST POSITIVE MAGNITUDE.
C           LIMEXP IS THE MAXIMUM NUMBER OF ELEMENTS THE EPSILON
C           TABLE CAN CONTAIN. IF THIS NUMBER IS REACHED, THE UPPER
C           DIAGONAL OF THE EPSILON TABLE IS DELETED.
C
C***FIRST EXECUTABLE STATEMENT  DQELG
      EPMACH = D1MACH(4)
      OFLOW = D1MACH(2)
      NRES = NRES+1
      ABSERR = OFLOW
      RESULT = EPSTAB(N)
      IF(N.LT.3) GO TO 100
      LIMEXP = 50
      EPSTAB(N+2) = EPSTAB(N)
      NEWELM = (N-1)/2
      EPSTAB(N) = OFLOW
      NUM = N
      K1 = N
      DO 40 I = 1,NEWELM
        K2 = K1-1
        K3 = K1-2
        RES = EPSTAB(K1+2)
        E0 = EPSTAB(K3)
        E1 = EPSTAB(K2)
        E2 = RES
        E1ABS = DABS(E1)
        DELTA2 = E2-E1
        ERR2 = DABS(DELTA2)
        TOL2 = DMAX1(DABS(E2),E1ABS)*EPMACH
        DELTA3 = E1-E0
        ERR3 = DABS(DELTA3)
        TOL3 = DMAX1(E1ABS,DABS(E0))*EPMACH
        IF(ERR2.GT.TOL2.OR.ERR3.GT.TOL3) GO TO 10
C
C           IF E0, E1 AND E2 ARE EQUAL TO WITHIN MACHINE
C           ACCURACY, CONVERGENCE IS ASSUMED.
C           RESULT = E2
C           ABSERR = ABS(E1-E0)+ABS(E2-E1)
C
        RESULT = RES
        ABSERR = ERR2+ERR3
C ***JUMP OUT OF DO-LOOP
        GO TO 100
   10   E3 = EPSTAB(K1)
        EPSTAB(K1) = E1
        DELTA1 = E1-E3
        ERR1 = DABS(DELTA1)
        TOL1 = DMAX1(E1ABS,DABS(E3))*EPMACH
C
C           IF TWO ELEMENTS ARE VERY CLOSE TO EACH OTHER, OMIT
C           A PART OF THE TABLE BY ADJUSTING THE VALUE OF N
C
        IF(ERR1.LE.TOL1.OR.ERR2.LE.TOL2.OR.ERR3.LE.TOL3) GO TO 20
        SS = 0.1D+01/DELTA1+0.1D+01/DELTA2-0.1D+01/DELTA3
        EPSINF = DABS(SS*E1)
C
C           TEST TO DETECT IRREGULAR BEHAVIOUR IN THE TABLE, AND
C           EVENTUALLY OMIT A PART OF THE TABLE ADJUSTING THE VALUE
C           OF N.
C
        IF(EPSINF.GT.0.1D-03) GO TO 30
   20   N = I+I-1
C ***JUMP OUT OF DO-LOOP
        GO TO 50
C
C           COMPUTE A NEW ELEMENT AND EVENTUALLY ADJUST
C           THE VALUE OF RESULT.
C
   30   RES = E1+0.1D+01/SS
        EPSTAB(K1) = RES
        K1 = K1-2
        ERROR = ERR2+DABS(RES-E2)+ERR3
        IF(ERROR.GT.ABSERR) GO TO 40
        ABSERR = ERROR
        RESULT = RES
   40 CONTINUE
C
C           SHIFT THE TABLE.
C
   50 IF(N.EQ.LIMEXP) N = 2*(LIMEXP/2)-1
      IB = 1
      IF((NUM/2)*2.EQ.NUM) IB = 2
      IE = NEWELM+1
      DO 60 I=1,IE
        IB2 = IB+2
        EPSTAB(IB) = EPSTAB(IB2)
        IB = IB2
   60 CONTINUE
      IF(NUM.EQ.N) GO TO 80
      INDX = NUM-N+1
      DO 70 I = 1,N
        EPSTAB(I)= EPSTAB(INDX)
        INDX = INDX+1
   70 CONTINUE
   80 IF(NRES.GE.4) GO TO 90
      RES3LA(NRES) = RESULT
      ABSERR = OFLOW
      GO TO 100
C
C           COMPUTE ERROR ESTIMATE
C
   90 ABSERR = DABS(RESULT-RES3LA(3))+DABS(RESULT-RES3LA(2))
     1  +DABS(RESULT-RES3LA(1))
      RES3LA(1) = RES3LA(2)
      RES3LA(2) = RES3LA(3)
      RES3LA(3) = RESULT
  100 ABSERR = DMAX1(ABSERR,0.5D+01*EPMACH*DABS(RESULT))
      RETURN
      END
      SUBROUTINE DQK15I(F,BOUN,INF,A,B,RESULT,ABSERR,RESABS,RESASC)
C***BEGIN PROLOGUE  DQK15I
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***CATEGORY NO.  H2A3A2,H2A4A2
C***KEYWORDS  15-POINT TRANSFORMED GAUSS-KRONROD RULES
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  The original (infinite integration range is mapped
C            onto the interval (0,1) and (A,B) is a part of (0,1).
C            it is the purpose to compute
C            I = Integral of transformed integrand over (A,B),
C            J = Integral of ABS(Transformed Integrand) over (A,B).
C***DESCRIPTION
C
C           Integration Rule
C           Standard Fortran subroutine
C           Double precision version
C
C           PARAMETERS
C            ON ENTRY
C              F      - Double precision
C                       Fuction subprogram defining the integrand
C                       FUNCTION F(X). The actual name for F needs to be
C                       Declared E X T E R N A L in the calling program.
C
C              BOUN   - Double precision
C                       Finite bound of original integration
C                       Range (SET TO ZERO IF INF = +2)
C
C              INF    - Integer
C                       If INF = -1, the original interval is
C                                   (-INFINITY,BOUND),
C                       If INF = +1, the original interval is
C                                   (BOUND,+INFINITY),
C                       If INF = +2, the original interval is
C                                   (-INFINITY,+INFINITY) AND
C                       The integral is computed as the sum of two
C                       integrals, one over (-INFINITY,0) and one over
C                       (0,+INFINITY).
C
C              A      - Double precision
C                       Lower limit for integration over subrange
C                       of (0,1)
C
C              B      - Double precision
C                       Upper limit for integration over subrange
C                       of (0,1)
C
C            ON RETURN
C              RESULT - Double precision
C                       Approximation to the integral I
C                       Result is computed by applying the 15-POINT
C                       KRONROD RULE(RESK) obtained by optimal addition
C                       of abscissae to the 7-POINT GAUSS RULE(RESG).
C
C              ABSERR - Double precision
C                       Estimate of the modulus of the absolute error,
C                       WHICH SHOULD EQUAL or EXCEED ABS(I-RESULT)
C
C              RESABS - Double precision
C                       Approximation to the integral J
C
C              RESASC - Double precision
C                       Approximation to the integral of
C                       ABS((TRANSFORMED INTEGRAND)-I/(B-A)) over (A,B)
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH
C***END PROLOGUE  DQK15I
C
      DOUBLE PRECISION A,ABSC,ABSC1,ABSC2,ABSERR,B,BOUN,CENTR,DABS,DINF,
     1  DMAX1,DMIN1,D1MACH,EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,
     2  RESABS,RESASC,RESG,RESK,RESKH,RESULT,TABSC1,TABSC2,UFLOW,WG,WGK,
     3  XGK
      INTEGER INF,J
      EXTERNAL F
C
      DIMENSION FV1(7),FV2(7),XGK(8),WGK(8),WG(8)
C
C           THE ABSCISSAE AND WEIGHTS ARE SUPPLIED FOR THE INTERVAL
C           (-1,1).  BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND
C           THEIR CORRESPONDING WEIGHTS ARE GIVEN.
C
C           XGK    - ABSCISSAE OF THE 15-POINT KRONROD RULE
C                    XGK(2), XGK(4), ... ABSCISSAE OF THE 7-POINT
C                    GAUSS RULE
C                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
C                    ADDED TO THE 7-POINT GAUSS RULE
C
C           WGK    - WEIGHTS OF THE 15-POINT KRONROD RULE
C
C           WG     - WEIGHTS OF THE 7-POINT GAUSS RULE, CORRESPONDING
C                    TO THE ABSCISSAE XGK(2), XGK(4), ...
C                    WG(1), WG(3), ... ARE SET TO ZERO.
C
      DATA XGK(1),XGK(2),XGK(3),XGK(4),XGK(5),XGK(6),XGK(7),XGK(8)/
     1     0.9914553711208126D+00,     0.9491079123427585D+00,
     2     0.8648644233597691D+00,     0.7415311855993944D+00,
     3     0.5860872354676911D+00,     0.4058451513773972D+00,
     4     0.2077849550078985D+00,     0.0000000000000000D+00/
C
      DATA WGK(1),WGK(2),WGK(3),WGK(4),WGK(5),WGK(6),WGK(7),WGK(8)/
     1     0.2293532201052922D-01,     0.6309209262997855D-01,
     2     0.1047900103222502D+00,     0.1406532597155259D+00,
     3     0.1690047266392679D+00,     0.1903505780647854D+00,
     4     0.2044329400752989D+00,     0.2094821410847278D+00/
C
      DATA WG(1),WG(2),WG(3),WG(4),WG(5),WG(6),WG(7),WG(8)/
     1     0.0000000000000000D+00,     0.1294849661688697D+00,
     2     0.0000000000000000D+00,     0.2797053914892767D+00,
     3     0.0000000000000000D+00,     0.3818300505051189D+00,
     4     0.0000000000000000D+00,     0.4179591836734694D+00/
C
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C
C           CENTR  - MID POINT OF THE INTERVAL
C           HLGTH  - HALF-LENGTH OF THE INTERVAL
C           ABSC*  - ABSCISSA
C           TABSC* - TRANSFORMED ABSCISSA
C           FVAL*  - FUNCTION VALUE
C           RESG   - RESULT OF THE 7-POINT GAUSS FORMULA
C           RESK   - RESULT OF THE 15-POINT KRONROD FORMULA
C           RESKH  - APPROXIMATION TO THE MEAN VALUE OF THE TRANSFORMED
C                    INTEGRAND OVER (A,B), I.E. TO I/(B-A)
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENT  DQK15I
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
      DINF = MIN0(1,INF)
C
      CENTR = 0.5D+00*(A+B)
      HLGTH = 0.5D+00*(B-A)
      TABSC1 = BOUN+DINF*(0.1D+01-CENTR)/CENTR
      FVAL1 = F(TABSC1)
      IF(INF.EQ.2) FVAL1 = FVAL1+F(-TABSC1)
      FC = (FVAL1/CENTR)/CENTR
C
C           COMPUTE THE 15-POINT KRONROD APPROXIMATION TO
C           THE INTEGRAL, AND ESTIMATE THE ERROR.
C
      RESG = WG(8)*FC
      RESK = WGK(8)*FC
      RESABS = DABS(RESK)
      DO 10 J=1,7
        ABSC = HLGTH*XGK(J)
        ABSC1 = CENTR-ABSC
        ABSC2 = CENTR+ABSC
        TABSC1 = BOUN+DINF*(0.1D+01-ABSC1)/ABSC1
        TABSC2 = BOUN+DINF*(0.1D+01-ABSC2)/ABSC2
        FVAL1 = F(TABSC1)
        FVAL2 = F(TABSC2)
        IF(INF.EQ.2) FVAL1 = FVAL1+F(-TABSC1)
        IF(INF.EQ.2) FVAL2 = FVAL2+F(-TABSC2)
        FVAL1 = (FVAL1/ABSC1)/ABSC1
        FVAL2 = (FVAL2/ABSC2)/ABSC2
        FV1(J) = FVAL1
        FV2(J) = FVAL2
        FSUM = FVAL1+FVAL2
        RESG = RESG+WG(J)*FSUM
        RESK = RESK+WGK(J)*FSUM
        RESABS = RESABS+WGK(J)*(DABS(FVAL1)+DABS(FVAL2))
   10 CONTINUE
      RESKH = RESK*0.5D+00
      RESASC = WGK(8)*DABS(FC-RESKH)
      DO 20 J=1,7
        RESASC = RESASC+WGK(J)*(DABS(FV1(J)-RESKH)+DABS(FV2(J)-RESKH))
   20 CONTINUE
      RESULT = RESK*HLGTH
      RESASC = RESASC*HLGTH
      RESABS = RESABS*HLGTH
      ABSERR = DABS((RESK-RESG)*HLGTH)
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.D0) ABSERR = RESASC*
     1 DMIN1(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = DMAX1
     1 ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END
      SUBROUTINE DQPSRT(LIMIT,LAST,MAXERR,ERMAX,ELIST,IORD,NRMAX)
C***BEGIN PROLOGUE  DQPSRT
C***REFER TO  DQAGE,DQAGIE,DQAGPE,DQAWSE
C***ROUTINES CALLED  (NONE)
C***REVISION DATE  810101   (YYMMDD)
C***KEYWORDS  SEQUENTIAL SORTING
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  This routine maintains the descending ordering in the
C            list of the local error estimated resulting from the
C            interval subdivision process. At each call two error
C            estimates are inserted using the sequential search
C            method, top-down for the largest error estimate and
C            bottom-up for the smallest error estimate.
C***DESCRIPTION
C
C           Ordering routine
C           Standard fortran subroutine
C           Double precision version
C
C           PARAMETERS (MEANING AT OUTPUT)
C              LIMIT  - Integer
C                       Maximum number of error estimates the list
C                       can contain
C
C              LAST   - Integer
C                       Number of error estimates currently in the list
C
C              MAXERR - Integer
C                       Maxerr points to the NRMAX-th largest error
C                       estimate currently in the list
C
C              ERMAX  - Double precision
C                       NRMAX-th largest error estimate
C                       ERMAX = ELIST(MAXERR)
C
C              ELIST  - Double precision
C                       Vector of dimension LAST containing
C                       the error estimates
C
C              IORD   - Integer
C                       Vector of dimension LAST, the first K elements
C                       of which contain pointers to the error
C                       estimates, such that
C                       ELIST(IORD(1)),...,  ELIST(IORD(K))
C                       form a decreasing sequence, with
C                       K = LAST if LAST.LE.(LIMIT/2+2), and
C                       K = LIMIT+1-LAST otherwise
C
C              NRMAX  - Integer
C                       MAXERR = IORD(NRMAX)
C***END PROLOGUE  DQPSRT
C
      DOUBLE PRECISION ELIST,ERMAX,ERRMAX,ERRMIN
      INTEGER I,IBEG,IDO,IORD,ISUCC,J,JBND,JUPBN,K,LAST,LIMIT,MAXERR,
     1  NRMAX
      DIMENSION ELIST(LAST),IORD(LAST)
C
C           CHECK WHETHER THE LIST CONTAINS MORE THAN
C           TWO ERROR ESTIMATES.
C
C***FIRST EXECUTABLE STATEMENT  DQPSRT
      IF(LAST.GT.2) GO TO 10
      IORD(1) = 1
      IORD(2) = 2
      GO TO 90
C
C           THIS PART OF THE ROUTINE IS ONLY EXECUTED IF, DUE TO A
C           DIFFICULT INTEGRAND, SUBDIVISION INCREASED THE ERROR
C           ESTIMATE. IN THE NORMAL CASE THE INSERT PROCEDURE SHOULD
C           START AFTER THE NRMAX-TH LARGEST ERROR ESTIMATE.
C
   10 ERRMAX = ELIST(MAXERR)
      IF(NRMAX.EQ.1) GO TO 30
      IDO = NRMAX-1
      DO 20 I = 1,IDO
        ISUCC = IORD(NRMAX-1)
C ***JUMP OUT OF DO-LOOP
        IF(ERRMAX.LE.ELIST(ISUCC)) GO TO 30
        IORD(NRMAX) = ISUCC
        NRMAX = NRMAX-1
   20    CONTINUE
C
C           COMPUTE THE NUMBER OF ELEMENTS IN THE LIST TO BE MAINTAINED
C           IN DESCENDING ORDER. THIS NUMBER DEPENDS ON THE NUMBER OF
C           SUBDIVISIONS STILL ALLOWED.
C
   30 JUPBN = LAST
      IF(LAST.GT.(LIMIT/2+2)) JUPBN = LIMIT+3-LAST
      ERRMIN = ELIST(LAST)
C
C           INSERT ERRMAX BY TRAVERSING THE LIST TOP-DOWN,
C           STARTING COMPARISON FROM THE ELEMENT ELIST(IORD(NRMAX+1)).
C
      JBND = JUPBN-1
      IBEG = NRMAX+1
      IF(IBEG.GT.JBND) GO TO 50
      DO 40 I=IBEG,JBND
        ISUCC = IORD(I)
C ***JUMP OUT OF DO-LOOP
        IF(ERRMAX.GE.ELIST(ISUCC)) GO TO 60
        IORD(I-1) = ISUCC
   40 CONTINUE
   50 IORD(JBND) = MAXERR
      IORD(JUPBN) = LAST
      GO TO 90
C
C           INSERT ERRMIN BY TRAVERSING THE LIST BOTTOM-UP.
C
   60 IORD(I-1) = MAXERR
      K = JBND
      DO 70 J=I,JBND
        ISUCC = IORD(K)
C ***JUMP OUT OF DO-LOOP
        IF(ERRMIN.LT.ELIST(ISUCC)) GO TO 80
        IORD(K+1) = ISUCC
        K = K-1
   70 CONTINUE
      IORD(I) = LAST
      GO TO 90
   80 IORD(K+1) = LAST
C
C           SET MAXERR AND ERMAX.
C
   90 MAXERR = IORD(NRMAX)
      ERMAX = ELIST(MAXERR)
      RETURN
      END
      
C
C        MODIFED FOR DOUBLE PRECISION (TAC....4/20/99)
C
* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module ZEROIN from package CMLIB.
* Retrieved from ARNO on Mon Apr 19 09:57:35 1999.
* ======================================================================
      SUBROUTINE DZEROIN(F,B,C,RE,AE,IFLAG)
C
C     SANDIA MATHEMATICAL PROGRAM LIBRARY
C     MATHEMATICAL COMPUTING SERVICES DIVISION 5422
C     SANDIA LABORATORIES
C     P. O. BOX 5800
C     ALBUQUERQUE, NEW MEXICO  87115
C     CONTROL DATA 6600 VERSION 4.5, 1 NOVEMBER 1971
C
C     MODIFIED TO RUN AT NBS BY D. KAHANER DIV 713
C        MODIFED FOR DOUBLE PRECISION (TA COHN....4/20/99)
C
C     ABSTRACT
C        ZEROIN SEARCHES FOR A ZERO OF A FUNCTION F(X) BETWEEN
C        THE GIVEN VALUES B AND C UNTIL THE WIDTH OF THE INTERVAL
C        (B,C) HAS COLLAPSED TO WITHIN A TOLERANCE SPECIFIED BY
C        THE STOPPING CRITERION, ABS(B-C) .LE. 2.*(RW*ABS(B)+AE).
C
C     DESCRIPTION OF ARGUMENTS
C        F     - NAME OF THE REAL VALUED EXTERNAL FUNCTION.  THIS NAME
C                MUST BE IN AN EXTERNAL STATEMENT IN THE CALLING
C                PROGRAM.  F MUST BE A FUNCTION OF ONE REAL ARGUMENT.
C        B     - ONE END OF THE INTERVAL (B,C).  THE VALUE RETURNED FOR
C                B USUALLY IS THE BETTER APPROXIMATION TO A ZERO OF F.
C        C     - THE OTHER END OF THE INTERVAL (B,C)
C        RE    - RELATIVE ERROR USED FOR RW IN THE STOPPING CRITERION.
C                IF THE REQUESTED RE IS LESS THAN MACHINE PRECISION,
C                THEN RW IS SET TO APPROXIMATELY MACHINE PRECISION.
C        AE    - ABSOLUTE ERROR USED IN THE STOPPING CRITERION.  IF THE
C                GIVEN INTERVAL (B,C) CONTAINS THE ORIGIN, THEN A
C                NONZERO VALUE SHOULD BE CHOSEN FOR AE.
C        IFLAG - RETURNS A STATUS OF THE RESULTS INDICATING WHICH
C                OF THE FOLLOWING CONDITIONS HOLD.
C                    A - ABS(B-C) .LE. 2.*(RW*ABS(B)+AE)
C                    B - F(B) * F(C) .LT. 0.
C                    C - ABS(F(B)) .LE. ABS(F(C))
C                    D - ABS(F(B   )) .LE. MAX(ABS(F(B  )),ABS(F(C  )))
C                               OUT                   IN          IN
C                    E - NUMBER OF EVALUATIONS OF F(X) .LE. 500
C               =1 INDICATES NORMAL CASE.  ALL CONDITIONS ABOVE HOLD.
C               =2 INDICATES F(B) = 0.  CONDITION A MAY NOT HOLD.
C               =3 INDICATES CONDITIONS A, B, C, AND E HOLD BUT D DOES
C                  NOT.  (B,C) PROBABLY CONTAINS A SINGULAR POINT OF F.
C               =4 INDICATES CONDITIONS A AND E HOLD BUT B DOES NOT.
C                  A LOCAL MINIMUM OF F(X) IN (B,C) MAY HAVE BEEN FOUND.
C               =5 INDICATES SEARCH WAS ABORTED WHEN CONDITION E FAILED.
C
C     REFERENCES
C       1.  L F SHAMPINE AND H A WATTS, ZEROIN, A ROOT-SOLVING CODE,
C           SC-TM-70-631, SEPT 1970.
C       2.  T J DEKKER, FINDING A ZERO BY MEANS OF SUCCESSIVE LINEAR
C           INTERPOLATION, *CONSTRUCTIVE ASPECTS OF THE FUNDAMENTAL
C           THEOREM OF ALGEBRA*, EDITED BY B DEJON AND P HENRICI, 1969.
C
C     INITIALIZE
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

      DATA ER/0.D0/
      IF (ER .EQ. 0.D00) ER = 4.D0 * (D1MACH (4) )
      RW=MAX(RE,ER)
      IC=0
      ACBS=ABS(B-C)
      A=C
      FA=F(A)
      FB=F(B)
      FC=FA
      KOUNT=2
      FX=MAX(ABS(FB),ABS(FC))
C
    1 IF (ABS(FC) .GE. ABS(FB)) GO TO 2
C     PERFORM INTERCHANGE
      A=B
      FA=FB
      B=C
      FB=FC
      C=A
      FC=FA
C
    2 CMB=0.5D0*(C-B)
      ACMB=ABS(CMB)
      TOL=RW*ABS(B)+AE
C
C     TEST STOPPING CRITERION
      IF (ACMB .LE. TOL) GO TO 10
C
C     CALCULATE NEW ITERATE IMPLICITLY AS B+P/Q
C     WHERE WE ARRANGE P .GE. 0.
C     THE IMPLICIT FORM IS USED TO PREVENT OVERFLOW.
      P=(B-A)*FB
      Q=FA-FB
      IF (P .GE. 0.D0) GO TO 3
      P=-P
      Q=-Q
C
C     UPDATE A AND CHECK FOR SATISFACTORY REDUCTION
C     IN THE SIZE OF OUR BOUNDING INTERVAL.
    3 A=B
      FA=FB
      IC=IC+1
      IF (IC .LT. 4) GO TO 4
      IF (8.D0*ACMB .GE. ACBS) GO TO 6
      IC=0
      ACBS=ACMB
C
C     TEST FOR TOO SMALL A CHANGE
    4 IF (P .GT. ABS(Q)*TOL) GO TO 5
C
C     INCREMENT BY TOLERANCE
      B=B+DSIGN(TOL,CMB)
      GO TO 7
C
C     ROOT OUGHT TO BE BETWEEN B AND (C+B)/2.
    5 IF (P .GE. CMB*Q) GO TO 6
C
C     INTERPOLATE
      B=B+P/Q
      GO TO 7
C
    6 B=0.5D0*(C+B)
C     BISECT
C
C     HAVE COMPLETED COMPUTATION FOR NEW ITERATE B
    7 FB=F(B)
      IF (FB .EQ. 0.D0) GO TO 11
      KOUNT=KOUNT+1
      IF (KOUNT .GT. 500) GO TO 15
C
C     DECIDE WHETHER NEXT STEP IS INTERPOLATION OR EXTRAPOLATION
      IF (DSIGN(1.D0,FB) .NE. DSIGN(1.D0,FC)) GO TO 1
      C=A
      FC=FA
      GO TO 1
C
C
C     FINISHED. PROCESS RESULTS FOR PROPER SETTING OF IFLAG
C
   10 IF (FB*FC .GT. 0.) GO TO 13
      IF (ABS(FB) .GT. FX) GO TO 12
      IFLAG = 1
      RETURN
   11 IFLAG = 2
      RETURN
   12 IFLAG = 3
      RETURN
   13 IFLAG = 4
      RETURN
   15 IFLAG = 5
      RETURN
      END
      DOUBLE PRECISION FUNCTION TNC(T, DF, DELTA, IFAULT)
C
C     ALGORITHM AS 243  APPL. STATIST. (1989), VOL.38, NO. 1
C
C     CUMULATIVE PROBABILITY AT T OF THE NON-CENTRAL T-DISTRIBUTION
C     WITH DF DEGREES OF FREEDOM (MAY BE FRACTIONAL) AND NON-CENTRALITY
C     PARAMETER DELTA.
C
C     NOTE - REQUIRES THE FOLLOWING AUXILIARY ROUTINES
C     ALNGAM (X)                         - ACM 291 OR AS 245
C     BETAIN (X, A, B, ALBETA, IFAULT)   - AS 63 (UPDATED IN ASR 19)
C     ALNORM (X, UPPER)                  - AS 66
C
      DOUBLE PRECISION
     *  A, ALBETA, ALNRPI, B, DEL, DELTA, DF, EN, ERRBD, ERRMAX,
     *	GEVEN, GODD, HALF, ITRMAX, LAMBDA, ONE, P, Q, R2PI, RXB, S, T,
     *	TT, TWO, X, XEVEN, XODD, ZERO
      LOGICAL NEGDEL
      
      DOUBLE PRECISION ALNORM,ALNGAM,BETAIN
C
C     NOTE - ITRMAX AND ERRMAX MAY BE CHANGED TO SUIT ONE'S NEEDS.
C
      DATA ITRMAX/100.1/, ERRMAX/1.D-06/
C
C     CONSTANTS - R2PI = 1/ {GAMMA(1.5) * SQRT(2)} = SQRT(2 / PI)
C                 ALNRPI = LN(SQRT(PI))
C
      DATA ZERO/0.D0/, HALF/0.5D0/, ONE/1.D0/, TWO/2.D0/,
     *	R2PI/0.79788 45608 02865 35588/,
     *  ALNRPI/0.57236 49429 24700 08707/
C
      TNC = ZERO
      IFAULT = 2
      IF (DF .LE. ZERO) RETURN
      IFAULT = 0
C
      TT = T
      DEL = DELTA
      NEGDEL = .FALSE.
      IF (T .GE. ZERO) GO TO 1
      NEGDEL = .TRUE.
      TT = -TT
      DEL = -DEL
    1 CONTINUE
C
C     INITIALIZE TWIN SERIES (GUENTHER, J. STATIST. COMPUTN. SIMULN.
C     VOL.6, 199, 1978).
C
      EN = ONE
      X = T * T / (T* T + DF)
      IF (X .LE. ZERO) GO TO 20
      LAMBDA = DEL * DEL
      P = HALF * EXP(-HALF * LAMBDA)
      Q = R2PI * P * DEL
      S = HALF - P
      A = HALF
      B = HALF * DF
      RXB = (ONE - X) ** B
      ALBETA = ALNRPI + ALNGAM(B, IFAULT) - ALNGAM(A + B, IFAULT)
      XODD = BETAIN(X, A, B, ALBETA, IFAULT)
      GODD = TWO * RXB * EXP(A * LOG(X) - ALBETA)
      XEVEN = ONE - RXB
      GEVEN = B * X * RXB
      TNC = P * XODD + Q * XEVEN
C
C     REPEAT UNTIL CONVERGENCE
C
   10 A = A + ONE
      XODD = XODD - GODD
      XEVEN = XEVEN - GEVEN
      GODD = GODD * X * (A + B - ONE) / A
      GEVEN = GEVEN * X * (A + B - HALF) / (A + HALF)
      P = P * LAMBDA / (TWO * EN)
      Q = Q * LAMBDA / (TWO * EN + ONE)
      S = S - P
      EN = EN + ONE
      TNC = TNC + P * XODD + Q * XEVEN
      ERRBD = TWO * S * (XODD - GODD)
      IF (ERRBD .GT. ERRMAX .AND. EN .LE. ITRMAX) GO TO 10
C
   20 IFAULT = 1
      IF (EN .GT. ITRMAX) RETURN
      IFAULT = 0
      TNC = TNC + ALNORM(DEL, .TRUE.)
      IF (NEGDEL) TNC = ONE - TNC
C
      RETURN
      END
C THIS FILE CONTAINS TWO ALGORITHMS FOR THE LOGARITHM OF THE GAMMA FUNCTION.
C ALGORITHM AS 245 IS THE FASTER (BUT LONGER) AND GIVES AN ACCURACY OF ABOUT
C 10-12 SIGNIFICANT DECIMAL DIGITS EXCEPT FOR SMALL REGIONS AROUND X = 1 AND
C X = 2, WHERE THE FUNCTION GOES TO ZERO.
C THE SECOND ALGORITHM IS NOT PART OF THE AS ALGORITHMS.   IT IS SLOWER BUT
C GIVES 14 OR MORE SIGNIFICANT DECIMAL DIGITS ACCURACY, EXCEPT AROUND X = 1
C AND X = 2.   THE LANCZOS SERIES FROM WHICH THIS ALGORITHM IS DERIVED IS
C INTERESTING IN THAT IT IS A CONVERGENT SERIES APPROXIMATION FOR THE GAMMA
C FUNCTION, WHEREAS THE FAMILIAR SERIES DUE TO DE MOIVRE (AND USUALLY WRONGLY
C CALLED STIRLING'S APPROXIMATION) IS ONLY AN ASYMPTOTIC APPROXIMATION, AS
C IS THE TRUE AND PREFERABLE APPROXIMATION DUE TO STIRLING.
C
C
C
      DOUBLE PRECISION FUNCTION ALNGAM(XVALUE, IFAULT)
C
C     ALGORITHM AS245  APPL. STATIST. (1989) VOL. 38, NO. 2
C
C     CALCULATION OF THE LOGARITHM OF THE GAMMA FUNCTION
C
      INTEGER IFAULT
      DOUBLE PRECISION ALR2PI, FOUR, HALF, ONE, ONEP5, R1(9), R2(9),
     +		R3(9), R4(5), TWELVE, X, X1, X2, XLGE, XLGST, XVALUE,
     +		Y, ZERO
C
C     COEFFICIENTS OF RATIONAL FUNCTIONS
C
      DATA R1/-2.66685 51149 5D0, -2.44387 53423 7D1,
     +        -2.19698 95892 8D1,  1.11667 54126 2D1,
     +	       3.13060 54762 3D0,  6.07771 38777 1D-1,
     +	       1.19400 90572 1D1,  3.14690 11574 9D1,
     +	       1.52346 87407 0D1/
      DATA R2/-7.83359 29944 9D1, -1.42046 29668 8D2,
     +         1.37519 41641 6D2,  7.86994 92415 4D1,
     +         4.16438 92222 8D0,  4.70668 76606 0D1,
     +         3.13399 21589 4D2,  2.63505 07472 1D2,
     +         4.33400 02251 4D1/
      DATA R3/-2.12159 57232 3D5,  2.30661 51061 6D5,
     +         2.74647 64470 5D4, -4.02621 11997 5D4,
     +        -2.29660 72978 0D3, -1.16328 49500 4D5,
     +        -1.46025 93751 1D5, -2.42357 40962 9D4,
     +        -5.70691 00932 4D2/
      DATA R4/ 2.79195 31791 8525D-1, 4.91731 76105 05968D-1,
     +         6.92910 59929 1889D-2, 3.35034 38150 22304D0,
     +         6.01245 92597 64103D0/
C
C     FIXED CONSTANTS
C
      DATA ALR2PI/9.18938 53320 4673D-1/, FOUR/4.D0/, HALF/0.5D0/,
     +     ONE/1.D0/, ONEP5/1.5D0/, TWELVE/12.D0/, ZERO/0.D0/
C
C     MACHINE-DEPENDANT CONSTANTS.
C     A TABLE OF VALUES IS GIVEN AT THE TOP OF PAGE 399 OF THE PAPER.
C     THESE VALUES ARE FOR THE IEEE DOUBLE-PRECISION FORMAT FOR WHICH
C     B = 2, T = 53 AND U = 1023 IN THE NOTATION OF THE PAPER.
C
      DATA XLGE/5.10D6/, XLGST/1.D+305/
C
      X = XVALUE
      ALNGAM = ZERO
C
C     TEST FOR VALID FUNCTION ARGUMENT
C
      IFAULT = 2
      IF (X .GE. XLGST) RETURN
      IFAULT = 1
      IF (X .LE. ZERO) RETURN
      IFAULT = 0
C
C     CALCULATION FOR 0 < X < 0.5 AND 0.5 <= X < 1.5 COMBINED
C
      IF (X .LT. ONEP5) THEN
	IF (X .LT. HALF) THEN
	  ALNGAM = -LOG(X)
	  Y = X + ONE
C
C     TEST WHETHER X < MACHINE EPSILON
C
	  IF (Y .EQ. ONE) RETURN
	ELSE
	  ALNGAM = ZERO
	  Y = X
	  X = (X - HALF) - HALF
	END IF
	ALNGAM = ALNGAM + X * ((((R1(5)*Y + R1(4))*Y + R1(3))*Y
     +                + R1(2))*Y + R1(1)) / ((((Y + R1(9))*Y + R1(8))*Y
     +                + R1(7))*Y + R1(6))
	RETURN
      END IF
C
C     CALCULATION FOR 1.5 <= X < 4.0
C
      IF (X .LT. FOUR) THEN
	Y = (X - ONE) - ONE
	ALNGAM = Y * ((((R2(5)*X + R2(4))*X + R2(3))*X + R2(2))*X
     +              + R2(1)) / ((((X + R2(9))*X + R2(8))*X + R2(7))*X
     +              + R2(6))
	RETURN
      END IF
C
C     CALCULATION FOR 4.0 <= X < 12.0
C
      IF (X .LT. TWELVE) THEN
	ALNGAM = ((((R3(5)*X + R3(4))*X + R3(3))*X + R3(2))*X + R3(1)) /
     +            ((((X + R3(9))*X + R3(8))*X + R3(7))*X + R3(6))
	RETURN
      END IF
C
C     CALCULATION FOR X >= 12.0
C
      Y = LOG(X)
      ALNGAM = X * (Y - ONE) - HALF * Y + ALR2PI
      IF (X .GT. XLGE) RETURN
      X1 = ONE / X
      X2 = X1 * X1
      ALNGAM = ALNGAM + X1 * ((R4(3)*X2 + R4(2))*X2 + R4(1)) /
     +              ((X2 + R4(5))*X2 + R4(4))
      RETURN
      END
C
C
C
C
	DOUBLE PRECISION FUNCTION LNGAMMA(Z, IER)
C
C       USES LANCZOS-TYPE APPROXIMATION TO LN(GAMMA) FOR Z > 0.
C       REFERENCE:
C            LANCZOS, C. 'A PRECISION APPROXIMATION OF THE GAMMA
C                    FUNCTION', J. SIAM NUMER. ANAL., B, 1, 86-96, 1964.
C       ACCURACY: ABOUT 14 SIGNIFICANT DIGITS EXCEPT FOR SMALL REGIONS
C                 IN THE VICINITY OF 1 AND 2.
C
C       PROGRAMMER: ALAN MILLER
C                   CSIRO DIVISION OF MATHEMATICS & STATISTICS
C
C	N.B. IT IS ASSUMED THAT THE FORTRAN COMPILER SUPPORTS LONG
C	     VARIABLE NAMES, INCLUDING THE UNDERLINE CHARACTER.   SOME
C	     COMPILERS WILL NOT ACCEPT THE 'IMPLICIT NONE' STATEMENT
C	     BELOW.
C
C       LATEST REVISION - 17 APRIL 1988
C
	IMPLICIT NONE
	DOUBLE PRECISION A(9), Z, LNSQRT2PI, TMP
	INTEGER IER, J
	DATA A/0.9999999999995183D0, 676.5203681218835D0,
     +         -1259.139216722289D0, 771.3234287757674D0,
     +         -176.6150291498386D0, 12.50734324009056D0,
     +         -0.1385710331296526D0, 0.9934937113930748D-05,
     +         0.1659470187408462D-06/
C
	DATA LNSQRT2PI/0.91893 85332 04672 7D0/
C
	IF (Z .LE. 0.D0) THEN
	  IER = 1
	  RETURN
	END IF
	IER = 0
C
	LNGAMMA = 0.D0
	TMP = Z + 7.D0
	DO 10 J = 9, 2, -1
	  LNGAMMA = LNGAMMA + A(J)/TMP
	  TMP = TMP - 1.D0
   10   CONTINUE
	LNGAMMA = LNGAMMA + A(1)
	LNGAMMA = LOG(LNGAMMA) + LNSQRT2PI - (Z+6.5D0) +
     +                               (Z-0.5D0)*LOG(Z+6.5D0)
	END
      DOUBLE PRECISION FUNCTION BETAIN(X, P, Q, BETA, IFAULT)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
C     ALGORITHM AS 63  APPL. STATIST. (1973), VOL.22, NO.3
C
C     COMPUTES INCOMPLETE BETA FUNCTION RATIO FOR ARGUMENTS
C     X BETWEEN ZERO AND ONE, P AND Q POSITIVE.
C     LOG OF COMPLETE BETA FUNCTION, BETA, IS ASSUMED TO BE KNOWN
C
      LOGICAL INDX
C
C     DEFINE ACCURACY AND INITIALISE
C
      DATA ZERO/0.0D0/, ONE/1.0D0/, ACU/0.1D-14/
      BETAIN=X
C
C     TEST FOR ADMISSIBILITY OF ARGUMENTS
C
      IFAULT=1
      IF(P.LE.ZERO .OR. Q.LE.ZERO) RETURN
      IFAULT=2
      IF(X.LT.ZERO .OR. X.GT.ONE) RETURN
      IFAULT=0
      IF(X.EQ.ZERO .OR. X.EQ. ONE) RETURN
C
C     CHANGE TAIL IF NECESSARY AND DETERMINE S
C
      PSQ=P+Q
      CX=ONE-X
      IF(P.GE.PSQ*X) GOTO 1
      XX=CX
      CX=X
      PP=Q
      QQ=P
      INDX=.TRUE.
      GOTO 2
    1 XX=X
      PP=P
      QQ=Q
      INDX=.FALSE.
    2 TERM=ONE
      AI=ONE
      BETAIN=ONE
      NS=QQ+CX*PSQ
C
C     USER SOPER'S REDUCTION FORMULAE.
C
      RX=XX/CX
    3 TEMP=QQ-AI
      IF(NS.EQ.0) RX=XX
    4 TERM=TERM*TEMP*RX/(PP+AI)
      BETAIN=BETAIN+TERM
      TEMP=ABS(TERM)
      IF(TEMP.LE.ACU .AND. TEMP.LE.ACU*BETAIN) GOTO 5
      AI=AI+ONE
      NS=NS-1
      IF(NS.GE.0) GOTO 3
      TEMP=PSQ
      PSQ=PSQ+ONE
      GOTO 4
C
C     CALCULATE RESULT
C
    5 BETAIN=BETAIN*EXP(PP*LOG(XX)+(QQ-ONE)*LOG(CX)-BETA)/PP
      IF(INDX) BETAIN=ONE-BETAIN
      RETURN
      END
C THIS FILE INCLUDES THE APPLIED STATISTICS ALGORITHM AS 66 FOR CALCULATING
C THE TAIL AREA UNDER THE NORMAL CURVE, AND TWO ALTERNATIVE ROUTINES WHICH
C GIVE HIGHER ACCURACY.   THE LATTER HAVE BEEN CONTRIBUTED BY ALAN MILLER OF
C CSIRO DIVISION OF MATHEMATICS & STATISTICS, CLAYTON, VICTORIA.   NOTICE
C THAT EACH FUNCTION OR ROUTINE HAS DIFFERENT CALL ARGUMENTS.
C
C
      DOUBLE PRECISION FUNCTION ALNORM(X,UPPER)
C
C         ALGORITHM AS66 APPLIED STATISTICS (1973) VOL22 NO.3
C
C       EVALUATES THE TAIL AREA OF THE STANDARDISED NORMAL CURVE
C       FROM X TO INFINITY IF UPPER IS .TRUE. OR
C       FROM MINUS INFINITY TO X IF UPPER IS .FALSE.
C
      DOUBLE PRECISION ZERO,ONE,HALF
      DOUBLE PRECISION CON,Z,Y,X
      DOUBLE PRECISION P,Q,R,A1,A2,A3,B1,B2,C1,C2,C3,C4,C5,C6
      DOUBLE PRECISION D1,D2,D3,D4,D5
      LOGICAL UPPER,UP
C*** MACHINE DEPENDENT CONSTANTS
      DOUBLE PRECISION LTONE,UTZERO
      DATA ZERO/0.0D0/, ONE/1.0D0/, HALF/0.5D0/
      DATA LTONE/7.0D0/,UTZERO/18.66D0/
      DATA CON/1.28D0/
      DATA P/0.398942280444D0/,Q/0.39990348504D0/,R/0.398942280385D0/   
      DATA A1/5.75885480458D0/,A2/2.62433121679D0/,A3/5.92885724438D0/  
      DATA B1/-29.8213557807D0/,B2/48.6959930692D0/
      DATA C1/-3.8052D-8/,C2/3.98064794D-4/,C3/-0.151679116635D0/
      DATA C4/4.8385912808D0/,C5/0.742380924027D0/,C6/3.99019417011D0/  
      DATA D1/1.00000615302D0/,D2/1.98615381364D0/,D3/5.29330324926D0/  
      DATA D4/-15.1508972451D0/,D5/30.789933034D0/
C
      UP=UPPER
      Z=X
      IF(Z.GE.ZERO)GOTO 10
      UP=.NOT.UP
      Z=-Z
   10 IF(Z.LE.LTONE.OR.UP.AND.Z.LE.UTZERO)GOTO 20
      ALNORM=ZERO
      GOTO 40
   20 Y=HALF*Z*Z
      IF(Z.GT.CON) GOTO 30
C
      ALNORM=HALF-Z*(P-Q*Y/(Y+A1+B1/(Y+A2+B2/(Y+A3))))
      GOTO 40
   30 ALNORM=R*DEXP(-Y)/(Z+C1+D1/(Z+C2+D2/(Z+C3+D3/(Z+C4+D4/(Z+C5+D5/   
     2   (Z+C6))))))
   40 IF(.NOT.UP)ALNORM=ONE-ALNORM
      RETURN
      END
C
C
C
	SUBROUTINE NORMP(Z, P, Q, PDF)
C
C	NORMAL DISTRIBUTION PROBABILITIES ACCURATE TO 1.E-15.
C	Z = NO. OF STANDARD DEVIATIONS FROM THE MEAN.
C	P, Q = PROBABILITIES TO THE LEFT & RIGHT OF Z.   P + Q = 1.
C       PDF = THE PROBABILITY DENSITY.
C
C       BASED UPON ALGORITHM 5666 FOR THE ERROR FUNCTION, FROM:
C       HART, J.F. ET AL, 'COMPUTER APPROXIMATIONS', WILEY 1968
C
C       PROGRAMMER: ALAN MILLER
C
C	LATEST REVISION - 30 MARCH 1986
C
	IMPLICIT DOUBLE PRECISION (A-H, O-Z)
	DATA P0, P1, P2, P3, P4, P5, P6/220.20 68679 12376 1D0,
     *	  221.21 35961 69931 1D0, 112.07 92914 97870 9D0,
     *	  33.912 86607 83830 0D0, 6.3739 62203 53165 0D0,
     *	  .70038 30644 43688 1D0, .35262 49659 98910 9D-01/,
     *	  Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7/440.41 37358 24752 2D0,
     *	  793.82 65125 19948 4D0, 637.33 36333 78831 1D0,
     *	  296.56 42487 79673 7D0, 86.780 73220 29460 8D0,
     *	  16.064 17757 92069 5D0, 1.7556 67163 18264 2D0,
     *	  .88388 34764 83184 4D-1/,
     *	  CUTOFF/7.071D0/, ROOT2PI/2.5066 28274 63100 1D0/
C
	ZABS = ABS(Z)
C
C	|Z| > 37.
C
	IF (ZABS .GT. 37.D0) THEN
	  PDF = 0.D0
	  IF (Z .GT. 0.D0) THEN
	    P = 1.D0
	    Q = 0.D0
	  ELSE
	    P = 0.D0
	    Q = 1.D0
	  END IF
	  RETURN
	END IF
C
C	|Z| <= 37.
C
	EXPNTL = EXP(-0.5D0*ZABS**2)
	PDF = EXPNTL/ROOT2PI
C
C	|Z| < CUTOFF = 10/SQRT(2).
C
	IF (ZABS .LT. CUTOFF) THEN
	  P = EXPNTL*((((((P6*ZABS + P5)*ZABS + P4)*ZABS + P3)*ZABS +
     *		P2)*ZABS + P1)*ZABS + P0)/(((((((Q7*ZABS + Q6)*ZABS +
     *		Q5)*ZABS + Q4)*ZABS + Q3)*ZABS + Q2)*ZABS + Q1)*ZABS +
     *		Q0)
C
C	|Z| >= CUTOFF.
C
	ELSE
	  P = PDF/(ZABS + 1.D0/(ZABS + 2.D0/(ZABS + 3.D0/(ZABS + 4.D0/
     *		(ZABS + 0.65D0)))))
	END IF
C
	IF (Z .LT. 0.D0) THEN
	  Q = 1.D0 - P
	ELSE
	  Q = P
	  P = 1.D0 - Q
	END IF
	RETURN
	END
C
C
C
        SUBROUTINE NPROB(Z,P,Q,PDF)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        SAVE

C
C       P, Q = PROBABILITIES TO THE LEFT AND RIGHT OF Z
C       FOR THE STANDARD NORMAL DISTRIBUTION.
C       PDF  = THE PROBABILITY DENSITY FUNCTION
C
C       REFERENCE: ADAMS,A.G. AREAS UNDER THE NORMAL CURVE,
C       ALGORITHM 39, COMPUTER J., VOL. 12, 197-8, 1969.
C
C       LATEST REVISION - 23 JANUARY 1981
C
C********************************************************************
C
        DATA A0,A1,A2,A3,A4,A5,A6,A7/0.5D0, 0.398942280444D0,
     1  0.399903438504D0, 5.75885480458D0, 29.8213557808D0,
     2  2.62433121679D0, 48.6959930692D0, 5.92885724438D0/,
     3  B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11/0.398942280385D0,
     4  3.8052D-8, 1.00000615302D0, 3.98064794D-4, 1.98615381364D0,
     5  0.151679116635D0, 5.29330324926D0, 4.8385912808D0,
     6  15.1508972451D0, 0.742380924027D0, 30.789933034D0,
     7  3.99019417011D0/
C
        ZABS = ABS(Z)
        IF(ZABS.GT.12.7D0) GO TO 20
        Y = A0*Z*Z
        PDF = EXP(-Y)*B0
        IF(ZABS.GT.1.28D0) GO TO 10
C
C       Z BETWEEN -1.28 AND +1.28
C
        Q = A0-ZABS*(A1-A2*Y/(Y+A3-A4/(Y+A5+A6/(Y+A7))))
        IF(Z.LT.0.D0) GO TO 30
        P = 1.D0-Q
        RETURN
C
C       ZABS BETWEEN 1.28 AND 12.7
C
   10   Q = PDF/(ZABS-B1+B2/(ZABS+B3+B4/(ZABS-B5+B6/(ZABS+B7-B8/
     1  (ZABS+B9+B10/(ZABS+B11))))))
        IF(Z.LT.0.D0) GO TO 30
        P = 1.D0-Q
        RETURN
C
C       Z FAR OUT IN TAIL
C
   20   Q = 0.D0
        PDF = 0.D0
        IF(Z.LT.0.D0) GO TO 30
        P = 1.D0
        RETURN
C
C       NEGATIVE Z, INTERCHANGE P AND Q
C
   30   P = Q
        Q = 1.D0-P
        RETURN
        END
C***********************************************************
C
C
C
C     DUMMY ROUTINES
C
      SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)
C
      DOUBLE PRECISION DX(1),DY(1)
      IF(N.LE.0) RETURN
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
      END
!====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
      SUBROUTINE ADJUSTLT(STRING)
!====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
! 
!         Left justifies an ASCII string
!
!
!         Author......Tim Cohn (TAC)
!         Date........21 January 2002
!
!====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT NONE

      CHARACTER*(*) STRING

      INTEGER I,J,K,N

        N = LEN_TRIM(STRING)
      DO 10 I=1,N
        IF(STRING(I:I) .NE. ' ') GOTO 20
10    CONTINUE
        RETURN
20    CONTINUE
      DO 30 J=I,N
        K = J-I+1
        STRING(K:K) = STRING(J:J)
30    CONTINUE
      DO 40 J=K+1,N
        STRING(J:J) = ' '
40    CONTINUE
      RETURN
      END
C====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
         SUBROUTINE DPORDER(X,N,IX)
C===============================================================================
C
C        SUBROUTINE DPORDER GIVES THE PERMUTATION OF A USER-SUPPLIED
C        VECTOR.  TREE-SORT ALGORITHM IS EMPLOYED (WHY NOT?)
C        RETURN RESULTING IX ARRAY WITH THE ASCENDING PERMUTATIONS
C
C          X(IX(1)) = SMALLEST VALUE IN X
C          X(IX(N)) = LARGEST VALUE IN X
C
C
C        AUTHOR....TIM COHN
C        DATE......APRIL 1,  1986
C        REVISED...AUGUST 9, 1986      (TAC)
C        MODIFIED......12 FEBRUARY 2003 (TAC)
C         --REMOVED ERRORS INTRODUCED IN 1986
C           WHEN CODE WAS REVISED BY DRH.  
C         --IMPLICIT NONE ADDED; ALL VARIABLES DECLARED
C         --DOUBLE PRECISION
C        MODIFIED......15 APRIL 2003 (TAC)
C         --RENAMED TO DPORDER FROM PORDER
C
C===============================================================================
C
C     PROPERTY OF US GOVERNMENT, U.S. GEOLOGICAL SURVEY
C
C     *** DO NOT MODIFY WITHOUT AUTHOR'S CONSENT ***
C
C     AUTHOR CAN BE CONTACTED AT:  TACOHN@USGS.GOV (703/648-5711)     
C
C===============================================================================
C
C        X(N)      R*8       INPUT VECTOR OF UNORDERED DATA
C        N         I*4       INPUT NUMBER OF OBSERVATIONS IN X
C        IX(N)     I*4       OUTPUT VECTOR CONTAINING PERMUTATION OF X
C                               X(IX(1)) IS THE SMALLEST VALUE OF X
C                                 . . . 
C                               X(IX(N)) IS THE LARGEST VALUE OF X
C
C===============================================================================

         IMPLICIT NONE
         
         INTEGER N_PTS,N_T
         PARAMETER (N_T=10000,N_PTS=10000)

         DOUBLE PRECISION X(*)
         
         INTEGER
     1     L(0:N_PTS),R(0:N_PTS),P(0:N_PTS),
     2     IX(*),I2,I,INDX,N,ICT

C===============================================================================
C
C    FIRST CHECK TO SEE IF WE HAVE AN ORDERED DATA VECTOR TO BEGIN WITH
C
         DO 50 I2=2,N
              IX(I2)    =  I2
              IF(X(I2) .LT. X(I2-1)) GOTO 1
   50    CONTINUE
              IX(1)     =  1
              RETURN

    1    CONTINUE

         L(1) =  0
         R(1) =  0
         P(1) =  0

        DO 10 I=2,N
         INDX =  1
         L(I)   =  0
         R(I)   =  0

   20    CONTINUE
         IF(X(I) .GE. X(INDX)) THEN
              IF(R(INDX) .EQ. 0) THEN
                   R(INDX)   =  I
                   P(I)      =  INDX
                   GOTO 10
              ELSE
                   INDX =  R(INDX)
                   GOTO 20
              ENDIF
         ELSE
              IF(L(INDX) .EQ. 0) THEN
                   L(INDX)   =  I
                   P(I)      =  INDX
                   GOTO 10
              ELSE
                   INDX =  L(INDX)
                   GOTO 20
              ENDIF
         ENDIF
   10   CONTINUE

          INDX =  1
         DO 40 ICT=1,N

   30    CONTINUE
         IF(L(INDX) .EQ. 0) THEN
              IX(ICT)     =  INDX
              P(R(INDX))   =  P(INDX)
              L(P(INDX))   =  R(INDX)
              INDX         =  P(INDX)
         ELSE
              INDX =  L(INDX)
              GOTO 30
         ENDIF
   40    CONTINUE
         RETURN
         END
C====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
         SUBROUTINE DSVRGP(N,X,Y,IORD)
C===============================================================================
C
C        SUBROUTINE DSVRGP SORTS AN ARRAY AND RETURNS 
C        THE PERMUTATION OF A USER-SUPPLIED
C        VECTOR.  TREE-SORT ALGORITHM IS EMPLOYED (WHY NOT?)
C        RETURN RESULTING IX ARRAY WITH THE ASCENDING PERMUTATIONS
C
C          X(IX(1)) = SMALLEST VALUE IN X
C          X(IX(N)) = LARGEST VALUE IN X
C
C          IORD(1)  = RANK OF FIRST VALUE IN X (1 WOULD BE SMALLEST, N LARGEST)
C          IORD(N)  = RANK OF LAST VALUE IN X
C
C        AUTHOR....TIM COHN
C        DATE......APRIL 15 2003 (TAC)
C
C===============================================================================
C
C     PROPERTY OF US GOVERNMENT, U.S. GEOLOGICAL SURVEY
C
C     *** DO NOT MODIFY WITHOUT AUTHOR'S CONSENT ***
C
C     AUTHOR CAN BE CONTACTED AT:  TACOHN@USGS.GOV (703/648-5711)     
C
C===============================================================================
C
C        N         I*4       INPUT NUMBER OF OBSERVATIONS IN X
C        X(N)      R*8       INPUT VECTOR OF UNORDERED DATA
C        Y(N)      R*8       OUTPUT VECTOR OF SORTED (ASCENDING) DATA
C        IX(N)     I*4       OUTPUT VECTOR CONTAINING PERMUTATION OF X
C                               X(IX(1)) IS THE SMALLEST VALUE OF X
C                                 . . . 
C                               X(IX(N)) IS THE LARGEST VALUE OF X
C
C===============================================================================

         IMPLICIT NONE
         
         DOUBLE PRECISION X(*),Y(*)
         INTEGER IORD(*),IX(10000),I,N
         
          IF(N .GE. 10000) THEN
            WRITE(6,*) 'ARRAY DIMENSION TOO SMALL IN DSVRGP'
            STOP
          ENDIF
          
         CALL DPORDER(X,N,IX)
         
         DO 10 I=1,N
           Y(I)        = X(IX(I))
           IORD(IX(I)) = I
10       CONTINUE

         RETURN
         END
C====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
         

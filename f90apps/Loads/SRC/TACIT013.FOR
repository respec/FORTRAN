        SUBROUTINE TACIT_PARMS
     1    (Y,NO,YD,X,IDIM,NP,BETA,STDDEV,PVALUE)
C*** REVISION 92.12 12/2/92
C=======================================================================
C
C      COMPUTES NEARLY-UNBIASED AMLE PARAMETER ESTIMATES
C
C
C      AUTHOR........TIM COHN
C      DATE..........FEBRUARY 14, 1992
C      Modified......July 28, 1992 (TAC & WGB)
C      Modified......Sept. 24, 1992 (TAC)
C      Modified......Oct. 10, 1992 (TAC)
C      Modified......Dec. 2, 1992 (TAC)
c
C=======================================================================
C
C      VARIABLE      TYPE           DEFINITION
C      --------      ----           ----------
C      Y(NO)         R*8            THE VECTOR OF DEPENDENT OBSERVATIONS (IN)
C      NO            I*4            NUMBER OF OBSERVATIONS (IN)
C      YD(NO)        R*8            THE VECTOR OF CENSORING THRESHOLDS; (IN)
C                                     Y IS OBSERVED IFF Y>=YD.
C      XI(IDIM,N)    R*8            THE MATRIX OF X.  NOTE THAT X(@,1) (IN)
C                                     IS A VECTOR OF 1'S IF A CONSTANT
C                                     IS INCLUDED IN MODEL
C      IDIM          I*4            LEADING DIMENSION OF X (IN)
C      NP            I*4            NUMBER OF PARAMETERS IN MODEL (IN)
C                                     NP = 1 ==>  FIT A CONSTANT
C                                     NP = 2 ==>  FIT A LINE, ETC.
C      BETA(*)       R*8            VECTOR OF AMLE PARAMETER ESTIMATES (OUT)
C                                     N.B. B_MLE(NP+1) CONTAINS S**2-HAT
C      STDDEV(*)     R*8            STD. DEV. OF BETA PARM ESTS. (OUT)
C      PVALUE(*)     R*8            CORRESPONDING LRT P-VALUES (OUT)
C
C=======================================================================

        IMPLICIT DOUBLE PRECISION (A-H,P-Z)

        INTEGER PPARAMS,POBS
        PARAMETER (PPARAMS=25,POBS=2500)

        DIMENSION
     1            Y(NO),YD(NO),X(IDIM,NP),BETA(NP+1),
     2            STDDEV(NP+1),PVALUE(NP)
C    1            Y(NO),YD(NO),X(IDIM,NP),BETA(NP),STDDEV(NP),PVALUE(NP)
C     statement corrected for array bounds error - pbd

        DIMENSION
     1            B_MLE(PPARAMS),B_AMLE(PPARAMS),
     2            CV_M(PPARAMS,PPARAMS),CV_A(PPARAMS,PPARAMS),
     3            BIAS(PPARAMS),CV(PPARAMS,PPARAMS),
     4            SBIAS(PPARAMS),SCV(PPARAMS,PPARAMS)

        DIMENSION
     1            XLIKE(0:PPARAMS),ITEST(PPARAMS),B_TEST(PPARAMS)

        DIMENSION
     1            XL(PPARAMS),CVXL(PPARAMS)

C
C    GET AMLE PARAMETER ESTIMATES
C
        CALL TACIT
     1    (Y,NO,YD,X,IDIM,NP,1,B_MLE,XLIKE(0),B_AMLE,CV_M,CV_A,
     2     DF,BIAS,CV,SBIAS,SCV)

        DO 10 J=1,NP+1
          BETA(J)     =  B_AMLE(J)
          STDDEV(J)   =  SQRT(CV_A(J,J))
   10   CONTINUE
C
C    GET LIKEL. RATIO TEST P-VALUES CORRESPONDING TO PARAMETER ESTIMATES
C
        DO 20 J=1,NP
            CALL DSET(NP,0.D0,B_TEST,1)
            CALL ISET(NP,0,ITEST,1)
            ITEST(J)    =  1
          CALL TACIT_TEST
     1      (Y,NO,YD,X,IDIM,NP,ITEST,B_TEST,B_MLE,XLIKE(J),B_AMLE)
          PVALUE(J)   =  TACIT_PVALUE(1.D0,XLIKE(J),XLIKE(0))
   20   CONTINUE
        RETURN
        END
C=======================================================================
        SUBROUTINE TACIT_LOADS
     1    (Y,NO,YD,X,IDIM,NP,NE,XE,KDIM,XLOADS,XLOADSUM,XLOADVAR)
C=======================================================================
C
C      VARIABLE      TYPE           DEFINITION
C      --------      ----           ----------
C      NE            I*4            NUMBER OF CASES TO PREDICT (IN)
C      XE(KDIM,*)    R*8            THE MATRIX OF PREDICTORS (IN)
C      KDIM          I*4            LEADING DIM. OF XE (IN)
C      XLOADS(*)     R*8            ESTIMATED LOADS (OUT)
C      XLOADSUM      R*8            SUM OF ESTIMATED LOADS (OUT)
C      XLOADVAR      R*8            APPROXIMATE VARIANCE OF
C                                     SUM OF ESTIMATED LOADS (OUT)
C
C      N.B.  THIS CODE COMPUTES THE LINEARIZED VARIANCE, NOT THE EXACT
C
C=======================================================================

        IMPLICIT DOUBLE PRECISION (A-H,P-Z)

        INTEGER PPARAMS,POBS
        PARAMETER (PPARAMS=25,POBS=2500)

        DIMENSION
     1            Y(NO),YD(NO),X(IDIM,NP)

        DIMENSION
     1            XE(KDIM,NP),XLOADS(NE)
C     1            XE(KDIM,NE),XLOADS(NE)    XE BLOWING OUT - PBD

        DIMENSION
     1            B_MLE(PPARAMS),B_AMLE(PPARAMS),
     2            CV_M(PPARAMS,PPARAMS),CV_A(PPARAMS,PPARAMS),
     3            BIAS(PPARAMS),CV(PPARAMS,PPARAMS),
     4            SBIAS(PPARAMS),SCV(PPARAMS,PPARAMS)

        DIMENSION
     1            XLIKE(0:PPARAMS),ITEST(PPARAMS),B_TEST(PPARAMS)

        DIMENSION
     1            XL(PPARAMS),CVXL(PPARAMS)

        CALL TACIT
     1    (Y,NO,YD,X,IDIM,NP,1,B_MLE,XLIKE(0),B_AMLE,CV_M,CV_A,
     2     DF,BIAS,CV,SBIAS,SCV)

          SUMLOAD      =  0.D0
        DO 30 I=1,NE
          CALL TAC_LOAD
     1      (NP,B_MLE,BIAS,CV,SBIAS,SCV,1,XE(I,1),KDIM,
     2      XLOADS(I),XTOT,XVARTOT)
            SUMLOAD   =  SUMLOAD + XTOT
   30   CONTINUE
            XLOADSUM  =  SUMLOAD
C
          CALL DSET(NP+1,0.D0,XL(1),1)
        DO 40 I=1,NE
            XL(NP+1)  =  XL(NP+1) + XLOADS(I)/2.D0
          DO 40 J=1,NP
            XL(J)     =  XL(J) + XE(I,J)*XLOADS(I)
   40   CONTINUE
C
        CALL DMRRRR
     1    (NP+1,NP+1,CV_M,PPARAMS,NP+1,1,XL,PPARAMS,NP+1,1,CVXL,PPARAMS)
        CALL DMXTYF
     1    (NP+1,1,XL,PPARAMS,NP+1,1,CVXL,PPARAMS,1,1,XLOADVAR,1)
        RETURN
        END
C=======================================================================
        SUBROUTINE TACIT_LOAD_VAR
     1    (Y,NO,YD,X,IDIM,NP,NE,XE,KDIM,XLOADS,XLOADSUM,XLOADVAR)
C=======================================================================
C
C       ENTRY POINT FOR EXACT LOADS AND VARIANCES
C         COMPUTES EXACT VARIANCE
C
C      N.B.  THIS CODE COMPUTES THE EXACT VARIANCE, BUT TAKES A LONG
C            TIME TO RUN
C
C=======================================================================

        IMPLICIT DOUBLE PRECISION (A-H,P-Z)

        INTEGER PPARAMS,POBS
        PARAMETER (PPARAMS=25,POBS=2500)

        DIMENSION
     1            Y(NO),YD(NO),X(IDIM,NP)

        DIMENSION
     1            XE(KDIM,NE),XLOADS(NE)

        DIMENSION
     1            B_MLE(PPARAMS),B_AMLE(PPARAMS),
     2            CV_M(PPARAMS,PPARAMS),CV_A(PPARAMS,PPARAMS),
     3            BIAS(PPARAMS),CV(PPARAMS,PPARAMS),
     4            SBIAS(PPARAMS),SCV(PPARAMS,PPARAMS)

        DIMENSION
     1            XLIKE(0:PPARAMS),ITEST(PPARAMS),B_TEST(PPARAMS)

        DIMENSION
     1            XL(PPARAMS),CVXL(PPARAMS)

        CALL TACIT
     1    (Y,NO,YD,X,IDIM,NP,1,B_MLE,XLIKE(0),B_AMLE,CV_M,CV_A,
     2     DF,BIAS,CV,SBIAS,SCV)

          CALL TAC_LOAD
     1      (NP,B_MLE,BIAS,CV,SBIAS,SCV,NE,XE,KDIM,
     2      XLOADS,XLOADSUM,XLOADVAR)
        RETURN
        END
C
       SUBROUTINE TACIT
     1    (Y,NO,YD,XI,IDIM,NP,IT,B_MLE,XLIKE,B_AMLE,CV_M,CV_A,
     2     DF,BIAS,CV,SBIAS,SCV)
C=======================================================================
C
C      COMPUTES NEARLY-UNBIASED AMLE PARAMETER ESTIMATES
C
C
C      AUTHOR........TIM COHN
C      DATE..........JANUARY 30, 1992
C      MODIFIED......FEBRUARY 4, 1992  (COMMENTS CHANGED)
C      MODIFIED......FEBRUARY 8, 1992  (CALL CHANGED)
C      MODIFIED......FEBRUARY 12, 1992 (COMMENTS CHANGED)
C      MODIFIED......MARCH 2, 1992 (CALL CHANGED--ALL ARRAYS MUST BE
C                                   DIMENSIONED TO 25 COLUMNS)
C
C
C      N.B.  FIRST-ORDER BIAS IS ELIMINATED FOR S^2, NOT S.
C            RESULTS ARE EQUIVALENT TO OLS ESTIMATES IF THERE
C            IS NO CENSORING (SECOND-ORDER ADJUSTMENT IS MADE TO
C            ELIMINATE FIRST-ORDER BIAS TO GUARANTEE THIS RESULT).
C
C            THE PARAMETER RESULTS ARE FOR THE FIRST N PARAMETERS, FOLLOWED
C            BY S**2, THE EST. STANDARD DEVIATION OF THE RESIDUALS.  THE
C            STD. COVARIANCES WRT S**2 ARE GIVEN IN CV, WRT S IN SCV.
C
C
C      ***** THIS FORTRAN CODE MAY NOT BE MODIFIED WITHOUT THE *****
C      *****              AUTHOR'S APPROVAL                    *****
C
C=======================================================================
C
C      VARIABLE      TYPE           DEFINITION
C      --------      ----           ----------
C      Y(NO)         R*8            THE VECTOR OF DEPENDENT OBSERVATIONS
C      NO            I*4            NUMBER OF OBSERVATIONS
C      YD(NO)        R*8            THE VECTOR OF CENSORING THRESHOLDS;
C                                     Y IS OBSERVED IFF Y>=YD.
C      XI(IDIM,N)    R*8            THE MATRIX OF X.  NOTE THAT X(@,1)
C                                     IS A VECTOR OF 1'S
C      IDIM          I*4            LEADING DIMENSION OF Y, X
C      NP            I*4            NUMBER OF PARAMETERS IN MODEL
C      IT            I*4            INPUT TO IDENTIFY IF PARMS ARE TO
C                                     BE ESTIMATED (1) OR NOT (0)
C      B_MLE(*)      R*8            VECTOR OF MLE PARAMETER ESTIMATES
C                                     N.B. B_MLE(NP+1) CONTAINS S**2-HAT
C      XLIKE         R*8            VALUE OF THE LOG-LIKELIHOOD FUNCTION
C      B_AMLE(*)     R*8            VECTOR OF AMLE PARAMETER ESTIMATES
C      CV_M(25,25)   R*8            MLE'S OF THE COVARIANCE MATRIX (0UTPUT)
C                                     N.B. CV_M(NP+1) REFERS TO COVARIANCE
C                                     WITH RESPECT TO S**2
C      CV_A(25,25)   R*8            AMLE'S OF THE COVARIANCE MATRIX
C                                     WRT S**2 (0UTPUT)
C      DF            R*8            ESTIMATED NUMBER OF DEGREES OF
C                                     FREEDOM IN ERROR DISTRIBUTION
C      BIAS(25)      R*8            BIAS OF MLE'S WRT S**2 (0UTPUT)
C      CV(25,25)     R*8            COVARIANCE MATRIX OF STANDARD NORMALS
C                                     CENSORED AT THE XSI'S WRT. S**2 (0UTPUT)
C      SBIAS(25)     R*8            BIAS OF MLE'S WRT S (0UTPUT)
C      SCV(25,25)    R*8            COVARIANCE MATRIX OF STANDARD NORMALS
C                                     CENSORED AT THE XSI'S WRT. S (0UTPUT)
C
C=======================================================================

       IMPLICIT DOUBLE PRECISION (A-H,M,Q-Z)
       IMPLICIT INTEGER (I-L,N-P)

       INTEGER    PPARAMS,POBS
       PARAMETER (PPARAMS=25,POBS=2500)

       REAL*8
     1         B_MLE(PPARAMS),B_AMLE(*),BIAS(PPARAMS),
     2         H2(PPARAMS,PPARAMS),
     3         H3(PPARAMS,PPARAMS,PPARAMS),
     4         H3S(PPARAMS,PPARAMS,PPARAMS),
     5         HK2(2,2),HK3(2,2,2),HK3S(2,2,2)

       REAL*8
     1         SBIAS(PPARAMS),
     2         SH2(PPARAMS,PPARAMS),
     3         SH3(PPARAMS,PPARAMS,PPARAMS),
     4         SH3S(PPARAMS,PPARAMS,PPARAMS),
     5         SHK2(2,2),SHK3(2,2,2),SHK3S(2,2,2)

       REAL*8
     1         Y(IDIM),YD(IDIM),XI(IDIM,PPARAMS)

       REAL*8
     1         CV_M(PPARAMS,PPARAMS),CV_A(PPARAMS,PPARAMS),
     2         CV(PPARAMS,PPARAMS),SCV(PPARAMS,PPARAMS)

       COMMON /TACJXJ/ X(POBS,PPARAMS)

       DO 3 K1=1,NO
           X(K1,NP+1) =  1.D0
         DO 3 K2 =1,NP
           X(K1,K2)   =  XI(K1,K2)
    3  CONTINUE

        IF(IT .EQ. 1) THEN
               CALL TACIT_R(NO,NP,POBS,Y,YD,X,B_MLE,XLIKE)
               S            =  B_MLE(NP+1)
               B_MLE(NP+1)  =  S**2
        ELSE
               S            =  SQRT(B_MLE(NP+1))
        ENDIF

       DO 5 K1=1,NP+1
         DO 5 K2=1,NP+1
           H2(K1,K2)            =  0.D0
           SH2(K1,K2)           =  0.D0
         DO 5 K3=1,NP+1
           H3(K1,K2,K3)         =  0.D0
           SH3(K1,K2,K3)        =  0.D0
           H3S(K1,K2,K3)        =  0.D0
           SH3S(K1,K2,K3)       =  0.D0
    5  CONTINUE

       DO 10 I=1,NO
                YHAT      =  0.0
           DO 20 K=1,NP
                YHAT      =  YHAT + X(I,K)*B_MLE(K)
   20      CONTINUE
                XSI       =  (YD(I)-YHAT)/S

       CALL TACIT_D(XSI,SHK2,SHK3,SHK3S,HK2,HK3,HK3S)
C
C     N.B. 'H' IDENTIFIES CV'S CORRESPONDING TO S**1
C          'S' PREFIX IDENTIFIES TERMS CORRESP. TO S**2
C

       DO 30 K1=1,NP+1
                  IK1  =  1 + K1/(NP+1)
         DO 30 K2=1,NP+1
                  IK2  =  1 + K2/(NP+1)
                H2(K1,K2)      =  H2(K1,K2) -
     1                 HK2(IK1,IK2)*X(I,K1)*X(I,K2)
                SH2(K1,K2)     =  SH2(K1,K2) -
     1                 SHK2(IK1,IK2)*X(I,K1)*X(I,K2)
           DO 30 K3=1,NP+1
                  IK3  =  1 + K3/(NP+1)
                H3(K1,K2,K3)   =  H3(K1,K2,K3)+
     1                 HK3(IK1,IK2,IK3)*X(I,K1)*X(I,K2)*X(I,K3)
                H3S(K1,K2,K3)  =  H3S(K1,K2,K3)+
     1                 HK3S(IK1,IK2,IK3)*X(I,K1)*X(I,K2)*X(I,K3)
                SH3(K1,K2,K3)  =  SH3(K1,K2,K3)+
     1                 SHK3(IK1,IK2,IK3)*X(I,K1)*X(I,K2)*X(I,K3)
                SH3S(K1,K2,K3) =  SH3S(K1,K2,K3)+
     1                 SHK3S(IK1,IK2,IK3)*X(I,K1)*X(I,K2)*X(I,K3)
   30  CONTINUE

   10  CONTINUE

        CALL DLINRG(NP+1,H2,PPARAMS,CV,PPARAMS)

        CALL DLINRG(NP+1,SH2,PPARAMS,SCV,PPARAMS)

       DO 40 I=1,NP+1
              BIAS(I)   =  0.0
              SBIAS(I)  =  0.0
          DO 40 J=1,NP+1
            DO 40 K=1,NP+1
              DO 40 L=1,NP+1

         BIAS(I)  = BIAS(I)+CV(I,J)*CV(K,L)*(H3S(J,K,L)-H3(J,K,L)/2.D0)
         SBIAS(I) = SBIAS(I)+SCV(I,J)*SCV(K,L)*
     1                 (SH3S(J,K,L)-SH3(J,K,L)/2.D0)

   40  CONTINUE

       DO 50 I=1,NP
         B_AMLE(I) = B_MLE(I) - S*BIAS(I)
   50  CONTINUE
         B_AMLE(NP+1)  =  S**2/(1.D0+BIAS(NP+1))
         DF            =  2.D0/CV(NP+1,NP+1) - NP

           S2_M             =  S**2
           S3_M             =  S**3
           S4_M             =  S**4
C
C      NOTE:  TO GET UNBIASED ESTIMATES OF THE POWERS OF S2_A, WE NEED
C             TO CONSIDER THE VARIANCE OF S2_A
C
           S2_A          =  B_AMLE(NP+1)
           S3_A          =  S2_A**1.5D0/(1.D0+0.375D0*CV(NP+1,NP+1))
           S4_A          =  S2_A**2/(1.D0+CV(NP+1,NP+1))

           CV_M(NP+1,NP+1) =  S4_M*CV(NP+1,NP+1)
           CV_A(NP+1,NP+1) =  S4_A*CV(NP+1,NP+1)
       DO 60 I=1,NP
           CV_M(I,NP+1)    =  S3_M*CV(I,NP+1)
             CV_M(NP+1,I)  =    CV_M(I,NP+1)
           CV_A(I,NP+1)    =  S3_A*CV(I,NP+1)
             CV_A(NP+1,I)  =    CV_A(I,NP+1)
         DO 60 J=1,NP
           CV_M(I,J)       =  S2_M*CV(I,J)
           CV_A(I,J)       =  S2_A*CV(I,J)
   60  CONTINUE

        RETURN
        END

       SUBROUTINE TACIT_D(XSI,K2,K3,K3S,S2K2,S2K3,S2K3S)

C=======================================================================
C
C      SUBROUTINE TO COMPUTE THE PARTIAL DERIVATIVES OF THE LIKELIHOOD
C      FUNCTION (L.F.) FOR A SINGLE OBSERVATION.  RESULTS ARE
C      STANDARDIZED (DIVIDED BY SIGMA), WHICH LEADS TO MORE PRECISE
C      ESTIMATES.
C
C      AUTHOR..........TIM COHN
C      DATE............JANUARY 30, 1992 (TAC)
C
C======================================================================
C
C      XSI           R*8       DIMENSIONLESS CENSORING THRESHOLD (INPUT)
C      K2(2,2)       R*8       DIMENSIONLESS HESSIAN OF L.F.
C                                 WRT SIGMA (OUTPUT)
C      K3(2,2,2)     R*8       DIMENSIONLESS MATRIX OF 3RD DERIVS OF
C                                L.F. WRT SIGMA (OUTPUT)
C      K3S(2,2,2)    R*8       DIM. MAT. OF EXP. V. OF L.F.
C                                 WRT SIGMA (OUTPUT)
C      S2K2(2,2)     R*8       DIMENSIONLESS HESSIAN OF L.F.
C                                 WRT SIGMA**2 (OUTPUT)
C      S2K3(2,2,2)   R*8       DIMENSIONLESS MATRIX OF 3RD DERIVS OF
C                                 WRT SIGMA**2 (OUTPUT)
C      S2K3S(2,2,2)  R*8       DIM. MAT. OF EXP. V. OF L.F.
C                                 WRT SIGMA**2 (OUTPUT)
C
C      SEE PAPER BY COHN [1987] FOR DETAILS, OR SHENTON AND BOWMAN
C        [1977].
C
C======================================================================

       IMPLICIT DOUBLE PRECISION (A-H,K-M,O-Z)

       DIMENSION
     1         K2(2,2),   K3(2,2,2),   K3S(2,2,2),
     1       S2K2(2,2), S2K3(2,2,2), S2K3S(2,2,2)

       XS     =  XSI

       CALL TACIT_CALC(XS,F,P,LOGPHI,A,G)

       OP  =  1.-P

       B  =  -A*(XS+A)
       E  =  -(B*(XS+A)+(1.+B)*A)
       H  =   1.0 + XS*G

       K2(1,1)   =   -OP+P*B
       K2(1,2)   =   -2.*F+P*(XS*B+A)
       K2(2,1)   =   K2(1,2)
       K2(2,2)   =   OP*(1.-3.*H)+P*(XS**2*B+2.*XS*A)

       K3(1,1,1) =  -P*E
       K3(1,1,2) =  OP*2.-P*(XS*E+2.*B)
         K3(1,2,1) =  K3(1,1,2)
         K3(2,1,1) =  K3(1,1,2)
       K3(1,2,2) =  6.*F-P*(XS**2*E+4.*XS*B+2.*A)
         K3(2,1,2) =  K3(1,2,2)
         K3(2,2,1) =  K3(1,2,2)
       K3(2,2,2) =  OP*(-2.+12.*H)-P*(XS**3*E+6.*XS**2*B+6.*XS*A)

       K3S(1,1,1)   =  -(F+P*(A*B+E))
       K3S(1,1,2)   =   XS*K3S(1,1,1)-2.*K2(1,1)
       K3S(1,2,1)   =  -(2.*XS*F+P*(A*(XS*B+A)+2.*B+XS*E))
         K3S(2,1,1)   =  K3S(1,2,1)
       K3S(1,2,2)   =   XS*K3S(1,2,1)-2.*K2(1,2)
         K3S(2,1,2)   =  K3S(1,2,2)
       K3S(2,2,1)   =  -(-F+3.*XS**2*F+P*(2.*XS*B+
     1                       (A*B+E)*XS**2+2.*(A+XS*(A*A+B))))
       K3S(2,2,2)   =   XS*K3S(2,2,1)-2.*K2(2,2)

C======================================================================
C
C      NOW, CALCULATE THE BIASES FOR SIGMA^2 INSTEAD OF SIGMA
C
C======================================================================


       S2K3(1,1,1)    =  K3(1,1,1)
       S2K3(1,1,2)    =  K3(1,1,2)/2.
       S2K3(1,2,1)    =  S2K3(1,1,2)
       S2K3(1,2,2)    =  (K3(1,2,2)-K2(1,2))/4.
       S2K3(2,1,1)    =  S2K3(1,1,2)
       S2K3(2,1,2)    =  S2K3(1,2,2)
       S2K3(2,2,1)    =  S2K3(1,2,2)
       S2K3(2,2,2)    =  (K3(2,2,2)-3.*K2(2,2))/8.0

       S2K3S(1,1,1)   =  K3S(1,1,1)
       S2K3S(1,1,2)   =  K3S(1,1,2)/2.
       S2K3S(1,2,1)   =  K3S(1,2,1)/2.
       S2K3S(1,2,2)   =  (K3S(1,2,2)-K2(1,2))/4.0
       S2K3S(2,1,1)   =  S2K3S(1,2,1)
       S2K3S(2,1,2)   =  S2K3S(1,2,2)
       S2K3S(2,2,1)   =  K3S(2,2,1)/4.
       S2K3S(2,2,2)   =  (K3S(2,2,2)-2.*K2(2,2))/8.0

       S2K2(1,1)      =  K2(1,1)
       S2K2(1,2)      =  K2(1,2)/2.
       S2K2(2,1)      =  S2K2(1,2)
       S2K2(2,2)      =  K2(2,2)/4.

        RETURN
        END
        SUBROUTINE TACIT_R(INO,INP,IDIM,FY,FYD,FX,BETA,LIKE)
C=======================================================================
C
C       SUBROUTINE TO PERFORM CENSORED REGRESSION PROBLEM
C
C       AUTHOR......TIM COHN
C       DATE........APRIL 1, 1987
C       REVISED.....APRIL 14, 1987 (TAC)
C       REVISED.....AUGUST 10, 1989 (TAC)
C       REVISED.....JULY 30, 1990 (TAC)
C
C=======================================================================
C
C      VARIABLE      TYPE           DEFINITION
C      --------      ----           ----------
C      INO           I*4            NUMBER OF OBSERVATIONS
C      INP           I*4            NR. PARAMETERS IN MODEL (INCL.
C                                     CONSTANT)
C      IDIM          I*4            LEADING DIMENSION OF FX
C      FY(NO)        R*8            THE VECTOR OF DEPENDENT OBSERVATIONS
C      FYD(NO)       R*8            THE VECTOR OF CENSORING THRESHOLDS;
C                                     Y IS OBSERVED IFF Y>YD.
C      FX(IDIM,N)    R*8            THE MATRIX OF X.  NOTE THAT X(@,1)
C                                     IS A VECTOR OF 1'S
C      BETA(*)       R*8            VECTOR OF PARAMETER ESTIMATES (OUTPUT)
C      LIKE          R*8            THE * LOGARITHM * OF THE LIKELIHOOD
C                                    ** CHANGED 12/2/92 ** (TAC)
C
C=======================================================================

        IMPLICIT DOUBLE PRECISION (A-H,L-M,Q-Z)
        IMPLICIT INTEGER (I-K,N-P)

        PARAMETER (PPARAMS=25,POBS=2500)

        EXTERNAL TACIT_L,TACIT_G,TACIT_H

        DIMENSION
     1         FY(INO),FYD(INO),FX(IDIM,INO),
     2         BETA(PPARAMS)

        DIMENSION
     1         XSCALE(PPARAMS),IPARAM(7),RPARAM(7)

       COMMON
     1   /BLKI1/ NO,NP
     2   /BLKF1/ X(POBS,PPARAMS)
     3   /BLKF2/ Y(POBS),YD(POBS)

       DATA
     1    XSCALE,FSCALE/PPARAMS*1.D0,1.D0/

               NO      =  INO
               NP      =  INP
       DO 10 I=1,NO
               Y(I)    =  FY(I)
               YD(I)   =  FYD(I)
          DO 10 J=1,NP
               X(I,J)  =  FX(I,J)
   10  CONTINUE
               CALL TACIT_S(NO,NP,Y,YD,BETA)
               IPARAM(1)  =  0

       CALL DUMIAH
     1   (TACIT_L,TACIT_G,TACIT_H,NP+1,BETA,XSCALE,FSCALE,IPARAM,
     2    RPARAM,BETA,FVALUE)

               BETA(NP+1)  =  EXP(BETA(NP+1))
               LIKE        =  -FVALUE

       RETURN
       END
       SUBROUTINE TACIT_CALC(XSI,F,PHI,LOGPHI,A,G)
C======================================================================
C
C      SUBROUTINE TO COMPUTE GAUSSIAN FUNCTIONS AND THEIR
C      DERIVATIVES
C
C        AUTHOR......TIM COHN
C        DATE........APRIL 1, 1987
C          ERROR CORRECTED OCTOBER 4, 1992 (TAC)
C
C======================================================================
       IMPLICIT DOUBLE PRECISION (A-Z)

       DATA LIMIT/6.D0/,CONST/0.3989422D0/

       IF(XSI .LT. -LIMIT) THEN
         PHI       =  0.0
         LOGPHI    =  LOG(-CONST/XSI) - 0.5*XSI**2
         F         =  0.0
         A         =  -XSI
         G         =  0.0
       ELSE IF (XSI .GT. LIMIT) THEN
         PHI       =  1.0
         LOGPHI    =  0.0
         F         =  0.0
         A         =  0.0
         G         =  XSI
       ELSE
          PHI      =  DNORDF(XSI)
         LOGPHI    =  LOG(PHI)
         F         =  CONST*EXP(-0.5*XSI**2)
         A         =  F/PHI
         G         =  F/(1.D0-PHI)
       ENDIF

       RETURN
       END

        SUBROUTINE TACIT_S(NO,NP,Y,YD,BETA)
C=======================================================================
C
C       SUBROUTINE TO GET STARTED IN CENSORED REGRESSION PROBLEM
C
C       AUTHOR......TIM COHN
C       DATE........APRIL 1, 1987
C
C=======================================================================

        IMPLICIT DOUBLE PRECISION (A-H,L-M,O-Z)
        DIMENSION
     1       BETA(NO),Y(NO),YD(NO)

             WSS =  0.D0

        DO 10 I=1,NO
             W   =  MAX(Y(I),YD(I))
             WSS =  WSS+W**2
   10   CONTINUE

        DO 20 K=1,NP
             BETA(K)  =  0.D0
   20   CONTINUE
             BETA(NP+1)    =  0.5*LOG(WSS/NO)

        RETURN
        END
       SUBROUTINE TACIT_L(N,BETA,L)
C=======================================================================
C
C      FUNCTION TO EVALUATE A PARTICULAR PARAMETER SET WITH RESPECT
C      TO LINEAR REGRESSION AND TOBIT MODELS
C      GOES WITH THE IMSL ROUTINE DUMIAH
C
C      AUTHOR........TIM COHN
C      DATE..........APRIL 1, 1987
C      REVISED............APRIL 14, 1987 (TAC)
C      REVISED............AUGUST 8, 1989 (TAC)
C
C=======================================================================
C
C      VARIABLE      TYPE           DEFINITION
C      --------      ----           ----------
C      NO            I*4            NUMBER OF OBSERVATIONS
C      NP            I*4            NUMBER OF PARAMETERS IN MODEL
C      IDUM          I*4            DUMMY ARGUMENT
C      Y(NO)         R*8            THE VECTOR OF DEPENDENT OBSERVATIONS
C      YD(NO)        R*8            THE VECTOR OF CENSORING THRESHOLDS;
C                                     Y IS OBSERVED IFF Y>YD.
C      X(IDIM,N)     R*8            THE MATRIX OF X.  NOTE THAT X(@,1)
C                                     IS A VECTOR OF 1'S
C      BETA(*)       R*8            VECTOR OF PARAMETER ESTIMATES
C      L             R*8            THE VALUE OF THE L.F. GIVEN BETA,X,Y
C                                     AND YD. (OUTPUT)
C======================================================================

       IMPLICIT DOUBLE PRECISION (A-H,L-M,Q-Z)
       IMPLICIT INTEGER (I-K,N-P)

       PARAMETER (PPARAMS=25,POBS=2500)

       DIMENSION BETA(PPARAMS)

       COMMON
     1   /BLKI1/ NO,NP
     2   /BLKF1/ X(POBS,PPARAMS)
     3   /BLKF2/ Y(POBS),YD(POBS)
     4   /BLKF3/ YHAT(POBS)

            L             =  0.D0
            S             =  EXP(BETA(NP+1))

       DO 10 I=1,NO
                YHAT(I)   =  0.D0
           DO 20 K=1,NP
                YHAT(I)   =  YHAT(I) + X(I,K)*BETA(K)
   20      CONTINUE

       IF(Y(I) .LT. YD(I)) THEN
         XSI       =  (YD(I)-YHAT(I))/S
         CALL TACIT_CALC(XSI,F,DPHI,LOGPHI,A,DUM1)
         LT         =  LOGPHI
       ELSE
         Z         =  (Y(I)-YHAT(I))/S
         LT         = -LOG(S) - Z**2/2.D0
       ENDIF

C======================================================================
C
C      CONSTRUCT NEGATIVES FOR IMSL ROUTINE DUMIAH (A MINIMIZER)
C
C======================================================================

                L         =  L - LT
   10  CONTINUE

       RETURN
       END
       SUBROUTINE TACIT_G(N,BETA,G)
C=======================================================================
C
C      FUNCTION TO EVALUATE A PARTICULAR PARAMETER SET WITH RESPECT
C      TO LINEAR REGRESSION AND TOBIT MODELS
C      GOES WITH THE IMSL ROUTINE DUMIAH
C
C      AUTHOR........TIM COHN
C      DATE..........APRIL 1, 1987
C      REVISED............APRIL 14, 1987 (TAC)
C      REVISED............AUGUST 8, 1989 (TAC)
C
C=======================================================================
C
C      VARIABLE      TYPE           DEFINITION
C      --------      ----           ----------
C      NO            I*4            NUMBER OF OBSERVATIONS
C      NP            I*4            NUMBER OF PARAMETERS IN MODEL
C      IDUM          I*4            DUMMY ARGUMENT
C      Y(NO)         R*8            THE VECTOR OF DEPENDENT OBSERVATIONS
C      YD(NO)        R*8            THE VECTOR OF CENSORING THRESHOLDS;
C                                     Y IS OBSERVED IFF Y>YD.
C      X(IDIM,N)     R*8            THE MATRIX OF X.  NOTE THAT X(@,1)
C                                     IS A VECTOR OF 1'S
C      BETA(*)       R*8            VECTOR OF PARAMETER ESTIMATES
C      G             R*8            THE VALUE OF THE L.F. GIVEN BETA,X,Y
C                                     AND YD. (OUTPUT)
C======================================================================

       IMPLICIT DOUBLE PRECISION (A-H,L-M,Q-Z)
       IMPLICIT INTEGER (I-K,N-P)

       PARAMETER (PPARAMS=25,POBS=2500)

       DIMENSION BETA(PPARAMS),G(PPARAMS),GT(2)

       COMMON
     1   /BLKI1/ NO,NP
     2   /BLKF1/ X(POBS,PPARAMS)
     3   /BLKF2/ Y(POBS),YD(POBS)
     4   /BLKF3/ YHAT(POBS)

            S             =  EXP(BETA(NP+1))
            S_1           =  1.D0/S
       DO 5 I=1,NP+1
            G(I)          =  0.D0
    5  CONTINUE

       DO 10 I=1,NO
                YHAT(I)   =  0.0
           DO 20 K=1,NP
                YHAT(I)   =  YHAT(I) + X(I,K)*BETA(K)
   20      CONTINUE

       IF(Y(I) .LT. YD(I)) THEN
         XSI       =  (YD(I)-YHAT(I))/S
         CALL TACIT_CALC(XSI,F,DPHI,LOGPHI,A,DUM1)
         D1        = -S_1
         D2        = -XSI*S_1

         GT(1)     =  D1*A
         GT(2)     =  D2*A

       ELSE
         Z         =  (Y(I)-YHAT(I))/S
         GT(1)     =  S_1*Z
         GT(2)     =  S_1*(Z**2-1.D0)
       ENDIF

C======================================================================
C
C      CONSTRUCT NEGATIVES FOR IMSL ROUTINE DUMIAH (A MINIMIZER)
C      N.B.  SINCE WE ARE OPTIMIZING WITH EXP(S), MUST ADJUST DERIVS.
C
C======================================================================

       DO 30 K1=1,NP
                G(K1)     =  G(K1) - GT(1)*X(I,K1)
   30  CONTINUE
                G(NP+1)   =  G(NP+1) - S*GT(2)
   10  CONTINUE

       RETURN
       END
       SUBROUTINE TACIT_H(N,BETA,H,PDH)
C=======================================================================
C
C      FUNCTION TO EVALUATE A PARTICULAR PARAMETER SET WITH RESPECT
C      TO LINEAR REGRESSION AND TOBIT MODELS
C      GOES WITH THE IMSL ROUTINE DUMIAH
C
C      AUTHOR........TIM COHN
C      DATE..........APRIL 1, 1987
C      REVISED............APRIL 14, 1987 (TAC)
C      REVISED............AUGUST 8, 1989 (TAC)
C
C=======================================================================
C
C      VARIABLE      TYPE           DEFINITION
C      --------      ----           ----------
C      NO            I*4            NUMBER OF OBSERVATIONS
C      NP            I*4            NUMBER OF PARAMETERS IN MODEL
C      IDUM          I*4            DUMMY ARGUMENT
C      Y(NO)         R*8            THE VECTOR OF DEPENDENT OBSERVATIONS
C      YD(NO)        R*8            THE VECTOR OF CENSORING THRESHOLDS;
C                                     Y IS OBSERVED IFF Y>YD.
C      X(IDIM,N)     R*8            THE MATRIX OF X.  NOTE THAT X(@,1)
C                                     IS A VECTOR OF 1'S
C      BETA(*)       R*8            VECTOR OF PARAMETER ESTIMATES
C      H             R*8            THE VALUE OF THE L.F. GIVEN BETA,X,Y
C                                     AND YD. (OUTPUT)
C      PDH           I*4            LEADING DIMENSION OF H
C======================================================================

       IMPLICIT DOUBLE PRECISION (A-H,L-M,Q-Z)
       IMPLICIT INTEGER (I-K,N-P)

       PARAMETER (PPARAMS=25,POBS=2500)

       DIMENSION BETA(PPARAMS),HT(PPARAMS,PPARAMS),H(NP+1,NP+1)
C      DIMENSION BETA(PPARAMS),HT(PPARAMS,PPARAMS),H(PDH,PDH)
C      correction for array bounds error - pbd

       COMMON
     1   /BLKI1/ NO,NP
     2   /BLKF1/ X(POBS,PPARAMS)
     3   /BLKF2/ Y(POBS),YD(POBS)
     4   /BLKF3/ YHAT(POBS)

            S             =  EXP(BETA(NP+1))
            S_1           =  1.D0/S
            S_2           =  S_1**2
       DO 5 K1=1,NP+1
       DO 5 K2=1,NP+1
            H(K1,K2)      =  0.D0
    5  CONTINUE

       DO 10 I=1,NO
                YHAT(I)   =  0.0
           DO 20 K=1,NP
                YHAT(I)   =  YHAT(I) + X(I,K)*BETA(K)
   20      CONTINUE

       IF(Y(I) .LT. YD(I)) THEN
         XSI       =  (YD(I)-YHAT(I))/S
         CALL TACIT_CALC(XSI,F,DPHI,LOGPHI,A,DUM1)
         B         = -A*(XSI+A)

         D1        = -S_1
         D2        = -XSI*S_1
         D11       =  0.D0
         D12       =  S_2
         D22       =  2.D0*XSI*S_2

         GT2       =  D2*A

         HT(1,1)   =  D11*A + D1*D1*B
         HT(1,2)   =  D12*A + D1*D2*B
         HT(2,2)   =  D22*A + D2*D2*B

       ELSE
         Z         =  (Y(I)-YHAT(I))/S

         GT2       =  S_1*(Z**2-1.D0)

         HT(1,1)   = -S_2
         HT(1,2)   = -S_2*(2.D0*Z)
         HT(2,2)   = -S_2*(3.D0*Z**2-1.D0)
       ENDIF

C======================================================================
C
C      CONSTRUCT NEGATIVES FOR IMSL ROUTINE DUMIAH (A MINIMIZER)
C
C======================================================================

       DO 30 K2=1,NP
                H(NP+1,K2)   =  H(NP+1,K2) - S*HT(1,2)*X(I,K2)
                H(K2,NP+1)   =  H(NP+1,K2)
         DO 30 K1=1,NP
                H(K1,K2)     =  H(K1,K2) - HT(1,1)*X(I,K1)*X(I,K2)
   30  CONTINUE
                H(NP+1,NP+1) =  H(NP+1,NP+1) - S*(GT2 + S*HT(2,2))
   10  CONTINUE

       RETURN
       END
       SUBROUTINE TACIT_TEST
     1    (YI,NO,YDI,XI,IDIM,NPC,ITEST,B_TEST,B_MLE,XLIKE,B_AMLE)
C=======================================================================
C
C      COMPUTES LIKELIHOOD RATIO TESTS FOR AMLE PARAMETER ESTIMATES
C
C
C      AUTHOR........TIM COHN
C      DATE..........FEBRUARY 12, 1992
C
C
C      ***** THIS FORTRAN CODE MAY NOT BE MODIFIED WITHOUT THE *****
C      *****              AUTHOR'S APPROVAL                    *****
C
C=======================================================================
C
C      VARIABLE      TYPE           DEFINITION
C      --------      ----           ----------
C      YI(NO)        R*8            THE VECTOR OF DEPENDENT OBSERVATIONS
C      NO            I*4            NUMBER OF OBSERVATIONS
C      YDI(NO)       R*8            THE VECTOR OF CENSORING THRESHOLDS;
C                                     Y IS OBSERVED IFF Y>=YD.
C      XI(IDIM,N)    R*8            THE MATRIX OF X.  NOTE THAT X(@,1)
C                                     IS A VECTOR OF 1'S
C      IDIM          I*4            LEADING DIMENSION OF Y, X
C      NPC           I*4            NUMBER OF EXPL. VARIABLES IN
C                                     COMPLEX MODEL
C      ITEST(*)      I*4            INPUT TO IDENTIFY WHICH VARS TO
C                                     USE AND HOW
C      B_TEST(*)     R*8            VECTOR OF FIXED PARAMETER ESTIMATES
C                                     H0 FOR HYPOTHESIS TEST
C      B_MLE(*)      R*8            VECTOR OF MLE PARAMETER ESTIMATES
C                                     N.B. B_MLE(NP+1) CONTAINS S**2-HAT
C      XLIKE         R*8            VALUE OF THE LOG-LIKELIHOOD FUNCTION
C      B_TEST(*)     R*8            VECTOR OF FIXED PARAMETER ESTIMATES
C                                     TO BE USED IN TESTING B=B0.
C      B_AMLE(*)     R*8            VECTOR OF AMLE PARAMETER ESTIMATES
C      CV_M(25,25)   R*8            MLE'S OF THE COVARIANCE MATRIX (0UTPUT)
C                                     N.B. CV_M(NP+1) REFERS TO COVARIANCE
C                                     WITH RESPECT TO S**2
C      CV_A(25,25)   R*8            AMLE'S OF THE COVARIANCE MATRIX
C                                     WRT S**2 (0UTPUT)
C      DF            R*8            ESTIMATED NUMBER OF DEGREES OF
C                                     FREEDOM IN ERROR DISTRIBUTION
C      BIAS(25)      R*8            BIAS OF MLE'S WRT S**2 (0UTPUT)
C      CV(25,25)     R*8            COVARIANCE MATRIX OF STANDARD NORMALS
C                                     CENSORED AT THE XSI'S WRT. S**2 (0UTPUT)
C      SBIAS(25)     R*8            BIAS OF MLE'S WRT S (0UTPUT)
C      SCV(25,25)    R*8            COVARIANCE MATRIX OF STANDARD NORMALS
C                                     CENSORED AT THE XSI'S WRT. S (0UTPUT)
C
C=======================================================================
C
C         IF ITEST(J) = 1      INCLUDE BETA(J) AT FIXED VALUE B_TEST(J)
C         IF ITEST(J) = 0      FIT BETA(J)
C
C=======================================================================

       IMPLICIT DOUBLE PRECISION (A-H,M,Q-Z)
       IMPLICIT INTEGER (I-L,N-P)

       PARAMETER (PPARAMS=25,POBS=2500)

       DIMENSION
     1         B_MLE(PPARAMS),B_AMLE(NPC),BIAS(PPARAMS),SBIAS(PPARAMS)

       DIMENSION
     1         YI(IDIM),YDI(IDIM),XI(IDIM,PPARAMS)

       DIMENSION
     1         CV_M(PPARAMS,PPARAMS),CV_A(PPARAMS,PPARAMS),
     2         CV(PPARAMS,PPARAMS),SCV(PPARAMS,PPARAMS)

       DIMENSION
     1         X(POBS,PPARAMS),Y(POBS),YD(POBS)

       DIMENSION
     1         ITEST(PPARAMS),B_TEST(PPARAMS)

         NP2FIT      =  0
         CALL DCOPY(NO,YI,1,Y,1)
         CALL DCOPY(NO,YDI,1,YD,1)
       DO 10 J=1,NPC
         IF(ITEST(J) .EQ. 0) THEN
           NP2FIT    =  NP2FIT + 1
           CALL DCOPY(NO,XI(1,J),1,X(1,NP2FIT),1)
         ELSE
           CALL DAXPY(NO,-B_TEST(J),XI(1,J),1,Y,1)
           CALL DAXPY(NO,-B_TEST(J),XI(1,J),1,YD,1)
         ENDIF
   10  CONTINUE

       CALL TACIT
     1    (Y,NO,YD,X,POBS,NP2FIT,1,B_MLE,XLIKE,B_AMLE,
     2     CV_M,CV_A,DF,BIAS,CV,SBIAS,SCV)

       RETURN
       END
       DOUBLE PRECISION FUNCTION TACIT_PVALUE
     1    (DIF_PARMS,XLIKE_S,XLIKE_C)
C=======================================================================
C
C      COMPUTES LIKELIHOOD RATIO TEST FOR AMLE PARAMETER ESTIMATES
C
C
C      AUTHOR........TIM COHN
C      DATE..........FEBRUARY 11, 1992
C
C=======================================================================
C
C      VARIABLE      TYPE           DEFINITION
C      --------      ----           ----------
C      DIF_PARMS     R*8            DIFFERENCES IN NO. PARMS IN S VS. C
C      XLIKE_S       R*8            LOG-L. ASSOC. WITH SIMPLE MODEL (S)
C      XLIKE_C       R*8            LOG-L. ASSOC. WITH COMPLEX MODEL (C)
C      TACIT_PVALUE  R*8            RETURNED APPROX. P VALUE
C
C	** NOTE:  PROGRAMS MODIFIED 12/2/92 TO USE LOG-LIKELIHOOD, NOT L. **
C
C=======================================================================

       IMPLICIT DOUBLE PRECISION (A-H,M,Q-Z)
       IMPLICIT INTEGER (I-L,N-P)

       IF(XLIKE_C .LE. XLIKE_S) THEN
         TACIT_PVALUE    =  1.D0
       ELSE
         XL              =  -2.D0*(XLIKE_S-XLIKE_C)
         TACIT_PVALUE    =  1.D0-DCHIDF(XL,DIF_PARMS)
       ENDIF
       RETURN
       END
       SUBROUTINE TAC_LOAD
     1    (NP,B_MLE,BIAS,CV,SBIAS,SCV,M,X,LDX,XLOAD,XTOT,XVARTOT)
C=======================================================================
C
C      COMPUTES NEARLY-UNBIASED AMLE LOAD ESTIMATES
C
C
C      AUTHOR........TIM COHN
C      DATE..........FEBRUARY 5, 1992
C       MODIFIED.....MAY 20, 1992 (TAC)
C
C      ***** THIS FORTRAN CODE MAY NOT BE MODIFIED WITHOUT THE *****
C      *****              AUTHOR'S APPROVAL                    *****
C
C	NOTES:  THE CODE ASSUMES THE PARAMETER VALUES HAVE BEEN ESTIMATED
C           FROM DATA.  IT ADJUSTS THE INCOMING MLE ESTIMATES TO THE
C           AMLE ESTIMATES BEFORE COMPUTING THE VARIANCES.
C           IF THIS IS USED IN A MONTE CARLO CONTEXT, THE TRUE PARAMETER
C           VALUES SHOULD BE USED UNCORRECTED FOR BIAS.
C
C=======================================================================
C
C      VARIABLE      TYPE           DEFINITION
C      --------      ----           ----------
C      NP            I*4            NUMBER OF PARAMETERS IN MODEL (IN)
C      B_MLE(*)      R*8            VECTOR OF MLE PARM. ESTIMATES (IN)
C                                     N.B. B_MLE(NP+1) CONTAINS S**2-HAT
C      BIAS(10)      R*8            BIAS OF MLE'S WRT S**2 (IN)
C      CV(10,10)     R*8            COVARIANCE MATRIX OF STANDARD NORMALS
C                                     CENSORED AT XSI'S WRT. S**2 (IN)
C      SBIAS(10)     R*8            BIAS OF MLE'S WRT S (IN)
C      SCV(10,10)    R*8            COVARIANCE MATRIX OF STANDARD NORMALS
C                                     CENSORED AT XSI'S WRT. S (IN)
C      M             I*4            NUMBER OF PREDICTORS IN ESTIMATION
C                                     DATA SET (M)  (IN)
C      X(LDX,NP)     R*8            THE MATRIX OF PREDICTORS (IN)
C      LDX           I*4            LEADING DIMENSION OF X (IN)
C      XLOAD(M)      R*8            THE VECTOR OF ESTIMATED LOADS (OUT)
C      XTOT          R*8            THE SUM OF ESTIMATED LOADS (OUT)
C      XVARTOT       R*8            THE VARIANCE OF XTOT (OUT)
C
C=======================================================================

        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        IMPLICIT INTEGER (I-N)
        DOUBLE PRECISION KAPPA

        INTEGER PPARAMS,POBS
        PARAMETER (PPARAMS=25,POBS=2500)

        DIMENSION
     1         B_MLE(PPARAMS),
     2         BIAS(PPARAMS),CV(PPARAMS,PPARAMS),
     3         SBIAS(PPARAMS),SCV(PPARAMS,PPARAMS)

        DIMENSION
     1         GAMMA(PPARAMS),B(PPARAMS),OMEGA(PPARAMS),
     2         C(PPARAMS,PPARAMS),XC(PPARAMS),
     3         XXC(PPARAMS),
     4         A1(POBS),B1(POBS),XX(PPARAMS)

        DIMENSION
     1         X(LDX,*),XLOAD(M)

C=======================================================================
C
C       DEFINE THE PARAMETERS, AND OMEGA
C
          S2         =  B_MLE(NP+1)
          S          =  SQRT(S2)
        DO 10 I=1,NP
          GAMMA(I)   =  SCV(I,NP+1)/SCV(NP+1,NP+1)
          OMEGA(I)   =  B_MLE(I) - GAMMA(I)*S
          B(I)       =  SBIAS(I)-GAMMA(I)*(1.D0+SBIAS(NP+1))
   10   CONTINUE

        DO 20 I=1,NP
          DO 20 J=1,NP
            C(I,J)   =  SCV(I,J) - SCV(NP+1,NP+1)*GAMMA(I)*GAMMA(J)
   20   CONTINUE

          ALPHA        =  (1.D0+BIAS(NP+1))**2/CV(NP+1,NP+1)
          KAPPA        =  (1.D0+BIAS(NP+1))/ALPHA

C=======================================================================
C
C     COMPUTE THE INDIVIDUAL LOADS
C
        DO 30 I=1,M
            AT           =  0.D0
          DO 40 J=1,NP
            AT         =  AT + X(I,J)*B(J)
   40     CONTINUE
            A1(I)        =  -AT
C
          DO 50 I2=1,NP
              XC(I2)     =  0.D0
            DO 50 J=1,NP
              XC(I2)     =  XC(I2) + X(I,J)*C(J,I2)
   50     CONTINUE
            XCX          =  0.D0
          DO 60 J=1,NP
            XCX          =  XCX + XC(J)*X(I,J)
   60     CONTINUE
            B1(I)        =  (1.D0-XCX)/2.D0
C
            XOMEGA       =  0.D0
          DO 70 J=1,NP
            XOMEGA       =  XOMEGA + X(I,J)*OMEGA(J)
   70     CONTINUE

            T1           =  EXP(XOMEGA)
            T2           =  EXPON(S2,ALPHA,KAPPA,B1(I),A1(I))
            XLOAD(I)     =  T1*T2
   30   CONTINUE

C=======================================================================
C
C     COMPUTE THE SUM OF THE LOADS, AND THEIR VARIANCE
C       N.B.  USE THE AMLE ESTIMATE FOR S^2, IN PLACE OF TRUE VALUE
         S2_U            =  S2/(1.D0+BIAS(NP+1))
         S_U             =  SQRT(S2_U)
         XTOT            =  0.D0
         XVARTOT         =  0.D0
       DO 80 I=1,M
           XTOT          =  XTOT + XLOAD(I)
         DO 80 J=1,I
             XXBSB       =  0.D0
             XXB         =  0.D0
           DO 90 K=1,NP
             XX(K)       =  X(I,K) + X(J,K)
             XXBSB       =  XXBSB + XX(K)*(B_MLE(K) + S_U*B(K))
             XXB         =  XXB + XX(K)*B_MLE(K)
   90      CONTINUE
C
          DO 100 I2=1,NP
              XXC(I2)    =  0.D0
            DO 100 J2=1,NP
              XXC(I2)    =  XXC(I2) + XX(J2)*C(J2,I2)
  100     CONTINUE
            XXCXX        =  0.D0
          DO 110 J2=1,NP
            XXCXX        =  XXCXX + XXC(J2)*XX(J2)
  110     CONTINUE
C
            T1           =  EXP(XXBSB + XXCXX*S2_U/2.D0)
            T2           =  EXPVAR(S2_U,ALPHA,KAPPA,B1(I),A1(I),
     1                                           B1(J),A1(J))
            T3           =  EXP(XXB+S2_U)

            COVAR        =  T1*T2 - T3
          IF(I .EQ. J) THEN
            XVARTOT      =  XVARTOT + COVAR
          ELSE
            XVARTOT      =  XVARTOT + 2.D0*COVAR
          ENDIF
   80  CONTINUE

          RETURN
          END
        DOUBLE PRECISION FUNCTION EXPON(S2,ALPHA,BETA,A,B)
C=======================================================================
C
C      COMPUTES AN UNBIASED ESTIMATE OF EXP(B*SIGMA+A*SIGMA^2)
C      ASSUMING THAT S2 IS A GAMMA(ALPHA,SIGMA^2*BETA) RV
C
C      THE RESULT IS EXACT ASIDE FROM MACHINE ERROR
C
C
C      AUTHOR........Timothy A. Cohn
C      DATE..........Monday, April 23, 1990 (TAC)
C      Modified......February 10, 1992 (TAC)
C
C      ***** THIS FORTRAN CODE MAY NOT BE MODIFIED WITHOUT THE *****
C      *****              AUTHOR'S APPROVAL                    *****
C
C=======================================================================
C
C      VARIABLE      TYPE           DEFINITION
C      --------      ----           ----------
C      S2            R*8            THE SAMPLE ESTIMATE OF SIGMA^2
C      ALPHA         R*4            PARAMETER OF GAMMA DISTRIBUTION
C                                     = (N-1)/2 IN STANDARD MODEL
C      BETA          R*8            PARAMETER OF GAMMA DISTRIBUTION
C                                     = 2/(N-1) IN STANDARD MODEL
C      A             R*8            COEFFICIENT IN FRONT OF SIGMA^2
C      B             R*8            COEFFICIENT IN FRONT OF SIGMA
C      EXPON         R*8            RETURNED VALUE (OUTPUT)
C                                     E[EXPON] = EXP(B*SIGMA+A*SIGMA^2)
C
C======================================================================

       IMPLICIT DOUBLE PRECISION (A-H,Q-Z)
       IMPLICIT INTEGER (I-P)

       DIMENSION C(0:1000),SK_UNB(0:1000)

       DATA TOL/1.D-15/

         ARG        =  ABS(A)*S2 + ABS(B)*SQRT(S2)
         N          =  MIN(998.D0,30.D0 + ARG*
     1                 (5.4-0.2*ARG+ABS(A)*
     2                 (8.28-1.11*ABS(A)+1.98*ARG)+1.33*ABS(B)))

    1    CONTINUE
         SUM        =  0.D0

         SK_UNB(0)  =  1.D0
         SK_UNB(1)  =  SQRT(S2/BETA)
     1                   *EXP(DLNGAM(ALPHA)-DLNGAM(ALPHA+0.5D0))


         CALL SQUARE(N,A,B,C)

        IF(ABS(B) .LT. TOL) THEN
          ISTEP  =  2
        ELSE
          ISTEP  =  1
        ENDIF

        DO 10 K=0,N,ISTEP
           DELTA    =  C(K)*SK_UNB(K)
           SUM      =  SUM + DELTA
           IF(ABS(DELTA) .LT. TOL) GOTO 50
            SK_UNB(K+2)=  SK_UNB(K)*(S2/BETA)/(ALPHA+0.5D0*K)
   10   CONTINUE
            GOTO 99
   50   CONTINUE
         EXPON      =  SUM
         RETURN

   99   CONTINUE
            N        =  N + 10
         IF(N .LT. 998) GOTO 1
C======================================================================
C
C        ERROR:  FAILURE TO CONVERGE IN 998 TERMS
C

         WRITE(*,*) 'FAILURE TO CONVERGE IN EXPON'
         RETURN
         END
C=======================================================================
        DOUBLE PRECISION FUNCTION EXPVAR(S2,ALPHA,BETA,A1,B1,A2,B2)
C=======================================================================
C
C      ESTIMATES THE COVARIANCE OF TWO EXPON ESTIMATORS OF THE FORM
C           EXPON(SIGMA^2,A,B)
C      ASSUMING THAT SIGMA^2 IS A GAMMA(ALPHA,S2*BETA) RV
C
C      THE RESULT IS EXACT ASIDE FROM MACHINE ERROR
C
C
C      AUTHOR........TIM COHN
C      DATE..........Friday, April 20, 1990 (TAC)
C      MODIFIED......February 5, 1992 (TAC)
C      MODIFIED......MAY 20, 1992 (TAC)
C
C      ***** THIS FORTRAN CODE MAY NOT BE MODIFIED WITHOUT THE *****
C      *****              AUTHOR'S APPROVAL                    *****
C
C=======================================================================
C
C      VARIABLE      TYPE           DEFINITION
C      --------      ----           ----------
C      S2            R*8            THE SAMPLE ESTIMATE OF SIGMA^2
C      ALPHA         R*4            PARAMETER OF GAMMA DISTRIBUTION
C                                     = (N-1)/2 IN STANDARD MODEL
C      BETA          R*8            PARAMETER OF GAMMA DISTRIBUTION
C                                     = 2/(N-1) IN STANDARD MODEL
C      A1            R*8            1 COEFFICIENT IN FRONT OF SIGMA^2
C      B1            R*8            1 COEFFICIENT IN FRONT OF SIGMA
C      A2            R*8            2 COEFFICIENT IN FRONT OF SIGMA^2
C      B2            R*8            2 COEFFICIENT IN FRONT OF SIGMA
C      EXPVAR        R*8            RETURNED VALUE
C
C======================================================================

       IMPLICIT DOUBLE PRECISION (A-H,Q-Z)
       IMPLICIT INTEGER (I-P)

       DIMENSION C1(0:1000),C2(0:1000),GAMM(0:1000)

       DATA TOL/1.D-6/

         SIGMA      =  SQRT(S2)

         ARG1       =  ABS(A1)*S2 + ABS(B1)*SIGMA
         ARG2       =  ABS(A2)*S2 + ABS(B2)*SIGMA
         ARG        =  MAX(ARG1,ARG2)
         A          =  MAX(ABS(A1),ABS(A2))
         B          =  MAX(ABS(B1),ABS(B2))
         N          =  MIN(998.D0,30.D0 + ARG*
     1                 (5.4-0.2*ARG+A*(8.28-1.11*A+1.98*ARG)+1.33*B))

    1    CONTINUE
           SUM        =  0.D0
         CALL SQUARE(N,A1,B1,C1)
         CALL SQUARE(N,A2,B2,C2)

            GAMM(0) =  1.D0
            GAMM(1) =  EXP(DLNGAM(ALPHA+0.5D0)-DLNGAM(ALPHA))
        DO 10 J=0,N
            GAMM(J+2)  =  (ALPHA+0.5D0*J)*GAMM(J)
            TSUM       =  0.D0
          DO 30 I=0,J
            TSUM    =  TSUM+C1(I)*C2(J-I)*
     1                   GAMM(J)/(GAMM(I)*GAMM(J-I))
   30     CONTINUE
            DELTA   =  SIGMA**J*TSUM
            SUM     =  SUM+DELTA

   10    CONTINUE
         IF(DELTA .GT. TOL*SUM) GOTO 99

         EXPVAR     =  SUM
         RETURN
   99   CONTINUE
          N         =  N + 10
        IF(N .LT. 998) GOTO 1
C======================================================================
C
C        ERROR:  FAILURE TO CONVERGE IN 998 TERMS
C
         WRITE(*,*) 'FAILURE TO CONVERGE IN EXPVAR'
         RETURN
         END
C=======================================================================
        SUBROUTINE SQUARE(N,A,B,C)
C=======================================================================
C
C      COMPUTES THE FIRST N COEFFICIENTS, C(K), SUCH THAT:
C
C         SUM(C(K)*X**K,K,0,INF)  =  EXP(A*X**2+B*X)
C
C      THE RESULT IS EXACT ASIDE FROM MACHINE ERROR
C
C
C      AUTHOR........Timothy A. Cohn
C      DATE..........Monday, April 23, 1990 (TAC)
C
C      ***** THIS FORTRAN CODE MAY NOT BE MODIFIED WITHOUT THE *****
C      *****              AUTHOR'S APPROVAL                    *****
C
C=======================================================================
C
C      VARIABLE      TYPE           DEFINITION
C      --------      ----           ----------
C      N             I*4            NUMBER OF COEFFICIENTS TO COMPUTE
C      A             R*8            COEFFICIENT IN FRONT OF SIGMA^2
C      B             R*8            COEFFICIENT IN FRONT OF SIGMA
C      C(N)          R*8            VECTOR OF COEFFICIENTS (OUTPUT)
C
C      N.B.  C(J) IS NOT SET TO ZERO FOR J>N
C
C======================================================================

       IMPLICIT DOUBLE PRECISION (A-H,Q-Z)
       IMPLICIT INTEGER (I-P)
       DIMENSION C(0:1000),T0(0:1000)

       DATA TOL1/1.D-15/,TOL2/1.D-15/

        IF(ABS(A) .GT. TOL1) THEN
         IF(ABS(B) .GT. TOL1) THEN
C======================================================================
C
C        CASE I:  BOTH A AND B ARE NON-ZERO
C
            T0(0)      =  1.D0
            TOLMIN     =  0.D0
            T2         =  B**2/A
          DO 25 K=0,N
            C(K)     =  0.D0
            T1       =  T0(K)
           DO 20 L=K/2,0,-1
              C(K)     =  C(K)+T1
              T1       =  T1*T2*L/((K-2.D0*L+2.D0)*(K-2.D0*L+1.D0))
              IF(ABS(T1) .LT. TOLMIN) GOTO 24
   20      CONTINUE
   24      CONTINUE
            IF(MOD(K,2) .EQ. 0) THEN
              T0(K+1)  =  T0(K)*B
              T0(K+2)  =  T0(K)*A/(0.5D0*(K+2.D0))
              IF(K .GT. 1)
     1          TOLMIN   =  TOL2*MIN( ABS(C(K)),ABS(C(K-1)) )
            ENDIF
   25     CONTINUE
            GOTO 60
         ELSE
C======================================================================
C
C        CASE II:  A IS NON-ZERO; B IS ZERO
C
            C(0)       =  1.D0
          DO 35 K=0,N,2
            C(K+1)     =  0.D0
            C(K+2)     =  C(K)*A/(0.5D0*(K+2.D0))
   35     CONTINUE
            GOTO 60
         ENDIF
        ELSE
         IF(ABS(B) .GT. 0.D0) THEN
C======================================================================
C
C        CASE III:  B IS NON-ZERO; A IS ZERO
C
            C(0)       =  1.D0
          DO 45 K=0,N
            C(K+1)     =  C(K)*B/(K+1.D0)
   45     CONTINUE
            GOTO 60
         ELSE
C======================================================================
C
C        CASE IV:  BOTH A AND B ARE ZERO--PRETTY SILLY
C
             C(0)      =  1.D0
           DO 55 K=1,N
             C(K)      =  0.D0
   55      CONTINUE
         ENDIF
        ENDIF
   60    CONTINUE
         RETURN
         END

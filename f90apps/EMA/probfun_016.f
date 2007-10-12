C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     SET OF SUBROUTINES TO DEAL WITH PROBABILITY FUNCTIONS
C
C     COPYRIGHT, TIMOTHY A. COHN, 1995
C     PROPERTY OF US GOVERNMENT, GEOLOGICAL SURVEY
C
C     *** DO NOT MODIFY WITHOUT AUTHOR'S CONSENT ***
C     
C
C     AUTHOR.......TIM COHN
C           DATE.....JANUARY 5, 1995
C
C     N.B.  IMSL SUBROUTINES ARE USED:
C                 DGAMMA
C                 DGAMDF
C                 DNORDF
C                 DNORIN
C                 DCHIIN
C
C
C     ROUTINES AVAILABLE
C           STANDARD NORMAL DISTRIBUTION
C                 FP_Z_PDF(X)
C                 FP_Z_PDF_LN(X)
C                 FP_Z_PDF_TRA(X,T)
C                 FP_Z_CDF(X)
C                 FP_Z_CDF_TRA(X,T)
C                 FP_Z_ICDF(P)
C                 FP_Z_ICDF_TRA(P,T)
C                 FP_Z_MOM(K)
C                 FP_Z_MOM_TRA(K,T)
C                 FP_Z_MOM_TRB(K,T)
C                 SP_Z(X,F,PHI,LOGPHI,A,G)
C
C           2 PARAMETER NORMAL
C                 FP_N2_PDF(X,PARMS)
C                 FP_N2_CDF(X,PARMS)
C                 FP_N2_ICDF(P,PARMS)
C
C           GAMMA DISTRIBUTION
C                 FP_G1_ICDF(P,ALPHA)
C                 FP_G2_ICDF(P,PARMS)  -- PARMS=(ALPHA,BETA)
C                 FP_G3_ICDF(P,PARMS)  -- PARMS=(TAU,ALPHA,BETA)
C                 FP_G1_MOM_TRA(ALPHA,T,K) 
C                 FP_G2_MOM_TRA(PARMS,T,K) 
C                 FP_G3_MOM_TRA(PARMS,T,K) 
C
C     NAMING CONVENTIONS:
C
C     1)    TYPE OF ROUTINE
C                 FP_         =     DOUBLE PRECISION FUNCTIONS
C                 SP_         =     SUBROUTINES
C
C     2)    NEXT COMES LETTERS DEFINING THE DISTRIBUTION:
C
C                 Z_          =     PHI(X)                              STANDARD NORMAL DISTRIBUTION
C                 N_          =     N(MU,SIGMA)                   2-P NORMAL DISTRIBUTION 
C                 G1_         =     GAMMA(X,ALPHA)                1-P GAMMA DISTRIBUTION 
C                 G2_         =     GAMMA(X,ALPHA,BETA)           2-P GAMMA DISTRIBUTION 
C                 G3_         =     GAMMA(X,ALPHA,BETA,TAU) 3-P GAMMA DISTRIBUTION 
C
C     3)    NEXT COMES FUNCTION TYPE:
C                 
C                 PDF_        =     PROBABILITY DENSITY FUNCTION
C                 CDF_        =     CUMULATIVE DENSITY FUNCTION
C                 ICDF_       =     INVERSE CDF
C                 MOM_        =     MOMENTS
C                 HAZ_        =     F/(1-F) 
C                 RAT_        =     F/F
C
C     4)    NEXT COMES WHETHER LOGS ARE USED
C
C                 LN_               =     USE NATURAL LOG
C
C     5)    NEXT COMES FUNCTION TYPE
C
C                 JAC_  =     JACOBIAN (VECTOR OF FIRST DERIVATIVES WRT PARAMS)
C                 HESS_ =     HESSIAN OF PDF (MATRIX OF SECOND DERIVATIVES WRT PARAMS)
C                 
C     6)    NEXT COMES TRUNCATION
C
C                 TRA_  =     DISTRIBUTION TRUNCATED ABOVE (I.E. ALL VALUES "LESS THAN")
C                 TRB_  =     DISTRIBUTION TRUNCATED BELOW
C 
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_Z_PDF(X)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE PDF OF STANDARD NORMAL DISTRIBUTION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      CALL SP_Z(X,F,PHI,XLNPHI,RAT,HAZ)
      
      FP_Z_PDF    =     F
      
      RETURN
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_Z_CDF(X)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE CDF OF STANDARD NORMAL DISTRIBUTION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      CALL SP_Z(X,F,PHI,XLNPHI,RAT,HAZ)
      
      FP_Z_CDF    =     PHI
      
      RETURN
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_Z_ICDF(P)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE INVERSE CDF OF STANDARD NORMAL DISTRIBUTION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      FP_Z_ICDF   =     DNORIN(P)
      
      RETURN
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_N2_PDF(X,PARMS)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE PDF OF STANDARD NORMAL DISTRIBUTION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARMS(2),MU,SIGMA

            MU          =     PARMS(1)
            SIGMA =     PARMS(2)    
            Z     =     (X-MU)/SIGMA
      CALL SP_Z(Z,F,PHI,XLNPHI,RAT,HAZ)
      
      FP_N2_PDF   =     F/SIGMA
      
      RETURN
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_N2_CDF(X,PARMS)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE CDF OF STANDARD NORMAL DISTRIBUTION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARMS(2),MU,SIGMA
      
            MU          =     PARMS(1)
            SIGMA =     PARMS(2)
            Z     =     (X-MU)/SIGMA
      
      CALL SP_Z(Z,F,PHI,XLNPHI,RAT,HAZ)
      
      FP_N2_CDF   =     PHI
      
      RETURN
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_N2_ICDF(P,PARMS)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE INVERSE CDF OF STANDARD NORMAL DISTRIBUTION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARMS(2),MU,SIGMA

            MU          =     PARMS(1)
            SIGMA =     PARMS(2)
      
      FP_N2_ICDF  =     MU + SIGMA * DNORIN(P)
      
      RETURN
      END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G1_PDF(X,ALPHA)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE PDF OF 1-P GAMMA DISTRIBUTION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      IF(X .GT. 0.D0) THEN
            IF(ALPHA .LT. 171.D0) THEN
                  FP_G1_PDF   =     X**(ALPHA-1.D0) * 
     1	                               EXP(-X)/DGAMMA(ALPHA)
            ELSE
                  TEMP        =     (ALPHA-1.D0) * LOG(X) - X -  
     1	                               DLNGAM(ALPHA)
                  FP_G1_PDF   =     EXP(TEMP)
            ENDIF 
      ELSE
            FP_G1_PDF   =     0.D0
      ENDIF
      
      RETURN
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G2_PDF(X,PARMS)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE PDF OF 2-P GAMMA DISTRIBUTION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PARMS(2)
      
      ALPHA =     PARMS(1)
      BETA  =     PARMS(2)
      
      FP_G2_PDF   =     FP_G1_PDF(X/BETA,ALPHA)/ABS(BETA)
      
      RETURN
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G3_PDF(X,PARMS)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE PDF OF 3-P GAMMA DISTRIBUTION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PARMS(3)
      
      TAU         =     PARMS(1)
      ALPHA =     PARMS(2)
      BETA  =     PARMS(3)
      
      FP_G3_PDF   =     FP_G2_PDF(X-TAU,PARMS(2))
      
      RETURN
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~

      DOUBLE PRECISION FUNCTION FP_G1_CDF(X,ALPHA)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE CDF OF 1-P GAMMA DISTRIBUTION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      FP_G1_CDF   =     DGAMDF(X,ALPHA)
      
      RETURN
      END
      
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~

      DOUBLE PRECISION FUNCTION FP_G2_CDF(X,PARMS)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE CDF OF 2-P GAMMA DISTRIBUTION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PARMS(2)
      
      ALPHA =     PARMS(1)
      BETA  =     PARMS(2)
      
      IF(BETA .GT. 0.D0) THEN
            FP_G2_CDF   =     FP_G1_CDF(X/BETA,ALPHA)
      ELSE IF(BETA .LT. 0.D0) THEN
            FP_G2_CDF   =     1.D0 - FP_G1_CDF(X/BETA,ALPHA)
      ELSE
            WRITE(*,*) ' INVALID PARAMETERS;  BETA = 0'
            RETURN
      ENDIF
      
      RETURN
      END
      
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~

      DOUBLE PRECISION FUNCTION FP_G3_CDF(X,PARMS)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE CDF OF 2-P GAMMA DISTRIBUTION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PARMS(3)
      
      TAU         =     PARMS(1)
C     ALPHA =     PARMS(2)
C     BETA  =     PARMS(3)
      
            FP_G3_CDF   =     FP_G2_CDF(X-TAU,PARMS(2))

      RETURN
      END
      
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G1_ICDF(P,ALPHA)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE INVERSE OF GAMMA(ALPHA)
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      IF(P .GT. 0.999999999) THEN
            FP_G1_ICDF  =     9.D99
            RETURN
	ELSE IF(P .LT. 0.000000001) THEN
            FP_G1_ICDF  =     0.D0
            RETURN	      
      ENDIF
      
            FP_G1_ICDF  =     0.5*DCHIIN(P,2.D0*ALPHA)
      RETURN
      END
      
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
C
      DOUBLE PRECISION FUNCTION FP_G2_ICDF(P,PARMS)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE INVERSE OF GAMMA(P,ALPHA,BETA)
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PARMS(2)
      
      ALPHA =     PARMS(1)
      BETA  =     PARMS(2)
      
      IF(BETA .GT. 0.D0) THEN
            FP_G2_ICDF  =     BETA * FP_G1_ICDF(P,ALPHA)
      ELSE IF(BETA .LT. 0.D0) THEN
            FP_G2_ICDF  =     BETA * FP_G1_ICDF(1.D0-P,ALPHA)
      ELSE
            WRITE(*,*) ' INVALID PARAMETERS;  BETA = 0'
            RETURN
      ENDIF

      RETURN
      END
      
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G3_ICDF(P,PARMS)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE INVERSE OF GAMMA(P,TAU,ALPHA,BETA)
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PARMS(3)
      
      TAU         =     PARMS(1)
C     ALPHA =     PARMS(2)
C     BETA  =     PARMS(3)
      
      FP_G3_ICDF  =     TAU + FP_G2_ICDF(P,PARMS(2))
      
      RETURN
      END
      
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G1_MOM_TRA(ALPHA,T,K)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE THE EXPECTED VALUE OF THE K-TH MOMENT OF 
C     A 1-PARAMETER GAMMA VARIATE CENSORED ABOVE AT T
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C

      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)

       T1   =     MAX(0.D0,T)
       ANS  =     DGAMDF(T1,ALPHA+K)/MAX(1.D-50,DGAMDF(T1,ALPHA))

C     THIS SECTION COMPUTES RATIO GAMMA(ALPHA+K)/GAMMA(ALPHA)

      DO 10 J=0,K-1
            ANS   =     ANS*(ALPHA+J)
10    CONTINUE

      FP_G1_MOM_TRA     =     ANS

      RETURN
      END
      
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G2_MOM_TRA(PARMS,T,K)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE THE EXPECTED VALUE OF THE K-TH MOMENT OF 
C     A 2-PARAMETER GAMMA VARIATE CENSORED ABOVE AT T
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C

      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
      DIMENSION PARMS(2)
      
      ALPHA =     PARMS(1)
      BETA  =     PARMS(2)
      
      IF(BETA .EQ. 0.D0) THEN
            WRITE(*,*) ' INVALID PARAMETERS;  BETA = 0'
            RETURN
      ELSE IF(BETA .GT. 0.D0) THEN
            FP_G2_MOM_TRA = BETA**K * FP_G1_MOM_TRA(ALPHA,T/BETA,K)
      ELSE
            FP_G2_MOM_TRA = BETA**K * FP_G1_MOM_TRB(ALPHA,T/BETA,K)
      ENDIF
            
      RETURN
      END
      
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G3_MOM_TRA(PARMS,T,K)
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE KTH NON-CENTRAL MOMENT OF PEARSON TYPE III 
C     RANDOM VARIABLE CENSORED AT T WITH PARAMETERS PARMS
C
C
      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
      DIMENSION PARMS(3)

            TAU         =     PARMS(1)
            ALPHA =     PARMS(2)
            BETA  =     PARMS(3)

            SUM   =     FP_G2_MOM_TRA(PARMS(2),T-TAU,K)
            
      IF(TAU .NE. 0.D0) THEN
            DO 10 J=0,K-1
                  SUM   =     SUM + CHOOSE(K,J)*TAU**(K-J)*
     1                       FP_G2_MOM_TRA(PARMS(2),T-TAU,J)
10          CONTINUE
      ENDIF
      
            FP_G3_MOM_TRA     =     SUM

      RETURN
      END

      
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G1_MOM_TRB(ALPHA,T,K)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE THE EXPECTED VALUE OF THE K-TH MOMENT OF 
C     A 1-PARAMETER GAMMA VARIATE CENSORED BELOW AT T
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C

      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)

            T1    =     MAX(0.D0,T)
            ANS         =     (1.D0 - DGAMDF(T1,ALPHA+K))/
     1                       MAX(1.D-50, 1.D0 - DGAMDF(T1,ALPHA))

C     THIS SECTION COMPUTES RATIO GAMMA(ALPHA+K)/GAMMA(ALPHA)

      DO 10 J=0,K-1
            ANS   =     ANS*(ALPHA+J)
10    CONTINUE

      FP_G1_MOM_TRB     =     ANS

      RETURN
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G2_MOM_TRB(PARMS,T,K)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE THE EXPECTED VALUE OF THE K-TH MOMENT OF 
C     A 2-PARAMETER GAMMA VARIATE CENSORED ABOVE AT T
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C

      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
      DIMENSION PARMS(2)
      
      ALPHA =     PARMS(1)
      BETA  =     PARMS(2)
      
      IF(BETA .EQ. 0.D0) THEN
            WRITE(*,*) ' INVALID PARAMETERS;  BETA = 0'
            RETURN
      ELSE IF(BETA .GT. 0.D0) THEN
            FP_G2_MOM_TRB = BETA**K * FP_G1_MOM_TRB(ALPHA,T/BETA,K)
      ELSE
            FP_G2_MOM_TRB = BETA**K * FP_G1_MOM_TRA(ALPHA,T/BETA,K)
      ENDIF
            
      RETURN
      END
      
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G3_MOM_TRB(PARMS,T,K)
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE KTH NON-CENTRAL MOMENT OF PEARSON TYPE III 
C     RANDOM VARIABLE CENSORED AT T WITH PARAMETERS PARMS
C
C
      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
      DIMENSION PARMS(3)

            TAU         =     PARMS(1)
            ALPHA =     PARMS(2)
            BETA  =     PARMS(3)

            SUM   =     FP_G2_MOM_TRB(PARMS(2),T-TAU,K)
            
      IF(TAU .NE. 0.D0) THEN
            DO 10 J=0,K-1
                  SUM   =     SUM + CHOOSE(K,J)*TAU**(K-J)*
     1                       FP_G2_MOM_TRB(PARMS(2),T-TAU,J)
10          CONTINUE
      ENDIF
      
            FP_G3_MOM_TRB     =     SUM

      RETURN
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G1_MOM_TRC(ALPHA,TL,TU,K)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE THE EXPECTED VALUE OF THE K-TH MOMENT OF 
C     A 1-PARAMETER GAMMA VARIATE BETWEEN TL AND TU WITH PARAMETER ALPHA
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C     
C     MODIFIED...DECEMBER 23, 1996 (TAC)
C     MODIFIED...JULY 25, 1998 (TAC)
C

      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)

            TL1   =     MAX(MIN(0.D0,TU),TL)
	IF(TL1 .EQ. TU) THEN
	      ANS = TL1**K
	ELSE
		DOWN	=  DGAMDF(TU,ALPHA) - DGAMDF(TL1,ALPHA)
	IF(DOWN .GT. 0.D0) THEN
		UP    =  DGAMDF(TU,ALPHA+K) - DGAMDF(TL1,ALPHA+K)
		ANS	=  UP/DOWN
C     THIS SECTION COMPUTES RATIO GAMMA(ALPHA+K)/GAMMA(ALPHA)
            DO 10 J=0,K-1
                  ANS   =     ANS*(ALPHA+J)
10          CONTINUE
	ELSE
	      IF(TL1 .GT. ALPHA) THEN
		   ANS = TL1**K
		ELSE
		   ANS = TU**K
		ENDIF
      ENDIF
	ENDIF

      FP_G1_MOM_TRC     =     ANS

      RETURN
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G2_MOM_TRC(PARMS,TL,TU,K)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE THE EXPECTED VALUE OF THE K-TH MOMENT OF 
C     A 2-PARAMETER GAMMA VARIATE BETWEEN TL AND TU WITH PARAMETERS PARMS

C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 28, 1994
C

      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
      DIMENSION PARMS(2)
      
      ALPHA =     PARMS(1)
      BETA  =     PARMS(2)
      
      IF(BETA .EQ. 0.D0) THEN
            WRITE(*,*) ' INVALID PARAMETERS;  BETA = 0'
            RETURN
      ELSE IF(BETA .GT. 0.D0) THEN
            FP_G2_MOM_TRC     =     BETA**K *  
     1	        FP_G1_MOM_TRC(ALPHA,TL/BETA,TU/BETA,K)
      ELSE
            FP_G2_MOM_TRC     =     BETA**K *  
     1	        FP_G1_MOM_TRC(ALPHA,TU/BETA,TL/BETA,K)
      ENDIF
            
      RETURN
      END
      
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G3_MOM_TRC(PARMS,TL,TU,K)
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE KTH NON-CENTRAL MOMENT OF PEARSON TYPE III 
C     RANDOM VARIABLE BETWEEN TL AND TU WITH PARAMETERS PARMS
C
C
      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
      DIMENSION PARMS(3)

            TAU         =     PARMS(1)
            ALPHA =     PARMS(2)
            BETA  =     PARMS(3)

            SUM   =     FP_G2_MOM_TRC(PARMS(2),TL-TAU,TU-TAU,K)
            
      IF(TAU .NE. 0.D0) THEN
            DO 10 J=0,K-1
                  SUM   =     SUM + CHOOSE(K,J)*TAU**(K-J)*
     1                       FP_G2_MOM_TRC(PARMS(2),TL-TAU,TU-TAU,J)
10          CONTINUE
      ENDIF
      
            FP_G3_MOM_TRC     =     SUM

      RETURN
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_G3_MOM_TRCO(PARMS,X_OFF,TL,TU,K)
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE KTH MOMENT, CENTRALIZED AROUND X_OFF, OF PEARSON TYPE III 
C     RANDOM VARIABLE BETWEEN TL AND TU WITH PARAMETERS PARMS
C
C
      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
      DIMENSION PARMS(3)

            TAU         =     PARMS(1)
            ALPHA =     PARMS(2)
            BETA  =     PARMS(3)

            SUM   =     FP_G2_MOM_TRC(PARMS(2),TL-TAU,TU-TAU,K)
            
      IF(TAU - X_OFF .NE. 0.D0) THEN
            DO 10 J=0,K-1
                  SUM   =     SUM + CHOOSE(K,J)*(TAU-X_OFF)**(K-J)*
     1                       FP_G2_MOM_TRC(PARMS(2),TL-TAU,TU-TAU,J)
10          CONTINUE
      ENDIF
      
            FP_G3_MOM_TRCO    =     SUM

      RETURN
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
       SUBROUTINE SP_Z(XSI,F,PHI,LOGPHI,A,G)
C======================================================================
C
C      SUBROUTINE TO COMPUTE GAUSSIAN FUNCTIONS AND THEIR
C      DERIVATIVES, WHERE XSI IS A STANDARD NORMAL VARIATE
C
C           PHI       =  STANDARD NORMAL CDF
C           LOGPHIT   =  LOG(PHIT)
C           AT        =  F/PHIT
C           GT        =  F/(1.D0-PHIT)
C
C           THE ROUTINE WORKS ACCURATELY OVER A VERY LONG RANGE, AND PROVIDES
C           A 2ND ORDER CONTINUITY CORRECTION WHERE ESTIMATION METHODS CHANGE.
C
C
C        AUTHOR......TIM COHN
C        DATE........APRIL 1, 1987
C          ERROR CORRECTED OCTOBER 4, 1992 (TAC)
C                 IMPROVED VERSION....DECEMBER 20, 1994
C
C======================================================================
       IMPLICIT DOUBLE PRECISION (A-Z)

       DATA LIMIT/6.5D0/,LIMITI/6.D0/,CONST/0.3989422804014327/
         
           Z(B)      =  1.D0/( SQRT(B)*(1.D0-1.D0*B*(1.D0-3.D0*B*
     1                  (1.D0-5.D0*B*(1.D0-7.D0*B*(1.D0-9.D0*B*
     2                  (1.D0-11.D0*B*(1.D0-13.D0*B*(1.D0-15.D0*B*
     3                  (1.D0-17.D0*B*(1.D0-19.D0*B*(1.D0-21.D0*B*
     4                  (1.D0-23.D0*B*(1.D0-12.5D0*B))))))))))))) )
              
         F         =  CONST*EXP(-0.5*XSI**2)
       IF(XSI .LT. -LIMITI) THEN
         PHI       =  F/Z(1.D0/XSI**2) 
         A         =  Z(1.D0/XSI**2)
         LOGPHI    =  LOG(CONST/A) - 0.5*XSI**2
         G         =  F/(1.D0-PHI) 
       ELSE IF (XSI .GT. LIMITI) THEN
         PHI       =  1.D0 - F/Z(1.D0/XSI**2) 
         LOGPHI    =  LOG(PHI) 
         A         =  F/PHI 
         G         =  Z(1.D0/XSI**2)
       ENDIF
         
       IF(ABS(XSI) .LT. LIMIT) THEN
           PHIT       =  DNORDF(XSI)
           LOGPHIT    =  LOG(PHIT)
           AT         =  F/PHIT
           GT         =  F/(1.D0-PHIT)
             IF(ABS(XSI) .GT. LIMITI) THEN
                  IF(ABS(XSI) .GT. LIMIT) THEN
                        ARG   =     1.D0
                  ELSE
                     ARGT      =  (ABS(XSI)-(LIMIT+LIMITI)/2.D0) 
     1	                               /(LIMIT-LIMITI)
                     ARG       =  (1.D0+SIN(3.14159*ARGT))/2.D0
                  ENDIF
             ELSE
               ARG       =  0.D0
             ENDIF
               PHI       =  ARG*PHI + (1.D0-ARG)*PHIT
               LOGPHI    =  ARG*LOGPHI + (1.D0-ARG)*LOGPHIT
               A         =  ARG*A + (1.D0-ARG)*AT
               G         =  ARG*G + (1.D0-ARG)*GT
       ENDIF
       RETURN
       END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION CHOOSE(N,K)
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
      INTEGER N,K
      
      IF(N .LT. MAX(0,K)) THEN
            WRITE(*,*) ' ERROR IN CHOOSE: (N,K) ',N,K
            STOP
      ENDIF

      IF(N .LT. 170.D0) THEN
            CHOOSE      =     DFAC(N)/(DFAC(N-K)*DFAC(K))
      ELSE
            ARG         =     DLNGAM(N+1.D0)-DLNGAM(N-K+1.D0)- 
     1	                               DLNGAM(K+1.D0)
            IF(ARG .LT. 500.D0) THEN
                  CHOOSE      =     EXP(ARG)
            ELSE
                  CHOOSE      =     -99.D0
            ENDIF
      ENDIF
      
      RETURN
      END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      SUBROUTINE SP_G1_CDF_HESS(X,ALPHA_IN,GRAD,HESS)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE HESSIAN AND GRADIENT OF 1-P GAMMA D'N
C
C     AUTHOR.....TIM COHN
C     DATE.......JANUARY 30, 1995
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     ORDER OF PARAMETERS IS (X, ALPHA)
C
C           GRAD(1)     =     D_F/D_X
C           GRAD(2)     =     D_F/D_ALPHA
C
C           HESS(1,1)   =     D2_F/D_X2
C           HESS(1,2)   =     HESS(2,1)   =     D2_F/D_X D_ALPHA 
C           HESS(2,2)   =     D2_F/D_ALPHA2
C
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL FCNGAM
      
      DIMENSION GRAD(2),HESS(2,2)
      
      COMMON /TACGAM001/ALPHA,M
      
      DATA ERRABS/0.D0/,ERREL/1.D-5/
      
      ALPHA =     ALPHA_IN
      
            P     =     FP_G1_CDF(X,ALPHA)
            
            FPDF        =     FP_G1_PDF(X,ALPHA)
            GRAD(1)           =     FPDF
            HESS(1,1)   =     FPDF * ( (ALPHA-1.D0)/X - 1.D0)
            HESS(1,2)   =     FPDF * ( LOG(X) - DPSI(ALPHA) )
            HESS(2,1)   =     HESS(1,2)
C
      M     =     1
            CALL DQDAGS(FCNGAM,0.D0,P,   ERRABS,ERREL,FP,ERREST)
            CALL DQDAGS(FCNGAM,0.D0,1.D0,ERRABS,ERREL,GP,ERREST)
      M     =     2
            CALL DQDAGS(FCNGAM,0.D0,P,   ERRABS,ERREL,FPP,ERREST)
            CALL DQDAGS(FCNGAM,0.D0,1.D0,ERRABS,ERREL,GPP,ERREST)
C
C
C     N.B.  P IS F
C           BECAUSE G   =     1.D0
C                 D1    =     FP*G - GP*P
C                 D2    =     ( ( (FPP * G + GP * FP) ) * G**2 - 2 * G * GP * (FP*G - GP*P) )/G**4
C
      GRAD(2)     =     FP - GP*P

      HESS(2,2)   =     ( ( (FPP +     GP * FP) - (GPP * P + GP * FP) )       
     1                  - 2 *     GP * (FP   - GP*P) )
      
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION FCNGAM(P)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      COMMON /TACGAM001/ALPHA,M
      
      X           =     FP_G1_ICDF(P,ALPHA)
      FCNGAM      =     LOG(X)**M
      
      RETURN
      END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION SIGN1(ARG)
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C
      DOUBLE PRECISION ARG

      IF(ARG .LT. 0.D0) THEN
            SIGN1 =     -1.D0
      ELSE
            SIGN1 =      1.D0
      ENDIF
      RETURN
      END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      SUBROUTINE SP_G1_CDF_LN_HESS(X,ALPHA,GRAD,HESS)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE HESSIAN AND GRADIENT OF LOGARITHM OF 1-P GAMMA D'N
C
C     AUTHOR.....TIM COHN
C     DATE.......MARCH 1, 1995
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     ORDER OF PARAMETERS IS (X, ALPHA)
C
C           GRAD(1)     =     D_LOG(F)/D_X
C           GRAD(2)     =     D_LOG(F)/D_ALPHA
C
C           HESS(1,1)   =     D2_LOG(F)/D_X2
C           HESS(1,2)   =     HESS(2,1)   =     D2_LOG(F)/D_X D_ALPHA 
C           HESS(2,2)   =     D2_LOG(F)/D_ALPHA2
C
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      DIMENSION GRAD(2),HESS(2,2),G1(2),H2(2,2)
      
      F     =     FP_G1_CDF(X,ALPHA)
      
      IF(F .GT. 0.D0) THEN
                  CALL SP_G1_CDF_HESS(X,ALPHA,G1,H2)
            DO 10 I=1,2
                  GRAD(I)           =     G1(I)/F
            DO 10 J=1,2
                  HESS(I,J)   =     ( H2(I,J)*F - G1(I)*G1(J) )/F**2
10          CONTINUE
      ELSE
            DO 20 I=1,2
                  GRAD(I)           =     0.D0
            DO 20 J=1,2
                  HESS(I,J)   =     0.D0
20          CONTINUE
      ENDIF
      
      RETURN
      END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      SUBROUTINE SP_G3_CDF_LN_HESS(X,PARMS,GRAD,HESS)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE HESSIAN AND GRADIENT OF LOGARITHM OF 3-P GAMMA D'N
C
C     AUTHOR.....TIM COHN
C     DATE.......MARCH 1, 1995
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     ORDER OF PARAMETERS IS (TAU, ALPHA, BETA)
C     HESS IS A SYMMETRIC MATRIX
C
C           GRAD(1)     =     D_LOG(F)/D_TAU
C           GRAD(2)     =     D_LOG(F)/D_ALPHA
C           GRAD(3)     =     D_LOG(F)/D_BETA
C
C           HESS(1,1)   =     D2_LOG(F)/D_TAU^2
C           HESS(1,2)   =     D2_LOG(F)/D_TAU D_ALPHA 
C           HESS(1,3)   =     D2_LOG(F)/D_TAU D_BETA 
C           HESS(2,2)   =     D2_LOG(F)/D_ALPHA^2
C           HESS(2,3)   =     D2_LOG(F)/D_ALPHA D_BETA 
C           HESS(3,3)   =     D2_LOG(F)/D_BETA^2
C
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      
      DIMENSION PARMS(3),GRAD(3),HESS(3,3),G1(2),H2(2,2)
      
            TAU         =     PARMS(1)
            ALPHA =     PARMS(2)
            BETA  =     PARMS(3)
            
            Y           =     (X-TAU)/BETA
                        
      CALL SP_G1_CDF_LN_HESS(Y,ALPHA,G1,H2)
      
                  GRAD(1)           =     G1(1) * (-1.D0/BETA)
                  GRAD(2)           =     G1(2)
                  GRAD(3)           =     G1(1) * (-Y/BETA)
                  
                  HESS(1,1)   =     H2(1,1) * (1.D0/BETA**2)
                  HESS(1,2)   =     H2(1,2) * (-1.D0/BETA)
                  HESS(1,3)   =     H2(1,1) * (Y/BETA**2) + G1(1) *  
     1	                               (1.D0/BETA**2)
                  HESS(2,2)   =     H2(2,2)
                  HESS(2,3)   =     H2(1,2) * (-Y/BETA)
                  HESS(3,3)   =     H2(1,1) * (-Y/BETA)**2 + G1(1) *  
     1	                               (2*Y/BETA**2)
                  
      DO 10 I=1,3
            DO 10 J=I+1,3
                  HESS(J,I)   =     HESS(I,J)
10    CONTINUE

      RETURN
      END
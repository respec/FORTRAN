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
C           NON-CENTRAL STUDENTS T DISTRIBUTION
C                 FP_NCT_CDF(X,NU,DELTA)
C                 FP_NCT_ICDF(P,NU,DELTA)
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
C      SAVE
      
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
C      SAVE
      
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
C      SAVE
      
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
C      SAVE
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
C      SAVE
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
C      SAVE
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
C      SAVE
      
      IF(X .GT. 0.D0 .AND. ALPHA .GT. 0.D0) THEN
            IF((ALPHA+X) .LT. 100.D0) THEN
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
C      SAVE
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
C      SAVE
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
C      SAVE
      
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
C      SAVE
      DIMENSION PARMS(2)
      
      ALPHA =     PARMS(1)
      BETA  =     PARMS(2)
      
      IF(BETA .GT. 0.D0) THEN
            FP_G2_CDF   =     FP_G1_CDF(X/BETA,ALPHA)
      ELSE IF(BETA .LT. 0.D0) THEN
            FP_G2_CDF   =     1.D0 - FP_G1_CDF(X/BETA,ALPHA)
      ELSE
            IF(X .GT. 0.D0) THEN
              FP_G2_CDF   =     1.D0
	    ELSE
              FP_G2_CDF   =     0.D0
	    ENDIF
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
C      SAVE
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
C      SAVE
      
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
C      SAVE
      DIMENSION PARMS(2)
      
      ALPHA =     PARMS(1)
      BETA  =     PARMS(2)
      
      IF(BETA .GT. 0.D0) THEN
            FP_G2_ICDF  =     BETA * FP_G1_ICDF(P,ALPHA)
      ELSE IF(BETA .LT. 0.D0) THEN
            FP_G2_ICDF  =     BETA * FP_G1_ICDF(1.D0-P,ALPHA)
      ELSE
            WRITE(6,*) ' INVALID PARAMETERS;  BETA = 0'
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
C      SAVE
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
C      SAVE

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
C      SAVE
      DIMENSION PARMS(2)
      
      ALPHA =     PARMS(1)
      BETA  =     PARMS(2)
      
      IF(BETA .EQ. 0.D0) THEN
            WRITE(6,*) ' INVALID PARAMETERS;  BETA = 0'
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
C      SAVE
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
C      SAVE

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
C      SAVE
      DIMENSION PARMS(2)
      
      ALPHA =     PARMS(1)
      BETA  =     PARMS(2)
      
      IF(BETA .EQ. 0.D0) THEN
            WRITE(6,*) ' INVALID PARAMETERS;  BETA = 0'
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
C      SAVE
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
C      SAVE

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
C      SAVE
      DIMENSION PARMS(2)
      
      ALPHA =     PARMS(1)
      BETA  =     PARMS(2)
      
      IF(BETA .EQ. 0.D0) THEN
            WRITE(6,*) ' INVALID PARAMETERS;  BETA = 0'
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
C      SAVE
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
C      SAVE
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
C      SAVE

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
C                 \ K /
C
C     N.B.  N AND K MUST BE INTEGERS;  CHOOSE IS DOUBLE PRECISION
C
C     AUTHOR.....TIM COHN
C     DATE.......DECEMBER 21, 1994
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C      SAVE
      INTEGER N,K
      
      IF(N .LT. MAX(0,K)) THEN
            WRITE(6,*) ' ERROR IN CHOOSE: (N,K) ',N,K
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
C      SAVE
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
C      SAVE
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
C      SAVE

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
C      SAVE
      
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
C      SAVE
      
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
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      SUBROUTINE DPDM(PARMS,JACT)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE JACOBIAN DP/DM TRANSFORMATION CONVERTING
C        PEARSON TYPE 3 NON-CENTRAL MOMENTS (M1,M2,M3)
C        INTO PARAMETERS (TAU,ALPHA,BETA) 
C
C     AUTHOR.....TIM COHN
C     DATE.......APRIL 10, 1999
C
C    N.B.  JAC IS COMPUTED;  JACT = TRANSPOSE(JAC) IS RETURNED
C
C            JAC(1,1) =  DT/DM1
C            JAC(1,2) =  DA/DM1
C            JAC(1,3) =  DB/DM1
C
C            JAC(2,1) =  DT/DM2
C            JAC(2,2) =  DA/DM2
C            JAC(2,3) =  DB/DM2
C
C            JAC(3,1) =  DT/DM3
C            JAC(3,2) =  DA/DM3
C            JAC(3,3) =  DB/DM3
C
C    N.B.: THIS CODE APPEARS TO HANDLE ALL CASES CORRECTLY (TAC 4/10/99)
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C      SAVE
      
      DOUBLE PRECISION PARMS(3),M(3),M1,M2,M3,S,A,B,D,T
      DOUBLE PRECISION JAC(3,3),JACT(3,3)
            
      CALL P2MN(PARMS,M)
        M1  =  M(1)
        M2  =  M(2)
        M3  =  M(3)
      
C   V IS THE VARIANCE

      V        =  M2 - M1**2
        DVDM1  =  -2.D0*M1
        DVDM2  =  1.D0
	DVDM3  =  0.D0

C   S  IS THE SKEW       
      S        =  M3 - 3.D0*M2*M1 + 2.D0*M1**3
        DSDM1  =  -3.D0*M2 + 6.D0*M1**2
	DSDM2  =  -3.D0*M1
	DSDM3  =  1.D0

C   A  IS THE PARAMETER

      A        =  4.D0*(V**3)/S**2
        DADM1  =  4.D0*(3.D0*V**2*DVDM1 * S**2 - 2*S*DSDM1*V**3)/S**4
        DADM2  =  4.D0*(3.D0*V**2*DVDM2 * S**2 - 2*S*DSDM2*V**3)/S**4
        DADM3  =  4.D0*(3.D0*V**2*DVDM3 * S**2 - 2*S*DSDM3*V**3)/S**4
	
	
C   D IS B**2

      D        =  V/A
        DDDM1  =  (DVDM1*A-DADM1*V)/A**2
        DDDM2  =  (DVDM2*A-DADM2*V)/A**2
        DDDM3  =  (DVDM3*A-DADM3*V)/A**2
        
C   B IS SIGN(S)*SQRT(D)

      B        =  SIGN(1.D0,S)*SQRT(D)
        DBDM1  =  0.5*DDDM1/B
        DBDM2  =  0.5*DDDM2/B
        DBDM3  =  0.5*DDDM3/B

C   T

      T        =  M1 - A*B
        DTDM1  =  1.D0 - (DADM1*B + A*DBDM1)
        DTDM2  =       - (DADM2*B + A*DBDM2)
        DTDM3  =       - (DADM3*B + A*DBDM3)

      JAC(1,1) =  DTDM1
      JAC(1,2) =  DADM1
      JAC(1,3) =  DBDM1

      JAC(2,1) =  DTDM2
      JAC(2,2) =  DADM2
      JAC(2,3) =  DBDM2

      JAC(3,1) =  DTDM3
      JAC(3,2) =  DADM3
      JAC(3,3) =  DBDM3
      
      DO 10 I=1,3
        DO 10 J=1,3
	  JACT(I,J) = JAC(J,I)
10    CONTINUE

      RETURN
      END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C	
        SUBROUTINE P2M(P,M)
C        SAVE
	IMPLICIT NONE
	DOUBLE PRECISION P(3),M(3),T,A,B
	T=P(1)
	A=P(2)
	B=P(3)
	M(1) =  A*B + T
	M(2) =  A*B**2
        M(3) =  2.D0/SQRT(A)
	IF(B .LT. 0.D0) M(3) = -M(3)
	RETURN
	END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
        SUBROUTINE M2P(M,P)
	IMPLICIT NONE
C        SAVE
	DOUBLE PRECISION P(3),A,B,T,M(3),M1,M2,M3
	M1=M(1)
	M2=M(2)
	M3=M(3)
        A = 4.D0/M3**2
	B = SQRT(M2/A)
	IF(M3 .LT. 0.D0) B=-B
	T = M1 - A*B
	P(1)  =  T
	P(2)  =  A
	P(3)  =  B
	RETURN
	END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C	
        SUBROUTINE P2MN(P,M)
C        SAVE
	IMPLICIT NONE
	DOUBLE PRECISION P(3),M(3),T,A,B
	T=P(1)
	A=P(2)
	B=P(3)
	M(1) =  A*B + T
	M(2) =  A*(1 + A)*B**2 + 2*A*B*T + T**2
        M(3) =  A*(1 + A)*(2 + A)*B**3 + 3*A*(1 + A)*B**2*T + 
     1             3*A*B*T**2 + T**3
	RETURN
	END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
        SUBROUTINE MN2P(M,P)
	IMPLICIT NONE
C      SAVE
	DOUBLE PRECISION P(3),M(3),M1,M2,M3
	M1=M(1)
	M2=M(2)
	M3=M(3)
        P(1) = (M1**2*M2 - 2*M2**2 + M1*M3)/(2*M1**3 - 3*M1*M2 + M3)
	P(2) = -4*(M1**2 - M2)**3/(2*M1**3 - 3*M1*M2 + M3)**2
	P(3) = (2*M1**3 - 3*M1*M2 + M3)/(-2*M1**2 + 2*M2)
	RETURN
	END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C	
        SUBROUTINE M2MN(M,MN)
C      SAVE
	IMPLICIT NONE
	DOUBLE PRECISION M(3),MN(3)
	MN(1) =  M(1)
	MN(2) =  M(2)+M(1)**2
        MN(3) =  M(3)*M(2)**1.5 + 3.D0*MN(2)*M(1) - 2.D0 * M(1)**3
	RETURN
	END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C	
        SUBROUTINE MN2M(MN,M)
C      SAVE
	IMPLICIT NONE
	DOUBLE PRECISION M(3),MN(3)
	M(1) =  MN(1)
	M(2) =  MN(2)-MN(1)**2
        M(3) =  (MN(3) - 3.D0*MN(2)*MN(1) + 2.D0 * MN(1)**3)
     1              /M(2)**1.5
	RETURN
	END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION D_G1_INV(PQ,ALPHA)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE DERIVATIVE WRT ALPHA OF 1-PARAMETER GAMMA QUANTILE
C         FUNCTION
C
C     AUTHOR.....TIM COHN
C     DATE.......APRIL 10, 1999
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C           PQ        =     QUANTILE TO BE ESTIMATED
C           ALPHA     =     PARAMETER OF G1 DISTRIBUTION
C
C     N.B.  DDGAM IS CONTAINED IN VARMOM SUBROUTINE FILE
C
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C      SAVE
      DOUBLE PRECISION PQ,Q1P,ALPHA
      
	 Q1P      =   FP_G1_ICDF(PQ,ALPHA)
	 D_G1_INV =  -DDGAM(ALPHA,Q1P)/FP_G1_PDF(Q1P,ALPHA)
	 
      RETURN
      END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
	 SUBROUTINE EXPMOMDERIV(PARMS,XMIN,XMAX,JAC)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE DERIVATIVE OF EXPECTED VALUE OF 
C         MOMENTS OF GAMMA VARIATE TRUNCATED BETWEEN XMIN AND XMAX
C
C     AUTHOR.....TIM COHN
C     DATE.......APRIL 10, 1999
C
C     REQUIRES FUNCTIONS FOR LOG-GAMMA AND PSI
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C      SAVE
      DOUBLE PRECISION 
     1  PARMS(3),XMIN,XMAX,JAC(3,3),
     2  JAC1(3,3),JAC2(3,3),ALPHA,BETA,TAU,M(3)

          TAU   = PARMS(1)
	  ALPHA = PARMS(2)
	  BETA  = PARMS(3)
        CALL DEXPECT(TAU,ALPHA,BETA,XMIN,XMAX,M,JAC1)
	 
        CALL DPDM(PARMS,JAC2)
	
	CALL DMRRRR(3,3,JAC1,3,3,3,JAC2,3,3,3,JAC,3)
	
	RETURN
	END
	
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION DDGAM(ALPHA,X)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE DERIVATIVE OF INCOMPLETE GAMMA FUNCTION
C
C     AUTHOR.....TIM COHN
C     DATE.......APRIL 9, 1999
C
C     REQUIRES FUNCTIONS FOR LOG-GAMMA AND PSI
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C      SAVE
      DOUBLE PRECISION I,TOL,LOGX,SUM
      
       DATA TOL/1.D-11/
      
       IF(ALPHA .LE. 0.D0 .OR. X .LE. 0.D0) THEN
               RESULT = 0.D0
	       GOTO 100
       END IF
              
       IF(ABS(X-ALPHA)/SQRT(ALPHA+1.D0) .GT. 7.D0) THEN
               RESULT = 0.D0
	       GOTO 100
       END IF
      
          A        = ALPHA
	  LOGX     =  LOG(X)
C
      IF(X .LT. 5.D0) THEN
          T        =  X**A
	  R        =  (A*LOGX-1.D0)/A**2
	  SUM      =  T*R
	DO 10 I2=1,1000
	   I       =  I2
	  AI       =  A+I
	  T        =  -T*X/I
	  R        =  (AI*LOGX-1.D0)/AI**2
	  DEL      =  R*T
	  SUM      =  SUM + DEL
	   IF(I .GT. 1 .AND. ABS(DEL) .LT. (1.D0+ABS(SUM))*TOL) THEN
	     RESULT =  SUM/EXP(DGAMLN(A,IERR)) - DPSI(A)*FP_G1_CDF(X,A)
	     GOTO 100
	   ENDIF
10      CONTINUE
          GOTO 99
C
      ELSE IF(X .GT. A+30.D0) THEN
          T        =  EXP(-X + (A-1.D0)*LOGX - DGAMLN(A,IERR) )
	  R        =  LOGX - DPSI(A)
	  SUM      =  R*T
	DO 20 I2=1,INT(A)-1
	  I       =  I2
	  AMI       =  A-I
	  T        =  T*(AMI)/X
	  R        =  LOGX - DPSI(AMI)
	  DEL      =  R*T
	  SUM      =  SUM + DEL
	   IF(I .GT. 1 .AND. ABS(DEL) .LT. (1.D0+ABS(SUM))*TOL) THEN
	     RESULT = -SUM
	     GOTO 100
	   ENDIF
20      CONTINUE
          GOTO 99
C
      ELSE
          T        =  EXP(-X + A*LOGX - DGAMLN(A+1.D0,IERR) )
	  R        =  LOGX - DPSI(A+1.D0)
	  SUM      =  R*T
	DO 30 I2=1,10000
	  I       =  I2
	  T        =  T*X/(A+I)
	  R        =  LOGX - DPSI(A+I+1.D0)
	  DEL      =  R*T
	  SUM      =  SUM + DEL
	   IF(I .GT. 1 .AND. ABS(DEL) .LT. (1.D0+ABS(SUM))*TOL) THEN
	     RESULT = SUM
	     GOTO 100
	   ENDIF
30      CONTINUE
          GOTO 99
      ENDIF
C
100   CONTINUE
        DDGAM      =  RESULT
      RETURN
99    CONTINUE
              WRITE(6,*) 'SOMETHING BAD (DDGAM).....'
              WRITE(6,*) 'CALL TIM (703/904-7374).....'
              WRITE(6,*) 'ALPHA: ',ALPHA
              WRITE(6,*) 'X:     ',X
	      READ(6,*)
	      STOP
      END

C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      SUBROUTINE DEXPECT(TAU,ALPHA,BETA,TL,TU,M3,DM3)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE DERIVATIVE OF INCOMPLETE GAMMA FUNCTION
C       TRUNCATED BELOW AT TL AND ABOVE AT TU, WITH PARMS TAU, ALPHA, BETA
C
C     AUTHOR.....TIM COHN
C     DATE.......APRIL 9, 1999
C
C     REQUIRES FUNCTIONS FOR LOG-GAMMA AND PSI
C
C     NOTE RETURN VARIABLES:
C         M3[J]      CONTAINS E[X^J]
C         DM3[J,K]   CONTAINS D[ E[X^J] ]/P[K]
C
C          DM3[1,1]  =   D[ E[X] ]/D_TAU
C          DM3[1,2]  =   D[ E[X] ]/D_ALPHA
C          DM3[1,3]  =   D[ E[X] ]/D_BETA
C
C          DM3[2,1]  =   D[ E[X^2] ]/D_TAU
C          DM3[2,2]  =   D[ E[X^2] ]/D_ALPHA
C          DM3[2,3]  =   D[ E[X^2] ]/D_BETA
C
C          DM3[3,1]  =   D[ E[X^3] ]/D_TAU
C          DM3[3,2]  =   D[ E[X^3] ]/D_ALPHA
C          DM3[3,3]  =   D[ E[X^3] ]/D_BETA
C
C       WHERE P[1] = TAU, P[2] = ALPHA, P[3] = BETA
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C      SAVE
      DOUBLE PRECISION 
     1    ALPHA,BETA,TAU,TL,TU,
     2    DG1(0:3),G1(0:3),
     3    ADJ(0:3),DADJ(0:3),B(0:3),DB(0:3),
     4    M1(3),DM1(0:3,3),M2(3),DM2(3,3),M3(3),DM3(3,3),
     5    LSTDL,LSTDU,FU(0:3),FL(0:3)

C
C    RETURN IF THE RESULTS DO NOT DEPEND ON THE PARAMETERS
C
	IF(TU .LE. TL) GOTO 99
	
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C    STANDARDIZE THE THRESHOLDS;  COMPUTE DERIVATIVES
C
	IF(BETA .GT. 0.D0) THEN
          STDL  =  MAX(0.D0,(TL-TAU)/BETA)
	  STDU  =  (TU-TAU)/BETA
	ELSE IF(BETA .LT. 0.D0) THEN
          STDL  =  MAX(0.D0,(TU-TAU)/BETA)
	  STDU  =  (TL-TAU)/BETA
        ELSE IF(BETA .EQ. 0.D0) THEN
	  GOTO 99
        ENDIF
	  LSTDL   =  LOG(MAX(1.D-99,STDL))
	  LSTDU   =  LOG(MAX(1.D-99,STDU))
	  
	  DSTDLT  =  -1.D0/BETA
	  DSTDUT  =  -1.D0/BETA
	  DSTDLB  =  -STDL/BETA
	  DSTDUB  =  -STDU/BETA

C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C    1.  FIRST FOR THE ONE-PARAMETER GAMMA (ASSUME TAU=0; BETA=1)
C         -- NOTE THE FOLLOWING:
C              E[X^P] = GAMMA[ALPHA+P,TL,TU] / GAMMA[ALPHA,TL,TU]
C                     = ADJ[P]*B[P]
C              ADJ[P] = GAMMA[ALPHA+P]/GAMMA[ALPHA]
C              B[P]   = GCDF[ALPHA+P,TL,TU] / GCDF[ALPHA,TL,TU]
C              GCDF[ALPHA,TL,TU] = GAMMA[ALPHA,TL,TU]/GAMMA[ALPHA,0,INF]
C

C
C      1.A   FIRST COMPUTE THE ADJUSTMENT FACTOR
C
          ADJ(0)    =  1.D0
	  DADJ(0)   =  0.D0
	DO 40 J=1,3
	  ADJ(J)    =  (ALPHA+J-1.D0) * ADJ(J-1)
	  DADJ(J)   =  ADJ(J-1) + (ALPHA+J-1.D0) * DADJ(J-1)
40      CONTINUE

C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C      1.B   SECOND COMPUTE THE INCOMPLETE G1 CDF AND ITS DERIVATIVES
C              -- NB:  B(J) IS THE NORMALIZED CDF
C                      DB(J) IS COMPUTED FROM RATIO
C                      G1 CONTAINS TAU, BETA THROUGH STDL
C
      DO 20 J=0,3
        GU      =  FP_G1_CDF(STDU,ALPHA+J)
	GL      =  FP_G1_CDF(STDL,ALPHA+J)
	G1(J)   =  GU-GL
        DG1(J)  =  DDGAM(ALPHA+J,STDU) - DDGAM(ALPHA+J,STDL)
	B(J)    =  G1(J)/G1(0)
	DB(J)   =  (DG1(J)*G1(0) - DG1(0)*G1(J))/G1(0)**2
	FU(J)   =  FP_G1_PDF(STDU,ALPHA+J)
	FL(J)   =  FP_G1_PDF(STDL,ALPHA+J)
        DM1(J,1) = (
     1     (FU(J)*DSTDUT - FL(J)*DSTDLT) * G1(0) -
     2     (FU(0)*DSTDUT - FL(0)*DSTDLT) * G1(J) )/G1(0)**2
        DM1(J,3) = (
     1     (FU(J)*DSTDUB - FL(J)*DSTDLB) * G1(0) -
     2     (FU(0)*DSTDUB - FL(0)*DSTDLB) * G1(J) )/G1(0)**2
20    CONTINUE
    
C
C      1.C   NOW COMPUTE THE DERIVATIVE OF THE PRODUCT
C
      DO 50 J=1,3
        M1(J)     =  ADJ(J)*B(J)
        DM1(J,2)  =  ADJ(J)*DB(J) + DADJ(J)*B(J)
	DM1(J,1)  =  ADJ(J)*DM1(J,1)
	DM1(J,3)  =  ADJ(J)*DM1(J,3)
50    CONTINUE

C
C
C    2.  NOW GENERALIZE TO THE 2-PARAMETER GAMMA (ASSUME TAU=0)
C        --NOTE THAT ORDER OF PARAMETERS IS TAU,ALPHA,BETA
C          (TAC -- ANOTHER GOOFY CONVENTION); THEREFORE
C              DM2(J,1) REFERS TO TAU
C              DM2(J,2) REFERS TO ALPHA
C              DM2(J,3) REFERS TO BETA
C
      DO 60 J=1,3
        M2(J)     = BETA**J * M1(J)
	DM2(J,1)  = BETA**J * DM1(J,1)
	DM2(J,2)  = BETA**J * DM1(J,2)
	DM2(J,3)  = J * BETA**(J-1) * M1(J) + BETA**J * DM1(J,3)
60    CONTINUE
C
C
C    3.  NOW GENERALIZE TO THE 3-PARAMETER GAMMA
C
C
      DO 70 J=1,3
          M3(J)     = M2(J) + TAU**J
	  DM3(J,1)  = J * POWER(TAU,(J-1)) + DM2(J,1)
	  DM3(J,2)  = DM2(J,2)
	  DM3(J,3)  = DM2(J,3)
	DO 70 K=1,J-1
	  CH        =  CHOOSE(J,K)
	  M3(J)     =  M3(J) + CH*TAU**(J-K) * M2(K)
	  DM3(J,1)  =  DM3(J,1) + CH*(
     1                   (J-K)*POWER(TAU,(J-K-1)) * M2(K) +
     2                   TAU**(J-K) * DM2(K,1) )
	  DM3(J,2)  =  DM3(J,2) + CH*TAU**(J-K) * DM2(K,2)
	  DM3(J,3)  =  DM3(J,3) + CH*TAU**(J-K) * DM2(K,3)
70    CONTINUE

      RETURN
C 
C  NON-FUNCTIONAL EXIT
C
99    CONTINUE
	  CALL DSET(9,0.D0,DM3,1)
	  CALL DSET(3,0.D0,M3,1)
	  RETURN
      END
C
      DOUBLE PRECISION FUNCTION POWER(X,I)
C      SAVE
      DOUBLE PRECISION X
      INTEGER I
      IF(X .EQ. 0.D0 .AND. I .EQ. 0) THEN
         POWER = 1.D0
      ELSE
         POWER = X**I
      ENDIF
      RETURN
      END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_TNC_CDF(TP,NU,DELTA)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE NON-CENTRAL T CDF
C
C     AUTHOR.....TIM COHN
C     DATE.......APRIL 9, 1999
C
C     REQUIRES FUNCTIONS FOR Z
C
C     REFERENCE:  APPROXIMATE FORMULA ON PAGE 949, A&S
C                 TNC FUNCTION IS ALGORITHM AS 243  
C                    APPL. STATIST. (1989), VOL.38, NO. 1
C
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT NONE
C      SAVE
      
      INTEGER IFAULT
      
      DOUBLE PRECISION TP,NU,DELTA,Z,FP_Z_CDF,TNC,ANS
      
      IF( NU .GT. 20.D0) THEN
       Z  = (TP*(1.D0-1.D0/(4.D0*NU))-DELTA)/SQRT(1.D0+TP**2/(2.D0*NU))
       ANS=  FP_Z_CDF(Z) 
      ELSE
       ANS=  TNC(TP,NU,DELTA,IFAULT)
       IF(IFAULT .NE. 0) THEN
          WRITE(6,*) 'TROUBLE IN FP_TNC_CDF:TP,NU,DELTA',TP,NU,DELTA
       Z  = (TP*(1.D0-1.D0/(4.D0*NU))-DELTA)/SQRT(1.D0+TP**2/(2.D0*NU))
       ANS=  FP_Z_CDF(Z) 
       ENDIF
      ENDIF
      
      FP_TNC_CDF =  ANS
      
      RETURN
      END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_TNC_ICDF(P,NU,DELTA)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE NON-CENTRAL T INVERSE CDF
C
C     AUTHOR.....TIM COHN
C     DATE.......APRIL 9, 1999
C
C     REQUIRES FUNCTIONS FOR Z-INVERSE
C
C     REFERENCE:  INVERSION OF APPROXIMATE FORMULA ON PAGE 949, A&S
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT NONE
C      SAVE
      DOUBLE PRECISION P,NU,DELTA,X,ANS,FP_Z_ICDF,FP_TNC2_ICDF

      IF(NU .LE. 20.D0) THEN
           ANS = FP_TNC2_ICDF(P,NU,DELTA)
      ELSE
      X    =  FP_Z_ICDF(P)
      ANS  =  (4.D0*NU*(DELTA*(-1.D0 + 4.D0*NU) + 
     1           X*SQRT(1.D0 + 16.D0*NU**2 + 
     2           8.D0*NU*(-1.D0 + DELTA**2 - X**2))))/
     3           (1.D0 + 16.D0*NU**2 - 8.D0*NU*(1.D0 + X**2))
      ENDIF
     
      FP_TNC_ICDF =  ANS
      RETURN
      END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FP_TNC2_ICDF(P_IN,NU_IN,DELTA_IN)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE NON-CENTRAL T INVERSE CDF
C
C     AUTHOR.....TIM COHN
C     DATE.......APRIL 9, 1999
C
C     REQUIRES FUNCTIONS FOR Z-INVERSE, GAMMA INVERSE
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

      IMPLICIT NONE
C      SAVE
      
      INTEGER IFLAG
      
      DOUBLE PRECISION 
     1   P_IN,NU_IN,DELTA_IN,
     2   P,NU,DELTA,PARMS(2),
     3   AE,RE,X1,X2,B,C
     
      DOUBLE PRECISION FTNCINV,FP_Z_ICDF,FP_G2_ICDF
      
      EXTERNAL FTNCINV
      
      COMMON /ZNCT02/P,NU,DELTA

      DATA RE/1.D-6/,AE/1.D-6/

      P     =  P_IN
      NU    =  NU_IN
      DELTA =  DELTA_IN
      
      X1    =  FP_Z_ICDF(P)+DELTA
        	PARMS(1)  =  NU/2.D0
	        PARMS(2)  =  2.D0
      X2    =  SQRT(FP_G2_ICDF(1.D0-P,PARMS)/NU)
      
      IF(X2 .LE. 0.D0) GOTO 99
      
        B     =  MIN(0.D0,X1,1.D0/X2,X1/X2)
        C     =  MAX(0.D0,X1,1.D0/X2,X1/X2)
	      
      CALL DZEROIN(FTNCINV,B,C,RE,AE,IFLAG)
      
      IF(IFLAG .GE. 5) GOTO 99
      
      FP_TNC2_ICDF =  B
      
      RETURN
      
99    CONTINUE
        WRITE(6,*) 'ERROR IN FP_TNC2_ICDF',IFLAG,P,NU,DELTA
	READ(6,*)
	FP_TNC2_ICDF =  0.D0
      
      RETURN
      END
C*_*-*~*_*-*~*             NEW PROGRAM BEGINS HERE            *_*-*~*_*-*~*_*-*~
      DOUBLE PRECISION FUNCTION FTNCINV(TP)
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C 
C     PROGRAM TO COMPUTE INVERSE CDF OF NON-CENTRAL T DISTRIBUTION
C
C     AUTHOR.....TIM COHN
C     DATE.......APRIL 19, 1999
C
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////

        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c        SAVE
        DOUBLE PRECISION TP,ANS,P,NU,DELTA,FP_TNC_CDF
      
        COMMON /ZNCT02/P,NU,DELTA

	  ANS     =    FP_TNC_CDF(TP,NU,DELTA) - P
	  FTNCINV =    ANS
	  
	RETURN
        END
         SUBROUTINE MDL_MR(N,X,XD,MU,SIGMA,PPALL,XSYNTH,MOMS,QUANTS)

C===============================================================================
C
C        SUBROUTINE TO FIT LN2 DISTRIBUTION TO MULTIPLY-CENSORED DATA
C          USING THE MR METHOD (SEE HELSEL AND COHN, WRR, 1988)
C
C
C        AUTHOR........TIM COHN
C        DATE..........12 FEBRUARY 2003 (TAC)
C
C===============================================================================
C     
C     ***  N.B.  THIS METHOD SHOULD NOT BE APPLIED TO WATER QUALITY DATA 
C     ***          EXCEPT WITH GREAT CARE...  TAC (12 MARCH 2003)
C
C     ***        THE MR/MDL APPROACH ASSUMES A TYPE II CENSORING MODEL;
C     ***          CENSORING RELATED TO WATER QUALITY DATA IS INVARIABLY TYPE I
C
C     ***        SEE WARNINGS OR CONTACT AUTHOR IF THERE IS ANY QUESTION
C     ***          ABOUT THIS.
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
C        N         I*4       INPUT NUMBER OF OBSERVATIONS (NO-DETECTS COUNT)
C        X(N)      R*8       INPUT VECTOR CONTAINING DATA;  '0' INDICATES NO-DETECT
C        XD(N)     R*8       INPUT VECTOR CONTAINING CENSORING THRESHOLD FOR EACH PT.
C        MU        R*8       OUTPUT FITTED VALUE OF MU
C        SIGMA     R*8       OUTPUT FITTED VALUE OF SIGMA
C        PPALL(N)  R*8       OUTPUT VECTOR OF PLOTTING POSITIONS CORRESONDING TO X
C                              (SEE HELSEL & COHN, WRR 24(12), 1988)
C        XSYNTH(N) R*8       OUTPUT VECTOR OF REAL AND SYNTHETIC OBSERVATIONS
C                              XSYNTH(I) = X(I);                       X(I)>=XD(I) 
C                              XSYNTH(I) = EXP(MU+Z(PPALL(I))*SIGMA);  X(I)<XD(I)
C        MOMS(2)   R*8       OUTPUT VECTOR OF MEAN(X), VARIANCE(X)
C        QUANTS(5) R*8       OUTPUT VECTOR OF 10TH, 25TH, 50TH, 75TH, AND 90TH
C                              PERCENTILES OF SAMPLE USING WEIBULL PLOTTING POS.
C
C===============================================================================
C
C        N.B.   1)  "SYNTHETIC" VALUES ARE ASSIGNED TO THE "LESS-THAN" VALUES.
C               2)  THE "SYNTHETIC VALUES" MAY EXCEED THE CENSORING THRESHOLD
C                   (THIS OCCURS FOR TWO REASONS:
C                    1)  AN LN2 MODEL IS EMPLOYED TO MODEL THE CENSORED DATA
C                    2)  THE PLOTTING POSITION AND MR FORMULAS ASSUME A
C                        TYPE-II CENSORING MODEL.  IN FACT, CENSORING OF WATER
C                        QUALITY DATA ALMOST INVARIABLY RESULTS FROM A TYPE-I
C                        CENSORING PROCESS)   
C
C===============================================================================

      IMPLICIT NONE

      DOUBLE PRECISION
     1     X(*),XD(*),MU,SIGMA,PPALL(*),XSYNTH(*),MOMS(2),
     2     P(5),QUANTS(5),
     3     XS,XSS,XM,XV,XSD
     
      INTEGER
     1     N,I,NP
     
      DATA NP/5/,P/0.10,0.25,0.50,0.75,0.90/
         

      CALL PPFIT(N,X,XD,MU,SIGMA,PPALL,XSYNTH)
         
        XS  = 0.D0
	    XSS = 0.D0
	  DO 30 I=1,N
	    XS  =  XS+XSYNTH(I)
	    XSS =  XSS+XSYNTH(I)**2
30    CONTINUE
        MOMS(1)  =  XS/DBLE(N)
        MOMS(2)  =  (XSS-N*MOMS(1)**2)/(N-1.D0)
        
      CALL QTL(XSYNTH,N,P,NP,QUANTS)

      RETURN
      END

         SUBROUTINE PPFIT(N,X,XD,MU,SIGMA,PPALL,XSYNTH)

C===============================================================================
C
C        SUBROUTINE TO FIT LN2 DISTRIBUTION TO MULTIPLY-CENSORED DATA
C
C        AUTHOR........TIM COHN
C        DATE..........APRIL 7, 1986
C        MODIFIED......12 FEBRUARY 2003 (TAC)
C         --REMOVED ERRORS INTRODUCED IN 1986
C           WHEN CODE WAS REVISED BY DRH.  
C         --IMPLICIT NONE ADDED; ALL VARIABLES DECLARED
C         --DOUBLE PRECISION
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
C        N         I*4       INPUT NUMBER OF OBSERVATIONS (NO-DETECTS COUNT)
C        X(N)      R*8       INPUT VECTOR CONTAINING DATA;  '0' INDICATES NO-DETECT
C        XD(N)     R*8       INPUT VECTOR CONTAINING CENSORING THRESHOLD FOR EACH PT.
C        MU        R*8       OUTPUT FITTED VALUE OF MU
C        SIGMA     R*8       OUTPUT FITTED VALUE OF SIGMA
C        PPALL(N)  R*8       OUTPUT VECTOR OF PLOTTING POSITIONS CORRESONDING TO X
C                              (SEE HELSEL & COHN, WRR 24(12), 1988)
C        XSYNTH(N) R*8       OUTPUT VECTOR OF REAL AND SYNTHETIC OBSERVATIONS
C                              XSYNTH(I) = X(I);                       X(I)>=XD(I) 
C                              XSYNTH(I) = EXP(MU+Z(PPALL(I))*SIGMA);  X(I)<XD(I)
C===============================================================================
C
C        N.B.   1)  "SYNTHETIC" VALUES ARE ASSIGNED TO THE "LESS-THAN" VALUES.
C               2)  THE "SYNTHETIC VALUES" MAY EXCEED THE CENSORING THRESHOLD
C                   (THIS OCCURS FOR TWO REASONS:
C                    1)  AN LN2 MODEL IS EMPLOYED TO MODEL THE CENSORED DATA
C                    2)  THE PLOTTING POSITION AND MR FORMULAS ASSUME A
C                        TYPE-II CENSORING MODEL.  IN FACT, CENSORING OF WATER
C                        QUALITY DATA ALMOST INVARIABLY RESULTS FROM A TYPE-I
C                        CENSORING PROCESS)   
C
C===============================================================================
C
C        NT        I*4       NUMBER OF THRESHOLDS
C        NB(NT)    I*4       NUMBER OF OBSERVATIONS BELOW EACH THRESHOLD
C        ND(NT)    I*4       NUMBER OF DETECTS ABOVE THIS THR. AND BELOW NEXT
C        NVAL      I*4       TOTAL NUMBER OF DETECTS
C
C===============================================================================

         IMPLICIT NONE

         INTEGER N_PTS,N_T
         PARAMETER (N_T=10000,N_PTS=10000)

         DOUBLE PRECISION
     1     X(*),XD(*),MU,SIGMA,
     2     W(N_PTS),WD(N_PTS),WTHRESH(N_T),Y(N_PTS),PP(N_PTS),ALPHA,
     3     PPC(N_PTS),PPALL(*),XSYNTH(*)
     
         INTEGER
     1     N,ND(N_T),NB(N_T),NT,NVAL,j

         CALL ARRANGE(X,XD,N,NT,ND,NB,NVAL,Y)

         CALL PPLOT(NT,ND,NB,PP,PPC)
         
         CALL PPSOLVE(Y,PP,NVAL,MU,SIGMA)
         
         CALL PLPOS(N,X,XD,PP,PPC,MU,SIGMA,PPALL,XSYNTH)

         RETURN
         END
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
         SUBROUTINE ARRANGE(X,XD,N,NT,ND,NB,NVAL,Y)
C===============================================================================
C
C        SUBROUTINE DESIGNED TO ARRANGE DATA FOR PPFIT, WHICH EMPLOYS DATA TO
C        FIT LN2 DISTRIBUTION TO MULTIPLY-CENSORED DATA
C
C        AUTHOR........TIM COHN
C        DATA..........APRIL 7, 1986
C        MODIFIED......12 FEBRUARY 2003 (TAC)
C         --REMOVED ERRORS INTRODUCED IN 1986
C           WHEN CODE WAS REVISED BY DRH.  
C         --IMPLICIT NONE ADDED; ALL VARIABLES DECLARED
C         --DOUBLE PRECISION
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
C        X(N)      R*8       INPUT VECTOR CONTAINING DATA;  '0' INDICATES NO-DETECT
C        XD(N)     R*8       INPUT VECTOR CONTAINING CENSORING THRESHOLD FOR EACH PT.
C        N         I*4       INPUT NUMBER OF OBSERVATIONS (NO-DETECTS COUNT)
C        NT        I*4       OUTPUT NUMBER OF THRESHOLDS
C        NB(NT)    I*4       OUTPUT NUMBER OF OBSERVATIONS BELOW EACH THRESHOLD
C        ND(NT)    I*4       OUTPUT NUMBER OF DETECTS ABOVE THIS THR. AND BELOW NEXT
C        NVAL      I*4       OUTPUT TOTAL NUMBER OF DETECTS
C        Y         R*8       OUTPUT VECTOR OF SORTED DETECTS
C
C===============================================================================

         IMPLICIT NONE

         DOUBLE PRECISION
     1      X(*),XD(*),Y(*),
     2      THRESH(200),PLUSINF
     
         INTEGER
     1      N,NT,NVAL,ND(*),NB(*),IP(10000),
     2      I,ICT,J

         DATA PLUSINF/1.0D30/

         CALL PORDER(XD,N,IP)

           ICT          =  0
           J            =  1
           THRESH(1)    =  XD(IP(1))
           NB(1)        =  0

         DO 10 I=1,N

            IF(XD(IP(I)) .NE. THRESH(J)) THEN
              J    =  J+1
              THRESH(J) =  XD(IP(I))
              ND(J)     =  0
              NB(J)     =  0
            ENDIF

            IF(X(IP(I)) .GE. THRESH(J)) THEN
              ICT       =  ICT+1
              Y(ICT)    =  X(IP(I))
            ELSE
              NB(J)     =  NB(J)+1
            ENDIF
   10    CONTINUE

         NT             =  J
         NVAL           =  ICT
         THRESH(NT+1)   =  PLUSINF

         CALL SHELLSORT(Y,ICT)
		
           J       =  1
           ND(1)   =  0
         DO 20 I=1,NVAL
   30      IF(Y(I) .GE. THRESH(J+1)) THEN
              J    =  J+1
              ND(J)=  0
              GOTO 30
           ELSE
              ND(J)=  ND(J)+1
           ENDIF
   20    CONTINUE

         DO 40 I=2,NT
           NB(I)   =  NB(I)+NB(I-1)+ND(I-1)
   40    CONTINUE

         RETURN
         END
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
         SUBROUTINE PORDER(X,N,IX)
C===============================================================================
C
C        SUBROUTINE PORDER GIVES THE PERMUTATION OF A USER-SUPPLIED
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
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
         SUBROUTINE PPLOT(NT,ND,NB,PP,PPC)
C===============================================================================
C
C        SUBROUTINE TO FIND PLOTTING POSITIONS FOR DATA CENSORED AT MULTIPLE THRESHOLDS
C
C        AUTHOR........TIM COHN
C        DATE..........APRIL 7, 1986
C        MODIFIED......12 FEBRUARY 2003 (TAC)
C         --REMOVED ERRORS INTRODUCED IN 1986
C           WHEN CODE WAS REVISED BY DRH.  
C         --IMPLICIT NONE ADDED; ALL VARIABLES DECLARED
C         --DOUBLE PRECISION
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
C        NT        I*4       INPUT NUMBER OF THRESHOLDS
C        ND(NT)    I*4       INPUT NUMBER OF DETECTS ABOVE THIS THR. AND BELOW NEXT
C        NB(NT)    I*4       INPUT NUMBER OF OBSERVATIONS BELOW EACH THRESHOLD
C        PP(NA)    R*8       OUTPUT VECTOR OF PLOTTING POSITIONS
C        PPC(NB)   R*8       INPUT VECTOR OF PLOTTING POSITIONS CORRESONDING TO X<XD
C
C===============================================================================

         IMPLICIT NONE

         INTEGER N_PTS,N_T
         PARAMETER (N_T=10000,N_PTS=10000)

         DOUBLE PRECISION
     1     PP(*),PE(0:N_T),ALPHA,PD,PPT,PPC(*)
     
         INTEGER
     1     ND(*),NB(*),NT,
     2     ISUM,I,J,JSUM,NBT

         DATA ALPHA/0.000D0/

           PE(NT+1)  =  0.D0
         DO 10 I=NT,1,-1
           PD        =  ND(I)/FLOAT(MAX(1,ND(I))+NB(I))
           PE(I)     =  PE(I+1) + (1.D0-PE(I+1)) * PD
   10    CONTINUE

           ISUM    =  0
           JSUM    =  0
           PE(0)   =  1.00D0

         DO 20 I=1,NT
           DO 30 J=1,ND(I)
               PPT  =  (J-ALPHA)/(ND(I) + 1.0 - 2.0*ALPHA)
               PP(ISUM+J)    =  (1.D0-PE(I)) + (PE(I)-PE(I+1))*PPT
   30      CONTINUE
               ISUM =  ISUM+ND(I)

           IF(I .EQ. 1) THEN
              NBT = NB(1)
           ELSE
              NBT = NB(I)-NB(I-1)-ND(I-1)
           ENDIF
           
           DO  40 J=1,NBT
             PPT= (J-ALPHA)/(NBT+1.0-2*ALPHA)
             PPC(JSUM+J) = (1-PE(I))*PPT
   40      CONTINUE
             JSUM=JSUM+NBT

   20    CONTINUE

         RETURN
         END
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
         SUBROUTINE PPSOLVE(Y,PP,NVAL,MU,SIGMA)
C===============================================================================
C
C        SUBROUTINE TO SOLVE FOR LEAST SQUARES ESTIMATES OF MU AND SIGMA
C        ASSUMING A LINEAR MODEL
C
C        AUTHOR........TIM COHN
C        DATE..........APRIL 7, 1986
C        MODIFIED......12 FEBRUARY 2003 (TAC)
C         --REMOVED ERRORS INTRODUCED IN 1986
C           WHEN CODE WAS REVISED BY DRH.  
C         --IMPLICIT NONE ADDED; ALL VARIABLES DECLARED
C         --DOUBLE PRECISION
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
C        Y(N)      R*8       INPUT VECTOR CONTAINING SORTED DETECT-DATA
C        PP(NA)    R*8       INPUT VECTOR OF PLOTTING POSITIONS
C        NVAL      I*4       INPUT TOTAL NUMBER OF DETECTS
C        MU        R*8       OUTPUT FITTED VALUE OF MU
C        SIGMA     R*8       OUTPUT FITTED VALUE OF SIGMA
C
C===============================================================================

         IMPLICIT NONE

         INTEGER N_PTS,N_T
         PARAMETER (N_T=10000,N_PTS=10000)

         DOUBLE PRECISION
     1     Y(*),PP(*),MU,SIGMA,
     2     PSIG(N_PTS),LOGY(N_PTS),
     3     FP_Z_ICDF,
     4     SUMY,SUMP,SUMPY,SUMP2,YBAR,PBAR
     
         INTEGER
     1     I,NVAL

              SUMY =  0.D0
              SUMP =  0.D0
              SUMPY=  0.D0
              SUMP2=  0.D0

         DO 10 I=1,NVAL
              PSIG(I)   =  FP_Z_ICDF(PP(I))
              LOGY(I)   =  LOG(Y(I))
              SUMY      =  SUMY    +  LOGY(I)
              SUMPY     =  SUMPY   +  LOGY(I)*PSIG(I)
              SUMP      =  SUMP    +  PSIG(I)
              SUMP2     =  SUMP2   +  PSIG(I)**2
   10    CONTINUE
              YBAR      =  SUMY/NVAL
              PBAR      =  SUMP/NVAL

              SIGMA     =  (SUMPY-NVAL*PBAR*YBAR)/(SUMP2-NVAL*PBAR**2)
              MU        =  YBAR - SIGMA*PBAR
         RETURN
         END
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
         SUBROUTINE PLPOS(N,X,XD,PP,PPC,MU,SIGMA,PPALL,XSYNTH)
C===============================================================================
C
C        SUBROUTINE PLPOS GIVES THE PLOTTING POSITIONS FOR ALL OF THE
C          OBSERVATIONS, CENSORED AND UNCENSORED, IN THE ORIGINAL INPUT
C          DATA SET
C
C
C        AUTHOR....TIM COHN
C        DATE......12 FEBRUARY 2003 (TAC)
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
C        N         I*4       INPUT NUMBER OF OBSERVATIONS (NO-DETECTS COUNT)
C        X(N)      R*8       INPUT VECTOR CONTAINING DATA;  '0' INDICATES NO-DETECT
C        XD(N)     R*8       INPUT VECTOR CONTAINING CENSORING THRESHOLD FOR EACH PT.
C        PP(NA)    R*8       INPUT VECTOR OF PLOTTING POSITIONS CORRESONDING TO X>=XD
C                              (SEE HELSEL & COHN, WRR 24(12), 1988)
C        PPC(NB)   R*8       INPUT VECTOR OF PLOTTING POSITIONS CORRESONDING TO X<XD
C        MU        R*8       INPUT FITTED VALUE OF MU
C        SIGMA     R*8       INPUT FITTED VALUE OF SIGMA
C        PPALL(N)  R*8       OUTPUT VECTOR OF PLOTTING POSITIONS CORRESONDING TO X
C                              (SEE HELSEL & COHN, WRR 24(12), 1988)
C        XSYNTH    R*8       OUTPUT VECTOR OF REAL AND SYNTHETIC OBSERVATIONS
C                              XSYNTH(I) = X(I);                       X(I)>=XD(I) 
C                              XSYNTH(I) = EXP(MU+Z(PPALL(I))*SIGMA);  X(I)<XD(I)
C
C===============================================================================

         IMPLICIT NONE
         
         INTEGER N_PTS,N_T
         PARAMETER (N_T=10000,N_PTS=10000)

         DOUBLE PRECISION 
     1     X(*),XD(*),MU,SIGMA,PPALL(*),PP(*),PPC(*),XSYNTH(*),
     2     FP_Z_ICDF
         
         INTEGER
     1     IX(N_PTS),IXD(N_PTS),N,NA,NB,I

         CALL PORDER(X,N,IX)
         CALL PORDER(XD,N,IXD)
         
           NA  =  0
           NB  =  0
         DO 10 I=1,N
           IF(X(IX(I)) .GE. XD(IX(I))) THEN
             NA            =  NA+1
             PPALL(IX(I))  =  PP(NA)
             XSYNTH(IX(I)) =  X(IX(I))
           ENDIF
           IF(X(IXD(I)) .LT. XD(IXD(I))) THEN
             NB            =  NB+1
             PPALL(IXD(I)) =  PPC(NB)
             XSYNTH(IXD(I))=  EXP(MU+FP_Z_ICDF(PPC(NB))*SIGMA)
           ENDIF
10       CONTINUE

         RETURN
         END
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
         SUBROUTINE SHELLSORT(X,N)
C===============================================================================
C
C        SUBROUTINE TO SORT A VECTOR IN PLACE
C
C        AUTHOR........TIM COHN
C        DATE..........12 FEBRUARY 2003 (TAC)
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
C        X(N)      R*8       INPUT/OUTPUT VECTOR OF DATA
C        N         I*4       INPUT SAMPLE SIZE
C
C===============================================================================

      IMPLICIT NONE
      
      DOUBLE PRECISION X(*),R
      
      INTEGER N,M,I,J,S,Z
      
      M=N
  100 M=INT(M/2)
      IF (M.EQ.0) GOTO 210
      DO 190 S=1,M
  130 I=S
      J=S+M
      Z=0
  140 IF (X(I).LE.X(J)) GOTO 160
      Z=1
      R=X(I)
      X(I)=X(J)
      X(J)=R
  160 I=J
      J=J+M
      IF (J.LT.(N+1)) GOTO 140
      IF (Z.EQ.1) GOTO 130
  190 CONTINUE
      GOTO 100
  210 RETURN
      END
C****|===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
         SUBROUTINE QTL(X,N,P,NP,Q)
C===============================================================================
C
C        SUBROUTINE TO COMPUTE QUANTILES OF A VECTOR
C        THIS EMPLOYS THE WEIBULL OR GUMBEL PLOTTING POSITIONS (I/(N+1))
C
C        AUTHOR........TIM COHN
C        DATE..........12 FEBRUARY 2003 (TAC)
C
C===============================================================================
C
C     PROPERTY OF US GOVERNMENT, U.S. GEOLOGICAL SURVEY
C
C     *** DO NOT MODIFY WITHOUT AUTHOR'S CONSENT ***
C
C     AUTHOR CAN BE CONTACTED AT:  TACOHN@USGS.GOV (703/648-5711)   
C
C
C===============================================================================
C
C        X(N)      R*8       INPUT VECTOR OF DATA
C        N         I*4       INPUT SAMPLE SIZE
C        P(N)      R*8       INPUT VECTOR OF PROBABILITIES (PERCENTILES/100) TO 
C                              ESTIMATE
C        Q(N)      R*8       OUTPUT VECTOR OF QUANTILES 
C
C===============================================================================
C
C        N.B.   1)  WEIBULL (AKA "GUMBEL") PLOTTING POSTIONS ARE USED, WHICH 
C                   CORRESPOND TO ALPHA=0.0 IN THE GENERAL EQUATION FOR 
C                   PLOTTING POSTIONS PP(I) =(I-ALPHA)/(N+1-2*ALPHA)
C                   EXCEL, AND MANY OTHER STATISTICAL PACKAGES, EMPLOY THE 
C                   "HAZEN" PLOTTING POSITIONS (ALPHA=0.5).
C                   
C===============================================================================

         IMPLICIT NONE
         
         INTEGER N_PTS
         PARAMETER (N_PTS=10000)

         DOUBLE PRECISION 
     1     X(*),P(*),Q(*),XS(0:N_PTS),FN,ALPHA
     
         INTEGER 
     1     I,INDX,NP,N
     
         CALL DSVRGN(N,X,XS(1))
           XS(0)     = -9.0D99
           XS(N+1)   =  9.0D99
         
         DO 20 I=1,NP
           FN        =  MAX(0.D0,MIN(1.D0,P(I)))*(N+1.D0)
           INDX      =  INT(FN)
           ALPHA     =  FN-INDX
           Q(I)      =  (1.D0-ALPHA)*XS(INDX)+ALPHA*XS(INDX+1)
20       CONTINUE

         RETURN
         END

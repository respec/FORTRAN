      SUBROUTINE P3_MOM_BIN(REG_SKEW,REG_WGT,N,TL,TU,PARMS_OUT)
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     FITS PEARSON TYPE III DISTRIBUTION TO CENSORED DATA USING 
C     AN EM ALGORITHM AND METHOD OF MOMENTS ESTIMATORS
C
C     FITS BINOMIAL-CENSORED DATA
C
C     AUTHOR....TIM COHN
C     DATE......FEBRUARY 9, 1994
C
C     MODIFIED 2/8/95         TAC
C     MODIFIED 2/22/95  TAC
C     MODIFIED 12/16/96 TAC;  FIXED BIAS-CORRECTION FACTORS
C                                               AND ADDED SECOND CONVERGENCE CRITERION
C     MODIFIED 11/17/98  TAC  CONV CRIT. ADDED
C     MODIFIED 03/13/99  TAC  REMOVED BIAS CORRECTION FACTORS
C
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     MODIFIED 10/31/98 TAC;  ADDED REGIONAL SKEWNESS/FIXED SKEWNESS
C                             CAPABILITY;  NOTE THAT CALL NOW HAS TWO
C                             ADDITIONAL ARGUMENTS:
C                               REG_SKEW    IS THE REGIONAL SKEW
C                               REG_WGT     IS THE RELATIVE WEIGHT (0-1) TO
C                                           ASSIGN REGIONAL VS AT-SITE SKEW
C                                           
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     NOTE: TL(I) IS THE LOWER BOUND FOR THE I-TH OBSERVATION
C                 TU(I) IS THE UPPER BOUND
C                       IF( TU = TL) THEN WE HAVE AN ORDINARY OBSERVATION AT TU
C
C
      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
      
      COMMON /PERF01/ITERS,IFAIL,DISF
      COMMON /REG001/RSKEW,RWGT
      
      PARAMETER (NSIZE=20)
      
      DIMENSION 
     1     TL(*),TU(*),PARMS_OUT(3),PARMS(3,NSIZE),MOMS(3,NSIZE),
     2     D11(NSIZE)
      
      DATA TOL1/1.D-6/, TOL2/1.D-8/, TOL3/1.D-8/

            RSKEW = REG_SKEW
            RWGT  = REG_WGT
            
            CALL MOMS_INIT(N,TL,TU,PARMS(1,1),MOMS(1,1))
      DO 20 J=1,6000
      DO 10 I=2,NSIZE

            CALL PARMS_P3(MOMS(1,I-1),PARMS(1,I))
		
            CALL MOMS_P3(N,TL,TU,PARMS(1,I),MOMS(1,I))

            D11(I)    =     DIST_P3(MOMS(1,I),MOMS(1,I-1))

	    IF(J .GT. 2 .AND. I .EQ. 2) THEN
	      WRITE(*,*) NSIZE*(J-1),'MOMS: ',(MOMS(KX,I),KX=1,3),D11(I)
	    ENDIF
C
C     CONVERGED?
C
            IF(D11(I) .EQ. 0.D0) THEN
              IB = I
              GOTO 99
            ENDIF
C
C   CHECK FOR ADEQUATE CONVERGENCE; NON-LINEARITY
C
            IF( (D11(I) .LT. TOL1) .AND. (I .GT. 3 ) ) THEN
              IF( ( D11(I-1) .LE. D11(I  ) ) .AND. 
     1            ( D11(I-1) .LE. D11(I-2) )  ) THEN
                IB = I-1
                GOTO 99
              ENDIF	
            ENDIF	
		
10    CONTINUE
                        DO 20 K=1,3
                        MOMS(K,1) = (MOMS(K,NSIZE)+MOMS(K,NSIZE-1))/2.D0
20    CONTINUE
C
C     FAILURE TO CONVERGE
C
            WRITE(*,*) ' FAILURE TO CONVERGE IN P3_MOM'
            ITERS =     0
            IFAIL =     1
      RETURN
C
C     SUCCESSFUL RETURN
C
99    CONTINUE
        DO 15 K=1,3
          PARMS_OUT(K) = PARMS(K,IB)
15      CONTINUE
          ITERS =     I+(J-1)*(NSIZE-1)
          IFAIL =     0
	  DISF  =     D11(IB)
      RETURN
      END
C
      SUBROUTINE MOMS_P3(N,TL,TU,PARMS,MOMS)
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     COMPUTES THE EXPECTED VALUE OF A G3 VARIATE WITH PARAMETERS
C     GIVEN BY PARMS(), WHOSE VALUE IS 
C     KNOWN TO LIE IN THE INTERVAL (TL,TU)
C
      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
      DOUBLE PRECISION 
     1     N_TOT,TL(*),TU(*),PARMS(3),MOMS(3),S_E(3),S_C(3)

C     NOTE THAT SETTING BCF TO 0 SHUTS OFF BIAS CORRECTION; 1.0 MEANS BIAS CORRECTED
      DATA BCF/1.D0/

            INIT  =     1
            GOTO 5
      ENTRY MOMS_INIT(N,TL,TU,PARMS,MOMS)
            INIT  =     0
5     CONTINUE

                  N_C         =     0
                  N_E         =     0
                  S_E(1)      =     0.D0
                  S_C(1)      =     0.D0
            DO 20 I=1,N
                  IF(TL(I) .EQ. TU(I)) THEN
                        N_E   =     N_E+1
                        S_E(1)      =     S_E(1) + TL(I)
                  ELSE IF(INIT .EQ. 0) THEN
                        GOTO 20
                  ELSE
                        N_C         =     N_C+1
                        S_C(1)      =     S_C(1) + 
     1                          PSUM_P3F(TL(I),TU(I),1,PARMS,0.D0)
                  ENDIF
20    CONTINUE
            N_TOT =     N_E + N_C
            MOMS(1)     =      (S_E(1) + S_C(1) )/N_TOT
            
      DO 30 J=2,3
                  S_E(J)      =     0.D0
                  S_C(J)      =     0.D0
            DO 40 I=1,N
                  IF(TL(I) .EQ. TU(I)) THEN
                        S_E(J)      =   S_E(J) + ( TL(I) - MOMS(1) )**J
                  ELSE IF(INIT .EQ. 0) THEN
                        GOTO 40
                  ELSE
                        S_C(J)  = S_C(J) + PSUM_P3F(TL(I),TU(I),
     1                            J,PARMS,MOMS(1))
                  ENDIF
40    CONTINUE
30    CONTINUE
            C2          =     FLOAT(N_E)/(N_E-1*BCF)
      MOMS(2)     =     ( C2*S_E(2) + S_C(2) )/N_TOT
            C3          =     ( N_E**2 )/
     1                             ( (N_E-1.*BCF)*(N_E-2.*BCF) )
      MOMS(3)     =     ( C3*S_E(3) + S_C(3) )/( N_TOT * MOMS(2)**1.5D0)

      RETURN
      END
      
      DOUBLE PRECISION FUNCTION PSUM_P3F(TL,TU,K,PARMS,X_OFF)
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C     PROGRAM TO COMPUTE KTH NON-CENTRAL MOMENT OF PEARSON TYPE III 
C     RANDOM VARIABLE CENSORED AT T WITH PARAMETERS PARMS
C
      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
	DIMENSION PARMS(3)
      COMMON/TACZXZ/ P_OLD(3,3),TL_OLD(3),TU_OLD(3),
     1     ARG_OLD(3),X_OFF_OLD


	   IF( 
     1     (K .GT. 3) .OR.
     2     (K .LT. 1) .OR.
     3     (PARMS(1) .NE. P_OLD(1,K)) .OR.
     4     (PARMS(2) .NE. P_OLD(2,K)) .OR.
     5     (PARMS(3) .NE. P_OLD(3,K)) .OR.
     6     (TL       .NE. TL_OLD(K)  ) .OR.
     7     (X_OFF    .NE. X_OFF_OLD  ) .OR.
     8     (TU       .NE. TU_OLD(K)  )
     9     ) THEN

                  ARG_OLD(K)  =     FP_G3_MOM_TRCO(PARMS,X_OFF,TL,TU,K)             
            DO 10 I=1,3
                  P_OLD(I,K)  =     PARMS(I)
10          CONTINUE
                  TL_OLD(K)   =     TL
                  TU_OLD(K)   =     TU
                  X_OFF_OLD   =     X_OFF
      ENDIF
	
            PSUM_P3F    =     ARG_OLD(K)
      RETURN
      END

      SUBROUTINE PARMS_P3(M,PARMS)
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C
      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
      DOUBLE PRECISION M(3),PARMS(3)
      COMMON /REG001/RSKEW,RWGT

      AVE_SKEW          =     RWGT*RSKEW + (1.D0-RWGT)*M(3)
      ALPHA             =     4.D0/MAX(1.D-6,(AVE_SKEW**2))
            PARMS(2)    =     ALPHA
      BETA              =     SIGN1(AVE_SKEW)*SQRT(M(2)/ALPHA)
            PARMS(3)    =     BETA
      TAU                     =     M(1) - ALPHA*BETA
            PARMS(1)    =     TAU

      RETURN
      END

      DOUBLE PRECISION FUNCTION DIST_P3(M1,M2)
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C
      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
      DIMENSION M1(3),M2(3),IP(3)
      DATA IP/1,2,0/

            SUM         =     0.D0
      DO 10 I=1,3
            SUM         =     SUM + (M1(I)-M2(I))**2/M2(2)**IP(I)
10    CONTINUE
            DIST_P3     =     SUM
            
      RETURN
      END

      FUNCTION N_OFF_SUPPORT(N,TL,TU,PARMS)
C****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C
      IMPLICIT DOUBLE PRECISION (A-H,M,O-Z)
      DIMENSION TL(*),TU(*),PARMS(3)

      TAU         =     PARMS(1)
      BETA  =     PARMS(2)
      
      NCT         =     0
      
      DO 10 I=1,N
        IF(TL(I) .NE. TU(I)) THEN
            IF(BETA .LT. 0.D0 .AND. TL(I) .GT. TAU) THEN
                  NCT   =     NCT+1
            ELSE IF(BETA .GT. 0.D0 .AND. TU(I) .LT. TAU) THEN
                  NCT   =     NCT+1
            ENDIF
        ENDIF
10    CONTINUE

      N_OFF_SUPPORT     =     NCT
            
      RETURN
      END
      
                  
            
      
      

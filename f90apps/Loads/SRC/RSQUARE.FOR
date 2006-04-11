        SUBROUTINE R_SQUARE(NC,NP,YPRED,X1,S,R2L,R2C)
C*** REVISION 92.05 5/8/92
C===================================================================
C
C       WILL COMPUTE THE R-SQUARED STATISTIC FOR THE REGRSSION BASED
C       ON LOAD AND FOR THE REGRESSION BASED ON CONCENTRATION
C
C       AUTHOR.........TIM COHN
C       DATE...........MAY 7, 1992 (TAC)
C===================================================================
C
C       DEFINITION OF VARIABLES
C
C         NC        I*4     NUMBER OF OBSERVATIONS
C         NP        I*4     NUMBER OF PARAMETERS (1 ==> CONSTANT)
C         YPRED(NC) R*8     THE Y PREDICTIONS (LOG OF LOAD)
C         X1(NC)    R*8     THE LOGARITHM OF FLOW
C         S         R*8     THE ESTIMATED STANDARD DEV. OF RESIDS.
C         R2L       R*8     THE R^2 FOR THE LOAD REGRESSION
C         R2C       R*8     THE R^2 FOR THE CONC. REGRESSION
C
C===================================================================

        IMPLICIT DOUBLE PRECISION (A-H,L,O-Z)
        PARAMETER (NSIZE=4000)
        DIMENSION YPRED(*),X1(*),YPRED2(NSIZE)

C===================================================================
C
C        FIRST, COMPUTE THE ORDINARY R^2
C
          SUM = 0.0
          DO 5 I = 1,NC
            SUM = SUM + YPRED(I)
    5     CONTINUE
          SUMSR  =  SUMSQ(YPRED,NC)-SUM**2/NC
          TSS    =  (NC-NP)*S**2 + SUMSR
          R2L    =  SUMSR/TSS

C===================================================================
C
C        NOW, COMPUTE R^2 FOR THE CONCENTRATION (REMOVE 1 FROM SLOPE)
C
        SUM = 0.0
        DO 10 I=1,NC
          YPRED2(I)  =  YPRED(I) - X1(I)
          SUM = SUM + YPRED2(I)
   10   CONTINUE

          SUMSR  =  SUMSQ(YPRED2,NC)-SUM**2/NC
          TSS    =  (NC-NP)*S**2 + SUMSR
          R2C    =  SUMSR/TSS

        RETURN
        END

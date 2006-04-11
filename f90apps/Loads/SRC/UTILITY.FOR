C*** REVISION 92.05 5/8/92
C===================================================================
C
C        SET OF UTILITY FUNCTIONS TO BE USED WITH GENERAL FORTRAN
C        PROGRAMS
C
C          SUM     --THE SUM OF A VECTOR
C          SUMSQ   --THE SUM A SQUARES OF A VECTOR
C          CENTMOM --THE M-TH CENTRAL MOMENT OF A VECTOR
C          CORR    --THE CORRELATION BETWEEN TWO VARIABLES
C          DTIME   --THE 'DECIMAL TIME' GIVEN DAY,MONTH,YEAR AND TIME
C
C===================================================================

         DOUBLE PRECISION FUNCTION SUM(X,N)
C===================================================================
C
C      FUNCTION TO COMPUTE THE SUM OF A VECTOR
C
C       AUTHOR.........TIM COHN
C       DATE...........NOVEMBER 2, 1988
C
C===================================================================
         DOUBLE PRECISION X(*)
         SUM = X(1)
         DO 10 I=2,N
         SUM = SUM+X(I)
   10    CONTINUE
         RETURN
         END
         DOUBLE PRECISION FUNCTION SUMSQ(X,N)
C===================================================================
C
C      FUNCTION TO COMPUTE THE SUM OF A VECTOR
C
C       AUTHOR.........TIM COHN
C       DATE...........NOVEMBER 2, 1988
C
C===================================================================
         DOUBLE PRECISION X(*)
         SUMSQ = X(1)**2
         DO 10 I=2,N
         SUMSQ = SUMSQ+X(I)**2
   10    CONTINUE
         RETURN
         END

         DOUBLE PRECISION FUNCTION CENT_MOM(X,N,M)
C===================================================================
C
C      FUNCTION TO COMPUTE THE M-TH MOMENT OF A VECTOR
C
C       AUTHOR.........TIM COHN
C       DATE...........NOVEMBER 2, 1988
C
C===================================================================
         DOUBLE PRECISION X(*),SUM
           SUM = 0.0
           DO 5 I=1,N
             SUM = X(I) + SUM
    5      CONTINUE
           XBAR    =  SUM/N
           CENT_MOM = (X(1)-XBAR)**M
         IF(M .LE. 1) THEN
           CENT_MOM = XBAR
         ELSE
           DO 10 I=2,N
             CENT_MOM = CENT_MOM+(X(I)-XBAR)**M
   10      CONTINUE
             CENT_MOM = CENT_MOM/N
         ENDIF
         RETURN
         END

         DOUBLE PRECISION FUNCTION CORR(N,X,Y)
C=======================================================================
C
C       SUBROUTINE TO COMPUTE CORRELATION BETWEEN TWO VARIABLES
C
C       DEFINITION OF VARIABLES
C
C         N         I*4     NUMBER OF OBSERVATIONS
C         X(N)      I*8     FIRST VARIABLE
C         Y(N)      I*4     SECOND VARIABLE
C         C         R*8     CORRELATION BETWEEN VARIABLES (OUTPUT)
C
C       AUTHOR........TIM COHN
C       DATE..........NOVEMBER 7, 1988
C
C=======================================================================

        IMPLICIT DOUBLE PRECISION (A-H,L-M,O-Z)
        DIMENSION X(*),Y(*)

          C1  =  0.D0
          MX  =  0.D0
          MY  =  0.D0
          SX  =  0.D0
          SY  =  0.D0
        DO 10 I=1,N
          C1  =  C1+X(I)*Y(I)
          MX  =  MX+X(I)
          MY  =  MY+Y(I)
          SX  =  SX+X(I)**2
          SY  =  SY+Y(I)**2
   10   CONTINUE

          CORR  =  (N*C1-MX*MY)/
     1             SQRT(MAX(1.D-44, (N*SX-MX**2)*(N*SY-MY**2) ) )

        RETURN
        END

        DOUBLE PRECISION FUNCTION DTIMEC(IYEAR,IMO,IDAY,ITIME)
C===================================================================
C
C      FUNCTION TO CONVERT THE YEAR,MONTH,DAY INTO A
C      DECIMAL YEAR VALUE
C
C       AUTHOR.........TIM COHN
C       DATE...........NOVEMBER 2, 1988
C
C===================================================================

         IMPLICIT DOUBLE PRECISION (A-H,L-M,O-Z)
         INTEGER NDAYS(12)

         DATA NDAYS/0,31,59,90,120,151,181,212,243,273,304,334/

         IF(IMO .LT. 1 .OR. IMO .GT. 12) GOTO 99

           ILEAP  =  0.0
         IF(MOD(IYEAR,4) .EQ. 0) THEN
           DAYS = 366.0
           IF(IMO .GT. 2) ILEAP  =  1.0
         ELSE
           DAYS = 365.0
         ENDIF

         IH     =  ITIME/100
         FDAY   =  (IH + (ITIME-100.0*IH)/60.0)/24.0
         DTIMEC  =  IYEAR+(NDAYS(IMO)+IDAY-1+ILEAP+FDAY)/DAYS

         RETURN

   99    CONTINUE
           WRITE(*,*) 'MONTH OUT OF RANGE (DTIMEC)'
         RETURN
         END

        FUNCTION NDAYS2(IDAY,IMONTH,IYEAR)
C===================================================================
C
C       PROGRAM TO COMPUTE THE NUMBER OF DAYS SINCE 1900
C
C       CORRECTS A BUG IN THE IMSL ROUTINE NDAYS
C
C       AUTHOR.........TIM COHN
C       DATE...........NOVEMBER 14, 1988
C
C===================================================================
C
C       DEFINITIONS OF VARIABLES
C
C         IDAY      I*4     DAY OF MONTH
C         IMONTH    I*4     MONTH
C         IYEAR     I*4     YEAR
C
C===================================================================

         IF(IYEAR .NE. 0) THEN
           IM      =  MOD(IMONTH-1,12)+1
           NXYR    =  (IMONTH-IM)/12
           NDAYS2  =  NDAYS(1,IM,IYEAR+NXYR)+IDAY-1
         ELSE
           NDAYS2  =  0
         ENDIF

         RETURN
         END

        SUBROUTINE GETDAT_E(MESSFG,QNAME,QUNIT,ISTA,N,IYEAR,IMONTH,
     &                      IDAY,FLOW,MESCOD)
C
C*** REVISION 94.07 7/13/94
C===================================================================
C
C       PROGRAM TO GENERATE VECTORS OF THE YEAR, MONTH, DAY AND FLOW
C       FROM THE STANDARD DV OUTPUT
C
C
C       AUTHOR.........TIM COHN
C       DATE...........NOVEMBER 9, 1988
C       REVISED........APRIL 23, 1992 (TAC)
C       REVISED........AUGUST 6, 1992 (TAC & WGB)
C       REVISED........JULY 13, 1994 (TAC)
C
C===================================================================
C
C       DEFINITIONS OF VARIABLES
C
C         ISTA      I*4     THE STATION NUMBER
C         N         I*4     THE NUMBER OF OBSERVATIONS READ IN
C         IYEAR(N)  R*8     THE YEAR FOR EACH OBSERVATION
C         IMONTH(N) I*4     THE MONTH OF EACH OBSERVATION
C         IDAY(N)   I*4     THE DAY OF EACH OBSERVATION
C         FLOW(N)   I*4     THE FLOW AT EACH OBSERVATION
C
C===================================================================

        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION IYEAR(*),IMONTH(*),IDAY(*),FLOW(*),NDYCNT(12)
        INTEGER        QUNIT,MESSFG,MESCOD
        CHARACTER*100  QNAME
        CHARACTER*100  FNAME,JFMT

        DATA JFMT/'(I2,I8,6X,I4,I2,I2,8F7.0,3(/,24X,8F7.0))'/

        DATA NDYCNT/31,29,31,30,31,30,31,31,30,31,30,31/

        FNAME = QNAME
        OPEN(UNIT=QUNIT,FILE=FNAME,STATUS='OLD')

        KSUM = 0
        DO 10 KYR=2,100
          READ(QUNIT,'()',END=99)

          DO 20 KMONTH=1,12

          READ(QUNIT,JFMT,ERR=99,END=99) ITYPE,ISTA,IYR,IMO,IGRP,
     1    (FLOW(K),K=KSUM+1,KSUM+(NDYCNT(IMO)))

            NNEW =  NDAYS2(1,IMO+1,IYR)-NDAYS2(1,IMO,IYR)

          DO 30 I=KSUM+1,KSUM+NNEW
            IYEAR(I)  =  IYR
            IMONTH(I) =  IMO
            IDAY(I)   =  I-KSUM
            IF(FLOW(I) .LE. 0.D0) THEN
              MESCOD = 4
              IF (MESSFG.EQ.1) THEN
                WRITE(*,*) ' **** FLOW MISSING OR <= 0 IN Q FILE **** '
                WRITE(*,'(I5,2I3,F14.4)')
     1            IYEAR(I),IMONTH(I),IDAY(I),FLOW(I)
                WRITE(*,101) IYEAR(I)
  101           FORMAT(' **** 1.D-12 SUBSTITUED FOR FLOW ',I4,' ****')
                WRITE(*,*) ' LOAD RESULTS WRONG IF BETA(FLOW) < 0'
              END IF
C
C             N.B.  THIS SUBSTITUTION WILL LEAD TO DISASTER IF THE COEFFICIENT
C             ON LOG-FLOW IN THE REGRESSION MODEL FOR LOAD IS LESS THAN
C             ZERO....AN UNLIKELY EVENT  12/17/92
C
              FLOW(I)       =      1.D-12
            ENDIF
   30     CONTINUE

          KSUM = KSUM + NNEW
          IF(IMO .EQ. 9) GOTO 10
   20   CONTINUE

   10   CONTINUE

   99   CONTINUE

        CLOSE(QUNIT)
C
C       PAD END OF RECORD WITH DUMMY OBSERVATIONS
C
        CALL DSET(365,1.D-12,FLOW(KSUM+1),1)
        N =  KSUM
C
        RETURN
        END

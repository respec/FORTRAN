      SUBROUTINE ESTIMATE
     I                   (MESSFG,OFNAME,LIEASY,CNAME,LYRBEG,LYREND,
     I                    DLNAME,IUNIT,CUNIT,QNAME,QUNIT,PNAME,PUNIT,
     I                    CMUNIT,IPCODE,RBEGIN,REND,NREG,IREG,CMNAME,
     O                    ERRCOD,MESCOD)
C
C     + + + PURPOSE + + +
C     computational routine of estimator program
C
C     *** REVISION 95.01 1/6/95
C
C     PROGRAM TO COMPUTE THE MEAN AND THE VARIANCE OF THE SUM
C     OF PREDICTED VALUES BASED ON A LOGNORMAL REGRESSION MODEL:
C
C       Y  =  LN(L) =  X*B + E
C
C     PROGRAM COMPUTES THE EXPECTED VALUE OF THE SUM OF L AND THE
C     VARIANCE OF THE SUM OF L, WHERE L IS COMPUTED USING THE
C     FORMULA GIVEN IN COHN [1992]
C
C     AUTHOR.........TIM COHN
C     DATE...........APRIL 22, 1992 (TAC)
C     MODIFIED.......JUNE 26, 1992 (TAC & WGB)
C     MODIFIED.......JULY 6, 1992 (TAC & WGB)
C     MODIFIED.......JULY 28, 1992 (TAC & WGB)
C     MODIFIED.......NOVEMBER 23, 1992 (TAC)
C     MODIFIED.......MAY 05, 1993 (TAC & WGB)
C     MODIFIED.......AUG 12, 1993 (TAC & WGB)
C     MODIFIED.......NOV 16, 1993 (TAC)
C
C     + + + INCLUDE FILES + + +
      INCLUDE 'estinclu.inc'
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER         LIEASY,LYRBEG,LYREND,NREG,IREG(200),MESSFG,MESCOD,
     1                IUNIT,CUNIT,QUNIT,PUNIT,CMUNIT,IPCODE,ERRCOD
      REAL            RBEGIN,REND
      CHARACTER*100   OFNAME,CNAME,DLNAME,QNAME,PNAME,CMNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFG - write messages to screen flag
C     OFNAME - output file name
C     LIEASY - flag for exact variances (0), speed (1)
C     CNAME  - output file name for comparison
C     DLNAME - name of daily load output file
C     LYRBEG - first year for estimation
C     LYREND - last year for estimation
C     IUNIT  - output file unit number
C     CUNIT  - comparison file unit number
C     QNAME  - q file name
C     QUNIT  - q file unit number
C     PNAME  - pstat file name
C     PUNIT  - pstat file unit number
C     CMUNIT - cmnd file unit number
C     CMNAME - cmnd file name
C     IPCODE - parameter code
C     RBEGIN - begin year for calibration data
C     REND   - end year for calibration data
C     NREG   - number of regressors to use
C     IREG   - array of regressors to use
C     ERRCOD - error return code
C              1 - ESTIMATION AND CALIBRATION DATA SETS DIFFER
C              2 - PARAMETER CODE VALUE NOT FOUND IN LIST
C              3 - MISSING OR NEGATIVE FLOW VALUE
C              4 - YEAR MUST BE BETWEEN 1901 AND 2050
C     MESCOD - message return code
C              1 - insufficient data for model calibration
C              2 - invalid year -- no flow data available
C              3 - dv flow unavailable for point
C              4 - flow missing or <= 0 in q file,
C                  1.d-12 substituted for flow
C              5 - instantaneous flow not available
C              6 - zero or negative value found
C              7 - unrecognized remark code found,
C                  recoded as missing value
C
C     + + + LOCAL VARIABLE DEFINITIONS + + +
C     NP        I*4     NUMBER OF PARAMETERS (NP=1 ==> ONLY
C                         A CONSTANT IN THE MODEL)
C     BETA(NP)  R*8     THE COLUMN VECTOR OF ESTIMATED PARAMETERS
C                         B(NP+1) = STND. DEV. RESIDS.
C     NC        I*4     NUMBER VARIABLES IN X_C USED TO CALIBRATE
C                         MODEL
C     NDC       I*4     ROW DIMENSION OF X_C AS DIMENSIONED
C     X_C(*,NP) R*8     EXPLANATORY VARIABLES FOR CALIBRATION
C                         X_C(1) = 1 ==> A CONSTANT IN MODEL
C     NE        I*4     NUMBER OBS. IN X_E
C     X_E(*,NP) R*8     REGRESSORS USED TO ESTIMATE
C
C     + + + DATA INITIALIZATIONS + + +
      DATA VERSION/95.01/
C
C     + + + END SPECIFICATIONS + + +
C
      ERRCOD = 0
      MESCOD = 0
C
C     GETTING STARTED:
C       1.   OPEN OUTPUT FILE
C       2.   DEFINE VARIABLES, STATION, ETC.
C       3.   CHOOSE TIME HORIZON
C
      FNAME = OFNAME
      OPEN(UNIT=IUNIT,FILE=FNAME,STATUS='UNKNOWN')
      WRITE(IUNIT,1001) VERSION
      WRITE(IUNIT,'('' OUTPUT FILE:'',A50)') FNAME
C
      IEASY = LIEASY
      IF(IEASY .EQ. 1) WRITE(IUNIT,'('' *APPROXIMATE VARIANCES*'')')
C
      CALL GETDAT_C (MESSFG,IUNIT,QNAME,QUNIT,PNAME,PUNIT,CMUNIT,
     I               IPCODE,RBEGIN,REND,NREG,IREG,CMNAME,
     O               ERRCOD,MESCOD)
C
      IF(ISTAT_E .NE. ISTAT_C) THEN
C       estimation and calibration data sets differ
        ERRCOD = 1
      ENDIF
C
      IF (ERRCOD .EQ. 0) THEN
        IF(NC-NCENS .LT. 3*NP) THEN
          MESCOD = 1
          IF (MESSFG.EQ.1) THEN
            WRITE(*,*) ' ***** INSUFF. DATA FOR MODEL CALIBRATION ****'
          END IF
        ENDIF
C
        WRITE(IUNIT,1002)
 1002   FORMAT(//,T53,'CONCENTRATION MODEL',T94,'LOAD MODEL',/,
     1     ' NUMBER',T11,'NAME',T36,'CENTER',T51,'COEFF.',T61,'S.D.',
     2      T71,'T',T79,'P',T91,'COEFF.',T101,'S.D.',T111,'T',T119,'P')
C
C====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
C       PERFORM REGRESSION
C       ADDITIONAL EFFORT TO SATISFY BOB SUMMERS
C
        CALL REGRESS(NC,NP,X_C,NSIZE,Y_C,YT_C,BETA,STDDEV,P_V,RES,PRED)
            S  =  SQRT(BETA(NP+1))
C
        CALL REGRESS(NC,NP,X_C,NSIZE,C_Y_C,C_YT_C,Bz,STDz,Pz,Rz,PRz)
            Sz =  SQRT(BETA(NP+1))
C
        DO 10 I=1,NP
          WRITE(IUNIT,1003) I,NAME(I),CENTER(I),
     1      Bz(I), STDz(I), Bz(I)/STDz(I), Pz(I),
     2      BETA(I),STDDEV(I),   BETA(I)/STDDEV(I),   P_V(I)
 1003   FORMAT(I4,4X,A20,1X,F12.3,2X,2(5X,2F9.4,F6.2,2X,F8.6,1X))
   10   CONTINUE
C
        WRITE(IUNIT,'(/,5X,''S'',T56,F12.5,T86,F12.5)') S,Sz
        CALL R_SQUARE(NC,NP,PRED,LOGQ_C,S,R2L,R2C)
        WRITE(IUNIT,'(/,5X,''R**2 (%)'',T58,F6.1,T88,F6.1)')
     1          100.*R2C,100.*R2L
C
        WRITE(IUNIT,'(5X,''N    '',I7)') NC
        WRITE(IUNIT,'(5X,''M    '',I7)') NC-NP
        WRITE(IUNIT,'(5X,''NCENS'',I7)') NCENS
        IF(NC-NCENS .LT. 3*NP) THEN
          WRITE(IUNIT,'('' ***** INSUFFICIENT DATA'',
     1                  '' FOR MODEL CALIBRATION ****'')')
          WRITE(IUNIT,'('' ***** MODEL VALIDITY SHOULD BE CHECKED'',
     1                  '' ****'')')
        ENDIF
C
        SCORR  = CORR(NC-1,RES(1),RES(2))
        WRITE(IUNIT,'(5X,''SER. CORR. OF RES.'',F8.5,)') SCORR
        IF(SCORR .GT. 0.5D0) THEN
          WRITE(IUNIT,'('' ***** SERIAL CORRELATION SUGGESTS'',
     1                  '' MIS-SPECIFIED MODEL ****'')')
          WRITE(IUNIT,'('' ***** MODEL VALIDITY SHOULD BE CHECKED'',
     1                  '' ****'')')
        ENDIF
C
        WRITE(IUNIT,'(10X,10(1X,A9))') (NAME(K),K=1,NP-1)
        DO 12 I=2,NP
          WRITE(IUNIT,'(1X,A9,8F10.4)')
     1    NAME(I),(CORR(NC,X_C(1,I),X_C(1,J)),J=1,I-1)
   12   CONTINUE
C
        CALL GRAPHRES
     1       (IUNIT,NC,IM_C,LOGQ_C,D_TIME_C,Y_C,YT_C,S,RES,PRED)
C
        PCT_VAR   =  EXP(BETA(NP+1))-1.D0
C
C===================================================================
C
C       CREATE A TABLE OF ACTUAL VS. PREDICTED CONCENTRATIONS AND LOADS
C
        FNAME = CNAME
        IF(FNAME. NE. '') THEN
          OPEN(UNIT=CUNIT,FILE=FNAME,STATUS='UNKNOWN')
          DO 31 I=1,NC
            IF (ERRCOD.EQ.0) THEN
              IF(DATA_C(I) .LE. 0.D0) GOTO 31
              CALL LOGLOAD (MESSFG,ID_C(I),IM_C(I),IY_C(I),
     1                      ID_C(I),IM_C(I),IY_C(I), LOAD,SELOAD,
     O                      ERRCOD,MESCOD)
            END IF
            IF (ERRCOD.EQ.0) THEN
              FL2447  =  MAX(1.D-10,(2.447*FLOW_E(IOFF+1)))
              DCC  =  LOAD/FL2447
              SE_PREP=  SQRT(SELOAD**2+PCT_VAR*SUMSQ(MVUE,NE)/NE**2)
              SEDCC=  SE_PREP/FL2447
              WRITE(CUNIT,'(I9,I6,2I3,I5,2F12.1,4X,3F8.3,5X,3F16.3,
     1              10X,20F10.6)')
     1        ISTAT_C,IY_C(I),IM_C(I),ID_C(I),IT_C(I),FLOW_C(I),
     2        FLOW_E(IOFF+1),DATA_C(I),DCC,SEDCC,
     3        2.447*DATA_C(I)*FLOW_E(IOFF+1),LOAD,SE_PREP
     4        ,(X_C(I,J),J=1,NP),Y_C(I),YT_C(I),RES(I),PRED(I)
            END IF
   31     CONTINUE
          CLOSE(CUNIT)
        ENDIF
C
C===================================================================
C
C       ESTIMATION WITH X_E FOR EXPLANATORY VARIABLES
C
        IYRBEG = LYRBEG
        IYREND = LYREND
C
C===================================================================
C
C       ARE DAILY LOADS NEEDED?
C
        IF (ERRCOD.EQ.0) THEN
          FNAME = DLNAME
          IF(FNAME. NE. '') THEN
            OPEN(UNIT=CUNIT,FILE=FNAME,STATUS='UNKNOWN')
            DO 55 IYR=IYRBEG,IYREND
             DO 55 IMO=1,12
              DO 55 IDA=1,NDAYS2(1,IMO+1,IYR)-NDAYS2(1,IMO,IYR)
               IF (ERRCOD.EQ.0) THEN
                 CALL LOGLOAD(MESSFG,IDA,IMO,IYR,IDA,IMO,IYR, LOAD,
     &                        SELOAD,
     O                        ERRCOD,MESCOD)
               END IF
               IF (ERRCOD.EQ.0) THEN
             SE_PRED  =  SQRT(SELOAD**2+PCT_VAR*SUMSQ(MVUE,NE)/NE**2)
             WRITE(CUNIT,'(3I5,3F17.4)') IYR,IMO,IDA,LOAD,SELOAD,SE_PRED
               END IF
   55     CONTINUE
            CLOSE(CUNIT)
          ENDIF
C
C===================================================================
C
          IF (ERRCOD.EQ.0) THEN
            WRITE(IUNIT,1005) NAME(0)
C
            DO 50 IYR=IYRBEG,IYREND
              WRITE(IUNIT,'(I6,'' CALENDAR YEAR'')') IYR
C
              DO 60 IMO=1,12
                IF (ERRCOD.EQ.0) THEN
                  CALL LOGLOAD(MESSFG,1,IMO,IYR, 0,IMO+1,IYR, LOAD,
     &                         SELOAD,
     O                         ERRCOD,MESCOD)
                END IF
                IF (ERRCOD.EQ.0) THEN
                SE_PRED  =  SQRT(SELOAD**2+PCT_VAR*SUMSQ(MVUE,NE)/NE**2)
                WRITE(IUNIT,'(I13,3F17.4)') IMO,LOAD,SELOAD,SE_PRED
                END IF
   60         CONTINUE
C
              IF (ERRCOD.EQ.0) THEN
                CALL LOGLOAD (MESSFG,1,1,IYR, 31,12,IYR, LOAD,SELOAD,
     O                        ERRCOD,MESCOD)
                IF (ERRCOD.EQ.0) THEN
                SE_PRED  =  SQRT(SELOAD**2+PCT_VAR*SUMSQ(MVUE,NE)/NE**2)
                  WRITE(IUNIT,'('' CY '',I6,3X,3F17.4)')
     1              IYR,LOAD,SELOAD,SE_PRED
                  CALL LOGLOAD (MESSFG,1,10,IYR-1,30,9,IYR, LOAD,SELOAD,
     O                          ERRCOD,MESCOD)
                  IF (ERRCOD.EQ.0) THEN
                SE_PRED  =  SQRT(SELOAD**2+PCT_VAR*SUMSQ(MVUE,NE)/NE**2)
                    WRITE(IUNIT,'('' WY '',I6,3X,3F17.4,/)')
     1                IYR,LOAD,SELOAD,SE_PRED
                  END IF
                END IF
              END IF
C
   50       CONTINUE
          END IF
        END IF
      END IF
C
      CLOSE (PUNIT)
      CLOSE (IUNIT)
C
 1001 FORMAT('1',//,T22,'MODEL FOR ESTIMATING CONSTITUENT LOADS',
     1        /,T32,'TIM COHN VERSION ',F5.2,//)
 1005 FORMAT('1',//,T30,'COMPUTED LOADS [KG/DAY] OR [G/DAY]',
     1  //,T7,'YEAR MO.',T25,'LOAD:',A9,5X,'S.E.',5X,'S.E. PRED.',/)
C====*===|====-====|====-====|====-====|====-====|====-====|====-====|==////////
C
      RETURN
      END

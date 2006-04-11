C
C
C
      SUBROUTINE   PRWDST
     I                    (MESSFL,WDMSFL)
C
C     + + + PURPOSE + + +
C     process WDMSFL datasets
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdsn.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SCLU,SGRP,RESP
      CHARACTER*8   PTHNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL    QRESP, PRWMSL, PRWMST, PRWMSE, PRWMLD
      EXTERNAL    PRWMRE, PRWMLA, PRNTXT
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 26
      RESP= 0
      PTHNAM = 'D       '
C
 10   CONTINUE
C       Datasets menu
        SGRP = 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
        GO TO (100,200,300,400,500,600,700), RESP
C
 100    CONTINUE
C         process WDMSFL dataset attributes
          CALL PRWMSL (MESSFL,WDMSFL,DSNBMX,
     M                 DSNBUF,DSNCNT)
          GO TO 900
C
 200    CONTINUE
C         process WDMSFL Timeseries data values
          CALL PRWMST (MESSFL,WDMSFL,DSNBMX,
     M                 DSNBUF,DSNCNT)
          GO TO 900
C
 300    CONTINUE
C         select WDMSFL datasets
          CALL PRWMSE (MESSFL,WDMSFL,DSNBMX,PTHNAM,
     M                 DSNBUF,DSNCNT)
          GO TO 900
C
 400    CONTINUE
C         delete old datasets
          IF (DSNCNT.GT.0) THEN
C           data sets in buffer to delete
            CALL PRWMLD (MESSFL,WDMSFL,SCLU,DSNBMX,DSNCNT,DSNBUF)
          ELSE
C           no data sets to delete
            SGRP= 2
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 900
C
 500    CONTINUE
C         renumber WDMSFL datasets
          CALL PRWMRE (MESSFL,WDMSFL,SCLU,DSNBMX,DSNCNT,
     M                 DSNBUF)
          GO TO 900
C
 600    CONTINUE
C         add a new dataset
          CALL PRWMLA (MESSFL,WDMSFL)
          GO TO 900
C
 700    CONTINUE
C         return to Opening screen
          GO TO 900
C
 900    CONTINUE
      IF (RESP.NE.7) GO TO 10
C
      RETURN
      END
C
C
C
CTMP      SUBROUTINE   PRWMSD
CTMP     I                    (MESSFL,WDMSFL,SCLU,DSNBMX,
CTMP     M                     DSNBUF,DSNCNT)
C
C     + + + PURPOSE + + +
C     processes WDMSFL data values
C
C     + + + DUMMY ARGUMENTS + + +
CTMP      INTEGER   MESSFL,WDMSFL,SCLU,DSNBMX,DSNCNT
CTMP      INTEGER   DSNBUF(DSNBMX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - cluster containing messages for this routine
C     DSNBMX - maximum size of DSN array
C     DSNBUF - array of dataset numbers
C     DSNCNT - count of datasets to work with
C
C     + + + LOCAL VARIABLES + + +
CTMP      INTEGER   SGRP, DSTYP
C
C     + + + EXTERNALS + + +
CTMP      EXTERNAL  QRESP, PRWMST
CTMP      EXTERNAL  PRNTXT
CPRH  EXTERNAL  PRWMSX
C
C     + + + END SPECIFICATIONS + + +
C
CTMP 10   CONTINUE
CTMPC       what type of data to work with?
CTMP        SGRP = 10
CTMP        DSTYP= 1
CTMP        CALL QRESP (MESSFL,SCLU,SGRP,DSTYP)
C
CTMP        GO TO (100,200,300,400), DSTYP
C
CTMP 100    CONTINUE
CTMPC         timeseries
CTMP          CALL PRWMST (MESSFL,WDMSFL)
CTMP          GO TO 900
C
CTMP 200    CONTINUE
CTMPC         table
CPRH          CALL PRWMSX (MESSFL,WDMSFL,DSNBMX,
CPRH     M                 DSNBUF,DSNCNT)
CTMPC         *** tables not currently implemented ***
CTMP          SGRP= 11
CTMP          CALL PRNTXT (MESSFL,SCLU,SGRP)
CTMP          GO TO 900
C
CTMP 300    CONTINUE
CTMPC         dlg
CTMPC         CALL PRWMDL (MESSFL,WDMSFL)
CTMPC         *** dlg not currently implemented ***
CTMP          SGRP= 11
CTMP          CALL PRNTXT (MESSFL,SCLU,SGRP)
CTMP          GO TO 900
C
CTMP 400    CONTINUE
CTMPC         done
CTMP          GO TO 900
C
CTMP 900    CONTINUE
C
CTMP      IF (DSTYP.NE.4) GO TO 10
C
CTMP      RETURN
CTMP      END
C
C
C
      SUBROUTINE   PRWMLD
     I                    (MESSFL,WDMSFL,SCLU,DSNBMX,DSNCNT,DSN)
C
C     + + + PURPOSE + + +
C     deletes datasets from the WDMSFL after
C     asking the user to verify the dataset to delete
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,SCLU,DSNBMX,DSNCNT
      INTEGER   DSN(DSNBMX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - cluster containing messages for this routine
C     DSNBMX - maximum size of DSN array
C     DSNCNT - count of datasets to delete
C     DSN    - array of DSNCNT dataset numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SGRP,I,J,ANS,CDSN,I0,I1,I12,I20,RETCOD,RETC,
     $          DREC,NUMC,CODES(7),GPFLG,DATE(12),IRET,ILEN
      CHARACTER*1 TBUFF(78)
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  PMXTXI, QRESP, PRNTXT, WDDSDL, ZMNSST, ZBLDWR
      EXTERNAL  LENSTR, ZSTCMA, ZGTRET, WDDSCK, WDSALV, WTFNDT
C
C     + + + DATA INITIALIZATIONS + + +
C                  numc tstype staid istaid tcode tsstep descrp stanam
      DATA   NUMC / 7 /,
     $       CODES    /     1,    2,    51,   17,    33,    10,    45 /
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I12 = 12
      I20 = 20
C
      CDSN= 1
C     make prev available
      I= 16
      CALL ZSTCMA(I,I1)
 100  CONTINUE
C       get info on dsn to be deleted
        CALL WDDSCK (WDMSFL, DSN(CDSN),
     O               DREC, RETC)
        IF (RETC .EQ. 0) THEN
C         list selected attributes of dsn
          SGRP = 40
          CALL PMXTXI (MESSFL,SCLU,SGRP,I20,I1,-I1,I1,DSN(CDSN))
C         list basic set of attributes
          DO 150 I= 1,NUMC
C           output a line for each attribute found on data set
            IF (I.NE.3 .OR. RETCOD.NE.0) THEN
C             keeps both STAID and ISTAID from being output,
C             thus saving extra line needed to fit menu in data window
              CALL WDSALV (MESSFL,WDMSFL,DREC,CODES(I),
     I                     TBUFF,RETCOD)
              IF (RETCOD.EQ.0) THEN
C               output buffer showing attribute and value
                J   = 78
                ILEN= LENSTR(J,TBUFF)
                CALL ZBLDWR (ILEN,TBUFF,I0,-I1,J)
              END IF
            END IF
 150      CONTINUE
C         period of record
          GPFLG = 1
          CALL WTFNDT (WDMSFL, DSN(CDSN), GPFLG, DREC,
     O                 DATE(1), DATE(7), RETCOD)
          IF (RETCOD .EQ. 0) THEN
C           data available from __ to __
            SGRP = 41
            CALL PMXTXI (MESSFL,SCLU,SGRP,I20,-I1,-I1,I12,DATE)
          END IF
        ELSE
C         no information available for dsn __
          SGRP = 42
          CALL PMXTXI (MESSFL,SCLU,SGRP,I20,I1,-I1,I1,DSN(CDSN))
        END IF
C       save screen
        CALL ZMNSST
C       ok? (1-NO,2-YES)
        ANS = 1
        SGRP= 43
        CALL QRESP (MESSFL,SCLU,SGRP,ANS)
C       get user exit command value
        CALL ZGTRET(IRET)
        IF (IRET.EQ.1) THEN
C         user wants to continue with deleting
          IF (ANS.EQ.2) THEN
C           ok to delete
            CALL WDDSDL (WDMSFL,DSN(CDSN),
     O                   RETCOD)
            IF (RETCOD.EQ.0) THEN
C             report that we're done
              SGRP= 44
              CALL PRNTXT (MESSFL,SCLU,SGRP)
C             cant have nonexistant datasets in the buffer
              I= CDSN
              IF (I .LT. DSNCNT) THEN
 200            CONTINUE
                  DSN(I)= DSN(I+1)
                  I     = I+ 1
                IF (I .LT. DSNCNT) GO TO 200
              END IF
              DSN(I)= 0
              DSNCNT= DSNCNT- 1
            ELSE
C             dataset not found to delete
              SGRP= 45
              CALL PRNTXT (MESSFL,SCLU,SGRP)
              CDSN= CDSN+ 1
            END IF
          ELSE
C           don't delete this one
            SGRP= 45
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            CDSN= CDSN+ 1
          END IF
        END IF
      IF (IRET.EQ.1 .AND. DSN(CDSN).GT.0) GO TO 100
C
C     turn off prev
      I= 16
      CALL ZSTCMA(I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMRE
     I                   (MESSFL,WDMSFL,SCLU,DSNBMX,DSNCNT,
     M                    DSNBUF)
C
C     + + + PURPOSE + + +
C     routine to renumber a dataset
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,SCLU,DSNBMX,DSNCNT
      INTEGER   DSNBUF(DSNBMX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - cluster containing messages for this routine
C     DSNBMX - maximum size of DSN array
C     DSNCNT - count of datasets to work with
C     DSNBUF - array of dataset numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IVAL(5),NREC,OREC,I,I0,I1,I2,SGRP,INUM,CVAL(3),RETC,
     $            ODSN,ODSSTR,ODSEND,ODSINC,NDSN,NDSINC
      REAL        RVAL
      CHARACTER*1 TBUFF(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDDSCK, PRNTXI, QRESPM, WDDSRN, PMXTXI, ZSTCMA, ZGTRET
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
C     make prev available
      I = 4
      CALL ZSTCMA(I,I1)
C     what dataset do you want to renumber and whats new number?
      IVAL(1)= -999
      IVAL(2)= -999
      IVAL(3)= 1
      IVAL(4)= -999
      IVAL(5)= 1
      INUM   = 5
      SGRP   = 20
      CALL QRESPM(MESSFL,SCLU,SGRP,INUM,I1,I1,
     M            IVAL,RVAL,CVAL,TBUFF)
C     return code
      CALL ZGTRET(I)
      IF (I .EQ. 1) THEN
C       user wants to continue
        ODSSTR= IVAL(1)
        IF (IVAL(2).GT.ODSSTR) THEN
C         ending data-set number entered
          ODSEND= IVAL(2)
        ELSE
C         set ending data set to starting data set
          ODSEND= ODSSTR
        END IF
        ODSINC= IVAL(3)
        NDSN  = IVAL(4)
        NDSINC= IVAL(5)
        DO 100 ODSN= ODSSTR,ODSEND,ODSINC
C         loop through range of old data-set numbers
          CALL WDDSCK (WDMSFL,ODSN,OREC,RETC)
          IF (RETC.NE.0) THEN
C           dataset doesn't exist
            SGRP= 21
            CALL PRNTXI (MESSFL,SCLU,SGRP,ODSN)
          ELSE
            CALL WDDSCK (WDMSFL,NDSN,NREC,RETC)
            IF (RETC.EQ.0) THEN
C             dataset number already being used
              SGRP= 22
              CALL PRNTXI (MESSFL,SCLU,SGRP,NDSN)
            ELSE
C             change DSN on dataset label
              CALL WDDSRN (WDMSFL,ODSN,NDSN,RETC)
              SGRP= 23
              IVAL(1)= ODSN
              IVAL(2)= NDSN
              CALL PMXTXI (MESSFL,SCLU,SGRP,I2,I0,I0,I2,IVAL)
C
              IF (DSNCNT.GT.0) THEN
C               check DSN buffer for renumbered datasets
                DO 10 I= 1,DSNCNT
                  IF (ODSN.EQ.DSNBUF(I)) THEN
C                   old dataset number was in buffer, change DSNBUF
                    DSNBUF(I)= NDSN
                  END IF
 10             CONTINUE
              END IF
            END IF
          END IF
C         increment new data-set number
          NDSN= NDSN+ NDSINC
 100    CONTINUE
      END IF
C     turn off prev
      I= 4
      CALL ZSTCMA(I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ANVRSN
C
C     + + + PURPOSE + + +
C     Dummy routine to include unix what version information for the
C     ann library.
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*64  VERSN
C
C     + + + END SPECIFICATIONS + + +
C
      INCLUDE 'fversn.inc'
C
      RETURN
      END

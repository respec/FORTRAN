C
C
C
      SUBROUTINE   PRWMST
     I                    (MESSFL,WDMSFL,MAXDSN,
     M                     DSN,DSNCNT)
C
C     + + + PURPOSE + + +
C     processes WDMSFL timeseries data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,MAXDSN,DSNCNT
      INTEGER   DSN(MAXDSN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     MAXDSN - maximum size of DSN array
C     DSN    - array of dataset numbers
C     DSNCNT - count of datasets to work with
C
C     + + + PARAMETERS + + +
      INTEGER    MXGEN
      PARAMETER (MXGEN=1)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SCLU,SGRP,RESP,EDITFG,GENCNT,GENBUF(MXGEN)
      CHARACTER*8 PTHNAM(1)
C     INTEGER     DSN,DELT,DATES(6),NVAL,DTRAN,QUALFG,TUNITS,RETCOD,
C    >            DATMP1(6),DATMP2(6),I,J,K
C     REAL        RVAL(100)
C     CHARACTER*1 ANS
C     CHARACTER*5 CPW
C
C     + + + INTRINSICS + + +
C     INTRINSIC  FLOAT
C
C     + + + EXTERNALS + + +
      EXTERNAL   PRWMTA, PRWMTC, PRWMTD, TSLIST, PRWMTM, QRESP
      EXTERNAL   OGENER, PRWMSE, PRNTXT, PRWMTT
CKMF  EXTERNAL   WDTGET, WDTPUT, TIMADD
C
CTMPC     + + + INPUT FORMATS + + +
CTMP 1000 FORMAT (A1)
CTMP 1001 FORMAT (A5)
CTMPC
CTMPC     + + + OUTPUT FORMATS + + +
CTMP 2000 FORMAT (I5,6G12.4)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 39
      EDITFG = 0
      PTHNAM(1) = 'DT      '
C
 10   CONTINUE
C       option-1select,2list,3table,4modify,5add,6copy,7delete,
C              8generate,9return
        SGRP = 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
        IF ((RESP.EQ.2 .OR. RESP.EQ.3) .AND. DSNCNT.EQ.0) THEN
C         can't do option, no data sets selected
          SGRP = RESP
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        ELSE
C         ok to do option
          GO TO (100,200,300,400,500,600,700,800,900), RESP
C
 100      CONTINUE
C           select data sets
            CALL PRWMSE (MESSFL,WDMSFL,MAXDSN,PTHNAM,
     M                   DSN,DSNCNT)
            GO TO 999
C
 200      CONTINUE
C           list data or screen data for outliers
            CALL TSLIST (MESSFL,WDMSFL,PTHNAM,DSNCNT,DSN,EDITFG)
            GO TO 999
C
 300      CONTINUE
C           table data from dataset
            CALL PRWMTT (MESSFL,WDMSFL,PTHNAM,DSNCNT,DSN)
            GO TO 999
C
 400      CONTINUE
C           mod timeseries data in file
            CALL PRWMTM (MESSFL,SCLU,WDMSFL)
            GO TO 999
C
 500      CONTINUE
C           add timeseries data in file
            CALL PRWMTA (MESSFL,SCLU,WDMSFL)
            GO TO 999
C
 600      CONTINUE
C           copy a timeseries dataset
            CALL PRWMTC (MESSFL,SCLU,WDMSFL)
            GO TO 999
C
 700      CONTINUE
C           delete part of data in timeseries dataset
            CALL PRWMTD (MESSFL,SCLU,WDMSFL)
            GO TO 999
C
CTMP 700      CONTINUE
CTMP            debug mode
CTMP            write(*,*) 'Enter password'
CTMP            read(*,1001) CPW
CTMP            IF (CPW .EQ. 'DEBUG') THEN
CTMP             prevent users from screwing up WDM file
CTMP              WRITE (*,*) 'ENTER DSN,DELT,NVAL,DTRAN,QUALFG,TUNITS:'
CTMP              READ  (*,*)        DSN,DELT,NVAL,DTRAN,QUALFG,TUNITS
CTMP              WRITE (*,*) 'ENTER START DATE:'
CTMP              READ  (*,*)        DATES
CTMP              WRITE (*,*) 'WRITE VALUES TO WDMSFL?'
CTMP              READ  (*,1000) ANS
CTMP              IF (NVAL.GT.100) THEN
CTMP                WRITE (*,*) 'NVAL RESET TO 100'
CTMP                NVAL= 100
CTMP              END IF
CTMP              IF (ANS.EQ.'Y') THEN
CTMP                DO 710 K= 1,6
CTMP                  DATMP1(K)= DATES(K)
CTMP 710            CONTINUE
CTMP                J= 1
CTMP               fill in data array for put
CTMP                DO 730 I= 1,NVAL
CTMP                  CALL TIMADD (DATMP1,TUNITS,DELT,J,
CTMP     O                       DATMP2)
CTMP                  RVAL(I)= DATMP2(2)+ FLOAT(DATMP2(3))/100.+
CTMP     1                   FLOAT(DATMP2(4))/10000.
CTMP                  DO 720 K= 1,6
CTMP                    DATMP1(K)= DATMP2(K)
CTMP 720              CONTINUE
CTMP 730            CONTINUE
CTMP                WRITE (*,*) 'LIST VALUES TO WRITE?'
CTMP                READ  (*,1000) ANS
CTMP                IF (ANS.EQ.'Y') THEN
CTMP                  DO 740 I=1,NVAL,6
CTMP                    K= I+ 5
CTMP                    IF (K.GT.NVAL) K= NVAL
CTMP                    WRITE (*,2000) I,(RVAL(J),J=I,K)
CTMP 740              CONTINUE
CTMP                END IF
CTMP                K= 0
CTMP                CALL WDTPUT (WDMSFL,DSN,DELT,DATES,NVAL,K,QUALFG,TUNITS,
CTMP     1                     RVAL,
CTMP     O                     RETCOD)
CTMP                WRITE (*,*) 'PUT RETCOD:',RETCOD
CTMP                END IF
CTMP
CTMP                CALL WDTGET (WDMSFL,DSN,DELT,DATES,NVAL,DTRAN,QUALFG,
CTMP     I                   TUNITS,
CTMP     O                   RVAL,RETCOD)
CTMP                WRITE (*,*) 'GET RETCOD:',RETCOD
CTMP                WRITE (*,*) 'LIST VALUES READ?'
CTMP                READ  (*,1000) ANS
CTMP                IF (ANS.EQ.'Y') THEN
CTMP                DO 750 I=1,NVAL,6
CTMP                  K= I+ 5
CTMP                  IF (K.GT.NVAL) K= NVAL
CTMP                  WRITE (*,2000) I,(RVAL(J),J=I,K)
CTMP 750            CONTINUE
CTMP              END IF
CTMP            END IF
CTMP            GO TO 999
C
 800      CONTINUE
C           generate a time series
            CALL OGENER (MESSFL,WDMSFL,MXGEN,PTHNAM,
     O                   GENCNT,GENBUF)
            GO TO 999
C
 900      CONTINUE
C           return to opening screen
            GO TO 999
C
 999      CONTINUE
        END IF
      IF (RESP.NE.9) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMTM
     I                    (MESSFL,SCLU,WDMSFL)
C
C     + + + PURPOSE + + +
C     modifys existing WDM timeseries data sets.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number of WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I0,I1,I6,I64,I80,SGRP,DSN,RETCOD,IRET,
     1            RIND,TGROUP,TGRPST,TGRNUM,PDAT,PDATV,GPOSST,
     2            DATSTR(12),DATEX(6),GPSDAT(6),TBSDAT(6),DATMAX(6),
     3            CURREC,CURBKS,CURPOS,CURTST,CURTUN,CURCMP,CURQUA,
     4            CURDAT(6),BLSDAT(6),BLEDAT(6),OLDQUA,TSPTAD,BADJFG,
     5            ADDAFG,QUPD,VORQ,DSFRC,DIND,DATMIN(6),
     6            LEN10,JUSTR,KNT,KNTMX,LEN37,OPEN,DELFG,OUTFL,
     7            LENTX,DSTYP,GRCNT,SDATE(6),IVAL(1)
C     INTEGER     AGAIN
      INTEGER*4   GRPOFF,NVAL,CURNOV,CURCNT
      REAL        TSFILL,TOLR,CURVAL,PREVAL,OLDVAL,RVAL(1)
      CHARACTER*1 TBUFF(80),BLNK(1),FLNAM1(64)
      CHARACTER*22 WNDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
      INTEGER*4   WBCWCL
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    QSCSTI, DATLST, GETTXT, PRTERR, QRESPI, WDMODT
      EXTERNAL    TIMADD, TIMDIF, WBCWCL, COPYI, WDRCGO, WDRCUP
      EXTERNAL    WDSCHA, WTDSPM, WTSGRP, WTSKVX, ZIPC, ZIPI, TIMCVT
      EXTERNAL    INTCHR, PRTSTR, QFCLOS, QFOPFN, ZBLDWR, ZMNSST
      EXTERNAL    ZSTCMA, ZGTRET, WTFNDT, PMXCNW, Q1INIT, Q1EDIT
      EXTERNAL    QSETI, QGETI, QSETR, QGETR, QSTCTF, QGTCTF, TSBCLR
C     EXTERNAL    QRESP
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  KNTMX, LEN10, LEN37, JUSTR
     *     /   50,    10,    37,     0/
      DATA  BLNK, I6, BADJFG, ADDAFG
     *     / ' ',  6,   0,      0 /
      DATA DATMIN / 0,  1,  1,  0,  0,  0 /
      DATA DATMAX / 0, 12, 31, 24, 59, 59 /
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I64 = 64
      I80 = 80
C
      DSN = -999
C     initialize data to undef
      I= 12
      CALL ZIPI (I,DSN,DATSTR)
      CALL ZIPC (I80,BLNK,TBUFF)
      CALL ZIPC (I64,BLNK,FLNAM1)
      OPEN = 0
C
      RETCOD = 0
CPRH 10   CONTINUE
CPRHC       what modify option 1-Values,2-Fill,3-Return
        VORQ = 1
CPRH        SGRP = 10
CPRH        CALL QRESP (MESSFL,SCLU,SGRP,VORQ)
        IF (VORQ.LE.2) THEN
C         get text for modify message
          SGRP = 10+ VORQ
          LENTX= 80
          CALL GETTXT (MESSFL,SCLU,SGRP,LENTX,TBUFF)
        END IF
        IF (VORQ .EQ. 2) THEN
C         set counter to output to summary file
          KNT = KNTMX
        END IF
        IF (VORQ .LE. 2) THEN
C         user wants to modify, allow previous and interrupt
          I= 4
          CALL ZSTCMA (I,I1)
          I= 16
          CALL ZSTCMA (I,I1)
 100      CONTINUE
C           back here on previous from date screen, get data set number
            SGRP= 14
            CALL QRESPI (MESSFL,SCLU,SGRP,DSN)
C           get user exit command value
            CALL ZGTRET (IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue, check validity of data set
              I= 2
              DSTYP= 1
              CALL WDSCHA (WDMSFL,DSN,DSTYP,I,
     O                     DSFRC,GRCNT,RETCOD)
              IF (RETCOD .EQ. 0) THEN
C               good data set, check for dates
                CALL WTFNDT (WDMSFL,DSN,I1,DSFRC,
     O                       DATSTR,DATSTR(7),RETCOD)
                CALL COPYI (I6,DATSTR,SDATE)
                IF (VORQ.EQ.1) THEN
C                 get start date each time user modifies
                  QUPD = 0
                ELSE
C                 recover all data after specified date, only need one date
                  QUPD = 1
                END IF
              END IF
              IF (RETCOD.NE.0) THEN
C               problem with data set
                WNDNAM= 'Modify (DTM) Problem'
                CALL PRTERR (MESSFL,WNDNAM,RETCOD)
                DSN= -999
C               get user exit command value
                CALL ZGTRET (IRET)
              END IF
            END IF
            IF (RETCOD.EQ.0 .AND. VORQ.NE.3 .AND. IRET.EQ.1) THEN
C             all ok, user wants to continue modifying
 200          CONTINUE
C               back here on previous from data modify screen
                IF (QUPD.NE.2) THEN
C                 specify date to modify, init screen
                  IF (VORQ.EQ.2 .AND. OPEN.EQ.0) THEN
C                   need output file for summary and start date
                    SGRP  = 15
                  ELSE
C                   just prompt for start date
                    SGRP  = 16
                  END IF
                  CALL Q1INIT (MESSFL,SCLU,SGRP)
C                 set min/max/default values on the fly
                  DATMIN(1)= SDATE(1)
                  DATMAX(1)= DATSTR(7)
                  CALL QSCSTI (I6,I1,DATMIN,DATMAX,DATSTR)
                  IF (VORQ.EQ.2 .AND. OPEN.EQ.0) THEN
C                   set current value for file name
                    CALL QSTCTF (I1,I64,FLNAM1)
                  END IF
C                 set current values for data fields
                  CALL QSETI (I6,DATSTR)
C                 perform editing of date to begin deleting
                  CALL Q1EDIT (IRET)
                  IF (IRET.EQ.1) THEN
C                   user wants to continue, check validity of data set
                    CALL QGETI (I6,DATSTR)
                    IF (VORQ.EQ.2 .AND. OPEN.EQ.0) THEN
C                     try to open specified file
                      CALL QGTCTF (I1,I64,FLNAM1)
                      SGRP= 13
                      CALL QFOPFN (MESSFL,SCLU,SGRP,FLNAM1,I0,
     O                             OUTFL,RETCOD)
C                     dont prompt for file name again
                      OPEN= 1
                    END IF
                    IF (QUPD.EQ.1) THEN
C                     only ask date first time through
                      QUPD= 2
                    END IF
                  ELSE IF (IRET.EQ.2) THEN
C                   user wants previous screen, back to data set number
                    IRET= -1
                  END IF
                END IF
                IF (IRET.EQ.1) THEN
C                 user continuing, get directory record
                  DIND = WDRCGO(WDMSFL,DSFRC)
C                 get units for data group pointer and start of data
                  CALL WTDSPM (WDMSFL,WIBUFF(1,DIND),WRBUFF(1,DIND),
     O                         TSFILL,TGROUP,TOLR,TBSDAT,TSPTAD)
C                 calculate pointer to first group pointer
                  PDAT  = WIBUFF(11,DIND)
                  TGRPST= PDAT+2
C                 calculate max number of group pointers
                  PDATV = WIBUFF(12,DIND)
                  TGRNUM= PDATV- PDAT- 2
C                 determine beginning of first group
                  CALL WTSGRP (DATSTR,TGROUP,
     O                         GPSDAT)
C                 determine starting pointer index
                  I= 1
                  CALL TIMDIF (TBSDAT,GPSDAT,TGROUP,I,
     O                         GRPOFF)
                  IF (GRPOFF.LT.I0 .OR. GRPOFF.GE.TGRNUM) THEN
C                   error: date selected out of range for this dsn
                    RETCOD= -10
                  ELSE
                    GPOSST= TGRPST+ GRPOFF
                    IF (WIBUFF(GPOSST,DIND).EQ.0) THEN
C                     error: no data present for date selected
                      RETCOD= -10
                    END IF
                  END IF
                  IF (RETCOD.EQ.0) THEN
C                   skip values in this group as required, dont need VBTIME
C                   because ADDAFG is 0
                    CALL WTSKVX (WDMSFL,GPOSST,GPSDAT,DATSTR,DSFRC,
     I                           TSFILL,TGROUP,BADJFG,ADDAFG,ADDAFG,
     O                           CURREC,CURBKS,CURPOS,CURNOV,CURVAL,
     O                           PREVAL,CURTST,CURTUN,CURCMP,CURQUA,
     O                           CURCNT,CURDAT,RETCOD,BLSDAT,BLEDAT)
                    NVAL= 1
                    CALL TIMADD (CURDAT,CURTUN,CURTST,NVAL,
     O                           DATEX)
C                   write out where we are
                    IF (CURCMP.EQ.0) THEN
C                     one interval will be changed
                      CALL COPYI (I6,DATSTR,BLSDAT)
                      CALL COPYI (I6,DATEX ,BLEDAT)
                    END IF
C
                    OLDQUA = CURQUA
                    OLDVAL = CURVAL
C
                    IF (QUPD.EQ.0) THEN
                      CALL DATLST (BLSDAT,TBUFF(16),I,RETCOD)
                      CALL DATLST (BLEDAT,TBUFF(56),I,RETCOD)
                      CALL ZBLDWR (I0,BLNK,I1,-I1,J)
                      CALL ZBLDWR (LEN37,TBUFF(41),I0,-I1,J)
                      CALL ZBLDWR (LEN37,TBUFF,I0,-I1,J)
                      IF (CURCMP.NE.0) THEN
C                       note, more than 1 interval will be changed
                        SGRP= 18
                        CALL PMXCNW (MESSFL,SCLU,SGRP,I1,-I1,-I1,J)
                        CALL TIMCVT (BLSDAT)
                      END IF
C                     save text written
                      CALL ZMNSST
C                     init data screen
                      SGRP = 17
                      CALL Q1INIT (MESSFL,SCLU,SGRP)
C                     set current values
                      IVAL(1)= CURQUA
                      CALL QSETI (I1,IVAL)
                      RVAL(1)= CURVAL
                      CALL QSETR (I1,RVAL)
C                     perform screen editing
                      CALL Q1EDIT (IRET)
                    ELSE IF (CURQUA.EQ.31) THEN
C                     update quality code
                      CURQUA= 30
                      CALL DATLST (BLSDAT,TBUFF(LENTX+1),I,RETCOD)
                    END IF
                    IF (IRET.EQ.1) THEN
C                     user wants to continue, get edited values
                      CALL QGETI (I1,IVAL)
                      CURQUA= IVAL(1)
                      CALL QGETR (I1,RVAL)
                      CURVAL= RVAL(1)
                      IF (CURQUA .NE. OLDQUA  .OR.
     1                    ABS(CURVAL-OLDVAL) .GT. 1.0E-20) THEN
C                       user modified value
                        RIND= WDRCGO(WDMSFL,CURREC)
                        IF (CURQUA.NE.OLDQUA) THEN
C                         recalc BCW
                          WIBUFF(CURBKS,RIND)=
     1                    WBCWCL(CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
                        END IF
                        WRBUFF(CURPOS,RIND)= CURVAL
C                       update WDM record
                        CALL WDRCUP (WDMSFL,RIND)
C                       clear modified data from time-series buffer
                        CALL TSBCLR (WDMSFL,DSN)
C                       set dataset modification date attribute
                        CALL WDMODT (WDMSFL,DSN)
                        IF (QUPD.NE.0) THEN
C                         output summary to file
                          CALL INTCHR (CURNOV,LEN10,JUSTR,I,TBUFF(1))
                          I = LENTX + 21
                          CALL PRTSTR (OUTFL,I,TBUFF)
                          IF (KNT .GE. KNTMX) THEN
C                           reminder to screen
                            KNT = 0
                            CALL ZBLDWR (I,TBUFF,I0,I1,J)
                          END IF
                          KNT = KNT + 1
                        END IF
                      END IF
C                     make end of current value new start
                      CALL COPYI (I6,BLEDAT,DATSTR)
                      CALL TIMCVT (DATSTR)
                    ELSE IF (IRET.EQ.2) THEN
C                     user wants previous screen, back to dates
                      IRET= -2
                    END IF
                  ELSE IF (VORQ.EQ.1) THEN
C                   print data not available message
                    WNDNAM= 'Modify (DTM) Problem'
                    CALL PRTERR (MESSFL,WNDNAM,RETCOD)
C                   get user exit command value
                    CALL ZGTRET (IRET)
                  END IF
CPRH                  IF (RETCOD.EQ.0 .AND. VORQ.EQ.1) THEN
CPRHC                   another value (1-NO,2-YES)
CPRH                    AGAIN = 1
CPRH                    SGRP = 12
CPRH                    CALL QRESP (MESSFL,SCLU,SGRP,AGAIN)
CPRH                    IF (AGAIN .EQ. 1) RETCOD = 1
CPRH                  END IF
                END IF
              IF ((IRET.EQ.1 .AND. RETCOD.EQ.0)
     1                        .OR. IRET.EQ.-2) GO TO 200
            END IF
          IF (IRET.EQ.-1) GO TO 100
        END IF
C       turn off previous and interrupt
        I= 4
        CALL ZSTCMA (I,I0)
        I= 16
        CALL ZSTCMA (I,I0)
CPRH      IF (VORQ .NE. 3) GO TO 10
C
      IF (OPEN .EQ. 1) THEN
C       close summary output file
        DELFG = 0
        CALL QFCLOS (OUTFL,DELFG)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMTC
     I                    (MESSFL,SCLU,WDMSFL)
C
C     + + + PURPOSE + + +
C     copies an existing WDM timeseries dataset
C     into a new one, with optional updates
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number of WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,I0,I1,SGRP,INUM,RNUM,CNUM,CVAL(3),IVAL(8),
     1             SDSN,TDSN,UPTFL,RETCOD,SDSFRC,TDSFRC,LEN,SLEN,
     2             CSDAT(6),CEDAT(6),SSDAT(6),SEDAT(6),TSDAT(6),
     3             TEDAT(6),TDAT(6),ERDAT(6),LCOPFG,DLEN,
     4             ANS,TRDTFG,GPFLG,UPDT,XWDMSF,DSNFG,COPYFG,IRET,RESP,
     5             NWDMFL,NTYPE
      REAL         RVAL(1000)
      CHARACTER*1  TBUFF(80),BLNK,TBUFF2(60)
      CHARACTER*25 WNDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER      TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL     CHRCHR, DATCHK, DATESQ, INTCHR, PRNTXT, PRTERR
      EXTERNAL     QFCLOS, QFOPEN, QRESP, TIMADD, TIMCHK
      EXTERNAL     WDATCP, WDDSCL, WTDSCU, WTFNDT, ZIPC, PMXTXI, QRESPM
      EXTERNAL     ZSTCMA, ZGTRET, ZWNSET, PMXCNW, ZMNSST
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (60A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
      BLNK  = ' '
      DLEN  = 6
      SLEN  = 60
      SDSN  = -999
      TDSN  = -999
      RETCOD= 0
      LCOPFG= 0
      DSNFG = 0
      COPYFG= 0
      UPTFL = 0
C
 10   CONTINUE
C       do main copy menu
        SGRP= 20
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
C       allow previous command
        I= 4
        CALL ZSTCMA (I,I1)
C
        GO TO (100,200,300,400,500), RESP
C
 100    CONTINUE
C         specify source and target DSNs
          INUM = 2
          RNUM = 1
          CNUM = 1
 110      CONTINUE
C           back here on previous from problem with specified data sets
            IVAL(1) = SDSN
            IVAL(2) = TDSN
            SGRP = 21
            CALL QRESPM (MESSFL,SCLU,SGRP,INUM,RNUM,CNUM,
     M                   IVAL,RVAL,CVAL,TBUFF)
C           get user exit command value
            CALL ZGTRET (IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue
              SDSN = IVAL(1)
              TDSN = IVAL(2)
              IF (SDSN.GT.0 .AND. TDSN.GT.0) THEN
C               get information on source data set
                GPFLG= 1
                CALL WTFNDT (WDMSFL,SDSN,GPFLG,
     O                       SDSFRC,SSDAT,SEDAT,RETCOD)
                IF (RETCOD.NE.0) THEN
C                 check for no data return code
                  WNDNAM= 'Datasets (DTCD) Problem'
                  CALL PRTERR (MESSFL,WNDNAM,RETCOD)
                  SDSN = -999
                  DSNFG= 0
                END IF
              ELSE
                RETCOD= 1
                DSNFG= 0
              END IF
            END IF
            IF (RETCOD.EQ.0 .AND. IRET.EQ.1) THEN
C             get information on target data set
              GPFLG= 2
              CALL WTFNDT (WDMSFL,TDSN,GPFLG,
     O                     TDSFRC,TSDAT,TEDAT,RETCOD)
              IF (RETCOD.EQ.-81) THEN
C               target dsn does not exist, copy source label(1-yes,2-no)
                SGRP= 22
                CALL QRESP (MESSFL,SCLU,SGRP,LCOPFG)
C               get user exit command value
                CALL ZGTRET (IRET)
                IF (IRET.EQ.1) THEN
C                 user wants to continue
                  IF (LCOPFG.EQ.2) THEN
C                   user does not want to copy source label
                    TDSN = -999
                    DSNFG= 0
                  ELSE
C                   copy source when copy is performed
C                   set flag indicating data sets properly defined
                    DSNFG = 1
C                   may be new set of data sets, reset not copied flag
                    COPYFG= 0
                  END IF
                END IF
              ELSE IF (RETCOD.EQ.-6) THEN
C               no data in file
                TRDTFG= 0
                RETCOD= 0
                DSNFG = 1
C               may be new set of data sets, reset not copied flag
                COPYFG= 0
              ELSE IF (RETCOD.NE.0) THEN
C               other problem with target data set
                WNDNAM= 'Datasets (DTCD) Problem'
                CALL PRTERR (MESSFL,WNDNAM,RETCOD)
C               get user exit command value
                CALL ZGTRET (IRET)
                TDSN = -999
                DSNFG= 0
              ELSE
C               target dsn exists and contains data
                TRDTFG= 1
                DSNFG = 1
C               may be new set of data sets, reset not copied flag
                COPYFG= 0
              END IF
            ELSE IF (RETCOD.EQ.0) THEN
C             user selected previous screen, reset IRET to exit loop
              IRET= 1
            END IF
          IF (IRET.EQ.2) GO TO 110
          IF (DSNFG.EQ.1) THEN
C           source/target data sets defined, copy start/end dates to copy dates
            CALL WDATCP (SSDAT,CSDAT)
            CALL WDATCP (SEDAT,CEDAT)
          END IF
          GO TO 900
C
 200    CONTINUE
C         specify dates to copy
          IF (DSNFG.EQ.1) THEN
C           source and target data sets have been properly specified
 210        CONTINUE
              RETCOD= 0
C             what period do we copy?
              WNDNAM= 'Period (DTCP)'
              CALL ZWNSET (WNDNAM)
              CALL DATESQ (DLEN,CSDAT,CEDAT)
C
C             date errors
C
              IF (TRDTFG.EQ.1 .AND. TIMCHK(CSDAT,TEDAT).EQ.1) THEN
C               target data ends after start of copy, cant overwrite
                RETCOD= 23
                CALL PMXCNW (MESSFL,SCLU,RETCOD,I1,I1,-I1,J)
                CALL WDATCP (TEDAT,CSDAT)
              ELSE IF (TIMCHK(CSDAT,SSDAT).EQ.1) THEN
C               source data starts after start of copy
                RETCOD= 24
                CALL PMXCNW (MESSFL,SCLU,RETCOD,I1,I1,-I1,J)
                CALL WDATCP (SSDAT,CSDAT)
              ELSE IF (TIMCHK(CEDAT,SEDAT).EQ.-1) THEN
C               source data ends before end of copy
                RETCOD= 25
                CALL PMXCNW (MESSFL,SCLU,RETCOD,I1,I1,-I1,J)
                CALL WDATCP (SEDAT,CEDAT)
              ELSE IF (TRDTFG.EQ.1 .AND. TIMCHK(CSDAT,TSDAT).EQ.1) THEN
C               target data present after start of copy
                RETCOD= 26
                CALL PMXCNW (MESSFL,SCLU,RETCOD,I1,I1,-I1,J)
                CALL WDATCP (TEDAT,CSDAT)
              END IF
              ANS= 2
              IF (RETCOD.NE.0) THEN
C               ask if we should try again, save problem message
                CALL ZMNSST
                SGRP= 27
                CALL QRESP (MESSFL,SCLU,SGRP,ANS)
C               get user exit command value
                CALL ZGTRET (IRET)
              END IF
            IF (ANS.EQ.1 .OR. IRET.EQ.2) GO TO 210
          ELSE
C           data sets not yet specified, select Datasets option
            SGRP= 35
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 900
C
 300    CONTINUE
C         specify source for updating values
 310      CONTINUE
C           back here on previous from 2nd update screen
            SGRP= 36
            CALL QRESP (MESSFL,SCLU,SGRP,UPDT)
C           get user exit command value
            CALL ZGTRET (IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue
              IF (UPTFL.GT.0) THEN
C               user changing source of updates, close old update file
                CALL QFCLOS (UPTFL,I0)
                UPTFL= 0
              END IF
              IF (UPDT.EQ.1) THEN
C               get file containing data value updates
                SGRP = 28
                CALL QFOPEN (MESSFL,SCLU,SGRP,
     O                       UPTFL,RETCOD)
              ELSE IF (UPDT.EQ.2) THEN
                SGRP = 29
                CALL QFOPEN (MESSFL,SCLU,SGRP,
     O                       UPTFL,RETCOD)
                IF (RETCOD.EQ.0) THEN
C                 fill in new file
                  INUM= 8
                  RNUM= 1
                  CNUM= 1
                  IVAL(8) = 1
                  IVAL(1) = 0
                  RVAL(1) = -999.
                  CVAL(1) = 4
C                 copy starting date
                  CALL WDATCP (CSDAT,TDAT)
C                 initialize buffer
                  CALL ZIPC (SLEN+20,BLNK,TBUFF)
C                 allow interrupt command
                  I= 16
                  CALL ZSTCMA (I,I1)
C
 340              CONTINUE
C                   loop to get and write updates
                    CALL WDATCP (TDAT,IVAL(2))
                    SGRP= 30
                    CALL QRESPM (MESSFL,SCLU,SGRP,INUM,RNUM,CNUM,
     M                           IVAL,RVAL,CVAL,TBUFF)
C                   get user exit command value
                    CALL ZGTRET (IRET)
                    IF (IVAL(2) .LE. 0) THEN
C                     user set year to 0, so no more processing
                      IRET = 7
                    END IF
                    IF (TIMCHK(IVAL(2),CEDAT) .LE. 0) THEN
C                     reached end of copy period
                      IRET = 7
                      SGRP = 39
                      CALL PRNTXT (MESSFL, SCLU, SGRP)
                    END IF
                    IF (IRET.EQ.1) THEN
C                     user wants to continue, check dates
                      CALL DATCHK (IVAL(2),ERDAT)
                      DO 360 K= 1,6
                          IF (ERDAT(K).GT.0) RETCOD= ERDAT(K)
 360                  CONTINUE
                      IF (RETCOD.NE.0) THEN
C                       bad date format
                        SGRP= 31
                        CALL PRNTXT (MESSFL,SCLU,SGRP)
                        RETCOD= 0
                      ELSE IF (TIMCHK(TDAT,IVAL(2)).EQ.-1) THEN
C                       dates not in order
                        SGRP= 32
                        CALL PRNTXT (MESSFL,SCLU,SGRP)
                      ELSE
C                       fill in buffer to write to scratch file
                        CALL ZIPC (SLEN,BLNK,TBUFF2)
C                       use numeric equivalent for time units
C                       write (99,*) 'CVAL',CVAL
                        CALL INTCHR (CVAL(1),CVAL(3),I0,
     O                               I,TBUFF(CVAL(2)))
                        LEN = 34
                        CALL CHRCHR (LEN,TBUFF(19),TBUFF2(1))
                        LEN = 11
                        CALL CHRCHR (LEN,TBUFF(1),TBUFF2(35))
                        LEN = 7
                        CALL CHRCHR (LEN,TBUFF(12),TBUFF2(46))
C                       write (99,*) 'TBUFF,TBUFF2,SLEN,UPTFL',SLEN,UPTFL
C                       write (99,*) TBUFF
C                       write (99,*) TBUFF2
C                       write update to file
                        WRITE (UPTFL,2000) TBUFF2
C                       add time
                        CALL TIMADD (IVAL(2),CVAL(1),IVAL(8),I1,
     O                               TDAT)
                      END IF
                    END IF
                  IF (IRET.EQ.1) GO TO 340
C                 force complete write of scratch file
                  ENDFILE UPTFL
                  REWIND  UPTFL
C                 turn off interrupt command
                  I= 16
                  CALL ZSTCMA (I,I0)
                END IF
              END IF
            END IF
          IF (IRET.EQ.2) GO TO 310
          GO TO 900
C
 400    CONTINUE
C         perform copy function
          IF (DSNFG.EQ.1) THEN
C           dsns specified, ok to try copy
            SGRP= 33
            CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,J)
C
            IF (LCOPFG.EQ.1) THEN
C             target data set does not exist, copy label from source
C             default to same wdm file and data set type
              NWDMFL = WDMSFL
              NTYPE  = 0
              CALL WDDSCL (WDMSFL,SDSN,NWDMFL,TDSN,NTYPE,RETCOD)
            END IF
            XWDMSF= WDMSFL
            RNUM  = 1000
            CALL WTDSCU (XWDMSF,SDSN,WDMSFL,TDSN,
     I                   UPTFL,CSDAT,CEDAT,RNUM,
     O                   RVAL,RETCOD)
C           copy complete
            SGRP = 34
            CALL PMXTXI (MESSFL,SCLU,SGRP,I1,-I1,I0,I1,RETCOD)
C           set flag indicating copy is complete
            COPYFG= 1
          ELSE
C           data sets not yet specified, cant do copy
            SGRP= 38
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            RESP= 1
          END IF
          GO TO 900
C
 500    CONTINUE
C         return to values screen
          IF (DSNFG.EQ.1 .AND. COPYFG.EQ.0) THEN
C           datasets specified, but copy not performed, sure you want to return
            SGRP= 37
            RESP= 2
            CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C           get user exit command value
            CALL ZGTRET (IRET)
            IF (RESP.EQ.1 .AND. IRET.EQ.1) THEN
C             yes, get me outta here
              RESP= 5
            ELSE
C             no, dont return, let me try again
              RESP= 4
            END IF
          END IF
          GO TO 900
C
 900    CONTINUE
C
C       turn off previous command
        I= 4
        CALL ZSTCMA (I,I0)
C
      IF (RESP.NE.5) GO TO 10
C
      IF (UPTFL.NE.0) THEN
C       close update file
        CALL QFCLOS (UPTFL,I0)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMTD
     I                    (MESSFL,SCLU,WDMSFL)
C
C     + + + PURPOSE + + +
C     Delete part of data in an existing WDM timeseries dataset.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number of WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I0,I1,I6,SGRP,DSN,GPFLG,DSFREC,
     1             ALLFLG,IRET,CHK,ANS,RETCOD,
     2             DATDEL(6),DATSTR(6),DATEND(6),DATMAX(6),DATMIN(6)
      CHARACTER*22 WNDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER      TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL     TIMCHK, QSCSTI, PRNTXI, PRNTXT, PRTERR, PMXCNW
      EXTERNAL     QRESP, QRESPI, ZSTCMA, ZGTRET, ZMNSST, TSBCLR
      EXTERNAL     WDATCP, WTDDEL, WTFNDT, Q1INIT, Q1EDIT, QSETI, QGETI
C
C     + + + DATA INITIIALIZATIONS + + +
      DATA DATMIN / 0,  1,  1,  0,  0,  0 /
      DATA DATMAX / 0, 12, 31, 24, 59, 59/
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I6 = 6
      GPFLG = 2
      RETCOD= 0
C     allow previous and interrupt
      I= 4
      CALL ZSTCMA (I,I1)
      I= 16
      CALL ZSTCMA (I,I1)
C
 10   CONTINUE
C       which dsn (0 to quit)?
        DSN = -999
        SGRP= 41
        CALL QRESPI (MESSFL,SCLU,SGRP,DSN)
C       get user exit command value
        CALL ZGTRET (IRET)
C
        IF (DSN.GT.0 .AND. IRET.EQ.1) THEN
C         check existance of dsn, get start and end dates
          CALL WTFNDT (WDMSFL,DSN,GPFLG,
     O                 DSFREC,DATSTR,DATEND,RETCOD)
          IF (RETCOD.NE.0) THEN
C           problem with dsn
            WNDNAM= 'Delete (DTD) Problem'
            CALL PRTERR (MESSFL,WNDNAM,RETCOD)
          ELSE
C           specify date to begin deleting
 20         CONTINUE
C             back here on previous from 3rd screen, init screen
              SGRP= 42
              CALL Q1INIT (MESSFL,SCLU,SGRP)
C             determine min/max for start year based on available data
              CALL WDATCP (DATEND,DATDEL)
              DATMIN(1)= DATSTR(1)
              DATMAX(1)= DATEND(1)
C             set min/max/default based on period of record for data set
              CALL QSCSTI (I6,I1,DATMIN,DATMAX,DATDEL)
C             set current values for data fields
              CALL QSETI (I6,DATDEL)
C             perform editing of date to begin deleting
              CALL Q1EDIT (IRET)
C
              IF (IRET.EQ.1) THEN
C               user wants to continue, get edited values
                CALL QGETI (I6,DATDEL)
C               check against start date
                CHK= TIMCHK (DATSTR,DATDEL)
                IF (CHK.LE.0) THEN
C                 date preceeds start date, all data will be deleted
                  CALL WDATCP (DATSTR,DATDEL)
                  ALLFLG= 1
                ELSE
C                 not all data to be deleted
                  ALLFLG= 0
                END IF
              ELSE IF (IRET.EQ.2) THEN
C               user wants previous screen (specify DSN number)
                IRET= -1
              END IF
C
              IF (RETCOD.EQ.0 .AND. IRET.EQ.1) THEN
C               check end date
                CHK= TIMCHK (DATDEL,DATEND)
                IF (CHK.LE.0) THEN
C                 no data to delete
                  SGRP= 43
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                  RETCOD= 2
                ELSE
C                 some data to delete
                  IF (ALLFLG.EQ.1) THEN
C                   in fact, all data will be deleted
                    SGRP= 44
                    CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,-I1,I)
C                   save this message for the upcoming menu
                    CALL ZMNSST
                  END IF
C                 are you sure? (1-Yes,2-No)
                  SGRP= 45
                  CALL QRESP (MESSFL,SCLU,SGRP,ANS)
                  IF (ANS.EQ.2) THEN
C                   cancel deletion
                    RETCOD= 3
                  END IF
                END IF
C               get user exit command value
                CALL ZGTRET (IRET)
              END IF
            IF (IRET.EQ.2) GO TO 20
          END IF
C
          IF (RETCOD.NE.0) THEN
C           delete aborted
            SGRP= 46
            CALL PRNTXI (MESSFL,SCLU,SGRP,RETCOD)
          ELSE IF (IRET.EQ.1) THEN
C           do delete
            CALL WTDDEL (WDMSFL,DSN,DATDEL,ALLFLG,RETCOD)
C           clear modified data from time-series buffer
            CALL TSBCLR (WDMSFL,DSN)
            SGRP= 47
            CALL PRNTXI (MESSFL,SCLU,SGRP,RETCOD)
          END IF
        END IF
C
      IF ((DSN.GT.0 .OR. IRET.EQ.-1) .AND. IRET.NE.7) GO TO 10
C
C     turn off previous and interrupt
      I= 4
      CALL ZSTCMA (I,I0)
      I= 16
      CALL ZSTCMA (I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMTA
     I                    (MESSFL,SCLU,WDMSFL)
C
C     + + + PURPOSE + + +
C     This routine adds data to a WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    MESSFL,SCLU,WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number of WDM file
C
C     + + + PARAMETERS + + +
      INTEGER   MXB
      PARAMETER (MXB = 512)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I0,I1,SGRP,DSN,RETCOD,FLG,QFLG,NVAL,
     1             SDATIM(6),EDATIM(6),DSDELT,DATLEN,DSTYP,
     2             SAIND,SALEN,DTOVWR,DSFRC,TUNITS,IRET,GRCNT,
     3             IVAL(9),TU(1,3),INUM,RNUM,CNUM,ERR(6)
      REAL         OBUFF(MXB),RVAL(1)
      CHARACTER*1  BUF(80)
      CHARACTER*23 WNDNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL     CKDATE, PRNTXT, PRTERR, COPYI, DATCHK
      EXTERNAL     WDTPUT, WTFNDT, WDBSGI, QRESPM, CKTSTU, QRSPRA
      EXTERNAL     TIMCVT, ZSTCMA, ZGTRET, QRESPI, WDSCHA
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   INUM, RNUM, CNUM
     #     /    9,    1,    1/
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      NVAL = -999
      QFLG = -999
C     allow previous and interrupt
      I= 4
      CALL ZSTCMA (I,I1)
      I= 16
      CALL ZSTCMA (I,I1)
      DSN = 0
C
 10   CONTINUE
C       back here on previous from 1st Add screen, which dataset?
Cmyg    use GRP 73 which does not tell user to enter 0 for dsn in order to
Cmyg    return to Timeseries screen; entering 0 doesn't work--should it?
        SGRP = 73
Cmyg    SGRP= 51
        CALL QRESPI (MESSFL,SCLU,SGRP,DSN)
C       get user exit command value
        CALL ZGTRET (IRET)
        IF (IRET.EQ.1) THEN
C         user wants to continue, check validity of data set
          I= 2
          DSTYP= 1
          CALL WDSCHA (WDMSFL,DSN,DSTYP,I,
     O                 DSFRC,GRCNT,RETCOD)
          IF (RETCOD .EQ. 0) THEN
C           good data set, check for dates
            CALL WTFNDT (WDMSFL,DSN,I1,DSFRC,
     O                   SDATIM,EDATIM,RETCOD)
            IF (RETCOD .NE. 0) THEN
C             use base year, month,day
              SALEN = 1
              SAIND = 27
              CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O                     EDATIM(1),RETCOD)
              IF (RETCOD .NE. 0) EDATIM(1) = 1900
              SALEN = 1
              SAIND = 28
              CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O                     EDATIM(2),RETCOD)
              IF (RETCOD .NE. 0) EDATIM(2) = 1
              SALEN = 1
              SAIND = 29
              CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O                     EDATIM(3),RETCOD)
              IF (RETCOD .NE. 0) EDATIM(3) = 1
              SALEN = 1
              SAIND = 30
              CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O                     EDATIM(4),RETCOD)
              IF (RETCOD .NE. 0) EDATIM(4) = 0
              EDATIM(5) = 0
              EDATIM(6) = 0
              RETCOD = 0
            END IF
C           get time step
            SALEN = 1
            SAIND = 33
            CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O                   DSDELT,RETCOD)
            IF (RETCOD.NE.0) THEN
C             assign default time step
              DSDELT = 1
            END IF
            SAIND = 17
            CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O                     TUNITS,RETCOD)
          END IF
          IF (RETCOD.NE.0) THEN
C           big problem with data set
            WNDNAM= 'Add - 1 (DTA) Problem'
            CALL PRTERR (MESSFL,WNDNAM,RETCOD)
C           get user exit command value
            CALL ZGTRET (IRET)
            IF (IRET.EQ.1) THEN
C             process continue the same as interrupt
              IRET= 7
            END IF
          END IF
        END IF
C
        IF (IRET.EQ.1) THEN
C         user wants to continue
          DATLEN= 6
          CALL COPYI (DATLEN,EDATIM,SDATIM)
          CALL TIMCVT (SDATIM)
C
C         get start date, number of values, quality code, time step, time units
 30       CONTINUE
C           back here on previous from data values screen
            CALL COPYI (DATLEN,SDATIM,IVAL)
            IVAL(7) = NVAL
            IVAL(8) = QFLG
            IVAL(9) = DSDELT
            TU(1,1)= TUNITS
 40         CONTINUE
C             back here on back values entered
              SGRP = 52
              CALL QRESPM (MESSFL,SCLU,SGRP,INUM,RNUM,CNUM,
     M                     IVAL,RVAL,TU,BUF)
C             get user exit command value
              CALL ZGTRET (IRET)
              IF (IRET.EQ.1) THEN
C               user wants to continue
                FLG = 0
                SGRP= 0
C               check validity of date entered
                CALL DATCHK (IVAL,ERR)
                DO 50 I= 1,DATLEN
                  IF (ERR(I).NE.0) THEN
C                   problem with date
                    FLG = 1
                    SGRP= 55
                  END IF
 50             CONTINUE
                IF (FLG.EQ.0) THEN
C                 make sure specified starting date
C                 is after end of current data
                  CALL CKDATE (IVAL,EDATIM,FLG)
                  IF (FLG.LE.0) THEN
C                   date to start adding is not after curren end of data
                    SGRP= 54
                  END IF
                END IF
                IF (FLG.EQ.0) THEN
C                 check time step and units
                  WNDNAM= 'Add - 2 (DTA) Problem'
                  CALL CKTSTU (IVAL(9),TU(1,1),I1,WNDNAM,
     O                         FLG)
                END IF
                IF (FLG.NE.0 .AND. SGRP.NE.0) THEN
C                 had problem, display message
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
C                 get user exit command value
                  CALL ZGTRET (IRET)
                  IF (IRET.NE.7) THEN
C                   user not interrupting, back to dates screen
                    IRET= 1
                  END IF
                ELSE
C                 flag may have been set to non-zero, but was not a problem
                  FLG = 0
                END IF
              END IF
            IF (FLG.NE.0 .AND. IRET.EQ.1) GO TO 40
            IF (IRET.EQ.1) THEN
C             user continuing
              CALL COPYI (DATLEN,IVAL,SDATIM)
              NVAL = IVAL(7)
              QFLG = IVAL(8)
              DSDELT = IVAL(9)
              TUNITS = TU(1,1)
C
C             fill array with input from terminal
              SGRP = 53
              CALL QRSPRA (MESSFL,SCLU,SGRP,MXB,I1,
     M                     NVAL,
     O                     OBUFF)
C             get user exit command value
              CALL ZGTRET (IRET)
              IF (IRET.EQ.1) THEN
C               user wants to continue, add values to wdm
                DTOVWR= 0
                CALL WDTPUT (WDMSFL,DSN,DSDELT,SDATIM,NVAL,DTOVWR,
     #                       QFLG,TUNITS,OBUFF,RETCOD)
                IF (RETCOD.NE.0) THEN
C                 problem writing to data set
                  WNDNAM= 'Add - 3 (DTA) Problem'
                  CALL PRTERR (MESSFL,WNDNAM,RETCOD)
                END IF
              END IF
            ELSE IF (IRET.EQ.2) THEN
C             back to 1st screen
              IRET= 1
            END IF
          IF (IRET.EQ.2) GO TO 30
        END IF
      IF (IRET.EQ.1) GO TO 10
C
C     turn off previous command
      I= 4
      CALL ZSTCMA (I,I0)
      I= 16
      CALL ZSTCMA (I,I0)
C
      RETURN
      END

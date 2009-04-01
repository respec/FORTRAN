C
C
C
      SUBROUTINE   PRWMTT
     I                    (MESSFL, WDMSFL, PTHNAM, DSNCNT, DSNBUF)
C
C     + + + PURPOSE + + +
C     Table information on timeseries from a WDM file.
C     Only a table of daily values is implemented.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL, WDMSFL, DSNCNT, DSNBUF(DSNCNT)
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     PTHNAM - character string of path of options selected to get here
C     DSNCNT - count of datasets to work with
C     DSNBUF - array of dataset numbers
C
C     + + + PARAMETERS + + +
      INTEGER     MXDSN,    NPARM
      PARAMETER  (MXDSN=300,NPARM=27)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I, I1, I6, N, NM, SCLU, SGRP, STMON, RESP,
     $            ENDMON, TDATE(6), SDATE(6), EDATE(6), FOUT, SALEN,
     $            RETCOD, INIT, YRCNT, ILEN, FLOWFG(MXDSN),
     $            SIGDIG(MXDSN), DECIML(MXDSN), DTRAN(MXDSN),
     $            START(6,MXDSN), END(6,MXDSN), SAIND, LPARM,
     $            TSFORM, ARSDIG(NPARM), ARDCML(NPARM), PARMCD(NPARM)
      REAL        QM(366), BAD(MXDSN)
      CHARACTER*1 TITLE(48,MXDSN),BLNK(1)
C
C     + + + FUNCTIONS + + +
      INTEGER    TIMCHK, DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   TIMCHK, DAYMON, QRESP, ZWNSOP, ZIPI, WTDTBL, WDBSGI
      EXTERNAL   GETFIL, TBSTIM, TBSPEC, GETDAT, TBSTAT, COPYI, ZIPC
      EXTERNAL   GETTXT, PRNTXT, WDBSGR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  TDATE/0,0,0,24,0,0/
      DATA PARMCD/1,2,3,4,5,9,10,12,13,14,20,25,30,35,36,
     $            45,50,52,54,55,60,65,70,76,80,90,95/
      DATA ARSDIG/6*4,5*3,4,4,3,3,4,3,3,9*4/
      DATA ARDCML/6*2,5*1,2,2,1,1,2,1,1,9*2/
C
C     + + + END SPECIFICATIONS + + +
C
C     + + + HISTORY + + +
C     The variable BAD was added to the PRM2 screen so the user could 
C     modify it to correct for the case when TSFILL is set to 0.0 but
C     the user wants to see 0.0 in the table instead of "-".
C     AML 7/15/94
C
      I1   = 1
      I6   = 6
      BLNK(1) = ' '
      FOUT = 0
      SCLU = 41
C
C     set defaults for table characteristics
      STMON = 10
      ENDMON= 9
      NM    = 12
      I= 0
      CALL ZIPI (6*MXDSN,I,START)
      CALL ZIPI (6*MXDSN,I,END)
      DO 5 I= 1,DSNCNT
C       set defaults for selected data sets
        START(3,I)= 1
        END(4,I)  = 24
C       get parameter code to determine type of data
        SAIND= 56
        CALL WDBSGI (WDMSFL,DSNBUF(I),SAIND,I1,
     O               LPARM,RETCOD)
        FLOWFG(I) = 0
        SGRP = 0
        IF (RETCOD.EQ.0) THEN
C         attribute value exists
          N= 0
 7        CONTINUE
C           check through valid parameter codes for a match
            N= N+ 1
            IF (LPARM.EQ.PARMCD(N)) THEN
C             matching parameter codes, set defaults
              SGRP= 100+ N
              SIGDIG(I)= ARSDIG(N)
              DECIML(I)= ARDCML(N)
              IF (LPARM.EQ.60) THEN
C               flow data, include inches calculation
                FLOWFG(I)= 1
              END IF
            END IF
          IF (SGRP.EQ.0 .AND. N.LT.NPARM) GO TO 7
        END IF
        ILEN = 48
        IF (SGRP.GT.0) THEN
C         get default title for type of data
          CALL GETTXT (MESSFL,SCLU,SGRP,ILEN,
     O                 TITLE(1,I))
        ELSE
C         unknown data type, init title to blanks
          CALL ZIPC (ILEN,BLNK,TITLE(1,I))
C         init significant digits and decimal places to defaults
          SIGDIG(I)= 3
          DECIML(I)= 1
        END IF
C       get form of data
        SAIND= 84
        CALL WDBSGI (WDMSFL,DSNBUF(I),SAIND,I1,
     O               TSFORM,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         attribute value exists
          IF (TSFORM.EQ.1) THEN
C           mean over time step
            DTRAN(I)= 0
          ELSE IF (TSFORM.EQ.2) THEN
C           total over time step
            DTRAN(I)= 1
          ELSE IF (TSFORM.EQ.3) THEN
C           instantaneous
            DTRAN(I)= 0
          ELSE IF (TSFORM.EQ.4) THEN
C           minimum over time step
            DTRAN(I)= 3
          ELSE IF (TSFORM.EQ.5) THEN
C           maximum over time step
            DTRAN(I)= 2
          END IF
        ELSE
C         no attribute value, set to default
          DTRAN(I)= 0
        END IF
C
C       get number for bad data
        SAIND = 32
        SALEN = 1
        CALL WDBSGR (WDMSFL,DSNBUF(I),SAIND,SALEN,
     O               BAD(I),RETCOD)
        IF (RETCOD.NE.0) THEN
C         could not get value for attribute, set to default value
          BAD(I) = -999999.
        END IF
 5    CONTINUE
C
 10   CONTINUE
C       do main Table menu
        CALL ZWNSOP (I1,PTHNAM)
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
        GO TO (100,200,300,400,500), RESP
C
 100    CONTINUE
C         prompt user for name of output file for storing tables
          CALL GETFIL (MESSFL, SCLU, PTHNAM,
     M                 FOUT)
          GO TO 999
C
 200    CONTINUE
C         get time parameters for table
          CALL TBSTIM (MESSFL,SCLU,PTHNAM,WDMSFL,DSNCNT,DSNBUF,
     M                 STMON,ENDMON,NM,START,END,DTRAN)
          GO TO 999
C
 300    CONTINUE
C         specify table characteristics
          CALL ZWNSOP (I1,PTHNAM)
          CALL TBSPEC (MESSFL,SCLU,DSNCNT,DSNBUF,
     M                 SIGDIG,DECIML,BAD,TITLE)
          GO TO 999
C
 400    CONTINUE
C         output time-series tables
          IF (FOUT.GT.0) THEN
C           output file opened, initialize data set counter
            N = 0
 420        CONTINUE
C             loop through each data set in DSNBUF
              N = N + 1
              INIT = 0
C             initialize year counter
              YRCNT = 1
C
C             set actual beg./end dates for table
              CALL COPYI (I6, START(1,N), SDATE)
              SDATE(2) = STMON
              CALL COPYI (I6, END(1,N), EDATE)
              EDATE(2) = ENDMON
              EDATE(3) = DAYMON (EDATE(1), EDATE(2))
C             write(99,*) 'get data for ',DSNBUF(N)
C             write(99,*) '       START ',(START(I,N),I=1,6)
C             write(99,*) '         END ',(END(I,N),I=1,6)
C             write(99,*) '       SDATE ',SDATE
C             write(99,*) '       EDATA ',EDATE
C
C             begin the loop for each year
              TDATE(1) = SDATE(1)
              TDATE(2) = SDATE(2) + NM - 1
              IF (TDATE(2) .GT. 12) THEN
                TDATE(2) = TDATE(2) - 12
                TDATE(1) = TDATE(1) + 1
              END IF
              TDATE(3) = DAYMON(TDATE(1),TDATE(2))
 430          CONTINUE
C               print message:  tabling year &
                CALL ZWNSOP (I1,PTHNAM)
                CALL TBSTAT (MESSFL, SCLU, DSNBUF(N), TDATE(1), NM,
     M                       INIT)
C               get the data
                CALL ZWNSOP (I1,PTHNAM)
                CALL GETDAT (MESSFL, SCLU, WDMSFL, DSNBUF(N),
     I                       YRCNT, START(1,N), END(1,N), NM, DTRAN(N),
     M                       SDATE, EDATE, TDATE,
     O                       QM, RETCOD)
                write(99,*) 'return code from GETDAT:',RETCOD
                IF (RETCOD .EQ. 0) THEN
C                 table the data
                  CALL WTDTBL (WDMSFL, FOUT, MESSFL, SCLU, DSNBUF(N),
     I                         SDATE(1), SDATE(2), NM, QM, SIGDIG(N),
     I                         DECIML(N), TITLE(1,N), FLOWFG(N), 
     I                         BAD(N))
                ELSE
C                 reset TBSTAT screen initialization variable--this
C                 is necessary since error messages written in GETDAT
                  INIT = 0
                END IF
C               reset dates for tabling next year
                SDATE(1) = SDATE(1) + 1
                TDATE(1) = TDATE(1) + 1
                TDATE(3) = DAYMON(TDATE(1),TDATE(2))
C               increment year counter
                YRCNT = YRCNT + 1
              IF (TIMCHK(TDATE,EDATE) .GE. 0) GO TO 430
            IF (N.LT.DSNCNT) GO TO 420
          ELSE
C           can't output time series, no file opened
            CALL ZWNSOP (I1,PTHNAM)
            SGRP= 3
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 999
C
 500    CONTINUE
C         return to Timeseries menu
          GO TO 999
C
 999    CONTINUE
C
      IF (RESP.NE.5) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETFIL
     I                  (MESSFL, SCLU, PTHNAM,
     M                   FOUT)
C
C     + + + PURPOSE + + +
C     Get output file for storing tables; user not allowed to continue
C     if no output file successfully opened.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL, SCLU, FOUT
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number of message file
C     PTHNAM - character string of path of options selected to get here
C     FOUT   - Fortran unit number of output file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I, I0, I1, SGRP, CMDID, IRET, DONFG, PLEN
      CHARACTER*8  PTHSTR
      CHARACTER*20 SCNAME
      CHARACTER*64 FNAME
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (FNAME,FNAM1)
      CHARACTER*1  FNAM1(64)
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     LENSTR, QFCLOS, Q1INIT, QSTCTF, Q1EDIT, QGTCTF
      EXTERNAL     ZSTCMA, CKNAME, ZWNSOP, CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C     set command id for 'Previous' command
      CMDID = 4
C
C     close output file, if one already open
      IF (FOUT .NE. 0) THEN
        CALL QFCLOS (FOUT, I0)
        FOUT = 0
      END IF
C     init file name
      FNAME= ' '
C     allow 'Previous' command
      CALL ZSTCMA (CMDID, I1)
      SGRP= 10
 10   CONTINUE
C       file name for output?
        CALL ZWNSOP (I1,PTHNAM)
        CALL Q1INIT(MESSFL,SCLU,SGRP)
        I= 64
        CALL QSTCTF(I1,I,FNAM1)
C       interact with user
        CALL Q1EDIT(IRET)
C       assume everythings ok
        DONFG= 0
        IF (IRET .EQ. 2) THEN
C         user wants out
          DONFG= 1
        ELSE
C         get name of daily output file
          I= 64
          CALL QGTCTF(I1,I,
     O                FNAM1)
C         try to open file, for write access
          I= 8
          PLEN= LENSTR(I,PTHNAM)
          CALL CARVAR(PLEN,PTHNAM,I,PTHSTR)
C         set screen name
          SCNAME= 'File ('//PTHSTR(1:PLEN)//'TF)'
          I= 2
          CALL CKNAME (SCNAME,FNAME,I,
     O                 FOUT)
          IF (FOUT .EQ. 0) THEN
C           problem with name
            DONFG= 2
          END IF
        END IF
      IF (DONFG .EQ. 2) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   TBSTIM
     I                   (MESSFL, SCLU, PTHNAM, WDMSFL, DSNCNT, DSNBUF,
     M                    STMON, ENDMON, TOTMON, START, END, DTRAN)
C
C     + + + PURPOSE + + +
C     Prompt user for input on time parameters for table specifications.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL, SCLU, WDMSFL, DSNCNT, DSNBUF(DSNCNT),
     $            STMON, ENDMON, TOTMON, DTRAN(DSNCNT),
     $            START(6,DSNCNT), END(6,DSNCNT)
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number of message file
C     PTHNAM - character string of path of options selected to get here
C     WDMSFL - Fortran unit number of WDM file
C     DSNCNT - count of data sets being tabled
C     DSNBUF - array of data-set numbers
C     STMON  - 1st (leftmost) month in table
C     ENDMON - last (rightmost) month in table
C     TOTMON - number of months included in table
C     START  - array of starting dates for data sets being tabled
C     END    - array of ending dates for data sets being tabled
C     DTRAN  - array of data transformations for data sets being tabled
C
C     + + + PARAMETERS + + +
      INTEGER     MXDSN
      PARAMETER  (MXDSN=300)
C 
C     + + + SAVES + + +
      INTEGER     PERIOD
      SAVE        PERIOD
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I0,I1,SGRP,CNUM,CVAL(3),INUM,IVAL(4),IRET,RNUM,
     $            IVAL2(3,MXDSN),CVAL2(3,MXDSN),CMDID,GPFLG,TDSFRC,
     $            LSDAT(6),LEDAT(6),STFLG,ENDFLG,TSTMON,ERROR,RETCOD,
     $            CLEN(2),ONUM,OTYP(4)
      REAL        RVAL(1)
      DOUBLE PRECISION DVAL(1)
      CHARACTER*1 CMON(3,12),CTXT(6)
C
C     + + + FUNCTIONS + + +
      INTEGER     DAYMON, TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL    DAYMON, TIMCHK, Q1INIT, Q1EDIT, QSETCO, QGETCO
      EXTERNAL    QSETI, QGETI, ZSTCMA, ZWNSOP, WTFNDT, WTDATE, ZIPI
      EXTERNAL    PMXTXM, ZMNSST, PRNTXT, CHRCHR
      EXTERNAL    Q2INIT, Q2EDIT, Q2SETI, Q2STCO, Q2GETI, Q2GTCO
C
C     + + + DATA INITIALIZATIONS + + +
      DATA PERIOD /1/
      DATA CMON/'J','A','N','F','E','B','M','A','R','A','P','R',
     $          'M','A','Y','J','U','N','J','U','L','A','U','G',
     $          'S','E','P','O','C','T','N','O','V','D','E','C'/
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
C     set command id for 'Previous' command
      CMDID = 4
C     turn on 'Previous' command
      CALL ZSTCMA (CMDID, I1)
C
 10   CONTINUE
C       back here on previous from 2nd screen
 20     CONTINUE
C         back here on oops from 1st screen, initialize screen
          CALL ZWNSOP (I1,PTHNAM)
          SGRP = 20
          CALL Q1INIT (MESSFL,SCLU,SGRP)
C         set default values for table spec.s
          CNUM = 3
          CVAL(1) = STMON
          CVAL(2) = ENDMON
          CVAL(3) = PERIOD
          CALL QSETCO (CNUM,CVAL)
C
C         get input from user
          CALL Q1EDIT (IRET)
C
C       if user chose 'Oops', redraw screen
        IF (IRET .EQ. -1) GOTO 20
C
        IF (IRET .EQ. 1) THEN
C         user wants to continue; retrieve values from screen
          CALL QGETCO (CNUM,
     O                 CVAL)
          STMON = CVAL(1)
          ENDMON= CVAL(2)
C         calculate no. of months to be tabled
          IF (STMON .GT. ENDMON) THEN
            TOTMON = (12-STMON+1) + (1+ENDMON-1)
          ELSE
            TOTMON = ENDMON - STMON + 1
          END IF
          PERIOD = CVAL(3)
C
C         determine period of record to table for each data set
          GPFLG= 1
 100      CONTINUE
C           back here on problem with dates entered
            ERROR= 0
            IF (PERIOD.EQ.1 .OR. PERIOD.EQ.2) THEN
C             user to specify common or constant period for all data sets
              CALL WTDATE (WDMSFL,DSNCNT,DSNBUF,PERIOD,
     O                     LSDAT,LEDAT,RETCOD)
 110          CONTINUE
C               back here on 'oops', show period available
                IVAL(1)= LSDAT(1)
                IVAL(2)= LEDAT(1)
                CLEN(1)= 3
                CLEN(2)= 3
                CALL CHRCHR (CLEN(1),CMON(1,LSDAT(2)),CTXT)
                CALL CHRCHR (CLEN(2),CMON(1,LEDAT(2)),CTXT(CLEN(1)+1))
                ONUM= 4
                OTYP(1)= 1
                OTYP(2)= 4
                OTYP(3)= 1
                OTYP(4)= 4
                SGRP= 20 + PERIOD
                CALL PMXTXM (MESSFL,SCLU,SGRP,I1,I1,ONUM,OTYP,-I1,
     I                       IVAL,RVAL,DVAL,CLEN,CTXT)
C               save screen text
                CALL ZMNSST
                CALL ZWNSOP (I1,PTHNAM)
C               modify common/constant dates
                SGRP= 22 + PERIOD
                CALL Q1INIT (MESSFL,SCLU,SGRP)
C               set start/end dates
                INUM= 2
                CALL QSETI (INUM,IVAL)
                CNUM= 2
                CVAL(1)= LSDAT(2)
                CVAL(2)= LEDAT(2)
                CALL QSETCO (CNUM,CVAL)
C               get user input
                CALL Q1EDIT (IRET)
              IF (IRET.EQ.-1) GO TO 110
C
              IF (IRET.EQ.1) THEN
C               user accepted values
                CALL QGETI (INUM,IVAL)
                CALL QGETCO (CNUM,CVAL)
                DO 120 I= 1,DSNCNT
C                 copy common/constant dates to each data-set's dates
                  START(1,I)= IVAL(1)
                  START(2,I)= CVAL(1)
                  END(1,I)= IVAL(2)
                  END(2,I)= CVAL(2)
 120            CONTINUE
C               check validity of dates
                IF (TIMCHK(START,END).LE.0) THEN
C                 ending date preceeds starting date
                  ERROR= 1
                ELSE IF (TOTMON.LT.12) THEN
C                 check if start/end mo. specified falls w/in mo.s to be tabled
                  STFLG = 0
                  ENDFLG= 0
                  DO 130 I = 0, TOTMON-1
                    IF (STMON+I .GT. 12) THEN
                      TSTMON = (STMON+I) - 12
                    ELSE
                      TSTMON = STMON + I
                    END IF
                    IF (START(2,1) .EQ. TSTMON) STFLG = 1
                    IF (END(2,1) .EQ. TSTMON) ENDFLG = 1
 130              CONTINUE
                  IF (.NOT. (STFLG.EQ.1 .AND. ENDFLG.EQ.1)) ERROR = 3
                END IF
              END IF
            ELSE IF (PERIOD.EQ.3) THEN
C             use full period
              DO 150 I= 1,DSNCNT
C               determine full period for each data set
                CALL WTFNDT (WDMSFL,DSNBUF(I),GPFLG,
     O                       TDSFRC,START(1,I),END(1,I),RETCOD)
 150          CONTINUE
            ELSE IF (PERIOD.EQ.4) THEN
C             user to specify unique period for each data set
 155          CONTINUE
C               back here on 'oops' command
                DO 160 I= 1,DSNCNT
C                 fill in defaults for data sets
                  CALL WTFNDT (WDMSFL,DSNBUF(I),GPFLG,
     O                         TDSFRC,LSDAT,LEDAT,RETCOD)
                  IVAL2(1,I)= I
                  IVAL2(2,I)= LSDAT(1)
                  CVAL2(1,I)= LSDAT(2)
                  IVAL2(3,I)= LEDAT(1)
                  CVAL2(2,I)= LEDAT(2)
                  CVAL2(3,I)= DTRAN(I) + 1
 160            CONTINUE
C               let user specify period for data sets
                CALL ZWNSOP (I1,PTHNAM)
C               init screen parameters
                SGRP= 25
                CALL Q2INIT (MESSFL,SCLU,SGRP)
C               set integer field values
                INUM= 3
                CALL Q2SETI (INUM,DSNCNT,IVAL2)
C               set character fields
                CNUM= 3
                CALL Q2STCO (CNUM,DSNCNT,CVAL2)
C               perform editing of dates
                CALL Q2EDIT (DSNCNT,
     O                       IRET)
              IF (IRET.EQ.-1) GO TO 155
C
              IF (IRET.EQ.1) THEN
C               user wants to continue, get edited values
                CALL Q2GETI (INUM,DSNCNT,
     O                       IVAL2)
                CALL Q2GTCO (CNUM,DSNCNT,
     O                       CVAL2)
                DO 180 I= 1,DSNCNT
C                 fill in edited values for each data set
                  CALL WTFNDT (WDMSFL,DSNBUF(I),GPFLG,
     O                         TDSFRC,LSDAT,LEDAT,RETCOD)
                  START(1,I)= IVAL2(2,I)
                  START(2,I)= CVAL2(1,I)
                  END(1,I)  = IVAL2(3,I)
                  END(2,I)  = CVAL2(2,I)
                  DTRAN(I)  = CVAL2(3,I) - 1
                  LSDAT(3)  = 1
                  LEDAT(3)  = DAYMON(LEDAT(1),LEDAT(2))
                  LEDAT(4)  = 24
                  J= 3
                  CALL ZIPI (J,I0,LSDAT(4))
                  J= 2
                  CALL ZIPI (J,I0,LEDAT(5))
                  IF (TIMCHK(START(1,I),END(1,I)) .LE. 0) THEN
C                   invalid period specified
                    ERROR = 1
                  ELSE IF (TIMCHK(START(1,I),LSDAT).EQ.1 .OR.
     $                     TIMCHK(END(1,I),LEDAT).EQ.-1) THEN
C                   start or end date out of range of available data
                    ERROR = 2
                  ELSE IF (TOTMON .LT. 12) THEN
C                   see if start/end mo. specified falls w/in mo.s to be tabled
                    STFLG = 0
                    ENDFLG = 0
                    DO 170 J = 0, TOTMON-1
                      IF (STMON+J .GT. 12) THEN
                        TSTMON = (STMON+J) - 12
                      ELSE
                        TSTMON = STMON + J
                      END IF
                      IF (START(2,I) .EQ. TSTMON) STFLG = 1
                      IF (END(2,I) .EQ. TSTMON) ENDFLG = 1
 170                CONTINUE
                    IF (.NOT. (STFLG.EQ.1 .AND. ENDFLG.EQ.1)) ERROR = 3
                  END IF
 180            CONTINUE
              END IF
            END IF
            DO 200 I= 1,DSNCNT
C             force start date to beginning of month
              START(3,I) = 1
              START(4,I) = 0
              START(5,I) = 0
              START(6,I) = 0
C             force end date to end of month
              END(3,I) = DAYMON(END(1,I),END(2,I))
              END(4,I) = 24
              END(5,I) = 0
              END(6,I) = 0
 200        CONTINUE
            IF (ERROR .NE. 0) THEN
C             error in user modification of start/end dates of data
              CALL ZWNSOP (I1,PTHNAM)
              IF (ERROR .EQ. 1) THEN
                SGRP = 26
              ELSE IF (ERROR .EQ. 2) THEN
                SGRP = 27
              ELSE
                SGRP = 28
              END IF
              CALL PRNTXT (MESSFL, SCLU, SGRP)
            END IF
          IF (ERROR.NE.0) GO TO 100
        ELSE
C         back to main Table menu
          IRET= 1
        END IF
      IF (IRET.EQ.2) GO TO 10
C
C     turn off 'Previous'
      CALL ZSTCMA (CMDID, I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   TBSPEC
     I                   (MESSFL,SCLU,DSNCNT,DSNBUF,
     M                    SIGDIG,DECIML,BAD,TITLE)
C
C     + + + PURPOSE + + +
C     Specify table characteristics such as data precision,
C     transformation, and table titles.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,DSNCNT,SIGDIG(DSNCNT),
     $            DECIML(DSNCNT),DSNBUF(DSNCNT)
      REAL        BAD(DSNCNT)
      CHARACTER*1 TITLE(48,DSNCNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     DSNCNT - count of data sets being tabled
C     DSNBUF - array of data set numbers
C     SIGDIG - array of significant digits for data sets being tabled
C     DECIML - array of decimal places for data sets being tabled
C     BAD    - array of bad value codes for data sets being tabled
C     TITLE  - array of table titles for data sets being tabled
C
C     + + + PARAMETERS + + +
      INTEGER     MXDSN
      PARAMETER  (MXDSN=300)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I1,SGRP,IRET,ILEN,RNUM,
     $            INUM,IVAL(3,MXDSN),CNUM,CVAL(1,MXDSN)
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZSTCMA, Q2INIT, Q2EDIT, Q2SETI, Q2SCTF
      EXTERNAL    Q2GETI, Q2GCTF, Q2SETR, Q2GETR
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
C     allow previous command
      I= 4
      CALL ZSTCMA (I,I1)
C
 5    CONTINUE
C       back here on 'oops' command
C       init screen parameters
        SGRP= 31
        CALL Q2INIT (MESSFL,SCLU,SGRP)
C
C       set initial values
        DO 10 I= 1,DSNCNT
C         fill in default values
          IVAL(1,I)= DSNBUF(I)
          IVAL(2,I)= SIGDIG(I)
          IVAL(3,I)= DECIML(I)
 10     CONTINUE
C       set integer fields
        INUM= 3
        CALL Q2SETI (INUM,DSNCNT,IVAL)
C       set real fields
        RNUM= 1
        CALL Q2SETR (RNUM,DSNCNT,BAD)
C       set text field (titles)
        CNUM= 1
        ILEN= 48
        CALL Q2SCTF (CNUM,ILEN,DSNCNT,TITLE)
C
C       let user make modifications
        CALL Q2EDIT (DSNCNT,
     O               IRET)
      IF (IRET.EQ.-1) GO TO 5

      IF (IRET.EQ.1) THEN
C       user wants to continue, get edited values
        CALL Q2GETI (INUM,DSNCNT,
     O               IVAL)
        CALL Q2GETR (RNUM,DSNCNT,
     O               BAD)
        DO 20 I= 1,DSNCNT
C         get edited values
          SIGDIG(I)= IVAL(2,I)
          DECIML(I)= IVAL(3,I)
 20     CONTINUE
C       get title values
        CALL Q2GCTF (CNUM,ILEN,DSNCNT,
     O               TITLE)
      END IF
C
C     turn off previous command
      I= 4
      CALL ZSTCMA (I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   TBSTAT
     I                    (MESSFL, SCLU, DATSET, ENDYR, TOTMON,
     M                     INIT)
C
C     + + + PURPOSE + + +
C     Give a status report on tabling.  A message is displayed to user
C     indicating which portion of data is currently being tabled.  Screen
C     window name indicates which data set is being tabled.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, DATSET, ENDYR, TOTMON, INIT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number of message file
C     DATSET - number of current data set
C     ENDYR  - year of last (rightmost) month in current table
C     TOTMON - number of months included in table
C     INIT   - flag sent to PMXUPD
C              1 - initial screen write, draw complete screen
C              2 - not initial screen write, therefore just update numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L2, L5, SGRP, JUST, OLEN, ILEN(2), IVAL(2)
      CHARACTER*1  CHRDSN(5), CHRMON(2)
      CHARACTER*5  DSNVAR
      CHARACTER*48 WNDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    INTCHR, CARVAR, LENSTR, PMXUPD, ZWNSET
C
C     + + + END SPECIFICATIONS + + +
C
      L2 = 2
      L5 = 5
C
C     convert dsn to string for use in screen window
      JUST = 1
      CALL INTCHR (DATSET, L5, JUST, OLEN, CHRDSN)
      CALL CARVAR (L5, CHRDSN, L5, DSNVAR)
      WNDNAM = 'DAily (ODa) tabling data set '//DSNVAR
      CALL ZWNSET (WNDNAM)
C
      IVAL(1) = TOTMON
      IVAL(2) = ENDYR
      JUST = 1
      CALL INTCHR (TOTMON, L2, JUST, OLEN, CHRMON)
      ILEN(1) = LENSTR (L2, CHRMON)
      ILEN(2) = 4
      IF (INIT .EQ. 0) THEN
C     initial screen write not just update
        INIT = 1
      ELSE
        INIT = 2
      END IF
      SGRP = 47
      CALL PMXUPD (MESSFL, SCLU, SGRP, INIT, ILEN, IVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETDAT
     I                    (MESSFL, SCLU, WDMSFL, DATSET, YRCNT,
     I                     START, END, NM, DTRAN,
     M                     SDATE, EDATE, TDATE,
     O                     QM, RETCOD)
C
C     + + + PURPOSE + + +
C     Get data to be tabled.  Data is modified if start/end dates (months)
C     of period of data don't correspond with start/end months of table.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMSFL, DATSET, YRCNT, START(6), END(6),
     $          NM, DTRAN, TDATE(6), SDATE(6), EDATE(6), RETCOD
      REAL      QM(366)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number of message file
C     WDMSFL - Fortran unit number of WDM file
C     DATSET - number of current data set
C     YRCNT  - counter of years being tabled for current data set
C     START  - starting date of data to be tabled
C     END    - ending date of data to be tabled
C     NM     - number of months to be included in table
C     DTRAN  - data transformation,
C              0 - Rate       2 - Maximum
C              1 - Total      3 - Minimum
C     SDATE  - starting date of current year being tabled
C     EDATE  - table ending date
C     TDATE  - ending date of current year being tabled
C     QM     - array of values to be tabled
C     RETCOD - status code returned from time-series get,
C              0 - all ok in data retrieval from WDM file
C            <>0 - prob. in getting data from WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I5, SGRP, TU, TS, XTRAVL, NVAL, MOSLFT, QFLG,
     $            INUM, IVAL(3), MAXLIN, SCINIT, WRTFLG
      REAL        NULL
C
C     + + + EXTERNALS + + +
      EXTERNAL    TIMDIF, PMXTXI, ZIPR, TSBWDS, TSBTIM, TSBGET
C
C     + + + END SPECIFICATIONS + + +
C
      I5 = 5
      TU = 4
      TS = 1
      NULL = -999.
C
C     get number of values to be tabled
      CALL TIMDIF (SDATE, TDATE, TU, TS,
     O             NVAL)
      IF (YRCNT .EQ. 1) THEN
C       1st year; check if need to modify data to be tabled
        IF (START(2) .LT. SDATE(2)) THEN
C         need to include data B4 that of 1st month in table
C         retrieve data for previous year
          SDATE(1) = SDATE(1) - 1
          TDATE(1) = TDATE(1) - 1
        END IF
        IF (END(2) .GT. EDATE(2)) THEN
C         need to include data following that of last mo.
C         in table; extend ending date by 1 year
          EDATE(1) = EDATE(1) + 1
        END IF
      END IF
C
      QFLG = 30
      CALL TSBWDS (WDMSFL,DATSET)
      CALL TSBTIM (TU,TS,DTRAN,QFLG)
      CALL TSBGET (SDATE,NVAL,
     O             QM,RETCOD)
C
C     calculate months left to be tabled
      CALL TIMDIF (SDATE, EDATE, I5, TS,
     O             MOSLFT)
      IF (RETCOD .NE. 0) THEN
C       problem in getting data
        INUM = 3
        IVAL(1) = NM
        IVAL(2) = TDATE(1)
        IVAL(3) = DATSET
        MAXLIN = 10
        SCINIT = 1
        WRTFLG = 0
        SGRP = 46
        CALL PMXTXI (MESSFL, SCLU, SGRP, MAXLIN, SCINIT,
     I               WRTFLG, INUM, IVAL)
      END IF
C
C     some parts of table may need blanking out
      IF (YRCNT.EQ.1 .AND. SDATE(2).NE.START(2)) THEN
C       "blank out" data in table which precedes START
        CALL TIMDIF (SDATE, START, TU, TS,
     O               XTRAVL)
        CALL ZIPR (XTRAVL, NULL, QM)
      ELSE IF (MOSLFT .EQ. NM) THEN
C       last table
        IF (EDATE(2) .NE. END(2)) THEN
C         "blank out" data in table following END
          CALL TIMDIF (END, EDATE, TU, TS,
     O                 XTRAVL)
          CALL ZIPR (XTRAVL, NULL, QM(NVAL-XTRAVL+1))
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTDTBL
     I                    (WDMFL,FOUT,MESSFL,SCLU,DSN,SYR,SMO,NM,
     I                     QM,SIGDIG,DECPLA,TITLE,FLOWFG,BAD)
C     + + + PURPOSE + + +
C     Produce a table on the output file of daily values.
C     The table has days for rows and months for columns.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMFL,FOUT,MESSFL,SCLU,SYR,SMO,DSN,NM,
     $            DECPLA,SIGDIG,FLOWFG
      REAL        QM(366), BAD
      CHARACTER*1 TITLE(48)
C
C     + + + ARGUMENT DEFINITION + + +
C     WDMFL  - Fortran unit number for WDM file
C     FOUT   - Fortran unit number for print file
C     MESSFL - Fortran unit number for main message file
C     SCLU   - cluster number on message file
C     DSN    - dataset number
C     SYR    - starting year
C     SMO    - starting month that defines type of year
C     NM     - number of months to use
C     QM     _ buffer of annual flow values
C     SIGDIG - number of significant digits
C     DECPLA - number of decimal places
C     TITLE  - title indicating type of data being tabled
C     FLOWFG - flag indicating if data type if flow,
C              0 - something other than flow
C              1 - data type is flow
C     BAD    - bad value flag, uses "-" in table for value
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I, J, K, L1, L2, L3, L4, L12, L16, L52, L80, L132, M,
     &            N, SGRP, SAIND, SALEN, IPOS, ILEN, OLEN, JUST,
     &            ISTN, CENTER, NSIZE, RETCOD, TU, TS, LINE, DCP, IY,
     &            TXTLEN, SDATE(6), TDATE(6)
      REAL        AREA, ZERO, SDAY(12), SUM(12), X, SMALL, LARGE,
     &            SMIN(12), SMAX(12), SX, BADTOL
      CHARACTER*1  OBUF(132), CTMP(20), BLNK, TBUF(80),DASH(1),CDAY(4),
     &             CHRMON(2), MONVR1
      CHARACTER*2  MONVR2
      CHARACTER*31 TEXT1
      CHARACTER*32 TEXT2
      CHARACTER*52 FULTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG10, ABS
C
C     + + + FUNCTIONS + + +
      INTEGER     STRLNX, DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZIPC, CHRCHR, GETTXT, INTCHR, DECCHX, WDBSGC, WDBSGI
      EXTERNAL    STRLNX, WDBSGR, LFTSTR, ZIPR, TIMDIF, DAYMON
      EXTERNAL    CARVAR, CVARAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA    CDAY/' ','D','a','y'/
      DATA  L1, L2, L3, L4, L12, L16, L52, L80, L132
     #     / 1,  2,  3,  4,  12,  16,  52,  80,  132/
      DATA  BLNK/' '/,   DASH/'-'/,  ZERO/0.0/
      DATA  SMALL/-1.0E9/,  LARGE/1.0E9/
      DATA  SDATE/0,0,1,0,0,0/,  TDATE/0,0,0,24,0,0/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('1')
 2001 FORMAT(//)
 2002 FORMAT(132A1)
C
C     + + + END SPECIFICATIONS + + +
C
      SDATE(1) = SYR
      SDATE(2) = SMO
      TU = 4
      TS = 1
C     size of field
      NSIZE = 10
      CENTER = (NM*NSIZE+10)/2
      JUST = 1
C     set tolerance for bad value code
      IF (ABS(BAD) .GT. 0.1) THEN
        BADTOL = 0.00001*ABS(BAD)
      ELSE
        BADTOL = 0.00001
      END IF
C
C     get station number
      SAIND = 2
      SALEN = 16
      CALL WDBSGC (WDMFL,DSN,SAIND,SALEN,
     O             CTMP,RETCOD)
      OLEN = STRLNX (L16,CTMP)
      IF (RETCOD .NE. 0) THEN
C       try other form
        SALEN = 1
        SAIND = 51
        CALL WDBSGI (WDMFL,DSN,SAIND,SALEN,
     O               ISTN,RETCOD)
        CALL INTCHR (ISTN,L16,JUST,OLEN,CTMP)
      END IF
      WRITE (FOUT,2000)
      WRITE (FOUT,2001)
      CALL ZIPC (L132, BLNK, OBUF)
C     get STATION NUMBER
      IPOS = CENTER - (15+OLEN)/2
      IF (IPOS .LT. 2) IPOS = 2
      OLEN = 15
      SGRP = 63
      CALL GETTXT (MESSFL,SCLU,SGRP,OLEN,
     O             OBUF(IPOS))
      IPOS = IPOS + OLEN + 1
      CALL CHRCHR (L16,CTMP,OBUF(IPOS))
      WRITE (FOUT,2002) OBUF
C
C     write title
C     get ending month
      M = SMO + NM - 1
      IY = SYR
      IF (M .GT. 12) THEN
        M = M - 12
        IY = IY + 1
      END IF
      IPOS = 1
      CALL ZIPC (L80,BLNK,TBUF)
      CALL INTCHR (NM, L2, JUST, OLEN, CHRMON)
      IF (OLEN .EQ. 1) THEN
        CALL CARVAR (L1, CHRMON, L1, MONVR1)
        TEXT1 = 'Table of daily values for the '//MONVR1
        FULTXT = TEXT1//'-month period ending  '
      ELSE
        CALL CARVAR (L2, CHRMON, L2, MONVR2)
        TEXT2 = 'Table of daily values for the '//MONVR2
        FULTXT = TEXT2//'-month period ending  '
      END IF
      CALL CVARAR (L52, FULTXT, L52, TBUF)
      TXTLEN = STRLNX (L52, TBUF)
      IPOS = IPOS + TXTLEN + 1
      OLEN = 9
      SGRP = 50 + M
      CALL GETTXT (MESSFL,SCLU,SGRP,OLEN,
     O             TBUF(IPOS))
      IPOS = IPOS + OLEN + 1
      CALL INTCHR (IY,L4,JUST,OLEN,TBUF(IPOS))
      OLEN = OLEN + IPOS
      CALL ZIPC (L132,BLNK,OBUF)
      IPOS = CENTER - OLEN/2
      IF (IPOS .LT. 2) IPOS = 2
      CALL CHRCHR (OLEN,TBUF,OBUF(IPOS))
      WRITE (FOUT,2002) OBUF
C
C     output title describing type of data
      ILEN = 48
      OLEN = STRLNX(ILEN,TITLE)
      IPOS = CENTER - OLEN/2
      IF (IPOS .LT. 2) IPOS = 2
      CALL ZIPC (L132,BLNK,OBUF)
      CALL CHRCHR (OLEN,TITLE,OBUF(IPOS))
      WRITE (FOUT,2002) OBUF
C
C     get station description
      CALL ZIPC (L80, BLNK, TBUF)
      SAIND = 45  
      SALEN = 48
      CALL WDBSGC (WDMFL,DSN,SAIND,SALEN,
     O             TBUF,RETCOD)
      IF (RETCOD .NE. 0) THEN
        SAIND = 10
        SALEN = 80
        CALL WDBSGC (WDMFL,DSN,SAIND,SALEN,
     O               TBUF,RETCOD)
        IF (RETCOD .NE. 0) THEN
C         couldn't get station description, init to blanks
          CALL ZIPC (L80, BLNK, TBUF)
        END IF
      END IF
C     put station description
      CALL ZIPC (L132,BLNK,OBUF)
      CALL LFTSTR(L80,TBUF)         
      ILEN = STRLNX(L80,TBUF)
      IPOS = CENTER - ILEN/2
      IF (IPOS .LT. 2) IPOS = 2
      CALL CHRCHR (ILEN,TBUF,OBUF(IPOS))
      WRITE (FOUT,2002) OBUF
C
C     get drainage area
      SAIND = 11
      SALEN = 1
      CALL WDBSGR (WDMFL,DSN,SAIND,SALEN,
     O             AREA,RETCOD)
      IF (RETCOD .NE. 0) THEN
        SAIND = 43
        CALL WDBSGR (WDMFL,DSN,SAIND,SALEN,
     O              AREA,RETCOD)
        IF (RETCOD .NE. 0) THEN
C         couldn't get area, set to undefined
          AREA = -999.
        END IF
      END IF
      IF (AREA .GT. 0.0) THEN
C       put drainage area
        CALL ZIPC (L132,BLNK,OBUF)
        CALL ZIPC (L80,BLNK,TBUF)
        IPOS = 1
        OLEN = 14
        SGRP = 67
        CALL GETTXT (MESSFL,SCLU,SGRP,OLEN,
     O               TBUF(IPOS))
        IPOS = IPOS + OLEN + 1
        ILEN = 14
        CALL DECCHX (AREA,ILEN,SIGDIG,DECPLA,
     O               TBUF(IPOS))
        CALL LFTSTR (ILEN,TBUF(IPOS))
        OLEN = STRLNX(L80,TBUF)
        IPOS = CENTER-OLEN/2
        IF (IPOS .LT. 2) IPOS = 2
        CALL CHRCHR (OLEN,TBUF,OBUF(IPOS))
        WRITE (FOUT,2002) OBUF
      END IF
      WRITE (FOUT,2001)
C
C     put header for columns
      CALL ZIPC (L132,BLNK,OBUF)
      IPOS = 4
      CALL CHRCHR (L4,CDAY,OBUF(IPOS))
      IPOS = IPOS + 8
      JUST = 0
      DO 20 I = 1,NM
        M = SMO + I - 1
        IF (M .GT. 12) M = M - 12
        CALL ZIPC (NSIZE, BLNK, CTMP)
        OLEN = 9
        SGRP = 50 + M
        CALL GETTXT (MESSFL,SCLU,SGRP,OLEN,
     O                CTMP)
        J = IPOS + NSIZE - OLEN
        CALL CHRCHR (OLEN,CTMP,OBUF(J))
        IPOS = IPOS + NSIZE
 20   CONTINUE
      WRITE (FOUT,2002) OBUF
C
      CALL ZIPC (L132,BLNK,OBUF)
      IPOS = 5
      CALL ZIPC (L3,DASH(1),OBUF(IPOS))
      IPOS = IPOS + 9
      N = NSIZE - 2
      DO 22 I = 1,NM
        CALL ZIPC (N,DASH(1),OBUF(IPOS))
        IPOS = IPOS + NSIZE
 22   CONTINUE
      WRITE (FOUT,2002) OBUF
C
C     write out table and compute sums
      CALL ZIPR (L12,ZERO,SDAY)
      CALL ZIPR (L12,ZERO,SUM)
      CALL ZIPR (L12,SMALL,SMAX)
      CALL ZIPR (L12,LARGE,SMIN)
      DO 60 K = 1,31
        IPOS = 4
        CALL ZIPC (L132,BLNK,OBUF)
        CALL INTCHR (K,L4,JUST,OLEN,OBUF(IPOS))
        IPOS = IPOS + 8
        TDATE(1) = SYR
        DO 50 N = 1,NM
          M = SMO + N - 1
          IF (M .GT. 12) THEN
            M = M - 12
            TDATE(1) = SYR + 1
          END IF
          TDATE(2) = M
          TDATE(3) = K
          IF (K .LE. DAYMON(TDATE(1),TDATE(2))) THEN
            CALL TIMDIF (SDATE, TDATE, TU, TS, I)
C           not end of month yet
            IF (ABS(QM(I) -BAD) .GT. BADTOL) THEN
              CALL DECCHX (QM(I),NSIZE,SIGDIG,DECPLA,
     O                     OBUF(IPOS))
              SUM(N) = SUM(N) + QM(I)
              SDAY(N) = SDAY(N) + 1.0
              IF (QM(I).GT.SMAX(N)) THEN
C               new max value
                SMAX(N) = QM(I)
              END IF
              IF (QM(I).LT.SMIN(N)) THEN
C               new min value
                SMIN(N) = QM(I)
              END IF
            ELSE
C             bad value, put dash
              J = IPOS + NSIZE - 2
              CALL CHRCHR (L1,DASH,OBUF(J))
            END IF
          END IF
          IPOS = IPOS + NSIZE
 50     CONTINUE
        WRITE (FOUT,2002) OBUF
 60   CONTINUE
C
C     write summary information
      CALL ZIPC (L132,BLNK,OBUF)
      IPOS = 12 + 2
      N = NSIZE - 2
      DO 70 I = 1,NM
        CALL ZIPC (N,DASH(1),OBUF(IPOS))
        IPOS = IPOS + NSIZE
 70   CONTINUE
      WRITE (FOUT,2002) OBUF
C
      DCP = DECPLA
      DO 90 LINE = 1,5
        IF (LINE .LE. 4 .OR. (LINE.EQ.5 .AND. AREA.GT.0.0 .AND.
     $      FLOWFG.EQ.1)) THEN
          CALL ZIPC (L132,BLNK,OBUF)
          SGRP = 68
          IPOS = 12
          DO 80 I = 1,NM
            IF (SDAY(I) .GT. 0.01) THEN
              IF (LINE .EQ. 1) THEN
C               total
                SGRP = 68
                SX = SUM(I)
              ELSE IF (LINE .EQ. 2) THEN
C               mean
                SGRP = 69
                SX = SUM(I)/SDAY(I)
              ELSE IF (LINE .EQ. 3) THEN
C               minimum
                SGRP = 70
                SX = SMIN(I)
              ELSE IF (LINE .EQ. 4) THEN
C               maximum
                SGRP = 71
                SX = SMAX(I)
              ELSE IF (LINE .EQ. 5) THEN
C               inches
                SGRP = 72
                X = AREA*26.889
                DCP = DECPLA + ALOG10(X) - 1
                IF (DCP .GT. 4) DCP = 4
                SX = SUM(I)/X
              END IF
              CALL DECCHX (SX,NSIZE,SIGDIG,DCP,
     O                     OBUF(IPOS))
            ELSE
C             no good data in month
              J = IPOS + NSIZE - 2
              CALL CHRCHR (L1, DASH, OBUF(J))
            END IF
            IPOS = IPOS + NSIZE
 80       CONTINUE
          IPOS = 3
          OLEN = 7
          CALL GETTXT (MESSFL,SCLU,SGRP,OLEN,
     O                 OBUF(IPOS))
          WRITE(FOUT,2002) OBUF
        END IF
 90   CONTINUE
C
      RETURN
      END

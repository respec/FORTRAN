C
C
C
      SUBROUTINE   TSLEGN
     I                   (MESSFL,SCLU,PTHNAM,WDMSFL,NUMDSN,DSN,TDSN,
     I                    TITLE,NUMHDR,HEADR)
C
C     + + + PURPOSE + + +
C     Generate time-series listing of specified datasets
C     with listing parameters as set in common block CTSLST.
C     Allow editing of time-series values in first column.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,WDMSFL,NUMDSN,DSN(NUMDSN),TDSN,NUMHDR
      CHARACTER*1 TITLE(78),HEADR(250,NUMHDR)
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     PTHNAM - character string of path of options selected to get here
C     WDMSFL - Fortran unit number of WDM file
C     NUMDSN - number of data sets in buffer
C     DSN    - array of data-set numbers in buffer
C     TDSN   - number of data set for temporarily storing edited data values
C     TITLE  - character array for title of listing
C     NUMHDR - number of rows of headers
C     HEADR  - character array of header info
C
C     + + + PARAMETERS + + +
      INCLUDE 'pbfmax.inc'
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctslst.inc'
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,I0,I1,I6,SGRP,INUM,INIT,IWRT,ILEN,
     $            IVAL(2),DATE(6),TDATE(6),PAUSE,GETNUM,COUNT,
     $            SAIND,IPOS,LENBUF,ID,OLEN,LRET,FLAG,NVAL,DROW,
     $            NUMB,NPOS,DTFG(MAXD),FIRST,SUMFLG,IRET,RETCOD,
     $            DPOS(300),LSDAT(6),QUALFG,WRTFLG
      REAL        SCNT(LSUM),SUM(LSUM),DATA(MAXD),ZERO,ODATA(MAXD),
     $            RTMP
      CHARACTER*1 BLNK(1),DASH(1),TBUFF(250)
C
C     DPOS should be dimensioned by the max number of rows
C          allowed on an AIDE data screen.
C     SUM(49-60) = HOURLY SUMS FOR UP TO 6 DATASETS
C     SUM(37-48) = DAILY SUMS FOR UP TO 6 DATASETS
C     SUM(25-36) = MONTHLY SUMS FOR UP TO 6 DATASETS
C     SUM(13-24) = ANNUAL SUMS FOR UP TO 6 DATASETS
C     SUM( 1-12) = GRAND TOTAL FOR UP TO 6 DATASETS
C     SCNT(49-60) = COUNT OF VALUES FOR HOURLY TOTAL
C     SCNT(37-48) = COUNT OF VALUES FOR DAILY TOTAL
C     SCNT(25-36) = COUNT OF VALUES FOR MONTHLY TOTAL
C     SCNT(13-24) = COUNT OF VALUES FOR ANNUAL TOTAL
C     SCNT( 1-12) = COUNT OF VALUES FOR GRAND TOTAL
C
C     + + + FUNCTIONS + + +
      INTEGER    LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   LENSTR, TSBWDS, TSBTIM, TSBGET, WDBSGR, WTEGRP
      EXTERNAL   ZIPC, ZIPR, PRNTXT, PRTLIN, PRSUED, ZSTCMA, ZGTRET
      EXTERNAL   TIMADD, TIMCNV, TIMDIF, COPYI, ZBLDWR, PRTLNO
      EXTERNAL   GETTXT, COPYR, PMXTXI, ZIPI, ZWNSOP
      EXTERNAL   Q2INIT, TSGDAT, ZMNSST, TSDROW, TSLWRT
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      I1   = 1
      I6   = 6
      ZERO = 0.0
      CALL ZIPR (LSUM,ZERO,SUM)
      CALL ZIPR (LSUM,ZERO,SCNT)
      QUALFG = 30
      BLNK(1)= ' '
      DASH(1)= '-'
      WRTFLG = 0
      SUMFLG = 0
      DO 10 I= 1,SNUM
C       check for summaries in use
        IF (TOTAVE(I).GT.0) THEN
C         at least one summary desired
          SUMFLG = 1
        END IF
 10   CONTINUE
      FIRST= 0
C     generate the listing
      INIT= 1
C     interrupt now allowed
      I= 16
      CALL ZSTCMA (I,I1)
C     but currently, previous is not
      I= 4
      CALL ZSTCMA (I,I0)
C     compute and print number of lines to expect
      CALL TIMDIF (SDATE,EDATE,TUNITS,TSTEP,NVAL)
C     init current date to start date
      CALL COPYI (I6,SDATE,DATE)
C     be sure no summaries ON less or equal to time units
      I= TUNITS- 2
      J= 8- TUNITS
      IF (I.GT.0 .AND. J.GT.0) THEN
C       init needed summaries to 0
        CALL ZIPI (I,I0,TOTAVE(J))
      END IF
C     show number of values to be retrieved
C     always display message right away
      IWRT= 1
      CALL ZWNSOP (I1,PTHNAM)
      SGRP= 61
      IVAL(1)= NVAL
      CALL PMXTXI (MESSFL,SCLU,SGRP,I1,INIT,IWRT,I1,IVAL)
C
      PAUSE = 0
      COUNT = 0
      LENBUF= BUFMAX/NUMDSN
C     COUNT is cumulated num of values from WDM file
C     GETNUM is num values got each time from WDM file
C
C     title of listing
      I = 78
      IF (LENSTR(I,TITLE) .GT. 0) THEN
        CALL ZBLDWR (I,TITLE,I1,-I1,J)
        CALL ZBLDWR (I1,BLNK,INIT,-I1,J)
        PAUSE = 2
      END IF
C
C     definition of - and *
      SGRP= 62
      I   = 80
      CALL GETTXT (MESSFL,SCLU,SGRP,
     M             I,
     O             TBUFF)
      CALL ZBLDWR (I,TBUFF,INIT,-I1,J)
      INIT= 0
      I= 1
      CALL ZBLDWR (I,BLNK,INIT,-I1,J)
C
C     determine how much of date string to list
      IF (TUNITS .LE. 3) THEN
C       eliminate sec only
        NUMB= 3
        NPOS= 19
      ELSE IF (TUNITS .EQ. 4) THEN
C       eliminate hour:...:sec
        NUMB= 8
        NPOS= 14
      ELSE IF (TUNITS .EQ. 5) THEN
C       eliminate day:...:sec
        NUMB= 11
        NPOS= 11
      ELSE
C       eliminate all but year
        NUMB= 15
        NPOS= 7
      END IF
C
C     output headers, start with dashed line
      CALL ZIPC (WIDTH,DASH,TBUFF)
      TBUFF(1)= BLNK(1)
      CALL ZBLDWR (WIDTH,TBUFF,INIT,-I1,J)
      PAUSE = PAUSE + 3
      IF (NUMHDR.GT.0) THEN
C       headers exist to output
        DO 50 I = 1,NUMHDR
          CALL ZBLDWR (WIDTH,HEADR(1,I),INIT,-I1,J)
 50     CONTINUE
C       finish header with another dashed line
        CALL ZBLDWR (WIDTH,TBUFF,INIT,-I1,J)
        PAUSE = PAUSE + NUMHDR + 1
      END IF
C
C     init for editing
      CALL ZMNSST
      SGRP = 70+ NUMDSN
      CALL Q2INIT (MESSFL,SCLU,SGRP)
C     init data row position pointer for all available data rows
      CALL ZIPI (LINES,I0,DPOS)
      DROW  = 0
C
 100  CONTINUE
C       begin loop to find and dump data in buffer.
C       get an optimal buffer size based on smallest group size
        CALL WTEGRP (DATE,GRPMAX,TDATE)
        CALL TIMDIF (DATE,TDATE,TUNITS,TSTEP,GETNUM)
      write (99,*) 'DATE,TDATE,GETNUM',DATE,TDATE,GETNUM
        IF (GETNUM .GT. LENBUF) THEN
C         buffer size limited
          GETNUM = LENBUF
C         reset end time
          CALL TIMADD (DATE,TUNITS,TSTEP,GETNUM,TDATE)
        ELSE IF (GETNUM .LT. 1) THEN
C         must be going over group boundary
          GETNUM = 1
          CALL TIMADD (DATE,TUNITS,TSTEP,GETNUM,TDATE)
        END IF
        IF (COUNT+GETNUM .GT. NVAL) THEN
C         last batch
          GETNUM = NVAL - COUNT
        END IF
      write (99,*) 'GETNUM is',GETNUM
C
C       fill buffer from WDM file
        IPOS = 1
        IRET = 1
        ID   = 0
 200    CONTINUE
C         for each data set in buffer
          ID= ID+ 1
C         get when print timestep and dataset timestep are same
          CALL TSBWDS (WDMSFL,DSN(ID))
          CALL TSBTIM (TUNITS,TSTEP,TRANS(ID),QFLG)
          CALL TSBGET (DATE,GETNUM,
     O                 YX(IPOS),RETCOD)
C         save start date for group retrieved
          CALL COPYI (I6,DATE,LSDAT)
          IF (RETCOD.NE.0) THEN
C           problem retrieving data for current data set
C           init portion of buffer to undefined
            SAIND= 32
            CALL WDBSGR (WDMSFL,DSN(ID),SAIND,I1,
     O                   RTMP,LRET)
            IF (LRET.NE.0) THEN
C             could not get value for attribute, set to default value
              RTMP = -999999.
            END IF
            CALL ZIPR (GETNUM,RTMP,YX(IPOS))
            IF (COUNT.EQ.0) THEN
C             tell user about problem 1st time through
              CALL ZWNSOP (I1,PTHNAM)
              INUM   = 2
              IVAL(1)= DSN(ID)
              IVAL(2)= RETCOD
              SGRP   = 67
              CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,I0,INUM,IVAL)
              INIT= 1
C             get user exit command value
              CALL ZGTRET (IRET)
              IF (IRET.EQ.1) THEN
C               redisplay headers
                PAUSE = 0
C               start with title
                I = 78
                IF (LENSTR(I,TITLE) .GT. 0) THEN
                  CALL ZBLDWR (I,TITLE,INIT,-I1,J)
                  INIT = 0
                  CALL ZBLDWR (I1,BLNK,INIT,-I1,J)
                  PAUSE = 2
                END IF
C               definition of - and *
                SGRP= 62
                I   = 80
                CALL GETTXT (MESSFL,SCLU,SGRP,
     M                       I,
     O                       TBUFF)
                CALL ZBLDWR (I,TBUFF,INIT,-I1,J)
                INIT= 0
                CALL ZBLDWR (I1,BLNK,INIT,-I1,J)
                CALL ZIPC(WIDTH,DASH,TBUFF)
                TBUFF(1)= BLNK(1)
                CALL ZBLDWR (WIDTH,TBUFF,INIT,-I1,J)
                PAUSE = PAUSE + 3
                IF (NUMHDR.GT.0) THEN
C                 headers exist to output
                  DO 250 I = 1,NUMHDR
                    CALL ZBLDWR (WIDTH,HEADR(1,I),INIT,-I1,J)
 250              CONTINUE
C                 finish header with another dashed line
                  CALL ZBLDWR (WIDTH,TBUFF,INIT,-I1,J)
                  PAUSE = PAUSE + NUMHDR + 1
                END IF
C               re-init for editing
                CALL ZMNSST
                SGRP = 70+ NUMDSN
                CALL Q2INIT (MESSFL,SCLU,SGRP)
              END IF
            END IF
          END IF
          IPOS = IPOS + GETNUM
        IF (ID.LT.NUMDSN .AND. IRET.EQ.1) GO TO 200
C
        IF (IRET.EQ.1) THEN
C         unload buffer to terminal
          INUM = 1
 300      CONTINUE
C           increment date with wdms convention
            CALL TIMADD (DATE,TUNITS,TSTEP,I1,TDATE)
            CALL COPYI (I6,TDATE,DATE)
C           convert to date convention for printing
            CALL TIMCNV (DATE)
C
            IF (TORF.EQ.1 .AND. PAUSE.GE.LINES) THEN
C             allow user to edit 1st column of values
              CALL TSGDAT (DROW,DPOS,
     O                     IRET)
              PAUSE= 0
              DROW = 0
C             need to initialize screen next time
              INIT = 1
C             re-init for next screen
              SGRP = 70+ NUMDSN
              CALL Q2INIT (MESSFL,SCLU,SGRP)
C             init data row position pointer for all available data rows
              CALL ZIPI (LINES,I0,DPOS)
            END IF
C
            IF (IRET.EQ.1) THEN
              FLAG = 0
              DO 350 ID = 1,NUMDSN
                K = (ID-1)*GETNUM + INUM
                DATA(ID) = YX(K)
C               check for value to be output
                IF (LORS .EQ. 1) THEN
C                 output data values within expected range
                  IF (DATA(ID).GT.THRSH(1) .AND.
     $                DATA(ID).LT.THRSH(2)) FLAG = 1
                ELSE
C                 output data values outside expected range
                  DTFG(ID) = 0
                  IF (DATA(ID).LT.THRSH(1)  .OR.
     $                DATA(ID).GT.THRSH(2)) THEN
                    FLAG = 1
                    DTFG(ID) = 1
                  END IF
                  IF (FIRST .EQ. 0) THEN
C                   first time
                    CALL COPYR (MAXD,DATA,ODATA)
                    FIRST = 1
                  END IF
                  RTMP= ODATA(ID)
                  IF (ABS(RTMP).GT.1.0E-30) THEN
C                   not going to divide by 0
                    IF ((DATA(ID)-RTMP)/RTMP.GT.THRSH(3)) THEN
                      FLAG    = 1
                      DTFG(ID)= 1
                    END IF
                  ELSE
C                   can't divide by zero, so flag if values not identical
                    IF (ABS(DATA(ID) - ODATA(ID)) .GT. 1.0E-20) THEN
                      FLAG    = 1
                      DTFG(ID)= 1
                    END IF
                  END IF
                  IF (ABS(DATA(ID) - ODATA(ID)) .GT. THRSH(4)) THEN
                    FLAG = 1
                    DTFG(ID) = 1
                  END IF
                END IF
                FIRST = 1
 350          CONTINUE
C
              CALL COPYR (MAXD,DATA,ODATA)
C
              IF (FLAG.EQ.1) THEN
C               set data values for this row
                IF (LORS .EQ. 1) THEN
C                 list 'good' data
                  CALL PRTLIN (DATE,NUMDSN,DATA,WIDTH,
     I                         SGFD,DPLA,THRSH,
     O                         OLEN,TBUFF)
                ELSE
C                 list 'bad' data
                  CALL PRTLNO (DATE,NUMDSN,DATA,WIDTH,
     I                         SGFD,DPLA,DTFG,
     O                         OLEN,TBUFF)
                END IF
C               remove portion of date string not needed
                CALL ZIPC (NUMB,BLNK,TBUFF(NPOS))
C               add another data row
                DROW= DROW+ 1
C               build edit buffer values for screen
                CALL TSDROW (DATA(1),DROW,NUMDSN,OLEN,TBUFF,
     I                       WIDTH,NPOS,SGFD,DPLA,LORS,THRSH,DTFG(1))
                IF (TDSN.GT.0) THEN
C                 store offset in data-set array for this data value
                  DPOS(DROW)= INUM
                END IF
                PAUSE = PAUSE+ 1
C
                IF (SUMFLG .EQ. 1) THEN
C                 compute sums
                  DO 370 K = 1,SNUM
                    IF (TOTAVE(K).GT.0) THEN
C                     computing this summary
                      DO 360 ID = 1,NUMDSN
                        J = (K-1)*MAXD + ID
                        IF (DATA(ID).LE.THRSH(2) .AND.
     1                      DATA(ID).GE.THRSH(1))  THEN
                          SUM(J) = SUM(J) + DATA(ID)
                          IF (TOTAVE(K).EQ.2) THEN
C                           performing average, update count of values
                            SCNT(J) = SCNT(J) + 1.0
                          END IF
                        END IF
 360                  CONTINUE
                    END IF
 370              CONTINUE
                END IF
              END IF
C
C             print sums if requested
              IF (SUMFLG .EQ. 1) THEN
C               some summary requested
                CALL PRSUED (MESSFL,SCLU,NUMDSN,ENDMO,WIDTH,DATE,
     I                       TOTAVE,SGFD,DPLA,THRSH,LSUM,
     I                       SUM,SCNT,LINES,
     O                       PAUSE,DROW,DPOS,IRET)
              END IF
C
CPRH            ELSE IF (IRET.EQ.2) THEN
CPRHC             back up one screen of values
CPRH              CALL TIMADD (DATE,TUNITS,TSTEP,-(2*LINES+1),
CPRH                           TDATE)
CPRH              CALL COPYI (I6,TDATE,DATE)
CPRH              INUM= INUM- 2*LINES
CPRH              IRET= 1
CPRH              IF (INUM.LT.0) THEN
CPRHC               back up past start of this buffer of data
CPRH                IF (COUNT.EQ.0) THEN
CPRHC                 on first screen, prev same as interrupt
CPRH                  IRET= 7
CPRH                ELSE
CPRHC                 need to get previous buffer of data
CPRH                END IF
CPRH              END IF
            END IF
C
            INUM = INUM + 1
C           return to regular date convention
            CALL COPYI (I6,TDATE,DATE)
          IF (INUM.LE.GETNUM .AND. IRET.EQ.1) GO TO 300
C
C         set new date for next batch
          CALL COPYI (I6,TDATE,DATE)
C
          COUNT = COUNT + GETNUM
C
          IF (COUNT.LT.NVAL .AND. IRET.EQ.1) THEN
C           at end of data group in YX buffer and more to come
            IF (DROW.GT.0) THEN
C             allow user to edit 1st column of values
              CALL TSGDAT (DROW,DPOS,
     O                     IRET)
              PAUSE= 0
              DROW = 0
C             need to initialize screen next time
              INIT = 1
C             re-init for next screen
              SGRP = 70+ NUMDSN
              CALL Q2INIT (MESSFL,SCLU,SGRP)
C             init data row position pointer for all available data rows
              CALL ZIPI (LINES,I0,DPOS)
            END IF
C
            IF (TDSN.GT.0) THEN
C             store edited data on temp data set
              CALL TSLWRT (WDMSFL,DSN(1),TDSN,WRTFLG,
     I                     TUNITS,TSTEP,QUALFG,LSDAT,GETNUM,YX)
              WRTFLG= 1
            END IF
C
C           check for more data
            GETNUM= NVAL - COUNT
            IF (GETNUM.GT.LENBUF) THEN
C             need to get more than buffer can handle
              GETNUM = LENBUF
            END IF
          END IF
        END IF
C
      IF (COUNT.LT.NVAL .AND. IRET.EQ.1) GO TO 100
C
      IF (IRET.EQ.1) THEN
C       continue listing
        IF (TOTAVE(1).GT.0) THEN
C         print grand total
          IF (TOTAVE(1).EQ.2) THEN
C           generating average
            DO 400 I = 1,MAXD
              IF (SCNT(I) .GT. 0.0) THEN
                SUM(I) = SUM(I)/SCNT(I)
              ELSE
                SUM(I) = -1.0E20
              END IF
 400        CONTINUE
          END IF
C         make sure theres room for grand total
          IF (PAUSE.GE.LINES) THEN
C           summary line would go beyond page boundary, do on next screen
C           allow user to edit 1st column of values
            CALL TSGDAT (DROW,DPOS,
     O                   IRET)
C           need to initialize screen next time
            INIT = 1
          END IF
          IF (IRET.EQ.1) THEN
C           user wants to continue
            CALL PRTLIN (DATE,NUMDSN,SUM,WIDTH,SGFD,DPLA,THRSH,
     O                   ILEN,TBUFF)
            SGRP= 64
            J   = 22
            CALL GETTXT (MESSFL,SCLU,SGRP,
     M                   J,
     O                   TBUFF)
C           add another data row
            DROW= DROW+ 1
C           build edit buffer values for screen
            CALL TSDROW (SUM(1),DROW,NUMDSN,ILEN,TBUFF,
     I                   WIDTH,J+1,SGFD,DPLA,LORS,THRSH,DTFG(1))
C           allow user to edit 1st column of values
            CALL TSGDAT (DROW,DPOS,
     O                   IRET)
          END IF
        ELSE
C         at end and no grand total coming
C         allow user to edit 1st column of values
          CALL TSGDAT (DROW,DPOS,
     O                 IRET)
        END IF
      END IF
C
      IF (TDSN.GT.0) THEN
C       store edited data on temp data set
        IF (WRTFLG.EQ.0) THEN
C         no write to temp data set yet, set flag for 1st and only write
          WRTFLG= -2
        ELSE
C         final write to temp data set
          WRTFLG= 2
        END IF
        CALL TSLWRT (WDMSFL,DSN(1),TDSN,WRTFLG,
     I               TUNITS,TSTEP,QUALFG,LSDAT,GETNUM,YX)
      END IF
C
C     turn off interrupt
      I= 16
      CALL ZSTCMA (I,I0)
C     finished listing data
      CALL ZWNSOP (I1,PTHNAM)
      SGRP = 66
      CALL PRNTXT (MESSFL,SCLU,SGRP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDROW
     I                   (DVAL,DROW,NUMDSN,OLEN,TBUFF,
     I                    WIDTH,NPOS,SGFD,DPLA,LORS,THRSH,DTFG)
C
C     + + + PURPOSE + + +
C     Set 2-d parm screen field values for time-series listing
C     with editing of the first field allowed.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DROW,NUMDSN,OLEN,WIDTH,NPOS,SGFD,DPLA,LORS,DTFG
      REAL        DVAL,THRSH(2)
      CHARACTER*1 TBUFF(OLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DVAL   - data value for column being edited
C     DROW   - data row number on 2-d screen
C     NUMDSN - number of data sets being listed
C     OLEN   - length of text buffer
C     TBUFF  - text buffer containing date string and data values
C     WIDTH  - width available for listing
C     NPOS   - position in date string after which info is superfluous
C     SGFD   - significant digits to display for data value
C     DPLA   - decimal places to display for data value
C     LORS   - List/Screen values flag
C              1 - List, 2 - Screen
C     THRSH  - threshhold values for valid data
C     DTFG   - valid data value flag,
C              0 - value ok, 1 - invalid value
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1,CLEN(1),ILEN,IPOS
      REAL       RVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2SCTX, Q2SRBR, DECPRC
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C
C     do data check and build edit buffer values for screen
      IF ((LORS.EQ.1 .AND.
     $    (DVAL.LT.THRSH(1) .OR. DVAL.GT.THRSH(2))) .OR.
     $    (LORS.EQ.2 .AND. DTFG.EQ.0)) THEN
C       data out of range, set to 'none'
        RVAL(1)= -999.0
      ELSE
C       good value
        RVAL(1)= DVAL
      END IF
C     set precision and decimal places
      CALL DECPRC (SGFD,DPLA,
     M             RVAL(1))
C     set date string for this data row
      CLEN(1)= NPOS - 1
      CALL Q2SCTX (I1,CLEN,CLEN(1),I1,I1,DROW,TBUFF)
C     set data values for this data row
      CALL Q2SRBR (I1,I1,I1,DROW,RVAL(1))
      IF (NUMDSN.GT.1) THEN
C       set remaining data fields as text strings
        ILEN = (WIDTH-22)/NUMDSN
        IF (ILEN.GT.12) ILEN = 12
        IPOS = ILEN+ 23
        CLEN(1)= OLEN- IPOS+ 1
        I = 2
        CALL Q2SCTX (I1,CLEN,CLEN(1),I,I1,DROW,TBUFF(IPOS))
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRSUED
     I                    (MESSFL,SCLU,NUMDSN,ENDMO,WDTH,DATE,TOTAVE,
     I                     SGFD,DPLA,THRSH,LSUM,SUM,SCNT,LINES,
     M                     PAUSE,DROW,DPOS,IRET)
C
C     + + + PURPOSE + + +
C     Print a line of hourly, daily, annual or grand totals as requested.
C     Allow editing of the first column.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,NUMDSN,ENDMO,WDTH,DATE(6),TOTAVE(5),
     $          SGFD,DPLA,LSUM,LINES,PAUSE,DROW,DPOS(LINES),IRET
      REAL      THRSH(2),SUM(LSUM),SCNT(LSUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     NUMDSN - number of datasets to output
C     ENDMO  - end month for annual summary
C              (9-water year, 12-calendar year)
C     WDTH   - width of output
C     DATE   - date/time (yr, mo, dy, hr, mn, sc)
C     TOTAVE - array containing flags for requested summaries,
C              0 - none, 1 - summed, 2 - averaged
C              (2) - annual summary
C              (3) - monthly summary
C              (4) - daily summary
C              (5) - hourly summary
C     SGFD   - number of significant digits in output
C     DPLA   - number of decimal places in output
C     THRSH  - array containing the limits for the output
C              (1) - minimum value to be output
C              (2) - maximum value to be output
C     LSUM   - size of SUM and SCNT arrays (5 times number of
C              possible datasets)
C     SUM    - hourly, daily, monthly, annual, grand sums for each
C              dataset in reverse order
C     SCNT   - hourly, daily, monthly, annual, grand counts for
C              each dataset in reverse order to compute averages
C     LINES  - number of lines allowed per screen
C     PAUSE  - counts the number of lines output
C     DROW   - counts the number of data rows for the 2-d parm screen
C     DPOS   - position in data-set being edited of values on 2-d screen
C     IRET   - user exit command value from paused screen
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     OLEN,IPOS,NPOS,I,I0,I1,I5,J,DCHK,IS,IE,NDS,SGRP
      CHARACTER*1 TBUFF(250),BLNK(1),CHTOT(5)
C
C     + + + FUNCTIONS + + +
      INTEGER     DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL    DAYMON, PRTLIN, ZIPC, TSGDAT, TSDROW, CHRCHR
      EXTERNAL    Q2INIT, ZIPI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  BLNK/' '/, CHTOT/'T','o','t','a','l'/
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I5 = 5
C     NDS is number of possible dataset used for offsets on do loops
      NDS = LSUM/5
C
      IRET= 1
      IE  = LSUM
      IS  = IE + 1 - NDS
      DCHK= DATE(5) + DATE(6)
      IF (DCHK .EQ. 0) THEN
C       print hourly sums
        IF (TOTAVE(5).GT.0) THEN
C         end of hour
          IPOS = IS
          IF (TOTAVE(5).EQ.2) THEN
C           average, not sum
            DO 10 I = IS,IE
              IF (SCNT(I) .GT. 0) THEN
                SUM(I) = SUM(I)/SCNT(I)
              ELSE
                SUM(I) = -1.0E20
              END IF
 10         CONTINUE
          END IF
C         screen output
          IF (PAUSE.GE.LINES) THEN
C           summary would go beyond page boundary, do on next screen
C           allow user to edit 1st column of values
            CALL TSGDAT (DROW,DPOS,
     O                   IRET)
            PAUSE= 0
            DROW = 0
C           re-init for next screen
            SGRP = 70+ NUMDSN
            CALL Q2INIT (MESSFL,SCLU,SGRP)
C           init data row position pointer for all available data rows
            CALL ZIPI (LINES,I0,DPOS)
          END IF
          IF (IRET.EQ.1) THEN
C           user wants to continue
            CALL PRTLIN (DATE,NUMDSN,SUM(IPOS),WDTH,SGFD,DPLA,THRSH,
     O                   OLEN,TBUFF)
            I = 3
            CALL ZIPC (I,BLNK,TBUFF(19))
C           put total label (or part of it) into date string
            CALL CHRCHR (I,CHTOT,TBUFF(19))
            NPOS = 23
C           build edit buffer values for screen
            DROW  = DROW + 1
            CALL TSDROW (SUM(IPOS),DROW,NUMDSN,OLEN,TBUFF,
     I                   WDTH,NPOS,SGFD,DPLA,I1,THRSH,I1)
            PAUSE = PAUSE + 1
            DO 15 I=1,NUMDSN
              J = IPOS -1 + I
              SUM(J) = 0.0
              SCNT(J) = 0.0
 15         CONTINUE
          END IF
        END IF
C
      IS = IS - NDS
      IE = IE - NDS
        IF (DATE(4) .EQ. 24) THEN
          IF (TOTAVE(4).GT.0) THEN
C           print daily sums at end of day
            IPOS = IS
            IF (TOTAVE(4).EQ.2) THEN
C             average, not sum
              DO 20 I = IS,IE
                IF (SCNT(I) .GT. 0) THEN
                  SUM(I) = SUM(I)/SCNT(I)
                ELSE
                  SUM(I) = -1.0E20
                END IF
 20           CONTINUE
            END IF
C           user continuing with screen output
            IF (PAUSE.GE.LINES) THEN
C             summary would go beyond page boundary, do on next screen
C             allow user to edit 1st column of values
              CALL TSGDAT (DROW,DPOS,
     O                     IRET)
              PAUSE= 0
              DROW = 0
C             re-init for next screen
              SGRP = 70+ NUMDSN
              CALL Q2INIT (MESSFL,SCLU,SGRP)
C             init data row position pointer for all available data rows
              CALL ZIPI (LINES,I0,DPOS)
            END IF
            IF (IRET.EQ.1) THEN
C             user wants to continue
              CALL PRTLIN (DATE,NUMDSN,SUM(IPOS),WDTH,SGFD,DPLA,THRSH,
     O                     OLEN,TBUFF)
              I = 8
              CALL ZIPC (I,BLNK,TBUFF(14))
C             put total label into date string
              CALL CHRCHR (I5,CHTOT,TBUFF(15))
              NPOS = 21
C             build edit buffer values for screen
              DROW  = DROW + 1
              CALL TSDROW (SUM(IPOS),DROW,NUMDSN,OLEN,TBUFF,
     I                     WDTH,NPOS,SGFD,DPLA,I1,THRSH,I1)
              PAUSE = PAUSE + 1
              DO 25 I = 1,NUMDSN
                J = IPOS - 1 + I
                SUM(J) = 0.0
                SCNT(J) = 0.0
 25           CONTINUE
            END IF
          END IF
C
        IS = IS - NDS
        IE = IE - NDS
          IF (DATE(3) .EQ. DAYMON(DATE(1),DATE(2))) THEN
            IF (TOTAVE(3).GT.0) THEN
C             print monthly sum at end of month
              IPOS = IS
              IF (TOTAVE(3).EQ.2) THEN
C               average, not sum
                DO 30 I = IS,IE
                  IF (SCNT(I) .GT. 0) THEN
                    SUM(I) = SUM(I)/SCNT(I)
                  ELSE
                    SUM(I) = -1.0E20
                  END IF
 30             CONTINUE
              END IF
C             user continuing with screen output
              IF (PAUSE.GE.LINES) THEN
C               summary would go beyond page boundary, do on next screen
C               allow user to edit 1st column of values
                CALL TSGDAT (DROW,DPOS,
     O                       IRET)
                PAUSE= 0
                DROW = 0
C               re-init for next screen
                SGRP = 70+ NUMDSN
                CALL Q2INIT (MESSFL,SCLU,SGRP)
C               init data row position pointer for all available data rows
                CALL ZIPI (LINES,I0,DPOS)
              END IF
              IF (IRET.EQ.1) THEN
C               user wants to continue
                CALL PRTLIN (DATE,NUMDSN,SUM(IPOS),WDTH,SGFD,DPLA,THRSH,
     O                       OLEN,TBUFF)
                I = 11
                CALL ZIPC (I,BLNK,TBUFF(11))
C               put total label into date string
                CALL CHRCHR (I5,CHTOT,TBUFF(12))
                NPOS = 18
C               build edit buffer values for screen
                DROW  = DROW + 1
                CALL TSDROW (SUM(IPOS),DROW,NUMDSN,OLEN,TBUFF,
     I                       WDTH,NPOS,SGFD,DPLA,I1,THRSH,I1)
                PAUSE = PAUSE + 1
                DO 35 I = 1,NUMDSN
                  J = IPOS - 1 + I
                  SUM(J) = 0.0
                  SCNT(J) = 0.0
 35             CONTINUE
              END IF
            END IF
C
            IS = IS - NDS
            IE = IE - NDS
            IF (DATE(2) .EQ. ENDMO) THEN
              IF (TOTAVE(2).GT.0) THEN
C               print annual sum at end of year period
                IPOS = IS
                IF (TOTAVE(2).EQ.2) THEN
C                 average, not sum
                  DO 40 I = IS,IE
                    IF (SCNT(I) .GT. 0) THEN
                      SUM(I) = SUM(I)/SCNT(I)
                    ELSE
                      SUM(I) = -1.0E20
                    END IF
 40               CONTINUE
                END IF
C               user continuing with screen output
                IF (PAUSE.GE.LINES) THEN
C                 summary would go beyond page boundary, do on next screen
C                 allow user to edit 1st column of values
                  CALL TSGDAT (DROW,DPOS,
     O                         IRET)
                  PAUSE= 0
                  DROW = 0
C                 re-init for next screen
                  SGRP = 70+ NUMDSN
                  CALL Q2INIT (MESSFL,SCLU,SGRP)
C                 init data row position pointer for all available data rows
                  CALL ZIPI (LINES,I0,DPOS)
                END IF
                IF (IRET.EQ.1) THEN
C                 user wants to continue
                  CALL PRTLIN (DATE,NUMDSN,SUM(IPOS),WDTH,SGFD,DPLA,
     O                         THRSH,OLEN,TBUFF)
                  I = 15
                  CALL ZIPC (I,BLNK,TBUFF(7))
C                 put total label into date string
                  CALL CHRCHR (I5,CHTOT,TBUFF(8))
                  NPOS = 14
C                 build edit buffer values for screen
                  DROW  = DROW + 1
                  CALL TSDROW (SUM(IPOS),DROW,NUMDSN,OLEN,TBUFF,
     I                         WDTH,NPOS,SGFD,DPLA,I1,THRSH,I1)
                  PAUSE = PAUSE + 1
                  DO 45 I = 1,NUMDSN
                    J = IPOS - 1 + I
                    SUM(J) = 0.0
                    SCNT(J) = 0.0
 45               CONTINUE
                END IF
              END IF
            END IF
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DSNTMP
     I                   (MESSFL,SCLU,WDMSFL,SDSN,
     O                    TDSN,IRET)
C
C     + + + PURPOSE + + +
C     Find and create a temporary data set for storing edited
C     values for a time-series listing which allows editing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMSFL,SDSN,TDSN,IRET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number for WDM file
C     SDSN   - data-set number to begin searching for free data set
C     TDSN   - temporary data-set number
C     IRET   - user exit command value
C              -1 - user wants back to main List menu
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1,I0,SGRP,RETCOD,LDSN,EXIST,DREC
C
C     + + + FUNCTIONS + + +
      INTEGER    WDCKDT
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDCKDT, ZSTCMA, QRESPI, WDDSCK, WDDSCL, ZGTRET, PRNTXT
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
C     allow previous command
      I = 4
      CALL ZSTCMA (I,I1)
C
C     find first available data set after data set being edited
      LDSN= SDSN
 5    CONTINUE
        LDSN  = LDSN + 1
        EXIST = WDCKDT (WDMSFL,LDSN)
      IF (EXIST .NE. 0) GO TO 5
C
 10   CONTINUE
C       get temporary data set for storing edited values
        SGRP= 70
        CALL QRESPI (MESSFL,SCLU,SGRP,LDSN)
C       get user exit command value
        CALL ZGTRET (IRET)
        IF (IRET.EQ.1 .AND. LDSN.GT.0) THEN
C         user wants to continue, make sure data set doesn't exist
          CALL WDDSCK (WDMSFL,LDSN,
     O                 DREC,RETCOD)
          IF (RETCOD.EQ.-81) THEN
C           data set doesn't exist, ok to use
C           copy label of source to target
            CALL WDDSCL (WDMSFL,SDSN,WDMSFL,LDSN,I0,
     O                   RETCOD)
          ELSE IF (RETCOD.EQ.0) THEN
C           data set exists, let user know to specify another data set
            SGRP = 78
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            LDSN = 0
C           get user exit command value
            CALL ZGTRET (IRET)
          END IF
          IF (RETCOD.NE.0) THEN
C           problem with temp data set specified, try again
            SGRP= 79
            CALL PRNTXT (MESSFL,SCLU,SGRP)
C           get user exit command value
            CALL ZGTRET (IRET)
          END IF
        ELSE IF (IRET.EQ.2) THEN
C         user wants back to main menu
          IRET = -1
          LDSN = 0
        END IF
      IF (IRET.EQ.2) GO TO 10
C
      IF (RETCOD.EQ.0 .AND. LDSN.GT.0) THEN
C       temp data set found and created ok
        TDSN = LDSN
      ELSE
C       will not be storing edit data on temp data set
        LDSN = 0
      END IF
C
C     turn off previous command
      I = 4
      CALL ZSTCMA (I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSGDAT
     I                   (DROW,DPOS,
     M                    IRET)
C
C     + + + PURPOSE + + +
C     Allow user to edits 1st column of listed time-series data.
C     Get edited values from 2-d parm screen containing time-series
C     data listing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DROW,DPOS(DROW),IRET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DROW   - number of data rows on 2-d screen
C     DPOS   - position in data-set being edited of values on 2-d screen
C     IRET   - user exit command value from 2-d screen
C
C     + + + PARAMETERS + + +
      INCLUDE 'pbfmax.inc'
      INCLUDE 'ptsmax.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1
      REAL       RVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q2EDIT, Q2GRBR
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C
C     allow user to edit 1st column of values
      CALL Q2EDIT (DROW,
     O             IRET)
C
      IF (IRET.EQ.1) THEN
C       user wants to continue
        DO 10 I= 1,DROW
C         get data values for this data row
          CALL Q2GRBR (I1,I1,I1,I,RVAL)
          IF (DPOS(I).GT.0) THEN
C           editable value, update value in data-set array
            YX(DPOS(I))= RVAL(1)
          END IF
 10     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSDSCP
     I                   (MESSFL,SCLU,WDMSFL,TDSN,DSN,RETCOD)
C
C     + + + PURPOSE + + +
C     Allow user to copy edited data stored on data-set number
C     TDSN back to the original data-set number DSN.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMSFL,TDSN,DSN,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number for WDM file
C     TDSN   - temporary data-set number for storing edited data
C     DSN    - original data-set number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP,CNUM,CVAL(2),IRET
C
C     + + + EXTERNALS + + +
      EXTERNAL   Q1INIT, QSETCO, QGETCO, Q1EDIT, WDDSDL, WDDSRN
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD = 0
      CVAL(1)= 2
      CVAL(2)= 2
C
 10   CONTINUE
C       init screen info
        SGRP= 80
        CALL Q1INIT (MESSFL,SCLU,SGRP)
C       set character option values
        CNUM= 2
        CALL QSETCO (CNUM,CVAL)
C       allow user to specify copy or not
        CALL Q1EDIT (IRET)
        IF (IRET.EQ.1) THEN
C         get edited values
          CALL QGETCO (CNUM,CVAL)
          IF (CVAL(1).EQ.1) THEN
C           user wants to overwrite old data w/new
C           first, delete old data set
            CALL WDDSDL (WDMSFL,DSN,
     O                   RETCOD)
            IF (RETCOD.EQ.0) THEN
C             now renumber the new data set to the old
              CALL WDDSRN (WDMSFL,TDSN,DSN,RETCOD)
            END IF
          ELSE
C           don't copy back to old data set
            IF (CVAL(2).EQ.2) THEN
C             don't save edited data
              CALL WDDSDL (WDMSFL,TDSN,
     O                     RETCOD)
            END IF
          END IF
        END IF
      IF (IRET.EQ.-1) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSLWRT
     I                   (WDMSFL,SDSN,TDSN,WRTFLG,
     I                    TUNITS,TSTEP,QUALFG,BSDAT,GETNUM,YX)
C
C     + + + PURPOSE + + +
C     Write edited time-series data values to new data set.
C     Copy any data from original data set to new data set to
C     make a complete copy/update of the original.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SDSN,TDSN,WRTFLG,
     $          TUNITS,TSTEP,QUALFG,BSDAT(6),GETNUM
      REAL      YX(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SDSN   - number of original data set
C     TDSN   - number of new data set containing edited values
C     WRTFLG - first, intermediate, or last write to new data set
C             -2 - first and last write (only one call)
C             -1 - intermediate write,
C                  update from last write to start of this one
C              0 - first write, update from start of source dsn
C              1 - intermediate write, update from buffer only
C              2 - last write, update to end of source dsn
C     TUNITS - time units
C     TSTEP  - time step
C     QUALFG - quality code for new data
C     BSDAT  - starting date of buffer of edited data
C     GETNUM - number of data values in edited data buffer
C     YX     - array of edited time-series values
C
C     + + + PARAMETERS + + +
      INTEGER    NRVAL
      PARAMETER (NRVAL=1000)
C
C     + + + SAVES + + +
      INTEGER    LEDAT(6)
      SAVE       LEDAT
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I0,I1,TDSFRC,SDAT(6),EDAT(6),LSDAT(6),NVALS,RETCOD
      REAL       RVAL(NRVAL)
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   WTFNDT, WDTPUT, TIMDIF, TIMADD, WTDSCU, WDATCP
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
C     get start/end date of original data set
      CALL WTFNDT (WDMSFL,SDSN,I1,
     O             TDSFRC,SDAT,EDAT,RETCOD)
C
      IF (WRTFLG.LE.0) THEN
C       write with updates from source dsn
        IF (WRTFLG.EQ.-1 .AND. LEDAT(1).GT.0) THEN
C         update from end of last update
          CALL WDATCP (LEDAT,LSDAT)
        ELSE
C         update from start of original data set
          CALL WDATCP (SDAT,LSDAT)
        END IF
C       determine number of intervals between start of update
C       and start of edited data being put on new data set
        CALL TIMDIF (LSDAT,BSDAT,TUNITS,TSTEP,
     O               NVALS)
        IF (NVALS.GT.0) THEN
C         copy data from original up to start of edited data
          CALL TIMADD (LSDAT,TUNITS,TSTEP,NVALS,
     O                 LEDAT)
          CALL WTDSCU (WDMSFL,SDSN,WDMSFL,TDSN,
     I                 I0,LSDAT,LEDAT,NRVAL,
     O                 RVAL,RETCOD)
        END IF
      END IF
C
      IF (GETNUM.GT.0) THEN
C       put edited values on new data set
        CALL WDTPUT (WDMSFL,TDSN,TSTEP,BSDAT,GETNUM,
     I               I1,QUALFG,TUNITS,YX,
     O               RETCOD)
C       save ending date
        CALL TIMADD (BSDAT,TUNITS,TSTEP,GETNUM,
     O               LEDAT)
      END IF
C
      IF (ABS(WRTFLG).EQ.2) THEN
C       last write, update to end of source data set
C       determine end date of edited values
        CALL TIMADD (BSDAT,TUNITS,TSTEP,GETNUM,
     O               LSDAT)
C       determine number of intervals between end of edited data
C       being put on new data set and end of original data-set
        CALL TIMDIF (LSDAT,EDAT,TUNITS,TSTEP,
     O               NVALS)
        IF (NVALS.GT.0) THEN
C         copy data from end of edited data to the
C         end of original data to the new data set
          CALL WTDSCU (WDMSFL,SDSN,WDMSFL,TDSN,
     I                 I0,LSDAT,EDAT,NRVAL,
     O                 RVAL,RETCOD)
        END IF
      END IF
C
      RETURN
      END

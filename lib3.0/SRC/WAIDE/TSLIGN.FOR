C
C
C
      SUBROUTINE   TSLIGN
     I                   (MESSFL,SCLU,PTHNAM,WDMSFL,NUMDSN,DSN,
     I                    TITLE,NUMHDR,HEADR)
C
C     + + + PURPOSE + + +
C     Generate time-series listing of specified datasets
C     with listing parameters as set in common block CTSLST.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,WDMSFL,NUMDSN,DSN(NUMDSN),NUMHDR
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
     $            SAIND,IPOS,LENBUF,ID,OLEN,LRET,FLAG,NVAL,
     $            NUMB,NPOS,DTFG(MAXD),FIRST,SUMFLG,IRET,RETCOD
      REAL        SCNT(LSUM),SUM(LSUM),DATA(MAXD),ZERO,ODATA(MAXD),RTMP
      CHARACTER*1 BLNK(1),DASH(1),TBUFF(250)
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
      EXTERNAL   ZIPC, ZIPR, PRNTXT, PRTLIN, PRTSUM, ZSTCMA, ZGTRET
      EXTERNAL   TIMADD, TIMCNV, TIMDIF, COPYI, PRTSTR, ZBLDWR, PRTLNO
      EXTERNAL   GETTXT, COPYR, PMXTXI, ZIPI, ZWNSOP
C
C     + + + OUTPUT FORMATS + + +
 2020 FORMAT (1H1)
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      I1   = 1
      I6   = 6
      ZERO = 0.0
      CALL ZIPR (LSUM,ZERO,SUM)
      CALL ZIPR (LSUM,ZERO,SCNT)
      BLNK(1)= ' '
      DASH(1)= '-'
      SUMFLG = 0
      DO 10 I= 1,SNUM
C       check for summaries in use
        IF (TOTAVE(I).GT.0) THEN
C         at least one summary desired
          SUMFLG = 1
        END IF
 10   CONTINUE
      FIRST= 0
      INIT = 1
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
CPRH      IF (TORF.EQ.1) THEN
CPRHC       don't show message until 1st screen of data displayed
CPRH        IWRT= -1
CPRH      ELSE
CPRHC       going to file, put message to screen right away
CPRH        IWRT= 1
CPRH      END IF
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
C     print header lines
      IF (TORF.EQ.2) THEN
C       put page start character to file
        WRITE (FUNIT,2020)
C       put blank line at top of file
        I= 1
        CALL PRTSTR(FUNIT,I,BLNK)
      END IF
C     title of list
      I = 78
      IF (LENSTR(I,TITLE) .GT. 0) THEN
        IF (TORF.EQ.2) THEN
          CALL PRTSTR (FUNIT,I,TITLE)
        ELSE
          CALL ZBLDWR (I,TITLE,INIT,-I1,J)
          INIT = 0
        END IF
        IF (TORF.EQ.2) THEN
          CALL PRTSTR (FUNIT,I1,BLNK)
        ELSE
          CALL ZBLDWR (I1,BLNK,INIT,-I1,J)
          PAUSE = 2
        END IF
      END IF
C
C     definition of - and *
      SGRP= 62
      I   = 80
      CALL GETTXT (MESSFL,SCLU,SGRP,
     M             I,
     O             TBUFF)
      IF (TORF.EQ.2) THEN
        CALL PRTSTR(FUNIT,I,TBUFF)
      ELSE
        CALL ZBLDWR (I,TBUFF,INIT,-I1,J)
        INIT = 0
      END IF
      IF (TORF.EQ.2) THEN
        CALL PRTSTR (FUNIT,I1,BLNK)
      ELSE
        CALL ZBLDWR (I1,BLNK,INIT,-I1,J)
      END IF
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
      IF (TORF.EQ.2) THEN
        CALL PRTSTR (FUNIT,WIDTH,TBUFF)
      ELSE
        CALL ZBLDWR (WIDTH,TBUFF,INIT,-I1,J)
      END IF
      PAUSE = PAUSE + 3
      IF (NUMHDR.GT.0) THEN
C       headers exist to output
        DO 50 I = 1,NUMHDR
          IF (TORF.EQ.2) THEN
            CALL PRTSTR (FUNIT,WIDTH,HEADR(1,I))
          ELSE
            CALL ZBLDWR (WIDTH,HEADR(1,I),INIT,-I1,J)
          END IF
 50     CONTINUE
C       finish header with another dashed line
        IF (TORF.EQ.2) THEN
          CALL PRTSTR(FUNIT,WIDTH,TBUFF)
        ELSE
          CALL ZBLDWR (WIDTH,TBUFF,INIT,-I1,J)
        END IF
        PAUSE = PAUSE + NUMHDR + 1
      END IF
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
C               title of list
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
                  CALL ZBLDWR (WIDTH,TBUFF,INIT,-I1,J)
                  PAUSE = PAUSE + NUMHDR + 1
                END IF
              END IF
            END IF
          END IF
          IPOS = IPOS + GETNUM
        IF (ID.LT.NUMDSN .AND. IRET.EQ.1) GO TO 200
C
        IF (IRET.EQ.1) THEN
C         unload buffer to terminal or file
          INUM = 1
 300      CONTINUE
C           increment date with wdms convention
            CALL TIMADD (DATE,TUNITS,TSTEP,I1,TDATE)
            CALL COPYI (I6,TDATE,DATE)
C           convert to date convention for printing
            CALL TIMCNV (DATE)
C
            IF (TORF.EQ.1 .AND. PAUSE.GE.LINES) THEN
C             pause for terminal output
              CALL ZBLDWR (I0,BLNK,I0,I0,J)
C             get user exit command value
              CALL ZGTRET (IRET)
              PAUSE= 0
C             need to initialize screen next time
              INIT = 1
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
                CALL ZIPC (NUMB,BLNK,TBUFF(NPOS))
                IF (TORF.EQ.2) THEN
                  CALL PRTSTR(FUNIT,WIDTH,TBUFF)
                ELSE
                  CALL ZBLDWR (WIDTH,TBUFF,INIT,-I1,J)
                  INIT= 0
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
                CALL PRTSUM (FUNIT,NUMDSN,ENDMO,WIDTH,DATE,
     I                       TOTAVE,SGFD,DPLA,THRSH,LSUM,
     I                       SUM,SCNT,TORF,LINES,
     O                       PAUSE,IRET)
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
C         check for more data
          COUNT = COUNT + GETNUM
          GETNUM= NVAL - COUNT
          IF (GETNUM.GT.LENBUF) THEN
C           need to get more than buffer can handle
            GETNUM = LENBUF
          END IF
C
          IF (COUNT.GE.NVAL .AND. TORF.EQ.1 .AND.
     1        TOTAVE(1).EQ.0 .AND. IRET.EQ.1) THEN
C           at end and no grand total coming, hold screen
            CALL ZBLDWR (I0,BLNK,INIT,I0,J)
C           get user exit command value
            CALL ZGTRET (IRET)
          END IF
        END IF
C
      IF (COUNT.LT.NVAL .AND. IRET.EQ.1) GO TO 100
C
      IF (TOTAVE(1).GT.0 .AND. IRET.EQ.1) THEN
C       print grand total
        IF (TOTAVE(1).EQ.2) THEN
C         generating average
          DO 400 I = 1,MAXD
            IF (SCNT(I) .GT. 0.0) THEN
              SUM(I) = SUM(I)/SCNT(I)
            ELSE
              SUM(I) = -1.0E20
            END IF
 400      CONTINUE
        END IF
        IF (TORF.EQ.2) THEN
          CALL PRTSTR (FUNIT,I1,BLNK)
        ELSE
C         make sure theres room for grand total
          IF (PAUSE+3.GT.LINES) THEN
C           summary would go beyond page boundary, do on next screen
            CALL ZBLDWR (I0,BLNK,I0,I0,J)
C           get user exit command value
            CALL ZGTRET (IRET)
            PAUSE= 0
C           need to initialize screen next time
            INIT = 1
          END IF
          IF (IRET.EQ.1) THEN
C           user wants to continue
            CALL ZBLDWR (I1,BLNK,INIT,-I1,J)
            INIT= 0
          END IF
        END IF
        I= 21
        CALL ZIPC (I,BLNK,TBUFF)
        I= WIDTH- I
        CALL ZIPC (I,DASH,TBUFF(22))
        IF (TORF.EQ.2) THEN
          CALL PRTSTR (FUNIT,WIDTH,TBUFF)
        ELSE
          CALL ZBLDWR (WIDTH,TBUFF,INIT,-I1,J)
        END IF
        CALL PRTLIN (DATE,NUMDSN,SUM,WIDTH,SGFD,DPLA,THRSH,
     O               ILEN,TBUFF)
        IF (TOTAVE(1).EQ.1) THEN
C         total
          SGRP = 64
        ELSE
C         average
          SGRP = 65
        END IF
        J = 22
        CALL GETTXT (MESSFL,SCLU,SGRP,
     M               J,
     O               TBUFF)
        IF (TORF.EQ.2) THEN
          CALL PRTSTR (FUNIT,WIDTH,TBUFF)
          CALL PRTSTR (FUNIT,I1,BLNK)
        ELSE
          CALL ZBLDWR (WIDTH,TBUFF,INIT,-I1,J)
          CALL ZBLDWR (I0,BLNK,INIT,I0,J)
        END IF
      END IF
C
      IRET= 1
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
      SUBROUTINE   PRTSUM
     I                    (FUNIT,NUMDSN,ENDMO,WDTH,DATE,TOTAVE,
     I                     SGFD,DPLA,THRSH,LSUM,SUM,SCNT,TORF,LINES,
     O                     PAUSE,IRET)
C
C     + + + PURPOSE + + +
C     Print a line of hourly, daily, annual or grand totals as requested.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FUNIT,NUMDSN,ENDMO,WDTH,DATE(6),TOTAVE(5),
     1          SGFD,DPLA,LSUM,TORF,LINES,PAUSE,IRET
      REAL      THRSH(2),SUM(LSUM),SCNT(LSUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FUNIT  - Fortran unit number for output
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
C     TORF   - output to Terminal (1) or File (2) flag
C     LINES  - number of lines allowed per screen
C     PAUSE  - counts the number of lines output
C     IRET   - user exit command value from paused screen
C
C     + + + COMMON BLOCKS + + +
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     OLEN,IPOS,I,I0,I1,J,DCHK,IS,IE,NDS,INIT
      CHARACTER*1 TBUFF(250),DASH(1),BLNK(1)
C
C     + + + FUNCTIONS + + +
      INTEGER     DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL    DAYMON, PRTLIN, ZIPC, PRTSTR, ZBLDWR, ZGTRET
C
C     + + + DATA INITIALIZATIONS + + +
      DATA DASH/'-'/, BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C     NDS is number of possible dataset used for offsets on do loops
      NDS = LSUM/5
C
      IRET= 1
      INIT= 0
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
          I = 21
          CALL ZIPC(I,BLNK,TBUFF)
          I = WDTH - I
          CALL ZIPC(I,DASH,TBUFF(22))
          IF (TORF.EQ.2) THEN
            CALL PRTSTR(FUNIT,WDTH,TBUFF)
          ELSE
C           screen output
            IF (PAUSE+3.GT.LINES) THEN
C             summary would go beyond page boundary, do on next screen
              CALL ZBLDWR (I0,BLNK,I0,I0,J)
C             get user exit command value
              CALL ZGTRET (IRET)
              PAUSE= 0
C             need to initialize screen next time
              INIT = 1
            END IF
            IF (IRET.EQ.1) THEN
C             user wants to continue
              CALL ZBLDWR (WDTH,TBUFF,INIT,-I1,J)
              INIT= 0
            END IF
          END IF
          CALL PRTLIN (DATE,NUMDSN,SUM(IPOS),WDTH,SGFD,DPLA,THRSH,
     O                 OLEN,TBUFF)
          I = 3
          CALL ZIPC (I,BLNK,TBUFF(19))
          IF (TORF.EQ.2) THEN
            CALL PRTSTR(FUNIT,WDTH,TBUFF)
            CALL PRTSTR(FUNIT,I1,BLNK)
          ELSE IF (IRET.EQ.1) THEN
C           user continuing screen output
            CALL ZBLDWR (WDTH,TBUFF,I0,-I1,J)
            CALL ZBLDWR (I1,BLNK,I0,-I1,J)
          END IF
          PAUSE = PAUSE + 3
          DO 15 I=1,NUMDSN
            J = IPOS -1 + I
            SUM(J) = 0.0
            SCNT(J) = 0.0
 15       CONTINUE
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
            I = 21
            CALL ZIPC (I,BLNK,TBUFF)
            I = WDTH- I
            CALL ZIPC (I,DASH,TBUFF(22))
            IF (TORF.EQ.2) THEN
              CALL PRTSTR(FUNIT,WDTH,TBUFF)
            ELSE IF (IRET.EQ.1) THEN
C             user continuing with screen output
              IF (PAUSE+3.GT.LINES) THEN
C               summary would go beyond page boundary, do on next screen
                CALL ZBLDWR (I0,BLNK,I0,I0,J)
C               get user exit command value
                CALL ZGTRET (IRET)
                PAUSE= 0
C               need to initialize screen next time
                INIT = 1
              END IF
              IF (IRET.EQ.1) THEN
C               user wants to continue
                CALL ZBLDWR (WDTH,TBUFF,INIT,-I1,J)
                INIT= 0
              END IF
            END IF
            CALL PRTLIN (DATE,NUMDSN,SUM(IPOS),WDTH,SGFD,DPLA,THRSH,
     O                   OLEN,TBUFF)
            I = 8
            CALL ZIPC (I,BLNK,TBUFF(14))
            IF (TORF.EQ.2) THEN
              CALL PRTSTR(FUNIT,WDTH,TBUFF)
              CALL PRTSTR(FUNIT,I1,BLNK)
            ELSE IF (IRET.EQ.1) THEN
C             user continuing screen output
              CALL ZBLDWR (WDTH,TBUFF,I0,-I1,J)
              CALL ZBLDWR (I1,BLNK,I0,-I1,J)
            END IF
            PAUSE = PAUSE + 3
            DO 25 I = 1,NUMDSN
              J = IPOS - 1 + I
              SUM(J) = 0.0
              SCNT(J) = 0.0
 25         CONTINUE
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
              I = 21
              CALL ZIPC (I,BLNK,TBUFF)
              I = WDTH- I
              CALL ZIPC (I,DASH,TBUFF(22))
              IF (TORF.EQ.2) THEN
                CALL PRTSTR(FUNIT,WDTH,TBUFF)
              ELSE IF (IRET.EQ.1) THEN
C               user continuing with screen output
                IF (PAUSE+3.GT.LINES) THEN
C                 summary would go beyond page boundary, do on next screen
                  CALL ZBLDWR (I0,BLNK,I0,I0,J)
C                 get user exit command value
                  CALL ZGTRET (IRET)
                  PAUSE= 0
C                 need to initialize screen next time
                  INIT = 1
                END IF
                IF (IRET.EQ.1) THEN
C                 user wants to continue
                  CALL ZBLDWR (WDTH,TBUFF,INIT,-I1,J)
                  INIT= 0
                END IF
              END IF
              CALL PRTLIN (DATE,NUMDSN,SUM(IPOS),WDTH,SGFD,DPLA,THRSH,
     O                     OLEN,TBUFF)
              I = 11
              CALL ZIPC (I,BLNK,TBUFF(11))
              IF (TORF.EQ.2) THEN
                CALL PRTSTR(FUNIT,WDTH,TBUFF)
                CALL PRTSTR(FUNIT,I1,BLNK)
              ELSE IF (IRET.EQ.1) THEN
C               user continuing screen output
                CALL ZBLDWR (WDTH,TBUFF,I0,-I1,J)
                CALL ZBLDWR (I1,BLNK,I0,-I1,J)
              END IF
              PAUSE = PAUSE + 3
              DO 35 I = 1,NUMDSN
                J = IPOS - 1 + I
                SUM(J) = 0.0
                SCNT(J) = 0.0
 35           CONTINUE
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
                I = 21
                CALL ZIPC (I,BLNK,TBUFF)
                I = WDTH- I
                CALL ZIPC (I,DASH,TBUFF(22))
                IF (TORF.EQ.2) THEN
                  CALL PRTSTR(FUNIT,WDTH,TBUFF)
                ELSE IF (IRET.EQ.1) THEN
C                 user continuing with screen output
                  IF (PAUSE+3.GT.LINES) THEN
C                   summary would go beyond page boundary, do on next screen
                    CALL ZBLDWR (I0,BLNK,I0,I0,J)
C                   get user exit command value
                    CALL ZGTRET (IRET)
                    PAUSE= 0
C                   need to initialize screen next time
                    INIT = 1
                  END IF
                  IF (IRET.EQ.1) THEN
C                   user wants to continue
                    CALL ZBLDWR (WDTH,TBUFF,INIT,-I1,J)
                    INIT= 0
                  END IF
                END IF
                CALL PRTLIN (DATE,NUMDSN,SUM(IPOS),WDTH,SGFD,DPLA,
     O                       THRSH,OLEN,TBUFF)
                I = 15
                CALL ZIPC (I,BLNK,TBUFF(7))
                IF (TORF.EQ.2) THEN
                  CALL PRTSTR(FUNIT,WDTH,TBUFF)
                  CALL PRTSTR(FUNIT,I1,BLNK)
                ELSE IF (IRET.EQ.1) THEN
C                 user continuing screen output
                  CALL ZBLDWR (WDTH,TBUFF,I0,-I1,J)
                  CALL ZBLDWR (I1,BLNK,I0,-I1,J)
                END IF
                PAUSE = PAUSE + 3
                DO 45 I = 1,NUMDSN
                  J = IPOS - 1 + I
                  SUM(J) = 0.0
                  SCNT(J) = 0.0
 45             CONTINUE
              END IF
            END IF
          END IF
        END IF
      END IF
C
      RETURN
      END

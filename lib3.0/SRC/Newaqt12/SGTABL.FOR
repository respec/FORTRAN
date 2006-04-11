C
C
C
      SUBROUTINE   SGWMTT
     I                    (MESSFL, WDMSFL, PTHNAM, DSNCNT, DSNBUF,
     I                     CSCENM,CLOCNM,CCONNM)
C
C     + + + PURPOSE + + +
C     Table information on timeseries from a WDM file.
C     Only a table of daily values is implemented.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL, WDMSFL, DSNCNT, DSNBUF(DSNCNT)
      CHARACTER*8 PTHNAM(1),CSCENM(DSNCNT),CLOCNM(DSNCNT),CCONNM(DSNCNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     PTHNAM - character string of path of options selected to get here
C     DSNCNT - count of datasets to work with
C     DSNBUF - array of dataset numbers
C     CSCENM - Scenario name assoc with data in dataset
C     CLOCNM - Location name assoc with data in dataset
C     CCONNM - Constituent name assoc with data in dataset
C
C     + + + PARAMETERS + + +
      INTEGER     MXDSN,    NPARM
      PARAMETER  (MXDSN=300,NPARM=27)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I, I1, I6, N, NM, SCLU, SGRP, STMON, RESP,
     $            ENDMON, TDATE(6), SDATE(6), EDATE(6), FOUT,
     $            RETCOD, INIT, YRCNT, ILEN, FLOWFG(MXDSN),
     $            SIGDIG(MXDSN), DECIML(MXDSN),
     $            START(6), ENDDT(6), SAIND, LPARM,
     $            ARSDIG(NPARM), ARDCML(NPARM), PARMCD(NPARM),
     $            ACT, SSDATE(2), SEDATE(2), TU, TS, DTRAN
      REAL        QM(366)
      CHARACTER*1 TITLE(48,MXDSN),BLNK(1)
      CHARACTER*8 CDSID,TPTHNM(1)
C
C     + + + FUNCTIONS + + +
      INTEGER    TIMCHK, DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   TIMCHK, DAYMON, QRESP, ZWNSOP, SGWTBL, WDBSGI
      EXTERNAL   GETFIL, SGSPEC, GETDAT, TBSTAT, COPYI, ZIPC
      EXTERNAL   GETTXT, PRNTXT, DTACT, DTGET, SGDATE
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
      DO 5 I= 1,DSNCNT
C       set defaults for selected data sets
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
C         specify dates to use
          SCLU = 64
          TPTHNM(1) = 'AOL'
          CALL SGDATE (MESSFL,SCLU,TPTHNM)
          SCLU = 41
          GO TO 999
C
 300    CONTINUE
C         specify table characteristics
          CALL ZWNSOP (I1,PTHNAM)
          CALL SGSPEC (MESSFL,SCLU,DSNCNT,DSNBUF,
     M                 SIGDIG,DECIML,TITLE)
          GO TO 999
C
 400    CONTINUE
C         output time-series tables
C
C         get dates for current active date set
          CALL DTACT (I)
          CALL DTGET (I,
     O                ACT,CDSID,START,ENDDT,SSDATE,SEDATE,TU,
     O                TS,DTRAN)
          STMON   = SSDATE(1)
          ENDMON  = SEDATE(1)
C         match starting and ending months to table start and end months
          START(2)= SSDATE(1)
          ENDDT(2)= SEDATE(1)
          IF (STMON.LE.ENDMON) THEN
C           number of months is simple to calc
            NM = ENDMON-STMON+1
          ELSE
C           overlaps a year boundary
            NM = 12-(STMON-ENDMON)+1
          END IF
C
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
              CALL COPYI (I6, START(1), SDATE)
              SDATE(2) = STMON
              CALL COPYI (I6, ENDDT(1), EDATE)
              EDATE(2) = ENDMON
              EDATE(3) = DAYMON (EDATE(1), EDATE(2))
              write(99,*) 'get data for ',DSNBUF(N)
              write(99,*) '       START ',(START(I),I=1,6)
              write(99,*) '         END ',(ENDDT(I),I=1,6)
              write(99,*) '       SDATE ',SDATE
              write(99,*) '       EDATA ',EDATE
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
     I                       YRCNT, START(1), ENDDT(1), NM, DTRAN,
     M                       SDATE, EDATE, TDATE,
     O                       QM, RETCOD)
                write(99,*) 'return code from GETDAT:',RETCOD
                IF (RETCOD .EQ. 0) THEN
C                 table the data
                  CALL SGWTBL (WDMSFL, FOUT, MESSFL, SCLU, DSNBUF(N),
     I                         SDATE(1), SDATE(2), NM, QM, SIGDIG(N),
     I                         DECIML(N), TITLE(1,N), FLOWFG(N),
     I                         CSCENM(N),CLOCNM(N),CCONNM(N),DTRAN)
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
      SUBROUTINE   SGSPEC
     I                   (MESSFL,SCLU,DSNCNT,DSNBUF,
     M                    SIGDIG,DECIML,TITLE)
C
C     + + + PURPOSE + + +
C     Specify table characteristics such as data precision
C     and table titles.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,DSNCNT,SIGDIG(DSNCNT),
     $            DECIML(DSNCNT),DSNBUF(DSNCNT)
      CHARACTER*1 TITLE(48,DSNCNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     DSNCNT - count of data sets being tabled
C     DSNBUF - array of data set numbers
C     SIGDIG - array of significant digits for data sets being tabled
C     DECIML - array of decimal places for data sets being tabled
C     TITLE  - array of table titles for data sets being tabled
C
C     + + + PARAMETERS + + +
      INTEGER     MXDSN
      PARAMETER  (MXDSN=300)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I1,SGRP,IRET,ILEN,
     $            INUM,IVAL(3,MXDSN),CNUM
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZSTCMA, Q2INIT, Q2EDIT, Q2SETI, Q2SCTF
      EXTERNAL    Q2GETI, Q2GCTF
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
      SUBROUTINE   SGWTBL
     I                    (WDMFL,FOUT,MESSFL,SCLU,DSN,SYR,SMO,NM,
     I                     QM,SIGDIG,DECPLA,TITLE,FLOWFG,
     I                     CSCENM,CLOCNM,CCONNM,DTRAN)
C     + + + PURPOSE + + +
C     Produce a table on the output file of daily values.
C     The table has days for rows and months for columns.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMFL,FOUT,MESSFL,SCLU,SYR,SMO,DSN,NM,
     $            DECPLA,SIGDIG,FLOWFG,DTRAN
      REAL        QM(366)
      CHARACTER*1 TITLE(48)
      CHARACTER*8 CSCENM,CLOCNM,CCONNM
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
C     CSCENM - Scenario name assoc with data in dataset
C     CLOCNM - Location name assoc with data in dataset
C     CCONNM - Constituent name assoc with data in dataset
C     DTRAN  - transformation function
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I, J, K, L1, L2, L3, L4, L12, L52, L80, L132, M,
     &            N, SGRP, SAIND, SALEN, IPOS, ILEN, OLEN, JUST,
     &            CENTER, NSIZE, RETCOD, TU, TS, LINE, DCP, IY,
     &            TXTLEN, SDATE(6), TDATE(6)
      REAL        AREA, ZERO, SDAY(12), SUM(12), X, BAD, SMALL, LARGE,
     &            SMIN(12), SMAX(12), SX
      CHARACTER*1  OBUF(132), CTMP(20), BLNK, TBUF(80),DASH(1),CDAY(4),
     &             CHRMON(2), MONVR1, OBUFTM(132)
      CHARACTER*2  MONVR2
      CHARACTER*8  CTRAN
      CHARACTER*31 TEXT1
      CHARACTER*32 TEXT2
      CHARACTER*52 FULTXT
      CHARACTER*80 OBUF80
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG10, ABS
C
C     + + + FUNCTIONS + + +
      INTEGER     STRLNX, DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZIPC, CHRCHR, GETTXT, INTCHR, DECCHX, WDBSGC
      EXTERNAL    STRLNX, WDBSGR, LFTSTR, ZIPR, TIMDIF, DAYMON
      EXTERNAL    CARVAR, CVARAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA    CDAY/' ','D','a','y'/
      DATA  L1, L2, L3, L4, L12, L52, L80, L132
     #     / 1,  2,  3,  4,  12,  52,  80,  132/
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
C
C     get number for bad data
      SAIND = 32
      SALEN = 1
      CALL WDBSGR (WDMFL,DSN,SAIND,SALEN,
     O             BAD,RETCOD)
      IF (RETCOD.NE.0) THEN
C       could not get value for attribute, set to default value
        BAD = -999999.
      END IF
C
C     get scenario, location, constituent
      IF (DTRAN.EQ.0) THEN
        CTRAN = 'AVERAGE'
      ELSE IF (DTRAN.EQ.1) THEN
        CTRAN = 'SUMMED '
      ELSE IF (DTRAN.EQ.2) THEN
        CTRAN = 'MAXIMUM'
      ELSE IF (DTRAN.EQ.3) THEN
        CTRAN = 'MINIMUM'
      END IF
      WRITE (FOUT,2000)
      WRITE (FOUT,2001)
      OBUF80=CTRAN // ' ' // CSCENM // ' ' // CCONNM // ' AT ' //CLOCNM
      CALL CVARAR (L80,OBUF80,L80,OBUFTM)
      OLEN = STRLNX(L80,OBUFTM)
C     write scenario, location, constituent
      CALL ZIPC (L132, BLNK, OBUF)
      IPOS = CENTER - (OLEN)/2
      IF (IPOS .LT. 2) IPOS = 2
      IPOS = IPOS + 1
      CALL CHRCHR (L80,OBUFTM,OBUF(IPOS))
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
            IF (ABS(QM(I) -BAD) .GT. 1.0E-9) THEN
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
                X = AREA*53.3
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

C
C
C
      SUBROUTINE   PKINTR
     I                    (MESSFL,DMAX,
     M                     WDMSFL,DCOUNT,DSNARR,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     This routine gets peak flow data and qualification codes from the
C     user and stores the data on a WDM file for further processing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  MESSFL, DMAX, WDMSFL, DCOUNT, DSNARR(DMAX),RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     DMAX   - maximum number of data sets
C     WDMSFL - Fortran unit number of WDM file
C     DCOUNT - number of data sets
C     DSNARR - buffer of data-set numbers
C     RETCOD - 0 all ok
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  SCLU, GROUP, RESP, L1,L2, DREC, I, BLDOPT,
     &         STYR, EDYR, CDUM, DSN, SALEN, SAIND, IRET, L16, L3,
     &         L0, L9, L80, NDN, NUP, NSA, NSASP, NDP, PSA, DSTYPE,
     &         L48, DELFG 
      REAL     LAT, LONG
      CHARACTER*1  BLNK, TBUFF(80)
      CHARACTER*8  PTHNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, PRWFIL, WDBSAC, GPPKTB, UPSTAT, QGETIB
      EXTERNAL  PRNTXT, ZIPC, ZIPI, WDLBAX, QRESP, PRNTXI
      EXTERNAL  QGETRB, QGTCOB, QGTCTF, WDBSAR, QFCLOS 
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  BLNK/' '/,L1,L2,L3,L16,L48,L0,L9,L80/1,2,3,16,48,0,9,80/
      DATA  NDN,NUP,NSA,NSASP,NDP/10,10,50,75,10/
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 121
C
C     name of WDM file to store peak flows
C     open WDM file
      PTHNAM = 'IK      '
      BLDOPT = 1
C
      IF (WDMSFL .GT. 0) THEN
C       wdm already open,(1-use existing, 2-new wdm, 3-return to main
        GROUP = 71
        CALL QRESP (MESSFL, SCLU, GROUP, RESP)
      ELSE
C       default, user must need a new one
        RESP = 2
      END IF
C
      IF (RESP .EQ. 2) THEN
C       build new wdm file
        IF (WDMSFL .GT. 0) THEN
C         close current WDM file
          DELFG = 0
          CALL QFCLOS (WDMSFL,DELFG)
        END IF
        WDMSFL = 0
        CALL PRWFLE (MESSFL, BLDOPT, PTHNAM,
     M               WDMSFL)
        DCOUNT = 0
        CALL ZIPI (DMAX, L0, DSNARR)
      END IF
C
      IF (RESP .LT. 3) THEN
C       continue processing
        IF (WDMSFL .LE. 0) THEN
C         could not create a WDM file w/ name provided, error code is &
          GROUP = 31
          CALL PRNTXI (MESSFL, SCLU, GROUP, RETCOD)
        ELSE
C         WDM file opened ok, show name in status window
          I= 2
          CALL UPSTAT (-I,I,WDMSFL,L0,L0)
          DSN = 0
 8        CONTINUE
            DSN = DSN + 1
C           check to see if exists
            CALL WDDSCK (WDMSFL,DSN,
     O                   DREC,RETCOD)
          IF (DREC.NE.0 .AND. RETCOD.EQ.0) GO TO 8 
C
 10       CONTINUE
C           begin loop to get stations of data
            CALL ZIPC (L80,BLNK,TBUFF)
 20         CONTINUE
C             get 1st/last year, station id and name,lat, long
              GROUP = 29
              CALL Q1INIT (MESSFL,SCLU,GROUP)
              CALL QSETIB (L1,L3,DSN)
              CALL Q1EDIT (IRET)
              CALL QGETIB (L1,L1,STYR)
              CALL QGETIB (L1,L2,EDYR)
              CALL QGETIB (L1,L3,DSN)
              CALL QGETRB (L1,L1,LAT)
              CALL QGETRB (L1,L2,LONG)
              CALL QGTCOB (L1,L1,CDUM)
              CALL QGTCTF (L2,L16,TBUFF(16))
              CALL QGTCTF (L3,L48,TBUFF(33))
              IF ( (EDYR - STYR) .GT. 99) THEN
C               error - maximum number of years for analysis is 100
                GROUP = 30
                CALL PRNTXT (MESSFL, SCLU, GROUP)
              END IF
C
            IF ( (EDYR - STYR) .GT. 99) GO TO 20
C
            IF (CDUM .EQ. 1) THEN
C             user wants to add data for another station
C             check to see if exists
              CALL WDDSCK (WDMSFL,DSN,
     O                     DREC,RETCOD)
              IF (DREC .GT. 0) THEN
C               data set exists, find a new number and tell user
 30             CONTINUE
                  DSN = DSN + 1
C                 check to see if exists
                  CALL WDDSCK (WDMSFL,DSN,
     O                         DREC,RETCOD)
                IF (DREC.NE.0 .AND. RETCOD.EQ.0) GO TO 30
C               data set that was selected already existed, so
C               was change to &, which does not already exist.
                GROUP = 73
                CALL PRNTXI (MESSFL, SCLU, GROUP, DSN)
              END IF
C
C             create data set
              DSTYPE = 2
              CALL WDLBAX (WDMSFL, DSN, DSTYPE, NDN, NUP, NSA,
     &                     NSASP, NDP, PSA)
C
C             add station number and data-set attributes
              SALEN = 16
              SAIND = 2
              CALL WDBSAC (WDMSFL, DSN, MESSFL, SAIND, SALEN, TBUFF(16),
     O                     RETCOD)
C
C             add station name to data-set attributes
              SALEN = 48
              SAIND = 45
              CALL WDBSAC (WDMSFL, DSN, MESSFL, SAIND, SALEN, TBUFF(33),
     O                     RETCOD)
C
C             add latitude to data-set attributes
              SALEN = 1
              SAIND = 8
              CALL WDBSAR (WDMSFL, DSN, MESSFL, SAIND, SALEN, LAT,
     O                     RETCOD)
C
C             add longitude to data-set attributes
              SALEN = 1
              SAIND = 9
              CALL WDBSAR (WDMSFL, DSN, MESSFL, SAIND, SALEN, LONG,
     O                     RETCOD)
C 
C             get and put peak flow table
              CALL GPPKTB (WDMSFL, DSN, MESSFL, STYR, EDYR, RETCOD)
C
              IF (RETCOD .NE. 0) THEN
C               data not added, return code is &
                GROUP = 72
                CALL PRNTXI (MESSFL, SCLU, GROUP, RETCOD)
              ELSE
C               all ok so add to selection buffer
                DCOUNT = DCOUNT + 1
                DSNARR(DCOUNT) = DSN
              END IF
            END IF
          IF (CDUM .EQ. 1) GO TO 10
        END IF
      ELSE
C       nothing done
        RETCOD = -1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GPPKTB
     I                    (WDMSFL, DSN, MESSFL, STYR, EDYR,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     This routine interactively gets peak flow data from user and stores
C     data on a WDM table data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  WDMSFL, DSN, MESSFL, STYR, EDYR, RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - number of data set in which to store peak flow data
C     MESSFL - Fortran unit number of J407 message file
C     STYR   - starting year of peak flow data
C     EDYR   - end year of peak flow data
C     RETCOD - return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  SCN, NPOS(20), NROW(20), LEN, II, IR, IC, I, K,
     &         QFLG, ISTAT, TABID, XNUM(4), XLEND(30), XCOLD(30),
     &         FLDD, TABPT, AFLDS, ANUM(4), ASLEN(30), ASCOL(30), ASPA,
     &         AGRP, AQNU,         DATFLG, FROW, FSPA, NUMYRS, XSPA,
     &         SCNFG, SCLU, GROUP, IVAL(6,100), CVAL(3,3,100), L0,
     &         SCLUX, SGRPX,       L1, L4, MAXI,MAXR,MAXC,MXROW
      REAL     TBRBUF(14,100), RVAL(3,100), ZERO
      CHARACTER*16 TNAME
      CHARACTER*1  XTYPD(16), ASTYP(16), TBUF(80,100), MFID(2), BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL  QRESCX, ZGTRET, WDTBTM, MKTBBF, WTBPUT
      EXTERNAL  ZIPI, ZIPR, ZIPC, ZSTCMA
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  BLNK/' '/,  L0,L1,L4/0,1,4/,  ZERO/0.0/
C
C     + + + END SPECIFICATIONS + + +
C
C     begin loop to input data
C
C     initialize screen number
      SCN = 1
C     initialize total year span
      NUMYRS = EDYR - STYR + 1
C     set starting position and length in buffer for 1st screen
      NPOS(SCN) = 1
      NROW(SCN) = NUMYRS
      IF (NROW(SCN) .GT. 12)  NROW(SCN) = 12
C     blank the buffer and the arrays of values for the screen
      LEN = 80 * NUMYRS
      CALL ZIPC (LEN, BLNK, TBUF)
      LEN = 3 * NUMYRS
      CALL ZIPR (LEN, ZERO, RVAL)
      LEN = 9 * NUMYRS
      CALL ZIPI (LEN, L0, CVAL)
      LEN = 6 * NUMYRS
      CALL ZIPI (LEN, L0, IVAL)
C
      QFLG = 0
      SCNFG = 1
      II = 6
      IR = 3
      IC = 3
C
      DO 15 I = STYR, EDYR
        K = I - STYR + 1
        IVAL(1,K) = I
 15   CONTINUE
C
      CALL ZSTCMA (L4,L1)
      SCLU = 121
      GROUP = 30
 10   CONTINUE
C       get the data
        CALL QRESCX
     I              (MESSFL, SCLU, GROUP, II, IR, IC, NROW(SCN), SCNFG,
     M               IVAL(1, NPOS(SCN)), RVAL(1, NPOS(SCN)),
     M               CVAL(1,1,NPOS(SCN)), TBUF(1, NPOS(SCN))  )
C
C       check screen return code
        CALL ZGTRET(ISTAT)
C
        IF (ISTAT .EQ. 2) THEN
C         user wants previous screen
          SCN = SCN - 1
          IF (SCN .EQ. 0) THEN
C           user out of peak flow data screen
            QFLG = 1
          ELSE
C           user wants previous peak flow data screen
          END IF
        ELSE
C         set up for next screen
          SCN = SCN + 1
          NPOS(SCN) = NPOS(SCN-1) + NROW(SCN-1)
          NROW(SCN) = NUMYRS - NPOS(SCN) + 1
          IF (NROW(SCN) .GT. 12)  NROW(SCN) = 12
        END IF
C
      IF (QFLG .EQ. 0 .AND. NROW(SCN).GT.0) GO TO 10
C
C     finished input
      IF (QFLG .EQ. 0) THEN
C       put table template in data set
        MFID(1) = 'X'
        MFID(2) = 'X'
        TABID = 1
        SCLUX = 20 
        SGRPX = 1
        CALL WDTBTM (MESSFL, MFID, SCLUX,SGRPX, WDMSFL, DSN, TABID,
     I               NUMYRS,     
     O               FLDD, XNUM, XTYPD, XLEND, XCOLD, XSPA, TNAME,
     O               TABPT, AFLDS, ANUM, ASTYP, ASLEN, ASCOL, ASPA,
     O               AGRP, AQNU, RETCOD)
C
        IF (RETCOD .EQ. 0) THEN
C         convert data from character buffer to real array for WDM
C         internal format
          MAXR = 3
          MAXI = 6
          MAXC = 80
          MXROW = 100
          CALL MKTBBF ( MXROW,MAXI,MAXR,MAXC,IVAL,RVAL,TBUF,NUMYRS,
     O                  TBRBUF, RETCOD )
          IF (RETCOD .EQ. 0) THEN
C           add table data to WDM data set
            DATFLG = 1
            FROW = 1
            FSPA = 1
            CALL WTBPUT (WDMSFL,DSN,TNAME,TABID,DATFLG,
     I                   FROW,NUMYRS,FSPA,XSPA,TBRBUF,
     O                   RETCOD)
          END IF
        END IF
      ELSE
C       data not written to table
        RETCOD = -1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MKTBBF
     I                   ( MXROW,MAXI,MAXR,MAXC,IVAL,RVAL,TBUF,NPT,
     O                     TRBUFF, RETC )
C
C     + + + PURPOSE + + +
C     Puts the peak flow values arrays into the table buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MXROW,MAXI,MAXR,MAXC,IVAL(MAXI,MXROW),NPT, RETC
      REAL        TRBUFF(14,MXROW), RVAL(MAXR,MXROW)
      CHARACTER*1 TBUF(MAXC,MXROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MXROW  - number of rows (peaks)
C     MAXI   - number of coluumns of integer data
C     MAXR   - number of columns of real data
C     MAXC   - number of columns of character data
C     IVAL   - array containing integer data
C     RVAL   - array containing real data
C     TBUF   - array containing character data
C     NPT    - number of years of peaks (end - begin + 1)
C     TRBUFF - real array containing all data
C     RETC   - return code
C               0 - ok
C              -1 - too many rows
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxpk.inc'
      INTEGER   MXPK12
      PARAMETER  (MXPK12=MXPK*12)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NINDEX, INDEX, LENTH, ITMP(MXPK), I,I1,I2     
      REAL      RTMP(MXPK)
      CHARACTER*1 CTMP(MXPK12)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WTBCDI, WTBCDR, WTBCDC
C
C     + + + END SPECIFICATIONS + + +
C
      RETC = 0
      IF (NPT .GT. MXROW) THEN
C       too many rows
        RETC = -1
      ELSE
C       put the data in the buffer
        NINDEX = 14
C       put year in buffer
        INDEX = 1
        DO 10 I = 1,NPT
          ITMP(I) = IVAL(1,I)
 10     CONTINUE
        CALL WTBCDI ( NPT, NINDEX, INDEX, ITMP, TRBUFF )
C
C       put month in buffer
        INDEX = 2
        DO 20 I = 1,NPT
          ITMP(I) = IVAL(2,I)
 20     CONTINUE
        CALL WTBCDI ( NPT, NINDEX, INDEX, ITMP, TRBUFF )
C
C       put day in buffer
        INDEX = 3
        DO 30 I = 1,NPT
          ITMP(I) = IVAL(3,I)
 30     CONTINUE
        CALL WTBCDI ( NPT, NINDEX, INDEX, ITMP, TRBUFF )
C
C       put peak in buffer
        INDEX = 4
        DO 40 I = 1,NPT
          RTMP(I) = RVAL(1,I)
 40     CONTINUE
        CALL WTBCDR ( NPT, NINDEX, INDEX, RTMP, TRBUFF )
C
C       put peak qualification codes in buffer
        INDEX = 5
        LENTH = 12
        I1 =  15
        I2 = 1
        DO 50 I = 1,NPT*LENTH
          I1 = I1 + 1
          IF (I1 .GT. 27) THEN
            I2 = I2 + 1
            I1 = 16
          END IF
          CTMP(I) = TBUF(I1,I2)
 50     CONTINUE
        CALL WTBCDC ( NPT, NINDEX, INDEX, LENTH, CTMP, TRBUFF )
C
C       put associated gage height in buffer
        INDEX = 8
        DO 60 I = 1,NPT
          RTMP(I) = RVAL(2,I)
 60     CONTINUE
        CALL WTBCDR ( NPT, NINDEX, INDEX, RTMP, TRBUFF )
C
C       put associated gage height qualification codes in buffer
        INDEX = 9
        LENTH = 4
        I1 =  35
        I2 = 1
        DO 70 I = 1,NPT*LENTH
          I1 = I1 + 1
          IF (I1 .GT. 39) THEN
            I2 = I2 + 1
            I1 = 36
          END IF
          CTMP(I) = TBUF(I1,I2)
 70     CONTINUE
        CALL WTBCDC ( NPT, NINDEX, INDEX, LENTH, CTMP, TRBUFF )
C
C       put highest peak since year in buffer
        INDEX = 10
        DO 80 I = 1,NPT
          ITMP(I) = IVAL(4,I)
 80     CONTINUE
        CALL WTBCDI ( NPT, NINDEX, INDEX, ITMP, TRBUFF )
C
C       put month of highest gage height IN buffer
        INDEX = 11
        DO 90 I = 1,NPT
          ITMP(I) = IVAL(5,I)
 90     CONTINUE
        CALL WTBCDI ( NPT, NINDEX, INDEX, ITMP, TRBUFF )
C
C       put day of highest gage height in buffer
        INDEX = 12
        DO 100 I = 1,NPT
          ITMP(I) = IVAL(6,I)
 100    CONTINUE
        CALL WTBCDI ( NPT, NINDEX, INDEX, ITMP, TRBUFF )
C
C       put highest gage height in buffer
        INDEX = 13
        DO 110 I = 1,NPT
          RTMP(I) = RVAL(3,I)
 110    CONTINUE
        CALL WTBCDR ( NPT, NINDEX, INDEX, RTMP, TRBUFF )
C
C       put highest gage height qualifications codes in buffer
        INDEX = 14
        LENTH = 4
        I1 =  55
        I2 = 1
        DO 120 I = 1,NPT*LENTH
          I1 = I1 + 1
          IF (I1 .GT. 59) THEN
            I2 = I2 + 1
            I1 = 56
          END IF
          CTMP(I) = TBUF(I1,I2)
 120    CONTINUE
        CALL WTBCDC ( NPT, NINDEX, INDEX, LENTH, CTMP, TRBUFF )
      END IF
C
      RETURN
      END

C
C
C
      SUBROUTINE   WSGSYS
     I                   (MESSFL,
     I                    FNAME,MXSEN,MXLOC,
     I                    MXLINE,MXCOV,MXWDM,MXBASN,
     I                    NWDM,WDNAME,WDID,MODLID,
     I                    ARHLOG,
     I                    PLCLR,PLNTYP,PPATRN,PSYMBL,
     I                    MAPCOV,NSELEC,SELDSN,MXINTV,UEDFG,BRSFG,
     M                    NBASIN,NBWDM,BWDID,BASNAM,BASDES,
     M                    BUCIPA,BWDMNM,
     M                    LWDM,LWDNAM,LWDID,UCIPTH,BASCUR,WDMSFL,
     M                    NLOC,LAT,LNG,LCID,
     M                    TU,SDATE,EDATE,SACTIV,UNSAVE,JSTSAV)
C
C     + + + PURPOSE + + +
C     modify system specifications
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,MXSEN,MXLOC,MXLINE,MODLID,
     $              TU,ARHLOG(2),MXCOV,MXWDM,NWDM,MXBASN,
     $              PLCLR(MXSEN),PLNTYP(MXSEN),WDMSFL,MXINTV,
     $              PPATRN(MXSEN),PSYMBL(MXSEN),BASCUR,
     $              SDATE(6),EDATE(6),MAPCOV(MXCOV),LWDM,
     $              NSELEC,SELDSN(MXLINE),NBASIN,NBWDM(MXBASN),
     $              NLOC,LCID(MXLOC),SACTIV,UNSAVE,JSTSAV,UEDFG,BRSFG
      REAL          LAT(MXLOC),LNG(MXLOC)
      CHARACTER*4   WDID(MXWDM),BWDID(MXBASN,MXWDM),LWDID(MXWDM)
      CHARACTER*8   BASNAM(MXWDM)
      CHARACTER*64  WDNAME(MXWDM),FNAME,BASDES(MXBASN),BUCIPA(MXBASN),
     $              BWDMNM(MXBASN,MXWDM),LWDNAM(MXWDM),UCIPTH
C
C     + + + ARGUMENT DEFINTIONS + + +
C     MESSFL - message file containing screen definitions
C     FNAME  - Status file name
C     MXSEN  - Maximum number of scenarios
C     MXLOC  - Maximum number of locations
C     MXLINE - maximum lines on plot
C     MXCOV  - Maximum coverages on map
C     MXWDM  - Maximum number of wdm files
C     MXINTV - maximum number of data sets in each wdm file
C     NLOC   - number of active locations
C     LCID   - location ids
C     LAT    - latitude of each location
C     LNG    - longitude of each location
C     WDNAME - Name of each wdm data file
C     NWDM   - number of wdm data files
C     WDID   - id of each wdm data file
C     MODLID - Model ID code - 0:HSPF, 2:DAFLOW
C     TU     - time units (1-sec, 2-min, 3-hour, 4-day....)
C     ARHLOG - type of axis - 1- arith, 2-log
C     UEDFG  - user allowed to edit UCI file flag
C     BRSFG  - browse flag: 1-go to browse screen from map click
C     PLCLR  - line colors for each line
C     PLNTYP - line types for each line
C     PPATRN - fill pattern for plots
C     PSYMBL - symbol type for each line
C     SDATE  - start of plot (yr,mo,dy,hr,mn,sec)
C     EDATE  - end of plot (yr,mo,dy,hr,mn,sec)
C     MAPCOV - color for each map coverage
C     NSELEC - number of selected data sets
C     SELDSN - array of selected data set numbers
C     NBASIN - number of basins available
C     NBWDM  - number of wdm files for this basin
C     BASNAM - basin name
C     BASDES - basin description
C     BUCIPA - basin uci path
C     BWDID  - basin wdm file ids
C     BWDMNM - basin wdm file names
C     LWDM   - number of wdm files available
C     LWDNAM - names of each available wdm file
C     LWDID  - ids of each available wdm file
C     UCIPTH - path to uci files for this basin
C     BASCUR - current basin number
C     WDMSFL - wdm file unit number
C     SACTIV - scenario active flag
C     UNSAVE - unsaved scenario flag
C     JSTSAV - just saved this scenario flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SCLU,SGRP,RESP,I,I0,I1,IRET,FILUN(1)
      CHARACTER*8 PTHNAM(1)
      CHARACTER*64 TFNAME(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   QRESP, WSWLOC, SGDATE, WSTWR, SBASIN
      EXTERNAL   ZSTCMA, Q1INIT, Q1EDIT, QGETF, QSETFN
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 64
      I0   = 0
      I1   = 1
C
 5    CONTINUE
C       display option menu
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        GO TO (10,20,30,40,100),RESP

 10     CONTINUE
C         date sets
          PTHNAM(1) = 'C'
          CALL SGDATE (MESSFL,SCLU,PTHNAM)
          GO TO 100
C
 20     CONTINUE
C         window locations
          PTHNAM(1) = 'C'
          CALL WSWLOC (MESSFL,PTHNAM)
          GO TO 100

 30     CONTINUE
C         save state of system
C         make previous available
          I= 4
          CALL ZSTCMA (I,I1)
          SGRP= 70
          CALL Q1INIT (MESSFL,SCLU,SGRP)
          TFNAME(1) = FNAME
          CALL QSETFN (I1,TFNAME)
C         ask user to enter file name
          CALL Q1EDIT (IRET)
          IF (IRET.NE.2) THEN
C           go ahead and write file
            CALL QGETF (I1,FILUN)
            INQUIRE (UNIT=FILUN(1),NAME=TFNAME(1))
            CLOSE (UNIT=FILUN(1))
            CALL WSTWR(TFNAME(1),MXSEN,MXLINE,
     I                 MXCOV,MXWDM,MXBASN,
     I                 NWDM,WDNAME,WDID,MODLID,UEDFG,BRSFG,
     I                 TU,ARHLOG,
     I                 PLCLR,PLNTYP,PPATRN,PSYMBL,
     I                 SDATE,EDATE,
     I                 MAPCOV,NSELEC,SELDSN,
     I                 NBASIN,NBWDM,BWDID,BASNAM,BASDES,
     I                 BUCIPA,BWDMNM)
          END IF
C         make previous unavailable
          I= 4
          CALL ZSTCMA (I,I0)
          GO TO 100
C
 40     CONTINUE
C         change basins or basin specifications
          CALL SBASIN (MESSFL,SCLU,MXBASN,MXWDM,NWDM,WDNAME,WDID,
     I                 MXINTV,MXLOC,MXLINE,NSELEC,SELDSN,
     M                 NBASIN,NBWDM,BWDID,BASNAM,BASDES,BUCIPA,BWDMNM,
     M                 LWDM,LWDNAM,LWDID,UCIPTH,BASCUR,WDMSFL,
     M                 NLOC,LAT,LNG,LCID,
     M                 SACTIV,UNSAVE,JSTSAV)
          GO TO 100
C
 100    CONTINUE
      IF (RESP .LE. 4) GO TO 5
C
      RETURN
      END
C
C
C
      SUBROUTINE   SGDATE
     I                   (MESSFL,SCLU,PTHNAM)
C
C     + + + PURPOSE + + +
C     specify date sets
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster
C     PTHNAM - character string of path of options selected to get here
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxdat.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SGRP,I0,I1,I3,I8,I,IRET,I4,I15,J,I11,
     $              IVAL1(15),CVAL1(4),ID,I7,
     $              ACT,NDATE,
     $              IVAL(11,MXDATE),CVAL(7,MXDATE),DTID,RETFLG,RESP,
     $              SDATE(6),EDATE(6),SSDATE(2),SEDATE(2),DTTU,
     $              DTTS,DTAG,LPTH,ACTIV
      CHARACTER*1   BLNK,TBUFF(8,MXDATE),TBUFF1(8)
      CHARACTER*8   CDSID
      CHARACTER*48  WNDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER       ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL      ZSTCMA,DTCNT,DTGET,ZIPI,QGETCO,ZWNSOP,ZLNTXT
      EXTERNAL      Q1INIT,ZIPC,Q1EDIT,QGETI,QGTCTF,ZWNSET
      EXTERNAL      CARVAR,CVARAR,QSETCO,QSTCTF,PRNTXT,PMXTXI
      EXTERNAL      DTPUT,DTADD,DTDEL,Q2GTCO,QSETI,DTSETA,DTSETI
      EXTERNAL      Q2INIT,Q2SETI,Q2STCO,Q2SCTF,Q2EDIT
C
C     + + + END SPECIFICATIONS + + +
C
      I1   = 1
      I3   = 3
      I0   = 0
      I7   = 7
      I8   = 8
      I11  = 11
      I15  = 15
      I4   = 4
      BLNK = ' '
C
C     make previous available
      I= 4
      CALL ZSTCMA (I,I1)
C
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))

  5   CONTINUE
C       assume wont need to return to this screen
        RETFLG = 0
C       get number of date sets
        CALL DTCNT (NDATE)
C       clear screen buffer arrays
        CALL ZIPI (7*NDATE,I0,CVAL)
        CALL ZIPC (8*NDATE,BLNK,TBUFF)
        DO 10 I = 1,NDATE
C         fill in screen for each set
          CALL DTGET (I,
     O                ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,DTTU,
     O                DTTS,DTAG)
          CVAL(1,I) = 1
          CVAL(2,I) = 1
          IF (ACT.EQ.0) THEN
C           not active, set to inactive
            CVAL(3,I) = 1
          ELSE
C           active, set that way
            CVAL(3,I) = 2
            ACTIV = I
          END IF
          CVAL(4,I) = 0
          IF (SSDATE(1).EQ.1 .AND. SSDATE(2).EQ.1 .AND.
     1        SEDATE(1).EQ.12 .AND. SEDATE(2).EQ.31) THEN
C           no seasons specified
            CVAL(5,I) = 1
          ELSE
C           seasons specified
            CVAL(5,I) = 2
          END IF
          CVAL(6,I) = DTTU
          CVAL(7,I) = DTAG+1
          IVAL(1,I) = SDATE(1)
          IVAL(2,I) = SDATE(2)
          IVAL(3,I) = SDATE(3)
          IVAL(4,I) = SDATE(4)
          IVAL(5,I) = SDATE(5)
          IVAL(6,I) = EDATE(1)
          IVAL(7,I) = EDATE(2)
          IVAL(8,I) = EDATE(3)
          IVAL(9,I)= EDATE(4)
          IVAL(10,I)= EDATE(5)
          IVAL(11,I)= DTTS
          CALL CVARAR (I8,CDSID,I8,TBUFF(1,I))
 10     CONTINUE
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'Dates ('//PTHNAM(1)(1:LPTH)//'D)'
        ELSE
C         no pathname
          WNDNAM= 'Dates (D)'
        END IF
        CALL ZWNSET (WNDNAM)
        SGRP = 2
        CALL Q2INIT (MESSFL,SCLU,SGRP)
        CALL Q2SETI (I11,NDATE,IVAL)
        CALL Q2STCO (I7,NDATE,CVAL)
        CALL Q2SCTF (I4,I8,NDATE,TBUFF)
        CALL Q2EDIT (NDATE,
     O               IRET)
        IF (IRET .EQ. 1) THEN
C         accept
          CALL Q2GTCO (I7,NDATE,
     O                 CVAL)
          DTID = NDATE
          I = 1
 20       CONTINUE
C           check each date set for action desired, active/inactive
            CALL DTGET (I,
     O                  ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,DTTU,
     O                  DTTS,DTAG)
            IF (CVAL(1,I).EQ.2) THEN
C             this date set is active, set it that way
              ACT = 1
              RETFLG = 1
C             deactivate current active date set
              CALL DTSETI (ACTIV)
              ACTIV = I
            END IF
C           put date set specs back to common
            CALL DTPUT (I,ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,DTTU,
     I                  DTTS,DTAG)
            IF (CVAL(1,I).EQ.3) THEN
C             modify this date set
              RETFLG = 1
              SGRP = 23
              IF (LPTH .GT. 0) THEN
C               path name available
                WNDNAM= 'Modify ('//PTHNAM(1)(1:LPTH)//'DM)'
              ELSE
C               no pathname
                WNDNAM= 'Modify (DM)'
              END IF
              CALL ZWNSET (WNDNAM)
              CALL Q1INIT (MESSFL,SCLU,SGRP)
              IVAL1(1)  = SDATE(1)
              IVAL1(2)  = SDATE(2)
              IVAL1(3)  = SDATE(3)
              IVAL1(4)  = SDATE(4)
              IVAL1(5)  = SDATE(5)
              IVAL1(6)  = SSDATE(1)
              IVAL1(7)  = SSDATE(2)
              IVAL1(8)  = EDATE(1)
              IVAL1(9)  = EDATE(2)
              IVAL1(10) = EDATE(3)
              IVAL1(11) = EDATE(4)
              IVAL1(12) = EDATE(5)
              IVAL1(13) = SEDATE(1)
              IVAL1(14) = SEDATE(2)
              IVAL1(15) = DTTS
              CALL QSETI (I15,IVAL1)
              CVAL1(2)  = DTTU
              CVAL1(3)  = DTAG+1
              CALL QSETCO (I3,CVAL1)
              CALL CVARAR (I8,CDSID,I8,TBUFF1(1))
              CALL QSTCTF (I1,I8,TBUFF1)
C             edit values
              CALL Q1EDIT (
     O                     RESP)
              IF (RESP.EQ.1) THEN
C               user wants to continue
                CALL QGETI (I15,IVAL1)
                SDATE(1)  = IVAL1(1)
                SDATE(2)  = IVAL1(2)
                SDATE(3)  = IVAL1(3)
                SDATE(4)  = IVAL1(4)
                SDATE(5)  = IVAL1(5)
                SDATE(6)  = 0
                SSDATE(1) = IVAL1(6)
                SSDATE(2) = IVAL1(7)
                EDATE(1)  = IVAL1(8)
                EDATE(2)  = IVAL1(9)
                EDATE(3)  = IVAL1(10)
                EDATE(4)  = IVAL1(11)
                EDATE(5)  = IVAL1(12)
                EDATE(6)  = 0
                SEDATE(1) = IVAL1(13)
                SEDATE(2) = IVAL1(14)
                DTTS      = IVAL1(15)
                CALL QGETCO (I3,CVAL1)
                DTTU = CVAL1(2)
                DTAG = CVAL1(3)-1
                CALL QGTCTF (I1,I8,TBUFF1)
                CALL CARVAR (I8,TBUFF1(1),I8,CDSID)
C               put date set specs back to common
                CALL DTPUT (I,ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,DTTU,
     I                      DTTS,DTAG)
              END IF
              I = I + 1
            ELSE IF (CVAL(1,I).EQ.4) THEN
C             drop this date set
              RETFLG = 1
              CALL DTCNT (J)
              IF (J.LE.1) THEN
C               can't drop if only one, tell user
                SGRP = 24
                IF (LPTH .GT. 0) THEN
C                 path name available
                  WNDNAM= 'Drop ('//PTHNAM(1)(1:LPTH)//'DD)'
                ELSE
C                 no pathname
                  WNDNAM= 'Drop (DD)'
                END IF
                CALL ZWNSET (WNDNAM)
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                I = I + 1
              ELSE
                CALL DTDEL (I)
                NDATE = NDATE-1
                IF (NDATE.GE.I) THEN
C                 there are more lines to follow, move actions up
                  DO 100 ID= I,NDATE
                    CVAL(1,ID) = CVAL(1,ID+1)
 100              CONTINUE
                END IF
                IF (ACTIV.EQ.I) THEN
C                 dropping active date set, make first active
                  ACTIV = 1
                  CALL DTSETA (ACTIV)
                ELSE IF (ACTIV.GT.I) THEN
C                 need to decrement active date set
                  ACTIV = ACTIV - 1
                  CALL DTSETA (ACTIV)
                END IF
              END IF
            ELSE IF (CVAL(1,I).EQ.5) THEN
C             want to copy this date set
              RETFLG = 1
              CALL DTADD (I,DTID)
              IF (DTID.EQ.0) THEN
C               no room for a new date set, tell user
                SGRP    = 25
                IF (LPTH .GT. 0) THEN
C                 path name available
                  WNDNAM= 'Copy ('//PTHNAM(1)(1:LPTH)//'DC) Problem'
                ELSE
C                 no pathname
                  WNDNAM= 'Copy (DC) Problem'
                END IF
                CALL ZWNSET (WNDNAM)
                IVAL1(1)= MXDATE
                CALL PMXTXI (MESSFL,SCLU,SGRP,I8,I1,I0,I1,IVAL1)
              END IF
              I = I + 1
            ELSE
C             look at the next date set
              I = I + 1
            END IF
          IF (I.LE.NDATE) GO TO 20
        END IF
      IF (IRET.EQ.1 .AND. RETFLG.EQ.1) GO TO 5
C
C     make previous unavailable
      I= 4
      CALL ZSTCMA (I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSWLOC
     I                   (MESSFL,PTHNAM)
C
C     + + + PURPOSE + + +
C     modify window locations
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL
      CHARACTER*8  PTHNAM(1)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     MESSFL - message file containing screen definition
C     PTHNAM - path chosen to get here
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxwin.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SGRP,I4,I1,I0,IM1,J,I,RNUM,TELLIT(MXWIN),CWNCNT,WID,
     $           LINCNT,IRET,CVAL(4,MXWIN),IVAL(1,MXWIN),LPTH,RETFLG,
     $           MSEL(1),OPVAL(1),RESP,IVAL1(1),SCLU,TMPTYP
      REAL       OLDDIM(4,MXWIN),RVAL(4,MXWIN),R0,RVAL1(4)
      CHARACTER*48  WNDNAM
      INTEGER    WIPEIT(MXWIN),MAPWIN,PLTWIN,
     1           WTYPE(MXWIN),NWIN
      REAL       WINDIM(4,MXWIN)
C
C     + + + FUNCTIONS + + +
      INTEGER    ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZSTCMA, Q1INIT, QSETR, QGETR, Q1EDIT, COPYR, ZIPI
      EXTERNAL   PMXTXI, PMXCNW, ZLNTXT, ZWNSOP, ZIPR, ZWNSET
      EXTERNAL   Q2INIT, Q2EDIT, Q2SETI, Q2STCO, Q2SETR, Q2GTCO
      EXTERNAL   QSETOP, QGETOP, PRNTXT, GETWIN, PUTWIN
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 64
      R0 = 0.0
      IM1= -1
      I0 = 0
      I1 = 1
      I4 = 4
C
C     make previous command available
      CALL ZSTCMA (I4,I1)
C
C     set prefix to window names
      CALL ZWNSOP (I1,PTHNAM(1))
C     length of path name
      LPTH= ZLNTXT(PTHNAM(1))
C
C     get window specs from common
      CALL GETWIN (MXWIN,
     O             WIPEIT,MAPWIN,PLTWIN,WTYPE,NWIN,WINDIM)
C
C     copy original window dimensions
      RNUM= 4*MXWIN
      CALL COPYR (RNUM,WINDIM,OLDDIM)
C     initialize flags for window changed messages
      CALL ZIPI (MXWIN,I0,TELLIT)
C
  5   CONTINUE
C       assume wont need to return to this screen
        RETFLG = 0
C       clear screen buffer arrays
        CALL ZIPI (4*NWIN,I0,CVAL)
        CALL ZIPI (NWIN,I0,IVAL)
        CALL ZIPR (4*NWIN,R0,RVAL)
        DO 10 I = 1,NWIN
C         fill in screen for each set
          CVAL(1,I) = 1
          CVAL(2,I) = 1
          IF (MAPWIN.NE.I .AND. PLTWIN.NE.I) THEN
C           not active, set to inactive
            CVAL(3,I) = 1
          ELSE
C           active, set that way
            CVAL(3,I) = 2
          END IF
          IF (WTYPE(I).EQ.1) THEN
C           map window
            CVAL(4,I) = 1
          ELSE
C           graph window
            CVAL(4,I) = 2
          END IF
          IVAL(1,I) = I
 10     CONTINUE
        IF (LPTH .GT. 0) THEN
C         path name available
          WNDNAM= 'Windows ('//PTHNAM(1)(1:LPTH)//'W)'
        ELSE
C         no pathname
          WNDNAM= 'Windows (W)'
        END IF
        CALL ZWNSET (WNDNAM)
        SGRP = 30
        CALL Q2INIT (MESSFL,SCLU,SGRP)
        CALL Q2SETI (I1,NWIN,IVAL)
        CALL Q2STCO (I4,NWIN,CVAL)
        CALL Q2SETR (I4,NWIN,WINDIM)
        CALL Q2EDIT (NWIN,
     O               IRET)
        IF (IRET .EQ. 1) THEN
C         accept
          CALL Q2GTCO (I4,NWIN,
     O                 CVAL)
          WID = NWIN
          I = 1
 20       CONTINUE
C           check each row for action desired, active/inactive
            IF (CVAL(1,I).EQ.2) THEN
C             this window is active, set it that way
              IF (WTYPE(I).EQ.1) THEN
C               this is active map window
                MAPWIN = I
              ELSE IF (WTYPE(I).EQ.2) THEN
C               this is active graph window
                PLTWIN = I
              END IF
              RETFLG = 1
            END IF
            IF (CVAL(1,I).EQ.3) THEN
C             modify this window set
              RETFLG= 1
              SGRP  = 31
              IF (LPTH .GT. 0) THEN
C               path name available
                WNDNAM= 'Modify ('//PTHNAM(1)(1:LPTH)//'WM)'
              ELSE
C               no pathname
                WNDNAM= 'Modify (WM)'
              END IF
              CALL ZWNSET (WNDNAM)
              CALL Q1INIT (MESSFL,SCLU,SGRP)
              RVAL1(1)  = WINDIM(1,I)
              RVAL1(2)  = WINDIM(2,I)
              RVAL1(3)  = WINDIM(3,I)
              RVAL1(4)  = WINDIM(4,I)
              CALL QSETR (I4,RVAL1)
              MSEL(1) = 1
              OPVAL(1)= WTYPE(I)
              CALL QSETOP (I1,I1,MSEL,MSEL,OPVAL)
C             edit values
              CALL Q1EDIT (
     O                     RESP)
              IF (RESP.EQ.1) THEN
C               user wants to continue
                CALL QGETR (I4,RVAL1)
                WINDIM(1,I) = RVAL1(1)
                WINDIM(2,I) = RVAL1(2)
                WINDIM(3,I) = RVAL1(3)
                WINDIM(4,I) = RVAL1(4)
                CALL QGETOP (I1,OPVAL)
                TMPTYP   = WTYPE(I)
                WTYPE(I) = OPVAL(1)
                IF ((I.EQ.MAPWIN .AND. OPVAL(1).EQ.2) .OR.
     1              (I.EQ.PLTWIN .AND. OPVAL(1).EQ.1)) THEN
C                 problem, cant change type of active window
                  SGRP = 37
                  IF (LPTH .GT. 0) THEN
C                   path name available
                    WNDNAM= 'Modify ('//PTHNAM(1)(1:LPTH)//'WM) Problem'
                  ELSE
C                   no pathname
                    WNDNAM= 'Modify (WM) Problem'
                  END IF
                  CALL ZWNSET (WNDNAM)
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                  IF (I.EQ.MAPWIN .AND. OPVAL(1).EQ.2) THEN
C                   set type back to what it used to be
                    WTYPE(I) = 1
                  ELSE IF (I.EQ.PLTWIN .AND. OPVAL(1).EQ.1) THEN
C                   set type back to what it used to be
                    WTYPE(I) = 2
                  END IF
                END IF
                IF (TMPTYP.NE.WTYPE(I)) THEN
C                 changed type for this window, need to wipe from screen
                  WIPEIT(I) = 1
                END IF
              END IF
              I = I + 1
            ELSE IF (CVAL(1,I).EQ.4) THEN
C             drop this window
              RETFLG = 1
              IF (I.EQ.MAPWIN .OR. I.EQ.PLTWIN) THEN
C               can't drop if current window, tell user
                SGRP = 34 + WTYPE(I)
                IF (LPTH .GT. 0) THEN
C                 path name available
                  WNDNAM= 'Drop ('//PTHNAM(1)(1:LPTH)//'WD)'
                ELSE
C                 no pathname
                  WNDNAM= 'Drop (WD)'
                END IF
                CALL ZWNSET (WNDNAM)
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                I = I + 1
              ELSE
C               go ahead and drop
                IF (I.LT.NWIN) THEN
C                 move later window sets
                  DO 105 WID= I,NWIN-1
                    WIPEIT(WID)  = WIPEIT(WID+1)
                    WTYPE(WID)   = WTYPE(WID+1)
                    WINDIM(1,WID)= WINDIM(1,WID+1)
                    WINDIM(2,WID)= WINDIM(2,WID+1)
                    WINDIM(3,WID)= WINDIM(3,WID+1)
                    WINDIM(4,WID)= WINDIM(4,WID+1)
                    OLDDIM(1,WID)= OLDDIM(1,WID+1)
                    OLDDIM(2,WID)= OLDDIM(2,WID+1)
                    OLDDIM(3,WID)= OLDDIM(3,WID+1)
                    OLDDIM(4,WID)= OLDDIM(4,WID+1)
 105              CONTINUE
                END IF
C               set to clear the dropped window off screen
                WIPEIT(I) = 1
                NWIN = NWIN-1
                IF (NWIN.GE.I) THEN
C                 there are more lines to follow, move actions up
                  DO 100 WID= I,NWIN
                    CVAL(1,WID) = CVAL(1,WID+1)
 100              CONTINUE
                END IF
                IF (MAPWIN.GT.I) THEN
C                 need to decrement active windows
                  MAPWIN = MAPWIN - 1
                END IF
                IF (PLTWIN.GT.I) THEN
C                 need to decrement active windows
                  PLTWIN = PLTWIN - 1
                END IF
              END IF
            ELSE IF (CVAL(1,I).EQ.5) THEN
C             want to copy this window set
              RETFLG = 1
              WID = NWIN + 1
              IF (WID .LE. MXWIN) THEN
C               room for a new window set
                WIPEIT(WID)  = WIPEIT(I)
                WTYPE(WID)   = WTYPE(I)
                WINDIM(1,WID)= WINDIM(1,I)
                WINDIM(2,WID)= WINDIM(2,I)
                WINDIM(3,WID)= WINDIM(3,I)
                WINDIM(4,WID)= WINDIM(4,I)
                OLDDIM(1,WID)= WINDIM(1,I)
                OLDDIM(2,WID)= WINDIM(2,I)
                OLDDIM(3,WID)= WINDIM(3,I)
                OLDDIM(4,WID)= WINDIM(4,I)
                CVAL(1,WID)  = 1
C               update number of window sets
                NWIN= WID
              ELSE
C               no room for a new window set, tell user
                SGRP    = 38
                IF (LPTH .GT. 0) THEN
C                 path name available
                  WNDNAM= 'Copy ('//PTHNAM(1)(1:LPTH)//'WC) Problem'
                ELSE
C                 no pathname
                  WNDNAM= 'Copy (WC) Problem'
                END IF
                CALL ZWNSET (WNDNAM)
                IVAL1(1)= MXWIN
                CALL PMXTXI (MESSFL,SCLU,SGRP,I4,I1,I0,I1,IVAL1)
              END IF
              I = I + 1
            ELSE
C             look at the next window set
              I = I + 1
            END IF
          IF (I.LE.NWIN) GO TO 20
        END IF
      IF (IRET.EQ.1 .AND. RETFLG.EQ.1) GO TO 5
C
      CWNCNT = 0
      DO 34 J = 1,NWIN
        DO 35 I = 1,4
C         check to see if window dimension has changed
          IF ((WINDIM(I,J)-OLDDIM(I,J)).GT.0.001 .OR.
     1        (WINDIM(I,J)-OLDDIM(I,J)).LT.-0.001) THEN
C           dimension has changed, flag it
            WIPEIT(J) = 1
            TELLIT(J) = 1
          END IF
 35     CONTINUE
        IF (TELLIT(J).EQ.1) THEN
C         send message that window dimensions will change
C         next time something is drawn in that window.
C         increment counter of changed windows
          CWNCNT = CWNCNT + 1
          IVAL1(1)= J
          IF (CWNCNT.EQ.1) THEN
C           first window altered, give first line of message
            IF (LPTH .GT. 0) THEN
C             path name available
              WNDNAM= 'Window ('//PTHNAM(1)(1:LPTH)//'W) Note'
            ELSE
C             no pathname
              WNDNAM= 'Window (W) Note'
            END IF
            CALL ZWNSET (WNDNAM)
            SGRP   = 32
            CALL PMXTXI (MESSFL,SCLU,SGRP,I4,I1,I1,I1,IVAL1)
          ELSE IF (CWNCNT.GE.2) THEN
C           additional windows altered, give additional messages
            SGRP   = 33
            CALL PMXTXI (MESSFL,SCLU,SGRP,I4,IM1,I1,I1,IVAL1)
          END IF
        END IF
 34   CONTINUE
      IF (CWNCNT.GT.0) THEN
C       at least one window has been altered, complete screen message
        SGRP = 34
        CALL PMXCNW (MESSFL,SCLU,SGRP,I4,IM1,I0,LINCNT)
      END IF
C
C     put window specs to common
      CALL PUTWIN (MXWIN,WIPEIT,MAPWIN,PLTWIN,WTYPE,NWIN,WINDIM)
C
C     turn off previous command
      CALL ZSTCMA (I4,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ANIMAN
     I                   (MESSFL,WDMPFL,SCLU)
C
C     + + + PURPOSE + + +
C     manage individual map segments specifications for animation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMPFL,SCLU
C
C     + + + ARGUMENT DEFINTIONS + + +
C     MESSFL - message file containing screen definition
C     WDMPFL - map boundary file unit number
C     SCLU   - screen cluster number
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxani.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE  'cmaprm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SGRP,I0,I1,I3,I,IRET,J,ILEN,ID,OLEN,I4,I8,
     $              ATT1(200),ATT2(200),INUM,TYPE(200),RETCOD,
     $              CVAL(3,MXANI),RETFLG,RESP,NANI,I20,
     $              IPOS,FLEN,IDUM(200),CURANI
      REAL          DLGBUF(8)
      CHARACTER*1   NAMAN1(20,MXANI),STRIN1(8*200)
      CHARACTER*8   GLOID,DLGHED(200),CNONE
      CHARACTER*20  NAMANI(MXANI)
C
C     + + + EXTERNALS + + +
      EXTERNAL      ZSTCMA,GTNANI,GETANI,CVARAR,Q2SCTF
      EXTERNAL      Q2INIT,Q2STCO,Q2EDIT,Q1INIT,Q1EDIT,QSTCTF,QGTCTF
      EXTERNAL      PRNTXT,COPANI,WDLLSU,WDLGET,CARVAR,STCURA
      EXTERNAL      DELANI,Q2GTCO,GSTGLV,MINDIV,MODANI,GTCURA
C
C     + + + READ FORMATS + + +
2000  FORMAT(2A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I1   = 1
      I3   = 3
      I4   = 4
      I8   = 8
      I0   = 0
      I20  = 20
C
C     get atmaj, atmin pairs for segments
      I =200
C     need to call once to clear out previous use of this routine
      CALL WDLLSU (WDMPFL,I1,I,
     O             INUM,TYPE,ATT1,ATT2,RETCOD)
      CALL WDLLSU (WDMPFL,DSNMAJ,I,
     O             INUM,TYPE,ATT1,ATT2,RETCOD)
      IF (RETCOD.NE.0) THEN
C       no segments available, tell user
        SGRP = 14
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      ELSE
C       some segments available, go ahead
        ILEN = 8
        ID   = 1
        DO 2 I=1,INUM
C         for each attribute pair get header
          CALL WDLGET (WDMPFL,DSNMAJ,TYPE(I),ATT1(I),ATT2(I),ILEN,
     M                 ID,
     O                 OLEN,DLGBUF,RETCOD)
C         put header into character array
          WRITE (DLGHED(I),2000) (DLGBUF(J),J=4,5)
 2      CONTINUE
C       now put those names into global valid values
        CNONE = 'NONE    '
        CALL CVARAR (I8,CNONE,I8,STRIN1(1))
        IPOS = 9
        DO 4 I = 1,INUM
C         put valid responses into global valid array
          CALL CVARAR (I8,DLGHED(I),I8,STRIN1(IPOS))
          IPOS = IPOS + 8
 4      CONTINUE
        FLEN  = 8
        ILEN  = 8*(INUM+1)
        GLOID = 'REACHNAM'
        CALL GSTGLV (GLOID,FLEN,ILEN,STRIN1,INUM+1,IDUM)
C
C       make previous available
        I= 4
        CALL ZSTCMA (I,I1)
C
  5     CONTINUE
C         assume wont need to return to this screen
          RETFLG = 0
C         get current number of animations
          CALL GTNANI (NANI)
C         get active animation
          CALL GTCURA (CURANI)
C         get animation names
          CALL GETANI (NANI,
     O                 NAMANI)
C         clear screen buffer arrays
          DO 10 I = 1,NANI
C           fill in screen for each set
            CVAL(1,I) = 1
            CVAL(2,I) = 1
            IF (CURANI.EQ.I) THEN
              CVAL(3,I) = 2
            ELSE
              CVAL(3,I) = 1
            END IF
            CALL CVARAR (I20,NAMANI(I),I20,NAMAN1(1,I))
 10       CONTINUE
          SGRP = 5
          CALL Q2INIT (MESSFL,SCLU,SGRP)
          CALL Q2STCO (I3,NANI,CVAL)
          CALL Q2SCTF (I4,I20,NANI,NAMAN1)
          CALL Q2EDIT (NANI,
     O                 IRET)
          IF (IRET .EQ. 1) THEN
C           accept
            CALL Q2GTCO (I3,NANI,
     O                   CVAL)
            I = 1
 20         CONTINUE
C             check each row for action desired
              IF (CVAL(1,I).EQ.2) THEN
C               activate this animation
                CALL STCURA (I)
                I = I + 1
                RETFLG = 1
              ELSE IF (CVAL(1,I).EQ.3) THEN
C               list these map segment set specifications
                RETFLG = 1
                CALL MINDIV (MESSFL,SCLU,I,INUM,ATT1,ATT2,DLGHED)
                I = I + 1
              ELSE IF (CVAL(1,I).EQ.4) THEN
C               drop this animation
                RETFLG = 1
                CALL GTNANI (J)
                IF (J.LE.1) THEN
C                 can't drop if only one, tell user
                  SGRP = 7
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                  I = I + 1
                ELSE
                  CALL DELANI (I)
                  NANI = NANI-1
                  IF (NANI.GE.I) THEN
C                   there are more lines to follow, move actions up
                    DO 100 ID= I,NANI
                      CVAL(1,ID) = CVAL(1,ID+1)
 100                CONTINUE
                  END IF
                  IF (CURANI.EQ.I) THEN
C                   just dropped current animation, make first current
                    CALL STCURA (I1)
                  ELSE IF (CURANI.GT.I) THEN
C                   need to decrement current animation
                    CURANI = CURANI - 1
                    CALL STCURA (CURANI)
                  END IF
                END IF
              ELSE IF (CVAL(1,I).EQ.5) THEN
C               want to copy this animation
                RETFLG = 1
                IF (NANI.EQ.MXANI) THEN
C                 no room to add another
                  SGRP = 8
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                ELSE
C                 okay to add another
                  CALL COPANI (I)
                END IF
                I = I + 1
              ELSE IF (CVAL(1,I).EQ.6) THEN
C               identify this animation
                RETFLG= 1
                SGRP  = 6
                CALL Q1INIT (MESSFL,SCLU,SGRP)
                CALL QSTCTF (I1,I20,NAMAN1(1,I))
C               edit values
                CALL Q1EDIT (
     O                       RESP)
                IF (RESP.EQ.1) THEN
C                 user wants to continue
                  CALL QGTCTF (I1,I20,NAMAN1(1,I))
                  CALL CARVAR (I20,NAMAN1(1,I),I20,NAMANI(I))
C                 put new name to common
                  CALL MODANI (I,NAMANI(I))
                END IF
                I = I + 1
              ELSE
C               look at the next row
                I = I + 1
              END IF
            IF (I.LE.NANI) GO TO 20
          END IF
        IF (IRET.EQ.1 .AND. RETFLG.EQ.1) GO TO 5
C       make previous unavailable
        I= 4
        CALL ZSTCMA (I,I0)
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MINDIV
     I                   (MESSFL,SCLU,ANAID,INUM,ATT1,ATT2,
     I                    DLGHED)
C
C     + + + PURPOSE + + +
C     manage individual map segments specifications for animation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,ANAID,INUM,ATT1(INUM),ATT2(INUM)
      CHARACTER*8 DLGHED(INUM)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     MESSFL - message file containing screen definition
C     SCLU   - screen cluster number
C     ANAID  - animation id
C     INUM   - number of possible segment names
C     ATT1   - major attribute for each segment
C     ATT2   - minor attribute for each segment
C     DLGHED - array of segment names
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxseg.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE  'cmaprm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SGRP,I0,I1,I3,I,IRET,I2,J,ID,I4,I8,DSNID,I11,
     $              IVAL1(3),CVAL1(5),NSEG,K,
     $              IVAL(1,MXSEG),CVAL(7,MXSEG),DTID,RETFLG,RESP,I7,
     $              ATMAJ,ATMIN,DSN,CLR1,CLR2,CLR3,
     $              KNAME,MXSEL(1),MNSEL(1),I5,OPVAL(11),
     $              TDSN,TCLR1,TCLR2,TCLR3,TKNAME,TATMAJ,TATMIN
      REAL          RMAX,RMIN,RVAL1(5),RVAL(5,MXSEG),TRMIN,TRMAX,
     $              WID1,WID2,WID3,TWID1,TWID2,TWID3
      CHARACTER*1   SEGHD1(8,MXSEG),WDID1(4,MXSEG),CTXT(4)
      CHARACTER*4   WDID,TWDID
      CHARACTER*8   CNONE
C
C     + + + EXTERNALS + + +
      EXTERNAL      GTNSEG,ZIPI,GETSEG,CVARAR,Q2SCTF,QSTCTF
      EXTERNAL      Q2INIT,Q2SETI,Q2STCO,Q2SETR,Q2EDIT,QGTCTF,CARVAR
      EXTERNAL      Q1INIT,Q1EDIT,QGETI,QGETR,QGETCO,QSETOP,QGETOP
      EXTERNAL      QSETCO,PRNTXT,COPSEG,WID2UA,PMXTXI
      EXTERNAL      MODSEG,DELSEG,Q2GTCO,QSETI,QSETR,WUA2ID
C
C     + + + END SPECIFICATIONS + + +
C
      I1   = 1
      I2   = 2
      I3   = 3
      I4   = 4
      I5   = 5
      I7   = 7
      I8   = 8
      I11  = 11
      I0   = 0
C
  5   CONTINUE
C       assume wont need to return to this screen
        RETFLG = 0
C       get current number of map segment specifications
        CALL GTNSEG (ANAID,
     O               NSEG)
C       clear screen buffer arrays
        CALL ZIPI (7*NSEG,I0,CVAL)
        DO 10 I = 1,NSEG
C         fill in screen for each set
          CALL GETSEG (ANAID,I,
     O                 ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,
     O                 DSNID,WID1,WID2,WID3)
C         translate dsn as an id to a wdm file and data set number
          CALL WID2UA (I0,DSNID,
     O                 J,DSN,WDID)
C         translate atmaj and atmin into a segment name
          IF (ATMAJ.EQ.0 .AND. ATMIN.EQ.0) THEN
C           want 'none' here
            CALL CVARAR (I8,CNONE,I8,SEGHD1(1,I))
          ELSE
C           find this segment name
            DO 14 J = 1,INUM
              IF (ATMAJ.EQ.ATT1(J) .AND. ATMIN.EQ.ATT2(J)) THEN
C               found the segment name
                CALL CVARAR (I8,DLGHED(J),I8,SEGHD1(1,I))
              END IF
 14         CONTINUE
          END IF
          CALL CVARAR (I4,WDID,I4,WDID1(1,I))
          IVAL(1,I) = DSN
          RVAL(1,I) = RMIN
          RVAL(2,I) = RMAX
          RVAL(3,I) = WID1
          RVAL(4,I) = WID2
          RVAL(5,I) = WID3
          CVAL(1,I) = 1
          CVAL(2,I) = 1
          CVAL(3,I) = 0
          CVAL(4,I) = 0
          CVAL(5,I) = CLR1 + 1
          CVAL(6,I) = CLR2 + 1
          CVAL(7,I) = CLR3 + 1
 10     CONTINUE
        SGRP = 10
        CALL Q2INIT (MESSFL,SCLU,SGRP)
        CALL Q2SETI (I1,NSEG,IVAL)
        CALL Q2STCO (I7,NSEG,CVAL)
        CALL Q2SCTF (I3,I8,NSEG,SEGHD1)
        CALL Q2SCTF (I4,I4,NSEG,WDID1)
        CALL Q2SETR (I5,NSEG,RVAL)
        CALL Q2EDIT (NSEG,
     O               IRET)
        IF (IRET .EQ. 1) THEN
C         accept
          CALL Q2GTCO (I7,NSEG,
     O                 CVAL)
          DTID = NSEG
          I = 1
 20       CONTINUE
C           check each spec for action desired
            IF (CVAL(1,I).EQ.2) THEN
C             modify this map segment set
              RETFLG = 1
              SGRP = 11
              CALL Q1INIT (MESSFL,SCLU,SGRP)
              CALL GETSEG (ANAID,I,
     O                     ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,DSNID,
     O                     WID1,WID2,WID3)
C             translate dsn as an id to a wdm file and data set number
              CALL WID2UA (I0,DSNID,
     O                     J,DSN,WDID)
C             translate atmaj and atmin to a segment name
              KNAME = 0
              DO 24 J = 1,INUM
                IF (ATMAJ.EQ.ATT1(J) .AND. ATMIN.EQ.ATT2(J)) THEN
C                 found the segment name
                  KNAME = J
                END IF
 24           CONTINUE
              IVAL1(1) = DSN
              RVAL1(1) = RMIN
              RVAL1(2) = RMAX
              RVAL1(3) = WID1
              RVAL1(4) = WID2
              RVAL1(5) = WID3
              CVAL1(1) = KNAME+ 1
              CVAL1(2) = 0
              CVAL1(3) = CLR1 + 1
              CVAL1(4) = CLR2 + 1
              CVAL1(5) = CLR3 + 1
              CALL QSETI (I1,IVAL1)
              CALL QSETR (I5,RVAL1)
              CALL QSETCO (I5,CVAL1)
              CALL CVARAR (I4,WDID,I4,CTXT(1))
              CALL QSTCTF (I2,I4,CTXT)
C             edit values
              CALL Q1EDIT (
     O                     RESP)
              IF (RESP.EQ.1) THEN
C               user wants to continue
                CALL QGETI (I1,IVAL1)
                CALL QGETR (I5,RVAL1)
                CALL QGETCO (I5,CVAL1)
                CALL QGTCTF (I2,I4,CTXT)
                DSN   = IVAL1(1)
                RMIN  = RVAL1(1)
                RMAX  = RVAL1(2)
                WID1  = RVAL1(3)
                WID2  = RVAL1(4)
                WID3  = RVAL1(5)
                KNAME = CVAL1(1) - 1
                CLR1  = CVAL1(3) - 1
                CLR2  = CVAL1(4) - 1
                CLR3  = CVAL1(5) - 1
                CALL CARVAR (I4,CTXT,I4,WDID)
                IF (KNAME.GT.0) THEN
C                 translate segment name back to atmaj, atmin
                  ATMAJ = ATT1(KNAME)
                  ATMIN = ATT2(KNAME)
                ELSE
                  ATMAJ = 0
                  ATMIN = 0
                END IF
C               translate dsn and wdm file id to a dsnid
                J = 0
                CALL WUA2ID (J,DSN,WDID,
     O                       DSNID)
C               put specs back to common
                CALL MODSEG (ANAID,I,ATMAJ,ATMIN,CLR1,CLR2,CLR3,
     I                       RMIN,RMAX,DSNID,WID1,WID2,WID3)
              END IF
              I = I + 1
            ELSE IF (CVAL(1,I).EQ.3) THEN
C             drop this segment set
              RETFLG = 1
              CALL GTNSEG (ANAID,
     O                     J)
              IF (J.LE.1) THEN
C               can't drop if only one, tell user
                SGRP = 12
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                I = I + 1
              ELSE
                CALL DELSEG (ANAID,I)
                NSEG = NSEG-1
                IF (NSEG.GE.I) THEN
C                 there are more lines to follow, move actions up
                  DO 100 ID= I,NSEG
                    CVAL(1,ID) = CVAL(1,ID+1)
 100              CONTINUE
                END IF
              END IF
            ELSE IF (CVAL(1,I).EQ.4) THEN
C             want to copy this segment set
              RETFLG = 1
              CALL COPSEG (ANAID,I,DTID)
              IF (DTID.EQ.0) THEN
C               no room for a new animation line, tell user
                SGRP = 15
                IVAL1(1)= MXSEG
                CALL PMXTXI (MESSFL,SCLU,SGRP,I8,I1,I0,I1,IVAL1)
              END IF
              I = I + 1
            ELSE IF (CVAL(1,I).EQ.5) THEN
C             want to perform a global action
              RETFLG = 1
              SGRP = 13
              CALL Q1INIT (MESSFL,SCLU,SGRP)
C             get specs for defaults
              CALL GETSEG (ANAID,I,
     O                     ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,DSNID,
     O                     WID1,WID2,WID3)
C             translate dsn as an id to a wdm file and data set number
              CALL WID2UA (I0,DSNID,
     O                     J,DSN,WDID)
C             translate atmaj and atmin to a segment name
              KNAME = 0
              DO 110 J = 1,INUM
                IF (ATMAJ.EQ.ATT1(J) .AND. ATMIN.EQ.ATT2(J)) THEN
C                 found the segment name
                  KNAME = J
                END IF
 110          CONTINUE
              IVAL1(1) = DSN
              RVAL1(1) = RMIN
              RVAL1(2) = RMAX
              RVAL1(3) = WID1
              RVAL1(4) = WID2
              RVAL1(5) = WID3
              CVAL1(1) = KNAME+ 1
              CVAL1(2) = 0
              CVAL1(3) = CLR1 + 1
              CVAL1(4) = CLR2 + 1
              CVAL1(5) = CLR3 + 1
              CALL QSETI (I1,IVAL1)
              CALL QSETR (I5,RVAL1)
              CALL QSETCO (I5,CVAL1)
              CALL CVARAR (I4,WDID,I4,CTXT(1))
              CALL QSTCTF (I2,I4,CTXT)
C             set option fields
              MXSEL(1) = 11
              MNSEL(1) = 0
              CALL ZIPI (I11,I0,OPVAL)
              CALL QSETOP (I1,I11,MXSEL,MNSEL,OPVAL)
C             edit values
              CALL Q1EDIT (
     O                     RESP)
              IF (RESP.EQ.1) THEN
C               user wants to continue
                CALL QGETOP (I11,OPVAL)
                CALL QGETI (I1,IVAL1)
                CALL QGETR (I5,RVAL1)
                CALL QGETCO (I5,CVAL1)
                CALL QGTCTF (I2,I4,CTXT)
                TDSN   = IVAL1(1)
                TRMIN  = RVAL1(1)
                TRMAX  = RVAL1(2)
                TWID1  = RVAL1(3)
                TWID2  = RVAL1(4)
                TWID3  = RVAL1(5)
                TKNAME = CVAL1(1) - 1
                TCLR1  = CVAL1(3) - 1
                TCLR2  = CVAL1(4) - 1
                TCLR3  = CVAL1(5) - 1
                CALL CARVAR (I4,CTXT,I4,WDID)
                IF (TKNAME.GT.0) THEN
C                 translate segment name back to atmaj, atmin
                  TATMAJ = ATT1(TKNAME)
                  TATMIN = ATT2(TKNAME)
                ELSE
                  TATMAJ = 0
                  TATMIN = 0
                END IF
                IF (OPVAL(1).EQ.1) THEN
C                 user wants global segment name
                  DO 112 J = 1,NSEG
                    CALL GETSEG (ANAID,J,
     O                           ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,
     O                           DSN,WID1,WID2,WID3)
C                   put specs back to common
                    CALL MODSEG (ANAID,J,TATMAJ,TATMIN,CLR1,CLR2,CLR3,
     I                           RMIN,RMAX,DSN,WID1,WID2,WID3)
 112              CONTINUE
                END IF
                IF (OPVAL(1).EQ.2 .OR. OPVAL(2).EQ.2) THEN
C                 user wants global WDM file
                  DO 118 J = 1,NSEG
                    CALL GETSEG (ANAID,J,
     O                           ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,
     O                           RMAX,DSNID,WID1,WID2,WID3)
C                   translate dsn as an id to a wdm file and dsn
                    CALL WID2UA (I0,DSNID,
     O                           K,DSN,TWDID)
C                   translate dsn and wdm file number to a dsnid
                    K = 0
                    CALL WUA2ID (J,DSN,WDID,
     O                           DSNID)
C                   put specs back to common
                    CALL MODSEG (ANAID,J,ATMAJ,ATMIN,CLR1,CLR2,CLR3,
     I                           RMIN,RMAX,DSNID,WID1,WID2,WID3)
 118              CONTINUE
                END IF
                IF (OPVAL(1).EQ.3 .OR. OPVAL(2).EQ.3 .OR.
     1              OPVAL(3).EQ.3) THEN
C                 user wants global data set number
C                 translate dsn and wdm file number to a dsnid
                  J = 0
                  CALL WUA2ID (J,TDSN,WDID,
     O                         DSNID)
                  DO 115 J = 1,NSEG
                    CALL GETSEG (ANAID,J,
     O                           ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,
     O                           DSN,WID1,WID2,WID3)
C                   put specs back to common
                    CALL MODSEG (ANAID,J,ATMAJ,ATMIN,CLR1,CLR2,CLR3,
     I                           RMIN,RMAX,DSNID,WID1,WID2,WID3)
 115              CONTINUE
                END IF
                IF (OPVAL(1).EQ.4 .OR. OPVAL(2).EQ.4 .OR.
     1              OPVAL(3).EQ.4 .OR. OPVAL(4).EQ.4) THEN
C                 user wants global min value
                  DO 116 J = 1,NSEG
                    CALL GETSEG (ANAID,J,
     O                           ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,
     O                           DSN,WID1,WID2,WID3)
C                   put specs back to common
                    CALL MODSEG (ANAID,J,ATMAJ,ATMIN,CLR1,CLR2,CLR3,
     I                           TRMIN,RMAX,DSN,WID1,WID2,WID3)
 116              CONTINUE
                END IF
                IF (OPVAL(1).EQ.5 .OR. OPVAL(2).EQ.5 .OR.
     1              OPVAL(3).EQ.5 .OR. OPVAL(4).EQ.5 .OR.
     1              OPVAL(5).EQ.5) THEN
C                 user wants global max value
                  DO 117 J = 1,NSEG
                    CALL GETSEG (ANAID,J,
     O                           ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,
     O                           DSN,WID1,WID2,WID3)
C                   put specs back to common
                    CALL MODSEG (ANAID,J,ATMAJ,ATMIN,CLR1,CLR2,CLR3,
     I                           RMIN,TRMAX,DSN,WID1,WID2,WID3)
 117              CONTINUE
                END IF
                IF (OPVAL(1).EQ.6 .OR. OPVAL(2).EQ.6 .OR.
     1              OPVAL(3).EQ.6 .OR. OPVAL(4).EQ.6 .OR.
     1              OPVAL(5).EQ.6 .OR. OPVAL(6).EQ.6) THEN
C                 user wants global first color
                  DO 113 J = 1,NSEG
                    CALL GETSEG (ANAID,J,
     O                           ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,
     O                           DSN,WID1,WID2,WID3)
C                   put specs back to common
                    CALL MODSEG (ANAID,J,ATMAJ,ATMIN,TCLR1,CLR2,CLR3,
     I                           RMIN,RMAX,DSN,WID1,WID2,WID3)
 113              CONTINUE
                END IF
                IF (OPVAL(1).EQ.7 .OR. OPVAL(2).EQ.7 .OR.
     1              OPVAL(3).EQ.7 .OR. OPVAL(4).EQ.7 .OR.
     1              OPVAL(5).EQ.7 .OR. OPVAL(6).EQ.7 .OR.
     1              OPVAL(7).EQ.7) THEN
C                 user wants global second color
                  DO 114 J = 1,NSEG
                    CALL GETSEG (ANAID,J,
     O                           ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,
     O                           DSN,WID1,WID3,WID3)
C                   put specs back to common
                    CALL MODSEG (ANAID,J,ATMAJ,ATMIN,CLR1,TCLR2,CLR3,
     I                           RMIN,RMAX,DSN,WID1,WID2,WID3)
 114              CONTINUE
                END IF
                IF (OPVAL(1).EQ.8 .OR. OPVAL(2).EQ.8 .OR.
     1              OPVAL(3).EQ.8 .OR. OPVAL(4).EQ.8 .OR.
     1              OPVAL(5).EQ.8 .OR. OPVAL(6).EQ.8 .OR.
     1              OPVAL(7).EQ.8 .OR. OPVAL(8).EQ.8) THEN
C                 user wants global third color
                  DO 121 J = 1,NSEG
                    CALL GETSEG (ANAID,J,
     O                           ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,
     O                           DSN,WID1,WID3,WID3)
C                   put specs back to common
                    CALL MODSEG (ANAID,J,ATMAJ,ATMIN,CLR1,CLR2,TCLR3,
     I                           RMIN,RMAX,DSN,WID1,WID2,WID3)
 121              CONTINUE
                END IF
                IF (OPVAL(1).EQ.9 .OR. OPVAL(2).EQ.9 .OR.
     1              OPVAL(3).EQ.9 .OR. OPVAL(4).EQ.9 .OR.
     1              OPVAL(5).EQ.9 .OR. OPVAL(6).EQ.9 .OR.
     1              OPVAL(7).EQ.9 .OR. OPVAL(8).EQ.9 .OR.
     1              OPVAL(9).EQ.9) THEN
C                 user wants global width 1
                  DO 119 J = 1,NSEG
                    CALL GETSEG (ANAID,J,
     O                           ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,
     O                           DSN,WID1,WID2,WID3)
C                   put specs back to common
                    CALL MODSEG (ANAID,J,ATMAJ,ATMIN,CLR1,CLR2,CLR3,
     I                           RMIN,RMAX,DSN,TWID1,WID2,WID3)
 119              CONTINUE
                END IF
                IF (OPVAL(1).EQ.10 .OR. OPVAL(2).EQ.10 .OR.
     1              OPVAL(3).EQ.10 .OR. OPVAL(4).EQ.10 .OR.
     1              OPVAL(5).EQ.10 .OR. OPVAL(6).EQ.10 .OR.
     1              OPVAL(7).EQ.10 .OR. OPVAL(8).EQ.10 .OR.
     1              OPVAL(9).EQ.10 .OR. OPVAL(10).EQ.10) THEN
C                 user wants global width 2
                  DO 120 J = 1,NSEG
                    CALL GETSEG (ANAID,J,
     O                           ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,
     O                           DSN,WID1,WID2,WID3)
C                   put specs back to common
                    CALL MODSEG (ANAID,J,ATMAJ,ATMIN,CLR1,CLR2,CLR3,
     I                           RMIN,RMAX,DSN,WID1,TWID2,WID3)
 120              CONTINUE
                END IF
                IF (OPVAL(1).EQ.11 .OR. OPVAL(2).EQ.11 .OR.
     1              OPVAL(3).EQ.11 .OR. OPVAL(4).EQ.11 .OR.
     1              OPVAL(5).EQ.11 .OR. OPVAL(6).EQ.11 .OR.
     1              OPVAL(7).EQ.11 .OR. OPVAL(8).EQ.11 .OR.
     1              OPVAL(9).EQ.11 .OR. OPVAL(10).EQ.11 .OR.
     1              OPVAL(11).EQ.11) THEN
C                 user wants global width 3
                  DO 122 J = 1,NSEG
                    CALL GETSEG (ANAID,J,
     O                           ATMAJ,ATMIN,CLR1,CLR2,CLR3,RMIN,RMAX,
     O                           DSN,WID1,WID2,WID3)
C                   put specs back to common
                    CALL MODSEG (ANAID,J,ATMAJ,ATMIN,CLR1,CLR2,CLR3,
     I                           RMIN,RMAX,DSN,WID1,WID2,TWID3)
 122              CONTINUE
                END IF
              END IF
              I = I + 1
            ELSE
C             look at the next set of specs
              I = I + 1
            END IF
          IF (I.LE.NSEG) GO TO 20
        END IF
      IF (IRET.EQ.1 .AND. RETFLG.EQ.1) GO TO 5
C
      RETURN
      END
C
C
C
      SUBROUTINE   SBASIN
     I                   (MESSFL,SCLU,MXBASN,MXWDM,NWDM,WDNAME,WDID,
     I                    MXINTV,MXLOC,MXLINE,NANAL,ADSN,
     M                    NBASIN,NBWDM,BWDID,BASNAM,BASDES,BUCIPA,
     M                    BWDMNM,LWDM,LWDNAM,LWDID,UCIPTH,BASCUR,WDMSFL,
     M                    NLOC,LAT,LNG,LCID,
     M                    SACTIV,UNSAVE,JSTSAV)
C
C     + + + PURPOSE + + +
C     scenario generator basin routine
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,SCLU,MXBASN,MXWDM,NWDM,NBASIN,MXLOC,
     1              NBWDM(MXBASN),LWDM,BASCUR,WDMSFL,MXINTV,
     2              MXLINE,NANAL,ADSN(MXLINE),NLOC,LCID(MXLOC),
     3              SACTIV,UNSAVE,JSTSAV
      REAL          LAT(MXLOC),LNG(MXLOC)
      CHARACTER*4   WDID(MXWDM),BWDID(MXBASN,MXWDM),LWDID(MXWDM)
      CHARACTER*8   BASNAM(MXBASN)
      CHARACTER*64  BASDES(MXBASN),UCIPTH,LWDNAM(MXWDM),
     1              WDNAME(MXWDM),BUCIPA(MXBASN),BWDMNM(MXBASN,MXWDM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster for message file
C     MXBASN - maximum number of basins
C     MXLINE - maximum data sets selected
C     MXWDM  - maximum number of wdm files
C     MXINTV - maximum number of data sets per wdm file
C     MXLOC  - maximum number of map locations
C     NANAL  - number of analysis sets
C     ADSN   - array of selected data set numbers
C     NLOC   - number of active locations
C     LCID   - location ids
C     LAT    - latitude of each location
C     LNG    - longitude of each location
C     NBASIN - number of basins
C     BASNAM - basin names
C     BASDES - basin decriptions
C     NWDM   - number of wdm files (global)
C     NBWDM  - number of wdm files for each basin
C     WDNAME - wdm file names (global)
C     WDID   - wdm file ids (global)
C     BWDMNM - wdm file names for each basin
C     BWDID  - wdm file ids for each basin
C     LWDM   - number of wdm files available
C     LWDNAM - names of each available wdm file
C     LWDID  - ids of each available wdm file
C     BUCIPA - path to each basin's uci files
C     UCIPTH - path to uci files for this basin
C     BASCUR - current basin number
C     WDMSFL - wdm file unit number
C     SACTIV - scenario active flag
C     UNSAVE - unsaved scenario flag
C     JSTSAV - just saved this scenario flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I0,I1,I4,REINIT,SGRP,RESP,TMPBAS,I,LINCNT,
     1              WDMERR,TMPWDM,NCLAS,CVAL(3,5),IVAL(1,5),RETFLG,
     2              I8,I20,I48,CNUM,CLEN(7),I3,IRET,TLEN,
     3              LEN1,LEN2,LEN3,LEN4,ID,IWDM,IVAL1(1)
      CHARACTER*1   BLNK,TBUFF(48,5),TBUFF1(268),WDID1(4)
      CHARACTER*4   TBWDID(5,4)
      CHARACTER*8   TBASNA(5)
      CHARACTER*64  TBASDE(5),TBWDMN(5,4),TBUCIP(5)
      CHARACTER*78  STBUFF
C
C     + + + FUNCTIONS + + +
      INTEGER       ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL      PRNTXT,ZSTCMA,PMXCNW,WDMSET,INWSEG,ZLNTXT,PMXTXI
      EXTERNAL      DRSCAN,CLCNT,CLFDSN,WDIINI,ZIPI,ZIPC,CVARAR
      EXTERNAL      Q2INIT,Q2SETI,Q2STCO,Q2SCTB,Q2EDIT,Q2GTCO,QUPCAS
      EXTERNAL      Q1INIT,Q1EDIT,QSETCT,QGETCT,CARVAR,ZSTADD,ZQUIET
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
      I3    = 3
      I4    = 4
      I8    = 8
      I20   = 20
      I48   = 48
      REINIT= 0
      BLNK  = ' '
C
C     make previous available
      I= 4
      CALL ZSTCMA (I,I1)
C
      TMPBAS= BASCUR
  5   CONTINUE
C       assume wont need to return to this screen
        RETFLG = 0
C       clear screen buffer arrays
        CALL ZIPI (3*NBASIN,I0,CVAL)
        CALL ZIPC (48*NBASIN,BLNK,TBUFF)
        DO 10 I = 1,NBASIN
C         fill in screen for each basin
          CVAL(1,I) = 1
          CVAL(2,I) = 1
          IF (BASCUR.NE.I) THEN
C           not active, set to inactive
            CVAL(3,I) = 1
          ELSE
C           active, set that way
            CVAL(3,I) = 2
          END IF
          IVAL(1,I) = NBWDM(I)
          CALL CVARAR (I8,BASNAM(I),I8,TBUFF(1,I))
          CALL CVARAR (I20,BASDES(I),I20,TBUFF(9,I))
          CALL CVARAR (I20,BUCIPA(I),I20,TBUFF(29,I))
 10     CONTINUE
        CNUM = 1
        SGRP = 50
        CALL Q2INIT (MESSFL,SCLU,SGRP)
        CALL Q2SETI (I1,NBASIN,IVAL)
        CALL Q2STCO (I3,NBASIN,CVAL)
        CLEN(1) = 8
        CLEN(2) = 20
        CLEN(3) = 20
        CALL Q2SCTB (I3,CLEN,I48,I4,NBASIN,TBUFF)
        CALL Q2EDIT (NBASIN,
     O               IRET)
        IF (IRET .EQ. 1) THEN
C         accept
          CALL Q2GTCO (I3,NBASIN,
     O                 CVAL)
          I = 1
 20       CONTINUE
C           check each line for action desired
            IF (CVAL(1,I).EQ.2) THEN
C             activate this basin
              BASCUR = I
              RETFLG = 1
              IF (TMPBAS.NE.BASCUR) THEN
C               we've changed active basins, will need to reinitialize
                REINIT = 1
C               update wdm file variables to include basin specific files
                LWDM = NWDM + NBWDM(BASCUR)
                DO 16 I = 1,NWDM
                  LWDNAM(I) = WDNAME(I)
                  LWDID(I)  = WDID(I)
 16             CONTINUE
                IF (NBWDM(BASCUR).GT.0) THEN
                  DO 17 I = 1,NBWDM(BASCUR)
                    IF (NWDM+I.LE.MXWDM) THEN
                      LWDNAM(NWDM+I) = BWDMNM(BASCUR,I)
                      LWDID(NWDM+I)  = BWDID(BASCUR,I)
                    END IF
 17               CONTINUE
                END IF
                UCIPTH = BUCIPA(BASCUR)
                TMPBAS = BASCUR
              END IF
              I = I + 1
            ELSE IF (CVAL(1,I).EQ.3) THEN
C             modify this basin line
              RETFLG= 1
              SGRP  = 65
              CALL Q1INIT (MESSFL,SCLU,SGRP)
C             set all text fields
              CNUM = 7
              CLEN(1) = 8
              CLEN(2) = 60
              CLEN(3) = 4
              CLEN(4) = 64
              CLEN(5) = 4
              CLEN(6) = 64
              CLEN(7) = 64
              TLEN = 268
              CALL ZIPC (TLEN,BLNK,TBUFF1)
              CALL CVARAR (CLEN(1),BASNAM(I),CLEN(1),TBUFF1(1))
              CALL CVARAR (CLEN(2),BASDES(I),CLEN(2),TBUFF1(9))
              IF (NBWDM(I).GT.0) THEN
C               at least one wdm file to fill in
                CALL CVARAR (CLEN(3),BWDID(I,1),CLEN(3),TBUFF1(69))
                CALL CVARAR (CLEN(4),BWDMNM(I,1),CLEN(4),TBUFF1(73))
                IF (NBWDM(I).GT.1) THEN
C                 two wdm files to fill in
                  CALL CVARAR (CLEN(5),BWDID(I,2),CLEN(5),TBUFF1(137))
                  CALL CVARAR (CLEN(6),BWDMNM(I,2),CLEN(6),TBUFF1(141))
                END IF
              END IF
              CALL CVARAR (CLEN(7),BUCIPA(I),CLEN(7),TBUFF1(205))
              CALL QSETCT (CNUM,CLEN,TLEN,TBUFF1)
C             edit values
              CALL Q1EDIT (
     O                     RESP)
              IF (RESP.EQ.1) THEN
C               user wants to continue
                CALL QGETCT (CNUM,CLEN,TLEN,TBUFF1)
C               save old values for comparison
                TBASNA(I)  = BASNAM(I)
                TBASDE(I)  = BASDES(I)
                TBWDID(I,1)= BWDID(I,1)
                TBWDMN(I,1)= BWDMNM(I,1)
                TBWDID(I,2)= BWDID(I,2)
                TBWDMN(I,2)= BWDMNM(I,2)
                TBUCIP(I)  = BUCIPA(I)
                CALL CARVAR (CLEN(1),TBUFF1(1),CLEN(1),BASNAM(I))
                CALL CARVAR (CLEN(2),TBUFF1(9),CLEN(2),BASDES(I))
                CALL CARVAR (CLEN(3),TBUFF1(69),CLEN(3),BWDID(I,1))
                CALL CARVAR (CLEN(4),TBUFF1(73),CLEN(4),BWDMNM(I,1))
                CALL CARVAR (CLEN(5),TBUFF1(137),CLEN(5),BWDID(I,2))
                CALL CARVAR (CLEN(6),TBUFF1(141),CLEN(6),BWDMNM(I,2))
                CALL CARVAR (CLEN(7),TBUFF1(205),CLEN(7),BUCIPA(I))
                LEN1 = ZLNTXT(BWDMNM(I,1))
                LEN2 = ZLNTXT(BWDMNM(I,2))
                LEN3 = ZLNTXT(BWDID(I,1))
                LEN4 = ZLNTXT(BWDID(I,2))
                IF (LEN3.GT.0) THEN
C                 make id all caps
                  CALL CVARAR (I4,BWDID(I,1),I4,WDID1)
                  CALL QUPCAS (I4,WDID1)
                  CALL CARVAR (I4,WDID1,I4,BWDID(I,1))
                END IF
                IF (LEN4.GT.0) THEN
C                 make id all caps
                  CALL CVARAR (I4,BWDID(I,2),I4,WDID1)
                  CALL QUPCAS (I4,WDID1)
                  CALL CARVAR (I4,WDID1,I4,BWDID(I,2))
                END IF
                IF (LEN1.LE.0 .OR. LEN3.LE.0) THEN
C                 id or file name not specified for number 1
                  LEN1 = 0
                END IF
                IF (LEN2.LE.0 .OR. LEN4.LE.0) THEN
C                 id or file name not specified for number 2
                  LEN2 = 0
                END IF
                IF (LEN1.EQ.0 .AND. LEN2.EQ.0) THEN
C                 no wdm files specified
                  NBWDM(I) = 0
                ELSE IF (LEN1.GT.0 .AND. LEN2.EQ.0) THEN
C                 one wdm file specified
                  NBWDM(I) = 1
                ELSE IF (LEN1.EQ.0 .AND. LEN2.GT.0) THEN
C                 one wdm file specified
                  NBWDM(I) = 1
                  BWDMNM(I,1)= BWDMNM(I,2)
                  BWDID(I,1) = BWDID(I,2)
                ELSE IF (LEN1.GT.0 .AND. LEN2.GT.0) THEN
C                 two wdm files specified
                  NBWDM(I) = 2
                END IF
                IF (BASCUR.EQ.I) THEN
C                 the current active basin
                  IF (TBASNA(I).NE.BASNAM(I) .OR.
     1                TBASDE(I).NE.BASDES(I) .OR.
     1                TBWDID(I,1).NE.BWDID(I,1) .OR.
     1                TBWDMN(I,1).NE.BWDMNM(I,1) .OR.
     1                TBWDID(I,2).NE.BWDID(I,2) .OR.
     1                TBWDMN(I,2).NE.BWDMNM(I,2) .OR.
     1                TBUCIP(I).NE.BUCIPA(I)) THEN
C                   we made some changes
                    REINIT = 1
C                   update wdm file variables to include basin specific files
                    LWDM = NWDM + NBWDM(BASCUR)
                    DO 130 I = 1,NWDM
                      LWDNAM(I) = WDNAME(I)
                      LWDID(I)  = WDID(I)
 130                CONTINUE
                    IF (NBWDM(BASCUR).GT.0) THEN
                      DO 140 I = 1,NBWDM(BASCUR)
                        IF (NWDM+I.LE.MXWDM) THEN
                          LWDNAM(NWDM+I) = BWDMNM(BASCUR,I)
                          LWDID(NWDM+I)  = BWDID(BASCUR,I)
                        END IF
 140                  CONTINUE
                    END IF
                  END IF
                END IF
              END IF
              I = I + 1
            ELSE IF (CVAL(1,I).EQ.4) THEN
C             drop this line
              RETFLG = 1
              IF (NBASIN.EQ.1) THEN
C               give cant drop message
                SGRP = 62
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                I = I + 1
              ELSE
                IF (I.LT.NBASIN) THEN
C                 move later basins up
                  DO 100 ID= I,NBASIN-1
                    BASNAM(ID)= BASNAM(ID+1)
                    BASDES(ID)= BASDES(ID+1)
                    BUCIPA(ID)= BUCIPA(ID+1)
                    NBWDM(ID) = NBWDM(ID+1)
                    DO 150 IWDM= 1,MXWDM
                      BWDID(ID,IWDM) = BWDID(ID+1,IWDM)
                      BWDMNM(ID,IWDM)= BWDMNM(ID+1,IWDM)
  150               CONTINUE
                    CVAL(1,ID)= CVAL(1,ID+1)
 100              CONTINUE
                END IF
C               update number of basins
                NBASIN= NBASIN- 1
                IF (BASCUR.EQ.I) THEN
C                 we've just dropped the current active basin
                  BASCUR = 1
                  TMPBAS = -1
                ELSE IF (BASCUR.GT.I) THEN
C                 decrement current basin number
                  BASCUR = BASCUR - 1
                END IF
              END IF
            ELSE IF (CVAL(1,I).EQ.5) THEN
C             want to copy this line
              RETFLG = 1
              NBASIN = NBASIN+ 1
              IF (NBASIN .LE. MXBASN) THEN
C               room for a new basin
                BASNAM(NBASIN)= BASNAM(I)
                BASDES(NBASIN)= BASDES(I)
                BUCIPA(NBASIN)= BUCIPA(I)
                NBWDM(NBASIN) = NBWDM(I)
                DO 120 IWDM= 1,MXWDM
                  BWDID(NBASIN,IWDM) = BWDID(I,IWDM)
                  BWDMNM(NBASIN,IWDM)= BWDMNM(I,IWDM)
  120           CONTINUE
                CVAL(1,NBASIN) = 1
              ELSE
C               no room
                SGRP = 57
                IVAL1(1)= MXBASN
                CALL PMXTXI (MESSFL,SCLU,SGRP,I4,I1,I0,I1,IVAL1)
                NBASIN= MXBASN
              END IF
              I = I + 1
            ELSE
C             look at the next line
              I = I + 1
            END IF
          IF (I.LE.NBASIN) GO TO 20
          IF (BASCUR.LT.1) THEN
C           one and only one basin can be active
            SGRP = 56
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            RETFLG = 1
          END IF
        END IF
      IF (IRET.EQ.1 .AND. RETFLG.EQ.1) GO TO 5
C
C     make previous unavailable
      I= 4
      CALL ZSTCMA (I,I0)
C
      IF (REINIT.EQ.1) THEN
C       something changed, need to reinitialize system
        CALL ZQUIET
        SGRP= 52
        CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,LINCNT)
C       initialize wdm file common block
        CALL WDIINI
C       set wdm files for scenario generator including global names
        SGRP= 54
        CALL WDMSET (MESSFL,SCLU,SGRP,MXINTV,MXWDM,
     I               LWDM,LWDNAM,LWDID,
     O               TMPWDM,WDMERR)
        IF (WDMERR.EQ.0) THEN
C         pass wdm unit number to segment drawing routines
          WDMSFL = TMPWDM
          CALL INWSEG (WDMSFL)
C         scan time series directory
          CALL DRSCAN (WDMSFL,MXLOC,MXLINE,NANAL,ADSN,UCIPTH,I1,
     O                 NLOC,LAT,LNG,LCID)
C         set class dataset information
          CALL CLCNT(NCLAS)
          DO 4 I= 1,NCLAS
            CALL CLFDSN(I)
 4        CONTINUE
          IF (SACTIV.GT.0) THEN
C           clear out status window if old scenario is there
            I= 2
            STBUFF= ' '
            CALL ZSTADD (I,STBUFF)
            CALL ZQUIET
          END IF
C         reinitialize scenario simulation related flags
          SACTIV = 0
          UNSAVE = 0
          JSTSAV = 0
        ELSE
C         error opening wdm files, make user fix it
          RESP = 1
        END IF
      END IF
C
      RETURN
      END

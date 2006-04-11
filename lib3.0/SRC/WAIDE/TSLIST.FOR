C
C
C
      SUBROUTINE   TSLIST
     I                    (MESSFL,WDMSFL,PTHNAM,NUMDSN,DSN,EDITFG)
C
C     + + + PURPOSE + + +
C     List information on up to 30 timeseries from a WDM file.
C     The user is asked to specify formats and the optional sums
C     to be output.  Allows user to edit values in the first column
C     when EDITFG is set to 1.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,WDMSFL,NUMDSN,DSN(NUMDSN),EDITFG
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     PTHNAM - character string of path of options selected to get here
C     NUMDSN - number of data sets in buffer
C     DSN    - array of data-set numbers in buffer
C     EDITFG - specify whether editing of values is allowed
C              0 - no editing of listed values
C              1 - values listed in first column may be edited
C
C     + + + PARAMETERS + + +
      INTEGER     MXNHDR
      PARAMETER  (MXNHDR = 2)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctslst.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I1,SGRP,SCLU,IRET,RESP,RETCOD,
     $            CNMDSN,LNMDSN,TDSN,NUMHDR,DSNERR
      CHARACTER*1 TITLE(78),HEADR(250,MXNHDR),DSNAME(8,MAXD)
C
C     + + + EXTERNALS + + +
      EXTERNAL   QFCLOS, ZSTCMA, QRESP
      EXTERNAL   TSLINI, TSLDAT, TSLOUT, TSLSUM, TSLORS, TSLHDR
      EXTERNAL   ZWNSOP, TIMPRM, TSLIGN, TSLEGN, DSNTMP, TSDSCP
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 40
      I0 = 0
      I1 = 1
C
C     init listing parameters, check number of data sets
      CALL TSLINI (MESSFL,SCLU,NUMDSN,
     O             CNMDSN,DSNERR)
      IF (DSNERR.EQ.0) THEN
C       set default time parameters (in common) based on selected data sets
        CALL TIMPRM (WDMSFL,CNMDSN,DSN,
     O               DSNAME)
      END IF
C
 50   CONTINUE
C       do main list menu
        IF (DSNERR.EQ.0) THEN
C         ok to do menu
          CALL ZWNSOP (I1,PTHNAM)
          SGRP= 1
          CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        END IF
C
C       allow 'Prev' command
        I= 4
        CALL ZSTCMA (I,I1)
C
        GO TO (100,200,300,400,500,600), RESP
C
 100    CONTINUE
C         time parameters
          CALL TSLDAT (MESSFL,SCLU,PTHNAM,CNMDSN,DSN)
          GO TO 900
C
 200    CONTINUE
C         get output options
          LNMDSN = CNMDSN
          CALL TSLOUT (MESSFL,SCLU,PTHNAM,NUMDSN,
     M                 CNMDSN)
          IF (CNMDSN.NE.LNMDSN) THEN
C           different data sets, set default time parameters (in common)
            CALL TIMPRM (WDMSFL,CNMDSN,DSN,
     O                   DSNAME)
          END IF
          GO TO 900
C
 300    CONTINUE
C         get summary options
          CALL TSLSUM (MESSFL,SCLU,PTHNAM)
          GO TO 900
C
 400    CONTINUE
C         List/Screen timeseries parameters
          CALL TSLORS (MESSFL,SCLU,PTHNAM)
          GO TO 900
C
 500    CONTINUE
C         generate the listing
C         get the headers for the listing
          CALL TSLHDR (MESSFL,SCLU,CNMDSN,DSN,MXNHDR,DSNAME,
     O                 TITLE,HEADR,NUMHDR)
          IF (EDITFG.EQ.1 .AND. TORF.EQ.1) THEN
C           generate listing, allow editing of 1st data set
C           get temporary data set for storing edited values
            CALL DSNTMP (MESSFL,SCLU,WDMSFL,DSN(1),
     O                   TDSN,IRET)
            IF (IRET.NE.-1) THEN
C             user ok to continue
              CALL TSLEGN (MESSFL,SCLU,PTHNAM,WDMSFL,CNMDSN,DSN,TDSN,
     I                     TITLE,NUMHDR,HEADR)
              IF (TDSN.GT.0) THEN
C               copy edited data back to original data set?
                CALL TSDSCP (MESSFL,SCLU,WDMSFL,TDSN,DSN(1),
     O                       RETCOD)
              END IF
            END IF
          ELSE
C           generate listing to file or to screen w/out editing allowed
            CALL TSLIGN (MESSFL,SCLU,PTHNAM,WDMSFL,CNMDSN,DSN,
     I                   TITLE,NUMHDR,HEADR)
          END IF
          GO TO 900
C
 600    CONTINUE
C         return to timeseries menu
          GO TO 900
C
 900    CONTINUE
C
C       turn off 'Prev' command
        I= 4
        CALL ZSTCMA (I,I0)
C
      IF (RESP.NE.6) GO TO 50
C
      IF (TORF.EQ.2) THEN
        CALL QFCLOS (FUNIT,I0)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSLINI
     I                   (MESSFL,SCLU,NUMDSN,
     O                    CNMDSN,DSNERR)
C
C     + + + PURPOSE + + +
C     Initialize parameters for time-series data listing
C     and check that the number of data sets being listed is ok.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,NUMDSN,CNMDSN,DSNERR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     NUMDSN - number of data sets available for listing
C     CNMDSN - actual number of data set to use for listing
C     DSNERR - flag indicating user wants to change data sets for listing
C              0 - data sets are ok for listing
C              1 - user wants to change data-set numbers
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctslst.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I0,I1,SGRP,IRET
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPI, ZSTCMA, PRNTXT, ZGTRET
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
C
      I = 5
      CALL ZIPI (I,I0,TOTAVE)
      ENDMO= 12
C     init transformation codes
      CALL ZIPI (MAXD,I1,TRANS)
C     init quality code and output format parms
      QFLG = 30
      DPLA = 2
      SGFD = 5
C     init list/screen thresholds
      THRSH(1) = 0.0
      THRSH(2) = 9.0E9          
      THRSH(3) = -999.0
      THRSH(4) = -999.0
C     init output parameters
      TORF  = 1
      LINES = 16
      WIDTH = 78
C     init List/Screen flag to List
      LORS  = 1
C
      DSNERR = 0
      CNMDSN = 0
      IF (NUMDSN.GT.7) THEN
C       can't list more than seven data sets to screen
C       allow previous
        I= 4
        CALL ZSTCMA (I,I1)
        SGRP= 2
        CALL PRNTXT (MESSFL,SCLU,SGRP)
C       get user exit command value
        CALL ZGTRET (IRET)
        IF (IRET.EQ.1) THEN
C         user wants to continue, set default for main menu
          CNMDSN= 7
        ELSE
C         user wants to go back from where they came
          DSNERR = 1
        END IF
C       turn off previous
        CALL ZSTCMA (I,I0)
      ELSE
C       use all available data sets
        CNMDSN= NUMDSN
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSLDAT
     I                   (MESSFL,SCLU,PTHNAM,CNMDSN,DSN)
C
C     + + + PURPOSE + + +
C     Specify start/end dates, time step, time units, and
C     data transformations for time-series data listing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,CNMDSN,DSN(CNMDSN)
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     PTHNAM - character string of path of options selected to get here
C     CNMDSN - number of data sets in buffer
C     DSN    - array of data-set numbers in buffer
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctslst.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1,I6,SGRP,INUM,IRET,IVAL(13),CVAL(1,MAXD),ERR(6)
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZWNSOP, Q1INIT, Q1EDIT, Q2INIT, Q2EDIT
      EXTERNAL   QSETI, QSETCO, QGETI, QGETCO, Q2STCO, Q2SETI, Q2GTCO
      EXTERNAL   PRNTXT, DATCHK, CKDATE, COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      I6 = 6
C
 10   CONTINUE
C       back here on prev from 1st screen
C       get start/end date and time step/units
        CALL ZWNSOP (I1,PTHNAM)
C       get screen info
        SGRP= 21
        CALL Q1INIT (MESSFL,SCLU,SGRP)
C       set integer values
        INUM= 13
        CALL COPYI (I6,SDATE,IVAL)
        CALL COPYI (I6,EDATE,IVAL(7))
        IVAL(INUM) = TSTEP
        CALL QSETI (INUM,IVAL)
C       set character values
        CVAL(1,1)= TUNITS
        CALL QSETCO (I1,CVAL)
C       edit screen of values
        CALL Q1EDIT (IRET)
        IF (IRET.EQ.1) THEN
C         user wants to continue
          SGRP= 0
C         get integer values
          CALL QGETI (INUM,
     O                IVAL)
C         get character values
          CALL QGETCO (I1,
     O                 CVAL)
C         check starting date
          CALL DATCHK (IVAL,ERR)
          DO 20 I= 1,6
            IF (ERR(I).GT.0) THEN
C             bad start date specified
              SGRP= 23
            END IF
 20       CONTINUE
          IF (SGRP.EQ.0) THEN
C           check ending date
            CALL DATCHK(IVAL(7),ERR)
            DO 30 I= 1,6
              IF (ERR(I).GT.0) THEN
C               bad start date specified
              SGRP= 24
              END IF
 30         CONTINUE
          END IF
          IF (SGRP.EQ.0) THEN
C           check order of dates
            CALL CKDATE(IVAL(1),IVAL(7),ERR(1))
            IF (ERR(1).GE.0) THEN
C             ending date not later than starting date
              SGRP= 25
            END IF
          END IF
          IF (SGRP.EQ.0) THEN
C           dates check out ok
            CALL COPYI (I6,IVAL,SDATE)
            CALL COPYI (I6,IVAL(7),EDATE)
            TSTEP = IVAL(13)
            TUNITS= CVAL(1,1)
C
C           get Totals or Rates for each data set
            CALL ZWNSOP (I1,PTHNAM)
C           init screen
            SGRP= 22
            CALL Q2INIT (MESSFL,SCLU,SGRP)
C           set character values
            DO 40 I= 1,CNMDSN
              CVAL(1,I)= TRANS(I)+ 1
 40         CONTINUE
            CALL Q2STCO (I1,CNMDSN,CVAL)
            CALL Q2SETI (I1,CNMDSN,DSN)
C           edit screen
            CALL Q2EDIT (CNMDSN,
     O                   IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue
              CALL Q2GTCO (I1,CNMDSN,
     O                     CVAL)
              DO 50 I= 1,CNMDSN
                TRANS(I)= CVAL(1,I)- 1
 50           CONTINUE
            END IF
          ELSE
C           problem with dates
            CALL ZWNSOP (I1,PTHNAM)
            CALL PRNTXT (MESSFL,SCLU,SGRP)
C           set IRET to go back to 1st screen
            IRET= 2
          END IF
        ELSE
C         user wants main list menu
          IRET= 1
        END IF
      IF (IRET.EQ.2) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSLOUT
     I                   (MESSFL,SCLU,PTHNAM,NUMDSN,
     M                    CNMDSN)
C
C     + + + PURPOSE + + +
C     Specify output options for time-series data listing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,NUMDSN,CNMDSN
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     PTHNAM - character string of path of options selected to get here
C     NUMDSN - number of available data sets in buffer
C     CNMDSN - number of data sets in buffer to use for listing
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctslst.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I0,RETCOD,SGRP,IRET
      CHARACTER*25 WNDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER    ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZLNTXT, OPTOUT, ZSTCMA, ZGTRET, PRNTXT
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
C
      I= ZLNTXT(PTHNAM(1))
      IF (I.GT.0) THEN
C       something in path to insert into window name
        WNDNAM= 'Output ('//PTHNAM(1)(1:I)//'LO)'
      ELSE
C       no path
        WNDNAM= 'Output (LO)'
      END IF
      CALL OPTOUT (MESSFL,WNDNAM,
     M             TORF,LINES,WIDTH,
     O             FUNIT,RETCOD)
      IF ((TORF.EQ.1 .AND. NUMDSN.GT.7) .OR.
     $    (TORF.EQ.2 .AND. NUMDSN.GT.MAXD)) THEN
C       too many data sets in buffer, let user know
        IF (TORF.EQ.1) THEN
C         too many to screen
          SGRP= 3
          CNMDSN= 7
        ELSE
C         too many to file
          SGRP= 4
          CNMDSN= MAXD
        END IF
C       turn off Previous command
        I= 4
        CALL ZSTCMA (I,I0)
        CALL PRNTXT (MESSFL,SCLU,SGRP)
C       get user exit command value
        CALL ZGTRET (IRET)
      ELSE
C       use all available data sets
        CNMDSN= NUMDSN
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSLSUM
     I                   (MESSFL,SCLU,PTHNAM)
C
C     + + + PURPOSE + + +
C     Specify summary options for time-series data listing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     PTHNAM - character string of path of options selected to get here
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctslst.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1,SGRP,CVAL(5),IRET
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZWNSOP, Q1INIT, Q1EDIT, QSETCO, QGETCO, PRNTXT
      EXTERNAL   QRESPI, ZGTRET
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C
      IF (LORS .EQ. 1) THEN
C       select summary options (TOTAVE(5)) (1=None,2=Sum,3=Ave)
C          5 = Hourly sum     4 = Daily sum      3 = Monthly sum
C          2 = Annual sum     1 = Grand total
 10     CONTINUE
C         get here on prev from month to end year screen
          CALL ZWNSOP (I1,PTHNAM)
C         init screen
          SNUM = 7 - TUNITS
          SGRP = 50+ SNUM
          CALL Q1INIT (MESSFL,SCLU,SGRP)
C         set summary fields
          DO 50 I= 1,SNUM
            CVAL(I)= TOTAVE(I)+ 1
 50       CONTINUE
          CALL QSETCO (SNUM,CVAL)
C         edit screen
          CALL Q1EDIT (IRET)
          IF (IRET.EQ.1) THEN
C           user wants to continue, get edited values
            CALL QGETCO (SNUM,
     O                   CVAL)
            DO 100 I= 1,SNUM
              TOTAVE(I) = CVAL(I)- 1
 100        CONTINUE
C
            IF (TOTAVE(2).GT.0) THEN
C             which month to end year
              CALL ZWNSOP (I1,PTHNAM)
              SGRP = 56
              CALL QRESPI (MESSFL,SCLU,SGRP,ENDMO)
C             get user exit command value
              CALL ZGTRET (IRET)
            END IF
          ELSE
C           user wants back to main List menu
            IRET= 1
          END IF
        IF (IRET.EQ.2) GO TO 10
      ELSE
C       screening values, no summaries allowed
        CALL ZWNSOP (I1,PTHNAM)
        SGRP= 57
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSLORS
     I                   (MESSFL,SCLU,PTHNAM)
C
C     + + + PURPOSE + + +
C     Specify Listing or Screening options for time-series data listing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     PTHNAM - character string of path of options selected to get here
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctslst.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I1,SGRP,INUM,RNUM,IVAL(3),IRET
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZWNSOP, QRESP, ZGTRET, Q1INIT, Q1EDIT
      EXTERNAL   QSETI, QSETR, QGETI, QGETR
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C
C     List(1) or Screen(2) timeseries
 10   CONTINUE
C       back here on prev from 2nd screen
        CALL ZWNSOP (I1,PTHNAM)
        SGRP = 41
        CALL QRESP (MESSFL,SCLU,SGRP,LORS)
C       get user exit command value
        CALL ZGTRET (IRET)
        IF (IRET.EQ.1) THEN
C         user wants to continue, select listing options
          CALL ZWNSOP (I1,PTHNAM)
          IF (LORS .EQ. 1) THEN
C           listing options for good data values
C           1 = low value threshold       4 = decimal places
C           2 = high value threshold      5 = significant digits
C           3 - quality flag
            RNUM = 2
            SGRP = 42
          ELSE
C           select outliers listing options
C           1 = low value threshold       5 = quality flag
C           2 = high value threshold      6 = decimal places
C           3 = percent change            7 = significant digits
C           4 = absolute change
            RNUM = 4
            SGRP = 43
          END IF
C         init screen
          CALL Q1INIT (MESSFL,SCLU,SGRP)
C         set real fields
          CALL QSETR (RNUM,THRSH)
C         set integer fields
          INUM = 3
          IVAL(1) = QFLG
          IVAL(2) = DPLA
          IVAL(3) = SGFD
          CALL QSETI (INUM,IVAL)
C         edit screen
          CALL Q1EDIT (IRET)
          IF (IRET.EQ.1) THEN
C           user wants to continue, get edited values
            CALL QGETI (INUM,
     O                  IVAL)
            QFLG = IVAL(1)
            DPLA = IVAL(2)
            SGFD = IVAL(3)
            CALL QGETR (RNUM,
     O                  THRSH)
          END IF
        ELSE
C         user wants back to main List menu
          IRET= 1
        END IF
      IF (IRET.EQ.2) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSLHDR
     I                   (MESSFL,SCLU,NUMDSN,DSN,MXNHDR,DSNAME,
     O                    TITLE,HEADR,NUMHDR)
C
C     + + + PURPOSE + + +
C     Build headers for time-series listing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,NUMDSN,DSN(NUMDSN),MXNHDR,NUMHDR
      CHARACTER*1 DSNAME(8,NUMDSN),TITLE(78),HEADR(250,MXNHDR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - cluster number on message file
C     NUMDSN - number of data sets being listed
C     DSN    - array of data-set numbers being listed
C     MXNHDR - max number of header rows
C     DSNAME - character arrays of data-set names
C     TITLE  - title for time-series listing
C     HEADR  - character array of headers
C     NUMHDR - actual number of header rows to be listed
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctslst.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,M,I0,ID,ILEN,OLEN,SGRP
      CHARACTER*1 BLNK(1),TBUFF(250)
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZIPC, GETTXT, INTCHR, CHRCHR
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      BLNK(1) = ' '
C
C     init output arguments
      ILEN = 78
      CALL ZIPC (ILEN,BLNK,TITLE)
      ILEN = 250 * MXNHDR
      CALL ZIPC (ILEN,BLNK,HEADR)
      NUMHDR = 2
C     create header lines
      ILEN= (WIDTH-22)/NUMDSN
      IF (ILEN.GT.12) ILEN = 12
      I  = 26 + ILEN*NUMDSN
      IF (I.LT.WIDTH) WIDTH = I
      SGRP= 63
      I   = 42
      CALL GETTXT (MESSFL,SCLU,SGRP,
     M             I,
     O             TBUFF)
      I= 21
C     data set number
      CALL CHRCHR (I,TBUFF,HEADR(1,1))
C     data set name
      CALL CHRCHR (I,TBUFF(22),HEADR(1,2))
      IF (ILEN .LT. 8) THEN
        K = ILEN - 1
      ELSE
        K = 8
      END IF
      DO 50 ID = 1,NUMDSN
C       put data set numbers and names in header line
        M= 22 + ILEN*(ID-1)
        CALL INTCHR (DSN(ID),ILEN,I0,
     O               OLEN,HEADR(M,1))
        J= 22 + ILEN-K +ILEN*(ID-1)
        CALL CHRCHR (K,DSNAME(1,ID),HEADR(J,2))
 50   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   TIMPRM
     I                   (WDMSFL,NUMDSN,DSN,
     O                    DSNAME)
C
C     + + + PURPOSE + + +
C     Set default time parameter values for time-series listing
C     based on selected data sets.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,NUMDSN,DSN(NUMDSN)
      CHARACTER*1 DSNAME(8,NUMDSN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     NUMDSN - number of data sets
C     DSN    - array of data-set numbers
C     DSNAME - array of text identifiers for each data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctslst.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I1,SAIND,SALEN,RETCOD,DSFREC,MINDEL,MINCOD,
     $            TSTEPF,TCDCMP,ISTA,SDATIM(6,MAXD),EDATIM(6,MAXD),
     $            DSDELT(MAXD),TCODE(MAXD),GRPSIZ(MAXD),FLG,IND
      CHARACTER*1 TSTYPE(4),BLNK(1)
      CHARACTER*8 CTMP
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDBSGI, WDBSGC, WTFNDT, CMPTIM, DLIMIT, CKDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (I8)
 2005 FORMAT (8A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      BLNK(1)= ' '
C
      DO 5 I = 1,6
        EDATIM(6,I) = 0
        SDATIM(6,I) = 0
 5    CONTINUE
C
C     set defaults based on selected data sets
      DO 10 I= 1,NUMDSN
C       get time step
        SALEN= 1
        SAIND= 33
        CALL WDBSGI (WDMSFL,DSN(I),SAIND,SALEN,
     O               DSDELT(I),RETCOD)
        IF (RETCOD.NE.0) THEN
C         set time step to default of 1
          DSDELT(I)= 1
        END IF
C       get time units
        SAIND= 17
        CALL WDBSGI (WDMSFL,DSN(I),SAIND,SALEN,
     O               TCODE(I),RETCOD)
        IF (RETCOD.NE.0) THEN
C         set to default of daily time units
          TCODE(I)= 4
        END IF
C       get data-set name
        SAIND= 51
        CALL WDBSGI (WDMSFL,DSN(I),SAIND,SALEN,
     O               ISTA,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         put station ID in temp variable
          WRITE (CTMP,2000) ISTA
        ELSE
C         get time-series type
          SAIND= 1
          SALEN= 4
          CALL WDBSGC (WDMSFL,DSN(I),SAIND,SALEN,
     O                 TSTYPE,RETCOD)
          IF (RETCOD.EQ.0) THEN
C           put time-series type in temp variable
            WRITE (CTMP,2005) (BLNK(1),J=1,4),TSTYPE
          ELSE
C           put blank in temp variable
            CTMP= ' '
          END IF
        END IF
        READ (CTMP,2005) (DSNAME(J,I),J=1,8)
C       get start and end dates for each data set
        CALL WTFNDT (WDMSFL,DSN(I),I1,
     O               DSFREC,SDATIM(1,I),EDATIM(1,I),RETCOD)
 10   CONTINUE
C
C     find smallest delta time
      MINDEL = DSDELT(1)
      MINCOD = TCODE(1)
       write (99,*)'DSDELT,TCODE',DSDELT(1),TCODE(1)
      IF (NUMDSN .GT. 1) THEN
        DO 20 I = 2,NUMDSN
          CALL CMPTIM (MINCOD,MINDEL,TCODE(I),DSDELT(I),
     O                 TSTEPF,TCDCMP)
       write (99,*)'DSDELT,TCODE,TCDCMP',DSDELT(I),TCODE(I),TCDCMP
            IF (TCDCMP .EQ. 2) THEN
C             next timestep shorter, so change minimum
              MINDEL = DSDELT(I)
              MINCOD = TCODE(I)
            END IF
 20     CONTINUE
      END IF
      TUNITS = MINCOD
      TSTEP  = MINDEL
       write (99,*)'TSTEP,TUNITS',TSTEP,TUNITS
C
C     find latest start date
      IND = 2
      CALL DLIMIT (SDATIM,NUMDSN,IND,SDATE)
C
C     find earliest end date
      IND = 1
      CALL DLIMIT (EDATIM,NUMDSN,IND,EDATE)
C
C     check start vs end date
      CALL CKDATE (SDATE,EDATE,FLG)
      IF (FLG .EQ. 1) THEN
C       end date before start date, default to first dsn
        DO 30 I = 1,6
          SDATE(I) = SDATIM(I,1)
          EDATE(I) = EDATIM(I,1)
 30     CONTINUE
      END IF
C
C     find largest group size of the DSNs
      GRPMAX = 1
      DO 40 I = 1,NUMDSN
        SALEN = 1
        SAIND = 34
        CALL WDBSGI (WDMSFL,DSN(I),SAIND,SALEN,
     O               GRPSIZ(I),RETCOD)
        IF (RETCOD.NE.0) THEN
C         could not get group size, set to default
          GRPSIZ(I) = 6
        END IF
        IF (GRPSIZ(I).GT.GRPMAX) THEN
C         new max group size
          GRPMAX = GRPSIZ(I)
        END IF
 40   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMTL
     I                    (MESSFL,WDMSFL,PTHNAM,NUMDSN,DSN)
C
C     + + + PURPOSE + + +
C     List information on up to 30 timeseries from a WDM file.
C     The user is asked to specify formats and the optional sums
C     to be output. Calls time-series list routine without
C     allowing editing of data values.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,WDMSFL,NUMDSN,DSN(NUMDSN)
      CHARACTER*8 PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     PTHNAM - character string of path of options selected to get here
C     NUMDSN - number of data sets in buffer
C     DSN    - array of data-set numbers in buffer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    EDITFG
C
C     + + + EXTERNALS + + +
      EXTERNAL   TSLIST
C
C     + + + END SPECIFICATIONS + + +
C
      EDITFG = 0
      CALL TSLIST (MESSFL,WDMSFL,PTHNAM,NUMDSN,DSN,EDITFG)
C
      RETURN
      END

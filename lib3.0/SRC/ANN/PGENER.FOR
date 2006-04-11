C
C
C
      SUBROUTINE   AGENER
     I                   (MESSFL,WDMSFL,MXNDSN,PTHNAM,
     O                    DSNCNT,DSNBUF)
C
C     + + + PURPOSE + + +
C     This routine and the routines it calls are used to perform time
C     shift and math transformations on time series data.
C
C     + + + KEYWORDS + + +
C     WDM file, message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL, WDMSFL, MXNDSN, DSNCNT, DSNBUF(MXNDSN)
      CHARACTER*8  PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     MXNDSN - max number of data sets that can be added in one gener session
C     DSNCNT - count of data sets added in this gener session
C     DSNBUF - buffer of data set numbers added
C     PTHNAM - name of path chosen to get here
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU, SGRP, OPTION, I0, I1, I6, TARDSN, TSDATE(6),
     $          SSDATE(6), EDATE(6), QUALCD, TSTEP, SRCDSN, I, I3,
     $          USEINT, TUNIT, STATUS, TRANSF, IVAL(22), RETFLG,
     $          INUM, OPCNT, OPLEN, MNSEL(2), OPVAL(2), RNUM, I4,
     $          CVAL(5), CNUM, RESP, I10, I80, NEWDSN, J, DSNT, DSNS
      REAL      RVAL(1), VALUE
      CHARACTER*1 CSTR1(80),BLNK,CTXT(4)
      CHARACTER*4 WDIDT,WDIDS
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPI, ZSTCMA, Q1INIT, COPYI, QSETI, QSETOP, QSETR
      EXTERNAL  QSETCO, Q1EDIT, QGETI, QGETOP, QGETR, QGETCO, ZIPC
      EXTERNAL  GNMATH, GNTIME, GNMANU, INTCHR, ZCBSST, QSETCB, GNATTR
      EXTERNAL  WUA2ID, WID2UA, CVARAR, QSTCTF, QGTCTF, CARVAR, ZWNSOP
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
      I3    = 3
      I4    = 4
      I6    = 6
      I10   = 10
      I80   = 80
      BLNK  = ' '
C     initialize count and buffer of data sets added
      DSNCNT= 0
      CALL ZIPI (MXNDSN,I0,DSNBUF)
C
C     initialize values for screen
      TARDSN = 0
      CALL ZIPI (I6,I0,TSDATE)
      CALL ZIPI (I6,I0,SSDATE)
      CALL ZIPI (I6,I0,EDATE)
      QUALCD = 0
      TSTEP  = 0
      SRCDSN = 0
      OPTION = 4
      USEINT = 0
      VALUE  = 0.0
      TUNIT  = 0
      STATUS = 0
      TRANSF = 0
C
      SCLU = 42
      CALL ZWNSOP (I1,PTHNAM)
C
 100  CONTINUE
        RETFLG = 0
C       do 'generate' screen
C       make previous available
        I= 4
        CALL ZSTCMA (I,I1)
        SGRP   = 1
        CALL Q1INIT (MESSFL,SCLU,SGRP)
C       set field call back
        CALL QSETCB (I0,I1,I0)
C       set initial integer values
C       translate dsn as an id to a wdm file and data set number
        CALL WID2UA (I0,TARDSN,
     O               J,DSNT,WDIDT)
        IVAL(1) = DSNT
        CALL COPYI (I6,TSDATE,IVAL(2))
        CALL COPYI (I6,SSDATE,IVAL(8))
        CALL COPYI (I6,EDATE,IVAL(14))
        IVAL(20) = TSTEP
        IVAL(21) = QUALCD
C       translate dsn as an id to a wdm file and data set number
        CALL WID2UA (I0,SRCDSN,
     O               J,DSNS,WDIDS)
        IVAL(22) = DSNS
        INUM = 22
        CALL QSETI (INUM,IVAL)
C       set option field initial values
        OPCNT = 2
        OPLEN = 2
        MNSEL(1) = 1
        MNSEL(2) = 1
        OPVAL(1) = OPTION
        OPVAL(2) = USEINT
        CALL QSETOP (OPCNT,OPLEN,MNSEL,MNSEL,OPVAL)
C       set initial real values
        RVAL(1) = VALUE
        RNUM = 1
        CALL QSETR (RNUM,RVAL)
C       set initial character values
        CVAL(1) = 0
        CVAL(2) = TUNIT
        CVAL(3) = 0
        CVAL(4) = STATUS
        CVAL(5) = TRANSF
        CNUM = 5
        CALL QSETCO (CNUM,CVAL)
        CALL CVARAR (I4,WDIDT,I4,CTXT(1))
        CALL QSTCTF (I1,I4,CTXT)
        CALL CVARAR (I4,WDIDS,I4,CTXT(1))
        CALL QSTCTF (I3,I4,CTXT)
C       put wdm file unit number into common for call back
        CALL ZIPC (I80,BLNK,CSTR1)
        CALL INTCHR (WDMSFL,I10,I0,I80,CSTR1)
        CALL ZCBSST (CSTR1)
C       now edit 'generate' screen
        CALL Q1EDIT (
     O               RESP)
        IF (RESP.EQ.1) THEN
C         user wants to continue
          RETFLG = 1
C         get integer values
          CALL QGETI (INUM,
     O                IVAL)
          DSNT   = IVAL(1)
          CALL COPYI (I6,IVAL(2),TSDATE)
          CALL COPYI (I6,IVAL(8),SSDATE)
          CALL COPYI (I6,IVAL(14),EDATE)
          TSTEP  = IVAL(20)
          QUALCD = IVAL(21)
          DSNS   = IVAL(22)
C         get option values
          CALL QGETOP (OPLEN,
     O                 OPVAL)
          OPTION = OPVAL(1)
          USEINT = OPVAL(2)
C         get real values
          CALL QGETR (RNUM,
     O                RVAL)
          VALUE = RVAL(1)
C         get character values
          CALL QGETCO (CNUM,
     O                 CVAL)
          CALL QGTCTF (I1,I4,CTXT)
          CALL CARVAR (I4,CTXT,I4,WDIDT)
C         translate dsn and wdm file id to a dsnid
          J = 0
          CALL WUA2ID (J,DSNT,WDIDT,
     O                 TARDSN)
          CALL QGTCTF (I3,I4,CTXT)
          CALL CARVAR (I4,CTXT,I4,WDIDS)
C         translate dsn and wdm file id to a dsnid
          J = 0
          CALL WUA2ID (J,DSNS,WDIDS,
     O                 SRCDSN)
          TUNIT  = CVAL(2)
          STATUS = CVAL(4)
          TRANSF = CVAL(5)
          NEWDSN = 0
          IF (OPTION .EQ. 1) THEN
C           compute math functions
            CALL GNMATH (MESSFL,SCLU,WDMSFL,TARDSN,TSDATE,EDATE,
     M                   NEWDSN)
          ELSE IF (OPTION .EQ. 2) THEN
C           transform time step and/or shift in time
            CALL GNTIME (MESSFL,SCLU,WDMSFL,TARDSN,SRCDSN,STATUS,
     I                   TSDATE,EDATE,SSDATE,TRANSF,TSTEP,TUNIT,
     M                   NEWDSN)
          ELSE IF (OPTION .EQ. 3) THEN
C           manually manipulate data
            CALL GNMANU (MESSFL,SCLU,WDMSFL,TARDSN,TSTEP,TUNIT,TSDATE,
     I                   EDATE,USEINT,VALUE,
     M                   NEWDSN)
          ELSE IF (OPTION .EQ. 4) THEN
C           user wants to return one level up
            RETFLG = 0
          END IF
          IF (NEWDSN.NE.0) THEN
C           created a new data set, let user modify attributes
            CALL GNATTR (MESSFL,SCLU,WDMSFL,NEWDSN)
            IF (DSNCNT.LT.MXNDSN) THEN
C             return buffer of newly created data set numbers
              DSNCNT = DSNCNT + 1
              DSNBUF(DSNCNT) = NEWDSN
            END IF
          END IF
        END IF
      IF (RETFLG.EQ.1) GO TO 100

C     make previous unavailable
      I= 4
      CALL ZSTCMA (I,I0)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GNMATH
     I                   (MESSFL,SCLU,WDMSFL,TARDSN,TSDATE,EDATE,
     M                    NEWDSN)
C
C     + + + PURPOSE + + +
C     Create a new time series from an existing time series by
C     transforming the time series by mathematical functions.
C     The user is asked to specify input and output data sets, the
C     time period to be transformed, and the type of transformation.
C
C     + + + KEYWORDS + + +
C     WDM file, message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,TARDSN,TSDATE(6),EDATE(6),SCLU,
     1          NEWDSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - screen cluster number
C     WDMSFL - Fortran unit number of WDM file
C     TARDSN - target data set number
C     TSDATE - target starting date
C     EDATE  - target ending date
C     NEWDSN - flag indicating if we have added a data set
C
C     + + + PARAMETERS + + +
      INTEGER    KSIZ
      PARAMETER (KSIZ = 100)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I, J, SGRP, RESP, FCTN, NIN, DSN(3), I0, I64, I1,
     $            TSTEP(3), TUNIT(3), TBLSIZ, ERRFLG, I4, K1, LEN,
     $            INIT, IWRT, IVAL(4), KOUNT, INUM, RNUM, NDSN(24),
     $            SRCDS1, SRCDS2, SIGFIG, OPCNT, OPLEN, MNSEL(1), JLEN,
     $            OPVAL(1), CNUM, CLEN(5), TLEN, FLAG, IN, DREC, RETC,
     $            START(6,3), STOP(6,3), I5, FLTABL, I2, I80, I10, LDSN,
     $            LWDMFL, WDMFL2, DSN2
      REAL        C1(KSIZ),C2(KSIZ),RVAL(2),SUM,CONST1,CONST2
      CHARACTER*1 BLNK,CTXT(126),DEC(1),CSTR1(80)
      CHARACTER*4 WDID1,WDID2
      CHARACTER*23 WNDNAM
      CHARACTER*64 FNAME
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR, STRFND, CHRINT
      REAL        CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZSTCMA, PRTERR, WDDSCK, WDDSCL, WDBSGI, WTFNDT
      EXTERNAL    GNMEXE, PMXTXR, PMXTXI, PMXCNW, PRNTXT, CHRDEC
      EXTERNAL    Q1INIT,QSETOP,QSETI,QSETR,ZIPC,QSETCT,Q1EDIT
      EXTERNAL    QGETOP,QGETI,QGETR,QGETCT,LENSTR,STRFND,CHRINT
      EXTERNAL    CARVAR,GETFUN,GETTXT,QFCLOS,QSETCB,INTCHR,ZCBSST
      EXTERNAL    WUA2ID,WID2UD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  NDSN / 0,2,2,3,3,3,3,3,3,2,2,3,2,2,2,2,2,2,3,3,2,2,2,2 /
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
      I2    = 2
      I4    = 4
      I5    = 5
      I10   = 10
      I64   = 64
      I80   = 80
      FCTN  = 1
      SRCDS1= 0
      SRCDS2= 0
      SIGFIG= 0
      CONST1= 0.0
      CONST2= 0.0
      BLNK  = ' '
C
C     allow Previous command
      I= 4
      J= 1
      CALL ZSTCMA (I,J)
C
C     do second screen for compute
      SGRP   = 2
      CALL Q1INIT (MESSFL,SCLU,SGRP)
C     set character call back
      CALL QSETCB (I1,I0,I0)
C
C     set option field initial values for math function
      OPCNT   = 1
      OPLEN   = 1
      MNSEL(1)= 1
      OPVAL(1)= FCTN
      CALL QSETOP (OPCNT,OPLEN,MNSEL,MNSEL,OPVAL)
C     set initial integer values
      IVAL(1)= SRCDS1
      IVAL(2)= SRCDS2
      IVAL(3)= SIGFIG
      INUM   = 3
      CALL QSETI (INUM,IVAL)
C     set initial real values
      RVAL(1)= CONST1
      RVAL(2)= CONST2
      RNUM   = 2
      CALL QSETR (RNUM,RVAL)
C     set initial character values
      CNUM   = 5
      CLEN(1)= 64
      CLEN(2)= 4
      CLEN(3)= 4
      CLEN(4)= 5
      CLEN(5)= 49
      TLEN   = 126
      CALL ZIPC (TLEN,BLNK,CTXT)
      SGRP   = 51 + FCTN
      CALL GETTXT (MESSFL,SCLU,SGRP,CLEN(1),CTXT(1))
      CLEN(1)= 64
      CALL QSETCT (CNUM,CLEN,TLEN,CTXT)
C     put message file unit number, cluster number for call back
      CALL ZIPC (I80,BLNK,CSTR1)
      CALL INTCHR (MESSFL,I10,I0,JLEN,CSTR1(1))
      CALL INTCHR (SCLU,I10,I0,JLEN,CSTR1(11))
      CALL ZCBSST (CSTR1)
C
C     now edit second screen
      CALL Q1EDIT (
     O             RESP)
C
C     turn off Previous command
      I= 4
      J= 0
      CALL ZSTCMA (I,J)
C
      IF (RESP.EQ.1) THEN
C       user wants to continue, get values from screen
        CALL QGETOP (OPLEN,
     O               OPVAL)
        FCTN = OPVAL(1)
        IF (FCTN.NE.1) THEN
C         not 'NONE', get integer values
          CALL QGETI (INUM,
     O                IVAL)
          SRCDS1 = IVAL(1)
          SRCDS2 = IVAL(2)
          SIGFIG = IVAL(3)
C         get real values
          CALL QGETR (RNUM,
     O                RVAL)
          CONST1 = RVAL(1)
          CONST2 = RVAL(2)
C         get character values
          CLEN(1)= 64
          CLEN(2)= 4
          CLEN(3)= 4
          CLEN(4)= 5
          CLEN(5)= 49
          TLEN   = 126
          CALL ZIPC (TLEN,BLNK,CTXT)
          CALL QGETCT (CNUM,CLEN,TLEN,
     O                 CTXT)
          DSN(1)= TARDSN
          CALL CARVAR (I4,CTXT(65),I4,WDID1)
C         translate dsn and wdm file id to a dsnid
          J = 0
          CALL WUA2ID (J,SRCDS1,WDID1,
     O                 LDSN)
          DSN(2)= LDSN
          CALL CARVAR (I4,CTXT(69),I4,WDID2)
C         translate dsn and wdm file id to a dsnid
          J = 0
          CALL WUA2ID (J,SRCDS2,WDID2,
     O                 LDSN)
          DSN(3)= LDSN
          NIN = NDSN(FCTN) - 1
          IN = NIN + 1
          FLAG = 0
          DO 120 I = 1, IN
C           check if dsn exists
            CALL WID2UD (I0,DSN(I),
     O                   LWDMFL,LDSN)
            CALL WDDSCK (LWDMFL,LDSN,
     O                   DREC,RETC)
            IF (RETC.LT.0 .AND. I.EQ.1) THEN
C             output data set does not exist, copy input attrib
              CALL WID2UD (I0,DSN(2),
     O                     WDMFL2,DSN2)
              CALL WDDSCL (WDMFL2,DSN2,LWDMFL,LDSN,I0,RETC)
              IF (RETC .NE. 0) THEN
C               attributes not successfully copied
                FLAG = FLAG + 1
              ELSE
C               create and copy ok, let user know
                IVAL(1)= DSN(1)
                IVAL(2)= DSN(2)
                SGRP= 90
                INUM= 2
                CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,I0,INUM,IVAL)
                NEWDSN = DSN(1)
              END IF
            ELSE IF (RETC .LT. 0) THEN
C             input data set does not exist
              FLAG = FLAG + 1
              SGRP = 83
              CALL PMXTXI (MESSFL,SCLU,SGRP,I4,I1,I0,
     $                     I1,DSN(I))
            END IF
            IF (RETC .EQ. 0) THEN
C             data set exists, get details
              CALL WDBSGI (LWDMFL,LDSN,33,I1,TSTEP(I),RETC)
              CALL WDBSGI (LWDMFL,LDSN,17,I1,TUNIT(I),RETC)
              CALL WTFNDT (LWDMFL,LDSN,I1,DREC,
     O                      START(1,I),STOP(1,I),RETC)
            END IF
 120      CONTINUE
          IF (FLAG .EQ. 0) THEN
C           check for compatible time steps
            DO 140 I = 2, NDSN(FCTN)
              IF (TUNIT(1).NE.TUNIT(I) .OR. TSTEP(1).NE.TSTEP(I)) THEN
C               time units/steps not the same
                FLAG = FLAG + 1
                SGRP = 84
                IVAL(1) = TUNIT(1)
                IVAL(2) = TSTEP(1)
                IVAL(3) = TUNIT(I)
                IVAL(4) = TSTEP(I)
                CALL PMXTXI (MESSFL,SCLU,SGRP,I4,I1,I0,I4,IVAL)
              END IF
 140        CONTINUE
          END IF
C
          IF (FLAG .EQ. 0) THEN
C           set coefficients
            K1 = DSN(3)
            C1(1) = CONST1
            C2(1) = CONST2
            IF (FCTN .EQ. 2) C2(1) = C1(1)
            IF (FCTN .EQ. 10) THEN
C             time series raised to a power, check for decimal
              LEN   = LENSTR (I5,CTXT(73))
              DEC(1)= '.'
              I     = STRFND (LEN,CTXT(73),I1,DEC)
              IF (I .EQ. 0) THEN
C               an integer constant
                K1 = CHRINT (LEN,CTXT(73))
              ELSE
C               a real constant
                C1(1) = CHRDEC (LEN,CTXT(73))
                K1 = 0
              END IF
            END IF
            IF (FCTN .EQ.22) THEN
C             sig figs
              K1 = SIGFIG
            END IF
            IF (FLAG.EQ.0 .AND. FCTN.EQ.24) THEN
C             File for table?
              CALL GETFUN (I1,FLTABL)
              CALL CARVAR (CLEN(3),CTXT(78),I64,FNAME)
              OPEN(UNIT=FLTABL,FILE=FNAME,ERR=150,STATUS='OLD')
              GO TO 155
 150          CONTINUE
C             flag error in file opening
              FLAG = 1
 155          CONTINUE
              I = 0
 162          CONTINUE
                I = I + 1
                READ (FLTABL,*,END=165,ERR=165) C1(I), C2(I)
                IF (I.GT.1) THEN
C                 C1 must be increasing function
                  IF (C1(I) .LE. C1(I-1)) THEN
C                   problem with C1 values
                    FLAG= 1
                  END IF
                END IF
              IF (I .LT. KSIZ) GO TO 162
              I = I + 1
 165          CONTINUE
              TBLSIZ = I - 1
              IF (TBLSIZ .LE. 1) THEN
C               not enough values read
                FLAG= 1
              END IF
              IF (FLAG.NE.0) THEN
C               had problem with table file
                SGRP= 89
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              END IF
C             close table file
              I= 0
              CALL QFCLOS (FLTABL,I)
            ELSE IF (FLAG.EQ.0) THEN
C             table size is 1 for all other options
              TBLSIZ= 1
            END IF
C
            IF (FLAG.EQ.0) THEN
C             everything okay, perform computation
              CALL GNMEXE (WDMSFL,DSN,FCTN,NIN,TSTEP,TUNIT,TSDATE,EDATE,
     I                     TBLSIZ,C1,C2,K1,
     O                     ERRFLG,SUM,KOUNT)
C
              IF (ERRFLG.NE.0) THEN
C               problem with generate
                WNDNAM= 'Execute (DTGCE) Problem'
                CALL PRTERR (MESSFL,WNDNAM,ERRFLG)
              ELSE
C               generate performed successfully
                INIT= 1
                IWRT= -1
                I   = 1
                IF (FCTN .EQ. 21) THEN
C                 output sum of timeseries
                  SGRP = 85
                  RVAL(1)= SUM
                  CALL PMXTXR (MESSFL,SCLU,SGRP,I,INIT,IWRT,I,RVAL)
                  INIT = -1
                END IF
                IF (KOUNT .GT. 0) THEN
C                 some values out of range
                  SGRP = 86
                  IVAL(1)= KOUNT
                  CALL PMXTXI (MESSFL,SCLU,SGRP,I,INIT,IWRT,I,IVAL)
                  INIT = -1
                END IF
C               generate complete
                SGRP = 87
                IWRT = 0
                CALL PMXCNW (MESSFL,SCLU,SGRP,I,INIT,IWRT,J)
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
      SUBROUTINE   GNTIME
     I                   (MESSFL,SCLU,WDMSFL,TARDSN,SRCDSN,NAORO,
     I                    TSDATE,EDATE,SSDATE,TRNIDX,TSTEP,TUNIT,
     M                    NEWDSN)
C
C     + + + PURPOSE + + +
C     Create a new time series from an existing time series by shifting
C     the time series in time and/or aggregating or disaggregating the
C     time series over time.  The user is asked to specify the time 
C     period to be transformed, the shifted start date, time step and
C     units for the new time series, the number of the output data set,
C     and the current status of the output data set.
C
C     + + + KEYWORDS + + +
C     WDM file, message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMSFL, SCLU, TARDSN, SRCDSN, NAORO, NEWDSN,
     $          TSDATE(6), EDATE(6), SSDATE(6), TRNIDX, TSTEP, TUNIT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SCLU   - screen cluster number
C     TARDSN - data-set number of target
C     SRCDSN - data-set number of source
C     NAORO  - status of target; new, append, or overwrite
C     TSDATE - target starting date
C     SSDATE - source starting date
C     EDATE  - source ending date
C     TRNIDX - transformation function index
C     TSTEP  - time step of the data set
C     TUNIT  - time units of the data set
C     NEWDSN - flag indicating if we have added a data set
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SGRP, I6, DSN(2), DATES(6,3), DUMDAT(6,2), ITMP(6),
     $          INIT, IWRT, L, MAXL, CRTFLG, CMPFLG, RETCOD, IVAL(3),
     $          DTRVAL(6), DTRAN, NUMI, INDXI(2), NUMR, INDXR(1),
     $          PRTFLG, ATRIBI(2), LRET, FLGTS, FLGTU,
     $          FLAG1, FLAG2, I
      REAL      ATRIBR(1)
      CHARACTER*23  WNDNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL  PMXCNW, PMXTXI, COPYI, GNTTRN, PRTERR, CMPTIM, CKBDRY
      EXTERNAL  WDAINF
C
C     + + + DATA INITIALIZATIONS + + +
C                  ave, sum, max, min, sam, div
      DATA DTRVAL / 0,   1,   2,   3,   0,   1 /
C
C     + + + END SPECIFICATIONS + + +
C
      I6   = 6
      MAXL = 10
      INIT = 0
      IWRT = 0
C
      DSN(1) = SRCDSN
      DSN(2) = TARDSN
      CALL COPYI (I6,SSDATE,DATES(1,1))
      CALL COPYI (I6,EDATE,DATES(1,2))
      CALL COPYI (I6,TSDATE,DATES(1,3))
      DTRAN = DTRVAL(TRNIDX)
C
C     do some more checking before executing
      NUMI    = 2
      INDXI(1)= 33
      INDXI(2)= 17
      NUMR    = 1
      INDXR(1)= 32
      PRTFLG  = 0
      CALL WDAINF (WDMSFL,SRCDSN,NUMI,NUMR,INDXI,INDXR,
     O             DUMDAT,ATRIBI,ATRIBR,LRET)
      IF (LRET.NE.0 .AND. LRET.NE.-107) THEN
C       problem with source data set
        IF (LRET .EQ. -6) THEN
C         no data present in data set
          SGRP = 21
          PRTFLG = 1
        ELSE IF (LRET .EQ. -81) THEN
C         data set does not exist
          SGRP = 22
          PRTFLG = 1
        ELSE IF (LRET .EQ. -82) THEN
C         data set exists, but wrong data type
          SGRP = 23
          PRTFLG = 1
        ELSE
C         unknown problem
          SGRP = 24
          PRTFLG = 1
        END IF
      END IF
      IF (PRTFLG.EQ.1) THEN
C       need to write error message
        CALL PMXCNW ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L )
      ELSE
C       need to check target data set
        CALL WDAINF (WDMSFL,TARDSN,NUMI,NUMR,INDXI,INDXR,
     O               DUMDAT,ATRIBI,ATRIBR,LRET)
        IF (NAORO .EQ. 1) THEN
C         expect output data set to not exist
          IF (LRET.NE.-81) THEN
C           problem because it should not exist
            IF (LRET .EQ. 0  .OR.  LRET .EQ. -1  .OR.
     $               LRET .EQ. -107) THEN
C             does exist, but shouldn't
              SGRP = 28
              PRTFLG = 1
            ELSE IF (LRET .EQ. -82) THEN
C             does exist but not time series, shouldn't exist
              SGRP = 26
              PRTFLG = 1
            ELSE
C             unknown problem
              SGRP = 27
              PRTFLG = 1
            END IF
          END IF
        ELSE
C         expect output data set to exist
          IF (LRET .EQ. 0 .OR. LRET .EQ. -6 .OR. LRET .EQ. -107) THEN
C           data set exists, are time steps compatible?
            CALL CMPTIM ( TUNIT, TSTEP, ATRIBI(2), ATRIBI(1),
     O                    FLGTS, FLGTU )
            IF (FLGTS .EQ. 1  .OR.  FLGTU .EQ. -1) THEN
C             time steps are not compatible
              SGRP = 25
              PRTFLG = 1
            END IF
          ELSE IF (LRET .EQ. -81) THEN
C           data set does not exist, but should
            SGRP = 32
            PRTFLG = 1
          ELSE IF (LRET .EQ. -82) THEN
C           dsn exists, but is wrong type, not a time series
            SGRP = 26
            PRTFLG = 1
          ELSE
C           unknown problem
            SGRP = 27
            PRTFLG = 1
          END IF
        END IF
        IF (PRTFLG.EQ.0) THEN
C         are dates compatible with time step
          FLAG1 = 0
          DO 150 I = 1, 3
            CALL COPYI (I6,DATES(1,I),ITMP)
            CALL CKBDRY ( ITMP, TSTEP, TUNIT, FLAG2 )
            IF (FLAG2 .NE. 0) THEN
C             date is not compatible with time step
              FLAG1 = 1
            END IF
 150      CONTINUE
          IF (FLAG1 .EQ. 1) THEN
C           date(s) not compatible with time step
            SGRP = 36
            CALL PMXCNW ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L )
          END IF
        END IF
        IF (PRTFLG.EQ.1) THEN
C         need to write error message
          CALL PMXCNW ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L )
        ELSE IF (FLAG1.EQ.0) THEN
C         input and output defined, ok to transform
          CALL GNTTRN ( MESSFL, WDMSFL, DSN, NAORO,
     I                  DATES, DTRAN, TSTEP, TUNIT,
     O                  CRTFLG, CMPFLG, RETCOD )
C         display messages upon return from execution
          IWRT = 1
          IF (CRTFLG.EQ.1) THEN
C           successfully created new data set
            SGRP   = 15
            L      = 2
            IVAL(1)= DSN(1)
            IVAL(2)= DSN(2)
            CALL PMXTXI (MESSFL,SCLU,SGRP,MAXL,INIT,IWRT,L,IVAL)
            NEWDSN = DSN(2)
          ELSE IF (CRTFLG.EQ.2) THEN
C           problem creating new data set
            SGRP   = 16
            L      = 3
            IVAL(1)= DSN(1)
            IVAL(2)= DSN(2)
            IVAL(3)= RETCOD
            CALL PMXTXI (MESSFL,SCLU,SGRP,MAXL,INIT,IWRT,L,IVAL)
          ELSE IF (CRTFLG.EQ.3) THEN
C           problem with tgroup for existing data set
            SGRP   = 17
            L      = 1
            IVAL(1)= DSN(2)
            CALL PMXTXI (MESSFL,SCLU,SGRP,MAXL,INIT,IWRT,L,IVAL)
          END IF
          INIT = 0
          IWRT = 0
          IF (CMPFLG.EQ.1) THEN
C           all data successfully transformed
            SGRP   = 14
          ELSE IF (CMPFLG.EQ.2) THEN
C           at least some data not transformed, error in get/put
            WNDNAM = 'Execute (DTGTE) Problem'
            CALL PRTERR (MESSFL,WNDNAM,RETCOD)
            SGRP   = 18
          ELSE IF (CMPFLG.EQ.3) THEN
C           none of the data transformed
            SGRP   = 19
          END IF
          CALL PMXCNW (MESSFL,SCLU,SGRP,MAXL,INIT,IWRT,L)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GNMANU
     I                   (MESSFL,SCLU,WDMSFL,TARDSN,TSTEP,TUNIT,TSDATE,
     I                    EDATE,USEINT,VALUE,
     M                    NEWDSN)
C
C     + + + PURPOSE + + +
C     Create a new time series from scratch.
C     The user is asked to specify data set, the time
C     period to be manipulated, and the fill value.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,TARDSN,TSTEP,TUNIT,TSDATE(6),EDATE(6),USEINT,
     $          MESSFL,SCLU,NEWDSN
      REAL      VALUE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - unit number of message file with attributes
C     SCLU   - screen cluster number
C     WDMSFL - Fortran unit number of WDM file
C     TARDSN - data-set number of data set to manipulate
C     TSTEP  - time step of the data set
C     TUNIT  - time units of the data set
C     TSDATE - requested start date and time
C     EDATE  - requested stop date and time
C     USEINT - interpolate(2) or use value(1) flag
C     VALUE  - real coefficient to use as fill value or interpolate to
C     NEWDSN - flag indicating if we have added a data set
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP, ERRCOD, CRECOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    GMANEX, PRNTXT
C
C     + + + END SPECIFICATIONS + + +
C
C     execute computation
      CALL GMANEX (MESSFL,WDMSFL,TARDSN,TSTEP,TUNIT,TSDATE,EDATE,
     I             USEINT,VALUE,
     O             ERRCOD,CRECOD)
      IF (CRECOD.EQ.1) THEN
C       new data set created successfully
        SGRP = 141
        CALL PRNTXT (MESSFL,SCLU,SGRP)
        NEWDSN = TARDSN
      ELSE IF (CRECOD.EQ.2) THEN
C       some problem creating new data set
        SGRP = 142
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      END IF
      IF (ERRCOD.EQ.0) THEN
C       manipulation performed successfully
        SGRP = 143
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      ELSE IF (ERRCOD.EQ.1) THEN
C       problem getting previous value to interpolate
        SGRP = 144
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      ELSE IF (ERRCOD.EQ.2) THEN
C       problem writing to data set
        SGRP = 145
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      ELSE IF (ERRCOD.EQ.3) THEN
C       no data set to get previous value to interpolate
        SGRP = 146
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZCBFL1
     I                   (CFLD,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     Call back routine to set dates based on data set returned.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CFLD,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CFLD   - field number to call back
C     RETCOD - call back return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     INUM,IVAL(22),DSN,NUMI,INDXI(2),NUMR,INDXR(1),I2,
     $            SDATE(6),EDATE(6),DATES(6,2),I0,I6,LRET,WDMSFL,I3,
     $            TSTEP,TUNIT,ATRIBI(2),ILEN,I80,I1,CNUM,CVAL(5),I4,
     $            STATUS,TRANSF,OPLEN,OPVAL(2),SRCDSN,LWDMFL
      REAL        ATRIBR(1)
      CHARACTER*1 CSTR1(80),CTXT(4)
      CHARACTER*4 WDIDS,WDIDT
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR,CHRINT
C
C     + + + EXTERNALS + + +
      EXTERNAL    QGETI,WDAINF,ZIPI,COPYI,QSETI,ZCBGST,LENSTR,CHRINT
      EXTERNAL    ZRFRSH,QGETCO,QSETCO,TIMADD,QGETOP,WCH2UD,CARVAR
      EXTERNAL    QGTCTF,QSTCTF,CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CFLD.LE.5 .OR. CFLD.EQ.29 .OR. CFLD.EQ.30) THEN
C       this is the field we need to check, go ahead
        INUM  = 22
        TSTEP = 0
        TUNIT = 0
        I0    = 0
        I1    = 1
        I2    = 2
        I3    = 3
        I4    = 4
        I6    = 6
        I80   = 80
        CALL ZIPI (I6,I0,SDATE)
        CALL ZIPI (I6,I0,EDATE)
C       get integer values
        CALL QGETI (INUM,
     O              IVAL)
        DSN    = IVAL(1)
        SRCDSN = IVAL(22)
C       get character values
        CNUM = 5
        CALL QGETCO (CNUM,
     O               CVAL)
        TUNIT  = CVAL(2)
        STATUS = CVAL(4)
        TRANSF = CVAL(5)
        CALL QGTCTF (I1,I4,CTXT)
        CALL CARVAR (I4,CTXT,I4,WDIDT)
        CALL QGTCTF (I3,I4,CTXT)
        CALL CARVAR (I4,CTXT,I4,WDIDS)
C       get option values
        OPLEN  = 2
        CALL QGETOP (OPLEN,
     O               OPVAL)
        IF (WDIDT.NE.'none' .AND. WDIDS.NE.'none') THEN
C         find default values for dates, tstep
          NUMI    = 2
          INDXI(1)= 33
          INDXI(2)= 17
          NUMR    = 1
          INDXR(1)= 32
C         get wdm file unit number from common
          CALL ZCBGST (CSTR1)
          ILEN   = LENSTR(I80,CSTR1)
          WDMSFL = CHRINT(ILEN,CSTR1)
          IF (OPVAL(1).NE.2) THEN
C           for compute or manual options set dates based on tardsn
            CALL WCH2UD (WDIDT,
     O                   LWDMFL)
            CALL WDAINF (LWDMFL,DSN,NUMI,NUMR,INDXI,INDXR,
     O                   DATES,ATRIBI,ATRIBR,LRET)
          ELSE
C           for transformation options set dates based on srcdsn
            CALL WCH2UD (WDIDS,
     O                   LWDMFL)
            CALL WDAINF (LWDMFL,SRCDSN,NUMI,NUMR,INDXI,INDXR,
     O                   DATES,ATRIBI,ATRIBR,LRET)
          END IF
          IF (LRET.EQ.0 .OR. LRET.EQ.-107) THEN
C           data available, some attributes may be missing
            IF (ATRIBI(1).NE.-999) THEN
C             tstep attribute exists
              TSTEP = ATRIBI(1)
            END IF
            IF (ATRIBI(2).NE.-999) THEN
C             tunit attribute exists
              TUNIT = ATRIBI(2)
            END IF
            IF (OPVAL(1).EQ.3) THEN
C             doing manual manipulation, start should be end of avail
              CALL COPYI (I6,DATES(1,2),SDATE(1))
              IF (DATES(1,2).EQ.0) THEN
C               no starting date, make ending date zero too
                CALL COPYI (I6,DATES(1,2),EDATE(1))
              ELSE
C               starting date exists, calculate ending date
                CALL TIMADD (DATES(1,2),TUNIT,TSTEP,I1,EDATE)
              END IF
            ELSE
C             doing transformation or compute, show complete range
              CALL COPYI (I6,DATES(1,1),SDATE(1))
              CALL COPYI (I6,DATES(1,2),EDATE(1))
            END IF
            IF (OPVAL(1).NE.2) THEN
C             looking at target data set
              IF (STATUS.EQ.1) THEN
C               must change status, its not new
                STATUS = 2
              END IF
            END IF
          ELSE IF (LRET.EQ.-81) THEN
C           no data set, status must be new
            IF (OPVAL(1).NE.2) THEN
C             looking at target data set
              STATUS = 1
            END IF
          END IF
          IF (OPVAL(1).EQ.2) THEN
C           still need to look at target data set
            CALL WCH2UD (WDIDT,
     O                   LWDMFL)
            CALL WDAINF (LWDMFL,DSN,NUMI,NUMR,INDXI,INDXR,
     O                   DATES,ATRIBI,ATRIBR,LRET)
            IF (LRET.EQ.0 .OR. LRET.EQ.-107) THEN
C             looking at target data set
              IF (STATUS.EQ.1) THEN
C               must change status, its not new
                STATUS = 2
              END IF
            ELSE IF (LRET.EQ.-81) THEN
C             no data set, status must be new
C             looking at target data set
              STATUS = 1
            END IF
          END IF
          INUM = 22
          IVAL(1) = DSN
          CALL COPYI (I6,SDATE(1),IVAL(2))
          CALL COPYI (I6,SDATE(1),IVAL(8))
          CALL COPYI (I6,EDATE(1),IVAL(14))
          IVAL(20) = 0
          IVAL(21) = TSTEP
          IVAL(22) = SRCDSN
          CALL QSETI (INUM,IVAL)
C         set character values
          CVAL(1) = 0
          CVAL(2) = TUNIT
          CVAL(3) = 0
          CVAL(4) = STATUS
          CVAL(5) = TRANSF
          CNUM = 5
          CALL QSETCO (CNUM,CVAL)
          CALL CVARAR (I4,WDIDT,I4,CTXT(1))
          CALL QSTCTF (I1,I4,CTXT)
          CALL CVARAR (I4,WDIDS,I4,CTXT(1))
          CALL QSTCTF (I3,I4,CTXT)
          CALL ZRFRSH (I1)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZCBCH1
     I                   (CFLD,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     Call back routine to set function based on math option selected.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CFLD,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CFLD   - field number to call back
C     RETCOD - call back return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I1,OPLEN,OPVAL(1),CLEN,SGRP,
     $            MESSFL,SCLU,I10,I64
      CHARACTER*1 BLNK,CTXT(64),CSTR1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER     CHRINT
C
C     + + + EXTERNALS + + +
      EXTERNAL    QGETOP,ZIPC,GETTXT,QSTCTF,ZRFRSH,CHRINT,ZCBGST
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CFLD.LE.24) THEN
C       this is the field we need to check, go ahead
        I1    = 1
        I10   = 10
        I64   = 64
        OPLEN = 1
        CALL QGETOP (OPLEN,
     O               OPVAL)
        IF (OPVAL(1).GE.1) THEN
C         an option selected, so ahead
C         set initial character values
          BLNK = ' '
          CALL ZIPC (I64,BLNK,CTXT)
C         get message file unit number, cluster number from common
          CALL ZCBGST (CSTR1)
          MESSFL = CHRINT(I10,CSTR1(1))
          SCLU   = CHRINT(I10,CSTR1(11))
C         get function from message file, put on screen
          SGRP = 51 + OPVAL(1)
          CLEN = 64
          CALL GETTXT (MESSFL,SCLU,SGRP,
     M                 CLEN,
     O                 CTXT(1))
          CALL QSTCTF (I1,I64,CTXT)
          CALL ZRFRSH (I1)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GNATTR
     I                   (MESSFL,SCLU,WDMSFL,DSN)
C
C     + + + PURPOSE + + +
C     allow user to edit basic attributes on newly created data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMSFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU   - screen cluster number
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - target data set number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SAIND,SALEN,RETCOD,ISTAID,TCODE,TSSTEP,SGRP,INUM,
     1              IVAL(4),CNUM,CLEN(7),TLEN,RESP,I0,J,LDSN
      CHARACTER*1   STAID(16),TSTYPE(4),STANAM(48),IDSCEN(8),IDCONS(8)
      CHARACTER*1   BLNK,IDLOCN(8),CTXT(120)
      CHARACTER*4   WDID
C
C     + + + EXTERNALS + + +
      EXTERNAL      WDBSGC,WDBSGI,ZIPC,Q1INIT,QSETI,QGETI,Q1EDIT
      EXTERNAL      CHRCHR,QSETCT,QGETCT,WDBSAC,WDBSAI,WID2UA,CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      BLNK = ' '
C     get attributes for this data set
C     get attribute for integer station id
      SAIND = 51
      SALEN = 1
      CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O             ISTAID,RETCOD)
      IF (RETCOD.LT.0) THEN
C       this attribute does not exist
        ISTAID = -999
      END IF
C     get attribute for tcode
      SAIND = 17
      SALEN = 1
      CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O             TCODE,RETCOD)
      IF (RETCOD.LT.0) THEN
C       this attribute does not exist
        TCODE = -999
      END IF
C     get attribute for tsstep
      SAIND = 33
      SALEN = 1
      CALL WDBSGI (WDMSFL,DSN,SAIND,SALEN,
     O             TSSTEP,RETCOD)
      IF (RETCOD.LT.0) THEN
C       this attribute does not exist
        TSSTEP = -999
      END IF
C     get attribute for station id
      SAIND = 2
      SALEN = 16
      CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O             STAID,RETCOD)
      IF (RETCOD.LT.0) THEN
C       this attribute does not exist
        CALL ZIPC(SALEN,BLNK,STAID)
      END IF
C     get attribute for tstype
      SAIND = 1
      SALEN = 4
      CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O             TSTYPE,RETCOD)
      IF (RETCOD.LT.0) THEN
C       this attribute does not exist
        CALL ZIPC(SALEN,BLNK,TSTYPE)
      END IF
C     get attribute for station name
      SAIND = 45
      SALEN = 48
      CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O             STANAM,RETCOD)
      IF (RETCOD.LT.0) THEN
C       this attribute does not exist
        CALL ZIPC(SALEN,BLNK,STANAM)
      END IF
C     get attribute for scenario name
      SAIND = 288
      SALEN = 8
      CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O             IDSCEN,RETCOD)
      IF (RETCOD.LT.0) THEN
C       this attribute does not exist
        CALL ZIPC(SALEN,BLNK,IDSCEN)
      END IF
C     get attribute for constituent name
      SAIND = 289
      SALEN = 8
      CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O             IDCONS,RETCOD)
      IF (RETCOD.LT.0) THEN
C       this attribute does not exist
        CALL ZIPC(SALEN,BLNK,IDCONS)
      END IF
C     get attribute for location name
      SAIND = 290
      SALEN = 8
      CALL WDBSGC (WDMSFL,DSN,SAIND,SALEN,
     O             IDLOCN,RETCOD)
      IF (RETCOD.LT.0) THEN
C       this attribute does not exist
        CALL ZIPC(SALEN,BLNK,IDLOCN)
      END IF
C
      SGRP = 10
      CALL Q1INIT (MESSFL,SCLU,SGRP)
C     translate dsn as an id to a wdm file and data set number
      CALL WID2UA (I0,DSN,
     O             J,LDSN,WDID)
C     set initial integer values
      IVAL(1)= LDSN
      IVAL(2)= ISTAID
      IVAL(3)= TCODE
      IVAL(4)= TSSTEP
      INUM   = 4
      CALL QSETI (INUM,IVAL)
C     set initial character values
      CNUM = 7
      CLEN(1) = 4
      CALL CVARAR (CLEN(1),WDID,CLEN(1),CTXT(1))
      CLEN(2) = 16
      CALL CHRCHR (CLEN(2),STAID,CTXT(5))
      CLEN(3) = 4
      CALL CHRCHR (CLEN(3),TSTYPE,CTXT(21))
      CLEN(4) = 48
      CALL CHRCHR (CLEN(4),STANAM,CTXT(25))
      CLEN(5) = 8
      CALL CHRCHR (CLEN(5),IDSCEN,CTXT(73))
      CLEN(6) = 8
      CALL CHRCHR (CLEN(6),IDLOCN,CTXT(81))
      CLEN(7) = 8
      CALL CHRCHR (CLEN(7),IDCONS,CTXT(89))
      TLEN = 96
      CALL QSETCT (CNUM,CLEN,TLEN,CTXT)
C     now edit screen
      CALL Q1EDIT (
     O             RESP)
      IF (RESP.EQ.1) THEN
C       user wants to continue
C       get integer values
        CALL QGETI (INUM,
     O              IVAL)
        LDSN   = IVAL(1)
        ISTAID = IVAL(2)
        TCODE  = IVAL(3)
        TSSTEP = IVAL(4)
C       get character values
        CALL QGETCT (CNUM,CLEN,TLEN,
     O               CTXT)
        CALL CHRCHR (CLEN(2),CTXT(5),STAID)
        CALL CHRCHR (CLEN(3),CTXT(21),TSTYPE)
        CALL CHRCHR (CLEN(4),CTXT(25),STANAM)
        CALL CHRCHR (CLEN(5),CTXT(73),IDSCEN)
        CALL CHRCHR (CLEN(6),CTXT(81),IDLOCN)
        CALL CHRCHR (CLEN(7),CTXT(89),IDCONS)
C       put edited values back to wdm data set
C       put attribute for integer station id
        SAIND = 51
        SALEN = 1
        CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,SALEN,ISTAID,
     O               RETCOD)
C       put attribute for tcode
        SAIND = 17
        SALEN = 1
        CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,SALEN,TCODE,
     O               RETCOD)
C       put attribute for tsstep
        SAIND = 33
        SALEN = 1
        CALL WDBSAI (WDMSFL,DSN,MESSFL,SAIND,SALEN,TSSTEP,
     O               RETCOD)
C       put attribute for station id
        SAIND = 2
        SALEN = 16
        CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,STAID,
     O               RETCOD)
C       put attribute for tstype
        SAIND = 1
        SALEN = 4
        CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,TSTYPE,
     O               RETCOD)
C       put attribute for station name
        SAIND = 45
        SALEN = 48
        CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,STANAM,
     O               RETCOD)
C       put attribute for scenario name
        SAIND = 288
        SALEN = 8
        CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,IDSCEN,
     O               RETCOD)
C       put attribute for constituent name
        SAIND = 289
        SALEN = 8
        CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,IDCONS,
     O               RETCOD)
C       put attribute for location name
        SAIND = 290
        SALEN = 8
        CALL WDBSAC (WDMSFL,DSN,MESSFL,SAIND,SALEN,IDLOCN,
     O               RETCOD)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OGENER
     I                   ( MESSFL, WDMSFL, MXDSN, PTHNAM,
     O                     DSNCNT, DSNBUF )
C
C     + + + PURPOSE + + +
C     This routine and the routines it calls are used to perform time
C     shift and math transformations on time series data.
C
C     + + + KEYWORDS + + +
C     WDM file, message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL, WDMSFL, MXDSN, DSNCNT, DSNBUF(MXDSN)
      CHARACTER*8  PTHNAM(1)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     WDMSFL - Fortran unit number of WDM file
C     MXNDSN - max number of data sets that can be added in one gener session
C     DSNCNT - count of data sets added in this gener session
C     DSNBUF - buffer of data set numbers added
C     PTHNAM - name of path chosen to get here
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU, SGRP, OPTION, I1, I0
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPI
      EXTERNAL  QRESP,  ZWNSOP
      EXTERNAL  GNMATO, GNTIMO
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   SCLU, I0, I1
     $      /  42,  0,  1 /
C
C     + + + END SPECIFICATIONS + + +
C
C     let user know how we got here
      CALL ZWNSOP ( I1, PTHNAM )
C
C     initialize count and buffer for new data sets
      DSNCNT = 0
      CALL ZIPI ( MXDSN, I0, DSNBUF )
C
 100  CONTINUE
C       generate option: 1-return, 2-compute math, 3-transform,
        SGRP = 101
        CALL QRESP ( MESSFL, SCLU, SGRP, OPTION )
        IF (OPTION .EQ. 2) THEN
C         compute math functions
          CALL GNMATO ( MESSFL, SCLU, WDMSFL )
        ELSE IF (OPTION .EQ. 3) THEN
C         transform time step and/or shift in time
          CALL GNTIMO ( MESSFL, SCLU, WDMSFL )
        END IF
      IF (OPTION .NE. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   GNMATO
     I                   ( MESSFL, SCLU, WDMSFL )
C
C     + + + PURPOSE + + +
C     Create a new time series from an existing time series by
C     transforming the time series by mathematical functions.
C     The user is asked to specify input and output data sets, the
C     time period to be transformed, and the type of transformation.
C
C     + + + KEYWORDS + + +
C     WDM file, message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - screen cluster number
C     WDMSFL - Fortran unit number of WDM file
C
C     + + + PARAMETERS + + +
      INTEGER    KSIZ
      PARAMETER (KSIZ = 100)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I, J, SGRP, RESP, IRET, FCTN, CFCT, FCTNFG,
     $            DATAFG, PERFG, NVAL, NIN, DSN(3), SDATE(6), EDATE(6),
     $            TSTEP(3), TUNIT(3), TBLSIZ, K1, RETCOD,
     $            INIT, IWRT, IVAL(1), ERRFLG, KOUNT
      REAL        C1(KSIZ),C2(KSIZ), SUM, RVAL(1)
      CHARACTER*1  TBUFF(80)
      CHARACTER*29 WNDNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL    DATES
      EXTERNAL    QRESP,  ZSTCMA, ZGTRET
      EXTERNAL    PRNTXT, PMXTXI, PMXTXR, PRTERR, PMXCNW
      EXTERNAL    GNMDSC, GNMEXE
C
C     + + + END SPECIFICATIONS + + +
C
      FCTN  = 0
      FCTNFG= 0
      DATAFG= 0
      PERFG = 0
      RESP  = 2
C
 100  CONTINUE
C       Compute optns: 1-Return,2-Function,3-Datasets,4-Period,5-Execute
        SGRP= 108
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
C       allow Previous command
        I= 4
        J= 1
        CALL ZSTCMA (I,J)
C
        IF (RESP.EQ.2) THEN
C         select math function
          CFCT= FCTN- 1
          SGRP= 109
          CALL QRESP (MESSFL,SCLU,SGRP,CFCT)
C         get user exit command value
          CALL ZGTRET (IRET)
          IF (IRET.EQ.1) THEN
C           user Accepted, save function selected
            FCTNFG= 1
            CFCT= CFCT+ 1
            IF (FCTN.NE.CFCT) THEN
C             new function selected, datasets and period need defining
              DATAFG= 0
              PERFG = 0
            END IF
            FCTN= CFCT
C           set default option to define dataset next
            RESP= 3
          END IF
        ELSE IF (RESP.EQ.3) THEN
C         define datasets and constants needed for computation
          IF (FCTNFG.EQ.1) THEN
C           function defined, ok to specify data sets
            CALL GNMDSC (MESSFL,SCLU,WDMSFL,FCTN,KSIZ,
     O                   DSN,NIN,TSTEP,TUNIT,SDATE,EDATE,
     O                   TBLSIZ,C1,C2,K1,RETCOD)
            IF (RETCOD.EQ.0) THEN
C             data sets defined
              DATAFG= 1
C             need to define period for data sets
              PERFG = 0
C             set default option to define period of record next
              RESP= 4
            END IF
          ELSE
C           math function not selected yet, can't define data sets
            SGRP= 41
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            RESP = 2
          END IF
        ELSE IF (RESP.EQ.4) THEN
C         define period of record
          IF (FCTNFG.EQ.1 .AND. DATAFG.EQ.1) THEN
C           function and data defined, ok to specify period
            I= 3
            J= 6
            CALL DATES (TSTEP,I,J,
     M                  SDATE,EDATE,
     O                  NVAL,TBUFF)
            PERFG= 1
C           set default option to execute computation
            RESP= 5
          ELSE
C           function and/or datasets not defined, can't specify period yet
            SGRP= 42
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            RESP = 2
          END IF
        ELSE IF (RESP.EQ.5) THEN
C         execute computation
          IF (FCTNFG.EQ.1 .AND. DATAFG.EQ.1 .AND. PERFG.EQ.1) THEN
C           everything defined properly, perform computation
            CALL GNMEXE (WDMSFL,DSN,FCTN,NIN,
     I                   TSTEP,TUNIT,SDATE,EDATE,
     I                   TBLSIZ,C1,C2,K1,
     O                   ERRFLG,SUM,KOUNT)
            IF (ERRFLG.NE.0) THEN
C             problem with generate
              WNDNAM= 'Execute (DTGCE) Problem'
              CALL PRTERR (MESSFL,WNDNAM,ERRFLG)
            ELSE
C             generate performed successfully
              INIT= 1
              IWRT= -1
              I   = 1
              IF (FCTN .EQ. 21) THEN
C               output sum of timeseries
                SGRP = 85
                RVAL(1)= SUM
                CALL PMXTXR (MESSFL,SCLU,SGRP,I,INIT,IWRT,I,RVAL)
                INIT = -1
              END IF
              IF (KOUNT .GT. 0) THEN
C               some values out of range
                SGRP = 86
                IVAL(1)= KOUNT
                CALL PMXTXI (MESSFL,SCLU,SGRP,I,INIT,IWRT,I,IVAL)
                INIT = -1
              END IF
C             generate complete
              SGRP = 87
              IWRT = 0
              CALL PMXCNW (MESSFL,SCLU,SGRP,I,INIT,IWRT,J)
            END IF
          ELSE
C           everything not defined, let 'em know
            SGRP= 43
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            RESP = 2
          END IF
        END IF
C
C       turn off Previous command
        I= 4
        J= 0
        CALL ZSTCMA (I,J)
C
      IF (RESP.NE.1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   GNMDSC
     I                   ( MESSFL, SCLU, WDMSFL, OPT, KSIZ,
     O                     DSN, NIN, TSSTEP, TUNIT, STRT, STPP,
     O                     TBLSIZ, C1, C2, K1, RETC )
C
C     + + + PURPOSE + + +
C     Gets the data-set numbers and constants required for
C     math compuations in Timeseries Generate.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMSFL, OPT, KSIZ, DSN(3), NIN,
     $          TSSTEP(3), TUNIT(3), STRT(6), STPP(6), TBLSIZ, K1, RETC
      REAL      C1(KSIZ), C2(KSIZ)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number of WDM file
C     OPT    - algebra option
C               1 - DONE    2 - +C      3 - *C
C               4 - ADD     5 - SUB     6 - MULT
C               7 - DIV     8 - MEAN    9 - WGHT
C              10 - **C    11 - C**    12 - POW
C              13 - LGE    14 - LG10   15 - LGE
C              16 - ABS    17 - Z-     18 - Z+
C              19 - MIN    20 - MAX    21 - SUM
C              22 - SIGF   23 - LINE   24 - TABLE
C     KSIZ   - array size for table of coefficents
C     DSN    - data-set numbers
C              (1) - output data set
C              (2) - input data set, T1
C              (3) - optional second input data set, T2
C     NIN    - count of input data sets
C     TSSTEP - time step of the data sets
C     TUNIT  - time units of the data sets
C     STRT   - requested start date and time
C     STPP   - requested stop date and time
C     TBLSIZ - array size of real coefficients
C     C1     - real coefficient, required for
C              OPT = 2, 3, 9, 11, 17, 18 and 23,
C              optional for OPT = 10
C     C2     - real coefficient, required for
C              OPT = 17, 18, and 23
C     K1     - integer coefficient, required for
C              OPT = 21, optional for OPT = 10
C     RETC   - return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP, INUM, RNUM, CNUM, INM(24), RNM(24), FLTABL,
     $            QNM(24), NDSN(24), CVAL(3), IN, FLAG, I, DREC,
     $            START(6,3), STOP(6,3), GPFLG, LINES, FLAG0, RETCOD,
     $            IVAL(4), LEN, I1, I4, I10, IRET
      REAL        CONST(2)
      CHARACTER*1 TBUFF(80), DEC
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR, STRFND, CHRINT
      REAL        CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, STRFND, CHRINT, CHRDEC, PMXCNW
      EXTERNAL    DLIMIT, WDDSCL, WDDSCK, PMXTXI, ZMNSST, QRESPM
      EXTERNAL    QFOPEN, PRNTXT, WDBSGI, WTFNDT, ZGTRET, QFCLOS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   I1, I4, I10, GPFLG, FLAG0, LINES, DEC
     $     /  1,  4,  10,     1,     0,     4, '.' /
      DATA  INM / 0,2,2,3,3,3,3,3,3,2,2,3,2,2,2,2,2,2,3,3,2,3,2,2 /,
     $      RNM / 0,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,2,2,1,1,1,1,2,1 /,
     $      QNM / 0,2,2,1,1,1,1,1,6,4,2,1,0,0,0,0,5,5,1,1,0,3,5,0 /,
     $     NDSN / 0,2,2,3,3,3,3,3,3,2,2,3,2,2,2,2,2,2,3,3,2,2,2,2 /
C
C     + + + END SPECIFICATIONS + + +
C
      CONST(1) = -999.
      CONST(2) = -999.
      DSN(1) = -999
      DSN(2) = -999
      DSN(3) = -999
      NIN  = NDSN(OPT) - 1
      INUM = INM(OPT)
      RNUM = RNM(OPT)
      CNUM = 1
C     you are using the following equation:
      SGRP = 51
      CALL PMXCNW ( MESSFL, SCLU, SGRP, I1, I1, -I1, I )
      SGRP = SGRP + OPT
      CALL PMXCNW ( MESSFL, SCLU, SGRP, I1, -I1, -I1, I )
C     save text for data entry screen
      CALL ZMNSST
C
C     enter data-set numbers and coefficients
      SGRP = 110 + QNM(OPT)
      CALL QRESPM ( MESSFL, SCLU, SGRP, INUM, RNUM, CNUM,
     M              DSN, CONST, CVAL, TBUFF )
C     get user exit command value
      CALL ZGTRET (IRET)
      IF (IRET.EQ.1) THEN
C       get info on selected data sets
        IN = NIN + 1
        FLAG = 0
        DO 120 I = 1, IN
C         check if dsn exists
          CALL WDDSCK ( WDMSFL, DSN(I),
     O                  DREC, RETC )
          IF (RETC .LT. 0  .AND.  I .EQ. 1) THEN
C           output data set does not exist, copy input attrib
            CALL WDDSCL ( WDMSFL, DSN(2), WDMSFL, DSN(1), FLAG0, RETC )
            IF (RETC .NE. 0) THEN
C             attributes not successfully copied
              FLAG = FLAG + 1
            ELSE
C             create and copy ok, let user know
              IVAL(1)= DSN(1)
              IVAL(2)= DSN(2)
              SGRP= 90
              INUM= 2
              CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,FLAG0,INUM,IVAL)
            END IF
          ELSE IF (RETC .LT. 0) THEN
C           input data set does not exist
            FLAG = FLAG + 1
            SGRP = 83
            CALL PMXTXI ( MESSFL, SCLU, SGRP, LINES, I1, FLAG0,
     $                    I1, DSN(I) )
          END IF
          IF (RETC .EQ. 0) THEN
C           data set exists, get details
            CALL WDBSGI ( WDMSFL, DSN(I), 33, I1, TSSTEP(I), RETC )
            CALL WDBSGI ( WDMSFL, DSN(I), 17, I1, TUNIT(I), RETC )
            CALL WTFNDT ( WDMSFL, DSN(I), GPFLG, DREC,
     O                    START(1,I), STOP(1,I), RETC )
          END IF
 120    CONTINUE
      ELSE
C       user selected previous, back to main Compute menu
        FLAG= -1
      END IF
C
      IF (FLAG .EQ. 0) THEN
C       check for compatible time steps
        DO 140 I = 2, NDSN(OPT)
          IF (TUNIT(1).NE.TUNIT(I) .OR. TSSTEP(1).NE.TSSTEP(I)) THEN
C           time units/steps not the same
            FLAG = FLAG + 1
            SGRP = 84
            IVAL(1) = TUNIT(1)
            IVAL(2) = TSSTEP(1)
            IVAL(3) = TUNIT(I)
            IVAL(4) = TSSTEP(I)
            CALL PMXTXI ( MESSFL, SCLU, SGRP, LINES, I1, FLAG0,
     $                    I4, IVAL )
          END IF
 140    CONTINUE
C
C       find latest start and earliest end dates
        I = 2
        CALL DLIMIT ( START(1,2), NIN, I, STRT )
        I = 1
        CALL DLIMIT ( STOP(1,2), NIN, I, STPP )
      END IF
C
      IF (FLAG .EQ. 0) THEN
C       set coefficients
        K1 = DSN(3)
        C1(1) = CONST(1)
        C2(1) = CONST(2)
        IF (OPT .EQ. 2) C2(1) = C1(1)
        IF (OPT .EQ. 10) THEN
C         time series raised to a power, check for decimal
          LEN = LENSTR ( I10, TBUFF(18) )
          I = STRFND ( LEN, TBUFF(18), I1, DEC )
          IF (I .EQ. 0) THEN
C           an integer constant
            K1 = CHRINT ( LEN, TBUFF(18) )
          ELSE
C           a real constant
            C1(1) = CHRDEC ( LEN, TBUFF(18) )
            K1 = 0
          END IF
        END IF
      END IF
C
      IF (FLAG .EQ. 0  .AND.  OPT .EQ. 24) THEN
C       File for table?
        SGRP = 117
        CALL QFOPEN (MESSFL, SCLU, SGRP, FLTABL, RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         file opened ok, read values
          I = 0
 162      CONTINUE
            I = I + 1
            READ (FLTABL,*,END=165,ERR=165) C1(I), C2(I)
            IF (I.GT.1) THEN
C             C1 must be increasing function
              IF (C1(I) .LE. C1(I-1)) THEN
C               problem with C1 values
                FLAG= 1
              END IF
            END IF
          IF (I .LT. KSIZ) GO TO 162
          I = I + 1
 165      CONTINUE
          TBLSIZ = I - 1
          IF (TBLSIZ .LE. 1) THEN
C           not enough values read
            FLAG= 1
          END IF
        ELSE
C         could not open table file
          FLAG= 1
        END IF
        IF (FLAG.NE.0) THEN
C         had problem with table file
          SGRP= 89
          CALL PRNTXT (MESSFL,SCLU,SGRP)
        END IF
C
C       ***** add echo of table here ****
C
C       close table file
        I= 0
        CALL QFCLOS (FLTABL,I)
      ELSE IF (FLAG.EQ.0) THEN
C       table size is 1 for all other options
        TBLSIZ= 1
      END IF
C
      IF (FLAG .EQ. 0) THEN
        RETC = 0
      ELSE
        RETC = -1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GNTIMO
     I                   ( MESSFL, SCLU, WDMSFL )
C
C     + + + PURPOSE + + +
C     Create a new time series from an existing time series by shifting
C     the time series in time and/or aggregating or disaggregating the
C     time series over time.  The user is asked to specify the time 
C     period to be transformed, the shifted start date, time step and
C     units for the new time series, the number of the output data set,
C     and the current status of the output data set.
C
C     + + + KEYWORDS + + +
C     WDM file, message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - screen cluster number on message file
C     WDMSFL - Fortran unit number of WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      FLGS, FLGD, SGRP, OPTN, RETC, DTRAN, RETCOD,
     $             DSN(2), DATES(6,3), TSTEP, TUNIT, TFORM, NAORO,
     $             INIT, IWRT, L, MAXL, IVAL(3), CMPFLG, CRTFLG
      REAL         MISS
      CHARACTER*23 WNDNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL  QRESP,  PMXCNW, PRTERR, PMXTXI, ZMNSST
      EXTERNAL  GNTSEL, GNTDEF, GNTTRN
C
C     + + + END SPECIFICATIONS + + +
C
      MAXL = 10
      FLGS = 0
      FLGD = 0
      OPTN = 2
 100  CONTINUE
C       transformation option: 1-Return,2-Select,3-Define,4-Transform
        INIT = 1
        IWRT = -1
        SGRP = 102
        CALL QRESP ( MESSFL, SCLU, SGRP, OPTN )
        IF (OPTN .EQ. 2) THEN
C         Select the input data set
          CALL GNTSEL ( MESSFL, SCLU, WDMSFL,
     O                  DSN, DATES, TSTEP, TUNIT, TFORM, MISS, NAORO,
     O                  RETC )
          IF (RETC .EQ. 0) THEN
C           input data set selected
            FLGS = 1
            FLGD = 0
          ELSE
C           input data set not selected
            FLGS = 0
            FLGD = 0
          END IF
          OPTN = 3
        ELSE IF (OPTN .EQ. 3) THEN
C         Define the transformation and output data set
          IF (FLGS .EQ. 1) THEN
C           input data set has been defined, define output
            CALL GNTDEF ( MESSFL, SCLU, DSN,
     M                    NAORO, DATES, TSTEP, TUNIT,
     O                    DTRAN, RETC )
            IF (RETC .EQ. 0) THEN 
C             output transformation and data set defined
              FLGD = 1
              OPTN = 4
            ELSE
C             problem defining output
              FLGD = 0
              OPTN = 2
            END IF
          ELSE
C           input data set not selected, can't define output
            SGRP = 11
            CALL PMXCNW ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L )
            CALL ZMNSST
            INIT = 0
            OPTN = 2
          END IF
          OPTN = 4
        ELSE IF (OPTN .EQ. 4) THEN
C         Transform the time series
          IF (FLGS .EQ. 1  .AND.  FLGD .EQ. 1) THEN
C           input and output defined, ok to transform
            CALL GNTTRN ( MESSFL, WDMSFL, DSN, NAORO,
     I                    DATES, DTRAN, TSTEP, TUNIT,
     O                    CRTFLG, CMPFLG, RETCOD )
C           display messages upon return from execution
            IWRT = 1
            INIT = 1
            IF (CRTFLG .EQ. 1) THEN
C             successfully created new data set
              SGRP   = 15
              L      = 2
              IVAL(1)= DSN(1)
              IVAL(2)= DSN(2)
              CALL PMXTXI (MESSFL,SCLU,SGRP,MAXL,INIT,IWRT,L,IVAL)
              INIT = -1
            ELSE IF (CRTFLG .EQ. 2) THEN
C             problem creating new data set
              SGRP   = 16
              L      = 3
              IVAL(1)= DSN(1)
              IVAL(2)= DSN(2)
              IVAL(3)= RETCOD
              CALL PMXTXI (MESSFL,SCLU,SGRP,MAXL,INIT,IWRT,L,IVAL)
              INIT = -1
            ELSE IF (CRTFLG .EQ. 3) THEN
C             problem with tgroup for existing data set
              SGRP   = 17
              L      = 1
              IVAL(1)= DSN(2)
              CALL PMXTXI (MESSFL,SCLU,SGRP,MAXL,INIT,IWRT,L,IVAL)
              INIT = -1
            END IF
            INIT = 0
            IWRT = 0
            IF (CMPFLG .EQ. 1) THEN
C             all data successfully transformed
              SGRP   = 14
            ELSE IF (CMPFLG .EQ. 2) THEN
C             at least some data not transformed, error in get/put
              WNDNAM = 'Execute (DTGTE) Problem'
              CALL PRTERR (MESSFL,WNDNAM,RETCOD)
              SGRP   = 18
            ELSE IF (CMPFLG .EQ. 3) THEN
C             none of the data transformed
              SGRP   = 19
            END IF
            CALL PMXCNW (MESSFL,SCLU,SGRP,MAXL,INIT,IWRT,L)
            FLGS = 0
            FLGD = 0
            OPTN = 2
          ELSE
C           not ready to generate
            IF (FLGS .EQ. 0  .AND.  FLGD .EQ. 0) THEN
C             Select input and Define output
              SGRP = 12
              OPTN = 2
            ELSE
C             Define the output
              SGRP = 13
              OPTN = 3
            END IF
C           informational message
            CALL PMXCNW ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L )
            CALL ZMNSST
            INIT = 0
          END IF
        END IF
      IF (OPTN .NE. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   GNTSEL
     I                   ( MESSFL, SCLU, WDMSFL,
     O                     DSN, DATES, TSTEP, TUNIT, TFORM, MISS,
     O                     NAORO, RETC )
C
C     + + + PURPOSE + + +
C     Gets an input time-series data set and information about that
C     data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMSFL, DSN(2), DATES(6,3), TSTEP, TUNIT,
     $          TFORM, NAORO, RETC
      REAL      MISS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - screen cluster number on message file
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data set numbers for the time series
C              (1) - input time-series data-set number
C              (2) - output time-series data-set number
C     DATES  - dates for transformation, defaults to input dsn dates
C              (_,1) start date for time series to be transformed
C              (_,2) end date for time series to be transformed
C              (_,3) start date for transformed time series
C     TSTEP  - time step of transformed time series
C     TUNIT  - time units of transformed time series
C     TFORM  - form of data
C              1 - mean over the time step
C              2 - total over the time step
C              3 - instantaneous at time (end of time step)
C              4 - minimum over the time step
C              5 - maximum over the time step
C     MISS   - missing value filler
C     NAORO  - output data set status flag
C              1 - new data set
C              2 - existing data set, append data
C              3 - existing data set, overwrite data
C     RETC   - return code
C               0 - data sets successfully identified
C              -1 - data sets were not selected
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NUMI, NUMR, INDXI(3), INDXR(1), ATRIBI(3), DATET(6,2),
     $          SGRP, ILEN, RET, INIT, MAXL, IWRT, L, CVAL(1),
     $          IRET, AGAIN, RESP, FLGTS, FLGTU, I, J
      REAL      ATRIBR(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDAINF, COPYI, ZMNSST, CMPTIM, ZSTCMA
      EXTERNAL  Q1INIT, QSETI, QSETCO, QRESP, PMXCNW
      EXTERNAL  Q1EDIT, QGETI, QGETCO
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  NUMI, NUMR, INIT, MAXL, IWRT
     $    /    3,    1,    1,  10,   -1 /
C                         tsstep tcode tsform   tsfill
      DATA  INDXI, INDXR /   33,   17,    84,     32 /
C
C     + + + END SPECIFICATIONS + + +
C
C     allow previous command
      I= 4
      J= 1
      CALL ZSTCMA (I,J)
C
      NAORO = 1
      DSN(1)= 0
      DSN(2)= 0
 100  CONTINUE
C       get the input and output data set number
        SGRP= 103
        CALL Q1INIT ( MESSFL, SCLU, SGRP )
        ILEN = 2
        CALL QSETI ( ILEN, DSN )
        ILEN = 1
C       set output status: 1-new,2-add,3-overwrite
        CVAL(1)= NAORO
        CALL QSETCO ( ILEN, CVAL )
        CALL Q1EDIT ( IRET )
        IF (IRET .EQ. 1) THEN
C         user accepted, continue on
          AGAIN = 0
C         get input/output data set number
          ILEN = 2
          CALL QGETI ( ILEN, DSN )
          ILEN = 1
          CALL QGETCO ( ILEN, CVAL )
          NAORO= CVAL(1)
        ELSE IF (IRET .EQ. 2) THEN
C         previous, user wants back to main Transform menu
          AGAIN= -1
        ELSE IF (IRET .EQ. -1) THEN
C         oops, try again
          AGAIN = 1
        END IF
        IF (AGAIN .EQ. 0) THEN
C         get period of record and attributes for input data set
          CALL WDAINF ( WDMSFL, DSN(1), NUMI, NUMR, INDXI, INDXR,
     O                  DATES, ATRIBI, ATRIBR, RET )
          IF (RET .EQ. 0  .OR.  RET .EQ. -107) THEN
C           data available, some atributes may be missing, go for it
            TSTEP = ATRIBI(1)
            TUNIT = ATRIBI(2)
            TFORM = ATRIBI(3)
            MISS  = ATRIBR(1)
          ELSE
C           problem with data set
            IF (RET .EQ. -6) THEN
C             no data present in data set
              SGRP = 21
            ELSE IF (RET .EQ. -81) THEN
C             data set does not exist
              SGRP = 22
            ELSE IF (RET .EQ. -82) THEN
C             data set exists, but wrong data type
              SGRP = 23
            ELSE
C             unknown problem
              SGRP = 24
            END IF
            CALL PMXCNW ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L )
            CALL ZMNSST
C           problem, 1-select again, 2-return to transform screen
            SGRP = 104
            RESP = 1
            CALL QRESP ( MESSFL, SCLU, SGRP, RESP )
            IF (RESP .EQ. 1) THEN
C             select again
              AGAIN = 1
            ELSE
C             abandon select option
              AGAIN = -1
            END IF
          END IF
        END IF
C
        IF (AGAIN .EQ. 0) THEN
C         check output data set
          SGRP = 0
          CALL WDAINF ( WDMSFL, DSN(2), NUMI, NUMR, INDXI, INDXR,
     O                  DATET, ATRIBI, ATRIBR, RET )
          IF (NAORO .EQ. 1) THEN
C           expect output data set to not exist
            IF (RET .EQ. -81) THEN
C             doesn't exist, use input attributes and start date
              ILEN = 6
              CALL COPYI ( ILEN, DATES(1,1), DATES(1,3) )
            ELSE IF (RET .EQ. 0  .OR.  RET .EQ. -1  .OR.
     $               RET .EQ. -107) THEN
C             does exist, but shouldn't
              SGRP = 28
            ELSE IF (RET .EQ. -82) THEN
C             does exist but not time series, shouldn't exist
              SGRP = 26
            ELSE
C             unknown problem
              SGRP = 27
            END IF
          ELSE
C           expect output data set to exist
            IF (RET .EQ. 0 .OR. RET .EQ. -6 .OR. RET .EQ. -107) THEN
C             data set exists, are time steps compatible?
              CALL CMPTIM ( TUNIT, TSTEP, ATRIBI(2), ATRIBI(1),
     O                      FLGTS, FLGTU )
              IF (FLGTS .EQ. 1  .OR.  FLGTU .EQ. -1) THEN
C               time steps are not compatible
                SGRP = 25
              ELSE
C               assume ok, add using output dsn time step
                TSTEP = ATRIBI(1)
                TUNIT = ATRIBI(2)
                IF (RET .EQ. -6) THEN
C                 empty dsn, use input start date for output date
                  ILEN = 6
                  CALL COPYI ( ILEN, DATES(1,1), DATES(1,3) )
                ELSE 
C                 use output end date for output date
                  ILEN = 6
                  CALL COPYI ( ILEN, DATET(1,2), DATES(1,3) )
                END IF
              END IF
            ELSE IF (RET .EQ. -81) THEN
C             data set does not exist, but should
              SGRP = 32
            ELSE IF (RET .EQ. -82) THEN
C             dsn exists, but is wrong type, not a time series
              SGRP = 26
            ELSE 
C             unknown problem
              SGRP = 27
            END IF
          END IF
          IF (SGRP .NE. 0) THEN
C           some kind of problem
            CALL PMXCNW ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L )
            CALL ZMNSST
C           problem: 1-select again, 2-return to transform screen
            SGRP = 104
            RESP = 1
            CALL QRESP ( MESSFL, SCLU, SGRP, RESP )
            IF (RESP .EQ. 1) THEN
C             select again
              AGAIN = 1
            ELSE
              AGAIN = -1
            END IF
          END IF
        END IF
      IF (AGAIN .EQ. 1) GO TO 100
      IF (AGAIN .EQ. 0) THEN
C       everything looks ok
        RETC = 0
      ELSE
C       no data sets selected
        DSN(1) = 0
        DSN(2) = 0
        RETC   = -1
      END IF
C
C     turn off previous command
      I= 4
      J= 0
      CALL ZSTCMA (I,J)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GNTDEF
     I                   ( MESSFL, SCLU, DSN, NAORO,
     M                     DATES, TSTEP, TUNIT,
     O                     DTRAN, RETC )
C
C     + + + PURPOSE + + +
C     Define the type of data transformation to be performed and the
C     output data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, DSN(2), NAORO, TSTEP, TUNIT, DATES(6,3),
     $          DTRAN, RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - screen cluster number on message file
C     DSN    - data set numbers for the time series
C              (1) - input time-series data-set number
C              (2) - output time-series data-set number
C     NAORO  - output data set status flag
C              1 - new data set
C              2 - existing data set, append data
C              3 - existing data set, overwrite data
C     TSTEP  - time step of transformed time series
C     TUNIT  - time units of transformed time series
C     DATES  - dates for transformation, defaults to input dsn dates
C              (_,1) start date for time series to be transformed
C              (_,2) end date for time series to be transformed
C              (_,3) start date for transformed time series
C     DTRAN  - type of transformation
C              0 - rate (average or same)
C              1 - total (sum or divide)
C              2 - maximum
C              3 - minimum
C     RETC   - return code
C               0 - transformation successfully defined
C              -1 - problem defining transformation
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, J, AGAIN, SGRP, ILEN, ITMP(21), CVAL(3),
     $          IRET, FLAGTS, FLAGTU, FLAG1, FLAG2, RESP, MAXL,
     $          INIT, IWRT, L, CTMP, DTRVAL(6),
     $          I2, I3, I18, I19, I21
C
C     + + + EXTERNALS + + +
      EXTERNAL   CMPTIM, CKBDRY, ZSTCMA
      EXTERNAL   Q1INIT, QSETI, QSETCO, PMXCNW, ZMNSST, QRESP
      EXTERNAL   Q1EDIT, QGETI, QGETCO, COPYI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  INIT, MAXL, IWRT, I2, I3, I18, I19, I21
     $    /    1,   10,   -1,  2,  3,  18,  19,  21 /
C                  ave, sum, max, min, sam, div
      DATA DTRVAL / 0,   1,   2,   3,   0,   1 /
C
C     + + + END SPECIFICATIONS + + +
C
C     allow previous command
      I= 4
      J= 1
      CALL ZSTCMA (I,J)
C
      DTRAN  = 0
      CVAL(1)= 1
C
 100  CONTINUE
C       initialize screen
        AGAIN = 0
        IF (NAORO .EQ. 1) THEN
C         new output data set includes time step, protect status
          SGRP = 105
          CVAL(2) = TUNIT
          CVAL(3) = NAORO
        ELSE
C         existing output data set includes status, protect time step
          SGRP = 106
          CVAL(2) = NAORO - 1
          CVAL(3) = TUNIT
        END IF
C       set dates (begin, end, shift), time step, i/o DSNs
        CALL COPYI ( I18, DATES, ITMP)
        ITMP(19)= TSTEP
        CALL COPYI ( I2, DSN, ITMP(20) )
        CALL Q1INIT ( MESSFL, SCLU, SGRP )
        CALL QSETI ( I21, ITMP )
        CALL QSETCO ( I3, CVAL )
C       edit the screen
        CALL Q1EDIT ( IRET )
        IF (IRET .EQ. -1) THEN
C         oops, try again
          AGAIN = 1
        ELSE IF (IRET .EQ. 2) THEN
C         previous, user wants back to main Transform menu
          AGAIN = -1
        ELSE
C         accepted, get dates and time step
          CALL QGETI ( I19, ITMP )
C         get transformation, time units, status
          CALL QGETCO ( I3, CVAL )
          IF (NAORO .NE. 1) THEN
C           existing data set, reverse order of status, time units
            CTMP= CVAL(2)
            CVAL(2)= CVAL(3)
            CVAL(3)= CTMP+ 1
          END IF
C
          SGRP = 0
C         is output time step compatible with input data set
          CALL CMPTIM ( TUNIT, TSTEP, CVAL(2), ITMP(19),
     O                  FLAGTS, FLAGTU )
          IF (FLAGTS .EQ. 1  .OR.  FLAGTU .EQ. -1) THEN
C           time steps are not compatible
            SGRP = 35
          ELSE
C           time steps ok, are dates compatible with time step
            FLAG1 = 0
            DO 150 I = 1, 13, 6
              CALL CKBDRY ( ITMP(I), ITMP(19), CVAL(2), FLAG2 )
              IF (FLAG2 .NE. 0) THEN
C               date is not compatible with time step
                FLAG1 = 1
              END IF
 150        CONTINUE
            IF (FLAG1 .EQ. 1) THEN
C             date(s) not compatible with time step
              SGRP = 36
            END IF
          END IF
          IF (SGRP .NE. 0) THEN
C           some kind of problem
            CALL PMXCNW ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT, L )
            CALL ZMNSST
C           problem: 1-reenter dates/time, 2-return to transform screen
            SGRP = 107
            RESP = 1
            CALL QRESP ( MESSFL, SCLU, SGRP, RESP )
            IF (RESP .EQ. 1) THEN
C             reenter time steps
              AGAIN = 1
            ELSE
C             abandon defining transformation
              AGAIN = -1
            END IF
          END IF
        END IF
      IF (AGAIN .EQ. 1) GO TO 100
C
      IF (AGAIN .EQ. 0) THEN
C       everything is ok, save the values
        ILEN = 18
        CALL COPYI ( ILEN, ITMP, DATES )
        TSTEP = ITMP(19)
        TUNIT = CVAL(2)
        DTRAN = DTRVAL(CVAL(1))
        NAORO = CVAL(3)
        RETC  = 0
      ELSE
C       failed to define the transformation
        RETC = -1
      END IF
C
C     turn off previous command
      I= 4
      J= 0
      CALL ZSTCMA (I,J)
C
      RETURN
      END

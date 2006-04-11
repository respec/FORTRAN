C
C
C
      SUBROUTINE   WDPLOT
     I                    (MESSFL,WDMSFL)
C
C     This routine contains the calling sequences to get input from the
C     user and make time-series plots from time-series WDM data sets,
C     or make x-y plots from table data sets, time-series data sets,
C     or data-set attributes.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     WDMSFL - Fortran unit number of WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I1,SCLU,SGRP,RESP,FE,IC(7),INPFG,CMPTYP,WNDCHK,
     $             WSID, ICLOS, IWAIT, RETCOD, WNDFLG, WNDOPN
      CHARACTER*8  PTHNAM(2)
C
C     + + + LOCAL DEFINITIONS + + +
C     INPFG  - data input flag
C              0 - no input defined
C              1 - input data is defined
C     WNDFLG - flag for change in device type, size, or location
C              0 - no change
C              1 - changed, need to close and reopen workstation
C     WDNOPN - open workstation flag
C              0 - the workstation window is not open
C              1 - the workstation window is open
C
C     + + + EXTERNALS + + +
      EXTERNAL     GPOPEN, ZIPI, QRESP, GCLKS, ANPRGT
      EXTERNAL     WDPINP, PROPLT, PRNTXT
      EXTERNAL     PSTUPW, PLTONE, PDNPLT
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 93
C
C     open GKS and set unit number for error file (will be 99)
      CALL GPOPEN (FE)
C
      I1= 1
      I = 7
      CALL ZIPI (I,I1,IC)
C     get computer type
      CALL ANPRGT ( I1, CMPTYP )
C
C     initialize flags
      INPFG  = 0
      WNDCHK = 0
      WNDOPN = 0
 10   CONTINUE
C       do main graphics menu (input, modify, plot, return)
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        IF (RESP.EQ.1) THEN
C         specify input data
          CALL WDPINP (MESSFL,SCLU,WDMSFL,
     M                 INPFG)
        ELSE IF (RESP.EQ.2) THEN
C         make desired revisions
          PTHNAM(1) = 'Modify'
          PTHNAM(2) = 'GM'
          IF (INPFG.EQ.1) THEN
C           ok to do modifications
            CALL PROPLT (MESSFL,IC,PTHNAM,WNDFLG)
C           set flag to close&open if not already set
            IF (WNDCHK .EQ. 0) WNDCHK = WNDFLG
          ELSE
C           can't modify yet, no input defined
            SGRP= 2
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
        ELSE IF (RESP.EQ.3) THEN
C         generate plot
          IF (INPFG.EQ.1) THEN
C           ok to do plot, input has been defined
            WSID = 1
            IWAIT = 0
            IF (WNDCHK .EQ. 1  .AND.  WNDOPN .EQ. 1) THEN
C             device type or code changed or window moved, close & reopen
              ICLOS = 1
              CALL PDNPLT ( WSID, ICLOS, IWAIT )
              WNDCHK = 0
              WNDOPN = 0
            END IF
            CALL PSTUPW ( WSID, RETCOD )
            WNDOPN = 1
            IF (RETCOD .EQ. 0) THEN
              CALL PLTONE
            ELSE
C             device not opened, no plot, check error.fil for problem
              SGRP = 3
              CALL PRNTXT ( MESSFL, SCLU, SGRP )
            END IF
            IF (CMPTYP .EQ. 1) THEN
C             pc, always close workstation after plot
              ICLOS = 1
              CALL PDNPLT ( WSID, ICLOS, IWAIT )
              WNDOPN = 0
            ELSE
C             for other types, just deactivate workstation
              ICLOS = 0
              CALL PDNPLT ( WSID, ICLOS, IWAIT )
            END IF
          ELSE
C           can't plot yet, no input defined
            SGRP= 4
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
        END IF
C       repeat menu if not return and no data selected
      IF (RESP .NE. 4) GO TO 10
C
      IF (CMPTYP .NE. 1) THEN
C       not pc, need to close workstation
        WSID  = 1
        ICLOS = 1
        IWAIT = 0
        CALL PDNPLT ( WSID, ICLOS, IWAIT )
      END IF
C     close GKS
      CALL GCLKS
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDPINP
     I                   (MESSFL,SCLU,WDMSFL,
     M                    INPFG)
C
C     + + + PURPOSE + + +
C     Get input data for plotting from WDM file as well as specs
C     for type of plot, axes, and time period.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMSFL,INPFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number for WDM file
C     INPFG  - flag to indicate if input data has been defined
C              0 - no input data yet, 1 - input data defined
C
C     + + + PARAMETERS + + +
      INCLUDE 'pwdplt.inc'
      INTEGER   MAXV13
      PARAMETER ( MAXV13 = 13*MAXVAR )
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  SGRP, RESP, IND, DSN(DSNMAX), DSNCNT, DMAX, LPTYPE,
     $         PTYPE, MXCRV, VARMAX, NAMLEN, BLNKS, I, I6, I13, J,
     $         NVAR, NCRV, DEVCOD, RETC
      CHARACTER*1  SANAM(13,MAXVAR), VARNAM(13,MAXVAR)
C
C     + + + FUNCTIONS + + +
      INTEGER  LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   ANPRGT, WDTIME, WDXYPL, WDATRB, WDPINI
      EXTERNAL   QRESP,  PRNTXT, LENSTR, CHRDEL, CHRCHR
      EXTERNAL   GPINIT, GGNCRV, GPDVCD
C
C     + + + SAVES + + +
      SAVE        LPTYPE
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  DSN,     DSNCNT, LPTYPE, I6, I13, SANAM
     $    / DSNMAX*0,   0,      0,    6,  13, MAXV13*' ' /
C
C     + + + END SPECIFICATIONS + + +
C
      DMAX = DSNMAX
      MXCRV = MAXCRV
      VARMAX = MAXVAR
C
 10   CONTINUE
C       do main Input menu:  1) time series, 2) x-y of time series,
C         3) x-y of table, 4) x-y of attributes, 5) return
        SGRP= 8
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        IF (RESP .LT. 5) THEN
C         data type selected
          PTYPE = RESP
C         get specs for data type
          IF (PTYPE.NE.LPTYPE) THEN
C           different type of plot than last one, initialize graphics common
            LPTYPE= PTYPE
            CALL GPINIT
C           reset Device to screen
            IND = 40
            CALL ANPRGT (IND,DEVCOD)
            CALL GPDVCD ( DEVCOD )
          END IF
          IF (PTYPE.EQ.1) THEN
C           time-series plot
            CALL WDTIME (MESSFL, SCLU, WDMSFL, PTYPE, MXCRV,
     M                   DSN,
     O                   RETC)
          ELSE IF (PTYPE.EQ.2) THEN
C           x-y plot of time-series data
            CALL WDXYPL (MESSFL, SCLU, WDMSFL, PTYPE, VARMAX,
     M                   DSN,
     O                   RETC)
          ELSE IF (PTYPE.EQ.3) THEN
C           table, not currently implemented
            SGRP = 9
            CALL PRNTXT (MESSFL, SCLU, SGRP)
          ELSE IF (PTYPE.EQ.4) THEN
C           attribute plot
            CALL WDATRB (MESSFL, SCLU, WDMSFL, DMAX, VARMAX,
     M                   DSN, DSNCNT, SANAM,
     O                   RETC)
          END IF
        END IF
      IF (RESP .NE. 5  .AND.  RETC .NE. 0) GO TO 10
C
      CALL GGNCRV ( NCRV, NVAR )
      IF (NVAR .GT. 0) THEN
C       input data defined
        INPFG= 1
        IF (PTYPE .EQ. 4) THEN
C         attribute plot, pretty up names
          I = 13*VARMAX
          CALL CHRCHR (I, SANAM, VARNAM)
          DO 25 I = 1, NVAR
            NAMLEN = LENSTR(I6, VARNAM(1,I))
            IF (NAMLEN .LT. I6) THEN
C             remove blanks between variable name and the operator (+-*/)
              BLNKS = 6 - NAMLEN
              DO 20 J = NAMLEN+1, NAMLEN+BLNKS
                CALL CHRDEL (I13, NAMLEN+1, VARNAM(1,I))
 20           CONTINUE
            END IF
 25       CONTINUE
        END IF
C       set defaults for plot
        CALL WDPINI (WDMSFL, NVAR, DSN, VARNAM, PTYPE)
      ELSE
C       no input data defined
        INPFG = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDTIME
     I                   ( MESSFL, SCLU, WDMSFL, PTYPE, MXCRV,
     M                     DSN,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     This routine prompts the user for the time-series data sets to be
C     included in a time-series plot.  External routines are called which
C     check the data sets entered by the user and retrieve the data from
C     the user's WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  MESSFL, SCLU, WDMSFL, PTYPE, MXCRV, DSN(MXCRV), RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - message file cluster number
C     WDMSFL - Fortran unit number of user's WDM file
C     PTYPE  - plot type
C              1- time-series plot
C              2- x-y plot
C     MXCRV  - max. no. of curves that can be specified by user
C              (each dsn specified defines a curve)
C     DSN    - array containing data-set numbers
C     RETC   - return code
C               0 - data successfully selected and retrieved
C              -1 - data not selected and retrieved
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
      INCLUDE 'pbfmax.inc'
      INCLUDE 'pwdplt.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  SGRP, MXDSN, IRET, IVAL(MAXCRV), RETCOD,
     $         CMDID, I, I0, I1, NVAR, NCRV, IPOS, N, NV,
     $         DTRAN(MAXCRV), DTYPE(MAXCRV),
     $         TSTEP(MAXCRV), TUNITS(MAXCRV),
     $         SDATIM(6), EDATIM(6), BUFPOS(2,TSMAX), AGAIN
      REAL     YX(BUFMAX)
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDPCDT, WDPSTM, WDPTDT, GPTIME, GPNCRV, GPDATR
      EXTERNAL  Q1INIT, QSETI,  Q1EDIT, QGETI, PRNTXT
      EXTERNAL  PMXCNW, ZSTCMA, ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      NVAR = 0
      NCRV = 0
      I = 2 * TSMAX
      CALL ZIPI (I, I0, BUFPOS)
C
C     turn on Previous command
      CMDID = 4
      CALL ZSTCMA (CMDID, I1)
      MXDSN = MXCRV
 10   CONTINUE
        AGAIN = 0
C       allow user to input dsns/modify dsns previously entered
        SGRP = 12
        CALL Q1INIT ( MESSFL, SCLU, SGRP )
        CALL QSETI ( MXDSN, DSN )
        CALL Q1EDIT ( IRET )
        IF (IRET.EQ.1) THEN
C         user wants to continue, get data set numbers
          CALL QGETI ( MXDSN, IVAL )
C         check data sets, get time defaults
          CALL WDPCDT (MESSFL, SCLU, WDMSFL, MXDSN, PTYPE, IVAL,
     O                 NVAR, DSN, RETCOD)
          IF (RETCOD.EQ.0) THEN
C           get time period, time step/units
            CALL WDPSTM (MESSFL, SCLU, WDMSFL, NVAR,
     M                   DSN, NCRV,
     O                   SDATIM, EDATIM, TSTEP, TUNITS, DTRAN, 
     O                   RETCOD)
            IF (RETCOD .EQ. 0) THEN
C             number of curves and variables
              CALL GPNCRV ( NCRV, NVAR )
C             default DTYPE to mean
              CALL ZIPI ( MXCRV, I1, DTYPE )
C             put in common block
              CALL GPTIME ( TSTEP, TUNITS, SDATIM, EDATIM, DTYPE )
            END IF
          END IF
          IF (RETCOD .EQ. -1) THEN
C           enter data set numbers again
            AGAIN = 1
          ELSE IF (RETCOD .EQ. -2) THEN
C           return to graphics menu
            AGAIN = 0
          END IF
        ELSE IF (IRET .EQ. -1 ) THEN
C         oops, start over
          AGAIN = 1
        ELSE IF (IRET .EQ. 2) THEN
C         user chose Previous, return to Graphics input menu
          RETCOD = -2
        END IF
      IF (AGAIN .EQ. 1) GO TO 10
C
C     turn off Previous
      CALL ZSTCMA (CMDID, I0)
C
      IF (NVAR.GT.0 .AND. RETCOD.EQ.0) THEN
C       retrieve data, show message
        SGRP= 19
        I1  = 1
        CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,I)
        CALL WDPTDT (WDMSFL,NVAR,DSN,SDATIM,EDATIM,
     I               TSTEP,TUNITS,DTRAN,BUFMAX,
     O               YX,BUFPOS,RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         put retrieved data in common block
          IPOS = 1
          DO 50 N = 1, NVAR
            NV = BUFPOS(2,N) - BUFPOS(1,N) + 1
            CALL GPDATR ( N, IPOS, NV, YX(IPOS), RETCOD )
            IPOS = IPOS + NV
 50       CONTINUE
          RETC = 0
        ELSE
C         problem retrieving data, return to input screen
          RETC = -1
          SGRP = 18
          CALL PRNTXT ( MESSFL, SCLU, SGRP )
        END IF
      ELSE
C       return to input screen
        RETC = -1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDXYPL
     I                   (MESSFL, SCLU, WDMSFL, PTYPE, VARMAX,
     M                    DSN,
     O                    RETC)
C
C     + + + PURPOSE + + +
C     This routine prompts the user for time-series data sets to be
C     included in an x-y plot.  External routines are called which
C     check the data sets entered by the user and retrieve the data
C     from the user's WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  MESSFL, SCLU, WDMSFL, PTYPE, VARMAX, DSN(VARMAX), RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number of message file
C     WDMSFL - Fortran unit number of user's WDM file
C     PTYPE  - plot type
C              1- time-series plot
C              2- x-y plot
C     VARMAX - max. no. of variables (dsns) that can be specified by user
C     DSN    - array of data-set numbers chosen by user
C     RETC   - return code
C               0 - data successfully retrieved
C              -1 - data not retrieved
C
C     + + + PARAMETERS + + +
      INCLUDE 'pwdplt.inc'
      INCLUDE 'ptsmax.inc'
      INCLUDE 'pbfmax.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  SGRP, MXDSN, IVAL(MAXVAR), IRET, RETCOD, CMDID,
     $         SDATIM(6,MAXCRV), EDATIM(6,MAXCRV), I, I0, I1,
     $         TSTEP(MAXCRV), TUNITS(MAXCRV), DTRAN(MAXVAR),
     $         NVAR, NCRV, IPOS, N, NV, BUFPOS(2,TSMAX), AGAIN
      REAL     YX(BUFMAX)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDPCDT, WDPSTX, WDPTDX, GPNCRV, GPDATR
      EXTERNAL   Q1INIT, QSETI,  Q1EDIT, QGETI
      EXTERNAL   PMXCNW, ZSTCMA, ZIPI, PRNTXT
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      NVAR = 0
      NCRV = 0
      I = 2 * TSMAX
      CALL ZIPI (I, I0, BUFPOS)
      CMDID = 4
C
C     get time-series data sets for x-y plot
      MXDSN = VARMAX
 10   CONTINUE
        AGAIN = 0
C       turn on Previous command
        CALL ZSTCMA (CMDID, I1)
C
C       allow user to input dsns/modify dsns previously entered
        SGRP= 13
        CALL Q1INIT ( MESSFL, SCLU, SGRP )
        CALL QSETI ( MXDSN, DSN )
        CALL Q1EDIT ( IRET )
        IF (IRET.EQ.1) THEN
C         user wants to continue, check data sets, get time defaults
          CALL QGETI ( MXDSN, IVAL )
C         check data sets, get time defaults
          CALL WDPCDT (MESSFL, SCLU, WDMSFL, MXDSN, PTYPE, IVAL,
     O                 NVAR, DSN, RETCOD)
          IF (RETCOD.EQ.0) THEN
C           get time period, time steps and units
            CALL WDPSTX (MESSFL, SCLU, WDMSFL, NVAR,
     M                   DSN, NCRV,
     O                   SDATIM, EDATIM, TSTEP, TUNITS, DTRAN,
     O                   RETCOD)
            IF (RETCOD .EQ. 0) THEN
C             put number of curves and variables
              CALL GPNCRV ( NCRV, NVAR )
            END IF
          END IF
          IF (RETCOD .EQ. -1) THEN
C           enter data set numbers again
            AGAIN = 1
          ELSE IF (RETCOD .EQ. -2) THEN
C           return to graphics menu
            AGAIN = 0
          END IF
        ELSE IF (IRET .EQ. -1 ) THEN
C         oops, start over
          AGAIN = 1
        ELSE IF (IRET .EQ. 2) THEN
C         user chose Previous, return to Graphics input menu
          RETCOD = -2
        END IF
      IF (AGAIN .EQ. 1) GO TO 10
C
C     turn off Previous
      CALL ZSTCMA (CMDID, I0)
C
      IF (NVAR.GT.0 .AND. RETCOD.EQ.0) THEN
C       retrieve data, show message
        SGRP= 19
        I1  = 1
        CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,I)
        CALL WDPTDX (WDMSFL,NVAR,NCRV,DSN,SDATIM,EDATIM,
     I               TSTEP,TUNITS,DTRAN,BUFMAX,
     O               YX,BUFPOS,RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         put in common block
          IPOS = 1
          DO 50 N = 1, NVAR
            NV = BUFPOS(2,N) - BUFPOS(1,N) + 1
            CALL GPDATR ( N, IPOS, NV, YX(IPOS), RETCOD )
            IPOS = IPOS + NV
 50       CONTINUE
          RETC = 0
        ELSE
C         problem retrieving data
          RETC = -1
          SGRP = 18
          CALL PRNTXT ( MESSFL, SCLU, SGRP )
        END IF
      ELSE
C       no data to retrieve
        RETC = -1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDATRB
     I                    (MESSFL, SCLU, WDMSFL, DMAX, VARMAX,
     M                     DSN, DSNCNT, SANAM,
     O                     RETC)
C
C     + + + PURPOSE + + +
C     This routine gets names of attributes from user to plot an x-y plot
C     of attribute values for the data sets put in buffer by user.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  MESSFL, SCLU, WDMSFL, DMAX, VARMAX, DSN(DMAX), DSNCNT,
     $         RETC
      CHARACTER*1  SANAM(13,VARMAX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number of message file
C     WDMSFL - Fortran unit number of user's WDM file
C     DMAX   - maximum number of data sets that can be specified by user
C     VARMAX - max. no. of variables (attributes) that user can specify
C     DSN    - array of data-set numbers specified by user
C     DSNCNT - number of data sets specified by user
C     SANAM  - array of all attribute names entered by user
C     RETC   - return code
C               0 - data successfully retrieved
C              -1 - data not retrieved
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
      INCLUDE 'pbfmax.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  NROW, CNUM, SGRP, IVAL(5), CVAL(6,3,5), J, IPOS, NV,
     $         I, I0, I1, I6, I7, OPTY(5), OPTX(5), SAIND(2,10),
     $         SALEN(2,10), SATYP(2,10), IRET, RETCOD, INUM, RNUM,
     $         SCFLAG, MXLINE, SCINIT, WRTFLG, PREVID, ROWARR(5), N,
     $         NVAR, NCRV, BUFPOS(2,TSMAX), WHICH(TSMAX), WCHVR(3,TSMAX)
      REAL     RVAL, YX(BUFMAX)
      CHARACTER*1  TBUFF(80,5), BLNK
      CHARACTER*8  PTHNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPI, ZIPC, ZGTRET, ZSTCMA, QRESCX, PRWMSE, CHRCHR
      EXTERNAL  PMXTXI, PMXTXT, PRNTXT, WDPADT, CHKATR
      EXTERNAL  GPNCRV, GPDATR, GPVWCH, GPWCXY
C
C     + + + END SPECIFICATIONS + + +
C
      PTHNAM = 'GIA     '
      I0 = 0
      I1 = 1
      I6 = 6
      I7 = 7
      BLNK = ' '
      NVAR = 0
      NCRV = 0
      I = 2 * TSMAX
      CALL ZIPI (I, I0, BUFPOS)
      I = 20
      CALL ZIPI (I, I0, SAIND)
      CALL ZIPI (I, I0, SATYP)
      CALL ZIPI (I, I0, SALEN)
      RVAL = 0.0
      PREVID = 4
C     define variables for PMXTXI screens
      MXLINE = 10
      WRTFLG = 0
      INUM = 1
C
C     allow user to select data sets containing attributes
      CALL PRWMSE (MESSFL,WDMSFL,DMAX,PTHNAM,
     M             DSN,DSNCNT)
      IF (DSNCNT.GT.0) THEN
C       data sets selected
        NROW= 5
        CALL ZIPI (NROW, I0, ROWARR)
        CALL ZIPC (80*NROW,BLNK,TBUFF)
        CNUM= 6
        CALL ZIPI (CNUM*NROW*3,I0,CVAL)
        DO 10 I= 1,NROW
          IVAL(I)= I
C         copy any var. names, operators previously stored in SANAM to TBUFF
          J= 2*(I-1)+ 1
          CALL CHRCHR(I6, SANAM(1,J), TBUFF(2,I))
          CALL CHRCHR(I6, SANAM(1,J+1), TBUFF(8,I))
          CALL CHRCHR(I7, SANAM(7,J), TBUFF(14,I))
          CALL CHRCHR(I7, SANAM(7,J+1), TBUFF(21,I))
 10     CONTINUE
C       allow 'Previous' command
        CALL ZSTCMA (PREVID, I1)
C       identify attributes to be plotted
        INUM = 1
        RNUM = 1
        SCFLAG = 1
        SGRP= 14
        CALL QRESCX (MESSFL,SCLU,SGRP,INUM,RNUM,CNUM,NROW,SCFLAG,
     M               IVAL,RVAL,CVAL,TBUFF)
C       get user exit command
        CALL ZGTRET (IRET)
C       turn off Previous
        CALL ZSTCMA (PREVID, I0)
        IF (IRET.EQ.1) THEN
C         user wants to continue, copy user's input into SANAM
          DO 20 I = 1,NROW
            J= 2*(I-1)+ 1
            CALL CHRCHR(I6, TBUFF(2,I), SANAM(1,J))
            CALL CHRCHR(I6, TBUFF(8,I), SANAM(1,J+1))
            CALL CHRCHR(I7, TBUFF(14,I), SANAM(7,J))
            CALL CHRCHR(I7, TBUFF(21,I), SANAM(7,J+1))
 20       CONTINUE
C         check attributes specified by user
          CALL CHKATR (MESSFL, SCLU, NROW, CVAL, TBUFF, VARMAX,
     M                 SANAM,
     O                 OPTY, OPTX, SAIND, SATYP, SALEN, ROWARR,
     O                 NCRV, NVAR, RETCOD)
          SCINIT = RETCOD
          IF (NVAR.GT.0) THEN
C           get the data values
            CALL WDPADT (MESSFL, SCLU, WDMSFL, SAIND, SATYP, SALEN,
     I                   BUFMAX, DSNCNT, DSN, OPTY, OPTX, ROWARR,
     M                   NVAR, NCRV, SCINIT, SANAM,
     O                   YX, BUFPOS,WHICH,WCHVR)
          END IF
C
          IF (SCINIT .LT. 0) THEN
C           had some problems
            RETC = -1
            IF (NCRV .GT. 0) THEN
C             print error msg.s, show how many valid curves they have
              SGRP= 20
              CALL PMXTXI (MESSFL, SCLU, SGRP, MXLINE, SCINIT, WRTFLG,
     I                     INUM, NCRV)
            ELSE
C             print error msg.s, let user know there are no curves to plot
              SGRP = 21
              CALL PMXTXT (MESSFL, SCLU, SGRP, MXLINE, SCINIT)
            END IF
          ELSE IF (NCRV.EQ.0) THEN
C           no curves, will need one before plotting
            RETC = -1
            SGRP= 21
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          ELSE
C           show how many valid curves specified
C           set screen window name
            RETC = 0
            SGRP = 20
            CALL PMXTXI (MESSFL, SCLU, SGRP, MXLINE, SCINIT, WRTFLG,
     I                   INUM, NCRV)
C           put number of curves and number of variables
            CALL GPNCRV ( NCRV, NVAR )
C           put data in common
            IPOS = 1
            DO 50 N = 1, NVAR
              NV = BUFPOS(2,N) - BUFPOS(1,N) + 1
              CALL GPDATR ( N, IPOS, NV, YX(IPOS), RETCOD )
              IPOS = IPOS + NV
 50         CONTINUE
            CALL GPVWCH ( I1, NVAR, WHICH )
            DO 80 I = 1, NCRV
              CALL GPWCXY ( I, WCHVR(1,I), WCHVR(2,I) )
 80         CONTINUE
          END IF
        ELSE IF (IRET .EQ. 2) THEN
C         user chose Previous--drop out of this routine
C         and return to Graphics input menu
          RETC = -1
        END IF
C       end if (dsncnt.gt.0)
      ELSE
C       no data sets selected
        RETC = -1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKATR
     I                  (MESSFL, SCLU, NROW, CVAL, TBUFF, VARMAX,
     M                   SANAM,
     O                   OPTY, OPTX, SAIND, SATYP, SALEN, ROWARR,
     O                   NCRV, NVAR, RETCOD)
C
C     + + + PURPOSE + + +
C     This routine checks the names of attributes to be plotted,
C     along with the names of any attributes to be arithmetically
C     combined (+-/or*) with each pair of attributes.  Index number,
C     type, and length of each attribute is returned.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  MESSFL, SCLU, NROW, CVAL(6,3,NROW), VARMAX, OPTY(NROW),
     &         OPTX(NROW), SAIND(2,10), SATYP(2,10), SALEN(2,10),
     &         ROWARR(NROW), NCRV, NVAR, RETCOD
      CHARACTER*1  TBUFF(80,NROW), SANAM(13,VARMAX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number of message file
C     NROW   - number of rows of input allowed to user; a pair of
C              attributes can be specified on each row to define
C              a curve on the x-y plot
C     CVAL   - array containing information about char. responses from
C              attribute input screen
C     TBUFF  - array of all attribute names, arithmetic operators
C              entered by user
C     VARMAX - max. no. of variables (attr.s) that can be specified by user
C              (an attr. which is arithmetically combined w/ another is
C               considered to be 1 variable)
C     SANAM  - array of all attribute names entered by user
C     OPTY   - array of response sequence numbers of arithmetic operators
C              for y-axis
C     OPTX   - array of response sequence numbers of arithmetic operators
C              for x-axis
C     **Note:  For the following 3 arrays, the 1st row is used to store
C              values pertaining to the pairs of attributes specified
C              for each curve.  The 2nd row is used to store the values
C              pertaining to the optional attributes which can be
C              specified to modify the pairs.  Each column corresponds
C              to a variable number.
C     SAIND  - array of attribute index numbers
C     SATYP  - array of attribute data types
C     SALEN  - array of attribute lengths
C     ROWARR - array containing no. of input screen row on which user
C              specified each curve
C     NCRV   - number of curves specified by user
C     NVAR   - no. of variables (atributes) specified by user; 2 attrib.s
C              (which may be combined arithmetically w/ other attrib.s)
C              must be specified to define a curve
C     RETCOD - return code
C              0 - all ok
C             -1 - problem in user's specification of attributes
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I, I0, I6, INIT, INUM, CRVNUM, SGRP, IND1, IND2,
     $         IND3, IND4, SATYP1, SATYP2, SATYP3, SATYP4, SALEN1,
     $         SALEN2, SALEN3, SALEN4, MXLINE, WRTFLG,
     $         ATRLN1, ATRLN2
      CHARACTER*1  BLNK
C
C     + + + FUNCTIONS + + +
      INTEGER  LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXI, CHRCHR, LENSTR, ZIPI, ZIPC, CHKATX
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I6 = 6
      BLNK = ' '
      CALL ZIPI (NROW, I0, OPTY)
      CALL ZIPI (NROW, I0, OPTX)
      I = 130
      CALL ZIPC (I, BLNK, SANAM)
      INIT = 1
      WRTFLG = 1
      MXLINE = 10
C
      DO 10 I = 1,NROW
        ATRLN1 = LENSTR (I6, TBUFF(2,I))
        ATRLN2 = LENSTR (I6, TBUFF(8,I))
C       set curve number for PMXTXM screens
        IF ((ATRLN1.EQ.0 .AND. ATRLN2.GT.0) .OR.
     &      (ATRLN1.GT.0 .AND. ATRLN2.EQ.0)) THEN
C         only 1 attribute specified, need a pair for x-y plot
          INUM = 1
          CRVNUM = I
          SGRP = 25
          CALL PMXTXI (MESSFL, SCLU, SGRP, MXLINE, INIT, WRTFLG,
     I                 INUM, CRVNUM)
          INIT = -1
        ELSE IF (ATRLN1.GT.0 .AND. ATRLN2.GT.0) THEN
C         pair of attributes specified, check first one
          CALL CHKATX ( MESSFL, SCLU, I,
     M                  INIT, TBUFF(2,I),
     O                  IND1, SATYP1, SALEN1 )
C         check second attribute of pair
          CALL CHKATX ( MESSFL, SCLU, I,
     M                  INIT, TBUFF(8,I),
     O                  IND2, SATYP2, SALEN2 )
          IF (IND1.GT.0 .AND. IND2.GT.0) THEN
C           both pairs are good
            NCRV= NCRV+ 1
C           store no. of row of input screen on which curve specified by user
            ROWARR(NCRV) = I
            NVAR= 2*(NCRV-1)+ 1
            CALL CHRCHR (I6, TBUFF(2,I), SANAM(1,NVAR))
            SAIND(1,NVAR)= IND1
            SATYP(1,NVAR)= SATYP1
            SALEN(1,NVAR)= SALEN1
            NVAR= NVAR+ 1
            CALL CHRCHR (I6, TBUFF(8,I), SANAM(1,NVAR))
            SAIND(1,NVAR)= IND2
            SATYP(1,NVAR)= SATYP2
            SALEN(1,NVAR)= SALEN2
C
            IF (LENSTR(I6,TBUFF(15,I)) .GT. 0) THEN
C             user selected optional attribute for y-axis
              CALL CHKATX ( MESSFL, SCLU, I,
     M                      INIT, TBUFF(15,I),
     O                      IND3, SATYP3, SALEN3 )
              IF (IND3 .GT. 0) THEN
C               add operator and attribute name to y-axis attr. name in SANAM
                SANAM(7,NVAR-1) = TBUFF(14,I)
                CALL CHRCHR (I6, TBUFF(15,I), SANAM(8,NVAR-1))
                SAIND(2,NVAR-1) = IND3
                SATYP(2,NVAR-1) = SATYP3
                SALEN(2,NVAR-1) = SALEN3
                OPTY(I) = CVAL(3,1,I)
              ELSE
                OPTY(I) = 0
              END IF
            END IF
            IF (LENSTR(I6,TBUFF(22,I)) .GT. 0) THEN
C             user selected optional attribute for x-axis
              CALL CHKATX ( MESSFL, SCLU, I,
     M                      INIT, TBUFF(22,I),
     O                      IND4, SATYP4, SALEN4 )
              IF (IND4 .GT. 0) THEN
C               add operator and attribute name to x-axis attr. name in SANAM
                SANAM(7,NVAR) = TBUFF(21,I)
                CALL CHRCHR (I6, TBUFF(22,I), SANAM(8,NVAR))
                SAIND(2,NVAR) = IND4
                SATYP(2,NVAR) = SATYP4
                SALEN(2,NVAR) = SALEN4
                OPTX(I) = CVAL(5,1,I)
              ELSE
                OPTX(I) = 0
              END IF
            END IF
C           end if (ind1.gt.0 .and. ind2.gt.0)
          END IF
C         end else if (atrln1.gt.0 .and. atrln2.gt.0)
        END IF
 10   CONTINUE
C
      RETCOD = INIT
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDPCDT
     I                   (MESSFL, SCLU, WDMSFL, MXVAR, PTYPE, IVAL,
     O                    NVAR, DSN, RETCOD)
C
C     + + + PURPOSE + + +
C     Check data set existence, type, and time period
C     for data sets entered by user to be plotted.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMSFL,MXVAR,PTYPE,IVAL(MXVAR),
     &          NVAR,DSN(MXVAR),RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number for WDM file
C     MXVAR  - maximum number of data sets for plotting
C     PTYPE  - plot type, 1 - time series, 2 - X-Y
C     IVAL   - array of input data-set numbers
C     NVAR   - number of data sets
C     DSN    - buffer of data-set numbers for plotting
C     RETCOD - return code
C               0 - everything is ok
C              -1 - return to data set selection screen
C              -2 - return to input screen
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I, I0, J, SGRP, IRET, MXLN, INIT, IWRT, KOUNT,
     &           INUM, ICRV, LPTH, KIND
      CHARACTER*8 PTHNAM(2)
C
C     + + + FUNCTIONS + + +
      INTEGER    WDCKDT
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPI
      EXTERNAL   WDCKDT
      EXTERNAL   PMXTXI, ZMNSST, ZWNSOP, QRESP
C
C     + + + END SPECIFICATIONS + + +
C
C     make sure data set buffer is empty
      I0 = 0
      CALL ZIPI (MXVAR,I0,DSN)
C
C     set window names for messages
      LPTH = 2
      IF (PTYPE .EQ. 1) THEN
C       time series
        PTHNAM(1) = 'Time    '
        PTHNAM(2) = 'T       '
      ELSE
C       x-y plot
        PTHNAM(1) = 'X-Y     '
        PTHNAM(2) = 'X       '
      END IF
      CALL ZWNSOP ( LPTH, PTHNAM )
C
C     define variables for PMXTXI screens
      INUM = 1
      MXLN = 10
      INIT = 1
      IWRT = -1
      ICRV = 0
      NVAR = 0
      I    = 0
 100  CONTINUE
C       check each curve
        ICRV = ICRV + 1
        KOUNT = 0
        DO 125 J = 1, PTYPE
C         check each variable for the curve
          I = I + 1
          IF (IVAL(I) .GT. 0) THEN
C           is it the correct kind of data set?
            KIND = WDCKDT ( WDMSFL, IVAL(I) )
            IF (KIND .EQ. 1) THEN
C             time-series data set, as required
              KOUNT = KOUNT + 1
            ELSE
C             some problem with data set
              IF (KIND .EQ. 0) THEN
C               data set does not exist
                SGRP = 45
              ELSE IF (KIND .GT. 1) THEN
C               data set exists but is not time series
                SGRP = 46
              END IF
C             print message
              CALL PMXTXI (MESSFL,SCLU,SGRP,MXLN,INIT,IWRT,INUM,IVAL(I))
              CALL ZMNSST
              INIT = 0
            END IF
          END IF
 125    CONTINUE
        IF (PTYPE .EQ. 1  .AND.  KOUNT .EQ. 1) THEN
C         good data set for time series plot
          NVAR = NVAR + 1
          DSN(NVAR) = IVAL(I)
        ELSE IF (PTYPE .EQ. 2  .AND.  KOUNT .EQ. 2) THEN
C         good data sets for x-y plot
          NVAR = NVAR + 1
          DSN(NVAR) = IVAL(I-1)
          NVAR = NVAR + 1
          DSN(NVAR) = IVAL(I)
        ELSE IF (PTYPE .EQ. 2  .AND.  KOUNT .EQ. 1) THEN
C         x-y plot requires a pair of time series
          SGRP = 47
          CALL PMXTXI ( MESSFL,SCLU,SGRP,MXLN,INIT,IWRT,INUM,ICRV )
          CALL ZMNSST
          INIT = 0
        END IF
      IF (I .LT. MXVAR) GO TO 100
C
      IF (INIT .EQ. 1) THEN
C       all data sets are valid
        RETCOD = 0
      ELSE
C       some problems with data sets
        IF (NVAR .EQ. 0) THEN
C         no valid data sets, return to: 1-Select,2-Input
          SGRP = 48
          IRET = 1
        ELSE
C         at least one data set: 1-Select,2-Input,3-Continue
          SGRP = 49
          IRET = 3
        END IF
        CALL ZWNSOP ( LPTH, PTHNAM )
        CALL QRESP ( MESSFL, SCLU, SGRP, IRET )
        IF (IRET .EQ. 3) THEN
C         use remaining data sets
          RETCOD = 0
        ELSE
C         Select data sets or return to Input
          RETCOD = -IRET
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDPSTN
     I                    (MESSFL, SCLU, WDMSFL, PTYPE,
     I                     NVAR, DSN, BUFMAX,
     O                     SDATIM, EDATIM, TS, TU, DTRAN, NCRV, RETCOD)
C
C     + + + PURPOSE + + +
C     This routine finds start and end dates of each data set and
C     asks user to pick start and end dates, time step and
C     time units.  Non-zero return code if data set doesn't exist
C     or if there is no common time period.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,WDMSFL,PTYPE,NVAR,DSN(NVAR),BUFMAX,
     1          SDATIM(6),EDATIM(6),TS(5),TU(5),DTRAN(5),NCRV,RETCOD
C
C     + + + ARGUMENT DEFINITION + + +
C     MESSFL - Fortran unit number of ANNIEQ message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number of users WDM file
C     PTYPE  - plot type, 1 - time series, 2 - X-Y
C     NVAR   - number of data sets to use
C     DSN    - buffer of data set numbers
C     BUFMAX - total number of data points possible
C     SDATIM - starting date yr/mo/dy/hr/min/sec
C     EDATIM - ending date yr/mo/dy/hr/min/sec
C     TS     - array of number of time units in time interval
C     TU     - array of time units
C              (1-sec, 2-min, 3-hour, 4-day, 5-mon, 6-yr)
C     DTRAN  - array of data transformations for time-series data
C     NCRV   - number of curves
C     RETCOD - return code,
C              0 - all ok,
C             -1 - go back to data set specification screen
C             -2 - go back to graphics menu
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I, J, I0, I1, I6, INUM(2), IVAL(17), CVAL(10,3), SGRP,
     &            NPTS, IWRT, II, IC, FLAG, TOTPTS, SAIND, IRET, ILEN,
     &            CORM, LPTH, RETC, INIT
      REAL        RDUM(1)
      DOUBLE PRECISION DDUM(1)
      CHARACTER*1  BUFF(160), BLNK
      CHARACTER*8  PTHNAM(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL    COPYI, TIMDIF, ZIPI, ZIPC
      EXTERNAL    WTDATE, WDBSGI
      EXTERNAL    PMXTXI, QRESCZ, QRESP, ZWNSOP, ZGTRET, ZMNSST
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I6  = 6
      BLNK= ' '
C
      I= 5
      CALL ZIPI (I,I0,TS)
      CALL ZIPI (I,I0,TU)
      CALL ZIPI (I,I0,DTRAN)
C
C     set window name
      LPTH = 2
      IF (PTYPE .EQ. 1) THEN
C       time-series plot
        PTHNAM(1) = 'Time    '
        PTHNAM(2) = 'T       '
      ELSE IF (PTYPE .EQ. 2) THEN
C       x-y plot
        PTHNAM(1) = 'X-Y     '
        PTHNAM(2) = 'X       '
      END IF
C
C     get common time period
      CORM = 1
      CALL WTDATE (WDMSFL,NVAR,DSN,CORM,
     O             SDATIM,EDATIM,RETCOD)
      NCRV = 0
      IF (RETCOD.EQ.0) THEN
C       dates retrieved ok
        DO 100 I= 1,NVAR,PTYPE
C         get time step and units for each curve
          NCRV= NCRV+ 1
          SAIND= 33
          CALL WDBSGI (WDMSFL,DSN(I),SAIND,I1,
     O                 TS(NCRV),RETC)
          IF (RETC .NE. 0) RETCOD = RETC
          SAIND= 17
          CALL WDBSGI (WDMSFL,DSN(I),SAIND,I1,
     O                 TU(NCRV),RETCOD)
          IF (RETC .NE. 0) RETCOD = RETC
 100    CONTINUE
      END IF

      IF (RETCOD .NE. 0) THEN
C       problem with at least one data set
        IF (RETCOD.EQ.-6) THEN
C         not all data sets have data
          SGRP= 12
        ELSE IF (RETCOD.EQ.2) THEN
C         no common period found
          SGRP= 25
        ELSE
C         some other problem, (should be caught above, but just in case)
          SGRP= 13
          WRITE (99,*) 'In WDPSTN, retcod =', RETCOD
        END IF
C       return to : 1-dsn menu, 2-graphics menu
        CALL ZWNSOP ( LPTH, PTHNAM )
        CALL QRESP ( MESSFL, SCLU, SGRP, IRET )
        RETCOD = -IRET
      ELSE
C       get info from user about curves
 10     CONTINUE
          FLAG= 0
          II = NCRV+ 12
          IC = 2*NCRV
          CALL COPYI (I6,SDATIM,IVAL(1))
          CALL COPYI (I6,EDATIM,IVAL(7))
          DO 20 I= 1,NCRV
            IVAL(12+I) = TS(I)
            J= 2*(I-1)+ 1
            CVAL(J,1)  = TU(I)
            CVAL(J+1,1)= DTRAN(I) + 1
 20       CONTINUE
          CALL ZWNSOP ( LPTH, PTHNAM )
          SGRP = 14+ NCRV
          ILEN = 20*NCRV+ 60
          CALL ZIPC (ILEN,BLNK,BUFF)
          CALL QRESCZ (MESSFL,SCLU,SGRP,II,I1,I1,IC,I1,I1,ILEN,
     M                 IVAL,RDUM,DDUM,CVAL,BUFF)
C         get user exit command value
          CALL ZGTRET (IRET)
          IF (IRET.EQ.1) THEN
C           user wants to continue
            CALL COPYI (I6,IVAL(1),SDATIM)
            CALL COPYI (I6,IVAL(7),EDATIM)
            TOTPTS= 0
            DO 30 I= 1,NCRV
              TS(I)   = IVAL(12+I)
              J= 2*(I-1)+ 1
              TU(I)   = CVAL(J,1)
              DTRAN(I)= CVAL(J+1,1) - 1
C             compute number of points
              CALL TIMDIF (SDATIM,EDATIM,TU(I),TS(I),NPTS)
              TOTPTS= TOTPTS+ PTYPE*NPTS
 30         CONTINUE
C
            IF (TOTPTS .GT. BUFMAX) THEN
C             Too many data points to plot, current is &, max is &.
              INIT = 1
              IWRT = -1
              II = 2
              INUM(1) = TOTPTS
              INUM(2) = BUFMAX
              IF (NCRV .EQ. 1) THEN
C               don't include line about reduceing # of curves
                SGRP = 41
              ELSE
C               include line about reduceing # of curves
                SGRP = 42
              END IF
              CALL ZMNSST
              CALL ZWNSOP ( LPTH, PTHNAM )
              CALL PMXTXI (MESSFL,SCLU,SGRP,I1,INIT,IWRT,II,INUM)
C             return to: 1-select dsn, 2-graphics, 3-change time
              SGRP = 43
              CALL ZMNSST
              CALL QRESP ( MESSFL, SCLU, SGRP, IRET )
              IF (IRET .EQ. 3) THEN
C               change the time period and/or time step
                FLAG = 1
              ELSE
C               abandon these curves
                RETCOD = -IRET
                FLAG = 0
              END IF
            END IF
          ELSE
C           back to data set specification screen
            RETCOD= -1
          END IF
        IF (FLAG .EQ. 1) GO TO 10
      END IF
C
      IF (RETCOD .NE. 0) THEN
C       user chose to return to dsn specification screen or graphics screen
C       reset number of variables and curves back to zero
        NVAR = 0
        NCRV = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDPTDT
     I                   (WDMSFL,NVAR,DSN,SDATIM,EDATIM,
     I                    TS,TU,DTRAN,BUFMAX,
     O                    YX,BUFPOS,RETCOD)
C
C     + + + PURPOSE + + +
C     Gets time-series data from a WDM file for the specified
C     data sets and time period and puts it in the YX array for
C     a time series plot.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,NVAR,DSN(NVAR),SDATIM(6),EDATIM(6),TS(NVAR),
     $          TU(NVAR),DTRAN(NVAR),BUFMAX,BUFPOS(2,NVAR),RETCOD
      REAL      YX(BUFMAX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     NVAR   - number of data sets
C     DSN    - data set numbers to use
C     SDATIM - starting date yr,mo,dy,hr,min,sec
C     EDATIM - ending date yr,mo,dy,hr,min,sec
C     TS     - array of time steps
C     TU     - array of time units (1-sec,2-min,3-hr,4-dy,5-mo,-6-yr)
C     DTRAN  - array of data transformations for time-series data
C     BUFMAX - size of YX array
C     YX     - array of values to plot
C     BUFPOS - array of positions in YX buffer for start/end of data
C     RETCOD - return code from time-series get, 0 if OK
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,NPTS,IPOS,QFLAG
C
C     + + + EXTERNALS + + +
      EXTERNAL   TIMDIF, TSBWDS, TSBTIM, TSBGET
C
C     + + + END SPECIFICATIONS + + +
C
      IPOS  = 1
      QFLAG = 30
      I = 1
 100  CONTINUE
C       determine number of points for this curve
        CALL TIMDIF (SDATIM,EDATIM,TU(I),TS(I),NPTS)
C       get the data from the WDM file
        IF (NPTS.LE.0) THEN
C         problem here, can't get data
          RETCOD = -1
        ELSE
C         everything a-okay
          CALL TSBWDS (WDMSFL,DSN(I))
          CALL TSBTIM (TU(I),TS(I),DTRAN(I),QFLAG)
          CALL TSBGET (SDATIM,NPTS,
     O                 YX(IPOS),RETCOD)
        END IF
        IF (RETCOD .EQ. 0) THEN
C         set start/ending positions in buffer
          BUFPOS(1,I)= IPOS
          BUFPOS(2,I)= IPOS+ NPTS- 1
C         write (99,*)'Set BUFPOS,NVAR,BUFPOS',I,BUFPOS(1,I),BUFPOS(2,I)
C         WRITE (99,*) 'In WDPTDT:  get dsn,retc=', DSN(I), RETCOD
          IPOS= IPOS+ NPTS
        ELSE
C         problem retrieving time series data
          WRITE (99,*) 'In WDPTDT:  error retrieving data'
          WRITE (99,*) '          dsn, retcod=', DSN(I), RETCOD
        END IF
C       increment variable counter
        I = I + 1
      IF (I .LE. NVAR  .AND.  RETCOD .EQ. 0) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDPTDX
     I                   (WDMSFL,NVAR,NCRV,DSN,SDATIM,EDATIM,
     I                    TS,TU,DTRAN,BUFMAX,
     O                    YX,BUFPOS,RETCOD)
C
C     + + + PURPOSE + + +
C     Gets time-series data from a WDM file for the specified
C     data set and time period and puts it in the YX array for
C     an X-Y time plot.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,NVAR,NCRV,DSN(NVAR),
     $          SDATIM(6,NCRV),EDATIM(6,NCRV),TS(NCRV),TU(NCRV),
     $          DTRAN(NVAR),BUFMAX,BUFPOS(2,NVAR), RETCOD
      REAL      YX(BUFMAX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     PTYPE  - plot type, 1 - time series, 2 - X-Y
C     NVAR   - number of variables
C     NCRV   - number of curves (should be half NVAR)
C     DSN    - data set numbers to use
C     SDATIM - starting date yr,mo,dy,hr,min,sec
C     EDATIM - ending date yr,mo,dy,hr,min,sec
C     TS     - array of time steps
C     TU     - array of time units (1-sec,2-min,3-hr,4-dy,5-mo,-6-yr)
C     DTRAN  - array of data transformations for time-series data
C     BUFMAX - size of YX array
C     YX     - array of values to plot
C     BUFPOS - array of positions in YX buffer for start/end of data
C     RETCOD - return code from time-series get, 0 if OK
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,NPTS,IPOS,QFLAG
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   TIMDIF, TSBWDS, TSBTIM, TSBGET
C
C     + + + END SPECIFICATIONS + + +
C
      IPOS  = 1
      QFLAG = 30
      I = 1
      J = 0
 100  CONTINUE
        IF (MOD(I,2) .EQ. 1) THEN
C         first of two variables for this curve, increment curve counter
          J = J + 1
C         how many points for the curve
          CALL TIMDIF (SDATIM(1,J),EDATIM(1,J),TU(J),TS(J),NPTS)
        END IF
C       get the data from the WDM file
        IF (NPTS.LE.0) THEN
C         problem here, can't get data
          RETCOD = -1
        ELSE
C         everything a-okay
          CALL TSBWDS (WDMSFL,DSN(I))
          CALL TSBTIM (TU(J),TS(J),DTRAN(I),QFLAG)
          CALL TSBGET (SDATIM(1,J),NPTS,
     O                 YX(IPOS),RETCOD)
        END IF
        IF (RETCOD .EQ. 0) THEN
C         set start/ending positions in buffer
          BUFPOS(1,I)= IPOS
          BUFPOS(2,I)= IPOS+ NPTS- 1
C         write (99,*)'Set BUFPOS,NVAR,BUFPOS',I,BUFPOS(1,I),BUFPOS(2,I)
C         WRITE (99,*) 'In WDPTDX:  get dsn,retc=', DSN(I), RETCOD
          IPOS= IPOS+ NPTS
        ELSE
C         problem retrieving time series data
          WRITE (99,*) 'In WDPTDX:  error retrieving data'
          WRITE (99,*) '          dsn, retcod=', DSN(I), RETCOD
        END IF
C       increment variable counter
        I = I + 1
      IF (I .LE. NVAR  .AND.  RETCOD .EQ. 0) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDPADT
     I                   (MESSFL,SCLU,WDMSFL,SAIND,SATYP,SALEN,
     I                    BUFMAX,DSNCNT,DSN,OPTY,OPTX,ROWARR,
     M                    NVAR,NCRV,INIT,SANAM,
     O                    YX,BUFPOS,WHICH,WCHVR)
C
C     + + + PURPOSE + + +
C     Gets attribute values from a WDM file for the specified
C     data sets and puts them in the YX array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMSFL, NVAR, NCRV, SAIND(2,NVAR),
     $          SATYP(2,NVAR), SALEN(2,NVAR), BUFMAX, DSNCNT,
     $          DSN(DSNCNT), OPTY(5), OPTX(5), ROWARR(5), INIT,
     $          BUFPOS(2,NVAR), WHICH(NVAR), WCHVR(3,NCRV)
      REAL      YX(BUFMAX)
      CHARACTER*1  SANAM(13,NVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number of message file
C     WDMSFL - Fortran unit number of WDM file
C     **Note:  For the following 3 arrays, the 1st row is used to store
C              values pertaining to the pairs of attributes specified
C              for each curve.  The 2nd row is used to store the values
C              pertaining to the optional attributes which can be
C              specified to modify the pairs.  Each column corresponds
C              to a variable number.
C     SAIND  - array of attribute indices being plotted
C     SATYP  - array of attribute types being plotted
C     SALEN  - array of attribute lengths being plotted
C     BUFMAX - size of YX array
C     DSNCNT - count of data sets
C     DSN    - array of data-set numbers containing attributes
C     OPTY   - array of operators specified by user for y-axis attr. combinations
C     OPTX   - array of operators specified by user for x-axis attr. combinations
C     ROWARR - array containing no. of input screen row on which user
C              entered attributes for each curve
C     NVAR   - number of attributes (2 per curve)
C     NCRV   - number of curves defined by user
C     INIT   - flag signaling problems encountered; used in screen
C              drawing--> if INIT passed in as 0, no prob.s encountered
C              so far, screen must be cleared before writing out any
C              error messages in this routine; if INIT passed in as -1,
C              prob.s already encountered so don't clear screen if write
C              out error messages (add error msg.s to previous ones)
C     SANAM  - array of all valid attribute names entered by user
C     YX     - array of values to plot
C     BUFPOS - array of positions in YX buffer for start/end of data
C     WHICH  - ?????
C     WCHVAR - ?????
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I, I13, ICRV, IPOS, ERRFLG,
     $           VAR1, TOTVLS, CURRVR, LENGTH, IGDCRV, IVAR, OPTION
      REAL       TEMPYX(600,2)
      CHARACTER*1  BLNK, TMPNAM(13,20)
C
C     + + + EXTERNALS + + +
      EXTERNAL   CHRCHR, FILTMP, ZIPC, MOVTMP
C
C     + + + END SPECIFICATIONS + + +
C
      I13 = 13
      BLNK = ' '
C
C     use CURRVR to store order no. of 1st var. in all pairs spec'd. by user
      CURRVR = 1
C     use VAR1 to store order no. of 1st var. in good pairs of variables
      VAR1 = 1
      IGDCRV = 0
      IPOS  = 1
      IVAR = 0
C
      DO 10 ICRV = 1,NCRV
C       fill temporary buffer w/ attribute values for curve ICRV
        CALL FILTMP (MESSFL, SCLU, WDMSFL, DSNCNT, DSN, ROWARR(ICRV),
     I               NVAR, SANAM, SAIND, SATYP, SALEN, CURRVR,
     M               INIT,
     O               TEMPYX, TOTVLS, ERRFLG)
C
        IF (ERRFLG .EQ. 0) THEN
C         increment number of good curves
          IGDCRV = IGDCRV + 1
C         write out variable names to TMPNAM
          CALL CHRCHR (I13, SANAM(1,CURRVR), TMPNAM(1,VAR1))
          CALL CHRCHR (I13, SANAM(1,CURRVR+1), TMPNAM(1,VAR1+1))
C
C         set y-axis parameters
          IVAR = IVAR + 1
          WCHVR(1,ICRV) = IVAR
          WHICH(IVAR) = 1
          BUFPOS(1,IVAR) = IPOS
          BUFPOS(2,IVAR) = IPOS + TOTVLS - 1
C         set y-axis plotting values
          OPTION = OPTY(ROWARR(ICRV))
          CALL MOVTMP ( OPTION, TEMPYX(1,1), TOTVLS, DSNCNT,
     M                  IPOS,
     O                  YX )
C
C         set x-axis parameters
          IVAR = IVAR + 1
          WCHVR(2,ICRV) = IVAR
          WHICH(IVAR) = 4
          BUFPOS(1,IVAR) = IPOS
          BUFPOS(2,IVAR) = IPOS + TOTVLS - 1
C         set x-axis plotting values
          OPTION = OPTX(ROWARR(ICRV))
          CALL MOVTMP ( OPTION, TEMPYX(1,2), TOTVLS, DSNCNT,
     M                  IPOS,
     O                  YX )
C         increment var1 to equal order no of 1st variable in next pair
          VAR1 = VAR1 + 2
        END IF
        CURRVR = CURRVR + 2
 10   CONTINUE
C
C     get length of SANAM to be blanked out before SANAM is rewritten
      LENGTH = NVAR*13
C
C     reset NCRV and NVAR based on no. of curves that can be plotted
      NCRV = IGDCRV
      NVAR = IGDCRV*2
C
C     copy TMPNAM to SANAM; TMPNAM holds only var. names of good curves
      CALL ZIPC (LENGTH, BLNK, SANAM)
      DO 30 I = 1, NVAR
        CALL CHRCHR (I13, TMPNAM(1,I), SANAM(1,I))
 30   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   FILTMP
     I                    (MESSFL, SCLU, WDMSFL, DSNCNT, DSN, CRVNUM,
     I                     NVAR, SANAM, SAIND, SATYP, SALEN, VRINDX,
     M                     SCINIT,
     O                     TEMPYX, TOTVLS, ERRFLG)
C
C     + + + PURPOSE + + +
C     This routine fills the temporary buffer TEMPYX with the values of
C     the attribute pair that defines curve CRVNUM.  The structure of
C     TEMPYX is as follows:  the 1st column stores values corresponding to
C     the 1st attribute in the pair; the first 1...DSNCNT rows are filled
C     with the values found for the attribute on each data set; the
C     following DSNCNT+1...(DSNCNT*2) rows are filled with the values for
C     any optional attribute that is to be arithmetically combined with the
C     1st attribute.  Values for the 2nd attribute in the pair, along with
C     the values of any optional attribute to be combined with it, are
C     stored in the 2nd column of TEMPYX following the same format.  If any
C     data set does not have a value for one of the attributes, the row in
C     TEMPYX corresponding to that data set is not filled.  Error messages
C     are written if data sets were specified by the user which did not
C     have values for one or both of the attributes in the pair or for the
C     optional attributes to be combined with the attributes in the pair.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  MESSFL, SCLU, WDMSFL, DSNCNT, DSN(DSNCNT), CRVNUM,
     &         NVAR, SAIND(2,NVAR), SATYP(2,NVAR), SALEN(2,NVAR),
     &         VRINDX, SCINIT, TOTVLS, ERRFLG
      REAL     TEMPYX(600,2)
      CHARACTER*1  SANAM(13,NVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number of message file
C     WDMSFL - Fortran unit number of WDM file
C     DSNCNT - count of data sets
C     DSN    - array of data-set numbers specified by user
C     CRVNUM - no. of curve to which current variable belongs
C              note:  this is the number of the row of the input screen on
C                     which user entered the attrib.s defining the curve
C     NVAR   - no. of variables that user has specified (includes only
C              the var.s from the main pairs, i.e. opt. attrib.s not
C              included in this total)
C     SANAM  - array of all valid attribute names entered by user
C     **Note:  For the following 3 arrays, the 1st row is used to store
C              values pertaining to the pairs of attributes specified
C              for each curve.  The 2nd row is used to store the values
C              pertaining to the optional attributes which can be
C              specified to modify the pairs. Each column corresponds
C              to a variable number.
C     SAIND  - array of attribute indices being plotted
C     SATYP  - array of attribute types being plotted
C     SALEN  - array of attribute lengths being plotted
C     VRINDX - index of first variable (attribute) in pair which is
C              currently being processed
C     SCINIT - flag signaling problems encountered; used in screen
C              drawing--> if INIT passed in as 0, no prob.s encountered
C              so far, screen must be cleared before writing out any
C              error messages in this routine; if INIT passed in as -1,
C              prob.s already encountered so don't clear screen if write
C              out error messages (add error msg.s to previous ones)
C     TEMPYX - temporary buffer which holds attribute values in addition
C              to the values of any attributes they are to be arithme-
C              tically combined with
C     TOTVLS - total no. of values put in TEMPYX; will be equal to DSNCNT
C              unless there was no value on 1 or more data sets for one
C              or both of the attributes in the current pair or no value
C              for the optional attributes they were to be combined with
C     ERRFLG - error flag
C              0-TEMPYX loaded ok
C             -1-all dsns specified did not have a value for one or both
C                of the attributes in the pair; TEMPYX could not be filled
C                for curve CRVNUM
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I, I6, NULVL1, NULVL2, ERRCD1, ERRCD2, COUNT, SGRP,
     &         NUMVAR, VARTYP(4), MXLINE, WRTFLG, IVAL(2), CLEN(4),
     &         ATRLN1, ATRLN2, OPTLN1, OPTLN2, DSNO, OPERR1, OPERR2,
     &         CNUM, INUM, POS
      REAL     ATRVL1, ATRVL2, OPTVL1, OPTVL2, RVAL, R0
      DOUBLE PRECISION  DVAL
      CHARACTER*1  CVAL(24), ATRNM1(6), ATRNM2(6), OPTNM1(6), OPTNM2(6),
     &             BLNK
C
C     + + + FUNCTIONS + + +
      INTEGER  LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXA, PMXTXI, PMXTXM, CHRCHR, LENSTR, ZIPC, ZIPR
      EXTERNAL   GETVAL
C
C     + + + END SPECIFICATIONS + + +
C
      I6 = 6
      R0 = 0
      RVAL = 0
      DVAL = 0
      MXLINE = 10
      WRTFLG = -1
      BLNK = ' '
      I = 24
      CALL ZIPC (I, BLNK, CVAL)
      I = 1200
      CALL ZIPR (I, R0, TEMPYX)
C
C     get names of attributes in case there are error msg.s to be written
      CALL CHRCHR (I6, SANAM(1,VRINDX), ATRNM1(1))
      CALL CHRCHR (I6, SANAM(1,VRINDX+1), ATRNM2(1))
C     get length of names
      ATRLN1 = LENSTR(I6, ATRNM1(1))
      ATRLN2 = LENSTR(I6, ATRNM2(1))
      IF (SAIND(2,VRINDX) .NE. 0) THEN
C       get name of optional attrib. to be combined w/ 1st attrib.
        CALL CHRCHR (I6, SANAM(8,VRINDX), OPTNM1(1))
C       get length of name
        OPTLN1 = LENSTR(I6, OPTNM1(1))
      ELSE
        OPTLN1 = 0
      END IF
      IF (SAIND(2,VRINDX+1) .NE. 0) THEN
C       get name of optional attrib. to be combined w/ 2nd attrib.
        CALL CHRCHR (I6, SANAM(8,VRINDX+1), OPTNM2(1))
C       get length of name
        OPTLN2 = LENSTR(I6, OPTNM2(1))
      ELSE
        OPTLN2 = 0
      END IF
C
      NULVL1 = 0
      NULVL2 = 0
      COUNT = 0
      DO 10 DSNO = 1, DSNCNT
C       get value for 1st attrib. in pair
        CALL GETVAL (WDMSFL, DSN(DSNO), SAIND(1,VRINDX),
     I               SATYP(1,VRINDX), SALEN(1,VRINDX),
     O               ATRVL1, ERRCD1)
C       get value for 2nd attrib. in pair
        CALL GETVAL (WDMSFL, DSN(DSNO), SAIND(1,VRINDX+1),
     I               SATYP(1,VRINDX+1), SALEN(1,VRINDX+1),
     O               ATRVL2, ERRCD2)
        IF (ERRCD1.EQ.0 .AND. ERRCD2.EQ.0) THEN
C         get values for optional attributes, if any
          IF (SAIND(2,VRINDX) .NE. 0) THEN
C           optional attribute to be combined w/ 1st attrib. in pair
            CALL GETVAL (WDMSFL, DSN(DSNO), SAIND(2,VRINDX),
     I                   SATYP(2,VRINDX), SALEN(2,VRINDX),
     O                   OPTVL1, OPERR1)
            IF (OPERR1 .NE. 0)  OPERR1 = -10
          ELSE
C           set OPERR1 to undefined value
            OPERR1 = -99
          END IF
          IF (SAIND(2,VRINDX+1) .NE. 0) THEN
C           optional attribute to be combined w/ 2nd attrib. in pair
            CALL GETVAL (WDMSFL, DSN(DSNO), SAIND(2,VRINDX+1),
     I                   SATYP(2,VRINDX+1), SALEN(2,VRINDX+1),
     O                   OPTVL2, OPERR2)
            IF (OPERR2 .NE. 0)  OPERR2 = -10
          ELSE
C           set OPERR2 to undefined value
            OPERR2 = -99
          END IF
C
          IF (OPERR1.EQ.-10 .OR. OPERR2.EQ.-10) THEN
C           1 or both optional attr. values do not exist for this data set
            NULVL2 = NULVL2 + 1
          ELSE
            COUNT = COUNT + 1
            TEMPYX(COUNT,1) = ATRVL1
            TEMPYX(COUNT,2) = ATRVL2
            IF (OPERR1.EQ.0 .AND. OPERR2.EQ.0) THEN
C             both var.s in pair have valid optional attrib. values
              TEMPYX(DSNCNT+COUNT,1) = OPTVL1
              TEMPYX(DSNCNT+COUNT,2) = OPTVL2
            ELSE IF (OPERR1 .EQ. 0) THEN
C             only 1st var. has a valid opt. attrib. to be combined with
              TEMPYX(DSNCNT+COUNT,1) = OPTVL1
            ELSE IF (OPERR2 .EQ. 0) THEN
C             only 2nd var. has a valid opt. attrib. to be combined with
              TEMPYX(DSNCNT+COUNT,2) = OPTVL2
            END IF
          END IF
C
        ELSE
C         1 or both attrib. values do not exist for this data set
          NULVL1 = NULVL1 + 1
        END IF
 10   CONTINUE
C
      TOTVLS = DSNCNT - (NULVL1 + NULVL2)
C
      IF (TOTVLS .EQ. 0) THEN
C       no values to plot
        IF (NULVL1 .EQ. DSNCNT) THEN
C         y-axis and/or x-axis attribute not on data sets specified
          CNUM = 2
          CLEN(1) = ATRLN1
          CLEN(2) = ATRLN2
          CALL CHRCHR (ATRLN1, ATRNM1, CVAL)
          CALL CHRCHR (ATRLN2, ATRNM2, CVAL(ATRLN1+1))
          SGRP = 31
          CALL PMXTXA (MESSFL, SCLU, SGRP, MXLINE, SCINIT, WRTFLG,
     I                 CNUM, CLEN, CVAL)
          SCINIT = -1
C         let user know which curve won't be plotted
          INUM = 1
          SGRP = 39
          CALL PMXTXI (MESSFL, SCLU, SGRP, MXLINE, SCINIT, WRTFLG,
     I                 INUM, CRVNUM)
        ELSE IF (NULVL2 .EQ. DSNCNT) THEN
C         y-axis and/or x-axis optional attrib. not on data sets specified
          IF (OPTLN1.GT.0 .AND. OPTLN2.GT.0) THEN
C           both y-axis and x-axis optional attrib.s were specified
            CNUM = 2
            CLEN(1) = OPTLN1
            CLEN(2) = OPTLN2
            CALL CHRCHR (OPTLN1, OPTNM1, CVAL)
            CALL CHRCHR (OPTLN2, OPTNM2, CVAL(OPTLN1+1))
            SGRP = 31
          ELSE IF (OPTLN1 .GT. 0) THEN
C           only y-axis optional attrib. specified
            CNUM = 1
            CLEN(1) = OPTLN1
            CALL CHRCHR (OPTLN1, OPTNM1, CVAL)
            SGRP = 32
          ELSE IF (OPTLN2 .GT. 0) THEN
C           only x-axis optional attrib. specified
            CNUM = 1
            CLEN(1) = OPTLN2
            CALL CHRCHR (OPTLN2, OPTNM2, CVAL)
            SGRP = 32
          END IF
          CALL PMXTXA (MESSFL, SCLU, SGRP, MXLINE, SCINIT, WRTFLG,
     I                 CNUM, CLEN, CVAL)
          SCINIT = -1
C         let user know which curve won't be plotted
          INUM = 1
          SGRP = 39
          CALL PMXTXI (MESSFL, SCLU, SGRP, MXLINE, SCINIT, WRTFLG,
     I                 INUM, CRVNUM)
        ELSE
C         no valid combo.s of 1 or both attr.s w/ opt. attr.s can be plotted
C         due to non-existent values of each attribute on data sets specified
          IF (OPTLN1.GT.0 .AND. OPTLN2.GT.0) THEN
C           both attrib.s in pair had optional attrib. specified
            CNUM = 4
            CLEN(1) = ATRLN1
            CLEN(2) = OPTLN1
            CLEN(3) = ATRLN2
            CLEN(4) = OPTLN2
            CALL CHRCHR (ATRLN1, ATRNM1(1), CVAL(1))
            POS = ATRLN1 + 1
            CALL CHRCHR (OPTLN1, OPTNM1(1), CVAL(POS))
            POS = POS + OPTLN1
            CALL CHRCHR (ATRLN2, ATRNM2(1), CVAL(POS))
            POS = POS + ATRLN2
            CALL CHRCHR (OPTLN2, OPTNM2(2), CVAL(POS))
            SGRP = 35
          ELSE IF (OPTLN1 .GT. 0) THEN
C           only 1st (y-axis) attrib. had optional attrib. specified
            CNUM = 2
            CLEN(1) = ATRLN1
            CLEN(2) = OPTLN1
            CALL CHRCHR (ATRLN1, ATRNM1, CVAL)
            CALL CHRCHR (OPTLN1, OPTNM1, CVAL(ATRLN1+1))
            SGRP = 36
          ELSE IF (OPTLN2 .GT. 0) THEN
C           only 2nd (x-axis) attrib. had optional attrib. specified
            CNUM = 2
            CLEN(1) = ATRLN2
            CLEN(2) = OPTLN2
            CALL CHRCHR (ATRLN2, ATRNM2, CVAL)
            CALL CHRCHR (OPTLN2, OPTNM2, CVAL(ATRLN2+1))
            SGRP = 36
          END IF
          CALL PMXTXA (MESSFL, SCLU, SGRP, MXLINE, SCINIT, WRTFLG,
     I                 CNUM, CLEN, CVAL)
          SCINIT = -1
C         let user know which curve won't be plotted
          INUM = 1
          SGRP = 39
          CALL PMXTXI (MESSFL, SCLU, SGRP, MXLINE, SCINIT, WRTFLG,
     I                 INUM, CRVNUM)
        END IF
        ERRFLG = -1
      ELSE
C       have points to plot
C       define PMXTXM variables for next possible screen
        NUMVAR = 4
        VARTYP(1) = 4
        VARTYP(2) = 4
        VARTYP(3) = 1
        VARTYP(4) = 1
        IVAL(2) = CRVNUM
        SGRP = 37
        IF (NULVL1 .GT. 0) THEN
C         some pt.s can't be plotted due to missing attrib. values
          IVAL(1) = NULVL1
          CLEN(1) = ATRLN1
          CLEN(2) = ATRLN2
          CALL CHRCHR (ATRLN1, ATRNM1(1), CVAL(1))
          CALL CHRCHR (ATRLN2, ATRNM2(1), CVAL(ATRLN1+1))
          CALL PMXTXM (MESSFL, SCLU, SGRP, MXLINE, SCINIT, NUMVAR,
     I                 VARTYP, WRTFLG, IVAL, RVAL, DVAL, CLEN, CVAL)
          SCINIT = -1
        END IF
C
        IF (NULVL2 .GT. 0) THEN
C         some pt.s can't be plotted due to missing opt. attrib. values
          IVAL(1) = NULVL2
          IF (OPTLN1.GT.0 .AND. OPTLN2.GT.0) THEN
C           both attrib.s in pair had optional attrib. specified
            CLEN(1) = OPTLN1
            CLEN(2) = OPTLN2
            CALL CHRCHR (OPTLN1, OPTNM1(1), CVAL(1))
            CALL CHRCHR (OPTLN2, OPTNM2(1), CVAL(OPTLN1+1))
            SGRP = 37
          ELSE IF (OPTLN1 .GT. 0) THEN
C           only 1st (y-axis) attrib. had optional attrib. specified
            NUMVAR = 3
            VARTYP(1) = 4
            VARTYP(2) = 1
            VARTYP(3) = 1
            CLEN(1) = OPTLN1
            CALL CHRCHR (OPTLN1, OPTNM1, CVAL)
            SGRP = 38
          ELSE IF (OPTLN2 .GT. 0) THEN
C           only 2nd (x-axis) attrib. had optional attrib. specified
            NUMVAR = 3
            VARTYP(1) = 4
            VARTYP(2) = 1
            VARTYP(3) = 1
            CLEN(1) = OPTLN2
            CALL CHRCHR (OPTLN2, OPTNM2, CVAL)
            SGRP = 38
          END IF
          CALL PMXTXM (MESSFL, SCLU, SGRP, MXLINE, SCINIT, NUMVAR,
     I                 VARTYP, WRTFLG, IVAL, RVAL, DVAL, CLEN, CVAL)
          SCINIT = -1
        END IF
C
        ERRFLG = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETVAL
     I                      (WDMSFL, DATSET, ATRIND, ATRTYP, ATRLEN,
     O                       VAL, ERRCOD)
C
C     + + + PURPOSE + + +
C     This function returns the attribute value corresponding to the
C     attribute index passed in for data set DATSET.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  WDMSFL, DATSET, ATRIND, ATRTYP, ATRLEN, ERRCOD
      REAL     VAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DATSET - data-set number
C     ATRIND - attribute index number
C     ATRTYP - attribute type
C     ATRLEN - attribute length
C     VAL    - attribute value
C     ERRCOD - return code; value assigned w/in external routines WDBSGI or WDBSGR
C              0 - all ok
C             <0 - no attribute value for dsn DATSET
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  IVAL
      REAL     RVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBSGI, WDBSGR
C
C     + + + END SPECIFICATIONS + + +
C
      ERRCOD = 0
C
      IF (ATRTYP .EQ. 1) THEN
C       get integer value
        CALL WDBSGI (WDMSFL, DATSET, ATRIND, ATRLEN,
     O               IVAL, ERRCOD)
        IF (ERRCOD .EQ. 0)  VAL = IVAL
      ELSE IF (ATRTYP .EQ. 2) THEN
C       get real value
        CALL WDBSGR (WDMSFL, DATSET, ATRIND, ATRLEN,
     O               RVAL, ERRCOD)
        IF (ERRCOD .EQ. 0)  VAL = RVAL
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDPINI
     I                   (WDMSFL, LNVAR, DSN, SANAM, PTYPE)
C
C     + + + PURPOSE + + +
C     Set plotting parameters to default values based on plot type.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL, LNVAR,DSN(LNVAR),PTYPE
      CHARACTER*1 SANAM(13,LNVAR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of user's WDM file
C     LNVAR  - for time-series plot or x-y plot of data sets, LNVAR
C              is the number of data sets; for an attribute x-y plot,
C              LNVAR is the number of variables (attributes).
C     DSN    - array of data-set numbers
C     SANAM  - array of attribute names being plotted
C     PTYPE  - type of plot
C              1 - time-series plot of one or more data sets
C              2 - x-y plot of pairs of time-series data sets
C              3 - column plot of pairs of columns in table data sets
C              4 - attribute plot of pairs of attributes of many data sets
C
C     + + + PARAMETERS + + +
      INCLUDE 'ptsmax.inc'
      INCLUDE 'pbfmax.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I, I0, I1, I4, I13, I20, I80, J, K, LEN, OLEN, IFLG1,
     $             IFLG2, IFLG3, IVAR, STLEN, REST, YNAMLN, XNAMLN,
     $             BASPOS, YDSNLN, XDSNLN, ADSNLN, LOC, TYPIND, TYPLEN,
     $             TYPRET, NUMAUX, ANEXT, YNEXT, XNEXT, NCRV, NVAR,
     $             BUFPOS(2,TSMAX), SYMBL(TSMAX), LNTYP(TSMAX),
     $             CTYPE(TSMAX), COLOR(TSMAX), PATTRN(TSMAX),
     $             TRANSF(TSMAX), WHICH(TSMAX), WCHVR(3,TSMAX),
     $              XTYPE, YTYPE(2), BVALFG(4), TICS(4), IV
      REAL         SMALL,LARGE,LMIN,LMAX,RMIN,RMAX,AMIN,AMAX,XMIN,XMAX,
     $             ALEN, YX(BUFMAX), PLMN(4), PLMX(4),
     $             YMIN(TSMAX), YMAX(TSMAX)
      CHARACTER*1  BLNK, COMMA, TSTYP(4), LBC(20,TSMAX),LBV(20,TSMAX),
     $             TITL(240), YLABL(80), YXLABL(80), YALABL(80),
     $             XLABL(80)
      CHARACTER*20 CLAB(2)
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   SCALIT, WDBSGC
      EXTERNAL   LENSTR, ZIPC, ZIPI, ZIPR, INTCHR, CVARAR, CHRCHR
      EXTERNAL   GGNCRV, GGDATX, GGTRAN, GPLABL, GPLBXB
      EXTERNAL   GPVAR,  GPCURV, GPSCLE, GPWCXS, GGWCXY
      EXTERNAL   GGATXB, GGATYL, GGATYR, GGBVFG, GGCTYP, GGVWCH
      EXTERNAL   GGLNTY, GGSYMB, GGPATN, GGCOLR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CLAB/'Data set            ',
     1          'Data sets           '/
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      I1   = 1
      I4   = 4
      I13  = 13
      I20  = 20
      I80  = 80
      LEN  = 5
      SMALL= -1.0E10
      LARGE= 1.0E10
      COMMA = ','
      TYPIND = 1
      TYPLEN = 4
C     get number of curves and variables
      CALL GGNCRV ( NCRV, NVAR )
C
C     initialize plot text
      BLNK = ' '
      I = 360
      CALL ZIPC (I, BLNK, LBC)
      CALL ZIPC (I, BLNK, LBV)
      I = 240
      CALL ZIPC (I, BLNK, TITL)
      CALL ZIPC (I80, BLNK, YLABL)
      CALL ZIPC (I80, BLNK, YXLABL)
      CALL ZIPC (I80, BLNK, YALABL)
      CALL ZIPC (I80, BLNK, XLABL)
C
      I= 4
      CALL ZIPR (I,SMALL,PLMX)
      CALL ZIPR (I,LARGE,PLMN)
C
C     get current values
      CALL GGATXB ( XTYPE )
      CALL GGATYL ( YTYPE(1) )
      CALL GGATYR ( YTYPE(2) )
      CALL GGBVFG ( BVALFG )
      I = 1
      CALL GGCTYP ( I, NCRV, CTYPE )
      CALL GGVWCH ( I, NVAR, WHICH )
      CALL GGTRAN ( I, NVAR, TRANSF )
      DO 5 I = 1, NCRV
        CALL GGLNTY ( I, LNTYP(I) )
        CALL GGSYMB ( I, SYMBL(I) )
        CALL GGPATN ( I, PATTRN(I) )
        CALL GGCOLR ( I, COLOR(I) )
 5    CONTINUE
C
      IF (PTYPE.EQ.1) THEN
C       time-series plot
        CALL ZIPI (NCRV,I0,SYMBL)
        XTYPE= 0
      ELSE
C       X-Y plot
        CALL ZIPI (NCRV,I0,LNTYP)
        IF (XTYPE.LT.0) THEN
C         set to valid value
          XTYPE= 1
        END IF
      END IF
      IF (YTYPE(1).LT.0) THEN
C       set to valid value
        YTYPE(1)= 1
      END IF
      IF (YTYPE(2).LT.0 .OR. NCRV.EQ.1) THEN
C       set to default value (no right axis)
        YTYPE(2)= 0
      END IF
C
C     init data ranges: left, right, aux, and x
      LMIN = 1.0E30
      LMAX = -1.0E30
      RMIN = 1.0E30
      RMAX = -1.0E30
      AMIN = 1.0E30
      AMAX = -1.0E30
      XMIN = 1.0E30
      XMAX = -1.0E30
      IFLG1= 0
      IFLG2= 0
      IFLG3= 0
      I    = BUFMAX
      CALL GGDATX ( I, BUFPOS, YX )
C
      DO 100 I= 1,NCRV
        IF (PTYPE.EQ.1) THEN
C         time-series plot, use lines
          IF (WHICH(I).EQ.0) THEN
C           set to valid value (default to left y-axis)
            WHICH(I)= 1
          END IF
          IF (LNTYP(I).EQ.1) THEN
C           initialized value, set for varying line types
            LNTYP(I)= I
          END IF
C         which axis for each variable
          WCHVR(1,I) = I
C         put data set number in curve label
          CALL CVARAR (I20,CLAB(1),I20,LBC(1,I))
          CALL INTCHR (DSN(I),LEN,I1,
     O                 OLEN,LBC(10,I))
C         put data set number in variable label
          CALL CVARAR (I20,CLAB(1),I20,LBV(1,I))
          CALL INTCHR (DSN(I),LEN,I1,
     O                 OLEN,LBV(10,I))
          IF (CTYPE(I).GE.6) THEN
C           currently set for x-y plot, set to default for time series
            CTYPE(I)= 1
          END IF
          YMIN(I)= 1.0E30
          YMAX(I)= -1.0E30
C         write (99,*) 'B4 10 loop, BUFPOS',BUFPOS(1,I),BUFPOS(2,I)
          DO 10 J= BUFPOS(1,I),BUFPOS(2,I)
            IF (YX(J).GT.YMAX(I)) THEN
C             new max value for this variable
              YMAX(I)= YX(J)
            END IF
            IF (YX(J).LT.YMIN(I)) THEN
C             new min value
              YMIN(I)= YX(J)
            END IF
 10       CONTINUE
          IF (WHICH(I).EQ.1) THEN
C           left axis
            IF (YMAX(I).GT.LMAX) LMAX= YMAX(I)
            IF (YMIN(I).LT.LMIN) LMIN= YMIN(I)
            IFLG1= 1
          ELSE IF (WHICH(I).EQ.2) THEN
C           right axis
            IF (YMAX(I).GT.RMAX) RMAX= YMAX(I)
            IF (YMIN(I).LT.RMIN) RMIN= YMIN(I)
            IFLG2= 1
          ELSE IF (WHICH(I).EQ.3) THEN
C           aux axis
            IF (YMAX(I).GT.AMAX) AMAX= YMAX(I)
            IF (YMIN(I).LT.AMIN) AMIN= YMIN(I)
            IFLG3= 1
          END IF
C
C         define axes labels
          IF (WHICH(I).EQ.1 .OR. WHICH(I).EQ.2) THEN
C           left y- or right y-axis variable, get TSTYPE
            CALL WDBSGC (WDMSFL, DSN(I), TYPIND, TYPLEN,
     O                   TSTYP, TYPRET)
            IF (WHICH(I).EQ.1 .AND. TYPRET.EQ.0) THEN
C             put TSTYPE in left y-axis label
              IF (LENSTR(I80, YLABL) .EQ. 0) THEN
                LOC = 1
              ELSE
                LOC = LENSTR(I80, YLABL) + 1
                YLABL(LOC) = COMMA
                LOC = LOC + 2
              END IF
              CALL CHRCHR (I4, TSTYP, YLABL(LOC))
            END IF
            IF (WHICH(I).EQ.2 .AND. TYPRET.EQ.0) THEN
C             put TSTYPE in right y-axis label
              IF (LENSTR(I80, YXLABL) .EQ. 0) THEN
                LOC = 1
              ELSE
                LOC = LENSTR(I80, YXLABL) + 1
                YXLABL(LOC) = COMMA
                LOC = LOC + 2
              END IF
              CALL CHRCHR (I4, TSTYP, YXLABL(LOC))
            END IF
          ELSE IF (WHICH(I) .EQ. 3) THEN
C           put dsn in aux axis label
            IF (LENSTR(I80, YALABL) .EQ. 0) THEN
C             1st curve, put 'Data set(s)' in label, add dsns
C             determine if there's >1 aux axis curve
              NUMAUX = 0
              DO 15 K = 1,NCRV
                IF (WHICH(K) .EQ. 3)  NUMAUX = NUMAUX + 1
 15           CONTINUE
              IF (NUMAUX .GT. 1) THEN
                CALL CVARAR (I20, CLAB(2), I80, YALABL)
                BASPOS = 11
              ELSE
                CALL CVARAR (I20, CLAB(1), I80, YALABL)
                BASPOS = 10
              END IF
              CALL INTCHR (DSN(I), LEN, I1, ADSNLN, YALABL(BASPOS))
C             set ANEXT to next available character in YALABL
              ANEXT = BASPOS + ADSNLN
            ELSE
              YALABL(ANEXT) = COMMA
              CALL INTCHR (DSN(I), LEN, I1, ADSNLN, YALABL(ANEXT+2))
              ANEXT = (ANEXT+2) + ADSNLN
            END IF
          END IF
        ELSE
C         X-Y plot, use symbols
          IF (NCRV .GT. 1) THEN
C           more than 1 curve, vary symbol:  . + * O X ? ?
            SYMBL(I)= I
          ELSE
C           one curve, use a symbol you can see: O
            SYMBL(I)= 4
          END IF
          IF (CTYPE(I).LT.6) THEN
C           currently set for time series, set to standard X-Y
            CTYPE(I)= 6
          END IF
          IVAR= 2*(I-1)+ 1
C         define curve label
          IF (PTYPE.EQ.2) THEN
C           put pair of data set numbers in label
            CALL CVARAR (I20,CLAB(2),I20,LBC(1,I))
            CALL INTCHR (DSN(IVAR),LEN,I1,
     O                   OLEN,LBC(11,I))
            CALL INTCHR (DSN(IVAR+1),LEN,I1,
     O                   OLEN,LBC(12+OLEN,I))
C           put first data set number in variable label
            CALL CVARAR (I20,CLAB(1),I20,LBV(1,IVAR))
            CALL INTCHR (DSN(I),LEN,I1,
     O                   OLEN,LBV(10,IVAR))
C           put second data set number in variable label
            CALL CVARAR (I20,CLAB(1),I20,LBV(1,IVAR+1))
            CALL INTCHR (DSN(IVAR+1),LEN,I1,
     O                   OLEN,LBV(10,IVAR+1))
C           put data-set numbers in axes labels
            IF (I .EQ. 1) THEN
C             1st curve, put 'Data set(s)' in label, add dsns
              IF (NCRV .GT. 1) THEN
                CALL CVARAR (I20, CLAB(2), I80, YLABL)
                CALL CVARAR (I20, CLAB(2), I80, XLABL)
                BASPOS = 11
              ELSE
                CALL CVARAR (I20, CLAB(1), I80, YLABL)
                CALL CVARAR (I20, CLAB(1), I80, XLABL)
                BASPOS = 10
              END IF
              CALL INTCHR (DSN(IVAR), LEN, I1, YDSNLN, YLABL(BASPOS))
C             set YNEXT to next available character in YLABL
              YNEXT = BASPOS + YDSNLN
              CALL INTCHR (DSN(IVAR+1), LEN, I1, XDSNLN, XLABL(BASPOS))
C             set XNEXT to next available character in XLABL
              XNEXT = BASPOS + XDSNLN
            ELSE
              YLABL(YNEXT) = COMMA
              CALL INTCHR (DSN(IVAR), LEN, I1, YDSNLN, YLABL(YNEXT+2))
              YNEXT = (YNEXT+2) + YDSNLN
              XLABL(XNEXT) = COMMA
              CALL INTCHR (DSN(IVAR+1), LEN, I1,
     O                     XDSNLN, XLABL(XNEXT+2))
              XNEXT = (XNEXT+2) + XDSNLN
            END IF
          ELSE IF (PTYPE.EQ.4) THEN
C           put attribute names in label
            STLEN = LENSTR(I13, SANAM(1,IVAR))
            CALL CHRCHR (STLEN, SANAM(1,IVAR), LBC(1,I))
            REST = 20 - STLEN
            IF (REST .GT. 14)  REST = 14
            CALL CHRCHR (REST-1, SANAM(1,IVAR+1), LBC(STLEN+2,I))
C           put first attribute name in variable label
            CALL CHRCHR (I13,SANAM(1,IVAR),LBV(1,IVAR))
C           put second attribute name in variable label
            CALL CHRCHR (I13,SANAM(1,IVAR+1),LBV(1,IVAR+1))
C           put attribute names in axes labels
            IF (I .EQ. 1) THEN
C             1st curve, don't need to precede name with comma
              YNAMLN = LENSTR(I13, SANAM(1,IVAR))
              CALL CHRCHR (YNAMLN, SANAM(1,IVAR), YLABL)
              XNAMLN = LENSTR(I13, SANAM(1,IVAR+1))
              CALL CHRCHR (XNAMLN, SANAM(1,IVAR+1), XLABL)
            ELSE
              YLABL(YNAMLN+1) = COMMA
              CALL CHRCHR (I13, SANAM(1,IVAR), YLABL(YNAMLN+3))
C             redefine YNAMLN so know where to write next attr. name
              YNAMLN = LENSTR(I80, YLABL)
              XLABL(XNAMLN+1) = COMMA
              CALL CHRCHR (I13, SANAM(1,IVAR+1), XLABL(XNAMLN+3))
C             redefine XNAMLN so know where to write next attr. name
              XNAMLN = LENSTR(I80, XLABL)
            END IF
          END IF
C
          IF (PTYPE .EQ. 4) THEN
C           get values from common set in WDATRB
            CALL GGWCXY ( I, WCHVR(1,I), WCHVR(2,I) )
          ELSE
C           set values
            WHICH(IVAR)  = 1
            WHICH(IVAR+1)= 4
            WCHVR(1,I)= IVAR
            WCHVR(2,I)= IVAR + 1
          END IF
          YMIN(IVAR)= 1.0E30
          YMAX(IVAR)= -1.0E30
C         write (99,*) 'B4 20 loop, BUFPOS',BUFPOS(1,I),BUFPOS(2,I)
          IV = WCHVR(1,I)
          DO 20 J= BUFPOS(1,IV),BUFPOS(2,IV)
            IF (YX(J).GT.YMAX(IVAR)) THEN
              YMAX(IVAR)= YX(J)
C             write (99,*) 'new Y max',YX(J),' at index',J
            END IF
            IF (YX(J).LT.YMIN(IVAR)) THEN
              YMIN(IVAR)= YX(J)
C             write (99,*) 'new Y min',YX(J),' at index',J
            END IF
 20       CONTINUE
          IF (YMAX(IVAR).GT.LMAX) LMAX= YMAX(IVAR)
          IF (YMIN(IVAR).LT.LMIN) LMIN= YMIN(IVAR)
C         set flag to determine default scale for y-axis
          IFLG1= 1
          IVAR = IVAR+ 1
          YMIN(IVAR)= 1.0E30
          YMAX(IVAR)= -1.0E30
C         write (99,*) 'B4 30 loop, BUFPOS',BUFPOS(1,IV),BUFPOS(2,IV)
          IV = WCHVR(2,I)
          DO 30 J= BUFPOS(1,IV),BUFPOS(2,IV)
            IF (YX(J).GT.YMAX(IVAR)) THEN
              YMAX(IVAR)= YX(J)
C             write (99,*) 'new X max',YX(J),' at index',J
            END IF
            IF (YX(J).LT.YMIN(IVAR)) THEN
              YMIN(IVAR)= YX(J)
C             write (99,*) 'new X min',YX(J),' at index',J
            END IF
 30       CONTINUE
          IF (YMAX(IVAR).GT.XMAX) XMAX= YMAX(IVAR)
          IF (YMIN(IVAR).LT.XMIN) XMIN= YMIN(IVAR)
        END IF
 100  CONTINUE
C
C     generate axis scaling
      IF (IFLG1.EQ.1) THEN
C       left y-axis being used
        CALL SCALIT (YTYPE(1),LMIN,LMAX,PLMN(1),PLMX(1))
        IF (PLMX(1)-PLMN(1).LT.1.0E-15) THEN
C         too small a difference
          PLMX(1)= PLMX(1)+ 1.0E-15
        END IF
      END IF
      IF (IFLG2.EQ.1) THEN
C       right y-axis being used
        CALL SCALIT (YTYPE(2),RMIN,RMAX,PLMN(2),PLMX(2))
        IF (PLMX(2)-PLMN(2).LT.1.0E-15) THEN
C         too small a difference
          PLMX(2)= PLMX(2)+ 1.0E-15
        END IF
      END IF
      IF (IFLG3.EQ.1) THEN
C       aux axis being used
        CALL SCALIT (I1,AMIN,AMAX,PLMN(3),PLMX(3))
        IF (PLMX(3)-PLMN(3).LT.1.0E-15) THEN
C         too small a difference
          PLMX(3)= PLMX(3)+ 1.0E-15
        END IF
      END IF
      IF (PTYPE.GT.1) THEN
C       x-axis being used
        CALL SCALIT (XTYPE,XMIN,XMAX,PLMN(4),PLMX(4))
        IF (PLMX(4)-PLMN(4).LT.1.0E-15) THEN
C         too small a difference
          PLMX(4)= PLMX(4)+ 1.0E-15
        END IF
      END IF
C
C     get defaults that were not set
      CALL GGTRAN ( I1, NVAR, TRANSF )
C     put values in common block
      DO 200 I = 1, NCRV
        CALL GPWCXS ( I, WCHVR(1,I), WCHVR(2,I), WCHVR(3,I) )
 200  CONTINUE
      ALEN = 0.0
      CALL GPLABL ( XTYPE, YTYPE, ALEN, YLABL, YXLABL, YALABL, TITL )
      CALL GPLBXB ( XLABL )
      CALL GPVAR  ( YMIN, YMAX, WHICH, TRANSF, LBV )
      CALL GPCURV ( CTYPE, LNTYP, SYMBL, COLOR, PATTRN, LBC )
      I = 10
      CALL ZIPI ( I4, I, TICS )
      TICS(3) = 2
      CALL GPSCLE ( PLMN, PLMX, TICS, BVALFG )
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKATX
     I                   ( MESSFL, SCLU, CURVE,
     M                     INIT, NAME,
     O                     INDX, TYPE, LENGTH )
C
C     + + + PURPOSE + + +
C     Verify, and expand if necessary, the attribute name.  Gets the
C     index number, type, and length of the attribute.  If the name is
C     not a valid attribute or the attribute type is not real or integer,
C     a message is written to the screen and the index number, type, and
C     length are set to 0.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL, SCLU, CURVE, INIT, INDX, TYPE, LENGTH
      CHARACTER*1 NAME(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of the message file
C     SCLU   - cluster number in message file
C     CURVE  - order number of the curve being processed, used in
C              error messages
C     INIT   - screen initialization flag, set to -1 if an error
C              message is written
C               1 - clear screen before message written
C              -1 - don't clear screen, add message to existing screen
C     NAME   - name of search attribute, requires enough characters to
C              uniquely identify the attribute, the complete attribute
C              name will be returned
C     INDX   - the index number of attribute NAME
C              returns as 0 if attribute is not valid
C     TYPE   - type of attribute
C              1 - integer attribute
C              2 - real attribute
C              0 - invalid attribute
C     LENGTH - size of the attribute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ONUM, OTYP(2), IWRT, MXLINE, I6, SGRP, LEN
      REAL      RDUM
      DOUBLE PRECISION DDUM
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  LENSTR, WDBSGX, PMXTXM
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  ONUM, OTYP, IWRT, MXLINE, RDUM, DDUM, I6
     $     /   2,  4,1,   1,    10,    0.0,  0.0,  6 /
C
C     + + + END SPECIFICATIONS + + +
C
C     get full attribute name and information about attribute
      CALL WDBSGX ( MESSFL,
     M              NAME,
     O              INDX, TYPE, LENGTH )
C
      IF (INDX .LT. 0) THEN
C       input value &a for the curve &i is not know as a valid attrib
        SGRP = 40
        LEN = LENSTR ( I6, NAME)
        CALL PMXTXM ( MESSFL, SCLU, SGRP,
     I                MXLINE, INIT, ONUM, OTYP, IWRT,
     I                CURVE, RDUM, DDUM, LEN, NAME )
        INIT = -1
      ELSE IF (TYPE .GT. 2) THEN
C       attrib &a for curve &i cannot be plotted, must be real or integer
        SGRP = 41
        LEN = LENSTR ( I6, NAME)
        CALL PMXTXM ( MESSFL, SCLU, SGRP,
     I                MXLINE, INIT, ONUM, OTYP, IWRT,
     I                CURVE, RDUM, DDUM, LEN, NAME )
        INIT = -1
        INDX = 0
        TYPE = 0
        LENGTH = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MOVTMP
     I                   ( OPTION, TEMPYX, TOTVLS, DSNCNT,
     M                     IPOS,
     O                     YX )
C
C     + + + PURPOSE + + +
C     Move data values from the temporary buffer to the yx array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OPTION, TOTVLS, DSNCNT, IPOS
      REAL      TEMPYX(600), YX(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPTION - indicator flag for math operation
C              0 - no math
C              1 - add 2 attributes together
C              2 - subtract one attribute from another
C              3 - divide one attribute by another
C              4 - multiply 2 attributes together
C     TEMPYX - temporary buffer containig attribute values
C     TOTVLS - number of values to be moved into yx array
C     DSNCNT - number of data sets values retrieved from
C     IPOS   - input as starting location in yx array for data
C              output as next available location in yx array
C     YX     - array of values to be plotted
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N
      REAL      ZERO, BAD
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  ZERO, BAD
     $    / .00000001, -99999. /
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OPTION .EQ. 0) THEN
C       no arithmetic
        DO 100 N = 1, TOTVLS
         YX(IPOS) = TEMPYX(N)
          IPOS = IPOS + 1
 100    CONTINUE
      ELSE IF (OPTION .EQ. 1) THEN
C       add attributes together
        DO 110 N = 1, TOTVLS
          YX(IPOS) = TEMPYX(N) + TEMPYX(DSNCNT+N)
          IPOS = IPOS + 1
 110    CONTINUE
      ELSE IF (OPTION .EQ. 2) THEN
C       subtract one attribute from another
        DO 120 N = 1, TOTVLS
          YX(IPOS) = TEMPYX(N) - TEMPYX(DSNCNT+N)
          IPOS = IPOS + 1
 120    CONTINUE
      ELSE IF (OPTION .EQ. 3) THEN
C       divide one attribute by another
        DO 130 N = 1, TOTVLS
          IF (ABS(TEMPYX(DSNCNT+N)) .GT. ZERO) THEN
            YX(IPOS) = TEMPYX(N) / TEMPYX(DSNCNT+N)
          ELSE
C           can't divide by zero
            YX(IPOS) = BAD
          END IF
          IPOS = IPOS + 1
 130    CONTINUE
      ELSE IF (OPTION .EQ. 4) THEN
C       multiply attributes
        DO 140 N = 1, TOTVLS
          YX(IPOS) = TEMPYX(N) * TEMPYX(DSNCNT+N)
          IPOS = IPOS + 1
 140    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE WDPSTX
     I                 ( MESSFL, SCLU, WDMSFL, NVAR,
     M                   DSN, NCRV,
     O                   SDATIM, EDATIM, TS, TU, DTRAN, RETCOD )
C
C     + + + PURPOSE + + +
C     This routine finds the common time step and period of record for
C     each pair of data sets to be plotted.  The user may than modify
C     these values.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMSFL, NVAR, DSN(NVAR), NCRV,
     $          SDATIM(6,*), EDATIM(6,*), TS(*), TU(*), DTRAN(NVAR),
     $          RETCOD
C
C     + + + ARGUMENT DEFINITION + + +
C     MESSFL - Fortran unit number of the message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number of users WDM file
C     NVAR   - number of data sets to use
C     DSN    - buffer of data set numbers
C              comes in with pairs of requested data sets
C              goes out with pairs of valid data sets
C     NCRV   - number of curves to be drawn
C     SDATIM - array of starting dates for each curve
C     EDATIM - array of ending dates for each curve
C     TS     - array of time steps, in TU units, for each curve
C     TU     - array of time units for each curve
C     DTRAN  - array of type of transformation for each data set
C     RETCOD - return code
C               0 - everything is ok
C              -1 - problem with data sets, return to data set selection
C              -2 - problem with data sets, return to input menu
C
C     + + + PARAMETERS + + +
      INCLUDE 'pwdplt.inc'
      INCLUDE 'pbfmax.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NDSN, IWRT, INIT, MAXL, INUM, RNUM, CNUM, SCNF, LEN,
     $          DSNT(MAXVAR), NV, NC, ND, SGRP, RESP, CMDID, OFF,
     $          SDAT(6,2), EDAT(6,2), TSTEP(2), TCODE(2), ITMP(2),
     $          IVAL(14,MAXCRV), CVAL(3,3,MAXCRV), TOTPTS, NPTS,
     $          AGAIN, RETC, RETC1, RETC2, IRET
      REAL      RVAL(1,1)
      CHARACTER*1 BLNK, BUFF(80,MAXCRV)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDATIM
      EXTERNAL   COPYI,  ZIPI,   ZIPC,  DTMCMN, TIMDIF
      EXTERNAL   PMXTXI, ZMNSST, QRESP, QRESCX, ZSTCMA, ZGTRET
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  NDSN, IWRT, MAXL, INUM, RNUM, CNUM, SCNF, BLNK, CMDID, OFF
     $     /   2,   -1,   10,   14,    1,    3,    1,  ' ',     4,   0 /
C
C     + + + END SPECIFICATIONS + + +
C
      INIT = 1
      NV = 1
      NC = 0
      ND = 1
      CALL COPYI ( NVAR, DSN, DSNT )
      CALL ZIPI ( NVAR, NC, DSN )
 100  CONTINUE
C       get periods of record and time steps for curve variables
        CALL WDATIM ( WDMSFL, DSNT(NV),
     O                SDAT(1,1), EDAT(1,1), TSTEP(1), TCODE(1), RETC1 )
        CALL WDATIM ( WDMSFL, DSNT(NV+1),
     O                SDAT(1,2), EDAT(1,2), TSTEP(2), TCODE(2), RETC2 ) 
        IF (RETC1 .EQ. 0  .AND.  RETC2 .EQ. 0) THEN
C         get common time period and time step
          NC = NC + 1
          CALL DTMCMN ( NDSN, SDAT, EDAT, TSTEP, TCODE,
     O                  IVAL(2,NC), IVAL(8,NC),
     O                  IVAL(14,NC), CVAL(1,1,NC), RETC )
          IF (RETC .EQ. 0) THEN
C           good curve, save data set numbers & initialize transformation
            DSN(ND) = DSNT(NV)
            DSN(ND+1) = DSNT(NV+1)
            CVAL(2,1,NC) = 1
            CVAL(3,1,NC) = 1
            IVAL(1,NC)   = NC
            ND = ND + 2
          ELSE
C           problem with dates or time step for this curve
            IF (RETC .EQ. -1) THEN
C             no common time period
              SGRP = 50
            ELSE IF (RETC .EQ. -2) THEN
C             time steps and units are not compatible
              SGRP = 51
            ELSE
C             unknown problem
              SGRP = 52
            END IF
            CALL PMXTXI ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT,
     I                    NDSN, DSNT(NV) )
            CALL ZMNSST
            INIT = 0
            NC = NC - 1
          END IF
        ELSE
C         problem retrieving dates and/or time steps
          IF (RETC1 .EQ. -6  .OR.  RETC2 .EQ. -6) THEN
C           no data in one of the data sets
            SGRP = 53
          ELSE IF (RETC1 .EQ. -82  .OR.  RETC2 .EQ. -82) THEN
C           at least one of the date sets is not a time series data set
            SGRP = 54
          ELSE
C           unknown problem
            SGRP = 52
          END IF
          CALL PMXTXI ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT,
     I                  NDSN, DSNT(NV) )
          CALL ZMNSST
          INIT = 0
        END IF
C       next pair of data sets
        NV = NV + 2
      IF (NV .LT. NVAR) GO TO 100
      NCRV = NC
C
      RETCOD = 0
      IF (INIT .EQ. 0) THEN
C       problem with at least one curve
        IF (NC .EQ. 0) THEN
C         no valid curves, return to: 1-dsn menu, 2-input
          SGRP = 48
          RESP = 1
        ELSE
C         at least one curve: 1-dsn menu, 2-input, 3-continue
          SGRP = 49
          RESP = 3
        END IF
        CALL QRESP ( MESSFL, SCLU, SGRP, RESP )
        IF (RESP .LT. 3) RETCOD = -RESP
      END IF
C
      IF (RETCOD .EQ. 0) THEN
C       at least one valid curve
        IF (INIT .EQ. 0) THEN
C         previous is ambiguous, turn it off
          CALL ZSTCMA ( CMDID, OFF )
        END IF
        LEN = 80 * NCRV
        CALL ZIPC ( LEN, BLNK, BUFF )
 200    CONTINUE
C         get dates, time step, and transformation
          AGAIN = 0
          SGRP = 59
          CALL QRESCX ( MESSFL, SCLU, SGRP,
     I                  INUM, RNUM, CNUM, NCRV, SCNF,
     M                  IVAL, RVAL, CVAL, BUFF )
          CALL ZGTRET ( IRET )
          IF (IRET .EQ. 1) THEN
C           accepted, get values
            TOTPTS = 0
            NV = 1
            DO 250 NC = 1, NCRV
C             get period of record and time step for each curve
              LEN = 6
              CALL COPYI ( LEN, IVAL(2,NC), SDATIM(1,NC) )
              CALL COPYI ( LEN, IVAL(8,NC), EDATIM(1,NC) )
              TS(NC) = IVAL(14,NC)
              TU(NC) = CVAL(1,1,NC)
C             transformation for each time series
              DTRAN(NV) = CVAL(2,1,NC) - 1
              DTRAN(NV+1) = CVAL(3,1,NC) - 1
C             number of points for each time series
              NV = NV + 2
              CALL TIMDIF ( SDATIM(1,NC), EDATIM(1,NC), TU(NC), TS(NC),
     O                      NPTS )
              TOTPTS = TOTPTS + 2 * NPTS
 250        CONTINUE
            IF (TOTPTS .GT. BUFMAX) THEN
C             Too many data points to plot, current is &, max is &.
              INIT = 1
              LEN  = 2
              ITMP(1) = TOTPTS
              ITMP(2) = BUFMAX
              IF (NCRV .EQ. 1) THEN
C               shorten the time step
                SGRP = 55
              ELSE
C               shorten the time step and/or reduce number of curves
                SGRP = 56
              END IF
              CALL PMXTXI ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT,
     I                      LEN, ITMP )
              CALL ZMNSST
C             return to: 1-select dsn, 2-input, 3-change time
              SGRP = 57
              RESP = 3
              CALL QRESP ( MESSFL, SCLU, SGRP, IRET )
              IF (IRET .EQ. 3) THEN
C               change the time period and/or time step
                AGAIN = 1
              ELSE
C               abandon these curves
                RETCOD = -IRET
              END IF
            END IF
          ELSE
C           assume Previous, back to data set specification screen
            RETCOD= -1
          END IF
        IF (AGAIN .EQ. 1) GO TO 200
      END IF
C
      IF (RETCOD .NE. 0) THEN
C       user chose to return to dsn specification screen or input screen
C       reset number of curves back to zero
        NCRV = 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE WDPSTM
     I                 ( MESSFL, SCLU, WDMSFL, NVAR,
     M                   DSN, NCRV,
     O                   SDATIM, EDATIM, TS, TU, DTRAN, RETCOD )
C
C     + + + PURPOSE + + +
C     This routine finds the common period of record for each data set
C     to be plotted on a time series plot and the time step for each of 
C     the time series.  The user may than modify these values.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, WDMSFL, NVAR, DSN(NVAR), NCRV,
     $          SDATIM(6), EDATIM(6), TS(NVAR), TU(NVAR), DTRAN(NVAR),
     $          RETCOD
C
C     + + + ARGUMENT DEFINITION + + +
C     MESSFL - Fortran unit number of the message file
C     SCLU   - cluster number on message file
C     WDMSFL - Fortran unit number of users WDM file
C     NVAR   - number of data sets to use
C     DSN    - buffer of data set numbers
C              comes in with requested data sets
C              goes out with valid data sets
C     NCRV   - number of curves to be drawn
C     SDATIM - starting date for the curves
C     EDATIM - ending date for the curves
C     TS     - array of time steps, in TU units, for each curve
C     TU     - array of time units for each curve
C     DTRAN  - array of type of transformation for each data set
C     RETCOD - return code
C               0 - everything is ok
C              -1 - problem with data sets, return to data set selection
C              -2 - problem with data sets, return to input menu
C
C     + + + PARAMETERS + + +
      INCLUDE 'pwdplt.inc'
      INCLUDE 'pbfmax.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IWRT, INIT, MAXL, LEN,
     $          DSNT(MAXVAR), NV, NC, RETC, SGRP, RESP, CMDID, OFF,
     $          SDAT(6,MAXCRV), EDAT(6,MAXCRV),
     $          ITMP(MAXCRV), IRET, ITMP1(1),
     $          IVAL(20), CVAL(2,MAXCRV), TOTPTS, NPTS,
     $          AGAIN
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYI,  ZIPI,   DATCMN, TIMDIF, WDATIM
      EXTERNAL   PMXTXI, ZMNSST, QRESP, ZSTCMA
      EXTERNAL   Q1INIT, QSETI, QSETCO
      EXTERNAL   Q1EDIT, QGETI, QGETCO
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  IWRT, MAXL, CMDID, OFF
     $     /  -1,   10,     4,   0 /
C
C     + + + END SPECIFICATIONS + + +
C
      INIT = 1
      NC = 0
      CALL COPYI ( NVAR, DSN, DSNT )
      CALL ZIPI ( NVAR, NC, DSN )
      DO 100 NV = 1, NVAR
C       get period of record and time step for curve
        NC = NC + 1
        CALL WDATIM ( WDMSFL, DSNT(NV),
     O                SDAT(1,NC), EDAT(1,NC), IVAL(NC+12), CVAL(1,NC),
     O                RETC )
        IF (RETC .EQ. 0) THEN
C         good curve, save dsn and initialize transformation
          DSN(NC) = DSNT(NV)
          CVAL(2,NC) = 1
        ELSE
C         problem with date or time step for this time series
          IF (RETC .EQ. -6) THEN
C           no data in the data set
            SGRP = 60
          ELSE IF (RETC .EQ. -82) THEN
C           the date set is not a time series data set
            SGRP = 61
          ELSE
C           unknown problem
            SGRP = 62
          END IF
          LEN = 1
          CALL PMXTXI ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT,
     I                  LEN, DSNT(NV) )
          CALL ZMNSST
          INIT = 0
          NC = NC - 1
        END IF
 100  CONTINUE
C
      NCRV = NC
      IF (NCRV .GT. 1) THEN
C       at least one curve to plot, get common period
        CALL DATCMN ( NCRV, SDAT, EDAT, 
     O                IVAL(1), IVAL(7), RETC )
        IF (RETC .NE. 0) THEN
C         no common period for the NC curves
          SGRP = 63
          LEN = 1
          ITMP1(1) = NCRV
          CALL PMXTXI ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT,
     I                  LEN, ITMP1 )
          CALL ZMNSST
          INIT = 0
          NCRV = 0
        END IF
      ELSE IF (NCRV .EQ. 1) THEN
C       only one curve, set period of record
        LEN = 6
        CALL COPYI ( LEN, SDAT, IVAL(1) )
        CALL COPYI ( LEN, EDAT, IVAL(7) )
      END IF
C
      RETCOD = 0
      IF (INIT .EQ. 0) THEN
C       some problem with at least one data set
        IF (NCRV .EQ. 0) THEN
C         no valid curves, return to: 1-dsn menu, 2-input
          SGRP = 48
          RESP = 1
        ELSE
C         at least one curve: 1-dsn menu, 2-input, 3-continue
          SGRP = 49
          RESP = 3
        END IF
        CALL QRESP ( MESSFL, SCLU, SGRP, RESP )
        IF (RESP .LT. 3) RETCOD = -RESP
      END IF
C
      IF (RETCOD .EQ. 0) THEN
C       at least one valid curve
        IF (INIT .EQ. 0) THEN
C         previous is ambiguous, turn it off
          CALL ZSTCMA ( CMDID, OFF)
        END IF
 200    CONTINUE
C         modify date, time steps, and transformations
          IF (NCRV .LE. 6) THEN
C           date, time steps, and tranformations on one screen
 210        CONTINUE
              AGAIN = 0
              SGRP = 70 + NCRV
              CALL Q1INIT ( MESSFL, SCLU, SGRP )
C             set dates and time step
              LEN = 12 + NCRV
              CALL QSETI ( LEN, IVAL )
C             set time unit and transformation
              LEN = 2 * NCRV
              CALL QSETCO ( LEN, CVAL )
              CALL Q1EDIT ( IRET )
              IF (IRET .EQ. 1) THEN
C               accepted, get values
                LEN = 12 + NCRV
                CALL QGETI ( LEN, IVAL )
C               time units and transformatin
                LEN = 2 * NCRV
                CALL QGETCO ( LEN, CVAL )
              ELSE IF (IRET .EQ. -1) THEN
C               oops, try again
                AGAIN = 1
              ELSE
C               assume previous
                RETCOD = -1
              END IF
            IF (AGAIN .EQ. 1) GO TO 210
          ELSE
C           date on one screen, time steps and transformations on second
 220        CONTINUE
 225          CONTINUE
                AGAIN = 0
                SGRP = 70
                CALL Q1INIT ( MESSFL, SCLU, SGRP )
C               set dates
                LEN = 12
                CALL QSETI ( LEN, IVAL )
                CALL Q1EDIT ( IRET )
                IF (IRET .EQ. 1) THEN
C                 accepted, get values
                  LEN = 12
                  CALL QGETI ( LEN, IVAL )
                ELSE IF (IRET .EQ. -1) THEN
C                 oops, try again
                  AGAIN = 1
                ELSE
C                 assume previous
                  RETCOD = -1
                END IF
              IF (AGAIN .EQ. 1) GO TO 225
              IF (RETCOD .EQ. 0) THEN
C               time steps and transformations
 230            CONTINUE
                  AGAIN = 0
                  SGRP = 70 + NCRV
                  CALL Q1INIT ( MESSFL, SCLU, SGRP )
C                 set time steps and transformation
                  LEN = NCRV
                  CALL QSETI ( LEN, IVAL(13) )
                  LEN = NCRV * 2
                  CALL QSETCO ( LEN, CVAL )
                  CALL Q1EDIT ( IRET )
                  IF (IRET .EQ. 1) THEN
C                   accepted values
                    LEN = NCRV
                    CALL QGETI ( LEN, IVAL(13) )
                    LEN = NCRV * 2
                    CALL QGETCO ( LEN, CVAL )
                  ELSE IF (IRET .EQ. -1) THEN
C                   oops, try again
                    AGAIN = 1
                  ELSE
C                   assume previous, turn off ambiguous previous
                    CALL ZSTCMA ( CMDID, OFF )
                    AGAIN = 2
                  END IF
C                 repeat time steps and transformations?
                IF (AGAIN .EQ. 1) GO TO 230
C               repeat dates and time steps and transfomations?
              IF (AGAIN .EQ. 2) GO TO 220
            END IF
          END IF
C
          IF (RETCOD .EQ. 0) THEN
C           check total number of points to be retrieved
            TOTPTS = 0
            NV = 1
            DO 240 NC = 1, NCRV
C             save time units and data transformation
              CALL TIMDIF ( IVAL(1), IVAL(7), CVAL(1,NC), IVAL(NC+12),
     O                      NPTS )
              TOTPTS = TOTPTS + NPTS
 240        CONTINUE
            IF (TOTPTS .LE. BUFMAX) THEN
C             good set of curves, set time period, time steps, trans
              LEN = 6
              CALL COPYI ( LEN, IVAL(1), SDATIM )
              CALL COPYI ( LEN, IVAL(7), EDATIM )
              CALL COPYI ( NCRV, IVAL(13), TS )
              DO 250 NC = 1, NCRV
                TU(NC) = CVAL(1,NC)
                DTRAN(NC) = CVAL(2,NC) - 1
 250          CONTINUE
            ELSE
C             Too many data points to plot, current is &, max is &.
              INIT = 1
              LEN  = 2
              ITMP(1) = TOTPTS
              ITMP(2) = BUFMAX
              IF (NCRV .EQ. 1) THEN
C               shorten the time step
                SGRP = 55
              ELSE
C               shorten the time step and/or reduce number of curves
                SGRP = 56
              END IF
              CALL PMXTXI ( MESSFL, SCLU, SGRP, MAXL, INIT, IWRT,
     I                      LEN, ITMP )
              CALL ZMNSST
C             return to: 1-select dsn, 2-input, 3-change time
              SGRP = 57
              RESP = 3
              CALL QRESP ( MESSFL, SCLU, SGRP, IRET )
              IF (IRET .EQ. 3) THEN
C               change the time period and/or time step
                AGAIN = 1
              ELSE
C               abandon these curves
                RETCOD = -IRET
              END IF
            END IF
          ELSE IF (IRET .EQ. -1) THEN
C           oops, try again
            AGAIN = 1
          ELSE
C           assume Previous, back to data set specification screen
            RETCOD= -1
          END IF
        IF (AGAIN .EQ. 1) GO TO 200
      END IF
C
      IF (RETCOD .NE. 0) THEN
C       user chose to return to dsn specification screen or input screen
C       reset number of curves back to zero
        NCRV = 0
      END IF
C
      RETURN
      END

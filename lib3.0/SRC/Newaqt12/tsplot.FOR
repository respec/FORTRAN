C
C
C
      SUBROUTINE   SGPLOT
     I                   (MESSFL,WDMSFL,NDSN,ADSN,
     I                    CSCENM,CLOCNM,CCONNM,GRPMAX,
     M                    LNTYP,COLOR,PATTRN,SYMBOL,
     M                    TU,TS,SDATE,EDATE,DTRAN,
     M                    PLTDEV,ARHLOG)
C
C     + + + PURPOSE + + +
C     plot data at requested stations for requested time
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       WDMSFL,MESSFL,NDSN,ADSN(NDSN),
     $              LNTYP(*),COLOR(*),PATTRN(*),SYMBOL(*),
     $              GRPMAX,TU,TS,SDATE(6),EDATE(6),
     $              PLTDEV,ARHLOG(2),DTRAN
      CHARACTER*8   CSCENM(NDSN),CLOCNM(NDSN),CCONNM(NDSN)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     WDMSFL - Fortran unit number for daily data file
C     MESSFL - Fortran unit number for message file
C     NDSN   - Number of datasets to plot
C     ADSN   - Datasets to plot
C     CSCENM - Scenario name assoc with data in dataset
C     CLOCNM - Location name assoc with data in dataset
C     CCONNM - Constituent name assoc with data in dataset
C     GRPMAX - Max group size
C     LNTYP  - line type for plot
C     COLOR  - color of line on plot
C     PATTRN - fill pattern for plot
C     SYMBOL - symbol type for plot
C     TU     - Default timeunits
C     TS     - Default timestep
C     SDATE  - Default start date
C     EDATE  - Default end date
C     ARHLOG - arith or log scale
C     PLTDEV - plotting device
C     DTRAN  - time series transformation flag
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmaxd.inc'
      INCLUDE 'pbfmax.inc'
      INCLUDE 'ptsmax.inc'
      INTEGER MXSTRM
      PARAMETER (MXSTRM=50)
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxwin.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cplotb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       QFLAG,WHICH(12),DTYPE(12),ISUB,CSTRM,PREVFG,
     $              I,I1,I0,I4,K,DEVCOD,SCLU,SGRP,RESP,L,TSYMBL(1),
     $              RETCOD,NPTS,IRET,IVAL2(6,MXSTRM),TLNTYP(1),
     $              CVAL(6,3,MAXD),IVAL,WINACT,LTS,LTU,I2,I7,I8,I6,
     $              CNUM,TYPIND(MAXD),LPLT,CMPTYP,DEVFIL,PLTCNT,LINCNT,
     $              MNLTZO,JTMP,MSEL(3),ILEN,PROBFG,
     $              CNTCON,CNTLOC,CNTSEN,PLTTYP,ITMP2(2),
     $              TTYP,TSYM,TLNT,TCOL,TPAT,TWHI,TDTY,LPAT(MAXD),
     $              STSTRM(6,MXSTRM),ENSTRM(6,MXSTRM),TCLU,ACT,
     $              SSDATE(2),SEDATE(2),REGLIN,STORMS,OPVAL(3),
     $              NVALS,I20,LEN1,LEN2,IPOS,STRMFG,
     $              CVAL2(1,MXSTRM),NEWDT,CLENG(2),IM1,PEKVOL(1),
     $              ISTRMM(1),INDX(BUFMAX),START(6),ENDDT(6)
      INTEGER       WIPEIT(MXWIN),MAPWIN,PLTWIN,WTYPE(MXWIN),NWIN
      REAL          WINDIM(4,MXWIN)
      REAL          R0,LL(2),RVAL,ARPCSC(1),TMPYX(BUFMAX),
     $              TRSTR1(MXSTRM),TRSTR2(MXSTRM),RSTRMP(1),
     $              ACOEF,BCOEF,RSQUAR,RVAL2(4,MXSTRM),
     $              STRMP1(MXSTRM),STRMP2(MXSTRM),AVSTV2(MXSTRM),
     $              AVSTV1(MXSTRM),RTEMP(1)
      CHARACTER*1   CLAB1(20),
     $              CLAB2(20),CVAL1(40)
      CHARACTER*7   CLINE,CCOLOR,CPATRN,CSYMBL,TMPSTR
      CHARACTER*8   PTHNAM(1),CDSID,CTRAN,CTUNIT
      CHARACTER*20  CLAB(MAXD),TCLA
      CHARACTER*48  WNDNAM
      CHARACTER*80  TBUF80(MAXD),XLAB,YRLAB,YLLAB,ALAB,TPXLAB,TPYLAB
      CHARACTER*240 TITL,TPTITL
C
C     + + + EQUIVALENCES + + +
      CHARACTER*1  TBUFF(80,MAXD),TMPST1(7),TPTTL1(240)
      EQUIVALENCE (TBUF80,TBUFF),(TMPSTR,TMPST1),(TPTITL,TPTTL1)
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     QDTMPL,PRNTXI,QRESP,TIMDIF,LENSTR,ZIPR,QDBRPL,COPYR
      EXTERNAL     ZSTCMA,Q1INIT,QSETI,QSETR,QSETOP,CMSTRM,WSWLOC
      EXTERNAL     QGETR,QGETOP,ZIPI,PDNPLT,CVARAR,TSBTIM,TSBWDS,TSBGET
      EXTERNAL     Q1EDIT,QGETI,PRNTXT,ZGTRET,PLTXXX,DTPUT,STFRPL
      EXTERNAL     QRESCX,GPLEDG,QDFDPL,PMXCNW,GGTGLV,QDXYPT,ZWNSET
      EXTERNAL     SGDATE,DTACT,DTGET,FITLIN,QDXYRG,CHRCHR,MATSTM
      EXTERNAL     Q2INIT,Q2EDIT,Q2SETI,Q2SETR,Q2STCO,Q2GTCO,DTADD
      EXTERNAL     ZMNSST,PMXTXA,QSETCO,QGETCO,SGLABL,GETWIN,ANPRGT
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (A20,4A7)
C
C     + + + END SPECIFICATIONS + + +
C
      IM1   = -1
      I0    = 0
      I1    = 1
      I2    = 2
      I4    = 4
      I6    = 6
      I7    = 7
      I8    = 8
      I20   = 20
      R0    = 0.0
      DEVFIL= 0
      QFLAG = 31
      PLTTYP= 1
C     get window dimensions
      CALL GETWIN (MXWIN,
     O             WIPEIT,MAPWIN,PLTWIN,WTYPE,NWIN,WINDIM)
C
      CALL ZIPI (MAXD,I1,DTYPE)
      WRITE(99,*) TU,TS,GRPMAX
      WRITE(99,*) SDATE
      WRITE(99,*) EDATE
C
      ARPCSC(1)= 100.0
C     do routine to make labels for plots
      CALL SGLABL (NDSN,CSCENM,CLOCNM,CCONNM,TU,DTRAN,
     M             WHICH,TYPIND,
     O             CNTCON,CNTSEN,CNTLOC,
     O             ALAB,YRLAB,YLLAB,TITL,CLAB,CTRAN,CTUNIT)
C     set default for regression line and no storms
      REGLIN = 1
      STORMS = 2
      PEKVOL(1) = 1
      RSTRMP(1) = 99.0
      ISTRMM(1) = 50
C
      SCLU  = 71
      RESP  = 1
C
 11   CONTINUE
C       plot option loop
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        GO TO (15,20,30,40,45,50,60,70), RESP
C
 15     CONTINUE
C         plot device
          SGRP= 11
          CALL QRESP(MESSFL,SCLU,SGRP,
     M               PLTDEV)
          GO TO 90
C
 20     CONTINUE
C         time units and span
          PTHNAM(1) = 'AG'
          TCLU = 64
          CALL SGDATE (MESSFL,TCLU,PTHNAM)
C         get dates for current active date set in case they changed
          CALL DTACT (I)
          CALL DTGET (I,
     O                ACT,CDSID,SDATE,EDATE,SSDATE,SEDATE,TU,
     O                TS,DTRAN)
C         update labels for plots
          CALL SGLABL (NDSN,CSCENM,CLOCNM,CCONNM,TU,DTRAN,
     M                 WHICH,TYPIND,
     O                 CNTCON,CNTSEN,CNTLOC,
     O                 ALAB,YRLAB,YLLAB,TITL,CLAB,CTRAN,CTUNIT)
          GO TO 90
C
 30     CONTINUE
C         plot specs
          SGRP= 31
C
          DO 300 I = 1,NDSN
C           increment curve variables by 1 to account for 'none' option
            LNTYP(I)  = LNTYP(I)  + 1
            COLOR(I)  = COLOR(I) + 1
            PATTRN(I) = PATTRN(I) + 1
            SYMBOL(I) = SYMBOL(I) + 1
 300      CONTINUE
C
C         make previous available
          I= 4
          CALL ZSTCMA (I,I1)
C
          CNUM= 6
C         initialize character buffer
          CALL ZIPI (18*NDSN,I0,CVAL)
          DO 310 I= 1,NDSN
            TMPSTR= '?LINETY'
            CALL GGTGLV(I7,LNTYP(I),TMPST1,ILEN)
            CLINE = TMPSTR
            TMPSTR= '?COLOR'
            CALL GGTGLV(I7,COLOR(I),TMPST1,ILEN)
            CCOLOR= TMPSTR
            TMPSTR= '?FILL'
            CALL GGTGLV(I7,PATTRN(I),TMPST1,ILEN)
            CPATRN= TMPSTR
            TMPSTR= '?SYMBOL'
            CALL GGTGLV(I7,SYMBOL(I),TMPST1,ILEN)
            CSYMBL= TMPSTR
            WRITE(TBUF80(I),2000) CLAB(I),CLINE,CCOLOR,CPATRN,CSYMBL
            CVAL(2,1,I)= LNTYP(I)
            CVAL(3,1,I)= COLOR(I)
            CVAL(4,1,I)= PATTRN(I)
            CVAL(5,1,I)= SYMBOL(I)
            CVAL(6,1,I)= WHICH(I)
 310      CONTINUE
          CALL QRESCX (MESSFL,SCLU,SGRP,I1,I1,CNUM,NDSN,I1,
     M                 IVAL,RVAL,CVAL,TBUFF)
C         how did user exit
          CALL ZGTRET(I)
          IF (I .EQ. 1) THEN
C           with accept
            DO 320 I= 1,NDSN
              LNTYP(I) = CVAL(2,1,I)
              COLOR(I) = CVAL(3,1,I)
              PATTRN(I)= CVAL(4,1,I)
              SYMBOL(I)= CVAL(5,1,I)
              WHICH(I) = CVAL(6,1,I)
              IF (WHICH(I).EQ.3) THEN
                ALAB = CCONNM(I)
              ELSE IF (WHICH(I).EQ.2) THEN
                YRLAB = CCONNM(I)
              END IF
 320        CONTINUE
          END IF
C
          DO 330 I = 1,NDSN
C           decrement curve variables by 1 to account for 'none' option
            LNTYP(I)  = LNTYP(I)  - 1
            COLOR(I)  = COLOR(I)  - 1
            PATTRN(I) = PATTRN(I) - 1
            SYMBOL(I) = SYMBOL(I) - 1
 330      CONTINUE
C
C         make previous unavailable
          I= 4
          CALL ZSTCMA (I,I0)
          GO TO 90
C
 40     CONTINUE
C         set current window
C         what type computer
          CALL ANPRGT(I1,CMPTYP)
C         do window screen
          PTHNAM(1) = 'AG'
          IF (CMPTYP .NE. 1) THEN
C           not a pc, go ahead
            CALL WSWLOC (MESSFL,PTHNAM)
C           get current map window specs
            CALL GETWIN (MXWIN,
     O                   WIPEIT,MAPWIN,PLTWIN,WTYPE,NWIN,WINDIM)
          ELSE
C           pc, multiple windows not supported
C           set prefix to window names
            WNDNAM= 'Windows (AGW)'
            CALL ZWNSET (WNDNAM)
            SGRP = 57
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
          GO TO 90
C
 45     CONTINUE
C         edit axis type and scale for plot
C         make previous command available
          CALL ZSTCMA (I4,I1)
          SGRP= 61
          CALL Q1INIT(MESSFL,SCLU,SGRP)
C         percent scale
          CALL QSETR (I1,ARPCSC)
C         axis type
          MSEL(1)= 1
          MSEL(2)= 1
          OPVAL(1)= ARHLOG(1)
          OPVAL(2)= ARHLOG(2)
          CALL QSETOP(I2,I2,MSEL,MSEL,OPVAL)
C         let user make changes
          CALL Q1EDIT
     O               (IRET)
          IF (IRET .EQ. 1) THEN
C           get users changes
            CALL QGETR (I1,
     O                  ARPCSC)
            CALL QGETOP(I2,
     O                  OPVAL)
            ARHLOG(1) = OPVAL(1)
            ARHLOG(2) = OPVAL(2)
C           WRITE(99,*) 'edit ARHLOG,ARPCSC:',ARPCSC(1),ARHLOG(1)
          END IF
C         turn off previous command
          CALL ZSTCMA (I4,I0)
          GO TO 90
C
 50     CONTINUE
C         which type of plot?
 55       CONTINUE
            SGRP= 71
            PREVFG = 0
C           make previous command available
            CALL ZSTCMA (I4,I1)
            IVAL = PLTTYP
            CALL QRESP (MESSFL,SCLU,SGRP,IVAL)
C           turn off previous command
            CALL ZSTCMA (I4,I0)
            CALL ZGTRET (IRET)
            IF (IRET.EQ.1) THEN
C             user wants to continue
              PLTTYP = IVAL
              IF (PLTTYP.EQ.7) THEN
C               need to do screen to see if regression line wanted
C               storm or specified time units, peaks or volumes
C               make previous command available
                CALL ZSTCMA (I4,I1)
                SGRP= 72
                CALL Q1INIT(MESSFL,SCLU,SGRP)
C               regression line?
                MSEL(1) = 1
                MSEL(2) = 1
                OPVAL(1)= REGLIN
                OPVAL(2)= STORMS
                CALL QSETOP(I2,I2,MSEL,MSEL,OPVAL)
                CALL QSETR (I1,RSTRMP)
                CALL QSETI (I1,ISTRMM)
                CALL QSETCO (I1,PEKVOL)
C               let user make changes
                CALL Q1EDIT
     O                     (IRET)
                IF (IRET .EQ. 1) THEN
C                 get users changes
                  CALL QGETOP(I2,
     O                        OPVAL)
                  REGLIN = OPVAL(1)
                  STORMS = OPVAL(2)
                  CALL QGETR (I1,
     O                        RSTRMP)
                  CALL QGETI (I1,
     O                        ISTRMM)
                  CALL QGETCO (I1,
     O                         PEKVOL)
                ELSE IF (IRET.EQ.2) THEN
C                 user wants previous
                  PREVFG = 1
                END IF
C               turn off previous command
                CALL ZSTCMA (I4,I0)
              END IF
            END IF
          IF (PREVFG.EQ.1) GO TO 55
          GO TO 90
C
 60     CONTINUE
C         produce the plot
          IF (WIPEIT(PLTWIN).EQ.1 .AND. PLTDEV.EQ.1) THEN
C           window size has been modified, window needs to disappear
            CALL PDNPLT (PLTWIN,I1,I0)
            WIPEIT(PLTWIN) = 0
          END IF
C         device details
          CALL PLTXXX (PLTDEV,PLTWIN,WINDIM(1,PLTWIN),
     M                 DEVFIL,
     O                 DEVCOD,CMPTYP,WINACT,PLTCNT)
C         retrieving data message
          SGRP = 40
          CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,LINCNT)
          IF (PLTTYP.NE.5) THEN
C           want timeseries or xy plot, continue
            LTS= TS
            LTU= TU
          ELSE
C           want flow/duration plot, how many points?
            LTU = 4
            LTS = 1
          END IF
C         how many points
          WRITE(99,*) 'pl start:',SDATE,LTU,LTS
          WRITE(99,*) '     end:',EDATE
          CALL TIMDIF (SDATE,EDATE,LTU,LTS,
     O                 NPTS)
          WRITE(99,*) '    npts:',NPTS
C         set time unit parameters for plotting
          CALL TSBTIM (LTU,LTS,DTRAN,QFLAG)
          IF (NPTS.LE.0) THEN
C           no points to plot, tell user
            SGRP= 47
            CALL PRNTXT (MESSFL,SCLU,SGRP)
            RETCOD = 1
          ELSE
C           we have some points, continue
            DO 510 K= 1,NDSN
C             get data
              JTMP= (K-1)* NPTS+ 1
              IF (JTMP+NPTS .GT. BUFMAX) THEN
C               oops,will blow out buffer
                WRITE(99,*) 'too many points to plot'
                SGRP= 46
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                RETCOD = 1
              ELSE
C               get data for plot
C               WRITE(99,*) 'b4g data:',I,K,JTMP,ADSN(K)
C               WRITE(99,*) '         ',YX(JTMP),YX(JTMP+NPTS-1)
C               WRITE(99,*) '         ',SDATE
C               CALL WDTGET (WDMSFL,ADSN(K),LTS,SDATE,
C    I                       NPTS,DTRAN,QFLAG,LTU,
C    O                       YX(JTMP),RETCOD)
C               WRITE(99,*) 'aft data:',RETCOD
C               WRITE(99,*) '         ',YX(JTMP),YX(JTMP+NPTS-1)
                CALL TSBWDS (WDMSFL,ADSN(K))
                CALL TSBGET (SDATE,NPTS,
     O                       YX(JTMP),RETCOD)
                IF (RETCOD .LT. 0) THEN
C                 could not retrieve data
                  SGRP= 41
                  CALL PRNTXI (MESSFL,SCLU,SGRP,ADSN(K))
                  RETCOD = 1
                END IF
              END IF
 510        CONTINUE
          END IF
C
          IF (RETCOD .EQ. 0) THEN
C           continue with plot, set title
C           set legend location upper left
            LL(1)= -1.0
            LL(2)= -1.0
            CALL GPLEDG (LL)
            IF (PLTDEV .GT. 1) THEN
              SGRP = 45
              CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,LINCNT)
            END IF
            IF (PLTTYP .EQ. 1) THEN
C             do standard timeseries plot
              MNLTZO= 0
              CALL QDTMPL(WINACT,ARHLOG,MNLTZO,ARPCSC(1),NDSN,WHICH,
     I                    NPTS,MAXD,BUFMAX,YX,SDATE,LTS,LTU,DTYPE,
     I                    TYPIND,LNTYP,COLOR,PATTRN,SYMBOL,
     I                    CLAB,YLLAB,YRLAB,ALAB,TITL,CMPTYP)
            ELSE IF (PLTTYP .EQ. 2) THEN
C             do residual timeseries plot
              IF (NDSN.GT.1) THEN
C               need 2 data sets for this type of plot
                MNLTZO= 3
C               data needs to be calculated
                DO 520 K = 1,NPTS
                  TMPYX(K) = YX(K+NPTS) - YX(K)
 520            CONTINUE
C               build title for this plot
                CALL CVARAR (I20,CLAB(1),I20,CLAB1)
                CALL CVARAR (I20,CLAB(2),I20,CLAB2)
                LEN1  = LENSTR(I20,CLAB1)
                LEN2  = LENSTR(I20,CLAB2)
                TPTITL= 'Residual Time Series Plot ('
                IPOS  = 28
                CALL CHRCHR (LEN2,CLAB2,TPTTL1(IPOS))
                IPOS  = IPOS+LEN2+1
                TPTTL1(IPOS) = '-'
                IPOS  = IPOS+2
                CALL CHRCHR (LEN1,CLAB1,TPTTL1(IPOS))
                IPOS  = IPOS+LEN1
                TPTTL1(IPOS) = ')'
                IPOS  = IPOS+2
C               add tu, dtran
                TPTTL1(IPOS) = 'f'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'o'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'r'
                IPOS = IPOS+2
                CALL CVARAR (I8,CTRAN,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                CALL CVARAR (I8,CTUNIT,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                IF (CNTSEN.EQ.1) THEN
C                 locn and con vary, add scen to title
                  CALL CVARAR (I8,CSCENM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTCON.EQ.1) THEN
C                 scen and loc vary, add constit to title
                  CALL CVARAR (I8,CCONNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTLOC.EQ.1) THEN
C                 scen and con vary, add location to title
                  TPTTL1(IPOS) = 'a'
                  IPOS = IPOS+1
                  TPTTL1(IPOS) = 't'
                  IPOS = IPOS+2
                  CALL CVARAR (I8,CLOCNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
C               don't allow fills or less than 100 percent of scale
                CALL ZIPI (MAXD,I0,LPAT)
                RTEMP(1) = 100.0
                CALL QDTMPL(WINACT,ARHLOG,MNLTZO,RTEMP(1),I1,WHICH,
     I                      NPTS,MAXD,BUFMAX,TMPYX,SDATE,LTS,LTU,DTYPE,
     I                      TYPIND,LNTYP,COLOR,LPAT,SYMBOL,
     I                      CLAB,YLLAB,YRLAB,ALAB,TPTITL,CMPTYP)
              ELSE
C               tell user not enough data sets
                SGRP= 50
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              END IF
            ELSE IF (PLTTYP .EQ. 3) THEN
C             do cumulative differences timeseries plot
              IF (NDSN.GT.1) THEN
C               need 2 data sets for this type of plot
                MNLTZO= 3
C               data needs to be calculated
                TMPYX(1) = YX(1+NPTS) - YX(1)
                DO 530 K = 2,NPTS
                  TMPYX(K) = (YX(K+NPTS) - YX(K)) + TMPYX(K-1)
 530            CONTINUE
C               build title for this plot
                CALL CVARAR (I20,CLAB(1),I20,CLAB1)
                CALL CVARAR (I20,CLAB(2),I20,CLAB2)
                LEN1  = LENSTR(I20,CLAB1)
                LEN2  = LENSTR(I20,CLAB2)
                TPTITL= 'Cumulative Differences Time Series Plot ('
                IPOS  = 42
                CALL CHRCHR (LEN2,CLAB2,TPTTL1(IPOS))
                IPOS  = IPOS+LEN2+1
                TPTTL1(IPOS) = '-'
                IPOS  = IPOS+2
                CALL CHRCHR (LEN1,CLAB1,TPTTL1(IPOS))
                IPOS  = IPOS+LEN1
                TPTTL1(IPOS) = ')'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = '&'
                IPOS  = IPOS+2
C               add tu, dtran
                TPTTL1(IPOS) = 'f'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'o'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'r'
                IPOS = IPOS+2
                CALL CVARAR (I8,CTRAN,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                CALL CVARAR (I8,CTUNIT,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                IF (CNTSEN.EQ.1) THEN
C                 locn and con vary, add scen to title
                  CALL CVARAR (I8,CSCENM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTCON.EQ.1) THEN
C                 scen and loc vary, add constit to title
                  CALL CVARAR (I8,CCONNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTLOC.EQ.1) THEN
C                 scen and con vary, add location to title
                  TPTTL1(IPOS) = 'a'
                  IPOS = IPOS+1
                  TPTTL1(IPOS) = 't'
                  IPOS = IPOS+2
                  CALL CVARAR (I8,CLOCNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
C               don't allow fills
                CALL ZIPI (MAXD,I0,LPAT)
                RTEMP(1) = 100.0
                CALL QDTMPL(WINACT,ARHLOG,MNLTZO,RTEMP(1),I1,WHICH,
     I                      NPTS,MAXD,BUFMAX,TMPYX,SDATE,LTS,LTU,DTYPE,
     I                      TYPIND,LNTYP,COLOR,LPAT,SYMBOL,
     I                      CLAB,YLLAB,YRLAB,ALAB,TPTITL,CMPTYP)
              ELSE
C               tell user not enough data sets
                SGRP= 52
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              END IF
            ELSE IF (PLTTYP .EQ. 4) THEN
C             do bar chart timeseries plot
              MNLTZO= 0
C             force arithmetic
              ITMP2(1) = 1
              ITMP2(2) = 1
C             initialize temp buffer
              CALL ZIPR (BUFMAX,R0,TMPYX)
C             data needs to be arranged in buffer with spacing
              PROBFG = 0
              DO 550 L = 1,NDSN
                DO 540 K = 1,NPTS
                  ISUB = L+((K-1)*(NDSN+1))+(NPTS*(NDSN+1)*(L-1))
                  IF (ISUB.LE.BUFMAX) THEN
C                   room to add to buffer
                    TMPYX(ISUB) = YX(K+((L-1)*NPTS))
                  ELSE
C                   problem, too many points
                    PROBFG = 1
                  END IF
 540            CONTINUE
 550          CONTINUE
              NPTS = (NDSN+1)*NPTS
              IF (PROBFG.EQ.1) THEN
C               tell user too many points
                SGRP= 53
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              END IF
C             set specs for blank 'spacing' data set
C             save current user's specs for this set
              TTYP = TYPIND(NDSN+1)
              TSYM = SYMBOL(NDSN+1)
              TLNT = LNTYP(NDSN+1)
              TCOL = COLOR(NDSN+1)
              TPAT = PATTRN(NDSN+1)
              TWHI = WHICH(NDSN+1)
              TDTY = DTYPE(NDSN+1)
              TCLA = CLAB(NDSN+1)
C             now set spacing data set specs
              TYPIND(NDSN+1) = NDSN+1
              SYMBOL(NDSN+1) = 0
              LNTYP(NDSN+1)  = 1
              COLOR(NDSN+1)  = 1
              PATTRN(NDSN+1) = 0
              WHICH(NDSN+1)  = 1
              DTYPE(NDSN+1)  = 1
              CLAB(NDSN+1)   = ' '
C             do plot
              CALL QDBRPL(WINACT,ITMP2,MNLTZO,ARPCSC(1),NDSN+1,WHICH,
     I                    NPTS,MAXD,BUFMAX,TMPYX,SDATE,LTS,LTU,DTYPE,
     I                    TYPIND,LNTYP,COLOR,PATTRN,SYMBOL,
     I                    CLAB,YLLAB,YRLAB,ALAB,TITL,CMPTYP)
C             put current user's specs back for this set
              TYPIND(NDSN+1)= TTYP
              SYMBOL(NDSN+1)= TSYM
              LNTYP(NDSN+1) = TLNT
              COLOR(NDSN+1) = TCOL
              PATTRN(NDSN+1)= TPAT
              WHICH(NDSN+1) = TWHI
              DTYPE(NDSN+1) = TDTY
              CLAB(NDSN+1)  = TCLA
            ELSE IF (PLTTYP .EQ. 5) THEN
C             do flow/duration plot
              XLAB  = 'Percent chance flow exceeded'
              LPLT  = 8
              CALL QDFDPL (LPLT,I2,WINACT,NPTS,BUFMAX,MAXD,
     I                     NDSN,TYPIND,COLOR,LNTYP,
     I                     CLAB,XLAB,YLLAB,YRLAB,TITL,I1,
     I                     DEVCOD,CMPTYP,
     M                     YX)
            ELSE IF (PLTTYP .EQ. 6) THEN
C             do difference xy plot
              IF (NDSN.GT.1) THEN
C               need 2 data sets for this type of plot
C               data needs to be calculated
                DO 560 K = 1,NPTS
C                 calculate sim - measured
                  TMPYX(K) = YX(K+NPTS) - YX(K)
 560            CONTINUE
                DO 570 K = 1,NPTS
C                 fill in measured
                  TMPYX(K+NPTS) = YX(K)
 570            CONTINUE
C               build title for this plot
                CALL CVARAR (I20,CLAB(1),I20,CLAB1)
                CALL CVARAR (I20,CLAB(2),I20,CLAB2)
                LEN1  = LENSTR(I20,CLAB1)
                LEN2  = LENSTR(I20,CLAB2)
                TPTITL= 'Difference Plot ('
                IPOS  = 18
                CALL CHRCHR (LEN2,CLAB2,TPTTL1(IPOS))
                IPOS  = IPOS+LEN2+1
                TPTTL1(IPOS) = '-'
                IPOS  = IPOS+2
                CALL CHRCHR (LEN1,CLAB1,TPTTL1(IPOS))
                IPOS  = IPOS+LEN1+1
                TPTTL1(IPOS) = 'v'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = 's'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = '.'
                IPOS  = IPOS+2
                CALL CHRCHR (LEN1,CLAB1,TPTTL1(IPOS))
                IPOS  = IPOS+LEN1
                TPTTL1(IPOS) = ')'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = '&'
                IPOS  = IPOS+2
C               add tu, dtran
                TPTTL1(IPOS) = 'f'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'o'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'r'
                IPOS = IPOS+2
                CALL CVARAR (I8,CTRAN,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                CALL CVARAR (I8,CTUNIT,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                IF (CNTSEN.EQ.1) THEN
C                 locn and con vary, add scen to title
                  CALL CVARAR (I8,CSCENM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTCON.EQ.1) THEN
C                 scen and loc vary, add constit to title
                  CALL CVARAR (I8,CCONNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTLOC.EQ.1) THEN
C                 scen and con vary, add location to title
                  TPTTL1(IPOS) = 'a'
                  IPOS = IPOS+1
                  TPTTL1(IPOS) = 't'
                  IPOS = IPOS+2
                  CALL CVARAR (I8,CLOCNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                LPLT  = 1
                TPYLAB= 'DIFFERENCE'
                TPXLAB= CLAB(1)
                ITMP2(1)= 1
                ITMP2(2)= 1
                TSYMBL(1)= SYMBOL(1)
                TLNTYP(1)= 0
                IF (SYMBOL(1).EQ.0) THEN
C                 need a symbol, default to one
                  TSYMBL(1) = 1
                END IF
                CALL QDXYPT(LPLT,ITMP2,WINACT,NPTS,I2,
     I                      BUFMAX,TMPYX,CLAB,TPXLAB,TPYLAB,YRLAB,
     I                      TPTITL,I1,DEVCOD,TSYMBL,COLOR,TLNTYP,CMPTYP)
              ELSE
C               tell user not enough data sets
                SGRP= 54
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              END IF
            ELSE IF (PLTTYP .EQ. 7) THEN
C             do xy plot
              IF (NDSN.LT.2) THEN
C               tell user not enough data sets
                SGRP= 54
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              ELSE
C               have 2 data sets for this type of plot
C               build title for this plot
                CALL CVARAR (I20,CLAB(1),I20,CLAB1)
                CALL CVARAR (I20,CLAB(2),I20,CLAB2)
                LEN1  = LENSTR(I20,CLAB1)
                LEN2  = LENSTR(I20,CLAB2)
                IF (PEKVOL(1).EQ.1 .AND. STORMS.EQ.1) THEN
C                 do title for storm peaks plot
                  TPTITL= 'Storm Peaks Plot ('
                  IPOS  = 19
                ELSE IF (PEKVOL(1).EQ.2 .AND. STORMS.EQ.1) THEN
                  TPTITL= 'Storm Volume Plot ('
                  IPOS  = 20
                ELSE IF (STORMS.EQ.2) THEN
                  TPTITL= 'XY Plot ('
                  IPOS  = 10
                END IF
                CALL CHRCHR (LEN1,CLAB1,TPTTL1(IPOS))
                IPOS  = IPOS+LEN1+1
                TPTTL1(IPOS) = 'v'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = 's'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = '.'
                IPOS  = IPOS+2
                CALL CHRCHR (LEN2,CLAB2,TPTTL1(IPOS))
                IPOS  = IPOS+LEN2
                TPTTL1(IPOS) = ')'
                IPOS  = IPOS+1
                TPTTL1(IPOS) = '&'
                IPOS  = IPOS+2
C               add tu, dtran
                TPTTL1(IPOS) = 'f'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'o'
                IPOS = IPOS+1
                TPTTL1(IPOS) = 'r'
                IPOS = IPOS+2
                CALL CVARAR (I8,CTRAN,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                CALL CVARAR (I8,CTUNIT,I8,TPTTL1(IPOS))
                IPOS = IPOS+9
                IF (CNTSEN.EQ.1) THEN
C                 locn and con vary, add scen to title
                  CALL CVARAR (I8,CSCENM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTCON.EQ.1) THEN
C                 scen and loc vary, add constit to title
                  CALL CVARAR (I8,CCONNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                IF (CNTLOC.EQ.1) THEN
C                 scen and con vary, add location to title
                  TPTTL1(IPOS) = 'a'
                  IPOS = IPOS+1
                  TPTTL1(IPOS) = 't'
                  IPOS = IPOS+2
                  CALL CVARAR (I8,CLOCNM(1),I8,TPTTL1(IPOS))
                  IPOS = IPOS+9
                END IF
                STRMFG = 0
                IF (STORMS.EQ.1) THEN
C                 user wants storm plot, begin to find them
                  CALL TIMDIF(SDATE,EDATE,TU,TS,NVALS)
C                 must have at least 100 points to find 1 percent of points
                  IF (NVALS.LT.100) THEN
C                   not enough points to search, tell user
                    SGRP= 56
                    CALL PRNTXT (MESSFL,SCLU,SGRP)
                    STRMFG = 2
                  ELSE
C                   enough values, continue
                    STRMFG = 1
C                   find storms for first data set
                    CALL CMSTRM (SDATE,EDATE,ISTRMM(1),
     I                           ISTRMM(1),RSTRMP(1),BUFMAX,YX,
     I                           TS,TU,NVALS,
     M                           INDX,
     O                           CSTRM,STSTRM,ENSTRM,TRSTR1,STRMP1,
     O                           AVSTV1)
                    NPTS = CSTRM
C                   find storm runoff for second data set for matching days
                    CALL MATSTM (WDMSFL,ADSN(2),CSTRM,STSTRM,ENSTRM,
     I                           ISTRMM(1),BUFMAX,
     M                           TMPYX,
     O                           TRSTR2,STRMP2,AVSTV2,RETCOD)
                    DO 710 I = 1,CSTRM
C                     build storm table screen
                      CVAL2(1,I) = 1
                      RVAL2(1,I) = AVSTV1(I)
                      RVAL2(2,I) = STRMP1(I)
                      RVAL2(3,I) = AVSTV2(I)
                      RVAL2(4,I) = STRMP2(I)
                      IVAL2(1,I) = STSTRM(1,I)
                      IVAL2(2,I) = STSTRM(2,I)
                      IVAL2(3,I) = STSTRM(3,I)
                      IVAL2(4,I) = ENSTRM(1,I)
                      IVAL2(5,I) = ENSTRM(2,I)
                      IVAL2(6,I) = ENSTRM(3,I)
 710                CONTINUE
C                   build first header line
                    SGRP    = 62
                    CLENG(1)= 20
                    IPOS    = 1
                    CALL CHRCHR (CLENG(1),CLAB1,CVAL1(IPOS))
                    IPOS    = 21
                    CLENG(2)= 20
                    CALL CHRCHR (CLENG(2),CLAB2,CVAL1(IPOS))
                    CALL PMXTXA (MESSFL,SCLU,SGRP,I1,I1,IM1,I2,
     I                           CLENG,CVAL1)
                    CALL ZMNSST
C                   build second header line
                    SGRP = 63
                    CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,IM1,LINCNT)
                    CALL ZMNSST
                    SGRP = 59
                    CALL Q2INIT (MESSFL,SCLU,SGRP)
                    CALL Q2STCO (I1,CSTRM,CVAL2)
                    CALL Q2SETR (I4,CSTRM,RVAL2)
                    CALL Q2SETI (I6,CSTRM,IVAL2)
                    CALL Q2EDIT (CSTRM,
     O                           IRET)
                    IF (IRET .EQ. 1) THEN
C                     accept, do we want to add any date sets
                      CALL Q2GTCO (I1,CSTRM,
     O                             CVAL2)
                      DO 720 I = 1,CSTRM
                        IF (CVAL2(1,I).EQ.2) THEN
C                         user wants to add this as a date set
C                         get number of date sets
                          CALL DTADD (I1,NEWDT)
                          CDSID = '<STORM>'
                          SSDATE(1)= STSTRM(2,I)
                          SSDATE(2)= STSTRM(3,I)
                          SEDATE(1)= ENSTRM(2,I)
                          SEDATE(2)= ENSTRM(3,I)
                          START(1) = STSTRM(1,I)
                          START(2) = STSTRM(2,I)
                          START(3) = STSTRM(3,I)
                          START(4) = STSTRM(4,I)
                          START(5) = STSTRM(5,I)
                          START(6) = STSTRM(6,I)
                          ENDDT(1) = ENSTRM(1,I)
                          ENDDT(2) = ENSTRM(2,I)
                          ENDDT(3) = ENSTRM(3,I)
                          ENDDT(4) = ENSTRM(4,I)
                          ENDDT(5) = ENSTRM(5,I)
                          ENDDT(6) = ENSTRM(6,I)
                          CALL DTPUT (NEWDT,I0,CDSID,START,ENDDT,
     I                                SSDATE,SEDATE,TU,TS,DTRAN)
                        END IF
 720                  CONTINUE
                    END IF
C                   now put storm peaks or volumes in xy buffer
                    IF (PEKVOL(1).EQ.1) THEN
C                     want storm peaks plot
                      CALL COPYR (NPTS,STRMP1,TMPYX(1))
                      CALL COPYR (NPTS,STRMP2,TMPYX(NPTS+1))
                    ELSE
C                     want average storm volumes plot
                      CALL COPYR (NPTS,AVSTV1,TMPYX(1))
                      CALL COPYR (NPTS,AVSTV2,TMPYX(NPTS+1))
                    END IF
                  END IF
                END IF
                LPLT  = 1
                TPYLAB= CLAB(1)
                TPXLAB= CLAB(2)
                ITMP2(1)= 1
                ITMP2(2)= 1
                TSYMBL(1)= SYMBOL(1)
                TLNTYP(1)= 0
                IF (SYMBOL(1).EQ.0) THEN
C                 need a symbol, default to one
                  TSYMBL(1) = 1
                END IF
                IF (STRMFG.EQ.1) THEN
C                 do storm plot
                  IF (REGLIN.EQ.1 .AND. NPTS.GT.2) THEN
C                   want 45 degree line and regression line on plot
                    CALL FITLIN (NPTS,BUFMAX,TMPYX,
     O                           ACOEF,BCOEF,RSQUAR)
                    WRITE (99,*) 'REGRESSION: ',ACOEF,BCOEF,RSQUAR
C                   set legend location so it wont appear
                    LL(1)= -2.0
                    LL(2)= -2.0
                    CALL GPLEDG (LL)
                    CALL QDXYRG(LPLT,ITMP2,WINACT,NPTS,I2,
     I                          BUFMAX,TMPYX,CLAB,TPXLAB,TPYLAB,YRLAB,
     I                          TPTITL,I1,DEVCOD,TSYMBL,COLOR(1),
     I                          CMPTYP,ACOEF,BCOEF,RSQUAR)
C                   set legend location back to upper left
                    LL(1)= -1.0
                    LL(2)= -1.0
                    CALL GPLEDG (LL)
                  ELSE
C                   no regression line
                    CALL QDXYPT(LPLT,ITMP2,WINACT,NPTS,I2,
     I                          BUFMAX,TMPYX,CLAB,TPXLAB,TPYLAB,YRLAB,
     I                          TPTITL,I1,DEVCOD,TSYMBL,COLOR,
     I                          TLNTYP,CMPTYP)
                  END IF
                ELSE IF (STRMFG.EQ.0) THEN
C                 no storms, regular time period
                  IF (REGLIN.EQ.1 .AND. NPTS.GT.2) THEN
C                   want 45 degree line and regression line on plot
                    CALL FITLIN (NPTS,BUFMAX,YX,
     O                           ACOEF,BCOEF,RSQUAR)
                    WRITE (99,*) 'REGRESSION: ',ACOEF,BCOEF,RSQUAR
C                   set legend location so it wont appear
                    LL(1)= -2.0
                    LL(2)= -2.0
                    CALL GPLEDG (LL)
                    CALL QDXYRG(LPLT,ITMP2,WINACT,NPTS,I2,
     I                          BUFMAX,YX,CLAB,TPXLAB,TPYLAB,YRLAB,
     I                          TPTITL,I1,DEVCOD,TSYMBL,COLOR(1),
     I                          CMPTYP,ACOEF,BCOEF,RSQUAR)
C                   set legend location back to upper left
                    LL(1)= -1.0
                    LL(2)= -1.0
                    CALL GPLEDG (LL)
                  ELSE
C                   no regression line
                    CALL QDXYPT(LPLT,ITMP2,WINACT,NPTS,I2,
     I                          BUFMAX,YX,CLAB,TPXLAB,TPYLAB,YRLAB,
     I                          TPTITL,I1,DEVCOD,TSYMBL,COLOR(1),
     I                          TLNTYP,CMPTYP)
                  END IF
                END IF
              END IF
            ELSE IF (PLTTYP .EQ. 8) THEN
C             do frequency plot
              CALL STFRPL (MESSFL,WDMSFL,NDSN,ADSN,SDATE(1),EDATE(1),
     I                     TITL,ARHLOG(1),SSDATE(1),SEDATE(1),COLOR,
     I                     WINACT,CMPTYP,CLAB)
            END IF
          END IF
C
          IF (PLTDEV.GE.2 .AND. CMPTYP.EQ. 5) THEN
C           close print file on aviion
            CLOSE (UNIT=DEVFIL)
C           close workstation
            CALL PDNPLT(WINACT,I1,I0)
C           graph put on file 'gksplt.out//n'
            IF (PLTTYP.LT.5) THEN
              SGRP= 43
            ELSE IF (PLTTYP.EQ.5) THEN
              SGRP= 44
            ELSE IF (PLTTYP.EQ.6 .OR. PLTTYP.EQ.7) THEN
              SGRP= 48
            END IF
            CALL PRNTXI(MESSFL,SCLU,SGRP,PLTCNT)
          END IF
          GO TO 90
C
 70     CONTINUE
C         return
          GO TO 90
C
 90     CONTINUE
      IF (RESP .NE. 8) GO TO 11
C
      RETURN
      END
C
C
C
      SUBROUTINE   MATSTM
     I                   (WDMSFL,DSN,CSTRM,STSTRM,ENSTRM,NSTRM,BUFMAX,
     M                    TMPYX,
     O                    TRSTRM,STRMPK,AVSTVL,RETCOD)
C
C     + + + PURPOSE + + +
C     computes storm runoffs for multiple sets of dates
C
C     + + + DUMMY ARGUENTS + + +
      INTEGER   WDMSFL,DSN,CSTRM,NSTRM,BUFMAX,
     1          STSTRM(6,NSTRM),ENSTRM(6,NSTRM),RETCOD
      REAL      TRSTRM(NSTRM),TMPYX(BUFMAX),STRMPK(NSTRM),AVSTVL(NSTRM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Dataset number to calculate stats on
C     NSTRM  - Max number of storms to find
C     CSTRM  - Number of storms to find
C     STSTRM - Start date of storms
C     ENSTRM - End date of storms
C     TRSTRM - Total runoff for each storm
C     RETCOD - Return code
C     BUFMAX - maximum number of values to in storm period
C     TMPYX  - array of values in storm period
C     STRMPK - array of storm peaks
C     AVSTVL - array of average storm volumes
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TS,TU,NVALS,DTRAN,QFLG,I,J,
     1          START(6),ENDDT(6)
      REAL      RCNT
C
C     + + + EXTERNALS + + +
      EXTERNAL  TIMDIF, WDTGET
C
C     + + + END SPECIFICATIONS + + +
C
      TS   = 1
      TU   = 4
      QFLG = 30
      DTRAN= 0
C
      DO 100 I = 1,CSTRM
C       get number of values
        START(1) = STSTRM(1,I)
        START(2) = STSTRM(2,I)
        START(3) = STSTRM(3,I)
        START(4) = STSTRM(4,I)
        START(5) = STSTRM(5,I)
        START(6) = STSTRM(6,I)
        ENDDT(1) = ENSTRM(1,I)
        ENDDT(2) = ENSTRM(2,I)
        ENDDT(3) = ENSTRM(3,I)
        ENDDT(4) = ENSTRM(4,I)
        ENDDT(5) = ENSTRM(5,I)
        ENDDT(6) = ENSTRM(6,I)
        CALL TIMDIF(START,ENDDT,TU,TS,NVALS)
C
        IF (NVALS .GT. 1000) THEN
C         can only read so much data
          NVALS= 1000
        END IF
C
        WRITE(99,*) 'number of days:',NVALS
C
C       get data
        CALL WDTGET(WDMSFL,DSN,TS,START,NVALS,DTRAN,QFLG,TU,
     O              TMPYX,RETCOD)
        WRITE(99,*) 'just got data:',RETCOD
C
        TRSTRM(I)= 0.0
C       initialize storm peak values
        STRMPK(I)= TMPYX(1)
        RCNT = 0.0
        DO 50 J= 1,NVALS
C         sum runoff values for these storm dates
          TRSTRM(I)= TRSTRM(I)+ TMPYX(J)
          RCNT= RCNT + 1.0
          IF (TMPYX(J).GT.STRMPK(I)) THEN
C           new storm peak
            STRMPK(I) = TMPYX(J)
          END IF
 50     CONTINUE
C       compute average storm volume
        AVSTVL(I) = TRSTRM(I)/RCNT
C
 100  CONTINUE
C
      RETURN
      END

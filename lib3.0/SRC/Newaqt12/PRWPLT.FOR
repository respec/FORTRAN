C
C
C
      SUBROUTINE   PRWPLT
     I                   (WDMSFL,MESSFL,
     I                    CONCUR,CONNAM,
     I                    NLOC,LCSTAT,LCNAME,
     I                    NSEN,NSENON,SENACT,SENNAM,
     I                    WINDIM,
     M                    WINDOW,
     M                    PLTDEV,TU,TS,SDATE,EDATE,DTRAN,QFLAG,
     M                    ARHLOG,WHICH,DTYPE,
     M                    CLAB,YLLAB,YRLAB,ALAB,TITL,
     M                    LNTYP,COLOR,PATTRN,SYMBOL,WIPEIT)
C
C     + + + PURPOSE + + +
C     plot data at requested stations for requested time
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       WDMSFL,MESSFL,
     $              CONCUR,
     $              NLOC,LCSTAT(*),
     $              NSEN,NSENON,SENACT(*),
     $              PLTDEV,TU,TS,SDATE(6),EDATE(6),DTRAN,QFLAG,
     $              ARHLOG(1),WHICH(5),DTYPE(5),
     $              LNTYP(*),COLOR(*),PATTRN(*),SYMBOL(*),
     $              WINDOW(1),WIPEIT(7)
      CHARACTER*10  SENNAM(*)
      CHARACTER*16  CONNAM
      CHARACTER*40  LCNAME(*)
      CHARACTER*20  CLAB(*)
      CHARACTER*80  YRLAB,YLLAB,ALAB
      CHARACTER*240 TITL
      REAL          WINDIM(4,7)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     WDMSFL - Fortran unit number for daily data file
C     MESSFL - Fortran unit number for message file
C     CONCUR - ID number of current constituent
C     CONNAM - name of current constituent
C     NLOC   - Number of available locations
C     LCSTAT - Location status 1-yes, 2-no
C     LCNAME - Location name
C     NSEN   - Number of available scenarios
C     NSENON - Number of scenarios on
C     SENACT - scenario active flag
C     SENNAM - names of scenarios
C     PLTDEV - Plot output device (1-display,2-printer,3-plotter)
C     TS     - time step in TU units
C     TU     - time units (1-sec, 2-min, 3-hour, 4-day....)
C     SDATE  - start of plot (yr,mo,dy,hr,mn,sec)
C     EDATE  - end of plot (yr,mo,dy,hr,mn,sec)
C     DTRAN  - transformation function
C     QFLAG  - allowed quality flag
C     ARHLOG - type of axis - 1- arith, 2-log
C     WHICH  - for each line,
C              1 - left y-axis
C              2 - right y-axis
C              3 - auxiliary axis
C     DTYPE  - data type (1 - mean, 2 - point)
C     CLAB   - array of legends for curves
C     YLLAB  - left axis label
C     YRLAB  - right axis label
C     ALAB   - auxillary axis label
C     TITL   - title of plot
C     LNTYP  - line type for plot
C     COLOR  - color of line on plot
C     PATTRN - fill pattern for plot
C     SYMBOL - symbol type for plot
C     WINDOW - currently active plot window on screen
C     WINDIM - window dimension array
C     WIPEIT - array of flags indicating if window sizes have been changed
C
C     + + + PARAMETERS + + +
      INTEGER      MXBUF
      PARAMETER   (MXBUF=10000)
      INCLUDE     'pmxsen.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,I1,I0,I4,K,SAIND,SALEN,DEVCOD,SCLU,SGRP,RESP,II,
     $             RETCOD,NPTS,INUM,IBAS,IRET,ITMP(1),IPLT,TNVAR,PLTFLG,
     $             ISEN,LSEN,CVAL(5,3,MXSEN),IVAL,WINACT,LTS,LTU,I2,I7,
     $             CNUM,TYPIND(MXSEN),LPLT,CMPTYP,DEVFIL,PLTCNT,LINCNT,
     $             MNLTZO,MXCRV,ALOC,JTMP,MSEL(1),ILEN,DSN
      REAL         YX(MXBUF),R0,LL(2),RVAL,ARPCSC(1)
      CHARACTER*7  CLINE,CCOLOR,CPATRN,CSYMBL,TMPSTR
      CHARACTER*40 STANAM
      CHARACTER*80 TBUF80(MXSEN),XLAB
C
C     + + + EQUIVALENCES + + +
      CHARACTER*1  TBUFF(80,MXSEN),TMPST1(7)
      EQUIVALENCE (TBUF80,TBUFF),(TMPSTR,TMPST1)
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDTGET,QDTMPL,PRNTXI,QRESP,TIMDIF
      EXTERNAL     ZSTCMA,Q1INIT,QSETI,QSETIB,QSETCO,QSETR,QSETOP
      EXTERNAL     QGETR,QGETOP,ZIPI,PDNPLT,TSESPC,TSDSM
      EXTERNAL     Q1EDIT,QGETI,QGETIB,QGETCO,PRNTXT,ZGTRET,PLTXXX
      EXTERNAL     QRESCX,GPLEDG,QDFDPL,PMXCNW,LAYPLT,GGTGLV
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (A10,4A7)
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
      I2    = 2
      I4    = 4
      I7    = 7
      R0    = 0.0
      DEVFIL= 0
      MXCRV = 5
C
      SCLU  = 51
      RESP  = 1
      SAIND = 45
      SALEN = 48
C
      ARPCSC(1)= 100.0
C
C     determine how many locations are on
      ALOC= 0
      DO 2 I = 1,NLOC
        IF (LCSTAT(I).EQ.1) THEN
C         location is on, add to counter
          ALOC= ALOC + 1
          WRITE(99,*) 'prwplt,active loc',I,ALOC
        END IF
 2    CONTINUE
C
 5    CONTINUE
C       plot option loop
        IF (NSENON .GT. 1) THEN
C         mult scenarios
          TNVAR= NSENON
        ELSE
C         number of locations
          TNVAR= ALOC
        END IF
C
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        GO TO (10,20,30,40,45,50,50,70), RESP
C
 10     CONTINUE
C         plot device
          SGRP= 11
          CALL QRESP(MESSFL,SCLU,SGRP,
     M               PLTDEV)
          GO TO 90
C
 20     CONTINUE
C         make previous available
          I= 4
          CALL ZSTCMA (I,I1)
C         query for time changes
          SGRP= 21
          CALL Q1INIT(MESSFL,SCLU,SGRP)
          INUM= 3
          CALL QSETI(INUM,SDATE)
          IBAS= 4
          CALL QSETIB(INUM,IBAS,EDATE)
          ITMP(1)= TU- 2
          CALL QSETCO(I1,ITMP(1))
C         interact with user
          CALL Q1EDIT(IRET)
          IF (IRET .NE. 2) THEN
            CALL QGETI (INUM,
     O                  SDATE)
            CALL QGETIB(INUM,IBAS,
     O                  EDATE)
            CALL QGETCO(I1,ITMP(1))
            TU= ITMP(1)+ 2
            IF (TU .EQ. 6) THEN
C             annual, adjust to years
              SDATE(2)= 1
              EDATE(2)= 12
            END IF
          END IF
C         make previous unavailable
          I= 4
          CALL ZSTCMA (I,I0)
C
          GO TO 90
C
 30     CONTINUE
C         plot specs
          SGRP= 31
C
          DO 300 I = 1,NSEN
C           increment curve variables by 1 to account for 'none' option
            LNTYP(I)  = LNTYP(I)  + 1
            PATTRN(I) = PATTRN(I) + 1
            SYMBOL(I) = SYMBOL(I) + 1
 300      CONTINUE
C
C         make previous available
          I= 4
          CALL ZSTCMA (I,I1)
C
          CNUM= 5
C         initialize character buffer
          CALL ZIPI (15*NSEN,I0,CVAL)
          DO 310 I= 1,NSEN
            TMPSTR= '?LINETY'
            CALL GGTGLV(I7,LNTYP(I),TMPST1,ILEN)
            CLINE = TMPSTR
            TMPSTR= '?COLOR'
            CALL GGTGLV(I7,COLOR(I)+1,TMPST1,ILEN)
            CCOLOR= TMPSTR
            TMPSTR= '?FILL'
            CALL GGTGLV(I7,PATTRN(I),TMPST1,ILEN)
            CPATRN= TMPSTR
            TMPSTR= '?SYMBOL'
            CALL GGTGLV(I7,SYMBOL(I),TMPST1,ILEN)
            CSYMBL= TMPSTR
            WRITE(TBUF80(I),2000) SENNAM(I),CLINE,CCOLOR,CPATRN,CSYMBL
            CVAL(2,1,I)= LNTYP(I)
            CVAL(3,1,I)= COLOR(I)+ 1
            CVAL(4,1,I)= PATTRN(I)
            CVAL(5,1,I)= SYMBOL(I)
 310      CONTINUE
          CALL QRESCX (MESSFL,SCLU,SGRP,I1,I1,CNUM,NSEN,I1,
     M                 IVAL,RVAL,CVAL,TBUFF)
C         how did user exit
          CALL ZGTRET(I)
          IF (I .EQ. 1) THEN
C           with accept
            DO 320 I= 1,NSEN
              LNTYP(I) = CVAL(2,1,I)
              COLOR(I) = CVAL(3,1,I)- 1
              PATTRN(I)= CVAL(4,1,I)
              SYMBOL(I)= CVAL(5,1,I)
 320        CONTINUE
          END IF
C
          DO 330 I = 1,NSEN
C           decrement curve variables by 1 to account for 'none' option
            LNTYP(I)  = LNTYP(I)  - 1
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
C         make previous command available
          CALL ZSTCMA (I4,I1)
          SGRP= 51
          CALL Q1INIT (MESSFL,SCLU,SGRP)
          INUM= 1
          CALL QSETI (INUM,WINDOW)
          CALL Q1EDIT (IRET)
          IF (IRET.EQ.1) THEN
C           user wants to continue, read plot specs
            CALL QGETI (INUM,WINDOW)
          END IF
C         turn off previous command
          CALL ZSTCMA (I4,I0)
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
          CALL QSETOP(I1,I1,MSEL,MSEL,ARHLOG)
C         let user make changes
          CALL Q1EDIT
     O               (IRET)
          IF (IRET .EQ. 1) THEN
C           get users changes
            CALL QGETR (I1,
     O                  ARPCSC)
            CALL QGETOP(I1,
     O                  ARHLOG)
C           WRITE(99,*) 'edit ARHLOG,ARPCSC:',ARPCSC(1),ARHLOG(1)
          END IF
C         turn off previous command
          CALL ZSTCMA (I4,I0)
          GO TO 90
C
 50     CONTINUE
C         produce the plot
          IF (WIPEIT(WINDOW(1)).EQ.1 .AND. PLTDEV.EQ.1) THEN
C           window size has been modified, window needs to disappear
            CALL PDNPLT (WINDOW(1),I1,I0)
            WIPEIT(WINDOW(1)) = 0
          END IF
C         device details
          CALL PLTXXX (PLTDEV,WINDOW(1),WINDIM(1,WINDOW(1)),
     M                 DEVFIL,
     O                 DEVCOD,CMPTYP,WINACT,PLTCNT)
C         initialize labels to blanks
          DO 500 II=1,MXCRV
            CLAB(II) = ' '
 500      CONTINUE
C         retrieving data message
          SGRP = 40
          CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,LINCNT)
          IF (RESP .EQ. 6) THEN
C           want timeseries plot, continue
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
          WRITE(99,*) '    nloc:',NLOC,ALOC
          DO 510 J= 1,NSEN
            WRITE(99,*) '  scenar:',J,SENACT(J)
 510      CONTINUE
          PLTFLG= 0
          IPLT  = 0
          RETCOD= 0
          I     = 0
 520      CONTINUE
            I= I+ 1
C           look through all locations to see which ones to plot
            IF (LCSTAT(I).EQ.1) THEN
C             this station selected to plot, save name
              STANAM= LCNAME(I)
              ISEN  = 0
              J     = 0
 530          CONTINUE
                J= J+ 1
                IF (SENACT(J).EQ.1) THEN
C                 active scenario
                  ISEN= ISEN+ 1
                  LSEN= J
C                 save which scenario
                  TYPIND(ISEN)= J
                  IPLT= IPLT+ 1
C                 set curve labels
                  IF (NSENON.EQ.1) THEN
C                   station name is the label
                    CLAB(IPLT)= STANAM(1:20)
                  ELSE
C                   scenarios name
                    CLAB(IPLT)= SENNAM(J)
                  END IF
C
C                 get data
                  JTMP= (IPLT-1)* NPTS+ 1
                  IF (JTMP+NPTS .GT. MXBUF) THEN
C                   oops,will blow out buffer
                    WRITE(99,*) 'too many points to plot'
                  ELSE
C                   find data set with this data
                    CALL TSESPC (SENNAM(J),STANAM,CONNAM)
                    DSN = 1
                    CALL TSDSM (DSN)
                    WRITE(99,*) 'got dsn:',DSN
                    IF (DSN.GT.0) THEN
C                     get data for plot
                      WRITE(99,*) 'b4g data:',I,J,DSN,IPLT,JTMP
                      WRITE(99,*) '       ',YX(JTMP),YX(JTMP+NPTS-1)
                      WRITE(99,*) '       ',SDATE
                      CALL WDTGET (WDMSFL,DSN,LTS,SDATE,
     I                             NPTS,DTRAN,QFLAG,LTU,
     O                             YX(JTMP),RETCOD)
                      WRITE(99,*) 'aft data:',RETCOD
                      WRITE(99,*) '       ',YX(JTMP),YX(JTMP+NPTS-1)
                    ELSE
C                     cant find specified scen,loc,cons dsn
                      RETCOD = -1
                    END IF
                  END IF
                END IF
              IF (IPLT.LT.TNVAR .AND. J.LT.NSEN) GO TO 530
C
              IF (RETCOD.EQ.0 .AND. IPLT.EQ.TNVAR) THEN
C               may need to see what user thinks
                IF (PLTFLG .EQ. 1) THEN
C                 let user look at first plot, with interrupt available
                  K= 16
                  CALL ZSTCMA(K,I1)
                  SGRP= 42
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                  CALL ZGTRET(K)
                  IF (K .NE. 1) THEN
C                   user wants out
                    RETCOD= 1
                  END IF
                  K= 16
                  CALL ZSTCMA(K,I0)
                END IF
              END IF
              IF (RETCOD.EQ.0 .AND. IPLT.EQ.TNVAR) THEN
C               continue with plot, set title
                IF (NSENON .GT. 1) THEN
                  TITL= 'Scenario comparision plot for '//STANAM
                ELSE
                  TITL= 'Upstream/Downstream plot for '//SENNAM(LSEN)
                  DO 540 II= 1,MXCRV
                    TYPIND(II)= II
 540              CONTINUE
                END IF
C               set legend location upper left
                LL(1)= -1.0
                LL(2)= -1.0
                CALL GPLEDG (LL)
                IF (PLTDEV .GT. 1) THEN
                  SGRP = 45
                  CALL PMXCNW (MESSFL,SCLU,SGRP,I1,I1,I1,LINCNT)
                END IF
                IF (RESP .EQ. 6) THEN
C                 do timeseries plot
                  IF (NPTS*TNVAR.LE.MXBUF) THEN
C                   not too many points, go ahead
                    IF (NSENON.GT.1) THEN
C                     need to make sure obs is plotted last
                      CALL LAYPLT (TNVAR,NPTS,MXCRV,MXBUF,
     M                             WHICH,YX,DTYPE,
     M                             LNTYP,COLOR,PATTRN,SYMBOL,CLAB)
                    END IF
                    MNLTZO= 0
                    CALL QDTMPL(WINACT,ARHLOG,MNLTZO,ARPCSC,TNVAR,WHICH,
     I                          NPTS,MXCRV,MXBUF,YX,SDATE,LTS,LTU,DTYPE,
     I                          TYPIND,LNTYP,COLOR,PATTRN,SYMBOL,
     I                          CLAB,YLLAB,YRLAB,ALAB,TITL,CMPTYP)
                    IF (NSENON.GT.1) THEN
C                     need to switch back obs and last scenario
                      CALL LAYPLT (TNVAR,NPTS,MXCRV,MXBUF,
     M                             WHICH,YX,DTYPE,
     M                             LNTYP,COLOR,PATTRN,SYMBOL,CLAB)
                    END IF
                  ELSE
C                   number of points exceeds maximum, give warning
                    SGRP= 46
                    CALL PRNTXT (MESSFL,SCLU,SGRP)
                  END IF
                ELSE IF (RESP .EQ. 7) THEN
C                 do flow/duration plot
                  IF (NPTS*TNVAR.LE.MXBUF) THEN
C                   not too many points, go ahead
                    XLAB  = 'Percent chance flow exceeded'
                    LPLT  = 8
                    CALL QDFDPL (LPLT,I2,WINACT,NPTS,MXBUF,MXCRV,
     I                           TNVAR,TYPIND,COLOR,LNTYP,
     I                           CLAB,XLAB,YLLAB,YRLAB,TITL,I1,
     I                           DEVCOD,CMPTYP,
     M                           YX)
                  ELSE
C                   number of points exceeds maximum, give warning
                    SGRP= 46
                    CALL PRNTXT (MESSFL,SCLU,SGRP)
                  END IF
                END IF
                IF (PLTDEV .EQ. 1) THEN
                  PLTFLG= 1
                END IF
                IPLT  = 0
              ELSE IF (RETCOD .LT. 0) THEN
C               could not retrieve data
                SGRP= 41
                CALL PRNTXI (MESSFL,SCLU,SGRP,DSN)
              END IF
            END IF
          IF (I.LT.NLOC .AND. RETCOD.EQ.0) GO TO 520
C
          IF (PLTDEV.GE.2 .AND. CMPTYP.EQ. 5) THEN
C           close print file on aviion
            CLOSE (UNIT=DEVFIL)
C           close workstation
            CALL PDNPLT(WINACT,I1,I0)
C           graph put on file 'gksplt.out//n'
            IF (RESP .EQ. 6) THEN
              SGRP= 43
            ELSE IF (RESP .EQ. 7) THEN
              SGRP= 44
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
      IF (RESP .NE. 8) GO TO 5
C
      RETURN
      END

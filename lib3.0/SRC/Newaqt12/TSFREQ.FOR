C
C
C
      SUBROUTINE   SGFREQ
     I                   (MESSFL,WDMFL,DSNCNT,DSNBUF,IGR,CSCENM,CRCHNM,
     I                    CCONST)
C
C     + + + PURPOSE + + +
C     **** cousin to code in SWSTAT ****
C     Fit a log Pearson Type III distribution to one or more
C     sets of input data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMFL,DSNCNT,DSNBUF(DSNCNT),IGR
      CHARACTER*8 CSCENM(DSNCNT),CRCHNM(DSNCNT),CCONST(DSNCNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for main message file
C     WDMFL  - Fortran unit number for users WDM file
C     DSNCNT - Count of datasets available
C     DSNBUF - Dataset numbers of available datasets
C     IGR    - graphics available flag
C              1 - graphics available, 2 - graphics not available
C     CSCENM - Name of scenario associated with DSN
C     CRCHNM - Name of reach associated with DSN
C     CCONST - Name of constituent associated with DSN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,N,I1,IK,TCNT,SCLU,SGRP,RESP,NSM,NEM,SCNOUT,IWAIT,
     &             NPARM,FPRT,IPLOT,CHGDAT,NZI,NBYR,NEYR,LOGARH,NQS,
     &             NMO,NMDAYS,ICNT,ILH,NTOP,NTOT,RETCOD,NMONE,DELFG,
     &             IC(7),RESP2,DEVTYP,WOUT,UGFLG,NUMONS,IVAL(2),J,
     &             DEVCOD,WNDFLG,WSID,ICLOS,WCLU,HILOFG,CVAL(2),
     &             OPLEN,OPVAL(1),MSEL(1),IRET, NDAY, DTRAN, CNUM, DREC,
     &             IWRT,MXLN,I2,LWDMFL,LDSN,ODSN,IWDM,IDSN,TU,TS, RETC,
     &             SDATE(6),EDATE(6),GRPSIZ,INUM,CLEN(4),TLEN,I4,I8,I0,
     &             PREVFG,NDUR(10),JLEN,CMPTYP
      REAL         FK(27),PLUS(27,35),FNEG(27,35),GP(35),GN(35),X(120),
     &             Y(120),C(27),XX(120),SZ(120),SE(27),P(13),RI(13),
     &             Q(13),ADP(13),CCPA(27),QCPA(27),QNEW(13),DEV,STD,
     &             FN,VAR,SKEW,SESKEW,SCC,SVAR,SUM1,SUM3,CVR,SNO,XBAR,
     &             FI,T,TNI,TZI,SUM2,SUM4
C     REAL         QQX, QX
      CHARACTER*1  STANAM(80),STAID(16),CBUF(133),CTXT(28),CTMP1(4)
      CHARACTER*4  IMON(3,12), SMON(3,12), WDID
      CHARACTER*8  WNDNAM(2),CSEN,CLOC,CCON,CTMP,LPTHNM(1)
      CHARACTER*64 FLNAME
      CHARACTER*80 CSTANA
C
C     + + + INTRINSICS + + +
      INTRINSIC    ALOG10, FLOAT, SQRT, ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     SFPLOT, FILLN, FILLP, FILLPL, FILLGN, FILLGP, PSTUPW
      EXTERNAL     PRAOPT, CPA193, LPINPT, INTERN, RANK, RANKLW, INTERS
      EXTERNAL     QRESP, QFCLOS, GETFUN, ANPRGT, GPDEVC, ZSTCMA, WID2UD
      EXTERNAL     PROPLT, PRNTXI, PMXTXI, PDNPLT, PLTONE, GPINIT
      EXTERNAL     LPWDO1, LPWDO2, CALCQP, LPWDO3, TSHILO, CVARAR
      EXTERNAL     Q1INIT, Q1EDIT, QSETOP, QGETOP, QGETCO, QSETCO
      EXTERNAL     WID2UA, TSDSPC, QSETI, QSETCT, TSDSAV, INTCHR
      EXTERNAL     QGETI, QGETCT, CARVAR, WCH2UD, WDDSCK, PRNTXT, ZWNSOP
C
C     + + + DATA INITIALIZATIONS + + +
      DATA IMON/'Janu','ary ','31  ','Febr','uary',' 28 ','Marc','h 31',
     1'    ','Apri','l 30','    ','May ','31  ','    ','June',' 30 ','
     2  ','July',' 31 ','    ','Augu','st 3','1   ','Sept','embe','r 30'
     3,'Octo','ber ','31  ','Nove','mber',' 30 ','Dece','mber',' 31 '/
      DATA SMON/'Janu','ary ','1   ','Febr','uary',' 1  ','Marc','h 1 ',
     1'    ','Apri','l 1 ','    ','May ','1   ','    ','June',' 1  ','
     2  ','July',' 1  ','    ','Augu','st 3','1   ','Sept','embe','r 1 '
     3,'Octo','ber ','1   ','Nove','mber',' 1  ','Dece','mber',' 1  '/
      DATA   I1/1/, IC/7*1/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('1'///
     & '  Log-Pearson Type III Statistics (formerly USGS Program',
     & ' A193, Jan. 1986)'/
     & '     Note -- Use of Log-Pearson Type III or Pearson-Type III'/
     & '             distributions are for preliminary computations.'/
     & '             User is responsible for assessment and '/
     & '             interpretation.')
 2001 FORMAT(//2X,A8,1X,A8,' at ',A8)
 2002 FORMAT(//
     & '  Analysis for -- ',I3,' month period'/
     & '                    starting ',3A4/
     & '                    ending   ',3A4/
     & '                    ',I4,'-',I4)
 2003 FORMAT(/' ',I5,' zero values in data')
 2004 FORMAT(//'  Parameter is',I3,'-day high value.')
 2005 FORMAT(//'  Parameter is',I3,'-day low value.')
 2006 FORMAT(5X,5F12.3)
 2007 FORMAT(//'  The following 7 statistics are based on non-zero',
     &          ' values.')
 2008 FORMAT(/'  Mean or varance is negative or zero.')
 2009 FORMAT('  No further processing for this data set.')
 2011 FORMAT(/'  Absolute value of skew is greater than 3.3')
 2012 FORMAT('  Error occurred in interpolation routine.')
 2013 FORMAT(/' Note -- Adjusted parameter values include zero values'/
     &       '         and correspond with non-exceedance probabilities'
     &      /'         in column 1 and recurrence interval in column 2.'
     &      /'         Parameter values in column 3 are based on',
     &       ' non-zero values.')
 2015 FORMAT(/' ',I5,' non-zero values in data')
 2016 FORMAT('  *** Datum to large or small to process:',E12.5)
 2020 FORMAT(//
     & '        Exceedence        Recurrence        Parameter'/
     & '        Probability        Interval           Value  '/
     & '        -----------       ----------        ---------')
 2030 FORMAT(//
     & '       Non-exceedance     Recurrence        Parameter'/
     & '        Probability        Interval           Value  '/
     & '        -----------       ----------        ---------')
 2025 FORMAT(7X,F12.4,5X,F12.2,5X,F12.3)
 2040 FORMAT(//
     & '                                           Ajusted    Ajusted '/
     & '    Exceedence  Recurrence   Parameter   Exceedence  Parameter'/
     & '   Probability   Interval      Value    Probability    Value  '/
     & '   -----------  -----------  ---------  -----------  ---------')
 2050 FORMAT(//
     &'       Non-                            Ajusted Non-   Ajusted '/
     &'    Exceedence   Recurrence  Parameter   Exceedence  Parameter'/
     &'   Probability    Interval     Value    Probability    Value  '/
     &'   -----------  -----------  ---------  -----------  ---------')
 2045 FORMAT(2X,F12.4,1X,F12.2,   F11.3,1X,F12.4,   F11.3)
 2060 FORMAT(/1X,I5,
     &         ' statistics added as attributes to users WDM file',
     &               ' on data set',I5)
 2061 FORMAT(10X,42A1/10X,42A1/10X,49A1)
 2062 FORMAT(//'  Parameter is',I3,'-month value.')
 2101 FORMAT(/
     &       '  Mean                             ',F14.3)
 2102 FORMAT('  Variance                         ',F14.3)
 2103 FORMAT('  Standard Deviation               ',F14.3)
 2104 FORMAT('  Skewness                         ',F14.3)
 2105 FORMAT('  Standard Error of Skewness       ',F14.3)
 2106 FORMAT('  Serial Correlation Coefficient   ',F14.3)
 2107 FORMAT('  Coefficient of Variation         ',F14.3)
 2201 FORMAT(/
     &       '  Mean (logs)                             ',F10.3)
 2202 FORMAT('  Variance (logs)                         ',F10.3)
 2203 FORMAT('  Standard Deviation (logs)               ',F10.3)
 2204 FORMAT('  Skewness (logs)                         ',F10.3)
 2205 FORMAT('  Standard Error of Skewness (logs)       ',F10.3)
 2206 FORMAT('  Serial Correlation Coefficient (logs)   ',F10.3)
 2207 FORMAT('  Coefficient of Variation (logs)         ',F10.3)
 2300 FORMAT('1'///
     & '  Pearson Type III Statistics (formerly USGS Program',
     & ' A193, Jan. 1986'/
     & '    (Note -- Use of Pearson Type III distribution is for'/
     & '             preliminary computations.  User is responsible'/
     & '             for assessment and interpretation.')
 2302 FORMAT(//
     & '  Analysis for -- ',I3,' month period'/
     & '                    starting ',3A4/
     & '                    ',I4,'-',I4)
 2320 FORMAT(//
     & '       Non-exceedance      Parameter'/
     & '        Probability          Value  '/
     & '        -----------        ---------')
 2330 FORMAT(//
     &'       Non-                Ajusted Non-   Ajusted '/
     &'    Exceedence   Parameter   Exceedence  Parameter'/
     &'   Probability     Value    Probability    Value  '/
     &'   -----------   ---------  -----------  ---------')
 2325 FORMAT(7X,F12.4,5X,F12.3)
 2345 FORMAT(2X,F12.4,1X,   F11.3,1X,F12.4,   F11.3)
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NDUR/1,2,3,7,10,30,60,90,183,365/
C
C     + + + END SPECIFICATIONS + + +
C
C     NMDAYS - number of days for flow statistic
C     ILH    - flag for statistic (0-low, 1-high, 0-month or other)
C     NEM    - end month of season
C     NMO    - number of months in season or period
C     NZI    - number of years of zero events
C     NPARM  - number of non-zero years
C     LOGARH - log transformation flag, 1-yes, 2-no
C
      I0 = 0
      I2 = 2
      I4 = 4
      I8 = 8
C
      CALL FILLN (FNEG)
      CALL FILLP (PLUS)
      CALL FILLGN (GN)
      CALL FILLGP (GP)
      SCLU = 72
      WCLU = 153
C
C     init output parameters
      CALL GETFUN (I1,FPRT)
      FLNAME = 'FRQNCY.OUT'
      OPEN (UNIT=FPRT,FILE=FLNAME,STATUS='UNKNOWN')
      WOUT   = 2
      IF (IGR.EQ.1) THEN
C       init to output graphics
        IPLOT  = 1
C       get graphics device code
        I = 40
        CALL ANPRGT (I, DEVCOD)
      ELSE
C       graphics not available
        IPLOT= 0
      END IF
      DEVTYP = 1
      CHGDAT = 1
      LOGARH = 1
      SCNOUT = 1
      HILOFG = 1
      DTRAN  = 2
      NDAY   = 1
C
 10   CONTINUE
C       do main menu
C       set window prefixes
        LPTHNM(1) = 'A'
        CALL ZWNSOP (I1,LPTHNM)
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
        GO TO (100,200,300,400), RESP
C
 100    CONTINUE
C         specify hi or low flows, ndays, trans function
C         make previous available
          I= 4
          J= 1
          CALL ZSTCMA (I,J)
          SGRP = 10
          CALL Q1INIT (MESSFL,SCLU,SGRP)
          OPLEN= 1
          OPLEN= 1
          MSEL(1) = 1
          OPVAL(1)= HILOFG
C         set option field for high or low flow
          CALL QSETOP (OPLEN,OPLEN,MSEL,MSEL,OPVAL)
          CNUM   = 2
          CVAL(1)= DTRAN + 1
          CVAL(2)= NDAY
C         set character field for trans function and ndays
          CALL QSETCO (CNUM,CVAL)
          CALL Q1EDIT (
     O                 IRET)
          IF (IRET.EQ.1) THEN
C           user wants to continue
C           read whether user is specifying high or low flows
            CALL QGETOP (OPLEN,
     O                   OPVAL)
            HILOFG = OPVAL(1)
            CALL QGETCO (CNUM,
     O                   CVAL)
            DTRAN = CVAL(1) - 1
            NDAY  = CVAL(2)
          END IF
C         make previous unavailable
          I= 4
          J= 0
          CALL ZSTCMA (I,J)
          GO TO 900
C
 200    CONTINUE
C         modify output options
          CALL PRAOPT (MESSFL,WCLU,WDMFL,DSNCNT,DSNBUF,IGR,
     M                 FLNAME,FPRT,IPLOT,WOUT,SCNOUT,CHGDAT,NBYR,NEYR,
     M                 LOGARH)
          GO TO 900
C
 300    CONTINUE
C         perform analysis
          ICNT= 0
 310      CONTINUE
C           begin loop for each station
            ICNT  = ICNT + 1
C           make previous available
            I= 4
            J= 1
            CALL ZSTCMA (I,J)
 315        CONTINUE
C             adjust wdm and dsn as needed
              CALL WID2UD (WDMFL,DSNBUF(ICNT),
     O                     LWDMFL,LDSN)
C             get new dsn for output
              SGRP = 20
              CALL Q1INIT (MESSFL,SCLU,SGRP)
C             default to next available data set
              ODSN = DSNBUF(ICNT)
              CALL TSDSAV (ODSN)
              CALL WID2UA (I0,ODSN,
     O                     IWDM,IDSN,WDID)
C             find out default scen, loc, const for this data set
              CALL TSDSPC (DSNBUF(ICNT),
     O                     CSEN,CLOC,CTMP,
     O                     TU,TS,SDATE,EDATE,GRPSIZ)
C             set constituent name for n day annual peaks
              CALL INTCHR (NDUR(NDAY),I4,I0,
     O                     JLEN,CTMP1)
              IF (HILOFG.EQ.1) THEN
C               do for high periods
                CTMP1(1) = 'H'
              ELSE IF (HILOFG.EQ.2) THEN
C               do for low periods
                CTMP1(1) = 'L'
              END IF
              IF (NDUR(NDAY) .LT. 10) CTMP1(3) = '0'
              IF (NDUR(NDAY) .LT. 100) CTMP1(2) = '0'
              CALL CARVAR (I4,CTMP1,I4,CCON)
              CCON(5:8) = CTMP(1:4)
C             fill in values on screen
              INUM = 1
              IVAL(1) = IDSN
              CALL QSETI (INUM,IVAL)
              CNUM = 4
              CLEN(1) = 4
              CLEN(2) = 8
              CLEN(3) = 8
              CLEN(4) = 8
              TLEN = 28
              CALL CVARAR (I4,WDID,I4,CTXT(1))
              CALL CVARAR (I8,CSEN,I8,CTXT(5))
              CALL CVARAR (I8,CLOC,I8,CTXT(13))
              CALL CVARAR (I8,CCON,I8,CTXT(21))
              CALL QSETCT (CNUM,CLEN,TLEN,CTXT)
C             let user edit screen
              CALL Q1EDIT (IRET)
              IF (IRET.EQ.1) THEN
C               user wants to continue
                CALL QGETI (INUM,IVAL)
                IDSN = IVAL(1)
                CALL QGETCT (CNUM,CLEN,TLEN,CTXT)
                CALL CARVAR (I4,CTXT(1),I4,WDID)
                CALL CARVAR (I8,CTXT(5),I8,CSEN)
                CALL CARVAR (I8,CTXT(13),I8,CLOC)
                CALL CARVAR (I8,CTXT(21),I8,CCON)
                CALL WCH2UD (WDID,
     O                       IWDM)
C               check existance of this data set
                CALL WDDSCK (IWDM,IDSN,
     O                       DREC,RETC)
                IF (RETC.LT.0) THEN
C                 data set does not exist, okay to create
                  PREVFG = 0
                  RETCOD = 0
                ELSE
C                 tell user cant create
                  PREVFG = 1
                  RETCOD = 4
                  SGRP = 21
                  CALL PRNTXT (MESSFL,SCLU,SGRP)
                END IF
              ELSE
C               user wants previous
                PREVFG = 1
                RETCOD = 3
              END IF
            IF (RETCOD.EQ.4) GO TO 315
C           make previous unavailable
            I= 4
            J= 0
            CALL ZSTCMA (I,J)
C
            IF (PREVFG.EQ.0) THEN
              CALL TSHILO (MESSFL,LWDMFL,LDSN,IWDM,IDSN,HILOFG,
     I                     DTRAN,NDAY,CSEN,CLOC,CCON,
     O                     RETCOD)
              write(99,*) 'out of TSHILO with',RETCOD
            END IF
C
            IF (RETCOD.EQ.0) THEN
              CALL LPINPT (MESSFL,WCLU,IWDM,IDSN,CHGDAT,
     M                     NBYR,NEYR,
     O                     STANAM,STAID,NMO,NSM,NEM,NPARM,NMDAYS,
     O                     NUMONS,NZI,ILH,Y,RETCOD)
            END IF
            IF (RETCOD.EQ.0) THEN
C             build stanam from scen, loc, constit
              CSTANA = CSCENM(ICNT) // ' ' // CCONST(ICNT) // ' at ' //
     1                 CRCHNM(ICNT)
              I = 80
              CALL CVARAR (I,CSTANA,I,STANAM)
C
              CALL FILLPL (SE)
              N = NPARM
              IF (LOGARH .EQ. 1) THEN
                WRITE (FPRT,2000)
              ELSE
                WRITE(FPRT,2300)
              END IF
              WRITE (FPRT,2001) CSCENM(ICNT),CCONST(ICNT),CRCHNM(ICNT)
              IF (NUMONS .GT. 0) THEN
                WRITE (FPRT,2302) NMO, (SMON(I,NSM),I=1,3),
     &                            NBYR, NEYR
              ELSE
                WRITE (FPRT,2002) NMO, (SMON(I,NSM),I=1,3),
     &                           (IMON(I,NEM),I=1,3), NBYR, NEYR
              END IF
              IF (NUMONS .GT. 0) THEN
C               monthly statistics
                WRITE (FPRT,2062) NUMONS
              ELSE IF (ILH .EQ. 1) THEN
C               n-day high flow
                WRITE (FPRT,2004) NMDAYS
              ELSE
C               n-day low flow
                WRITE (FPRT,2005) NMDAYS
              END IF
              WRITE (FPRT,2003) NZI
              WRITE (FPRT,2015) N
              WRITE (FPRT,2006) (Y(I),I=1,N)
              WRITE (FPRT,2007)
C
              IF (N .LT. 3) THEN
C               Could not get enough data for dataset &. Skipping analysis.
                SGRP = 12
                CALL PMXTXI(MESSFL,WCLU,SGRP,I1,I1,I1,I1,DSNBUF(ICNT))
              ELSE
C               enough data, continue analysis
                IF (SCNOUT .EQ. 2) THEN
C                 show data set being analyzed
                  SGRP= 13
                  CALL PMXTXI(MESSFL,WCLU,SGRP,I1,I1,I1,I1,
     &                  DSNBUF(ICNT))
                ELSE
C                 processing data set (print and go)
                  SGRP = 34
                  CALL PMXTXI(MESSFL,WCLU,SGRP,I1,I1,I1,I1,
     &                  DSNBUF(ICNT))
                END IF
                UGFLG = 0
                SUM1=0.0
                IF (LOGARH .EQ. 2) THEN
C                 compute with no transformations
                  DO 320 I = 1, N
                    X(I) = Y(I)
                    SUM1 = SUM1 + X(I)
 320              CONTINUE
                ELSE
C                 convert data to logs base 10 and accumulate
C                 the sum of the logs for computation of mean
                  DO 325 I = 1,N
                    X(I)=ALOG10(Y(I))
                    SUM1=SUM1+X(I)
 325              CONTINUE
                END IF
C               accumulate sum of squares and cube of deviations from mean
                FN = FLOAT(N)
                XBAR=SUM1/FN
                IF (LOGARH .EQ. 2) THEN
C                 mean
                  WRITE (FPRT,2101) XBAR
                ELSE
C                 mean of logs
                  WRITE (FPRT,2201) XBAR
                END IF
                SUM2 = 0.0
                SUM3=0.0
                DO 330 I=1,N
                  DEV=X(I)-XBAR
                  IF (ABS(DEV) .LT. 1.0E12) THEN
                    SUM2=SUM2+DEV*DEV
                    SUM3=SUM3+DEV*DEV*DEV
                  ELSE
C                   datum too large
                    WRITE(FPRT,2016) X(I)
                    UGFLG = 1
                  END IF
 330            CONTINUE
C               compute the variance
                VAR=SUM2/(FN-1.0)
                IF (LOGARH .EQ. 2) THEN
                  WRITE (FPRT,2102) VAR
                ELSE
                  WRITE (FPRT,2202) VAR
                END IF
C
                IF (VAR .GT. 0.1E-7 .AND. UGFLG .EQ. 0) THEN
C                 compute the standard deviation
                  STD=SQRT(VAR)
                  IF (LOGARH .EQ. 2) THEN
                    WRITE (FPRT,2103) STD
                  ELSE
                    WRITE (FPRT,2203) STD
                  END IF
C                 compute the skewness
                  SKEW= (FN*SUM3)/((FN-1.0)*(FN-2.0)*STD*STD*STD)
                  IF (LOGARH .EQ. 2) THEN
                    WRITE (FPRT,2104) SKEW
                  ELSE
                    WRITE (FPRT,2204) SKEW
                  END IF
C                 compute the standard error of the skewness
                  SVAR=(6.0*FN*(FN-1.0))/
     1                 ((FN-2.0)*(FN+1.0)*(FN+3.0))
                  SESKEW=SQRT(SVAR)
                  IF (LOGARH .EQ. 2) THEN
                    WRITE (FPRT,2105) SESKEW
                  ELSE
                    WRITE (FPRT,2205) SESKEW
                  END IF
C                 compute serial correlation and coefficient of variation
                  SUM3 = 0.0
                  SNO = N-1
                  NMONE = N - 1
                  DO 335 I = 1,NMONE
                    XX(I) = X(I)*X(I+1)
                    SUM3 = XX(I) + SUM3
 335              CONTINUE
                  SCC = (SUM1-X(N))*(SUM1-X(1))
                  SCC = (SNO*SUM3)-SCC
                  SUM3 = 0.0
                  DO 340 I=1,N
                    XX(I) = X(I)*X(I)
                    SUM3 = XX(I) + SUM3
 340              CONTINUE
                  SUM4 = (SUM3-(X(1)*X(1)))*SNO
                  SUM3 = (SUM3-(X(N)*X(N)))*SNO
                  SUM2 = SUM1-X(1)
                  SUM1 = SUM1-X(N)
                  SUM1 = SUM1 * SUM1
                  SUM2 = SUM2 * SUM2
                  SUM3 = (SUM3-SUM1)*(SUM4-SUM2)
                  SUM3 = SQRT(SUM3)
                  SCC = SCC/SUM3
                  CVR = STD/XBAR
                  IF (LOGARH .EQ. 2) THEN
                    WRITE (FPRT,2106) SCC
                    WRITE (FPRT,2107) CVR
                  ELSE
                    WRITE (FPRT,2206) SCC
                    WRITE (FPRT,2207) CVR
                  END IF
                ELSE
                  WRITE (FPRT,2008)
                  WRITE (FPRT,2009)
                  UGFLG = 1
                END IF
C
                IF (UGFLG .EQ. 0) THEN
C
                  IF (ILH.EQ.0) THEN
                    CALL RANKLW (N, X)
                  ELSE
                    CALL RANK (N, X)
                  END IF
                  IF (ABS(SKEW).GT.3.30) THEN
                    WRITE (FPRT,2011)
                    WRITE (FPRT,2009)
                    UGFLG = 1
                  ELSE IF (SKEW.GE.0.0) THEN
                    CALL INTERS (SKEW,PLUS,GP,FK,IK)
                    IF (IK.NE.0) THEN
                      WRITE (FPRT,2012)
                      WRITE (FPRT,2009)
                      UGFLG = 1
                    END IF
                  ELSE
                    CALL INTERN (SKEW,FNEG,GN,FK,IK)
                    IF (IK.NE.0) THEN
                      WRITE (FPRT,2012)
                      WRITE (FPRT,2009)
                      UGFLG = 1
                    END IF
                  END IF
C
                  IF (WOUT .EQ. 2) THEN
C                   Put attributes on WDM file
                    CALL LPWDO1 (LWDMFL,MESSFL,WCLU,LDSN,
     &                         LOGARH,XBAR,STD,SKEW,NZI,NPARM,
     &                         CBUF,TCNT,RETCOD)
                  END IF
                  IF (UGFLG .EQ. 0) THEN
                    IF (WOUT .EQ. 2) THEN
C                     Put attributes on WDM file
                      CALL LPWDO1 (LWDMFL,MESSFL,WCLU,LDSN,
     &                             LOGARH,XBAR,STD,SKEW,NZI,NPARM,
     &                             CBUF,TCNT,RETCOD)
                    END IF
                    DO 345 I=1,27
                      C(I)=XBAR+FK(I)*STD
 345                CONTINUE
                    NTOP=0
                    NTOT=N + NZI
                    IF (NZI.GT.0) THEN
C                     conditional probability adjustment
                      CALL CPA193 (C,NTOT,NZI,NTOP,
     O                             CCPA)
                      DO 350 I=1,27
                        IF ((ABS(CCPA(I))+31.0) .GT. 0.001) THEN
                          IF (LOGARH .EQ. 1) THEN
                            QCPA(I)=10.0**CCPA(I)
                          ELSE
                            QCPA(I) = CCPA(I)
                          END IF
                        ELSE
                          QCPA(I)=0.0
                        END IF
 350                  CONTINUE
                    END IF
                    T = N
                    TZI = NZI
                    TNI = T + TZI
                    IF (NZI .LE. 0) THEN
                      IF (NUMONS .GT. 0) THEN
                        WRITE(FPRT,2320)
                      ELSE IF (ILH .EQ. 1) THEN
                        WRITE (FPRT,2020)
                      ELSE
                        WRITE (FPRT,2030)
                      END IF
                    ELSE
                      IF (NUMONS .GT. 0) THEN
                        WRITE(FPRT,2330)
                      ELSE IF (ILH .EQ. 1) THEN
                        WRITE (FPRT,2040)
                      ELSE
                        WRITE (FPRT,2050)
                      END IF
                    END IF
C
C                   Compute flow statistics for selected
C                   recurrence intervals
                    IF (NUMONS .GT. 0) THEN
                      NQS = 12
                    ELSE
C                     n-day stats
                      NQS = 11
                    END IF
                    CALL CALCQP (LOGARH, ILH,C,QCPA,NZI,NUMONS,NQS,
     &                           SE, Q,QNEW,P)
C
                    DO 360 I = 1,11
                      RI(I) = 1.0/P(I)
 360                CONTINUE
                    IF (NZI .LE. 0) THEN
                      IF (NUMONS .GT. 0) THEN
                        WRITE (FPRT,2325) (P(I),Q(I),I=1,NQS)
                      ELSE
                        WRITE (FPRT,2025) (P(I),RI(I),Q(I),I = 1,11)
                      END IF
                    ELSE
                      IF (ILH .EQ. 0) THEN
C                       n-day low flow
                        DO 365 I = 1,11
                          ADP(I) = (T/TNI)*P(I) + TZI/TNI
 365                    CONTINUE
                      ELSE
                        DO 370 I = 1,11
                          ADP(I) = T/TNI*P(I)
 370                    CONTINUE
                      END IF
                      IF (NUMONS .GT. 0) THEN
                        WRITE (FPRT,2345) (P(I),Q(I),ADP(I),
     &                                           QNEW(I),I=1,NQS)
                      ELSE
                        WRITE (FPRT,2045) (P(I),RI(I),Q(I),ADP(I),
     &                                     QNEW(I),I=1,11)
                      END IF
                      WRITE (FPRT,2013)
                    END IF
                    IF (WOUT.EQ.2) THEN
C                     construct attribute name, find number, put attributute
                      IF (NMDAYS .GT. 0) THEN
C                       for n-day statistics
                        CALL LPWDO2(LWDMFL, MESSFL, WCLU, LDSN,
     &                               ILH, NMDAYS, NZI, RI,
     &                               QNEW, Q, SCNOUT, TCNT, CBUF)
                      ELSE
C                       for n-month statistics
                        CALL LPWDO3(LWDMFL, MESSFL, WCLU, LDSN,
     &                               NZI, NQS,
     &                               RI, QNEW, Q, SCNOUT, TCNT, CBUF)
                      END IF
                      IF (TCNT .GT. 0) THEN
C                       print out which attributes added
                        WRITE(FPRT,2060) TCNT, LDSN
                        WRITE(FPRT,2061) CBUF
                      END IF
                    END IF
C
                    IF (IPLOT.EQ.1) THEN
C                     generate output plot
C                     initialize plotting specs
                      CALL GPINIT
                      DO 385 I=1,N
                        FI=FLOAT(I)
                        IF (ILH .EQ. 1  .OR.  NZI .EQ. 0) THEN
C                         for highs w and w/o zeros, plot lows w/o zeros
                          SZ(I)=FI/(TNI+1.0)
                        ELSE
                          SZ(I)=(FI+TZI)/(TNI+1.0)
                        END IF
 385                  CONTINUE
C
                      IF (NZI.GT.0) THEN
                        CALL SFPLOT (MESSFL,WCLU,X,SZ,CCPA,SE,N,NZI,
     I                               ILH,NMDAYS,STANAM,DEVTYP,LOGARH,
     I                               NSM,NEM)
                      ELSE
                        CALL SFPLOT (MESSFL,WCLU,X,SZ,C,SE,N,NZI,
     I                               ILH,NMDAYS,STANAM,DEVTYP,LOGARH,
     I                               NSM,NEM)
                      END IF
 390                  CONTINUE
C                       do main plot menu
                        LPTHNM(1) = 'A'
                        CALL ZWNSOP (I1,LPTHNM)
                        SGRP= 20
                        CALL QRESP (MESSFL,WCLU,SGRP,RESP2)
                        IF (RESP2.EQ.2) THEN
C                         modify options
                          WNDNAM(1)= 'Modify'
                          WNDNAM(2)= 'AFAM'
                          CALL PROPLT (MESSFL,IC,WNDNAM,WNDFLG)
                        ELSE IF (RESP2.EQ.1) THEN
C                         generate plot
                          WSID = DEVTYP
                          IWAIT = 0
                          ICLOS = 0
                          CALL PSTUPW (WSID, RETCOD)
                          CALL PLTONE
                          CALL PDNPLT (WSID,ICLOS,IWAIT)
C                         reset devtyp to scr. in case changed in PROPLT
                          DEVTYP = 1
C                         put device type and code in common block
                          CALL GPDEVC (DEVTYP, DEVCOD)
                        END IF
                      IF (RESP2.NE.3) GO TO 390
                    END IF
                  END IF
                END IF
              END IF
            ELSE IF (RETCOD .EQ. 2) THEN
C             skipping dsn &, not annual time series
              SGRP = 17
              CALL PRNTXI (MESSFL,WCLU,SGRP,DSNBUF(ICNT))
            ELSE IF (RETCOD.EQ.1) THEN
C             Could not get enough data for dataset &. Skipping
C             analysis.
              SGRP = 16
              CALL PRNTXI (MESSFL,WCLU,SGRP,DSNBUF(ICNT))
            ELSE IF (RETCOD.LT.0) THEN
C             user wants to stop all analysis
C             Could not get data for dsn @. error code @.
              SGRP = 33
              IVAL(1) = DSNBUF(ICNT)
              IVAL(2) = RETCOD
              IWRT = 0
              MXLN = 10
              CALL PMXTXI (MESSFL,WCLU,SGRP,MXLN,I1,IWRT,I2,
     $                     IVAL)
            END IF
          IF (ICNT .LT. DSNCNT .AND .PREVFG.EQ.0) GO TO 310
C         what type computer
          I= 1
          CALL ANPRGT (I,
     O                 CMPTYP)
          IWAIT= 0
          IF (CMPTYP .NE. 1) THEN
C           dont close workstation
            ICLOS= 0
          ELSE
C           close workstation on pc
            ICLOS= 1
          END IF
          CALL PDNPLT ( WSID, ICLOS, IWAIT )
          GO TO 900
C
 400    CONTINUE
C         return to stats menu
          GO TO 900
C
 900    CONTINUE
C
      IF (RESP.NE.4) GO TO 10
C
      DELFG = 0
      CALL QFCLOS (FPRT, DELFG)
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSHILO
     I                    (MESSFL,WDMFL,IDSN,OWDM,ODSN,
     I                     HILOFG,DTRAN,NDAY,CSEN,CLOC,CCON,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     **** cousin to NDHILO ****
C     This routine gets options from the user and determines n-day
C     high and low flow for each year between a start and end date.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMFL,IDSN,ODSN,RETCOD,HILOFG,DTRAN,NDAY,OWDM
      CHARACTER*8  CSEN,CLOC,CCON
C
C     + + + ARGUMENT DEFINITION + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number of users WDM file
C     IDSN   - Input dataset number
C     OWDM   - output wdm unit number
C     ODSN   - Output dataset number
C     RETCOD - Return code
C     HILOFG - hi or low flow wanted flag (1-hi,2-lo)
C     DTRAN  - transformation function
C     NDAY   - n-day value to use
C     CSEN   - scenario name
C     CLOC   - location name
C     CCON   - constituent name
C
C     + + + PARAMETERS + + +
      INTEGER    MXDUR, MXYRS
      PARAMETER (MXDUR = 10)
      PARAMETER (MXYRS = 100)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,L,L1,L4,L256,L257,I0,
     &             SAIND,SCLU,SGRP,NDUR(MXDUR),
     &             TUY,TUD,TS,DSNID,
     &             TEDATE(6),TSDATE(6),GPFLG,
     &             ERRCOD,QFLG,SMO,EMO,ILINE,
     &             DREC,TYR,NYR,NVAL,
     &             DTOVWR,K,IK,
     &             SALEN,JUST,OLEN,YDATE(6),TUNITS,TSTEP,
     &             NTYPE,ITMP(1),ITMPX(1),IVAL(1)
      REAL         QM(MXYRS,MXDUR),RDUR(MXDUR),ZAP,Q(366),
     &             TSFILL,AVE(MXDUR)
      CHARACTER*1  CTST(4),BUFF(80,MXDUR),BLNK,CTMP(8)
C
C     + + + INTRINSICS + + +
      INTRINSIC    REAL, MOD
C
C     + + + FUNCTIONS + + +
      INTEGER      DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL     DAYMON
      EXTERNAL     WTFNDT, ZIPR, ZIPC, TIMDIF
      EXTERNAL     WDTPUT, WDTGET, CVARAR
      EXTERNAL     WDBSAC, WDBSAI, WDDSCL, INTCHR, PMXTXI
      EXTERNAL     WDBSGR, WDBSGI, TSDSGN, WUD2ID
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  L4/4/, L1/1/,
     &       L256/256/,  L257/257/
      DATA NDUR/1,2,3,7,10,30,60,90,183,365/
      DATA  YDATE/0,1,1,0,0,0/
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      TUY   = 6
      TUD   = 4
      JUST  = 0
      DTOVWR= 0
      SCLU  = 72
      TS    = 1
      BLNK  = ' '
      I = 80 * MXDUR
      CALL ZIPC (I, BLNK, BUFF)
C
      SMO   = 10
      EMO   = 9
C     init to default number of durations
      RDUR(NDAY)= REAL(NDUR(NDAY))
      IF (HILOFG.EQ.1) THEN
C       do for high periods
        ZAP = -1.0E20
      ELSE IF (HILOFG.EQ.2) THEN
C       do for low periods
        ZAP = 1.0E20
      END IF
C
C     get start/end date for each dataset
      GPFLG = 1
      CALL WTFNDT (WDMFL,IDSN,GPFLG,
     O             DREC,TSDATE,TEDATE,ERRCOD)
      IF (ERRCOD.EQ.0) THEN
C       dates found ok
        IF (TSDATE(2).GT.SMO) THEN
C         data starts in month after start of season
C         start analysis in next year (first full year)
          TSDATE(1) = TSDATE(1) + 1
        END IF
        TSDATE(2) = SMO
        IF (TEDATE(2) .LT. EMO) THEN
C         data ends in month before end of season
C         end analysis in previous year (last full year)
          TEDATE(1) = TEDATE(1) - 1
        END IF
        TEDATE(2) = EMO
        TEDATE(3) = DAYMON(TEDATE(1), TEDATE(2))
      END IF
      IF (ERRCOD.EQ.0) THEN
C       get number of years
        IF (EMO .GT. SMO) THEN
C         period within one year
          TYR = TEDATE(1) - TSDATE(1) + 1
          YDATE(1) = TSDATE(1)
        ELSE
C         period spans two years
          TYR = TEDATE(1) - TSDATE(1)
          YDATE(1) = TSDATE(1) + 1
        END IF
        IF (TYR .GT. MXYRS) TYR = MXYRS
C
        NYR = 1
        TEDATE(1) = TSDATE(1)
        IF (EMO .LT. SMO) TEDATE(1) = TEDATE(1) + 1
        CALL TIMDIF (TSDATE, TEDATE, TUD, TS, NVAL)
        L = NDAY*MXYRS
        CALL ZIPR (L, ZAP, QM)
C
C       get TSFILL attribute for check of missing data
        SAIND = 32
        SALEN = 1
        CALL WDBSGR (WDMFL,IDSN,SAIND,SALEN,TSFILL,
     O               RETCOD)
        IF (RETCOD .NE. 0) THEN
C         set TSFILL
          TSFILL = -999.
        END IF
C       get time step
        SAIND= 33
        CALL WDBSGI (WDMFL,IDSN,SAIND,SALEN,
     O               TSTEP,RETCOD)
        IF (RETCOD.NE.0) THEN
C         set time step to default of one
          TSTEP= 1
        END IF
C       get time units
        SAIND= 17
        CALL WDBSGI (WDMFL,IDSN,SAIND,SALEN,
     O               TUNITS,ERRCOD)
        IF (ERRCOD.NE.0) THEN
C         set default time units to days
          TUNITS= 4
        END IF
      END IF
C
      IF (ERRCOD.EQ.0) THEN
C       everything ok so far, processing input dataset n
        SGRP = 12
        ITMPX(1) = IDSN
        CALL PMXTXI (MESSFL,SCLU,SGRP,L1,
     I               L1,L1,L1,ITMPX)
C       keep track of lines output so buffer doesn't blow out
        ILINE= 1
 620    CONTINUE
C         get year of data from WDM
          QFLG = 30
          CALL WDTGET (WDMFL,IDSN,TS,TSDATE,NVAL,
     I                 DTRAN,QFLG,TUD,
     O                 Q,RETCOD)
          IF (RETCOD .EQ. 0) THEN
C           check for bad values
            DO 630 I = 1,NVAL
              IF (Q(I).LE.TSFILL+1.0E-9) THEN
C               bad data value
                RETCOD = 1
              END IF
 630        CONTINUE
            IF (RETCOD .EQ. 0) THEN
C             compute statistics
              DO 655 I = 1,NVAL
                IF (I .GE. NDUR(NDAY)) THEN
C                 cumulate enough to check
                  AVE(NDAY) = 0.0
                  DO 640 K = 1, NDUR(NDAY)
                    IK = I + 1 - K
                    AVE(NDAY) = AVE(NDAY) + Q(IK)
 640              CONTINUE
                  IF (HILOFG.EQ.1) THEN
C                   high period
                    IF (AVE(NDAY) .GT. QM(NYR,NDAY)) THEN
                      QM(NYR,NDAY)=AVE(NDAY)
                    END IF
                  ELSE IF (HILOFG.EQ.2) THEN
C                   low period
                    IF (AVE(NDAY) .LT. QM(NYR,NDAY)) THEN
                      QM(NYR,NDAY)=AVE(NDAY)
                    END IF
                  END IF
                END IF
 655          CONTINUE
              QM(NYR,NDAY) = QM(NYR,NDAY)/RDUR(NDAY)
            ELSE
C             year beginning & has bad values, skipped analysis
              IF (ILINE.LT.49) THEN
C               still room in buffer to write message
                SGRP = 13
                ITMPX(1) = TSDATE(1)
                CALL PMXTXI (MESSFL,SCLU,SGRP,L1,-L1,
     I                       L1,L1,ITMPX)
C               update number of lines output so far
                ILINE= ILINE+ 1
              END IF
              QM(NYR,NDAY) = TSFILL
            END IF
          ELSE
C           data read error, values set to -999
            IF (ILINE.LT.49) THEN
C             still room in buffer to write message
              SGRP = 14
              CALL PMXTXI (MESSFL,SCLU,SGRP,L1,
     I                     -L1,L1,L1,ITMP)
              RETCOD = ITMP(1)
C             update number of lines output so far
              ILINE= ILINE+ 3
            END IF
            QM(NYR,NDAY) = -999.
          END IF
          NYR = NYR + 1
          IF (MOD(NYR,10) .EQ. 0) THEN
C           _ years processed
            IF (ILINE.LT.49) THEN
C             still room in buffer to write message
              SGRP = 15
              ITMP(1) = NYR
              CALL PMXTXI (MESSFL,SCLU,SGRP,L1,
     I                     -L1,L1,L1,ITMP)
C             update number of lines output so far
              ILINE= ILINE+ 1
            END IF
          END IF
          TEDATE(1) = TEDATE(1) + 1
          TSDATE(1) = TSDATE(1) + 1
          CALL TIMDIF (TSDATE,TEDATE,TUD,TS,
     O                 NVAL)
        IF (NYR .LE. TYR) GO TO 620
      END IF
C
      IF (ERRCOD.EQ.0) THEN
C       put temp annual time-series on WDM file
C       create a new dataset and copy attributes
C       assume same data set type
        NTYPE = 0
        CALL WDDSCL (WDMFL,IDSN,OWDM,ODSN,
     O               NTYPE,RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         set base year
          SAIND  = 27
          IVAL(1)= 1800
          CALL WDBSAI (OWDM,ODSN,MESSFL,
     I                 SAIND,L1,IVAL,
     O                 RETCOD)
C         set time for group pointers
          SAIND  = 34
          IVAL(1)= 7
          CALL WDBSAI (OWDM,ODSN,MESSFL,
     I                 SAIND,L1,IVAL,
     O                 RETCOD)
C         set time units
          SAIND  = 17
          IVAL(1)= 6
          CALL WDBSAI (OWDM,ODSN,MESSFL,
     I                 SAIND,L1,IVAL,
     O                 RETCOD)
C         uniform time step
          SAIND  = 85
          IVAL(1)= 1
          CALL WDBSAI (OWDM,ODSN,MESSFL,
     I                 SAIND,L1,IVAL,
     O                 RETCOD)
C         time series form
          SAIND  = 84
          IVAL(1)= 1
          CALL WDBSAI (OWDM,ODSN,MESSFL,
     I                 SAIND,L1,IVAL,
     O                 RETCOD)
C         time step
          SAIND  = 33
          IVAL(1)= 1
          CALL WDBSAI (OWDM,ODSN,MESSFL,
     I                 SAIND,L1,IVAL,
     O                 RETCOD)
C         set type of time-series
          CALL INTCHR (NDUR(NDAY),L4,JUST,
     O                 OLEN,CTST)
          IF (HILOFG.EQ.1) THEN
C           do for high periods
            CTST(1) = 'H'
          ELSE IF (HILOFG.EQ.2) THEN
C           do for low periods
            CTST(1) = 'L'
          END IF
          IF (NDUR(NDAY) .LT. 10) CTST(3) = '0'
          IF (NDUR(NDAY) .LT. 100) CTST(2) = '0'
C         store season
          IVAL(1) = SMO
          CALL WDBSAI (OWDM,ODSN,MESSFL,
     I                 L256,L1,IVAL,
     O                 RETCOD)
          IVAL(1) = EMO
          CALL WDBSAI (OWDM,ODSN,MESSFL,
     I                 L257,L1,IVAL,
     O                 RETCOD)
          CALL WDBSAC (OWDM,ODSN,MESSFL,L1,L4,CTST,
     O                 RETCOD)
C         put attribute for scenario name
          SAIND = 288
          SALEN = 8
          CALL CVARAR (SALEN,CSEN,SALEN,CTMP)
          CALL WDBSAC (OWDM,ODSN,MESSFL,SAIND,SALEN,CTMP,
     O                 RETCOD)
C         put attribute for constituent name
          SAIND = 289
          SALEN = 8
          CALL CVARAR (SALEN,CCON,SALEN,CTMP)
          CALL WDBSAC (OWDM,ODSN,MESSFL,SAIND,SALEN,CTMP,
     O                 RETCOD)
C         put attribute for location name
          SAIND = 290
          SALEN = 8
          CALL CVARAR (SALEN,CLOC,SALEN,CTMP)
          CALL WDBSAC (OWDM,ODSN,MESSFL,SAIND,SALEN,CTMP,
     O                 RETCOD)
C         let time series directory know this dsn exists
          CALL WUD2ID (OWDM,ODSN,
     O                 DSNID)
          IF (DSNID.EQ.ODSN) THEN
C           only one wdm file or first wdm file
            CALL TSDSGN (OWDM,ODSN)
          ELSE
C           mult wdm files
            CALL TSDSGN (I0,DSNID)
          END IF
        END IF
C
        QFLG = 0
        CALL WDTPUT (OWDM,ODSN,TS,YDATE,TYR,
     I               DTOVWR,QFLG,TUY,QM(1,NDAY),
     O               RETCOD)
        IF (RETCOD.NE.0) THEN
C         Return was &.
          IF (ILINE.LT.49) THEN
C           still room in buffer to write message
            SGRP = 14
            ITMPX(1) = RETCOD
            CALL PMXTXI (MESSFL,SCLU,SGRP,L1,-L1,
     I                   L1,L1,ITMPX)
C           update number of lines output so far
            ILINE= ILINE+ 3
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   STFRPL
     I                   (MESSFL,WDMFL,NDSN,ADSN,NBYR,NEYR,TITL,
     I                    ARHLOG,NSM,NEM,LCOLOR,
     I                    WINDOW,CMPTYP,CLAB)
C
C     + + + PURPOSE + + +
C     This routine sets up a frequency plot for specified data sets.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,WDMFL,NDSN,ARHLOG,ADSN(NDSN),LCOLOR(*),
     1             NBYR,NEYR,NSM,NEM,WINDOW,CMPTYP
      CHARACTER*20  CLAB(*)
      CHARACTER*240 TITL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     WDMFL  - Fortran unit number of users WDM file
C     NDSN   - no of WDM datasets for analysis
C     ADSN   - WDM dataset numbers for analysis
C     NBYR   - begin year for analysis
C     NEYR   - end year for analysis
C     ARHLOG - arith or log flag
C     NSM    - starting month
C     NEM    - ending month
C     TITL   - title of plot
C     LCOLOR - color of each line in plot
C     WINDOW - window to plot in
C     CMPTYP - computer type
C     CLAB   - label for each data set
C
C     + + + PARAMETERS + + +
      INTEGER    MXDSN
      PARAMETER (MXDSN = 5)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,NPARM,NZI(MXDSN),NTOP,NTOT,UGFLG,SAIND,
     1             SDATE(6),EDATE(6),GPFLG,RETCOD,N(MXDSN),L3,SCLU,
     2             DREC,QFLG,DTRAN,TUNITS,TSSTEP,IK,ILH(MXDSN),SALEN,
     3             I240,SGRP,LEN,L20,WCLU,IDSN,L80,L7,I1,I4,ILEN,JLEN
      REAL         FK(27),PLUS(27,35),FNEG(27,35),GP(35),GN(35),
     &             DEV,STD,Y(120),
     &             FN,VAR,SKEW,SUM1,SUM3,XBAR,FI,T,TNI,TZI,SUM2
      REAL         C(27,MXDSN),CCPA(27,MXDSN),SZ(120,MXDSN),
     &             SE(27),X(120,MXDSN)
      CHARACTER*1  TSTYPE(4)
      CHARACTER*1  GLB(20,2,MXDSN),CY(80),CX(80),GTITL(240),TMPLAB(20)
      CHARACTER*8  CTLAB
      CHARACTER*80 CY80
      CHARACTER*240 TMTITL
C
C     + + + FUNCTIONS + + +
      INTEGER      CHRINT,LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC    ALOG10, FLOAT, SQRT, ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDTGET, WTFNDT, TIMDIF, FILLPL, QTFRPL, CHRINT
      EXTERNAL    CPA193, INTERN, RANK, RANKLW, INTERS, WDBSGC
      EXTERNAL    GETTXT, COPYC, CVARAR, CARVAR, INTCHR, LENSTR
      EXTERNAL    FILLN, FILLP, FILLGN, FILLGP, PRNTXT
C
C     + + + DATA INITIALIZATION + + +
      DATA I240
     #    / 240/
      DATA  L20, L80
     #    /  20,  80/
C
C     + + + END SPECIFICATIONS + + +
C
      I1    = 1
      I4    = 4
      L7    = 7
      L3    = 3
      SCLU  = 72
      WCLU  = 153
      UGFLG = 0
      IDSN  = 0
C
      CALL FILLN (FNEG)
      CALL FILLP (PLUS)
      CALL FILLGN (GN)
      CALL FILLGP (GP)
      TUNITS= 6
      TSSTEP= 1
      GPFLG = 0
      DREC  = 0
C
 2    CONTINUE
C       loop to do plot for each data set selected
        IDSN = IDSN + 1
C
C       get available dates
        CALL WTFNDT (WDMFL,ADSN(IDSN),GPFLG,DREC,
     O               SDATE,EDATE,RETCOD)
        NPARM = 0
        NZI(IDSN) = 0
C       get type of flow statistic
        SAIND = 1
        SALEN = 4
        CALL WDBSGC (WDMFL,ADSN(IDSN),SAIND,SALEN,
     O               TSTYPE,RETCOD)
        IF (TSTYPE(1) .EQ. 'H') THEN
          ILH(IDSN) = 1
          CY80   = '   -DAY HIGH VALUE'
          CALL CARVAR (L3,TSTYPE(2),L3,CY80)
          CALL CVARAR (L80,CY80,L80,CY)
        ELSE IF (TSTYPE(1) .EQ. 'L') THEN
          ILH(IDSN) = 0
          CY80   = '   -DAY LOW VALUE'
          CALL CARVAR (L3,TSTYPE(2),L3,CY80)
          CALL CVARAR (L80,CY80,L80,CY)
        ELSE
C         problem, this is not the right kind of data set
          RETCOD = 2
        END IF
        IF (CY(1).EQ.'0') THEN
C         remove leading zero
          CY(1) = ' '
          IF (CY(2).EQ.'0') THEN
C           remove another leading zero
            CY(2) = ' '
          END IF
        END IF
        IF (RETCOD.EQ.0) THEN
C         set start and end year
          SDATE(1) = NBYR
          EDATE(1) = NEYR
          CALL TIMDIF (SDATE,EDATE,TUNITS,TSSTEP,
     O                 N(IDSN))
C
          IF (N(IDSN) .LE. 3) THEN
C           not enough data values
            RETCOD = 1
          ELSE
C           get data
            DTRAN = 0
            QFLG = 31
            CALL WDTGET (WDMFL,ADSN(IDSN),TSSTEP,SDATE,N(IDSN),
     I                   DTRAN,QFLG,TUNITS,
     O                   Y,RETCOD)
            IF (RETCOD .EQ. 0) THEN
C             get number of good years and non-zero years
              DO 20 I = 1,N(IDSN)
                IF (Y(I) .GE. 1.0E-9) THEN
                  NPARM = NPARM + 1
                  Y(NPARM) = Y(I)
                ELSE IF (Y(I) .GT. -1.0E-9) THEN
C                 zero defined as between -1.0E-9 and 1.0E-9
                  NZI(IDSN) = NZI(IDSN) + 1
                END IF
C               negative values ignored
 20           CONTINUE
            END IF
          END IF
        END IF
        IF (RETCOD.EQ.0) THEN
          CALL FILLPL (SE)
          N(IDSN) = NPARM
C
          SUM1=0.0
          IF (ARHLOG .EQ. 1) THEN
C           compute with no transformations
            DO 320 I = 1, N(IDSN)
              X(I,IDSN) = Y(I)
              SUM1 = SUM1 + X(I,IDSN)
 320        CONTINUE
          ELSE
C           convert data to logs base 10 and accumulate
C           the sum of the logs for computation of mean
            DO 325 I = 1,N(IDSN)
              X(I,IDSN)=ALOG10(Y(I))
              SUM1=SUM1+X(I,IDSN)
 325        CONTINUE
          END IF
C         accumulate sum of squares and cube of deviations from mean
          FN = FLOAT(N(IDSN))
          XBAR=SUM1/FN
          SUM2= 0.0
          SUM3=0.0
          DO 330 I=1,N(IDSN)
            DEV=X(I,IDSN)-XBAR
            IF (ABS(DEV) .LT. 1.0E12) THEN
              SUM2=SUM2+DEV*DEV
              SUM3=SUM3+DEV*DEV*DEV
            ELSE
C             datum too large
              UGFLG = 1
            END IF
 330      CONTINUE
C         compute the variance
          VAR=SUM2/(FN-1.0)
C
          IF (VAR .GT. 0.1E-7 .AND. UGFLG .EQ. 0) THEN
C           compute the standard deviation
            STD=SQRT(VAR)
C           compute the skewness
            SKEW= (FN*SUM3)/((FN-1.0)*(FN-2.0)*STD*STD*STD)
          ELSE
            UGFLG = 1
          END IF
C
          IF (UGFLG .EQ. 0) THEN
            IF (ILH(IDSN).EQ.0) THEN
              CALL RANKLW (N(IDSN), X(1,IDSN))
            ELSE
              CALL RANK (N(IDSN), X(1,IDSN))
            END IF
            IF (ABS(SKEW).GT.3.30) THEN
              UGFLG = 1
            ELSE IF (SKEW.GE.0.0) THEN
              CALL INTERS (SKEW,PLUS,GP,FK,IK)
              IF (IK.NE.0) THEN
                UGFLG = 1
              END IF
            ELSE
              CALL INTERN (SKEW,FNEG,GN,FK,IK)
              IF (IK.NE.0) THEN
                UGFLG = 1
              END IF
            END IF
C
            IF (UGFLG .EQ. 0) THEN
              DO 345 I=1,27
                C(I,IDSN)=XBAR+FK(I)*STD
 345          CONTINUE
              NTOP=0
              NTOT=N(IDSN) + NZI(IDSN)
              IF (NZI(IDSN).GT.0) THEN
C               conditional probability adjustment
                CALL CPA193 (C(1,IDSN),NTOT,NZI(IDSN),NTOP,
     O                       CCPA(1,IDSN))
              END IF
C
              T = N(IDSN)
              TZI = NZI(IDSN)
              TNI = T + TZI
              DO 385 I=1,N(IDSN)
                FI=FLOAT(I)
                IF (ILH(IDSN) .EQ. 1  .OR.  NZI(IDSN) .EQ. 0) THEN
C                 for highs w and w/o zeros, plot lows w/o zeros
                  SZ(I,IDSN)=FI/(TNI+1.0)
                ELSE
                  SZ(I,IDSN)=(FI+TZI)/(TNI+1.0)
                END IF
 385          CONTINUE
            END IF
          END IF
        END IF
        IF (UGFLG.NE.0) THEN
C         problem in computations, dont plot
          RETCOD = 3
        END IF
C
C       set specs for curves
C       Log-Pearson Type III
        SGRP= 25
        LEN = 80
        CALL GETTXT (MESSFL,WCLU,SGRP,LEN,GLB(1,1,IDSN))
        IF (ARHLOG .EQ. 1) THEN
C         delete Log- if not Log-Pearson analysis
          CALL COPYC (L20,GLB(5,2,IDSN),GLB(1,2,IDSN))
        END IF
C       set name of data set (scen/loc/const)
        CALL CVARAR (L20,CLAB(IDSN),L20,TMPLAB)
        ILEN = LENSTR(L20,TMPLAB)
        CALL CVARAR (L20,CLAB(IDSN),L20,GLB(1,1,IDSN))
        IF (ILEN.LT.14) THEN
C         room to add 'points'
          CTLAB = ' Points '
          ILEN  = ILEN + 1
          CALL CVARAR (L7,CTLAB,L7,GLB(ILEN,1,IDSN))
        END IF
C       see if another data set to do
      IF (RETCOD.EQ.0 .AND. IDSN.LT.NDSN .AND. IDSN.LT.MXDSN) GO TO 2
C
      IF (TITL(1:8).EQ.'Analysis') THEN
C       remove first 36 characters
        TMTITL = ' '
        TMTITL = TITL(37:240)
      ELSE
C       remove first 18 characters
        TMTITL = ' '
        TMTITL = TITL(19:240)
      END IF
      CALL CVARAR (I240,TMTITL,I240,GTITL)
C     add dates to title
      ILEN  = LENSTR(I240,GTITL)
      TMTITL= ',      through      '
      CALL CVARAR (L20,TMTITL,L20,GTITL(ILEN+1))
      CALL INTCHR (NBYR,I4,I1,JLEN,GTITL(ILEN+3))
      CALL INTCHR (NEYR,I4,I1,JLEN,GTITL(ILEN+16))
C     label for x and y axes
      IF (ILH(IDSN).EQ.1) THEN
C       for high flow
        SGRP = 46
      ELSE
        SGRP = 43
      END IF
      LEN = 80
      CALL GETTXT (MESSFL,WCLU,SGRP,LEN,CX)
C
      IF (RETCOD.EQ.0) THEN
C       go ahead and do plot
        CALL QTFRPL (IDSN,ARHLOG,WINDOW,ILH,CMPTYP,
     I               GLB,CY,CX,GTITL,LCOLOR,
     I               NZI,CCPA,C,SZ,SE,X,N)
      ELSE IF (RETCOD.EQ.1) THEN
C       not enough data values
        SGRP = 31
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      ELSE IF (RETCOD.EQ.2) THEN
C       this is not the right kind of data set
        SGRP = 32
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      ELSE IF (RETCOD.EQ.3) THEN
C       problem in computations
        SGRP = 33
        CALL PRNTXT (MESSFL,SCLU,SGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SFPLOT
     I                   (MESSFL,SCLU,X,SZ,C,SE,N,NZ,ILH,
     I                    NMDAYS,STATN,GDEVTY,LOGARH,NSM,NEM)
C
C     + +  PURPOSE + + +
C     Fill graphics common block with default values for
C     Log-Pearson type plot, cousin to fplot in awstat
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,N,NZ,ILH,NMDAYS,GDEVTY,LOGARH,NSM,NEM
      REAL        X(120),SZ(120),C(27),SE(27)
      CHARACTER*1 STATN(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     X      - log of peak flows base 10
C     SZ     - probabilities as a fraction
C     C      - log of computed flow base 10
C     SE     - probabilities of computed flows
C     N      - number of measured peak flows
C     NZ     - number of zero peak flows
C     ILH    - flag for statistic (0-low, 1-high, 0-month or other)
C     NMDAYS - number of days for flow statistic
C     STATN  - station number and name
C     GDEVTY - device type
C              1 - display monitor
C              2 - laser printer
C              3 - pen plotter
C              4 - CGM or GKS meta file
C     LOGARH - log transformation flag, 1-yes, 2-no
C     NSM    - start month of season
C     NEM    - end month of season
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I1,I2,I4,I5,I80,I240,SGRP,IPOS,RETCOD,I3,    
     1            LEN,LOC,OLEN,NP,NZP,LL(2),WHICH(4),GCV(2),GLN(2),
     2            GSY(2),GPT(2),GTRAN(4),GCL(2),LX,IMIN,IMAX,JUST,
     3            GDEVCD,IND,GTICS(4),GBVALF(4),L20,IIMAX,IIMIN
      REAL        TMP(120),Z,VMIN(4),VMAX(4),GLOC(2),R0,
     1            GSIZEL,GXPAGE,GYPAGE,GXPHYS,GYPHYS,GXLEN,GYLEN,
     2            GPLMN(4),GPLMX(4),
     3            XMIN,XMAX,YMIN,YMAX
      CHARACTER*1 CDUM(80),BLNK,CY(80),CX(80),GTITL(240),GLB(20,4),
     1            CD(5),CL(4),CH(5),
     2            MC(12,3)
      CHARACTER*80  CY80,CX80
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + FUNCTIONS + + +
      REAL        GAUSEX, SRMIN, SRMAX
C
C     + + + EXTERNALS + + +
      EXTERNAL    GAUSEX, SRMIN, SRMAX, GPNCRV, GPDATA, GPWCXY
      EXTERNAL    GETTXT, GPLEDG, CHRCHR, INTCHR, GPLABL, ZIPC, COPYI
      EXTERNAL    GPCURV, GPVAR, GPSIZE, GPDEVC, ANPRGT, SCALIT, GPSCLE
      EXTERNAL    GPLBXB, CVARAR, CARVAR
C
C     + + + DATA INITIALIZATION + + +
      DATA   I1, I2, I3, I4, I5, I80, I240,BLNK, R0
     #    /   1,  2,  3,  4,  5,  80,  240, ' ',0.0/
      DATA GCV/6,6/, GLN/0,1/, GSY/4,0/, GPT/0,0/, GCL/1,1/, L20/20/
      DATA CL/'l','o','w',' '/,  CH/'h','i','g','h',' '/
     &     CD/'-','d','a','y',' '/
      DATA GBVALF,GTICS/4*1,4*10/
      DATA GSIZEL, GXPAGE, GYPAGE, GXPHYS, GYPHYS, GXLEN, GYLEN
     1    /  0.11,   10.0,    8.0,    1.5,    1.5,   7.5,   5.0/
      DATA MC/'J','a','n','F','e','b','M','a','r','A','p','r',
     &        'M','a','y','J','u','n','J','u','l','A','u','g',
     &        'S','e','p','O','c','t','N','o','v','D','e','c'/
C
C     + + + END SPECIFICATIONS + + +
C
      NP = N
      IF (NP .GT. 120) NP = 120
      NZP = NZ
      IF (NZP .GT. 120) NZP = 120
C
C     set default device
      IND = 39+ GDEVTY
      CALL ANPRGT (IND,GDEVCD)
      CALL GPDEVC (GDEVTY,GDEVCD)
C     number of curves and variables
      CALL GPNCRV (I2,I4)
C     data to plot
      IPOS = 1
      DO 2 I = 1,NP
        TMP(I) = GAUSEX(SZ(I))
 2    CONTINUE
      Z = ABS(TMP(1))
      IF (Z .LT. ABS(TMP(NP))) Z = ABS(TMP(NP))
      VMIN(1) = -Z
      VMAX(1) = Z
      WHICH(1) = 4
      CALL GPDATA (I1,NP,TMP,RETCOD)
C
      IPOS = IPOS + NP
      DO 4 I = 1,NP
        IF (LOGARH .EQ. 1) THEN
          TMP(I) = 10.0**X(I)
        ELSE
          TMP(I) = X(I)
        END IF
 4    CONTINUE
      VMIN(2) = SRMIN(NP,TMP)
      VMAX(2) = SRMAX(NP,TMP)
      WHICH(2) = 1
      CALL GPDATA (I2,NP,TMP,RETCOD)
C
      IPOS = IPOS + NP
      DO 6 I = 1,27
        TMP(I) = GAUSEX(SE(I))
 6    CONTINUE
      IF (ILH.EQ.1) THEN
C       for high flows
        DO 7 I = 1,27
          IF (TMP(I) .LT. -Z) IMIN = I
          IF (TMP(I) .LT. Z)  IMAX = I
 7      CONTINUE
        IMIN = IMIN + 1
        VMIN(3) = TMP(IMIN)
        VMAX(3) = TMP(IMAX)
      ELSE
C       for low flows
        DO 8 I = 1,27
          IF (TMP(I) .GT. -Z) IMAX = I
          IF (TMP(I) .GT. Z)  IMIN = I
 8      CONTINUE
        IMIN = IMIN + 1
        VMIN(3) = TMP(IMAX)
        VMAX(3) = TMP(IMIN)
      END IF
      LEN = ABS(IMAX-IMIN) + 1
      WHICH(3) = 4
C     drop any zero flows if analysis with logs
      IF (LOGARH .EQ. 1) THEN
C       values in C are logs when LOGARH = 1
        IIMIN = IMIN
        IIMAX = IMAX
        DO 11 I = IMIN, IMAX
          IF (C(I) .LT. -10.0) IIMIN = I+1
 11     CONTINUE
C       reset IMIN and LEN
        LEN = ABS(IIMAX - IIMIN) + 1
        IMIN = IIMIN
        IMAX = IIMAX
      END IF
      CALL GPDATA (I3,LEN,TMP(IMIN),RETCOD)
C
      IPOS = IPOS + LEN
      DO 9 I = IMIN,IMAX
        IF (LOGARH .EQ. 1) THEN
          TMP(I) = 10.0**C(I)
        ELSE
          TMP(I) = C(I)
        END IF
 9    CONTINUE
      VMIN(4) = SRMIN(LEN,TMP(IMIN))
      VMAX(4) = SRMAX(LEN,TMP(IMIN))
      WHICH(4) = 1
      CALL GPDATA (I4,LEN,TMP(IMIN),RETCOD)
C
C     set which variable for each curve
      CALL GPWCXY (I1,I2,I1)
      CALL GPWCXY (I2,I4,I3)
C
C     labels and axis type
      CALL ZIPC (I80,BLNK,CDUM)
C     select log or arith for left y-axis
C     if LOGARH = 1, LL = 2,   if LOGARH = 2, LL = 1
      LL(1)= 3 - LOGARH
      LL(2)= 0
C     label for x and y axes
      IF (ILH.EQ.1) THEN
C       for high flow
        SGRP = 46
        LX   = 3
      ELSE
        SGRP = 43
        LX   = 3
      END IF
      LEN = 80
      CALL GETTXT (MESSFL,SCLU,SGRP,LEN,CX)
C
      IF (ILH.EQ.1) THEN
        CY80 = '   -DAY HIGH VALUE'
        CALL CVARAR (I80,CY80,I80,CY)
        JUST = 0
        CALL INTCHR (NMDAYS,I3,JUST,
     O               I,CY)
      ELSE
        CY80   = '   -DAY LOW VALUE'
        CALL CVARAR (I80,CY80,I80,CY)
        JUST = 0
        CALL INTCHR (NMDAYS,I3,JUST,
     O               I,CY)
      END IF
C
      CALL ZIPC (I240,BLNK,GTITL)
      LEN = 80
      CALL CHRCHR (LEN, STATN, GTITL)
      CALL GPLABL (LX,LL,R0,CY,CX,CDUM,GTITL)
C     also x-axis label
      CALL CARVAR (I80,CX,I80,CX80)
      CALL GPLBXB (CX80)
C     determine default x-axis scale based on min/max values
      IF (VMIN(1).LT.VMIN(3)) THEN
        XMIN= VMIN(1)
      ELSE
        XMIN= VMIN(3)
      END IF
      IF (VMAX(1).GT.VMAX(3)) THEN
        XMAX= VMAX(1)
      ELSE
        XMAX= VMAX(3)
      END IF
      CALL SCALIT (LX,XMIN,XMAX,GPLMN(4),GPLMX(4))
C     determine default y-axis scale based on min/max values
      IF (VMIN(2).LT.VMIN(4)) THEN
        YMIN= VMIN(2)
      ELSE
        YMIN= VMIN(4)
      END IF
      IF (VMAX(2).GT.VMAX(4)) THEN
        YMAX= VMAX(2)
      ELSE
        YMAX= VMAX(4)
      END IF
      CALL SCALIT (LL(1),YMIN,YMAX,GPLMN(1),GPLMX(1))
C     set scale for axes
      GPLMN(2)= 0.0
      GPLMN(3)= 0.0
      GPLMX(2)= 0.0
      GPLMX(3)= 0.0
      CALL GPSCLE (GPLMN,GPLMX,GTICS,GBVALF)
C     location of legend
      IF (ILH.EQ.1) THEN
        GLOC(1) = 0.05
      ELSE
        GLOC(1) = 0.5
      END IF
      GLOC(2) = 0.9
      CALL GPLEDG (GLOC)
C     get variable names and set min/max for each variable
C     Assigned std deviates, Observed flow, Calc std deviates,
C     Estimated flow
      SGRP= 24
      LEN = 80
      CALL GETTXT (MESSFL,SCLU,SGRP,LEN,GLB)
C     set transformations flags
      GTRAN(1) = 1
      GTRAN(2) = LL(1)
      GTRAN(3) = 1
      GTRAN(4) = LL(1)
      CALL GPVAR (VMIN,VMAX,WHICH,GTRAN,GLB)
C     set specs for curves
C     Observed    Log-Pearson Type III
      SGRP= 25
      LEN = 80
      CALL GETTXT (MESSFL,SCLU,SGRP,LEN,GLB)
C
      IF (LOGARH .EQ. 2) THEN
C       delete Log- if not Log-Pearson analysis
        CALL COPYI (L20,GLB(5,2),GLB(1,2))
      END IF
C     construct label for flow statistic
      IF (ILH .GE. 0) THEN
C       n-day (high/low) flow
        LOC = 1
        LEN = 5
        CALL INTCHR (NMDAYS,LEN,I1,OLEN,GLB(LOC,1))
        LOC = LOC + OLEN
        CALL CHRCHR (I5,CD,GLB(LOC,1))
        LOC = LOC + I5
        IF (ILH .EQ. 0) THEN
          CALL CHRCHR (I4,CL,GLB(LOC,1))
          LOC = LOC + I4
        ELSE
          CALL CHRCHR (I5,CH,GLB(LOC,1))
          LOC = LOC + I5
        END IF
      ELSE
C       month-to month flow
        LOC = 1
        LEN = 3
        CALL CHRCHR (LEN,MC(NSM,1),GLB(LOC,1))
        GLB(4,1) = '-'
        LOC = 5
        CALL CHRCHR (LEN,MC(NEM,1),GLB(LOC,1))
        LOC = 9
      END IF
C
      CALL GPCURV (GCV,GLN,GSY,GCL,GPT,GLB)
C     set plot sizes
      CALL GPSIZE (GSIZEL,GXPAGE,GYPAGE,GXPHYS,GYPHYS,GXLEN,GYLEN,R0)
C
      RETURN
      END

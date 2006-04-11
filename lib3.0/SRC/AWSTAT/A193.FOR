C
C
C
      SUBROUTINE   PRA193
     I                    (MESSFL,WDMFL,IGR,
     M                     DSNCNT,DSNBMX,DSNBUF)
C
C     + + + PURPOSE + + +
C     Fit a log Pearson Type III distribution to one or more
C     sets of input data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMFL,IGR,DSNCNT,DSNBMX,DSNBUF(DSNBMX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for main message file
C     WDMFL  - Fortran unit number for users WDM file
C     IGR    - graphics available flag
C              1 - graphics available, 2 - graphics not available
C     DSNCNT - number of data sets in the buffer
C     DSNBMX - size of data set buffer
C     DSNBUF - array of data set numbers to be processed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,N,I1,IK,TCNT,SCLU,SGRP,RESP,NSM,NEM,SCNOUT,IWAIT,
     &             NPARM,FPRT,IPLOT,CHGDAT,NZI,NBYR,NEYR,LOGARH,NQS,
     &             NMO,NMDAYS,ICNT,ILH,NTOP,NTOT,RETCOD,NMONE,DELFG,
     &             IC(7),RESP2,DEVTYP,WOUT,UGFLG,NUMONS,IVAL(2),
     &             DEVCOD,WNDFLG,WSID,ICLOS,IRET,
     &             XTYP, IXTYP, LEN, I2, MXLN, SCNFG, IWRT
      REAL         FK(27),PLUS(27,35),FNEG(27,35),GP(35),GN(35),X(120),
     &             Y(120),C(27),XX(120),SZ(120),SE(27),P(13),RI(13),
     &             Q(13),ADP(13),CCPA(27),QCPA(27),QNEW(13),DEV,STD,
     &             FN,VAR,SKEW,SESKEW,SCC,SVAR,SUM1,SUM3,CVR,SNO,XBAR,
     &             FI,T,TNI,TZI,SUM2,SUM4,RSOUT(22)  
C     REAL         QQX, QX
      CHARACTER*1  STANAM(48),STAID(16),CBUF(133),CXLAB(80),STATN(80)
      CHARACTER*8  PTHNAM,LPTHNM(1)
      CHARACTER*4  IMON(3,12), SMON(3,12)
      CHARACTER*8  WNDNAM(2)
      CHARACTER*64 FLNAME
C
C     + + + INTRINSICS + + +
      INTRINSIC    ALOG10, FLOAT, SQRT, ABS, REAL, INT
C
C     + + + EXTERNALS + + +
      EXTERNAL     FPLOT, FILLN, FILLP, FILLPL, FILLGN, FILLGP, PSTUPW
      EXTERNAL     PRAOPT, CPA193, LPINPT, INTERN, RANK, RANKLW, INTERS
      EXTERNAL     QRESP,  QFCLOS, PRWMSE, GETFUN, ANPRGT, GETTXT
      EXTERNAL     PROPLT, PRNTXT, PRNTXI, PMXTXI, PDNPLT, PLTONE 
      EXTERNAL     LPWDO1, LPWDO2, CALCQP, LPWDO3, GPLBXB, GGATXB
      EXTERNAL     GPINIT, DSINFO, Q1INIT, QSETR,  Q1EDIT, ZWNSOP
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
      DATA   I1,I2/1,2/, IC/7*1/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('1'///
     & '  Log-Pearson Type III Statistics (formerly USGS Program',
     & ' A193, Jan. 1986)'/
     & '     Note -- Use of Log-Pearson Type III or Pearson-Type III'/
     & '             distributions are for preliminary computations.'/
     & '             User is responsible for assessment and '/
     & '             interpretation.')
 2001 FORMAT(//2X,130A1)                    
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
 2019 FORMAT('1'//1X,80A1)
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
 2060 FORMAT(///1X,I5,
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
C     zero out data set buffer
      CALL FILLN (FNEG)
      CALL FILLP (PLUS)
      CALL FILLGN (GN)
      CALL FILLGP (GP)
      SCLU = 153
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
      LOGARH= 1
      SCNOUT = 1
C
 10   CONTINUE
C       do main menu
        LPTHNM(1) = 'S'
        CALL ZWNSOP (I1,LPTHNM)
        SGRP= 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
C
        GO TO (100,200,300,400), RESP
C
 100    CONTINUE
C         select datasets
          PTHNAM = 'SF      '
          CALL PRWMSE (MESSFL,WDMFL,DSNBMX, PTHNAM,
     M                 DSNBUF,DSNCNT)
          GO TO 900
C
 200    CONTINUE
C         modify output options
          CALL PRAOPT (MESSFL,SCLU,WDMFL,DSNCNT,DSNBUF,IGR,
     M                 FLNAME,FPRT,IPLOT,WOUT,SCNOUT,CHGDAT,NBYR,NEYR,
     M                 LOGARH)
          GO TO 900
C
 300    CONTINUE
C         perform analysis
          IF (DSNCNT.GT.0) THEN
C           data sets to work on
            ICNT= 0
 310        CONTINUE
C             begin loop for each station
              ICNT = ICNT + 1
              CALL LPINPT (MESSFL,SCLU,WDMFL,DSNBUF(ICNT),CHGDAT,
     M                     NBYR,NEYR,
     O                     STANAM,STAID,NMO,NSM,NEM,NPARM,NMDAYS,
     O                     NUMONS,NZI,ILH,Y,RETCOD)
              IF (RETCOD.EQ.0) THEN
                CALL FILLPL (SE)
                N = NPARM
                IF (LOGARH .EQ. 1) THEN
                  WRITE (FPRT,2000)
                ELSE
                  WRITE(FPRT,2300)
                END IF
C               get and write station identification
                LEN = 80
                CALL DSINFO (WDMFL,DSNBUF(ICNT),LEN,STATN)    
                WRITE (FPRT,2001) STATN         
                IF (NUMONS .GT. 0) THEN
                  WRITE (FPRT,2302) NMO, (SMON(I,NSM),I=1,3),
     &                              NBYR, NEYR
                ELSE
                  WRITE (FPRT,2002) NMO, (SMON(I,NSM),I=1,3),
     &                             (IMON(I,NEM),I=1,3), NBYR, NEYR
                END IF
                IF (NUMONS .GT. 0) THEN
C                 monthly statistics
                  WRITE (FPRT,2062) NUMONS
                ELSE IF (ILH .EQ. 1) THEN
C                 n-day high flow
                  WRITE (FPRT,2004) NMDAYS
                ELSE
C                 n-day low flow
                  WRITE (FPRT,2005) NMDAYS
                END IF
                WRITE (FPRT,2003) NZI
                WRITE (FPRT,2015) N
                WRITE (FPRT,2006) (Y(I),I=1,N)
                WRITE (FPRT,2007)
C
                IF (N .LT. 3) THEN
C                 Could not get enough data for dataset &. Skipping analysis.
                  SGRP = 12
                  CALL PMXTXI(MESSFL,SCLU,SGRP,I1,I1,I1,I1,DSNBUF(ICNT))
                ELSE
C                 enough data, continue analysis
                  IF (SCNOUT .EQ. 2) THEN
C                   show data set being analyzed
                    SGRP= 13
                    CALL PMXTXI(MESSFL,SCLU,SGRP,I1,I1,I1,I1,
     &                    DSNBUF(ICNT))
                  ELSE
C                   processing data set (print and go)
                    SGRP = 34
                    CALL PMXTXI(MESSFL,SCLU,SGRP,I1,I1,I1,I1,
     &                    DSNBUF(ICNT))
                  END IF
                  UGFLG = 0
                  SUM1=0.0
                  IF (LOGARH .EQ. 2) THEN
C                   compute with no transformations
                    DO 320 I = 1, N
                      X(I) = Y(I)
                      SUM1 = SUM1 + X(I)
 320                CONTINUE
                  ELSE
C                   convert data to logs base 10 and accumulate
C                   the sum of the logs for computation of mean
                    DO 325 I = 1,N
                      X(I)=ALOG10(Y(I))
                      SUM1=SUM1+X(I)
 325                CONTINUE
                  END IF
C                 accumulate sum of squares and cube of deviations from mean
                  FN = FLOAT(N)
                  XBAR=SUM1/FN
                  IF (LOGARH .EQ. 2) THEN
C                   mean
                    WRITE (FPRT,2101) XBAR
                  ELSE
C                   mean of logs
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
C                     datum too large
                      WRITE(FPRT,2016) X(I)
                      UGFLG = 1
                    END IF
 330              CONTINUE
C                 compute the variance
                  VAR=SUM2/(FN-1.0)
                  IF (LOGARH .EQ. 2) THEN
                    WRITE (FPRT,2102) VAR
                  ELSE
                    WRITE (FPRT,2202) VAR
                  END IF
C
                  IF (VAR .GT. 0.1E-7 .AND. UGFLG .EQ. 0) THEN
C                   compute the standard deviation
                    STD=SQRT(VAR)
                    IF (LOGARH .EQ. 2) THEN
                      WRITE (FPRT,2103) STD
                    ELSE
                      WRITE (FPRT,2203) STD
                    END IF
C                   compute the skewness
                    SKEW= (FN*SUM3)/((FN-1.0)*(FN-2.0)*STD*STD*STD)
                    IF (LOGARH .EQ. 2) THEN
                      WRITE (FPRT,2104) SKEW
                    ELSE
                      WRITE (FPRT,2204) SKEW
                    END IF
C                   compute the standard error of the skewness
                    SVAR=(6.0*FN*(FN-1.0))/
     1                   ((FN-2.0)*(FN+1.0)*(FN+3.0))
                    SESKEW=SQRT(SVAR)
                    IF (LOGARH .EQ. 2) THEN
                      WRITE (FPRT,2105) SESKEW
                    ELSE
                      WRITE (FPRT,2205) SESKEW
                    END IF
C                   compute serial correlation and coefficient of variation
                    SUM3 = 0.0
                    SNO = N-1
                    NMONE = N - 1
                    DO 335 I = 1,NMONE
                      XX(I) = X(I)*X(I+1)
                      SUM3 = XX(I) + SUM3
 335                CONTINUE
                    SCC = (SUM1-X(N))*(SUM1-X(1))
                    SCC = (SNO*SUM3)-SCC
                    SUM3 = 0.0
                    DO 340 I=1,N
                      XX(I) = X(I)*X(I)
                      SUM3 = XX(I) + SUM3
 340                CONTINUE
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
C                     Put attributes on WDM file
                      CALL LPWDO1 (WDMFL,MESSFL,SCLU,DSNBUF(ICNT),
     &                           LOGARH,XBAR,STD,SKEW,NZI,NPARM,
     &                           CBUF,TCNT,RETCOD)
                    END IF
                    IF (UGFLG .EQ. 0) THEN
                      IF (WOUT .EQ. 2) THEN
C                       Put attributes on WDM file
                        CALL LPWDO1 (WDMFL,MESSFL,SCLU,DSNBUF(ICNT),
     &                             LOGARH,XBAR,STD,SKEW,NZI,NPARM,
     &                             CBUF,TCNT,RETCOD)
                      END IF
                      DO 345 I=1,27
                        C(I)=XBAR+FK(I)*STD
 345                  CONTINUE
                      NTOP=0
                      NTOT=N + NZI
                      IF (NZI.GT.0) THEN
C                       conditional probability adjustment
                        CALL CPA193 (C,NTOT,NZI,NTOP,
     O                               CCPA)
                        DO 350 I=1,27
                          IF (ABS(CCPA(I)+31.0) .GT. 0.001) THEN
C                           This logic is to change a very small
C                           number as defined in the HARTAK routine
C                           used by CPA193
                            IF (LOGARH .EQ. 1) THEN
                              QCPA(I)=10.0**CCPA(I)
                            ELSE
                              QCPA(I) = CCPA(I)
                            END IF
                          ELSE
                            QCPA(I)=0.0
                          END IF
 350                    CONTINUE
                      END IF
C
C                     repeat station name for next table
                      WRITE(FPRT,2019) STATN
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
C                     Compute flow statistics for selected
C                     recurrence intervals
                      IF (NUMONS .GT. 0) THEN
                        NQS = 12
                      ELSE
C                       n-day stats
                        NQS = 11
                      END IF
                      CALL CALCQP (LOGARH, ILH,C,QCPA,NZI,NUMONS,NQS,
     &                             SE, Q,QNEW,P)
C
                      DO 360 I = 1,11
                        RI(I) = 1.0/P(I)
                        RSOUT(I) = REAL(INT(100.0*RI(I)+0.01))/100.0
                        RSOUT(I+11) = Q(I)      
 360                  CONTINUE
                      IF (NZI .LE. 0) THEN
                        IF (NUMONS .GT. 0) THEN
                          WRITE (FPRT,2325) (P(I),Q(I),I=1,NQS)
                        ELSE
                          WRITE (FPRT,2025) (P(I),RI(I),Q(I),I = 1,11)
                        END IF
                      ELSE
                        IF (ILH .EQ. 0) THEN
C                         n-day low flow
                          DO 365 I = 1,11
                            ADP(I) = (T/TNI)*P(I) + TZI/TNI
 365                      CONTINUE
                        ELSE
                          DO 370 I = 1,11
                            ADP(I) = T/TNI*P(I)
 370                      CONTINUE
                        END IF
                        IF (NUMONS .GT. 0) THEN
                          WRITE (FPRT,2345) (P(I),Q(I),ADP(I),
     &                                             QNEW(I),I=1,NQS)
                        ELSE
                          WRITE (FPRT,2045) (P(I),RI(I),Q(I),ADP(I),
     &                                       QNEW(I),I=1,11)
                        END IF
                        WRITE (FPRT,2013)
                      END IF
C
                      IF (WOUT.EQ.2) THEN
C                       construct attribute name, find number, put attributute
                        IF (NMDAYS .GT. 0) THEN
C                         for n-day statistics
                          CALL LPWDO2(WDMFL, MESSFL, SCLU, DSNBUF(ICNT),
     &                                 ILH, NMDAYS, NZI, RI,
     &                                 QNEW, Q, SCNOUT, TCNT, CBUF)
                        ELSE
C                         for n-month statistics
                          CALL LPWDO3(WDMFL, MESSFL, SCLU, DSNBUF(ICNT),
     &                                 NZI, NQS,
     &                                 RI, QNEW, Q, SCNOUT, TCNT, CBUF)
                        END IF
                        IF (TCNT .GT. 0) THEN
C                         print out which attributes added
                          WRITE(FPRT,2060) TCNT, DSNBUF(ICNT)
                          WRITE(FPRT,2061) CBUF
                        END IF
                      END IF
C
                      IF (SCNOUT .EQ. 2) THEN
C                       put results on screen
                        SGRP = 40
                        CALL Q1INIT (MESSFL,SCLU,SGRP)
                        LEN = 22
                        CALL QSETR (LEN,RSOUT)
                        CALL Q1EDIT (IRET)
                      END IF
C
                      IF (IPLOT.EQ.1) THEN
C                       generate output plot
C                       initialize plotting specs
                        CALL GPINIT
                        DO 385 I=1,N
                          FI=FLOAT(I)
                          IF (ILH .EQ. 1  .OR.  NZI .EQ. 0) THEN
C                           for highs w and w/o zeros, plot lows w/o zeros
                            SZ(I)=FI/(TNI+1.0)
                          ELSE
                            SZ(I)=(FI+TZI)/(TNI+1.0)
                          END IF
 385                    CONTINUE
C
                        IF (NZI.GT.0) THEN
                          CALL FPLOT (MESSFL,SCLU,X,SZ,CCPA,SE,N,NZI,
     I                                ILH,NMDAYS,STATN,DEVTYP,LOGARH,
     I                                NSM,NEM)
                        ELSE
                          CALL FPLOT (MESSFL,SCLU,X,SZ,C,SE,N,NZI,
     I                                ILH,NMDAYS,STATN,DEVTYP,LOGARH,
     I                                NSM,NEM)
                        END IF
                        WSID = 1
 390                    CONTINUE
C                         do main plot menu
                          LPTHNM(1) = 'S'
                          CALL ZWNSOP (I1,LPTHNM)
                          SGRP= 20
                          CALL QRESP (MESSFL,SCLU,SGRP,RESP2)
                          IF (RESP2.EQ.2) THEN
C                           modify options
                            WNDNAM(1)= 'Modify'
                            WNDNAM(2)= 'SFAM'
                            CALL GGATXB (IXTYP)
                            CALL PROPLT (MESSFL,IC,WNDNAM,WNDFLG)
                            CALL GGATXB (XTYP)
                            IF (IXTYP .NE. XTYP) THEN
C                             user changed x axis type so change label
                              LEN = 80
                              SGRP = 40 + XTYP
                              CALL GETTXT (MESSFL,SCLU,SGRP,LEN,CXLAB)
                              CALL GPLBXB (CXLAB)
                            END IF
                            IF (WNDFLG .EQ. 1) THEN
C                             user changed device
                              IWAIT = 0
                              ICLOS = 1
                              CALL PDNPLT (WSID,ICLOS,IWAIT)
                            END IF
                          ELSE IF (RESP2.EQ.1) THEN
C                           generate plot
                            IWAIT = 0
                            ICLOS = 0
                            CALL PSTUPW (WSID, RETCOD)
                            CALL PLTONE
                            CALL PDNPLT (WSID,ICLOS,IWAIT)
                          END IF
                        IF (RESP2.NE.3) GO TO 390
                      END IF
                    END IF
                  END IF
                END IF
              ELSE IF (RETCOD .EQ. 2) THEN
C               skipping dsn &, not annual time series
                SGRP = 17
                CALL PRNTXI (MESSFL,SCLU,SGRP,DSNBUF(ICNT))
              ELSE IF (RETCOD.EQ.1) THEN
C               Could not get enough data for dataset &. Skipping
C               analysis.
                SGRP = 16
                CALL PRNTXI (MESSFL,SCLU,SGRP,DSNBUF(ICNT))
              ELSE IF (RETCOD.LT.0) THEN
C               user wants to stop all analysis
C               Could not get data for dsn @. error code @.
                SGRP = 33
                IVAL(1) = DSNBUF(ICNT)
                IVAL(2) = RETCOD
                IWRT = 0
                MXLN = 10
                SCNFG = 1
                CALL PMXTXI (MESSFL,SCLU,SGRP,MXLN,SCNFG,IWRT,I2,
     $                       IVAL)
              END IF
            IF (ICNT .LT. DSNCNT) GO TO 310
C           close workstations
            ICLOS = 1
            IWAIT = 0
            CALL PDNPLT (WSID,ICLOS,IWAIT)
          ELSE
C           no data sets to work with
            SGRP= 18
            CALL PRNTXT (MESSFL,SCLU,SGRP)
          END IF
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
      SUBROUTINE   LPWDO1
     I                   (WDMFL,MESSFL,SCLU,DSN,LOGARH,
     I                     XBAR,STD,SKEW,NZI,NPARM,
     O                     CBUF,TCNT,RETCOD)
C     + + + PURPOSE + + +
C     This routine stores six computed statistics as attributes on
C     the WDM file for the Pearson and Log-Pearson Type III
C     distributions.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL, MESSFL, SCLU, DSN, LOGARH, RETCOD, NZI,
     &          NPARM, TCNT
      REAL      XBAR, STD, SKEW
      CHARACTER*1 CBUF(133)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of users WDM file
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number for AIDE subroutines
C     DSN    - WDM data set number for analysis
C     LOGARH - log transformation flag, 1-yes, 2-no
C     XBAR   - mean of annual series
C     STD    - standard deviation of annual series
C     SKEW   - skew coefficient of annual series
C     NZI    - number of years of zero events
C     NPARM  - number of non-zero years
C     CBUF   - character string of attribute names added to
C              data set on users WDM file
C     TCNT   - count of attributes added to users WDM file
C     RETCOD - return code = 0 if attributes put on WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CPOS, I133, SALEN, LOC, I6, SGRP, I1, J,
     &          ONUM, OTYP(2), SAIND, IVAL(1), CLEN(1)
      REAL      RDUM(1), RVAL(1)
      DOUBLE PRECISION DDUM(1)
      CHARACTER*1 CA(54), BLNK, DCHAR(8)
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPC, WDBSAR, CHRCHR, PMXTXM, WDBSAI, WDBSAC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CA/'M','E','A','N','N','D','S','D','N','D',' ',' ',
     &        'S','K','W','N','D',' ','N','U','M','Z','R','O',
     &        'N','O','N','Z','R','O','M','E','A','N','V','L',
     &        'S','T','D','D','E','V','S','K','E','W','C','F',
     &        'L','D','I','S','T',' '/
      DATA   BLNK/' '/,  I1,I6,I133/1,6,133/
      DATA   DCHAR/'L','P','3',' ','L','P',' ',' '/
C
C     + + + END SPECIFICATIONS + + +
C
      OTYP(1)= 4
      OTYP(2)= 1
      ONUM   = 2
      CPOS = 1
      TCNT = 0
      CALL ZIPC (I133,BLNK,CBUF)
C     put mean, variance and skew on dataset attributes
C     MEANND, SDND, SKWND for log transforms
C     MEANVL,STDDEV,SKEWCF for no transforms
      IF (LOGARH .EQ.1 ) THEN
        SAIND = 280
      ELSE
        SAIND = 14
      END IF
      SALEN = 1
      RVAL(1)= XBAR
      CALL WDBSAR (WDMFL,DSN,MESSFL,
     I             SAIND,SALEN,RVAL,
     O             RETCOD)
      IF (RETCOD .EQ. 0) THEN
C       fill out message to user
        TCNT = TCNT + 1
        IF (LOGARH .EQ. 1) THEN
          LOC = 1
        ELSE
          LOC = 31
        END IF
        CALL CHRCHR (I6,CA(LOC),CBUF(CPOS))
        CPOS = CPOS + 7
      ELSE
C       Above attribute not written, return code &
        SGRP = 14
        IVAL(1) = RETCOD
        CLEN(1) = 6
        CALL PMXTXM (MESSFL,SCLU,SGRP,I1,-I1,ONUM,OTYP,
     I               I1,IVAL,RDUM,DDUM,CLEN,CA)
      END IF
      IF (LOGARH .EQ. 1) THEN
        SAIND = 281
      ELSE
        SAIND = 15
      END IF
      RVAL(1)= STD
      CALL WDBSAR (WDMFL,DSN,MESSFL,
     I             SAIND,SALEN,RVAL,
     O             RETCOD)
      IF (RETCOD .EQ. 0) THEN
C       fill out message to user
        TCNT = TCNT + 1
        IF (LOGARH .EQ. 1) THEN
          LOC = 7
        ELSE
          LOC = 37
        END IF
        CALL CHRCHR(I6,CA(LOC),CBUF(CPOS))
        CPOS = CPOS + 7
      ELSE
C       Above attribute not written, return code &
        SGRP = 14
        IVAL(1) = RETCOD
        CLEN(1) = 6
        CALL PMXTXM (MESSFL,SCLU,SGRP,I1,-I1,ONUM,OTYP,
     I               I1,IVAL,RDUM,DDUM,CLEN,CA(7))
      END IF
      IF (LOGARH .EQ. 1) THEN
        SAIND = 282
      ELSE
        SAIND = 16
      END IF
      RVAL(1)= SKEW
      CALL WDBSAR (WDMFL,DSN,MESSFL,
     I             SAIND,SALEN,RVAL,
     O             RETCOD)
      IF (RETCOD .EQ. 0) THEN
C       fill out message to user
        TCNT = TCNT + 1
        IF (LOGARH .EQ. 1) THEN
          LOC = 13
        ELSE
          LOC = 43
        END IF
        CALL CHRCHR(I6,CA(LOC),CBUF(CPOS))
        CPOS = CPOS + 7
      ELSE
C       Above attribute not written, return code &
        SGRP = 14
        IVAL(1) = RETCOD
        CLEN(1) = 6
        CALL PMXTXM (MESSFL,SCLU,SGRP,I1,-I1,ONUM,OTYP,
     I               I1,IVAL,RDUM,DDUM,CLEN,CA(13))
      END IF
C     put number of zero and number of non-zero events
C     as attributes on dataset (NUMZRO NONZRO)
      SAIND = 287
      IVAL(1)= NZI
      CALL WDBSAI (WDMFL,DSN,MESSFL,
     I             SAIND,SALEN,IVAL,
     O             RETCOD)
      IF (RETCOD .EQ. 0) THEN
C       fill out message to user
        TCNT = TCNT + 1
        CALL CHRCHR(I6,CA(19),CBUF(CPOS))
        CPOS = CPOS + 7
      ELSE
C       Above attribute not written, return code &
        SGRP = 14
        IVAL(1) = RETCOD
        CLEN(1) = 6
        CALL PMXTXM (MESSFL,SCLU,SGRP,I1,-I1,ONUM,OTYP,
     I               I1,IVAL,RDUM,DDUM,CLEN,CA(19))
      END IF
      SAIND = 286
      IVAL(1)= NPARM
      CALL WDBSAI (WDMFL,DSN,MESSFL,
     I             SAIND,SALEN,IVAL,
     O             RETCOD)
      IF (RETCOD .EQ. 0) THEN
C       fill out message to user
        TCNT = TCNT + 1
        CALL CHRCHR(I6,CA(25),CBUF(CPOS))
        CPOS = CPOS + 7
      ELSE
C       Above attribute not written, return code &
        SGRP = 14
        IVAL(1) = RETCOD
        CLEN(1) = 6
        CALL PMXTXM (MESSFL,SCLU,SGRP,I1,-I1,ONUM,OTYP,
     I               I1,IVAL,RDUM,DDUM,CLEN,CA(25))
      END IF
C
C     store distribution type as attribute LDIST
      SAIND = 326
      SALEN = 4
      IF (LOGARH .EQ. 1) THEN
        J = 1
      ELSE
        J = 5
      END IF
      CALL WDBSAC (WDMFL,DSN,MESSFL,
     I             SAIND,SALEN,DCHAR(J),
     O             RETCOD)
      IF (RETCOD .EQ. 0) THEN
C       fill out message to user
        TCNT = TCNT + 1
        CALL CHRCHR(I6,CA(49),CBUF(CPOS))
        CPOS = CPOS + 7
      ELSE
C       Above attribute not written, return code &
        SGRP = 14
        IVAL(1) = RETCOD
        CLEN(1) = 6
        CALL PMXTXM (MESSFL,SCLU,SGRP,I1,-I1,ONUM,OTYP,
     I               I1,IVAL,RDUM,DDUM,CLEN,CA(25))
      END IF
      RETURN
      END
C
C
C
      SUBROUTINE   LPWDO2
     I                  (WDMFL,MESSFL,SCLU,DSN,ILH,NMDAYS,
     I                   NZI,RI,QNEW,Q,SCNOUT,
     M                   TCNT,CBUF)
C
C     + + + PURPOSE + + +
C     This routine stores values of events for pre-selected recurrence
C     intervals as attributes on a WDM file for the Pearson and
C     Log-Pearson Type III distribution.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL, MESSFL, SCLU, DSN,
     &          ILH, NMDAYS, NZI, TCNT, SCNOUT
      REAL      RI(13), QNEW(13), Q(13)
      CHARACTER*1 CBUF(133)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of users WDM file
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number for AIDE subroutines
C     DSN    - WDM data set number for analysis
C     ILH    - flag for statistic (0-low, 1-high, 0-month or other)
C     NMDAYS - number of days for flow statistic
C     NZI    - number of years of zero events
C     RI     - recurrence interval
C     QNEW   - statistic adjusted for zero flow
C     Q      - statistics for each specified recurrence interval
C     SCNOUT - flag for screen output (1-no, 2-yes)
C     TCNT   - count of number of attributes placed on WDM file
C     CBUF   - character string of attribute names added to
C              data set on users WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,K,JUST,SAIND,SATYP,SALEN,I1,SGRP,
     &          RETCOD, CPOS, OLEN, I6, I0, OTYP(2), ONUM,
     &          I42, I49, IVAL(1), CLEN(1)
      REAL      RDUM(1), QTEMP, RVAL(1)
      DOUBLE PRECISION   DDUM(1)
      CHARACTER*1    SANAM(6), BLNK, Z0
C
C     + + + INTRINSICS
      INTRINSIC  INT
C
C     + + + EXTERNALS + + +
      EXTERNAL  INTCHR, WDBSGX, WDBSAR, CHRCHR, PMXTXM, ZBLDWR
      EXTERNAL  PMXTXI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   Z0/'0'/,  BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      OTYP(1)= 4
      OTYP(2)= 1
      ONUM   = 2
      I1     = 1
      I0     = 0
      I42 = 42
      I49 = 49
      CPOS   = 7*TCNT + 1
      I6     = 6
      JUST   = 0
C
      DO 380 I = 1,11
        IF (ILH .EQ. 0) THEN
          SANAM(1) = 'L'
        ELSE
          SANAM(1) = 'H'
        END IF
        J = 2
        CALL INTCHR (NMDAYS,J,JUST,
     O               OLEN,SANAM(2))
        J = INT(RI(I)+0.001)
        IF (J .GE. 2) THEN
          K = 3
          CALL INTCHR(J,K,JUST,OLEN,SANAM(4))
          DO 375 K = 1,6
            IF(SANAM(K) .EQ. BLNK) SANAM(K) = Z0
 375      CONTINUE
C         find attribute number
          CALL WDBSGX (MESSFL,
     M                 SANAM,
     O                 SAIND,SATYP,SALEN)
C         put attribute value
          IF (SAIND .GT. 1) THEN
            IF (NZI .GT. 0) THEN
C             adjusted for zero flows
              QTEMP = QNEW(I)
            ELSE
C             no adjustments
              QTEMP = Q(I)
            END IF
            RVAL(1) = QTEMP
            CALL WDBSAR (WDMFL,DSN,MESSFL,
     I                   SAIND,SALEN,RVAL,
     O                   RETCOD)
            IF (RETCOD .EQ. 0) THEN
              TCNT = TCNT + 1
              CALL CHRCHR (I6, SANAM, CBUF(CPOS))
              CPOS = CPOS + 7
            ELSE
              IF (SCNOUT .EQ. 2) THEN
C               Above attribute not written, return code &
                SGRP = 14
                IVAL(1)= RETCOD
                CLEN(1)= 6
                CALL PMXTXM (MESSFL,SCLU,SGRP,I1,-I1,
     I                       ONUM,OTYP,I1,IVAL,
     I                       RDUM,DDUM,CLEN,SANAM)
              END IF
            END IF
          END IF
        END IF
 380  CONTINUE
      IF (SCNOUT .EQ. 2) THEN
C       & flow statistics written as dataset attributes
        SGRP = 15
        IVAL(1) = TCNT
        CALL PMXTXI (MESSFL,SCLU,SGRP,I1,-I1,I1,I1,IVAL)
        CALL ZBLDWR (I42,CBUF(1),I0,I1,J)
        CALL ZBLDWR (I42,CBUF(43),I0,I1,J)
        CALL ZBLDWR (I49,CBUF(85),I0,I0,J)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   LPWDO3
     I                  (WDMFL,MESSFL,SCLU,DSN,
     I                   NZI,NQS,RI,QNEW,Q,SCNOUT,
     M                   TCNT,CBUF)
C
C     + + + PURPOSE + + +
C     This routine stores values of events for pre-selected recurrence
C     intervals as attributes on a WDM file for the Pearson and
C     Log-Pearson Type III distribution.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL, MESSFL, SCLU, DSN, NQS,
     &          NZI, TCNT, SCNOUT
      REAL      RI(13), QNEW(13), Q(13)
      CHARACTER*1 CBUF(133)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of users WDM file
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number for AIDE subroutines
C     DSN    - WDM data set number for analysis
C     NMDAYS - number of days for flow statistic
C     NZI    - number of years of zero events
C     NQS    - number of quantiles
C     RI     - recurrence interval
C     QNEW   - statistic adjusted for zero flow
C     Q      - statistics for each specified recurrence interval
C     SCNOUT - flag for screen output (1-no, 2-yes)
C     TCNT   - count of number of attributes placed on WDM file
C     CBUF   - character string of attribute names added to
C              data set on users WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,K,JUST,SAIND,SATYP,SALEN,I1,SGRP,
     &          RETCOD, CPOS, OLEN, I6, I0, OTYP(2), ONUM,
     &          I42, I49, IPCT, IVAL(1), CLEN(1)
      REAL      RDUM(1), QTEMP, RVAL(1)
      DOUBLE PRECISION   DDUM(1)
      CHARACTER*1    SANAM(6), BLNK, Z0
C
C     + + + INTRINSICS
      INTRINSIC  INT
C
C     + + + EXTERNALS + + +
      EXTERNAL  INTCHR, WDBSGX, WDBSAR, CHRCHR, PMXTXM, ZBLDWR
      EXTERNAL  PMXTXI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   Z0/'0'/,  BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      OTYP(1)= 4
      OTYP(2)= 1
      ONUM   = 2
      I1     = 1
      I0     = 0
      I42 = 42
      I49 = 49
      CPOS   = 7*TCNT + 1
      I6     = 6
      JUST   = 0
C
      DO 380 I = 1,NQS
        SANAM(1) = 'L'
        SANAM(2) = 'Q'
        SANAM(3) = 'U'
        J = 3
         IPCT = INT(0.1 + 1000.0/RI(I))
        CALL INTCHR (IPCT,J,JUST,
     O               OLEN,SANAM(4))
        DO 375 K = 1,6
            IF(SANAM(K) .EQ. BLNK) SANAM(K) = Z0
 375    CONTINUE
C       find attribute number
        CALL WDBSGX (MESSFL,
     M               SANAM,
     O               SAIND,SATYP,SALEN)
C       put attribute value
        IF (SAIND .GT. 1) THEN
          IF (NZI .GT. 0) THEN
C           adjusted for zero flows
            QTEMP = QNEW(I)
          ELSE
C           no adjustments
            QTEMP = Q(I)
          END IF
          RVAL(1) = QTEMP
          CALL WDBSAR (WDMFL,DSN,MESSFL,
     I                 SAIND,SALEN,RVAL,
     O                 RETCOD)
          IF (RETCOD .EQ. 0) THEN
            TCNT = TCNT + 1
            CALL CHRCHR (I6, SANAM, CBUF(CPOS))
            CPOS = CPOS + 7
          ELSE
            IF (SCNOUT .EQ. 2) THEN
C             provide screen output
C             Above attribute not written, return code &
              SGRP = 14
              IVAL(1) = RETCOD
              CLEN(1) = 6
              CALL PMXTXM (MESSFL,SCLU,SGRP,I1,-I1,
     I                     ONUM,OTYP,I1,IVAL,
     I                     RDUM,DDUM,CLEN,SANAM)
            END IF
          END IF
        END IF
 380  CONTINUE
C
      IF (SCNOUT .EQ. 2) THEN
C       & flow statistics written as dataset attributes
        SGRP = 15
        IVAL(1) = TCNT
        CALL PMXTXI (MESSFL,SCLU,SGRP,I1,-I1,I1,I1,IVAL)
        CALL ZBLDWR (I42,CBUF(1),I0,I1,J)
        CALL ZBLDWR (I42,CBUF(43),I0,I1,J)
        CALL ZBLDWR (I49,CBUF(85),I0,I0,J)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CALCQP
     I                  (LOGARH,ILH,C,QCPA,NZI,NUMONS,NQS,
     M                   SE,
     O                   Q, QNEW,P)
C
C     + + + PURPOSE + + +
C     This routine computes statistics and probabilities for Pearson
C     Type III distribution based on pre-selected probabilities.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LOGARH, ILH, NZI, NUMONS, NQS
      REAL      C(27), QCPA(27), SE(27), Q(NQS), QNEW(NQS), P(NQS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LOGARH - log transformation flag, 1-yes, 2-no
C     ILH    - flag for statistic (0-low, 1-high, 0-month or other)
C     C      - statistics for selected recurrence intervals
C     QCPA   - statistics adjusted for zero flows
C     NZI    - number of years of zero events
C     SE     - probabilities of selected recurrence intervals
C     Q      - statistics for each specified recurrence interval
C     QNEW   - statistic adjusted for zero flow
C     P      - probability of selected recurrence intervals
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, DL(11), DH(11), MO(13)
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   DL/5,6,9,10,11,14,17,18,20,22,23/
      DATA   DH/5,9,10,11,14,17,18,20,22,23,24/
      DATA   MO/5,6,9,10,11,14,17,18,19,22,23,24,25/
C     + + + END SPECIFICATIONS + + +
C
      IF (NUMONS .LE. 0) THEN
C       for n-day statistics
        IF (ILH.EQ.0) THEN
C         for low flow statistics
          IF (LOGARH .EQ. 1) THEN
            DO 10 I = 1,11
              Q(I) = 10.0**C(DL(I))
 10         CONTINUE
          ELSE
            DO 20 I = 1,11
              Q(I) = C(DL(I))
 20         CONTINUE
          END IF
          IF (NZI.GT.0) THEN
            DO 30 I = 1,11
              QNEW(I) = QCPA(DL(I))
 30         CONTINUE
          END IF
          DO 35  I = 1,27
            SE(I) = 1.0-SE(I)
 35     CONTINUE
          DO 40 I = 1,11
            P(I) = SE(DL(I))
 40     CONTINUE
        ELSE
C         ILH equal 1 for high flow statistics
          IF (LOGARH .EQ. 1) THEN
            DO 50 I = 1,11
              Q(I) = 10.0**C(DH(I))
 50         CONTINUE
          ELSE
            DO 60 I = 1,11
              Q(I) = C(DH(I))
 60         CONTINUE
          END IF
          IF (NZI.GT.0) THEN
            DO 70 I = 1,11
              QNEW(I) = QCPA(DH(I))
 70         CONTINUE
          END IF
          DO 80 I = 1,11
            P(I) = SE(DH(I))
 80       CONTINUE
        END IF
      ELSE
C       for n-month statistics
        IF (LOGARH .EQ. 1) THEN
          DO 110 I = 1,NQS
            Q(I) = 10.0**C(MO(I))
 110      CONTINUE
        ELSE
          DO 120 I = 1,NQS
            Q(I) = C(MO(I))
 120      CONTINUE
        END IF
        IF (NZI.GT.0) THEN
          DO 130 I = 1,NQS
            QNEW(I) = QCPA(MO(I))
 130      CONTINUE
        END IF
        DO 135  I = 1,27
          SE(I) = 1.0-SE(I)
 135    CONTINUE
        DO 140 I = 1,NQS
          P(I) = SE(MO(I))
 140    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRAOPT
     I                   (MESSFL,SCLU,WDMFL,DSNCNT,DSNBUF,IGR,
     M                    FLNAME,FPRT,IPLOT,WOUT,SCNOUT,CHGDAT,
     M                    NBYR,NEYR,LOGARH)
C
C     + + + PURPOSE + + +
C     Modify parameters for output and options for
C     A193 Log-Pearson Frequency analysis.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,SCLU,WDMFL,DSNCNT,DSNBUF(DSNCNT),IGR,SCNOUT,
     &             FPRT,IPLOT,WOUT,CHGDAT,NBYR,NEYR,LOGARH
      CHARACTER*64 FLNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     WDMFL  - Fortran unit number for WDM file
C     DSNCNT - number of data sets in buffer
C     DSNBUF - array of data-set numbers in buffer
C     IGR    - graphics available flag
C              1 - graphics available, 2 - graphics not available
C     FLNAME - output file name
C     FPRT   - Fortran unit number for output file
C     IPLOT  - generate frequency plot flag,
C              0 - dont generate, 1 - generate frequency plot
C     WOUT   - output stats to WDM file flag (1 - NO, 2 - YES)
C     SCNOUT - output stats to screen (1 - no,  2 - yes)
C              1 - dont output to WDM, 2 - output to WDM
C     CHGDAT - change data period flag
C              1 - use Full period for each data set
C              2 - use Common period for all data sets
C              3 - Specify period for each data set
C     NBYR   - beginning year for analysis
C     NEYR   - ending year for analysis
C     LOGARH - log transformation flag, 1-yes, 2-no
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I, I0, I1, I64, I80, SGRP, INUM, CNUM, IRET, RETCOD,
     &            IVAL(2), CVAL(5), SDATE(6), EDATE(6), CIND, CLEN,
     &            IMIN(2), IMAX(2), IDEF(2), CORM, CBAS, NEWFUN, INQERR
      REAL        RVAL
      DOUBLE PRECISION DDUM
      CHARACTER*1  BUFF(80), FLNAM1(64), NEWFL1(64), BLNK
      CHARACTER*64 NEWFIL
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZSTCMA, ZGTRET, QSCSET, QRESPM
      EXTERNAL    CARVAR, CVARAR, ZIPC, QFOPFN, QFCLOS, GETFUN
      EXTERNAL    PRNTXT, WTDATE
      EXTERNAL    Q1INIT, Q1EDIT, QSTCTF, QSTCOB, QGTCTF, QGTCOB
C
C     + + + END SPECIFICATIONS + + +
C
      I0  = 0
      I1  = 1
      I64 = 64
      I80 = 80
      BLNK= ' '
C
C     allow previous command
      I= 4
      CALL ZSTCMA (I,I1)
C
 10   CONTINUE
C       return here on previous from failed open of output file
        NEWFUN = 0
C       output options screen
        SGRP= 2
        CALL Q1INIT (MESSFL, SCLU, SGRP)
C       set default values
        CALL CVARAR (I64, FLNAME, I64, FLNAM1)
        CIND = 1
        CLEN = 64
        CALL QSTCTF (CIND, CLEN, FLNAM1)
        CNUM = 5
        CBAS = 2
        CVAL(1) = IPLOT+ 1
        CVAL(2) = WOUT
        CVAL(3) = SCNOUT
        CVAL(4) = CHGDAT
        CVAL(5) = LOGARH
        CALL QSTCOB (CNUM, CBAS, CVAL)
C       allow user to edit screen
        CALL Q1EDIT (IRET)
        IF (IRET.EQ.1) THEN
C         user wants to continue
C         get values from screen
          CALL QGTCTF (CIND, CLEN,
     O                 NEWFL1)
          CALL QGTCOB (CNUM, CBAS,
     O                 CVAL)
          IPLOT = CVAL(1) - 1
          WOUT = CVAL(2)
          SCNOUT = CVAL(3)
          CHGDAT = CVAL(4)
          LOGARH = CVAL(5)
          CALL CARVAR (I64, NEWFL1, I64, NEWFIL)
          INQUIRE (FILE=NEWFIL, NUMBER=NEWFUN, IOSTAT=INQERR)
          IF (INQERR .NE. 0) THEN
C           invalid top-level directory specified
            SGRP = 4
            CALL PRNTXT (MESSFL, SCLU, SGRP)
C           get user exit command value
            CALL ZGTRET (IRET)
            IF (IRET .EQ. 1) THEN
C             user chose 'Accept'; reset output file to default
C             close any current output file
              IF (FPRT .NE. 0) THEN
                CALL QFCLOS (FPRT, I0)
                FPRT = 0
              END IF
              CALL GETFUN (I1, FPRT)
              FLNAME = 'FRQNCY.OUT'
              OPEN (UNIT=FPRT, FILE=FLNAME, STATUS='UNKNOWN')
            END IF
C           prevent the following IF/THEN from being entered
            NEWFUN = FPRT
          END IF
          IF (NEWFUN .NE. FPRT) THEN
C           output file specified different than current output file
C           close current output file
            CALL QFCLOS (FPRT,I0)
            FPRT = 0
C           open file for general output
            SGRP = 3
            CALL QFOPFN (MESSFL,SCLU,SGRP,NEWFL1,I0,
     O                   FPRT,RETCOD)
            IF (RETCOD.NE.0) THEN
C             problem opening file
              SGRP = 4
              CALL PRNTXT (MESSFL,SCLU,SGRP)
C             get user exit command value
              CALL ZGTRET (IRET)
              IF (IRET.EQ.1) THEN
C               user wants to continue, reset output file to default
                CALL GETFUN (I1,FPRT)
                FLNAME= 'FRQNCY.OUT'
                OPEN (UNIT=FPRT,FILE=FLNAME,STATUS='UNKNOWN')
              END IF
            ELSE
C             different file opened successfully
              CALL CARVAR (I64, NEWFL1, I64, FLNAME)
            END IF
          END IF
          IF (CHGDAT.EQ.2 .AND. IRET.EQ.1) THEN
C           user wants common period
            IF (DSNCNT.EQ.0) THEN
C             no data sets to determine common period for
              SGRP= 6
              CALL PRNTXT (MESSFL,SCLU,SGRP)
            ELSE
C             determine common period
              CORM = 1
              CALL WTDATE (WDMFL,DSNCNT,DSNBUF,CORM,
     O                     SDATE,EDATE,RETCOD)
              IF (RETCOD.EQ.0) THEN
C               common period found, modify as desired
                NBYR = SDATE(1)
                NEYR = EDATE(1)
 30             CONTINUE
C                 set default year bounds
                  INUM= 2
                  IMIN(1)= NBYR
                  IMAX(1)= NEYR
                  IDEF(1)= NBYR
                  IMIN(2)= NBYR
                  IMAX(2)= NEYR
                  IDEF(2)= NEYR
                  IVAL(1)= 0
                  IVAL(2)= 0
                  CALL QSCSET (INUM,I1,I1,I1,INUM,IMIN,IMAX,IDEF,
     I                         RVAL,RVAL,RVAL,DDUM,DDUM,DDUM,
     I                         IVAL,I1,I1,I1,BLNK)
C                 get starting and ending year
                  IVAL(1)= NBYR
                  IVAL(2)= NEYR
                  CALL ZIPC (I80,BLNK,BUFF)
                  SGRP = 5
                  CALL QRESPM (MESSFL,SCLU,SGRP,INUM,I1,I1,
     M                         IVAL,RVAL,CVAL,BUFF)
C                 get user exit command value
                  CALL ZGTRET (IRET)
                  IF (IRET.EQ.1) THEN
C                   user wants to continue
                    NBYR= IVAL(1)
                    NEYR= IVAL(2)
                    IF (NBYR.GE.NEYR) THEN
C                     start year must preceed end year
                      SGRP = 7
                      CALL PRNTXT (MESSFL,SCLU,SGRP)
                    END IF
                  END IF
                IF (NBYR.GE.NEYR) GO TO 30
              ELSE
C               no common period found, problem
                SGRP= 8
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              END IF
            END IF
          END IF
        ELSE
C         user wants back to main Frequency menu
          IRET= 1
        END IF
      IF (IRET.EQ.2) GO TO 10
C
C     turn off previous command
      I= 4
      CALL ZSTCMA (I,I0)
C
      IF (IPLOT.EQ.1 .AND. IGR.EQ.2) THEN
C       user wants graphics, but it is not available
        SGRP= 26
        CALL PRNTXT (MESSFL,SCLU,SGRP)
        IPLOT= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   LPINPT
     I                   (MESSFL,SCLU,WDMFL,DSN,CHGDAT,
     M                    NBYR,NEYR,
     O                    STANAM,STAID,NMO,NSM,NEM,NPARM,NMDAYS,NUMONS,
     O                    NZI,ILH,Y,RETCOD)
C
C     + + + PURPOSE + + +
C     This routine retrieves data for frequency analysis from a
C     WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,WDMFL,DSN,CHGDAT,NUMONS,NSM,
     1            NBYR,NEYR,NMO,NEM,NPARM,NMDAYS,NZI,ILH,RETCOD
      REAL        Y(120)
      CHARACTER*1 STANAM(48),STAID(16)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - cluster number on message file
C     WDMFL  - Fortran unit number of users WDM file
C     DSN    - WDM dataset number for analysis
C     CHGDAT - change data period flag
C              1 - use Full period for each data set
C              2 - use Common period for all data sets
C              3 - Specify period for each data set
C     NBYR   - begin year for analysis
C     NEYR   - end year for analysis
C     STANAM - station name
C     STAID  - station number
C     NMO    - number of months in season or period
C     NSM    - start month of season
C     NEM    - end month of season
C     NPARM  - number of non-zero years
C     NMDAYS - number of days for flow statistic
C     NUMONS - number of months for statistic
C     NZI    - number of years of zero events
C     ILH    - flag for statistic (0-low, 1-high, 0-month or other)
C     Y      - array of n-day hi or lo flows
C     RETCOD - return code
C             -1 - problem and user wants to stop all analysis
C              0 - ok
C              1 - error, couldn't get all input, skip data set
C              2 - problem with tsstep/tcode, not annual, skip data set
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,N,I0,I1,L3,L8,L48,SAIND,IVAL,SALEN,IRET,
     1            SDATE(6),EDATE(6),SGRP,OLEN,JUST,GPFLG,
     2            DREC,QFLG,CVAL(3),DTRAN,TUNITS,TSSTEP,
     3            LI,LC,LR,LVAL(2),RETC,BE(2),FLG,CTUTS(4),
     4            I88, CNUM, CVLU(1,3),L16,L2, ITMP(1)
      CHARACTER*1 TSTYPE(4),DFSTA(48),DFSTAN(16),TBUF(80),BLNK,BUFF(88)
      REAL        RVAL(1)
      DOUBLE PRECISION   DDUM
C
C     + + + FUNCTIONS + + +
      INTEGER     CHRINT
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRINT, WDBSGC, WDBSGI, WDTGET, WTFNDT, INTCHR
      EXTERNAL    ZIPC, PRNTXT, QRESPM, TIMDIF, CMPTIM
      EXTERNAL    ZSTCMA, ZGTRET, ZMNSST, PMXTXI, QRESCZ, COPYC
C
C     + + + DATA INITIALIZATION + + +
      DATA  I0, I1, L3, L8, L48, I88, BLNK, L16, L2
     #    /  0,  1,  3,  8,  48,  88, ' ',   16,  2/
      DATA DFSTA/'(','n','o',' ','s','t','a','t','i','o','n',' ',
     &           'd','e','s','c','r','i','p','t','i','o','n',' ',
     &           'o','n',' ','W','D','M',' ','f','i','l','e',')',
     &           12*' '/
      DATA DFSTAN /'(','n','o',' ','s','t','a','t','i','o','n',' ',
     &             '#',')',' ',' '/
C
C     + + + END SPECIFICATIONS + + +
C
      JUST  = 0
      TUNITS= 6
      TSSTEP= 1
      GPFLG = 0
      DREC  = 0
C     get station id name
      SAIND = 45
      SALEN = 48
      CALL WDBSGC (WDMFL,DSN,SAIND,SALEN,
     O             STANAM,RETCOD)
      IF (RETCOD.NE.0) THEN
C       station name attribute not found, write default string
        CALL COPYC (L48, DFSTA, STANAM)
      END IF
C
C     get station number
      SAIND = 51
      SALEN = 1
      CALL WDBSGI (WDMFL,DSN,SAIND,SALEN,
     O             IVAL,RETCOD)
      IF (RETCOD .EQ. 0) THEN
C       got it
        CALL INTCHR (IVAL,L8,JUST,
     O               OLEN,STAID)
        CALL ZIPC (L8, BLNK, STAID(9))
      ELSE
C       try character version
        SAIND = 2
        SALEN = 16
        CALL WDBSGC (WDMFL,DSN,SAIND,SALEN,
     O               STAID,RETCOD)
        IF (RETCOD .NE. 0) THEN
C         put in default statement if no station id
          CALL COPYC (L16, DFSTAN, STAID)
        END IF
      END IF
C
C     check tsstep and tcode for annual time series
      CTUTS(1) = 0
      CTUTS(2) = 0
      CTUTS(3) = 0
      CTUTS(4) = 0
      RETCOD = 0
      SALEN = 1
      SAIND = 17
      CALL WDBSGI (WDMFL,DSN,SAIND,SALEN,
     O             CTUTS(1),CTUTS(3))
      SAIND = 33
      CALL WDBSGI (WDMFL,DSN,SAIND,SALEN,
     O             CTUTS(2),CTUTS(4))
      IF (CTUTS(3).EQ.0 .AND. CTUTS(4).EQ.0) THEN
C       retrieved tsstep and tunits, check for annual time step
        CALL CMPTIM (CTUTS(1),CTUTS(2),TUNITS,TSSTEP,
     O               CTUTS(3),CTUTS(4))
        IF (CTUTS(3).NE.0 .OR. CTUTS(4).NE.0) RETCOD = 2
      ELSE
C       problem retrieving tsstep and/or tunits
        RETCOD = 2
      END IF
      IF (RETCOD .EQ. 0) THEN
C       get available dates
        CALL WTFNDT (WDMFL,DSN,GPFLG,DREC,
     O               SDATE,EDATE,RETCOD)
        NPARM = 0
        NZI = 0
      END IF
      IF (RETCOD .EQ. 0) THEN
C       set values
        IF (CHGDAT.NE.2) THEN
C         determine start and end date for data set
 10       CONTINUE
C           save default start and end years for data set
            BE(1) = SDATE(1)
            BE(2) = EDATE(1)
C           allow 'Prev' command
            I= 4
            CALL ZSTCMA (I,I1)
            IF (CHGDAT .EQ. 3) THEN
C             user specifying start and end for each data set
              SGRP = 19
C             for data set &
              ITMP(1) = DSN
              CALL PMXTXI (MESSFL,SCLU,SGRP,I1,I1,-I1,I1,ITMP)
C             save text
              CALL ZMNSST
              LI = 2
              LR = 1
              LC = 1
              SGRP = 9
              CALL QRESPM (MESSFL,SCLU,SGRP,LI,LR,LC,
     M                     BE,RVAL,CVAL,TBUF)
C             get user exit command value
              CALL ZGTRET (IRET)
            ELSE
C             use full period for each data set
              IRET= 1
            END IF
            IF (IRET.EQ.1) THEN
C             user wants to continue, check validity of dates
              NBYR= BE(1)
              NEYR= BE(2)
              FLG = 0
              IF (NBYR.LT.SDATE(1) .OR. NEYR.GT.EDATE(1)) THEN
C               bad start and end year specified
C               allow 'Intrpt' command also
                I= 16
                CALL ZSTCMA (I,I1)
                SGRP= 27
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                FLG= 1
              ELSE IF (NEYR.LT.NBYR+3 .AND. CHGDAT.EQ.3) THEN
C               time span not long enough, try again
C               allow 'Intrpt' command also
                I= 16
                CALL ZSTCMA (I,I1)
                SGRP= 28
                CALL PRNTXT (MESSFL,SCLU,SGRP)
                FLG= 1
              ELSE IF (NEYR.LT.NBYR+3) THEN
C               using full period for data sets, but this one is too short
                SGRP= 29
                CALL PRNTXT (MESSFL,SCLU,SGRP)
              END IF
              IF (FLG.EQ.1) THEN
C               had a problem, see what user wants to do
C               get user exit command value
                CALL ZGTRET (IRET)
                IF (IRET.EQ.1 .AND. CHGDAT.EQ.3) THEN
C                 user wants to try again
                  FLG = 1
                ELSE IF (IRET.EQ.2) THEN
C                 user wants to stop analysis
                  RETCOD= -1
                  FLG= 0
                ELSE IF (IRET.EQ.7) THEN
C                 user wants to skip this data set
                  RETCOD= 1
                  FLG= 0
                END IF
              END IF
            ELSE
C             user wants previous, take em back to main frequency menu
              RETCOD= -1
            END IF
C           turn off 'Prev' and 'Intrpt' commands
            I= 4
            CALL ZSTCMA (I,I0)
            I= 16
            CALL ZSTCMA (I,I0)
          IF (FLG .EQ. 1) GO TO 10
        END IF
C
        IF (RETCOD.EQ.0) THEN
C         continue with analysis, set start and end year
          SDATE(1) = NBYR
          EDATE(1) = NEYR
          CALL TIMDIF (SDATE,EDATE,TUNITS,TSSTEP,
     O                 N)
C
          IF (N .LE. 3) THEN
C           not enough data values
            RETCOD = 1
          ELSE
C           get data
            DTRAN = 0
            QFLG = 31
            CALL WDTGET (WDMFL,DSN,TSSTEP,SDATE,N,DTRAN,QFLG,TUNITS,
     O                   Y,RETCOD)
            IF (RETCOD .EQ. 0) THEN
C             get number of good years and non-zero years
              DO 20 I = 1,N
                IF (Y(I) .GE. 1.0E-9) THEN
                  NPARM = NPARM + 1
                  Y(NPARM) = Y(I)
                ELSE IF (Y(I) .GT. -1.0E-9) THEN
C                 zero defined as between -1.0E-9 and 1.0E-9
                  NZI = NZI + 1
                END IF
C               negative values ignored
 20           CONTINUE
C
C             get type of flow statistic
              SAIND = 1
              SALEN = 4
              CALL WDBSGC (WDMFL,DSN,SAIND,SALEN,
     O                     TSTYPE,RETCOD)
              NUMONS = 0
              NMDAYS = 0
              IF (TSTYPE(1) .EQ. 'H') THEN
                ILH = 1
                NMDAYS = CHRINT(L3,TSTYPE(2))
              ELSE IF (TSTYPE(1) .EQ. 'L') THEN
                ILH = 0
                NMDAYS = CHRINT(L3,TSTYPE(2))
              ELSE IF (TSTYPE(1) .EQ. 'M') THEN
C               monthly statistics
                ILH = 0
                NUMONS = CHRINT (L2,TSTYPE(3))
              ELSE
C               must ask user, LOW(1) or HIGH(2) flow statistic?
C               and # days in event
C               Duration of statistic not found on WDM dataset.
                SGRP = 10
                ITMP(1) = DSN
                CALL PMXTXI (MESSFL,SCLU,SGRP,I1,
     I                       I1,-I1,I1,ITMP)
C               Enter duration of statistic.
                SGRP = 11
                I1 = 1
                CNUM = 1
                CALL ZIPC (I88,BLNK,BUFF)
                CVLU(1,1) = 1
                IVAL = -999
                CALL QRESCZ (MESSFL,SCLU,SGRP,I1,I1,I1,CNUM,I1,I1,I88,
     &                       IVAL,RVAL,DDUM,CVLU,BUFF)
                ILH = CVLU(1,1) - 1
                NMDAYS = IVAL
              END IF
C
C             Get beg and end month of season
              SAIND = 256
              SALEN = 1
              CALL WDBSGI (WDMFL,DSN,SAIND,SALEN,
     O                     NSM,RETC)
              IF (RETC .NE. 0) NSM = -999
              SAIND = 257
              SALEN = 1
              CALL WDBSGI (WDMFL,DSN,SAIND,SALEN,
     O                     NEM,RETC)
              IF (RETC.NE.0) NEM = -999
              IF (NSM.LT.0 .OR. (NEM.LT.0 .AND. TSTYPE(1).NE.'M'))THEN
C               must get season from user
C               enter starting and ending month of season
                LVAL(1) = NSM
                LVAL(2) = NEM
                LI  = 2
                LR  = 1
                LC  = 1
                SGRP= 31
                CALL QRESPM (MESSFL,SCLU,SGRP,LI,LR,LC,
     M                       LVAL,RVAL,CVAL,TBUF)
                NSM = LVAL(1)
                NEM = LVAL(2)
              END IF
C
              IF (TSTYPE(1) .EQ. 'M') THEN
C               special case of monthly statistic
                NEM = NSM + NUMONS - 1
                IF (NEM .GT. 12) NEM = NEM - 12
                NMO = NUMONS
              ELSE
C               compute length of season in months for other cases
                NMO = NEM
                IF (NEM .GE. NSM) THEN
                  NMO = NMO + 1 - NSM
                ELSE
                  NMO = NMO + 13 - NSM
                END IF
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
      SUBROUTINE   FPLOT
     I                   (MESSFL,SCLU,X,SZ,C,SE,N,NZ,ILH,
     I                    NMDAYS,STATN,GDEVTY,LOGARH,NSM,NEM)
C
C     + +  PURPOSE + + +
C     Fill graphics common block with default values for
C     Log-Pearson type plot.
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
     2            GSY(2),GPT(2),GTRAN(4),GCL(2),LX,IMIN,IMAX,
     3            GDEVCD,IND,GTICS(4),GBVALF(4),L20,IIMAX,IIMIN
      REAL        TMP(120),Z,VMIN(4),VMAX(4),GLOC(2),R0,
     1            GSIZEL,GXPAGE,GYPAGE,GXPHYS,GYPHYS,GXLEN,GYLEN,
     2            GPLMN(4),GPLMX(4),
     3            XMIN,XMAX,YMIN,YMAX
      CHARACTER*1 CDUM(80),BLNK,CY(80),CX(80),GTITL(240),GLB(20,4),
     1            CD(5),CF(4),CL(4),CH(5),
     2            MC(12,3)
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
      EXTERNAL    GPLBXB
C
C     + + + DATA INITIALIZATION + + +
      DATA   I1, I2, I3, I4, I5, I80, I240,BLNK, R0
     #    /   1,  2,  3,  4,  5,  80,  240, ' ',0.0/
      DATA GCV/6,6/, GLN/0,1/, GSY/4,0/, GPT/0,0/, GCL/1,1/, L20/20/
      DATA CL/'l','o','w',' '/,  CH/'h','i','g','h',' '/
     &     CF/'f','l','o','w'/,  CD/'-','d','a','y',' '/
      DATA GBVALF,GTICS/4*1,4*10/
      DATA GSIZEL, GXPAGE, GYPAGE, GXPHYS, GYPHYS, GXLEN, GYLEN
     1    /  0.11,   10.0,    8.0,    1.5,    1.5,   7.5,   5.0/
      DATA MC/'J','a','n','F','e','b','M','a','r','A','p','r',
     &        'M','a','y','J','u','n','J','u','l','A','u','g',
     &        'S','e','p','O','c','t','N','o','v','D','e','c'/
C
C    + + + OUTPUT FORMATS + + +
C    (keep in code for general information)
C 400 FORMAT ('0','THE FOLLOWING DATA VALUE ',F13.3,' WITH ASSOCIATED ',
C    1'PROBABILITY OF ',F7.4,' WAS NOT PLOTTED'/)
C 410 FORMAT ('0','THE FOLLOWING COMPUTED VALUE ',F13.3,' WITH ASSOCIAT'
C    1,'ED PROBABILITY OF ',F7.4,' WAS NOT PLOTTED'/)
C 420 FORMAT (' ',60X,'PROBABILITY'/' ',7X,'0.995 0.99',11X,'0.95',4X,'0
C    1.90',5X,'0.80',14X,'0.50',14X,'0.20',5X,'0.1',7X,'0.04',3X,'0.02',
C    22X,'0.01',1X,'0.005',1X)
C 450 FORMAT (' ',7X,'1.005 1.01',11X,'1.05',4X,'1.11',5X,'1.25',15X,'2'
C    1,17X,'5',7X,'10',8X,'25',5X,'50',4X,'100',2X,'200'/' ',56X,'RECURR
C    2ENCE INTERVALS',1X//)
C 460 FORMAT ('0','THE FOLLOWING SYMBOLS MAY APPEAR IN THE PLOT'/' ','X
C    1- AN INPUT DATA VALUE'/' ','* - A CALCULATED VALUE'/' ','O - A CAL
C    2CULATED VALUE AND ONE DATA VALUE AT SAME POSITION'/' ','2 - TWO IN
C    3PUT DATA VALUES PLOTTED AT SAME POSITION'/' ','3 - THREE INPUT DAT
C    4A VALUES PLOTTED AT SAME POSITION'/' ','A - A CALCULATED VALUE AND
C    5 TWO DATA VALUES AT SAME POSITION'/' ','B - A CALCULATED VALUE AND
C    6 THREE DATA VALUES AT SAME POSITION')
C 470 FORMAT ('0','NOTE -- THE INPUT DATA VALUES ARE BASED ON NON-ZERO V
C    1ALUES AND TOTAL (NON-ZERO + ZERO VALUES) SAMPLE SIZE.')
C 480 FORMAT (' ',8X,'THE CALCULATED VALUES ARE BASED ON ADJUSTED (UNCON
C    1DITIONAL) PARAMETER VALUES.')
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
      SGRP= 21
      LEN = 80
      CALL GETTXT (MESSFL,SCLU,SGRP,LEN,CY)
      CALL ZIPC (I240,BLNK,GTITL)
      LEN = 80
      CALL CHRCHR (LEN, STATN, GTITL)
      CALL GPLABL (LX,LL,R0,CY,CX,CDUM,GTITL)
C     also x-axis label
      CALL GPLBXB (CX)
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
C     add 'flow'
      CALL CHRCHR (I4,CF,GLB(LOC,1))
C
      CALL GPCURV (GCV,GLN,GSY,GCL,GPT,GLB)
C     set plot sizes
      CALL GPSIZE (GSIZEL,GXPAGE,GYPAGE,GXPHYS,GYPHYS,GXLEN,GYLEN,R0)
C
      RETURN
      END

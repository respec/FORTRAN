      SUBROUTINE STATS
C=======================================================================
C                 STATISTICAL ANALYSIS BLOCK WRITTEN BY
C
C                           DONALD J. POLMANN
C                   ENVIRONMENTAL ENGINEERING SCIENCES
C                         UNIVERSITY OF FLORIDA
C                         GAINESVILLE,  FLORIDA
C       JULY 1981         (UPDATED JANUARY 1983)
C       OCTOBER 1985      (REVISED BY BOB DICKINSON)
C       OCTOBER 1988      (REVISED BY BOB DICKINSON)
C       MAY     1989      (REVISED BY BOB DICKINSON)
C       SEPTEMBER 1990    (REVISED BY LAURA TERRELL, CDM)
C       December 1990     (REVISED BY BOB DICKINSON)
C       December 1992     (Zero divide check, WCH)
C       July 1993         WCH. Labels for return period in years, 
C                           and miscellaneous format corrections.
C                         Use average and max flow values of cfs
C                           and cms, not cf/hr or m^3/hr.
C                         Make sure tables are sorted before plotting.
C                         Warn about small return periods. 
C       November 1993     Correction for metric conversions and make
C                         summations consistent with other blocks
C                         regarding time step averaging, WCH.
C=======================================================================
C     THE STATS BLOCK WILL PERFORM STATISTICAL ANALYSES ON EVENTS OF
C     STORMWATER FLOW AT ONE CHOSEN LOCATION.  THE OPTIONS AVAILABLE
C     INCLUDE A TABLE OF MAGNITUDE,   RETURN PERIOD AND FREQUENCY, A
C     GRAPH OF MAGNITUDE VS. RETURN PERIOD, A GRAPH OF MAGNITUDE VS.
C     FREQUENCY, AND THE FIRST THREE MOMENTS OF THE EVENT DATA.  ANY
C     OR ALL OF THESE OPTIONS CAN BE CHOSEN FOR ANY OR ALL OF FIVE
C     RAINFALL PARAMETERS (VOLUME, AVERAGE INTENSITY, PEAK INTENSITY,
C     EVENT DURATION, AND INTEREVENT DURATION).  ANY
C     OR  ALL OF THESE OPTIONS CAN BE CHOSEN FOR ANY OR  ALL OF FIVE
C     FLOW PARAMETERS (TOTAL FLOW,  AVERAGE FLOW,  PEAK FLOW,  EVENT
C     DURATION,  INTEREVENT DURATION)  AND FIVE POLLUTANT PARAMETERS
C     (TOTAL LOAD,  AVERAGE LOAD,  PEAK LOAD,  FLOW WEIGHTED AVERAGE
C     CONCENTRATION,PEAK CONCENTRATION). THE FLOW/POLLUTANT DATA ARE
C     SEPARATED INTO EVENTS ON THE BASIS OF FLOW RATE WHETHER OR NOT
C     ANY STATISTICAL  OPTIONS ARE SELECTED FOR FLOW.   FROM ZERO TO
C     TEN POLLUTANTS MAY BE ANALYZED.
C=======================================================================
C     THE  STATS BLOCK  MAY BE CALLED AFTER ANY BLOCK THAT GENERATES
C     AN INTERFACE FILE OF FLOW AND POLLUTANT DATA.  BESIDES INSTAN-
C     TANEOUS VALUES FOR FLOW RATE,  THE FILE MAY CONTAIN  UP TO TEN
C     POLLUTANTS.  THE NUMBER OF LOCATIONS IN THE FILE IS LIMITED TO
C     200.  THE NUMBER OF TIME STEPS IS NOT DIRECTLY CONSTRAINED BUT
C     THE NUMBER OF EVENTS THAT CAN BE SORTED AND ANALYZED IS LIMIT-
C     ED TO 4000. THE NUMBER OF EVENTS IN A GIVEN TIME SERIES IS DE-
C     PENDENT ON THE USER-DEFINED MINIMUM INTEREVENT TIME. THIS LIM-
C     IT OF 4000  EVENTS CAN BE EXTENDED (OR DECREASED)  BY THE USER
C     BY ALTERING THE PARAMETER STATEMENT FOR LIMIT (E.G., 
C     LIMIT=4000) IN STCOM.INC, CONTAINING THE STATS BLOCK 
C     COMMON GROUPS. 
C=======================================================================
C     THE  STATS BLOCK  INCLUDES THE SUBROUTINES 'STATS',  'SBTABL',
C     'MOMENT',   'SORT',  'POINTS',  AND 'STBLOCK'.  OTHER SWMM SUB-
C     ROUTINES CALLED INCLUDE 'CURVE'.
C=======================================================================
C
C     ATTENTION VAX PROGRAMMERS:  YOU MAY ENCOUNTER AN OUTPUT OVERFLOWS
C     RECORD ERROR BECAUSE OF THE LENGTH OF THE FORMAT STATEMENT (IE. 3550)
C     A SOLUTION TO THIS MAY BE TO MODIFY THE FDL FILE 
C     ASSOCIATED WITH YOUR OUTPUT FILE TO INCREASE RECORD LENGTH.
C
C
C========================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'LAB.INC'
      INCLUDE 'STCOM.INC'
C=======================================================================
      DIMENSION FLOWP(5),POLLP(10,5),Q(NIE),POLL(10,NIE),PCONV(10),
     1          IFPAR(5),IPPAR(10,5),ISFLOW(10,6),ISPOLL(10,5,5)
C#### WCH, 7/21/93. INCREASE DIMENSION OF RPHOR TO 3.
      DIMENSION RPHOR(3),PHOR(2),VRTITL(15),SICONV(9,2),JSTA(10)
      REAL MIT
C#### WCH, 7/21/93. MAKE NEWLAB BE 8 BYTES TO AGREE WITH ENGLAB.
      CHARACTER RPTITL*30,PTITL*30,BLANK*10,PHOR*30,
     1          BMJ*10,KOCRQ*10,RPHOR*30,VRTITL*10,NEWLAB(4)*8,
     2          COND*3,OLD*3,QUAN(3)*8,OTHER(4)*8,PAA(10)*8
C=======================================================================
      DATA SICONV/9*1.0,3*25.4,3*0.4536,3*28.32/
      DATA RPTITL/'Magnitude vs. Return period'/
      DATA PTITL /'Magnitude  vs.  Frequency  '/
C#### WCH, 7/21/93.  ADD LABEL FOR YEARS.
      DATA RPHOR/' Base 10 Log of Return period',' (Log months)  ',
     +                                           ' (Log years)   '/
      DATA PHOR/'  Percent of occurrences less',
     +          'than/equal to given magnitude'/
      DATA VRTITL/' Total  Q ',' Avg   Q  ',
     +            '  Peak  Q ',' Duration ',
     +            'Interevent','Total load',
     +            ' Avg load ','Peak load ',
     +            ' Avg conc.','Peak conc.',
     +            ' Total V  ','Avg  int. ',
     +            ' Peak int.',
     +            ' Duration ','Interevent'/
      DATA QUAN/'Quantity','Quan/hr ','Quan/hr '/
      DATA OTHER/'ft3*unit','cfs*unit','lit*unit','l/s*unit'/
      DATA BLANK/'          '/,BMJ/'          '/
C#### WCH, 7/21/93.  UNITS OF CFS AND CMS AND 8 BYTES.
      DATA NEWLAB/'feet^3  ',' cfs    ','meter^3 ','m^3/sec '/
C=======================================================================
C     Label the graphs in Subroutine Curve.
C=======================================================================
      VERT1  = BLANK
      VERT2  = BLANK
      VERT3  = BLANK
      INCNT  = INCNT  + 1
      IOUTCT = IOUTCT + 1
      LAST   = JIN(INCNT)
      NOUT   = JOUT(IOUTCT)
      IF(LAST.LE.0) CALL ERROR(130)
      IF(NOUT.LE.0) CALL ERROR(131)
      WRITE(*,34)
      WRITE(N6,34)
C=======================================================================
C     Open all input/output files for the Statistics Block.
C=======================================================================
      IF(JIN(INCNT).GT.0.AND.(FFNAME(INCNT).EQ.'JOT.UF'.OR.
     +      FFNAME(INCNT).EQ.'JIN.UF'))
     +      OPEN(JIN(INCNT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JIN(INCNT).GT.0.AND.FFNAME(INCNT).NE.'JOT.UF'.AND.
     +      FFNAME(INCNT).NE.'JIN.UF')
     +      OPEN(JIN(INCNT),FILE=FFNAME(INCNT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
      IF(JOUT(IOUTCT).GT.0.AND.(FFNAME(25+IOUTCT).EQ.'JOT.UF'.OR.
     +      FFNAME(25+IOUTCT).EQ.'JIN.UF'))
     +      OPEN(JOUT(IOUTCT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JOUT(IOUTCT).GT.0.AND.FFNAME(25+IOUTCT).NE.'JOT.UF'.AND.
     +      FFNAME(25+IOUTCT).NE.'JIN.UF')
     +      OPEN(JOUT(IOUTCT),FILE=FFNAME(25+IOUTCT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
C=======================================================================
      REWIND NOUT
      REWIND LAST
C=======================================================================
C     Initialize several variables and arrays.
C=======================================================================
      COND       = 'DRY'
      OLD        = 'DRY'
      NNEND      = 0
      NEXIT      = 0
      IPEVNT     = 0
      IDATEZ     = 0
      DRYTS      = 0.0
      TZERO      = 0.0
      HSEC       = 3600.0
      DO 5 I     = 1,5
      IFPAR(I)   = 0
5     FLOWP(I)   = 0.0
      DO 6 I     = 1,5
      DO 6 J     = 1,10
      POLLP(J,I) = 0.0
6     IPPAR(J,I) = 0
C=======================================================================
C >>>>Read Data Group A1 <<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,ISTART,TSTART,IEND,TEND,INLOG,JCUBE
C=======================================================================
C>>>>>>>>>> Read Data Group B1 <<<<<<<<<<<<<<<<<
C=======================================================================
      IF(JCE.EQ.0) THEN
                   READ(N5,*,ERR=888) CC,MIT,BASE,EBASE,LOCRQ,
     +                                LOCRN,NPR,NPOINT,METRIC,LRET,A
                   ELSE
                   READ(N5,*,ERR=888) CC,MIT,BASE,EBASE,KOCRQ,
     +                                LOCRN,NPR,NPOINT,METRIC,LRET,A
                   IF(KOCRQ.NE.' ') LOCRQ = 1
                   ENDIF
      IF(LOCRN.GT.0) INLOG = 0
      WRITE(N6,92) MIT,NPOINT,METRIC,LRET,A,INLOG,JCUBE
      METRIC = METRIC + 1
      IF(LOCRQ.GT.0.AND.JCUBE.GT.0) THEN
                                    ENGLAB(1) = NEWLAB(1)
                                    ENGLAB(2) = NEWLAB(2)
                                    ENGLAB(3) = NEWLAB(2)
                                    SILAB(1)  = NEWLAB(3)
                                    SILAB(2)  = NEWLAB(4)
                                    SILAB(3)  = NEWLAB(4)
                                    ENDIF
C=======================================================================
C>>>>>>> Read Data Group B2 <<<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,KSEQ,KTERM,KTSEQS
      WRITE(N6,93)          KSEQ,KTERM,KTSEQS
C=======================================================================
C >>>>>>>  Read Data Group B3 <<<<<<<<<<<<<<<
C=======================================================================
      IF(NPR.GT.0) READ(N5,*,ERR=888) CC,(IPOLRQ(K),K=1,NPR)
C=======================================================================
C >>>>>>>>>>> Read Data Group C1 <<<<<<<<<<<<<<<<<<<
C=======================================================================
      IF(LOCRQ.GT.0) THEN
                     CALL INFACE(1,LAST)
                     READ(N5,*,ERR=888) CC,(ISFLOW(1,J),J=1,5)
                     DO 200 J = 1,5
                     I1 = ISFLOW(1,J)/1000
                     J2 = ISFLOW(1,J) - I1*1000
                     I2 = J2/100
                     J3 = J2 - I2*100
                     I3 = J3/10
                     I4 = J3 - I3*10
                     ISFLOW(2,J) = I1
                     ISFLOW(3,J) = I2
                     ISFLOW(4,J) = I3
                     ISFLOW(5,J) = I4
                     IF(I1.EQ.1.OR.I2.EQ.1.OR.I3.EQ.1.OR.I4.EQ.1)
     +                                              IFPAR(J) = 1
  200                CONTINUE
                     ENDIF
C=======================================================================
C >>>> Read Data Group D1 <<<<<<<<<<<<<<<<<
C=======================================================================
      IF(NPR.GT.0) THEN
                   DO 250 J = 1,NPR
                   READ(N5,*,ERR=888) CC,(ISPOLL(J,1,K),K=1,5)
                   DO 275 K = 1,5
                   I1 = ISPOLL(J,1,K)/1000
                   J2 = ISPOLL(J,1,K) - I1*1000
                   I2 = J2/100
                   J3 = J2 - I2*100
                   I3 = J3/10
                   I4 = J3 - I3*10
                   ISPOLL(J,2,K)  = I1
                   ISPOLL(J,3,K)  = I2
                   ISPOLL(J,4,K)  = I3
                   ISPOLL(J,5,K)  = I4
                   IF(I1.EQ.1.OR.I2.EQ.1.OR.I3.EQ.1.OR.I4.EQ.1)
     +                                          IPPAR(J,K) = 1
  275              CONTINUE
  250              CONTINUE
                   ENDIF
C=======================================================================
C >>>>>>>>>>> Read Data Group E1 <<<<<<<<<<<<<<<<<<<
C=======================================================================
      IF(LOCRN.GT.0) THEN
                     REWIND LAST
                     READ(LAST,ERR=9070) NSTA,MRAIN,(JSTA(I),I=1,NSTA)
                     WRITE(N6,2115)  NSTA
                     WRITE(N6,2120) (I,JSTA(I),I=1,NSTA)
                     DO 290 I = 1,NSTA
                     IF(LOCRN.EQ.JSTA(I)) THEN
                                          MSTA = I
                                          GO TO 295
                                          ENDIF
 290                 CONTINUE
                     WRITE(N6,9030) LOCRN
 295                 CONTINUE
                     READ(N5,*,ERR=888) CC,(ISFLOW(6,J),J=1,5)
                     DO 300 J = 1,5
                     I1 = ISFLOW(6,J)/1000
                     J2 = ISFLOW(6,J) - I1*1000
                     I2 = J2/100
                     J3 = J2 - I2*100
                     I3 = J3/10
                     I4 = J3 - I3*10
                     ISFLOW(7,J)  = I1
                     ISFLOW(8,J)  = I2
                     ISFLOW(9,J)  = I3
                     ISFLOW(10,J) = I4
                     IF(I1.EQ.1.OR.I2.EQ.1.OR.I3.EQ.1.OR.I4.EQ.1)
     +                                              IFPAR(J) = 1
  300                CONTINUE
                     NPR = 0
                     ENDIF
C=======================================================================
C     Set up pollutant conversions.
C=======================================================================
      IF(NPR.GT.0) THEN
                   DO 350 K = 1,10
                   PAA(K)   = BLANK
                   IF(K.GT.NPR) GO TO 350
                   JJ       = IPOLRQ(K)
                   IF(METRIC.EQ.1) THEN
C=======================================================================
C        16018.93 is equal to 0.0353157 ft/l * 453592.4 mg/lb
C        3.53157E-2 = ft3/l = 1/28.31605
C=======================================================================
                           PCONV(JJ) = 16018.93
                           PAA(K)    = ENGLAB(6)
                           IF(NDIM(JJ).EQ.1) PCONV(JJ) = 3.53157E-2
                           IF(NDIM(JJ).EQ.1) PAA(K)    = QUAN(1)
                           IF(NDIM(JJ).GE.2) PAA(K)    = OTHER(2)
                           ELSE
                           PCONV(JJ) = 1000.0
                           PAA(K)    = SILAB(6)
                           IF(NDIM(JJ).EQ.1) PAA(K)    = QUAN(1)
                           IF(NDIM(JJ).EQ.1) PCONV(JJ) = 0.001
                           IF(NDIM(JJ).GE.2) PAA(K)    = OTHER(4)
                           ENDIF
                   IF(NDIM(JJ).GE.2) PCONV(JJ) = 1.0
  350              CONTINUE
                   ENDIF
C=======================================================================
C     Summarize input data.
C=======================================================================
      WRITE(N6,100) ISTART,TSTART,IEND,TEND
      TSTART = TSTART * 3600.0
      TEND   = TEND   * 3600.0
      IF(ISTART.GT.0) THEN
                      NYEAR  = ISTART/10000
                      NDAY   = ISTART - NYEAR*10000
                      MONTH  = NDAY/100
                      NDAY   = NDAY - MONTH*100
                      IF(NDAY.EQ.0)   NDAY = 1
                      IF(NDAY.GT.31)  NDAY = 1
                      IF(MONTH.EQ.0)  MONTH = 1
                      IF(MONTH.GT.12) MONTH = 1
                      ISTART = 1000*NYEAR + JDATE(NDAY,MONTH,NYEAR)
                      ENDIF
      IF(IEND.GT.0) THEN
                    NYEAR  = IEND/10000
                    NDAY   = IEND - NYEAR*10000
                    MONTH  = NDAY/100
                    NDAY   = NDAY - MONTH*100
                    IF(NDAY.EQ.0)   NDAY = 12
                    IF(NDAY.GT.31)  NDAY = 12
                    IF(MONTH.EQ.0)  MONTH = 12
                    IF(MONTH.GT.12) MONTH = 12
                    IEND   = 1000*NYEAR + JDATE(NDAY,MONTH,NYEAR)
                    ENDIF
      IF(LOCRQ.GT.0) THEN
                     JULDAY = IDATEZ
                     TIMDAY = TZERO
                     ENDIF
      WRITE(N6,101) ISTART,TSTART,IEND,TEND
C=======================================================================
      IF((ISTART.EQ.0.AND.TSTART.EQ.0.0).OR.(IEND.EQ.0.AND.TEND.
     +        EQ.0.0)) WRITE(N6,102)
      WRITE(N6,105) MIT
      IF(JCE.EQ.0) WRITE(N6,110)  LOCRQ
      IF(JCE.EQ.1) WRITE(N6,1110) KOCRQ
      WRITE(N6,111) LOCRN
      MIT         = MIT*3600.0
      IF(METRIC.EQ.1) THEN
                      WRITE(N6,113)
                      WRITE(N6,112) NPR,BASE,EBASE
                      ELSE
                      WRITE(N6,115)
                      WRITE(N6,1112) NPR,BASE,EBASE
                      ENDIF
C=======================================================================
C     Print pollutants requested, by number, to indicate
C     correct/incorrect choices by user.
C=======================================================================
      IF(NPR.GT.0) WRITE(N6,114) (IPOLRQ(I),I=1,NPR)
C=======================================================================
C     Print matrices indicating Stat options for flow and pollutants.
C=======================================================================
      IF(LOCRQ.GT.0) WRITE(N6,118) ((ISFLOW(I,J),J=1,5),I=2,5)
      IF(LOCRN.GT.0) WRITE(N6,119) ((ISFLOW(I,J),J=1,5),I=7,10)
      IF(NPR.GT.0) THEN
                   DO 130 J = 1,NPR
                   KK       = IPOLRQ(J)
                   WRITE(N6,120) PNAME(KK),((ISPOLL(J,I,K),K=1,5),I=2,5)
130                CONTINUE
                   ENDIF
C=======================================================================
C     Find the position on the interface file of the requested number.
C=======================================================================
      KK = MSTA
      IF(LOCRQ.GT.0) THEN
                     DO 140 J = 1,LOCATS
                     IF(JCE.EQ.0.AND.LOCRQ.EQ.NLOC(J)) GO TO 145
                     IF(JCE.EQ.1.AND.KOCRQ.EQ.KAN(J))  GO TO 145
140                  CONTINUE
                     IF(JCE.EQ.0) WRITE(N6,142) LOCRQ
                     IF(JCE.EQ.1) WRITE(N6,143) KOCRQ
                     RETURN
145                  KK = J
                     ENDIF
C=======================================================================
C     Initialize read counter to zero.
C     Initialize event counter to zero.
C     Used to find first date/time on file.
C     If zero value for starting date/time is chosen.
C     Initialize time counter to one.  Used to set T1
C     (Beginning of elapsed time for the period of analysis)
C=======================================================================
      NEVNTS = 0
      KREAD  = 0
      KTIME  = 1
      TIME   = TZERO
C#### WCH, 11/30/93.
      ITEST  = 0
      DLAST  = 0.0
C=======================================================================
C     Read flow (THEN) or rainfall (ELSE) interface files.
C=======================================================================
      WRITE(N6,375)
      WRITE(*,911)
      WRITE(*,913)
      WRITE(*,914) NEVNTS + 1
 400  IF(LOCRQ.GT.0) THEN
                     IF(NQUAL.EQ.0) READ(LAST,END=2005)
     +                  JULDAY,TIMDAY,DELTA,(Q(K),K=1,LOCATS)
                     IF(NQUAL.GT.0) READ(LAST,END=2005) JULDAY,TIMDAY,
     +                  DELTA,(Q(K),(POLL(J,K),J=1,NQUAL),K=1,LOCATS)
                     ELSE
                     JDAY  = JULDAY
                     TMDAY = TIMDAY
                     READ(LAST,END=2005,ERR=9070) JULDAY,TIMDAY,
     +                                   THISTO,(Q(K),K=1,NSTA)
                     ENDIF
      KREAD = KREAD + 1
      IF(LOCRQ.GT.0.AND.DELTA.EQ.0.0) GO TO 400
C=======================================================================
C     Check for ending date/time.
C=======================================================================
      IF(JULDAY.EQ.IEND.AND.TIMDAY.GT.TEND.AND.IEND.NE.0) GO TO 2000
      IF(JULDAY.GT.IEND.AND.IEND.NE.0)                    GO TO 2000
C=======================================================================
C     Check for default value for starting date/time.
C=======================================================================
      IF(ISTART.EQ.0.AND.TSTART.EQ.0.0) GO TO 410
C=======================================================================
C     Find desired starting date/time on interface file.
C=======================================================================
      IF(JULDAY.LT.ISTART) GO TO 400
      IF(JULDAY.LE.ISTART.AND.TIMDAY.LT.TSTART) GO TO 400
C=======================================================================
C     Establish starting point for period of analysis.
C=======================================================================
  410 IF(KTIME.EQ.1) THEN
                     T1    = TIME
                     JDAY  = JULDAY
                     TMDAY = TIMDAY
                     IF(LOCRN.GT.0) THEN
                                    IDATEZ = JULDAY
                                    TZERO  = TIMDAY
                                    ENDIF
                     ENDIF
      IF(LOCRN.GT.0) THEN
                     CALL NTIME(JDAY,TMDAY,DELT)
                     DELTA = -DELT
                     ENDIF
      TOLD  = TIME
      TIME  = TIME + DELTA
C=======================================================================
C#### WCH, 11/30/93.  MAKE SUMMATIONS CONSISTENT WITH OTHER BLOCKS.
C                     USE DELT FOR MULTIPLICATION FOR VOLUMES/LOADS IF
C                     PREVIOUS TIME STEP WAS DRY.  OTHERWISE, USE
C                     AVERAGE DELT = DMEAN.
C=======================================================================
      IF(LOCRQ.GT.0) THEN
           DMEAN = 0.5 * (DELTA + DLAST)
           IF(ITEST.EQ.0) DMEAN = DELTA
           DLAST = DELTA
           ITEST = 0
           IF(Q(KK).GT.0.0) ITEST = 1
           ELSE
           DMEAN = DELTA
           ENDIF
C
      CALL DATED
C=======================================================================
C     Print starting date/time if default value is chosen.
C     Establish starting point for period of analysis.
C=======================================================================
      IF(KREAD.EQ.1) THEN
                     IEVNTB(1) = JULDAY
                     TEVNTB(1) = TIMDAY/3600.0
                     WRITE(N6,415) JULDAY,TIMDAY
                     T1        = TZERO
                     ENDIF
      KTIME = KTIME + 1
C=======================================================================
C     Separate time series into events.  Event consists of at least
C     one wet time step; interevent is a dry period at least as long
C     as the minimum interevent time chosen.
C=======================================================================
C     If Q = 0.0, Time step is dry;  If Q > 0, Time step is wet.
C=======================================================================
C     Rainfall interface file.
C=======================================================================
      IF(LOCRN.GT.0) THEN
                     NEXT = 1
                     IF(TIME-TOLD.GE.MIT) THEN
                                          NEXT     = 2
                                          BACKSPACE LAST
                                          FLOWP(4) = DURAT
                                          FLOWP(5) = DELTA
                                          ENDIF
                     IF(DELTA.EQ.0.0) NEXT = 3
                     ENDIF
C=======================================================================
C     Flow and water quality interface file.
C=======================================================================
      IF(LOCRQ.GT.0) THEN
                     IF(Q(KK).GT.BASE) THEN
                                       COND = 'WET'
                                       ELSE
                                       COND = 'DRY'
                                       ENDIF
                     IF(OLD.EQ.'WET'.AND.COND.EQ.'WET')   NEXT = 1
                     IF(OLD.EQ.'WET'.AND.COND.EQ.'DRY')   NEXT = 2
                     IF(OLD.EQ.'DRY'.AND.COND.EQ.'WET')   NEXT = 3
                     IF(OLD.EQ.'DRY'.AND.COND.EQ.'DRY')   NEXT = 4
                     IF(NEXT.EQ.3.AND.DRYTS+DELTA.LT.MIT) NEXT = 1
                     IF(MIT.EQ.0.0)                       NEXT = 3
                     OLD = COND
                     ENDIF
 1350 CONTINUE
C=======================================================================
C     Previous condition is wet; New condition is wet;
C     Continue the event; increment event duration.
C=======================================================================
      IF(NEXT.EQ.1) THEN
                    IF(LOCRQ.GT.0) THEN
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELTA.
                             FLOWP(1) = FLOWP(1) + Q(KK)*DMEAN
C#### WCH, 7/21/93.  USE UNITS OF CFS OR CMS, NOT PER HOUR
C                            IF(Q(KK)*HSEC.GT.FLOWP(3))
C    +                                    FLOWP(3) = Q(KK)*HSEC
                             IF(Q(KK).GT.FLOWP(3))
     +                                    FLOWP(3) = Q(KK)
                             ENDIF
                    IF(LOCRN.GT.0) FLOWP(1) = FLOWP(1) +
     +                                        Q(KK)*THISTO/3600.0
                    DURAT = DURAT + DELTA
                    DRYTS = 0.0
                    ENDIF
C=======================================================================
C     Flow parameter 1 is Total flow.
C     Flow parameter 2 is Average flow.
C     Flow parameter 3 is Peak flow.
C     Flow parameter 4 is Event duration.
C     Flow parameter 5 is Interevent duration.
C=======================================================================
C     Previous condition is wet; New condition is dry.
C=======================================================================
      IF(NEXT.EQ.2) THEN
                    DURAT    = DURAT + DELTA
                    DRYTS    = DELTA
                    IF(LOCRQ.GT.0) FLOWP(4) = DURAT
                    ENDIF
C=======================================================================
C     Previous condition is dry; New condition is wet; New event.
C=======================================================================
      IF(NEXT.EQ.3) THEN
                                 NEVNTS = NEVNTS + 1
                    WRITE(*,914) NEVNTS
                    IPEVNT         = 0
                    IEVNTB(NEVNTS) = NYEAR*10000 + MONTH*100 + NDAY
                    TEVNTB(NEVNTS) = TIMDAY/3600.0
                    DURAT          = DELTA
                    FLOWP(4)       = DURAT
                    IF(LOCRQ.GT.0) THEN
                                   DRYTS    = DRYTS+DELTA
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELTA.
                                   FLOWP(1) = Q(KK)*DMEAN
C#### WCH, 7/21/93
C                                  FLOWP(3) = Q(KK)*DELTA
                                   FLOWP(3) = Q(KK)
                                   FLOWP(5) = DRYTS
                                   ENDIF
                    IF(LOCRN.GT.0) THEN
                                   FLOWP(1) = Q(KK)*THISTO/3600.0
                                   FLOWP(3) = Q(KK)
                                   DURAT    = THISTO
                                   FLOWP(4) = DURAT
                                   ENDIF
                    DRYTS    = 0.0
                    IF(MIT.EQ.0.0.AND.LOCRQ.GT.0) THEN
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELTA.
                                   FLOWP(1) = Q(KK)*DMEAN
C#### WCH, 7/21/93
C                                  FLOWP(2) = FLOWP(1)
C                                  FLOWP(3) = FLOWP(1)
                                   FLOWP(2) = Q(KK)
                                   FLOWP(3) = Q(KK)
                                   FLOWP(4) = DURAT
                                   ENDIF
                    IF(MIT.EQ.0.0.AND.LOCRN.GT.0) THEN
                                   IF(THISTO.EQ.0.0) THISTO = 1.0
                                   FLOWP(1) = Q(KK)*THISTO/3600.0
                                   FLOWP(2) = FLOWP(1)
                                   FLOWP(3) = FLOWP(1)
                                   FLOWP(4) = DURAT
                                   ENDIF
                    ENDIF
C=======================================================================
C      Previous condition is dry.
C      New condition is dry.        Extend interevent time.
C=======================================================================
      IF(NEXT.EQ.4) THEN
                    DRYTS = DRYTS + DELTA
                    IF(DRYTS.LT.MIT) DURAT = DURAT + DELTA
                    ENDIF
C=======================================================================
C     Calculation of pollutant parameters.
C     If condition is dry, pollutant parameters will be unchanged.
C=======================================================================
C     Pollutant Parameter 1 is Total Load
C                         2 is Average Load
C                         3 is Peak Load
C                         4 is Flow Weighted Average Concentration = 
C                               Event Mean Concentration (EMC)
C                         5 is Peak Concentration
C=======================================================================
      IF(NEXT.EQ.2.OR.NEXT.EQ.4) GO TO 1500
      IF(NPR.EQ.0) GO TO 1500
      DO 1400 K = 1,NPR
      JJ        = IPOLRQ(K)
C=======================================================================
C     Special exception to routine if MIT = 0.
C=======================================================================
       IF(MIT.EQ.0) THEN
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELTA.
                    POLLP(K,1) = POLL(JJ,KK)*DMEAN
                    POLLP(K,2) = POLLP(K,1)
                    POLLP(K,3) = POLLP(K,1)
                    IF(Q(KK).NE.0.0) POLLP(K,4) = POLL(JJ,KK)/Q(KK)
                    IF(Q(KK).EQ.0.0) POLLP(K,4) = POLL(JJ,KK)
                    POLLP(K,5) = POLLP(K,4)
                    ELSE
                    TEAK       = POLL(JJ,KK)
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELTA.
                    POLLP(K,1) = POLLP(K,1) + POLL(JJ,KK)*DMEAN
                    IF(Q(KK).EQ.0.0) Q(KK)  = 1.0
                    IF(TEAK*HSEC.GT.POLLP(K,3))  POLLP(K,3) = TEAK*HSEC
                    IF(TEAK/Q(KK).GT.POLLP(K,5)) POLLP(K,5) = TEAK/Q(KK)
                    ENDIF
1400  CONTINUE
C=======================================================================
C     If dry period criterion has not been met, read next time step.
C=======================================================================
1500  IF(NNEND.EQ.0) THEN
         IF(LOCRQ.GT.0.AND.DRYTS.LT.MIT) GO TO 1700
         IF(IPEVNT.EQ.1)                 GO TO 1700
         IF(LOCRN.GT.0.AND.NEXT.NE.2)    GO TO 1700
         IPEVNT = 1
         ENDIF
C=======================================================================
C     If first event has not yet been reached, continue reading.
C=======================================================================
      IF(NEVNTS.EQ.0) GO TO 1700
C=======================================================================
C     Otherwise, convert parameters to desired units and write event
C     information to off-line file.
C=======================================================================
      IF(LOCRQ.GT.0) THEN
C#### WCH, 11/30/93.  METRIC CORRECTION.  LEAVE FLTOT WITH METRIC
C                     UNITS IF USED. DON'T MULT BY QCONV.
            FLTOT    = FLOWP(1)
            IF(JCUBE.EQ.0) THEN
C#### WCH, 11/30/93.  HERE, CONVERT FLTOT TO CUBIC FEET.
               FLOWP(1) = FLTOT*QCONV*SICONV(1,METRIC)/(3630.0*TRIBA)
C#### WCH, 7/21/93.  MULTIPLY BY 3600 TO GET DEPTH/HOUR.
               FLOWP(3) = FLOWP(3)*QCONV*SICONV(3,METRIC)/(3630.0*TRIBA)
     *                 *3600.0
               ENDIF
            FLOWP(4) = FLOWP(4)/3600.0
            FLOWP(5) = FLOWP(5)/3600.0
C#### WCH, 12/92  CHECK FOR ZERO DIVIDE BY FLOWP(4)
              IF(ABS(FLOWP(4)).LT.1E-20) THEN
                 FLOWP(2) = 0.0
                 ELSE
C#### WCH, 7/21/93. DIVIDE BY 3600. TO GET CFS OR CMS.
                 FLOWP(2) = FLOWP(1)/FLOWP(4)/3600.0
                 ENDIF
            ENDIF
C=======================================================================
C     If statements added to test if total event flow is less than
C     EBASE, the flow threshold input in line B1.  If total flow is
C     less than EBASE, the event will not be included in the analysis.
C=======================================================================
      IF(FLOWP(1).LT.EBASE.OR.FLOWP(4).LE.0.0) THEN
          IF(NNEND.EQ.1) THEN
                         IF(FLOWP(1).EQ.0.) GO TO 1619
                         ENDIF
          NEVNTS = NEVNTS - 1
          GO TO 1619
          ENDIF
C=======================================================================
      IF(LOCRN.GT.0) THEN
                     FLOWP(4) = FLOWP(4)/3600.0
                     FLOWP(5) = FLOWP(5)/3600.0
                     IF(FLOWP(4).EQ.0.0) FLOWP(4) = 1.0
                     FLOWP(2) = FLOWP(1)/FLOWP(4)
                     ENDIF
C=======================================================================
C     Unit conversions for pollutant parameters.  For U.S. units,
C        PCONV(1) = 16018.93 is equal to 0.03531 ft/l * 453592.4 mg/lb 
C        Divide cuft*mg/l by 16016.35 to get pounds.
C     For metric units, PCONV(1) = 1000. Divide cum*mg/l by 1000 to 
C        get kg.
C     NDIM   METRIC   PCONV
C       0      1      16018.93
C       0      2      1000.
C       1      1      0.0353157
C       1      2      0.001
C       2     1 & 2    1.0
C=======================================================================
C#### WCH, 11/30/93.  Can consolidate three loops into one.
C=======================================================================
      IF(NPR.GT.0) THEN
           DO 1540 K = 1,NPR
           JJ        = IPOLRQ(K)
           IF(FLTOT.NE.0.0) POLLP(K,4) = POLLP(K,1)/FLTOT
           IF(FLTOT.LE.0.0) POLLP(K,4) = 0.0
           POLLP(K,1) = POLLP(K,1)/PCONV(JJ)
           POLLP(K,2) = POLLP(K,1)/FLOWP(4)
           POLLP(K,3) = POLLP(K,3)/PCONV(JJ)
1540       CONTINUE
C#### WCH, 11/30/93.  HERE, OLD LOOPS 1560 AND 1580 HAVE BEEN DELETED.
           ENDIF
C=======================================================================
C     Write event information to off-line file.
C=======================================================================
      WRITE(NOUT)  IEVNTB(NEVNTS),TEVNTB(NEVNTS),(FLOWP(I),I=1,5)
      IF(NPR.GT.0) WRITE(NOUT) ((POLLP(K1,J),J=1,5),K1=1,10)
C=======================================================================
C     If end of period of analysis or end of interface file,
C     has been reached, go to event analysis.
C=======================================================================
1619  IF(NNEND.EQ.1) GO TO 2025
C=======================================================================
C     After writing event info, set appropriate parameters to zero.
C=======================================================================
      DO 1620 I1   = 1,3,2
      FLOWP(I1)    = 0.0
1620  CONTINUE
      DO 1640 K2   = 1,NPR
      DO 1630 I2   = 1,5,2
      POLLP(K2,I2) = 0.0
1630  CONTINUE
1640  CONTINUE
C=======================================================================
C     Read another time step, provided that number of events has not
C     reached its maximum value.
C=======================================================================
1700  IF(NEVNTS.LT.LIMIT) GO TO 400
C=======================================================================
C     If event maximum reached, check for termination of program;
C     Iftermination not desired, begin event analysis.
C=======================================================================
      IF(KTERM.EQ.0) GO TO 1975
C=======================================================================
C     Otherwise, terminate program; first check for printing of series.
C=======================================================================
      WRITE(N6,1750) LIMIT,JULDAY,TIMDAY
      IF(KTSEQS.GT.0) THEN
                      WRITE(N6,1760) LIMIT
                      NEXIT = 1
                      GO TO 3400
                      ENDIF
      WRITE(N6,1790)
      RETURN
C=======================================================================
C     T2 represents the end of the period of analysis.
C=======================================================================
1975  T2 = TIME
      GO TO 2010
C=======================================================================
C     Print last date/time on interface file.
C=======================================================================
2000  T2       = TIME - DELTA
      FLOWP(4) = DURAT
      NEXT     = 1
      WRITE(N6,1950) JULDAY,TIMDAY
      GO TO 2010
2005  T2       = TIME
      IF(NEVNTS.EQ.0) THEN
                      NEVNTS = 1
                      WRITE(*,914) NEVNTS
                      ENDIF
      IF(NPR.GT.0)    THEN
                      DO 3005 K   = 1,NPR
                      JJ          = IPOLRQ(K)
                      POLL(JJ,KK) = 0.0
3005                  CONTINUE
                      ENDIF
      Q(KK)    = 0.0
      FLOWP(4) = DURAT
      NEXT     = 1
      WRITE(N6,1950) JULDAY,TIMDAY
C=======================================================================
C     THE ELAPSED TIME FROM THE BEGINNING TO THE END OF THE PERIOD OF
C     ANALYSIS IS THE DIFFERENCE BETWEEN T2 AND T1.  A ROUNDED VALUE FOR
C     THE NUMBER OF MONTHS IN THIS PERIOD CAN BE FOUND USING 730.5 AS THE
C     AVERAGE NUMBER OF HOURS PER MONTH.  NOMOS WILL BE USED TO FIND THE
C     RETURN PERIOD FOR AN EVENT OF A GIVEN MAGNITUDE.
C=======================================================================
2010  IF(LRET.EQ.1) THEN
                    NOMOS = INT(((T2-T1)/3600.0)/730.5  + 0.5)
                    WRITE(N6,2020) NOMOS
C#### WCH, 7/22/93. WARNING FOR T FOR LOW NUMBER OF MONTHS.
                    IF(NOMOS.LT.1) WRITE(N6,2022) 
                    ELSE
                    NOMOS = INT(((T2-T1)/3600.0)/8760.0 + 0.5)
                    WRITE(N6,2021) NOMOS
C#### WCH, 7/22/93. WARNING FOR T FOR LOW NUMBER OF YEARS.
                    IF(NOMOS.LT.1) WRITE(N6,2023) 
                    ENDIF
      NNEND = 1
      GO TO 1350
2025  WRITE(N6,2030) NEVNTS
C=======================================================================
C     Print message if needed.
C=======================================================================
      IF(NEVNTS.EQ.0) THEN
                      WRITE(N6,2040)
                      RETURN
                      ENDIF
      IF(NEVNTS.NE.LIMIT) GO TO 3300
      WRITE(N6,2035) LIMIT
C=======================================================================
C     Check option to print sequential series; If so, Rewind NOUT and
C     print date, time, flow volume, duration, interevent duration.
C=======================================================================
3300  IF(KSEQ.EQ.0) GO TO 4040
3400  WRITE(N6,3500)
      WRITE(*,912)
      IF(LOCRQ.GT.0) THEN
             IF(JCUBE.EQ.0.AND.NPR.EQ.0.AND.METRIC.EQ.1) WRITE(N6,3570)
             IF(JCUBE.GT.0.AND.NPR.EQ.0.AND.METRIC.EQ.1) WRITE(N6,3580)
             IF(JCUBE.EQ.0.AND.NPR.EQ.0.AND.METRIC.EQ.2) WRITE(N6,3575)
             IF(JCUBE.GT.0.AND.NPR.EQ.0.AND.METRIC.EQ.2) WRITE(N6,3585)
             IF(NPR.GT.0.AND.METRIC.EQ.1) WRITE(N6,3550)
     +                       (PNAME(K),K=1,5),(PNAME(K),K=1,5),
     +                       ENGLAB(1),ENGLAB(5),(PUNIT(K),K=1,5),
     +                       PAA(1),PAA(2),PAA(3),PAA(4),PAA(5)
             IF(NPR.GT.0.AND.METRIC.EQ.2) WRITE(N6,3550)
     +                       (PNAME(K),K=1,5),(PNAME(K),K=1,5),
     +                       SILAB(1),SILAB(5),(PUNIT(K),K=1,5),
     +                       PAA(1),PAA(2),PAA(3),PAA(4),PAA(5)
             ELSE
             IF(METRIC.EQ.1) WRITE(N6,3560)
             IF(METRIC.EQ.2) WRITE(N6,3565)
             ENDIF
      REWIND NOUT
      FLWSUM    = 0.0
      P1LSUM    = 0.0
      P2LSUM    = 0.0
      P3LSUM    = 0.0
      P4LSUM    = 0.0
      P5LSUM    = 0.0
      FFF5      = 0.0
      DO 3900 I = 1,NEVNTS
      READ(NOUT,END=4040) I1,T1,(FLOWP(I2),I2=1,5)
      IF(NPR.GT.0) READ (NOUT) ((POLLP(K1,J),J=1,5),K1=1,10)
      FF1       = FLOWP(1)
      FF4       = FLOWP(4)
      FFF5      = FF5
      FF5       = FLOWP(5)
      IF(NPR.GT.0) THEN
                   P1EMC     = POLLP(1,4)
                   P2EMC     = POLLP(2,4)
                   P3EMC     = POLLP(3,4)
                   P4EMC     = POLLP(4,4)
                   P5EMC     = POLLP(5,4)
                   P1LOAD    = POLLP(1,1)
                   P2LOAD    = POLLP(2,1)
                   P3LOAD    = POLLP(3,1)
                   P4LOAD    = POLLP(4,1)
                   P5LOAD    = POLLP(5,1)
                   P1LSUM    = P1LSUM + POLLP(1,1)
                   P2LSUM    = P2LSUM + POLLP(2,1)
                   P3LSUM    = P3LSUM + POLLP(3,1)
                   P4LSUM    = P4LSUM + POLLP(4,1)
                   P5LSUM    = P5LSUM + POLLP(5,1)
                   ENDIF
      FLWSUM    = FLWSUM + FLOWP(1)
      IF(LOCRQ.GT.0.AND.NPR.GT.0)
     +               WRITE(N6,3700) I1,T1,FF1,FF4,P1EMC,P2EMC,P3EMC,
     +               P4EMC,P5EMC,P1LOAD,P2LOAD,P3LOAD,P4LOAD,P5LOAD
      IF(LOCRQ.GT.0.AND.NPR.EQ.0) WRITE(N6,3700) I1,T1,FF1,FF4,FFF5
      IF(LOCRN.GT.0) WRITE(N6,3700) I1,T1,FF1,FF4,FFF5
C=======================================================================
      IF(MOD(I,60).EQ.0) THEN
         WRITE(N6,3875)
         IF(LOCRQ.GT.0) THEN
             IF(JCUBE.EQ.0.AND.NPR.EQ.0.AND.METRIC.EQ.1) WRITE(N6,3570)
             IF(JCUBE.GT.0.AND.NPR.EQ.0.AND.METRIC.EQ.1) WRITE(N6,3580)
             IF(JCUBE.EQ.0.AND.NPR.EQ.0.AND.METRIC.EQ.2) WRITE(N6,3575)
             IF(JCUBE.GT.0.AND.NPR.EQ.0.AND.METRIC.EQ.2) WRITE(N6,3585)
             IF(NPR.GT.0.AND.METRIC.EQ.1) WRITE(N6,3550)
     +                       (PNAME(K),K=1,5),(PNAME(K),K=1,5),
     +                       ENGLAB(1),ENGLAB(5),(PUNIT(K),K=1,5),
     +                       PAA(1),PAA(2),PAA(3),PAA(4),PAA(5)
             IF(NPR.GT.0.AND.METRIC.EQ.2) WRITE(N6,3550)
     +                       (PNAME(K),K=1,5),(PNAME(K),K=1,5),
     +                       SILAB(1),SILAB(5),(PUNIT(K),K=1,5),
     +                       PAA(1),PAA(2),PAA(3),PAA(4),PAA(5)
             ELSE
             IF(METRIC.EQ.1) WRITE(N6,3560)
             IF(METRIC.EQ.2) WRITE(N6,3565)
             ENDIF
         ENDIF
3900  CONTINUE
C=======================================================================
C     Check if program should be terminated after printing
C     series rewind off-line file; proceed with event
C     analysis for flow parameters.
C=======================================================================
4040  REWIND NOUT
      IF(KSEQ.GT.0.AND.LOCRQ.GT.0) THEN
C#### WCH, 11/30/93.  PRINT TOTAL LOADS FOR U.S. AND METRIC.
               IF(NPR.GT.0) WRITE(N6,6024) P1LSUM,P2LSUM,P3LSUM,
     +                                         P4LSUM, P5LSUM
               IF(METRIC.EQ.1) WRITE(N6,6025) LOCRQ,FLWSUM,ENGLAB(1)
               IF(METRIC.EQ.2) WRITE(N6,6025) LOCRQ,FLWSUM,SILAB(1)
               ENDIF
      IF(KSEQ.GT.0.AND.LOCRN.GT.0) THEN
               IF(METRIC.EQ.1) WRITE(N6,6030) LOCRN,FLWSUM,ENGLAB(1)
               IF(METRIC.EQ.2) WRITE(N6,6030) LOCRN,FLWSUM,SILAB(1)
               ENDIF
      IF(NEXIT.EQ.1) RETURN
      DO 4100 N = 1,NEVNTS
      READ(NOUT,END=4140) I1,T1,(PARAM(N,I),I=1,5)
      IF(NPR.GT.0) READ(NOUT) ((PDUM,J=1,5),K1=1,10)
4100  CONTINUE
4140  CONTINUE
      DO 4600 J = 1,5
      IF(IFPAR(J).EQ.0) GO TO 4600
                     JMB = 1
                     J1  = 1
      IF(LOCRN.GT.0) THEN
                     JMB = 6
                     J1  = J + 10
                     ENDIF
C=======================================================================
C     Sort data if table or graph requested and generate/print table.
C=======================================================================
      IF(ISFLOW(JMB+1,J).EQ.1) THEN
                               CALL SORT(J)
                               CALL SBTABL(J,0,NOMOS)
                               ENDIF
C=======================================================================
C     If graph of return period not desired, go to next option.
C     Call curve to make graph of return period.
C=======================================================================
      IF(ISFLOW(JMB+2,J).EQ.1) THEN
C#### WCH, 7/22/93. SORT IF NOT ALREADY DONE SO. 
      IF(ISFLOW(JMB+1,J).NE.1) CALL SORT(J)
                               CALL POINTS(J,1,NOMOS)
                               HTITLE(1) = RPTITL
                               HTITLE(2) = BLANK
                               HORIZ(1)  = RPHOR(1)
C#### WCH, 7/21/93. PRINT CORRECT UNITS FOR RETURN PERIOD.
                               IF(LRET.EQ.1) HORIZ(2)  = RPHOR(2)
                               IF(LRET.EQ.0) HORIZ(2)  = RPHOR(3)
                               VERT1     = VRTITL(J)
                               IF(METRIC.EQ.2)  THEN
                                                VERT2 = SILAB(J)
                                                ELSE
                                                VERT2 = ENGLAB(J)
                                                ENDIF
                               CALL CURVE(X,Y,NPT,1,LOCRQ,BMJ)
                               ENDIF
C=======================================================================
C     If graph of frequency not desired, go to next option.
C=======================================================================
      IF(ISFLOW(JMB+3,J).EQ.1) THEN
C#### WCH, 7/22/93. SORT IF NOT ALREADY DONE SO.
      IF(ISFLOW(JMB+1,J).NE.1.AND.ISFLOW(JMB+2,J).NE.1) CALL SORT(J)
                               CALL POINTS(J,2,NOMOS)
                               HTITLE(1) = PTITL
                               HTITLE(2) = BLANK
                               HORIZ(1)  = PHOR(1)
                               HORIZ(2)  = PHOR(2)
                               VERT1     = VRTITL(J)
                               IF(METRIC.EQ.2) THEN
                                               VERT2 = SILAB(J)
                                               ELSE
                                               VERT2 = ENGLAB(J)
                                               ENDIF
                               CALL CURVE(X,Y,NPT,1,LOCRQ,BMJ)
                               ENDIF
C=======================================================================
C     If moments desired call moments.
C=======================================================================
      IF(ISFLOW(JMB+4,J).EQ.1) THEN
                IF(INLOG.GT.0) THEN
                               WRITE(N6,6037)
                               WRITE(N6,6038)
                               ELSE
                               WRITE(N6,6035)
                               WRITE(N6,6036)
                               ENDIF
                CALL MOMENT(J,0)
                ENDIF
4600  CONTINUE
C=======================================================================
C     Statistical analysis for pollutants.
C=======================================================================
      IF(NPR.EQ.0) GO TO 6000
C=======================================================================
C     Read off-line file for pollutant requested.
C=======================================================================
      DO 5300 K = 1,NPR
      REWIND NOUT
      JJ        = IPOLRQ(K)
      DO 4750 N = 1,NEVNTS
      READ(NOUT,END=4760) I1,T1,(QDUM,I=1,5)
      IF(NPR.GT.0) THEN
                   READ(NOUT) ((POLLP(K1,J),J=1,5),K1=1,10)
                   PARAM(N,1) = POLLP(K,1)
                   PARAM(N,2) = POLLP(K,2)
                   PARAM(N,3) = POLLP(K,3)
                   PARAM(N,4) = POLLP(K,4)
                   PARAM(N,5) = POLLP(K,5)
                   ENDIF
4750  CONTINUE
4760  CONTINUE
      DO 5200 J = 1,5
      J1        = J + 5
      IF(IPPAR(K,J).EQ.0) GO TO 5200
C=======================================================================
C     Generate and print table.
C=======================================================================
      IF(ISPOLL(K,2,J).EQ.1) THEN
                             CALL SORT(J)
                             CALL SBTABL(J,JJ,NOMOS)
                             ENDIF
C=======================================================================
C     If graph of return period not desired, go to next option.
C=======================================================================
      IF(ISPOLL(K,3,J).EQ.1) THEN
C#### WCH, 7/22/93. SORT IF NOT ALREADY DONE SO.
      IF(ISPOLL(K,2,J).NE.1) CALL SORT(J)
                             CALL POINTS(J,1,NOMOS)
                             HTITLE(1) = RPTITL
                             HTITLE(2) = BLANK
                             HORIZ(1)  = RPHOR(1)
C#### WCH, 7/21/93. PRINT CORRECT UNITS FOR RETURN PERIOD.
                               IF(LRET.EQ.1) HORIZ(2)  = RPHOR(2)
                               IF(LRET.EQ.0) HORIZ(2)  = RPHOR(3)
                             VERT1     = VRTITL(J1)
                             IF(METRIC.EQ.2) THEN
                                             VERT2 = SILAB(J1)
                                             ELSE
                                             VERT2 = ENGLAB(J1)
                                             ENDIF
                             IF(NDIM(JJ).EQ.1.AND.J.LE.3) THEN
                                              VERT2 = QUAN(J)
                                              ENDIF
                             IF(NDIM(JJ).EQ.2.AND.J.EQ.1.AND.
     +                              METRIC.EQ.1) VERT2 = OTHER(1)
                             IF(NDIM(JJ).EQ.2.AND.J.EQ.1.AND.
     +                              METRIC.EQ.2) VERT2 = OTHER(3)
                             IF(NDIM(JJ).EQ.2.AND.J.EQ.2.AND.
     +                              METRIC.EQ.1) VERT2 = OTHER(2)
                             IF(NDIM(JJ).EQ.2.AND.J.EQ.2.AND.
     +                              METRIC.EQ.2) VERT2 = OTHER(4)
                             IF(NDIM(JJ).EQ.2.AND.J.EQ.3.AND.
     +                              METRIC.EQ.1) VERT2 = OTHER(2)
                             IF(NDIM(JJ).EQ.2.AND.J.EQ.3.AND.
     +                              METRIC.EQ.2) VERT2 = OTHER(4)
                             IF(J.GE.4)       THEN
                                              VERT2 = PUNIT(JJ)
                                              ENDIF
                             CALL CURVE(X,Y,NPT,1,LOCRQ,BMJ)
                             ENDIF
C=======================================================================
C     If graph of frequency not desired, go to next option.
C=======================================================================
      IF(ISPOLL(K,4,J).EQ.1) THEN
C#### WCH, 7/22/93. SORT IF NOT ALREADY DONE SO.
      IF(ISPOLL(K,2,J).NE.1.AND.ISPOLL(K,3,J).NE.1) CALL SORT(J)
                             CALL POINTS(J,2,NOMOS)
                             HTITLE(1) = PTITL
                             HTITLE(2) = BLANK
                             HORIZ(1)  = PHOR(1)
                             HORIZ(2)  = PHOR(2)
                             VERT1     = VRTITL(J1)
                             IF(METRIC.EQ.2) THEN
                                             VERT2 = SILAB(J1)
                                             ELSE
                                             VERT2 = ENGLAB(J1)
                                             ENDIF
                             IF(NDIM(JJ).EQ.1.AND.J.LE.3) THEN
                                              VERT2 = QUAN(J)
                                              ENDIF
                             IF(NDIM(JJ).EQ.2.AND.J.EQ.1.AND.
     +                              METRIC.EQ.1) VERT2 = OTHER(1)
                             IF(NDIM(JJ).EQ.2.AND.J.EQ.1.AND.
     +                              METRIC.EQ.2) VERT2 = OTHER(3)
                             IF(NDIM(JJ).EQ.2.AND.J.EQ.2.AND.
     +                              METRIC.EQ.1) VERT2 = OTHER(2)
                             IF(NDIM(JJ).EQ.2.AND.J.EQ.2.AND.
     +                              METRIC.EQ.2) VERT2 = OTHER(4)
                             IF(NDIM(JJ).EQ.2.AND.J.EQ.3.AND.
     +                              METRIC.EQ.1) VERT2 = OTHER(2)
                             IF(NDIM(JJ).EQ.2.AND.J.EQ.3.AND.
     +                              METRIC.EQ.2) VERT2 = OTHER(4)
                             IF(J.GE.4)       THEN
                                              VERT2 = PUNIT(JJ)
                                              ENDIF
                             CALL CURVE(X,Y,NPT,1,LOCRQ,BMJ)
                             ENDIF
C=======================================================================
C     Call moment subroutine for pollutant parameter (N,J); Print out.
C=======================================================================
5000  CONTINUE
      IF(ISPOLL(K,5,J).EQ.1) THEN
                IF(INLOG.GT.0) THEN
                               WRITE(N6,6037)
                               WRITE(N6,6038)
                               ELSE
                               WRITE(N6,6035)
                               WRITE(N6,6036)
                               ENDIF
                CALL MOMENT(J,JJ)
                ENDIF
5200  CONTINUE
5300  CONTINUE
6000  WRITE(N6,6050)
      RETURN
9070  WRITE(N6,9080)
      RETURN
C=======================================================================
34    FORMAT(//,
     1' ######################################',/,
     1' # Statistical Analysis Block written #',/,
     1' #   by the University of Florida.    #',/,
     1' #   Last updated Nov. 1993 at OSU.   #',/,
     1' # See data examples or STATS.DOC for #',/,
     1' # information on new parameters not  #',/,
     1' # included in User''s Manual.         #',/,
     1' ######################################',/)
C#### WCH, 7/21/93.  SLIGHT CHANGE FOR PRINT OF LRET.
92    FORMAT(1X,/,
     1'      ####################################',/,
     1'      #      Stats Block input commands  #',/,
     1'      ####################################',//,
     1 1X,'Minimum interevent time (hours).......',F10.2,/,
     1 1X,'NPOINT (number of printed events).....',I10,/,
     1 1X,'METRIC (0-U.S. Customary, 1-Metric)...',I10,//,
     1 1X,'LRET (return period units, 0=yr, 1=mo)',I10,/,
     1 1X,'A (plotting position parameter).......',F10.3,/,
     1 1X,'Calculate logarithmic moments.(INLOG).',I10,//,
     1 1X,'Use inch or millimeter flow JCUBE = 0,',/,
     1 1X,'or cfs or cms flow values   JCUBE = 1.',I10,/)
C    1 1X,'Use storm event definition  JSEA  = 0,',/,
C    1 1X,' or monthly events          JSEA  = 1,',/,
C    1 1X,' or seasonal events         JSEA  = 2.',I10,/)
93    FORMAT(
     1 1X,'Print sequential series (KSEQ)........',I10,/,
     1 1X,'Terminate program parameter (KTERM)...',I10,/,
     1 1X,'KTSEQS................................',I10,/)
100   FORMAT(//,
     +' ##################################################',/,
     +' #  The period of time for which the statistical  #',/,
     +' #              analysis is being performed is:   #',/,
     +' ##################################################',//,
     +'  Starting date...............',I6,'  Starting time...',F5.2,
     +' hours',/,
     +'  Ending date.................',I6,'  Ending time.....',F5.2,
     +' hours')
C#### WCH, 7/21/93.  ALTER FORMAT FOR OUTPUT OF SECONDS.
101   FORMAT(//,
     1' ##################################################',/,
     1' #  The period of time for which the statistical  #',/,
     1' #              analysis is being performed is:   #',/,
     1' ##################################################',//,
     1'  Starting Julian date........',I6,'  Starting time...',F6.0,
     1' seconds',/,
     1'  Ending Julian date..........',I6,'  Ending time.....',F6.0,
     1' seconds')
102   FORMAT(/,
     1' ===> A zero starting date indicates that the analysis ',/,
     2'      commences at the beginning of the record.  A zero ',/,
     3'      ending date indicates that the analysis continues to',/,
     4'      the end of the record.')
105   FORMAT(/,' The minimum interevent time has been defined as ',F9.2,
     1         ' hours.')
110   FORMAT(/,
     1' ******************************************************',/,
     2' * The flow location number requested for Statistical *',/,
     3' * Analysis is: ',I9,'                             *',/,
     4' ******************************************************')
1110  FORMAT(/,
     1' ******************************************************',/,
     2' * The flow location requested for Statistical        *',/,
     3' * Analysis is: ',A10,'                            *',/,
     4' ******************************************************')
111   FORMAT(/,
     1' ******************************************************',/,
     2' * The rain location number requested for Statistical *',/,
     3' * Analysis is: ',I7,'                               *',/,
     4' ******************************************************')
112   FORMAT(/,
     1' The number of quality parameters',/,
     2' requested for statistical analysis is..',I10,/,
     3' The base flow to separate events is....',F10.4,' cfs.',/,
     4' Threshold event flow inches (cfs)......',F10.4,/)
1112  FORMAT(/,
     1' The number of quality parameters',/,
     2' requested for statistical analysis is..',I10,/,
     3' The base flow to separate events is....',F10.4,' cms.',/,
     4' Threshold event flow millimeters (cms).',F10.4,/)
113   FORMAT(/,' U.S. customary units are used in input/output.')
115   FORMAT(/,' Metric units are used in input/output.')
114   FORMAT(/,' The pollutants requested for this run, ',/,
     1' identified by number, are as follows: ',10I3)
118   FORMAT(//,
     1' ############################################',/,
     1' #  The statistical options requested for   #',/,
     1' #       flow rate are indicated by "1"     #',/,
     1' ############################################',//,
     241X,'   Total Flow  Average Flow     Peak Flow  Event Duratn  Inte
     2revent Duratn',/,
     341X,'  ------------ -------------  -----------  ------------  ----
     3-------------',/,
     4' Table of return period and frequency',T40,5I14,/,
     5' Graph of return period',T40,5I14,/,
     5' Graph of frequency',T40,5I14,/,' Moments',T40,5I14)
119   FORMAT(/,
     1' ############################################',/,
     1' #  The statistical options requested for   #',/,
     1' #       rainfall are indicated by "1"      #',/,
     1' ############################################',//,
     241X,'  Total Volume Average Inten  Peak Intens  Event Duratn  Inte
     2revent Duratn',/,
     341X,'  ------------ -------------  -----------  ------------  ----
     3-------------',/,
     4' Table of return period and frequency',T40,5I14,/,
     5' Graph of return period',T40,5I14,/,
     5' Graph of frequency',T40,5I14,/,' Moments',T40,5I14)
120   FORMAT(//,
     1' ####################################################',/,
     1' #  The statistical options requested for           #',/,
     1' #  quality parameter ',A8,' are indicated by "1" #',/,
     1' ####################################################',//,
     143X,'Total Load  Average Load     Peak Load Flow Wtd Conc     Peak
     1 Conc',/,
     143X,'----------  ------------     --------- -------------     ----
     1-----',/,
     4' Table of return period and frequency',T40,5I14,/,
     5' Graph of return period',T40,5I14,/,
     5' Graph of frequency',T40,5I14,/,' Moments',T40,5I14)
142   FORMAT(/,' ===> Error !! The location number requested was',/,
     +         '      not found on the interface file.',/,
     +         '      Location ',I10,' not found on interface file',/,
     +         '      execution of Stats block terminated.')
143   FORMAT(/,' ===> Error !! The location number requested was',/,
     +         '      not found on the interface file.',/,
     +         '      Location ',A10,' not found on interface file',/,
     +         '      execution of Stats block terminated.')
334   FORMAT(//,
     +' ***********************************************************',/,
     +' *  Precipitation input was created using the Rain block   *',/,
     +' *  NWS Precipitation station....',I9,'                 *',/,
     +' ***********************************************************')
375   FORMAT(//,' ===> Program execution continuing.  Data will be ',/,
     +'      read from the interface file and separated into events.')
415   FORMAT(/,' ===> The first date and time on the interface file',/,
     1         '      are ',I7,' and ',F9.2,' seconds',/)
911   FORMAT(/,' Reading interface file.')
912   FORMAT(/,' Computing statistics.')
913   FORMAT(/,' Reading event  #',/)
914   FORMAT('+',I16)
1610  FORMAT(I6,F5.2,5G10.4,10(5G10.4))
1750  FORMAT(/,' ===>  Number of events has reached ',I5,'.',/,
     1 '       execution has been terminated.',/,
     2 '       last date and time read are ',I7,1X,F5.2,' hours')
1760  FORMAT(/,' ===> A table of the first ',I5,' events will be',/,
     +                                          ' printed.')
1790  FORMAT(/,' ===> As you requested, no output is provided.')
1950  FORMAT(/,' ===> End of interface file reached.',//,
     1         ' ===> Last Julian date and time read ',/,
     2         '      are ',I7,' and ',F9.2,' seconds',//,
     3         ' ===> Program continuing with analysis of events.')
2020  FORMAT(/,' ===> The number of months within the period of',/,
     1'      analysis rounded to the nearest month, is ',I6,'.')
2021  FORMAT(/,' ===> The number of years within the period of',/,
     1'      analysis rounded to the nearest year,  is ',I6,'.')
C#### WCH, 7/22/93
2022  FORMAT(/,' ###> WARNING. The rounded number of months is < 1.',/
     1,'      Computed return periods are likely to be meaningless.')
2023  FORMAT(/,' ###> WARNING. The rounded number of years is < 1.',/
     1,'      Computed return periods are likely to be meaningless.')
2030  FORMAT(/,' ===> The number of events within the period of',/,
     1'      analysis is                               ',I6,'.')
2035  FORMAT(/,' ===> Error !! The maximum number of events has been',
     +' reached.',/,
     +'               No further time steps can be read.',/,
     +'               The program is continuing with an analysis',/,
     +'               of the first ',I5,' events.')
2040  FORMAT(/,' ===> Error !! The total number of events was zero !!')
2115  FORMAT(//,
     +' ********************************************************',/,
     +' *  Precipitation output created using the Rain block   *',/,
     +' *  Number of precipitation stations...',I9,'        *',/,
     +' ********************************************************',/)
2120  FORMAT(' Location Station number',/,
     +       ' -------- --------------',/,
     +       10(I9,'. ',I13,/))
3500  FORMAT('1',/,15X,27(1H#),/,15X,'Sequential Series of Events',
     +           /,15X,27(1H#),/)
3550  FORMAT('                     Flow      Event  |<------------------
     +----------EMC------------------------->|<----------------------Tot
     +al Load----------------------->|',/,
     +   '            Time   Volume   Duration    ',1X,A8,9(4X,A8),/,
     +   '   Date    (hour)',1X,A8,2X,A8,4X,A8,11(4X,A8),/,
     +       '   ----    ------ --------  --------   ---------   -------
     +--  ----------  ----------   ---------   ---------   ---------   -
     +--------  ---------   ---------')
3560  FORMAT('                      Rain      Event  Interevent',/,
     +       '           Time     Volume   Duration    Duration',/,
     +       '   Date   (hour)   (inches)    (hours)     (hours)',/,
     +       '   ----   ------  --------    -------     -------')
3565  FORMAT('                      Rain      Event  Interevent',/,
     +       '           Time     Volume   Duration    Duration',/,
     +       '   Date   (Hour)   (Millim)    (hours)     (hours)',/,
     +       '   ----   ------  --------    -------     -------')
3570  FORMAT('                      Flow      Event  Interevent',/,
     +       '           Time     Volume   Duration    Duration',/,
     +       '   Date   (hour)   (inches)    (hours)     (hours)',/,
     +       '   ----   ------  --------    -------     -------')
3575  FORMAT('                      Flow      Event  Interevent',/,
     +       '           Time     Volume   Duration    Duration',/,
     +       '   Date   (Hour)   (Millim)    (hours)     (hours)',/,
     +       '   ----   ------  --------    -------     -------')
3580  FORMAT('                      Flow      Event  Interevent',/,
     +       '           Time     Volume   Duration    Duration',/,
     +       '   Date   (hour)   ( ft^3 )    (hours)     (hours)',/,
     +       '   ----   ------  --------    -------     -------')
3585  FORMAT('                      Flow      Event  Interevent',/,
     +       '           Time     Volume   Duration    Duration',/,
     +       '   Date   (Hour)  (meter^3)    (hours)     (hours)',/,
     +       '   ----   ------  --------    -------     -------')
3700  FORMAT(2X,I6,1X,F8.2,1X,1PG9.3,0PF9.2,4(3X,F9.2),7(3X,1PG9.2))
3875  FORMAT('1',/,15X,40(1H#),/,15X,
     1'Sequential Series of Events  (continued)',/,15X,40(1H#),/)
4050  FORMAT(11X,5G10.4)
6024  FORMAT(' Total Loads--->:',82X,1PG9.3,4(3X,G9.3))
6025  FORMAT(//,
     1' ************************************************************',/,
     2' * Total Flow at location ',I9,' is ',1PG11.4,' ',A9,'*',/,
     3' ************************************************************',
     4//)
6030  FORMAT(//,
     1' ************************************************************',/,
     2' * Total Rain at location ',I9,' is ',1PG11.4,' ',A9,' *',/,
     3' ************************************************************',
     4//)
6035  FORMAT(/,88(1H#),/,'  Moments',/,88(1H#))
6037  FORMAT(/,90(1H#),/,
     1'  Moments (Log-Normal [natural log] Distribution)',/,90(1H#))
6036  FORMAT(/,
     1' Constituent            Event                            Standard
     1    Coef. of   Coef. of',/,
     2'    Analyzed        Parameter        Mean    Variance   Deviation
     2   Variation   Skewness',/,
     3'  ----------  ---------------    --------    --------   ---------
     3   ---------  ---------')
6038  FORMAT(/,
     1'  Constituent           Event <-----Logarithmic------>   <-------
     1-----------Arithmetic-------------->',/,
     2'     Analyzed       Parameter        Mean   Std. dev.        Mean
     2   Std. dev.  C. of var.     Median',/,
     3'  ------------  -------------   ---------   ---------    --------
     3   ---------  ----------  ---------')
6050  FORMAT(/,' ===>  Stats Block terminated normally.')
9030  FORMAT(/,' Rainfall location (LOCRN).........',I9,/,
     + ' is not located on the rainfall interface file.  Stats ended.')
9080  FORMAT(/,' ===> Error reading rainfall interface file.')
C=======================================================================
 888  CALL IERROR
      END

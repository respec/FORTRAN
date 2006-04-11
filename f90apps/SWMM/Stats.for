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
C       2/27/95, WCH.     Mysterious correction to read arrays
C                         IEVNTB, TEVNTB again off NOUT because they
C                         are somehow corrupted earlier, prior to
C                         sorting, resulting in wrong date assigned
C                         to ranked data.
C       8/1/95, WCH.      To correspond to Rain Block change, change
C                         rainfall station ID, LOCRN, to character.
C       8/7/95, WCH.      Add check for requested ending date before
C                         first date on interface file.
C       7/22/96, WCH.     Correct computation of rainfall interevent
C                         times (was THISTO too great).
C       7/23/96, WCH.     Correct computation of flow etc. duration
C                         and interevent times.  Correct print-out of
C                         interevent time for flows etc. (was off by
C                         one line).  Add fix to catch first flow etc.
C                         event when it starts before time MIT.
C                         Add event number to event print out.  Add
C                         message explaining duration and interevent
C                         calculation for hydrographs.  Print all
C                         concentrations with G-format.  Improve
C                         print-out of KTSEQS.  Print correct
C                         definition of JCUBE.
C       7/29/96, WCH.     Additional initialization.  Otherwise,
C                         parameter METRIC was incremented on successive
C                         STATS runs, if slash delimeter (nul input)
C                         was used on B1 input line!
C       8/7/96, WCH.      Correction for event separation.
C      10/2/96, WCH.      Remove extra BACKSPACE in error routine.
C       6/3/97, CIM.      Add check for zero TRIBA for JCUBE=0 option.
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
C     RECORD ERROR BECAUSE OF THE LENGTH OF THE FORMAT STATEMENT
C     (I.E., 3550).  A SOLUTION TO THIS MAY BE TO MODIFY THE FDL FILE
C     ASSOCIATED WITH YOUR OUTPUT FILE TO INCREASE RECORD LENGTH.
C
C========================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'LAB.INC'
      INCLUDE 'STCOM.INC'
C=======================================================================
CIM CHANGE DIMENSIONS FROM 10 TO MQUAL
      DIMENSION FLOWP(5),POLLP(MQUAL,5),Q(NIE),POLL(MQUAL,NIE),
     1          PCONV(MQUAL),IFPAR(5),IPPAR(MQUAL,5),ISFLOW(10,6),
     1          ISPOLL(MQUAL,5,5)
C#### WCH, 7/21/93. INCREASE DIMENSION OF RPHOR TO 3.
CIM INCREASE HYDROGRAPHS  ~~~~~~~~~~~~~~~~~~~~~~~
      DIMENSION RPHOR(3),PHOR(2),VRTITL(15),SICONV(9,2),JSTA(MAXRG)
CIM  CHANGE TO WRITE ALL PARAMETERS
      DIMENSION PEMC(MQUAL),PLOAD(MQUAL),PLSUM(MQUAL)
cim      DIMENSION RPHOR(3),PHOR(2),VRTITL(15),SICONV(9,2),JSTA(10)
cim ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      REAL MIT
C#### WCH, 7/21/93. MAKE NEWLAB BE 8 BYTES TO AGREE WITH ENGLAB.
      CHARACTER RPTITL*30,PTITL*30,BLANK*10,PHOR*30,
     1          BMJ*10,KOCRQ*10,RPHOR*30,VRTITL*10,NEWLAB(4)*8,
     2          COND*3,OLD*3,QUAN(3)*8,OTHER(4)*8,PAA(MQUAL)*8
C#### WCH, 8/1/95. CHANGE PRECIP. STATION ID TO CHARACTER AND 3 MORE.
      CHARACTER*8 JSTA, BLANK1*1,TRYLOC*8,TRYSTA*8
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
C#### WCH, 8/1/95.  ADD SINGLE BLANK.
      DATA BLANK/'          '/,BMJ/'          '/,BLANK1/' '/
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
      REWIND (NOUT)
      REWIND (LAST)
C=======================================================================
C     Initialize several variables and arrays.
C=======================================================================
      COND       = 'DRY'
      OLD        = 'DRY'
C#### WCH, 8/1/95.  INITIALIZE LOCRN.
      LOCRN      = '        '
C#### WCH, 7/29/96.  INITIALIZE ADDITIONAL VARIABLES.
      METRIC     = 0
      ISTART     = 0
      TSTART     = 0.
      IEND       = 0
      TEND       = 0.
      INLOG      = 0
      JCUBE      = 0
      MIT        = 0
      BASE       = 0.
      EBASE      = 0.
      LOCRQ      = 0
      KOCRQ      = '        '
      LLOCRN     = 0
      NPR        = 0
      NPOINT     = 0
      LRET       = 0
      A          = 0.
C
      NNEND      = 0
      NEXIT      = 0
      IPEVNT     = 0
      IDATEZ     = 0
      DRYTS      = 0.0
      TZERO      = 0.0
      HSEC       = 3600.0
      DO 5 I     = 1,5
      IFPAR(I)   = 0
    5 FLOWP(I)   = 0.0
      DO 6 I     = 1,5
      DO 6 J     = 1,MQUAL
      POLLP(J,I) = 0.0
    6 IPPAR(J,I) = 0
C=======================================================================
C >>>>Read Data Group A1 <<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,ISTART,TSTART,IEND,TEND,INLOG,JCUBE,JNEG
      JYEAR = ISTART/10000
      IF ((ISTART.GT.0).AND.(JYEAR.LT.100)) THEN
      ISTART = ISTART - JYEAR*10000
      JYEAR = JYEAR + 1900
      ISTART = ISTART + JYEAR*10000
      ENDIF
      JYEAR = IEND/10000
      IF ((IEND.GT.0).AND.(JYEAR.LT.100)) THEN
      IEND = IEND - JYEAR*10000
      JYEAR = JYEAR + 1900
      IEND = IEND + JYEAR*10000
      ENDIF
C=======================================================================
C>>>>>>>>>> Read Data Group B1 <<<<<<<<<<<<<<<<<
C=======================================================================
C#### WCH, 8/1/95.
C     LOCRN CHANGED TO CHARACTER.  INSTALL AN ERROR CHECK IN CASE
C     USER MISTAKENLY PUTS IT IN AS INTEGER.  IF SO, CONVERT TO CHARCT.
C     MAY NOT NEED THIS FOR LAHEY!  LAHEY APPARENTLY READS CHARACTER
C     VARIABLES CORRECTLY EVEN IF NOT INCLUDED IN QUOTES!
C=======================================================================
      IF(JCE.EQ.0) THEN
                   READ(N5,*,ERR=885) CC,MIT,BASE,EBASE,LOCRQ,
     +                                LOCRN,NPR,NPOINT,METRIC,LRET,A
                   GO TO 15
  885              BACKSPACE N5
C#### WCH (B. LAZERTE), 10/2/96.  REMOVE EXTRA BACKSPACE.
C####                   BACKSPACE N5
                   READ(N5,*,ERR=888) CC,MIT,BASE,EBASE,LOCRQ,
     +                                LLOCRN,NPR,NPOINT,METRIC,LRET,A
                   IF(LLOCRN.EQ.0) LOCRN = ' '
                   IF(LLOCRN.GT.0) WRITE(LOCRN,'(I8)') LLOCRN
                   WRITE(N6,9040) LLOCRN
                   ELSE
                   READ(N5,*,ERR=886) CC,MIT,BASE,EBASE,KOCRQ,
     +                                LOCRN,NPR,NPOINT,METRIC,LRET,A
                   IF(KOCRQ.NE.' ') LOCRQ = 1
                   GO TO 15
  886              BACKSPACE N5
                   BACKSPACE N5
                   READ(N5,*,ERR=888) CC,MIT,BASE,EBASE,KOCRQ,
     +                                LLOCRN,NPR,NPOINT,METRIC,LRET,A
                   IF(LLOCRN.EQ.0) LOCRN = ' '
                   IF(LLOCRN.GT.0) WRITE(LOCRN,'(I8)') LLOCRN
                   WRITE(N6,9040) LLOCRN
                   IF(KOCRQ.NE.' ') LOCRQ = 1
                   ENDIF
C#### WCH, 8/1/95.  LOCRN NOW CHARACTER.
C####      IF(LOCRN.GT.0) INLOG = 0
   15 IF(LOCRN.EQ.'0') LOCRN = ' '
      IF(LOCRN.NE.' ') INLOG = 0
cim process all
      IF (KOCRQ.EQ.'-1') LOCRQ = -1
      LLOCRQ = LOCRQ
      IF (LOCRQ.LT.0) THEN
      LOCRQ = 1
      WRITE(N6,8005)
      ENDIF
cim all
      WRITE(N6,92) MIT,NPOINT,METRIC,LRET,A,INLOG,JCUBE,JNEG
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
cim check for zero triba
                     IF(TRIBA.LE.0.0.AND.JCUBE.EQ.0) THEN
                     WRITE(N6,*) 'ERROR - TRIBA EQUALS ZERO,',
     +                           ' CANNOT USE JCUBE = 0'
                     WRITE(N6,*) '        SET JCUBE = 1'
                     STOP ' ZERO TRIBA'
                     ENDIF
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
C#### WCH, 8/1/95.  LOCRN NOW CHARACTER.
C####      IF(LOCRN.GT.0) THEN
      IF(LOCRN.NE.' ') THEN
                     REWIND (LAST)
                     READ(LAST,ERR=9070) NSTA,MRAIN,(JSTA(I),I=1,NSTA)
                     WRITE(N6,2115)  NSTA
                     WRITE(N6,2120) (I,JSTA(I),I=1,NSTA)
C=======================================================================
C#### WCH, 8/1/95.  INORDINATE HASSEL TO COMPARE CHARACTER VARIABLES
C     BECAUSE DON'T KNOW WHERE NON-BLANK CHARACTERS ARE IN FIELD.
C     TRYLOC WILL BE RIGHT-ADJUSTED VALUE OF LOCRN, THAT IS, PUSH
C     NON-BLANK CHARACTERS OF LOCRN TO RIGHT OF 8-CHARACTER FIELD.
C     FUNCTION NBLANK() GIVES POSITION OF LAST NON-BLANK CHARACTER.
C     CAUTION!  FUNCTION NBLANK() MAY BE LAHEY-SPECIFIC.
C     // OPERATOR IN LINE 230 IS CONCATENATION OPERATOR FOR CHARACTER
C     VARIABLES.  BLANKS WILL BE "ADDED" ON LEFT, WITH EXTRA CHARACTERS
C     (BEYOND 8-CHARACTER FIELD WIDTH) LOST TO RIGHT.  THESE LOST
C     CHARACTERS ARE SUPPOSED TO BE BLANKS!
C=======================================================================
c  nblank is lahey
c                     NCLEN = 8 - NBLANK(LOCRN)
c  len_trim is digital visual fortran
                     NCLEN = 8 - LEN_TRIM(LOCRN)
                     TRYLOC = LOCRN
                     IF(NCLEN.GT.0) THEN
                          DO 230 I = 1,NCLEN
  230                     TRYLOC = BLANK1//TRYLOC
                          ENDIF
                     DO 290 I = 1,NSTA
                     TRYSTA = JSTA(I)
c nblank is lahey
c                     NCLEN = 8 - NBLANK(JSTA(I))
c  len_trim is digital visual fortran
                     NCLEN = 8 - LEN_TRIM(JSTA(I))
                     IF(NCLEN.GT.0) THEN
                          DO 240 K = 1,NCLEN
  240                     TRYSTA = BLANK1//TRYSTA
                          ENDIF
                     IF(TRYLOC.EQ.TRYSTA) THEN
                                          MSTA = I
                                          GO TO 295
                                          ENDIF
  290                CONTINUE
C#### WCH, 8/7/95.  ENHANCE ERROR MESSAGE.
                     WRITE(N6,9030) LOCRN,JSTA(NSTA)
C#### WCH, 10/19/95.  ASSIGN VALUE OF MSTA!
                     MSTA = NSTA
  295                CONTINUE
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
                   DO 350 K = 1,MQUAL
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
C#### WCH, 8/1/95.  LOCRN NOW CHARACTER.
C####      IF(LOCRN.GT.0) WRITE(N6,119) ((ISFLOW(I,J),J=1,5),I=7,10)
      IF(LOCRN.NE.' ') WRITE(N6,119) ((ISFLOW(I,J),J=1,5),I=7,10)
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
CIM  CHANGE TO LOOP THROUGH ALL LOCATIONS IF LOCRQ IS LESS THAN ZERO 6/97
CIM  OR KOCRQ is '-1'
CIM
      ILOCRQ = 0
8100  ILOCRQ = ILOCRQ + 1
      IF (LLOCRQ.LT.0) THEN
      IF (ILOCRQ.GT.1) THEN
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
      DO 8105 I     = 1,5
 8105 FLOWP(I)   = 0.0
      DO 8106 I     = 1,5
      DO 8106 J     = 1,MQUAL
      POLLP(J,I) = 0.0
 8106 CONTINUE
      REWIND (LAST)
      REWIND (NOUT)
      CALL INFACE(1,LAST)
      ENDIF
      IF (JCE.EQ.0) THEN
         LOCRQ = NLOC(ILOCRQ)
         WRITE(N6,2110) LOCRQ
      ELSE
         KOCRQ = KAN(ILOCRQ)
         WRITE(N6,2111) KOCRQ
      ENDIF
      ENDIF
CIM  <:)
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
cim   need to write to 6 not * to get carriage control on writes to console
      WRITE(6,914) NEVNTS + 1
  400 IF(LOCRQ.GT.0) THEN
                     IF(NQUAL.EQ.0) READ(LAST,END=2005)
     +                  JULDAY,TIMDAY,DELTA,(Q(K),K=1,LOCATS)
                     IF(NQUAL.GT.0) READ(LAST,END=2005) JULDAY,TIMDAY,
     +                  DELTA,(Q(K),(POLL(J,K),J=1,NQUAL),K=1,LOCATS)
c      Write(n6,*) JULDAY,TIMDAY,DELTA,(Q(K),K=1,LOCATS)
      JYEAR = JULDAY/1000
      IF (JYEAR.LT.100) THEN
      JULDAY = JULDAY - JYEAR*1000
      JYEAR = JYEAR + 1900
      JULDAY = JULDAY + JYEAR*1000
      ENDIF

                     ELSE
                     JDAY  = JULDAY
                     TMDAY = TIMDAY
C#### WCH, 7/22/96.  SAVE OLD THISTO TO SUBTRACT TO GET INTEREVENT TIME.
                     THISLD = THISTO
                     READ(LAST,END=2005,ERR=9070) JULDAY,TIMDAY,
     +                                   THISTO,(Q(K),K=1,NSTA)
      JYEAR = JULDAY/1000
      IF (JYEAR.LT.100) THEN
      JULDAY = JULDAY - JYEAR*1000
      JYEAR = JYEAR + 1900
      JULDAY = JULDAY + JYEAR*1000
      ENDIF
                     ENDIF
      KREAD = KREAD + 1
C=======================================================================
C#### WCH, 8/7/95.  ADD CHECK FOR MIS-MATCH OF DATES.
C=======================================================================
      IF(KREAD.EQ.1) THEN
           IF(IEND.EQ.0) GO TO 405
           IF(JULDAY.LT.IEND) GO TO 405
           IF(JULDAY.EQ.IEND.AND.TIMDAY.LT.TEND) GO TO 405
C=======================================================================
C     Here, starting time on file is later than end time requested
C     on input.  Print error message and return.
C=======================================================================
           WRITE(N6,9455) JULDAY,LAST,IEND
           WRITE(*,9455)  JULDAY,LAST,IEND
           RETURN
           ENDIF
C
  405 IF(LOCRQ.GT.0.AND.DELTA.EQ.0.0) GO TO 400
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
C#### 8/1/95.  LOCRN NOW CHARACTER.
C####                     IF(LOCRN.GT.0) THEN
                     IF(LOCRN.NE.' ') THEN
                                    IDATEZ = JULDAY
                                    TZERO  = TIMDAY
C#### WCH, 7/22/96.
                                    THISLD = THISTO
                                    ENDIF
                     ENDIF
C#### 8/1/95.  LOCRN NOW CHARACTER.
C####                     IF(LOCRN.GT.0) THEN
      IF(LOCRN.NE.' ') THEN
                     CALL NTIME(JDAY,TMDAY,DELT)
                     DELTA = -DELT
                     ENDIF
      TOLD  = TIME
      TIME  = TIME + DELTA
C=======================================================================
C     Caution in interpretting DELTA:  For flows etc. off interface
C     file, DELTA is time step >prior< to current time (TIME).  That is,
C     DELTA "looks backwards."  For rainfall interface file, DELTA is
C     time since last rain >began< and also looks backwards.
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
C#### 8/1/95.  LOCRN NOW CHARACTER.
C####                     IF(LOCRN.GT.0) THEN
      IF(LOCRN.NE.' ') THEN
                     NEXT = 1
C=======================================================================
C     If satisfy the following IF-statement, then have new event.
C     Note that rainfall interface file is assumed to contain only
C     non-zero values.  Thus, every record on interface file is either
C     a new event or a continuation of an existing event.  There are no
C     dry rainfall periods.
C
C#### WCH, 7/22/96.  DELTA IS TIME BETWEEN >START< OF TWO RAINFALL
C     INCREMENTS.  THUS, ACTUAL INTEREVENT TIME IS DELTA - THISLD.
C     THISLD = THISTO FOR PREVIOUS RAINFALL VALUE.
C
C     Note, for rainfall, interevent time, FLOWP(5), looks forward from
C     event that will be placed on the scatch file.
C=======================================================================
C####                     IF(TIME-TOLD.GE.MIT) THEN
                     IF(TIME-TOLD-THISLD.GE.MIT) THEN
                                          NEXT     = 2
                                          BACKSPACE LAST
                                          FLOWP(4) = DURAT
C#### WCH, 7/22/96.
C####                                          FLOWP(5) = DELTA
                                          FLOWP(5) = DELTA - THISLD
                                          ENDIF
                     IF(DELTA.EQ.0.0) NEXT = 3
                     ENDIF
C=======================================================================
C     Flow and water quality interface file.
C=======================================================================
      IF(LOCRQ.GT.0) THEN
	           IF(JNEG.EQ.0) THEN
                     IF(Q(KK).GT.BASE) THEN
                                       COND = 'WET'
                                       ELSE
                                       COND = 'DRY'
                                       ENDIF
	           ELSE
                     IF(Q(KK).LT.BASE) THEN
                                       COND = 'WET'
                                       ELSE
                                       COND = 'DRY'
                                       ENDIF
	           ENDIF
                     IF(OLD.EQ.'WET'.AND.COND.EQ.'WET')   NEXT = 1
                     IF(OLD.EQ.'WET'.AND.COND.EQ.'DRY')   NEXT = 2
                     IF(OLD.EQ.'DRY'.AND.COND.EQ.'WET')   NEXT = 3
                     IF(OLD.EQ.'DRY'.AND.COND.EQ.'DRY')   NEXT = 4
C#### WCH, 7/23/96.  MISSING FIRST EVENT IF IT STARTS BEFORE TIME MIT.
C####                     IF(NEXT.EQ.3.AND.DRYTS+DELTA.LT.MIT) NEXT = 1
C#### WCH, 8/7/96.  STILL DIDN'T GET IT RIGHT.  SHOULD NOT ADD DELTA.
C     OTHERWISE, SEPARATING EVENTS AT MIT-1.
C####     IF(NEVNTS.GT.0.AND.NEXT.EQ.3.AND.DRYTS+DELTA.LT.MIT) NEXT = 1
          IF(NEVNTS.GT.0.AND.NEXT.EQ.3.AND.DRYTS.LT.MIT) NEXT = 1
                     IF(MIT.EQ.0.0)                       NEXT = 3
                     OLD = COND
                     ENDIF
 1350 CONTINUE
C=======================================================================
C     Previous condition is wet; new condition is wet;
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
C#### 8/1/95.  LOCRN NOW CHARACTER.
C####                    IF(LOCRN.GT.0) FLOWP(1) = FLOWP(1) +
                    IF(LOCRN.NE.' ') FLOWP(1) = FLOWP(1) +
     +                              Q(KK)*THISTO/3600.0
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
C     Previous condition is wet; new condition is dry.
C=======================================================================
      IF(NEXT.EQ.2) THEN
C#### WCH, 7/23/96.  PUT THIS BEFORE ADDING NEW DELTA, OTHERWISE DURAT
C     IS TOO LONG.
                    IF(LOCRQ.GT.0) FLOWP(4) = DURAT
                    DURAT    = DURAT + DELTA
                    DRYTS    = DELTA
                    ENDIF
C=======================================================================
C     Previous condition is dry; new condition is wet ==> new event.
C=======================================================================
      IF(NEXT.EQ.3) THEN
                                 NEVNTS = NEVNTS + 1
cim   need to write to 6 not * to get carriage control on writes to console
                    WRITE(6,914) NEVNTS
                    IPEVNT         = 0
                    IEVNTB(NEVNTS) = NYEAR*10000 + MONTH*100 + NDAY
                    TEVNTB(NEVNTS) = TIMDAY/3600.0
                    DURAT          = DELTA
                    FLOWP(4)       = DURAT
                    IF(LOCRQ.GT.0) THEN
C#### WCH, 7/23/96.  INTEREVENT TIME TOO GREAT WITH THIS.
C####                                   DRYTS    = DRYTS+DELTA
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELTA.
                                   FLOWP(1) = Q(KK)*DMEAN
C#### WCH, 7/21/93
C                                  FLOWP(3) = Q(KK)*DELTA
                                   FLOWP(3) = Q(KK)
C=======================================================================
C     Note, for flow etc., interevent time, FLOWP(5), looks backwards
C     from event that will be placed on scratch file.
C=======================================================================
                                   FLOWP(5) = DRYTS
                                   ENDIF
C#### 8/1/95.  LOCRN NOW CHARACTER.
C####                     IF(LOCRN.GT.0) THEN
                    IF(LOCRN.NE.' ') THEN
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
C#### 8/1/95.  LOCRN NOW CHARACTER.
C####                    IF(MIT.EQ.0.0.AND.LOCRN.GT.0) THEN
                    IF(MIT.EQ.0.0.AND.LOCRN.NE.' ') THEN
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
C#### WCH, 8/1/95.  LOCRN NOW CHARACTER.
C####         IF(LOCRN.GT.0.AND.NEXT.NE.2)    GO TO 1700
         IF(LOCRN.NE.' '.AND.NEXT.NE.2)    GO TO 1700
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
C#### WCH, 11/30/93.  HERE, CONVERT FLTOT TO CUBIC FEET, THEN TO DEPTH.
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
C     Note, if JCUBE = 1, and flows etc. are analyzed, then test against
C     volume, not depth.
C=======================================================================
cim change below to abs(flowp(1) for negative flow events.
      IF(ABS(FLOWP(1)).LT.EBASE.OR.FLOWP(4).LE.0.0) THEN
          IF(NNEND.EQ.1) THEN
                         IF(FLOWP(1).EQ.0.) GO TO 1619
                         ENDIF
          NEVNTS = NEVNTS - 1
          GO TO 1619
          ENDIF
C=======================================================================
C#### WCH, 8/1/95.  LOCRN NOW CHARACTER.
C####      IF(LOCRN.GT.0) THEN
      IF(LOCRN.NE.' ') THEN
                     FLOWP(4) = FLOWP(4)/3600.0
                     FLOWP(5) = FLOWP(5)/3600.0
                     IF(FLOWP(4).EQ.0.0) FLOWP(4) = 1.0
                     FLOWP(2) = FLOWP(1)/FLOWP(4)
                     ENDIF
C=======================================================================
C     Unit conversions for pollutant parameters.  For U.S. units,
C        PCONV(1) = 16018.93 is equal to 0.03531 ft3/l * 453592.4 mg/lb
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
      IF(NPR.GT.0) WRITE(NOUT) ((POLLP(K1,J),J=1,5),K1=1,NPR)
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
C     The elapsed time from the beginning to the end of the period of
C     analysis is the difference between T2 and T1.  A rounded value for
C     the number of months in this period can be found using 730.5 as the
C     average number of hours per month.  NOMOS will be used to find the
C     return period for an event of a given magnitude.
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
C#### WCH, 7/23/96.  ADD DEFINITION OF DURATION AND INTEREVENT TIME FOR
C     HYDROGRAPHS.
      IF(LOCRQ.GT.0) WRITE(N6,3501)
      WRITE(*,912)
      IF(LOCRQ.GT.0) THEN
             IF(JCUBE.EQ.0.AND.NPR.EQ.0.AND.METRIC.EQ.1) WRITE(N6,3570)
             IF(JCUBE.GT.0.AND.NPR.EQ.0.AND.METRIC.EQ.1) WRITE(N6,3580)
             IF(JCUBE.EQ.0.AND.NPR.EQ.0.AND.METRIC.EQ.2) WRITE(N6,3575)
             IF(JCUBE.GT.0.AND.NPR.EQ.0.AND.METRIC.EQ.2) WRITE(N6,3585)
	       IF(NPR.GT.0) THEN
	       WRITE(N6,3549) '|<- EMC ----',('------------',K=1,NPR-2),
     +	   '---------->|','<-TOT LOAD -',('------------',K=1,NPR-2),
     +	   '---------->|'
             WRITE(N6,3550) (PNAME(K),K=1,NPR),(PNAME(K),K=1,NPR)
	         IF (METRIC.EQ.1) THEN
	            WRITE(N6,3551) ENGLAB(1),ENGLAB(5),ENGLAB(5),
     +                           (PUNIT(K),K=1,NPR),
     +                           (PAA(K),K=1,NPR)
	         ELSE
	            WRITE(N6,3551) SILAB(1),SILAB(5),SILAB(5),
     +                           (PUNIT(K),K=1,NPR),
     +                           (PAA(K),K=1,NPR)
	         ENDIF
	        WRITE(N6,3552) ('  ----------',K=1,NPR*2)
	       ENDIF
c             IF(NPR.GT.0.AND.METRIC.EQ.1) WRITE(N6,3550)
c     +                       (PNAME(K),K=1,NPR),(PNAME(K),K=1,NPR),
c     +                       ENGLAB(1),ENGLAB(5),(PUNIT(K),K=1,NPR),
c     +                       (PAA(K),K=1,NPR)
c             IF(NPR.GT.0.AND.METRIC.EQ.2) WRITE(N6,3550)
c     +                       (PNAME(K),K=1,NPR),(PNAME(K),K=1,NPR),
c     +                       SILAB(1),SILAB(5),(PUNIT(K),K=1,NPR),
c     +                       (PAA(K),K=1,NPR)
             ELSE
             IF(METRIC.EQ.1) WRITE(N6,3560)
             IF(METRIC.EQ.2) WRITE(N6,3565)
             ENDIF
      REWIND (NOUT)
      FLWSUM    = 0.0
c
c      P1LSUM    = 0.0
c      P2LSUM    = 0.0
c      P3LSUM    = 0.0
c      P4LSUM    = 0.0
c      P5LSUM    = 0.0
      DO IP =1,NPR
	PLSUM(IP) = 0.0
	ENDDO
C#### WCH, 7/23/96.  CHANGE FROM FFF5 = 0 TO FF5 = 0.
      FF5       = 0.0
      DO 3900 I = 1,NEVNTS
      READ(NOUT,END=4040) I1,T1,(FLOWP(I2),I2=1,5)
      IF(NPR.GT.0) READ (NOUT) ((POLLP(K1,J),J=1,5),K1=1,NPR)
      FF1       = FLOWP(1)
      FF4       = FLOWP(4)
C=======================================================================
C#### WCH, 7/23/96.  For rain, must save interevent time for next print,
C     but not for flow.
C=======================================================================
      IF(LOCRQ.GT.0) THEN
                   FFF5 = FLOWP(5)
                   ELSE
                   FFF5 = FF5
                   ENDIF
      FF5       = FLOWP(5)
      IF(NPR.GT.0) THEN
c                   P1EMC     = POLLP(1,4)
c                   P2EMC     = POLLP(2,4)
c                   P3EMC     = POLLP(3,4)
c                   P4EMC     = POLLP(4,4)
c                   P5EMC     = POLLP(5,4)
c                   P1LOAD    = POLLP(1,1)
c                   P2LOAD    = POLLP(2,1)
c                   P3LOAD    = POLLP(3,1)
c                   P4LOAD    = POLLP(4,1)
c                   P5LOAD    = POLLP(5,1)
      DO IP = 1,NPR
	  PEMC(IP) = POLLP(IP,4)
	  PLOAD(IP) = POLLP(IP,1)
        PLSUM(IP) = PLSUM(IP) + POLLP(IP,1)
	ENDDO
c                   P1LSUM    = P1LSUM + POLLP(1,1)
c                   P2LSUM    = P2LSUM + POLLP(2,1)
c                   P3LSUM    = P3LSUM + POLLP(3,1)
c                   P4LSUM    = P4LSUM + POLLP(4,1)
c                   P5LSUM    = P5LSUM + POLLP(5,1)
                   ENDIF
      FLWSUM    = FLWSUM + FLOWP(1)
C#### WCH, 7/23/96.  ADD EVENT ID NUMBER TO THESE PRINTS.
      IF(LOCRQ.GT.0.AND.NPR.GT.0)
c     +               WRITE(N6,3700) I,I1,T1,FF1,FF4,P1EMC,P2EMC,P3EMC,
c     +               P4EMC,P5EMC,P1LOAD,P2LOAD,P3LOAD,P4LOAD,P5LOAD
     +                 WRITE(N6,3700)  I,I1,T1,FF1,FF4,FFF5,
     +				                 (PEMC(IP),IP=1,NPR),
     +								 (PLOAD(IP),IP=1,NPR)
      IF(LOCRQ.GT.0.AND.NPR.EQ.0) WRITE(N6,3701) I,I1,T1,FF1,FF4,FFF5
C#### WCH, 8/1/95.  LOCRN NOW CHARACTER.
C####      IF(LOCRN.GT.0) WRITE(N6,3700) I1,T1,FF1,FF4,FFF5
      IF(LOCRN.NE.' ') WRITE(N6,3701) I,I1,T1,FF1,FF4,FFF5
C=======================================================================
C  ELIMINATE INTERMEDIATE PRINTOUT
c      IF(MOD(I,60).EQ.0) THEN
c         WRITE(N6,3875)
c         IF(LOCRQ.GT.0) THEN
c             IF(JCUBE.EQ.0.AND.NPR.EQ.0.AND.METRIC.EQ.1) WRITE(N6,3570)
c             IF(JCUBE.GT.0.AND.NPR.EQ.0.AND.METRIC.EQ.1) WRITE(N6,3580)
c             IF(JCUBE.EQ.0.AND.NPR.EQ.0.AND.METRIC.EQ.2) WRITE(N6,3575)
c             IF(JCUBE.GT.0.AND.NPR.EQ.0.AND.METRIC.EQ.2) WRITE(N6,3585)
c             IF(NPR.GT.0.AND.METRIC.EQ.1) WRITE(N6,3550)
c     +                       (PNAME(K),K=1,NPR),(PNAME(K),K=1,NPR),
c     +                       ENGLAB(1),ENGLAB(5),(PUNIT(K),K=1,NPR),
c     +                       (PAA(K),K=1,NPR)
c             IF(NPR.GT.0.AND.METRIC.EQ.2) WRITE(N6,3550)
c     +                       (PNAME(K),K=1,NPR),(PNAME(K),K=1,NPR),
c     +                       SILAB(1),SILAB(5),(PUNIT(K),K=1,NPR),
c     +                       (PAA(K),K=1,NPR)
c             ELSE
c             IF(METRIC.EQ.1) WRITE(N6,3560)
c             IF(METRIC.EQ.2) WRITE(N6,3565)
c             ENDIF
c         ENDIF
3900  CONTINUE
C=======================================================================
C     Check if program should be terminated after printing
C     series; rewind off-line file; proceed with event
C     analysis for flow parameters.
C=======================================================================
4040  CONTINUE
        REWIND (NOUT)
      IF(KSEQ.GT.0.AND.LOCRQ.GT.0) THEN
C#### WCH, 11/30/93.  PRINT TOTAL LOADS FOR U.S. AND METRIC.
c               IF(NPR.GT.0) WRITE(N6,6024) P1LSUM,P2LSUM,P3LSUM,
c     +                                         P4LSUM, P5LSUM
               IF(NPR.GT.0) THEN 
	         WRITE(N6,6022)
	         WRITE(N6,6023) ('            ',IP = 1,NPR)
			 WRITE(N6,6024) (PLSUM(IP),IP=1,NPR)
	         ENDIF
               IF (JCE.EQ.0) THEN
                 IF(METRIC.EQ.1) WRITE(N6,6025) LOCRQ,FLWSUM,ENGLAB(1)
                 IF(METRIC.EQ.2) WRITE(N6,6025) LOCRQ,FLWSUM,SILAB(1)
                 ELSE
                 IF(METRIC.EQ.1) WRITE(N6,6026) KOCRQ,FLWSUM,ENGLAB(1)
                 IF(METRIC.EQ.2) WRITE(N6,6026) KOCRQ,FLWSUM,SILAB(1)
               ENDIF
               ENDIF
C#### WCH, 8/1/95.  LOCRN NOW CHARACTER.
C####      IF(KSEQ.GT.0.AND.LOCRN.GT.0) THEN
      IF(KSEQ.GT.0.AND.LOCRN.NE.' ') THEN
               IF(METRIC.EQ.1) WRITE(N6,6030) LOCRN,FLWSUM,ENGLAB(1)
               IF(METRIC.EQ.2) WRITE(N6,6030) LOCRN,FLWSUM,SILAB(1)
               ENDIF
      IF(NEXIT.EQ.1) RETURN
      DO 4100 N = 1,NEVNTS
C#### WCH, 2/27/95. READ IEVNTB,TEVNTB AGAIN TO AVOID CORRUPTED VALUES?
      READ(NOUT,END=4140) IEVNTB(N),TEVNTB(N),(PARAM(N,I),I=1,5)
C####      READ(NOUT,END=4140) I1,T1,(PARAM(N,I),I=1,5)
      IF(NPR.GT.0) READ(NOUT) ((PDUM,J=1,5),K1=1,NPR)
4100  CONTINUE
4140  CONTINUE
      DO 4600 J = 1,5
      IF(IFPAR(J).EQ.0) GO TO 4600
                     JMB = 1
                     J1  = 1
C#### WCH, 8/1/95.  LOCRN NOW CHARACTER.
C####      IF(LOCRN.GT.0) THEN
      IF(LOCRN.NE.' ') THEN
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
      REWIND (NOUT)
      JJ        = IPOLRQ(K)
      DO 4750 N = 1,NEVNTS
      READ(NOUT,END=4760) I1,T1,(QDUM,I=1,5)
      IF(NPR.GT.0) THEN
                   READ(NOUT) ((POLLP(K1,J),J=1,5),K1=1,NPR)
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
      IF (LLOCRQ.LT.0.AND.ILOCRQ.LT.LOCATS) GO TO 8100
      RETURN
9070  WRITE(N6,9080)
      RETURN
C=======================================================================
34    FORMAT(//,
     1' ######################################',/,
     1' # Entry made to the Stats Block      #',/,
     1' # Statistical Analysis Block written #',/,
     1' #   by the University of Florida.    #',/,
     1' #   Last updated Dec. 1996 at OSU.   #',/,
     1' # See data examples or STATS.DOC for #',/,
     1' # information on new parameters not  #',/,
     1' # included in User''s Manual.         #',/,
     1' ######################################',/)
C#### WCH, 7/21/93.  SLIGHT CHANGE FOR PRINT OF LRET.
C#### WCH, 7/23/96.  CORRECT DEFINITION OF JCUBE FOR VOLUMES.
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
     1 1X,'or ft^3 or m^3 flow values  JCUBE = 1.',I10,/
	1 1X,'Process positive flow events JNEG = 0,',/,
	1 1x,'or negative flow events      JNEG = 1,',I10,/)
C    1 1X,'Use storm event definition  JSEA  = 0,',/,
C    1 1X,' or monthly events          JSEA  = 1,',/,
C    1 1X,' or seasonal events         JSEA  = 2.',I10,/)
C#### WCH, 7/23/96.  MODIFY THIRD LINE.
   93 FORMAT(
     1 1X,'Print sequential series (KSEQ)..............',I10,/,
     1 1X,'Terminate program parameter (KTERM).........',I10,/,
     1 1X,'Print series if reach event limit (KTSEQS)..',I10,/)
  100 FORMAT(//,
     +' ##################################################',/,
     +' #  The period of time for which the statistical  #',/,
     +' #              analysis is being performed is:   #',/,
     +' ##################################################',//,
     +'  Starting date...............',I8,'  Starting time...',F5.2,
     +' hours',/,
     +'  Ending date.................',I8,'  Ending time.....',F5.2,
     +' hours')
C#### WCH, 7/21/93.  ALTER FORMAT FOR OUTPUT OF SECONDS.
101   FORMAT(//,
     1' ##################################################',/,
     1' #  The period of time for which the statistical  #',/,
     1' #              analysis is being performed is:   #',/,
     1' ##################################################',//,
     1'  Starting Julian date........',I8,'  Starting time...',F6.0,
     1' seconds',/,
     1'  Ending Julian date..........',I8,'  Ending time.....',F6.0,
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
     3' * Analysis is: ',I10,'                            *',/,
     4' ******************************************************')
1110  FORMAT(/,
     1' ******************************************************',/,
     2' * The flow location requested for Statistical        *',/,
     3' * Analysis is: ',A10,'                            *',/,
     4' ******************************************************')
2110   FORMAT(/,
     1' ******************************************************',/,
     2' * The following analysis is for location number :    *',/,
     3' * ',I10,'                                         *',/,
     4' ******************************************************')
2111  FORMAT(/,
     1' ******************************************************',/,
     2' * The following analysis is for location number :    *',/,
     3' * ',A10,'                                         *',/,
     4' ******************************************************')
C#### WCH, 8/1/95.  CHANGE I7 TO A8.
111   FORMAT(/,
     1' ******************************************************',/,
     2' * The rain location number requested for Statistical *',/,
     3' * Analysis is: ',A8,'                               *',/,
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
     1' identified by number, are as follows: ',99I3)
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
C   next format not used
C1610  FORMAT(I6,F5.2,5G10.4,10(5G10.4))
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
C#### WCH, 8/1/95.  CHANGE I13 TO A13
 2120 FORMAT(' Location Station number',/,
     +       ' -------- --------------',/,
     +       10(I9,'. ',A13,/))
 3500 FORMAT('1',/,15X,27(1H#),/,15X,'Sequential Series of Events',
     +           /,15X,27(1H#),/)
 3501 FORMAT(' Note: SWMM hydrographs are instantaneous values at',
     1' given time.',/,
     +' Event duration is from first non-zero flow to trailing',
     +' hydrograph zero.',/,
     +' Interevent duration is from trailing hydrograph zero to next',
     +' non-zero flow')
c     +' (not printed when quality is printed).',/)
C#### WCH, 7/23/96.  ADD EVENT NO. TO THIS PRINT-OUT AND FIX UNITS FMT.
 3549 FORMAT(27X,'Flow      Event  Interevent',2X,99(A12))
C     +'|<----------------------------EMC------------------------->|',
C     +'<----------------------Total Load----------------------->|')
 3550 FORMAT(' Event',12X,'Time   Volume   Duration    Duration',
     +99(4X,A8))
 3551 FORMAT('   No.   Date    (hour)',1X,A8,2X,A8,4X,A8,
     +99(4X,A8))
 3552 FORMAT('  ----   ----    ------ --------  --------   ---------',
     +99(A12))
c     +'   ---------  ----------  ----------   ---------   ---------',
c     +'   ---------   ---------  ---------   ---------')
C#### WCH, 7/23/96.  ALTER 3560-3585 FOR EVENT NO. PRINT-OUT.
3560  FORMAT('                            Rain      Event  Interevent'
     + ,/,
     +     ' Event           Time     Volume   Duration    Duration',/,
     +     '   No.   Date   (hour)   (inches)   (hours)     (hours)',/,
     +     '  ----   ----   ------  --------    -------     -------')
3565  FORMAT('                            Rain      Event  Interevent'
     + ,/,
     +     ' Event           Time     Volume   Duration    Duration',/,
     +     '   No.   Date   (hour)      (mm)    (hours)     (hours)',/,
     +     '  ----   ----   ------  --------    -------     -------')
3570  FORMAT('                            Flow      Event  Interevent'
     + ,/,
     +     ' Event           Time     Volume   Duration    Duration',/,
     +     '   No.   Date   (hour)   (inches)   (hours)     (hours)',/,
     +     '  ----   ----   ------  --------    -------     -------')
3575  FORMAT('                            Flow      Event  Interevent'
     + ,/,
     +     ' Event           Time     Volume   Duration    Duration',/,
     +     '   No.   Date   (hour)      (mm)    (hours)     (hours)',/,
     +     '  ----   ----   ------  --------    -------     -------')
3580  FORMAT('                            Flow      Event  Interevent'
     + ,/,
     +     ' Event           Time     Volume   Duration    Duration',/,
     +     '   No.   Date   (hour)   ( ft^3 )   (hours)     (hours)',/,
     +     '  ----   ----   ------  --------    -------     -------')
3585  FORMAT('                            Flow      Event  Interevent'
     + ,/,
     +     ' Event           Time     Volume   Duration    Duration',/,
     +     '   No.   Date   (hour)  (meter^3)   (hours)     (hours)',/,
     +     '  ----   ----   ------  --------    -------     -------')
C#### WCH, 7/23/96.  ADD EVENT NO. AND CHANGE EMC ETC. PRINTS TO
C     G-FORMAT, FROM F-FORMAT.
3700  FORMAT(I4,2X,I8,1X,F8.2,1X,1PG9.3,0PF9.2,F12.2,99(3X,1PG9.3))
C#### WCH, 7/23/96.  ADD FORMAT FOR EVENT NO. PRINT-OUT.
3701  FORMAT(I4,2X,I8,F8.2,2X,1PG9.3,0PF9.2,F12.2)
3875  FORMAT('1',/,15X,40(1H#),/,15X,
     1'Sequential Series of Events  (continued)',/,15X,40(1H#),/)
C#### WCH, 8/7/95.  THIS FORMAT NOT USED??
C#### 4050  FORMAT(11X,5G10.4)
6022  FORMAT('      Total Loads (see to right)--->:',
     1'                 ',$)
6023  FORMAT(A12,$)
6024  FORMAT(3X,1PG9.3,99(3X,G9.3))
6025  FORMAT(//,
     1' ************************************************************',/,
     2' * Total Flow at location ',I10,'is ',1PG11.4,' ',A9,'*',/,
     3' ************************************************************',
     4//)
6026  FORMAT(//,
     1' ************************************************************',/,
     2' * Total Flow at location ',A10,'is ',1PG11.4,' ',A9,'*',/,
     3' ************************************************************',
     4//)
C#### WCH, 8/1/95.  CHANGE I9 TO A9 FOR STATION ID.
6030  FORMAT(//,
     1' ************************************************************',/,
     2' * Total Rain at location ',A9,' is ',1PG11.4,' ',A9,' *',/,
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
C#### WCH, 8/1/95.  CHANGE I9 TO A9 AND ENHANCE ERROR MESSAGE.
 9030 FORMAT(/,' WARNING!! Rainfall location (LOCRN)......... ',A8,/,
     + ' is not located on the rainfall interface file.',/,
     + ' Stats continues, using last station on rainfall interface file
     += ',A8,/,' Possible cause is mis-match of alphanumeric station IDs
     +.',/,' Try inputting LOCRN in exactly the same form as ISTA was in
     +put in the Rain Block,',/,' including leading/trailing blanks.')
C#### WCH, 8/1/95.  NEW ERROR MESSAGE.
 9040 FORMAT(/,' ==> WARNING!  Rainfall station ID, LOCRN, should be inp
     1ut as alphanumeric',/,' value (8/1/95).  Your integer input =',I9,
     2 ' entered successfully',/,' and converted to alphanumeric value,
     3but please use correct',/,' alphanumeric format in future.')
C#### WCH, 8/7/95.
 9455 FORMAT(' ERROR! BEGINNING JULIAN DATE =',I9,' ON UNIT',I3,' IS LAT
     +ER THAN',/,
     + ' REQUESTED ENDING JULIAN DATE =',I9,' ENTERED ON A1 LINE.',/,
     + ' (DATES INCLUDE CONSIDERATION OF TIME OF DAY.)',/,
     + ' STATS BLOCK ENDED.  RETURN TO MAIN PROGRAM.')
 9080 FORMAT(/,' ===> Error reading rainfall interface file.')
CIM  PROCESS ALL FILES
 8005 FORMAT('LOCRQ ENTERED AS NEGATIVE NUMBER. ALL FLOW LOCATIONS IN',
     +' THE INTERFACE FILE WILL BE PROCESSED')
C=======================================================================
 888  CALL IERROR
      END

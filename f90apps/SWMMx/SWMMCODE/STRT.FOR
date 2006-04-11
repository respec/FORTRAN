      SUBROUTINE STRT
C=======================================================================
C     STORAGE/TREATMENT MODEL  -- JUNE 1981. (UPDATED JANUARY 1988)
C
C     MODEL WRITTEN BY STEPHAN J. NIX
C     UPDATED AND MODIFIED BY ROBERT E. DICKINSON
C                      ENVIRONMENTAL ENGINEERING SCIENCES
C                      UNIVERSITY OF FLORIDA
C                      GAINESVILLE, FLORIDA 32611
C     CURRENT (5/93) PHONE : 813-886-7724 AT XP SOFTWARE
C     UPDATE HEADER, 5/93 BY WCH AT OREGON STATE UNIVERSITY
C     ROB JAMES AND WCH, 8/4/93.  FIX METRIC CONVERSION WHILE 
C       WRITING TO INTERFACE FILE.
C     WCH (RED), 9/23/93.  CHANGE A FORMAT FROM 2A4 TO A8.
C=======================================================================
C     NOTE, WHEN METRIC OPTION IS USED, THE S/T BLOCK USES METRIC UNITS
C       INTERNALLY.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'S1.INC'
C=======================================================================
      DIMENSION ANS(2),IPOLL(3),PCAR(3),PCAR1(3),TEMP(12),ALPHA(2)
      DIMENSION POLC(3),POLCL(3),POLL(10,NIE),POLL1(10,NIE)
      DIMENSION SPG(10),VIS(12),QO1(NIE),QO(NIE)
      CHARACTER ANS*3,SOUR(3)*40,DMON(12)*4,PDUM1*2,ALPHA*80
      DATA ANS/' NO','YES'/
      DATA DMON/' JAN',' FEB',' MAR',' APR',' MAY','JUNE','JULY',' AUG',
     1          'SEPT',' OCT',' NOV',' DEC'/,PDUM1/'TO'/
C=======================================================================
C     Define statement function for linear interpolation.
C=======================================================================
      QLINTP(Q1,Q2,T1,T2,T) = Q1 + (Q2-Q1)*(T-T1)/(T2-T1)
C=======================================================================
      SOUR(1) = 'EXTERNAL FILE'
      SOUR(2) = 'INPUT ON LINE J1'
      SOUR(3) = 'INPUT ON LINE J1 AND EXTERNAL FILE'
C=======================================================================
      WRITE(*,150)
      WRITE(N6,150)
      LOCATS = 1
      KDT    = 0
      IPT    = 0
      INCNT  = INCNT  + 1
      IOUTCT = IOUTCT + 1
      LAST   = JIN(INCNT)
      NEXT   = JOUT(IOUTCT)
C=======================================================================
C     Open all input/output files for the Storage/Treatment Block.
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
C     MULTIPLY FLOWS ON INTERFACE FILE BY QCONV TO OBTAIN CFS.
C     QCONV IS INPUT FROM PRIOR BLOCKS OR ELSE ALTERED LATER IF
C       METRIC OPTION IS USED.
C=======================================================================
      QCONV  = 1.0
      WRITE(*,9500)
C=======================================================================
C>>>>>>>>  READ DATA GROUP A1  <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,TITLE(1)
      READ(N5,*,ERR=888) CC,TITLE(2)
      ALPHA(1) = TITLE(1)
      ALPHA(2) = TITLE(2)
      TITLE(3) = TITLE(1)
      TITLE(4) = TITLE(2)
C=======================================================================
C>>>>>>>>  READ DATA GROUP B1  <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,NOTAPE,JNS,NDT,DS,NU,NP,ICOST,METRIC,TRIBA
      METRIC = METRIC + 1
      IF(NOTAPE.EQ.1) THEN
                      NLOC(1) = JNS
                      JS      = 1
                      ENDIF
      IF(NU.LE.0)    NU    = 1
      IF(ICOST.GT.1) ICOST = 1
C=======================================================================
C>>>>>>>>  READ DATA GROUP C1 AND C2  <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,IDATE,TIME,ISUM,IDET,NPR
      IF(ISUM.GT.2) ISUM=2
      TZERO    = TIME*3600.0
      TIMDAY   = TZERO
      TIME     = TZERO
      TCAR1    = TIME/3600.0
      QCAR1    = 0.0
      PCAR1(1) = 0.0
      PCAR1(2) = 0.0
      PCAR1(3) = 0.0
      NYEAR   = IDATE/10000
      NDAY    = IDATE - NYEAR*10000
      MONTH   = NDAY/100
      NDAY    = NDAY - MONTH*100
      IF(NDAY.LE.0) NDAY   = 2
      IF(MONTH.LE.0) MONTH = 8
      IF(NYEAR.LT.0) NYEAR = 41
      JULDAY               = 1000*NYEAR + JDATE(NDAY,MONTH,NYEAR)
      CALL DATED
      NBD(1) = NYEAR
      NBD(2) = MONTH
      NBD(3) = NDAY
      NBD(4) = JHR
      NBD(5) = MINUTE
      NBD(6) = JSEC
      JDAY   = JULDAY
      TMDAY  = TIMDAY
C=======================================================================
      IF(IDET.GT.0) READ(N5,*,ERR=888) CC,(ISTART(L),IEND(L),L=1,NPR)
C=======================================================================
      DO 1000 L = 1,NPR
      N1        = ISTART(L)/10000
      N3        = ISTART(L) - N1*10000
      N2        = N3/100
      N3        = N3 - N2*100
      ISTART(L) = 1000*N1 + JDATE(N3,N2,N1)
      N1        = IEND(L)/10000
      N3        = IEND(L) - N1*10000
      N2        = N3/100
      N3        = N3 - N2*100
      IEND(L)  = 1000*N1 + JDATE(N3,N2,N1)
 1000 CONTINUE
C=======================================================================
C>>>>>>>>  READ DATA GROUP D1  <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,(E(NM),NM=1,12)
      IF(NP.EQ.0) GO TO 2000
C=======================================================================
C>>>>>>>>  READ DATA GROUP E1  <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,(IPOLL(IP),NDIM(IP),IPART(IP),
     1                       PNAME(IP),PUNIT(IP),IP=1,NP)
      DO 1420 IP = 1,NP
      IPT        = IPT+IPART(IP)
      IF(NOTAPE.EQ.1) IPOLL(IP) = IP
 1420 CONTINUE
C=======================================================================
C>>>>>>>>  READ DATA GROUP E2 AND E3  <<<<<<<<
C=======================================================================
      IF(IPT.LE.0) GO TO 2000
      READ(N5,*,ERR=888) CC,NVS,NNR
      READ(N5,*,ERR=888) CC,(RAN(KJ,1),RAN(KJ,2),KJ=1,NNR)
      IF(NVS.GT.0) THEN
                   DO 1450 KJ = 1,NNR
 1450              VS(KJ) = (RAN(KJ,1)+RAN(KJ,2))/2.0
                   ELSE
C=======================================================================
C>>>>>>>>  READ DATA GROUP E4 AND E5  <<<<<<<<
C=======================================================================
                   READ(N5,*,ERR=888) CC,(SPG(KJ),KJ=1,NNR)
                   READ(N5,*,ERR=888) CC,(TEMP(NM),NM=1,12)
                   DO 1470 NM = 1,12
                   IF(METRIC.EQ.1) VIS(NM)=8.46E-4/(TEMP(NM)+10.0)
 1470              IF(METRIC.EQ.2) VIS(NM)=0.78596/(1.8*TEMP(NM)+42.0)
                   ENDIF
C=======================================================================
C>>>>>>>>  READ DATA GROUP E6  <<<<<<<<
C=======================================================================
      DO 1495 IP = 1,NP
      IF(IPART(IP).LE.0) GO TO 1495
      READ(N5,*,ERR=888) CC,(PSD(IP,KJ),KJ=1,NNR)
      TOTPSD     = 0.0
      DO 1485 KJ = 1,NNR
 1485 TOTPSD     = TOTPSD+PSD(IP,KJ)
      IF(TOTPSD.LT.0.99999.OR.TOTPSD.GT.1.00001) THEN
                                     WRITE(N6,1490) IPOLL(IP)
                                     STOP
                                     ENDIF
 1495 CONTINUE
C=======================================================================
C     READ INPUT INTERFACE FILE HEADER INFORMATION
C=======================================================================
 2000 IF(NOTAPE.NE.1) CALL INFACE(1,LAST)
      IF(NP.GT.0) THEN
                  DO 2270 IP = 1,3
                  IF(IP.GT.NP) GO TO 2270
                  IF(METRIC.EQ.1) THEN
                            PCONV(IP) = 16017.0
                            IF(NDIM(IP).EQ.1) PCONV(IP)=3.5315E-2
                            ELSE
                            PCONV(IP) = 1000.0
                            IF(NDIM(IP).EQ.1) PCONV(IP)=0.001
                            ENDIF
                  IF(NDIM(IP).GE.2) PCONV(IP) = 1.0
 2270             CONTINUE
                  ENDIF
C=======================================================================
C     WRITE THE OUTPUT INTERFACE HEADER INFORMATION
C=======================================================================
      IF(NEXT.GT.0) THEN
C#### WCH, 8/4/93. SET QCONV FOR METRIC IF S/T IS FIRST BLOCK RUN.
                    IF(METRIC.EQ.2.AND.NOTAPE.EQ.1) QCONV=35.31
                    REWIND NEXT
                    SOURCE   = 'S/T BLOCK'
                    TITLE(3) = ALPHA(1)
                    TITLE(4) = ALPHA(2)
                    WRITE(NEXT) LOCATS,NP
                    WRITE(NEXT) (NLOC(JJS),JJS=1,LOCATS)
                    CALL INFACE(2,NEXT)
                    ENDIF
C=======================================================================
C      LOCATE INPUT INTERFACE NODE LOCATIONS
C=======================================================================
 2500 IF(NOTAPE.NE.1) THEN
                      DO 2550 JS = 1,LOCATS
                      IF(NLOC(JS).EQ.JNS) GO TO 3000
 2550                 CONTINUE
                      WRITE(N6,2570) JNS,LAST
                      STOP
                      ENDIF
C=======================================================================
 3000 WRITE(N6,3010) SOUR(NOTAPE+1)
      WRITE(N6,3020)  JNS,NDT,DS,NU,ANS(ICOST+1),MONTH,NDAY,NYEAR,JHR,
     +                MINUTE,JSEC
      IF(METRIC.EQ.1) WRITE(N6,3030) TRIBA
      IF(METRIC.EQ.2) WRITE(N6,3040) TRIBA
      WRITE(N6,3060) NP
      IF(NP.GT.0) THEN
                  DO 3070 IP = 1,NP
 3070             WRITE(N6,3080)IP,PNAME(IP),PUNIT(IP),ANS(IPART(IP)+1)
                  ENDIF
C
      IF(METRIC.EQ.1) WRITE(N6,3110)
      IF(METRIC.EQ.2) WRITE(N6,3120)
      WRITE(N6,3130) (DMON(NM),NM=1,12),(E(NM),NM=1,12)
C
      IF(IPT.LE.0) GO TO 4000
      IF(NVS.LE.0) WRITE(N6,3210)
      IF(NVS.GT.0) WRITE(N6,3220)
      WRITE(N6,3230) (RAN(KJ,1),KJ=1,NNR)
      IF(NVS.LE.0) WRITE(N6,3240) (PDUM1,KJ=1,NNR)
      IF(METRIC.EQ.1.AND.NVS.GT.0) WRITE(N6,3250) (PDUM1,KJ=1,NNR)
      IF(METRIC.EQ.2.AND.NVS.GT.0) WRITE(N6,3255) (PDUM1,KJ=1,NNR)
      WRITE(N6,3230) (RAN(KJ,2),KJ=1,NNR)
      IF(NVS.EQ.0) WRITE(N6,3260) (SPG(KJ),KJ=1,NNR)
      DO 3280 IP = 1,NP
 3280 WRITE(N6,3290) PNAME(IP),(PSD(IP,KJ),KJ=1,NNR)
C
 3300 IF(NVS.GT.0)   GO TO 4000
      WRITE(N6,3350) (DMON(NM),NM=1,12)
      IF(METRIC.EQ.1) WRITE(N6,3370)(TEMP(NM),NM=1,12),(VIS(NM),NM=1,12)
      IF(METRIC.EQ.2) WRITE(N6,3390)(TEMP(NM),NM=1,12),(VIS(NM),NM=1,12)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 4000 WRITE(*,9510)
      CALL STRDAT
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      QOL     = 0.0
      QCARL   = 0.0
      QO(JS)  = 0.0
      QO1(JS) = 0.0
      IF(NP.GT.0) THEN
                  DO 6010 IP = 1,NP
 6010             POLCL(IP)  = 0.0
                  ENDIF
C=======================================================================
C     MAIN TIME LOOP
C=======================================================================
      WRITE(*,23) NDT
      DO 9000 KDT = 1,NDT
      WRITE(*,22)   KDT
      TIME        = TIME + DS
      TIMEHR      = TIME/3600.0
      CALL STIME(DS)
      CALL DATED
      IF(NP.GT.0) THEN
                  DO 6110 IP  = 1,NP
                  POLC(IP)    = 0.0
                  PCRC(IP)    = 0.0
 6110             PCTP(IP)    = 0.0
                  ENDIF
C=======================================================================
C>>>>>>>>  READ DATA GROUP J1  <<<<<<<<
C=======================================================================
      IF(NOTAPE.GE.1) THEN
 7050           IF(TIMEHR.GT.TCAR1) THEN
                   TCAR = TCAR1
                   QCAR = QCAR1
                   IF(NP.GT.0) THEN
                               PCAR(1) = PCAR1(1)
                               PCAR(2) = PCAR1(2)
                               PCAR(3) = PCAR1(3)
                               READ(N5,*,ERR=888) CC,TCAR1,QCAR1,
     +                                        (PCAR1(IP),IP=1,NP)
                               ELSE
                               READ(N5,*,ERR=888) CC,TCAR1,QCAR1
                               ENDIF
                   IF(TCAR1.LT.TIMEHR) GO TO 7050
                   ENDIF
                QCARD = QLINTP(QCAR,QCAR1,TCAR,TCAR1,TIMEHR)
                IF(NP.GT.0) THEN
                            DO 6210 IP = 1,NP
                            QQ1      = QCAR*PCAR(IP)
                            QQ2      = QCAR1*PCAR1(IP)
                            POLC(IP) = QLINTP(QQ1,QQ2,TCAR,TCAR1,TIMEHR)
 6210                       CONTINUE
                            ENDIF
                ENDIF
C=======================================================================
C     READ INTERFACE FILE FLOWS AND LOADS
C=======================================================================
      IF(NOTAPE.NE.1) THEN
 6400 IF(JULDAY.GT.JDAY.OR.(JULDAY.EQ.JDAY.AND.TIMDAY.GE.TMDAY)) THEN
              QO(JS)    = QO1(JS)
              IF(NP.GT.0) THEN
                          DO 4520 J  = 1,NP
 4520                     POLL(J,JS) = POLL1(J,JS)
                          ENDIF
      IF(NQUAL.EQ.0) READ(LAST,END=999) JDAY,TMDAY,DELTA,
     +                                  (QO1(J),J=1,LOCATS)
      IF(NQUAL.GT.0) READ(LAST,END=999) JDAY,TMDAY,DELTA,
     +                    (QO1(J),(POLL1(IP,J),IP=1,NQUAL),J=1,LOCATS)
      GO TO 2995
  999 QO(JS)      = 0.0
      QO1(JS)     = 0.0
      DO 2990  J  = 1,NQUAL
      POLL1(J,JS) = 0.0
      POLL(J,JS)  = 0.0
 2990 CONTINUE
      WRITE(N6,9200) TIME/3600.0
      JDAY       = 999999
      TMDAY      = 0.0
 2995 CONTINUE
      TREF = TIMEHR
      CALL NTIME(JDAY,TMDAY,TFILE)
      IF(TFILE.LT.0.0) GO TO 6400
      TFILE = TFILE/3600.0
      IF(NQUAL.GT.0.AND.NP.GT.0) THEN
                      DO 6410 IP   = 1,NP
                      IPX          = IPOLL(IP)
C=======================================================================
C     If METRIC = 2, internal flows are in cms and loads are cms*concen.
C     Thus, multiply by 2.8317E-2 because will multiply by QCONV later.
C     QCONV = 35.31 cfs/cms = 1 / 0.28317E-2 when metric flows are on 
C     interface file.  Thus, interface flows and loads are converted 
C     properly whether in cfs or cms.  
C=======================================================================
                      IF(METRIC.EQ.2) POLL1(IPX,JS) =
     +                                POLL1(IPX,JS)*2.8317E-2
 6410                 POLL1(IPX,JS) = POLL1(IPX,JS)*QCONV
                      ENDIF
      IF(METRIC.EQ.2) QO1(JS) = QO1(JS)*2.8317E-2
      ENDIF
C=======================================================================
      THR = TIMEHR - TREF
      QQ1 = QO(JS)
      QQ2 = QO1(JS)
      IF(TFILE.EQ.0.0) QOO = QQ2*QCONV
      IF(TFILE.GT.0.0) QOO = QLINTP(QQ1,QQ2,0.0,TFILE,THR)*QCONV
      IF(NP.GT.0) THEN
                  DO 4530 IP = 1,NP
                  IPX        = IPOLL(IP)
                  QQ1        = POLL(IPX,JS)
                  QQ2        = POLL1(IPX,JS)
                  IF(TFILE.EQ.0.0) POLC(IP) = POLC(IP) + QQ2*QCONV
                  IF(TFILE.GT.0.0) POLC(IP) = POLC(IP) +
     +                         QLINTP(QQ1,QQ2,0.0,TFILE,THR)*QCONV
 4530             CONTINUE
                  ENDIF
      ENDIF
C=======================================================================
C     Calculate average flow over the time interval.
C=======================================================================
      QQTP  = (QCARD + QCARL + QOO + QOL)/2.0
C=======================================================================
C     Save old flow values.
C=======================================================================
      QCARL = QCARD
      QOL   = QOO
C=======================================================================
C     CALCULATE AVERAGE CONCENTRATION OVER THE TIME INTERVAL
C=======================================================================
      IF(NP.GT.0) THEN
                  DO 6470 IP = 1,NP
                  IPX        = IPOLL(IP)
                  IF(METRIC.EQ.2) POLL(IPX,JS) = POLL(IPX,JS)*2.8317E-2
                  IF(QQTP.GT.0.0) PCTP(IP)     = (POLC(IP) + POLCL(IP))
     +                                                     / (2.0*QQTP)
                  POLCL(IP) = POLC(IP)
 6470             CONTINUE
                  ENDIF
C=======================================================================
C     COMPUTE SETTLING VELOCITIES (TO LABEL 6580).  THIS ROUTINE
C     WAS DEVELOPED BY M.B. SONNEN, SEPTEMBER 1977.
C=======================================================================
      IF(IPT.LE.0.OR.NVS.GT.0)      GO TO 6600
      IF(LTM.EQ.MONTH.AND.KDT.GT.1) GO TO 6600
      IF(METRIC.EQ.2) VIS(MONTH) = VIS(MONTH)/929.03
      DO 6580 KJ = 1,NNR
      PSIZE      = (RAN(KJ,1)+RAN(KJ,2))/(2.0*304800.0)
      IF(SPG(KJ).LT.1.0) SPG(KJ) = 1.0
      CON1   = (4.0/3.0)*PSIZE*32.2*(SPG(KJ)-1.0)
      CD     = 0.34
      VS(KJ) = SQRT(CON1/CD)
      R      = VS(KJ)*PSIZE/VIS(MONTH)
      IF(R.GT.3000.0) GO TO 6580
      CD     = 1.0
      VS(KJ) = SQRT(CON1/CD)
      R      = VS(KJ)*PSIZE/VIS(MONTH)
      IF(R.GT.100.0) GO TO 6510
      CD     = 10.0
      VS(KJ) = SQRT(CON1/CD)
      R      = VS(KJ)*PSIZE/VIS(MONTH)
      IF(R.GT.2.0) GO TO 6510
      CD     = 200.0
      VS(KJ) = SQRT(CON1/CD)
      R      = VS(KJ)*PSIZE/VIS(MONTH)
      IF(R.GT.0.1) GO TO 6510
      GO TO 6570
 6510 KN = 0
 6520 KN = KN+1
      R  = VS(KJ)*PSIZE/VIS(MONTH)
      IF(R.LT.0.1) GO TO 6570
      F = CD-(24.0*VIS(MONTH)/PSIZE)/VS(KJ)-(3.0*(VIS(MONTH)/PSIZE)**
     1    0.5)/VS(KJ)**0.5-0.34
      IF(ABS(F).LT.0.005*CD) GO TO 6580
      IF(KN.GT.1)            GO TO 6530
      FL = F
      IF(ABS(FL).LT.0.0049)      GO TO 6580
 6530 IF(ABS(F).GT.10.0*ABS(FL)) GO TO 6540
                      CD1 = CD-F
      IF(CD1.LT.0.34) CD1 = 0.34
      CD     = CD1
      VS(KJ) = SQRT(CON1/CD)
      IF(KN.LT.10) GO TO 6520
 6540 WRITE(N6,6550) RAN(KJ,1),RAN(KJ,2)
      STOP
 6570 VS(KJ) = (32.2/18.0)*(SPG(KJ)-1.0)*PSIZE**2.0/VIS(MONTH)
 6580 CONTINUE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 6600 CALL CONTRL
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 6700 IF(NEXT.GT.0) THEN
C#### ROB JAMES (BY WCH), 8/4/93.  REMOVE THIS CONVERSION IN ORDER
C####   TO PLACE METRIC FLOWS (M^3/SEC) ON INTERFACE FILE.
CC      IF(METRIC.EQ.2) QQRC = QQRC/2.8317E-2
      QO(JS)    = QQRC
      DO 6720 J = 1,LOCATS
      IF(J.EQ.JS) GO TO 6720
      QO(J)     = QO1(J)
 6720 CONTINUE
      IF(NP.GT.0) THEN
                  DO 6730 IP   = 1,NP
 6730             POLL1(IP,JS) = PCRC(IP)*QQRC
                  WRITE(NEXT) JULDAY,TIMDAY,DS,(QO(J),(POLL1(IP,J),
     1                        IP=1,NP),J=1,LOCATS)
                  ELSE
                  WRITE(NEXT) JULDAY,TIMDAY,DS,(QO(J),J=1,LOCATS)
                  ENDIF
      ENDIF
 9000 CONTINUE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF(ICOST.GT.0) CALL STCOST
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      WRITE(*,8050)
      WRITE(N6,8050)
C=======================================================================
   22 FORMAT('+',6X,I5)
   23 FORMAT(/,' Beginning loop thru ',I6,' Time steps',/,
     +         ' Time step # ',/)
  150 FORMAT(/,' ###########################################',/,
     1         ' # Entry made to Storage/Treatment Block.  #',/,
     2         ' # Storage/Treatment model written by the  #',/,
     3         ' # University of Florida,  June 1981.      #',/,
     4         ' # Last Updated Aug. 1993 at Oregon St. U. #',/,
     5         ' ###########################################',/)
 1490 FORMAT(/,' ===> ERROR !! IN DATA GROUP E6 : THE PARTICLE SIZE OR V
     1ELOCITY DISTRIBUTION FOR POLLUTANT ',I1,' DOES NOT SUM TO 1.0.',/,
     232X,'SIMULATION TERMINATED.')
 2570 FORMAT(/,' ===> ERROR !! IN DATA GROUP B1 : THE SELECTED EXTERNAL EL
     1EMENT NUMBER, ',I4,' IS NOT AVAILABLE FROM',/,32X,'DATA-SET ',I3,'
     2.  SIMULATION TERMINATED.')
 3010 FORMAT(/,10X,'INPUT DATA SOURCE         :',2X,A40)
 3020 FORMAT(/,10X,'EXTERNAL ELEMENT NUMBER   :',I10,//,
     1         10X,'NUMBER OF TIME STEPS      :',I10,//,
     2         10X,'TIME STEP SIZE,SECONDS    :',F10.1,//,
     3         10X,'NUMBER OF S/T UNITS       :',I10,//,
     4         10X,'COST MODEL USED?          :',7X,A3,//,
     5         10X,'STARTING DATE             :',4X,I2,'/',I2,'/',I2,//,
     6         10X,'STARTING TIME             :',2X,I3,':',I3,':',I3)
 3030 FORMAT(/,10X,'INPUT/OUTPUT UNITS        :   U.S. CUSTOMARY',//,
     1         10X,'TRIBUTARY AREA,ACRES      :',F10.1)
 3040 FORMAT(/,10X,'INPUT/OUTPUT UNITS        :      METRIC',//,
     1         10X,'TRIBUTARY AREA,ACRES      :',F10.1)
 3060 FORMAT(/,10X,'NUMBER OF POLLUTANTS      :',I10)
 3080 FORMAT(/,10X,'POLLUTANT ',I1,'               :',4X,A8,//,
     1         15X,'UNITS                :',4X,A8,/,
     2         15X,'PART. SIZE/VEL. USED?:',7X,A3)
 3110 FORMAT(///,10X,'MONTHLY EVAPORATION RATES, IN/DAY')
 3120 FORMAT(///,10X,'MONTHLY EVAPORATION RATES, MM/DAY')
 3130 FORMAT(/,10X,12(2X,A4,2X),//,10X,12(2X,F5.3,1X))
 3210 FORMAT(1H1,///,10X,'PARTICLE SIZES AND POLLUTANT FRACTIONS',
     1//)
 3220 FORMAT(1H1,///,10X,'SETTLING VELOCITIES AND POLLUTANT FRACTIONS'
     1          ,//)
 3230 FORMAT(30X,10(1X,E9.3))
 3240 FORMAT(10X,'RANGE IN MICRONS    ',10(6X,1A2,2X))
 3250 FORMAT(10X,'RANGE IN FEET/SEC   ',10(6X,A2,2X))
 3255 FORMAT(10X,'RANGE IN CM/SEC     ',10(6X,A2,2X))
 3260 FORMAT(/,10X,'SPECIFIC GRAVITY    ',10(2X,F8.3))
C#### WCH (RED), 9/23/93.  CHANGE 2A4 TO A8.
 3290 FORMAT(/,10X,'FRAC. OF ',A8,3X,10(5X,F5.3))
 3350 FORMAT(/////,10X,'MONTHLY WATER TEMPERATURES AND KINEMATIC VISCOSI
     1TIES',//,30X,12(2X,1A4,2X))
 3370 FORMAT(/,10X,'TEMPERATURE,DEG. F  ',12(2X,F4.1,2X),//,10X,
     1'VISCOSITY,SQ FT/SEC ',12(1X,E7.2))
 3390 FORMAT(/,10X,'TEMPERATURE,DEG. C  ',12(2X,F4.1,2X),//,10X,
     1'VISCOSITY,SQ CM/SEC ',12(1X,E7.2))
 6311 FORMAT (' WITHIN SUB. STRT, IDATE (YR/MO/DAY)=',I8,/,
     1 ' TIME OF DAY =',F7.3,' HRS.',/,' RUNNING TIME =',F9.3,' HRS.')
 6550 FORMAT(/,' ===> WARNING !! SUBROUTINE STRT : THE SETTLING VELOCITY
     1 ROUTINE HAS NOT CONVERGED FOR THE RANGE',/,36X,F8.1,' TO ',F8.1,
     2' MICRONS.  SIMULATION TERMINATED.')
 8050 FORMAT(/,' ===> Storage/Treatment simulation ended normally.')
 9200 FORMAT(/,' ===> WARNING !! END OF INPUT FILE REACHED AT TIME = ',
     +         F6.1,' HOURS.',/,
     +'                 SIMULATION CONTINUES WITH ZERO INFLOW.',/)
 9500 FORMAT(/,' Reading general data and control information.')
 9510 FORMAT(/,' Reading Storage/Treatment unit information.')
C=======================================================================
      RETURN
  888 CALL IERROR
      END

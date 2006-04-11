      SUBROUTINE STRT
C=======================================================================
C     STORAGE/TREATMENT MODEL  -- JUNE 1981. (UPDATED JANUARY 1988)
C
C     MODEL WRITTEN BY STEPHAN J. NIX
C     UPDATED AND MODIFIED BY ROBERT E. DICKINSON
C                      ENVIRONMENTAL ENGINEERING SCIENCES
C                      UNIVERSITY OF FLORIDA
C                      GAINESVILLE, FLORIDA 32611
C     CURRENT (12/94) PHONE : 813-886-7724 AT XP SOFTWARE
C     UPDATE HEADER, 5/93 BY WCH AT OREGON STATE UNIVERSITY
C     ROB JAMES AND WCH, 8/4/93.  FIX METRIC CONVERSION WHILE 
C       WRITING TO INTERFACE FILE.
C     WCH (RED), 9/23/93.  CHANGE A FORMAT FROM 2A4 TO A8.
C     WCH, 12/5/94.  CHANGE SEVERAL FORMATS TO MOVE OUTPUT TO LEFT
C       AND ADD PRINT-OUT OF PRINT AND DATE/TIME VARIABLES. 
C     WCH, 2/27/95.  CORRECT PRINT-OUTS OF POLLUTANT NAME, UNITS. 
C     WCH, 7/7/95.  CORRECT METRIC PRINT-OUT OF TRIBA.
C     WCH, 7/7/95.  MAKE CORRECTIONS FOR METRIC CONVERSION ON INTERFACE
C       FILE. 
C     CIM  4/99 MODIFIED TO INCREASE NUMBER OF CONSTITUENT FROM 10
C          TO MQUAL.  
C=======================================================================
C     NOTE, WHEN METRIC OPTION IS USED, THE S/T BLOCK USES METRIC UNITS
C       INTERNALLY.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'S1.INC'
C=======================================================================
CIM   CHANGE 3 to MQUAL
CIM	CHANGE 10 TO MQUAL
      DIMENSION ANS(2),IPOLL(MQUAL),PCAR(MQUAL),PCAR1(MQUAL),TEMP(12),
     1ALPHA(2)
      DIMENSION POLC(MQUAL),POLCL(MQUAL),POLL(MQUAL,NIE),
     1POLL1(MQUAL,NIE)
      DIMENSION SPG(MQUAL),VIS(12),QO1(NIE),QO(NIE)
C#### WCH, 2/27/95.  NEED THREE TEMPORARY ARRAYS FOR POLLUTANTS.
      DIMENSION PNAMET(MQUAL), PUNITT(MQUAL), NDIMT(MQUAL)
      CHARACTER PNAMET*8, PUNITT*8
C
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
C#### WCH, 7/7/95.  TRIBA ALWAYS IN ACRES.
      IF(METRIC.EQ.2) TRIBA = TRIBA*2.471
C
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
      JYEAR = IDATE/10000
      IF (JYEAR.LT.100) THEN
       IDATE = IDATE-JYEAR*10000
       JYEAR = JYEAR + 1900
       IDATE = IDATE+JYEAR*10000
       ENDIF
      IF(ISUM.GT.2) ISUM=2
      TZERO    = TIME*3600.0
      TIMDAY   = TZERO
      TIME     = TZERO
      TCAR1    = TIME/3600.0
      QCAR1    = 0.0
	DO IP = 1,NP
	PCAR1(IP) = 0.0
c      PCAR1(1) = 0.0
c      PCAR1(2) = 0.0
c      PCAR1(3) = 0.0
	ENDDO
      NYEAR   = IDATE/10000
      NDAY    = IDATE - NYEAR*10000
      MONTH   = NDAY/100
      NDAY    = NDAY - MONTH*100
      IF(NDAY.LE.0) NDAY   = 2
      IF(MONTH.LE.0) MONTH = 8
      IF(NYEAR.LT.0) NYEAR = 1941
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
C#### WCH, 12/5/94.  MOVE ECHO OF B1-C1 DATA TO HERE (FROM STMT 3000).
C     ALSO, ADD ECHO OF PRINT CONTROL INPUT DATA.
C#### WCH, 12/5/94.  ADD NOTAPE, INITIAL JULIAN DAY, DURATION TO 
C       PRINT-OUT.
C=======================================================================
      WRITE(N6,3010) NOTAPE,SOUR(NOTAPE+1)
      WRITE(N6,3020)  JNS,NDT,DS,NU,ANS(ICOST+1),MONTH,NDAY,NYEAR,
     1                JULDAY,JHR,MINUTE,JSEC
C=======================================================================
C#### WCH, 12/5/94.  Include print of ending Julian date.
C     Calculate ending Julian day.
C=======================================================================
      DUR    = FLOAT(NDT)*DS
      CALL NDATE(DUR,JEND,TMEND)
      DURTON = FLOAT(NDT)*DS/3600.
      WRITE (N6,3019) DURTON,JEND,TMEND/3600.
C#### WCH, 12/5/94.  ECHO INPUT PRINT PARAMETERS.  
      IF(ISUM.EQ.1) WRITE (N6,3021)
      IF(ISUM.EQ.2) WRITE (N6,3022)
      IF(ISUM.EQ.0) WRITE (N6,3023)
      IF(IDET.EQ.0) WRITE (N6,3024)
      DURTON  = FLOAT(IDET)*DS/60.
      IF(IDET.GT.0) WRITE (N6,3025) IDET,DURTON
C
C=======================================================================
C#### WCH, 12/5/94.  CHECK FOR IDET > 0 AND NPR = 0 AND ALLOW FOR
C     INPUTTING 0,0 FOR ISTART AND IEND AS IN RUNOFF BLOCK M2 LINES.
C=======================================================================
      IF(IDET.GT.0.AND.NPR.EQ.0) THEN
           WRITE (N6,3026)
           NPR = 1
           ENDIF           
C=======================================================================
C>>>>>>>> READ DATA GROUP C2 <<<<<<<<
C=======================================================================
      IF(IDET.GT.0) READ(N5,*,ERR=888) CC,(ISTART(L),IEND(L),L=1,NPR)
      DO L=1,NPR
      JYEAR = ISTART(L)/10000
      IF ((ISTART(L).GT.0).AND.(JYEAR.LT.100)) THEN
        ISTART(L) = ISTART(L)-JYEAR*10000
        JYEAR = JYEAR + 1900
        ISTART(L) = ISTART(L)+JYEAR*10000
        ENDIF 
      JYEAR = IEND(L)/10000
      IF ((IEND(L).GT.0).AND.(JYEAR.LT.100)) THEN
        IEND(L) = IEND(L)-JYEAR*10000
        JYEAR = JYEAR + 1900
        IEND(L) = IEND(L)+JYEAR*10000
        ENDIF 
      ENDDO
C=======================================================================
      DO 1000 L = 1,NPR
C=======================================================================
C#### WCH, 12/5/94.  ALLOW 0,0 ENTRY ON SINGLE C2 LINE FOR PRINTING
C     FOR ENTIRE SIMULATION, AS IN RUNOFF BLOCK M2 LINES.
C=======================================================================
      IF(NPR.EQ.1.AND.ISTART(1).EQ.0.AND.IEND(1).EQ.0) THEN
           WRITE (N6,3027)
           ISTART(1) = JULDAY
           IEND(1)   = JEND
           GO TO 1000
           ELSE
           IF(L.EQ.1) WRITE (N6,3028)
           WRITE (N6,3029) L,ISTART(L),IEND(L)
           ENDIF
C
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
C>>>>>>>>  READ DATA GROUPS E2 AND E3  <<<<<<<<
C=======================================================================
      IF(IPT.LE.0) GO TO 2000
C=======================================================================
C>>>>>>>>  READ DATA GROUP E2 <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,NVS,NNR
C=======================================================================
C>>>>>>>>  READ DATA GROUP E3 <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,(RAN(KJ,1),RAN(KJ,2),KJ=1,NNR)
      IF(NVS.GT.0) THEN
                   DO 1450 KJ = 1,NNR
 1450              VS(KJ) = (RAN(KJ,1)+RAN(KJ,2))/2.0
                   ELSE
C=======================================================================
C>>>>>>>>  READ DATA GROUPS E4 AND E5  <<<<<<<<
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
C#### WCH, 2/27/95.  NEED TO SET UP CORRECT NAME, UNITS, NDIM FROM
C     POSITION OF POLLUTANT ON FILE ==> IPOLL().
C     EASIEST WAY IS TO SAVE TEMPORARILY INCOMING NAME, UNITS, NDIM.  
                  DO 2260 IP = 1,NQUAL
                  PNAMET(IP) = PNAME(IP)
                  PUNITT(IP) = PUNIT(IP)
 2260             NDIMT(IP)  = NDIM(IP)
C
                  DO 2270 IP = 1,NP
c                  IF(IP.GT.NP) GO TO 2270
C#### WCH, 2/27/95.  GIVE CORRECT NAME, UNIT, DIMENSIONS TO POLLUTANTS.
                  IPX = IPOLL(IP)
                  PNAME(IP) = PNAMET(IPX)
                  PUNIT(IP) = PUNITT(IPX)
                  NDIM(IP)  = NDIMT(IPX)
C
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
C#### WCH, 12/5/94.  MOVE LINES B1-C2 I/O ECHO PRINTS TO EARLIER SPOT. 
C# 3000 WRITE(N6,3010) SOUR(NOTAPE+1)
C#      WRITE(N6,3020)  JNS,NDT,DS,NU,ANS(ICOST+1),MONTH,NDAY,NYEAR,JHR,
C#     +                MINUTE,JSEC
 3000 IF(METRIC.EQ.1) WRITE(N6,3030) TRIBA
C#### WCH, 7/7/95.  CORRECT METRIC PRINT-OUT OF TRIBA.  CONVERT TO HA.
      IF(METRIC.EQ.2) WRITE(N6,3040) TRIBA/2.471
      WRITE(N6,3060) NP
      IF(NP.GT.0) THEN
                  DO 3070 IP = 1,NP
C#### WCH, 12/5/94.  ADD PRINT OF IPART().
 3070             WRITE(N6,3080)IP,PNAME(IP),PUNIT(IP),IPART(IP),
     1                    ANS(IPART(IP)+1)
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
C#### WCH, 7/7/95.  QO(JS) is used twice.  Define new variable to keep
C       separate.
      QOJS    = 0.0
C
      QO(JS)  = 0.0
      QO1(JS) = 0.0
      IF(NP.GT.0) THEN
                  DO 6010 IP = 1,NP
 6010             POLCL(IP)  = 0.0
                  ENDIF
C=======================================================================
C     MAIN TIME LOOP
C=======================================================================
      WRITE(*,23) NDT,JEND
      DO 9000 KDT = 1,NDT
C####      WRITE(*,22)   KDT
      TIME        = TIME + DS
      TIMEHR      = TIME/3600.0
      CALL STIME(DS)
      CALL DATED
C#### WCH, 12/5/94.  MOVE WRITE TO HERE AND ADD JULIAN DAY.
C***WCH, 9/27/99. WRITE TO UNIT 6 TO GET CARRIAGE CONTROL.
C      WRITE(*,22)   KDT,JULDAY
      WRITE(6,22)   KDT,JULDAY
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
						DO IP = 1,NP
	                    PCAR(IP) = PCAR1(IP)
	                    ENDDO
c                               PCAR(1) = PCAR1(1)
c                               PCAR(2) = PCAR1(2)
c                               PCAR(3) = PCAR1(3)
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
C#### WCH, 7/7/95.  HERE, USE NEW VARIABLE QOJS FOR OLD INFLOW. 
C####          QO(JS)    = QO1(JS)
           QOJS = QO1(JS)
C
           IF(NP.GT.0) THEN
                DO 4520 J  = 1,NP
 4520           POLL(J,JS) = POLL1(J,JS)
                ENDIF
           IF(NQUAL.EQ.0) READ(LAST,END=999) JDAY,TMDAY,DELTA,
     +                                  (QO1(J),J=1,LOCATS)
           IF(NQUAL.GT.0) READ(LAST,END=999) JDAY,TMDAY,DELTA,
     +                    (QO1(J),(POLL1(IP,J),IP=1,NQUAL),J=1,LOCATS)
           GO TO 2995
  999      QO(JS)      = 0.0
C#### WCH, 7/7/95
           QOJS        = 0.0
C
           QO1(JS)     = 0.0
           DO 2990  J  = 1,NQUAL
           POLL1(J,JS) = 0.0
           POLL(J,JS)  = 0.0
 2990      CONTINUE
           WRITE(N6,9200) TIME/3600.0
           JDAY       = 9999999
           TMDAY      = 0.0
 2995      CONTINUE
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
C     QCONV = 35.314667 cfs/cms = 1 / 0.28317E-2 when metric flows are on 
C     interface file.  Thus, interface flows and loads are converted 
C     properly whether in cfs or cms.  
C=======================================================================
                IF(METRIC.EQ.2) POLL1(IPX,JS) =
     +                                POLL1(IPX,JS)*2.8317E-2
 6410           POLL1(IPX,JS) = POLL1(IPX,JS)*QCONV
                ENDIF
C=======================================================================
C     If METRIC = 2 (metric units), QO1 has units of cms/35...  in line
C     below.  But flow is multiplied by QCONV = 35...  later below 
C     and converted back to cms. 
C=======================================================================
           IF(METRIC.EQ.2) QO1(JS) = QO1(JS)*2.8317E-2
           ENDIF
C=======================================================================
      THR = TIMEHR - TREF
C#### WCH, 7/7/95.  HERE, USE NEW VARIABLE QOJS.
C####      QQ1 = QO(JS)
      QQ1 = QOJS
      QQ2 = QO1(JS)
      IF(TFILE.EQ.0.0) QOO = QQ2*QCONV
      IF(TFILE.GT.0.0) QOO = QLINTP(QQ1,QQ2,0.0,TFILE,THR)*QCONV
C=======================================================================
C     AT THIS POINT, QOO SHOULD HAVE UNITS OF CFS OR CMS, APPROPRIATELY.
C=======================================================================
      IF(NP.GT.0) THEN
                  DO 4530 IP = 1,NP
                  IPX        = IPOLL(IP)
                  QQ1        = POLL(IPX,JS)
                  QQ2        = POLL1(IPX,JS)
C#######################################################################
C#### WCH, 7/7/95.  ONE CONVERSION TOO MANY??  POLL1 AND POLL SHOULD
C     ALREADY BE IN UNITS OF FLOW*CONC WHERE FLOW IS CFS OR CMS OR
C     WHATEVER UNITS ARE ON INTERFACE FILE.  DON'T MULTIPLY AGAIN
C     BY QCONV (= 35... FOR METRIC=2).  
C=======================================================================
C####                  IF(TFILE.EQ.0.0) POLC(IP) = POLC(IP) + QQ2*QCONV
C####                  IF(TFILE.GT.0.0) POLC(IP) = POLC(IP) +
C####     +                         QLINTP(QQ1,QQ2,0.0,TFILE,THR)*QCONV
                  IF(TFILE.EQ.0.0) POLC(IP) = POLC(IP) + QQ2
                  IF(TFILE.GT.0.0) POLC(IP) = POLC(IP) +
     +                         QLINTP(QQ1,QQ2,0.0,TFILE,THR)
C
 4530             CONTINUE
                  ENDIF
      ENDIF
C=======================================================================
C     Calculate average flow over the time interval.
C     QQTP = will have units of cfs or cms appropriately.
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
C######################################################################
C#### WCH, 7/7/95.  DON'T CONVERT AGAIN HERE.  POLL(JS) = POLL1(JS)
C     JUST BEFORE READING INTERFACE FILE.  SHOULD BE OK.
C     TO GET AVG. CONC., SHOULD BE DIVIDING CFS*CONC / CFS OR
C     CMS*CONC / CMS.
C=======================================================================
C####           IF(METRIC.EQ.2) POLL(IPX,JS) = POLL(IPX,JS)*2.8317E-2
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
C=======================================================================
C     HERE, ASSIGN OUTFLOW FROM PLANT TO QO(JS) AND SET FLOWS AT OTHER
C     NODES ON INTERFACE FILE TO THEIR INFLOW VALUES.
C=======================================================================
           QO(JS)    = QQRC
           DO 6720 J = 1,LOCATS
           IF(J.EQ.JS) GO TO 6720
           QO(J)     = QO1(J)
 6720      CONTINUE
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
C#### WCH, 12/5/94.  Alter 22 and 23 to add Julian dates.
   22 FORMAT('+',I19,I22)
   23 FORMAT(/,' Beginning loop through',I7,' time steps.',/,
     1         ' End at Julian date:',I8,/,
     2         ' Current time step #   Current Julian date ',/)
  150 FORMAT(/,' ###########################################',/,
     1         ' # Entry made to Storage/Treatment Block.  #',/,
     2         ' # Storage/Treatment model written by the  #',/,
     3         ' # University of Florida, June 1981.       #',/,
     4         ' # Last updated July 1995 at Oregon St. U. #',/,
     5         ' ###########################################',/)
 1490 FORMAT(/,' ===> ERROR !! IN DATA GROUP E6 : THE PARTICLE SIZE OR V
     1ELOCITY DISTRIBUTION FOR POLLUTANT ',I1,' DOES NOT SUM TO 1.0.',/,
     232X,'SIMULATION TERMINATED.')
 2570 FORMAT(/,' ===> ERROR !! IN DATA GROUP B1: THE SELECTED EXTERNAL',
     1' ELEMENT NUMBER, ',I4,' IS NOT AVAILABLE FROM',/,32X,'DATA-SET. '
     2,I3,'.  SIMULATION TERMINATED.')
C#### WCH, 12/5/94.  ADD PRINT OF NOTAPE.
 3010 FORMAT(/,1X,'VALUE OF NOTAPE           :',I10,//,
     1         1X,'INPUT DATA SOURCE         :',2X,A40)
C#### WCH, 12/5/94.  NEW 3019 FORMAT.
 3019 FORMAT(/,1X,'SIMULATION DURATION, HOURS:',F10.3,//,
     1         1X,'ENDING JULIAN DATE        :',I10,//,
     1         1X,'ENDING TIME OF DAY        :',F10.3,' HOURS')
C#### WCH, 12/5/94.  CHANGE A BUNCH OF LEADING 10X SPACES TO 1X. THIS
C     IS EQUIVALENT TO SUBTRACTING 9 LEADING SPACES IN MANY PLACES. 
C#### WCH, 12/5/94.  ADD STARTING JULIAN DATE. 
 3020 FORMAT(/,1X,'EXTERNAL ELEMENT NUMBER   :',I10,//,
     1         1X,'NUMBER OF TIME STEPS      :',I10,//,
     2         1X,'TIME STEP SIZE,SECONDS    :',F10.1,//,
     3         1X,'NUMBER OF S/T UNITS       :',I10,//,
     4         1X,'COST MODEL USED?          :',7X,A3,//,
     5         1X,'STARTING DATE             :',4X,I2,'/',I2,'/',I4,//,
     6         1X,'STARTING JULIAN DATE      :',I10,//,
     7         1X,'STARTING TIME             :',2X,I3,':',I3,':',I3)
C#### WCH, 12/5/94.  NEW 3021 - 3029 FORMATS.
 3021 FORMAT(/,1X,'PRINT ANNUAL AND TOTAL SUMMARIES (ISUM = 1)')
 3022 FORMAT(/,1X,'PRINT MONTHLY, ANNUAL AND TOTAL SUMMARIES (ISUM = 2)
     1 ')
 3023 FORMAT(/,1X,'PRINT TOTAL SIMULATION SUMMARY ONLY (ISUM = 0)')
 3024 FORMAT(/,1X,'IDET = 0. NO DETAILED (TIME STEP) PRINT-OUTS.')
 3025 FORMAT(/,1X,'DETAILED PRINT-OUT EVERY',I4,' (IDET) TIME STEPS = EV
     1ERY',F8.2,'MIN.')
 3026 FORMAT(/,1X,'WARNING. IDET > 0 AND NPR = 0.  PROGRAM WILL TRY TO R
     1EAD AT LEAST ONE C2 LINE.')
 3027 FORMAT(/,1X,'ZERO VALUES FOR ISTART, IEND (LINE C2).  PRINT FOR EN
     1TIRE SIMULATION.')
 3028 FORMAT(/,' TIME INTERVALS FOR DETAILED PRINT-OUTS (LINE C2):',/,
     1 ' NUMBER     ISTART      IEND',/,
     2 '          YR-MO-DAY  YR-MO-DAY')
 3029 FORMAT(I5,I13,I11)
C
 3030 FORMAT(//,1X,'INPUT/OUTPUT UNITS        :   U.S. CUSTOMARY',//,
     1          1X,'TRIBUTARY AREA, ACRES     :',F10.1)
C#### WCH, 12/5/94.  CHANGE METRIC AREA TO HECTARES.
 3040 FORMAT(//,1X,'INPUT/OUTPUT UNITS        :      METRIC',//,
     1          1X,'TRIBUTARY AREA, HECTARES  :',F10.1)
 3060 FORMAT(/,1X,'NUMBER OF POLLUTANTS      :',I10)
 3080 FORMAT(/,1X,'POLLUTANT ',I1,'               :',4X,A8,//,
     1         6X,'UNITS                :',4X,A8,/,
     2         6X,'IPART                :',I10,/,
     3         6X,'PART. SIZE/VEL. USED?:',7X,A3)
 3110 FORMAT(//,10X,'MONTHLY EVAPORATION RATES, IN/DAY')
 3120 FORMAT(//,10X,'MONTHLY EVAPORATION RATES, MM/DAY')
 3130 FORMAT(/,1X,12(2X,A4,2X),//,1X,12(2X,F5.3,1X))
 3210 FORMAT(//,1X,'PARTICLE SIZES AND POLLUTANT FRACTIONS',//)
 3220 FORMAT(//,1X,'SETTLING VELOCITIES AND POLLUTANT FRACTIONS',//)
 3230 FORMAT(1X,10(1X,E9.3))
 3240 FORMAT(1X,'RANGE IN MICRONS    ',10(6X,1A2,2X))
 3250 FORMAT(1X,'RANGE IN FEET/SEC   ',10(6X,A2,2X))
 3255 FORMAT(1X,'RANGE IN CM/SEC     ',10(6X,A2,2X))
 3260 FORMAT(/,1X,'SPECIFIC GRAVITY    ',10(2X,F8.3))
C#### WCH (RED), 9/23/93.  CHANGE 2A4 TO A8.
 3290 FORMAT(/,1X,'FRAC. OF ',A8,3X,10(5X,F5.3))
 3350 FORMAT(//,1X,'MONTHLY WATER TEMPERATURES AND KINEMATIC VISCOSITIES
     1',//,21X,12(2X,1A4,2X))
 3370 FORMAT(/,1X,'TEMPERATURE, DEG. F ',12(2X,F4.1,2X),//,1X,
     1            'VISCOSITY, SQ FT/SEC',12(1X,E7.2))
 3390 FORMAT(/,1X,'TEMPERATURE, DEG. C ',12(2X,F4.1,2X),//,1X,
     1            'VISCOSITY, SQ CM/SEC',12(1X,E7.2))
C#### WCH, 12/5/94. ELIMINATE UNUSED 6311 FORMAT.
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
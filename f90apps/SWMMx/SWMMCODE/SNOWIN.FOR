      SUBROUTINE SNOWIN(KLING)
C=======================================================================
C     SUBROUTINE SNOWIN USED TO BE PART OF SUBROUTINE RHYDRO
C                CHANGE MADE FOR SWMM 4.0
C     UPDATED 11/92 BY WCH FOR CORRECTION TO PRINT FOR METRIC SNOTMP
C     CORRECTION 5/93 BY RED FOR ERROR PRINT-OUT. 
C     MINOR UPDATING OF OUTPUT, WCH 8/5/93.
C     ALLOW FOR ALPHANUMERIC SUBCATCHMENT NAMES FOR SNOW INPUT, WCH 8/93
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      DIMENSION SN(11),SND(11),SNR(11)
C#### WCH, 8/5/93. ALLOW FOR ALPHANUMERIC SNOWMELT SUBCATCHMENT NAMES.
      CHARACTER KK1*10,KK2*10
C=======================================================================
C>>>>>>>>>> READ DATA GROUP C1 <<<<<<<<<<
C=======================================================================
      IF(KLING.EQ.0) THEN
      IF(ISNOW.EQ.1) READ(N5,*,ERR=888) CC,ELEV,FWFRAC(1),FWFRAC(2)
      IF(ISNOW.EQ.2) READ(N5,*,ERR=888) CC,ELEV,FWFRAC(1),FWFRAC(2),
     1               FWFRAC(3),SNOTMP,SCF,TIPM,RNM,ANGLAT,DTLONG
      IF(SCF.LE.0.0) SCF = 1.0
C=======================================================================
C>>>>>>>> READ DATA GROUP C2 <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,NUMB,(N,WIND(N),J=1,NUMB)
C=======================================================================
C>>>>>>>>>> READ DATA GROUP C3 <<<<<<<<<<<<<
C=======================================================================
      IF(ISNOW.EQ.2) READ(N5,*,ERR=888) CC,ADCI
C=======================================================================
C>>>>>>>>>> READ DATA GROUP C4 <<<<<<<<<<<<
C=======================================================================
      IF(ISNOW.EQ.2) READ(N5,*,ERR=888) CC,ADCP
C=======================================================================
C>>>>>>>> READ DATA GROUP C5 <<<<<<<<
C=======================================================================
      IF(ISNOW.EQ.1) READ(N5,*,ERR=888) CC,DTAIR,NAIRT,
     1                                  (TAIR(IZ),IZ=1,NAIRT)
      IF(IPRN(5).EQ.0) WRITE(N6,200)
      IF(ISNOW.EQ.2) GO TO 240
C=======================================================================
C******** HERE, PRINT VARIABLES FOR SINGLE-EVENT SNOW MELT
C=======================================================================
      IF(IPRN(5).EQ.0) THEN
      IF(METRIC.EQ.1) WRITE(N6,210) ELEV
      IF(METRIC.EQ.2) WRITE(N6,215) ELEV
      WRITE(N6,220)   FWFRAC(2),FWFRAC(1)
      IF(METRIC.EQ.1) WRITE(N6,230) DTAIR,(TAIR(I),I=1,NAIRT)
      IF(METRIC.EQ.2) WRITE(N6,235) DTAIR,(TAIR(I),I=1,NAIRT)
      ENDIF
C=======================================================================
C******** CONVERT TO DEG. F.
C=======================================================================
      IF(METRIC.EQ.2) THEN
                      DO 237 I = 1,NAIRT
  237                 TAIR(I) =  1.8*TAIR(I) + 32.0
                      ENDIF
      GO TO 310
C=======================================================================
C******** HERE, PRINT VARIABLES FOR CONTINUOUS SNOW MELT
C=======================================================================
C#### WCH, 8/5/93.  ADD EXPLANATORY PRINT REGARDING NSCRAT(3).
  240 WRITE (N6,249) NSCRAT(3)
      REWIND NSCRAT(3)
      READ(NSCRAT(3)) LOCAT3
      IF(RNM.LE.0.0) RNM = 0.6
      IF(IPRN(5).EQ.0) THEN
           IF(METRIC.EQ.1) WRITE(N6,250) ELEV,SNOTMP,SCF
           IF(METRIC.EQ.2) WRITE(N6,255) ELEV,SNOTMP,SCF
           IF(METRIC.EQ.2) SNOTMP = 1.8*SNOTMP+32.0
           WRITE (N6,220)  FWFRAC(2),FWFRAC(1)
           WRITE (N6,260)  FWFRAC(3)
           WRITE (N6,270) TIPM,RNM,ANGLAT,DTLONG,LOCAT3
           ENDIF
      DTLONG = DTLONG / 60.0
      IF(ANGLAT.NE.0) ANGLAT = TAN(ANGLAT * 3.14159 / 180.0)
      IF(IPRN(5).EQ.0) WRITE(N6,280)
      DO 290 I = 1,10
      YAK      = 0.1 * FLOAT(I-1)
  290 IF(IPRN(5).EQ.0) WRITE(N6,300) YAK,ADCI(I),ADCP(I)
  300 FORMAT(T5,F5.2,T23,F5.2,T37,F5.2)
      YAK = 1.0
      IF(IPRN(5).EQ.0)                WRITE(N6,300) YAK,YAK,YAK
  310 IF(ISNOW.EQ.1.AND.IPRN(5).EQ.0) WRITE(N6,320)
  320 FORMAT(1H1)
      IF(METRIC.EQ.1.AND.IPRN(5).EQ.0) WRITE(N6,330) WIND
      IF(METRIC.EQ.2.AND.IPRN(5).EQ.0) WRITE(N6,335) WIND
      DO 337 I=1,12
  337 WIND(I) = WIND(I)*CMET(11,METRIC)
C=======================================================================
C******** COMPUTE AVERAGE ATM. PRESSURE IN IN. HG.
C=======================================================================
      ELEV = ELEV*CMET(1,METRIC)/1000.0
      PA = 29.9 - 1.02 * ELEV + 0.0032 * ELEV ** 2.4
C=======================================================================
C******** CALCULATE PSYCHOMETRIC CONSTANT * 7.5
C=======================================================================
      SGAMMA = 7.5 * PA * 0.000359
      RETURN
      ENDIF
C=======================================================================
C     END OF ENTRY OF PRELIMINARY SNOWMELT DATA, DATA GROUPS C1 - C5.
C
C     BEGINNING OF ENTRY OF SUBCATCHMENT SNOWMELT DATA, 
C       DATA GROUPS I1 - I2.
C=======================================================================
      IF(KLING.EQ.1) THEN
      KSNO = 0
      IF(IPRN(5).EQ.0) THEN
      WRITE (N6,1210)
      IF(METRIC.EQ.1.AND.ISNOW.EQ.1) WRITE(N6,1220)
      IF(METRIC.EQ.2.AND.ISNOW.EQ.1) WRITE(N6,1225)
      IF(METRIC.EQ.1.AND.ISNOW.EQ.2) WRITE(N6,1230)
      IF(METRIC.EQ.2.AND.ISNOW.EQ.2) WRITE(N6,1235)
      ENDIF
C=======================================================================
C     SET DEFAULT VALUES AND RATIOS.
C=======================================================================
      JLST                  = 4
      IF(ISNOW.EQ.2) JLST   = 11
      MAXCAT                = NW
C=======================================================================
C******** USER MUST SUPPLY ANY DEFAULT VALUES.
C=======================================================================
      DO 1250 J = 1,JLST
      SND(J)    = 0.0
 1250 SNR(J)    = 1.0
C=======================================================================
C     ENTER LOOP TO READ DATA.
C=======================================================================
      N         = 0
      DO 1400 I = 1,900
      N         = N+1
C=======================================================================
C>>>>>>>> READ DATA GROUP I1 <<<<<<<
C=======================================================================
C WCH, 8/5/93.  FOR READ STATEMENTS BELOW, ALLOW FOR ALPHANUMERIC NAME.
C#######################################################################
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'I1') GO TO 1410
      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,JK1,SNN1,SNN2,SNN3,SNN4,SNN5,
     *             SNN6,(SN(J),J=1,4)
      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,KK1,SNN1,SNN2,SNN3,SNN4,SNN5,
     *             SNN6,(SN(J),J=1,4)
C=======================================================================
C>>>>>>>> READ DATA GROUP I2 <<<<<<<<
C=======================================================================
      IF(ISNOW.EQ.2.AND.JCE.EQ.0) READ(N5,*,ERR=888) CC,JK2,SNN7,SNN8,
     1               (SN(J),J=5,11),SNN9,SNN10,SNN11,SNN12,SNN13,SNN14
      IF(ISNOW.EQ.2.AND.JCE.EQ.1) READ(N5,*,ERR=888) CC,KK2,SNN7,SNN8,
     1               (SN(J),J=5,11),SNN9,SNN10,SNN11,SNN12,SNN13,SNN14
       IF(JCE.EQ.1) THEN      
           IF(KK1.EQ.'-1') JK1 = -1
           IF(KK2.EQ.'-1') JK2 = -1
           IF(KK1.EQ.'-2') JK1 = -2
           IF(KK2.EQ.'-2') JK2 = -2
           ENDIF
      IF(JK1.NE.-1) GO TO 1280
C=======================================================================
C     ALTER INPUT RATIOS.
C=======================================================================
      DO 1270 J=1,JLST
      IF(SN(J).NE.0.0) SNR(J)=SN(J)
 1270 CONTINUE
      N=N-1
      GO TO 1400
 1280 IF(JK1.NE.-2) GO TO 1300
C=======================================================================
C     ALTER INPUT DEFAULT VALUES.
C=======================================================================
      DO 1290 J = 1,JLST
      IF(SN(J).NE.0.0) SND(J) = SN(J)
 1290 CONTINUE
      N=N-1
      GO TO 1400
C=======================================================================
C     PERFORM MISCELLANEOUS ERROR CHECKS.
C=======================================================================
C#### WCH, 8/5/93. SEVERAL CHANGES BELOW TO ACCOMODATE ALPHA NAMES.
C     SHOULD BE ABLE TO FIND MOST CHANGES BY SEARCHING FOR JCE.
C#######################################################################
 1300 IF(JCE.EQ.0.AND.JK1.EQ.NAMEW(N)) GO TO 1320
      IF(JCE.EQ.1.AND.KK1.EQ.KAMEW(N)) GO TO 1320
      IF(IPRN(5).EQ.0) THEN
          IF(JCE.EQ.0) WRITE(N6,1310) N,JK1,NAMEW(N)
          IF(JCE.EQ.1) WRITE(N6,1311) N,KK1,KAMEW(N)
          KSNO = 1
          IF(JCE.EQ.0) WRITE(N6,1315) JK1,SNN1,SNN2,SNN3,SNN4,SNN5,
     +                 SNN6,(SN(J),J=1,4)
          IF(JCE.EQ.1) WRITE(N6,1316) KK1,SNN1,SNN2,SNN3,SNN4,SNN5,
     +                 SNN6,(SN(J),J=1,4)
          ENDIF
      IF(ISNOW.EQ.1) GO TO 1400
 1320 IF(JCE.EQ.0.AND.(ISNOW.EQ.1.OR.JK1.EQ.JK2)) GO TO 1340
      IF(JCE.EQ.1.AND.(ISNOW.EQ.1.OR.KK1.EQ.KK2)) GO TO 1340
      KSNO = 2
      IF(IPRN(5).EQ.0) THEN
         IF(JCE.EQ.0) THEN
            WRITE(N6,1330) JK1,JK2,N
            WRITE (N6,1315) JK1,SNN1,SNN2,SNN3,SNN4,SNN5,SNN6,
     +                      (SN(J),J=1,4)
            WRITE (N6,1315) JK2,SNN7,SNN8,(SN(J),J=5,11),SNN9, 
     +                     SNN10,SNN11,SNN12,SNN13,SNN14
            ELSE
            WRITE (N6,1331) KK1,KK2,N
            WRITE (N6,1316) KK1,SNN1,SNN2,SNN3,SNN4,SNN5,SNN6,
     +                      (SN(J),J=1,4)
            WRITE (N6,1316) KK2,SNN7,SNN8,(SN(J),J=5,11),SNN9, 
     +                     SNN10,SNN11,SNN12,SNN13,SNN14
            ENDIF
      ENDIF
      GO TO 1400
 1340 IF(N.LE.MAXCAT) GO TO 1360
      KSNO = 3
      IF(IPRN(5).EQ.0) THEN
         IF(JCE.EQ.0) THEN
            WRITE(N6,1350) N,JK1,MAXCAT
            WRITE (N6,1315) JK1,SNN1,SNN2,SNN3,SNN4,SNN5,SNN6,
     +                     (SN(J),J=1,4)
            ELSE
            WRITE(N6,1351) N,KK1,MAXCAT
            WRITE (N6,1316) KK1,SNN1,SNN2,SNN3,SNN4,SNN5,SNN6,
     +                     (SN(J),J=1,4)
            ENDIF
      ENDIF
      GO TO 1410
C=======================================================================
C     NO INPUT ERRORS DETECTED IF REACH 1360.
C     ASSIGN DEFAULT VALUES AND MULTIPLY BY RATIOS.
C=======================================================================
 1360 DO 1370 J = 1,JLST
      IF(SN(J).EQ.0.0) SN(J) = SND(J)
 1370                  SN(J) = SN(J)*SNR(J)
C=======================================================================
C     TRANSFER VALUES AND PRINT INPUTS.
C=======================================================================
      XSNN1 = 1.0-SNN1
      IF(IPRN(5).EQ.0) THEN
      IF(JCE.EQ.0) THEN
         IF(ISNOW.EQ.1) WRITE (N6,1380) N,JK1,SNN2,SNN1,SNN4,SNN3,SNN6,
     1                               SNN5,SN(2),SN(1),SN(4),SN(3)
         IF(ISNOW.EQ.2) WRITE(N6,1390) N,JK1,SNN1,XSNN1,SNN10,SNN11,
     1                       SNN12,SNN13,SNN14,SNN9
         ELSE
         IF(ISNOW.EQ.1) WRITE (N6,1381) N,KK1,SNN2,SNN1,SNN4,SNN3,SNN6,
     1                               SNN5,SN(2),SN(1),SN(4),SN(3)
         IF(ISNOW.EQ.2) WRITE(N6,1391) N,KK1,SNN1,XSNN1,SNN10,SNN11,
     1                       SNN12,SNN13,SNN14,SNN9
         ENDIF
      ENDIF
      SNCP(N)  = SNN2
      PCI      = (WAR(1,N)+WAR(3,N))/WAREA(N)
      WAR(4,N) = WAREA(N)*PCI*SNN1
      WAR(1,N) = WAREA(N)*PCI*XSNN1*(100.0-PCTZER)/100.0
      WAR(3,N) = WAREA(N)*PCI*XSNN1*PCTZER/100.0
C=======================================================================
C     CONVERT SOME UNITS TO FEET AND SECONDS
C=======================================================================
      WSNOW(1,N) = SNN3
      WSNOW(2,N) = SNN4
      FW(1,N)    = SNN5
      FW(2,N)    = SNN6
      IF(ISNOW.EQ.1) THEN
                     DHMAX(1,N) = SN(1)/CMET(6,METRIC)
                     DHMAX(2,N) = SN(2)/CMET(6,METRIC)
                     ENDIF
      TBASE(1,N) = SN(3)
      TBASE(2,N) = SN(4)
      IF(ISNOW.EQ.1) GO TO 1400
      WSNOW(3,N) = SNN7
      FW(3,N)    = SNN8
      DHMAX(1,N) = SN(1)
      DHMAX(2,N) = SN(2)
      DHMAX(3,N) = SN(5)
      TBASE(3,N) = SN(6)
      DHMIN(1,N) = SN(7)
      DHMIN(2,N) = SN(8)
      DHMIN(3,N) = SN(9)
      SI(1,N)    = SN(10)
      SI(2,N)    = SN(11)
      WEPLOW(N)  = SNN9/CMET(3,METRIC)
      WWW        = WAR(1,N)+WAR(3,N)
      CHKFRA     = SNN10+SNN11+SNN12+SNN13+SNN14
      IF(ABS(CHKFRA-1.0).GT.0.001) WRITE (N6,1395) N,CHKFRA
      IF(WAR(4,N).GT.0.0)   SFRAC(1,N) = SNN10*WWW/WAR(4,N)
      IF(WAR(2,N).GT.0.0)   SFRAC(2,N) = SNN11*WWW/WAR(2,N)
      IF(WAR(2,NOW).GT.0.0) SFRAC(3,N) = SNN12*WWW/WAR(2,NOW)
      SFRAC(4,N) = SNN13*WWW
      SFRAC(5,N) = SNN14
 1400 CONTINUE
 1410 N = N-1
      IF(N.NE.NOW) THEN
                   WRITE(N6,1420) N,NOW
                   KSNO = 4
                   ENDIF
C##### RED, 5/12/93  CHANGE ERROR FROM 14 TO 15. 
      IF(KSNO.GE.1) CALL ERROR(15)
      IF(ISNOW.EQ.1) GO TO 1500
C=======================================================================
C     PRINT ADD'L DATA FOR CONTINUOUS SIMULATION.
C=======================================================================
      IF(IPRN(5).EQ.0) THEN
      IF(METRIC.EQ.1) WRITE(N6,1440)
      IF(METRIC.EQ.2) WRITE(N6,1445)
C
      DO 1450 N = 1,NOW
      IF(JCE.EQ.1) WRITE(N6,1461) N,KAMEW(N),WSNOW(2,N),WSNOW(1,N),
     1  WSNOW(3,N),FW(2,N),FW(1,N),FW(3,N),SI(2,N),SI(1,N)
 1450 IF(JCE.EQ.0) WRITE(N6,1460) N,NAMEW(N),WSNOW(2,N),WSNOW(1,N),
     1  WSNOW(3,N),FW(2,N),FW(1,N),FW(3,N),SI(2,N),SI(1,N)
      IF(METRIC.EQ.1) WRITE(N6,1470)
      IF(METRIC.EQ.2) WRITE(N6,1475)
      DO 1480 N=1,NOW
      IF(JCE.EQ.1) WRITE(N6,1491) N,KAMEW(N),DHMAX(2,N),DHMAX(1,N),
     1     DHMAX(3,N),DHMIN(2,N),DHMIN(1,N),DHMIN(3,N),TBASE(2,N),
     2     TBASE(1,N),TBASE(3,N)
 1480 IF(JCE.EQ.0) WRITE(N6,1490) N,NAMEW(N),DHMAX(2,N),DHMAX(1,N),
     1     DHMAX(3,N),DHMIN(2,N),DHMIN(1,N),DHMIN(3,N),TBASE(2,N),
     2     TBASE(1,N),TBASE(3,N)
      ENDIF
C=======================================================================
C     CONVERT ALL UNITS TO FEET AND SECONDS.
C=======================================================================
 1500 DO 1520 N = 1,NOW
      IF(ISNOW.EQ.2) SI(1,N) = SI(1,N)/CMET(3,METRIC)
      IF(ISNOW.EQ.2) SI(2,N) = SI(2,N)/CMET(3,METRIC)
      DO 1510 I  = 1,3
      WSNOW(I,N) = WSNOW(I,N)/CMET(3,METRIC)
      IF(METRIC.EQ.2) TBASE(I,N) = 1.8*TBASE(I,N)+32.0
      FW(I,N) = FW(I,N)/12.0
      IF(FW(I,N).GT.FWFRAC(I)*WSNOW(I,N)) FW(I,N) = FWFRAC(I)*WSNOW(I,N)
C=======================================================================
C     CONVERT TO MORE CONVENIENT FORMS FOR LATER USE.
C=======================================================================
      IF(ISNOW.NE.1) THEN
                     DXX = DHMAX(I,N)/CMET(6,METRIC)
                     DXM = DHMIN(I,N)/CMET(6,METRIC)
                     DHMAX(I,N) = (DXX+DXM)/2.0
                     DHMIN(I,N) = (DXX-DXM)/2.0
                     ENDIF
 1510 CONTINUE
      CNT(3) = CNT(3) + (WSNOW(2,N)+FW(2,N))*WAR(2,N)
      CNT(3) = CNT(3) + (WSNOW(1,N)+FW(1,N))*WAR(4,N)
      CNT(3) = CNT(3) + (WSNOW(3,N)+FW(3,N))*(WAR(1,N)+WAR(3,N))
 1520 CONTINUE
      RETURN
      ENDIF
  888 CALL IERROR
C=======================================================================
  200 FORMAT(//,
     1' ****************************************************',/,
     2' *  SNOWMELT SIMULATION - GENERAL INPUT PARAMETERS  *',/,
     3' ****************************************************',//)
  210 FORMAT(/,' SINGLE - EVENT SNOWMELT, ISNOW = 1',//,
     1' CATCHMENT ELEVATION...........',F7.1,' FEET MEAN SEA LEVEL')
  215 FORMAT(/,' SINGLE - EVENT SNOWMELT, ISNOW = 1',//,
     1' CATCHMENT ELEVATION...........',F7.1,' METERS MEAN SEA LEVEL')
  220 FORMAT(//,'MAXIMUM FREE WATER, FRACTION OF SNOW WATER EQUIV.',/,
     1' PERVIOUS AREA................',T55,F7.2,/,
     2' IMPERVIOUS AREA..............',T55,F7.2)
  230 FORMAT(//,' AIR TEMPERATURES IN DEGREES FAHRENHEIT AT EACH TIME IN
     1TERVAL (' ,F7.2,' HOUR(S))',//,(10F10.2))
  235 FORMAT(//,' AIR TEMPERATURES IN DEGREES CELSIUS AT EACH TIME INTER
     1VAL (' ,F7.2,' HOUR(S))',//,(10F10.2))
C#### WCH, 8/5/93.
  249 FORMAT(/,' CONTINUOUS SNOWMELT SIMULATION (ISNOW = 2).',/,
     1' PROGRAM EXPECTS TEMPERATURE AND WIND DATA ON NSCRAT(3) = UNIT NO
     2.',I3)
  250 FORMAT(/,' SNOWMELT PARAMETERS FOR ISNOW = 2',//,
     1' CATCHMENT ELEVATION...........',F7.1,' FEET MEAN SEA LEVEL',/,
     2' DIVIDING TEMPERATURE, BETWEEN ',/,
     3' SNOW AND RAIN.................',F7.1,' DEGREES FAHRENHEIT',//,
     4' MULTIPLY GAGE CATCH OF SNOW BY',F6.2,' TO GET ADJUSTED SNOW',/,
     5' CATCH FOR SIMULATION.',//)
C### WCH, 11/92.  INSERT CORRECT TEMPERATURE UNITS (SHOULD BE DEG C)
  255 FORMAT(/,' SNOWMELT PARAMETERS FOR ISNOW = 2',//,
     1' CATCHMENT ELEVATION...........',F7.1,' METERS MEAN SEA LEVEL',/,
     2' DIVIDING TEMPERATURE, BETWEEN ',/,
     3' SNOW AND RAIN.................',F7.1,' DEGREES CELSIUS',//,
     4' MULTIPLY GAGE CATCH OF SNOW BY',F6.2,' TO GET ADJUSTED SNOW',/,
     5' CATCH FOR SIMULATION.',//)
  260 FORMAT(' NORMALLY BARE IMPERVIOUS AREA',T55,F7.2,//)
  270 FORMAT(/,
     1' ANTECEDENT SNOW TEMPERATURE INDEX PARAMETER =',T55,F6.2,//,
     2' RATIO, NEGATIVE MELT COEFFICIENT/MELT COEFFICIENT = ',
     3T55,F6.2,//,' LATITUDE OF CATCHMENT =',T55,
     4 F6.2,' DEGREES NORTH',//,' LONGITUDE CORRECTION, STANDARD TIME MI
     5NUS MEAN SOLAR TIME =',T55,F6.1,' MINUTES',//,' NWS ID NUMBER FOR
     6TEMPERATURE MEASUREMENT STATION = ',T55,I6)
  280 FORMAT(1H1,/,'0VALUES OF AREAL DEPLETION CURVES (FRACTION OF SNOW
     1COVER VS. RELATIVE WATER EQUIV.)',/,'0WATER EQUIV/      FRACTION O
     2F AREA COVERED-',/,' WATER EQUIV AT     IMPERVIOUS     PERVIOUS',/
     3,'  100% COVER           AREA          AREA')
  330 FORMAT(/,' AVERAGE MONTHLY WIND SPEEDS IN MILES PER HOUR',//,
     1'  JAN.  FEB.  MAR.  APR.  MAY   JUN.  JUL.  AUG.  SEP.  OCT.  NOV
     2.  DEC.',/,12F6.1)
  335 FORMAT(/,' AVERAGE MONTHLY WIND SPEEDS IN KM PER HOUR',//,
     1'  JAN.  FEB.  MAR.  APR.  MAY   JUN.  JUL.  AUG.  SEP.  OCT.  NOV
     2.  DEC.',/,12F6.1)
 1210 FORMAT(//,
     1' ****************************************',/,
     2' *   SNOW DATA FOR EACH SUBCATCHMENT    *',/,
     3' ****************************************',//)
 1220 FORMAT(//,
     1' ********************************************************',/,
     2' * DATA FOR SINGLE-EVENT SNOWMELT SIMULATION (ISNOW=1)  *',/,
     3' ********************************************************',/,
     4' SUBCATCH- SNOW COVERED   INIT SNOW    INIT FREE  MELT COEFFICI',
     5'ENTS  BASE TEMPERATURES',/,' MENT',8X,
     6'FRACTIONS   (IN.WATER)  WATER (IN.) (IN.H2O/(HR-DEG F))',
     7 6X,'(DEG F)',/,' ORDER NO.  PERV  IMP     PERV IMP    PERV  IMP
     8   PERV    IMP',9X,'PERV    IMP',/,
     9' ---------  ----  ---     ---- ---    ----  ---     ----    ---',
     1 9X,'----    ---')
 1225 FORMAT(//,
     1' ********************************************************',/,
     2' * DATA FOR SINGLE-EVENT SNOWMELT SIMULATION (ISNOW=1)  *',/,
     3' ********************************************************',/,
     4' SUBCATCH- SNOW COVERED   INIT SNOW    INIT FREE  MELT COEFFICI',
     5'ENTS  BASE TEMPERATURES',/,' MENT',8X,
     6'FRACTIONS   (MM.WATER)  WATER (MM.) (MM.H2O/(HR-DEG F))',
     7 6X,'(DEG F)',/,' ORDER NO.  PERV  IMP     PERV IMP    PERV  IMP
     8   PERV    IMP',9X,'PERV    IMP',/,
     9' ---------  ----  ---     ---- ---    ----  ---     ----    ---',
     1 9X,'----    ---')
 1230 FORMAT(///,
     1' ******************************************************',/,
     2' * DATA FOR CONTINUOUS SNOWMELT SIMULATION (ISNOW=2)  *',/,
     3' ******************************************************',/,
     4 '* IMPERVIOUS AREA FRACTIONS AND SNOW REDISTRIBUTION  *',/,
     5' *       (PLOWING) FRACTIONS                          *',/,
     6' ******************************************************',//,
     7' SUBCATCH-  FRACTION IMPERVIOUS AREA    FRACTION OF SNOW ABOVE WE
     8PLOW IN. ON IMP2 AREA MOVED  WEPLOW',/,
     9'     MENT     SNOW COV-    NORMALLY        IMP1    PERV   PERV IN
     1  OUT OF  IMMED',10X,'(IN WATER)',/,
     2' ORDER NO.    ERED=IMP1    BARE=IMP2                    LAST SUBC
     3  SYSTEM   MELT',/,
     4' ---------   ----------    ---------       ----    ---- ---------
     5  ------   ----')
 1235 FORMAT(///,
     1' ******************************************************',/,
     2' * DATA FOR CONTINUOUS SNOWMELT SIMULATION (ISNOW=2)  *',/,
     3' ******************************************************',/,
     4 '* IMPERVIOUS AREA FRACTIONS AND SNOW REDISTRIBUTION  *',/,
     5' *       (PLOWING) FRACTIONS                          *',/,
     6' ******************************************************',//,
     7' SUBCATCH-  FRACTION IMPERVIOUS AREA    FRACTION OF SNOW ABOVE WE
     8PLOW MM. ON IMP2 AREA MOVED  WEPLOW',/,
     9'     MENT     SNOW COV-    NORMALLY        IMP1    PERV   PERV IN
     1  OUT OF  IMMED',10X,'(MM WATER)',/,
     2' ORDER NO.    ERED=IMP1    BARE=IMP2                    LAST SUBC
     3  SYSTEM   MELT',/,
     4' ---------   ----------    ---------       ----    ---- ---------
     5  ------   ----')
 1310 FORMAT ('/////// SNOW DATA LINE NO',I3, ' FOR SUBCAT',I5,' SHOULD
     1BE FOR SUBCAT',I5,/,'. DATA PRINTED BELOW (USING F5.2 FORMAT).')
 1311 FORMAT ('/////// SNOW DATA LINE NO',I3,' FOR SUBCAT',A10,/,
     1' SHOULD BE FOR SUBCAT',A10,'. DATA PRINTED BELOW (USING F5.2 FORM
     2AT).')
 1315 FORMAT(I5,15F5.2)
 1316 FORMAT(A10,15F5.2)
 1330 FORMAT ('//////// SUBCAT NOS',I5,' AND',I5,' ON SNOW DATA CARDS NO
     1.',I3,/,' DO NOT AGREE. CARDS PRINTED BELOW (USING F5.2 FORMAT).')
 1331 FORMAT ('//////// SUBCAT NOS',A10,' AND',A10,' ON SNOW DATA CARDS
     1NO.',I3,/,' DO NOT AGREE. CARDS PRINTED BELOW (USING F5.2 FORMAT).
     2')
 1350 FORMAT ('////// SNOW DATA CARD NO',I3, ' FOR SUBCAT',I5,/,
     1 ' EXCEEDS MAX ALLOWABLE NO. OF SUBCATCHMENTS,=',I3,/,
     2'. CARD PRINTED BELOW (USING F5.2 FORMAT).')
 1351 FORMAT ('////// SNOW DATA CARD NO',I3, ' FOR SUBCAT',A10,/,
     1 ' EXCEEDS MAX ALLOWABLE NO. OF SUBCATCHMENTS,=',I3,/,
     2'. CARD PRINTED BELOW (USING F5.2 FORMAT).')
 1380 FORMAT(2I5,F6.2,F5.2,F9.2,F6.2,F8.2,F5.2,F10.4,1X,F7.4,F11.2,F7.2)
 1381 FORMAT(I3,A7,F6.2,F5.2,F9.2,F6.2,F8.2,F5.2,F10.4,1X,F7.4,F11.2,
     1 F7.2)
 1390 FORMAT (2I5,F11.2,F13.2,F12.2,4F8.2,12X,F8.2)
 1391 FORMAT (I3,A7,F11.2,F13.2,F12.2,4F8.2,12X,F8.2)
 1395 FORMAT (//,' $$$ CAUTION. SNOW DATA CARD NO.',I3,'. REDISTRIBUTION
     1 FRACTIONS SUM TO ',F4.2,' INSTEAD OF 1.0.  CONTINUITY ERROR MAY R
     2ESULT.')
 1420 FORMAT ('/// IMPROPER NO. OF SNOW DATA CARDS READ IN,=',I3,
     1 '. REQUIRE',I4)
 1440 FORMAT(//,
     1' *************************************',/,
     2' * INITIAL CONDITIONS AND OTHER DATA *',/,
     3' *************************************',//,
     4' SUBCATCH-  INITIAL SNOW COVER',4X,'INITIAL FREE',7X,
     5'SNOW AMOUNT FOR 100%',/, ' MENT',9X,'  (IN WATER)  ',7X,
     6'WATER (IN.)',8X,'COVER (MM. WATER)',/,
     7' ORDER NO.   PERV  IMP1  IMP2    PERV  IMP1  IMP2',8X,
     8'PERV   IMP1',/,
     9' --------    ----  ----  ----    ----  ----  ----',8X,
     1'----   ----')
 1445 FORMAT(//,
     1' *************************************',/,
     2' * INITIAL CONDITIONS AND OTHER DATA *',/,
     3' *************************************',//,
     4' SUBCATCH-  INITIAL SNOW COVER',4X,'INITIAL FREE',7X,
     5'SNOW AMOUNT FOR 100%',/, ' MENT',9X,'  (MM WATER)  ',7X,
     6'WATER (MM.)',8X,'COVER (MM. WATER)',/,
     7' ORDER NO.   PERV  IMP1  IMP2    PERV  IMP1  IMP2',8X,
     8'PERV   IMP1',/,
     9' --------    ----  ----  ----    ----  ----  ----',8X,
     1'----   ----')
 1460 FORMAT (2I5,F7.2,2F6.2,F8.2,2F6.2,F12.2,F7.2)
 1461 FORMAT (I3,A7,F7.2,2F6.2,F8.2,2F6.2,F12.2,F7.2)
 1470 FORMAT(//,' *******************************',/,
     1          ' *  MELT COMPUTATION DATA      *',/,
     2          ' *******************************',//,
     3'SUBCATCH-  MAXIMUM MELT COEFFICIENTS  MINIMUM MELT COEFFICIENTS',
     4 4X,'BASE TEMPERATURES',/,' MENT',8X,
     5'(IN. WATER/(HR-DEG C)) (IN. WATER/(HR-DEG C))',11X,'(DEG C)',/,
     6' ORDER NO.      PERV   IMP1   IMP2',9X,'PERV   IMP1   IMP2',7X,
     7' PERV  IMP1  IMP2',/,
     8' ---------      ----   ----   ----',9X,
     9'----   ----   ----',7X,' ----  ----  ----')
 1475 FORMAT(//,' *******************************',/,
     1          ' *  MELT COMPUTATION DATA      *',/,
     2          ' *******************************',//,
     3'SUBCATCH-  MAXIMUM MELT COEFFICIENTS  MINIMUM MELT COEFFICIENTS',
     44X,'BASE TEMPERATURES',/,' MENT',8X,
     5'(MM. WATER/(HR-DEG C)) (MM. WATER/(HR-DEG C))',11X,'(DEG C)',/,
     6' ORDER NO.      PERV   IMP1   IMP2',9X,'PERV   IMP1   IMP2',7X,
     7' PERV  IMP1  IMP2',/,
     8' ---------      ----   ----   ----',9X,
     9'----   ----   ----',7X,' ----  ----  ----')
 1490 FORMAT (2I5,F10.4,2F7.4,F13.4,2F7.4,F12.2,2F6.2)
 1491 FORMAT (I3,A7,F10.4,2F7.4,F13.4,2F7.4,F12.2,2F6.2)
C=======================================================================
      END

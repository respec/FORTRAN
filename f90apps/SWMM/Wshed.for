      SUBROUTINE WSHED(REIN,KWIK)
C     RUNOFF BLOCK
C     CALLED BY HYDRO NEAR LINE 349
C=======================================================================
C     WSHED last updated December, 1990 by Bob Dickinson
C     Green-Ampt infiltration option added Feb 79 by Russell G. Mein.
C
C     This subroutine computes the instantaneous water depth
C          and flow rate for the watershed areas.
C=======================================================================
C      UPDATED 11/92 BY WCH TO READ EVAP DATA FROM NSCRAT(3) WHEN IVAP=4
C      UPDATED 3/93 BY RED TO CORRECT FOR RETURNING INFILTRATION FROM
C        SUBROUTINE GROUND.
C      UPDATED 8/93 BY CHUCK MOORE, CDM, TO ADD SUBCATCHMENT STATISTICS.
C      METRIC FIX AND NEW IF-STMT, 9/23/93.  WCH (RED).
C      OPTION FOR NO EVAPORATION DURING RAINY TIME STEP, WCH (CDM,
C        CHUCK MOORE), 10/5/93.
C      CORRECTION FOR MORE THAN ONE AIR TEMPERATURE PER DAY, RED (WCH),
C        1/31/94.
C      INITIALIZE VARIABLE  KWIKSN  TO INDICATE PRESENCE OF SNOW IN
C        ORDER TO USE  WET  TIME STEP, WCH, 4/7/94.
C      CHANGE LOCATION OF CHECK FOR GROUNDWATER FLOW, WCH, 4/7/94.
C      CHECK TO SEE IF K=1 CALCS HAVE BEEN MADE FOR USE BY K=3 SUBAREA,
C        WCH, 5/25/94.
C#######################################################################
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
      INCLUDE 'GRWTR.INC'
C#### C. MOORE, CDM, 8/93
      INCLUDE 'RUNSTAT.INC'
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CIM   MAXINF  C.Moore B. Cunningham CDM
      INCLUDE 'MAXINF.INC'
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CIM INCREASE HYETOGRAPHS   ~~~~~
      DIMENSION IDXSNO(4),TP(NW),REIN(MAXRG)
CIM      DIMENSION IDXSNO(4),TP(NW),REIN(10)
CIM  ~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DIMENSION GWFLWB(NGW),STPOLL(2,NGW)
      CHARACTER*3 CTYPE
      DATA IDXSNO/3,2,3,1/
C=======================================================================
C#######################################################################
C WCH, 11/92  ADD OPTION FOR IVAP=4 TO READ TEMP BLOCK OUTPUT
C               FOR EVAPORATION DATA
C#######################################################################
      IF((ISNOW.EQ.2.OR.IVAP.EQ.4).AND.MTIME.EQ.1) THEN
               IF(NSCRAT(3).EQ.0) CALL ERROR(20)
C### WCH 11/92
               REWIND NSCRAT(3)
C###
               READ(NSCRAT(3)) ISTA
               WRITE(N6,9200) ISTA
 5             READ(NSCRAT(3)) JTDAY,TMAX,TMIN,EVAPOR,WINDER
               JYEAR = JTDAY / 1000
               IF (JYEAR.LT.100) THEN
               JTDAY = JTDAY - JYEAR*1000
               JYEAR = JYEAR + 1900
               JTDAY = JTDAY + JYEAR*1000
               ENDIF
C#### WCH (RED), 9/93.  METRIC FIX.
               EVAPOR = EVAPOR/CMET(7,METRIC)
               TMAX1 = TMAX
               IF(JTDAY.LT.JULDAY) GO TO 5
               ENDIF
C=======================================================================
      IF(MTIME.EQ.1) THEN
                     JDREF   = JULDAY - 1
                     RINE    = 0.0
                     XSINFL  = 0.0
                     WINDER  = 0.0
                     DO 10 N = 1,NW
   10                TP(N)   = 0.0
                                    KKK = 3
                     IF(ISNOW.GE.1) KKK = 4
                     ENDIF
C=======================================================================
      IF(ISNOW.EQ.1) THEN
                     INDT = (TIME-TZERO)/(DTAIR*3600.0)
                     IF(INDT.LT.1) INDT = 1
                     IF(INDT.GT.NAIRT) INDT = NAIRT
                     TA = TAIR(INDT)
                     ENDIF
C=======================================================================
C     Here, compute melt factors for different days.
C     Assume sinusoidal variation between min on Dec 21 &
C     Max on June 21.  Compute only when starting a new day.
C     The number 0.0172615 = PI/182.
C=======================================================================
C####################
C WCH 11/92  CHANGE TO READ NSCRAT3 FOR EVAP ALONE
C####################
      IF((ISNOW.EQ.2.OR.IVAP.EQ.4).AND.JTDAY.LT.JULDAY) THEN
               TMAX1 = TMAX
               READ(NSCRAT(3)) JTDAY,TMAX,TMIN,EVAPOR,WINDER
               JYEAR = JTDAY / 1000
               IF (JYEAR.LT.100) THEN
               JTDAY = JTDAY - JYEAR*1000
               JYEAR = JYEAR + 1900
               JTDAY = JTDAY + JYEAR*1000
               ENDIF
C#### WCH (RED), 9/93.  METRIC FIX.
               EVAPOR = EVAPOR/CMET(7,METRIC)
               ENDIF
C=======================================================================
      IF(ISNOW.GT.1.AND.JULDAY.GT.JDREF) THEN
                     JDREF   = JULDAY
                     MDAY    = JULDAY - 1000*(JULDAY/1000)
                     DAYNO   = FLOAT(MDAY)
                     SEASON  = SIN(0.0172615*(DAYNO-81.0))
                     DO 80 N = 1,NOW
                     DO 80 I = 1,3
   80                DH(I,N) = DHMAX(I,N)+DHMIN(I,N)*SEASON
C=======================================================================
C******** COMPUTE HOUR OF DAY OF SUNRISE AND SUNSET.
C******** THEN MIN. TEMP. IS ASSUMED TO OCCUR AT SUNRISE.
C******** MAX. TEMP. IS ASSUMED TO OCCUR AT SUNSET MINUS THREE HOURS.
C
C******** COMPUTE EARTH'S DECLINATION.
C******** THE NUMBER 0.40928 = 23.45 (DEGREES) * PI / 180.
C******** THE NUMBER 0.017202 = 2 PI / 365.
C
C******** COMPUTE THE HOUR ANGLE OF SUNRISE/SUNSET.
C******** THE NUMBER 3.8197 = 12 / PI.
C
C******** COMPUTE HOURLY TEMPERATURE USING SINSUOIDAL INTERPOLATION.
C=======================================================================
                     DECL  = 0.40928*COS(0.017202*(172.0-DAYNO))
                     HRANG = 3.8197*ACOS(-TAN(DECL)*ANGLAT)
                     HRSR  = 12.0 - HRANG + DTLONG + 0.5
                     HRSS  = 12.0 + HRANG + DTLONG - 2.5
                     DHRDY =  HRSR - HRSS
                     DYDIF =  24.0 + HRSR - HRSS
                     HRDAY = (HRSR + HRSS)/2.0
                     TAVE  = (TMAX + TMIN) / 2.0
                     TRNG  = (TMAX - TMIN) / 2.0
                     TRNG1 = (TMAX1 - TMIN)
                     ENDIF
C#### RED (WCH), 1/31/94.  SHOULD HAVE ENDIF HERE, OTHERWISE
C####   ONLY ONE AIR TEMPERATURE PER DAY.  BUT ALSO NEED ADDITIONAL
C####   IF-STMT HERE TO PERFORM FOLLOWING ONLY FOR ISNOW > 1.
      IF(ISNOW.GT.1) THEN
                     HR    = TIMDAY/3600.0
                     IF(HR.LT.HRSR) TA = TMIN +
     +                  TRNG1 * SIN(3.14159 / DYDIF * (HRSR - HR))
                     IF(HR.GE.HRSR.AND.HR.LE.HRSS) TA = TAVE
     +                 + TRNG * SIN(3.14159 / DHRDY * (HRDAY - HR))
                     IF(HR.GT.HRSS) TA  = TMAX -
     +                  TRNG * SIN(3.14159 / DYDIF *(HR - HRSS))
                     ENDIF
C=======================================================================
C     Begin major loop for WSHED.
C=======================================================================
      RI       = 0.0
      RIN      = 0.0
      IF(IVAP.LE.2) EVAP = VAP(MONTH)
      IF(IVAP.EQ.3) THEN
                    INDX = 12*(NYEAR - NVAP(1)) + MONTH
                    IF(INDX.LE.0.OR.INDX.GT.NVAP(2)) CALL ERROR(153)
                    EVAP = VAP(INDX)
                    ENDIF
      IF(IVAP.EQ.4) EVAP = EVAPOR
                        WINDY  = WINDER
      IF(WINDER.EQ.0.0) WINDY  = WIND(MONTH)
      KWIK     = 0
      KOMPUT   = 0
C#### WCH, 4/7/94.  USE ONLY VARIABLE KWIKSN FOR SNOW.
C#### WCH (RED), 9/93.  ADD IF-STMT.
C####      IF(ISNOW.EQ.1) KWIK = 1
C#### WCH, 4/7/94.  INITIALIZE SNOW INDICATOR VARIABLE.
      KWIKSN   = 0
      DO 390 J = 1,NOW
      NGAG     = NHYET(J)
      RI       = REIN(NGAG)/CMET(4,METRIC)
      IF(ISNOW.EQ.2.AND.TA.LE.SNOTMP)  RI = -RI * SCF
      IF(ISNOW.EQ.1.AND.NGAG.GT.1) KOMPUT = 0
C#######################################################################
C     WCH (CDM, Chuck Moore), 10/5/93.  If IIVAP > 0 then use zero
C     evaporation during time steps with precipitation.
C     IF ABS(IIVAP) = 1 evap is allowed from channels.
C     IF ABS(IIVAP) = 2 evap is not allowed from channels.
C     SVAP = evaporation from this subcatchment that will be set to
C     zero if it is raining or snowing.
C     GVAP = evaporation from channel surfaces that will be set to
C     zero if there is rain or snow over any subcatchment.
C===> NOTE! EVAP changed to SVAP from here to end of subroutine (5
C     locations) and to SVAP or GVAP in subroutines GAMP, HORTON,
C     GROUND, and GUTTER.
C#######################################################################
C#### WCH (CDM), 10/5/93.  NEW PARAMS FOR NO EVAP DURING RAIN.
      IF(IIVAP.GT.0) THEN
           IF(RI.LE.0.0) THEN
                SVAP = EVAP
                GVAP = EVAP
                IF (IIVAP.EQ.2) GVAP = 0.0
                ELSE
                SVAP = 0.0
                GVAP = 0.0
                ENDIF
            ELSE
            SVAP = EVAP
            GVAP = EVAP
            IF (IIVAP.EQ.-2) GVAP = 0.0
           ENDIF
C
      RIN      = RI
      RINE     = 0.0
      IF(RIN.GT.0.0) RINE = RIN
      WFLOW(J) = 0.0
      IF(WAREA(J).EQ.0.0) GO TO 390
C=======================================================================
C     Begin loop on 3 or 4 subareas within each subcatchment.
C
C     Subscripts for different areas ---
C
C     K=1 -- IMPERVIOUS AREA WITH DEPRESSION STORAGE.  NORMALLY BARE,
C            BUT MAY HAVE SNOW FOR ISNOW=2.
C     K=2 -- PERVIOUS AREA WITH DEPRESSION STORAGE.  SNOW COVER VARIES
C            ACCORDING TO AREAL DEPLETION CURVE FOR ISNOW=2 AND IS CONSTANT
C            FOR ISNOW=1.
C     K=3 -- IMPERVIOUS AREA WITH ZERO DEPRESSION STORAGE.  SNOW COVER
C            IS SAME AS FOR K=1.
C     K=4 -- IMPERVIOUS AREA WITH DEPRESSION STORAGE.  USED ONLY WHEN
C            SNOW CALCS ARE MADE.  SNOW COVER VARIES ACCORDING TO AREAL
C            DEPLETION CURVE FOR ISNOW=2 AND IS 100% FOR ISNOW=1.
C
C     SUBSCRIPTS FOR SNOW VARIABLES ---
C        FOR K =  1 II = 3
C            K =  2 II = 2
C            K =  3 II = 3 (SAME SNOW PACK AS K=1)
C            K =  4 II = 1
C=======================================================================
C#### WCH, 5/25/94.  INDICATOR VARIABLE TO SEE IF K=1 IS USED.
      K3CALC = 0
      DO 380 K = 1,KKK
      IF(WAR(K,J).LT.0.01) GO TO 380
      WFLO   = 0.0
      SMIMED = 0.0
      ASC    = 0.0
      II     = IDXSNO(K)
                 RLOSS  = 0.0
      IF(K.NE.2) RLOSS  = SVAP
C=======================================================================
C     Treat melt in same manner as rain on area.
C=======================================================================
      IF(K.EQ.1) THEN
C#### WCH, 5/25/94. NEED K=1 CALCS FOR K=3.  CHECK IF CALCULATED.
                 K3CALC = 1
                 IF(ISNOW.GT.0) CALL SNOW(TA,SMIMED,J,K,II,RIN,RINE,
     +                                    ASC,WINDY,KOMPUT)
                 RI     = RI*ASC + (1.0-ASC)*RINE + SMIMED
                 RISAVE = RI
                 DOI    = WDEPTH(K,J) - WSTORE(1,J)
                 WSTOR  = WSTORE(1,J)
                 IF(WDEPTH(K,J).LE.0.0.AND.RI.LE.0.0) GO TO 350
                 ENDIF
C=======================================================================
      IF(K.EQ.3) THEN
C#### WCH, 5/25/94. BE SURE THAT K=1 CALCS HAVE BEEN MADE!
                 IF(K3CALC.EQ.0) THEN
                      IF(ISNOW.GT.0) CALL SNOW(TA,SMIMED,J,K,II,RIN,
     +                                    RINE,ASC,WINDY,KOMPUT)
                      RI     = RI*ASC + (1.0-ASC)*RINE + SMIMED
                      RISAVE = RI
                      ENDIF
                 RI     = RISAVE
                 DOI    = WDEPTH(K,J)
                 WSTOR  = 0.0
                 IF(WDEPTH(K,J).LE.0.0.AND.RI.LE.0.0) GO TO 350
                 ENDIF
C=======================================================================
      IF(K.EQ.4) THEN
                 IF(ISNOW.GT.0) CALL SNOW(TA,SMIMED,J,K,II,RIN,RINE,
     +                                    ASC,WINDY,KOMPUT)
                 RI     = RI*ASC + (1.0-ASC)*RINE + SMIMED
                 DOI    = WDEPTH(K,J) - WSTORE(1,J)
                 WSTOR  = WSTORE(1,J)
                 IF(WDEPTH(K,J).LE.0.0.AND.RI.LE.0.0) GO TO 350
                 ENDIF
C=======================================================================
C     K = 2 Calls infiltration routines for pervious areas.
C                 Treat melt in same manner as rain on area.
C=======================================================================
      IF(K.EQ.2) THEN
                 IF(ISNOW.GT.0) CALL SNOW(TA,SMIMED,J,K,II,RIN,RINE,
     +                                    ASC,WINDY,KOMPUT)
                 RI     = RI*ASC + (1.0-ASC)*RINE + SMIMED
                 DOI   = WDEPTH(K,J) - WSTORE(2,J)
                 WSTOR = WSTORE(2,J)
C   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CIM FIX FOR INFILM =2 or 3
                 IF(INFILM.EQ.0.OR.INFILM.EQ.2) CALL HORTON(TP(J),K,J)
                 IF(INFILM.EQ.1.OR.INFILM.EQ.3)
C   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     +           CALL GAMP(WLMAX(J),SMD(J),DECAY(J),
     1                  IFLAG(J),FU(J),FTOT(J),WLMIN(J),WDEPTH(K,J),
     2                  TP(J),DELT,TIME,RI,RLOSS,UL(J),SVAP,
     3                  KAMEW(J),NAMEW(J))
C=======================================================================
C     Call groundwater subroutine.
C=======================================================================
cim   the following line causes the number of subcatchments with
cim   groundwater to have to exceed or equal the number of subcatchments
cim   I changed array of NMSUB to NW in GRWTR.INC.
                 IF(NMSUB(J).GT.0) THEN
                             ENFIL = RLOSS - SVAP
                             IF(ENFIL.LT.0.0) ENFIL = 0.0
                             CALL GROUND(J,XSINFL)
C#### WCH, 4/7/94.  CHECK HERE FOR GROUNDWATER FLOW IN ORDER TO
C     USE WETDRY TIME STEP.
                             IF(GWFLOW(J).GT.0.0) KWIK = 1
C#### CORRECTION BY RED, 3/93. DIVIDE BY DELT.
                             ENFIL   = ENFIL - XSINFL/DELT
                             IF(ENFIL.LT.0.0) ENFIL = 0.0
C#### CORRECTION BY RED, 3/93. DIVIDE BY DELT.
                             RLOSS   = RLOSS - XSINFL/DELT
                             IF(RLOSS.LT.0.0) RLOSS = SVAP
                             CNT(11) = CNT(11) + ETU*WAR(2,J)*DELT
                             CNT(12) = CNT(12) + ETD*WAREA(J)*DELT
                             CNT(14) = CNT(14) + DEPPRC*WAREA(J)*DELT
                             CNT(17) = CNT(17) + ENFIL*WAR(K,J)*DELT
                             ENDIF
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C CHANGED IF BECAUSE TOTAL INFILTRATION CALCULATIONS SHOULD NOT BE
C BYPASSED DURING A DRY TIME STEP
C                IF(WDEPTH(K,J).LE.0.0.AND.RI.LE.0.0) GO TO 350
        IF(WDEPTH(K,J).LE.0.0.AND.RI.LE.0.0.AND.INFILM.LT.2) GO TO 350
CIM  Fix to limit maximum infiltration volume and track infiltration
C%%% here
         IF (INFILM.GE.2) THEN
                 FINF = DMAX1((RLOSS - SVAP),DBLE(0.0))
                 TEMPIN=TOTINF(J) + FINF*DELT
                 FAVAIL = AMAX1((RMAXINF(J)-TEMPIN)/DELT,0.0)
                 IF (FAVAIL.LE.FINF) THEN
CIM  REDUCE FINF CAUSE INFILTRATION VOLUME IS USED UP
                  FINF=FAVAIL
                  TEMPIN=TOTINF(J) + FINF*DELT
                 RLOSS=FINF+SVAP
                 ENDIF
                 TOTINF(J)=TEMPIN
              ENDIF
                 ENDIF
C=======================================================================
      RSTAR = RI - RLOSS
                                        CTYPE = 'DR2'
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C CHANGED STATEMENT BELOW BECAUSE ROUNDOFF ERROR WAS CAUSING DR1s TO BE
C DR2s
C      IF(RSTAR+WDEPTH(K,J)/DELT.LE.0.0) CTYPE = 'DR1'
      IF(RSTAR*DELT+WDEPTH(K,J).LE.0.000001) CTYPE = 'DR1'
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      IF(RSTAR*DELT+DOI.GT.0.0)         CTYPE = 'WET'
C=======================================================================
C     DR1 - No water is available to run off.
C=======================================================================
      IF(CTYPE.EQ.'DR1') THEN
                         RLOSS       = RI + WDEPTH(K,J)/DELT
                         WDEPTH(K,J) = 0.0
                         WFLO        = 0.0
                         ENDIF
C=======================================================================
C     DR2 - Depth less than depression storage.
C=======================================================================
      IF(CTYPE.EQ.'DR2') THEN
                         WFLO  = 0.0
                         DCORR = WDEPTH(K,J) + RSTAR*DELT
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C HOW CAN DCORR EVER BE LESS THAN 0.0?  IF DCORR IS LESS THAN 0.0, CTYPE
C IS 'DR1' BY DEFINITION.  GET RID OF IF?
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                         IF(DCORR.LT.0.0) THEN
                                          RLOSS = WDEPTH(K,J)/DELT + RI
                                          DCORR = 0.0
                                          ENDIF
                         WDEPTH(K,J) = DCORR
                         ENDIF
C=======================================================================
C     Storage greater than depression storage - Call flow routing.
C=======================================================================
      IF(CTYPE.EQ.'WET') THEN
                          WCC = WCON(1,J)
               IF(K.EQ.2) WCC = WCON(2,J)
C=======================================================================
C              Test for the condition in which old DOI is negative but
C              the new DOI will be positive based on RI - RLOSS.
C=======================================================================
               IF(DOI.LT.0.0) THEN
                              RSTAR       = RSTAR + DOI/DELT
                              DOI         = 0.0
                              WDEPTH(K,J) = WSTOR
                              ENDIF
               CALL OVERLND(J,WCC,RSTAR,DOI,DEL,DELT)
               DCORR = WDEPTH(K,J) + DEL
               IF(DCORR.GT.WSTOR) THEN
                        WFLO = -WCC*WAR(K,J)*(DCORR-WSTOR)**1.666667
                        ELSE
                        WFLO  = 0.0
                        DCORR = WDEPTH(K,J) + RSTAR*DELT
                        IF(DCORR.LT.0.0) THEN
                                         RLOSS = WDEPTH(K,J)/DELT + RI
                                         DCORR = 0.0
                                         ENDIF
                        ENDIF
               WDEPTH(K,J) = DCORR
               KWIK        = 1
               ENDIF
C=======================================================================
C     Sum for continuity check.
C=======================================================================
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C FINF HAS ALREADY BEEN CALCULATED IF INFILM=2, SO ADD IF
      IF(K.EQ.2) THEN
C%%%%%
                 IF(INFILM.LT.2) THEN
                                 FINF= RLOSS - SVAP
                 IF(FINF.LT.0.0) FINF = 0.0
                 ENDIF
C%%%%%
                 RLOSS                  = RLOSS - FINF
                 IF(RLOSS.LT.0.) RLOSS  = 0.0
                 CNT(4)  = CNT(4) + FINF*WAR(K,J)*DELT
                 CNT(6)  = CNT(6) + RLOSS*WAR(K,J)*DELT
                 ELSE
                 CNT(6)  = CNT(6) + RLOSS*WAR(K,J)*DELT
                 ENDIF
C#######################################################################
C  C. MOORE, CDM, 8/93.  STATISTICS FOR SUBCATCHMENTS.
C#######################################################################
      IF (SUBQPEAK(K,J).LT.WFLO)  SUBQPEAK(K,J) = WFLO
      SUBDEP(K,J) = SUBDEP(K,J) + WFLO*DMEAN
C###
      CNT(21)  = CNT(21)  + WFLO*DMEAN
      WFLOW(J) = WFLOW(J) + WFLO
 350  CNT(1)   = CNT(1)   + ABS(RIN)*WAR(K,J)*DELT
      CNT(2)   = CNT(2)   + RINE*WAR(K,J)*DELT
C   CIM     track total infiltration volume
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C  REGENERATE TOTINF
      IF(K.EQ.2.AND.INFILM.EQ.2) THEN
        IF(TOTINF(J).GE.RMAXINF(J)) TOTINF(J)=RMAXINF(J)
        IF(WDEPTH(K,J).LE.0.0.AND.RI.LE.0.0) THEN
          IF(TOTINF(J).LE.RMAXINF(J)*.01) THEN
            TOTINF(J)=0.0
          ELSE
            TOTINF(J)=RMAXINF(J)*EXP(-DECAY(J)*REGEN*(DELT-ALOG(TOTINF(J
     *)/RMAXINF(J))/(DECAY(J)*REGEN)))
          ENDIF
        ENDIF
      ENDIF
c  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  380 CONTINUE
C#######################################################################
C  C. MOORE, CDM, 8/93.  STATISTICS FOR SUBCATCMENTS.
C  PEAK TOTAL IMPERVIOUS AREA RUNOFF
C=======================================================================
      TEMP = SUBQPEAK(1,J) + SUBQPEAK(3,J)
      IF (SUBQPEAK(5,J).LT.TEMP) SUBQPEAK(5,J) = TEMP
C=======================================================================
C  PEAK TOTAL RUNOFF RATE
C=======================================================================
      TEMP = TEMP + SUBQPEAK(2,J) + SUBQPEAK(4,J)
      IF (SUBQPEAK(6,J).LT.TEMP) SUBQPEAK(6,J) = TEMP
C#######################################################################
  390 CONTINUE
C=======================================================================
C     Save Subsurface plot information.
C=======================================================================
      IF(NSVGW.GT.0) THEN
                     MCOUN     = 0
                     DO 395 JH = 1,NOGWSC
C#### WCH, 4/7/94.  MAKE THIS CHECK EARLIER.
C####                     IF(GWFLOW(JH).GT.0.0) KWIK = 1
                     IF(NSCSFG(JH).EQ.0) GO TO 395
                     MCOUN           = MCOUN+1
                     GWFLWB(MCOUN)   = GWFLOW(JH)/CMET(1,METRIC)
                     STPOLL(1,MCOUN) = STG(JH)/CMET(8,METRIC)
                     STPOLL(2,MCOUN) = TH1(JH)
395                  CONTINUE
                     WRITE(NSCRAT(6)) JULDAY,TIMDAY,TIME,(GWFLWB(JI),
     .                               (STPOLL(JV,JI),JV=1,2),JI=1,NSVGW)
                     ENDIF
      RETURN
C=======================================================================
 9200 FORMAT(/,' Reading station number ',I10,' from NSCRAT(3) for',
     +' Temperature, Wind Speed, or Evaporation data.')
C=======================================================================
      END

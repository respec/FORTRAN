      SUBROUTINE GROUND(J,XSINFL)
C     RUNOFF BLOCK
C     CALLED BY WSHED NEAR LINE 313
C=======================================================================
C     THIS MODULE IS DESIGNED TO PREDICT MOISTURE CONTENT,
C     FREE SURFACE GROUND-WATER LEVEL, AND GROUNDWATER FLOW.
C     CREATED APRIL, 1987 BY BRETT CUNNINGHAM
C     UPDATED 12/92 BY WCH TO ADD LIMIT TO NON-CONVERGENCE MESSAGES.
C     CORRECTION BY RED, 3/93 (& 9/93).  ADD ERROR CHECKS FOR D2 VALUE IN 
C       ITERATIVE SOLUTION.
C     CORRECTION 7/27/93 BY BRETT CUNNINGHAM, CDM, TO FIX DERIVATIVE, DZ.
C     CHANGE EVAP TO SVAP (3 LOCATIONS) FOR NO EVAPORATION DURING 
C       RAINY TIME STEPS, WCH (CDM), 10/5/93.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'GRWTR.INC'
C#######################################################################
C WCH, 12/92.  ADD LIMIT ON NUMBER OF NON-CONVERGENCE MESSAGES.
C PARAMETER IPRNGW ENTERED ON B2 LINE, HELD IN TIMER.INC
C#######################################################################
      IF(MTIME.LE.1) IPGW = 0
C=======================================================================
C     INITIALIZE PARAMETERS
C=======================================================================
      ENFILT = ENFIL + XTRENF(J)
      ETAVLB = SVAP
      IF(ETAVLB.LT.0.0) ETAVLB = 0.0
      PERAR = WAR(2,J)/WAREA(J)
      D1    = STG(J) - BELEV(J)
      DTOT  = GRELEV(J) - BELEV(J)
      DWT1  = GRELEV(J) - STG(J)
      ZEL   = 0.0010/POR(J)
      BO    = BC(J) - BELEV(J)
      IF(TW(J).GE.0.0) THEN
                TA = TW(J) - BELEV(J)
                ELSE
                DO 85 IV = 1,NOG
                IF(JCE.EQ.0.AND.NGWTOG(J).NE.NAMEG(IV)) GO TO 85
                IF(JCE.EQ.1.AND.KGWTOG(J).NE.KAMEG(IV)) GO TO 85
                TWDUM = GDEPTH(IV)
                IF(NPG(IV).EQ.2) TWDUM=GWIDTH(IV)/2.*(1-COS(GDEPTH(IV)))
                TA = TWDUM + BO
                GO TO 90
  85            CONTINUE
                ENDIF
  90  XSINFL = 0.0
C=======================================================================
C     DETERMINE FLUXES THAT ARE CONSTANT OVER TIME STEP
C=======================================================================
CWCH, 7/16/99, correction for divide by zero error here. 
C      IF(TA-BO.GT.0.0.OR.B2(J).NE.0.) THEN
      TWFLW = 0.0
	IF(TA-BO.GT.0.0.AND.B2(J).NE.0.) THEN
	                                TWFLW = A2(J)*(TA-BO)**B2(J)
                                      ELSE
                                      TWFLW = A2(J)
                                      ENDIF
      ETU = CET(J)*SVAP
      IF(TH1(J).LT.WP(J).OR.ENFIL.GT.0.0) ETU = 0.0
      IF(ETU.GT.ETAVLB)                   ETU = ETAVLB
      ETD = (DET(J)-DWT1)*SVAP*(1-CET(J))/DET(J)
      IF(ETD.GT.(ETAVLB-ETU)) ETD = ETAVLB-ETU
      IF(ETD.LE.0.0)          ETD = 0.0
      HKTH = HKSAT(J)*EXP((TH1(J)-POR(J))*HCO(J))
      PERC = 0.0
      IF(DWT1.EQ.0.)      GO TO 100
      IF(TH1(J).LE.FC(J)) GO TO 100
      PERC = HKTH*(1.+PCO(J)*(TH1(J)-FC(J))/(DWT1/2.))
      IF(PERC*DELT.GT.(TH1(J)-FC(J))*DWT1) PERC=(TH1(J)-FC(J))*DWT1/DELT
C=======================================================================
C     CALCULATE BEGINNING-OF-TIME-STEP GROUNDWATER FLOW AND
C     DEEP PERCOLATION AND OTHER CONSTANTS USED REPEATEDLY
C     IN CALCULATIONS
C=======================================================================
 100  DEPPRC = (D1/DTOT)*DP(J)
      IF(D1.GT.BO.AND.TA.LT.D1) THEN
                  GWFLW = A1(J)*(D1-BO)**B1(J)-TWFLW+A3(J)*D1*TA
                  ELSE
                  GWFLW = 0.0
                  ENDIF
C=======================================================================
C      DETERMINE AVAILABLE STORAGE BASED ON INITIAL AND CONSTANT
C      CONDITIONS. ADJUST PERC, IF NECESSARY.
C=======================================================================
      IF(DWT1.LE.0.0) THEN
                      TH2    = FC(J)
                      AVLVOL = DWT1*(POR(J)-TH1(J)) +
     +                        ((ETD+ETU)*PERAR+GWFLW+DEPPRC)*DELT
                      ELSE
                      AVLVOL = DWT1*(POR(J)-TH1(J)) +
     +                        ((ETD+ETU)*PERAR+GWFLW+DEPPRC)*DELT
                      APERC = AVLVOL/DELT-ENFILT*PERAR
                      IF(DWT1.LE.ZEL*1.0001.AND.PERC.GE.APERC)
     +                                          PERC = .999*APERC
                      IF(PERC.LT.0.0)           PERC = 0.0
                      TH2 = ((ENFILT*PERAR-PERC-ETU*PERAR)*DELT +
     +                        TH1(J)*DWT1)/DWT1
                      ENDIF
      CLWRZN = PERC-ETD*PERAR
      DTHETA = TH1(J)-TH2
      PRTHTA = POR(J)-TH2
C=======================================================================
C **** IF SOIL WILL BE SATURATED OR VERY CLOSE TO SATURATED      ****
C **** DURING THE TIME STEP, BYPASS ITERATIONS.                  ****
C=======================================================================
      IF((ENFILT*PERAR*DELT).GT.AVLVOL) GOTO 250
 125  IF((AVLVOL-ENFILT*PERAR*DELT).LT..0001) GOTO 290
C=======================================================================
C     DETERMINE IF STAGE IS RISING OR FALLING
C=======================================================================
      IF((CLWRZN-GWFLW-DEPPRC).LT.0.0) GO TO 190
C=======================================================================
C     BEGIN ITERATIONS TO DETERMINE END-OF-TIME-STEP GROUNDWATER ****
C     FLOW,STAGE, AND SOIL MOISTURE FOR RISING WATER TABLE       ****
C=======================================================================
      D2        = D1
      DO 180 I1 = 1,12
      DOLD      = D2
      IF(D2.LE.BO.OR.D2.LE.TA) GOTO 140
      Z=((CLWRZN-.5*(GWFLW+A1(J)*(D2-BO)**B1(J)-TWFLW+DEPPRC+(D2/
     .  DTOT)*DP(J)+A3(J)*D2*TA))*DELT+(D2-D1)*DTHETA)/PRTHTA+D1-D2
C#### BRETT CUNNINGHAM (CDM), 7/27/93. 
C#### MULTIPLY A1 BY B1 TO GET CORRECT DERIVATIVE.
      DZ=-1+(DTHETA-.5*DELT*(A1(J)*B1(J)*(D2-BO)**(B1(J)-1)+DP(J)/DTOT
     * +  A3(J)*TA))/PRTHTA
      NFLFLG = 1
      D2     = D2-Z/DZ
      GOTO 150
 140  Z = ((CLWRZN-.5*(GWFLW+DEPPRC+D2/DTOT*DP(J)))*DELT+(D2-D1)*
     .      DTHETA)/PRTHTA+D1-D2
      DZ     = -1.0 + (DTHETA-.5*DELT*DP(J)/DTOT)/PRTHTA
      NFLFLG = 0
      D2     = D2-Z/DZ
 150  IF(ABS(D2-DOLD).GT..0001) GOTO 180
      IF(D2.LE.BO.OR.D2.LE.TA.AND.NFLFLG.EQ.0) GOTO 170
      IF(D2.GT.BO.AND.D2.GT.TA.AND.NFLFLG.EQ.1) GOTO 160
      GOTO 180
 160  GWFLOW(J) = A1(J)*(D2-BO)**B1(J)-TWFLW+A3(J)*D2*TA
      GOTO 330
 170  GWFLOW(J) = 0.0
      GOTO 330
 180  CONTINUE
      IF(I1.EQ.12.AND.ABS(DOLD-D2).LE..0001) GOTO 330
      GWFLOW(J) = GWFLW
      D2        = D1
C##### WCH, 12/92
      IPGW = IPGW + 1
      IF(IPGW.LE.IPRNGW.AND.JCE.EQ.0) WRITE(N6,1000) NAMEW(J),TIME
      IF(IPGW.LE.IPRNGW.AND.JCE.EQ.1) WRITE(N6,1001) KAMEW(J),TIME
      GOTO 330
C=======================================================================
C **** BEGIN ITERATIONS TO DETERMINE END-OF-TIME-STEP GROUNDWATER ****
C **** FLOW,STAGE,AND SOIL MOISTURE FOR FALLING WATER TABLE       ****
C=======================================================================
 190  D2        = D1
      DO 240 IP = 1,12
      DOLD      = D2
      IF(D2.LE.BO.OR.D2.LE.TA) GOTO 200
      Z=((CLWRZN-.5*(GWFLW+A1(J)*(D2-BO)**B1(J)-TWFLW+DEPPRC+A3(J)*D2*TA
     .          + D2/DTOT*DP(J)))*DELT)/PRTHTA+D1-D2
C#### BRETT CUNNINGHAM (CDM), 7/27/93. 
C#### MULTIPLY A1 BY B1 TO GET CORRECT DERIVATIVE.
      DZ = -1.0-.5*DELT/PRTHTA*(A1(J)*B1(J)*(D2-BO)**(B1(J)-1) +
     +      A3(J)*TA+DP(J)/DTOT)
      NFLFLG = 1
      D2     = D2-Z/DZ
      GOTO 210
 200  Z = ((CLWRZN-.5*(GWFLW+DEPPRC+D2/DTOT*DP(J)))*DELT)/PRTHTA+D1-D2
      DZ = -1.0-0.5*DP(J)*DELT/DTOT/PRTHTA
      D2 = D2-Z/DZ
      NFLFLG = 0
 210  IF(ABS(D2-DOLD).GT..0001) GOTO 240
      IF(D2.LE.BO.OR.D2.LE.TA.AND.NFLFLG.EQ.0) GOTO 230
      IF(D2.GT.BO.AND.D2.GT.TA.AND.NFLFLG.EQ.1) GOTO 220
      GOTO 240
 220  GWFLOW(J) = A1(J)*(D2-BO)**B1(J)-TWFLW+A3(J)*D2*TA
      GOTO 330
 230  GWFLOW(J) = 0.0
      GOTO 330
 240  CONTINUE
      IF(IP.EQ.12.AND.ABS(DOLD-D2).LE..0001) GOTO 330
      GWFLOW(J) = GWFLW
      D2        = D1
C##### WCH, 12/92
      IPGW = IPGW + 1
      IF(IPGW.LE.IPRNGW.AND.JCE.EQ.0) WRITE(N6,1000) NAMEW(J),TIME
      IF(IPGW.LE.IPRNGW.AND.JCE.EQ.1) WRITE(N6,1001) KAMEW(J),TIME
      GOTO 330
C=======================================================================
C **** RESET INITIAL INITIAL CONDITIONS, DETERMINE GROUNDWATER ****
C **** FLOW, SET FLAG, AND CALCULATE INFILTRATION NOT ACCEPTED ****
C **** WHEN SOIL IS SATURATED.                                 ****
C=======================================================================
 250  D2     = DTOT
      IF(D2.GT.BO.AND.D2.GT.TA) GOTO 260
      GWFLOW(J) = 0.0
      GOTO 270
 260  GWFLOW(J) = A1(J)*(DTOT-BO)**B1(J)-TWFLW+A3(J)*DTOT*TA
 270  AVLVOL    = DWT1*(POR(J)-TH1(J))+((ETD+ETU)*PERAR+.5*GWFLOW(J) +
     +            0.5*GWFLW+DEPPRC)*DELT
      XSINFL    = ENFILT*DELT-AVLVOL/PERAR
      IF(XSINFL.LT.0.0) THEN
                        GWFLW  = (GWFLOW(J)+GWFLW)/2.
                        AVLVOL = AVLVOL-(GWFLOW(J)-GWFLW)*DELT
                        GO TO 125
                        ENDIF
 280  STG(J)    = GRELEV(J)
      TH1(J)    = POR(J)
      XTRENF(J) = 0.0
      GWFLOW(J) = GWFLOW(J)*WAREA(J)
      DEPPRC    = (DEPPRC+DP(J))/2
      RETURN
C=======================================================================
C **** DETERMINE END-OF-TIME-STEP DEPTH FOR CONDITION WHERE ****
C **** SOIL IS VERY CLOSE TO SATURATED                      ****
C=======================================================================
 290  GWFLOW(J) = GWFLW
      D2        = DTOT-ZEL
      TH2       = 0.9*POR(J)
C=======================================================================
C **** DETERMINE GROUNDWATER FLOW, EXCESS INFILTRATION, AND RESET ****
C **** INITIAL CONDITIONS FOR VERY CLOSE TO SATURATED CONDITIONS. ****
C=======================================================================
 320  TH1(J)    = TH2
C#### CORRECTION BY RED, 9/93.  CHECK FOR BAD SOLUTIONS.
      IF(BELEV(J)+D2.LT.0.0) D2 = STG(J)
      STG(J)    = BELEV(J)+D2
      GWFLOW(J) = GWFLOW(J)*WAREA(J)
      XSINFL    = ENFILT*DELT+(.0001-AVLVOL)/PERAR
      XTRENF(J) = 0.0
      IF(XSINFL.LE.ENFIL*DELT) RETURN
      XTRENF(J) = XSINFL/DELT-ENFIL
      XSINFL    = ENFIL*DELT
      RETURN
C=======================================================================
C **** CALCULATE GROUNDWATER FLOW AND RESET INITIAL CONDITIONS FOR ****
C **** UNSATURATED CONDITIONS.                                     ****
C=======================================================================
 330  GWFLOW(J) = GWFLOW(J)*WAREA(J)
      TH1(J)    = TH2
C#### CORRECTION BY RED, 9/93.  CHECK FOR BAD SOLUTIONS.
      IF(BELEV(J)+D2.LT.0.0) D2 = STG(J)
      STG(J)    = BELEV(J) + D2
      DEPPRC    = (DEPPRC  + D2/DTOT*DP(J))/2.0
      XTRENF(J) = 0.0
C=======================================================================
 1000 FORMAT(' NO CONVERGENCE IN GROUNDWATER SUBROUTINE FOR',
     .       ' SUBCATCHMENT ',I10,' AT TIME =',1PE10.3)
 1001 FORMAT(' NO CONVERGENCE IN GROUNDWATER SUBROUTINE FOR',
     .       ' SUBCATCHMENT ',A10,' AT TIME =',1PE10.3)
C=======================================================================
      RETURN
      END

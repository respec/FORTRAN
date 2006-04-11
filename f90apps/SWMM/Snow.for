      SUBROUTINE SNOW(TA,SMIMED,J,K,II,RIN,RINE,ASC,WINDY,KOMPUT)
C     RUNOFF BLOCK
C     CALLED BY WSHED NEAR LINE 254, 266, 278, 290
C=======================================================================
C     This subroutine adds/deletes water from snow pack, performs
C       redistribution of snow, calls AREAL and MELT, and routes 
C       liquid water through snow pack.
C     Updated 4/7/94 by WCH to indicate if snow is present in 
C       catchment.  Put new variable KWIKSN in SUBCAT.INC.  
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
C=======================================================================
C     Add new snow to snow pack.
C=======================================================================
      IF(RIN.LT.0.0) WSNOW(II,J) = WSNOW(II,J)-RIN*DELT
      IF(WSNOW(II,J).LE.0.0) RETURN
C#######################################################################
C#### WCH, 4/7/94.  Here, indicate presence of snow in order to set
C       WET time step in Sub. HYDRO.
C=======================================================================
      KWIKSN = 1
C
      IF(K.EQ.2.OR.K.EQ.4) GO TO 110
      IF(ISNOW.EQ.2) GO TO 105
      IF(RIN.GE.0.0) RETURN
      IF(RIN.LT.0.0) GO TO 115
C=======================================================================
C     IMPERVIOUS AREA WITH DEPRESSION STORAGE (K=1).
C     PERFORM SNOW CALCS ONLY FOR ISNOW=2 (CONTINUOUS MELT).
C     NORMALLY WITH NO SNOW BUT MAY HAVE UP TO WEPLOW FT WATER EQUIV.
C
C     CHECK FOR REDISTRIBUTION OF SNOW (PLOWING).
C=======================================================================
  105 IF(WSNOW(3,J).LT.WEPLOW(J)) GO TO 110
      EXC = WSNOW(3,J)-WEPLOW(J)
C=======================================================================
C     MOVE EXCESS SNOW TO:
C        OTHER IMPERVIOUS IN SUBCATCHMENT,
C        (FRACTIONS HAVE BEEN MULTIPLIED BY (WAR(1,J)+WAR(3,J))/WAR(KX,JX)
C        EARLIER.)
C=======================================================================
      WSNOW(1,J) = WSNOW(1,J)+SFRAC(1,J)*EXC
C=======================================================================
C     PERVIOUS IN SUBCATCHMENT,
C=======================================================================
      WSNOW(2,J) = WSNOW(2,J)+SFRAC(2,J)*EXC
C=======================================================================
C     PERVIOUS IN LAST SUBCATCHMENT,
C=======================================================================
      WSNOW(2,NOW) = WSNOW(2,NOW)+SFRAC(3,J)*EXC
C=======================================================================
C     OUT OF SYSTEM, (KEEP TALLY ON THIS QUANTITY),
C=======================================================================
      CNT(9) = CNT(9) + SFRAC(4,J)*EXC
C=======================================================================
C        CONVERT TO IMMEDIATE MELT.
C=======================================================================
      SMIMED     = SFRAC(5,J)*EXC/DELT
      WSNOW(3,J) = WEPLOW(J)
C=======================================================================
C     SNOW MELT CALCULATIONS FOR ALL AREAS.
C=======================================================================
  110 SMELT = 0.0
      RI    = RINE
C=======================================================================
C     CEASE SNOW CALCS WHEN AMT LT 0.001 IN. (0.00008 FT).
C=======================================================================
      IF(WSNOW(II,J).GT.0.00008) GO TO 120
  115 SMIMED      = SMIMED + (WSNOW(II,J) + FW(II,J))/DELT
      WSNOW(II,J) = 0.0
      FW(II,J)    = 0.0
      IF(ISNOW.EQ.2) COLDC(II,J) = 0.0
      ASC = 0.0
      RETURN
C=======================================================================
C     DETERMINE FRACTION OF AREA THAT IS SNOW COVERED = ASC.
C=======================================================================
  120                           ASC = 1.0
      IF(ISNOW.EQ.1.AND.K.EQ.2) ASC = SNCP(J)
      IF(ASC.LE.0.0) RETURN
C
      IF(ISNOW.NE.2) GO TO 130
      IF(K.EQ.2) CALL AREAL(WSNOW(II,J),SI(II,J),NEWSNO(II,J),ASC,
     1                  AWE(II,J),SBA(II,J),SBWS(II,J),RIN,ADCP,DELT)
C
      IF(K.EQ.4) CALL AREAL(WSNOW(II,J),SI(II,J),NEWSNO(II,J),ASC,
     1                  AWE(II,J),SBA(II,J),SBWS(II,J),RIN,ADCI,DELT)
C
C     DETERMINE MELT RATE (FT/SEC) = SMELT.
C     MELT RETURNS SMELT, IF MELT OCCURS, OR ELSE ADDS TO COLD CONTENT.
C
                     DHM = DH(II,J)
  130 IF(ISNOW.EQ.1) DHM = DHMAX(II,J)
      IF(WSNOW(II,J).GT.0.0) CALL MELT(DHM,TBASE(II,J),RIN,TA,DELT,
     1SMELT,COLDC(II,J),ATI(II,J),WINDY,ASC,WSNOW(II,J),KOMPUT)
C=======================================================================
      IF(SMELT.LE.0.0) RETURN
C=======================================================================
C     ROUTE MELT THROUGH SNOW PACK.
C=======================================================================
                            RI = SMELT*DELT*ASC
      IF(RI.GT.WSNOW(II,J)) RI = WSNOW(II,J)
      WSNOW(II,J) = WSNOW(II,J) - RI
      FW(II,J)    = FW(II,J) + RI + RINE*DELT
      RI          = FW(II,J) - FWFRAC(II)*WSNOW(II,J)
      IF(RI.LT.0.0) RI = 0.0
      FW(II,J)         = FW(II,J) - RI
      RI               = RI / DELT
      RETURN
      END

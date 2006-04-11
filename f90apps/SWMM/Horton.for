      SUBROUTINE HORTON(TP,K,J)
C     RUNOFF BLOCK
C     CALLED BY WSHED NEAR LINE 297
C=======================================================================
C     THIS SUBROUTINE CREATED FROM WSHED FOR SWMM 4.0
C     NEGATIVE SIGN CORRECTION, WCH, 8/93.
C     CHANGE EVAP TO SVAP (4 LOCATIONS) FOR NO EVAPORATION DURING 
C       RAIN, WCH (CDM), 10/5/93.
C     CHECK FOR TP1 < TP DURING NEWTON-RAPHSON ITERATION, WCH, 11/15/93.
C     WCH, 10/14/94.  CHECK FOR TOO LARGE DEXP ARGUMENT.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CIM   note, changes for maximum infiltration volume and it's recovery
cim   in horton and wshed are commented using c%%%%  1/96
      INCLUDE 'MAXINF.INC'      
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      DOUBLE PRECISION TP1,TP2,TP3,TP4
      DATA NUMERR/0/
C=======================================================================
C     DEFINE STATEMENT FUNCTION FOR CUMULATIVE HORTON INFILTRATION.
C=======================================================================
      CUMINF(TT) = WLN*TT + (WLX-WLN)/DCY*(1.0-EXP(-DCY*TT))
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     IF WATER TABLE HAS REACHED SURFACE (I.E., IF TOTINF(J)=
C     RMAXINF(J)), SET TP=TLIM=16.0/DECAY, RLOSS=SVAP, AND RETURN
      IF(INFILM.EQ.2) THEN
        IF(TOTINF(J).GE.RMAXINF(J)) THEN
          TP=16.0/DECAY(J)
          RLOSS=SVAP
          RETURN
        ENDIF
      ENDIF
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C=======================================================================
C     REGENERATE INFILTRATION CAPACITY DURING DRY TIME STEPS.
C     MAX INFIL CAP (WLMAX) CORRESPONDS TO TP=0.
C     REGENERATE AT RATE = REGEN*DECAY.
C     INCREMENT TIME ALONG REGENERATION CURVE, THEN FIND EQUIV NEW TP.
C     EQUATIONS BELOW COMBINE ALL STEPS.
C=======================================================================
      IF(RI.LE.0.0.AND.WDEPTH(K,J).LE.0.0) THEN
              IF(TP.LE.0.0) RETURN
              EEE = DECAY(J)*TP
              IF(EEE.GE.60.0) EEE = 60.0
              EEE = 1.0 - EXP(-EEE)
              TP1 = -REGEN*DECAY(J)*DELT
C#### WCH, 10/14/94.  CHECK FOR TOO LARGE DEXP ARGUMENT.
              TP2 = 0.0
              IF(-TP1.LT.50) TP2 = DEXP(TP1)
              TP3 = 1.0-EEE*TP2
              TP4 = DLOG(TP3)
C#### WCH, 8/93.  ADD NEGATIVE SIGN BELOW.
              TP  = -TP4/DECAY(J)
C              TP  = -DLOG(1.0-EEE*DEXP(-REGEN*DECAY(J)*DELT))/DECAY(J)
              IF(TP.LE.0.1*DELT) TP = 0.0
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C RLOSS MUST BE SET TO SVAP IF WDEPTH IS EVER TO BE ALLOWED TO
C REGENERATE BELOW WSTORE.  ADDED LINE BELOW
              RLOSS=SVAP
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%              
              RETURN
              END IF
      FAVAIL = RI + WDEPTH(K,J)/DELT - SVAP
C=======================================================================
C     NO WATER AVAILABLE FOR INFILTRATION.
C=======================================================================
      IF(FAVAIL.LE.0.0) THEN
                        RLOSS = SVAP
                        RETURN
                        ENDIF
C=======================================================================
C     ASSUME FLAT INFILTRATION CURVE FOR T>16/DECAY.
C=======================================================================
      TLIM = 16.0/DECAY(J)
      DCY  = DECAY(J)
      WLN  = WLMIN(J)
      WLX  = WLMAX(J)
C=======================================================================
C     TP IS TIME ON HORTON CURVE THAT CORRESPONDS TO CUMULATIVE INFIL.
C     HERE IF STILL ON EXPONENTIAL CURVE.
C     USE DIFFERENCE OF CUMULATIVE INFIL TO FIND AVE INFIL IN ORDER TO
C         BE ACCURATE OVER LARGE TIME STEPS.
C=======================================================================
      TP1 = TP
      IF(TP+DELT.GE.TLIM) THEN
                          RLOSS1 = WLMIN(J)
                          TP     = TLIM
                          ELSE
                          TPDEL  = TP + DELT
                          RLOSS1 = (CUMINF(TPDEL) - CUMINF(TP))/DELT
                          END IF
C=======================================================================
C     CHECK TO SEE IF WATER AVAILABLE FOR INFIL EXCEEDS INFIL CAPACITY.
C     HERE IF ALL INFILTRATION CAPACITY IS USED.
C=======================================================================
      IF(FAVAIL.GE.RLOSS1) THEN
                           RLOSS = RLOSS1 + SVAP
                           TP    = TP     + DELT
                           IF(TP.GT.TLIM) TP = TLIM
                           RETURN
                           ENDIF
C=======================================================================
C     HERE, MUST ITERATE TO FIND TIME TP TO MATCH CUMULATIVE INFIL,CUMI.
C     NO COMPUTATION IF OPERATING ON FLAT PART OF CURVE.
C=======================================================================
      RLOSS = FAVAIL + SVAP
      IF(TP+DELT.GE.TLIM) THEN
                          TP = TLIM
                          RETURN
                          ENDIF
C=======================================================================
C     FIND CUMI FROM INTEGRATED HORTON'S EQUATION.
C=======================================================================
      CUMI = CUMINF(TP)
      CUMI = CUMI + FAVAIL*DELT
      TP1  = TP   + DELT/2.0
C=======================================================================
C     NEWTON-RAPHSON ITERATION TO FIND NEW TP.
C=======================================================================
      WEE      = WLMAX(J) - WLMIN(J)
      DO 200 I = 1,11
C#### WCH, 11/15/93.  SEE IF THIS HELPS CONVERGENCE.
      IF(TP1.LT.TP) TP1 = TP
      BEE      = TP1*DECAY(J)
      IF(BEE.GE.60) BEE = 60.0
      EEE      = EXP(-BEE)
      DFF      = WLMIN(J)     + WEE*EEE
      FF       = WLMIN(J)*TP1 + WEE/DECAY(J)*(1.0-EEE) - CUMI
      DELF     = FF/DFF
      TP1      = TP1 - DELF
      IF(ABS(DELF).GT.0.001*DELT)      GO TO 200
      IF(TP1.LT.TP+DELT.AND.TP1.GT.TP) THEN
                                       TP = TP1
                                       IF(TP.GT.TLIM) TP = TLIM
                                       RETURN
                                       ENDIF
  200 CONTINUE
C=======================================================================
C     NO CONVERGENCE. AS DEFAULT, USE TP=TP+0.9*DELT.
C=======================================================================
      NUMERR = NUMERR + 1
      IF(NUMERR.LE.25) THEN
                       WRITE(N6,210)
                       WRITE(N6,220) J,TP,TP1,FAVAIL,TIME/3600.0,DELT
                       ENDIF
                     TP = TP + 0.9*DELT
      IF(TP.GT.TLIM) TP = TLIM
C=======================================================================
  210 FORMAT(
     +' *******************************************************',/,
     +' * ==> WARNING. NO CONVERGENCE OF HORTON INFILTRATION. *',/,
     +' *******************************************************')
  220 FORMAT(' SUBCAT/INPUT # ',I9,' TP(SECONDS) = ',F9.1,
     +       ' TP1(SECONDS) = ',F9.1,/,' FAVAIL  = ',1PE14.7,
     +       ' TIME(HOURS) = ',0PF9.1,' DELT(SECONDS) = ',F9.1,/)
C=======================================================================
      RETURN
      END

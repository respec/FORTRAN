      SUBROUTINE GQUAL(J,AREA0,AREA1,FLOW0,VSURO,QINT)
C=======================================================================
C     GQUAL was updated by W.HUBER and R.DICKINSON March, 1988.
C     GQUAL was last updated December, 1990.
C     Updated 7/28/93 by WCH to correct calculation for average
C       inflow flux during surcharging.
C=======================================================================
C     This Subroutine routes quality in channel J for the flow values
C     computed in subroutine GUTTER.  NQS quality C's are calculated.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
      INCLUDE 'GRWTR.INC'
      INCLUDE 'NEW88.INC'
      DIMENSION FLUX(10),FLUXAV(10),FLUX0(NRQ,NG)
      EQUIVALENCE (FLUX0,CDOT)
C=======================================================================
C     Check flow and set zeros if below minimum.
C=======================================================================
      IF(OUTFLW(J).LE.1.0E-9) THEN
                              DO 10 K   = 1,NQS
                              C(K,J)    = 0.0
                              FLUX0(K,J)= 0.0
   10                         CONTINUE
                              RETURN
                              ELSE
                              DO 15 K = 1,NQS
                              FLUX(K) = 0.0
   15                         CONTINUE
                              ENDIF
C=======================================================================
C     Compute inputs from groundwater.
C=======================================================================
      IF(NOGWSC.GT.0) THEN
                DO 20 IA = 1,NOGWSC
                IF(JCE.EQ.0.AND.NGWTOG(IA).NE.NAMEG(J)) GO TO 20
                IF(JCE.EQ.1.AND.KGWTOG(IA).NE.KAMEG(J)) GO TO 20
                DO 22 K   = 1,NQS
                GWQ(K)    = GWQ(K)  + GWFLOW(IA)*CGWQ(K)*DELT
 22             FLUX(K)   = FLUX(K) + GWFLOW(IA)*CGWQ(K)
 20             CONTINUE
                ENDIF
C=======================================================================
C     Compute inputs from upstream channels.
C     Inputs are instantaneous at the end of the time step.
C=======================================================================
      QACT    = QIN(J)
      DO 50 K = 1,NCP
      L       = NGTOG(K,J)
      IF(L.EQ.0) GO TO 60
      DO 40 M = 1,NQS
      FLUX(M) = FLUX(M) + C(M,L) * OUTFLW(L)
   40 CONTINUE
   50 CONTINUE
   60 CONTINUE
C=======================================================================
C-.....Add mass input from adjacent watersheds..POFF in mg/sec.....
C-.....POFF is the instantaneous value at the end of the time step.
C=======================================================================
      DO 80 K  = 1,NCP
      L        = NWTOG(K,J)
      IF(L .EQ.0) GO TO 90
      DO 70 M  = 1,NQS
      ND       = NDIM(M) + 1
      DO 70 JJ = 1,N1
      IF(N1.EQ.1) I = KLAND(L)
      IF(N1.GT.1) I = JJ
      FLUX(M)  = FLUX(M) + POFF(I,M,L)/FACT3(ND)
   70 CONTINUE
   80 CONTINUE
   90 CONTINUE
C=======================================================================
C-    OUTFLW is the instantaneous flow at the end of a time step.
C-
C-    If NPG(J) is a dummy channel calculate concentration and return.
C=======================================================================
      IF(NPG(J).EQ.3) THEN
                      DO 100 M = 1,NQS
  100                 C(M,J)   = FLUX(M) / OUTFLW(J)
                      RETURN
                      ENDIF
C=======================================================================
C-.....Account for surcharge contribution
C
C     Compute the average inflow concentration.
C     Here, FLUXAV now means average inflow concentration, not flux.
C=======================================================================
      IF(QSUR(J).LE.0.0.AND.VSURO.LE.0.0) THEN
             DO 135 M  = 1,NQS
             FLUXAV(M) = 0.0
             IF(QACT.GT.0.0) FLUXAV(M) = 0.5*(FLUX0(M,J)+FLUX(M))/QACT
  135        CONTINUE
             ELSE
C=======================================================================
C#### Calculations with surcharge altered by WCH, 7/28/93.
C     VSURO   --> Previous time step surcharge storage.
C     QSUR(J) --> Current time step surcharge storage.
C     QADD    --> Change in surcharge storage (flow rate).
C                 QADD > 0 ==> Draining from stored surcharge.
C                 QADD < 0 ==> Adding to stored surcharge.
C     QIN(J)  --> The average total potential inflow.
C     QACT    --> The average actual inflow (surcharge volume 
C                 subtracted or added).
C-======================================================================
             QADD = (VSURO - QSUR(J)) / DELT
             QACT = QIN(J) + QADD
C=======================================================================
C     Compute total potential flux in.
C     FLUXIN --> Average potential flux in without surcharge.
C=======================================================================
             DO 120 M = 1,NQS
             FLUXIN   = (FLUX0(M,J)+FLUX(M))/2.0
C=======================================================================
C    Case 1: Surcharge volume (and load) is draining (diminishing).
C    Add load to average potential flux.
C    Stored surcharge load has no decay or loss.  Use old concentration
C      (CVSUR/VSURO) over the time step.
C=======================================================================
             IF(QADD.GE.0.0) THEN
                  SRFLUX     = QADD*CVSUR(M,J)/VSURO
                  FLUXAV(M)  = FLUXIN + SRFLUX
                  CVSUR(M,J) = CVSUR(M,J) - SRFLUX * DELT
                  ENDIF             
C=======================================================================
C     Case 2: Adding to surcharge volume (and load).
C     Subtract load stored in surcharge from average potential flux.
C     Add to load stored in surcharge.
C     Remember, QADD will be negative here.
C=======================================================================
            IF(QADD.LT.0) THEN
                 SRFLUX     = - QADD * FLUXIN/QIN(J)
                 FLUXAV(M)  = FLUXIN - SRFLUX
                 CVSUR(M,J) = CVSUR(M,J) + SRFLUX * DELT
                 ENDIF
C=======================================================================
C     Convert to average inflow concentration over the time step.
C=======================================================================
             FLUXAV(M)  = FLUXAV(M)/QACT
  120        IF(CVSUR(M,J).LT.0.0) CVSUR(M,J) = 0.0
             ENDIF
C=======================================================================
C     Compute current values of channel parameters for routing.
C=======================================================================
      V0   = GLEN(J) * AREA0
      V1   = GLEN(J) * AREA1
      TWOV = V0 + V1
      ARG  = 0.0
      IF(V0.GT.0.0) ARG = FLOW0/V0
      IF(V1.GT.0.0) ARG = ARG + OUTFLW(J)/V1
      ARG               = ARG*DELT/2.0
C=======================================================================
C     Allow for case in which conduit is only draining, with no inflow.
C#### WCH, 7/28/93.  NEED FACTOR OF 2 FOR DV/V CALC TO GET AVG. VOL.
C=======================================================================
      IF(TWOV.GT.0.0)  ARG = ARG + 2.0 * (V1-V0)/TWOV
      IF(ARG.LT.0.0)   ARG = 0.0
                      EXXP = 0.0
      IF(ARG.LT.10.0) EXXP = EXP(-ARG)
C=======================================================================
C     If there is no initial volume, conc. must equal inflow concen.
C=======================================================================
      IF(V0.LT.1.0E-5) EXXP = 0.0
C=======================================================================
C     Compute final parameters.
C     Routing assumes complete mixing within channel/pipe.
C     FLUX0  = Array that stores the computed flux for the next step.
C     C(M,J) = Concentration at the beginning of the time step.
C=======================================================================
      IF(QINT.LE.0.0.AND.EXXP.EQ.0.0) THEN
                                      DO 170 M   = 1,NQS
  170                                 FLUX0(M,J) = FLUX(M)
                                      RETURN
                                      ENDIF
      DO 150 M = 1,NQS
      WCH      = C(M,J)
C=======================================================================
C     If EXXP = 0, use instantaneous inflow concentration.
C=======================================================================
      IF(EXXP.EQ.0.0) C(M,J) = FLUX(M)/QINT
C=======================================================================
C     THE FOLLOWING EQUATION USES AVERAGE QUANTITIES OVER THE
C     TIME STEP TO ELIMINATE STABILITY PROBLEMS.
C
C     THE EQUATION INTEGRATED IS :
C
C     VAVG * DC/DT = QAVG(CIN-C) - C * DV/DT - K * C * V + L
C
C     K AND L ARE DROPPED FROM THE FINAL FORM SINCE THEY
C     ARE ZERO IN THIS EXAMPLE.
C
C     Average CONCENTRATION entering channel/pipe = FLUXAV(M).
C=======================================================================
      IF(EXXP.GT.0.0) C(M,J) = FLUXAV(M)*(1.0-EXXP) + WCH*EXXP
C=======================================================================
C     Define FLUX0(M,J) for the next time step.
C=======================================================================
  150 FLUX0(M,J) = FLUX(M)
      RETURN
      END

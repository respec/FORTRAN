      SUBROUTINE QSHED
C=======================================================================
C     Enter QSHED for time-step water quality calculations.
C=======================================================================
C     Last updated in December, 1992 by WCH for typo at statement 620.
C     Updated by WCH, 9/3/93 to include quality load from 
C       infiltration/inflow.
C     Updated by WCH (from RED), 9/23/93 to correct subscript order
C       in WSNOW.  
C     Add error check for negative flow and correct GO TO, WCH, 9/29/93.
C     Use DMEAN for multiplication for loads, consistent with summations
C       for flow volumes, WCH, 11/30/93.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
C#### WCH, 9/93
      INCLUDE 'RDII.INC'
      DOUBLE PRECISION ATTEN,POW,DTHR2,RP
C=======================================================================
C     Initialize several parameters.
C     If have erosion, perform buildup-washoff for NQS-1 constituents.
C     NQSS = NQS - 1 ---> This subtraction was performed in QHYDRO.
C=======================================================================
      DTHR2 = DELT/7200.0
      DELT2 = DELT/2.0
C#### WCH, 9/29/93.  INITIALIZE ERROR PRINT COUNTER.
      IF(MTIME.EQ.1) KERR = 0
C=======================================================================
C     Time step calculations for each subcatchment.
C=======================================================================
C     Compute instantaneous runoff rate for total subcatchment (in/hr).
C=======================================================================
      DO 1000 N     = 1,NOW
      DO 1000 JJ    = 1,N1
      IF(N1.EQ.1) J = KLAND(N)
      IF(N1.GT.1) J = JJ
      LAH           = 0
      IF(WAREA(N).NE.0.0) THEN
           RUN2 = WFLOW(N)/WAREA(N)*43200.0
           ELSE 
           RUN2 = 0.0
           ENDIF
C=======================================================================
C     If runoff rate is small, perform buildup.
C=======================================================================
      IF(RUN2.LT.0.000005) GO TO 510
C=======================================================================
C     Perform buildup if this is first 'WET' timestep.
C=======================================================================
      IF(TBUILD(J,N).LE.0.0) GO TO 600
      LAH = 1
      GO TO 530
C=======================================================================
C     TBUILD = time since last runoff or sweeping.
C=======================================================================
  510 TBUILD(J,N) = TBUILD(J,N) + DELT
C=======================================================================
C     Here, check for sweeping, etc.
C     JOULE = Julian day of year.
C     No sweeping if wrong dates or if snow is present on imperv area.
C=======================================================================
      JOULE = JULDAY - NYEAR*1000
      IF(JOULE-KLNBGN.LT.0)           GO TO 560
      IF(JOULE-KLNBGN.EQ.0)           GO TO 530
      IF(JOULE-KLNBGN.GT.0) THEN
               IF(JOULE.GE.KLNEND)    GO TO 560
C#### WCH (RED), 9/93. TRANSPOSE SUBSCRIPTS IN WSNOW.
               IF(WSNOW(3,N).GT.0.0)  GO TO 560
C=======================================================================
C     Increment street sweeping time and check to see if should sweep.
C=======================================================================
               TCLEAN(J,N) = TCLEAN(J,N) + DELT
               IF(TCLEAN(J,N).LT.CLFREQ(J)) GO TO 560
               ENDIF
C=======================================================================
C     Time to sweep.  First perform buildup for each constituent.
C     Buildup for some constituents only if snow is present.
C=======================================================================
  530 DO 550 K = 1,NQSS
      IF(KALC(J,K).EQ.4) GO TO 550
      IF(KALC(J,K).EQ.0) KC = METHOD(J)
      IF(KALC(J,K).GT.0) KC = KALC(J,K) - 1
      OPS      = PSHED(J,K,N)
      IF(LINKUP(J,K).EQ.0) GO TO 540
C#### WCH (RED), 9/93.  TRANSPOSE SUBSCRIPTS IN WSNOW.
      IF(WSNOW(1,N).LE.0.0.AND.WSNOW(3,N).LE.0.0) GO TO 550
  540 CALL BUILD(N,K,1,KC,CULIM(J,K,N),CPOW(J,K,N),CCOEF(J,K,N),
     +                TBUILD(J,N),PSHED(J,K,N))
      SUM(K,2) = SUM(K,2) + (PSHED(J,K,N) - OPS)
C=======================================================================
C     Regenerate catchbasin loading.
C=======================================================================
      IF(BASINS(N).GT.0.0) THEN
                           OPB = PBASIN(K,N)
                           PBASIN(K,N) = PBASIN(K,N) + TBUILD(J,N)
     +                                    * (PPBASN(K,N)/DRYBSN)
                           IF(PBASIN(K,N).GT.PPBASN(K,N))
     +                                       PBASIN(K,N) = PPBASN(K,N)
                           SUM(K,4) = SUM(K,4) + (PBASIN(K,N) - OPB)
                           ENDIF
  545 CONTINUE
  550 CONTINUE
      TBUILD(J,N) = 0.0
C=======================================================================
C     Check to see if only needed to buildup in order to
C     wash off, not to sweep.
C=======================================================================
      IF(LAH.EQ.1) GO TO 600
C=======================================================================
C     Now, finally sweep streets.
C=======================================================================
      DO 555 K     = 1,NQSS
      REMOVE       = AVSWP(J)     * REFF(J,K) * PSHED(J,K,N)
      SUM(K,5)     = SUM(K,5)     + REMOVE
  555 PSHED(J,K,N) = PSHED(J,K,N) - REMOVE
      TCLEAN(J,N)  = 0.0
C=======================================================================
C     No washoff during dry weather.
C=======================================================================
  560 DO 570 K    = 1,NQS
  570 POFF(J,K,N) = 0.0
      GO TO 1000
C=======================================================================
C     PERFORM WASHOFF CALCULATIONS.  ULTIMATELY, CALC LOAD RATE
C        (E.G.,MG/SEC) OF EACH CONSTITUENT WASHED OFF.
C     Total of three options:
C        KWASH = 0, modified exponential washoff.
C              = 1, rating curve, no limit.
C              = 2, rating curve with limit from buildup equation.
C=======================================================================
  600 IF(RUN2.LT.0.000005) GO TO 560
      DO 800 K    = 1,NQSS
      TEMPLD(J,K) = 0.0
      IF(KWASH(J,K).GT.0) GO TO 620
C=======================================================================
C     Here, modified exponential washoff.
C=======================================================================
                             RP = RUN2
      IF(WASHPO(J,K).NE.1.0) RP = RUN2**WASHPO(J,K)
C=======================================================================
C     Use trapezoidal rule for incremental integration within exponent.
C=======================================================================
      POW          = RCOEF(J,K)*DTHR2*(RP+OLDQP(J,K,N))
      OLDQP(J,K,N) = RP
C=======================================================================
C     Compute modified exponential decay.
C=======================================================================
                     ATTEN = 0.0
      IF(POW.LT.35.) ATTEN = DEXP(-POW)
C=======================================================================
C     Calc washoff load rate (E.G., mg/sec, mpn/sec).
C=======================================================================
      TEMPLD(J,K) = RCOEFX(J,K)*RP*PSHED(J,K,N)*ATTEN
C=======================================================================
C     Decrease constituent surface load (E.G, mg, mpn)
C=======================================================================
      DPSHED = TEMPLD(J,K) * DELT
C=======================================================================
C     Here, have attempted to remove more than PSHED.
C=======================================================================
      IF(DPSHED.GT.PSHED(J,K,N))  THEN
                                  TEMPLD(J,K)  = PSHED(J,K,N)/DELT
                                  DPSHED       = PSHED(J,K,N)
                                  ENDIF
                                  PSHED(J,K,N) = PSHED(J,K,N) - DPSHED
      IF(PSHED(J,K,N).LT.1.0E-25) PSHED(J,K,N) = 0.0
      GO TO 700
C=======================================================================
C     Here, compute washoff load rate (E.G., mg/sec, mpn/sec)
C     using a rating curve with no upper limit, or a rating
C     curve with upper limit given by buildup equation.
C=======================================================================
C####################################################################### 
C     WCH, 9/29/93.  ERROR CHECK.
C#######################################################################
  620 SCFLOW  =  PLAND(J,N)*WFLOW(N)
      IF(SCFLOW.LT.0.0) THEN
           IF(KERR.LE.200) THEN
                WRITE(N6,9000) TIME,J,K,N,PLAND(J,N),WFLOW(N),SCFLOW
                KERR = KERR + 1
                ELSE
                IF(KERR.EQ.201) WRITE(N6,9001)
                KERR = KERR + 1
                ENDIF
           ENDIF
C#######################################################################
C#### WCH, 12/3/92.  Change to "EQ.1"
      IF(KWASH(J,K).EQ.1) THEN
C####                  SCFLOW      = PLAND(J,N)*WFLOW(N)
                    TEMPLD(J,K) = RCOEF(J,K)*SCFLOW**WASHPO(J,K)
                    ELSE
C=======================================================================
C     Here if KWASH = 2
C=======================================================================
C#### WCH, 9/29/93.  SHOULD BE GO TO 700, NOT 800.
                    IF(PSHED(J,K,N).LE.0.0) GO TO 700
C####                    SCFLOW      = PLAND(J,N)*WFLOW(N)
                    TEMPLD(J,K) = RCOEF(J,K)*SCFLOW**WASHPO(J,K)
                    PP          = (OLDQP(J,K,N) + TEMPLD(J,K))*DELT2
                    IF(PP.GE.PSHED(J,K,N)) THEN
                         TEMPLD(J,K) = PSHED(J,K,N)/DELT2 - OLDQP(J,K,N)
                         IF(TEMPLD(J,K).LT.0.0) TEMPLD(J,K)  = 0.0
                                                PSHED(J,K,N) = 0.0
                         ELSE
                         PSHED(J,K,N) = PSHED(J,K,N) - PP
                         ENDIF
                    OLDQP(J,K,N)      = TEMPLD(J,K)
                    ENDIF
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELT.
  700 SUM(K,6) = SUM(K,6) + TEMPLD(J,K)*DMEAN
C=======================================================================
C     Compute catchbasin contribution.
C=======================================================================
      IF(BASINS(N).GT.0.0) THEN
         PP          = PBASIN(K,N) * PLAND(J,N) * WFLOW(N) / BASINS(N)
         PBASIN(K,N) = PBASIN(K,N) - PP * DELT
         IF(PBASIN(K,N).LT.0.0) THEN
                                PP          = PBASIN(K,N) / DELT + PP
                                PBASIN(K,N) = 0.0
                                ENDIF
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELT.
         SUM(K,7)    = SUM(K,7)    + PP*DMEAN
         TEMPLD(J,K) = TEMPLD(J,K) + PP
         ENDIF
  800 CONTINUE
C=======================================================================
C     Simulate erosion by the universal soil loss equation.
C     Erosion index by method of Wischemier and Smith.
C=======================================================================
      IF(IROS.EQ.1) THEN
                    TEMPLD(J,NQS) = 0.0
                    IF(CNSTNT(N).LE.0.0) GO TO 900
                    RNINHR = RINE*43200.0
                    IF(RNINHR.LT.0.01) GO TO 900
                    Y = (9.16+3.31*ALOG10(RNINHR))*RNINHR/3600.0
                    TEMPLD(J,NQS) = Y*RAINIT*CNSTNT(N)
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELT.
                    SUM(NQS,6)    = SUM(NQS,6) + TEMPLD(J,NQS)*DMEAN
                    IF(IROSAD.GT.0) TEMPLD(J,IROSAD) = TEMPLD(J,IROSAD)
     +                                               + TEMPLD(J,NQS)
                    ENDIF
C=======================================================================
  900 DO 950 K    = 1,NQS
      POFF(J,K,N) = TEMPLD(J,K)
      ND          = NDIM(K) + 1
C=======================================================================
C     Add fractions from other constituents.
C=======================================================================
      DO 930 L    = 1,NQS
      IF(L.NE.K) THEN
                 PP          = TEMPLD(J,L) * F1(K,L)
                 POFF(J,K,N) = POFF(J,K,N) + PP
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELT.
                 SUM(K,8)    = SUM(K,8)    + PP*DMEAN
                 ENDIF
  930 CONTINUE
C=======================================================================
C     Add precipitation load = Flow*concentration.
C     CONCRN                 = Precip concentration*conversion factor.
C=======================================================================
      PP          = PLAND(J,N)*WFLOW(N)*CONCRN(J,K)*FACT3(ND)
      POFF(J,K,N) = POFF(J,K,N) + PP
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELT.
  940 SUM(K,9)    = SUM(K,9)    + PP*DMEAN
C#######################################################################
C     WCH, 9/93.  Add contribution from infiltration/inflow.  
C     CONCII = constant concentration in I/I.
C=======================================================================
      IF(RRMAX.GT.0.0.AND.JJ.EQ.1) THEN
           IF(FLOWII(N).LE.0.0) GO TO 950
           PP          = FLOWII(N)*CONCII(K)*FACT3(ND)
           POFF(J,K,N) = POFF(J,K,N) + PP           
C#### WCH, 11/30/93.  MULT BY DMEAN, NOT DELT.
           SUMRDII(K)  = SUMRDII(K) + PP*DMEAN
           ENDIF
  950 CONTINUE
C=======================================================================
C     End loop on subcatchments.
C=======================================================================
 1000 CONTINUE
      RETURN
C=======================================================================
C#### WCH, 9/29/93.
 9000 FORMAT(' $$$$$ WARNING!! SERIOUS ERROR FROM QSHED. SHOULD NOT HAVE
     1 PLAND OR WFLOW < 0. ON-SCREEN ERROR 1230 LIKELY. RUN CONTINUES.',
     2/, ' TIME=',F7.1,' SEC. J,K,N =',3I4,' PLAND(J,N),WFLOW(N),SCFLOW 
     3=',3E14.6)
 9001 FORMAT (' $$$$$ LIMIT OF 200 NEGATIVE FLOW ERROR MESSAGES FROM QSH
     1ED REACHED.  NO MORE PRINTED.')
      END

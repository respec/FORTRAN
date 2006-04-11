      SUBROUTINE QINT
C	RUNOFF BLOCK
C	CALLED BY SUBROUTINE QHYDRO NEAR LINE 727
C=======================================================================
C     Enter QINT for initial calculations.
C     Modified by WCH, 4/12/93 to fix hectare conversion.
C     Modified by WCH, 7/29/93 to maintain separate buildup parameters
C       for each land use fraction.  This means increasing dimensions
C       of arrays CCOEF, CPOW and CULIM all to (NLU,MQUAL,NW).
C     Fix inversion of QFACT subscripts affecting KALC=0 buildup and
C       ensure catchbasin buildup is function of land use fractions,
C       WCH, 10/1/93 and 10/8/93.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
C=======================================================================
C     Initialize several parameters.
C     If have erosion, perform buildup-washoff for NQS-1 constituents.
C     NQSS = NQS - 1 ---> This subtraction was performed in QHYDRO.
C=======================================================================
      NSUM    = 10
C=======================================================================
C     Determine if any constituents use dust and dirt (DD).
C=======================================================================
      NOTDD   = 0
      DD      = 0.0
      DO 50 K = 1,NQSS
      DO 45 J = 1,N1
C=======================================================================
C     If KALC(J,K) = 0 Buildup is a fraction of dust and dirt / land use
C=======================================================================
      IF(KALC(J,K).LE.0) THEN
                         NOTDD = 1
                         GO TO 55
                         ENDIF
   45 CONTINUE
   50 CONTINUE
C=======================================================================
C     Initialize all quality summations.
C=======================================================================
   55 DO 60 K  = 1,NQS
      DO 60 L  = 1,NSUM
   60 SUM(K,L) = 0.0
C=======================================================================
C     Establish initial loads.  Perform mini-simulaiton for
C     DRYDAY days, assumming zero initial loads.
C=======================================================================
      DO 300 N      = 1,NOW
      DO 300 JJ     = 1,N1
      IF(N1.EQ.1) J = KLAND(N)
      IF(N1.GT.1) J = JJ
C=======================================================================
C     Check to see if load/acre or load/hectare has been used.
C=======================================================================
      DO 210 K = 1,NQSS
      KEY(J,K) = 0
      IF(PSHED(J,K,N).LE.0.0) GO TO 210
      ND       = NDIM(K) + 1
C=======================================================================
C     IF METRIC = 1 Multiply PSHED by the area in acres and convert
C                   from units of NDIM to units of mg.
C=======================================================================
      IF(METRIC.EQ.1) PSHED(J,K,N) = PSHED(J,K,N) * WAREA(N)/43560.0 *
     1                               FACT1(ND)    * PLAND(J,N)
C=======================================================================
C#### WCH, 4/12/93. FIX CONVERSION FACTOR.  DIVIDE (NOT MULT) BY 2.471.
C     IF METRIC = 2 Divide by the conversion factor 2.471 to change
C                   load/hectare to load/acre.  Convert load in units
C                   of NDIM - METRIC to mg.  Multiply by the area in
C                   acres.  WAREA was read in as hectares and converted
C                           to units of square feet.
C
C     If the load is in units of kilograms ( NDIM = 0 ) apply the
C              conversion factor 1.0E06 to change the units to mg.
C     If the load in units of concentration or other units
C              there is no conversion factor.
C=======================================================================
                              CONV = 1.0
      IF(NDIM(K).EQ.0)        CONV = 1.0E06
C#### WCH, 4/12/93.  DIVIDE (NOT MULT) BY 2.471.
      IF(METRIC.EQ.2) PSHED(J,K,N) = PSHED(J,K,N) / 2.471 * CONV *
     1                               WAREA(N) / 43560.0   * PLAND(J,N)
      KEY(J,K) = 1
  210 CONTINUE
C=======================================================================
C     Allow dust and dirt parameters to be functions of
C     gutter length or catchment area.
C=======================================================================
                         DM = 1.0
      IF(JACGUT(J).EQ.0) DM = PLAND(J,N)*GQLEN(N)
      IF(JACGUT(J).EQ.1) DM = PLAND(J,N)*WAREA(N)/43560.0
                        DDF = DDFACT(J)*DM
                        DDL = DDLIM(J)*DM
C=======================================================================
C     Note  : Here, operating with time units of days.
C     DSLCL = days since last cleaning.
C     DTS   = day of first cleaning after last rain.
C=======================================================================
      IF(CLFREQ(J).GT.0.0) NCL = DRYDAY / CLFREQ(J)
      IF(CLFREQ(J).EQ.0.0) NCL = DRYDAY
                           DTS = DRYDAY-FLOAT(NCL)*CLFREQ(J)-DSLCL(J)
      IF(DTS.LE.0.0)       DTS = DTS + CLFREQ(J)
      IF(DTS.GT.DRYDAY)    DTS = DRYDAY
C=======================================================================
C     No accumulation if zero buildup time.
C=======================================================================
      IF(DTS.LE.0.0) GO TO 300
C=======================================================================
C     Calculate initial dust and dirt load, DD, in pounds.
C=======================================================================
      IF(NOTDD.EQ.1) CALL BUILD(N,J,0,METHOD(J),DDL,DDPOW(J),DDF,DTS,DD)
C=======================================================================
C     Now calculate initial load for each constituent.
C     Units depend upon NDIM.
C        NDIM = 0, load = mg.
C             = 1, load = total 'QUANTITY', e.g., MPN.
C             = 2, load = volume*concentration, e.g., cu. ft. * JTU.
C=======================================================================
      TTT      = DTS
      INCR     = 0
  230 DO 250 K = 1,NQSS
C=======================================================================
C     If initial loads were inputed on line L1 or no
C        buildup is required skip this buildup section.
C=======================================================================
      IF(KEY(J,K).EQ.1.OR.KALC(J,K).EQ.4) GO TO 250
      ND = NDIM(K) + 1
      IF(KALC(J,K).GT.0) GO TO 240
C=======================================================================
C     Here, load is a fraction of dust and dirt.
C=======================================================================
C#### WCH, 10/1/93.  SUBSCRIPTS J,1,K WERE INVERTED.
      PSHED(J,K,N) = DD*QFACT(J,1,K)*FACT2(ND)
      GO TO 250
C=======================================================================
C     Here, calculate buildup using parameters for each constituent.
C=======================================================================
  240 KC = KALC(J,K) - 1
C=======================================================================
C     Allow upper limit and coefficient to be functions of
C     either gutter length or catchment area.
C=======================================================================
                           DPM = 1.0
      IF(KACGUT(J,K).EQ.0) DPM = PLAND(J,N)*GQLEN(N)
      IF(KACGUT(J,K).EQ.1) DPM = PLAND(J,N)*WAREA(N)/43560.0
                  ULIM = QFACT(J,1,K)*DPM*FACT1(ND)
                  COEF = QFACT(J,3,K)
      IF(KC.EQ.0) COEF = COEF*DPM*FACT1(ND)
      CALL BUILD(N,K,INCR,KC,ULIM,QFACT(J,2,K),COEF,TTT,PSHED(J,K,N))
  250 CONTINUE
C=======================================================================
C     Sweep streets, then simulate for remainder of DRYDAY days.
C=======================================================================
      IF(DTS.GE.DRYDAY) GO TO 300
C=======================================================================
C     Perform street sweeping on DD and constituents.
C=======================================================================
      REMAIN     = 1.0 - AVSWP(J)*REFFDD
      DD         = DD*REMAIN
      DO 270 K   = 1,NQSS
      REMAIN     = 1.0 - AVSWP(J) * REFF(J,K)
      IF(KEY(J,K).EQ.1) GO TO 270
      SUM(K,5)     = SUM(K,5)     + PSHED(J,K,N) - PSHED(J,K,N) * REMAIN
      PSHED(J,K,N) = PSHED(J,K,N) * REMAIN
  270 CONTINUE
C=======================================================================
C     Increment time by cleaning interval.
C=======================================================================
      DTS1 = DTS
      DTS  = DTS + CLFREQ(J)
      IF(DTS.GT.DRYDAY) DTS = DRYDAY
      DELDTS = DTS - DTS1
C=======================================================================
C     Increment dust and dirt load.
C=======================================================================
      IF(NOTDD.EQ.1)
     +         CALL BUILD(N,J,1,METHOD(J),DDL,DDPOW(J),DDF,DELDTS,DD)
C=======================================================================
C     Return through loop to increment individual constituent loads.
C=======================================================================
      INCR = 1
      TTT  = DELDTS
      GO TO 230
C=======================================================================
C     End of land use buildup loop.
C=======================================================================
  300 CONTINUE
C=======================================================================
C     For continuous SWMM, compute regeneration constants,
C     and convert time to seconds.
C=======================================================================
      DO 360 N      = 1,NOW
      DO 360 JJ     = 1,N1
      IF(N1.EQ.1) J = KLAND(N)
      IF(N1.GT.1) J = JJ
C=======================================================================
C     DD params may be functions of gutter length or subcatchment area.
C=======================================================================
                         DM = 1.0
      IF(JACGUT(J).EQ.0) DM = PLAND(J,N)*GQLEN(N)
      IF(JACGUT(J).EQ.1) DM = PLAND(J,N)*WAREA(N)/43560.0
      DDF = DDFACT(J)*DM
      DDL = DDLIM(J)*DM
C=======================================================================
C     Calculate three params for each constituent.
C=======================================================================
      DO 350 K = 1,NQSS
      IF(KALC(J,K).EQ.4) GO TO 350
      ND = NDIM(K) + 1
      IF(KALC(J,K).GT.0) GO TO 325
C=======================================================================
C     Here, have power-linear dust and dirt.
C=======================================================================
      IF(METHOD(J).EQ.0) THEN
C#### WCH, 10/1/93.  SUBSCRIPTS J,1,K WERE INVERTED.
                       CCOEF(J,K,N) = DDF*QFACT(J,1,K)*FACT2(ND)/86400.0
                       CPOW(J,K,N)  = DDPOW(J)
                       ENDIF
C=======================================================================
C     Here, have exponential dust and dirt.
C=======================================================================
      IF(METHOD(J).EQ.1) THEN
                       CPOW(J,K,N)  = DDPOW(J)/86400.0
                       CCOEF(J,K,N) = 0.0
                       ENDIF
C=======================================================================
C     Here, have Michaelis-Menten dust and dirt.
C=======================================================================
      IF(METHOD(J).EQ.2) THEN
                       CCOEF(J,K,N) = DDFACT(J)*86400.0
                       CPOW(J,K,N)  = 0.0
                       ENDIF
C=======================================================================
C     Convert same upper limit for all.
C=======================================================================
C#### WCH, 10/1/93.  SUBSCRIPTS J,1,K WERE INVERTED.
      CULIM(J,K,N) = DDL*QFACT(J,1,K)*FACT2(ND)
      GO TO 350
C=======================================================================
C     Here, have buildup coefficients for each constituent.
C     Params may be functions of gutter length or subcatchment area.
C=======================================================================
  325 DPM = 1.0
C#### WCH, 10/8/93.  INCLUDE PLAND FRACTION!!!!!
      IF(KACGUT(J,K).EQ.0) DPM = PLAND(J,N) * GQLEN(N)
      IF(KACGUT(J,K).EQ.1) DPM = PLAND(J,N) * WAREA(N)/43560.0
C=======================================================================
C     Here, have power-linear constituent.
C=======================================================================
      IF(KALC(J,K).EQ.1) THEN
                       CCOEF(J,K,N) = DPM*QFACT(J,3,K)*FACT1(ND)/86400.0
                       CPOW(J,K,N)  = QFACT(J,2,K)
                       ENDIF
C=======================================================================
C     Here, have exponential constituent.
C=======================================================================
      IF(KALC(J,K).EQ.2) THEN
                       CPOW(J,K,N) = QFACT(J,2,K)/86400.0
                       ENDIF
C=======================================================================
C     Here, have Michaelis-Menten constituent.
C=======================================================================
      IF(KALC(J,K).EQ.3) THEN
                       CCOEF(J,K,N) = QFACT(J,3,K)*86400.0
                       CPOW(J,K,N)  = 0.0
                       ENDIF
C=======================================================================
C     Set same upper limit for all methods.
C=======================================================================
      CULIM(J,K,N) = DPM*QFACT(J,1,K)*FACT1(ND)
  350 CONTINUE
C=======================================================================
C     More initialization.
C=======================================================================
      TBUILD(J,N) = 0.0
      TCLEAN(J,N) = DSLCL(J)*86400.0
  345 CONTINUE
  360 CONTINUE
C=======================================================================
C     Convert street sweeping parameters.
C=======================================================================
      DO 370 J  = 1,JLAND
  370 CLFREQ(J) = CLFREQ(J)*86400.0
C=======================================================================
C     Convert constituent parameters.
C=======================================================================
      DO 410 K    = 1,NQSS
      DO 410 J    = 1,JLAND
  410 RCOEFX(J,K) = RCOEF(J,K)/3600.0
C=======================================================================
C     Compute catchbasin loadings.
C     Note units:
C     CBVOL  = Cubic feet per basin
C     BASINS = No. basins per subcatchment, but redefined below as
C              total volume of catchbasins in subcatchment x regression
C              slope of 1.3 used for exponential wash-out (User's
C              Manual, Figure 4-40, page 181).
C     CBFACT = concentration of constituent in basin for land use J
C              and constituent K
C     DRYBSN = days required to regenerate catchbasin load
C     PBASIN = initial constituent load (e.g., mg, MPN) for all basins
C              in subcatchment
C     PLAND  = Fraction of land use J in subcatchment N, applied to
C              catchbasin volume here and to subcatchment runoff (WFLOW)
C              in Subroutine QSHED at each time step for computation
C              of catchbasin contribution.
C=======================================================================
      DRYBSN    = DRYBSN*86400.0
      DO 450 N  = 1,NOW
C=======================================================================
C     Catchbasin loadings.
C=======================================================================
      DO 420 K    = 1,NQSS
      PBASIN(K,N) = 0.0
      ND          = NDIM(K) + 1
C#### WCH, 10/1/93.  INITIAL CATCHBASIN LOADS ARE ALSO FUNCTIONS OF
C                    LAND USE FRACTIONS.
      DO 420 JJ = 1,N1
      IF(N1.EQ.1) J = KLAND(N)
      IF(N1.GT.1) J = JJ
      PBASIN(K,N) = PBASIN(K,N) + CBVOL * BASINS(N) * PLAND(J,N) *
     1                            CBFACT(J,K) * FACT3(ND)
  420 CONTINUE
      PPBASN(K,N) = PBASIN(K,N)
      BASINS(N)   = BASINS(N)*CBVOL*1.3
  450 CONTINUE
C=======================================================================
C     Save initial surface and catchbasin loadings.
C=======================================================================
      DO 470 K     = 1,NQSS
      DO 460 N     = 1,NOW
      IF(BASINS(N).GT.0.0) SUM(K,3) = SUM(K,3) + PBASIN(K,N)
      DO 460 J     = 1,JLAND
      SUM(K,1)     = SUM(K,1) + PSHED(J,K,N)
      OLDQP(J,K,N) = 0.0
  460 CONTINUE
  470 CONTINUE
      RETURN
      END

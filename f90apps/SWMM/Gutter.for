      SUBROUTINE GUTTER(REIGN,KWIKGT)
C     RUNOFF BLOCK
C     CALLED BY HYDRO NEAR LINES 98, 368
C=======================================================================
C     THIS SUBROUTINE COMPUTES THE INSTANTANEOUS WATER DEPTH
C     AND FLOW RATE FOR ALL THE CHANNELS/PIPES
C
C     GUTTER WAS UPDATED FOR SWMM RELEASE 4.0 DURING FEBRUARY, 1988
C     GUTTER WAS UPDATED DECEMBER 1992 BY WCH.
C     UPDATED 8/93 BY CHUCK MOORE, CDM FOR SUBCATCHMENT STATISTICS.
C     UPDATED 9/23/93 BY WCH (CDM) TO CORRECT DIMENSIONS.
C     CHANGED EVAP TO GVAP (4 LOCATIONS) FOR NO EVAPORATION DURING RAINY
C       TIME STEPS, WCH (CDM, CHUCK MOORE), 10/5/93.
C     UNITS CORRECTION FOR POFF = TDAY ETC. TO GET KG FOR METRIC=2,
C       WCH (WARREN CHRUSCIEL), 10/15/93.
C     TRY USING DMEAN INSTEAD OF DELT FOR TOTALS, WCH, 11/30/93.
C     ADD CHECK (KWIKGT) FOR PRESENCE OF CHANNEL/PIPE ROUTING IN ORDER
C       TO BE SURE TO USE  WET  TIME STEP, WCH, 4/7/94.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
      INCLUDE 'GRWTR.INC'
      INCLUDE 'NEW88.INC'
C#### C. MOORE, 8/93
      INCLUDE 'RUNSTAT.INC'
      CHARACTER*8 JNAME,ISPACE,IDASH,MOUT
      DIMENSION MOUT(MQUAL),JNAME(4)
C#### WCH (CDM), 9/93.  ALTER DIMENSIONS TO NG
      DIMENSION FLOWED(NG),QINTOT(NG),JSUR(NG)
      DIMENSION STGA(NGW),TH1A(NGW),GWFLWA(NGW)
      DATA JNAME/'Pounds ','Kilogram','Sums    ','Concent.'/
      DATA ISPACE/'        '/,IDASH/'--------'/
cWRMb  save variables to preserve values between
c      subroutine calls
      SAVE NSCRT2,NSCRT5,NSCRT7,INLETS,NQAL,TY,NOUT,
     A     JDREF,JRDAY,MONREF,NYREF,RAIND,RAINM,RAINY
cWRMe
C=======================================================================
C     DEFINITIONS
C
C     J - FIRST CHANNELS ROUTED ARE ACTUAL INPUT CHANNELS,
C         THEN NSAVE DUMMY CHANNELS ARE ROUTED.
C         ONE DUMMY CHANNEL IS ASSIGNED TO EACH INLET.
C     NGTOI = INTERNAL SUBSCRIPT OF A DUMMY CHANNEL FEEDING AN INLET.
C     NWTOG = INTERNAL SUBSCRIPT OF SUBCATCHMENT(S) FEEDING A CHANNEL
C     NGTOG = INTERNAL SUBSCRIPT OF CHANNEL(S) FEEDING A CHANNEL.
C     NOG   = NUMBER OF ACTUAL INPUT CHANNELS
C     NSAVE = NUMBER OF DUMMY CHANNELS
C
C     QIN MUST BE INTIALIZED TO ZERO FOR THE FIRST TIME STEP.
C     QIN IS USED IN TWO WAYS.
C     1. THE AVERAGE FLOW IN A TIME INTERVAL FOR SUBROUTINE GQUAL.
C     2. THE INSTANTANEOUS INFLOW FOR THE REST OF SUBROUTINE GUTTER.
C=======================================================================
      IF(MTIME.EQ.0) THEN
           JDREF  = JULDAY
           JRDAY  = NDAY
           MONREF = MONTH
           NYREF  = NYEAR
           NOUT   = JOUT(IOUTCT)
           NSCRT2 = NSCRAT(2)
           NSCRT5 = NSCRAT(5)
           NSCRT7 = NSCRAT(7)
           IF(NSCRT2.GT.0) REWIND NSCRT2
           IF(NSCRT5.GT.0) REWIND NSCRT5
           IF(NSCRT7.GT.0) REWIND NSCRT7
           NQAL   = NQS + 1
           RAIND  = 0.0
           RAINM  = 0.0
           RAINY  = 0.0
           TFLOW  = 0.0
           INLETS = NSAVE
           IF(METRIC.EQ.1) TY = TRIBA * 3630.0
           IF(METRIC.EQ.2) TY = TRIBA * 3630.0 / 25.4
C=======================================================================
C                    Write the first line of the interface file.
C=======================================================================
           IF(NOUT.GT.0) THEN
                BELT = 0.0
                IF(NQS.GT.0) WRITE(NOUT) JULDAY,TIMDAY,BELT,
     +            (FLOWED(JJ),(POFF(1,K,JJ),K=1,NQS),JJ=1,INLETS)
                IF(NQS.EQ.0) WRITE(NOUT) JULDAY,TIMDAY,BELT,
     +            (FLOWED(N),N=1,NSAVE)
                ENDIF
           IF(NQS.GT.0) THEN
                DO 5 J = 1,NQS
                IF(NDIM(J).EQ.0.AND.METRIC.EQ.1) MOUT(J)=JNAME(1)
                IF(NDIM(J).EQ.0.AND.METRIC.EQ.2) MOUT(J)=JNAME(2)
                IF(NDIM(J).EQ.1)  MOUT(J)   = JNAME(3)
                IF(NDIM(J).EQ.2)  MOUT(J)   = JNAME(4)
    5           CONTINUE
                ELSE
                MOUT(1) = ISPACE
                ENDIF
C#### WCH (CDM), 9/93.  LOOP SHOULD BE TO NG, NOT NW.
           DO 15 I    = 1,NG
           GFLOW(I)   = 0.0
           QSUR(I)    = 0.0
           JSUR(I)    = 0
   15      QIN(I)     = 0.0
           DO 30 J    = 1,MQUAL+1
C#### WCH (CDM), 9/93.  LOOP SHOULD BE TO NG, NOT NW.
           DO 20 I    = 1,NG
           TDAY(I,J)  = 0.0
           TMON(I,J)  = 0.0
   20      TYEAR(I,J) = 0.0
   30      CONTINUE
C=======================================================================
           IF(IPRN(3).EQ.0) RETURN
           IF(IPRN(3).EQ.2) WRITE(N6,40) PMONTH(MONTH),NYEAR,
     +                           (PNAME(NN),NN=1,NQS)
           IF(IPRN(3).EQ.1) WRITE(N6,50) NYEAR,(PNAME(NN),
     +                            NN=1,NQS)
           IF(METRIC.EQ.1)  WRITE(N6,55) (ISPACE,MOUT(NN),
     +                            NN=1,NQS)
           IF(METRIC.EQ.2)  WRITE(N6,56) (ISPACE,MOUT(NN),
     +                            NN=1,NQS)
           WRITE(N6,57)     (IDASH,NN=1,NQAL)
           RETURN
           ENDIF
C#### WCH, 4/7/94.  INITIALIZE KWIKGT
      KWIKGT = 0
      NOGG                = NOG + NSAVE
      IF(NOG.EQ.999) NOGG = NSAVE
      DO 290 N            = 1, NOGG
      J                   = NGUT(N)
C=======================================================================
C     Inputs from adjacent watershed areas.
C
C     OUTFLW is the instantaneous flow from a channel/pipe.
C     WFLOW is the instantaneous flow from a subcatchment
C                  at the end of a time interval.
C     QINN stores the instantaneous inflow from all subcatchments.
C=======================================================================
      QINN      = 0.0
      OUTFLW(J) = 0.0
      DO 70 JK  = 1,NCP
      IF(NWTOG(JK,J).EQ.0) GO TO 75
      NX        = NWTOG(JK,J)
      QINN      = QINN      + WFLOW(NX)
  70  OUTFLW(J) = OUTFLW(J) + WFLOW(NX)
C=======================================================================
C     Add subsurface flows if necessary.
C=======================================================================
 75   IF(NOGWSC.GT.0) THEN
                DO 77 IA = 1,NOGWSC
                IF(JCE.EQ.0.AND.NGWTOG(IA).NE.NAMEG(J)) GO TO 77
                IF(JCE.EQ.1.AND.KGWTOG(IA).NE.KAMEG(J)) GO TO 77
                QINN  =  QINN + GWFLOW(IA)
C##### WCH, 12/92 USE DMEAN (AVG DT) FOR CONTINUITY
                CNT(13)   = CNT(13)   + GWFLOW(IA)*DMEAN
                IF(NPG(J).EQ.3) OUTFLW(J) = OUTFLW(J) + GWFLOW(IA)
 77             CONTINUE
                ENDIF
C=======================================================================
C     QINT stores the instantaneous inflow from all subcatchments
C          and from all upstream channels/pipes.
C
C     The QIN(J) on the right side of the equation is the old
C         instantaneous inflow from the subcatchments for the
C         previous time step.
C
C     The new QIN(J) is the average inflow over the two time steps
C         from the subcatchments.
C=======================================================================
      QINT   = QINN
      QIN(J) = 0.5 * ( QIN(J) + QINN )
C=======================================================================
C     Inflows from upstream channel/pipes.
C=======================================================================
      DO 90 JK = 1,NCP
      IF(NGTOG(JK,J).EQ.0) GO TO 100
      NX        = NGTOG(JK,J)
      OUTFLW(J) = OUTFLW(J) + OUTFLW(NX)
      QINT      = QINT      + OUTFLW(NX)
C=======================================================================
C     The final value of QIN is the average inflow over a time step.
C=======================================================================
  90  QIN(J) = QIN(J) + GFLOW(NX)
C=======================================================================
C     Bypass routing for dummy channels or zero inflow and depth.
C=======================================================================
 100  IF(NPG(J).NE.3) THEN
                VSURO = QSUR(J)
C#######################################################################
C#### WCH, 4/7/94.  Add check for presence of channel/pipe routing to
C     use to set time step to WET in Sub. HYDRO.
C=======================================================================
                IF(QIN(J).GT.0.0.OR.GDEPTH(J).GT.0.0) THEN
                     KWIKGT = 1
                     CALL GUTNR
     +          (J,GLEN(J),GDEPTH(J),DELT,NPG(J),WTYPE(J),
     +          WELEV(J),WDIS(J),SPILL(J),GS1(J),GS2(J),GWIDTH(J),
     +          GCON(J),DFULL(J),QIN(J),QSUR(J),GFLOW(J),OUTFLW(J),
     +          AX0,AX1,FLOW0)
                     ENDIF
                ELSE
                GFLOW(J) = QIN(J)
                VSURO    = 0.0
                ENDIF
C=======================================================================
C     Calculate the evaporation loss from trapezoidal
C               or parabolic channels.
C=======================================================================
      IF(GVAP.GT.0.0.AND.(NPG(J).EQ.1.OR.NPG(J).EQ.4)) THEN
         IF(NPG(J).EQ.1) WIDTH = (GS1(J)+GS2(J))*GDEPTH(J)+GWIDTH(J)
         IF(NPG(J).EQ.4) WIDTH = GWIDTH(J)*SQRT(GDEPTH(J)/DFULL(J))
         ELOSS = GVAP*WIDTH*GLEN(J)
         IF(GDEPTH(J)-GVAP*DELT.LT.0.0) THEN
                      ELOSS     = GDEPTH(J)/DELT*WIDTH*GLEN(J)
                      GDEPTH(J) = 0.0
                      ELSE
                      GDEPTH(J) = GDEPTH(J) - GVAP*DELT
                      ENDIF
         CNT(22) = CNT(22) + ELOSS*DELT
         ENDIF
C=======================================================================
C     Call GQUAL for water quality routing.
C=======================================================================
      IF(NQS.GT.0) CALL GQUAL(J,AX0,AX1,FLOW0,VSURO,QINT)
C=======================================================================
C     USE ARRAY QIN TO STORE THE INSTANTANEOUS INFLOW FROM SUBCATCHMENTS.
C     USE THE ARRAY QINTOT TO STORE THE INSTANTANEOUS INFLOW
C     FROM THE SUBCATCHMENTS AND UPSTREAM CHANNEL/PIPES.
C=======================================================================
      QIN(J)    = QINN
      QINTOT(J) = QINT
C#######################################################################
C     C. Moore, CDM, 8/93. Check for max flow, depth for summary output.
C#######################################################################
      IF(QINTOT(J) .GT. MAXQIN(J)) MAXQIN(J) = QINTOT(J)
      IF(NPG(J).EQ.3) THEN 
           IF(OUTFLW(J).LE.MAXDEP(J)) GO TO 289
           MAXDEP(J) = OUTFLW(J)
           MAXJUL(J) = JULDAY
           MAXTIM(J) = TIMDAY
           ELSE 
           IF(GDEPTH(J).LE.MAXDEP(J)) GO TO 289
           MAXJUL(J) = JULDAY
           MAXTIM(J) = TIMDAY
           MAXDEP(J) = GDEPTH(J)
           ENDIF
  289 CONTINUE
  290 CONTINUE
C=======================================================================
C     Write the scratch data set that communicates with PRFLOW.
C=======================================================================
      IF(NGWGF.GT.0) THEN
           IF(JSTART(1).GT.0) THEN
                DO 300 L = 1,NDET
                IF(JULDAY.GE.JSTART(L).AND.JULDAY.LE.JSTOP(L))
     +                                     GO TO 305
  300           CONTINUE
                GO TO 310
  305           CONTINUE
                ENDIF
           LU = 0
           DO 291 LB = 1,NOGWSC
           IF(NGWGW(LB).EQ.0) GO TO 291
           LU         = LU + 1
           TH1A(LU)   = TH1(LB)
           GWFLWA(LU) = GWFLOW(LB)
           STGA(LU)   = STG(LB)
 291       CONTINUE

c           WRITE(NSCRT5,ERR=776,IOSTAT=IECODE) JULDAY,TIMDAY,DELT,
c     +               (STGA(J8),J8=1,NGWGF),(GWFLWA(J9),J9=1,NGWGF),
c     +               (TH1A(J7),J7=1,NGWGF)
                WRITE(NSCRT5) JULDAY,TIMDAY,DELT,
     +               (STGA(J8),J8=1,NGWGF),(GWFLWA(J9),J9=1,NGWGF),
     +               (TH1A(J7),J7=1,NGWGF)
 310       CONTINUE
           ENDIF
C=======================================================================
C     Change the order in array OUTFLW from INLET based to NPRNT based.
C=======================================================================
      IF(NPRNT.GT.0) THEN
           IF(JSTART(1).GT.0) THEN
               DO 400 L = 1,NDET
               IF(JULDAY.GE.JSTART(L).AND.JULDAY.LE.JSTOP(L)) GO TO 405
  400          CONTINUE
               GO TO 410
  405          CONTINUE
               ENDIF
           DO 500 N = 1,NPRNT
           J        = IPRNT(N)
           K        = IABS(J)
           IF(J.LT.0) FLOWED(N) = OUTFLW(K)
           IF(J.GT.0) FLOWED(N) = QINTOT(K)
  500      CONTINUE
           WRITE(NSCRT2,ERR=777) JULDAY,TIMDAY,DELT,
     +                           (FLOWED(N),N=1,NPRNT)
           IF(NQS.NE.0) THEN
               DO 520 N    = 1,NPRNT
               J           = IABS(IPRNT(N))
               DO 520 K    = 1,NQS
 520           POFF(1,K,N) = C(K,J)
               WRITE(NSCRT2,ERR=777)
     +                ((POFF(1,J,JJ),J=1,NQS),JJ=1,NPRNT)
               ENDIF
 410       CONTINUE
           ENDIF
C=======================================================================
C     Write the channel depth scratch file.
C=======================================================================
      IF(MDEEP.GT.0) THEN
           IF(JSTART(1).GT.0) THEN
               DO 600 L = 1,NDET
               IF(JULDAY.GE.JSTART(L).AND.JULDAY.LE.JSTOP(L)) GO TO 605
  600          CONTINUE
               GO TO 610
  605          CONTINUE
               ENDIF
           WRITE(NSCRT7,ERR=779) JULDAY,TIMDAY,DELT,
     +                           (GDEPTH(IDEEP(N)),N=1,MDEEP)
  610      CONTINUE
           ENDIF
C=======================================================================
C     Print surcharge information for QSUR > 0.0
C=======================================================================
      IF(NOG.NE.999) THEN
           DO 350 N = 1,NOG
           J        = NGUT(N)
           IF(QSUR(J).GT.0) THEN
C#######################################################################
C     Chuck Moore, 8/93.
C     Save maximum surcharge volume, surcharge length for summary output.
C#######################################################################
               SURLEN(J) = SURLEN(J) + DELT
               MAXSUR(J)=AMAX1(MAXSUR(J),QSUR(J))
C
               JSUR(J) = JSUR(J) + 1
               IF(JSUR(J).EQ.1.OR.MOD(JSUR(J),20).EQ.0) THEN
                    IF(METRIC.EQ.1) THEN
                       IF(JCE.EQ.0) WRITE(N6,340) NAMEG(J),TIME/3600.0,
     +                              JSUR(J),QSUR(J),OUTFLW(J)
                       IF(JCE.EQ.1) WRITE(N6,341) KAMEG(J),TIME/3600.0,
     +                              JSUR(J),QSUR(J),OUTFLW(J)
                       ELSE
                       IF(JCE.EQ.0) WRITE(N6,342) NAMEG(J),TIME/3600.0,
     +                       JSUR(J),QSUR(J)/CMET(8,METRIC),
     +                       OUTFLW(J)/CMET(8,METRIC)
                       IF(JCE.EQ.1) WRITE(N6,343) KAMEG(J),TIME/3600.0,
     +                       JSUR(J),QSUR(J)/CMET(8,METRIC),
     +                       OUTFLW(J)/CMET(8,METRIC)
                       ENDIF
                    ENDIF
               ENDIF
  350      CONTINUE
           ENDIF
C=======================================================================
C     Write inlets to be saved.
C=======================================================================
      DO 650 N  = 1,INLETS
      J         = NGTOI(N)
      FLOWED(N) = OUTFLW(J)/CMET(8,METRIC)
C=======================================================================
C     POFF WILL HAVE  UNITS OF NDIM * CFS (METRIC=1) OR NDIM * CMS (=2).
C     C HAS UNITS OF NDIM: MG/L OR MPN/L OR CONCENTRATION.
C     OUTFLW HAS UNITS OF CFS.
C     FLOWED HAS UNITS OF CFS OR CMS, DEPENDING ON METRIC.
C
C     SUM(IZ,10) AND TDAY ARE THE ARRAYS THAT SUM THE TOTAL LOADS TO THE
C     INLETS IN UNITS OF NDIM * CF (METRIC=1) OR NDIM * CM (=2).
C     INTEGRATE TOTAL LOAD TO THE INLETS BY USING TRAPEZOIDAL RULE.
C
C     CAUTION!  POFF(J,K,N) IS DIMENSIONED POFF(NLU,NQS,NW).  BUT
C     DIMENSION N BELOW GOES FROM 1 TO NO. INLETS, AND NO. INLETS CAN GO
C     FROM 1 TO NG.  HOWEVER, SHOULD ALWAYS HAVE NO. INLETS <= NW, SO 
C     SHOULD BE OK.
C=======================================================================
      IF(NQS.GT.0) THEN
           DO 370 IZ    = 1,NQS
           POFF(1,IZ,N) = C(IZ,J)    * OUTFLW(J)/CMET(8,METRIC)
C#### WCH, 11/30/93.  TRY USING DMEAN INSTEAD OF DELT FOR TOTALS.
C####           SUM(IZ,10)   = SUM(IZ,10) + POFF(1,IZ,N)*DELT
           SUM(IZ,10)   = SUM(IZ,10) + POFF(1,IZ,N)*DMEAN
  370      CONTINUE
           ENDIF
C=======================================================================
C     Print summary data.
C=======================================================================
      IF(IPRN(3).EQ.0) GO TO 650
C#### WCH, 11/30/93.  TRY USING DMEAN INSTEAD OF DELT FOR TOTALS.
C####      TFLOW      = TFLOW     + OUTFLW(J)*DELT
C####      TDAY(N,1)  = TDAY(N,1) + OUTFLW(J)*DELT
      TFLOW      = TFLOW     + OUTFLW(J)*DMEAN
      TDAY(N,1)  = TDAY(N,1) + OUTFLW(J)*DMEAN
      DO 385 K   = 1,NQS
      KK         = K + 1
C#### WCH, 11/30/93.  TRY USING DMEAN INSTEAD OF DELT FOR TOTALS.
C####  385   TDAY(N,KK) = TDAY(N,KK) + POFF(1,K,N)*DELT
  385 TDAY(N,KK) = TDAY(N,KK) + POFF(1,K,N)*DMEAN
C=======================================================================
C     DAILY, MONTHLY, YEARLY TOTAL FLOWS  ARE IN UNITS OF DEPTH: IN. FOR 
C     METRIC = 1 AND MM FOR METRIC = 2, ACCOMPLISHED BY DIVIDING
C     TDAY(N,1)  (CFS) BY TY.  TY IS SET FOR U.S. OR METRIC EARLIER TO 
C     GIVE EITHER INCHES OR MM.
C     CONVERT QUALITY LOADING FROM UNITS OF NDIM * CF OR NDIM * CM
C     TO EITHER POUNDS OR KILOGRAMS DEPENDING ON THE VALUE OF METRIC.
C=======================================================================
      IF(N.EQ.1) RAIND = RAIND + REIGN*DELT/3600.0
C=======================================================================
      IF(TIME+DELT/10.0.GE.LONG) GO TO 380
      IF(JULDAY.EQ.JDREF)        GO TO 650
 380  IF(TDAY(N,1).GT.0.0) THEN
           IF(TY.NE.0.0) THEN
                TDAY(N,1) = TDAY(N,1)/TY
                ELSE
                TDAY(N,1) = 0.0
                ENDIF
           DO 395 L  = 2,NQAL
           JJ        = L - 1
           ND        = NDIM(JJ) + 1
           IF(METRIC.EQ.1) TDAY(N,L) = TDAY(N,L)*FACT3(ND)/FACT1(ND)
           IF(ND.EQ.2)     TDAY(N,L) = TDAY(N,L)*1.0E06
C#### WCH, 10/15/93.  SHOULD CONVERT FROM CM*MG/L TO KG.
C#### 395       IF(METRIC.EQ.2) TDAY(N,L) = TDAY(N,L)*FACT3(ND)/1.0E06
 395       IF(METRIC.EQ.2) TDAY(N,L) = TDAY(N,L)/1.0E03
           ENDIF
C=======================================================================
C     Write the daily summary.
C=======================================================================
      IF(IPRN(3).EQ.2.AND.(RAIND.GT.0.0.OR.TDAY(N,1).GT.0.0)) THEN
           IF(METRIC.EQ.1) THEN
                IF(JCE.EQ.0) WRITE(N6,415) JRDAY,NAMEG(J),RAIND,
     +                (TDAY(N,IZ),IZ=1,NQAL)
                IF(JCE.EQ.1) WRITE(N6,416) JRDAY,KAMEG(J),RAIND,
     +                (TDAY(N,IZ),IZ=1,NQAL)
                ELSE
                IF(JCE.EQ.0) WRITE(N6,815) JRDAY,NAMEG(J),RAIND,
     +                (TDAY(N,IZ),IZ=1,NQAL)
                IF(JCE.EQ.1) WRITE(N6,816) JRDAY,KAMEG(J),RAIND,
     +                (TDAY(N,IZ),IZ=1,NQAL)
                ENDIF
           ENDIF
C=======================================================================
C     Calculate the monthly flow and quality values.
C=======================================================================
      IF(N.EQ.1)      THEN
                      RAINM = RAINM + RAIND
                      RAINY = RAINY + RAIND
                      ENDIF
      IF(N.EQ.INLETS) THEN
                      JDREF = JULDAY
                      JRDAY = NDAY
                      RAIND = 0.0
                      ENDIF
      DO 430 IZ = 1,NQAL
      TMON(N,IZ) = TMON(N,IZ) + TDAY(N,IZ)
      TDAY(N,IZ) = 0.0
  430 CONTINUE
C=======================================================================
C     If it is the end of the simulation the monthly
C     and yearly totals will be printed.
C=======================================================================
      IF(TIME+DELT/10.0.GE.LONG) THEN
                                 NMNTH = MONTH
                                 GO TO 450
                                 ENDIF
      IF(MONTH.EQ.MONREF) GO TO 650
      IF(N.EQ.INLETS) MONREF = MONTH
      IF(N.EQ.1) THEN
                                NMNTH = MONTH - 1
                 IF(NMNTH.EQ.0) NMNTH = 12
                 ENDIF
C=======================================================================
C     PRINT MONTHLY TOTALS DEPENDING ON VALUE OF IPRN(3)
C=======================================================================
 450  IF(IPRN(3).EQ.2) THEN
           IF(METRIC.EQ.1) THEN
                IF(JCE.EQ.0) WRITE(N6,470) NAMEG(J),RAINM,
     +                                    (TMON(N,IZ),IZ=1,NQAL)
                IF(JCE.EQ.1) WRITE(N6,471) KAMEG(J),RAINM,
     +                                    (TMON(N,IZ),IZ=1,NQAL)
                ELSE
                IF(JCE.EQ.0) WRITE(N6,870) NAMEG(J),RAINM,
     +                                    (TMON(N,IZ),IZ=1,NQAL)
                IF(JCE.EQ.1) WRITE(N6,871) KAMEG(J),RAINM,
     +                                    (TMON(N,IZ),IZ=1,NQAL)
                ENDIF
           ENDIF
      IF(IPRN(3).EQ.1) THEN
           IF(METRIC.EQ.1) THEN
                IF(JCE.EQ.0) WRITE(N6,460) PMONTH(NMNTH),NAMEG(J),
     +                RAINM,(TMON(N,IZ),IZ=1,NQAL)
                IF(JCE.EQ.1) WRITE(N6,461) PMONTH(NMNTH),KAMEG(J),
     +                RAINM,(TMON(N,IZ),IZ=1,NQAL)
                ELSE
                IF(JCE.EQ.0) WRITE(N6,860) PMONTH(NMNTH),NAMEG(J),
     +                RAINM,(TMON(N,IZ),IZ=1,NQAL)
                IF(JCE.EQ.1) WRITE(N6,861) PMONTH(NMNTH),KAMEG(J),
     +                RAINM,(TMON(N,IZ),IZ=1,NQAL)
                ENDIF
           ENDIF
      IF(TIME+DELT/10.0.GE.LONG) GO TO 525
C=======================================================================
C     PRINT THE PAGE TITLES FOR A NEW MONTH.
C=======================================================================
      IF(IPRN(3).EQ.2.AND.N.EQ.INLETS) THEN
           WRITE(N6,40) PMONTH(MONTH),NYEAR,(PNAME(NN),NN=1,NQS)
           IF(METRIC.EQ.1)  WRITE(N6,55) (ISPACE,MOUT(NN),NN=1,NQS)
           IF(METRIC.EQ.2)  WRITE(N6,56) (ISPACE,MOUT(NN),NN=1,NQS)
           WRITE(N6,57)     (DASH,NN=1,NQAL)
           ENDIF
C=======================================================================
C     CALCULATE THE YEARLY FLOW AND QUALITY VALUES
C=======================================================================
  525 DO 490 IZ   = 1,NQAL
      TYEAR(N,IZ) = TYEAR(N,IZ) + TMON(N,IZ)
  490 TMON(N,IZ)  = 0.0
      IF(N.EQ.INLETS) RAINM = 0.0
C=======================================================================
C     IF IT'S THE END OF THE SIMULATION OR A NEW YEAR THEN PRINT THE
C     YEARLY TOTALS
C=======================================================================
      IF(TIME+DELT/10.0.GE.LONG) GO TO 530
      IF(NYEAR.EQ.NYREF)         GO TO 650
 530  IF(METRIC.EQ.1) THEN
      IF(JCE.EQ.0) WRITE(N6,510) NAMEG(J),RAINY,(TYEAR(N,IZ),IZ=1,NQAL)
      IF(JCE.EQ.1) WRITE(N6,511) KAMEG(J),RAINY,(TYEAR(N,IZ),IZ=1,NQAL)
           ELSE
      IF(JCE.EQ.0) WRITE(N6,910) NAMEG(J),RAINY,(TYEAR(N,IZ),IZ=1,NQAL)
      IF(JCE.EQ.1) WRITE(N6,911) KAMEG(J),RAINY,(TYEAR(N,IZ),IZ=1,NQAL)
           ENDIF
      IF(TIME+DELT/10.0.GE.LONG) GO TO 650
      IF(N.EQ.INLETS) NYREF = NYEAR
      DO 515 IZ             = 1,NQAL
 515  TYEAR(N,IZ)           = 0.0
      IF(N.EQ.INLETS) RAINY = 0.0
      IF(N.LT.INLETS) GO TO 650
C=======================================================================
C     PRINT THE NEW PAGE TITLES FOR A NEW YEAR.
C=======================================================================
      IF(IPRN(3).EQ.1) WRITE(N6,50) NYEAR,(PNAME(NN),NN=1,NQS)
      IF(IPRN(3).EQ.2) WRITE(N6,40) PMONTH(MONTH),
     +                            NYEAR,(PNAME(NN),NN=1,NQS)
      IF(METRIC.EQ.1)  WRITE(N6,55) (ISPACE,MOUT(NN),NN=1,NQS)
      IF(METRIC.EQ.2)  WRITE(N6,56) (ISPACE,MOUT(NN),NN=1,NQS)
      WRITE(N6,57)     (IDASH,NN=1,NQAL)
  650 CONTINUE
C=======================================================================
C     Write the SWMM interface file.
C=======================================================================
C     JULDAY = the Julian date
C     TIMDAY = time of day in seconds
C     POFF   = Quality in mg/l * cfs units
C=======================================================================
      IF(NOUT.GT.0) THEN
                    IF(NQS.GT.0) WRITE(NOUT,ERR=778) JULDAY,TIMDAY,DELT,
     1                 (FLOWED(JJ),(POFF(1,J,JJ),J=1,NQS),JJ=1,INLETS)
                    IF(NQS.EQ.0) WRITE(NOUT,ERR=778) JULDAY,TIMDAY,DELT,
     1                           (FLOWED(N),N=1,INLETS)
                    ENDIF
      RETURN
C=======================================================================
C     Stop when interface or scratch file writing error occurs.
C=======================================================================
 776  WRITE(N6,9776) IECODE
      STOP
 777  WRITE(N6,9777)
      STOP
 778  WRITE(N6,9778)
      STOP
 779  WRITE(N6,9779)
      STOP
C=======================================================================
cim 4/99 changed formats from 10( to 99(
cim elimate tabs
cim expand table to write all 10 characters and digits
 40   FORMAT(/,1H1,/,
     +' ***********************************************',/,
     +' * Summary of Quantity and Quality results for *',/,
     +' * ',10X,A10,' ',I4,10X,'         *',/,
     +' ***********************************************',//,
     12X,'Day',6X,'Inlet',5X,'Rain',5X,'Flow',1X,99(1X,A8,1X))
 50   FORMAT(/,1H1,/,
     +' ***********************************************',/,
     +' * Summary of Quantity and Quality results for *',/,
     +' * ',16X,' ',I4,14X,'         *',/,
     +' ***********************************************',//,
     16X,'Month',6X,'Inlet',4X,'Rain',4X,'Flow',1X,99(1X,A8,1X))
 55   FORMAT(              21X,'Inch',4X,'Inch',1X,99(A2,A8))
 56   FORMAT(              21X,' mm ',4X,' mm ',1X,99(A2,A8))
 57   FORMAT(' ---  ---------- -------- --------',99(1X,A8,'-'))
 340  FORMAT(/,
     1' ======> Channel/Pipe # ',I10,' Surcharged.',/
     2' ======> Time   =',1PE9.2,'  hours.  Occurrence #',I9,/,
     3' ======> Volume =',1PE9.2,' cub ft.  Full flow =',0PF9.1,' cfs.')
 341  FORMAT(/,
     1' ======> Channel/Pipe # ',A10,' Surcharged.',/
     2' ======> Time   =',1PE9.2,'  hours.  Occurrence #',I9,/,
     3' ======> Volume =',1PE9.2,' cub ft.  Full flow =',0PF9.1,' cfs.')
 342  FORMAT(/,
     1' ======> Channel/Pipe # ',I10,' Surcharged.',/
     2' ======> Time   =',1PE9.2,'  hours.  Occurrence #',I9,/,
     3' ======> Volume =',1PE9.2,' cub met. Full flow =',0PF9.1,' cms.')
 343  FORMAT(/,
     1' ======> Channel/Pipe # ',A10,' Surcharged.',/
     2' ======> Time   =',1PE9.2,'  hours.  Occurrence #',I9,/,
     3' ======> Volume =',1PE9.2,' cub met. Full flow =',0PF9.1,' cms.')
  415 FORMAT(1X,I3,2X,I10,1X,F8.5,1X,F8.5,99(G10.3))
  416 FORMAT(1X,I3,2X,A10,1X,F8.5,1X,F8.5,99(G10.3))
  460 FORMAT(1X,A10,2X,I10,1X,F8.5,1X,F8.5,99(G10.3))
  461 FORMAT(1X,A10,2X,A10,1X,F8.5,1X,F8.5,99(G10.3))
  470 FORMAT('Total ',I10,1X,F8.5,1X,F8.5,99(G10.3))
  471 FORMAT('Total ',A10,1X,F8.5,1X,F8.5,99(G10.3))
  510 FORMAT(' Year ',I10,1X,F8.5,1X,F8.5,99(G10.3))
  511 FORMAT(' Year ',A10,1X,F8.5,1X,F8.5,99(G10.3))
  815 FORMAT(1X,I3,2X,I10,1X,F8.2,1X,F8.5,99(G10.3))
  816 FORMAT(1X,I3,2X,A10,1X,F8.2,1X,F8.5,99(G10.3))
  860 FORMAT(1X,A10,1X,I10,1X,F8.2,1X,F8.5,99(G10.3))
  861 FORMAT(1X,A10,1X,A10,1X,F8.2,1X,F8.5,99(G10.3))
  870 FORMAT('Total ',I10,1X,F8.2,1X,F8.5,99(G10.3))
  871 FORMAT('Total ',A10,1X,F8.2,1X,F8.5,99(G10.3))
  910 FORMAT(' Year ',I10,1X,F8.2,1X,F8.5,99(G10.3))
  911 FORMAT(' Year ',A10,1X,F8.2,1X,F8.5,99(G10.3))
 9776 FORMAT(/,' ===> ERROR !!  WRITING GROUNDWATER INFORMATION ON THE',
     +         ' NSCRAT(5) FILE.  IOSTAT = ',I10)
 9777 FORMAT(/,' ===> ERROR !!  WRITING FLOW AND/OR QUALITY ON THE',
     +         ' NSCRAT(2) FILE.')
 9778 FORMAT(/,' ===> ERROR !!  WRITING OUTPUT INTERFACE FILE',
     +         ' ON THE JOUT FILE.')
 9779 FORMAT(/,' ===> ERROR !!  WRITING CHANNEL DEPTH ON THE',
     +         ' NSCRAT(7) FILE.')
C=======================================================================
       END

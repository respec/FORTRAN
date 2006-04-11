      SUBROUTINE QUAL(JNEXT)
C     TRANSPORT BLOCK
C     CALLED WITH JNEXT OF 0 FROM DWLOAD NEAR LINE 186
C     CALLED WITH JNEXT OF 1 FROM TRANS NEAR LINE 688
C=======================================================================
C     Routing routine for pollutants in sewer elements.
C     Complete mixing assumed in all elements.
C     Scour/deposition routine available using Shield's criterion.
C
C     UPDATES BY W.C.HUBER AND E.FOUFOULA, SEPT. 1981.
C     Updated March 1984 by Wayne Huber.
C     Last updated December, 1990 by R.E.D.
C     Updated 10/6/93 by WCH to prevent negative DENOM in routing.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'NAMES.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'TST.INC'
CIMT  ADD COMMON THAT CONTAINS PMANN
      INCLUDE 'NEWTR.INC'
      DIMENSION SURGE1(NET),SURGE2(NET),WELL1(NET),WELL2(NET),
     1            QO(NET),QI(NET),QO1(NET),QO2(NET),PUMP(NET)
      CHARACTER BMJ*10
      EQUIVALENCE (PUMP(1),DIST(1)),(SURGE1(1),P1(1)),(SURGE2(1),P2(1))
      EQUIVALENCE (QO1(1),QMAX(1)),(QO2(1),QFULL(1))
      EQUIVALENCE (QO(1),Q(1,2,2)),(QI(1),Q(1,1,2))
      EQUIVALENCE (WELL1(1),SLOPE(1)),(WELL2(1),ROUGH(1))
CIM FORMALLY SAVE VALUES FROM CALL TO CALL 
	SAVE G,B,C2,C3,A0,A1,A2,A3,ALFA
C=======================================================================
C     Initialization for first time step.
C=======================================================================
      IF(N.LE.1) THEN
                 G    = 32.17
                 B    = 0.1568
                 C2   = 0.4342944
                 C3   = 2.30258
                 A0   = -.907895
                 A1   = -1.232609
                 A2   =  0.729864
                 A3   = -0.0772426
                 ALFA = 0.0227
                 ENDIF
      NTPE = NTYPE(M)
C=======================================================================
C     Pipe mixing volume found by averaging upstream and downstream
C     flow areas and by multiplying by length of pipe.
C=======================================================================
      IF(NTPE.LE.18) THEN
            VOL1  = DIST(M)*(A(M,1,1)+A(M,2,1))/2.0*BARREL(M)
            VOL2  = DIST(M)*(A(M,1,2)+A(M,2,2))/2.0*BARREL(M)
            AREAF = (A(M,1,2) + A(M,2,2))/2.0
            RHYD  = RADH(AREAF)
            ENDIF
C=======================================================================
C     Manhole mixing volume is volume currently stored as surcharging.
C=======================================================================
      IF(NTPE.EQ.19.OR.NTPE.EQ.21.OR.NTPE.GE.23) THEN
                                           VOL1      = SURGE1(M)
                                           VOL2      = SURGE2(M)
                                           SURGE1(M) = SURGE2(M)
                                           ENDIF
C=======================================================================
C     Lift station mixing volume is volume currently in wet well.
C=======================================================================
      IF(NTPE.EQ.20) THEN
                   VOL1     = WELL1(M)
                   VOL2     = WELL2(M)
                   WELL1(M) = WELL2(M)
                   ENDIF
C=======================================================================
C     Storage unit.  Retrieve old and new storage volumes.
C     Note, old storage, STORL, not set equal to new storage, STORE,
C     until beginning of Subroutine TSTORG.  At end of TSTORG, STORE
C     will be different from STORL unless flow is constant.
C=======================================================================
      IF(NTPE.EQ.22) THEN
                   LSTOR = KSTORE(M)
                   VOL1  = STORL(LSTOR)
                   VOL2  = STORE(LSTOR)
                   ENDIF
C=======================================================================
C     Begin loop on pollutants.  Each pollutant has units according
C     to NDIM.  All inflow and upstream loads have units of cfs *
C     concentration. CPOL1, CPOL2 have units of concentration.
C     SCOUR and SUSP have units of conc (mg/l) * cubic feet.
C     Must have NDIM = 0 (conc = mg/l) to use scour/deposition.
C
C     CPOL1 is upstream.   CPOL1(M,1,IP) means at old time step.
C     CPOL2 is downstream. CPOL2(M,2,IP) means at new time step.
C=======================================================================
      DO 400 IP = 1, NPOLL
      IF(JNEXT.EQ.0.AND.SPG(IP).LE.1.0) GO TO 400
C=======================================================================
C     Must compute three inflow loads.
C          TOTAL1 = Load from upstream.
C          TOTAL2 = Load from DWF, INFILTRATION and MANHOLES.
C          TOTAL3 = Load from external inflows.
C=======================================================================
      DSUM   = 0.0
      PMAN   = 0.0
      TOTAL1 = 0.0
      TOTAL3 = PLUTO(IP,M)
C=======================================================================
C     Manhole load (conc*cfs) stored in different arrays.
C=======================================================================
CIMT      IF(NTPE.EQ.19) THEN
CIMT                   IF(IP.EQ.1) PMAN = GEOM1(M)
CIMT                   IF(IP.EQ.2) PMAN = SLOPE(M)
CIMT                   IF(IP.EQ.3) PMAN = ROUGH(M)
CIMT                   IF(IP.EQ.4) PMAN = GEOM2(M)
CIMT
CIM CHANGE FOR MONTHLY BASE FLOW FACTORS  PMAN IS LOAD
CIMT                   PMAN = PMAN * BFFMONTH(1,NINT(GEOM3(M)))
      IF(NTPE.EQ.19)   PMAN = PMANN(M,IP) * BFFMONTH(1,NINT(GEOM3(M)))
CIMT                   ENDIF
      TOTAL2     = PMAN + CPINF(IP)*QINFIL(M) + POLDWF(IP)
      DO 180 JJP = 1,3
      NNEED      = INUE(M,JJP)
      NTPEU        = NTYPE(NNEED)
CIM MODIFY NEXT LINE FOR TYPE 27 QUALITY FLOW DIVIDER 4/99
      IF(NTPEU.GT.20.AND.NTPEU.LT.27) THEN
                    KK  = GEOM3(NNEED)
                    BMJ = KGEOM(NNEED)
                    QQ  = QO2(NNEED)
                    IF(JCE.EQ.0.AND.NOE(M).EQ.KK)  QQ = QO1(NNEED)
                    IF(JCE.EQ.1.AND.KOE(M).EQ.BMJ) QQ = QO1(NNEED)
                    ELSE
                    QQ = Q(NNEED,2,2)
                    ENDIF
      TOTAL1                  = TOTAL1 + CPOL2(NNEED,2,IP)*QQ
      IF(SPG(IP).GT.1.0) DSUM = DSUM+CPOL2(NNEED,2,IP)*QQ*DS(NNEED,IP)
  180 CONTINUE
C=======================================================================
C     Compute inflow concentration.
C=======================================================================
      CPOL1(M,2,IP) = CPOL1(M,1,IP)
      IF(Q(M,1,2).GT.0.00001) CPOL1(M,2,IP) = (TOTAL1+TOTAL2+TOTAL3)/
     1                                         Q(M,1,2)
      ERODE = 0.0
      DEPOS = 0.0
      QAVE  = 0.5*(Q(M,2,1) + Q(M,2,2))
      VOL   = 0.5*(VOL1     + VOL2)
      DVDT  = (VOL2 - VOL1)/DT
      IF(NTPE.LT.19.OR.NTPE.EQ.22) GO TO 190
      IF(VOL.GT.0.0) GO TO 190
      CPOL2(M,1,IP) = CPOL1(M,2,IP)
  190 CONTINUE
      IF(SPG(IP).LE.1.0) GO TO 320
C=======================================================================
C     Scour-deposition routine starts here.
C     Add manhole contribution.
C=======================================================================
      DSUM  = DSUM + PMAN * PSDWF(IP) + POLDWF(IP) * PSDWF(IP)
     1             + PLUTO(IP,M)      * PSIZE(5,IP)
      SUSPD = QI(M)*CPOL1(M,2,IP)
                       DSMIX = 0.0
      IF(SUSPD.GT.0.0) DSMIX = DSUM/SUSPD
      DS(M,IP) = DSMIX
      IF(NTPE.GE.19)    GO TO 320
      IF(RHYD.LE.0.0) GO TO 320
C=======================================================================
C     Compute critical diameter of solids for motion
C     using Shields' diagram (only for conduits).
C=======================================================================
      USTAR = SQRT(G*RHYD*SLOPE(M))
      ANUM  = (RHYD*SLOPE(M))**(1.0-B/2.0)*(GNU**B)
      DEN   = (SPG(IP)-1.0)*ALFA*G**(B/2.0)
      CRD   = (ANUM/DEN)**(1.0/(B+1.0))
      RE    = CRD*USTAR/GNU
      IF(RE.GT.400.0)              GO TO 250
      IF(RE.GE.10.0.OR.RE.LE.1.47) GO TO 295
      CRD   = 5.735*GNU/SQRT(RHYD*SLOPE(M)*G)
      ITER  = 1
      C     = RHYD*SLOPE(M)*(SPG(IP)-1.0)
      USTAR = SQRT(G*RHYD*SLOPE(M))
      K     = 0
  220 RE    = CRD*USTAR/GNU
      IF(RE.LT.1.0)  GO TO 280
  230 IF(ITER.GT.20) GO TO 240
      X    = ALOG10(RE)
      SF   = A0  + A1*X + A2*X*X + A3*X*X*X
      DSF  = A1  + 2.0*A2*X + 3.0*A3*X*X
      F    = CRD - C*10.0**(-SF)
      DF   = 1.0 + C*10.0**(-SF)*C3*DSF*(C2/CRD)
      CRD1 = CRD-F/DF
      IF(ABS((CRD1-CRD)/CRD).LE.0.001) GO TO 270
      CRD  = CRD1
      ITER = ITER+1
      GO TO 220
  240 CRD  = 1.47*GNU/USTAR
      GO TO 295
  250 CRD  = RHYD*SLOPE(M)/(0.06*(SPG(IP)-1.))
      GO TO 295
  260 CRD  = 10.0*GNU/USTAR
      GO TO 295
  270 IF(RE.LE.1.47) GO TO 240
      IF(RE-10.0) 295,260,260
  280 K   = K+1
      RE  = 1.1
      IF(K-2) 230,230,240
  295                          CRITD = CRD*304.8
      IF(CRITD.GT.PSIZE(5,IP)) CRITD = PSIZE(5,IP)
C=======================================================================
C     Compute scour and deposition.
C     DS    = Max particle size in suspension
C     DB    = Min particle size in bed
C     FS    = Fraction of bed mass that is scoured.
C     FD    = Fraction of suspended mass that is deposited.
C     ERODE = Units of flow (cfs) * conc (mg/l).
C     DEPOS = Units of flow (cfs) * conc (mg/l).
C     SUSPD = Units of flow (cfs) * conc (mg/l).
C     SCOUR = Units of volume (cu ft) * conc (mg/l).
C=======================================================================
      PCTC = PCT(CRITD,IP,PGR(1,IP),PSIZE(1,IP),M,NOE(M),KOE(M),N)
      IF(CRITD.LT.DB(M,IP)) GO TO 310
C=======================================================================
C     Here, calculate erosion from bed.
C=======================================================================
      PCTB  = PCT(DB(M,IP),IP,PGR(1,IP),PSIZE(1,IP),M,NOE(M),KOE(M),N)
      FS    = 0.0
      IF(PCTB.GT.0.0) FS = (PCTB - PCTC)/PCTB
      ERODE = FS*SCOUR(M,IP)/DT
      DSS   = 0.0
      DENOM = SUSPD + ERODE
      IF(DENOM.GT.0.0) DSS = (DS(M,IP)*SUSPD+CRITD*ERODE)/DENOM
      DS(M,IP) = DSS
      DB(M,IP) = CRITD
      SUSPD    = SUSPD + ERODE
C#######################################################################
C     Here, calculate deposition to bed.
C     Changes in calculation of DEPOS and SCOUR as of December, 1990
C     thanks to suggestions of Poshu Huang of Najarian Associates.
C#######################################################################
  310 IF(CRITD.GT.DS(M,IP)) GO TO 320
      PCTS  = PCT(DS(M,IP),IP,PGR(1,IP),PSIZE(1,IP),M,NOE(M),KOE(M),N)
      FD    = 0.0
      DENOM = PGR(1,IP) - PCTS
      IF(DENOM.GT.0.0) FD = (PCTC-PCTS)/DENOM
      DEPOS = FD*SUSPD
      DBB   = 0.0
      DENOM = SCOUR(M,IP) + DEPOS*DT
      IF(DENOM.GT.0.0) DBB = (DB(M,IP)*SCOUR(M,IP)+CRITD*DEPOS*DT)/DENOM
      DB(M,IP)    = DBB
      SCOUR(M,IP) = SCOUR(M,IP) + (DEPOS-ERODE)*DT
      DS(M,IP)    = CRITD
C=======================================================================
C     Compute outflow concentration by complete mixing.
C     Use integrated form of complete mixing equation.
C     Use average flows, volumes and concentrations over 1 time step.
C=======================================================================
  320 D1    = DECAY(IP)
                         DENOM = 0.0
      IF(VOL.GT.1.0E-20) DENOM = (QAVE + DVDT)/VOL + D1
C#### WCH, 10/6/93.  DON'T ALLOW NEGATIVE DENOM.
      IF(DENOM.LT.0.0) DENOM = 0.0
      ARG   = DENOM*DT
                      EXXP = 0.0
      IF(ARG.LT.15.0) EXXP = EXP(-ARG)
      CQAVE                = 0.5*(Q(M,1,1)*CPOL1(M,1,IP) +
     1                            Q(M,1,2)*CPOL1(M,2,IP))
      CPOL2(M,2,IP)        = CPOL2(M,1,IP)*EXXP
      IF(DENOM.NE.0.0.AND.VOL.GT.1.0E-20)
     1                    CPOL2(M,2,IP) = (1.0-EXXP)*(CQAVE +
     1                          ERODE - DEPOS)/DENOM/VOL + CPOL2(M,2,IP)
      IF(DENOM.NE.0.0.AND.VOL.LE.1.0E-20) CPOL2(M,2,IP) =
     1                    CPOL2(M,2,IP) + CQAVE*(1.0-EXXP)
      IF(CPOL2(M,2,IP).LE.0.0) THEN
                               CPOL2(M,2,IP) = 0.0
                               CAVE          = CPOL2(M,1,IP)/2.0
                               ELSE
                               CAVE          = 0.5*CPOL2(M,1,IP) +
     1                                         0.5*CPOL2(M,2,IP)
                               ENDIF
CIMT  change 25 to (9+4*MQUAL)
      IF(D1.GT.0.0) XNT((9+4*MQUAL)+IP) =
     1              XNT((9+4*MQUAL)+IP) + D1*DT*VOL*CAVE
CIMQP  ADD CODE FOR TYPE 27 ELEMENTS
CIMQP  ADJUST CPOL2 FOR DELIVERY RATIO
CIMQP  TRACK LOST VOLUME FOR THIS ELEMENT/CONSTITUENT
CIMQP  AND TOTAL LOST VOLUME FOR THIS CONSTITUENT
      IF (NTPE.EQ.27) THEN
	XNT27(M,IP) = XNT27(M,IP) + 
     1              DT*(1.0-DRATIO(M,IP))*CPOL2(M,2,IP)*QO(M)
	XNT27(NE+1,IP) = XNT27(NE+1,IP) +
     1              DT*(1.0-DRATIO(M,IP))*CPOL2(M,2,IP)*QO(M)
	CPOL2(M,2,IP) = CPOL2(M,2,IP) * DRATIO(M,IP)
	ENDIF
  400 CONTINUE
      RETURN
      END

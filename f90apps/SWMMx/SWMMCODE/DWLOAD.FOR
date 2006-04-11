      SUBROUTINE DWLOAD
C=======================================================================
C     Routine to establish initial solids deposition using hourly
C     time step for 'DWDAYS' of dry weather prior to simulation.
C     Scour/deposition routines are in subroutine QUAL.
C
C     ROUTINE UPDATED BY E. FOUFOULA AND W. HUBER, SEPT. 1981.
C     Routine last updated by R.E.D. December 1990 and May 1993.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'DRWF.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'TST.INC'
      DIMENSION QI(NET),QO(NET),QO1(NET),QO2(NET),STSAVE(2)
      CHARACTER KILOG*9,BMJ*10
      EQUIVALENCE (QO1(1),QMAX(1)),(QO2(1),QFULL(1))
      EQUIVALENCE (QO(1),Q(1,2,2)),(QI(1),Q(1,1,2))
      DATA KILOG/'Kilograms'/
C=======================================================================
      DTSAVE = DT
      DT     = 3600.0
C=======================================================================
C     Begin loop on hour of day.
C     Must let N be one hour behind KJ in order to initialize storage
C                                              unit values when N = 0.
C=======================================================================
      N         = -1
      LDAY      = KDAY
      DO 200 KJ = 1,24
      N         = N + 1
C=======================================================================
C     Model DWF for a hour starting with empty sewer each hour
C           (Except for any deposition from previous hour).
C=======================================================================
      DO 100 I  = 1,NE
      M         = JR(I)
      NT        = NTYPE(M)
C=======================================================================
C     Initialize array values.
C=======================================================================
      INPUT     = M
      DDWF      = 0.0
      SUM1      = 0.0
      DO 10 K   = 1,NPOLL
   10 POLDWF(K) = 0.0
C=======================================================================
C     Correct sewage for daily variation
C     WDWF(INPUT,1) = B O D IN MG/L * cfs
C     WDWF(INPUT,2) = SS IN MG/L * cfs
C     WDWF(INPUT,3) = COLIFORM IN MPN/L * cfs
C=======================================================================
      IF(NFILTH.GT.0) THEN
      IF(I.EQ.1) THEN
                                 LHOUR = JHR   + KJ - 1
                 IF(LHOUR.GT.24) LHOUR = LHOUR - 24
                 IF(KJ.GT.LHOUR) LDAY  = LDAY  + 1
                 IF(LDAY.GT.7)   LDAY  = 1
                 ENDIF
      WDWF1 = WDWF(INPUT,1)*DVBOD(LDAY)*DVDWF(LDAY)
      WDWF2 = WDWF(INPUT,2)*DVSS(LDAY)*DVDWF(LDAY)
      WDWF3 = WDWF(INPUT,3)*HVCOLI(LHOUR)*HVDWF(LHOUR)
C=======================================================================
C     Correct sewage for hourly variation.
C=======================================================================
      POLDWF(1) = WDWF1*HVBOD(LHOUR)*HVDWF(LHOUR)
      POLDWF(2) = WDWF2*HVSS(LHOUR) *HVDWF(LHOUR)
      POLDWF(3) = WDWF3*HVCOLI(LHOUR)*HVDWF(LHOUR)
      DDWF      = QDWF(INPUT)*DVDWF(LDAY)*HVDWF(LHOUR)
      ENDIF
C=======================================================================
C     Add upstream flows.
C=======================================================================
      DO 20 J = 1,3
      L       = INUE(M,J)
      IF(L.GT.NE) GO TO 20
      NTU     = NTYPE(L)
      IF(NTU.GE.21) THEN
                    KK  = GEOM3(L)
                    BMJ = KGEOM(L)
                    QQ  = QO2(L)
                    IF(JCE.EQ.0.AND.NOE(M).EQ.KK)  QQ = QO1(L)
                    IF(JCE.EQ.1.AND.KOE(M).EQ.BMJ) QQ = QO1(L)
                    ELSE
                    QQ  = Q(L,2,1)*BARREL(L)
                    ENDIF
      SUM1 = SUM1 + QQ
   20 CONTINUE
      IF(NT.EQ.22.AND.KJ.EQ.1) THEN
                               KSTOR         = KSTORE(M)
                               STSAVE(KSTOR) = STORL(KSTOR)
                               ENDIF
C=======================================================================
C     Add DWF and infiltration for this hour.
C=======================================================================
                   QMANHO = 0.0
      IF(NT.EQ.19) QMANHO = DIST(M)
      SUM1                = SUM1 + DDWF + QINFIL(M) + QMANHO
      Q(M,1,1)            = SUM1/BARREL(M)
      Q(M,2,1)            = Q(M,1,1)
      Q(M,1,2)            = Q(M,1,1)
      Q(M,2,2)            = Q(M,1,1)
      IF(SUM1.LE.0.0) GO TO 100
C=======================================================================
C     In a flow divider type element, assume all DWF is not diverted.
C=======================================================================
      IF(NT.EQ.21.OR.NT.EQ.23.OR.NT.EQ.24) THEN
                                           QO1(M) = SUM1
                                           QO2(M) = 0.0
                                           ENDIF
C=======================================================================
C     For storage unit, do routing in ROUTE and TSTORG.
C=======================================================================
      IF(NT.EQ.22) THEN
C#### RED, 5/24/93.  REMOVE THIS LINE:   NITER = 1
                   CALL ROUTE(D1,Q1)
                   ENDIF
      IF(NT.EQ.25) THEN
                   QO1(M) = 0.0
                   QO2(M) = SUM1
                   ENDIF
C=======================================================================
C     If a non-conduit do not calculate any deposition.
C=======================================================================
      IF (NT.LE.18) THEN
                    PS = Q(M,1,1)/QFULL(M)
                    CALL FINDA(PS,A(M,1,2))
                    A(M,2,2) = A(M,1,2)
                    A(M,1,1) = A(M,1,2)
                    A(M,2,1) = A(M,1,2)
                    ENDIF
      CALL QUAL(0)
      DO 60 IP      = 1,NPOLL
      CPOL1(M,1,IP) = CPOL1(M,2,IP)
   60 CPOL2(M,1,IP) = CPOL2(M,2,IP)
  100 CONTINUE
  200 CONTINUE
C=======================================================================
C     Assume the save bed load accumulation for each DWDAYS.
C=======================================================================
      DO 210 IP   = 1,NPOLL
      DO 210 I    = 1,NE
      SCOUR(I,IP) = DWDAYS*SCOUR(I,IP)
  210 CONTINUE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      IDIDIT    = 0
      WRITE (N6,940) DWDAYS
      DO 4730 I = 1,NE
      M         = JR(I)
      IF(NTYPE(M).EQ.22) THEN
                         KSTOR        = KSTORE(M)
                         STORL(KSTOR) = STSAVE(KSTOR)
                         ENDIF
      IF(NTYPE(M).GT.18) GO TO 4730
      INDX      = 0
C=======================================================================
C     Convert to KG for output.
C=======================================================================
      SUM       = 0.0
      WINDEX    = 0.0
      DO 4725 K = 1,NPOLL
      IF(SPG(K).LE.1.0) GO TO 4725
      INDX       = INDX+1
      CPPP(INDX) = SCOUR(M,K)*28.3E-6
      SUM        = SUM + CPPP(INDX)
 4725 CONTINUE
      IF(SUM.GT.0.0) THEN
                     IF(IDIDIT.EQ.0) THEN
                                     WRITE (N6,942) (PNAME(K),K=1,KSPG)
                                     WRITE (N6,943) (KILOG,K=1,KSPG)
                                     KILOG = '---------'
                                     WRITE (N6,946) (KILOG,K=1,KSPG)
                                     IDIDIT = 1
                                     ENDIF
                     IF(JCE.EQ.0) WRITE(N6,944)NOE(M),(CPPP(J),J=1,KSPG)
                     IF(JCE.EQ.1) WRITE(N6,945)KOE(M),(CPPP(J),J=1,KSPG)
                     WINDEX = 1.0
                     ENDIF
 4730 CONTINUE
      IF(WINDEX.EQ.0.0) WRITE(N6,945)
      DT = DTSAVE
      RETURN
C=======================================================================
  940 FORMAT(//,1H1,/,
     1' *******************************************************',/,
     2' * Bed of solids after ',F5.1,6X,' days of dry-weather *',/,
     3' * flow (base flow) prior to beginning of simulation.  *',/,
     4' *******************************************************')
  942 FORMAT(//,'   ELEMENT',4(3X,A8))
  943 FORMAT('     NO.  ',4(1X,A9,1X))
  944 FORMAT(1X,I9,2X,4G11.5)
  954 FORMAT(1X,A9,2X,4G11.5)
  945 FORMAT(/,' No bed of solids existed before the ',
     +         'start of the storm.',//)
  946 FORMAT('  --------',4(1X,A9,1X))
C=======================================================================
      END

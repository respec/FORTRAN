      SUBROUTINE INITAL
C	TRANSPORT BLOCK
C	CALLED BY INTRAN NEAR LINE 1209
C=======================================================================
C     ROUTINE INITIALIZES FLOWS, AREAS, AND CONCENTRATIONS TO VALUES
C     CORRESPONDING TO DRY WEATHER FLOW PLUS INFILTRATION.
C     WHEN THERE ARE NO INITIAL VALUES OF DWF OR INFIL, VALUES REMAIN 0.0
C
C     LAST UPDATED AUGUST 1989 AND MAY 1993 BY R.E.D.
C     BE SURE TO INITIALIZE STORAGE CONCENTRATIONS, WCH, 10/6/93.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'DRWF.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'TST.INC'
CIMT  INCLUDE COMMON THAT INCLUDES PMANN
      INCLUDE 'NEWTR.INC'
CIMT
C=======================================================================
CIMT  SUM2 dimensioned to MQUAL
      DIMENSION SUM2(MQUAL),QO(NET),QI(NET),QO1(NET),QO2(NET)
      DIMENSION WELL1(NET),WELL2(NET)
CIMT changed format for writting results
CIMT  DIMENSION FIRMAT(13),KIRMAT(13),EFMT(2)
CIMT  CHARACTER FIRMAT*4,KIRMAT*4,EFMT*4
      CHARACTER BMJ*10
      EQUIVALENCE (QO1(1),QMAX(1)),(QO2(1),QFULL(1))
      EQUIVALENCE (QO(1),Q(1,2,2)),(QI(1),Q(1,1,2))
      EQUIVALENCE (WELL1(1),SLOPE(1)),(WELL2(1),ROUGH(1))
CIMT      DATA FIRMAT/'(I6,','I7,3','F10.','3,2X',',F9.','3,1X',',F9.',
CIMT     1            '3,1X',',F9.','3,1X',',F9.','3,1X',')   '/
CIMT      DATA KIRMAT/'(A9,','I4,3','F10.','3,2X',',F9.','3,1X',',F9.',
CIMT     1            '3,1X',',F9.','3,1X',',F9.','3,1X',')   '/
CIMT      DATA EFMT/',1X,','E9.3'/
C=======================================================================
      WRITE (N6,900)
      IF(NPOLL.LE.0) THEN
                     WRITE (N6,905)
                     IF(METRIC.EQ.1) WRITE (N6,906)
                     IF(METRIC.EQ.2) WRITE (N6,907)
	               WRITE(N6,908)
                     ELSE
                     WRITE (N6,905) (PNAME(K),K=1,NPOLL)
                     IF(METRIC.EQ.1) WRITE (N6,906) (PUNIT(K),K=1,NPOLL)
                     IF(METRIC.EQ.2) WRITE (N6,907) (PUNIT(K),K=1,NPOLL)
	               WRITE(N6,908) ('--------',K=1,NPOLL)
                     ENDIF
CIMT      IF(NPOLL.GT.0) THEN
CIMT      DO 200 K = 1,NPOLL
CIMT      IF(NDIM(K).NE.1) GO TO 200
CIMT      NNX         = 2 + 2*K
CIMT      DO 190 I    = 1,2
CIMT      MMX         = NNX + I
CIMT      KIRMAT(MMX) = EFMT(I)
CIMT  190 FIRMAT(MMX) = EFMT(I)
CIMT  200 CONTINUE
CIMT      ENDIF
C=======================================================================
C     SET UP METRIC CONVERSION FACTORS.
C     MULTIPLY FIRST UNITS TO OBTAIN SECOND UNITS.
C=======================================================================
      IF(METRIC.EQ.1) THEN
                      CMET1 = 1.0
                      CMET2 = 1.0
                      CMET3 = 1.0
                      ELSE
                      CMET1 = 3.281
C=======================================================================
C                     SQ M TO SQ FT.
C=======================================================================
                      CMET2 = 10.764
                      CMET3 = 35.31
                      ENDIF
      N        = 0
      DO 100 I = 1,NE
      M        = JR(I)
      NTPE       = NTYPE(M)
      IF(NTPE.EQ.22) KSTOR = KSTORE(M)
C=======================================================================
C     SUM UPSTREAM FLOWS AND POLLUTANT INFLOWS.
C=======================================================================
      SUM1    = 0.0
      IF(NPOLL.GT.0) THEN
                     DO 10 K = 1,NPOLL
   10                SUM2(K) = 0.0
                     ENDIF
      DO 20 J = 1,3
      L       = INUE(M,J)
      NTPEU     = NTYPE(L)
      IF(L.GT.NE) GO TO 20
CIM THIS LINE NEEDS TO BE MODIFIED TO ROUTE FLOWS FOR 
CIM TYPE 27
      IF(NTPEU.LE.20.OR.NTPEU.EQ.27) THEN
                    QQ   = Q(L,2,1)*BARREL(L)
                    ELSE
                    KK   = GEOM3(L)
                    BMJ  = KGEOM(L)
                    QQ   = QO2(L)
                    IF(JCE.EQ.0.AND.NOE(M).EQ.KK)  QQ = QO1(L)
                    IF(JCE.EQ.1.AND.KOE(M).EQ.BMJ) QQ = QO1(L)
                    ENDIF
      IF(NPOLL.GT.0) THEN
                     DO 25 K = 1,NPOLL
   25                SUM2(K) = SUM2(K) + CPOL2(L,1,K) * QQ
                     ENDIF
      SUM1 = SUM1 + QQ
   20 CONTINUE
      KHR     = JHR + 1
      IF(KHR.GT.24) KHR = 24
                         QMAN    =  0.0
CIM  CHANGE FOR VARIABLE BASE FLOW IN TRANSPORT
CIM  CHANGE MADE TO INCLUDE MONTHLY FLOW FACTOR TO DETERMINE
CIM  BASE FLOWS IN INITIAL CONDITIONS
CIM
      IF(NTPE.EQ.19) QMAN    = DIST(M)*BFFMONTH(1,NINT(GEOM3(M)))
CIM                                        ------------------------
CIM
CIM      IF(NTYPE(M).EQ.19) QMAN    = DIST(M)
cim  monthly variation in DWF
      IF (NTPE.EQ.19) THEN
	    BFF = BFFMONTH(2,NINT(GEOM3(M)))
	ELSE
	    BFF = 1.0
	ENDIF
      QQQ      = QDWF(M)*DVDWF(KDAY)*HVDWF(KHR)
cim  monthly variation
     a           * BFF
cims
      SUM1     = SUM1 + QINFIL(M) + QMAN + QQQ
      Q(M,1,1) = SUM1/BARREL(M)
      Q(M,2,1) = Q(M,1,1)
      IF(Q(M,1,1).LT.0.0) Q(M,1,1) = 0.0
      IF(Q(M,2,1).LT.0.0) Q(M,2,1) = 0.0
      IF(NPOLL.GT.0.AND.SUM1.GT.0.0) THEN
           DO 35 K = 1,NPOLL
           IF(K.EQ.1) SUM2(K) = SUM2(K)+WDWF(M,K)*DVBOD(KDAY)*
     1                  HVBOD(KHR)*DVDWF(KDAY)*HVDWF(KHR)
cim monthly variation
     2                  * BFF
cim
           IF(K.EQ.2) SUM2(K) = SUM2(K)+WDWF(M,K)*DVSS(KDAY)*
     1                  HVSS(KHR)*DVDWF(KDAY)*HVDWF(KHR)
cim monthly variation
     2                  * BFF
cim
           IF(K.EQ.3) SUM2(K) = SUM2(K)+WDWF(M,K)*1.0*HVCOLI(KHR)*
     1                     DVDWF(KDAY)*HVDWF(KHR)
cim monthly variation
     2                  * BFF
cim
           PMAN = 0.0
CIMT           IF(NTYPE(M).EQ.19) THEN
CIMT                         IF(K.EQ.1) PMAN = GEOM1(M)
CIMT                         IF(K.EQ.2) PMAN = SLOPE(M)
CIMT                         IF(K.EQ.3) PMAN = ROUGH(M)
CIMT                         IF(K.EQ.4) PMAN = GEOM2(M)
CIMT                         ENDIF
CIM CHANGE FOR MONTHLY BASE FLOW FACTORS
cim note that geom1 ...  are now mass not concentration
CIM   
CIMT Change for additional constituents
      IF(NTPE.EQ.19)  PMAN = PMANN(M,K)*BFFMONTH(1,NINT(GEOM3(M)))
CIMT
           SUM2(K)      = SUM2(K) + PMAN + CPINF(K)*QINFIL(M)
           CPOL1(M,1,K) = SUM2(K)/SUM1
CIMQP   INITIAL INITIAL OUTFLOW FOR TYPE 27 Quality Splitting Elements
      IF(NTPE.EQ.27) CPOL1(M,1,K) = CPOL1(M,1,K) * DRATIO(M,K)
CIMQP
           CPOL2(M,1,K) = CPOL1(M,1,K)
           IF(NTPE.EQ.22.AND.PTC0(KSTOR,K).GT.0.0)
     1                               CPOL2(M,1,K) = PTC0(KSTOR,K)
   35      CONTINUE
           ELSE
C#######################################################################
C     WCH, 10/6/93.  DON'T MISS OUT ON INITIALIZING STORAGE VOLUME
C       CONCENTRATIONS JUST BECAUSE THERE'S NO INFLOW.
C=======================================================================
           IF(NPOLL.GT.0.AND.SUM1.LE.0.0.AND.NTPE.EQ.22) THEN
                DO 38 K = 1,NPOLL
   38           CPOL2(M,1,K) = PTC0(KSTOR,K)
                ENDIF
           ENDIF
C=======================================================================
C     ASSUME ALL DWF IS NON-DIVERTED IN A FLOW DIVIDER TYPE ELEMENT.
C=======================================================================
C   MODIFIY TO INCLUDE TYPE 26 FLOW SPLIT
      IF(NTPE.EQ.21.OR.NTPE.EQ.23.OR.NTPE.EQ.26) THEN
                               QO1(M) = SUM1
                               QO2(M) = 0.0
                               ENDIF
C=======================================================================
C     LIFT STATION.
C=======================================================================
      IF(NTPE.EQ.20) THEN
                   WELL1(M) = WELL2(M)
                   IF(Q(M,2,1).GT.DIST(M)) THEN
                                           Q(M,2,1) = DIST(M)
                                           WELL1(M) = GEOM1(M)
                                           WELL2(M) = WELL1(M)
                                           ENDIF
                   ENDIF
C=======================================================================
C     STORAGE UNIT.
C=======================================================================
      IF(NTPE.EQ.22) THEN
C#### RED, 5/25/93.  REMOVE THIS LINE:    NITER = 1
                   CALL ROUTE(D1,Q1)
                   ENDIF
C=======================================================================
C     BACKWATER ELEMENT. ALL INITIAL FLOW IS
C                        THROUGH THE INTERMEDIATE CONDUITS.
C=======================================================================
      IF(NTPE.EQ.25) THEN
                   QO1(M)   = 0.0
                   QO2(M)   = SUM1
                   ENDIF
      VELINT   = 0.0
      A(M,1,1) = 0.0
C=======================================================================
C     DETERMINE INITIAL FLOW AREA FOR CONDUITS.
C=======================================================================
      IF(KLASS(NTPE).LE.2) THEN
                         PS       = Q(M,1,1)/QFULL(M)
                         CALL FINDA(PS,A(M,1,1))
                         A(M,2,1) = A(M,1,1)
                         VELINT   = VEL(Q(M,1,1),A(M,1,1))
                         ENDIF
      W1              = SUM1/CMET3
      IF(NTPE.EQ.22) W1 = QO(M)
      W2              = A(M,1,1)/CMET2
      W3              = VELINT/CMET1
CIMT  CHANGE FORMAT ON OUTPUTS  7001 was FIRMAT and 7002 was KIRMAT    
      IF(JCE.EQ.0.AND.NPOLL.LT.1) WRITE(N6,7001) NOE(M),NTPE,W1,W2,W3
      IF(JCE.EQ.1.AND.NPOLL.LT.1) WRITE(N6,7002) KOE(M),NTPE,W1,W2,W3
      IF(JCE.EQ.0.AND.NPOLL.GT.0) WRITE(N6,7001) NOE(M),NTPE,W1,W2,W3,
     +                            (CPOL2(M,1,K),K=1,NPOLL)
      IF(JCE.EQ.1.AND.NPOLL.GT.0) WRITE(N6,7002) KOE(M),NTPE,W1,W2,W3,
     +                            (CPOL2(M,1,K),K=1,NPOLL)
  100 CONTINUE
      RETURN
C=======================================================================
  900 FORMAT(1H1,/,
     1' ************************************************************',/,
     2' * ELEMENT FLOWS, AREAS, AND CONCENTRATIONS ARE INITIALIZED *',/,
     3' *         TO DRY WEATHER FLOW AND INFILTRATION VALUES.     *',/,
     4' ************************************************************',//
     5)
CIM SET MAXIMUM NUMBER TO 99
CWCH, 9/27/99.  USE WIDTH-12 FIELDS, IN CASE OF LARGE BACTERIA NOS. 
  905 FORMAT (3X,' ELEMENT',9X,'FLOW',6X,'AREA',5X,'VELOCITY',
     +          99(4X,A8))
  906 FORMAT (5X,'NUMBER  TYPE   (CFS)    (SQ FT)   (FT/SEC)',99(4X,A8))
  907 FORMAT (5X,'NUMBER  TYPE   (CMS)    (SQ M)     (M/SEC)',99(4X,A8))
  908 FORMAT (5X,'------  ----   -----    ------    --------',99(4x,A8))
7001  FORMAT(I10,I6,3F10.3,1X,99F12.3)
7002  FORMAT(A10,I6,3F10.3,1X,99F12.3)
C=======================================================================
      END

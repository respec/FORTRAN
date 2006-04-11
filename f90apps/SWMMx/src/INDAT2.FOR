      SUBROUTINE INDAT2
C=======================================================================
C     This subroutine reads data groups E1-I2
C     except for hydrograph lines in 'INFLOW'.
C     IT ALSO PERFORMS SOME INITIALIZATION.   ALL NODE-CONDUIT LINKAGES
C     ARE SET UP AND CONVERTED TO THE INTERNAL NUMBER SYSTEM
C     Additional error message placed by WCH, 8/28/92.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'ORF.INC'
      INCLUDE 'WEIR.INC'
      INCLUDE 'BND.INC'
      INCLUDE 'OUT.INC'
      INCLUDE 'TIDE.INC'
      INCLUDE 'HYFLOW.INC'
      INCLUDE 'VOLDAT.INC'
      DIMENSION ISWTCH(3)
      CHARACTER JTYPE(3)*10,ONAME(60)*8,WNAME(60)*8,
     +            PNAME(25)*8,OUTF(25)*8,OUTG(25)*8,BMJ*10
      DATA JTYPE/'  CONSTANT','  VARIABLE',' POWER FNC'/
      DATA ONAME/'ORF  # 1','ORF  # 2','ORF  # 3','ORF  # 4','ORF  # 5',
     +           'ORF  # 6','ORF  # 7','ORF  # 8','ORF  # 9','ORF  #10',
     +           'ORF  #11','ORF  #12','ORF  #13','ORF  #14','ORF  #15',
     +           'ORF  #16','ORF  #17','ORF  #18','ORF  #19','ORF  #20',
     +           'ORF  #21','ORF  #22','ORF  #23','ORF  #24','ORF  #25',
     +           'ORF  #26','ORF  #27','ORF  #28','ORF  #29','ORF  #30',
     +           'ORF  #31','ORF  #32','ORF  #33','ORF  #34','ORF  #35',
     +           'ORF  #36','ORF  #37','ORF  #38','ORF  #39','ORF  #40',
     +           'ORF  #41','ORF  #42','ORF  #43','ORF  #44','ORF  #45',
     +           'ORF  #46','ORF  #47','ORF  #48','ORF  #49','ORF  #50',
     +           'ORF  #51','ORF  #52','ORF  #53','ORF  #54','ORF  #55',
     +           'ORF  #56','ORF  #57','ORF  #58','ORF  #59','ORF  #60'/
      DATA WNAME/'WEIR # 1','WEIR # 2','WEIR # 3','WEIR # 4','WEIR # 5',
     +           'WEIR # 6','WEIR # 7','WEIR # 8','WEIR # 9','WEIR #10',
     +           'WEIR #11','WEIR #12','WEIR #13','WEIR #14','WEIR #15',
     +           'WEIR #16','WEIR #17','WEIR #18','WEIR #19','WEIR #20',
     +           'WEIR #21','WEIR #22','WEIR #23','WEIR #24','WEIR #25',
     +           'WEIR #26','WEIR #27','WEIR #28','WEIR #29','WEIR #30',
     +           'WEIR #31','WEIR #32','WEIR #33','WEIR #34','WEIR #35',
     +           'WEIR #36','WEIR #37','WEIR #38','WEIR #39','WEIR #40',
     +           'WEIR #41','WEIR #42','WEIR #43','WEIR #44','WEIR #45',
     +           'WEIR #46','WEIR #47','WEIR #48','WEIR #49','WEIR #50',
     +           'WEIR #51','WEIR #52','WEIR #53','WEIR #54','WEIR #55',
     +           'WEIR #56','WEIR #57','WEIR #58','WEIR #59','WEIR #60'/
      DATA PNAME/'PUMP # 1','PUMP # 2','PUMP # 3','PUMP # 4','PUMP # 5',
     +           'PUMP # 6','PUMP # 7','PUMP # 8','PUMP # 9','PUMP #10',
     +           'PUMP #11','PUMP #12','PUMP #13','PUMP #14','PUMP #15',
     +           'PUMP #16','PUMP #17','PUMP #18','PUMP #19','PUMP #20',
     +           'PUMP #21','PUMP #22','PUMP #23','PUMP #24','PUMP #25'/
      DATA  OUTF/'FREE # 1','FREE # 2','FREE # 3','FREE # 4','FREE # 5',
     +           'FREE # 6','FREE # 7','FREE # 8','FREE # 9','FREE #10',
     +           'FREE #11','FREE #12','FREE #13','FREE #14','FREE #15',
     +           'FREE #16','FREE #17','FREE #18','FREE #19','FREE #20',
     +           'FREE #21','FREE #22','FREE #23','FREE #24','FREE #25'/
      DATA  OUTG/'GATE # 1','GATE # 2','GATE # 3','GATE # 4','GATE # 5',
     +           'GATE # 6','GATE # 7','GATE # 8','GATE # 9','GATE #10',
     +           'GATE #11','GATE #12','GATE #13','GATE #14','GATE #15',
     +           'GATE #16','GATE #17','GATE #18','GATE #19','GATE #20',
     +           'GATE #21','GATE #22','GATE #23','GATE #24','GATE #25'/
C=======================================================================
C     READ STORAGE JUNCTION DATA ON DATA GROUP E1
C=======================================================================
      DO 640 I = 1,NVSE
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'E1') GO TO 645
      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,JSTORE(I),ZTOP(I),
     +                                ASTORE(I),NUMST
      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,KSTORE(I),ZTOP(I),
     +                                ASTORE(I),NUMST
C=======================================================================
C     READ VARIABLE STORAGE JUNCTION DATA
C=======================================================================
      IF(ASTORE(I).LT.0.0.AND.NUMST.GT.0) THEN
C         IF(NUMST.GT.NVST) CALL ERROR(119)
          NUMV(I) = NUMST
          READ(N5,*,ERR=888) CC
          IF(CC.NE.'E2') GO TO 888
          BACKSPACE N5
          READ(N5,*,ERR=888) CC,(VCURVE(I,1,J),VCURVE(I,2,J),J=1,NUMST)
          DO 252 JES = 2,NUMST
          IF(VCURVE(I,2,JES-1).GT.VCURVE(I,2,JES))
     +              WRITE(N6,999) VCURVE(I,2,JES-1),VCURVE(I,2,JES)
 252      CONTINUE
C=======================================================================
C         INTEGRATE VOLUME CURVE FROM AREA/STAGE DATA
C=======================================================================
          VCURVE(I,3,1)         = 0.0
          IF(METRIC.EQ.1) QCOEF = 43560.0
          IF(METRIC.EQ.2) QCOEF = 10000.0
          VCURVE(I,1,1) = VCURVE(I,1,1)*QCOEF
          DO 251      J = 2,NUMST
          VCURVE(I,1,J) = VCURVE(I,1,J)*QCOEF
  251     VCURVE(I,3,J) = VCURVE(I,3,J-1)+
     +                    (VCURVE(I,2,J)-VCURVE(I,2,J-1))*
     +                    (VCURVE(I,1,J)+VCURVE(I,1,J-1))/2.0
          ENDIF
C=======================================================================
C     READ VARIABLE STORAGE JUNCTION DATA DEFINED AS A POWER FUNCTION
C     VCURVE(I,1,1) = COEFFICIENT
C     VCURVE(I,2,1) = POWER
C=======================================================================
      IF(NUMST.LT.0) THEN
                      NUMV(I)   =   NUMST
                      ASTORE(I) = -1234.0
                      READ(N5,*,ERR=888) CC
                      IF(CC.NE.'E2') GO TO 888
                      BACKSPACE N5
                      READ(N5,*,ERR=888) CC,VCURVE(I,1,1),VCURVE(I,2,1)
                      ENDIF
  640 CONTINUE
  645 NSTORE = I-1
C=======================================================================
C     WRITE THE PAGE TITLES
C=======================================================================
      IF(NSTORE.GT.0) THEN
                      WRITE(N6,2999)
                      WRITE(N6,5060) ALPHA1,ALPHA2
                      WRITE(N6,5398)
      IF(METRIC.EQ.1) WRITE(N6,5495)
      IF(METRIC.EQ.2) WRITE(N6,5496)
C=======================================================================
C     CONVERT TO INTERNAL NUMBER SYSTEM
C=======================================================================
      DO 646 I = 1,NSTORE
      DO 648 J = 1,NJ
      IF(JCE.EQ.0.AND.JSTORE(I).EQ.JUN(J))  GO TO 650
      IF(JCE.EQ.1.AND.KSTORE(I).EQ.AJUN(J)) GO TO 650
  648 CONTINUE
      IF(JCE.EQ.0) WRITE(N6,5494) JSTORE(I)
      IF(JCE.EQ.1) WRITE(N6,5484) KSTORE(I)
      NSTOP     = NSTOP + 1
  650 JSTORE(I) = J
      JSKIP(J)  = 0
      ZCROWN(J) = ZTOP(I)
      JJ        = NUMV(I)
      IF(ZCROWN(J).GT.GRELEV(J)) GRELEV(J) = ZCROWN(J) + 0.1
      IF(ASTORE(I).GT.0) THEN
                         CF  = ASTORE(I)*(ZTOP(I)-Z(J))
                         AF  = ASTORE(I)
                         JJJ = 1
                         ENDIF
      IF(ASTORE(I).LT.0.AND.JJ.GT.0) THEN
                                     CF  = VCURVE(I,3,JJ)
                                     AF  = VCURVE(I,1,JJ)
                                     JJJ = 2
                                     ENDIF
      IF(ASTORE(I).LT.0.AND.JJ.LT.0) THEN
                                     AF  = VCURVE(I,1,1) *
     +                                     (ZTOP(I)-Z(J))**VCURVE(I,2,1)
                                     CF  = AF*(ZTOP(I) - Z(J))
                                     JJJ = 3
                                     ENDIF
      LSTORE  = JSTORE(I)
      IF(JCE.EQ.0) WRITE(N6,5399)  JUN(LSTORE),JTYPE(JJJ),AF,CF,ZTOP(I)
      IF(JCE.EQ.1) WRITE(N6,5499) AJUN(LSTORE),JTYPE(JJJ),AF,CF,ZTOP(I)
  646 CONTINUE
      NTL = NTL + NSTORE
      ENDIF
C=======================================================================
C     INITIALIZE NTC AND NTL
C=======================================================================
      NTC  = NC
      NTL  = NC
      NVOR = 0
C=======================================================================
C     Read orifice data on data group F1.
C=======================================================================
      DO 690 I = 1,NEO
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'F1') GO TO 695
      N = NTC+I
      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,(NJUNC(N,K),K=1,2),
     +                       NKLASS(N),AORIF(I),CORIF(I),ZU(N)
      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,(KJUNC(N,K),K=1,2),
     +                       NKLASS(N),AORIF(I),CORIF(I),ZU(N)
C=======================================================================
C     READ F2 DATA GROUP FOR ORIFICES WITH A TIME HISTORY
C=======================================================================
      IF(NKLASS(N).LT.0) THEN
                         NVOR      = NVOR + 1
                         NKLASS(N) = IABS(NKLASS(N))
                         READ(N5,*,ERR=888) CC,NTIME,(VORIF(NVOR,J,1),
     +                     VORIF(NVOR,J,2),VORIF(NVOR,J,3),J=1,NTIME)
                         ENDIF
  690 CONTINUE
  695 NORIF = I-1
      NTC   = NTC + NORIF
      NTL   = NTL + NORIF
      IF(NORIF.GT.0) THEN
      IF(METRIC.EQ.1) WRITE(N6,5420)
      IF(METRIC.EQ.2) WRITE(N6,5421)
      DO 780 I = 1,NORIF
      N        = NTC - NORIF + I
      IF(JCE.EQ.0) WRITE(N6,5440) (NJUNC(N,K),K=1,2),NKLASS(N),
     +                             AORIF(I),CORIF(I),ZU(N)
      IF(JCE.EQ.1) WRITE(N6,5441) (KJUNC(N,K),K=1,2),NKLASS(N),
     +                             AORIF(I),CORIF(I),ZU(N)
C=======================================================================
C     Convert to internal number system.
C=======================================================================
      LORIF(I) = N
      NCOND(N) = N + 90000
      IF(JCE.EQ.1) ACOND(N) = ONAME(I)
      DEEP(N)  = SQRT(4.0*AORIF(I)/3.14159)
      WIDE(N)  = DEEP(N)
      AFULL(N) = AORIF(I)
      RFULL(N) = DEEP(N)/4.0
                      DDD = DELT
      IF(NEQUAL.GT.1) DDD = FLOAT(NEQUAL)
      CLEN     = 2.0*DDD*SQRT(GRVT*DEEP(N))
      LEN(N)   = AMAX1(200.,CLEN)
      CMANN    = CMET(9,METRIC)
      ROUGH(N) = CMANN*RFULL(N)**.66667/(CORIF(I)*SQRT(LEN(N)*2.0*GRVT))
      NKLASS(N)= NKLASS(N) + 8
      IF(NKLASS(N).EQ.10) ZU(N) = -0.96*DEEP(N)
C=======================================================================
C     NKLASS(N) = 1, NKLASS(N) = 9  - SIDE OUTLET
C     NKLASS(N) = 2, NKLASS(N) = 10 - BOTTOM OUTLET (SUMP)
C=======================================================================
      DO 770 K = 1,2
      DO 700 J = 1,NJ
      IF(JCE.EQ.0.AND.NJUNC(N,K).EQ.JUN(J))  GO TO 720
      IF(JCE.EQ.1.AND.KJUNC(N,K).EQ.AJUN(J)) GO TO 720
  700 CONTINUE
      IF(JCE.EQ.0) WRITE(N6,5450) NJUNC(N,K)
      IF(JCE.EQ.1) WRITE(N6,5451) KJUNC(N,K)
      NSTOP      = NSTOP+1
  720 NJUNC(N,K) = J
      IF(JSKIP(J).EQ.1) THEN
                        JSKIP(J)   = 0
                        IF(ZCROWN(J).LT.DEEP(N)) ZCROWN(J) = DEEP(N)
                        ENDIF
C=======================================================================
C     SET ZU(N) FOR BOTTOM OUTLET
C     SET ZU(N) AND ZD(N) ELEVATIONS
C     LOWER Z(J) AT UPSTREAM END BY -0.96*DEEP(N)
C=======================================================================
      IF(K.EQ.1) THEN
                 ZU(N) = ZU(N) + Z(J)
                 ZD(N) = ZU(N) - 0.01/CMET(1,METRIC)
                 IF(NKLASS(N).EQ.10) THEN
                              IF(Y(J).GT.0.0) Y(J) = Y(J) + 0.96*DEEP(N)
                              Z(J) = ZU(N)
                              ENDIF
                 ENDIF
C=======================================================================
C     CHECK GROUND ELEVATION
C=======================================================================
      IF(ZU(N)+DEEP(N).GE.GRELEV(J)) THEN
                                     IF(JCE.EQ.0) WRITE(N6,5455) JUN(J)
                                     IF(JCE.EQ.1) WRITE(N6,5456)AJUN(J)
                                     NSTOP = NSTOP+1
                                     ENDIF
      IF(ZD(N)+DEEP(N).GE.GRELEV(J)) THEN
                                     IF(JCE.EQ.0) WRITE(N6,5455) JUN(J)
                                     IF(JCE.EQ.1) WRITE(N6,5456)AJUN(J)
                                     NSTOP = NSTOP+1
                                     ENDIF
  730 CONTINUE
      DO 740 KK = 1,8
      IF(NCHAN(J,KK)) 760,760,740
  740 CONTINUE
  760 NCHAN(J,KK) = N
  770 CONTINUE
C=======================================================================
C     CHECK GRAVITY FLOW DIRECTION
C=======================================================================
      IF(ZU(N).LT.ZD(N)) THEN
                         J2    = NJUNC(N,2)
                         IF(JCE.EQ.0) WRITE(N6,5458)  JUN(J2)
                         IF(JCE.EQ.1) WRITE(N6,5459) AJUN(J2)
                         NSTOP = NSTOP+1
                         ENDIF
  780 CONTINUE
      DO 790 I = 1,NORIF
      N        = LORIF(I)
      IF(JCE.EQ.0) WRITE(N6,6010) I,NCOND(N),DEEP(N),LEN(N),
     +                            ROUGH(N),ZU(N),ZD(N)
  790 IF(JCE.EQ.1) WRITE(N6,6011) I,ACOND(N),DEEP(N),LEN(N),
     +                            ROUGH(N),ZU(N),ZD(N)
      ENDIF
C=======================================================================
C     READ WEIR DATA ON DATA GROUP G1
C     THIS ROUTINE HAS BEEN MODIFIED TO TRANSFER
C     WEIR DISCHARGES FROM NODE TO NODE RATHER
C     THAN FROM NODE TO CONDUIT
C=======================================================================
      DO 820 I = 1,NEW
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'G1') GO TO 840
      N = NTC+I
      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,(NJUNC(N,K),K=1,2),KWEIR(I),
     +                             YCREST(I),YTOP(I),WLEN(I),COEF(I)
      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,(KJUNC(N,K),K=1,2),KWEIR(I),
     +                             YCREST(I),YTOP(I),WLEN(I),COEF(I)
  820 CONTINUE
  840 NWEIR = I-1
      IF(NWEIR.GT.0) THEN
                     IF(METRIC.EQ.1) WRITE(N6,5480)
                     IF(METRIC.EQ.2) WRITE(N6,5481)
                     DO 1020 I = 1,NWEIR
                     N1        = NTC+I
                     LWEIR(I)  = N1
                     IF(JCE.EQ.0) NCOND(N1) = 90000 + N1
                     IF(JCE.EQ.1) ACOND(N1) = WNAME(I)
                     COEFS(I)  = 0.0
                     IF(JCE.EQ.0) WRITE(N6,5487) (NJUNC(N1,K),K=1,2),
     +                            NCOND(N1),KWEIR(I),YCREST(I),
     +                            YTOP(I),WLEN(I),COEF(I)
                     IF(JCE.EQ.1) WRITE(N6,5488) (KJUNC(N1,K),K=1,2),
     +                            ACOND(N1),KWEIR(I),YCREST(I),
     +                            YTOP(I),WLEN(I),COEF(I)
                     DO 875 K = 1,2
                     IF(JCE.EQ.0.AND.NJUNC(N1,K).EQ.0)   GO TO 875
                     IF(JCE.EQ.1.AND.KJUNC(N1,K).EQ.' ') GO TO 875
                     DO 870 J = 1,NJ
                     IF(JCE.EQ.0.AND.NJUNC(N1,K).EQ.JUN(J))  GO TO 871
                     IF(JCE.EQ.1.AND.KJUNC(N1,K).EQ.AJUN(J)) GO TO 871
  870                CONTINUE
                     IF(JCE.EQ.0) WRITE(N6,5490) NJUNC(N1,K)
                     IF(JCE.EQ.1) WRITE(N6,5491) KJUNC(N1,K)
                     NSTOP       = NSTOP+1
  871                NJUNC(N1,K) = J
                     DO 873 KK   = 1,8
                     IF(NCHAN(J,KK)) 874,874,873
  873                CONTINUE
  874                NCHAN(J,KK) = N1
  875                CONTINUE
 1020                CONTINUE
                     NTL = NTL + NWEIR
                     ENDIF
C=======================================================================
C     READ PUMP DATA ON DATA GROUP H1
C
C     IPTYP = 1  OFF-LINE PUMP OPERATES ON WET WELL VOLUME
C     IPTYP = 2  IN-LINE PUMP OPERATES ON HEAD AT JUNCTION
C     IPTYP = 3  IN-LINE PUMP WITH THREE POINT PUMP CURVE
C
C     NOTE -- ONLY ONE INFLUENT PIPE MAY BE CONNECTED TO
C             AN OFF-LINE PUMP NODE
C=======================================================================
      DO 1060 I = 1,NEP
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'H1') GO TO 1080
      READ(N5,*,ERR=888) CC,IPTYP(I)
      BACKSPACE N5
      N = NTL + I
      IF(IPTYP(I).EQ.1.AND.JCE.EQ.0) READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (NJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     2                (VRATE(I,K),K=1,3),VWELL(I)
      IF(IPTYP(I).EQ.1.AND.JCE.EQ.1) READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (KJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     2                (VRATE(I,K),K=1,3),VWELL(I)
      IF(IPTYP(I).EQ.2.AND.JCE.EQ.0) READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (NJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     1                (VRATE(I,K),K=1,2)
      IF(IPTYP(I).EQ.2.AND.JCE.EQ.1) READ(N5,*,ERR=888) CC,IPTYP(I),
     1                (KJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     1                (VRATE(I,K),K=1,2)
      IF(IPTYP(I).EQ.3.AND.JCE.EQ.0) READ(N5,*,ERR=888) CC,IPTYP(I),
     1                        (NJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     2                    (VRATE(I,K),K=1,3),VWELL(I),PON(I),POFF(I)
      IF(IPTYP(I).EQ.3.AND.JCE.EQ.1) READ(N5,*,ERR=888) CC,IPTYP(I),
     1                        (KJUNC(N,K),K=1,2),(PRATE(I,K),K=1,3),
     2                    (VRATE(I,K),K=1,3),VWELL(I),PON(I),POFF(I)
      ISWTCH(IPTYP(I)) = IPTYP(I)
 1060 CONTINUE
 1080 NPUMP = I - 1
C=======================================================================
C     PRINT PUMP NODES
C=======================================================================
      IF(NPUMP.GT.0) THEN
                     DO 1175 J=1,3
                     IF (ISWTCH(J)-1) 1175,1110,1130
C=======================================================================
C     PRINT OFF-LINE PUMP DATA
C=======================================================================
 1110                IF(METRIC.EQ.1) WRITE(N6,5560)
                     IF(METRIC.EQ.2) WRITE(N6,5561)
                     DO 1120 I = 1,NPUMP
                     N         = NTL+I
                     IF (IPTYP(I)-1) 1120,1115,1120
 1115                IF(JCE.EQ.0) WRITE(N6,5580) I,(NJUNC(N,K),K=1,2),
     +                   (PRATE(I,K),K=1,3),(VRATE(I,K),K=1,3),VWELL(I)
                     IF(JCE.EQ.1) WRITE(N6,5584) I,(KJUNC(N,K),K=1,2),
     +                    (PRATE(I,K),K=1,3),(VRATE(I,K),K=1,3),VWELL(I)
 1120               CONTINUE
                    GO TO 1175
 1130               IF(ISWTCH(J)-2) 1175,1140,1160
C=======================================================================
C                   PRINT IN-LINE PUMP DATA
C=======================================================================
 1140               IF(METRIC.EQ.1) WRITE(N6,5565)
                    IF(METRIC.EQ.2) WRITE(N6,5566)
                    DO 1150 I = 1,NPUMP
                    N         = NTL+I
                    IF(IPTYP(I)-2) 1150,1145,1150
 1145               IF(JCE.EQ.0) WRITE(N6,5581) I,(NJUNC(N,K),K=1,2),
     +                 (PRATE(I,K),K=1,3),(VRATE(I,K),K=1,2)
                    IF(JCE.EQ.1) WRITE(N6,5585) I,(KJUNC(N,K),K=1,2),
     +                 (PRATE(I,K),K=1,3),(VRATE(I,K),K=1,2)
 1150               CONTINUE
                    GO TO 1175
C=======================================================================
C                   PRINT PUMP CURVE DATA
C=======================================================================
 1160               IF(METRIC.EQ.1) WRITE(N6,5570)
                    IF(METRIC.EQ.2) WRITE(N6,5571)
                    DO 1170 I = 1,NPUMP
                    N         = NTL + I
                    IF(IPTYP(I)-3) 1170,1165,1170
 1165               IF(JCE.EQ.0) WRITE(N6,5582) I,(NJUNC(N,K),K=1,2),
     +                           VWELL(I),(PRATE(I,K),K=1,3),
     +                           (VRATE(I,K),K=1,3),PON(I),POFF(I)
                    IF(JCE.EQ.1) WRITE(N6,5586) I,(KJUNC(N,K),K=1,2),
     +                           VWELL(I),(PRATE(I,K),K=1,3),
     +                           (VRATE(I,K),K=1,3),PON(I),POFF(I)
C#######################################################################
C CHECK FOR FLAT PUMP CURVE.  CANNOT HAVE VRATE1=VRATE2 OR VRATE2=VRATE3
C THIS AVOIDS ZERO DIVIDE IN SUBROUTINE BOUND
C WCH, 8/28/92
C#######################################################################
           IF(VRATE(I,1).EQ.VRATE(I,2).OR.VRATE(I,2).EQ.VRATE(I,3)) THEN
             WRITE(N6,5575)
             NSTOP = NSTOP+1
           ENDIF
 1170               CONTINUE
 1175               CONTINUE
C=======================================================================
C                   CONVERT TO INTERNAL NUMBER SYSTEM
C=======================================================================
                    DO 1240 I = 1,NPUMP
                    N         = NTL+I
                    LPUMP(I)  = N
                    NCOND(N)  = N + 90000
                    IF(JCE.EQ.1) ACOND(N)  = PNAME(I)
                    DO 1220 K = 1,2
                    DO 1180 J = 1,NJ
                    IF(JCE.EQ.0.AND.NJUNC(N,K).EQ.JUN(J))  GO TO 1190
                    IF(JCE.EQ.1.AND.KJUNC(N,K).EQ.AJUN(J)) GO TO 1190
 1180               CONTINUE
                    IF(JCE.EQ.0) WRITE(N6,5590) NJUNC(N,K)
                    IF(JCE.EQ.1) WRITE(N6,5591) KJUNC(N,K)
                    NSTOP      = NSTOP+1
 1190               NJUNC(N,K) = J
                    DO 1195 KK = 1,8
                    IF(NCHAN(J,KK)) 1200,1200,1195
 1195               CONTINUE
 1200               NCHAN(J,KK) = N
                    IF(IPTYP(I).GE.2) GO TO 1220
                    IF(KK.LE.2)       GO TO 1220
                    IF(K.EQ.2)        GO TO 1220
                    IF(JCE.EQ.0) WRITE(N6,5595)  JUN(J)
                    IF(JCE.EQ.1) WRITE(N6,5596) AJUN(J)
                    NSTOP = NSTOP + 1
 1220               CONTINUE
C=======================================================================
C                   SET INFLOW INDEX FOR PUMP NODE
C=======================================================================
                    JP  = NJUNC(N,1)
                    IF(IPTYP(I).GE.2) GO TO 1235
                    JSKIP(JP) = 1
                    Z(JP)     = -100.0
 1235               CONTINUE
                    JPFUL(I)  = 1
 1240               CONTINUE
                    NTL       = NTL + NPUMP
                    ENDIF
C=======================================================================
C     READ DATA FOR OUTFALLS WITHOUT TIDE GATES ON DATA GROUP I1
C=======================================================================
      NFREE     = 0
      DO 1280 I = 1,NTG
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'I1') GO TO 1300
      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,JFREE(I),NBCF(I)
      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,KFREE(I),NBCF(I)
      NFREE = NFREE + 1
      IF(NPUMP.GT.0) THEN
               DO 1340 K = 1,NPUMP
               N         = LPUMP(K)
               JDN       = NJUNC(N,2)
               IF(JCE.EQ.1) BMJ       = KJUNC(N,2)
               IF(JCE.EQ.0.AND.JUN(JDN).EQ.JFREE(I))  JSKIP(JDN) = 2
               IF(JCE.EQ.1.AND.AJUN(JDN).EQ.KFREE(I)) JSKIP(JDN) = 2
C              IF(JSKIP(JDN).EQ.2)   ZCROWN(JDN) = 2.0 + Z(JDN)
 1340          CONTINUE
               ENDIF
C=======================================================================
C     CHECK FOR WEIR OUTFALLS WITHOUT TIDE GATES
C=======================================================================
      IF(NWEIR.GT.0) THEN
               DO 1350 K = 1,NWEIR
               N         = LWEIR(K)
               J         = NJUNC(N,1)
               IF(JCE.EQ.1) BMJ       = KJUNC(N,1)
               IF(JCE.EQ.0.AND.J.EQ.JFREE(I))   THEN
                                                JTIDES(J) = NBCF(I)
                                                NFREE     = NFREE - 1
                                                GO TO 1350
                                                ENDIF
               IF(JCE.EQ.1.AND.BMJ.EQ.KFREE(I)) THEN
                                                JTIDES(J) = NBCF(I)
                                                NFREE     = NFREE - 1
                                                GO TO 1350
                                                ENDIF
 1350          CONTINUE
               ENDIF
C=======================================================================
C     Check for pump outfalls.
C=======================================================================
      IF(NPUMP.GT.0) THEN
               DO 1375 K = 1,NPUMP
               N         = LPUMP(K)
               J         = NJUNC(N,1)
               IF(JCE.EQ.1) BMJ       = KJUNC(N,1)
               IF(JCE.EQ.0.AND.J.EQ.JFREE(I))   THEN
                                                JTIDES(J) = NBCF(I)
                                                NFREE     = NFREE - 1
                                                GO TO 1375
                                                ENDIF
               IF(JCE.EQ.1.AND.BMJ.EQ.KFREE(I)) THEN
                                                JTIDES(J) = NBCF(I)
                                                NFREE     = NFREE - 1
                                                GO TO 1375
                                                ENDIF
 1375          CONTINUE
               ENDIF
 1280 CONTINUE
 1300 CONTINUE
C=======================================================================
C     PRINT OUTFLOW NODES
C=======================================================================
      IF(NFREE.GT.0) THEN
                     WRITE(N6,5616)
                     IF(JCE.EQ.0) WRITE(N6,5620) (JFREE(I),
     +                                           NBCF(I),I=1,NFREE)
                     IF(JCE.EQ.1) WRITE(N6,5621) (KFREE(I),
     +                                           NBCF(I),I=1,NFREE)
C=======================================================================
C                    CONVERT TO INTERNAL NUMBER SYSTEM
C=======================================================================
                     DO 1390 I = 1,NFREE
                     DO 1360 J = 1,NJ
                     IF(JCE.EQ.0.AND.JFREE(I).EQ.JUN(J))  GO TO 1380
                     IF(JCE.EQ.1.AND.KFREE(I).EQ.AJUN(J)) GO TO 1380
 1360                CONTINUE
                     IF(JCE.EQ.0) WRITE(N6,5630) JFREE(I)
                     IF(JCE.EQ.1) WRITE(N6,5631) KFREE(I)
                     NSTOP      = NSTOP + 1
 1380                JFREE(I)   = J
C=======================================================================
C                   CHECK FREE BC'S FOR MULTIPLE INPUT CONDUITS
C=======================================================================
                     IF(NCHAN(J,2).GT.0) THEN
                              IF(JCE.EQ.0) WRITE(N6,1853)  JUN(J)
                              IF(JCE.EQ.1) WRITE(N6,1854) AJUN(J)
                              NSTOP     = NSTOP + 1
                              ENDIF
                     JTIDES(J)  = NBCF(I)
                     N          = NTL + I
                     NJUNC(N,1) = J
                     NJUNC(N,2) = 0
                     IF(JCE.EQ.1) KJUNC(N,1) = AJUN(J)
                     IF(JCE.EQ.1) KJUNC(N,2) = 'BOUNDARY  '
                     NCHAN(J,2) = N
                     NCOND(N)   = N + 90000
                     IF(JCE.EQ.1) ACOND(N)   = OUTF(I)
                     IF(JSKIP(J).EQ.0) JSKIP(J)   = 1
 1390                CONTINUE
                     NTL        = NTL + NFREE
                     ENDIF
C=======================================================================
C     READ DATA FOR OUTFALLS WITH TIDE GATES ON DATA GROUP I2
C=======================================================================
      NGATE     = 0
      DO 1420 I = 1,NTG
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'I2') GO TO 1440
      IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,JGATE(I),NBCG(I)
      IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,KGATE(I),NBCG(I)
      NGATE = NGATE + 1
C=======================================================================
C     CHECK FOR WEIR OUTFALLS WITH TIDE GATES
C=======================================================================
      IF(NWEIR.GT.0) THEN
               DO 1450 K = 1,NWEIR
               N         = LWEIR(K)
               J         = NJUNC(N,1)
               IF(JCE.EQ.1) BMJ       = KJUNC(N,1)
               IF(JCE.EQ.0.AND.J.EQ.JGATE(I))   THEN
                                                JTIDES(J) = NBCG(I)
                                                NGATE     = NGATE - 1
                                                GO TO 1450
                                                ENDIF
               IF(JCE.EQ.1.AND.BMJ.EQ.KGATE(I)) THEN
                                                JTIDES(J) = NBCG(I)
                                                NGATE     = NGATE - 1
                                                GO TO 1450
                                                ENDIF
 1450          CONTINUE
               ENDIF
C=======================================================================
C     CHECK FOR PUMP OUTFALLS WITH TIDE GATES
C=======================================================================
      IF(NPUMP.GT.0) THEN
               DO 1475 K = 1,NPUMP
               N         = LPUMP(K)
               J         = NJUNC(N,1)
               IF(JCE.EQ.1) BMJ       = KJUNC(N,1)
               IF(JCE.EQ.0.AND.J.EQ.JGATE(I))   THEN
                                                JTIDES(J) = NBCG(I)
                                                NGATE     = NGATE - 1
                                                GO TO 1475
                                                ENDIF
               IF(JCE.EQ.1.AND.BMJ.EQ.KGATE(I)) THEN
                                                JTIDES(J) = NBCG(I)
                                                NGATE     = NGATE - 1
                                                GO TO 1475
                                                ENDIF
 1475          CONTINUE
               ENDIF
 1420 CONTINUE
 1440 CONTINUE
C=======================================================================
C     PRINT TIDE GATE NODES
C=======================================================================
      IF(NGATE.GT.0) THEN
                     WRITE(N6,5656)
                     IF(JCE.EQ.0) WRITE(N6,5660) (JGATE(I),
     +                                           NBCG(I),I=1,NGATE)
                     IF(JCE.EQ.1) WRITE(N6,5661) (KGATE(I),
     +                                           NBCG(I),I=1,NGATE)
C=======================================================================
C                    CONVERT TO INTERNAL NUMBER SYSTEM
C=======================================================================
                     DO 1510 I = 1,NGATE
                     DO 1480 J = 1,NJ
                     IF(JCE.EQ.0.AND.JGATE(I).EQ.JUN(J))  GO TO 1500
                     IF(JCE.EQ.1.AND.KGATE(I).EQ.AJUN(J)) GO TO 1500
 1480                CONTINUE
                     IF(JCE.EQ.0) WRITE(N6,5662) JGATE(I)
                     IF(JCE.EQ.1) WRITE(N6,5663) KGATE(I)
                     NSTOP      = NSTOP + 1
 1500                JGATE(I)   = J
C=======================================================================
C                   CHECK TIDAL BC'S FOR MULTIPLE INPUT CONDUITS
C=======================================================================
                     IF(NCHAN(J,2).GT.0) THEN
                              IF(JCE.EQ.0) WRITE(N6,1853)  JUN(J)
                              IF(JCE.EQ.1) WRITE(N6,1854) AJUN(J)
                              NSTOP     = NSTOP + 1
                              ENDIF
                     JTIDES(J)  = NBCG(I)
                     N          = NTL + I
                     NJUNC(N,1) = J
                     NJUNC(N,2) = 0
                     IF(JCE.EQ.1) KJUNC(N,1) = AJUN(J)
                     IF(JCE.EQ.1) KJUNC(N,2) = 'BOUNDARY  '
                     NCHAN(J,2) = N
                     NCOND(N)   = N + 90000
                     IF(JCE.EQ.1) ACOND(N)   = OUTG(I)
                     JSKIP(J)   = 1
 1510                CONTINUE
                     NTL        = NTL + NGATE
                     ENDIF
C=======================================================================
C     PRINT WEIR OUTFALL NODES
C=======================================================================
      IF(NWEIR.GT.0) THEN
                     ILOOP      = 0
                     DO 1470 K  = 1,NWEIR
                     N          = LWEIR(K)
                     J          = NJUNC(N,1)
                     JJ         = NJUNC(N,2)
                     IF(JJ.LE.0) THEN
                          ILOOP = ILOOP + 1
                          IF(ILOOP.EQ.1) WRITE(N6,6660)
                          IF(JCE.EQ.0) WRITE(N6,6665)  JUN(J),JTIDES(J)
                          IF(JCE.EQ.1) WRITE(N6,6670) AJUN(J),JTIDES(J)
                          ENDIF
 1470                CONTINUE
                     ENDIF
C=======================================================================
C     PRINT PUMP OUTFALL NODES
C=======================================================================
      IF(NPUMP.GT.0) THEN
                     ILOOP      = 0
                     DO 1570 K  = 1,NPUMP
                     N          = LPUMP(K)
                     J          = NJUNC(N,1)
                     JJ         = NJUNC(N,2)
                     IF(JJ.LE.0) THEN
                        ILOOP = ILOOP + 1
                        IF(ILOOP.EQ.1) WRITE(N6,6680)
                        IF(JCE.EQ.0) WRITE(N6,6665)  JUN(J),JTIDES(J)
                        IF(JCE.EQ.1) WRITE(N6,6670) AJUN(J),JTIDES(J)
                        ENDIF
 1570                CONTINUE
                     ENDIF
C=======================================================================
C     INTERNAL CONNECTIVITY INFORMATION
C=======================================================================
      WRITE(N6,2999)
      WRITE(N6,5060) ALPHA1,ALPHA2
      WRITE(N6,5665)
      IF(JCE.EQ.0) WRITE(N6,5670)
      IF(JCE.EQ.1) WRITE(N6,5671)
      N1        = NC+1
      DO 1525 N = N1,NTL
      J1        = NJUNC(N,1)
      J2        = NJUNC(N,2)
      IF(J2.GT.0.AND.JCE.EQ.0) J2 = JUN(J2)
      IF(JCE.EQ.0) WRITE(N6,5675) NCOND(N),JUN(J1),J2
      IF(JCE.EQ.1) WRITE(N6,5685) ACOND(N),KJUNC(N,1),KJUNC(N,2)
 1525 CONTINUE
      IF(NJ.GT.NEE) THEN
                    WRITE(N6,5676) NEE
                    NSTOP = NSTOP+1
                    ENDIF
      IF(NTL.GT.NEE) THEN
                     WRITE(N6,5677) NEE
                     NSTOP = NSTOP+1
                     ENDIF
      RETURN
 888  CALL IERROR
C=======================================================================
 999  FORMAT(/,' ====> WARNING !!! AREA DECREASES BETWEEN STAGES ',
     +       F10.3,' AND ',F10.3)
 1853 FORMAT(/,' ===> ERROR !!  OUTFALL JUNCTION ',I9,' HAS TWO OR',/,
     +         '                MORE CONNECTING CONDUITS.')
 1854 FORMAT(/,' ===> ERROR !!  OUTFALL JUNCTION ',A10,' HAS TWO OR',/,
     +         '                MORE CONNECTING CONDUITS.')
2999  FORMAT(/,
     1       '1',40(2H--)/' ','ENVIRONMENTAL PROTECTION AGENCY',13X,40H*
     2***   EXTENDED TRANSPORT PROGRAM   ****,8X,'WATER RESOURCES DIVISI
     3ON',/,' ','WASHINGTON, D.C.            ',16X,4H****,32X,4H****,8X,
     4'CAMP DRESSER & MCKEE INC.',/,' ','                ',28X,4H****,
     56X,'   ANALYSIS MODULE  ',6X,4H****,8X,'ANNANDALE, VIRGINIA')
 5060 FORMAT(/,5X,A80,/,5X,A80,/)
 5398 FORMAT(//,
     +' ******************************************************',/,
     +' *              STORAGE JUNCTION DATA                 *',/,
     +' ******************************************************',/)
 5399 FORMAT(8X,I9,A10,2F18.2,F11.3)
 5499 FORMAT(7X,A10,A10,2F18.2,F11.3)
 5420 FORMAT(//,
     +' *********************************************',/,
     +' *              ORIFICE DATA                 *',/,
     +' *********************************************',//,
     *'       FROM         TO                AREA      DISCHARGE   HEIGH
     +T ABOVE',/,
     *'   JUNCTION   JUNCTION      TYPE     (FT2)    COEFFICIENT  JUNCTI
     +ON (FT)',/,
     *'   --------   --------      ----     -----    -----------  ------
     +-------')
 5421 FORMAT(//,
     +' *********************************************',/,
     +' *              ORIFICE DATA                 *',/,
     +' *********************************************',//,
     *'       FROM         TO                AREA      DISCHARGE   HEIGH
     +T ABOVE',/,
     *'   JUNCTION   JUNCTION      TYPE    (MET2)    COEFFICIENT  JUNCTI
     +ON  (M)',/,
     *'   --------   --------      ----     -----    -----------  ------
     +-------')
 5440 FORMAT(1X,3I10,F10.2,F15.3,F15.3)
 5441 FORMAT(1X,2A10,I10,F10.2,F15.3,F15.3)
 5450 FORMAT(/,' ====> ERROR !!!  ORIFICE JUNCTION ',I9,
     1' IS NOT CONTAINED IN JUNCTION DATA')
 5451 FORMAT(/,' ====> ERROR !!!  ORIFICE JUNCTION ',A10,
     1' IS NOT CONTAINED IN JUNCTION DATA')
 5455 FORMAT(/,' ====> ERROR !!! ORIFICE TOP LIES ABOVE GROUND ELEVATION
     . AT JUNCTION ',I9)
 5456 FORMAT(/,' ====> ERROR !!! ORIFICE TOP LIES ABOVE GROUND ELEVATION
     . AT JUNCTION ',A10)
 5458 FORMAT(/,' ====> ERROR !!! ORIFICE OUTLET AT JUNCTION ',I9,
     1' IS HIGHER THAN INLET')
 5459 FORMAT(/,' ====> ERROR !!! ORIFICE OUTLET AT JUNCTION ',A10,
     1' IS HIGHER THAN INLET')
 5480 FORMAT(//,
     +' *********************************************',/,
     +' *                 WEIR DATA                 *',/,
     +' *********************************************',//,
     *'       FROM        TO      LINK                 CREST      WEIR
     *      WEIR     DISCHARGE',/,
     *'   JUNCTION  JUNCTION    NUMBER      TYPE  HEIGHT(FT)   TOP(FT)
     *LENGTH(FT)   COEFFICIENT',/,
     *'   --------  --------    ------      ----  ----------   -------
     *----------   -----------')
 5481 FORMAT(//,
     +' *********************************************',/,
     +' *                 WEIR DATA                 *',/,
     +' *********************************************',//,
     *'       FROM        TO      LINK                 CREST      WEIR
     *      WEIR     DISCHARGE',/,
     *'   JUNCTION  JUNCTION    NUMBER      TYPE  HEIGHT (M)   TOP (M)
     *LENGTH (M)   COEFFICIENT',/,
     *'   --------  --------    ------      ----  ----------   -------
     *----------   -----------')
 5487 FORMAT(1X,4I10,F12.2,F10.2,F12.2,F14.4)
 5488 FORMAT(1X,3A10,I10,F12.2,F10.2,F12.2,F14.4)
 5490 FORMAT(/,' ====> ERROR !!! WEIR JUNCTION',I9,
     1       ' IS NOT CONTAINED IN JUNCTION DATA')
 5491 FORMAT(/,' ====> ERROR !!! WEIR JUNCTION ',A10,
     1       ' IS NOT CONTAINED IN JUNCTION DATA')
 5484 FORMAT(/,' ====> ERROR !!! STORAGE JUNCTION ',A10,
     1' IS NOT CONTAINED IN JUNCTION DATA')
 5494 FORMAT(/,' ====> ERROR !!! STORAGE JUNCTION ',I9,
     1' IS NOT CONTAINED IN JUNCTION DATA')
 5495 FORMAT(
     +'                                MAXIMUM OR          PEAK OR
     +  CROWN  ',/,
     +' STORAGE JUNCTION  JUNCTION  CONSTANT SURFACE   CONSTANT VOLUME
     +ELEVATION',/,
     +'   NUMBER OR NAME      TYPE     AREA (FT2)        (CUBIC FEET)
     +   (FT)  ',/,
     +'   --------------  --------  ----------------   ---------------
     +---------')
 5496 FORMAT(
     +'                                MAXIMUM OR          PEAK OR
     +  CROWN  ',/,
     +' STORAGE JUNCTION  JUNCTION  CONSTANT SURFACE   CONSTANT VOLUME
     +ELEVATION',/,
     +'   NUMBER OR NAME      TYPE     AREA  (M2)        (CUBIC MET.)
     +    (M)  ',/,
     +'   --------------  --------  ----------------   ---------------
     +---------')
 5560 FORMAT(//,
     +' *********************************************',/,
     +' *            OFF-LINE PUMP DATA             *',/,
     +' *********************************************',//,5X,
     *'         JUNCTIONS             PUMP RATES              VOLUME STA
     +GES   TOTAL WET WELL INITAL WET WELL',/,5X,
     *'      FROM      TO     # 1 CFS   # 2 CFS   # 3 CFS   # 1 FT3   #
     +2 FT3    VOLUME, FT3     VOLUME, FT3',/,5X,
     +'  --------   -------   -------   -------   -------   -------   --
     +-----    -----------     -----------')
 5561 FORMAT(//,
     +' *********************************************',/,
     +' *            OFF-LINE PUMP DATA             *',/,
     +' *********************************************',//,5X,
     *'         JUNCTIONS             PUMP RATES              VOLUME STA
     +GES   TOTAL WET WELL INITAL WET WELL',/,5X,
     *'      FROM      TO     # 1 CMS   # 2 CMS   # 3 CMS   # 1  M3   #
     +2  M3    VOLUME,  M3     VOLUME,  M3',/,5X,
     +'  --------   -------   -------   -------   -------   -------   --
     +-----    -----------     -----------')
 5565 FORMAT(//,
     +' *********************************************',/,
     +' *             IN-LINE PUMP DATA             *',/,
     +' *********************************************',//,5X,
     *'       JUNCTIONS                PUMP RATES                STAGES'
     +,/,5X,
     *'      FROM      TO     # 1 CFS   # 2 CFS   # 3 CFS    # 1 FT    #
     + 2 FT',/,5X,
     *'   -------   -------   -------   -------   -------   -------   --
     +-----')
 5566 FORMAT(//,
     +' *********************************************',/,
     +' *             IN-LINE PUMP DATA             *',/,
     +' *********************************************',//,5X,
     *'       JUNCTIONS                PUMP RATES                STAGES'
     +,/,5X,
     *'      FROM      TO     # 1 CMS   # 2 CMS   # 3 CMS    # 1  M    #
     + 2  M',/,5X,
     *'   -------   -------   -------   -------   -------   -------   --
     +-----')
 5570 FORMAT(//,
     +' *********************************************',/,
     +' *             PUMP CURVE DATA               *',/,
     +' *********************************************',//,5X,
     *'       JUNCTIONS      INITIAL DEPTH               PUMP RATES
     +          HEAD DIFFERENCES         ON    OFF',/,5X,
     *'      FROM      TO      IN WELL,  M   # 1 CFS   # 2 CFS   # 3 CFS
     +    # 1 FT    # 2 FT    # 3 FT   FEET   FEET',/,5X,
     *'   -------   -------    -----------   -------   -------   -------
     +   -------   -------  --------   ----  -----')
 5571 FORMAT(//,
     +' *********************************************',/,
     +' *             PUMP CURVE DATA               *',/,
     +' *********************************************',//,5X,
     *'       JUNCTIONS      INITIAL DEPTH               PUMP RATES
     +          HEAD DIFFERENCES         ON    OFF',/,5X,
     *'      FROM      TO      IN WELL,  M   # 1 CMS   # 2 CMS   # 3 CMS
     +    # 1  M    # 2  M    # 3  M  METER  METER',/,5X,
     *'   -------   -------    -----------   -------   -------   -------
     +   -------   -------  --------   ----  -----')
 5575 FORMAT(/,' ====> ERROR !!! CANNOT HAVE EQUAL VRATE (HEAD) VALUES 
     *ON PUMP CURVE (TO AVOID ZERO DIVIDE UPON INTERPOLATION).',/,
     *'       MUST PROVIDE AT LEAST MINIMAL HEAD DIFFERENCE.')
 5580 FORMAT(I4,'.',2I10,5F10.1,2F15.1)
 5581 FORMAT(I4,'.',2I10,5F10.1)
 5582 FORMAT(I4,'.',2I10,F15.1,6F10.1,2F7.1)
 5584 FORMAT(I4,'.',1X,A9,1X,A9,F15.1,5F10.1,2F15.1)
 5585 FORMAT(I4,'.',1X,A9,1X,A9,F15.1,5F10.1)
 5586 FORMAT(I4,'.',1X,A9,1X,A9,F15.1,6F10.1,2F7.1)
 5590 FORMAT(/,' ====> ERROR !!! PUMP JUNCTION ',I9,
     +     ' IS NOT CONTAINED IN THE JUNCTION DATA')
 5591 FORMAT(/,' ====> ERROR !!! PUMP JUNCTION ',A10,
     +     ' IS NOT CONTAINED IN THE JUNCTION DATA')
 5595 FORMAT(/,' ====> ERROR !!!  MORE THAN ONE PIPE IS INFLUENT TO OFF-
     .LINE PUMP JUNCTION ',I9)
 5596 FORMAT(/,' ====> ERROR !!!  MORE THAN ONE PIPE IS INFLUENT TO OFF-
     .LINE PUMP JUNCTION ',A10)
 5616 FORMAT(//,
     +' **************************************************',/,
     +' *          FREE OUTFALL DATA (DATA GROUP I1)     *',/,
     +' *         BOUNDARY CONDITION ON DATA GROUP J1    *'/,
     +' **************************************************',/)
 5620 FORMAT(' OUTFALL AT JUNCTION....',I10,
     +       ' HAS BOUNDARY CONDITION NUMBER...',I10)
 5621 FORMAT(' OUTFALL AT JUNCTION....',A10,
     +       ' HAS BOUNDARY CONDITION NUMBER...',I10)
 5630 FORMAT(/,' ====> ERROR !!!  FREE OUTFALL JUNCTION ',I9,' IS NOT',
     1       ' CONTAINED IN JUNCTION DATA')
 5631 FORMAT(/,' ====> ERROR !!!  FREE OUTFALL JUNCTION ',A10,' IS NOT',
     1       ' CONTAINED IN JUNCTION DATA')
 5656 FORMAT(//,
     +' ***********************************************',/,
     +' *    TIDE GATE OUTFALL DATA (DATA GROUP I2)   *',/,
     +' *      BOUNDARY CONDITION ON DATA GROUP J1    *'/,
     +' ***********************************************',/)
 5660 FORMAT(' OUTFALL AT JUNCTION....',I10,
     +       ' HAS BOUNDARY CONDITION NUMBER...',I10)
 5661 FORMAT(' OUTFALL AT JUNCTION... ',A10,
     +       ' HAS BOUNDARY CONDITION NUMBER...',I10)
 5662 FORMAT(/,' ====> ERROR !!!  TIDE GATE JUNCTION ',I9,' IS NOT',
     1       ' CONTAINED IN JUNCTION DATA')
 5663 FORMAT(/,' ====> ERROR !!!  TIDE GATE JUNCTION ',A10,' IS NOT',
     1       ' CONTAINED IN JUNCTION DATA')
 5665 FORMAT(//,
     +' **************************************************',/,
     +' *        INTERNAL CONNECTIVITY INFORMATION       *',/,
     +' **************************************************',/)
 5670 FORMAT('          CONDUIT     JUNCTION     JUNCTION',/,
     +       '          -------     --------     --------')
 5671 FORMAT('     CONDUIT      JUNCTION     JUNCTION',/,
     +       '     -------      --------     --------')
 5675 FORMAT(4X,I11,2I13)
 5676 FORMAT(/,' ====> ERROR !!! TOTAL NUMBER OF JUNCTIONS(INCLUDING WEI
     1RS) EXCEED PROGRAM DIMENSIONS, NEE =',I4)
 5677 FORMAT(/,' ====> ERROR !!! TOTAL NUMBER OF LINKS EXCEEDS PROGRAM D
     1IMENSIONS, NEE =',I4)
 5685 FORMAT(5X,A10,3X,A10,3X,A10)
 6010 FORMAT(/,' ====> EQUIVALENT PIPE INFORMATION FOR ORIFICE # ',I7,/,
     .       '       CONDUIT NUMBER..........................',I9,/,
     .       '       PIPE DIAMETER........................',F12.2,/,
     .       '       PIPE LENGTH..........................',F12.2,/,
     .       '       MANNINGS ROUGHNESS...................',F12.4,/,
     .       '       INVERT ELEVATION AT UPSTREAM END.....',F12.4,/,
     .       '       INVERT ELEVATION AT DOWNSTREAM END...',F12.4)
 6011 FORMAT(/,' ====> EQUIVALENT PIPE INFORMATION FOR ORIFICE # ',I7,/,
     .       '       CONDUIT NAME........................... ',A10,/,
     .       '       PIPE DIAMETER........................',F12.2,/,
     .       '       PIPE LENGTH..........................',F12.2,/,
     .       '       MANNINGS ROUGHNESS...................',F12.4,/,
     .       '       INVERT ELEVATION AT UPSTREAM END.....',F12.4,/,
     .       '       INVERT ELEVATION AT DOWNSTREAM END...',F12.4)
 6660 FORMAT(//,
     +' ***********************************************',/,
     +' *               WEIR OUTFALL DATA             *',/,
     +' *      BOUNDARY CONDITION ON DATA GROUP J1    *'/,
     +' ***********************************************',/)
 6665 FORMAT(' WEIR OUTFALL AT JUNCTION....',I10,
     +       ' HAS BOUNDARY CONDITION NUMBER...',I10)
 6670 FORMAT(' WEIR OUTFALL AT JUNCTION... ',A10,
     +       ' HAS BOUNDARY CONDITION NUMBER...',I10)
 6680 FORMAT(//,
     +' ***********************************************',/,
     +' *               PUMP OUTFALL DATA             *',/,
     +' *      BOUNDARY CONDITION ON DATA GROUP H1    *'/,
     +' ***********************************************',/)
C=======================================================================
      END

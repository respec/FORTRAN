      SUBROUTINE RHYDRO1
C	RUNOFF BLOCK
C	CALLED BY SUBROUTINE RUNOFF NEAR LINE 217
C=======================================================================
C     General Input Subroutine of the Runoff Block.
C     RHYDRO1 updated August 1992.
C     Updated 12/92 by WCH to include limit for groundwater
C       non-convergence messages (entered optionally on B2 line).
C     Call I/I routine, C. Moore, CDM, 8/93.
C     Metric correction, WCH (RED), 9/23/93.  UNDONE, RED, 12/31/93.
C     Option for zero evaporation during rainy time steps, WCH (CDM,
C       Chuck Moore), 10/5/93.  Also slight change to print-out for
C       IVAP parameter.
C     Metric correction, WCH (RED), 11/12/93, for print of QFULL.
C     Correction to print alphanumeric tributary subareas in linkage
C       table, WCH, 11/30/93.
C     Fix to use correct Format statement for evaporation in in. or mm
C       per day, WCH, 4/19/94.
C     Add check to be sure when reading F1 evap. data, WCH, 12/5/94.
C     Check for maximum number of connections to channel/pipe/inlets,
C       WCH, 12/11/96.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
      INCLUDE 'GRWTR.INC'
      INCLUDE 'NEW88.INC'
      INCLUDE 'NEW89.INC'
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CIM   MAXINF  C.Moore B. Cunningham CDM
      INCLUDE 'MAXINF.INC'
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C=======================================================================
      CHARACTER JUNIT(5)*10,GTYPE(4)*9,CTYPE(3)*12
CIM Modification to table to print 10 characters and digits of id's 4/99
      CHARACTER GOUT(20)*90,FOR1(2)*90,FOR2(2)*90,FOR3(2)*90,FOR4(2)*90
C#### WCH, 12/92
      DIMENSION G(8),GD(8),GR(8)
      DATA JUNIT/' Seconds',' Minutes',' Hours',' Days',' Yr/Mo/Dy'/
      DATA GTYPE/'Trapezoid',' Circular',
     1           '  Dummy  ','Parabolic'/
      DATA CTYPE/' Weir ','V-Notch Weir',' Orifice '/
      GOUT(1) = '('' Input   NAMEG: '''//
     1'3X,'' Drains                         '','
      GOUT(2) = '(''Sequen  Channel'''//
     1'3X,''   to     Channel  Width  Length '','
      GOUT(3) = '(''Number    ID # '''//
     1'3X,''  NGTO:    Type     (ft)   (ft)  '','
      GOUT(4) = '(''------  ------------------'''//
     1','' -------- ------ ------- '','
      GOUT(5) = '''  Invert  L Side   R Side  Intial     Max  Mann-   Fu
     +ll'')'
      GOUT(6) = '''   Slope   Slope    Slope   Depth   Depth   ings   Fl
     +ow'')'
      GOUT(7) = '''  (ft/ft) (ft/ft)  (ft/ft)   (ft)    (ft)    "N"  (cf
     +s)'')'
      GOUT(8) = '''  ------- -------- ------- ------- ------- -----  ---
     +--'')'
      GOUT(9) = '(''Number    ID # '''//
     1'3X,''  NGTO:    Type     (m)    (m)   '','
      GOUT(10)= '''  (m/m)   (m/m)    (m/m)     (m)     (m)    "N"   (cm
     +s)'')'
      GOUT(11)= '('' Input   NAMEG:  Drains   Type of     Crest '','
      GOUT(12)= '(''Sequen  Channel   to      Control      Elev.'','
      GOUT(13)= '(''Number    ID #   NGTO:   Structure     (ft) '','
      GOUT(14)= '(''------  --------------- -----------  ------ '','
      GOUT(15)= '''    Discharge   Spillway Width, Notch '')'
      GOUT(16)= '''  Coefficient  Angle, or Orifice area  '')'
      GOUT(17)= ''' (ft**1/2)/s)  (ft or ft**2 or degrees)'')'
      GOUT(18)= ''' ------------  ------------------------'')'
      GOUT(19)= '(''Number    ID #   NGTO:   Structure     (m) '','
      GOUT(20)= '''  (m**1/2)/s)   (m or m**2 or degrees)'')'
      SOURCE  = 'Runoff Block'
C=======================================================================
C     General information.
C=======================================================================
C>>>>>>>>>> READ DATA GROUP A1 <<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,TITLE(1)
      READ(N5,*,ERR=888) CC,TITLE(2)
      WRITE(N6,14)          TITLE(1),TITLE(2)
      TITLE(3) = TITLE(1)
      TITLE(4) = TITLE(2)
C=======================================================================
C>>>>>>>> READ DATA GROUP B1 <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,METRIC,ISNOW,NRGAG,INFILM,KWALTY,
     1                   IVAP,NHR,NMN,NDAY,MONTH,NYEAR,IVCHAN
      IF (NYEAR.LT.100) NYEAR = NYEAR+1900
      WRITE(N6,31) ISNOW,NRGAG
      IF(INFILM.EQ.0) WRITE(N6,32) INFILM
      IF(INFILM.EQ.1) WRITE(N6,33) INFILM
C   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      IF(INFILM.EQ.2) WRITE(N6,321) INFILM
      IF(INFILM.EQ.3) WRITE(N6,331) INFILM
C   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      IF(KWALTY.EQ.1) WRITE(N6,34) KWALTY
      IF(KWALTY.EQ.0) WRITE(N6,35) KWALTY
C#### WCH (CDM, C. MOORE), 10/5/93.  ADD OPTION FOR NO EVAP DURING RAIN.
      IF(IVAP.LT.0) THEN
c  if iivap > 0 then no evaporation during rainfall.
c           < 0 then evaporation is allowed during rainfall.
c  if abs(iivap) = 1 then evaporation is allowed from channels.
c                = 2 then evaporation is not allowed from channels.
           IIVAP = 1
           IF (IVCHAN.EQ.1) IIVAP = 2
           IVAP  = -IVAP
           ELSE
           IIVAP = -1
           IF (IVCHAN.EQ.1) IIVAP = -2
           ENDIF
      IF(IIVAP.GT.0) WRITE(N6,41)
      IF(IVCHAN.EQ.1) WRITE(N6,45)
C
      IF(IVAP.EQ.0) WRITE(N6,36)   IVAP
C#### WCH, 10/5/93.  MODIFY IVAP WRITE.
      IF(IVAP.GE.1.AND.IVAP.LE.3) WRITE(N6,37)   IVAP
      IF(IVAP.EQ.4) WRITE(N6,42) IVAP
      WRITE(N6,38) NHR,NMN
      TZERO = FLOAT(NHR) + FLOAT(NMN)/60.0
      WRITE (N6,39) TZERO
      TZERO  = TZERO * 3600.0
      TIMDAY = TZERO
      METRIC = METRIC + 1
      IF(METRIC.EQ.1) WRITE(N6,48) METRIC-1
      IF(METRIC.EQ.2) WRITE(N6,49) METRIC-1
C=======================================================================
C>>>>>>>> READ DATA GROUP B2 <<<<<<<<
C=======================================================================
      IPRNGW = 0
      READ(N5,*,ERR=888) CC,IPRN(1),IPRN(2),IPRN(3),IPRNGW
      WRITE(N6,40)          IPRN(1),IPRN(2),IPRN(3)
      IF(IPRNGW.EQ.0) IPRNGW = 10000
      WRITE(N6,9070) IPRNGW
C
      IPRN(4) = 1
      IPRN(5) = 1
      IPRN(6) = 1
      IPRN(7) = 1
      IF(IPRN(1).EQ.0) THEN
                       IPRN(4) = 0
                       IPRN(5) = 0
                       IPRN(6) = 0
                       IPRN(7) = 0
                       ENDIF
      IF(IPRN(1).GT.1) THEN
         I1 = IPRN(1)/1000
         J2 = IPRN(1) - I1*1000
         I2 = J2/100
         J3 = J2 - I2*100
         I3 = J3/10
         I4 = J3 - I3*10
         IF(I1.EQ.2.OR.I2.EQ.2.OR.I3.EQ.2.OR.I4.EQ.2) IPRN(4) = 0
         IF(I1.EQ.3.OR.I2.EQ.3.OR.I3.EQ.3.OR.I4.EQ.3) IPRN(5) = 0
         IF(I1.EQ.4.OR.I2.EQ.4.OR.I3.EQ.4.OR.I4.EQ.4) IPRN(6) = 0
         IF(I1.EQ.5.OR.I2.EQ.5.OR.I3.EQ.5.OR.I4.EQ.5) IPRN(7) = 0
         ENDIF
C=======================================================================
C>>>>>>>> READ DATA GROUP B3 <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,WET,WETDRY,DRY,LUNIT,LONG
C=======================================================================
C>>>>>>>> READ DATA GROUP B4 (IF PRESENT) <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'B4') THEN
                   BACKSPACE N5
                   READ(N5,*,ERR=888) CC,PCTZER,REGEN
                   ELSE
                   BACKSPACE N5
                   ENDIF
      IF(NDAY.LE.0)   NDAY   =  2
      IF(MONTH.LE.0)  MONTH  =  8
      IF(NYEAR.LE.0)  NYEAR  = 1941
      JULDAY = NYEAR*1000 + JDATE(NDAY,MONTH,NYEAR)
      IDATEZ = JULDAY
      CALL DATED
      NBD(1) = NYEAR
      NBD(2) = MONTH
      NBD(3) = NDAY
      NBD(4) = JHR
      NBD(5) = MINUTE
      NBD(6) = JSEC
      IF(NRGAG.EQ.0)    NRGAG  =    1
      IF(REGEN.LE.0.0)  REGEN  = 0.01
      IF(PCTZER.EQ.0.0) PCTZER = 25.0
C=======================================================================
      WRITE (N6,162)  MONTH,NDAY,NYEAR
      WRITE (N6,160)  WET,DRY,WETDRY
      WRITE (N6,165)  LONG,JUNIT(LUNIT+1)
      WRITE (N6,174)  PCTZER
C   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      IF(INFILM.EQ.0.OR.INFILM.EQ.2) WRITE(N6,180) REGEN
cim      IF(INFILM.EQ.0) WRITE(N6,180) REGEN
C   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C=======================================================================
      IF(LUNIT.EQ.1)  LONG = LONG*60.0
      IF(LUNIT.EQ.2)  LONG = LONG*3600.0
      IF(LUNIT.EQ.3)  LONG = LONG*86400.0
      IF(LUNIT.EQ.4)  THEN
                      JEND  = IFIX(LONG)
                      MYEAR = JEND/10000
                      MDAY  = JEND - MYEAR*10000
                      IF (MYEAR.LT.100) MYEAR = MYEAR + 1900
                      MMNTH = MDAY/100
                      MDAY  = MDAY - MMNTH*100
                      JEND  = 1000*MYEAR + JDATE(MDAY,MMNTH,MYEAR)
                      TMD   = 86400.0
                      CALL NTIME(JEND,TMD,TDIFF)
                      LONG  = TDIFF
                      ENDIF
C=======================================================================
C     Read Snow Input Data Groups.
C=======================================================================
      IF(ISNOW.GT.0) THEN
                     WRITE(*,9050)
                     CALL SNOWIN(0)
                     ENDIF
C=======================================================================
C     Rainfall Intensity Histogram.
C=======================================================================
      WRITE(*,9060)
      CALL MKRAIN
C=======================================================================
C>>>>>>>> Read Data Group F1 If IVAP >= 1 <<<<<<<<
C=======================================================================
      IF(IVAP.EQ.0) THEN
                    CDEF     = 0.1
                    IF(METRIC.EQ.2) CDEF = 3.0
                    DO 535 I = 1,12
  535               VAP(I)   = CDEF
                    IF(METRIC.EQ.1) WRITE(N6,550) (VAP(I),I=1,12)
                    IF(METRIC.EQ.2) WRITE(N6,555) (VAP(I),I=1,12)
                    ENDIF
      IF(IVAP.EQ.1) THEN
                    READ(N5,*,ERR=888) CC,(VAP(I),I=1,12)
C#### WCH, 12/5/94.  CHECK FOR PRESENCE OF F1 LINE.
                    IF(CC.NE.'F1') THEN
                         CC = 'F1'
                         GO TO 888
                         ENDIF
C#### WCH, 4/19/94.  USE CORRECT FORMAT STATEMENTS FOR IN OR MM/DAY.
                    IF(METRIC.EQ.1) WRITE(N6,550) (VAP(I),I=1,12)
                    IF(METRIC.EQ.2) WRITE(N6,555) (VAP(I),I=1,12)
                    ENDIF
      IF(IVAP.EQ.2) THEN
                    READ(N5,*,ERR=888) CC,(VAP(I),I=1,12)
C#### WCH, 12/5/94.  CHECK FOR PRESENCE OF F1 LINE.
                    IF(CC.NE.'F1') THEN
                         CC = 'F1'
                         GO TO 888
                         ENDIF
                    IF(METRIC.EQ.1) WRITE(N6,551) (VAP(I),I=1,12)
                    IF(METRIC.EQ.2) WRITE(N6,556) (VAP(I),I=1,12)
                    DO 540 I = 1,12
  540               VAP(I)   = VAP(I)/PDAYS(I)
                    ENDIF
      IF(IVAP.EQ.3) THEN
                    READ(N5,*,ERR=888) CC,NVAP(1),NVAP(2)
                    IF (NVAP(1).LE.100) NVAP(1) = NVAP(1) + 1900
C#### WCH, 12/5/94.  CHECK FOR PRESENCE OF F1 LINE.
                    IF(CC.NE.'F1') THEN
                         CC = 'F1'
                         GO TO 888
                         ENDIF
                    IF(NVAP(2).GT.600) CALL ERROR(150)
                    IF(NVAP(1).LE.00)  CALL ERROR(151)
C                   IF(NVAP(1).GT.100) CALL ERROR(152)
                    DO 5300 I  = 1,NVAP(2),12
                    KK         = I - 1
                    READ(N5,*,ERR=888) CC,(VAP(K+KK),K=1,12)
5300                CONTINUE
                    IF(METRIC.EQ.1) WRITE(N6,5505)
                    IF(METRIC.EQ.2) WRITE(N6,5506)
                    ISTYR      = NVAP(1) - 1
                    DO 5400 I  = 1,NVAP(2),12
                    ISTYR      = ISTYR   + 1
                    KK         = I       - 1
                    WRITE(N6,5510) ISTYR,(VAP(K+KK),K=1,12)
5400                CONTINUE
                    DO 5600 I  = 1,NVAP(2),12
                    DO 5500 K  = 1,12
                    KK         = K - 1
5500                VAP(I+KK)  = VAP(I+KK)/PDAYS(K)/CMET(7,METRIC)
5600                CONTINUE
                    ENDIF
C=======================================================================
C     Convert from in/day or mm/day to ft/sec.
C=======================================================================
      IF(IVAP.LE.2) THEN
                    DO 560 I = 1,12
560                 VAP(I)   = VAP(I)/CMET(7,METRIC)
                    ENDIF
      FOR1(1) = GOUT(1)
      FOR1(2) = GOUT(5)
      FOR2(1) = GOUT(2)
      FOR2(2) = GOUT(6)
      FOR4(1) = GOUT(4)
      FOR4(2) = GOUT(8)
      IF(METRIC.EQ.1) THEN
                      FOR3(1) = GOUT(3)
                      FOR3(2) = GOUT(7)
                      ELSE
                      FOR3(1) = GOUT(9)
                      FOR3(2) = GOUT(10)
                      ENDIF
C#######################################################################
C####   C. Moore, 8/93.  Add option to read I/I data.
C#######################################################################
C>>>>>>>> Read F3 and F4 lines <<<<<<<<
C=======================================================================
C     Look for new F3 line with first call to RDIIREAD
C=======================================================================
      CALL RDIIREAD(0,0)
C=======================================================================
C     Look for new F4 lines with second call to RDIIREAD
C=======================================================================
      CALL RDIIREAD(1,0)
C=======================================================================
C     Channel and Pipe Data.
C=======================================================================
      DO 562 I = 1,8
      GD(I)    = 0.0
  562 GR(I)    = 1.0
      NOGG     = 0
      N        = 0
      WRITE(*,9040)
      DO 660 I = 1,900
      N        = N + 1
C=======================================================================
C>>>>>>>> Read Data Groups G1 and G2 <<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC
      IF(CC.NE.'G1'.AND.CC.NE.'G2') THEN
                     BACKSPACE N5
                     IF(I.EQ.1) WRITE(N6,600)
                     GO TO 670
                     ENDIF
      IF(CC.EQ.'G1') THEN
                     BACKSPACE N5
                     IF(JCE.EQ.0) READ(N5,*,ERR=888)
     +                                 CC,NAMEG(N),NGTO(N),NP,G
                     IF(JCE.EQ.1) READ(N5,*,ERR=888)
     +                                 CC,KAMEG(N),KGTO(N),NP,G
                     IF(NP.GT.4) THEN
                                 READ(N5,*,ERR=888) CC,WTYPE(N),
     +                                WELEV(N),WDIS(N),SPILL(N)
                                 IF(CC.NE.'G2') THEN
                                    IF(JCE.EQ.0) WRITE(N6,8900) NAMEG(N)
                                    IF(JCE.EQ.1) WRITE(N6,8901) KAMEG(N)
                                    CALL ERROR(200)
                                    ENDIF
                                 IF(NP.LE.6) NP = NP - 4
                                 IF(NP.EQ.7) NP =  4
                                 ENDIF
                     ENDIF
      IF(JCE.EQ.0)        THEN
      IF(NAMEG(N).EQ.-1)  THEN
                          DO 610 J = 1,8
  610                     IF(G(J).NE.0.0) GR(J)=G(J)
                          N = N - 1
                          GO TO 660
                          ENDIF
      IF(NAMEG(N).EQ.-2)  THEN
                          DO 640 J = 1,8
  640                     IF(G(J).NE.0.0) GD(J)=G(J)
                          N = N - 1
                          GO TO 660
                          ENDIF
                          ENDIF
      IF(JCE.EQ.1)        THEN
      IF(KAMEG(N).EQ.'-1')THEN
                          DO 810 J = 1,8
  810                     IF(G(J).NE.0.0) GR(J)=G(J)
                          N = N - 1
                          GO TO 660
                          ENDIF
      IF(KAMEG(N).EQ.'-2')THEN
                          DO 840 J = 1,8
  840                     IF(G(J).NE.0.0) GD(J)=G(J)
                          N = N - 1
                          GO TO 660
                          ENDIF
                          ENDIF
      IF(NP.NE.3) THEN
                   DO 605 IG = 1,8
                   IF(G(IG).EQ.0.0) G(IG) = GD(IG)
  605              G(IG)                  = GR(IG) * G(IG)
                   ENDIF
C=======================================================================
C     Calculate Qfull for Channel or Pipe.
C=======================================================================
      IF(NP.NE.3) THEN
      IF(G(6).LE.0.0) G(6) = 0.014
      IF(NP.EQ.1) THEN
             AX = 0.5*(G(4)+G(5))*G(7)**2.0+G(1)*G(7)
             PW = SQRT(G(4)**2.0+1.0)*G(7)+SQRT(G(5)**2.0+1.0)*G(7)+G(1)
             ENDIF
      IF(NP.EQ.2)  THEN
                   SIN2D0  = 0.5*SIN(2.0*2.62)
                   AX      = G(1)**2*(2.62-SIN2D0)/4.0
                   PW      = G(1)*2.62
                   ENDIF
      IF(NP.EQ.4)     THEN
      IF(G(7).GT.0.0) THEN
                      WIDTH = G(1)
                      AX    = 0.66666667*WIDTH*G(7)
                      X     = WIDTH/2.0
                      AA    = WIDTH**4.0/(64.0*G(7)**2.0)
                      PW    = 8.0*G(7)/WIDTH**2.0*(X*SQRT(AA+X**2.0) +
     +                                     AA*LOG(X+SQRT(AA+X**2.0)) -
     +                                     AA*LOG(SQRT(AA)))
                     ELSE
                     AX    = 0.0
                     PW    = 0.001
                     ENDIF
                     ENDIF
      IF(AX.LT.0.001) AX   = 0.0
      IF(PW.LE.0.001) PW   = 0.001
      RAD                  = AX/PW
      GCON(N)              = 1.486/G(6)*SQRT(G(3))
                     QFULL = 0.0
      IF(RAD.GT.0.0) QFULL = GCON(N)*AX*RAD**0.666666667
      ELSE
      QFULL                = 0.0
      ENDIF
C#### WCH (RED), 11/12/93.
      IF(METRIC.EQ.2) QFULL = QFULL/1.486
C=======================================================================
C     Print Channel/Pipe data.
C=======================================================================
      IF(IPRN(4).EQ.0)   THEN
      IF(N.EQ.1.OR.MOD(N,50).EQ.0) THEN
                                   WRITE(N6,620)
                                   WRITE(N6,FOR1)
                                   WRITE(N6,FOR2)
                                   WRITE(N6,FOR3)
                                   WRITE(N6,FOR4)
                                   ENDIF
      IF(JCE.EQ.0) WRITE(N6,630) N,NAMEG(N),NGTO(N),GTYPE(NP),
     +             G(1),G(2),G(3),G(4),G(5),G(8),G(7),G(6),QFULL
      IF(JCE.EQ.1) WRITE(N6,631) N,KAMEG(N),KGTO(N),GTYPE(NP),
     +             G(1),G(2),G(3),G(4),G(5),G(8),G(7),G(6),QFULL
      ENDIF
C=======================================================================
C     Transfer data and convert units.
C=======================================================================
      NPG(N)    = NP
      GWIDTH(N) = G(1)*CMET(1,METRIC)
      GLEN(N)   = G(2)*CMET(1,METRIC)
      GS1(N)    = G(4)
      GS2(N)    = G(5)
      DFULL(N)  = G(7)*CMET(1,METRIC)
      IF(G(8).LE.0.0.AND.NP.EQ.4) G(8) = 0.0001
      GDEPTH(N) = G(8)*CMET(1,METRIC)
C=======================================================================
C     For circular pipes, Dfull is in radians.
C     2.62=Half angle of wetted perimeter at maximum flow.
C=======================================================================
      IF(NP.EQ.2) THEN
                  DFULL(N)  = 2.62
                  IF(GDEPTH(N).GE.GWIDTH(N)/2.0) THEN
                               TH9 = GDEPTH(N) -  GWIDTH(N)/2.0
                               GDEPTH(N) =  3.1415927
     +                                    - ACOS(TH9/(GWIDTH(N)/2.0))
                               ENDIF
                  IF(GDEPTH(N).LT.GWIDTH(N)/2.0) THEN
                               TH9 =  GWIDTH(N)/2.0 - GDEPTH(N)
                               GDEPTH(N) =  ACOS(TH9/(GWIDTH(N)/2.0))
                               ENDIF
                  ENDIF
      NOGG = 1
  660 CONTINUE
  670 NOG = N-1
C=======================================================================
C     Write control structure data.
C=======================================================================
      IF(IPRN(4).EQ.0) THEN
      FOR1(1) = GOUT(11)
      FOR1(2) = GOUT(15)
      FOR2(1) = GOUT(12)
      FOR2(2) = GOUT(16)
      FOR4(1) = GOUT(14)
      FOR4(2) = GOUT(18)
      IF(METRIC.EQ.1) THEN
                      FOR3(1) = GOUT(13)
                      FOR3(2) = GOUT(17)
                      ELSE
                      FOR3(1) = GOUT(19)
                      FOR3(2) = GOUT(20)
                      ENDIF
      I        = 0
      DO 680 N = 1,NOG
      IF(WTYPE(N).EQ.-1) GO TO 680
      I = I + 1
      IF(I.EQ.1.OR.MOD(I,50).EQ.0) THEN
                                   WRITE(N6,625)
                                   WRITE(N6,FOR1)
                                   WRITE(N6,FOR2)
                                   WRITE(N6,FOR3)
                                   WRITE(N6,FOR4)
                                   ENDIF
      N1 = WTYPE(N) + 1
C#### WCH (RED), 9/93.  METRIC CORRECTION FOR WELEV.
C#### RED (WCH), 12/31/93.  UNDO CORRECTION.  UNNECESSARY BECAUSE
C####                       CONVERTED WELEV AFTER WRITE STATEMENTS.
      IF(JCE.EQ.0) WRITE(N6,635) N,NAMEG(N),NGTO(N),
     +             CTYPE(N1),WELEV(N),WDIS(N),SPILL(N)
      IF(JCE.EQ.1) WRITE(N6,636) N,KAMEG(N),KGTO(N),
     +             CTYPE(N1),WELEV(N),WDIS(N),SPILL(N)
C#######################################################################
C MOVE NEXT THREE STATEMENTS TO HERE TO CONVERT PARAMETERS AFTER
C  PRINT-OUT. WCH, 8/28/92
C#######################################################################
      WELEV(N) = WELEV(N)*CMET(1,METRIC)
      IF(WTYPE(N).EQ.2) SPILL(N) = SPILL(N)*CMET(1,METRIC)**2.0
C#######################################################################
C CORRECT ANGLE (SPILL) TO INPUT UNITS OF DEGREES.  WCH, 8/28/92
C#######################################################################
      IF(WTYPE(N).EQ.1) SPILL(N) =TAN(SPILL(N)/2.0*3.14159/180)
 680  CONTINUE
      ENDIF
C=======================================================================
C     Set up channel connectivity tables.
C=======================================================================
      INLETS = 0
      IF(NOG.GT.0) THEN
                   DO 720 N     = 1,NOG
                   NN           = NOG + INLETS
                   DO 690 NGOTO = 1,NN
                   IF(JCE.EQ.0.AND.NGTO(N).EQ.NAMEG(NGOTO)) GO TO 700
                   IF(JCE.EQ.1.AND.KGTO(N).EQ.KAMEG(NGOTO)) GO TO 700
  690              CONTINUE
C=======================================================================
C                  Create dummy channels as needed
C=======================================================================
                   INLETS = INLETS + 1
                   NGOTO  = NOG    + INLETS
                   IF(NGOTO.GT.NG) CALL ERROR(13)
                   IF(JCE.EQ.0) NAMEG(NGOTO) = NGTO(N)
                   IF(JCE.EQ.1) KAMEG(NGOTO) = KGTO(N)
                   NPG(NGOTO)    = 3
                   NGTOI(INLETS) = NGOTO
  700              CONTINUE
                   DO 710 J = 1,NCP
                   IF(NGTOG(J,NGOTO).GT.0) GO TO 710
                   NGTOG(J,NGOTO) = N
                   GO TO 720
  710              CONTINUE
C#### WCH, 12/11/96.  ADD ERROR MESSAGE HERE IF EXCEED NCP CONNECTIONS.
                   IF(JCE.EQ.0) WRITE(N6,8910) NCP,NAMEG(NGOTO),NAMEG(N)
                   IF(JCE.EQ.1) WRITE(N6,8911) NCP,KAMEG(NGOTO),KAMEG(N)
                   WRITE (N6,8912) NCP
                   WRITE (*,8912) NCP
                   STOP 'See error message in output file.'
  720              CONTINUE
                   ENDIF
C=======================================================================
C     Read subcatchment information from Subroutine CATCH.
C=======================================================================
      WRITE(*,9010)
      CALL CATCH(TRIBA)
C=======================================================================
C     Set up connectivity tables.
C=======================================================================
      DO 940 N     = 1,NOW
      NN           = NOG + INLETS
      DO 910 NGOTO = 1,NN
      IF(JCE.EQ.0.AND.NGTO(N).EQ.NAMEG(NGOTO)) GO TO 920
      IF(JCE.EQ.1.AND.KGTO(N).EQ.KAMEG(NGOTO)) GO TO 920
  910 CONTINUE
C=======================================================================
C     Identify additional inlets.
C=======================================================================
      INLETS        = INLETS+1
      NGOTO         = NOG + INLETS
      IF(JCE.EQ.0) NAMEG(NGOTO)  = NGTO(N)
      IF(JCE.EQ.1) KAMEG(NGOTO)  = KGTO(N)
      NPG(NGOTO)    = 3
      NGTOI(INLETS) = NGOTO
      IF(NGOTO.GT.NG) CALL ERROR(13)
  920 CONTINUE
C=======================================================================
C     Channel connection.
C=======================================================================
      DO 930 J = 1,NCP
      IF(NWTOG(J,NGOTO).GT.0) GO TO 930
      NWTOG(J,NGOTO) = N
      GO TO 940
  930 CONTINUE
C#### WCH, 12/11/96.  ADD ERROR MESSAGE HERE IF EXCEED NCP CONNECTIONS.
      IF(JCE.EQ.0) WRITE(N6,8920) NCP,NAMEG(NGOTO),NAMEW(N)
      IF(JCE.EQ.1) WRITE(N6,8921) NCP,KAMEG(NGOTO),KAMEW(N)
      WRITE (N6,8912) NCP
      WRITE (*,8912) NCP
      STOP 'See error message in output file.'
  940 CONTINUE
C=======================================================================
C     Print connectivity summary.
C=======================================================================
      WRITE(N6,950)
      IF(NOG.GT.0) THEN
                   WRITE(N6,960)
                   DO 1100 J = 1,NOG
                   IF(MOD(J,25).EQ.0) THEN
                                      WRITE(N6,950)
                                      WRITE(N6,960)
                                      ENDIF
                   DO 990 N = 1,NCP
                   IF(NGTOG(N,J)) 980,1000,980
  980              INUM    = NGTOG(N,J)
                   IF(JCE.EQ.0) NGTO(N) = NAMEG(INUM)
                   IF(JCE.EQ.1) KGTO(N) = KAMEG(INUM)
  990              CONTINUE
 1000              N = N-1
                   IF(JCE.EQ.0.AND.N.LE.0) WRITE(N6,1030) NAMEG(J)
                   IF(JCE.EQ.1.AND.N.LE.0) WRITE(N6,1031) KAMEG(J)
                   IF(JCE.EQ.0.AND.N.GT.0) WRITE(N6,1170)
     +                                     NAMEG(J),(NGTO(K),K=1,N)
                   IF(JCE.EQ.1.AND.N.GT.0) WRITE(N6,1171)
     +                                     KAMEG(J),(KGTO(K),K=1,N)
                   DO 1060 N = 1,NCP
                   IF(NWTOG(N,J)) 1050,1070,1050
 1050              INUM    = NWTOG(N,J)
                   IF(JCE.EQ.0) NGTO(N) = NAMEW(INUM)
                   IF(JCE.EQ.1) KGTO(N) = KAMEW(INUM)
 1060              CONTINUE
 1070              N = N-1
                   IF(N.EQ.0) WRITE(N6,1090)
                   IF(JCE.EQ.0.AND.N.GT.0) WRITE(N6,1095)(NGTO(K),K=1,N)
                   IF(JCE.EQ.1.AND.N.GT.0) WRITE(N6,1096)(KGTO(K),K=1,N)
 1100              CONTINUE
                   ENDIF
      WRITE(N6,1120)
C-----------------------------------------------------------------------
      DO 1180 I = 1,INLETS
      N         = NGTOI(I)
      JG        = 0
      JW        = 10
      DO 1160 J = 1,NCP
      IF(NGTOG(J,N).NE.0) THEN
                          JG       = JG+1
                          INUM     = NGTOG(J,N)
                          IF(JCE.EQ.0) NGTO(JG) = NAMEG(INUM)
                          IF(JCE.EQ.1) KGTO(JG) = KAMEG(INUM)
                          ENDIF
      IF(NWTOG(J,N).NE.0) THEN
                          JW       = JW+1
                          INUM     = NWTOG(J,N)
                          IF(JCE.EQ.0) NGTO(JW) = NAMEW(INUM)
                          IF(JCE.EQ.1) KGTO(JW) = KAMEW(INUM)
                          ENDIF
 1160 CONTINUE
      IF(JCE.EQ.0.AND.JG.EQ.0) WRITE(N6,1030) NAMEG(N)
      IF(JCE.EQ.1.AND.JG.EQ.0) WRITE(N6,1031) KAMEG(N)
      IF(JCE.EQ.0.AND.JG.GT.0) WRITE(N6,1170) NAMEG(N),(NGTO(J),J=1,JG)
      IF(JCE.EQ.1.AND.JG.GT.0) WRITE(N6,1171) KAMEG(N),(KGTO(J),J=1,JG)
      IF(JW.EQ.10) WRITE(N6,1090)
      IF(JCE.EQ.0.AND.JW.GT.10) WRITE(N6,1095) (NGTO(J),J=11,JW)
C#### WCH, 11/30/93.  CHANGE NGTO TO KGTO HERE.
      IF(JCE.EQ.1.AND.JW.GT.10) WRITE(N6,1096) (KGTO(J),J=11,JW)
 1180 CONTINUE
C=======================================================================
C     Read information to control inlets saved and printed.
C=======================================================================
      NSAVE     = INLETS
      DO 1190 J = 1,INLETS
      N         = NGTOI(J)
      IF(JCE.EQ.0) ISAVE(J) = NAMEG(N)
 1190 IF(JCE.EQ.1) KSAVE(J) = KAMEG(N)
      IF(JCE.EQ.0) WRITE(N6,1200) INLETS,(ISAVE(K),K=1,INLETS)
      IF(JCE.EQ.1) WRITE(N6,1201) INLETS,(KSAVE(K),K=1,INLETS)
C=======================================================================
C     Set up header for subsurface information to be graphed.
C=======================================================================
C     IF(NSVGW.GT.0) THEN
C                    LKOUN  = 0
C                    DO 1206 JF = 1,NOGWSC
C                    IF(NSCSFG(JF).EQ.0) GO TO 1206
C                    LKOUN         = LKOUN+1
C                    NSSVFG(LKOUN) = NSCSFG(JF)
C1206                CONTINUE
C                    ENDIF
C=======================================================================
C     Snow melt data for each catchment.
C=======================================================================
      IF(ISNOW.GT.0) THEN
                     WRITE(*,9030)
                     CALL SNOWIN(1)
                     ENDIF
      RETURN
  888 CALL IERROR
C=======================================================================
   14 FORMAT(//,20X,A80,//,20X,A80,/)
   31 FORMAT(//,1X,'Snowmelt parameter - ISNOW.......................',
     2 T55,I5,// ,1X,'Number of rain gages - NRGAG.....................'
     4 ,T55,I5)
   32 FORMAT(/,' Horton infiltration equation used - INFILM.......',
     1 T55,I5)
   33 FORMAT (/,1X,'Green-Ampt infiltration equation used - INFILM...',
     1 T55,I5)
C   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  321 FORMAT(/,' Horton infiltration equation used - INFILM.......',
     1 T55,I5,/,
     2' Maximum infiltration volume is limited to RMAXINF input on ',
     3' subcatchment lines.  Infiltration volume regenerates during ',
     4' non rainfall periods.')
  331 FORMAT (/,1X,'Green-Ampt infiltration equation used - INFILM...',
     1 T55,I5,/,
     2' Maximum infiltration volume is limited to RMAXINF input on ',
     3' subcatchment lines.  Does not regenerate. ')
C   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   34 FORMAT(/,1X,'Quality is simulated - KWALTY....................',
     1 T55,I5)
   35 FORMAT (/,1X,'Quality is not simulated - KWALTY................',
     1 T55,I5)
   36 FORMAT(/,1X,'Default evaporation rate used - IVAP.............',
     1  T55,I5)
   37 FORMAT(/,1X,'Read evaporation data on line(s) F1 (F2) - IVAP..',
     1  T55,I5)
   38 FORMAT(/,1X,'Hour of day at start of storm - NHR..............',
     1 T55,I5,//,1X,'Minute of hour at start of storm - NMN...........',
     2 T55,I5)
   39 FORMAT(/,' Time TZERO at start of storm (hours).............',
     1 F9.3)
   40 FORMAT(/,' Runoff input print control...',T55,I5,
     1      //,' Runoff graph plot control....',T55,I5,
     2      //,' Runoff output print control..',T55,I5)
C#### WCH, 10/5/93.
   41 FORMAT(/,1X,'IVAP is negative.  Evaporation will be set to zero'
     1 ,/,'     during time steps with rainfall.')
   45 FORMAT(/,1x,'IVCHAN equals 1.  Evaporation is not allowed from '
     1,' channels or conduits.')
C#### WCH, 10/5/93.
   42 FORMAT(/,1X,'Input evaporation data on NSCRAT(3) from the',/,
     1           ' Temp Block - IVAP................................',
     2  T55,I5)
   48 FORMAT(/,' Use U.S. Customary units for most I/O - METRIC...',
     1 T55,I5)
   49 FORMAT(/,' Use Metric units for I/O - METRIC................',
     1 T55,I5,/,'  ===> Ft-sec units used in all internal computations')
  162 FORMAT (/,' Month, day, year of start of storm is: ',
     1 9X,2(I2,'/'),I4)
  160 FORMAT(/,1X,
     +  'Wet time step length (seconds).......',T50,F10.0,//,1X,
     +  'Dry time step length (seconds).......',T50,F10.0,//,1X,
     +  'Wet/Dry time step length (seconds)...',T50,F10.0)
  165 FORMAT(/,1X,'Simulation length is......',T50,F10.1,A10)
  174 FORMAT(/,' Percent of impervious area with zero detention',
     .       ' depth',1X,F5.1)
  180 FORMAT(/,1X,'Horton infiltration model being used',/,' Rate for
     1regeneration of infiltration = REGEN * DECAY'
     1,/,' DECAY is read in for each subcatchment',/,
     2   ' REGEN = ............................................'
     3   ,T50,F10.5,/,1H1)
  550 FORMAT(/,
     +'   #############################',/,
     +'   #        Data Group F1      #',/,
     +'   # Evaporation Rate (in/day) #',/,
     +'   #############################',//,
     1'  JAN.  FEB.  MAR.  APR.  MAY   JUN.  JUL.  AUG.  SEP.  OCT.  NOV
     +.  DEC.',/,
     +'  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---
     +-  ----',/,12F6.2)
  551 FORMAT(/,
     +'   ##############################',/,
     +'   #        Data Group F1       #',/,
     +'   # Evaporation Rate (in/month)#',/,
     +'   ##############################',//,
     1'  JAN.  FEB.  MAR.  APR.  MAY   JUN.  JUL.  AUG.  SEP.  OCT.  NOV
     +.  DEC.',/,
     +'  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---
     +-  ----',/,12F6.2)
  555 FORMAT(/,
     +'   #############################',/,
     +'   #        Data Group F1      #',/,
     +'   # Evaporation Rate (mm/day) #',/,
     +'   #############################',//,
     1'  JAN.  FEB.  MAR.  APR.  MAY   JUN.  JUL.  AUG.  SEP.  OCT.  NOV
     +.  DEC.',/,
     +'  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---
     +-  ----',/,12F6.2)
  556 FORMAT(/,
     +'   ###############################',/,
     +'   #        Data Group F1        #',/,
     +'   # Evaporation Rate (mm/month) #',/,
     +'   ###############################',//,
     1'  JAN.  FEB.  MAR.  APR.  MAY   JUN.  JUL.  AUG.  SEP.  OCT.  NOV
     +.  DEC.',/,
     +'  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---
     +-  ----',/,12F6.2)
  600 FORMAT(//,
     +' ************************************************',/,
     +' *          No Channel or Pipe Network          *',/,
     +' ************************************************',//)
  620 FORMAT(/,1H1,/
     +' *********************************************************',/,
     +' *         C H A N N E L  A N D  P I P E  D A T A        *',/,
     +' *********************************************************',/)
  625 FORMAT(/,1H1,/
     +' **********************************************************',/,
     +' *       C O N T R O L  S T R U C T U R E   D A T A       *',/,
     +' **********************************************************',/)
  630 FORMAT(1X,I3,1X,I10,1X,I10,1X,A9,F6.1,F8.1,F10.4,
     1                             F9.4,F8.4,2F8.1,F8.4,1PE9.2)
  631 FORMAT(1X,I3,1X,A10,1X,A10,1X,A9,F6.1,F8.1,F10.4,
     1                             F9.4,F8.4,2F8.1,F8.4,1PE9.2)
  635 FORMAT(1X,I3,1X,I10,1X,I10,1X,A12,F7.1,F12.3,5X,F12.4)
  636 FORMAT(1X,I3,1X,A10,1X,A10,1X,A12,F7.1,F12.3,5X,F12.4)
  950 FORMAT(/,1H1,/
     +' *********************************************************',/,
     +' *     Arrangement of Subcatchments and Channel/Pipes    *',/,
     +' *********************************************************')
  960 FORMAT(//,4X,'Channel',/,4X,'or Pipe')
 1030 FORMAT(1X,I10,5X,'No Tributary Channel/Pipes')
 1031 FORMAT(1X,A10,5X,'No Tributary Channel/Pipes')
 1090 FORMAT(    16X,'No Tributary Subareas.....')
 1095 FORMAT(    16X,'Tributary Subareas........',5(1X,I10))
 1096 FORMAT(    16X,'Tributary Subareas........',5(1X,A10))
 1120 FORMAT(//,2X,'   INLET')
 1170 FORMAT(1X,I10,5X,'Tributary Channel/Pipes...',5(1X,I10))
 1171 FORMAT(1X,A10,5X,'Tributary Channel/Pipes...',5(1X,A10))
 1200 FORMAT(//,
     1' ***********************************************************',/,
     2' * Hydrographs will be stored for the following',I4,' INLETS *',
     3/,' ***********************************************************',
     4 /,(5X,6I10))
 1201 FORMAT(//,
     1' ***********************************************************',/,
     2' * Hydrographs will be stored for the following',I4,' INLETS *',
     3/,' ***********************************************************',
     4 /,(1X,6A10))
 5505 FORMAT(/,
     1'   ###############################',/,
     2'   #        Data Group F2        #',/,
     3'   # Evaporation Rate (in/month) #',/,
     4'   ###############################',//,
     5'  Year  Jan.  Feb.  Mar.  Apr.  May   June  July  Aug.  Sep.  Oct
     6.  Nov. Dec.',/,
     7'  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---
     8   ---- ----')
 5506 FORMAT(/,
     +'   ###############################',/,
     +'   #        Data Group F2        #',/,
     +'   # Evaporation Rate (mm/month) #',/,
     +'   ###############################',//,
     1'  Year  Jan.  Feb.  Mar.  Apr.  May   June  July  Aug.  Sep.  Oct
     2.  Nov. Dec.',/,
     1'  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---
     2-  ---- ----')
 5510 FORMAT(I6,12F6.2)
 8900 FORMAT(/,' ===> Error !!  There was no G2 data line for Channel/Pi
     +pe number ',I10,/)
 8901 FORMAT(/,' ===> Error !!  There was no G2 data line for Channel/Pi
     +pe name ',A10,/)
C#### WCH, 12/11/96.
 8910 FORMAT(/,' ERROR! MAXIMUM OF',I3,' CHANNEL/PIPE CONNECTIONS TO ANO
     1THER',/,' CHANNEL/PIPE/INLET EXCEEDED FOR CH/PIPE/INLET',I10,/,
     2' BY CHANNEL/PIPE',I10,'. SUGGEST USING DUMMY PIPE TO ALLOW MORE C
     3ONNECTIONS.')
 8911 FORMAT(/,' ERROR! MAXIMUM OF',I3,' CHANNEL/PIPE CONNECTIONS TO ANO
     1THER',/,' CHANNEL/PIPE/INLET EXCEEDED FOR CH/PIPE/INLET',A10,/,
     2' BY CHANNEL/PIPE',A10,'. SUGGEST USING DUMMY PIPE TO ALLOW MORE C
     3ONNECTIONS.')
 8912 FORMAT(' Run stopped due to exceeding maximum no. connections, NCP
     1 =',I3)
 8920 FORMAT(/,' ERROR! MAXIMUM OF',I3,' SUBCATCHMENT CONNECTIONS TO A'
     1,/,' CHANNEL/PIPE/INLET EXCEEDED FOR CH/PIPE/INLET',I10,/,
     2' BY SUBCATCHMENT',I10,'. SUGGEST USING DUMMY PIPE TO ALLOW MORE C
     3ONNECTIONS.')
 8921 FORMAT(/,' ERROR! MAXIMUM OF',I3,' SUBCATCHMENT CONNECTIONS TO A'
     1,/,' CHANNEL/PIPE/INLET EXCEEDED FOR CH/PIPE/INLET',A10,/,
     2' BY SUBCATCHMENT',A10,'. SUGGEST USING DUMMY PIPE TO ALLOW MORE C
     3ONNECTIONS.')
C
 9010 FORMAT(/,' Reading subcatchment information.')
 9030 FORMAT(/,' Reading subcatchment snowmelt information.')
 9040 FORMAT(/,' Reading channel/pipe information.')
 9050 FORMAT(/,' Reading snowmelt information.')
 9060 FORMAT(/,' Reading rainfall information.')
C##### WCH, 12/92
 9070 FORMAT(/,' Limit number of groundwater convergence messages to ',
     +I5,' (if simulated)')
C=======================================================================
      END

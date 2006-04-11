      SUBROUTINE CATCH(TRIBA)
C     RUNOFF BLOCK
C     CALLED BY RHYDRO1
C=======================================================================
C     THIS SUBROUTINE READS SUBCATCHMENT INPUT DATA FOR
C     THE SURFACE AND SUBSURFACE CATCHMENT COMPARTMENTS
C     LAST UPDATED NOVEMBER, 10/92 BY WCH FOR UNITS CORRECTION
C       TO PARAMETERS A1,A2,A3 AND MISC. UNITS
C       CORRECTION TO FORMAT STATEMENTS.  ALSO CORRECTION TO CHKVOL
C       IF-STATEMENT (12/92).
C     CHECK FOR IMPERVIOUSNESS > 100 %.  WCH, 4/27/93.
C     ADD CALL TO RDIIREAD, C. MOORE, CDM, 8/93
C     ADD CHECK FOR DET >= 0.  WCH (RED), 9/23/93.
C     INITIALIZE TWO GROUNDWATER VARIABLES, WCH, 4/11/94.
C     MAKE RATIO/DEFAULT OPTION USEABLE FOR GROUNDWATER AND ADD AN ERROR
C       MESSAGE, WCH, 4/21/94.
C     CHECK FOR INPUT OF AT LEAST ONE SUBCATCHMENT! WCH, 12/5/94.
C     MINOR OUTPUT FORMAT CHANGE FOR HYD. COND., WCH, 12/5/94.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'QUALTY.INC'
      INCLUDE 'GRWTR.INC'
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
CIM   MAXINF  C.Moore B. Cunningham CDM
      INCLUDE 'MAXINF.INC'      
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
      DIMENSION GRDR(5),GRDF(5),GRNR(10),GRNF(10),GRD(5),GRN(10),
     1          GROR(5),GROF(5),GRO(5),WD(11),WW(11),WR(11),PZ(NW)
C=======================================================================
C     User must supply any default values.
C=======================================================================
      NOGWSC    = 0
      IRFLAG    = 0
C#### 4/11/94.  INITIALIZE NSVGW AND NGWGF.
      NSVGW = 0
      NGWGF = 0
C#### WCH 10/92
C### METRIC=1, CONVERT IN/(HR-FT^2) TO FT/(SEC-FT^2)  (ALREADY OK)
                      CMETT = 43200.0
C### METRIC=1, CONVERT MM/(HR-M^2) TO FT/(SEC-FT^2)   (CORRECTION)
      IF(METRIC.EQ.2) CMETT = 43200. * 25.4 * 3.2808**2
      DO  90 J  = 1,11
      WD(J)     = 0.0
   90 WR(J)     = 1.0
      DO  95 JM = 1,NGW
      NMSUB(JM) = 0
  95  NGWGW(JM) = 0
C=======================================================================
      DO 100 J1 = 1,5
      GRDR(J1)  = 1.0
  100 GRDF(J1)  = 0.0
      DO 110 J2 = 1,10
      GRNR(J2)  = 1.0
  110 GRNF(J2)  = 0.0
      DO 120 J3 = 1,5
      GROR(J3)  = 1.0
  120 GROF(J3)  = 0.0
      ISFPF     = 0
      ISFGF     = 0
C=======================================================================
C     Read watershed data.
C=======================================================================
C>>>>>>>> READ DATA LINE H0 <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.EQ.'H0') THEN
                     KREAD = 1
                     ELSE
                     KREAD = 0
                     BACKSPACE N5
                     ENDIF
C=======================================================================
      IF(IPRN(6).EQ.0) THEN
      WRITE(N6,770)
      IF(METRIC.EQ.1) THEN
                      IF(INFILM.EQ.0) WRITE(N6,780)
                      IF(INFILM.EQ.1) WRITE(N6,784)
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
                      IF(INFILM.EQ.2) WRITE(N6,7801)
                      IF(INFILM.EQ.3) WRITE(N6,7841)
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
                      ELSE
                      IF(INFILM.EQ.0) WRITE(N6,782)
                      IF(INFILM.EQ.1) WRITE(N6,786)
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
                      IF(INFILM.EQ.2) WRITE(N6,7821)
                      IF(INFILM.EQ.3) WRITE(N6,7861)
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
                      ENDIF
      ENDIF
      TRIBA     = 0.0
      ARIMP     = 0.0
      N         = 0
      SUMWD     = 0.0
C=======================================================================
      DO 880 I  = 1,900
      N         = N + 1
C=======================================================================
C>>>>>>>> READ DATA LINES H1,H2,H3, AND H4 <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC
      IF(CC.NE.'H1'.AND.CC.NE.'H2'.AND.CC.NE.'H5') THEN
                     BACKSPACE N5
                     GO TO 895
                     ENDIF
      IF(CC.EQ.'H1') THEN
                     BACKSPACE N5
                     IF(KREAD.EQ.0) THEN
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
                      IF (INFILM.LE.1) THEN
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
                        IF(JCE.EQ.0) READ(N5,*,ERR=888)
     +                                 CC,JK,NAMEW(N),NGTO(N),WW
                        IF(JCE.EQ.1) READ(N5,*,ERR=888)
     +                                 CC,JK,KAMEW(N),KGTO(N),WW
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
                      ELSE
                        IF(JCE.EQ.0) READ(N5,*,ERR=888)
     +                              CC,JK,NAMEW(N),NGTO(N),WW,RMAXINF(N)
                        IF(JCE.EQ.1) READ(N5,*,ERR=888)
     +                              CC,JK,KAMEW(N),KGTO(N),WW,RMAXINF(N)
                      ENDIF
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
                        ELSE
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
                      IF (INFILM.LE.1) THEN
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
                        IF(JCE.EQ.0) READ(N5,*,ERR=888)
     +                                 CC,JK,NAMEW(N),NGTO(N),PZ(N),WW
                        IF(JCE.EQ.1) READ(N5,*,ERR=888)
     +                                 CC,JK,KAMEW(N),KGTO(N),PZ(N),WW
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
                      ELSE
                        IF(JCE.EQ.0) READ(N5,*,ERR=888)
     +                        CC,JK,NAMEW(N),NGTO(N),PZ(N),WW,RMAXINF(N)
                        IF(JCE.EQ.1) READ(N5,*,ERR=888)
     +                        CC,JK,KAMEW(N),KGTO(N),PZ(N),WW,RMAXINF(N)
                      ENDIF
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
                        ENDIF
C#### WCH, 4/21/94.  ADD ERROR MESSAGE HERE.
C     MUST HAVE H1 LINE BE FIRST IN SEQUENCE OF H1-H5.
                     ELSE
                     WRITE(N6,1070) N,CC
                     STOP
                     ENDIF
C=======================================================================
C     ALTER RATIOS.
C=======================================================================
      IF(JCE.EQ.0)         THEN
         IF(NAMEW(N).EQ.-1)   THEN
                              DO 810 J = 1,11
                              IF(WW(J).NE.0.0) WR(J)=WW(J)
  810                         CONTINUE
                              N = N - 1
                              GO TO 880
                              ENDIF
                           ENDIF
      IF(JCE.EQ.1)         THEN
         IF(KAMEW(N).EQ.'-1') THEN
                              DO 815 J = 1,11
                              IF(WW(J).NE.0.0) WR(J)=WW(J)
  815                         CONTINUE
                              N = N - 1
                              GO TO 880
                              ENDIF
                           ENDIF
C=======================================================================
C     ALTER INPUT DEFAULT VALUES.
C=======================================================================
      IF(JCE.EQ.0)         THEN
         IF(NAMEW(N).EQ.-2)   THEN
                              DO 820 J = 1,11
                              IF(WW(J).NE.0.0) WD(J)=WW(J)
  820                         CONTINUE
                              N = N - 1
                              GO TO 880
                              ENDIF
                           ENDIF
      IF(JCE.EQ.1)         THEN
         IF(KAMEW(N).EQ.'-2') THEN
                              DO 825 J = 1,11
                              IF(WW(J).NE.0.0) WD(J)=WW(J)
  825                         CONTINUE
                              N = N - 1
                              GO TO 880
                              ENDIF
                           ENDIF
      IF(N.GT.NW) GO TO 895
C=======================================================================
C     ASSIGN DEFAULT VALUES AND MULTIPLY BY RATIOS.
C=======================================================================
      DO 850 J               = 1,11
      IF(WW(J).EQ.0.0) WW(J) = WD(J)
  850 WW(J)                  = WW(J)*WR(J)
      IF(JK.EQ.0)         JK = 1
      IF(WW(5).LE.0.0) WW(5) = 0.020
      IF(WW(6).LE.0.0) WW(6) = 0.020
      IF(WW(9).EQ.0.0.OR.WW(10).EQ.0.0.OR.WW(11).EQ.0.0) WRITE(N6,855)
C##### WCH, 4/27,93.  ADD CHECK FOR IMPERVIOUSNESS > 100 %.
      IF(WW(3).GT.100.0) THEN
             WRITE(N6,856) WW(3)
             WW(3) = 100.0
             ENDIF
C=======================================================================
C     PRINT WATERSHED DATA
C=======================================================================
      IF(IPRN(6).EQ.0) THEN
         IF(N.NE.51.AND.N.NE.101.AND.N.NE.151) GO TO 860
         WRITE(N6,770)
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
      IF(METRIC.EQ.1) THEN
                      IF(INFILM.EQ.0) WRITE(N6,780)
                      IF(INFILM.EQ.1) WRITE(N6,784)
                      IF(INFILM.EQ.2) WRITE(N6,7801)
                      IF(INFILM.EQ.3) WRITE(N6,7841)
                      ELSE
                      IF(INFILM.EQ.0) WRITE(N6,782)
                      IF(INFILM.EQ.1) WRITE(N6,786)
                      IF(INFILM.EQ.2) WRITE(N6,7821)
                      IF(INFILM.EQ.3) WRITE(N6,7861)
                      ENDIF
cim         IF(INFILM.EQ.0) THEN
cim                      IF(METRIC.EQ.1) WRITE(N6,780)
cim                      IF(METRIC.EQ.2) WRITE(N6,782)
cim                      ELSE
cim                      IF(METRIC.EQ.1) WRITE(N6,784)
cim                      IF(METRIC.EQ.2) WRITE(N6,786)
cim                      ENDIF
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
  860    IF(JCE.EQ.0.AND.INFILM.EQ.0) WRITE(N6,870)
     +                             N,NAMEW(N),NGTO(N),WW,JK
         IF(JCE.EQ.1.AND.INFILM.EQ.0) WRITE(N6,872)
     +                             N,KAMEW(N),KGTO(N),WW,JK
         IF(JCE.EQ.0.AND.INFILM.EQ.1) WRITE(N6,871)
     +                             N,NAMEW(N),NGTO(N),WW,JK
         IF(JCE.EQ.1.AND.INFILM.EQ.1) WRITE(N6,873)
     +                             N,KAMEW(N),KGTO(N),WW,JK
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
         IF(JCE.EQ.0.AND.INFILM.EQ.2) WRITE(N6,8701)
     +                             N,NAMEW(N),NGTO(N),WW,JK,RMAXINF(N)
         IF(JCE.EQ.1.AND.INFILM.EQ.2) WRITE(N6,8721)
     +                             N,KAMEW(N),KGTO(N),WW,JK,RMAXINF(N)
         IF(JCE.EQ.0.AND.INFILM.EQ.3) WRITE(N6,8711)
     +                             N,NAMEW(N),NGTO(N),WW,JK,RMAXINF(N)
         IF(JCE.EQ.1.AND.INFILM.EQ.3) WRITE(N6,8731)
     +                             N,KAMEW(N),KGTO(N),WW,JK,RMAXINF(N)
cim correct rmaxinf for metric units     (needs to be feet?)
         RMAXINF(N) = RMAXINF(N)/CMET(3,metric)
cim  initialize totinf array to zero
         TOTINF(N) = 0.0         
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
         ENDIF
C=======================================================================
C     TRANSFER DATA AND CONVERT UNITS
C=======================================================================
      NHYET(N) = JK
      WAREA(N) = WW(2)*43560.*CMET(2,METRIC)
C=======================================================================
C     SUM THE IMPERVIOUS AREA OF THE CATCHMENT.
C     SUMWD - IS A SUMMER VARIABLE THAT SUMS THE CATCHMENT WIDTH
C=======================================================================
      IF(KREAD.EQ.0) PZERO  = PCTZER
      IF(KREAD.EQ.1) PZERO  = PZ(N)
      ARIMP       = ARIMP +  WW(2) * WW(3) / 100.00
      SUMWD       = SUMWD +  WW(1)
      WAR(1,N)    = WAREA(N)*WW(3)/100.*(100.0-PZERO)/100.0
      WAR(3,N)    = WAREA(N)*WW(3)/100.0*PZERO/100.0
      WAR(2,N)    = WAREA(N)*(100.-WW(3))/100.0
      WSLOPE(N)   = WW(4)
      WAR(4,N)    = 0.0
      WSTORE(1,N) = WW(7)/CMET(3,METRIC)
      WSTORE(2,N) = WW(8)/CMET(3,METRIC)
      WLMAX(N)    = WW(9)/CMET(4,METRIC)
      WLMIN(N)    = WW(10)/CMET(4,METRIC)
      DECAY(N)    = WW(11)
      IF(INFILM.EQ.1) THEN
                      SMD(N)   = WW(11)
                      WLMAX(N) = WW(9)/CMET(3,METRIC)
                      UL(N)    = 4.0*SQRT(WW(10)/CMET(5,METRIC))/12.0
                      IFLAG(N) = 0
                      FTOT(N)  = 0.0
                      FU(N)    = 0.0
                      ENDIF
      WCON(1,N) = 0.0
      WCON(2,N) = 0.0
      IF(WW(3).GT.0.0.AND.(WAR(1,N)+WAR(3,N)).NE.0.0) 
     1                  WCON(1,N) = -(1.486/WW(5))*SQRT(WW(4))
     1                  *WW(1)/(WAR(1,N) + WAR(3,N))*CMET(1,METRIC)
      IF(WW(3).LT.100.0.AND.WAR(2,N).NE.0.0) 
     1                   WCON(2,N) = -(1.486/WW(6))
     1                   *SQRT(WW(4))*WW(1)/WAR(2,N)*CMET(1,METRIC)
      TRIBA = TRIBA + WW(2)
C=======================================================================
C>>>>> READ DATA LINES H2,H3,AND H4 IF PRESENT <<<<<<
C=======================================================================
C#### WCH, 4/21/94.  NEED TO RETURN HERE IF RATIO/DEFAULT USED.  PROVIDE
C     STATEMENT NUMBER 865.
C
  865 READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.EQ.'H2') THEN
C#### WCH, 4/21/94.  MOVE TO AFTER RATIO/DEFAULT STUFF.
C####                     NOGWSC = NOGWSC + 1
                     IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,NMSUB(N),
     +                            NGWTOG(N),ISFPF,ISFGF,(GRD(I9),I9=1,5)
                     IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,KMSUB(N),
     +                            KGWTOG(N),ISFPF,ISFGF,(GRD(I9),I9=1,5)
                     IF(JCE.EQ.1) NMSUB(N) = N
                     READ(N5,*,ERR=888) CC,(GRN(I11),I11=1,10)
                     READ(N5,*,ERR=888) CC,(GRO(I51),I51=1,5)
C=======================================================================
C     ALTER RATIOS
C=======================================================================
      IF(JCE.EQ.0)         THEN
         IF(NMSUB(N).EQ.-1)   THEN
                              DO 10 I1 = 1,5
                              IF(GRD(I1).NE.0.) GRDR(I1) = GRD(I1)
 10                           CONTINUE
                              DO 15 I2 = 1,10
                              IF(GRN(I2).NE.0.) GRNR(I2) = GRN(I2)
 15                           CONTINUE
                              DO 16 I31 = 1,5
                              IF(GRO(I31).NE.0.) GROR(I31) = GRO(I31)
 16                           CONTINUE
C#### WCH, 4/21/94.  GO BACK TO STATEMENT 865 AND DON'T DECREASE N.
C####                              N = N - 1
C####                              GO TO 880
                              GO TO 865
                              ENDIF
                           ENDIF
      IF(JCE.EQ.1)         THEN
         IF(KMSUB(N).EQ.'-1') THEN
                              DO 20 I1 = 1,5
                              IF(GRD(I1).NE.0.) GRDR(I1) = GRD(I1)
 20                           CONTINUE
                              DO 25 I2 = 1,10
                              IF(GRN(I2).NE.0.) GRNR(I2) = GRN(I2)
 25                           CONTINUE
                              DO 26 I31 = 1,5
                              IF(GRO(I31).NE.0.) GROR(I31) = GRO(I31)
 26                           CONTINUE
C#### WCH, 4/21/94.  GO BACK TO STATEMENT 865 AND DON'T DECREASE N.
C####                              N = N - 1
C####                              GO TO 880
                              GO TO 865
                              ENDIF
                           ENDIF
C=======================================================================
C     ALTER INPUT DEFAULT VALUES
C=======================================================================
      IF(JCE.EQ.0)         THEN
         IF(NMSUB(N).EQ.-2)   THEN
                              DO 35 I3 = 1,5
                              IF(GRD(I3).NE.0.) GRDF(I3) = GRD(I3)
 35                           CONTINUE
                              DO 40 I4 = 1,10
                              IF(GRN(I4).NE.0.) GRNF(I4) = GRN(I4)
 40                           CONTINUE
                              DO 41 I41 = 1,5
                              IF(GRO(I41).NE.0.) GROF(I41) = GRO(I41)
 41                           CONTINUE
C#### WCH, 4/21/94.  GO BACK TO STATEMENT 865 AND DON'T DECREASE N.
C####                              N = N - 1
C####                              GO TO 880
                              GO TO 865
                              ENDIF
                           ENDIF
      IF(JCE.EQ.1)         THEN
         IF(KMSUB(N).EQ.'-2') THEN
                              DO 45 I3 = 1,5
                              IF(GRD(I3).NE.0.) GRDF(I3) = GRD(I3)
 45                           CONTINUE
                              DO 50 I4 = 1,10
                              IF(GRN(I4).NE.0.) GRNF(I4) = GRN(I4)
 50                           CONTINUE
                              DO 51 I41 = 1,5
                              IF(GRO(I41).NE.0.) GROF(I41) = GRO(I41)
 51                           CONTINUE
C#### WCH, 4/21/94.  GO BACK TO STATEMENT 865 AND DON'T DECREASE N.
C####                              N = N - 1
C####                              GO TO 880
                              GO TO 865
                              ENDIF
                           ENDIF
C=======================================================================
C                    ASSIGN DEFAULT VALUES AND MULTIPLY BY RATIOS
C=======================================================================
C#### WCH, 4/21/94.  INCREMENT COUNTER HERE.
                     NOGWSC = NOGWSC + 1
                     DO 2045 I5 = 1,5
                     IF(GRD(I5).EQ.0.) GRD(I5) = GRDF(I5)
 2045                GRD(I5)                   = GRD(I5)*GRDR(I5)
                     DO 2050 I6                = 1,10
                     IF(GRN(I6).EQ.0.) GRN(I6) = GRNF(I6)
 2050                GRN(I6)                   = GRN(I6)*GRNR(I6)
                     DO 2060 I6                = 1,5
                     IF(GRO(I6).EQ.0.) GRO(I6) = GROF(I6)
 2060                GRO(I6)                   = GRO(I6)*GROR(I6)
C=======================================================================
C         TRANSFER DATA AND CONVERT UNITS
C=======================================================================
          BELEV(N)  = GRD(1)*CMET(1,METRIC)
          GRELEV(N) = GRD(2)*CMET(1,METRIC)
          STG(N)    = GRD(3)*CMET(1,METRIC)
          BC(N)     = GRD(4)*CMET(1,METRIC)
          TW(N)     = GRD(5)*CMET(1,METRIC)
          A1(N)     = GRN(1)/CMET(4,METRIC)*CMET(1,METRIC)**GRN(2)
          B1(N)     = GRN(2)
          A2(N)     = GRN(3)/CMET(4,METRIC)*CMET(1,METRIC)**GRN(4)
          B2(N)     = GRN(4)
          A3(N)     = GRN(5)/CMETT
          POR(N)    = GRN(6)
          WP(N)     = GRN(7)
          FC(N)     = GRN(8)
          HKSAT(N)  = GRN(9)/CMET(4,METRIC)
          TH1(N)    = GRN(10)
          HCO(N)    = GRO(1)
          PCO(N)    = GRO(2)
          CET(N)    = GRO(3)
          DP(N)     = GRO(4)/CMET(4,METRIC)
C#### WCH (RED), 9/93.  DET CAN'T BE ZERO.
          IF(GRO(5).LE.0.0) THEN
               WRITE(N6,1065) N,GRO(5)
               GRO(5) = 1.0
               ENDIF
          DET(N)    = GRO(5)*CMET(1,METRIC)
C=======================================================================
C     CALCULATE NGWGF AND RESET NGWGW(N)  IF NECESSARY
C     CALCULATE NSVGW AND RESET NSCSFG(N) IF NECESSARY
C=======================================================================
      IF(ISFPF.EQ.1) THEN
                     NGWGF    = NGWGF + 1
                     NGWGW(N) = 1
                     ELSE
                     NGWGW(N) = 0
                     ENDIF
      IF(ISFGF.EQ.1) THEN
                     NSVGW     = NSVGW + 1
                     NSCSFG(N) = 1
                     ELSE
                     NSCSFG(N) = 0
                     ENDIF
C=======================================================================
C     SUM BEGINNING SUBSURFACE STORAGE
C=======================================================================
      CNT(15) = CNT(15)+TH1(N)*(GRELEV(N)-STG(N))*WAREA(N)+
     .          POR(N)*(STG(N)-BELEV(N))*WAREA(N)
C=======================================================================
C     CHECK FOR POSSIBLE ERRORS IN INPUT DATA
C=======================================================================
      CHKVOL = (GRELEV(N)-STG(N))*(POR(N)-TH1(N))
C##### WCH, 12/92  INSERT DECIMAL POINT INTO 0.0001
      IF(CHKVOL.LE.0.0001) WRITE(N6,1001) NMSUB(N)
      IF(BELEV(N).GE.GRELEV(N).OR.STG(N).GT.GRELEV(N).OR.
     .                         BELEV(N).GT.STG(N)) THEN
                               IF(JCE.EQ.0) WRITE(N6,1020) NMSUB(N)
                               IF(JCE.EQ.1) WRITE(N6,1021) KMSUB(N)
                               IRFLAG = 1
                               ENDIF
      IF(WP(N).GE.FC(N))  THEN
                          IF(JCE.EQ.0) WRITE(N6,1030) NMSUB(N)
                          IF(JCE.EQ.1) WRITE(N6,1031) KMSUB(N)
                          IRFLAG = 1
                          ENDIF
      IF(FC(N).GT.POR(N)*0.9) THEN
C#### WCH,12/92  CHANGE THIS ERROR TO JUST A WARNING.
                              IF(JCE.EQ.0) WRITE(N6,1040) NMSUB(N)
                              IF(JCE.EQ.1) WRITE(N6,1041) KMSUB(N)
                              ENDIF
      IF(BC(N).GT.TW(N)) THEN
                         IF(TW(N).GE.0.0) THEN
                                  IF(JCE.EQ.0) WRITE(N6,1050) NMSUB(N)
                                  IF(JCE.EQ.1) WRITE(N6,1051) KMSUB(N)
                                  TW(N) = BC(N)
                                  ENDIF
                         ENDIF
      IF(POR(N).GT.1.0.OR.FC(N).GT.1.0.OR.WP(N).GT.1.0
     +                .OR.TH1(N).GT.1.0) THEN
                                 IRFLAG = 1
                                 IF(JCE.EQ.0) WRITE(N6,1060) NMSUB(N)
                                 IF(JCE.EQ.1) WRITE(N6,1061) KMSUB(N)
                                 ENDIF
      ENDIF
C=======================================================================
C     END OF IF-THEN BLOCK FOR GROUNDWATER INPUT
C=======================================================================
C#### C. MOORE, 8/93
C      SECOND CALL TO RDIIREAD
C=======================================================================
      CALL RDIIREAD(2,N)
  880 CONTINUE
  895 NOW    = N-1
C=======================================================================
C#### WCH, 12/5/94.  CHECK FOR PRESENCE OF AT LEAST ONE SUBCATCHMENT!
      IF(NOW.EQ.0) THEN
         WRITE (N6,879)
         WRITE (*,879)
         STOP
         ENDIF
C=======================================================================
      IF(NOGWSC.GT.0) THEN
         IF(NGWGF.GT.0.AND.NSCRAT(5).EQ.0) THEN
                                           WRITE(N6,881)
                                           IRFLAG = 1
                                           ELSE
                                           REWIND NSCRAT(5)
                                           ENDIF
         IF(NSVGW.GT.0.AND.NSCRAT(6).EQ.0) THEN
                                           WRITE(N6,886)
                                           IRFLAG = 1
                                           ELSE
                                           REWIND NSCRAT(6)
                                           ENDIF
         ENDIF
C=======================================================================
C     CALCULATE THE FRACTION IMPERVIOUSNESS OF THE ENTIRE CATCHMENT
C               AND THE TOTAL PERVIOUS AREA.
C=======================================================================
      IF (TRIBA.NE.0.0) THEN
      PRCIMP = ARIMP/TRIBA
      ELSE
      PRCIMP = 0.0
      END IF
      ARPRV  = TRIBA - ARIMP
      PRC    = PRCIMP * 100.0
C
      IF(IPRN(6).EQ.0) THEN
      IF(METRIC.EQ.1) WRITE(N6,900) NOW,TRIBA,ARIMP,ARPRV,SUMWD,PRC
      IF(METRIC.EQ.2) WRITE(N6,905) NOW,TRIBA,ARIMP,ARPRV,SUMWD,PRC
      ENDIF
      TRIBA = TRIBA*CMET(2,METRIC)
      IF(NOW.GT.NW) CALL ERROR(17)
C=======================================================================
C     GROUNDWATER INPUT PRINTOUT
C=======================================================================
      IF(NOGWSC.GT.0.AND.IPRN(6).EQ.0) THEN
                WRITE(N6,2301)
                IF(METRIC.EQ.1) WRITE(N6,2305)
                IF(METRIC.EQ.2) WRITE(N6,2310)
                DO 2325 JBC = 1,NOGWSC
                GR1 = BELEV(JBC)/CMET(1,METRIC)
                GR2 = GRELEV(JBC)/CMET(1,METRIC)
                GR3 = STG(JBC)/CMET(1,METRIC)
                GR4 = BC(JBC)/CMET(1,METRIC)
                GR5 = TW(JBC)/CMET(1,METRIC)
                GR6 = A1(JBC)*CMET(4,METRIC)/CMET(1,METRIC)**B1(JBC)
                GR7 = A2(JBC)*CMET(4,METRIC)/CMET(1,METRIC)**B2(JBC)
                GR8 = A3(JBC)*CMETT
                IF(JCE.EQ.0) WRITE(N6,2315) NMSUB(JBC),NGWTOG(JBC),
     +                         GR2,GR1,GR3,GR4,GR5,GR6,B1(JBC),GR7,
     +                         B2(JBC),GR8
                IF(JCE.EQ.1) WRITE(N6,2335) KMSUB(JBC),KGWTOG(JBC),
     +                         GR2,GR1,GR3,GR4,GR5,GR6,B1(JBC),GR7,
     +                         B2(JBC),GR8
 2325           CONTINUE
                WRITE(N6,2326)
                IF(METRIC.EQ.1) WRITE(N6,2307)
                IF(METRIC.EQ.2) WRITE(N6,2308)
                DO 2327 JBC = 1,NOGWSC
                GR1         = HKSAT(JBC)*CMET(4,METRIC)
                GR2         = DP(JBC)*CMET(4,METRIC)
                GR3         = DET(JBC)/CMET(1,METRIC)
                IF(JCE.EQ.0) WRITE(N6,2309) NMSUB(JBC),POR(JBC),
     +                          GR1,WP(JBC),FC(JBC),TH1(JBC),GR2,
     +                          HCO(JBC),PCO(JBC),GR3,CET(JBC)
                IF(JCE.EQ.1) WRITE(N6,2319) KMSUB(JBC),POR(JBC),
     +                          GR1,WP(JBC),FC(JBC),TH1(JBC),GR2,
     +                          HCO(JBC),PCO(JBC),GR3,CET(JBC)
 2327           CONTINUE
                IF(IRFLAG.GT.1) WRITE(N6,907)
                ENDIF
C=======================================================================
  770 FORMAT(1H1,//,
     +'  ***************************************************',/,
     +'  *          S U B C A T C H M E N T  D A T A       *',/,
     +'  ***************************************************',/)
CIM CHANGE TO TABLES TO PRINT 10 CHARACTERS AND DIGIT IDS  4/99
 780  FORMAT(
     1'       SUBCATCH-  CHANNEL      WIDTH    AREA  PERCENT',
     1'    SLOPE    RESISTANCE  FACTOR    DEPRES. STORAGE(IN)',
     1' INFILTRATION   DECAY RATE GAGE',/,
     2'        MENT NO.  OR INLET      (FT)    (AC)  IMPERV.',
     2'   (FT/FT)    IMPERV.     PERV.   IMPERV.    PERV.    ',
     2'  RATE(IN/HR)    (1/SEC)    NO.',/,
     3'                                                     ',
     3'                                                      ',
     3' MAXIMUM MINIMUM',/,
     4'  ---  --------   -------- --------- --------  ----- ',
     4'  -------    -------  --------    ------    ------    ',
     4' ----   -------  -------  ------')
C  780 FORMAT(' SUBCATCH-  CHANNEL  WIDTH   AREA   PERCENT    SLOPE    RE
C     1SISTANCE FACTOR  DEPRES. STORAGE(IN)    INFILTRATION  DECAY RATE
C     2GAGE',/,
C     3' MENT NO.  OR INLET  (FT)    (AC)   IMPERV.   (FT/FT)    IMPERV.
C     4   PERV.    IMPERV.    PERV.      RATE(IN/HR)    (1/SEC)    NO.',/
C     5,T97,'MAXIMUM MINIMUM',/,
C     6' ---  ---   ----    ------   ------  -----   -------     ------
C     7  ------    ------    ------     ----   -------  -------  ------')
 782  FORMAT(
     1'       SUBCATCH-  CHANNEL      WIDTH    AREA  PERCENT',
     1'    SLOPE    RESISTANCE  FACTOR    DEPRES. STORAGE(MM)',
     1' INFILTRATION   DECAY RATE GAGE',/,
     2'        MENT NO.  OR INLET      (M)     (HA)  IMPERV.',
     2'    (M/M)     IMPERV.     PERV.   IMPERV.    PERV.    ',
     2'  RATE(MM/HR)    (1/SEC)    NO.',/,
     3'                                                     ',
     3'                                                      ',
     3' MAXIMUM MINIMUM',/,
     4'  ---  --------   -------- --------- --------  ----- ',
     4'  -------    -------  --------    ------    ------    ',
     4' ----   -------  -------  ------')
C 782 FORMAT(' SUBCATCH-  CHANNEL  WIDTH   AREA   PERCENT    SLOPE    RE
C    1SISTANCE FACTOR  DEPRES. STORAGE(MM)    INFILTRATION  DECAY RATE
C    2GAGE',/,
C    3' MENT NO.  OR INLET   (M)    (HA)   IMPERV.   (M/M)      IMPERV.
C    4   PERV.    IMPERV.    PERV.      RATE(MM/HR)    (1/SEC)    NO.',/
C    5,T97,'MAXIMUM MINIMUM',/,
C    6' ---  ---   ----    ------   ------  -----   -------     ------
C    7  ------    ------    ------     ----   -------  -------  ------')
 784  FORMAT(
     1'       SUBCATCH-  CHANNEL      WIDTH    AREA  PERCENT',
     1'    SLOPE    RESISTANCE  FACTOR    DEPRES. STORAGE(IN)',
     1'  GREEN-AMPT INFIL PARAMS     GAGE',/,
     2'        MENT NO.  OR INLET      (FT)    (AC)  IMPERV.',
     2'   (FT/FT)    IMPERV.     PERV.   IMPERV.    PERV.    ',
	2' SUCTION  HYD.CON     IMD      NO.',/
     3'                                                     ',
     3'                                                      ',
	3'   (IN)   (IN/HR)',/,
     4'  ---  --------   -------- --------- --------  ----- ',
     4'  -------    -------  --------    ------    ------    ',
     4' ----   -------  -------  ------')
C  784 FORMAT(' SUBCATCH-  GUTTER   WIDTH   AREA   PERCENT    SLOPE    RE
C     1SISTANCE FACTOR  DEPRES. STORAGE(IN)  GREEN-AMPT INFIL PARAMS     
C     2GAGE',/,
C     3' MENT NO.  OR INLET  (FT)    (AC)   IMPERV.   (FT/FT)    IMPERV.
C     4   PERV.    IMPERV.    PERV.  SUCTION  HYD.CON     IMD      NO.',/
C     5,T99,'(IN)   (IN/HR)',/,
C     6' ---  ---   ----    ------   ------  -----   -------     ------
C     7  ------    ------    ------     ----   -------  -------  ------')
 786  FORMAT(
     1'       SUBCATCH-  CHANNEL      WIDTH    AREA  PERCENT',
     1'    SLOPE    RESISTANCE  FACTOR    DEPRES. STORAGE(MM)',
     1'  GREEN-AMPT INFIL PARAMS     GAGE',/,
     2'        MENT NO.  OR INLET       (M)    (HA)  IMPERV.',
     2'    (M/M))    IMPERV.     PERV.   IMPERV.    PERV.    ',
	2' SUCTION  HYD.CON     IMD      NO.',/
     3'                                                     ',
     3'                                                      ',
	3'   (MM)   (MM/HR)',/,
     4'  ---  --------   -------- --------- --------  ----- ',
     4'  -------    -------  --------    ------    ------    ',
     4' ----   -------  -------  ------')
C 786 FORMAT(' SUBCATCH-  GUTTER   WIDTH   AREA   PERCENT    SLOPE    RE
C    1SISTANCE FACTOR  DEPRES. STORAGE(MM)  GREEN-AMPT INFIL PARAMS
C    2GAGE',/,
C    3' MENT NO.  OR INLET   (M)    (HA)   IMPERV.   (M/M)      IMPERV.
C    4   PERV.    IMPERV.    PERV.  SUCTION  HYD.CON     IMD      NO.',/
C    5,T99,'(MM)   (MM/HR)',/,
C    6' ---  ---   ----    ------   ------  -----   -------     ------
C    7  ------    ------    ------     ----   -------  -------  ------')
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
 7801 FORMAT(
     1'       SUBCATCH-  CHANNEL      WIDTH    AREA  PERCENT',
     1'    SLOPE    RESISTANCE  FACTOR    DEPRES. STORAGE(IN)',
     1' INFILTRATION   DECAY RATE GAGE  MAXIMUM',/,
     2'        MENT NO.  OR INLET      (FT)    (AC)  IMPERV.',
     2'   (FT/FT)    IMPERV.     PERV.   IMPERV.    PERV.    ',
     2'  RATE(IN/HR)    (1/SEC)    NO.  VOLUME',/,
     3'                                                     ',
     3'                                                      ',
     3' MAXIMUM MINIMUM',16X,'   (INCHES)',/,
     4'  ---  --------   -------- --------- --------  ----- ',
     4'  -------    -------  --------    ------    ------    ',
     4' ----   -------  -------  ------ -------')
c 7801 FORMAT(' SUBCATCH-  CHANNEL  WIDTH   AREA   PERCENT    SLOPE    RE
c     1SISTANCE FACTOR  DEPRES. STORAGE(IN)    INFILTRATION  DECAY RATE
c     2GAGE  MAXIMUM',/,
c     3' MENT NO.  OR INLET  (FT)    (AC)   IMPERV.   (FT/FT)    IMPERV.
c     4   PERV.    IMPERV.    PERV.      RATE(IN/HR)    (1/SEC)    NO.',
c     5'  VOLUME',/,T97,'MAXIMUM MINIMUM',16x,'   (INCHES)',/,
c     6' ---  ---   ----    ------   ------  -----   -------     ------
c     7  ------    ------    ------     ----   -------  -------  ------',
c     8' -------')  
 7821 FORMAT(
     1'       SUBCATCH-  CHANNEL      WIDTH    AREA  PERCENT',
     1'    SLOPE    RESISTANCE  FACTOR    DEPRES. STORAGE(MM)',
     1' INFILTRATION   DECAY RATE GAGE  MAXIMUM',/,
     2'        MENT NO.  OR INLET       (M)    (HA)  IMPERV.',
     2'    (M/M)     IMPERV.     PERV.   IMPERV.    PERV.    ',
     2'  RATE(MM/HR)    (1/SEC)    NO.  VOLUME',/,
     3'                                                     ',
     3'                                                      ',
     3' MAXIMUM MINIMUM',16X,'      (MM) ',/,
     4'  ---  --------   -------- --------- --------  ----- ',
     4'  -------    -------  --------    ------    ------    ',
     4' ----   -------  -------  ------ -------')
C7821 FORMAT(' SUBCATCH-  CHANNEL  WIDTH   AREA   PERCENT    SLOPE    RE
C    1SISTANCE FACTOR  DEPRES. STORAGE(MM)    INFILTRATION  DECAY RATE
C    2GAGE   MAXIMUM',/,
C    3' MENT NO.  OR INLET   (M)    (HA)   IMPERV.   (M/M)      IMPERV.
C    4   PERV.    IMPERV.    PERV.      RATE(MM/HR)    (1/SEC)    NO.',
C    5' VOLUME',/,T97,'MAXIMUM MINIMUM ',16x,'   (MM)',/,
C    6' ---  ---   ----    ------   ------  -----   -------     ------
C    7  ------    ------    ------     ----   -------  -------  ------',
C    8' -------')
 7841 FORMAT(
     1'       SUBCATCH-  CHANNEL      WIDTH    AREA  PERCENT',
     1'    SLOPE    RESISTANCE  FACTOR    DEPRES. STORAGE(IN)',
     1'  GREEN-AMPT INFIL PARAMS     GAGE   MAXIMUM',/,
     2'        MENT NO.  OR INLET      (FT)    (AC)  IMPERV.',
     2'   (FT/FT)    IMPERV.     PERV.   IMPERV.    PERV.    ',
	2' SUCTION  HYD.CON     IMD      NO.',/
     3'                                                     ',
     3'                                                      ',
	3'   (IN)   (IN/HR)',16x,'    (INCHES)',/,
     4'  ---  --------   -------- --------- --------  ----- ',
     4'  -------    -------  --------    ------    ------    ',
     4' ----   -------  -------  ------ ------')
c 7841 FORMAT(' SUBCATCH-  GUTTER   WIDTH   AREA   PERCENT    SLOPE    RE
c     1SISTANCE FACTOR  DEPRES. STORAGE(IN)  GREEN-AMPT INFIL PARAMS
c     2GAGE   MAXIMUM',/,
c     3' MENT NO.  OR INLET  (FT)    (AC)   IMPERV.   (FT/FT)    IMPERV.
c     4   PERV.    IMPERV.    PERV.  SUCTION  HYD.CON     IMD      NO.',
c     5' VOLUME',/,T99,'(IN)   (IN/HR)',16x,'    (INCHES)',/,
c     6' ---  ---   ----    ------   ------  -----   -------     ------
c     7  ------    ------    ------     ----   -------  -------  ------',
c     8' ------')
 7861 FORMAT(
     1'       SUBCATCH-  CHANNEL      WIDTH    AREA  PERCENT',
     1'    SLOPE    RESISTANCE  FACTOR    DEPRES. STORAGE(MM)',
     1'  GREEN-AMPT INFIL PARAMS     GAGE   MAXIMUM',/,
     2'        MENT NO.  OR INLET       (M)    (HA)  IMPERV.',
     2'    (M/M)     IMPERV.     PERV.   IMPERV.    PERV.    ',
	2' SUCTION  HYD.CON     IMD      NO.',/
     3'                                                     ',
     3'                                                      ',
	3'   (MM)   (MM/HR)',16x,'       (MM) ',/,
     4'  ---  --------   -------- --------- --------  ----- ',
     4'  -------    -------  --------    ------    ------    ',
     4' ----   -------  -------  ------ ------')
C7861 FORMAT(' SUBCATCH-  GUTTER   WIDTH   AREA   PERCENT    SLOPE    RE
C    1SISTANCE FACTOR  DEPRES. STORAGE(MM)  GREEN-AMPT INFIL PARAMS
C    2GAGE    MAXIMUM',/,
C    3' MENT NO.  OR INLET   (M)    (HA)   IMPERV.   (M/M)      IMPERV.
C    4   PERV.    IMPERV.    PERV.  SUCTION  HYD.CON     IMD      NO.',
C    5' VOLUME',/,T99,'(MM)   (MM/HR) ',16x,'    (MM)',/,
C    6' ---  ---   ----    ------   ------  -----   -------     ------
C    7  ------    ------    ------     ----   -------  -------  ------',
C    8' ------')
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
  855 FORMAT(/,' ===> WARNING !! ONE OR MORE OF THE INFILTRATION PARAMET
     1ERS HAVE NOT BEEN SET TO NON-ZERO VALUES')
C#### WCH, 4/27/93.
  856 FORMAT(' ===> WARNING! IMPERVIOUSNESS OF',F7.2,' IS > 100%.  RE-SE
     1T TO LIMIT OF 100 %.') 
  870 FORMAT(1X,I3,1X,I10,1X,I10,1X,F9.2,F9.2,F7.2,F10.4,1X,4F10.3,
     1F9.2,F8.2,F11.5,I6)
C#### WCH, 12/5/94. CHANGE LAST F10.2 TO F10.3.
  871 FORMAT(1X,I3,1X,I10,1X,I10,1X,F9.2,F9.2,F7.2,F10.4,1X,4F10.3,
     1F9.2,F10.3,F9.3,I8)

CIM  872 FORMAT(1X,I3,1X,A6,1X,A5,1X,F8.2,F9.2,F7.2,F10.4,1X,4F10.3,
CIM     +       F9.2,F8.2,F11.5,I6)
  872 FORMAT(1X,I3,1X,A10,1X,A10,1X,F8.2,F9.2,F7.2,F10.4,1X,4F10.3,
     +       F9.2,F8.2,F11.5,I6)
C#### WCH, 12/5/94. CHANGE LAST F10.2 TO F10.3.
  873 FORMAT(1X,I3,1X,A10,1X,A10,1X,F8.2,F9.2,F7.2,F10.4,1X,4F10.3,
     +       F9.2,F10.3,F9.3,I8)
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
 8701 FORMAT(1X,I3,1X,I10,1X,I10,1X,F9.2,F9.2,F7.2,F10.4,1X,4F10.3,
     1 F9.2,F8.2,F11.5,I6,f11.5)     
 8721 FORMAT(1X,I3,1X,A10,1X,A10,1X,F8.2,F9.2,F7.2,F10.4,1X,4F10.3,
     +       F9.2,F8.2,F11.5,I6,f11.5)
 8711 FORMAT(1X,I3,1X,I10,1X,I10,1X,F9.2,F9.2,F7.2,F10.4,1X,4F10.3,
     1F9.2,F10.3,F9.3,I8,f11.5)
 8731 FORMAT(1X,I3,1X,A10,1X,A10,1X,F8.2,F9.2,F7.2,F10.4,1X,4F10.3,
     +       F9.2,F10.3,F9.3,I8,f11.5)
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
C#### WCH, 12/5/94.
  879 FORMAT (/,' ERROR!  MUST HAVE AT LEAST ONE SUBCATCHMENT!!',/,
     1 ' NO H1 LINES FOUND (OR READ ERRONEOUSLY AS ANOTHER LINE ID EARLI
     2ER).',/,' RUN STOPPED WHILE TRYING TO READ SUBCATCHMENT DATA.')
  881 FORMAT(/,' ===> ERROR !! NSCRAT(5) NEEDS TO BE > 0')
  886 FORMAT(/,' ===> ERROR !! NSCRAT(6) NEEDS TO BE > 0')
  900 FORMAT(/,' TOTAL NUMBER OF SUBCATCHMENTS...',5X,I10,/,
     1         ' TOTAL TRIBUTARY AREA (ACRES)....',F15.2,/,
     2         ' IMPERVIOUS AREA (ACRES).........',F15.2,/,
     3         ' PERVIOUS AREA (ACRES)...........',F15.2,/,
     3         ' TOTAL WIDTH (FEET)..............',F15.2,/,
     4         ' PERCENT IMPERVIOUSNESS..........',F15.2)
  905 FORMAT(/,' TOTAL NUMBER OF SUBCATCHMENTS...',5X,I10,/,
     1         ' TOTAL TRIBUTARY AREA (HECTARES).',F15.2,/,
     2         ' IMPERVIOUS AREA (HECTARES)......',F15.2,/,
     3         ' PERVIOUS AREA (HECTARES)........',F15.2,/,
     3         ' TOTAL WIDTH (METERS)............',F15.2,/,
     4         ' PERCENT IMPERVIOUSNESS..........',F15.2)
  907 FORMAT(' ===> ERROR !! RUN STOPPED DUE TO ERROR IN GROUNDWATER',
     +       ' INPUT DATA')
 1001 FORMAT(' ===> WARNING !! INITIAL AVAILABLE SUBSURFACE VOLUME IS',
     +       ' < .0001 FT FOR SUBCATCHMENT ',I5)
 1020 FORMAT(' ===> ERROR !! ONE OR MORE ELEVATIONS IN SUBCATCHMENT ',
     .I5,' ARE EXCEEDED BY',/,
     .'AT LEAST ONE ELEVATION THAT SHOULD BE LOWER.')
 1021 FORMAT(' ===> ERROR !! ONE OR MORE ELEVATIONS IN SUBCATCHMENT ',
     .A10,' ARE EXCEEDED BY',/,
     .'AT LEAST ONE ELEVATION THAT SHOULD BE LOWER.')
 1030 FORMAT(' ===> ERROR !! WILTING POINT FOR SUBCATCHMENT ',I10,
     +       ' IS GREATER THAN FIELD CAPACITY')
 1031 FORMAT(' ===> ERROR !! WILTING POINT FOR SUBCATCHMENT ',A10,
     +       ' IS GREATER THAN FIELD CAPACITY')
 1040 FORMAT(' ===> WARNING !! FIELD CAPACITY FOR SUBCATCHMENT ',I10,
     .       ' IS GREATER THAN 0.9*POROSITY')
 1041 FORMAT(' ===> WARNING !! FIELD CAPACITY FOR SUBCATCHMENT ',A10,
     .       ' IS GREATER THAN 0.9*POROSITY')
 1050 FORMAT(' ===> WARNING !! TW BEING SET EQUAL TO BC FOR ',
     +       'SUBCATCHMENT ',I8)
 1051 FORMAT(' ===> WARNING !! TW BEING SET EQUAL TO BC FOR ',
     +       'SUBCATCHMENT ',A10)
 1060 FORMAT(' ===> ERROR !! POROSITY, FIELD CAPACITY, WILTING POINT,',
     +       ' OR INITIAL MOISTURE IS = OR > 1.0, FOR SUBCATCHMENT',I5)
 1061 FORMAT(' ===> ERROR !! POROSITY, FIELD CAPACITY, WILTING POINT,',
     +   ' OR INITIAL MOISTURE IS = OR > 1.0, FOR SUBCATCHMENT ',A10)
C#### WCH (RED), 9/93.
 1065 FORMAT(' ===> WARNING! SUBCATCHMENT SEQUENCE NO. ',I3,'. PARAMETER
     1 DET =',F6.2,/,'     DET MUST BE >= 0 AND IS BEING SET TO 1 FT [M]
     2')
C#### WCH, 4/21/94.
 1070 FORMAT(' ===> ERROR! MUST HAVE H1 LINE BE FIRST IN A SEQUENCE OF H
     11-H5 LINES.',/,' WHILE READING NUMBER ',I3,' OF SUBCATCHMENT DATA
     2GROUPS,',/,' ENCOUNTERED LINE ID = ',A2,' INSTEAD.  RUN STOPPED.')
 2301 FORMAT(1H1,/,
     +' ********************************************************',/,
     +' *      G R O U N D W A T E R   I N P U T   D A T A     *',/,
     +' ********************************************************',//)
C
C### WCH, 10/92  CORRECTED UNITS FOR A1,A2,A3
C
 2305 FORMAT(3X,'  SUB-     CHANNEL',
     +'     ========== E L E V A T I O N S ========  ====',
     +'======= F L O W   C O N S T A N T S ============',/,3X,
     +'  CATCH         OR      GROUND  BOTTOM   STAGE     BC      TW',  
     +7X,'A1         B1        A2         B2        A3',/,3X,
     +'  NUMBER     INLET       (FT)    (FT)     (FT)    (FT)    (FT)',
     +' (IN/HR-FT^B1)       (IN/HR-FT^B2)        (IN/HR-FT^2)  ',/,3X,
     +'  ------     -----     -------  ------  ------  ------  ------',
     +' -----------  ------- -----------  ------- -----------')
 2310 FORMAT(3X,'  SUB-     CHANNEL',
     +'     ========== E L E V A T I O N S ========  ====',
     +'======= F L O W   C O N S T A N T S ============',/,3X,
     +'  CATCH         OR      GROUND  BOTTOM   STAGE     BC      TW',
     +7X,'A1         B1        A2         B2        A3',/,3X,
     +'  NUMBER     INLET        (M)     (M)      (M)     (M)     (M)',
     +' (MM/HR-M^B1)         (MM/HR-M^B2)         (MM/HR-M^2)  ',/,3X,
     +'  ------     -----     -------  ------  ------  ------  ------',
     +' -----------  ------- -----------  ------- -----------')
 2315 FORMAT(1X,I10,I10,6X,5(F6.2,2X),1PE10.3,2X,0PF7.3,2X,
     +       1PE10.3,2X,0PF7.3,1PE12.2)
 2335 FORMAT(1X,A10,A10,6X,5(F6.2,2X),1PE10.3,2X,0PF7.3,2X,
     +       1PE10.3,2X,0PF7.3,1PE12.2)
 2326 FORMAT(1H1,//,
     +' ***********************************************************',/,
     +' * G R O U N D W A T E R   I N P U T   D A T A (CONTINUED) *',/,
     +' ***********************************************************',//,
     116X,'S O I L   P R O P E R T I E S',/,24X,
     2'SATURATED',43X,'PERCOLATION',5X,'E T  P A R A M E T E R S',/,
     38X,'SUBCAT.',11X,'HYDRAULIC  WILTING  FIELD   INITIAL    ',
     4' MAX. DEEP',3X,'PARAMETERS',9X,'DEPTH  FRACTION OF ET',/,
     58X,'NO.   POROSITY CONDUCTIVITY  POINT  CAPACITY MOISTURE  ',
     6'PERCOLATION   HCO    PCO        OF ET  TO UPPER ZONE')
 2307 FORMAT(25X,'(in/hr)',32X,'(in/hr)',24X,'(ft)',/,3X,
     +'    ----   ------     -------    ------  ------   ------    -----
     +-----  ------  ------     ------      ------')
C
C ### WCH, 10/92, CORRECT UNITS FROM CM/HR TO MM/HR AND FT TO M 
C                   IN FORMAT STATEMENT
C
 2308 FORMAT(25X,'(mm/hr)',32X,'(mm/hr)',24X,'(m)',/,3X,
     +'    ----   ------     -------    ------  ------   ------    -----
     +-----  ------  ------     ------      ------')
CIM  FIX FOLLOWING FORMAT STATEMENT
 2309 FORMAT(1X,I10,4X,F5.4,5X,F7.3,5X,F5.4,3X,F5.4,4X,F5.4,4X,
     +       1PE10.3,2X,0PF6.2,1X,F7.2,5X,F6.2,7X,F5.3)
 2319 FORMAT(1X,A10,4X,F5.4,5X,F7.3,5X,F5.4,3X,F5.4,4X,F5.4,4X,
     +       1PE10.3,2X,0PF6.2,1X,F7.2,5X,F6.2,7X,F5.3)
C=======================================================================
      RETURN
  888 CALL IERROR
      END

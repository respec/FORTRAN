      SUBROUTINE UNIT(I)
C=======================================================================
C  WCH, 8/93.  Increase allowable number of plugs (parameter NPLUG)
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'S1.INC'
C=======================================================================
C#### WCH, 8/93.  ADD OPTION FOR EASY INCREASE OF PLUGS DIMENSION
C      PARAMETER (NPLUG=200)
      PARAMETER (NPLUG=2000)
      DIMENSION ADEP(5,NPLUG),ASTP(5,NPLUG),C1(5,3),FREM(5,10)
      DIMENSION ION(5),KIN(5,NPLUG),KKDT(5),KQRS(5)
      DIMENSION LLDT(5),LPREV(5),NDTS(5),PCINN(5,3,NPLUG)
      DIMENSION PMINN(5,3,NPLUG),PMSL(5,3),PREM(3)
      DIMENSION PSIN(3,10),PSRN(3,10),PSSL(5,3,10)
      DIMENSION PSINN(5,3,NPLUG,10),QOUTL(5),QRESL(5)
      DIMENSION SAO(16),SAR(16),SAT(16),SDE(16)
      DIMENSION SST(16),SSTOP(5),VLIN(5,NPLUG),X1(11)
C=======================================================================
      JINT = NINT(I)
  100 IF(KDT.LE.1) THEN
                   JFLOOD   = 0
                   DOP(I)   = 0.0
C#### WCH, 8/93
                   NPL      = NPLUG
                   QMAXS(I) = 0.0
                   VMAXS(I) = 0.0
                   IF(NP.GT.0) THEN
                   DO 120 IP = 1,NP
                   PMSL(I,IP) = 0.0
                   IF(IPART(IP).GT.0) THEN
                                      DO 110 KJ     = 1,NNR
  110                                 PSSL(I,IP,KJ) = 0.0
                                      ENDIF
  120              CONTINUE
                   ENDIF
                   ENDIF
      QQIN(I) = QQIT(I)
      QQBY(I) = 0.0
      QQOU(I) = 0.0
      QQRS(I) = 0.0
      QQEV(I) = 0.0
      IF(NP.GT.0) THEN
                  DO 220 IP  = 1,NP
                  PMIN(IP)   = PMIT(I,IP)
                  PMBY(I,IP) = 0.0
                  PMOU(I,IP) = 0.0
                  PMRS(I,IP) = 0.0
                  PMRM(IP)   = 0.0
                  PMRN(I,IP) = 0.0
                  IF(IPART(IP).GT.0) THEN
                                     DO 210 KJ     = 1,NNR
                                     PSIN(IP,KJ)   = PSIT(I,IP,KJ)
                                     PSBY(I,IP,KJ) = PSIT(I,IP,KJ)
                                     PSOU(I,IP,KJ) = 0.0
                                     PSRS(I,IP,KJ) = 0.0
  210                                PSRN(IP,KJ)   = 0.0
                                     ENDIF
  220             CONTINUE
                  ENDIF
C=======================================================================
      IF(QQIT(I).GT.QMAX(I)) THEN
                             QQBY(I) = QQIT(I)-QMAX(I)
                             QQIN(I) = QMAX(I)
                             IF(NP.GT.0) THEN
                                DO 320 IP  = 1,NP
                                PMBY(I,IP) = PMIT(I,IP)*QQBY(I)/QQIT(I)
                                PMIN(IP)   = PMIT(I,IP)-PMBY(I,IP)
  320                           CONTINUE
                                ENDIF
                             ENDIF
      IF(QQIN(I).GT.QMAXS(I)) QMAXS(I)=QQIN(I)
C=======================================================================
      IF(IDENT(I).EQ.0) THEN
         IF(IPT.LE.0.OR.KDT.GT.1) GO TO 1020
         DO 1010 KJ = 1,NNR
         FREM(I,KJ) = 0.0
         IF(RAN(KJ,2).LE.PSC(I).OR.RAN(KJ,1).GT.PSC(I)) GO TO 1010
         FREM(I,KJ) = (RAN(KJ,2)-PSC(I))/(RAN(KJ,2)-RAN(KJ,1))
         RAN(KJ,2)  = PSC(I)
 1010    IF(RAN(KJ,2).GT.PSC(I).AND.RAN(KJ,1).GT.PSC(I))FREM(I,KJ)=1.0
 1020    IF(QQIN(I).LE.0.0) GO TO 6000
         QQRS(I) = QQIN(I)*QRF(I)
         QQOU(I) = QQIN(I)-QQRS(I)
         IF(NP.LE.0) GO TO 6000
         DO 1060 IP = 1,NP
         PREM(IP)   = 0.0
C
         IF(IPART(IP).GT.0) THEN
         DO 1030 KJ = 1,NNR
         PREM(IP)   = PREM(IP)+FREM(I,KJ)*PSIN(IP,KJ)
         PSRS(I,IP,KJ) = PSRS(I,IP,KJ)+FREM(I,KJ)*PSIN(IP,KJ)*PMIN(IP)
 1030    PSOU(I,IP,KJ) = PSOU(I,IP,KJ) +
     +                  (1.0-FREM(I,KJ))*PSIN(IP,KJ)*PMIN(IP)
         ELSE
         IF(RMX(I,IP).GT.0.0) THEN
            DO 1050 K = 1,11
            X1(K)     = 1.0
            IF(INPUT(I,IP,K).LE.0) GO TO 1050
            IF(INPUT(I,IP,K).EQ.2) X1(K) = PMIN(1)/(QQIN(I)*DS)
            IF(INPUT(I,IP,K).EQ.3) X1(K) = PMIN(2)/(QQIN(I)*DS)
            IF(INPUT(I,IP,K).EQ.4) X1(K) = PMIN(3)/(QQIN(I)*DS)
            IF(IP.GE.2.AND.INPUT(I,IP,K).EQ.5) X1(K) = PREM(1)
            IF(IP.GE.3.AND.INPUT(I,IP,K).EQ.6) X1(K) = PREM(2)
            IF(INPUT(I,IP,K).EQ.7)             X1(K) = QQIN(I)
 1050       CONTINUE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            CALL EQUATE(I,IP,X1,PREM(IP))
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            ENDIF
         ENDIF
 1060    CONTINUE
         DO 1080 IP = 1,NP
         PMOU(I,IP) = PMIN(IP)*(1.0-PREM(IP))
         PMRS(I,IP) = PMIN(IP)-PMOU(I,IP)
         IF(IPART(IP).GT.0) THEN
                            DO 1070 KJ = 1,NNR
                            IF(PMOU(I,IP).GT.0.0) PSOU(I,IP,KJ) =
     +                                    PSOU(I,IP,KJ)/PMOU(I,IP)
 1070                       IF(PMRS(I,IP).GT.0.0) PSRS(I,IP,KJ) =
     +                                    PSRS(I,IP,KJ)/PMRS(I,IP)
                            ENDIF
 1080    CONTINUE
         GO TO 6000
         ENDIF
C
      QOUT   = 0.0
      QRES   = 0.0
      STORE  = 0.0
      STORE1 = 0.0
      DEPTH  = 0.0
      SO2DT2 = 0.0
      IF(QQIN(I)+WARN(I).LE.0.0.AND.KDT.GT.1) GO TO 2100
      MIN = 1
      DO 2010 MM = 1,JINT
      SDE(MM)    = SDEPTH(I,MM)
      SAR(MM)    = SAREA(I,MM)
      SST(MM)    = SSTORE(I,MM)
 2010 CONTINUE
      IF(KDT.GT.1) GO TO 2100
      NDTS(I)   = 0
      ION(I)    = 0
      KQRS(I)   = 0
      QRESL(I)  = 0.0
      QOUTL(I)  = 0.0
      DEPTHL(I) = 0.0
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF(IOUT(I).GE.2)   CALL INTERP(SDE,SST,JINT,DSTOP(I),SSTOP(I))
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 2020 IF(WARN(I).GT.0.0) CALL INTERP(SST,SDE,JINT,WARN(I),DEPTHL(I))
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 2100 IQRS = 0
      IF(IDRAW(I)) 2110,2200,2120
 2110 KQRS(I) = KQRS(I)+1
      IF(WARN(I).LE.0.0)       NDTS(I) = 0
      IF(KQRS(I).EQ.-IDRAW(I)) NDTS(I) = 1
      IF(KQRS(I).EQ.-IDRAW(I)) KQRS(I) = 0
      IF(QQIN(I)+QOUTL(I).LE.0.0.AND.NDTS(I).GT.0) IQRS = 1
      GO TO 2130
 2120 IF(QQIN(I)+QOUTL(I).GT.0.0) NDTS(I) = 0
      IF(QQIN(I)+QOUTL(I).LE.0.0) NDTS(I) = NDTS(I)+1
      IF(NDTS(I).GE.IDRAW(I))     IQRS = 1
 2130 IF(IQRS.GT.0) GO TO 2300
C
 2200 IF(QQIN(I)+WARN(I).LE.0.0) GO TO 2700
      IF(IOUT(I).LE.1)           GO TO 2300
      IF(DEPTHL(I).LE.DSTOP(I))    ION(I) = 0
      IF(DEPTHL(I).GE.DSTART(I,1)) ION(I) = 1
      IF(DEPTHL(I).GE.DSTART(I,2)) ION(I) = 2
      IF(DEPTHL(I).LE.DSTART(I,1).AND.ION(I).EQ.2) ION(I) = 1
      KION = ION(I)
      IF(ION(I).GT.0) SO2DT2   = QPUMP(I,KION) * DS/2.0
      IF(ION(I).GT.0) QOUTL(I) = QPUMP(I,KION)
      IF(ION(I).LE.0) QOUTL(I) = 0.0
      SAT(MIN)  = SSTOP(I)  + SO2DT2
      SAT(JINT) = SST(JINT) + SO2DT2
C
 2300 IF(IOUT(I).LT.2) THEN
                       DO 2310 MM = 1,JINT
                       IF(IQRS.GT.0) SAO(MM) = SQQRS(I,MM)*DS/2.0
                       IF(IQRS.LE.0) SAO(MM) = SQQOU(I,MM)*DS/2.0
                       SAT(MM) =     SST(MM) + SAO(MM)
                       IF(SAO(MM).LE.0.0) MIN = MM
 2310                  CONTINUE
                       ENDIF
C
      IF(QQIN(I).GT.0.0) QRESL(I)=0.0
      STERMS = (QQIN(I)-(QOUTL(I)+QRESL(I))/2.0)*DS+WARN(I)
      IF(IQRS.LE.0.AND.STERMS.LE.SAT(MIN).AND.QOUTL(I).GT.0.0)
     +                 STERMS = SAT(MIN)
      IF(IQRS.GT.0.AND.STERMS.LE.SAT(MIN).AND.QRESL(I).GT.0.0)
     +                 STERMS  = SAT(MIN)
      IF(STERMS.GT.SAT(JINT)) THEN
                              FLOOD = STERMS-SAT(JINT)
                              IF(NP.GT.0) THEN
                              DO 2410 IP = 1,NP
                              PMBY(I,IP) = PMBY(I,IP) +
     +                                     FLOOD*PMIN(IP)/(QQIN(I)*DS)
 2410                         PMIN(IP)   = PMIN(IP)*
     +                                     (1.0-FLOOD/(QQIN(I)*DS))
                              ENDIF
                              QQBY(I)    = QQBY(I)+FLOOD/DS
                              QQIN(I)    = QQIN(I)-FLOOD/DS
                              STERMS     = SAT(JINT)
                              JFLOOD     = JFLOOD + 1
                              IF(JFLOOD.EQ.1) WRITE(N6,2430)
     +                                 I,MONTH,NDAY,NYEAR,JHR,MINUTE
                              ENDIF
      IF(STERMS.LE.0.0001) STERMS = 0.0
C
      IF(IOUT(I).GE.2.AND.IQRS.LE.0) GO TO 2510
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL INTERP(SAT,SAO,JINT,STERMS,SO2DT2)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 2510                       STORE    = STERMS-SO2DT2
      IF(STORE.LE.0.001)    STORE    = 0.0
      IF(STORE.GT.VMAXS(I)) VMAXS(I) = STORE
      IF(IQRS.LE.0) THEN
                    QOUT    = SO2DT2*2.0/DS
                    QQOU(I) = QQIN(I)+(WARN(I)-STORE)/DS
                    IF(QQOU(I).LE.0.0001) QQOU(I) = 0.0
                    ELSE
                    QRES    = SO2DT2*2.0/DS
                    QQRS(I) = (WARN(I)-STORE)/DS
                    IF(QQRS(I).LE.0.0001) QQRS(I) = 0.0
                    ENDIF
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL INTERP(SST,SAR,JINT,STORE,AREA)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF(METRIC.EQ.1.AND.STORE.GT.0.0) QQEV(I)=AREA*E(MONTH)/1036800.0
      IF(METRIC.EQ.2.AND.STORE.GT.0.0) QQEV(I)=AREA*E(MONTH)/8.64E7
      STORE1 = STORE - QQEV(I)*DS
      IF(STORE1.LE.0.0) THEN
                        QQEV(I) = STORE/DS
                        STORE1  = 0.0
                        ENDIF
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL INTERP(SST,SDE,JINT,STORE1,DEPTH)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 2700 IF(NP.LE.0)   GO TO 5000
      IF(IROUTE(I)) 4000,4000,3000
C=======================================================================
C     COMPLETE MIXING FLOW ROUTING WITH WATER QUALITY
C=======================================================================
 3000 IF(KDT.LE.1) THEN
                   DO 3010 IP = 1,NP
 3010              C1(I,IP)   = PC0(I,IP)
                   ENDIF
      IF(QQIN(I)+WARN(I).LE.0.0) GO TO 5000
      DO 3130 IP = 1,NP
      PREM(IP)   = 0.0
      IF(RMX(I,IP).LE.0.0) GO TO 3120
      DO 3110 K = 1,11
      X1(K)     = 1.0
      IF(INPUT(I,IP,K).LE.0) GO TO 3110
      IF(INPUT(I,IP,K).EQ.1)             X1(K) = DS
      IF(IP.GE.2.AND.INPUT(I,IP,K).EQ.5) X1(K) = PREM(1)
      IF(IP.GE.3.AND.INPUT(I,IP,K).EQ.6) X1(K) = PREM(2)
 3110 CONTINUE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL EQUATE(I,IP,X1,PREM(IP))
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 3120 DIV = STORE*(1.0+PREM(IP)/2.0)+(QOUT+QRES)*DS/2.0
      C2  = 0.0
      IF(DIV.GT.0.0)C2 = (PMIN(IP)+C1(I,IP)*(WARN(I)*(1.0-PREM(IP)/2.0)-
     +                   (QOUTL(I)+QRESL(I))*DS/2.0))/DIV
      IF(C2.LE.0.0.OR.STORE.LE.0.0) C2 = 0.0
      PMRM(IP) = PREM(IP)*(C1(I,IP)*WARN(I)+C2*STORE)/2.0
      POR      = PMIN(IP)+C1(I,IP)*WARN(I)-PMRM(IP)-C2*STORE
      IF(POR.LE.0.0) POR = 0.0
      IF(IQRS.LE.0) PMOU(I,IP) = POR
      IF(IQRS.GT.0) PMRS(I,IP) = POR
      PMRN(I,IP)               = C2*STORE
      C1(I,IP)                 = C2
 3130 CONTINUE
      GO TO 5000
C=======================================================================
C     PLUG FLOW ROUTING WITH WATER QUALITY
C=======================================================================
 4000 IF(QQIN(I)+WARN(I).LE.0.0)      GO TO 4200
      IF(KDT.GT.1.AND.WARN(I).GT.0.0) GO TO 4010
      KKDT(I)  = 0
      LLDT(I)  = 0
      LPREV(I) = 1
 4010 IF(QQIN(I).LE.0.0.AND.KDT.GT.1) GO TO 4100
      KKDT(I) = KKDT(I)+1
                         LLDT(I) = LLDT(I)+1
      IF(LLDT(I).GT.NPL) LLDT(I) = 1
      KLDT        = LLDT(I)
      KIN(I,KLDT) = KDT
      IF(IPT.LE.0) GO TO 4020
      DEP          = (DEPTH+DEPTHL(I))/2.0
      ADEP(I,KLDT) = DEP
      ASTP(I,KLDT) = 1.0
 4020 VLIN(I,KLDT) = QQIN(I)*DS
      IF(KDT.LE.1.AND.WARN(I).GT.0.0) THEN
                      MPL       = NPL/2
                      DO 4029 J = 1,MPL
 4029                 VLIN(I,J) = VLIN(I,J) + WARN(I)/FLOAT(MPL)
                      KKDT(I)   = MPL
                      LLDT(I)   = MPL
                      LPREV(I)  = MPL
                      ENDIF
      DO 4040 IP       = 1,NP
      PMINN(I,IP,KLDT) = PMIN(IP)
      PCINN(I,IP,KLDT) = 0.0
      IF(KDT.LE.1) PMINN(I,IP,1) = PMINN(I,IP,1)+PC0(I,IP)*WARN(I)
      IF(VLIN(I,KLDT).GT.0.0) PCINN(I,IP,KLDT) = PMINN(I,IP,KLDT)/
     1                                           VLIN(I,KLDT)
      IF(IPART(IP).LE.0) GO TO 4040
      DO 4030 KJ             = 1,NNR
 4030 PSINN(I,IP,LLDT(I),KJ) = PSIT(I,IP,KJ)
 4040 CONTINUE
 4100                  LOUT1 = LPREV(I)
      IF(LOUT1.GT.NPL) LOUT1 = LOUT1-NPL
      VOUT = QQOU(I)*DS
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL PLUGS(I,NPL,VLIN,VOUT,KKDT(I),LPREV(I),FRAC,LLIN,IER)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      LOUT2 = LPREV(I)
      IF(IER.GT.0) THEN
                   WRITE(N6,4110) I,NPL,MONTH,NDAY,NYEAR,JHR,MINUTE
                   STOP
                   ENDIF
      IF(VOUT.LE.0.0) GO TO 4200
      DO 4170 KP = LOUT1,LOUT2
      DO 4130 IP = 1,NP
 4130 PREM(IP)   = 0.0
      KK         = KP
      IF(KP.GT.NPL)KK     = KP-NPL
      FR                  = 1.0
      IF(KP.EQ.LOUT2)  FR = FRAC
      IF(FR.LE.0.0001) FR = 0.0
      DNT                 = FLOAT(KDT-KIN(I,KK))*DS
      IF(IPT.LE.0)GO TO 4150
                      AADEP = ADEP(I,KK)
      IF(METRIC.EQ.2) AADEP = AADEP/0.3048
      DO 4145 KJ = 1,NNR
      FREM(I,KJ) = 0.0
      IF(DNT.LE.0.0) GO TO 4140
      VUP   = AADEP/DNT
      ALPHA = VS(KJ)*(AADEP**(1.0/6.0))/((ALEN(I)/DNT)*AMAN(I)*5.675)
      IF(ALPHA.GT.1.0)  ALPHA = 1.0
      IF(ALPHA.LE.0.01) ALPHA = 0.01
                       FREMQ  = VS(KJ)/VUP
      IF(FREMQ.GE.1.0) FREMQ  = 1.0
                       FREMT  = 1.0
      IF(VS(KJ)/VUP.LE.10.0) FREMT = 1.0-EXP(-VS(KJ)/VUP)
      FREM(I,KJ) = FREMQ+(ALOG(ALPHA)/4.605)*(FREMQ-FREMT)
      IF(FREM(I,KJ).GT.1.0) FREM(I,KJ)   = 1.0
      IF(FREM(I,KJ).LE.0.0001)FREM(I,KJ) = 0.0
 4140 DO 4145 IP = 1,NP
      IF(IPART(IP).LE.0) GO TO 4145
      PREM(IP)      = PREM(IP)+FREM(I,KJ)*PSINN(I,IP,KK,KJ)
      PSSL(I,IP,KJ) = PSSL(I,IP,KJ)+FREM(I,KJ)*PSINN(I,IP,KK,KJ)*
     1                FR*PMINN(I,IP,KK)
      PSOU(I,IP,KJ) = PSOU(I,IP,KJ)+(1.0-FREM(I,KJ))*PSINN(I,IP,KK,KJ)*
     1                FR*PMINN(I,IP,KK)
 4145 CONTINUE
      GO TO 4165
 4150 DO 4160 IP = 1,NP
      IF(IPART(IP).GT.0)   GO TO 4160
      IF(RMX(I,IP).LE.0.0) GO TO 4160
      DO 4155 K = 1,11
      X1(K)     = 1.0
      IF(INPUT(I,IP,K).LE.0) GO TO 4155
      IF(INPUT(I,IP,K).EQ.1) X1(K) = DNT
      IF(INPUT(I,IP,K).EQ.2) X1(K) = PCINN(I,1,KK)
      IF(INPUT(I,IP,K).EQ.3) X1(K) = PCINN(I,2,KK)
      IF(INPUT(I,IP,K).EQ.4) X1(K) = PCINN(I,3,KK)
      IF(INPUT(I,IP,K).EQ.5.AND.IP.GE.2) X1(K) = PREM(1)
      IF(INPUT(I,IP,K).EQ.6.AND.IP.GE.3) X1(K) = PREM(2)
 4155 CONTINUE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL EQUATE(I,IP,X1,PREM(IP))
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 4160 CONTINUE
 4165 IF(KP.EQ.LOUT2) VLIN(I,KK) = VLIN(I,KK)*(1.0-FR)
      DO 4170 IP = 1,NP
      PMOU(I,IP) = PMOU(I,IP)+PMINN(I,IP,KK)*FR*(1.0-PREM(IP))
      PMSL(I,IP) = PMSL(I,IP)+PMINN(I,IP,KK)*FR*PREM(IP)
      IF(KP.EQ.LOUT2) PMINN(I,IP,KK) = PMINN(I,IP,KK)*(1.0-FR)
 4170 CONTINUE
      KPSL     = NPSL(I)
      SLVOL(I) = 0.0
      IF(KPSL.GT.0.AND.SLDEN(I).GT.0.0) SLVOL(I) = PMSL(I,KPSL)/SLDEN(I)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL INTERP(SST,SDE,JINT,SLVOL(I),SLDEP)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF(SLDEP.GT.SLDMAX(I)) WRITE(N6,4175)
     +                       I,MONTH,NDAY,NYEAR,JHR,MINUTE
      DO 4190 IP = 1,NP
      IF(IPART(IP).LE.0) GO TO 4190
      DO 4185 KJ = 1,NNR
 4185 IF(PMOU(I,IP).GT.0.0) PSOU(I,IP,KJ) = PSOU(I,IP,KJ)/PMOU(I,IP)
 4190 CONTINUE
C
 4200 SUM = 0.0
      IF(NP.GT.0) THEN
                  DO 4220 IP = 1,NP
                  IF(IPART(IP).LE.0) GO TO 4220
                  DO 4210 KJ  = 1,NNR
 4210             PSRN(IP,KJ) = PSSL(I,IP,KJ)
 4220             PMRN(I,IP)  = PMSL(I,IP)
                  ENDIF
      IF(WARN(I)+QQIN(I).LE.0.0) GO TO 5000
      DO 4250 KP       = LOUT2,LLIN
                    KK = KP
      IF(KP.GT.NPL) KK = KP-NPL
      SUM        = SUM + VLIN(I,KK)
      DO 4250 IP = 1,NP
      PMRN(I,IP) = PMRN(I,IP) + PMINN(I,IP,KK)
      IF(IPART(IP).LE.0) GO TO 4250
      DO 4240 KJ  = 1,NNR
 4240 PSRN(IP,KJ) = PSRN(IP,KJ)+PSINN(I,IP,KK,KJ)*PMINN(I,IP,KK)
 4250 CONTINUE
C
 4300 VRES = QQRS(I)*DS
      IF(VRES.GT.0.0) THEN
                      KKDT(I)    = 1
                      LLDT(I)    = 1
                      LPREV(I)   = 1
                      LOUT2      = 1
                      KIN(I,1)   = KDT+1
                      ASTP(I,1)  = 0.0
                      ADEP(I,1)  = 0.0
                      RSFR       = VRES/WARN(I)
                      VLIN(I,1)  = SUM*(1.0-RSFR)
                      DO 4320 IP = 1,NP
                      PMRS(I,IP) = PMRN(I,IP)*RSFR
                      PMRN(I,IP) = PMRN(I,IP)*(1.0-RSFR)
                      PMSL(I,IP) = 0.0
                      PMINN(I,IP,1) = PMRN(I,IP)
                      PCINN(I,IP,1) = 0.0
                      IF(VLIN(I,1).GT.0.0) PCINN(I,IP,1) =
     +                                     PMINN(I,IP,1)/VLIN(I,1)
                      IF(IPART(IP).LE.0) GO TO 4320
                      DO 4310 KJ    = 1,NNR
                      PSSL(I,IP,KJ) = 0.0
                      PSRS(I,IP,KJ) = PSRN(IP,KJ)*
     +                                (1.0-RSFR)/PMINN(I,IP,1)
 4310                 PSINN(I,IP,1,KJ)=PSRS(I,IP,KJ)
 4320                 CONTINUE
                      ENDIF
      IF(QQEV(I).GT.0.0) THEN
                         DO 4410 KP = LOUT2,LLIN
                         KK         = KP
                         IF(KP.GT.NPL) KK = KP-NPL
                         VLIN(I,KK) = VLIN(I,KK)*(1.0-QQEV(I)*DS/STORE)
 4410                    CONTINUE
                         ENDIF
      IF(IPT.GT.0) THEN
                    DO 4510 KP = LOUT2,LLIN
                    KK         = KP
                    IF(KP.GT.NPL) KK = KP-NPL
                    TEMP             = ADEP(I,KK)*ASTP(I,KK) +
     +                                 (DEPTHL(I)+DEPTH)/2.0
                    ASTP(I,KK)       = ASTP(I,KK)+1.0
                    ADEP(I,KK)       = TEMP/ASTP(I,KK)
 4510               CONTINUE
                    ENDIF
 5000 WARN(I)   = STORE1
      QOUTL(I)  = QOUT
      QRESL(I)  = QRES
 5010 DEPTHL(I) = DEPTH
C
 6000 IF(QQIN(I).GT.0.0.OR.QQOU(I).GT.0.0.OR.QQRS(I).GT.0.0)
     +    DOP(I) = DOP(I)+DS/3600.0
      RETURN
C=======================================================================
 2430 FORMAT(/,' ===>  Note !! UNIT ',I2,' HAS EXCEEDED THE MAXIMUM STOR
     1AGE VOLUME ON ',I2,'/',I2,'/',I2,' AT ',I2,I3,/,
     2'      The excess has been bypassed.  Further messages will be sup
     3pressed.')
 4110 FORMAT(/,' ===> WARNING !! IN SUBROUTINE UNIT : UNIT ',I2,' CONTAI
     1NS MORE THAN ',I4,' PLUGS ON ',I2,'/',I2,'/',I2,/,36X,'AT ',I2,I3,
     2'. CONSULT USERS MANUAL. SIMULATION TERMINATED.')
 4175 FORMAT(/,' ===> WARNING !! FROM SUBROUTINE UNIT : UNIT ',I2,' HAS
     1EXCEEDED THE MAXIMUM SLUDGE DEPTH ON ',I2,'/',I2,'/',I2,' AT ',I2,
     2I3,'.',/,38X,'SIMULATION CONTINUES.')
C=======================================================================
      END

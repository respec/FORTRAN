      SUBROUTINE CONTRL
C	STORAGE/TREATMENT BLOCK
C	CALLED BY STRT NEAR LINE 590
C=======================================================================
C  UPDATED 9/23/93 BY WCH (RED) TO FIX METRIC OUTPUT OF FLOWS.
C##UNCORRECTED 12/9/94, WCH.  S/T ALREADY IN METRIC.  NO CONVERSION NEC.
C  ADD ZERO-DIVIDE ERROR CHECK FOR ERRST, WCH, 12/5/94.
C  ADD COUNTERS FOR NUMBER OF VOLUME-EXCEEDANCE BYPASSES, WCH, 12/5/94.
C  MINOR FORMAT ADJUSTMENTS, WCH, 12/9/94.
C  ZERO-DIVIDE ERROR CHECK, WCH, 12/4/96.
C  MODIFIED 4/99 to increase number of pollutant and number of units
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'S1.INC'
C=======================================================================
      DIMENSION PBYT(MSTU,MQUAL,3),PCBY(MSTU,MQUAL),PCIN(MSTU,MQUAL),
     a          PCIT(MSTU,MQUAL)
      DIMENSION PCOU(MSTU,MQUAL),PCRS(MSTU,MQUAL),PDEC(MQUAL),
     a          PDIS(MQUAL),PDST(MQUAL,3),PDCY(MQUAL,3)
      DIMENSION PINT(MSTU,MQUAL,3),PITT(MSTU,MQUAL,3),
     a          POUT(MSTU,MQUAL,3),PRCT(MQUAL,3)
      DIMENSION PREC(MQUAL),PRMT(MSTU,MQUAL,3),PRNT(MQUAL),
     a          PRST(MSTU,MQUAL,3),TOTCHK(MSTU)
      DIMENSION WBYT(MSTU,3),WDST(3),WEPT(3),WEVT(MSTU,3),WINT(MSTU,3)
      DIMENSION WITT(MSTU,3),WOUT(MSTU,3),WRCT(3),WRST(MSTU,3),
     a          ERROR(MQUAL+1)
      CHARACTER DMON(13)*4,PDUM1*10,PDUM2(11)*16,PDUM3*1
      CHARACTER PDUM6(8)*20,PDUM7(3)*8,PDUM8(3)*8
C=======================================================================
      DATA DMON/' JAN',' FEB',' MAR',' APR',' MAY','JUNE','JULY',' AUG',
     1          'SEPT',' OCT',' NOV',' DEC','YEAR'/
      DATA PDUM1/'----------'/,PDUM2/
     2     'INFLOW,TOTAL    ','INFLOW,NET      ',
     2     'BYPASS          ','TREATED OUTFLOW ',
     3     'RESIDUAL FLOW   ','REMOVED BY DECAY',
     4     'REMAINING       ','DEPTH           ',
     5     'EVAPORATION     ','INITIAL COND.   ','PERCENT ERROR   '/
      DATA PDUM3/'-'/
      DATA PDUM7/' POUNDS ','QUANTITY',' OTHER  '/
      DATA PDUM8/' KILOGRM','QUANTITY',' OTHER  '/
      DATA PDUM6/' TOTAL TO PLANT     ',' TOTAL TO NEXT BLOCK',
     1           ' TOTAL TO ULT. DISP.',' REMOVED BY DECAY   ',
     2           ' REMAINING IN PLANT ',' TOTAL EVAPORATION  ',
     3           ' INITIAL PLANT TOTAL',' PERCENT ERROR      '/
C=======================================================================
C     Printing summary:
C     ISUM  ISUM1 IS
C       0      3   3  Summary at end of simulation only.
C       1      2   2  Summary at end and annual.
C       2      1   1  Summary at end, annual and monthly.
C=======================================================================
      IF(KDT.EQ.1) THEN
                   ISUM1      = 3-ISUM
                   IDONE      = 0
                   IDONE1     = 0
                   IPAGE      = 0
                   NP1        = NP+1
                   LLTM       = MONTH
                   LLTY       = NYEAR
                   DO 270 IS  = ISUM1,3
                   WRCT(IS)   = 0.0
                   WDST(IS)   = 0.0
                   WEPT(IS)   = 0.0
                   DO 250 I   = 1,NU
                   WITT(I,IS) = 0.0
                   WINT(I,IS) = 0.0
                   WBYT(I,IS) = 0.0
                   WEVT(I,IS) = 0.0
                   WOUT(I,IS) = 0.0
  250              WRST(I,IS) = 0.0
                   IF(NP.GT.0) THEN
                               DO 275 IP   = 1,NP
                               PRCT(IP,IS) = 0.0
                               PDCY(IP,IS) = 0.0
                               PDST(IP,IS) = 0.0
                               DO 275 I      = 1,NU
                               PITT(I,IP,IS) = 0.0
                               PINT(I,IP,IS) = 0.0
                               PBYT(I,IP,IS) = 0.0
                               POUT(I,IP,IS) = 0.0
                               PRMT(I,IP,IS) = 0.0
                               PRST(I,IP,IS) = 0.0
  275                          CONTINUE
                               ENDIF
  270              CONTINUE
                   DO 290 I = 1,NU
                   IDONE    = IDONE  + IDENT(I)
  290              IDONE1   = IDONE1 + IROUTE(I)
                   IF(NU.EQ.1) WRITE(N6,4495)
                   ENDIF
C=======================================================================
C     START HERE WHEN KDT > 1
C=======================================================================
      WRNT = 0.0
      WREC = 0.0
      WDIS = 0.0
      IF(NP.GT.0) THEN
                  DO 350 IP = 1,NP
                  PRNT(IP)  = 0.0
                  PREC(IP)  = 0.0
                  PDEC(IP)  = 0.0
                  PDIS(IP)  = 0.0
  350             PCRC(IP)  = 0.0
                  ENDIF
      DO 1580 I = 1,NU
      QQIT(I)   = 0.0
      IF(NP.GT.0) THEN
                  DO 1040 IP = 1,NP
                  PMIT(I,IP) = 0.0
                  PCIT(I,IP) = 0.0
                  PCIN(I,IP) = 0.0
                  PCBY(I,IP) = 0.0
                  PCOU(I,IP) = 0.0
                  PCRS(I,IP) = 0.0
                  IF(IPART(IP).GT.0) THEN
                                     DO 1030 KJ    = 1,NNR
 1030                                PSIT(I,IP,KJ) = 0.0
                                     ENDIF
 1040             CONTINUE
                  ENDIF
      IF(I.GT.1) GO TO 1100
C=======================================================================
C     First unit receives average inflow from input sources.
C=======================================================================
      QQIT(I) = QQTP
      IF(NP.LE.0) GO TO 1100
      DO 1090 IP = 1,NP
C=======================================================================
C     PMIT = conc x flow rate x dt = load (e.g., mass).
C=======================================================================
      PMIT(I,IP) = PCTP(IP)*QQTP*DS
      IF(IPART(IP).LE.0) GO TO 1090
      DO 1070 KJ    = 1,NNR
 1070 PSIT(I,IP,KJ) = PSD(IP,KJ)
 1090 CONTINUE
C=======================================================================
 1100 IF(I.LE.1)GO TO 1200
      IM1        = I-1
      DO 1180 II = 1,IM1
      DO 1180 IK = 1,3
      IF(IDIREC(II,IK).NE.I) GO TO 1180
      IF(IK-2) 1120,1140,1160
 1120 QQIT(I) = QQIT(I)+QQBY(II)
      IF(NP.LE.0) GO TO 1180
      DO 1130 IP = 1,NP
      PMIT(I,IP) = PMIT(I,IP)+PMBY(II,IP)
      IF(IPART(IP).LE.0) GO TO 1130
      DO 1125 KJ    = 1,NNR
 1125 PSIT(I,IP,KJ) = PSIT(I,IP,KJ)+PSBY(II,IP,KJ)*PMBY(II,IP)
 1130 CONTINUE
      GO TO 1180
 1140 QQIT(I) = QQIT(I)+QQOU(II)
      IF(NP.LE.0) GO TO 1180
      DO 1150 IP = 1,NP
      PMIT(I,IP) = PMIT(I,IP)+PMOU(II,IP)
      IF(IPART(IP).LE.0) GO TO 1150
      DO 1145 KJ    = 1,NNR
 1145 PSIT(I,IP,KJ) = PSIT(I,IP,KJ)+PSOU(II,IP,KJ)*PMOU(II,IP)
 1150 CONTINUE
      GO TO 1180
 1160 QQIT(I) = QQIT(I)+QQRS(II)
      IF(NP.LE.0) GO TO 1180
      DO 1170 IP = 1,NP
      PMIT(I,IP) = PMIT(I,IP)+PMRS(II,IP)
      IF(IPART(IP).LE.0) GO TO 1170
      DO 1165 KJ    = 1,NNR
 1165 PSIT(I,IP,KJ) = PSIT(I,IP,KJ)+PSRS(II,IP,KJ)*PMRS(II,IP)
 1170 CONTINUE
 1180 CONTINUE
      IF(IPT.LE.0.0) GO TO 1200
      DO 1190 IP = 1,NP
      IF(IPART(IP).LE.0) GO TO 1190
      DO 1185 KJ = 1,NNR
 1185 IF(PMIT(I,IP).GT.0.0) PSIT(I,IP,KJ)=PSIT(I,IP,KJ)/PMIT(I,IP)
 1190 CONTINUE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 1200 CALL UNIT(I)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 1300 IF(NP.GT.0) THEN
            DO 1350 IP = 1,NP
            IF(QQIT(I).GT.0.0) PCIT(I,IP)=PMIT(I,IP)/(QQIT(I)*DS)
            IF(QQIN(I).GT.0.0) PCIN(I,IP)=PMIN(IP)/(QQIN(I)*DS)
            IF(QQBY(I).GT.0.0) PCBY(I,IP)=PMBY(I,IP)/(QQBY(I)*DS)
            IF(QQOU(I).GT.0.0) PCOU(I,IP)=PMOU(I,IP)/(QQOU(I)*DS)
            IF(QQRS(I).GT.0.0) PCRS(I,IP)=PMRS(I,IP)/(QQRS(I)*DS)
            PMRN(I,IP)                   =PMRN(I,IP)/PCONV(IP)
 1350       CONTINUE
            ENDIF
C
      DO 1480 IK = 1,3
      IF(IDIREC(I,IK)-100) 1480,1405,1440
 1405 IF(IK-2)             1410,1420,1430
 1410 WREC = WREC+QQBY(I)*DS
      IF(NP.GT.0) THEN
                  DO 1415 IP = 1,NP
 1415             PREC(IP)   = PREC(IP)+PMBY(I,IP)
                  ENDIF
      GO TO 1480
 1420 WREC = WREC+QQOU(I)*DS
      IF(NP.GT.0) THEN
                  DO 1425 IP = 1,NP
 1425             PREC(IP)   = PREC(IP)+PMOU(I,IP)
                  ENDIF
      GO TO 1480
 1430 WREC = WREC+QQRS(I)*DS
      IF(NP.GT.0) THEN
                  DO 1435 IP = 1,NP
 1435             PREC(IP)   = PREC(IP)+PMRS(I,IP)
                  ENDIF
      GO TO 1480
 1440 IF(IK-2) 1445,1455,1465
 1445 WDIS = WDIS+QQBY(I)*DS
      IF(NP.GT.0) THEN
                  DO 1450 IP = 1,NP
 1450             PDIS(IP)   = PDIS(IP)+PMBY(I,IP)
                  ENDIF
      GO TO 1480
 1455 WDIS = WDIS+QQOU(I)*DS
      IF(NP.GT.0) THEN
                  DO 1460 IP = 1,NP
 1460             PDIS(IP)   = PDIS(IP)+PMOU(I,IP)
                  ENDIF
      GO TO 1480
 1465 WDIS = WDIS+QQRS(I)*DS
      IF(NP.GT.0) THEN
                  DO 1470 IP = 1,NP
 1470             PDIS(IP)   = PDIS(IP)+PMRS(I,IP)
                  ENDIF
 1480 CONTINUE
      WRNT = WRNT+WARN(I)
      IF(NP.GT.0) THEN
                  DO 1490 IP = 1,NP
                  PDEC(IP)   = PDEC(IP) + PMRM(IP)
 1490             PRNT(IP)   = PRNT(IP) + PMRN(I,IP)
                  ENDIF
      DO 1580 IS = ISUM1,3
      WITT(I,IS) = WITT(I,IS)+QQIT(I)*DS
      WINT(I,IS) = WINT(I,IS)+QQIN(I)*DS
      WBYT(I,IS) = WBYT(I,IS)+QQBY(I)*DS
      WOUT(I,IS) = WOUT(I,IS)+QQOU(I)*DS
      WRST(I,IS) = WRST(I,IS)+QQRS(I)*DS
      WEVT(I,IS) = WEVT(I,IS)+QQEV(I)*DS
      WEPT(IS)   = WEPT(IS)+QQEV(I)*DS
      IF(NP.GT.0) THEN
                  DO 1530 IP = 1,NP
                  PITT(I,IP,IS) = PITT(I,IP,IS)+PMIT(I,IP)/PCONV(IP)
                  PINT(I,IP,IS) = PINT(I,IP,IS)+PMIN(IP)/PCONV(IP)
                  PBYT(I,IP,IS) = PBYT(I,IP,IS)+PMBY(I,IP)/PCONV(IP)
                  POUT(I,IP,IS) = POUT(I,IP,IS)+PMOU(I,IP)/PCONV(IP)
                  PRMT(I,IP,IS) = PRMT(I,IP,IS)+PMRM(IP)/PCONV(IP)
                  PRST(I,IP,IS) = PRST(I,IP,IS)+PMRS(I,IP)/PCONV(IP)
 1530             CONTINUE
                  ENDIF
      IF(I.EQ.NU) THEN
                  WRCT(IS) = WRCT(IS)+WREC
                  WDST(IS) = WDST(IS)+WDIS
                  IF(NP.GT.0) THEN
                     DO 1570 IP  = 1,NP
                     PRCT(IP,IS) = PRCT(IP,IS)+PREC(IP)/PCONV(IP)
                     PDCY(IP,IS) = PDCY(IP,IS)+PDEC(IP)/PCONV(IP)
 1570                PDST(IP,IS) = PDST(IP,IS)+PDIS(IP)/PCONV(IP)
                     ENDIF
                  ENDIF
 1580 CONTINUE
      QQRC = WREC/DS
      IF(NP.GT.0) THEN
                  DO 1590 IP = 1,NP
 1590             IF(WREC.GT.0.0) PCRC(IP) = PREC(IP)/WREC
                  ENDIF
      IF(IDET.LE.0) GO TO 3000
      DO 2020 L = 1,NPR
      IF(JULDAY.GE.ISTART(L).AND.JULDAY.LE.IEND(L)) GO TO 2030
 2020 CONTINUE
      GO TO 3000
 2030 TOTA = 0.0
      DO 2070 I = 1,NU
      TOTCHK(I) = QQIT(I)+QQOU(I)+QQRS(I)
 2070 TOTA      = TOTA+TOTCHK(I)
C=======================================================================
      IF(MOD(KDT,IDET).NE.0.OR.TOTA.LE.0.0) GO TO 3000
C=======================================================================
      IGO = 1
 2205 IG  = IGO
      IF(IPAGE.GT.0.OR.NP.GT.0) GO TO 2213
      IF(METRIC.EQ.1) WRITE(N6,2210)  (PDUM1,IM=1,3)
      IF(METRIC.EQ.2) WRITE(N6,2211)  (PDUM1,IM=1,3)
 2213 IF(IPAGE.GT.0.OR.NP.LE.0) GO TO 2217
c
      WRITE(N6,2214) (PNAME(IP),IP=1,NP)
      IF(METRIC.EQ.1) THEN
         WRITE(N6,2215) (PUNIT(IP),IP=1,NP)
	ELSE
	   WRITE(N6,2216) (PUNIT(IP),IP=1,NP)
	ENDIF
	WRITE(N6,2218) (PDUM1,IP=1,2*NP)
c      IF(METRIC.EQ.1) WRITE(N6,2215) (PNAME(IP),IP=1,3),
c     1                (PUNIT(IP),IP=1,3),(PDUM1,IP=1,3+NP)
c      IF(METRIC.EQ.2) WRITE(N6,2216) (PNAME(IP),IP=1,3),
c     1                (PUNIT(IP),IP=1,3),(PDUM1,IP=1,3+NP)
 2217 DO 2290 I = IG,NU
      IF(TOTCHK(I).LE.0.0) GO TO 2285
      IPAGE = IPAGE+1
      IF(NP) 2220,2220,2250
 2220 IF(I.EQ.1) WRITE(N6,2225) DMON(MONTH),NDAY,NYEAR,JHR,MINUTE,
     1                          I,PDUM2(1),QQIT(I),PDUM3,PDUM3
      IF(I.GT.1) WRITE(N6,2230) I,PDUM2(1),QQIT(I),PDUM3,PDUM3
      WRITE(N6,2235) PDUM2(2),QQIN(I),PDUM3,PDUM3
      WRITE(N6,2235) PDUM2(3),QQBY(I),PDUM3,PDUM3
      WRITE(N6,2235) PDUM2(4),QQOU(I),PDUM3,PDUM3
      WRITE(N6,2235) PDUM2(5),QQRS(I),PDUM3,PDUM3
      IF(IDENT(I).GT.0) THEN
                        WRITE(N6,2240) PDUM2(7),PDUM3,WARN(I),PDUM3
                        WRITE(N6,2243) PDUM2(8),PDUM3,PDUM3,DEPTHL(I)
                        WRITE(N6,2245) PDUM2(9),QQEV(I),PDUM3,PDUM3
                        ENDIF
      GO TO 2285
C#### WCH (RED), 9/93.  ADJUST QQIT FOR METRIC OUTPUT.
C#### WCH, 12/9/94.  UPON REFLECTION, WRONG.  S/T ALREADY IN METRIC!
 2250 IF(I.EQ.1) WRITE(N6,2255) DMON(MONTH),NDAY,NYEAR,JHR,MINUTE,I,
C####     +PDUM2(1),QQIT(I)/CMET(8,METRIC),PDUM3,PDUM3,(PCIT(I,IP),IP=1,NP)
     +PDUM2(1),QQIT(I),PDUM3,PDUM3,(PCIT(I,IP),IP=1,NP)
      IF(I.GT.1) WRITE(N6,2260) I,PDUM2(1),QQIT(I),
     1                          PDUM3,PDUM3,(PCIT(I,IP),IP=1,NP)
C#### WCH (RED), 9/93.  ADJUST Q'S FOR METRIC OUTPUT.
C#### WCH, 12/9/94.  NO.  S/T ALREADY IN METRIC.
      WRITE(N6,2265) PDUM2(2),QQIN(I),PDUM3,PDUM3,
     1     (PCIN(I,IP),IP=1,NP)
      WRITE(N6,2265) PDUM2(3),QQBY(I),PDUM3,PDUM3,
     1     (PCBY(I,IP),IP=1,NP)
      WRITE(N6,2265) PDUM2(4),QQOU(I),PDUM3,PDUM3,
     1     (PCOU(I,IP),IP=1,NP)
      WRITE(N6,2265) PDUM2(5),QQRS(I),PDUM3,PDUM3,
     1     (PCRS(I,IP),IP=1,NP)
      IF(IDENT(I).GT.0) THEN
          WRITE(N6,2270) PDUM2(7),PDUM3,WARN(I),(PDUM3,IP=1,NP+1)
          WRITE(N6,2275) PDUM2(8),PDUM3,PDUM3,DEPTHL(I),(PDUM3,IP=1,NP)
          WRITE(N6,2280) PDUM2(9),QQEV(I),(PDUM3,IP=1,NP+2)
          ENDIF
 2285 IF(IPAGE.LT.8) GO TO 2290
      IPAGE = 0
      IGO   = I+1
      IF(IGO.LE.NU) GO TO 2205
 2290 CONTINUE
C=======================================================================
C     Printing summary:
C     ISUM  ISUM1 IS
C       0      3   3  Summary at end of simulation only.
C       1      2   2  Summary at end and annual.
C       2      1   1  Summary at end, annual and monthly.
C=======================================================================
 3000 IPR1 = 0
      IPR2 = 0
      IF(KDT.GE.NDT) THEN
                     IPR1 = ISUM1
                     IPR2 = 3
                     GO TO 4070
                     ENDIF
      IF(ISUM.LE.0) GO TO 4070
      IF(ISUM.EQ.2) GO TO 4050
      IF(NYEAR.NE.LLTY) IPR1 = 2
      IPR2 = IPR1
      GO TO 4070
 4050 IF(MONTH.NE.LLTM) IPR1 = 1
      IPR2 = IPR1
      IF(NYEAR.NE.LLTY) IPR2 = 2
 4070 IF(IPR1.LE.0.OR.IPR2.LE.0) RETURN
C=======================================================================
      IPAGE      = 0
      DO 4590 IS = IPR1,IPR2
      IF(IS.EQ.2) LLTM = 13
      IF(IS.NE.3) WRITE(N6,4130) DMON(LLTM),LLTY
      IF(IS.EQ.3) WRITE(N6,4150) DMON(NBD(2)),NBD(3),NBD(1),NBD(4),
     1            NBD(5),NBD(6),DMON(MONTH),NDAY,NYEAR,JHR,MINUTE,JSEC
 4200 DO 4290 I = 1,NU
      IF(NP) 4210,4210,4250
C=======================================================================
C     WATER QUALITY IS NOT SIMULATED
C=======================================================================
 4210 IF(METRIC.EQ.1.AND.I.EQ.1) WRITE(N6,4220)
      IF(METRIC.EQ.2.AND.I.EQ.1) WRITE(N6,4225)
      WRITE(N6,4230) I,PDUM2(1),WITT(I,IS)
      WRITE(N6,4240) PDUM2(2),WINT(I,IS)
      WRITE(N6,4240) PDUM2(3),WBYT(I,IS)
      WRITE(N6,4240) PDUM2(4),WOUT(I,IS)
      WRITE(N6,4240) PDUM2(5),WRST(I,IS)
      ERROR(1) = WBYT(I,IS) + WOUT(I,IS) + WRST(I,IS)
      IF(IDENT(I).GT.0) THEN
                        WRITE(N6,4240) PDUM2(7),WARN(I)
                        WRITE(N6,4240) PDUM2(9),WEVT(I,IS)
C#### WCH, 12/5/94.  ADD PRINT OF NO. VOL. BYPASSES DURING INTERVAL.
                        WRITE(N6,4241) JFLOOD(I,IS)
                        WRITE(N6,4242) JFDAY(I,IS)
                        ERROR(1) = ERROR(1) + WARN(I) + WEVT(I,IS)
                        ENDIF
      ERRST = WITT(I,IS) + VOLINC(I)
C#### WCH, 12/5/94.  CHECK FOR ZERO ERRST.
      IF(ERRST.NE.0.0) THEN
           ERROR(1) = 100.0 * (ERRST - ERROR(1)) / ERRST
           ELSE
           ERROR(1) = 0.0
           ENDIF
      IF(IS.EQ.3) WRITE(N6,4240) PDUM2(10),VOLINC(I)
      IF(IS.EQ.3) WRITE(N6,4276) PDUM2(11),ERROR(1)
      GO TO 4290
C=======================================================================
C     WATER QUALITY IS SIMULATED
C=======================================================================
c 4250 IF(METRIC.EQ.1.AND.I.EQ.1) WRITE(N6,4260) (PNAME(IP),IP=1,3),
c     +                                  (PDUM7(NDIM(IP)+1),IP=1,NP)
c      IF(METRIC.EQ.2.AND.I.EQ.1) WRITE(N6,4265) (PNAME(IP),IP=1,3),
c     +                                  (PDUM8(NDIM(IP)+1),IP=1,NP)
 4250	IF (I.EQ.1) THEN
	WRITE(N6,4260) (PNAME(IP),IP=1,NP)
	IF (METRIC.EQ.1) THEN
	WRITE(N6,4261) (PDUM7(NDIM(IP)+1),IP=1,NP)
	ELSE
	WRITE(N6,4265) (PDUM8(NDIM(IP)+1),IP=1,NP)
	ENDIF
	WRITE(N6,4263) (PDUM1,IP=1,NP1)
	ENDIF
CIM
cim      IF(I.EQ.1) WRITE(N6,4263) (PDUM1,IP=1,NP1)
      WRITE(N6,4270) I,PDUM2(1),WITT(I,IS),(PITT(I,IP,IS),IP=1,NP)
      WRITE(N6,4275)   PDUM2(2),WINT(I,IS),(PINT(I,IP,IS),IP=1,NP)
      WRITE(N6,4275)   PDUM2(3),WBYT(I,IS),(PBYT(I,IP,IS),IP=1,NP)
      WRITE(N6,4275)   PDUM2(4),WOUT(I,IS),(POUT(I,IP,IS),IP=1,NP)
      WRITE(N6,4275)   PDUM2(5),WRST(I,IS),(PRST(I,IP,IS),IP=1,NP)
      ERROR(1)    = WBYT(I,IS)    + WOUT(I,IS)    + WRST(I,IS)
      DO 9250 IP  = 1,NP
 9250 ERROR(IP+1) = PBYT(I,IP,IS) + POUT(I,IP,IS) + PRST(I,IP,IS)
      IF(IDENT(I).GT.0) THEN
              IF(IROUTE(I).GE.1) WRITE(N6,4280) PDUM2(6),PDUM3,
     +                           (PRMT(I,IP,IS),IP=1,NP)
              WRITE(N6,4275) PDUM2(7),WARN(I),(PMRN(I,IP),IP=1,NP)
              WRITE(N6,4285) PDUM2(9),WEVT(I,IS),(PDUM3,IP=1,NP)
C#### WCH, 12/5/94.  ADD PRINT OF NO. VOL. BYPASSES DURING INTERVAL.
              WRITE(N6,4241) JFLOOD(I,IS)
              WRITE(N6,4242) JFDAY(I,IS)
              ERROR(1) = ERROR(1) + WARN(I) + WEVT(I,IS)
              DO 9260 IP  = 1,NP
              IF(IROUTE(I).GE.1) ERROR(IP+1) = ERROR(IP+1)+PRMT(I,IP,IS)
 9260         ERROR(IP+1)                    = ERROR(IP+1)+PMRN(I,IP)
              ENDIF
      ERRST       = WITT(I,IS) + VOLINC(I)
C#### WCH, 12/5/94.  CHECK FOR ZERO ERRST.
      IF(ERRST.NE.0.0) THEN
           ERROR(1) = 100.0 * (ERRST - ERROR(1)) / ERRST
           ELSE
           ERROR(1) = 0.0
           ENDIF
      DO 9270 IP  = 1,NP
      ERRST       = PITT(I,IP,IS) + POLINC(I,IP)
C#### WCH, 12/4/96.  AGAIN, CHECK FOR ZERO ERRST.
      IF(ERRST.GT.0.0) THEN
          ERROR(IP+1) = 100.0 * (ERRST - ERROR(IP+1)) / ERRST
          ELSE
          ERROR(IP+1) = 0.0
          ENDIF
 9270 CONTINUE
C#### 9270 ERROR(IP+1) = 100.0 * (ERRST - ERROR(IP+1)) / ERRST
      IF(IS.EQ.3) WRITE(N6,4275) PDUM2(10),VOLINC(I),
     +                          (POLINC(I,IP),IP=1,NP)
      IF(IS.EQ.3) WRITE(N6,4276) PDUM2(11),ERROR(1),
     +                          (ERROR(IP+1),IP=1,NP)
 4290 CONTINUE
C=======================================================================
C     IF THERE IS ONLY ONE UNIT DO NOT PRINT THE PLANT SUMMARY
C=======================================================================
      IF(NU.GT.1) THEN
         IF(IS.NE.3) WRITE(N6,4330) DMON(LLTM),LLTY
         IF(IS.EQ.3) WRITE(N6,4350) DMON(NBD(2)),NBD(3),NBD(1),NBD(4),
     +            NBD(5),NBD(6),DMON(MONTH),NDAY,NYEAR,JHR,MINUTE,JSEC
 4400    IF(NP) 4410,4410,4450
C=======================================================================
C     WATER QUALITY IS NOT SIMULATED
C=======================================================================
 4410    IF(METRIC.EQ.1) WRITE(N6,4420)
         IF(METRIC.EQ.2) WRITE(N6,4430)
         WRITE(N6,4440) PDUM6(1),WITT(1,IS)
         WRITE(N6,4440) PDUM6(2),WRCT(IS)
         WRITE(N6,4440) PDUM6(3),WDST(IS)
         ERROR(1) = WRCT(IS) + WDST(IS)
         IF(IDONE.GT.0) THEN
                     ERROR(1) = ERROR(1) + WRNT + WEPT(IS)
                     WRITE(N6,4440) PDUM6(5),WRNT
                     WRITE(N6,4440) PDUM6(6),WEPT(IS)
                     ENDIF
         VOLC         = 0.0
         DO 4411 JJ   = 1,NU
 4411    VOLC         = VOLC + VOLINC(JJ)
         ERRST        = WITT(1,IS) + VOLC
         ERROR(1)     = 100.0 * (ERRST - ERROR(1)) / ERRST
         IF(IS.EQ.3) WRITE(N6,4440) PDUM6(7),VOLC
         IF(IS.EQ.3) WRITE(N6,4441) PDUM6(8),ERROR(1)
         GO TO 4500
C=======================================================================
C     WATER QUALITY IS SIMULATED
C=======================================================================
c 4450    IF(METRIC.EQ.1) WRITE(N6,4460) (PNAME(IP),IP=1,3),
c     +                       (PDUM7(NDIM(IP)+1),IP=1,NP)
c         IF(METRIC.EQ.2) WRITE(N6,4470) (PNAME(IP),IP=1,3),
c     +                       (PDUM8(NDIM(IP)+1),IP=1,NP)
 4450    WRITE(N6,4460) (PNAME(IP),IP=1,NP)
         IF(METRIC.EQ.1) THEN
	      WRITE(N6,4461) (PDUM7(NDIM(IP)+1),IP=1,NP)
	   ELSE
	      WRITE(N6,4470) (PDUM8(NDIM(IP)+1),IP=1,NP)
	   ENDIF
         WRITE(N6,4465) (PDUM1,IP=1,NP1)
         WRITE(N6,4480) PDUM6(1),WITT(1,IS),(PITT(1,IP,IS),IP=1,NP)
         WRITE(N6,4480) PDUM6(2),WRCT(IS),(PRCT(IP,IS),IP=1,NP)
         WRITE(N6,4480) PDUM6(3),WDST(IS),(PDST(IP,IS),IP=1,NP)
         ERROR(1)    = WRCT(IS)    + WDST(IS)
         DO 9350 IP  = 1,NP
 9350    ERROR(IP+1) = PRCT(IP,IS) + PDST(IP,IS)
         IF(IDONE.GT.0) THEN
               ERROR(1) = ERROR(1) + WRNT + WEPT(IS)
               IF(IDONE1.GE.1) WRITE(N6,4485) PDUM6(4),PDUM3,
     +                                        (PDCY(IP,IS),IP=1,NP)
               WRITE(N6,4480) PDUM6(5),WRNT,(PRNT(IP),IP=1,NP)
               WRITE(N6,4490) PDUM6(6),WEPT(IS),(PDUM3,IP=1,NP)
               DO 9360 IP  = 1,NP
               IF(IDONE1.GE.1) ERROR(IP+1) = ERROR(IP+1) + PDCY(IP,IS)
 9360                          ERROR(IP+1) = ERROR(IP+1) + PRNT(IP)
               ENDIF
         VOLC         = 0.0
         DO 4481 JJ   = 1,NU
         VOLC         = VOLC + VOLINC(JJ)
         DO 4481 IP   = 1,NP
 4481    IF(JJ.GT.1)    POLINC(1,IP) = POLINC(1,IP) + POLINC(JJ,IP)
         ERRST        = WITT(1,IS) + VOLC
         ERROR(1)     = 100.0 * (ERRST - ERROR(1)) / ERRST
         DO 9370 IP  = 1,NP
         ERRST       = PITT(1,IP,IS) + POLINC(1,IP)
 9370    ERROR(IP+1) = 100.0 * (ERRST - ERROR(IP+1)) / ERRST
         IF(IS.EQ.3) WRITE(N6,4480) PDUM6(7),VOLC,(POLINC(1,IP),IP=1,NP)
         IF(IS.EQ.3) WRITE(N6,4441) PDUM6(8),ERROR(1),
     1                             (ERROR(IP+1),IP=1,NP)
         ENDIF
C=======================================================================
 4500 WRCT(IS) = 0.0
      WDST(IS) = 0.0
      WEPT(IS) = 0.0
      IF(NP.GT.0) THEN
                  DO 4510 IP=1,NP
                  PRCT(IP,IS)=0.0
                  PDCY(IP,IS)=0.0
 4510             PDST(IP,IS)=0.0
                  ENDIF
      DO 4570 I  = 1,NU
      WITT(I,IS) = 0.0
      WINT(I,IS) = 0.0
      WBYT(I,IS) = 0.0
      WOUT(I,IS) = 0.0
      WRST(I,IS) = 0.0
      WEVT(I,IS) = 0.0
C#### WCH, 12/5/94.  RESET FLOODING BYPASS COUNTERS.
      JFLOOD(I,IS) = 0
      JFDAY(I,IS)  = 0
      IF(NP.GT.0) THEN
                  DO 4550 IP    = 1,NP
                  PITT(I,IP,IS) = 0.0
                  PINT(I,IP,IS) = 0.0
                  PBYT(I,IP,IS) = 0.0
                  POUT(I,IP,IS) = 0.0
                  PRMT(I,IP,IS) = 0.0
 4550             PRST(I,IP,IS) = 0.0
                  ENDIF
 4570 CONTINUE
 4590 CONTINUE
      LLTM       = MONTH
      LLTY       = NYEAR
C=======================================================================
CIM CHANGE 3( to 99( and change some formats for more NP's
 2040 FORMAT(/,1X,'DETAILED PERFORMANCE FOR ',I6,' TO ',I6,/)
C#### WCH, 12/9/94.  REMOVE NEXT-T0-LAST 2X FROM 2210 AND 2211.
 2210 FORMAT(1H1,//,1X,'DATE',8X,'TIME',3X,'UNIT',6X,'PARAMETER',
     18X,'FLOW',7X,'STORAGE',6X,'DEPTH',/,
     2' MO/DY/YEAR',4X,' HR:MIN ',29X,'CFS',8X,'CU FT',8X,
     2'FT',/,1X,13('-'),2X,7('-'),2X,4('-'),2X,16('-'),99(2X,A10))
 2211 FORMAT(1H1,//,5X,'DATE',8X,'TIME',3X,'UNIT',6X,'PARAMETER',
     18X,'FLOW',7X,'STORAGE',6X,'DEPTH',/,
     2' MO/DY/YEAR',4X,' HR:MIN ',26X,'CU M/SEC',6X,'CU M',10X,
     2'M',/,1X,13('-'),2X,7('-'),2X,4('-'),2X,16('-'),99(2X,A10))
c 2215 FORMAT(1H1,//,5X,'DATE',8X,'TIME',3X,'UNIT',6X,'PARAMETER',
c     18X,'FLOW',7X,'STORAGE',6X,'DEPTH',1X,3(4X,A8),/,
c     2' MO/DY/YEAR',4X,' HR:MIN ',29X,'CFS',
c     28X,'CU FT',8X,'FT',3X,3(4X,A8),/,1X,13('-'),2X,7('-'),
c     32X,4('-'),2X,16('-'),6(2X,A10))
c 2216 FORMAT(1H1,//,5X,'DATE',8X,'TIME',3X,'UNIT',6X,'PARAMETER',
c     18X,'FLOW',7X,'STORAGE',6X,'DEPTH',1X,3(4X,A8),/,
c     2' MO/DY/YEAR',4X,' HR:MIN ',26X,'CU M/SEC',
c     26X,'CU M',10X,'M',3X,3(4X,A8),/,1X,13('-'),2X,7('-'),2X,4('-'),
c     32X,16('-'),6(2X,A10))
 2214 FORMAT(1H1,//,5X,'DATE',8X,'TIME',3X,'UNIT',6X,'PARAMETER',
     18X,'FLOW',7X,'STORAGE',6X,'DEPTH',1X,99(4X,A8))
 2215 FORMAT(' MO/DY/YEAR',4X,' HR:MIN ',29X,'CFS',
     28X,'CU FT',8X,'FT',3X,99(4X,A8))
 2216 FORMAT(' MO/DY/YEAR',4X,' HR:MIN ',26X,'CU M/SEC',
     26X,'CU M',10X,'M',3X,99(4X,A8))
 2218 FORMAT(1X,13('-'),2X,7('-'),2X,4('-'),2X,16('-'),99(2X,A10))
CIM
 2225 FORMAT(/,1X,1A4,1X,I2,', ',I4,2X,I2,I4,3X,I2,4X,A16,2X,1PE10.4,
     1         8X,A1,11X,A1)
 2230 FORMAT(/,35X,I2,4X,A16,2X,1PE10.4,8X,A1,11X,A1)
C#### WCH, 12/9/94.  CHANGE 3X TO 2X TO AGREE WITH 2265.
 2235 FORMAT(31X,A16,2X,1PE10.4,8X,A1,11X,A1)
 2240 FORMAT(31X,A16,8X,A1,5X,1PE10.4,8X,A1)
 2243 FORMAT(31X,A16,8X,A1,11X,A1,5X,1PE10.4)
 2245 FORMAT(31X,A16,2X,1PE10.4,8X,A1,11X,A1)
 2255 FORMAT(/,1X,A4,1X,I2,', ',I4,2X,I2,I4,3X,I2,4X,A16,2X,1PE10.4,
     18X,A1,11X,A1,3X,99(2X,1PE10.4))
C#### WCH, 12/9/94.  CHANGE A 3X TO A 2X TO AGREE WITH 2230 FORMAT. 
 2260 FORMAT(/,35X,I2,4X,A16,2X,1PE10.4,8X,A1,11X,A1,3X,99(2X,1PE10.4))
 2265 FORMAT(31X,A16,2X,1PE10.4,8X,A1,11X,A1,3X,99(2X,1PE10.4))
 2270 FORMAT(31X,A16,8X,A1,5X,1PE10.4,8X,A1,99(11X,A1))
 2275 FORMAT(31X,A16,8X,A1,11X,A1,5X,1PE10.4,8X,A1,99(11X,A1))
 2280 FORMAT(31X,A16,2X,1PE10.4,8X,A1,99(11X,A1))
 4130 FORMAT('1',/,
     +' ***************************************************',/,
     +' *        UNIT PERFORMANCE SUMMARIES FOR ',A4,' ',I4,' *',/,
     +' ***************************************************',/)
 4150 FORMAT('1',/,
     +' ***************************************',/,
     +' *    ENTIRE SIMULATION UNIT SUMMARIES *',/,
     +' ***************************************',/,
     11X,1A4,1X,I2,', ',I4,', ',I2,I2,':',I2,' TO ',1A4,1X,I2,
     2', ',I4,', ',I2,I2,':',I2,'.',//)
 4220 FORMAT(1X,'UNIT',6X,'PARAMETER',7X,'VOLUME',/,28X,'CU FT',/,
     11X,4('-'),2X,16('-'),2X,10('-'))
 4225 FORMAT(1X,'UNIT',6X,'PARAMETER',7X,'VOLUME',/,28X,'CU M',/,
     11X,4('-'),2X,16('-'),2X,10('-'))
 4230 FORMAT(/,1X,I2,3X,A16,2X,1PE10.4)
C#### WCH, 12/5/94.  CHANGE INITIAL 1X TO 6X.
 4240 FORMAT(6X,A16,2X,1PE10.4)
C#### WCH, 12/5/94.  NEW 4241, 4242 FORMATS.
 4241 FORMAT(6X,'NO. VOL. BYPASSES',I11)
 4242 FORMAT(6X,'NO. VOL. BYPASS DAYS',I8)
 4260 FORMAT(1X,'UNIT',6X,'PARAMETER',7X,'VOLUME',1X,99(4X,A8))
 4261 FORMAT(28X,'CU FT',1X,99(4X,A8))
 4263 FORMAT(4('-'),2X,16('-'),99(2X,A10))
 4265 FORMAT(28X,'CU M',2X,99(4X,A8))
CIM CHANGE FROM 4( to 99(
 4270 FORMAT(/,1X,I2,3X,A16,99(2X,1PE10.4))
 4275 FORMAT(6X,A16,99(2X,1PE10.4))
 4276 FORMAT(6X,A16,99(2X,F10.4))
 4280 FORMAT(6X,A16,8X,A1,3X,99(2X,1PE10.4))
 4285 FORMAT(6X,A16,2X,1PE10.4,8X,A1,99(11X,A1))
 4330 FORMAT('1',/,
     +' *******************************************',/,
     +' * PLANT PERFORMANCE SUMMARY FOR ',A4,' ',I4,' *',/,
     +' *******************************************',/)
 4350 FORMAT('1',/,
     +' ***********************************************',/,
     +' * ENTIRE SIMULATION PLANT PERFORMANCE SUMMARY *',/
     +' ***********************************************',//,
     11X,1A4,1X,I2,' ',I4,', ',I2,I2,':',I2,
     2' TO ',A4,1X,I2,', ',I4,', ',I2,I2,':',I2,'.',//)
 4420 FORMAT(6X,'PARAMETER',9X,'VOLUME',/,25X,'CU FT',/,1X,20('-'),
     1       2X,10('-'))
 4430 FORMAT(6X,'PARAMETER',9X,'VOLUME',/,25X,'CU M',/,1X,20('-'),
     1       2X,10('-'))
 4440 FORMAT(1X,A20,2X,1PE10.4)
 4441 FORMAT(1X,A20,99(2X,F10.4))
c 4460 FORMAT(6X,'PARAMETER',9X,'VOLUME',1X,3(4X,A8),/,25X,'CU FT',
c     1                                                1X,3(4X,A8))
 4460 FORMAT(6X,'PARAMETER',9X,'VOLUME',1X,99(4X,A8))
 4461 FORMAT(25X,'CU FT',1X,99(4X,A8))
 4465 FORMAT(1X,20('-'),99(2X,A10))
c 4470 FORMAT(6X,'PARAMETER',9X,'VOLUME',1X,3(4X,A8),/,25X,'CU M',
c     1                                               2X,3(4X,A8))
 4470 FORMAT(25X,'CU M',2X,99(4X,A8))
 4480 FORMAT(1X,A20,99(2X,1PE10.4))
 4485 FORMAT(1X,A20,8X,A1,3X,99(2X,1PE10.4))
 4490 FORMAT(1X,A20,2X,1PE10.4,8X,A1,99(11X,A1))
 4495 FORMAT(//,
     +' **************************************************',/,
     +' * NOTE: IF THERE IS ONLY ONE UNIT THE PLANT      *',/,
     +' *       PERFORMANCE SUMMARY WILL NOT BE PRINTED. *',/,
     +' **************************************************',/)
C=======================================================================
      RETURN
      END

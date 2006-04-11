      SUBROUTINE STRDAT
C=======================================================================
C     INPUT SUBROUTINE FOR STORAGE/TREATMENT BLOCK.
C     LAST UPDATED: JANUARY 1988.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'S1.INC'
C=======================================================================
      DIMENSION IRES(5)
      CHARACTER*16 PDUM1(2),PDUM6(3)*1,PDUM4(7,4)*4,PDUM5(2,3)*5,
     +             PDUM2(4,2)*4,PDUM3(24)*4
C=======================================================================
      DATA PDUM1/' NON-DETENTION  ',' DETENTION     '/
      DATA PDUM2/'PLUG',' FLO','W   ','    ','COMP','LETE','LY M',
     2'IXED'/,PDUM3/'DRAW','N OF','F EV','ERY ',' TIM','E ST','EPS ',
     3'    ','NEVE','R DR','AWN ','OFF ','    ','    ','    ','    ',
     4'DRAW','N OF','F AF','TER ',' DRY',' TIM','E ST','EPS '/
      DATA PDUM4/'REMO','VAL ','EQUA','TION','    ','    ','    ',
     1'PART','ICLE',' SET','TLIN','G   ','    ','    ','CRIT','ICAL',
     2' PAR','TICL','E SI','ZE  ','    ','CRIT','ICAL',' SET','TLIN',
     3'G VE','LOCI','TY  '/,PDUM5/'MICR','ONS ','FT/S','EC  ','CM/S',
     4'EC  '/,PDUM6/' ','*','+'/
C
      DO 2495 I = 1,NU
C=======================================================================
C>>>>>>>>  READ DATA GROUP F1 AND F2  <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,UNAME(I)
      READ(N5,*,ERR=888) CC,IDENT(I),QMAX(I),QRF(I),(IDIREC(I,J),J=1,3)
C
      DO 1190 J=1,3
      IF(IDIREC(I,J).GT.I) GO TO 1190
      WRITE(N6,1170) I
      STOP
 1190 CONTINUE
C
      IF(NP.LE.0) GO TO 1300
C=======================================================================
C>>>>>>>>  READ DATA GROUP G1  <<<<<<<<
C=======================================================================
      DO 1270 IP = 1,NP
      IF(IPART(IP).GT.0) GO TO 1270
C
      READ(N5,*,ERR=888) CC,RMX(I,IP)
      IF(RMX(I,IP).LE.0.0) RMX(I,IP) = 0.0
      IF(RMX(I,IP).GE.1.0) RMX(I,IP) = 1.0
C=======================================================================
C>>>>>>>>  READ DATA GROUP G2 AND G3  <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,(INPUT(I,IP,K),K=1,11)
      READ(N5,*,ERR=888) CC,(A(I,IP,K),K=1,16)
 1270 CONTINUE
C=======================================================================
C>>>>>>>>  READ DATA GROUP G4 <<<<<<<<
C=======================================================================
      IF(IPT.GT.0.AND.IDENT(I).LE.0) READ(N5,*,ERR=888) CC,PSC(I)
C
 1300 IROUTE(I) = 0
      IF(IDENT(I).LE.0) GO TO 1400
C=======================================================================
C>>>>>>>>  READ DATA GROUP H1 <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,IROUTE(I),IOUT(I),IDRAW(I),IRES(I)
      IF(IROUTE(I).LE.0.OR.IPT.LE.0) GO TO 1310
      WRITE(N6,1305) I
      STOP
C=======================================================================
C>>>>>>>>  READ DATA GROUP H2  <<<<<<<<
C=======================================================================
 1310 IF(IPT.GT.0) READ(N5,*,ERR=888) CC,ALEN(I),AMAN(I)
C=======================================================================
C>>>>>>>>  READ DATA GROUP GROUP H3  <<<<<<<<
C=======================================================================
      NINT(I)    = 0
      STORE      = 0.0
      DO 1315 MM = 1,17
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'H3') GO TO 1320
      READ(N5,*,ERR=888) CC,SDEPTH(I,MM),SAREA(I,MM),
     +                   SSTORE(I,MM),SQQOU(I,MM),SQQRS(I,MM)
      IF(SDEPTH(I,1)+SSTORE(I,1).GT.0.0) THEN
                                         WRITE(N6,1313) I
                                         STOP
                                         ENDIF
      NINT(I) = NINT(I) + 1
      STORE   = STORE   + SSTORE(I,MM)
 1315 CONTINUE
 1320 JINT = NINT(I)
      IF(STORE.GT.0.0) GO TO 1330
      DO 1325 MM   = 2,JINT
 1325 SSTORE(I,MM) = SSTORE(I,MM-1)+(SDEPTH(I,MM)-SDEPTH(I,MM-1))*
     1              (SAREA(I,MM)+SAREA(I,MM-1))/2.0
 1330 VMAX(I)      = SSTORE(I,JINT)
C
      IF(IOUT(I)-1) 1370,1340,1350
C=======================================================================
C>>>>>>>>  READ DATA GROUP H4  <<<<<<<<
C=======================================================================
 1340 READ(N5,*,ERR=888) CC,C1,D0,C2
      DO 1345 MM  = 1,JINT
      SQQOU(I,MM) = 0.0
      IF(SDEPTH(I,MM)-D0.GT.0.0001) SQQOU(I,MM)=C1*(SDEPTH(I,MM)-D0)**C2
 1345 CONTINUE
      GO TO 1370
C=======================================================================
C>>>>>>>>  READ DATA GROUP H5  <<<<<<<<
C=======================================================================
 1350 READ(N5,*,ERR=888) CC,DSTART(I,1),DSTART(I,2),QPUMP(I,1),
     +                      QPUMP(I,2),DSTOP(I)
      IF(DSTART(I,2).GE.DSTART(I,1)) GO TO 1360
      WRITE(N6,1355) I
 1360 IF(DSTOP(I).LE.DSTART(I,1)) GO TO 1370
      WRITE(N6,1365) I
      STOP
 1370 IF(IRES(I).LE.0) GO TO 1385
C=======================================================================
C>>>>>>>>  READ DATA GROUP H6  <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,C3,D1,C4
      DO 1380 MM  = 1,JINT
      SQQRS(I,MM) = 0.0
      IF(SDEPTH(I,MM)-D1.GT.0.0001) SQQRS(I,MM)=C3*(SDEPTH(I,MM)-D1)**C4
 1380 CONTINUE
 1385 NPSL(I) = 0
C=======================================================================
C>>>>>>>>  READ DATA GROUP H7  <<<<<<<<
C=======================================================================
      IF(NP.GE.1.AND.IROUTE(I).LE.0) READ(N5,*,ERR=888) CC,NPSL(I),
     +                                           SLDEN(I),SLDMAX(I)
C=======================================================================
C>>>>>>>>  READ DATA GROUP H8  <<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,WARN(I),(PC0(I,IP),IP=1,NP)
C=======================================================================
C>>>>>>>>  READ DATA GROUP I  <<<<<<<<
C=======================================================================
 1400 IF(ICOST.GT.0) READ(N5,*,ERR=888) CC,KPC(I,1),CCC(I,1),
     +               CCC(I,2),KPC(I,2),CCC(I,3),CCC(I,4),CCC(I,5)
C
      IF(I.EQ.1) WRITE (N6,2050)
      IF(I.GT.1) WRITE(N6,2130)
      WRITE(N6,2150) I
C
      ID1 = IDENT(I)+1
      WRITE(N6,2210)    UNAME(I),PDUM1(ID1)
      IF(METRIC.EQ.1)   WRITE(N6,2220) QMAX(I)
      IF(METRIC.EQ.2)   WRITE(N6,2230) QMAX(I)
      IF(IDENT(I).LE.0) WRITE(N6,2240) QRF(I)
      WRITE(N6,2250)   (IDIREC(I,J),J=1,3)
C
      IF(NP.GT.0) THEN
                  WRITE(N6,2310)
                  DO 2390 IP = 1,NP
                  IF(IPART(IP).GT.0) GO TO 2320
                  IMEC = 1
                  GO TO 2330
 2320             IMEC=2
                  IF(IDENT(I).GT.0) GO TO 2330
                  IMEC = 3
                  IF(NVS.GT.0) IMEC = 4
 2330             WRITE(N6,2340) PNAME(IP),(PDUM4(IN,IMEC),IN=1,7)
                  IF(IPART(IP).LE.0.OR.IDENT(I).GT.0) GO TO 2360
                  IMEC1 = IMEC-2+METRIC
                  WRITE(N6,2350) (PDUM4(IN,IMEC),IN=1,7),PSC(I),
     +                           (PDUM5(IN,IMEC1),IN=1,2)
                  GO TO 2390
 2360             IF(IPART(IP).GT.0) GO TO 2390
                  WRITE(N6,2370) RMX(I,IP),(INPUT(I,IP,K),K=1,11),
     +                           (A(I,IP,J),J=1,16)
 2390             CONTINUE
                  ENDIF
C
      IF(IDENT(I).LE.0) GO TO 2495
      WRITE(N6,2405)
      IR1 = IROUTE(I)+1
      IF(NP.GT.0) WRITE(N6,2410) (PDUM2(IN,IR1),IN=1,4)
      IDW  = 9
      IDW3 = 16
      NDW  = 0
      IF(IDRAW(I)) 2413,2418,2415
 2413 IDW  = 1
      IDW3 = 8
      NDW  = -IDRAW(I)
      GO TO 2418
 2415 IDW  = 17
      IDW3 = 24
      NDW  = IDRAW(I)
 2418 IDW1 = IDW+3
      IDW2 = IDW+4
      IF(IDRAW(I).EQ.0) WRITE(N6,2420)(PDUM3(IN),IN=IDW,IDW1)
      IF(IDRAW(I).NE.0) WRITE(N6,2420) (PDUM3(IN),IN=IDW,IDW1),
     +                            NDW,(PDUM3(IN),IN=IDW2,IDW3)
      IF(METRIC.EQ.1.AND.IPT.GT.0) WRITE(N6,2430)ALEN(I),AMAN(I)
      IF(METRIC.EQ.2.AND.IPT.GT.0) WRITE(N6,2433)ALEN(I),AMAN(I)
      IF(METRIC.EQ.2) ALEN(I) = ALEN(I)/0.3048
      IPUMP = 1
      IF(IOUT(I).GE.1) IPUMP = 2
      KR = 1
      IF(IRES(I).GE.1) KR = 3
      IF(METRIC.EQ.1) WRITE(N6,2435) PDUM6(IPUMP),PDUM6(KR)
      IF(METRIC.EQ.2) WRITE(N6,2437) PDUM6(IPUMP),PDUM6(KR)
      DO 2440 MM = 1,JINT
 2440 WRITE(N6,2445) SDEPTH(I,MM),SAREA(I,MM),SSTORE(I,MM),SQQOU(I,MM),
     1               SQQRS(I,MM)
      IF(IOUT(I).NE.1) GO TO 2449
      WRITE (N6,2447) PDUM6(2),C1,D0,C2
      GO TO 2458
 2449 IF(IOUT(I).NE.2) GO TO 2458
      WRITE(N6,2450)
      IF(METRIC.EQ.1) WRITE(N6,2455) DSTART(I,1),DSTART(I,2),QPUMP(I,1),
     1                               QPUMP(I,2),DSTOP(I)
      IF(METRIC.EQ.2) WRITE(N6,2457) DSTART(I,1),DSTART(I,2),QPUMP(I,1),
     1                               QPUMP(I,2),DSTOP(I)
 2458 IF(IRES(I).GE.1) WRITE (N6,2447) PDUM6(3),C3,D1,C4
      IF(NPSL(I).GT.0) THEN
                       WRITE(N6,2460)
                       NPSAVE = NPSL(I)
                       WRITE(N6,2465) PNAME(NPSAVE),PUNIT(NPSAVE),
     +                                SLDEN(I)
                       IF(METRIC.EQ.1) WRITE(N6,2470) SLDMAX(I)
                       IF(METRIC.EQ.2) WRITE(N6,2473) SLDMAX(I)
                       ENDIF
      IF(METRIC.EQ.1) WRITE(N6,2480) WARN(I)
      IF(METRIC.EQ.2) WRITE(N6,2483) WARN(I)
      VOLINC(I) = WARN(I)
      IF(NP.GT.0) THEN
                  WRITE(N6,2485)
                  DO 2490 IP = 1,NP
 2490             WRITE(N6,2493) PNAME(IP),PUNIT(IP),PC0(I,IP)
                  WRITE(N6,2486)
                  DO 2492 IP = 1,NP
                  IF(METRIC.EQ.1) PLOAD = WARN(I)*PC0(I,IP)/PCONV(IP)
                  IF(METRIC.EQ.2) PLOAD = WARN(I)*PC0(I,IP)/PCONV(IP)
                  POLINC(I,IP)  = PLOAD
                  IF(METRIC.EQ.1) WRITE(N6,3493)  PNAME(IP),PLOAD
 2492             IF(METRIC.EQ.2) WRITE(N6,3494)  PNAME(IP),PLOAD
                  ENDIF
 2495 CONTINUE
      RETURN
  888 CALL IERROR
C=======================================================================
 1170 FORMAT(/,' ===> ERROR !! IN DATA GROUP F2 : FLOW FROM UNIT ',I2,
     1'IS NOT DIRECTED TO A UNIT WITH',/,32X,'A GREATER NUMBER. SIMULATI
     2ON TERMINATED.')
 1305 FORMAT(/,' ===> ERROR !! IN DATA GROUP H1 : UNIT ',I2,' HAS BEEN C
     1HARACTERIZED AS COMPLETLY MIXED AND AT LEAST ONE POLLUTANT',/,32X,
     2'HAS BEEN CHARACTERIZED BY A PARTICLE SIZE/VELOCITY DISTRIBUTION.
     3SIMULATION TERMINATED.')
 1313 FORMAT(/,' ===> ERROR !! IN DATA GROUP H3 : THE FIRST ENTRIES FOR
     1DEPTH AND VOLUME FOR UNIT ',I2,/,32X,'ARE NON-ZERO. SIMULATION TER
     2MINATED.')
 1355 FORMAT(/,' ===> ERROR !! IN DATA GROUP H5 : IN UNIT ',I2,', THE DE
     1PTH AT WHICH THE SECOND PUMPING RATE BEGINS IS LESS',/,32X,'THAN T
     2HE DEPTH AT WHICH THE FIRST PUMPING RATE BEGINS. SIMULATION TERMIN
     3ATED.')
 1365 FORMAT(/,' ===> ERROR !! IN DATA GROUP H5 : IN UNIT ',I2,', THE DE
     1PTH AT WHICH ALL PUMPING STOPS IS GREATER THAN',/,32X,'THE DEPTH A
     2T WHICH PUMPING BEGINS. SIMULATION TERMINATED.')
 2050 FORMAT(1H1,//,10X,'UNIT CHARACTERISTICS',//)
 2130 FORMAT(1H1)
 2150 FORMAT(10X,25('*'),/,10X,'UNIT # ',I2,' CHARACTERISTICS',/,10X,
     125('*'),//)
 2210 FORMAT(10X,'GENERAL UNIT DESCRIPTION:',//,10X,'NAME
     1      : ',A18,/,10X,'TYPE OF UNIT             : ',A16)
 2220 FORMAT(10X,'MAX. ALLOWABLE INFLOW,CFS: ',G10.4)
 2230 FORMAT(10X,'MAX. ALLOWABLE INFLOW,CMS: ',G10.4)
 2240 FORMAT(10X,'RES. FLOW, FRAC. OF INFL.: ',G10.4)
 2250 FORMAT(//,10X,'FLOW DIRECTIONS:          ',/,
     +          10X,'BYPASS TO UNIT #          ',I3,/,
     1          10X,'TREATED OUTFLOW TO UNIT # ',I3,/,
     +          10X,'RESIDUALS TO UNIT #       ',I3,//,
     210X,'===> NOTE: UNIT 100 IS THE NEXT BLOCK AND UNIT 200 IS ULTIMAT
     3E DISPOSAL')
 2310 FORMAT(//,10X,'POLLUTANT REMOVAL MECHANISM:')
 2340 FORMAT(/,10X,'POLLUTANT          : ',A8,/,
     +         10X,'REMOVAL MECHANISM  : ',7A4)
 2350 FORMAT(10X,7A4,': ',1PE8.3,1X,2A4)
 2370 FORMAT(10X,'MAX. REMOVAL FRAC. : ',F10.4,/,10X,'VARIABLE TYPES
     1 : ',11(4X,I1),/,10X,'COEFFICIENT VALUES : ',8(1X,1PE10.3),/,31X,
     28               (1X,1PE10.3))
 2405 FORMAT(//,10X,'DETENTION UNIT CHARACTERISTICS:',/)
 2410 FORMAT(10X,'POLLUTANT ROUTING METHOD : ',4A4)
 2420 FORMAT(10X,'RESIDUALS DRAW-OFF SCHEME: ',4A4,I6,4A4)
 2430 FORMAT(10X,'TRAVEL LENGTH,FT         : ',1PE10.4,/,10X,'MANNING CO
     1EF.            : ',1PE10.4)
 2433 FORMAT(10X,'TRAVEL LENGTH,M          : ',E10.4,/,10X,'MANNING COEF
     1.            : ',E10.4)
 2435 FORMAT(/,10X,'DEPTH-AREA-STORAGE-FLOW RELATIONSHIPS :',//,15X,
     1'DEPTH,FT',4X,'SURFACE AREA,SQ FT',4X,'STORAGE,CU FT',4X,
     2'TREATED OUTFLOW,CFS',A1,3X,'RESIDUAL FLOW,CFS',A1,/)
 2437 FORMAT(/,10X,'DEPTH-AREA-STORAGE-FLOW RELATIONSHIPS :',//,15X,
     1'DEPTH,M ',4X,'SURFACE AREA,SQ M ',4X,'STORAGE,CU M ',4X,
     2'TREATED OUTFLOW,CMS',A1,3X,'RESIDUAL FLOW,CMS',A1,/)
 2445 FORMAT(13X,1PE10.4,8X,1PE10.4,10X,1PE10.4,10X,1PE10.4,12X,1PE10.4)
 2447 FORMAT (/,10X,A1,' GOVERNED BY POWER EQUATION:',/,10X,'OUTFLOW =',
     1 F8.2,' * (DEPTH -',F7.2,') **',F6.3)
 2450 FORMAT(/,10X,'* GOVERNED BY PUMPING',/)
 2455 FORMAT(10X,'PUMPED OUTFLOW:',//,15X,'DEPTH AT WHICH FIRST PUMPING
     1RATE BEGINS,FT  : ',1PE10.4,/,15X,'DEPTH AT WHICH SECOND PUMPING R
     2ATE BEGINS,FT : ',1PE10.4,/,15X,'FIRST PUMPING RATE,CFS
     3            : ',1PE10.4,/,15X,'SECOND PUMPING RATE,CFS
     4          : ',1PE10.4,/,15X,'DEPTH AT WHICH ALL PUMPING STOPS,FT
     5        : ',1PE10.4)
 2457 FORMAT(10X,'PUMPED OUTFLOW:',//,15X,'DEPTH AT WHICH FIRST PUMPING
     1RATE BEGINS,M   : ',1PE10.4,/,15X,'DEPTH AT WHICH SECOND PUMPING R
     2ATE BEGINS,M  : ',1PE10.4,/,15X,'FIRST PUMPING RATE,CU M/SEC
     3            : ',1PE10.4,/,15X,'SECOND PUMPING RATE,CU M/SEC
     4           : ',1PE10.4,/,15X,'DEPTH AT WHICH ALL PUMPING STOPS,M
     5  : ',1PE10.4)
 2460 FORMAT(/,10X,'SLUDGE GENERATION:')
 2465 FORMAT(/,15X,'POLLUTANT DETERMINING SLUDGE GENERATION : ',2X,A8,
     1/,15X,'CONCENTRATION IN SLUDGE,',A8,'        : ',1PE10.4)
 2470 FORMAT(15X,'MAXIMUM SLUDGE DEPTH,FT                 : ',1PE10.4)
 2473 FORMAT(15X,'MAXIMUM SLUDGE DEPTH,M                  : ',1PE10.4)
 2480 FORMAT(/,10X,'INITIAL CONDITIONS:',//,15X,'VOLUME,CU FT
     1      : ',1PE10.4)
 2483 FORMAT(/,10X,'INITIAL CONDITIONS:',//,15X,'VOLUME,CU M
     1      : ',1PE10.4)
 2485 FORMAT(15X,'POLLUTANT CONCENTRATIONS     :')
 2486 FORMAT(15X,'INITIAL POLLUTANT LOAD       :')
 2493 FORMAT(20X,A8,',',A8,7X,': ',1PE10.4)
 3493 FORMAT(20X,A8,',',15X,': ',1PE10.4,' POUNDS')
 3494 FORMAT(20X,A8,',',15X,': ',1PE10.4,' KILOGRAMS')
C=======================================================================
      END

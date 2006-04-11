      SUBROUTINE OTRAIN
C=======================================================================
C     Output Subroutine for the University of Florida Transport Model
C                Last updated December, 1990 BY R.E.D.
C     WCH (CDM - Chuck Moore), add on-screen display of continuity error.
C     WCH, 3/28/94.  Add metric conversion for QF in surcharge summary.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'DRWF.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'NAMES.INC'
      INCLUDE 'TST.INC'
      INCLUDE 'NEW81.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEWTR.INC'
C=======================================================================
      CHARACTER KILOG*9
      DIMENSION SURGE2(NET),WELL2(NET)
      EQUIVALENCE (SURGE2(1),P2(1)),(WELL2(1),ROUGH(1))
      DATA KILOG/'Kilograms'/
C=======================================================================
C     Transport Continuity
C=======================================================================
      WRITE(*,6040)
      DO 3995 J = 9,12
 3995 XNT(J)    = XNT(J)*28.3E-06
      TY        = TRIBA*3630.0/CMET(2,METRIC)
C=======================================================================
C     XNT(6)  = Remaining volume in conduits
C     XNT(8)  = Surcharge volume
C     XNT(30) = Wet well volume
C=======================================================================
      DO 4000 I = 1,NE
      M         = JR(I)
      IF(NTYPE(M).GE.19) THEN
                         XNT(8)   = XNT(8)  + SURGE2(M)
                         IF(NPOLL.GT.0) THEN
                         DO 4004 J = 1,NPOLL
 4004                    XNT(16+J) = XNT(16+J) +
     +                               SURGE2(M)*CPOL2(M,2,J)*28.3E-06
                         ENDIF
                         ENDIF
      IF(NTYPE(M).EQ.20) XNT(30)  = XNT(30) + WELL2(M)
      IF(NTYPE(M).EQ.22) THEN
                         IS     = KSTORE(M)
                         XNT(6) = XNT(6) + STORE(IS)
                         ENDIF
      IF(NPOLL.GT.0.AND.NTYPE(M).EQ.22) THEN
               DO 4006 J = 1,NPOLL
               IS        = KSTORE(M)
 4006          XNT(16+J) = XNT(16+J) + STORE(IS) *
     +                                 CPOL2(M,2,J)*28.3E-06
               ENDIF
      IF(NTYPE(M).GT.18) GO TO 4000
C#### WCH, 5/12/94.  INCLUDE NUMBER OF BARRELS IN VOLUME/MASS CALC.
      AAA       = 0.5*(A(M,1,2) + A(M,2,2))*DIST(M)*BARREL(M)
      XNT(6)    = XNT(6) + AAA
      IF(NPOLL.GT.0) THEN
               DO 4005 J = 1,NPOLL
 4005          XNT(16+J) = XNT(16+J) + AAA*0.5*(CPOL1(M,2,J)
     +                               + CPOL2(M,2,J))*28.3E-06
               ENDIF
 4000 CONTINUE
C=======================================================================
C     Write Transport flow continuity.
C=======================================================================
      SUM       = XNT(1)    + XNT(2) + XNT(3) + XNT(4) + XNT(5)
      SUM1      = XNT(6)    + XNT(7) + XNT(8) + XNT(30)
      DO 4010 J = 1,8
      ZUM(J)    = XNT(J)/TY * CMET(5,METRIC)
 4010 XNT(J)    = XNT(J)    / CMET(8,METRIC)
      ZUM(30)   = XNT(30)/TY* CMET(5,METRIC)
      XNT(30)   = XNT(30)   / CMET(8,METRIC)
      ERROR     = 0.0
      IF(SUM1.GT.0.0) ERROR  = 100.0 * (SUM1 - SUM)/SUM1
      ZUM(9)    = SUM/TY    * CMET(5,METRIC)
      ZUM(10)   = SUM1/TY   * CMET(5,METRIC)
      SUM       = SUM       / CMET(8,METRIC)
      SUM1      = SUM1      / CMET(8,METRIC)
      IF(METRIC.EQ.1) WRITE(N6,33)
     +             XNT(1),ZUM(1),XNT(2),ZUM(2),XNT(3),ZUM(3),XNT(4),
     +             ZUM(4),XNT(5),ZUM(5),XNT(6),ZUM(6),XNT(8),ZUM(8),
     +             XNT(30),ZUM(30),
     +             XNT(7),ZUM(7),SUM,ZUM(9),SUM1,ZUM(10),ERROR
      IF(METRIC.EQ.2) WRITE(N6,35)
     +             XNT(1),ZUM(1),XNT(2),ZUM(2),XNT(3),ZUM(3),XNT(4),
     +             ZUM(4),XNT(5),ZUM(5),XNT(6),ZUM(6),XNT(8),ZUM(8),
     +             XNT(30),ZUM(30),
     +             XNT(7),ZUM(7),SUM,ZUM(9),SUM1,ZUM(10),ERROR
C#### WCH (CDM), 8/93.  ADD ON-SCREEN CONTINUITY ERROR.
      WRITE(*,9031) ERROR
C=======================================================================
C     Write surcharge summary.
C=======================================================================
      IISURG    = 0
      DO 5000 I = 1,NE
      M         = JR(I)
      IF(NTYPE(M).GE.19)   GO TO 5000
      IF(SURLEN(M).EQ.0.0) GO TO 5000
      IISURG = IISURG + 1
      IF(IISURG.EQ.1) THEN
                      IF(METRIC.EQ.1) WRITE(N6,5010)
                      IF(METRIC.EQ.2) WRITE(N6,5015)
                      ENDIF
      SURLEN(M) = SURLEN(M)/60.0 - DT/60.0
      SPEAK(M)  = SPEAK(M)/CMET(8,METRIC)
C#### WCH, 3/28/94.  ADD METRIC CONVERSION TO QFULL.
      QF        = P1(M)*SQRT(SLOPE(M))/CMET(8,METRIC)
      IF(JCE.EQ.0) WRITE(N6,5020) NOE(M),ISTIME(M,1),ISTIME(M,2),
     +              JSTIME(M,1),JSTIME(M,2),SURLEN(M),QF,QR(M),SPEAK(M)
      IF(JCE.EQ.1) WRITE(N6,5021) KOE(M),ISTIME(M,1),ISTIME(M,2),
     +              JSTIME(M,1),JSTIME(M,2),SURLEN(M),QF,QR(M),SPEAK(M)
 5000 CONTINUE
      IF(IISURG.EQ.0) WRITE(N6,5030)
C=======================================================================
C     Write iteration and max error information for conduits.
C=======================================================================
      II        = 0
      DO 6000 I = 1,NE
      M         = JR(I)
      IF(NTYPE(M).GE.19) GO TO 6000
      II = II + 1
      IF(II.EQ.1.OR.MOD(II,40).EQ.0) THEN
                                     IF(METRIC.EQ.1) WRITE(N6,6010)
                                     IF(METRIC.EQ.2) WRITE(N6,6015)
                                     ENDIF
      QBIG(M)   = QBIG(M)/CMET(8,METRIC)
      STOTAL(M) = STOTAL(M)/CMET(8,METRIC)
      SMEAN(M)  = SMEAN(M)/FLOAT(NDT)
      IF(JCE.EQ.0) WRITE(N6,6020) NOE(M),KITER(M),KITMAX(M),EMAX(M),
     +             QBIG(M),SLOPE(M),SBIG(M),SMAL(M),SMEAN(M),STOTAL(M)
      IF(JCE.EQ.1) WRITE(N6,6021) KOE(M),KITER(M),KITMAX(M),EMAX(M),
     +             QBIG(M),SLOPE(M),SBIG(M),SMAL(M),SMEAN(M),STOTAL(M)
 6000 CONTINUE
C=======================================================================
C     Write total information for non-conduits.
C=======================================================================
      II        = 0
      DO 6100 I = 1,NE
      M         = JR(I)
      IF(NTYPE(M).LT.19) GO TO 6100
      II        = II + 1
      STOTAL(M) = STOTAL(M)/CMET(8,METRIC)
      IF(II.EQ.1.OR.MOD(II,40).EQ.0) THEN
                                     IF(METRIC.EQ.1) WRITE(N6,6030)
                                     IF(METRIC.EQ.2) WRITE(N6,6031)
                                     ENDIF
      IF(JCE.EQ.0) WRITE(N6,6035) NOE(M),STOTAL(M)
      IF(JCE.EQ.1) WRITE(N6,6036) KOE(M),STOTAL(M)
 6100 CONTINUE
C=======================================================================
C     Print final solids deposition.
C=======================================================================
      IF(KSPG.GT.0) THEN
                    WRITE (N6,940)
                    WINDEX    = 0.0
                    IDIDIT    = 0
                    DO 4730 I = 1,NE
                    M         = JR(I)
                    IF(NTYPE(M).GT.18) GO TO 4730
C=======================================================================
C                   Convert to KG for output.
C=======================================================================
                    INDX      = 0
                    SUM       = 0.0
                    DO 4725 K = 1,NPOLL
                    IF(SPG(K).LE.1.0) GO TO 4725
                    INDX       = INDX + 1
                    KMESS(INDX)= K
                    CPPP(INDX) = SCOUR(M,K)*28.3E-6
                    SUM        = SUM + CPPP(INDX)
 4725               CONTINUE
                    IF(IDIDIT.EQ.0) THEN
                              WRITE(N6,942) (PNAME(KMESS(K)),K=1,KSPG)
                              WRITE(N6,943) (KILOG,K=1,KSPG)
                              KILOG = '---------'
                              WRITE(N6,946) (KILOG,K=1,KSPG)
                              IDIDIT = 1
                              ENDIF
                    IF(SUM.GT.0.0) THEN
                           IF(JCE.EQ.0) WRITE(N6,944) NOE(M),
     +                                     (CPPP(J),J=1,KSPG)
                           IF(JCE.EQ.1) WRITE(N6,954) KOE(M),
     +                                     (CPPP(J),J=1,KSPG)
                           WINDEX = 1.0
                           ENDIF
 4730               CONTINUE
                    IF(WINDEX.EQ.0.0) WRITE(N6,945)
                    ENDIF
C=======================================================================
C     Print results of pollutant or flow monitoring routine.
C     Convert 'RANKS' to CONC*L*E-6 or CONC*L/SEC*E-6.
C     If concentration is in mg/l, ranks are in kilograms.
C=======================================================================
      NV        = 2*NPOLL
      TIM       = DT*FLOAT(NDT)*28.3E-6
      DO 4746 I = 1,NE
      RANQ(I)   = DT*RANQ(I)/CMET(8,METRIC)
      IF(NPOLL.GT.0) THEN
                     DO 4745 K = 1,NV
                     RANK(I,K) = DT*RANK(I,K)*28.3E-6
 4745                CONTINUE
                     ENDIF
 4746 CONTINUE
C=======================================================================
C     Quality printout.
C=======================================================================
      IF(NPOLL.GT.0) THEN
      DO 3000 K = 1,NPOLL
      IF(METRIC.EQ.1) WRITE(N6,990)  PNAME(K)
      IF(METRIC.EQ.2) WRITE(N6,5990) PNAME(K)
      T1 = 0.0
      T2 = 0.0
      T3 = 0.0
      T4 = 0.0
      T5 = 0.0
      T6 = 0.0
      T7 = 0.0
      DO 2500 I      = 1,NE
      M              = JR(I)
      IF(NTYPE(M).LT.19) GO TO 2500
C=======================================================================
C     WWW = dry weather flow.
C=======================================================================
                 WWW = QINFIL(M)*CPINF(K)*TIM
      IF(K.LE.3) WWW = TIM*WDWF(M,K) + WWW
      BEGIN     = 0.0
      END       = 0.0
      IF(SPG(K).GT.1.0) THEN
                        DO 2750 J = 1,3
                        L         = INUE(M,J)
                        IF(L.GT.NE)        GO TO 2750
                        IF(NTYPE(L).GE.19) GO TO 2750
                        BEGIN = BEGIN + RANK(L,K+4)*28.3E-06
                        END   = END   + SCOUR(L,K)*28.3E-06
 2750                   CONTINUE
                        ENDIF
      DIFF = END - BEGIN
      SUM  = RANK(M,K) + WWW + DIFF
      IF(JCE.EQ.0) WRITE(N6,992) NOE(M),RANK(M,K),
     +                           WWW,BEGIN,END,DIFF,SUM,RANQ(M)
      IF(JCE.EQ.1) WRITE(N6,996) KOE(M),RANK(M,K),
     +                           WWW,BEGIN,END,DIFF,SUM,RANQ(M)
      T1 = T1 + RANK(M,K)
      T2 = T2 + WWW
      T3 = T3 + BEGIN
      T4 = T4 + END
      T5 = T5 + DIFF
      T6 = T6 + SUM
      T7 = T7 + RANQ(M)
 2500 CONTINUE
      WRITE(N6,993) T1,T2,T3,T4,T5,T6,T7
      XNT(20+K) = XNT(20+K)*28.3E-06*DT
      XNT(25+K) = XNT(25+K)*28.3E-06
      XNT(30+K) = DT*XNT(K+30)*28.3E-6
      SUM       = T1 + T3       + XNT(20+K) + XNT(12+K) + XNT(30+K)
      SUM1      = T4 + XNT(8+K) + XNT(16+K) + XNT(25+K)
      ERROR     = 0.0
      IF(SUM1.GT.0.0) ERROR = 100.0*(SUM1 - SUM)/SUM1
      WRITE(N6,34) PNAME(K),T1,XNT(20+K),XNT(30+K),T3,T4,XNT(12+K),
     +             XNT(16+K),XNT(25+K),XNT(8+K),SUM,SUM1,ERROR
 3000 CONTINUE
      ENDIF
C=======================================================================
C     Write simulation length.
C=======================================================================
      DURHR  = TIME/3600.0
      DURMIN = TIME/60.0
      WRITE(N6,910) TIME,DURMIN,DURHR,DT
      WRITE(N6,925) NYEAR,MONTH,NDAY,TIMDAY
C=======================================================================
  33  FORMAT(//,
     +10X,' ####################################',/,
     +10X,' # Transport Block Flow Continuity  #',/,
     +10X,' ####################################',//,
     + ' Sum of WET WEATHER Inflow........',1PE12.3,' cubic feet',
     + 0PF10.4,' inches',/,
     + ' Sum of DRY WEATHER Inflow........',1PE12.3,' cubic feet',
     + 0PF10.4,' inches',/,
     + ' Sum of INFILTRATION Inflow.......',1PE12.3,' cubic feet',
     + 0PF10.4,' inches',/,
     + ' Sum of MANHOLE CONSTANT Inflow...',1PE12.3,' cubic feet',
     + 0PF10.4,' inches',/,
     + ' INITIAL VOLUME IN CONDUITS.......',1PE12.3,' cubic feet',
     + 0PF10.4,' inches',/,
     + ' REMAINING VOLUME IN CONDUITS.....',1PE12.3,' cubic feet',
     + 0PF10.4,' inches',/,
     + ' Sum of FINAL SURCHARGE STORAGE...',1PE12.3,' cubic feet',
     + 0PF10.4,' inches',/,
     + ' Sum of FINAL WET WELL VOLUME.....',1PE12.3,' cubic feet',
     + 0PF10.4,' inches',/,
     + ' Sum of TRANSPORT Outflows........',1PE12.3,' cubic feet',
     + 0PF10.4,' inches',/,
     + ' Sum of INITIAL VOLUME + Inflows..',1PE12.3,' cubic feet',
     + 0PF10.4,' inches',/,
     + ' Sum of REMAINING VOLUME + Outflow',1PE12.3,' cubic feet',
     + 0PF10.4,' inches',/,
     + ' Percent flow continuity error....',0PF12.3,//)
  34  FORMAT(//,
     +10X,' ####################################',/,
     +10X,' #  ',A8,'    Quality Continuity  #',/,
     +10X,' ####################################',//,
     +' Sum of inflow load..................',1PE12.3,' kilograms',/,
     +' Sum of D.W.F. inflow load...........',1PE12.3,' kilograms',/,
     +' Sum of constant manhole load........',1PE12.3,' kilograms',/,
     +' Sum of initial sewer deposition.....',1PE12.3,' kilograms',/,
     +' Sum of final sewer deposition.......',1PE12.3,' kilograms',/,
     +' Sum of initial conduit load.........',1PE12.3,' kilograms',/,
     +' Sum of remaining conduit load.......',1PE12.3,' kilograms',/,
     +' Sum of decay in system..............',1PE12.3,' kilograms',/,
     +' Sum of outflow load.................',1PE12.3,' kilograms',/,
     +' Sum of inflow + initial deposition..',1PE12.3,' kilograms',/,
     +' Sum of outflow + final deposition...',1PE12.3,' kilograms',/,
     +' Percent quality continuity error....',0PF12.3,//)
  35  FORMAT(//,
     +10X,' ####################################',/,
     +10X,' # Transport Block Flow Continuity  #',/,
     +10X,' ####################################',//,
     + ' Sum OF WET WEATHER Inflow........',1PE12.3,' cubic meters',
     + 0PF10.4,' millimeters',/,
     + ' Sum OF DRY WEATHER Inflow........',1PE12.3,' cubic meters',
     + 0PF10.4,' millimeters',/,
     + ' Sum OF INFILTRATION Inflow.......',1PE12.3,' cubic meters',
     + 0PF10.4,' millimeters',/,
     + ' Sum OF MANHOLE CONSTANT Inflow...',1PE12.3,' cubic meters',
     + 0PF10.4,' millimeters',/,
     + ' INITIAL VOLUME IN CONDUITS.......',1PE12.3,' cubic meters',
     + 0PF10.4,' millimeters',/,
     + ' REMAINING VOLUME IN CONDUITS.....',1PE12.3,' cubic meters',
     + 0PF10.4,' millimeters',/,
     + ' Sum OF FINAL SURCHARGE STORAGE...',1PE12.3,' cubic meters',
     + 0PF10.4,' millimeters',/,
     + ' Sum OF FINAL WET WELL VOLUME.....',1PE12.3,' cubic meters',
     + 0PF10.4,' millimeters',/,
     + ' Sum OF TRANSPORT Outflows........',1PE12.3,' cubic meters',
     + 0PF10.4,' millimeters',/,
     + ' Sum OF INITIAL VOLUME + Inflows..',1PE12.3,' cubic meters',
     + 0PF10.4,' millimeters',/,
     + ' Sum OF REMAINING VOLUME + Outflow',1PE12.3,' cubic meters',
     + 0PF10.4,' millimeters',/,
     + ' Percent Flow Continuity error....',0PF12.3,//)
  400 FORMAT(' INCREASE WIDTH OF EXT ELEMENT ',I8,' BY 0.50 TO ',F7.3,
     1' FT.')
  401 FORMAT(' INCREASE WIDTH OF EXT ELEMENT ',A10,' BY 0.50 TO ',F7.3,
     1' FT.')
  415 FORMAT(' REPLACE EXT ELEMENT ',I8,' BY CIRCULAR CONDUIT OF DIAMETE
     1R ',F7.3,'FT.')
  416 FORMAT(' REPLACE EXT ELEMENT ',A10,' BY CIRCULAR CONDUIT OF DIAMET
     1ER ',F7.3,'FT.')
  480 FORMAT(' INCREASE DIAMETER OF EXT ELEMENT ',I8,' by ',F6.2,
     1' to ',F8.3,'ft.')
  481 FORMAT(' INCREASE DIAMETER OF EXT ELEMENT ',A10,' by ',F6.2,
     1' to ',F8.3,'ft.')
  910 FORMAT (//, ' The Total Simulation Time =',F12.1,' seconds.'/,
     1            '                            ',F12.2,' minutes.',/,
     2            '                            ',F12.3,'   hours.',/,
     3            '         The Time Step(DT) =',F12.1,' seconds.')
  925 FORMAT (//,' The ending date (YR/MO/DAY)....',I3,'/',I2,'/',I2,/,
     1           ' The ending time of day.........',F9.3,' seconds.',//)
  931 FORMAT(1X,2I5,1X,A1,A16,F8.5,F9.2,F9.4,F9.3,2F7.3,F6.1,3F9.3,
     +                                                       5X,A4)
  932 FORMAT(1X,A5,I5,1X,A1,A16,F8.5,F9.2,F9.4,F9.3,2F7.3,F6.1,
     +                                              3F9.3,5X,A4)
  940 FORMAT(/,1H1,/,
     1' ################################################',/,
     2' #   Bed of Solids in Conduits at end of storm  #',/,
     3' ################################################')
  942 FORMAT(//,' ELEMENT',4(3X,A8))
  943 FORMAT(   '   NO.  ',4(1X,A9,1X))
  944 FORMAT(I6,2X,4G11.5)
  945 FORMAT(' No bed of solids existed at the end of the storm.')
  946 FORMAT ('        ',4(1X,A9,1X))
  954 FORMAT(1X,A6,1X,4G11.5)
  992 FORMAT(1X,I6,7(1X,1PE9.2))
  993 FORMAT(1X,'Total ',7(1X,1PE9.2))
  996 FORMAT(1X,A6,7(1X,1PE9.2))
 4750 FORMAT(//,11X,A80,/,11X,A80)
 990  FORMAT(1H1,/,
     110X,' *******************************************************',/,
     210X,' * Results of quality monitoring routine for quality   *',/,
     310X,' * parameters associated with manholes (Inlet Points). *',/,
     310X,' * Units are: Total Load (kilograms), Flow (cub feet). *',/,
     410X,' * Total Load = Runoff + D.W.F. + Scour (Deposit) Load *',/,
     410X,' *******************************************************',/,
     410X,'                For  Quality Constituent : ',A8,//,
     41X,' Manhole  Runoff    D.W.F.   Initial     Final      Load     T
     6otal     Total',/,1X,' Inlet      Load      Load   Deposit   Depos
     7it   Scoured      Load    Inflow',/,1X,' -----    ------    ------
     8   -------   -------   -------     -----     -----')
 5990 FORMAT(1H1,/,
     110X,' *******************************************************',/,
     210X,' * Results of quality monitoring routine for quality   *',/,
     310X,' * parameters associated with manholes (Inlet Points). *',/,
     310X,' * Units are: Total Load (kilograms), Flow (cub met.). *',/,
     410X,' * Total Load = Runoff + D.W.F. + Scour (Deposit) Load *',/,
     410X,' *******************************************************',/,
     410X,'                For  Quality Constituent : ',A8,//,
     41X,' Manhole  Runoff    D.W.F.   Initial     Final      Load     T
     6otal     Total',/,1X,' Inlet      Load      Load   Deposit   Depos
     7it   Scoured      Load    Inflow',/,1X,' -----    ------    ------
     8   -------   -------   -------     -----     -----')
 5010 FORMAT(/,1H1,/,
     1' ********************************************************',/,
     2' *   C O N D U I T   S U R C H A R G E   S U M M A R Y  *',/,
     3' ********************************************************',//,
     3'            STARTING  STARTING    ENDING    ENDING SURCHARGE    F
     +ULL   PEAK FLW  SURCHARGE',/,
     +'   ELEMENT    JULIAN   TIME OF    JULIAN   TIME OF    LENGTH    F
     +LOW      RATIO     PEAK  ',/,
     +'    NUMBER      DATE  DAY(SEC)      DATE  DAY(SEC)   MINUTES    (
     +CFS)  Qp/QFULL   VOL.(CF)',/,
     +'  -------- --------- --------- --------- --------- ---------  ---
     +----- --------  ---------')
 5015 FORMAT(/,1H1,/,
     1' ********************************************************',/,
     2' *   C O N D U I T   S U R C H A R G E   S U M M A R Y  *',/,
     3' ********************************************************',//,
     3'            STARTING  STARTING    ENDING    ENDING SURCHARGE    F
     +ULL   PEAK FLW  SURCHARGE',/,
     +'   ELEMENT    JULIAN   TIME OF    JULIAN   TIME OF    LENGTH    F
     +LOW      RATIO     PEAK  ',/,
     +'    NUMBER      DATE  DAY(SEC)      DATE  DAY(SEC)   MINUTES    (
     +CMS)  Qp/QFULL   VOL.(CM)',/,
     +'  -------- --------- --------- --------- --------- ---------  ---
     +----- --------  ---------')
 5020 FORMAT(5I10,2F10.2,F9.2,1PE11.2)
 5021 FORMAT(1X,A9,4I10,2F10.2,F9.2,1PE11.2)
 5030 FORMAT(/,
     1' *********************************************************',/,
     2' * Surcharge did not occur in this Transport simulation. *',/,
     3' *********************************************************',/)
 6010 FORMAT(//,
     +' ************************************************',/,
     +' * TOTAL ITERATIONS AND MAXIMUM # OF ITERATIONS *',/,
     +' ************************************************',//,
     +'   ELEMENT   TOTAL NUMBER  MAXIMUM NUMBER     MAXIMUM    MAXIMUM
     +  CONDUIT   MAXIMUM   MINIMUM      MEAN     TOTAL',/,
     +'    NUMBER  OF ITERATIONS   OF ITERATIONS       ERROR  FLOW(CFS)
     +    SLOPE     SLOPE     SLOPE     SLOPE   FLOW-CF',/,
     +'   -------  -------------  --------------  ----------  ---------
     +   ------   -------   -------   -------   -------')
 6015 FORMAT(//,
     +' ************************************************',/,
     +' * TOTAL ITERATIONS AND MAXIMUM # OF ITERATIONS *',/,
     +' ************************************************',//,
     +'   ELEMENT   TOTAL NUMBER  MAXIMUM NUMBER     MAXIMUM    MAXIMUM
     +  CONDUIT   MAXIMUM   MINIMUM      MEAN     TOTAL',/,
     +'    NUMBER  OF ITERATIONS   OF ITERATIONS       ERROR  FLOW(CMS)
     +    SLOPE     SLOPE     SLOPE     SLOPE   FLOW-CM',/,
     +'   -------  -------------  --------------  ----------  ---------
     +   ------   -------   -------   -------   -------')
 6020 FORMAT(I10,I15,I16,F12.4,F11.3,5(1X,1PE9.2))
 6021 FORMAT(1X,A9,I15,I16,F12.4,F11.5,6(1X,1PE9.2))
 6030 FORMAT(//,
     +' *******************************************',/,
     +' * Total flow through non-conduit elements *',/,
     +' *******************************************',//,
     +'    ELEMENT       TOTAL',/,
     +'     NUMBER     FLOW-CF',/,
     +'    -------   ---------')
 6031 FORMAT(//,
     +' *******************************************',/,
     +' * Total flow through non-conduit elements *',/,
     +' *******************************************',//,
     +'    ELEMENT       TOTAL',/,
     +'     NUMBER     FLOW-CM',/,
     +'    -------   ---------')
 6035 FORMAT(1X,I10,1PE12.2)
 6036 FORMAT(1X,A10,1PE12.2)
 6040 FORMAT(/,' Calling output subroutines.')
C#### WCH (CDM), 8/93.
 9031 FORMAT(/,' Percent flow continuity error =',F8.3)
C=======================================================================
      RETURN
      END

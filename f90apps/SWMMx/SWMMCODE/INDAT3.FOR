      SUBROUTINE INDAT3
C=======================================================================
C     THIS SUBROUTINE READS DATA GROUPS J1-J4, EXCEPT FOR THE INPUT
C          HYDROGRAPH LINES READ BY SUBROUTINE INFLOW.  IT ALSO PERFORMS
C          SOME INITIALIZATION.  ALL NODE-CONDUIT LINKAGES ARE
C          SET UP AND CONVERTED TO THE INTERNAL NUMBER SYSTEM.
C     CORRECTION TO J4 READ BY WCH, 4/1/93
C     CORRECTION FOR INITIAL DATE BY RED, 5/12/93
C     CONVERT OPTIONAL USER-INPUT INITIAL DATE TO JULIAN, WCH, 4/11/94.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'ORF.INC'
      INCLUDE 'WEIR.INC'
      INCLUDE 'BND.INC'
      INCLUDE 'OUT.INC'
      INCLUDE 'TIDE.INC'
      INCLUDE 'HYFLOW.INC'
      DIMENSION LOCNOS(NIE)
      CHARACTER KSW(NEH)*10,KOCNOS(NIE)*10
C======================================================================
C     Read tidal boundary data on data gouups J1-J4.
C======================================================================
      M2S2      = 0
      DO 1800 I = 1,NTE
      READ(N5,*,ERR=888) CC
      BACKSPACE N5
      IF(CC.NE.'J1') GO TO 1801
      IF(I.EQ.1) WRITE(N6,5710)
      READ(N5,*,ERR=888) CC,NTIDE(I)
      M2S2 = M2S2 + 1
C======================================================================
C     NTIDE = 1  NO CONTROL WATERSURFACE AT THE OUTFALLS
C             2  OUTFALL CONTROL WATER SURFACE AT CONSTANT ELEVATION=A1
C             3  TIDE COEFFICIENTS READ BY PROGRAM FROM INPUT FILE
C             4  COMPUTE TIDE COEFFICIENTS
C======================================================================
      IF(NTIDE(I).EQ.1) WRITE(N6,5715) I
      IF(NTIDE(I).EQ.2) THEN
             READ(N5,*,ERR=888) CC,A1(I)
             IF(METRIC.EQ.1) WRITE(N6,5790) I,A1(I)
             IF(METRIC.EQ.2) WRITE(N6,5791) I,A1(I)
             PHLAGS(I) = 0.0
             ENDIF
      IF(NTIDE(I).EQ.3) THEN
         READ(N5,*,ERR=888) CC,A1(I),W(I),A2(I),
     1                      A3(I),A4(I),A5(I),A6(I),A7(I)
         WRITE(N6,5780) I,A1(I),A2(I),A3(I),A4(I),A5(I),A6(I),A7(I),W(I)
         W(I)      = 2.0*3.14159/W(I)
         PHLAGS(I) = 0.0
         ENDIF
C======================================================================
C     READ DATA GROUPS J2, J3 AND J4
C======================================================================
      IF(NTIDE(I).EQ.4) THEN
         READ(N5,*,ERR=888) CC,A1(I),W(I)
         READ(N5,*,ERR=888) CC,KO,NI,NCHTID,DELTA
         NUMTID(I)  = NI
         IF(DELTA.LE.0.0) THEN
                          IF(METRIC.EQ.1) DELTA = 0.005
                          IF(METRIC.EQ.2) DELTA = 0.001524
                          ENDIF
C#### WCH, 4/1/93  INSERT CHECK ON NUMBER OF DATA POINTS.
         IF(NI.GT.NTVAL) THEN
           WRITE (N6,5707) I,NTIDE(I),NI,NTVAL
           NSTOP = NSTOP + 1
           ENDIF
         DO 1765  K = 1,NI,5
         J1         = K + 4
         IF(J1.GT.NI) J1 = NI
 1765    READ(N5,*,ERR=888) CC,(TT(J),YY(J),J=K,J1)
         CALL TIDCF(KO,NI,NCHTID,I,DELTA)
         ENDIF
C=======================================================================
C        READ DOWNSTREAM BOUNDARY STAGE INFORMATION
C        READ DATA GROUPS J3 AND J4
C=======================================================================
      IF(NTIDE(I).EQ.5) THEN
         READ(N5,*,ERR=888) CC,KO,NI,NCHTID
C#######################################################################
C  WCH, 4/1/93.  FIX J4 READ WITH NTIDE=5 TO READ 5 VALUES PER LINE,
C                WITH LINE IDENTIFIER.
C#######################################################################
         IF(NI.GT.NTVAL) THEN
           WRITE (N6,5707) I,NTIDE(I),NI,NTVAL
           NSTOP = NSTOP + 1
           ENDIF
         DO 1770  K = 1,NI,5
         J1 = K + 4
         IF(J1.GT.NI) J1 = NI
 1770    READ(N5,*,ERR=888) CC,(STIDE(I,1,J),STIDE(I,2,J),J=K,J1)
C
         IF(NCHTID.EQ.1) THEN
                   IF(METRIC.EQ.1) WRITE(N6,1780) I
                   IF(METRIC.EQ.2) WRITE(N6,1785) I
                   WRITE(N6,1790) (J,STIDE(I,1,J),STIDE(I,2,J),J=1,NI)
                   ENDIF
         NUMTID(I) = NI
         PHLAGS(I) = 0.0
         ENDIF
 1800 CONTINUE
 1801 CONTINUE
C=======================================================================
C     CHECK JTIDES ARRAY FOR BC'S NOT INPUT ON J1-J4 DATA GROUPS
C=======================================================================
      DO 1850 J = 1,NJ
      IF(JTIDES(J).LE.M2S2) GO TO 1850
      IF(JCE.EQ.0) WRITE(N6,1851)  JUN(J),JTIDES(J)
      IF(JCE.EQ.1) WRITE(N6,1852) AJUN(J),JTIDES(J)
      NSTOP     = NSTOP + 1
 1850 CONTINUE
C=======================================================================
C     SET PRINT : PLOT ARRAYS IN INTERNAL NUMBER SYSTEM
C=======================================================================
      IF(NQPRT.GT.0) THEN
               DO 1550 K = 1,NQPRT
               DO 1540 N = 1,NTL
               IF(JCE.EQ.0.AND.NCOND(N).EQ.CPRT(K))   GO TO 1545
               IF(JCE.EQ.1.AND.ACOND(N).EQ.AOUT(K,2)) GO TO 1545
 1540          CONTINUE
               IF(JCE.EQ.0) WRITE(N6,5678) CPRT(K)
               IF(JCE.EQ.1) WRITE(N6,5679) AOUT(K,2)
               NSTOP   = NSTOP+1
 1545          CPRT(K) = N
 1550          CONTINUE
               ENDIF
C=======================================================================
      IF(LPLT.GT.0) THEN
                    DO 1620 K = 1,LPLT
                    DO 1580 N = 1,NTL
                    IF(JCE.EQ.0.AND.NCOND(N).EQ.KPLT(K))   GO TO 1600
                    IF(JCE.EQ.1.AND.ACOND(N).EQ.AOUT(K,4)) GO TO 1600
 1580               CONTINUE
                    IF(JCE.EQ.0) WRITE(N6,5680) KPLT(K)
                    IF(JCE.EQ.1) WRITE(N6,5681) AOUT(K,4)
                    NSTOP = NSTOP+1
                    GO TO 1620
 1600               KPLT(K) = N
 1620               CONTINUE
                    ENDIF
C=======================================================================
      IF(NHPRT.GT.0) THEN
                     DO 1660 I = 1,NHPRT
                     DO 1650 J = 1,NJ
                     IF(JCE.EQ.0.AND.JUN(J).EQ.JPRT(I))    GO TO 1655
                     IF(JCE.EQ.1.AND.AJUN(J).EQ.AOUT(I,1)) GO TO 1655
 1650                CONTINUE
                     IF(JCE.EQ.0) WRITE(N6,5690) JPRT(I)
                     IF(JCE.EQ.1) WRITE(N6,5691) AOUT(I,1)
                     NSTOP   = NSTOP+1
 1655                JPRT(I) = J
 1660                CONTINUE
                     ENDIF
C=======================================================================
      IF(NPLT.GT.0) THEN
                    DO 1720 N = 1,NPLT
                    DO 1680 J = 1,NJ
                    IF(JCE.EQ.0.AND.JUN(J).EQ.JPLT(N)) GO TO 1700
                    IF(JCE.EQ.1.AND.AJUN(J).EQ.AOUT(N,3)) GO TO 1700
 1680               CONTINUE
                    IF(JCE.EQ.0) WRITE(N6,5700) JPLT(N)
                    IF(JCE.EQ.1) WRITE(N6,5701) AOUT(N,3)
                    NSTOP = NSTOP+1
                    GO TO 1720
 1700               JPLT(N) = J
 1720               CONTINUE
                    ENDIF
C=======================================================================
      IF(NSURF.GT.0) THEN
                     DO 1730 K = 1,NSURF
                     DO 1740 N = 1,NTC
                     IF(JCE.EQ.0.AND.NCOND(N).EQ.JSURF(K))  GO TO 1735
                     IF(JCE.EQ.1.AND.ACOND(N).EQ.AOUT(K,5)) GO TO 1735
 1740                CONTINUE
                     IF(JCE.EQ.0) WRITE(N6,5705) JSURF(K)
                     IF(JCE.EQ.1) WRITE(N6,5706) AOUT(K,5)
                     NSTOP = NSTOP+1
                     GO TO 1730
 1735                JSURF(K) = N
 1730                CONTINUE
                     ENDIF
C======================================================================
C     Conduit roughness initialization.
C======================================================================
      DO 1820 N = 1,NTC
 1820 ROUGH(N)  = GRVT*ROUGH(N)**2/CMET(9,METRIC)**2
C======================================================================
C     Hydrograph input initialization.
C======================================================================
      DO 1840  L = 1,NIE
      DO 1840 K  = 1,2
 1840 QTAPE(L,K) = 0.0
      DO 1841  L = 1,NEH
      JSW(L)     = 0
      DO 1841  K = 1,2
 1841 QCARD(L,K) = 0.0
C======================================================================
C     Read input hydrograph information (disk file).
C======================================================================
      LOCATS = 0
      TIMDAY = TZERO
C#### WCH, 4/11/94.  CODE TO CONVERT INITIAL YR/MO/DAY TO JULIAN.
      MYEAR  = IDATZ/10000
      MDAY   = IDATZ - MYEAR*10000
      MMNTH  = MDAY/100
      MDAY   = MDAY - MMNTH*100
      JULDAY = 1000*MYEAR + JDATE(MDAY,MMNTH,MYEAR)
      IDATEZ = JULDAY
C####     JULDAY = 88001
C
      IF(LAST.GT.0) THEN
                    CALL INFACE(1,LAST)
                    TIMDAY = TZERO
                    JULDAY = IDATEZ
C======================================================================
C                   CONVERT TO INTERNAL NUMBERS
C======================================================================
                    DO 1920 L = 1,LOCATS
                    DO 1880 J = 1,NJ
                    IF(JCE.EQ.0.AND.NLOC(L).EQ.JUN(J)) GO TO 1900
                    IF(JCE.EQ.1.AND.KAN(L).EQ.AJUN(J)) GO TO 1900
 1880               CONTINUE
                    IF(JCE.EQ.0) WRITE(N6,5820) NLOC(L)
                    IF(JCE.EQ.1) WRITE(N6,5821) KAN(L)
                    NSTOP   = NSTOP + 1
                    GO TO 1920
 1900               NLOC(L) = J
 1920               CONTINUE
                    ENDIF
C======================================================================
C     Read input hydrograph data (lines) on data groups K1-K3.
C======================================================================
      IF(NJSW.GT.0) THEN
                    READ(N5,*,ERR=888) CC,NINC
                    DO 1970  L = 1,NJSW,NINC
                                   L1 = L + NINC - 1
                    IF(L1.GT.NJSW) L1 = NJSW
                    IF(JCE.EQ.0) READ(N5,*,ERR=888) CC,
     +                                    (JSW(LL),LL=L,L1)
 1970               IF(JCE.EQ.1) READ(N5,*,ERR=888) CC,
     +                                    (KSW(LL),LL=L,L1)
                    WRITE(N6,2999)
                    WRITE(N6,5060) ALPHA1,ALPHA2
C======================================================================
C                   Convert to internal numbers
C======================================================================
                    DO 2020 L = 1,NJSW
                    DO 1980 J = 1,NJ
                    IF(JCE.EQ.0.AND.JSW(L).EQ.JUN(J))  GO TO 2000
                    IF(JCE.EQ.1.AND.KSW(L).EQ.AJUN(J)) GO TO 2000
 1980               CONTINUE
                    IF(JCE.EQ.0) WRITE(N6,5820) JSW(L)
                    IF(JCE.EQ.1) WRITE(N6,5821) KSW(L)
                    NSTOP  = NSTOP + 1
                    GO TO 2020
 2000               JSW(L) = J
 2020               CONTINUE
                    ENDIF
C======================================================================
C     Determine outflow nodes.
C======================================================================
      IF(NEXT.GT.0) THEN
                    N1        = NTC + 1
                    I         = 0
                    DO 2045 N = N1,NTL
                    IF(NJUNC(N,2).NE.0) GO TO 2045
                    I         = I + 1
                    IF(JCE.EQ.0) LOCNOS(I) = JUN(NJUNC(N,1))
                    IF(JCE.EQ.1) KOCNOS(I) = AJUN(NJUNC(N,1))
 2045               CONTINUE
                    MJSW    = I
                    IF(MJSW.GT.NIE) WRITE(N6,5850)
                    IF(MJSW.EQ.0) NSTOP = NSTOP + 1
                    ENDIF
C======================================================================
C     Write file headers for output hydrograph.
C======================================================================
      IF(NEXT.GT.0) THEN
C#### RED, 5/12/93. GIVE EXTRAN A NON-ZERO INITIAL DATE.
C#### WCH, 4/11/94. 88001 IS DEFAULT ONLY IF NO USER-SUPPLIED VALUE.
C#### WCH                    IF(LAST.EQ.0) IDATEZ = 88001
                    REWIND NEXT
                    NPOLL = 0
                    WRITE(NEXT) MJSW,NPOLL
                    IF(JCE.EQ.0) WRITE(NEXT) (LOCNOS(I),I=1,MJSW)
                    IF(JCE.EQ.1) WRITE(NEXT) (KOCNOS(I),I=1,MJSW)
                    SOURCE = 'EXTRAN BLOCK'
                    QQCONV = QCONV
                    QCONV  = CMET(8,METRIC)
                    TITLE(3) = ALPHA1
                    TITLE(4) = ALPHA2
                    CALL INFACE(2,NEXT)
                    QCONV    = QQCONV
                    ENDIF
      IF(NSTOP.GT.0) THEN
                     WRITE(N6,5920) NSTOP
                     STOP
                     ENDIF
C======================================================================
 1780 FORMAT(/,
     +' ******************************************',/,
     +' *  DOWNSTREAM BOUNDARY STAGE INFORMATION *',/,
     +' *  FOR BOUNDARY CONDITION # ',I5,'.       *',/,
     +' ******************************************',//,
     +'  NO.  TIME(HR) STAGE(FT)  NO.  TIME(HR) STAGE(FT)  NO.  TIME(HR)
     + STAGE(FT)',/,
     +'  ---  -------- --------   ---  -------- ---------  ---  --------
     + ---------')
 1785 FORMAT(/,
     +' ******************************************',/,
     +' *  DOWNSTREAM BOUNDARY STAGE INFORMATION *',/,
     +' *  FOR BOUNDARY CONDITION # ',I5,'.       *',/,
     +' ******************************************',//,
     +'  NO.  TIME(HR) STAGE (M)  NO.  TIME(HR) STAGE (M)  NO.  TIME(HR)
     + STAGE(FT)',/,
     +'  ---  -------- --------   ---  -------- ---------  ---  --------
     + ---------')
 1790 FORMAT(3(I5,F10.2,F10.3))
 1851 FORMAT(/,' ===> ERROR !!  JUNCTION ',I10,' HAD A BOUNDARY ',/,
     +         ' CONDITION # (',I5,') NOT INPUT ON DATA GROUPS J1-J4')
 1852 FORMAT(/,' ===> ERROR !!  JUNCTION ',A10,' HAD A BOUNDARY ',/,
     +         ' CONDITION # (',I5,') NOT INPUT ON DATA GROUPS J1-J4')
 2999 FORMAT(/,
     1       '1',40(2H--)/' ','ENVIRONMENTAL PROTECTION AGENCY',13X,40H*
     2***   EXTENDED TRANSPORT PROGRAM   ****,8X,'WATER RESOURCES DIVISI
     3ON',/,' ','WASHINGTON, D.C.            ',16X,4H****,32X,4H****,8X,
     4'CAMP DRESSER & MCKEE INC.',/,' ','                ',28X,4H****,
     56X,'   ANALYSIS MODULE  ',6X,4H****,8X,'ANNANDALE, VIRGINIA')
 5060 FORMAT(/,5X,A80,/,5X,A80,/)
 5678 FORMAT(/,' ====> ERROR !!! CONDUIT',I9,' REQUESTED FOR PRINTOUT',
     1       ' IS NOT CONTAINED IN CONDUIT DATA')
 5679 FORMAT(/,' ====> ERROR !!! CONDUIT ',A10,
     1     ' REQUESTED FOR PRINTOUT IS NOT CONTAINED IN CONDUIT DATA')
 5680 FORMAT(/,' ====> ERROR !!! CONDUIT ',I9,' REQUESTED FOR PLOTTING',
     1       ' IS NOT CONTAINED IN CONDUIT DATA')
 5681 FORMAT(/,' ====> ERROR !!! CONDUIT ',A10,
     1     ' REQUESTED FOR PLOTTING IS NOT CONTAINED IN CONDUIT DATA')
 5690 FORMAT(/,' ====> ERROR !!! JUNCTION ',I9,
     1     ' REQUESTED FOR PRINTOUT IS NOT CONTAINED IN JUNCTION DATA')
 5691 FORMAT(/,' ====> ERROR !!! JUNCTION ',A10,
     1 ' REQUESTED FOR PRINTOUT IS NOT CONTAINED IN JUNCTION DATA')
 5700 FORMAT(/,' ====> ERROR !!! JUNCTION ',I9,
     1  ' REQUESTED FOR PLOTTING IS NOT CONTAINED IN JUNCTION DATA')
 5701 FORMAT(/,' ====> ERROR !!! JUNCTION ',A10,
     1  ' REQUESTED FOR PLOTTING IS NOT CONTAINED IN JUNCTION DATA')
 5705 FORMAT(/,' ====> ERROR !!! CONDUIT ',I9,
     1' REQUESTED FOR PLOTTING WATER SURFACE SLOPE IS NOT CONTAINED',
     2' IN CONDUIT DATA')
 5706 FORMAT(/,' ====> ERROR !!! CONDUIT ',A10,
     1' REQUESTED FOR PLOTTING WATER SURFACE SLOPE IS NOT CONTAINED',
     2' IN CONDUIT DATA')
 5707 FORMAT(/,' ====> ERROR !!! FOR BOUNDARY CONDITION NO. ',I3,
     1' AND NTIDE = ',I2,':',/,' NI =',I4,' TIME/STAGE POINTS GREATER TH
     2AN NTVAL = ',I3, 'ALLOWED.  NTVAL VALUE SET IN TAPES.INC')
 5710 FORMAT(1H1,/,
     +' *****************************************************',/,
     +' *        BOUNDARY CONDITON INFORMATION              *',/,
     +' *                 DATA GROUPS J1-J4                 *',/,
     +' *****************************************************',//)
 5715 FORMAT(/,' BC NUMBER..',I10,
     +         ' HAS NO CONTROL WATER SURFACE.')
 5780 FORMAT(/,
     +' ******************************************',/,
     +' *     USER SUPPLIED TIDAL COEFFICIENTS   *',/,
     +' *  FOR BOUNDARY CONDITION # ',I5,'.       *',/,
     +' ******************************************',/,
     +' TIDAL COEFFICIENTS.........',7F10.4,
     +' TIDAL PERIOD (HOURS).......',F10.2)
 5790 FORMAT(/,' BC NUMBER..',I10,
     +         ' CONTROL WATER SURFACE ELEVATION IS..',F10.2,' FEET.')
 5791 FORMAT(/,' BC NUMBER..',I10,
     +         ' CONTROL WATER SURFACE ELEVATION IS..',F10.2,' METERS.')
 5820 FORMAT(/,' ====> WARNING !!! PROGRAM CANNOT MATCH HYDROGRAPH AT NO
     +DE ',I9,' TO JUNCTION DATA.')
 5821 FORMAT(/,' ====> WARNING !!! PROGRAM CANNOT MATCH HYDROGRAPH AT NO
     +DE ',A10,' TO JUNCTION DATA.')
 5830 FORMAT(3X,I5,'/',F7.2,7(3X,I5,'/',F7.2))
 5850 FORMAT(/,' ====> ERROR !!! MORE THAN NIE OUTFALL JUNCTIONS.')
 5920 FORMAT(//,' ====> ERROR !!! EXECUTION TERMINATED BECAUSE OF ',
     *I3,' DATA ERROR(S).')
C=======================================================================
      RETURN
 888  CALL IERROR
      END

      SUBROUTINE INDAT3
C	EXTRAN BLOCK
C	CALLED BY EXTRAN NEAR LINE 200
C=======================================================================
C     THIS SUBROUTINE READS DATA GROUPS J1-J4, EXCEPT FOR THE INPUT
C          HYDROGRAPH LINES READ BY SUBROUTINE INFLOW.  IT ALSO PERFORMS
C          SOME INITIALIZATION.  ALL NODE-CONDUIT LINKAGES ARE
C          SET UP AND CONVERTED TO THE INTERNAL NUMBER SYSTEM.
C     CORRECTION TO J4 READ BY WCH, 4/1/93
C     CORRECTION FOR INITIAL DATE BY RED, 5/12/93
C     CONVERT OPTIONAL USER-INPUT INITIAL DATE TO JULIAN, WCH, 4/11/94.
C     ENHANCE ERROR MESSAGE FOR MIS-MATCH OF INPUT NODES, WCH, 11/10/94.
C     ADD PRINT-OUT FOR K2 INPUT LOCATION LIST AND CHECK FOR DUPLICATE
C       LOCATIONS ON THAT LIST, WCH, 10/17/95.
C     ALLOW MIS-MATCH OF INTERFACE FILE JUNCTIONS AND EXTRAN JUNCTIONS.
C       EXTRAN WILL IGNORE INFLOW FROM UN-MATCHED JUNCTS, WCH, 10/17/95.
C     COMPUTE CORRECT TIME DISPLACEMENT FOR EXTRAN STARTING DATE/TIME
C       WHEN USING HOT START WITH INTERFACE FILE, WCH, 7/25/96.
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
CIM  NEW COMMON FOR WRITING OF RESULTS TO ASCII FILE
      INCLUDE 'CWRITE.INC'
CIM
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
cimtide
C             5  Stage-time history of water surface elevations input
C                by user on J3 line.
C             6  Stage time histories of water surface elevation input
C                on separate file (NSCRAT4)
cimtide
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
cimtide
C=======================================================================
C        READ DOWNSTREAM BOUNDARY STAGE INFORMATION FROM SEPARATE FILE
C        NTIDE = 6
C        CALL INTIDEF TO INITIALIZE TIDE STUFF
C=======================================================================
      IF(NTIDE(I).EQ.6) CALL INTIDEF
CIMTIDE
 1800 CONTINUE
 1801 CONTINUE
C=======================================================================
C     CHECK JTIDES ARRAY FOR BC'S NOT INPUT ON J1-J4 DATA GROUPS
C=======================================================================
      DO 1850 J = 1,NJ
      IF(JTIDES(J).LE.M2S2) GO TO 1850
cimtide
      if(jtides(j).gt.10000) go to 1850
cimtide
      IF(JCE.EQ.0) THEN
        WRITE(N6,1851)  JUN(J),JTIDES(J)
      ELSE
       WRITE(N6,1852) AJUN(J),JTIDES(J)
      ENDIF
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
               IF(JCE.EQ.0) THEN
                 WRITE(N6,5678) CPRT(K)
               ELSE
                 WRITE(N6,5679) AOUT(K,2)
               ENDIF
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
                    IF(JCE.EQ.0) THEN
                      WRITE(N6,5680) KPLT(K)
                    ELSE
                      WRITE(N6,5681) AOUT(K,4)
                    ENDIF
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
                     IF(JCE.EQ.0) THEN
                      WRITE(N6,5690) JPRT(I)
                     ELSE
                      WRITE(N6,5691) AOUT(I,1)
                     ENDIF
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
                    IF(JCE.EQ.0) THEN
                      WRITE(N6,5700) JPLT(N)
                    ELSE
                      WRITE(N6,5701) AOUT(N,3)
                    ENDIF
                    NSTOP = NSTOP+1
                    GO TO 1720
 1700               JPLT(N) = J
 1720               CONTINUE
                    ENDIF
C=======================================================================
      IF(NSURF.GT.0) THEN
                     DO 1730 K = 1,NSURF
                     DO 1740 N = 1,NTL
                     IF(JCE.EQ.0.AND.NCOND(N).EQ.JSURF(K))  GO TO 1735
                     IF(JCE.EQ.1.AND.ACOND(N).EQ.AOUT(K,5)) GO TO 1735
 1740                CONTINUE
                     IF(JCE.EQ.0) THEN
                       WRITE(N6,5705) JSURF(K)
                     ELSE
                       WRITE(N6,5706) AOUT(K,5)
                     ENDIF
                     NSTOP = NSTOP+1
                     GO TO 1730
 1735                JSURF(K) = N
 1730                CONTINUE
                     ENDIF
C=======================================================================
CIM WRITING OF RESULTS TO ASCII FILE
CIM  FIRST FIND CONDUIT NUMBER FOR ALL FLOWOUT OR AFLWOUT
      IF(NOFLOW.GT.0) THEN
                     DO 1751 K = 1,NOFLOW+NOFDUP
                     DO 1750 N = 1,NTL
                     IF(JCE.EQ.0.AND.NCOND(N).EQ.FLOWOUT(K))  GO TO 1755
                     IF(JCE.EQ.1.AND.ACOND(N).EQ.AFLWOUT(K))  GO TO 1755
 1750                CONTINUE
                     IF(JCE.EQ.0) THEN
                       WRITE(N6,5805) FLOWOUT(K)
                     ELSE
                       WRITE(N6,5806) AFLWOUT(K)
                     ENDIF
                     NSTOP = NSTOP+1
                     GO TO 1751
 1755                FLOWOUT(K) = N
 1751                CONTINUE
CIM NOW FIND REFERENCE FOR DUPLICATES
                     DO 1760 K =1,NOFDUP
                     DO 1761 N = 1,NOFLOW
                     NN = FLOWOUT(N)
CIM                  WRITE(N6,*) K,N,NN,ACOND(NN),AFLOWREF(K)
                     IF (JCE.EQ.0.AND.NCOND(NN).EQ.FLOWREF(K))
     1                             GO TO 1766
                     IF (JCE.EQ.1.AND.ACOND(NN).EQ.AFLOWREF(K))
     1                             GO TO 1766
 1761                CONTINUE
                     IF (JCE.EQ.0) THEN
                       WRITE(N6,6807) FLOWREF(K),K
                     ELSE
                       WRITE(N6,6808) AFLOWREF(K),K
                     ENDIF
                     NSTOP = NSTOP + 1
                     GO TO 1760
 1766                FLOWREF(K) = N
 1760                CONTINUE
                     ENDIF
CIM END
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
      IF (MYEAR.LT.100) MYEAR = MYEAR+1900
      JULDAY = 1000*MYEAR + JDATE(MDAY,MMNTH,MYEAR)
      IDATEZ = JULDAY
C####     JULDAY = 88001
C
      IF(LAST.GT.0) THEN
C#### WCH, 7/25/96.
                    TZEROX = TZERO
                    CALL INFACE(1,LAST)
C#######################################################################
C#### WCH, 7/25/96.  ALWAYS START AT DATE GIVEN ON INTERFACE
C     FILE, PLUS TZERO (HOURS) GIVEN ON LINE B1.
C=======================================================================
c                    IF(JREDO.EQ.1.OR.JREDO.EQ.3) THEN
cim doesn't this next line double count TZERO???
Cim                       TZEROS = TZEROX + TZERO
                         TZEROS = TZEROX
                         TIMDAY = TZERO
                         JULDAY = IDATEZ
C=======================================================================
C     NDATE computes new Julian date (JDAY) and time of day (TMDAY, sec)
C     by adding TZEROS (sec) to current Julian date (JULDAY) and time
C     of day (TIMDAY, sec).
C=======================================================================
                         CALL NDATE(TZEROS,JDAY,TMDAY)
                         JULDAY = JDAY
                         TIMDAY = TMDAY
C=======================================================================
C     DATED computes year, month, day etc. from current Julian day and
C     time of day.
C=======================================================================
                         CALL DATED
                         WRITE(N6,5815) JULDAY,NYEAR,MONTH,NDAY,
     +                      TIMDAY/3600.,TZEROX/3600.
c                         ELSE
c                         TIMDAY = TZERO
c                         JULDAY = IDATEZ
ccim add call to dated
c                         CALL DATED
c                         ENDIF
C======================================================================
C                   CONVERT TO INTERNAL NUMBERS
C======================================================================
                    DO 1920 L = 1,LOCATS
                    DO 1880 J = 1,NJ
                    IF(JCE.EQ.0.AND.NLOC(L).EQ.JUN(J)) GO TO 1900
                    IF(JCE.EQ.1.AND.KAN(L).EQ.AJUN(J)) GO TO 1900
 1880               CONTINUE
                    IF(JCE.EQ.0) THEN
                      WRITE(N6,5820) NLOC(L)
                    ELSE
                      WRITE(N6,5821) KAN(L)
                    ENDIF
C#### WCH, 10/17/95.  CHANGE TO WARNING IF NO MATCH.
C####                    NSTOP   = NSTOP + 1
                    NLOC(L) = 0
                    GO TO 1920
 1900               NLOC(L) = J
 1920               CONTINUE
                    ENDIF
      write(n6,*) ' TZERO = ',JULDAY,timday
C======================================================================
C     Read input hydrograph data (lines) on data groups K1-K3.
C======================================================================
      IF(NJSW.GT.0) THEN
C======================================================================
C    >>>>>>>>>>>>> READ LINE K1 <<<<<<<<<<<<<<<<<
C======================================================================
                    READ(N5,*,ERR=888) CC,NINC
C#### WCH, 10/17/95.  ADD PRINT OF NINC.
                    WRITE(N6,5064) NINC
                    DO 1970  L = 1,NJSW,NINC
                                   L1 = L + NINC - 1
                    IF(L1.GT.NJSW) L1 = NJSW
C======================================================================
C    >>>>>>>>>>>>> READ DATA GROUP K2 <<<<<<<<<<<<<
C======================================================================
                    IF(JCE.EQ.0) THEN
                                 READ(N5,*,ERR=888) CC,(JSW(LL),LL=L,L1)
                    ELSE
                                 READ(N5,*,ERR=888) CC,(KSW(LL),LL=L,L1)
                    ENDIF
 1970 CONTINUE
C#######################################################################
C#### WCH, 10/17/95.  PRINT K2 INPUT LOCATIONS AND CHECK FOR DUPLICATE
C     LOCATIONS.  DO NOT ALLOW IDENTICAL LOCATION TO BE INPUT TWICE,
C     CONTRARY TO USER'S MANUAL INSTRUCTIONS, P. 30.
C=======================================================================
                    IF(JCE.EQ.0) THEN
                        WRITE (N6,5065) (JSW(L),L=1,NJSW)
                    ELSE
                        WRITE (N6,5066) (KSW(L),L=1,NJSW)
                    ENDIF
                    IF(NJSW.GT.1) THEN
                         NJSWW = NJSW - 1
                         DO 1975 K = 1,NJSWW
                         LL = K + 1
                         DO 1975 L = LL,NJSW
                         IF(JCE.EQ.0.AND.JSW(K).NE.JSW(L)) GO TO 1975
                         IF(JCE.EQ.1.AND.KSW(K).NE.KSW(L)) GO TO 1975
C=======================================================================
C     HERE IF THERE IS A DUPLICATION OF NUMBERS.
C=======================================================================
                         IF(JCE.EQ.0) THEN
                           WRITE(N6,5067) JSW(K)
                         ELSE
                           WRITE(N6,5068) KSW(K)
                         ENDIF
                         NSTOP = NSTOP + 1
 1975                    CONTINUE
                         ENDIF
C
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
                    IF(JCE.EQ.0) THEN
                      WRITE(N6,5820) JSW(L)
                    ELSE
                      WRITE(N6,5821) KSW(L)
                    ENDIF
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
                    IF(JCE.EQ.0) THEN
                      LOCNOS(I) = JUN(NJUNC(N,1))
                    ELSE
                      KOCNOS(I) = AJUN(NJUNC(N,1))
                    ENDIF
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
                    IF(JCE.EQ.0) THEN
                      WRITE(NEXT) (LOCNOS(I),I=1,MJSW)
                    ELSE
                      WRITE(NEXT) (KOCNOS(I),I=1,MJSW)
                    ENDIF
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
C
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
cred99 STAGE(FT)',/,  - wrong label - 9/9/99
     + STAGE (M)',/,
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
C#### WCH, 10/17/95.  FIVE NEW FORMAT STATEMENTS.
 5064 FORMAT (/,
     1' **************************************************',/,
     2' *   LINE-INPUT HYDROGRAPHS (DATA GROUPS K1-K3)   *',/,
     3' *                                                *',/,
     4' *    Expect ',I3,' junction IDs on each K2 line.    *',/,
     5' **************************************************')
 5065 FORMAT (/,' NJSW INPUT LOCATIONS FROM K2 LINES:',/,(8(I10,1X)))
 5066 FORMAT (/,' NJSW INPUT LOCATIONS FROM K2 LINES:',/,(8(A10,1X)))
 5067 FORMAT (/,' *** ERROR!  INPUT LOCATION ',I10,' IS DUPLICATED LATER
     1 IN K2 LIST.',/,' ALLOW LOCATION TO BE USED ONLY ONCE.  PROGRAM WI
     2LL STOP BELOW.')
 5068 FORMAT (/,' *** ERROR!  INPUT LOCATION ',A10,' IS DUPLICATED LATER
     1 IN K2 LIST.',/,' ALLOW LOCATION TO BE USED ONLY ONCE.  PROGRAM WI
     2LL STOP BELOW.')
C#### WCH, 7/25/96.
 5815 FORMAT(/,' >>> STARTING DATE AND TIME OF EXTRAN RUN ARE:',/,
     2' JULIAN DATE:',I8,/,
     3' YR/MO/DA:   ',I4,'/',I2,'/',I2,/,
     4' TIME OF DAY:',F7.3,' HRS',/,
     5' THIS IS',F7.3,' HOURS BEYOND INTERFACE FILE STARTING TIME',/,
     6' AS PROVIDED BY TZERO ON LINE B1.')
 5678 FORMAT(/,' ====> ERROR !!! CONDUIT',I10,
     1' REQUESTED FOR PRINTOUT IS NOT CONTAINED IN CONDUIT DATA')
 5679 FORMAT(/,' ====> ERROR !!! CONDUIT ',A10,
     1     ' REQUESTED FOR PRINTOUT IS NOT CONTAINED IN CONDUIT DATA')
 5680 FORMAT(/,' ====> ERROR !!! CONDUIT ',I10,
     1' REQUESTED FOR PLOTTING IS NOT CONTAINED IN CONDUIT DATA')
 5681 FORMAT(/,' ====> ERROR !!! CONDUIT ',A10,
     1     ' REQUESTED FOR PLOTTING IS NOT CONTAINED IN CONDUIT DATA')
 5690 FORMAT(/,' ====> ERROR !!! JUNCTION ',I10,
     1     ' REQUESTED FOR PRINTOUT IS NOT CONTAINED IN JUNCTION DATA')
 5691 FORMAT(/,' ====> ERROR !!! JUNCTION ',A10,
     1 ' REQUESTED FOR PRINTOUT IS NOT CONTAINED IN JUNCTION DATA')
 5700 FORMAT(/,' ====> ERROR !!! JUNCTION ',I10,
     1  ' REQUESTED FOR PLOTTING IS NOT CONTAINED IN JUNCTION DATA')
 5701 FORMAT(/,' ====> ERROR !!! JUNCTION ',A10,
     1  ' REQUESTED FOR PLOTTING IS NOT CONTAINED IN JUNCTION DATA')
 5705 FORMAT(/,' ====> ERROR !!! CONDUIT ',I10,
     1' REQUESTED FOR PLOTTING WATER SURFACE SLOPE IS NOT CONTAINED',
     2' IN CONDUIT DATA')
 5706 FORMAT(/,' ====> ERROR !!! CONDUIT ',A10,
     1' REQUESTED FOR PLOTTING WATER SURFACE SLOPE IS NOT CONTAINED',
     2' IN CONDUIT DATA')
CIM WRITING OF FLOW TO FILE
 5805 FORMAT(/,' ====> ERROR !!! CONDUIT ',I10,
     1' REQUESTED FOR WRITING OF FLOWS TO ASCII FILE IS NOT CONTAINED',
     2' IN CONDUIT DATA')
 5806 FORMAT(/,' ====> ERROR !!! CONDUIT ',A10,
     1' REQUESTED FOR WRITING OF FLOWS TO ASCII FILE IS NOT CONTAINED',
     2' IN CONDUIT DATA')
6807  FORMAT(/,' ====> ERROR !!! REFERENCE CONDUIT ',I10,
     1' IS NOT CONTAINED IN CONDUITS REQUESTED ON B9 LINE FOR',
     2' DUPLICATE CONDUIT NUMBER ',I10)
6808  FORMAT(/,' ====> ERROR !!! REFERENCE CONDUIT ',A10,
     1' IS NOT CONTAINED IN CONDUITS REQUESTED ON B9 LINE FOR',
     2' DUPLICATE CONDUIT NUMBER ',I10)
CIM END
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
C#### WCH, 11/10/94.  ENHANCE ERROR MESSAGES IN 5820 AND 5821.
C#### WCH, 10/17/95.  CHANGE 5220,5821 ERROR MESSAGES TO WARNING MESGS.
 5820 FORMAT(/,' ==> WARNING !! PROGRAM CANNOT MATCH HYDROGRAPH AT NODE
     1',   I10,' TO JUNCTION',/,' DATA.  EXTRAN WILL IGNORE INFLOW FROM
     2THIS JUNCTION AND CONTINUE SIMULATION.')
 5821 FORMAT(/,' ==> WARNING !! PROGRAM CANNOT MATCH HYDROGRAPH AT NODE
     1',   A10,' TO JUNCTION',/,' DATA.  EXTRAN WILL IGNORE INFLOW FROM
     2THIS JUNCTION AND CONTINUE SIMULATION.')
 5830 FORMAT(3X,I5,'/',F7.2,7(3X,I5,'/',F7.2))
 5850 FORMAT(/,' ====> ERROR !!! MORE THAN NIE OUTFALL JUNCTIONS.')
 5920 FORMAT(//,' ====> ERROR !!! EXECUTION TERMINATED BECAUSE OF ',
     *I3,' DATA ERROR(S).')
C=======================================================================
      RETURN
 888  CALL IERROR
      END

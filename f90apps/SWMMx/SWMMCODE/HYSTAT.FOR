      SUBROUTINE HYSTAT(X,Y,NPT,NPLTS,LOC,APLOT,TAREA,METRIC,
     +                                       USE,PUNIT,PNAME)
C=======================================================================
C     NEW SUBROUTINE TO CALCULATE SIMPLE STATISTICS OF HYDROGRAPHS.
C     CALLED FROM SUBROUTINE GRAPH OF EXECUTIVE BLOCK.
C     WRITTEN FEBRUARY  1979 BY W.C.H.
C     UPDATED JANUARY   1983 BY W.C.H.
C     LAST UPDATED NOVEMBER 1988 BY R.E.D.
C=======================================================================
      INCLUDE 'TAPES.INC'
      DIMENSION X(201,2),Y(201,2),NPT(2)
      CHARACTER APLOT*10,NAME1*10,NAME2*10,USE*4,PUNIT*8,PNAME*8
      DATA NAME1/'Predicted '/,NAME2/'Measured  '/
C=======================================================================
      IF(NPT(1).LE.1.AND.NPLTS.EQ.1) RETURN
C=======================================================================
C     3630  = 43560 SQ FT/AC / 12 IN/FT.
C=======================================================================
C     4.047 = 43560 SQFT/AC / 304.8 MM/FT / 35.31 CU FT/CU M = CU M/AC-MM.
C=======================================================================
      IF(METRIC.EQ.1) THEN
           TAI = TAREA*3630.
           IF(JCE.EQ.0) THEN
                IF(USE.EQ.'FLOW') WRITE(N6,900)  LOC
                IF(USE.EQ.'CONC') WRITE(N6,1900) LOC,PNAME,PUNIT,PUNIT
                IF(USE.EQ.'LOAD') WRITE(N6,2900) LOC,PNAME
                ELSE
                IF(USE.EQ.'FLOW') WRITE(N6,905)  APLOT
                IF(USE.EQ.'CONC') WRITE(N6,1905) APLOT,PNAME,PUNIT,PUNIT
                IF(USE.EQ.'LOAD') WRITE(N6,2905) APLOT,PNAME
                ENDIF
           ELSE
           TAI = TAREA*4.047
           IF(JCE.EQ.0) THEN
                IF(USE.EQ.'FLOW') WRITE(N6,901)  LOC
                IF(USE.EQ.'CONC') WRITE(N6,1900) LOC,PNAME,PUNIT,PUNIT
                IF(USE.EQ.'LOAD') WRITE(N6,2900) LOC,PNAME
                ELSE
                IF(USE.EQ.'FLOW') WRITE(N6,906)  APLOT
                IF(USE.EQ.'CONC') WRITE(N6,1905) APLOT,PNAME,PUNIT,PUNIT
                IF(USE.EQ.'LOAD') WRITE(N6,2905) APLOT,PNAME
                ENDIF
           ENDIF
C=======================================================================
C     First find statistics of total hydrographs.
C     Do predicted (Subscript 1) first.
C=======================================================================
      LYDIA = 0
      N1    = NPT(1)
      IF(N1.LE.1) GO TO 200
      K1       = 1
      M1       = N1
   50 BIG      = Y(K1,1)
      LL       = K1
      SUM      = 0.0
      TDELT    = 0.0
      DO 100 J = K1,M1
C=======================================================================
C     PREDICTED HYDROGRAPH HAS UNEQUAL TIME SPACING.
C=======================================================================
      IF(J.EQ.K1) T1  = X(J+1,1) - X(J,1)
      IF(J.NE.K1) T1  = X(J,1)   - X(J-1,1)
      SUM   = SUM   + Y(J,1)*T1
      TDELT = TDELT + T1
      IF(J.EQ.K1.OR.J.EQ.M1) SUM = SUM-Y(J,1)*T1/2.0
      IF(Y(J,1).GT.BIG) THEN
                        BIG = Y(J,1)
                        LL  = J
                        ENDIF
  100 CONTINUE
C=======================================================================
C     NOTE: IF METRIC=2, FLOWS ARE IN CU M/SEC.
C           IF METRIC=1, FLOWS ARE IN CFS.
C=======================================================================
C     ALSO HAVE VOLUME AS A DEPTH OVER THE CATCHMENT AREA.
C        METRIC=1, DEPTH=INCHES.
C              =2, DEPTH=MM.
C=======================================================================
      IF(USE.EQ.'FLOW') V1 = SUM*3600.0
      IF(USE.EQ.'CONC') V1 = SUM/TDELT
      IF(USE.EQ.'LOAD') V1 = SUM*3600.0
                        V2 = 0.0
      IF(TAI.GT.0.0)    V2 = V1/TAI
      DIF                  = X(M1,1)-X(K1,1)
      NOP                  = M1-K1+1
      IF(USE.EQ.'CONC') WRITE(N6,911) NAME1,V1,X(LL,1),BIG,
     +                                X(K1,1),X(M1,1),DIF,NOP
      IF(USE.EQ.'LOAD') WRITE(N6,911) NAME1,V1,X(LL,1),BIG,
     +                                X(K1,1),X(M1,1),DIF,NOP
      IF(USE.EQ.'FLOW') WRITE(N6,910) NAME1,V1,V2,X(LL,1),BIG,
     +                                X(K1,1),X(M1,1),DIF,NOP
      IF(LYDIA.EQ.1) GO TO 720
      WRITE(N6,920)
  200 IF(NPLTS.EQ.1) RETURN
C=======================================================================
C     NOW WORK ON MEASURED HYDROGRAPH (SUBSCRIPT 2).
C=======================================================================
      N2 = NPT(2)
      IF(N2.LE.1) RETURN
      K2       = 1
      M2       = N2
  230 BIG      = Y(K2,2)
      LL       = K2
      SUM      = 0.0
      TDELT    = 0.0
      DO 300 J = K2,M2
C=======================================================================
C     FIRST EQUAL TIME SPACING.
C     SECONDLY UNEQUAL TIME SPACING. USE TRAPEZOIDAL RULE.
C=======================================================================
      IF(J.EQ.K2) T2  = X(J+1,2) - X(J,2)
      IF(J.NE.K2) T2  = X(J,2)   - X(J-1,2)
      SUM             = SUM   + Y(J,2)*T2
      TDELT           = TDELT + T2
      IF(J.EQ.K2.OR.J.EQ.M2) SUM = SUM-Y(J,2)*T2/2.0
      IF(Y(J,2).GT.BIG) THEN
                        BIG = Y(J,2)
                        LL  = J
                        ENDIF
  300 CONTINUE
C=======================================================================
      IF(USE.EQ.'FLOW') V1 = SUM*3600.0
      IF(USE.EQ.'CONC') V1 = SUM/TDELT
      IF(USE.EQ.'LOAD') V1 = SUM*3600.0
                        V2 = 0.0
      IF(TAI.GT.0.0)    V2 = V1/TAI
      DIF = X(M2,2)-X(K2,2)
      NOP = M2-K2+1
      IF(USE.EQ.'CONC') WRITE(N6,911) NAME2,V1,X(LL,2),BIG,
     +                                X(K2,2),X(M2,2),DIF,NOP
      IF(USE.EQ.'LOAD') WRITE(N6,911) NAME2,V1,X(LL,2),BIG,
     +                                X(K2,2),X(M2,2),DIF,NOP
      IF(USE.EQ.'FLOW') WRITE(N6,910) NAME2,V1,V2,X(LL,2),BIG,
     +                                X(K2,1),X(M2,2),DIF,NOP
      IF(LYDIA.EQ.1) GO TO 750
      WRITE(N6,920)
      IF(N1.LE.1) RETURN
C=======================================================================
C     NOW, FIND OVERLAPPING TIMES AND REPEAT CALCULATIONS.
C     FIRST, SEARCH FOR COMMON INITIAL TIME.
C=======================================================================
      K1 = 1
      K2 = 1
      IF(X(1,1).GE.X(1,2)) GO TO 400
C=======================================================================
C     HERE, MEASURED STARTS AFTER PREDICTED.
C=======================================================================
      X12      = X(1,2)
      DO 330 J = 1,N1
      K1       = J
      IF(X(J,1).GE.X12) GO TO 350
  330 CONTINUE
C=======================================================================
C     IF A RETURN IS REACHED, THERE IS NO OVERLAPPING SEGMENT.
C=======================================================================
      RETURN
  350 IF(X(K1,1).EQ.X12) GO TO 500
      IF(X(K1,1)-X12.LE.X12-X(K1-1,1)) GO TO 500
      K1 = K1-1
      GO TO 500
  400 IF(X(1,1).EQ.X(1,2)) GO TO 500
C=======================================================================
C     HERE, MEASURED STARTS BEFORE PREDICTED.
C=======================================================================
      X11      = X(1,1)
      DO 430 J = 1,N2
      K2       = J
      IF(X(J,2).GE.X11) GO TO 450
  430 CONTINUE
      RETURN
  450 IF(X(K2,2).EQ.X11) GO TO 500
      IF(X(K2,2)-X11.LE.X11-X(K2-1,2)) GO TO 500
      K2 = K2-1
C=======================================================================
C     NOW , SEARCH FOR COMMON END TIME.
C=======================================================================
  500 M1 = N1
      M2 = N2
      IF(X(N1,1).LE.X(N2,2)) GO TO 600
C=======================================================================
C     HERE, MEASURED ENDS BEFORE PREDICTED.
C=======================================================================
      XM       = X(N2,2)
      DO 530 J = 1,N1
      L        = N1-J+1
      M1       = L
      IF(X(L,1).LE.XM) GO TO 550
  530 CONTINUE
      RETURN
  550 IF(X(M1,1).EQ.XM) GO TO 700
      IF(XM-X(M1,1).LE.X(M1+1,1)-XM) GO TO 700
      M1 = M1+1
      GO TO 700
  600 IF(X(N1,1).EQ.X(N2,2)) GO TO 700
C=======================================================================
C     HERE, MEASURED ENDS AFTER PREDICTED.
C=======================================================================
      XP       = X(N1,1)
      DO 630 J = 1,N2
      L        = N2-J+1
      M2       = L
      IF(X(L,2).LE.XP) GO TO 650
  630 CONTINUE
      RETURN
  650 IF(X(M2,2).EQ.XP) GO TO 700
      IF(XP-X(M2,2).LE.X(M2+1,2)-XP) GO TO 700
      M2 = M2+1
C=======================================================================
C     USE PREVIOUS CODING TO CALCULATE STATISTICS.
C=======================================================================
  700 LYDIA = 1
      GO TO 50
  720 WRITE(N6,930)
      BIGP = BIG
      V1P  = V1
      V2P  = V2
      XP   = X(LL,1)
      GO TO 230
  750 WRITE(N6,930)
C=======================================================================
C     COMPUTE DIFFERENCES AND PERCENT DIFFERENCES.
C=======================================================================
      V1P  = V1-V1P
      V2P  = V2-V2P
      XP   = X(LL,2)-XP
      BIGP = BIG-BIGP
      WRITE(N6,940) V1P,V2P,XP,BIGP
      V1P  = V1P/V1*100.0
      BIGP = BIGP/BIG*100.0
      WRITE(N6,950) V1P,BIGP
      RETURN
C=======================================================================
  900 FORMAT(/,20X,'Hydrograph Statistics for location ',I9,//,
     1 14X,'========VOLUME======   ====PEAK FLOW====  ==========DURATION
     1==========    NUMBER',/,
     2 14X,'CUBIC FEET    INCHES   TIME,HR  FLOW,CFS  START,HR    END,HR
     2 LENGTH,HR    POINTS',/,
     3 14X,'---------- --------- --------- --------- --------- ---------
     3 --------- ---------')
  901 FORMAT(//,20X,'Hydrograph Statistics for location ',I9,//,
     1 14X,'========VOLUME======   ====PEAK FLOW====  ==========DURATION
     1==========    NUMBER',/,
     2 14X,'CUBIC MET.        MM   TIME,HR  FLOW,CMS  START,HR    END,HR
     2  LENGTH,HR    POINTS',/,
     3 14X,'---------- --------- --------- --------- --------- ---------
     3 ---------- ---------')
  905 FORMAT(/,20X,'Hydrograph Statistics for location ',A10,//,
     1 14X,'========VOLUME======   ====PEAK FLOW====  ==========DURATION
     1==========    NUMBER',/,
     2 14X,'CUBIC FEET    INCHES   TIME,HR  FLOW,CFS  START,HR    END,HR
     2 LENGTH,HR    POINTS',/,
     3 14X,'---------- --------- --------- --------- --------- ---------
     3 --------- ---------')
  906 FORMAT(//,20X,'Hydrograph Statistics for location ',A10,//,
     1 14X,'========VOLUME======   ====PEAK FLOW====  ==========DURATION
     1==========    NUMBER',/,
     2 14X,'CUBIC MET.        MM   TIME,HR  FLOW,CMS  START,HR    END,HR
     2  LENGTH,HR    POINTS',/,
     3 14X,'---------- --------- --------- --------- --------- ---------
     3 ---------- ---------')
  910 FORMAT(/,1X,A10,3X,G10.3,6(1X,G9.3),I10)
  911 FORMAT(/,1X,A10,3X,6(1X,G9.3),I10)
  920 FORMAT('TOTAL TIME')
  930 FORMAT('OVERLAP TIME')
  940 FORMAT(/,'DIFFERENCES   ',/,
     +         '   ABSOLUTE   ',4(1X,F9.3))
  950 FORMAT(' % OF MEASURED',10X,F10.3,10X,F10.3)
 1900 FORMAT(/,20X,'POLLUTOGRAPH STATISTICS FOR LOCATION ',I9,/,
     1         '                  QUALITY CONSTITUENT ',A8,//,
     113X,'AVERAGE CONCENTRATION  PEAK CONCENTRATION   =====DURATION====
     1    NUMBER',/,
     214X,A8,'     TIME,HR',2X,A8,'  START,HR    END,HR LENGTH,HR    POI
     2NTS',/,
     314X,'-------  -----------  --------  --------  --------  --------
     3 --------')
 1905 FORMAT(/,20X,'POLLUTOGRAPH STATISTICS FOR LOCATION ',A10,/,
     1         '                  QUALITY CONSTITUENT ',A8,//,
     113X,'AVERAGE CONCENTRATION  PEAK CONCENTRATION   =====DURATION====
     1    NUMBER',/,
     214X,A8,'     TIME,HR',2X,A8,'  START,HR    END,HR LENGTH,HR    POI
     2NTS',/,
     314X,'-------  -----------  --------  --------  --------  --------
     3 --------')
 2900 FORMAT(/,20X,'Loadograph statistics for location ',I9,/,
     1         '                     QUALITY CONSTITUENT ',A8,'.',//,
     114X,'=========MASS=======  ====PEAK RATE=====   =====DURATION====
     1   NUMBER',/,
     214X,'    POUNDS  LBS/AREA   TIME,HR   LBS/SEC    END,HR LENGTH,HR
     2   POINTS',/,
     314X,'---------- ---------  --------  --------  --------  --------
     3 --------')
 2905 FORMAT(/,20X,'Loadograph Statistics for location ',A10,/,
     1         '                     QUALITY CONSTITUENT ',A8,'.',//,
     114X,'=========MASS=======  ====PEAK RATE=====   =====DURATION====
     1   NUMBER',/,
     214X,'    POUNDS  LBS/AREA   TIME,HR   LBS/SEC    END,HR LENGTH,HR
     2   POINTS',/,
     314X,'---------- ---------  --------  --------  --------  --------
     3 --------')
C=======================================================================
      END

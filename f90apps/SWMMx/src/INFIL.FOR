      SUBROUTINE INFIL
C=======================================================================
C     ROUTINE TO INPUT INFILTRATION PARAMETERS AND CALCULATE
C     INFILTRATION INPUT TO MANHOLES OR OTHER NON-CONDUITS.
C     UPDATED SEPTEMBER 1981 BY W.C.H.
C     UPDATED NOVEMBER 1988 BY R.E.D.
C     UPDATED NOVEMBER 1992 BY WCH TO CORRECT METRIC CONVERSION
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TABLES.INC'
      INCLUDE 'NAMES.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      DIMENSION NDD(12),NDXDAY(380)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP K1 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,DINFIL,GINFIL,RINFIL,RSMAX,
     +                      (CPINF(I),I=1,NPOLL)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP K2 <<<<<<<<<<<<
C=======================================================================
      READ (N5,*,ERR=888) CC,(NDD(I),I=1,12)
                      CMET3 = 1.0
      IF(METRIC.EQ.2) CMET3 = 35.31
      WRITE (N6,600)
      IF(METRIC.EQ.1) WRITE(N6,601) DINFIL,GINFIL,RINFIL,RSMAX
      IF(METRIC.EQ.2) WRITE(N6,602) DINFIL,GINFIL,RINFIL,RSMAX
      IF(NPOLL.GT.0)  WRITE(N6,603) (I,PNAME(I),CPINF(I),
     +                               PUNIT(I),I=1,NPOLL)
      NDYUD = JULDAY - 1000*(JULDAY/1000)
      WRITE (N6,606) (I,NDD(I),I=1,12)
      SINFIL = 0.0
      IF(GINFIL.GT.0.0) GO TO 50
      IF(RSMAX.LE.0.0) GO TO 200
C=======================================================================
C     HERE, WILL USE DEGREE-DAYS TO CALC. RESIDUAL MOISTURE MELT.
C     DEGREE DAYS IN DEG. F * DAYS ONLY (NO METRIC).
C=======================================================================
      MFREZ = 0
      MLTBE = 0
      MLTEN = 0
C=======================================================================
C     PLACE MONTHLY VALUES AT MIDDLE OF MONTH
C=======================================================================
      II            = 0
      DO 100 I      = 1,12
      NDDAY         = II+15
      NDXDAY(NDDAY) = NDD(I)
      II            = II+30
  100 CONTINUE
C=======================================================================
C     INTERPOLATE FOR FIRST AND LAST 15 DAYS OF 'YEAR'
C=======================================================================
      NX  = 345
      NY1 = NDXDAY(345)
      NY2 = NDXDAY(15)
      NX1 = 345
      NX2 = 375
      DO 110 I   = 1,30
      NX         = NX+1
      NY         = ((NY2-NY1)*(NX-NX1))/30 + NY1
      NDXDAY(NX) = NY
  110 CONTINUE
C=======================================================================
C     CONVERT LAST 15 DAYS TO FIRST 15
C=======================================================================
      NX        = 360
      DO 120 I  = 1,15
      NX        = NX+1
      NDXDAY(I) = NDXDAY(NX)
  120 CONTINUE
C=======================================================================
C     INTERPOLATE FROM NDDAY=16 THRU NDDAY=344
C=======================================================================
      K   = 1
      NX  = 16
      NX1 = 15
      NX2 = 45
  130 NY1 = NDXDAY(NX1)
      NY2 = NDXDAY(NX2)
      NY  = ((NY2-NY1)*(NX-NX1))/30 + NY1
      NDXDAY(NX) = NY
      K  = K+1
      NX = NX+1
      IF(K.LE.30) GO TO 130
      NX1 = NX1+30
      NX2 = NX2+30
      K   = 1
      IF(NX2.LE.345) GO TO 130
C=======================================================================
C     DETERMINE BEGINNING OF FREEZING PERIOD(MFREZ)
C=======================================================================
      DO 150 I = 1,360
      IF(NDXDAY(I).GT.750) GO TO 160
  150 CONTINUE
  160 MFREZ = I
C=======================================================================
C     IF STORM DAY IS PRIOR TO FREEZING, SET SINFIL=0.0
C=======================================================================
      IF(NDYUD.LE.MFREZ) GO TO 200
C=======================================================================
C     STORM OCCURED AFTER FREEZING BEGAN
C=======================================================================
      NTOT = 0
      DO 170 I = MFREZ,360
      IF(NDXDAY(I).LE.750) GO TO 175
  170 NTOT   = NTOT+(NDXDAY(I)-750)
  175 NAREA1 = NTOT
      MLTBE  = I
      NTOT   = 0
      IF(NDYUD.LT.MLTBE) GO TO 200
      DO 180 I = MLTBE,360
      NTOT     = NTOT+(750-NDXDAY(I))
      IF(NTOT.GE.NAREA1) GO TO 185
  180 CONTINUE
C=======================================================================
C     IF EQUAL AREA NOT REACHED SET MLTEN=360
C=======================================================================
  185 MLTEN = I
      IF(NDYUD.GE.MLTEN) GO TO 200
C=======================================================================
C     CALCULATE SINFIL
C=======================================================================
      XMLTBE = MLTBE
      XNDYUD = NDYUD
      XMLTEN = MLTEN
      XXARG  = ((XNDYUD-XMLTBE)/(XMLTEN-XMLTBE))*3.1416
      SINFIL = RSMAX*SIN(XXARG)
      IF(METRIC.EQ.1) WRITE (N6,610) SINFIL
      IF(METRIC.EQ.2) WRITE (N6,611) SINFIL
      GO TO 40
  200 SINFIL = 0.0
   40 QINF   = RINFIL+SINFIL+DINFIL
      GO TO 60
   50 QINF = GINFIL
   60 CONTINUE
      OPINF   = 0.0
      OPNFIL  = 0.0
      DO 70 K = 1,NE
      M       = JR(K)
      NT      = NTYPE(M)
      IF(KLASS(NT).LE.2) OPINF = SQRT(AFULL(M))*DIST(M)+OPINF
   70 CONTINUE
      IF(METRIC.EQ.1) WRITE (N6,615) QINF
      IF(METRIC.EQ.2) WRITE (N6,616) QINF
      WRITE (N6,620) DINFIL,GINFIL,SINFIL,RINFIL
      WRITE (N6,630)
      IF(METRIC.EQ.1) WRITE (N6,631)
      IF(METRIC.EQ.2) WRITE (N6,632)
C
      SUM     = 0.0
      DO 80 K = 1,NE
      M       = JR(K)
      NT      = NTYPE(M)
      IF(KLASS(NT).LE.2) THEN
                         OPNFIL     = SQRT(AFULL(M))*DIST(M)
                         OP         = OPNFIL/OPINF
                         QQINF      = QINF*OP
                         MI         = INUE(M,1)
C###### WCH, 11/93
C SHOULD MULTIPLY BY CMET3, NOT DIVIDE
C######
                         QINFIL(MI) = QINFIL(MI) + QQINF*CMET3
                         SUM        = SUM + QQINF
                         IF(JCE.EQ.0) WRITE(N6,640) NOE(M),
     +                                              QQINF,OP,NOE(MI)
                         IF(JCE.EQ.1) WRITE(N6,641) KOE(M),
     +                                              QQINF,OP,KOE(MI)
                         ENDIF
   80 CONTINUE
      WRITE(N6,645) SUM
      RETURN
  888 CALL IERROR
C=======================================================================
  500 FORMAT (2X,F8.0,7F10.0)
  501 FORMAT (2X,I3,11I5)
  600 FORMAT(1H1,/,
     1' *******************************************************',/,
     2' * INFILTRATION APPORTIONMENT THROUGH CONDUIT NETWORK. *',/,
     3' *              INPUT TO SUBROUTINE INFIL:             *',/,
     4' *******************************************************'/)
  601 FORMAT(' DINFIL =',F10.4,' CFS. BASE DRY-WEATHER INFILTRATION.',/,
     1' GINFIL =',F10.4,' CFS. GROUNDWATER INFILTRATION.',/,
     2' RINFIL =',F10.4,' CFS. RAINWATER INFILTRATION.',/,
     3' RSMAX  =',F10.4,' CFS. PEAK RESIDUAL MOISTURE (TO BE MELTED).')
  602 FORMAT (' DINFIL =',F10.4,' CU M/SEC. BASE DRY-WEATHER INFILTRATIO
     1N.',/,
     2' GINFIL =',F10.4,' CU M/SEC. GROUNDWATER INFILTRATION.',/,
     3' RINFIL =',F10.4,' CU M/SEC. RAINWATER INFILTRATION.',/,
     4' RSMAX  =',F10.4,' CU M/SEC. PEAK RESIDUAL MOISTURE (TO BE MELTED
     5).')
  603 FORMAT(//,
     1' ************************************************',/,
     2' * CONCENTRATION OF POLLUTANTS IN INFILTRATION: *',/,
     3' ************************************************',//,
     1' NO.   NAME    CONCENTRATION   UNITS',/,
     2' ---   ----    -------------   -----',/,
     3           4(1X,I2,2X,A8,F13.3,4X,A8,/))
  606 FORMAT (//,
     1' ***************************************************',/
     1' * DEGREE-DAYS (DEG F * DAYS), JANUARY - DECEMBER: *',/,
     2' ***************************************************',/,
     3        1X,6(I5,' = ',I5),/,1X,6(I5,' = ',I5))
  610 FORMAT (//,' CALCULATED MELT INFILTRATION (SINFIL) =',F10.4,
     1 ' CFS.')
  611 FORMAT (//,' CALCULATED MELT INFILTRATION (SINFIL) =',F10.4,
     1 ' CU M/SEC.')
  615 FORMAT (//,10X,'TOTAL AREA INFILTRATION (CFS) =',F10.4,', DUE TO:
     1',//,
     2 10X,'BASE FLOW',5X,'GROUND WATER',11X,'    MELT',5X,'  RAIN',/,
     3 10X,'---------',5X,'------------',11X,'    ----',5X,'  ----')
  616 FORMAT (//,10X,'TOTAL AREA INFILTRATION (CU M/SEC) =',F10.4,
     1 ', DUE TO:',//,
     2 10X,'BASE FLOW',5X,'GROUND WATER',11X,'    MELT',5X,'  RAIN',/,
     3 10X,'---------',5X,'------------',11X,'    ----',5X,'  ----')
  620 FORMAT(10X,F9.4,5X,F12.4,11X,F8.4,3X,F8.4,//)
  630 FORMAT (/,
     1' ******************************************',/,
     1' * APPORTIONED INFILTRATION (BY RELATIVE  *',/,
     3' * CONDUIT PERIMETER AREA):               *',/,
     1' ******************************************',//,
     112X,'   INFILTRATION',15X,'  PROPORTION  ',T58,'INFIL.INPUT AT')
  631 FORMAT (1X,'ELEMENT NO.',10X,'(CFS)',15X,'OF TOT. INFIL.',T58,
     1'UPSTREAM ELE.NO.',/,1X,'-----------',5X,'----------',15X,
     2'--------------',T58,'----------------',/)
  632 FORMAT (1X,'ELEMENT NO.',10X,'(CMS)',15X,'OF TOT. INFIL.',T58,
     1'UPSTREAM ELE.NO.',/,1X,'-----------',10X,'-----',15X,
     2'--------------',T58,'----------------',/)
  640 FORMAT(1X,I5,10X,F12.6,12X,F8.4,T63,I5)
  641 FORMAT(1X,A10,5X,F12.6,12X,F8.4,T63,A10)
  645 FORMAT(' TOTAL INFIL ',3X,F12.6)
C=======================================================================
      END

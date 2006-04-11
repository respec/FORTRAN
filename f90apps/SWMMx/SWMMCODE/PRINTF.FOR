      SUBROUTINE PRINTF(IGO)
C=======================================================================
C     ROUTINE TO PRINT INFLOWS AND OUTFLOWS ONLY, (NO QUALITY).
C     ALSO INCLUDES COMPUTATION OF VOLUMES AND FIRST TWO MOMENTS.
C     W.C.H., SEPT. 1981.  (UPDATED JANUARY 1988.)
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'HUGO.INC'
      INCLUDE 'NEW81.INC'
      DIMENSION CMEAN(10),TOTFLO(10),TOTSQ(10),QQ(10),FMAX(10),FMIN(10)
      CHARACTER BMJ*2
      DATA BMJ/'  '/
C=======================================================================
      XNDT    = FLOAT(NDT)
      IF(IGO.EQ.1) GO TO 700
      II = 0
   50 II = II+1
      IF(JPRINT.EQ.2) II = 2
      IF(JPRINT.EQ.1.AND.II.EQ.2) GO TO 600
C=======================================================================
C     II = 1, READ AND PRINT INFLOWS.
C          2, READ AND PRINT OUTFLOWS.
C=======================================================================
      NTX = NSCRAT(II)
      REWIND NTX
C=======================================================================
C     PRINT OUT RESULTS FOR 10 ELEMENTS AT A TIME.
C     MUST RE-READ SCRATCH FILE FOR EACH GROUP OF 10.
C
C     BEGIN LOOP ON GROUPS OF 10.
C=======================================================================
      IF(II.EQ.2)   NNYN = NNPE
      DO 500 JJ = 1,NNYN,10
                          JLAST = 10
      IF(NNYN-JJ+1.LT.10) JLAST = NNYN-JJ+1
      LASTEL    = JJ+JLAST-1
      NREAD1    = JJ-1
      NREAD2    = NNYN-NREAD1-JLAST
      JULDAY    = IDATEZ
      TIMDAY    = TZERO
      DO 100 I  = 1,10
      FMIN(I)   = 1.0E30
      FMAX(I)   = 0.0
      TOTFLO(I) = 0.0
  100 TOTSQ(I)  = 0.0
C=======================================================================
C     RE-READ HEADER INFORMATION OFF FILE.
C=======================================================================
      REWIND NTX
C=======================================================================
C     PRINT HEADINGS.
C=======================================================================
      IF(II.EQ.1) WRITE (N6,9020)
      IF(II.EQ.2) WRITE (N6,9030)
      WRITE(N6,9000) TITLE(1),TITLE(2)
      IF(METRIC.EQ.1) WRITE (N6,9032)
      IF(METRIC.EQ.2) WRITE (N6,9034)
      IF(II.EQ.1) THEN
                  WRITE(N6,9040) (BMJ,I=JJ,LASTEL)
                  IF(JCE.EQ.0) WRITE(N6,9060) (NYN(I),I=JJ,LASTEL)
                  IF(JCE.EQ.1) WRITE(N6,9061) (KYN(I),I=JJ,LASTEL)
                  ENDIF
      IF(II.EQ.2) THEN
                  WRITE(N6,9050) (BMJ,I=JJ,LASTEL)
                  IF(JCE.EQ.0) WRITE(N6,9060) (NPE(I),I=JJ,LASTEL)
                  IF(JCE.EQ.1) WRITE(N6,9061) (KPE(I),I=JJ,LASTEL)
                  ENDIF
      WRITE (N6,9065) (BMJ,I=JJ,LASTEL)
C=======================================================================
C     BEGIN LOOP ON NO. OF TIME STEPS.
C     PRINT RESULT FOR EACH TIME STEP.
C=======================================================================
      KLINE     = 1
      KPR       = 0
      DO 400 KK = 1,NDT
      KPR       = KPR+1
      IF(KPR.GT.INTPRT) KPR = 1
C=======================================================================
C     UPDATE TIME AND CALENDAR.
C=======================================================================
      CALL STIME(DT)
      CALL DATED
C=======================================================================
C     READ GROUP OF ELEMENTS ALREADY PRINTED.
C=======================================================================
      IF(NREAD1.GT.0) THEN
                      DO 150 I = 1,NREAD1
  150                 READ (NTX) DUM
                      ENDIF
C=======================================================================
C     HERE, READ RECORDS OF INTEREST.
C=======================================================================
      DO 180 I = 1,JLAST
  180 READ (NTX) QQ(I)
C=======================================================================
C     READ REMAINING ELEMENTS ON FILE FOR THIS TIME STEP.
C=======================================================================
      IF(NREAD2.GT.0) THEN
                      DO 190 I = 1,NREAD2
  190                 READ (NTX) DUM
                      ENDIF
C=======================================================================
C     PERFORM SUMMATIONS AND CONVERT TO METRIC, IF NECESSARY.
C=======================================================================
      DO 220 I  = 1,JLAST
      TOTFLO(I) = TOTFLO(I) + QQ(I)
      TOTSQ(I)  = TOTSQ(I)  + QQ(I)*QQ(I)
      QQ(I)     = QQ(I)/CMET(8,METRIC)
      IF(QQ(I).LT.FMIN(I)) FMIN(I) = QQ(I)
  220 IF(QQ(I).GT.FMAX(I)) FMAX(I) = QQ(I)
C
      IF(KPR.EQ.INTPRT) THEN
             WRITE (N6,9250) MONTH,NDAY,NYEAR,JHR,MINUTE,
     +                       JSEC,KK,(QQ(I),I=1,JLAST)
             KLINE = KLINE + 1
             IF(MOD(KLINE,50).EQ.0.AND.KK+INTPRT.NE.NDT) THEN
                        IF(II.EQ.1) WRITE (N6,9020)
                        IF(II.EQ.2) WRITE (N6,9030)
                        WRITE(N6,9000) TITLE(1),TITLE(2)
                        IF(METRIC.EQ.1) WRITE (N6,9032)
                        IF(METRIC.EQ.2) WRITE (N6,9034)
                        IF(II.EQ.1) THEN
                           WRITE(N6,9040) (BMJ,I=JJ,LASTEL)
                           IF(JCE.EQ.0) WRITE(N6,9060)
     +                                  (NYN(I),I=JJ,LASTEL)
                           IF(JCE.EQ.1) WRITE(N6,9061)
     +                                  (KYN(I),I=JJ,LASTEL)
                           ENDIF
                        IF(II.EQ.2) THEN
                           WRITE(N6,9050) (BMJ,I=JJ,LASTEL)
                           IF(JCE.EQ.0) WRITE(N6,9060)
     +                                  (NPE(I),I=JJ,LASTEL)
                           IF(JCE.EQ.1) WRITE(N6,9061)
     +                                  (KPE(I),I=JJ,LASTEL)
                           ENDIF
                        WRITE (N6,9065) (BMJ,I=JJ,LASTEL)
                        ENDIF
             ENDIF
  400 CONTINUE
C=======================================================================
C     COMPUTE MOMENTS.
C=======================================================================
      DO 450 I  = 1,JLAST
      CMEAN(I)  = TOTFLO(I)/XNDT
      ARG       = TOTSQ(I)-XNDT*CMEAN(I)**2
      TOTSQ(I)  = 0.0
      IF(ARG.GT.0.0) TOTSQ(I) = SQRT(ARG)/(XNDT-1.0)
      CMEAN(I)  = CMEAN(I)/CMET(8,METRIC)
      TOTSQ(I)  = TOTSQ(I)/CMET(8,METRIC)
  450 TOTFLO(I) = TOTFLO(I)*DT/CMET(8,METRIC)
C
      WRITE (N6,9065) (BMJ,I=JJ,LASTEL)
      WRITE (N6,9460) (CMEAN(I),I=1,JLAST)
      WRITE (N6,9470) (TOTSQ(I),I=1,JLAST)
      WRITE (N6,9474) (FMAX(I),I=1,JLAST)
      WRITE (N6,9476) (FMIN(I),I=1,JLAST)
      IF(METRIC.EQ.1) WRITE (N6,9480) (TOTFLO(I),I=1,JLAST)
      IF(METRIC.EQ.2) WRITE (N6,9490) (TOTFLO(I),I=1,JLAST)
  500 CONTINUE
      IF(II.EQ.1) GO TO 50
  600 CONTINUE
C=======================================================================
C     WRITE THE CHANNEL DEPTH INFORMATION( NEW TO SWMM 4.0)
C=======================================================================
  700 CONTINUE
      IF(NSURF.GT.0) THEN
      NTX        = NSCRAT(7)
      DO 4500 JJ = 1,NSURF,10
                           JLAST = 10
      IF(NSURF-JJ+1.LT.10) JLAST = NSURF-JJ+1
      LASTEL = JJ + JLAST - 1
      NREAD1 = JJ - 1
      NREAD2 = NSURF-NREAD1-JLAST
      JULDAY = IDATEZ
      TIMDAY = TZERO
C=======================================================================
      DO 4600 I  = 1,10
      FMIN(I)    = 1.0E30
      FMAX(I)    = 0.0
      TOTFLO(I)  = 0.0
 4600 TOTSQ(I)   = 0.0
C=======================================================================
C     RE-READ HEADER INFORMATION OFF FILE.
C=======================================================================
      REWIND NTX
C=======================================================================
C     PRINT HEADINGS.
C=======================================================================
      IF(METRIC.EQ.1) WRITE(N6,4030) TITLE(1),TITLE(2)
      IF(METRIC.EQ.2) WRITE(N6,4035) TITLE(1),TITLE(2)
      IF(METRIC.EQ.1) WRITE(N6,4032)
      IF(METRIC.EQ.2) WRITE(N6,4034)
                      WRITE(N6,9045) (BMJ,I=JJ,LASTEL)
      IF(JCE.EQ.0)    WRITE(N6,9060) (JSURF(I),I=JJ,LASTEL)
      IF(JCE.EQ.1)    WRITE(N6,9061) (KOE(JSURF(I)),I=JJ,LASTEL)
                      WRITE(N6,9065) (BMJ,I=JJ,LASTEL)
C=======================================================================
C     BEGIN LOOP ON NO. OF TIME STEPS.
C     PRINT RESULT FOR EACH TIME STEP.
C=======================================================================
      KLINE      = 1
      KPR        = 0
      DO 4000 KK = 1,NDT
                        KPR = KPR+1
      IF(KPR.GT.INTPRT) KPR = 1
C=======================================================================
C     UPDATE TIME AND CALENDAR.
C=======================================================================
      CALL STIME(DT)
      CALL DATED
C=======================================================================
C     READ GROUP OF ELEMENTS ALREADY PRINTED.
C=======================================================================
      IF(NREAD1.GT.0) THEN
                      DO 4150 I = 1,NREAD1
 4150                 READ (NTX) DUM
                      ENDIF
C=======================================================================
C     HERE, READ RECORDS OF INTEREST.
C=======================================================================
      DO 4180 I = 1,JLAST
 4180 READ (NTX) QQ(I)
C=======================================================================
C     READ REMAINING ELEMENTS ON FILE FOR THIS TIME STEP.
C=======================================================================
      IF(NREAD2.GT.0) THEN
                      DO 4190 I = 1,NREAD2
 4190                 READ (NTX) DUM
                      ENDIF
C=======================================================================
C     PERFORM SUMMATIONS AND CONVERT TO METRIC, IF NECESSARY.
C=======================================================================
      DO 4220 I  = 1,JLAST
      TOTFLO(I)  = TOTFLO(I) + QQ(I)
      TOTSQ(I)   = TOTSQ(I)  + QQ(I)*QQ(I)
      QQ(I)      = QQ(I)/CMET(1,METRIC)
      IF(QQ(I).LT.FMIN(I)) FMIN(I) = QQ(I)
 4220 IF(QQ(I).GT.FMAX(I)) FMAX(I) = QQ(I)
C
      IF(KPR.EQ.INTPRT) THEN
             WRITE (N6,9250) MONTH,NDAY,NYEAR,JHR,MINUTE,
     +                       JSEC,KK,(QQ(I),I=1,JLAST)
             KLINE = KLINE + 1
             IF(MOD(KLINE,50).EQ.0.AND.KK+INTPRT.NE.NDT) THEN
                        IF(METRIC.EQ.1) WRITE(N6,4030)
     +                                  TITLE(1),TITLE(2)
                        IF(METRIC.EQ.2) WRITE(N6,4035)
     +                                  TITLE(1),TITLE(2)
                        IF(METRIC.EQ.1) WRITE(N6,4032)
                        IF(METRIC.EQ.2) WRITE(N6,4034)
                        WRITE(N6,9045)  (BMJ,I=JJ,LASTEL)
                        IF(JCE.EQ.0) WRITE(N6,9060)
     +                               (JSURF(I),I=JJ,LASTEL)
                        IF(JCE.EQ.1) WRITE(N6,9061)
     +                               (KOE(JSURF(I)),I=JJ,LASTEL)
                        WRITE (N6,9065) (BMJ,I=JJ,LASTEL)
                        ENDIF
             ENDIF
 4000 CONTINUE
C=======================================================================
C     COMPUTE MOMENTS.
C=======================================================================
      DO 4450 I  = 1,JLAST
      CMEAN(I)   = TOTFLO(I)/XNDT
      ARG        = TOTSQ(I)-XNDT*CMEAN(I)**2
      TOTSQ(I)   = 0.0
      IF(ARG.GT.0.0) TOTSQ(I) = SQRT(ARG)/(XNDT-1.0)
      CMEAN(I)   = CMEAN(I)/CMET(1,METRIC)
 4450 TOTSQ(I)   = TOTSQ(I)/CMET(1,METRIC)
C=======================================================================
      WRITE(N6,9065) (BMJ,I=JJ,LASTEL)
      WRITE(N6,9560) (CMEAN(I),I=1,JLAST)
      WRITE(N6,9570) (TOTSQ(I),I=1,JLAST)
      WRITE(N6,9574) (FMAX(I),I=1,JLAST)
      WRITE(N6,9576) (FMIN(I),I=1,JLAST)
 4500 CONTINUE
      ENDIF
      RETURN
C=======================================================================
 4030 FORMAT(1H1,/,
     1' ************************************************',/,
     1' *  SUMMARY OF QUANTITY RESULTS (DEPTH IN FEET) *',/,
     1' ************************************************',//,
     14X,A80,/,4X,A80,/)
 4035 FORMAT(1H1,/,
     1' **************************************************',/,
     1' *  SUMMARY OF QUANTITY RESULTS (DEPTH IN METERS) *',/,
     1' **************************************************',//,
     14X,A80,/,4X,A80,/)
 4032 FORMAT (/,5X,'ALL DEPTHS ARE IN UNITS OF FEET.')
 4034 FORMAT (/,5X,'ALL DEPTHS ARE IN UNITS OF METERS.')
 9000 FORMAT (/,11X,A80,/,11X,A80)
 9020 FORMAT(/,1H1,/,10X,
     +' *******************************************************',/,10X,
     +' *      SELECTED INLET HYDROGRAPHS AND POLLUTOGRAPHS   *',/,10X,
     +' *******************************************************')
 9030 FORMAT(/,1H1,/,10X,
     +' *******************************************************',/,10X,
     +' *     SELECTED OUTFLOW HYDROGRAPHS AND POLLUTOGRAPHS  *',/,10X,
     +' *******************************************************')
 9032 FORMAT (//,35X,'ALL FLOW RATES ARE IN UNITS OF CFS.')
 9034 FORMAT (//,35X,'ALL FLOW RATES ARE IN UNITS OF CU M/SEC.')
 9040 FORMAT (//,7X,'DATE',7X,'TIME',4X,'TIME',4X,10(A2,'INFLOW  '))
 9045 FORMAT (//,7X,'DATE',7X,'TIME',4X,'TIME',4X,10(A2,'DEPTH   '))
 9050 FORMAT (//,7X,'DATE',7X,'TIME',4X,'TIME',4X,10(A2,'OUTFLOW '))
 9060 FORMAT (5X,'MO/DA/YR',1X,'HR:MIN:SEC','  STEP',4X,10(I9,1X))
 9061 FORMAT (5X,'MO/DA/YR',1X,'HR:MIN:SEC','  STEP',4X,10(1X,A9))
 9065 FORMAT (5X,'--------',1X,'----------','  ----',3X,
     +       10(A2,'--------'))
 9250 FORMAT (5X,I2,'/',I2,'/',I2,I3,2I4,I6,3X,10F10.3)
 9460 FORMAT (/,' AVERAGE FLOW....................',10F10.3)
 9470 FORMAT (' STANDARD DEVIATION OF FLOW......',10F10.3)
 9474 FORMAT (' MAXIMUM FLOW....................',10F10.3)
 9476 FORMAT (' MINIMUM FLOW....................',10F10.3)
 9480 FORMAT (' FLOW VOLUME (CUBIC FEET)........',10(1PE10.2))
 9490 FORMAT (' FLOW VOLUME (CUBIC METERS)......',10(1PE10.2))
 9560 FORMAT (/,' AVERAGE DEPTH...................',10F10.3)
 9570 FORMAT (' STANDARD DEVIATION OF DEPTH.....',10F10.3)
 9574 FORMAT (' MAXIMUM DEPTH...................',10F10.3)
 9576 FORMAT (' MINIMUM DEPTH...................',10F10.3)
C=======================================================================
      END

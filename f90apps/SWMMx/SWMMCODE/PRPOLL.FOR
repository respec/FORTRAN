      SUBROUTINE PRPOLL
C=======================================================================
C     PRPOLL created for SWMM release 4.0 by R.DICKINSON.
C     WCH, 8/6/93.  ADD MESSAGE ABOUT QUALITY CONTINUITY ERROR.
C     WCH, 9/3/93.  ADD I/I QUALITY TO CONTINUITY.
C     WCH, 11/15/93.  CHECK FOR NON-ZERO MINIMA WHEN ALL FLOW = 0.
C     WCH, 11/30/93.  MAKE TIME STEP AVERAGING CONSISTENT WITH OTHER
C                     BLOCKS FOR VOLUME/LOAD CALCULATIONS.
C     WCH, 4/7/94. ADD MESSAGE ABOUT BUILDUP TOTALS.  
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'NEW88.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'QUALTY.INC'
C#### WCH, 9/93
      INCLUDE 'RDII.INC'
      CHARACTER BMJ*3,UNTN(4)*8,EFMT*8,FOROUT(19)*35,
     +          CLOSE*1,UTOT(10)*8,LINOUT(2)*4,IDASH*4,KOCGUT*10
      DIMENSION QUAL(10,NW),FLOW(NW),FMAX(11),FMIN(11)
C#### WCH, 9/93
      DIMENSION PCENT(NRQ,14),PSUM(NRQ,5),PCRDIS(NRQ),PCRDIT(NRQ)
      DIMENSION TQUALC(NW),TQUALS(10),CMEAN(10),STNDEV(10),TQUALF(10)
C=======================================================================
      DATA LINOUT/' IN','OUT'/
      DATA EFMT/',1PE10.3'/,CLOSE/')'/
      DATA UNTN/'  POUNDS','TOT QUAN',' Q*C*DT ','KILOGRAM'/
      DATA BMJ/'   '/,IDASH/'----'/
C=======================================================================
C####      FOROUT(13) = '(''Flow wtd means.....'',F11.4'
C####      FOROUT(14) = '(''Flow wtd std devs..'',F11.4'
C#### WCH, 12/2/93. MAKE PRINTOUTS CONSISTENT AMONG SUBROUTINES.
C#### DWD (CSC) - Begin change.
C     Date: Tuesday, 10 May 1994.  Time: 12:43:23.
C     Modified hollerith value to correct fatal error generated
C     during compilation using Lahey 32-bit FORTRAN, version 5.01
C     Fatal - Context requires INTEGER Hollerith constant to be: 4Htext.
C      FOROUT(13) = 29H('Flow wt''d means....',F11.4
C      FOROUT(14) = 29H('Flow wt''d std devs.',F11.4
      FOROUT(13) = '(''Flow wtd means.....'',F11.4'
      FOROUT(14) = '(''Flow wtd std devs..'',F11.4'
C#### DWD (CSC) - End change.
C     Date: Tuesday, 10 May 1994.  Time: 12:43:23.
      FOROUT(15) = '(''Maximum value......'',F11.3'
      FOROUT(16) = '(''Minimum value......'',F11.3'
      FOROUT(17) = '(''Total loads........'',1PE11.3'
      FOROUT(18) = '(2X,I2,''/'',I2,''/'',I2,2X,I2,I5,F11.3'
      FOROUT(19) = '                      '
      DO 50 J    = 2,12
  50  FOROUT(J)  = '                        '
      NSCRT2     = NSCRAT(2)
C=======================================================================
C********** CHANGE THE UNITS OF ARRAY SUM FROM MG AND MG/SEC
C**********                  TO KG AND KG/SEC
C********** IF METRIC EQUALS ONE CHANGE THE UNITS TO LBS AND LBS/SEC
C********** IF NDIM(J) GT 0 THEN DO NOT DIVIDE BY 1.0E06
C=======================================================================
      DO 280 J = 1,NQS
      KDIM     = NDIM(J) + 1
      GWQ(J)   = GWQ(J)  * FACT3(KDIM)
      IF(KDIM.EQ.1)   GWQ(J)    = GWQ(J)    / 1.0E06
C#### WCH, 9/93
      IF(KDIM.EQ.1)   SUMRDII(J) = SUMRDII(J)/1.0E6
      IF(METRIC.EQ.1) THEN
                      GWQ(J)    = GWQ(J)    * 2.2046
                      REFF(1,J) = REFF(1,J) * 2.2046
C#### WCH, 9/93.
                      SUMRDII(J) = SUMRDII(J)*2.2046
                      ENDIF
 280  CONTINUE
      DO 400 K = 1,10
      DO 399 I = 1,NQS
      KDIM     = NDIM(I) + 1
      IF(KDIM.EQ.1) THEN
                    SUM(I,K)    = SUM(I,K)    / 1.0E06
                    IF(METRIC.EQ.1) THEN
                       SUM(I,K)     = SUM(I,K)     * 2.2046
                       PSHED(1,I,K) = PSHED(1,I,K) * 2.2046
                       PBASIN(I,K)  = PBASIN(I,K)  * 2.2046
                       ENDIF
                    ENDIF
C=======================================================================
C****** SUM(I,1)  STORES THE INITIAL BUILDUP FROM QSHED
C****** SUM(I,3)  STORES THE INITIAL CATCHBASIN LOADS FROM QSHED
C******           NOW THEY MUST BE ADDED TO SUM(I,2) AND SUM(I,4)
C=======================================================================
      IF(K.EQ.2) SUM(I,K) = SUM(I,K) + SUM(I,1)
      IF(K.EQ.4) SUM(I,K) = SUM(I,K) + SUM(I,3)
C=======================================================================
C********** CONVERT SUM(I,10) FROM UNITS OF QUANTITY * FLOW TO
C********** UNITS OF QUANTITY, I.E., KG, MPN, OR STANDARD UNITS
C=======================================================================
      IF(K.EQ.10) SUM(I,K)  = SUM(I,K)  * FACT3(KDIM) * CMET(8,METRIC)
 399  CONTINUE
 400  CONTINUE
C=======================================================================
C********* CALCULATE PSUM BASED ON ARRAY SUM FROM QSHED
C*********
C*********       PSUM(K,1) = TOTAL SURFACE AND CATCHBASIN BUILDUP
C*********       PSUM(K,2) = NET SURFACE BUILDUP
C*********       PSUM(K,3) = TOTAL WASHOFF
C*********       PSUM(K,4) = TOTAL SUBCATCHMENT LOAD
C*********       PSUM(K,5) = FLOW WEIGHTED INLET CONCENTRATION
C*********                   INLET LOAD / TOTAL PIPE/CHANNEL FLOW
C=======================================================================
      DO 500 K  = 1,NQS
      PSUM(K,1) = SUM(K,2) + SUM(K,4)
      PSUM(K,2) = SUM(K,2) - SUM(K,5)
      PSUM(K,3) = SUM(K,6) + SUM(K,7)
C#### WCH, 9/93
      PSUM(K,4) = SUM(K,8) + SUM(K,9) + PSUM(K,3) + GWQ(K) + SUMRDII(K)
      KDIM      = NDIM(K)  + 1
      IF ((CNT(5) * FACT3(KDIM)).NE.0.0) THEN
      PSUM(K,5) = SUM(K,10) / (CNT(5) * FACT3(KDIM))
      ELSE
      PSUM(K,5) = 0.0
      END IF
      IF(METRIC.EQ.2) PSUM(K,5) = PSUM(K,5) * 0.02831605
C=======================================================================
C     IF KDIM EQUALS ZERO RECONVERT THE CONCENTRATION BACK TO
C        MG/L FROM POUNDS OR KILOGRAMS.
C=======================================================================
      IF(KDIM.EQ.1) THEN
                                    PSUM(K,5) = PSUM(K,5) * 1.0E06
                    IF(METRIC.EQ.1) PSUM(K,5) = PSUM(K,5) / 2.2046
                    ENDIF
 500  CONTINUE
C=======================================================================
C******** CALCULATE PCENT BASED ON ARRAY SUM AND ARRAY PSUM
C********
C********     PCENT(K,1) = STREET SWEEPING / TOTAL SURFACE BUILDUP
C********     PCENT(K,2) = SURFACE WASHOFF / TOTAL SURFACE BUILDUP
C********     PCENT(K,3) = SURFACE WASHOFF / NET SURFACE BUILDUP
C********     PCENT(K,4) = SURFACE WASHOFF / TOTAL SUBCATCHMENT LOAD
C********     PCENT(K,5) = SURFACE WASHOFF / TOTAL LOAD TO INLETS
C********     PCENT(K,6) = CATCHBASIN WASHOFF / INSOLUBLE FRACTION
C********     PCENT(K,7) = CATCHBASIN WASHOFF / TOTAL LOAD TO INLETS
C********     PCENT(K,8) = INSOLUBLE FRACTION / TOTAL SUBCATCHMENT LOAD
C********     PCENT(K,9) = INSOLUBLE FRACTION / TOTAL LOAD TO INLETS
C********     PCENT(K,10)= PRECIPITATION / TOTAL SUBCATCHMENT LOAD
C********     PCENT(K,11)= PRECIPITATION / TOTAL LOAD TO INLETS
C********     PCENT(K,12)= GROUNDWATER LOAD / TOTAL SUBCATCHMENT LOAD
C********     PCENT(K,13)= GROUNDWATER LOAD / TOTAL LOAD TO INLETS
C********     PCENT(K,14)= PERCENT ERROR IN QUALITY CONTINUITY
C
C#### WCH, 9/93
C             PCRDIS(K)  = I/I LOAD / TOTAL SUBCATCHMENT LOAD
C             PCRDIT(K)  = I/I LOAD / TOTAL LOAD TO INLETS
C
C********                  ERROR--> INLET LOAD + MASS REM - SUBCAT LOAD
C********                           -----------------------------------
C********                                      SUBCAT LOAD
C=======================================================================
      DO 550 K   = 1,NQS
C#### WCH, 9/93
      PCRDIS(K)  = 0.0
      PCRDIT(K)  = 0.0
      DO 550 J   = 1,14
      PCENT(K,J) = 0.0
 550  CONTINUE
      DO 600 K   = 1,NQS
C=======================================================================
C********** TEST THE DENOMINATOR'S FOR ZERO VALUES
C=======================================================================
      IF(SUM(K,2).GT.0.0)  THEN
                           PCENT(K,1) = 100.0 *  SUM(K,5) / SUM(K,2)
                           PCENT(K,2) = 100.0 *  SUM(K,6) / SUM(K,2)
                           ENDIF
      IF(PSUM(K,2).GT.0.0) PCENT(K,3) = 100.0 *  SUM(K,6) / PSUM(K,2)
      IF(PSUM(K,4).GT.0.0) THEN
                           PCENT(K,4) = 100.0 *  SUM(K,6) / PSUM(K,4)
                           PCENT(K,10)= 100.0 *  SUM(K,9) / PSUM(K,4)
C#### WCH, 9/93
                           PCRDIS(K)  = 100.0 *  SUMRDII(K)/PSUM(K,4)
                           PCENT(K,8) = 100.0 *  SUM(K,8) / PSUM(K,4)
                           PCENT(K,6) = 100.0 *  SUM(K,7) / PSUM(K,4)
                           PCENT(K,12)= 100.0 *  GWQ(K)   / PSUM(K,4)
                           ENDIF
      IF(SUM(K,10).GT.0.0) THEN
                           PCENT(K,5) = 100.0 *  SUM(K,6) / SUM(K,10)
                           PCENT(K,7) = 100.0 *  SUM(K,7) / SUM(K,10)
                           PCENT(K,9) = 100.0 *  SUM(K,8) / SUM(K,10)
                           PCENT(K,11)= 100.0 *  SUM(K,9) / SUM(K,10)
C#### WCH, 9/93
                           PCRDIT(K)  = 100.0 *  SUMRDII(K)/SUM(K,10)
                           PCENT(K,13)= 100.0 *  GWQ(K)   / SUM(K,10)
                           PCENT(K,14)= 100.0 * (SUM(K,10) - PSUM(K,4)
     +                                        + REFF(1,K))/PSUM(K,4)
                           ENDIF
 600  CONTINUE
C=======================================================================
      WRITE(N6,650)
      WRITE(N6,660) (PNAME(J),J=1,NQS)
      WRITE(N6,660) (PUNIT(J),J=1,NQS)
      WRITE(N6,665) (IDASH,J=1,NQS)
      WRITE(N6,666)
      WRITE(N6,670) (SUM(K,1),K=1,NQS)
      WRITE(N6,680) (SUM(K,2),K=1,NQS)
      WRITE(N6,690) (SUM(K,3),K=1,NQS)
      WRITE(N6,700) (SUM(K,4),K=1,NQS)
      WRITE(N6,710) (PSUM(K,1),K=1,NQS)
      WRITE(N6,667)
      WRITE(N6,720) (PSHED(1,K,1),K=1,NQS)
      WRITE(N6,730) (PBASIN(K,1),K=1,NQS)
      WRITE(N6,740) (REFF(1,K),K=1,NQS)
      WRITE(N6,668)
      WRITE(N6,750) (SUM(K,5),K=1,NQS)
      WRITE(N6,760) (PSUM(K,2),K=1,NQS)
      WRITE(N6,770) (SUM(K,6),K=1,NQS)
      WRITE(N6,780) (SUM(K,7),K=1,NQS)
      WRITE(N6,790) (PSUM(K,3),K=1,NQS)
      WRITE(N6,800) (SUM(K,8),K=1,NQS)
      WRITE(N6,810) (SUM(K,9),K=1,NQS)
      WRITE(N6,815) (GWQ(K),  K=1,NQS)
C#### WCH, 9/93
      WRITE(N6,816) (SUMRDII(K),K=1,NQS)
      WRITE(N6,820) (PSUM(K,4),K=1,NQS)
      WRITE(N6,830) (SUM(K,10),K=1,NQS)
      WRITE(N6,835) (PSUM(K,5),K=1,NQS)
      WRITE(N6,669)
      WRITE(N6,840) (PCENT(K,1),K=1,NQS)
      WRITE(N6,850) (PCENT(K,2),K=1,NQS)
      WRITE(N6,860) (PCENT(K,3),K=1,NQS)
      WRITE(N6,870) (PCENT(K,4),K=1,NQS)
      WRITE(N6,880) (PCENT(K,5),K=1,NQS)
      WRITE(N6,890) (PCENT(K,6),K=1,NQS)
      WRITE(N6,900) (PCENT(K,7),K=1,NQS)
      WRITE(N6,910) (PCENT(K,8),K=1,NQS)
      WRITE(N6,920) (PCENT(K,9),K=1,NQS)
      WRITE(N6,930) (PCENT(K,10),K=1,NQS)
      WRITE(N6,940) (PCENT(K,11),K=1,NQS)
      WRITE(N6,945) (PCENT(K,12),K=1,NQS)
      WRITE(N6,946) (PCENT(K,13),K=1,NQS)
C#### WCH, 9/93
      WRITE(N6,947) (PCRDIS(K),K=1,NQS)
      WRITE(N6,948) (PCRDIT(K),K=1,NQS)
      WRITE(N6,950) (PCENT(K,14),K=1,NQS)
C#### WCH, 8/6/93.  PRINT WARNING ABOUT QUALITY CONTINUITY ERROR.
      WRITE(N6,955)
C=======================================================================
      IF(NPRNT.EQ.0) RETURN
C=======================================================================
C     PRINT OUT QUALITY INFORMATION
C
C     CALCULATE THE REQUIRED CONVERSION FACTORS.
C                                       FOR METRIC = 1:
C     CFACT1 CONVERTS MG/L * CU FT  TO  POUNDS.
C     CFACT2 CONVERTS QUANTITY/L * CU FT  TO  QUANTITY.
C                                      FOR METRIC = 2:
C     CFACT1 CONVERTS MG/L * CU M  TO  KG.
C     CFACT2 CONVERTS QUANTITY/L * CU M  TO  QUANTITY.
C=======================================================================
      CFACT1 = 28.31605 / 453592.
      CFACT2 = 28.31605
      IF(METRIC.EQ.2) THEN
                      CFACT1 = 1000.0 / 1000000.0
                      CFACT2 = 1000.0
                      ENDIF
C=======================================================================
C     READ THE HEADER INFORMATION ON THE SCRATCH FILE NPRNT TIMES.
C     NPRNT = THE NUMBER OF INLETS REQUESTED ON GROUP M1.
C=======================================================================
      DO 2000 J = 1,NPRNT
                  REWIND NSCRT2
      TIME      = 0.0
      FOROUT(1) = FOROUT(18)
      K         = IABS(IPRNT(J))
      LOCGUT    = NAMEG(K)
      KOCGUT    = KAMEG(K)
      TOTFLO    = 0.0
      TFLOSQ    = 0.0
      FMAX(1)   = 0.0
      FMIN(1)   = 1.0E30
      DO 1010 N = 1,NQS
      FMAX(N+1) = 0.0
      FMIN(N+1) = 1.0E30
      TQUALC(N) = 0.0
      TQUALS(N) = 0.0
      CMEAN(N)  = 0.0
      STNDEV(N) = 0.0
      TQUALF(N) = 0.0
 1010 CONTINUE
C=======================================================================
C     The variable LX specifies whether inflows or outflows are printed.
C=======================================================================
                        LX = 1
      IF(IPRNT(J).LT.0) LX = 2
C=======================================================================
C     WRITE THE TITLE CARD FOR QUANTITY AND QUALITY.
C     THERE ARE TWO POSSIBLE TITLE CARDS DEPENDING ON THE VALUE OF METRIC.
C=======================================================================
         IF(METRIC.EQ.1) THEN
                         IF(JCE.EQ.0) WRITE(N6,1025) LOCGUT,
     +                                LINOUT(LX),TITLE(1),TITLE(2)
                         IF(JCE.EQ.1) WRITE(N6,1026) KOCGUT,
     +                                     TITLE(1),TITLE(2)
                         WRITE(N6,1040) (PNAME(KK),KK=1,NQS)
                         WRITE(N6,1050) (PUNIT(KK),KK=1,NQS)
                         WRITE(N6,1055) (BMJ,KK=1,NQS)
                         ELSE
                         IF(JCE.EQ.0) WRITE(N6,1020) LOCGUT,
     +                                LINOUT(LX),TITLE(1),TITLE(2)
                         IF(JCE.EQ.1) WRITE(N6,1021) KOCGUT,
     +                                     TITLE(1),TITLE(2)
                         WRITE(N6,1040) (PNAME(KK),KK=1,NQS)
                         WRITE(N6,1060) (PUNIT(KK),KK=1,NQS)
                         WRITE(N6,1055) (BMJ,KK=1,NQS)
                         ENDIF
      DO 1500 I     = 1,NQS
      FOROUT(I+1)   = EFMT
 1500 CONTINUE
      FOROUT(NQS+2) = CLOSE
C=======================================================================
      DO 1535 K = 1,NQS
      IF(NDIM(K).EQ.0) THEN
                       UTOT(K)   = UNTN(1)
                       IF(METRIC.EQ.2) UTOT(K) = UNTN(4)
                       ENDIF
      IF(NDIM(K).EQ.1) UTOT(K) = UNTN(2)
      IF(NDIM(K).EQ.2) UTOT(K) = UNTN(3)
 1535 CONTINUE
C=======================================================================
C     KLINE --> IS A COUNTER THAT SUMS THE NUMBER OF LINES PRINTED ON A PAGE.
C=======================================================================
      KLINE     = 0
      INTCNT    = 0
      DLAST     = 0.0
C=======================================================================
C#### WCH, 11/30/93.  MAKE CONSISTENT SUMMATION WITH RUNOFF, COMBINE
C                     AND STATS.  IF PREVIOUS FLOW WAS ZERO, THEN USE
C                     NEW DELT FOR DMEAN.
C=======================================================================
      ITEST     = 0      
      DO 1800 M = 1, 1000000
      READ(NSCRT2,END=1810,ERR=1810) JULDAY,TIMDAY,
     +                               DELT,(FLOW(N),N=1,NPRNT)
      DMEAN     = 0.5 * (DELT + DLAST)
C#### WCH, 11/30/93
      IF(ITEST.EQ.0) DMEAN = DELT
      ITEST     = 0
      IF(FLOW(J).GT.0.0) ITEST = 1
C
      DLAST     = DELT
      TIME      = TIME     + DELT
      IF(KLINE.EQ.151) KLINE = 0
      INTCNT                 = INTCNT + 1
      IF(FLOW(J).GT.0.0.AND.INTCNT.EQ.INTERV) KLINE = KLINE + 1
C=======================================================================
C     Caution! from here on, flows are in cu m/sec if METRIC = 2.
C=======================================================================
      IF(METRIC.EQ.2) FLOW(J) = FLOW(J) * 0.028317
      CALL DATED
C=======================================================================
      READ(NSCRT2,END=1810,ERR=1810) ((QUAL(L,N),L=1,NQS),N=1,NPRNT)
      IF(FLOW(J).EQ.0.0) DLAST = 0.0
      IF(FLOW(J).GT.0.0) THEN
         IF(INTCNT.EQ.INTERV) WRITE(N6,FOROUT) 
     1     MONTH,NDAY,NYEAR,JHR,MINUTE,FLOW(J),(QUAL(N,J),N=1,NQS)
         TOTFLO   = TOTFLO + DMEAN*FLOW(J)
         TFLOSQ   = TFLOSQ + DMEAN*FLOW(J)**2
         IF(FLOW(J).GT.FMAX(1)) FMAX(1) = FLOW(J)
         IF(FLOW(J).LT.FMIN(1)) FMIN(1) = FLOW(J)
         DO 135 N   = 1,NQS
         TQUALF(N)  = TQUALF(N)  + DMEAN*QUAL(N,J)*FLOW(J)
         TQUALS(N)  = TQUALS(N)  + DMEAN*QUAL(N,J)*QUAL(N,J)*FLOW(J)
         IF(QUAL(N,J).GT.FMAX(N+1)) FMAX(N+1) = QUAL(N,J)
         IF(QUAL(N,J).LT.FMIN(N+1)) FMIN(N+1) = QUAL(N,J)
  135    CONTINUE
C=======================================================================
C     Write the title lines if 50 lines per page have been written.
C=======================================================================
         IF(KLINE.EQ.51.OR.KLINE.EQ.101.OR.KLINE.EQ.151) THEN
            IF(INTCNT.LT.INTERV) GO TO 1800
            IF(METRIC.EQ.1) THEN
                      IF(JCE.EQ.0) WRITE(N6,1025) LOCGUT,
     +                             LINOUT(LX),TITLE(1),TITLE(2)
                      IF(JCE.EQ.1) WRITE(N6,1026) KOCGUT,
     +                                  TITLE(1),TITLE(2)
                      WRITE(N6,1040) (PNAME(KK),KK=1,NQS)
                      WRITE(N6,1050) (PUNIT(KK),KK=1,NQS)
                      WRITE(N6,1055) (BMJ,KK=1,NQS)
                      ELSE
                      IF(JCE.EQ.0) WRITE(N6,1020) LOCGUT,
     +                             LINOUT(LX),TITLE(1),TITLE(2)
                      IF(JCE.EQ.1) WRITE(N6,1021) KOCGUT,
     +                                  TITLE(1),TITLE(2)
                      WRITE(N6,1040) (PNAME(KK),KK=1,NQS)
                      WRITE(N6,1060) (PUNIT(KK),KK=1,NQS)
                      WRITE(N6,1055) (BMJ,KK=1,NQS)
                      ENDIF
            ENDIF
         ENDIF
      IF(INTCNT.EQ.INTERV) INTCNT = 0
 1800 CONTINUE
C=======================================================================
C     Calculate the event statistics for SWMM.
C=======================================================================
 1810 DO 1850 K = 1,NQS
      IF (NDIM(K).EQ.0) TQUALC(K) = TQUALF(K) * CFACT1
      IF (NDIM(K).EQ.1) TQUALC(K) = TQUALF(K) * CFACT2
      IF (NDIM(K).EQ.2) TQUALC(K) = TQUALF(K)
 1850 CONTINUE
C=======================================================================
C     TOTFLO may equal zero. This really should not occur.
C=======================================================================
      TMEAN = TOTFLO / TIME
C#### WCH, 11/15/93
      FLODEV = 0.0
      IF(TOTFLO.GT.0.0) THEN
                        ARG    = TFLOSQ/TIME - TMEAN*TMEAN
                        IF(ARG.LT.1.0E-35) ARG = 0.0
                        IF(ARG.GT.0.0) FLODEV  = SQRT(ARG)
                        DO 1950 I = 1,NQS
                        CMEAN(I)  = TQUALF(I) / TOTFLO
                        STNDEV(I) = 0.0
                        ARG       = TQUALS(I)/TOTFLO-CMEAN(I)*CMEAN(I)
                        IF(ARG.LT.1.0E-35) ARG   = 0.0
                        IF(ARG.GT.0.0) STNDEV(I) = SQRT(ARG)
 1950                   CONTINUE
                        ENDIF
      FOROUT(1) = FOROUT(13)
      WRITE(N6,1055) (BMJ,KK=1,NQS)
      WRITE(N6,FOROUT) TMEAN,(CMEAN(I),I=1,NQS)
      FOROUT(1) = FOROUT(14)
      WRITE(N6,FOROUT) FLODEV,(STNDEV(I),I=1,NQS)
      FOROUT(1) = FOROUT(15)
      WRITE(N6,FOROUT) FMAX(1),(FMAX(I+1),I=1,NQS)
      FOROUT(1) = FOROUT(16)
C#### WCH, 11/15/93
      WRITE(N6,FOROUT) AMIN1(0.0,FMIN(1)),(AMIN1(0.0,FMIN(I+1)),I=1,NQS)
      FOROUT(1) = FOROUT(17)
      DO 1900 I   = 1,NQS
 1900 FOROUT(I+1) = EFMT
      WRITE(N6,FOROUT) TOTFLO,(TQUALC(I),I=1,NQS)
      IF(METRIC.EQ.1) WRITE(N6,1985) (UTOT(K),K=1,NQS)
      IF(METRIC.EQ.2) WRITE(N6,1990) (UTOT(K),K=1,NQS)
 2000 CONTINUE
C=======================================================================
 650  FORMAT(/,1H1,/,16X,
     1' #####################################################',/,16X,
     2' #             Runoff Quality Summary Page           #',/,16X,
     3' # If NDIM = 0 Loads are in units of Kgs---Lbs and   #',/,16X,
     4' #             mass rates have units of Kgs---Lbs/sec#',/,16X,
     5' # If NDIM = 1 Loads are in units of quantity        #',/,16X,
     6' #             and mass rates are quantity/sec       #',/,16X,
     7' # If NDIM = 2 loads are in units of concentration   #',/,16X,
     8' #             times volume and mass rates have units#',/,16X,
     9' #             of concentration times volume/second  #',/,16X,
     1' #####################################################',/)
 660  FORMAT(33X,10(2X,A8))
 665  FORMAT(33X,10(2X,A4,'----'))
 666  FORMAT(1X,'Inputs',/,
     .         1X,'------')
 667  FORMAT(/,1X,'Remaining Loads',/,
     .         1X,'---------------')
 668  FORMAT(/,1X,'Removals',/,
     .         1X,'--------')
 669  FORMAT(/,1X,'Percentages',
     .         /,1X,'-----------')
 670  FORMAT(1X,' 1. INITIAL SURFACE LOAD........',10(1PE10.2))
 680  FORMAT(1X,' 2. TOTAL SURFACE BUILDUP.......',10(1PE10.2))
 690  FORMAT(1X,' 3. INITIAL CATCHBASIN LOAD.....',10(1PE10.2))
 700  FORMAT(1X,' 4. TOTAL CATCHBASIN LOAD.......',10(1PE10.2))
 710  FORMAT(1X,' 5. TOTAL CATCHBASIN AND       ',/,
     .          '     SURFACE BUILDUP (2+4).......',10(1PE10.2))
 720  FORMAT(1X,' 6. LOAD REMAINING ON SURFACE...',10(1PE10.2))
 730  FORMAT(1X,' 7. REMAINING IN CATCHBASINS....',10(1PE10.2))
 740  FORMAT(1X,' 8. REMAINING IN CHANNEL/PIPES..',10(1PE10.2))
 750  FORMAT(1X,' 9. STREET SWEEPING REMOVAL.....',10(1PE10.2))
 760  FORMAT(1X,'10. NET SURFACE BUILDUP (2-9)...',10(1PE10.2))
 770  FORMAT(1X,'11. SURFACE WASHOFF.............',10(1PE10.2))
 780  FORMAT(1X,'12. CATCHBASIN WASHOFF..........',10(1PE10.2))
 790  FORMAT(1X,'13. TOTAL WASHOFF (11+12).......',10(1PE10.2))
 800  FORMAT(1X,'14. INSOLUBLE WASHOFF...........',10(1PE10.2))
 810  FORMAT(1X,'15. PRECIPITATION...............',10(1PE10.2))
 815  FORMAT(1X,'16. TOTAL GROUNDWATER LOAD......',10(1PE10.2))
C#### WCH, 9/93
 816  FORMAT(1X,'16a.TOTAL I/I LOAD..............',10(1PE10.2))
 820  FORMAT(1X,'17. TTL SUBC LD(13+14+15+16+16a)',10(1PE10.2))
 830  FORMAT(1X,'18. TOTAL LOAD TO INLETS........',10(1PE10.2))
 835  FORMAT(1X,'19. FLOW WT''D AVE.CONCENTRATION',
     .      /,1X, '    (INLET LOAD/TOTAL FLOW).....',10(1PE10.2))
 840  FORMAT(1X,'20. STREET SWEEPING (9/2).......',10(F10.3))
 850  FORMAT(1X,'21. SURFACE WASHOFF (11/2)......',10(F10.3))
 860  FORMAT(1X,'22. NET SURFACE WASHOFF(11/10)..',10(F10.3))
 870  FORMAT(1X,'23. WASHOFF/SUBCAT LOAD(11/17)..',10(F10.3))
 880  FORMAT(1X,'24. SURFACE WASHOFF/INLET LOAD',
     .       /,1X,'    (11/18).....................',10(F10.3))
  890 FORMAT(1X,'25. CATCHBASIN WASHOFF/         ',
     .       /,1X,'    SUBCATCHMENT LOAD (12/17)...',10(F10.3))
  900 FORMAT(1X,'26. CATCHBASIN WASHOFF/         ',
     .       /,1X,'    INLET LOAD (12/18)..........',10(F10.3))
  910 FORMAT(1X,'27. INSOLUBLE FRACTION/         ',
     .       /,1X,'    SUBCATCHMENT LOAD (14/17)...',10(F10.3))
  920 FORMAT(1X,'28. INSOLUBLE FRACTION/         ',
     .       /,1X,'    INLET LOAD (14/18)..........',10(F10.3))
  930 FORMAT(1X,'29. PRECIPITATION/              ',
     .       /,1X,'    SUBCATCHMENT LOAD (15/17)...',10(F10.3))
  940 FORMAT(1X,'30. PRECIPITATION/              ',
     .       /,1X,'    INLET LOAD (15/18)..........',10(F10.3))
  945 FORMAT(1X,'31. GROUNDWATER LOAD/           ',
     .       /,1X,'    SUBCATCHMENT LOAD (16/17)...',10(F10.3))
  946 FORMAT(1X,'32. GROUNDWATER LOAD/           ',
     .       /,1X,'    INLET LOAD (16/18)..........',10(F10.3))
  947 FORMAT(1X,'32a.INFILTRATION/INFLOW LOAD/   ',
     .       /,1X,'    SUBCATCHMENT LOAD (16a/17)..',10(F10.3))
  948 FORMAT(1X,'32b.INFILTRATION/INFLOW LOAD/   '
     .       /,1X,'    INLET LOAD (16a/18).........',10(F10.3))
  950 FORMAT(1X,'33. INLET LOAD SUMMATION ERROR',
     .       /,1X,'    (18+8-17)/17................',10(F10.3))
C#### WCH, 8/6/93.
C#### WCH, 4/7/94.
  955 FORMAT(/,' CAUTION. Due to method of quality routing (Users Manual
     1, Appendix IX)',/,' quality routing through channel/pipes is sensi
     2tive to the time step.',/,' Large "Inlet Load Summation Errors" ma
     3y result.',/,' These can be reduced by adjusting the time step(s).
     4',/,' Note: surface accumulation during dry time steps at end of s
     5imulation is',/,' not included in totals.  Buildup is only perform
     6ed at beginning of',/,' wet steps or for street cleaning.')
 1020 FORMAT(/,1H1,/,
     1' ****************************************************',/,
     1' *  Summary of Quantity and Quality Results at      *',/,
     1' *  Location ',I8,2X,A3,'Flow in cms.              *',/,
     2' *  Values are instantaneous at indicated time step *',/,
     1' ****************************************************',//,
     13X,A80,/,2X,A80,/)
 1021 FORMAT(/,1H1,/,
     1' ****************************************************',/,
     1' *  Summary of Quantity and Quality Results at      *',/,
     1' *  Location ',A11,3X,'Flow in cms.              *',/,
     2' *  Values are instantaneous at indicated time step *',/,
     1' ****************************************************',//,
     13X,A80,/,2X,A80,/)
 1025 FORMAT(/,1H1,/,
     1' ****************************************************',/,
     1' *  Summary of Quantity and Quality Results at      *',/,
     1' *  Location ',I8,2X,A3,'Flow in cfs.              *',/,
     2' *  Values are instantaneous at indicated time step *',/,
     1' ****************************************************',//,
     13X,A80,/,3X,A80,/)
 1026 FORMAT(/,1H1,/,
     1' ****************************************************',/,
     1' *  Summary of Quantity and Quality Results at      *',/,
     1' *  Location ',A10,3X,'Flow in cfs.              *',/,
     2' *  Values are instantaneous at indicated time step *',/,
     1' ****************************************************',//,
     13X,A80,/,3X,A80,/)
 1040 FORMAT(4X,'Date',5X,'Time',8X,'Flow ',10(2X,A8))
 1050 FORMAT(2X,'Mo/Da/Yr',2X,'Hr:Min',8X,'cfs  ',10(2X,A8))
 1055 FORMAT(2X,'--------',2X,'-------',4X,'-------',10(A2,'--------'))
 1060 FORMAT(2X,'Mo/Da/Yr',2X,'Hr:Min',8X,'cum/s',10(2X,A8))
 1985 FORMAT(23X,'Cub-Ft ',10(2X,A8))
 1990 FORMAT(23X,'Cub-Met',10(2X,A8))
C=======================================================================
      RETURN
      END

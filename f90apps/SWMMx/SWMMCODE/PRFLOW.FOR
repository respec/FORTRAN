      SUBROUTINE PRFLOW(ISKIP)
C=======================================================================
C     PRFLOW was updated for SWMM release 4.0 by R.DICKINSON.
C     The most recent update: JANUARY, 1990.
C     Correct subscript for FLOW and add IF-STMT for FLEW = 0, WCH (RED),
C       9/23/93.
C     Ensure zero std.dev. if zero flows, WCH, 11/15/93.
C     Make time step averaging consistent with other blocks for volume
C       calculations, WCH, 11/30/93.
C     Fix subscript and index for print-out of titles for depth 
C       print-outs, WCH, 4/22/94.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STIMER.INC'
      INCLUDE 'TIMER.INC'
      INCLUDE 'DETAIL.INC'
      INCLUDE 'SUBCAT.INC'
      INCLUDE 'GRWTR.INC'
      INCLUDE 'NEW88.INC'
      DIMENSION FLOW(NW),FMEAN(3,10),DEV(3,10),TOTFLW(3,10)
      DIMENSION FMAX(3,10),FMIN(3,10)
      DIMENSION STGA(NGW),TH1A(NGW),GWFLWA(NGW),TOTDEV(3,10)
      CHARACTER*3 BMJ
      DATA BMJ/'   '/
C=======================================================================
      NSCRT2 = NSCRAT(2)
      NSCRT5 = NSCRAT(5)
      NSCRT7 = NSCRAT(7)
      IF(ISKIP.EQ.1) GO TO 2900
      IF(NPRNT.EQ.0) GO TO 2900
C=======================================================================
C     Read the header information on the scratch file NPRNT times.
C              NPRNT = The number of inlets requested on group M1.
C=======================================================================
      DO 2000 J  = 1,NPRNT,9
      REWIND NSCRT2
      KOT        = J + 8
      TIME       = 0.0
      IF(KOT.GT.NPRNT) KOT = NPRNT
      IEND        = KOT - J + 1
      DO 2100 K   = 1,IEND
      FMAX(1,K)   = 0.0
      FMIN(1,K)   = 1.0E30
      DEV(1,K)    = 0.0
      TOTFLW(1,K) = 0.0
 2100 TOTDEV(1,K) = 0.0
      IF(METRIC.EQ.1) THEN
                      WRITE(N6,1030) TITLE(1),TITLE(2)
                      WRITE(N6,1040) (BMJ,K=J,KOT)
                      IF(JCE.EQ.0) WRITE(N6,1041)
     +                             (NAMEG(IABS(IPRNT(K))),K=J,KOT)
                      IF(JCE.EQ.1) WRITE(N6,1043)
     +                             (KAMEG(IPRNT(K)),K=J,KOT)
                      WRITE(N6,1045) (BMJ,K=J,KOT)
                      WRITE(N6,1050) (BMJ,K=J,KOT)
                      WRITE(N6,1055) (BMJ,K=J,KOT)
                      ELSE
                      WRITE(N6,1035) TITLE(1),TITLE(2)
                      WRITE(N6,1040) (BMJ,K=J,KOT)
                      IF(JCE.EQ.0) WRITE(N6,1041)
     +                             (NAMEG(IABS(IPRNT(K))),K=J,KOT)
                      IF(JCE.EQ.1) WRITE(N6,1043)
     +                             (KAMEG(IPRNT(K)),K=J,KOT)
                      WRITE(N6,1060) (BMJ,K=J,KOT)
                      WRITE(N6,1065) (BMJ,K=J,KOT)
                      ENDIF
      KLINE     = 0
      INTCNT    = 0
      DLAST     = 0.0
C=======================================================================
C#### WCH, 11/30/93.  MAKE CONSISTENT SUMMATION WITH RUNOFF, COMBINE
C                     AND STATS.  IF PREVIOUS FLOW WAS ZERO, THEN USE
C                     NEW DELT FOR DMEAN.
C=======================================================================
      ITEST = 0
      DO 2800 M = 1, 1000000
      READ(NSCRT2,END=2810) JULDAY,TIMDAY,DELT,(FLOW(N),N=1,NPRNT)
      TIME      = TIME + DELT
      DMEAN     = 0.5 * (DELT + DLAST)
C#### WCH, 11/30/93.
      IF(ITEST.EQ.0) DMEAN = DELT
      DLAST     = DELT
      IF(KLINE.EQ.151) KLINE = 0
      INTCNT                 = INTCNT + 1
      CALL DATED
C=======================================================================
C     Caution! from here on, flows are in cubic met/sec if METRIC = 2.
C=======================================================================
      KK        = 0
      FLEW      = 0.0
C#### WCH, 11/30/93.
      ITEST = 0
      DO 2850 K = J,KOT
      IF(METRIC.EQ.2) FLOW(K) = FLOW(K) * 0.028317
C#### WCH, 11/30/93
      IF(FLOW(K).GT.0.0) ITEST = 1
      KK = KK + 1
      IF(FLOW(K).LT.FMIN(1,KK)) FMIN(1,KK) = FLOW(K)
C#### WCH (RED), 9/93.  CHANGE SUBSCRIPT J TO K FOR FLOW().
C#### WCH, 11/30/93        IF(FLOW(K).EQ.0.0) DLAST = 0.0
      IF(FLOW(K).GT.0.0) THEN
                   FLEW = FLEW + FLOW(K)
                   IF(FLOW(K).GT.FMAX(1,KK)) FMAX(1,KK) = FLOW(K)
                   TOTFLW(1,KK) = TOTFLW(1,KK) + DMEAN*FLOW(K)
                   TOTDEV(1,KK) = TOTDEV(1,KK) + DMEAN*FLOW(K)**2
                   ENDIF
 2850 CONTINUE
C#### WCH (RED), 9/93.  ADD IF-STMT FOR FLEW = 0.
C#### WCH, 11/30/93      IF(FLEW.EQ.0.0) DLAST = 0.0
      IF(FLEW.GT.0.0.AND.INTCNT.EQ.INTERV) KLINE = KLINE + 1
      IF(INTCNT.EQ.INTERV.AND.FLEW.GT.0.0) WRITE(N6,1610)
     +             MONTH,NDAY,NYEAR,JHR,MINUTE,(FLOW(K),K=J,KOT)
C=======================================================================
C     Write the title lines if 50 lines per page have been written.
C=======================================================================
      IF(INTCNT.LT.INTERV) GO TO 2800
      IF(KLINE.EQ.51.OR.KLINE.EQ.101.OR.KLINE.EQ.151) THEN
                   IF(METRIC.EQ.1) THEN
                         WRITE(N6,1030) TITLE(1),TITLE(2)
                         WRITE(N6,1040) (BMJ,K=1,IEND)
                         IF(JCE.EQ.0) WRITE(N6,1041)
     +                             (NAMEG(IABS(IPRNT(K))),K=J,KOT)
                         IF(JCE.EQ.1) WRITE(N6,1043)
     +                             (KAMEG(IPRNT(K)),K=J,KOT)
                         WRITE(N6,1045) (BMJ,K=1,IEND)
                         WRITE(N6,1050) (BMJ,K=1,IEND)
                         WRITE(N6,1055) (BMJ,K=1,IEND)
                         ELSE
                         WRITE(N6,1035) TITLE(1),TITLE(2)
                         WRITE(N6,1040) (BMJ,K=1,IEND)
                         IF(JCE.EQ.0) WRITE(N6,1041)
     +                             (NAMEG(IABS(IPRNT(K))),K=J,KOT)
                         IF(JCE.EQ.1) WRITE(N6,1043)
     +                             (KAMEG(IPRNT(K)),K=J,KOT)
                         WRITE(N6,1045) (BMJ,K=1,IEND)
                         WRITE(N6,1060) (BMJ,K=1,IEND)
                         WRITE(N6,1065) (BMJ,K=1,IEND)
                         ENDIF
                   ENDIF
      IF(INTCNT.EQ.INTERV) INTCNT = 0
 2800 CONTINUE
C=======================================================================
C     Calculate the event statistics for SWMM.
C=======================================================================
 2810 DO 2825 K   = 1,IEND
      FMEAN (1,K) = TOTFLW(1,K) / TIME
C#### WCH, 11/15/93
      DEV(1,K) = 0.0
      IF(TOTFLW(1,K).GT.0.0) THEN
                        ARG  = TOTDEV(1,K)/TIME - FMEAN(1,K)**2
                        IF(ARG.GT.0.0) DEV(1,K) = SQRT(ARG)
                        ENDIF
 2825 CONTINUE
      WRITE(N6,1960) (FMEAN(1,K),K=1,IEND)
      WRITE(N6,1970) (DEV(1,K),K=1,IEND)
      WRITE(N6,1975) (FMAX(1,K),K=1,IEND)
      WRITE(N6,1976) (FMIN(1,K),K=1,IEND)
      WRITE(N6,1980) (TOTFLW(1,K),K=1,IEND)
      IF(METRIC.EQ.1) WRITE(N6,1985) (BMJ,K=1,IEND)
      IF(METRIC.EQ.2) WRITE(N6,1990) (BMJ,K=1,IEND)
 2000 CONTINUE
C=======================================================================
C     Print out subsurface water information.
C=======================================================================
 2900 IF(NGWGF.EQ.0) GO TO 3900
      DO 3000 IG = 1,NOGWSC,3
      KOT        = IG + 2
      IF(KOT.GT.NOGWSC) KOT = NOGWSC
      REWIND NSCRT5
      IEND        = KOT - IG + 1
      DO 3100 J   = 1,3
      DO 3100 K   = 1,IEND
      FMAX(J,K)   = 0.0
      FMIN(J,K)   = 1.0E30
      TOTFLW(J,K) = 0.0
 3100 TOTDEV(J,K) = 0.0
      IF(JCE.EQ.0) WRITE(N6,2530) (BMJ,NAMEW(J),NGWTOG(J),J=IG,KOT)
      IF(JCE.EQ.1) WRITE(N6,2531) (BMJ,KAMEW(J),KGWTOG(J),J=IG,KOT)
      WRITE(N6,2540) (BMJ,J=IG,KOT)
      IF(METRIC.EQ.1) WRITE(N6,2550) (BMJ,J=IG,KOT)
      IF(METRIC.EQ.2) WRITE(N6,2555) (BMJ,J=IG,KOT)
      WRITE(N6,2560) (BMJ,J=IG,KOT)
      NTCOUN     = 0
      JLINE      = 0
      DO 3500 MA = 1,100000
      READ(NSCRT5,END=3501) JULDAY,TIMDAY,DELT,(STGA(J8),
     +                       J8=1,NGWGF),(GWFLWA(J9),J9=1,NGWGF),
     +                       (TH1A(J7),J7=1,NGWGF)
      CALL DATED
      NTCOUN = NTCOUN + 1
      IF(NTCOUN.EQ.INTERV) JLINE = JLINE+1
      KK        = 0
      DO 3300 K = IG,KOT
      IF(METRIC.EQ.2) THEN
                      STGA(K)   = STGA(K)/3.281
                      GWFLWA(K) = GWFLWA(K)/35.3198
                      ENDIF
      KK = KK + 1
      TOTFLW(1,KK) = TOTFLW(1,KK) + DELT*STGA(K)
      TOTDEV(1,KK) = TOTDEV(1,KK) + DELT*STGA(K)**2.0
      IF(STGA(K).GT.FMAX(1,KK))   FMAX(1,KK) = STGA(K)
      IF(STGA(K).LT.FMIN(1,KK))   FMIN(1,KK) = STGA(K)
      IF(TH1A(K).GT.FMAX(2,KK))   FMAX(2,KK) = TH1A(K)
      IF(TH1A(K).LT.FMIN(2,KK))   FMIN(2,KK) = TH1A(K)
      IF(GWFLWA(K).GT.FMAX(3,KK)) FMAX(3,KK) = GWFLWA(K)
      IF(GWFLWA(K).LT.FMIN(3,KK)) FMIN(3,KK) = GWFLWA(K)
      TOTFLW(2,KK) = TOTFLW(2,KK) + DELT*TH1A(K)
      TOTDEV(2,KK) = TOTDEV(2,KK) + DELT*TH1A(K)**2
      TOTFLW(3,KK) = TOTFLW(3,KK) + DELT*GWFLWA(K)
 3300 TOTDEV(3,KK) = TOTDEV(3,KK) + DELT*GWFLWA(K)**2
      IF(JLINE.EQ.51) THEN
                      IF(JCE.EQ.0) WRITE(N6,2530)
     +                             (BMJ,NAMEW(J),NGWTOG(J),J=IG,KOT)
                      IF(JCE.EQ.1) WRITE(N6,2531)
     +                             (BMJ,KAMEW(J),KGWTOG(J),J=IG,KOT)
                      WRITE(N6,2540) (BMJ,J=IG,KOT)
                      IF(METRIC.EQ.1) WRITE(N6,2550) (BMJ,J=IG,KOT)
                      IF(METRIC.EQ.2) WRITE(N6,2555) (BMJ,J=IG,KOT)
                      WRITE(N6,2560) (BMJ,J=IG,KOT)
                      JLINE = 1
                      ENDIF
      IF(NTCOUN.EQ.INTERV)  WRITE(N6,2570) MONTH,NDAY,NYEAR,JHR,MINUTE,
     +                      (STGA(J),TH1A(J),GWFLWA(J),J=IG,KOT)
      IF(NTCOUN.EQ.INTERV)  NTCOUN = 0
 3500 CONTINUE
 3501 DO 3400 J  = 1,3
      DO 3400 K  = 1,IEND
      FMEAN(J,K) = TOTFLW(J,K)/TIME
      DEV(J,K)   = 0.0
      GARG       = TOTDEV(J,K)/TIME - FMEAN(J,K)**2.0
 3400 IF(GARG.GT.0.0) DEV(J,K) = SQRT(GARG)
                      WRITE(N6,2575) ((FMEAN(J,K),J=1,3),K=1,IEND)
                      WRITE(N6,2580) ((DEV(J,K),J=1,3),K=1,IEND)
                      WRITE(N6,2581) ((FMAX(J,K),J=1,3),K=1,IEND)
                      WRITE(N6,2582) ((FMIN(J,K),J=1,3),K=1,IEND)
                      WRITE(N6,2585) (TOTFLW(3,K),K=1,IEND)
      IF(METRIC.EQ.1) WRITE(N6,2586) (BMJ,J=1,IEND)
      IF(METRIC.EQ.2) WRITE(N6,2587) (BMJ,J=1,IEND)
 3000 CONTINUE
 3900 CONTINUE
C=======================================================================
C     Write the channel depth information.
C=======================================================================
      IF(MDEEP.GT.0) THEN
      DO 4000 J  = 1,MDEEP,9
      REWIND NSCRT7
      KOT        = J + 8
      TIME       = 0.0
C#### WCH, 4/22/94.  CHANGE NPRNT TO MDEEP.
C####      IF(KOT.GT.NPRNT) KOT = MDEEP
      IF(KOT.GT.MDEEP) KOT = MDEEP
      IEND        = KOT - J + 1
      DO 4100 K   = 1,IEND
      FMAX(1,K)   = 0.0
      FMIN(1,K)   = 1.0E30
      DEV(1,K)    = 0.0
      TOTFLW(1,K) = 0.0
 4100 TOTDEV(1,K) = 0.0
      IF(METRIC.EQ.1) THEN
                      WRITE(N6,4030) TITLE(1),TITLE(2)
                      WRITE(N6,4040) (BMJ,K=J,KOT)
                      IF(JCE.EQ.0) WRITE(N6,4041)
     +                             (NAMEG(IDEEP(K)),K=J,KOT)
                      IF(JCE.EQ.1) WRITE(N6,4043)
     +                             (KAMEG(IDEEP(K)),K=J,KOT)
                      WRITE(N6,4045) (BMJ,K=J,KOT)
                      WRITE(N6,4050) (BMJ,K=J,KOT)
                      WRITE(N6,4055) (BMJ,K=J,KOT)
                      ELSE
                      WRITE(N6,4035) TITLE(1),TITLE(2)
                      WRITE(N6,4040) (BMJ,K=J,KOT)
                      IF(JCE.EQ.0) WRITE(N6,4041)
     +                             (NAMEG(IDEEP(K)),K=J,KOT)
                      IF(JCE.EQ.1) WRITE(N6,4043)
     +                             (KAMEG(IDEEP(K)),K=J,KOT)
                      WRITE(N6,4060) (BMJ,K=J,KOT)
                      WRITE(N6,4065) (BMJ,K=J,KOT)
                      ENDIF
      KLINE     = 0
      INTCNT    = 0
      DO 4800 M = 1, 1000000
      READ(NSCRT7,END=4810) JULDAY,TIMDAY,DELT,(GDEPTH(N),N=1,MDEEP)
      TIME      = TIME + DELT
      IF(KLINE.EQ.151) KLINE = 0
      INTCNT                 = INTCNT + 1
      CALL DATED
      KK        = 0
      FLEW      = 0.0
      DO 4850 K = J,KOT
      JJ        = IDEEP(K)
      IF(NPG(JJ).EQ.2) THEN
                       THETA     = COS(GDEPTH(K))*GWIDTH(JJ)/2.0
                       GDEPTH(K) = GWIDTH(JJ)/2.0 - THETA
                       IF(GDEPTH(K).LT.0.0) THEN
                            THETA     = COS(3.1415927-GDEPTH(K)) *
     +                                               GWIDTH(JJ)/2.0
                            GDEPTH(K) = THETA + GWIDTH(JJ)/2.0
                            ENDIF
                       ENDIF
      GDEPTH(K) = GDEPTH(K)/CMET(1,METRIC)
      KK = KK + 1
      IF(GDEPTH(K).LT.FMIN(1,KK)) FMIN(1,KK) = GDEPTH(K)
      IF(GDEPTH(K).GT.0.0) THEN
                   FLEW = FLEW + GDEPTH(K)
                   IF(GDEPTH(K).GT.FMAX(1,KK)) FMAX(1,KK) = GDEPTH(K)
                   TOTFLW(1,KK) = TOTFLW(1,KK) + DELT*GDEPTH(K)
                   TOTDEV(1,KK) = TOTDEV(1,KK) + DELT*GDEPTH(K)**2
                   ENDIF
 4850 CONTINUE
      IF(INTCNT.EQ.INTERV) KLINE = KLINE + 1
      IF(INTCNT.EQ.INTERV) WRITE(N6,1610)
     +             MONTH,NDAY,NYEAR,JHR,MINUTE,(GDEPTH(K),K=J,KOT)
C=======================================================================
C     WRITE THE TITLE LINES IF 50 LINES PER PAGE HAVE BEEN WRITTEN.
C=======================================================================
      IF(INTCNT.LT.INTERV) GO TO 4800
      IF(KLINE.EQ.51.OR.KLINE.EQ.101.OR.KLINE.EQ.151) THEN
                   IF(METRIC.EQ.1) THEN
                         WRITE(N6,4030) TITLE(1),TITLE(2)
                         WRITE(N6,4040) (BMJ,K=1,IEND)
                         IF(JCE.EQ.0) WRITE(N6,4041)
C#### WCH, 4/22/94.  CHANGE SUBCRIPT IPRNT TO IDEEP IN THESE TWO LINES.
     +                             (NAMEG(IABS(IDEEP(K))),K=J,KOT)
                         IF(JCE.EQ.1) WRITE(N6,4043)
     +                             (KAMEG(IDEEP(K)),K=J,KOT)
                         WRITE(N6,4045) (BMJ,K=1,IEND)
                         WRITE(N6,4050) (BMJ,K=1,IEND)
                         WRITE(N6,4055) (BMJ,K=1,IEND)
                         ELSE
                         WRITE(N6,4035) TITLE(1),TITLE(2)
                         WRITE(N6,4040) (BMJ,K=1,IEND)
                         IF(JCE.EQ.0) WRITE(N6,4041)
     +                             (NAMEG(IABS(IPRNT(K))),K=J,KOT)
                         IF(JCE.EQ.1) WRITE(N6,4043)
     +                             (KAMEG(IPRNT(K)),K=J,KOT)
                         WRITE(N6,4045) (BMJ,K=1,IEND)
                         WRITE(N6,4060) (BMJ,K=1,IEND)
                         WRITE(N6,4065) (BMJ,K=1,IEND)
                         ENDIF
                   ENDIF
      IF(INTCNT.EQ.INTERV) INTCNT = 0
 4800 CONTINUE
C=======================================================================
C     Calculate the event statistics for SWMM.
C=======================================================================
 4810 DO 4825 K   = 1,IEND
      FMEAN (1,K) = TOTFLW(1,K) / TIME
C#### WCH, 11/15/93
      DEV(1,K) = 0.0
      IF(TOTFLW(1,K).GT.0.0) THEN
                        ARG  = TOTDEV(1,K)/TIME - FMEAN(1,K)**2.0
                        IF(ARG.GT.0.0) DEV(1,K) = SQRT(ARG)
                        ENDIF
 4825 CONTINUE
      WRITE(N6,1960) (FMEAN(1,K),K=1,IEND)
      WRITE(N6,1970) (DEV(1,K),K=1,IEND)
      WRITE(N6,1975) (FMAX(1,K),K=1,IEND)
      WRITE(N6,1976) (FMIN(1,K),K=1,IEND)
 4000 CONTINUE
C=======================================================================
C     ENDIF for IF-BLOCK for depth print-out.
C=======================================================================
      ENDIF
C=======================================================================
 1030 FORMAT(1H1,/,
     1' **********************************************',/,
     1' *  Summary of quantity results (flow in cfs) *',/,
     1' **********************************************',//,
     14X,A80,/,4X,A80,/)
 1035 FORMAT(1H1,/,
     1' **********************************************',/,
     1' *  Summary of quantity results (flow in cms) *',/,
     1' **********************************************',//,
     14X,A80,/,4X,A80,/)
 1040 FORMAT(/,19X,10(A3,'Chan/Inlt'))
 1041 FORMAT(19X,10(5X,I7))
 1043 FORMAT(20X,10(2X,A10))
 1045 FORMAT(4X,'Date',5X,'Time',3X,10(A3,2X,'  Flow '))
 1050 FORMAT(2X,'Mo/Da/Yr',2X,'Hr:Min ',10(A3,6X,'Cfs'))
 1055 FORMAT(2X,'--------',2X,'------ ',10(A3,5X,'----'))
 1060 FORMAT(2X,'Mo/Da/Yr',2X,'Hr:Min ',10(A3,'Cubic m/s'))
 1065 FORMAT(2X,'--------',2X,'------ ',10(A3,'---------'))
 1560 FORMAT(1X,I2,'/',I2,'/',I2,I5)
 1610 FORMAT(2X,I2,'/',I2,'/',I2,2X,I2,I4,1X,10F12.3)
 1960 FORMAT(/,'Flow wt''d means....',10(F12.4))
 1970 FORMAT(  'Flow wt''d std-devs.',10(F12.4))
 1975 FORMAT(  'Maximum value......',10(F12.4))
 1976 FORMAT(  'Minimum value......',10(F12.4))
 1980 FORMAT(  'Total loads........',10(1PE12.2))
 1985 FORMAT(19X,10(A3,' Cubic-ft'))
 1990 FORMAT(19X,10(A3,'  Cubic-m'))
 2530 FORMAT(1H1,/,
     1' *************************************************',/,
     1' *  Subsurface Summary for Stage, Soil Moisture, *',/,
     1' *                     and Groundwater Flow      *',/,
     1' *************************************************',/,
     2' *   Flow from Subcatchment # to Channel/Pipe #  *',/,
     1' *************************************************',//,
     3 21X,3(A3,'From #',I5,' to #',I5,7X))
 2531 FORMAT(1H1,/,
     1' *************************************************',/,
     1' *  Subsurface Summary for Stage, Soil Moisture, *',/,
     1' *                     and Groundwater Flow      *',/,
     1' *************************************************',/,
     2' *   Flow from Subcatchment # to Channel/Pipe #  *',/,
     1' *************************************************',//,
     3 21X,3(A3,'From #',A5,' to #',A10,2X))
 2540 FORMAT(/,19X,3(A3,'  Stage      Soil       Flow'))
 2550 FORMAT(2X,'Mo/Da/Yr   Hr:Min',
     .         2(A3,'   Feet  Moisture        cfs'))
 2555 FORMAT(2X,'Mo/Da/Yr   Hr:Min',
     .         3(A3,'   Feet  Moisture      cum/s'))
 2560 FORMAT(2X,'--------   ------',
     +         3(A3,'   ----  --------      -----'))
 2570 FORMAT(2X,2(I2,'/'),I2,3X,I2,I4,3(2F10.3,F11.3))
 2575 FORMAT(/,'Flow wt''d means....',3(2F10.3,F11.3))
 2580 FORMAT  ('Flow wt''d std-devs.',3(2F10.3,F11.3))
 2581 FORMAT  ('Maximum value......',3(2F10.3,F11.3))
 2582 FORMAT  ('Mininum value......',3(2F10.3,F11.3))
 2585 FORMAT  ('Total loads........',3(20X,1PE11.2))
 2586 FORMAT(19X,3(A3,16X,'  Cubic-feet'))
 2587 FORMAT(19X,3(A3,16X,'Cubic-meters'))
 4030 FORMAT(1H1,/,
     1' ################################################',/,
     1' #  Summary of quantity results (Depth in feet) #',/,
     1' ################################################',//,
     14X,A80,/,4X,A80,/)
 4035 FORMAT(1H1,/,
     1' ##################################################',/,
     1' #  Summary of quantity results (Depth in meters) #',/,
     1' ##################################################',//,
     14X,A80,/,4X,A80,/)
 4040 FORMAT(/,19X,10(A3,' Channel '))
 4041 FORMAT(19X,10(5X,I7))
 4043 FORMAT(20X,10(2X,A10))
 4045 FORMAT(4X,'Date',5X,'Time',3X,10(A3,2X,' Depth '))
 4050 FORMAT(2X,'Mo/Da/Yr',2X,'HR:MIN ',10(A3,5X,'Feet'))
 4055 FORMAT(2X,'--------',2X,'------ ',10(A3,5X,'----'))
 4060 FORMAT(2X,'Mo/Da/Yr',2X,'Hr:Min ',10(A3,'  Meters '))
 4065 FORMAT(2X,'--------',2X,'------ ',10(A3,'---------'))
C=======================================================================
      RETURN
      END

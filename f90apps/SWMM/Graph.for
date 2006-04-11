      SUBROUTINE GRAPH
C     GRAPH BLOCK
C=======================================================================
C     THIS ROUTINE PREPARES SIMULATED AND MEASURED HYDROGRAPHS,
C     POLLUTOGRAPHS  AND LOADOGRAPHS FOR PLOTTING BY SUBROUTINE CURVE.
C     SIMULATED AND/OR MEASURED DATA MAY BE PLOTTED.  IT IS NO LONGER
C     POSSIBLE TO PLOT MORE THAN ONE LOCATION ON ONE GRAPH.
C     THIS ROUTINE LAST UPDATED BY W. HUBER, AUGUST 1993.
C     SCRATCH FILE 1 NOT USED.  DON'T OPEN.  WCH, 4/18/94.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'LAB.INC'
C=======================================================================
C#### WCH (CDM), 8/93. REMOVE NEWFIL*60
      CHARACTER KPRED(20)*10,KMEAS(20)*10,JMEAS(20)*10
      CHARACTER PNDUM*8,PUDUM*8,VER1*10,VER2*10,VER3*10,VER4*10,
     +          PRIN(3)*14,FIRMAT*80,DEFMT*16,HORIZG(2)*30,
     +          VERM(4)*10,USE*4,PPNAME*8,PPUNIT*8,ZX(2)*20,BMJ*4
      DIMENSION X(201,2),Y(201,2),NPT(2),JPLOT(20),
     1          YT(201,6),ITAB(20),IPLOT(20),MTAB(20),KPLOT(20),
     2          TIMX(16),YVAL(16),QQT(200),PPT(10,200),IPOLX(11),
     3          NNDIM(MQUAL),PPNAME(MQUAL),PPUNIT(MQUAL),NLOAD(11)
      LOGICAL NOTEND
C=======================================================================
      DATA PRIN/'HYDROGRAPH   ','POLLUTOGRAPH ','LOADOGRAPH '/
      DATA HORIZG/'      TIME OF DAY IN HOURS    ',
     +            ' PREDICTED = *    MEASURED = +'/
      DATA VER1/'   FLOW   '/,VER2/'    IN    '/,
     +     VER3/'    CFS   '/,VER4/' CU M/SEC '/
      DATA VERM/'  POUNDS  ','   PER   ','  SECOND  ','MILLIGRAMS'/
      DATA DEFMT/'(2X,F8.0,7F10.0)'/,BMJ/'    '/
      DATA ZX/'  PLOT CONCENTRATION','  PLOT LOADOGRAPH'/
C=======================================================================
C#### WCH, 8/5/93.  ADD NO-QUOTE OPTION.
C=======================================================================
      IF(NOQ.EQ.0) THEN
                   WRITE(*,10)
                   WRITE(N6,10)
                   ELSE
                   WRITE(*,12)
                   WRITE(N6,12)
                   ENDIF
C#######################################################################
C WCH, 8/5/93. INCREMENT THE JIN/JOUT COUNTERS.
C=======================================================================
      INCNT  = INCNT + 1
      IOUTCT = IOUTCT + 1
C=======================================================================
C     Open all input/output files for the Graph Block.
C=======================================================================
      IF(JIN(INCNT).GT.0.AND.(FFNAME(INCNT).EQ.'JOT.UF'.OR.
     +      FFNAME(INCNT).EQ.'JIN.UF'))
     +      OPEN(JIN(INCNT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JIN(INCNT).GT.0.AND.FFNAME(INCNT).NE.'JOT.UF'.AND.
     +      FFNAME(INCNT).NE.'JIN.UF')
     +      OPEN(JIN(INCNT),FILE=FFNAME(INCNT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
C=======================================================================
C     DON'T NEED JOUT BUT OPEN OUT OF HABIT IF JOUT > 0.
C=======================================================================
      IF(JOUT(IOUTCT).GT.0.AND.(FFNAME(25+IOUTCT).EQ.'JOT.UF'.OR.
     +      FFNAME(25+IOUTCT).EQ.'JIN.UF'))
     +      OPEN(JOUT(IOUTCT),FORM='UNFORMATTED',STATUS='SCRATCH')
      IF(JOUT(IOUTCT).GT.0.AND.FFNAME(25+IOUTCT).NE.'JOT.UF'.AND.
     +      FFNAME(25+IOUTCT).NE.'JIN.UF')
     +      OPEN(JOUT(IOUTCT),FILE=FFNAME(25+IOUTCT),FORM='UNFORMATTED',
     +      STATUS='UNKNOWN')
C=======================================================================
C=======================================================================
C#### WCH, 4/18/94.  DON'T OPEN THIS IF DON'T USE IT.
C#### (DON'T) OPEN FORMATTED SCRATCH FILE
C##=====================================================================
C##      IF(NSCRAT(1).GT.0.AND.FFNAME(51).NE.'SCRT1.UF') OPEN(NSCRAT(1),
C##     +             FILE=FFNAME(51),FORM='FORMATTED',STATUS='UNKNOWN')
C##      IF(NSCRAT(1).GT.0.AND.FFNAME(51).EQ.'SCRT1.UF') OPEN(NSCRAT(1),
C##     +             FORM='FORMATTED',STATUS='SCRATCH')
C##                    NOUT = NSCRAT(1)
C##      IF(NOUT.GT.0) REWIND NOUT
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP A1 <<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,NTAPE,NPLOT,MEAS,
     +                       MFILE,MPLOT,NQP,METRIC,MCTH
      NLP    = NQP+1
      IF(MFILE.LE.0) MFILE = N5
      WRITE(N6,30) NTAPE,METRIC,NQP,NPLOT,MEAS,MFILE,MPLOT,MCTH
      METRIC = METRIC + 1
      IF(METRIC.EQ.1) THEN
                      CFACT1 = 28.31605/453592.0
                      CFACT2 = 28.31605
                      ELSE
                      CFACT1 = 1000.0 / 1.0E06
                      CFACT2 = 1000.0
                      ENDIF
      IF(NPLOT.EQ.0.AND.MEAS.EQ.0) RETURN
                    NUNIT = N5
      IF(MEAS.EQ.2) NUNIT = MFILE
      IF(NPLOT.LT.1.OR.NTAPE.LT.1) GO TO 4000
C=======================================================================
C     INTERFACING MECHANISM FOR QUANTITY AND QUALITY OF RUNOFF
C=======================================================================
C#### WCH, 8/5/93.  ADD ERROR MESSAGE.
      IF(NTAPE.NE.JIN(INCNT)) THEN
         WRITE(N6,9500) NTAPE, JIN(INCNT), INCNT
         WRITE(*,9500)  NTAPE, JIN(INCNT), INCNT
         STOP
         ENDIF
      CALL INFACE(1,NTAPE)
      IF(NQUAL.GT.0) THEN
                     DO 110 K  = 1,NQUAL
                     PPNAME(K) = PNAME(K)
                     PPUNIT(K) = PUNIT(K)
                     NNDIM(K)  = NDIM(K)
  110                CONTINUE
                     ENDIF
C=======================================================================
C     INPUT INFORMATION FOR QUALITY PARAMETERS.
C=======================================================================
 4000 IF(NQP.LE.0) GO TO 4200
      WRITE (N6,4090)
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP B1 <<<<<<<<<<<<
C=======================================================================
      DO 4140 K = 1,NQP
      READ(N5,*,ERR=888) CC,KPOL,NLOAD(K+1),PNDUM,PUDUM,NDUM
C=======================================================================
C     HERE, USER INPUT DEFINES POLLUTANTS.
C=======================================================================
      K1        = K + 1
      IF(KPOL.EQ.0.OR.NPLOT.EQ.0) THEN
                                  IPOLX(K1) = K1
                                  NDIM(K1)  = NDUM
                                  PNAME(K1) = PNDUM
                                  PUNIT(K1) = PUDUM
                                  ELSE
                                  IPOLX(K1) = KPOL
                                  NDIM(K1)  = NNDIM(KPOL)
                                  PNAME(K1) = PPNAME(KPOL)
                                  PUNIT(K1) = PPUNIT(KPOL)
                                  ENDIF
      KK1 = NLOAD(K1) + 1
      WRITE(N6,4150) K,PNAME(K1),PUNIT(K1),NDIM(K1),KPOL,ZX(KK1)
 4140 CONTINUE
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP C1 <<<<<<<<<<<<
C=======================================================================
 4200 IF(NPLOT.GT.0) THEN
                     IF(JCE.EQ.0) THEN  
                         READ(N5,*,ERR=888) CC,(IPLOT(N),N=1,NPLOT)
                         WRITE(N6,40) NPLOT,(IPLOT(N),N=1,NPLOT)
                         ELSE
                         READ(N5,*,ERR=888) CC,(KPRED(N),N=1,NPLOT)
                         WRITE(N6,41) NPLOT,(KPRED(N),N=1,NPLOT)
                         ENDIF
                     ENDIF
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP C2 <<<<<<<<<<<<
C=======================================================================
      IF(MPLOT.GT.0) THEN
                     IF(JCE.EQ.0) THEN
                        READ(N5,*,ERR=888) CC,(KPLOT(N),N=1,MPLOT)
                        WRITE(N6,55) MPLOT,(KPLOT(N),N=1,MPLOT)
                        ELSE
                        READ(N5,*,ERR=888) CC,(KMEAS(N),N=1,MPLOT)
                        WRITE(N6,56) MPLOT,(KMEAS(N),N=1,MPLOT)
                        ENDIF
                     ENDIF
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP D1 <<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,TITLE(1)
      HTITLE(1) = TITLE(1)
      HTITLE(2) = BMJ
      DO 100 J  = 1,20
  100 MTAB(J)   = 0
      HORIZ(1)  = HORIZG(1)
      HORIZ(2)  = HORIZG(2)
      NNN       = 0
C=======================================================================
C     ITAB = SUBSCRIPT OF LOCATION OF MEASURED GRAPH WHEN MEASURED
C            LOCATION MATCHES DESIRED PLOT LOCATION.
C=======================================================================
      IF(MPLOT.GT.0) THEN
                     DO 180 M  = 1,MPLOT
                     NNN       = NNN + 1
                     DO 160 J  = 1,LOCATS
                     IF(JCE.EQ.0.AND.KPLOT(M).EQ.NLOC(J)) GO TO 170
                     IF(JCE.EQ.1.AND.KMEAS(M).EQ.KAN(J))  GO TO 170
  160                CONTINUE
                     ITAB(M) = 0
                     MTAB(M) = M
                     GO TO 180
  170                ITAB(M) = J
                     MTAB(M) = M
  180                CONTINUE
                     ENDIF
C=======================================================================
C     CHECK FOR MATCHING LOCATIONS TO BE PLOTTED FROM SWMM FILE.
C     MTAB EQ 0, NO MATCH BETWEEN IPLOT AND KPLOT.
C     MTAB NE 0, MTAB = SUBSCRIPT OF IPLOT.
C=======================================================================
      IF(NPLOT.GT.0) THEN
                     DO 210 J = 1,NPLOT
                     IF(MPLOT.EQ.0) GO TO 201
                     DO 200 M = 1,MPLOT
                     IF(JCE.EQ.0) JPLOT(M) = KPLOT(M)
                     IF(JCE.EQ.1) JMEAS(M) = KMEAS(M)
                     IF(JCE.EQ.0.AND.IPLOT(J).EQ.KPLOT(M)) GO TO 205
                     IF(JCE.EQ.1.AND.KPRED(J).EQ.KMEAS(M)) GO TO 205
  200                CONTINUE
  201                DO 206 N  = 1,LOCATS
                     IF(JCE.EQ.0.AND.IPLOT(J).EQ.NLOC(N)) GO TO 203
                     IF(JCE.EQ.1.AND.KPRED(J).EQ.KAN(N))  GO TO 203
  206                CONTINUE
                     IF(JCE.EQ.0) WRITE(N6,9020) IPLOT(J)
                     IF(JCE.EQ.1) WRITE(N6,9021) KPRED(J)
                     STOP
  203                NNN        = NNN + 1
                     IF(JCE.EQ.0) JPLOT(NNN) = IPLOT(J)
                     IF(JCE.EQ.1) JMEAS(NNN) = KPRED(J)
                     ITAB(NNN)  = N
                     MTAB(NNN)  = 0
                     GO TO 210
  205                MTAB(M)   = M
  210                CONTINUE
                     END IF
C=======================================================================
C     FROM PREDICTED OUTPUT, STORE EVERY MCTH DATA POINT
C     PROGRAM THEN INTERPOLATES TO PLOT 100 POINTS AT MOST.
C     FIRST PLOTTED POINT IS ALWAYS ZERO AT TZERO.
C=======================================================================
      IF(MCTH.LE.0) MCTH = 1
      WRITE(*,34)   NNN
      DO 800 MT = 1,NNN
      IBACK     = 0
      NOTEND    = .FALSE.
      WRITE(*,35) MT
      IF(NPLOT.EQ.0)                    GO TO 7000
      IF(MT.LE.MPLOT.AND.ITAB(MT).EQ.0) GO TO 7000
C=======================================================================
C     READ INTERFACE FILE HEADERS
C=======================================================================
      CALL INFACE(0,NTAPE)
      TIME     = TZERO
      N        = 0
 5000 DO 250 K = 1,100000,MCTH
      N        = N+1
      DO 230 J = 1,MCTH
C=======================================================================
C     READ TIME STEP VALUES FROM INTERFACE FILE.
C     STORE IN TEMPORARY FILE AND PULL OUT DESIRED VALUES.
C=======================================================================
      IF(NQUAL.EQ.0) THEN
               READ(NTAPE,END=251) JDAY,TMDAY,DELTA,(QQT(I),I=1,LOCATS)
               ELSE
               READ(NTAPE,END=251) JDAY,TMDAY,DELTA,(QQT(I),
     +                             (PPT(L,I),L=1,NQUAL),I=1,LOCATS)
              END IF
      TIME   = TIME + DELTA
  230 CONTINUE
      X(N,1) = TIME/3600.0
      ITB      = ITAB(MT)
      DO 240 J = 1,NLP
      IF(J.EQ.1) THEN
                 YT(N,J) = QQT(ITB)
                 ELSE
                 KP = IPOLX(J)
                 IF(KP.GT.0) THEN
                             YT(N,J) = PPT(KP,ITB)
                             ELSE
                             YT(N,J) = 0.0
                             ENDIF
                 ENDIF
  240 CONTINUE
      IF(N.GE.201) THEN
                   IBACK = IBACK + 1
                   GO TO 252
                   ENDIF
  250 CONTINUE
  251 NOTEND = .FALSE.
      N      = N - 1
      IBACK  = IBACK + 1
      IF(N.LE.0) GO TO 800
      GO TO 253
  252 NOTEND = .TRUE.
  253 NPT(1) = N
C=======================================================================
C     OUTER LOOP ON HYDROGRAPH PLUS NUMBER OF POLLUTANTS.
C     ONLY READ MEASURED DATA FOR FIRST 200 PREDICTED DATA POINTS.
C=======================================================================
 7000 DO 820 J  = 1,NLP
      IF(IBACK.GT.1) GO TO 6000
             IF(J.EQ.1) THEN
                        VERT1 = VER1
                        VERT2 = VER2
                        IF(METRIC.EQ.1) VERT3 = VER3
                        IF(METRIC.EQ.2) VERT3 = VER4
                        ELSE IF(NLOAD(J).EQ.0) THEN
                             JJ        = IPOLX(J)
                             HTITLE(1) = TITLE(1)
                             VERT1     = PNAME(JJ)
                             VERT2     = VER2
                             VERT3     = PUNIT(JJ)
                             ELSE
                             JJ        = IPOLX(J)
                             HTITLE(1) = PNAME(JJ)
                             VERT2     = VERM(2)
                             VERT3     = VERM(3)
                             IF(METRIC.EQ.1) VERT1 = VERM(1)
                             IF(METRIC.EQ.2) VERT1 = VERM(4)
                             ENDIF
             IF(NPLOT.GT.0) THEN
                            IF(NLOAD(J).EQ.1) GO TO 6000
                            IF(MTAB(MT).EQ.0) GO TO 6000
                            IF(MPLOT.EQ.0)    GO TO 6000
                            ENDIF
             KK = 0
C=======================================================================
C     LOOP ON NUMBER OF MEASURED LOCATIONS TO BE PLOTTED.
C=======================================================================
C     READ PARAMETERS FOR MEASURED INPUT FOR EACH LOCATION FOR EACH
C        MEASURED HYDROGRAPH AND POLLUTOGRAPH.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP E1 <<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,MDATA,LCARD,MTIME,
     +                   MUNIT,TMZERO,TQUIT,DTMHR
      WRITE(N6,310) PRIN(1),KPLOT(MT),
     +              MDATA,LCARD,MTIME,MUNIT,TMZERO,TQUIT,DTMHR
      IF(MDATA.EQ.0) GO TO 6000
C=======================================================================
C     READ IN VARIABLE FORMAT FOR EACH MEASURED DATA INPUT.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP E2 <<<<<<<<<<<<
C=======================================================================
      READ(N5,*,ERR=888) CC,FIRMAT
      IF(FIRMAT.EQ.' ')     FIRMAT = DEFMT
      WRITE(N6,360) FIRMAT
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP E3 <<<<<<<<<<<<
C=======================================================================
C                    MUNIT = 0  TIME IS IN MINUTES.
C                    MUNIT = 1  TIME IS IN HOURS.MINUTES.
C                    MUNIT = 2  TIME IS IN HOURS.
C=======================================================================
      IF(MTIME.EQ.0) THEN
                     KSTOP = 0
                     KK    = 0
  370                READ(NUNIT,FIRMAT) (TIMX(LL),YVAL(LL),LL=1,LCARD)
                     DO 380 LL = 1,LCARD
                     IF(TIMX(LL).GE.TQUIT) THEN
                                           KKX   = LL-1
                                           KSTOP = 1
                                           GO TO 390
                                           END IF
  380                CONTINUE
                     KKX = LCARD
  390                IF(KKX.LE.0) GO TO 500
                     DO 490 LL = 1,KKX
                     LX        = KK+LL
                     IF(MUNIT-1.LT.0) THEN
                                X(LX,2) = TIMX(LL)/60.+TMZERO
                                ELSE
                                IF(MUNIT-1.EQ.0) THEN
                                   TIM     = IFIX(TIMX(LL))
                                   X(LX,2) = TIM+(TIMX(LL)-TIM)/0.6+
     +                                            TMZERO
                                   ELSE
                                   X(LX,2)  = TIMX(LL)+TMZERO
                                   ENDIF
                                ENDIF
  490                Y(LX,2) = YVAL(LL)
  500                KK      = KK+KKX
                     IF(KK.LT.201.AND.KSTOP.EQ.0) GO TO 370
                     IF(KK.GT.201) KK = 201
                     NPT(2) = KK
                     WRITE(N6,505) KK
                     ENDIF
C=======================================================================
C     HERE, COMPUTE TIME USING CONSTANT TIME INCREMENT, DTMHR, IN HOURS.
C=======================================================================
C>>>>>>>>>>>> READ DATA GROUP E3 <<<<<<<<<<<<
C=======================================================================
      IF(MTIME.GT.0)      THEN
         IF(MTIME.GT.201) THEN
                          WRITE(N6,520) MTIME
                          MTIME = 201
                          ENDIF
         IF(MTIME.GT.0)   THEN
                          READ(N5,FIRMAT) (Y(LL,2),LL=1,MTIME)
                          DO 550 LX  = 1,MTIME
  550                     X(LX,2)    = TMZERO+FLOAT(LX-1)*DTMHR
                          NPT(2)     = MTIME
                          ENDIF
                          ENDIF
C=======================================================================
C     CONVERT PREDICTED FLOWS TO APPROPRIATE UNITS
C=======================================================================
                     NN = NPT(1)
      IF(NPLOT.EQ.0) NN = NPT(2)
      DO 630 N = 2,NN
      IF(J.EQ.1) THEN
                 Y(N,1) = YT(N,J)/CMET(1,METRIC)**3.0
                 GO TO 630
                 ENDIF
C=======================================================================
C     CONVERT PREDICTED AND MEASURED
C             POLLUTOGRAPHS TO CONCENTRATION FROM CFS*CONC.
C=======================================================================
      IF(NLOAD(J).EQ.0) THEN
                  IF(YT(N,1).GT.0.0) Y(N,1) = YT(N,J)/YT(N,1)
                  ELSE
                  IF(NDIM(J).EQ.0) THEN
                                   Y(N,1) = YT(N,J)*CFACT1
                                   Y(N,2) =  Y(N,2)*CFACT1
                                   ELSE
                                   Y(N,1) = YT(N,J)*CFACT2
                                   Y(N,2) =  Y(N,2)*CFACT2
                                   ENDIF
                  ENDIF
  630 CONTINUE
      CALL CURVE(X,Y,NPT,2,KPLOT(MT),KMEAS(MT))
      IF(J.EQ.1) THEN
                 USE = 'FLOW'
                 ELSE
                 IF(NLOAD(J).EQ.0) USE = 'CONC'
                 IF(NLOAD(J).EQ.1) USE = 'LOAD'
                 ENDIF
      KP = IPOLX(J)
      IF(KP.EQ.0) KP = 1
      CALL HYSTAT(X,Y,NPT,2,KPLOT(MT),KMEAS(MT),TRIBA,METRIC,
     +                                USE,PUNIT(KP),PNAME(KP))
C=======================================================================
C     PLOT PREDICTED GRAPHS ONLY.
C     CONVERT PREDICTED POLLUTOGRAPHS TO CONCENTRATION FROM CFS*CONC.
C=======================================================================
      IF(ITAB(MT).EQ.0) GO TO 820
 6000 IF(MTAB(MT).EQ.0.OR.(J.GT.1.AND.NLOAD(J).GT.0)) THEN
                        IF(J.EQ.1) THEN
                                   USE = 'FLOW'
                                   ELSE
                                   IF(NLOAD(J).EQ.0) USE = 'CONC'
                                   IF(NLOAD(J).EQ.1) USE = 'LOAD'
                                   ENDIF
                        NN       = NPT(1)
                        SUMFLW   = 0.0
                        DO 930 N = 1,NN
                        IF(J.EQ.1) THEN
                                   Y(N,1) = YT(N,J)/CMET(1,METRIC)**3.0
                                   SUMFLW = SUMFLW + Y(N,1)
                                   GO TO 930
                                   ENDIF
                        IF(NLOAD(J).EQ.0) THEN
                           IF(YT(N,1).GT.0.0) Y(N,1) = YT(N,J)/YT(N,1)
                           ELSE
                           IF(NDIM(J).EQ.0) THEN
                                      Y(N,1) = YT(N,J)*CFACT1
                                      Y(N,2) =  Y(N,2)*CFACT1
                                      ELSE
                                      Y(N,1) = YT(N,J)*CFACT2
                                      Y(N,2) =  Y(N,2)*CFACT2
                                      ENDIF
                           ENDIF
  930                   CONTINUE
C=======================================================================
C                       PLOT ONLY THE PREDICTED DATA USING CURVE
C=======================================================================
                        IF(J.EQ.1.AND.SUMFLW.EQ.0.0) THEN
                        IF(JCE.EQ.0) WRITE(N6,9040) JPLOT(MT),NPT(1)
                        IF(JCE.EQ.1) WRITE(N6,9041) JMEAS(MT),NPT(1)
                        ELSE
                        CALL CURVE(X,Y,NPT,1,JPLOT(MT),JMEAS(MT))
                        IF(J.EQ.1) THEN
                                   USE = 'FLOW'
                                   ELSE
                                   IF(NLOAD(J).EQ.0) USE = 'CONC'
                                   IF(NLOAD(J).EQ.1) USE = 'LOAD'
                                   ENDIF
                        KP = IPOLX(J)
                        IF(KP.EQ.0) KP = 1
                        CALL HYSTAT(X,Y,NPT,1,JPLOT(MT),JMEAS(MT),TRIBA,
     +                       METRIC,USE,PUNIT(KP),PNAME(KP))
                        ENDIF
                        ENDIF
  820 CONTINUE
C=======================================================================
C     IF MORE THAN 201 DATA POINTS ARE ON THE INTERFACE FILE
C     RETURN AND CONTINUE PLOTTING THE HYDROGRAPHS AND POLLUTOGRAPHS
C=======================================================================
      IF(IBACK.GE.1.AND.NOTEND) THEN
                                N = 0
                                GO TO 5000
                                ENDIF
  800 CONTINUE
C=======================================================================
   10 FORMAT(/,
     +' ***************************************************',/,
     +' * ENTRY TO GRAPH BLOCK. LAST UPDATED AUGUST 1993. *',/,
     +' * "All art is quite useless."                     *',/,
     +' *                             Oscar Wilde (1891)  *',/,
     +' ***************************************************',/)
C#### WCH, 8/5/93
   12 FORMAT(/,
     +' ***************************************************',/,
     +' * ENTRY TO GRAPH BLOCK. LAST UPDATED AUGUST 1993. *',/,
     +' ***************************************************',/)
   30 FORMAT(/,' INPUT PARAMETER SUMMARY:',//,
     1 ' NUMBER OF OFF-LINE FILE OF PREDICTED DATA(NTAPE)....',I3,/,
     2 ' FLOW ORDINATE IN CFS (=0) OR CU M/S (=1) (METRIC)...',I3,/,
     3 ' NUMBER OF POLLUTOGRAPHS TO BE PLOTTED (NQP).........',I3,/,
     4 ' NUMBER OF LOCATIONS FOR PREDICTED PLOTS(NPLOT)......',I3,/,
     5 ' INDICATING MEASURED DATA AND THEIR STORAGE(MEAS)....',I3,/,
     6 ' FILE NUMBER FOR RETRIEVAL OF MEASURED DATA(MFILE)...',I3,/,
     7 ' NUMBER OF LOCATIONS FOR MEASURED DATA(MPLOT)........',I3,/,
     8 ' SKIP MCTH VALUES ON INTERFACE FILE..................',I3)
   34 FORMAT(/,' PLOTTING ',I5,' LOCATIONS',/,' PLOTTING GRAPH # ',/)
   35 FORMAT('+',I16)
   40 FORMAT(//,' PREDICTED GRAPHS ARE TO BE PLOTTED FOR THE FOLLOWING
     1 ',I4,' LOCATIONS :',//,10I11,/,10I11)
   41 FORMAT(//,' PREDICTED GRAPHS ARE TO BE PLOTTED FOR THE FOLLOWING
     1 ',I4,' LOCATIONS :',//,10A11,/,10A11)
   55 FORMAT(/,' MEASURED GRAPHS ARE TO BE PLOTTED FOR THE FOLLOWING ',
     1I4,' LOCATIONS :',//,10I11,/,10I11)
   56 FORMAT(/,' MEASURED GRAPHS ARE TO BE PLOTTED FOR THE FOLLOWING ',
     1I4,' LOCATIONS :',//,10A11,/,10A11)
  310 FORMAT(//,1X,A14,
     1 ' DATA GROUP E1 INPUT PARAMETERS FOR LOCATION ',I5,' ARE:',/,
     2' MDATA = ',I10,'     LCARD = ',I10,  '    MTIME = ',I10,/,
     2' MUNIT = ',I10,'    TMZERO = ',F10.4,'    TQUIT = ',F10.4,/,
     3' DTMHR = ',F10.4)
  360 FORMAT(/,' INPUT FORMAT FOR THESE DATA IS: ',A80)
  505 FORMAT(I6,' MEASURED DATA POINTS READ IN.')
  520 FORMAT(/,' ===> WARNING! MTIME=',I5,', GT 201. NOT ALLOWED.',
     1 '  MTIME WILL BE SET = 201.')
 4090 FORMAT(/,
     1' ****************************************',/,
     1' *  QUALITY CONSTITUENTS TO BE GRAPHED  *',/,
     1' ****************************************',//,
     1 '                       TYPE OF   INTERFACE FILE',/,
     3 ' NO.  NAME     UNITS    UNITS   POSITION (IF ANY) PLOT',
     3 ' DESCRIPTION',/,
     4 ' --- -----     -----    -----   ----------------------',
     5 '------------')
 4150 FORMAT(I3,2X,A8,2X,A8,I6,I13,A30)
 9020 FORMAT(/,' ===> ERROR  LOCATION: ',I10,' WAS NOT FOUND ON THE ',
     +                                      'INTERFACE FILE.')
 9021 FORMAT(/,' ===> ERROR  LOCATION: ',A10,' WAS NOT FOUND ON THE ',
     +                                      'INTERFACE FILE.')
 9040 FORMAT(/,' ===> HYDROGRAPH FOR LOCATION ',I10,' WITH ',I5,
     + ' POINTS',/,'      WAS NOT PLOTTED BECAUSE THE FLOW WAS ZERO.')
 9041 FORMAT(/,' ===> HYDROGRAPH FOR LOCATION ',A10,' WITH ',I5,
     + ' POINTS', /,'      WAS NOT PLOTTED BECAUSE THE FLOW WAS ZERO.')
C#### WCH, 8/5/93
 9500 FORMAT(/,' ===> ERROR. FOR NTAPE > 0, NTAPE MUST EQUAL JIN.',/,
     + '     NTAPE =',I5,'  JIN =',I5,'  COUNTER (INCNT) =',I5)
C=======================================================================
      RETURN
  888 CALL IERROR
      END

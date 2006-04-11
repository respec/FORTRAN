      SUBROUTINE TIDCF(KO,NI,NCHTID,ICNOW,DELTA)
C	EXTRAN BLOCK
C	CALLED BY INDAT3 AT LNE 95
C=======================================================================
C     THIS SUBROUTINE COMPUTES SEVEN COEFFICIENTS
C     FOR A FOURIER EXPANSION OF THE DIURNAL TIDE STAGE
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'TIDE.INC'
C=======================================================================
C     TIDE COEFFICIENTS
C     TIDAL CURVE FIT, 7 TERM, SINUSOIDAL EQUATION
C=======================================================================
      WRITE(N6,140) ICNOW,KO,NI,NCHTID
C=======================================================================
C     IF KO EQUALS ONE, PROGRAM WILL
C     READ FOUR POINTS OF INFORMATION AND EXPAND THEM FOR A FULL TIDE
C
C     NT IS THE NUMBER OF INFORMATION POINTS
C     IF NCHTID EQUALS ONE, TIDAL INPUT-OUTPUT WILL BE PRINTED
C
C     MAXIT IS THE MAXIMUM NUMBER OF ITERATIONS
C     DELTA IS THE ACCURACY LIMIT IN FEET
C     PHLAG = PHASE LAG OF TIDE
C     TT    = REAL TIME ASSOCIATED WITH YY
C     YY    = 4 TIDE POINTS INPUT
C=======================================================================
      PERIOD = W(ICNOW)
      MAXIT  = 50
      NTT      = 7
      W(ICNOW) = 2.0*3.14159/PERIOD
      IF(KO.EQ.0) GO TO 225
      TT(50)   = TT(1) + PERIOD
      YY(50)   = YY(1)
      DTT      = TT(4)  - TT(1)
      DIFF     = PERIOD - DTT
      IF(DIFF) 100,101,101
  100 WRITE(N6,111)
      STOP
  101 PHLAG    = TT(1) - DIFF*0.5
      DO 110 I = 1,4
      TT(I)    = TT(I) - PHLAG
  110 CONTINUE
C=======================================================================
      DO 220 I = 1,4
      J        = I+1
      IF(J.GT.4) J = 50
      NI       = NI + 1
      TT(NI)   = (3.0*TT(I)   + TT(J))/4.0
      YY(NI)   = 0.8535*YY(I) + 0.1465*YY(J)
      NI       = NI + 1
      TT(NI)   = (TT(I) + TT(J))/2.0
      YY(NI)   = (YY(I) + YY(J))/2.0
      NI       = NI + 1
      TT(NI)   = (TT(I)       + 3.0*TT(J))/4.0
      YY(NI)   = 0.1465*YY(I) + 0.8535*YY(J)
  220 CONTINUE
  225 CONTINUE
C=======================================================================
      DO 280 J = 1,NTT
      DO 260 K = 1,NTT
  260 SXX(K,J) = 0.0
      AA(J)    = 0.0
  280 SXY(J)   = 0.0
      NJ2      = NTT/2 + 1
      DO 360 I = 1,NI
      DO 320 J = 1,NTT
      FJ1      = FLOAT(J-1)
      FJ3      = FLOAT(J-NJ2)
      IF( J.LE.NJ2 ) GO TO 300
      XX(J)    = COS(FJ3*W(ICNOW)*TT(I))
      GO TO 320
  300 XX(J)    = SIN(FJ1*W(ICNOW)*TT(I))
      IF(J.EQ.1 ) XX(J) = 1.0
  320 SXY(J)   = SXY(J) + XX(J) * YY(I)
      DO 340 J = 1,NTT
      DO 340 K = 1,NTT
  340 SXX(K,J) = SXX(K,J) + XX(K) * XX(J)
  360 CONTINUE
      IT       = 0
  380 IT       = IT + 1
      DELMAX   = 0.0
      DO 420 K = 1,NTT
      SUM      = 0.0
      DO 400 J = 1,NTT
      IF(J.EQ.K) GO TO 400
      SUM      = SUM - AA(J)*SXX(K,J)
  400 CONTINUE
      SUM      = (SUM + SXY(K))/SXX(K,K)
      DEL      = ABS(SUM - AA(K))
      IF(DEL.GT.DELMAX ) DELMAX = DEL
  420 AA(K)    = SUM
      IF(IT.GE.MAXIT)     GO TO 440
      IF(DELMAX.GT.DELTA) GO TO 380
      GO TO 460
  440 WRITE(N6,150)
      STOP
  460 CONTINUE
C=======================================================================
C     TRANSFER TIDAL COEFFICIENTS
C=======================================================================
      A1(ICNOW) = AA(1)
      A2(ICNOW) = AA(2)
      A3(ICNOW) = AA(3)
      A4(ICNOW) = AA(4)
      A5(ICNOW) = AA(5)
      A6(ICNOW) = AA(6)
      A7(ICNOW) = AA(7)
C=======================================================================
C     WRITE OUT THE TIDAL INFORMATION
C=======================================================================
      IF(NCHTID.NE.1) GO TO 540
      IF(METRIC.EQ.1) WRITE(N6,152)
      IF(METRIC.EQ.2) WRITE(N6,153)
      RES      = 0.0
      DO 520 I = 1,NI
      SUM      = 0.0
      DO 500 J = 2,NTT
      FJ1      = FLOAT ( J-1 )
      FJ3      = FLOAT ( J-NJ2 )
      IF(J.LE.NJ2) GO TO 480
      SUM      = SUM + AA(J) * COS(FJ3*W(ICNOW)*TT(I))
      GO TO 500
  480 SUM      = SUM + AA(J) * SIN(FJ1*W(ICNOW)*TT(I))
  500 CONTINUE
      SUM      = SUM + AA(1)
      DIFF     = SUM - YY(I)
      RES      = RES + ABS(DIFF)
  520 WRITE(N6,154) I,TT(I),YY(I),SUM,DIFF
      WRITE(N6,156) RES
  540 CONTINUE
C====================================================================
C     CONSTANTS FOR INPUT WAVE FORM
C====================================================================
      WRITE(N6,158)   AA(1),AA(2),AA(3),AA(4),AA(5),
     +                AA(6),AA(7),W(ICNOW),PHLAG
      PHLAGS(ICNOW) = PHLAG
C====================================================================
  111 FORMAT(/,' ====> ERROR !!!  FOUR TIDE POINTS DO NOT FALL IN THE',
     1' TIDAL PERIOD',/)
  140 FORMAT(/,
     +' ***********************************************',/,
     +' *   TIDAL INFORMATION FROM THE J3 DATA GROUP  *',/,
     +' *   FOR BOUNDARY CONDITION NUMBER ',I5,'       *',/,
     +' ***********************************************',//,
     +' KO............................',I5,
     +' NUMBER OF POINTS(NI)..........',I5,/,
     +' MAXIMUM NUMBER OF ITERATIONS..   50',
     +' TIDE CHECK SWITCH(NCHTID).....',I5,/)
  150 FORMAT(' ===> ERROR !! CANNOT REACH DESIRED DELTA.  INCREASE EITHE
     1R NI OR DELTA AND TRY AGAIN')
  152 FORMAT('           TIME IN    OBSERVED   COMPUTED',/,
     +       '  NO.        HOURS   STAGE(FT)   STAGE(FT)  DIFFERENCE',/,
     +       '  ---      -------   ---------   ---------  ----------')
  153 FORMAT('           TIME IN    OBSERVED   COMPUTED',/,
     +       '  NO.        HOURS   STAGE (M)   STAGE (M)  DIFFERENCE',/,
     +       '  ---      -------   ---------   ---------  ----------')
  154 FORMAT(I4,'. ',4F12.4)
  156 FORMAT(' ABSOLUTE RESIDUAL........................',F12.4)
  158 FORMAT(/,
     +' ************************************************',/,
     1' *          TIDAL STAGE COEFFICIENTS            *',/,
     +' ************************************************',/,
     1'     A1     A2     A3     A4     A5     A6     A7      W  PHASE L
     +AG',/,
     1'  -----  -----  -----  -----  -----  -----  -----  -----  -------
     +--',/,8F7.2,F11.2,/,
     +' ************************************************',/,
     2' * THE WAVEFORM FUNCTION HAS THE FOLLOWING FORM.*',/,
     +' ************************************************',/,
     3' H(J) = A1 + A2*SIN(WT) + A3*SIN(2WT) + A4*SIN(3WT) ',/,
     4'           + A5*COS(WT) + A6*COS(2WT) + A7*COS(3WT)',/,
     +' ************************************************')
C=======================================================================
      RETURN
      END

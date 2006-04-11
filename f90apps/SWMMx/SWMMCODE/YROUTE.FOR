      SUBROUTINE YROUTE
C=======================================================================
C     THIS IS SUBROUTINE YROUTE OF THE SEWER MODEL
C     IT PERFORMS THE MODIFIED EULER SOLUTION OF THE
C     MOTION AND CONTINUITY EQUATIONS LAST UPDATED NOVEMBER, 1989
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'BD.INC'
      INCLUDE 'BND.INC'
      INCLUDE 'HYFLOW.INC'
      INCLUDE 'CONTR.INC'
      INCLUDE 'JUNC.INC'
      INCLUDE 'PIPE.INC'
      INCLUDE 'TIDE.INC'
      INCLUDE 'OUT.INC'
      INCLUDE 'ORF.INC'
      INCLUDE 'WEIR.INC'
      INCLUDE 'FLODAT.INC'
C=======================================================================
C     STORE OLD TIME STEP FLOW VALUES
C=======================================================================
      DO 60 N   = 1,NTL
   60 QO(N)     = Q(N)
C=======================================================================
C     INITIALIZE CONTINUITY PARAMETERS
C=======================================================================
      DO 80 J  = 1,NJ
      YO(J)    = Y(J)
      AS(J)    = AMEN
      SUMQ(J)  = QIN(J)
      SUMQS(J) = QIN(J)
   80 SUMAL(J) = 0.0
C=======================================================================
C     HALF-STEP AREA, RADIUS : VELOCITY
C     FULL-STEP AREA, RADIUS : VELOCITY
C     HALF-STEP FLOW
C=======================================================================
      DO 100 N = 1,NTC
      NL       = NJUNC(N,1)
      NH       = NJUNC(N,2)
      H(N,1)   = Y(NL) + Z(NL)
      H(N,2)   = Y(NH) + Z(NH)
      CALL HEAD(N,NL,NH,H(N,1),H(N,2),QO(N),A(N),V(N),HRAD,
     +                       ANH,ANL,RNL,RNH,IDOIT,LINK(N))
      IF(HRAD.GT.HMAX(N)) HMAX(N) = HRAD
      IF(A(N).GT.AMAX(N)) AMAX(N) = A(N)
      IF(IDOIT.EQ.0) THEN
                     QT(N) = 0.0
                     AKON  = 0.0
                     GO TO 95
                     ENDIF
      DELQ4 =  DELT2*QO(N)*(ANH-ANL)/(AT(N)*A(N)*LEN(N))
      DELQ3 =  2.0*(A(N)-AT(N))/A(N)
      DELQ2 =  DELT2*GRVT*A(N)*(H(N,2)-H(N,1))/LEN(N)
      DELQ1 =  DELT2*(ROUGH(N)/HRAD**1.33333)*ABS(V(N))
      QNEW  =  QO(N) - DELQ2
      AKON  =  DELQ1 - DELQ4 - DELQ3
      QT(N) =  QNEW/(1.0+AKON)
C=======================================================================
C     CHECK FOR NORMAL FLOW - IF THE WATER SURFACE SLOPE IS LESS THAN
C                             THE CONDUIT SLOPE THE FLOW IS SUPERCRIT.
C=======================================================================
      ICHECK(N) = IND(1)
      IF(KSUPER.EQ.0.OR.NKLASS(N).GE.9.OR.JSKIP(NL).GT.0.
     +                                 OR.JSKIP(NH).GT.0) THEN
      IF(QT(N).GT.0.0.AND.H(N,1).LE.ZCROWN(NL)) THEN
      DELH      = H(N,1) - H(N,2)
      DELZP     = ZU(N)  - ZD(N)
      IF(DELH-DELZP.LT.0.0) THEN
               QNORM = SQRT(GRVT*(ZU(N)-ZD(N))/
     +                   (LEN(N)*ROUGH(N)))*ANL*RNL**0.666667
               IF(QNORM.LE.QT(N)) THEN
                                  ICHECK(N) = IND(2)
                                  QT(N)     = QNORM
                                  ENDIF
               ENDIF
      ENDIF
      ENDIF
C=======================================================================
C     CHECK FOR NORMAL FLOW - IF THE FROUDE NUMBER IS > 1.
C                             THE FLOW IS SUPERCRITICAL.
C=======================================================================
      IF(NKLASS(N).LE.8.AND.H(N,1).LE.ZCROWN(NL).AND.JSKIP(NL).EQ.0.
     +                  AND.JSKIP(NH).EQ.0) THEN
      IF(QT(N).GT.0.0.AND.H(N,1).LE.ZCROWN(NL)) THEN
      DIFF1 = H(N,1) - ZU(N)
      DIFF2 = H(N,2) - ZD(N)
      IF(KSUPER.EQ.1.AND.DIFF1.GT.0.0.AND.DIFF2.GT.0.0) THEN
               FF1 = ABS(QT(N))/ANL/SQRT(GRVT*(DIFF1))
               FF2 = ABS(QT(N))/ANH/SQRT(GRVT*(DIFF2))
               QNORM = SQRT(GRVT*(ZU(N)-ZD(N))/
     +                   (LEN(N)*ROUGH(N)))*ANL*RNL**0.666667
                                            ITEST = 0
               IF(FF1.GE.1.0.OR.FF2.GE.1.0) ITEST = 1
               IF(QNORM.LE.QT(N).AND.ITEST.EQ.1) THEN
                                                 ICHECK(N) = IND(2)
                                                 QT(N)     = QNORM
                                                 ENDIF
               ENDIF
      ENDIF
      ENDIF
C=======================================================================
C     COMPUTE CONTINUITY PARAMETERS
C=======================================================================
      IF(INGATE(N).EQ.1.AND.QT(N).LT.0.0) QT(N) = 0.0
      IF(INGATE(N).EQ.2.AND.QT(N).GT.0.0) QT(N) = 0.0
  95  DQDH      = 1.0/(1.0+AKON)*GRVT*DELT2*A(N)/LEN(N)
      SUMQ(NL)  = SUMQ(NL)  - 0.5*(QT(N)+QO(N))
      SUMQS(NL) = SUMQS(NL) - QT(N)
      SUMAL(NL) = SUMAL(NL) + DQDH
      SUMQ(NH)  = SUMQ(NH)  + 0.5*(QT(N)+QO(N))
      SUMQS(NH) = SUMQS(NH) + QT(N)
      SUMAL(NH) = SUMAL(NH) + DQDH
  100 CONTINUE
C=======================================================================
C     SET HALF STEP OUTFLOWS AND INTERNAL TRANSFERS
C=======================================================================
      CALL BOUND(Y,YT,QT,TIME2,DELT2)
C=======================================================================
      DO 130 N  = NTC+1,NTL
      NL        = NJUNC(N,1)
CC$$$$5/3/92
      IF(ABS(QT(N)).LT.1E-10) QT(N) = 0.0
      SUMQ(NL)  = SUMQ(NL)  - (QT(N)+QO(N))
      SUMQS(NL) = SUMQS(NL) - QT(N)
      NH        = NJUNC(N,2)
      IF(NH.GT.0) THEN
                  SUMQ(NH)  = SUMQ(NH)  + 0.5*(QT(N)+QO(N))
                  SUMQS(NH) = SUMQS(NH) + QT(N)
                  ENDIF
  130 CONTINUE
C=======================================================================
C     CALCULATE THE HALF-STEP HEAD
C=======================================================================
      DO 320 J = 1,NJ
      IF(JSKIP(J).GT.0) GO TO 320
C=======================================================================
C     COMPUTE YT FOR FREE SURFACE JUNCTIONS
C=======================================================================
      IF(Y(J)-YCROWN(J).LE.0.0) THEN
           YT(J)                  = YO(J)  + SUMQ(J)*DELT2/AS(J)
           ASFULL(J)              = AS(J)
           IF(YT(J).LT.0.0) YT(J) = FUDGE
           ELSE
                                      DENOM = SUMAL(J)
           IF(Y(J).LT.1.25*YCROWN(J)) DENOM = SUMAL(J) +
     + (ASFULL(J)/DELT2-SUMAL(J))*EXP(-15.*(Y(J)-YCROWN(J))/YCROWN(J))
                               CORR     = 0.50
           IF(NCHAN(J,2).EQ.0) CORR     = 0.30
           YT(J)                        = Y(J) + CORR*SUMQS(J)/DENOM
           IF(YT(J).LT.YCROWN(J)) YT(J) = YCROWN(J) - FUDGE
           ENDIF
           IF((YT(J)+Z(J)).GT.GRELEV(J)) YT(J) = GRELEV(J)-Z(J)
C=======================================================================
C     CALCULATE THE ALLOWABLE SURCHARGE TOLERANCE
C=======================================================================
      QAVE     = 0.0
      DO 280 K = 1,8
      IF(NCHAN(J,K).LE.0) GO TO 290
  280 QAVE = QAVE + ABS(Q(NCHAN(J,K)))
  290 K    = K-1
      QAVE = SURTOL*QAVE/FLOAT(K)
      TOL  = (TOL*(NSUR-1)+QAVE)/NSUR
      NSUR = NSUR+1
C=======================================================================
C     INITIALIZE FOR FULL STEP FLOWS
C=======================================================================
  300 AS(J)    = AMEN
      SUMQ(J)  = QIN(J)
      SUMQS(J) = QIN(J)
      SUMAL(J) = 0.0
  320 CONTINUE
C=======================================================================
C=======================================================================
C     HALF-STEP AREA, RADIUS : VELOCITY
C     FULL-STEP FLOW
C=======================================================================
C=======================================================================
      DO 1000 KLOOP = 1,100000
      ERROR         = 0.0
      DO 360 N      = 1,NTC
      NL            = NJUNC(N,1)
      NH            = NJUNC(N,2)
C=======================================================================
C     CHECK WHETHER SURCHARGE ITERATIONS OCCURRING
C=======================================================================
      IF(IT.GT.0.AND.JCHECK(NH).EQ.IND(1).AND.JCHECK(NL).EQ.IND(1))
     +                                        GO TO 360
      H(N,1) = YT(NL) + Z(NL)
      H(N,2) = YT(NH) + Z(NH)
      CALL HEAD(N,NL,NH,H(N,1),H(N,2),QT(N),AT(N),VT(N),HRAD,
     +                         ANH,ANL,RNL,RNH,IDOIT,LINK(N))
      IF(IDOIT.EQ.0) THEN
                     Q(N)  = 0.0
                     AKON  = 0.0
                     GO TO 995
                     ENDIF
      DELQ4 =  DELT2*QT(N)*(ANH-ANL)/(AT(N)*A(N)*LEN(N))
      DELQ3 =  4.0*(AT(N)-A(N))/AT(N)
      DELQ2 =  DELT*GRVT*AT(N)*(H(N,2)-H(N,1))/LEN(N)
      DELQ1 =  DELT*(ROUGH(N)/HRAD**1.33333)*ABS(VT(N))
      QNEW  =  QO(N) - DELQ2
      AKON  =  DELQ1 - DELQ4 - DELQ3
      Q(N)  =  QNEW/(1.0+AKON)
C=======================================================================
C     CHECK FOR NORMAL FLOW - IF THE WATER SURFACE SLOPE IS LESS THAN
C                             THE CONDUIT SLOPE THE FLOW IS SUPERCRIT.
C=======================================================================
      ICHECK(N) = IND(1)
      IF(KSUPER.EQ.0.OR.NKLASS(N).GE.9.OR.JSKIP(NL).GT.0.
     +                                 OR.JSKIP(NH).GT.0) THEN
      IF(Q(N).GT.0.0.AND.H(N,1).LE.ZCROWN(NL)) THEN
      DELH      = H(N,1) - H(N,2)
      DELZP     = ZU(N)  - ZD(N)
      IF(DELH-DELZP.LT.0.0) THEN
               QNORM = SQRT(GRVT*(ZU(N)-ZD(N))/
     +                   (LEN(N)*ROUGH(N)))*ANL*RNL**0.666667
               IF(QNORM.LE.Q(N)) THEN
                                 ICHECK(N) = IND(2)
                                 Q(N)      = QNORM
                                 ENDIF
               ENDIF
      ENDIF
      ENDIF
C=======================================================================
C     CHECK FOR NORMAL FLOW - IF THE FROUDE NUMBER IS > 1.
C                             THE FLOW IS SUPERCRITICAL.
C=======================================================================
      IF(NKLASS(N).LE.8.AND.H(N,1).LE.ZCROWN(NL).AND.JSKIP(NL).EQ.0.
     +                  AND.JSKIP(NH).EQ.0) THEN
      IF(Q(N).GT.0.0.AND.H(N,1).LE.ZCROWN(NL)) THEN
      DIFF1 = H(N,1) - ZU(N)
      DIFF2 = H(N,2) - ZD(N)
      IF(KSUPER.EQ.1.AND.DIFF1.GT.0.0.AND.DIFF2.GT.0.0) THEN
               FF1 = ABS(Q(N))/ANL/SQRT(GRVT*(DIFF1))
               FF2 = ABS(Q(N))/ANH/SQRT(GRVT*(DIFF2))
               QNORM = SQRT(GRVT*(ZU(N)-ZD(N))/
     +                   (LEN(N)*ROUGH(N)))*ANL*RNL**0.666667
                                            ITEST = 0
               IF(FF1.GE.1.0.OR.FF2.GE.1.0) ITEST = 1
               IF(QNORM.LE.Q(N).AND.ITEST.EQ.1) THEN
                                                ICHECK(N) = IND(2)
                                                Q(N)      = QNORM
                                                ENDIF
               ENDIF
      ENDIF
      ENDIF
C=======================================================================
C     DO NOT ALLOW A FLOW REVERSAL IN ONE TIME STEP
C=======================================================================
      DIRQT = SIGN(1.0,QT(N))
      DIRQ  = SIGN(1.0,Q(N))
      IF(DIRQT/DIRQ.LT.0.0) Q(N) = 0.001*DIRQ
C=======================================================================
C     COMPUTE CONTINUITY PARAMETERS
C=======================================================================
      IF(INGATE(N).EQ.1.AND.Q(N).LT.0.0) Q(N) = 0.0
      IF(INGATE(N).EQ.2.AND.Q(N).GT.0.0) Q(N) = 0.0
  995 DQDH      = 1.0/(1.0+AKON)*GRVT*DELT*AT(N)/LEN(N)
      SUMQ(NL)  = SUMQ(NL)  - 0.5*(Q(N)+QO(N))
      SUMQS(NL) = SUMQS(NL) - Q(N)
      SUMAL(NL) = SUMAL(NL) + DQDH
      SUMQ(NH)  = SUMQ(NH)  + 0.5*(Q(N)+QO(N))
      SUMQS(NH) = SUMQS(NH) + Q(N)
      SUMAL(NH) = SUMAL(NH) + DQDH
  360 CONTINUE
C=======================================================================
C     SET FULL STEP OUTFLOWS AND INTERNAL TRANSFERS
C=======================================================================
      CALL BOUND(YT,Y,Q,TIME,DELT)
C=======================================================================
      N1       = NTC+1
      DO 370 N = N1,NTL
      NL       = NJUNC(N,1)
      NH       = NJUNC(N,2)
CC$$$$$5/3/92
      IF(ABS(Q(N)).LT.1E-10) Q(N) = 0.0
C=======================================================================
C     CHECK WHETHER SURCHARGE ITERATIONS OCCURRING
C=======================================================================
      IF(NH.GT.0) THEN
                  IF(IT.GT.0.AND.JCHECK(NH).EQ.IND(1).AND.
     +                 JCHECK(NL).EQ.IND(1))           GO TO 370
                  ENDIF
C=======================================================================
C     DO NOT ALLOW FLOW REVERSAL IN ONE TIME STEP
C=======================================================================
      DIRQT = SIGN(1.0,QT(N))
      DIRQ  = SIGN(1.0,Q(N))
      IF(DIRQT/DIRQ.LT.0.0) Q(N) = 0.001*DIRQ
      SUMQ(NL)  = SUMQ(NL)  - 0.5*(Q(N)+QO(N))
      SUMQS(NL) = SUMQS(NL) - Q(N)
      IF(NH.NE.0) THEN
                  SUMQ(NH)  = SUMQ(NH) + 0.5*(Q(N)+QO(N))
                  SUMQS(NH) = SUMQS(NH)+ Q(N)
                  ENDIF
  370 CONTINUE
C=======================================================================
C     CALCULATE THE FULL-STEP HEAD
C=======================================================================
      DO 560 J = 1,NJ
      IF(JSKIP(J).GT.0) GO TO 560
C=======================================================================
C     CHECK WHETHER SURCHARGE ITERATIONS OCCURRING
C     COMPUTE Y FOR FREE SURFACE JUNCTIONS
C=======================================================================
      IF(YT(J)-YCROWN(J).LE.0.0) THEN
           IF(IT.GT.0) GO TO 560
           JCHECK(J)             = IND(1)
           Y(J)                  = YO(J)  + SUMQ(J)*DELT/AS(J)
           ASFULL(J)             = AS(J)
           IF(Y(J).LT.0.0) Y(J) = FUDGE
           ELSE
                                       DENOM = SUMAL(J)
           IF(YT(J).LT.1.25*YCROWN(J)) DENOM = SUMAL(J) +
     +  (ASFULL(J)/DELT-SUMAL(J))*EXP(-15.*(YT(J)-YCROWN(J))/YCROWN(J))
                               CORR   = 1.00
           IF(NCHAN(J,2).EQ.0) CORR   = 0.60
           Y(J)                       = YT(J) + CORR*SUMQS(J)/DENOM
           IF(Y(J).LT.YCROWN(J)) Y(J) = YCROWN(J) - FUDGE
           JCHECK(J)                  = IND(2)
           IF((Y(J)+Z(J)).LT.GRELEV(J)) ERROR = ERROR + SUMQS(J)
           ENDIF
           IF((Y(J)+Z(J)).GT.GRELEV(J)) Y(J)  = GRELEV(J)-Z(J)
  560 CONTINUE
C=======================================================================
C     CHECK CONVERGENCE OF THE FLOW ERROR IN SURCHARGED AREAS
C                       OR INITIALIZE FOR NEXT ITERATION
C=======================================================================
      IF(ABS(ERROR)-TOL.LE.0.0.OR.IT-ITMAX+1.GT.0) RETURN
      IT       = IT+1
      DO 570 J = 1,NJ
      IF(JCHECK(J).EQ.IND(2)) THEN
                              YT(J)    = Y(J)
                              SUMQ(J)  = QIN(J)
                              SUMQS(J) = QIN(J)
                              SUMAL(J) = 0.0
                              ENDIF
  570 CONTINUE
 1000 CONTINUE
      RETURN
      END

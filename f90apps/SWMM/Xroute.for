      SUBROUTINE XROUTE
C	EXTRAN BLOCK
C	CALLED BY TRANSX NEAR LINE 605
C=======================================================================
C     THIS IS SUBROUTINE XROUTE OF THE SEWER MODEL
C     IT PERFORMS THE MODIFIED EULER SOLUTION OF THE
C     MOTION AND CONTINUITY EQUATIONS
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
      INCLUDE 'WEIR.INC'
      INCLUDE 'FLODAT.INC'
      DOUBLE PRECISION AKON,QNEW,DELQ1,DELQ2,DELQ3,DELQ4,DELQ5
      DIMENSION SUMQ1(NEE),SUMQS1(NEE),SUMAL1(NEE),AS1(NEE)
      DIMENSION SUMQL(NEE),SUMQSL(NEE),SUMALL(NEE),ASL(NEE)
C=======================================================================
C     STORE OLD TIME STEP FLOW VALUES
C=======================================================================
      DO 60 N = 1,NTL
   60 QO(N)   = Q(N)
C=======================================================================
C     INITIALIZE CONTINUITY PARAMETERS
C=======================================================================
      DO 80 J  = 1,NJ
      YO(J)    = Y(J)
      AS(J)    = AMEN
      SUMQ(J)  = QIN(J)
      SUMQS(J) = QIN(J)
   80 SUMAL(J) = 0.0
CIM START  OOOOOO
CIM  SET HALFSTEP GATED ORIFICE PARAMETERS
CIM        FLOWS COMPUTED WILL BE BASED ON FULLSTEP DEPTHS (Y) AT CONTROL NODE
      CALL OGATES(DELT2,Y,V)
CIM END  OOOOOOOO
C=======================================================================
C     HALF-STEP AREA, RADIUS : VELOCITY
C     FULL-STEP AREA, RADIUS : VELOCITY
C     HALF-STEP FLOW
C=======================================================================
C$OMP PARALLEL
C$OMP& PRIVATE(N,NL,NH,HRAD,IDOIT,AKON,DQDH,ANH,ANL,J)
C$OMP& PRIVATE(DELQ1,DELQ2,DELQ3,DELQ4,DELQ5,QNEW,DELH,DELZP,DIFF1)
C$OMP& PRIVATE(DIFF2,RNL,QNORM,ANH,RNH,SUMQ1,SUMQS1,SUMAL1)
C$OMP& PRIVATE(AS1)
C
CWRM CLEAR THE PARTIAL SUMS
      DO J = 1, NJ
        SUMQ1(J)  = QIN(J)
        SUMQS1(J) = QIN(J)
        SUMAL1(J) = 0.0
        AS1(J) = AMEN
      ENDDO
C
C$OMP DO SCHEDULE(GUIDED)
      DO 100 N = 1,NTC
      NL       = NJUNC(N,1)
      NH       = NJUNC(N,2)
      H(N,1)    = AMAX1(Y(NL) + Z(NL),ZU(N))
      H(N,2)    = AMAX1(Y(NH) + Z(NH),ZD(N))
      CALL NHEAD(N,NL,NH,H(N,1),H(N,2),QO(N),A(N),V(N),HRAD,
     +                   ANH,ANL,RNL,RNH,IDOIT,LINK(N),AS1)
      IF(HRAD.GT.HMAX(N)) HMAX(N) = HRAD
      IF(A(N).GT.AMAX(N)) AMAX(N) = A(N)
      IF(IDOIT.EQ.0) THEN
                     QT(N) = 0.0
                     AKON  = 0.0
                     GO TO 95
                     ENDIF
c
c calculate additional loss terms per red incorporated by cim cdm 9/97
c
c    Q/ANH = velocity at downstream end used for exit loss
c    Q/ANL = velocity at upstream end used for entrance loss
c       The loss = 1/2*K*V^2/g is factored into momentum
c       equation similarly to the friction slope
c       The loss momentum term = g*A*[1/2*K*V^2/g]
c                              = g*A*[1/2*K*Q/A*Q/A/g]
C                               =g  *[1/2*K*|Q*A|  /g] * Q
C                               =    [1/2*K*|Q*A|    ] * Q
c     DELQ5 is sum of losses
      DELQ5 = 0.0
      IF (ANH.GT.0.0)  DELQ5 = DELQ5 + 0.5 * ABS(QO(N)/ANH)*ENTK(N)
      IF (ANL.GT.0.0)  DELQ5 = DELQ5 + 0.5 * ABS(QO(N)/ANL)*EXITK(N)
      IF (A(N).GT.0.0) DELQ5 = DELQ5 + 0.5 * ABS(QO(N)/A(N))*OTHERK(N)
      DELQ5 =  DELQ5 * DELT2/LEN(N)
C
      DELQ4 =  DELT2*V(N)**2*(ANH-ANL)/LEN(N)
      DELQ3 =  2.0*V(N)*(A(N)-AT(N))
      DELQ2 =  DELT2*GRVT*A(N)*(H(N,2)-H(N,1))/LEN(N)
      DELQ1 =  DELT2*(ROUGH(N)/HRAD**1.33333)*ABS(V(N))
      AKON  =  DELQ1 + DELQ5
      QNEW  =  QO(N) - DELQ2 + DELQ3 + DELQ4
      QT(N) =  QNEW/(1.0+AKON)
C=======================================================================
C     CHECK FOR NORMAL FLOW - IF THE WATER SURFACE SLOPE IS LESS THAN
C                             THE CONDUIT SLOPE THE FLOW IS SUPERCRIT.
C=======================================================================
      ICHECK(N) = IND(1)
CIM START  OOOOOOOOO
CIM     ORIFICES
      IF(KSUPER.EQ.0.OR.NKLASS(N).GE.51.OR.JSKIP(NL).GT.0.
CIM END      OOOOOOOO
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
CIM START  <><><><><>
CIM  ALL BUT ORIFICES
      IF(NKLASS(N).LE.50.AND.H(N,1).LE.ZCROWN(NL).AND.JSKIP(NL).EQ.0.
CIM END      <><><><><><>
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
               IF(QNORM.LT.QT(N).AND.ITEST.EQ.1) THEN
                                                 ICHECK(N) = IND(2)
                                                 QT(N)     = QNORM
                                                 ENDIF
               ENDIF
      ENDIF
      ENDIF
C=======================================================================
C     COMPUTE CONTINUITY PARAMETERS
C=======================================================================
      SELECT CASE (INGATE(N))
      CASE (1)
      QT(N) = AMAX1(QT(N),0.0)
c      IF(INGATE(N).EQ.1.AND.QT(N).LT.0.0) QT(N) = 0.0
      CASE (2)
      QT(N) = AMIN1(QT(N),0.0)
c      IF(INGATE(N).EQ.2.AND.QT(N).GT.0.0) QT(N) = 0.0
      END SELECT
      IF (NKLASS(N).LE.21) THEN
      QT(N) = AMAX1(STHETA(N),QT(N))
      QT(N) = AMIN1(SPHI(N),QT(N))
      ENDIF
  95  DQDH      = 1.0/(1.0+AKON)*GRVT*DELT2*A(N)/LEN(N)
      SUMQ1(NL)  = SUMQ1(NL)  - 0.5*(QT(N)+QO(N))
      SUMQS1(NL) = SUMQS1(NL) - QT(N)
      SUMAL1(NL) = SUMAL1(NL) + DQDH
      SUMQ1(NH)  = SUMQ1(NH)  + 0.5*(QT(N)+QO(N))
      SUMQS1(NH) = SUMQS1(NH) + QT(N)
      SUMAL1(NH) = SUMAL1(NH) + DQDH
  100 CONTINUE
C$OMP END DO NOWAIT
CWRM COMBINE THE PARTIAL SUMS
CWRM SPLIT CRITICAL SECTIONS REDUCES CONTENTION FOR LOCKS
C$OMP CRITICAL (TOP)
      DO J = 1, NJ
        SUMQ(J)  = SUMQ(J)  + (SUMQ1(J)  - QIN(J))
        SUMQS(J) = SUMQS(J) + (SUMQS1(J) - QIN(J))
      ENDDO
C$OMP END CRITICAL
C$OMP CRITICAL (BOTTOM)
      DO J = 1, NJ
        SUMAL(J) = SUMAL(J) + SUMAL1(J)
        AS(J)    = AS(J)    + (AS1(J) - AMEN)
      ENDDO
C$OMP END CRITICAL
C$OMP END PARALLEL
C=======================================================================
C     SET HALF STEP OUTFLOWS AND INTERNAL TRANSFERS
C=======================================================================
      CALL BOUND(Y,YT,QT,TIME2,DELT2)
C
      DO 130 N  = NTC+1,NTL
      NL        = NJUNC(N,1)
CC$$$$5/3/92
      IF(ABS(QT(N)).LT.1E-10) QT(N) = 0.0
      SUMQ(NL)  = SUMQ(NL)  - 0.5*(QT(N)+QO(N))
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
      IF(Y(J)-YCROWN(J).LE.0.) THEN
           IF(AS(J).LE.0.0) THEN
                            IF(NERROR.LE.10) WRITE(N6,2400) ICYC,JUN(J)
                            NERROR = NERROR+1
                            YT(J)  = 0.0
                            GO TO 300
                            ENDIF
                            YT(J) = YO(J) + SUMQ(J)*DELT2/AS(J)
           IF(YT(J).LT.0.0) YT(J) = 0.0
           ASFULL(J) = AS(J)
C=======================================================================
C     WHEN JUNCTION SURCHARGES, 'ASFULL' WILL BE THE LAST
C          VALUE OF 'AS' UNDER FREE SURFACE FLOW
C     ADJUST HEAD AT SURCHARGED JUNCTIONS APPLY 1/2 OF COMPUTED CORRECTION
C     DECREASE THE HEAD CORRECTION FACTOR FOR UPSTREAM TERMINAL JUNCTION
C     COMPUTE THE CONVERGENCE CRITERIA FOR FLOW ERRORS IN SURCHARGED AREA
C=======================================================================
           ELSE
           DENOM = SUMAL(J)
           IF(Y(J).LT.1.25*YCROWN(J)) DENOM = SUMAL(J)+
     +   (ASFULL(J)/DELT2-SUMAL(J))*EXP(-15.*(Y(J)-YCROWN(J))/YCROWN(J))
                               CORR     = 0.50
           IF(NCHAN(J,2).EQ.0) CORR     = 0.30
           YT(J)                        = Y(J) + CORR*SUMQS(J)/DENOM
           IF(YT(J).LT.YCROWN(J)) YT(J) = YCROWN(J) - FUDGE
           ENDIF
C     HERE USE SURELEV IN PLACE OF GRELEV
           IF((YT(J)+Z(J)).GT.SURELEV(J)) YT(J) = SURELEV(J)-Z(J)
C=======================================================================
C     Calculate the allowable surcharge tolerance.
C=======================================================================
      QAVE     = 0.0
      DO 280 K = 1,NCHN
      IF(NCHAN(J,K).LE.0) GO TO 290
  280 QAVE = QAVE + ABS(Q(NCHAN(J,K)))
  290 K    = K-1
      QAVE = SURTOL*QAVE/FLOAT(K)
      TOL  = (TOL*(NSUR-1)+QAVE)/NSUR
      NSUR = NSUR+1
C=======================================================================
C     Initialize for full step flows.
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
CIM BEGIN   OOOOOO
CIM   FIRST COMPUTE GATED ORIFICE PARAMETERS
CIM   BASED ON HALFSTEP DEPTHS AT CONTROL NODE
      CALL OGATES(DELT2,YT,VT)
CIM END OOOOOOO
      DO 1000 KLOOP = 1,100000
      ERROR    = 0.0
CWRM SAVING THE PREVIOUS VALUES IMPROVES NUMERICAL CONSISTENCY
      DO J = 1, NJ
        SUMQL(J)  = SUMQ(J)
        SUMQSL(J) = SUMQS(J)
        SUMALL(J) = SUMAL(J)
        ASL(J) = AS(J)
      ENDDO
C$OMP PARALLEL
C$OMP& PRIVATE(N,NL,NH,HRAD,IDOIT,AKON,DQDH,ANH,ANL,HRAD,J)
C$OMP& PRIVATE(DELQ1,DELQ2,DELQ3,DELQ4,DELQ5,QNEW,DELH,DELZP,DIFF1)
C$OMP& PRIVATE(DIFF2,RNL,QNORM,ANH,RNH,DIRQ,DIRQT,SUMQ1,SUMQS1,SUMAL1)
C$OMP& PRIVATE(AS1)
C
CWRM CLEAR THE PARTIAL SUMS
CWRM STARTING PARTIAL SUMS AT PREVIOUS VALUE IMPROVES NUMERICAL CONSISTENCY
      DO J = 1, NJ
        SUMQ1(J)  = SUMQL(J)
        SUMQS1(J) = SUMQSL(J)
        SUMAL1(J) = SUMALL(J)
        AS1(J) = ASL(J)
      ENDDO
C
C$OMP DO SCHEDULE(GUIDED)
      DO 360 N = 1,NTC
      NL       = NJUNC(N,1)
      NH       = NJUNC(N,2)
C=======================================================================
C     CHECK WHETHER SURCHARGE ITERATIONS OCCURRING
C=======================================================================
      IF(IT.GT.0.AND.JCHECK(NH).EQ.IND(1).AND.JCHECK(NL).EQ.IND(1))
     +                                        GO TO 360
      H(N,1)    = AMAX1(YT(NL) + Z(NL),ZU(N))
      H(N,2)    = AMAX1(YT(NH) + Z(NH),ZD(N))
      CALL NHEAD(N,NL,NH,H(N,1),H(N,2),QT(N),AT(N),VT(N),HRAD,
     +                     ANH,ANL,RNL,RNH,IDOIT,LINK(N),AS1)
      IF(IDOIT.EQ.0) THEN
                     Q(N)  = 0.0
                     AKON  = 0.0
                     GO TO 995
                     ENDIF
c
c calculate additional loss terms per red incorporated by cim cdm 9/97
c
c    Q/ANH = velocity at downstream end used for exit loss
c    Q/ANL = velocity at upstream end used for entrance loss
c       The loss = 1/2*K*V^2/g is factored into momentum
c       equation similarly to the friction slope
c       The loss momentum term = g*A*[1/2*K*V^2/g]
c                              = g*A*[1/2*K*Q/A*Q/A/g]
C                               =g  *[1/2*K*|Q*A|  /g] * Q
C                               =    [1/2*K*|Q*A|    ] * Q
c     DELQ5 is sum of losses
      DELQ5 = 0.0
      IF (ANH.GT.0.0)   DELQ5 = DELQ5 + 0.5 * ABS(QT(N)/ANH)*ENTK(N)
      IF (ANL.GT.0.0)   DELQ5 = DELQ5 + 0.5 * ABS(QT(N)/ANL)*EXITK(N)
      IF (AT(N).GT.0.0) DELQ5 =  DELQ5 + 0.5*ABS(QT(N)/AT(N))*OTHERK(N)
      DELQ5 =  DELQ5 * DELT/LEN(N)
C
      DELQ4 =  DELT*VT(N)**2*(ANH-ANL)/LEN(N)
      DELQ3 =  4.0*VT(N)*(AT(N)-A(N))
      DELQ2 =  DELT*GRVT*AT(N)*(H(N,2)-H(N,1))/LEN(N)
      DELQ1 =  DELT*(ROUGH(N)/HRAD**1.33333)*ABS(VT(N))
      QNEW  =  QO(N) - DELQ2 + DELQ3 + DELQ4
      AKON  =  DELQ1 + DELQ5
      Q(N)  =  QNEW/(1.0+AKON)
C=======================================================================
C     CHECK FOR NORMAL FLOW - IF THE WATER SURFACE SLOPE IS LESS THAN
C                             THE CONDUIT SLOPE THE FLOW IS SUPERCRIT.
C=======================================================================
      ICHECK(N) = IND(1)
CIM START OOOOOOOO
CIM  ORIFICES
      IF(KSUPER.EQ.0.OR.NKLASS(N).GE.51.OR.JSKIP(NL).GT.0.
CIM END      OOOOOOO
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
CIM START  <><><><><><><>
CIM  ALL BUT ORIFICES
      IF(NKLASS(N).LE.50.AND.H(N,1).LE.ZCROWN(NL).AND.JSKIP(NL).EQ.0.
CIM END      <><><><><><>
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
      SELECT CASE (INGATE(N))
      CASE (1)
      Q(N) = AMAX1(Q(N),0.0)
c      IF(INGATE(N).EQ.1.AND.Q(N).LT.0.0) Q(N) = 0.0
      CASE (2)
      Q(N) = AMIN1(Q(N),0.0)
c      IF(INGATE(N).EQ.2.AND.Q(N).GT.0.0) Q(N) = 0.0
      END SELECT
      IF (NKLASS(N).LE.21) THEN
      Q(N) = AMAX1(STHETA(N),Q(N))
      Q(N) = AMIN1(SPHI(N),Q(N))
      ENDIF
  995 DQDH      = 1.0/(1.0+AKON)*GRVT*DELT*AT(N)/LEN(N)
      SUMQ1(NL)  = SUMQ1(NL)  - 0.5*(Q(N)+QO(N))
      SUMQS1(NL) = SUMQS1(NL) - Q(N)
      SUMAL1(NL) = SUMAL1(NL) + DQDH
      SUMQ1(NH)  = SUMQ1(NH)  + 0.5*(Q(N)+QO(N))
      SUMQS1(NH) = SUMQS1(NH) + Q(N)
      SUMAL1(NH) = SUMAL1(NH) + DQDH
  360 CONTINUE
C$OMP END DO NOWAIT
CWRM COMBINE THE PARTIAL SUMS
CWRM SPLIT CRITICAL SECTIONS REDUCES CONTENTION FOR LOCKS
C$OMP CRITICAL (TOP)
      DO J = 1, NJ
        SUMQ(J)  = SUMQ(J)  + (SUMQ1(J) - SUMQL(J))
        SUMQS(J) = SUMQS(J) + (SUMQS1(J) - SUMQSL(J))
      ENDDO
C$OMP END CRITICAL
C$OMP CRITICAL (BOTTOM)
      DO J = 1, NJ
        SUMAL(J) = SUMAL(J) + (SUMAL1(J) - SUMALL(J))
        AS(J)    = AS(J)    + (AS1(J) - ASL(J))
      ENDDO
C$OMP END CRITICAL
C$OMP END PARALLEL
C=======================================================================
C     SET FULL STEP OUTFLOWS AND INTERNAL TRANSFERS
C=======================================================================
      CALL BOUND(YT,Y,Q,TIME,DELT)
C
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
           IF(AS(J).LE.0.0) THEN
                            IF(NERROR.LE.10) WRITE(N6,2400) ICYC,JUN(J)
                            NERROR = NERROR+1
                            Y(J)   = 0.0
                            GO TO 560
                            ENDIF
           Y(J)      = YO(J) + SUMQ(J)*DELT/AS(J)
c     sum total volumes here and below
c      In an attempt to have program better
c      track nodal volumes.  
           VOL(J) = VOL(J) + SUMQ(J)*DELT
cend
           JCHECK(J) = IND(1)
C  CIMoore     10/98
C  If QIN is negative and computed Y is less than zero then
C  Adjust QIN appropriately. Fix for continuity when negative
C  flow is specified on D1 or K3 cards
c          IF(Y(J).LT.0.0) Y(J) = 0.0
          IF(Y(J).LT.0.0) THEN
          IF(QIN(J).LT.0.0) THEN
          FLOWNEG = Y(J)*AS(J)/DELT
c         write(n6,*) 'TESTING',J,Y(J),AS(J),DELT,FLOWNEG,QIN(J)
          QIN(J) = AMIN1(QIN(J)-FLOWNEG,0.0)
c         write(n6,*) qin(j)
          ENDIF
           Y(J) = 0.0
          END IF
           ASFULL(J) = AS(J)
C=======================================================================
C     ADJUST HEAD AT SURCHARGED JUNCTIONS APPLY 1/2 OF COMPUTED CORRECTION
C     DECREASE THE HEAD CORRECTION FACTOR FOR UPSTREAM TERMINAL JUNCTION
C     COMPUTE SURCHARGE FLOW ERROR IN JUNCTIONS NOT FLOODED
C=======================================================================
           ELSE
           DENOM = SUMAL(J)
           IF(YT(J).LT.1.25*YCROWN(J)) DENOM = SUMAL(J)+
     +   (ASFULL(J)/DELT-SUMAL(J))*EXP(-15.*(YT(J)-YCROWN(J))/YCROWN(J))
                               CORR   = 1.00
           IF(NCHAN(J,2).EQ.0) CORR   = 0.60
                              Y(J)    = YT(J)     + CORR*SUMQS(J)/DENOM
c     sum total volumes here
           VOL(J) = VOL(J) + CORR*SUMQS(J)*DELT
cend
           IF(Y(J).LT.YCROWN(J)) Y(J) = YCROWN(J) - FUDGE
           JCHECK(J) = IND(2)
C     HERE USE SURELEV IN PLACE OF GRELEV
           IF((Y(J)+Z(J)).LT.SURELEV(J)) ERROR = ERROR + SUMQS(J)
           ENDIF
           IF((Y(J)+Z(J)).GT.SURELEV(J)) Y(J)  = SURELEV(J)-Z(J)
  560 CONTINUE
C=======================================================================
C     CHECK CONVERGENCE OF THE FLOW ERROR IN SURCHARGED AREAS
C                       OR INITIALIZE FOR NEXT ITERATION
C=======================================================================
      IF(ABS(ERROR)-TOL.LE.0.0.OR.IT-ITMAX+1.GT.0.0) RETURN
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
C=======================================================================
 2400 FORMAT(' ====> WARNING !!!! ICYC=',I5,'  ZERO SURFACE AREA COMPUTE
     .D AT JUNCTION',I6,'  CHECK INPUT DATA FOR HIGH PIPE.')
C=======================================================================
      RETURN
      END

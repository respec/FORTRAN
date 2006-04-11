      SUBROUTINE ZROUTE(FAIL)
C	EXTRAN BLOCK
C	CALLED BY TRANSX NEAR LINE 672
C=======================================================================
C     THIS IS SUBROUTINE ZROUTE OF THE SEWER MODEL
C     IT PERFORMS THE ITERATIVE UNDER-RELAXATION SOLUTION OF THE
C     MOTION AND CONTINUITY EQUATIONS
C     LAST UPDATED JANUARY, 1989
C     WCH, 8/7/95.  CHECK FOR ZERO DEPTH IN COMPUTING SQUARED FROUDE
C       NUMBERS.  NOTE!  THIS CODE QUITE DIFFERENT NOW FROM XP CODE.
C     WCH, 7/25/96.  CORRECT THE FROUDE NUMBER CALCULATION.
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
	INCLUDE 'TRANAID.INC'
      INTEGER  HSKIP(NEE),QSKIP
      DIMENSION AOLD(NEE),DEL(NEE,3),Y0(NEE),Q0(NEE),AI(NEE)
      LOGICAL FAIL
      EQUIVALENCE (AOLD(1),ASFULL(1)),(Y0(1),SUMAL(1)),(Q0(1),SUMQS(1))
      DATA ILOOP/0/
C=======================================================================
C     STORE OLD TIME STEP FLOW VALUES
C=======================================================================
      ILOOP    = ILOOP + 1
      ERROR    = 1.0
      IF(ILOOP.EQ.1) THEN
                     OLDFLW  = 1.0
                     OLDDEP  = 1.0
                     WD      = 0.60
                     WT      = 0.40
                     ENDIF
      DO 80 J  = 1,NJ
      Y0(J)    = Y(J)
      YO(J)    = Y(J)
      HSKIP(J) = 0
      SUMQ(J)  = QIN(J)
      AOLD(J)  = AS(J)
   80 AS(J)    = AMEN
C=======================================================================
      DO 90 N  = 1,NTL
      AT(N)    = 0.0
      Q0(N)    = Q(N)
   90 QO(N)    = Q(N)
      FAIL     = .FALSE.
      IF(NPUMP.GT.0) THEN
                     DO 95 I  = 1,NPUMP
  95                 VWBEG(I) = VWELL(I)
                     ENDIF
C=======================================================================
C     ITERATION LOOP
C=======================================================================
 2000 CONTINUE
      IF(IT.LE.1)        OMEGA = 0.75
      IF(IT.GE.2)        OMEGA = 0.50
CIM START  OOOOO
CIM        SET FULLSTEP GATED ORIFICE PARAMETERS
CIM        FLOWS COMPUTED WILL BE BASED ON FULLSTEP DEPTHS (Y) AT CONTROL NODE
      CALL OGATES(DELT,Y,V)
CIM END  OOOOOO
C=======================================================================
C     FIND THE NEW CONDUIT FLOWS
C=======================================================================
      DO 360 N = 1,NTC
      NL       = NJUNC(N,1)
      NH       = NJUNC(N,2)
      H(N,1)    = AMAX1(Y(NL) + Z(NL),ZU(N))
      H(N,2)    = AMAX1(Y(NH) + Z(NH),ZD(N))
      CALL NHEAD(N,NL,NH,H(N,1),H(N,2),Q(N),A(N),V(N),HRAD,
     +                      ANH,ANL,RNL,RNH,IDOIT,LINK(N),AS)
      IF(IDOIT.EQ.0)   THEN
                       Q(N)  = 0.0
                       GO TO 995
                       ENDIF
      IF(AT(N).GT.0.0) THEN
                       Q(N)  = AT(N)
                       GO TO 995
                       ENDIF
C=======================================================================
C     CALCULATE THE FLOW FROM THE FULL DYNAMIC EQUATION FOR FROUDE # <1
C=======================================================================
      QQ       = Q(N)
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
      IF (ANH.GT.0.0) DELQ5 =  DELQ5 + 0.5 * ABS(QO(N)/ANH)*ENTK(N)
      IF (ANL.GT.0.0) DELQ5 =  DELQ5 + 0.5 * ABS(QO(N)/ANL)*EXITK(N)
      IF (A(N).GT.0.0)DELQ5 =  DELQ5 + 0.5 * ABS(QO(N)/A(N))*OTHERK(N)
      DELQ5 =  DELQ5 * DELT/LEN(N)
C
      DELQ4    = DELT*Q(N)*(1.0/ANH-1.0/ANL)/LEN(N)
      DELQ2    = DELT*A(N)*GRVT*(H(N,2)-H(N,1))/LEN(N)
      DELQ1    = DELT*(ROUGH(N)/HRAD**1.33333)*ABS(V(N))
      IF(IT.LE.1) THEN
                  IF(HRAD.GT.HMAX(N)) HMAX(N) = HRAD
                  IF(A(N).GT.AMAX(N)) AMAX(N) = A(N)
c??
                  DEL(N,1) = (DELQ1+DELQ5)*QO(N)
                  DEL(N,2) = DELQ2
                  DEL(N,3) = DELQ4*QO(N)
                  VT(N)    = V(N)
                  AI(N)    = A(N)
                  ENDIF
CC&&&&&&&      VAVE     = VT(N)       + V(N)
      VAVE     = Q(N)/ANH + Q(N)/ANL
      QNEW     = QO(N)       - DELQ2*WD    - DEL(N,2)*WT    -
     +           DEL(N,1)*WT - DEL(N,3)*WT + 0.5*VAVE*(A(N) - AI(N))
      AKON     = DELQ1*WD    + DELQ4*WD + DELQ5*WD
      Q(N)     = QNEW/(1.0   + AKON)
C=======================================================================
C     CALCULATE THE MAXIMUM SQUARED FROUDE NUMBER FOR THE CONDUIT
C=======================================================================
      DIFF1 = 0.0
      DIFF2 = 0.0
C#### WCH, 8/7/95.  ADD ADDITIONAL ERROR CHECK FOR ZERO DEPTH.
C     ALSO, VERY LOW FLOWS SHOULD BE SUB-CRITICAL WITH SMALL
C     SQUARED FROUDE NUMBER.
C#### WCH, 7/25/96.  CLEARLY A FROUDE NUMBER CALC. ERROR BELOW.
      IF(ANL.GT.FUDGE) THEN
                       DIFF1 = H(N,1) - ZU(N)
                       IF(DIFF1.GT.FUDGE) THEN
C#### WCH, 7/25/96                            CSQ = GRVT*ANL/DIFF1
                            FF1 = 0.1
                            CALL HYDRAD(N,NKLASS(N),DIFF1,HRAD1,ANL,BNL)
                            IF(ANL.GT.0.0.AND.BNL.GT.0.0) THEN
                                 CSQ = GRVT*ANL/BNL
C####                         FF1   = (Q(N)/ANL)**2/(GRVT*DIFF1)
                                 FF1 = (Q(N)/ANL)**2/CSQ
                                 ENDIF
                            ELSE
                            FF1 = 0.1
                            ENDIF
                       ELSE
C####                       FF1   = 1.01
                       FF1 = 0.1
                       ENDIF
      IF(ANH.GT.FUDGE) THEN
                       DIFF2 = H(N,2) - ZD(N)
                       IF(DIFF2.GT.FUDGE) THEN
C#### WCH, 7/25/96                            CSQ = GRVT*ANH/DIFF2
                            FF2 = 0.1
                            CALL HYDRAD(N,NKLASS(N),DIFF2,HRAD2,ANH,BNH)
                            IF(ANH.GT.0.0.AND.BNH.GT.0.0) THEN
                                 CSQ = GRVT*ANH/BNH
C####                         FF2   = (Q(N)/ANH)**2/(GRVT*DIFF2)
                                 FF2 = (Q(N)/ANH)**2/CSQ
                                 ENDIF
                            ELSE
                            FF2 = 0.1
                            ENDIF
                       ELSE
C####                       FF2   = 1.01
                       FF2 = 0.1
                       ENDIF
      FF   = AMAX1(FF1,FF2)
      DIFF = AMAX1(DIFF1,DIFF2)
C=======================================================================
C     CHECK FOR NORMAL FLOW - IF THE WATER SURFACE SLOPE IS LESS THAN
C                             THE CONDUIT SLOPE THE FLOW IS SUPERCRITAL.
C=======================================================================
      ICHECK(N) = IND(1)
CIM BEGIN   OOOOOO
CIM  ORIFICES
      IF(KSUPER.EQ.0.OR.NKLASS(N).GE.51.OR.JSKIP(NL).GT.0.
CIM END      OOOOOOO
     +                                 OR.JSKIP(NH).GT.0) THEN
           IF(Q(N).GT.0.0)                                THEN
                IF(DIFF.LT.1.25*DEEP(N))                  THEN
                     DELH      = H(N,1)  - H(N,2)
                     DELZP     = ZU(N)   - ZD(N)
                     IF(DELH-DELZP.LT.0.0) THEN
                            QNORM = SQRT(GRVT*(ZU(N)-ZD(N))/
     +                            (LEN(N)*ROUGH(N)))*ANL*RNL**0.666667
                            IF(QNORM.LE.Q(N)) THEN
                                              ICHECK(N) = IND(2)
                                              Q(N)      = QNORM
                                              ENDIF
                            ENDIF
                     ENDIF
                ENDIF
           ENDIF
C=======================================================================
C     CHECK FOR NORMAL FLOW - IF THE FROUDE NUMBER IS > 1.0
C                             THE FLOW IS SUPERCRITICAL.
C=======================================================================
      IF(KSUPER.EQ.1.AND.FF.GE.1.0) THEN
           IF(DIFF.LT.1.25*DEEP(N))      THEN
CIM BEGIN  <><><><><><><>
CIM    ALL BUT ORIFICES
                IF(NKLASS(N).LE.51.AND.JSKIP(NH).EQ.0.AND.
CIM END    <><><><><><><>
     +                   JSKIP(NL).EQ.0) THEN
                     IF(Q(N).GT.0.0) THEN
                          QNORM = SQRT(GRVT*(ZU(N)-ZD(N))/
     +                        (LEN(N)*ROUGH(N)))*ANL*RNL**0.666667
                          IF(QNORM.LT.Q(N)) THEN
                                        ICHECK(N) = IND(2)
                                        Q(N)      = QNORM
                                        ENDIF
                          ENDIF
                     ENDIF
                ENDIF
           ENDIF
C=======================================================================
C     DO NOT ALLOW A FLOW REVERSAL IN ONE TIME STEP
C=======================================================================
      DIRQT = SIGN(1.0,QO(N))
      DIRQ  = SIGN(1.0,Q(N))
      IF(DIRQT/DIRQ.LT.0.0) Q(N) = 0.001*DIRQ
C=======================================================================
C     COMPUTE CONTINUITY PARAMETERS
C=======================================================================
                        TEST = ABS(Q0(N)-Q(N))/QFULL(N)
      IF(JCHECK(NL).EQ.IND(2).OR.JCHECK(NH).EQ.IND(2)) TEST = 1.0
      IF(TEST.GT.0.001) Q(N) = (1.0-OMEGA)*QQ  + OMEGA*Q(N)
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
C=======================================================================
  995 SUMQ(NL)  = SUMQ(NL)  - (Q(N)+QO(N))/2.0
      SUMQ(NH)  = SUMQ(NH)  + (Q(N)+QO(N))/2.0
  360 CONTINUE
C=======================================================================
C     SET FULL STEP OUTFLOWS AND INTERNAL TRANSFERS
C=======================================================================
      CALL BOUND(Y,Y,Q,TIME,DELT)
C=======================================================================
C     DO NOT ALLOW FLOW REVERSAL IN ONE TIME STEP
C=======================================================================
      DO 370 N = NTC+1,NTL
      NL       = NJUNC(N,1)
      NH       = NJUNC(N,2)
CC$$$$$$$5/3/92
      IF(ABS(Q(N)).LT.1E-10) Q(N)=0.0
      DIRQT    = SIGN(1.0,QO(N))
      DIRQ     = SIGN(1.0,Q(N))
      IF(DIRQT/DIRQ.LT.0.0) Q(N) = 0.001*DIRQ
      SUMQ(NL)  = SUMQ(NL)  - Q(N)/2.0 - QO(N)/2.0
      IF(NH.GT.0) SUMQ(NH)  = SUMQ(NH) +  Q(N)/2.0 + QO(N)/2.0
 370  CONTINUE
C=======================================================================
C     CALCULATE NEW HEAD FOR THIS ITERATION
C=======================================================================
      DO 560 J = 1,NJ
      IF(JSKIP(J).GT.0)  GO TO 560
      ZZ        = Y(J)
      DENOM     = AS(J)/2.0 +  AOLD(J)/2.0
      Y(J)      = YO(J)     +  SUMQ(J)*DELT/DENOM
C SUM TO NODAL VOLUME HERE
	VOL(J)   = VOL(J)   +  SUMQ(J)
      TEST      = ABS(Y0(J)-Y(J))/YCROWN(J)
      IF(JCHECK(J).EQ.IND(2)) TEST = 1.0
      IF(TEST.GT.0.001) Y(J) = (1.0-OMEGA)*ZZ + OMEGA*Y(J)
      IF(Y(J).LT.0.0)              Y(J)      = FUDGE
C     HERE USE SURELEV IN PLACE OF GRELEV
      IF((Y(J)+Z(J)).GT.SURELEV(J)) Y(J)      = SURELEV(J)-Z(J)
      IF(Y(J).LT.1.25*YCROWN(J).AND.ZZ.GT.1.25*YCROWN(J))
     +                                  Y(J) = 1.25*YCROWN(J) - FUDGE
      IF(Y(J).GT.1.25*YCROWN(J).AND.ZZ.LT.1.25*YCROWN(J))
     +                                  Y(J) = 1.25*YCROWN(J) + FUDGE
      IF(Y(J).LT.YCROWN(J))        JCHECK(J) = IND(1)
      IF(Y(J).GE.YCROWN(J))        JCHECK(J) = IND(2)
  560 CONTINUE
C=======================================================================
C     CHECK CONVERGENCE OF THE FLOW ERROR IN SURCHARGED AREAS
C                       OR INITIALIZE FOR NEXT ITERATION
C=======================================================================
      E2       = 0.0
      TOTDEP   = 0.0
      DO 570 J = 1,NJ
      HSKIP(J) = 0
      TOTDEP   = TOTDEP + Y(J)
      ERROR    = ABS(Y0(J)-Y(J))/YCROWN(J)
      IF(ERROR.GT.E2)          E2  = ERROR
      IF(ERROR.LE.SURTOL) HSKIP(J) = 1
      Y0(J)    = Y(J)
  570 CONTINUE
C=======================================================================
C     CALCULATE THE MAXIMUM FLOW ERROR FOR CONDUITS
C=======================================================================
      SUMFLW   = 0.0
      E1       = 0.0
      IWAT     = 0
      DO 580 N = 1,NTC
      QSKIP    = 0
      ERROR    = ABS(Q0(N)-Q(N))/QFULL(N)
      IF(ERROR.GT.E1)  E1 = ERROR
      SUMFLW              = SUMFLW + ABS(Q(N))
      IF(ERROR.LE.SURTOL)   QSKIP = 1
      NL                  = NJUNC(N,1)
      NH                  = NJUNC(N,2)
      IF(QSKIP.GT.0.AND.HSKIP(NL).GT.0.AND.HSKIP(NH).GT.0)IWAT=IWAT+1
      Q0(N) = Q(N)
  580 CONTINUE
C=======================================================================
      ERROR  = AMAX1(E1,E2)
C=======================================================================
C     RETURN WHEN CONVERGENCE CRITERIA ARE SATISFIED
C=======================================================================
      IF(IWAT.GE.NTC.OR.ABS(ERROR).LE.SURTOL) RETURN
      IF(ABS(TOTDEP-OLDDEP).LE.FUDGE
     +                   .AND.ABS(SUMFLW-OLDFLW).LE.FUDGE) THEN
                          ERROR    = 0.0
                          RETURN
                          ENDIF
      OLDDEP   = TOTDEP
      OLDFLW   = SUMFLW
C=======================================================================
C     RETURN WHEN CONVERGENCE CRITERIA ARE NOT SATISFIED
C=======================================================================
      IF(IT.EQ.ITMAX) THEN
                      FAIL = .TRUE.
                      RETURN
                      ENDIF
      IT       = IT+1
      DO 590 J = 1,NJ
      SUMQ(J)  = QIN(J)
      AS(J)    = AMEN
  590 CONTINUE
      GO TO 2000
      END

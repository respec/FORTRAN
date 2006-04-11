      SUBROUTINE ZROUTE(FAIL)
C=======================================================================
C     THIS IS SUBROUTINE ZROUTE OF THE SEWER MODEL
C     IT PERFORMS THE ITERATIVE UNDER-RELAXATION SOLUTION OF THE
C     MOTION AND CONTINUITY EQUATIONS
C     LAST UPDATED JANUARY, 1989
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
C=======================================================================
C     FIND THE NEW CONDUIT FLOWS
C=======================================================================
      DO 360 N = 1,NTC
      NL       = NJUNC(N,1)
      NH       = NJUNC(N,2)
      H(N,1)   = Y(NL) + Z(NL)
      H(N,2)   = Y(NH) + Z(NH)
      CALL HEAD(N,NL,NH,H(N,1),H(N,2),Q(N),A(N),V(N),HRAD,
     +                      ANH,ANL,RNL,RNH,IDOIT,LINK(N))
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
      DELQ4    = DELT*Q(N)*(1.0/ANH-1.0/ANL)/LEN(N)
      DELQ2    = DELT*A(N)*GRVT*(H(N,2)-H(N,1))/LEN(N)
      DELQ1    = DELT*(ROUGH(N)/HRAD**1.33333)*ABS(V(N))
      IF(IT.LE.1) THEN
                  IF(HRAD.GT.HMAX(N)) HMAX(N) = HRAD
                  IF(A(N).GT.AMAX(N)) AMAX(N) = A(N)
                  DEL(N,1) = DELQ1*QO(N)
                  DEL(N,2) = DELQ2
                  DEL(N,3) = DELQ4*QO(N)
                  VT(N)    = V(N)
                  AI(N)    = A(N)
                  ENDIF
CC&&&&&&&      VAVE     = VT(N)       + V(N)
      VAVE     = Q(N)/ANH + Q(N)/ANL
      QNEW     = QO(N)       - DELQ2*WD    - DEL(N,2)*WT    -
     +           DEL(N,1)*WT - DEL(N,3)*WT + 0.5*VAVE*(A(N) - AI(N))
      AKON     = DELQ1*WD    + DELQ4*WD
      Q(N)     = QNEW/(1.0   + AKON)
C=======================================================================
C     CALCULATE THE MAXIMUM FROUDE NUMBER FOR THE CONDUIT
C=======================================================================
      DIFF1 = 0.0
      DIFF2 = 0.0
      IF(ANL.GT.FUDGE) THEN
                       DIFF1 = H(N,1) - ZU(N)
                       FF1   = (Q(N)/ANL)**2/(GRVT*DIFF1)
                       ELSE
                       FF1   = 1.01
                       ENDIF
      IF(ANH.GT.FUDGE) THEN
                       DIFF2 = H(N,2) - ZD(N)
                       FF2   = (Q(N)/ANH)**2/(GRVT*DIFF2)
                       ELSE
                       FF2   = 1.01
                       ENDIF
      FF             = AMAX1(FF1,FF2)
      DIFF           = AMAX1(DIFF1,DIFF2)
C=======================================================================
C     CHECK FOR NORMAL FLOW - IF THE WATER SURFACE SLOPE IS LESS THAN
C                             THE CONDUIT SLOPE THE FLOW IS SUPERCRITAL.
C=======================================================================
      ICHECK(N) = IND(1)
      IF(KSUPER.EQ.0.OR.NKLASS(N).GE.9.OR.JSKIP(NL).GT.0.
     +                                 OR.JSKIP(NH).GT.0) THEN
      IF(Q(N).GT.0.0)                                     THEN
      IF(DIFF.LT.1.25*DEEP(N))                            THEN
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
      IF(NKLASS(N).LE.8.AND.JSKIP(NH).EQ.0.AND.JSKIP(NL).EQ.0) THEN
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
      IF(INGATE(N).EQ.1.AND.Q(N).LT.0.0) Q(N) = 0.0
      IF(INGATE(N).EQ.2.AND.Q(N).GT.0.0) Q(N) = 0.0
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
      TEST      = ABS(Y0(J)-Y(J))/YCROWN(J)
      IF(JCHECK(J).EQ.IND(2)) TEST = 1.0
      IF(TEST.GT.0.001) Y(J) = (1.0-OMEGA)*ZZ + OMEGA*Y(J)
      IF(Y(J).LT.0.0)              Y(J)      = FUDGE
      IF((Y(J)+Z(J)).GT.GRELEV(J)) Y(J)      = GRELEV(J)-Z(J)
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

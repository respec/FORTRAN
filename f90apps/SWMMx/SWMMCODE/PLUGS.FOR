      SUBROUTINE PLUGS(I,NPL,VLIN,VOUT,KKDT,LPREV,FRAC,LP,IER)
C=======================================================================
C     SUBROUTINE TO CALCULATE PLUG FLOW.
C=======================================================================
      DIMENSION KLPREV(5),VLIN(5,NPL)
C=======================================================================
  100 IF(KKDT.LE.1)      KLPREV(I) = 1
      IF(KLPREV(I).LE.0) KLPREV(I) = 1
      INX = 0
      LP  = KKDT
      JP  = KLPREV(I)
      IER = 1
      IF(LP-JP.GE.NPL) RETURN
      IER = 0
      IF(VOUT.LE.0.0)  RETURN
C
  200 IF(JP.LE.NPL) GO TO 300
      JP  = JP-NPL
      LP  = LP-NPL
      INX = INX+1
      GO TO 200
C=======================================================================
C     SUM THE INFLOWS AND BRANCH WHEN THE TOTAL IS GREATER THAN
C     THE OUTFLOW FOR THIS TIME STEP.
C=======================================================================
  300 SUM      = 0.0
      DO 350 L = JP,LP
      LT       = L
      LPREV    = L
      IF(L.GT.NPL) LT =   L - NPL
      SUM             = SUM + VLIN(I,LT)
      IF(VOUT.LE.SUM) GO TO 400
  350 CONTINUE
C=======================================================================
C     COMPUTE THE FRACTION OF THE LAST PLUG THAT IS REQUIRED TO
C     MAINTAIN CONTINUITY.
C=======================================================================
  400 EXTRA = SUM - VOUT
      FRAC  = 1.0
      IF(VLIN(I,LT).GT.0.0) FRAC = 1.0 - EXTRA/VLIN(I,LT)
      KLPREV(I) = LPREV + INX*NPL
      RETURN
      END

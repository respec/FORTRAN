      SUBROUTINE PINE(X1,Y1,X2,Y2,NSYM,NPLOT,ALPLOT)
      CHARACTER ALPLOT*10
C=======================================================================
      AXA = X1
      AXB = X2
      AYA = Y1
      AYB = Y2
      IF((AXB.EQ.AXA).AND.(AYB.EQ.AYA)) RETURN
      D = 1.0
      IF(ABS(AXB-AXA).LT.ABS(AYB-AYA)) GO TO 40
C=======================================================================
C     SET PARAMETERS FOR X DIRECTION
C=======================================================================
      IF(AXB.GT.AXA) GO TO 10
      AXA = X2
      AXB = X1
      AYA = Y2
      AYB = Y1
   10 CONTINUE
      IXA = AXA+.5
      IXB = AXB+.5
      IYA = AYA+.5
      IYB = AYB+.5
   20 CONTINUE
      IF(IXA.LT.0.OR.IXA.GT.100) GO TO 30
      IF(IYA.LT.0.OR.IYA.GT.50) GO TO 30
C=======================================================================
      CALL PPLOT(IXA,IYA,NSYM,NPLOT,ALPLOT)
C=======================================================================
   30 CONTINUE
      IXA = IXA+1
      YA  = (D*(AYB-AYA))/(AXB-AXA)
      IYA = AYA+YA+0.5
      D   = D + 1.0
      IF(IXA.LE.IXB) GO TO 20
      GO TO 90
C=======================================================================
C     SET PARAMETERS FOR Y DIRECTION
C=======================================================================
   40 CONTINUE
      IF(AYB.GT.AYA)   GO TO 50
      AYB = Y1
      AYA = Y2
      AXB = X1
      AXA = X2
   50 CONTINUE
      IXA = AXA+.5
      IXB = AXB+.5
      IYA = AYA+.5
      IYB = AYB+.5
   60 CONTINUE
      IF(IXA.LT.0.OR.IXA.GT.100) GO TO 70
      IF(IYA.LT.0.OR.IYA.GT.50) GO TO 70
C=======================================================================
      CALL PPLOT(IXA,IYA,NSYM,NPLOT,ALPLOT)
C=======================================================================
   70 CONTINUE
      IYA = IYA+1
      XA  = (D*(AXB-AXA))/(AYB-AYA)
      IXA = XA+AXA+0.5
      D   = D + 1.0
      IF(IYA-IYB) 60,80,90
   80 IXA = IXB
      GO TO 60
   90 RETURN
      END

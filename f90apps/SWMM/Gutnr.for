      SUBROUTINE GUTNR(J,GLEN,GD,RDELT,NPG,WTYPE,WELEV,WDIS,SPILL,
     +           GS1,GS2,GWIDTH,GCON,DFULL,QIN,QSUR,GFLOW,OUTFLW,
     +           AXZERO,AX,FLZERO)
C     RUNOFF BLOCK
C     CALLED BY GUTTER NEAR LINE 26
C=======================================================================               
C     NEWTON-RAPHSON SOLUTION OF GUTTER OR PIPE DEPTH
C     LAST UPDATED 4/22/92 BY RED AND WCH.
C     UPDATED 9/23/93 BY WCH (RED) TO FIX IF-STMT NEAR END.
C     WCH (CDM), 8/13/96.  REMOVE ERRANT CONVERGENCE CHECK FOR CHANNELS
C       WITH WEIRS/ORIFICES.  
C=======================================================================
      INCLUDE  'TAPES.INC'
      INCLUDE  'NEW88.INC'
      DIMENSION NSEQ(4),YZ(4),DZ(4),DY(4)
      INTEGER   WTYPE
      DATA NSEQ/1,2,4,8/
C=======================================================================
      NUSE     = 4
      KCHAN(J) = KCHAN(J) + 1
C=======================================================================
C     MAIN LOOP
C=======================================================================
      DO 400 JJ= 1,NUSE
      GDEPTH   = GD
      DELT     = RDELT/FLOAT(NSEQ(JJ))
      DELD     = 0.0
      DO 250 K = 1,NSEQ(JJ)
      GDEPTH   = GDEPTH + DELD
      IFLG     = 0
      IFLAG2   = 0
      DELD     = 0.0
      D0       = GDEPTH
      AX0      = 0.0
      WP0      = 0.001
C=======================================================================
C     TRAPEZOIDAL PARAMETERS
C=======================================================================
      IF(NPG.EQ.1) THEN
         AX0 = 0.5*(GS1+GS2)*D0**2.0 + GWIDTH*D0
         WP0 = SQRT(GS1**2.0+1.0)*D0 + SQRT(GS2**2.0+1.0)*D0 + GWIDTH
         ENDIF
C=======================================================================
C     CIRCULAR PARAMETERS
C=======================================================================
      IF(NPG.EQ.2) THEN
                   SIN2D0  = 0.5*SIN(2.0*D0)
                   AX0     = GWIDTH**2*(D0-SIN2D0)/4.0
                   WP0     = GWIDTH*D0
                   D1      = 1.5707963
                   DELD    = D1 - GDEPTH
                   ENDIF
C=======================================================================
C     PARABOLIC PARAMETERS
C=======================================================================
      IF(NPG.EQ.4.AND.GDEPTH.GT.0.0) THEN
                      WIDTH = GWIDTH*SQRT(D0/DFULL)
                      X     = WIDTH/2.0
                      A2    = GWIDTH**4.0/(64.0*DFULL**2.0)
                      WP0   = 8.0*DFULL/GWIDTH**2.0*(X*SQRT(A2+X**2.0) + 
     +                                       A2*LOG(X+SQRT(A2+X**2.0)) - 
     +                                       A2*LOG(SQRT(A2)))       
                      AX0   = 0.66666667*WIDTH*D0
                      ENDIF
C=======================================================================
      IF(AX0.LT.0.001) AX0 = 0.0
      IF(WP0.LE.0.001) WP0 = 0.001
      RAD                  = AX0/WP0
C=======================================================================
      FLOW0 = 0.0
      IF(WTYPE.EQ.-1.AND.RAD.GT.0.0) FLOW0 = GCON*AX0*RAD**0.666666667
      IF(WTYPE.EQ.0.AND.D0.GT.WELEV) FLOW0 = WDIS*SPILL*(D0-WELEV)**1.5
      IF(WTYPE.EQ.1.AND.D0.GT.WELEV) FLOW0 = WDIS*SPILL*(D0-WELEV)**2.5
      IF(WTYPE.EQ.2.AND.D0.GT.WELEV) FLOW0 = WDIS*SPILL*
     +                                       (2.0*32.2*(D0-WELEV))**0.5
      IF(JJ.EQ.1) THEN
                  FLZERO = FLOW0
                  AXZERO = AX0
                  ENDIF
C=======================================================================
C     COMPUTE CHANGE IN DEPTH (NEWTON-RAPHSON)
C
C     D1 = ESTIMATED FINAL DEPTH.
C          FOR PIPES, 'DEPTH' IS HALF OF ANGLE SUBTENDED BY
C          WETTED PERIMETER, IN RADIANS.
C=======================================================================
                        KSOL = 0
      IF(DELT.GE.900.0) KSOL = 1
      DO 210 I = 1,20
      D1       = GDEPTH+DELD
C=======================================================================
C     TRAPEZOIDAL CHANNEL
C=======================================================================
      IF(NPG.EQ.1) THEN
             IF(D1.LE.0.0) THEN
                           IFLAG2 = IFLAG2 + 1
                           D1     = 0.0
                           DELD   =  -GDEPTH
                           ENDIF
             DELV = GLEN*DELD * ((GS1+GS2)*(D0 + 0.5 * DELD)
     +                                         + GWIDTH)/DELT
             DDELV = GLEN*((GS1+GS2)*D1  + GWIDTH)/DELT
             AX    = 0.5*(GS1+GS2)*D1**2 + GWIDTH*D1
             DAX   = (GS1+GS2)*D1+GWIDTH
             WP    = SQRT(GS1**2+1.0)*D1+SQRT(GS2**2+1.0)*D1+GWIDTH
             DWP   = SQRT(GS1**2+1.0)+SQRT(GS2**2+1.0)
             ENDIF
C=======================================================================
C     CIRCULAR PIPE
C=======================================================================
      IF(NPG.EQ.2) THEN
                   IF(I.EQ.1) THEN
                              D1   = 1.5707963
                              DELD = D1 - GDEPTH
                              ENDIF
                   IF(D1.LE.0.0) THEN
                                 IFLAG2 = IFLAG2 + 1
                                 D1     = 0.0
                                 DELD   = -GDEPTH
                                 ENDIF
                   IF(D1.GT.DFULL) THEN
                                   D1   = DFULL
                                   DELD = D1 - GDEPTH
                                   ENDIF
                   SIN2D1 = 0.5*SIN(2.0*D1)
                   COS2D1 = COS(2.0*D1)
                   DELV   = .25*GLEN*GWIDTH**2*(DELD-SIN2D1+SIN2D0)/DELT
                   DDELV  = .25*GLEN*GWIDTH**2*(1.0-COS2D1)/DELT
                   AX     = GWIDTH**2*(D1-SIN2D1)/4.0
                   DAX    = GWIDTH**2*(1.0-COS2D1)/4.0
                   WP     = GWIDTH*D1
                   DWP    = GWIDTH
                   ENDIF
C=======================================================================
C     PARABOLIC CHANNEL
C=======================================================================
      IF(NPG.EQ.4) THEN
             IF(D1.LE.0.0) THEN
                           IFLAG2 = IFLAG2 + 1
                           D1     = 0.001
                           DELD   =  -GDEPTH + 0.001
                           ENDIF
             WID0  = GWIDTH*SQRT(D0/DFULL)
             WID1  = GWIDTH*SQRT(D1/DFULL)
             DELV  = 0.66666667*GLEN/DELT*(D1*WID1 - D0*WID0)
             DDELV = 0.66666667*GLEN/DELT*1.50*WID1
             AX    = 0.66666667*WID1*D1
             DAX   = 0.66666667*WID1*1.50
             X     = WID1/2.0
             A2    = GWIDTH**4.0/(64.0*DFULL**2.0)
             WP    = 8.0*DFULL/GWIDTH**2.0*(X*SQRT(A2+X**2.0) + 
     +                              A2*LOG(X+SQRT(A2+X**2.0)) - 
     +                              A2*LOG(SQRT(A2)))       
                           DX  = 0.0
             IF(D1.GT.0.0) DX  = 0.25*GWIDTH/SQRT(D1*DFULL)
             DXX   = 0.25*GWIDTH**2.0/DFULL
             DWP   = 8.0*DFULL/GWIDTH**2.0*(DX*SQRT(A2+X**2.0) +
     +               0.5*X*DXX/SQRT(A2+X**2.0) + 
     +         (DX+0.5*DXX/SQRT(A2+X**2.0))/(LOG(X+SQRT(A2+X**2.0))))
             ENDIF
C=======================================================================
C     HYDRAULIC RADIUS (ALL CROSS-SECTIONS)
C=======================================================================
      IF(AX.LE.0.001) AX = 0.000
      IF(WP.LE.0.001) WP = 0.001
      RAD                = AX/WP
C=======================================================================
C     FLOW1 = INSTANTANEOUS FLOW AT END OF TIME STEP.
C=======================================================================
                      FLOW1 = 0.0
CWCH, 7/16/99.  SEEM TO HAVE TROUBLE WITH ZERO TO A POWER, SO CHECK.
      IF(WTYPE.EQ.-1.AND.RAD.GT.0.0) FLOW1 = GCON*AX*RAD**0.666666667 
	IF(WTYPE.EQ.0.AND.D1.GT.WELEV) FLOW1 = WDIS*SPILL*(D1-WELEV)**1.5
      IF(WTYPE.EQ.1.AND.D1.GT.WELEV) FLOW1 = WDIS*SPILL*(D1-WELEV)**2.5
      IF(WTYPE.EQ.2.AND.D1.GT.WELEV) FLOW1 = WDIS*SPILL*
     +                                       (2.0*32.2*(D1-WELEV))**0.5
      FLOW   = 0.5*(FLOW1+FLOW0)
      DFLOW  = 0.0
CWCH, 7/16/99.  SEEM TO HAVE TROUBLE WITH ZERO TO A POWER, SO CHECK.
      IF(WTYPE.EQ.-1.AND.RAD.GT.0.0) DFLOW =  0.5*GCON*(1.6666667*
     +    (RAD**0.66666667)*DAX - 0.66666667*(RAD**1.6666667)*DWP)
      IF(WTYPE.EQ.0.AND.D1.GT.WELEV) DFLOW = 1.5*WDIS*SPILL*
     +                                       (D1-WELEV)**0.5
      IF(WTYPE.EQ.1.AND.D1.GT.WELEV) DFLOW = 2.5*WDIS*SPILL*
     +                                       (D1-WELEV)**1.5
      IF(WTYPE.EQ.2.AND.D1.GT.WELEV) DFLOW = 32.2*WDIS*SPILL/
     +                                       (64.4*(D1-WELEV))**0.5
C=======================================================================
C     NEWTON-RAPHSON CORRECTION (ALL CROSS-SECTIONS)
C=======================================================================
      IF(KSOL.EQ.0) THEN
C$$$  WCH THINKS SHOULD DIVIDE BY RDELT (CONST DT), NOT DELT (VARIABLE DT)
C$$$  TRY CORRECTION 4/20/92
                    F     = DELV  + FLOW  - QIN - QSUR/RDELT
                    DF    = DDELV + DFLOW
                    ELSE
                    DFLOW = 2.0*DFLOW
                    F     = DELV  + FLOW1 - QIN - QSUR/RDELT
                    DF    = DDELV + DFLOW
                    ENDIF
C=======================================================================
C     ZERO SLOPE
C=======================================================================
      IF(ABS(DF).LE.1.0E-10) THEN
                        DEL = 0.001
                        ELSE
                        DEL = DELD - F/DF
                        ENDIF
C=======================================================================
C     CONVERGENCE CHECK (INDIVIDUAL GUTTER)
C=======================================================================
C#### WCH (CDM=C.I.M.), 8/13/96
C     THIS IF-LOOP SEEMS TO CAUSE PREMATURE END TO CONVERGENCE
C     ITERATIONS FOR CHANNELS WITH WEIRS/ORIFICES WHEN GUESS CAUSES
C     LEVEL BELOW OUTLET LEVEL (WELEV).  THIS MAKES IT HARD FOR CHANNEL 
C     TO "FILL UP" TO OUTLET LEVEL, ESPECIALLY AT START OF STORM WHEN 
C     CHANNEL IS DRY.  ELIMINATING THIS IF-LOOP SEEMS TO WORK, BASED
C     ON CDM EXPERIENCE.
C
C####      IF(WTYPE.GE.0.AND.GDEPTH+DEL.LT.WELEV) THEN
C####                                          GD       = GDEPTH+DEL
C####                                          LCHAN(J) = LCHAN(J) + 1
C####                                          GO TO 402
C####                                          ENDIF                    
      IF(GDEPTH+DEL.GE.DFULL) THEN
                       IF(IFLG.EQ.1) THEN
                                     GD       = DFULL
                                     LCHAN(J) = LCHAN(J) + 1
                                     GO TO 402
                                     ENDIF
                       DEL  = DFULL-GDEPTH
                       IFLG = 1
                       ELSE
                       IFLG = 0
                       IF(ABS(DEL-DELD).LE.0.001) GO TO 250
                       IF(IFLAG2.GE.2) KSOL = 1
                       ENDIF
  210 DELD = DEL
C=======================================================================
C     NO CONVERGENCE.
C
C     NEW DEPTH AT END OF TIME INTERVAL
C=======================================================================
  250 DELD   = DEL
      GDEPTH = GDEPTH + DELD
C=======================================================================
C     CHECK FOR ALLOWABLE CONDUIT DEPTHS.  4/22/92
C=======================================================================
      IF(GDEPTH.GT.DFULL) GDEPTH = DFULL
      IF(GDEPTH.LT.0.0)   GDEPTH = 0.0
      QSUR   = 0.0
      YZ(JJ) = GDEPTH
      IF(JJ.EQ.1) DY(1) = YZ(1)
      IF(JJ.EQ.2) THEN
                  DY(2) = (4.0*YZ(2) - YZ(1))/3.0
                  IF(ABS(DY(2)-DY(1)).LE.0.001*ABS(DY(1))) THEN
                                      LCHAN(J) = LCHAN(J) + 3
                                      GD       = DY(2)
                                      GO TO 402
                                      ENDIF
                  ENDIF
      IF(JJ.EQ.3) THEN
                  DY(3) = (4.0*YZ(3)  - YZ(2))/3.0
                  DZ(3) = (16.0*DY(3) - DY(2))/15.0
                  IF(ABS(DZ(3)-DY(3)).LE.0.001*ABS(DZ(3))) THEN
                                      LCHAN(J) = LCHAN(J) + 7
                                      GD       = DZ(3)
                                      GO TO 402
                                      ENDIF
                 ENDIF
      IF(JJ.EQ.4)THEN
                 DY(4)    = (4.0*YZ(4)  - YZ(3))/3.0
                 DZ(4)    = (16.0*DY(4) - DY(3))/15.0
                 DZ(4)    = (64.0*DZ(4) - DZ(3))/63.0
                 GD       = DZ(4)
                 LCHAN(J) = LCHAN(J) + 15
                 ENDIF
 400  CONTINUE
 402  CONTINUE
C=======================================================================
C     CHECK FOR ALLOWABLE CONDIUT DEPTHS.  4/22/92
C=======================================================================
      IF(GD.GT.DFULL) GD = DFULL
      IF(GD.LT.0.0)   GD = 0.0
      IF(WTYPE.EQ.-1) THEN
      IF(NPG.EQ.1)    THEN
             AX = 0.5*(GS1+GS2)*GD**2.0 + GWIDTH*GD
             WP = SQRT(GS1**2.0+1.0)*GD + SQRT(GS2**2.0+1.0)*GD+GWIDTH
             ENDIF
      IF(NPG.EQ.2)  THEN
                    SIN2D0  = 0.5*SIN(2.0*GD)
                    AX      = GWIDTH**2*(GD-SIN2D0)/4.0
                    WP      = GWIDTH*GD
                    ENDIF
      IF(NPG.EQ.4)  THEN
           IF(GD.GT.0.0) THEN
                    WIDTH = GWIDTH*SQRT(GD/DFULL)
                    AX    = 0.66666667*WIDTH*GD
                    X     = WIDTH/2.0
                    A2    = GWIDTH**4.0/(64.0*DFULL**2.0)
                    WP    = 8.0*DFULL/GWIDTH**2.0*(X*SQRT(A2+X**2.0) + 
     +                                     A2*LOG(X+SQRT(A2+X**2.0)) - 
     +                                     A2*LOG(SQRT(A2)))       
                    ELSE
                    AX    = 0.0
                    WP    = 0.001
                    ENDIF
           ENDIF
      IF(AX.LT.0.001) AX = 0.0
      IF(WP.LE.0.001) WP = 0.001
      RAD                = AX/WP
      ENDIF
                                     FLOW1 = 0.0
      IF(WTYPE.EQ.-1.AND.RAD.GT.0.0) FLOW1 = GCON*AX*RAD**0.666666667                        
      IF(WTYPE.EQ.0.AND.GD.GT.WELEV) FLOW1 = WDIS*SPILL*(GD-WELEV)**1.5
      IF(WTYPE.EQ.1.AND.GD.GT.WELEV) FLOW1 = WDIS*SPILL*(GD-WELEV)**2.5
      IF(WTYPE.EQ.2.AND.GD.GT.WELEV) FLOW1 = WDIS*SPILL*
     +                                       (2.0*32.2*(GD-WELEV))**0.5
      OUTFLW = FLOW1                        
      FLOW   = (OUTFLW + FLZERO)/2.0
      IF(FLOW.LT.1.0E-10.AND.WTYPE.EQ.-1) THEN
                                          FLOW   = 0.0
                                          GD     = 0.0
                                          ENDIF
      IF(FLOW.LT.0.0.AND.WTYPE.GE.0) THEN
                                     FLOW   = 0.0
                                     GD     = 0.0
                                     ENDIF
      GFLOW  = FLOW
C#### WCH (RED), 9/93.  ADD QSUR > 0.0 TO IF STMT.
      IF(IFLG.EQ.1.OR.QSUR.GT.0.0) THEN
                    QSUR   = QSUR + (QIN-FLOW)*RDELT
                    IF(QSUR.LT.0.0) QSUR = 0.0
                    ENDIF
      RETURN
      END

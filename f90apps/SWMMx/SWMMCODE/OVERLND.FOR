      SUBROUTINE OVERLND(KK,WC1,R1,WD,DEL,RDELT)
C=======================================================================
C      NONLINEAR RESERVOIR ROUTING FOR SURFACE RUNOFF
C                CREATED MARCH, 1988 BY BOB DICKINSON
C                UPDATED OCTOBER, 1988 BY BOB DICKINSON
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'NEW88.INC'
      DIMENSION NSEQ(4),YZ(4),DY(4),DZ(4)
      DATA NSEQ/1,2,4,8/,NUSE/4/
C=======================================================================
      KLND(KK) = KLND(KK) + 1
      DO 100 J = 1,NUSE
      DOI      = WD
      DELT     = RDELT/FLOAT(NSEQ(J))
      RSTAR    = R1*DELT
      WCC      = WC1*DELT
      DO 400 K = 1,NSEQ(J)
      DELR     = 1.0E-10
      DO 300 I = 1,21
      DO       =  DOI + DELR/2.0
      IF(DO.LE.0.0) THEN
                    TEST = -RSTAR/WCC
                    IF(TEST.GT.0.0) THEN
                                    DO  = TEST**0.666667
                                    DEL = (DO - DOI)/2.0
                                    ELSE
                                    DEL = -DOI
                                    ENDIF
                    GO TO 400
                    ENDIF
      F   = DELR  - RSTAR - WCC*DO**1.6666667
      DF  =  1.0  - 0.8333333*WCC*DO**0.6666667
      DEL = DELR  - F/DF
      IF(I.NE.1.AND.ABS(DEL-DELR).LE.ABS(0.001*DELR)) GO TO 400
  300 DELR  = DEL
  400 DOI   = DOI + DEL
      YZ(J) = DOI - WD
      IF(J.EQ.1) DY(1) = YZ(1)
      IF(J.EQ.2) THEN
                 DY(2) = (4.0*YZ(2) - YZ(1))/3.0
                 IF(ABS(DY(2)-DY(1)).LE.0.001*ABS(DY(1))) THEN
                                                    DEL     = DY(2)
                                                    LND(KK) = LND(KK)+3
                                                    RETURN
                                                    ENDIF
                 ENDIF
      IF(J.EQ.3) THEN
                 DY(3) = (4.0*YZ(3)  - YZ(2))/3.0
                 DZ(3) = (16.0*DY(3) - DY(2))/15.0
                 IF(ABS(DZ(3)-DY(3)).LE.0.001*ABS(DZ(3))) THEN
                                                    DEL     = DZ(3)
                                                    LND(KK) = LND(KK)+7
                                                    RETURN
                                                    ENDIF
                 ENDIF
      IF(J.EQ.4) THEN
                 DY(4)   = (4.0*YZ(4)  - YZ(3))/3.0
                 DZ(4)   = (16.0*DY(4) - DY(3))/15.0
                 DZ(4)   = (64.0*DZ(4) - DZ(3))/63.0
                 DEL     = DZ(4)
                 LND(KK) = LND(KK) + 15
                 ENDIF
 100  CONTINUE
      RETURN
      END

      SUBROUTINE BUILD(N,K,INCR,METH,ULIM,POW,COEF,T,EL)
C     RUNOFF BLOCK
C     CALLED BY QSHED NEAR LINE 94
C=======================================================================
C     This subroutine performs buildup calculations, initially
C     and during continuous simulation.  Created by WCH, 8/80.
C     Last updated July 1993 by WCH for minor change to avoid calcs with
C       zero upper limit.
C=======================================================================
      INCLUDE 'TAPES.INC'
      DATA JERR/0/
C=======================================================================
C     INCR = 0, Initial buildup, before storm.
C          = 1, Incremental buildup, during simulation.
C     METH = 0, Power-linear buildup.
C          = 1, Exponential buildup.
C          = 2, Michaelis-Menten buildup.
C     ULIM =    Upper limit for load.
C     POW  =    Power or exponent.
C     COEF =    Coefficient.
C     T    =    time or incremental time.
C     EL   =    Load.  Units are mg or mpn or other quantity.
C=======================================================================
C#### WCH, 7/29/93. BYPASS CALCS IF ULIM = 0.0
C=======================================================================
      IF(ULIM.LE.0.0) THEN
         EL = 0.0
         RETURN
         ENDIF
      IF(INCR.GT.0) GO TO 200
C=======================================================================
C     Calculate initial buildup.
C=======================================================================
C     Power-linear buildup.
C=======================================================================
      IF(METH.EQ.0) THEN
                    IF(POW.EQ.1.0) THEN
                                   EL = COEF*T
                                   ELSE
                                   EL = COEF*T**POW
                                   ENDIF
                    IF(EL.GT.ULIM) EL = ULIM
                    RETURN
                    ENDIF
C=======================================================================
C     Exponential buildup.
C=======================================================================
      IF(METH.EQ.1) THEN
                    EL = ULIM*(1.0-EXP(-POW*T))
                    RETURN
                    ENDIF
C=======================================================================
C     Michaelis-Menten buildup.
C=======================================================================
      IF(METH.EQ.2) THEN
                    EL = ULIM*T/(COEF+T)
                    RETURN
                    ENDIF
C=======================================================================
C     Calculate incremental buildup during simulation.
C=======================================================================
C     Power-linear buildup.
C=======================================================================
  200 IF(METH.EQ.0) THEN
         IF(COEF.EQ.0.0) THEN
                 JERR = JERR + 1
                 IF(JERR.LE.50) WRITE(N6,1000) N,K,COEF,POW,EL,ULIM,T
                 IF(JERR.EQ.51) WRITE(N6,1010)
                 ELSE
                 IF(POW.EQ.1.0) THEN
                                EL = EL+COEF*T
                                ELSE IF(POW.NE.0.0) THEN
                                EL = COEF*((EL/COEF)**(1.0/POW)+T)**POW
                                ENDIF
                 ENDIF
         IF(EL.GT.ULIM) EL = ULIM
         RETURN
         ENDIF
C=======================================================================
C     Exponential buildup.
C=======================================================================
      IF(METH.EQ.1) THEN
                     EL = ULIM-EXP(-POW*T)*(ULIM-EL)
                     RETURN
                     ENDIF
C=======================================================================
C     Michaelis-Menten buildup.
C=======================================================================
      IF(METH.EQ.2) THEN
                    XT = COEF*EL/(ULIM-EL)
                    EL = ULIM*(XT+T)/(COEF+XT+T)
                    RETURN
                    ENDIF
C=======================================================================
 1000 FORMAT(/,
     .' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',/,
     .' @ Error....You are dividing by zero in Subroutine Build @',/,
     .' @          using power linear buildup.                  @',/,
     .' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',/,
     .' Subcat...',I10,  ' Land use/quality constituent..',I6,/,
     .' Coef.....',F10.2,' Power.........................',F6.2,/,
     .' Load.....',1PE10.3,' Upper limit...................',E10.3,/,
     .' Inc time.',E10.3)
 1010 FORMAT(/,' Allowable number of errors exceeded in Subroutine',
     +         ' BUILD.  Stop error printout.')
C=======================================================================
      END

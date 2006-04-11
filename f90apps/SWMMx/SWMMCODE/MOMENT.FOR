      SUBROUTINE MOMENT(J,NLABEL)
C=======================================================================
C     This subroutine is called by the STATS Block.  For a given series
C     of event data, it computes and print the mean, variance, standard
C     deviation, coefficient of variation, and coefficient of skewness.
C
C     Updated September, 1990 by Laura B. Terrell (CDM).
C     Updated December,  1990 by Bob Dickinson.
C=======================================================================
      INCLUDE 'TAPES.INC'
      INCLUDE 'INTER.INC'
      INCLUDE 'STCOM.INC'
      DOUBLE PRECISION XEVNTS,SUMX,SUMX2,SUMX3,AMED,ACV,AXBAR,
     1                 ASDX,NUMER,FACTOR,DENOM,DPARM
C=======================================================================
C     Compute sum, sum of squares, and sum of cubes.
C=======================================================================
      JTEMP    = 0
      SUMX     = 0.0
      SUMX2    = 0.0
      SUMX3    = 0.0
      DO 100 I = 1,NEVNTS
      DPARM    = PARAM(I,J)
      IF(INLOG.GT.0.AND.DPARM.NE.0.0) DPARM = DLOG(DPARM)
      SUMX     = SUMX  + DPARM
      SUMX2    = SUMX2 + DPARM*DPARM
      SUMX3    = SUMX3 + DPARM*DPARM*DPARM
100   CONTINUE
C=======================================================================
C     Calculate moments.
C=======================================================================
      XEVNTS = DFLOAT(NEVNTS)
      XBAR   = SUMX/XEVNTS
      IF(NEVNTS.GT.1) THEN
                      VARX   = (SUMX2 - XEVNTS*(XBAR)**2)/(XEVNTS-1.0)
                      SDX    = SQRT(ABS(VARX))
                                      CV  = 0.0
                      IF(XBAR.NE.0.0) CV  = SDX/XBAR
                      ELSE
                      CV     = 0.0
                      SDX    = 0.0
                      VARX   = 0.0
                      ENDIF
      IF(NEVNTS.GT.2.AND.XBAR.NE.0.0) THEN
                   NUMER  = SUMX3/XEVNTS - 3.0*SUMX2*XBAR/XEVNTS +
     1                      2.0*(XBAR)**3
                   FACTOR = DSQRT(XEVNTS*(XEVNTS-1.0)) / (XEVNTS-2.0)
                   DENOM  = SUMX2/XEVNTS - XBAR*XBAR
                   IF(DENOM.LE.0.0) DENOM  = 1.0
                   DENOM  = DENOM**1.5
                   COSKEW = NUMER * FACTOR / DENOM
                   ELSE
                   COSKEW = 0.0
                   ENDIF
C=======================================================================
C     If analysis is for a pollutant, go to print statement for pollutants
C=======================================================================
      IF(NLABEL.EQ.0) THEN
                      J1    = J
                      JTEMP = 1
                      IF(LOCRN.GT.0) J1 = J + 10
                      ELSE
                      J1    = J + 5
                      ENDIF
C=======================================================================
C     Print parameter identifier.
C=======================================================================
C     Perform log transform calculations on log emc data if desired.
C     This will override other flow and pollutant output.
C
C     From previously calculated log data (XBAR & SDX), arithmetic
C     statistics may be calculated.
C
C     XBAR  = Logarithmetic mean
C     SDX   = Logarithmetic standard deviation
C     AXBAR = Arithmetic mean
C     ACV   = Arithmetic coefficient of variation
C     ASDX  = Arithmetic standard deviation
C     AMED  = Arithmetic median
C=======================================================================
      IF(INLOG.EQ.1) THEN
           AXBAR = DEXP(XBAR + 0.5*SDX*SDX)
           ACV   = DSQRT(EXP(SDX*SDX) - 1.0)
           ASDX  = AXBAR * ACV
           AMED  = DEXP(XBAR)
           IF(JTEMP.EQ.1) THEN
              IF(LOCRQ.GT.0) WRITE(N6,1000) RAINF(2),PARLAB(J1),
     +                               XBAR,SDX,AXBAR,ASDX,ACV,AMED
              IF(LOCRN.GT.0) WRITE(N6,1000) RAINF(1),PARLAB(J1),
     +                               XBAR,SDX,AXBAR,ASDX,ACV,AMED
              ELSE
              WRITE(N6,1000) PNAME(NLABEL),PARLAB(J1),
     +                               XBAR,SDX,AXBAR,ASDX,ACV,AMED
              ENDIF
           ELSE
           IF(JTEMP.EQ.1) THEN
              IF(LOCRQ.GT.0) WRITE(N6,1000) RAINF(2),PARLAB(J1),XBAR,
     +                                            VARX,SDX,CV,COSKEW
              IF(LOCRN.GT.0) WRITE(N6,1000) RAINF(1),PARLAB(J1),XBAR,
     +                                            VARX,SDX,CV,COSKEW
              ELSE
              WRITE(N6,1000) PNAME(NLABEL),PARLAB(J1),XBAR,VARX,
     +                                            SDX,CV,COSKEW
              ENDIF
           ENDIF
C=======================================================================
C     Print note regarding magnitude units for NDIM = 2.
C=======================================================================
      IF(NLABEL.EQ.0)       RETURN
      IF(NDIM(NLABEL).NE.2) RETURN
      IF(J.EQ.1) THEN
                 IF(METRIC.EQ.1) THEN
                                 WRITE(N6,1022) PUNIT(NLABEL)
                                 ELSE
                                 WRITE(N6,1026) PUNIT(NLABEL)
                                 ENDIF
                 ENDIF
      IF(J.GE.2.AND.J.LE.3) THEN
                 IF(METRIC.EQ.1) THEN
                                 WRITE(N6,1110) PUNIT(NLABEL)
                                 ELSE
                                 WRITE(N6,1130) PUNIT(NLABEL)
                                 ENDIF
                 ENDIF
      IF(J.GE.4) WRITE(N6,1160) PUNIT(NLABEL)
      RETURN
C=======================================================================
1000  FORMAT(1X,A8,A20,6(1X,1PG11.4))
1022  FORMAT(/,1X,'Note:  Magnitude has units of (cubic feet) * ',
     +   A8,'.  See user''s manual for explanation.')
1026  FORMAT(/,1X,'Note:  Magnitude has units of (liters) * ',
     +   A8,'.  See user''s manual for explanation.')
1110  FORMAT(/,1X,'Note:  Magnitude has units of (cfs) * ',
     +   A8,'  See user''s manual for explanation.')
1130  FORMAT(/,1X,'Note:  Magnitude has units of (liters/s) * ',
     +   A8,'.  See user''s manual for explanation.')
1160  FORMAT(/,1X,'Note:  Magnitude has units of ',
     +   A8,'.  See user''s manual for explanation.')
C=======================================================================
      END

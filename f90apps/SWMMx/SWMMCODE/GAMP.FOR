      SUBROUTINE GAMP(SUCT,SMD,SMDMAX,IFLAG,FU,F,HYDCON,WDEPTH,
     1           TR,DELT,TIME,RI,RLOSS,UL,SVAP,KAMEW,NAMEW)
C=======================================================================
C     Green-Ampt infiltration routine,  written FEB 1979 by R.G.MEIN.
C
C     Theory as given in Mein & Larson, Water Resources Research,
C                                       Vol. 9, NO. 2, 384-394, 1973
C=======================================================================
C     Redistribution rate depends on soil hydraulic conductivity
C     and is considered only for the surface region.
C#######################################################################
C     WCH (CDM), 10/5/93.  CHANGE EVAP TO SVAP FOR CONSISTENCY, 5 
C       LOCATIONS.
C=======================================================================
      INCLUDE 'TAPES.INC'
      DOUBLE PRECISION RI,RLOSS,SVAP
      CHARACTER KAMEW*10
C=======================================================================
C     Statement function for Green-Ampt equation.
C=======================================================================
      F22(X) = X-(X-F-C1*ALOG(X+C1)+C2)/(1.0-C1/(X+C1))
C=======================================================================
      C1    = SUCT*SMD
      TBS   = 6.0
      R1    = RI-SVAP
      FUMAX = UL*SMDMAX
      RVOL  = R1*DELT
C=======================================================================
C     Check for surface saturation.
C=======================================================================
      IF(IFLAG.EQ.1)   GO TO 200
C=======================================================================
C     Surface unsaturated at the beginning of the time step.
C=======================================================================
      TR = TR - DELT
C=======================================================================
C     No rainfall.
C=======================================================================
      IF(R1.LE.0.0) THEN
                    RLOSS = RI
                    IF(FU.LE.0.0) RETURN
C=======================================================================
C                   Deplete soil moisture during dry period.
C=======================================================================
                    DF  = UL*DELT/90000.
                    DEP = DF*FUMAX
                    F   = F  - DEP
                    FU  = FU - DEP
                    IF(FU.LE.0.0) THEN
                                  FU  = 0.0
                                  F   = 0.0
                                  SMD = SMDMAX
                                  RETURN
                                  ENDIF
C=======================================================================
C                   If sufficient time elapsed since rain, redistribute.
C=======================================================================
                    IF(TR.LE.0.0) THEN
                                  SMD = (FUMAX-FU)/UL
                                  F   = 0.0
                                  ENDIF
                    RETURN
                    ENDIF
C=======================================================================
C     Low intensity, all infiltrates.  Rain intensity > 0.0
C=======================================================================
      IF(R1.LE.HYDCON) THEN
                       FU = FU + RVOL
                       IF(FU.GT.FUMAX) FU = FUMAX
                       F                  = F + RVOL
                       RLOSS              = RI
                       IF(TR.LE.0.0) THEN
                                     SMD = (FUMAX-FU)/UL
                                     F   = 0.0
                                     ENDIF
                       RETURN
                       ENDIF
C=======================================================================
C     Rain intensity > Hyd. conductivity.
C=======================================================================
      TR = 900.0*TBS/UL
      FS = HYDCON*C1/(R1-HYDCON)-F
C=======================================================================
C     Soil still wet from last rain, surface saturates.
C=======================================================================
      IF(FS.LE.0.0) THEN
                    IFLAG = 1
                    GO TO 200
                    ENDIF
C=======================================================================
C     All water infiltrates.
C=======================================================================
      IF(FS.GE.RVOL) THEN
                     RLOSS = RI
                     F     = F  + RVOL
                     FU    = FU + RVOL
                     IF(FU.GT.FUMAX) FU = FUMAX
                     RETURN
                     ENDIF
C=======================================================================
C     Surface saturates during interval.
C=======================================================================
      F  = F + FS
      TS = FS/R1
      C2 = C1*ALOG(F+C1)-HYDCON*(DELT-TS)
      GO TO 300
C=======================================================================
C     Surface is saturated, so compute vol of potential infiltration.
C=======================================================================
  200 TR = 900.0*TBS/UL
      TS = 0.0
      IF(C1.LE.0.0) THEN
                    F2 = HYDCON*DELT+F
                    ELSE
                    C2 = C1*ALOG(F+C1) - HYDCON*DELT
                    GO TO 300
                    ENDIF
C=======================================================================
C     Excess water at surface.  Corrected December, 1990.
C=======================================================================
  210 IF((F2-F).LE.(WDEPTH+RVOL)) THEN
                                  RLOSS = (F2-F)/DELT + SVAP
                                  FU    = F2 - F      + FU
                                  F     = F2
C---- old code                    FU    = F2
                                  IF(FU.GT.FUMAX) FU = FUMAX
                                  RETURN
C=======================================================================
C     Rain+surface water infiltrates.  Corrected December, 1990.
C=======================================================================
                                  ELSE
                                  RLOSS = WDEPTH/DELT + RI
                                  F     = F  + AMAX1(WDEPTH+RVOL,0.0)
                                  IFLAG = 0
                                  FU    = FU + AMAX1(WDEPTH+RVOL,0.0)
C--- old code                     FU    = F
                                  IF(FU.GT.FUMAX) FU = FUMAX
                                  RETURN
                                  ENDIF
C=======================================================================
C     Newton-Raphson iteration to solve for infiltration volume.
C=======================================================================
  300 F21      = F
      DO 310 I = 1,11
      F2       = F22(F21)
      IF(ABS(F2-F21).LT.0.0001)   GO TO 320
      F21      = F2
  310 CONTINUE
C=======================================================================
C     No convergence.
C=======================================================================
      F2 = F + (DELT-TS)*R1/2.0
      IF(JCE.EQ.0) WRITE(N6,315) NAMEW,TIME,IFLAG,SMD,F,R1
      IF(JCE.EQ.1) WRITE(N6,316) KAMEW,TIME,IFLAG,SMD,F,R1
C=======================================================================
C     Convergence, return to call point.
C=======================================================================
  320 IF(IFLAG.EQ.0) THEN
                     RLOSS = (F2 - F + FS)/DELT + SVAP
                     FU    = F2 - F  + FU
                     F     = F2
                     IFLAG = 1
C--- old code        FU    = F2
                     IF(FU.GT.FUMAX) FU = FUMAX
                     RETURN
                     ENDIF
      GO TO 210
C=======================================================================
315   FORMAT(' ===> !! Warning, No convergence of Green-Ampt equation. F
     1or subcat. input # ',I9,' Time =',F9.0,' seconds',/,
     2 ' IFLAG =',I2,' SMD =',F6.4,' F(ft) =',F7.4,' Rain-Evap(ft/s) =',
     3 E12.5,' ===>  Assume infil. vol. = half of rain-evap.')
316   FORMAT(' ===> !! Warning, No convergence of Green-Ampt equation. F
     1or subcat. input name  ',A10,' Time =',F9.0,' seconds',/,
     2 ' IFLAG =',I2,' SMD =',F6.4,' F(ft) =',F7.4,' Rain-Evap(ft/s) =',
     3 E12.5,' ===>  Assume infil. vol. = half of rain-evap.')
C=======================================================================
      END

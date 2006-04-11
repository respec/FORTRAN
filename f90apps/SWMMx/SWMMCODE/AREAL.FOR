      SUBROUTINE AREAL(WSNOW,SI,NEWSNO,ASC,AWE,SBA,SBWS,RIN,ADC,DELT)
C=======================================================================
C
C     FOR UF SNOW MELT ROUTINES, THIS ROUTINE FINDS FRACTION OF AREA
C        COVERED BY SNOW (ASC).
C     DEVELOPED JULY 1, 1977, BY W. HUBER AND UPDATED OCT. 4, 1977.
C     USE ANDERSON'S NWS REPORT METHODS FOR HANDLING NEW SNOW.
C     USES AREAL DEPLETION CURVE IN ARRAY ADC, (FOR VALUES OF ASC).
C=======================================================================
      DIMENSION ADC(10)
C
      IF(WSNOW.LT.SI) GO TO 10
C
C     HERE IF COMPLETE SNOW COVERAGE FOR AREA.
C
      NEWSNO = 0
      ASC    = 1.0
      RETURN
   10 IF(WSNOW.GT.0.0) GO TO 20
C
C     HERE IF NO SNOW.
C
      NEWSNO = 0
      ASC    = 0.0
      RETURN
   20 IF(RIN.LE.0.0.OR.NEWSNO.EQ.1) GO TO 30
C
C      HERE IF DESCENDING DOWN DEPLETION CURVE.
C
      AWESI = WSNOW/SI
C
C     USE SUBROUTINE FINDSC FOR LINEAR INTERPOLATION.
C
C***********************************************************************
C
      NEWSNO = 0
      RETURN
   30 IF(RIN.GE.0.0) GO TO 40
C
C     HERE IF NEW SNOW HAS FALLEN WHILE ON DEPLETION CURVE.
C     MUST SET UP NEW PARAMETERS FOR TEMPORARY LINEAR DEPLETION CURVE.
C     PARAMETER NEWSNO INDICATES NEW SNOW ON PARTIALLY BARE GROUND.
C
C     FIND AMOUNT OF SNOW PRESENT AT LAST TIME STEP
C
                     AWE = (WSNOW+RIN*DELT)/SI
      IF(AWE.LE.0.0) AWE = 0.0
C
      CALL FINDSC(AWE,SBA,ADC)
C***********************************************************************
C
                      SBWS = AWE - RIN * 0.75 * DELT / SI
      IF(SBWS.GT.1.0) SBWS = 1.0
      ASC    = 1.0
      NEWSNO = 1
      RETURN
C
C     HERE IF ON TEMPORARY DEPLETION CURVE.
C
   40 AWESI = WSNOW/SI
      IF(AWESI.GE.AWE) GO TO 50
C
C     HERE IF HAVE MOVED BACK TO REGULAR DEPLETION CURVE.
C
      CALL FINDSC(AWESI,ASC,ADC)
C***********************************************************************
C
      NEWSNO = 0
      RETURN
   50 IF(AWESI.LT.SBWS) GO TO 60
C
C     HERE IF STILL HAVE 100 % COVER OF NEW SNOW.
C
      ASC    = 1.0
      NEWSNO = 1
      RETURN
C
C     HERE IF ON TEMPORARY LINEAR DEPLETION CURVE.
C
   60 ASC = SBA+(1.0-SBA)/(SBWS-AWE)*(AWESI-AWE)
      NEWSNO = 1
      RETURN
      END

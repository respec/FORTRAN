      SUBROUTINE NTIME(JDAY,TMDAY,TRAIN)
C#######################################################################
C     CALCULATE THE TIME DIFFERENCE IN SECONDS BETWEEN JDAY AND TMDAY
C     AND THE EXISTING JULDAY AND TIMDAY
C
C     JDAY   = JULIAN DAY
C     TMDAY  = TIME OF DAY IN SECONDS
C     TRAIN  = TIME DIFFERENCE IN SECONDS (+ or -)
C#######################################################################
      INCLUDE 'STIMER.INC'
      JYEAR = JDAY/1000
      IYEAR = JULDAY/1000
      LDAY  = JDAY   - JYEAR*1000
      KDAY  = JULDAY - IYEAR*1000
      IF (JYEAR.LT.100) THEN
      JYEAR = JYEAR + 1900
      JDAY = LDAY + JYEAR*1000
      ENDIF
      IF (IYEAR.LT.100) THEN
      IYEAR = IYEAR + 1900
      JULDAY = KDAY + IYEAR*1000
      ENDIF
      JDIFF = 0
C=======================================================================
C     IYEAR AND JYEAR ARE DIFFERENT YEARS
C=======================================================================
      IF(JYEAR.GT.IYEAR) THEN
      IY    = IYEAR  + 1
      JY    = JYEAR  - 1
      IF(JY.GE.IY) THEN
                   DO 100 KYEAR = IY,JY
                                         ID = 365
                   IF(MOD(KYEAR,4).EQ.0) ID = 366
                   JDIFF = JDIFF + ID
 100               CONTINUE
                   ENDIF
      JDIFF = JDIFF + LDAY
                            ID = 365
      IF(MOD(IYEAR,4).EQ.0) ID = 366
      JDIFF = JDIFF + ID - KDAY
      ENDIF
C=======================================================================
C     IYEAR AND JYEAR ARE DIFFERENT YEARS
C=======================================================================
      IF(JYEAR.LT.IYEAR) THEN
      IY    = IYEAR  - 1
      JY    = JYEAR  + 1
      IF(IY.GE.JY) THEN
                   DO 200 KYEAR = JY,IY
                                         ID = 365
                   IF(MOD(KYEAR,4).EQ.0) ID = 366
                   JDIFF = JDIFF - ID
 200               CONTINUE
                   ENDIF
      JDIFF = JDIFF - KDAY
                            ID = 365
      IF(MOD(JYEAR,4).EQ.0) ID = 366
      JDIFF = JDIFF - ID + LDAY
      ENDIF
C=======================================================================
C     IYEAR AND JYEAR ARE THE SAME YEAR
C=======================================================================
      IF(JYEAR.EQ.IYEAR) THEN
                         JDIFF = LDAY - KDAY
                         ENDIF
C=======================================================================
C     CALCULATE DIFFERENCE IN SECONDS
C=======================================================================
      TDIFF = TMDAY - TIMDAY
      TRAIN = FLOAT(JDIFF)*86400.0 + TDIFF
      RETURN
      END

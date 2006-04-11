      SUBROUTINE NDATE(TIME,JDAY,TMDAY)
C#######################################################################
C     THIS SUBROUTINE CALCULATES THE NEW JULIAN DAY AND TIME OF DAY
C     RESULTING FROM ADDING TIME TO THE EXISTING JULDAY AND TIMDAY
C
C     JDAY  = JULIAN DAY
C     TMDAY = TIME OF DAY IN SECONDS
C     TIME  = NEW TIME IN SECONDS
C#######################################################################
      INCLUDE 'STIMER.INC'
C=======================================================================
      TMDAY = TIMDAY + TIME
      JDAY  = JULDAY
 100  IF(TMDAY.GE.86400.0) THEN
                TMDAY = TMDAY - 86400.0
                JDAY  = JDAY + 1
                JYEAR = JDAY/1000
                IDAY  = JDAY - 1000*JYEAR
                IF(MOD(JYEAR,4).EQ.0.AND.IDAY.GE.367.OR.
     +             MOD(JYEAR,4).NE.0.AND.IDAY.GE.366) THEN
                                         JYEAR  = JYEAR + 1
                                         JDAY   = JYEAR*1000 + 1
                                         ENDIF
                ENDIF
      IF(TMDAY.GE.86400.0) GO TO 100
      RETURN
      END

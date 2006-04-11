      SUBROUTINE STIME(DELTA)
C#######################################################################
C     JULDAY = JULIAN DAY
C     TIMDAY = TIME OF DAY IN SECONDS
C     DELTA  = NEW TIME STEP IN SECONDS
C#######################################################################
      INCLUDE 'STIMER.INC'
      TIMDAY = TIMDAY + DELTA
 100  IF(TIMDAY.GE.86400.0) THEN
                TIMDAY = TIMDAY - 86400.0
                JULDAY = JULDAY + 1
                IDAY   = JULDAY - 1000*(JULDAY/1000)
                IF(MOD(NYEAR,4).EQ.0.AND.IDAY.EQ.367.OR.
     +             MOD(NYEAR,4).NE.0.AND.IDAY.EQ.366) THEN
                                         NYEAR  = NYEAR + 1
                                         JULDAY = NYEAR*1000 + 1
                                         ENDIF
                CALL DATED
                ENDIF
      IF(TIMDAY.GE.86400.0) GO TO 100
      RETURN
      END

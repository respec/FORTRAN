      SUBROUTINE ZINCBK (IBLOCK, JUL, IYR, IMON, IDAY)
C
C     Increments to the next time block for
C     all time-seires data
C
C     Written by Bill Charley at HEC, 1989
C
      INTEGER*4 JUL, IYMDJL                                             ML
C
C
      IF (IBLOCK.EQ.1) THEN
C     Increment by day
      JUL = IYMDJL(IYR,IMON,IDAY) + 1
      K = JLIYMD (JUL,IYR,IMON,IDAY)
C
      ELSE IF (IBLOCK.EQ.2) THEN
C     Increment by month
      IMON = IMON + 1
      IF (IMON.GT.12) THEN
      IYR = IYR + 1
      IMON = IMON - 12
      ENDIF
      JUL = IYMDJL (IYR, IMON, IDAY)
C
      ELSE IF (IBLOCK.EQ.3) THEN
C     Increment by year
      IYR = IYR + 1
      JUL = IYMDJL (IYR, IMON, IDAY)
C
      ELSE IF (IBLOCK.EQ.4) THEN
C     Increment by decade
      IYR = IYR + 10
      JUL = IYMDJL (IYR, IMON, IDAY)
C
      ELSE IF (IBLOCK.EQ.5) THEN
C     Increment by century
      IYR = IYR + 100
      JUL = IYMDJL (IYR, IMON, IDAY)
      ENDIF
C
C
      RETURN
      END

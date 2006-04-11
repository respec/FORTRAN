      SUBROUTINE ZR2MIN (IDATES, DATES, JDATE, NVALS)
C
C     Real Dates to Minutes for irregular interval time series data
C
C     Converts a Date array in fractions of a day since a base date
C     to minutes past midnight since the base date
C     The compiler should show a type mismatch for IDATES
C
C     Written by Bill Charley, HEC,  Jan 1990
C
C     INTEGER*6 IDATES(*)                                               H
      INTEGER*4 IDATES(*)                                               ML
C     INTEGER IDATES(*)                                                 u
      REAL DATES(*), RMIN
      INTEGER*4 JDATE, JUL, IDIFF, MIN, I1440                           ML
C
C
      I1440 = 1440
C
      DO 20 I=1,NVALS
      JUL = DATES(I)
      RMIN = DATES(I) - FLOAT(JUL)
      RMIN = (RMIN * 1440.) + 0.7
      MIN = INT (RMIN)
      IDIFF = JUL - JDATE
      IDATES(I) = (IDIFF * I1440) + MIN
 20   CONTINUE
C
C
      RETURN
      END

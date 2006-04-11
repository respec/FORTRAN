      FUNCTION JDATE(IDAY,MONTH,IYEAR)
C#######################################################################
C     Find the Julian date for this year.
C#######################################################################
      JDATE = 0
      IFEB  = 28
      IF (IYEAR.LT.100) IYEAR = IYEAR + 1900
      IF((IYEAR/4)*4-IYEAR.EQ.0) IFEB = 29
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12),MONTH
 12   JDATE = JDATE+30
 11   JDATE = JDATE+31
 10   JDATE = JDATE+30
  9   JDATE = JDATE+31
  8   JDATE = JDATE+31
  7   JDATE = JDATE+30
  6   JDATE = JDATE+31
  5   JDATE = JDATE+30
  4   JDATE = JDATE+31
  3   JDATE = JDATE+IFEB
  2   JDATE = JDATE+31
  1   JDATE = JDATE+IDAY
      RETURN
      END

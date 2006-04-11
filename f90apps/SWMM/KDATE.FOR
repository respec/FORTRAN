      FUNCTION KDATE(IDAY,MONTH,IYEAR)
C#######################################################################
C     Use by subtraction to find the number of days between two dates.
C     KDATE = number of days since January 1, 1901.  
C     WCH, 3/2/93.  CORRECTION FOR FEBRUARY DAYS.
C#######################################################################
      KDATE    = IDAY
      IF (IYEAR.LT.100) IYEAR = IYEAR + 1900
      JYEAR    = IYEAR - 1
      DO 300 I = 1,JYEAR
      IF((I/4)*4-I.NE.0) THEN
                         KDATE = KDATE + 365
                         ELSE
                         KDATE = KDATE + 366
                         ENDIF
 300  CONTINUE
C#### WCH, 3/2/93.  ADD NON-LEAP YEAR NUMBER OF DAYS FOR FEBRUARY.
      IFEB = 28
      IF((IYEAR/4)*4-IYEAR.EQ.0) IFEB = 29
C=======================================================================
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12),MONTH
 12   KDATE = KDATE+30
 11   KDATE = KDATE+31
 10   KDATE = KDATE+30
  9   KDATE = KDATE+31
  8   KDATE = KDATE+31
  7   KDATE = KDATE+30
  6   KDATE = KDATE+31
  5   KDATE = KDATE+30
  4   KDATE = KDATE+31
  3   KDATE = KDATE+IFEB
  2   KDATE = KDATE+31
  1   RETURN
      END

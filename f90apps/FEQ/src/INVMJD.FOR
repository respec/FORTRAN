C
C
C
      INTEGER FUNCTION   MJD
     I                      (YR, MN, DY)
 
C     + + + PURPOSE + + +
C     Compute modified julian date for any date with a year greater
C     than 1582.  We take the resulting date to represent the
C     elapsed time from some point in the past to the first instant
C     of the given day.
C     The date must be later than Nov. 17, 1858 for MJD to be
C     a positive number.  Thus for use in FEQ the year must be
C     1859 or greater.
 
C     This routine and INVMJD have been checked for every day
C     from 1860 through the year 25000.
 
C     Developed from information given in: "Astronomical Formulae
C     for Calculators', Jean Meeus, published by Willmann-Bell.

C     + + + DUMMY ARGUMENTS + + +
      INTEGER DY, MN, YR
 
C     + + +DUMMY ARGUMENT DEFINITIONS + + +
C     YR     - calendar year
C     MN     - number of month(1-12)
C     DY     - day in the month
 
C     + + + LOCAL VARIABLES + + +
      INTEGER A, B, M, Y
 
C     + + + INTRINSICS + + +
      INTRINSIC INT
C***********************************************************************
      IF(MN.GT.2) THEN
        Y = YR
        M = MN
      ELSE
        Y = YR - 1
        M = MN + 12
      ENDIF
 
      A = Y/100
      B = 2 - A + A/4
 
      MJD = (36525*Y)/100 + INT(30.6001*(M+1)) + DY + B - 679006
      RETURN
      END

      SUBROUTINE ZPIRTS (IFLTAB, CPATH, NPATH, IBUFF, KBUFF,            MLu
     * IDUM, NDUM, DATES, VALUES, NVALS, BDATE, CUNITS, CTYPE,          MLu
     * INFLAG, ISTAT)                                                   MLu
C     SUBROUTINE ZPIRT6 (IFLTAB, CPATH, NPATH, IBUFF, KBUFF,            H
C    * IDUM, NDUM, DATES, VALUES, NVALS, BDATE, CUNITS, CTYPE,          H
C    * INFLAG, ISTAT)                                                   H
C
C     Replaced by ZSITS
C
C
C     INTEGER*6 IFLTAB(*)                                               H
      INTEGER*4 IFLTAB(*)                                               ML
C     INTEGER IFLTAB(*)                                                 u
      INTEGER*4 JDATE, IBDATE                                           ML
      CHARACTER CPATH*(*), CUNITS*(*), CTYPE*(*)
      REAL DATES(*), VALUES(*)
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
      IBDATE = DATES(1) + BDATE
      JDATE = DATES(1)
C
C     Convert fractions of a day to minutes
      CALL ZR2MIN (DATES, DATES, JDATE, NVALS)
C
C
      IFLAG = INFLAG
      CALL ZSITS (IFLTAB, CPATH(1:NPATH), DATES, VALUES, NVALS,
     * IBDATE, CUNITS, CTYPE, IFLAG, ISTAT)
C
C     Convert minutes back to fractions of a day
      CALL ZMIN2R (DATES, DATES, REAL(JDATE), NVALS)
C
C
 800  CONTINUE
      RETURN
C
      END

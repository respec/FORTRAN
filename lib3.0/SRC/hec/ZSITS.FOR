      SUBROUTINE ZSITS (IFLTAB, CPATH, ITIMES, VALUES, NVALS, IBDATE,
     * CUNITS, CTYPE, INFLAG, ISTAT)
C
C     Store irregular interval time series data short version
C     For user header or data compression flags, call ZSITSX
C
C     INTEGER*6 IFLTAB(*), ITIMES(*), JQUAL, IUHEAD                     H
C     INTEGER IFLTAB(*), ITIMES(*)                                      u
      INTEGER*4 IFLTAB(*), ITIMES(*), JQUAL, IUHEAD                     ML
      INTEGER*4 IBDATE                                                  ML
      REAL VALUES(*)
      CHARACTER CPATH*(*), CUNITS*(*), CTYPE*(*)
C
      COMMON /ZDSSCP/ JCOMP, BASEV, LBASEV, LDHIGH, NPREC
      LOGICAL LBASEV, LDHIGH
C
C
      CALL ZSITSX (IFLTAB, CPATH, ITIMES, VALUES, NVALS, IBDATE,
     * JQUAL, .FALSE., CUNITS, CTYPE, IUHEAD, 0, INFLAG, ISTAT)
C
      JCOMP = 0
      BASEV = 0.0
      LBASEV = .FALSE.
      LDHIGH = .FALSE.
C
      RETURN
      END

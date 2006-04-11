      SUBROUTINE ZRITS (IFLTAB, CPATH, JULS, ISTIME, JULE, IETIME,
     * ITIMES, VALUES, KVALS, NVALS, IBDATE, CUNITS, CTYPE, ISTAT)
C
C     Retrieve irregular time series data
C     Short version
C     For data quailty or user header call ZRITSX
C
C     INTEGER*6 IFLTAB(*), ITIMES(*), IUHEAD, JQUAL                     H
C     INTEGER IFLTAB(*), ITIMES(*)                                      u
      INTEGER*4 IFLTAB(*), ITIMES(*), IUHEAD, JQUAL                     ML
      INTEGER*4 JULS, JULE, IBDATE                                      ML
      REAL VALUES(*)
      CHARACTER CPATH*(*), CUNITS*(*), CTYPE*(*)
      LOGICAL LQUAL
C
      CALL ZRITSX (IFLTAB, CPATH, JULS, ISTIME, JULE, IETIME,
     * ITIMES, VALUES, KVALS, NVALS, IBDATE, JQUAL, .FALSE., LQUAL,
     * CUNITS, CTYPE, IUHEAD, 0, NDUM, 0, ISTAT)
C
      RETURN
      END

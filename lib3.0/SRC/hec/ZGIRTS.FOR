      SUBROUTINE ZGIRTS (IFLTAB, CPATH, NPATH, JULS, ISTIME,            MLu
     * JULE, IETIME, IBUFF, KBUFF, IDUM, NDUM, KVALS, DATES, VALUES,    MLu
     * NVALS, BDATE, CUNITS, CTYPE, ISTAT)                              MLu
C     SUBROUTINE ZGIRT6 (IFLTAB, CPATH, NPATH, JULS, ISTIME,            H
C    * JULE, IETIME, IBUFF, KBUFF, IDUM, NDUM, KVALS, DATES, VALUES,    H
C    * NVALS, BDATE, CUNITS, CTYPE, ISTAT)                              H
C
C     Get Irregular Time Series data
C     Replaced by ZRITSX
C
C
C     INTEGER*6 IFLTAB(*), IBUFF(KBUFF)                                 H
      INTEGER*4 IFLTAB(*), IBUFF(KBUFF)                                 ML
C     INTEGER IFLTAB(*), IBUFF(KBUFF)                                   u
      INTEGER*4 JULS, JULE, IBDATE                                      ML
C     INTEGER*6 JQUAL, IUHEAD                                           H
      INTEGER*4 JQUAL, IUHEAD                                           ML
      LOGICAL LQUAL
      CHARACTER CPATH*(*), CUNITS*(*), CTYPE*(*)
      REAL DATES(*), VALUES(*)
C
C
      INFLAG = 0
      IF (ISTAT.EQ.-11) INFLAG = 1
      IF (ISTAT.EQ.-12) INFLAG = 2
      IF (ISTAT.EQ.-13) INFLAG = 3
      ISTAT = 0
C
      CALL ZRITSX (IFLTAB, CPATH(1:NPATH), JULS, ISTIME, JULE, IETIME,
     * DATES, VALUES, KVALS, NVALS, IBDATE, JQUAL, .FALSE., LQUAL,
     * CUNITS, CTYPE, IUHEAD, 0, NDUM, INFLAG, ISTAT)
C
      BDATE = IBDATE
      CALL ZMIN2R (DATES, DATES, 0.0, NVALS)
C
C
      RETURN
      END

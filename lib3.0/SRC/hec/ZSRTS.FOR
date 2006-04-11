C     SUBROUTINE ZSRTS6 ( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES,   H
C    * CUNITS, CTYPE, IPLAN, ISTAT)                                     H
      SUBROUTINE ZSRTS ( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES,    MLu
     * CUNITS, CTYPE, IPLAN, ISTAT)                                     MLu
C
C     Store regular interval time series data short version
C     For data compression, quality flags or user header informaiton
C     use the extended version (ZSRTSX)
C
C
C     INTEGER*6 IFLTAB(*), IQUAL, IHEAD                                 H
      INTEGER*4 IFLTAB(*), IQUAL, IHEAD                                 ML
C     INTEGER IFLTAB(*)                                                 u
      CHARACTER CPATH*(*), CDATE*(*), CTIME*(*), CUNITS*(*), CTYPE*(*)
      REAL VALUES(*)
      INTEGER NVALS, NHEAD, IPLAN, ISTAT
C
      COMMON /ZDSSCP/ JCOMP, BASEV, LBASEV, LDHIGH, NPREC
      LOGICAL LBASEV, LDHIGH
C
C
      NHEAD = -1
C
      CALL ZSRTSX ( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES,
     * IQUAL, .FALSE., CUNITS, CTYPE, IHEAD, NHEAD, IPLAN,
     * JCOMP, BASEV, LBASEV, LDHIGH, NPREC, ISTAT)
C
      JCOMP = 0
      BASEV = 0.0
      LBASEV = .FALSE.
      LDHIGH = .FALSE.
C
C
      RETURN
      END

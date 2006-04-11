C     SUBROUTINE ZRRTS6 ( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES,   H
C    * CUNITS, CTYPE, IOFSET, ISTAT)                                    H
      SUBROUTINE ZRRTS ( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES,    MLu
     * CUNITS, CTYPE, IOFSET, ISTAT)                                    MLu
C
C     Short version for retrieving regular interval time series data
C
C
C     INTEGER*6 IFLTAB(*), IQUAL, IUHEAD                                H
      INTEGER*4 IFLTAB(*), IQUAL, IUHEAD                                ML
C     INTEGER IFLTAB(*)                                                 u
      INTEGER*4 IOFSET                                                  ML
      CHARACTER CPATH*(*), CDATE*(*), CTIME*(*), CUNITS*(*), CTYPE*(*)
      REAL VALUES(*)
      INTEGER NVALS, NUHEAD, ISTAT
      LOGICAL LQUAL
C
C
      LQUAL = .FALSE.
      NUHEAD = 0
C
      CALL ZRRTSX ( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES,
     * IQUAL, .FALSE., LQUAL, CUNITS, CTYPE, IUHEAD, KUHEAD,
     * NUHEAD, IOFSET, ICOMP, ISTAT)
C
C
      RETURN
      END

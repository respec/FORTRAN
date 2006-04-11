C     SUBROUTINE ZGTPF6 (IFLTAB, CPATH, NPATH, NORD, NCURVE, IHORIZ,    H
C    * C1UNIT, C2UNIT, C1TYPE, C2TYPE, CLABEL, KLABEL, NLABEL, IBUFF,   H
C    * KBUFF, NBUFF, VALUES, KVALS, NVALS, ISTAT)                       H
C
      SUBROUTINE ZGTPFD (IFLTAB, CPATH, NPATH, NORD, NCURVE, IHORIZ,    MLu
     * C1UNIT, C2UNIT, C1TYPE, C2TYPE, CLABEL, KLABEL, NLABEL, IBUFF,   MLu
     * KBUFF, NBUFF, VALUES, KVALS, NVALS, ISTAT)                       MLu
C
C     Replaced by ZRPD
C
C
      LOGICAL LABEL
      CHARACTER *(*) CPATH,C1UNIT,C2UNIT,C1TYPE,C2TYPE,CLABEL
      DIMENSION CLABEL(*),VALUES(*)
C     INTEGER*6 IFLTAB(*), IUHEAD                                       H
      INTEGER*4 IFLTAB(*), IUHEAD                                       ML
C     INTEGER IFLTAB(*)                                                 u
C
C
C
      CALL ZRPD (IFLTAB, CPATH(1:NPATH), NORD, NCURVE, IHORIZ,
     * C1UNIT, C1TYPE, C2UNIT, C2TYPE, VALUES, KVALS, NVALS,
     * CLABEL, KLABEL, LABEL, IUHEAD, 0, NUHEAD, ISTAT)
C
C
      IF (ISTAT.EQ.0) THEN
      IF (LABEL) THEN
      NLABEL = NCURVE
      ELSE
      NLABEL = 0
      ENDIF
C
      ELSE
      ENDIF
C     ------------------------------------------------------------------
C
      RETURN
      END

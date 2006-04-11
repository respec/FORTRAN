      SUBROUTINE ZPTPFD (IFLTAB, CPATH, NPATH, NORD, NCURVE, IHORIZ,    MLu
     * C1UNIT, C2UNIT, C1TYPE, C2TYPE, CLABEL, KLABEL, NLABEL, IBUFF,   MLu
     * KBUFF, NBUFF, VALUES, KVALS, NVALS, IPLAN, ISTAT)                MLu
C     SUBROUTINE ZPTPF6 (IFLTAB, CPATH, NPATH, NORD, NCURVE, IHORIZ,    H
C    * C1UNIT, C2UNIT, C1TYPE, C2TYPE, CLABEL, KLABEL, NLABEL, IBUFF,   H
C    * KBUFF, NBUFF, VALUES, KVALS, NVALS, IPLAN, ISTAT)                H
C
C
C     Replaced by ZSPD
C     ------------------------------------------------------------------
C
C
      CHARACTER *(*) CPATH,C1UNIT,C2UNIT,C1TYPE,C2TYPE,CLABEL(*)
      REAL VALUES(*)
C     INTEGER*6 IFLTAB(*), IBUFF(*), IUHEAD                             H
C     INTEGER IFLTAB(*), IBUFF(*)                                       u
      INTEGER*4 IFLTAB(*), IBUFF(*), IUHEAD                             ML
      LOGICAL LABEL
C
C
      IF (NLABEL.GT.0) THEN
      LABEL = .TRUE.
      ELSE
      LABEL = .FALSE.
      ENDIF
C
      CALL ZSPD (IFLTAB, CPATH(1:NPATH), NORD, NCURVE, IHORIZ,
     * C1UNIT, C1TYPE, C2UNIT, C2TYPE, VALUES, CLABEL, LABEL,
     * IUHEAD, 0, IPLAN, ISTAT)
C
      RETURN
      END

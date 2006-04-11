      SUBROUTINE ZCATLG (IFLTAB, ICUNIT, INUNIT, CINSTR, IBEG, ILEN,    MLu
     *  LABREV, LDOSRT, NORECS)                                         MLu
C     SUBROUTINE ZCATL6 (IFLTAB, ICUNIT, INUNIT, CINSTR, IBEG, ILEN,    H
C    *  LABREV, LDOSRT, NORECS)                                         H
C
C     Substituted by subroutine ZCAT
C
C     INTEGER*6 IFLTAB(*)                                               H
      INTEGER*4 IFLTAB(*), NORECS                                       ML
C     INTEGER IFLTAB(*)                                                 u
      CHARACTER CINSTR*(*)
      LOGICAL LABREV, LDOSRT, LCDCAT
C
C
      IF (ILEN.GT.IBEG) THEN
      IEND = IBEG + ILEN - 1
      CALL ZCAT (IFLTAB, ICUNIT, 0 , INUNIT, CINSTR(IBEG:IEND),
     *  LABREV, LDOSRT, LCDCAT, NORECS)
      ELSE
      CALL ZCAT (IFLTAB, ICUNIT, 0 , INUNIT, ' ',
     *  LABREV, LDOSRT, LCDCAT, NORECS)
      ENDIF
C
      RETURN
      END

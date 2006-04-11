      SUBROUTINE ZDTYPE (IFLTAB, CPATH, NDATA, LFOUND, CDTYPE, IDTYPE)  MLu
C     SUBROUTINE ZDTYP6 (IFLTAB, CPATH, NDATA, LFOUND, CDTYPE, IDTYPE)  H
C
C     Determine the data type of a record and whether it exists
C
C     Written by Bill Charley at HEC, 1990.
C
C     INTEGER*6 IFLTAB(*)                                               H
      INTEGER*4 IFLTAB(*)                                               ML
C     INTEGER IFLTAB(*)                                                 u
      CHARACTER CPATH*(*), CDTYPE*(*)
      LOGICAL LFOUND
      INTEGER IBPART(6), IEPART(6), ILPART(6)
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 20) IFLTAB(KUNIT), CPATH         D
 20   FORMAT (T6,'-----DSS---Debug: Enter ZDTYPE',/,T10,                D
     * 'UNIT =',I4,'  PATH: ',A)                                        D
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR (IFLTAB, 5, 'ZDTYPE', 0,          MLu
     * IFLTAB, ' ', 0, ' ',0)                                           MLu
C     IF ((IFLTAB(1).NE.6).AND.(IFLTAB(1).NE.4)) CALL ZERROR (IFLTAB,   H
C    * 5, 'ZDTYPE', 0, IFLTAB, ' ', 0, ' ',0)                           H
C
C
C     Check if this record exists
C
      CALL CHRLNB (CPATH, NPATH)
      CALL ZCHECK (IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
C
C     IF ((LFOUND).AND.(IFLTAB(1).EQ.6)) THEN                           H
      IF (LFOUND) THEN                                                  MLu
C     Get data type
      IDTYPE = IFLTAB(KDTYPE)
C
      ELSE
C
      NDATA = 0
      IDTYPE = 0
C     See if we can determine if this is a time series record
      CALL ZUPATH (CPATH, IBPART, IEPART, ILPART, ISTAT)
      IF ((ISTAT.EQ.0).AND.(ILPART(5).GE.4)) THEN
      IST = 1
      CALL ZGINTL (INTL, CPATH(IBPART(5):IEPART(5)), N, IST)
      IF (IST.EQ.0) THEN
      IDTYPE = 100
      ELSE IF (IST.EQ.1) THEN
      IDTYPE = 110
      ENDIF
      ENDIF
C
      ENDIF
C
C
      IF (IDTYPE.EQ.100) THEN
      CDTYPE = 'RTS'
      ELSE IF (IDTYPE.EQ.110) THEN
      CDTYPE = 'ITS'
      ELSE IF (IDTYPE.EQ.200) THEN
      CDTYPE = 'PD '
      ELSE IF (IDTYPE.EQ.300) THEN
      CDTYPE = 'TXT'
      ELSE
      CDTYPE = 'UND'
      ENDIF
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT, 820) LFOUND, IDTYPE, CDTYPE       D
 820  FORMAT (T6,'-----DSS--Debug: EXIT ZDTYPE;  Found: ',L1,           D
     * ';  Type:',I4,2X,A)                                              D
C
      RETURN
C
      END

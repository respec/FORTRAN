      SUBROUTINE ZRDINF (IFLTAB, CPATH, NHEAD, NDATA, ISTAT)
C
C     Read the info block for a record
C     ISTAT is returned as:
C        0  Record found, ok to read/write
C       -1  Record not found
C        1  Record found, user does not have delete access
C           (but does have read/write access)
C        2  Record found, user has only read access
C        3  Record found, user does not have access
C
C     Written by Bill Charley at HEC, 1989.
C
C     INTEGER*6 IFLTAB(*), NADD                                         H
      INTEGER*4 IFLTAB(*), NADD                                         ML
C     INTEGER IFLTAB(*)                                                 u
      CHARACTER CPATH*(*), CTPATH*80, CV*4
      LOGICAL LFOUND
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
CADD C.ZDSSLZ                                                           H
      INCLUDE 'zdsslz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 20) IFLTAB(KUNIT), CPATH         D
 20   FORMAT (T6,'-----DSS---Debug: Enter ZRDINF',/,T10,                D
     * 'UNIT =',I4,'  PATH: ',A)                                        D
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR (IFLTAB, 5, 'ZRDINF',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
      CALL CHRLNB (CPATH, NPATH)
C
C     Check if this record exists
C
      CALL ZCHECK ( IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
C
C     If the record does not exist, write message then return
      IF (.NOT.LFOUND) THEN
      NHEAD = 0
      NDATA = 0
      ISTAT = -1
      ELSE
C
C     Get Information Block
      NADD = IPNBIN(JPNBIN+NPPWRD+KBAINF)
      CALL ZGTREC (IFLTAB, INFO, NINFO+NPPWRD, NADD, .FALSE.)
C
C     Double Check that this is the correct pathname
      CALL HOL2CH ( INFO(KIPATH), CTPATH, NPMWRD)
      IF (NPATH.NE.INFO(KINPAT)) GO TO 900
      IF (CPATH(1:NPATH).NE.CTPATH(1:NPATH)) GO TO 900
C
C     Check the record access
      IF (INFO(NPPWRD+KIPASS).NE.0) THEN
C     Check access for this user
      ISTAT = 3   !!!! TEMPOARY !!!!!!
      CALL ZINQIR (IFLTAB, 'FVER', CV, I)
      I = ICHAR(CV(3:3))
      IF (I.LT.69) ISTAT = 0
      ELSE
C     Every one has access
      ISTAT = 0
      ENDIF
C
      ENDIF
C
      IF (MLEVEL.GE.11) WRITE (MUNIT, 820) ISTAT                        D
 820  FORMAT (T6,'-----DSS--Debug: EXIT ZRDINF;  Status:',I3)           D
C
      RETURN
C
C
 900  CONTINUE
      CALL ZDEBUG (MUNIT, INFO, 1, 30)
      NP = INFO(KINPAT)
      CALL ZERROR (IFLTAB, 11, 'ZRDINF', 0, NADD, CPATH, NPATH, CTPATH,
     * NP)
C
      END

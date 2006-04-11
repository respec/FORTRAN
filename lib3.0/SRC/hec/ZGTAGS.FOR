      SUBROUTINE ZGTAGS (IFLTAB, CLINE, ISTAT)
C
C     Gets the current tag scheme and places it in CLINE
C     If no tag scheme is set, ISTAT is returned as -1.
C
C     Written by Bill Charley at HEC, 1990
C
C     INTEGER*6 IFLTAB(*), IADD                                         H
      INTEGER*4 IFLTAB(*), IADD                                         ML
C     INTEGER IFLTAB(*)                                                 u
      CHARACTER CLINE*(*)
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
C
      CLINE = ' '
      ISTAT = 0
C
C     Get the permanent section of the file
      IADD = 1
      CALL ZGTREC (IFLTAB, IFLTAB(KPERM), NPERM, IADD, .FALSE.)
C
C     Is a scheme set?
      IF (IFLTAB(KTAGS).EQ.0) GO TO 900
C
      IPOS = 0
C
C     Loop through, getting each part character and location
      DO 40 I=1,8
C
      ILOC = ((I-1) * 2) + KTAGS
      IPART = IFLTAB(ILOC)
C
C     If this value is zero, we are done
      IF (IPART.EQ.0) GO TO 800
C     Put a comma after the proceeding value
      IF (I.GT.1) THEN
      IPOS = IPOS + 1
      CLINE(IPOS:IPOS) = ','
      ENDIF
C
      IPOS = IPOS + 1
C     If IPART is > 32, it is a part letter
      IF (IPART.GE.32) THEN
      CLINE(IPOS:IPOS) = CHAR(IPART)
C
      ELSE
C
C     If it is less than zero, it is a flag to parse the part
C     e.g., for "FLOW-RES OUT", get the "R"
      IF (IPART.LT.0) THEN
      CLINE(IPOS:IPOS) = '_'
      IPART = IABS(IPART)
      IPOS = IPOS + 1
      ENDIF
C
C     Get the position within the part
      CLINE(IPOS:IPOS) = CHAR(IPART+64)
      IPOS = IPOS + 1
      INUMB = IFLTAB(ILOC+1)
C
      IF (INUMB.LT.10) THEN
      WRITE (CLINE(IPOS:IPOS), '(I1)') INUMB
      ELSE
      WRITE (CLINE(IPOS:IPOS+1), '(I2)') INUMB
      IPOS = IPOS + 1
      ENDIF
C
      ENDIF
C
 40   CONTINUE
C
C
 800  CONTINUE
      RETURN
C
 900  CONTINUE
      ISTAT = -1
      GO TO 800
C
      END

      SUBROUTINE ZSTAGS (IFLTAB, CLINE, ISTAT)
C
C     Sets the record tag scheme permanently for a file.
C     Cline should contain up to 8 portions identifying
C     the part character or a character for the tag.
C     A part character is identified by a part letter (A,B,C,D,E, or F),
C     followed by the number of the letter in that part.  For example,
C     the second letter of part B would be B2.
C     A single character will be used if no number follows (e.g., '-')
C     An example CLINE might be:
C        B1,B2,B3,B4,-,C1,F1
C
C     Written by Bill Charley at HEC, 1990
C
C     INTEGER*6 IFLTAB(*), NADD                                         H
      INTEGER*4 IFLTAB(*), NADD                                         ML
      INTEGER*4 INTGR                                                   ML
C     INTEGER IFLTAB(*)                                                 u
      INTEGER IBF(8), IEF(8), ILF(8), IPART(8), IPOS(8)
      CHARACTER CLINE*(*)
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSNZ                                                           H
      INCLUDE 'zdssnz.h'                                                MLu
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR (IFLTAB, 5, 'ZSTAGS',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
C     Clear arrays
      DO 20 I=1,8
      IPART(I) = 0
      IPOS(I) = 0
 20   CONTINUE
C
C     Parse CLINE
      CALL PARSLI (CLINE, 8, NFIELD, IBF, IEF, ILF)
C
      IF (NFIELD.GT.0) THEN
      DO 40 I=1,NFIELD
      JPOS = IBF(I)
      IF (ILF(I).GE.2) THEN
      IF (CLINE(JPOS:JPOS).EQ.'_') THEN
      IDELM = -1
      JPOS = JPOS + 1
      ELSE
      IDELM = 1
      ENDIF
C
      IPART(I) = ICHAR(CLINE(JPOS:JPOS)) - 64
      IF ((IPART(I).LT.1).OR.(IPART(I).GT.6)) GO TO 900
      IPOS(I) = INTGR (CLINE(JPOS:), 2, IEF(I)-JPOS, ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      IF ((IPOS(I).LT.1).OR.(IPOS(I).GT.32)) GO TO 900
      IPART(I) = IPART(I) * IDELM
      ELSE
      IPART(I) = ICHAR(CLINE(JPOS:JPOS))
      IF ((IPART(I).LT.32).OR.(IPART(I).GT.126)) GO TO 900
      ENDIF
 40   CONTINUE
      ELSE
C     Clear tag scheme settings (done by loop at statement 20)
      ENDIF
C
C     Now store this information in the permanent section of the file
C     Get the permanent section and lock the file
      CALL ZMULTU ( IFLTAB, .TRUE.)
C
      DO 80 I=1,8
      ILOC = ((I-1) * 2) + KTAGS
      IFLTAB(ILOC) = IPART(I)
      IFLTAB(ILOC+1) = IPOS(I)
 80   CONTINUE
C
      NADD = 1
      CALL ZPTREC (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .FALSE.)
C
C     Release the file and dump the buffer
      CALL ZMULTU ( IFLTAB, .FALSE.)
C
 800  CONTINUE
      RETURN
C
 900  CONTINUE
      ISTAT = -1
      GO TO 800
C
      END

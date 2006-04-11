      SUBROUTINE PARSLI (CLINE, MAXF, NFIELD, IBF, IEF, ILF)
C
C     Parses a line, returning the beginning position, ending
C     position, and length of each field.
C     A field is delimited by a comma or/and a blank
C     Will not distinguish a null or blank field
C     (i.e., commas are treated as blanks)
C     (e.g.,  ',,' or ', ,')
C     The maximum number of fields (and diminsion limit
C     of the arrays are passed in as MAXF
C
C     Written by Bill Charley, HEC, June, 1989
C
C
      CHARACTER CLINE*(*)
      INTEGER IBF(*), IEF(*), ILF(*)
C
C
      CALL CHRLNB (CLINE, ILEN)
      NFIELD = 0
      IPOS = 0
C
 20   CONTINUE
      IF (NFIELD.GE.MAXF) GO TO 800
C     Parse for beginning of string (non-blank)
      JPOS = IPOS
      DO 40 I=JPOS,ILEN
      IPOS = IPOS + 1
      IF ((CLINE(IPOS:IPOS).NE.' ').AND.(CLINE(IPOS:IPOS).NE.',')) THEN
      NFIELD = NFIELD + 1
      IBF(NFIELD) = IPOS
      GO TO 60
      ENDIF
 40   CONTINUE
      GO TO 800
C
C
C     Parse for end of string (blank or comma)
 60   CONTINUE
      JPOS = IPOS
      DO 80 I=JPOS,ILEN
      IPOS = IPOS + 1
      IF ((CLINE(IPOS:IPOS).EQ.' ').OR.(CLINE(IPOS:IPOS).EQ.',')) THEN
      IEF(NFIELD) = IPOS - 1
      ILF(NFIELD) = IEF(NFIELD) - IBF(NFIELD) + 1
      GO TO 20
      ENDIF
 80   CONTINUE
C
C     Reached end of line
      IEF(NFIELD) = ILEN
      ILF(NFIELD) = IEF(NFIELD) - IBF(NFIELD) + 1
C
 800  CONTINUE
      RETURN
      END

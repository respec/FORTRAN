      SUBROUTINE LJCHR (CLINE, NLINE)
C
C     Left Justifies CLINE, and adjusts the length, NLINE
C
      CHARACTER CLINE*(*), CTEMP*132
C
      IF (NLINE.LE.0) GO TO 800
      IF (NLINE.GT.132) NLINE = 132
C
      N = NINDX (CLINE(1:NLINE), ' ')
      IF (N.LE.1) GO TO 800
C
      CTEMP = CLINE(1:NLINE)
      CLINE(1:NLINE) = CTEMP(N:NLINE)
      N = NLINE
      CALL CHRLNB (CLINE(1:N), NLINE)
C
 800  CONTINUE
      RETURN
      END

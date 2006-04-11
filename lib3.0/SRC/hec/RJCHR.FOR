      SUBROUTINE RJCHR (CLINE)
C
C     Right Justifies CLINE
C
      CHARACTER CLINE*(*), CTEMP*132
C
C
      CALL CHRLNB (CLINE, N)
      IF (N.EQ.0) GO TO 800
      IF (N.GT.132) N = 132
      NLINE = LEN (CLINE)
      IF (N.GE.NLINE) GO TO 800
C
      CTEMP = CLINE(1:N)
      J = NLINE - N + 1
      CLINE(J:NLINE) = CTEMP(1:N)
      IF (J.GT.1) CLINE(1:J-1) = ' '
C
 800  CONTINUE
      RETURN
      END

      SUBROUTINE ZIRDOW (ICUR, NPAIRS, NDOWN, IMULT, BUFF)
C
C     For irregular interval time series data
C     Moves data in the array Buff down (or up) NDOWN spaces for
C     insertion (or deletion) of data
C
C     Written by Bill Charley
C
      REAL BUFF(*)
C
C
C
      IF (NDOWN.GT.0) THEN
C
C     Enlarge the Buffer
      JCUR = ICUR + NDOWN
      DO 20 I=NPAIRS,JCUR,-1
      K = ((I-1) * IMULT) + 1
      J = ((I-NDOWN-1) * IMULT) + 1
      IF (IMULT.EQ.3) BUFF(K+2) = BUFF(J+2)
      BUFF(K+1) = BUFF(J+1)
      BUFF(K) = BUFF(J)
 20   CONTINUE
C
      ELSE IF (NDOWN.LT.0) THEN
C
C     Shrink the buffer
      DO 40 I=ICUR,NPAIRS
      K = ((I-1) * IMULT) + 1
      J = ((I-NDOWN-1) * IMULT) + 1
      BUFF(K) = BUFF(J)
      BUFF(K+1) = BUFF(J+1)
      IF (IMULT.EQ.3) BUFF(K+2) = BUFF(J+2)
 40   CONTINUE
C
      ENDIF
C
      RETURN
C
      END

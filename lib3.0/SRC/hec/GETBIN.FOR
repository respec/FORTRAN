      SUBROUTINE GETBIN (JWORDS, NBYTES, CREPR)
C
C     Get the binary representation of JWORDS and place
C     into CREPR.  CREPR must be a CHARACTER array of 8 characters
C     in length.  NBYTES is the number of bytes to convert.
C
C
C     PARAMETER(IWRDL=3)                                                H
C     PARAMETER(IWRDL=4)                                                lg
      PARAMETER(IWRDL=2)                                                ML
      INTEGER JWORDS(*)
      CHARACTER CREPR(*)*8
C
      LOGICAL LBTEST
      CHARACTER CVAR*(IWRDL)
C     EQUIVALENCE (IVAR,CVAR)                                           HMlg
C
C
      NW = (NBYTES-1)/IWRDL + 1
      JBYTES = 0
C
      DO 30 I=1,NW
C     IVAR = JWORDS(I)                                                  HMlg
      CALL HOLCHR (JWORDS(I), 1, IWRDL, CVAR, 1)                        L
C
      DO 20 J=1,IWRDL
      JBYTES = JBYTES + 1
      IF (JBYTES.GT.NBYTES) RETURN
      ICH = ICHAR(CVAR(J:J))
C
      DO 10 N=1,8
      M = 8 - N
      IF (LBTEST(ICH,M)) THEN
      CREPR(JBYTES)(N:N) = '1'
      ELSE
      CREPR(JBYTES)(N:N) = '0'
      ENDIF
 10   CONTINUE
C
 20   CONTINUE
C
 30   CONTINUE
C
      END

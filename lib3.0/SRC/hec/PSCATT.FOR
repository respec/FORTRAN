      SUBROUTINE PSCATT ( CATT, CSCN, ISATT, ILINE,NLINE,IOFSET,IPSTAT)
C------
C------ Get the meaning of an attribute character and send it
C------ to the terminal.  Note most attributes are cumulative
C------ in affect so set to 00 if necessary!! Ignore undefines
C------
      CHARACTER CATT*1, CSCN(*)*1, CTMP*2
C------
C     WRITE(3,*) 'IS,IL,NL ',ISATT, ILINE, NLINE,IOFSET
C     WRITE(3,*) CATT,CSCN(ISATT),CSCN(ISATT+1)
C ------
      IF ( IOFSET .LT.  0 ) THEN
C ------ RETURN THE NULL DEFINED ATTRIBUTE IF ONE EXISTS
C ------ (I.E.   X=
      K = IABS(IOFSET) - 1
      IS = ISATT - ILINE
      DO 15 I = 1, NLINE
      IS = IS + ILINE
      IF ( CSCN(IS+K+1).EQ.' '.AND.CSCN(IS+K+2).EQ.' ') THEN
      IPSTAT = 4
      CATT = CSCN(IS)
      RETURN
      ENDIF
   15 CONTINUE
      IPSTAT = 0
      RETURN
      ENDIF
C ------
      IS = ISATT - ILINE
      DO 20 I = 1, NLINE
      IS = IS + ILINE
C     WRITE(3,*) 'ATT ',CSCN(IS)
      IF ( CSCN(IS) .EQ. CATT ) THEN
      K = IOFSET - 1
      IF ( CSCN(IS+K) .NE. '=' ) GO TO 150
C ------ Check if null defined attribute character encountoured
      IF(CSCN(IS+K+1).EQ.' '.AND.CSCN(IS+K+2).EQ.' ') GO TO 200
    2 CTMP(1:1) = CSCN(IS+K+1)
      CTMP(2:2) = CSCN(IS+K+2)
      READ(CTMP,50,ERR=100) IATT
C     WRITE(3,*)' IATT ',IATT,K,N,CTMP
   50 FORMAT(I2)
      CALL VATT ( IATT )
      K = K + 3
      IF ( CSCN(IS+K) .NE. '+' ) GO TO 100
      GO TO 2
      ENDIF
   20 CONTINUE
C ------ UNDEFINED CHAR
      IPSTAT = 3
      GO TO 900
  100 IPSTAT = 0
C     CALL WAITS(1.0)
      GO TO 900
C ------ ERROR ILLEGAL FORMAT
  150 IPSTAT = 1
      GO TO 900
C ------ HERE FOR NULL DEFINED CHAR
  200 IPSTAT = 2
  900 RETURN
      END

      SUBROUTINE RAINCD(ICODE,JD1)
C=======================================================================
C     Routine to put indication of special codes in an array.
C     Written by Wayne Huber, 4/26/94.
C=======================================================================
      INCLUDE 'PRECIP.INC'
C
      CHARACTER*1 BLANK,IM,IAB,IAS,II,IX,ICODE,DASH
      DATA BLANK/' '/,IM/'M'/,IAB/'A'/,IAS/'a'/,II/'I'/,IX/'O'/,
     1     DASH/'-'/
C
      IF(ICODE.EQ.BLANK) RETURN
C
      IF(ICODE.EQ.IAB.OR.ICODE.EQ.IAS) THEN
           ACODE(JD1,1) = IAB
           GO TO 100
           ENDIF
      IF(ICODE.EQ.II)                  THEN
           ACODE(JD1,2) = II
           GO TO 100
           ENDIF
      IF(ICODE.EQ.IM)                  THEN
           ACODE(JD1,3) = IM
           GO TO 100
           ENDIF
      IF(ICODE.NE.BLANK)               THEN
           ACODE(JD1,4) = IX
           GO TO 100
           ENDIF
C=======================================================================
C     Fill in with dashes if any values are non-blanks.
C=======================================================================
  100 DO 200 I=1,4
      IF(ACODE(JD1,I).EQ.BLANK) ACODE(JD1,I) = DASH
  200 CONTINUE
      RETURN
      END

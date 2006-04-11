*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/icard.f,v 1.1 1998/07/07 19:32:19 grogers Exp $
*  icard.f
*
      INTEGER FUNCTION ICARD (CHLINE, LENG, IERR)

*  Purpose: Read an integer from a line of card image.
********************************************************
*
*  $Log: icard.f,v $
*  Revision 1.1  1998/07/07 19:32:19  grogers
*  PR#0, initial load of nadcon_lib
*
*
*** LENG is the length of the card
*** blanks are the delimiters of the integer

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER LENG, IERR, I, J, ILENG
      INTEGER IVAR
      CHARACTER*80 CHLINE

      IERR = 0

* Find first non-blank character

* DO WHILE line character is bland, I is first non-blank character

      I = 1
   10 IF ( CHLINE(I:I) .EQ. ' '  .OR.  CHLINE(I:I) .EQ. ',' ) THEN
        I = I + 1

* Check for totally blank card

        IF ( I .GE. LENG) THEN
          ICARD = 0
          LENG = 0
          RETURN
        ENDIF

      GOTO 10
      ENDIF

* Find first blank character (or end of line)

* DO WHILE line character is not a blank

      J = I + 1
   20 IF ( CHLINE(J:J) .NE. ' '  .AND.  CHLINE(J:J) .NE. ',' ) THEN
        J = J + 1

* Check for totally filed card

        IF ( J .GT. LENG) THEN
          GOTO 40
        ENDIF

      GOTO 20
      ENDIF

* J is now 1 more than the position of the last non-blank character

   40 J = J - 1

* ILENG is the length of the integer string, it cannot be greater
* than 13 characters

      ILENG = J - I + 1

      IF (ILENG .GT. 13) THEN
        STOP 'ICARD'
      ENDIF

* Read the integer variable from the line, and set the return VAR to it

      READ (CHLINE(I:J), 55, ERR=9999) IVAR
   55 FORMAT (I13)
      ICARD = IVAR

* Now reset the values for LENG and CHLINE to the rest of the card

      CHLINE( 1 : LENG ) = CHLINE( (J+1) : LENG )
      LENG = LENG - J

      RETURN

* Read error

 9999 IERR = 1
      RETURN
      END

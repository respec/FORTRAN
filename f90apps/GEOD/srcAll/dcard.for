*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/dcard.f,v 1.1 1998/07/07 19:32:03 grogers Exp $
*  dcard.f
*
      DOUBLE PRECISION FUNCTION DCARD (CHLINE, LENG, IERR)

*  Purpose: Read a double precision number from a line of card image.
*********************************************************************
*
*** LENG is the length of the card
*** blanks are the delimiters of the REAL*8 variable
*
*
*  $Log: dcard.f,v $
*  Revision 1.1  1998/07/07 19:32:03  grogers
*  PR#0, initial load of nadcon_lib
*
*
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION VAR
      INTEGER LENG, IERR, I, J, ILENG
      CHARACTER*80 CHLINE

      IERR = 0

* Find first non-blank character

* DO WHILE line character is blank, I is first non-blank character

      I = 1
   10 IF ( CHLINE(I:I) .EQ. ' '  .OR.  CHLINE(I:I) .EQ. ',' ) THEN
        I = I + 1

* Check for totally blank card

        IF ( I .GE. LENG) THEN
          DCARD = 0.0D0
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

* ILENG is the length of the real*8 string, it cannot be greater
* than 15 characters

      ILENG = J - I + 1

      IF (ILENG .GT. 20) THEN
        STOP 'DCARD'
      ENDIF

* Read the real*8 variable from the line, and set the return VAR to it

      READ (CHLINE(I:J), 55, ERR=9999) VAR
   55 FORMAT (F20.0)
      DCARD = VAR

* Now reset the values of LENG and CHLINE to the rest of the card

      CHLINE( 1 : LENG ) = CHLINE( (J+1) : LENG )
      LENG = LENG - J

      RETURN

* Read error

 9999 IERR = 1
      RETURN
      END

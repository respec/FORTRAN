*  $Header: /nwiscvs/watstore/geod/src/nadcon_lib/ccard.f,v 1.1 1998/07/07 19:32:01 grogers Exp $
*  ccard.f
*
      CHARACTER*(*) FUNCTION CCARD (CHLINE, LENG, IERR)

*  Purpose: Read a character variable from a line of card image.
****************************************************************
*
*** LENG is the length of the card
*** blanks are the delimiters of the character variable
*
*  $Log: ccard.f,v $
*  Revision 1.1  1998/07/07 19:32:01  grogers
*  PR#0, initial load of nadcon_lib
*
*
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER LENG, IERR, I, J, ILENG
      CHARACTER*80 CHLINE

      IERR = 0

* Find first non-blank character

* DO WHILE line character is blank, I is first non-blank character

      I = 1
   10 IF ( CHLINE(I:I) .EQ. ' '  .OR.  CHLINE(I:I) .EQ. ',' ) THEN
        I = I + 1

* Check for totally blank card (assume length of 2)

        IF ( I .GE. LENG) THEN

         CCARD = '  '
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

* ILENG is the length of the character string, it can be any length
* up to the length of the line

      ILENG = J - I + 1

      IF (ILENG .GT. LENG) THEN
        STOP 'CCARD'
      ENDIF

* Read the char variable from the line, and set the return VAR to it

c     READ (CHLINE(I:J), 55, ERR=9999) CCARD
c  55 FORMAT (A80)

* Now reset the values of LENG and CHLINE to the rest of the card

c     CHLINE( 1 : LENG ) = CHLINE( (J+1) : LENG )
c     LENG = LENG - J
c set ccard = to the non blank portion of chline
	CCARD = ' '
	CCARD(1:ILENG) = CHLINE(I:J)
	RETURN

* Read error

 9999 IERR = 1
      RETURN
      END

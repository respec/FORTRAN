      FUNCTION INTCHR(J1)
C=======================================================================
C     Change a character to an integer.
C=======================================================================
      CHARACTER*1 J1(3)
      INTEGER      I(3)
      INTCHR   = 0
      DO 100 J = 1,3
      I(J)     = ICHAR(J1(J)) - 48
100   INTCHR   = INTCHR + I(J)*10**(3-J)
      RETURN
      END

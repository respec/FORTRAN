      real function fslq(q,n,c1,y)
c
c     + + + PURPOSE + + +
c
c     Function FSLQ computes the friction slope at the channel outlet.
c
c     Called from: SR FRICHN
c     Author(s): Ascough II, R. van der Zweep, V. Lopes
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by: Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real n, q, c1, y
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     n  -
c     q  -
c     c1 -
c     y  -
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
c     + + + LOCAL DEFINITIONS + + +
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      fslq = (q*n/(1.49*c1*y**(8.0/3.0))) ** 2.0
c
      return
      end

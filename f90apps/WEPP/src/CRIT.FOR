      real function crit(z,q)
c
c     + + + PURPOSE + + +
c
c     Function CRIT computes the critical flow depth.
c
c     Called from: SRS FRICHN, FSLPAR
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
      real z, q
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     z -
c     q -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcon.inc'
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
      crit = (2.0*beta*q**2/(agrav*z**2)) ** 0.2
c
      return
      end

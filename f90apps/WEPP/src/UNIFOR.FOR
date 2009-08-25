      real function unifor(slope,z,n,q)
c
c     + + + PURPOSE + + +
c
c     Function UNIFOR computes the normal depth of flow.
c
c     Called from: SR FSLPAR
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
      real slope, z, n, q
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     slope -
c     z     -
c     n     -
c     q     -
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
      real c1
c
c     + + + LOCAL DEFINITIONS + + +
c
c     c1 -
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
      c1 = (z**2.5/(2.0*sqrt(z**2+1.0))) ** (2.0/3.0)
      unifor = (q*n/(c1*1.49*sqrt(slope))) ** 0.375
c
      return
      end
